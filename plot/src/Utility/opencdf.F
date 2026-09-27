      SUBROUTINE opencdf (ncname1,ncname2,ncname3)
!
!svn $Id: opencdf.F 833 2017-01-24 22:20:16Z arango $
!================================================== Hernan G. Arango ===
!  Copyright (c) 2002-2017 The ROMS/TOMS Group                         !
!    Licensed under a MIT/X style license                              !
!    See License_ROMS.txt                                              !
!=======================================================================
!                                                                      !
!  This routine opens an existing NetCDF file and inquires about it    !
!  contents, and checks for consistency with model dimensions.         !
!                                                                      !
!  On Input:                                                           !
!                                                                      !
!     ncname1  Input primary NetCDF file name.                         !
!     ncname2  Input secondary NetCDF file name.                       !
!     ncname3  Input grid NetCDF file name.                            !
!                                                                      !
!  Calls:   ncagtc, ncainq, ncclos, ncdinq, ncinq, ncopn, ncpopt,      !
!           ncvinq  (NetCDF library)                                   !
!           crash, lenstr                                              !
!                                                                      !
!=======================================================================
!
!-----------------------------------------------------------------------
!  Define global variables.
!-----------------------------------------------------------------------
!
      implicit none
#include "param.h"
#include "pconst.h"
#include "domdat.h"
#include "fldsid.h"
#include "fmask.h"
#include "ncinfo.h"
#include "netcdf.inc"
#include "pltfld.h"
#include "pltio.h"
#include "pltlab.h"
#include "pltncar.h"
!
!-----------------------------------------------------------------------
!  Define local variables.
!-----------------------------------------------------------------------
!
      logical goter, goteu, gotev, gotxr, gotxu, gotxv
      integer dimid, dimsiz, i, ival, j, ldim, lstr1, lstr2, lstr3,     &
     &        latt, lvar, ndims, ngatts, nvardim, nvatts, recdim,       &
     &        status
      integer count(4), start(4), vardim(nf_max_dims)
      integer lenstr
      character*1   char1
      character*40  Tname1, Tname2, attnam, calendar, dimnam, tunits
      character*40  vname
      character*(*) ncname1, ncname2, ncname3
!
!=======================================================================
!  Begin executable code.
!=======================================================================
!
      gotangle1=.false.
      gotrho1=.false.
      gottime1=.false.
      gotadtime1=.false.
      gotsalt1=.false.
      gotzeta1=.false.
      gotangle2=.false.
      gotrho2=.false.
      gottime2=.false.
      gotadtime2=.false.
      gotsalt2=.false.
      gotzeta2=.false.
      goter=.false.
      goteu=.false.
      gotev=.false.
      gotxr=.false.
      gotxu=.false.
      gotxv=.false.
      mskinp1=.false.
      mskinp2=.false.
      julday1=.false.
      julday2=.false.
      dstart=c0
      ntime1=0
      ntime2=0
      Tscale1=1.0
      Tscale2=1.0
      timeref1=0.0d0
      timeref2=0.0d0
      Vtransform1=1
      Vtransform2=1
      nfcst1=0
      nfcst2=0
      Lr=0
      Lu=0
      Lv=0
      Mr=0
      Mu=0
      Mv=0
      Nr=0
      Nw=0
      NL=0
!
!  Open primary input NetCDF file.
!
      lstr1=lenstr(ncname1)
      status=nf_open(ncname1(1:lstr1),nf_nowrite,ncid1)
      IF (status.ne.nf_noerr) THEN
        WRITE (stdout,901) ncname1(1:lstr1)
        CALL crash ('OPENCDF',status)
      END IF
!
!  Open secondary input NetCDF file, if any.
!
      IF ((iref.eq.2).or.(iref.eq.3).or.(abs(iref).eq.5)) THEN
        lstr2=lenstr(ncname2)
        status=nf_open(ncname2(1:lstr2),nf_nowrite,ncid2)
        IF (status.ne.nf_noerr) THEN
          WRITE (stdout,901) ncname2(1:lstr2)
          CALL crash ('OPENCDF',status)
        END IF
      END IF
!
!  Open grid input NetCDF file, if applicable.
!
      IF (readgrd) THEN
        lstr3=lenstr(ncname3)
        status=nf_open(ncname3(1:lstr3),nf_nowrite,ncgrd)
        IF (status.ne.nf_noerr) THEN
          WRITE (stdout,901) ncname3(1:lstr3)
          CALL crash ('OPENCDF',status)
        END IF
      ELSE
        ncgrd=ncid1
      END IF
!
!=======================================================================
!  Inquire about the primary file.
!=======================================================================
!
!  Inquire about the contents of input NetCDF file:  Inquire about the
!  dimensions and variables.
!
      status=nf_inq(ncid1,ndims,nvars,ngatts,recdim)
      IF (nvars.gt.maxvar) THEN
        WRITE (stdout,902) maxvar, nvars
        CALL crash ('OPENCDF',0)
      END IF
      IF (status.eq.nf_noerr) THEN
!
!  Inquire and get global "type" attribute.
!
        DO i=1,ngatts
          status=nf_inq_attname(ncid1,nf_global,i,attnam)
          IF (status.eq.nf_noerr) THEN
            latt=lenstr(attnam)
            IF (attnam(1:latt).eq.'type') THEN
              status=nf_get_att_text(ncid1,nf_global,'type',type1)
              IF (status.ne.nf_noerr) THEN
                WRITE (stdout,903) 'type (global)', ncname1(1:lstr1)
                CALL crash ('OPENCDF',status)
              END IF
              GO TO 10
            END IF
          ELSE
            WRITE (stdout,904) 'global attribute', ncname1(1:lstr1)
          END IF
        END DO
  10    CONTINUE
!
!  Inquire about dimensions.
!
        DO i=1,ndims
          dimid=i
          status=nf_inq_dim(ncid1,dimid,dimnam,dimsiz)
          IF (status.ne.nf_noerr) THEN
            WRITE (stdout,905) dimid, ncname1(1:lstr1)
            CALL crash ('OPENCDF',status)
          END IF
          ldim=lenstr(dimnam)
          IF ((dimnam(1:ldim).eq.'xi_rho').or.                          &
     &        (dimnam(1:ldim).eq.'lon_rho')) THEN
            Lr=dimsiz
            gotxr=.true.
          ELSE IF ((dimnam(1:ldim).eq.'xi_u').or.                        &
     &            (dimnam(1:ldim).eq.'lon_u')) THEN
            Lu=dimsiz
            gotxu=.true.
          ELSE IF ((dimnam(1:ldim).eq.'xi_v').or.                        &
     &            (dimnam(1:ldim).eq.'lon_v')) THEN
            Lv=dimsiz
            gotxv=.true.
          ELSE IF ((dimnam(1:ldim).eq.'eta_rho').or.                     &
     &            (dimnam(1:ldim).eq.'lat_rho')) THEN
            Mr=dimsiz
            gotxr=.true.
          ELSE IF ((dimnam(1:ldim).eq.'eta_u').or.                       &
     &            (dimnam(1:ldim).eq.'lat_u')) THEN
            Mu=dimsiz
            goteu=.true.
          ELSE IF ((dimnam(1:ldim).eq.'eta_v').or.                       &
     &            (dimnam(1:ldim).eq.'lat_v')) THEN
            Mv=dimsiz
            gotev=.true.
          ELSE IF (dimnam(1:ldim).eq.'N') THEN
            NL=dimsiz
          ELSE IF ((dimnam(1:ldim).eq.'s_rho').or.                       &
     &            (dimnam(1:ldim).eq.'level')) THEN
            Nr=dimsiz
          ELSE IF (dimnam(1:ldim).eq.'s_w') THEN
            Nw=dimsiz
          ELSE IF (dimnam(1:ldim).eq.'forecast') THEN
            nfcst1=dimsiz
            status=nf_inq_varid(ncid1,dimnam(1:ldim),varid)
            status=nf_get_vara_real(ncid1,varid,1,nfcst1,fcst1)
          ELSE IF (INDEX(dimnam(1:ldim),'time').ne.0) THEN
            IF (recdim.lt.0) THEN
              ntime1=dimsiz         ! time dimesion is not unlimited
            END IF
          END IF
        END DO
!
!  If not found, assign number of U- and V-points.
!
        IF (Lr.gt.0) THEN
          IF (Lu.eq.0) Lu=Lr-1
          IF (Lv.eq.0) Lv=Lr
        END IF
        IF (Mr.gt.0) THEN
          IF (Mu.eq.0) Mu=Mr
          IF (Mv.eq.0) Mv=Mr-1
        END IF
!
!  Set-up correct number of vertical w-levels.
!
        IF ((NL.gt.0).and.(Nr.eq.Nw)) THEN
          Nw=NL+1
        END IF
!
!  Inquire about variables.  Read in various variables.
!
        rho0=c1000
        DO i=1,nvars
          varid=i
          status=nf_inq_var(ncid1,varid,varnam(i),vartyp,nvdims(i),     &
     &                      vdims(1,i),nvatts)
          IF (status.ne.nf_noerr) THEN
            WRITE (stdout,906) varid, ncname1(1:lstr1)
            CALL crash ('OPENCDF',status)
          END IF
          lvar=lenstr(varnam(i))
          IF (varnam(i)(1:lvar).eq.'zeta') THEN
            gotzeta1=.true.
          ELSE IF ((varnam(i)(1:lvar).eq.'rho').and.                     &
     &            (nvdims(i).gt.1)) THEN
            gotrho1=.true.
          ELSE IF ((varnam(i)(1:lvar).eq.'salt').and.                    &
     &            (nvdims(i).gt.1)) THEN
            gotsalt1=.true.
          ELSE IF (varnam(i)(1:lvar).eq.'angle') THEN
            gotangle1=.true.
          ELSE IF ((varnam(i)(1:lvar).eq.'mask_rho').or.                 &
     &            (varnam(i)(1:lvar).eq.'mask_u').or.                    &
     &            (varnam(i)(1:lvar).eq.'mask_v')) THEN
            mskinp1=.true.
          ELSE IF (varnam(i)(1:lvar).eq.'Vtransform') THEN
            status=nf_get_var1_int(ncid1,varid,1,Vtransform1)
          ELSE IF (varnam(i)(1:lvar).eq.'xl') THEN
            status=nf_get_var1_real(ncid1,varid,1,xbasin)
          ELSE IF (varnam(i)(1:lvar).eq.'el') THEN
            status=nf_get_var1_real(ncid1,varid,1,ybasin)
          ELSE IF (varnam(i)(1:lvar).eq.'rho0') THEN
            status=nf_get_var1_real(ncid1,varid,1,rho0)
          ELSE IF (varnam(i)(1:lvar).eq.'dstart') THEN
            status=nf_get_var1_real(ncid1,varid,1,dstart)
            DO j=1,nvatts
              status=nf_inq_attname(ncid1,varid,j,attnam)
              IF (status.eq.nf_noerr) THEN
                latt=lenstr(attnam)
                IF (attnam(1:latt).eq.'add_offset') THEN
                  julday1=.true.
                ELSE IF (attnam(1:latt).eq.'calendar') THEN
                  status=nf_get_att_text(ncid1,varid,attnam(1:latt),    &
     &                                   calendar)
                  IF (status.eq.nf_noerr) THEN
                    IF (calendar(1:9).eq.'gregorian') THEN
!                     julday1=.true.  ! only if units attribute has 
                    END IF            ! different base that
                  END IF              ! ... since 1968-05-23 00:00:00
                END IF
              END IF
            END DO
          ELSE IF (varnam(i)(1:lvar).eq.'Lev') THEN
            status=nf_inq_dim(ncid1,vdims(1,i),dimnam,nlev1)
            status=nf_get_vara_int(ncid1,varid,1,nlev1,lev1)
          ELSE IF (varnam(i)(1:lvar).eq.'spherical') THEN
            IF (vartyp.eq.nf_int) THEN
              status=nf_get_var1_int(ncid1,varid,1,ival)
              IF (ival.eq.0) THEN
                spherical=.false.
              ELSE
                spherical=.true.
              END IF
            ELSE IF (vartyp.eq.nf_char) THEN
              status=nf_get_var1_text(ncid1,varid,1,char1)
              IF ((char1.eq.'t').or.(char1.eq.'T')) THEN
                spherical=.true.
              ELSE
                spherical=.false.
              END IF
            END IF
          ELSE IF (varnam(i)(1:lvar).eq.'scrum_time') THEN
            gottime1=.true.
            Tname1='scrum_time'
            Tscale1=sec2day
          ELSE IF (varnam(i)(1:lvar).eq.'ocean_time') THEN
            gottime1=.true.
            Tname1='ocean_time'
            Tscale1=sec2day
          ELSE IF (varnam(i)(1:lvar).eq.'model_time') THEN
            gottime1=.true.
            Tname1='model_time'
            Tscale1=sec2day
          ELSE IF (varnam(i)(1:lvar).eq.'pair_time') THEN
            gottime1=.true.
            Tname1='pair_time'
            Tscale1=1.0
          ELSE IF (varnam(i)(1:lvar).eq.'tair_time') THEN
            gottime1=.true.
            Tname1='tair_time'
            Tscale1=1.0
          ELSE IF (varnam(i)(1:lvar).eq.'cloud_time') THEN
            gottime1=.true.
            Tname1='cloud_time'
            Tscale1=1.0
          ELSE IF (varnam(i)(1:lvar).eq.'wind_time') THEN
            gottime1=.true.
            Tname1='wind_time'
            Tscale1=1.0
          ELSE IF (varnam(i)(1:lvar).eq.'qair_time') THEN
            gottime1=.true.
            Tname1='qairl_time'
            Tscale1=1.0
          ELSE IF (varnam(i)(1:lvar).eq.'rain_time') THEN
            gottime1=.true.
            Tname1='rain_time'
            Tscale1=1.0
          ELSE IF (varnam(i)(1:lvar).eq.'swf_time') THEN
            gottime1=.true.
            Tname1='swf_time'
            Tscale1=1.0
          ELSE IF (varnam(i)(1:lvar).eq.'zout') THEN
            status=nf_get_vara_real(ncid1,varid,1,Nr,zinp1)
          END IF
        END DO
      ELSE
        WRITE (stdout,907) ncname1(1:lstr1)
        CALL crash ('OPENCDF',status)
      END IF
!
!  Set levels for restart files.
!
      IF ((type1(1:13).eq.'SCRUM restart').or.                          &
     &    (type1(1:13).eq.'ROMS restart')) THEN
        nlev1=Nr
        DO i=1,nlev1
          lev1(i)=i
        END DO
      ELSE IF (type1(1:13).eq.'ROMS/TOMS adjoint') THEN
        gotadtime1=.true.
      END IF
!
!-----------------------------------------------------------------------
!  Inquire size of unlimited time record dimension.  Read in time
!  coordinate values.
!-----------------------------------------------------------------------
!
      IF (recdim.gt.0) THEN
        status=nf_inq_dim(ncid1,recdim,dimnam,ntime1)
        IF (status.ne.nf_noerr) THEN
          WRITE (stdout,908) 'time', ncname1(1:lstr1)
          CALL crash ('OPENCDF',status)
        END IF
      ELSE
        dimnam='time'
      END IF
      IF (gottime1) THEN
        lvar=lenstr(Tname1)
        status=nf_inq_varid(ncid1,Tname1(1:lvar),varid)
        IF (status.eq.nf_noerr) THEN
          start(1)=1
          count(1)=ntime1
!!        status=nf_get_vara_real(ncid1,varid,start,count,time1)
          status=nf_get_vara_double(ncid1,varid,start,count,time1)
          IF (status.ne.nf_noerr) THEN
            WRITE (stdout,909) Tname1(1:lvar), ncname1(1:lstr1)
            CALL crash ('OPENCDF',status)
          END IF
!
          status=nf_inq_var(ncid1,varid,varnam,vartyp,nvdims,vdims,     &
     &                      nvatts)
          DO i=1,nvatts
            status=nf_inq_attname(ncid1,varid,i,attnam)
            IF (status.eq.nf_noerr) THEN
              latt=lenstr(attnam)
              IF (attnam(1:latt).eq.'add_offset') THEN
                julday1=.true.
              ELSE IF (attnam(1:latt).eq.'calendar') THEN
                status=nf_get_att_text(ncid1,varid,attnam(1:latt),      &
     &                                 calendar)
                IF (status.eq.nf_noerr) THEN
                  IF (calendar(1:9).eq.'gregorian') THEN
!                   julday1=.true.    ! only if units attribute has 
                  END IF              ! different base that
                END IF                ! ... since 1968-05-23 00:00:00
              ELSE IF (attnam(1:latt).eq.'units') THEN
                status=nf_get_att_text(ncid1,varid,attnam(1:latt),      &
     &                                 tunits)
                IF (status.eq.nf_noerr) THEN
                  IF (tunits(1:6).eq.'second') THEN
                    Tscale1=sec2day
                  END IF
                  CALL dateref (tunits,timeref1,julday1)
                ELSE
                  WRITE (stdout,903) attnam(1:latt), ncname1(1:lstr1)
                  CALL crash ('OPENCDF',status)
                END IF
              END IF
            END IF
          END DO
!!        IF (.not.julday1.and.(time1(1)*sec2day.gt.365)) THEN
!!          julday1=.true.
!!        END IF
        ELSE
          WRITE (stdout,910) Tname1(1:lvar), ncname1(1:lstr1)
          CALL crash ('OPENCDF',status)
        END IF
      ELSE IF ((type1(1:2).eq.'OA').or.(type1(1:4).eq.'DATA')) THEN
        ldim=lenstr(dimnam)
        status=nf_inq_varid(ncid1,dimnam(1:ldim),varid)
        IF (status.eq.nf_noerr) THEN
          start(1)=1
          count(1)=ntime1
!!        status=nf_get_vara_real(ncid1,varid,start,count,time1)
          status=nf_get_vara_double(ncid1,varid,start,count,time1)
          IF (status.ne.nf_noerr) THEN
            WRITE (stdout,909) dimnam(1:ldim), ncname1(1:lstr1)
            CALL crash ('OPENCDF',status)
          END IF
!
          status=nf_inq_var(ncid1,varid,varnam,vartyp,nvdims,vdims,     &
     &                      nvatts)
          DO i=1,nvatts
            status=nf_inq_attname(ncid1,varid,i,attnam)
            IF (status.eq.nf_noerr) THEN
              latt=lenstr(attnam)
              IF (attnam(1:latt).eq.'add_offset') THEN
                julday1=.true.
              ELSE IF (attnam(1:latt).eq.'calendar') THEN
                status=nf_get_att_text(ncid1,varid,attnam(1:latt),      &
     &                                 calendar)
                IF (status.eq.nf_noerr) THEN
                  IF (calendar(1:9).eq.'gregorian') THEN
                    julday1=.true.
                  END IF
                END IF
              END IF
            END IF
          END DO
!!        IF (.not.julday1.and.(time1(1).gt.365)) THEN
!!          julday1=.true.
!!        END IF
        ELSE
          WRITE (stdout,910) 'time', ncname1(1:lstr1)
          CALL crash ('OPENCDF',status)
        END IF
      END IF
!
!-----------------------------------------------------------------------
!  If unable to read primary time coordinate, inquire the name of time
!  variable associated with the first variable to process.
!-----------------------------------------------------------------------
!
      IF (.not.gottime1) THEN
        vname=vname1(fldid(1))
        lvar=lenstr(vname)
        status=nf_inq_varid(ncid1,vname(1:lvar),varid)
        IF (status.ne.nf_noerr) THEN
          WRITE (stdout,911) vname(1:lvar)
          CALL crash ('OPENCDF',status)
        END IF
        status=nf_inq_var(ncid1,varid,vname,vartyp,nvardim,vardim,      &
     &                    nvatts)
        IF (status.eq.nf_noerr) THEN
          DO i=1,nvatts
            status=nf_inq_attname(ncid1,varid,i,attnam)
            IF (status.eq.nf_noerr) THEN
              latt=lenstr(attnam)
              IF (attnam(1:latt).eq.'time') THEN
                status=nf_inq_attlen(ncid1,varid,attnam(1:latt),        &
     &                               lvar)
                IF (status.ne.nf_noerr) THEN
                  WRITE (stdout,912) attnam(1:latt)
                  CALL crash ('OPENCDF',status)
                END IF
                status=nf_get_att_text(ncid1,varid,attnam(1:latt),      &
     &                                 Tname1(1:lvar))
                IF (status.ne.nf_noerr) THEN
                  WRITE (stdout,913) attnam(1:latt)
                  CALL crash ('OPENCDF',status)
                END IF
                status=nf_inq_varid(ncid1,Tname1(1:lvar),varid)
                IF (status.ne.nf_noerr) THEN
                  WRITE (stdout,914) Tname1(1:lvar)
                  CALL crash ('OPENCDF',status)
                END IF
                status=nf_inq_var(ncid1,varid,Tname1(1:lvar),vartyp,    &
     &                            nvardim,vardim,nvatts)
                IF (status.ne.nf_noerr) THEN
                  WRITE (stdout,911) Tname1(1:lvar)
                  CALL crash ('OPENCDF',status)
                END IF
                status=nf_inq_dim(ncid1,vardim(1),dimnam,ntime1)
                IF (status.ne.nf_noerr) THEN
                  ldim=lenstr(dimnam)
                  WRITE (stdout,915) dimnam(1:ldim), Tname1(1:lvar)
                  CALL crash ('OPENCDF',status)
                END IF
!!              status=nf_get_vara_real(ncid1,varid,1,ntime1,time1)
                status=nf_get_vara_double(ncid1,varid,1,ntime1,time1)
                IF (status.ne.nf_noerr) THEN
                  WRITE (stdout,909) Tname1(1:lvar), ncname1(1:lstr1)
                  CALL crash ('OPENCDF',status)
                END IF
                gottime1=.true.
              END IF
            END IF
          END DO
          IF (.not.gottime1) THEN
            WRITE (stdout,916) vname(1:lvar)
!!          CALL crash ('OPENCDF',status)
          END IF
        ELSE
          WRITE (stdout,917) vname(1:lvar)
          CALL crash ('OPENCDF',status)
        END IF
      END IF
!
!-----------------------------------------------------------------------
!  Report plottable fields in primary file.
!-----------------------------------------------------------------------
!
      CALL what_flds (ncid1,nvars,1,ncname1)
!
!=======================================================================
!  Inquire about the secondary file, if applicable.
!=======================================================================
!
      IF ((iref.eq.2).or.(iref.eq.3).or.(abs(iref).eq.5)) THEN
!
!  Inquire about the contents of input NetCDF file:  Inquire about the
!  dimensions and variables.
!
        status=nf_inq(ncid2,ndims,nvars,ngatts,recdim)
        IF (nvars.gt.maxvar) THEN
          WRITE (stdout,904) maxvar, nvars
          CALL crash ('OPENCDF',0)
        END IF
        IF (status.eq.nf_noerr) THEN
!
!  Inquire and get global "type" attribute.
!
          DO i=1,ngatts
            status=nf_inq_attname(ncid1,nf_global,i,attnam)
            IF (status.eq.nf_noerr) THEN
              latt=lenstr(attnam)
              IF (attnam(1:latt).eq.'type') THEN
                status=nf_get_att_text(ncid1,nf_global,'type',type2)
                IF (status.ne.nf_noerr) THEN
                  WRITE (stdout,903) 'type (global)', ncname1(1:lstr2)
                  CALL crash ('OPENCDF',status)
                END IF
                GO TO 20
              END IF
            ELSE
              WRITE (stdout,904) 'type (global)',ncname2(1:lstr2)
            END IF
          END DO
  20      CONTINUE
!
!  Inquire about dimensions.
!
          DO i=1,ndims
            dimid=i
            status=nf_inq_dim(ncid2,dimid,dimnam,dimsiz)
            IF (status.ne.nf_noerr) THEN
              WRITE (stdout,905) dimid, ncname2(1:lstr2)
              CALL crash ('OPENCDF',status)
            END IF
            ldim=lenstr(dimnam)
            IF ((dimnam(1:ldim).eq.'s_rho').or.                         &
     &          (dimnam(1:ldim).eq.'level')) THEN
              nlev2=dimsiz
            ELSE IF (dimnam(1:ldim).eq.'forecast') THEN
              nfcst2=dimsiz
              status=nf_inq_varid(ncid1,dimnam(1:ldim),varid)
              status=nf_get_vara_real(ncid2,varid,1,nfcst2,fcst2)
            ELSE IF (INDEX(dimnam(1:ldim),'time').ne.0) THEN
              IF (recdim.lt.0) THEN
                ntime2=dimsiz       ! time dimesion is not unlimited
              END IF
            END IF
          END DO
!
!  Inquire about variables.
!
          DO i=1,nvars
            varid=i
            status=nf_inq_var(ncid2,varid,varnam(i),vartyp,nvdims(i),   &
     &                        vdims(1,i),nvatts)
            IF (status.ne.nf_noerr) THEN
              WRITE (stdout,906) varid, ncname2(1:lstr2)
              CALL crash ('OPENCDF',status)
            END IF
            lvar=lenstr(varnam(i))
            IF (varnam(i)(1:lvar).eq.'zeta') THEN
              gotzeta2=.true.
            ELSE IF ((varnam(i)(1:lvar).eq.'rho').and.                   &
     &              (nvdims(i).gt.1)) THEN
              gotrho2=.true.
            ELSE IF (varnam(i)(1:lvar).eq.'salt') THEN
              gotsalt2=.true.
            ELSE IF (varnam(i)(1:lvar).eq.'angle') THEN
              gotangle2=.true.
            ELSE IF ((varnam(i)(1:lvar).eq.'mask_rho').or.               &
     &              (varnam(i)(1:lvar).eq.'mask_u').or.                 &
     &              (varnam(i)(1:lvar).eq.'mask_v')) THEN
              mskinp2=.true.
            ELSE IF (varnam(i)(1:lvar).eq.'Vtransform') THEN
              status=nf_get_var1_int(ncid1,varid,1,Vtransform2)
            ELSE IF (varnam(i)(1:lvar).eq.'Lev') THEN
              status=nf_inq_dim(ncid2,vdims(1,i),dimnam,nlev2)
              status=nf_get_vara_int(ncid2,varid,1,nlev2,lev2)
            ELSE IF (varnam(i)(1:lvar).eq.'scrum_time') THEN
              Tname2='scrum_time'
              gottime2=.true.
              Tscale2=sec2day
            ELSE IF (varnam(i)(1:lvar).eq.'ocean_time') THEN
              Tname2='ocean_time'
              gottime2=.true.
              Tscale2=sec2day
            ELSE IF (varnam(i)(1:lvar).eq.'model_time') THEN
              Tname2='model_time'
              gottime2=.true.
              Tscale2=sec2day
            ELSE IF (varnam(i)(1:lvar).eq.'zout') THEN
              status=nf_get_vara_real(ncid2,varid,1,nlev2,zinp2)
            END IF
          END DO
        ELSE
          WRITE (stdout,907) ncname2(1:lstr1)
          CALL crash ('OPENCDF',status)
        END IF
!
!  Set levels for restart files.
!
      IF ((type2(1:13).eq.'SCRUM restart').or.                          &
     &    (type2(1:13).eq.'ROMS restart')) THEN
        DO i=1,nlev2
          lev2(i)=i
        END DO
      ELSE IF (type1(1:13).eq.'ROMS/TOMS adjoint') THEN
        gotadtime2=.true.
      END IF
!
!-----------------------------------------------------------------------
!  Inquire size of unlimited time record dimension.  Read in time
!  coordinate values.
!-----------------------------------------------------------------------
!
        IF (recdim.gt.0) THEN
          status=nf_inq_dim(ncid2,recdim,dimnam,ntime2)
          IF (status.ne.nf_noerr) THEN
            WRITE (stdout,908) 'time', ncname2(1:lstr2)
            CALL crash ('OPENCDF',status)
          END IF
        ELSE
          dimnam='time'
        END IF
        IF (gottime2) THEN
          lvar=lenstr(Tname2)
          status=nf_inq_varid(ncid2,Tname2(1:lvar),varid)
          IF (status.eq.nf_noerr) THEN
            start(1)=1
            count(1)=ntime2
!!          status=nf_get_vara_real(ncid2,varid,start,count,time2)
            status=nf_get_vara_double(ncid2,varid,start,count,time2)
            IF (status.ne.nf_noerr) THEN
              WRITE (stdout,909) Tname2(1:lvar), ncname2(1:lstr2)
              CALL crash ('OPENCDF',status)
            END IF
          ELSE
            WRITE (stdout,910) Tname2(1:lvar), ncname2(1:lstr2)
            CALL crash ('OPENCDF',status)
          END IF
        ELSE IF ((type2(1:2).eq.'OA').or.(type1(1:4).eq.'DATA')) THEN
          ldim=lenstr(dimnam)
          status=nf_inq_varid(ncid2,dimnam(1:ldim),varid)
          IF (status.eq.nf_noerr) THEN
            start(1)=1
            count(1)=ntime2
!!          status=nf_get_vara_real(ncid2,varid,start,count,time2)
            status=nf_get_vara_double(ncid2,varid,start,count,time2)
            IF (status.ne.nf_noerr) THEN
              WRITE (stdout,909) dimnam(1:ldim), ncname2(1:lstr2)
              CALL crash ('OPENCDF',status)
            END IF
          ELSE
            WRITE (stdout,910) 'time', ncname2(1:lstr2)
            CALL crash ('OPENCDF',status)
          END IF
        ELSE IF (type2(1:7).eq.'Gridpak') THEN
          ntime2=ntime1
          Tscale2=Tscale1
          DO i=1,ntime2
            time2(i)=time1(i)
          END DO
        END IF
!
!-----------------------------------------------------------------------
!  Report plottable fields in secondary file.
!-----------------------------------------------------------------------
!
        CALL what_flds(ncid2,nvars,2,ncname2)
      END IF
!
!=======================================================================
!  Inquire about the grid NetCDF file, if applicable.
!=======================================================================
!
      IF (readgrd) THEN
!
!  Inquire about the contents of input GRID NetCDF file:  Inquire about
!  the dimensions and variables.
!
        status=nf_inq(ncgrd,ndims,nvars,ngatts,recdim)
        IF (nvars.gt.maxvar) THEN
          WRITE (stdout,904) maxvar, nvars
          CALL crash ('OPENCDF',0)
        END IF
        IF (status.eq.nf_noerr) THEN
!
!  Inquire about dimensions.
!
          DO i=1,ndims
            dimid=i
            status=nf_inq_dim(ncgrd,dimid,dimnam,dimsiz)
            IF (status.ne.nf_noerr) THEN
              WRITE (stdout,905) dimid, ncname3(1:lstr3)
              CALL crash ('OPENCDF',status)
            END IF
            ldim=lenstr(dimnam)
            IF (((dimnam(1:ldim).eq.'xi_rho').or.                       &
     &           (dimnam(1:ldim).eq.'lon_rho')).and.                    &
     &           (dimsiz.ne.Lr).and.gotxr) THEN
              WRITE (stdout,918) 'xi_rho', dimsiz, Lr
              CALL crash ('OPENCDF',0)
            ELSE IF (((dimnam(1:ldim).eq.'xi_u').or.                     &
     &               (dimnam(1:ldim).eq.'lon_u')).and.                  &
     &               (dimsiz.ne.Lu).and.gotxu) THEN
              WRITE (stdout,918) 'xi_u', dimsiz, Lu
              CALL crash ('OPENCDF',0)
            ELSE IF (((dimnam(1:ldim).eq.'xi_v').or.                     &
     &               (dimnam(1:ldim).eq.'lon_v')).and.                  &
     &               (dimsiz.ne.Lv).and.gotxv) THEN
              WRITE (stdout,918) 'xi_v', dimsiz, Lv
              CALL crash ('OPENCDF',0)
            ELSE IF (((dimnam(1:ldim).eq.'eta_rho').or.                  &
     &               (dimnam(1:ldim).eq.'lat_rho')).and.                &
     &               (dimsiz.ne.Mr).and.goter) THEN
              WRITE (stdout,918) 'eta_rho', dimsiz, Mr
              CALL crash ('OPENCDF',0)
            ELSE IF (((dimnam(1:ldim).eq.'eta_u').or.                    &
     &               (dimnam(1:ldim).eq.'lat_u')).and.                  &
     &               (dimsiz.ne.Mu).and.goteu) THEN
              WRITE (stdout,918) 'eta_u', dimsiz, Mu
              CALL crash ('OPENCDF',0)
            ELSE IF (((dimnam(1:ldim).eq.'eta_v').or.                    &
     &               (dimnam(1:ldim).eq.'lat_rho')).and.                &
     &               (dimsiz.ne.Mv).and.gotev) THEN
              WRITE (stdout,918) 'eta_v', dimsiz, Mv
              CALL crash ('OPENCDF',0)
            END IF
          END DO
!
!  Inquire about variables.
!
          DO i=1,nvars
            varid=i
            status=nf_inq_var(ncgrd,varid,varnam(i),vartyp,nvdims(i),   &
     &                        vdims(1,i),nvatts)
            IF (status.ne.nf_noerr) THEN
              WRITE (stdout,906) varid, ncname3(1:lstr3)
              CALL crash ('OPENCDF',status)
            END IF
            lvar=lenstr(varnam(i))
            IF ((varnam(i)(1:lvar).eq.'mask_rho').or.                   &
     &          (varnam(i)(1:lvar).eq.'mask_u').or.                     &
     &          (varnam(i)(1:lvar).eq.'mask_v')) THEN
              mskinp1=.true.
            ELSE IF (varnam(i)(1:lvar).eq.'xl') THEN
              status=nf_get_var1_real(ncgrd,varid,1,xbasin)
            ELSE IF (varnam(i)(1:lvar).eq.'el') THEN
              status=nf_get_var1_real(ncgrd,varid,1,ybasin)
            ELSE IF (varnam(i)(1:lvar).eq.'spherical') THEN
              IF (vartyp.eq.nf_int) THEN
                status=nf_get_var1_int(ncgrd,varid,1,ival)
                IF (ival.eq.0) THEN
                  spherical=.false.
                ELSE
                  spherical=.true.
                END IF
              ELSE IF (vartyp.eq.nf_char) THEN
                status=nf_get_var1_text(ncgrd,varid,1,char1)
                IF ((char1.eq.'t').or.(char1.eq.'T')) THEN
                  spherical=.true.
                ELSE
                  spherical=.false.
                END IF
              END IF
            ELSE IF (varnam(i)(1:lvar).eq.'srum_time') THEN
              gottime2=.true.
            END IF
          END DO
        ELSE
          WRITE (stdout,907) ncname3(1:lstr3)
          CALL crash ('OPENCDF',status)
        END IF
      END IF
!
 901  FORMAT (/,' OPENCDF - unable to open input NetCDF file: ',a)
 902  FORMAT (/,' OPENCDF - too small dimension parameter, maxvar = ',  &
     &        2i5,/,11x,'change file  ncscrum.h  and recompile.')
 903  FORMAT (/,' OPENCDF - error while reading attribute: ',a,2x,      &
     &        ' in input NetCDF file: ',a)
 904  FORMAT (/,' OPENCDF - cannot find attribute: ',a,2x,              &
     &        ' in input NetCDF file: ',a)
 905  FORMAT (/,' OPENCDF - error while reading dimension ID: ',i3,2x,  &
     &         ' in input NetCDF file: ',a)
 906  FORMAT (/,' OPENCDF - error while inquiring inFORMAT ion for ',   &
     &        ' variable ID: ',i3,2x,' in input NetCDF file: ',a)
 907  FORMAT (/,' OPENCDF - unable to inquire about contents of',       &
     &        ' input NetCDF file: ',a)
 908  FORMAT (/,' OPENCDF - error inquiring dimension: ',a,2x,          &
     &        ' in input NetCDF file: ',a)
 909  FORMAT (/,' OPENCDF - error while reading variable: ',a,2x,       &
     &        ' in input NetCDF file: ',a)
 910  FORMAT (/,' OPENCDF - cannot find variable: ',a,2x,               &
     &        ' in input NetCDF file: ',a)
 911  FORMAT (/,' GET_PLTDAY - error while inquiring variable: ',a)
 912  FORMAT (/,' GET_PLTDAT - error while inquiring length of',        &
     &        ' attribute: ',a)
 913  FORMAT (/,' GET_PLTDAT - error while reading attribute: ',a)
 914  FORMAT (/,' GET_PLTDAY - error while inquiring ID for variable: ',&
     &        a)
 915  FORMAT (/,' GET_PLTDAT - error while inquiring dimension: ',a,    &
     &        ' for variable: ',a)
 916  FORMAT (/,' GET_PLTDAT - unable to find time coordinate for',     &
     &        ' variable: ',a)
 917  FORMAT (/,' GET_PLTDAY - error while inquiring inFORMAT ion for', &
     &        ' variable: ',a)
 918  FORMAT (/,' OPENCDF - inconsistent dimension ',a,                 &
     &        ' grid and primary NetCDF files: ',2i5)

      RETURN
      END
