      SUBROUTINE get_nc3dat (f,nv2,im,jm,km,ncgid,ncfid,tindx,fldnam,   &
     &                       igrid,ncvarid,spv)
!
!svn $Id: get_nc3dat.F 833 2017-01-24 22:20:16Z arango $
!================================================== Hernan G. Arango ===
!  Copyright (c) 2002-2017 The ROMS/TOMS Group                         !
!    Licensed under a MIT/X style license                              !
!    See License_ROMS.txt                                              !
!=======================================================================
!                                                                      !
!  This routine reads requested 2D field from input NetCDF file.       !
!                                                                      !
!  On Input:                                                           !
!                                                                      !
!     nv2      Allocated size for array "f".                           !
!     ncgid    NetCDF ID for input grid file (integer).                !
!     ncfid    NetCDF ID for input field file (integer).               !
!     tindx    Time dimension index to read (integer).                 !
!     fldnam   Name of field to read (character).                      !
!                                                                      !
!  On Output:                                                          !
!                                                                      !
!     f        Requested 2D field (real array).                        !
!     im       Size of inner dimension (integer).                      !
!     jm       Size of outer dimension (integer).                      !
!     km       Size of vertical dimension (integer).                   !
!     igrid    Staggered C grid type (integer):                        !
!                igrid=0  => None.                                     !
!                igrid=1  => density points.                           !
!                igrid=2  => streamfunction points.                    !
!                igrid=3  => u-velocity points.                        !
!                igrid=4  => v-velocity points.                        !
!                igrid=5  => w-velocity points.                        !
!     ncvarid  Variable ID in input NetCDF file (integer).             !
!     spv      Missing data/fill data value (real).                    !
!                                                                      !
!  Calls:                                                              !
!                                                                      !
!     (NetCDF library)                                                 !
!     crash, lenstr                                                    !
!                                                                      !
!=======================================================================
!
!-----------------------------------------------------------------------
!  Define global data.
!-----------------------------------------------------------------------
!
      implicit none
#include "param.h"
#include "pconst.h"
#include "domdat.h"
#include "fields.h"
#include "fmask.h"
#include "forecast.h"
#include "netcdf.inc"
!
!-----------------------------------------------------------------------
!  Define local data.
!-----------------------------------------------------------------------
!
      logical waterpoints
      integer igrid, im, ip, iw, istr, jm, k, km, mskid, n, nv2, ncfid, &
     &        ncgid, ncvarid, nvatts, nvdims, lenatt, lenfld, lendim,   &
     &        lenvar, lindx, status, tindx, vartyp, wp
      integer count(nf_max_dims), dims(nf_max_dims), start(nf_max_dims),&
     &        vdims(nf_max_dims)
      integer lenstr
      real Vmin, Vmax, scale, spv
      real f(nv2)
      character*20  msknam, varnam, dimnam(nf_max_dims)
      character*60  attnam
      character*(*) fldnam
!
!=======================================================================
!  Begin executable code.
!=======================================================================
!
      im=0
      jm=0
      km=1
      igrid=0
      Vmin=spval1
      Vmax=-spval1
      waterpoints=.false.
      msknam='                    '
      lenfld=lenstr(fldnam)
!
!-----------------------------------------------------------------------
!  Read in requested field.
!-----------------------------------------------------------------------
!
      IF (lenfld.gt.0) THEN
!
!  Inquire ID of requested field in input NetCDF file.
!
        status=nf_inq_varid(ncfid,fldnam(1:lenfld),ncvarid)
        IF (status.ne.nf_noerr) THEN
          PRINT 10, fldnam(1:lenfld)
          CALL crash ('GET_NC3DAT',status)
        END IF
!
!  Inquire information about requested field in input NetCDF file.
!
        status=nf_inq_var(ncfid,ncvarid,varnam,vartyp,nvdims,vdims,     &
     &                    nvatts)
        IF (status.eq.nf_noerr) THEN
          lenvar=lenstr(varnam)
          DO n=1,nvdims
            status=nf_inq_dim(ncfid,vdims(n),dimnam(n),dims(n))
            IF (status.ne.nf_noerr) THEN
              PRINT 20, dimnam(n), varnam(1:lenvar)
              CALL crash ('GET_NC3DAT',status)
            END IF
            lendim=lenstr(dimnam(n))
            IF ((dimnam(n)(1:lendim).eq.'xi_rho').or.                   &
     &          (dimnam(n)(1:lendim).eq.'lon_rho').or.                  &
     &          (dimnam(n)(1:lendim).eq.'lon')) THEN
              igrid=1
              start(n)=1
              count(n)=dims(n)
              im=dims(n)
            ELSE IF ((dimnam(n)(1:lendim).eq.'xi_psi').or.               &
     &              (dimnam(n)(1:lendim).eq.'lon_psi')) THEN
              igrid=2
              start(n)=1
              count(n)=dims(n)
              im=dims(n)
            ELSE IF ((dimnam(n)(1:lendim).eq.'xi_u').or.                 &
     &              (dimnam(n)(1:lendim).eq.'lon_u')) THEN
              igrid=3
              start(n)=1
              count(n)=dims(n)
              im=dims(n)
            ELSE IF ((dimnam(n)(1:lendim).eq.'xi_v').or.                 &
     &              (dimnam(n)(1:lendim).eq.'lon_v')) THEN
              igrid=4
              start(n)=1
              count(n)=dims(n)
              im=dims(n)
            ELSE IF ((dimnam(n)(1:lendim).eq.'eta_rho').or.              &
     &              (dimnam(n)(1:lendim).eq.'lat_rho').or.              &
     &              (dimnam(n)(1:lendim).eq.'lat')) THEN
              igrid=1
              start(n)=1
              count(n)=dims(n)
              jm=dims(n)
            ELSE IF ((dimnam(n)(1:lendim).eq.'eta_psi').or.              &
     &              (dimnam(n)(1:lendim).eq.'lat_psi')) THEN
              igrid=2
              start(n)=1
              count(n)=dims(n)
              jm=dims(n)
            ELSE IF ((dimnam(n)(1:lendim).eq.'eta_u').or.                &
     &              (dimnam(n)(1:lendim).eq.'lat_u')) THEN
              igrid=3
              start(n)=1
              count(n)=dims(n)
              jm=dims(n)
            ELSE IF ((dimnam(n)(1:lendim).eq.'eta_v').or.                &
     &              (dimnam(n)(1:lendim).eq.'lat_v')) THEN
              igrid=4
              start(n)=1
              count(n)=dims(n)
              jm=dims(n)
            ELSE IF ((dimnam(n)(1:lendim).eq.'s_rho').or.                &
     &              (dimnam(n)(1:lendim).eq.'level')) THEN
              lindx=n
              km=dims(n)
            ELSE IF (dimnam(n)(1:lendim).eq.'s_w') THEN
              igrid=5
              lindx=n
              km=dims(n)
            ELSE IF (dimnam(n)(1:lendim).eq.'xy_rho') THEN
              waterpoints=.true.
              igrid=1
              start(n)=1
              count(n)=dims(n)
              wp=dims(n)
              im=Lr
              jm=Mr
            ELSE IF (dimnam(n)(1:lendim).eq.'xy_u') THEN
              waterpoints=.true.
              igrid=3
              start(n)=1
              count(n)=dims(n)
              wp=dims(n)
              im=Lu
              jm=Mu
            ELSE IF (dimnam(n)(1:lendim).eq.'xy_v') THEN
              waterpoints=.true.
              igrid=4
              start(n)=1
              count(n)=dims(n)
              wp=dims(n)
              im=Lv
              jm=Mv
            ELSE IF (dimnam(n)(1:lendim).eq.'xyz_rho') THEN
              waterpoints=.true.
              igrid=1
              start(n)=1
              count(n)=dims(n)
              wp=dims(n)
              im=Lr
              jm=Mr
              km=Nr
            ELSE IF (dimnam(n)(1:lendim).eq.'xyz_u') THEN
              waterpoints=.true.
              igrid=3
              start(n)=1
              count(n)=dims(n)
              wp=dims(n)
              im=Lu
              jm=Mu
              km=Nr
            ELSE IF (dimnam(n)(1:lendim).eq.'xyz_v') THEN
              waterpoints=.true.
              igrid=4
              start(n)=1
              count(n)=dims(n)
              wp=dims(n)
              im=Lv
              jm=Mv
              km=Nr
            ELSE IF (dimnam(n)(1:lendim).eq.'xyz_w') THEN
              waterpoints=.true.
              igrid=5
              start(n)=1
              count(n)=dims(n)
              wp=dims(n)
              im=Lr
              jm=Mr
              km=Nw
            ELSE IF (dimnam(n)(1:lendim).eq.'xybed') THEN
              waterpoints=.true.
              igrid=1
              start(n)=1
              count(n)=dims(n)
              wp=dims(n)
              im=Lr
              jm=Mr
              km=Nbed
            ELSE IF (INDEX(dimnam(n)(1:lendim),'time').ne.0) THEN
              start(n)=tindx
              count(n)=1
            ELSE IF (dimnam(n)(1:lendim).eq.'forecast') THEN
              start(n)=findx
              count(n)=1
            END IF
          END DO
        ELSE
          PRINT 30, fldnam(1:lenfld)
          CALL crash ('GET_NC3DAT',status)
        END IF
!
!  Inquire "missing value" or "fill value" for the requested field.
!  If "scale_factor" attribute is present for a variable, the data
!  are to be multiplied by this factor.
!
        scale=1.0
        DO n=1,nvatts
          status=nf_inq_attname(ncfid,ncvarid,n,attnam)
          IF (status.eq.nf_noerr) THEN
            lenatt=lenstr(attnam)
            IF ((attnam(1:lenatt).eq.'missing_value').or.               &
     &          (attnam(1:lenatt).eq.'_FillValue')) THEN
              status=nf_get_att_real(ncfid,ncvarid,attnam(1:lenatt),spv)
              IF (status.ne.nf_noerr) THEN
                PRINT 40, attnam(1:lenatt), varnam(1:lenvar)
                CALL crash ('GET_NC3DAT',status)
              END IF
            ELSE IF (attnam(1:lenatt).eq.'scale_factor') THEN
              status=nf_get_att_real(ncfid,ncvarid,attnam(1:lenatt),    &
     &                               scale)
              IF (status.ne.nf_noerr) THEN
                PRINT 40, attnam(1:lenatt), varnam(1:lenvar)
                CALL crash ('GET_NC2DAT',status)
              END IF
            ELSE IF (attnam(1:lenatt).eq.'water_points') THEN
              status=nf_get_att_text(ncfid,ncvarid,attnam(1:lenatt),    &
     &                              msknam)
              IF (status.ne.nf_noerr) THEN
                PRINT 40, attnam(1:lenatt), varnam(1:lenvar)
                CALL crash ('GET_NC2DAT',status)
              END IF
            END IF
          END IF
        END DO
!
!  Check if the array size allocated is sufficient.
!
        IF ((im*jm*km).gt.nv2) THEN
          PRINT 50, im*jm*km, nv2
          CALL crash ('GET_NC3DAT',0)
        END IF
!
!  If processing water points only, read in Land/Sea mask.
!
        IF (waterpoints) THEN
          lenvar=lenstr(msknam)
          status=nf_inq_varid(ncgid,msknam(1:lenvar),mskid)
          IF (status.ne.nf_noerr) THEN
            PRINT 10, msknam(1:lenvar)
            CALL crash ('GET_NC2DAT',status)
          END IF
          status=nf_get_var_real(ncgid,mskid,mask)
          IF (status.ne.nf_noerr) THEN
            PRINT 60, msknam(1:lenvar)
            CALL crash ('GET_NC2DAT',status)
          END IF
!
!  If processing water points only, read in field and load it into
!  full array.
!
          status=nf_get_vara_real(ncfid,ncvarid,start,count,f7)
          IF (status.ne.nf_noerr) THEN
            PRINT 60, fldnam(1:lenfld)
            CALL crash ('GET_NC2DAT',status)
          END IF
          ip=0
          iw=0
          DO k=1,km
            DO n=1,im*jm
              ip=ip+1
              IF (mask(n).gt.0.0) THEN
                iw=iw+1
                f(ip)=f7(iw)*scale
                Vmin=MIN(Vmin,f(ip))
                Vmax=MAX(Vmax,f(ip))
              ELSE
                f(ip)=0.0
              END IF
            END DO
          END DO
          PRINT 70, fldnam(1:lenfld), tindx, Vmin, Vmax
        ELSE
!
!  Read in requested full 3D field.
!
          DO k=1,km
            start(lindx)=k
            count(lindx)=1
            istr=1+(k-1)*im*jm
            status=nf_get_vara_real(ncfid,ncvarid,start,count,f(istr))
            IF (status.ne.nf_noerr) THEN
              PRINT 60, fldnam(1:lenfld), k
              CALL crash ('GET_NC3DAT',status)
            END IF
          END DO
          IF ((tindx.gt.0).and.(fldnam(1:lenfld).ne.'h')) THEN
            DO n=1,im*jm*km
              f(n)=f(n)*scale
              Vmin=MIN(Vmin,f(n))
              Vmax=MAX(Vmax,f(n))
            END DO
            PRINT 70, fldnam(1:lenfld), tindx, Vmin, Vmax
          END IF
        END IF
!
!-----------------------------------------------------------------------
!  Requested field not found.
!-----------------------------------------------------------------------
!
      ELSE
        PRINT 80, fldnam(1:lenfld)
        CALL crash ('GET_NC3DAT',status)
      END IF
!
  10  FORMAT (/' GET_NC3DAT - cannot inquiring ID for variable: ',a)
  20  FORMAT (/' GET_NC3DAT - error while inquiring dimension: ',a,2x,  &
     &         ' for variable: ',a)
  30  FORMAT (/' GET_NC3DAT - error while inquiring information for',   &
     &         ' variable: ',a)
  40  FORMAT (/' GET_NC3DAT - error while reading attribute: ',a,2x,    &
     &         ' for variable: ',a)
  50  FORMAT (/' GET_NC3DAT - required array size = ',i10,/,13x,        &
     &         ' is bigger than that allocated = ',i10,/,13x,           &
     &         ' Change parameter NV in  param.h  and recompile.')
  60  FORMAT (/' GET_NC3DAT - error while reading variable: ',a,2x,     &
     &         ' at LEVEL = ',i3)
  70  FORMAT (5x,a,t20,'Rec = ',i5.5,1x,'Min = ',1pe15.8,               &
     &        ' Max = ',1pe15.8)
  80  FORMAT (/' GET_NC3DAT - cannot find variable: ',a,/,14x,          &
     &         ' Requested variable may not have z-dimension.')

      RETURN
      END
