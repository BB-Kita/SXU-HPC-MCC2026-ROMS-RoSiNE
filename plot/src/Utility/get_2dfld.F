     SUBROUTINE get_2dfld (day,xpos,ypos,s1,s2,np,file,idfld,level)
!
!svn $Id: get_2dfld.F 833 2017-01-24 22:20:16Z arango $
!================================================== Hernan G. Arango ===
!  Copyright (c) 2002-2017 The ROMS/TOMS Group                         !
!    Licensed under a MIT/X style license                              !
!    See License_ROMS.txt                                              !
!=======================================================================
!                                                                      !
!  This routine reads in the requested field, at the requested level   !
!  and time from input NetCDF file.                                    !
!                                                                      !
!  On Input:                                                           !
!                                                                      !
!     day      Day to read (real)                                      !
!     file     NetCDF file to read (primary or secondary; integer)     !
!     idfld    Identification of field to read (integer)               !
!     level    Depth (>0), level (>0), or isopycnal (>1000) to         !
!              process (real).                                         !
!                                                                      !
!  On Output:                                                          !
!                                                                      !
!     xpos     X-positions of the requested field (real array)         !
!     ypos     Y-positions of the requested field (real array)         !
!     s1       Scalar or vector x-component field (real array)         !
!     s2       Vector y-component (real array)                         !
!     np       Number of points in requested field (integer)           !
!                                                                      !
!  Calls:                                                              !
!                                                                      !
!    crash, cvector, get_ape, get_bvf, get_ertel, get_nc2dat,          !
!    get_nc2pos, get_rig, get_3dfld, idens, mskscl, svel,              !
!    time_indx, xhslice                                                !
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
#include "fields.h"
#include "fldsid.h"
#include "ncinfo.h"
#include "ndimen.h"
#include "pltfld.h"
#include "pltio.h"
#include "pltlab.h"
#include "pltncar.h"
#include "secbath.h"
!
!-----------------------------------------------------------------------
!  Define local data.
!-----------------------------------------------------------------------
!
      logical gotangle, gotrho, gotsalt, gotzeta
      integer file, idfld, igrid, ixgrid, iygrid, imx, imy, ip, jmx,    &
     &        jmy, n, nc_fid, nc_gid, nc_vid, nc_vxid, nc_vyid, np,     &
     &        status
      real day, level, scle, spv, xspv, yspv
      real s1(NV), s2(NV), xpos(NV), ypos(NV)
      real idens, svel
      character*4 fldnam
!
!=======================================================================
!  Begin executable code.
!=======================================================================
!
!  Determine NetCDF ID for input file to read and time index of the
!  field to read.
!
      gotangle=.false.
      gotrho=.false.
      gotsalt=.false.
      gotzeta=.false.
      isosurf=.false.
      IF (file.eq.1) THEN
        nc_fid=ncid1
        IF (readgrd) THEN
          nc_gid=ncgrd
          gotangle=.true.
        ELSE
          nc_gid=ncid1
          gotangle=gotangle1
        END IF
        gotrho=gotrho1
        gotsalt=gotsalt1
        gotzeta=gotzeta1
        IF (Lrecord) THEN
          tindx=trecord
        ELSE
          CALL time_indx (var_time1,tindx,day,idfld,file)
        END IF
      ELSE IF (file.eq.2) THEN
        nc_fid=ncid2
        IF (readgrd) THEN
          nc_gid=ncgrd
          gotangle=.true.
        ELSE
          nc_gid=ncid2
          gotangle=gotangle2
        END IF
        gotrho=gotrho2
        gotsalt=gotsalt2
        gotzeta=gotzeta2
        IF (Lrecord) THEN
          tindx=trecord
        ELSE
          CALL time_indx (var_time2,tindx,day,idfld,file)
        END IF
      ELSE
        WRITE (stdout,10) file
  10    FORMAT (/' GET_2DFLD - unable to determine file type in input', &
     &          ' NetCDF file: ',i2)
        CALL crash ('GET_2DFLD',1)
      END IF
!
!=======================================================================
!  Process requested field.  First, determine the field type (1: scalar,
!  2: vector, 3: derived). Second, read field(s); if the requested
!  field is secondary, then compute it from just read primary field(s).
!  Third, mask/scale read field(s) if appropriate. And fourth, read
!  field positions.  If "level" is positive, it processes fields at the
!  requested level.  However, if "level" is negative, it processes
!  fields at the requested depth via interpolation.
!=======================================================================
!
!  Determine field(s) type and get field(s) name.
!
      levless=is2d(idfld)
      status=vtype(idfld)
      fldnam1=vname1(idfld)
      fldnam2=vname2(idfld)
      fldlab=vlabel(idfld)
      ptype=0
!
!=======================================================================
!  Process field by levels.
!=======================================================================
!
      spv=spval1
      IF (levless.or.((c0.lt.level).and.(level.lt.c1000))) THEN
        plotit=.true.
!
!-----------------------------------------------------------------------
!  2D scalar field.
!-----------------------------------------------------------------------
!
        IF (status.eq.1) THEN
!
!  Compute bathymetry at PSI-points.
!
          IF (vcode(idfld).eq.'IDBTHP') THEN
            CALL get_nc2dat (f5,NV,imx,jmx,nc_gid,nc_fid,tindx,fldnam1, &
     &                       level,igrid,nc_vid,spv)
            im=imx-1
            jm=jmx-1
            igrid=2
            CALL cfield (s1,im,jm,1,f5,imx,jmx,1,igrid)
!
!  Generic scalar fields.
!
          ELSE
            CALL get_nc2dat (s1,NV,im,jm,nc_gid,nc_fid,tindx,fldnam1,   &
     &                       level,igrid,nc_vid,spv)
          END IF
          CALL mskscl (s1,im,jm,1,nc_gid,idfld,igrid,spv,spval1)
          IF (iref.eq.6) THEN
            CALL mskerr (s1,im,jm,1,file,nc_gid,nc_fid,tindx,level,     &
     &                   idover,igrid,rmax,spv,spval1)
          END IF
          ptype=pscalar
!
!-----------------------------------------------------------------------
!  2D vector field.
!-----------------------------------------------------------------------
!
        ELSE IF (status.eq.2) THEN
          CALL get_nc2dat (f5,NV,imx,jmx,nc_gid,nc_fid,tindx,fldnam1,   &
     &                     level,ixgrid,nc_vxid,xspv)
          CALL get_nc2dat (f6,NV,imy,jmy,nc_gid,nc_fid,tindx,fldnam2,   &
     &                     level,iygrid,nc_vyid,yspv)
!
!  Compute baroclinic velocity components.
!
          IF ((vcode(idfld).eq.'IDCMAG').or.                            &
     &        (vcode(idfld).eq.'IDCVEC')) THEN
            fldnam='ubar'
            CALL get_nc2dat (s1,NV,imx,jmx,nc_gid,nc_fid,tindx,fldnam,  &
     &                       level,ixgrid,nc_vxid,xspv)
            fldnam='vbar'
            CALL get_nc2dat (s2,NV,imy,jmy,nc_gid,nc_fid,tindx,fldnam,  &
     &                       level,iygrid,nc_vyid,yspv)
            DO n=1,imx*jmx
              f5(n)=f5(n)-s1(n)
            END DO
            DO n=1,imy*jmy
              f6(n)=f6(n)-s2(n)
            END DO
          END IF
!
!  Compute vector components at interior RHO-points or PSI-points.
!
          CALL mskscl (f5,imx,jmx,1,nc_gid,idfld,ixgrid,xspv,c0)
          CALL mskscl (f6,imy,jmy,1,nc_gid,idfld,iygrid,yspv,c0)
          IF ((ixgrid.eq.1).and.(iygrid.eq.1)) THEN
            igrid=1
          ELSE
            IF (lvecrho) THEN
              igrid=7
            ELSE
              igrid=2
            END IF
          END IF
          CALL cvector (s1,s2,im,jm,1,f5,imx,jmx,f6,imy,jmy,igrid,      &
     &                  nc_gid,gotangle)
          IF (iref.eq.6) THEN
            CALL mskerr (s1,im,jm,1,file,nc_gid,nc_fid,tindx,level,     &
     &                   idover,igrid,rmax,spv,c0)
            CALL mskerr (s2,im,jm,1,file,nc_gid,nc_fid,tindx,level,     &
     &                   idover,igrid,rmax,spv,c0)
          END IF
!
!  Compute vector magnitude.  Set plot type.
!
          IF ((vcode(idfld).eq.'IDTMAG').or.                            &
     &        (vcode(idfld).eq.'ID3MAG').or.                            &
     &        (vcode(idfld).eq.'IDCMAG').or.                            &
     &        (vcode(idfld).eq.'IDBMAG').or.                            &
     &        (vcode(idfld).eq.'ID2MAG').or.                            &
     &        (vcode(idfld).eq.'IDIMAG').or.                            &
     &        (vcode(idfld).eq.'IDSMSM').or.                            &
     &        (vcode(idfld).eq.'IDBMSM').or.                            &
     &        (vcode(idfld).eq.'IDBWSM').or.                            &
     &        (vcode(idfld).eq.'IDSWDM').or.                            &
     &        (vcode(idfld).eq.'IDBEDM').or.                            &
     &        (vcode(idfld).eq.'IDBOTM').or.                            &
     &        (vcode(idfld).eq.'IDSURM').or.                            &
     &        (vcode(idfld).eq.'DM2PGM').or.                            &
     &        (vcode(idfld).eq.'DM2COM').or.                            &
     &        (vcode(idfld).eq.'DM2HAM').or.                            &
     &        (vcode(idfld).eq.'DM2SSM').or.                            &
     &        (vcode(idfld).eq.'DM2BSM').or.                            &
     &        (vcode(idfld).eq.'DM2HVM').or.                            &
     &        (vcode(idfld).eq.'DM2HRM').or.                            &
     &        (vcode(idfld).eq.'DM2ACM').or.                            &
     &        (vcode(idfld).eq.'DM3PGM').or.                            &
     &        (vcode(idfld).eq.'DM3COM').or.                            &
     &        (vcode(idfld).eq.'DM3HAM').or.                            &
     &        (vcode(idfld).eq.'DM3VAM').or.                            &
     &        (vcode(idfld).eq.'DM3HVM').or.                            &
     &        (vcode(idfld).eq.'DM3VVM').or.                            &
     &        (vcode(idfld).eq.'DM3HRM').or.                            &
     &        (vcode(idfld).eq.'DM3VRM').or.                            &
     &        (vcode(idfld).eq.'DM3ACM')) THEN
            DO n=1,im*jm
              IF ((s1(n).ne.spv).and.(s2(n).ne.spv)) THEN
                s1(n)=SQRT(s1(n)*s1(n)+s2(n)*s2(n))
              END IF
            END DO
            ptype=pscalar
          ELSE IF ((vcode(idfld).eq.'IDTSTR').or.                       &
     &             (vcode(idfld).eq.'ID3STR').or.                       &
     &             (vcode(idfld).eq.'IDCSTR').or.                       &
     &             (vcode(idfld).eq.'IDBSTR').or.                       &
     &             (vcode(idfld).eq.'ID2STR').or.                       &
     &             (vcode(idfld).eq.'IDISTR').or.                       &
     &             (vcode(idfld).eq.'IDSMSS').or.                       &
     &             (vcode(idfld).eq.'IDBMSS').or.                       &
     &             (vcode(idfld).eq.'IDBWSS').or.                       &
     &             (vcode(idfld).eq.'IDSWDS').or.                       &
     &             (vcode(idfld).eq.'IDBEDS').or.                       &
     &             (vcode(idfld).eq.'IDBOTS').or.                       &
     &             (vcode(idfld).eq.'IDSSTR').or.                       &
     &             (vcode(idfld).eq.'IDRSMS').or.                       &
     &             (vcode(idfld).eq.'DM2PGS').or.                       &
     &             (vcode(idfld).eq.'DM2COS').or.                       &
     &             (vcode(idfld).eq.'DM2HAS').or.                       &
     &             (vcode(idfld).eq.'DM2SSS').or.                       &
     &             (vcode(idfld).eq.'DM2BSS').or.                       &
     &             (vcode(idfld).eq.'DM2HVS').or.                       &
     &             (vcode(idfld).eq.'DM2HRS').or.                       &
     &             (vcode(idfld).eq.'DM2ACS').or.                       &
     &             (vcode(idfld).eq.'DM3PGS').or.                       &
     &             (vcode(idfld).eq.'DM3COS').or.                       &
     &             (vcode(idfld).eq.'DM3HAS').or.                       &
     &             (vcode(idfld).eq.'DM3VAS').or.                       &
     &             (vcode(idfld).eq.'DM3HVS').or.                       &
     &             (vcode(idfld).eq.'DM3VVS').or.                       &
     &             (vcode(idfld).eq.'DM3HRS').or.                       &
     &             (vcode(idfld).eq.'DM3VRS').or.                       &
     &             (vcode(idfld).eq.'DM3ACS')) THEN
            ptype=pstream
          ELSE IF ((vcode(idfld).eq.'IDTBRB').or.                       &
     &             (vcode(idfld).eq.'ID3BRB').or.                       &
     &             (vcode(idfld).eq.'IDCBRB').or.                       &
     &             (vcode(idfld).eq.'IDBBRB').or.                       &
     &             (vcode(idfld).eq.'ID2BRB').or.                       &
     &             (vcode(idfld).eq.'IDIBRB').or.                       &
     &             (vcode(idfld).eq.'IDSMSB').or.                       &
     &             (vcode(idfld).eq.'IDBMSB').or.                       &
     &             (vcode(idfld).eq.'IDBWSB').or.                       &
     &             (vcode(idfld).eq.'IDSWDB').or.                       &
     &             (vcode(idfld).eq.'IDBEDB').or.                       &
     &             (vcode(idfld).eq.'IDBOTB').or.                       &
     &             (vcode(idfld).eq.'IDSBRB').or.                       &
     &             (vcode(idfld).eq.'IDRSMB').or.                       &
     &             (vcode(idfld).eq.'DM2PGB').or.                       &
     &             (vcode(idfld).eq.'DM2COB').or.                       &
     &             (vcode(idfld).eq.'DM2HAB').or.                       &
     &             (vcode(idfld).eq.'DM2SSB').or.                       &
     &             (vcode(idfld).eq.'DM2BSB').or.                       &
     &             (vcode(idfld).eq.'DM2HVB').or.                       &
     &             (vcode(idfld).eq.'DM2HRB').or.                       &
     &             (vcode(idfld).eq.'DM2ACB').or.                       &
     &             (vcode(idfld).eq.'DM3PGB').or.                       &
     &             (vcode(idfld).eq.'DM3COB').or.                       &
     &             (vcode(idfld).eq.'DM3HAB').or.                       &
     &             (vcode(idfld).eq.'DM3VAB').or.                       &
     &             (vcode(idfld).eq.'DM3HVB').or.                       &
     &             (vcode(idfld).eq.'DM3VVB').or.                       &
     &             (vcode(idfld).eq.'DM3HRB').or.                       &
     &             (vcode(idfld).eq.'DM3VRB').or.                       &
     &             (vcode(idfld).eq.'DM3ACB')) THEN
            ptype=pwbarbs
          ELSE
            ptype=pvector
          END IF
!
!-----------------------------------------------------------------------
!  Secondary field derived from two primary fields.
!-----------------------------------------------------------------------
!
        ELSE IF (status.eq.3) THEN
          CALL get_nc2dat (s1,NV,im,jm,nc_gid,nc_fid,tindx,fldnam1,     &
     &                     level,igrid,nc_vxid,xspv)
          CALL get_nc2dat (s2,NV,im,jm,nc_gid,nc_fid,tindx,fldnam2,     &
     &                     level,igrid,nc_vyid,yspv)
!
!  Compute density anomaly from temperature and salinity.
!
          IF (vcode(idfld).eq.'IDCDEN') THEN
            CALL get_nc3pos (z,NV,im,jm,Nr,nc_gid,nc_fid,tindx,igrid,   &
     &                       igrid,gotzeta,file)
            DO n=1,im*jm
              ip=n+(INT(level)-1)*im*jm
              IF ((s1(n).ne.spv).and.(s2(n).ne.spv)) THEN
                s1(n)=idens(s1(n),s2(n),z(ip))
              ELSE
                s1(n)=spval1
              END IF
            END DO
            ptype=pscalar
!
!  Compute Coriolis/Depth ratio (1/ms).
!
          ELSE IF (vcode(idfld).eq.'IDFOHR') THEN
            DO n=1,im*jm
              IF ((s1(n).ne.spv).and.(s2(n).ne.spv).and.                &
     &            (s2(n).ne.c0)) THEN
                s1(n)=s1(n)/s2(n)
              ELSE
                s1(n)=spval1
              END IF
            END DO
            ptype=pscalar
!
!  Compute local snow thickness (m).
!
          ELSE IF (vcode(idfld).eq.'IDSHOA') THEN
            DO n=1,im*jm
              IF ((s1(n).ne.spv).and.(s2(n).ne.spv).and.                &
     &            (s2(n).ne.c0)) THEN
                s1(n)=s1(n)/s2(n)
              ELSE
                s1(n)=spval1
              END IF
            END DO
            ptype=pscalar
!
!  Compute local ice thickness (m).
!
          ELSE IF (vcode(idfld).eq.'IDIHOA') THEN
            DO n=1,im*jm
              IF ((s1(n).ne.spv).and.(s2(n).ne.spv).and.                &
     &            (s2(n).ne.c0)) THEN
                s1(n)=s1(n)/s2(n)
              ELSE
                s1(n)=spval1
              END IF
            END DO
            ptype=pscalar
!
!  Compute sigma-t from temperature and salinity.
!
          ELSE IF (vcode(idfld).eq.'IDSIGT') THEN
            DO n=1,im*jm
              IF ((s1(n).ne.spv).and.(s2(n).ne.spv)) THEN
                s1(n)=idens(s1(n),s2(n),c0)
              ELSE
                s1(n)=spval1
              END IF
            END DO
            ptype=pscalar
!
!  Compute sound speed (m/s) from temperature and salinity.
!
          ELSE IF (vcode(idfld).eq.'IDSNDS') THEN
            CALL get_nc3pos (z,NV,im,jm,Nr,nc_gid,nc_fid,tindx,igrid,   &
     &                       igrid,gotzeta,file)
            DO n=1,im*jm
              IF ((s1(n).ne.spv).and.(s2(n).ne.spv)) THEN
                ip=n+(INT(level)-1)*im*jm
                s1(n)=svel(s2(n),s1(n),ABS(z(ip)))
              ELSE
                s1(n)=spval1
              END IF
            END DO
            ptype=pscalar
          END IF
          CALL mskscl (s1,im,jm,1,nc_gid,idfld,igrid,spv,spval1)
          IF (iref.eq.6) THEN
            CALL mskerr (s1,im,jm,1,file,nc_gid,nc_fid,tindx,level,     &
     &                   idover,igrid,rmax,spv,spval1)
          END IF
!
!-----------------------------------------------------------------------
!  Special fields: vector or scalar.
!-----------------------------------------------------------------------
!
        ELSE IF ((status.eq.4).or.(status.eq.5)) THEN
!
!  Depths of requested field iso-surface: temperature, salinity,
!  "in situ" density, sigma-t, or potential density.
!
          IF ((vcode(idfld).eq.'IDISOT').or.                            &
     &        (vcode(idfld).eq.'IDISOH').or.                            &
     &        (vcode(idfld).eq.'IDISOR').or.                            &
     &        (vcode(idfld).eq.'IDISOD').or.                            &
     &        (vcode(idfld).eq.'IDISOS')) THEN
            CALL get_isosur (s1,xpos,ypos,im,jm,igrid,nc_gid,nc_fid,    &
     &                       idfld,tindx,gotzeta,gotsalt,gotrho,file)
            ptype=pscalar
!
!  Maximum entropy equilibrium transport.
!
          ELSE IF (vcode(idfld).eq.'IDNPYT') THEN
            CALL get_entropy (s1,s2,f5,xpos,ypos,im,jm,igrid,nc_gid,    &
     &                        idfld,gotangle)
!
!  Maximum entropy equilibrium velocity.
!
          ELSE IF ((vcode(idfld).eq.'IDUNPY').or.                       &
     &             (vcode(idfld).eq.'IDVNPY').or.                       &
     &             (vcode(idfld).eq.'IDNPYV').or.                       &
     &             (vcode(idfld).eq.'IDNPYM')) THEN
            CALL get_entropy (f5,s1,s2,xpos,ypos,im,jm,igrid,nc_gid,    &
     &                        idfld,gotangle)
!
!  Joint Effect of Baroclinicity and bottom Relief (JEBAR).
!
          ELSE IF ((vcode(idfld).eq.'IDMHUJ').or.                       &
     &             (vcode(idfld).eq.'IDMHVJ').or.                       &
     &             (vcode(idfld).eq.'IDMHJV').or.                       &
     &             (vcode(idfld).eq.'IDMHJM').or.                       &
     &             (vcode(idfld).eq.'IDRHUJ').or.                       &
     &             (vcode(idfld).eq.'IDRHVJ').or.                       &
     &             (vcode(idfld).eq.'IDRHJV').or.                       &
     &             (vcode(idfld).eq.'IDRHJM')) THEN
            CALL get_jebar (s1,s2,xpos,ypos,z,im,jm,igrid,nc_gid,       &
     &                      nc_fid,idfld,tindx,gotzeta,gotsalt,gotrho,  &
     &                      gotangle,file)
!
!  Brunt-Vaisala frequency.
!
          ELSE IF (vcode(idfld).eq.'IDBVFQ') THEN
            CALL get_bvf (s1,xpos,ypos,z,im,jm,km,nc_gid,nc_fid,idfld,  &
     &                    tindx,level,gotzeta,gotsalt,gotrho,file)
            ptype=pscalar
!
!  Horizontal divergence fields.
!
          ELSE IF ((vcode(idfld).eq.'IDTVHD').or.                       &
     &             (vcode(idfld).eq.'IDBVHD').or.                       &
     &             (vcode(idfld).eq.'IDCVHD').or.                       &
     &             (vcode(idfld).eq.'IDSWHD')) THEN
            CALL get_hdiv (s1,xpos,ypos,z,im,jm,km,igrid,nc_gid,nc_fid, &
     &                     idfld,tindx,level,gotzeta,file)
            ptype=pscalar
!
!  Relative vorticity fields.
!
          ELSE IF ((vcode(idfld).eq.'IDTVRV').or.                       &
     &             (vcode(idfld).eq.'IDBVRV').or.                       &
     &             (vcode(idfld).eq.'IDCVRV').or.                       &
     &             (vcode(idfld).eq.'IDSWRV')) THEN
            CALL get_rvor (s1,xpos,ypos,z,im,jm,km,igrid,nc_gid,nc_fid, &
     &                     idfld,tindx,level,gotzeta,file)
            ptype=pscalar
!
!  Potential vorticity
!
          ELSE IF (vcode(idfld).eq.'IDPVOR') THEN
            CALL get_pv (s1,xpos,ypos,z,im,jm,km,igrid,nc_gid,nc_fid,   &
     &                   idfld,tindx,level,gotzeta,gotsalt,gotrho,file)
            ptype=pscalar
!
!  Ertel potential vorticity.
!
          ELSE IF (vcode(idfld).eq.'IDERPV') THEN
            CALL get_ertel (s1,xpos,ypos,z,im,jm,km,igrid,nc_gid,       &
     &                      nc_fid,idfld,tindx,level,gotzeta,file)
            ptype=pscalar
!
!  Available potential energy.
!
          ELSE IF (vcode(idfld).eq.'IDPENG') THEN
            CALL get_ape (s1,xpos,ypos,z,im,jm,km,igrid,nc_gid,nc_fid,  &
     &                    idfld,tindx,level,gotzeta,gotsalt,gotrho,file)
            ptype=pscalar
!
!  Dynamic pressure.
!
          ELSE IF (vcode(idfld).eq.'IDDPRS') THEN
            CALL get_dpres (s1,xpos,ypos,z,im,jm,km,igrid,nc_gid,       &
     &                      nc_fid,idfld,tindx,level,gotzeta,gotsalt,   &
     &                      gotrho,file)
            ptype=pscalar
!
!  Bernoulli function.
!
          ELSE IF (vcode(idfld).eq.'IDBERN') THEN
            CALL get_bernoulli (s1,xpos,ypos,z,im,jm,km,igrid,nc_gid,   &
     &                          nc_fid,idfld,tindx,level,gotzeta,       &
     &                          gotsalt,gotrho,file)
            ptype=pscalar
!
!  Bathymetry slope or bathymetry r-factor.
!
          ELSE IF ((vcode(idfld).eq.'IDMHSL').or.                       &
     &             (vcode(idfld).eq.'IDRHSL').or.                       &
     &             (vcode(idfld).eq.'IDMHRF').or.                       &
     &             (vcode(idfld).eq.'IDRHRF')) THEN
            CALL get_hslope (s1,xpos,ypos,im,jm,igrid,nc_gid,nc_fid,    &
     &                       idfld,tindx,gotzeta)
            ptype=pscalar
!
!  Gradient Richardson number.
!
          ELSE IF (vcode(idfld).eq.'IDRIGN') THEN
            CALL get_rig (s1,xpos,ypos,z,im,jm,km,igrid,nc_gid,nc_fid,  &
     &                    idfld,tindx,level,gotzeta,gotsalt,gotrho,file)
            ptype=pscalar
!
!  Surface/Bottom momentum stress curl or Ekman pumping.
!
          ELSE IF (vcode(idfld).eq.'IDSMSC'.or.                         &
     &             vcode(idfld).eq.'IDBMSC'.or.                         &
     &             vcode(idfld).eq.'IDBWSC'.or.                         &
     &             vcode(idfld).eq.'IDSMEK'.or.                         &
     &             vcode(idfld).eq.'IDBMEK'.or.                         &
     &             vcode(idfld).eq.'IDBWEK') THEN
            CALL get_wcurl (s1,xpos,ypos,im,jm,igrid,nc_gid,nc_fid,     &
     &                      idfld,tindx)
            ptype=pscalar
!
!  Total chlorophyll pigment concentration.
!
          ELSE IF (vcode(idfld).eq.'IDCOL1'.or.                         &
     &             vcode(idfld).eq.'IDCOL3'.or.                         &
     &             vcode(idfld).eq.'IDCOL4'.or.                         &
     &             vcode(idfld).eq.'IDCOL5') THEN
            CALL get_pig (s1,xpos,ypos,z,im,jm,km,igrid,nc_gid,nc_fid,  &
     &                    idfld,tindx,level,gotzeta,file)
            ptype=pscalar
          END IF
!
!  If appropriate, scale and apply Land/Sea mask.
!
          CALL mskscl (s1,im,jm,1,nc_gid,idfld,igrid,spv,spval1)
          IF ((status.eq.5).or.(status.eq.6)) THEN
            CALL mskscl (s2,im,jm,1,nc_gid,idfld,igrid,spv,spval1)
          END IF
        ELSE
          plotit=.false.
        END IF
!
!  Read in field positions.
!
        IF ((1.le.status).and.(status.le.3).or.(status.eq.6)) THEN
          CALL get_nc2pos (xpos,ypos,NV,im,jm,nc_gid,igrid)
        END IF
!
!  Set number of points in requested field.
!
        np=im*jm
!
!=======================================================================
!  Process field by depths or depth of isopycnal; intepolate vertically.
!=======================================================================
!
      ELSE IF ((level.le.c0).or.(level.ge.c1000)) THEN
        interpolate=.true.
        IF (level.ge.c1000) THEN
          CALL zisopyc (day,ziso,s1,s2,file,level,idfld)
        ELSE
          scle=c1                              ! meters
          IF (coorscl.eq.2) THEN
            scle=m2cm                          ! meters to centimeters
          ELSE IF (coorscl.eq.3) THEN
            scle=m2mm                          ! meters to milimeters
          END IF
          np=Lr*Mr
          DO n=1,np
            depth(n)=scle*level
          END DO
        END IF
        CALL get_3dfld (day,xpos,ypos,z,s1,s2,np,igrid,file,idfld,level)
        IF (interpolate) THEN
          IF (level.ge.c1000) THEN
            CALL cfield (depth,im,jm,1,ziso,Lr,Mr,1,igrid)
          END IF
          IF (plotit) THEN
            IF ((status.eq.2).or.(status.eq.5).or.(status.eq.6)) THEN
              DO n=1,np
                f5(n)=s1(n)
                f6(n)=s2(n)
              END DO
              CALL xhslice (s1,depth,z,f5,im,jm,km,vintrp,spval1)
              CALL xhslice (s2,depth,z,f6,im,jm,km,vintrp,spval1)
            ELSE
              DO n=1,np
                f5(n)=s1(n)
              END DO
              CALL xhslice (s1,depth,z,f5,im,jm,km,vintrp,spval1)
            END IF
          END IF
        END IF
      END IF

      RETURN
      END
