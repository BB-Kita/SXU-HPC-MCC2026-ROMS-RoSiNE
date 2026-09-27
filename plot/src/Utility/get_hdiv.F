      SUBROUTINE get_hdiv (hdiv,xpos,ypos,zpos,im,jm,km,igrid,ncgid,    &
     &                     ncfid,idfld,tindex,level,gotzeta,file)
!
!svn $Id: get_hdiv.F 833 2017-01-24 22:20:16Z arango $
!================================================== Hernan G. Arango ===
!  Copyright (c) 2002-2017 The ROMS/TOMS Group                         !
!    Licensed under a MIT/X style license                              !
!    See License_ROMS.txt                                              !
!=======================================================================
!                                                                      !
!  This subroutine computes horizontal divergence from primary         !
!  fields in input NetCDF file.                                        !
!                                                                      !
!  On Input:                                                           !
!                                                                      !
!     ncgid    NetCDF ID for input grid (integer).                     !
!     ncfid    NetCDF ID for input fields (integer).                   !
!     idfld    Identification of field to read (integer)               !
!     tindex   Time dimension index to read (integer).                 !
!     level    Depth (>0), level (>0), or isopycnal (>1000) to         !
!              process (real). If LEVEL=0, the full 3D is passed.      !
!     gotzeta  Logical switch indicating availability of free-         !
!              surface.                                                !
!     file     NetCDF file to read (primary or secondary; integer).    !
!                                                                      !
!  On Output:                                                          !
!                                                                      !
!     hdiv     Requested horizontal divergence (real array).           !
!     im       Size of inner dimension (integer).                      !
!     jm       Size of outer dimension (integer).                      !
!     km       Size of vertical dimension (integer).                   !
!     igrid    Staggered C-grid type for requested field (integer).    !
!     xpos     X-positions of the requested field (real array).        !
!     ypos     Y-positions of the requested field (real array).        !
!     zpos     Z-positions of the requested field (real array).        !
!                                                                      !
!  Calls:    get_nc2dat, get_nc3dat, get_nc2pos, get_nc3pos, hordiv    !
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
#include "pltfld.h"
#include "pltlab.h"
#include "secbath.h"
!
!-----------------------------------------------------------------------
!  Define local variables.
!-----------------------------------------------------------------------
!
      logical gotzeta
      integer file, i, idfld, igrid, ih, ihgrid, im, imr, imu, imv, ip, &
     &        ir, irgrid, iugrid, iv, ivgrid, ivp, iwgrid, j, jm, jmr,  &
     &        jmu, jmv, k, km, kmr, ncfid, nchid, ncgid, nczid, nc_pid, &
     &        nc_uid, nc_vid, np, tindex
      real level, hspv, ods, spv, uspv, vspv, zspv
      real hdiv(NV), xpos(NV), ypos(NV), zpos(NV)
      character*20 fldnam, hposnam, zposnam
!
!=======================================================================
!  Begin executable code.
!=======================================================================
!
!  Determine vector field to process.
!
      levless=is2d(idfld)
      fldnam1=vname1(idfld)
      fldnam2=vname2(idfld)
!
!  Read in U-component and store in f2.
!
      IF (levless) THEN
        CALL get_nc2dat (f2,NV,imu,jmu,ncgid,ncfid,tindex,fldnam1,0,    &
     &                   iugrid,nc_uid,uspv)
      ELSE
        CALL get_nc3dat (f2,NV,imu,jmu,kmr,ncgid,ncfid,tindex,fldnam1,  &
     &                   iugrid,nc_uid,uspv)
      END IF
      IF (vcode(idfld).eq.'IDCVHD') THEN
        fldnam='ubar'
        CALL get_nc2dat (f4,NV,imu,jmu,ncgid,ncfid,tindex,fldnam,0,     &
     &                   iugrid,nc_uid,uspv)
        DO k=1,kmr
          DO j=1,jmu
            DO i=1,imu
              ih=i+(j-1)*imu
              iv=ih+(k-1)*imu*jmu
              f2(iv)=f2(iv)-f4(ih)
            END DO
          END DO
        END DO
      END IF
!
!  If appropriate, interpolate U-component to requested depth or depths
!  of isopycnal.
!
      IF (.not.levless.and.(level.le.c0).or.(level.ge.c1000)) THEN
        irgrid=1
        CALL get_nc3pos(zpos,NV,imu,jmu,kmr,ncgid,ncfid,tindex,iugrid,  &
     &                  irgrid,gotzeta,file)
        IF (level.ge.c1000) THEN
          CALL cfield (depth,imu,jmu,1,ziso,Lr,Mr,1,iugrid)
        END IF
        np=imu*jmu*kmr
        DO iv=1,np
          f5(iv)=f2(iv)
        END DO
        CALL xhslice (f2,depth,zpos,f5,imu,jmu,kmr,vintrp,spval1)
        interpolate=.false.
      END IF
!
!  Read in V-component and store in f3.
!
      IF (levless) THEN
        CALL get_nc2dat (f3,NV,imv,jmv,ncgid,ncfid,tindex,fldnam2,0,    &
     &                   ivgrid,nc_vid,vspv)
      ELSE
        CALL get_nc3dat (f3,NV,imv,jmv,kmr,ncgid,ncfid,tindex,fldnam2,  &
     &                   ivgrid,nc_vid,vspv)
      END IF
      IF (vcode(idfld).eq.'IDCVHD') THEN
        fldnam='vbar'
        CALL get_nc2dat (f4,NV,imv,jmv,ncgid,ncfid,tindex,fldnam,0,     &
     &                   ivgrid,nc_vid,vspv)
        DO k=1,kmr
          DO j=1,jmv
            DO i=1,imv
              ih=i+(j-1)*imv
              iv=ih+(k-1)*imv*jmv
              f3(iv)=f3(iv)-f4(ih)
            END DO
          END DO
        END DO
      END IF
!
!  If appropriate, interpolate V-component to requested depth or depths
!  of isopycnal.
!
      IF (.not.levless.and.(level.le.c0).or.(level.ge.c1000)) THEN
        irgrid=1
        CALL get_nc3pos(zpos,NV,imv,jmv,kmr,ncgid,ncfid,tindex,ivgrid,  &
     &                  irgrid,gotzeta,file)
        IF (level.ge.c1000) THEN
          CALL cfield (depth,imv,jmv,1,ziso,Lr,Mr,1,ivgrid)
        END IF
        np=imv*jmv*kmr
        DO iv=1,np
          f5(iv)=f3(iv)
        END DO
        CALL xhslice (f3,depth,zpos,f5,imv,jmv,kmr,vintrp,spval1)
        interpolate=.false.
      END IF
!
!  Read in Coordinate transformation metrics.
!
      fldnam='pm'
      CALL get_nc2dat (x,NV,imr,jmr,ncgid,ncgid,0,fldnam,0,irgrid,      &
     &                 nc_pid,spv)
      fldnam='pn'
      CALL get_nc2dat (y,NV,imr,jmr,ncgid,ncgid,0,fldnam,0,irgrid,      &
     &                 nc_pid,spv)
!
!  If barotropic fields, compute total depth.
!
      IF ((vcode(idfld).eq.'IDBVHD').or.                                &
     &    (vcode(idfld).eq.'IDSWHD')) THEN
        IF (gotzeta) THEN
          ihgrid=1
          zposnam='zeta'
          CALL get_nc2dat (f4,NV,imr,jmr,ncgid,ncfid,tindx,zposnam,0,   &
     &                     ihgrid,nczid,zspv)
        ELSE
          np=imr*jmr
          DO ih=1,np
            f4(ih)=c0
          END DO
        END IF
        ihgrid=1
        hposnam='h'
        CALL get_nc2dat (bath,NH,imr,jmr,ncgid,ncgid,tindx,hposnam,0,   &
     &                   ihgrid,nchid,hspv)
        np=imr*jmr
        DO ih=1,np
          f4(ih)=f4(ih)+bath(ih)
        END DO
      ELSE
!
!  If baroclinic fields, compute vertical thicknesses, Hz.
!
        IF ((c0.le.level).and.(level.lt.c1000)) THEN
          ihgrid=1
          iwgrid=6
          CALL get_nc3pos (zpos,NV,imr,jmr,Nw,ncgid,ncfid,tindex,ihgrid,&
     &                     iwgrid,gotzeta,file)
          ods=c1/FLOAT(Nr)
          DO k=1,Nr
            DO j=1,jmr
              DO i=1,imr
                ih=i+(j-1)*imr
                iv=ih+(k-1)*imr*jmr
                ivp=ih+(k)*imr*jmr
                f4(iv)=(zpos(ivp)-zpos(iv))*ods
              END DO
            END DO
          END DO
!
!  If interpolating to requested depths, set thickness to unity (1 m).
!
        ELSE
          np=imr*jmr
          DO ih=1,np
            f4(ih)=c1
          END DO
        END IF
      END IF
!
!-----------------------------------------------------------------------
!  Compute horizontal divergence (m/s) in flux form at RHO-points.
!-----------------------------------------------------------------------
!
      im=imr-2
      jm=jmr-2
      IF (levless.or.(level.le.c0).or.(level.ge.c1000)) THEN
        km=1
      ELSE
        km=kmr
      END IF
      CALL hordiv (hdiv,imr-1,jmr-1,km,f2,f3,f4,x,y,spval1)
!
!  Get horizontal positions at interior RHO-points.
!
      igrid=7
      CALL get_nc2pos (xpos,ypos,NV,im,jm,ncgid,igrid)
!
!  Get vertical positions at horizontal RHO-points and vertical
!  RHO-points.
!
      IF (.not.levless.and.(level.le.c0).or.(level.ge.1000)) THEN
        irgrid=1
        CALL get_nc3pos (zpos,NV,im,jm,km,ncgid,ncfid,tindex,igrid,     &
     &                   irgrid,gotzeta,file)
      END IF
!
!  If appropriate, extract horizontal divergence at the requested level.
!
      IF (.not.levless) THEN
        IF ((1.le.INT(level)).and.(INT(level).le.km)) THEN
          np=im*jm*km
          DO iv=1,np
            f4(iv)=hdiv(iv)
          END DO
          ip=0
          DO j=1,jm
            DO i=1,im
              ip=ip+1
              ir=i+(j-1)*im+(INT(level)-1)*im*jm
              hdiv(ip)=f4(ir)
            END DO
          END DO
        END IF
      END IF

      RETURN
      END
