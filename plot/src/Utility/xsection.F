      SUBROUTINE xsection (sec,is,ks,fld,xfld,yfld,zfld,im,jm,km,       &
     &                     v_intrp,spval)
!
!svn $Id: xsection.F 833 2017-01-24 22:20:16Z arango $
!================================================== Hernan G. Arango ===
!  Copyright (c) 2002-2017 The ROMS/TOMS Group                         !
!    Licensed under a MIT/X style license                              !
!    See License_ROMS.txt                                              !
!=======================================================================
!                                                                      !
!  This routine extracts a data section from a 3-D field using         !
!  Bicubic interpolation in the horizontal and linear or cubic         !
!  splines interpolation in the vertical.                              !
!                                                                      !
!  On Input:                                                           !
!                                                                      !
!     xsec    Section X-coordinate (km).                               !
!     ysec    Section Y-coordinate (km).                               !
!     is      Number of section points in the horizontal direction.    !
!     ks      Number of section points in the Z-direction.             !
!     fld     Field from which the section is extracted.               !
!     xfld    X-positions of field "fld" (km).                         !
!     yfld    Y-positions of field "fld" (km).                         !
!     zfld    Depths of field "fld" (meters).                          !
!     im      Number of field points in the X-direction.               !
!     jm      Number of field points in the Y-direction.               !
!     km      Number of field points in the Z-direction.               !
!     v_intrp  Vertical interpolation switch (integer):                !
!               [0] linear, [1] cubic spline.                          !
!     spval   Special value to flag missing data.                      !
!                                                                      !
!  On Output:                                                          !
!                                                                      !
!     sec     extracted section (real).                                !
!                                                                      !
!  Calls:                                                              !
!                                                                      !
!     bcintrp, geodesic_dist, hindices, lintrp, spline, splint,        !
!     vindices                                                         !
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
#include "pltncar.h"
#include "pltfld.h"
#include "fields.h"
#include "secbath.h"
#include "secpos.h"
!
!-----------------------------------------------------------------------
!  Define local data.
!-----------------------------------------------------------------------
!
      integer nz
      parameter (nz=1000)
!
      logical overwrite
      integer i, im, is, ip, jm, k, kend, km, ks, kpts, kstr, status,   &
     &        v_intrp
      integer isec(NH), jsec(NH)
      integer vindices
      real bearing, der1, derlz, delz, dist, fder, scle, spval, spv,    &
     &      val, xdist, ydist, zbot, ztop
      real fk(nz), fld(im,jm,km), sec(is,ks), wk(nz), xfld(im,jm),      &
     &     yfld(im,jm), zfld(im,jm,km), zk(nz)
      real bcintrp
      parameter (overwrite=.false.)
      parameter (der1=c1ep30, derlz=c1ep30, spv=spval0)
!
!=======================================================================
!  Begin executable code.
!=======================================================================
!
!  Determine vertical coordinate units.
!
      scle=c1                                  ! meters
      IF (coorscl.eq.2) THEN
        scle=m2cm                              ! meters to centimeters
      ELSE IF (coorscl.eq.3) THEN
        scle=m2mm                              ! meters to milimeters
      END IF
!
!  Set the (i,j) indices of the 3D grid cell cointaining the (x,y) to
!  horizontally interpolate.
!
      CALL hindices (isec,jsec,xsec,ysec,ims,xfld,yfld,im,jm)
!
!-----------------------------------------------------------------------
!  Extract section bath curve.
!-----------------------------------------------------------------------
!
      DO i=1,ims
        sbth(i)=bcintrp(xsec(i),ysec(i),isec(i),jsec(i),bath,xfld,      &
     &                  yfld,im,jm,spval,spval)
      END DO
!
!-----------------------------------------------------------------------
!  Extract requested section.
!-----------------------------------------------------------------------
!
      DO i=1,ims
!
!  Perform horizontal interpolation.  Set depths to be positive so the
!  first coordinate of the data entering the splines routines is in
!  ascending.
!
        DO k=1,km
          fk(k)=bcintrp(xsec(i),ysec(i),isec(i),jsec(i),fld(1,1,k),     &
     &                  xfld,yfld,im,jm,spval,spval)
          zk(k)=bcintrp(xsec(i),ysec(i),isec(i),jsec(i),zfld(1,1,k),    &
     &                  xfld,yfld,im,jm,spval,spval)
        END DO
!
!  Check viable vertical indices in extracted profile.
!
        status=vindices(fk,km,spv,kstr,kend)
        kpts=kend-kstr+1
        IF (-ABS(sztop*scle).gt.zk(kend)) THEN
          ztop=zk(kend)
        ELSE
          ztop=-ABS(sztop*scle)
        END IF
        IF (zk(kstr).gt.(-ABS(szbot*scle))) THEN
          zbot=zk(kstr)
        ELSE
          zbot=-ABS(szbot*scle)
        END IF
!
!  Set depths to be used in contour location routine CPMPXY.
!
        delz=(ztop-zbot)/FLOAT(kms-1)
        DO k=1,kms
          ip=i+(k-1)*ims
          f5(ip)=zbot+FLOAT(k-1)*delz
        END DO
        ip=i+(kms-1)*ims
        f5(ip)=ztop
!
!  Perform vertical interpolation.  First, check viable indices in
!  extracted vertical profile.
!
        IF (status.eq.0) THEN
          IF (v_intrp.eq.0) THEN
            DO k=1,kms
              ip=i+(k-1)*ims
              IF ((ztop.ge.f5(ip)).and.(f5(ip).ge.zbot)) THEN
                CALL lintrp (kpts,zk(kstr),fk(kstr),1,f5(ip),val)
                sec(i,k)=val
              ELSE
                sec(i,k)=spval
              END IF
            END DO
          ELSE IF (v_intrp.eq.1) THEN
            CALL spline (zk(kstr),fk(kstr),kpts,der1,derlz,wk)
            DO k=1,kms
              ip=i+(k-1)*ims
              IF ((ztop.ge.f5(ip)).and.(f5(ip).ge.zbot)) THEN
                CALL splint (zk(kstr),fk(kstr),wk,kpts,f5(ip),val,fder)
                sec(i,k)=val
              ELSE
                sec(i,k)=spval
              END IF
            END DO
          END IF
        ELSE
          DO k=1,kms
            sec(i,k)=spval
          END DO
        END IF
      END DO
!
!-----------------------------------------------------------------------
!  Set section positions as used by contour location routine CPMPXY.
!-----------------------------------------------------------------------
!
!  Cartesian axis (application grid or metric units).
!
      lataxis=.false.
      IF ((isecpos.eq.0).or.(isecpos.eq.1)) THEN
        DO i=1,ims
          dist=SQRT((xsec(i)-xsec(1))**2+(ysec(i)-ysec(1))**2)
          DO k=1,kms
            ip=i+(k-1)*ims
            x(ip)=dist
          END DO
        END DO
!
!  Spherical axis (metric or degrees units).
!
      ELSE IF (isecpos.eq.2) THEN
        IF (secaxis.eq.0) THEN
          DO i=1,ims
            CALL geodesic_dist (xsec(1),ysec(1),xsec(i),ysec(i),4,dist, &
     &                          bearing)
            DO k=1,kms
              ip=i+(k-1)*ims
              x(ip)=dist
            END DO
          END DO
        ELSE
          IF (xsec(1).eq.xsec(ims)) THEN
            DO i=1,ims
              DO k=1,kms
                ip=i+(k-1)*ims
                x(ip)=ysec(i)
              END DO
            END DO
            lataxis=.true.
          ELSE IF (ysec(1).eq.ysec(ims)) THEN
            DO i=1,ims
              DO k=1,kms
                ip=i+(k-1)*ims
                x(ip)=xsec(i)
              END DO
            END DO
          ELSE
            CALL geodesic_dist (xsec(1),ysec(1),xsec(ims),ysec(i),4,    &
     &                          xdist,bearing)
            CALL geodesic_dist (xsec(1),ysec(1),xsec(1),ysec(ims),4,    &
     &                          ydist,bearing)
            IF (xdist.gt.ydist) THEN
              DO i=1,ims
                DO k=1,kms
                  ip=i+(k-1)*ims
                  x(ip)=xsec(i)
                END DO
              END DO
            ELSE
              DO i=1,ims
                DO k=1,kms
                  ip=i+(k-1)*ims
                  x(ip)=ysec(i)
                END DO
              END DO
              lataxis=.true.
            END IF
          END IF
        END IF
      END IF
      DO ip=1,ims*kms
        y(ip)=f5(ip)
      END DO
!
!-----------------------------------------------------------------------
!  The code below is for plotting a section of observations (ASCII).
!  This is done to facilitate the comparison between observations
!  and NetCDF data plots.  Notice that the switch "overwrite" is set
!  to false at the beggining of this routine.
!-----------------------------------------------------------------------
!
      IF (overwrite) THEN
        spherical=.false.
        READ (50,*) ims, kms
        is=ims
        ks=kms
        DO i=1,ims
          DO k=1,kms
            ip=i+(k-1)*ims
            READ (50,*) x(ip), y(ip), f2(ip)
          END DO
        END DO
      END IF

      RETURN
      END
