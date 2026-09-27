      SUBROUTINE get_entropy (psi,u,v,xpos,ypos,im,jm,igrid,ncgid,      &
     &                        idfld,gotangle)
!
!svn $Id: get_entropy.F 833 2017-01-24 22:20:16Z arango $
!================================================== Hernan G. Arango ===
!  Copyright (c) 2002-2017 The ROMS/TOMS Group                         !
!    Licensed under a MIT/X style license                              !
!    See License_ROMS.txt                                              !
!=======================================================================
!                                                                      !
!  This subroutine computes the maximum entropy equilibrium fields     !
!  reported by Eby and Holloway (1994).                                !
!                                                                      !
!  On Input:                                                           !
!                                                                      !
!     ncgid    NetCDF ID for input grid (integer).                     !
!     idfld    Identification of field to read (integer).              !
!                                                                      !
!  On Output:                                                          !
!                                                                      !
!     psi      Maximum entropy equilibrium transport (real array).     !
!     u        Maximum entropy equilibrium U-component (real array).   !
!     v        Maximum entropy equilibrium V-component (real array).   !
!     xpos     X-positions of the requested field (real array).        !
!     ypos     Y-positions of the requested field (real array).        !
!     im       Size of inner dimension (integer).                      !
!     jm       Size of outer dimension (integer).                      !
!     igrid    Grid type (integer).                                    !
!     gotangle Logical switch indicating availability of grid          !
!              rotation angle.                                         !
!                                                                      !
!  Calls:    cfield, cvector, get_nc2dat, get_nc2pos, velocity         !
!                                                                      !
!  Reference:                                                          !
!                                                                      !
!    Eby, M. and G. Holloway, 1994: sensitivity of a large-scale       !
!            ocean model parameterization of topographic stress.       !
!            J. Phys. Oceanog., 24, 2577-2588.                         !
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
#include "fields.h"
#include "fldsid.h"
#include "ncinfo.h"
#include "pltfld.h"
#include "pltncar.h"
#include "secbath.h"
!
!-----------------------------------------------------------------------
!  Define local variables.
!-----------------------------------------------------------------------
!
      logical gotangle
      integer i, idfld, igrid, im, imr, ip, irgrid, j, jm, jmr, ncgid,  &
     &        nc_fid, nc_hid, nc_lid, nc_pid
      real Lscle, spv
      real psi(NV), u(NV), v(NV), xpos(NV), ypos(NV)
      character*20 fldnam
!
!=======================================================================
!  Begin executable code.
!=======================================================================
!
      levless=is2d(idfld)
      fldnam1=vname1(idfld)
      fldnam2=vname2(idfld)
!
!  Read in Coriolis parameter.
!
      CALL get_nc2dat (f2,NH,imr,jmr,ncgid,ncgid,0,fldnam1,0,irgrid,    &
     &                 nc_fid,spv)
!
!  Read in bathymetry.
!
      CALL get_nc2dat (bath,NH,imr,jmr,ncgid,ncgid,0,fldnam2,0,irgrid,  &
     &                 nc_hid,spv)
!
!  Read in latitude.
!
      fldnam='lat_rho'
      CALL get_nc2dat (f3,NH,imr,jmr,ncgid,ncgid,0,fldnam,0,irgrid,     &
     &                 nc_lid,spv)
!
!-----------------------------------------------------------------------
!  Compute maximum entropy equilibrium transport (m3/s) at PSI-points.
!-----------------------------------------------------------------------
!
      DO j=1,jmr
        DO i=1,imr
          ip=i+(j-1)*imr
          Lscle=7500.0+4500.0*COS(c2*deg2rad*f3(ip))
          f4(ip)=-f2(ip)*bath(ip)*Lscle*Lscle
        END DO
      END DO
      igrid=2
      im=imr-1
      jm=jmr-1
      CALL cfield (psi,im,jm,1,f4,imr,jmr,1,igrid)
      ptype=pscalar
!
!-----------------------------------------------------------------------
!  Compute maximum entropy equilibrium velocities (m/s).
!-----------------------------------------------------------------------
!
      IF ((vcode(idfld).eq.'IDUNPY').or.(vcode(idfld).eq.'IDVNPY').or.  &
     &    (vcode(idfld).eq.'IDNPYV').or.(vcode(idfld).eq.'IDNPYM')) THEN
!
!  Read in Coordinate transformation metrics.
!
        fldnam='pm'
        CALL get_nc2dat (x,NV,imr,jmr,ncgid,ncgid,0,fldnam,0,irgrid,    &
     &                   nc_pid,spv)
        fldnam='pn'
        CALL get_nc2dat (y,NV,imr,jmr,ncgid,ncgid,0,fldnam,0,irgrid,    &
     &                   nc_pid,spv)
!
!  Compute vector components from streamfunction.
!
        CALL velocity (f3,f4,imr-1,jmr-1,psi,bath,x,y)
!
!  Interpolate to requested grid.
!
        IF ((vcode(idfld).eq.'IDNPYV').or.                              &
     &      (vcode(idfld).eq.'IDNPYM').or.                              &
     &      (vcode(idfld).eq.'IDNPYS')) THEN
          IF (lvecrho) THEN
            igrid=7
          ELSE
            igrid=2
          END IF
          CALL cvector (u,v,im,jm,1,f3,imr-1,jmr,f4,imr,jmr-1,igrid,    &
     &                  ncgid,gotangle)
          IF (vcode(idfld).eq.'IDNPYM') THEN
            DO ip=1,im*jm
              u(ip)=SQRT(u(ip)*u(ip)+v(ip)*v(ip))
            END DO
            ptype=pscalar
          ELSE IF (vcode(idfld).eq.'IDNPYS') THEN
            ptype=pstream
          ELSE IF (vcode(idfld).eq.'IDNPYB') THEN
            ptype=pwbarbs
          ELSE
            ptype=pvector
          END IF
        ELSE IF (vcode(idfld).eq.'IDUNPY') THEN
          im=imr-1
          jm=jmr
          igrid=3
          DO j=1,jm
            DO i=1,im
              ip=i+(j-1)*im
              u(ip)=f3(ip)
            END DO
          END DO
        ELSE IF (vcode(idfld).eq.'IDVNPY') THEN
          im=imr
          jm=jmr-1
          igrid=4
          DO j=1,jm
            DO i=1,im
              ip=i+(j-1)*im
              u(ip)=f4(ip)
            END DO
          END DO
        END IF
      END IF
!
!  Get horizontal positions (Lp,Mp) at RHO-points
!
      CALL get_nc2pos (xpos,ypos,NV,im,jm,ncgid,igrid)

      RETURN
      END
