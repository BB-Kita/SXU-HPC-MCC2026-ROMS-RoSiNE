      SUBROUTINE get_wcurl (curl,xpos,ypos,im,jm,igrid,ncgid,ncfid,     &
     &                      idfld,tindex)
!
!svn $Id: get_wcurl.F 833 2017-01-24 22:20:16Z arango $
!================================================== Hernan G. Arango ===
!  Copyright (c) 2002-2017 The ROMS/TOMS Group                         !
!    Licensed under a MIT/X style license                              !
!    See License_ROMS.txt                                              !
!=======================================================================
!                                                                      !
!  This subroutine computes surface wind stress curl or Ekman          !
!  Pumping (curl/f) from input NetCDF file.                            !
!                                                                      !
!  On Input:                                                           !
!                                                                      !
!     ncgid    NetCDF ID for input grid (integer).                     !
!     ncfid    NetCDF ID for input fields (integer).                   !
!     idfld    Identification of field to read (integer).              !
!     tindex   Time dimension index to read (integer).                 !
!                                                                      !
!  On Output:                                                          !
!                                                                      !
!     curl     Surface wind stress curl (m/s2) or Ekman pumping        !
!              (m/s).                                                  !
!     im       Size of inner dimension (integer).                      !
!     jm       Size of outer dimension (integer).                      !
!     igrid    Staggered C-grid type for requested field (integer).    !
!     xpos     X-positions of the requested field (real array).        !
!     ypos     Y-positions of the requested field (real array).        !
!                                                                      !
!  Calls:    fld_type, get_nc2dat, get_nc2pos.                         !
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
#include "pltlab.h"
#include "secbath.h"
!
!-----------------------------------------------------------------------
!  Define local variables.
!-----------------------------------------------------------------------
!
      logical ekman
      integer i, idfld, igrid, im, imr, imu, imv, ip, irgrid, iugrid,   &
     &        ivgrid, j, jm, jmr, jmu, jmv, ncfid, ncgid, nc_pid,       &
     &        nc_uid, nc_vid, tindex
      real beta, spv, uspv, vspv
      real curl(NV), xpos(NV), ypos(NV)
      character*20 fldnam
      parameter (beta=1.9e-11)
!
!=======================================================================
!  Begin executable code.
!=======================================================================
!
      levless=is2d(idfld)
      fldnam1=vname1(idfld)
      fldnam2=vname2(idfld)
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
!  Read in surface u-wind stress and store in f2.
!
      CALL get_nc2dat (f2,NV,imu,jmu,ncgid,ncfid,tindex,fldnam1,0,      &
     &                 iugrid,nc_uid,uspv)
!
!  Read in surface v-wind stress and store in f3.
!
      CALL get_nc2dat (f3,NV,imv,jmv,ncgid,ncfid,tindex,fldnam2,0,      &
     &                 ivgrid,nc_vid,vspv)
!
!  Read in Coriolis Parameter.
!
      fldnam='f'
      CALL get_nc2dat (f4,NV,imr,jmr,ncgid,ncgid,0,fldnam,0,irgrid,     &
     &                 nc_pid,spv)
      ekman=.false.
      IF ((vcode(idfld).eq.'IDSMEK').or.                                &
     &    (vcode(idfld).eq.'IDBMEK').or.                                &
     &    (vcode(idfld).eq.'IDBWEK')) THEN
        ekman=.true.
      END IF
!
!-----------------------------------------------------------------------
!  Compute surface wind stress curl (m/s2) at interior RHO-points.
!-----------------------------------------------------------------------
!
!  Scale surface wind stress curl to (m/s2).  Divide by rho0; assume a
!  value of 1000 kg/m3.
!
      im=imr-2
      jm=jmr-2
      CALL curlz (curl,imr-1,jmr-1,f2,f3,f4,x,y,ekman)
      DO j=1,jm
        DO i=1,im
          ip=i+(j-1)*im
          curl(ip)=curl(ip)*r1000
        END DO
      END DO
!
!  Get horizontal positions at interior RHO-points
!
      igrid=7
      CALL get_nc2pos (x,y,NV,im,jm,ncgid,igrid)

      RETURN
      END
