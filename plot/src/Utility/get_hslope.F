      SUBROUTINE get_hslope (h,xpos,ypos,im,jm,igrid,ncgid,ncfid,idfld, &
     &                       tindex,gotzeta)
!
!svn $Id: get_hslope.F 833 2017-01-24 22:20:16Z arango $
!================================================== Hernan G. Arango ===
!  Copyright (c) 2002-2017 The ROMS/TOMS Group                         !
!    Licensed under a MIT/X style license                              !
!    See License_ROMS.txt                                              !
!=======================================================================
!                                                                      !
!  This subroutine computes either the bathymetry slope or the         !
!  bathymetry r-factor.                                                !
!                                                                      !
!  On Input:                                                           !
!                                                                      !
!     ncgid    NetCDF ID for input grid (integer).                     !
!     ncfid    NetCDF ID for input fields (integer).                   !
!     idfld    Identification of field to read (integer)               !
!     tindex   Time dimension index to read (integer).                 !
!     gotzeta  Logical switch indicating availability of free-         !
!              surface.                                                !
!                                                                      !
!  On Output:                                                          !
!                                                                      !
!     h        bathymetry slope or r-factor (real array).              !
!     im       Size of inner dimension (integer).                      !
!     jm       Size of outer dimension (integer).                      !
!     igrid    Staggered C-grid type for requested field (integer).    !
!     xpos     X-positions of the requested field (real array).        !
!     ypos     Y-positions of the requested field (real array).        !
!                                                                      !
!  Calls:    get_nc2dat, get_nc2pos                                    !
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
!
!-----------------------------------------------------------------------
!  Define local variables.
!-----------------------------------------------------------------------
!
      logical gotzeta
      integer i, idfld, igrid, ij, im, imr, ip, irgrid, j, jm, jmr,     &
     &        ncfid, ncgid, nc_hid, nc_pid, tindex
      real Se, Sx, spv
      real h(NV), xpos(NV), ypos(NV)
      character*20 fldnam
!
!=======================================================================
!  Begin executable code.
!=======================================================================
!
!  Read bathymetry.
!
      fldnam=vname1(idfld)
      CALL get_nc2dat (f2,NV,imr,jmr,ncgid,ncgid,tindex,fldnam,0,       &
     &                 irgrid,nc_hid,spv)
!
!-----------------------------------------------------------------------
!  Compute r-factor at PSI-points.
!-----------------------------------------------------------------------
!
      IF ((vcode(idfld).eq.'IDMHRF').or.                                &
     &    (vcode(idfld).eq.'IDRHRF')) THEN
        DO j=1,jmr
          DO i=1,imr-1
            ij=i+(j-1)*imr
            f3(ij)=ABS(f2(ij+1)-f2(ij))/(f2(ij)+f2(ij+1))
          END DO
        END DO
        DO j=1,jmr-1
          DO i=1,imr
            ij=i+(j-1)*imr
            f4(ij)=ABS(f2(ij+imr)-f2(ij))/(f2(ij)+f2(ij+imr))
          END DO
        END DO
        ip=0
        DO j=1,jmr-1
          DO i=1,imr-1
            ip=ip+1
            ij=i+(j-1)*imr
            Sx=p5*(f3(ij)+f3(ij+imr))
            Se=p5*(f4(ij)+f4(ij+1))
            h(ip)=MAX(Sx,Se)
          END DO
        END DO
      END IF
!
!-----------------------------------------------------------------------
!  Compute bathymetry slope at PSI-points.
!-----------------------------------------------------------------------
!
      IF ((vcode(idfld).eq.'IDMHSL').or.                                &
     &    (vcode(idfld).eq.'IDRHSL')) THEN
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
!  Compute gradients.
!
        DO j=1,jmr
          DO i=1,imr-1
            ij=i+(j-1)*imr
            f3(ij)=p5*(x(ij)+x(ij+1))*(f2(ij+1)-f2(ij))
          END DO
        END DO
        DO j=1,jmr-1
          DO i=1,imr
            ij=i+(j-1)*imr
            f4(ij)=p5*(y(ij)+y(ij+imr))*(f2(ij+imr)-f2(ij))
          END DO
        END DO
        ip=0
        DO j=1,jmr-1
          DO i=1,imr-1
            ip=ip+1
            ij=i+(j-1)*imr
            Sx=p5*(f3(ij)+f3(ij+imr))
            Se=p5*(f4(ij)+f4(ij+1))
            h(ip)=SQRT(Sx*Sx+Se*Se)
          END DO
        END DO
      END IF
!
!-----------------------------------------------------------------------
!  Compute horizontal positions at PSI-points.
!-----------------------------------------------------------------------
!
      im=imr-1
      jm=jmr-1
      igrid=2
      CALL get_nc2pos (xpos,ypos,NV,im,jm,ncgid,igrid)
  
      RETURN
      END
