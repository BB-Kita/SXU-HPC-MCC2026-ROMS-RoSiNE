      SUBROUTINE get_jebar (ujbar,vjbar,xpos,ypos,zpos,im,jm,igrid,     &
     &                      ncgid,ncfid,idfld,tindex,gotzeta,gotsalt,   &
     &                      gotrho,gotangle,file)
!
!svn $Id: get_jebar.F 833 2017-01-24 22:20:16Z arango $
!================================================== Hernan G. Arango ===
!  Copyright (c) 2002-2017 The ROMS/TOMS Group                         !
!    Licensed under a MIT/X style license                              !
!    See License_ROMS.txt                                              !
!=======================================================================
!                                                                      !
!  This subroutine computes the Joint Effect of Baroclinicity and      !
!  bottom Relief (JEBAR). It uses either model or raw bathymetry.      !
!                                                                      !
!  On Input:                                                           !
!                                                                      !
!     ncgid    NetCDF ID for input grid (integer).                     !
!     ncfid    NetCDF ID for input fields (integer).                   !
!     idfld    Identification of field to read (integer).              !
!     tindex   Time dimension index to read (integer).                 !
!     gotzeta  Logical switch indicating availability of free-         !
!              surface.                                                !
!     gotsalt  Logical switch indicating availability of salinity.     !
!     gotrho   Logical switch indicating availability of density.      !
!     gotangle Logical switch indicating availability of grid          !
!              rotation angle.                                         !
!     file     NetCDF file to read (primary or secondary; integer).    !
!                                                                      !
!  On Output:                                                          !
!                                                                      !
!     ujbar    JEBAR component in the XI-direction (real array).       !
!     vjbar    JEBAR component in the ETA-direction (real array).      !
!     im       Size of inner dimension (integer).                      !
!     jm       Size of outer dimension (integer).                      !
!     igrid    grid type (integer).                                    !
!     xpos     X-positions of the requested field (real array).        !
!     ypos     Y-positions of the requested field (real array).        !
!     ypos     Z-positions of the requested field (real array).        !
!                                                                      !
!  Calls:    cvector, get_nc3dat, get_nc2pos, get_nc3pos, jbar,        !
!            idens                                                     !
!                                                                      !
!=======================================================================
!
!-----------------------------------------------------------------------
!  Define global variables.
!-----------------------------------------------------------------------
!
      implicit none
#include "param.h"
#include "domdat.h"
#include "fields.h"
#include "fldsid.h"
#include "ncinfo.h"
#include "pltfld.h"
#include "pltlab.h"
#include "pltncar.h"
#include "secbath.h"
!
!-----------------------------------------------------------------------
!  Define local variables.
!-----------------------------------------------------------------------
!
      logical gotangle, gotrho, gotsalt, gotzeta
      integer file, i, idfld, igrid, im, imr, ip, irgrid, iwgrid, j,    &
     &        jm, jmr, k, kmr, kpr, kr, ncfid, ncgid, nc_hid, nc_pid,   &
     &        nc_rid, nc_sid, nc_tid, tindex
      real g, gorho0, hspv, rspv, spv, sspv, tspv
      real idens
      real ujbar(NV), vjbar(NV), xpos(NV), ypos(NV), zpos(NV)
      character*20 fldnam
      parameter (g=9.808)
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
!  Get density anomaly (kg/m^3).
!
      IF (gotrho) THEN
        CALL get_nc3dat (f2,NV,imr,jmr,kmr,ncgid,ncfid,tindex,fldnam1,  &
     &                   irgrid,nc_rid,rspv)
      ELSE IF (gotsalt) THEN
        fldnam='temp'
        CALL get_nc3dat (f3,NV,imr,jmr,kmr,ncgid,ncfid,tindex,fldnam,   &
     &                   irgrid,nc_tid,tspv)
        fldnam='salt'
        CALL get_nc3dat (f4,NV,imr,jmr,kmr,ncgid,ncfid,tindex,fldnam,   &
     &                   irgrid,nc_sid,sspv)
        irgrid=1
        CALL get_nc3pos (zpos,NV,imr,jmr,Nr,ncgid,ncfid,tindex,irgrid,  &
     &                   irgrid,gotzeta,file)
        ip=0
        DO k=1,kmr
          DO j=1,jmr
            DO i=1,imr
              ip=ip+1
              f2(ip)=idens(f3(ip),f4(ip),zpos(ip))
            END DO
          END DO
        END DO
      ELSE
        PRINT 10
  10    FORMAT (/,' GET_JEBAR - unable to get/compute density anomaly.')
        RETURN
      END IF
!
!  Get depths (m) at RHO- and W-points.
!
      IF (gotrho) THEN
        irgrid=1
        CALL get_nc3pos (zpos,NV,imr,jmr,Nr,ncgid,ncfid,tindex,irgrid,  &
     &                   irgrid,gotzeta,file)
      END IF
      iwgrid=6
      CALL get_nc3pos (f3,NV,imr,jmr,Nw,ncgid,ncfid,tindex,irgrid,      &
     &                 iwgrid,gotzeta,file)
!
!  Read in bathymetry (m).
!
      CALL get_nc2dat (bath,NH,imr,jmr,ncgid,ncgid,tindex,fldnam2,0,    &
     &                 irgrid,nc_hid,hspv)
!
!-----------------------------------------------------------------------
!  Compute dynamic pressure (m/s^2).  Integrate hydrostatic equation
!  vertically.  Surface density is assumed to be that of level "Nr".
!-----------------------------------------------------------------------
!
      gorho0=g/rho0
      DO j=1,jmr
        DO i=1,imr
          ip=i+(j-1)*imr
          kr=ip+(kmr-1)*imr*jmr
          kpr=ip+kmr*imr*jmr
          f4(kr)=gorho0*(f3(kpr)-zpos(kr))*f2(kr)
        END DO
      END DO
!
      DO k=kmr-1,1,-1
        DO j=1,jmr
          DO i=1,imr
            ip=i+(j-1)*imr
            kr=ip+(k-1)*imr*jmr
            kpr=ip+k*imr*jmr
            f4(kr)=f4(kpr)+g*((zpos(kpr)-f3(kr))*f2(kpr)+               &
     &                        (f3(kr)-zpos(kr))*f2(kr))
          END DO
        END DO
      END DO
!
!-----------------------------------------------------------------------
!  Compute JEBAR (m/s^2) at PSI-points.
!-----------------------------------------------------------------------
!
!  Notice that work array "f4" is re-shaped in the subroutine call to
!  a 2D array and only the bottom dynamic pressure is used.
!
      CALL jebar (f5,f6,imr-1,jmr-1,f4,bath,x,y)
!
!  Interpolate vectors to requested grid.
!
      IF ((vcode(idfld).eq.'IDRHJV').or.                                &
     &    (vcode(idfld).eq.'IDRHJM').or.                                &
     &    (vcode(idfld).eq.'IDMHJV').or.                                &
     &    (vcode(idfld).eq.'IDMHJM')) THEN
        IF (lvecrho) THEN
          igrid=7
        ELSE
          igrid=2
        END IF
        CALL cvector (ujbar,vjbar,im,jm,1,f5,imr-1,jmr,f6,imr,jmr-1,    &
     &                igrid,ncgid,gotangle)
        IF ((vcode(idfld).eq.'IDRHJM').or.                              &
     &      (vcode(idfld).eq.'IDMHJM')) THEN
          DO ip=1,im*jm
            ujbar(ip)=SQRT(ujbar(ip)*ujbar(ip)+vjbar(ip)*vjbar(ip))
          END DO
          ptype=pscalar
        ELSE IF ((vcode(idfld).eq.'IDRHJS').or.                          &
     &          (vcode(idfld).eq.'IDMHJS')) THEN
          ptype=pstream
        ELSE IF ((vcode(idfld).eq.'IDRHJB').or.                          &
     &          (vcode(idfld).eq.'IDMHJB')) THEN
          ptype=pwbarbs
        ELSE
          ptype=pvector
        END IF
      ELSE IF ((vcode(idfld).eq.'IDRHUJ').or.                            &
     &         (vcode(idfld).eq.'IDMHUJ')) THEN
        im=imr-1
        jm=jmr
        igrid=3
        DO j=1,jm
          DO i=1,im
            ip=i+(j-1)*im
            ujbar(ip)=f5(ip)
          END DO
        END DO
        ptype=pscalar
      ELSE IF ((vcode(idfld).eq.'IDRHVJ').or.                            &
     &         (vcode(idfld).eq.'IDMHVJ')) THEN
        im=imr
        jm=jmr-1
        igrid=4
        DO j=1,jm
          DO i=1,im
            ip=i+(j-1)*im
            ujbar(ip)=f5(ip)
          END DO
        END DO
        ptype=pscalar
      END IF
!
!  Get horizontal positions (Lp,Mp) at RHO-points
!
      CALL get_nc2pos (xpos,ypos,NV,im,jm,ncgid,igrid)

      RETURN
      END
