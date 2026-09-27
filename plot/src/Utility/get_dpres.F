      SUBROUTINE get_dpres (dpres,xpos,ypos,zpos,im,jm,km,igrid,ncgid,  &
     &                      ncfid,idfld,tindex,level,gotzeta,gotsalt,   &
     &                      gotrho,file)
!
!svn $Id: get_dpres.F 833 2017-01-24 22:20:16Z arango $
!================================================== Hernan G. Arango ===
!  Copyright (c) 2002-2017 The ROMS/TOMS Group                         !
!    Licensed under a MIT/X style license                              !
!    See License_ROMS.txt                                              !
!=======================================================================
!                                                                      !
!  This subroutine computes hydrostatic dynamic pressure.              !
!                                                                      !
!  On Input:                                                           !
!                                                                      !
!     ncgid    NetCDF ID for input grid (integer).                     !
!     ncfid    NetCDF ID for input fields (integer).                   !
!     idfld    Identification of field to read (integer).              !
!     tindex   Time dimension index to read (integer).                 !
!     level    Depth (>0), level (>0), or isopycnal (>1000) to         !
!              process (real). If LEVEL=0, the full 3D is passed.      !
!     gotzeta  Logical switch indicating availability of free-         !
!              surface.                                                !
!     gotsalt  Logical switch indicating availability of salinity.     !
!     gotrho   Logical switch indicating availability of density.      !
!     file     NetCDF file to read (primary or secondary; integer)     !
!                                                                      !
!  On Output:                                                          !
!                                                                      !
!     dpres    Requested 3D dynamic pressure  (real array).            !
!     im       Size of inner dimension (integer).                      !
!     jm       Size of outer dimension (integer).                      !
!     km       Size of vertical dimension (integer).                   !
!     igrid    Staggered C-grid type for requested field (integer).    !
!     xpos     X-positions of the requested field (real array).        !
!     ypos     Y-positions of the requested field (real array).        !
!     ypos     Z-positions of the requested field (real array).        !
!                                                                      !
!  Calls:    get_nc3dat, get_nc2pos, get_nc3pos, idens                 !
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
!
!-----------------------------------------------------------------------
!  Define local variables.
!-----------------------------------------------------------------------
!
      logical gotrho, gotsalt, gotzeta
      integer file, i, idfld, igrid, im, imr, ip, irgrid, iwgrid, j, jm,&
     &        jmr, k, km, kmr, kpr, kr, ncfid, ncgid, nc_rid, nc_sid,   &
     &        nc_tid, tindex
      real g, level, rspv, sspv, tspv
      real idens
      real dpres(NV), xpos(NV), ypos(NV), zpos(NV)
      character*20 fldnam
      parameter (g=9.808)
!
!=======================================================================
!  Begin executable code.
!=======================================================================
!
      levless=is2d(idfld)
      fldnam1=vname1(idfld)
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
  10    FORMAT (/,' GET_DPRES - unable to get/compute density anomaly.')
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
!-----------------------------------------------------------------------
!  Compute dynamic pressure (N).  Integrate hydrostatic equation
!  vertically.  Surface density is assumed to be that of level "Nr".
!-----------------------------------------------------------------------
!
      DO j=1,jmr
        DO i=1,imr
          ip=i+(j-1)*imr
          kr=ip+(kmr-1)*imr*jmr
          kpr=ip+kmr*imr*jmr
          dpres(kr)=g*(f3(kpr)-zpos(kr))*f2(kr)
        END DO
      END DO
!
      DO k=kmr-1,1,-1
        DO j=1,jmr
          DO i=1,imr
            ip=i+(j-1)*imr
            kr=ip+(k-1)*imr*jmr
            kpr=ip+k*imr*jmr
            dpres(kr)=dpres(kpr)+g*((zpos(kpr)-f3(kr))*f2(kpr)+         &
     &                              (f3(kr)-zpos(kr))*f2(kr))
          END DO
        END DO
      END DO
!
!  Get horizontal positions (Lp,Mp) at RHO-points
!
      im=imr
      jm=jmr
      km=kmr
      igrid=1
      CALL get_nc2pos (xpos,ypos,NV,imr,jmr,ncgid,igrid)
!
!  If appropriate, extract dynamic pressure at the requested level.
!
      IF ((1.le.int(level)).and.(int(level).le.km)) THEN
        DO ip=1,im*jm*km
          f4(ip)=dpres(ip)
        END DO
        ip=0
        DO j=1,jm
          DO i=1,im
            ip=ip+1
            kr=i+(j-1)*im+(int(level)-1)*im*jm
            dpres(ip)=f4(kr)
          END DO
        END DO
      END IF

      RETURN
      END
