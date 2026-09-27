      SUBROUTINE get_isosur (Diso,xpos,ypos,im,jm,igrid,ncgid,ncfid,    &
     &                       idfld,tindex,gotzeta,gotsalt,gotrho,file)
!
!svn $Id: get_isosur.F 833 2017-01-24 22:20:16Z arango $
!================================================== Hernan G. Arango ===
!  Copyright (c) 2002-2017 The ROMS/TOMS Group                         !
!    Licensed under a MIT/X style license                              !
!    See License_ROMS.txt                                              !
!=======================================================================
!                                                                      !
!  This subroutine computes depths of requested field isosurface.      !
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
!     file     NetCDF file to read (primary or secondary; integer).    !
!                                                                      !
!  On Output:                                                          !
!                                                                      !
!     Diso     Isosurface depths (real array).                         !
!     im       Size of inner dimension (integer).                      !
!     jm       Size of outer dimension (integer).                      !
!     igrid    Staggered C-grid type for requested field (integer).    !
!     xpos     X-positions of the requested field (real array).        !
!     ypos     Y-positions of the requested field (real array).        !
!                                                                      !
!  Calls:    depth_iso, get_nc3dat, get_nc2pos, idens, pdens           !
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
#include "pltfld.h"
#include "pltncar.h"
!
!-----------------------------------------------------------------------
!  Define local variables.
!-----------------------------------------------------------------------
!
      logical gotrho, gotsalt, gotzeta
      integer file, idfld, igrid, im, ip, jm, km, ncfid, ncgid, nc_rid, &
     &        nc_sid, nc_tid, pref, tindex
      real rspv, sspv, tspv
      real Diso(NV), xpos(NV), ypos(NV)
      real idens, pdens
!
!=======================================================================
!  Begin executable code.
!=======================================================================
!
      isosurf=.true.
      levless=is2d(idfld)
      fldnam1=vname1(idfld)
      fldnam2=vname2(idfld)
!
!  Get depths at RHO-points.
!
      igrid=1
      CALL get_nc3pos (z,NV,Lr,Mr,Nr,ncgid,ncfid,tindex,igrid,igrid,    &
     &                 gotzeta,file)
!
!  Get field to process.
!
      IF ((vcode(idfld).eq.'IDISOT').or.                                &
     &    (vcode(idfld).eq.'IDISOH')) THEN
        CALL get_nc3dat (f2,NV,im,jm,km,ncgid,ncfid,tindex,fldnam1,     &
     &                   igrid,nc_tid,rspv)
      ELSE IF (vcode(idfld).eq.'IDISOR') THEN
        IF (gotrho) THEN
          CALL get_nc3dat (f2,NV,im,jm,km,ncgid,ncfid,tindex,fldnam1,   &
     &                     igrid,nc_rid,rspv)
        ELSE
          fldnam1='temp'
          fldnam2='salt'
          CALL get_nc3dat (f3,NV,im,jm,km,ncgid,ncfid,tindex,fldnam1,   &
     &                     igrid,nc_tid,tspv)
          CALL get_nc3dat (f4,NV,im,jm,km,ncgid,ncfid,tindex,fldnam2,   &
     &                     igrid,nc_sid,sspv)
          DO ip=1,im*jm*km
            f2(ip)=idens(f3(ip),f4(ip),z(ip))
          END DO
        END IF
      ELSE IF ((vcode(idfld).eq.'IDISOD').or.                           &
     &         (vcode(idfld).eq.'IDISOS')) THEN
        IF (vcode(idfld).eq.'IDISOD') THEN
          pref=c0
        ELSE IF (vcode(idfld).eq.'IDISOS') THEN
          pref=-abs(zref)
        END IF
        CALL get_nc3dat (f3,NV,im,jm,km,ncgid,ncfid,tindex,fldnam1,     &
     &                   igrid,nc_tid,tspv)
        CALL get_nc3dat (f4,NV,im,jm,km,ncgid,ncfid,tindex,fldnam2,     &
     &                   igrid,nc_sid,sspv)
        DO ip=1,im*jm*km
          f2(ip)=pdens(f3(ip),f4(ip),pref)
        END DO
      END IF
!
!  Compute the depths of requested field isosurface via interpolation.
!
      CALL depth_iso (Diso,isoval,f2,z,im,jm,km,spval1)
!
!  Get horizontal positions (Lp,Mp) at RHO-points
!
      igrid=1
      CALL get_nc2pos (xpos,ypos,NV,im,jm,ncgid,igrid)

      RETURN
      END
