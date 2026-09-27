      SUBROUTINE landmask (fill)
!
!svn $Id: landmask.F 833 2017-01-24 22:20:16Z arango $
!================================================== Hernan G. Arango ===
!  Copyright (c) 2002-2017 The ROMS/TOMS Group                         !
!    Licensed under a MIT/X style license                              !
!    See License_ROMS.txt                                              !
!=======================================================================
!                                                                      !
!  This routine retrieves, projects, and adds the coastlines and       !
!  islands, if any, to the area map.                                   !
!                                                                      !
!=======================================================================
!
!-----------------------------------------------------------------------
!  Define global data.
!-----------------------------------------------------------------------
!
      implicit none
#include "pconst.h"
#include "cntwkspa.h"
#include "colpal.h"
#include "pltncar.h"
#include "pltio.h"
!
!-----------------------------------------------------------------------
!  Define local data.
!-----------------------------------------------------------------------
!
      logical load, pen_up
      integer fill, ic, idl, idr, igp, itype
      real del, lon, lat
      real x(5), y(5)
!
!=======================================================================
!  Begin executable code.
!=======================================================================
!
!  Fill entire model domain with water.
!
      IF (fill.eq.0) THEN
        CALL maptrn (pblat,pllon,x(1),y(1))
        CALL maptrn (ptlat,pllon,x(2),y(2))
        CALL maptrn (ptlat,prlon,x(3),y(3))
        CALL maptrn (pblat,prlon,x(4),y(4))
        x(5)=x(1)
        y(5)=y(1)
        CALL clrbox (x,y,5,ncl+3,.false.)
!
      ELSE IF (fill.eq.1) THEN
!
!-----------------------------------------------------------------------
!  Check if user provided the land boundary data to mask.  If not,
!  use EZMAP dataset.  Turn off the drawing of coastlines by routine
!  HBACKGROUND.
!-----------------------------------------------------------------------
!
        addezmap=.false.
        IF (cst) THEN
          add=.true.
          cst=.false.
        END IF
!
!-----------------------------------------------------------------------
!  Add frame perimeter to the area map and if ADD is false add EZMAP
!  boundary outlines. (See routine MAPEOD).
!-----------------------------------------------------------------------
!
        CALL mapbla (iam)
!
!-----------------------------------------------------------------------
!  Add User land boundaries to area map.  Each land boundaries
!  segment is assumed to have the land to its right when looking
!  from the position of the first point toward the second point of
!  the segment.  The right edge identifier is assigned to 233 (land)
!  whereas the left edge identifier is assigned to 2 (water).
!-----------------------------------------------------------------------
!
        IF (add) THEN
          igp=1
          ic=0
          idr=223
          idl=2
          del=p5*lgrid
!
!  Read coastlines, islands and lakes and add then to the area map.
!
          DO WHILE (.true.)
            READ (inpcst,*,end=20) lat,lon
            IF (lat.eq.spvgeo) THEN
              IF (lon.eq.spvgeo) THEN
                load=.true.
              ELSE
                itype=abs(int(lon))
                load=.false.
                IF (itype.le.lmsk) load=.true.
              END IF
              pen_up=.true.
              GO TO 10
            ELSE IF ((pblat-del.le.lat).and.(lat.le.ptlat+del)) THEN
              IF (eastlon.and.(lon.lt.c0)) lon=c360+lon
              IF ((pllon-del.le.lon).and.(lon.le.prlon+del)) THEN
                IF (load) THEN
                  IF (pen_up) THEN
                    ic=ic+1
                    IF (ic.gt.1) CALL mapiqa (iam,igp,idl,idr)
                    CALL mapita (lat,lon,0,iam,igp,idl,idr)
                    pen_up=.false.
                  ELSE
                    CALL mapita (lat,lon,1,iam,igp,idl,idr)
                  END IF
                END IF
              END IF
            ELSE
              pen_up=.true.
            END IF
  10        CONTINUE
          END DO
  20      CALL mapiqa (iam,igp,idl,idr)
          REWIND (inpcst)
        END IF
      END IF

      RETURN
      END
