      SUBROUTINE draw_line (iunit,kind)
!
!svn $Id: draw_line.F 833 2017-01-24 22:20:16Z arango $
!================================================== Hernan G. Arango ===
!  Copyright (c) 2002-2017 The ROMS/TOMS Group                         !
!    Licensed under a MIT/X style license                              !
!    See License_ROMS.txt                                              !
!=======================================================================
!                                                                      !
!  This routine reads and draws map attribute data.                    !
!                                                                      !
!  On Input:                                                           !
!                                                                      !
!     kind    [0] coastlines, islands and lakes                        !
!                 or isobaths (lat,lon)                                !
!             [1] Lagrangian trajectories (lat,lon,z,time)             !
!             [2] hydrographic stations                                !
!                                                                      !
!=======================================================================
!
!-----------------------------------------------------------------------
!  Define global data.
!-----------------------------------------------------------------------
!
      implicit none
#include "pconst.h"
#include "colpal.h"
#include "pltncar.h"
!
!-----------------------------------------------------------------------
!  Define local data.
!-----------------------------------------------------------------------
!
      logical load, pen_up
      integer indx, ista, itime, itype, iunit, julday, kind, npts
      real day, lon, lat, time, x, y, z
      real ir, ig, ib
!
!=======================================================================
!  Begin executable code.
!=======================================================================
!
!  Read and draw coastlines, islands and lakes or isobath.  Check wich
!  features to draw: new coastlines files has only the latitude as a
!  special value when drawing a new coastline segment, the longitude
!  value has the type of segment (itype=ABS(INT(lon))): [1] land,
!  [2]=lake, [3]=island_in_lake, or [4]=pond_in_island_in lake. Notice
!  that in order to activate this, lmsk must negative in input scripts.
!
      IF (kind.eq.0) THEN
        DO WHILE (.true.)
          READ (iunit,*,end=40) lat, lon
          IF (lat.eq.spvgeo) THEN
            IF (lon.eq.spvgeo) THEN
              load=.true.
            ELSE
              itype=ABS(INT(lon))
              load=.false.
              IF ((ABS(lmsk).eq.0).or.(itype.le.ABS(lmsk))) load=.true.
            END IF
            pen_up=.true.
            GO TO 10
          ELSE IF ((pblat.le.lat).and.(lat.le.ptlat)) THEN
            IF (eastlon.and.(lon.lt.c0)) lon=c360+lon
            IF ((pllon.le.lon).and.(lon.le.prlon)) THEN
              IF (load) THEN
                CALL maptrn (lat,lon,x,y)
                IF (pen_up) THEN
                  CALL frstpt (x,y)
                  pen_up=.false.
                ELSE
                  CALL vector (x,y)
                END IF
              END IF
            END IF
          ELSE
            pen_up=.true.
          END IF
  10      CONTINUE
        END DO
!
!-----------------------------------------------------------------------
!  Draw Lagrangian trajectories.
!-----------------------------------------------------------------------
!
      ELSE IF (kind.eq.1) THEN
        DO WHILE (.true.)
          READ (iunit,*,end=40) lat, lon, z, time
          IF ((lon.ge.spvgeo).and.(lat.ge.spvgeo)) THEN
            pen_up=.true.
            GO TO 20
          ELSE IF ((pblat.le.lat).and.(lat.le.ptlat)) THEN
            IF (eastlon.and.(lon.lt.c0)) lon=c360+lon
            IF ((pllon.le.lon).and.(lon.le.prlon)) THEN
              CALL maptrn (lat,lon,x,y)
              IF (pen_up) THEN
                CALL plchhq (x,y,':1256:',c5,c0,c0)
                CALL frstpt (x,y)
                pen_up=.false.
                day=FLOAT(INT(time))+c1
              ELSE
                CALL vector (x,y)
              END IF
              IF (time.gt.day) THEN
                day=day+c1
                CALL plchhq (x,y,':1245:',c5,c0,c0)
              END IF
            END IF
          ELSE
            pen_up=.true.
          END IF
  20      CONTINUE
        END DO
!
!-----------------------------------------------------------------------
!  Draw cruise track stations.
!-----------------------------------------------------------------------
!
      ELSE IF (kind.eq.2) THEN
        DO WHILE (.true.)
          READ (iunit,*,end=40) lat, lon, z, julday, itime, npts, ista
          IF (eastlon.and.(lon.ne.spvgeo).and.(lon.lt.c0)) lon=c360+lon
          IF ((pblat.le.lat).and.(lat.le.ptlat)) THEN
            IF ((pllon.le.lon).and.(lon.le.prlon)) THEN
              CALL maptrn (lat,lon,x,y)
              CALL plchhq (x,y,':1245:',c5,c0,c0)
            END IF
          END IF
        END DO
!
!-----------------------------------------------------------------------
!  Draw user lines with specified colors.
!-----------------------------------------------------------------------
!
      ELSE IF (kind.eq.3) THEN
        indx=ncl+5
        DO WHILE (.true.)
          READ (iunit,*,end=40) lat, lon
          IF (ABS(lat).eq.spvgeo) THEN
            IF (ABS(lon).ne.spvgeo) THEN
              BACKSPACE (iunit)
              READ (iunit,*,end=40) lat, ir, ig, ib
              indx=indx+1
              CALL gscr (1,indx,ir/c255,ig/c255,ib/c255)
              CALL sflush
              CALL gsplci (indx)
            END IF
            pen_up=.true.
            GO TO 30
          ELSE IF ((pblat.le.lat).and.(lat.le.ptlat)) THEN
            IF (eastlon.and.(lon.lt.c0)) lon=c360+lon
            IF ((pllon.le.lon).and.(lon.le.prlon)) THEN
              CALL maptrn (lat,lon,x,y)
              IF (pen_up) THEN
                CALL frstpt (x,y)
                pen_up=.false.
              ELSE
                CALL vector (x,y)
              END IF
            END IF
          ELSE
            pen_up=.true.
          END IF
  30      CONTINUE
        END DO
      END IF
  40  CONTINUE
      REWIND (iunit)

      RETURN
      END
