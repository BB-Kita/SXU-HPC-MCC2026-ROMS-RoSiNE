      SUBROUTINE dombox
!
!svn $Id: dombox.F 833 2017-01-24 22:20:16Z arango $
!================================================== Hernan G. Arango ===
!  Copyright (c) 2002-2017 The ROMS/TOMS Group                         !
!    Licensed under a MIT/X style license                              !
!    See License_ROMS.txt                                              !
!=======================================================================
!                                                                      !
!  This routine draws model domain box.  Domain coordinates are        !
!  stored at the beginning of storage array FIELDS.                    !
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
#include "fields.h"
#include "fmask.h"
#include "ndimen.h"
#include "pltfld.h"
#include "pltncar.h"
!
!-----------------------------------------------------------------------
!  Define local data.
!-----------------------------------------------------------------------
!
      logical pen_up
      integer i, ip, j
      real x_map, y_map
      real xmap(NX), ymap(NX)
!
!=======================================================================
!  Begin executable code.
!=======================================================================
!
!  Set viewport.
!
      CALL set (fl,fr,fb,ft,ul,ur,ub,ut,1)
!
!-----------------------------------------------------------------------
!  Draw domain grid.
!-----------------------------------------------------------------------
!
      IF (iref.eq.-1) THEN
        IF (spherical) THEN
          DO j=1,jm
            pen_up=.true.
            DO i=1,im
              ip=i+(j-1)*im
              IF (mskinp1) THEN
                IF (mask(ip).gt.c0) THEN
                  CALL maptrn (y(ip),x(ip),x_map,y_map)
                  IF (pen_up) THEN
                    CALL frstpt (x_map,y_map)
                    pen_up=.false.
                  ELSE
                    CALL vector (x_map,y_map)
                  END IF
                ELSE
                  pen_up=.true.
                END IF
              ELSE
                CALL maptrn (y(ip),x(ip),xmap(i),ymap(i))
              END IF
            END DO
            IF (.not.mskinp1) CALL curve (xmap,ymap,im)
          END DO
          DO i=1,im
            pen_up=.true.
            DO j=1,jm
              ip=i+(j-1)*im
              IF (mskinp1) THEN
                IF (mask(ip).gt.c0) THEN
                  CALL maptrn (y(ip),x(ip),x_map,y_map)
                  IF (pen_up) THEN
                    CALL frstpt (x_map,y_map)
                    pen_up=.false.
                  ELSE
                    CALL vector (x_map,y_map)
                  END IF
                ELSE
                  pen_up=.true.
                END IF
              ELSE
                CALL maptrn (y(ip),x(ip),xmap(j),ymap(j))
              END IF
            END DO
            IF (.not.mskinp1) CALL curve (xmap,ymap,jm)
          END DO
        ELSE
          DO j=1,jm
            DO i=1,im
              ip=i+(j-1)*im
              xmap(i)=x(ip)
              ymap(i)=y(ip)
            END DO
            CALL curve (xmap,ymap,im)
          END DO
          DO i=1,im
            DO j=1,jm
              ip=i+(j-1)*im
              xmap(j)=x(ip)
              ymap(j)=y(ip)
            END DO
            CALL curve (xmap,ymap,jm)
          END DO
        END IF
      END IF
!
!-----------------------------------------------------------------------
!  Draw domain box.
!-----------------------------------------------------------------------
!
      IF (ldomain) THEN
        CALL gslwsc (c2)
        IF (spherical) THEN
          DO j=1,jm
            ip=1+(j-1)*im
            CALL maptrn (y(ip),x(ip),xmap(j),ymap(j))
          END DO
          CALL curve (xmap,ymap,jm)
          DO i=1,im
            ip=i+(jm-1)*im
            CALL maptrn (y(ip),x(ip),xmap(i),ymap(i))
          END DO
          CALL curve (xmap,ymap,im)
          DO j=1,jm
            ip=im+(j-1)*im
            CALL maptrn (y(ip),x(ip),xmap(j),ymap(j))
          END DO
          CALL curve (xmap,ymap,jm)
          DO i=1,im
            CALL maptrn (y(i),x(i),xmap(i),ymap(i))
          END DO
          CALL curve (xmap,ymap,im)
        ELSE
          DO j=1,jm
            ip=1+(j-1)*im
            xmap(j)=x(ip)
            ymap(j)=y(ip)
          END DO
          CALL curve (xmap,ymap,jm)
          DO i=1,im
            ip=i+(jm-1)*im
            xmap(i)=x(ip)
            ymap(i)=y(ip)
          END DO
          CALL curve (xmap,ymap,im)
          DO j=1,jm
            ip=im+(j-1)*im
            xmap(j)=x(ip)
            ymap(j)=y(ip)
          END DO
          CALL curve (xmap,ymap,jm)
          DO i=1,im
            xmap(i)=x(i)
            ymap(i)=y(i)
          END DO
          CALL curve (xmap,ymap,im)
        END IF
        CALL gslwsc (c1)
      END IF
!
!-----------------------------------------------------------------------
!  If requested, draw frame box.
!-----------------------------------------------------------------------
!
      IF (lframe) THEN
        CALL gslwsc (c2)
        CALL line (ul,ub,ul,ut)
        CALL line (ul,ut,ur,ut)
        CALL line (ur,ut,ur,ub)
        CALL line (ur,ub,ul,ub)
        CALL gslwsc (c1)
      END IF

      RETURN
      END

