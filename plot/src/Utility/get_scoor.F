      SUBROUTINE get_scoor (xpos,ypos,im,jm)
!
!svn $Id: get_scoor.F 833 2017-01-24 22:20:16Z arango $
!================================================== Hernan G. Arango ===
!  Copyright (c) 2002-2017 The ROMS/TOMS Group                         !
!    Licensed under a MIT/X style license                              !
!    See License_ROMS.txt                                              !
!=======================================================================
!                                                                      !
!  This routine sets the section horizontal coordinate.                !
!                                                                      !
!  On Input:                                                           !
!                                                                      !
!     xpos     X-positions of requested field (km or degrees).         !
!     ypos     Y-positions of requested field (km or degrees).         !
!     im       size of inner dimension.                                !
!     jm       size of outer dimension.                                !
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
#include "pltfld.h"
#include "pltio.h"
#include "secpos.h"
!
!-----------------------------------------------------------------------
!  Define local data.
!-----------------------------------------------------------------------
!
      integer i, i1, i2, im, istep, j, j1, j2, jm, jstep, n
      real c, slope, small, u, v, xs1, xs2, ys1, ys2
      real xpos(im,jm), ypos(im,jm)
      parameter (small=1.0e-6)
!
!=======================================================================
!  Begin executable code.
!=======================================================================
!
!-----------------------------------------------------------------------
!  Set section horizontal coordinates.
!-----------------------------------------------------------------------
!
!  Set starting and ending section points.  If points are in grid units,
!  get them physical units.
!
      IF (.not.spherical.and.(isecpos.eq.0)) THEN
        i1=INT(xsec1)
        i2=INT(xsec2)
        j1=INT(ysec1)
        j2=INT(ysec2)
        IF ((i1.eq.i2).or.(j1.eq.j2)) THEN
          IF (i1.eq.i2) THEN
            j1=MAX(1 ,MIN(INT(ysec1),INT(ysec2)))
            j2=MIN(jm,MAX(INT(ysec1),INT(ysec2)))
            n=0
            DO j=j1,j2
              n=n+1
              xsec(n)=xpos(i1,j)
              ysec(n)=ypos(i1,j)
            END DO
            ims=n
            xs1=xsec(1)
            xs2=xsec(ims)
            ys1=ysec(1)
            ys2=ysec(ims)
            GO TO 30
          ELSE IF (j1.eq.j2) THEN
            i1=MAX(1 ,MIN(INT(xsec1),INT(xsec2)))
            i2=MIN(im,MAX(INT(xsec1),INT(xsec2)))
            n=0
            DO i=i1,i2
              n=n+1
              xsec(n)=xpos(i,j1)
              ysec(n)=ypos(i,j1)
            END DO
            ims=n
            xs1=xsec(1)
            xs2=xsec(ims)
            ys1=ysec(1)
            ys2=ysec(ims)
            GO TO 30
          END IF
        END IF
        i=INT(xsec1)
        j=INT(ysec1)
        u=xsec1-AINT(xsec1)
        v=ysec1-AINT(ysec1)
        IF ((u.eq.c0).and.(v.eq.0)) THEN
          IF (i.lt.1) i=1
          IF (i.gt.im) i=im
          IF (j.lt.1) j=1
          IF (j.gt.jm) j=jm
          xs1=xpos(i,j)
          ys1=ypos(i,j)
        ELSE
          IF (i.lt.1) i=1
          IF (i.gt.im-1) i=im-1
          IF (j.lt.1) j=1
          IF (j.gt.jm-1) j=jm-1
          xs1=(c1-u)*(c1-v)*xpos(i,j)+u*(c1-v)*xpos(i+1,j)+             &
     &        u*v*xpos(i+1,j+1)+v*(c1-u)*xpos(i,j+1)
          ys1=(c1-u)*(c1-v)*ypos(i,j)+u*(c1-v)*ypos(i+1,j)+             &
     &        u*v*ypos(i+1,j+1)+v*(c1-u)*ypos(i,j+1)
        END IF
        i=INT(xsec2)
        j=INT(ysec2)
        u=xsec2-AINT(xsec2)
        v=ysec2-AINT(ysec2)
        IF ((u.eq.c0).and.(v.eq.0)) THEN
          IF (i.lt.1) i=1
          IF (i.gt.im) i=im
          IF (j.lt.1) j=1
          IF (j.gt.jm) j=jm
          xs2=xpos(i,j)
          ys2=ypos(i,j)
        ELSE
          IF (i.lt.1) i=1
          IF (i.gt.im-1) i=im-1
          IF (j.lt.1) j=1
          IF (j.gt.jm-1) j=jm-1
          xs2=(c1-u)*(c1-v)*xpos(i,j)+u*(c1-v)*xpos(i+1,j)+             &
     &        u*v*xpos(i+1,j+1)+v*(c1-u)*xpos(i,j+1)
          ys2=(c1-u)*(c1-v)*ypos(i,j)+u*(c1-v)*ypos(i+1,j)+             &
     &        u*v*ypos(i+1,j+1)+v*(c1-u)*ypos(i,j+1)
        END IF
      ELSE IF ((isecpos.eq.1).or.(isecpos.eq.2)) THEN
        CALL hindices (i1,j1,xsec1,ysec1,1,xpos,ypos,im,jm)
        CALL hindices (i2,j2,xsec2,ysec2,1,xpos,ypos,im,jm)
!
!  Check starting point. Adjust if possible.
!
        IF ((i1.ne.0).or.(j1.ne.0)) THEN
          xs1=xsec1
          ys1=ysec1
        ELSE
          IF ((i2.ne.0).or.(j2.ne.0)) THEN
            IF (xsec1.eq.xsec2) THEN                 ! along x-direction
              xs1=xsec1
              IF (ysec1.gt.ypos(i2,jm)) THEN
                ys1=ypos(i2,jm)                      ! use top value
              ELSE IF (ysec1.lt.ypos(i2,1)) THEN
                ys1=ypos(i2,1 )                      ! use bottom value
              END IF
            ELSE IF (ysec1.eq.ysec2) THEN            ! along y-direction
              ys1=ysec1
              IF (xsec1.gt.xpos(im,j2)) THEN
                xs1=xpos(im,j2)                      ! use right value
              ELSE IF (xsec1.lt.xpos(1,j2)) THEN
                xs1=xpos(1 ,j2)                      ! use left value
              END IF
            ELSE
              WRITE (stdout,10) 'starting point out of bounds',         &
     &                          'Check (XSEC1,YSEC1)'
  10          FORMAT (/,' GET_SCOOR: ',a,                               &
     &                /,12x,'in cross-section specification. ',         &
     &                /,12x,a)
              STOP
            END IF
          ELSE
            WRITE (stdout,10) 'starting/ending points out of bounds',   &
     &                        'Check (XSEC1,YSEC1) and (XSEC2,YSEC2).'
            STOP
          END IF
        END IF
!
!  Check ending point. Adjust if possible.
!
        IF ((i2.ne.0).or.(j2.ne.0)) THEN
          xs2=xsec2
          ys2=ysec2
        ELSE
          IF ((i1.ne.0).or.(j1.ne.0)) THEN
            IF (xsec1.eq.xsec2) THEN                 ! along x-direction
              xs2=xsec2
              IF (ysec2.gt.ypos(i1,jm)) THEN
                ys2=ypos(i1,jm)                      ! use top value
              ELSE IF (ysec2.lt.ypos(i1,1)) THEN
                ys2=ypos(i1,1 )                      ! use bottom value
              END IF
            ELSE IF (ysec1.eq.ysec2) THEN            ! along y-direction
              ys2=ysec2
              IF (xsec2.gt.xpos(im,j1)) THEN
                xs2=xpos(im,j1)                      ! use right value
              ELSE IF (xsec2.lt.xpos(1,j1)) THEN
                xs2=xpos(1 ,j1)                      ! use left value
              END IF
            ELSE
              WRITE (stdout,10) 'ending point out of bounds',           &
     &                          'Check (XSEC1,YSEC1)'
              STOP
            END IF
          END IF
        END IF          
      END IF
!
!  Sections along XI- or ETA-axis but in spherical configuration.
!
      IF (spherical.and.(isecpos.eq.0)) THEN
        i1=MAX(1,MIN(im,INT(xsec1)))
        j1=MAX(1,MIN(jm,INT(ysec1)))
        i2=MAX(1,MIN(im,INT(xsec2)))
        j2=MAX(1,MIN(jm,INT(ysec2)))
        IF (i1.eq.i2) THEN
          n=0
          jstep=1
          IF (j1.gt.j2) jstep=-1
          DO j=j1,j2,jstep
            n=n+1
            xsec(n)=xpos(i1,j)
            ysec(n)=ypos(i1,j)
          END DO
          ims=n
        ELSE IF (j1.eq.j2) THEN
          n=0
          istep=1
          IF (i1.gt.i2) istep=-1
          DO i=i1,i2,istep
            n=n+1
            xsec(n)=xpos(i,j1)
            ysec(n)=ypos(i,j1)
          END DO
          ims=n
        ELSE
          WRITE (stdout,10) 'starting/ending points out of bounds',     &
     &                      'Check (XSEC1,YSEC1) and (XSEC2,YSEC2).'
          STOP
        END IF
      ELSE
!
!  Compute the slope of the line connecting the two points defining
!  the section.
!
        IF (xs1.eq.xs2) THEN
          slope=c0
        ELSE
          slope=(ys2-ys1)/(xs2-xs1)
        END IF
        c=dels/SQRT(c1+slope*slope)
!
!  Set section starting point.
!
        n=1
        xsec(n)=xs1
        ysec(n)=ys1
!
!  Compute interior points.
!
        DO WHILE (.true.)
          n=n+1
          IF (xs1.eq.xs2) THEN
            xsec(n)=xsec(n-1)
            IF (ys1.lt.ys2) THEN
              ysec(n)=ysec(n-1)+c
              IF (ysec(n).gt.ys2) GO TO 20
            ELSE
              ysec(n)=ysec(n-1)-c
              IF (ysec(n).lt.ys2) GO TO 20
            END IF
          ELSE IF (ys1.eq.ys2) THEN
            ysec(n)=ysec(n-1)
            IF (xs1.lt.xs2) THEN
              xsec(n)=xsec(n-1)+c
              IF (xsec(n).gt.xs2) GO TO 20
            ELSE
              xsec(n)=xsec(n-1)-c
              IF (xsec(n).lt.xs2) GO TO 20
            END IF
          ELSE
            IF (xs1.lt.xs2) THEN
              xsec(n)=xsec(n-1)+c
              IF (xsec(n).gt.xs2) GO TO 20
            ELSE
              xsec(n)=xsec(n-1)-c
              IF (xsec(n).lt.xs2) GO TO 20
            END IF
            ysec(n)=ysec(n-1)+slope*(xsec(n)-xsec(n-1))
          END IF
        END DO
!
!  Set last section point.
!
  20    IF ((xsec(n-1).ne.xs2).and.(ysec(n-1).ne.ys2)) THEN
          ims=n
          xsec(n)=xs2
          ysec(n)=ys2
        ELSE
          ims=n-1
        END IF
      END IF
!
!  Subtract or add a small value to section ends to bound coordinates
!  inside the grid during indices search by routine INSIDE.
!
  30  IF (xs1.gt.xs2) THEN
        xsec(1)=xsec(1)-small
        xsec(ims)=xsec(ims)+small
      ELSE IF (xs1.lt.xs2) THEN
        xsec(1)=xsec(1)+small
        xsec(ims)=xsec(ims)-small
      END IF
      IF (ys1.gt.ys2) THEN
        ysec(1)=ysec(1)-small
        ysec(ims)=ysec(ims)+small
      ELSE IF (ys1.lt.ys2) THEN
        ysec(1)=ysec(1)+small
        ysec(ims)=ysec(ims)-small
      END IF

      RETURN
      END
