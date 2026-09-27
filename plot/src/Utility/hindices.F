#define CHECKING
      SUBROUTINE hindices (isec,jsec,xsec,ysec,ims,xfld,yfld,im,jm)
!
!svn $Id: hindices.F 833 2017-01-24 22:20:16Z arango $
!================================================== Hernan G. Arango ===
!  Copyright (c) 2002-2017 The ROMS/TOMS Group                         !
!    Licensed under a MIT/X style license                              !
!    See License_ROMS.txt                                              !
!=======================================================================
!                                                                      !
!  This routine extracts the (i,j) indices of the grided field cell    !
!  containing section.                                                 !
!                                                                      !
!  On Input:                                                           !
!                                                                      !
!     xsec    Section X-coordinate (km).                               !
!     ysec    Section Y-coordinate (km).                               !
!     ims     Number of section points in the horizontal direction.    !
!     xfld    Field X-positions of field (km).                         !
!     yfld    Field Y-positions of field (km).                         !
!     im      Number of field points in the X-direction.               !
!     jm      Number of field points in the Y-direction.               !
!                                                                      !
!  On Output:                                                          !
!                                                                      !
!     isec    Field cell I-indices containing the section.             !
!     jsec    Field cell J-indices containing the section.             !
!                                                                      !
!  Calls:                                                              !
!                                                                      !
!    crash, Try_range                                                  !
!                                                                      !
!=======================================================================
!
      implicit none
      logical found, Try_Range
      integer i0, im, imax, imin, ims, j0, jm, jmax, jmin, n
      integer isec(ims), jsec(ims)
      real xfld(im,jm), xsec(ims), yfld(im,jm), ysec(ims)
#ifdef CHECKING
      real dx0, dx1, dx2, dx3, dy0, dy1, dy2, dy3, s01, s12, s23, s30
#endif
!
!-----------------------------------------------------------------------
!  Find grid cell indices.
!-----------------------------------------------------------------------
!
!  Reset all indices.
!
      DO n=1,ims
        isec(n)=0
        jsec(n)=0
      END DO
!
!  Check if [xsec(:),ysec(:)] falls inside the whole domain.
!
      DO n=1,ims
        found=Try_Range(1,im,1,jm,xsec(n),ysec(n),xfld,yfld,im,jm)
        IF (found) THEN
          imin=1
          imax=im
          jmin=1
          jmax=jm
!
!  Once it is established that the point belongs to the whole domain,
!  find the exact box to which it belongs by dividing the domain
!  successively by a half and checking.
!
          DO WHILE ((imax-imin.gt.1).or.(jmax-jmin.gt.1))
            IF ((imax-imin).gt.1) THEN
              i0=(imin+imax)/2
              found=Try_Range(imin,i0,jmin,jmax,xsec(n),ysec(n),        &
     &                        xfld,yfld, im,jm)
              IF (found) THEN
                imax=i0
              ELSE
                imin=i0
              END IF
            END IF
            IF ((jmax-jmin).gt.1) THEN
              j0=(jmin+jmax)/2
              found=Try_Range(imin,imax,jmin,j0,xsec(n),ysec(n),        &
     &                        xfld,yfld,im,jm)
              IF (found) THEN
                jmax=j0
              ELSE
                jmin=j0
              END IF
            END IF
          END DO
#ifdef CHECKING
!
!  Check, that point [xsec(:),ysec(:)] indeed belongs to the
!  grid box (imin:imin+1,jmin:jmin+1).  Since the grid box is
!  a CONVEX polygon, if the point [xsec(:),ysec(:)] is inside,
!  then all consecutive pairs of corners of the grid box
!  numbered as 0 --> 1 --> 2 --> 3 --> 0 must be seen either
!  left --> right  or  right --> left (all the same), which is
!  equivalent to the mathematical condition that all cyclic
!  vector products must be of the same sign. Failure of this
!  checking indicates logical algorithm error in either of
!  Try_Range or logical function inside.
!
          dx0=xfld(imin  ,jmin  )-xsec(n)
          dx1=xfld(imin+1,jmin  )-xsec(n)
          dx2=xfld(imin+1,jmin+1)-xsec(n)
          dx3=xfld(imin  ,jmin+1)-xsec(n)
          dy0=yfld(imin  ,jmin  )-ysec(n)
          dy1=yfld(imin+1,jmin  )-ysec(n)
          dy2=yfld(imin+1,jmin+1)-ysec(n)
          dy3=yfld(imin  ,jmin+1)-ysec(n)
!
! Check that all cyclic vector products have the same sign.
!
          s01=dx0*dy1-dy0*dx1
          s12=dx1*dy2-dy1*dx2
          s23=dx2*dy3-dy2*dx3
          s30=dx3*dy0-dy3*dx0
!
          IF ((s01.ge.0.0).and.(s12.ge.0.0).and.                        &
     &        (s23.ge.0.0).and.(s30.ge.0.0)) THEN
            isec(n)=imin
            jsec(n)=jmin
          ELSE IF ((s01.le.0.0).and.(s12.le.0.0).and.                    &
     &            (s23.le.0.0).and.(s30.le.0.0)) THEN
            isec(n)=imin
            jsec(n)=jmin
          ELSE
            PRINT 10
 10         FORMAT (' HINDICES - Algorithm failure.')
            CALL crash ('HINDICES',1)
          END IF
#else
          isec(n)=imin
          jsec(n)=jmin
#endif
        END IF
      END DO

      RETURN
      END
 
      LOGICAL FUNCTION Try_Range (imin,imax,jmin,jmax,x0,y0,            &
     &                            xfld,yfld,im,jm)
!
!=======================================================================
!  Copyright (c) 2000 Rutgers University                               !
!=======================================================================
!                                                                      !
!  This function finds if the point (x0,y0) is inside of the box       !
!  defined by the corners (imin,jmin) and (imax,jmax).                 !
!                                                                      !
!  On Input:                                                           !
!                                                                      !
!     imin       I-index of search box lower-left corner.              !
!     imax       I-index of search box upper-right corner.             !
!     jmin       J-index of search box lower-left corner.              !
!     jmax       J-index of search box upper-right corner.             !
!     x0         X-coordinate of point to search.                      !
!     y0         Y-coordinate of point to search.                      !
!     im         Number of field points in the X-direction.            !
!     jm         Number of field points in the Y-direction.            !
!     xfld       grided X-positions of field to search.                !
!     yfld       grided Y-positions of field to search.                !
!                                                                      !
!  On Output:                                                          !
!                                                                      !
!     Try_Range  Logical switch indicating if point (x0,y0) is         !
!                inside or outside of search polygon.                  !
!                                                                      !
!  Calls:                                                              !
!                                                                      !
!    crash, inside                                                     !
!                                                                      !
!=======================================================================
!
      implicit none
#include "param.h"
!
      logical inside
      integer i, im, imax, imin, j, jm, jmax, jmin, nb, shft
      real x0, y0
      real xb(NX), xfld(im,jm), yb(NX), yfld(im,jm)
!
!-----------------------------------------------------------------------
!  Find if point (x0,y0) is inside of requested polygon.
!-----------------------------------------------------------------------
!
!  Check if allocated number of points is large enough, then if so,
!  define closed polygon.  NOTE that the last point [xb(nb),yb(nb)]
!  does not repeat [xb(1),yb(1)].  Instead in "inside" it is implied
!  that the closing segment is [xb(nb),yb(nb)] --> [xb(1),yb(1)].
!  In fact, the logical function inside sets xb(nb+1)=xb(1) and
!   yb(nb+1)=yb(1).
!
      nb=2*(jmax-jmin+imax-imin)
      IF (nb.lt.NX) THEN
        shft=1-imin
        DO i=imin,imax-1
          xb(i+shft)=xfld(i,jmin)
          yb(i+shft)=yfld(i,jmin)
        END DO
        shft=1-jmin+imax-imin
        DO j=jmin,jmax-1
          xb(j+shft)=xfld(imax,j)
          yb(j+shft)=yfld(imax,j)
        END DO
        shft=1+jmax-jmin+2*imax-imin
        DO i=imax,imin+1,-1
          xb(shft-i)=xfld(i,jmax)
          yb(shft-i)=yfld(i,jmax)
        END DO
        shft=1+ 2*jmax-jmin+2*(imax-imin)
        DO j=jmax,jmin+1,-1
          xb(shft-j)=xfld(imin,j)
          yb(shft-j)=yfld(imin,j)
        END DO
!
!  Check if point (x0,y0) is inside of the polygon.
!
        Try_Range=inside(x0,y0,xb,yb,nb)
      ELSE
        PRINT 10, nb
 10     FORMAT (' Try_Range - insufficient number of points.',          &
     &          13x,'parameter NX must be at least',i6,'.')
        CALL crash ('Try_Range',1)
      END IF

      RETURN
      END
