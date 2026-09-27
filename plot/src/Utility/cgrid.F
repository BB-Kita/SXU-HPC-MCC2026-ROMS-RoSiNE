      SUBROUTINE cgrid (x,y,ixy,jxy,xg,yg,im,jm,igrid,derive)
!
!svn $Id: cgrid.F 833 2017-01-24 22:20:16Z arango $
!================================================== Hernan G. Arango ===
!  Copyright (c) 2002-2017 The ROMS/TOMS Group                         !
!    Licensed under a MIT/X style license                              !
!    See License_ROMS.txt                                              !
!=======================================================================
!                                                                      !
!  This routine computes requested horizontal grid from curvilinear    !
!  metric coefficients.                                                !
!                                                                      !
!  On Input:                                                           !
!                                                                      !
!     ixy        Size of inner dimension for "x" and "y".              !
!     jxy        Size of outer dimension for "x" and "y".              !
!     xg         Curvilinear metric (m) or longitude (degrees) in      !
!                the XI-direction.                                     !
!     yg         Curvilinear metric (m) or latitude (degrees) in       !
!                the ETA-direction.                                    !
!     im         Size of inner dimension for "xg" and "yg".            !
!     jm         Size of outer dimension for "xg" and "yg".            !
!     igrid      Staggered C grid type:                                !
!                 igrid=0  => None.                                    !
!                 igrid=1  => density points.                          !
!                 igrid=2  => streamfunction points.                   !
!                 igrid=3  => u-velocity points.                       !
!                 igrid=4  => v-velocity points.                       !
!                 igrid=5  => w-velocity points.                       !
!                 igrid=6  => w-velocity points, range 0:N.            !
!                 igrid=7  => interior density points.                 !
!     derive     Switch to derive Cartesian grid (logical).            !
!                                                                      !
!  On Output:                                                          !
!                                                                      !
!     x        X-positions of current field.                           !
!     y        Y-positions of current field.                           !
!                                                                      !
!=======================================================================
!
!-----------------------------------------------------------------------
!  Define local variables.
!-----------------------------------------------------------------------
!
      implicit none
      logical derive
      integer i, igrid, im, ixy, j, jm, jxy
      real c0, c1, p5, p25
      real xg(im,jm), yg(im,jm), x(ixy,jxy), y(ixy,jxy)
      parameter (c0=0.0, c1=1.0, p5=0.5, p25=0.25)
!
!=======================================================================
!  Begin executable code.
!=======================================================================
!
      IF (.not.derive) THEN
!
!-----------------------------------------------------------------------
!  Compute requested grid.
!-----------------------------------------------------------------------
!
!  Compute horizontal grid positions at RHO-points.
!
        IF ((igrid.eq.1).or.(igrid.eq.5).or.(igrid.eq.6)) THEN
          DO j=1,jxy
            DO i=1,ixy
              x(i,j)=xg(i,j)
              y(i,j)=yg(i,j)
            END DO
          END DO
!
!  Compute horizontal grid positions at streamfunction points.
!
        ELSE IF (igrid.eq.2) THEN
          DO j=1,jxy
            DO i=1,ixy
              x(i,j)=p25*(xg(i,j)+xg(i+1,j)+xg(i,j+1)+xg(i+1,j+1))
              y(i,j)=p25*(yg(i,j)+yg(i+1,j)+yg(i,j+1)+yg(i+1,j+1))
            END DO
          END DO
!
!  Compute horizontal grid positions at U-velocity points.
!
        ELSE IF (igrid.eq.3) THEN
          DO j=1,jxy
            DO i=1,ixy
              x(i,j)=p5*(xg(i,j)+xg(i+1,j))
              y(i,j)=p5*(yg(i,j)+yg(i+1,j))
            END DO
          END DO
!
!  Compute horizontal grid positions at V-velocity points.
!
        ELSE IF (igrid.eq.4) THEN
          DO j=1,jxy
            DO i=1,ixy
              x(i,j)=p5*(xg(i,j)+xg(i,j+1))
              y(i,j)=p5*(yg(i,j)+yg(i,j+1))
            END DO
          END DO
!
!  Compute horizontal grid positions at interior density points.
!
        ELSE IF (igrid.eq.7) THEN
          DO j=1,jxy
            DO i=1,ixy
              x(i,j)=xg(i+1,j+1)
              y(i,j)=yg(i+1,j+1)
            END DO
          END DO
        END IF
      ELSE
!
!-----------------------------------------------------------------------
!  Derive Cartesian grid from curvilinear metric terms.
!-----------------------------------------------------------------------
!
        DO j=1,jm
          DO i=1,im
            xg(i,j)=c1/xg(i,j)
            yg(i,j)=c1/yg(i,j)
          END DO
        END DO
!
!  Compute horizontal grid positions at RHO-points.
!
        IF ((igrid.eq.1).or.(igrid.eq.5).or.(igrid.eq.6)) THEN
          DO j=1,jxy
            x(1,j)=-p5*xg(1,j)
            DO i=2,ixy
              x(i,j)=x(i-1,j)+p5*(xg(i-1,j)+xg(i,j))
            END DO
          END DO
          DO i=1,ixy
            y(i,1)=-p5*xg(i,1)
            DO j=2,jxy
              y(i,j)=y(i,j-1)+p5*(yg(i,j-1)+yg(i,j))
            END DO
          END DO
!
!  Compute horizontal grid positions at streamfunction points.
!
        ELSE IF (igrid.eq.2) THEN
          DO j=1,jxy
            x(1,j)=c0
            DO i=2,ixy
              x(i,j)=x(i-1,j)+xg(i,j)
            END DO
          END DO
          DO i=1,ixy
            y(i,1)=c0
            DO j=2,jxy
              y(i,j)=y(i,j-1)+yg(i,j)
            END DO
          END DO
!
!  Compute horizontal grid positions at U-velocity points.
!
        ELSE IF (igrid.eq.3) THEN
          DO j=1,jxy
            x(1,j)=c0
            DO i=2,ixy
              x(i,j)=x(i-1,j)+xg(i,j)
            END DO
          END DO
          DO i=1,ixy
            y(i,1)=-p5*xg(i,1)
            DO j=2,jxy
              y(i,j)=y(i,j-1)+p5*(yg(i,j-1)+yg(i,j))
            END DO
          END DO
!
!  Compute horizontal grid positions at V-velocity points.
!
        ELSE IF (igrid.eq.4) THEN
          DO j=1,jxy
            x(1,j)=-p5*xg(1,j)
            DO i=2,ixy
              x(i,j)=x(i-1,j)+p5*(xg(i-1,j)+xg(i,j))
            END DO
          END DO
          DO i=1,ixy
            y(i,1)=c0
            DO j=2,jxy
              y(i,j)=y(i,j-1)+yg(i,j)
            END DO
          END DO
!
!  Compute horizontal grid positions at interior density points.
!
        ELSE IF (igrid.eq.7) THEN
          DO j=1,jxy
            x(1,j)=p5*xg(2,j+1)
            DO i=2,ixy
              x(i,j)=x(i-1,j)+p5*(xg(i,j+1)+xg(i+1,j+1))
            END DO
          END DO
          DO i=1,ixy
            y(i,1)=-p5*xg(i+1,2)
            DO j=2,jxy
              y(i,j)=y(i,j-1)+p5*(yg(i+1,j)+yg(i+1,j+1))
            END DO
          END DO
        END IF
      END IF

      RETURN
      END
