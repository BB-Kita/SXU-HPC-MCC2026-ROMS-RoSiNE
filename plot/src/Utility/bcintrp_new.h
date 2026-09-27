#define CUBIC
#undef  CUBIC_XX
#define LIMTR
      FUNCTION bcintrp (xs,ys,is,js,fld,x,y,im,jm,spv,spval)
!
!svn $Id: bcintrp_new.h 833 2017-01-24 22:20:16Z arango $
!========================================== Alexander F. Shchepetkin ===
!  Copyright (c) 2002-2017 The ROMS/TOMS Group                         !
!    Licensed under a MIT/X style license                              !
!    See License_ROMS.txt                                              !
!=======================================================================
!                                                                      !
!  This routine interpolates field FLD, defined at curvilinear grid    !
!  (X,Y), to location (XS,YS).                                         !
!                                                                      !
!  On Input:                                                           !
!                                                                      !
!     xs, ys   Position at which the field is interpolated (km).       !
!     is, js   Indices of the grided cell in which (xs,ys) lies.       !
!     f        Field to interpolate from.                              !
!     x, y     Field positions (km).                                   !
!     im, jm   Inner and outer dimensions of "f".                      !
!     spv      field special value used during checking.               !
!     spval    field special value used during masking.                !
!                                                                      !
!  On Output:                                                          !
!                                                                      !
!     bcintrp  Interpolated value.                                     !
!                                                                      !
!=======================================================================
!
      implicit none
      integer im, is, iter, jm, js
      real a11, a12, a21, a22, cff, d1, d2, dx, dy, e11, e12, e21, e22, &
     &     eta, spv, spval, xi, xs, xy, ys, yx
      real bcintrp
      real fld(im,jm), x(im,jm), y(im,jm)
      real limtr
      parameter (limtr=3.0)

#ifdef CUBIC
# ifdef CUBIC_XX
      integer i, j
      real f(-1:2,-1:2)
# else
      real dfc, dfx(0:1,0:1), dfy(0:1,0:1), f(0:1,0:1)
# endif
      real f0, fx, fxx, fxxx, fxxy, fxy, fxyy, fy, fyy, fyyy
      real OneFortyEighth, OneThirtySecond, OneSixteenth,               &
     &     OneSixth, Quarter, Half, FiveSixteenth, FiveEighth,          &
     &     ThreeHalf, ThirteenTwentyFourth
      parameter (OneFortyEighth=1./48., OneThirtySecond=1./32.,         &
     &     OneSixteenth=1./16.,  OneSixth=1./6.,  Quarter=0.25,         &
     &     Half=0.5,   FiveSixteenth=5./16.,  FiveEighth=5./8.,         &
     &     ThreeHalf=1.5,   ThirteenTwentyFourth=13./24.)
#endif
!
!-----------------------------------------------------------------------
!  Interpolates requested field FLD to location (XS,YS).
!-----------------------------------------------------------------------
!
      IF ((fld(is  ,js).eq.spv).or.(fld(is+1,js+1).eq.spv).or.          &
     &    (fld(is+1,js).eq.spv).or.(fld(is  ,js+1).eq.spv)) THEN
        bcintrp=spval
        RETURN
      END IF
!
!  Determine local fractional coordinates (xi,eta) corresponding to
!  the target point (xs,ys) on the grid (x,y); "xi,eta" are defined,
!  in such a way, that xi=eta=0 corresponds to the middle of the cell
!  (is:is+1,js:js+1), while xi=+/-1/2 eta=+/-1/2 (any combination +/-
!  signs) corresponds to four corner point of the cell. Inside the cell
!  it is assumed that (x,y) are expressed via bi-linear functions of
!  (xi,eta), where term proportional to xi*eta does not vanish because
!  coordinate transformation may be at least weakly non-orthogonal due
!  to discretization errors. The associated non-linear system is solved
!  by iterative method of Newton.
!
      xy=x(is+1,js+1)-x(is,js+1)-x(is+1,js)+x(is,js)
      yx=y(is+1,js+1)-y(is,js+1)-y(is+1,js)+y(is,js)
!
      dx=xs -0.25*(x(is+1,js+1)+x(is,js+1)+x(is+1,js)+x(is,js))
      dy=ys -0.25*(y(is+1,js+1)+y(is,js+1)+y(is+1,js)+y(is,js))
!
      e11=0.5*(x(is+1,js+1)-x(is,js+1)+x(is+1,js)-x(is,js))
      e12=0.5*(x(is+1,js+1)+x(is,js+1)-x(is+1,js)-x(is,js))
      e21=0.5*(y(is+1,js+1)-y(is,js+1)+y(is+1,js)-y(is,js))
      e22=0.5*(y(is+1,js+1)+y(is,js+1)-y(is+1,js)-y(is,js))
!
      cff=1.0/(e11*e22-e12*e21)           ! Coordinate transformation
      xi=cff*(e22*dx-e12*dy)              ! matrix
      eta=cff*(e11*dy-e21*dx)             !          e11 e12
!                                         !          e21 e22
      DO iter=1,4                         !
        d1=dx -e11*xi-e12*eta-xy*xi*eta   ! contains derivatives of x,y
        d2=dy -e21*xi-e22*eta-yx*xi*eta   ! with respect to xi,eta.
!                                          ! Because the coordinates may
        a11=e11+xy*eta                    ! be non-orthogonal (at least
        a12=e12+xy*xi                     ! due to discretization
        a21=e21+yx*eta                    ! errors), nonlinear system
        a22=e22+yx*xi                     !
                                          ! e11*xi+e12*eta+xy*xi*eta=dx
        cff=1./(a11*a22-a12*a21)          ! e21*xi+e22*eta+yx*xi*eta=dy
        xi=xi + cff*(a22*d1-a12*d2)       !
        eta=eta+cff*(a11*d2-a21*d1)       ! needs to be solved in order
      END DO                               ! to retain symmetry.
#if VERBOSE > 3
      IF (ABS(xi).gt.0.500001 .or. ABS(eta).gt.0.500001) THEN
        WRITE (*,'(/2(1x,A),2I4,2F12.8/)') 'ERROR in bcintrp: point',   &
     &                       'is outside grid cell:', is,js,xi,eta
      END IF
#endif
#ifdef CUBIC
# ifdef CUBIC_XX
!
!  Genuinely two-dimensional, isotropic cubic interpolation scheme
!  using 12-point stencil.  In the code below the interpolated field
!  "f" is expanded into two-dimensional Taylor series of local frac-
!  tional coordinates "xi" and "eta", retaining all terms of combined
!  power up to third order (i.e., xi, eta, xi^2, eta^2, xi*eta, xi^3,
!  eta^3, xi^2*eta, and xi*eta^2), with all
!  coefficients (i.e., derivatives) computed via          x  x
!  two-dimensional finite difference expressions          |  |
!  of "natural" order of accuracy: 4th-order for       x--x--x--x
!  the field itself and its first derivatives in          |  |
!  both directions; and 2nd-order for all higher-      x--x--x--x
!  order derivatives. The permissible range of            |  |
!  of coordinates is -1/2 < xi,eta < +1/2, which          x--x
!  covers the central cell on the stencil, while
!  xi=eta=0 corresponds to its center. This interpolation scheme has
!  the property that if xi,eta=+/-1/2 (any combination of +/- signs)
!  it reproduces exactly value of the function at the corresponding
!  corner of the central "working" cell. However, it does not pass
!  exactly through the  extreme points of the stencil, where either
!  xi=+/-3/2 or eta+/-3/2. And, unlike a split-directional scheme,
!  when interpolating along the line eta=+/-1/2 (similarly xi=+/-1/2),
!  it has non-zero contribution from points on the side from the line,
!  except if xi=-1/2; 0; +1/2 (similarly eta=-1/2; 0; +1/2).
!
!  WARNING: Code segment below is a prototype algorithm. For the sake
!  of clarity, land masking capability is not implemented in this code
!  segment. It is kept here for illustration, reference, and debugging
!  purposes only. The complete algorithm is placed below.
!
        DO j=-1,2
          DO i=-1,2
            f(i,j)=fld(MAX(1,MIN(im,is+i)),MAX(1,MIN(jm,js+j)))
          END DO
        END DO

        f0=FiveSixteenth*(f(1,1)+f(1,0) +f(0,1)+f(0,0))-                &
     &     OneThirtySecond*(f(2,0)+f(2,1)+f(1,2)+f(0,2)+                &
     &                      f(-1,1)+f(-1,0)+f(0,-1)+f(1,-1))

        fx=FiveEighth*(f(1,1)+f(1,0)-f(0,1)-f(0,0))-                    &
     &     OneFortyEighth*(f(2,1)+f(2,0)-f(-1,1)-f(-1,0))-              &
     &     OneSixteenth*(f(1,2)-f(0,2)+f(1,-1)-f(0,-1))

        fy=FiveEighth*(f(1,1)-f(1,0)+f(0,1)-f(0,0))-                    &
     &     OneFortyEighth*(f(1,2)+f(0,2)-f(1,-1)-f(0,-1))-              &
     &     OneSixteenth*(f(2,1)-f(2,0)+f(-1,1)-f(-1,0))

        fxy=f(1,1)-f(1,0)-f(0,1)+f(0,0)

        fxx=Quarter*(f(2,1)-f(1,1)-f(0,1)+f(-1,1)+                      &
     &      f(2,0)-f(1,0)-f(0,0)+f(-1,0))

        fyy=Quarter*(f(1,2)-f(1,1)-f(1,0)+f(1,-1)+                      &
     &      f(0,2)-f(0,1)-f(0,0)+f(0,-1))

        fxxx=Half*(f(2,1)+f(2,0)-f(-1,1)-f(-1,0))-                      &
     &       ThreeHalf*(f(1,1)+f(1,0)-f(0,1)-f(0,0))

        fyyy=Half*(f(1,2)+f(0,2)-f(1,-1)-f(0,-1))-                      &
     &       ThreeHalf*(f(1,1)-f(1,0)+f(0,1)-f(0,0))

        fxxy=Half*(f(2,1)-f(1,1)-f(0,1)+f(-1,1)-                        &
     &             f(2,0)+f(1,0)+f(0,0)-f(-1,0))

        fxyy=Half*(f(1,2)-f(1,1)-f(1,0)+f(1,-1)-                        &
     &             f(0,2)+f(0,1)+f(0,0)-f(0,-1))
# else
!
!  Algorithm below is equivalent to the one above, except that special
!  care is taken to avoid interpolation accross land. This is achieved
!  by shortening the stencil and reducing order of polynomial, if
!  extreme points of the stencil touch land. This is achieved by
!  expressing all f0,fx,fy,...,fxyy in terms of values of interpolated
!  field at the four corners of central cell (which already checked to
!  stay away from land), and eight one-sided differences dfx,dfy (see
!  below) in such a way that field values at the extreme points of the
!  12-point stencil do not participate directly into f0,fx,...,fxyy.
!  Should an extreme point of the stencil touch land, thus making it
!  impossible to compute the corresponding one-sided difference, this
!  difference is retracted toward the center of the stencil.
!                                      ! Optionally a slope-limiting
        f(0,0)=fld(is  ,js  )          ! algorithm may be employed to
        f(1,0)=fld(is+1,js  )          ! prevent spurious oscillations
        f(0,1)=fld(is  ,js+1)          ! of the interpolant. This is a
        f(1,1)=fld(is+1,js+1)          ! valuable property, if dealing
!                                      ! with rough data, however, as
!                                      ! a side effect, it turns off
        dfc=f(1,1)-f(0,1)              ! high-order interpolation in
        IF (is+2.gt.im) THEN           ! the vicinity of extrema.
          dfx(1,1)=dfc
        ELSE IF (fld(is+2,js+1).eq.spv) THEN
          dfx(1,1)=dfc
        ELSE
          dfx(1,1)=fld(is+2,js+1)-f(1,1)
#  ifdef LIMTR
          IF ((dfx(1,1)*dfc).lt.0.0) THEN
            dfx(1,1)=0.0
          ELSE IF (ABS(dfx(1,1)).gt.(limtr*ABS(dfc))) THEN
            dfx(1,1)=limtr*dfc
          END IF
#  endif
        END IF                                 ! The slope-limiting
!                                             ! algorithm employed
        dfc=f(1,0)-f(0,0)                     ! here checks that two
        IF ((is+2).gt.im) THEN                ! consecutive elementary
          dfx(1,0)=dfc                        ! differences, "dfx" and
        ELSE IF (fld(is+2,js).eq.spv) THEN     ! "dfc" have the same
          dfx(1,0)=dfc                        ! sign and differ in
        ELSE                                  ! magnitude by no more
          dfx(1,0)=fld(is+2,js)-f(1,0)        ! than factor of 3.
#  ifdef LIMTR
          IF ((dfx(1,0)*dfc).lt.0.0) THEN
            dfx(1,0)=0.0
          ELSE IF (ABS(dfx(1,0)).gt.(limtr*ABS(dfc))) THEN
            dfx(1,0)=limtr*dfc
          END IF
#  endif
        END IF
!
        dfc=f(1,1)-f(0,1)
        IF (is-1.lt.1) THEN
          dfx(0,1)=dfc
        ELSE IF (fld(is-1,js+1).eq.spv) THEN
          dfx(0,1)=dfc
        ELSE
          dfx(0,1)=f(0,1)-fld(is-1,js+1)
#  ifdef LIMTR
          IF ((dfx(0,1)*dfc).lt.0.0) THEN
            dfx(0,1)=0.0
          ELSE IF (ABS(dfx(0,1)).gt.(limtr*ABS(dfc))) THEN
            dfx(0,1)=limtr*dfc
          END IF
#  endif
        END IF
!
        dfc=f(1,0)-f(0,0)
        IF (is-1.lt.1) THEN
          dfx(0,0)=dfc
        ELSE IF (fld(is-1,js).eq.spv) THEN
          dfx(0,0)=dfc
        ELSE
          dfx(0,0)=f(0,0)-fld(is-1,js)
#  ifdef LIMTR
          IF ((dfx(0,0)*dfc).lt.0.0) THEN
            dfx(0,0)=0.0
          ELSE IF (ABS(dfx(0,0)).gt.(limtr*ABS(dfc))) THEN
            dfx(0,0)=limtr*dfc
          END IF
#  endif
        END IF
!
        dfc=f(1,1)-f(1,0)
        IF (js+2.gt.jm) THEN
          dfy(1,1)=dfc
        ELSE IF (fld(is+1,js+2).eq.spv) THEN
          dfy(1,1)=dfc
        ELSE
          dfy(1,1)=fld(is+1,js+2)-f(1,1)
#  ifdef LIMTR
          IF ((dfy(1,1)*dfc).lt.0.0) THEN
            dfy(1,1)=0.0
          ELSE IF (ABS(dfy(1,1)).gt.(limtr*ABS(dfc))) THEN
            dfy(1,1)=limtr*dfc
          END IF
#  endif
        END IF
!
        dfc=f(0,1)-f(0,0)
        IF (js+2.gt.jm) THEN
          dfy(0,1)=dfc
        ELSE IF (fld(is,js+2).eq.spv) THEN
          dfy(0,1)=dfc
        ELSE
          dfy(0,1)=fld(is,js+2)-f(0,1)
#  ifdef LIMTR
          IF ((dfy(0,1)*dfc).lt.0.0) THEN
            dfy(0,1)=0.0
          ELSE IF (ABS(dfy(0,1)).gt.(limtr*ABS(dfc))) THEN
            dfy(0,1)=limtr*dfc
         END IF
#  endif
        END IF
!
        dfc=f(1,1)-f(1,0)
        IF (js-1.lt.1) THEN
          dfy(1,0)=dfc
        ELSE IF (fld(is+1,js-1).eq.spv) THEN
          dfy(1,0)=dfc
        ELSE
          dfy(1,0)=f(1,0)-fld(is+1,js-1)
#  ifdef LIMTR
          IF ((dfy(1,0)*dfc).lt.0.0) THEN
            dfy(1,0)=0.0
          ELSE IF (ABS(dfy(1,0)).gt.(limtr*ABS(dfc))) THEN
            dfy(1,0)=limtr*dfc
          END IF
#  endif
        END IF
!
        dfc=f(0,1)-f(0,0)
        IF (js-1.lt.1) THEN
          dfy(0,0)=dfc
        ELSE IF (fld(is,js-1).eq.spv) THEN
          dfy(0,0)=dfc
        ELSE
          dfy(0,0)=f(0,0)-fld(is,js-1)
#  ifdef LIMTR
          IF ((dfy(0,0)*dfc).lt.0.0) THEN
            dfy(0,0)=0.0
          ELSE IF (ABS(dfy(0,0)).gt.(limtr*ABS(dfc))) THEN
            dfy(0,0)=limtr*dfc
          END IF
#  endif
        END IF
!
        f0=Quarter*(f(1,1)+f(1,0)+f(0,1)+f(0,0))-                       &
     &     OneThirtySecond*(dfx(1,1)+dfx(1,0)-dfx(0,1)-dfx(0,0)+        &
     &                      dfy(1,1)-dfy(1,0)+dfy(0,1)-dfy(0,0))

        fx=ThirteenTwentyFourth*(f(1,1)-f(0,1)+f(1,0)-f(0,0))-          &
     &     OneFortyEighth*(dfx(1,1)+dfx(1,0)+dfx(0,1)+dfx(0,0))-        &
     &     OneSixteenth*(dfy(1,1)-dfy(0,1)-dfy(1,0)+dfy(0,0))

        fy=ThirteenTwentyFourth*(f(1,1)-f(1,0)+f(0,1)-f(0,0))-          &
     &     OneFortyEighth*(dfy(1,1)+dfy(0,1)+dfy(1,0)+dfy(0,0))-        &
     &     OneSixteenth*(dfx(1,1)-dfx(1,0)-dfx(0,1)+dfx(0,0))

        fxy=f(1,1)-f(1,0)-f(0,1)+f(0,0)

        fxx=Quarter*(dfx(1,1)-dfx(0,1)+dfx(1,0)-dfx(0,0))

        fyy=Quarter*(dfy(1,1)-dfy(1,0)+dfy(0,1)-dfy(0,0))

        fxxx=Half*(dfx(1,1)+dfx(1,0)+dfx(0,1)+dfx(0,0))-                &
     &       f(1,1)+f(0,1)-f(1,0)+f(0,0)

        fyyy=Half*(dfy(1,1)+dfy(0,1)+dfy(1,0)+dfy(0,0))-                &
     &       f(1,1)+f(1,0)-f(0,1)+f(0,0)

        fxxy=Half*(dfx(1,1)-dfx(0,1)-dfx(1,0)+dfx(0,0))

        fxyy=Half*(dfy(1,1)-dfy(1,0)-dfy(0,1)+dfy(0,0))
# endif
        bcintrp=f0 + fx*xi + fy*eta +                                   &
     &          Half*fxx*xi*xi + fxy*xi*eta + Half*fyy*eta*eta +        &
     &          OneSixth*fxxx*xi*xi*xi + Half*fxxy*xi*xi*eta +          &
     &          OneSixth*fyyy*eta*eta*eta + Half*fxyy*xi*eta*eta
#else
        bcintrp=(0.5+eta)*((0.5-xi)*fld(is  ,js+1)+                     &
     &                     (0.5+xi)*fld(is+1,js+1))+                    &
     &          (0.5-eta)*((0.5-xi)*fld(is  ,js  )+                     &
     &                     (0.5+xi)*fld(is+1,js  ))
#endif

      RETURN
      END
