      SUBROUTINE splint (x,y,y2,n,xx,yy,dydx)
!
!svn $Id: splint.F 833 2017-01-24 22:20:16Z arango $
!================================================== Hernan G. Arango ===
!  Copyright (c) 2002-2017 The ROMS/TOMS Group                         !
!    Licensed under a MIT/X style license                              !
!    See License_ROMS.txt                                              !
!=======================================================================
!                                                                      !
!  Given arrays X and Y of length N, which tabulate a function,        !
!  Y=f(X), with the Xs  in ascending order, and given the array        !
!  Y2 which contains the second derivative of the interpolating        !
!  function  at the  tabulated points X as computed  by routine        !
!  SPLINE, and given a value  XX, this routine returns a cubic-        !
!  spline interpolated value YY.                                       !
!                                                                      !
!  Reference :                                                         !
!                                                                      !
!  Press, W.H, B.P. Flannery, S.A. Teukolsky, and W.T. Vetterling,     !
!         1986: Numerical Recipes, the art of scientific computing.    !
!         Cambridge University Press.                                  !
!                                                                      !
!  Modified by H.G. Arango (1989) to output the first derivative       !
!  DYDX at a given value XX.                                           !
!                                                                      !
!=======================================================================
!
!-----------------------------------------------------------------------
!  Define global data.
!-----------------------------------------------------------------------
!
      implicit none
#include "pconst.h"
!
!-----------------------------------------------------------------------
!  Define local data.
!-----------------------------------------------------------------------
!
      integer k, khi, klo, n
      real a, b, c, d, dydx, e, f, h, xx, yy
      real x(n), y(n), y2(n)
!
!-----------------------------------------------------------------------
!  Begin executable code.
!-----------------------------------------------------------------------
!
!  Found the right place of XX in the table by means of bisection.
!
      klo=1
      khi=n
  10  IF ((khi-klo).gt.1) THEN
        k=(khi+klo)/2
        IF (x(k).gt.xx) THEN
          khi=k
        ELSE
          klo=k
        END IF
        GO TO 10
      END IF
!
!  KLO and KHI now bracket the input value XX.
!
      h=x(khi)-x(klo)
      IF (h.eq.c0) THEN
        PRINT *, ' SPLINT: bad X input, they must be distinct.'
        CALL crash ('SPLINT',1)
      END IF
!
!  Evaluate cubic spline polynomial.
!
      a=(x(khi)-xx)/h
      b=(xx-x(klo))/h
      c=(a*a*a-a)*(h*h)/c6
      d=(b*b*b-b)*(h*h)/c6
      e=(c3*(a*a)-c1)*h/c6
      f=(c3*(b*b)-c1)*h/c6
      yy=a*y(klo)+b*y(khi)+c*y2(klo)+d*y2(khi)
      dydx=(y(khi)-y(klo))/h-e*y2(klo)+f*y2(khi)

      RETURN
      END
