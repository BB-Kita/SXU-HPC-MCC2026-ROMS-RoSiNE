      SUBROUTINE spline (x,y,n,yp1,ypn,y2)
!
!svn $Id: spline.F 833 2017-01-24 22:20:16Z arango $
!================================================== Hernan G. Arango ===
!  Copyright (c) 2002-2017 The ROMS/TOMS Group                         !
!    Licensed under a MIT/X style license                              !
!    See License_ROMS.txt                                              !
!=======================================================================
!                                                                      !
!  Given X, Y of length N containing a tabulated function,  Y=f(X),    !
!  with the Xs  in ascending order,  and given values  Yp1 and  Ypn    !
!  for the first derivative of the interpolating function at points    !
!  1 and N, respectively this routine returns an array Y2 of length    !
!  N  which contains the  second  derivatives of the  interpolating    !
!  function at the tabulated points X.  If Yp1 and/or Ypn are equal    !
!  to  1.0E+30  or larger,  the routine  is  signalled  to  set the    !
!  corresponding boundary condition for a natural spline, with zero    !
!  second derivative on that boundary.                                 !
!                                                                      !
!  Reference :                                                         !
!                                                                      !
!  Press, W.H, B.P. Flannery, S.A. Teukolsky, and W.T. Vetterling,     !
!        1986: Numerical Recipes, the art of scientific computing.     !
!        Cambridge University Press.                                   !
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
!  Define local data.  Change NMAX as desired to be the largest
!  anticipated value of N.
!-----------------------------------------------------------------------
!
      integer i, k, n, nmax
      parameter (nmax=10000)
      real p, qn, sig, un, ypn, yp1
      real x(n), y(n), y2(n), u(nmax)
!
!-----------------------------------------------------------------------
!  Begin excutable code.
!-----------------------------------------------------------------------
!
      IF (n.gt.nmax) THEN
        PRINT 10, n,nmax
 10     FORMAT (/' SPLINE: underdimensioned array, N, NMAX = ',2i5)
        CALL crash ('SPLINE',1)
      END IF
!
!  The lower boundary condition is set either to be "natural" or else
!  to have a specified first derivative.
!
      IF (yp1.gt.spval2) THEN
        y2(1)=c0
        u(1)=c0
      ELSE
        y2(1)=-p5
        u(1)=(c3/(x(2)-x(1)))*((y(2)-y(1))/(x(2)-x(1))-yp1)
      END IF
!
!  This is the decomposition loop of the tridiagonal algorithm. Y2 and
!  U are used for temporary storage of the decomposition factors.
!
      DO i=2,n-1
        sig=(x(i)-x(i-1))/(x(i+1)-x(i-1))
        p=sig*y2(i-1)+c2
        y2(i)=(sig-c1)/p
        u(i)=(c6*((y(i+1)-y(i))/(x(i+1)-x(i))-                          &
     &           (y(i)-y(i-1))/(x(i)-x(i-1)))/                          &
     &           (x(i+1)-x(i-1))-sig*u(i-1))/p
      END DO
!
!  The upper boundary condition is set either to be "natural" or else
!  to have a specified first derivative.
!
      IF (ypn.gt.spval2) THEN
        qn=c0
        un=c0
      ELSE
        qn=p5
        un=(c3/(x(n)-x(n-1)))*(ypn-(y(n)-y(n-1))/(x(n)-x(n-1)))
      END IF
      y2(n)=(un-qn*u(n-1))/(qn*y2(n-1)+c1)
!
!  This is the back-substitution loop of the tridiagonal algorithm.
!
      DO k=n-1,1,-1
        y2(k)=y2(k)*y2(k+1)+u(k)
      END DO

      RETURN
      END
