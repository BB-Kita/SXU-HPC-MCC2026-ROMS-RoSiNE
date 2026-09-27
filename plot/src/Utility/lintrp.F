      SUBROUTINE lintrp (n,x,y,ni,xi,yi)
!
!svn $Id: lintrp.F 833 2017-01-24 22:20:16Z arango $
!================================================== Hernan G. Arango ===
!  Copyright (c) 2002-2017 The ROMS/TOMS Group                         !
!    Licensed under a MIT/X style license                              !
!    See License_ROMS.txt                                              !
!=======================================================================
!                                                                      !
!  Given arrays X and Y of length N, which tabulate a function,        !
!  Y = F(X),  with the Xs  in ascending order, and given array         !
!  XI of lenght NI, this routine returns a linear interpolated         !
!  array YI.                                                           !
!                                                                      !
!=======================================================================
!
!-----------------------------------------------------------------------
!  Define local variable.
!-----------------------------------------------------------------------
!
      implicit none
      integer i, ii, j, n, ni
      real d1, d2
      real x(n), y(n), xi(ni), yi(ni)
!
!-----------------------------------------------------------------------
!  Begin executable code.
!-----------------------------------------------------------------------
!
      DO j=1,ni
        IF (xi(j).le.x(1)) THEN
          ii=1
          yi(j)=y(1)
        ELSE IF (xi(j).ge.x(n))then
          yi(j)=y(n)
        ELSE
          DO i=1,n-1
            IF ((x(i).lt.xi(j)).and.(xi(j).le.x(i+1))) THEN
              ii=i
              GO TO 10
            END IF
          END DO
 10       d1=xi(j)-x(ii)
          d2=x(ii+1)-xi(j)
          yi(j)=(d1*y(ii+1)+d2*y(ii))/(d1+d2)
        END IF
      END DO

      RETURN
      END
