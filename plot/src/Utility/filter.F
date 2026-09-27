      SUBROUTINE filter (f,im,jm,km,ismooth,nord,nsappl)
!
!svn $Id: filter.F 833 2017-01-24 22:20:16Z arango $
!================================================== Hernan G. Arango ===
!  Copyright (c) 2002-2017 The ROMS/TOMS Group                         !
!    Licensed under a MIT/X style license                              !
!    See License_ROMS.txt                                              !
!=======================================================================
!                                                                      !
!  This routine smooths the a field F using Bleck's 25 point filter.   !
!                                                                      !
!  On Input:                                                           !
!                                                                      !
!     f        Field to smooth (real array).                           !
!     im       number of points in the X-direction (integer).          !
!     jm       number of points in the Y-direction (integer).          !
!     km       number of points in the Z-direction (integer).          !
!     ismooth  Type of smoothing filter (integer):                     !
!              [1] Bleck 25-point filter.                              !
!              [2] NORD Shapiro filter.                                !
!     nord     Order of Shapiro filter (integer).                      !
!     nsappl   Number of smoothing applications (integer).             !
!                                                                      !
!  On Input:                                                           !
!                                                                      !
!     f        Smoothed field (real array).                            !
!                                                                      !
!                                                                      !
!  Calls:   BLECK, SHAPIRO                                             !
!                                                                      !
!=======================================================================
!
      implicit none
#include "param.h"
!
      integer im, ismooth, jm, k, km, n, nord, nsappl
      real f(im,jm,km), wk(NH)
!
!=======================================================================
!  Begin executable code.
!=======================================================================
!
!  Bleck 25-point filter.
!
      IF (ismooth.eq.1) THEN
        DO n=1,nsappl
          DO k=1,km
            CALL bleck (f(1,1,k),im,jm,wk,1,im,1,jm)
          END DO
        END DO
!
!  Shapiro filter.
!
      ELSE IF (ismooth.eq.2) THEN
        DO n=1,nsappl
          DO k=1,km
            CALL shapiro (f(1,1,k),im,im,jm,nord)
          END DO
        END DO
      END IF

      RETURN
      END

      SUBROUTINE bleck (f,iln,ilt,w,lis,li,ljs,lj)
!
!=======================================================================
!                                                                      !
!  This is a filter using Bleck's 25 point smoothing scheme.           !
!                                                                      !
!  On Input:                                                           !
!                                                                      !
!     f        2D-field to smooth (real array).                        !
!     iln      Size of first-dimension (integer).                      !
!     ilt      Size of second-dimension (integer).                     !
!     w        2D work array (real array).                             !
!     lis      Starting-index in the first-dimension.                  !
!     li       ENDing-index in the first-dimension.                    !
!     ljs      Starting-index in the first-dimension.                  !
!     lj       ENDing-index in the first-dimension.                    !
!                                                                      !
!  On Input:                                                           !
!                                                                      !
!     f        Smoothed 2D-field (real array).                         !
!                                                                      !
!  Bleck, R., 1965:   Lineare approximationsmethoden zur bestimmung    !
!      ein-und zweidimensionaler numerischer filter der dynamischen    !
!      meteorologie.  Intitut  fur  Theoretische  Meteorologie  der    !
!      Freien Universitat Berlin.                                      !
!                                                                      !
!=======================================================================
!
      implicit none
      integer i, iln, ilt, j, li, li1, li2, lis, lis1, lis2, lj, lj1,   &
     &        lj2, ljs, ljs1, ljs2
      real c2, c3, c4, p25, r16
      real f(iln,ilt), w(iln,ilt)
      parameter (c2=2.0, c3=3.0, c4=4.0, p25=0.25, r16=0.0625)
!
!=======================================================================
!  Begin executable code.
!=======================================================================
!
      li1=li-1
      lj1=lj-1
      li2=li1-1
      lj2=lj1-1
      lis1=lis+1
      ljs1=ljs+1
      lis2=lis1+1
      ljs2=ljs1+1
!
!  Smooth boundaries with 3-point (1-2-1) smoother.
!
      DO j=ljs1,lj1
        w(lis,j)=p25*(f(lis,j+1)+f(lis,j-1)+c2*f(lis,j))
        w(li ,j)=p25*(f(li ,j+1)+f(li ,j-1)+c2*f(li ,j))
      END DO
      DO i=lis1,li1
        w(i,ljs)=p25*(f(i+1,ljs)+f(i-1,ljs)+c2*f(i,ljs))
        w(i,lj )=p25*(f(i+1,lj )+f(i-1,lj )+c2*f(i,lj ))
      END DO
!
!  Smooth first interior points with 9-point (1-2-1) smoother.
!
      DO j=ljs1,lj1
        w(lis1,j)=r16*(f(lis2,j+1)+f(lis ,j-1)+f(lis2,j-1)+f(lis,j+1)+  &
     &            c2*(f(lis2,j  )+f(lis1,j+1)+f(lis1,j-1)+f(lis,j  ))+  &
     &            c4*f(lis1,j))
        w(li1 ,j)=r16*(f(li,j+1)+f(li2,j-1)+f(li ,j-1)+f(li2,j+1)+      &
     &            c2*(f(li,j  )+f(li1,j+1)+f(li1,j-1)+f(li2,j  ))+      &
     &            c4*f(li1,j))
      END DO
      DO i=lis1,li1
        w(i,ljs1)=r16*(f(i+1,ljs2)+f(i-1,ljs )+f(i+1,ljs)+f(i-1,ljs2)+  &
     &            c2*(f(i+1,ljs1)+f(i  ,ljs2)+f(i  ,ljs)+f(i-1,ljs1))+  &
     &            c4*f(i,ljs1))
        w(i,lj1 )=r16*(f(i+1,lj )+f(i-1,lj2)+f(i+1,lj2)+f(i-1,lj )+     &
     &            c2*(f(i+1,lj1)+f(i  ,lj )+f(i  ,lj2)+f(i-1,lj1))+     &
     &            c4*f(i,lj1))
      END DO
!
!  Corner points.
!
      w(lis,ljs)=(w(lis1,ljs)+w(lis ,ljs1)+w(lis1,ljs1))/c3
      w(lis,lj )=(w(lis ,lj1)+w(lis1,lj  )+w(lis1,lj1 ))/c3
      w(li ,ljs)=(w(li1 ,ljs)+w(li  ,ljs1)+w(li1 ,ljs1))/c3
      w(li ,lj )=(w(li1 ,lj )+w(li  ,lj1 )+w(li1 ,lj1 ))/c3
!
!  Filter all points within first interior boundary with
!  Bleck's 25-point filter.
!
      DO j=ljs2,lj2
        DO i=lis2,li2
          w(i,j)=0.279372*f(i,j)+0.171943*(f(i-1,j)+f(i,j-1)+           &
     &           f(i+1,j)+f(i,j+1))-0.006918*(f(i-2,j)+f(i,j-2)+        &
     &           f(i+2,j)+f(i,j+2))+0.077458*(f(i-1,j-1)+               &
     &           f(i+1,j+1)+f(i+1,j-1)+f(i-1,j+1))-                     &
     &           0.024693*(f(i-1,j-2)+f(i+1,j-2)+f(i-2,j-1)+            &
     &           f(i+2,j-1)+f(i-2,j+1)+f(i+2,j+1)+f(i-1,j+2)+           &
     &           f(i+1,j+2))-0.01294*(f(i-2,j-2)+f(i+2,j-2)+            &
     &           f(i-2,j+2)+f(i+2,j+2))
        END DO
      END DO
!
!  Load filtered data.
!
      DO j=ljs,lj
        DO i=lis,li
          f(i,j)=w(i,j)
        END DO
      END DO

      RETURN
      END

      SUBROUTINE shapiro (zz,im,m,n,nord)
!
!=======================================================================
!                                                                      !
!  Alternate directions SHAPIRO filter.                                !
!                                                                      !
!  On Input:                                                           !
!                                                                      !
!     ZZ      field to be filtered                                     !
!     IM      first dimension of ZZ in the calling program             !
!     M       number of points in the x-direction                      !
!     N       number of points in the y-direction                      !
!     NORD    order of the Shapiro filter                              !
!                                                                      !
!  On Output:                                                          !
!                                                                      !
!     ZZ      filtered field                                           !
!                                                                      !
!=======================================================================
!
      implicit none
#include "param.h"
!
      integer i, idown, im, iodev, ip, iup, j, kord, m, n, nord
      real c1, c2, fac
      real g(0:NH,0:1), zz(*)
      parameter (c1=1.0, c2=2.0)
!
!=======================================================================
!  Begin executable code.
!=======================================================================
!
      iup=1
      iodev=(nord+1)/2-nord/2
      fac=-c1+c2*FLOAT(iodev)
      fac=fac/c2**(2*nord)
!
!  Filter by rows.
!
      DO j=1,n
        DO i=1,m
          ip=i+(j-1)*im
          g(i,iup)=zz(ip)
        END DO
        DO kord=1,nord
          g(0,iup)=g(2,iup)
          g(m+1,iup)=g(m-1,iup)
          idown=1-iup
          DO i=1,m
            g(i,idown)=-c2*g(i,iup)+g(i+1,iup)+g(i-1,iup)
          END DO
          iup=1-iup
        END DO
        DO i=1,m
          ip=i+(j-1)*im
          zz(ip)=zz(ip)+fac*g(i,iup)
        END DO
      END DO
!
!  Filter by columns.
!
      DO i=1,m
        DO j=1,n
          ip=i+(j-1)*im
          g(j,iup)=zz(ip)
        END DO
        DO kord=1,nord
          g(0,iup)=g(2,iup)
          g(n+1,iup)=g(n-1,iup)
          idown=1-iup
          DO j=1,n
            g(j,idown)=-c2*g(j,iup)+g(j+1,iup)+g(j-1,iup)
          END DO
          iup=1-iup
        END DO
        DO j=1,n
          ip=i+(j-1)*im
          zz(ip)=zz(ip)+fac*g(j,iup)
        END DO
      END DO

      RETURN
      END
