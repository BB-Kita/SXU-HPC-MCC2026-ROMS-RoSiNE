      SUBROUTINE depth_iso (d,fval,f,z,im,jm,km,spval)
!
!svn $Id: depth_iso.F 833 2017-01-24 22:20:16Z arango $
!================================================== Hernan G. Arango ===
!  Copyright (c) 2002-2017 The ROMS/TOMS Group                         !
!    Licensed under a MIT/X style license                              !
!    See License_ROMS.txt                                              !
!=======================================================================
!                                                                      !
!  This routine computes the depth of requested isosurface.            !
!                                                                      !
!  On Output:                                                          !
!                                                                      !
!     d        Depths of isosurface (m).                               !
!                                                                      !
!  On Input:                                                           !
!                                                                      !
!     fval     Isosurface value (real scalar).                         !
!     f        Field to process (real array).                          !
!     z        Depths of field (real array; m).                        !
!     im       Number of points in the XI-direction.                   !
!     jm       Number of points in the ETA-direction.                  !
!     km       Number of points in the S-direction.                    !
!     spval    Special value for missing data.                         !
!                                                                      !
!=======================================================================
!
!-----------------------------------------------------------------------
!  Define local variables.
!-----------------------------------------------------------------------
!
      integer i, im, j, jm, k, km
      real c1, delta, fac1, fac2, fval, spval
      real d(im,jm), f(im,jm,km), z(im,jm,km)
      parameter (c1=1.0)
!
!=======================================================================
!  Begin executable code.
!=======================================================================
!
!  Initialize depths with special values.
!
      DO j=1,jm
        DO i=1,im
          d(i,j)=spval
        END DO
      END DO
!
!  Find depths of isosurfave via interpolation starting from the bottom.
!
      DO j=1,jm
        DO i=1,im
          DO k=1,km-1
            IF ((f(i,j,k).ge.fval).and.(fval.gt.f(i,j,k+1))) THEN
              delta=c1/(f(i,j,k)-f(i,j,k+1))
              fac1=(fval-f(i,j,k+1))*delta
              fac2=(f(i,j,k)-fval)*delta
              d(i,j)=fac1*z(i,j,k)+fac2*z(i,j,k+1)
              GO TO 10
            ELSE IF ((f(i,j,k).lt.fval).and.(fval.le.f(i,j,k+1))) THEN
              delta=c1/(f(i,j,k+1)-f(i,j,k))
              fac1=(f(i,j,k+1)-fval)*delta
              fac2=(fval-f(i,j,k))*delta
              d(i,j)=fac1*z(i,j,k)+fac2*z(i,j,k+1)
              GO TO 10
            END IF
          END DO
  10      CONTINUE
        END DO
      END DO

      RETURN
      END
