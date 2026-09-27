      SUBROUTINE cfield (fout,im,jm,km,finp,imr,jmr,kmr,igrid)
!
!svn $Id: cfield.F 833 2017-01-24 22:20:16Z arango $
!================================================== Hernan G. Arango ===
!  Copyright (c) 2002-2017 The ROMS/TOMS Group                         !
!    Licensed under a MIT/X style license                              !
!    See License_ROMS.txt                                              !
!=======================================================================
!                                                                      !
!  This routine computes computes field at requested horizontal        !
!  grid positions from the original field at RHO-points.               !
!                                                                      !
!  On Input:                                                           !
!                                                                      !
!     im         Size of X-dimension for "fout".                       !
!     jm         Size of Y-dimension for "fout".                       !
!     km         Size of Z-dimension for "fout".                       !
!     finp       input field at RHO-points.                            !
!     imr        Size of X-dimension for "finp".                       !
!     jmr        Size of Y-dimension for "finp".                       !
!     kmr        Size of Z-dimension for "finp".                       !
!     igrid      Staggered C grid type:                                !
!                 igrid=0  => None.                                    !
!                 igrid=1  => density points.                          !
!                 igrid=2  => streamfunction points.                   !
!                 igrid=3  => u-velocity points.                       !
!                 igrid=4  => v-velocity points.                       !
!                 igrid=5  => w-velocity points.                       !
!                 igrid=6  => w-velocity points, range 0:N.            !
!                 igrid=7  => interior density points.                 !
!                                                                      !
!  On Output:                                                          !
!                                                                      !
!     fout       Field at requested grid.                              !
!                                                                      !
!=======================================================================
!
!-----------------------------------------------------------------------
!  Define global variables.
!-----------------------------------------------------------------------
!
      implicit none
#include "pconst.h"
!
!-----------------------------------------------------------------------
!  Define local variables.
!-----------------------------------------------------------------------
!

      integer i, igrid, im, imr, j, jm, jmr, k, km, kmr
      real finp(imr,jmr,kmr), fout(im,jm,km)
!
!=======================================================================
!  Begin executable code.
!=======================================================================
!
!  Compute field at RHO-points.
!
      IF ((igrid.eq.1).or.(igrid.eq.5)) THEN
        DO k=1,km
          DO j=1,jm
            DO i=1,im
              fout(i,j,k)=finp(i,j,k)
            END DO
          END DO
        END DO
!
!  Compute field at PSI-points.
!
      ELSE IF (igrid.eq.2) THEN
        DO k=1,km
          DO j=1,jm
            DO i=1,im
              IF ((finp(i  ,j  ,k).gt.spval2).or.                       &
     &            (finp(i+1,j  ,k).gt.spval2).or.                       &
     &            (finp(i  ,j+1,k).gt.spval2).or.                       &
     &            (finp(i+1,j+1,k).gt.spval2)) THEN
                fout(i,j,k)=spval1
              ELSE
                fout(i,j,k)=p25*(finp(i,j  ,k)+finp(i+1,j  ,k)+         &
     &                           finp(i,j+1,k)+finp(i+1,j+1,k))
              END IF
            END DO
          END DO
        END DO
!
!  Compute field U-points.
!
      ELSE IF (igrid.eq.3) THEN
        DO k=1,km
          DO j=1,jm
            DO i=1,im
              IF ((finp(i  ,j  ,k).gt.spval2).or.                       &
     &            (finp(i+1,j  ,k).gt.spval2)) THEN
                fout(i,j,k)=spval1
              ELSE
                fout(i,j,k)=p5*(finp(i,j,k)+finp(i+1,j,k))
              END IF
            END DO
          END DO
        END DO
!
!  Compute field at V-points.
!
      ELSE IF (igrid.eq.4) THEN
        DO k=1,km
          DO j=1,jm
            DO i=1,im
              IF ((finp(i  ,j  ,k).gt.spval2).or.                       &
     &            (finp(i  ,j+1,k).gt.spval2)) THEN
                fout(i,j,k)=spval1
              ELSE
                fout(i,j,k)=p5*(finp(i,j,k)+finp(i,j+1,k))
              END IF
            END DO
          END DO
        END DO
!
!  Compute field at interior RHO-points.
!
      ELSE IF (igrid.eq.7) THEN
        DO k=1,km
          DO j=1,jm
            DO i=1,im
              fout(i,j,k)=finp(i+1,j+1,k)
            END DO
          END DO
        END DO
      END IF

      RETURN
      END
