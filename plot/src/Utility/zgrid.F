      SUBROUTINE zgrid (zpos,im,jm,km,z,iz,jz,kz,igrid)
!
!svn $Id: zgrid.F 833 2017-01-24 22:20:16Z arango $
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
!     im       Size of XI-dimension for "zpos".                        !
!     jm       Size of ETA-dimension for "zpos".                       !
!     km       Size of S-dimension for "zpos".                         !
!     z        3D depths at horizontal RHO-points.                     !
!     iz       Size of XI-dimension for "z".                           !
!     jz       Size of ETA-dimension for "z".                          !
!     kz       Size of S-dimension for "z".                            !
!     igrid    Staggered C grid type:                                  !
!              igrid=0  => None.                                       !
!              igrid=1  => density points.                             !
!              igrid=2  => streamfunction points.                      !
!              igrid=3  => u-velocity points.                          !
!              igrid=4  => v-velocity points.                          !
!              igrid=5  => w-velocity points.                          !
!              igrid=6  => w-velocity points, range 0:N.               !
!              igrid=7  => interior density points.                    !
!                                                                      !
!  On Output:                                                          !
!                                                                      !
!     zpos     Depths of current field.                                !
!                                                                      !
!=======================================================================
!
!-----------------------------------------------------------------------
!  Define local variables.
!-----------------------------------------------------------------------
!
      implicit none
      integer i, igrid, im, iz, j, jm, jz, k, km, kz
      real p5, p25
      real z(iz,jz,kz), zpos(im,jm,km)
      parameter (p5=0.5, p25=0.25)
!
!=======================================================================
!  Begin executable code.
!=======================================================================
!
!  Compute depths at density points.
!
      IF ((igrid.eq.1).or.(igrid.eq.5).or.(igrid.eq.6).or.              &
     &    (igrid.eq.7)) THEN
        DO k=1,km
          DO j=1,jm
            DO i=1,im
              zpos(i,j,k)=z(i,j,k)
            END DO
          END DO
        END DO
!
!  Compute depths at streamfunction points.
!
      ELSE IF (igrid.eq.2) THEN
        DO k=1,km
          DO j=1,jm
            DO i=1,im
              zpos(i,j,k)=p25*(z(i,j  ,k)+z(i+1,j  ,k)+                 &
     &                         z(i,j+1,k)+z(i+1,j+1,k))
            END DO
          END DO
        END DO
!
!  Compute horizontal grid positions at U-velocity points.
!
      ELSE IF (igrid.eq.3) THEN
        DO k=1,km
          DO j=1,jm
            DO i=1,im
              zpos(i,j,k)=p5*(z(i,j,k)+z(i+1,j,k))
            END DO
          END DO
        END DO
!
!  Compute horizontal grid positions at V-velocity points.
!
      ELSE IF (igrid.eq.4) THEN
        DO k=1,km
          DO j=1,jm
            DO i=1,im
              zpos(i,j,k)=p5*(z(i,j,k)+z(i,j+1,k))
            END DO
          END DO
        END DO
      END IF

      RETURN
      END
