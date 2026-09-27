      SUBROUTINE bv_freq (bvf,t,s,z_w,im,jm,km)
!
!svn $Id: bv_freq.F 833 2017-01-24 22:20:16Z arango $
!================================================== Hernan G. Arango ===
!  Copyright (c) 2002-2017 The ROMS/TOMS Group                         !
!    Licensed under a MIT/X style license                              !
!    See License_ROMS.txt                                              !
!=======================================================================
!                                                                      !
!  This subroutine computes the Brunt-Vaisala frequency  (1/s^2) at    !
!  horizontal RHO-points and vertical W-points,                        !
!                                                                      !
!                   bvf = - g/rho0 d(rho)/d(z).                        !
!                                                                      !
!  The potential density difference is computed lowering/rising the    !
!  water parcel above/below adiabatically at W-point depth "z_w".      !
!                                                                      !
!  On Output:                                                          !
!                                                                      !
!     bvf      Brunt-Vaisala frequency (1/s^2).                        !
!                                                                      !
!  On Input:                                                           !
!                                                                      !
!     t        temperature (C).                                        !
!     s        salinity (PSU).                                         !
!     z_w      Depth (m) at W-points.                                  !
!     im       Number of points in the XI-direction.                   !
!     jm       Number of points in the ETA-direction.                  !
!     km       Number of points in the S-direction.                    !
!                                                                      !
!  Reference:                                                          !
!                                                                      !
! << This equation of state formulation has been derived by Jackett    !
!    and McDougall (1992), unpublished manuscript, CSIRO, Australia.   !
!    It computes in-situ density anomaly as a function of potential    !
!    temperature (Celsius) relative to the surface, salinity (PSU),    !
!    and depth (meters).  It assumes  no  pressure  variation along    !
!    geopotential  surfaces,  that  is,  depth  and  pressure  are     !
!    interchangeable. >>                                               !
!                                          John Wilkin, 29 July 92     !
!                                                                      !
!=======================================================================
!
!-----------------------------------------------------------------------
!  Define global variables.
!-----------------------------------------------------------------------
!
      implicit none
#include "domdat.h"
!
!-----------------------------------------------------------------------
!  Define local variables.
!-----------------------------------------------------------------------
!
      integer i, im, j, jm, k, km
      real A0, A1, A2, A3, A4, B0, B1, B2, B3, D0, D1, D2, E0, E1, E2,  &
     &     E3, F0, F1, F2, G0, G1, G2, G3, H0, H1, H2, Q0, Q1, Q3, Q4,  &
     &     Q5, Q6, U0, U1, U2, U3, U4, V0, V1, V2, W0
      real Tt, Ts, Tz, bulk, g, gorho0, rhodn, rhoup, zup, zdn
      real bvf(0:im,0:jm,km-1), t(0:im,0:jm,km), s(0:im,0:jm,km),       &
     &     z_w(0:im,0:jm,0:km)
      parameter (A0=+19092.56   , A1=+209.8925   , A2=+3.041638   ,     &
     &           A3=-1.852732e-3, A4=+1.361629e-5, B0=+104.4077   ,     &
     &           B1=+6.500517   , B2=+0.1553190  , B3=-2.326469e-4,     &
     &           D0=-5.587545   , D1=+0.7390729  , D2=+1.909078e-2,     &
     &           E0=+4.721788e-1, E1=+1.028859e-2, E2=-2.512549e-4,     &
     &           E3=+5.939910e-7, F0=-1.571896e-2, F1=+2.598241e-4,     &
     &           F2=-7.267926e-6, G0=+2.042967e-3, G1=+1.045941e-5,     &
     &           G2=5.782165e-10, G3=+1.296821e-7, H0=-2.595994e-7,     &
     &           H1=-1.248266e-9, H2=-3.508914e-9)
      parameter (Q0=+999.842594 , Q1=+6.793952e-2, Q3=-9.095290e-3,     &
     &           Q4=+1.001685e-4, Q5=-1.120083e-6, Q6=+6.536332e-9,     &
     &           U0=+0.824493   , U1=-4.08990e-3 , U2=+7.64380e-5 ,     &
     &           U3=-8.24670e-7 , U4=+5.38750e-9 , V0=-5.72466e-3 ,     &
     &           V1=+1.02270e-4 , V2=-1.65460e-6 , W0=+4.8314e-4)
      parameter (g=9.81)
!
!=======================================================================
!  Begin executable code.
!=======================================================================
!
      gorho0=g/rho0
      DO k=1,km-1
        DO j=0,jm
          DO i=0,im
            Tt=t(i,j,k+1)
            Ts=s(i,j,k+1)
            Tz=z_w(i,j,k)
!
!-----------------------------------------------------------------------
!  Compute potential density at "z_w" by lowering water parcel above
!  (K+1) adiabatically.
!-----------------------------------------------------------------------
!
!  Compute secant bulk modulus.
!
            bulk=A0+Tt*(A1-Tt*(A2-Tt*(A3-Tt*A4)))+                      &
     &              Ts*(B0-Tt*(B1-Tt*(B2-Tt*B3)))+                      &
     &              SQRT(Ts*Ts*Ts)*(D0+Tt*(D1-Tt*D2))-                  &
     &              Tz*(E0+Tt*(E1+Tt*(E2-Tt*E3)))-                      &
     &              Tz*Ts*(F0-Tt*(F1+Tt*F2))-                           &
     &              Tz*SQRT(Ts*Ts*Ts)*G0+Tz*Tz*(G1-Tt*(G2-Tt*G3))+      &
     &              Tz*Tz*Ts*(H0+Tt*(H1+Tt*H2))
!
!  Compute density anomaly (kg/m^3).
!
            rhoup=(Q0+Tt*(Q1+Tt*(Q3+Tt*(Q4+Tt*(Q5+Tt*Q6))))+            &
     &                Ts*(U0+Tt*(U1+Tt*(U2+Tt*(U3+Tt*U4))))+            &
     &                SQRT(Ts*Ts*Ts)*(V0+Tt*(V1+Tt*V2))+                &
     &                W0*Ts*Ts) / (1.0+0.1*Tz/bulk) - rho0
!
!-----------------------------------------------------------------------
!  Compute potential density at "z_w" by rising water parcel above
!  (K) adiabatically.
!-----------------------------------------------------------------------
!
            Tt=t(i,j,k)
            Ts=s(i,j,k)
            Tz=z_w(i,j,k)
!
!  Compute secant bulk modulus.
!
            bulk=A0+Tt*(A1-Tt*(A2-Tt*(A3-Tt*A4)))+                      &
     &              Ts*(B0-Tt*(B1-Tt*(B2-Tt*B3)))+                      &
     &              SQRT(Ts*Ts*Ts)*(D0+Tt*(D1-Tt*D2))-                  &
     &              Tz*(E0+Tt*(E1+Tt*(E2-Tt*E3)))-                      &
     &              Tz*Ts*(F0-Tt*(F1+Tt*F2))-                           &
     &              Tz*SQRT(Ts*Ts*Ts)*G0+Tz*Tz*(G1-Tt*(G2-Tt*G3))+      &
     &              Tz*Tz*Ts*(H0+Tt*(H1+Tt*H2))
!
!  Compute density anomaly (kg/m^3).
!
            rhodn=(Q0+Tt*(Q1+Tt*(Q3+Tt*(Q4+Tt*(Q5+Tt*Q6))))+            &
     &                Ts*(U0+Tt*(U1+Tt*(U2+Tt*(U3+Tt*U4))))+            &
     &                SQRT(Ts*Ts*Ts)*(V0+Tt*(V1+Tt*V2))+                &
     &                W0*Ts*Ts) / (1.0+0.1*Tz/bulk) - rho0
!
!-----------------------------------------------------------------------
!  Compute Brunt-Vaisala frequency (1/s^2).
!-----------------------------------------------------------------------
!
            zup=0.5*(z_w(i,j,k+1)+z_w(i,j,k))
            zdn=0.5*(z_w(i,j,k-1)+z_w(i,j,k))
            bvf(i,j,k)=-gorho0*(rhoup-rhodn)/(zup-zdn)
          END DO
        END DO
      END DO

      RETURN
      END
