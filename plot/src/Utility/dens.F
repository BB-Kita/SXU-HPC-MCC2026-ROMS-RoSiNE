      FUNCTION idens (t,s,p)
!
!svn $Id: dens.F 833 2017-01-24 22:20:16Z arango $
!================================================== Hernan G. Arango ===
!  Copyright (c) 2002-2017 The ROMS/TOMS Group                         !
!    Licensed under a MIT/X style license                              !
!    See License_ROMS.txt                                              !
!=======================================================================
!                                                                      !
!  This routine computes "in situ" density via equation of state for   !
!  seawater.                                                           !
!                                                                      !
!  On Input:                                                           !
!                                                                      !
!     t         potential temperature (Celsius).                       !
!     s         salinity (PSU).                                        !
!     p         pressure/depth, (depth in meters and negative).        !
!                                                                      !
!  On Output:                                                          !
!                                                                      !
!     idens     "in situ" density (kg/m^3).                            !
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
!  Define local variables.
!-----------------------------------------------------------------------
!
      implicit none
      real A0, A1, A2, A3, A4, B0, B1, B2, B3, D0, D1, D2, E0, E1, E2,  &
     &     E3, F0, F1, F2, G0, G1, G2, G3, H0, H1, H2, Q0, Q1, Q3, Q4,  &
     &     Q5, Q6, U0, U1, U2, U3, U4, V0, V1, V2, W0
      real bulk, p, s, t
      real idens
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
!
!=======================================================================
!  Begin executable code.
!=======================================================================
!
!-----------------------------------------------------------------------
!  Non-linear equation of state, Jackett and McDougall (1992).
!-----------------------------------------------------------------------
!
!  Compute secant bulk modulus and store into a utility work array.
!
      bulk=A0+t*(A1-t*(A2-t*(A3-t*A4)))+                                &
     &        s*(B0-t*(B1-t*(B2-t*B3)))+                                &
     &        SQRT(s*s*s)*(D0+t*(D1-t*D2))-                             &
     &        p*(E0+t*(E1+t*(E2-t*E3)))-                                &
     &        p*s*(F0-t*(F1+t*F2))-                                     &
     &        p*SQRT(s*s*s)*G0+p*p*(G1-t*(G2-t*G3))+                    &
     &        p*p*s*(H0+t*(H1+t*H2))
!
!  Compute "in situ" density anomaly (kg/m^3).
!
      idens=(Q0+t*(Q1+t*(Q3+t*(Q4+t*(Q5+t*Q6))))+                       &
     &          s*(U0+t*(U1+t*(U2+t*(U3+t*U4))))+                       &
     &          SQRT(s*s*s)*(V0+t*(V1+ t*V2))+W0*s*s) /                 &
     &      (1.0+0.1*p/bulk) - 1000.0
      RETURN
      END

      FUNCTION pdens (t,s,z)
!
!=======================================================================
!                                                                      !
!  This routine computes density anomaly based on 1980 Equation of     !
!  State for Seawater and 1978 Practical Salinity Scale.  If input     !
!  potential temperature, it returns potential density anomaly. It     !
!  assumes no pressure variation along geopotential surfaces, that     !
!  is,  depth (meters; positive) and pressure (decibars; positive)     !
!  are interchangeable.                                                !
!                                                                      !
!  On Input:                                                           !
!                                                                      !
!     t        Temperature (Celsius).                                  !
!     s        Salinity (PSU).                                         !
!     z        pressure (decibars).                                    !
!                                                                      !
!  On Output:                                                          !
!                                                                      !
!     pdens    density anomaly (kg/m^3).                               !
!                                                                      !
!  Check Values: (T=3 C, S=35.5, Z=-3000 m)                            !
!                                                                      !
!     svan  =   5.90124 e-8 m^3/kg                                     !
!     sigma =  41.87663 kg/m^3                                         !
!                                                                      !
!  References:                                                         !
!                                                                      !
!     Millero, et al., 1980: A new high-pressure equation of state     !
!             for seawater. Deep-Sea Res., 27a, 255-264.               !
!     Millero and Poisson, 1981: Summary of data treatment for the     !
!             Unesco one atmosphere equation of state for seawater     !
!             Deep-Sea Res., 28a, 625-629.                             !
!                                                                      !
!=======================================================================
!
!-----------------------------------------------------------------------
!  Define local data.
!-----------------------------------------------------------------------
!
      implicit none
      real Tp, Ts, Tt, a, aw, a1, b, bw, b1, c, c1, c10, d, d350, dk,   &
     &     dr35p, dr350, dvan, e, gam, kw, k0, k35, pk, r01, r02, r03,  &
     &     r04, r3500, s, sig, sigma, t, srTs, sva, v350p, z
      real pdens
      data dr350, r3500, r04 /28.106331, 1028.1063, 4.8314e-4/
      parameter (c1=1.0, c10=10.0)
!
!=======================================================================
!  Begin executable code.
!=======================================================================
!
      Tt=t
      Ts=s
      Tp=ABS(z)/c10
      srTs=SQRT(ABS(Ts))
!
!  Density of the reference pure water (Bigg, P.H., 1967, Br. J.
!  Applied Physics, 8, 521-537)
!
      r01=((((6.536332e-9*Tt-1.120083e-6)*Tt+1.001685e-4)*Tt-           &
     &    9.095290e-3)*Tt+6.793952e-2)*Tt-28.263737
!
!  Seawater density at atmospheric pressure.
!
      r02=(((5.3875e-9*Tt-8.2467e-7)*Tt+7.6438e-5)*Tt-4.0899e-3)*Tt+    &
     &    8.24493e-1
      r03=(-1.6546e-6*Tt+1.0227e-4)*Tt-5.72466e-3
      sig=(r04*Ts+r03*srTs+r02)*Ts+r01
!
!  Specific volume at atmospheric pressure.
!
      v350p=c1/r3500
      sva=-sig*v350p/(r3500+sig)
      sigma=dr350-(sva/(v350p*(v350p+sva)))
!
!  Compute compression terms.
!
      IF (Tp.ne.0.0) THEN
        e=(9.1697e-10*Tt+2.0816e-8)*Tt-9.9348e-7
        bw=(5.2787e-8*Tt-6.12293e-6)*Tt+3.47718e-5
        b=bw+e*Ts
!
        d=1.91075e-4
        c=(-1.6078e-6*Tt-1.0981e-5)*Tt+2.2838e-3
        aw=((-5.77905e-7*Tt+1.16092e-4)*Tt+1.43713e-3)*Tt-0.1194975
        a=(d*srTs+c)*Ts+aw
!
        b1=(-5.3009e-4*Tt+1.6483e-2)*Tt+7.944e-2
        a1=((-6.1670e-5*Tt+1.09987e-2)*Tt-0.603459)*Tt+54.6746
        kw=(((-5.155288e-5*Tt+1.360477e-2)*Tt-2.327105)*Tt+             &
     &     148.4206)*Tt-1930.06
        k0=(b1*srTs+a1)*Ts+kw
!
!  Evaluate pressure polynomial:
!
!   K is the secant bulk modulus of seawater
!   DK = K(s,t,p) - K(35,0,p)
!   K35 = K(35,0,p)
!
        dk=(b*Tp+a)*Tp+k0
        k35=(5.03217e-5*Tp+3.359406)*Tp+21582.27
        gam=Tp/k35
        pk=c1-gam
        sva=sva*pk+(v350p+sva)*Tp*dk/(k35*(k35+dk))
!
!-----------------------------------------------------------------------
!  Compute density anamoly with respect to 1000.0 kg/m**3:
!
!   1) dr350: density anamoly at 35 PSU, 0 deg. C, atmospheric pressure
!   2) dr35p: density anamoly 35 PSU, 0 deg. C, with pressure variation
!   3) dvan : density anamoly variations involving specific vol. anamoly
!-----------------------------------------------------------------------
!
        v350p=v350p*pk
        d350=gam/pk
        dr35p=r3500*d350
        dvan=sva/(v350p*(v350p+sva))
        sigma=dr350+dr35p-dvan
      END IF
      pdens=sigma

      RETURN
      END
