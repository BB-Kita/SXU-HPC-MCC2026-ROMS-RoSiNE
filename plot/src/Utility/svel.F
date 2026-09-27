      FUNCTION svel(s,t,p0)
!
!svn $Id: svel.F 833 2017-01-24 22:20:16Z arango $
!================================================== Hernan G. Arango ===
!  Copyright (c) 2002-2017 The ROMS/TOMS Group                         !
!    Licensed under a MIT/X style license                              !
!    See License_ROMS.txt                                              !
!=======================================================================
!                                                                      !
!  This routine computes sound speed in seawater using formula         !
!  developed by Chen and Millero (1977).                               !
!                                                                      !
!  On Input:                                                           !
!                                                                      !
!     P0       pressure (real; decibars)                               !
!     T        temperature (real; degree Celsius IPTS-68)              !
!     S        salinity (real; PSU, IPSS-78)                           !
!                                                                      !
!  On Output:                                                          !
!                                                                      !
!     SVEL     sound speed (real; meters/second)                       !
!                                                                      !
!  Check value:                                                        !
!                                                                      !
!     svel = 1731.995 m/s                                              !
!                                                                      !
!            for  s=40 (PSS-78), t=40 C, p=10000 decibars              !
!                                                                      !
!  Reference:                                                          !
!                                                                      !
!     Chen, C.T. and F.J. Millero, 1976: re-evaluation of Willson      !
!           sound-speed measurements in pure water. J. Acoust. Soc.    !
!           of Amer., 60(6), 1270-1273.                                !
!                                                                      !
!=======================================================================
!
!-----------------------------------------------------------------------
!  Define local data.
!-----------------------------------------------------------------------
!
      implicit none
      real a, a0, a1, a2, a3, b, b0, b1, c, c0, c1, c2, c3, d, p, p0,   &
     &     s, sr, t
      real svel
!
!=======================================================================
!  Begin executable code.
!=======================================================================
!
!  Scale pressure to bars and take square root salinity.
!
      p=p0/10.
      sr=SQRT(ABS(s))
!
!  S**2 term.
!
      d=1.727e-3-7.9836e-6*p
!
!  S**3/2 term.
!
      b1=7.3637e-5+1.7945e-7*t
      b0=-1.922e-2-4.42e-5*t
      b=b0+b1*p
!
!  S**1 term.
!
      a3=(-3.389e-13*t+6.649e-12)*t+1.100e-10
      a2=((7.988e-12*t-1.6002e-10)*t+9.1041e-9)*t-3.9064e-7
      a1=(((-2.0122e-10*t+1.0507e-8)*t-6.4885e-8)*t-1.2580e-5)*t        &
     &   +9.4742e-5
      a0=(((-3.21e-8*t+2.006e-6)*t+7.164e-5)*t-1.262e-2)*t              &
     &   +1.389
      a=((a3*p+a2)*p+a1)*p+a0
!
!  S**0 term.
!
      c3=(-2.3643e-12*t+3.8504e-10)*t-9.7729e-9
      c2=(((1.0405e-12*t-2.5335e-10)*t+2.5974e-8)*t-1.7107e-6)*t        &
     &   +3.1260e-5
      c1=(((-6.1185e-10*t+1.3621e-7)*t-8.1788e-6)*t+6.8982e-4)*t        &
     &   +0.153563
      c0=((((3.1464e-9*t-1.47800e-6)*t+3.3420e-4)*t-5.80852e-2)*t       &
     &   +5.03711)*t+1402.388
      c=((c3*p+c2)*p+c1)*p+c0
!
!  Sound speed.
!
      svel=c+(a+b*sr+d*s)*s

      RETURN
      END
