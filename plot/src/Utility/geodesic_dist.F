      SUBROUTINE geodesic_dist (lon1,lat1,lon2,lat2,flag,gdist,galpha)
!
!svn $Id: geodesic_dist.F 833 2017-01-24 22:20:16Z arango $
!================================================== Hernan G. Arango ===
!  Copyright (c) 2002-2017 The ROMS/TOMS Group                         !
!    Licensed under a MIT/X style license                              !
!    See License_ROMS.txt                                              !
!=======================================================================
!                                                                      !
!  Inverse, non-iterative solutions for distance and geodesic azimuth  !
!  between two points on the ellipsoid (The Earth) from the equations, !
!  second order in spheroidal flatttening, given by:                   !
!                                                                      !
!  Sodano , E.M., and T. Robinson, 1963: Direct and inverse solutions  !
!   of geodesics, Army Map Service Technical Report No. 7, AD 657591.  !
!                                                                      !
!  On input:    Longitude is positive to the east and negative to the  !
!               west.  Latitude is positive to the north and negative  !
!               to the south.                                          !
!                                                                      !
!     LON1    Longitude point 1 (decimal degrees, real*8)              !
!     LAT1    Latitude  point 1 (decimal degrees, real*8)              !
!     LON2    Longitude point 2 (decimal degrees, real*8)              !
!     LAT2    Latitude  point 2 (decimal degrees, real*8)              !
!     FLAG    flag for distance units on output (integer)              !
!                                                                      !
!  On output:                                                          !
!                                                                      !
!     GALPHA  Geodesic azimuth from point 1 to point 2 clockwise from  !
!             North (decimal degrees, real*8)                          !
!     GDIST   Geodesic distance between point 1 and point 2 (real*8)   !
!                                                                      !
!             Units of distance                                        !
!                                                                      !
!               Flag    Units                                          !
!              ------  -------                                         !
!                 1     Meters                                         !
!                 2     Nautical Miles                                 !
!                 3     Feet                                           !
!                 4     Kilometers                                     !
!                 5     Statute Mile                                   !
!                                                                      !
!=======================================================================
!
!-----------------------------------------------------------------------
!  Define local data
!-----------------------------------------------------------------------
!
      implicit none
      logical first
      integer flag
      real galpha, gdist, lat1, lat2, lon1, lon2
      real a, adist, alpha, b, beta1, beta2, c, cott, ct, delta, dist,  &
     &     d_lat1, d_lat2, d_lon1, d_lon2, f, l, lambda, m, q, r_lat1,  &
     &     r_lat2, smaj, smin, sob, st, t, w, x, y, z
      real c0, c1, c4, c5, c90, c180, c360, deg2rad, pi, p0625, p5,     &
     &       p25, p125, rad2deg
      parameter (c0=0.d0, c1=1.0d0, c4=4.0d0, c5=5.d0, c90=90.0d0,      &
     &           c180=180.0d0, c360=360.0d0, p0625=0.0625d0, p5=0.5d0,  &
     &           p25=0.25d0, p125=0.125d0)
      save deg2rad, f, pi, rad2deg, smaj, smin
      data first /.true./
!
!=======================================================================
!  Begin executable code.
!=======================================================================
!
!  Define parameters on first pass (SMIN: Ellipsoid semi-minor axis in
!  meters; SMAJ: Ellipsoid semi-major axis in meters; F: spheroidal
!  flattening).
!
      IF (first) THEN
        smin=6356750.52d0
        smaj=6378135.0d0
        f=c1-(smin/smaj)
        pi=c4*ATAN(c1)
        deg2rad=pi/c180
        rad2deg=c180/pi
        first=.false.
      END IF
!
!  RETURN if zero distance.
!
      IF ((lon1.eq.lon2).and.(lat1.eq.lat2)) THEN
        gdist=c0
        galpha=c0
        RETURN
      END IF
!
!  Convert input data to double precision.
!
      d_lon1=lon1
      d_lat1=lat1
      d_lon2=lon2
      d_lat2=lat2
!
!  Determine proper longitudinal shift.
!
      delta=d_lon2-d_lon1
      l=ABS(delta)
      IF (l.ge.c180) l=c360-ABS(d_lon1-d_lon2)
!
!  Convert Decimal degrees to radians.
!
      r_lat1=d_lat1*deg2rad
      r_lat2=d_lat2*deg2rad
      l=l*deg2rad
!
!  Calculate S/Bo subformulas.
!
      beta1=ATAN(TAN(r_lat1)*(c1-f))
      beta2=ATAN(TAN(r_lat2)*(c1-f))
      a=SIN(beta1)*SIN(beta2)
      b=COS(beta1)*COS(beta2)
      ct=a+b*COS(l)
      st=SQRT(((SIN(l)*COS(beta2))**2)+(((SIN(beta2)*COS(beta1))-       &
     &         (SIN(beta1)*COS(beta2)*COS(l)))**2))
      t=ASIN(st)
      c=(b*SIN(l))/st
      m=c1-(c*c)
!
!  Calculate S/Bo term.
!
      q=f+(f*f)
      z=f*f*p5
      x=f*f*p0625
      y=f*f*p125
      w=f*f*p25
!
      sob=((c1+q)*t)+(a*((q*st)-(z*(t*t)*(c1/SIN(t)))))+                &
     &    (m*(((-q*p5)*t)-((q*p5)*st*ct)+(z*(t*t)*(c1/TAN(t)))))+       &
     &    ((a**2)*(-z*st*ct))+                                          &
     &    ((m**2)*((x*t)+(x*st*ct)-(z*(t*t)*(c1/TAN(t)))-               &
     &     (y*st*(ct*ct*ct))))+                                         &
     &    ((a*m)*((z*(t*t)*(c1/SIN(t)))+(z*st*(ct*ct))))
!
!  Compute geodesic azimuth from point 1 to point 2 clockwise from
!  North, alpha.
!
      lambda=q*t+a*(-z*st-f*f*t*t/SIN(t))+                              &
     &       m*(-c5*w*t+w*st*COS(t)+f*f*t*t/TAN(t))
      lambda=c*lambda+l
      IF (lambda.eq.c0) THEN
        IF (d_lat1.lt.d_lat2) alpha=c0
        IF (d_lat1.gt.d_lat2) alpha=c180
        GO TO 10
      END IF
!
      cott=(SIN(beta2)*COS(beta1)-COS(lambda)*SIN(beta1)*               &
     &                            COS(beta2))/                          &
     &     (SIN(lambda)*COS(beta2))
      IF (cott.eq.c0) THEN
        alpha=c90
      ELSE
        alpha=ATAN(c1/cott)*rad2deg
      END IF
!
!  Compute heading from point 1 to point 2 clockwise from north.
!
      IF (delta.gt.c0) THEN
        IF (cott.gt.c0) THEN
          alpha=alpha
        ELSE IF (cott.lt.c0) THEN
          alpha=c180+alpha
        END IF
      END IF
      IF (delta.lt.c0) THEN
        IF (cott.lt.c0) THEN
          alpha=c180-alpha
        ELSE IF (cott.gt.c0) THEN
          alpha=c360-alpha
        END IF
      END IF
!
!  Calculate distance from point 1 to point 2.
!
  10  adist=sob*smin
!
!  Check flag for proper output units.
!
      IF (flag.eq.1) dist=adist                   ! meters
      IF (flag.eq.2) dist=adist*5.396d-4          ! nautical miles
      IF (flag.eq.3) dist=adist*3.281d0           ! feed
      IF (flag.eq.4) dist=adist*1.d-3             ! kilometers
      IF (flag.eq.5) dist=adist*6.214d-4          ! statute mile
!
!  convert to single precision.
!
      gdist=dist
      galpha=alpha

      RETURN
      END
