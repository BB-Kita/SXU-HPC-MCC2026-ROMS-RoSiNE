      FUNCTION vindices (f,km,spv,kstr,kend)
!
!svn $Id: vindices.F 833 2017-01-24 22:20:16Z arango $
!================================================== Hernan G. Arango ===
!  Copyright (c) 2002-2017 The ROMS/TOMS Group                         !
!    Licensed under a MIT/X style license                              !
!    See License_ROMS.txt                                              !
!=======================================================================
!                                                                      !
!  This routine checks the contents of profile  F(z) and extracts      !
!  the starting and ending indices in such a way that masked data      !
!  will excluded at either end.  This routine is used in the high      !
!  order vertical interpolation of cross-sections.                     !
!                                                                      !
!  On Input                                                            !
!                                                                      !
!     f            Field vertical profile (real array).                !
!     km           Number of points in F (integer).                    !
!     spv          Field special value used during checking (real).    !
!                                                                      !
!  On Output                                                           !
!                                                                      !
!     kstr         Starting index (integer).                           !
!     kend         Ending index (integer).                             !
!     vindices     check status (integer):                             !
!                    vindices=0  successful extraction.                !
!                    vindices=1  all profile is masked.                !
!                    vindices=2  only one or two points are viable.    !
!                                                                      !
!=======================================================================
!
!-----------------------------------------------------------------------
!  Define local data.
!-----------------------------------------------------------------------
!
      implicit none
      integer k, kend, km, kstr
      integer vindices
      real f(km), spv
!
!=======================================================================
!  Begin executable code.
!=======================================================================
!
      vindices=1
      kstr=km
      kend=km
!
!  Get starting index.
!
      DO k=1,km
        IF (f(k).lt.spv) THEN
          kstr=k
          GO TO 10
        END IF
      END DO
  10  IF (kstr.eq.km) RETURN
!
!  Get ending index.
!
      DO k=km,kstr+1,-1
        IF (f(k).gt.spv) THEN
          kend=k-1
        END IF
      END DO
!
!  Check extracted indices.
!
      IF ((kend-kstr).le.1) THEN
        vindices=2
      ELSE
        vindices=0
      END IF

      RETURN
      END
