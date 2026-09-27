      SUBROUTINE day_code(month,day,year,code)
!
!svn $Id: day_code.F 833 2017-01-24 22:20:16Z arango $
!================================================== Hernan G. Arango ===
!  Copyright (c) 2002-2017 The ROMS/TOMS Group                         !
!    Licensed under a MIT/X style license                              !
!    See License_ROMS.txt                                              !
!=======================================================================
!                                                                      !
!  This subroutine computes a code for the day of the week, given      !
!  the date. This code is good for date after:                         !
!                                                                      !
!                              January 1, 1752 AD                      !
!                                                                      !
!  the year the Gregorian calander was adopted in Britian and the      !
!  American colonies.                                                  !
!                                                                      !
!  On Input:                                                           !
!                                                                      !
!     DAY       The day of the month (integer).                        !
!     MONTH     The month, 1=January, 2=February, ... (integer).       !
!     YEAR      The year, including the century (integer).             !
!                                                                      !
!  On Output:                                                          !
!                                                                      !
!     CODE      A code for the corresponding day of the week           !
!               (integer):                                             !
!               CODE = 0  =>  Sunday                                   !
!               CODE = 1  =>  Monday                                   !
!               CODE = 2  =>  Tuesday                                  !
!               CODE = 3  =>  Wednesday                                !
!               CODE = 4  =>  Thursday                                 !
!               CODE = 5  =>  Friday                                   !
!               CODE = 6  =>  Saturday                                 !
!                                                                      !
!     CALLs:  none                                                     !
!                                                                      !
!=======================================================================
!
!-----------------------------------------------------------------------
!  Define local data.
!-----------------------------------------------------------------------
!
      implicit none
      logical leap_flag
      integer base_cen,base_qcen,base_qyear,base_year,bym1_dec31,code,  &
     &        day,feb_end,leap,month,n,no_day,no_yr,nqy,nyc,nyqc,year
      integer month_day(12)
      parameter (base_cen=1700,base_qcen=1600,base_qyear=1748,          &
     &           base_year=1752,bym1_dec31=5,feb_end=59)
      data month_day /31,28,31,30,31,30,31,31,30,31,30,31/
!
!=======================================================================
!  Begin executable code.
!=======================================================================
!
!-----------------------------------------------------------------------
!  Compute the number of years since the base year, the number of
!  years since the beginning of the base century and the number of
!  years since the beginning of the base 400 year.
!-----------------------------------------------------------------------
!
      no_yr=year-base_year
      nqy=year-base_qyear
      nyc=year-base_cen
      nyqc=year-base_qcen
!
!-----------------------------------------------------------------------
!  Compute the number of leapdays in that time.  Determine if this
!  is a leap year.
!-----------------------------------------------------------------------
!
      leap=nqy/4-nyc/100+nyqc/400
      leap_flag=((MOD(nqy,4).eq.0).and.(MOD(nyc,100).ne.0)).or.         &
     &           (MOD(nyqc,400).eq.0)
!
!-----------------------------------------------------------------------
!  Compute the number of days this year.  The leap year corrections
!  are:
!        Jan. 1 - Feb. 28   Have not had the leap day counted above.
!        Feb.29             Counting leap day twice.
!-----------------------------------------------------------------------
!
      no_day=day
      DO n=1,month-1
        no_day=no_day+month_day(n)
      END DO
      IF (leap_flag.and.(no_day.le.feb_end))  no_day=no_day-1
      IF (leap_flag.and.(month.eq.2).and.(day.eq.29)) no_day=no_day-1
!
!-----------------------------------------------------------------------
!  Compute the total number of days since Jan. 1 of the base year,
!  exclusive of the 364 day per year which represent an even 52
!  weeks.  Actually, only need to do the addition mod 7.
!-----------------------------------------------------------------------
!
      no_day=MOD(no_day,7)+MOD(leap,7)+MOD(no_yr,7)+bym1_dec31
!
!-----------------------------------------------------------------------
!  Get the day of the week code.
!-----------------------------------------------------------------------
!
      code=MOD(no_day,7)

      RETURN
      END
