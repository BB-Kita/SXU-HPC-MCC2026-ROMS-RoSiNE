      PROGRAM fieldid
!
!svn $Id: fieldid.F 833 2017-01-24 22:20:16Z arango $
!================================================== Hernan G. Arango ===
!  Copyright (c) 2002-2017 The ROMS/TOMS Group                         !
!    Licensed under a MIT/X style license                              !
!    See License_ROMS.txt                                              !
!=======================================================================
!                                                                      !
!  This program checks the contens of the variable information file    !
!  which is used by the plotting package.                              !
!                                                                      !
!=======================================================================
!
      implicit none
      logical is2d
      integer icheck, inpvar, lcode, llab, lvar1, lvar2, n, stdinp,     &
     &        stdout, vid, vtype
      character*6  vcode
      character*20 vname1, vname2
      character*60 vlabel
      character*80 fname
      parameter (inpvar=1, stdinp=5, stdout=6)
!
!-----------------------------------------------------------------------
!  Begin executable code.
!-----------------------------------------------------------------------
!
!  Get input file name
!
      WRITE (stdout,10)
 10   FORMAT ('Enter input variable ID file name: ',$)
      READ (stdinp,20) fname
 20   FORMAT (a)

      OPEN (inpvar,file=fname,form='formatted',status='old')
!
!  Read in and report out variable information.
!
      n=0
      icheck=0
      DO WHILE (icheck.lt.999)
        READ (inpvar,*) vid, vcode, vtype, is2d, vname1, vname2, vlabel
        lcode=LEN_TRIM(vcode)
        lvar1=LEN_TRIM(vname1)
        lvar2=LEN_TRIM(vname2)
        llab=LEN_TRIM(vlabel)
        WRITE (stdout,30) vid, vcode(1:lcode), vtype, is2d,             &
     &                    vname1(1:lvar1), vname2(1:lvar2),             &
     &                    vlabel(1:llab)
 30     FORMAT (i3.3,1x,a,1x,i1,1x,l1,1x,a,1x,a,t40,a)
        icheck=vid
        n=n+1
      END DO

      STOP
      END
