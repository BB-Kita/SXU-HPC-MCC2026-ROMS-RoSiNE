      SUBROUTINE barlab(lab,lenlab,fval,fmin,fmax,scl)
!
!svn $Id: barlab.F 833 2017-01-24 22:20:16Z arango $
!================================================== Hernan G. Arango ===
!  Copyright (c) 2002-2017 The ROMS/TOMS Group                         !
!    Licensed under a MIT/X style license                              !
!    See License_ROMS.txt                                              !
!=======================================================================
!                                                                      !
!  This routine sets label LAB for the color bar.                      !
!                                                                      !
!=======================================================================
!
!-----------------------------------------------------------------------
!  Define Global data.
!-----------------------------------------------------------------------
!
      implicit none
#include "pconst.h"
#include "pltncar.h"
!
!-----------------------------------------------------------------------
!  Define local data.
!-----------------------------------------------------------------------
!
      integer i,ic,idec,idot,iend,istar,lenlab,lens
      integer lenstr
      real favg,fmax,fmin,frms,fval,fvar,scl
      character*4  frmt1
      character*6  frmt2
      character*15 text
      character*10 lab
!
!=======================================================================
!  Begin executable code.
!=======================================================================
!
      favg=p5*(fmin+fmax)*scl
      frms=p5*(fmin*fmin+fmax*fmax)*scl*scl
      fvar=frms-favg*favg
!
!  Determine color bar numeric label.
!
      WRITE (text,10) fval*scl
  10  FORMAT (f15.7)
      lens=lenstr(text)
      ic=0
      DO 20 i=1,lens
        IF (text(i:i).eq.' ') THEN
          ic=ic+1
        ELSE
          GO TO 30
        END IF
  20  CONTINUE
  30  istar=ic+1
      iend=lens-istar
      text=text(istar:lens)
      DO 40 i=1,iend
        IF (text(i:i).eq.'.') GO TO 50
  40  CONTINUE
  50  idot=i-1
      idec=0
      IF (log_cnt) THEN
        idec=3
      ELSE
        IF (fvar.lt.c1) idec=2
        IF (fvar.lt.r100) idec=3
        IF (fvar.ge.c1) idec=1
      END IF
      IF (idec.eq.0) THEN
        IF ((nint(fval*scl)).gt.(int(fval*scl))) THEN
          WRITE (frmt1,60) idot+1
        ELSE
          WRITE (frmt1,60) idot
        END IF
  60    FORMAT ('(i',i1,')')
        WRITE (lab,frmt1) nint(fval*scl)
      ELSE
        WRITE (frmt2,70) idot+idec+1,idec
  70    FORMAT ('(f',i1,'.',i1,')')
        WRITE (lab,frmt2) fval*scl
      END IF
      lenlab=lenstr(lab)
      ic=0
      DO 80 i=1,lenlab
        IF (lab(i:i).eq.' ') THEN
          ic=ic+1
        ELSE
          GO TO 90
        END IF
  80  CONTINUE
  90  istar=ic+1
      lab=lab(istar:lenlab)
      lenlab=lenstr(lab)

      RETURN
      END
