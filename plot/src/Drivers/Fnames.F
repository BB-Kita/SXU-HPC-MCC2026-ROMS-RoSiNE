      PROGRAM Fnames
!
!svn $Id: Fnames.F 833 2017-01-24 22:20:16Z arango $
!================================================== Hernan G. Arango ===
!  Copyright (c) 2002-2017 The ROMS/TOMS Group                         !
!    Licensed under a MIT/X style license                              !
!    See License_ROMS.txt                                              !
!=======================================================================
!                                                                      !
!  This decodes variables ID file and writes out current plotting      !
!  field identification.                                               !
!                                                                      !
!=======================================================================
!
      implicit none
      logical first, is2df
      integer ftype, i, icheck, inp, out, lstr, stdinp, stdout
      integer lenstr
      character*1  char1, char2
      character*6  fcode
      character*20 name1, name2
      character*60 flabel
      character*80 Iname, Oname
      parameter (inp=1, out=2, stdinp=5, stdout=6, char1=' ', char2='*')
!
!-----------------------------------------------------------------------
!  Read in and write out plotting field identification.
!-----------------------------------------------------------------------
!
      WRITE (stdout,10)
 10   FORMAT (/,'Enter input  identification file: ',$)
      READ (stdinp,'(a)') Iname
      WRITE (stdout,20)
 20   FORMAT ('Enter output identification file: ',$)
      READ (stdinp,'(a)') Oname
!
      OPEN (inp,file=Iname,form='formatted',status='old')
      OPEN (out,file=Oname,form='formatted',status='unknown')
!
      WRITE (out,30)
 30   FORMAT (1x,'Plotting Fields classification (* derived fields):',/)
!
      first=.true.
      icheck=0
      DO WHILE (icheck.lt.999)
        READ (inp,*) i, fcode, ftype, is2df, name1, name2, flabel
        IF (first.and.(i.ge.200)) THEN
          first=.false.
          WRITE (out,'(a)') ' '
        END IF
        IF (i.ne.999) THEN
          lstr=lenstr(flabel)
          IF (ftype.eq.1) THEN
            WRITE (out,40) char1, i, fcode, flabel(1:lstr)
          ELSE
            WRITE (out,40) char2, i, fcode, flabel(1:lstr)
          END IF
 40       FORMAT (a1,'['i3.3,']',2x,a6,4x,a)
        END IF
        icheck=i
      END DO
      CLOSE (inp)
      CLOSE (out)

      STOP
      END

      FUNCTION lenstr (string)
!
!=======================================================================
!                                                                      !
!  This function RETURNs the character position of the last non-       !
!  blank character in "string" after removing the leading blank        !
!  characters, if any.                                                 !
!                                                                      !
!=======================================================================
!
!-----------------------------------------------------------------------
!  Define local data.
!-----------------------------------------------------------------------
!
      implicit none
      integer i, ic, istart, nchar
      integer lenstr
      character*(*) string
!
!-----------------------------------------------------------------------
!  Begin executable code.
!-----------------------------------------------------------------------
!
!  Find length of input character string.
!
      nchar=len(string)
!
!  Strip leading blank characters, if any.
!
      ic=0
      DO i=1,nchar
        IF (string(i:i).eq.' ') THEN
          ic=ic+1
        ELSE
          GO TO 10
        END IF
      END DO
  10  istart=ic+1
      IF (istart.lt.nchar) string=string(istart:nchar)
!
!  Strip trailing blank characters and determine the its length.
!
      DO i=nchar,1,-1
        IF (string(i:i).ne.' ') GO TO 20
      END DO
      lenstr=0
      RETURN
  20  lenstr=i

      RETURN
      END
