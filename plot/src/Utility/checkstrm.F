      SUBROUTINE checkstrm (text,lstr,strm)
!
!svn $Id: checkstrm.F 833 2017-01-24 22:20:16Z arango $
!================================================== Hernan G. Arango ===
!  Copyright (c) 2002-2017 The ROMS/TOMS Group                         !
!    Licensed under a MIT/X style license                              !
!    See License_ROMS.txt                                              !
!=======================================================================
!                                                                      !
!  This routine checks for special character, like colon, in a         !
!  text string.   If the special character is found the string         !
!  is over-written with correct NCAR syntax for that character.        !
!                                                                      !
!=======================================================================
!
!-----------------------------------------------------------------------
!  Define local data.
!-----------------------------------------------------------------------
!
      implicit none
      integer i, icount, ip, lstr
      integer lenstr
      character*5   colon
      character*(*) strm,text
      data colon /':133:'/
!
!=======================================================================
!  Begin executable code.
!=======================================================================
!
      lstr=len(text)
      DO i=1,lstr
        text(i:i)=' '
      END DO
!
      lstr=lenstr(strm)
      ip=0
      icount=0
      DO i=1,lstr
        ip=ip+1
        IF (strm(i:i).eq.':') THEN
          text(ip:ip+5)=colon
          ip=ip+5
          icount=icount+1
        ELSE
          text(ip:ip)=strm(i:i)
        END IF
      END DO
      lstr=lstr+5*icount

      RETURN
      END
