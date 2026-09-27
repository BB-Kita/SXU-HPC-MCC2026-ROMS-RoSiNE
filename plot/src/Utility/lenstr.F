      FUNCTION lenstr (string)
!
!svn $Id: lenstr.F 833 2017-01-24 22:20:16Z arango $
!================================================== Hernan G. Arango ===
!  Copyright (c) 2002-2017 The ROMS/TOMS Group                         !
!    Licensed under a MIT/X style license                              !
!    See License_ROMS.txt                                              !
!=======================================================================
!                                                                      !
!  This function returns the character position of the last non-       !
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
