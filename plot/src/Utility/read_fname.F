      SUBROUTINE read_fname (unit, file_name)
!
!svn $Id: read_fname.F 833 2017-01-24 22:20:16Z arango $
!================================================== Hernan G. Arango ===
!  Copyright (c) 2002-2017 The ROMS/TOMS Group                         !
!    Licensed under a MIT/X style license                              !
!    See License_ROMS.txt                                              !
!=======================================================================
!                                                                      !
!  This routine reads file name string from specified fortran unit.    !
!                                                                      !
!=======================================================================
!
!-----------------------------------------------------------------------
!  Define local data.
!-----------------------------------------------------------------------
!
      implicit none
      integer ldir, lstr, unit
      integer lenstr
      character*(*) file_name
      character*80  home
      character*160 text
!
!=======================================================================
!  Begin executable code.
!=======================================================================
!
!  Read file name. If name starts with '~/', replace tilde with home
!  directory path from the HOME environmental variable.
!
      READ (unit,'(a)') text
      lstr=lenstr(text)
      IF (text(1:2).eq.'~/') THEN
        CALL getenv ('HOME', home)
        ldir=lenstr(home)
        file_name=home(1:ldir)//text(2:lstr)
      ELSE      
        file_name=text(1:lstr)
      END IF

      RETURN
      END
