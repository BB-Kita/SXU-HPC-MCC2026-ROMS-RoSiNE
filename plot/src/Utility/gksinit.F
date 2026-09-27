      SUBROUTINE gksinit
!
!svn $Id: gksinit.F 833 2017-01-24 22:20:16Z arango $
!================================================== Hernan G. Arango ===
!  Copyright (c) 2002-2017 The ROMS/TOMS Group                         !
!    Licensed under a MIT/X style license                              !
!    See License_ROMS.txt                                              !
!=======================================================================
!                                                                      !
!  Declare an array to hold the gks "aspect source flags" and          !
!  initialize the values in the aspect-source-flag array.              !
!                                                                      !
!=======================================================================
!
!-----------------------------------------------------------------------
!  Define local data.
!-----------------------------------------------------------------------
!
      implicit none
      integer iasf(13)
      data iasf /13*1/
!
!=======================================================================
!  Begin executable code.
!=======================================================================
!
!  Open GKS.
!
      CALL opngks
!
!  Turn on the clipping indicator.
!
      CALL gsclip(1)
!
!  Set all the GKS aspect source flags to "individual".
!
      CALL gsasf(iasf)
!
!  Force solid fill.
!
      CALL gsfais(1)

      RETURN
      END
