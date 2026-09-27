      SUBROUTINE viewport (fl,fr,fb,ft,sclmap,ipltpg,npage,icolor,      &
     &                     wrthdr,pltlogo)
!
!svn $Id: viewport.F 833 2017-01-24 22:20:16Z arango $
!================================================== Hernan G. Arango ===
!  Copyright (c) 2002-2017 The ROMS/TOMS Group                         !
!    Licensed under a MIT/X style license                              !
!    See License_ROMS.txt                                              !
!=======================================================================
!                                                                      !
!  This routine sets the viewport coordinates in the fractional        !
!  system according to the number of plots per page and current        !
!  plot in the page.                                                   !
!                                                                      !
!=======================================================================
!
!-----------------------------------------------------------------------
!  Define global data.
!-----------------------------------------------------------------------
!
      implicit none
#include "pconst.h"
!
!-----------------------------------------------------------------------
!  Define local data.
!-----------------------------------------------------------------------
!
      logical first,landscape,pltlogo,portrait,small,wrthdr
      integer icolor,ipltpg,npage
      real fb,fl,fr,ft,pgb,pgl,pgr,pgt,sclmap,vb,vl,vr,vt,wdthbar
      data first /.true./
      save first,landscape,portrait,pgb,pgl,pgr,pgt
!
!=======================================================================
!  Begin executable code.
!=======================================================================
!
!  Set page dimensions in inches.
!
      IF (first) THEN
        ipltpg=0
        IF ((npage.eq.1).or.(npage.eq.2)) THEN
          portrait=.true.
          landscape=.false.
          pgl=0.0
          pgr=8.5
          pgb=0.0
          pgt=11.0
        ELSE IF (npage.eq.4) THEN
          portrait=.false.
          landscape=.true.
          pgl=0.0
          pgr=11.0
          pgb=0.0
          pgt=8.5
        END IF
        first=.false.
      END IF
!
!  Set offset due to color bar
!
      IF (icolor.eq.0) THEN
        wdthbar=0.0
      ELSE
        wdthbar=1.0
      END IF
!
!  Determine size of plot pannels acording to the presence of plot
!  titles and logo.
!
      IF ((.not.pltlogo).and.(.not.wrthdr)) THEN
        small=.false.
      ELSE
        small=.true.
      END IF
!
!  Set viewport coordinates of the edges in inches.
!
      IF ((ipltpg+1).gt.npage) ipltpg=0
      ipltpg=ipltpg+1
      IF (small) THEN
        IF (npage.eq.1) THEN
          vl=1.0
          vr=10.0-wdthbar
          vb=2.0
          vt=8.75
          sclmap=1.0
        ELSE IF ((npage.eq.2).and.(ipltpg.eq.1)) THEN
          vl=2.0
          vr=9.0-wdthbar
          vb=5.5
          vt=9.0
          sclmap=c1
        ELSE IF ((npage.eq.2).and.(ipltpg.eq.2)) THEN
           vl=2.0
           vr=9.0-wdthbar
           vb=0.75
           vt=4.25
           sclmap=1.0
         ELSE IF ((npage.eq.4).and.(ipltpg.eq.1)) THEN
           vl=0.75
           vr=5.4375-wdthbar
           vb=5.5
           vt=9.0
           sclmap=0.5
         ELSE IF ((npage.eq.4).and.(ipltpg.eq.2)) THEN
           vl=6.25
           vr=10.9375-wdthbar
           vb=5.5
           vt=9.0
           sclmap=0.5
         ELSE IF ((npage.eq.4).and.(ipltpg.eq.3)) THEN
           vl=0.75
           vr=5.4375-wdthbar
           vb=0.75
           vt=4.25
           sclmap=0.5
         ELSE IF ((npage.eq.4).and.(ipltpg.eq.4)) THEN
           vl=6.25
           vr=10.9375-wdthbar
           vb=0.75
           vt=4.25
           sclmap=0.5
         END IF
      ELSE
        IF (npage.eq.1) THEN
          vl=1.0
          vr=10.0-wdthbar
          vb=2.0
          vt=10.0
          sclmap=1.0
        ELSE IF ((npage.eq.2).and.(ipltpg.eq.1)) THEN
          vl=2.0
          vr=9.0-wdthbar
          vb=6.0
          vt=10.0
          sclmap=1.0
        ELSE IF ((npage.eq.2).and.(ipltpg.eq.2)) THEN
          vl=2.0
          vr=9.0-wdthbar
          vb=0.75
          vt=4.75
          sclmap=1.0
        ELSE IF ((npage.eq.4).and.(ipltpg.eq.1)) THEN
          vl=0.75
          vr=5.4375-wdthbar
          vb=6.0
          vt=10.0
          sclmap=0.5
        ELSE IF ((npage.eq.4).and.(ipltpg.eq.2)) THEN
          vl=6.25
          vr=10.9375-wdthbar
          vb=6.0
          vt=10.0
          sclmap=0.5
        ELSE IF ((npage.eq.4).and.(ipltpg.eq.3)) THEN
          vl=0.75
          vr=5.4375-wdthbar
          vb=0.75
          vt=4.75
          sclmap=0.5
        ELSE IF ((npage.eq.4).and.(ipltpg.eq.4)) THEN
          vl=6.25
          vr=10.9375-wdthbar
          vb=0.75
          vt=4.75
          sclmap=0.5
        END IF
      END IF
!
!  Set normalized viewport coordinates.
!
      IF (portrait) THEN
        fl=vl/(pgt-pgb)
        fr=vr/(pgt-pgb)
        fb=vb/(pgt-pgb)
        ft=vt/(pgt-pgb)
      ELSE IF (landscape) THEN
        fl=vl/(pgr-pgl)
        fr=vr/(pgr-pgl)
        fb=vb/(pgr-pgl)
        ft=vt/(pgr-pgl)
      END IF

      RETURN
      END
