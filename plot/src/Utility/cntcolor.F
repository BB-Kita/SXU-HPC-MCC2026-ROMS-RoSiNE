      SUBROUTINE cntcolor (f,mm,m,n,fmin,fmax,iccnt,ipos,lcmsk,overlay)
!
!svn $Id: cntcolor.F 833 2017-01-24 22:20:16Z arango $
!================================================== Hernan G. Arango ===
!  Copyright (c) 2002-2017 The ROMS/TOMS Group                         !
!    Licensed under a MIT/X style license                              !
!    See License_ROMS.txt                                              !
!=======================================================================
!                                                                      !
!  This routine contours in color (solid fill) and draws color bar.    !
!  It assumes  that the User has  call SET previously for properly     !
!  positioning of the color map.  Color palette is selected by the     !
!  User and stored in common block COLPAL.                             !
!                                                                      !
!=======================================================================
!
!-----------------------------------------------------------------------
!  Define global data.
!-----------------------------------------------------------------------
!
      implicit none
#include "param.h"
#include "pconst.h"
#include "cntwkspa.h"
#include "colpal.h"
#include "fldsid.h"
#include "fmask.h"
#include "pltfld.h"
#include "pltncar.h"
!
!-----------------------------------------------------------------------
!  Define local data.
!-----------------------------------------------------------------------
!
      logical overlay
      integer fill, i, iccnt, ipos, isu, liwku, lcmsk, lrwku, m, mm, n
      real cint, clv, cmin, cmax, fmax, fmin, zmn, zmx
      real f(mm,n)
      external colram
!
!=======================================================================
!  Begin executable code.
!=======================================================================
!
      cmin=fmin
      cmax=fmax
!
!-----------------------------------------------------------------------
!  Initialize area map.
!-----------------------------------------------------------------------
!
      IF (.not.overlay) THEN
        CALL arinam (iam,lam)
      END IF
!
!-----------------------------------------------------------------------
!  If land masking, paint the entire plot window with blue water.
!-----------------------------------------------------------------------
!
      lmask=lcmsk
      IF (.not.overlay.and.(lcmsk.gt.0)) THEN
        fill=0
        CALL landmask (fill)
        IF (msk) THEN
          fill=1
          CALL landmask (fill)
        END IF
      END IF
!
!-----------------------------------------------------------------------
!  Set CONPACK options.
!-----------------------------------------------------------------------
!
!  Prevent CONPACK from setting the viewport.
!
      CALL cpseti ('SET - do-set-call flag',0)
!
!  Set mapping to that provided by the User.
!
      IF (spherical) THEN
        CALL cpseti ('MAP - mapping flag',3)
      ELSE
        CALL cpseti ('MAP - mapping flag',4)
      END IF
!
!  Set special value feature.
!
      CALL cpsetr ('SPV - special value',spval1)
!
!  Set  NCL-1  contour levels,  splitting the range into NCL equal
!  bands, one for each of the NCL colors available.  Specify those
!  levels, and define exactly what is to be done at each level.
!
      CALL cpseti ('CLS - contour level selector',0)
      CALL cpseti ('NCL - number of contour levels',ncl-1)
      IF (log_cnt) THEN
        IF (cmin.le.c0) THEN
          cint=LOG10(cmax)/FLOAT(ncl)
          cmin=c0          
        ELSE
          cint=(LOG10(cmax)-LOG10(cmin))/FLOAT(ncl)
        END IF
        DO i=1,ncl-1
          IF (cmin.le.c0) THEN
            clv=10**(FLOAT(i)*cint)
          ELSE
            clv=10**(LOG10(cmin)+FLOAT(i)*cint)
          END IF
          CALL cpseti ('PAI - parameter index array',i)
          CALL cpsetr ('CLV - contour level',clv)
          CALL cpseti ('CLU - contour level use',1)
          CALL cpseti ('AIB - area indentifier below level',i)
          CALL cpseti ('AIA - area indentifier above level',i+1)
        END DO
      ELSE IF (c_levels) THEN
        DO i=1,ncl-1
          clv=clevels(i)
          CALL cpseti ('PAI - parameter index array',i)
          CALL cpsetr ('CLV - contour level',clv)
          IF (vcode(fieldid).eq.'IDHRAW') THEN
            IF (clv.lt.0) THEN
              CALL cpseti ('CLU - contour level use',1)
            ELSE                ! turn off drawing of land contour lines
              CALL cpseti ('CLU - contour level use',0)  
            END IF
          ELSE
            CALL cpseti ('CLU - contour level use',1)
          END IF
          CALL cpseti ('AIB - area indentifier below level',i)
          CALL cpseti ('AIA - area indentifier above level',i+1)
        END DO
      ELSE
        cint=(cmax-cmin)/FLOAT(ncl)
        DO i=1,ncl-1
          clv=cmin+FLOAT(i)*cint
          CALL cpseti ('PAI - parameter index array',i)
          CALL cpsetr ('CLV - contour level',clv)
          CALL cpseti ('CLU - contour level use',1)
          CALL cpseti ('AIB - area indentifier below level',i)
          CALL cpseti ('AIA - area indentifier above level',i+1)
        END DO
      END IF
!
!  Provide more room for X and Y coordinates defining contours lines.
!
      CALL cpseti ('RWC - real workspace for contours',300)
!
!-----------------------------------------------------------------------
!  Initialize the drawing of the contour plot.
!-----------------------------------------------------------------------
!
      CALL cprect (f,mm,m,n,rwrk,lrwk,iwrk,liwk)
      CALL cpgetr ('ZMN - minimum value found',zmn)
      CALL cpgetr ('ZMX - maximum value found',zmx)
!
!-----------------------------------------------------------------------
!  Put contour lines into area map.
!-----------------------------------------------------------------------
!
      CALL cpclam (f,rwrk,iwrk,iam)
!
!-----------------------------------------------------------------------
!  Color the map.
!-----------------------------------------------------------------------
!
      CALL arpram (iam,0,0,0)
      CALL arscam (iam,xcs,ycs,mcs,iai,iag,mai,colram)
!
!-----------------------------------------------------------------------
!  Put contour lines over the color map.
!-----------------------------------------------------------------------
!
      IF (iccnt.eq.1) THEN
        CALL gsplci (1)
        CALL cpcldr (f,rwrk,iwrk)
      END IF
!
!-----------------------------------------------------------------------
!  If land masking, retrieve, project, and add the selected coastlines
!  to a new area map and then paint land areas.  Notice that the area
!  map is re-initialized.
!-----------------------------------------------------------------------
!
      IF ((lcmsk.gt.0).and.(.not.msk)) THEN
        fill=1
        CALL arinam (iam,lam)
        CALL landmask (fill)
        CALL arpram (iam,0,0,0)
        CALL arscam (iam,xcs,ycs,mcs,iai,iag,mai,colram)
      END IF
!
!-----------------------------------------------------------------------
!  Report the work space used.
!-----------------------------------------------------------------------
!
      CALL cpgeti ('RWU - real workspace usage',lrwku)
      CALL cpgeti ('IWU - integer workspace usage',liwku)
      isu=lam-(iam(6)-iam(5)-1)
      WRITE (6,10) lrwku, lrwk, liwku, liwk, isu, lam
  10  FORMAT (/' CNTCOLOR - real workspace used: ',5x,i10,5x,'given: ', &
     &       i10,/10x,'- integer workspace used: ',2x,i10,5x,'given: ', &
     &       i10,/10x,'- space used in area mask: ',1x,i10,5x,'given: ',&
     &       i10,/)
!
!-----------------------------------------------------------------------
!  Zero-out contour level array to facilitate overlays, if any.
!  Set back defaults.
!-----------------------------------------------------------------------
!
      DO i=1,ncl-1
        CALL cpseti ('PAI - parameter index array',i)
        CALL cpsetr ('CLV - contour level',c0)
      END DO
!
!-----------------------------------------------------------------------
!  Draw color bar (turn off and on clipping).
!-----------------------------------------------------------------------
!
      CALL gsclip (0)
      IF (lclrbar) CALL colorbar (ncl,ibarlab,cmin,cmax,ipos)
      CALL gsclip (1)

      RETURN
      END
