      SUBROUTINE cntpack (f,mm,m,n,fmin,fmax,cint,imask,icolor,overlay)
!
!=======================================================================
!  Copyright (c) 1996 Rutgers University                             ===
!=======================================================================
!                                                                    ===
!  This routine uses  NCAR  CONPACK  utility to  contour the grided  ===
!  field F(M,N). It assumes that the user has call FRAME previously  ===
!  for properly positioning of the color levels.                     ===
!                                                                    ===
!=======================================================================
!
      implicit none
#include "pconst.h"
#include "cntwkspa.h"
#include "colpal.h"
#include "pltfld.h"
#include "pltncar.h"
!
!-----------------------------------------------------------------------
!  Define local data.
!-----------------------------------------------------------------------
!
      logical overlay
      integer i, iclu, icolor, imask, isu, liwku, lrwku, m, mm, n, nocl,&
     &        radx, rady
      real cint, fmax, fmin
      real c_lev(2000), f(mm,n)
      parameter (radx=2,rady=2)
      data c_lev /2000*c0/
      external drawcl, cpdrpl
      integer ilevs, ifield
      parameter (ilevs = 47)
      parameter (ifield = 1)
!
!=======================================================================
!  Begin executable code.
!=======================================================================
!
!  Reset all COMPACK parameters to their default values to facilitate
!  overlays.
!
      CALL cprset
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
!  Set contour interval and specify the interval between labelled
!  contours.
!
      IF (c_levels) THEN
        CALL cpseti('CLS - contour level selector',0)
        CALL cpseti('NCL - number of contour levels',ncl)
        DO i=1,ncl-1
          CALL cpseti('PAI - array index',i)
          CALL cpsetr('CLV - contour level',clevels(i))
          CALL cpseti('CLU - line type',3)
        END DO
      ELSE IF (cint .lt. 0.) THEN
        CALL cpseti('CLS - contour level selector',0)
        CALL cpseti('NCL - number of contour levels',ilevs)
        DO i=1,ilevs
          CALL cpseti('PAI - array index',i)
          CALL cpsetr('CLV - contour level',cont(ifield,i))
          CALL cpseti('CLU - line type',3)
        END DO
      ELSE
        CALL cpsetr('CMN - contour minimum',fmin)
        CALL cpsetr('CMX - contour maximum',fmax)
        CALL cpsetr('CIS - contour interval selector',abs(cint))
        IF (imask.eq.0) THEN
           CALL cpseti('LIS - label interval specifier',2)  ! overalays
        ELSE
           CALL cpseti('LIS - label interval specifier',4)  ! primary
        END IF
      END IF
!
!  Set up color of contours.
!
      IF (icolor.eq.2) THEN
        CALL gsplci (ncl+4)
      END IF
!
!  Set high/low label information.
!
      IF (lhiglow) THEN
        CALL cpsetc ('HIT - High label test string','D$B$$ZDVU$m$E$')
        CALL cpsetc ('LOT - Low label text string','S$B$$ZDVU$m$E$')
        CALL cpsetr ('HLS - High/Low label size',p009)
        CALL cpsetr ('HLW - High/Low white space width',r400)
        CALL cpseti ('HLX - High/Low search radius in x',radx)
        CALL cpseti ('HLY - High/Low search radius in y',rady)
      END IF
!
!  Make PLOTCHAR use high-quality character.
!
      CALL pcseti ('QU - quality flag',0)
!
!  Make all CONPACK-written characters a little bigger.
!
      CALL cpsetr ('CWM - character width multiplier',1.25)
!
!  Turn off informational label.
!
      CALL cpsetc ('ILT - informational label text',' ')
!
!  If overlays, turn on the positioning of labels by the penalty scheme
!  and turn off high/low labels.  The penalty scheme is tricky.  The
!  constants below are the default values.
!
      IF (imask.eq.0) THEN
        CALL cpseti ('LLP - line label positioning',3)
        CALL cpsetr ('PC1 - penalty scheme constant 1',1.0)
        CALL cpsetr ('PC2 - penalty scheme constant 2',5.0)
        CALL cpsetr ('PC3 - penalty scheme constant 3',60.0)
        CALL cpsetr ('PC4 - penalty scheme constant 4',0.05)
        CALL cpsetr ('PC5 - penalty scheme constant 5',0.15)
        CALL cpsetr ('PC6 - penalty scheme constant 6',0.30)
        CALL cpsetc ('HIT - High label test string',' ')
        CALL cpsetc ('LOT - Low label text string',' ')
      END IF
!
!  Provide more room for X and Y coordinates defining contours lines.
!
      CALL cpseti ('RWC - real workspace for contours',300)
!
!  Initialize the drawing of the contour plot.
!
      CALL cprect (f,mm,m,n,rwrk,lrwk,iwrk,liwk)
!
!-----------------------------------------------------------------------
!  Set contour levels.
!-----------------------------------------------------------------------
!
      CALL cppkcl (f,rwrk,iwrk)
      CALL cpgeti ('NCL - number of contour levels',nocl)
      CALL cpgetr ('CIU - contour interval used',cint)
      DO i=1,nocl
        CALL cpseti ('PAI - parameter array index',i)
        CALL cpgetr ('CLV - contour level value',c_lev(i))
!
!  If requested, omit the drawing of zero-contour lines.
!
        IF (lnozero.and.(c_lev(i).eq.c0)) THEN
          CALL cpseti ('CLU - contour level use flags',0)
        END IF
!
!  Set solid lines for positive contour levels and dash pattern
!  for negative contour levels.
!
        IF (c_lev(i).ge.0) THEN
          CALL cpseti ('CLD - contour line solid pattern',65535)
        ELSE
          CALL cpseti ('CLD - contour line dash pattern',43690)
        END IF
!
!  Do not label contours levels.
!
        IF (.not.lcntlab) THEN
          CALL cpseti ('CLU - contour-line line width',1)
        END IF
!
!  Set width of labeled contour lines.
!
        IF (lcntlab) THEN
          CALL cpgeti ('CLU - contour level use flag',iclu)
          IF (iclu.eq.3) THEN
            CALL cpseti ('CLL - contour-line line width',cnt_cll)
          END IF
        END IF
      END DO
!
!  Get contour interval used for information purposes.
!
      IF (cint.eq.c0) THEN
        cint=abs(c_lev(2)-c_lev(1))
      END IF
!
!-----------------------------------------------------------------------
!  Draw masked or unmasked contour lines.
!-----------------------------------------------------------------------
!
      IF (imask.eq.1) THEN
!
!  Initialize area map.
!
        CALL arinam (iam,lam)
!
!  Define mask area edges.
!
        CALL aredam (iam,xcs,ycs,nxy,5,0,-1)
!
!  Pre-process the area map.
!
        CALL arpram (iam,0,0,0)
!
!  Draw masked countour lines.
!
        CALL cpcldm (f,rwrk,iwrk,iam,drawcl)
!
!  Draw regular contour lines but bock the drawing of contour lines
!  through High/Low labels.
!
      ELSE IF (imask.lt.0) THEN
        CALL arinam (iam,lam)
        CALL cplbdr (f,rwrk,iwrk)
        CALL cplbam (f,rwrk,iwrk,iam)
        CALL arpram (iam,0,0,0)
        CALL cpcldm (f,rwrk,iwrk,iam,cpdrpl)
!
!  Draw regular contour lines.
!
      ELSE 
        CALL arinam (iam,lam)
        CALL cplbam (f,rwrk,iwrk,iam)
        CALL cplbdr (f,rwrk,iwrk)
        CALL arpram (iam,0,0,0)
        CALL cpcldm (f,rwrk,iwrk,iam,drawcl)
      END IF
!
!  Set polyline color back to its foreground color.
!
      IF (icolor.eq.2) CALL gsplci (1)
!
!-----------------------------------------------------------------------
!  Give the work space used.
!-----------------------------------------------------------------------
!
      CALL cpgeti ('RWU - real workspace usage',lrwku)
      CALL cpgeti ('IWU - integer workspace usage',liwku)
      WRITE (6,10) lrwku, lrwk, liwku, liwk
  10  FORMAT (/' CNTPACK  - real workspace used: ',5x,i10,5x,'given: ', &
     &       i10,/10x,'- integer workspace used: ',2x,i10,5x,'given: ', &
     &       i10,/)
      IF (imask.gt.0) THEN
        isu=lam-(iam(6)-iam(5)-1)
        WRITE (6,20) isu,lam
  20    FORMAT (9x, '- space used in area mask: ',i6,5x,'given: ',i7)
      END IF

      RETURN
      END
