      SUBROUTINE readpar (iunit, file)
!
!svn $Id: readpar.F 833 2017-01-24 22:20:16Z arango $
!================================================== Hernan G. Arango ===
!  Copyright (c) 2002-2017 The ROMS/TOMS Group                         !
!    Licensed under a MIT/X style license                              !
!    See License_ROMS.txt                                              !
!=======================================================================
!                                                                      !
!  This routine reads plotting internal parameters.                    !
!                                                                      !
!=======================================================================
!
!-----------------------------------------------------------------------
!  Define global data.
!-----------------------------------------------------------------------
!
      implicit none
#include "pltio.h"
#include "pltncar.h"
#include "pltfld.h"
!
!-----------------------------------------------------------------------
!  Define local data.
!-----------------------------------------------------------------------
!
      integer i, iflds, ilevs, iunit, j, k, lstr
      integer lenstr
      character*12  pname
      character*(*) file
!
!=======================================================================
!  Read internal plotting parameters.
!=======================================================================
!
      pname='LVPINFO'
      READ (iunit,*,ERR=20) lvpinfo, pname
      IF (pname(1:7).ne.'LVPINFO') GO TO 20

      pname='SDV_FAC'
      READ (iunit,*,ERR=20) sdv_fac, pname
      IF (pname(1:7).ne.'SDV_FAC') GO TO 20

      pname='GEO_LAB'
      READ (iunit,*,ERR=20) geo_lab, pname
      IF (pname(1:7).ne.'GEO_LAB') GO TO 20

      pname='LOG_CNT'
      READ (iunit,*,ERR=20) log_cnt, pname
      IF (pname(1:7).ne.'LOG_CNT') GO TO 20

      pname='LNOZERO'
      READ (iunit,*,ERR=20) lnozero, pname
      IF (pname(1:7).ne.'LNOZERO') GO TO 20

      pname='LCNTLAB'
      READ (iunit,*,ERR=20) lcntlab, pname
      IF (pname(1:7).ne.'LCNTLAB') GO TO 20

      pname='CNT_CLL'
      READ (iunit,*,ERR=20) cnt_cll, pname
      IF (pname(1:7).ne.'CNT_CLL') GO TO 20

      pname='LDRWUSR'
      READ (iunit,*,ERR=20) ldrwusr, pname
      IF (pname(1:7).ne.'LDRWUSR') GO TO 20

      pname='LHIGLOW'
      READ (iunit,*,ERR=20) lhiglow, pname
      IF (pname(1:7).ne.'LHIGLOW') GO TO 20

      pname='LMODDAY'
      READ (iunit,*,ERR=20) lmodday, pname
      IF (pname(1:7).ne.'LMODDAY') GO TO 20

      pname='LDOMAIN'
      READ (iunit,*,ERR=20) ldomain, pname
      IF (pname(1:7).ne.'LDOMAIN') GO TO 20

      pname='LFRAME'
      READ (iunit,*,ERR=20) lframe, pname
      IF (pname(1:6).ne.'LFRAME') GO TO 20

      pname='LTMARKS'
      READ (iunit,*,ERR=20) ltmarks, pname
      IF (pname(1:7).ne.'LTMARKS') GO TO 20

      pname='LCLRBAR'
      READ (iunit,*,ERR=20) lclrbar, pname
      IF (pname(1:7).ne.'LCLRBAR') GO TO 20

      pname='LVECRHO'
      READ (iunit,*,ERR=20) lvecrho, pname
      IF (pname(1:7).ne.'LVECRHO') GO TO 20

      pname='LVCOLOR'
      READ (iunit,*,ERR=20) lvcolor, pname
      IF (pname(1:7).ne.'LVCOLOR') GO TO 20

      pname='LROTVEC'
      READ (iunit,*,ERR=20) lrotvec, pname
      IF (pname(1:7).ne.'LROTVEC') GO TO 20

      pname='LZEROZN'
      READ (iunit,*,ERR=20) lzerozn, pname
      IF (pname(1:7).ne.'LZEROZN') GO TO 20

      pname='THCKUSR'
      READ (iunit,*,ERR=20) thckusr, pname
      IF (pname(1:7).ne.'THCKUSR') GO TO 20

      pname='VEC_VPO'
      READ (iunit,*,ERR=20) vec_vpo, pname
      IF (pname(1:7).ne.'VEC_VPO') GO TO 20

      pname='VEC_AMN'
      READ (iunit,*,ERR=20) vec_amn, pname
      IF (pname(1:7).ne.'VEC_AMN') GO TO 20

      pname='VEC_AMX'
      READ (iunit,*,ERR=20) vec_amx, pname
      IF (pname(1:7).ne.'VEC_AMX') GO TO 20

      pname='VEC_VLC'
      READ (iunit,*,ERR=20) vec_vlc, pname
      IF (pname(1:7).ne.'VEC_VLC') GO TO 20

      pname='VEC_VFR'
      READ (iunit,*,ERR=20) vec_vfr, pname
      IF (pname(1:7).ne.'VEC_VFR') GO TO 20

      pname='VEC_VRM'
      READ (iunit,*,ERR=20) vec_vrm, pname
      IF (pname(1:7).ne.'VEC_VRM') GO TO 20

      pname='VEC_MXX'
      READ (iunit,*,ERR=20) vec_mxx, pname
      IF (pname(1:7).ne.'VEC_MXX') GO TO 20

      pname='VEC_MXY'
      READ (iunit,*,ERR=20) vec_mxy, pname
      IF (pname(1:7).ne.'VEC_MXY') GO TO 20

      pname='SECAXIS'
      READ (iunit,*,ERR=20) secaxis, pname
      IF (pname(1:7).ne.'SECAXIS') GO TO 20

      pname='COORSCL'
      READ (iunit,*,ERR=20) coorscl, pname
      IF (pname(1:7).ne.'COORSCL') GO TO 20

      pname='HVELSCL'
      READ (iunit,*,ERR=20) hvelscl, pname
      IF (pname(1:7).ne.'HVELSCL') GO TO 20

      pname='WVELSCL'
      READ (iunit,*,ERR=20) wvelscl, pname
      IF (pname(1:7).ne.'WVELSCL') GO TO 20

      pname='ZREF'
      READ (iunit,*,ERR=20) zref, pname
      IF (pname(1:4).ne.'ZREF') GO TO 20

      pname='SMTHOUT'
      READ (iunit,*,ERR=20) smthout, pname
      IF (pname(1:7).ne.'SMTHOUT') GO TO 20

      pname='NSAPPL'
      READ (iunit,*,ERR=20) nsappl, pname
      IF (pname(1:6).ne.'NSAPPL') GO TO 20

      pname='NORD'
      READ (iunit,*,ERR=20) nord, pname
      IF (pname(1:4).ne.'NORD') GO TO 20

      pname='FontSz1'
      READ (iunit,*,ERR=20) FontSize1, pname
      IF (pname(1:7).ne.'FontSz1') GO TO 20

      pname='FontSz2'
      READ (iunit,*,ERR=20) FontSize2, pname
      IF (pname(1:7).ne.'FontSz2') GO TO 20

      pname='FontSz3'
      READ (iunit,*,ERR=20) FontSize3, pname
      IF (pname(1:7).ne.'FontSz3') GO TO 20

      pname='FontSz4'
      READ (iunit,*,ERR=20) FontSize4, pname
      IF (pname(1:7).ne.'FontSz4') GO TO 20

      pname='FontSz5'
      READ (iunit,*,ERR=20) FontSize5, pname
      IF (pname(1:7).ne.'FontSz5') GO TO 20

      pname='FontSz6'
      READ (iunit,*,ERR=20) FontSize6, pname
      IF (pname(1:7).ne.'FontSz6') GO TO 20
!
!  Read in contour interval matrix.
!
      READ (iunit,*,ERR=20) iflds, ilevs
      DO i=0,iflds-1
        READ (iunit,*,ERR=20) j, (cont(j,k),k=1,ilevs)
      END DO
      GO TO 40
 20   CONTINUE
      lstr=lenstr(pname)
      WRITE (stdout,30) pname(1:lstr), file
 30   FORMAT (/,' READPAR - error while reading internal plotting ',    &
     &        ' parameter, ',a,/,11x,'File: ', a,/,                     &
     &        /,11x,'Get latest version of this input file.',           &
     &        /,11x,'It must contain the following parameters:',/,      &
     &        /,11x,'LVPINFO, SDV_FAC, GEO_LAB, LOG_CNT, LNOZERO,',     &
     &        /,11x,'LCNTLAB, CNT_CLL, LDRWUSR, LHIGLOW, LMODDAY,',     &
     &        /,11x,'LDOMAIN, LFRAME,  LTMARKS, LCLRBAR, LVECRHO,',     &
     &        /,11x,'LVCOLOR, LROTVEC, LZEROZN, THCKUDR, VEC_VPO,',     &
     &        /,11x,'VEC_AMN, VEC_AMX, VEC_VLC, VEC_VFR, VEC_VRM,',     &
     &        /,11x,'VEC_MXX, VEC_MXY, SECAXIS, COORSCL, HVELSCL,',     &
     &        /,11x,'WVELSCL, ZREF,    SMTHOUT, NSAPPL,  NORD,   ',     &
     &        /,11x,'IFLDS,   ILEVS')
      CALL crash('READPAR',0)
 40   CONTINUE

      RETURN
      END
