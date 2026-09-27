      SUBROUTINE mskscl (f,ix,iy,iz,ncid,idfld,igrid,spv,spval)
!
!svn $Id: mskscl.F 833 2017-01-24 22:20:16Z arango $
!================================================== Hernan G. Arango ===
!  Copyright (c) 2002-2017 The ROMS/TOMS Group                         !
!    Licensed under a MIT/X style license                              !
!    See License_ROMS.txt                                              !
!=======================================================================
!                                                                      !
!  This routine masks or scales current field F.                       !
!                                                                      !
!  On Input:                                                           !
!                                                                      !
!     f        Field to scale and/or mask (real array).                !
!     ix       Size of inner dimension (integer).                      !
!     iy       Size of outer dimension (integer).                      !
!     iz       Number of vertical levels (integer).                    !
!     ncid     NetCDF ID for input file (integer).                     !
!     idfld    Identification of field to plot (integer).              !
!     igrid    Staggered C grid type (integer):                        !
!                igrid=0  => None.                                     !
!                igrid=1  => density points.                           !
!                igrid=2  => streamfunction points.                    !
!                igrid=3  => u-velocity points.                        !
!                igrid=4  => v-velocity points.                        !
!                igrid=5  => w-velocity points.                        !
!     spv      Missing data/fill data value (real).                    !
!     spval    Mask special value (real).                              !
!                                                                      !
!  On Output:                                                          !
!                                                                      !
!     f        Masked and/or scale field (real array).                 !
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
#include "fldsid.h"
#include "fmask.h"
#include "ncinfo.h"
#include "pltfld.h"
#include "pltncar.h"
!
!-----------------------------------------------------------------------
!  Define local data.
!-----------------------------------------------------------------------
!
      logical gotmask
      integer i, idfld, igrid, im, imgrid, ip, ix, iy, iz, j, jm, k,    &
     &        ncid, nc_vid
      real Vmax, Vmin, scale, spv, spval, spvnc
      real f(ix,iy,iz)
      character*20 fldnam
!
!=======================================================================
!  Begin executable code.
!=======================================================================
!
!  Determine Land/Sea mask switch, if any.
!
      gotmask=.false.
      IF ((lmsk.gt.0).and.                                              &
          ((mskinp1.and.(ncid.eq.ncgrd)).or.                            &
     &     (mskinp2.and.(ncid.eq.ncid2)))) THEN
        gotmask=.true.
      END IF
!
!-----------------------------------------------------------------------
!  If requested, smooth output data.
!-----------------------------------------------------------------------
!
      IF (smthout.gt.0) THEN
        CALL filter (f,ix,iy,iz,smthout,nord,nsappl)
      END IF
!
!-----------------------------------------------------------------------
!  Scale fields.
!-----------------------------------------------------------------------
!
!  Scale horizontal velocity.  Assumed in (m/s) at input.
!
      IF ((vcode(idfld).eq.'IDUTOT').or.(vcode(idfld).eq.'IDVTOT').or.  &
     &    (vcode(idfld).eq.'IDUCLI').or.(vcode(idfld).eq.'IDVCLI').or.  &
     &    (vcode(idfld).eq.'IDU3DD').or.(vcode(idfld).eq.'IDV3DD').or.  &
     &    (vcode(idfld).eq.'IDUBAR').or.(vcode(idfld).eq.'IDVBAR').or.  &
     &    (vcode(idfld).eq.'IDU2DD').or.(vcode(idfld).eq.'IDV2DD').or.  &
     &    (vcode(idfld).eq.'IDUBED').or.(vcode(idfld).eq.'IDVBED').or.  &
     &    (vcode(idfld).eq.'IDUBOT').or.(vcode(idfld).eq.'IDVBOT').or.  &
     &    (vcode(idfld).eq.'IDUICE').or.(vcode(idfld).eq.'IDVICE').or.  &
     &    (vcode(idfld).eq.'IDUNPY').or.(vcode(idfld).eq.'IDVNPY').or.  &
     &    (vcode(idfld).eq.'IDUSUR').or.(vcode(idfld).eq.'IDVSUR').or.  &
     &    (vcode(idfld).eq.'IDRSUM').or.(vcode(idfld).eq.'IDRSVM').or.  &
     &    (vcode(idfld).eq.'IDSUWD').or.(vcode(idfld).eq.'IDSVWD').or.  &
     &    (vcode(idfld).eq.'IDTVEC').or.(vcode(idfld).eq.'IDTSTR').or.  &
     &    (vcode(idfld).eq.'IDCVEC').or.(vcode(idfld).eq.'IDCSTR').or.  &
     &    (vcode(idfld).eq.'ID3VEC').or.(vcode(idfld).eq.'ID3STR').or.  &
     &    (vcode(idfld).eq.'IDBVEC').or.(vcode(idfld).eq.'IDBSTR').or.  &
     &    (vcode(idfld).eq.'ID2VEC').or.(vcode(idfld).eq.'ID2STR').or.  &
     &    (vcode(idfld).eq.'IDBEDV').or.(vcode(idfld).eq.'IDBEDS').or.  &
     &    (vcode(idfld).eq.'IDBOTV').or.(vcode(idfld).eq.'IDBOTS').or.  &
     &    (vcode(idfld).eq.'IDIVEC').or.(vcode(idfld).eq.'IDISTR').or.  &
     &    (vcode(idfld).eq.'IDNPYV').or.(vcode(idfld).eq.'IDNPYS').or.  &
     &    (vcode(idfld).eq.'IDSVEC').or.(vcode(idfld).eq.'IDSSTR').or.  &
     &    (vcode(idfld).eq.'IDRSMV').or.(vcode(idfld).eq.'IDRSMS').or.  &
     &    (vcode(idfld).eq.'IDSWDV').or.(vcode(idfld).eq.'IDSWDS').or.  &
     &    (vcode(idfld).eq.'IDTBRB').or.(vcode(idfld).eq.'IDCBRB').or.  &
     &    (vcode(idfld).eq.'ID3BRB').or.(vcode(idfld).eq.'ID2BRB').or.  &
     &    (vcode(idfld).eq.'IDBBRB').or.(vcode(idfld).eq.'IDBEDB').or.  &
     &    (vcode(idfld).eq.'IDBOTB').or.(vcode(idfld).eq.'IDIBRB').or.  &
     &    (vcode(idfld).eq.'IDNPYB').or.(vcode(idfld).eq.'IDSBRB').or.  &
     &    (vcode(idfld).eq.'IDRSMB').or.(vcode(idfld).eq.'IDSWDB')) THEN
        IF (hvelscl.eq.0) THEN
          scale=c1
        ELSE IF (hvelscl.eq.1) THEN
          scale=m2cm
        ELSE IF (hvelscl.eq.2) THEN
          scale=m2mm
        ELSE IF (hvelscl.eq.3) THEN
          scale=-m2knot
        ELSE IF (hvelscl.eq.4) THEN
          scale=-m2smallknot
        END IF
        DO k=1,iz
          DO j=1,iy
            DO i=1,ix
              IF (f(i,j,k).ne.spv) f(i,j,k)=scale*f(i,j,k)
            END DO
          END DO
        END DO
!
!  Scale vertical velocity. Assumed in (m/s) at input.
!
      ELSE IF ((vcode(idfld).eq.'IDOMEG').or.                           &
     &         (vcode(idfld).eq.'IDWVEL').or.                           &
     &         (vcode(idfld).eq.'IDSMEK').or.                           &
     &         (vcode(idfld).eq.'IDBMEK').or.                           &
     &         (vcode(idfld).eq.'IDBWEK')) THEN
        IF (wvelscl.eq.0) THEN
          scale=c1
        ELSE IF (wvelscl.eq.1) THEN
          scale=day2sec
        ELSE IF (wvelscl.eq.2) THEN
          scale=m2cm*day2sec
        END IF
        DO k=1,iz
          DO j=1,iy
            DO i=1,ix
              IF (f(i,j,k).ne.spv) f(i,j,k)=scale*f(i,j,k)
            END DO
          END DO
        END DO
!
!  Scale horizontal velocity diagnostics.  Assumed in (m/s2) at input.
!
      ELSE IF                                                           &
     &   ((vcode(idfld).eq.'DU2DPG').or.(vcode(idfld).eq.'DV2DPG').or.  &
     &    (vcode(idfld).eq.'DM2PGV').or.(vcode(idfld).eq.'DM2PGM').or.  &
     &    (vcode(idfld).eq.'DM2PGS').or.(vcode(idfld).eq.'DM2PGB').or.  &
     &    (vcode(idfld).eq.'DU2DCO').or.(vcode(idfld).eq.'DV2DCO').or.  &
     &    (vcode(idfld).eq.'DM2COV').or.(vcode(idfld).eq.'DM2COM').or.  &
     &    (vcode(idfld).eq.'DM2COS').or.(vcode(idfld).eq.'DM2COB').or.  &
     &    (vcode(idfld).eq.'DU2DHA').or.(vcode(idfld).eq.'DV2DHA').or.  &
     &    (vcode(idfld).eq.'DM2HAV').or.(vcode(idfld).eq.'DM2HAM').or.  &
     &    (vcode(idfld).eq.'DM2HAS').or.(vcode(idfld).eq.'DM2HAB').or.  &
     &    (vcode(idfld).eq.'DU2DSS').or.(vcode(idfld).eq.'DV2DSS').or.  &
     &    (vcode(idfld).eq.'DM2SSV').or.(vcode(idfld).eq.'DM2SSM').or.  &
     &    (vcode(idfld).eq.'DM2SSS').or.(vcode(idfld).eq.'DM2SSB').or.  &
     &    (vcode(idfld).eq.'DU2DBS').or.(vcode(idfld).eq.'DV2DBS').or.  &
     &    (vcode(idfld).eq.'DM2BSV').or.(vcode(idfld).eq.'DM2BSM').or.  &
     &    (vcode(idfld).eq.'DM2BSS').or.(vcode(idfld).eq.'DM2BSB').or.  &
     &    (vcode(idfld).eq.'DU2DHV').or.(vcode(idfld).eq.'DV2DHV').or.  &
     &    (vcode(idfld).eq.'DM2HVV').or.(vcode(idfld).eq.'DM2HVM').or.  &
     &    (vcode(idfld).eq.'DM2HVS').or.(vcode(idfld).eq.'DM2HVB').or.  &
     &    (vcode(idfld).eq.'DU2DHR').or.(vcode(idfld).eq.'DV2DHR').or.  &
     &    (vcode(idfld).eq.'DM2HRV').or.(vcode(idfld).eq.'DM2HRM').or.  &
     &    (vcode(idfld).eq.'DM2HRS').or.(vcode(idfld).eq.'DM2HRB').or.  &
     &    (vcode(idfld).eq.'DU2DAC').or.(vcode(idfld).eq.'DV2DAC').or.  &
     &    (vcode(idfld).eq.'DM2ACV').or.(vcode(idfld).eq.'DM2ACM').or.  &
     &    (vcode(idfld).eq.'DM2ACS').or.(vcode(idfld).eq.'DM2ACB').or.  &
     &    (vcode(idfld).eq.'DU3DPG').or.(vcode(idfld).eq.'DV3DPG').or.  &
     &    (vcode(idfld).eq.'DM3PGV').or.(vcode(idfld).eq.'DM3PGM').or.  &
     &    (vcode(idfld).eq.'DM3PGS').or.(vcode(idfld).eq.'DM3PGB').or.  &
     &    (vcode(idfld).eq.'DU3DCO').or.(vcode(idfld).eq.'DV3DCO').or.  &
     &    (vcode(idfld).eq.'DM3COV').or.(vcode(idfld).eq.'DM3COM').or.  &
     &    (vcode(idfld).eq.'DM3COS').or.(vcode(idfld).eq.'DM3COB').or.  &
     &    (vcode(idfld).eq.'DU3DHA').or.(vcode(idfld).eq.'DV3DHA').or.  &
     &    (vcode(idfld).eq.'DM3HAV').or.(vcode(idfld).eq.'DM3HAM').or.  &
     &    (vcode(idfld).eq.'DM3HAS').or.(vcode(idfld).eq.'DM3HAB').or.  &
     &    (vcode(idfld).eq.'DU3DVA').or.(vcode(idfld).eq.'DV3DVA').or.  &
     &    (vcode(idfld).eq.'DM3VAV').or.(vcode(idfld).eq.'DM3VAM').or.  &
     &    (vcode(idfld).eq.'DM3VAS').or.(vcode(idfld).eq.'DM3VAB').or.  &
     &    (vcode(idfld).eq.'DU3DHV').or.(vcode(idfld).eq.'DV3DHV').or.  &
     &    (vcode(idfld).eq.'DM3HVV').or.(vcode(idfld).eq.'DM3HVM').or.  &
     &    (vcode(idfld).eq.'DM3HVS').or.(vcode(idfld).eq.'DM3HVB').or.  &
     &    (vcode(idfld).eq.'DU3DVV').or.(vcode(idfld).eq.'DV3DVV').or.  &
     &    (vcode(idfld).eq.'DM3VVV').or.(vcode(idfld).eq.'DM3VVM').or.  &
     &    (vcode(idfld).eq.'DM3VVS').or.(vcode(idfld).eq.'DM3VVB').or.  &
     &    (vcode(idfld).eq.'DU3DHR').or.(vcode(idfld).eq.'DV3DHR').or.  &
     &    (vcode(idfld).eq.'DM3HRV').or.(vcode(idfld).eq.'DM3HRM').or.  &
     &    (vcode(idfld).eq.'DM3HRS').or.(vcode(idfld).eq.'DM3HRB').or.  &
     &    (vcode(idfld).eq.'DU3DVR').or.(vcode(idfld).eq.'DV3DVR').or.  &
     &    (vcode(idfld).eq.'DM3VRV').or.(vcode(idfld).eq.'DM3VRM').or.  &
     &    (vcode(idfld).eq.'DM3VRS').or.(vcode(idfld).eq.'DM3VRB').or.  &
     &    (vcode(idfld).eq.'DU3DAC').or.(vcode(idfld).eq.'DV3DAC').or.  &
     &    (vcode(idfld).eq.'DM3ACV').or.(vcode(idfld).eq.'DM3ACM').or.  &
     &    (vcode(idfld).eq.'DM3ACS').or.(vcode(idfld).eq.'DM3ACB')) THEN
        IF (hvelscl.eq.0) THEN
          scale=c1
        ELSE IF (hvelscl.eq.1) THEN
          scale=m2cm
        ELSE IF (hvelscl.eq.2) THEN
          scale=m2mm
        ELSE IF (hvelscl.eq.3) THEN
          scale=-m2knot
        ELSE IF (hvelscl.eq.4) THEN
          scale=-m2smallknot
        END IF
        DO k=1,iz
          DO j=1,iy
            DO i=1,ix
              IF (f(i,j,k).ne.spv) f(i,j,k)=scale*f(i,j,k)
            END DO
          END DO
        END DO
!
!  If depth of oceanic boundary layer, fill non-computational points
!  with the closest value to the point.
!
      ELSE IF ((vcode(idfld).eq.'IDHSBL').or.                            &
     &         (vcode(idfld).eq.'IDHBBL')) THEN
        DO j=1,iy
          f(1, j,iz)=f(2   ,j,iz)
          f(ix,j,iz)=f(ix-1,j,iz)
        END DO
        DO i=1,ix
          f(i,1 ,iz)=f(i,2   ,iz)
          f(i,iy,iz)=f(i,iy-1,iz)
        END DO
!
!  If vertical viscosity/diffusion coefficients, fill non-computational
!  points with the closest value to the point.
!
      ELSE IF ((vcode(idfld).eq.'IDAKSC').or.                           &
     &         (vcode(idfld).eq.'IDAKTC').or.                           &
     &         (vcode(idfld).eq.'IDAKVC')) THEN
        IF (iz.gt.1) THEN
          DO j=1,iy
            DO i=1,ix
              f(i,j,iz)=f(i,j,iz-1)
            END DO
          END DO
        END IF
        DO k=1,iz
          DO j=1,iy
            f(1 ,j,k)=f(2   ,j,k)
            f(ix,j,k)=f(ix-1,j,k)
          END DO
          DO i=1,ix
            f(i,1 ,k)=f(i,2   ,k)
            f(i,iy,k)=f(i,iy-1,k)
          END DO
        END DO
!
!  Scale curvilinear transformation angle to degrees.
!
      ELSE IF (vcode(idfld).eq.'IDANGL') THEN
        scale=rad2deg
        DO k=1,iz
          DO j=1,iy
            DO i=1,ix
              IF (f(i,j,k).ne.spv) f(i,j,k)=scale*f(i,j,k)
            END DO
          END DO
        END DO
!
!  Scale grid-spacing from 1/m to km.
!
      ELSE IF ((vcode(idfld).eq.'IDDELX').or.                           &
     &         (vcode(idfld).eq.'IDDELY')) THEN
        scale=m2km
        DO k=1,iz
          DO j=1,iy
            DO i=1,ix
              IF (f(i,j,k).ne.spv) f(i,j,k)=scale/f(i,j,k)
            END DO
          END DO
        END DO
!
!  Scale transport streamfunction to Sverdrups.
!
      ELSE IF ((vcode(idfld).eq.'IDTPSI').or.                           &
     &         (vcode(idfld).eq.'IDNPYT')) THEN
        scale=1.0e-6
        DO k=1,iz
          DO j=1,iy
            DO i=1,ix
              IF (f(i,j,k).ne.spv) f(i,j,k)=scale*f(i,j,k)
            END DO
          END DO
        END DO
!
!  Scale surfac atmospheric pressure from Pascal to mbar.
!  (1 mbar = 100 Pa; 1 bar = 100000 Pa).
!
      ELSE IF (vcode(idfld).eq.'IDPAIR') THEN
        scale=0.01
        DO k=1,iz
          DO j=1,iy
            DO i=1,ix
              IF (f(i,j,k).ne.spv) f(i,j,k)=scale*f(i,j,k)
            END DO
          END DO
        END DO
      END IF
!
!-----------------------------------------------------------------------
!  Check for special fill value.
!-----------------------------------------------------------------------
!
      IF (ABS(spv).eq.999.0) THEN
        DO k=1,iz
          DO j=1,iy
            DO i=1,ix
              IF (f(i,j,k).eq.spv) f(i,j,k)=spval
            END DO
          END DO
        END DO
      END IF
!
!-----------------------------------------------------------------------
!  Apply Land/Sea mask, if any.
!-----------------------------------------------------------------------
!
      IF (gotmask.and.(igrid.gt.0)) THEN
!
!  Read in mask field from NetCDF file.
!
        IF ((igrid.eq.1).or.(igrid.eq.5).or.(igrid.eq.7)) THEN
          fldnam='mask_rho'
        ELSE IF (igrid.eq.2) THEN
          fldnam='mask_psi'
        ELSE IF (igrid.eq.3) THEN
          fldnam='mask_u'
        ELSE IF (igrid.eq.4) THEN
          fldnam='mask_v'
        ELSE
          fldnam='      '
        END IF
        CALL get_nc2dat (mask,NH,im,jm,ncid,ncid,0,fldnam,0,imgrid,     &
     &                   nc_vid,spvnc)
!
!  Mask 2D or 3D field, if appropriate.
!
        Vmin=spval1
        Vmax=-spval1
        IF ((im.eq.ix).and.(jm.eq.iy)) THEN
          DO k=1,iz
            DO j=1,iy
              DO i=1,ix
                ip=i+(j-1)*ix
                IF (mask(ip).lt.c1) THEN
                  f(i,j,k)=spval
                ELSE
                  Vmin=MIN(Vmin,f(i,j,k))
                  Vmax=MAX(Vmax,f(i,j,k))
                END IF
              END DO
            END DO
          END DO
        ELSE IF (igrid.eq.7) THEN
          DO k=1,iz
            DO j=1,iy
              DO i=1,ix
                ip=(i+1)+j*(ix+2)
                IF (mask(ip).lt.c1) THEN
                  f(i,j,k)=spval
                ELSE
                  Vmin=MIN(Vmin,f(i,j,k))
                  Vmax=MAX(Vmax,f(i,j,k))
                END IF
              END DO
            END DO
          END DO
        END IF
        PRINT 10, Vmin, Vmax
  10    FORMAT (t20,'Sea Points ',1x,'Min = ',1pe15.8,                  &
     &          ' Max = ',1pe15.8)
      END IF

      RETURN
      END
