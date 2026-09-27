      SUBROUTINE get_3dfld (day,xpos,ypos,zpos,s1,s2,np,igrid,file,     &
     &                      idfld,level)
!
!svn $Id: get_3dfld.F 833 2017-01-24 22:20:16Z arango $
!================================================== Hernan G. Arango ===
!  Copyright (c) 2002-2017 The ROMS/TOMS Group                         !
!    Licensed under a MIT/X style license                              !
!    See License_ROMS.txt                                              !
!=======================================================================
!                                                                      !
!  This routine reads in the requested 3D field from input NetCDF      !
!  file and interpolates at the requested depth.                       !
!                                                                      !
!  On Input:                                                           !
!                                                                      !
!     day      Day to read (real)                                      !
!     file     NetCDF file to read (primary or secondary; integer)     !
!     idfld    Identification of field to read (integer)               !
!     level    Depth (>0), level (>0), or isopycnal (>1000) to         !
!              process (real). If level=0, it process all available    !
!              levels.                                                 !
!                                                                      !
!  On Output:                                                          !
!                                                                      !
!     xpos     X-positions of the requested field (real array)         !
!     ypos     Y-positions of the requested field (real array)         !
!     ypos     Z-positions of the requested field (real array)         !
!     s1       Scalar or vector x-component field (real array)         !
!     s2       Vector y-component (real array)                         !
!     np       Number of points in requested field (integer)           !
!     igrid    Requested file staggered C-grid type:                   !
!                 igrid=0  => None.                                    !
!                 igrid=1  => density points.                          !
!                 igrid=2  => streamfunction points.                   !
!                 igrid=3  => u-velocity points.                       !
!                 igrid=4  => v-velocity points.                       !
!                 igrid=5  => w-velocity points.                       !
!                 igrid=6  => w-velocity points, range 0:N.            !
!                 igrid=7  => interior density points.                 !
!                                                                      !
!  Calls:                                                              !
!                                                                      !
!    crash, cvector, get_ape, get_bvf, get_ertel, get_nc2dat,          !
!    get_nc2pos, get_nc3dat, get_nc3pos, get_rig, idens, mskscl,       !
!    svel                                                              !
!                                                                      !
!                                                                      !
!=======================================================================
!
!-----------------------------------------------------------------------
!  Define global variables.
!-----------------------------------------------------------------------
!
      implicit none
#include "param.h"
#include "pconst.h"
#include "domdat.h"
#include "fields.h"
#include "fldsid.h"
#include "ncinfo.h"
#include "ndimen.h"
#include "pltfld.h"
#include "pltio.h"
#include "pltlab.h"
#include "pltncar.h"
!
!-----------------------------------------------------------------------
!  Define local data.
!-----------------------------------------------------------------------
!
      logical gotangle, gotrho, gotsalt, gotzeta
      integer file, idfld, igrid, i, ij, ijk, ixgrid, iygrid, imx, imy, &
     &        j, jmx, jmy, k, n, nc_fid, nc_gid, nc_vid, nc_vxid,       &
     &        nc_vyid, np, status
      real day, level, spv, xspv, yspv
      real s1(NV), s2(NV), xpos(NV), ypos(NV), zpos(NV)
      real idens, svel
      character*4 fldnam
!
!=======================================================================
!  Begin executable code.
!=======================================================================
!
!  Determine NetCDF ID for input file to read and time index of the
!  field to read.
!
      gotangle=.false.
      gotrho=.false.
      gotsalt=.false.
      gotzeta=.false.
      isosurf=.false.
      IF (file.eq.1) THEN
        nc_fid=ncid1
        IF (readgrd) THEN
          nc_gid=ncgrd
          gotangle=.true.
        ELSE
          nc_gid=ncid1
          gotangle=gotangle1
        END IF
        gotrho=gotrho1
        gotsalt=gotsalt1
        gotzeta=gotzeta1
        IF (Lrecord) THEN
          tindx=trecord
        ELSE
          CALL time_indx (var_time1,tindx,day,idfld,file)
        END IF
      ELSE IF (file.eq.2) THEN
        nc_fid=ncid2
        IF (readgrd) THEN
          nc_gid=ncgrd
          gotangle=.true.
        ELSE
          nc_gid=ncid2
          gotangle=gotangle2
        END IF
        gotrho=gotrho2
        gotsalt=gotsalt2
        gotzeta=gotzeta2
        IF (Lrecord) THEN
          tindx=trecord
        ELSE
          CALL time_indx (var_time2,tindx,day,idfld,file)
        END IF
      ELSE
        WRITE (stdout,10) file
  10    FORMAT (/' GET_2DFLD - unable to determine file type in input', &
     &          ' NetCDF file: ',i2)
        CALL crash ('GET_2DFLD',1)
      END IF
!
!-----------------------------------------------------------------------
!  Determine field(s) type and get field(s) name.
!-----------------------------------------------------------------------
!
      levless=is2d(idfld)
      status=vtype(idfld)
      fldnam1=vname1(idfld)
      fldnam2=vname2(idfld)
      fldlab=vlabel(idfld)
      ptype=0
!
!-----------------------------------------------------------------------
!  If reading data for cross-sections, and processing one vector
!  component, modify local variable "status" to read both components
!  so the vector component can be rotated normal to the cross-section.
!-----------------------------------------------------------------------
!
      IF (lrotvec.and.(plttyp.eq.secplt)) THEN
        IF ((vcode(idfld).eq.'IDUTOT').or.                              &
     &      (vcode(idfld).eq.'IDU3DD').or.                              &         
     &      (vcode(idfld).eq.'IDUCLI')) THEN
          status=2
          idfld=idfld+2
          fldnam1=vname1(idfld)
          fldnam2=vname2(idfld)
        ELSE IF ((vcode(idfld).eq.'IDVTOT').or.                         &
     &           (vcode(idfld).eq.'IDV3DD').or.                         &         
     &           (vcode(idfld).eq.'IDVCLI')) THEN
          status=2
          idfld=idfld+1
          fldnam1=vname1(idfld)
          fldnam2=vname2(idfld)
        END IF
      END IF
!
!=======================================================================
!  Process 3D fields.
!=======================================================================
!
      spv=spval1
      plotit=.false.
      IF (.not.levless) THEN
        plotit=.true.
!
!-----------------------------------------------------------------------
!  3D scalar field.
!-----------------------------------------------------------------------
!
        IF (status.eq.1) THEN
          CALL get_nc3dat (s1,NV,im,jm,km,nc_gid,nc_fid,tindx,fldnam1,  &
     &                     igrid,nc_vid,spv)
!
!  Baroclinic velocity components.
!
          IF (vcode(idfld).eq.'IDUCLI') THEN
            fldnam='ubar'
            CALL get_nc2dat (f5,NV,im,jm,nc_gid,nc_fid,tindx,fldnam,    &
     &                       level,ixgrid,nc_vxid,xspv)
            DO k=1,km
              DO j=1,jm
                DO i=1,im
                  ij=i+(j-1)*im
                  ijk=ij+(k-1)*im*jm
                  s1(ijk)=s1(ijk)-f5(ij)
                END DO
              END DO
            END DO
          ELSE IF (vcode(idfld).eq.'IDVCLI') THEN
            fldnam='vbar'
            CALL get_nc2dat (f5,NV,im,jm,nc_gid,nc_fid,tindx,fldnam,    &
     &                       level,ixgrid,nc_vxid,xspv)
            DO k=1,km
              DO j=1,jm
                DO i=1,im
                  ij=i+(j-1)*im
                  ijk=ij+(k-1)*im*jm
                  s1(ijk)=s1(ijk)-f5(ij)
                END DO
              END DO
            END DO
          END IF
          ptype=pscalar
          CALL mskscl (s1,im,jm,km,nc_gid,idfld,igrid,spv,spval1)
!
!-----------------------------------------------------------------------
!  3D vector field.
!-----------------------------------------------------------------------
!
        ELSE IF (status.eq.2) THEN
          CALL get_nc3dat (f5,NV,imx,jmx,km,nc_gid,nc_fid,tindx,fldnam1,&
     &                     ixgrid,nc_vxid,xspv)
          CALL get_nc3dat (f6,NV,imy,jmy,km,nc_gid,nc_fid,tindx,fldnam2,&
     &                     iygrid,nc_vyid,yspv)
!
!  Compute baroclinic velocity components.
!
          IF ((vcode(idfld).eq.'IDCMAG').or.                            &
     &        (vcode(idfld).eq.'IDCVEC')) THEN
            fldnam='ubar'
            CALL get_nc2dat (s1,NV,imx,jmx,nc_gid,nc_fid,tindx,fldnam,  &
     &                       level,ixgrid,nc_vxid,xspv)
            fldnam='vbar'
            CALL get_nc2dat (s2,NV,imy,jmy,nc_gid,nc_fid,tindx,fldnam,  &
     &                       level,iygrid,nc_vyid,yspv)
            DO k=1,km
              DO j=1,jmx
                DO i=1,imx
                  ij=i+(j-1)*imx
                  ijk=ij+(k-1)*imx*jmx
                  f5(ijk)=f5(ijk)-s1(ij)
                END DO
              END DO
              DO j=1,jmy
                DO i=1,imy
                  ij=i+(j-1)*imy
                  ijk=ij+(k-1)*imy*jmy
                  f6(ijk)=f6(ijk)-s2(ij)
                END DO
              END DO
            END DO
          END IF
!
!  Compute vector components at interior RHO-points or PSI-points.
!
          CALL mskscl (f5,imx,jmx,km,nc_gid,idfld,ixgrid,xspv,c0)
          CALL mskscl (f6,imy,jmy,km,nc_gid,idfld,iygrid,yspv,c0)
          IF ((ixgrid.eq.1).and.(iygrid.eq.1)) THEN
            igrid=1
          ELSE
            IF (lvecrho) THEN
              igrid=7
            ELSE
              igrid=2
            END IF
          END IF
          CALL cvector (s1,s2,im,jm,km,f5,imx,jmx,f6,imy,jmy,igrid,     &
     &                  nc_gid,gotangle)
!
!  Compute vector magnitude.
!
          IF ((vcode(idfld).eq.'IDTMAG').or.                            &
     &        (vcode(idfld).eq.'ID3MAG').or.                            &
     &        (vcode(idfld).eq.'IDCMAG').or.                            &
     &        (vcode(idfld).eq.'DM3PGM').or.                            &
     &        (vcode(idfld).eq.'DM3COM').or.                            &
     &        (vcode(idfld).eq.'DM3HAM').or.                            &
     &        (vcode(idfld).eq.'DM3VAM').or.                            &
     &        (vcode(idfld).eq.'DM3HVM').or.                            &
     &        (vcode(idfld).eq.'DM3VVM').or.                            &
     &        (vcode(idfld).eq.'DM3HRM').or.                            &
     &        (vcode(idfld).eq.'DM3VRM').or.                            &
     &        (vcode(idfld).eq.'DM3ACM')) THEN
            DO n=1,im*jm*km
              IF ((s1(n).ne.spv).and.(s2(n).ne.spv)) THEN
                s1(n)=SQRT(s1(n)*s1(n)+s2(n)*s2(n))
              ELSE
                s1(n)=spval1
              END IF
            END DO
            ptype=pscalar
          ELSE IF ((vcode(idfld).eq.'IDTSTR').or.                       &
     &             (vcode(idfld).eq.'ID3STR').or.                       &
     &             (vcode(idfld).eq.'IDCSTR').or.                       &
     &             (vcode(idfld).eq.'DM3PGS').or.                       &
     &             (vcode(idfld).eq.'DM3COS').or.                       &
     &             (vcode(idfld).eq.'DM3HAS').or.                       &
     &             (vcode(idfld).eq.'DM3VAS').or.                       &
     &             (vcode(idfld).eq.'DM3HVS').or.                       &
     &             (vcode(idfld).eq.'DM3VVS').or.                       &
     &             (vcode(idfld).eq.'DM3HRS').or.                       &
     &             (vcode(idfld).eq.'DM3VRS').or.                       &
     &             (vcode(idfld).eq.'DM3ACS')) THEN
            ptype=pstream
          ELSE IF ((vcode(idfld).eq.'IDTBRB').or.                       &
     &             (vcode(idfld).eq.'ID3BRB').or.                       &
     &             (vcode(idfld).eq.'IDCBRB').or.                       &
     &             (vcode(idfld).eq.'DM3PGB').or.                       &
     &             (vcode(idfld).eq.'DM3COB').or.                       &
     &             (vcode(idfld).eq.'DM3HAB').or.                       &
     &             (vcode(idfld).eq.'DM3VAB').or.                       &
     &             (vcode(idfld).eq.'DM3HVB').or.                       &
     &             (vcode(idfld).eq.'DM3VVB').or.                       &
     &             (vcode(idfld).eq.'DM3HRB').or.                       &
     &             (vcode(idfld).eq.'DM3VRB').or.                       &
     &             (vcode(idfld).eq.'DM3ACB')) THEN
            ptype=pwbarbs
          ELSE
            ptype=pvector
          END IF
!
!  Compute kinetic energy.
!
          IF (vcode(idfld).eq.'IDKENG') THEN
            DO n=1,im*jm*km
              IF ((s1(n).ne.spv).and.(s2(n).ne.spv)) THEN
                s1(n)=p5*(s1(n)*s1(n)+s2(n)*s2(n))/rho0
              ELSE
                s1(n)=spval1
              END IF
            END DO
            ptype=pscalar
          END IF
!
!-----------------------------------------------------------------------
!  Secondary field derived from two primary fields.
!-----------------------------------------------------------------------
!
        ELSE IF (status.eq.3) THEN
          CALL get_nc3dat (s1,NV,im,jm,km,nc_gid,nc_fid,tindx,fldnam1,  &
     &                     igrid,nc_vxid,xspv)
          CALL get_nc3dat (s2,NV,im,jm,km,nc_gid,nc_fid,tindx,fldnam2,  &
     &                     igrid,nc_vyid,yspv)
!
!  Compute density anomaly from temperature and salinity.
!
          IF (vcode(idfld).eq.'IDCDEN') THEN
            CALL get_nc3pos (z,NV,im,jm,km,nc_gid,nc_fid,tindx,igrid,   &
     &                       igrid,gotzeta,file)
            DO n=1,im*jm*km
              IF ((s1(n).ne.spv).and.(s2(n).ne.spv)) THEN
                s1(n)=idens(s1(n),s2(n),z(n))
              ELSE
                s1(n)=spval1
              END IF
            END DO
            ptype=pscalar
!
!  Compute sigma-t from temperature and salinity.
!
          ELSE IF (vcode(idfld).eq.'IDSIGT') THEN
             DO n=1,im*jm*km
              IF ((s1(n).ne.spv).and.(s2(n).ne.spv)) THEN
                s1(n)=idens(s1(n),s2(n),c0)
              ELSE
                s1(n)=spval1
              END IF
            END DO
            ptype=pscalar
!
!  Compute sound speed (m/s) from temperature and salinity.
!
          ELSE IF (vcode(idfld).eq.'IDSNDS') THEN
            CALL get_nc3pos (z,NV,im,jm,km,nc_gid,nc_fid,tindx,igrid,   &
     &                       igrid,gotzeta,file)
            DO n=1,im*jm*km
              IF ((s1(n).ne.spv).and.(s2(n).ne.spv)) THEN
                s1(n)=svel(s1(n),s2(n),ABS(z(n)))
              ELSE
                s1(n)=spval1
              END IF
            END DO
            ptype=pscalar
          END IF
          CALL mskscl (s1,im,jm,km,nc_gid,idfld,igrid,spv,spval1)
!
!-----------------------------------------------------------------------
!  Special fields.
!-----------------------------------------------------------------------
!
        ELSE IF ((status.eq.4).or.(status.eq.5)) THEN
!
!  Brunt-Vaisala frequency.
!
          IF (vcode(idfld).eq.'IDBVFQ') THEN
            CALL get_bvf (s1,xpos,ypos,zpos,im,jm,km,igrid,nc_gid,      &
     &                    nc_fid,idfld,tindx,level,gotzeta,gotsalt,     &
     &                    gotrho,file)
            ptype=pscalar
!
!  Horizontal divergence fields.
!
          ELSE IF ((vcode(idfld).eq.'IDTVHD').or.                       &
     &             (vcode(idfld).eq.'IDCVHD')) THEN
            CALL get_hdiv (s1,xpos,ypos,zpos,im,jm,km,igrid,nc_gid,     &
     &                     nc_fid,idfld,tindx,level,gotzeta,file)
            ptype=pscalar
!
!  Relative vorticity fields.
!
          ELSE IF (vcode(idfld).eq.'IDTVRV') THEN
            CALL get_rvor (s1,xpos,ypos,zpos,im,jm,km,igrid,nc_gid,     &
     &                     nc_fid,idfld,tindx,level,gotzeta,file)
            ptype=pscalar
!
!  Potential vorticity
!
          ELSE IF (vcode(idfld).eq.'IDPVOR') THEN
            CALL get_pv (s1,xpos,ypos,z,im,jm,km,igrid,nc_gid,nc_fid,   &
     &                   idfld,tindx,level,gotzeta,gotsalt,gotrho,file)
            ptype=pscalar
!
!  Ertel potential vorticity.
!
          ELSE IF (vcode(idfld).eq.'IDERPV') THEN
            CALL get_ertel (s1,xpos,ypos,zpos,im,jm,km,igrid,nc_gid,    &
     &                      nc_fid,idfld,tindx,level,gotzeta,file)
            ptype=pscalar
!
!  Available potential energy.
!
          ELSE IF (vcode(idfld).eq.'IDPENG') THEN
            CALL get_ape (s1,xpos,ypos,zpos,im,jm,km,igrid,nc_gid,      &
     &                    nc_fid,idfld,tindx,level,gotzeta,gotsalt,     &
     &                    gotrho,file)
            ptype=pscalar
!
!  Dynamic pressure.
!
          ELSE IF (vcode(idfld).eq.'IDDPRS') THEN
            CALL get_dpres (s1,xpos,ypos,z,im,jm,km,igrid,nc_gid,       &
     &                      nc_fid,idfld,tindx,level,gotzeta,gotsalt,   &
     &                      gotrho,file)
            ptype=pscalar
!
!  Bernoulli function.
!
          ELSE IF (vcode(idfld).eq.'IDBERN') THEN
            CALL get_bernoulli (s1,xpos,ypos,z,im,jm,km,igrid,nc_gid,   &
     &                          nc_fid,idfld,tindx,level,gotzeta,       &
     &                          gotsalt,gotrho,file)
            ptype=pscalar
!
!  Gradient Richardson number.
!
          ELSE IF (vcode(idfld).eq.'IDRIGN') THEN
            CALL get_rig (s1,xpos,ypos,zpos,im,jm,km,igrid,nc_gid,      &
     &                    nc_fid,idfld,tindx,level,gotzeta,gotsalt,     &
     &                    gotrho,file)
            ptype=pscalar
!
!  Total chlorophyll pigment concentration.
!
          ELSE IF (vcode(idfld).eq.'IDCOL1'.or.                         &
     &             vcode(idfld).eq.'IDCOL3'.or.                         &
     &             vcode(idfld).eq.'IDCOL4'.or.                         &
     &             vcode(idfld).eq.'IDCOL5') THEN
            CALL get_pig (s1,xpos,ypos,z,im,jm,km,igrid,nc_gid,nc_fid,  &
     &                    idfld,tindx,level,gotzeta,file)
            ptype=pscalar
          END IF
!
!  If appropriate, scale and apply Land/Sea mask.
!
          CALL mskscl (s1,im,jm,km,nc_gid,idfld,igrid,spv,spval1)
        ELSE
          plotit=.false.
        END IF
!
!-----------------------------------------------------------------------
!  Read in field depths and horizontal positions.
!-----------------------------------------------------------------------
!
        IF ((1.le.status).and.(status.le.3)) THEN
          CALL get_nc3pos (zpos,NV,im,jm,km,nc_gid,nc_fid,tindx,igrid,  &
     &                     igrid,gotzeta,file)
          CALL get_nc2pos (xpos,ypos,NV,im,jm,nc_gid,igrid)
        END IF
!
!  Set number of points in requested field.
!
        np=im*jm*km
      END IF

      RETURN
      END
