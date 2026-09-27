      SUBROUTINE get_nc3pos (zpos,np,im,jm,km,ncgid,ncfid,tindex,       &
     &                       ihgrid,ivgrid,gotzeta,file)
!
!svn $Id: get_nc3pos.F 833 2017-01-24 22:20:16Z arango $
!================================================== Hernan G. Arango ===
!  Copyright (c) 2002-2017 The ROMS/TOMS Group                         !
!    Licensed under a MIT/X style license                              !
!    See License_ROMS.txt                                              !
!=======================================================================
!                                                                      !
!  This routine reads in the depths of the current 2D or 3D field.     !
!                                                                      !
!  On Input:                                                           !
!                                                                      !
!     np       Allocated size for array "zpos" (integer).              !
!     im       Number of inner dimension points to read (integer).     !
!     jm       Number of inner dimension points to read (integer).     !
!     km       Number of vertical levels (integer).                    !
!     ncgid    NetCDF ID for input grid (integer).                     !
!     ncfid    NetCDF ID for input fields (integer).                   !
!     tindex   Time dimension index to read (integer).                 !
!     ihgrid   Horizontal staggered C-grid type (integer):             !
!                ihgrid=0  => None.                                    !
!                ihgrid=1  => density points.                          !
!                ihgrid=2  => streamfunction points.                   !
!                ihgrid=3  => u-velocity points.                       !
!                ihgrid=4  => v-velocity points.                       !
!                ihgrid=7  => interior density points.                 !
!     ivgrid   Vertical staggered grid type (integer):                 !
!                ivgrid=1  => density points.                          !
!                ivgrid=5  => w-velocity points, range 1:N.            !
!                ivgrid=6  => w-velocity points, range 0:N.            !
!     gotzeta  Logical switch indicating availability of free-         !
!              surface.                                                !
!     file     NetCDF file to read (primary or secondary; integer).    !
!                                                                      !
!  On Output:                                                          !
!                                                                      !
!     zpos     Z-positions (depths) of current field.                  !
!                                                                      !
!  Calls:                                                              !
!                                                                      !
!     ncdinq, ncvid, ncvinq (NetCDF library)                           !
!     crash, lenstr, ncvget, zgrid                                     !
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
#include "domdat.h"
#include "fields.h"
#include "pltfld.h"
#include "pltncar.h"
#include "ncinfo.h"
#include "netcdf.inc"
#include "secbath.h"
!
!-----------------------------------------------------------------------
!  Define local data.
!-----------------------------------------------------------------------
!
      logical gotzeta, gotsrho
      integer file, i, igrid, ij, ihgrid, ijk, im, ixy, ivgrid, j, jm,  &
     &        jxy, k, km, kz, lvar, nccid, ncfid, ncgid, nchid, ncsid,  &
     &        nczid, ndims, ngatts, np, nvatts, recdim, status, tindex
      integer lenstr
      real cspv, hc, hspv, scle, sspv, zspv, z0
      real Cs(NK), sc(NK), zpos(np)
      character*60  cposnam, hposnam, sposnam, zposnam
!
!=======================================================================
!  Begin Executable code.
!=======================================================================
!
!-----------------------------------------------------------------------
!  Set Depths for flat coordinates data.  It is assumed that k=1 is the
!  bottom and k=km is the surface, as in the ocean model.
!-----------------------------------------------------------------------
!
      IF ((file.eq.1).and.(type1(1:2).eq.'OA')) THEN
        ixy=Lr
        jxy=Mr
        ijk=0
        DO k=1,km
          DO j=1,jxy
            DO i=1,ixy
              ijk=ijk+1
              f6(ijk)=zinp1(k)
            END DO
          END DO
        END DO
        ij=0
        DO j=1,jxy
          DO i=1,ixy
            ij=ij+1
            bath(ij)=abs(zinp1(1))
          END DO
        END DO
      ELSE IF ((file.eq.2).and.(type2(1:2).eq.'OA')) THEN
        ixy=Lr
        jxy=Mr
        ijk=0
        DO k=1,km
          DO j=1,jxy
            DO i=1,ixy
              ijk=ijk+1
              f6(ijk)=zinp2(k)
            END DO
          END DO
        END DO
        ij=0
        DO j=1,jxy
          DO i=1,ixy
            ij=ij+1
            bath(ij)=abs(zinp2(1))
          END DO
        END DO
!
!-----------------------------------------------------------------------
!  Set Depths for terrain-following coordinates data.
!-----------------------------------------------------------------------
!
      ELSE
!
!  Get free-surface (m), store in utility array "f5".
!
        IF (.not.readgrd.and.gotzeta) THEN
          zposnam='zeta'
          CALL get_nc2dat (f5,NV,ixy,jxy,ncgid,ncfid,tindex,zposnam,0,  &
     &                     igrid,nczid,zspv)
        END IF
!
!  Read in bottom topography.
!
        hposnam='h'
        CALL get_nc2dat (bath,NH,ixy,jxy,ncgid,ncgid,tindex,hposnam,0,  &
     &                   igrid,nchid,hspv)
!
!  Read in nondimensional S-coordinate and stretching curves. The
!  variables "sc_w" and "sc_r" were renamed in newer versions to
!  "s_w" and "s_rho" to conform with CF NetCDF convections.
!
        gotsrho=.false.
        status=nf_inq(ncfid,ndims,nvars,ngatts,recdim)
        IF (status.eq.nf_noerr) THEN
          DO i=1,nvars
            varid=i
            status=nf_inq_var(ncfid,varid,varnam(i),vartyp,nvdims(i),   &
     &                        vdims(1,i),nvatts)
            IF (status.ne.nf_noerr) THEN
              PRINT 10, varid
  10          FORMAT (/' GET_NC3POS - cannot inquire variable ID: ',i4)
              CALL crash ('GET_NC3POS',status)
            END IF
            lvar=lenstr(varnam(i))
            IF (varnam(i)(1:lvar).eq.'s_rho') THEN
              gotsrho=.true.
            END IF
          END DO
        ELSE
          PRINT 20, 'sc_r or s_rho'
  20      FORMAT (/' GET_NC3POS - unable to inquire for variables: ',a)
          CALL crash ('GET_NC3POS',status)
        END IF
!
        IF ((ivgrid.eq.5).or.(ivgrid.eq.6)) THEN
          IF (gotsrho) THEN
            sposnam='s_w'
          ELSE
            sposnam='sc_w'
          END IF
          cposnam='Cs_w'
        ELSE
          IF (gotsrho) THEN
            sposnam='s_rho'
          ELSE
            sposnam='sc_r'
          END IF
          cposnam='Cs_r'
        END IF
        CALL get_nc1dat (sc,NK,kz,ncfid,tindex,sposnam,ncsid,sspv)
        CALL get_nc1dat (Cs,NK,kz,ncfid,tindex,cposnam,nccid,cspv)
!
!  Get full range (0:N) of parameters at W-points.  The history file
!  does not store fields at level 0.  The value of "sc_w" and "Cs_w"
!  is always -1.0 at level 0.  This step is avoided if the input file
!  has the full range values.
!
        IF (ivgrid.eq.6) THEN
          IF (kz.eq.(km-1)) THEN
            DO k=kz,1,-1
              sc(k+1)=sc(k)
              Cs(k+1)=Cs(k)
            END DO
            sc(1)=-c1
            Cs(1)=-c1
          END IF
        END IF
!
!  Read in S-coordinate parameter, "hc".
!
        status=nf_inq_varid(ncfid,'hc',varid)
        IF (status.ne.nf_noerr) THEN
          PRINT 30, 'hc'
  30      FORMAT (/' GET_NC3POS - cannot find position variable: ',a)
          CALL crash ('GET_NC3POS',1)
        END IF
        status=nf_get_var1_real(ncfid,varid,0,hc)
!
!-----------------------------------------------------------------------
!  Compute depths for current field at the requested vertical grid.
!-----------------------------------------------------------------------
!
        scle=c1                                ! meters
        IF (coorscl.eq.2) THEN
          scle=m2cm                            ! meters to centimeters
        ELSE IF (coorscl.eq.3) THEN
          scle=m2mm                            ! meters to milimeters
        END IF
        IF (readgrd.or.nesting) THEN
          DO j=1,jxy
            DO i=1,ixy
              ij=i+(j-1)*ixy
              f5(ij)=c0
            END DO
          END DO
        END IF

        IF (((file.eq.1).and.(Vtransform1.eq.1)).or.                    &
     &      ((file.eq.2).and.(Vtransform2.eq.1))) THEN
          DO k=1,km
            DO j=1,jxy
              DO i=1,ixy
                ij=i+(j-1)*ixy
                ijk=ij+(k-1)*ixy*jxy
                z0=hc*(sc(k)-Cs(k))+Cs(k)*bath(ij)
                f6(ijk)=z0+f5(ij)*(c1+z0/bath(ij))
                f6(ijk)=scle*f6(ijk)
              END DO
            END DO
          END DO
        ELSE IF (((file.eq.1).and.(Vtransform1.eq.2)).or.               &
     &           ((file.eq.2).and.(Vtransform2.eq.2))) THEN
          DO k=1,km
            DO j=1,jxy
              DO i=1,ixy
                ij=i+(j-1)*ixy
                ijk=ij+(k-1)*ixy*jxy
                z0=(hc*sc(k)+bath(ij)*Cs(k))/(hc+bath(ij))
                f6(ijk)=f5(ij)+(f5(ij)+bath(ij))*z0
                f6(ijk)=scle*f6(ijk)
              END DO
            END DO
          END DO
        END IF
!
!  If plotting cross-sections and requested, reset surface depth to
!  zero to avoid uneven surface section.  This is an aesthetic
!  approximation.
!
        IF ((plttyp.eq.secplt).and.lzerozn) THEN
          DO j=1,jxy
            DO i=1,ixy
              ij=i+(j-1)*ixy
              ijk=ij+(km-1)*ixy*jxy
              f6(ijk)=c0
            END DO
          END DO
        END IF
      END IF
!
!-----------------------------------------------------------------------
!  Compute depths at the requested horizontal positions.
!-----------------------------------------------------------------------
!
      CALL zgrid (zpos,im,jm,km,f6,ixy,jxy,km,ihgrid)
!
!-----------------------------------------------------------------------
!  Compute bathymetry at the requeted horizontal positions.
!-----------------------------------------------------------------------
!
      DO j=1,jxy
        DO i=1,ixy
          ij=i+(j-1)*ixy
          f5(ij)=scle*bath(ij)
        END DO
      END DO
      CALL zgrid (bath,im,jm,1,f5,ixy,jxy,1,ihgrid)

      RETURN
      END
