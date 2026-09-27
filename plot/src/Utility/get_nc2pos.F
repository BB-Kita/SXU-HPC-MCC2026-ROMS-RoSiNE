      SUBROUTINE get_nc2pos (xpos,ypos,np,im,jm,ncid,igrid)
!
!svn $Id: get_nc2pos.F 833 2017-01-24 22:20:16Z arango $
!================================================== Hernan G. Arango ===
!  Copyright (c) 2002-2017 The ROMS/TOMS Group                         !
!    Licensed under a MIT/X style license                              !
!    See License_ROMS.txt                                              !
!=======================================================================
!                                                                      !
!  This routine reads in the 2D horizontal positions for the current   !
!  field.                                                              !
!                                                                      !
!  On Input:                                                           !
!                                                                      !
!     np       Allocated size for arrays "xpos" and "ypos"             !
!              (integer).                                              !
!     im       Number of inner dimension points to read (integer).     !
!     jm       Number of inner dimension points to read (integer).     !
!     ncid     NetCDF ID for input file (integer).                     !
!     igrid    Staggered C grid type (integer):                        !
!                igrid=0  => None.                                     !
!                igrid=1  => density points.                           !
!                igrid=2  => streamfunction points.                    !
!                igrid=3  => u-velocity points.                        !
!                igrid=4  => v-velocity points.                        !
!                igrid=5  => w-velocity points.                        !
!                igrid=6  => w-velocity points, range 0:N.             !
!                igrid=7  => interior density points.                  !
!                                                                      !
!  On Output:                                                          !
!                                                                      !
!     xpos     X-positions of current field (metric or degrees).       !
!     ypos     Y-positions of current field (metric or degrees).       !
!                                                                      !
!  Calls:                                                              !
!                                                                      !
!     NetCDF library                                                   !
!     cgrid, get_nc3dat, lenstr                                        !
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
#include "fields.h"
#include "netcdf.inc"
#include "pltfld.h"
#include "pltio.h"
#include "pltncar.h"
!
!-----------------------------------------------------------------------
!  Define local data.
!-----------------------------------------------------------------------
!
      logical average, derive, gotlat, gotlon, gotplat, gotplon, gotpx, &
     &        gotpy, gotrlat, gotrlon, gotrx, gotry, gotulat, gotulon,  &
     &        gotux, gotuy, gotvlat, gotvlon, gotvx, gotvy
      integer ggrid, i, igrid, im, ip, ixy, j, jm, jxy, lvar, n, ncid,  &
     &        ndims, ngatts, np, nvars, nvatts, nvdims, recdim, status, &
     &        varid, vartyp, xposid, yposid
      integer vdims(10)
      integer lenstr
      real xyspv, scle
      real xpos(np), ypos(np)
      character*60  varnam, xposnam, yposnam
!
!=======================================================================
!  Begin Executable code.
!=======================================================================
!
!-----------------------------------------------------------------------
!  Inquire about position variables.
!-----------------------------------------------------------------------
!
      derive=.false.
      average=.false.
      gotlon=.false.
      gotlat=.false.
      gotrlon=.false.
      gotrlat=.false.
      gotplon=.false.
      gotplat=.false.
      gotulon=.false.
      gotulat=.false.
      gotvlon=.false.
      gotvlat=.false.
      gotrx=.false.
      gotry=.false.
      gotpx=.false.
      gotpy=.false.
      gotux=.false.
      gotuy=.false.
      gotvx=.false.
      gotvy=.false.
!
      status=nf_inq(ncid,ndims,nvars,ngatts,recdim)
      IF (status.eq.nf_noerr) THEN
        DO i=1,nvars
          varid=i
          status=nf_inq_var(ncid,varid,varnam,vartyp,nvdims,vdims,      &
     &                      nvatts)
          IF (status.ne.nf_noerr) THEN
            WRITE (stdout,10) varid, ncid
            CALL crash ('GET_NC2POS',status)
          END IF
          lvar=lenstr(varnam)
          IF (varnam(1:lvar).eq.'lon') THEN
            gotlon=.true.
          ELSE IF (varnam(1:lvar).eq.'lat') THEN
            gotlat=.true.
          ELSE IF (varnam(1:lvar).eq.'lon_rho') THEN
            gotrlon=.true.
          ELSE IF (varnam(1:lvar).eq.'lat_rho') THEN
            gotrlat=.true.
          ELSE IF (varnam(1:lvar).eq.'lon_psi') THEN
            gotplon=.true.
          ELSE IF (varnam(1:lvar).eq.'lat_psi') THEN
            gotplat=.true.
          ELSE IF (varnam(1:lvar).eq.'lon_u') THEN
            gotulon=.true.
          ELSE IF (varnam(1:lvar).eq.'lat_u') THEN
            gotulat=.true.
          ELSE IF (varnam(1:lvar).eq.'lon_v') THEN
            gotvlon=.true.
          ELSE IF (varnam(1:lvar).eq.'lat_v') THEN
            gotvlat=.true.
          ELSE IF (varnam(1:lvar).eq.'x_rho') THEN
            gotrx=.true.
          ELSE IF (varnam(1:lvar).eq.'y_rho') THEN
            gotry=.true.
          ELSE IF (varnam(1:lvar).eq.'x_psi') THEN
            gotpx=.true.
          ELSE IF (varnam(1:lvar).eq.'y_psi') THEN
            gotpy=.true.
          ELSE IF (varnam(1:lvar).eq.'x_u') THEN
            gotux=.true.
          ELSE IF (varnam(1:lvar).eq.'y_u') THEN
            gotuy=.true.
          ELSE IF (varnam(1:lvar).eq.'x_v') THEN
            gotvx=.true.
          ELSE IF (varnam(1:lvar).eq.'y_v') THEN
            gotvy=.true.
          END IF
        END DO
      ELSE
        WRITE (stdout,20) ncid
        CALL crash ('GET_NC2POS',status)
      END IF
!
!-----------------------------------------------------------------------
!  Read in spherical grid.
!-----------------------------------------------------------------------
!
      IF (spherical) THEN
!
!  Read in longitude and latitude at RHO-points.
!
        IF (gotlon) THEN
          xposnam='lon'
        ELSE IF (gotplon.and.(igrid.eq.2)) THEN
          xposnam='lon_psi'
        ELSE IF (gotulon.and.(igrid.eq.3)) THEN
          xposnam='lon_u'
        ELSE IF (gotvlon.and.(igrid.eq.4)) THEN
          xposnam='lon_v'
        ELSE
          xposnam='lon_rho'
          average=.true.
        END IF
        IF (gotlat) THEN
          yposnam='lat'
        ELSE IF (gotplat.and.(igrid.eq.2)) THEN
          yposnam='lat_psi'
        ELSE IF (gotulat.and.(igrid.eq.3)) THEN
          yposnam='lat_u'
        ELSE IF (gotvlat.and.(igrid.eq.4)) THEN
          yposnam='lat_v'
        ELSE
          yposnam='lat_rho'
          average=.true.
        END IF
        CALL get_nc2dat (f5,NV,ixy,jxy,ncid,ncid,0,xposnam,0,ggrid,     &
     &                   xposid,xyspv)
        CALL get_nc2dat (f6,NV,ixy,jxy,ncid,ncid,0,yposnam,0,ggrid,     &
     &                   yposid,xyspv)
!
!  If applicable, calculate spherical grid at the requested positions.
!
        IF (average) THEN
          CALL get_nc2dat (f5,NV,ixy,jxy,ncid,ncid,0,xposnam,0,ggrid,   &
     &                     xposid,xyspv)
          CALL get_nc2dat (f6,NV,ixy,jxy,ncid,ncid,0,yposnam,0,ggrid,   &
     &                     yposid,xyspv)
          CALL cgrid (xpos,ypos,im,jm,f5,f6,ixy,jxy,igrid,derive)
        ELSE
          CALL get_nc2dat (xpos,NV,ixy,jxy,ncid,ncid,0,xposnam,0,ggrid, &
     &                     xposid,xyspv)
          CALL get_nc2dat (ypos,NV,ixy,jxy,ncid,ncid,0,yposnam,0,ggrid, &
     &                     yposid,xyspv)
        END IF
!
!  Convert longitudes to EAST-longitudes.
!
        IF (eastlon) THEN
          DO j=1,jm
            DO i=1,im
              ip=i+(j-1)*im
              IF (xpos(ip).lt.c0) xpos(ip)=c360+xpos(ip)
            END DO
          END DO
        END IF
      ELSE
!
!-----------------------------------------------------------------------
!  Read in Cartesian grid.
!-----------------------------------------------------------------------
!
!  Read in curvilinear metrics.
!
        IF (gotpx.and.(igrid.eq.2)) THEN
          xposnam='x_psi'
        ELSE IF (gotux.and.(igrid.eq.3)) THEN
          xposnam='x_u'
        ELSE IF (gotvx.and.(igrid.eq.4)) THEN
          xposnam='x_v'
        ELSE IF (gotrx) THEN
          xposnam='x_rho'
          average=.true.
        ELSE
          xposnam='pm'
          derive=.true.
          average=.true.
        END IF
        IF (gotpy.and.(igrid.eq.2)) THEN
          yposnam='y_psi'
        ELSE IF (gotuy.and.(igrid.eq.3)) THEN
          yposnam='y_u'
        ELSE IF (gotvy.and.(igrid.eq.4)) THEN
          yposnam='y_v'
        ELSE IF (gotry) THEN
          yposnam='y_rho'
          average=.true.
        ELSE
          yposnam='pn'
          derive=.true.
          average=.true.
        END IF
!
!  Compute positions at requested grid from curvilinear metrics.
!
        IF (average) THEN
          CALL get_nc2dat (f5,NV,ixy,jxy,ncid,ncid,0,xposnam,0,ggrid,   &
     &                     xposid,xyspv)
          CALL get_nc2dat (f6,NV,ixy,jxy,ncid,ncid,0,yposnam,0,ggrid,   &
     &                     yposid,xyspv)
          CALL cgrid (xpos,ypos,im,jm,f5,f6,ixy,jxy,igrid,derive)
        ELSE
          CALL get_nc2dat (xpos,NV,ixy,jxy,ncid,ncid,0,xposnam,0,ggrid, &
     &                     xposid,xyspv)
          CALL get_nc2dat (ypos,NV,ixy,jxy,ncid,ncid,0,yposnam,0,ggrid, &
     &                     yposid,xyspv)
        END IF
!
!  Convert Cartesian grid to requested units.
!
        scle=c1                            ! meters
        IF (coorscl.eq.0) THEN
          scle=m2km                        ! meters to kilometers
        ELSE IF (coorscl.eq.2) THEN
          scle=m2cm                        ! meters to centimeters
        ELSE IF (coorscl.eq.3) THEN
          scle=m2mm                        ! meters to milimeters
        END IF
        DO n=1,im*jm
          xpos(n)=xpos(n)*scle
          ypos(n)=ypos(n)*scle
        END DO
      END IF
!
  10  FORMAT (/,' GET_NC2POS - error while inquiring information for ', &
     &          ' variable ID: ',i3,2x,' in input NetCDF ID: ',i3)
  20  FORMAT (/,' GET_NC2POS - unable to inquire about contents of',    &
     &          ' input NetCDF ID: ',i3)

      RETURN
      END
