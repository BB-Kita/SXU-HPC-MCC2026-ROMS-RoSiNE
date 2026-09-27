      SUBROUTINE get_pig (chl,xpos,ypos,zpos,im,jm,km,igrid,ncgid,      &
     &                    ncfid, idfld,tindex,level,gotzeta,file)
!
!svn $Id: get_pig.F 833 2017-01-24 22:20:16Z arango $
!================================================== Hernan G. Arango ===
!  Copyright (c) 2002-2017 The ROMS/TOMS Group                         !
!    Licensed under a MIT/X style license                              !
!    See License_ROMS.txt                                              !
!=======================================================================
!                                                                      !
!  This subroutine computes total pigment concentration as the sum     !
!  of contributuions from the phytoplankton constituents.              !
!                                                                      !
!  On Input:                                                           !
!                                                                      !
!     ncgid    NetCDF ID for input grid (integer).                     !
!     ncfid    NetCDF ID for input fields (integer).                   !
!     idfld    Identification of field to read (integer).              !
!     tindex   Time dimension index to read (integer).                 !
!     level    Depth (>0), level (>0), or isopycnal (>1000) to         !
!              process (real). If LEVEL=0, the full 3D is passed.      !
!     gotzeta  Logical switch indicating availability of free-         !
!              surface.                                                !
!     file     NetCDF file to read (primary or secondary; integer).    !
!                                                                      !
!  On Output:                                                          !
!                                                                      !
!     chl      Total pigment concentration (real array).               !
!     im       Size of inner dimension (integer).                      !
!     jm       Size of outer dimension (integer).                      !
!     km       Size of vertical dimension (integer).                   !
!     igrid    Staggered C-grid type for requested field (integer).    !
!     xpos     X-positions of the requested field (real array).        !
!     ypos     Y-positions of the requested field (real array).        !
!     zpos     Z-positions of the requested field (real array).        !
!                                                                      !
!  Calls:    get_nc3dat, get_nc2pos, get_nc3pos                        !
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
#include "pltfld.h"
#include "pltlab.h"
!
!-----------------------------------------------------------------------
!  Define local variables.
!-----------------------------------------------------------------------
!
      logical gotzeta
      integer file, i, idfld, igrid, im, imr, ip, ipig, irgrid, j, jm,  &
     &        jmr, k, km, kmr, kr, ncfid, ncgid, nc_rid, npig, tindex
      real level, rspv
      real chl(NV), xpos(NV), ypos(NV), zpos(NV)
      character*20 fldname(7)
!
!=======================================================================
!  Begin executable code.
!=======================================================================
!
!  Set approprite pigments to process.
!
      IF (vcode(idfld).eq.'IDCOL1') THEN
        npig=4
        fldname(1)='Pigs11'
        fldname(2)='Pigs21'
        fldname(3)='Pigs31'
        fldname(4)='Pigs41'
      ELSE IF (vcode(idfld).eq.'IDCOL3') THEN
        npig=3
        fldname(1)='Pigs13'
        fldname(2)='Pigs23'
        fldname(3)='Pigs33'
      ELSE IF (vcode(idfld).eq.'IDCOL4') THEN
        npig=3
        fldname(1)='Pigs14'
        fldname(2)='Pigs24'
        fldname(3)='Pigs34'
      ELSE IF (vcode(idfld).eq.'IDCOL5') THEN
        npig=4
        fldname(1)='Pigs15'
        fldname(2)='Pigs25'
        fldname(3)='Pigs35'
      END IF
!
!  Read and accumulate requested pigment concentration.
!
      CALL get_nc3dat (chl,NV,imr,jmr,kmr,ncgid,ncfid,tindex,           &
     &                 fldname(1),irgrid,nc_rid,rspv)
      DO ipig=2,npig
        CALL get_nc3dat (f2,NV,imr,jmr,kmr,ncgid,ncfid,tindex,          &
     &                   fldname(ipig),irgrid,nc_rid,rspv)
        ip=0
        DO k=1,kmr
          DO j=1,jmr
            DO i=1,imr
              ip=ip+1
              chl(ip)=chl(ip)+f2(ip)
            END DO
          END DO
        END DO
      END DO
!
!  Get horizontal positions at RHO-points
!
      im=imr
      jm=jmr
      km=Nr
      igrid=1
      CALL get_nc2pos (xpos,ypos,NV,imr,jmr,ncgid,igrid)
!
!  Get vertical positions at RHO-points.
!
      IF ((level.le.c0).or.(level.ge.1000)) THEN
        irgrid=1
        CALL get_nc3pos (zpos,NV,imr,jmr,Nr,ncgid,ncfid,tindex,igrid,   &
     &                   irgrid,gotzeta,file)
      END IF
!
!  If appropriate, extract total pigment at the requested level.
!
      IF ((1.le.int(level)).and.(int(level).le.km)) THEN
        DO ip=1,im*jm*km
          f2(ip)=chl(ip)
        END DO
        ip=0
        DO j=1,jm
         DO i=1,im
            ip=ip+1
            kr=i+(j-1)*im+(int(level)-1)*im*jm
            chl(ip)=f2(kr)
          END DO
        END DO
      END IF

      RETURN
      END
