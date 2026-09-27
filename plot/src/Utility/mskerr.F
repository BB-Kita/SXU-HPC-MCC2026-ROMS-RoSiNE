      SUBROUTINE mskerr (f,ix,iy,iz,file,ncgid,ncfid,tindex,level,iderr,&
     &                   igrid,errmax,spv,spval)
!
!svn $Id: mskerr.F 833 2017-01-24 22:20:16Z arango $
!================================================== Hernan G. Arango ===
!  Copyright (c) 2002-2017 The ROMS/TOMS Group                         !
!    Licensed under a MIT/X style license                              !
!    See License_ROMS.txt                                              !
!=======================================================================
!                                                                      !
!  This routine masks input OA fields by its maximum error variance    !
!  tolerance.                                                          !
!                                                                      !
!  On Input:                                                           !
!                                                                      !
!     f        Field to scale and/or mask (real array).                !
!     ix       Size of inner dimension (integer).                      !
!     iy       Size of outer dimension (integer).                      !
!     iz       Number of vertical levels (integer).                    !
!     file     NetCDF file to read (primary or secondary; integer)     !
!     ncgid    NetCDF ID for input GRID file (integer).                !
!     ncfid    NetCDF ID for input field file (integer).               !
!     tindx    Time dimension index to read (integer).                 !
!     level    Depth (>0), level (>0), or isopycnal (>1000) to         !
!              process (real).                                         !
!     iderr    Identification of error field to read (integer).        !
!     igrid    Staggered C grid type (integer):                        !
!                igrid=0  => None.                                     !
!                igrid=1  => density points.                           !
!                igrid=2  => streamfunction points.                    !
!                igrid=3  => u-velocity points.                        !
!                igrid=4  => v-velocity points.                        !
!                igrid=5  => w-velocity points.                        !
!     errmax   Maximum error tolerance (fractional percentage: 0-1)    !
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
#include "fields.h"
#include "fldsid.h"
#include "ncinfo.h"
!
!-----------------------------------------------------------------------
!  Define local data.
!-----------------------------------------------------------------------
!
      logical errmsk, levless
      integer file, i, iderr, iegrid, igrid, im, ip, ix, iy, iz, j, jm, &
     &        k, level, ncfid, ncgid, nc_vid, tindex
      real errmax, spv, spverr, spval
      real f(ix,iy,iz)
      character*20 fldnam
!
!=======================================================================
!  Begin executable code.
!=======================================================================
!
      errmsk=.false.
      IF (file.eq.1) THEN
        IF (type1(1:2).eq.'OA') THEN
          errmsk=.true.
        END IF
      ELSE IF (file.eq.2) THEN
        IF (type2(1:2).eq.'OA') THEN
          errmsk=.true.
        END IF
      END IF
!
!-----------------------------------------------------------------------
!  Mask field by its error variance.
!-----------------------------------------------------------------------
!
      IF (errmsk) THEN
!
!  Read in error field.
!
        levless=is2d(iderr)
        fldnam=vname1(iderr)
        IF (levless) THEN
          CALL get_nc2dat (f6,NV,im,jm,ncgid,ncfid,tindex,fldnam,level, &
     &                     iegrid,nc_vid,spverr)
          CALL cfield (f5,ix,iy,1,f6,im,jm,1,igrid)
        END IF
!
!  Mask by maximum error tolerance.
!
        DO k=1,iz
          DO j=1,iy
            DO i=1,ix
              ip=i+(j-1)*ix+(k-1)*ix*iy
              IF (f5(ip).ge.errmax) THEN
                f(i,j,k)=spval
              END IF
            END DO
          END DO
        END DO
      END IF

      RETURN
      END

