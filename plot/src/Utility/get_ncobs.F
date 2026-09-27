      SUBROUTINE get_ncobs (f,np,im,jm,ncid,tindex,fldname,level,igrid, &
     &                      ncvarid,spv)
!
!svn $Id: get_ncobs.F 833 2017-01-24 22:20:16Z arango $
!================================================== Hernan G. Arango ===
!  Copyright (c) 2002-2017 The ROMS/TOMS Group                         !
!    Licensed under a MIT/X style license                              !
!    See License_ROMS.txt                                              !
!=======================================================================
!                                                                      !
!  This routine reads requested observation field from input NetCDF    !
!  file.  The field maybe of "full" grid size or sampled from grid.    !
!  There is not two independent horizontal dimensions.                 !
!                                                                      !
!  On Input:                                                           !
!                                                                      !
!     np       Allocated size for array "f".                           !
!     ncid     NetCDF ID for input file (integer).                     !
!     tindex   Time dimension index to read (integer).                 !
!     fldname  Name of field to read (character).                      !
!     level    Depth (>0), level (>0), or isopycnal (>1000) to         !
!              process (real).                                         !
!                                                                      !
!  On Output:                                                          !
!                                                                      !
!     f        Requested 2D field (real array).                        !
!     im       Size of inner dimension (integer).                      !
!     jm       Size of outer dimension (integer).                      !
!     igrid    Staggered C grid type (integer):                        !
!                igrid=0  => None.                                     !
!                igrid=1  => density points.                           !
!                igrid=2  => streamfunction points.                    !
!                igrid=3  => u-velocity points.                        !
!                igrid=4  => v-velocity points.                        !
!                igrid=5  => w-velocity points.                        !
!     ncvarid  Variable ID in input NetCDF file (integer).             !
!     spv      Missing data/fill data value (real).                    !
!                                                                      !
!  Calls:                                                              !
!                                                                      !
!     (NetCDF library)                                                 !
!     crash, lenstr                                                    !
!                                                                      !
!=======================================================================
!
!-----------------------------------------------------------------------
!  Define global data.
!-----------------------------------------------------------------------
!
      implicit none
#include "param.h"
#include "domdat.h"
#include "fields.h"
#include "forecast.h"
#include "netcdf.inc"
!
!-----------------------------------------------------------------------
!  Define local data.
!-----------------------------------------------------------------------
!
      integer igrid, im, ip, jm, km, n, ncid, ncposid, ncvarid, nobs,   &
     &        np, mvatts, mvdims, lenatt, lenfld, lendim, lenvar, lindx,&
     &        status, tindex, vartyp
      integer count(nf_max_dims), dims(nf_max_dims), start(nf_max_dims),&
     &        vardims(nf_max_dims)
      integer lenstr
      integer IJpos(NH)
      real level, spv
      real f(np)
      character*20  varname, dimname(nf_max_dims)
      character*60  attname, posname
      character*(*) fldname
!
!=======================================================================
!  Begin executable code.
!=======================================================================
!
      im=0
      jm=0
      km=0
      igrid=0
      lindx=0
      lenfld=lenstr(fldname)
!
!-----------------------------------------------------------------------
!  Read in requested field.
!-----------------------------------------------------------------------
!
      IF (lenfld.gt.0) THEN
!
!  Inquire ID of requested field in input NetCDF file.
!
        status=nf_inq_varid(ncid,fldname(1:lenfld),ncvarid)
        IF (status.ne.nf_noerr) THEN
          PRINT 10, fldname(1:lenfld)
          CALL crash ('GET_NCOBS',status)
        END IF
!
!  Inquire information about requested field in input NetCDF file.
!
        status=nf_inq_var(ncid,ncvarid,varname,vartyp,mvdims,vardims,   &
     &                    mvatts)
        IF (status.eq.nf_noerr) THEN
          lenvar=lenstr(varname)
          DO n=1,mvdims
            status=nf_inq_dim(ncid,vardims(n),dimname(n),dims(n))
            IF (status.ne.nf_noerr) THEN
              CALL crash ('GET_NCOBS',status)
            END IF
            lendim=lenstr(dimname(n))
            IF ((dimname(n)(1:lendim).eq.'Nobs_zeta').or.               &
     &          (dimname(n)(1:lendim).eq.'Nobs_SST' ).or.               &
     &          (dimname(n)(1:lendim).eq.'Nobs_temp').or.               &
     &          (dimname(n)(1:lendim).eq.'Nobs_SSS' ).or.               &
     &          (dimname(n)(1:lendim).eq.'Nobs_salt')) THEN
              igrid=1
              start(n)=1
              count(n)=dims(n)
              nobs=dims(n)
              im=Lr
              jm=Mr
            ELSE IF ((dimname(n)(1:lendim).eq.'Nobs_Usur').or.           &
     &              (dimname(n)(1:lendim).eq.'Nobs_u')) THEN
              igrid=3
              start(n)=1
              count(n)=dims(n)
              nobs=dims(n)
              im=Lu
              jm=Mu
            ELSE IF ((dimname(n)(1:lendim).eq.'Nobs_Vsur').or.           &
     &              (dimname(n)(1:lendim).eq.'Nobs_v')) THEN
              igrid=4
              start(n)=1
              count(n)=dims(n)
              nobs=dims(n)
              nobs=dims(n)
              im=Lv
              jm=Mv
            ELSE IF ((dimname(n)(1:lendim).eq.'s_rho').or.               &
     &              (dimname(n)(1:lendim).eq.'level')) THEN
              lindx=n
              start(n)=int(level)
              count(n)=1
              km=dims(n)
            ELSE IF (dimname(n)(1:lendim).eq.'s_w') THEN
              igrid=5
              lindx=n
              start(n)=int(level)
              count(n)=1
              km=dims(n)
            ELSE IF (INDEX(dimname(n)(1:lendim),'time').ne.0) THEN
              start(n)=tindex
              count(n)=1
            END IF
          END DO
        ELSE
          PRINT 30, fldname(1:lenfld)
          CALL crash ('GET_NCOBS',status)
        END IF
!
!  Inquire "missing value" or "fill value" and "positions" for the
!  requested field.
!
        DO n=1,mvatts
          status=nf_inq_attname(ncid,ncvarid,n,attname)
          IF (status.eq.nf_noerr) THEN
            lenatt=lenstr(attname)
            IF ((attname(1:lenatt).eq.'missing_value').or.              &
     &          (attname(1:lenatt).eq.'_FillValue')) THEN
              status=nf_get_att_real(ncid,ncvarid,attname(1:lenatt),spv)
              IF (status.ne.nf_noerr) THEN
                PRINT 40, attname(1:lenatt), varname(1:lenvar)
                CALL crash ('GET_NCOBS',status)
              END IF
            ELSE IF (attname(1:lenatt).eq.'positions') THEN
              status=nf_get_att_text(ncid,ncvarid,attname(1:lenatt),    &
     &                               posname)
              IF (status.ne.nf_noerr) THEN
                PRINT 40, attname(1:lenatt), varname(1:lenvar)
                CALL crash ('GET_NCOBS',status)
              END IF
            END IF
          END IF
        END DO
!
!  Check if the array size allocated is sufficient.
!
        IF (im*jm.gt.np) THEN
          PRINT 50, im*jm, np
          CALL crash ('GET_NCOBS',0)
        END IF
!
!  Read in requested field.
!
        status=nf_get_vara_real(ncid,ncvarid,start,count,f7)
        IF (status.ne.nf_noerr) THEN
          PRINT 60, fldname(1:lenfld)
          CALL crash ('GET_NCOBS',status)
        END IF
!
!  Read in IJ-index positions.  If ploting mean recordless data, set
!  positions to first record.
!
        lenfld=lenstr(posname)
        status=nf_inq_varid(ncid,posname(1:lenfld),ncposid)
        IF (lindx.gt.0) THEN
          count(lindx)=count(lindx+1)
        END IF
        IF (tindex.eq.0) THEN
          start(mvdims+1)=1
          count(mvdims+1)=1
        END IF
        status=nf_get_vara_int(ncid,ncposid,start,count,IJpos)
        IF (status.ne.nf_noerr) THEN
          PRINT 60, fldname(1:lenfld)
          CALL crash ('GET_NCOBS',status)
        END IF
!
!  Load field as a full grided field, fill with read special value.
!
        IF (nobs.eq.im*jm) THEN
          DO n=1,nobs
            f(n)=f7(n)
          END DO
        ELSE
          DO n=1,im*jm
            f(n)=spv
          END DO
          DO n=1,nobs
            IF ((igrid.eq.1).or.(igrid.eq.4)) THEN
              ip=IJpos(n)+1
            ELSE IF (igrid.eq.3) THEN
              ip=IJpos(n)
            END IF
            f(ip)=f7(n)
          END DO
        END IF
!
!-----------------------------------------------------------------------
!  Requested field not found.
!-----------------------------------------------------------------------
!
      ELSE
        PRINT 70, fldname(1:lenfld)
        CALL crash ('GET_NCOBS',status)
      END IF
!
  10  FORMAT (/' GET_NCOBS - cannot inquiring ID for variable: ',a)
  20  FORMAT (/' GET_NCOBS - error while inquiring dimension: ',a,2x,   &
     &         ' for variable: ',a)
  30  FORMAT (/' GET_NCOBS - error while inquiring information for',    &
     &         ' variable: ',a)
  40  FORMAT (/' GET_NCOBS - error while reading attribute: ',a,2x,     &
     &         ' for variable: ',a)
  50  FORMAT (/' GET_NCOBS - required array size = ',i6,2x,             &
     &         ' is bigger than that allocated = ',i6,/,14x,            &
     &         ' Change parameter NH and recompile.')
  60  FORMAT (/' GET_NCOBS - error while reading variable: ',a)
  70  FORMAT (/' GET_NCOBS - cannot find variable: ',a)

      RETURN
      END
