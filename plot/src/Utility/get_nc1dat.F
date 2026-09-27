      SUBROUTINE get_nc1dat (f,np,lm,ncid,tindx,fldnam,ncvarid,spv)
!
!svn $Id: get_nc1dat.F 833 2017-01-24 22:20:16Z arango $
!================================================== Hernan G. Arango ===
!  Copyright (c) 2002-2017 The ROMS/TOMS Group                         !
!    Licensed under a MIT/X style license                              !
!    See License_ROMS.txt                                              !
!=======================================================================
!                                                                      !
!  This routine reads requested 1D field from input NetCDF file.       !
!                                                                      !
!  On Input:                                                           !
!                                                                      !
!     np       Allocated size for array "f".                           !
!     ncid     NetCDF ID for input file (integer).                     !
!     tindx    Time dimension index to read (integer).                 !
!     fldnam   Name of field to read (character).                      !
!                                                                      !
!  On Output:                                                          !
!                                                                      !
!     f        Requested 1D field (real array).                        !
!     lm       Size of dimension (integer).                            !
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
#include "netcdf.inc"
!
!-----------------------------------------------------------------------
!  Define local data.
!-----------------------------------------------------------------------
!
      integer lm, n, ncid, ncvarid, np, nvatts, nvdims, lenatt, lenfld, &
     &        lendim, lenvar, status, tindx, vartyp
      integer count(nf_max_dims), dims(nf_max_dims), start(nf_max_dims),&
     &        vdims(nf_max_dims)
      integer lenstr
      real scale, spv
      real f(np)
      character*10  varnam, dimnam(nf_max_dims)
      character*60  attnam
      character*(*) fldnam
!
!=======================================================================
!  Begin executable code.
!=======================================================================
!
      lm=0
      lenfld=lenstr(fldnam)
!
!-----------------------------------------------------------------------
!  Read in requested field.
!-----------------------------------------------------------------------
!
      IF (lenfld.gt.0) THEN
!
!  Inquire ID of requested field in input NetCDF file.
!
        status=nf_inq_varid(ncid,fldnam(1:lenfld),ncvarid)
        IF (status.ne.nf_noerr) THEN
          PRINT 10, fldnam(1:lenfld)
          CALL crash ('GET_NC1DAT',status)
        END IF
!
!  Inquire information about requested field in input NetCDF file.
!
        status=nf_inq_var(ncid,ncvarid,varnam,vartyp,nvdims,vdims,      &
     &                    nvatts)
        IF (status.eq.nf_noerr) THEN
          lenvar=lenstr(varnam)
          DO n=1,nvdims
            status=nf_inq_dim(ncid,vdims(n),dimnam(n),dims(n))
            IF (status.ne.nf_noerr) THEN
              PRINT 20, dimnam(n), varnam(1:lenvar)
              CALL crash ('GET_NC1DAT',status)
            END IF
            lendim=lenstr(dimnam(n))
            IF (dimnam(n)(1:lendim).eq.'time') THEN
              start(n)=tindx
              count(n)=1
            ELSE
              start(n)=1
              count(n)=dims(n)
              lm=dims(n)
            END IF
          END DO
        ELSE
          PRINT 30, fldnam(1:lenfld)
          CALL crash ('GET_NC1DAT',status)
        END IF
!
!  Inquire "missing value" or "fill value" for the requested field.
!  If "scale_factor" attribute is present for a variable, the data
!  are to be multiplied by this factor.
!
        scale=1.0
        DO n=1,nvatts
          status=nf_inq_attname(ncid,ncvarid,n,attnam)
          IF (status.eq.nf_noerr) THEN
            lenatt=lenstr(attnam)
            IF ((attnam(1:lenatt).eq.'missing_value').or.               &
     &          (attnam(1:lenatt).eq.'_FillValue')) THEN
              status=nf_get_att_real(ncid,ncvarid,attnam(1:lenatt),spv)
              IF (status.ne.nf_noerr) THEN
                PRINT 40, attnam(1:lenatt), varnam(1:lenvar)
                CALL crash ('GET_NC1DAT',status)
              END IF
            ELSE IF (attnam(1:lenatt).eq.'scale_factor') THEN
              status=nf_get_att_real(ncid,ncvarid,attnam(1:lenatt),     &
     &                               scale)
              IF (status.ne.nf_noerr) THEN
                PRINT 40, attnam(1:lenatt), varnam(1:lenvar)
                CALL crash ('GET_NC2DAT',status)
              END IF
            END IF
          END IF
        END DO
!
!  Check if the array size allocated is sufficient.
!
        IF (lm.gt.np) THEN
          PRINT 50, lm, np
          CALL crash ('GET_NC1DAT',0)
        END IF
!
!  Read in requested field.
!
        status=nf_get_vara_real(ncid,ncvarid,start,count,f)
        IF (status.ne.nf_noerr) THEN
          PRINT 60, fldnam(1:lenfld)
          CALL crash ('GET_NC1DAT',status)
        END IF
        DO n=1,lm
          f(n)=f(n)*scale
        END DO
!
!-----------------------------------------------------------------------
!  Requested field not found.
!-----------------------------------------------------------------------
!
      ELSE
        PRINT 70, fldnam(1:lenfld)
        CALL crash ('GET_NC1DAT',status)
      END IF
!
  10  FORMAT (/' GET_NC1DAT - cannot inquiring ID for variable: ',a)
  20  FORMAT (/' GET_NC1DAT - error while inquiring dimension: ',a,2x,  &
     &         ' for variable: ',a)
  30  FORMAT (/' GET_NC1DAT - error while inquiring information for',   &
     &         ' variable: ',a)
  40  FORMAT (/' GET_NC1DAT - error while reading attribute: ',a,2x,    &
     &         ' for variable: ',a)
  50  FORMAT (/' GET_NC1DAT - required array size = ',i6,2x,            &
     &         ' is bigger than that allocated = ',i6,/,14x,            &
     &         ' Change parameter NH and recompile.')
  60  FORMAT (/' GET_NC1DAT - error while reading variable: ',a)
  70  FORMAT (/' GET_NC1DAT - cannot find variable: ',a)

      RETURN
      END
