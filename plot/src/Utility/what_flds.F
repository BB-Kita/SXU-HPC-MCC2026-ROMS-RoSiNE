      SUBROUTINE what_flds (ncid,ivars,file,ncname)
!
!=======================================================================
!  Copyright (c) 1996 Rutgers University                             ===
!=======================================================================
!                                                                    ===
!  This routine reports plottable fields and thier times available   ===
!  in NetCDF file NCNAME.                                            ===
!                                                                    ===
!  On Input:                                                         ===
!                                                                    ===
!     ncid       NetCDF file ID (integer).                           ===
!     ivars      number of variables in NetCDF file (integer).       ===
!     file       flag for primary (FILE=1) or secondary (FILE=2)     ===
!                NetCDF file (integer).                              ===
!     ncname     Input NetCDF file name (character).                 ===
!                                                                    ===
!  Calls:                                                            ===
!                                                                    ===
!    fldnam2id, lenstr                                               ===
!                                                                    ===
!=======================================================================
!
!-----------------------------------------------------------------------
!  Define global data.
!-----------------------------------------------------------------------
!
      implicit none
#include "param.h"
#include "pconst.h"
#include "ncinfo.h"
#include "netcdf.inc"
#include "pltio.h"
!
!-----------------------------------------------------------------------
!  Define local data.
!-----------------------------------------------------------------------
!
      integer file, i, ic, idfld, latt, lfld, lstr, n, ncid, ivars,     &
     &        nvatts, status
      integer fldnam2id, lenstr
      character*40  attnam, vname
      character*240 field
      character*(*) ncname
!
!=======================================================================
!  Begin executable code.
!=======================================================================
!
      lstr=lenstr(ncname)
      IF (file.eq.1) THEN
        WRITE (stdout,10) ncname(1:lstr)
      ELSE IF (file.eq.2) THEN
        WRITE (stdout,20) ncname(1:lstr)
      END IF
!
!  Inquire variables with the attribute "field".
!
      ic=0
      DO i=1,ivars
        varid=i
        status=nf_inq_var(ncid,varid,vname,vartyp,nvdims,vdims,nvatts)
        IF (status.eq.nf_noerr) THEN
          DO n=1,nvatts
            status=nf_inq_attname(ncid,varid,n,attnam)
            IF (status.eq.nf_noerr) THEN
              latt=lenstr(attnam)
              IF (attnam(1:latt).eq.'field') THEN
                status=nf_inq_attlen(ncid,varid,'long_name',lfld)
                IF ((status.eq.nf_noerr).and.(lfld.le.240)) THEN
                  status=nf_get_att_text(ncid,varid,'long_name',field)
                  IF (status.eq.nf_noerr) THEN
                    ic=ic+1
                    IF (file.eq.1) THEN
                      IF (gottime1) THEN
                        idfld=fldnam2id(vname)
                        IF (idfld.ne.-99) THEN
                          WRITE (stdout,30) 'ID=',idfld, field(1:lfld)
                        END IF
                      ELSE
                        idfld=fldnam2id(vname)
                        IF (idfld.ne.-99) THEN
                          WRITE (stdout,30) 'ID=',idfld, field(1:lfld)
                        END IF
                      END IF
                    ELSE IF (file.eq.2) THEN
                      IF (gottime2) THEN
                        idfld=fldnam2id(vname)
                        IF (idfld.ne.-99) THEN
                          WRITE (stdout,30) 'ID=',idfld, field(1:lfld)
                        END IF
                      ELSE
                        idfld=fldnam2id(vname)
                        IF (idfld.ne.-99) THEN
                          WRITE (stdout,30) 'ID=',idfld, field(1:lfld)
                        END IF
                      END IF
                    END IF
                  END IF
                END IF
              END IF
            END IF
          END DO
        END IF
      END DO
!
!  Report available times.
!
      IF ((ntime1.gt.0).or.(ntime2.gt.0)) THEN
        WRITE (stdout,40)
        IF (file.eq.1) THEN
          IF (gottime1) THEN
            WRITE (stdout,50) (Tscale1*time1(i),i=1,ntime1)
          END IF
        ELSE IF (file.eq.2) THEN
          IF (gottime2) THEN
            WRITE (stdout,50) (Tscale2*time2(i),i=1,ntime2)
          END IF
        END IF
      END IF
!
!  Report available levels.
!
      IF ((nlev1.gt.0).or.(nlev2.gt.0)) THEN
        WRITE (stdout,60)
        IF (file.eq.1) THEN
          IF ((type1(1:5).eq.'SCRUM').or.(type1(1:5).eq.'ROMS').or.     &
     &        (type1(1:3).eq.'ICE')) THEN
            WRITE (stdout,70) (n,lev1(n),n=1,nlev1)
          END IF
        ELSE IF (file.eq.2) THEN
          IF ((type2(1:5).eq.'SCRUM').or.(type2(1:5).eq.'ROMS').or.     &
     &        (type2(1:3).eq.'ICE')) THEN
            WRITE (stdout,70) (n,lev2(n),n=1,nlev2)
          END IF
        END IF
      END IF
!
  10  FORMAT (/,2x,'Plottable Fields in Primary NetCDF File: ',/,2x,    &
     &       '---------------------------------------',/,/,10x,a,/)
  20  FORMAT (/,2x,'Plottable Fields in Secondary NetCDF File: ',/,2x,  &
     &       '-----------------------------------------',/,/,10x,a,/)
  30  FORMAT (1x,'[',a,i3,']',1x,a)
  40  FORMAT (/,2x,'Available Times: ',/,2x,'---------------',/)
  50  FORMAT (5g15.7)
  60  FORMAT (/,2x,'Available levels: ',/,2x,'----------------',/)
  70  FORMAT (1x,'[',i2,']',1x,'for DATA Level:',1x,i5)

      RETURN
      END
