      PROGRAM csec
!
!svn $Id: csec.F 833 2017-01-24 22:20:16Z arango $
!================================================== Hernan G. Arango ===
!  Copyright (c) 2002-2017 The ROMS/TOMS Group                       ===
!    Licensed under a MIT/X style license                            ===
!    See License_ROMS.txt                                            ===
!=======================================================================
!                                                                    ===
!  This program contours a vertical section of the desired field(s)  ===
!  at the given  time(s) using  NCAR  GKS  plotting  package (color  ===
!  version).                                                         ===
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
#include "fields.h"
#include "fldsid.h"
#include "ncinfo.h"
#include "ndimen.h"
#include "pltfld.h"
#include "pltio.h"
#include "pltncar.h"
#include "secbath.h"
#include "secpos.h"
!
!-----------------------------------------------------------------------
!  Define local data.
!-----------------------------------------------------------------------
!
      integer file, i, id, idfld, igrid, m, np, rec
      real day, fac, level
      real get_pltday
!
!=======================================================================
!  Begin executable code.
!=======================================================================
!
!  Initialize NCAR GKS plotting package.
!
      CALL gksinit
!
!  Set switch for plot type.  Set level to zero to process all available
!  levels.
!
      plttyp=secplt
      level=c0
!
!  Read in plot job parameters.
!
      CALL get_parm
!
!-----------------------------------------------------------------------
!  Read in fields at the specified time.
!-----------------------------------------------------------------------
!
      Lrecord=.false.
      idfld=fldid(1)
      file=1
      day=get_pltday(0,idfld,file)
      rec=frstr
!
      DO WHILE ((frstr.le.rec).and.(rec.le.lastr))
        trecord=rec
        do m=1,nfields
          idfld=fldid(m)
          file=1
          day=get_pltday(rec,idfld,file)
          CALL get_3dfld (day,x,y,z,f1,f2,np,igrid,file,idfld,level)
          IF ((iref.eq.3).or.(ABS(iref).eq.5)) THEN
            IF (ABS(iref).eq.5) THEN
              id=idover
              fac=SIGN(c1,FLOAT(iref))
            ELSE
              id=idfld
              fac=-c1
            END IF
            file=2
            CALL get_3dfld (day,x,y,z,f3,f4,np,igrid,file,id,level)
            DO i=1,np
              IF ((f3(i).gt.spval0).or.(f1(i).gt.spval0)) THEN
                f1(i)=spval1
              ELSE
                f1(i)=f1(i)+fac*f3(i)
              END IF
              IF ((f4(i).gt.spval0).or.(f2(i).gt.spval0)) THEN
                f2(i)=spval1
              ELSE
                f2(i)=f2(i)+fac*f4(i)
              END IF
            END DO
          ELSE IF (iref.eq.4) THEN
            file=1
            IF (Lrecord) THEN
              trecord=frstr
            END IF
            CALL get_3dfld (frstd,x,y,z,f3,f4,np,igrid,file,idfld,level)
            trecord=rec
            DO i=1,np
              IF ((f3(i).gt.spval0).or.(f1(i).gt.spval0)) THEN
                f1(i)=spval1
              ELSE
                f1(i)=f3(i)-f1(i)
              END IF
              IF ((f4(i).gt.spval0).or.(f2(i).gt.spval0)) THEN
                f2(i)=spval1
              ELSE
                f2(i)=f4(i)-f2(i)
              END IF
            END DO
          END IF
!
!  Get section coordinates.
!
          CALL get_scoor (x,y,im,jm)
!
!  Extract/interpolate requested section for primary field. If plotting
!  a vector component, process the other component and then rotate to
!  section coordinates.  Notice that X- and Y-position are saved since
!  this arrays are over-written in "xsection".
!
          idfld=fldid(m)
          IF (lrotvec.and.((vcode(idfld).eq.'IDUTOT').or.               &
     &                     (vcode(idfld).eq.'IDVTOT').or.               &
     &                     (vcode(idfld).eq.'IDUCLI').or.               &
     &                     (vcode(idfld).eq.'IDVCLI'))) THEN
            DO i=1,np
              f3(i)=x(i)
              f4(i)=y(i)
            END DO
            CALL xsection (f6,ims,kms,f1,f3,f4,z,im,jm,km,vintrp,spval1)
            CALL xsection (f1,ims,kms,f2,f3,f4,z,im,jm,km,vintrp,spval1)
            CALL sec_rotvec (f2,f6,f1,ims,kms,idfld,spval1)
          ELSE
            CALL xsection (f2,ims,kms,f1,x,y,z,im,jm,km,vintrp,spval1)
          END IF
!
!-----------------------------------------------------------------------
!  Plot requested field(s).
!-----------------------------------------------------------------------
!
          IF (plotit) CALL pltsec (day,f2,f4,NV,file,idfld)
        END DO
!
!-----------------------------------------------------------------------
!  Set next record to plot or terminate plotting.
!-----------------------------------------------------------------------
!
        rec=rec+rskip
      END DO
!
!-----------------------------------------------------------------------
!  Close GKS software and datasets.
!-----------------------------------------------------------------------
!
      CALL crash ('DONE',1)

      STOP
      END

      SUBROUTINE get_parm
!
!=======================================================================
!                                                                    ===
!  This routine reads in input parameters from standard input        ===
!  script and sets various parameters and switches needed for        ===
!  plotting.                                                         ===
!                                                                    ===
!  Calls:                                                            ===
!                                                                    ===
!    get_date, opencdf, readpal, setcolpal                           ===
!                                                                    ===
!=======================================================================
!
!-----------------------------------------------------------------------
!  Define Global data.
!-----------------------------------------------------------------------
!
      implicit none
#include "param.h"
#include "pconst.h"
#include "domdat.h"
#include "fldsid.h"
#include "forecast.h"
#include "ncinfo.h"
#include "ndimen.h"
#include "pltfld.h"
#include "pltio.h"
#include "pltlab.h"
#include "pltncar.h"
#include "secpos.h"
!
!-----------------------------------------------------------------------
!  Define local data.
!-----------------------------------------------------------------------
!
      logical is2df

      integer ftype, i, icheck, lstr
      integer lenstr

      character*6  fcode
      character*25 name1, name2
      character*40 text
      character*80 flabel
!
!=======================================================================
!  Begin executable code.
!=======================================================================
!
!  Set I/O logical units.
!
      stdinp=5
      stdout=6
      inppal=7
      inpcnt=8
      inpvid=10
!
!-----------------------------------------------------------------------
!  Read in input parameters and switches for standard input.
!-----------------------------------------------------------------------
!
!  Read in year and starting year day.
!
      READ (stdinp,*) iyear,yday
      wdate=.true.
      IF (yday.lt.c0) wdate=.false.
!
!  Read in plot titles.
!
      READ (stdinp,'(a)') pltitle(1)
      READ (stdinp,'(a)') pltitle(2)
      READ (stdinp,'(a)') pltitle(3)
      READ (stdinp,'(a)') pltitle(4)
!
!  Read in number of different fields to plot.
!
      READ (stdinp,*) nfields
!
!  Read in field(s) identification.
!
      READ (stdinp,*) (fldid(i),i=1,nfields)
!
!  Read in first and last day or time record (if negative) to plot.
!
      READ (stdinp,*) frstd
      READ (stdinp,*) lastd
!
!  Read in interval between plots (days). If zero, the data is plotted
!  at their own time frequency. If negative, the data will be every
!  other ABS(INT(dskip)) records.
!
      READ (stdinp,*) dskip
!
!  Read in forecast record to process, if any.  Used only when there is
!  a "forecast" dimension in input NetCDF files.
!
      READ (stdinp,*) findx
!
!  Read in flag for extracting section coordinates:
!     [0] grid units, [1] Cartesian (km), [2] Spherical (degrees).
!
      READ (stdinp,*) isecpos
!
!  Read in the coordinates of the two points defining the section
!  (grid units, or kilometers, or degrees).  Convert longitudes to
!  EAST-longitudes, if appropriate.
!
      READ (stdinp,*) xsec1
      READ (stdinp,*) ysec1
      READ (stdinp,*) xsec2
      READ (stdinp,*) ysec2
      IF (isecpos.eq.2) THEN
        IF (((xsec1.le.c0).and.(xsec2.ge.c0)).or.                       &
     &      ((xsec1.le.c0).and.(xsec2.le.c0)).or.                       &
     &      ((xsec1.gt.c0).and.(xsec2.gt.c0))) THEN
          eastlon=.false.
        ELSE
          eastlon=.true.
          IF (xsec1.lt.c0) xsec1=c360+xsec1
          IF (xsec2.lt.c0) xsec2=c360+xsec2
        END IF
      END IF
!
!  Read in spacing (km or degrees) used to compute section points
!  between (xsec1,ysec1) and (xsec2,ysec2).
!
      READ (stdinp,*) dels
!
!  Read in section minimum and maximun depths (meters) to consider.
!
      READ (stdinp,*) sztop
      READ (stdinp,*) szbot
!
!  Read number of refiment levels to interpolate section between SZTOP
!  and SZBOT.
!
      READ (stdinp,*) kms
!
!  Read in switch for vertical interpolation scheme to use during
!  section extraction: [0] linear, [1] cubic spline.
!
      READ (stdinp,*) vintrp
      IF ((vintrp.lt.0).or.(vintrp.gt.1)) THEN
        WRITE (stdout,10) vintrp
  10    FORMAT (' GET_PARM - illegal interpolation scheme, VINTRP = ',  &
     &          i4)
        CALL crash ('GET_PARM',1)
      END IF
!
!  Read in field minimum and maximum limit values for color palette.
!
      READ (stdinp,*) pmin
      READ (stdinp,*) pmax
!
!  Read in switch to put contour lines over color bands:
!           [0] no, [1] yes.
!
      READ (stdinp,*) icnt
!
!  Read in switch to plot secondary field:
!          [0] no secondary or reference field to plot
!          [1] primary file overlay
!          [2] secondary file overlay
!          [3] primary - secondary (field subtaction)
!          [4] Day0 - DayN (field subtraction)
!          [5] primary +/- secondary (different field combination:
!                                     addition [+5] or subtraction [-5])
!          [6] Mask OA fields by its maximum error variance tolerance
!              which is specified as fractional percentage in RMAX.
!
      READ (stdinp,*) iref
!
!  Read in overlay field identification.
!
      READ (stdinp,*) idover
!
!  Read in overlay field minimum and maximum limit values to contour.
!
      READ (stdinp,*) rmin
      READ (stdinp,*) rmax
!
!  Read in number of plots per page: 1, 2, or 4.
!
      READ (stdinp,*) npage
      nframe=0
      pframe=0
!
!  Read in switch to mask area below of water column: [0] no, [1] yes.
!
      READ (stdinp,*) ifill
!
!  Read in logical switch to read in positions from grid NetCDF file.
!
      READ (stdinp,'(l1)') readgrd
!
!  Read in logical switch to activate drawing of Harvard Logo.
!
      READ (stdinp,'(l1)') pltlogo
!
!  Read in logical switch to activate writing of plot header titles.
!
      READ (stdinp,'(l1)') wrthdr
!
!  Read in logical switch to activate writing of plot bottom title.
!
      READ (stdinp,'(l1)') wrtblab
!
!  Read in logical switches to activate writing of information about the
!  range values of the plotted primary and secondary (if any) data and
!  the contour interval when appropriate.
!
      READ (stdinp,'(l1,a)') wrtrang1, text
      lstr=lenstr(text)
      IF ((text(1:1).eq.'T').or.(text(1:1).eq.'t')) THEN
        wrtrang2=.true.
      ELSE
        wrtrang2=.false.
      END IF
!
!  Read in logical switch to activate writing of input filename.
!
      READ (stdinp,'(l1)') wrtfnam
!
!  Read in logical switch to activate writing of current plotting date.
!
      READ (stdinp,'(l1)') wrtdate
!
!  Read in file name for variable IDs.
!
      CALL read_fname (stdinp, fname(0))
!
!  Read in filename for color palette.
!
      CALL read_fname (stdinp, fname(1))
!
!  Read in filename for plotting parameters.
!
      CALL read_fname (stdinp, fname(2))
!
!  Read in primary NetCDF filename.
!
      CALL read_fname (stdinp, fname(3))
!
!  Read in secondary NetCDF filename.
!
      CALL read_fname (stdinp, fname(4))
!
!  Read in grid NetCDF filename.
!
      CALL read_fname (stdinp, fname(5))
!
!-----------------------------------------------------------------------
!  Set execution date label
!-----------------------------------------------------------------------
!
      CALL get_date (date_str)
!
!-----------------------------------------------------------------------
!  Read in variables IDs.
!-----------------------------------------------------------------------
!
      OPEN (inpvid,file=fname(0),form='formatted',status='old')
      nvar=0
      icheck=0
      DO WHILE (icheck.lt.999)
        READ (inpvid,*) i, fcode, ftype, is2df, name1, name2, flabel
        IF (i.ne.999) THEN
          vid(i)=i
          vcode(i)=fcode
          vtype(i)=ftype
          is2d(i)=is2df
          vname1(i)=name1
          vname2(i)=name2
          vlabel(i)=flabel
          nvar=MAX(nvar,i)
        END IF
        icheck=i
      END DO
!
!-----------------------------------------------------------------------
!  Open primary and secondary (if any) input NetCDF files and get the
!  necessary information for plotting.
!-----------------------------------------------------------------------
!
      CALL opencdf (fname(3),fname(4),fname(5))
!
!-----------------------------------------------------------------------
!  Read and set color palette indices.
!-----------------------------------------------------------------------
!
      OPEN (inppal,file=fname(1),form='formatted',status='old')
      CALL readpal (inppal)
      CALL setcolpal
!
!-----------------------------------------------------------------------
!  Read in plot parameters: internal parameters and contour interval
!  matrix as function of field ID and level.
!-----------------------------------------------------------------------
!
      OPEN (inpcnt,file=fname(2),form='formatted',status='old')
      CALL readpar (inpcnt, fname(2))
!
!-----------------------------------------------------------------------
!  Report plotting parameters.
!-----------------------------------------------------------------------
!
      WRITE (stdout,20) Lr, Mr, Nr, xbasin*m2km, ybasin*m2km
  20  FORMAT (/,/,'  Parameters: ',/,/,                                 &
     &        5x,' number of points (im,jm,km) = ',3i4,/,               &
     &        5x,'    basin dimensions (Lx,Ly) = ',2f10.3,/)
      IF (wrthdr) THEN
        WRITE (stdout,'(a)') pltitle(1)
        WRITE (stdout,'(a)') pltitle(2)
        WRITE (stdout,'(a)') pltitle(3)
        WRITE (stdout,'(a)') pltitle(4)
      END IF
      WRITE (stdout,*) ' '

      RETURN
      END

      SUBROUTINE pltsec (day,s1,s2,n,file,idfld)
!
!=======================================================================
!                                                                    ===
!  This routine contours scalar cross-sections.  It also contours    ===
!  scalar overlays in the same map.                                  ===
!                                                                    ===
!  On Input:                                                         ===
!                                                                    ===
!     day     time of the field(s) to plot (real).                   ===
!     s1      primary scalar field (real array).                     ===
!     s2      scondary scalar field (real array).                    ===
!     n       number points in field (integer)                       ===
!     file    NetCDF file to plot (primary or secondary).            ===
!     idfld   field identification (integer).                        ===
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
#include "fdiagn.h"
#include "fields.h"
#include "fldsid.h"
#include "ncinfo.h"
#include "ndimen.h"
#include "pltfld.h"
#include "pltio.h"
#include "pltncar.h"
#include "secpos.h"
!
!=======================================================================
!  Define local data.
!=======================================================================
!
      logical overlay, var_time
      integer file, i, icolor, id, idfld, igrid, ipos, imask, n, np
      real cint, cmax, cmin, day, level
      real fmax1, fmax2, fmin1, fmin2

      real cntint
      real s1(n), s2(n)
      parameter (icolor=1, ipos=1, level=20000.0, imask=0)
!
!=======================================================================
!  Plot scalar primary field.
!=======================================================================
!
      fmin1=c0
      fmax1=c0
      fmin2=c0
      fmax2=c0
!
!-----------------------------------------------------------------------
!  Set area mask, if any.
!-----------------------------------------------------------------------
!
      lmsk=0
      CALL areamask (s1,n)
!
!-----------------------------------------------------------------------
!  Set section frame.
!-----------------------------------------------------------------------
!
      CALL sec_frame (icolor)
!
!-----------------------------------------------------------------------
!  Contour primary scalar field (color)
!-----------------------------------------------------------------------
!
      scalar1=.true.
      overlay=.false.
      CALL fdiag (s1,ims,kms,level,day,idfld)
      IF ((pmin.eq.c0).and.(pmax.eq.c0)) THEN
        IF (sdv_fac.eq.c0) THEN
          cmin=fmin
          cmax=fmax
        ELSE
          cmin=MAX(fmin,favg-sdv_fac*fsdv)
          cmax=MIN(fmax,favg+sdv_fac*fsdv)
        END IF
      ELSE
        cmin=pmin
        cmax=pmax
      END IF
      fmin1=fmin
      fmax1=fmax
      CALL cntcolor (s1,ims,ims,kms,cmin,cmax,icnt,ipos,lmsk,overlay)
!
!=======================================================================
!  Plot secondary (overlay) field.
!=======================================================================
!
      IF ((iref.eq.1).or.(iref.eq.2)) THEN
!
!  Read in overlay field, if any.
!
        IF (iref.eq.1) file=1
        IF (iref.eq.2) file=2
        id=idover
        var_time=var_time1
        CALL get_3dfld (day,x,y,z,f1,f2,np,igrid,file,id,c0)
        var_time1=var_time
!
!  Get section coordinates and extract/interpolate requested section.
!
        CALL get_scoor (x,y,im,jm)
        IF ((vcode(idover).eq.'IDUTOT').or.                             &
     &      (vcode(idover).eq.'IDVTOT').or.                             &
     &      (vcode(idover).eq.'IDUCLI').or.                             &
     &      (vcode(idover).eq.'IDVCLI')) THEN
          DO i=1,np
            f3(i)=x(i)
            f4(i)=y(i)
          END DO
          CALL xsection (f6,ims,kms,f1,f3,f4,z,im,jm,km,vintrp,spval1)
          CALL xsection (f1,ims,kms,f2,f3,f4,z,im,jm,km,vintrp,spval1)
          CALL sec_rotvec (s2,f6,f1,ims,kms,idover,spval1)
        ELSE
          CALL xsection (s2,ims,kms,f1,x,y,z,im,jm,km,vintrp,spval1)
        END IF
!
!  Plot overlay.
!
        scalar2=.true.
        overlay=.true.
        CALL fdiag (s2,ims,kms,level,day,idover)
        IF ((rmin.eq.c0).and.(rmax.eq.c0)) THEN
          IF (sdv_fac.eq.c0) THEN
            cmin=fmin
            cmax=fmax
          ELSE
            cmin=MAX(fmin,favg-sdv_fac*fsdv)
            cmax=MIN(fmax,favg+sdv_fac*fsdv)
          END IF
        ELSE
          cmin=rmin
          cmax=rmax
        END IF
        cint=cntint(idover,1,cmin,cmax,km,cont,cfld,clev)
        CALL set (fl,fr,fb,ft,ul,ur,ub,ut,1)
        CALL cntpack (s2,ims,ims,kms,cmin,cmax,cint,imask,icolor+1,     &
     &                overlay)
        fmin2=fmin
        fmax2=fmax
!
!  Draw vertical, terrain-following levels.
!
      ELSE IF (iref.eq.-1) THEN
        CALL slevels
      END IF
!
!-----------------------------------------------------------------------
!  Area fill the section below water column.
!-----------------------------------------------------------------------
!
      IF (ifill.ge.0) CALL areafill (icolor,ifill)
!
!-----------------------------------------------------------------------
!  Draw vertical background plot and advance frame when appropriate.
!-----------------------------------------------------------------------
!
      CALL vbackground (day,cint,fmin1,fmax1,fmin2,fmax2,frms,          &
     &                  icolor,idfld)
      nframe=nframe+1
      IF (MOD(nframe,npage).eq.0) THEN
        pframe=pframe+1
        CALL frame
        WRITE (6,10) pframe
 10     FORMAT (' Plotted frame: ',i5.5,/)
      END IF
      RETURN
      END
