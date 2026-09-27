      PROGRAM cnt
!
!svn $Id: cnt.F 833 2017-01-24 22:20:16Z arango $
!================================================== Hernan G. Arango ===
!  Copyright (c) 2002-2017 The ROMS/TOMS Group                       ===
!    Licensed under a MIT/X style license                            ===
!    See License_ROMS.txt                                            ===
!=======================================================================
!                                                                    ===
!  This program contours the desired field(s)  at the given depth    ===
!  level(s) and time(s) using NCAR GKS plotting package (noncolor    ===
!  version).                                                         ===
!                                                                    ===
!=======================================================================
!
!-----------------------------------------------------------------------
!  Define global variables.
!-----------------------------------------------------------------------
!
      implicit none
#include "param.h"
#include "pconst.h"
#include "fdiagn.h"
#include "fields.h"
#include "ndimen.h"
#include "ncinfo.h"
#include "pltfld.h"
#include "pltncar.h"
!
!-----------------------------------------------------------------------
!  Define local variables.
!-----------------------------------------------------------------------
!
      integer file, i, id, idfld, m, n, np, rec
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
!  Set switch for plot type.
!
      plttyp=cntplt
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
        DO n=1,nlevels
          DO m=1,nfields
            idfld=fldid(m)
            level=fldlev(n)
            file=1
            day=get_pltday(rec,idfld,file)
            CALL get_2dfld (day,x,y,f1,f2,np,file,idfld,level)
            IF ((n.gt.1).and.levless) goto 10
            IF ((iref.eq.3).or.(ABS(iref).eq.5)) THEN
              IF (ABS(iref).eq.5) THEN
               id=idover
               fac=SIGN(c1,FLOAT(iref))
              ELSE
               id=idfld
               fac=-c1
              END IF
              file=2
              CALL get_2dfld (day,x,y,f3,f4,np,file,id,level)
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
              CALL get_2dfld (frstd,x,y,f3,f4,np,file,idfld,level)
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
!-----------------------------------------------------------------------
!  Plot requested field(s).
!-----------------------------------------------------------------------
!
            IF (plotit) CALL pltfield (day,f1,f2,f3,np,file,idfld,level)
  10        CONTINUE
          END DO
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
#include "fldsid.h"
#include "forecast.h"
#include "ncinfo.h"
#include "ndimen.h"
#include "pltfld.h"
#include "pltio.h"
#include "pltlab.h"
#include "pltncar.h"
!
!-----------------------------------------------------------------------
!  Define local variables.
!-----------------------------------------------------------------------
!
      logical is2df

      integer ftype, i, icheck, k, lstr
      integer lenstr

      real llon, rlon

      character*6  fcode
      character*25 name1, name2
      character*40 proj, text
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
      inpcnt=8
      inpcst=9
      inpvid=10
!
!-----------------------------------------------------------------------
!  Read in input parameters and switches for standard input.
!-----------------------------------------------------------------------
!
!  Read in year and starting year day.
!
      READ (stdinp,*) iyear, yday
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
!  Read in number of different levels and/or depths to plot.  If
!  NLEVELS=0, plot all levels available for the requested field(s).
!
      READ (stdinp,*) nlevels
!
!  Read in field depths (<0), levels (>0) and/or isopycnals (>1000) to
!  plot.  If the values are negative, it activates vertical interpolation
!  to the requested depth. If positive and greater 1000, it activates
!  the vertical interpolation to requested isopycnal.
!
      IF (nlevels.eq.0) THEN
        READ (stdinp,'(a)') text
      ELSE
        READ (stdinp,*) (fldlev(i),i=1,nlevels)
      END IF
!
!  Read in first and last day or time record (if negative) to plot.
!
      READ (stdinp,*) frstd
      READ (stdinp,*) lastd
!
!  Read in interval between plots (days). If zero, the data is plotted
!  at their own time frequency. If negative, the data will be every
!  other ABS(int(dskip)) records.
!
      READ (stdinp,*) dskip
!
!  Read in forecast record to process, if any.  Used only when there is
!  a "forecast" dimension in input NetCDF files.
!
      READ (stdinp,*) findx
!
!  Read in flag for vertical interpolation scheme.
!
      READ (stdinp,*) vintrp
      IF ((vintrp.lt.0).or.(vintrp.gt.1)) THEN
        WRITE (stdout,10) vintrp
  10    FORMAT (' GET_PARM - illegal interpolation scheme, VINTRP = ',  &
     &          i4)
        CALL crash ('GET_PARM',1)
      END IF
!
!  Read in field minimum and maximum limit values to contour.
!  (PMIN=PMAX=0.0, default range).
!
      READ (stdinp,*) pmin
      READ (stdinp,*) pmax
!
!  Read in contour interval (CNTVAL=0.0, default contour interval).
!
      READ (stdinp,*) cntval
!
!  Read in iso-surface value to process.
!
      READ (stdinp,*) isoval
!
!  Read in vector line width, vector length scale, and vector grid
!  sampling in the X- and Y-direction.
!
      READ (stdinp,*) vlwd
      READ (stdinp,*) vlscl
      READ (stdinp,*) ivinc
      READ (stdinp,*) jvinc
      ivinc=ABS(ivinc)
      jvinc=ABS(jvinc)
!
!  Read in switch to plot secondary field:
!         [-2] overlay any specified user data from fortran unit 35
!         [-1] overlay horizontal grid
!         [ 0] no secondary or reference field to plot
!         [ 1] primary file overlay
!         [ 2] secundary file overlay
!         [ 3] primary - secundary (field subtaction)
!         [ 4] Day0 - DayN (field subtraction)
!         [ 5] primary +/- secondary (different field combination:
!                                     addition [+5] or subtraction [-5])
!         [ 6] Mask OA fields by its maximum error variance tolerance
!              which is specified as fractional percentage in RMAX.
!
      READ (stdinp,*) iref
!
!  Read in overlay field identification.
!
      READ (stdinp,*) idover
!
!  Read in level of the field to overlay.  If LEVOVER is zero, the
!  level selected is the same as the current primary field.
!
      READ (stdinp,*) levover
!
!  Read in overlay field minimum and maximum limit values to contour.
!
      READ (stdinp,*) rmin
      READ (stdinp,*) rmax
!
!  Read in desired longitude/latitude grid spacing (degrees).  If
!  negative, the longitude labels will be rotated by 90 degrees.
!
      READ (stdinp,*) lgrid
!
!  Read in map projection switch:
!              [1] cylindrical equidistant
!              [2] Mercator
!              [3] Lambert conformal conical
!              [4] Stereographic zimuthal
!
      READ (stdinp,*) iproj
!
!  Read in projection Pole longitude and latitude and rotation angle
!  (clockwise; degrees).
!
      READ (stdinp,*) plon
      READ (stdinp,*) plat
      READ (stdinp,*) rota
!
!  Read in switch to color mask land:
!              [0] no land masking
!              [1] mask land grey color
!
      READ (stdinp,*) lmsk
!
!  Read in number of plots per page: 1, 2, or 4.  If negative, preserve
!  plot aspect ratio.
!
      READ (stdinp,*) npage
      a_ratio=.false.
      IF (npage.lt.0) THEN
        a_ratio=.true.
        npage=ABS(npage)
      END IF
      nframe=0
      pframe=0
!
!  Read in logical switch to read in positions from grid NetCDF file.
!
      READ (stdinp,'(l1)') readgrd
!
!  Read in logical switch to activate drawing of Logo.
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
!  Read in logical switch to plot coastlines.
!
      READ (stdinp,'(l1)') cst
!
!  Read in map coordinates: bottom, top, left, and right edges (km or
!  degrees).
!
      READ (stdinp,*) YBmap, YTmap
      READ (stdinp,*) XLmap, XRmap
!
!  Read in file name for variable IDs.
!
      CALL read_fname (stdinp, fname(0))
!
!  Read in file name for color palette.  This file is ignored. It is
!  added here for compatability with color program.
!
      CALL read_fname (stdinp, fname(6))
!
!  Read in file name for plotting parameters.
!
      CALL read_fname (stdinp, fname(1))
!
!  Read in primary NetCDF filename.
!
      CALL read_fname (stdinp, fname(2))
!
!  Read in secundary NetCDF filename.
!
      CALL read_fname (stdinp, fname(3))
!
!  Read in grid NetCDF filename.
!
      CALL read_fname (stdinp, fname(4))
!
!  Read in coastline switch and filename.
!
      CALL read_fname (stdinp, fname(5))
      IF (cst) THEN
        OPEN (inpcst,file=fname(5),form='formatted',status='old')
      END IF
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
!  Set background (white) and foreground (back) colors.
!-----------------------------------------------------------------------
!
      CALL gscr (1, 0, 1.0, 1.0, 1.0)        ! backgound  (white)
      CALL gscr (1, 1, 0.0, 0.0, 0.0)        ! foreground (black)
!
!-----------------------------------------------------------------------
!  Read in plot parameters: internal parameters and contour interval
!  matrix as function of field ID and level.
!-----------------------------------------------------------------------
!
      OPEN (inpcnt,file=fname(1),form='formatted',status='old')
      CALL readpar (inpcnt, fname(2))
!
!-----------------------------------------------------------------------
!  Set execution date label
!-----------------------------------------------------------------------
!
      CALL get_date (date_str)
!
!-----------------------------------------------------------------------
!  Open primary and secundary (if any) input NetCDF files and get the
!  necessary information for plotting.
!-----------------------------------------------------------------------
!
      CALL opencdf (fname(2),fname(3),fname(4))
!
!  If NLEVELS=0, set levels to plot.
!
      IF (nlevels.eq.0) THEN
        nlevels=Nr
        k=0
        DO i=Nr,1,-1
          k=k+1
          fldlev(k)=i
        END DO
      END IF
!
!  If spherical, convert map longitudes to EAST-longitudes.
!
      IF (spherical) THEN
        pblat=YBmap
        ptlat=YTmap
        llon=XLmap
        rlon=XRmap
        eastlon=.false.
        IF (((llon.lt.c0).and.(rlon.ge.c0)).or.                         &
     &      ((llon.lt.c0).and.(rlon.le.c0)).or.                         &
     &      ((llon.ge.c0).and.(rlon.gt.c0))) THEN
          IF ((ABS(llon).eq.c180).and.(ABS(rlon).eq.c180)) THEN
            eastlon=.true.
            pllon=c0
            prlon=c360
          ELSE
            pllon=llon
            prlon=rlon
          END IF
        ELSE
          eastlon=.true.
          IF (ABS(llon-rlon).gt.355.0) THEN
            pllon=c0
            prlon=c360
          ELSE
            IF (llon.lt.c0) llon=c360+llon
            IF (rlon.lt.c0) rlon=c360+rlon
            pllon=MIN(llon,rlon)
            prlon=MAX(llon,rlon)
          END IF
        END IF
      END IF
!
!-----------------------------------------------------------------------
!  Report plotting parameters.
!-----------------------------------------------------------------------
!
      WRITE (stdout,20) Lr, Mr, Nr, xbasin*m2km, ybasin*m2km
  20  FORMAT (/,/,'  Parameters: ',/,/,                                 &
     &        5x,' number of points (Lr,Mr,Nr) = ',3i4,/,               &
     &        5x,'    basin dimensions (Lx,Le) = ',2f10.3)
      IF (spherical) THEN
        IF (iproj.eq.1) proj='cylindrical equidistant'
        IF (iproj.eq.2) proj='Mercator'
        IF (iproj.eq.3) proj='Lambert conformal conical'
        IF (iproj.eq.4) proj='stereographic azimuthal'
        WRITE (stdout,30) pllon, prlon, pblat, ptlat, proj
  30    FORMAT (5x,' map coordinates (Rlon,Llon) = ',2f10.3,/,          &
     &          5x,'                 (Blat,Tlat) = ',2f10.3,/,          &
     &          5x,'              map projection = ',a)
      END IF
      IF (wrthdr) THEN
        WRITE (stdout,*) ' '
        WRITE (stdout,'(a)') pltitle(1)
        WRITE (stdout,'(a)') pltitle(2)
        WRITE (stdout,'(a)') pltitle(3)
        WRITE (stdout,'(a)') pltitle(4)
      END IF
      WRITE (stdout,*) ' '

      RETURN
      END

      SUBROUTINE pltfield (day,s1,s2,s3,n,file,idfld,level)
!
!=======================================================================
!                                                                    ===
!  This routine contours scalar and/or draws vector fields.   It     ===
!  also contours scalar overlays or draws vector overlays in the     ===
!  same map.                                                         ===
!                                                                    ===
!  On Input:                                                         ===
!                                                                    ===
!     day     Time of the field(s) to plot (real).                   ===
!     s1      Scalar or vector x-component (real array).             ===
!     s2      Vector y-component (real array).                       ===
!     s3      Scrath space (real array).                             ===
!     n       Number points in field (integer)                       ===
!     file    NetCDF file to plot (primary or secundary).            ===
!     idfld   Field identification (integer).                        ===
!     level   Depth (>0), level (>0), or isopycnal (>1000) to        ===
!             plot (real).                                           ===
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
#include "domdat.h"
#include "fdiagn.h"
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
      logical overlay, var_time
      integer file, icolor, idfld, ipos, igrid, imask, n, ncgid, np
      real cint, cmax, cmin, day, lev, level, vmax, vmin
      real fmax1, fmax2, fmin1, fmin2
      real cntint
      real s1(n), s2(n), s3(n)
      parameter (icolor=0, imask=0, ipos=-1)
!
!=======================================================================
!  Begin executable code.
!=======================================================================
!
!=======================================================================
!  Plot primary field (scalar or vector).
!=======================================================================
!
!  Determine primary field switches.
!
      scalar1=.false.
      stream1=.false.
      vector1=.false.
      wbarbs1=.false.
      IF (ptype.eq.pscalar) THEN
        scalar1=.true.
        overlay=.false.
      ELSE IF (ptype.eq.pstream) THEN
        stream1=.true.
        overlay=.false.
      ELSE IF (ptype.eq.pvector) THEN
        vector1=.true.
        overlay=.false.
      ELSE IF (ptype.eq.pwbarbs) THEN
        wbarbs1=.true.
        overlay=.false.
      END IF
      levless=is2d(idfld)
      fmin1=c0
      fmax1=c0
      fmin2=c0
      fmax2=c0
!
!-----------------------------------------------------------------------
!  Draw vector field.  Recall that vector magnitude ID is always
!  (idfld+1).
!-----------------------------------------------------------------------
!
      IF (vector1.or.stream1.or.wbarbs1) THEN
        CALL vecmag (s3,s1,s2,im,im,jm,vmin,vmax,spval0,spval1)
        IF (vector1) THEN
          CALL fdiag (s3,im,jm,level,day,idfld+1)
        ELSE IF (stream1) THEN
          CALL fdiag (s3,im,jm,level,day,idfld-1)
        END IF
        IF ((pmin.eq.c0).and.(pmax.eq.c0)) THEN
          IF (sdv_fac.eq.c0) THEN
            cmin=vmin
            cmax=vmax
          ELSE
            cmin=MAX(vmin,favg-sdv_fac*fsdv)
            cmax=MIN(vmax,favg+sdv_fac*fsdv)
          END IF
        ELSE
          cmin=pmin
          cmax=pmax
        END IF
        IF (readgrd) THEN
          ncgid=ncgrd
        ELSE
          ncgid=ncid1
        END IF
        igrid=1
        CALL cnt_frame (icolor)
        IF (vector1) THEN
          CALL vecpack (s1,s2,s3,im,im,jm,cmin,cmax,icolor,ipos,        &
     &                  overlay)
        END IF
        IF (stream1) THEN
          CALL streamlines (s1,s2,s3,im,im,jm,cmin,cmax,icolor,ipos,    &
     &                      overlay)
        END IF
        IF (wbarbs1) THEN
          CALL wind_barbs (s1,s2,s3,im,im,jm,cmin,cmax,icolor,ipos,     &
     &                     overlay)
        END IF
        CALL dombox
        fmin1=vmin
        fmax1=vmax
!
!-----------------------------------------------------------------------
!  Contour scalar field.
!-----------------------------------------------------------------------
!
      ELSE IF (scalar1) THEN
        CALL fdiag (s1,im,jm,level,day,idfld)
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
        IF (cntval.ne.c0) THEN
          cint=cntval
        ELSE
          cint=cntint(idfld,lev,cmin,cmax,Nr,cont,cfld,clev)
        END IF
        CALL cnt_frame (icolor)
        CALL cntpack (s1,im,im,jm,cmin,cmax,cint,imask,icolor,overlay)
        CALL dombox
        fmin1=fmin
        fmax1=fmax
      END IF
!
!=======================================================================
!  Plot secundary (overlay) field (scalar or vector).
!=======================================================================
!
      IF ((iref.eq.1).or.(iref.eq.2)) THEN
!
!-----------------------------------------------------------------------
!  Read in secundary overlay field.
!-----------------------------------------------------------------------
!
        IF (iref.eq.1) file=1
        IF (iref.eq.2) file=2
        IF (levover.eq.c0) THEN
          lev=level
        ELSE
          lev=levover
        END IF
        var_time=var_time1
        CALL get_2dfld (day,x,y,s1,s2,np,file,idover,lev)
        var_time1=var_time
!
!-----------------------------------------------------------------------
!  Determine overlay field switches.
!-----------------------------------------------------------------------
!
        scalar2=.false.
        stream2=.false.
        vector2=.false.
        wbarbs2=.false.
        IF (ptype.eq.pscalar) THEN
          scalar2=.true.
          overlay=.true.
        ELSE IF (ptype.eq.pstream) THEN
          stream2=.true.
          overlay=.true.
        ELSE IF (ptype.eq.pvector) THEN
          vector2=.true.
          overlay=.true.
        ELSE IF (ptype.eq.pwbarbs) THEN
          wbarbs2=.true.
          overlay=.true.
        END IF
        levless=is2d(idover)
!
!-----------------------------------------------------------------------
!  Draw vector overlay, if any.
!-----------------------------------------------------------------------
!
        IF (vector2.or.stream2.or.wbarbs2) THEN
          CALL vecmag (s3,s1,s2,im,im,jm,vmin,vmax,spval0,spval1)
          IF (vector2) THEN
            CALL fdiag (s3,im,jm,lev,day,idover+1)
          ELSE IF (stream2) THEN
            CALL fdiag (s3,im,jm,lev,day,idover-1)
          END IF
          IF ((rmin.eq.c0).and.(rmax.eq.c0)) THEN
            IF (sdv_fac.eq.c0) THEN
              cmin=vmin
              cmax=vmax
            ELSE
              cmin=MAX(vmin,favg-sdv_fac*fsdv)
              cmax=MIN(vmax,favg+sdv_fac*fsdv)
            END IF
          ELSE
            cmin=rmin
            cmax=rmax
          END IF
          igrid=1
          CALL set (fl,fr,fb,ft,ul,ur,ub,ut,1)
          IF (vector2) THEN
            CALL vecpack (s1,s2,s3,im,im,jm,cmax,cmin,icolor,ipos,      &
     &                    overlay)
          END IF
          IF (stream2) THEN
            CALL streamlines (s1,s2,s3,im,im,jm,cmax,cmin,icolor,ipos,  &
     &                        overlay)
          END IF
          IF (wbarbs2) THEN
            CALL wind_barbs (s1,s2,s3,im,im,jm,cmax,cmin,icolor,ipos,   &
     &                       overlay)
          END IF
          fmin2=vmin
          fmax2=vmax
!
!------------------------------------------------------------------------
!  Contour scalar overlay, if any.
!------------------------------------------------------------------------
!
        ELSE IF (scalar2) THEN
          CALL fdiag (s1,im,jm,lev,day,idover)
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
          IF (cntval.ne.c0) THEN
            cint=cntval
          ELSE
            cint=cntint(idover,lev,cmin,cmax,km,cont,cfld,clev)
          END IF
          CALL set (fl,fr,fb,ft,ul,ur,ub,ut,1)
          CALL cntpack (s1,im,im,jm,cmin,cmax,cint,imask,icolor,overlay)
          fmin2=fmin
          fmax2=fmax
        END IF
      END IF
!
!-----------------------------------------------------------------------
!  Draw background plot and advance frame when appropriate.
!-----------------------------------------------------------------------
!
      CALL hbackground (day,cint,fmin1,fmax1,fmin2,fmax2,frms,icolor,   &
     &                  idfld,level)
      nframe=nframe+1
      IF (MOD(nframe,npage).eq.0) THEN
        pframe=pframe+1
        CALL frame
        WRITE (6,10) pframe
 10     FORMAT (' Plotted frame: ',i5.5,/)
      END IF

      RETURN
      END
