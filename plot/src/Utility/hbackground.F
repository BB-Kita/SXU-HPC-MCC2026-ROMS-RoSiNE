      SUBROUTINE hbackground (day,cint,fmin1,fmax1,fmin2,fmax2,frms,    &
     &                        icolor,idfld,level)
!
!svn $Id: hbackground.F 833 2017-01-24 22:20:16Z arango $
!================================================== Hernan G. Arango ===
!  Copyright (c) 2002-2017 The ROMS/TOMS Group                         !
!    Licensed under a MIT/X style license                              !
!    See License_ROMS.txt                                              !
!=======================================================================
!                                                                      !
!  This routine draws background plot like grid, coastlines, top       !
!  and bottom titles, and various overlays.                            !
!                                                                      !
!=======================================================================
!
!-----------------------------------------------------------------------
!  Define global data.
!-----------------------------------------------------------------------
!
      implicit none
#include "pconst.h"
#include "pltio.h"
#include "pltlab.h"
#include "pltfld.h"
#include "pltncar.h"
!
!-----------------------------------------------------------------------
!  Define local data.
!-----------------------------------------------------------------------
!
      logical floats
      integer Ideg, Imin, Jdeg, Jmin, icolor, idfld, lstr
      integer lenstr
      real blat, cint, d, day,  dbot, delmaj, delmin, delta, dlef,      &
     &     dmax, dmin, dtop, drig, fbs, fllab, fls, fmax1, fmax2,       &
     &     fmin1, fmin2, frms, frs, ftlab, fts, fxl, fyl, level,        &
     &     llon, minute, rlon, tlat, x, xg, xl, xlab, xlon, xr,         &
     &     x1, x2, y, yb, yg, ylab, ylat, yt, y1, y2
      character*5  dlab
      character*30 xchar, ychar
!
!=======================================================================
!  Begin executive code.
!=======================================================================
!
!  Set plotting frame.
!
      CALL set (fl,fr,fb,ft,ul,ur,ub,ut,1)
!
!  Set line pattern: solid lines (65535) or dash lines (43690).
!
      CALL dashdb (43690)
!
!  Turn off clipping.
!
      CALL gsclip (0)
!
!=======================================================================
!  Draw grid and labeled it.
!=======================================================================
!
      IF (ltmarks) THEN
!
!-----------------------------------------------------------------------
!  Draw spherical grid.
!-----------------------------------------------------------------------
!
        IF (spherical.and.((iproj.eq.1).or.(iproj.eq.2))) THEN
          llon=pllon
          rlon=prlon
          blat=pblat
          tlat=ptlat
          CALL maptrn (blat,llon,xl,yb)
          CALL maptrn (tlat,rlon,xr,yt)
!
!  Determine if axis labels are integer or floats.
!
          delta=ABS(lgrid)-FLOAT(INT(ABS(lgrid)))
          floats=.false.
          IF (delta.gt.c0) floats=.true.
!
!  Draw iso-latitude grid-lines and write labels.
!
          fllab=fl-r100
          xlab=ul+(fllab-fl)*(ur-ul)/(fr-fl)
          IF (MOD(blat,ABS(lgrid)).eq.c0) THEN
            ylat=blat
          ELSE
            ylat=blat-MOD(blat,ABS(lgrid))*SIGN(1.0,blat)
          END IF
          CALL maptrn (ylat,llon,x,y)
          DO WHILE (y.le.yt)
            IF (y.ge.yb) THEN
              CALL maptrn (ylat,llon,x1,y1)
              CALL maptrn (ylat,rlon,x2,y2)
              CALL lined (x1,y1,x2,y2)
              ylab=y1
              yg=ylat
              Jdeg=INT(ABS(yg))
              minute=(ABS(yg)-AINT(ABS(yg)))*60.0
              Jmin=nINT(minute)
              IF ((c0.le.ABS(yg)).and.(ABS(yg).lt.c10)) THEN
                IF (floats) THEN
                  IF (yg.lt.c0) THEN
                    WRITE (ychar,'(f4.1)') yg
                    lstr=4
                  ELSE
                    WRITE (ychar,'(f3.1)') yg
                    lstr=3
                  END IF
                ELSE
                  IF (yg.lt.c0) THEN
                    WRITE (ychar,'(i2)') INT(yg)
                    lstr=2
                  ELSE
                    WRITE (ychar,'(i1)') INT(yg)
                    lstr=1
                  END IF
                END IF
              ELSE IF ((c10.le.ABS(yg)).and.(ABS(yg).lt.c100)) THEN
                IF (floats) THEN
                  IF (yg.lt.c0) THEN
                    WRITE (ychar,'(f5.1)') yg
                    lstr=5
                  ELSE
                    WRITE (ychar,'(f4.1)') yg
                    lstr=4
                  END IF
                ELSE
                  IF (yg.lt.c0) THEN
                    WRITE (ychar,'(i3)') INT(yg)
                    lstr=3
                  ELSE
                    WRITE (ychar,'(i2)') INT(yg)
                    lstr=2
                  END IF
                END IF
              END IF
              IF (geo_lab) THEN
                IF (Jmin.ne.0) THEN
                  WRITE (ychar,10) Jdeg, Jmin
                ELSE
                  WRITE (ychar,20) Jdeg
                END IF
                lstr=lenstr(ychar)
              END IF
              CALL plchhq (xlab,ylab,ychar(1:lstr),FontSize4,c0,c1)
            END IF
            ylat=ylat+ABS(lgrid)
            CALL maptrn (ylat,llon,x,y)
          END DO
!
!  Draw iso-longitude grid-lines and write labels.
!
          IF (lgrid.gt.c0) THEN
            ftlab=ft+0.02
          ELSE
            ftlab=ft+0.01
          END IF
          ylab=ub+(ftlab-fb)*(ut-ub)/(ft-fb)
          IF (MOD(llon,ABS(lgrid)).eq.c0) THEN
            xlon=llon
          ELSE
            xlon=llon-MOD(llon,ABS(lgrid))*SIGN(1.0,llon)
          END IF
          CALL maptrn (blat,xlon,x,y)
          DO WHILE (x.le.xr)
            IF (x.ge.xl) THEN
              CALL maptrn (blat,xlon,x1,y1)
              CALL maptrn (tlat,xlon,x2,y2)
              CALL lined (x1,y1,x2,y2)
              xlab=x2
              IF (eastlon.and.(x.gt.c180)) THEN
                xg=x-c360
              ELSE
                xg=xlon
              END IF
              Ideg=INT(ABS(xg))
              minute=(ABS(xg)-AINT(ABS(xg)))*60.0
              Imin=NINT(minute)
              IF ((c0.le.ABS(xg)).and.(ABS(xg).lt.c10)) THEN
                IF (floats) THEN
                  IF (xg.lt.c0) THEN
                    WRITE (xchar,'(f4.1)') xg
                    lstr=4
                  ELSE
                    WRITE (xchar,'(f3.1)') xg
                    lstr=3
                  END IF
                ELSE
                  IF (xg.lt.c0) THEN
                    WRITE (xchar,'(i2)') INT(xg)
                    lstr=2
                  ELSE
                    WRITE (xchar,'(i1)') INT(xg)
                    lstr=1
                  END IF
                END IF
                IF (geo_lab) THEN
                  IF (Jmin.ne.0) THEN
                    WRITE (xchar,30) Ideg, Imin
                  ELSE
                    WRITE (xchar,40) Ideg
                  END IF
                  lstr=lenstr(xchar)
                END IF
              ELSE IF ((c10.le.ABS(xg)).and.(ABS(xg).lt.c100)) THEN
                IF (floats) THEN
                  IF (xg.lt.c0) THEN
                    WRITE (xchar,'(f5.1)') xg
                    lstr=5
                  ELSE
                    WRITE (xchar,'(f4.1)') xg
                    lstr=4
                  END IF
                ELSE
                  IF (xg.lt.c0) THEN
                    WRITE (xchar,'(i3)') INT(xg)
                    lstr=3
                  ELSE
                    WRITE (xchar,'(i2)') INT(xg)
                    lstr=2
                  END IF
                END IF
                IF (geo_lab) THEN
                  IF (Jmin.ne.0) THEN
                    WRITE (xchar,50) Ideg, Imin
                  ELSE 
                    WRITE (xchar,60) Ideg
                  END IF
                  lstr=lenstr(xchar)
                END IF
              ELSE IF ((c100.le.ABS(xg)).and.(ABS(xg).lt.c1000)) THEN
                IF (floats) THEN
                  IF (xg.lt.c0) THEN
                    WRITE (xchar,'(f6.1)') xg
                    lstr=6
                  ELSE
                    WRITE (xchar,'(f5.1)') xg
                    lstr=5
                  END IF
                ELSE
                  IF (xg.lt.c0) THEN
                    WRITE (xchar,'(i4)') INT(xg)
                    lstr=4
                  ELSE
                    WRITE (xchar,'(i3)') INT(xg)
                    lstr=3
                  END IF
                END IF
                IF (geo_lab) THEN
                  IF (Jmin.ne.0) THEN
                    WRITE (xchar,70) Ideg, Imin
                  ELSE
                    WRITE (xchar,80) Ideg
                  END IF
                  lstr=lenstr(xchar)
                END IF
              END IF
              IF (lgrid.gt.c0) THEN
                CALL plchhq (xlab,ylab,xchar(1:lstr),FontSize4,c0,c0)
              ELSE
                CALL plchhq (xlab,ylab,xchar(1:lstr),FontSize4,c90,-c1)
              END IF
            END IF
            xlon=xlon+ABS(lgrid)
            CALL maptrn (blat,xlon,x,y)
          END DO
!
!-----------------------------------------------------------------------
!  Use default EZMAP grid drawing routine for complicated projections.
!-----------------------------------------------------------------------
!
        ELSE IF (spherical.and.(iproj.ge.3)) THEN
          CALL mapgrd
          CALL maplbm
        ELSE
!
!-----------------------------------------------------------------------
!  Draw Cartesian grid.
!-----------------------------------------------------------------------
!
!  Draw horizontal X-axis minor tick marks.
!
          dmin=ul
          dmax=ur
          IF (INT((dmax-dmin)/c5).le.10) THEN
            delmin=c5
            delmaj=c25
          ELSE IF (INT((dmax-dmin)/c10).le.10) THEN
            delmin=c10
            delmaj=c50
          ELSE IF (INT((dmax-dmin)/c50).le.5) THEN
            delmin=c10
            delmaj=c50
          ELSE IF (INT((dmax-dmin)/c100).le.5) THEN
            delmin=c20
            delmaj=c100
          ELSE IF (INT((dmax-dmin)/c200).le.5) THEN
            delmin=c50
            delmaj=c200
          ELSE IF (INT((dmax-dmin)/c500).le.5) THEN
            delmin=c100
            delmaj=c500
          ELSE IF (INT((dmax-dmin)/c1000).le.5) THEN
            delmin=c200
            delmaj=c1000
          ELSE
            delmin=c500
            delmaj=c5000
          END IF
          fts=ft-r100
          fbs=fb+r100
          dtop=ub+(ut-ub)*(fts-fb)/(ft-fb)
          dbot=ub+(ut-ub)*(fbs-fb)/(ft-fb)
          d=ul-MOD(ul,delmin)
          DO WHILE ((d+delmin).le.dmax)
            d=d+delmin
            IF (d.ge.dmin) THEN
              CALL line (d,ut,d,dtop)
              CALL line (d,ub,d,dbot)
            END IF
          END DO
!
!  Label horizontal X-axis and draw its associated major tick marks.
!
          fts=ft-r50
          fbs=fb+r50
          fxl=ft+p015
          dtop=ub+(ut-ub)*(fts-fb)/(ft-fb)
          dbot=ub+(ut-ub)*(fbs-fb)/(ft-fb)
          xlab=ub+(ut-ub)*(fxl-fb)/(ft-fb)
          d=FLOAT(INT(c2*ul/delmaj))*delmaj
          DO WHILE (d.le.dmax)
            IF (d.ge.dmin) THEN
              WRITE (dlab,'(i4)') INT(d)
              IF (d.eq.c0) THEN
                IF (coorscl.eq.0) THEN
                  dlab='0 km'
                ELSE IF (coorscl.eq.1) THEN
                  dlab='0 m '
                ELSE IF (coorscl.eq.2) THEN
                  dlab='0 cm'
                ELSE IF (coorscl.eq.3) THEN
                  dlab='0 mm'
                END IF
                CALL plchhq (d,xlab,dlab,c10,c0,-c1)
              ELSE
                CALL plchhq (d,xlab,dlab,c10,c0,c0)
              END IF
              CALL line (d,ut,d,dtop)
              CALL line (d,ub,d,dbot)
            END IF
            d=d+delmaj
          END DO
!
!  Draw horizontal y-axis minor tick marks.
!
          dmin=ub
          dmax=ut
          IF (INT((dmax-dmin)/c5).le.10) THEN
            delmin=c5
            delmaj=c25
          ELSE IF (INT((dmax-dmin)/c10).le.10) THEN
            delmin=c10
            delmaj=c50
          ELSE IF (INT((dmax-dmin)/c50).le.5) THEN
            delmin=c10
            delmaj=c50
          ELSE IF (INT((dmax-dmin)/c100).le.5) THEN
            delmin=c20
            delmaj=c100
          ELSE IF (INT((dmax-dmin)/c200).le.5) THEN
            delmin=c50
            delmaj=c200
          ELSE IF (INT((dmax-dmin)/c500).le.5) THEN
            delmin=c100
            delmaj=c500
          ELSE
            delmin=c200
            delmaj=c1000
          END IF
          fls=fl+r100
          frs=fr-r100
          dlef=ul+(ur-ul)*(fls-fl)/(fr-fl)
          drig=ul+(ur-ul)*(frs-fl)/(fr-fl)
          d=ub-MOD(ub,delmin)
          DO WHILE ((d+delmin).le.dmax)
            d=d+delmin
            IF (d.ge.dmin) THEN
              CALL line (ul,d,dlef,d)
              CALL line (ur,d,drig,d)
            END IF
          END DO
!
!  Label horizontal Y-axis and draw its associated major tick marks.
!
          fls=fl+r40
          frs=fr-r40
          fyl=fl-r33
          dlef=ul+(ur-ul)*(fls-fl)/(fr-fl)
          drig=ul+(ur-ul)*(frs-fl)/(fr-fl)
          ylab=ul+(ur-ul)*(fyl-fl)/(fr-fl)
          d=FLOAT(INT(c2*ub/delmaj))*delmaj
          DO WHILE (d.le.dmax)
            IF (d.ge.dmin) THEN
              IF (d.eq.c0) THEN
                IF (coorscl.eq.0) THEN
                  dlab='0 km'
                ELSE IF (coorscl.eq.1) THEN
                  dlab='0 m '
                ELSE IF (coorscl.eq.2) THEN
                  dlab='0 cm'
                ELSE IF (coorscl.eq.3) THEN
                  dlab='0 mm'
                END IF
              ELSE
                WRITE (dlab,'(i4)') INT(d)
              END IF
              CALL plchhq (ylab,d,dlab,c10,c0,c0)
              CALL line (ul,d,dlef,d)
              CALL line (ur,d,drig,d)
            END IF
            d=d+delmaj
          END DO
        END IF
        CALL dashdb (65535)
      END IF
!
!-----------------------------------------------------------------------
!  Read coastlines and islands and plot them.
!-----------------------------------------------------------------------
!
      IF (cst) CALL draw_line (inpcst,0)
!
!-----------------------------------------------------------------------
!  Write map titles.
!-----------------------------------------------------------------------
!
      CALL maptitles (day,cint,fmin1,fmax1,fmin2,fmax2,frms,            &
     &                icolor,idfld,level)
!
!-----------------------------------------------------------------------
!  Draw any USER application.
!-----------------------------------------------------------------------
!
      CALL draw_user
!
!  Turn on clipping
!
      CALL gsclip (1)
!
 10   FORMAT (i2,':S:o:N:',1x,i2,':S:,:N:')
 20   FORMAT (i2,':S:o:N:')
 30   FORMAT (i1,':S:o:N:',1x,i2,':S:,:N:')
 40   FORMAT (i1,':S:o:N:')
 50   FORMAT (i2,':S:o:N:',1x,i2,':S:,:N:')
 60   FORMAT (i2,':S:o:N:')
 70   FORMAT (i3,':S:o:N:',1x,i2,':S:,:N:')
 80   FORMAT (i3,':S:o:N:')

      RETURN
      END

      SUBROUTINE mapgrd
!
!=======================================================================
!                                                                      !
!  This routine draw default grid according to the map projection.     !
!  Adapted fron NCAR routine.                                          !
!                                                                      !
!=======================================================================
!
!  Declare required common blocks.  See MAPBD for descriptions of these
!  common blocks and the variables in them.
!
#if defined NCARG430
      common /mapcm1/  coso,cosr,phoc,sino,sinr,iprj,irod
      real             coso,cosr,phoc,sino,sinr
      integer          iprj,irod
      save   /mapcm1/
#elif defined NCARG431 || defined NCARG441
      common /mapcm1/  coso,cosr,phoc,sino,sinr,iprj,irod
      double precision coso,cosr,phoc,sino,sinr
      integer          iprj,irod
      save   /mapcm1/
#elif defined NCARG520
      common /mapcm1/  coso,cosr,sino,sinr,iprj,irod
      double precision coso,cosr,sino,sinr
      integer          iprj,irod
      save   /mapcm1/
#else
      common /mapcm1/ iprj,phoc,irod,rsno,rcso,rsnr,rcsr
      save /mapcm1/
      integer         iprj,irod
      real            phoc,rsno,rcso,rsnr,rcsr
#endif
!
      common /mapdp1/ dsno,dcso,dsnr,dcsr
      double precision dsno,dcso,dsnr,dcsr
      save /mapdp1/
!
#if defined NCARG422
      common /mapcm2/ umin,umax,vmin,vmax,ucen,vcen,urng,vrng,          &
     &                blam,slam,blom,slom,issl,peps
      integer         issl
      real            umin,umax,vmin,vmax,ucen,vcen,urng,vrng,          &
     &                blam,slam,blom,slom,peps
      save   /mapcm2/
#elif defined NCARG430
      common /mapcm2/  blam,blom,peps,slam,slom,ucen,umax,umin,uoff,    &
     &                 urng,vcen,vmax,vmin,voff,vrng,issl
      real             blam,blom,peps,slam,slom,ucen,umax,umin,uoff,    &
     &                 urng,vcen,vmax,vmin,voff,vrng
      integer          issl
      save   /mapcm2/
#elif defined NCARG431
      common /mapcm2/  blam,blom,peps,slam,slom,ucen,umax,umin,uoff,    &
     &                 urng,vcen,vmax,vmin,voff,vrng,issl
      double precision blam,blom,peps,slam,slom,ucen,umax,umin,uoff,    &
     &                 urng,vcen,vmax,vmin,voff,vrng
      integer          issl
      save   /mapcm2/
#elif defined NCARG441 || defined NCARG520
      common /mapcm2/  blam,blom,peps,slam,slom,ucen,umax,umin,uoff,    &
     &                 urng,vcen,vmax,vmin,voff,vrng,issl
      integer          issl
      double precision blam,blom,peps,slam,slom,ucen,umax,umin,uoff,    &
     &                 urng,vcen,vmax,vmin,voff,vrng
      save /mapcm2/
#else
      common /mapcm2/ umin,umax,vmin,vmax,ueps,veps,ucen,vcen,          &
     &                urng,vrng,blam,slam,blom,slom,issl
      integer         issl
      real            umin,umax,vmin,vmax,ueps,veps,ucen,vcen,          &
     &                urng,vrng,blam,slam,blom,slom
      save /mapcm2/
#endif
!
#if defined NCARG430
      common /mapcm4/  grdr,grid,grla,grlo,grpo,otol,phia,phio,pla1,    &
     &                 pla2,pla3,pla4,plb1,plb2,plb3,plb4,pltr,rota,    &
     &                 srch,xlow,xrow,ybow,ytow,idot,idsh,idtl,ilcw,    &
     &                 ilts,jprj,elpf,intf,lblf,prmf
      real             grdr,grid,grla,grlo,grpo,otol,phia,phio,pla1,    &
     &                 pla2,pla3,pla4,plb1,plb2,plb3,plb4,pltr,rota,    &
     &                 srch,xlow,xrow,ybow,ytow
      integer          idot,idsh,idtl,ilcw,ilts,jprj
      logical          elpf,intf,lblf,prmf
      save   /mapcm4/
#elif defined NCARG431 || defined NCARG441 || defined NCARG520
      common /mapcm4/  grdr,grid,grla,grlo,grpo,otol,phia,phio,pla1,    &
     &                 pla2,pla3,pla4,plb1,plb2,plb3,plb4,pltr,rota,    &
     &                 srch,xlow,xrow,ybow,ytow,idot,idsh,idtl,ilcw,    &
     &                 ilts,jprj,elpf,intf,lblf,prmf
      double precision grdr,grid,grla,grlo,grpo,otol,phia,phio,pla1,    &
     &                 pla2,pla3,pla4,plb1,plb2,plb3,plb4,pltr,rota,    &
     &                 srch,xlow,xrow,ybow,ytow
      integer          idot,idsh,idtl,ilcw,ilts,jprj
      logical          elpf,intf,lblf,prmf
      save   /mapcm4/
#else
      common /mapcm4/ intf,jprj,phia,phio,rota,ilts,pla1,pla2,pla3,pla4,&
     &                plb1,plb2,plb3,plb4,pltr,grid,idsh,idot,lblf,prmf,&
     &                elpf,xlow,xrow,ybow,ytow,idtl,grdr,srch,ilcw,grla,&
     &                grlo,grpo
      logical         intf,lblf,prmf,elpf
      integer         idot,idsh,ilcw,ilts,jprj
      real            phia,phio,rota,pla1,pla2,pla3,pla4,plb1,plb2,plb3,&
     &                plb4,pltr,grid,xlow,xrow,ybow,ytow,idtl,grdr,srch,&
     &                grla,grlo,grpo
      save /mapcm4/
#endif
!
!  Define local logical flags.
!
      logical imf, ipf
      integer icfell
#if defined NCARG431 || defined NCARG441 || defined NCARG520
      double precision blat, blon, dlat, dlon, dtor, glat, glon, olat,  &
     &                 rlat, rlon, slat, slon, u, v, x, xlon, xlat
      double precision cling, floor
#else
      real blat, blon, dlat, dlon, dtor, glat, glon, olat, rlat, rlon,  &
     &     slat, slon, u, v, x, xlon, xlat
      real cling, floor
#endif
!
!  Define required constants.
!
      data dtor / .017453292519943 /
!
!  The arithmetic statement functions floor and cling give, respectively,
!  the "floor" of x - the largest integer less than or equal to x - and
!  the "ceiling" of x - the smallest integer greater than or equal to x.
!
#if defined NCARG431 || defined NCARG441 || defined NCARG520
      floor(x)=DINT(x+1.d4)-1.d4
      cling(x)=-floor(-x)
#else
      floor(x)=REAL(DINT(DBLE(x)+1.d4)-1.d4)
      cling(x)=-floor(-x)
#endif
!
!  Check for an uncleared prior error.
!
      IF (icfell('MAPGRD - uncleared prior error',1).ne.0) RETURN
!
!  If ezmap needs initialization, do nothing.
!
      IF (intf) RETURN
!
!  If the grid is suppressed, do nothing.
!
      IF (grid.le.0.) RETURN
!
!  Otherwise, set the latitude and longitude grid spacings.
!
      glat=grid
      glon=grid
      IF (grla.gt.0.) glat=grla
      IF (grlo.gt.0.) glon=grlo
!
!  Reset the color index, dotting, and dash pattern for the grid.
!
#if defined NCARG430 || defined NCARG431 || defined NCARG441 || \
    defined NCARG520
      CALL mdpchi (2,0,idsh)
#else
      CALL mapchi (2,0,idsh)
#endif
      IF (icfell('MAPGRD',2).ne.0) RETURN
!
!  Set the flags imf and ipf, which are true if and only if meridians and
!  parallels, respectively, are straight lines and it is "safe" to draw
!  them using long line segments.  what we have to be sure of is that at
!  least one of the two endpoints of each meridian, or its midpoint, will
!  be visible.  (if two points are invisible, mapit draws nothing, even
!  though the line joining them may be visible along part of its length.)
!
      IF ((iprj.ge.1).and.(iprj.le.6)) THEN
        IF (elpf) THEN
          imf=((ucen/urng)**2+(vcen/vrng)**2).lt.1.
        ELSE
          imf=(umin*umax.lt.0.).and.(vmin*vmax.lt.0.)
        END IF
        IF (iprj.ne.1) imf=imf.and.(ABS(phia).ge.89.9999)
      ELSE if (((iprj.eq.10).or.(iprj.eq.11)).and.(issl.ne.0)) THEN
        imf=.true.
      ELSE
        imf=.false.
      END IF
!
      ipf=((iprj.eq.10).or.(iprj.eq.11).or.                             &
     &    ((iprj.eq.12).and.(ilts.eq.1))).and.(issl.ne.0)
!
!  Transfer the latitude/longitude limits computed by mapint to local,
!  modifiable variables.
!
      slat=slam
      blat=blam
      slon=slom
      blon=blom
!
!  For certain azimuthal projections centered at a pole, the latitude
!  limit furthest from the pole needs adjustment to make it projectable
!  and visible.  otherwise, we have trouble with portions of meridians
!  disappearing.
!
      IF ((iprj.eq.3).or.(iprj.eq.4).or.(iprj.eq.6)) THEN
        IF (phia.gt.+89.9999) THEN
          slat=slat+srch
          IF (iprj.eq.3) slat=slat+srch
        END IF
        IF (phia.lt.-89.9999) THEN
          blat=blat-srch
          IF (iprj.eq.3) blat=blat-srch
        END IF
      END IF
!
!  RLON is the smallest longitude for which a meridian is to be drawn,
!  XLON the biggest.  avoid drawing a given meridian twice.
!
      rlon=glon*floor(slon/glon)
      xlon=glon*cling(blon/glon)
      IF (xlon-rlon.gt.359.9999) THEN
        IF (iprj.eq.1) THEN
          rlon=glon*cling((phoc-179.9999)/glon)
          xlon=glon*floor((phoc+179.9999)/glon)
        ELSE if ((iprj.ge.2).and.(iprj.le.9)) THEN
          xlon=xlon-glon
          IF (xlon-rlon.gt.359.9999) xlon=xlon-glon
        END IF
      END IF
!
!  OLAT is the latitude at which meridians which do not extend all the
!  way to the poles are to stop.
!
      IF ((iprj.eq.10).or.(iprj.eq.11)) THEN
        olat=90.0
      ELSE
        IF (INT(grpo/1000.0).eq.0) THEN
          olat=glat*floor(89.9999/glat)
        ELSE
          olat=glat*floor(MIN(89.9999,REAL(INT(grpo/1000.0)))/glat)
        END IF
      END IF
!
!  Draw the meridians.
!
      rlon=rlon-glon
  101 rlon=rlon+glon
      xlat=olat
      IF (MOD(grpo,1000.0d0).gt.0.0) THEN
        IF (MOD(rlon,MOD(grpo,1000.0d0)).eq.0.0) xlat=90.0
      END IF
      rlat=MAX(slat,-xlat)
      xlat=MIN(blat,xlat)
      IF (imf) THEN
        dlat=0.2*(xlat-rlat)
      ELSE
        dlat=(xlat-rlat)/cling((xlat-rlat)/grdr)
      END IF
      CALL mapit (rlat,rlon,0)
      IF (icfell('MAPGRD',3).ne.0) RETURN
  102 rlat=rlat+dlat
      CALL mapit (rlat,rlon,1)
      IF (icfell('MAPGRD',4).ne.0) RETURN
      IF (rlat.lt.(xlat-0.5*dlat)) go to 102
      IF (rlon.lt.(xlon-0.5*glon)) go to 101
!
!  Round the latitude limits to appropriate multiples of glat.
!
      slat=glat*floor(slat/glat)
      IF (slat.le.-90.0) slat=slat+glat
      blat=glat*cling(blat/glat)
      IF (blat.ge.90.0) blat=blat-glat
!
!  If a fast-path cylindrical equidistant projection is in use and either
!  or both of the poles is within the (rectangular) perimeter, arrange
!  for the parallels at -90 and/or +90 to be drawn.
!
      IF (iprj.eq.10) THEN
        CALL maptrn (-90.0,phoc,u,v)
        IF (icfell('MAPGRD',5).ne.0) RETURN
        IF ((u.ge.umin).and.(u.le.umax).and.                            &
     &      (v.ge.vmin).and.(v.le.vmax)) slat=slat-glat
        CALL maptrn (90.0,phoc,u,v)
        IF (icfell('MAPGRD',6).ne.0) RETURN
        IF ((u.ge.umin).and.(u.le.umax).and.                            &
     &      (v.ge.vmin).and.(v.le.vmax)) blat=blat+glat
      END IF
!
!  Draw the parallels.
!
      xlat=slat-glat
  103 xlat=xlat+glat
      rlat=MAX(-90.0d0,MIN(90.0d0,xlat))
      IF ((INT(grpo/1000.0).eq.0).or.                                   &
     &    (ABS(rlat).le.REAL(INT(grpo/1000.0)))) THEN
        rlon=floor(slon)
        xlon=MIN(cling(blon),rlon+360.0)
        IF (ipf) THEN
          dlon=0.2*(xlon-rlon)
        ELSE
          dlon=(xlon-rlon)/cling((xlon-rlon)/grdr)
        END IF
        CALL mapit (rlat,rlon,0)
        IF (icfell('MAPGRD',7).ne.0) RETURN
  104   rlon=rlon+dlon
        CALL mapit (rlat,rlon,1)
        IF (icfell('MAPGRD',8).ne.0) RETURN
        IF (rlon.lt.(xlon-0.5*dlon)) go to 104
      END IF
      IF (xlat.lt.(blat-0.5*glat)) go to 103
!
!  Restore the color index, dotting, and dash pattern.
!
#if defined NCARG430 || defined NCARG431 || defined NCARG441 || \
    defined NCARG520
      CALL mdpchi (-2,0,idsh)
#else
      CALL mapchi (-2,0,idsh)
#endif
      IF (icfell('MAPGRD',9).ne.0) RETURN
!
!  Draw the limb lines.
!
      CALL maplmb
      IF (icfell('MAPGRD',10).ne.0) RETURN

      RETURN
      END

      SUBROUTINE maplbm
!
!=======================================================================
!  This routine labels the meridians if and only if the current        !
!  projection is azimuthal and centered at one of the poles, a         !
!  circular boundary is being used, and the grid increment is an       !
!  integral divisor of 180.  The routine was not thought general       !
!  enough to include in EZMAP itself, but may nevertheless be of       !
!  interest to users.                                                  !
!=======================================================================
!
!-----------------------------------------------------------------------
!  Define local data.
!-----------------------------------------------------------------------
!
      integer i, ibew, icsz, ielp, igrd, ilew, irew, itew, lnlg, nchs
      integer kfpx, kfpy
      real a, angd, angr, b, c, delx, dely, fbew, flew, fmul, frew,     &
     &     ftew, grid, hoch, holb, plat, plon, rota, ulew, urew, vbew,  &
     &     vtew, woch, wolb, xend, yend
      character*2 proj
      character*3 chrs
      character*4 chlb
      external kfpx, kfpy
!
!-----------------------------------------------------------------------
!  See if the conditions required for MAPLBM to work are met.
!-----------------------------------------------------------------------
!
!  The projection must be azimuthal, ...
!
      CALL mapgtc ('PR',proj)
      IF ((proj.ne.'ST').and.(proj.ne.'OR').and.(proj.ne.'LE').and.     &
     &    (proj.ne.'GN').and.(proj.ne.'AE')) RETURN
!
!  The pole latitude must be +90 degrees or -90 degrees, ...
!
      CALL mapgtr ('PT',plat)
      IF (ABS(plat).lt.89.9999) RETURN
!
!  The perimeter must be elliptical, ...
!
      CALL mapgti ('EL',ielp)
      IF (ielp.eq.0) RETURN
!
!  The values used in the set call must define a circle, ...
!
      CALL getset (flew,frew,fbew,ftew,ulew,urew,vbew,vtew,lnlg)
      ilew=kfpx(flew)
      irew=kfpx(frew)
      ibew=kfpy(fbew)
      itew=kfpy(ftew)
      IF (ulew+urew.gt.0.0001.or.vbew+vtew.gt.0.0001) RETURN
      IF (ulew+vtew.gt.0.0001.or.vbew+urew.gt.0.0001) RETURN
!
!  And the grid spacing must be an integral divisor of 180.
!
      CALL mapgtr ('GR',grid)
      IF (AMOD(grid,1.).ne.0.or.                                        &
     &    MOD(180,ifix(grid)).ne.0) RETURN
!
!-----------------------------------------------------------------------
!  All conditions are satisfied.  Label the meridians.
!-----------------------------------------------------------------------
!
!  Collect the necessary information.
!
      igrd=grid
      CALL mapgtr ('PN',plon)
      CALL mapgtr ('RO',rota)
      CALL mapgti ('LS',icsz)
      IF (icsz.eq.0) THEN
        icsz=8
      ELSE if (icsz.eq.1) THEN
        icsz=12
      ELSE if (icsz.eq.2) THEN
        icsz=16
      ELSE if (icsz.eq.3) THEN
        icsz=24
      END IF
      woch=(FLOAT(  icsz)/FLOAT(irew-ilew))*(urew-ulew)
      hoch=(FLOAT(2*icsz)/FLOAT(itew-ibew))*(vtew-vbew)
      holb=hoch/1.5
!
!  Loop on the label values.
!
      DO i=-180,179,igrd
!
!  Express the value of the longitude in a nice form.
!
        write (chrs,'(i3)') iABS(i)
        nchs=0
        IF (iABS(i).ge.100) THEN
          nchs=nchs+1
          chlb(nchs:nchs)=chrs(1:1)
        END IF
        IF (iABS(i).ge.10) THEN
          nchs=nchs+1
          chlb(nchs:nchs)=chrs(2:2)
        END IF
        nchs=nchs+1
        chlb(nchs:nchs)=chrs(3:3)
        IF (i.gt.-180.and.i.lt.0) THEN
          nchs=nchs+1
          chlb(nchs:nchs)='W'
        ELSE IF (i.gt.0.and.i.lt.180) THEN
          nchs=nchs+1
          chlb(nchs:nchs)='E'
        END IF
!
!  Compute the width of the label.
!
        wolb=FLOAT(nchs)*woch
!
!  Find the angle at which the labelled Meridian lies on the plot.
!
        IF (plat.gt.0.) THEN
          angd=FLOAT(i-90)-plon-rota
        ELSE
          angd=FLOAT(90-i)+plon-rota
        END IF
!
!  Reduce the angle to the range from -180 to +180 and find its
!  equivalent in radians.
!
        angd=angd-SIGN(180.,angd+180.)                                  &
     &           +SIGN(180.,180.-angd)
        angr=0.017453292519943*angd
!
!  Figure out where the end of the meridian is.
!
        xend=urew*COS(angr)
        yend=vtew*SIN(angr)
!
!  Extend the meridian a little to make a tick mark.
!
        CALL line (xend,yend,1.015*xend,1.015*yend)
!
!  Compute a center position for the label which puts its nearest
!  edge at a fixed distance from the perimeter.  first, compute
!  the components (delx,dely) of the vector from the center of the
!  label box to the edge nearest the perimeter.
!
        IF (angd.lt.-179.9999) THEN
          delx=+0.5*wolb
          dely= 0.0
        ELSE IF (angd.lt. -90.0001) THEN
          delx=+0.5*wolb
          dely=+0.5*holb
        ELSE IF (angd.lt. -89.9999) THEN
          delx= 0.0
          dely=+0.5*holb
        ELSE IF (angd.lt.  -0.0001) THEN
          delx=-0.5*wolb
          dely=+0.5*holb
        ELSE IF (angd.lt.  +0.0001) THEN
          delx=-0.5*wolb
          dely= 0.0
        ELSE IF (angd.lt. +89.9999) THEN
          delx=-0.5*wolb
          dely=-0.5*holb
        ELSE IF (angd.lt. +90.0001) THEN
          delx= 0.0
          dely=-0.5*holb
        ELSE IF (angd.lt.+179.9999) THEN
          delx=+0.5*wolb
          dely=-0.5*holb
        ELSE
          delx=+0.5*wolb
          dely= 0.0
        END IF
!
!  Then, solve (for FMUL) the following equation:
!
!    SQRT((FMUL*xend+delx)**2+(FMUL*yend+dely)**2))=1.02*urew
!
!  which expresses the condition that the corner of the box
!  nearest the circular perimeter should be at a distance of
!  1.02*(the radius of the perimeter) away from the center of
!  the plot.
!
        a=xend*xend+yend*yend
        b=2.*(xend*delx+yend*dely)
        c=delx*delx+dely*dely-1.0404*urew*urew
        fmul=(-b+SQRT(b*b-4.*a*c))/(2.*a)
!
!  draw the label.
!
        CALL pwrit (fmul*xend,fmul*yend,chlb,nchs,icsz,0,0)
      END DO

      RETURN
      END
