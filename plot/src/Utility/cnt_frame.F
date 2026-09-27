      SUBROUTINE cnt_frame (icolor)
!
!svn $Id: cnt_frame.F 833 2017-01-24 22:20:16Z arango $
!================================================== Hernan G. Arango ===
!  Copyright (c) 2002-2017 The ROMS/TOMS Group                         !
!    Licensed under a MIT/X style license                              !
!    See License_ROMS.txt                                              !
!=======================================================================
!                                                                      !
!  This routine sets viewport space for the current map on the         !
!  page.                                                               !
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
#include "domdat.h"
#include "ndimen.h"
#include "pltfld.h"
#include "pltio.h"
#include "pltncar.h"
!
#if defined NCARG431
        common /mapcm2/  blam,blom,peps,slam,slom,ucen,umax,umin,uoff,  &
     &                   urng,vcen,vmax,vmin,voff,vrng,issl
        double precision blam,blom,peps,slam,slom,ucen,umax,umin,uoff,  &
     &                   urng,vcen,vmax,vmin,voff,vrng
        integer          issl
        save   /mapcm2/
#elif defined NCARG430
        common /mapcm2/  blam,blom,peps,slam,slom,ucen,umax,umin,uoff,  &
     &                   urng,vcen,vmax,vmin,voff,vrng,issl
        real             blam,blom,peps,slam,slom,ucen,umax,umin,uoff,  &
     &                   urng,vcen,vmax,vmin,voff,vrng
        integer          issl
        save   /mapcm2/
#elif defined NCARG422
      common /mapcm2/ umin,umax,vmin,vmax,ucen,vcen,urng,vrng,blam,slam,&
     &                blom,slom,issl,peps
      save   /mapcm2/
      integer         issl
      real            umin,umax,vmin,vmax,ucen,vcen,urng,vrng,blam,slam,&
     &                blom,slom,peps
#else
      common /mapcm2/ umin,umax,vmin,vmax,ueps,veps,ucen,vcen,urng,vrng,&
     &                blam,slam,blom,slom,issl
      save /mapcm2/
      integer         issl
      real            umin,umax,vmin,vmax,ueps,veps,ucen,vcen,urng,vrng,&
     &                blam,slam,blom,slom
#endif
!
!-----------------------------------------------------------------------
!  Define local data.
!-----------------------------------------------------------------------
!
      integer icolor, ip
      real wb, wl, wr, wt, xlen, xmax, xmin, ylen, ymax, ymin
!
!=======================================================================
!  Begin executable code.
!=======================================================================
!
!-----------------------------------------------------------------------
!  Set viewport according to the number of maps per page.
!-----------------------------------------------------------------------
!
      CALL viewport (wl,wr,wb,wt,sclmap,ipltpg,npage,icolor,wrthdr,     &
     &               pltlogo)
!
!  Set frame coordinates according to domain aspect ratio.
!
      fl=wl
      fr=wr
      fb=wb
      ft=wt
      IF (.not.spherical) THEN
        xmin=spval1
        xmax=-spval1
        ymin=spval1
        ymax=-spval1
        DO ip=1,im*jm
          xmin=MIN(xmin,x(ip))
          xmax=MAX(xmax,x(ip))
          ymin=MIN(ymin,y(ip))
          ymax=MAX(ymax,y(ip))
        END DO
        IF (XLmap.ne.XRmap) THEN
          xmin=MAX(xmin,XLmap)
          xmax=MIN(xmax,XRmap)
        END IF
        IF (YBmap.ne.YTmap) THEN
          ymin=MAX(ymin,YBmap)
          ymax=MIN(ymax,YTmap)
        END IF
        IF (a_ratio) THEN
          xlen=xmax-xmin
          ylen=ymax-ymin
          IF (xlen.ge.ylen) THEN
            fl=wl
            fr=wr
            fb=p5*(wb+wt)-p5*(fr-fl)*ylen/xlen
            ft=p5*(wb+wt)+p5*(fr-fl)*ylen/xlen
          ELSE IF (ylen.gt.xlen) THEN
            fb=wb
            ft=wt
            fl=p5*(wl+wr)-p5*(ft-fb)*xlen/ylen
            fr=p5*(wl+wr)+p5*(ft-fb)*xlen/ylen
          END IF
        END IF
      END IF
!
!-----------------------------------------------------------------------
!  Use EZMAP to set the map viewport acconding with the spherical map
!  projection.  Notice that for stereographic projection, used mainly
!  in polar projections, "lgrid" has the meaning of angular distances.
!-----------------------------------------------------------------------
!
      IF (spherical) THEN
!!      CALL mapstr ('GR - grid spacing in degrees',FLOAT(lgrid))
        CALL mapstl ('PE - draw map perimeter',.false.)
        IF (iproj.eq.1) THEN
          CALL maproj ('CE - cylindrical equidistant',plat,plon,rota)
          CALL mapset ('CO - corners',pblat,pllon,ptlat,prlon)
        ELSE IF (iproj.eq.2) THEN
          CALL maproj ('ME - Mercator',plat,plon,rota)
          CALL mapset ('CO - corners',pblat,pllon,ptlat,prlon)
        ELSE IF (iproj.eq.3) THEN
          CALL maproj ('LC - Lambert conformal conic',plat,plon,rota)
          CALL mapset ('CO - corners',pblat,pllon,ptlat,prlon)
        ELSE IF (iproj.eq.4) THEN
          CALL maproj ('ST - stereographic azimuthal',plat,plon,rota)
!!        CALL mapsti ('EL - elliptical perimeter flag',1)
!!        CALL mapset ('AN - angular distances',lgrid,lgrid,lgrid,lgrid)
          CALL mapset ('CO - corners',pblat,pllon,ptlat,prlon)
        END IF
        IF (icolor.eq.0) THEN
          CALL mapsti ('DA - solid dash patern for grids',65535)
        ELSE IF (icolor.eq.1) THEN
          CALL mapstl ('DL - draw dotted lines',.true.)
          CALL mapsti ('DD - distance between dots',4)
        END IF
        CALL mapstc ('OU - outline data set','CO')
        CALL mapsti ('VS - vertical stripping',0)
        CALL mapint
        IF (iproj.eq.4) THEN
          ul=umin
          ur=umax
          ub=vmin
          ut=vmax
        ELSE
          CALL maptrn (pblat,pllon,ul,ub)
          CALL maptrn (ptlat,prlon,ur,ut)
          IF (a_ratio) THEN
            xlen=ABS(ur-ul)
            ylen=ABS(ut-ub)
            IF (xlen.ge.ylen) THEN
              fl=wl
              fr=wr
              fb=p5*(wb+wt)-p5*(fr-fl)*ylen/xlen
              ft=p5*(wb+wt)+p5*(fr-fl)*ylen/xlen
            ELSE IF (ylen.gt.xlen) THEN
              fb=wb
              ft=wt
              fl=p5*(wl+wr)-p5*(ft-fb)*xlen/ylen
              fr=p5*(wl+wr)+p5*(ft-fb)*xlen/ylen
            END IF
          END IF
        END IF
      ELSE
!
!-----------------------------------------------------------------------
!  Set Cartesian map viewport.
!-----------------------------------------------------------------------
!
        ul=xmin
        ur=xmax
        ub=ymin
        ut=ymax
      END IF
!
!-----------------------------------------------------------------------
!  If coastlines and islands data is not provided, use EZMAP defaults.
!  (noncolor only).
!-----------------------------------------------------------------------
!
      IF (spherical.and.(.not.cst).and.(icolor.eq.0)) THEN
        CALL mapdrw
      END IF
!
!-----------------------------------------------------------------------
!  Report viewport information.
!-----------------------------------------------------------------------
!
      IF (lvpinfo) THEN
        WRITE (6,"(/' CNT_FRAME - plots/page    = ',i4)") npage
        WRITE (6,"( ' CNT_FRAME - current plot  = ',i4)") ipltpg
        WRITE (6,"( ' CNT_FRAME - window left   = ',f9.4)") wl
        WRITE (6,"( ' CNT_FRAME - window right  = ',f9.4)") wr
        WRITE (6,"( ' CNT_FRAME - window bottom = ',f9.4)") wb
        WRITE (6,"( ' CNT_FRAME - window top    = ',f9.4)") wt
        WRITE (6,"( ' CNT_FRAME - frame left    = ',f9.4)") fl
        WRITE (6,"( ' CNT_FRAME - frame right   = ',f9.4)") fr
        WRITE (6,"( ' CNT_FRAME - frame bottom  = ',f9.4)") fb
        WRITE (6,"( ' CNT_FRAME - frame top     = ',f9.4)") ft
        IF (spherical) THEN
          WRITE (6,"( ' CNT_FRAME - left Lon      = ',f9.4)") pllon
          WRITE (6,"( ' CNT_FRAME - right Lon     = ',f9.4)") prlon
          WRITE (6,"( ' CNT_FRAME - bottom Lat    = ',f9.4)") pblat
          WRITE (6,"( ' CNT_FRAME - top Lat       = ',f9.4)") ptlat
        END IF
        WRITE (6,"( ' CNT_FRAME - box Xmin      = ',f9.4)") ul
        WRITE (6,"( ' CNT_FRAME - box Xmax      = ',f9.4)") ur
        WRITE (6,"( ' CNT_FRAME - box Ymin      = ',f9.4)") ub
        WRITE (6,"( ' CNT_FRAME - box Ymax      = ',f9.4/)") ut
      END IF
!
!-----------------------------------------------------------------------
!  Set plotting frame.
!-----------------------------------------------------------------------
!
      CALL set (fl,fr,fb,ft,ul,ur,ub,ut,1)

      RETURN
      END
