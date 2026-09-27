      FUNCTION bcintrp (x,y,ix,iy,f,xf,yf,im,jm,spv,spval)
!
!svn $Id: bcintrp.h 833 2017-01-24 22:20:16Z arango $
!================================================== Hernan G. Arango ===
!  Copyright (c) 2002-2017 The ROMS/TOMS Group                         !
!    Licensed under a MIT/X style license                              !
!    See License_ROMS.txt                                              !
!=======================================================================
!                                                                      !
!  This routine performs a 4-point bicubic interpolation at the        !
!  grid locations X and Y from the 2D grided field F.                  !
!                                                                      !
!  On Input:                                                           !
!                                                                      !
!     x, y     Position at which the field is interpolated (km).       !
!     ix, iy   Indices of the grided cell in which (x,y) lies.         !
!     f        Field to interpolate from.                              !
!     xf, yf   Field positions (km).                                   !
!     im, jm   Inner and outer dimensions of "f".                      !
!     spv      field special value used during checking.               !
!     spval    field special value used during masking.                !
!                                                                      !
!  On Output:                                                          !
!                                                                      !
!     bcintrp  Interpolated value.                                     !
!                                                                      !
!  Calls:                                                              !
!                                                                      !
!    bcucof                                                            !
!                                                                      !
!  Reference :  (Routine adapted from)                                 !
!                                                                      !
!  Press, W.H, B.P. Flannery, S.A. Teukolsky, and W.T. Vetterling,     !
!        1986: Numerical Recipes, the art of scientific computing.     !
!        Cambridge University Press.                                   !
!                                                                      !
!=======================================================================
!
!-----------------------------------------------------------------------
!  Define global data.
!-----------------------------------------------------------------------
!
      implicit none
#include "pconst.h"
!
!-----------------------------------------------------------------------
!  Define local data.
!-----------------------------------------------------------------------
!
      logical bilinear
      integer i, im, ix, iy, jm, n
      integer ic(4), jc(4)
      real dx, dy, dvaldx, dvaldy, spv, spval, val, t, u, x, y
      real c(4,4), dfdx(4), dfdy(4), d2fdxy(4), fc(16), xc(4), yc(4)
      real f(im,jm), xf(im,jm), yf(im,jm)
      real bcintrp
!
!=======================================================================
!  Begin executable code.
!=======================================================================
!
!-----------------------------------------------------------------------
!  If (x,y) is not inside (xf,yf), set interpolation value to special
!  value.
!-----------------------------------------------------------------------
!
      IF ((ix.eq.0).or.(iy.eq.0)) THEN
        bcintrp=spval
        RETURN
      END IF
!
!-----------------------------------------------------------------------
!  Set-up auxiliary table of the field and its derivatives at the four
!  points interpolation grid cell (numbered counterclockwise from the
!  lower left.  Evaluate derivarives using centered differences.
!-----------------------------------------------------------------------
!
!  Set grid cell.
!
      ic(1)=ix
      ic(2)=ix+1
      ic(3)=ix+1
      ic(4)=ix
      jc(1)=iy
      jc(2)=iy
      jc(3)=iy+1
      jc(4)=iy+1
!
!  Extract grid cell raw data.  Bilinear interpolation has a 4-point
!  footprint whereas bicubic has a 16-point footprint.
!
      DO n=1,4
        fc(n)=f (ic(n),jc(n))
        xc(n)=xf(ic(n),jc(n))
        yc(n)=yf(ic(n),jc(n))
      END DO
      fc(5 )=f(MAX(1,ix-1),MIN(jm,iy+1))
      fc(6 )=f(MAX(1,ix-1),iy)
      fc(7 )=f(MAX(1,ix-1),MAX(1,iy-1))
      fc(8 )=f(ix,MAX(1,iy-1))
      fc(9 )=f(MIN(im,ix+1),MAX(1,iy-1))
      fc(10)=f(MIN(im,ix+2),MAX(1,iy-1))
      fc(11)=f(MIN(im,ix+2),iy)
      fc(12)=f(MIN(im,ix+2),MIN(jm,iy+1))
      fc(13)=f(MIN(im,ix+2),MIN(jm,iy+2))
      fc(14)=f(MIN(im,ix+1),MIN(jm,iy+2))
      fc(15)=f(ix,MIN(jm,iy+2))
      fc(16)=f(MAX(1,ix-1),MIN(jm,iy+2))
!
!  If interpolation point is at the corner of the grid cell, avoid
!  interpolation and RETURN value of field at that corner.
!
      DO n=1,4
        IF ((x.eq.xc(n)).and.(y.eq.yc(n))) THEN
          bcintrp=fc(n)
!!        bcintrp=f(ix,iy)
          RETURN
        END IF
      END DO
!
!  If masked raw data, avoid interpolation.
!
      DO n=1,4
        IF (fc(n).ge.spv) THEN
          bcintrp=spval
          RETURN
        END IF
      END DO
!
!  If masked raw data outside cell, try bilinear interpolation.
!
      bilinear=.false.
      DO n=5,16
        IF (fc(n).ge.spv) bilinear=.true.
      END DO
      IF (bilinear) THEN
        dx=p5*((xc(2)-xc(1))+(xc(3)-xc(4)))
        dy=p5*((yc(4)-yc(1))+(yc(3)-yc(2)))
        t=(x-xc(1))/dx
        u=(y-yc(1))/dy
        val=(c1-t)*(c1-u)*fc(1)+t*(c1-u)*fc(2)+t*u*fc(3)+(c1-t)*u*fc(4)
        bcintrp=val
        RETURN
      END IF
!
!  Compute first derivative in the X-direction.
!
      IF (ix.lt.2) THEN
        dfdx(1)=(-c3*f(1,jc(1))+c4*f(2,jc(1))-f(3,jc(1)))/              &
     &          (xf(3,jc(1))-xf(1,jc(1)))
        dfdx(2)=(f (3,jc(2))-f (1,jc(2)))/                              &
     &          (xf(3,jc(2))-xf(1,jc(2)))
        dfdx(3)=(f (3,jc(3))-f (1,jc(3)))/                              &
     &          (xf(3,jc(3))-xf(1,jc(3)))
        dfdx(4)=(-c3*f(1,jc(4))+c4*f(2,jc(4))-f(3,jc(4)))/              &
     &          (xf(3,jc(4))-xf(1,jc(4)))
      ELSE IF (ix.ge.(im-1)) THEN
        dfdx(1)=(f (im,jc(1))-f (im-2,jc(1)))/                          &
     &          (xf(im,jc(1))-xf(im-2,jc(1)))
        dfdx(2)=(c3*f(im,jc(2))-c4*f(im-1,jc(2))+f(im-2,jc(2)))/        &
     &          (xf(im,jc(2))-xf(im-2,jc(2)))
        dfdx(3)=(c3*f(im,jc(3))-c4*f(im-1,jc(3))+f(im-2,jc(3)))/        &
     &          (xf(im,jc(3))-xf(im-2,jc(3)))
        dfdx(4)=(f (im,jc(4))-f (im-2,jc(4)))/                          &
     &          (xf(im,jc(4))-xf(im-2,jc(4)))
      ELSE
        DO n=1,4
          dfdx(n)=(f (ic(n)+1,jc(n))-f (ic(n)-1,jc(n)))/                &
     &            (xf(ic(n)+1,jc(n))-xf(ic(n)-1,jc(n)))
        END DO
      END IF
!
!  Compute first derivative in the Y-direction.
!
      IF (iy.lt.2) THEN
        dfdy(1)=(-c3*f(ic(1),1)+c4*f(ic(1),2)-f(ic(1),3))/              &
     &          (yf(ic(1),3)-yf(ic(1),1))
        dfdy(2)=(-c3*f(ic(2),1)+c4*f(ic(2),2)-f(ic(2),3))/              &
     &          (yf(ic(2),3)-yf(ic(2),1))
        dfdy(3)=(f (ic(3),3)-f (ic(3),1))/                              &
     &          (yf(ic(3),3)-yf(ic(3),1))
        dfdy(4)=(f (ic(4),3)-f (ic(4),1))/                              &
     &          (yf(ic(4),3)-yf(ic(4),1))
      ELSE IF (iy.ge.(jm-1)) THEN
        dfdy(1)=(f (ic(1),jm)-f (ic(1),jm-2))/                          &
     &          (yf(ic(1),jm)-yf(ic(1),jm-2))
        dfdy(2)=(f (ic(2),jm)-f (ic(2),jm-2))/                          &
     &          (yf(ic(2),jm)-yf(ic(2),jm-2))
        dfdy(3)=(c3*f(ic(3),jm)-c4*f(ic(3),jm-1)+f(ic(3),jm-2))/        &
     &          (yf(ic(3),jm)-yf(ic(3),jm-2))
        dfdy(4)=(c3*f(ic(4),jm)-c4*f(ic(4),jm-1)+f(ic(4),jm-2))/        &
     &          (yf(ic(4),jm)-yf(ic(4),jm-2))
      ELSE
        DO n=1,4
          dfdy(n)=(f (ic(n),jc(n)+1)-f (ic(n),jc(n)-1))/                &
     &            (yf(ic(n),jc(n)+1)-yf(ic(n),jc(n)-1))
        END DO
      END IF
!
!  Compute cross derivative.
!
      IF (((2.lt.ix).and.(ix.lt.im-1)).and.                             &
     &    ((2.lt.iy).and.(iy.lt.jm-1))) THEN
        DO n=1,4
          d2fdxy(n)=((f(ic(n)+1,jc(n)+1)-f(ic(n)+1,jc(n)-1))-           &
     &               (f(ic(n)-1,jc(n)+1)-f(ic(n)-1,jc(n)-1)))/          &
     &              ((xf(ic(n)+1,jc(n)-1)-xf(ic(n)-1,jc(n)-1))*         &
     &               (yf(ic(n)+1,jc(n)+1)-yf(ic(n)+1,jc(n)-1)))
        END DO
      ELSE
        DO n=1,4
          d2fdxy(n)=c0
        END DO
      END IF
!
!  Compute length of grid cell.
!
      dx=p5*((xc(2)-xc(1))+(xc(3)-xc(4)))
      dy=p5*((yc(4)-yc(1))+(yc(3)-yc(2)))
!
!-----------------------------------------------------------------------
!  Compute bicubic interpolation coefficients.
!-----------------------------------------------------------------------
!
      CALL bcucof (fc,dfdx,dfdy,d2fdxy,dx,dy,c)
!
!-----------------------------------------------------------------------
!  Interpolate field at requested (x,y).
!-----------------------------------------------------------------------
!
      t=(x-xc(1))/dx
      u=(y-yc(1))/dy
      val=c0
      dvaldx=c0
      dvaldy=c0
      DO i=4,1,-1
        val=t*val+((c(i,4)*u+c(i,3))*u+c(i,2))*u+c(i,1)
        dvaldx=u*dvaldx+(c3*c(4,i)*t+c2*c(3,i))*t+c(2,i)
        dvaldy=t*dvaldy+(c3*c(i,4)*u+c2*c(i,3))*u+c(i,2)
      END DO
      dvaldx=dvaldx/dx
      dvaldy=dvaldy/dy
      bcintrp=val

      RETURN
      END

      SUBROUTINE bcucof (y,y1,y2,y12,d1,d2,c)
!
!=======================================================================
!                                                                      !
!  Given arrays Y, Y1, Y2, and Y12, each of length 4, containing the   !
!  the function, gradients,  and cross derivative at the four points   !
!  of a rectangular grid cell  (numbered counterclockwise from lower   !
!  left), and given D1 and D2, the length of the grid cell in the 1-   !
!  and 2-directions,  this routine  returns the table C that is used   !
!  by routine BCINTRP for bicubic interpolation.                       !
!                                                                      !
!  Reference :                                                         !
!                                                                      !
!  Press, W.H, B.P. Flannery, S.A. Teukolsky, and W.T. Vetterling,     !
!        1986: Numerical Recipes, the art of scientific computing.     !
!        Cambridge University Press.                                   !
!                                                                      !
!=======================================================================
!
!-----------------------------------------------------------------------
!  Define local variables.
!-----------------------------------------------------------------------
!
      implicit none
      integer i, j, k, l
      real d1, d1d2, d2, xx
      real c(4,4), cl(16), wt(16,16), x(16), y(4), y1(4),y2(4), y12(4)
      data wt / 1.,0.,-3.,2.,4*0.,-3.,0.,9.,-6.,2.,0.,-6.,              &
     &          4.,8*0.,3.,0.,-9.,6.,-2.,0.,6.,-4.,10*0.,9.,-6.,        &
     &          2*0.,-6.,4.,2*0.,3.,-2.,6*0.,-9.,6.,2*0.,6.,-4.,        &
     &          4*0.,1.,0.,-3.,2.,-2.,0.,6.,-4.,1.,0.,-3.,2.,8*0.,      &
     &          -1.,0.,3.,-2.,1.,0.,-3.,2.,10*0.,-3.,2.,2*0.,3.,        &
     &          -2.,6*0.,3.,-2.,2*0.,-6.,4.,2*0.,3.,-2.,0.,1.,-2.,      &
     &          1.,5*0.,-3.,6.,-3.,0.,2.,-4.,2.,9*0.,3.,-6.,3.,0.,      &
     &          -2.,4.,-2.,10*0.,-3.,3.,2*0.,2.,-2.,2*0.,-1.,1.,        &
     &          6*0.,3.,-3.,2*0.,-2.,2.,5*0.,1.,-2.,1.,0.,-2.,4.,       &
     &          -2.,0.,1.,-2.,1.,9*0.,-1.,2.,-1.,0.,1.,-2.,1.,10*0.,    &
     &          1.,-1.,2*0.,-1.,1.,6*0.,-1.,1.,2*0.,2.,-2.,2*0.,-1.,1. /
!
!=======================================================================
!  Begin executable code.
!=======================================================================
!
      d1d2=d1*d2
!
!  Pack a temporary vector X.
!
      DO i=1,4
        x(i)=y(i)
        x(i+4)=y1(i)*d1
        x(i+8)=y2(i)*d2
        x(i+12)=y12(i)*d1d2
      END DO
!
!  Matrix multiply by the stored table.
!
      DO i=1,16
        xx=0.0
        DO k=1,16
          xx=xx+wt(i,k)*x(k)
        END DO
        cl(i)=xx
      END DO
!
!  Unpack the result into the output table.
!
      l=0
      DO i=1,4
        DO j=1,4
          l=l+1
          c(i,j)=cl(l)
        END DO
      END DO

      RETURN
      END
