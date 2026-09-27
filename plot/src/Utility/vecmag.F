      SUBROUTINE vecmag(vmag,u,v,im,m,n,vmin,vmax,spv,spval)
!
!svn $Id: vecmag.F 833 2017-01-24 22:20:16Z arango $
!================================================== Hernan G. Arango ===
!  Copyright (c) 2002-2017 The ROMS/TOMS Group                         !
!    Licensed under a MIT/X style license                              !
!    See License_ROMS.txt                                              !
!=======================================================================
!                                                                      !
!  Given a vector field with components U and V, this routine          !
!  computes the vector magnitude  VMAG  and finds the minimum          !
!  and maximum (VMIN,VMAX) vector magnitude values.                    !
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
       integer i,im,ip,j,n,m
       real spv,spval,vmax,vmin
       real u(*),v(*),vmag(*)
!
!=======================================================================
!  Begin executable code.
!=======================================================================
!
      vmin=spval1
      vmax=-spval1
      DO j=1,n
        DO i=1,m
          ip=i+(j-1)*im
          IF ((u(ip).ge.spv).or.(v(ip).ge.spv)) THEN
            vmag(ip)=spval
          ELSE
            vmag(ip)=SQRT(u(ip)*u(ip)+v(ip)*v(ip))
            vmin=MIN(vmin,vmag(ip))
            vmax=MAX(vmax,vmag(ip))
          END IF
        END DO
      END DO

      RETURN
      END
