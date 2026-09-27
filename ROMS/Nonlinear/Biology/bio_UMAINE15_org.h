      SUBROUTINE biology (ng,tile)
!
!************************************************** Hernan G. Arango ***
!  Copyright (c) 2002-2008 The ROMS/TOMS Group                         !
!    Licensed under a MIT/X style license                              !
!    See License_ROMS.txt                                              !
!***********************************************************************
!                                                                      !
!  UMaine CoSiNE ecosystem model 15                                    !
!                                                                      !
!  This routine computes the biological sources and sinks and adds     !
!  then the global biological fields. The model is based on the        !
!  Carbon, Silicon, Nitrogen Ecosystem (CoSiNE) model (Chai et al.,    !
!  2002). The model state variables are:                               !
!                                                                      !
!    iNO3_      Nitrate                                                !  
!    iSiOH      Silicate                                               !
!    iNH4_      Ammonium                                               !
!    iSphy      Small Phytoplankton                                    !
!    iLphy      Diatoms                                                !
!    iSzoo      Micro Zooplankton                                      !
!    iLphy      Meso Zooplankton                                       !
!    iSDet      Detritus-nitrogen                                      !
!    iopal      Detritus-silicate                                      !
!    iPH4_      Phosphate                                              !
!    iOxyg      Dissolved Oxygen                                       !
!    ITIC_      Total CO2                                              !
!    iTAlk      Total Alkalinity                                       !
!    iChl1      Chlorophyll for Sphy                                   !
!    iChl2      Chlorophyll for diatom                                 !
!                                                                      !
!  Please cite:                                                        !
!                                                                      !
!    Chai, F., R.C. Dugdale, T-H Peng, F.P.Wilkerson, and R.T. Barber  !
!      (2002): One dimensional Ecosystem Model of the Equatorial       !
!      Pacific Upwelling System, Part I: Model Development and Silicon !
!      and Nitorgen Cycle. Deep-Sea Res. II, Vol. 49, No. 13-14,       !
!      2713-2745.                                                      !
!                                                                      !
!    Xiu, P., and F. Chai, 2011. Modeled biogeochemical responses to   !
!       mesoscale eddies in the South China Sea, Journal of Geophysical! 
!       Research, 116, C10006, doi:10.1029/2010JC006800.               !
!                                                                      !
!  Adapted from 1D code develped by Fei Chai of UMaine, and further    !
!  modified and developed by Lei Shi and adding 4 more biological      !
!  statevariables                                                      !
!                                                                      !
!  Last: PENG XIU 10/2016                                                !
!  Last: Qicheng Meng 06/2020 	                                       !
!                                                                      !
!  Options you can use: OXYGEN, CARBON, SINK_OP1, SINK_OP2             !
!                       TALK_NONCONSERV,DIAGNOSTICS_BIO                !
!  Feng Zhou (06/2014)                                                 !
!     add options:  BIO_SEDIMENT, with/without DENITRIFICATION         !
!     add change in light availability in extreme shallow regions      !
!***********************************************************************                                
!
      USE mod_param
#ifdef DIAGNOSTICS_BIO
      USE mod_diags
#endif
      USE mod_forces
      USE mod_grid
      USE mod_ncparam
      USE mod_ocean
      USE mod_stepping
!
!  Imported variable declarations.
!
      integer, intent(in) :: ng, tile
!
!  Local variable declarations.
!
#include "tile.h"
!
!  Set header file name.
!
#ifdef DISTRIBUTE
      IF (Lbiofile(iNLM)) THEN
#else
      IF (Lbiofile(iNLM).and.(tile.eq.0)) THEN
#endif
        Lbiofile(iNLM)=.FALSE.
        BIONAME(iNLM)=__FILE__
      END IF
!
#ifdef PROFILE
      CALL wclock_on (ng, iNLM, 15)
#endif
      CALL biology_tile (ng, tile,                                     &
     &                   LBi, UBi, LBj, UBj, N(ng), NT(ng),            &
     &                   IminS, ImaxS, JminS, JmaxS,                   &
     &                   nstp(ng), nnew(ng),                           &
#ifdef MASKING
     &                   GRID(ng) % rmask,                             &
# if defined WET_DRY && defined DIAGNOSTICS_BIO
     &                   GRID(ng) % rmask_io,                          &
# endif
#endif
     &                   GRID(ng) % Hz,                                &
     &                   GRID(ng) % H,                                 &
     &                   GRID(ng) % z_r,                               &
     &                   GRID(ng) % z_w,                               &
     &                   FORCES(ng) % srflx,                           &
#if defined OXYGEN || defined CARBON
# ifdef BULK_FLUXES
     &                   FORCES(ng) % Uwind,                           &
     &                   FORCES(ng) % Vwind,                           &
# else
     &                   FORCES(ng) % sustr,                           &
     &                   FORCES(ng) % svstr,                           &
# endif
#endif
#ifdef CARBON
     &                   OCEAN(ng) % pH,                               &
#endif
#ifdef DIAGNOSTICS_BIO
     &                   DIAGS(ng) % DiaBio2d,                         &
     &                   DIAGS(ng) % DiaBio3d,                         &
#endif
     &                   OCEAN(ng) % t)

#ifdef PROFILE
      CALL wclock_off (ng, iNLM, 15)
#endif
      RETURN
      END SUBROUTINE biology
!
!
!
!
!
!
!
!
!-----------------------------------------------------------------------
      SUBROUTINE biology_tile (ng, tile,                               &
     &                         LBi, UBi, LBj, UBj, UBk, UBt,           &
     &                         IminS, ImaxS, JminS, JmaxS,             &
     &                         nstp, nnew,                             &
#ifdef MASKING
     &                         rmask,                                  &
# if defined WET_DRY && defined DIAGNOSTICS_BIO
     &                         rmask_io,                               &
# endif
#endif
     &                         Hz,hh, z_r, z_w, srflx,                 &
#if defined OXYGEN || defined CARBON
# ifdef BULK_FLUXES
     &                         Uwind, Vwind,                           &
# else
     &                         sustr, svstr,                           &
# endif
#endif
#ifdef CARBON
     &                         pH,                                     &
#endif
#ifdef DIAGNOSTICS_BIO
     &                         DiaBio2d, DiaBio3d,                     &
#endif
     &                         t)
!-----------------------------------------------------------------------
!
      USE mod_param
      USE mod_biology
      USE mod_ncparam
      USE mod_scalars
      USE mod_parallel
      USE mod_sources     ! Peng Xiu
!
!  Imported variable declarations.
!
      integer, intent(in) :: ng, tile
      integer, intent(in) :: LBi, UBi, LBj, UBj, UBk, UBt
      integer, intent(in) :: IminS, ImaxS, JminS, JmaxS
      integer, intent(in) :: nstp, nnew

#ifdef ASSUMED_SHAPE
# ifdef MASKING
      real(r8), intent(in) :: rmask(LBi:,LBj:)
#  if defined WET_DRY && defined DIAGNOSTICS_BIO
      real(r8), intent(in) :: rmask_io(LBi:,LBj:)
#  endif      
# endif
      real(r8), intent(in) :: Hz(LBi:,LBj:,:)
      real(r8), intent(in) :: hh(LBi:,LBj:)
      real(r8), intent(in) :: z_r(LBi:,LBj:,:)
      real(r8), intent(in) :: z_w(LBi:,LBj:,0:)
      real(r8), intent(in) :: srflx(LBi:,LBj:)
# if defined OXYGEN || defined CARBON
#  ifdef BULK_FLUXES
      real(r8), intent(in) :: Uwind(LBi:,LBj:)
      real(r8), intent(in) :: Vwind(LBi:,LBj:)
#  else
      real(r8), intent(in) :: sustr(LBi:,LBj:)
      real(r8), intent(in) :: svstr(LBi:,LBj:)
#  endif
# endif
# ifdef CARBON
      real(r8), intent(inout) :: pH(LBi:,LBj:)
# endif
# ifdef DIAGNOSTICS_BIO
      real(r8), intent(inout) :: DiaBio2d(LBi:,LBj:,:)
      real(r8), intent(inout) :: DiaBio3d(LBi:,LBj:,:,:)
# endif
      real(r8), intent(inout) :: t(LBi:,LBj:,:,:,:)
      
#else

# ifdef MASKING
      real(r8), intent(in) :: rmask(LBi:UBi,LBj:UBj)
#  if defined WET_DRY && defined DIAGNOSTICS_BIO
      real(r8), intent(in) :: rmask_io(LBi:UBi,LBj:UBj)
#  endif      
# endif
      real(r8), intent(in) :: Hz(LBi:UBi,LBj:UBj,UBk)
      real(r8), intent(in) :: hh(LBi:UBi,LBj:UBj,UBk)
      real(r8), intent(in) :: z_r(LBi:UBi,LBj:UBj,UBk)
      real(r8), intent(in) :: z_w(LBi:UBi,LBj:UBj,0:UBk)
      real(r8), intent(in) :: srflx(LBi:UBi,LBj:UBj)
# if defined OXYGEN || defined CARBON
#  ifdef BULK_FLUXES
      real(r8), intent(in) :: Uwind(LBi:UBi,LBj:UBj)
      real(r8), intent(in) :: Vwind(LBi:UBi,LBj:UBj)
#  else
      real(r8), intent(in) :: sustr(LBi:UBi,LBj:UBj)
      real(r8), intent(in) :: svstr(LBi:UBi,LBj:UBj)
#  endif
# endif
# ifdef CARBON
      real(r8), intent(inout) :: pH(LBi:UBi,LBj:UBj)
# endif
# ifdef DIAGNOSTICS_BIO
      real(r8), intent(inout) :: DiaBio2d(LBi:UBi,LBj:UBj,NDbio2d)
      real(r8), intent(inout) :: DiaBio3d(LBi:UBi,LBj:UBj,UBk,NDbio3d)
# endif
      real(r8), intent(inout) :: t(LBi:UBi,LBj:UBj,UBk,3,UBt)
      
#endif
!
!  Local variable declarations.
!
      integer, parameter :: Nsink = 6
#if defined OXYGEN || defined CARBON
      real(r8) :: u10squ, u10spd
#endif

      integer :: Iter, i, indx, isink, ibio, ivar, j, k, ks,is,ii,jj

      integer, dimension(Nsink) :: idsink

      real(r8), parameter :: Minval = 1.0e-6_r8
      real(r8), parameter :: zeptic = 1000000.0_r8	!100.0_r8  ! Feng Zhou

      real(r8) :: dtdays

      real(r8) :: cff, cff0, cff1,cff2,cff3,cff4,cff5 ! Feng Zhou

      real(r8), dimension(Nsink) :: Wbio

      real(r8), dimension(IminS:ImaxS) :: PARsur

#if defined OXYGEN || defined CARBON
      real(r8), dimension(IminS:ImaxS) :: kw660
#endif

#ifdef OXYGEN
      real(r8), dimension(IminS:ImaxS) :: o2sat
      real(r8), dimension(IminS:ImaxS) :: o2flx
#endif

#ifdef CARBON
      real(r8), dimension(IminS:ImaxS) :: co2flx
      real(r8), dimension(IminS:ImaxS) :: pco2s
#endif

      integer, parameter :: mmax = 31
      real(r8), dimension(IminS:ImaxS,N(ng),NT(ng)) :: Bio
      real(r8), dimension(IminS:ImaxS,N(ng),NT(ng)) :: Bio_bak

      real(r8), dimension(IminS:ImaxS,N(ng)) :: Hz_inv
      real(r8), dimension(IminS:ImaxS,N(ng)) :: Hz_inv2
      real(r8), dimension(IminS:ImaxS,N(ng)) :: Hz_inv3
      real(r8), dimension(IminS:ImaxS,N(ng)) :: hzl

      real(r8), dimension(IminS:ImaxS,N(ng)+1) :: PIO
      real(r8), dimension(IminS:ImaxS,N(ng)) :: PAR
      real(r8), dimension(IminS:ImaxS,N(ng)) :: ADPT, cffik    ! Feng Zhou, cffik
      real(r8), dimension(IminS:ImaxS) :: cffzhou              ! Feng Zhou, cffzhou

      real(r8), dimension(IminS:ImaxS,N(ng),mmax) :: a_abs
      real(r8), dimension(IminS:ImaxS,N(ng),mmax) :: bbp
      real(r8), dimension(IminS:ImaxS,N(ng),mmax) :: bb
      real(r8), dimension(IminS:ImaxS,N(ng),mmax) :: bts
      real(r8), dimension(IminS:ImaxS,N(ng)) :: kdpar
      
      
      real(r8), dimension(N(ng)) :: sinkindx

      real(r8) :: cents1,cens1,cens2,cents2,cent1
      real(r8) :: thick,upo4s1,uco2s1,pnh4s2,OXR, Q10
      real(r8) :: alts1, alts2, grows1, grows2,unh4s1,pnh4s1
      real(r8) :: uno3s2, unh4s2, uno3s1,usio4s2,upo4s2,uco2s2
      real(r8) :: xco2_in,gno3s1,gnh4s1,gno3s2,gnh4s2,gsio4s2
      real(r8) :: nps1,rps1,nps2,rps2,morts1,morts2,excrz1
      real(r8) :: excrz2,remvz2,nitrif,midd,middsi
      real(r8) :: gs1zz1,ro8,ro9,gs2zz2,gzz1zz2,gddzz2,gtzz2
      real(r8) :: Qsms1,Qsms2,Qsms3,Qsms4,Qsms5,Qsms6,Qsms7
      real(r8) :: Qsms8,Qsms9,Qsms10,Qsms11,Qsms12,Qsms13
      real(r8) :: NQsms1,NQsms2,NQsms3,NQsms4,NQsms5,NQsms6,NQsms7
      real(r8) :: NQsms8,NQsms9,NQsms10,NQsms11,NQsms12,NQsms13
      real(r8) :: sms1,sms2,sms3,sms4,sms5,sms6,sms7,SOC
      real(r8) :: sms8,sms9,sms10,sms11,sms12,sms13
      
      real(r8) :: npchl1,npchl2,gchl1zz1,gchl2zz2,morchl1,morchl2
      real(r8) :: Qsms14,Qsms15,NQsms14,NQsms15,sms14,sms15
      real(r8) :: Chl2C_s1,Chl2C_s2,excrz1_2,excrz2_2,II00
      real(r8) :: aggregs1,aggregs2,aggregchl1,aggregchl2
      
      real(r8) :: cffL, cffR, cu, dltL, dltR
      integer, dimension(IminS:ImaxS,N(ng)) :: ksource
      real(r8), dimension(IminS:ImaxS,0:N(ng)) :: FC
      real(r8), dimension(IminS:ImaxS,N(ng)) :: WL
      real(r8), dimension(IminS:ImaxS,N(ng)) :: WR
      real(r8), dimension(IminS:ImaxS,N(ng)) :: bL
      real(r8), dimension(IminS:ImaxS,N(ng)) :: bR
      real(r8), dimension(IminS:ImaxS,N(ng)) :: qc

#include "set_bounds.h"
!
#ifdef DIAGNOSTICS_BIO
!
!-----------------------------------------------------------------------
! If appropriate, initialize time-averaged diagnostic arrays.
!-----------------------------------------------------------------------
!
      IF (((iic(ng).gt.ntsDIA(ng)).and.                                &
     &     (MOD(iic(ng),nDIA(ng)).eq.1)).or.                           &
     &    ((iic(ng).ge.ntsDIA(ng)).and.(nDIA(ng).eq.1)).or.            &
     &    ((nrrec(ng).gt.0).and.(iic(ng).eq.ntstart(ng)))) THEN
        DO ivar=1,NDbio2d
          DO j=Jstr,Jend
            DO i=Istr,Iend
              DiaBio2d(i,j,ivar)=0.0_r8
            END DO
          END DO
        END DO
        DO ivar=1,NDbio3d
          DO k=1,N(ng)
            DO j=Jstr,Jend
              DO i=Istr,Iend
                DiaBio3d(i,j,k,ivar)=0.0_r8
              END DO
            END DO
          END DO
        END DO
      END IF
#endif
!
!-----------------------------------------------------------------------
!  Add biological Source/Sink terms.
!-----------------------------------------------------------------------
!
!  Set time-stepping according to the number of iterations.
!
      dtdays=dt(ng)*sec2day/REAL(BioIter(ng),r8)
!
!  Set vertical sinking indentification vector.
!
      idsink(1)=iLphy
      idsink(2)=iSDet
      idsink(3)=iopal
      idsink(4)=iSphy
      idsink(5)=iChl1
      idsink(6)=iChl2
!
!  Set vertical sinking velocity vector in the same order as the
!  identification vector, IDSINK.
!
      Wbio(1)=wsp(ng)                ! iLphy
      Wbio(2)=wsd(ng)                ! iSDet
      Wbio(3)=wsdsi(ng)              ! iopal
      Wbio(4)=0.0_r8                ! iSphy
      Wbio(5)=0.0_r8                ! iSphy ichl1
      Wbio(6)=wsp(ng)                ! iLphy ichl2
!
!  Compute inverse thickness to avoid repeated divisions.
!
      J_LOOP : DO j=Jstr,Jend
      
      
        DO k=1,N(ng)
          DO i=Istr,Iend
            hzl(i,k)=Hz(i,j,k)
            Hz_inv(i,k)=1.0_r8/Hz(i,j,k)
          END DO
        END DO
        DO k=1,N(ng)-1
          DO i=Istr,Iend
            Hz_inv2(i,k)=1.0_r8/(Hz(i,j,k)+Hz(i,j,k+1))
          END DO
        END DO
        DO k=2,N(ng)-1
          DO i=Istr,Iend
            Hz_inv3(i,k)=1.0_r8/(Hz(i,j,k-1)+Hz(i,j,k)+Hz(i,j,k+1))
          END DO
        END DO
!
!  Extract biological variables from tracer arrays, place them into
!  scratch arrays, and restrict their values to be positive definite.
!  At input, all tracers (index nnew) from predictor step have
!  transport units (m Tunits) since we do not have yet the new
!  values for zeta and Hz. These are known after the 2D barotropic
!  time-stepping.
!
        DO ibio=1,NBT
          indx=idbio(ibio)
          DO k=1,N(ng)
            DO i=Istr,Iend
              Bio_bak(i,k,indx)=MAX(t(i,j,k,nstp,indx),0.000001_r8)
              Bio(i,k,indx)=Bio_bak(i,k,indx)
            END DO
          END DO
        END DO
! Meng 20200711 start       
        DO k=1,N(ng)
          DO i=Istr,Iend
            Bio_bak(i,k,iNO3_)=MAX(t(i,j,k,nstp,iNO3_),0.0001_r8)
            Bio_bak(i,k,iNO3_)=MIN(Bio_bak(i,k,iNO3_),200.0_r8)
            Bio(i,k,iNO3_)=Bio_bak(i,k,iNO3_)
            Bio_bak(i,k,iSiOH)=MAX(t(i,j,k,nstp,iSiOH),0.0001_r8)            
            Bio_bak(i,k,iSiOH)=MIN(Bio_bak(i,k,iSiOH),200.0_r8)
            Bio(i,k,iSiOH)=Bio_bak(i,k,iSiOH)
            Bio_bak(i,k,iNH4_)=MAX(t(i,j,k,nstp,iNH4_),0.0001_r8)            
            Bio_bak(i,k,iNH4_)=MIN(Bio_bak(i,k,iNH4_),20.0_r8)
            Bio(i,k,iNH4_)=Bio_bak(i,k,iNH4_)
            Bio_bak(i,k,iSphy)=MAX(t(i,j,k,nstp,iSphy),0.0_r8)            
!            Bio_bak(i,k,iSphy)=MIN(Bio_bak(i,k,iSphy),10.0_r8)
            Bio(i,k,iSphy)=Bio_bak(i,k,iSphy)
            Bio_bak(i,k,iLphy)=MAX(t(i,j,k,nstp,iLphy),0.0_r8)            
!            Bio_bak(i,k,iLphy)=MIN(Bio_bak(i,k,iLphy),10.0_r8)
            Bio(i,k,iLphy)=Bio_bak(i,k,iLphy)
            Bio_bak(i,k,iSDet)=MAX(t(i,j,k,nstp,iSDet),0.0001_r8)            
            Bio_bak(i,k,iSDet)=MIN(Bio_bak(i,k,iSDet),100.0_r8)
            Bio(i,k,iSDet)=Bio_bak(i,k,iSDet)
            Bio_bak(i,k,iopal)=MAX(t(i,j,k,nstp,iopal),0.0001_r8)            
            Bio_bak(i,k,iopal)=MIN(Bio_bak(i,k,iopal),400.0_r8)
            Bio(i,k,iopal)=Bio_bak(i,k,iopal)
            Bio_bak(i,k,iPO4_)=MAX(t(i,j,k,nstp,iPO4_),0.0001_r8)            
            Bio_bak(i,k,iPO4_)=MIN(Bio_bak(i,k,iPO4_),10.0_r8)
            Bio(i,k,iPO4_)=Bio_bak(i,k,iPO4_)
            Bio_bak(i,k,iOxyg)=MAX(t(i,j,k,nstp,iOxyg),0.0_r8)            
            Bio_bak(i,k,iOxyg)=MIN(Bio_bak(i,k,iOxyg),800.0_r8)
            Bio(i,k,iOxyg)=Bio_bak(i,k,iOxyg)
!            Bio_bak(i,k,iChl1)=MAX(t(i,j,k,nstp,iChl1),0.001_r8)
!            Bio(i,k,iChl1)=Bio_bak(i,k,iChl1)
!            Bio_bak(i,k,iChl2)=MAX(t(i,j,k,nstp,iChl2),0.001_r8)
!            Bio(i,k,iChl2)=Bio_bak(i,k,iChl2)
          END DO
        END DO
! Meng 20200711 end
#ifdef CARBON
        DO k=1,N(ng)
          DO i=Istr,Iend
            Bio_bak(i,k,iTIC_)=MIN(Bio(i,k,iTIC_),3000.0_r8)
            Bio_bak(i,k,iTIC_)=MAX(Bio_bak(i,k,iTIC_),400.0_r8)
            Bio(i,k,iTIC_)=Bio_bak(i,k,iTIC_)
          END DO
        END DO
#endif
#ifdef OXYGEN
        DO k=1,N(ng)
          DO i=Istr,Iend
              Bio_bak(i,k,iOxyg)=MIN(Bio(i,k,iOxyg),600.0_r8) !Feng Zhou 800.0_r8 Peng Xiu
              Bio(i,k,iOxyg)=Bio_bak(i,k,iOxyg)
           END DO
         END DO  
#endif
!
!  Extract potential temperature and salinity.
!

        DO k=1,N(ng)
          DO i=Istr,Iend
!            Bio(i,k,itemp)=MIN(t(i,j,k,nstp,itemp),35.0_r8)
!            Bio(i,k,isalt)=MAX(t(i,j,k,nstp,isalt), 0.0_r8)
            Bio(i,k,itemp)=t(i,j,k,nstp,itemp)
            Bio(i,k,isalt)=t(i,j,k,nstp,isalt)

!adjust river points if needed            
!            DO is=1,Nsrc(ng)
!              ii=SOURCES(ng)%Isrc(is)
!              jj=SOURCES(ng)%Jsrc(is)
!             if((rmask(ii,jj)==1).and.(ii==i).and.(jj==j)) then
!                if(Bio_bak(i,k,iTIC_) .lt. 1800.0) then
!                   Bio_bak(i,k,iTIC_)=1800.0
!                 endif
!              endif
!            END DO
 
          Bio(i,k,iTIC_)=MAX(t(i,j,k,nstp,iTIC_), 1800.0_r8)
                  
          END DO
        END DO


!  Calculate surface Photosynthetically Available Radiation (PAR).  The
!  net shortwave radiation is scaled back to Watts/m2 and multiplied by
!  the fraction that is photosynthetically available, PARfrac.
!
        DO i=Istr,Iend
          PARsur(i)=PARfrac(ng)*srflx(i,j)*rho0*Cp
        END DO
!
!=======================================================================
!  Start internal iterations to achieve convergence of the nonlinear
!=======================================================================
!
!
!  The iterative loop below is to iterate toward an universal Backward-
!  Euler treatment of all terms. So if there are oscillations in the
!  system, they are only physical oscillations. These iterations,
!  however, do not improve the accuaracy of the solution.
!
        ITER_LOOP: DO Iter=1,BioIter(ng)
!
!-----------------------------------------------------------------------
!  Light-limited computations.
!-----------------------------------------------------------------------
!
!  Compute attenuation coefficient based on the concentration of
!  microphytoplankotn and diatom concentration within each grid box. 
!  Then, photosynthetically available radiation (PAR) are calculated
!  as the depth-averaged PAR within the vertical grid. Since the light
!  penetreated from surface to bottom, so the PAR are calculated in the 
!  same order. PARsur is surface PAR value. PIO is the PAR value at 
!  surface or bottom of a vertical grid, or at w location vertically.
!
! All replaced by Feng Zhou

!#ifdef OPTICS_OP1
!      call optic_property(Istr, Iend, ng,                               &
!     &                       LBi, UBi, LBj, UBj, UBk,                   &
!     &                       IminS, ImaxS, j,                           &
!#  ifdef MASKING
!     &                       rmask,                                     &
!#  endif
!     &                       Bio(IminS:,1:,isalt), hzl,                 &
!     &                       Bio(IminS:,1:,iChl1), Bio(IminS:,1:,iChl2),&                                   
!     &                       Bio(IminS:,1:,iSphy), Bio(IminS:,1:,iLphy),&                  
!     &                       Bio(IminS:,1:,iSDet),                      &                              
!     &                       a_abs, bbp, bb, bts, kdpar)
!#else

!      DO i=Istr,Iend
!        do k=1,N(ng)
!           kdpar(i,k)=100.0_r8
!        end do
!      END DO
      
!#endif



!    DO i=Istr,Iend
!      PIO(i,N(ng)+1)=PARsur(i)
!      IF (PIO(i,N(ng)+1) .lt. 0.0_r8) PIO(i,N(ng)+1)=0.0_r8
!      DO k=N(ng),1,-1
!         if(kdpar(i,k).ge.0.001_r8.and.kdpar(i,k).le.10.0_r8)then
!             cff1=kdpar(i,k)*HZ(i,j,k)
!        else
        
!!       cff1=(AK1(ng)+(Bio(i,k,iSphy)+Bio(i,k,iLphy))*AK2(ng))*HZ(i,j,k)
!       cff1=(AK1(ng)+(Bio(i,k,iChl1)+Bio(i,k,iChl2))*AK2(ng))*HZ(i,j,k)
!        endif
!       
!       PIO(i,K)=PIO(i,K+1)*EXP(-cff1)
!       PAR(i,K)=(PIO(i,K+1)-PIO(i,K))/cff1
!       ADPT(i,K) = 1.0_r8-4.0_r8*z_r(i,j,k)/zeptic
!      END DO
!    END DO

!       DO k=1,N(ng)
!          DO i=Istr,Iend

    DO i=Istr,Iend
      PIO(i,N(ng)+1)=PARsur(i)
      IF (PIO(i,N(ng)+1) .lt. 0.0_r8) PIO(i,N(ng)+1)=0.0_r8
      DO k=N(ng),1,-1
        cff1=(AK1(ng)+(Bio(i,k,iSphy)+Bio(i,k,iLphy))*AK2(ng))*HZ(i,j,k)
        PIO(i,K)=PIO(i,K+1)*EXP(-cff1)
        PAR(i,K)=(PIO(i,K+1)-PIO(i,K))/cff1
        ADPT(i,K) = 1.0_r8-4.0_r8*z_r(i,j,k)/zeptic

        !Feng Zhou
        cff2 = (abs(z_w(i,j,1))-10.0_r8)/8.0_r8
        cff2 = max(cff2,-0.25_r8);
        if (abs(z_w(i,j,1)) .lt. 10) then
            cffik(i,K) = float(k)/float(N(ng)) * min(1.0_r8,exp(cff2))
! Meng 20220512
!            cffik(i,K) = float(k)/float(N(ng)) * min(1.0_r8,exp(cff2))*abs(z_w(i,j,1))/15.0_r8
        else
            cffik(i,K) = 1.0_r8
        endif

      END DO
      
!      !Feng Zhou
!        cff2 = (abs(z_r(i,j,1))-14.0_r8)/8.0_r8
!        cff2 = max(cff2,-1.00_r8);
!        cffzhou(i) = min(1.0_r8,exp(cff2))

    END DO


    cff5 = 1          ! for diagnostic code only

       DO k=1,N(ng)
          DO i=Istr,Iend


!
!-----------------------------------------------------------------------
!     CALCULATING the temperature dependence of biology processes
!-----------------------------------------------------------------------
! different options
     Q10=exp(0.069_r8*(Bio(i,k,itemp)-25.0_r8))    !Feng Zhou
!    Q10=exp(0.069_r8*(Bio(i,k,itemp)-25.0_r8))
!   Q10 = 1.0_r8                                   !Peng Xiu
!    Q10=exp(-4000.0_r8*(1.0_r8/(Bio(i,k,itemp)+273.15_r8)-  &
!   &  1.0_r8/303.15_r8))
!
!-----------------------------------------------------------------------
!     CALCULATING THE OXIDATION RATE OF ORGANIC MATTER
!-----------------------------------------------------------------------
!
!  Any biology processes that consume oxygen will be limited by the
!  availability of dissolved oxygen except the bottom layer.
!
#ifdef OXYGEN
! different half saturation of OX for nitrification and bottom denitrification
      if(k .gt. 1)then
         OXR = Bio(i,k,iOxyg)/(Bio(i,k,iOxyg)+AKOX(ng)) ! Feng Zhou      	
!        OXR=max((Bio(i,k,iOxyg)-6.0_r8)/(Bio(i,k,iOxyg)+AKOX(ng)-6.0_r8),0.0_r8)
      else
      	 OXR = 1.0_r8 ! Feng Zhou
!        OXR=max((Bio(i,k,iOxyg)-6.0_r8)/(Bio(i,k,iOxyg)+3.0_r8  -6.0_r8),0.0_r8) !Fennel 2013
      endif
#else
      OXR = 1.0_r8
#endif
!
!-----------------------------------------------------------------------
!     CALCULATING THE GROWTH RATE AS NO3,NH4, AND LIGHT;
!     GRAZING, PARTICLE SINKING AND REGENERATION
!-----------------------------------------------------------------------
!
!-----------------------------------------------------------------------
!  small phytoplankton nutrient uptake, and growth
!-----------------------------------------------------------------------
!
!!  nitrogen limited growth
!	pnh4s1=exp(-pis1(ng)*Bio(i,k,iNH4_))
!	cens1=1.0_r8+Bio(i,k,iNH4_)/aknh4s1(ng)+                           &
!     &       pnh4s1*Bio(i,k,iNO3_)/akno3s1(ng)
!    uno3s1 = pnh4s1*Bio(i,k,iNO3_)/(akno3s1(ng)*cens1)
!    unh4s1 = Bio(i,k,iNH4_)/(aknh4s1(ng)*cens1)
!
!!  phosphate limited growth
!	upo4s1=Bio(i,k,iPO4_)/(akpo4s1(ng)+Bio(i,k,iPO4_))
!	
!!  tco2 limited growth
!#ifdef CARBON
!	uco2s1=Bio(i,k,iTIC_)/(akco2s1(ng)+Bio(i,k,iTIC_))
!#else
!	uco2s1=1.0_r8
!#endif
!
!! light
!    alts1= 1.0_r8 - exp(-PAR(i,k)*ADPT(i,k)/parsats1(ng))
!    
!! growth
!    
!    grows1=min(uno3s1+unh4s1,upo4s1,uco2s1)*alts1
!
!! adjustments
!
!    cents1=grows1/(uno3s1+unh4s1+Minval)
!    uno3s1=cents1*uno3s1
!    unh4s1=cents1*unh4s1
!    upo4s1=grows1
!    uco2s1=grows1

!change Peng-------------------

!  nitrogen limited growth
	pnh4s1=exp(-pis1(ng)*Bio(i,k,iNH4_))

!another option
!    pnh4s1=	1.0_r8/(1.0_r8+Bio(i,k,iNH4_)/aknh4s1(ng))
	cens1=1.0_r8+Bio(i,k,iNH4_)/aknh4s1(ng)+                           &
     &       pnh4s1*Bio(i,k,iNO3_)/akno3s1(ng)	                  ! Feng Zhou
    uno3s1 = pnh4s1*Bio(i,k,iNO3_)/(akno3s1(ng)*cens1)            ! Feng Zhou
    unh4s1 = Bio(i,k,iNH4_)/(aknh4s1(ng)*cens1)                   ! Feng Zhou
!    uno3s1 = pnh4s1*Bio(i,k,iNO3_)/(akno3s1(ng)+Bio(i,k,iNO3_))
!    unh4s1 = Bio(i,k,iNH4_)/(aknh4s1(ng)+Bio(i,k,iNH4_))

!  phosphate limited growth
	upo4s1=Bio(i,k,iPO4_)/(akpo4s1(ng)+Bio(i,k,iPO4_))
	
!  tco2 limited growth
#ifdef CARBON
	uco2s1=Bio(i,k,iTIC_)/(akco2s1(ng)+Bio(i,k,iTIC_))
#else
	uco2s1=1.0_r8
#endif

! light adaption if you dont have chloropyll variable
!!    alts1= 1.0_r8 - exp(-PAR(i,k)*(ADPT(i,k)/40.0_r8)/gmaxs1(ng))
!

! if you have chloropyll variable

!    alts1=1.0_r8 - exp(-PAR(i,k)*amaxs1(ng)/gmaxs1(ng))   

!    alts1=amaxs1(ng)*PAR(i,k)/sqrt(gmaxs1(ng)*gmaxs1(ng)+   &
!   &             amaxs1(ng)*PAR(i,k)*amaxs1(ng)*PAR(i,k))

     alts1= 1.0_r8 - exp(-PAR(i,k)*ADPT(i,k)/parsats1(ng))  ! Feng Zhou
!Feng Zhou: reduce the light availability at extreme shallow regions
!           where water depth is shallower than 20 m.
!           1/50 for top 2 layers (about 1.3 meter)
!           zero for all other lower layers
!     alts1= alts1*cffzhou(i)
      alts1= alts1*cffik(i,k)   
!    if (cff5 .gt.100) then
!       stop
!    else
!       if (abs(z_r(i,j,1)) .lt.25) then
!           cff5 = cff5 + 1
!           print *, 'alts1 shallow(z_w) : ', z_w(i,j,1),cffzhou(i),alts1
!       endif
!    endif      
! growth
    
    grows1=min(uno3s1+unh4s1,upo4s1)*alts1

!! adjustments
!
    cents1=grows1/(uno3s1+unh4s1+Minval)
    uno3s1=cents1*uno3s1
    unh4s1=cents1*unh4s1
    upo4s1=grows1
    uco2s1=grows1
    
!change end--------------------------    
            
!
!-----------------------------------------------------------------------
!  diatom nutrient uptake, and growth
!-----------------------------------------------------------------------
!          

!!    pnh4s2= exp(-pis2(ng)*Bio(i,k,iNH4_))
!!    cens2=1.0_r8+Bio(i,k,iNH4_)/aknh4s2(ng)+                           &
!!    &     pnh4s2*Bio(i,k,iNO3_)/akno3s2(ng)
!!    uno3s2 = pnh4s2*Bio(i,k,iNO3_)/(akno3s2(ng)*cens2)
!!    unh4s2 = Bio(i,k,iNH4_)/(aknh4s2(ng)*cens2)
!!    
!!!   uno3s2 = pnh4s2*no3(k)/(akno3s2+no3(k))
!!!   unh4s2 = nh4(k)/(aknh4s2+nh4(k))
!!!
!!
!!    usio4s2 = Bio(i,k,iSiOH)/(aksio4s2(ng)+Bio(i,k,iSiOH))
!!
!!    upo4s2 = Bio(i,k,iPO4_)/(akpo4s2(ng)+Bio(i,k,iPO4_))
!!
!!#ifdef CARBON
!!    uco2s2 = Bio(i,k,iTIC_)/(akco2s2(ng)+Bio(i,k,iTIC_))
!!#else
!!	uco2s2=1.0_r8
!!#endif
!!
!!    alts2= 1.0_r8 - exp(-PAR(i,k)*ADPT(i,k)/parsats2(ng))
!!
!!    grows2=min(uno3s2+unh4s2,usio4s2,upo4s2,uco2s2)*alts2
!!
!!    cents2=grows2/(uno3s2+unh4s2+Minval)
!!    uno3s2=cents2*uno3s2
!!    unh4s2=cents2*unh4s2
!!    usio4s2=grows2
!!    upo4s2=grows2
!!    uco2s2=grows2
    
!!change here-------

!    pnh4s2= exp(-pis2(ng)*Bio(i,k,iNH4_))
    
!!another option
!!    pnh4s2=	1.0_r8/(1.0_r8+Bio(i,k,iNH4_)/aknh4s2(ng))
    
!    uno3s2 = pnh4s2*Bio(i,k,iNO3_)/(akno3s2(ng)+Bio(i,k,iNO3_))
!    unh4s2 = Bio(i,k,iNH4_)/(aknh4s2(ng)+Bio(i,k,iNH4_))

!    usio4s2 = Bio(i,k,iSiOH)/(aksio4s2(ng)+Bio(i,k,iSiOH))

!    upo4s2 = Bio(i,k,iPO4_)/(akpo4s2(ng)+Bio(i,k,iPO4_))

!#ifdef CARBON
!    uco2s2 = Bio(i,k,iTIC_)/(akco2s2(ng)+Bio(i,k,iTIC_))
!#else
!	uco2s2=1.0_r8
!#endif

!! light adaption if you dont have chloropyll variable

!!    alts2= 1.0_r8 - exp(-PAR(i,k)*(ADPT(i,k)/40.0_r8)/gmaxs2(ng))

!!
!!if you have chloropyll variable
!!
!    alts2=1.0_r8 - exp(-PAR(i,k)*amaxs2(ng)/gmaxs2(ng))
    
!!    alts2=amaxs2(ng)*PAR(i,k)/sqrt(gmaxs2(ng)*gmaxs2(ng)+   &
!!   &             amaxs2(ng)*PAR(i,k)*amaxs2(ng)*PAR(i,k))



!! careful with this limitation term for diatoms

!    grows2=min(uno3s2+unh4s2,usio4s2,upo4s2)*alts2
!!    grows2=usio4s2*alts2

!    cents2=grows2/(uno3s2+unh4s2+Minval)
!    uno3s2=cents2*uno3s2
!    unh4s2=cents2*unh4s2
    
        
!    usio4s2=grows2
!    upo4s2=grows2
!    uco2s2=grows2

!!end change----------------        
    
! whole block is replaced by Feng Zhou     
    pnh4s2= exp(-pis2(ng)*Bio(i,k,iNH4_))
    cens2=1.0_r8+Bio(i,k,iNH4_)/aknh4s2(ng)+                           &
    &     pnh4s2*Bio(i,k,iNO3_)/akno3s2(ng)
    uno3s2 = pnh4s2*Bio(i,k,iNO3_)/(akno3s2(ng)*cens2)
    unh4s2 = Bio(i,k,iNH4_)/(aknh4s2(ng)*cens2)
    
!   uno3s2 = pnh4s2*no3(k)/(akno3s2+no3(k))
!   unh4s2 = nh4(k)/(aknh4s2+nh4(k))
!

    usio4s2 = Bio(i,k,iSiOH)/(aksio4s2(ng)+Bio(i,k,iSiOH))

    upo4s2 = Bio(i,k,iPO4_)/(akpo4s2(ng)+Bio(i,k,iPO4_))

#ifdef CARBON
    uco2s2 = Bio(i,k,iTIC_)/(akco2s2(ng)+Bio(i,k,iTIC_))
#else
	uco2s2=1.0_r8
#endif

     alts2= 1.0_r8 - exp(-PAR(i,k)*ADPT(i,k)/parsats2(ng))

!Feng Zhou: reduce the light availability at extreme shallow regions
!           where water depth is shallower than 20 m.
!           1/10 for top 2 layers (about 1.3 meter)
!           zero for all other lower layers
!     alts2=alts2*cffzhou(i)
      alts2=alts2*cffik(i,k)


    grows2=min(uno3s2+unh4s2,usio4s2,upo4s2,uco2s2)*alts2

    cents2=grows2/(uno3s2+unh4s2+Minval)
    uno3s2=cents2*uno3s2
    unh4s2=cents2*unh4s2
    usio4s2=grows2
    upo4s2=grows2
    uco2s2=grows2
! end replace     
! uptake     
    gno3s1  = gmaxs1(ng)*uno3s1   
    gnh4s1  = gmaxs1(ng)*unh4s1
    gno3s2  = gmaxs2(ng)*uno3s2
    gnh4s2  = gmaxs2(ng)*unh4s2
    gsio4s2 = gmaxs2(ng)*usio4s2
                        
            
!     -------------------------------------------------------
!     CALCULATING THE NEW,REGENERATED,AND PRIMARY PRODUCTION 
!     -------------------------------------------------------
      nps1 =  gno3s1 * Bio(i,k,iSphy)
      rps1 =  gnh4s1 * Bio(i,k,iSphy)
      nps2 =  gno3s2 * Bio(i,k,iLphy) 
      rps2 =  gnh4s2 * Bio(i,k,iLphy)


!      
!     *** Chla for s1: Xiu and Geng ***  npchl1; npchl2; Chl2cs1_m(ng); Chl2cs2_m(ng)
!

!      cff=c2n(ng)*12.0_r8                                          ! Meng 20200612
!      Chl2C_s1=MIN(Bio(i,k,iChl1)/(Bio(i,k,iSphy)*cff+Minval),  &  ! Meng 20200612
!     &                    Chl2cs1_m(ng))                           ! Meng 20200612
     
!      Chl2C_s2=MIN(Bio(i,k,iChl2)/(Bio(i,k,iLphy)*cff+Minval),  &  ! Meng 20200612
!     &                    Chl2cs2_m(ng))                           ! Meng 20200612
     
!      npchl1=(gno3s1+gnh4s1)*(gno3s1+gnh4s1)*Chl2cs1_m(ng)      &  ! Meng 20200612
!      &   *Bio(i,k,iChl1)/                             &           ! Meng 20200612
!      &   (amaxs1(ng)*MAX(Chl2C_s1,Minval)*PAR(i,k)+Minval)        ! Meng 20200612 

!      npchl2=(gno3s2+gnh4s2)*(gno3s2+gnh4s2)*Chl2cs2_m(ng)     &   ! Meng 20200612
!      &   *Bio(i,k,iChl2)/                             &           ! Meng 20200612
!      &   (amaxs2(ng)*MAX(Chl2C_s2,Minval)*PAR(i,k)+Minval)        ! Meng 20200612
      

      
!     -------------------------------------------------------
!     CALAULATING THE mortality and excretion of zoo
!     -------------------------------------------------------
      morts1=bgamma3(ng)*Bio(i,k,iSphy)
      morts2=bgamma4(ng)*Bio(i,k,iLphy)
      excrz1=reg1(ng)*Bio(i,k,iSzoo)
      excrz2=reg2(ng)*Bio(i,k,iLzoo)     
 
!assimilate related excretion from Fennel
      
      excrz1_2=bgamma1(ng)*reg1(ng)*Bio(i,k,iSphy)*          &
     & Bio(i,k,iSphy)*Bio(i,k,iSzoo)/                        &
     &     (akz1(ng)+Bio(i,k,iSphy)*Bio(i,k,iSphy))
      
      excrz2_2=bgamma2(ng)*reg2(ng)*( Bio(i,k,iLphy)*         &
     &  Bio(i,k,iLphy)*Bio(i,k,iLzoo)/          &
     &     (akz2(ng)+Bio(i,k,iLphy)*Bio(i,k,iLphy)) +         &
     &     Bio(i,k,iSzoo)*Bio(i,k,iSzoo)*Bio(i,k,iLzoo)/      &
     &     (akz2(ng)+Bio(i,k,iSzoo)*Bio(i,k,iSzoo)) )
     
     
       
      remvz2 = bgamma0(ng)*Bio(i,k,iLzoo)*Bio(i,k,iLzoo)
      
!     *** Chla for s1 and s2: Xiu and Geng
!      

!       morchl1=bgamma3(ng)*Bio(i,k,iChl1)  ! Meng 20200612

!       morchl2=bgamma4(ng)*Bio(i,k,iChl2)  ! Meng 20200612
      
! aggregates

      aggregs1=bgamma6(ng)*(Bio(i,k,iSphy)+Bio(i,k,iLphy))*Bio(i,k,iSphy)  
      aggregs2=bgamma6(ng)*(Bio(i,k,iSphy)+Bio(i,k,iLphy))*Bio(i,k,iLphy) 
!      aggregchl1=bgamma6(ng)*(Bio(i,k,iSphy)+Bio(i,k,iLphy))*Bio(i,k,iChl1)  ! Meng 20200612
!      aggregchl2=bgamma6(ng)*(Bio(i,k,iSphy)+Bio(i,k,iLphy))*Bio(i,k,iChl2)  ! Meng 20200612   
            
!      -------------------------------------------------------
!     CALAULATING THE nitrification and reminalization
!     -------------------------------------------------------
!!       nitrif = bgamma7(ng)*Bio(i,k,iNH4_)

!!another option
!        II00=0.0095_r8
!        nitrif = bgamma7(ng)*(1.0_r8-max(0.0_r8,(PAR(i,k)-II00)/   &
!       &       (0.1_r8+PAR(i,k)-II00)))*Bio(i,k,iNH4_)

!!important
!!      if (hh(i,j) .le. 10.0) then
!!        cent1=0.5_r8
!!       else
!
!      if(k .gt. 1)then
!!        cent1=max(0.21_r8*Bio(i,k,itemp)/25.0_r8                       &
!!      &           +0.01_r8, 0.01_r8)
!        cent1=bgamma5(ng)
!      else
!! two options here, may change bottom oxygen comsumption
!!
!        cent1=1.0_r8*(1.0_r8/dtdays) 
!!        cent1=Wbio(2)*Bio(i,k+1,iSDet)/(HZ(i,j,k)*Bio(i,k,iSDet)) 
!
!      endif
!      
!!      endif
!      
!      midd = cent1*Bio(i,k,iSDet)
!      
!!      MIDDSI(k) = bgamma5*ddsi(k)

!!      if (hh(i,j) .le. 10.0) then
!!        cent1=0.5_r8
!!       else

!      if(k .gt. 1)then
!!       cent1=max(0.19_r8*Bio(i,k,itemp)/25.0_r8+0.01_r8,               &
!!     &        0.01_r8)
!        cent1=bgamma5(ng)*0.5_r8
!! the unit is day^{-1}
!      else

!        cent1=1.0_r8*(1.0_r8/dtdays) 
!!        cent1=Wbio(3)*Bio(i,k+1,iopal)/(HZ(i,j,k)*Bio(i,k,iopal)) 
!      endif
!      
!!      endif
!      
!      middsi = cent1*Bio(i,k,iopal)
! whole block is repleced by Feng Zhou
       nitrif = bgamma7(ng)*Bio(i,k,iNH4_)

      if(k .gt. 1)then
!        cent1=max(0.15_r8*Bio(i,k,itemp)/25.0_r8                       &
!      &           +0.05_r8, 0.05_r8)
        cent1=max(0.75_r8*bgamma5(ng)*Bio(i,k,itemp)/25.0_r8                       &
      &           +0.05_r8, 0.05_r8)               ! Meng 20200719
      else
!        cent1=1.5_r8*bgamma5(ng)
        cent1=bgamma5(ng)            ! Meng 20200719
      endif
      
      midd = cent1*Bio(i,k,iSDet)
      
!        the unit is day^{-1}
!      MIDDSI(k) = bgamma5*ddsi(k)
      if(k .gt. 1)then
!       the minimum cent1 is related to temperature. Temerature of the Kuroshio
!       could be lower than 8 (T<8), I increase the minimum value of dissolution,
!       which only affects the deep wwater.  Feng Zhou
        cent1=max(0.19_r8*Bio(i,k,itemp)/25.0_r8+0.01_r8, 0.08_r8)
!       cent1=0.19*temp(k)/25.0+0.01
!!!FengZhou
!         if (k .ge. N(ng)-1) then
!            cent1= (float(k)/float(N(ng)))*10.0_r8*bgamma5(ng)
!            print *, 'cent1 : ', cent1
!         endif
!!!endofFengZhou
      else
        !!cent1=4.5_r8*bgamma5(ng)
        cent1= 8.0_r8*bgamma5(ng)   ! Feng Zhou
      endif
      
      middsi = cent1*Bio(i,k,iopal)
           
! end replace           
!     -------------------------------------------------------
!     CALAULATING THE GRAZING RATE 
!     -------------------------------------------------------

      gs1zz1 = beta1(ng)*Bio(i,k,iSzoo)*Bio(i,k,iSphy)                 &
              &    /(akz1(ng)+Bio(i,k,iSphy))

!another option s curve
!      gs1zz1 = beta1(ng)*Bio(i,k,iSzoo)*Bio(i,k,iSphy)*Bio(i,k,iSphy)   &
!              &    /(akz1(ng)+Bio(i,k,iSphy)*Bio(i,k,iSphy))


!
!     *** Chla for s1: Xiu and Geng
!              
!      gchl1zz1 = gs1zz1*Bio(i,k,iChl1)/Bio(i,k,iSphy)        ! Meng 20200612        



      ro8=ro5(ng)*Bio(i,k,iLphy)+ro6(ng)*Bio(i,k,iSzoo)                &
              & +ro7(ng)*Bio(i,k,iSDet)
      ro9=ro5(ng)*Bio(i,k,iLphy)*Bio(i,k,iLphy)+ro6(ng)                &
            & *Bio(i,k,iSzoo)*Bio(i,k,iSzoo)                           &
            & +ro7(ng)*Bio(i,k,iSDet)*Bio(i,k,iSDet)

      if((ro8 .le. 0.0_r8) .and. (ro9 .le. 0.0_r8))then
           gs2zz2  = 0.0_r8
           gddzz2  = 0.0_r8
           gzz1zz2 = 0.0_r8
!           gchl2zz2 = 0.0_r8    ! Xiu and Geng  ! Meng 20200612
      else
      gs2zz2= beta2(ng)*ro5(ng)*Bio(i,k,iLphy)*Bio(i,k,iLphy)          &
           & *Bio(i,k,iLzoo)/(akz2(ng)*ro8+ro9)
      gzz1zz2=beta2(ng)*ro6(ng)*Bio(i,k,iSzoo)*Bio(i,k,iSzoo)          &
            & *Bio(i,k,iLzoo)/(akz2(ng)*ro8+ro9)
      gddzz2=beta2(ng)*ro7(ng)*Bio(i,k,iSDet)*Bio(i,k,iSDet)           &
            & *Bio(i,k,iLzoo)/(akz2(ng)*ro8+ro9)
            
            !
!     *** Chla for s2: Xiu and Geng
!     
!      gchl2zz2 = gs2zz2*Bio(i,k,iChl2)/Bio(i,k,iLphy)   ! Meng 20200612
           
           
      endif

! critical values to stop grazing Feng Zhou

    if(Bio(i,k,iSphy) .le. 0.005_r8)then
        gs1zz1=0.0_r8
!        gchl1zz1=0.0_r8    ! Xiu and Geng ! Meng 20200612
    endif
    if(Bio(i,k,iLphy) .le. 0.005_r8)then
        gs2zz2=0.0_r8
        gddzz2= 0.0_r8     ! Feng Zhou
        gzz1zz2= 0.0_r8    ! Feng Zhou    
!        gchl2zz2=0.0_r8    ! Xiu and Geng ! Meng 20200612
    endif
          
! total grazing for large zoo

   gtzz2 =  gddzz2 + gzz1zz2 + gs2zz2
      
      
!-------------------------------------------------------------------------
!     CALCULATING THE TOTAL RATE  
!--------------------------------------------------------------------------

!sediment o2 consumption parameterization, careful

!     SOC=0.614_r8*(2.0_r8**(Bio(i,k,itemp)/5.474_r8))/HZ(i,j,1)
     
!     SOC=6.0_r8*(2.0_r8**(Bio(i,k,itemp)/10.0_r8))*                 &   !Hetland 2008
!    &    (1.0_r8-exp(-Bio(i,k,iOxyg)/30.0_r8))/HZ(i,j,1)
 
       SOC=0.0_r8 
        
!      if(k .gt. 1)then  
!        Qsms1 = - nps1 - nps2 + OXR*nitrif
!        Qsms3 = - rps1 - rps2 + OXR*excrz1 + OXR*excrz2             &
!     &          - OXR*nitrif+OXR*midd+OXR*(excrz1_2+excrz2_2)
!      else
!         Qsms1 = - nps1 - nps2 + OXR*nitrif
!         Qsms3 = - rps1 - rps2 + OXR*excrz1 + OXR*excrz2            &
!     & -           OXR*nitrif+OXR*midd+OXR*(excrz1_2+excrz2_2)      &
!     & + 0.036_r8*SOC
!
!      endif
     
!        Qsms6 = + bgamma1(ng)*gs1zz1 - OXR*excrz1 - gzz1zz2 -OXR*excrz1_2
!        Qsms7 = + bgamma2(ng)*gtzz2 - OXR*excrz2 - remvz2   -OXR*excrz2_2
!        Qsms8 = + (1-bgamma1(ng))*gs1zz1 + (1-bgamma2(ng))*gtzz2       &
!     &             - gddzz2+ morts1 + morts2- OXR*midd+ remvz2   &
!     &             + aggregs1 + aggregs2
     
!        Qsms2 = - (nps2 + rps2)*si2n(ng) + OXR*middsi
!!        if(k .eq. 1) then
!!          Qsms2= Qsms2+si2n(ng)*0.036_r8*SOC 
!!        endif
      
!        Qsms9 = (gs2zz2 + morts2+aggregs2)*si2n(ng) - OXR*middsi
!        Qsms10= - (nps1+rps1+nps2+rps2)*p2n(ng)                        &
!     &          +OXR*(excrz1+excrz2)*p2n(ng)+ OXR*midd*p2n(ng)
!!        if(k .eq. 1) then
!!          Qsms10= Qsms10+p2n(ng)*0.036_r8*SOC 
!!        endif
!   Feng Zhou
        Qsms1 = - nps1 - nps2 + OXR*nitrif
        Qsms3 = - rps1 - rps2 + OXR*excrz1 + OXR*excrz2                &
     &          - OXR*nitrif+OXR*midd     
        Qsms4 = + nps1 + rps1 - gs1zz1 - morts1 
        Qsms5 = + nps2 + rps2 - gs2zz2 - morts2
        Qsms6 = + bgamma1(ng)*gs1zz1 - OXR*excrz1 - gzz1zz2
        Qsms7 = + bgamma2(ng)*gtzz2 - OXR*excrz2 - remvz2
        Qsms8 = + (1-bgamma1(ng))*gs1zz1 + (1-bgamma2(ng))*gtzz2       &
     &             - gddzz2+ morts1 + morts2- OXR*midd+ remvz2
        Qsms2 = - (nps2 + rps2)*si2n(ng) + middsi
        Qsms9 = ( gs2zz2 + morts2)*si2n(ng) - middsi
        Qsms10= - (nps1+rps1+nps2+rps2)*p2n(ng)                        &
     &          +OXR*(excrz1+excrz2)*p2n(ng)+ OXR*midd*p2n(ng)     
! end Feng Zhou          
#ifdef OXYGEN

!        Qsms11= (nps1+nps2)*o2no(ng)+(rps1+rps2)*o2nh(ng)              &
!     &         -2.0_r8*OXR*nitrif - OXR*(excrz1 + excrz2)*o2nh(ng)     &
!     &         - OXR*midd*o2nh(ng)
     
!      if(k .eq. 1) then
!        Qsms11= Qsms11-SOC 
!      endif
!Feng Zhou Meng 20200719
      if(k .gt. 1)then
        Qsms11= (nps1+nps2)*o2no(ng)+(rps1+rps2)*o2nh(ng)              &
     &         -2.0_r8*OXR*nitrif - OXR*(excrz1 + excrz2)*o2nh(ng)     &
     &         - OXR*midd*o2nh(ng)
      else
        Qsms11= (nps1+nps2)*o2no(ng) + (rps1+rps2)*o2nh(ng) 
      endif
! end Feng Zhou      
      
#endif

#ifdef CARBON
        Qsms12= - (nps1+rps1+nps2+rps2)*c2n(ng)+OXR*(excrz1+excrz2)    &
     &          *c2n(ng)+ OXR*midd*c2n(ng)
        Qsms13= - Qsms1 + Qsms3
! Feng Zhou        
!        if(k .eq. 1) then
!           Qsms12=Qsms12+0.9475_r8*SOC
!        endif
! end Feng Zhou        
        
#endif


!        Qsms4 = + nps1 + rps1 - gs1zz1 - morts1 - aggregs1 
!        Qsms5 = + nps2 + rps2 - gs2zz2 - morts2 - aggregs2

!        if(Bio(i,k,iChl1) .ge. 0.01_r8) then                        ! Meng 20200612
!          Qsms14 = + npchl1 - morchl1 - gchl1zz1 - aggregchl1       ! Meng 20200612
!          Qsms4 = + nps1 + rps1 - gs1zz1 - morts1 - aggregs1        ! Meng 20200612
!        else                                                        ! Meng 20200612
!          Qsms14 = npchl1                                           ! Meng 20200612
!          Qsms4 = + nps1 + rps1                                     ! Meng 20200612 
!        endif
        
!        if(Bio(i,k,iChl2) .ge. 0.01_r8) then                        ! Meng 20200612
!          Qsms15 = + npchl2 - morchl2 - gchl2zz2 - aggregchl2       ! Meng 20200612
!          Qsms5 = + nps2 + rps2 - gs2zz2 - morts2 - aggregs2        ! Meng 20200612
!        else                                                        ! Meng 20200612
!          Qsms15 = npchl2- morchl2                                  ! Meng 20200612
!          Qsms5 = + nps2 + rps2- morts2                             ! Meng 20200612
!        endif                                                       ! Meng 20200612

        NQsms1 = 0.0_r8
        NQsms3 = 0.0_r8
        NQsms4 = 0.0_r8
        NQsms5 = 0.0_r8
        NQsms6 = 0.0_r8
        NQsms7 = 0.0_r8 
        NQsms8 = 0.0_r8
        NQsms2 = 0.0_r8
        NQsms9 = 0.0_r8
        NQsms10= 0.0_r8
#ifdef OXYGEN
        NQsms11= 0.0_r8
#endif

#ifdef CARBON
        NQsms12= 0.0_r8
        NQsms13= - NQsms1 + NQsms3
#endif
        
!
!      Chla : Xiu and Geng
!        
!        NQsms14 = 0.0_r8  ! Meng 20200612
!        NQsms15 = 0.0_r8  ! Meng 20200612
        

        sms1 = Q10*Qsms1 + NQsms1
        sms2 = Q10*Qsms2 + NQsms2
        sms3 = Q10*Qsms3 + NQsms3
        sms4 = Q10*Qsms4 + NQsms4
        sms5 = Q10*Qsms5 + NQsms5
        sms6 = Q10*Qsms6 + NQsms6
        sms7 = Q10*Qsms7 + NQsms7
        sms8 = Q10*Qsms8 + NQsms8
        sms9 = Q10*Qsms9 + NQsms9
        sms10= Q10*Qsms10 + NQsms10
#ifdef OXYGEN
        sms11= Q10*Qsms11 + NQsms11
#endif

#ifdef CARBON
        sms12= Q10*Qsms12 + NQsms12
        sms13= Q10*Qsms13 + NQsms13     
#endif


!
!      Chla : Xiu and Geng
!
!       sms14 = Q10*Qsms14 + NQsms14  ! Meng 20200612
!       sms15 = Q10*Qsms15 + NQsms15  ! Meng 20200612
       
       

#ifdef DIAGNOSTICS_BIO
        DiaBio3d(i,j,k,iPPro1)=DiaBio3d(i,j,k,iPPro1)+                 &
# ifdef WET_DRY
        &            rmask_io(i,j)*                                    &
# endif        
        &         (nps1 + rps1)*dtdays
        
        DiaBio3d(i,j,k,iPPro2)=DiaBio3d(i,j,k,iPPro2)+                 &
# ifdef WET_DRY
        &            rmask_io(i,j)*                                    &
# endif        
        &            (nps2 + rps2)*dtdays
        
        
        DiaBio3d(i,j,k,iNO3u)=DiaBio3d(i,j,k,iNO3u)+                   &
# ifdef WET_DRY
        &              rmask_io(i,j)*                                  &
# endif        
        &              (nps1+nps2)*dtdays
# endif
        
! update values 
      
    Bio(i,k,iNO3_)=Bio(i,k,iNO3_)+dtdays*sms1
	Bio(i,k,iSiOH)=Bio(i,k,iSiOH)+dtdays*sms2
	Bio(i,k,iNH4_)=Bio(i,k,iNH4_)+dtdays*sms3
	Bio(i,k,iSphy)=Bio(i,k,iSphy)+dtdays*sms4
	Bio(i,k,iLphy)=Bio(i,k,iLphy)+dtdays*sms5
	Bio(i,k,iSzoo)=Bio(i,k,iSzoo)+dtdays*sms6
	Bio(i,k,iLzoo)=Bio(i,k,iLzoo)+dtdays*sms7
	Bio(i,k,iSDet)=Bio(i,k,iSDet)+dtdays*sms8
	Bio(i,k,iopal)=Bio(i,k,iopal)+dtdays*sms9
	Bio(i,k,iPO4_)=Bio(i,k,iPO4_)+dtdays*sms10
	
#ifdef OXYGEN
	Bio(i,k,iOxyg)=Bio(i,k,iOxyg)+dtdays*sms11
#endif
#ifdef CARBON
	Bio(i,k,iTIC_)=Bio(i,k,iTIC_)+dtdays*sms12
#ifdef TALK_NONCONSERV
	Bio(i,k,iTAlk)=Bio(i,k,iTAlk)+dtdays*sms13
#endif	
#endif


!
!      Chla : Xiu and Geng
!Chl2cs1_m(ng)  Chl2cs2_m(ng)
!    Bio(i,k,iChl1)=Bio(i,k,iChl1)+dtdays*sms14  ! Meng 20200612
!    Bio(i,k,iChl2)=Bio(i,k,iChl2)+dtdays*sms15  ! Meng 20200612
    Bio(i,k,iChl1)=Bio(i,k,iSphy)*Chl2cs1_m(ng)  ! Meng 20200612
    Bio(i,k,iChl2)=Bio(i,k,iLphy)*Chl2cs2_m(ng)  ! Meng 20200612    
 
        END DO  !k loop
    END DO  !i loop
      
      
      
!other flux

#if defined OXYGEN || defined CARBON
!
!-----------------------------------------------------------------------
!     CALCULATING gas transfer velocity at a Schmidt number 
!-----------------------------------------------------------------------
!
    k=N(ng)
    DO i=Istr,Iend
!
!  Compute wind speed.
!
# ifdef BULK_FLUXES
    u10squ=Uwind(i,j)*Uwind(i,j)+Vwind(i,j)*Vwind(i,j)
# else
!
!  drag coefficient is 0.001, and air density is 1.2 kg per cube meter
!
        cff1=rho0/(0.001_r8*1.2_r8)
!       cff1=rho0*550.0_r8

!convert wind stress to wind speed square

      u10squ=cff1*SQRT((0.5_r8*(sustr(i,j)+sustr(i+1,j)))**2+          &
     &                 (0.5_r8*(svstr(i,j)+svstr(i,j+1)))**2)
# endif
      u10spd=sqrt(u10squ)
!
! Compute gas transfer velocity at a Schmidt number of 660.
! climatology wind speed (Wanninkhof & Mcgillis, 1999).
!      kw660=1.09*u10spd-0.333*u10squ+0.078*u10spd*u10squ      
! (in units of cm/hr), the one is too pronounced with large speed
!  short-term (<1 day) winds (Wanninkhof & Mcgillis, 1999).
!      kw660=0.0283*u10spd*u10squ                     
!( in units of cm/hr)
!
          kw660(i)=0.31*u10squ                     

     END DO
#endif

#ifdef OXYGEN
!
!-----------------------------------------------------------------------
!     CALCULATING THE O2 SURFACE saturation concentration and FLUX  
!-----------------------------------------------------------------------
!
    k=N(ng)
    CALL O2_flux (Istr, Iend, LBi, UBi, LBj, UBj,                      &
     &                     IminS, ImaxS, j,                            &
# ifdef MASKING
     &                     rmask,                                      &
# endif
     &                     Bio(IminS:,k,itemp), Bio(IminS:,k,isalt),   &
     &                     Bio(IminS:,k,iOxyg), kw660,                 &
     &                     1.0_r8, o2sat, o2flx)     
    DO i=Istr,Iend
	  Bio(i,k,iOxyg)=bio(i,k,iOxyg)+dtdays*o2flx(i)*Hz_inv(i,k)
    
# ifdef DIAGNOSTICS_BIO
            DiaBio2d(i,j,iO2fx)=DiaBio2d(i,j,iO2fx)+                   &
#  ifdef WET_DRY
     &                          rmask_io(i,j)*                         &
#  endif
     &                          o2flx(i)*dtdays
# endif

      END DO
    
#endif

#ifdef CARBON
!
!-----------------------------------------------------------------------
!  CALCULATING
!  Surface equilibrium partial pressure inorganic carbon (ppmv) at the
!  surface, and CO2 gas exchange.
!-----------------------------------------------------------------------
!
     k=N(ng)
     Bio(IminS:,k,isalt)=MAX(Bio(IminS:,k,isalt),20.0_r8)  ! Meng 20200602 lower limit of salinity
     Bio(IminS:,k,isalt)=MIN(Bio(IminS:,k,isalt),40.0_r8)  ! Meng 20200602 upper limit of salinity
     Bio(IminS:,k,itemp)=MAX(Bio(IminS:,k,itemp),2.0_r8)  ! Meng 20200602 lower limit of  temprature
     Bio(IminS:,k,itemp)=MIN(Bio(IminS:,k,itemp),35.0_r8)  ! Meng 20200602 upper limit of temprature
     Bio(IminS:,k,iPO4_)=MAX(Bio(IminS:,k,iPO4_),0.001_r8)  ! Meng 20200511 remove negative PO4  
     Bio(IminS:,k,iSiOH)=MAX(Bio(IminS:,k,iSiOH),0.001_r8)  ! Meng 20200511 remove negative SiOH4        
      CALL CO2_flux (Istr, Iend, LBi, UBi, LBj, UBj,                   &
     &                     IminS, ImaxS, j,                            &
#ifdef MASKING
     &                     rmask,                                      &
#endif
     &                     Bio(IminS:,k,itemp), Bio(IminS:,k,isalt),   &
     &                     Bio(IminS:,k,iTIC_), Bio(IminS:,k,iTAlk),   &
     &                     Bio(IminS:,k,iPO4_), Bio(IminS:,k,iSiOH),   &
     &                     kw660, 1.0_r8,pco2a(ng), co2flx,pco2s)
    DO i=Istr,Iend
	  Bio(i,k,iTIC_)=Bio(i,k,iTIC_)+dtdays*co2flx(i)*Hz_inv(i,k)
	  
# ifdef DIAGNOSTICS_BIO
            DiaBio2d(i,j,iCOfx)=DiaBio2d(i,j,iCOfx)+                   &
#  ifdef WET_DRY
     &                          rmask_io(i,j)*                         &
#  endif
     &                          co2flx(i)*dtdays
     
            DiaBio2d(i,j,ipCO2)=pco2s(i)
#  ifdef WET_DRY
            DiaBio2d(i,j,ipCO2)=DiaBio2d(i,j,ipCO2)*rmask_io(i,j)
#  endif
# endif
	  
    END DO

!#ifdef TALK_NONCONSERV       	       
!     adjust the alkalinity
!     DO i=Istr,Iend
!         DO k=1,N(ng)
!       	cff0=Bio_bak(i,k,iNO3_)-Bio(i,k,iNO3_)-                    &
!     &       (Bio_bak(i,k,iNH4_)-Bio(i,k,iNH4_))
!	        Bio(i,k,iTAlk)=Bio(i,k,iTAlk)+cff0
!         END DO
!      END DO
!#endif

#endif


! 
!-----------------------------------------------------------------------
!     CALCULATING THE SINKING FLUX
!-----------------------------------------------------------------------
!

#ifdef SINK_OP1

  SINK_LOOP: DO isink=1,Nsink    
    indx=idsink(isink)
    
!FengZhou copy form SINK_OP2
    DO k=1,N(ng)
       DO i=Istr,Iend
          qc(i,k)=Bio(i,k,indx)
       END DO
    END DO
!
    DO k=N(ng)-1,1,-1
       DO i=Istr,Iend
          FC(i,k)=(qc(i,k+1)-qc(i,k))*Hz_inv2(i,k)
       END DO
    END DO
!FengZhou
    
    DO i=Istr,Iend
        DO k=1,N(ng)
          thick=HZ(i,j,k)
	      cff0=thick/dtdays
	      !cff1=min(0.9_r8*cff0,wbio(isink))
	      cff1=wbio(isink)
          if(k.eq.N(ng))then
             sinkindx(k) = cff1*Bio(i,k,indx)/thick
          elseif(k.gt.1.and.k.lt.n(ng))then
              sinkindx(k) = cff1*(Bio(i,k,indx)-Bio(i,k+1,indx))/thick
          elseif(k.eq.1)then
              sinkindx(k) = cff1* (-Bio(i,k+1,indx))/thick
          endif
       END DO
       DO k=1,N(ng)
	     Bio(i,k,indx)=Bio(i,k,indx)-dtdays*sinkindx(k)
	     Bio(i,k,indx)=max(Bio(i,k,indx),Minval)
       END DO
    END DO
  END DO SINK_LOOP

#endif

#ifdef SINK_OP2

!  Need to test this one
!
!  Reconstruct vertical profile of selected biological constituents
!  "Bio(:,:,isink)" in terms of a set of parabolic segments within each
!  grid box. Then, compute semi-Lagrangian flux due to sinking.
!
    SINK_LOOP: DO isink=1,Nsink
            indx=idsink(isink)
!
!  Copy concentration of biological particulates into scratch array
!  "qc" (q-central, restrict it to be positive) which is hereafter
!  interpreted as a set of grid-box averaged values for biogeochemical
!  constituent concentration.
!
            DO k=1,N(ng)
              DO i=Istr,Iend
                qc(i,k)=Bio(i,k,indx)
              END DO
            END DO
!
            DO k=N(ng)-1,1,-1
              DO i=Istr,Iend
                FC(i,k)=(qc(i,k+1)-qc(i,k))*Hz_inv2(i,k)
              END DO
            END DO
            DO k=2,N(ng)-1
              DO i=Istr,Iend
                dltR=Hz(i,j,k)*FC(i,k)
                dltL=Hz(i,j,k)*FC(i,k-1)
                cff=Hz(i,j,k-1)+2.0_r8*Hz(i,j,k)+Hz(i,j,k+1)
                cffR=cff*FC(i,k)
                cffL=cff*FC(i,k-1)
!
!  Apply PPM monotonicity constraint to prevent oscillations within the
!  grid box.
!
                IF ((dltR*dltL).le.0.0_r8) THEN
                  dltR=0.0_r8
                  dltL=0.0_r8
                ELSE IF (ABS(dltR).gt.ABS(cffL)) THEN
                  dltR=cffL
                ELSE IF (ABS(dltL).gt.ABS(cffR)) THEN
                  dltL=cffR
                END IF
!
!  Compute right and left side values (bR,bL) of parabolic segments
!  within grid box Hz(k); (WR,WL) are measures of quadratic variations.
!
!  NOTE: Although each parabolic segment is monotonic within its grid
!        box, monotonicity of the whole profile is not guaranteed,
!        because bL(k+1)-bR(k) may still have different sign than
!        qc(i,k+1)-qc(i,k).  This possibility is excluded,
!        after bL and bR are reconciled using WENO procedure.
!
                cff=(dltR-dltL)*Hz_inv3(i,k)
                dltR=dltR-cff*Hz(i,j,k+1)
                dltL=dltL+cff*Hz(i,j,k-1)
                bR(i,k)=qc(i,k)+dltR
                bL(i,k)=qc(i,k)-dltL
                WR(i,k)=(2.0_r8*dltR-dltL)**2
                WL(i,k)=(dltR-2.0_r8*dltL)**2
              END DO
            END DO
            cff=1.0E-14_r8
            DO k=2,N(ng)-2
              DO i=Istr,Iend
                dltL=MAX(cff,WL(i,k  ))
                dltR=MAX(cff,WR(i,k+1))
                bR(i,k)=(dltR*bR(i,k)+dltL*bL(i,k+1))/(dltR+dltL)
                bL(i,k+1)=bR(i,k)
              END DO
            END DO
            DO i=Istr,Iend
              FC(i,N(ng))=0.0_r8            ! NO-flux boundary condition
#if defined LINEAR_CONTINUATION
              bL(i,N(ng))=bR(i,N(ng)-1)
              bR(i,N(ng))=2.0_r8*qc(i,N(ng))-bL(i,N(ng))
#elif defined NEUMANN
              bL(i,N(ng))=bR(i,N(ng)-1)
              bR(i,N(ng))=1.5_r8*qc(i,N(ng))-0.5_r8*bL(i,N(ng))
#else
              bR(i,N(ng))=qc(i,N(ng))       ! default strictly monotonic
              bL(i,N(ng))=qc(i,N(ng))       ! conditions
              bR(i,N(ng)-1)=qc(i,N(ng))
#endif
#if defined LINEAR_CONTINUATION
              bR(i,1)=bL(i,2)
              bL(i,1)=2.0_r8*qc(i,1)-bR(i,1)
#elif defined NEUMANN
              bR(i,1)=bL(i,2)
              bL(i,1)=1.5_r8*qc(i,1)-0.5_r8*bR(i,1)
#else
              bL(i,2)=qc(i,1)               ! bottom grid boxes are
              bR(i,1)=qc(i,1)               ! re-assumed to be
              bL(i,1)=qc(i,1)               ! piecewise constant.
#endif
            END DO
!
!  Apply monotonicity constraint again, since the reconciled interfacial
!  values may cause a non-monotonic behavior of the parabolic segments
!  inside the grid box.
!
            DO k=1,N(ng)
              DO i=Istr,Iend
                dltR=bR(i,k)-qc(i,k)
                dltL=qc(i,k)-bL(i,k)
                cffR=2.0_r8*dltR
                cffL=2.0_r8*dltL
                IF ((dltR*dltL).lt.0.0_r8) THEN
                  dltR=0.0_r8
                  dltL=0.0_r8
                ELSE IF (ABS(dltR).gt.ABS(cffL)) THEN
                  dltR=cffL
                ELSE IF (ABS(dltL).gt.ABS(cffR)) THEN
                  dltL=cffR
                END IF
                bR(i,k)=qc(i,k)+dltR
                bL(i,k)=qc(i,k)-dltL
              END DO
            END DO
!
!  After this moment reconstruction is considered complete. The next
!  stage is to compute vertical advective fluxes, FC. It is expected
!  that sinking may occurs relatively fast, the algorithm is designed
!  to be free of CFL criterion, which is achieved by allowing
!  integration bounds for semi-Lagrangian advective flux to use as
!  many grid boxes in upstream direction as necessary.
!
!  In the two code segments below, WL is the z-coordinate of the
!  departure point for grid box interface z_w with the same indices;
!  FC is the finite volume flux; ksource(:,k) is index of vertical
!  grid box which contains the departure point (restricted by N(ng)).
!  During the search: also add in content of whole grid boxes
!  participating in FC.
!
            cff=dtdays*ABS(Wbio(isink))
            DO k=1,N(ng)
              DO i=Istr,Iend
                FC(i,k-1)=0.0_r8
                WL(i,k)=z_w(i,j,k-1)+cff
                WR(i,k)=Hz(i,j,k)*qc(i,k)
                ksource(i,k)=k
              END DO
            END DO
            DO k=1,N(ng)
              DO ks=k,N(ng)-1
                DO i=Istr,Iend
                  IF (WL(i,k).gt.z_w(i,j,ks)) THEN
                    ksource(i,k)=ks+1
                    FC(i,k-1)=FC(i,k-1)+WR(i,ks)
                  END IF
                END DO
              END DO
            END DO
!
!  Finalize computation of flux: add fractional part.
!
            DO k=1,N(ng)
              DO i=Istr,Iend
                ks=ksource(i,k)
                cu=MIN(1.0_r8,(WL(i,k)-z_w(i,j,ks-1))*Hz_inv(i,ks))
                FC(i,k-1)=FC(i,k-1)+                                   &
     &                    Hz(i,j,ks)*cu*                               &
     &                    (bL(i,ks)+                                   &
     &                     cu*(0.5_r8*(bR(i,ks)-bL(i,ks))-             &
     &                         (1.5_r8-cu)*                            &
     &                         (bR(i,ks)+bL(i,ks)-                     &
     &                          2.0_r8*qc(i,ks))))
              END DO
            END DO
            
            DO k=1,N(ng)
              DO i=Istr,Iend
                Bio(i,k,indx)=qc(i,k)+(FC(i,k)-FC(i,k-1))*Hz_inv(i,k)
              END DO
            END DO

    END DO SINK_LOOP
    
# endif


!
!-----------------------------------------------------------------------
!  Keep tracer variables positive.
!-----------------------------------------------------------------------
!
!  Meng 20200626 start
!        DO ibio=1,NBT
!          indx=idbio(ibio)
!          DO k=1,N(ng)
!            DO i=Istr,Iend
!              Bio(i,k,indx)=MAX(Bio(i,k,indx),0.0_r8)
!            END DO
!          END DO
!       END DO
!         DO k=1,N(ng)
!            DO i=Istr,Iend
!              Bio(i,k,iNO3_)=MIN(Bio(i,k,iNO3_),200.0_r8)
!              Bio(i,k,iSiOH)=MIN(Bio(i,k,iSiOH),200.0_r8)
!              Bio(i,k,iNH4_)=MIN(Bio(i,k,iNH4_),20.0_r8)
!              Bio(i,k,iPO4_)=MIN(Bio(i,k,iPO4_),10.0_r8)
!            END DO
!          END DO        
! Meng 20200626 end

!         DO k=1,N(ng)
!            DO i=Istr,Iend
!              Bio(i,k,iSphy)=MAX(Bio(i,k,iSphy),0.005_r8)
!              Bio(i,k,iLphy)=MAX(Bio(i,k,iLphy),0.005_r8)
!              Bio(i,k,iChl1)=MAX(Bio(i,k,iChl1),0.01_r8)
!              Bio(i,k,iChl2)=MAX(Bio(i,k,iChl2),0.01_r8)
!            END DO
!          END DO
!!!FengZhou
!        DO k=1,N(ng)
!           DO i=Istr,Iend
!              Bio(i,k,iSiOH)=MAX(Bio(i,k,iSiOH),0.1_r8)
!              Bio(i,k,iNO3_)=MIN(Bio(i,k,iNO3_),200.0_r8) ! Meng 20200626
!              Bio(i,k,iSiOH)=MIN(Bio(i,k,iSiOH),200.0_r8) ! Meng 20200626
!              Bio(i,k,iNH4_)=MIN(Bio(i,k,iNH4_),20.0_r8)  ! Meng 20200626
!              Bio(i,k,iPO4_)=MIN(Bio(i,k,iPO4_),10.0_r8)  ! Meng 20200626            
!           END DO
!        END DO
!!!endFengZhou	
      END DO ITER_LOOP
      

!
!-----------------------------------------------------------------------
!  Update global tracer variables.
!-----------------------------------------------------------------------
!
!
!-----------------------------------------------------------------------
!  Update global tracer variables: Add increment due to BGC processes
!  to tracer array in time index "nnew". Index "nnew" is solution after
!  advection and mixing and has transport units (m Tunits) hence the
!  increment is multiplied by Hz.  Notice that we need to subtract
!  original values "Bio_old" at the top of the routine to just account
!  for the concentractions affected by BGC processes. This also takes
!  into account any constraints (non-negative concentrations, carbon
!  concentration range) specified before entering BGC kernel. If "Bio"
!  were unchanged by BGC processes, the increment would be exactly
!  zero. Notice that final tracer values, t(:,:,:,nnew,:) are not
!  bounded >=0 so that we can preserve total inventory of N and
!  C even when advection causes tracer concentration to go negative.
!  (J. Wilkin and H. Arango, Apr 27, 2012)
!-----------------------------------------------------------------------
!
!  Meng 20200626 start
      DO k=1,N(ng)
        DO i=Istr,Iend
#ifdef CARBON         
            Bio(i,k,iTIC_)=MIN(Bio(i,k,iTIC_),3000.0_r8)
            Bio(i,k,iTIC_)=MAX(Bio(i,k,iTIC_),400.0_r8)
            Bio(i,k,iTAlk)=MIN(Bio(i,k,iTAlk),3000.0_r8)
            Bio(i,k,iTAlk)=MAX(Bio(i,k,iTAlk),400.0_r8)            
#endif          
!#ifdef OXYGEN
!            Bio(i,k,iOxyg)=MIN(Bio(i,k,iOxyg),800.0_r8)
!#endif          
          END DO
        END DO
! Meng 20200626 end

        DO ibio=1,NBT
          indx=idbio(ibio)
          DO k=1,N(ng)
            DO i=Istr,Iend
              t(i,j,k,nnew,indx)=max(t(i,j,k,nnew,indx)+                   &
     &            (Bio(i,k,indx)-Bio_bak(i,k,indx))*  Hz(i,j,k),0.00001_r8) 

            END DO
          END DO
        END DO
        
        
      END DO J_LOOP
	  
      RETURN
      
      
    END SUBROUTINE biology_tile
      
     
      
      
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
! Subroutines used in the model
!



#ifdef CARBON
        subroutine   CO2_flux (Istr, Iend, LBi, UBi, LBj, UBj,         &
     &                     IminS, ImaxS, j,                            &
#ifdef MASKING
     &                     rmask,                                      &
#endif
     &                     t, ss,dic, alk,po4,si,kw660, ppo, xco2,     &
     &                     co2ex,pco2s)
!
!**********************************************************************
!
!  Computes the time rate of change of DIC in the surface
!  layer due to air-sea gas exchange in mmol/m^3/day.
!
!  Inputs:
!    t        model surface temperature (deg C)
!    ss        model surface salinity (permil)
!    kw660    gas transfer velocity at a Schmidt number of 660,
!               accounting for sea ice fraction (cm/hr)
!    ppo      surface pressure divided by 1 atm
!    dic      surface DIC concentration (mol/m^3)
!    alk      surface alkalinity (eq/m^3)
!    po4      surface phosphate concentration (mol/m^3)
!    si       surface silicate concentration (mol/m^3)
!    xco2     atmospheric CO2 mixing ratio (ppm)
!  Output:
!    co2ex    time rate of change of DIC in the surface layer due
!               to air-sea exchange (mmol/m^3/day)
!c**********************************************************************
      
      USE mod_kinds
!
      implicit none
!
!  Imported variable declarations.
!
      integer,  intent(in) :: LBi, UBi, LBj, UBj, IminS, ImaxS
      integer,  intent(in) :: Istr, Iend, j
      real(r8),  intent(in) :: ppo,xco2
      integer :: i
#  ifdef ASSUMED_SHAPE
#   ifdef MASKING
      real(r8), intent(in) :: rmask(LBi:,LBj:)
#   endif
      real(r8), intent(in) :: t(IminS:)
      real(r8), intent(in) :: ss(IminS:)
      real(r8), intent(in) :: dic(IminS:)
      real(r8), intent(in) :: alk(IminS:)
      real(r8), intent(in) :: po4(IminS:)
      real(r8), intent(in) :: si(IminS:)
#  else
#   ifdef MASKING
      real(r8), intent(in) :: rmask(LBi:UBi,LBj:UBj)
#   endif
      real(r8), intent(in) :: t(IminS:ImaxS)
      real(r8), intent(in) :: ss(IminS:ImaxS)
      real(r8), intent(in) :: dic(IminS:ImaxS)
      real(r8), intent(in) :: alk(IminS:ImaxS)
      real(r8), intent(in) :: po4(IminS:ImaxS)
      real(r8), intent(in) :: si(IminS:ImaxS)
#  endif

      real(r8), intent(in) :: kw660(IminS:ImaxS)
      real(r8), intent(out) :: co2ex(IminS:ImaxS)
      real(r8), intent(out) :: pco2s(IminS:ImaxS)
      real(r8) :: s(IminS:ImaxS)
      real(r8) :: scco2,kwco2,phlo,phhi
      real(r8) :: co2star,dco2star,pCO2surf,dpco2,ph
      real(r8) :: dic2,alk2,po42,si2
      
       I_LOOP: DO i=Istr,Iend

         dic2=dic(i)/1000.0_r8
         alk2=alk(i)/1000.0_r8
         po42=po4(i)/1000.0_r8
         si2 = si(i)/1000.0_r8
         
!         !not work well for low s
!         if (ss(i) .lt. 5.0_r8) then
!            s(i)=5.0_r8
!         else
            s(i)=ss(i)
!         endif
         
#  ifdef MASKING
        IF (rmask(i,j).gt.0.0_r8) THEN
#  endif
      scco2 = 2073.1_r8 - 125.62_r8*t(i) + 3.6276_r8*t(i)*t(i)         &
     & - 0.043219_r8*t(i)*t(i)*t(i)

      kwco2 = Kw660(i) * (660.0_r8/scco2)**0.5_r8             
!     (in units of cm/hr)
!  Compute the transfer velocity for CO2 in m/day

      kwco2=kwco2*0.01_r8*24.0_r8                        

      phlo = 6.0_r8
      phhi = 9.0_r8

      CALL co2calc(t(i),s(i),dic2,alk2,po42,si2,phlo,phhi,             &
     & 	   xco2,ppo,co2star,dco2star,pCO2surf,dpco2,ph)

      co2ex(i) = kwco2*dco2star

!  Compute time rate of change of CO2 due to gas exchange [1] in 
!  mmol/m^3/day.

      co2ex(i) = 1000.0_r8*co2ex(i)
      pco2s(i) = pCO2surf
      
!    write(*,*) '++++++++++++++++++++++'
!    write(*,*)t(i),s(i),dic(i),alk(i),po4(i),si(i),xco2,pCO2surf,     &
!   & ph,co2ex(i)

#  ifdef MASKING
      ELSE
        co2ex(i)=0.0_r8
        pco2s(i) = 0.0_r8
      END IF
#  endif

      END DO I_LOOP
      RETURN
      END SUBROUTINE CO2_flux
      
!------------------------------------------------   
      
       subroutine co2calc(t,s,dic_in,ta_in,pt_in,sit_in                &
     &                  ,phlo,phhi,xco2_in,atmpres                     &
     &                  ,co2star,dco2star,pCO2surf,dpco2,ph)
      USE mod_kinds
      implicit none
!**********************************************************************
!
! SUBROUTINE CO2CALC
!
! PURPOSE
!      Calculate delta co2* from total alkalinity and total CO2 at
! temperature (t), salinity (s) and "atmpres" atmosphere total pressure. 
!
! USAGE
!       call co2calc(t,s,dic_in,ta_in,pt_in,sit_in
!    &                  ,phlo,phhi,ph,xco2_in,atmpres
!    &                  ,co2star,dco2star,pCO2surf,dpco2)
!
! INPUT
!      dic_in = total inorganic carbon (mol/m^3) 
!                where 1 T = 1 metric ton = 1000 kg
!      ta_in  = total alkalinity (eq/m^3) 
!      pt_in  = inorganic phosphate (mol/m^3) 
!      sit_in = inorganic silicate (mol/m^3) 
!      t      = temperature (degrees C)
!      s      = salinity (PSU)
!      phlo   = lower limit of pH range
!      phhi   = upper limit of pH range
!      xco2_in=atmospheric mole fraction CO2 in dry air (ppmv) 
!      atmpres= atmospheric pressure in atmospheres (1 atm==1013.25mbar)
!
!       Note: arguments dic_in, ta_in, pt_in, sit_in, and xco2_in are 
!             used to initialize variables dic, ta, pt, sit, and xco2.
!             * Variables dic, ta, pt, and sit are in the common block 
!               "species".
!             * Variable xco2 is a local variable.
!             * Variables with "_in" suffix have different units 
!               than those without.
! OUTPUT
!      co2star  = CO2*water (mol/m^3)
!      dco2star = delta CO2 (mol/m^3)
!       pco2surf = oceanic pCO2 (ppmv)
!       dpco2    = Delta pCO2, i.e, pCO2ocn - pCO2atm (ppmv)
!
! IMPORTANT: Some words about units - (JCO, 4/4/1999)
!  - Models carry tracers in mol/m^3 (on a per volume basis)
!  - Conversely, this routine, which was written by observationalists 
!       (C. Sabine and R. Key), passes input arguments in umol/kg  
!       (i.e., on a per mass basis)
!  - I have changed things slightly so that input arguments are in 
!     mol/m^3,
!  - Thus, all input concentrations (dic_in, ta_in, pt_in, and st_in) 
!    should be given in mol/m^3; output arguments "co2star" and 
!    "dco2star" are likewise in mol/m^3.
!**********************************************************************
       real(r8),intent(in)  :: t,s,dic_in,ta_in,pt_in,sit_in
       real(r8),intent(in)  :: phlo,phhi,xco2_in,atmpres
       real(r8),intent(out) :: co2star,dco2star,pCO2surf,dpco2,ph
!
!  Local variable declarations.
!       
       real(r8) :: invtk,is,is2,bt,st,ft,sit,pt,dic,ta
       real(r8) :: k0,k1,k2,kw,kb,ks,kf,k1p,k2p,k3p,ksi,ff,htotal
       real(r8) :: permil,permeg,xco2,tk,tk100,tk1002,dlogtk,sqrtis,s2
       real(r8) :: sqrts,s15,scl,x1,x2,xacc,htotal2,co2starair

!       Change units from the input of mol/m^3 -> mol/kg:
!       (1 mol/m^3)  x (1 m^3/1024.5 kg)
!       where the ocean''s mean surface density is 1024.5 kg/m^3
!       Note: mol/kg are actually what the body of this routine uses 
!       for calculations.  

       permil = 1.0_r8 / 1024.5_r8
       pt=pt_in*permil
       sit=sit_in*permil
       ta=ta_in*permil
       dic=dic_in*permil
       permeg=0.000001_r8
!       To convert input in uatm -> atm
      xco2=xco2_in*permeg
!
! Calculate all constants needed to convert between various measured
! carbon species. References for each equation are noted in the code. 
! Once calculated, the constants are
! stored and passed in the common block "const". The original version 
! of this code was based on the code by Dickson in Version 2 of 
! "Handbook of Methods for the Analysis of the Various Parameters of 
! the Carbon Dioxide System in Seawater", DOE, 1994 (SOP No. 3, p25-26). 
!
! Derive simple terms used more than once
!
      tk = 273.15_r8 + t
      tk100 = tk/100.0_r8
      tk1002=tk100*tk100
      invtk=1.0_r8/tk
      dlogtk=log(tk)
      is=19.924_r8*s/(1000.0_r8-1.005_r8*s)
      is2=is*is
      sqrtis=sqrt(is)
      s2=s*s
      sqrts=sqrt(s)
      s15=s**1.5_r8
      scl=s/1.80655_r8
!
! f = k0(1-pH2O)*correction term for non-ideality
!
! Weiss & Price (1980, Mar. Chem., 8,347-359; Eq 13 with table 6 values)
!
      ff = exp(-162.8301_r8 + 218.2968_r8/tk100  +                     &
     &            90.9241_r8*log(tk100) - 1.47696_r8*tk1002 +          &
     &            s * (0.025695_r8 - 0.025225_r8*tk100 +               &
     &            0.0049867_r8*tk1002))
!
! K0 (Weiss 1974) IS THE CO2 SOLUBILITY IN SEAWATER (IN MMOL M-3 UATM-1)
!
      k0 = exp(93.4517_r8/tk100 - 60.2409_r8 + 23.3585_r8 * log(tk100) &
     & +s*(0.023517_r8 - 0.023656_r8 * tk100 + 0.0047036_r8 * tk1002))

!
! k1 = [H][HCO3]/[H2CO3]
! k2 = [H][CO3]/[HCO3]
!
! Millero p.664 (1995) using Mehrbach et al. data on seawater scale 
!
      k1=10.0_r8**(-1.0_r8*(3670.7_r8*invtk - 62.008_r8 +              &
     & 	      9.7944_r8*dlogtk -0.0118_r8 * s + 0.000116_r8*s2))
!
      k2=10.0_r8**(-1.0_r8*(1394.7_r8*invtk + 4.777_r8 -               &
     &            0.0184_r8*s + 0.000118_r8*s2))
!
! kb = [H][BO2]/[HBO2]
!
! Millero p.669 (1995) using data from Dickson (1990)
!
      kb=exp((-8966.90_r8 - 2890.53_r8*sqrts - 77.942_r8*s +           &
     &            1.728_r8*s15 - 0.0996_r8*s2)*invtk +                 &
     &            (148.0248_r8 + 137.1942_r8*sqrts + 1.62142_r8*s) +   &
     &            (-24.4344_r8 - 25.085_r8*sqrts - 0.2474_r8*s) *      &
     &            dlogtk + 0.053105_r8*sqrts*tk)
!
! k1p = [H][H2PO4]/[H3PO4]
!
! DOE(1994) eq 7.2.20 with footnote using data from Millero (1974)
!
      k1p = exp(-4576.752_r8*invtk + 115.525_r8 - 18.453_r8 * dlogtk + &
     &            (-106.736_r8*invtk + 0.69171_r8) * sqrts +           &
     &            (-0.65643_r8*invtk - 0.01844_r8) * s)      
!
! k2p = [H][HPO4]/[H2PO4]
!
! DOE(1994) eq 7.2.23 with footnote using data from Millero (1974))
!
      k2p = exp(-8814.715_r8*invtk + 172.0883_r8 - 27.927_r8 * dlogtk+ &
     &            (-160.340_r8*invtk + 1.3566_r8) * sqrts +            &
     &            (0.37335_r8*invtk - 0.05778_r8) * s)
!
! k3p = [H][PO4]/[HPO4]
!
! DOE(1994) eq 7.2.26 with footnote using data from Millero (1974)
!
      k3p = exp(-3070.75_r8*invtk - 18.141_r8 +                        &
     &            (17.27039_r8*invtk + 2.81197_r8) *                   &
     &            sqrts + (-44.99486_r8*invtk - 0.09984_r8) * s)
!
! ksi = [H][SiO(OH)3]/[Si(OH)4]
!
! Millero p.671 (1995) using data from Yao and Millero (1995)
!
      ksi = exp(-8904.2_r8*invtk + 117.385_r8 - 19.334_r8 * dlogtk +   &
     &            (-458.79_r8*invtk + 3.5913_r8) * sqrtis +            &
     &            (188.74_r8*invtk - 1.5998_r8) * is +                 &
     &            (-12.1652_r8*invtk + 0.07871_r8) * is2 +             &
     &            log(1.0_r8-0.001005_r8*s))
!
! kw = [H][OH]
!
! Millero p.670 (1995) using composite data
!
      kw = exp(-13847.26_r8*invtk + 148.9652_r8 - 23.6521_r8 *dlogtk+  &
     &            (118.67_r8*invtk - 5.977_r8 + 1.0495_r8 * dlogtk) *  &
     &            sqrts - 0.01615_r8 * s)
!
! ks = [H][SO4]/[HSO4]
!
! Dickson (1990, J. chem. Thermodynamics 22, 113)
!
      ks=exp(-4276.1_r8*invtk + 141.328_r8 - 23.093_r8*dlogtk +        &
     &    (-13856.0_r8*invtk +324.57_r8-47.986_r8*dlogtk)*sqrtis+      &
     &    (35474.0_r8*invtk - 771.54_r8 + 114.723_r8*dlogtk) * is -    &
     &     2698.0_r8*invtk*is**1.5_r8 + 1776.0_r8*invtk*is2 +          &
     &     log(1.0_r8 - 0.001005_r8*s))
!
! kf = [H][F]/[HF]
!
! Dickson and Riley (1979) -- change pH scale to total
!
      kf=exp(1590.2_r8*invtk - 12.641_r8 + 1.525_r8*sqrtis +           &
     &            log(1.0_r8 - 0.001005_r8*s) +                        &
     &            log(1.0_r8 + (0.1400_r8/96.062_r8)*(scl)/ks))
!
! Calculate concentrations for borate, sulfate, and fluoride
!
! Uppstrom (1974)
      bt = 0.000232_r8 * scl/10.811_r8
! Morris & Riley (1966)
      st = 0.14_r8 * scl/96.062_r8
! Riley (1965)
      ft = 0.000067_r8 * scl/18.9984_r8
!
!
! Calculate [H+] total when DIC and TA are known at T, S and 1 atm.
! The solution converges to err of xacc. The solution must be within
! the range x1 to x2.
!
! If DIC and TA are known then either a root finding or iterative method
! must be used to calculate htotal. In this case we use the 
! Newton-Raphson "safe" method taken from "Numerical Recipes" (function 
! "rtsafe.f" with error trapping removed).
!
! As currently set, this procedure iterates about 12 times. The x1 and 
! x2 values set below will accomodate ANY oceanographic values. If an 
! initial guess of the pH is known, then the number of iterations can 
! be reduced to about 5 by narrowing the gap between x1 and x2. 
! It is recommended that the first few time steps be run with x1 and x2 
! set as below. After that, set x1 and x2 to the previous value of the 
! pH +/- ~0.5. The current setting of xacc will result in co2star 
! accurate to 3 significant figures (xx.y). Making xacc bigger will 
! result in faster convergence also, but this is not recommended 
! (xacc of 10**-9 drops precision to 2 significant figures).
!
! Parentheses added around negative exponents (Keith Lindsay)
!
      x1 = 10.0_r8**(-phhi)
      x2 = 10.0_r8**(-phlo)
      xacc = 0.0000000001_r8
      call drtsafe(x1,x2,xacc,                                         &
    & k0,k1,k2,k1p,k2p,k3p,st,ks,dic,bt,kb,kw,pt,sit,ksi,ft,kf,ta,ff,  &
    & htotal )
!
! Calculate [CO2*] as defined in DOE Methods Handbook 1994 Ver.2, 
! ORNL/CDIAC-74, Dickson and Goyet, eds. (Ch 2 p 10, Eq A.49)
!
      htotal2=htotal*htotal
      co2star=dic*htotal2/(htotal2 + k1*htotal + k1*k2)
      co2starair=xco2*ff*atmpres
      dco2star=co2starair-co2star
      ph=-log10(htotal)

        pCO2surf = co2star / ff
        dpCO2    = pCO2surf - xco2*atmpres
!
!  Convert units of output arguments
!      Note: co2star and dco2star are calculated in mol/kg within this  
!      routine. Thus Convert now from mol/kg -> mol/m^3

       co2star  = co2star / permil
       dco2star = dco2star / permil

       pCO2surf = pCO2surf / permeg
       dpCO2    = dpCO2 / permeg
!       write(*,*) '++++++++',pCO2surf,dpCO2,co2star,ff,ph,htotal,  &
!     &k1,k2,permil,permeg      
      RETURN
      END SUBROUTINE co2calc
 
!-----------------------------------------------------------------------     
     SUBROUTINE DRTSAFE(X1,X2,XACC,                                    &
   & k0,k1,k2,k1p,k2p,k3p,st,ks,dic,bt,kb,kw,pt,sit,ksi,ft,kf,ta,ff,   &
   & DRTSAFE2)
      USE mod_kinds
      implicit none
      real(r8),intent(in)  :: X1,X2,XACC
      real(r8),intent(in)  :: k0,k1,k2,k1p,k2p,k3p,st,ks,dic,bt
      real(r8),intent(in)  :: kb,kw,pt,sit,ksi,ft,kf,ta,ff
      real(r8),intent(out) :: DRTSAFE2
      
      integer  :: j,MAXIT
      real(r8) :: FL,DF,FH,XL,XH,SWAP,DXOLD,DX,TEMP,F
!
!      File taken from Numerical Recipes. Modified  R.M.Key 4/94
!
      MAXIT=100
      CALL ta_iter_1(X1,FL,DF,k0,k1,k2,k1p,k2p,k3p,st,ks,dic,bt,kb,   &
    &  	      kw,pt,sit,ksi,ft,kf,ta,ff)
      CALL ta_iter_1(X2,FH,DF,k0,k1,k2,k1p,k2p,k3p,st,ks,dic,bt,kb,   &
    &  	      kw,pt,sit,ksi,ft,kf,ta,ff)
      IF(FL .LT. 0.0_r8) THEN
        XL=X1
        XH=X2
      ELSE
        XH=X1
        XL=X2
        SWAP=FL
        FL=FH
        FH=SWAP
      END IF
      DRTSAFE2=0.5_r8*(X1+X2)
      DXOLD=ABS(X2-X1)
      DX=DXOLD
      CALL ta_iter_1(DRTSAFE2,F,DF,k0,k1,k2,k1p,k2p,k3p,st,ks,dic,bt, &
     & 	      kb,kw,pt,sit,ksi,ft,kf,ta,ff)
      DO J=1,MAXIT
        IF(((DRTSAFE2-XH)*DF-F)*((DRTSAFE2-XL)*DF-F) .GE. 0.0_r8 .OR. &
     &            ABS(2.0_r8*F) .GT. ABS(DXOLD*DF)) THEN
          DXOLD=DX
          DX=0.5_r8*(XH-XL)
          DRTSAFE2=XL+DX
          IF(XL .EQ. DRTSAFE2) RETURN
        ELSE
          DXOLD=DX
          DX=F/DF
          TEMP=DRTSAFE2
          DRTSAFE2=DRTSAFE2-DX
          IF(TEMP .EQ. DRTSAFE2) RETURN
      END IF
        IF(ABS(DX) .LT. XACC) RETURN
      CALL ta_iter_1(DRTSAFE2,F,DF,k0,k1,k2,k1p,k2p,k3p,st,ks,dic,bt,  &
     &   	kb,kw,pt,sit,ksi,ft,kf,ta,ff)
        IF(F .LT. 0.0_r8) THEN
          XL=DRTSAFE2
          FL=F
        ELSE
          XH=DRTSAFE2
          FH=F
        END IF
       END DO
      RETURN
      END SUBROUTINE DRTSAFE
!-----------------------------------------------------------------------  
  SUBROUTINE ta_iter_1(x,fn,df,k0,k1,k2,k1p,k2p,k3p,st,ks,dic,bt,kb,&
    &  	      kw,pt,sit,ksi,ft,kf,ta,ff)
      USE mod_kinds
      implicit none
      real(r8),intent(in)  :: x,k0,k1,k2,k1p,k2p,k3p,st,ks,dic,bt,kb
      real(r8),intent(in)  :: kw,pt,sit,ksi,ft,kf,ta,ff
      real(r8),intent(out) :: fn,df
      
      real(r8) ::  k12,k12p,k123p,x2,x3,c,a,a2,da,b,b2,db
  
!
! This routine expresses TA as a function of DIC, htotal and constants.
! It also calculates the derivative of this function with respect to 
! htotal. It is used in the iterative solution for htotal. In the call
! "x" is the input value for htotal, "fn" is the calculated value for TA
! and "df" is the value for dTA/dhtotal
!
      x2=x*x
      x3=x2*x
      k12 = k1*k2
      k12p = k1p*k2p
      k123p = k12p*k3p
      c = 1.0_r8 + st/ks
      a = x3 + k1p*x2 + k12p*x + k123p
      a2=a*a
      da = 3.0_r8*x2 + 2.0_r8*k1p*x + k12p
      b = x2 + k1*x + k12
      b2=b*b
      db = 2.0_r8*x + k1
!
!   fn = hco3+co3+borate+oh+hpo4+2*po4+silicate+hfree+hso4+hf+h3po4-ta
!
      fn = k1*x*dic/b +                                   &
     &           2.0_r8*dic*k12/b +                       &
     &           bt/(1.0_r8 + x/kb) +                     &
     &           kw/x +                                   &
     &           pt*k12p*x/a +                            &
     &           2.0_r8*pt*k123p/a +                      &
     &           sit/(1.0_r8 + x/ksi) -                   &
     &           x/c -                                    &
     &           st/(1.0_r8 + ks/x/c) -                   &
     &           ft/(1.0_r8 + kf/x) -                     &
     &           pt*x3/a -                                &
     &           ta
!
!      df = dfn/dx
!
      df = ((k1*dic*b) - k1*x*dic*db)/b2 -                             &
     &           2.0_r8*dic*k12*db/b2 -                                &
     &           bt/kb/(1.0_r8+x/kb)**2.0_r8 -                         &
     &           kw/x2 +                                               &
     &           (pt*k12p*(a - x*da))/a2 -                             &
     &           2.0_r8*pt*k123p*da/a2 -                               &
     &           sit/ksi/(1.0_r8+x/ksi)**2.0_r8 -                      &
     &           1.0_r8/c +                                            &
     &           st*(1.0_r8 + ks/x/c)**(-2.0_r8)*(ks/c/x2) +           &
     &           ft*(1.0_r8 + kf/x)**(-2.0_r8)*kf/x2 -                 &
     &           pt*x2*(3.0_r8*a-x*da)/a2               
     
      return
      END SUBROUTINE ta_iter_1
!--------------------
#endif    
      
      
      
      
#ifdef OXYGEN
      SUBROUTINE O2_flux (Istr, Iend,                                  &
     &                       LBi, UBi, LBj, UBj,                       &
     &                       IminS, ImaxS, j,                          &
#  ifdef MASKING
     &                       rmask,                                    &
#  endif
     &                       T, S, O2, kw660, ppo, o2sat, O2flx)
!
!***********************************************************************
!                                                                      !
!  Computes the time rate of change of oxygen in the surface           !
!  layer due to air-sea gas exchange in mol/m^3/day                    !
!                                                                      !
!  On Input:                                                           !
!                                                                      !
!     Istr       Starting tile index in the I-direction.               !
!     Iend       Ending   tile index in the I-direction.               !
!     LBi        I-dimension lower bound.                              !
!     UBi        I-dimension upper bound.                              !
!     LBj        J-dimension lower bound.                              !
!     UBj        J-dimension upper bound.                              !
!     IminS      I-dimension lower bound for private arrays.           !
!     ImaxS      I-dimension upper bound for private arrays.           !
!     j          j-pipelined index.                                    !
!     rmask      Land/Sea masking.                                     !
!     T          Surface temperature (Celsius).                        !
!     S          Surface salinity (PSS).                               !
!     O2         Dissolevd oxygen concentration (micromole O2/m^3)     !
!     kw660      gas transfer velocity at a Schmidt number of 660,     !
!                  accounting for sea ice fraction (cm/hr)             !
!     ppo        surface pressure divided by 1 atm.                    !
!                                                                      !
!  On Output:                                                          !
!                                                                      !
!     o2sat      dissolved oxygen saturation concentration (mmol/m^3)  !
!                  due to air-sea exchange (mmol/m^2/day)              !
!     o2flx      time rate of oxygen O2 flux in the sea surface        !
!                  due to air-sea exchange (mmol/m^2/day)              !
!                                                                      !
!                                                                      !
!***********************************************************************
!
      USE mod_kinds
!
      implicit none
!
!  Imported variable declarations.
!
      integer,  intent(in) :: LBi, UBi, LBj, UBj, IminS, ImaxS
      integer,  intent(in) :: Istr, Iend, j
!
      real(r8),  intent(in) :: ppo
!
#  ifdef ASSUMED_SHAPE
#   ifdef MASKING
      real(r8), intent(in) :: rmask(LBi:,LBj:)
#   endif
      real(r8), intent(in) :: T(IminS:)
      real(r8), intent(in) :: S(IminS:)
      real(r8), intent(in) :: O2(IminS:)
#  else
#   ifdef MASKING
      real(r8), intent(in) :: rmask(LBi:UBi,LBj:UBj)
#   endif
      real(r8), intent(in) :: T(IminS:ImaxS)
      real(r8), intent(in) :: S(IminS:ImaxS)
      real(r8), intent(in) :: O2(IminS:ImaxS)
#  endif

      real(r8), intent(in) :: kw660(IminS:ImaxS)

      real(r8), intent(out) :: o2sat(IminS:ImaxS)
      real(r8), intent(out) :: o2flx(IminS:ImaxS)
!
!  Local variable declarations.
!

      integer :: i

      real(r8) :: sco2, kwo2
      real(r8) :: TT, TK, TS, TS2, TS3, TS4, TS5, CO

      real(r8), parameter :: A0 = 2.00907_r8       ! Oxygen
      real(r8), parameter :: A1 = 3.22014_r8       ! saturation
      real(r8), parameter :: A2 = 4.05010_r8       ! coefficients
      real(r8), parameter :: A3 = 4.94457_r8
      real(r8), parameter :: A4 =-0.256847_r8
      real(r8), parameter :: A5 = 3.88767_r8
      real(r8), parameter :: B0 =-0.00624523_r8
      real(r8), parameter :: B1 =-0.00737614_r8
      real(r8), parameter :: B2 =-0.0103410_r8
      real(r8), parameter :: B3 =-0.00817083_r8
      real(r8), parameter :: C0 =-0.000000488682_r8
!
!=======================================================================
!  Determine coefficients.  If land/sea
!  masking, compute only on water points.
!=======================================================================
!
      I_LOOP: DO i=Istr,Iend
#  ifdef MASKING
        IF (rmask(i,j).gt.0.0_r8) THEN
#  endif
!
! ********************************************************************
!                                     
! Computes the oxygen saturation concentration at 1 atm total pressure
! in mmol/m^3 given the temperature (t, in deg C) and the salinity (s,
! in permil). 
!
! FROM GARCIA AND GORDON (1992), LIMNOLOGY and OCEANOGRAPHY.
! THE FORMULA USED IS FROM PAGE 1310, EQUATION (8).
!
! o2sato IS DEFINED BETWEEN T(freezing) <= T <= 40(deg C) AND
! 0 permil <= S <= 42 permil
! C
! CHECK VALUE:  T = 10.0 deg C, S = 35.0 permil, 
! o2sato = 282.015 mmol/m^3
!
! ********************************************************************
!
      TT  = 298.15_r8-T(i)
      TK  = 273.15_r8+T(i)
      TS  = LOG(TT/TK)
      TS2 = TS**2
      TS3 = TS**3
      TS4 = TS**4
      TS5 = TS**5
      CO  = A0 + A1*TS + A2*TS2 + A3*TS3 + A4*TS4 + A5*TS5             &
     &     + S(i)*(B0 + B1*TS + B2*TS2 + B3*TS3)                       &
     &     + C0*(S(i)*S(i))
      o2sat(i) = EXP(CO)
!
!  Convert from ml/l to mol/m^3
!
      o2sat(i) = (o2sat(i)/22391.6_r8)*1000.0_r8
!
!  Convert from mol/m^3 to mmol/m^3
!
      o2sat(i) = o2sat(i)*1000.0_r8
!
!
!*********************************************************************
!
!  Computes the Schmidt number of oxygen in seawater using the
!  formulation proposed by Keeling et al. (1998, Global Biogeochem.
!  Cycles, 12, 141-163).  Input is temperature in deg C.
!
!*********************************************************************
!
      sco2 = 1638.0_r8 - 81.83_r8*t(i) +                               &
     &       1.483_r8*t(i)**2 - 0.008004_r8*t(i)**3

!
!  Compute the transfer velocity for O2 in m/s
!
!    kwo2 = Kw660 * (sco2(t)/660)**-0.5*0.01/3600.0    !(in  m/sec)
!
      kwo2 = Kw660(i) * sqrt(660.0_r8/sco2)   !(in units of cm/hr)
!
!  Compute the transfer velocity for O2 in m/day
!
      KWO2=KWO2*0.01_r8*24.0_r8
!
!  (in units of m/day)
!
!  Compute the saturation concentrations for O2
!
!      o2sat(i) = o2sato(t,s)*ppo       
!  OCMIP
!      o2sat = dosat(t+273.15,s)     
!  Weiss
!
!  Compute time rate of O2 gas exchange
!
      o2flx(i) = kwo2*(o2sat(i)*ppo-o2(i))
#  ifdef MASKING
      ELSE
        o2sat(i)=0.0_r8
        o2flx(i)=0.0_r8
      END IF
#  endif

      END DO I_LOOP
      RETURN
      END SUBROUTINE O2_flux
# endif




#ifdef OPTICS_OP1
      subroutine optic_property(Istr, Iend, ng,                         &
     &                       LBi, UBi, LBj, UBj, UBk,                   &
     &                       IminS, ImaxS, j,                           &
#  ifdef MASKING
     &                       rmask,                                     &
#  endif
     &                       salt,                                      &
     &                       hzl,                                       &
     &                       chl1, chl2,                                &  
     &                       s1, s2,                                    &
     &                       ddn,                                       &
     &                       a_abs, bbp, bb, bts, kdpar)
!
!***********************************************************************
!                                                                      !
!  This routine computes water optical properties                      !
!                                                                      !
!  By Peng Xiu                                                !
!                                                                      !
!***********************************************************************
!
      USE mod_kinds
      USE mod_param

      implicit none
!
!  Imported variable declarations.
!
      integer, parameter :: mmax = 31
      integer,  intent(in) :: ng
      integer,  intent(in) :: LBi, UBi, LBj, UBj, UBk, IminS, ImaxS
      integer,  intent(in) :: Istr, Iend, j
!
#  ifdef ASSUMED_SHAPE
#   ifdef MASKING
      real(r8), intent(in) :: rmask(LBi:,LBj:)
#   endif
      real(r8), intent(in) :: salt(IminS:,:)
      real(r8), intent(in) :: hzl(IminS:,:)
      real(r8), intent(in) :: chl1(IminS:,:)
      real(r8), intent(in) :: chl2(IminS:,:)
      real(r8), intent(in) :: s1(IminS:,:)
      real(r8), intent(in) :: s2(IminS:,:)
      real(r8), intent(in) :: ddn(IminS:,:)
      real(r8), intent(out) :: a_abs(IminS:,:,:)
      real(r8), intent(out) :: bbp(IminS:,:,:)
      real(r8), intent(out) :: bb(IminS:,:,:)
      real(r8), intent(out) :: bts(IminS:,:,:)
      real(r8), intent(out) :: kdpar(IminS:,:)
#  else
#   ifdef MASKING
      real(r8), intent(in) :: rmask(LBi:UBi,LBj:UBj)
#   endif
      real(r8), intent(in) :: T(IminS:ImaxS)
      real(r8), intent(in) :: S(IminS:ImaxS)
      real(r8), intent(in) :: O2(IminS:ImaxS)

      real(r8), intent(in) :: salt(IminS:ImaxS,N(ng))
      real(r8), intent(in) :: hzl(IminS:ImaxS,N(ng))
      real(r8), intent(in) :: chl1(IminS:ImaxS,N(ng))
      real(r8), intent(in) :: chl2(IminS:ImaxS,N(ng))
      real(r8), intent(in) :: s1(IminS:ImaxS,N(ng))
      real(r8), intent(in) :: s2(IminS:ImaxS,N(ng))
      real(r8), intent(in) :: ddn(IminS:ImaxS,N(ng))
      real(r8), intent(out) :: a_abs(IminS:ImaxS,N(ng),mmax)
      real(r8), intent(out) :: bbp(IminS:ImaxS,N(ng),mmax)
      real(r8), intent(out) :: bb(IminS:ImaxS,N(ng),mmax)
      real(r8), intent(out) :: bts(IminS:ImaxS,N(ng),mmax)
      real(r8), intent(out) :: kdpar(IminS:ImaxS,N(ng))
#  endif

!
!  Local variable declarations.
!

      integer :: i, k, otrc


      real(r8), dimension(N(ng)) :: kpar1
      real(r8), dimension(N(ng)) :: kpar2
      real(r8), dimension(N(ng),mmax) :: bbw
      real(r8), dimension(N(ng),mmax) :: achl1
      real(r8), dimension(N(ng),mmax) :: achl2
      real(r8), dimension(N(ng),mmax) :: ap
      real(r8), dimension(N(ng),mmax) :: ap1
      real(r8), dimension(N(ng),mmax) :: ap2
      real(r8), dimension(N(ng),mmax) :: adet
      real(r8), dimension(N(ng),mmax) :: acdom
      real(r8), dimension(N(ng),mmax) :: bbp1
      real(r8), dimension(N(ng),mmax) :: bbp2

      real(r8), parameter :: kapa0=-0.057_r8
      real(r8), parameter :: kapa1=0.482_r8
      real(r8), parameter :: kapa2=4.221_r8
      real(r8), parameter :: ksai0=0.183_r8
      real(r8), parameter :: ksai1=0.702_r8
      real(r8), parameter :: ksai2=-2.567_r8
      real(r8), parameter :: alpha0=0.090_r8
      real(r8), parameter :: alpha1=1.465_r8
      real(r8), parameter :: alpha2=-0.667_r8
      real(r8), parameter :: thetaa=5.0_r8*3.1416_r8/180.0_r8
!thetaa solar zenith angle degree
      real(r8), parameter :: massPOC   = 12.0_r8
      real(r8), parameter :: bbg=0.00035_r8
      real(r8), parameter :: r_phy_POC=0.3_r8

      real(r8), dimension(mmax) :: aw_abs
      real(r8), dimension(mmax) :: bw_abs
      real(r8), dimension(mmax) :: achlstar
      real(r8), dimension(mmax) :: adetstar
      real(r8) :: cff

      data aw_abs/0.0066,0.0047,0.0045,0.0050,0.0063,0.0092,0.0098,  &
     &0.0106,0.0127,0.0150,0.0204,0.0325,0.0409,0.0434,0.0474,       &
     &0.0565,0.0619,0.0695,0.0896,0.1351,0.2224,0.2644,0.2755,       &
     &0.2916,0.3180,0.3400,0.4100,0.4390,0.4650,0.5160,0.6240/
      data bw_abs/0.0076,0.0068,0.0061,0.0055,0.0049,0.0045,0.0041,  &
     &0.0037,0.0034,0.0031,0.0029,0.0026,0.0024,0.0022,0.0021,       &
     &0.0019,0.0018,0.0017,0.0016,0.0015,0.0014,0.0013,0.0012,       &
     &0.0011,0.0010,0.0010,0.0008,0.0008,0.0007,0.0007,0.0007/
      data achlstar/0.6870,0.8280,0.9130,0.9730,1.0000,0.9440,0.9170,&
     &0.8700,0.7980,0.7500,0.6680,0.6180,0.5280,0.4740,0.4160,       &
     &0.3570,0.2940,0.2760,0.2910,0.2820,0.2360,0.2520,0.2760,       &
     &0.3170,0.3340,0.3560,0.4410,0.5950,0.5020,0.3290,0.2150/
      data adetstar/0.1000,0.1000,0.1000,0.1000,0.1000,0.1000,0.1000,&
     &0.1000,0.1000,0.1000,0.1000,0.1000,0.1000,0.1000,0.1000,       &
     &0.1000,0.1000,0.1000,0.1000,0.1000,0.1000,0.1000,0.1000,       &
     &0.1000,0.1000,0.1000,0.1000,0.1000,0.1000,0.1000,0.1000/


      I_LOOP: DO i=Istr,Iend
#  ifdef MASKING
        IF (rmask(i,j).gt.0.0_r8) THEN
#  endif
      do k=1,N(ng)
           
        do otrc=1,mmax
           achl1(k,otrc)=0.06*achlstar(otrc)*(chl1(i,k))**0.65
           achl2(k,otrc)=0.06*achlstar(otrc)*(chl2(i,k))**0.65
           
          ap1(k,otrc)=achl1(k,otrc)
          ap2(k,otrc)=achl2(k,otrc)
          ap(k,otrc)=ap1(k,otrc)+ap2(k,otrc)

          adet(k,otrc)=adetstar(otrc)*ddn(i,k)*6.7_r8*0.001_r8*   &
     &     massPOC                                                &
     &    *exp(-0.011_r8*((400.0_r8+10.0_r8*(otrc-1))-440.0_r8))
           
           !Hong et al., 2005 Pearl river, a(355)
           !cff=-0.045_r8*salt(i,k)+1.8068_r8
           
           !Bai 2013JGR
           cff=max((salt(i,k)-35.595_r8)/(-14.151_r8),0.0_r8)
           
           aCDOM(k,otrc)=(cff+0.016)                              &
     &    *exp(-0.0145_r8*((400.0_r8+10.0_r8*(otrc-1))-355.0_r8))
        
            bbp1(k,otrc)                                          &
     & =( (s1(i,k)*6.7_r8*12.0_r8/r_phy_POC/476935.8_r8)          &
     & **(1.0_r8/1.277_r8))                                       &
     &    *( ((400.0_r8+10.0_r8*(otrc-1))/510.0_r8)**(-0.5_r8))
                      
                     bbp2(k,otrc)                                 &
     & =( (s2(i,k)*6.7_r8*12.0_r8/r_phy_POC/17069.0_r8)           &
     &  **(1.0_r8/0.859_r8))                                      &
     & *( ((400.0_r8+10.0_r8*(otrc-1))/510.0_r8)**(-0.5_r8) )
     


      bbp(i,k,otrc)=bbp1(k,otrc)+bbp2(k,otrc)+bbg   
      a_abs(i,k,otrc)=ap(k,otrc)+adet(k,otrc)+                    &
     &                acdom(k,otrc)+aw_abs(otrc)




!Kpar method from Lee et al., (2005), 

      bbw(k,otrc)=0.5_r8*bw_abs(otrc)*(1.0_r8+0.3_r8*salt(i,k)     &
    &   /37.0_r8)
      bb(i,k,otrc)=bbw(k,otrc)+bbp(i,k,otrc)
      bts(i,k,otrc)=r_phy_POC*(1.0_r8/0.01_r8)*bbp1(k,otrc) +       &
     &           r_phy_POC*(1.0_r8/0.006_r8)*bbp2(k,otrc) +         &
     & (1.0_r8-r_phy_POC)*(1.0_r8/0.015_r8)*(bbp1(k,otrc) +         &
     &                bbp2(k,otrc)) +                               &
     &               (1.0_r8/0.020_r8)*bbg

       bts(i,k,otrc) = bts(i,k,otrc) + bw_abs(otrc)   !add pure water b
	   
       enddo   !otrc (wavelength) loop
       
       kpar1(k)=(kapa0+kapa1*sqrt(a_abs(i,k,10))+kapa2*bb(i,k,10))  &
     &          *(1.0_r8+alpha0*sin(thetaa))
       kpar2(k)=(ksai0+ksai1*a_abs(i,k,10)+ksai2*bb(i,k,10))        &
     &         *(alpha1+alpha2*(cos(thetaa)))
       kdpar(i,k)= kpar1(k)+kpar2(k)/sqrt(1.0_r8+hzl(i,k))

      enddo

#  ifdef MASKING
      END IF
#  endif
      END DO I_LOOP

      RETURN
      END SUBROUTINE optic_property
#endif







