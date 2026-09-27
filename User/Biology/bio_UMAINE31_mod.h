!                                                                      !
!  Parameters for UMaine CoSiNE model:                                 !
!                                                                      !
!   reg1     Microzooplankton excretion rate to ammonium [1/day].      !
!   reg2     Mesozooplankton excretion rate to ammonium [1/day].       !
!   gmaxs1   Maximum specific growth rate of small phytoplankton       !
!              [1/day]                                                 !
!   gmaxs2   Maximum specific growth rate of diatom [1/day]            !
!   beta1    Microzooplankton maximum grazing rate [1/day]             !
!   beta2    Mesozooplankton maximum grazing rate [1/day]              !
!   akz1     Half saturation constant for microzooplankton grazing     !
!              [mmol_N/m3]                                             !
!   akz2     Half saturation constant for mesozooplankton grazing      !
!              [mmol_N/m3]                                             !
!   PARfrac  Fraction of shortwave radiation that is available for     !
!              photosyntesis [nondimensional].                         !
!   amaxs2   Initial slope of P-I curve of small phytoplankton         !
!              [1/(Watts/m2)/day]                                      !
!   parsats1 PAR saturation onset parameter of small phytoplankton     !
!              [Watts/m2]                                              !
!   parsats2 PAR saturation onset parameter of diatom [Watts/m2]       !
!              [Watts/m2]                                              !
!   pis1     Ammonium inhibition parameter for small phytoplankton     !
!              [mmol_N/m3]                                             !
!   pis2     Ammonium inhibition parameter for diatom [mmol_N/m3]      !
!   akno3s1  Half saturation concentration for nitrate uptake by       !
!              small phytoplankton [mmol_N/m3].                        !
!   akno3s2  Half saturation concentration for nitrate uptake by       !
!              diatom [mmol_N/m3].                                     !
!   aknh4s1  Half saturation concentration for ammonium uptake by      !
!              small phytoplankton [mmol_N/m3].                        !
!   aknh4s2  Half saturation concentration for ammonium uptake by      !
!              diatom [mmol_N/m3].                                     !
!   akpo4s1  Half saturation concentration for phosphate uptake by     !
!              small phytoplankton [mmol_P/m3].                        !
!   akpo4s2  Half saturation concentration for phosphate uptake by     !
!              diatom [mmol_P/m3].                                     !
!   akco2s1  Half saturation concentration for co2 uptake by           !
!              small phytoplankton [mmol_C/m3].                        !
!   akco2s2  Half saturation concentration for co2 uptake by           !
!              diatom [mmol_C/m3].                                     !
!   aksio4s2 Half saturation constant for silicate uptake by           !
!              diatom [mmol_Si/m3].                                    !
!   ak1      Light attenuation coefficient of water [1/m]              !
!   ak2      Specific light attenuation coefficient for                !
!              phytoplankton [1/m/(mmol_N/m3)].                        !
!   bgamma0   Mesozooplankton specific mortality rate [1/day].         !
!   bgamma1   Grazing efficiency of microzooplankton [nondimensional]. !
!   bgamma2   Grazing efficiency of mesozooplankton [nondimensional].  !
!   bgamma3   Death rate of small phytoplankton [1/day].               !
!   bgamma4   Death rate of large phytoplankton [1/day].               !
!   bgamma5   Decay rate of detritus [1/day].                          !
!   bgamma6                                                            !
!   bgamma7   Nitrafication rate [1/day].                              !
!   wsd      Sinking velocity of detritus [m/day].                     !
!   wsdsi    Sinking velocity of detritus silicate [m/day].            !
!   wsp      Sinking velocity of large phytoplankton [m/day].          !
!   pco2a    Air pCO2 [ppmv].                                          !
!   si2n     Silicate to nitrogen ratio [mol_Si/mol_N].                !
!   p2n      Phosphorus to nitrogen ratio [mol_P/mol_N].               !
!   o2no     Oxygen to nitrate ratio [mol_O2/mol_NO3].                 !
!   o2nh     Oxygen to ammonium ratio [mol_O2/mol_NH4].                !
!   c2n      Carbon to nitrogen ratio [mol_C/mol_N].                   !
!   ro5      Grazing preference for diatom [nondimensional].           !
!   ro6      Grazing preference for mesozooplankton [nondimensional]   !
!   ro7      Grazing preference for detritus [nondimensional].         !
!   By: PENG XIU 12/2013                                               !
!=======================================================================
!
      USE mod_param

      implicit none
!
!  Set biological tracer identification indices.
!
      integer, allocatable :: idbio(:) ! Biological tracers
      integer :: iNO3_                 ! Nitrate concentration
      integer :: iNH4_                 ! Ammonium concentration
      integer :: iSiOH                 ! Silicate concentration
      integer :: iPO4_                 ! Phosphate concentration
      integer :: iS1_N                 ! Samll phytoplankton N
      integer :: iS1_C                 ! Samll phytoplankton C
      integer :: iS1CH                 ! Samll phytoplankton CHL
      integer :: iS2_N                 ! Diatom concentration N
      integer :: iS2_C                 ! Diatom concentration C
      integer :: iS2CH                 ! Diatom concentration CHL
      integer :: iS3_N                 ! coccolithophores concentration N
      integer :: iS3_C                 ! coccolithophores concentration C
      integer :: iS3CH                 ! coccolithophores concentration CHL
      integer :: iZ1_N                 ! Small zooplankotn concentration N
      integer :: iZ1_C                 ! Small zooplankotn concentration C
      integer :: iZ2_N                 ! Mesozooplankotn concentration N
      integer :: iZ2_C                 ! Mesozooplankotn concentration C
      integer :: iBAC_                 ! Bacteria concentration N
      integer :: iDD_N                 ! Detritus concentration N
      integer :: iDD_C                 ! Detritus concentration C
      integer :: iDDSi                 ! Biogenic silicate concentration
      integer :: iLDON                 ! Labile dissolved organic matter N
      integer :: iLDOC                 ! Labile dissolved organic matter C
      integer :: iSDON                 ! Semi-labile dissolved organic matter N
      integer :: iSDOC                 ! semi-labile dissolved organic matter C
      integer :: iCLDC                 ! Colored labile dissolved organic matter C
      integer :: iCSDC                 ! Colored semi-labile dissolved organic matter C
      integer :: iDDCA                 ! Particulate inorganic carbon concentration
#ifdef OXYGEN
      integer :: iOxyg                 ! Dissolved oxygen concentration
#endif
#ifdef CARBON
      integer :: iTIC_                 ! Total inorganic carbon
      integer :: iTAlk                 ! Total alkalinity
#endif

#if defined DIAGNOSTICS && defined DIAGNOSTICS_BIO
!
!  Biological 2D diagnostic variable IDs.
!
      integer, allocatable :: iDbio2(:)       ! 2D biological terms

      integer  :: iCOfx                       ! air-sea CO2 flux
      integer  :: ipCO2                       ! partial pressure of CO2
      integer  :: iO2fx                       ! air-sea O2 flux
!
!  Biological 3D diagnostic variable IDs.
!
      integer, allocatable :: iDbio3(:)       ! 3D biological terms

      integer  :: iPPro1 = 1                   ! primary productivity for s1
      integer  :: iPPro2 = 2                   ! primary productivity for s2
      integer  :: iPPro3 = 3                   ! primary productivity for s3
      integer  :: iNO3u = 4                   ! NO3 uptake
#endif


      integer, allocatable :: BioIter(:)

      real(r8), allocatable :: reg1(:)            ! 1/day
      real(r8), allocatable :: reg2(:)            ! 1/day
      real(r8), allocatable :: gmaxs1(:)          ! 1/day
      real(r8), allocatable :: gmaxs2(:)          ! 1/day
      real(r8), allocatable :: gmaxs3(:)          ! 1/day
      real(r8), allocatable :: beta1(:)           ! 1/day
      real(r8), allocatable :: beta2(:)           ! 1/day
      real(r8), allocatable :: akz1(:)            ! mmol_N/m3
      real(r8), allocatable :: akz2(:)            ! mmol_N/m3
      real(r8), allocatable :: PARfrac(:)         ! nondimensional
      real(r8), allocatable :: alphachl_s1(:)     ! mol_C m2/(g_Chl W day)
      real(r8), allocatable :: alphachl_s2(:)     ! mol_C m2/(g_Chl W day)
      real(r8), allocatable :: alphachl_s3(:)     ! mol_C m2/(g_Chl W day)
      real(r8), allocatable :: pis1(:)            ! m3/mmol_N
      real(r8), allocatable :: pis2(:)            ! m3/mmol_N
      real(r8), allocatable :: pis3(:)            ! m3/mmol_N
      real(r8), allocatable :: akno3s1(:)         ! mmol_N/m3
      real(r8), allocatable :: akno3s2(:)         ! mmol_N/m3
      real(r8), allocatable :: akno3s3(:)         ! mmol_N/m3
      real(r8), allocatable :: aknh4s1(:)         ! mmol_N/m3
      real(r8), allocatable :: aknh4s2(:)         ! mmol_N/m3
      real(r8), allocatable :: aknh4s3(:)         ! mmol_N/m3
      real(r8), allocatable :: akpo4s1(:)         ! mmol_P/m3
      real(r8), allocatable :: akpo4s2(:)         ! mmol_P/m3
      real(r8), allocatable :: akpo4s3(:)         ! mmol_P/m3
      real(r8), allocatable :: akco2s1(:)         ! mmol_C/m3
      real(r8), allocatable :: akco2s2(:)         ! mmol_C/m3
      real(r8), allocatable :: akco2s3(:)         ! mmol_C/m3
      real(r8), allocatable :: aksio4s2(:)        ! mmol_Si/m3
      real(r8), allocatable :: ES1(:)             ! nondimensional
      real(r8), allocatable :: ES2(:)             ! nondimensional
      real(r8), allocatable :: ES3(:)             ! nondimensional
      real(r8), allocatable :: ak1(:)             ! 1/m
      real(r8), allocatable :: ak2(:)             ! 1/m/(mmol_N/m3)
      real(r8), allocatable :: Qmax(:)            ! mol_N/mol_C
      real(r8), allocatable :: Qmin(:)            ! mol_N/mol_C
      real(r8), allocatable :: lambdano3_s1(:)    ! mol_C/mol_N
      real(r8), allocatable :: lambdano3_s2(:)    ! mol_C/mol_N
      real(r8), allocatable :: lambdano3_s3(:)    ! mol_C/mol_N
      real(r8), allocatable :: thetaNmax_s1(:)    ! g_Chl/mol_N
      real(r8), allocatable :: thetaNmax_s2(:)    ! g_Chl/mol_N
      real(r8), allocatable :: thetaNmax_s3(:)    ! g_Chl/mol_N
      real(r8), allocatable :: bgamma(:)          ! 1/day
      real(r8), allocatable :: bgamma1(:)         ! [nondimensional]
      real(r8), allocatable :: bgamma2(:)         ! [nondimensional]
      real(r8), allocatable :: bgamma22(:)        ! [nondimensional]
      real(r8), allocatable :: bgamma3(:)         ! 1/day
      real(r8), allocatable :: bgamma4(:)         ! 1/day
      real(r8), allocatable :: bgamma10(:)        ! 1/day
      real(r8), allocatable :: bgamma12(:)        ! 1/day
      real(r8), allocatable :: bgamma5(:)         ! 1/day
      real(r8), allocatable :: bgamma7(:)         ! 1/day
      real(r8), allocatable :: bgamma11(:)        ! 1/day
      real(r8), allocatable :: bgamma13(:)        ! 1/day
      real(r8), allocatable :: mtos1(:)           ! nondimensional
      real(r8), allocatable :: mtos2(:)           ! nondimensional
      real(r8), allocatable :: mtos3(:)           ! nondimensional
      real(r8), allocatable :: flz1(:)            ! nondimensional
      real(r8), allocatable :: flz2(:)            ! nondimensional
      real(r8), allocatable :: lk1(:)             ! nondimensional
      real(r8), allocatable :: lk2(:)             ! nondimensional
      real(r8), allocatable :: lk3(:)             ! nondimensional
      real(r8), allocatable :: ratiol1(:)         ! nondimensional
      real(r8), allocatable :: ratiol2(:)         ! nondimensional
      real(r8), allocatable :: wsdn(:)            ! m/day
      real(r8), allocatable :: wsdc(:)            ! m/day
      real(r8), allocatable :: wsdsi(:)           ! m/day
      real(r8), allocatable :: wsdca(:)           ! m/day
      real(r8), allocatable :: wsp1(:)            ! m/day
      real(r8), allocatable :: wsp2(:)            ! m/day
      real(r8), allocatable :: wsp3(:)            ! m/day
      real(r8), allocatable :: pco2a(:)           ! ppmv
      real(r8), allocatable :: p2n(:)             ! mol_P/mol_N
      real(r8), allocatable :: o2no(:)            ! mol_O2/mol_NO3
      real(r8), allocatable :: o2nh(:)            ! mol_O2/mol_NH4
      real(r8), allocatable :: cnb(:)             ! mol_C/mol_N
      real(r8), allocatable :: apsilon(:)         ! mol_C/mol_N
      real(r8), allocatable :: ro5(:)             ! nondimensional
      real(r8), allocatable :: ro6(:)             ! nondimensional
      real(r8), allocatable :: ro7(:)             ! nondimensional
      real(r8), allocatable :: ro10(:)            ! nondimensional
      real(r8), allocatable :: rop(:)             ! nondimensional
      real(r8), allocatable :: rob(:)             ! nondimensional
      real(r8), allocatable :: kabac(:)           ! mmol_N/m3
      real(r8), allocatable :: klbac(:)           ! mmol_C/m3
      real(r8), allocatable :: ksdoc(:)           ! mmol_C/m3
      real(r8), allocatable :: ksdon(:)           ! mmol_N/m3
      real(r8), allocatable :: ratiob(:)          ! nondimensional
      real(r8), allocatable :: ratiobc(:)         ! nondimensional
      real(r8), allocatable :: RtUVLDOC(:)        ! mmol_C/m2/d
      real(r8), allocatable :: RtUVSDOC(:)        ! mmol_C/m2/d
      real(r8), allocatable :: RtUVLDIC(:)        ! mmol_C/m2/d
      real(r8), allocatable :: RtUVSDIC(:)        ! mmol_C/m2/d
      real(r8), allocatable :: colorFR1(:)        ! nondimensional
      real(r8), allocatable :: colorFR2(:)        ! nondimensional

      CONTAINS

      SUBROUTINE initialize_biology
!
!=======================================================================
!                                                                      !
!  This routine sets several variables needed by the biology model.    !
!  It allocates and assigns biological tracers indices.                !
!                                                                      !
!=======================================================================
!
!  Local variable declarations
!
      integer :: i, ic
!
!-----------------------------------------------------------------------
!  Determine number of biological tracers.
!-----------------------------------------------------------------------
!
#ifdef CARBON
# ifdef OXYGEN
      NBT=31
# else
      NBT=30
# endif
#else
# ifdef OXYGEN
      NBT=29
# else
      NBT=28
# endif
#endif

#if defined DIAGNOSTICS && defined DIAGNOSTICS_BIO
      NDbio3d=4
      NDbio2d=0
# ifdef CARBON                                                                                        
      NDbio2d=NDbio2d+2                                                                               
# endif                                                                                               
# ifdef OXYGEN                                                                                        
      NDbio2d=NDbio2d+1                                                                               
# endif

     ic=0                                                                                            
# ifdef CARBON                                                                                        
      iCOfx=ic+1                                                                                      
      ipCO2=ic+2                                                                                      
      ic=ic+2                                                                                         
# endif                                                                                               
# ifdef OXYGEN                                                                                        
      iO2fx=ic+1                                                                                      
# endif                                                                                               
#endif 

!-----------------------------------------------------------------------
!  Allocate various module variables.
!-----------------------------------------------------------------------
      IF (.not.allocated(BioIter)) THEN
        allocate ( BioIter(Ngrids) )
      END IF
      IF (.not.allocated(reg1)) THEN
        allocate ( reg1(Ngrids) )
      END IF
      IF (.not.allocated(reg2)) THEN
        allocate ( reg2(Ngrids) )
      END IF
      IF (.not.allocated(gmaxs1)) THEN
        allocate ( gmaxs1(Ngrids) )
      END IF
      IF (.not.allocated(gmaxs2)) THEN
        allocate ( gmaxs2(Ngrids) )
      END IF
      IF (.not.allocated(gmaxs3)) THEN
        allocate ( gmaxs3(Ngrids) )
      END IF
      IF (.not.allocated(beta1)) THEN
        allocate ( beta1(Ngrids) )
      END IF
      IF (.not.allocated(beta2)) THEN
        allocate ( beta2(Ngrids) )
      END IF
      IF (.not.allocated(akz1)) THEN
        allocate ( akz1(Ngrids) )
      END IF
      IF (.not.allocated(akz2)) THEN
        allocate ( akz2(Ngrids) )
      END IF
      IF (.not.allocated(PARfrac)) THEN
        allocate ( PARfrac(Ngrids) )
      END IF
      IF (.not.allocated(alphachl_s1)) THEN
        allocate ( alphachl_s1(Ngrids) )
      END IF   
      IF (.not.allocated(alphachl_s2)) THEN
        allocate ( alphachl_s2(Ngrids) )
      END IF
      IF (.not.allocated(alphachl_s3)) THEN
        allocate ( alphachl_s3(Ngrids) )
      END IF
      IF (.not.allocated(pis1)) THEN
        allocate ( pis1(Ngrids) )
      END IF
      IF (.not.allocated(pis2)) THEN
        allocate ( pis2(Ngrids) )
      END IF
      IF (.not.allocated(pis3)) THEN
        allocate ( pis3(Ngrids) )
      END IF
      IF (.not.allocated(akno3s1)) THEN
        allocate ( akno3s1(Ngrids) )
      END IF
      IF (.not.allocated(akno3s2)) THEN
        allocate ( akno3s2(Ngrids) )
      END IF
      IF (.not.allocated(akno3s3)) THEN
        allocate ( akno3s3(Ngrids) )
      END IF
      IF (.not.allocated(aknh4s1)) THEN
        allocate ( aknh4s1(Ngrids) )
      END IF
      IF (.not.allocated(aknh4s2)) THEN
        allocate ( aknh4s2(Ngrids) )
      END IF
      IF (.not.allocated(aknh4s3)) THEN
        allocate ( aknh4s3(Ngrids) )
      END IF
      IF (.not.allocated(akpo4s1)) THEN
        allocate ( akpo4s1(Ngrids) )
      END IF
      IF (.not.allocated(akpo4s2)) THEN
        allocate ( akpo4s2(Ngrids) )
      END IF
      IF (.not.allocated(akpo4s3)) THEN
        allocate ( akpo4s3(Ngrids) )
      END IF
      IF (.not.allocated(akco2s1)) THEN
        allocate ( akco2s1(Ngrids) )
      END IF
      IF (.not.allocated(akco2s2)) THEN
        allocate ( akco2s2(Ngrids) )
      END IF
      IF (.not.allocated(akco2s3)) THEN
        allocate ( akco2s3(Ngrids) )
      END IF
      IF (.not.allocated(aksio4s2)) THEN
        allocate ( aksio4s2(Ngrids) )
      END IF
      IF (.not.allocated(ES1)) THEN
        allocate ( ES1(Ngrids) )
      END IF
      IF (.not.allocated(ES2)) THEN
        allocate ( ES2(Ngrids) )
      END IF
      IF (.not.allocated(ES3)) THEN
        allocate ( ES3(Ngrids) )
      END IF
      IF (.not.allocated(ak1)) THEN
        allocate ( ak1(Ngrids) )
      END IF
      IF (.not.allocated(ak2)) THEN
        allocate ( ak2(Ngrids) )
      END IF
      IF (.not.allocated(Qmax)) THEN
        allocate ( Qmax(Ngrids) )
      END IF
      IF (.not.allocated(Qmin)) THEN
        allocate ( Qmin(Ngrids) )
      END IF
      IF (.not.allocated(lambdano3_s1)) THEN
        allocate ( lambdano3_s1(Ngrids) )
      END IF
      IF (.not.allocated(lambdano3_s2)) THEN
        allocate ( lambdano3_s2(Ngrids) )
      END IF
      IF (.not.allocated(lambdano3_s3)) THEN
        allocate ( lambdano3_s3(Ngrids) )
      END IF
      IF (.not.allocated(thetaNmax_s1)) THEN
        allocate ( thetaNmax_s1(Ngrids) )
      END IF
      IF (.not.allocated(thetaNmax_s2)) THEN
        allocate ( thetaNmax_s2(Ngrids) )
      END IF
      IF (.not.allocated(thetaNmax_s3)) THEN
        allocate ( thetaNmax_s3(Ngrids) )
      END IF
      IF (.not.allocated(bgamma)) THEN
        allocate ( bgamma(Ngrids) )
      END IF
      IF (.not.allocated(bgamma1)) THEN
        allocate ( bgamma1(Ngrids) )
      END IF
      IF (.not.allocated(bgamma2)) THEN
        allocate ( bgamma2(Ngrids) )
      END IF
      IF (.not.allocated(bgamma22)) THEN
        allocate ( bgamma22(Ngrids) )
      END IF
      IF (.not.allocated(bgamma3)) THEN
        allocate ( bgamma3(Ngrids) )
      END IF
      IF (.not.allocated(bgamma4)) THEN
        allocate ( bgamma4(Ngrids) )
      END IF
      IF (.not.allocated(bgamma10)) THEN
        allocate ( bgamma10(Ngrids) )
      END IF
      IF (.not.allocated(bgamma12)) THEN
        allocate ( bgamma12(Ngrids) )
      END IF
      IF (.not.allocated(bgamma5)) THEN
        allocate ( bgamma5(Ngrids) )
      END IF
      IF (.not.allocated(bgamma7)) THEN
        allocate ( bgamma7(Ngrids) )
      END IF
      IF (.not.allocated(bgamma11)) THEN
        allocate ( bgamma11(Ngrids) )
      END IF
      IF (.not.allocated(bgamma13)) THEN
        allocate ( bgamma13(Ngrids) )
      END IF
      IF (.not.allocated(mtos1)) THEN
        allocate ( mtos1(Ngrids) )
      END IF
      IF (.not.allocated(mtos2)) THEN
        allocate ( mtos2(Ngrids) )
      END IF
      IF (.not.allocated(mtos3)) THEN
        allocate ( mtos3(Ngrids) )
      END IF
      IF (.not.allocated(flz1)) THEN
        allocate ( flz1(Ngrids) )
      END IF
      IF (.not.allocated(flz2)) THEN
        allocate ( flz2(Ngrids) )
      END IF
      IF (.not.allocated(lk1)) THEN
        allocate ( lk1(Ngrids) )
      END IF
      IF (.not.allocated(lk2)) THEN
        allocate ( lk2(Ngrids) )
      END IF
      IF (.not.allocated(lk3)) THEN
        allocate ( lk3(Ngrids) )
      END IF
      IF (.not.allocated(ratiol1)) THEN
        allocate ( ratiol1(Ngrids) )
      END IF
      IF (.not.allocated(ratiol2)) THEN
        allocate ( ratiol2(Ngrids) )
      END IF
      IF (.not.allocated(wsdn)) THEN
        allocate ( wsdn(Ngrids) )
      END IF
      IF (.not.allocated(wsdc)) THEN
        allocate ( wsdc(Ngrids) )
      END IF
      IF (.not.allocated(wsdsi)) THEN
        allocate ( wsdsi(Ngrids) )
      END IF
      IF (.not.allocated(wsdca)) THEN
        allocate ( wsdca(Ngrids) )
      END IF
      IF (.not.allocated(wsp1)) THEN
        allocate ( wsp1(Ngrids) )
      END IF
      IF (.not.allocated(wsp2)) THEN
        allocate ( wsp2(Ngrids) )
      END IF
      IF (.not.allocated(wsp3)) THEN
        allocate ( wsp3(Ngrids) )
      END IF
      IF (.not.allocated(pco2a)) THEN
        allocate ( pco2a(Ngrids) )
      END IF
      IF (.not.allocated(p2n)) THEN
        allocate ( p2n(Ngrids) )
      END IF
      IF (.not.allocated(o2no)) THEN
        allocate ( o2no(Ngrids) )
      END IF
      IF (.not.allocated(o2nh)) THEN
        allocate ( o2nh(Ngrids) )
      END IF
      IF (.not.allocated(cnb)) THEN
        allocate ( cnb(Ngrids) )
      END IF
      IF (.not.allocated(apsilon)) THEN
        allocate ( apsilon(Ngrids) )
      END IF
      IF (.not.allocated(ro5)) THEN
        allocate ( ro5(Ngrids) )
      END IF
      IF (.not.allocated(ro6)) THEN
        allocate ( ro6(Ngrids) )
      END IF
      IF (.not.allocated(ro7)) THEN
        allocate ( ro7(Ngrids) )
      END IF
      IF (.not.allocated(ro10)) THEN
        allocate ( ro10(Ngrids) )
      END IF
      IF (.not.allocated(rop)) THEN
        allocate ( rop(Ngrids) )
      END IF
      IF (.not.allocated(rob)) THEN
        allocate ( rob(Ngrids) )
      END IF
      IF (.not.allocated(kabac)) THEN
        allocate ( kabac(Ngrids) )
      END IF
      IF (.not.allocated(klbac)) THEN
        allocate ( klbac(Ngrids) )
      END IF
      IF (.not.allocated(ksdoc)) THEN
        allocate ( ksdoc(Ngrids) )
      END IF
      IF (.not.allocated(ksdon)) THEN
        allocate ( ksdon(Ngrids) )
      END IF
      IF (.not.allocated(ratiob)) THEN
        allocate ( ratiob(Ngrids) )
      END IF
      IF (.not.allocated(ratiobc)) THEN
        allocate ( ratiobc(Ngrids) )
      END IF
      IF (.not.allocated(RtUVLDOC)) THEN
        allocate ( RtUVLDOC(Ngrids) )
      END IF
      IF (.not.allocated(RtUVSDOC)) THEN
        allocate ( RtUVSDOC(Ngrids) )
      END IF
      IF (.not.allocated(RtUVLDIC)) THEN
        allocate ( RtUVLDIC(Ngrids) )
      END IF
      IF (.not.allocated(RtUVSDIC)) THEN
        allocate ( RtUVSDIC(Ngrids) )
      END IF
      IF (.not.allocated(colorFR1)) THEN
        allocate ( colorFR1(Ngrids) )
      END IF
      IF (.not.allocated(colorFR2)) THEN
        allocate ( colorFR2(Ngrids) )
      END IF
!
!-----------------------------------------------------------------------
!  Initialize tracer identification indices.
!-----------------------------------------------------------------------
!
!  Allocate biological tracer vector.
!
      IF (.not.allocated(idbio)) THEN
        allocate ( idbio(NBT) )
      END IF
      
#if defined DIAGNOSTICS && defined DIAGNOSTICS_BIO
!     
!  Allocate biological diagnostics vectors
!       
      IF (.not.allocated(iDbio2)) THEN
        allocate ( iDbio2(NDbio2d) )
      END IF
      IF (.not.allocated(iDbio3)) THEN
        allocate ( iDbio3(NDbio3d) )
      END IF
#endif

      
!
!  Set identification indices.
!
      ic=NAT+NPT+NCS+NNS
      DO i=1,NBT
        idbio(i)=ic+i
      END DO
      iNO3_=ic+1
      iNH4_=ic+2
      iSiOH=ic+3
      iPO4_=ic+4
      iS1_N=ic+5
      iS1_C=ic+6
      iS1CH=ic+7
      iS2_N=ic+8              
      iS2_C=ic+9
      iS2CH=ic+10
      iS3_N=ic+11
      iS3_C=ic+12
      iS3CH=ic+13
      iZ1_N=ic+14             
      iZ1_C=ic+15
      iZ2_N=ic+16             
      iZ2_C=ic+17
      iBAC_=ic+18
      iDD_N=ic+19             
      iDD_C=ic+20
      iDDSi=ic+21            
      iLDON=ic+22
      iLDOC=ic+23
      iSDON=ic+24
      iSDOC=ic+25
      iCLDC=ic+26
      iCSDC=ic+27
      iDDCA=ic+28
      ic=ic+28
# ifdef OXYGEN
      iOxyg=ic+1
      ic=ic+1
# endif
# ifdef CARBON
      iTIC_=ic+1
      iTAlk=ic+2
      ic=ic+2
# endif

      RETURN
      END SUBROUTINE initialize_biology
