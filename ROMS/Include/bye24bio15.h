/*
** svn $Id: bye24bio13.h 709 2014-02-03 14:45:51Z zhoufeng $
*******************************************************************************
** Copyright (c) 2002-2014 The ROMS/TOMS Group                               **
**   Licensed under a MIT/X style license                                    **
**   See License_ROMS.txt                                                    **
*******************************************************************************
**
** Options for: BohaiSea-YellowSea-EastChinaSea - 1/24 degree.
**
** Application flag:   BYE24BIO15
** Input script:       ocean_bye24bio15.in, bio_umaine15.in
*/
#undef USE_DEBUG

#define NESTING
#undef  ONE_WAY
#define  WET_DRY
#define LIMIT_BSTRESS

#define PERFECT_RESTART
/*      Momentum Equation        */
#define UV_ADV
#define UV_COR
#define UV_QDRAG
#define UV_VIS2
#define DJ_GRADPS

/*      Tracer Equation          */
#undef  TS_U3HADVECTION        
#undef  TS_C4VADVECTION       
#undef  TS_DIF2
#define TS_U3ADV_SPLIT
#define SALINITY
#define NONLIN_EOS
#define SOLAR_SOURCE
#define QCORRECTION
#define SRELAXATION         

/*      Model configuration      */
/*SPLINES option may affect the downward flux in the vertical diffusion/viscosty terms
*/
#define SOLVE3D
#define CURVGRID
#define MASKING
#define SPHERICAL            
#define AVERAGES
#define AVERAGES_AKV
#define AVERAGES_AKT
#define AVERAGES_AKS
#undef AVERAGES_DETIDE

/*      Tidal forcing            */
#define RAMP_TIDES
#define SSH_TIDES
#define UV_TIDES

/*      Horizontal mixing for Momentum and tracers    */
#define VISC_GRID        /*together with MIX_S_UV*/
#define MIX_S_UV         /*for steep topography ? */

#define DIFF_GRID       
#ifdef TS_U3ADV_SPLIT
#  define MIX_GEO_TS
#endif
#define MIX_S_TS         /*better for Changjiang*/


/*      Vertical mixing          */
#undef  LMD_MIXING       /* for 1st year */
#undef  MY25_MIXING
#define GLS_MIXING

#ifdef  LMD_MIXING
# define LMD_RIMIX
# define LMD_CONVEC
# define LMD_DDMIX 
# define LMD_SKPP
# define LMD_BKPP 
# define LMD_NONLOCAL
# define LMD_SHAPIRO
#endif

#if defined GLS_MIXING 
# define CANUTO_A
# define N2S2_HORAVG
#endif

#if defined MY25_MIXING
# define KANTHA_CLAYSON
# define N2S2_HORAVG
#endif


/*      Lateral boundary         */
#define RADIATION_2D
#define SPONGE

/*       Tide boundary       */
#ifdef SSH_TIDES
# define ADD_FSOBC
#endif

#ifdef UV_TIDES
# define ADD_M2OBC
#endif


/*      Climatology         */
#undef  TCLIMATOLOGY
#undef  TCLM_NUDGING
#undef  ZCLIMATOLOGY
#undef  ZCLM_NUDGING
#undef  M2CLIMATOLOGY
#undef  M2CLM_NUDGING
#undef  M3CLIMATOLOGY
#undef  M3CLM_NUDGING

#define ANA_BSFLUX
#define ANA_BTFLUX

/*
**  Biological model options.
*/
#define BIO_UMAINE15

#ifdef BIO_UMAINE15
# undef   DIAGNOSTICS_BIO
# define  CARBON
# define  TALK_NONCONSERV
# define OXYGEN
# define SINK_OP1
# define DENITRIFICATION
# define ANA_BPFLUX
# define ANA_SPFLUX
#endif

/* NetCDF input/output OPTIONS
 */

#define NO_LBC_ATT // use to not check NLM_LBC global attribute on restart
#undef DIURNAL_SRFLUX

/* diagnostics
 *  */

#undef DIAGNOSTICS

#ifdef DIAGNOSTICS
# define DIAGNOSTICS_UV
# define DIAGNOSTICS_TS
# define DIAGNOSTICS_BIO
#endif

/* Stations
 * */
#undef STATIONS
