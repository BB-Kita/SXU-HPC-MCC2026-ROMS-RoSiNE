!
!       $Id: vvtext.F 833 2017-01-24 22:20:16Z arango $
!                                                                      
!                Copyright (C)  2000
!        University Corporation for Atmospheric Research
!                All Rights Reserved
!
! This file is free software; you can redistribute it and/or modify
! it under the terms of the GNU General Public License as published
! by the Free Software Foundation; either version 2 of the License, or
! (at your option) any later version.
!
! This software is distributed in the hope that it will be useful, but
! WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
! General Public License for more details.
!
! You should have received a copy of the GNU General Public License
! along with this software; if not, write to the Free Software
! Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
! USA.
!
! This module contains four subroutines for text support of the
! VELVCT utility. 
! The first, VVARTX, is a relatively high level routine
! that outputs a horizontally aligned vector arrow, with a numerical
! string presumably specifying the vector magnitude above and an
! informational label below. It is designed to allow a message
! showing the maximum and/or minimum vector contained the plot
! to be displayed. If the vectors are colored by vector magnitude
! then the vector is drawn using its designated color.
!
! The other modules are lowel level support routines:
!
! VVTXLN determines the first and last non-blank characters of an
!        arbitrary text string
!
! VVTXIQ returns the height and width of piece of text intended for
!        output using the PLOTCHAR routine PLCHHQ. 
!
! VVTXPO allows for 9 way justification of a text string using a
!        position mode parameter compatible with the CONPACK text 
!        positioning method. Given the position mode, position,
!        width and height of the text in the fractional system,
!        a position in user coordinates is returned for use with
!        the PLCHHQ routine assuming the CNTR parameter is set to
!        0.0.
! 
! Note that these routines as yet have no support for non-horizontal
! text. 
!
! --------------------------------------------------------------------
!
      SUBROUTINE VVARTX(CTX,IPO,FWX,FWY,TSZ,ITC,VMG,ASZ)
!
      CHARACTER*(*) CTX
!
! Writes an informational label consisting of a horizontal vector
! with the size over it, and a text string describing the
! vector underneath
!
! Input parameters:
!
! CTX - the character string
! IPO - the positioning mode for the informational box as a whole
! FWX,FWY - the reference position as a fraction of the viewport
! TSZ - the text size as a fraction of the viewport
! ITC - the text color
! VMG - the vector magnitude 
! ASZ - the arrow size in the fractional system
!
! ---------------------------------------------------------------------
!
! NOTE:
! Since implicit typing is used for all real and integer variables
! a consistent length convention has been adopted to help clarify the
! significance of the variables encountered in the code for this 
! utility. All local variable and subroutine parameter identifiers 
! are limited to 1,2,or 3 characters. Four character names identify  
! members of common blocks. Five and 6 character variable names 
! denote PARAMETER constants or subroutine or function names.
!
! Declare the VV common blocks.
!
! IPLVLS - Maximum number of color threshold level values
! IPAGMX - Maximum number of area groups allowed in the area map
!
      PARAMETER (IPLVLS = 256, IPAGMX = 64)
!
! Integer and real common block variables
!
!
      COMMON /VVCOM/                                                    &
     &                IUD1       ,IVD1       ,IPD1       ,IXDM       ,  &
     &                IYDN       ,VLOM       ,VHIM       ,ISET       ,  &
     &                VRMG       ,VRLN       ,VFRC       ,IXIN       ,  &
     &                IYIN       ,ISVF       ,UUSV       ,UVSV       ,  &
     &                UPSV       ,IMSK       ,ICPM       ,UVPS       ,  &
     &                UVPL       ,UVPR       ,UVPB       ,UVPT       ,  &
     &                UWDL       ,UWDR       ,UWDB       ,UWDT       ,  &
     &                UXC1       ,UXCM       ,UYC1       ,UYCN       ,  &
     &                NLVL       ,IPAI       ,ICTV       ,WDLV       ,  &
     &                UVMN       ,UVMX       ,PMIN       ,PMAX       ,  &
     &                RVMN       ,RVMX       ,RDMN       ,RDMX       ,  &
     &                ISPC       ,RVMD       ,IPLR       ,IVST       ,  &
     &                IVPO       ,ILBL       ,IDPF       ,IMSG       ,  &
     &                ICLR(IPLVLS)           ,TVLU(IPLVLS)
!
! Arrow size/shape parameters
!
       COMMON / VVARO /                                                 &
     &                HDSZ       ,HINF       ,HANG       ,IAST       ,  &
     &                HSIN       ,HCOS       ,FAMN       ,FAMX       ,  &
     &                UVMG       ,FAIR       ,FAWR       ,FAWF       ,  &
     &                FAXR       ,FAXF       ,FAYR       ,FAYF       ,  &
     &                AROX(8)    ,AROY(8)    ,FXSZ       ,FYSZ       ,  &
     &                FXRF       ,FXMN       ,FYRF       ,FYMN       ,  &
     &                FWRF       ,FWMN       ,FIRF       ,FIMN       ,  &
     &                AXMN       ,AXMX       ,AYMN       ,AYMX       ,  &
     &                IACM       ,IAFO       ,WBAD       ,WBTF       ,  &
     &                WBCF       ,WBDF       ,WBSC
!
!
! Text related parameters
!
       COMMON /VVTXP /                                                  &
     &                FCWM    ,ICSZ    ,                                &
     &                FMNS    ,FMNX    ,FMNY    ,IMNP    ,IMNC  ,       &
     &                FMXS    ,FMXX    ,FMXY    ,IMXP    ,IMXC  ,       &
     &                FZFS    ,FZFX    ,FZFY    ,IZFP    ,IZFC  ,       &
     &                FILS    ,FILX    ,FILY    ,IILP    ,IILC  ,       &
     &                FLBS    ,ILBC

!
! Character variable declartions
!
      CHARACTER*160 CSTR
      PARAMETER (IPCHSZ=36)
      CHARACTER*(IPCHSZ)  CMNT,CMXT,CZFT,CLBT,CILT
!
! Text string parameters
!
      COMMON /VVCHAR/ CSTR,CMNT,CMXT,CZFT,CLBT,CILT
!
      SAVE /VVCOM/, /VVARO/, /VVTXP/, /VVCHAR/
!
! The mapping common block: made available to user mapping routines
!
      COMMON /VVMAP/                                                    &
     &                IMAP       ,                                      &
     &                XVPL       ,XVPR       ,YVPB       ,YVPT       ,  &
     &                WXMN       ,WXMX       ,WYMN       ,WYMX       ,  &
     &                XLOV       ,XHIV       ,YLOV       ,YHIV       ,  &
     &                SXDC       ,SYDC       ,NXCT       ,NYCT       ,  &
     &                RLEN       ,LNLG       ,INVX       ,INVY       ,  &
     &                ITRT       ,IWCT       ,FW2W       ,FH2H       ,  &
     &                DVMN       ,DVMX       ,RBIG       ,IBIG
!
      SAVE /VVMAP/
!
! Math constants
!
      PARAMETER (PDTOR  = 0.017453292519943,                            &
     &           PRTOD  = 57.2957795130823,                             &
     &           P1XPI  = 3.14159265358979,                             &
     &           P2XPI  = 6.28318530717959,                             &
     &           P1D2PI = 1.57079632679489,                             &
     &           P5D2PI = 7.85398163397448) 
!
! --------------------------------------------------------------------
!
      CHARACTER*10 LBL
      EXTERNAL VVDUMB
!
! Space height factors -- if the vector is large in comparison with
! the text the space is calculated as a fraction (PSPFC1) of the 
! text size.
! Otherwise, (and preferably, because the minimum and maximum labels
! will align properly), the space height is calculated as half the
! remainder when the vector height is subtracted from a multiple
! (PSPFC2) of the text height.
!
      PARAMETER (PSPFC1=0.5,PSPFC2=2.0)
      REAL IAR(4)
      DATA IDM / 0 /
!
! --------------------------------------------------------------------
!
! Turn off clipping because this label is likely to be outside
! the current viewport
!
      CALL GQCLIP(IER,ICL,IAR)
      CALL GSCLIP(0)
!
! Save and reset the SET transformation to an identity (for safety)
!     
      CALL GETSET(VPL,VPR,VPB,VPT,WDL,WDR,WDB,WDT,ILG)
      CALL SET(0.0,1.0,0.0,1.0,0.0,1.0,0.0,1.0,1)
!
! Convert fraction of viewport values into fractional system
! coordinates
!      
      XF = XVPL + FWX * FW2W
      YF = YVPB + FWY * FH2H
!
! Write out the label and compute its height and width
!
!HGA  WRITE(LBL,'(E10.3)')VMG
      WRITE(LBL,'(1PE10.3)')VMG
      CALL VVTXLN(LBL,10,LB,LE)
      CALL VVTXIQ(LBL(LB:LE),TSZ*FW2W,WL,HL)
!
! We know the width of the arrow, compute its height
! If less than or equal to 0.0 it will not be drawn unless using
! wind barbs
!
! Determine the height of the arrow by calling the drawing routine
! using a negative value of the vector len
!
      WA = ASZ
      UVMG = VMG
      IF (WA .GT. 0.0) THEN
         IF (IAST .EQ. 0) THEN
            CALL VVDRAW(0.5,0.5,0.5+WA,0.5,-WA,LBL,0,IDM,VVDUMB,0)
         ELSE IF (IAST .EQ. 1) THEN
            CALL VVDRFL(0.5,0.5,0.5+WA,0.5,-WA,LBL,0,IDM,VVDUMB,0)
         ELSE
            CALL VVDRWB(0.5,0.5,0.5+WA,0.5,-WA,LBL,0,IDM,VVDUMB,0)
         END IF
      END IF
      IF (WA .LE. 0.0) THEN
         HA = 0.0
         YAJ = 0.0
      ELSE
         HA = FYSZ
         YAJ = ((AYMX - 0.5) - (0.5 - AYMN)) / 2.0
      END IF
!
! Now get the size of the vector text string
!
      CALL VVTXLN(CTX,IPCHSZ,IB,IE)
      CALL VVTXIQ(CTX(IB:IE),TSZ*FW2W,WT,HT)
!
! Compute a space height based on the text height and arrow height
!
      HS = MAX(PSPFC1 * HT, (PSPFC2*HT - HA) / 2.0) 
!
! Compute the total size of the info label
! Two space heights are added
!
      WTT = MAX(WT,WA,WL)
      HTT = HL + HA + 2*HS + HT
!
! Adjust the input position so that we can effectively
! treat it as lower left.
!
      IF (IPO.LE.-4) THEN
         XFA=XF
         YFA=YF
      ELSE IF (IPO.EQ.-3) THEN
         XFA=XF-0.5*WTT
         YFA=YF
      ELSE IF (IPO.EQ.-2) THEN
         XFA=XF-WTT
         YFA=YF
      ELSE IF (IPO.EQ.-1) THEN
         XFA=XF
         YFA=YF-0.5*HTT
      ELSE IF (IPO.EQ.0) THEN
         XFA=XF-0.5*WTT
         YFA=YF-0.5*HTT
      ELSE IF (IPO.EQ.1) THEN
         XFA=XF-WTT
         YFA=YF-0.5*HTT
      ELSE IF (IPO.EQ.2) THEN
         XFA=XF
         YFA=YF-HTT
      ELSE IF (IPO.EQ.3) THEN
         XFA=XF-0.5*WTT
         YFA=YF-HTT
      ELSE IF (IPO.GE.4) THEN
         XFA=XF-WTT
         YFA=YF-HTT
      END IF
!
! Position each piece of output and draw it
!
      CXT = XFA + 0.5*WTT
      CYT = YFA + 0.5*HT
      CXA = CXT
      CYA = YFA + HT + HS + 0.5*HA - YAJ
      CXL = CXT
      CYL = YFA + HT + 2.0*HS + HA + 0.5*HL
!
! Save the current polyline and text colors
!
      CALL GQTXCI(IER,IOT)
      CALL GQPLCI(IER,IOC)
      CALL GQFAIS(IER,IOF)
      CALL GQFACI(IER,IOK)
      IF (IAST .EQ. 2) THEN
         CALL GSFACI(IOC)
      END IF
!
! If the vectors are colored by magnitude then find the correct color
! Set the text color too if required.
!
      IF (ABS(ICTV).EQ.1) THEN
         DO 100 K=1,NLVL,1
            IF (VMG .LE. TVLU(K) .OR. K.EQ.NLVL) THEN
               IF (IAST .EQ. 0) THEN
                  CALL GSPLCI(ICLR(K))
               ELSE IF (IAST .EQ. 1) THEN
                  IF (IACM .EQ. -1 .OR. IACM .GE. 1) THEN
                     CALL GSPLCI(ICLR(K))
                  END IF
                  IF (IACM .EQ. 0 .OR. ABS(IACM) .GE. 2) THEN
                     CALL GSFACI(ICLR(K))
                  END IF
               ELSE
                     CALL GSPLCI(ICLR(K))
                     CALL GSFACI(ICLR(K))
               END IF
!
               IF (ITC .EQ. -2) CALL GSTXCI(ICLR(K))
!
               GO TO 101
            END IF
 100     CONTINUE
!     
 101     CONTINUE
      END IF
!
! Adjust the colors depending on the text block setting
!
      IF (ITC .GE. 0) THEN
         CALL GSTXCI(ITC)
         CALL GSPLCI(ITC)
         IF (IAST.NE.0) CALL GSFACI(ITC)
      ELSE IF (ITC .LT. -2) THEN
         CALL  GSPLCI(IOT)
         IF (IAST.NE.0) CALL GSFACI(IOT)
      END IF
!
! Temporarily reset the vector positioning flag to ensure a 
! centered arrow; also set linewidth
!
      ISP=IVPO
      IVPO=0
      CALL GQLWSC(IER,ROW)
      CALL GSLWSC(WDLV)
      IF (WA .GT. 0.0) THEN
         IF (IAST .EQ. 0) THEN
            CALL VVDRAW(CXA,CYA,CXA+WA,CYA,WA,LBL,0,IDM,VVDUMB,0)
         ELSE IF (IAST .EQ. 1) THEN
            CALL VVDRFL(CXA,CYA,CXA+WA,CYA,WA,LBL,0,IDM,VVDUMB,0)
         ELSE
            CALL VVDRWB(CXA,CYA,CXA+WA,CYA,WA,LBL,0,IDM,VVDUMB,0)
         END IF
      END IF
!
! Restore linewidth and centering flag
!
      CALL GSLWSC(ROW)
      IVPO=ISP
      IF (ITC.EQ.-1) CALL GSPLCI(IOT)
!
      XW=CFUX(CXT)
      YW=CFUY(CYT)
      CALL PLCHHQ(XW,YW,CTX(IB:IE),TSZ*FW2W,0.0,0.0)
      CALL PLCHHQ(CFUX(CXL),CFUY(CYL),LBL(LB:LE),TSZ*FW2W,0.0,0.0)
!
      CALL GSTXCI(IOT)
      CALL GSPLCI(IOC)
      CALL GSFACI(IOK)
      CALL GSFAIS(IOF)
!
! Restore clipping and the set transformation.
!
      CALL GSCLIP(ICL)
      CALL SET(VPL,VPR,VPB,VPT,WDL,WDR,WDB,WDT,ILG)
!
! Done
!
      RETURN
      END
!
! --------------------------------------------------------------------
!
      SUBROUTINE VVTXLN(CTX,MXL,IB,IE)
!
      CHARACTER*(*) CTX
!
! Input parameters:
!
! CTX - the text string
! MXL - the maximum length of the text string
!
! Output parameters:
!
! IB,IE - integer position of the first and last non-blank
!         characters in the text string
!
! ------------------------------------------------------------------
!
! Given a text string, finds the first and last non-blank characters
!
      DO 10 I=1,MXL,1
         IF (CTX(I:I) .NE. ' ') GO TO 11
 10   CONTINUE
 11   CONTINUE
      IB=I
!
      DO 20 I=MXL,1,-1
         IF (CTX(I:I) .NE. ' ') GO TO 21
 20   CONTINUE
 21   CONTINUE
      IE=I
!
! Done
!
      RETURN
      END
!
! --------------------------------------------------------------------
!
      SUBROUTINE VVTXIQ(CTX,SIZ,W,H)
!
      CHARACTER*(*) CTX
!
! Find the height and width of a piece of text in fractional system.
!
! Input parameters:
!
! CTX - Text string
! SIZ - Text size in fractional coordinates
!
! Output parameters:
!
! H,W - height and width of text
!
! Local variables:
!
! VPL,VPR,VPB,VPT,WDL,WDR,WDB,WDT,ILG  - saved coordinate system state
! ITE                                  - saved text extent
! DL,DR,DB,DT                          - returned text metrics
!
! --------------------------------------------------------------------
!
! Temporarily reset the window so the window coordinates
! are guaranteed to be uniform
!
      CALL GETSET(VPL,VPR,VPB,VPT,WDL,WDR,WDB,WDT,ILG)
      CALL SET(0.0,1.0,0.0,1.0,0.0,1.0,0.0,1.0,1)
!
! Save then set the text extent inquiry PLOTCHAR parameter
!
      CALL PCGETI('TE - Text extent computation', ITE)
      CALL PCSETI('TE - Text extent computation', 1)
!
! Call PLCHHQ in inquiry mode
!
      CALL PLCHHQ(0.5,0.5,CTX,SIZ,360.0,0.0)
!
! Get the results
!
      CALL PCGETR('DL - left distance', DL)
      CALL PCGETR('DR - right distance', DR)
      CALL PCGETR('DB - bottom distance', DB)
      CALL PCGETR('DT - top distance', DT)
!
      W = DR+DL
      H = DT+DB
!
! Restore state
!
      CALL SET(VPL,VPR,VPB,VPT,WDL,WDR,WDB,WDT,ILG)
      CALL PCSETI('TE - Text extent computation', ITE)
!
! Done
!
      RETURN
      END
!
! --------------------------------------------------------------------
!
      SUBROUTINE VVTXPO(IPO,FXI,FYI,W,H,XW,YW)
!
! Given the a position mode, location, and width and height of a
! horizontally aligned text box in the fractional coordinate system 
! returns the center of the text box in window coordinates.
! Angles are not yet supported.
!
! Input parameters:
!
! IPO      - text positioning method a la CONPACK
! FXI, FYI - input position in the fractional system
! H,W      - height and width of text in the fractional system
!
! Output parameters:
!
! XW,YW  - position in window coordinates to use for a call 
!          to PLCHHQ, assuming CNTR is set to 0.0
!
! --------------------------------------------------------------------
!
! Calculate an adjusted position based on the positioning mode
!
      IF (IPO .LE. -4) THEN
         FXA=FXI+0.5*W
         FYA=FYI+0.5*H
      ELSE IF (IPO .EQ. -3) THEN
         FXA=FXI
         FYA=FYI+0.5*H
      ELSE IF (IPO .EQ. -2) THEN
         FXA=FXI-0.5*W
         FYA=FYI+0.5*H
      ELSE IF (IPO .EQ. -1) THEN
         FXA=FXI+0.5*W
         FYA=FYI
      ELSE IF (IPO .EQ. 0) THEN
         FXA=FXI
         FYA=FYI
      ELSE IF (IPO .EQ. 1) THEN
         FXA=FXI-0.5*W
         FYA=FYI
      ELSE IF (IPO .EQ. 2) THEN
         FXA=FXI+0.5*W
         FYA=FYI-0.5*H
      ELSE IF (IPO .EQ. 3) THEN
         FXA=FXI
         FYA=FYI-0.5*H
      ELSE IF (IPO .GE. 4) THEN
         FXA=FXI-0.5*W
         FYA=FYI-0.5*H
      END IF
!
      XW=CFUX(FXA)
      YW=CFUY(FYA)
!
! Done
!
      RETURN
      END
!
      SUBROUTINE VVDUMB()
      RETURN
      END
