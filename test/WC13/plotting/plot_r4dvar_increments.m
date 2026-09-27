%
% PLOT_R4DVAR_INCREMENTS:  Plot the R4D-Var increments
%

% svn $Id: plot_r4dvar_increments.m 831 2017-01-24 21:38:51Z arango $
%===========================================================================%
%  Copyright (c) 2002-2017 The ROMS/TOMS Group                              %
%    Licensed under a MIT/X style license                                   %
%    See License_ROMS.txt                                                   %
%===========================================================================%

clear                                  % clear workspace
close all                              % close all figures

PRINT=0;                               % Switch to save figures as PNG
SHADING_INTERP=1;                      % switch for shading interp (1) or
                                       %            shading flat (0)

Cp=3985.0;
rho0=1025.0;

% Set input NetCDF files.

Inpb='../R4DVAR/wc13_fwd_000.nc';      % prior circulation (NLM)
Inpa='../R4DVAR/wc13_fwd_001.nc';      % posterior circulation (RPM)
Inpr='../R4DVAR/wc13_irp.nc';          % RPM initial conditions

Grd='../Data/wc13_grd.nc';             % grid

% Load coastline coordinates.

coast=load('../Data/wc13_cst.mat','-mat');   % coastlines structure

% Read grid coordinates.

rlon=nc_read(Grd,'lon_rho');
rlat=nc_read(Grd,'lat_rho');

ulon=nc_read(Grd,'lon_u');
ulat=nc_read(Grd,'lat_u');

vlon=nc_read(Grd,'lon_v');
vlat=nc_read(Grd,'lat_v');

% Determine number of records and number of vertical levels in
% input NetCDF history files.

Nrec=length(nc_read(Inpb,'ocean_time'));
N   =length(nc_read(Inpb,'s_rho'));

klev=N;                   % processing surface level

% Read in prior circulation initial conditions (Rec=1) and
% replace land/sea mask values with NaNs.

Rec=1;
FillValue=NaN;

zetab  =nc_read(Inpb,'zeta'  ,Rec,FillValue);   % free-surface
ub     =nc_read(Inpb,'u'     ,Rec,FillValue);   % 3D u-momentum
vb     =nc_read(Inpb,'v'     ,Rec,FillValue);   % 3D v-momentum
tempb  =nc_read(Inpb,'temp'  ,Rec,FillValue);   % potential temperature
saltb  =nc_read(Inpb,'salt'  ,Rec,FillValue);   % salinity

% Read in posterior circulation initial conditions (Rec=1) and
% replace land/sea mask values with NaNs.

zetaa  =nc_read(Inpa,'zeta'  ,Rec,FillValue);   % free-surface
ua     =nc_read(Inpa,'u'     ,Rec,FillValue);   % 3D u-momentum
va     =nc_read(Inpa,'v'     ,Rec,FillValue);   % 3D v-momentum
tempa  =nc_read(Inpa,'temp'  ,Rec,FillValue);   % potential temperature
salta  =nc_read(Inpa,'salt'  ,Rec,FillValue);   % salinity

% Compute I4D-Var increments by substracting prior from
% posterior initial conditions.

diffz=zetaa-zetab;
diffu=ua-ub;
diffv=va-vb;
difft=tempa-tempb;
diffs=salta-saltb;

% Since the posterior is the RPM file, the increments cannot be
% computed by subtracting prior (NLM) from posterior (RPM) since
% there is an extra dimension (frc_adjust) in the tangent file (Inpa).
% We get the increments directly from the IRP file (Rec=2) and scale
% them back to kinematic fluxes.

Rec=2;                            % IRP rec to process
frc_adjust=1;                     % initial time adjusting record

difftx=nc_read(Inpr,'sustr' ,Rec,FillValue);
difftx=squeeze(difftx(:,:,frc_adjust)).*rho0;
diffty=nc_read(Inpr,'svstr' ,Rec,FillValue);
diffty=squeeze(diffty(:,:,frc_adjust)).*rho0;
diffqh=nc_read(Inpr,'shflux',Rec,FillValue);
diffqh=squeeze(diffqh(:,:,frc_adjust)).*(rho0*Cp);

% Plot R4D-Var state variables increments.  If 3D field, plot
% selected level (klev).

figure

subplot(3,2,1);
pcolor(rlon,rlat,diffz);
if (SHADING_INTERP), shading interp; else shading flat; end
colorbar; hold on;
plot(coast.lon,coast.lat,'k');
title('\zeta increment (m)');

subplot(3,2,2);
pcolor(ulon,ulat,squeeze(diffu(:,:,klev)));
if (SHADING_INTERP), shading interp; else shading flat; end
colorbar; hold on;
plot(coast.lon,coast.lat,'k');
title('u increment (m/s)');

subplot(3,2,3);
pcolor(vlon,vlat,squeeze(diffv(:,:,klev)));
if (SHADING_INTERP), shading interp; else shading flat; end
colorbar; hold on;
plot(coast.lon,coast.lat,'k');
title('v increment (m/s)');

subplot(3,2,4);
pcolor(rlon,rlat,squeeze(difft(:,:,klev)));
if (SHADING_INTERP), shading interp; else shading flat; end
colorbar; hold on;
plot(coast.lon,coast.lat,'k');
title('T increment (Celsius)');

subplot(3,2,5);
pcolor(rlon,rlat,squeeze(diffs(:,:,klev)));
if (SHADING_INTERP), shading interp; else shading flat; end
colorbar; hold on;
plot(coast.lon,coast.lat,'k');
title('S increment');

if (PRINT),
  print -dpng -r300 plot_r4dvar_increments_page1.png
end,

% Plot I4D-Var surface forcing variables increments.

figure

subplot(3,2,1);
pcolor(ulon,ulat,difftx);
if (SHADING_INTERP), shading interp; else shading flat; end
colorbar; hold on;
plot(coast.lon,coast.lat,'k');
title('\tau_x increment (Pa)');

subplot(3,2,2);
pcolor(vlon,vlat,diffty);
if (SHADING_INTERP), shading interp; else shading flat; end
colorbar; hold on;
plot(coast.lon,coast.lat,'k');
title('\tau_y increment (Pa)');

subplot(3,2,3);
pcolor(rlon,rlat,diffqh);
if (SHADING_INTERP), shading interp; else shading flat; end
colorbar; hold on;
plot(coast.lon,coast.lat,'k');
title('Q increment (W/m^2)');

if (PRINT),
  print -dpng -r300 plot_r4dvar_increments_page2.png
end,
