%
% PLOT_R4DVAR_COST:  Plot R4D-Var cost function
%

% svn $Id: plot_r4dvar_cost.m 831 2017-01-24 21:38:51Z arango $
%===========================================================================%
%  Copyright (c) 2002-2017 The ROMS/TOMS Group                              %
%    Licensed under a MIT/X style license                                   %
%    See License_ROMS.txt                                                   %
%===========================================================================%

clear                                  % clear workspace
close all                              % close all figures

PRINT=0;                               % switch to save figure as PNG

% Set input NetCDF files.

Inp='../R4DVAR/wc13_mod.nc';           % ROMS 4D-Var file

% Read in cost function variables.

Jo=nc_read(Inp,'Jobs');                % observation cost function
Jb=nc_read(Inp,'Jb');                  % background cost function
J =nc_read(Inp,'Jact');                % total cost function

Jo=Jo./2;                              % Need to divide by 2
Jb=Jb./2;                              % because of definition
J=J./2;                                % of penalty functional

Niter=length(Jo);                      % number of 4D-Var iterations

% Compute theoretical minimum cost function value (Nobs/2).  Find number
% of bounded 4D-Var observations by counting nonzero values of the
% screening flag (obs_scale).

obs_scale=nc_read(Inp,'obs_scale');
Nobs=length(nonzeros(obs_scale));

Jmin=Nobs/2;

% Plot cost functions.

plot(log10(J),'k','LineWidth',2);
hold on;
plot(log10(Jo),'b','LineWidth',2);
plot(log10(Jb),'r','LineWidth',2);
axis([1 Niter 0 max(log10(J))]);

line([1 Niter],[log10(Jmin) log10(Jmin)], ...
     'LineStyle','--','Color',[0 0 0],'LineWidth',2);

legend('J','J_o','J_b','J_{min}','Location','Southeast');

title('R4D-Var Cost Function');
ylabel('log_{10}(J)');
xlabel('Iteration number');

if (PRINT),
  print -dpng -r300 plot_r4dvar_cost.png
end,
