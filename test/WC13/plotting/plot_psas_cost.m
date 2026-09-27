%
% PLOT_PSAS_COST:  Plot 4D-PSAS cost function
%

% svn $Id: plot_psas_cost.m 831 2017-01-24 21:38:51Z arango $
%===========================================================================%
%  Copyright (c) 2002-2017 The ROMS/TOMS Group                              %
%    Licensed under a MIT/X style license                                   %
%    See License_ROMS.txt                                                   %
%===========================================================================%

clear                                  % clear workspace
close all                              % close all figures

SINGLE_PLOT=true;                      % Plot all outer-loop together
PRINT=false;                           % switch to save figure as PNG

% Set input NetCDF files.

Inp='../PSAS/wc13_mod.nc';             % ROMS 4D-Var file

% Read in cost function variables.

Jo =nc_read(Inp,'Jobs',NaN);           % observation cost function
Jb =nc_read(Inp,'Jb',NaN);             % background cost function
J  =nc_read(Inp,'Jact',NaN);           % total cost function
JNL=nc_read(Inp,'NL_fDataPenalty');    % final NL model data penalty

%Jo(1,:)=NaN;
%Jb(1,:)=NaN;
%J (1,:)=NaN;

Jo=Jo./2;                              % Need to divide by 2
Jb=Jb./2;                              % because of definition
J=J./2;                                % of penalty functional
JNL=JNL./2;

[Niter,Nouter]=size(Jo);               % number of 4D-Var iterations

% Compute theoretical minimum cost function value (Nobs/2).  Find number
% of bounded 4D-Var observations by counting nonzero values of the
% screening flag (obs_scale).

obs_scale=nc_read(Inp,'obs_scale');
Nobs=length(nonzeros(obs_scale));

Jmin=Nobs/2;

% Plot cost functions.

if (SINGLE_PLOT),
  figure;
  plot(log10(squeeze(J (:))),'k','LineWidth',2);
  hold on;
  plot(log10(squeeze(Jo(:))),'b','LineWidth',2);
  plot(log10(squeeze(Jb(:))),'r','LineWidth',2);

  axis([1 Niter*Nouter 0 max(log10(J(:)))]);
  line([1 Niter*Nouter],[log10(Jmin) log10(Jmin)],                      ...
       'LineStyle','--','Color',[0 0 0],'LineWidth',2);

  plot(Niter*Nouter,log10(JNL(end,Nouter)+Jb(end,Nouter)),              ...
       'rx','LineWidth',2,'MarkerSize',10);

  legend('J','J_o','J_b','J_{min}','J_{NL}','Location','Southeast');
  title(['WC13: PSAS Cost Functions, outer loops = ', num2str(1:Nouter)]);
  ylabel('log_{10}(J)');
  xlabel('Iteration number');
  grid on;
  hold off;

  if (PRINT),
    print -dpng -r300 plot_psas_cost.png
  end

else
  for outer=1:Nouter
    figure;
    plot(log10(squeeze(J (:,outer))),'k','LineWidth',2);
    hold on;
    plot(log10(squeeze(Jo(:,outer))),'b','LineWidth',2);
    plot(log10(squeeze(Jb(:,outer))),'r','LineWidth',2);

    axis([1 Niter 0 max(log10(J(:)))]);
    line([1 Niter],[log10(Jmin) log10(Jmin)],                           ...
         'LineStyle','--','Color',[0 0 0],'LineWidth',2);

    plot(Niter,log10(JNL(end,outer)+Jb(end,outer)),                     ...
         'rx','LineWidth',2,'MarkerSize',10);

    legend('J','J_o','J_b','J_{min}','J_{NL}','Location','Southeast');
    title(['WC13: PSAS Cost Functions, outer loop = ', num2str(outer)]);
    ylabel('log_{10}(J)');
    xlabel('Iteration number');
    grid on;
    hold off;

    if (PRINT),
      print -dpng -r300 plot_psas_cost.png
    end
  end

end
