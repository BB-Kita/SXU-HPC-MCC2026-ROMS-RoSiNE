function Busy(theFigure)

% Busy -- Set the watch-cursor.
%  Busy(theFigure) sets the watch-cursor in theFigure.
%   The companion routine is "Idle".
 
% svn $Id: busy.m 328 2009-03-09 01:41:19Z arango $
%=======================================================================
% Copyright (C) 1996 Dr. Charles R. Denham, ZYDECO.
% All Rights Reserved.
%   Disclosure without explicit written consent from the
%    copyright owner does not constitute publication.
%=======================================================================

if ~any(findobj('Type', 'figure')), return, end

if nargin < 1
   theFigure = gcf;
end

set(theFigure, 'Pointer', 'watch');
