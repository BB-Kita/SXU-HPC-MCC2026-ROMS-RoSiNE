function help_help

%    Help for SeaGrid is available in the "Help" menu, where
% various topics can be selected.
%    The "Help/Help" menu item invokes a dialog box that lists
% all the available help.
 
% svn $Id: help_help.m 328 2009-03-09 01:41:19Z arango $
%=======================================================================
% Copyright (C) 1999 Dr. Charles R. Denham, ZYDECO.
%  All Rights Reserved.
%   Disclosure without explicit written consent from the
%    copyright owner does not constitute publication.
%=======================================================================
 
% Version of 23-Dec-1999 09:14:39.
% Updated    23-Dec-1999 09:14:39.

seagrid_helpdlg(help(mfilename), 'SeGrid Help')
