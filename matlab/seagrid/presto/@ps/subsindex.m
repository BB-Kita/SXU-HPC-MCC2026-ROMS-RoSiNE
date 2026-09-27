function theResult = subsindex(self)

% ps/subsindex -- Value of a "ps" as an index.
%  subsindex(self) posts an error, since "ps"
%   objects may not be used as indices.
 
% svn $Id: subsindex.m 361 2009-07-02 15:43:20Z arango $
%=======================================================================
% Copyright (C) 1999 Dr. Charles R. Denham, ZYDECO.
%  All Rights Reserved.
%   Disclosure without explicit written consent from the
%    copyright owner does not constitute publication.
%=======================================================================
 
% Version of 09-Dec-1999 02:51:19.
% Updated    09-Dec-1999 02:51:19.

error(' ## "ps" objects may not be used as indices.')
