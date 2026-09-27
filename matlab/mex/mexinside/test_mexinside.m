% Function TEST_MEXINSIDE
% Simple test of mex file mexinside 

% svn $Id: test_mexinside.m 361 2009-07-02 15:43:20Z arango $

x=rand(100,1);
y=rand(100,1);
xpoly=[.2 .2 .7 .2];
ypoly=[.2 .8 .5 .2];
inds = mexinside(x,y,xpoly,ypoly);
ind2 = find(inds);
ind3 = find(1-inds);
plot(x(ind2),y(ind2),'r*',x(ind3),y(ind3),'g*',xpoly,ypoly,'k-');

title('Test of Mexinside: Red stars should be inside triangle!');

