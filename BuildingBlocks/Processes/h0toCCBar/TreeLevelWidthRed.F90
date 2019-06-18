double precision function h0toCCBarTree()
 use constants
 implicit none
#include "looptools.h"
 double precision :: totalAmplitude

 totalAmplitude = (1.5D0*CA2*EL2*MC2*(-4.D0*MC2 + Mh02))/(MW2*SB2*SW2)

 h0toCCBarTree = totalAmplitude
end function h0toCCBarTree