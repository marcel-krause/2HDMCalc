double precision function h0toUUBarTree()
 use constants
 implicit none
#include "looptools.h"
 double precision :: totalAmplitude

 totalAmplitude = (1.5D0*CA2*EL2*(Mh02 - 4.D0*MU2)*MU2)/(MW2*SB2*SW2)

 h0toUUBarTree = totalAmplitude
end function h0toUUBarTree