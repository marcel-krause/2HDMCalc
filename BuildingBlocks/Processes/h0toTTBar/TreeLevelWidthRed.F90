double precision function h0toTTBarTree()
 use constants
 implicit none
#include "looptools.h"
 double precision :: totalAmplitude

 totalAmplitude = (1.5D0*CA2*EL2*MT2*(Mh02 - 2.D0*MT2 - 2.D0*DBLE(MT**INT(2.D0))))/(MW2*SB2*SW2)

 h0toTTBarTree = totalAmplitude
end function h0toTTBarTree