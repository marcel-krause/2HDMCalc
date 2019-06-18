double precision function h0toSSBarTree()
 use constants
 implicit none
#include "looptools.h"
 double precision :: totalAmplitude

 totalAmplitude = (1.5D0*EL2*(Mh02 - 4.D0*MS2)*MS2*DBLE(Yuk1**INT(2.D0)))/(MW2*SW2)

 h0toSSBarTree = totalAmplitude
end function h0toSSBarTree