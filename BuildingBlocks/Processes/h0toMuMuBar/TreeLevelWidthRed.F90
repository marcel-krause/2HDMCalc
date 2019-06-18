double precision function h0toMuMuBarTree()
 use constants
 implicit none
#include "looptools.h"
 double precision :: totalAmplitude

 totalAmplitude = (0.5D0*EL2*(Mh02 - 4.D0*MM2)*MM2*DBLE(Yuk4**INT(2.D0)))/(MW2*SW2)

 h0toMuMuBarTree = totalAmplitude
end function h0toMuMuBarTree