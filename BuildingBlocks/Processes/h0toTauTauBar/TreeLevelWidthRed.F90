double precision function h0toTauTauBarTree()
 use constants
 implicit none
#include "looptools.h"
 double precision :: totalAmplitude

 totalAmplitude = (0.5D0*EL2*(Mh02 - 4.D0*ML2)*ML2*DBLE(Yuk4**INT(2.D0)))/(MW2*SW2)

 h0toTauTauBarTree = totalAmplitude
end function h0toTauTauBarTree