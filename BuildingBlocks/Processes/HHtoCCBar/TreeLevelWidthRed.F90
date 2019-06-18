double precision function HHtoCCBarTree()
 use constants
 implicit none
#include "looptools.h"
 double precision :: totalAmplitude

 totalAmplitude = (1.5D0*EL2*MC2*(-4.D0*MC2 + MHH2)*SA2)/(MW2*SB2*SW2)

 HHtoCCBarTree = totalAmplitude
end function HHtoCCBarTree