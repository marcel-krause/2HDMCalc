double precision function A0toUUBarTree()
 use constants
 implicit none
#include "looptools.h"
 double precision :: totalAmplitude

 totalAmplitude = (1.5D0*EL2*MA02*MU2)/(MW2*SW2*TB2)

 A0toUUBarTree = totalAmplitude
end function A0toUUBarTree