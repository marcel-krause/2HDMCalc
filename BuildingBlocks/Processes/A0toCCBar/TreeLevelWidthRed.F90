double precision function A0toCCBarTree()
 use constants
 implicit none
#include "looptools.h"
 double precision :: totalAmplitude

 totalAmplitude = (1.5D0*EL2*MA02*MC2)/(MW2*SW2*TB2)

 A0toCCBarTree = totalAmplitude
end function A0toCCBarTree