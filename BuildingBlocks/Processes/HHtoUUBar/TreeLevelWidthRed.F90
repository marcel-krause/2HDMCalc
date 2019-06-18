double precision function HHtoUUBarTree()
 use constants
 implicit none
#include "looptools.h"
 double precision :: totalAmplitude

 totalAmplitude = (1.5D0*EL2*(MHH2 - 4.D0*MU2)*MU2*SA2)/(MW2*SB2*SW2)

 HHtoUUBarTree = totalAmplitude
end function HHtoUUBarTree