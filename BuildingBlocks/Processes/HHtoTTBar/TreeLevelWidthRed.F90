double precision function HHtoTTBarTree()
 use constants
 implicit none
#include "looptools.h"
 double precision :: totalAmplitude

 totalAmplitude = (1.5D0*EL2*MT2*SA2*(MHH2 - 4.D0*MT2))/(MW2*SB2*SW2)

 HHtoTTBarTree = totalAmplitude
end function HHtoTTBarTree