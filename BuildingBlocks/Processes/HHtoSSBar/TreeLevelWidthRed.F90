double precision function HHtoSSBarTree()
 use constants
 implicit none
#include "looptools.h"
 double precision :: totalAmplitude

 totalAmplitude = (1.5D0*EL2*(MHH2 - 4.D0*MS2)*MS2*DBLE(Yuk2**INT(2.D0)))/(MW2*SW2)

 HHtoSSBarTree = totalAmplitude
end function HHtoSSBarTree