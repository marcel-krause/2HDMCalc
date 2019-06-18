double precision function HHtoMuMuBarTree()
 use constants
 implicit none
#include "looptools.h"
 double precision :: totalAmplitude

 totalAmplitude = (0.5D0*EL2*(MHH2 - 4.D0*MM2)*MM2*DBLE(Yuk5**INT(2.D0)))/(MW2*SW2)

 HHtoMuMuBarTree = totalAmplitude
end function HHtoMuMuBarTree