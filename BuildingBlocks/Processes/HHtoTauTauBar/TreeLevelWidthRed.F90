double precision function HHtoTauTauBarTree()
 use constants
 implicit none
#include "looptools.h"
 double precision :: totalAmplitude

 totalAmplitude = (0.5D0*EL2*(MHH2 - 4.D0*ML2)*ML2*DBLE(Yuk5**INT(2.D0)))/(MW2*SW2)

 HHtoTauTauBarTree = totalAmplitude
end function HHtoTauTauBarTree