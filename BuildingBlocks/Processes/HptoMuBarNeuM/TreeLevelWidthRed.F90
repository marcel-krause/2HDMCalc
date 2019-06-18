double precision function HptoMuBarNeuMTree()
 use constants
 implicit none
#include "looptools.h"
 double precision :: totalAmplitude

 totalAmplitude = (0.5D0*EL2*(MHp2 - 1.D0*MM2)*MM2*DBLE(Yuk6**INT(2.D0)))/(MW2*SW2)

 HptoMuBarNeuMTree = totalAmplitude
end function HptoMuBarNeuMTree