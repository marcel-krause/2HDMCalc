double precision function HHtoBBBarTree()
 use constants
 implicit none
#include "looptools.h"
 double precision :: totalAmplitude

 totalAmplitude = (1.5D0*EL2*MB2*(-4.D0*MB2 + MHH2)*DBLE(Yuk2**INT(2.D0)))/(MW2*SW2)

 HHtoBBBarTree = totalAmplitude
end function HHtoBBBarTree