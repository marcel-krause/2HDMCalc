double precision function HHtoElElBarTree()
 use constants
 implicit none
#include "looptools.h"
 double precision :: totalAmplitude

 totalAmplitude = (0.5D0*EL2*ME2*(-4.D0*ME2 + MHH2)*DBLE(Yuk5**INT(2.D0)))/(MW2*SW2)

 HHtoElElBarTree = totalAmplitude
end function HHtoElElBarTree