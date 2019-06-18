double precision function h0toBBBarTree()
 use constants
 implicit none
#include "looptools.h"
 double precision :: totalAmplitude

 totalAmplitude = (1.5D0*EL2*MB2*(-4.D0*MB2 + Mh02)*DBLE(Yuk1**INT(2.D0)))/(MW2*SW2)

 h0toBBBarTree = totalAmplitude
end function h0toBBBarTree