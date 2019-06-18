double precision function h0toElElBarTree()
 use constants
 implicit none
#include "looptools.h"
 double precision :: totalAmplitude

 totalAmplitude = (0.5D0*EL2*ME2*(-4.D0*ME2 + Mh02)*DBLE(Yuk4**INT(2.D0)))/(MW2*SW2)

 h0toElElBarTree = totalAmplitude
end function h0toElElBarTree