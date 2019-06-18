double precision function h0toWmWpTree()
 use constants
 implicit none
#include "looptools.h"
 double precision :: totalAmplitude

 totalAmplitude = (EL2*MW2*SBA2*(3.D0 - (1.D0*Mh02)/MW2 + 0.25D0*DBLE(Mh0**INT(4.D0))*DBLE(MW**INT(-4.D0))))/SW2

 h0toWmWpTree = totalAmplitude
end function h0toWmWpTree