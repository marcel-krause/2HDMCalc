double precision function HHtoWmWpTree()
 use constants
 implicit none
#include "looptools.h"
 double precision :: totalAmplitude

 totalAmplitude = (CBA2*EL2*MW2*(3.D0 - (1.D0*MHH2)/MW2 + 0.25D0*DBLE(MHH**INT(4.D0))*DBLE(MW**INT(-4.D0))))/SW2

 HHtoWmWpTree = totalAmplitude
end function HHtoWmWpTree