double precision function HHtoZ0Z0Tree()
 use constants
 implicit none
#include "looptools.h"
 double precision :: totalAmplitude

 totalAmplitude = (CBA2*EL2*MW2*DBLE(CW**INT(-4.D0))*(3.D0 - (1.D0*MHH2)/MZ2 + 0.25D0*DBLE(MHH**INT(4.D0))*DBLE(MZ**INT(-4.D0))))&
  &/SW2

 HHtoZ0Z0Tree = totalAmplitude
end function HHtoZ0Z0Tree