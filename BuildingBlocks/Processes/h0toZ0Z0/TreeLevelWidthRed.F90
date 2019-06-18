double precision function h0toZ0Z0Tree()
 use constants
 implicit none
#include "looptools.h"
 double precision :: totalAmplitude

 totalAmplitude = (EL2*MW2*SBA2*DBLE(CW**INT(-4.D0))*(3.D0 - (1.D0*Mh02)/MZ2 + 0.25D0*DBLE(Mh0**INT(4.D0))*DBLE(MZ**INT(-4.D0))))&
  &/SW2

 h0toZ0Z0Tree = totalAmplitude
end function h0toZ0Z0Tree