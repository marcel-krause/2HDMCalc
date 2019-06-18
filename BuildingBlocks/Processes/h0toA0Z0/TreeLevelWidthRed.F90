double precision function h0toA0Z0Tree()
 use constants
 implicit none
#include "looptools.h"
 double precision :: totalAmplitude

 totalAmplitude = (0.25D0*CBA2*EL2*(-2.D0*MA02*Mh02 - 2.D0*MA02*MZ2 - 2.D0*Mh02*MZ2 + DBLE(MA0**INT(4.D0)) + DBLE(Mh0**INT(4.D0))&
  & + DBLE(MZ**INT(4.D0))))/(CW2*MZ2*SW2)

 h0toA0Z0Tree = totalAmplitude
end function h0toA0Z0Tree