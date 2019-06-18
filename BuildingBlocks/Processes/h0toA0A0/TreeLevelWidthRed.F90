double precision function h0toA0A0Tree()
 use constants
 implicit none
#include "looptools.h"
 double precision :: totalAmplitude

 totalAmplitude = (0.25D0*EL2*DBLE(((2.D0*MA02 - 1.D0*Mh02)*SBA + (CAB*(2.D0*Mh02 - (4.D0*Lambda5*MW2*SW2)/EL2))/S2B)**INT(2.D0))&
  &)/(MW2*SW2)

 h0toA0A0Tree = totalAmplitude
end function h0toA0A0Tree