double precision function h0toHmHpTree()
 use constants
 implicit none
#include "looptools.h"
 double precision :: totalAmplitude

 totalAmplitude = (0.25D0*EL2*DBLE(((Mh02 - 2.D0*MHp2)*SBA - (1.D0*CAB*(2.D0*Mh02 - (4.D0*Lambda5*MW2*SW2)/EL2))/S2B)**INT(2.D0))&
  &)/(MW2*SW2)

 h0toHmHpTree = totalAmplitude
end function h0toHmHpTree