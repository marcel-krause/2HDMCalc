double precision function h0toHHHHTree()
 use constants
 implicit none
#include "looptools.h"
 double precision :: totalAmplitude

 totalAmplitude = (0.25D0*EL2*SBA2*DBLE(((Mh02 + 2.D0*MHH2)*S2A - (2.D0*Lambda5*MW2*(3.D0*S2A + S2B)*SW2)/EL2)**INT(2.D0)))/(MW2*&
  &S2B2*SW2)

 h0toHHHHTree = totalAmplitude
end function h0toHHHHTree