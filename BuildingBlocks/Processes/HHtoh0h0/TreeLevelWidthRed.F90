double precision function HHtoh0h0Tree()
 use constants
 implicit none
#include "looptools.h"
 double precision :: totalAmplitude

 totalAmplitude = (0.25D0*CBA2*DBLE((EL2*(2.D0*Mh02 + MHH2)*S2A + 2.D0*Lambda5*MW2*(-3.D0*S2A + S2B)*SW2)**INT(2.D0)))/(EL2*MW2*S&
  &2B2*SW2)

 HHtoh0h0Tree = totalAmplitude
end function HHtoh0h0Tree