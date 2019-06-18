double precision function HHtoA0A0Tree()
 use constants
 implicit none
#include "looptools.h"
 double precision :: totalAmplitude

 totalAmplitude = (0.25D0*EL2*DBLE((CBA*(2.D0*MA02 - 1.D0*MHH2) + (SAB*(2.D0*MHH2 - (4.D0*Lambda5*MW2*SW2)/EL2))/S2B)**INT(2.D0))&
  &)/(MW2*SW2)

 HHtoA0A0Tree = totalAmplitude
end function HHtoA0A0Tree