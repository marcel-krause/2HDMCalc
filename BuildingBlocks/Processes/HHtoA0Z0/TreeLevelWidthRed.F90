double precision function HHtoA0Z0Tree()
 use constants
 implicit none
#include "looptools.h"
 double precision :: totalAmplitude

 totalAmplitude = (0.25D0*EL2*SBA2*(-2.D0*MA02*MHH2 - 2.D0*MA02*MZ2 - 2.D0*MHH2*MZ2 + DBLE(MA0**INT(4.D0)) + DBLE(MHH**INT(4.D0))&
  & + DBLE(MZ**INT(4.D0))))/(CW2*MZ2*SW2)

 HHtoA0Z0Tree = totalAmplitude
end function HHtoA0Z0Tree