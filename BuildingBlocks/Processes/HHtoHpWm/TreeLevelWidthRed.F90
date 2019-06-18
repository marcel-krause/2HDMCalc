double precision function HHtoHpWmTree()
 use constants
 implicit none
#include "looptools.h"
 double precision :: totalAmplitude

 totalAmplitude = (0.25D0*EL2*SBA2*(-2.D0*MHH2*MHp2 - 2.D0*MHH2*MW2 - 2.D0*MHp2*MW2 + DBLE(MHH**INT(4.D0)) + DBLE(MHp**INT(4.D0))&
  & + DBLE(MW**INT(4.D0))))/(MW2*SW2)

 HHtoHpWmTree = totalAmplitude
end function HHtoHpWmTree