double precision function h0toHpWmTree()
 use constants
 implicit none
#include "looptools.h"
 double precision :: totalAmplitude

 totalAmplitude = (0.25D0*CBA2*EL2*(-2.D0*Mh02*MHp2 - 2.D0*Mh02*MW2 - 2.D0*MHp2*MW2 + DBLE(Mh0**INT(4.D0)) + DBLE(MHp**INT(4.D0))&
  & + DBLE(MW**INT(4.D0))))/(MW2*SW2)

 h0toHpWmTree = totalAmplitude
end function h0toHpWmTree