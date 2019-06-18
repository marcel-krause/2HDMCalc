double precision function HptoDBarUTree()
 use constants
 implicit none
#include "looptools.h"
 double precision :: totalAmplitude

 totalAmplitude = (0.75D0*EL2*DBLE(CKM11**INT(2.D0))*((MHp2 - 1.D0*DBLE((MD - 1.D0*MU)**INT(2.D0)))*DBLE((MU/TB - 1.D0*MD*Yuk3)**&
  &INT(2.D0)) + (MHp2 - 1.D0*DBLE((MD + MU)**INT(2.D0)))*DBLE((MU/TB + MD*Yuk3)**INT(2.D0))))/(MW2*SW2)

 HptoDBarUTree = totalAmplitude
end function HptoDBarUTree