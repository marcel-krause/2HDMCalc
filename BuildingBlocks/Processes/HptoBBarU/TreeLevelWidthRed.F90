double precision function HptoBBarUTree()
 use constants
 implicit none
#include "looptools.h"
 double precision :: totalAmplitude

 totalAmplitude = (0.75D0*EL2*DBLE(CKM13**INT(2.D0))*((MHp2 - 1.D0*DBLE((MB - 1.D0*MU)**INT(2.D0)))*DBLE((MU/TB - 1.D0*MB*Yuk3)**&
  &INT(2.D0)) + (MHp2 - 1.D0*DBLE((MB + MU)**INT(2.D0)))*DBLE((MU/TB + MB*Yuk3)**INT(2.D0))))/(MW2*SW2)

 HptoBBarUTree = totalAmplitude
end function HptoBBarUTree