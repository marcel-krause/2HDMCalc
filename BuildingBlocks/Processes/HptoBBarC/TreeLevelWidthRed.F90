double precision function HptoBBarCTree()
 use constants
 implicit none
#include "looptools.h"
 double precision :: totalAmplitude

 totalAmplitude = (0.75D0*EL2*DBLE(CKM23**INT(2.D0))*((MHp2 - 1.D0*DBLE((MB - 1.D0*MC)**INT(2.D0)))*DBLE((MC/TB - 1.D0*MB*Yuk3)**&
  &INT(2.D0)) + (MHp2 - 1.D0*DBLE((MB + MC)**INT(2.D0)))*DBLE((MC/TB + MB*Yuk3)**INT(2.D0))))/(MW2*SW2)

 HptoBBarCTree = totalAmplitude
end function HptoBBarCTree