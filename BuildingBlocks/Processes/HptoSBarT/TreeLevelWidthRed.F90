double precision function HptoSBarTTree()
 use constants
 implicit none
#include "looptools.h"
 double precision :: totalAmplitude

 totalAmplitude = (0.75D0*EL2*DBLE(CKM32**INT(2.D0))*((MHp2 - 1.D0*DBLE((MS - 1.D0*MT)**INT(2.D0)))*DBLE((MT/TB - 1.D0*MS*&
  &Yuk3)**INT(2.D0)) + (MHp2 - 1.D0*DBLE((MS + MT)**INT(2.D0)))*DBLE((MT/TB + MS*Yuk3)**INT(2.D0))))/(MW2*SW2)

 HptoSBarTTree = totalAmplitude
end function HptoSBarTTree