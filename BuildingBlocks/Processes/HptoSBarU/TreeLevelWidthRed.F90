double precision function HptoSBarUTree()
 use constants
 implicit none
#include "looptools.h"
 double precision :: totalAmplitude

 totalAmplitude = (0.75D0*EL2*DBLE(CKM12**INT(2.D0))*((MHp2 - 1.D0*DBLE((MS - 1.D0*MU)**INT(2.D0)))*DBLE((MU/TB - 1.D0*MS*Yuk3)**&
  &INT(2.D0)) + (MHp2 - 1.D0*DBLE((MS + MU)**INT(2.D0)))*DBLE((MU/TB + MS*Yuk3)**INT(2.D0))))/(MW2*SW2)

 HptoSBarUTree = totalAmplitude
end function HptoSBarUTree