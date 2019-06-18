double precision function HHtoHmHpTree()
 use constants
 implicit none
#include "looptools.h"
 double precision :: totalAmplitude

 totalAmplitude = (0.25D0*EL2*DBLE((CBA*(MHH2 - 2.D0*MHp2) + (2.D0*SAB*(-1.D0*MHH2 + (2.D0*Lambda5*MW2*SW2)/EL2))/S2B)**INT(2.D0)&
  &))/(MW2*SW2)

 HHtoHmHpTree = totalAmplitude
end function HHtoHmHpTree