double complex function SelfGpHpAdd(x)
 use constants
 implicit none
#include "looptools.h"
 double precision, intent(in) :: x
 double complex :: totalAmplitude

 totalAmplitude = (0.0625D0*CBA*EL2*SBA*(-0.5D0*MHp2 + x)*(-1.D0*B0(x, Mh02, MW2) + B0(x, MHH2, MW2)))/(PI2*SW2)

 SelfGpHpAdd = totalAmplitude
end function SelfGpHpAdd

