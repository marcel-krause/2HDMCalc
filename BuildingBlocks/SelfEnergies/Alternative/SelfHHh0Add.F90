double complex function SelfHHh0Add(x)
 use constants
 implicit none
#include "looptools.h"
 double precision, intent(in) :: x
 double complex :: totalAmplitude

 totalAmplitude = (0.03125D0*CBA*EL2*SBA*(0.5D0*(-1.D0*Mh02 - 1.D0*MHH2) + x)*(B0(x, MA02, MZ2) + 2.D0*CW2*(B0(x, MHp2, MW2) - 1.&
  &D0*B0(x, MW2, MW2)) - 1.D0*B0(x, MZ2, MZ2)))/(CW2*PI2*SW2)

 SelfHHh0Add = totalAmplitude
end function SelfHHh0Add

