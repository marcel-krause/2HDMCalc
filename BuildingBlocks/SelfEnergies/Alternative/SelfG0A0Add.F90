double complex function SelfG0A0Add(x)
 use constants
 implicit none
#include "looptools.h"
 double precision, intent(in) :: x
 double complex :: totalAmplitude

 totalAmplitude = (0.03125D0*CBA*EL2*SBA*(-0.5D0*MA02 + x)*(-1.D0*B0(x, Mh02, MZ2) + B0(x, MHH2, MZ2)))/(CW2*PI2*SW2)

 SelfG0A0Add = totalAmplitude
end function SelfG0A0Add

