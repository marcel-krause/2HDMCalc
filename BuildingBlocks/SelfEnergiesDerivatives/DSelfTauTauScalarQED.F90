double complex function DSelfTauTauScalarQED(x)
 use constants
 implicit none
#include "looptools.h"
 double precision, intent(in) :: x
 integer :: j
 double complex :: totalAmplitude
 double complex :: amplitudes(1)

 amplitudes(1) = (-0.0625D0*EL2*(3.D0 + GaugeXiA)*DB0(x, 0.D0, ML2))/PI2

  totalAmplitude = (0D0,0D0)
 do j=1,1
  totalAmplitude = totalAmplitude + amplitudes(j)
 end do
 DSelfTauTauScalarQED = totalAmplitude
end function DSelfTauTauScalarQED

