double complex function DSelfMuMuScalar(x)
 use constants
 implicit none
#include "looptools.h"
 double precision, intent(in) :: x
 integer :: j
 double complex :: totalAmplitude
 double complex :: amplitudes(9)

 amplitudes(1) = (0.015625D0*EL2*MM2*DB0(x, Mh02, MM2)*DBLE(Yuk4**INT(2.D0)))/(MW2*PI2*SW2)

 amplitudes(2) = (0.015625D0*EL2*MM2*DB0(x, MHH2, MM2)*DBLE(Yuk5**INT(2.D0)))/(MW2*PI2*SW2)

 amplitudes(3) = (-0.015625D0*EL2*MM2*DB0(x, MA02, MM2)*DBLE(Yuk6**INT(2.D0)))/(MW2*PI2*SW2)

 amplitudes(4) = (-0.015625D0*EL2*MM2*DB0(x, MM2, GaugeXiZ*MZ2))/(MW2*PI2*SW2)

 amplitudes(5) = 0.D0

 amplitudes(6) = 0.D0

 amplitudes(7) = (-0.0625D0*EL2*(3.D0 + GaugeXiA)*DB0(x, 0.D0, MM2))/PI2

 amplitudes(8) = (-0.03125D0*EL2*(-1.D0 + 2.D0*SW2)*(3.D0*DB0(x, MM2, MZ2) + GaugeXiZ*DB0(x, MM2, GaugeXiZ*MZ2)))/(CW2*PI2)

 amplitudes(9) = 0.D0

  totalAmplitude = (0D0,0D0)
 do j=1,9
  totalAmplitude = totalAmplitude + amplitudes(j)
 end do
 DSelfMuMuScalar = totalAmplitude
end function DSelfMuMuScalar

