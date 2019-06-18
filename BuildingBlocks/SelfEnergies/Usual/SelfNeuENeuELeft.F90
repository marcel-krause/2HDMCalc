double complex function SelfNeuENeuELeftUsual(x)
 use constants
 implicit none
#include "looptools.h"
 double precision, intent(in) :: x
 integer :: j
 double complex :: totalAmplitude
 double complex :: amplitudes(4)

 amplitudes(1) = (-0.03125D0*EL2*ME2*B1(x, ME2, MHp2)*DBLE(Yuk6**INT(2.D0)))/(MW2*PI2*SW2)

 amplitudes(2) = (-0.03125D0*EL2*ME2*B1(x, ME2, GaugeXiW*MW2))/(MW2*PI2*SW2)

 amplitudes(3) = (-0.015625D0*EL2*(MZ2 + (MZ2 - 1.D0*x)*B0(x, 0.D0, MZ2) + (-1.D0*GaugeXiZ*MZ2 + x)*B0(x, 0.D0, GaugeXiZ*MZ2) + 2&
  &.D0*MZ2*B1(x, 0.D0, MZ2) - 1.D0*x*B1(x, 0.D0, MZ2) + x*B1(x, 0.D0, GaugeXiZ*MZ2)))/(CW2*MZ2*PI2*SW2)

 amplitudes(4) = (-0.03125D0*EL2*(MW2 + (ME2 + MW2 - 1.D0*x)*B0(x, ME2, MW2) - 1.D0*(ME2 + GaugeXiW*MW2 - 1.D0*x)*B0(x, ME2, Gaug&
  &eXiW*MW2) + ME2*B1(x, ME2, MW2) + 2.D0*MW2*B1(x, ME2, MW2) - 1.D0*x*B1(x, ME2, MW2) - 1.D0*ME2*B1(x, ME2, GaugeXiW*MW2) + x*B1&
  &(x, ME2, GaugeXiW*MW2)))/ (MW2*PI2*SW2)

  totalAmplitude = (0D0,0D0)
 do j=1,4
  totalAmplitude = totalAmplitude + amplitudes(j)
 end do
 SelfNeuENeuELeftUsual = totalAmplitude
end function SelfNeuENeuELeftUsual

