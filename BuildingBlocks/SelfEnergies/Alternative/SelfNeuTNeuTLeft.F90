double complex function SelfNeuTNeuTLeftAlter(x)
 use constants
 implicit none
#include "looptools.h"
 double precision, intent(in) :: x
 integer :: j
 double complex :: totalAmplitude
 double complex :: amplitudes(21)

 amplitudes(1) = 0.D0

 amplitudes(2) = 0.D0

 amplitudes(3) = 0.D0

 amplitudes(4) = 0.D0

 amplitudes(5) = 0.D0

 amplitudes(6) = 0.D0

 amplitudes(7) = 0.D0

 amplitudes(8) = 0.D0

 amplitudes(9) = 0.D0

 amplitudes(10) = 0.D0

 amplitudes(11) = 0.D0

 amplitudes(12) = 0.D0

 amplitudes(13) = 0.D0

 amplitudes(14) = 0.D0

 amplitudes(15) = 0.D0

 amplitudes(16) = 0.D0

 amplitudes(17) = 0.D0

 amplitudes(18) = (-0.03125D0*EL2*ML2*B1(x, ML2, MHp2)*DBLE(Yuk6**INT(2.D0)))/(MW2*PI2*SW2)

 amplitudes(19) = (-0.03125D0*EL2*ML2*B1(x, ML2, GaugeXiW*MW2))/(MW2*PI2*SW2)

 amplitudes(20) = (-0.015625D0*EL2*(MZ2 + (MZ2 - 1.D0*x)*B0(x, 0.D0, MZ2) + (-1.D0*GaugeXiZ*MZ2 + x)*B0(x, 0.D0, GaugeXiZ*MZ2) + &
  &2.D0*MZ2*B1(x, 0.D0, MZ2) - 1.D0*x*B1(x, 0.D0, MZ2) + x*B1(x, 0.D0, GaugeXiZ*MZ2)))/(CW2*MZ2*PI2*SW2)

 amplitudes(21) = (-0.03125D0*EL2*(MW2 + (ML2 + MW2 - 1.D0*x)*B0(x, ML2, MW2) - 1.D0*(ML2 + GaugeXiW*MW2 - 1.D0*x)*B0(x, ML2, Gau&
  &geXiW*MW2) + ML2*B1(x, ML2, MW2) + 2.D0*MW2*B1(x, ML2, MW2) - 1.D0*x*B1(x, ML2, MW2) - 1.D0*ML2*B1(x, ML2, GaugeXiW*MW2) + x*B&
  &1(x, ML2, GaugeXiW*MW2)))/ (MW2*PI2*SW2)

  totalAmplitude = (0D0,0D0)
 do j=1,21
  totalAmplitude = totalAmplitude + amplitudes(j)
 end do
 SelfNeuTNeuTLeftAlter = totalAmplitude
end function SelfNeuTNeuTLeftAlter

