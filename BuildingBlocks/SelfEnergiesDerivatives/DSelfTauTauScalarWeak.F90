double complex function DSelfTauTauScalarWeak(x)
 use constants
 implicit none
#include "looptools.h"
 double precision, intent(in) :: x
 integer :: j
 double complex :: totalAmplitude
 double complex :: amplitudes(85)

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

 amplitudes(18) = 0.D0

 amplitudes(19) = 0.D0

 amplitudes(20) = 0.D0

 amplitudes(21) = 0.D0

 amplitudes(22) = 0.D0

 amplitudes(23) = 0.D0

 amplitudes(24) = 0.D0

 amplitudes(25) = 0.D0

 amplitudes(26) = 0.D0

 amplitudes(27) = 0.D0

 amplitudes(28) = 0.D0

 amplitudes(29) = 0.D0

 amplitudes(30) = 0.D0

 amplitudes(31) = 0.D0

 amplitudes(32) = 0.D0

 amplitudes(33) = 0.D0

 amplitudes(34) = 0.D0

 amplitudes(35) = 0.D0

 amplitudes(36) = 0.D0

 amplitudes(37) = 0.D0

 amplitudes(38) = 0.D0

 amplitudes(39) = 0.D0

 amplitudes(40) = 0.D0

 amplitudes(41) = 0.D0

 amplitudes(42) = 0.D0

 amplitudes(43) = 0.D0

 amplitudes(44) = 0.D0

 amplitudes(45) = 0.D0

 amplitudes(46) = 0.D0

 amplitudes(47) = 0.D0

 amplitudes(48) = 0.D0

 amplitudes(49) = 0.D0

 amplitudes(50) = 0.D0

 amplitudes(51) = 0.D0

 amplitudes(52) = 0.D0

 amplitudes(53) = 0.D0

 amplitudes(54) = 0.D0

 amplitudes(55) = 0.D0

 amplitudes(56) = 0.D0

 amplitudes(57) = 0.D0

 amplitudes(58) = 0.D0

 amplitudes(59) = 0.D0

 amplitudes(60) = 0.D0

 amplitudes(61) = 0.D0

 amplitudes(62) = 0.D0

 amplitudes(63) = 0.D0

 amplitudes(64) = 0.D0

 amplitudes(65) = 0.D0

 amplitudes(66) = 0.D0

 amplitudes(67) = 0.D0

 amplitudes(68) = 0.D0

 amplitudes(69) = 0.D0

 amplitudes(70) = 0.D0

 amplitudes(71) = 0.D0

 amplitudes(72) = 0.D0

 amplitudes(73) = 0.D0

 amplitudes(74) = 0.D0

 amplitudes(75) = 0.D0

 amplitudes(76) = 0.D0

 amplitudes(77) = 0.D0

 amplitudes(78) = (0.015625D0*EL2*ML2*DB0(x, Mh02, ML2)*DBLE(Yuk4**INT(2.D0)))/(MW2*PI2*SW2)

 amplitudes(79) = (0.015625D0*EL2*ML2*DB0(x, MHH2, ML2)*DBLE(Yuk5**INT(2.D0)))/(MW2*PI2*SW2)

 amplitudes(80) = (-0.015625D0*EL2*ML2*DB0(x, MA02, ML2)*DBLE(Yuk6**INT(2.D0)))/(MW2*PI2*SW2)

 amplitudes(81) = (-0.015625D0*EL2*ML2*DB0(x, ML2, GaugeXiZ*MZ2))/(MW2*PI2*SW2)

 amplitudes(82) = 0.D0

 amplitudes(83) = 0.D0

 amplitudes(84) = (-0.03125D0*EL2*(-1.D0 + 2.D0*SW2)*(3.D0*DB0(x, ML2, MZ2) + GaugeXiZ*DB0(x, ML2, GaugeXiZ*MZ2)))/(CW2*PI2)

 amplitudes(85) = 0.D0

  totalAmplitude = (0D0,0D0)
 do j=1,85
  totalAmplitude = totalAmplitude + amplitudes(j)
 end do
 DSelfTauTauScalarWeak = totalAmplitude
end function DSelfTauTauScalarWeak

