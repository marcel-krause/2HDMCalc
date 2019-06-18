double complex function SelfElElRightAlter(x)
 use constants
 implicit none
#include "looptools.h"
 double precision, intent(in) :: x
 integer :: j
 double complex :: totalAmplitude
 double complex :: amplitudes(100)

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

 amplitudes(19) = 0D0

 amplitudes(20) = 0D0

 amplitudes(21) = 0D0

 amplitudes(22) = 0D0

 amplitudes(23) = 0D0

 amplitudes(24) = 0D0

 amplitudes(25) = 0D0

 amplitudes(26) = 0D0

 amplitudes(27) = 0D0

 amplitudes(28) = 0D0

 amplitudes(29) = 0D0

 amplitudes(30) = 0D0

 amplitudes(31) = 0D0

 amplitudes(32) = 0D0

 amplitudes(33) = 0D0

 amplitudes(34) = 0D0

 amplitudes(35) = 0D0

 amplitudes(36) = 0D0

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

 amplitudes(78) = 0.D0

 amplitudes(79) = 0.D0

 amplitudes(80) = 0.D0

 amplitudes(81) = 0.D0

 amplitudes(82) = 0.D0

 amplitudes(83) = 0.D0

 amplitudes(84) = 0.D0

 amplitudes(85) = 0.D0

 amplitudes(86) = 0.D0

 amplitudes(87) = 0.D0

 amplitudes(88) = 0.D0

 amplitudes(89) = 0.D0

 amplitudes(90) = 0.D0

 amplitudes(91) = 0.D0

 amplitudes(92) = (0.0078125D0*EL2*ME2*(-1.D0*A0(ME2) + A0(Mh02) + ME2*B0(x, ME2, Mh02) - 1.D0*Mh02*B0(x, ME2, Mh02) + x*B0(x, ME&
  &2, Mh02))*DBLE(Yuk4**INT(2.D0)))/ (MW2*PI2*SW2*x)

 amplitudes(93) = (0.0078125D0*EL2*ME2*(-1.D0*A0(ME2) + A0(MHH2) + ME2*B0(x, ME2, MHH2) - 1.D0*MHH2*B0(x, ME2, MHH2) + x*B0(x, ME&
  &2, MHH2))*DBLE(Yuk5**INT(2.D0)))/ (MW2*PI2*SW2*x)

 amplitudes(94) = (0.0078125D0*EL2*ME2*(A0(MA02) - 1.D0*A0(ME2) - 1.D0*MA02*B0(x, MA02, ME2) + ME2*B0(x, MA02, ME2) + x*B0(x, MA0&
  &2, ME2))*DBLE(Yuk6**INT(2.D0)))/ (MW2*PI2*SW2*x)

 amplitudes(95) = (0.0078125D0*EL2*ME2*(-1.D0*A0(ME2) + A0(GaugeXiZ*MZ2) + ME2*B0(x, ME2, GaugeXiZ*MZ2) - 1.D0*GaugeXiZ*MZ2*B0(x,&
  & ME2, GaugeXiZ*MZ2) + x*B0(x, ME2, GaugeXiZ*MZ2)))/(MW2*PI2*SW2*x)

 amplitudes(96) = (0.015625D0*EL2*ME2*(A0(MHp2) + (-1.D0*MHp2 + x)*B0(x, 0.D0, MHp2))*DBLE(Yuk6**INT(2.D0)))/(MW2*PI2*SW2*x)

 amplitudes(97) = (0.015625D0*EL2*ME2*(A0(GaugeXiW*MW2) + (-1.D0*GaugeXiW*MW2 + x)*B0(x, 0.D0, GaugeXiW*MW2)))/(MW2*PI2*SW2*x)

 amplitudes(98) = (0.03125D0*EL2*(-2.D0*x - 2.D0*A0(ME2) + ME2*B0(x, 0.D0, ME2) + GaugeXiA*ME2*B0(x, 0.D0, ME2) + x*B0(x, 0.D0, M&
  &E2) + GaugeXiA*x*B0(x, 0.D0, ME2) - 2.D0*ME2*x*C0Mine(DBLE(0.D0), DBLE(x), DBLE(x), DBLE(0.D0), DBLE(0.D0), DBLE(ME2)) + 2.D0*&
  &GaugeXiA*ME2*x*C0Mine(DBLE(0.D0), DBLE(x), DBLE(x), DBLE(0.D0), DBLE(0.D0), DBLE(ME2)) + C0Mine(DBLE(0.D0), DBLE(x), DBLE(x), &
  &DBLE(0.D0), DBLE(0.D0), DBLE(ME2))*DBLE(ME**INT(4.D0)) - 1.D0*GaugeXiA*C0Mine(DBLE(0.D0), DBLE(x), DBLE(x), DBLE(0.D0), DBLE(0&
  &.D0), DBLE(ME2))*DBLE(ME**INT(4.D0)) + C0Mine(DBLE(0.D0), DBLE(x), DBLE(x), DBLE(0.D0), DBLE(0.D0), DBLE(ME2))*DBLE(x**INT(2.D&
  &0)) - 1.D0*GaugeXiA*C0Mine(DBLE(0.D0), DBLE(x), DBLE(x), DBLE(0.D0), DBLE(0.D0), DBLE(ME2))*DBLE(x**INT(2.D0))))/(PI2*x)

 amplitudes(99) = (0.0078125D0*EL2*(-8.D0*MZ2*x*DBLE(SW**INT(4.D0)) - 8.D0*MZ2*A0(ME2)*DBLE(SW**INT(4.D0)) + 4.D0*ME2*A0(MZ2)*DBL&
  &E(SW**INT(4.D0)) + 8.D0*MZ2*A0(MZ2)*DBLE(SW**INT(4.D0)) - 4.D0*x*A0(MZ2)*DBLE(SW**INT(4.D0)) - 4.D0*ME2*A0(GaugeXiZ*MZ2)*DBLE(&
  &SW**INT(4.D0)) + 4.D0*x*A0(GaugeXiZ*MZ2)*DBLE(SW**INT(4.D0)) + 4.D0*ME2*MZ2*B0(x, ME2, MZ2)*DBLE(SW**INT(4.D0)) - 8.D0*ME2*x*B&
  &0(x, ME2, MZ2)*DBLE(SW**INT(4.D0)) + 4.D0*MZ2*x*B0(x, ME2, MZ2)*DBLE(SW**INT(4.D0)) + 4.D0*GaugeXiZ*ME2*MZ2*B0(x, ME2, GaugeXi&
  &Z*MZ2)*DBLE(SW**INT(4.D0)) + 8.D0*ME2*x*B0(x, ME2, GaugeXiZ*MZ2)*DBLE(SW**INT(4.D0)) + 4.D0*GaugeXiZ*MZ2*x*B0(x, ME2, GaugeXiZ&
  &*MZ2)*DBLE(SW**INT(4.D0)) + 4.D0*B0(x, ME2, MZ2)*DBLE(ME**INT(4.D0))*DBLE(SW**INT(4.D0)) - 4.D0*B0(x, ME2, GaugeXiZ*MZ2)*DBLE(&
  &ME**INT(4.D0))*DBLE(SW**INT(4.D0)) - 8.D0*B0(x, ME2, MZ2)*DBLE(MZ**INT(4.D0))*DBLE(SW**INT(4.D0)) + 4.D0*B0(x, ME2, MZ2)*DBLE(&
  &SW**INT(4.D0))*DBLE(x**INT(2.D0)) - 4.D0*B0(x, ME2, GaugeXiZ*MZ2)*DBLE(SW**INT(4.D0))*DBLE(x**INT(2.D0))))/(CW2*MZ2*PI2*SW2*x)

 amplitudes(100) = 0.D0

  totalAmplitude = (0D0,0D0)
 do j=1,100
  totalAmplitude = totalAmplitude + amplitudes(j)
 end do
 SelfElElRightAlter = totalAmplitude
end function SelfElElRightAlter

