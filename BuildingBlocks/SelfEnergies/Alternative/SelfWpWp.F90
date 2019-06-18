double complex function SelfWpWpAlter(x)
 use constants
 implicit none
#include "looptools.h"
 double precision, intent(in) :: x
 integer :: j
 double complex :: totalAmplitude
 double complex :: amplitudes(108)

 amplitudes(1) = (0.015625D0*EL2*A0(Mh02))/(PI2*SW2)

 amplitudes(2) = (0.015625D0*EL2*A0(MHH2))/(PI2*SW2)

 amplitudes(3) = (0.015625D0*EL2*A0(MA02))/(PI2*SW2)

 amplitudes(4) = (0.015625D0*EL2*A0(GaugeXiZ*MZ2))/(PI2*SW2)

 amplitudes(5) = (0.03125D0*EL2*A0(MHp2))/(PI2*SW2)

 amplitudes(6) = (0.03125D0*EL2*A0(GaugeXiW*MW2))/(PI2*SW2)

 amplitudes(7) = 0.D0

 amplitudes(8) = (0.0078125D0*CW2*EL2*(18.D0*A0(MZ2) + 6.D0*GaugeXiZ*A0(GaugeXiZ*MZ2) - 1.D0*MZ2*(15.D0 + DBLE(GaugeXiZ**INT(2.D0&
  &)))))/(PI2*SW2)

 amplitudes(9) = (0.0078125D0*EL2*(18.D0*A0(MW2) + 6.D0*GaugeXiW*A0(GaugeXiW*MW2) - 1.D0*MW2*(15.D0 + DBLE(GaugeXiW**INT(2.D0))))&
  &)/(PI2*SW2)

 amplitudes(10) = (0.125D0*EL2*ME2*SBA*Yuk4*A0(ME2))/(Mh02*PI2*SW2)

 amplitudes(11) = (0.125D0*EL2*MM2*SBA*Yuk4*A0(MM2))/(Mh02*PI2*SW2)

 amplitudes(12) = (0.125D0*EL2*ML2*SBA*Yuk4*A0(ML2))/(Mh02*PI2*SW2)

 amplitudes(13) = (0.375D0*CA*EL2*MU2*SBA*A0(MU2))/(Mh02*PI2*SB*SW2)

 amplitudes(14) = (0.375D0*CA*EL2*MC2*SBA*A0(MC2))/(Mh02*PI2*SB*SW2)

 amplitudes(15) = (0.375D0*CA*EL2*MT2*SBA*A0(MT2))/(Mh02*PI2*SB*SW2)

 amplitudes(16) = (0.375D0*EL2*MD2*SBA*Yuk1*A0(MD2))/(Mh02*PI2*SW2)

 amplitudes(17) = (0.375D0*EL2*MS2*SBA*Yuk1*A0(MS2))/(Mh02*PI2*SW2)

 amplitudes(18) = (0.375D0*EL2*MB2*SBA*Yuk1*A0(MB2))/(Mh02*PI2*SW2)

 amplitudes(19) = (0.125D0*CBA*EL2*ME2*Yuk5*A0(ME2))/(MHH2*PI2*SW2)

 amplitudes(20) = (0.125D0*CBA*EL2*MM2*Yuk5*A0(MM2))/(MHH2*PI2*SW2)

 amplitudes(21) = (0.125D0*CBA*EL2*ML2*Yuk5*A0(ML2))/(MHH2*PI2*SW2)

 amplitudes(22) = (0.375D0*CBA*EL2*MU2*SA*A0(MU2))/(MHH2*PI2*SB*SW2)

 amplitudes(23) = (0.375D0*CBA*EL2*MC2*SA*A0(MC2))/(MHH2*PI2*SB*SW2)

 amplitudes(24) = (0.375D0*CBA*EL2*MT2*SA*A0(MT2))/(MHH2*PI2*SB*SW2)

 amplitudes(25) = (0.375D0*CBA*EL2*MD2*Yuk2*A0(MD2))/(MHH2*PI2*SW2)

 amplitudes(26) = (0.375D0*CBA*EL2*MS2*Yuk2*A0(MS2))/(MHH2*PI2*SW2)

 amplitudes(27) = (0.375D0*CBA*EL2*MB2*Yuk2*A0(MB2))/(MHH2*PI2*SW2)

 amplitudes(28) = (-0.046875D0*SBA*(2.D0*CAB*EL2*Mh02 + EL2*Mh02*S2A*SBA - 4.D0*CAB*CBA2*Lambda5*MW2*SW2)*A0(Mh02))/(Mh02*PI2*S2B&
  &*SW2)

 amplitudes(29) = (0.015625D0*SBA2*(EL2*(Mh02 + 2.D0*MHH2)*S2A - 2.D0*Lambda5*MW2*(3.D0*S2A + S2B)*SW2)*A0(MHH2))/(Mh02*PI2*S2B*S&
  &W2)

 amplitudes(30) = (0.015625D0*SBA*(EL2*(-2.D0*MA02 + Mh02)*S2B*SBA + CAB*(-2.D0*EL2*Mh02 + 4.D0*Lambda5*MW2*SW2))*A0(MA02))/(Mh02&
  &*PI2*S2B*SW2)

 amplitudes(31) = (-0.015625D0*EL2*SBA2*A0(GaugeXiZ*MZ2))/(PI2*SW2)

 amplitudes(32) = (0.03125D0*SBA*(EL2*(Mh02 - 2.D0*MHp2)*S2B*SBA + CAB*(-2.D0*EL2*Mh02 + 4.D0*Lambda5*MW2*SW2))*A0(MHp2))/(Mh02*P&
  &I2*S2B*SW2)

 amplitudes(33) = (-0.03125D0*EL2*SBA2*A0(GaugeXiW*MW2))/(PI2*SW2)

 amplitudes(34) = (-0.015625D0*CBA2*(EL2*(2.D0*Mh02 + MHH2)*S2A + 2.D0*Lambda5*MW2*(-3.D0*S2A + S2B)*SW2)*A0(Mh02))/(MHH2*PI2*S2B&
  &*SW2)

 amplitudes(35) = (0.046875D0*CBA*(CBA*EL2*MHH2*S2A - 2.D0*EL2*MHH2*SAB + 4.D0*Lambda5*MW2*SAB*SBA2*SW2)*A0(MHH2))/(MHH2*PI2*S2B*&
  &SW2)

 amplitudes(36) = (0.015625D0*CBA*(CBA*EL2*(-2.D0*MA02 + MHH2)*S2B - 2.D0*EL2*MHH2*SAB + 4.D0*Lambda5*MW2*SAB*SW2)*A0(MA02))/(MHH&
  &2*PI2*S2B*SW2)

 amplitudes(37) = (-0.015625D0*CBA2*EL2*A0(GaugeXiZ*MZ2))/(PI2*SW2)

 amplitudes(38) = (0.03125D0*CBA*(CBA*EL2*(MHH2 - 2.D0*MHp2)*S2B - 2.D0*EL2*MHH2*SAB + 4.D0*Lambda5*MW2*SAB*SW2)*A0(MHp2))/(MHH2*&
  &PI2*S2B*SW2)

 amplitudes(39) = (-0.03125D0*CBA2*EL2*A0(GaugeXiW*MW2))/(PI2*SW2)

 amplitudes(40) = (0.03125D0*EL2*GaugeXiZ*MW2*SBA2*A0(GaugeXiZ*MZ2))/(CW2*Mh02*PI2*SW2)

 amplitudes(41) = (0.03125D0*EL2*GaugeXiW*MW2*SBA2*A0(GaugeXiW*MW2))/(Mh02*PI2*SW2)

 amplitudes(42) = (0.03125D0*EL2*GaugeXiW*MW2*SBA2*A0(GaugeXiW*MW2))/(Mh02*PI2*SW2)

 amplitudes(43) = (0.03125D0*CBA2*EL2*GaugeXiZ*MW2*A0(GaugeXiZ*MZ2))/(CW2*MHH2*PI2*SW2)

 amplitudes(44) = (0.03125D0*CBA2*EL2*GaugeXiW*MW2*A0(GaugeXiW*MW2))/(MHH2*PI2*SW2)

 amplitudes(45) = (0.03125D0*CBA2*EL2*GaugeXiW*MW2*A0(GaugeXiW*MW2))/(MHH2*PI2*SW2)

 amplitudes(46) = (0.03125D0*EL2*MW2*SBA2*(2.D0*MZ2 - 3.D0*A0(MZ2) - 1.D0*GaugeXiZ*A0(GaugeXiZ*MZ2)))/(CW2*Mh02*PI2*SW2)

 amplitudes(47) = (0.0625D0*EL2*MW2*SBA2*(2.D0*MW2 - 3.D0*A0(MW2) - 1.D0*GaugeXiW*A0(GaugeXiW*MW2)))/(Mh02*PI2*SW2)

 amplitudes(48) = (0.03125D0*CBA2*EL2*MW2*(2.D0*MZ2 - 3.D0*A0(MZ2) - 1.D0*GaugeXiZ*A0(GaugeXiZ*MZ2)))/(CW2*MHH2*PI2*SW2)

 amplitudes(49) = (0.0625D0*CBA2*EL2*MW2*(2.D0*MW2 - 3.D0*A0(MW2) - 1.D0*GaugeXiW*A0(GaugeXiW*MW2)))/(MHH2*PI2*SW2)

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

 amplitudes(81) = (0.003472222222222222D0*EL2*(2.D0*(3.D0*ME2 - 1.D0*x)*x + 3.D0*(ME2 - 2.D0*x)*A0(ME2) - 3.D0*B0(x, 0.D0, ME2)*(&
  &ME2*x + DBLE(ME**INT(4.D0)) - 2.D0*DBLE(x**INT(2.D0)))))/(PI2*SW2*x)

 amplitudes(82) = (0.003472222222222222D0*EL2*(2.D0*(3.D0*MM2 - 1.D0*x)*x + 3.D0*(MM2 - 2.D0*x)*A0(MM2) - 3.D0*B0(x, 0.D0, MM2)*(&
  &MM2*x + DBLE(MM**INT(4.D0)) - 2.D0*DBLE(x**INT(2.D0)))))/(PI2*SW2*x)

 amplitudes(83) = (0.003472222222222222D0*EL2*(2.D0*(3.D0*ML2 - 1.D0*x)*x + 3.D0*(ML2 - 2.D0*x)*A0(ML2) - 3.D0*B0(x, 0.D0, ML2)*(&
  &ML2*x + DBLE(ML**INT(4.D0)) - 2.D0*DBLE(x**INT(2.D0)))))/(PI2*SW2*x)

 amplitudes(84) = (-0.010416666666666666D0*CKM11*CKMC11*EL2*(-6.D0*MD2*x - 6.D0*MU2*x + (-3.D0*MD2 + 3.D0*MU2 + 6.D0*x)*A0(MD2) +&
  & 3.D0*(MD2 - 1.D0*MU2 + 2.D0*x)*A0(MU2) - 6.D0*MD2*MU2*B0(x, MD2, MU2) + 3.D0*MD2*x*B0(x, MD2, MU2) + 3.D0*MU2*x*B0(x, MD2, MU&
  &2) + 3.D0*B0(x, MD2, MU2)*DBLE(MD**INT(4.D0)) + 3.D0*B0(x, MD2, MU2)*DBLE(MU**INT(4.D0)) + 2.D0*DBLE(x**INT(2.D0)) - 6.D0*B0(x&
  &, MD2, MU2)*DBLE(x**INT(2.D0))))/(PI2*SW2*x)

 amplitudes(85) = (-0.010416666666666666D0*CKM21*CKMC21*EL2*(-6.D0*MC2*x - 6.D0*MD2*x + (-3.D0*MC2 + 3.D0*MD2 + 6.D0*x)*A0(MC2) +&
  & 3.D0*(MC2 - 1.D0*MD2 + 2.D0*x)*A0(MD2) - 6.D0*MC2*MD2*B0(x, MC2, MD2) + 3.D0*MC2*x*B0(x, MC2, MD2) + 3.D0*MD2*x*B0(x, MC2, MD&
  &2) + 3.D0*B0(x, MC2, MD2)*DBLE(MC**INT(4.D0)) + 3.D0*B0(x, MC2, MD2)*DBLE(MD**INT(4.D0)) + 2.D0*DBLE(x**INT(2.D0)) - 6.D0*B0(x&
  &, MC2, MD2)*DBLE(x**INT(2.D0))))/(PI2*SW2*x)

 amplitudes(86) = (-0.010416666666666666D0*CKM31*CKMC31*EL2*(-6.D0*MD2*x - 6.D0*MT2*x + (-3.D0*MD2 + 3.D0*MT2 + 6.D0*x)*A0(MD2) +&
  & 3.D0*(MD2 - 1.D0*MT2 + 2.D0*x)*A0(MT2) - 6.D0*MD2*MT2*B0(x, MD2, MT2) + 3.D0*MD2*x*B0(x, MD2, MT2) + 3.D0*MT2*x*B0(x, MD2, MT&
  &2) + 3.D0*B0(x, MD2, MT2)*DBLE(MD**INT(4.D0)) + 3.D0*B0(x, MD2, MT2)*DBLE(MT**INT(4.D0)) + 2.D0*DBLE(x**INT(2.D0)) - 6.D0*B0(x&
  &, MD2, MT2)*DBLE(x**INT(2.D0))))/(PI2*SW2*x)

 amplitudes(87) = (-0.010416666666666666D0*CKM12*CKMC12*EL2*(-6.D0*MS2*x - 6.D0*MU2*x + (-3.D0*MS2 + 3.D0*MU2 + 6.D0*x)*A0(MS2) +&
  & 3.D0*(MS2 - 1.D0*MU2 + 2.D0*x)*A0(MU2) - 6.D0*MS2*MU2*B0(x, MS2, MU2) + 3.D0*MS2*x*B0(x, MS2, MU2) + 3.D0*MU2*x*B0(x, MS2, MU&
  &2) + 3.D0*B0(x, MS2, MU2)*DBLE(MS**INT(4.D0)) + 3.D0*B0(x, MS2, MU2)*DBLE(MU**INT(4.D0)) + 2.D0*DBLE(x**INT(2.D0)) - 6.D0*B0(x&
  &, MS2, MU2)*DBLE(x**INT(2.D0))))/(PI2*SW2*x)

 amplitudes(88) = (-0.010416666666666666D0*CKM22*CKMC22*EL2*(-6.D0*MC2*x - 6.D0*MS2*x + (-3.D0*MC2 + 3.D0*MS2 + 6.D0*x)*A0(MC2) +&
  & 3.D0*(MC2 - 1.D0*MS2 + 2.D0*x)*A0(MS2) - 6.D0*MC2*MS2*B0(x, MC2, MS2) + 3.D0*MC2*x*B0(x, MC2, MS2) + 3.D0*MS2*x*B0(x, MC2, MS&
  &2) + 3.D0*B0(x, MC2, MS2)*DBLE(MC**INT(4.D0)) + 3.D0*B0(x, MC2, MS2)*DBLE(MS**INT(4.D0)) + 2.D0*DBLE(x**INT(2.D0)) - 6.D0*B0(x&
  &, MC2, MS2)*DBLE(x**INT(2.D0))))/(PI2*SW2*x)

 amplitudes(89) = (-0.010416666666666666D0*CKM32*CKMC32*EL2*(-6.D0*MS2*x - 6.D0*MT2*x + (-3.D0*MS2 + 3.D0*MT2 + 6.D0*x)*A0(MS2) +&
  & 3.D0*(MS2 - 1.D0*MT2 + 2.D0*x)*A0(MT2) - 6.D0*MS2*MT2*B0(x, MS2, MT2) + 3.D0*MS2*x*B0(x, MS2, MT2) + 3.D0*MT2*x*B0(x, MS2, MT&
  &2) + 3.D0*B0(x, MS2, MT2)*DBLE(MS**INT(4.D0)) + 3.D0*B0(x, MS2, MT2)*DBLE(MT**INT(4.D0)) + 2.D0*DBLE(x**INT(2.D0)) - 6.D0*B0(x&
  &, MS2, MT2)*DBLE(x**INT(2.D0))))/(PI2*SW2*x)

 amplitudes(90) = (-0.010416666666666666D0*CKM13*CKMC13*EL2*(-6.D0*MB2*x - 6.D0*MU2*x + (-3.D0*MB2 + 3.D0*MU2 + 6.D0*x)*A0(MB2) +&
  & 3.D0*(MB2 - 1.D0*MU2 + 2.D0*x)*A0(MU2) - 6.D0*MB2*MU2*B0(x, MB2, MU2) + 3.D0*MB2*x*B0(x, MB2, MU2) + 3.D0*MU2*x*B0(x, MB2, MU&
  &2) + 3.D0*B0(x, MB2, MU2)*DBLE(MB**INT(4.D0)) + 3.D0*B0(x, MB2, MU2)*DBLE(MU**INT(4.D0)) + 2.D0*DBLE(x**INT(2.D0)) - 6.D0*B0(x&
  &, MB2, MU2)*DBLE(x**INT(2.D0))))/(PI2*SW2*x)

 amplitudes(91) = (-0.010416666666666666D0*CKM23*CKMC23*EL2*(-6.D0*MB2*x - 6.D0*MC2*x + (-3.D0*MB2 + 3.D0*MC2 + 6.D0*x)*A0(MB2) +&
  & 3.D0*(MB2 - 1.D0*MC2 + 2.D0*x)*A0(MC2) - 6.D0*MB2*MC2*B0(x, MB2, MC2) + 3.D0*MB2*x*B0(x, MB2, MC2) + 3.D0*MC2*x*B0(x, MB2, MC&
  &2) + 3.D0*B0(x, MB2, MC2)*DBLE(MB**INT(4.D0)) + 3.D0*B0(x, MB2, MC2)*DBLE(MC**INT(4.D0)) + 2.D0*DBLE(x**INT(2.D0)) - 6.D0*B0(x&
  &, MB2, MC2)*DBLE(x**INT(2.D0))))/(PI2*SW2*x)

 amplitudes(92) = (-0.010416666666666666D0*CKM33*CKMC33*EL2*(-6.D0*MB2*x - 6.D0*MT2*x + (-3.D0*MB2 + 3.D0*MT2 + 6.D0*x)*A0(MB2) +&
  & 3.D0*(MB2 - 1.D0*MT2 + 2.D0*x)*A0(MT2) - 6.D0*MB2*MT2*B0(x, MB2, MT2) + 3.D0*MB2*x*B0(x, MB2, MT2) + 3.D0*MT2*x*B0(x, MB2, MT&
  &2) + 3.D0*B0(x, MB2, MT2)*DBLE(MB**INT(4.D0)) + 3.D0*B0(x, MB2, MT2)*DBLE(MT**INT(4.D0)) + 2.D0*DBLE(x**INT(2.D0)) - 6.D0*B0(x&
  &, MB2, MT2)*DBLE(x**INT(2.D0))))/(PI2*SW2*x)

 amplitudes(93) = (0.001736111111111111D0*CBA2*EL2*(-6.D0*Mh02*x - 6.D0*MHp2*x - 3.D0*(Mh02 - 1.D0*MHp2 + x)*A0(Mh02) + 3.D0*(Mh0&
  &2 - 1.D0*MHp2 - 1.D0*x)*A0(MHp2) - 6.D0*Mh02*MHp2*B0(x, Mh02, MHp2) - 6.D0*Mh02*x*B0(x, Mh02, MHp2) - 6.D0*MHp2*x*B0(x, Mh02, &
  &MHp2) + 3.D0*B0(x, Mh02, MHp2)*DBLE(Mh0**INT(4.D0)) + 3.D0*B0(x, Mh02, MHp2)*DBLE(MHp**INT(4.D0)) + 2.D0*DBLE(x**INT(2.D0)) + &
  &3.D0*B0(x, Mh02, MHp2)*DBLE(x**INT(2.D0))))/(PI2*SW2*x)

 amplitudes(94) = (0.001736111111111111D0*EL2*SBA2*(-6.D0*MHH2*x - 6.D0*MHp2*x - 3.D0*(MHH2 - 1.D0*MHp2 + x)*A0(MHH2) + 3.D0*(MHH&
  &2 - 1.D0*MHp2 - 1.D0*x)*A0(MHp2) - 6.D0*MHH2*MHp2*B0(x, MHH2, MHp2) - 6.D0*MHH2*x*B0(x, MHH2, MHp2) - 6.D0*MHp2*x*B0(x, MHH2, &
  &MHp2) + 3.D0*B0(x, MHH2, MHp2)*DBLE(MHH**INT(4.D0)) + 3.D0*B0(x, MHH2, MHp2)*DBLE(MHp**INT(4.D0)) + 2.D0*DBLE(x**INT(2.D0)) + &
  &3.D0*B0(x, MHH2, MHp2)*DBLE(x**INT(2.D0))))/(PI2*SW2*x)

 amplitudes(95) = (0.001736111111111111D0*EL2*(-6.D0*MA02*x - 6.D0*MHp2*x - 3.D0*(MA02 - 1.D0*MHp2 + x)*A0(MA02) + 3.D0*(MA02 - 1&
  &.D0*MHp2 - 1.D0*x)*A0(MHp2) - 6.D0*MA02*MHp2*B0(x, MA02, MHp2) - 6.D0*MA02*x*B0(x, MA02, MHp2) - 6.D0*MHp2*x*B0(x, MA02, MHp2)&
  & + 3.D0*B0(x, MA02, MHp2)*DBLE(MA0**INT(4.D0)) + 3.D0*B0(x, MA02, MHp2)*DBLE(MHp**INT(4.D0)) + 2.D0*DBLE(x**INT(2.D0)) + 3.D0*&
  &B0(x, MA02, MHp2)*DBLE(x**INT(2.D0))))/(PI2*SW2*x)

 amplitudes(96) = (0.001736111111111111D0*EL2*SBA2*(-6.D0*Mh02*x - 6.D0*GaugeXiW*MW2*x - 3.D0*(Mh02 - 1.D0*GaugeXiW*MW2 + x)*A0(M&
  &h02) + 3.D0*(Mh02 - 1.D0*GaugeXiW*MW2 - 1.D0*x)*A0(GaugeXiW*MW2) - 6.D0*GaugeXiW*Mh02*MW2*B0(x, Mh02, GaugeXiW*MW2) - 6.D0*Mh0&
  &2*x*B0(x, Mh02, GaugeXiW*MW2) - 6.D0*GaugeXiW*MW2*x*B0(x, Mh02, GaugeXiW*MW2) + 3.D0*B0(x, Mh02, GaugeXiW*MW2)*DBLE(Mh0**INT(4&
  &.D0)) + 3.D0*B0(x, Mh02, GaugeXiW*MW2)*DBLE(GaugeXiW**INT(2.D0))*DBLE(MW**INT(4.D0)) + 2.D0*DBLE(x**INT(2.D0)) + 3.D0*B0(x, Mh&
  &02, GaugeXiW*MW2)*DBLE(x**INT(2.D0))))/(PI2*SW2*x)

 amplitudes(97) = (0.001736111111111111D0*CBA2*EL2*(-6.D0*MHH2*x - 6.D0*GaugeXiW*MW2*x - 3.D0*(MHH2 - 1.D0*GaugeXiW*MW2 + x)*A0(M&
  &HH2) + 3.D0*(MHH2 - 1.D0*GaugeXiW*MW2 - 1.D0*x)*A0(GaugeXiW*MW2) - 6.D0*GaugeXiW*MHH2*MW2*B0(x, MHH2, GaugeXiW*MW2) - 6.D0*MHH&
  &2*x*B0(x, MHH2, GaugeXiW*MW2) - 6.D0*GaugeXiW*MW2*x*B0(x, MHH2, GaugeXiW*MW2) + 3.D0*B0(x, MHH2, GaugeXiW*MW2)*DBLE(MHH**INT(4&
  &.D0)) + 3.D0*B0(x, MHH2, GaugeXiW*MW2)*DBLE(GaugeXiW**INT(2.D0))*DBLE(MW**INT(4.D0)) + 2.D0*DBLE(x**INT(2.D0)) + 3.D0*B0(x, MH&
  &H2, GaugeXiW*MW2)*DBLE(x**INT(2.D0))))/(PI2*SW2*x)

 amplitudes(98) = (0.001736111111111111D0*EL2*(-6.D0*GaugeXiW*MW2*x - 6.D0*GaugeXiZ*MZ2*x - 3.D0*(GaugeXiW*MW2 - 1.D0*GaugeXiZ*MZ&
  &2 + x)*A0(GaugeXiW*MW2) - 3.D0*(-1.D0*GaugeXiW*MW2 + GaugeXiZ*MZ2 + x)*A0(GaugeXiZ*MZ2) - 6.D0*GaugeXiW*GaugeXiZ*MW2*MZ2*B0(x,&
  & GaugeXiW*MW2, GaugeXiZ*MZ2) - 6.D0*GaugeXiW*MW2*x*B0(x, GaugeXiW*MW2, GaugeXiZ*MZ2) - 6.D0*GaugeXiZ*MZ2*x*B0(x, GaugeXiW*MW2,&
  & GaugeXiZ*MZ2) + 3.D0*B0(x, GaugeXiW*MW2, GaugeXiZ*MZ2)*DBLE(GaugeXiW**INT(2.D0))*DBLE(MW**INT(4.D0)) + 3.D0*B0(x, GaugeXiW*MW&
  &2, GaugeXiZ*MZ2)*DBLE(GaugeXiZ**INT(2.D0))*DBLE(MZ**INT(4.D0)) + 2.D0*DBLE(x**INT(2.D0)) + 3.D0*B0(x, GaugeXiW*MW2, GaugeXiZ*M&
  &Z2)*DBLE(x**INT(2.D0))))/(PI2*SW2*x)

 amplitudes(99) = (0.001736111111111111D0*EL2*(-2.D0*x*(-3.D0*GaugeXiW*MW2 + x) + 3.D0*(GaugeXiW*MW2 + x)*A0(GaugeXiW*MW2) - 3.D0&
  &*B0(x, 0.D0, GaugeXiW*MW2)*DBLE((-1.D0*GaugeXiW*MW2 + x)**INT(2.D0))))/(PI2*x)

 amplitudes(100) = (0.001736111111111111D0*CW2*EL2*(6.D0*GaugeXiW*MW2*x + 6.D0*GaugeXiZ*MZ2*x + 3.D0*(GaugeXiW*MW2 - 1.D0*GaugeXi&
  &Z*MZ2 + x)*A0(GaugeXiW*MW2) + 3.D0*(-1.D0*GaugeXiW*MW2 + GaugeXiZ*MZ2 + x)*A0(GaugeXiZ*MZ2) + 6.D0*GaugeXiW*GaugeXiZ*MW2*MZ2*B&
  &0(x, GaugeXiW*MW2, GaugeXiZ*MZ2) + 6.D0*GaugeXiW*MW2*x*B0(x, GaugeXiW*MW2, GaugeXiZ*MZ2) + 6.D0*GaugeXiZ*MZ2*x*B0(x, GaugeXiW*&
  &MW2, GaugeXiZ*MZ2) - 3.D0*B0(x, GaugeXiW*MW2, GaugeXiZ*MZ2)*DBLE(GaugeXiW**INT(2.D0))*DBLE(MW**INT(4.D0)) - 3.D0*B0(x, GaugeXi&
  &W*MW2, GaugeXiZ*MZ2)*DBLE(GaugeXiZ**INT(2.D0))*DBLE(MZ**INT(4.D0)) - 2.D0*DBLE(x**INT(2.D0)) - 3.D0*B0(x, GaugeXiW*MW2, GaugeX&
  &iZ*MZ2)*DBLE(x**INT(2.D0))))/(PI2*SW2*x)

 amplitudes(101) = (0.001736111111111111D0*EL2*(-2.D0*x*(-3.D0*GaugeXiW*MW2 + x) + 3.D0*(GaugeXiW*MW2 + x)*A0(GaugeXiW*MW2) - 3.D&
  &0*B0(x, 0.D0, GaugeXiW*MW2)*DBLE((-1.D0*GaugeXiW*MW2 + x)**INT(2.D0))))/(PI2*x)

 amplitudes(102) = (0.001736111111111111D0*CW2*EL2*(6.D0*GaugeXiW*MW2*x + 6.D0*GaugeXiZ*MZ2*x + 3.D0*(GaugeXiW*MW2 - 1.D0*GaugeXi&
  &Z*MZ2 + x)*A0(GaugeXiW*MW2) + 3.D0*(-1.D0*GaugeXiW*MW2 + GaugeXiZ*MZ2 + x)*A0(GaugeXiZ*MZ2) + 6.D0*GaugeXiW*GaugeXiZ*MW2*MZ2*B&
  &0(x, GaugeXiW*MW2, GaugeXiZ*MZ2) + 6.D0*GaugeXiW*MW2*x*B0(x, GaugeXiW*MW2, GaugeXiZ*MZ2) + 6.D0*GaugeXiZ*MZ2*x*B0(x, GaugeXiW*&
  &MW2, GaugeXiZ*MZ2) - 3.D0*B0(x, GaugeXiW*MW2, GaugeXiZ*MZ2)*DBLE(GaugeXiW**INT(2.D0))*DBLE(MW**INT(4.D0)) - 3.D0*B0(x, GaugeXi&
  &W*MW2, GaugeXiZ*MZ2)*DBLE(GaugeXiZ**INT(2.D0))*DBLE(MZ**INT(4.D0)) - 2.D0*DBLE(x**INT(2.D0)) - 3.D0*B0(x, GaugeXiW*MW2, GaugeX&
  &iZ*MZ2)*DBLE(x**INT(2.D0))))/(PI2*SW2*x)

 amplitudes(103) = (-0.0008680555555555555D0*EL2*(-6.D0*x*(-11.D0*GaugeXiW*MW2 + 9.D0*x + GaugeXiA*x)*A0(GaugeXiW*MW2) - 15.D0*x*&
  &DBLE(MW**INT(4.D0)) + 192.D0*x*B0(x, 0.D0, MW2)*DBLE(MW**INT(4.D0)) + 12.D0*GaugeXiA*x*B0(x, 0.D0, MW2)*DBLE(MW**INT(4.D0)) - &
  &9.D0*x*DBLE(GaugeXiW**INT(2.D0))*DBLE(MW**INT(4.D0)) - 12.D0*x*B0(x, 0.D0, GaugeXiW*MW2)*DBLE(GaugeXiW**INT(2.D0))*DBLE(MW**IN&
  &T(4.D0)) + 4.D0*DBLE(MW**INT(6.D0)) - 4.D0*GaugeXiA*DBLE(MW**INT(6.D0)) - 48.D0*B0(x, 0.D0, MW2)*DBLE(MW**INT(6.D0)) - 12.D0*G&
  &augeXiA*B0(x, 0.D0, MW2)*DBLE(MW**INT(6.D0)) - 48.D0*x*C0Mine(DBLE(0.D0), DBLE(x), DBLE(x), DBLE(0.D0), DBLE(0.D0), DBLE(MW2))&
  &* DBLE(MW**INT(6.D0)) + 48.D0*GaugeXiA*x*C0Mine(DBLE(0.D0), DBLE(x), DBLE(x), DBLE(0.D0), DBLE(0.D0), DBLE(MW2))*DBLE(MW**INT(&
  &6.D0)) - 6.D0*C0Mine(DBLE(0.D0), DBLE(x), DBLE(x), DBLE(0.D0), DBLE(0.D0), DBLE(MW2))*DBLE(MW**INT(8.D0)) + 6.D0*GaugeXiA*C0Mi&
  &ne(DBLE(0.D0), DBLE(x), DBLE(x), DBLE(0.D0), DBLE(0.D0), DBLE(MW2))*DBLE(MW**INT(8.D0)) - 28.D0*MW2*DBLE(x**INT(2.D0)) + 12.D0&
  &*GaugeXiA*MW2*DBLE(x**INT(2.D0)) + 20.D0*GaugeXiW*MW2*DBLE(x**INT(2.D0)) + 4.D0*GaugeXiA*GaugeXiW*MW2*DBLE(x**INT(2.D0)) + 192&
  &.D0*MW2*B0(x, 0.D0, MW2)*DBLE(x**INT(2.D0)) + 12.D0*GaugeXiA*MW2*B0(x, 0.D0, MW2)*DBLE(x**INT(2.D0)) + 12.D0*GaugeXiW*MW2*B0(x&
  &, 0.D0, GaugeXiW*MW2)*DBLE(x**INT(2.D0)) + 12.D0*GaugeXiA*GaugeXiW*MW2*B0(x, 0.D0, GaugeXiW*MW2)*DBLE(x**INT(2.D0)) + 108.D0*C&
  &0Mine(DBLE(0.D0), DBLE(x), DBLE(x), DBLE(0.D0), DBLE(0.D0), DBLE(MW2))*DBLE(MW**INT(4.D0))*DBLE(x**INT(2.D0)) - 108.D0*GaugeXi&
  &A*C0Mine(DBLE(0.D0), DBLE(x), DBLE(x), DBLE(0.D0), DBLE(0.D0), DBLE(MW2))*DBLE(MW**INT(4.D0))*DBLE(x**INT(2.D0)) + 6.D0*C0Mine&
  &(DBLE(0.D0), DBLE(x), DBLE(x), DBLE(0.D0), DBLE(0.D0), DBLE(GaugeXiW*MW2))*DBLE(GaugeXiW**INT(2.D0))*DBLE(MW**INT(4.D0))* DBLE&
  &(x**INT(2.D0)) - 6.D0*GaugeXiA*C0Mine(DBLE(0.D0), DBLE(x), DBLE(x), DBLE(0.D0), DBLE(0.D0), DBLE(GaugeXiW*MW2))*DBLE(GaugeXiW*&
  &*INT(2.D0))* DBLE(MW**INT(4.D0))*DBLE(x**INT(2.D0)) + 6.D0*A0(MW2)*(13.D0*MW2*x + 9.D0*DBLE(MW**INT(4.D0)) + GaugeXiA*DBLE((MW&
  &2 - 1.D0*x)**INT(2.D0)) + 9.D0*DBLE(x**INT(2.D0))) - 48.D0*B0(x, 0.D0, MW2)*DBLE(x**INT(3.D0)) - 12.D0*GaugeXiA*B0(x, 0.D0, MW&
  &2)*DBLE(x**INT(3.D0)) + 48.D0*B0(x, 0.D0, GaugeXiW*MW2)*DBLE(x**INT(3.D0)) + 12.D0*GaugeXiA*B0(x, 0.D0, GaugeXiW*MW2)*DBLE(x**&
  &INT(3.D0)) - 48.D0*MW2*C0Mine(DBLE(0.D0), DBLE(x), DBLE(x), DBLE(0.D0), DBLE(0.D0), DBLE(MW2))*DBLE(x**INT(3.D0)) + 48.D0*Gaug&
  &eXiA*MW2*C0Mine(DBLE(0.D0), DBLE(x), DBLE(x), DBLE(0.D0), DBLE(0.D0), DBLE(MW2))*DBLE(x**INT(3.D0)) - 12.D0*GaugeXiW*MW2*C0Min&
  &e(DBLE(0.D0), DBLE(x), DBLE(x), DBLE(0.D0), DBLE(0.D0), DBLE(GaugeXiW*MW2))*DBLE(x**INT(3.D0)) + 12.D0*GaugeXiA*GaugeXiW*MW2*C&
  &0Mine(DBLE(0.D0), DBLE(x), DBLE(x), DBLE(0.D0), DBLE(0.D0), DBLE(GaugeXiW*MW2))*DBLE(x**INT(3.D0)) - 6.D0*C0Mine(DBLE(0.D0), D&
  &BLE(x), DBLE(x), DBLE(0.D0), DBLE(0.D0), DBLE(MW2))*DBLE(x**INT(4.D0)) + 6.D0*GaugeXiA*C0Mine(DBLE(0.D0), DBLE(x), DBLE(x), DB&
  &LE(0.D0), DBLE(0.D0), DBLE(MW2))*DBLE(x**INT(4.D0)) + 6.D0*C0Mine(DBLE(0.D0), DBLE(x), DBLE(x), DBLE(0.D0), DBLE(0.D0), DBLE(G&
  &augeXiW*MW2))*DBLE(x**INT(4.D0)) - 6.D0*GaugeXiA*C0Mine(DBLE(0.D0), DBLE(x), DBLE(x), DBLE(0.D0), DBLE(0.D0), DBLE(GaugeXiW*MW&
  &2))*DBLE(x**INT(4.D0))))/(MW2*PI2*x)

 amplitudes(104) = (0.0008680555555555555D0*CW2*EL2*(-66.D0*GaugeXiW*MW2*MZ2*x*A0(GaugeXiW*MW2) - 66.D0*GaugeXiZ*MW2*MZ2*x*A0(Gau&
  &geXiZ*MZ2) + 3.D0*MZ2*x*DBLE(MW**INT(4.D0)) + 12.D0*GaugeXiZ*MZ2*x*DBLE(MW**INT(4.D0)) + 6.D0*GaugeXiZ*MZ2*A0(GaugeXiZ*MZ2)*DB&
  &LE(MW**INT(4.D0)) - 54.D0*x*A0(GaugeXiZ*MZ2)*DBLE(MW**INT(4.D0)) - 192.D0*MZ2*x*B0(x, MW2, MZ2)*DBLE(MW**INT(4.D0)) - 12.D0*Ga&
  &ugeXiZ*MZ2*x*B0(x, MW2, GaugeXiZ*MZ2)*DBLE(MW**INT(4.D0)) + 9.D0*MZ2*x*DBLE(GaugeXiW**INT(2.D0))*DBLE(MW**INT(4.D0)) + 12.D0*M&
  &Z2*x*B0(x, GaugeXiW*MW2, MZ2)*DBLE(GaugeXiW**INT(2.D0))*DBLE(MW**INT(4.D0)) - 6.D0*A0(GaugeXiZ*MZ2)*DBLE(MW**INT(6.D0)) + 48.D&
  &0*MZ2*B0(x, MW2, MZ2)*DBLE(MW**INT(6.D0)) + 48.D0*x*B0(x, MW2, MZ2)*DBLE(MW**INT(6.D0)) + 12.D0*GaugeXiZ*MZ2*B0(x, MW2, GaugeX&
  &iZ*MZ2)*DBLE(MW**INT(6.D0)) - 48.D0*x*B0(x, MW2, GaugeXiZ*MZ2)*DBLE(MW**INT(6.D0)) + 6.D0*B0(x, MW2, MZ2)*DBLE(MW**INT(8.D0)) &
  &- 6.D0*B0(x, MW2, GaugeXiZ*MZ2)*DBLE(MW**INT(8.D0)) + 3.D0*MW2*x*DBLE(MZ**INT(4.D0)) + 12.D0*GaugeXiW*MW2*x*DBLE(MZ**INT(4.D0)&
  &) + 6.D0*GaugeXiW*MW2*A0(GaugeXiW*MW2)*DBLE(MZ**INT(4.D0)) - 54.D0*x*A0(GaugeXiW*MW2)*DBLE(MZ**INT(4.D0)) - 192.D0*MW2*x*B0(x,&
  & MW2, MZ2)*DBLE(MZ**INT(4.D0)) - 12.D0*GaugeXiW*MW2*x*B0(x, GaugeXiW*MW2, MZ2)*DBLE(MZ**INT(4.D0)) + 9.D0*MW2*x*DBLE(GaugeXiZ*&
  &*INT(2.D0))*DBLE(MZ**INT(4.D0)) + 12.D0*MW2*x*B0(x, MW2, GaugeXiZ*MZ2)*DBLE(GaugeXiZ**INT(2.D0))*DBLE(MZ**INT(4.D0)) - 108.D0*&
  &B0(x, MW2, MZ2)*DBLE(MW**INT(4.D0))* DBLE(MZ**INT(4.D0)) - 6.D0*B0(x, GaugeXiW*MW2, MZ2)*DBLE(GaugeXiW**INT(2.D0))*DBLE(MW**IN&
  &T(4.D0))*DBLE(MZ**INT(4.D0)) - 6.D0*B0(x, MW2, GaugeXiZ*MZ2)*DBLE(GaugeXiZ**INT(2.D0))*DBLE(MW**INT(4.D0))*DBLE(MZ**INT(4.D0))&
  & - 6.D0*A0(GaugeXiW*MW2)*DBLE(MZ**INT(6.D0)) + 48.D0*MW2*B0(x, MW2, MZ2)*DBLE(MZ**INT(6.D0)) + 48.D0*x*B0(x, MW2, MZ2)*DBLE(MZ&
  &**INT(6.D0)) + 12.D0*GaugeXiW*MW2*B0(x, GaugeXiW*MW2, MZ2)*DBLE(MZ**INT(6.D0)) - 48.D0*x*B0(x, GaugeXiW*MW2, MZ2)*DBLE(MZ**INT&
  &(6.D0)) + 6.D0*B0(x, MW2, MZ2)*DBLE(MZ**INT(8.D0)) - 6.D0*B0(x, GaugeXiW*MW2, MZ2)*DBLE(MZ**INT(8.D0)) + 6.D0*MW2*A0(MZ2)*(-13&
  &.D0*MZ2*x + 9.D0*MW2*(MZ2 + x) + DBLE(MW**INT(4.D0)) - 9.D0*DBLE(MZ**INT(4.D0)) - 1.D0*GaugeXiW*DBLE((MZ2 - 1.D0*x)**INT(2.D0)&
  &) - 9.D0*DBLE(x**INT(2.D0))) + 40.D0*MW2*MZ2*DBLE(x**INT(2.D0)) - 24.D0*GaugeXiW*MW2*MZ2*DBLE(x**INT(2.D0)) - 24.D0*GaugeXiZ*M&
  &W2*MZ2*DBLE(x**INT(2.D0)) + 54.D0*MZ2*A0(GaugeXiW*MW2)*DBLE(x**INT(2.D0)) + 6.D0*GaugeXiZ*MZ2*A0(GaugeXiW*MW2)*DBLE(x**INT(2.D&
  &0)) + 54.D0*MW2*A0(GaugeXiZ*MZ2)*DBLE(x**INT(2.D0)) + 6.D0*GaugeXiW*MW2*A0(GaugeXiZ*MZ2)*DBLE(x**INT(2.D0)) - 192.D0*MW2*MZ2*B&
  &0(x, MW2, MZ2)*DBLE(x**INT(2.D0)) - 12.D0*GaugeXiZ*MW2*MZ2*B0(x, MW2, GaugeXiZ*MZ2)*DBLE(x**INT(2.D0)) - 12.D0*GaugeXiW*MW2*MZ&
  &2*B0(x, GaugeXiW*MW2, MZ2)*DBLE(x**INT(2.D0)) - 12.D0*GaugeXiW*GaugeXiZ*MW2*MZ2*B0(x, GaugeXiW*MW2, GaugeXiZ*MZ2)*DBLE(x**INT(&
  &2.D0)) - 108.D0*B0(x, MW2, MZ2)*DBLE(MW**INT(4.D0))*DBLE(x**INT(2.D0)) + 108.D0*B0(x, MW2, GaugeXiZ*MZ2)*DBLE(MW**INT(4.D0))*D&
  &BLE(x**INT(2.D0)) - 6.D0*B0(x, GaugeXiW*MW2, MZ2)*DBLE(GaugeXiW**INT(2.D0))*DBLE(MW**INT(4.D0))*DBLE(x**INT(2.D0)) + 6.D0*B0(x&
  &, GaugeXiW*MW2, GaugeXiZ*MZ2)*DBLE(GaugeXiW**INT(2.D0))*DBLE(MW**INT(4.D0))*DBLE(x**INT(2.D0)) - 108.D0*B0(x, MW2, MZ2)*DBLE(M&
  &Z**INT(4.D0))*DBLE(x**INT(2.D0)) + 108.D0*B0(x, GaugeXiW*MW2, MZ2)*DBLE(MZ**INT(4.D0))*DBLE(x**INT(2.D0)) - 6.D0*B0(x, MW2, Ga&
  &ugeXiZ*MZ2)*DBLE(GaugeXiZ**INT(2.D0))*DBLE(MZ**INT(4.D0))*DBLE(x**INT(2.D0)) + 6.D0*B0(x, GaugeXiW*MW2, GaugeXiZ*MZ2)*DBLE(Gau&
  &geXiZ**INT(2.D0))*DBLE(MZ**INT(4.D0))*DBLE(x**INT(2.D0)) - 6.D0*MZ2*A0(MW2)*(-9.D0*MZ2*x + MW2*(-9.D0*MZ2 + 13.D0*x) + 9.D0*DB&
  &LE(MW**INT(4.D0)) - 1.D0*DBLE(MZ**INT(4.D0)) + GaugeXiZ*DBLE((MW2 - 1.D0*x)**INT(2.D0)) + 9.D0*DBLE(x**INT(2.D0))) + 48.D0*MW2&
  &*B0(x, MW2, MZ2)*DBLE(x**INT(3.D0)) + 48.D0*MZ2*B0(x, MW2, MZ2)*DBLE(x**INT(3.D0)) - 48.D0*MW2*B0(x, MW2, GaugeXiZ*MZ2)*DBLE(x&
  &**INT(3.D0)) + 12.D0*GaugeXiZ*MZ2*B0(x, MW2, GaugeXiZ*MZ2)*DBLE(x**INT(3.D0)) + 12.D0*GaugeXiW*MW2*B0(x, GaugeXiW*MW2, MZ2)*DB&
  &LE(x**INT(3.D0)) - 48.D0*MZ2*B0(x, GaugeXiW*MW2, MZ2)*DBLE(x**INT(3.D0)) - 12.D0*GaugeXiW*MW2*B0(x, GaugeXiW*MW2, GaugeXiZ*MZ2&
  &)*DBLE(x**INT(3.D0)) - 12.D0*GaugeXiZ*MZ2*B0(x, GaugeXiW*MW2, GaugeXiZ*MZ2)*DBLE(x**INT(3.D0)) + 6.D0*B0(x, MW2, MZ2)*DBLE(x**&
  &INT(4.D0)) - 6.D0*B0(x, MW2, GaugeXiZ*MZ2)*DBLE(x**INT(4.D0)) - 6.D0*B0(x, GaugeXiW*MW2, MZ2)*DBLE(x**INT(4.D0)) + 6.D0*B0(x, &
  &GaugeXiW*MW2, GaugeXiZ*MZ2)*DBLE(x**INT(4.D0))))/(MW2*MZ2*PI2*SW2*x)

 amplitudes(105) = (0.001736111111111111D0*EL2*MW2*(-3.D0*(-1.D0 + GaugeXiA)*A0(GaugeXiW*MW2) + 6.D0*(-1.D0*GaugeXiW*MW2 + 5.D0*x&
  & + GaugeXiA*(GaugeXiW*MW2 + x))* B0(x, 0.D0, GaugeXiW*MW2) - 1.D0*(-1.D0 + GaugeXiA)*(-2.D0*(GaugeXiW*MW2 + 2.D0*x) + 3.D0*C0M&
  &ine(DBLE(0.D0), DBLE(x), DBLE(x), DBLE(0.D0), DBLE(0.D0), DBLE(GaugeXiW*MW2))*DBLE((-1.D0*GaugeXiW*MW2 + x)**INT(2.D0)))))/(PI&
  &2*x)

 amplitudes(106) = (0.005208333333333333D0*EL2*MW2*SW2*(-2.D0*MZ2*x + 2.D0*GaugeXiZ*MZ2*x - 1.D0*(-1.D0 + GaugeXiZ)*MZ2*A0(GaugeX&
  &iW*MW2) - 1.D0*(-1.D0*GaugeXiW*MW2 + MZ2 + x)*A0(MZ2) - 1.D0*GaugeXiW*MW2*A0(GaugeXiZ*MZ2) + GaugeXiZ*MZ2*A0(GaugeXiZ*MZ2) + x&
  &*A0(GaugeXiZ*MZ2) - 2.D0*GaugeXiW*MW2*MZ2*B0(x, GaugeXiW*MW2, MZ2) - 2.D0*GaugeXiW*MW2*x*B0(x, GaugeXiW*MW2, MZ2) + 10.D0*MZ2*&
  &x*B0(x, GaugeXiW*MW2, MZ2) + 2.D0*GaugeXiW*GaugeXiZ*MW2*MZ2*B0(x, GaugeXiW*MW2, GaugeXiZ*MZ2) + 2.D0*GaugeXiW*MW2*x*B0(x, Gaug&
  &eXiW*MW2, GaugeXiZ*MZ2) + 2.D0*GaugeXiZ*MZ2*x*B0(x, GaugeXiW*MW2, GaugeXiZ*MZ2) + B0(x, GaugeXiW*MW2, MZ2)*DBLE(GaugeXiW**INT(&
  &2.D0))*DBLE(MW**INT(4.D0)) - 1.D0*B0(x, GaugeXiW*MW2, GaugeXiZ*MZ2)*DBLE(GaugeXiW**INT(2.D0))*DBLE(MW**INT(4.D0)) + B0(x, Gaug&
  &eXiW*MW2, MZ2)*DBLE(MZ**INT(4.D0)) - 1.D0*B0(x, GaugeXiW*MW2, GaugeXiZ*MZ2)*DBLE(GaugeXiZ**INT(2.D0))*DBLE(MZ**INT(4.D0)) + B0&
  &(x, GaugeXiW*MW2, MZ2)*DBLE(x**INT(2.D0)) - 1.D0*B0(x, GaugeXiW*MW2, GaugeXiZ*MZ2)*DBLE(x**INT(2.D0))))/(CW2*MZ2*PI2*x)

 amplitudes(107) = (0.005208333333333333D0*EL2*SBA2*(-2.D0*MW2*x + 2.D0*GaugeXiW*MW2*x - 1.D0*(-1.D0 + GaugeXiW)*MW2*A0(Mh02) + (&
  &Mh02 - 1.D0*MW2 - 1.D0*x)*A0(MW2) - 1.D0*Mh02*A0(GaugeXiW*MW2) + GaugeXiW*MW2*A0(GaugeXiW*MW2) + x*A0(GaugeXiW*MW2) - 2.D0*Mh0&
  &2*MW2*B0(x, Mh02, MW2) - 2.D0*Mh02*x*B0(x, Mh02, MW2) + 10.D0*MW2*x*B0(x, Mh02, MW2) + 2.D0*GaugeXiW*Mh02*MW2*B0(x, Mh02, Gaug&
  &eXiW*MW2) + 2.D0*Mh02*x*B0(x, Mh02, GaugeXiW*MW2) + 2.D0*GaugeXiW*MW2*x*B0(x, Mh02, GaugeXiW*MW2) + B0(x, Mh02, MW2)*DBLE(Mh0*&
  &*INT(4.D0)) - 1.D0*B0(x, Mh02, GaugeXiW*MW2)*DBLE(Mh0**INT(4.D0)) + B0(x, Mh02, MW2)*DBLE(MW**INT(4.D0)) - 1.D0*B0(x, Mh02, Ga&
  &ugeXiW*MW2)*DBLE(GaugeXiW**INT(2.D0))*DBLE(MW**INT(4.D0)) + B0(x, Mh02, MW2)*DBLE(x**INT(2.D0)) - 1.D0*B0(x, Mh02, GaugeXiW*MW&
  &2)*DBLE(x**INT(2.D0))))/(PI2*SW2*x)

 amplitudes(108) = (0.005208333333333333D0*CBA2*EL2*(-2.D0*MW2*x + 2.D0*GaugeXiW*MW2*x - 1.D0*(-1.D0 + GaugeXiW)*MW2*A0(MHH2) + (&
  &MHH2 - 1.D0*MW2 - 1.D0*x)*A0(MW2) - 1.D0*MHH2*A0(GaugeXiW*MW2) + GaugeXiW*MW2*A0(GaugeXiW*MW2) + x*A0(GaugeXiW*MW2) - 2.D0*MHH&
  &2*MW2*B0(x, MHH2, MW2) - 2.D0*MHH2*x*B0(x, MHH2, MW2) + 10.D0*MW2*x*B0(x, MHH2, MW2) + 2.D0*GaugeXiW*MHH2*MW2*B0(x, MHH2, Gaug&
  &eXiW*MW2) + 2.D0*MHH2*x*B0(x, MHH2, GaugeXiW*MW2) + 2.D0*GaugeXiW*MW2*x*B0(x, MHH2, GaugeXiW*MW2) + B0(x, MHH2, MW2)*DBLE(MHH*&
  &*INT(4.D0)) - 1.D0*B0(x, MHH2, GaugeXiW*MW2)*DBLE(MHH**INT(4.D0)) + B0(x, MHH2, MW2)*DBLE(MW**INT(4.D0)) - 1.D0*B0(x, MHH2, Ga&
  &ugeXiW*MW2)*DBLE(GaugeXiW**INT(2.D0))*DBLE(MW**INT(4.D0)) + B0(x, MHH2, MW2)*DBLE(x**INT(2.D0)) - 1.D0*B0(x, MHH2, GaugeXiW*MW&
  &2)*DBLE(x**INT(2.D0))))/(PI2*SW2*x)

  totalAmplitude = (0D0,0D0)
 do j=1,108
  totalAmplitude = totalAmplitude + amplitudes(j)
 end do
 SelfWpWpAlter = totalAmplitude
end function SelfWpWpAlter

