double complex function SelfGpGpAlter(x)
 use constants
 implicit none
#include "looptools.h"
 double precision, intent(in) :: x
 integer :: j
 double complex :: totalAmplitude
 double complex :: amplitudes(106)

 amplitudes(1) = (-0.0078125D0*(-1.D0*EL2*Mh02*S2B + CBA2*(EL2*(Mh02*S2A - 1.D0*MHH2*S2A - 2.D0*MHp2*S2B) + 4.D0*Lambda5*MW2*S2B*&
  &SW2))*A0(Mh02))/(MW2*PI2*S2B*SW2)

 amplitudes(2) = (0.0078125D0*(EL2*((-1.D0*Mh02*S2A + 2.D0*MHp2*S2B)*SBA2 + MHH2*(S2B + S2A*SBA2)) - 4.D0*Lambda5*MW2*S2B*SBA2*SW&
  &2)*A0(MHH2))/(MW2*PI2*S2B*SW2)

 amplitudes(3) = (-0.0078125D0*(CBA2*EL2*MHH2*S2B - 2.D0*CBA*EL2*MHH2*SAB + EL2*(-2.D0*MHp2*S2B + Mh02*SBA*(-2.D0*CAB + S2B*SBA))&
  & + 4.D0*Lambda5*MW2*S2B*SW2)* A0(MA02))/(MW2*PI2*S2B*SW2)

 amplitudes(4) = (0.0078125D0*EL2*(CBA2*MHH2 + Mh02*SBA2)*A0(GaugeXiZ*MZ2))/(MW2*PI2*SW2)

 amplitudes(5) = (-0.015625D0*(-1.D0*EL2*(MA02*S2B - 1.D0*Mh02*(S2A - 2.D0*CBA2*S2B) + MHH2*(S2A + 2.D0*S2B*SBA2)) + 4.D0*Lambda5&
  &*MW2*S2B*SW2)*A0(MHp2))/ (MW2*PI2*S2B*SW2)

 amplitudes(6) = (0.03125D0*EL2*(CBA2*MHH2 + Mh02*SBA2)*A0(GaugeXiW*MW2))/(MW2*PI2*SW2)

 amplitudes(7) = 0.D0

 amplitudes(8) = (-0.015625D0*EL2*(2.D0*MZ2 - 3.D0*A0(MZ2) - 1.D0*GaugeXiZ*A0(GaugeXiZ*MZ2))*DBLE((CW2 - 1.D0*SW2)**INT(2.D0)))/(&
  &CW2*PI2*SW2)

 amplitudes(9) = (0.03125D0*EL2*(-2.D0*MW2 + 3.D0*A0(MW2) + GaugeXiW*A0(GaugeXiW*MW2)))/(PI2*SW2)

 amplitudes(10) = (0.0625D0*EL2*ME2*SBA*Yuk4*A0(ME2))/(MW2*PI2*SW2)

 amplitudes(11) = (0.0625D0*EL2*MM2*SBA*Yuk4*A0(MM2))/(MW2*PI2*SW2)

 amplitudes(12) = (0.0625D0*EL2*ML2*SBA*Yuk4*A0(ML2))/(MW2*PI2*SW2)

 amplitudes(13) = (0.1875D0*CA*EL2*MU2*SBA*A0(MU2))/(MW2*PI2*SB*SW2)

 amplitudes(14) = (0.1875D0*CA*EL2*MC2*SBA*A0(MC2))/(MW2*PI2*SB*SW2)

 amplitudes(15) = (0.1875D0*CA*EL2*MT2*SBA*A0(MT2))/(MW2*PI2*SB*SW2)

 amplitudes(16) = (0.1875D0*EL2*MD2*SBA*Yuk1*A0(MD2))/(MW2*PI2*SW2)

 amplitudes(17) = (0.1875D0*EL2*MS2*SBA*Yuk1*A0(MS2))/(MW2*PI2*SW2)

 amplitudes(18) = (0.1875D0*EL2*MB2*SBA*Yuk1*A0(MB2))/(MW2*PI2*SW2)

 amplitudes(19) = (0.0625D0*CBA*EL2*ME2*Yuk5*A0(ME2))/(MW2*PI2*SW2)

 amplitudes(20) = (0.0625D0*CBA*EL2*MM2*Yuk5*A0(MM2))/(MW2*PI2*SW2)

 amplitudes(21) = (0.0625D0*CBA*EL2*ML2*Yuk5*A0(ML2))/(MW2*PI2*SW2)

 amplitudes(22) = (0.1875D0*CBA*EL2*MU2*SA*A0(MU2))/(MW2*PI2*SB*SW2)

 amplitudes(23) = (0.1875D0*CBA*EL2*MC2*SA*A0(MC2))/(MW2*PI2*SB*SW2)

 amplitudes(24) = (0.1875D0*CBA*EL2*MT2*SA*A0(MT2))/(MW2*PI2*SB*SW2)

 amplitudes(25) = (0.1875D0*CBA*EL2*MD2*Yuk2*A0(MD2))/(MW2*PI2*SW2)

 amplitudes(26) = (0.1875D0*CBA*EL2*MS2*Yuk2*A0(MS2))/(MW2*PI2*SW2)

 amplitudes(27) = (0.1875D0*CBA*EL2*MB2*Yuk2*A0(MB2))/(MW2*PI2*SW2)

 amplitudes(28) = (-0.0234375D0*SBA*(2.D0*CAB*EL2*Mh02 + EL2*Mh02*S2A*SBA - 4.D0*CAB*CBA2*Lambda5*MW2*SW2)*A0(Mh02))/(MW2*PI2*S2B&
  &*SW2)

 amplitudes(29) = (0.0078125D0*SBA2*(EL2*(Mh02 + 2.D0*MHH2)*S2A - 2.D0*Lambda5*MW2*(3.D0*S2A + S2B)*SW2)*A0(MHH2))/(MW2*PI2*S2B*S&
  &W2)

 amplitudes(30) = (0.0078125D0*SBA*(EL2*(-2.D0*MA02 + Mh02)*S2B*SBA + CAB*(-2.D0*EL2*Mh02 + 4.D0*Lambda5*MW2*SW2))*A0(MA02))/(MW2&
  &*PI2*S2B*SW2)

 amplitudes(31) = (-0.0078125D0*EL2*Mh02*SBA2*A0(GaugeXiZ*MZ2))/(MW2*PI2*SW2)

 amplitudes(32) = (0.015625D0*SBA*(EL2*(Mh02 - 2.D0*MHp2)*S2B*SBA + CAB*(-2.D0*EL2*Mh02 + 4.D0*Lambda5*MW2*SW2))*A0(MHp2))/(MW2*P&
  &I2*S2B*SW2)

 amplitudes(33) = (-0.015625D0*EL2*Mh02*SBA2*A0(GaugeXiW*MW2))/(MW2*PI2*SW2)

 amplitudes(34) = (-0.0078125D0*CBA2*(EL2*(2.D0*Mh02 + MHH2)*S2A + 2.D0*Lambda5*MW2*(-3.D0*S2A + S2B)*SW2)*A0(Mh02))/(MW2*PI2*S2B&
  &*SW2)

 amplitudes(35) = (0.0234375D0*CBA*(CBA*EL2*MHH2*S2A - 2.D0*EL2*MHH2*SAB + 4.D0*Lambda5*MW2*SAB*SBA2*SW2)*A0(MHH2))/(MW2*PI2*S2B*&
  &SW2)

 amplitudes(36) = (-0.0078125D0*CBA*(CBA*EL2*(2.D0*MA02 - 1.D0*MHH2)*S2B + 2.D0*SAB*(EL2*MHH2 - 2.D0*Lambda5*MW2*SW2))*A0(MA02))/&
  &(MW2*PI2*S2B*SW2)

 amplitudes(37) = (-0.0078125D0*CBA2*EL2*MHH2*A0(GaugeXiZ*MZ2))/(MW2*PI2*SW2)

 amplitudes(38) = (0.015625D0*CBA*(CBA*EL2*(MHH2 - 2.D0*MHp2)*S2B - 2.D0*EL2*MHH2*SAB + 4.D0*Lambda5*MW2*SAB*SW2)*A0(MHp2))/(MW2*&
  &PI2*S2B*SW2)

 amplitudes(39) = (-0.015625D0*CBA2*EL2*MHH2*A0(GaugeXiW*MW2))/(MW2*PI2*SW2)

 amplitudes(40) = (0.015625D0*EL2*GaugeXiZ*SBA2*A0(GaugeXiZ*MZ2))/(CW2*PI2*SW2)

 amplitudes(41) = (0.015625D0*EL2*GaugeXiW*SBA2*A0(GaugeXiW*MW2))/(PI2*SW2)

 amplitudes(42) = (0.015625D0*EL2*GaugeXiW*SBA2*A0(GaugeXiW*MW2))/(PI2*SW2)

 amplitudes(43) = (0.015625D0*CBA2*EL2*GaugeXiZ*A0(GaugeXiZ*MZ2))/(CW2*PI2*SW2)

 amplitudes(44) = (0.015625D0*CBA2*EL2*GaugeXiW*A0(GaugeXiW*MW2))/(PI2*SW2)

 amplitudes(45) = (0.015625D0*CBA2*EL2*GaugeXiW*A0(GaugeXiW*MW2))/(PI2*SW2)

 amplitudes(46) = (0.015625D0*EL2*SBA2*(2.D0*MZ2 - 3.D0*A0(MZ2) - 1.D0*GaugeXiZ*A0(GaugeXiZ*MZ2)))/(CW2*PI2*SW2)

 amplitudes(47) = (0.03125D0*EL2*SBA2*(2.D0*MW2 - 3.D0*A0(MW2) - 1.D0*GaugeXiW*A0(GaugeXiW*MW2)))/(PI2*SW2)

 amplitudes(48) = (0.015625D0*CBA2*EL2*(2.D0*MZ2 - 3.D0*A0(MZ2) - 1.D0*GaugeXiZ*A0(GaugeXiZ*MZ2)))/(CW2*PI2*SW2)

 amplitudes(49) = (0.03125D0*CBA2*EL2*(2.D0*MW2 - 3.D0*A0(MW2) - 1.D0*GaugeXiW*A0(GaugeXiW*MW2)))/(PI2*SW2)

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

 amplitudes(81) = (-0.03125D0*EL2*ME2*(A0(ME2) + (ME2 - 1.D0*x)*B0(x, 0.D0, ME2)))/(MW2*PI2*SW2)

 amplitudes(82) = (-0.03125D0*EL2*MM2*(A0(MM2) + (MM2 - 1.D0*x)*B0(x, 0.D0, MM2)))/(MW2*PI2*SW2)

 amplitudes(83) = (-0.03125D0*EL2*ML2*(A0(ML2) + (ML2 - 1.D0*x)*B0(x, 0.D0, ML2)))/(MW2*PI2*SW2)

 amplitudes(84) = (-0.09375D0*CKM11*CKMC11*EL2*((MD2 + MU2)*A0(MD2) + (MD2 + MU2)*A0(MU2) + B0(x, MD2, MU2)*(-1.D0*MU2*x - 1.D0*M&
  &D2*(2.D0*MU2 + x) + DBLE(MD**INT(4.D0)) + DBLE(MU**INT(4.D0)))))/(MW2*PI2*SW2)

 amplitudes(85) = (-0.09375D0*CKM21*CKMC21*EL2*((MC2 + MD2)*A0(MC2) + (MC2 + MD2)*A0(MD2) + B0(x, MC2, MD2)*(-1.D0*MD2*x - 1.D0*M&
  &C2*(2.D0*MD2 + x) + DBLE(MC**INT(4.D0)) + DBLE(MD**INT(4.D0)))))/(MW2*PI2*SW2)

 amplitudes(86) = (-0.09375D0*CKM31*CKMC31*EL2*((MD2 + MT2)*A0(MD2) + (MD2 + MT2)*A0(MT2) + B0(x, MD2, MT2)*(-1.D0*MT2*x - 1.D0*M&
  &D2*(2.D0*MT2 + x) + DBLE(MD**INT(4.D0)) + DBLE(MT**INT(4.D0)))))/(MW2*PI2*SW2)

 amplitudes(87) = (-0.09375D0*CKM12*CKMC12*EL2*((MS2 + MU2)*A0(MS2) + (MS2 + MU2)*A0(MU2) + B0(x, MS2, MU2)*(-1.D0*MU2*x - 1.D0*M&
  &S2*(2.D0*MU2 + x) + DBLE(MS**INT(4.D0)) + DBLE(MU**INT(4.D0)))))/(MW2*PI2*SW2)

 amplitudes(88) = (-0.09375D0*CKM22*CKMC22*EL2*((MC2 + MS2)*A0(MC2) + (MC2 + MS2)*A0(MS2) + B0(x, MC2, MS2)*(-1.D0*MS2*x - 1.D0*M&
  &C2*(2.D0*MS2 + x) + DBLE(MC**INT(4.D0)) + DBLE(MS**INT(4.D0)))))/(MW2*PI2*SW2)

 amplitudes(89) = (-0.09375D0*CKM32*CKMC32*EL2*((MS2 + MT2)*A0(MS2) + (MS2 + MT2)*A0(MT2) + B0(x, MS2, MT2)*(-1.D0*MT2*x - 1.D0*M&
  &S2*(2.D0*MT2 + x) + DBLE(MS**INT(4.D0)) + DBLE(MT**INT(4.D0)))))/(MW2*PI2*SW2)

 amplitudes(90) = (-0.09375D0*CKM13*CKMC13*EL2*((MB2 + MU2)*A0(MB2) + (MB2 + MU2)*A0(MU2) + B0(x, MB2, MU2)*(-1.D0*MU2*x - 1.D0*M&
  &B2*(2.D0*MU2 + x) + DBLE(MB**INT(4.D0)) + DBLE(MU**INT(4.D0)))))/(MW2*PI2*SW2)

 amplitudes(91) = (-0.09375D0*CKM23*CKMC23*EL2*((MB2 + MC2)*A0(MB2) + (MB2 + MC2)*A0(MC2) + B0(x, MB2, MC2)*(-1.D0*MC2*x - 1.D0*M&
  &B2*(2.D0*MC2 + x) + DBLE(MB**INT(4.D0)) + DBLE(MC**INT(4.D0)))))/(MW2*PI2*SW2)

 amplitudes(92) = (-0.09375D0*CKM33*CKMC33*EL2*((MB2 + MT2)*A0(MB2) + (MB2 + MT2)*A0(MT2) + B0(x, MB2, MT2)*(-1.D0*MT2*x - 1.D0*M&
  &B2*(2.D0*MT2 + x) + DBLE(MB**INT(4.D0)) + DBLE(MT**INT(4.D0)))))/(MW2*PI2*SW2)

 amplitudes(93) = (0.015625D0*CBA2*EL2*B0(x, Mh02, MHp2)*DBLE((Mh02 - 1.D0*MHp2)**INT(2.D0)))/(MW2*PI2*SW2)

 amplitudes(94) = (0.015625D0*EL2*SBA2*B0(x, MHH2, MHp2)*DBLE((MHH2 - 1.D0*MHp2)**INT(2.D0)))/(MW2*PI2*SW2)

 amplitudes(95) = (0.015625D0*EL2*B0(x, MA02, MHp2)*DBLE((MA02 - 1.D0*MHp2)**INT(2.D0)))/(MW2*PI2*SW2)

 amplitudes(96) = (0.015625D0*EL2*SBA2*B0(x, Mh02, GaugeXiW*MW2)*DBLE(Mh0**INT(4.D0)))/(MW2*PI2*SW2)

 amplitudes(97) = (0.015625D0*CBA2*EL2*B0(x, MHH2, GaugeXiW*MW2)*DBLE(MHH**INT(4.D0)))/(MW2*PI2*SW2)

 amplitudes(98) = (0.015625D0*EL2*GaugeXiW*GaugeXiZ*MW2*(CW2 - 1.D0*SW2)*B0(x, GaugeXiW*MW2, GaugeXiZ*MZ2))/(CW2*PI2*SW2)

 amplitudes(99) = (0.015625D0*EL2*GaugeXiW*GaugeXiZ*MW2*(CW2 - 1.D0*SW2)*B0(x, GaugeXiW*MW2, GaugeXiZ*MZ2))/(CW2*PI2*SW2)

 amplitudes(100) = (0.015625D0*EL2*(-8.D0*MW2 - 1.D0*(-1.D0 + GaugeXiA)*A0(MW2) + (-1.D0 + GaugeXiA)*A0(GaugeXiW*MW2) + 10.D0*MW2&
  &*B0(x, 0.D0, MW2) + 2.D0*GaugeXiA*MW2*B0(x, 0.D0, MW2) - 2.D0*x*B0(x, 0.D0, MW2) + 2.D0*GaugeXiA*x*B0(x, 0.D0, MW2) + 2.D0*Gau&
  &geXiW*MW2*B0(x, 0.D0, GaugeXiW*MW2) + 2.D0*GaugeXiA*GaugeXiW*MW2*B0(x, 0.D0, GaugeXiW*MW2) + 2.D0*x*B0(x, 0.D0, GaugeXiW*MW2) &
  &- 2.D0*GaugeXiA*x*B0(x, 0.D0, GaugeXiW*MW2) - 2.D0*MW2*x*C0Mine(DBLE(0.D0), DBLE(x), DBLE(x), DBLE(0.D0), DBLE(0.D0), DBLE(MW2&
  &)) + 2.D0*GaugeXiA*MW2*x*C0Mine(DBLE(0.D0), DBLE(x), DBLE(x), DBLE(0.D0), DBLE(0.D0), DBLE(MW2)) + 2.D0*GaugeXiW*MW2*x*C0Mine(&
  &DBLE(0.D0), DBLE(x), DBLE(x), DBLE(0.D0), DBLE(0.D0), DBLE(GaugeXiW*MW2)) - 2.D0*GaugeXiA*GaugeXiW*MW2*x*C0Mine(DBLE(0.D0), DB&
  &LE(x), DBLE(x), DBLE(0.D0), DBLE(0.D0), DBLE(GaugeXiW*MW2)) + C0Mine(DBLE(0.D0), DBLE(x), DBLE(x), DBLE(0.D0), DBLE(0.D0), DBL&
  &E(MW2))*DBLE(MW**INT(4.D0)) - 1.D0*GaugeXiA*C0Mine(DBLE(0.D0), DBLE(x), DBLE(x), DBLE(0.D0), DBLE(0.D0), DBLE(MW2))*DBLE(MW**I&
  &NT(4.D0)) - 1.D0*C0Mine(DBLE(0.D0), DBLE(x), DBLE(x), DBLE(0.D0), DBLE(0.D0), DBLE(GaugeXiW*MW2))*DBLE(GaugeXiW**INT(2.D0))*DB&
  &LE(MW**INT(4.D0)) + GaugeXiA*C0Mine(DBLE(0.D0), DBLE(x), DBLE(x), DBLE(0.D0), DBLE(0.D0), DBLE(GaugeXiW*MW2))*DBLE(GaugeXiW**I&
  &NT(2.D0))*DBLE(MW**INT(4.D0)) + C0Mine(DBLE(0.D0), DBLE(x), DBLE(x), DBLE(0.D0), DBLE(0.D0), DBLE(MW2))*DBLE(x**INT(2.D0)) - 1&
  &.D0*GaugeXiA*C0Mine(DBLE(0.D0), DBLE(x), DBLE(x), DBLE(0.D0), DBLE(0.D0), DBLE(MW2))*DBLE(x**INT(2.D0)) - 1.D0*C0Mine(DBLE(0.D&
  &0), DBLE(x), DBLE(x), DBLE(0.D0), DBLE(0.D0), DBLE(GaugeXiW*MW2))*DBLE(x**INT(2.D0)) + GaugeXiA*C0Mine(DBLE(0.D0), DBLE(x), DB&
  &LE(x), DBLE(0.D0), DBLE(0.D0), DBLE(GaugeXiW*MW2))*DBLE(x**INT(2.D0))))/PI2

 amplitudes(101) = (0.015625D0*EL2*SW2*(-8.D0*MW2*MZ2 - 1.D0*(-1.D0 + GaugeXiZ)*MZ2*A0(MW2) - 1.D0*MZ2*A0(GaugeXiW*MW2) + GaugeXi&
  &Z*MZ2*A0(GaugeXiW*MW2) - 1.D0*(-1.D0 + GaugeXiW)*MW2*A0(MZ2) - 1.D0*MW2*A0(GaugeXiZ*MZ2) + GaugeXiW*MW2*A0(GaugeXiZ*MZ2) + 10.&
  &D0*MW2*MZ2*B0(x, MW2, MZ2) - 2.D0*MW2*x*B0(x, MW2, MZ2) - 2.D0*MZ2*x*B0(x, MW2, MZ2) + 2.D0*GaugeXiZ*MW2*MZ2*B0(x, MW2, GaugeX&
  &iZ*MZ2) + 2.D0*MW2*x*B0(x, MW2, GaugeXiZ*MZ2) + 2.D0*GaugeXiZ*MZ2*x*B0(x, MW2, GaugeXiZ*MZ2) + 2.D0*GaugeXiW*MW2*MZ2*B0(x, Gau&
  &geXiW*MW2, MZ2) + 2.D0*GaugeXiW*MW2*x*B0(x, GaugeXiW*MW2, MZ2) + 2.D0*MZ2*x*B0(x, GaugeXiW*MW2, MZ2) + 2.D0*GaugeXiW*GaugeXiZ*&
  &MW2*MZ2*B0(x, GaugeXiW*MW2, GaugeXiZ*MZ2) - 2.D0*GaugeXiW*MW2*x*B0(x, GaugeXiW*MW2, GaugeXiZ*MZ2) - 2.D0*GaugeXiZ*MZ2*x*B0(x, &
  &GaugeXiW*MW2, GaugeXiZ*MZ2) + B0(x, MW2, MZ2)*DBLE(MW**INT(4.D0)) - 1.D0*B0(x, MW2, GaugeXiZ*MZ2)*DBLE(MW**INT(4.D0)) - 1.D0*B&
  &0(x, GaugeXiW*MW2, MZ2)*DBLE(GaugeXiW**INT(2.D0))*DBLE(MW**INT(4.D0)) + B0(x, GaugeXiW*MW2, GaugeXiZ*MZ2)*DBLE(GaugeXiW**INT(2&
  &.D0))*DBLE(MW**INT(4.D0)) + B0(x, MW2, MZ2)*DBLE(MZ**INT(4.D0)) - 1.D0*B0(x, GaugeXiW*MW2, MZ2)*DBLE(MZ**INT(4.D0)) - 1.D0*B0(&
  &x, MW2, GaugeXiZ*MZ2)*DBLE(GaugeXiZ**INT(2.D0))*DBLE(MZ**INT(4.D0)) + B0(x, GaugeXiW*MW2, GaugeXiZ*MZ2)*DBLE(GaugeXiZ**INT(2.D&
  &0))*DBLE(MZ**INT(4.D0)) + B0(x, MW2, MZ2)*DBLE(x**INT(2.D0)) - 1.D0*B0(x, MW2, GaugeXiZ*MZ2)*DBLE(x**INT(2.D0)) - 1.D0*B0(x, G&
  &augeXiW*MW2, MZ2)*DBLE(x**INT(2.D0)) + B0(x, GaugeXiW*MW2, GaugeXiZ*MZ2)*DBLE(x**INT(2.D0))))/(CW2*MZ2*PI2)

 amplitudes(102) = (0.0625D0*EL2*(A0(GaugeXiW*MW2) - 2.D0*(GaugeXiW*MW2 + x)*B0(x, 0.D0, GaugeXiW*MW2) - 1.D0*(-1.D0 + GaugeXiA)*&
  &C0Mine(DBLE(0.D0), DBLE(x), DBLE(x), DBLE(0.D0), DBLE(0.D0), DBLE(GaugeXiW*MW2))* DBLE((-1.D0*GaugeXiW*MW2 + x)**INT(2.D0))))/&
  &PI2

 amplitudes(103) = (0.015625D0*EL2*DBLE((CW2 - 1.D0*SW2)**INT(2.D0))*(MZ2*A0(GaugeXiW*MW2) - 1.D0*(-1.D0*GaugeXiW*MW2 + MZ2 + x)*&
  &A0(MZ2) - 1.D0*GaugeXiW*MW2*A0(GaugeXiZ*MZ2) - 1.D0*GaugeXiZ*MZ2*A0(GaugeXiZ*MZ2) + x*A0(GaugeXiZ*MZ2) - 2.D0*GaugeXiW*MW2*MZ2&
  &*B0(x, GaugeXiW*MW2, MZ2) - 2.D0*GaugeXiW*MW2*x*B0(x, GaugeXiW*MW2, MZ2) - 2.D0*MZ2*x*B0(x, GaugeXiW*MW2, MZ2) + 2.D0*GaugeXiW&
  &*MW2*x*B0(x, GaugeXiW*MW2, GaugeXiZ*MZ2) + B0(x, GaugeXiW*MW2, MZ2)*DBLE(GaugeXiW**INT(2.D0))*DBLE(MW**INT(4.D0)) - 1.D0*B0(x,&
  & GaugeXiW*MW2, GaugeXiZ*MZ2)*DBLE(GaugeXiW**INT(2.D0))*DBLE(MW**INT(4.D0)) + B0(x, GaugeXiW*MW2, MZ2)*DBLE(MZ**INT(4.D0)) + B0&
  &(x, GaugeXiW*MW2, MZ2)*DBLE(x**INT(2.D0)) - 1.D0*B0(x, GaugeXiW*MW2, GaugeXiZ*MZ2)*DBLE(x**INT(2.D0))))/(CW2*MZ2*PI2*SW2)

 amplitudes(104) = (0.015625D0*EL2*SBA2*(MW2*A0(Mh02) + (Mh02 - 1.D0*MW2 - 1.D0*x)*A0(MW2) - 1.D0*Mh02*A0(GaugeXiW*MW2) - 1.D0*Ga&
  &ugeXiW*MW2*A0(GaugeXiW*MW2) + x*A0(GaugeXiW*MW2) - 2.D0*Mh02*MW2*B0(x, Mh02, MW2) - 2.D0*Mh02*x*B0(x, Mh02, MW2) - 2.D0*MW2*x*&
  &B0(x, Mh02, MW2) + 2.D0*Mh02*x*B0(x, Mh02, GaugeXiW*MW2) + B0(x, Mh02, MW2)*DBLE(Mh0**INT(4.D0)) - 1.D0*B0(x, Mh02, GaugeXiW*M&
  &W2)*DBLE(Mh0**INT(4.D0)) + B0(x, Mh02, MW2)*DBLE(MW**INT(4.D0)) + B0(x, Mh02, MW2)*DBLE(x**INT(2.D0)) - 1.D0*B0(x, Mh02, Gauge&
  &XiW*MW2)*DBLE(x**INT(2.D0))))/(MW2*PI2*SW2)

 amplitudes(105) = (0.015625D0*CBA2*EL2*(MW2*A0(MHH2) + (MHH2 - 1.D0*MW2 - 1.D0*x)*A0(MW2) - 1.D0*MHH2*A0(GaugeXiW*MW2) - 1.D0*Ga&
  &ugeXiW*MW2*A0(GaugeXiW*MW2) + x*A0(GaugeXiW*MW2) - 2.D0*MHH2*MW2*B0(x, MHH2, MW2) - 2.D0*MHH2*x*B0(x, MHH2, MW2) - 2.D0*MW2*x*&
  &B0(x, MHH2, MW2) + 2.D0*MHH2*x*B0(x, MHH2, GaugeXiW*MW2) + B0(x, MHH2, MW2)*DBLE(MHH**INT(4.D0)) - 1.D0*B0(x, MHH2, GaugeXiW*M&
  &W2)*DBLE(MHH**INT(4.D0)) + B0(x, MHH2, MW2)*DBLE(MW**INT(4.D0)) + B0(x, MHH2, MW2)*DBLE(x**INT(2.D0)) - 1.D0*B0(x, MHH2, Gauge&
  &XiW*MW2)*DBLE(x**INT(2.D0))))/(MW2*PI2*SW2)

 amplitudes(106) = (0.015625D0*EL2*(-1.D0*(MW2 - 1.D0*GaugeXiZ*MZ2 + x)*A0(MW2) + (-1.D0*GaugeXiW*MW2 - 1.D0*GaugeXiZ*MZ2 + x)*A0&
  &(GaugeXiW*MW2) + MW2*A0(GaugeXiZ*MZ2) - 2.D0*GaugeXiZ*MW2*MZ2*B0(x, MW2, GaugeXiZ*MZ2) - 2.D0*MW2*x*B0(x, MW2, GaugeXiZ*MZ2) -&
  & 2.D0*GaugeXiZ*MZ2*x*B0(x, MW2, GaugeXiZ*MZ2) + 2.D0*GaugeXiZ*MZ2*x*B0(x, GaugeXiW*MW2, GaugeXiZ*MZ2) + B0(x, MW2, GaugeXiZ*MZ&
  &2)*DBLE(MW**INT(4.D0)) + B0(x, MW2, GaugeXiZ*MZ2)*DBLE(GaugeXiZ**INT(2.D0))*DBLE(MZ**INT(4.D0)) - 1.D0*B0(x, GaugeXiW*MW2, Gau&
  &geXiZ*MZ2)*DBLE(GaugeXiZ**INT(2.D0))*DBLE(MZ**INT(4.D0)) + B0(x, MW2, GaugeXiZ*MZ2)*DBLE(x**INT(2.D0)) - 1.D0*B0(x, GaugeXiW*M&
  &W2, GaugeXiZ*MZ2)*DBLE(x**INT(2.D0))))/(MW2*PI2*SW2)

  totalAmplitude = (0D0,0D0)
 do j=1,106
  totalAmplitude = totalAmplitude + amplitudes(j)
 end do
 SelfGpGpAlter = totalAmplitude
end function SelfGpGpAlter

