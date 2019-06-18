double complex function SelfHHh0Alter(x)
 use constants
 implicit none
#include "looptools.h"
 double precision, intent(in) :: x
 integer :: j
 double complex :: totalAmplitude
 double complex :: amplitudes(76)

 amplitudes(1) = (0.0234375D0*CBA*S2A*(EL2*(Mh02 - 1.D0*MHH2)*S2A*SBA + 2.D0*CAB*(EL2*Mh02 - 2.D0*Lambda5*MW2*SW2))*A0(Mh02))/(MW&
  &2*PI2*S2B2*SW2)

 amplitudes(2) = (0.0234375D0*S2A*SBA*(CBA*EL2*(-1.D0*Mh02 + MHH2)*S2A - 2.D0*EL2*MHH2*SAB + 4.D0*Lambda5*MW2*SAB*SW2)*A0(MHH2))/&
  &(MW2*PI2*S2B2*SW2)

 amplitudes(3) = (0.0078125D0*(2.D0*CAB*CBA*EL2*Mh02*S2A - 2.D0*S2A*(EL2*MHH2*SAB*SBA + 2.D0*C2B*Lambda5*MW2*SW2) + CBA*S2B*SBA*(&
  &EL2*(-1.D0*Mh02*S2A + MHH2*S2A + 2.D0*MA02*S2B) - 4.D0*Lambda5*MW2*S2B*SW2))*A0(MA02))/(MW2*PI2*S2B2*SW2)

 amplitudes(4) = (0.0078125D0*CBA*SBA*(EL2*(Mh02*S2A - 1.D0*MHH2*S2A - 2.D0*MA02*S2B) + 4.D0*Lambda5*MW2*S2B*SW2)*A0(GaugeXiZ*MZ2&
  &))/(MW2*PI2*S2B*SW2)

 amplitudes(5) = (0.015625D0*(2.D0*CAB*CBA*EL2*Mh02*S2A - 2.D0*S2A*(EL2*MHH2*SAB*SBA + 2.D0*C2B*Lambda5*MW2*SW2) + CBA*S2B*SBA*(E&
  &L2*(-1.D0*Mh02*S2A + MHH2*S2A + 2.D0*MHp2*S2B) - 4.D0*Lambda5*MW2*S2B*SW2))*A0(MHp2))/(MW2*PI2*S2B2*SW2)

 amplitudes(6) = (0.015625D0*CBA*SBA*(EL2*(Mh02*S2A - 1.D0*MHH2*S2A - 2.D0*MHp2*S2B) + 4.D0*Lambda5*MW2*S2B*SW2)*A0(GaugeXiW*MW2)&
  &)/(MW2*PI2*S2B*SW2)

 amplitudes(7) = (0.0625D0*CBA*ME2*(EL2*(2.D0*Mh02 + MHH2)*S2A + 2.D0*Lambda5*MW2*(-3.D0*S2A + S2B)*SW2)*Yuk4*A0(ME2))/(Mh02*MW2*&
  &PI2*S2B*SW2)

 amplitudes(8) = (0.0625D0*CBA*MM2*(EL2*(2.D0*Mh02 + MHH2)*S2A + 2.D0*Lambda5*MW2*(-3.D0*S2A + S2B)*SW2)*Yuk4*A0(MM2))/(Mh02*MW2*&
  &PI2*S2B*SW2)

 amplitudes(9) = (0.0625D0*CBA*ML2*(EL2*(2.D0*Mh02 + MHH2)*S2A + 2.D0*Lambda5*MW2*(-3.D0*S2A + S2B)*SW2)*Yuk4*A0(ML2))/(Mh02*MW2*&
  &PI2*S2B*SW2)

 amplitudes(10) = (0.1875D0*CA*CBA*MU2*(EL2*(2.D0*Mh02 + MHH2)*S2A + 2.D0*Lambda5*MW2*(-3.D0*S2A + S2B)*SW2)*A0(MU2))/(Mh02*MW2*P&
  &I2*S2B*SB*SW2)

 amplitudes(11) = (0.1875D0*CA*CBA*MC2*(EL2*(2.D0*Mh02 + MHH2)*S2A + 2.D0*Lambda5*MW2*(-3.D0*S2A + S2B)*SW2)*A0(MC2))/(Mh02*MW2*P&
  &I2*S2B*SB*SW2)

 amplitudes(12) = (0.1875D0*CA*CBA*MT2*(EL2*(2.D0*Mh02 + MHH2)*S2A + 2.D0*Lambda5*MW2*(-3.D0*S2A + S2B)*SW2)*A0(MT2))/(Mh02*MW2*P&
  &I2*S2B*SB*SW2)

 amplitudes(13) = (0.1875D0*CBA*MD2*(EL2*(2.D0*Mh02 + MHH2)*S2A + 2.D0*Lambda5*MW2*(-3.D0*S2A + S2B)*SW2)*Yuk1*A0(MD2))/(Mh02*MW2&
  &*PI2*S2B*SW2)

 amplitudes(14) = (0.1875D0*CBA*MS2*(EL2*(2.D0*Mh02 + MHH2)*S2A + 2.D0*Lambda5*MW2*(-3.D0*S2A + S2B)*SW2)*Yuk1*A0(MS2))/(Mh02*MW2&
  &*PI2*S2B*SW2)

 amplitudes(15) = (0.1875D0*CBA*MB2*(EL2*(2.D0*Mh02 + MHH2)*S2A + 2.D0*Lambda5*MW2*(-3.D0*S2A + S2B)*SW2)*Yuk1*A0(MB2))/(Mh02*MW2&
  &*PI2*S2B*SW2)

 amplitudes(16) = (-0.0625D0*ME2*SBA*(EL2*(Mh02 + 2.D0*MHH2)*S2A - 2.D0*Lambda5*MW2*(3.D0*S2A + S2B)*SW2)*Yuk5*A0(ME2))/(MHH2*MW2&
  &*PI2*S2B*SW2)

 amplitudes(17) = (-0.0625D0*MM2*SBA*(EL2*(Mh02 + 2.D0*MHH2)*S2A - 2.D0*Lambda5*MW2*(3.D0*S2A + S2B)*SW2)*Yuk5*A0(MM2))/(MHH2*MW2&
  &*PI2*S2B*SW2)

 amplitudes(18) = (-0.0625D0*ML2*SBA*(EL2*(Mh02 + 2.D0*MHH2)*S2A - 2.D0*Lambda5*MW2*(3.D0*S2A + S2B)*SW2)*Yuk5*A0(ML2))/(MHH2*MW2&
  &*PI2*S2B*SW2)

 amplitudes(19) = (-0.1875D0*MU2*SA*SBA*(EL2*(Mh02 + 2.D0*MHH2)*S2A - 2.D0*Lambda5*MW2*(3.D0*S2A + S2B)*SW2)*A0(MU2))/(MHH2*MW2*P&
  &I2*S2B*SB*SW2)

 amplitudes(20) = (-0.1875D0*MC2*SA*SBA*(EL2*(Mh02 + 2.D0*MHH2)*S2A - 2.D0*Lambda5*MW2*(3.D0*S2A + S2B)*SW2)*A0(MC2))/(MHH2*MW2*P&
  &I2*S2B*SB*SW2)

 amplitudes(21) = (-0.1875D0*MT2*SA*SBA*(EL2*(Mh02 + 2.D0*MHH2)*S2A - 2.D0*Lambda5*MW2*(3.D0*S2A + S2B)*SW2)*A0(MT2))/(MHH2*MW2*P&
  &I2*S2B*SB*SW2)

 amplitudes(22) = (-0.1875D0*MD2*SBA*(EL2*(Mh02 + 2.D0*MHH2)*S2A - 2.D0*Lambda5*MW2*(3.D0*S2A + S2B)*SW2)*Yuk2*A0(MD2))/(MHH2*MW2&
  &*PI2*S2B*SW2)

 amplitudes(23) = (-0.1875D0*MS2*SBA*(EL2*(Mh02 + 2.D0*MHH2)*S2A - 2.D0*Lambda5*MW2*(3.D0*S2A + S2B)*SW2)*Yuk2*A0(MS2))/(MHH2*MW2&
  &*PI2*S2B*SW2)

 amplitudes(24) = (-0.1875D0*MB2*SBA*(EL2*(Mh02 + 2.D0*MHH2)*S2A - 2.D0*Lambda5*MW2*(3.D0*S2A + S2B)*SW2)*Yuk2*A0(MB2))/(MHH2*MW2&
  &*PI2*S2B*SW2)

 amplitudes(25) = (0.0234375D0*CBA*(-2.D0*CAB*EL2*Mh02 - 1.D0*EL2*Mh02*S2A*SBA + 4.D0*CAB*CBA2*Lambda5*MW2*SW2)* (EL2*(2.D0*Mh02 &
  &+ MHH2)*S2A + 2.D0*Lambda5*MW2*(-3.D0*S2A + S2B)*SW2)*A0(Mh02))/(EL2*Mh02*MW2*PI2*S2B2*SW2)

 amplitudes(26) = (0.0078125D0*CBA*SBA*(EL2*(2.D0*Mh02 + MHH2)*S2A + 2.D0*Lambda5*MW2*(-3.D0*S2A + S2B)*SW2)* (EL2*(Mh02 + 2.D0*M&
  &HH2)*S2A - 2.D0*Lambda5*MW2*(3.D0*S2A + S2B)*SW2)*A0(MHH2))/(EL2*Mh02*MW2*PI2*S2B2*SW2)

 amplitudes(27) = (0.0078125D0*CBA*(EL2*(2.D0*Mh02 + MHH2)*S2A + 2.D0*Lambda5*MW2*(-3.D0*S2A + S2B)*SW2)* (EL2*(-2.D0*MA02 + Mh02&
  &)*S2B*SBA + CAB*(-2.D0*EL2*Mh02 + 4.D0*Lambda5*MW2*SW2))*A0(MA02))/(EL2*Mh02*MW2*PI2*S2B2*SW2)

 amplitudes(28) = (-0.0078125D0*CBA*SBA*(EL2*(2.D0*Mh02 + MHH2)*S2A + 2.D0*Lambda5*MW2*(-3.D0*S2A + S2B)*SW2)*A0(GaugeXiZ*MZ2))/(&
  &MW2*PI2*S2B*SW2)

 amplitudes(29) = (0.015625D0*CBA*(EL2*(2.D0*Mh02 + MHH2)*S2A + 2.D0*Lambda5*MW2*(-3.D0*S2A + S2B)*SW2)* (EL2*(Mh02 - 2.D0*MHp2)*&
  &S2B*SBA + CAB*(-2.D0*EL2*Mh02 + 4.D0*Lambda5*MW2*SW2))*A0(MHp2))/(EL2*Mh02*MW2*PI2*S2B2*SW2)

 amplitudes(30) = (-0.015625D0*CBA*SBA*(EL2*(2.D0*Mh02 + MHH2)*S2A + 2.D0*Lambda5*MW2*(-3.D0*S2A + S2B)*SW2)*A0(GaugeXiW*MW2))/(M&
  &W2*PI2*S2B*SW2)

 amplitudes(31) = (0.0078125D0*CBA*SBA*(EL2*(2.D0*Mh02 + MHH2)*S2A + 2.D0*Lambda5*MW2*(-3.D0*S2A + S2B)*SW2)* (EL2*(Mh02 + 2.D0*M&
  &HH2)*S2A - 2.D0*Lambda5*MW2*(3.D0*S2A + S2B)*SW2)*A0(Mh02))/(EL2*MHH2*MW2*PI2*S2B2*SW2)

 amplitudes(32) = (-0.0234375D0*SBA*(EL2*(Mh02 + 2.D0*MHH2)*S2A - 2.D0*Lambda5*MW2*(3.D0*S2A + S2B)*SW2)*(CBA*EL2*MHH2*S2A - 2.D0&
  &*EL2*MHH2*SAB + 4.D0*Lambda5*MW2*SAB*SBA2*SW2)*A0(MHH2))/(EL2*MHH2*MW2*PI2*S2B2*SW2)

 amplitudes(33) = (-0.0078125D0*SBA*(EL2*(Mh02 + 2.D0*MHH2)*S2A - 2.D0*Lambda5*MW2*(3.D0*S2A + S2B)*SW2)*(CBA*EL2*(-2.D0*MA02 + M&
  &HH2)*S2B - 2.D0*EL2*MHH2*SAB + 4.D0*Lambda5*MW2*SAB*SW2)*A0(MA02))/(EL2*MHH2*MW2*PI2*S2B2*SW2)

 amplitudes(34) = (0.0078125D0*CBA*SBA*(EL2*(Mh02 + 2.D0*MHH2)*S2A - 2.D0*Lambda5*MW2*(3.D0*S2A + S2B)*SW2)*A0(GaugeXiZ*MZ2))/(MW&
  &2*PI2*S2B*SW2)

 amplitudes(35) = (-0.015625D0*SBA*(EL2*(Mh02 + 2.D0*MHH2)*S2A - 2.D0*Lambda5*MW2*(3.D0*S2A + S2B)*SW2)*(CBA*EL2*(MHH2 - 2.D0*MHp&
  &2)*S2B - 2.D0*EL2*MHH2*SAB + 4.D0*Lambda5*MW2*SAB*SW2)*A0(MHp2))/(EL2*MHH2*MW2*PI2*S2B2*SW2)

 amplitudes(36) = (0.015625D0*CBA*SBA*(EL2*(Mh02 + 2.D0*MHH2)*S2A - 2.D0*Lambda5*MW2*(3.D0*S2A + S2B)*SW2)*A0(GaugeXiW*MW2))/(MW2&
  &*PI2*S2B*SW2)

 amplitudes(37) = (0.015625D0*CBA*GaugeXiZ*SBA*(EL2*(2.D0*Mh02 + MHH2)*S2A + 2.D0*Lambda5*MW2*(-3.D0*S2A + S2B)*SW2)*A0(GaugeXiZ*&
  &MZ2))/(CW2*Mh02*PI2*S2B*SW2)

 amplitudes(38) = (0.015625D0*CBA*GaugeXiW*SBA*(EL2*(2.D0*Mh02 + MHH2)*S2A + 2.D0*Lambda5*MW2*(-3.D0*S2A + S2B)*SW2)*A0(GaugeXiW*&
  &MW2))/(Mh02*PI2*S2B*SW2)

 amplitudes(39) = (0.015625D0*CBA*GaugeXiW*SBA*(EL2*(2.D0*Mh02 + MHH2)*S2A + 2.D0*Lambda5*MW2*(-3.D0*S2A + S2B)*SW2)*A0(GaugeXiW*&
  &MW2))/(Mh02*PI2*S2B*SW2)

 amplitudes(40) = (-0.015625D0*CBA*GaugeXiZ*SBA*(EL2*(Mh02 + 2.D0*MHH2)*S2A - 2.D0*Lambda5*MW2*(3.D0*S2A + S2B)*SW2)*A0(GaugeXiZ*&
  &MZ2))/(CW2*MHH2*PI2*S2B*SW2)

 amplitudes(41) = (-0.015625D0*CBA*GaugeXiW*SBA*(EL2*(Mh02 + 2.D0*MHH2)*S2A - 2.D0*Lambda5*MW2*(3.D0*S2A + S2B)*SW2)*A0(GaugeXiW*&
  &MW2))/(MHH2*PI2*S2B*SW2)

 amplitudes(42) = (-0.015625D0*CBA*GaugeXiW*SBA*(EL2*(Mh02 + 2.D0*MHH2)*S2A - 2.D0*Lambda5*MW2*(3.D0*S2A + S2B)*SW2)*A0(GaugeXiW*&
  &MW2))/(MHH2*PI2*S2B*SW2)

 amplitudes(43) = (0.015625D0*CBA*SBA*(EL2*(2.D0*Mh02 + MHH2)*S2A + 2.D0*Lambda5*MW2*(-3.D0*S2A + S2B)*SW2)*(2.D0*MZ2 - 3.D0*A0(M&
  &Z2) - 1.D0*GaugeXiZ*A0(GaugeXiZ*MZ2)))/ (CW2*Mh02*PI2*S2B*SW2)

 amplitudes(44) = (0.03125D0*CBA*SBA*(EL2*(2.D0*Mh02 + MHH2)*S2A + 2.D0*Lambda5*MW2*(-3.D0*S2A + S2B)*SW2)*(2.D0*MW2 - 3.D0*A0(MW&
  &2) - 1.D0*GaugeXiW*A0(GaugeXiW*MW2)))/ (Mh02*PI2*S2B*SW2)

 amplitudes(45) = (-0.015625D0*CBA*SBA*(EL2*(Mh02 + 2.D0*MHH2)*S2A - 2.D0*Lambda5*MW2*(3.D0*S2A + S2B)*SW2)*(2.D0*MZ2 - 3.D0*A0(M&
  &Z2) - 1.D0*GaugeXiZ*A0(GaugeXiZ*MZ2)))/ (CW2*MHH2*PI2*S2B*SW2)

 amplitudes(46) = (-0.03125D0*CBA*SBA*(EL2*(Mh02 + 2.D0*MHH2)*S2A - 2.D0*Lambda5*MW2*(3.D0*S2A + S2B)*SW2)*(2.D0*MW2 - 3.D0*A0(MW&
  &2) - 1.D0*GaugeXiW*A0(GaugeXiW*MW2)))/ (MHH2*PI2*S2B*SW2)

 amplitudes(47) = (-0.03125D0*EL2*ME2*Yuk4*Yuk5*(2.D0*A0(ME2) + (4.D0*ME2 - 1.D0*x)*B0(x, ME2, ME2)))/(MW2*PI2*SW2)

 amplitudes(48) = (-0.03125D0*EL2*MM2*Yuk4*Yuk5*(2.D0*A0(MM2) + (4.D0*MM2 - 1.D0*x)*B0(x, MM2, MM2)))/(MW2*PI2*SW2)

 amplitudes(49) = (-0.03125D0*EL2*ML2*Yuk4*Yuk5*(2.D0*A0(ML2) + (4.D0*ML2 - 1.D0*x)*B0(x, ML2, ML2)))/(MW2*PI2*SW2)

 amplitudes(50) = (-0.09375D0*CA*EL2*MU2*SA*(2.D0*A0(MU2) + (4.D0*MU2 - 1.D0*x)*B0(x, MU2, MU2)))/(MW2*PI2*SB2*SW2)

 amplitudes(51) = (-0.09375D0*CA*EL2*MC2*SA*(2.D0*A0(MC2) + (4.D0*MC2 - 1.D0*x)*B0(x, MC2, MC2)))/(MW2*PI2*SB2*SW2)

 amplitudes(52) = (-0.09375D0*CA*EL2*MT2*SA*(2.D0*A0(MT2) + (4.D0*MT2 - 1.D0*x)*B0(x, MT2, MT2)))/(MW2*PI2*SB2*SW2)

 amplitudes(53) = (-0.09375D0*EL2*MD2*Yuk1*Yuk2*(2.D0*A0(MD2) + (4.D0*MD2 - 1.D0*x)*B0(x, MD2, MD2)))/(MW2*PI2*SW2)

 amplitudes(54) = (-0.09375D0*EL2*MS2*Yuk1*Yuk2*(2.D0*A0(MS2) + (4.D0*MS2 - 1.D0*x)*B0(x, MS2, MS2)))/(MW2*PI2*SW2)

 amplitudes(55) = (-0.09375D0*EL2*MB2*Yuk1*Yuk2*(2.D0*A0(MB2) + (4.D0*MB2 - 1.D0*x)*B0(x, MB2, MB2)))/(MW2*PI2*SW2)

 amplitudes(56) = (-0.0234375D0*CBA*(-2.D0*CAB*EL2*Mh02 - 1.D0*EL2*Mh02*S2A*SBA + 4.D0*CAB*CBA2*Lambda5*MW2*SW2)* (EL2*(2.D0*Mh02&
  & + MHH2)*S2A + 2.D0*Lambda5*MW2*(-3.D0*S2A + S2B)*SW2)*B0(x, Mh02, Mh02))/(EL2*MW2*PI2*S2B2*SW2)

 amplitudes(57) = (0.0234375D0*SBA*(EL2*(Mh02 + 2.D0*MHH2)*S2A - 2.D0*Lambda5*MW2*(3.D0*S2A + S2B)*SW2)*(CBA*EL2*MHH2*S2A - 2.D0*&
  &EL2*MHH2*SAB + 4.D0*Lambda5*MW2*SAB*SBA2*SW2)*B0(x, MHH2, MHH2))/(EL2*MW2*PI2*S2B2*SW2)

 amplitudes(58) = (-0.015625D0*CBA*SBA*(EL2*(2.D0*Mh02 + MHH2)*S2A + 2.D0*Lambda5*MW2*(-3.D0*S2A + S2B)*SW2)* (EL2*(Mh02 + 2.D0*M&
  &HH2)*S2A - 2.D0*Lambda5*MW2*(3.D0*S2A + S2B)*SW2)*B0(x, Mh02, MHH2))/(EL2*MW2*PI2*S2B2*SW2)

 amplitudes(59) = (0.0078125D0*(EL2*(2.D0*MA02 - 1.D0*Mh02)*S2B*SBA + 2.D0*CAB*(EL2*Mh02 - 2.D0*Lambda5*MW2*SW2))* (CBA*EL2*(2.D0&
  &*MA02 - 1.D0*MHH2)*S2B + 2.D0*SAB*(EL2*MHH2 - 2.D0*Lambda5*MW2*SW2))*B0(x, MA02, MA02))/(EL2*MW2*PI2*S2B2*SW2)

 amplitudes(60) = (0.0078125D0*CBA*EL2*Mh02*MHH2*SBA*B0(x, GaugeXiZ*MZ2, GaugeXiZ*MZ2))/(MW2*PI2*SW2)

 amplitudes(61) = (-0.015625D0*CBA*EL2*(MA02 - 1.D0*Mh02)*(MA02 - 1.D0*MHH2)*SBA*B0(x, MA02, GaugeXiZ*MZ2))/(MW2*PI2*SW2)

 amplitudes(62) = (0.015625D0*(-1.D0*EL2*(Mh02 - 2.D0*MHp2)*S2B*SBA + 2.D0*CAB*(EL2*Mh02 - 2.D0*Lambda5*MW2*SW2))* (-1.D0*CBA*EL2&
  &*(MHH2 - 2.D0*MHp2)*S2B + 2.D0*SAB*(EL2*MHH2 - 2.D0*Lambda5*MW2*SW2))*B0(x, MHp2, MHp2))/(EL2*MW2*PI2*S2B2*SW2)

 amplitudes(63) = (0.015625D0*CBA*EL2*Mh02*MHH2*SBA*B0(x, GaugeXiW*MW2, GaugeXiW*MW2))/(MW2*PI2*SW2)

 amplitudes(64) = (0.015625D0*CBA*EL2*(Mh02 - 1.D0*MHp2)*(-1.D0*MHH2 + MHp2)*SBA*B0(x, MHp2, GaugeXiW*MW2))/(MW2*PI2*SW2)

 amplitudes(65) = (0.015625D0*CBA*EL2*(Mh02 - 1.D0*MHp2)*(-1.D0*MHH2 + MHp2)*SBA*B0(x, MHp2, GaugeXiW*MW2))/(MW2*PI2*SW2)

 amplitudes(66) = (-0.015625D0*CBA*EL2*MW2*SBA*B0(x, GaugeXiZ*MZ2, GaugeXiZ*MZ2)*DBLE(CW**INT(-4.D0))*DBLE(GaugeXiZ**INT(2.D0)))/&
  &(PI2*SW2)

 amplitudes(67) = (-0.015625D0*CBA*EL2*MW2*SBA*B0(x, GaugeXiW*MW2, GaugeXiW*MW2)*DBLE(GaugeXiW**INT(2.D0)))/(PI2*SW2)

 amplitudes(68) = (-0.015625D0*CBA*EL2*MW2*SBA*B0(x, GaugeXiW*MW2, GaugeXiW*MW2)*DBLE(GaugeXiW**INT(2.D0)))/(PI2*SW2)

 amplitudes(69) = (0.0078125D0*CBA*EL2*MW2*SBA*DBLE(CW**INT(-4.D0))*DBLE(MZ**INT(-4.D0))*(-2.D0*(-1.D0 + GaugeXiZ)*MZ2*A0(MZ2) + &
  &2.D0*(-1.D0 + GaugeXiZ)*MZ2*A0(GaugeXiZ*MZ2) - 4.D0*MZ2*x*B0(x, MZ2, MZ2) + 4.D0*MZ2*x*B0(x, MZ2, GaugeXiZ*MZ2) + 4.D0*GaugeXi&
  &Z*MZ2*x*B0(x, MZ2, GaugeXiZ*MZ2) - 4.D0*GaugeXiZ*MZ2*x*B0(x, GaugeXiZ*MZ2, GaugeXiZ*MZ2) - 8.D0*DBLE(MZ**INT(4.D0)) + 12.D0*B0&
  &(x, MZ2, MZ2)*DBLE(MZ**INT(4.D0)) - 2.D0*B0(x, MZ2, GaugeXiZ*MZ2)*DBLE(MZ**INT(4.D0)) + 4.D0*GaugeXiZ*B0(x, MZ2, GaugeXiZ*MZ2)&
  &*DBLE(MZ**INT(4.D0)) - 2.D0*B0(x, MZ2, GaugeXiZ*MZ2)*DBLE(GaugeXiZ**INT(2.D0))*DBLE(MZ**INT(4.D0)) + 4.D0*B0(x, GaugeXiZ*MZ2, &
  &GaugeXiZ*MZ2)*DBLE(GaugeXiZ**INT(2.D0))*DBLE(MZ**INT(4.D0)) + B0(x, MZ2, MZ2)*DBLE(x**INT(2.D0)) - 2.D0*B0(x, MZ2, GaugeXiZ*MZ&
  &2)*DBLE(x**INT(2.D0)) + B0(x, GaugeXiZ*MZ2, GaugeXiZ*MZ2)*DBLE(x**INT(2.D0))))/(PI2*SW2)

 amplitudes(70) = (0.015625D0*CBA*EL2*SBA*(-2.D0*(-1.D0 + GaugeXiW)*MW2*A0(MW2) + 2.D0*(-1.D0 + GaugeXiW)*MW2*A0(GaugeXiW*MW2) - &
  &4.D0*MW2*x*B0(x, MW2, MW2) + 4.D0*MW2*x*B0(x, MW2, GaugeXiW*MW2) + 4.D0*GaugeXiW*MW2*x*B0(x, MW2, GaugeXiW*MW2) - 4.D0*GaugeXi&
  &W*MW2*x*B0(x, GaugeXiW*MW2, GaugeXiW*MW2) - 8.D0*DBLE(MW**INT(4.D0)) + 12.D0*B0(x, MW2, MW2)*DBLE(MW**INT(4.D0)) - 2.D0*B0(x, &
  &MW2, GaugeXiW*MW2)*DBLE(MW**INT(4.D0)) + 4.D0*GaugeXiW*B0(x, MW2, GaugeXiW*MW2)*DBLE(MW**INT(4.D0)) - 2.D0*B0(x, MW2, GaugeXiW&
  &*MW2)*DBLE(GaugeXiW**INT(2.D0))*DBLE(MW**INT(4.D0)) + 4.D0*B0(x, GaugeXiW*MW2, GaugeXiW*MW2)*DBLE(GaugeXiW**INT(2.D0))* DBLE(M&
  &W**INT(4.D0)) + B0(x, MW2, MW2)*DBLE(x**INT(2.D0)) - 2.D0*B0(x, MW2, GaugeXiW*MW2)*DBLE(x**INT(2.D0)) + B0(x, GaugeXiW*MW2, Ga&
  &ugeXiW*MW2)*DBLE(x**INT(2.D0))))/(MW2*PI2*SW2)

 amplitudes(71) = (0.015625D0*CBA*EL2*SBA*(-1.D0*MZ2*A0(MA02) + (-1.D0*MA02 + MZ2 + x)*A0(MZ2) + MA02*A0(GaugeXiZ*MZ2) + GaugeXiZ&
  &*MZ2*A0(GaugeXiZ*MZ2) - 1.D0*x*A0(GaugeXiZ*MZ2) + 2.D0*MA02*MZ2*B0(x, MA02, MZ2) + 2.D0*MA02*x*B0(x, MA02, MZ2) + 2.D0*MZ2*x*B&
  &0(x, MA02, MZ2) - 2.D0*MA02*x*B0(x, MA02, GaugeXiZ*MZ2) - 1.D0*B0(x, MA02, MZ2)*DBLE(MA0**INT(4.D0)) + B0(x, MA02, GaugeXiZ*MZ&
  &2)*DBLE(MA0**INT(4.D0)) - 1.D0*B0(x, MA02, MZ2)*DBLE(MZ**INT(4.D0)) - 1.D0*B0(x, MA02, MZ2)*DBLE(x**INT(2.D0)) + B0(x, MA02, G&
  &augeXiZ*MZ2)*DBLE(x**INT(2.D0))))/ (CW2*MZ2*PI2*SW2)

 amplitudes(72) = (0.015625D0*CBA*EL2*SBA*((-1.D0*MZ2 + GaugeXiZ*MZ2 - 1.D0*x)*A0(MZ2) + (MZ2 - 2.D0*GaugeXiZ*MZ2 + x)*A0(GaugeXi&
  &Z*MZ2) - 2.D0*MZ2*x*B0(x, MZ2, GaugeXiZ*MZ2) - 2.D0*GaugeXiZ*MZ2*x*B0(x, MZ2, GaugeXiZ*MZ2) + 2.D0*GaugeXiZ*MZ2*x*B0(x, GaugeX&
  &iZ*MZ2, GaugeXiZ*MZ2) + B0(x, MZ2, GaugeXiZ*MZ2)*DBLE(MZ**INT(4.D0)) - 2.D0*GaugeXiZ*B0(x, MZ2, GaugeXiZ*MZ2)*DBLE(MZ**INT(4.D&
  &0)) + B0(x, MZ2, GaugeXiZ*MZ2)*DBLE(GaugeXiZ**INT(2.D0))*DBLE(MZ**INT(4.D0)) - 1.D0*B0(x, GaugeXiZ*MZ2, GaugeXiZ*MZ2)*DBLE(Gau&
  &geXiZ**INT(2.D0))*DBLE(MZ**INT(4.D0)) + B0(x, MZ2, GaugeXiZ*MZ2)*DBLE(x**INT(2.D0)) - 1.D0*B0(x, GaugeXiZ*MZ2, GaugeXiZ*MZ2)*D&
  &BLE(x**INT(2.D0))))/(CW2*MZ2*PI2*SW2)

 amplitudes(73) = (0.015625D0*CBA*EL2*SBA*(-1.D0*MW2*A0(MHp2) + (-1.D0*MHp2 + MW2 + x)*A0(MW2) + MHp2*A0(GaugeXiW*MW2) + GaugeXiW&
  &*MW2*A0(GaugeXiW*MW2) - 1.D0*x*A0(GaugeXiW*MW2) + 2.D0*MHp2*MW2*B0(x, MHp2, MW2) + 2.D0*MHp2*x*B0(x, MHp2, MW2) + 2.D0*MW2*x*B&
  &0(x, MHp2, MW2) - 2.D0*MHp2*x*B0(x, MHp2, GaugeXiW*MW2) - 1.D0*B0(x, MHp2, MW2)*DBLE(MHp**INT(4.D0)) + B0(x, MHp2, GaugeXiW*MW&
  &2)*DBLE(MHp**INT(4.D0)) - 1.D0*B0(x, MHp2, MW2)*DBLE(MW**INT(4.D0)) - 1.D0*B0(x, MHp2, MW2)*DBLE(x**INT(2.D0)) + B0(x, MHp2, G&
  &augeXiW*MW2)*DBLE(x**INT(2.D0))))/ (MW2*PI2*SW2)

 amplitudes(74) = (0.015625D0*CBA*EL2*SBA*(-1.D0*MW2*A0(MHp2) + (-1.D0*MHp2 + MW2 + x)*A0(MW2) + MHp2*A0(GaugeXiW*MW2) + GaugeXiW&
  &*MW2*A0(GaugeXiW*MW2) - 1.D0*x*A0(GaugeXiW*MW2) + 2.D0*MHp2*MW2*B0(x, MHp2, MW2) + 2.D0*MHp2*x*B0(x, MHp2, MW2) + 2.D0*MW2*x*B&
  &0(x, MHp2, MW2) - 2.D0*MHp2*x*B0(x, MHp2, GaugeXiW*MW2) - 1.D0*B0(x, MHp2, MW2)*DBLE(MHp**INT(4.D0)) + B0(x, MHp2, GaugeXiW*MW&
  &2)*DBLE(MHp**INT(4.D0)) - 1.D0*B0(x, MHp2, MW2)*DBLE(MW**INT(4.D0)) - 1.D0*B0(x, MHp2, MW2)*DBLE(x**INT(2.D0)) + B0(x, MHp2, G&
  &augeXiW*MW2)*DBLE(x**INT(2.D0))))/ (MW2*PI2*SW2)

 amplitudes(75) = (0.015625D0*CBA*EL2*SBA*((-1.D0*MW2 + GaugeXiW*MW2 - 1.D0*x)*A0(MW2) + (MW2 - 2.D0*GaugeXiW*MW2 + x)*A0(GaugeXi&
  &W*MW2) - 2.D0*MW2*x*B0(x, MW2, GaugeXiW*MW2) - 2.D0*GaugeXiW*MW2*x*B0(x, MW2, GaugeXiW*MW2) + 2.D0*GaugeXiW*MW2*x*B0(x, GaugeX&
  &iW*MW2, GaugeXiW*MW2) + B0(x, MW2, GaugeXiW*MW2)*DBLE(MW**INT(4.D0)) - 2.D0*GaugeXiW*B0(x, MW2, GaugeXiW*MW2)*DBLE(MW**INT(4.D&
  &0)) + B0(x, MW2, GaugeXiW*MW2)*DBLE(GaugeXiW**INT(2.D0))*DBLE(MW**INT(4.D0)) - 1.D0*B0(x, GaugeXiW*MW2, GaugeXiW*MW2)*DBLE(Gau&
  &geXiW**INT(2.D0))*DBLE(MW**INT(4.D0)) + B0(x, MW2, GaugeXiW*MW2)*DBLE(x**INT(2.D0)) - 1.D0*B0(x, GaugeXiW*MW2, GaugeXiW*MW2)*D&
  &BLE(x**INT(2.D0))))/(MW2*PI2*SW2)

 amplitudes(76) = (0.015625D0*CBA*EL2*SBA*((-1.D0*MW2 + GaugeXiW*MW2 - 1.D0*x)*A0(MW2) + (MW2 - 2.D0*GaugeXiW*MW2 + x)*A0(GaugeXi&
  &W*MW2) - 2.D0*MW2*x*B0(x, MW2, GaugeXiW*MW2) - 2.D0*GaugeXiW*MW2*x*B0(x, MW2, GaugeXiW*MW2) + 2.D0*GaugeXiW*MW2*x*B0(x, GaugeX&
  &iW*MW2, GaugeXiW*MW2) + B0(x, MW2, GaugeXiW*MW2)*DBLE(MW**INT(4.D0)) - 2.D0*GaugeXiW*B0(x, MW2, GaugeXiW*MW2)*DBLE(MW**INT(4.D&
  &0)) + B0(x, MW2, GaugeXiW*MW2)*DBLE(GaugeXiW**INT(2.D0))*DBLE(MW**INT(4.D0)) - 1.D0*B0(x, GaugeXiW*MW2, GaugeXiW*MW2)*DBLE(Gau&
  &geXiW**INT(2.D0))*DBLE(MW**INT(4.D0)) + B0(x, MW2, GaugeXiW*MW2)*DBLE(x**INT(2.D0)) - 1.D0*B0(x, GaugeXiW*MW2, GaugeXiW*MW2)*D&
  &BLE(x**INT(2.D0))))/(MW2*PI2*SW2)

  totalAmplitude = (0D0,0D0)
 do j=1,76
  totalAmplitude = totalAmplitude + amplitudes(j)
 end do
 SelfHHh0Alter = totalAmplitude
end function SelfHHh0Alter

