double complex function SelfG0A0Alter(x)
 use constants
 implicit none
#include "looptools.h"
 double precision, intent(in) :: x
 integer :: j
 double complex :: totalAmplitude
 double complex :: amplitudes(61)

 amplitudes(1) = (0.0078125D0*CBA*(2.D0*CAB*(EL2*Mh02 - 2.D0*Lambda5*MW2*SW2) + SBA*(EL2*(Mh02*S2A - 1.D0*MHH2*S2A - 2.D0*MA02*S2&
  &B) + 4.D0*Lambda5*MW2*S2B*SW2))* A0(Mh02))/(MW2*PI2*S2B*SW2)

 amplitudes(2) = (-0.0078125D0*SBA*(2.D0*SAB*(EL2*MHH2 - 2.D0*Lambda5*MW2*SW2) + CBA*(EL2*(Mh02*S2A - 1.D0*MHH2*S2A - 2.D0*MA02*S&
  &2B) + 4.D0*Lambda5*MW2*S2B*SW2))* A0(MHH2))/(MW2*PI2*S2B*SW2)

 amplitudes(3) = (0.0234375D0*(2.D0*CAB*CBA*EL2*Mh02 + CBA*EL2*(-1.D0*Mh02 + MHH2)*S2B*SBA - 2.D0*(EL2*MHH2*SAB*SBA + 2.D0*C2B*La&
  &mbda5*MW2*SW2))*A0(MA02))/ (MW2*PI2*S2B*SW2)

 amplitudes(4) = (0.0234375D0*CBA*EL2*(Mh02 - 1.D0*MHH2)*SBA*A0(GaugeXiZ*MZ2))/(MW2*PI2*SW2)

 amplitudes(5) = (0.015625D0*(2.D0*CAB*CBA*EL2*Mh02 + CBA*EL2*(-1.D0*Mh02 + MHH2)*S2B*SBA - 2.D0*(EL2*MHH2*SAB*SBA + 2.D0*C2B*Lam&
  &bda5*MW2*SW2))*A0(MHp2))/ (MW2*PI2*S2B*SW2)

 amplitudes(6) = (0.015625D0*CBA*EL2*(Mh02 - 1.D0*MHH2)*SBA*A0(GaugeXiW*MW2))/(MW2*PI2*SW2)

 amplitudes(7) = (0.0625D0*CBA*EL2*ME2*(-1.D0*MA02 + Mh02)*Yuk4*A0(ME2))/(Mh02*MW2*PI2*SW2)

 amplitudes(8) = (0.0625D0*CBA*EL2*(-1.D0*MA02 + Mh02)*MM2*Yuk4*A0(MM2))/(Mh02*MW2*PI2*SW2)

 amplitudes(9) = (0.0625D0*CBA*EL2*(-1.D0*MA02 + Mh02)*ML2*Yuk4*A0(ML2))/(Mh02*MW2*PI2*SW2)

 amplitudes(10) = (0.1875D0*CA*CBA*EL2*(-1.D0*MA02 + Mh02)*MU2*A0(MU2))/(Mh02*MW2*PI2*SB*SW2)

 amplitudes(11) = (0.1875D0*CA*CBA*EL2*MC2*(-1.D0*MA02 + Mh02)*A0(MC2))/(Mh02*MW2*PI2*SB*SW2)

 amplitudes(12) = (0.1875D0*CA*CBA*EL2*(-1.D0*MA02 + Mh02)*MT2*A0(MT2))/(Mh02*MW2*PI2*SB*SW2)

 amplitudes(13) = (0.1875D0*CBA*EL2*MD2*(-1.D0*MA02 + Mh02)*Yuk1*A0(MD2))/(Mh02*MW2*PI2*SW2)

 amplitudes(14) = (0.1875D0*CBA*EL2*(-1.D0*MA02 + Mh02)*MS2*Yuk1*A0(MS2))/(Mh02*MW2*PI2*SW2)

 amplitudes(15) = (0.1875D0*CBA*EL2*MB2*(-1.D0*MA02 + Mh02)*Yuk1*A0(MB2))/(Mh02*MW2*PI2*SW2)

 amplitudes(16) = (0.0625D0*EL2*ME2*(MA02 - 1.D0*MHH2)*SBA*Yuk5*A0(ME2))/(MHH2*MW2*PI2*SW2)

 amplitudes(17) = (0.0625D0*EL2*(MA02 - 1.D0*MHH2)*MM2*SBA*Yuk5*A0(MM2))/(MHH2*MW2*PI2*SW2)

 amplitudes(18) = (0.0625D0*EL2*(MA02 - 1.D0*MHH2)*ML2*SBA*Yuk5*A0(ML2))/(MHH2*MW2*PI2*SW2)

 amplitudes(19) = (0.1875D0*EL2*(MA02 - 1.D0*MHH2)*MU2*SA*SBA*A0(MU2))/(MHH2*MW2*PI2*SB*SW2)

 amplitudes(20) = (0.1875D0*EL2*MC2*(MA02 - 1.D0*MHH2)*SA*SBA*A0(MC2))/(MHH2*MW2*PI2*SB*SW2)

 amplitudes(21) = (0.1875D0*EL2*(MA02 - 1.D0*MHH2)*MT2*SA*SBA*A0(MT2))/(MHH2*MW2*PI2*SB*SW2)

 amplitudes(22) = (0.1875D0*EL2*MD2*(MA02 - 1.D0*MHH2)*SBA*Yuk2*A0(MD2))/(MHH2*MW2*PI2*SW2)

 amplitudes(23) = (0.1875D0*EL2*(MA02 - 1.D0*MHH2)*MS2*SBA*Yuk2*A0(MS2))/(MHH2*MW2*PI2*SW2)

 amplitudes(24) = (0.1875D0*EL2*MB2*(MA02 - 1.D0*MHH2)*SBA*Yuk2*A0(MB2))/(MHH2*MW2*PI2*SW2)

 amplitudes(25) = (0.0234375D0*CBA*(MA02 - 1.D0*Mh02)*(2.D0*CAB*EL2*Mh02 + EL2*Mh02*S2A*SBA - 4.D0*CAB*CBA2*Lambda5*MW2*SW2)*A0(M&
  &h02))/(Mh02*MW2*PI2*S2B*SW2)

 amplitudes(26) = (0.0078125D0*CBA*(-1.D0*MA02 + Mh02)*SBA*(EL2*(Mh02 + 2.D0*MHH2)*S2A - 2.D0*Lambda5*MW2*(3.D0*S2A + S2B)*SW2)*A&
  &0(MHH2))/(Mh02*MW2*PI2*S2B*SW2)

 amplitudes(27) = (0.0078125D0*CBA*(-1.D0*MA02 + Mh02)*(EL2*(-2.D0*MA02 + Mh02)*S2B*SBA + CAB*(-2.D0*EL2*Mh02 + 4.D0*Lambda5*MW2*&
  &SW2))*A0(MA02))/ (Mh02*MW2*PI2*S2B*SW2)

 amplitudes(28) = (0.0078125D0*CBA*EL2*(MA02 - 1.D0*Mh02)*SBA*A0(GaugeXiZ*MZ2))/(MW2*PI2*SW2)

 amplitudes(29) = (0.015625D0*CBA*(-1.D0*MA02 + Mh02)*(EL2*(Mh02 - 2.D0*MHp2)*S2B*SBA + CAB*(-2.D0*EL2*Mh02 + 4.D0*Lambda5*MW2*SW&
  &2))*A0(MHp2))/(Mh02*MW2*PI2*S2B*SW2)

 amplitudes(30) = (0.015625D0*CBA*EL2*(MA02 - 1.D0*Mh02)*SBA*A0(GaugeXiW*MW2))/(MW2*PI2*SW2)

 amplitudes(31) = (0.0078125D0*CBA*(-1.D0*MA02 + MHH2)*SBA*(EL2*(2.D0*Mh02 + MHH2)*S2A + 2.D0*Lambda5*MW2*(-3.D0*S2A + S2B)*SW2)*&
  &A0(Mh02))/(MHH2*MW2*PI2*S2B*SW2)

 amplitudes(32) = (0.0234375D0*(MA02 - 1.D0*MHH2)*SBA*(CBA*EL2*MHH2*S2A - 2.D0*EL2*MHH2*SAB + 4.D0*Lambda5*MW2*SAB*SBA2*SW2)*A0(M&
  &HH2))/(MHH2*MW2*PI2*S2B*SW2)

 amplitudes(33) = (-0.0078125D0*(-1.D0*MA02 + MHH2)*SBA*(CBA*EL2*(-2.D0*MA02 + MHH2)*S2B - 2.D0*EL2*MHH2*SAB + 4.D0*Lambda5*MW2*S&
  &AB*SW2)*A0(MA02))/ (MHH2*MW2*PI2*S2B*SW2)

 amplitudes(34) = (-0.0078125D0*CBA*EL2*(MA02 - 1.D0*MHH2)*SBA*A0(GaugeXiZ*MZ2))/(MW2*PI2*SW2)

 amplitudes(35) = (0.015625D0*(MA02 - 1.D0*MHH2)*SBA*(CBA*EL2*(MHH2 - 2.D0*MHp2)*S2B - 2.D0*EL2*MHH2*SAB + 4.D0*Lambda5*MW2*SAB*S&
  &W2)*A0(MHp2))/(MHH2*MW2*PI2*S2B*SW2)

 amplitudes(36) = (-0.015625D0*CBA*EL2*(MA02 - 1.D0*MHH2)*SBA*A0(GaugeXiW*MW2))/(MW2*PI2*SW2)

 amplitudes(37) = (0.015625D0*CBA*EL2*GaugeXiZ*(-1.D0*MA02 + Mh02)*SBA*A0(GaugeXiZ*MZ2))/(CW2*Mh02*PI2*SW2)

 amplitudes(38) = (0.015625D0*CBA*EL2*GaugeXiW*(-1.D0*MA02 + Mh02)*SBA*A0(GaugeXiW*MW2))/(Mh02*PI2*SW2)

 amplitudes(39) = (0.015625D0*CBA*EL2*GaugeXiW*(-1.D0*MA02 + Mh02)*SBA*A0(GaugeXiW*MW2))/(Mh02*PI2*SW2)

 amplitudes(40) = (0.015625D0*CBA*EL2*GaugeXiZ*(MA02 - 1.D0*MHH2)*SBA*A0(GaugeXiZ*MZ2))/(CW2*MHH2*PI2*SW2)

 amplitudes(41) = (0.015625D0*CBA*EL2*GaugeXiW*(MA02 - 1.D0*MHH2)*SBA*A0(GaugeXiW*MW2))/(MHH2*PI2*SW2)

 amplitudes(42) = (0.015625D0*CBA*EL2*GaugeXiW*(MA02 - 1.D0*MHH2)*SBA*A0(GaugeXiW*MW2))/(MHH2*PI2*SW2)

 amplitudes(43) = (0.015625D0*CBA*EL2*(-1.D0*MA02 + Mh02)*SBA*(2.D0*MZ2 - 3.D0*A0(MZ2) - 1.D0*GaugeXiZ*A0(GaugeXiZ*MZ2)))/(CW2*Mh&
  &02*PI2*SW2)

 amplitudes(44) = (0.03125D0*CBA*EL2*(-1.D0*MA02 + Mh02)*SBA*(2.D0*MW2 - 3.D0*A0(MW2) - 1.D0*GaugeXiW*A0(GaugeXiW*MW2)))/(Mh02*PI&
  &2*SW2)

 amplitudes(45) = (0.015625D0*CBA*EL2*(MA02 - 1.D0*MHH2)*SBA*(2.D0*MZ2 - 3.D0*A0(MZ2) - 1.D0*GaugeXiZ*A0(GaugeXiZ*MZ2)))/(CW2*MHH&
  &2*PI2*SW2)

 amplitudes(46) = (0.03125D0*CBA*EL2*(MA02 - 1.D0*MHH2)*SBA*(2.D0*MW2 - 3.D0*A0(MW2) - 1.D0*GaugeXiW*A0(GaugeXiW*MW2)))/(MHH2*PI2&
  &*SW2)

 amplitudes(47) = (0.03125D0*EL2*ME2*Yuk6*(2.D0*A0(ME2) - 1.D0*x*B0(x, ME2, ME2)))/(MW2*PI2*SW2)

 amplitudes(48) = (0.03125D0*EL2*MM2*Yuk6*(2.D0*A0(MM2) - 1.D0*x*B0(x, MM2, MM2)))/(MW2*PI2*SW2)

 amplitudes(49) = (0.03125D0*EL2*ML2*Yuk6*(2.D0*A0(ML2) - 1.D0*x*B0(x, ML2, ML2)))/(MW2*PI2*SW2)

 amplitudes(50) = (-0.09375D0*EL2*MU2*(2.D0*A0(MU2) - 1.D0*x*B0(x, MU2, MU2)))/(MW2*PI2*SW2*TB)

 amplitudes(51) = (-0.09375D0*EL2*MC2*(2.D0*A0(MC2) - 1.D0*x*B0(x, MC2, MC2)))/(MW2*PI2*SW2*TB)

 amplitudes(52) = (-0.09375D0*EL2*MT2*(2.D0*A0(MT2) - 1.D0*x*B0(x, MT2, MT2)))/(MW2*PI2*SW2*TB)

 amplitudes(53) = (0.09375D0*EL2*MD2*Yuk3*(2.D0*A0(MD2) - 1.D0*x*B0(x, MD2, MD2)))/(MW2*PI2*SW2)

 amplitudes(54) = (0.09375D0*EL2*MS2*Yuk3*(2.D0*A0(MS2) - 1.D0*x*B0(x, MS2, MS2)))/(MW2*PI2*SW2)

 amplitudes(55) = (0.09375D0*EL2*MB2*Yuk3*(2.D0*A0(MB2) - 1.D0*x*B0(x, MB2, MB2)))/(MW2*PI2*SW2)

 amplitudes(56) = (-0.015625D0*CBA*(MA02 - 1.D0*Mh02)*(EL2*(2.D0*MA02 - 1.D0*Mh02)*S2B*SBA + 2.D0*CAB*(EL2*Mh02 - 2.D0*Lambda5*MW&
  &2*SW2))*B0(x, MA02, Mh02))/ (MW2*PI2*S2B*SW2)

 amplitudes(57) = (0.015625D0*(MA02 - 1.D0*MHH2)*SBA*(CBA*EL2*(2.D0*MA02 - 1.D0*MHH2)*S2B + 2.D0*SAB*(EL2*MHH2 - 2.D0*Lambda5*MW2&
  &*SW2))*B0(x, MA02, MHH2))/ (MW2*PI2*S2B*SW2)

 amplitudes(58) = (0.015625D0*CBA*EL2*Mh02*(-1.D0*MA02 + Mh02)*SBA*B0(x, Mh02, GaugeXiZ*MZ2))/(MW2*PI2*SW2)

 amplitudes(59) = (0.015625D0*CBA*EL2*(MA02 - 1.D0*MHH2)*MHH2*SBA*B0(x, MHH2, GaugeXiZ*MZ2))/(MW2*PI2*SW2)

 amplitudes(60) = (0.015625D0*CBA*EL2*SBA*(MZ2*A0(Mh02) + (Mh02 - 1.D0*MZ2 - 1.D0*x)*A0(MZ2) - 1.D0*Mh02*A0(GaugeXiZ*MZ2) - 1.D0*&
  &GaugeXiZ*MZ2*A0(GaugeXiZ*MZ2) + x*A0(GaugeXiZ*MZ2) - 2.D0*Mh02*MZ2*B0(x, Mh02, MZ2) - 2.D0*Mh02*x*B0(x, Mh02, MZ2) - 2.D0*MZ2*&
  &x*B0(x, Mh02, MZ2) + 2.D0*Mh02*x*B0(x, Mh02, GaugeXiZ*MZ2) + B0(x, Mh02, MZ2)*DBLE(Mh0**INT(4.D0)) - 1.D0*B0(x, Mh02, GaugeXiZ&
  &*MZ2)*DBLE(Mh0**INT(4.D0)) + B0(x, Mh02, MZ2)*DBLE(MZ**INT(4.D0)) + B0(x, Mh02, MZ2)*DBLE(x**INT(2.D0)) - 1.D0*B0(x, Mh02, Gau&
  &geXiZ*MZ2)*DBLE(x**INT(2.D0))))/ (CW2*MZ2*PI2*SW2)

 amplitudes(61) = (0.015625D0*CBA*EL2*SBA*(-1.D0*MZ2*A0(MHH2) + (-1.D0*MHH2 + MZ2 + x)*A0(MZ2) + MHH2*A0(GaugeXiZ*MZ2) + GaugeXiZ&
  &*MZ2*A0(GaugeXiZ*MZ2) - 1.D0*x*A0(GaugeXiZ*MZ2) + 2.D0*MHH2*MZ2*B0(x, MHH2, MZ2) + 2.D0*MHH2*x*B0(x, MHH2, MZ2) + 2.D0*MZ2*x*B&
  &0(x, MHH2, MZ2) - 2.D0*MHH2*x*B0(x, MHH2, GaugeXiZ*MZ2) - 1.D0*B0(x, MHH2, MZ2)*DBLE(MHH**INT(4.D0)) + B0(x, MHH2, GaugeXiZ*MZ&
  &2)*DBLE(MHH**INT(4.D0)) - 1.D0*B0(x, MHH2, MZ2)*DBLE(MZ**INT(4.D0)) - 1.D0*B0(x, MHH2, MZ2)*DBLE(x**INT(2.D0)) + B0(x, MHH2, G&
  &augeXiZ*MZ2)*DBLE(x**INT(2.D0))))/ (CW2*MZ2*PI2*SW2)

  totalAmplitude = (0D0,0D0)
 do j=1,61
  totalAmplitude = totalAmplitude + amplitudes(j)
 end do
 SelfG0A0Alter = totalAmplitude
end function SelfG0A0Alter

