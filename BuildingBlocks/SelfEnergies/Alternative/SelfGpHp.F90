double complex function SelfGpHpAlter(x)
 use constants
 implicit none
#include "looptools.h"
 double precision, intent(in) :: x
 integer :: j
 double complex :: totalAmplitude
 double complex :: amplitudes(73)

 amplitudes(1) = (0.0078125D0*CBA*(2.D0*CAB*(EL2*Mh02 - 2.D0*Lambda5*MW2*SW2) + SBA*(EL2*(Mh02*S2A - 1.D0*MHH2*S2A - 2.D0*MHp2*S2&
  &B) + 4.D0*Lambda5*MW2*S2B*SW2))* A0(Mh02))/(MW2*PI2*S2B*SW2)

 amplitudes(2) = (-0.0078125D0*SBA*(2.D0*SAB*(EL2*MHH2 - 2.D0*Lambda5*MW2*SW2) + CBA*(EL2*(Mh02*S2A - 1.D0*MHH2*S2A - 2.D0*MHp2*S&
  &2B) + 4.D0*Lambda5*MW2*S2B*SW2))* A0(MHH2))/(MW2*PI2*S2B*SW2)

 amplitudes(3) = (0.0078125D0*(2.D0*CAB*CBA*EL2*Mh02 + CBA*EL2*(-1.D0*Mh02 + MHH2)*S2B*SBA - 2.D0*(EL2*MHH2*SAB*SBA + 2.D0*C2B*La&
  &mbda5*MW2*SW2))*A0(MA02))/ (MW2*PI2*S2B*SW2)

 amplitudes(4) = (0.0078125D0*CBA*EL2*(Mh02 - 1.D0*MHH2)*SBA*A0(GaugeXiZ*MZ2))/(MW2*PI2*SW2)

 amplitudes(5) = (0.03125D0*(2.D0*CAB*CBA*EL2*Mh02 + CBA*EL2*(-1.D0*Mh02 + MHH2)*S2B*SBA - 2.D0*(EL2*MHH2*SAB*SBA + 2.D0*C2B*Lamb&
  &da5*MW2*SW2))*A0(MHp2))/ (MW2*PI2*S2B*SW2)

 amplitudes(6) = (0.03125D0*CBA*EL2*(Mh02 - 1.D0*MHH2)*SBA*A0(GaugeXiW*MW2))/(MW2*PI2*SW2)

 amplitudes(7) = (0.0625D0*CBA*EL2*ME2*(Mh02 - 1.D0*MHp2)*Yuk4*A0(ME2))/(Mh02*MW2*PI2*SW2)

 amplitudes(8) = (0.0625D0*CBA*EL2*(Mh02 - 1.D0*MHp2)*MM2*Yuk4*A0(MM2))/(Mh02*MW2*PI2*SW2)

 amplitudes(9) = (0.0625D0*CBA*EL2*(Mh02 - 1.D0*MHp2)*ML2*Yuk4*A0(ML2))/(Mh02*MW2*PI2*SW2)

 amplitudes(10) = (0.1875D0*CA*CBA*EL2*(Mh02 - 1.D0*MHp2)*MU2*A0(MU2))/(Mh02*MW2*PI2*SB*SW2)

 amplitudes(11) = (0.1875D0*CA*CBA*EL2*MC2*(Mh02 - 1.D0*MHp2)*A0(MC2))/(Mh02*MW2*PI2*SB*SW2)

 amplitudes(12) = (0.1875D0*CA*CBA*EL2*(Mh02 - 1.D0*MHp2)*MT2*A0(MT2))/(Mh02*MW2*PI2*SB*SW2)

 amplitudes(13) = (0.1875D0*CBA*EL2*MD2*(Mh02 - 1.D0*MHp2)*Yuk1*A0(MD2))/(Mh02*MW2*PI2*SW2)

 amplitudes(14) = (0.1875D0*CBA*EL2*(Mh02 - 1.D0*MHp2)*MS2*Yuk1*A0(MS2))/(Mh02*MW2*PI2*SW2)

 amplitudes(15) = (0.1875D0*CBA*EL2*MB2*(Mh02 - 1.D0*MHp2)*Yuk1*A0(MB2))/(Mh02*MW2*PI2*SW2)

 amplitudes(16) = (-0.0625D0*EL2*ME2*(MHH2 - 1.D0*MHp2)*SBA*Yuk5*A0(ME2))/(MHH2*MW2*PI2*SW2)

 amplitudes(17) = (-0.0625D0*EL2*(MHH2 - 1.D0*MHp2)*MM2*SBA*Yuk5*A0(MM2))/(MHH2*MW2*PI2*SW2)

 amplitudes(18) = (-0.0625D0*EL2*(MHH2 - 1.D0*MHp2)*ML2*SBA*Yuk5*A0(ML2))/(MHH2*MW2*PI2*SW2)

 amplitudes(19) = (-0.1875D0*EL2*(MHH2 - 1.D0*MHp2)*MU2*SA*SBA*A0(MU2))/(MHH2*MW2*PI2*SB*SW2)

 amplitudes(20) = (-0.1875D0*EL2*MC2*(MHH2 - 1.D0*MHp2)*SA*SBA*A0(MC2))/(MHH2*MW2*PI2*SB*SW2)

 amplitudes(21) = (-0.1875D0*EL2*(MHH2 - 1.D0*MHp2)*MT2*SA*SBA*A0(MT2))/(MHH2*MW2*PI2*SB*SW2)

 amplitudes(22) = (-0.1875D0*EL2*MD2*(MHH2 - 1.D0*MHp2)*SBA*Yuk2*A0(MD2))/(MHH2*MW2*PI2*SW2)

 amplitudes(23) = (-0.1875D0*EL2*(MHH2 - 1.D0*MHp2)*MS2*SBA*Yuk2*A0(MS2))/(MHH2*MW2*PI2*SW2)

 amplitudes(24) = (-0.1875D0*EL2*MB2*(MHH2 - 1.D0*MHp2)*SBA*Yuk2*A0(MB2))/(MHH2*MW2*PI2*SW2)

 amplitudes(25) = 0D0

 amplitudes(26) = 0D0

 amplitudes(27) = 0D0

 amplitudes(28) = 0D0

 amplitudes(29) = 0D0

 amplitudes(30) = 0D0

 amplitudes(31) = 0D0

 amplitudes(32) = 0D0

 amplitudes(33) = 0D0

 amplitudes(34) = (-0.0234375D0*CBA*(Mh02 - 1.D0*MHp2)*(2.D0*CAB*EL2*Mh02 + EL2*Mh02*S2A*SBA - 4.D0*CAB*CBA2*Lambda5*MW2*SW2)*A0(&
  &Mh02))/(Mh02*MW2*PI2*S2B*SW2)

 amplitudes(35) = (0.0078125D0*CBA*(Mh02 - 1.D0*MHp2)*SBA*(EL2*(Mh02 + 2.D0*MHH2)*S2A - 2.D0*Lambda5*MW2*(3.D0*S2A + S2B)*SW2)*A0&
  &(MHH2))/(Mh02*MW2*PI2*S2B*SW2)

 amplitudes(36) = (0.0078125D0*CBA*(Mh02 - 1.D0*MHp2)*(EL2*(-2.D0*MA02 + Mh02)*S2B*SBA + CAB*(-2.D0*EL2*Mh02 + 4.D0*Lambda5*MW2*S&
  &W2))*A0(MA02))/ (Mh02*MW2*PI2*S2B*SW2)

 amplitudes(37) = (-0.0078125D0*CBA*EL2*(Mh02 - 1.D0*MHp2)*SBA*A0(GaugeXiZ*MZ2))/(MW2*PI2*SW2)

 amplitudes(38) = (0.015625D0*CBA*(Mh02 - 1.D0*MHp2)*(EL2*(Mh02 - 2.D0*MHp2)*S2B*SBA + CAB*(-2.D0*EL2*Mh02 + 4.D0*Lambda5*MW2*SW2&
  &))*A0(MHp2))/(Mh02*MW2*PI2*S2B*SW2)

 amplitudes(39) = (-0.015625D0*CBA*EL2*(Mh02 - 1.D0*MHp2)*SBA*A0(GaugeXiW*MW2))/(MW2*PI2*SW2)

 amplitudes(40) = (0.0078125D0*CBA*(MHH2 - 1.D0*MHp2)*SBA*(EL2*(2.D0*Mh02 + MHH2)*S2A + 2.D0*Lambda5*MW2*(-3.D0*S2A + S2B)*SW2)*A&
  &0(Mh02))/(MHH2*MW2*PI2*S2B*SW2)

 amplitudes(41) = (-0.0234375D0*(MHH2 - 1.D0*MHp2)*SBA*(CBA*EL2*MHH2*S2A - 2.D0*EL2*MHH2*SAB + 4.D0*Lambda5*MW2*SAB*SBA2*SW2)*A0(&
  &MHH2))/(MHH2*MW2*PI2*S2B*SW2)

 amplitudes(42) = (-0.0078125D0*(MHH2 - 1.D0*MHp2)*SBA*(CBA*EL2*(-2.D0*MA02 + MHH2)*S2B - 2.D0*EL2*MHH2*SAB + 4.D0*Lambda5*MW2*SA&
  &B*SW2)*A0(MA02))/ (MHH2*MW2*PI2*S2B*SW2)

 amplitudes(43) = (0.0078125D0*CBA*EL2*(MHH2 - 1.D0*MHp2)*SBA*A0(GaugeXiZ*MZ2))/(MW2*PI2*SW2)

 amplitudes(44) = (-0.015625D0*(MHH2 - 1.D0*MHp2)*SBA*(CBA*EL2*(MHH2 - 2.D0*MHp2)*S2B - 2.D0*EL2*MHH2*SAB + 4.D0*Lambda5*MW2*SAB*&
  &SW2)*A0(MHp2))/ (MHH2*MW2*PI2*S2B*SW2)

 amplitudes(45) = (0.015625D0*CBA*EL2*(MHH2 - 1.D0*MHp2)*SBA*A0(GaugeXiW*MW2))/(MW2*PI2*SW2)

 amplitudes(46) = (0.015625D0*CBA*EL2*GaugeXiZ*(Mh02 - 1.D0*MHp2)*SBA*A0(GaugeXiZ*MZ2))/(CW2*Mh02*PI2*SW2)

 amplitudes(47) = (0.015625D0*CBA*EL2*GaugeXiW*(Mh02 - 1.D0*MHp2)*SBA*A0(GaugeXiW*MW2))/(Mh02*PI2*SW2)

 amplitudes(48) = (0.015625D0*CBA*EL2*GaugeXiW*(Mh02 - 1.D0*MHp2)*SBA*A0(GaugeXiW*MW2))/(Mh02*PI2*SW2)

 amplitudes(49) = (-0.015625D0*CBA*EL2*GaugeXiZ*(MHH2 - 1.D0*MHp2)*SBA*A0(GaugeXiZ*MZ2))/(CW2*MHH2*PI2*SW2)

 amplitudes(50) = (-0.015625D0*CBA*EL2*GaugeXiW*(MHH2 - 1.D0*MHp2)*SBA*A0(GaugeXiW*MW2))/(MHH2*PI2*SW2)

 amplitudes(51) = (-0.015625D0*CBA*EL2*GaugeXiW*(MHH2 - 1.D0*MHp2)*SBA*A0(GaugeXiW*MW2))/(MHH2*PI2*SW2)

 amplitudes(52) = (0.015625D0*CBA*EL2*(Mh02 - 1.D0*MHp2)*SBA*(2.D0*MZ2 - 3.D0*A0(MZ2) - 1.D0*GaugeXiZ*A0(GaugeXiZ*MZ2)))/(CW2*Mh0&
  &2*PI2*SW2)

 amplitudes(53) = (0.03125D0*CBA*EL2*(Mh02 - 1.D0*MHp2)*SBA*(2.D0*MW2 - 3.D0*A0(MW2) - 1.D0*GaugeXiW*A0(GaugeXiW*MW2)))/(Mh02*PI2&
  &*SW2)

 amplitudes(54) = (-0.015625D0*CBA*EL2*(MHH2 - 1.D0*MHp2)*SBA*(2.D0*MZ2 - 3.D0*A0(MZ2) - 1.D0*GaugeXiZ*A0(GaugeXiZ*MZ2)))/(CW2*MH&
  &H2*PI2*SW2)

 amplitudes(55) = (-0.03125D0*CBA*EL2*(MHH2 - 1.D0*MHp2)*SBA*(2.D0*MW2 - 3.D0*A0(MW2) - 1.D0*GaugeXiW*A0(GaugeXiW*MW2)))/(MHH2*PI&
  &2*SW2)

 amplitudes(56) = (0.03125D0*EL2*ME2*Yuk6*(A0(ME2) + (ME2 - 1.D0*x)*B0(x, 0.D0, ME2)))/(MW2*PI2*SW2)

 amplitudes(57) = (0.03125D0*EL2*MM2*Yuk6*(A0(MM2) + (MM2 - 1.D0*x)*B0(x, 0.D0, MM2)))/(MW2*PI2*SW2)

 amplitudes(58) = (0.03125D0*EL2*ML2*Yuk6*(A0(ML2) + (ML2 - 1.D0*x)*B0(x, 0.D0, ML2)))/(MW2*PI2*SW2)

 amplitudes(59) = (-0.09375D0*CKM11*CKMC11*EL2*((MU2 - 1.D0*MD2*TB*Yuk3)*A0(MD2) + (MU2 - 1.D0*MD2*TB*Yuk3)*A0(MU2) + B0(x, MD2, &
  &MU2)*(-1.D0*MU2*x + MD2*(TB*x*Yuk3 + MU2*(-1.D0 + TB*Yuk3)) - 1.D0*TB*Yuk3*DBLE(MD**INT(4.D0)) + DBLE(MU**INT(4.D0)))))/ (MW2*&
  &PI2*SW2*TB)

 amplitudes(60) = (-0.09375D0*CKM21*CKMC21*EL2*((MC2 - 1.D0*MD2*TB*Yuk3)*A0(MC2) + (MC2 - 1.D0*MD2*TB*Yuk3)*A0(MD2) + B0(x, MC2, &
  &MD2)*(MD2*TB*(-1.D0*MD2 + x)*Yuk3 - 1.D0*MC2*(x + MD2*(1.D0 - 1.D0*TB*Yuk3)) + DBLE(MC**INT(4.D0)))))/(MW2*PI2*SW2*TB)

 amplitudes(61) = (-0.09375D0*CKM31*CKMC31*EL2*((MT2 - 1.D0*MD2*TB*Yuk3)*A0(MD2) + (MT2 - 1.D0*MD2*TB*Yuk3)*A0(MT2) + B0(x, MD2, &
  &MT2)*(-1.D0*MT2*x + MD2*(TB*x*Yuk3 + MT2*(-1.D0 + TB*Yuk3)) - 1.D0*TB*Yuk3*DBLE(MD**INT(4.D0)) + DBLE(MT**INT(4.D0)))))/ (MW2*&
  &PI2*SW2*TB)

 amplitudes(62) = (-0.09375D0*CKM12*CKMC12*EL2*((MU2 - 1.D0*MS2*TB*Yuk3)*A0(MS2) + (MU2 - 1.D0*MS2*TB*Yuk3)*A0(MU2) + B0(x, MS2, &
  &MU2)*(-1.D0*MU2*x + MS2*(TB*x*Yuk3 + MU2*(-1.D0 + TB*Yuk3)) - 1.D0*TB*Yuk3*DBLE(MS**INT(4.D0)) + DBLE(MU**INT(4.D0)))))/ (MW2*&
  &PI2*SW2*TB)

 amplitudes(63) = (-0.09375D0*CKM22*CKMC22*EL2*((MC2 - 1.D0*MS2*TB*Yuk3)*A0(MC2) + (MC2 - 1.D0*MS2*TB*Yuk3)*A0(MS2) + B0(x, MC2, &
  &MS2)*(MS2*TB*(-1.D0*MS2 + x)*Yuk3 - 1.D0*MC2*(x + MS2*(1.D0 - 1.D0*TB*Yuk3)) + DBLE(MC**INT(4.D0)))))/(MW2*PI2*SW2*TB)

 amplitudes(64) = (-0.09375D0*CKM32*CKMC32*EL2*((MT2 - 1.D0*MS2*TB*Yuk3)*A0(MS2) + (MT2 - 1.D0*MS2*TB*Yuk3)*A0(MT2) + B0(x, MS2, &
  &MT2)*(-1.D0*MT2*x + MS2*(TB*x*Yuk3 + MT2*(-1.D0 + TB*Yuk3)) - 1.D0*TB*Yuk3*DBLE(MS**INT(4.D0)) + DBLE(MT**INT(4.D0)))))/ (MW2*&
  &PI2*SW2*TB)

 amplitudes(65) = (-0.09375D0*CKM13*CKMC13*EL2*((MU2 - 1.D0*MB2*TB*Yuk3)*A0(MB2) + (MU2 - 1.D0*MB2*TB*Yuk3)*A0(MU2) + B0(x, MB2, &
  &MU2)*(-1.D0*MU2*x + MB2*(TB*x*Yuk3 + MU2*(-1.D0 + TB*Yuk3)) - 1.D0*TB*Yuk3*DBLE(MB**INT(4.D0)) + DBLE(MU**INT(4.D0)))))/ (MW2*&
  &PI2*SW2*TB)

 amplitudes(66) = (-0.09375D0*CKM23*CKMC23*EL2*((MC2 - 1.D0*MB2*TB*Yuk3)*A0(MB2) + (MC2 - 1.D0*MB2*TB*Yuk3)*A0(MC2) + B0(x, MB2, &
  &MC2)*(-1.D0*MC2*x + MB2*(TB*x*Yuk3 + MC2*(-1.D0 + TB*Yuk3)) - 1.D0*TB*Yuk3*DBLE(MB**INT(4.D0)) + DBLE(MC**INT(4.D0)))))/ (MW2*&
  &PI2*SW2*TB)

 amplitudes(67) = (-0.09375D0*CKM33*CKMC33*EL2*((MT2 - 1.D0*MB2*TB*Yuk3)*A0(MB2) + (MT2 - 1.D0*MB2*TB*Yuk3)*A0(MT2) + B0(x, MB2, &
  &MT2)*(-1.D0*MT2*x + MB2*(TB*x*Yuk3 + MT2*(-1.D0 + TB*Yuk3)) - 1.D0*TB*Yuk3*DBLE(MB**INT(4.D0)) + DBLE(MT**INT(4.D0)))))/ (MW2*&
  &PI2*SW2*TB)

 amplitudes(68) = (0.015625D0*CBA*(Mh02 - 1.D0*MHp2)*(-1.D0*EL2*(Mh02 - 2.D0*MHp2)*S2B*SBA + 2.D0*CAB*(EL2*Mh02 - 2.D0*Lambda5*MW&
  &2*SW2))*B0(x, Mh02, MHp2))/ (MW2*PI2*S2B*SW2)

 amplitudes(69) = (0.015625D0*(MHH2 - 1.D0*MHp2)*SBA*(CBA*EL2*(MHH2 - 2.D0*MHp2)*S2B - 2.D0*EL2*MHH2*SAB + 4.D0*Lambda5*MW2*SAB*S&
  &W2)*B0(x, MHH2, MHp2))/ (MW2*PI2*S2B*SW2)

 amplitudes(70) = (0.015625D0*CBA*EL2*Mh02*(Mh02 - 1.D0*MHp2)*SBA*B0(x, Mh02, GaugeXiW*MW2))/(MW2*PI2*SW2)

 amplitudes(71) = (-0.015625D0*CBA*EL2*MHH2*(MHH2 - 1.D0*MHp2)*SBA*B0(x, MHH2, GaugeXiW*MW2))/(MW2*PI2*SW2)

 amplitudes(72) = (0.015625D0*CBA*EL2*SBA*(MW2*A0(Mh02) + (Mh02 - 1.D0*MW2 - 1.D0*x)*A0(MW2) - 1.D0*Mh02*A0(GaugeXiW*MW2) - 1.D0*&
  &GaugeXiW*MW2*A0(GaugeXiW*MW2) + x*A0(GaugeXiW*MW2) - 2.D0*Mh02*MW2*B0(x, Mh02, MW2) - 2.D0*Mh02*x*B0(x, Mh02, MW2) - 2.D0*MW2*&
  &x*B0(x, Mh02, MW2) + 2.D0*Mh02*x*B0(x, Mh02, GaugeXiW*MW2) + B0(x, Mh02, MW2)*DBLE(Mh0**INT(4.D0)) - 1.D0*B0(x, Mh02, GaugeXiW&
  &*MW2)*DBLE(Mh0**INT(4.D0)) + B0(x, Mh02, MW2)*DBLE(MW**INT(4.D0)) + B0(x, Mh02, MW2)*DBLE(x**INT(2.D0)) - 1.D0*B0(x, Mh02, Gau&
  &geXiW*MW2)*DBLE(x**INT(2.D0))))/(MW2*PI2*SW2)

 amplitudes(73) = (0.015625D0*CBA*EL2*SBA*(-1.D0*MW2*A0(MHH2) + (-1.D0*MHH2 + MW2 + x)*A0(MW2) + MHH2*A0(GaugeXiW*MW2) + GaugeXiW&
  &*MW2*A0(GaugeXiW*MW2) - 1.D0*x*A0(GaugeXiW*MW2) + 2.D0*MHH2*MW2*B0(x, MHH2, MW2) + 2.D0*MHH2*x*B0(x, MHH2, MW2) + 2.D0*MW2*x*B&
  &0(x, MHH2, MW2) - 2.D0*MHH2*x*B0(x, MHH2, GaugeXiW*MW2) - 1.D0*B0(x, MHH2, MW2)*DBLE(MHH**INT(4.D0)) + B0(x, MHH2, GaugeXiW*MW&
  &2)*DBLE(MHH**INT(4.D0)) - 1.D0*B0(x, MHH2, MW2)*DBLE(MW**INT(4.D0)) - 1.D0*B0(x, MHH2, MW2)*DBLE(x**INT(2.D0)) + B0(x, MHH2, G&
  &augeXiW*MW2)*DBLE(x**INT(2.D0))))/ (MW2*PI2*SW2)

  totalAmplitude = (0D0,0D0)
 do j=1,73
  totalAmplitude = totalAmplitude + amplitudes(j)
 end do
 SelfGpHpAlter = totalAmplitude
end function SelfGpHpAlter

