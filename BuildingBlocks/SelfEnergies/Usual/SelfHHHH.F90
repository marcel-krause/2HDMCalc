double complex function SelfHHHHUsual(x)
 use constants
 implicit none
#include "looptools.h"
 double precision, intent(in) :: x
 integer :: j
 double complex :: totalAmplitude
 double complex :: amplitudes(38)

 amplitudes(1) = (0.0078125D0*(3.D0*CBA2*EL2*Mh02*S2A2 + EL2*S2A*(-1.D0*Mh02*S2B + MHH2*(S2B + 3.D0*S2A*SBA2)) + 2.D0*Lambda5*MW2&
  &*(-3.D0*S2A2 + S2B2)*SW2)*A0(Mh02))/ (MW2*PI2*S2B2*SW2)

 amplitudes(2) = (0.0234375D0*A0(MHH2)*(CBA2*EL2*MHH2*S2A2 - 4.D0*CBA*EL2*MHH2*S2A*SAB + EL2*(4.D0*MHH2*SAB2 + Mh02*S2A2*SBA2) - &
  &2.D0*Lambda5*MW2*SW2*DBLE((C2A - 1.D0*C2B)**INT(2.D0))))/(MW2*PI2*S2B2*SW2)

 amplitudes(3) = (0.0078125D0*(CBA2*EL2*S2B*(MHH2*S2A + 2.D0*MA02*S2B) - 2.D0*CBA*EL2*MHH2*(S2A + S2B)*SAB + EL2*(4.D0*MHH2*SAB2 &
  &+ Mh02*S2A*SBA*(-2.D0*CAB + S2B*SBA)) - 4.D0*Lambda5*MW2*(SAB2 + C2B2*SBA2)*SW2)*A0(MA02))/(MW2*PI2*S2B2*SW2)

 amplitudes(4) = (0.0078125D0*(EL2*((-1.D0*Mh02*S2A + 2.D0*MA02*S2B)*SBA2 + MHH2*(S2B + S2A*SBA2)) - 4.D0*Lambda5*MW2*S2B*SBA2*SW&
  &2)*A0(GaugeXiZ*MZ2))/ (MW2*PI2*S2B*SW2)

 amplitudes(5) = (0.015625D0*(CBA2*EL2*S2B*(MHH2*S2A + 2.D0*MHp2*S2B) - 2.D0*CBA*EL2*MHH2*(S2A + S2B)*SAB + EL2*(4.D0*MHH2*SAB2 +&
  & Mh02*S2A*SBA*(-2.D0*CAB + S2B*SBA)) - 4.D0*Lambda5*MW2*(SAB2 + C2B2*SBA2)*SW2)*A0(MHp2))/(MW2*PI2*S2B2*SW2)

 amplitudes(6) = (0.015625D0*(EL2*((-1.D0*Mh02*S2A + 2.D0*MHp2*S2B)*SBA2 + MHH2*(S2B + S2A*SBA2)) - 4.D0*Lambda5*MW2*S2B*SBA2*SW2&
  &)*A0(GaugeXiW*MW2))/ (MW2*PI2*S2B*SW2)

 amplitudes(7) = (0.015625D0*EL2*(-2.D0*MZ2 + 3.D0*A0(MZ2) + GaugeXiZ*A0(GaugeXiZ*MZ2)))/(CW2*PI2*SW2)

 amplitudes(8) = (0.03125D0*EL2*(-2.D0*MW2 + 3.D0*A0(MW2) + GaugeXiW*A0(GaugeXiW*MW2)))/(PI2*SW2)

 amplitudes(9) = (-0.03125D0*EL2*ME2*(2.D0*A0(ME2) + (4.D0*ME2 - 1.D0*x)*B0(x, ME2, ME2))*DBLE(Yuk5**INT(2.D0)))/(MW2*PI2*SW2)

 amplitudes(10) = (-0.03125D0*EL2*MM2*(2.D0*A0(MM2) + (4.D0*MM2 - 1.D0*x)*B0(x, MM2, MM2))*DBLE(Yuk5**INT(2.D0)))/(MW2*PI2*SW2)

 amplitudes(11) = (-0.03125D0*EL2*ML2*(2.D0*A0(ML2) + (4.D0*ML2 - 1.D0*x)*B0(x, ML2, ML2))*DBLE(Yuk5**INT(2.D0)))/(MW2*PI2*SW2)

 amplitudes(12) = (-0.09375D0*EL2*MU2*SA2*(2.D0*A0(MU2) + (4.D0*MU2 - 1.D0*x)*B0(x, MU2, MU2)))/(MW2*PI2*SB2*SW2)

 amplitudes(13) = (-0.09375D0*EL2*MC2*SA2*(2.D0*A0(MC2) + (4.D0*MC2 - 1.D0*x)*B0(x, MC2, MC2)))/(MW2*PI2*SB2*SW2)

 amplitudes(14) = (-0.09375D0*EL2*MT2*SA2*(2.D0*A0(MT2) + (4.D0*MT2 - 1.D0*x)*B0(x, MT2, MT2)))/(MW2*PI2*SB2*SW2)

 amplitudes(15) = (-0.09375D0*EL2*MD2*(2.D0*A0(MD2) + (4.D0*MD2 - 1.D0*x)*B0(x, MD2, MD2))*DBLE(Yuk2**INT(2.D0)))/(MW2*PI2*SW2)

 amplitudes(16) = (-0.09375D0*EL2*MS2*(2.D0*A0(MS2) + (4.D0*MS2 - 1.D0*x)*B0(x, MS2, MS2))*DBLE(Yuk2**INT(2.D0)))/(MW2*PI2*SW2)

 amplitudes(17) = (-0.09375D0*EL2*MB2*(2.D0*A0(MB2) + (4.D0*MB2 - 1.D0*x)*B0(x, MB2, MB2))*DBLE(Yuk2**INT(2.D0)))/(MW2*PI2*SW2)

 amplitudes(18) = (0.0078125D0*CBA2*B0(x, Mh02, Mh02)*DBLE((EL2*(2.D0*Mh02 + MHH2)*S2A + 2.D0*Lambda5*MW2*(-3.D0*S2A + S2B)*SW2)*&
  &*INT(2.D0)))/(EL2*MW2*PI2*S2B2*SW2)

 amplitudes(19) = (0.0703125D0*B0(x, MHH2, MHH2)*DBLE((CBA*EL2*MHH2*S2A - 2.D0*EL2*MHH2*SAB + 4.D0*Lambda5*MW2*SAB*SBA2*SW2)**INT&
  &(2.D0)))/(EL2*MW2*PI2*S2B2*SW2)

 amplitudes(20) = (0.015625D0*SBA2*B0(x, Mh02, MHH2)*DBLE((EL2*(Mh02 + 2.D0*MHH2)*S2A - 2.D0*Lambda5*MW2*(3.D0*S2A + S2B)*SW2)**I&
  &NT(2.D0)))/(EL2*MW2*PI2*S2B2*SW2)

 amplitudes(21) = (0.0078125D0*B0(x, MA02, MA02)*DBLE((CBA*EL2*(2.D0*MA02 - 1.D0*MHH2)*S2B + 2.D0*SAB*(EL2*MHH2 - 2.D0*Lambda5*MW&
  &2*SW2))**INT(2.D0)))/ (EL2*MW2*PI2*S2B2*SW2)

 amplitudes(22) = (0.0078125D0*CBA2*EL2*B0(x, GaugeXiZ*MZ2, GaugeXiZ*MZ2)*DBLE(MHH**INT(4.D0)))/(MW2*PI2*SW2)

 amplitudes(23) = (0.015625D0*EL2*SBA2*B0(x, MA02, GaugeXiZ*MZ2)*DBLE((MA02 - 1.D0*MHH2)**INT(2.D0)))/(MW2*PI2*SW2)

 amplitudes(24) = (0.015625D0*B0(x, MHp2, MHp2)*DBLE((CBA*EL2*(MHH2 - 2.D0*MHp2)*S2B - 2.D0*EL2*MHH2*SAB + 4.D0*Lambda5*MW2*SAB*S&
  &W2)**INT(2.D0)))/ (EL2*MW2*PI2*S2B2*SW2)

 amplitudes(25) = (0.015625D0*CBA2*EL2*B0(x, GaugeXiW*MW2, GaugeXiW*MW2)*DBLE(MHH**INT(4.D0)))/(MW2*PI2*SW2)

 amplitudes(26) = (0.015625D0*EL2*SBA2*B0(x, MHp2, GaugeXiW*MW2)*DBLE((MHH2 - 1.D0*MHp2)**INT(2.D0)))/(MW2*PI2*SW2)

 amplitudes(27) = (0.015625D0*EL2*SBA2*B0(x, MHp2, GaugeXiW*MW2)*DBLE((MHH2 - 1.D0*MHp2)**INT(2.D0)))/(MW2*PI2*SW2)

 amplitudes(28) = (-0.015625D0*CBA2*EL2*MW2*B0(x, GaugeXiZ*MZ2, GaugeXiZ*MZ2)*DBLE(CW**INT(-4.D0))*DBLE(GaugeXiZ**INT(2.D0)))/(PI&
  &2*SW2)

 amplitudes(29) = (-0.015625D0*CBA2*EL2*MW2*B0(x, GaugeXiW*MW2, GaugeXiW*MW2)*DBLE(GaugeXiW**INT(2.D0)))/(PI2*SW2)

 amplitudes(30) = (-0.015625D0*CBA2*EL2*MW2*B0(x, GaugeXiW*MW2, GaugeXiW*MW2)*DBLE(GaugeXiW**INT(2.D0)))/(PI2*SW2)

 amplitudes(31) = (0.0078125D0*CBA2*EL2*MW2*DBLE(CW**INT(-4.D0))*DBLE(MZ**INT(-4.D0))*(-2.D0*(-1.D0 + GaugeXiZ)*MZ2*A0(MZ2) + 2.D&
  &0*(-1.D0 + GaugeXiZ)*MZ2*A0(GaugeXiZ*MZ2) - 4.D0*MZ2*x*B0(x, MZ2, MZ2) + 4.D0*MZ2*x*B0(x, MZ2, GaugeXiZ*MZ2) + 4.D0*GaugeXiZ*M&
  &Z2*x*B0(x, MZ2, GaugeXiZ*MZ2) - 4.D0*GaugeXiZ*MZ2*x*B0(x, GaugeXiZ*MZ2, GaugeXiZ*MZ2) - 8.D0*DBLE(MZ**INT(4.D0)) + 12.D0*B0(x,&
  & MZ2, MZ2)*DBLE(MZ**INT(4.D0)) - 2.D0*B0(x, MZ2, GaugeXiZ*MZ2)*DBLE(MZ**INT(4.D0)) + 4.D0*GaugeXiZ*B0(x, MZ2, GaugeXiZ*MZ2)*DB&
  &LE(MZ**INT(4.D0)) - 2.D0*B0(x, MZ2, GaugeXiZ*MZ2)*DBLE(GaugeXiZ**INT(2.D0))*DBLE(MZ**INT(4.D0)) + 4.D0*B0(x, GaugeXiZ*MZ2, Gau&
  &geXiZ*MZ2)*DBLE(GaugeXiZ**INT(2.D0))*DBLE(MZ**INT(4.D0)) + B0(x, MZ2, MZ2)*DBLE(x**INT(2.D0)) - 2.D0*B0(x, MZ2, GaugeXiZ*MZ2)*&
  &DBLE(x**INT(2.D0)) + B0(x, GaugeXiZ*MZ2, GaugeXiZ*MZ2)*DBLE(x**INT(2.D0))))/(PI2*SW2)

 amplitudes(32) = (0.015625D0*CBA2*EL2*(-2.D0*(-1.D0 + GaugeXiW)*MW2*A0(MW2) + 2.D0*(-1.D0 + GaugeXiW)*MW2*A0(GaugeXiW*MW2) - 4.D&
  &0*MW2*x*B0(x, MW2, MW2) + 4.D0*MW2*x*B0(x, MW2, GaugeXiW*MW2) + 4.D0*GaugeXiW*MW2*x*B0(x, MW2, GaugeXiW*MW2) - 4.D0*GaugeXiW*M&
  &W2*x*B0(x, GaugeXiW*MW2, GaugeXiW*MW2) - 8.D0*DBLE(MW**INT(4.D0)) + 12.D0*B0(x, MW2, MW2)*DBLE(MW**INT(4.D0)) - 2.D0*B0(x, MW2&
  &, GaugeXiW*MW2)*DBLE(MW**INT(4.D0)) + 4.D0*GaugeXiW*B0(x, MW2, GaugeXiW*MW2)*DBLE(MW**INT(4.D0)) - 2.D0*B0(x, MW2, GaugeXiW*MW&
  &2)*DBLE(GaugeXiW**INT(2.D0))*DBLE(MW**INT(4.D0)) + 4.D0*B0(x, GaugeXiW*MW2, GaugeXiW*MW2)*DBLE(GaugeXiW**INT(2.D0))* DBLE(MW**&
  &INT(4.D0)) + B0(x, MW2, MW2)*DBLE(x**INT(2.D0)) - 2.D0*B0(x, MW2, GaugeXiW*MW2)*DBLE(x**INT(2.D0)) + B0(x, GaugeXiW*MW2, Gauge&
  &XiW*MW2)*DBLE(x**INT(2.D0))))/(MW2*PI2*SW2)

 amplitudes(33) = (0.015625D0*EL2*SBA2*(MZ2*A0(MA02) + (MA02 - 1.D0*MZ2 - 1.D0*x)*A0(MZ2) - 1.D0*MA02*A0(GaugeXiZ*MZ2) - 1.D0*Gau&
  &geXiZ*MZ2*A0(GaugeXiZ*MZ2) + x*A0(GaugeXiZ*MZ2) - 2.D0*MA02*MZ2*B0(x, MA02, MZ2) - 2.D0*MA02*x*B0(x, MA02, MZ2) - 2.D0*MZ2*x*B&
  &0(x, MA02, MZ2) + 2.D0*MA02*x*B0(x, MA02, GaugeXiZ*MZ2) + B0(x, MA02, MZ2)*DBLE(MA0**INT(4.D0)) - 1.D0*B0(x, MA02, GaugeXiZ*MZ&
  &2)*DBLE(MA0**INT(4.D0)) + B0(x, MA02, MZ2)*DBLE(MZ**INT(4.D0)) + B0(x, MA02, MZ2)*DBLE(x**INT(2.D0)) - 1.D0*B0(x, MA02, GaugeX&
  &iZ*MZ2)*DBLE(x**INT(2.D0))))/ (CW2*MZ2*PI2*SW2)

 amplitudes(34) = (0.015625D0*CBA2*EL2*((-1.D0*MZ2 + GaugeXiZ*MZ2 - 1.D0*x)*A0(MZ2) + (MZ2 - 2.D0*GaugeXiZ*MZ2 + x)*A0(GaugeXiZ*M&
  &Z2) - 2.D0*MZ2*x*B0(x, MZ2, GaugeXiZ*MZ2) - 2.D0*GaugeXiZ*MZ2*x*B0(x, MZ2, GaugeXiZ*MZ2) + 2.D0*GaugeXiZ*MZ2*x*B0(x, GaugeXiZ*&
  &MZ2, GaugeXiZ*MZ2) + B0(x, MZ2, GaugeXiZ*MZ2)*DBLE(MZ**INT(4.D0)) - 2.D0*GaugeXiZ*B0(x, MZ2, GaugeXiZ*MZ2)*DBLE(MZ**INT(4.D0))&
  & + B0(x, MZ2, GaugeXiZ*MZ2)*DBLE(GaugeXiZ**INT(2.D0))*DBLE(MZ**INT(4.D0)) - 1.D0*B0(x, GaugeXiZ*MZ2, GaugeXiZ*MZ2)*DBLE(GaugeX&
  &iZ**INT(2.D0))*DBLE(MZ**INT(4.D0)) + B0(x, MZ2, GaugeXiZ*MZ2)*DBLE(x**INT(2.D0)) - 1.D0*B0(x, GaugeXiZ*MZ2, GaugeXiZ*MZ2)*DBLE&
  &(x**INT(2.D0))))/(CW2*MZ2*PI2*SW2)

 amplitudes(35) = (0.015625D0*EL2*SBA2*(MW2*A0(MHp2) + (MHp2 - 1.D0*MW2 - 1.D0*x)*A0(MW2) - 1.D0*MHp2*A0(GaugeXiW*MW2) - 1.D0*Gau&
  &geXiW*MW2*A0(GaugeXiW*MW2) + x*A0(GaugeXiW*MW2) - 2.D0*MHp2*MW2*B0(x, MHp2, MW2) - 2.D0*MHp2*x*B0(x, MHp2, MW2) - 2.D0*MW2*x*B&
  &0(x, MHp2, MW2) + 2.D0*MHp2*x*B0(x, MHp2, GaugeXiW*MW2) + B0(x, MHp2, MW2)*DBLE(MHp**INT(4.D0)) - 1.D0*B0(x, MHp2, GaugeXiW*MW&
  &2)*DBLE(MHp**INT(4.D0)) + B0(x, MHp2, MW2)*DBLE(MW**INT(4.D0)) + B0(x, MHp2, MW2)*DBLE(x**INT(2.D0)) - 1.D0*B0(x, MHp2, GaugeX&
  &iW*MW2)*DBLE(x**INT(2.D0))))/(MW2*PI2*SW2)

 amplitudes(36) = (0.015625D0*EL2*SBA2*(MW2*A0(MHp2) + (MHp2 - 1.D0*MW2 - 1.D0*x)*A0(MW2) - 1.D0*MHp2*A0(GaugeXiW*MW2) - 1.D0*Gau&
  &geXiW*MW2*A0(GaugeXiW*MW2) + x*A0(GaugeXiW*MW2) - 2.D0*MHp2*MW2*B0(x, MHp2, MW2) - 2.D0*MHp2*x*B0(x, MHp2, MW2) - 2.D0*MW2*x*B&
  &0(x, MHp2, MW2) + 2.D0*MHp2*x*B0(x, MHp2, GaugeXiW*MW2) + B0(x, MHp2, MW2)*DBLE(MHp**INT(4.D0)) - 1.D0*B0(x, MHp2, GaugeXiW*MW&
  &2)*DBLE(MHp**INT(4.D0)) + B0(x, MHp2, MW2)*DBLE(MW**INT(4.D0)) + B0(x, MHp2, MW2)*DBLE(x**INT(2.D0)) - 1.D0*B0(x, MHp2, GaugeX&
  &iW*MW2)*DBLE(x**INT(2.D0))))/(MW2*PI2*SW2)

 amplitudes(37) = (0.015625D0*CBA2*EL2*((-1.D0*MW2 + GaugeXiW*MW2 - 1.D0*x)*A0(MW2) + (MW2 - 2.D0*GaugeXiW*MW2 + x)*A0(GaugeXiW*M&
  &W2) - 2.D0*MW2*x*B0(x, MW2, GaugeXiW*MW2) - 2.D0*GaugeXiW*MW2*x*B0(x, MW2, GaugeXiW*MW2) + 2.D0*GaugeXiW*MW2*x*B0(x, GaugeXiW*&
  &MW2, GaugeXiW*MW2) + B0(x, MW2, GaugeXiW*MW2)*DBLE(MW**INT(4.D0)) - 2.D0*GaugeXiW*B0(x, MW2, GaugeXiW*MW2)*DBLE(MW**INT(4.D0))&
  & + B0(x, MW2, GaugeXiW*MW2)*DBLE(GaugeXiW**INT(2.D0))*DBLE(MW**INT(4.D0)) - 1.D0*B0(x, GaugeXiW*MW2, GaugeXiW*MW2)*DBLE(GaugeX&
  &iW**INT(2.D0))*DBLE(MW**INT(4.D0)) + B0(x, MW2, GaugeXiW*MW2)*DBLE(x**INT(2.D0)) - 1.D0*B0(x, GaugeXiW*MW2, GaugeXiW*MW2)*DBLE&
  &(x**INT(2.D0))))/(MW2*PI2*SW2)

 amplitudes(38) = (0.015625D0*CBA2*EL2*((-1.D0*MW2 + GaugeXiW*MW2 - 1.D0*x)*A0(MW2) + (MW2 - 2.D0*GaugeXiW*MW2 + x)*A0(GaugeXiW*M&
  &W2) - 2.D0*MW2*x*B0(x, MW2, GaugeXiW*MW2) - 2.D0*GaugeXiW*MW2*x*B0(x, MW2, GaugeXiW*MW2) + 2.D0*GaugeXiW*MW2*x*B0(x, GaugeXiW*&
  &MW2, GaugeXiW*MW2) + B0(x, MW2, GaugeXiW*MW2)*DBLE(MW**INT(4.D0)) - 2.D0*GaugeXiW*B0(x, MW2, GaugeXiW*MW2)*DBLE(MW**INT(4.D0))&
  & + B0(x, MW2, GaugeXiW*MW2)*DBLE(GaugeXiW**INT(2.D0))*DBLE(MW**INT(4.D0)) - 1.D0*B0(x, GaugeXiW*MW2, GaugeXiW*MW2)*DBLE(GaugeX&
  &iW**INT(2.D0))*DBLE(MW**INT(4.D0)) + B0(x, MW2, GaugeXiW*MW2)*DBLE(x**INT(2.D0)) - 1.D0*B0(x, GaugeXiW*MW2, GaugeXiW*MW2)*DBLE&
  &(x**INT(2.D0))))/(MW2*PI2*SW2)

  totalAmplitude = (0D0,0D0)
 do j=1,38
  totalAmplitude = totalAmplitude + amplitudes(j)
 end do
 SelfHHHHUsual = totalAmplitude
end function SelfHHHHUsual

