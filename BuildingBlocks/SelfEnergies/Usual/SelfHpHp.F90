double complex function SelfHpHpUsual(x)
 use constants
 implicit none
#include "looptools.h"
 double precision, intent(in) :: x
 integer :: j
 double complex :: totalAmplitude
 double complex :: amplitudes(31)

 amplitudes(1) = (-0.0078125D0*(-2.D0*CBA*EL2*MHH2*S2A*SAB + 2.D0*CAB*EL2*Mh02*(-1.D0*S2A + S2B)*SBA + EL2*S2B*(Mh02*S2A - 2.D0*M&
  &Hp2*S2B)*SBA2 + CAB2*(-4.D0*EL2*Mh02 + 4.D0*Lambda5*MW2*SW2) + CBA2*(EL2*MHH2*S2A*S2B + 4.D0*C2B2*Lambda5*MW2*SW2))*A0(Mh02))/&
  &(MW2*PI2*S2B2*SW2)

 amplitudes(2) = (0.0078125D0*(CBA2*EL2*S2B*(MHH2*S2A + 2.D0*MHp2*S2B) - 2.D0*CBA*EL2*MHH2*(S2A + S2B)*SAB + EL2*(4.D0*MHH2*SAB2 &
  &+ Mh02*S2A*SBA*(-2.D0*CAB + S2B*SBA)) - 4.D0*Lambda5*MW2*(SAB2 + C2B2*SBA2)*SW2)*A0(MHH2))/(MW2*PI2*S2B2*SW2)

 amplitudes(3) = (0.0078125D0*(4.D0*CAB2*EL2*Mh02 + CBA2*EL2*MHH2*S2B2 - 4.D0*CBA*EL2*MHH2*S2B*SAB + 4.D0*EL2*MHH2*SAB2 - 4.D0*CA&
  &B*EL2*Mh02*S2B*SBA + EL2*Mh02*S2B2*SBA2 - 8.D0*C2B2*Lambda5*MW2*SW2)*A0(MA02))/(MW2*PI2*S2B2*SW2)

 amplitudes(4) = (-0.0078125D0*(CBA2*EL2*MHH2*S2B - 2.D0*CBA*EL2*MHH2*SAB + EL2*(-2.D0*MHp2*S2B + Mh02*SBA*(-2.D0*CAB + S2B*SBA))&
  & + 4.D0*Lambda5*MW2*S2B*SW2)* A0(GaugeXiZ*MZ2))/(MW2*PI2*S2B*SW2)

 amplitudes(5) = (0.03125D0*(4.D0*CAB2*EL2*Mh02 + CBA2*EL2*MHH2*S2B2 - 4.D0*CBA*EL2*MHH2*S2B*SAB + 4.D0*EL2*MHH2*SAB2 - 4.D0*CAB*&
  &EL2*Mh02*S2B*SBA + EL2*Mh02*S2B2*SBA2 - 8.D0*C2B2*Lambda5*MW2*SW2)*A0(MHp2))/(MW2*PI2*S2B2*SW2)

 amplitudes(6) = (-0.015625D0*(-1.D0*EL2*(MA02*S2B - 1.D0*Mh02*(S2A - 2.D0*CBA2*S2B) + MHH2*(S2A + 2.D0*S2B*SBA2)) + 4.D0*Lambda5&
  &*MW2*S2B*SW2)*A0(GaugeXiW*MW2))/ (MW2*PI2*S2B*SW2)

 amplitudes(7) = 0.D0

 amplitudes(8) = (-0.015625D0*EL2*(2.D0*MZ2 - 3.D0*A0(MZ2) - 1.D0*GaugeXiZ*A0(GaugeXiZ*MZ2))*DBLE((CW2 - 1.D0*SW2)**INT(2.D0)))/(&
  &CW2*PI2*SW2)

 amplitudes(9) = (0.03125D0*EL2*(-2.D0*MW2 + 3.D0*A0(MW2) + GaugeXiW*A0(GaugeXiW*MW2)))/(PI2*SW2)

 amplitudes(10) = (-0.03125D0*EL2*ME2*(A0(ME2) + (ME2 - 1.D0*x)*B0(x, 0.D0, ME2))*DBLE(Yuk6**INT(2.D0)))/(MW2*PI2*SW2)

 amplitudes(11) = (-0.03125D0*EL2*MM2*(A0(MM2) + (MM2 - 1.D0*x)*B0(x, 0.D0, MM2))*DBLE(Yuk6**INT(2.D0)))/(MW2*PI2*SW2)

 amplitudes(12) = (-0.03125D0*EL2*ML2*(A0(ML2) + (ML2 - 1.D0*x)*B0(x, 0.D0, ML2))*DBLE(Yuk6**INT(2.D0)))/(MW2*PI2*SW2)

 amplitudes(13) = (-0.09375D0*CKM11*CKMC11*EL2*(A0(MD2)*(MU2 + MD2*TB2*DBLE(Yuk3**INT(2.D0))) + A0(MU2)*(MU2 + MD2*TB2*DBLE(Yuk3*&
  &*INT(2.D0))) + B0(x, MD2, MU2)*(-1.D0*MU2*x + DBLE(MU**INT(4.D0)) + TB2*DBLE(MD**INT(4.D0))*DBLE(Yuk3**INT(2.D0)) + MD2*(-1.D0&
  &*TB2*x*DBLE(Yuk3**INT(2.D0)) + MU2*(1.D0 + 4.D0*TB*Yuk3 + TB2*DBLE(Yuk3**INT(2.D0)))))))/(MW2*PI2*SW2*TB2)

 amplitudes(14) = (-0.09375D0*CKM21*CKMC21*EL2*(A0(MC2)*(MC2 + MD2*TB2*DBLE(Yuk3**INT(2.D0))) + A0(MD2)*(MC2 + MD2*TB2*DBLE(Yuk3*&
  &*INT(2.D0))) + B0(x, MC2, MD2)*(DBLE(MC**INT(4.D0)) + MD2*TB2*(MD2 - 1.D0*x)*DBLE(Yuk3**INT(2.D0)) + MC2*(-1.D0*x + MD2*(1.D0 &
  &+ 4.D0*TB*Yuk3 + TB2*DBLE(Yuk3**INT(2.D0)))))))/(MW2*PI2*SW2*TB2)

 amplitudes(15) = (-0.09375D0*CKM31*CKMC31*EL2*(A0(MD2)*(MT2 + MD2*TB2*DBLE(Yuk3**INT(2.D0))) + A0(MT2)*(MT2 + MD2*TB2*DBLE(Yuk3*&
  &*INT(2.D0))) + B0(x, MD2, MT2)*(-1.D0*MT2*x + DBLE(MT**INT(4.D0)) + TB2*DBLE(MD**INT(4.D0))*DBLE(Yuk3**INT(2.D0)) + MD2*(-1.D0&
  &*TB2*x*DBLE(Yuk3**INT(2.D0)) + MT2*(1.D0 + 4.D0*TB*Yuk3 + TB2*DBLE(Yuk3**INT(2.D0)))))))/(MW2*PI2*SW2*TB2)

 amplitudes(16) = (-0.09375D0*CKM12*CKMC12*EL2*(A0(MS2)*(MU2 + MS2*TB2*DBLE(Yuk3**INT(2.D0))) + A0(MU2)*(MU2 + MS2*TB2*DBLE(Yuk3*&
  &*INT(2.D0))) + B0(x, MS2, MU2)*(-1.D0*MU2*x + DBLE(MU**INT(4.D0)) + TB2*DBLE(MS**INT(4.D0))*DBLE(Yuk3**INT(2.D0)) + MS2*(-1.D0&
  &*TB2*x*DBLE(Yuk3**INT(2.D0)) + MU2*(1.D0 + 4.D0*TB*Yuk3 + TB2*DBLE(Yuk3**INT(2.D0)))))))/(MW2*PI2*SW2*TB2)

 amplitudes(17) = (-0.09375D0*CKM22*CKMC22*EL2*(A0(MC2)*(MC2 + MS2*TB2*DBLE(Yuk3**INT(2.D0))) + A0(MS2)*(MC2 + MS2*TB2*DBLE(Yuk3*&
  &*INT(2.D0))) + B0(x, MC2, MS2)*(DBLE(MC**INT(4.D0)) + MS2*TB2*(MS2 - 1.D0*x)*DBLE(Yuk3**INT(2.D0)) + MC2*(-1.D0*x + MS2*(1.D0 &
  &+ 4.D0*TB*Yuk3 + TB2*DBLE(Yuk3**INT(2.D0)))))))/(MW2*PI2*SW2*TB2)

 amplitudes(18) = (-0.09375D0*CKM32*CKMC32*EL2*(A0(MS2)*(MT2 + MS2*TB2*DBLE(Yuk3**INT(2.D0))) + A0(MT2)*(MT2 + MS2*TB2*DBLE(Yuk3*&
  &*INT(2.D0))) + B0(x, MS2, MT2)*(-1.D0*MT2*x + DBLE(MT**INT(4.D0)) + TB2*DBLE(MS**INT(4.D0))*DBLE(Yuk3**INT(2.D0)) + MS2*(-1.D0&
  &*TB2*x*DBLE(Yuk3**INT(2.D0)) + MT2*(1.D0 + 4.D0*TB*Yuk3 + TB2*DBLE(Yuk3**INT(2.D0)))))))/(MW2*PI2*SW2*TB2)

 amplitudes(19) = (-0.09375D0*CKM13*CKMC13*EL2*(A0(MB2)*(MU2 + MB2*TB2*DBLE(Yuk3**INT(2.D0))) + A0(MU2)*(MU2 + MB2*TB2*DBLE(Yuk3*&
  &*INT(2.D0))) + B0(x, MB2, MU2)*(-1.D0*MU2*x + DBLE(MU**INT(4.D0)) + TB2*DBLE(MB**INT(4.D0))*DBLE(Yuk3**INT(2.D0)) + MB2*(-1.D0&
  &*TB2*x*DBLE(Yuk3**INT(2.D0)) + MU2*(1.D0 + 4.D0*TB*Yuk3 + TB2*DBLE(Yuk3**INT(2.D0)))))))/(MW2*PI2*SW2*TB2)

 amplitudes(20) = (-0.09375D0*CKM23*CKMC23*EL2*(A0(MB2)*(MC2 + MB2*TB2*DBLE(Yuk3**INT(2.D0))) + A0(MC2)*(MC2 + MB2*TB2*DBLE(Yuk3*&
  &*INT(2.D0))) + B0(x, MB2, MC2)*(-1.D0*MC2*x + DBLE(MC**INT(4.D0)) + TB2*DBLE(MB**INT(4.D0))*DBLE(Yuk3**INT(2.D0)) + MB2*(-1.D0&
  &*TB2*x*DBLE(Yuk3**INT(2.D0)) + MC2*(1.D0 + 4.D0*TB*Yuk3 + TB2*DBLE(Yuk3**INT(2.D0)))))))/(MW2*PI2*SW2*TB2)

 amplitudes(21) = (-0.09375D0*CKM33*CKMC33*EL2*(A0(MB2)*(MT2 + MB2*TB2*DBLE(Yuk3**INT(2.D0))) + A0(MT2)*(MT2 + MB2*TB2*DBLE(Yuk3*&
  &*INT(2.D0))) + B0(x, MB2, MT2)*(-1.D0*MT2*x + DBLE(MT**INT(4.D0)) + TB2*DBLE(MB**INT(4.D0))*DBLE(Yuk3**INT(2.D0)) + MB2*(-1.D0&
  &*TB2*x*DBLE(Yuk3**INT(2.D0)) + MT2*(1.D0 + 4.D0*TB*Yuk3 + TB2*DBLE(Yuk3**INT(2.D0)))))))/(MW2*PI2*SW2*TB2)

 amplitudes(22) = (0.015625D0*B0(x, Mh02, MHp2)*DBLE((EL2*(Mh02 - 2.D0*MHp2)*S2B*SBA + CAB*(-2.D0*EL2*Mh02 + 4.D0*Lambda5*MW2*SW2&
  &))**INT(2.D0)))/ (EL2*MW2*PI2*S2B2*SW2)

 amplitudes(23) = (0.015625D0*B0(x, MHH2, MHp2)*DBLE((CBA*EL2*(MHH2 - 2.D0*MHp2)*S2B - 2.D0*EL2*MHH2*SAB + 4.D0*Lambda5*MW2*SAB*S&
  &W2)**INT(2.D0)))/ (EL2*MW2*PI2*S2B2*SW2)

 amplitudes(24) = (0.015625D0*CBA2*EL2*B0(x, Mh02, GaugeXiW*MW2)*DBLE((Mh02 - 1.D0*MHp2)**INT(2.D0)))/(MW2*PI2*SW2)

 amplitudes(25) = (0.015625D0*EL2*SBA2*B0(x, MHH2, GaugeXiW*MW2)*DBLE((MHH2 - 1.D0*MHp2)**INT(2.D0)))/(MW2*PI2*SW2)

 amplitudes(26) = (0.015625D0*EL2*B0(x, MA02, GaugeXiW*MW2)*DBLE((MA02 - 1.D0*MHp2)**INT(2.D0)))/(MW2*PI2*SW2)

 amplitudes(27) = (0.0625D0*EL2*(A0(MHp2) - 2.D0*(MHp2 + x)*B0(x, 0.D0, MHp2) - 1.D0*(-1.D0 + GaugeXiA)*C0Mine(DBLE(0.D0), DBLE(x&
  &), DBLE(x), DBLE(0.D0), DBLE(0.D0), DBLE(MHp2))*DBLE((MHp2 - 1.D0*x)**INT(2.D0))))/PI2

 amplitudes(28) = (0.015625D0*EL2*DBLE((CW2 - 1.D0*SW2)**INT(2.D0))*(MZ2*A0(MHp2) + (MHp2 - 1.D0*MZ2 - 1.D0*x)*A0(MZ2) - 1.D0*MHp&
  &2*A0(GaugeXiZ*MZ2) - 1.D0*GaugeXiZ*MZ2*A0(GaugeXiZ*MZ2) + x*A0(GaugeXiZ*MZ2) - 2.D0*MHp2*MZ2*B0(x, MHp2, MZ2) - 2.D0*MHp2*x*B0&
  &(x, MHp2, MZ2) - 2.D0*MZ2*x*B0(x, MHp2, MZ2) + 2.D0*MHp2*x*B0(x, MHp2, GaugeXiZ*MZ2) + B0(x, MHp2, MZ2)*DBLE(MHp**INT(4.D0)) -&
  & 1.D0*B0(x, MHp2, GaugeXiZ*MZ2)*DBLE(MHp**INT(4.D0)) + B0(x, MHp2, MZ2)*DBLE(MZ**INT(4.D0)) + B0(x, MHp2, MZ2)*DBLE(x**INT(2.D&
  &0)) - 1.D0*B0(x, MHp2, GaugeXiZ*MZ2)*DBLE(x**INT(2.D0))))/(CW2*MZ2*PI2*SW2)

 amplitudes(29) = (0.015625D0*CBA2*EL2*(MW2*A0(Mh02) + (Mh02 - 1.D0*MW2 - 1.D0*x)*A0(MW2) - 1.D0*Mh02*A0(GaugeXiW*MW2) - 1.D0*Gau&
  &geXiW*MW2*A0(GaugeXiW*MW2) + x*A0(GaugeXiW*MW2) - 2.D0*Mh02*MW2*B0(x, Mh02, MW2) - 2.D0*Mh02*x*B0(x, Mh02, MW2) - 2.D0*MW2*x*B&
  &0(x, Mh02, MW2) + 2.D0*Mh02*x*B0(x, Mh02, GaugeXiW*MW2) + B0(x, Mh02, MW2)*DBLE(Mh0**INT(4.D0)) - 1.D0*B0(x, Mh02, GaugeXiW*MW&
  &2)*DBLE(Mh0**INT(4.D0)) + B0(x, Mh02, MW2)*DBLE(MW**INT(4.D0)) + B0(x, Mh02, MW2)*DBLE(x**INT(2.D0)) - 1.D0*B0(x, Mh02, GaugeX&
  &iW*MW2)*DBLE(x**INT(2.D0))))/(MW2*PI2*SW2)

 amplitudes(30) = (0.015625D0*EL2*SBA2*(MW2*A0(MHH2) + (MHH2 - 1.D0*MW2 - 1.D0*x)*A0(MW2) - 1.D0*MHH2*A0(GaugeXiW*MW2) - 1.D0*Gau&
  &geXiW*MW2*A0(GaugeXiW*MW2) + x*A0(GaugeXiW*MW2) - 2.D0*MHH2*MW2*B0(x, MHH2, MW2) - 2.D0*MHH2*x*B0(x, MHH2, MW2) - 2.D0*MW2*x*B&
  &0(x, MHH2, MW2) + 2.D0*MHH2*x*B0(x, MHH2, GaugeXiW*MW2) + B0(x, MHH2, MW2)*DBLE(MHH**INT(4.D0)) - 1.D0*B0(x, MHH2, GaugeXiW*MW&
  &2)*DBLE(MHH**INT(4.D0)) + B0(x, MHH2, MW2)*DBLE(MW**INT(4.D0)) + B0(x, MHH2, MW2)*DBLE(x**INT(2.D0)) - 1.D0*B0(x, MHH2, GaugeX&
  &iW*MW2)*DBLE(x**INT(2.D0))))/(MW2*PI2*SW2)

 amplitudes(31) = (0.015625D0*EL2*(MW2*A0(MA02) + (MA02 - 1.D0*MW2 - 1.D0*x)*A0(MW2) - 1.D0*MA02*A0(GaugeXiW*MW2) - 1.D0*GaugeXiW&
  &*MW2*A0(GaugeXiW*MW2) + x*A0(GaugeXiW*MW2) - 2.D0*MA02*MW2*B0(x, MA02, MW2) - 2.D0*MA02*x*B0(x, MA02, MW2) - 2.D0*MW2*x*B0(x, &
  &MA02, MW2) + 2.D0*MA02*x*B0(x, MA02, GaugeXiW*MW2) + B0(x, MA02, MW2)*DBLE(MA0**INT(4.D0)) - 1.D0*B0(x, MA02, GaugeXiW*MW2)*DB&
  &LE(MA0**INT(4.D0)) + B0(x, MA02, MW2)*DBLE(MW**INT(4.D0)) + B0(x, MA02, MW2)*DBLE(x**INT(2.D0)) - 1.D0*B0(x, MA02, GaugeXiW*MW&
  &2)*DBLE(x**INT(2.D0))))/(MW2*PI2*SW2)

  totalAmplitude = (0D0,0D0)
 do j=1,31
  totalAmplitude = totalAmplitude + amplitudes(j)
 end do
 SelfHpHpUsual = totalAmplitude
end function SelfHpHpUsual

