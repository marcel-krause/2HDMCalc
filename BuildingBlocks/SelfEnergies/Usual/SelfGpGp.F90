double complex function SelfGpGpUsual(x)
 use constants
 implicit none
#include "looptools.h"
 double precision, intent(in) :: x
 integer :: j
 double complex :: totalAmplitude
 double complex :: amplitudes(35)

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

 amplitudes(10) = (-0.03125D0*EL2*ME2*(A0(ME2) + (ME2 - 1.D0*x)*B0(x, 0.D0, ME2)))/(MW2*PI2*SW2)

 amplitudes(11) = (-0.03125D0*EL2*MM2*(A0(MM2) + (MM2 - 1.D0*x)*B0(x, 0.D0, MM2)))/(MW2*PI2*SW2)

 amplitudes(12) = (-0.03125D0*EL2*ML2*(A0(ML2) + (ML2 - 1.D0*x)*B0(x, 0.D0, ML2)))/(MW2*PI2*SW2)

 amplitudes(13) = (-0.09375D0*CKM11*CKMC11*EL2*((MD2 + MU2)*A0(MD2) + (MD2 + MU2)*A0(MU2) + B0(x, MD2, MU2)*(-1.D0*MU2*x - 1.D0*M&
  &D2*(2.D0*MU2 + x) + DBLE(MD**INT(4.D0)) + DBLE(MU**INT(4.D0)))))/(MW2*PI2*SW2)

 amplitudes(14) = (-0.09375D0*CKM21*CKMC21*EL2*((MC2 + MD2)*A0(MC2) + (MC2 + MD2)*A0(MD2) + B0(x, MC2, MD2)*(-1.D0*MD2*x - 1.D0*M&
  &C2*(2.D0*MD2 + x) + DBLE(MC**INT(4.D0)) + DBLE(MD**INT(4.D0)))))/(MW2*PI2*SW2)

 amplitudes(15) = (-0.09375D0*CKM31*CKMC31*EL2*((MD2 + MT2)*A0(MD2) + (MD2 + MT2)*A0(MT2) + B0(x, MD2, MT2)*(-1.D0*MT2*x - 1.D0*M&
  &D2*(2.D0*MT2 + x) + DBLE(MD**INT(4.D0)) + DBLE(MT**INT(4.D0)))))/(MW2*PI2*SW2)

 amplitudes(16) = (-0.09375D0*CKM12*CKMC12*EL2*((MS2 + MU2)*A0(MS2) + (MS2 + MU2)*A0(MU2) + B0(x, MS2, MU2)*(-1.D0*MU2*x - 1.D0*M&
  &S2*(2.D0*MU2 + x) + DBLE(MS**INT(4.D0)) + DBLE(MU**INT(4.D0)))))/(MW2*PI2*SW2)

 amplitudes(17) = (-0.09375D0*CKM22*CKMC22*EL2*((MC2 + MS2)*A0(MC2) + (MC2 + MS2)*A0(MS2) + B0(x, MC2, MS2)*(-1.D0*MS2*x - 1.D0*M&
  &C2*(2.D0*MS2 + x) + DBLE(MC**INT(4.D0)) + DBLE(MS**INT(4.D0)))))/(MW2*PI2*SW2)

 amplitudes(18) = (-0.09375D0*CKM32*CKMC32*EL2*((MS2 + MT2)*A0(MS2) + (MS2 + MT2)*A0(MT2) + B0(x, MS2, MT2)*(-1.D0*MT2*x - 1.D0*M&
  &S2*(2.D0*MT2 + x) + DBLE(MS**INT(4.D0)) + DBLE(MT**INT(4.D0)))))/(MW2*PI2*SW2)

 amplitudes(19) = (-0.09375D0*CKM13*CKMC13*EL2*((MB2 + MU2)*A0(MB2) + (MB2 + MU2)*A0(MU2) + B0(x, MB2, MU2)*(-1.D0*MU2*x - 1.D0*M&
  &B2*(2.D0*MU2 + x) + DBLE(MB**INT(4.D0)) + DBLE(MU**INT(4.D0)))))/(MW2*PI2*SW2)

 amplitudes(20) = (-0.09375D0*CKM23*CKMC23*EL2*((MB2 + MC2)*A0(MB2) + (MB2 + MC2)*A0(MC2) + B0(x, MB2, MC2)*(-1.D0*MC2*x - 1.D0*M&
  &B2*(2.D0*MC2 + x) + DBLE(MB**INT(4.D0)) + DBLE(MC**INT(4.D0)))))/(MW2*PI2*SW2)

 amplitudes(21) = (-0.09375D0*CKM33*CKMC33*EL2*((MB2 + MT2)*A0(MB2) + (MB2 + MT2)*A0(MT2) + B0(x, MB2, MT2)*(-1.D0*MT2*x - 1.D0*M&
  &B2*(2.D0*MT2 + x) + DBLE(MB**INT(4.D0)) + DBLE(MT**INT(4.D0)))))/(MW2*PI2*SW2)

 amplitudes(22) = (0.015625D0*CBA2*EL2*B0(x, Mh02, MHp2)*DBLE((Mh02 - 1.D0*MHp2)**INT(2.D0)))/(MW2*PI2*SW2)

 amplitudes(23) = (0.015625D0*EL2*SBA2*B0(x, MHH2, MHp2)*DBLE((MHH2 - 1.D0*MHp2)**INT(2.D0)))/(MW2*PI2*SW2)

 amplitudes(24) = (0.015625D0*EL2*B0(x, MA02, MHp2)*DBLE((MA02 - 1.D0*MHp2)**INT(2.D0)))/(MW2*PI2*SW2)

 amplitudes(25) = (0.015625D0*EL2*SBA2*B0(x, Mh02, GaugeXiW*MW2)*DBLE(Mh0**INT(4.D0)))/(MW2*PI2*SW2)

 amplitudes(26) = (0.015625D0*CBA2*EL2*B0(x, MHH2, GaugeXiW*MW2)*DBLE(MHH**INT(4.D0)))/(MW2*PI2*SW2)

 amplitudes(27) = (0.015625D0*EL2*GaugeXiW*GaugeXiZ*MW2*(CW2 - 1.D0*SW2)*B0(x, GaugeXiW*MW2, GaugeXiZ*MZ2))/(CW2*PI2*SW2)

 amplitudes(28) = (0.015625D0*EL2*GaugeXiW*GaugeXiZ*MW2*(CW2 - 1.D0*SW2)*B0(x, GaugeXiW*MW2, GaugeXiZ*MZ2))/(CW2*PI2*SW2)

 amplitudes(29) = (0.015625D0*EL2*(-8.D0*MW2 - 1.D0*(-1.D0 + GaugeXiA)*A0(MW2) + (-1.D0 + GaugeXiA)*A0(GaugeXiW*MW2) + 10.D0*MW2*&
  &B0(x, 0.D0, MW2) + 2.D0*GaugeXiA*MW2*B0(x, 0.D0, MW2) - 2.D0*x*B0(x, 0.D0, MW2) + 2.D0*GaugeXiA*x*B0(x, 0.D0, MW2) + 2.D0*Gaug&
  &eXiW*MW2*B0(x, 0.D0, GaugeXiW*MW2) + 2.D0*GaugeXiA*GaugeXiW*MW2*B0(x, 0.D0, GaugeXiW*MW2) + 2.D0*x*B0(x, 0.D0, GaugeXiW*MW2) -&
  & 2.D0*GaugeXiA*x*B0(x, 0.D0, GaugeXiW*MW2) - 2.D0*MW2*x*C0Mine(DBLE(0.D0), DBLE(x), DBLE(x), DBLE(0.D0), DBLE(0.D0), DBLE(MW2)&
  &) + 2.D0*GaugeXiA*MW2*x*C0Mine(DBLE(0.D0), DBLE(x), DBLE(x), DBLE(0.D0), DBLE(0.D0), DBLE(MW2)) + 2.D0*GaugeXiW*MW2*x*C0Mine(D&
  &BLE(0.D0), DBLE(x), DBLE(x), DBLE(0.D0), DBLE(0.D0), DBLE(GaugeXiW*MW2)) - 2.D0*GaugeXiA*GaugeXiW*MW2*x*C0Mine(DBLE(0.D0), DBL&
  &E(x), DBLE(x), DBLE(0.D0), DBLE(0.D0), DBLE(GaugeXiW*MW2)) + C0Mine(DBLE(0.D0), DBLE(x), DBLE(x), DBLE(0.D0), DBLE(0.D0), DBLE&
  &(MW2))*DBLE(MW**INT(4.D0)) - 1.D0*GaugeXiA*C0Mine(DBLE(0.D0), DBLE(x), DBLE(x), DBLE(0.D0), DBLE(0.D0), DBLE(MW2))*DBLE(MW**IN&
  &T(4.D0)) - 1.D0*C0Mine(DBLE(0.D0), DBLE(x), DBLE(x), DBLE(0.D0), DBLE(0.D0), DBLE(GaugeXiW*MW2))*DBLE(GaugeXiW**INT(2.D0))*DBL&
  &E(MW**INT(4.D0)) + GaugeXiA*C0Mine(DBLE(0.D0), DBLE(x), DBLE(x), DBLE(0.D0), DBLE(0.D0), DBLE(GaugeXiW*MW2))*DBLE(GaugeXiW**IN&
  &T(2.D0))*DBLE(MW**INT(4.D0)) + C0Mine(DBLE(0.D0), DBLE(x), DBLE(x), DBLE(0.D0), DBLE(0.D0), DBLE(MW2))*DBLE(x**INT(2.D0)) - 1.&
  &D0*GaugeXiA*C0Mine(DBLE(0.D0), DBLE(x), DBLE(x), DBLE(0.D0), DBLE(0.D0), DBLE(MW2))*DBLE(x**INT(2.D0)) - 1.D0*C0Mine(DBLE(0.D0&
  &), DBLE(x), DBLE(x), DBLE(0.D0), DBLE(0.D0), DBLE(GaugeXiW*MW2))*DBLE(x**INT(2.D0)) + GaugeXiA*C0Mine(DBLE(0.D0), DBLE(x), DBL&
  &E(x), DBLE(0.D0), DBLE(0.D0), DBLE(GaugeXiW*MW2))*DBLE(x**INT(2.D0))))/PI2

 amplitudes(30) = (0.015625D0*EL2*SW2*(-8.D0*MW2*MZ2 - 1.D0*(-1.D0 + GaugeXiZ)*MZ2*A0(MW2) - 1.D0*MZ2*A0(GaugeXiW*MW2) + GaugeXiZ&
  &*MZ2*A0(GaugeXiW*MW2) - 1.D0*(-1.D0 + GaugeXiW)*MW2*A0(MZ2) - 1.D0*MW2*A0(GaugeXiZ*MZ2) + GaugeXiW*MW2*A0(GaugeXiZ*MZ2) + 10.D&
  &0*MW2*MZ2*B0(x, MW2, MZ2) - 2.D0*MW2*x*B0(x, MW2, MZ2) - 2.D0*MZ2*x*B0(x, MW2, MZ2) + 2.D0*GaugeXiZ*MW2*MZ2*B0(x, MW2, GaugeXi&
  &Z*MZ2) + 2.D0*MW2*x*B0(x, MW2, GaugeXiZ*MZ2) + 2.D0*GaugeXiZ*MZ2*x*B0(x, MW2, GaugeXiZ*MZ2) + 2.D0*GaugeXiW*MW2*MZ2*B0(x, Gaug&
  &eXiW*MW2, MZ2) + 2.D0*GaugeXiW*MW2*x*B0(x, GaugeXiW*MW2, MZ2) + 2.D0*MZ2*x*B0(x, GaugeXiW*MW2, MZ2) + 2.D0*GaugeXiW*GaugeXiZ*M&
  &W2*MZ2*B0(x, GaugeXiW*MW2, GaugeXiZ*MZ2) - 2.D0*GaugeXiW*MW2*x*B0(x, GaugeXiW*MW2, GaugeXiZ*MZ2) - 2.D0*GaugeXiZ*MZ2*x*B0(x, G&
  &augeXiW*MW2, GaugeXiZ*MZ2) + B0(x, MW2, MZ2)*DBLE(MW**INT(4.D0)) - 1.D0*B0(x, MW2, GaugeXiZ*MZ2)*DBLE(MW**INT(4.D0)) - 1.D0*B0&
  &(x, GaugeXiW*MW2, MZ2)*DBLE(GaugeXiW**INT(2.D0))*DBLE(MW**INT(4.D0)) + B0(x, GaugeXiW*MW2, GaugeXiZ*MZ2)*DBLE(GaugeXiW**INT(2.&
  &D0))*DBLE(MW**INT(4.D0)) + B0(x, MW2, MZ2)*DBLE(MZ**INT(4.D0)) - 1.D0*B0(x, GaugeXiW*MW2, MZ2)*DBLE(MZ**INT(4.D0)) - 1.D0*B0(x&
  &, MW2, GaugeXiZ*MZ2)*DBLE(GaugeXiZ**INT(2.D0))*DBLE(MZ**INT(4.D0)) + B0(x, GaugeXiW*MW2, GaugeXiZ*MZ2)*DBLE(GaugeXiZ**INT(2.D0&
  &))*DBLE(MZ**INT(4.D0)) + B0(x, MW2, MZ2)*DBLE(x**INT(2.D0)) - 1.D0*B0(x, MW2, GaugeXiZ*MZ2)*DBLE(x**INT(2.D0)) - 1.D0*B0(x, Ga&
  &ugeXiW*MW2, MZ2)*DBLE(x**INT(2.D0)) + B0(x, GaugeXiW*MW2, GaugeXiZ*MZ2)*DBLE(x**INT(2.D0))))/(CW2*MZ2*PI2)

 amplitudes(31) = (0.0625D0*EL2*(A0(GaugeXiW*MW2) - 2.D0*(GaugeXiW*MW2 + x)*B0(x, 0.D0, GaugeXiW*MW2) - 1.D0*(-1.D0 + GaugeXiA)*C&
  &0Mine(DBLE(0.D0), DBLE(x), DBLE(x), DBLE(0.D0), DBLE(0.D0), DBLE(GaugeXiW*MW2))* DBLE((-1.D0*GaugeXiW*MW2 + x)**INT(2.D0))))/P&
  &I2

 amplitudes(32) = (0.015625D0*EL2*DBLE((CW2 - 1.D0*SW2)**INT(2.D0))*(MZ2*A0(GaugeXiW*MW2) - 1.D0*(-1.D0*GaugeXiW*MW2 + MZ2 + x)*A&
  &0(MZ2) - 1.D0*GaugeXiW*MW2*A0(GaugeXiZ*MZ2) - 1.D0*GaugeXiZ*MZ2*A0(GaugeXiZ*MZ2) + x*A0(GaugeXiZ*MZ2) - 2.D0*GaugeXiW*MW2*MZ2*&
  &B0(x, GaugeXiW*MW2, MZ2) - 2.D0*GaugeXiW*MW2*x*B0(x, GaugeXiW*MW2, MZ2) - 2.D0*MZ2*x*B0(x, GaugeXiW*MW2, MZ2) + 2.D0*GaugeXiW*&
  &MW2*x*B0(x, GaugeXiW*MW2, GaugeXiZ*MZ2) + B0(x, GaugeXiW*MW2, MZ2)*DBLE(GaugeXiW**INT(2.D0))*DBLE(MW**INT(4.D0)) - 1.D0*B0(x, &
  &GaugeXiW*MW2, GaugeXiZ*MZ2)*DBLE(GaugeXiW**INT(2.D0))*DBLE(MW**INT(4.D0)) + B0(x, GaugeXiW*MW2, MZ2)*DBLE(MZ**INT(4.D0)) + B0(&
  &x, GaugeXiW*MW2, MZ2)*DBLE(x**INT(2.D0)) - 1.D0*B0(x, GaugeXiW*MW2, GaugeXiZ*MZ2)*DBLE(x**INT(2.D0))))/(CW2*MZ2*PI2*SW2)

 amplitudes(33) = (0.015625D0*EL2*SBA2*(MW2*A0(Mh02) + (Mh02 - 1.D0*MW2 - 1.D0*x)*A0(MW2) - 1.D0*Mh02*A0(GaugeXiW*MW2) - 1.D0*Gau&
  &geXiW*MW2*A0(GaugeXiW*MW2) + x*A0(GaugeXiW*MW2) - 2.D0*Mh02*MW2*B0(x, Mh02, MW2) - 2.D0*Mh02*x*B0(x, Mh02, MW2) - 2.D0*MW2*x*B&
  &0(x, Mh02, MW2) + 2.D0*Mh02*x*B0(x, Mh02, GaugeXiW*MW2) + B0(x, Mh02, MW2)*DBLE(Mh0**INT(4.D0)) - 1.D0*B0(x, Mh02, GaugeXiW*MW&
  &2)*DBLE(Mh0**INT(4.D0)) + B0(x, Mh02, MW2)*DBLE(MW**INT(4.D0)) + B0(x, Mh02, MW2)*DBLE(x**INT(2.D0)) - 1.D0*B0(x, Mh02, GaugeX&
  &iW*MW2)*DBLE(x**INT(2.D0))))/(MW2*PI2*SW2)

 amplitudes(34) = (0.015625D0*CBA2*EL2*(MW2*A0(MHH2) + (MHH2 - 1.D0*MW2 - 1.D0*x)*A0(MW2) - 1.D0*MHH2*A0(GaugeXiW*MW2) - 1.D0*Gau&
  &geXiW*MW2*A0(GaugeXiW*MW2) + x*A0(GaugeXiW*MW2) - 2.D0*MHH2*MW2*B0(x, MHH2, MW2) - 2.D0*MHH2*x*B0(x, MHH2, MW2) - 2.D0*MW2*x*B&
  &0(x, MHH2, MW2) + 2.D0*MHH2*x*B0(x, MHH2, GaugeXiW*MW2) + B0(x, MHH2, MW2)*DBLE(MHH**INT(4.D0)) - 1.D0*B0(x, MHH2, GaugeXiW*MW&
  &2)*DBLE(MHH**INT(4.D0)) + B0(x, MHH2, MW2)*DBLE(MW**INT(4.D0)) + B0(x, MHH2, MW2)*DBLE(x**INT(2.D0)) - 1.D0*B0(x, MHH2, GaugeX&
  &iW*MW2)*DBLE(x**INT(2.D0))))/(MW2*PI2*SW2)

 amplitudes(35) = (0.015625D0*EL2*(-1.D0*(MW2 - 1.D0*GaugeXiZ*MZ2 + x)*A0(MW2) + (-1.D0*GaugeXiW*MW2 - 1.D0*GaugeXiZ*MZ2 + x)*A0(&
  &GaugeXiW*MW2) + MW2*A0(GaugeXiZ*MZ2) - 2.D0*GaugeXiZ*MW2*MZ2*B0(x, MW2, GaugeXiZ*MZ2) - 2.D0*MW2*x*B0(x, MW2, GaugeXiZ*MZ2) - &
  &2.D0*GaugeXiZ*MZ2*x*B0(x, MW2, GaugeXiZ*MZ2) + 2.D0*GaugeXiZ*MZ2*x*B0(x, GaugeXiW*MW2, GaugeXiZ*MZ2) + B0(x, MW2, GaugeXiZ*MZ2&
  &)*DBLE(MW**INT(4.D0)) + B0(x, MW2, GaugeXiZ*MZ2)*DBLE(GaugeXiZ**INT(2.D0))*DBLE(MZ**INT(4.D0)) - 1.D0*B0(x, GaugeXiW*MW2, Gaug&
  &eXiZ*MZ2)*DBLE(GaugeXiZ**INT(2.D0))*DBLE(MZ**INT(4.D0)) + B0(x, MW2, GaugeXiZ*MZ2)*DBLE(x**INT(2.D0)) - 1.D0*B0(x, GaugeXiW*MW&
  &2, GaugeXiZ*MZ2)*DBLE(x**INT(2.D0))))/(MW2*PI2*SW2)

  totalAmplitude = (0D0,0D0)
 do j=1,35
  totalAmplitude = totalAmplitude + amplitudes(j)
 end do
 SelfGpGpUsual = totalAmplitude
end function SelfGpGpUsual

