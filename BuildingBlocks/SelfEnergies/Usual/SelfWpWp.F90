double complex function SelfWpWpUsual(x)
 use constants
 implicit none
#include "looptools.h"
 double precision, intent(in) :: x
 integer :: j
 double complex :: totalAmplitude
 double complex :: amplitudes(37)

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

 amplitudes(10) = (0.003472222222222222D0*EL2*(2.D0*(3.D0*ME2 - 1.D0*x)*x + 3.D0*(ME2 - 2.D0*x)*A0(ME2) - 3.D0*B0(x, 0.D0, ME2)*(&
  &ME2*x + DBLE(ME**INT(4.D0)) - 2.D0*DBLE(x**INT(2.D0)))))/(PI2*SW2*x)

 amplitudes(11) = (0.003472222222222222D0*EL2*(2.D0*(3.D0*MM2 - 1.D0*x)*x + 3.D0*(MM2 - 2.D0*x)*A0(MM2) - 3.D0*B0(x, 0.D0, MM2)*(&
  &MM2*x + DBLE(MM**INT(4.D0)) - 2.D0*DBLE(x**INT(2.D0)))))/(PI2*SW2*x)

 amplitudes(12) = (0.003472222222222222D0*EL2*(2.D0*(3.D0*ML2 - 1.D0*x)*x + 3.D0*(ML2 - 2.D0*x)*A0(ML2) - 3.D0*B0(x, 0.D0, ML2)*(&
  &ML2*x + DBLE(ML**INT(4.D0)) - 2.D0*DBLE(x**INT(2.D0)))))/(PI2*SW2*x)

 amplitudes(13) = (-0.010416666666666666D0*CKM11*CKMC11*EL2*(-6.D0*MD2*x - 6.D0*MU2*x + (-3.D0*MD2 + 3.D0*MU2 + 6.D0*x)*A0(MD2) +&
  & 3.D0*(MD2 - 1.D0*MU2 + 2.D0*x)*A0(MU2) - 6.D0*MD2*MU2*B0(x, MD2, MU2) + 3.D0*MD2*x*B0(x, MD2, MU2) + 3.D0*MU2*x*B0(x, MD2, MU&
  &2) + 3.D0*B0(x, MD2, MU2)*DBLE(MD**INT(4.D0)) + 3.D0*B0(x, MD2, MU2)*DBLE(MU**INT(4.D0)) + 2.D0*DBLE(x**INT(2.D0)) - 6.D0*B0(x&
  &, MD2, MU2)*DBLE(x**INT(2.D0))))/(PI2*SW2*x)

 amplitudes(14) = (-0.010416666666666666D0*CKM21*CKMC21*EL2*(-6.D0*MC2*x - 6.D0*MD2*x + (-3.D0*MC2 + 3.D0*MD2 + 6.D0*x)*A0(MC2) +&
  & 3.D0*(MC2 - 1.D0*MD2 + 2.D0*x)*A0(MD2) - 6.D0*MC2*MD2*B0(x, MC2, MD2) + 3.D0*MC2*x*B0(x, MC2, MD2) + 3.D0*MD2*x*B0(x, MC2, MD&
  &2) + 3.D0*B0(x, MC2, MD2)*DBLE(MC**INT(4.D0)) + 3.D0*B0(x, MC2, MD2)*DBLE(MD**INT(4.D0)) + 2.D0*DBLE(x**INT(2.D0)) - 6.D0*B0(x&
  &, MC2, MD2)*DBLE(x**INT(2.D0))))/(PI2*SW2*x)

 amplitudes(15) = (-0.010416666666666666D0*CKM31*CKMC31*EL2*(-6.D0*MD2*x - 6.D0*MT2*x + (-3.D0*MD2 + 3.D0*MT2 + 6.D0*x)*A0(MD2) +&
  & 3.D0*(MD2 - 1.D0*MT2 + 2.D0*x)*A0(MT2) - 6.D0*MD2*MT2*B0(x, MD2, MT2) + 3.D0*MD2*x*B0(x, MD2, MT2) + 3.D0*MT2*x*B0(x, MD2, MT&
  &2) + 3.D0*B0(x, MD2, MT2)*DBLE(MD**INT(4.D0)) + 3.D0*B0(x, MD2, MT2)*DBLE(MT**INT(4.D0)) + 2.D0*DBLE(x**INT(2.D0)) - 6.D0*B0(x&
  &, MD2, MT2)*DBLE(x**INT(2.D0))))/(PI2*SW2*x)

 amplitudes(16) = (-0.010416666666666666D0*CKM12*CKMC12*EL2*(-6.D0*MS2*x - 6.D0*MU2*x + (-3.D0*MS2 + 3.D0*MU2 + 6.D0*x)*A0(MS2) +&
  & 3.D0*(MS2 - 1.D0*MU2 + 2.D0*x)*A0(MU2) - 6.D0*MS2*MU2*B0(x, MS2, MU2) + 3.D0*MS2*x*B0(x, MS2, MU2) + 3.D0*MU2*x*B0(x, MS2, MU&
  &2) + 3.D0*B0(x, MS2, MU2)*DBLE(MS**INT(4.D0)) + 3.D0*B0(x, MS2, MU2)*DBLE(MU**INT(4.D0)) + 2.D0*DBLE(x**INT(2.D0)) - 6.D0*B0(x&
  &, MS2, MU2)*DBLE(x**INT(2.D0))))/(PI2*SW2*x)

 amplitudes(17) = (-0.010416666666666666D0*CKM22*CKMC22*EL2*(-6.D0*MC2*x - 6.D0*MS2*x + (-3.D0*MC2 + 3.D0*MS2 + 6.D0*x)*A0(MC2) +&
  & 3.D0*(MC2 - 1.D0*MS2 + 2.D0*x)*A0(MS2) - 6.D0*MC2*MS2*B0(x, MC2, MS2) + 3.D0*MC2*x*B0(x, MC2, MS2) + 3.D0*MS2*x*B0(x, MC2, MS&
  &2) + 3.D0*B0(x, MC2, MS2)*DBLE(MC**INT(4.D0)) + 3.D0*B0(x, MC2, MS2)*DBLE(MS**INT(4.D0)) + 2.D0*DBLE(x**INT(2.D0)) - 6.D0*B0(x&
  &, MC2, MS2)*DBLE(x**INT(2.D0))))/(PI2*SW2*x)

 amplitudes(18) = (-0.010416666666666666D0*CKM32*CKMC32*EL2*(-6.D0*MS2*x - 6.D0*MT2*x + (-3.D0*MS2 + 3.D0*MT2 + 6.D0*x)*A0(MS2) +&
  & 3.D0*(MS2 - 1.D0*MT2 + 2.D0*x)*A0(MT2) - 6.D0*MS2*MT2*B0(x, MS2, MT2) + 3.D0*MS2*x*B0(x, MS2, MT2) + 3.D0*MT2*x*B0(x, MS2, MT&
  &2) + 3.D0*B0(x, MS2, MT2)*DBLE(MS**INT(4.D0)) + 3.D0*B0(x, MS2, MT2)*DBLE(MT**INT(4.D0)) + 2.D0*DBLE(x**INT(2.D0)) - 6.D0*B0(x&
  &, MS2, MT2)*DBLE(x**INT(2.D0))))/(PI2*SW2*x)

 amplitudes(19) = (-0.010416666666666666D0*CKM13*CKMC13*EL2*(-6.D0*MB2*x - 6.D0*MU2*x + (-3.D0*MB2 + 3.D0*MU2 + 6.D0*x)*A0(MB2) +&
  & 3.D0*(MB2 - 1.D0*MU2 + 2.D0*x)*A0(MU2) - 6.D0*MB2*MU2*B0(x, MB2, MU2) + 3.D0*MB2*x*B0(x, MB2, MU2) + 3.D0*MU2*x*B0(x, MB2, MU&
  &2) + 3.D0*B0(x, MB2, MU2)*DBLE(MB**INT(4.D0)) + 3.D0*B0(x, MB2, MU2)*DBLE(MU**INT(4.D0)) + 2.D0*DBLE(x**INT(2.D0)) - 6.D0*B0(x&
  &, MB2, MU2)*DBLE(x**INT(2.D0))))/(PI2*SW2*x)

 amplitudes(20) = (-0.010416666666666666D0*CKM23*CKMC23*EL2*(-6.D0*MB2*x - 6.D0*MC2*x + (-3.D0*MB2 + 3.D0*MC2 + 6.D0*x)*A0(MB2) +&
  & 3.D0*(MB2 - 1.D0*MC2 + 2.D0*x)*A0(MC2) - 6.D0*MB2*MC2*B0(x, MB2, MC2) + 3.D0*MB2*x*B0(x, MB2, MC2) + 3.D0*MC2*x*B0(x, MB2, MC&
  &2) + 3.D0*B0(x, MB2, MC2)*DBLE(MB**INT(4.D0)) + 3.D0*B0(x, MB2, MC2)*DBLE(MC**INT(4.D0)) + 2.D0*DBLE(x**INT(2.D0)) - 6.D0*B0(x&
  &, MB2, MC2)*DBLE(x**INT(2.D0))))/(PI2*SW2*x)

 amplitudes(21) = (-0.010416666666666666D0*CKM33*CKMC33*EL2*(-6.D0*MB2*x - 6.D0*MT2*x + (-3.D0*MB2 + 3.D0*MT2 + 6.D0*x)*A0(MB2) +&
  & 3.D0*(MB2 - 1.D0*MT2 + 2.D0*x)*A0(MT2) - 6.D0*MB2*MT2*B0(x, MB2, MT2) + 3.D0*MB2*x*B0(x, MB2, MT2) + 3.D0*MT2*x*B0(x, MB2, MT&
  &2) + 3.D0*B0(x, MB2, MT2)*DBLE(MB**INT(4.D0)) + 3.D0*B0(x, MB2, MT2)*DBLE(MT**INT(4.D0)) + 2.D0*DBLE(x**INT(2.D0)) - 6.D0*B0(x&
  &, MB2, MT2)*DBLE(x**INT(2.D0))))/(PI2*SW2*x)

 amplitudes(22) = (0.001736111111111111D0*CBA2*EL2*(-6.D0*Mh02*x - 6.D0*MHp2*x - 3.D0*(Mh02 - 1.D0*MHp2 + x)*A0(Mh02) + 3.D0*(Mh0&
  &2 - 1.D0*MHp2 - 1.D0*x)*A0(MHp2) - 6.D0*Mh02*MHp2*B0(x, Mh02, MHp2) - 6.D0*Mh02*x*B0(x, Mh02, MHp2) - 6.D0*MHp2*x*B0(x, Mh02, &
  &MHp2) + 3.D0*B0(x, Mh02, MHp2)*DBLE(Mh0**INT(4.D0)) + 3.D0*B0(x, Mh02, MHp2)*DBLE(MHp**INT(4.D0)) + 2.D0*DBLE(x**INT(2.D0)) + &
  &3.D0*B0(x, Mh02, MHp2)*DBLE(x**INT(2.D0))))/(PI2*SW2*x)

 amplitudes(23) = (0.001736111111111111D0*EL2*SBA2*(-6.D0*MHH2*x - 6.D0*MHp2*x - 3.D0*(MHH2 - 1.D0*MHp2 + x)*A0(MHH2) + 3.D0*(MHH&
  &2 - 1.D0*MHp2 - 1.D0*x)*A0(MHp2) - 6.D0*MHH2*MHp2*B0(x, MHH2, MHp2) - 6.D0*MHH2*x*B0(x, MHH2, MHp2) - 6.D0*MHp2*x*B0(x, MHH2, &
  &MHp2) + 3.D0*B0(x, MHH2, MHp2)*DBLE(MHH**INT(4.D0)) + 3.D0*B0(x, MHH2, MHp2)*DBLE(MHp**INT(4.D0)) + 2.D0*DBLE(x**INT(2.D0)) + &
  &3.D0*B0(x, MHH2, MHp2)*DBLE(x**INT(2.D0))))/(PI2*SW2*x)

 amplitudes(24) = (0.001736111111111111D0*EL2*(-6.D0*MA02*x - 6.D0*MHp2*x - 3.D0*(MA02 - 1.D0*MHp2 + x)*A0(MA02) + 3.D0*(MA02 - 1&
  &.D0*MHp2 - 1.D0*x)*A0(MHp2) - 6.D0*MA02*MHp2*B0(x, MA02, MHp2) - 6.D0*MA02*x*B0(x, MA02, MHp2) - 6.D0*MHp2*x*B0(x, MA02, MHp2)&
  & + 3.D0*B0(x, MA02, MHp2)*DBLE(MA0**INT(4.D0)) + 3.D0*B0(x, MA02, MHp2)*DBLE(MHp**INT(4.D0)) + 2.D0*DBLE(x**INT(2.D0)) + 3.D0*&
  &B0(x, MA02, MHp2)*DBLE(x**INT(2.D0))))/(PI2*SW2*x)

 amplitudes(25) = (0.001736111111111111D0*EL2*SBA2*(-6.D0*Mh02*x - 6.D0*GaugeXiW*MW2*x - 3.D0*(Mh02 - 1.D0*GaugeXiW*MW2 + x)*A0(M&
  &h02) + 3.D0*(Mh02 - 1.D0*GaugeXiW*MW2 - 1.D0*x)*A0(GaugeXiW*MW2) - 6.D0*GaugeXiW*Mh02*MW2*B0(x, Mh02, GaugeXiW*MW2) - 6.D0*Mh0&
  &2*x*B0(x, Mh02, GaugeXiW*MW2) - 6.D0*GaugeXiW*MW2*x*B0(x, Mh02, GaugeXiW*MW2) + 3.D0*B0(x, Mh02, GaugeXiW*MW2)*DBLE(Mh0**INT(4&
  &.D0)) + 3.D0*B0(x, Mh02, GaugeXiW*MW2)*DBLE(GaugeXiW**INT(2.D0))*DBLE(MW**INT(4.D0)) + 2.D0*DBLE(x**INT(2.D0)) + 3.D0*B0(x, Mh&
  &02, GaugeXiW*MW2)*DBLE(x**INT(2.D0))))/(PI2*SW2*x)

 amplitudes(26) = (0.001736111111111111D0*CBA2*EL2*(-6.D0*MHH2*x - 6.D0*GaugeXiW*MW2*x - 3.D0*(MHH2 - 1.D0*GaugeXiW*MW2 + x)*A0(M&
  &HH2) + 3.D0*(MHH2 - 1.D0*GaugeXiW*MW2 - 1.D0*x)*A0(GaugeXiW*MW2) - 6.D0*GaugeXiW*MHH2*MW2*B0(x, MHH2, GaugeXiW*MW2) - 6.D0*MHH&
  &2*x*B0(x, MHH2, GaugeXiW*MW2) - 6.D0*GaugeXiW*MW2*x*B0(x, MHH2, GaugeXiW*MW2) + 3.D0*B0(x, MHH2, GaugeXiW*MW2)*DBLE(MHH**INT(4&
  &.D0)) + 3.D0*B0(x, MHH2, GaugeXiW*MW2)*DBLE(GaugeXiW**INT(2.D0))*DBLE(MW**INT(4.D0)) + 2.D0*DBLE(x**INT(2.D0)) + 3.D0*B0(x, MH&
  &H2, GaugeXiW*MW2)*DBLE(x**INT(2.D0))))/(PI2*SW2*x)

 amplitudes(27) = (0.001736111111111111D0*EL2*(-6.D0*GaugeXiW*MW2*x - 6.D0*GaugeXiZ*MZ2*x - 3.D0*(GaugeXiW*MW2 - 1.D0*GaugeXiZ*MZ&
  &2 + x)*A0(GaugeXiW*MW2) - 3.D0*(-1.D0*GaugeXiW*MW2 + GaugeXiZ*MZ2 + x)*A0(GaugeXiZ*MZ2) - 6.D0*GaugeXiW*GaugeXiZ*MW2*MZ2*B0(x,&
  & GaugeXiW*MW2, GaugeXiZ*MZ2) - 6.D0*GaugeXiW*MW2*x*B0(x, GaugeXiW*MW2, GaugeXiZ*MZ2) - 6.D0*GaugeXiZ*MZ2*x*B0(x, GaugeXiW*MW2,&
  & GaugeXiZ*MZ2) + 3.D0*B0(x, GaugeXiW*MW2, GaugeXiZ*MZ2)*DBLE(GaugeXiW**INT(2.D0))*DBLE(MW**INT(4.D0)) + 3.D0*B0(x, GaugeXiW*MW&
  &2, GaugeXiZ*MZ2)*DBLE(GaugeXiZ**INT(2.D0))*DBLE(MZ**INT(4.D0)) + 2.D0*DBLE(x**INT(2.D0)) + 3.D0*B0(x, GaugeXiW*MW2, GaugeXiZ*M&
  &Z2)*DBLE(x**INT(2.D0))))/(PI2*SW2*x)

 amplitudes(28) = (0.001736111111111111D0*EL2*(-2.D0*x*(-3.D0*GaugeXiW*MW2 + x) + 3.D0*(GaugeXiW*MW2 + x)*A0(GaugeXiW*MW2) - 3.D0&
  &*B0(x, 0.D0, GaugeXiW*MW2)*DBLE((-1.D0*GaugeXiW*MW2 + x)**INT(2.D0))))/(PI2*x)

 amplitudes(29) = (0.001736111111111111D0*CW2*EL2*(6.D0*GaugeXiW*MW2*x + 6.D0*GaugeXiZ*MZ2*x + 3.D0*(GaugeXiW*MW2 - 1.D0*GaugeXiZ&
  &*MZ2 + x)*A0(GaugeXiW*MW2) + 3.D0*(-1.D0*GaugeXiW*MW2 + GaugeXiZ*MZ2 + x)*A0(GaugeXiZ*MZ2) + 6.D0*GaugeXiW*GaugeXiZ*MW2*MZ2*B0&
  &(x, GaugeXiW*MW2, GaugeXiZ*MZ2) + 6.D0*GaugeXiW*MW2*x*B0(x, GaugeXiW*MW2, GaugeXiZ*MZ2) + 6.D0*GaugeXiZ*MZ2*x*B0(x, GaugeXiW*M&
  &W2, GaugeXiZ*MZ2) - 3.D0*B0(x, GaugeXiW*MW2, GaugeXiZ*MZ2)*DBLE(GaugeXiW**INT(2.D0))*DBLE(MW**INT(4.D0)) - 3.D0*B0(x, GaugeXiW&
  &*MW2, GaugeXiZ*MZ2)*DBLE(GaugeXiZ**INT(2.D0))*DBLE(MZ**INT(4.D0)) - 2.D0*DBLE(x**INT(2.D0)) - 3.D0*B0(x, GaugeXiW*MW2, GaugeXi&
  &Z*MZ2)*DBLE(x**INT(2.D0))))/(PI2*SW2*x)

 amplitudes(30) = (0.001736111111111111D0*EL2*(-2.D0*x*(-3.D0*GaugeXiW*MW2 + x) + 3.D0*(GaugeXiW*MW2 + x)*A0(GaugeXiW*MW2) - 3.D0&
  &*B0(x, 0.D0, GaugeXiW*MW2)*DBLE((-1.D0*GaugeXiW*MW2 + x)**INT(2.D0))))/(PI2*x)

 amplitudes(31) = (0.001736111111111111D0*CW2*EL2*(6.D0*GaugeXiW*MW2*x + 6.D0*GaugeXiZ*MZ2*x + 3.D0*(GaugeXiW*MW2 - 1.D0*GaugeXiZ&
  &*MZ2 + x)*A0(GaugeXiW*MW2) + 3.D0*(-1.D0*GaugeXiW*MW2 + GaugeXiZ*MZ2 + x)*A0(GaugeXiZ*MZ2) + 6.D0*GaugeXiW*GaugeXiZ*MW2*MZ2*B0&
  &(x, GaugeXiW*MW2, GaugeXiZ*MZ2) + 6.D0*GaugeXiW*MW2*x*B0(x, GaugeXiW*MW2, GaugeXiZ*MZ2) + 6.D0*GaugeXiZ*MZ2*x*B0(x, GaugeXiW*M&
  &W2, GaugeXiZ*MZ2) - 3.D0*B0(x, GaugeXiW*MW2, GaugeXiZ*MZ2)*DBLE(GaugeXiW**INT(2.D0))*DBLE(MW**INT(4.D0)) - 3.D0*B0(x, GaugeXiW&
  &*MW2, GaugeXiZ*MZ2)*DBLE(GaugeXiZ**INT(2.D0))*DBLE(MZ**INT(4.D0)) - 2.D0*DBLE(x**INT(2.D0)) - 3.D0*B0(x, GaugeXiW*MW2, GaugeXi&
  &Z*MZ2)*DBLE(x**INT(2.D0))))/(PI2*SW2*x)

 amplitudes(32) = (-0.0008680555555555555D0*EL2*(-6.D0*x*(-11.D0*GaugeXiW*MW2 + 9.D0*x + GaugeXiA*x)*A0(GaugeXiW*MW2) - 15.D0*x*D&
  &BLE(MW**INT(4.D0)) + 192.D0*x*B0(x, 0.D0, MW2)*DBLE(MW**INT(4.D0)) + 12.D0*GaugeXiA*x*B0(x, 0.D0, MW2)*DBLE(MW**INT(4.D0)) - 9&
  &.D0*x*DBLE(GaugeXiW**INT(2.D0))*DBLE(MW**INT(4.D0)) - 12.D0*x*B0(x, 0.D0, GaugeXiW*MW2)*DBLE(GaugeXiW**INT(2.D0))*DBLE(MW**INT&
  &(4.D0)) + 4.D0*DBLE(MW**INT(6.D0)) - 4.D0*GaugeXiA*DBLE(MW**INT(6.D0)) - 48.D0*B0(x, 0.D0, MW2)*DBLE(MW**INT(6.D0)) - 12.D0*Ga&
  &ugeXiA*B0(x, 0.D0, MW2)*DBLE(MW**INT(6.D0)) - 48.D0*x*C0Mine(DBLE(0.D0), DBLE(x), DBLE(x), DBLE(0.D0), DBLE(0.D0), DBLE(MW2))*&
  & DBLE(MW**INT(6.D0)) + 48.D0*GaugeXiA*x*C0Mine(DBLE(0.D0), DBLE(x), DBLE(x), DBLE(0.D0), DBLE(0.D0), DBLE(MW2))*DBLE(MW**INT(6&
  &.D0)) - 6.D0*C0Mine(DBLE(0.D0), DBLE(x), DBLE(x), DBLE(0.D0), DBLE(0.D0), DBLE(MW2))*DBLE(MW**INT(8.D0)) + 6.D0*GaugeXiA*C0Min&
  &e(DBLE(0.D0), DBLE(x), DBLE(x), DBLE(0.D0), DBLE(0.D0), DBLE(MW2))*DBLE(MW**INT(8.D0)) - 28.D0*MW2*DBLE(x**INT(2.D0)) + 12.D0*&
  &GaugeXiA*MW2*DBLE(x**INT(2.D0)) + 20.D0*GaugeXiW*MW2*DBLE(x**INT(2.D0)) + 4.D0*GaugeXiA*GaugeXiW*MW2*DBLE(x**INT(2.D0)) + 192.&
  &D0*MW2*B0(x, 0.D0, MW2)*DBLE(x**INT(2.D0)) + 12.D0*GaugeXiA*MW2*B0(x, 0.D0, MW2)*DBLE(x**INT(2.D0)) + 12.D0*GaugeXiW*MW2*B0(x,&
  & 0.D0, GaugeXiW*MW2)*DBLE(x**INT(2.D0)) + 12.D0*GaugeXiA*GaugeXiW*MW2*B0(x, 0.D0, GaugeXiW*MW2)*DBLE(x**INT(2.D0)) + 108.D0*C0&
  &Mine(DBLE(0.D0), DBLE(x), DBLE(x), DBLE(0.D0), DBLE(0.D0), DBLE(MW2))*DBLE(MW**INT(4.D0))*DBLE(x**INT(2.D0)) - 108.D0*GaugeXiA&
  &*C0Mine(DBLE(0.D0), DBLE(x), DBLE(x), DBLE(0.D0), DBLE(0.D0), DBLE(MW2))*DBLE(MW**INT(4.D0))*DBLE(x**INT(2.D0)) + 6.D0*C0Mine(&
  &DBLE(0.D0), DBLE(x), DBLE(x), DBLE(0.D0), DBLE(0.D0), DBLE(GaugeXiW*MW2))*DBLE(GaugeXiW**INT(2.D0))*DBLE(MW**INT(4.D0))* DBLE(&
  &x**INT(2.D0)) - 6.D0*GaugeXiA*C0Mine(DBLE(0.D0), DBLE(x), DBLE(x), DBLE(0.D0), DBLE(0.D0), DBLE(GaugeXiW*MW2))*DBLE(GaugeXiW**&
  &INT(2.D0))* DBLE(MW**INT(4.D0))*DBLE(x**INT(2.D0)) + 6.D0*A0(MW2)*(13.D0*MW2*x + 9.D0*DBLE(MW**INT(4.D0)) + GaugeXiA*DBLE((MW2&
  & - 1.D0*x)**INT(2.D0)) + 9.D0*DBLE(x**INT(2.D0))) - 48.D0*B0(x, 0.D0, MW2)*DBLE(x**INT(3.D0)) - 12.D0*GaugeXiA*B0(x, 0.D0, MW2&
  &)*DBLE(x**INT(3.D0)) + 48.D0*B0(x, 0.D0, GaugeXiW*MW2)*DBLE(x**INT(3.D0)) + 12.D0*GaugeXiA*B0(x, 0.D0, GaugeXiW*MW2)*DBLE(x**I&
  &NT(3.D0)) - 48.D0*MW2*C0Mine(DBLE(0.D0), DBLE(x), DBLE(x), DBLE(0.D0), DBLE(0.D0), DBLE(MW2))*DBLE(x**INT(3.D0)) + 48.D0*Gauge&
  &XiA*MW2*C0Mine(DBLE(0.D0), DBLE(x), DBLE(x), DBLE(0.D0), DBLE(0.D0), DBLE(MW2))*DBLE(x**INT(3.D0)) - 12.D0*GaugeXiW*MW2*C0Mine&
  &(DBLE(0.D0), DBLE(x), DBLE(x), DBLE(0.D0), DBLE(0.D0), DBLE(GaugeXiW*MW2))*DBLE(x**INT(3.D0)) + 12.D0*GaugeXiA*GaugeXiW*MW2*C0&
  &Mine(DBLE(0.D0), DBLE(x), DBLE(x), DBLE(0.D0), DBLE(0.D0), DBLE(GaugeXiW*MW2))*DBLE(x**INT(3.D0)) - 6.D0*C0Mine(DBLE(0.D0), DB&
  &LE(x), DBLE(x), DBLE(0.D0), DBLE(0.D0), DBLE(MW2))*DBLE(x**INT(4.D0)) + 6.D0*GaugeXiA*C0Mine(DBLE(0.D0), DBLE(x), DBLE(x), DBL&
  &E(0.D0), DBLE(0.D0), DBLE(MW2))*DBLE(x**INT(4.D0)) + 6.D0*C0Mine(DBLE(0.D0), DBLE(x), DBLE(x), DBLE(0.D0), DBLE(0.D0), DBLE(Ga&
  &ugeXiW*MW2))*DBLE(x**INT(4.D0)) - 6.D0*GaugeXiA*C0Mine(DBLE(0.D0), DBLE(x), DBLE(x), DBLE(0.D0), DBLE(0.D0), DBLE(GaugeXiW*MW2&
  &))*DBLE(x**INT(4.D0))))/(MW2*PI2*x)

 amplitudes(33) = (0.0008680555555555555D0*CW2*EL2*(-66.D0*GaugeXiW*MW2*MZ2*x*A0(GaugeXiW*MW2) - 66.D0*GaugeXiZ*MW2*MZ2*x*A0(Gaug&
  &eXiZ*MZ2) + 3.D0*MZ2*x*DBLE(MW**INT(4.D0)) + 12.D0*GaugeXiZ*MZ2*x*DBLE(MW**INT(4.D0)) + 6.D0*GaugeXiZ*MZ2*A0(GaugeXiZ*MZ2)*DBL&
  &E(MW**INT(4.D0)) - 54.D0*x*A0(GaugeXiZ*MZ2)*DBLE(MW**INT(4.D0)) - 192.D0*MZ2*x*B0(x, MW2, MZ2)*DBLE(MW**INT(4.D0)) - 12.D0*Gau&
  &geXiZ*MZ2*x*B0(x, MW2, GaugeXiZ*MZ2)*DBLE(MW**INT(4.D0)) + 9.D0*MZ2*x*DBLE(GaugeXiW**INT(2.D0))*DBLE(MW**INT(4.D0)) + 12.D0*MZ&
  &2*x*B0(x, GaugeXiW*MW2, MZ2)*DBLE(GaugeXiW**INT(2.D0))*DBLE(MW**INT(4.D0)) - 6.D0*A0(GaugeXiZ*MZ2)*DBLE(MW**INT(6.D0)) + 48.D0&
  &*MZ2*B0(x, MW2, MZ2)*DBLE(MW**INT(6.D0)) + 48.D0*x*B0(x, MW2, MZ2)*DBLE(MW**INT(6.D0)) + 12.D0*GaugeXiZ*MZ2*B0(x, MW2, GaugeXi&
  &Z*MZ2)*DBLE(MW**INT(6.D0)) - 48.D0*x*B0(x, MW2, GaugeXiZ*MZ2)*DBLE(MW**INT(6.D0)) + 6.D0*B0(x, MW2, MZ2)*DBLE(MW**INT(8.D0)) -&
  & 6.D0*B0(x, MW2, GaugeXiZ*MZ2)*DBLE(MW**INT(8.D0)) + 3.D0*MW2*x*DBLE(MZ**INT(4.D0)) + 12.D0*GaugeXiW*MW2*x*DBLE(MZ**INT(4.D0))&
  & + 6.D0*GaugeXiW*MW2*A0(GaugeXiW*MW2)*DBLE(MZ**INT(4.D0)) - 54.D0*x*A0(GaugeXiW*MW2)*DBLE(MZ**INT(4.D0)) - 192.D0*MW2*x*B0(x, &
  &MW2, MZ2)*DBLE(MZ**INT(4.D0)) - 12.D0*GaugeXiW*MW2*x*B0(x, GaugeXiW*MW2, MZ2)*DBLE(MZ**INT(4.D0)) + 9.D0*MW2*x*DBLE(GaugeXiZ**&
  &INT(2.D0))*DBLE(MZ**INT(4.D0)) + 12.D0*MW2*x*B0(x, MW2, GaugeXiZ*MZ2)*DBLE(GaugeXiZ**INT(2.D0))*DBLE(MZ**INT(4.D0)) - 108.D0*B&
  &0(x, MW2, MZ2)*DBLE(MW**INT(4.D0))* DBLE(MZ**INT(4.D0)) - 6.D0*B0(x, GaugeXiW*MW2, MZ2)*DBLE(GaugeXiW**INT(2.D0))*DBLE(MW**INT&
  &(4.D0))*DBLE(MZ**INT(4.D0)) - 6.D0*B0(x, MW2, GaugeXiZ*MZ2)*DBLE(GaugeXiZ**INT(2.D0))*DBLE(MW**INT(4.D0))*DBLE(MZ**INT(4.D0)) &
  &- 6.D0*A0(GaugeXiW*MW2)*DBLE(MZ**INT(6.D0)) + 48.D0*MW2*B0(x, MW2, MZ2)*DBLE(MZ**INT(6.D0)) + 48.D0*x*B0(x, MW2, MZ2)*DBLE(MZ*&
  &*INT(6.D0)) + 12.D0*GaugeXiW*MW2*B0(x, GaugeXiW*MW2, MZ2)*DBLE(MZ**INT(6.D0)) - 48.D0*x*B0(x, GaugeXiW*MW2, MZ2)*DBLE(MZ**INT(&
  &6.D0)) + 6.D0*B0(x, MW2, MZ2)*DBLE(MZ**INT(8.D0)) - 6.D0*B0(x, GaugeXiW*MW2, MZ2)*DBLE(MZ**INT(8.D0)) + 6.D0*MW2*A0(MZ2)*(-13.&
  &D0*MZ2*x + 9.D0*MW2*(MZ2 + x) + DBLE(MW**INT(4.D0)) - 9.D0*DBLE(MZ**INT(4.D0)) - 1.D0*GaugeXiW*DBLE((MZ2 - 1.D0*x)**INT(2.D0))&
  & - 9.D0*DBLE(x**INT(2.D0))) + 40.D0*MW2*MZ2*DBLE(x**INT(2.D0)) - 24.D0*GaugeXiW*MW2*MZ2*DBLE(x**INT(2.D0)) - 24.D0*GaugeXiZ*MW&
  &2*MZ2*DBLE(x**INT(2.D0)) + 54.D0*MZ2*A0(GaugeXiW*MW2)*DBLE(x**INT(2.D0)) + 6.D0*GaugeXiZ*MZ2*A0(GaugeXiW*MW2)*DBLE(x**INT(2.D0&
  &)) + 54.D0*MW2*A0(GaugeXiZ*MZ2)*DBLE(x**INT(2.D0)) + 6.D0*GaugeXiW*MW2*A0(GaugeXiZ*MZ2)*DBLE(x**INT(2.D0)) - 192.D0*MW2*MZ2*B0&
  &(x, MW2, MZ2)*DBLE(x**INT(2.D0)) - 12.D0*GaugeXiZ*MW2*MZ2*B0(x, MW2, GaugeXiZ*MZ2)*DBLE(x**INT(2.D0)) - 12.D0*GaugeXiW*MW2*MZ2&
  &*B0(x, GaugeXiW*MW2, MZ2)*DBLE(x**INT(2.D0)) - 12.D0*GaugeXiW*GaugeXiZ*MW2*MZ2*B0(x, GaugeXiW*MW2, GaugeXiZ*MZ2)*DBLE(x**INT(2&
  &.D0)) - 108.D0*B0(x, MW2, MZ2)*DBLE(MW**INT(4.D0))*DBLE(x**INT(2.D0)) + 108.D0*B0(x, MW2, GaugeXiZ*MZ2)*DBLE(MW**INT(4.D0))*DB&
  &LE(x**INT(2.D0)) - 6.D0*B0(x, GaugeXiW*MW2, MZ2)*DBLE(GaugeXiW**INT(2.D0))*DBLE(MW**INT(4.D0))*DBLE(x**INT(2.D0)) + 6.D0*B0(x,&
  & GaugeXiW*MW2, GaugeXiZ*MZ2)*DBLE(GaugeXiW**INT(2.D0))*DBLE(MW**INT(4.D0))*DBLE(x**INT(2.D0)) - 108.D0*B0(x, MW2, MZ2)*DBLE(MZ&
  &**INT(4.D0))*DBLE(x**INT(2.D0)) + 108.D0*B0(x, GaugeXiW*MW2, MZ2)*DBLE(MZ**INT(4.D0))*DBLE(x**INT(2.D0)) - 6.D0*B0(x, MW2, Gau&
  &geXiZ*MZ2)*DBLE(GaugeXiZ**INT(2.D0))*DBLE(MZ**INT(4.D0))*DBLE(x**INT(2.D0)) + 6.D0*B0(x, GaugeXiW*MW2, GaugeXiZ*MZ2)*DBLE(Gaug&
  &eXiZ**INT(2.D0))*DBLE(MZ**INT(4.D0))*DBLE(x**INT(2.D0)) - 6.D0*MZ2*A0(MW2)*(-9.D0*MZ2*x + MW2*(-9.D0*MZ2 + 13.D0*x) + 9.D0*DBL&
  &E(MW**INT(4.D0)) - 1.D0*DBLE(MZ**INT(4.D0)) + GaugeXiZ*DBLE((MW2 - 1.D0*x)**INT(2.D0)) + 9.D0*DBLE(x**INT(2.D0))) + 48.D0*MW2*&
  &B0(x, MW2, MZ2)*DBLE(x**INT(3.D0)) + 48.D0*MZ2*B0(x, MW2, MZ2)*DBLE(x**INT(3.D0)) - 48.D0*MW2*B0(x, MW2, GaugeXiZ*MZ2)*DBLE(x*&
  &*INT(3.D0)) + 12.D0*GaugeXiZ*MZ2*B0(x, MW2, GaugeXiZ*MZ2)*DBLE(x**INT(3.D0)) + 12.D0*GaugeXiW*MW2*B0(x, GaugeXiW*MW2, MZ2)*DBL&
  &E(x**INT(3.D0)) - 48.D0*MZ2*B0(x, GaugeXiW*MW2, MZ2)*DBLE(x**INT(3.D0)) - 12.D0*GaugeXiW*MW2*B0(x, GaugeXiW*MW2, GaugeXiZ*MZ2)&
  &*DBLE(x**INT(3.D0)) - 12.D0*GaugeXiZ*MZ2*B0(x, GaugeXiW*MW2, GaugeXiZ*MZ2)*DBLE(x**INT(3.D0)) + 6.D0*B0(x, MW2, MZ2)*DBLE(x**I&
  &NT(4.D0)) - 6.D0*B0(x, MW2, GaugeXiZ*MZ2)*DBLE(x**INT(4.D0)) - 6.D0*B0(x, GaugeXiW*MW2, MZ2)*DBLE(x**INT(4.D0)) + 6.D0*B0(x, G&
  &augeXiW*MW2, GaugeXiZ*MZ2)*DBLE(x**INT(4.D0))))/(MW2*MZ2*PI2*SW2*x)

 amplitudes(34) = (0.001736111111111111D0*EL2*MW2*(-3.D0*(-1.D0 + GaugeXiA)*A0(GaugeXiW*MW2) + 6.D0*(-1.D0*GaugeXiW*MW2 + 5.D0*x &
  &+ GaugeXiA*(GaugeXiW*MW2 + x))* B0(x, 0.D0, GaugeXiW*MW2) - 1.D0*(-1.D0 + GaugeXiA)*(-2.D0*(GaugeXiW*MW2 + 2.D0*x) + 3.D0*C0Mi&
  &ne(DBLE(0.D0), DBLE(x), DBLE(x), DBLE(0.D0), DBLE(0.D0), DBLE(GaugeXiW*MW2))*DBLE((-1.D0*GaugeXiW*MW2 + x)**INT(2.D0)))))/(PI2&
  &*x)

 amplitudes(35) = (0.005208333333333333D0*EL2*MW2*SW2*(-2.D0*MZ2*x + 2.D0*GaugeXiZ*MZ2*x - 1.D0*(-1.D0 + GaugeXiZ)*MZ2*A0(GaugeXi&
  &W*MW2) - 1.D0*(-1.D0*GaugeXiW*MW2 + MZ2 + x)*A0(MZ2) - 1.D0*GaugeXiW*MW2*A0(GaugeXiZ*MZ2) + GaugeXiZ*MZ2*A0(GaugeXiZ*MZ2) + x*&
  &A0(GaugeXiZ*MZ2) - 2.D0*GaugeXiW*MW2*MZ2*B0(x, GaugeXiW*MW2, MZ2) - 2.D0*GaugeXiW*MW2*x*B0(x, GaugeXiW*MW2, MZ2) + 10.D0*MZ2*x&
  &*B0(x, GaugeXiW*MW2, MZ2) + 2.D0*GaugeXiW*GaugeXiZ*MW2*MZ2*B0(x, GaugeXiW*MW2, GaugeXiZ*MZ2) + 2.D0*GaugeXiW*MW2*x*B0(x, Gauge&
  &XiW*MW2, GaugeXiZ*MZ2) + 2.D0*GaugeXiZ*MZ2*x*B0(x, GaugeXiW*MW2, GaugeXiZ*MZ2) + B0(x, GaugeXiW*MW2, MZ2)*DBLE(GaugeXiW**INT(2&
  &.D0))*DBLE(MW**INT(4.D0)) - 1.D0*B0(x, GaugeXiW*MW2, GaugeXiZ*MZ2)*DBLE(GaugeXiW**INT(2.D0))*DBLE(MW**INT(4.D0)) + B0(x, Gauge&
  &XiW*MW2, MZ2)*DBLE(MZ**INT(4.D0)) - 1.D0*B0(x, GaugeXiW*MW2, GaugeXiZ*MZ2)*DBLE(GaugeXiZ**INT(2.D0))*DBLE(MZ**INT(4.D0)) + B0(&
  &x, GaugeXiW*MW2, MZ2)*DBLE(x**INT(2.D0)) - 1.D0*B0(x, GaugeXiW*MW2, GaugeXiZ*MZ2)*DBLE(x**INT(2.D0))))/(CW2*MZ2*PI2*x)

 amplitudes(36) = (0.005208333333333333D0*EL2*SBA2*(-2.D0*MW2*x + 2.D0*GaugeXiW*MW2*x - 1.D0*(-1.D0 + GaugeXiW)*MW2*A0(Mh02) + (M&
  &h02 - 1.D0*MW2 - 1.D0*x)*A0(MW2) - 1.D0*Mh02*A0(GaugeXiW*MW2) + GaugeXiW*MW2*A0(GaugeXiW*MW2) + x*A0(GaugeXiW*MW2) - 2.D0*Mh02&
  &*MW2*B0(x, Mh02, MW2) - 2.D0*Mh02*x*B0(x, Mh02, MW2) + 10.D0*MW2*x*B0(x, Mh02, MW2) + 2.D0*GaugeXiW*Mh02*MW2*B0(x, Mh02, Gauge&
  &XiW*MW2) + 2.D0*Mh02*x*B0(x, Mh02, GaugeXiW*MW2) + 2.D0*GaugeXiW*MW2*x*B0(x, Mh02, GaugeXiW*MW2) + B0(x, Mh02, MW2)*DBLE(Mh0**&
  &INT(4.D0)) - 1.D0*B0(x, Mh02, GaugeXiW*MW2)*DBLE(Mh0**INT(4.D0)) + B0(x, Mh02, MW2)*DBLE(MW**INT(4.D0)) - 1.D0*B0(x, Mh02, Gau&
  &geXiW*MW2)*DBLE(GaugeXiW**INT(2.D0))*DBLE(MW**INT(4.D0)) + B0(x, Mh02, MW2)*DBLE(x**INT(2.D0)) - 1.D0*B0(x, Mh02, GaugeXiW*MW2&
  &)*DBLE(x**INT(2.D0))))/(PI2*SW2*x)

 amplitudes(37) = (0.005208333333333333D0*CBA2*EL2*(-2.D0*MW2*x + 2.D0*GaugeXiW*MW2*x - 1.D0*(-1.D0 + GaugeXiW)*MW2*A0(MHH2) + (M&
  &HH2 - 1.D0*MW2 - 1.D0*x)*A0(MW2) - 1.D0*MHH2*A0(GaugeXiW*MW2) + GaugeXiW*MW2*A0(GaugeXiW*MW2) + x*A0(GaugeXiW*MW2) - 2.D0*MHH2&
  &*MW2*B0(x, MHH2, MW2) - 2.D0*MHH2*x*B0(x, MHH2, MW2) + 10.D0*MW2*x*B0(x, MHH2, MW2) + 2.D0*GaugeXiW*MHH2*MW2*B0(x, MHH2, Gauge&
  &XiW*MW2) + 2.D0*MHH2*x*B0(x, MHH2, GaugeXiW*MW2) + 2.D0*GaugeXiW*MW2*x*B0(x, MHH2, GaugeXiW*MW2) + B0(x, MHH2, MW2)*DBLE(MHH**&
  &INT(4.D0)) - 1.D0*B0(x, MHH2, GaugeXiW*MW2)*DBLE(MHH**INT(4.D0)) + B0(x, MHH2, MW2)*DBLE(MW**INT(4.D0)) - 1.D0*B0(x, MHH2, Gau&
  &geXiW*MW2)*DBLE(GaugeXiW**INT(2.D0))*DBLE(MW**INT(4.D0)) + B0(x, MHH2, MW2)*DBLE(x**INT(2.D0)) - 1.D0*B0(x, MHH2, GaugeXiW*MW2&
  &)*DBLE(x**INT(2.D0))))/(PI2*SW2*x)

  totalAmplitude = (0D0,0D0)
 do j=1,37
  totalAmplitude = totalAmplitude + amplitudes(j)
 end do
 SelfWpWpUsual = totalAmplitude
end function SelfWpWpUsual

