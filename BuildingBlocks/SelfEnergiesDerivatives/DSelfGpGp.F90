double complex function DSelfGpGp(x)
 use constants
 implicit none
#include "looptools.h"
 double precision, intent(in) :: x
 integer :: j
 double complex :: totalAmplitude
 double complex :: amplitudes(35)

 amplitudes(1) = 0.D0

 amplitudes(2) = 0.D0

 amplitudes(3) = 0.D0

 amplitudes(4) = 0.D0

 amplitudes(5) = 0.D0

 amplitudes(6) = 0.D0

 amplitudes(7) = 0.D0

 amplitudes(8) = 0.D0

 amplitudes(9) = 0.D0

 amplitudes(10) = (-0.03125D0*EL2*ME2*(-1.D0*B0(x, 0.D0, ME2) + (ME2 - 1.D0*x)*DB0(x, 0.D0, ME2)))/(MW2*PI2*SW2)

 amplitudes(11) = (-0.03125D0*EL2*MM2*(-1.D0*B0(x, 0.D0, MM2) + (MM2 - 1.D0*x)*DB0(x, 0.D0, MM2)))/(MW2*PI2*SW2)

 amplitudes(12) = (-0.03125D0*EL2*ML2*(-1.D0*B0(x, 0.D0, ML2) + (ML2 - 1.D0*x)*DB0(x, 0.D0, ML2)))/(MW2*PI2*SW2)

 amplitudes(13) = (-0.09375D0*CKM11*CKMC11*EL2*((-1.D0*MD2 - 1.D0*MU2)*B0(x, MD2, MU2) + DB0(x, MD2, MU2)*(-1.D0*MU2*x - 1.D0*MD2&
  &*(2.D0*MU2 + x) + DBLE(MD**INT(4.D0)) + DBLE(MU**INT(4.D0)))))/(MW2*PI2*SW2)

 amplitudes(14) = (-0.09375D0*CKM21*CKMC21*EL2*((-1.D0*MC2 - 1.D0*MD2)*B0(x, MC2, MD2) + DB0(x, MC2, MD2)*(-1.D0*MD2*x - 1.D0*MC2&
  &*(2.D0*MD2 + x) + DBLE(MC**INT(4.D0)) + DBLE(MD**INT(4.D0)))))/(MW2*PI2*SW2)

 amplitudes(15) = (-0.09375D0*CKM31*CKMC31*EL2*((-1.D0*MD2 - 1.D0*MT2)*B0(x, MD2, MT2) + DB0(x, MD2, MT2)*(-1.D0*MT2*x - 1.D0*MD2&
  &*(2.D0*MT2 + x) + DBLE(MD**INT(4.D0)) + DBLE(MT**INT(4.D0)))))/(MW2*PI2*SW2)

 amplitudes(16) = (-0.09375D0*CKM12*CKMC12*EL2*((-1.D0*MS2 - 1.D0*MU2)*B0(x, MS2, MU2) + DB0(x, MS2, MU2)*(-1.D0*MU2*x - 1.D0*MS2&
  &*(2.D0*MU2 + x) + DBLE(MS**INT(4.D0)) + DBLE(MU**INT(4.D0)))))/(MW2*PI2*SW2)

 amplitudes(17) = (-0.09375D0*CKM22*CKMC22*EL2*((-1.D0*MC2 - 1.D0*MS2)*B0(x, MC2, MS2) + DB0(x, MC2, MS2)*(-1.D0*MS2*x - 1.D0*MC2&
  &*(2.D0*MS2 + x) + DBLE(MC**INT(4.D0)) + DBLE(MS**INT(4.D0)))))/(MW2*PI2*SW2)

 amplitudes(18) = (-0.09375D0*CKM32*CKMC32*EL2*((-1.D0*MS2 - 1.D0*MT2)*B0(x, MS2, MT2) + DB0(x, MS2, MT2)*(-1.D0*MT2*x - 1.D0*MS2&
  &*(2.D0*MT2 + x) + DBLE(MS**INT(4.D0)) + DBLE(MT**INT(4.D0)))))/(MW2*PI2*SW2)

 amplitudes(19) = (-0.09375D0*CKM13*CKMC13*EL2*((-1.D0*MB2 - 1.D0*MU2)*B0(x, MB2, MU2) + DB0(x, MB2, MU2)*(-1.D0*MU2*x - 1.D0*MB2&
  &*(2.D0*MU2 + x) + DBLE(MB**INT(4.D0)) + DBLE(MU**INT(4.D0)))))/(MW2*PI2*SW2)

 amplitudes(20) = (-0.09375D0*CKM23*CKMC23*EL2*((-1.D0*MB2 - 1.D0*MC2)*B0(x, MB2, MC2) + DB0(x, MB2, MC2)*(-1.D0*MC2*x - 1.D0*MB2&
  &*(2.D0*MC2 + x) + DBLE(MB**INT(4.D0)) + DBLE(MC**INT(4.D0)))))/(MW2*PI2*SW2)

 amplitudes(21) = (-0.09375D0*CKM33*CKMC33*EL2*((-1.D0*MB2 - 1.D0*MT2)*B0(x, MB2, MT2) + DB0(x, MB2, MT2)*(-1.D0*MT2*x - 1.D0*MB2&
  &*(2.D0*MT2 + x) + DBLE(MB**INT(4.D0)) + DBLE(MT**INT(4.D0)))))/(MW2*PI2*SW2)

 amplitudes(22) = (0.015625D0*CBA2*EL2*DB0(x, Mh02, MHp2)*DBLE((Mh02 - 1.D0*MHp2)**INT(2.D0)))/(MW2*PI2*SW2)

 amplitudes(23) = (0.015625D0*EL2*SBA2*DB0(x, MHH2, MHp2)*DBLE((MHH2 - 1.D0*MHp2)**INT(2.D0)))/(MW2*PI2*SW2)

 amplitudes(24) = (0.015625D0*EL2*DB0(x, MA02, MHp2)*DBLE((MA02 - 1.D0*MHp2)**INT(2.D0)))/(MW2*PI2*SW2)

 amplitudes(25) = (0.015625D0*EL2*SBA2*DB0(x, Mh02, GaugeXiW*MW2)*DBLE(Mh0**INT(4.D0)))/(MW2*PI2*SW2)

 amplitudes(26) = (0.015625D0*CBA2*EL2*DB0(x, MHH2, GaugeXiW*MW2)*DBLE(MHH**INT(4.D0)))/(MW2*PI2*SW2)

 amplitudes(27) = (0.015625D0*EL2*GaugeXiW*GaugeXiZ*MW2*(CW2 - 1.D0*SW2)*DB0(x, GaugeXiW*MW2, GaugeXiZ*MZ2))/(CW2*PI2*SW2)

 amplitudes(28) = (0.015625D0*EL2*GaugeXiW*GaugeXiZ*MW2*(CW2 - 1.D0*SW2)*DB0(x, GaugeXiW*MW2, GaugeXiZ*MZ2))/(CW2*PI2*SW2)

 amplitudes(29) = (0.015625D0*EL2*(-2.D0*B0(x, 0.D0, MW2) + 2.D0*GaugeXiA*B0(x, 0.D0, MW2) + 2.D0*B0(x, 0.D0, GaugeXiW*MW2) - 2.D&
  &0*GaugeXiA*B0(x, 0.D0, GaugeXiW*MW2) - 2.D0*MW2*C0Mine(DBLE(0.D0), DBLE(x), DBLE(x), DBLE(0.D0), DBLE(0.D0), DBLE(MW2)) + 2.D0&
  &*GaugeXiA*MW2*C0Mine(DBLE(0.D0), DBLE(x), DBLE(x), DBLE(0.D0), DBLE(0.D0), DBLE(MW2)) + 2.D0*x*C0Mine(DBLE(0.D0), DBLE(x), DBL&
  &E(x), DBLE(0.D0), DBLE(0.D0), DBLE(MW2)) - 2.D0*GaugeXiA*x*C0Mine(DBLE(0.D0), DBLE(x), DBLE(x), DBLE(0.D0), DBLE(0.D0), DBLE(M&
  &W2)) + 2.D0*GaugeXiW*MW2*C0Mine(DBLE(0.D0), DBLE(x), DBLE(x), DBLE(0.D0), DBLE(0.D0), DBLE(GaugeXiW*MW2)) - 2.D0*GaugeXiA*Gaug&
  &eXiW*MW2*C0Mine(DBLE(0.D0), DBLE(x), DBLE(x), DBLE(0.D0), DBLE(0.D0), DBLE(GaugeXiW*MW2)) - 2.D0*x*C0Mine(DBLE(0.D0), DBLE(x),&
  & DBLE(x), DBLE(0.D0), DBLE(0.D0), DBLE(GaugeXiW*MW2)) + 2.D0*GaugeXiA*x*C0Mine(DBLE(0.D0), DBLE(x), DBLE(x), DBLE(0.D0), DBLE(&
  &0.D0), DBLE(GaugeXiW*MW2)) + 10.D0*MW2*DB0(x, 0.D0, MW2) + 2.D0*GaugeXiA*MW2*DB0(x, 0.D0, MW2) - 2.D0*x*DB0(x, 0.D0, MW2) + 2.&
  &D0*GaugeXiA*x*DB0(x, 0.D0, MW2) + 2.D0*GaugeXiW*MW2*DB0(x, 0.D0, GaugeXiW*MW2) + 2.D0*GaugeXiA*GaugeXiW*MW2*DB0(x, 0.D0, Gauge&
  &XiW*MW2) + 2.D0*x*DB0(x, 0.D0, GaugeXiW*MW2) - 2.D0*GaugeXiA*x*DB0(x, 0.D0, GaugeXiW*MW2) - 2.D0*MW2*x*(DC01Mine(DBLE(0.D0), D&
  &BLE(x), DBLE(x), DBLE(0.D0), DBLE(0.D0), DBLE(MW2)) + DC02Mine(DBLE(0.D0), DBLE(x), DBLE(x), DBLE(0.D0), DBLE(0.D0), DBLE(MW2)&
  &)) + 2.D0*GaugeXiA*MW2*x*(DC01Mine(DBLE(0.D0), DBLE(x), DBLE(x), DBLE(0.D0), DBLE(0.D0), DBLE(MW2)) + DC02Mine(DBLE(0.D0), DBL&
  &E(x), DBLE(x), DBLE(0.D0), DBLE(0.D0), DBLE(MW2))) + DBLE(MW**INT(4.D0))*(DC01Mine(DBLE(0.D0), DBLE(x), DBLE(x), DBLE(0.D0), D&
  &BLE(0.D0), DBLE(MW2)) + DC02Mine(DBLE(0.D0), DBLE(x), DBLE(x), DBLE(0.D0), DBLE(0.D0), DBLE(MW2))) - 1.D0*GaugeXiA*DBLE(MW**IN&
  &T(4.D0))* (DC01Mine(DBLE(0.D0), DBLE(x), DBLE(x), DBLE(0.D0), DBLE(0.D0), DBLE(MW2)) + DC02Mine(DBLE(0.D0), DBLE(x), DBLE(x), &
  &DBLE(0.D0), DBLE(0.D0), DBLE(MW2))) + DBLE(x**INT(2.D0))*(DC01Mine(DBLE(0.D0), DBLE(x), DBLE(x), DBLE(0.D0), DBLE(0.D0), DBLE(&
  &MW2)) + DC02Mine(DBLE(0.D0), DBLE(x), DBLE(x), DBLE(0.D0), DBLE(0.D0), DBLE(MW2))) - 1.D0*GaugeXiA*DBLE(x**INT(2.D0))* (DC01Mi&
  &ne(DBLE(0.D0), DBLE(x), DBLE(x), DBLE(0.D0), DBLE(0.D0), DBLE(MW2)) + DC02Mine(DBLE(0.D0), DBLE(x), DBLE(x), DBLE(0.D0), DBLE(&
  &0.D0), DBLE(MW2))) + 2.D0*GaugeXiW*MW2*x*(DC01Mine(DBLE(0.D0), DBLE(x), DBLE(x), DBLE(0.D0), DBLE(0.D0), DBLE(GaugeXiW*MW2)) +&
  & DC02Mine(DBLE(0.D0), DBLE(x), DBLE(x), DBLE(0.D0), DBLE(0.D0), DBLE(GaugeXiW*MW2))) - 2.D0*GaugeXiA*GaugeXiW*MW2*x*(DC01Mine(&
  &DBLE(0.D0), DBLE(x), DBLE(x), DBLE(0.D0), DBLE(0.D0), DBLE(GaugeXiW*MW2)) + DC02Mine(DBLE(0.D0), DBLE(x), DBLE(x), DBLE(0.D0),&
  & DBLE(0.D0), DBLE(GaugeXiW*MW2))) - 1.D0*DBLE(GaugeXiW**INT(2.D0))*DBLE(MW**INT(4.D0))* (DC01Mine(DBLE(0.D0), DBLE(x), DBLE(x)&
  &, DBLE(0.D0), DBLE(0.D0), DBLE(GaugeXiW*MW2)) + DC02Mine(DBLE(0.D0), DBLE(x), DBLE(x), DBLE(0.D0), DBLE(0.D0), DBLE(GaugeXiW*M&
  &W2))) + GaugeXiA*DBLE(GaugeXiW**INT(2.D0))*DBLE(MW**INT(4.D0))* (DC01Mine(DBLE(0.D0), DBLE(x), DBLE(x), DBLE(0.D0), DBLE(0.D0)&
  &, DBLE(GaugeXiW*MW2)) + DC02Mine(DBLE(0.D0), DBLE(x), DBLE(x), DBLE(0.D0), DBLE(0.D0), DBLE(GaugeXiW*MW2))) - 1.D0*DBLE(x**INT&
  &(2.D0))*(DC01Mine(DBLE(0.D0), DBLE(x), DBLE(x), DBLE(0.D0), DBLE(0.D0), DBLE(GaugeXiW*MW2)) + DC02Mine(DBLE(0.D0), DBLE(x), DB&
  &LE(x), DBLE(0.D0), DBLE(0.D0), DBLE(GaugeXiW*MW2))) + GaugeXiA*DBLE(x**INT(2.D0))*(DC01Mine(DBLE(0.D0), DBLE(x), DBLE(x), DBLE&
  &(0.D0), DBLE(0.D0), DBLE(GaugeXiW*MW2)) + DC02Mine(DBLE(0.D0), DBLE(x), DBLE(x), DBLE(0.D0), DBLE(0.D0), DBLE(GaugeXiW*MW2))))&
  &)/PI2

 amplitudes(30) = (0.015625D0*EL2*SW2*(-2.D0*MW2*B0(x, MW2, MZ2) - 2.D0*MZ2*B0(x, MW2, MZ2) + 2.D0*x*B0(x, MW2, MZ2) + 2.D0*MW2*B&
  &0(x, MW2, GaugeXiZ*MZ2) + 2.D0*GaugeXiZ*MZ2*B0(x, MW2, GaugeXiZ*MZ2) - 2.D0*x*B0(x, MW2, GaugeXiZ*MZ2) + 2.D0*GaugeXiW*MW2*B0(&
  &x, GaugeXiW*MW2, MZ2) + 2.D0*MZ2*B0(x, GaugeXiW*MW2, MZ2) - 2.D0*x*B0(x, GaugeXiW*MW2, MZ2) - 2.D0*GaugeXiW*MW2*B0(x, GaugeXiW&
  &*MW2, GaugeXiZ*MZ2) - 2.D0*GaugeXiZ*MZ2*B0(x, GaugeXiW*MW2, GaugeXiZ*MZ2) + 2.D0*x*B0(x, GaugeXiW*MW2, GaugeXiZ*MZ2) + 10.D0*M&
  &W2*MZ2*DB0(x, MW2, MZ2) - 2.D0*MW2*x*DB0(x, MW2, MZ2) - 2.D0*MZ2*x*DB0(x, MW2, MZ2) + 2.D0*GaugeXiZ*MW2*MZ2*DB0(x, MW2, GaugeX&
  &iZ*MZ2) + 2.D0*MW2*x*DB0(x, MW2, GaugeXiZ*MZ2) + 2.D0*GaugeXiZ*MZ2*x*DB0(x, MW2, GaugeXiZ*MZ2) + 2.D0*GaugeXiW*MW2*MZ2*DB0(x, &
  &GaugeXiW*MW2, MZ2) + 2.D0*GaugeXiW*MW2*x*DB0(x, GaugeXiW*MW2, MZ2) + 2.D0*MZ2*x*DB0(x, GaugeXiW*MW2, MZ2) + 2.D0*GaugeXiW*Gaug&
  &eXiZ*MW2*MZ2*DB0(x, GaugeXiW*MW2, GaugeXiZ*MZ2) - 2.D0*GaugeXiW*MW2*x*DB0(x, GaugeXiW*MW2, GaugeXiZ*MZ2) - 2.D0*GaugeXiZ*MZ2*x&
  &*DB0(x, GaugeXiW*MW2, GaugeXiZ*MZ2) + DB0(x, MW2, MZ2)*DBLE(MW**INT(4.D0)) - 1.D0*DB0(x, MW2, GaugeXiZ*MZ2)*DBLE(MW**INT(4.D0)&
  &) - 1.D0*DB0(x, GaugeXiW*MW2, MZ2)*DBLE(GaugeXiW**INT(2.D0))*DBLE(MW**INT(4.D0)) + DB0(x, GaugeXiW*MW2, GaugeXiZ*MZ2)*DBLE(Gau&
  &geXiW**INT(2.D0))*DBLE(MW**INT(4.D0)) + DB0(x, MW2, MZ2)*DBLE(MZ**INT(4.D0)) - 1.D0*DB0(x, GaugeXiW*MW2, MZ2)*DBLE(MZ**INT(4.D&
  &0)) - 1.D0*DB0(x, MW2, GaugeXiZ*MZ2)*DBLE(GaugeXiZ**INT(2.D0))*DBLE(MZ**INT(4.D0)) + DB0(x, GaugeXiW*MW2, GaugeXiZ*MZ2)*DBLE(G&
  &augeXiZ**INT(2.D0))*DBLE(MZ**INT(4.D0)) + DB0(x, MW2, MZ2)*DBLE(x**INT(2.D0)) - 1.D0*DB0(x, MW2, GaugeXiZ*MZ2)*DBLE(x**INT(2.D&
  &0)) - 1.D0*DB0(x, GaugeXiW*MW2, MZ2)*DBLE(x**INT(2.D0)) + DB0(x, GaugeXiW*MW2, GaugeXiZ*MZ2)*DBLE(x**INT(2.D0))))/(CW2*MZ2*PI2&
  &)

 amplitudes(31) = (0.0625D0*EL2*(-2.D0*B0(x, 0.D0, GaugeXiW*MW2) - 2.D0*(-1.D0 + GaugeXiA)*(-1.D0*GaugeXiW*MW2 + x)*C0Mine(DBLE(0&
  &.D0), DBLE(x), DBLE(x), DBLE(0.D0), DBLE(0.D0), DBLE(GaugeXiW*MW2)) - 2.D0*(GaugeXiW*MW2 + x)*DB0(x, 0.D0, GaugeXiW*MW2) - 1.D&
  &0*(-1.D0 + GaugeXiA)*DBLE((-1.D0*GaugeXiW*MW2 + x)**INT(2.D0))*(DC01Mine(DBLE(0.D0), DBLE(x), DBLE(x), DBLE(0.D0), DBLE(0.D0),&
  & DBLE(GaugeXiW*MW2)) + DC02Mine(DBLE(0.D0), DBLE(x), DBLE(x), DBLE(0.D0), DBLE(0.D0), DBLE(GaugeXiW*MW2)))))/PI2

 amplitudes(32) = (0.015625D0*EL2*DBLE((CW2 - 1.D0*SW2)**INT(2.D0))*(-1.D0*A0(MZ2) + A0(GaugeXiZ*MZ2) - 2.D0*GaugeXiW*MW2*B0(x, G&
  &augeXiW*MW2, MZ2) - 2.D0*MZ2*B0(x, GaugeXiW*MW2, MZ2) + 2.D0*x*B0(x, GaugeXiW*MW2, MZ2) + 2.D0*GaugeXiW*MW2*B0(x, GaugeXiW*MW2&
  &, GaugeXiZ*MZ2) - 2.D0*x*B0(x, GaugeXiW*MW2, GaugeXiZ*MZ2) - 2.D0*GaugeXiW*MW2*MZ2*DB0(x, GaugeXiW*MW2, MZ2) - 2.D0*GaugeXiW*M&
  &W2*x*DB0(x, GaugeXiW*MW2, MZ2) - 2.D0*MZ2*x*DB0(x, GaugeXiW*MW2, MZ2) + 2.D0*GaugeXiW*MW2*x*DB0(x, GaugeXiW*MW2, GaugeXiZ*MZ2)&
  & + DB0(x, GaugeXiW*MW2, MZ2)*DBLE(GaugeXiW**INT(2.D0))*DBLE(MW**INT(4.D0)) - 1.D0*DB0(x, GaugeXiW*MW2, GaugeXiZ*MZ2)*DBLE(Gaug&
  &eXiW**INT(2.D0))*DBLE(MW**INT(4.D0)) + DB0(x, GaugeXiW*MW2, MZ2)*DBLE(MZ**INT(4.D0)) + DB0(x, GaugeXiW*MW2, MZ2)*DBLE(x**INT(2&
  &.D0)) - 1.D0*DB0(x, GaugeXiW*MW2, GaugeXiZ*MZ2)*DBLE(x**INT(2.D0))))/(CW2*MZ2*PI2*SW2)

 amplitudes(33) = (0.015625D0*EL2*SBA2*(-1.D0*A0(MW2) + A0(GaugeXiW*MW2) - 2.D0*Mh02*B0(x, Mh02, MW2) - 2.D0*MW2*B0(x, Mh02, MW2)&
  & + 2.D0*x*B0(x, Mh02, MW2) + 2.D0*Mh02*B0(x, Mh02, GaugeXiW*MW2) - 2.D0*x*B0(x, Mh02, GaugeXiW*MW2) - 2.D0*Mh02*MW2*DB0(x, Mh0&
  &2, MW2) - 2.D0*Mh02*x*DB0(x, Mh02, MW2) - 2.D0*MW2*x*DB0(x, Mh02, MW2) + 2.D0*Mh02*x*DB0(x, Mh02, GaugeXiW*MW2) + DB0(x, Mh02,&
  & MW2)*DBLE(Mh0**INT(4.D0)) - 1.D0*DB0(x, Mh02, GaugeXiW*MW2)*DBLE(Mh0**INT(4.D0)) + DB0(x, Mh02, MW2)*DBLE(MW**INT(4.D0)) + DB&
  &0(x, Mh02, MW2)*DBLE(x**INT(2.D0)) - 1.D0*DB0(x, Mh02, GaugeXiW*MW2)*DBLE(x**INT(2.D0))))/(MW2*PI2*SW2)

 amplitudes(34) = (0.015625D0*CBA2*EL2*(-1.D0*A0(MW2) + A0(GaugeXiW*MW2) - 2.D0*MHH2*B0(x, MHH2, MW2) - 2.D0*MW2*B0(x, MHH2, MW2)&
  & + 2.D0*x*B0(x, MHH2, MW2) + 2.D0*MHH2*B0(x, MHH2, GaugeXiW*MW2) - 2.D0*x*B0(x, MHH2, GaugeXiW*MW2) - 2.D0*MHH2*MW2*DB0(x, MHH&
  &2, MW2) - 2.D0*MHH2*x*DB0(x, MHH2, MW2) - 2.D0*MW2*x*DB0(x, MHH2, MW2) + 2.D0*MHH2*x*DB0(x, MHH2, GaugeXiW*MW2) + DB0(x, MHH2,&
  & MW2)*DBLE(MHH**INT(4.D0)) - 1.D0*DB0(x, MHH2, GaugeXiW*MW2)*DBLE(MHH**INT(4.D0)) + DB0(x, MHH2, MW2)*DBLE(MW**INT(4.D0)) + DB&
  &0(x, MHH2, MW2)*DBLE(x**INT(2.D0)) - 1.D0*DB0(x, MHH2, GaugeXiW*MW2)*DBLE(x**INT(2.D0))))/(MW2*PI2*SW2)

 amplitudes(35) = (0.015625D0*EL2*(-1.D0*A0(MW2) + A0(GaugeXiW*MW2) - 2.D0*MW2*B0(x, MW2, GaugeXiZ*MZ2) - 2.D0*GaugeXiZ*MZ2*B0(x,&
  & MW2, GaugeXiZ*MZ2) + 2.D0*x*B0(x, MW2, GaugeXiZ*MZ2) + 2.D0*GaugeXiZ*MZ2*B0(x, GaugeXiW*MW2, GaugeXiZ*MZ2) - 2.D0*x*B0(x, Gau&
  &geXiW*MW2, GaugeXiZ*MZ2) - 2.D0*GaugeXiZ*MW2*MZ2*DB0(x, MW2, GaugeXiZ*MZ2) - 2.D0*MW2*x*DB0(x, MW2, GaugeXiZ*MZ2) - 2.D0*Gauge&
  &XiZ*MZ2*x*DB0(x, MW2, GaugeXiZ*MZ2) + 2.D0*GaugeXiZ*MZ2*x*DB0(x, GaugeXiW*MW2, GaugeXiZ*MZ2) + DB0(x, MW2, GaugeXiZ*MZ2)*DBLE(&
  &MW**INT(4.D0)) + DB0(x, MW2, GaugeXiZ*MZ2)*DBLE(GaugeXiZ**INT(2.D0))*DBLE(MZ**INT(4.D0)) - 1.D0*DB0(x, GaugeXiW*MW2, GaugeXiZ*&
  &MZ2)*DBLE(GaugeXiZ**INT(2.D0))* DBLE(MZ**INT(4.D0)) + DB0(x, MW2, GaugeXiZ*MZ2)*DBLE(x**INT(2.D0)) - 1.D0*DB0(x, GaugeXiW*MW2,&
  & GaugeXiZ*MZ2)*DBLE(x**INT(2.D0))))/ (MW2*PI2*SW2)

  totalAmplitude = (0D0,0D0)
 do j=1,35
  totalAmplitude = totalAmplitude + amplitudes(j)
 end do
 DSelfGpGp = totalAmplitude
end function DSelfGpGp

