double complex function DSelfCCLeft(x)
 use constants
 implicit none
#include "looptools.h"
 double precision, intent(in) :: x
 integer :: j
 double complex :: totalAmplitude
 double complex :: amplitudes(15)

 amplitudes(1) = (0.0078125D0*CA2*EL2*MC2*(B0(x, MC2, Mh02) + MC2*DB0(x, MC2, Mh02) - 1.D0*Mh02*DB0(x, MC2, Mh02) + x*DB0(x, MC2,&
  & Mh02)))/ (MW2*PI2*SB2*SW2*x) - (0.0078125D0*CA2*EL2*MC2*(-1.D0*A0(MC2) + A0(Mh02) + MC2*B0(x, MC2, Mh02) - 1.D0*Mh02*B0(x, MC&
  &2, Mh02) + x*B0(x, MC2, Mh02))*DBLE(x**INT(-2.D0)))/(MW2*PI2*SB2*SW2)

 amplitudes(2) = (0.0078125D0*EL2*MC2*SA2*(B0(x, MC2, MHH2) + MC2*DB0(x, MC2, MHH2) - 1.D0*MHH2*DB0(x, MC2, MHH2) + x*DB0(x, MC2,&
  & MHH2)))/ (MW2*PI2*SB2*SW2*x) - (0.0078125D0*EL2*MC2*SA2*(-1.D0*A0(MC2) + A0(MHH2) + MC2*B0(x, MC2, MHH2) - 1.D0*MHH2*B0(x, MC&
  &2, MHH2) + x*B0(x, MC2, MHH2))*DBLE(x**INT(-2.D0)))/(MW2*PI2*SB2*SW2)

 amplitudes(3) = (0.0078125D0*EL2*MC2*(B0(x, MA02, MC2) - 1.D0*MA02*DB0(x, MA02, MC2) + MC2*DB0(x, MA02, MC2) + x*DB0(x, MA02, MC&
  &2)))/(MW2*PI2*SW2*TB2*x) - (0.0078125D0*EL2*MC2*(A0(MA02) - 1.D0*A0(MC2) - 1.D0*MA02*B0(x, MA02, MC2) + MC2*B0(x, MA02, MC2) +&
  & x*B0(x, MA02, MC2))*DBLE(x**INT(-2.D0)))/ (MW2*PI2*SW2*TB2)

 amplitudes(4) = (0.0078125D0*EL2*MC2*(B0(x, MC2, GaugeXiZ*MZ2) + MC2*DB0(x, MC2, GaugeXiZ*MZ2) - 1.D0*GaugeXiZ*MZ2*DB0(x, MC2, G&
  &augeXiZ*MZ2) + x*DB0(x, MC2, GaugeXiZ*MZ2)))/(MW2*PI2*SW2*x) - (0.0078125D0*EL2*MC2*(-1.D0*A0(MC2) + A0(GaugeXiZ*MZ2) + MC2*B0&
  &(x, MC2, GaugeXiZ*MZ2) - 1.D0*GaugeXiZ*MZ2*B0(x, MC2, GaugeXiZ*MZ2) + x*B0(x, MC2, GaugeXiZ*MZ2))*DBLE(x**INT(-2.D0)))/(MW2*PI&
  &2*SW2)

 amplitudes(5) = (-0.015625D0*CKM21*CKMC21*EL2*DBLE(x**INT(-2.D0))*(-1.D0*MD2*TB2*A0(MD2)*DBLE(Yuk3**INT(2.D0)) + MD2*TB2*A0(MHp2&
  &)*DBLE(Yuk3**INT(2.D0)) - 1.D0*MD2*MHp2*TB2*B0(x, MD2, MHp2)*DBLE(Yuk3**INT(2.D0)) + MD2*TB2*x*B0(x, MD2, MHp2)*DBLE(Yuk3**INT&
  &(2.D0)) + TB2*B0(x, MD2, MHp2)*DBLE(MD**INT(4.D0))*DBLE(Yuk3**INT(2.D0))))/(MW2*PI2*SW2*TB2) + (0.015625D0*CKM21*CKMC21*EL2*(M&
  &D2*TB2*B0(x, MD2, MHp2)*DBLE(Yuk3**INT(2.D0)) - 1.D0*MD2*MHp2*TB2*DB0(x, MD2, MHp2)*DBLE(Yuk3**INT(2.D0)) + MD2*TB2*x*DB0(x, M&
  &D2, MHp2)*DBLE(Yuk3**INT(2.D0)) + TB2*DB0(x, MD2, MHp2)*DBLE(MD**INT(4.D0))*DBLE(Yuk3**INT(2.D0))))/(MW2*PI2*SW2*TB2*x)

 amplitudes(6) = (-0.015625D0*CKM22*CKMC22*EL2*DBLE(x**INT(-2.D0))*(MS2*TB2*A0(MHp2)*DBLE(Yuk3**INT(2.D0)) - 1.D0*MS2*TB2*A0(MS2)&
  &*DBLE(Yuk3**INT(2.D0)) - 1.D0*MHp2*MS2*TB2*B0(x, MHp2, MS2)*DBLE(Yuk3**INT(2.D0)) + MS2*TB2*x*B0(x, MHp2, MS2)*DBLE(Yuk3**INT(&
  &2.D0)) + TB2*B0(x, MHp2, MS2)*DBLE(MS**INT(4.D0))*DBLE(Yuk3**INT(2.D0))))/(MW2*PI2*SW2*TB2) + (0.015625D0*CKM22*CKMC22*EL2*(MS&
  &2*TB2*B0(x, MHp2, MS2)*DBLE(Yuk3**INT(2.D0)) - 1.D0*MHp2*MS2*TB2*DB0(x, MHp2, MS2)*DBLE(Yuk3**INT(2.D0)) + MS2*TB2*x*DB0(x, MH&
  &p2, MS2)*DBLE(Yuk3**INT(2.D0)) + TB2*DB0(x, MHp2, MS2)*DBLE(MS**INT(4.D0))*DBLE(Yuk3**INT(2.D0))))/(MW2*PI2*SW2*TB2*x)

 amplitudes(7) = (-0.015625D0*CKM23*CKMC23*EL2*DBLE(x**INT(-2.D0))*(-1.D0*MB2*TB2*A0(MB2)*DBLE(Yuk3**INT(2.D0)) + MB2*TB2*A0(MHp2&
  &)*DBLE(Yuk3**INT(2.D0)) - 1.D0*MB2*MHp2*TB2*B0(x, MB2, MHp2)*DBLE(Yuk3**INT(2.D0)) + MB2*TB2*x*B0(x, MB2, MHp2)*DBLE(Yuk3**INT&
  &(2.D0)) + TB2*B0(x, MB2, MHp2)*DBLE(MB**INT(4.D0))*DBLE(Yuk3**INT(2.D0))))/(MW2*PI2*SW2*TB2) + (0.015625D0*CKM23*CKMC23*EL2*(M&
  &B2*TB2*B0(x, MB2, MHp2)*DBLE(Yuk3**INT(2.D0)) - 1.D0*MB2*MHp2*TB2*DB0(x, MB2, MHp2)*DBLE(Yuk3**INT(2.D0)) + MB2*TB2*x*DB0(x, M&
  &B2, MHp2)*DBLE(Yuk3**INT(2.D0)) + TB2*DB0(x, MB2, MHp2)*DBLE(MB**INT(4.D0))*DBLE(Yuk3**INT(2.D0))))/(MW2*PI2*SW2*TB2*x)

 amplitudes(8) = (0.015625D0*CKM21*CKMC21*EL2*(MD2*B0(x, MD2, GaugeXiW*MW2) - 1.D0*GaugeXiW*MD2*MW2*DB0(x, MD2, GaugeXiW*MW2) + M&
  &D2*x*DB0(x, MD2, GaugeXiW*MW2) + DB0(x, MD2, GaugeXiW*MW2)*DBLE(MD**INT(4.D0))))/(MW2*PI2*SW2*x) - (0.015625D0*CKM21*CKMC21*EL&
  &2*(-1.D0*MD2*A0(MD2) + MD2*A0(GaugeXiW*MW2) - 1.D0*GaugeXiW*MD2*MW2*B0(x, MD2, GaugeXiW*MW2) + MD2*x*B0(x, MD2, GaugeXiW*MW2) &
  &+ B0(x, MD2, GaugeXiW*MW2)*DBLE(MD**INT(4.D0)))*DBLE(x**INT(-2.D0)))/(MW2*PI2*SW2)

 amplitudes(9) = (0.015625D0*CKM22*CKMC22*EL2*(MS2*B0(x, MS2, GaugeXiW*MW2) - 1.D0*GaugeXiW*MS2*MW2*DB0(x, MS2, GaugeXiW*MW2) + M&
  &S2*x*DB0(x, MS2, GaugeXiW*MW2) + DB0(x, MS2, GaugeXiW*MW2)*DBLE(MS**INT(4.D0))))/(MW2*PI2*SW2*x) - (0.015625D0*CKM22*CKMC22*EL&
  &2*(-1.D0*MS2*A0(MS2) + MS2*A0(GaugeXiW*MW2) - 1.D0*GaugeXiW*MS2*MW2*B0(x, MS2, GaugeXiW*MW2) + MS2*x*B0(x, MS2, GaugeXiW*MW2) &
  &+ B0(x, MS2, GaugeXiW*MW2)*DBLE(MS**INT(4.D0)))*DBLE(x**INT(-2.D0)))/(MW2*PI2*SW2)

 amplitudes(10) = (0.015625D0*CKM23*CKMC23*EL2*(MB2*B0(x, MB2, GaugeXiW*MW2) - 1.D0*GaugeXiW*MB2*MW2*DB0(x, MB2, GaugeXiW*MW2) + &
  &MB2*x*DB0(x, MB2, GaugeXiW*MW2) + DB0(x, MB2, GaugeXiW*MW2)*DBLE(MB**INT(4.D0))))/(MW2*PI2*SW2*x) - (0.015625D0*CKM23*CKMC23*E&
  &L2*(-1.D0*MB2*A0(MB2) + MB2*A0(GaugeXiW*MW2) - 1.D0*GaugeXiW*MB2*MW2*B0(x, MB2, GaugeXiW*MW2) + MB2*x*B0(x, MB2, GaugeXiW*MW2)&
  & + B0(x, MB2, GaugeXiW*MW2)*DBLE(MB**INT(4.D0)))*DBLE(x**INT(-2.D0)))/(MW2*PI2*SW2)

 amplitudes(11) = (-0.013888888888888888D0*EL2*DBLE(x**INT(-2.D0))*(-2.D0*x - 2.D0*A0(MC2) + MC2*B0(x, 0.D0, MC2) + GaugeXiA*MC2*&
  &B0(x, 0.D0, MC2) + x*B0(x, 0.D0, MC2) + GaugeXiA*x*B0(x, 0.D0, MC2) - 2.D0*MC2*x*C0Mine(DBLE(0.D0), DBLE(x), DBLE(x), DBLE(0.D&
  &0), DBLE(0.D0), DBLE(MC2)) + 2.D0*GaugeXiA*MC2*x*C0Mine(DBLE(0.D0), DBLE(x), DBLE(x), DBLE(0.D0), DBLE(0.D0), DBLE(MC2)) + C0M&
  &ine(DBLE(0.D0), DBLE(x), DBLE(x), DBLE(0.D0), DBLE(0.D0), DBLE(MC2))*DBLE(MC**INT(4.D0)) - 1.D0*GaugeXiA*C0Mine(DBLE(0.D0), DB&
  &LE(x), DBLE(x), DBLE(0.D0), DBLE(0.D0), DBLE(MC2))*DBLE(MC**INT(4.D0)) + C0Mine(DBLE(0.D0), DBLE(x), DBLE(x), DBLE(0.D0), DBLE&
  &(0.D0), DBLE(MC2))*DBLE(x**INT(2.D0)) - 1.D0*GaugeXiA*C0Mine(DBLE(0.D0), DBLE(x), DBLE(x), DBLE(0.D0), DBLE(0.D0), DBLE(MC2))*&
  &DBLE(x**INT(2.D0))))/PI2 + (0.013888888888888888D0*EL2*(-2.D0 + B0(x, 0.D0, MC2) + GaugeXiA*B0(x, 0.D0, MC2) - 2.D0*MC2*C0Mine&
  &(DBLE(0.D0), DBLE(x), DBLE(x), DBLE(0.D0), DBLE(0.D0), DBLE(MC2)) + 2.D0*GaugeXiA*MC2*C0Mine(DBLE(0.D0), DBLE(x), DBLE(x), DBL&
  &E(0.D0), DBLE(0.D0), DBLE(MC2)) + 2.D0*x*C0Mine(DBLE(0.D0), DBLE(x), DBLE(x), DBLE(0.D0), DBLE(0.D0), DBLE(MC2)) - 2.D0*GaugeX&
  &iA*x*C0Mine(DBLE(0.D0), DBLE(x), DBLE(x), DBLE(0.D0), DBLE(0.D0), DBLE(MC2)) + MC2*DB0(x, 0.D0, MC2) + GaugeXiA*MC2*DB0(x, 0.D&
  &0, MC2) + x*DB0(x, 0.D0, MC2) + GaugeXiA*x*DB0(x, 0.D0, MC2) - 2.D0*MC2*x*(DC01Mine(DBLE(0.D0), DBLE(x), DBLE(x), DBLE(0.D0), &
  &DBLE(0.D0), DBLE(MC2)) + DC02Mine(DBLE(0.D0), DBLE(x), DBLE(x), DBLE(0.D0), DBLE(0.D0), DBLE(MC2))) + 2.D0*GaugeXiA*MC2*x*(DC0&
  &1Mine(DBLE(0.D0), DBLE(x), DBLE(x), DBLE(0.D0), DBLE(0.D0), DBLE(MC2)) + DC02Mine(DBLE(0.D0), DBLE(x), DBLE(x), DBLE(0.D0), DB&
  &LE(0.D0), DBLE(MC2))) + DBLE(MC**INT(4.D0))*(DC01Mine(DBLE(0.D0), DBLE(x), DBLE(x), DBLE(0.D0), DBLE(0.D0), DBLE(MC2)) + DC02M&
  &ine(DBLE(0.D0), DBLE(x), DBLE(x), DBLE(0.D0), DBLE(0.D0), DBLE(MC2))) - 1.D0*GaugeXiA*DBLE(MC**INT(4.D0))* (DC01Mine(DBLE(0.D0&
  &), DBLE(x), DBLE(x), DBLE(0.D0), DBLE(0.D0), DBLE(MC2)) + DC02Mine(DBLE(0.D0), DBLE(x), DBLE(x), DBLE(0.D0), DBLE(0.D0), DBLE(&
  &MC2))) + DBLE(x**INT(2.D0))*(DC01Mine(DBLE(0.D0), DBLE(x), DBLE(x), DBLE(0.D0), DBLE(0.D0), DBLE(MC2)) + DC02Mine(DBLE(0.D0), &
  &DBLE(x), DBLE(x), DBLE(0.D0), DBLE(0.D0), DBLE(MC2))) - 1.D0*GaugeXiA*DBLE(x**INT(2.D0))* (DC01Mine(DBLE(0.D0), DBLE(x), DBLE(&
  &x), DBLE(0.D0), DBLE(0.D0), DBLE(MC2)) + DC02Mine(DBLE(0.D0), DBLE(x), DBLE(x), DBLE(0.D0), DBLE(0.D0), DBLE(MC2)))))/(PI2*x)

 amplitudes(12) = (-0.0008680555555555555D0*EL2*DBLE(x**INT(-2.D0))*(-18.D0*MZ2*x + 48.D0*MZ2*SW2*x - 18.D0*MZ2*A0(MC2) + 48.D0*M&
  &Z2*SW2*A0(MC2) + 9.D0*MC2*A0(MZ2) + 18.D0*MZ2*A0(MZ2) - 24.D0*MC2*SW2*A0(MZ2) - 48.D0*MZ2*SW2*A0(MZ2) - 9.D0*x*A0(MZ2) + 24.D0&
  &*SW2*x*A0(MZ2) - 9.D0*MC2*A0(GaugeXiZ*MZ2) + 24.D0*MC2*SW2*A0(GaugeXiZ*MZ2) + 9.D0*x*A0(GaugeXiZ*MZ2) - 24.D0*SW2*x*A0(GaugeXi&
  &Z*MZ2) + 9.D0*MC2*MZ2*B0(x, MC2, MZ2) - 24.D0*MC2*MZ2*SW2*B0(x, MC2, MZ2) - 18.D0*MC2*x*B0(x, MC2, MZ2) + 9.D0*MZ2*x*B0(x, MC2&
  &, MZ2) + 48.D0*MC2*SW2*x*B0(x, MC2, MZ2) - 24.D0*MZ2*SW2*x*B0(x, MC2, MZ2) + 9.D0*GaugeXiZ*MC2*MZ2*B0(x, MC2, GaugeXiZ*MZ2) - &
  &24.D0*GaugeXiZ*MC2*MZ2*SW2*B0(x, MC2, GaugeXiZ*MZ2) + 18.D0*MC2*x*B0(x, MC2, GaugeXiZ*MZ2) + 9.D0*GaugeXiZ*MZ2*x*B0(x, MC2, Ga&
  &ugeXiZ*MZ2) - 48.D0*MC2*SW2*x*B0(x, MC2, GaugeXiZ*MZ2) - 24.D0*GaugeXiZ*MZ2*SW2*x*B0(x, MC2, GaugeXiZ*MZ2) + 9.D0*B0(x, MC2, M&
  &Z2)*DBLE(MC**INT(4.D0)) - 24.D0*SW2*B0(x, MC2, MZ2)*DBLE(MC**INT(4.D0)) - 9.D0*B0(x, MC2, GaugeXiZ*MZ2)*DBLE(MC**INT(4.D0)) + &
  &24.D0*SW2*B0(x, MC2, GaugeXiZ*MZ2)*DBLE(MC**INT(4.D0)) - 18.D0*B0(x, MC2, MZ2)*DBLE(MZ**INT(4.D0)) + 48.D0*SW2*B0(x, MC2, MZ2)&
  &*DBLE(MZ**INT(4.D0)) - 32.D0*MZ2*x*DBLE(SW**INT(4.D0)) - 32.D0*MZ2*A0(MC2)*DBLE(SW**INT(4.D0)) + 16.D0*MC2*A0(MZ2)*DBLE(SW**IN&
  &T(4.D0)) + 32.D0*MZ2*A0(MZ2)*DBLE(SW**INT(4.D0)) - 16.D0*x*A0(MZ2)*DBLE(SW**INT(4.D0)) - 16.D0*MC2*A0(GaugeXiZ*MZ2)*DBLE(SW**I&
  &NT(4.D0)) + 16.D0*x*A0(GaugeXiZ*MZ2)*DBLE(SW**INT(4.D0)) + 16.D0*MC2*MZ2*B0(x, MC2, MZ2)*DBLE(SW**INT(4.D0)) - 32.D0*MC2*x*B0(&
  &x, MC2, MZ2)*DBLE(SW**INT(4.D0)) + 16.D0*MZ2*x*B0(x, MC2, MZ2)*DBLE(SW**INT(4.D0)) + 16.D0*GaugeXiZ*MC2*MZ2*B0(x, MC2, GaugeXi&
  &Z*MZ2)*DBLE(SW**INT(4.D0)) + 32.D0*MC2*x*B0(x, MC2, GaugeXiZ*MZ2)*DBLE(SW**INT(4.D0)) + 16.D0*GaugeXiZ*MZ2*x*B0(x, MC2, GaugeX&
  &iZ*MZ2)*DBLE(SW**INT(4.D0)) + 16.D0*B0(x, MC2, MZ2)*DBLE(MC**INT(4.D0))*DBLE(SW**INT(4.D0)) - 16.D0*B0(x, MC2, GaugeXiZ*MZ2)*D&
  &BLE(MC**INT(4.D0))*DBLE(SW**INT(4.D0)) - 32.D0*B0(x, MC2, MZ2)*DBLE(MZ**INT(4.D0))*DBLE(SW**INT(4.D0)) + 9.D0*B0(x, MC2, MZ2)*&
  &DBLE(x**INT(2.D0)) - 24.D0*SW2*B0(x, MC2, MZ2)*DBLE(x**INT(2.D0)) - 9.D0*B0(x, MC2, GaugeXiZ*MZ2)*DBLE(x**INT(2.D0)) + 24.D0*S&
  &W2*B0(x, MC2, GaugeXiZ*MZ2)*DBLE(x**INT(2.D0)) + 16.D0*B0(x, MC2, MZ2)*DBLE(SW**INT(4.D0))*DBLE(x**INT(2.D0)) - 16.D0*B0(x, MC&
  &2, GaugeXiZ*MZ2)*DBLE(SW**INT(4.D0))*DBLE(x**INT(2.D0))))/(CW2*MZ2*PI2*SW2) + (0.0008680555555555555D0*EL2*(-18.D0*MZ2 + 48.D0&
  &*MZ2*SW2 - 9.D0*A0(MZ2) + 24.D0*SW2*A0(MZ2) + 9.D0*A0(GaugeXiZ*MZ2) - 24.D0*SW2*A0(GaugeXiZ*MZ2) - 18.D0*MC2*B0(x, MC2, MZ2) +&
  & 9.D0*MZ2*B0(x, MC2, MZ2) + 48.D0*MC2*SW2*B0(x, MC2, MZ2) - 24.D0*MZ2*SW2*B0(x, MC2, MZ2) + 18.D0*x*B0(x, MC2, MZ2) - 48.D0*SW&
  &2*x*B0(x, MC2, MZ2) + 18.D0*MC2*B0(x, MC2, GaugeXiZ*MZ2) + 9.D0*GaugeXiZ*MZ2*B0(x, MC2, GaugeXiZ*MZ2) - 48.D0*MC2*SW2*B0(x, MC&
  &2, GaugeXiZ*MZ2) - 24.D0*GaugeXiZ*MZ2*SW2*B0(x, MC2, GaugeXiZ*MZ2) - 18.D0*x*B0(x, MC2, GaugeXiZ*MZ2) + 48.D0*SW2*x*B0(x, MC2,&
  & GaugeXiZ*MZ2) + 9.D0*MC2*MZ2*DB0(x, MC2, MZ2) - 24.D0*MC2*MZ2*SW2*DB0(x, MC2, MZ2) - 18.D0*MC2*x*DB0(x, MC2, MZ2) + 9.D0*MZ2*&
  &x*DB0(x, MC2, MZ2) + 48.D0*MC2*SW2*x*DB0(x, MC2, MZ2) - 24.D0*MZ2*SW2*x*DB0(x, MC2, MZ2) + 9.D0*GaugeXiZ*MC2*MZ2*DB0(x, MC2, G&
  &augeXiZ*MZ2) - 24.D0*GaugeXiZ*MC2*MZ2*SW2*DB0(x, MC2, GaugeXiZ*MZ2) + 18.D0*MC2*x*DB0(x, MC2, GaugeXiZ*MZ2) + 9.D0*GaugeXiZ*MZ&
  &2*x*DB0(x, MC2, GaugeXiZ*MZ2) - 48.D0*MC2*SW2*x*DB0(x, MC2, GaugeXiZ*MZ2) - 24.D0*GaugeXiZ*MZ2*SW2*x*DB0(x, MC2, GaugeXiZ*MZ2)&
  & + 9.D0*DB0(x, MC2, MZ2)*DBLE(MC**INT(4.D0)) - 24.D0*SW2*DB0(x, MC2, MZ2)*DBLE(MC**INT(4.D0)) - 9.D0*DB0(x, MC2, GaugeXiZ*MZ2)&
  &*DBLE(MC**INT(4.D0)) + 24.D0*SW2*DB0(x, MC2, GaugeXiZ*MZ2)*DBLE(MC**INT(4.D0)) - 18.D0*DB0(x, MC2, MZ2)*DBLE(MZ**INT(4.D0)) + &
  &48.D0*SW2*DB0(x, MC2, MZ2)*DBLE(MZ**INT(4.D0)) - 32.D0*MZ2*DBLE(SW**INT(4.D0)) - 16.D0*A0(MZ2)*DBLE(SW**INT(4.D0)) + 16.D0*A0(&
  &GaugeXiZ*MZ2)*DBLE(SW**INT(4.D0)) - 32.D0*MC2*B0(x, MC2, MZ2)*DBLE(SW**INT(4.D0)) + 16.D0*MZ2*B0(x, MC2, MZ2)*DBLE(SW**INT(4.D&
  &0)) + 32.D0*x*B0(x, MC2, MZ2)*DBLE(SW**INT(4.D0)) + 32.D0*MC2*B0(x, MC2, GaugeXiZ*MZ2)*DBLE(SW**INT(4.D0)) + 16.D0*GaugeXiZ*MZ&
  &2*B0(x, MC2, GaugeXiZ*MZ2)*DBLE(SW**INT(4.D0)) - 32.D0*x*B0(x, MC2, GaugeXiZ*MZ2)*DBLE(SW**INT(4.D0)) + 16.D0*MC2*MZ2*DB0(x, M&
  &C2, MZ2)*DBLE(SW**INT(4.D0)) - 32.D0*MC2*x*DB0(x, MC2, MZ2)*DBLE(SW**INT(4.D0)) + 16.D0*MZ2*x*DB0(x, MC2, MZ2)*DBLE(SW**INT(4.&
  &D0)) + 16.D0*GaugeXiZ*MC2*MZ2*DB0(x, MC2, GaugeXiZ*MZ2)*DBLE(SW**INT(4.D0)) + 32.D0*MC2*x*DB0(x, MC2, GaugeXiZ*MZ2)*DBLE(SW**I&
  &NT(4.D0)) + 16.D0*GaugeXiZ*MZ2*x*DB0(x, MC2, GaugeXiZ*MZ2)*DBLE(SW**INT(4.D0)) + 16.D0*DB0(x, MC2, MZ2)*DBLE(MC**INT(4.D0))*DB&
  &LE(SW**INT(4.D0)) - 16.D0*DB0(x, MC2, GaugeXiZ*MZ2)*DBLE(MC**INT(4.D0))*DBLE(SW**INT(4.D0)) - 32.D0*DB0(x, MC2, MZ2)*DBLE(MZ**&
  &INT(4.D0))*DBLE(SW**INT(4.D0)) + 9.D0*DB0(x, MC2, MZ2)*DBLE(x**INT(2.D0)) - 24.D0*SW2*DB0(x, MC2, MZ2)*DBLE(x**INT(2.D0)) - 9.&
  &D0*DB0(x, MC2, GaugeXiZ*MZ2)*DBLE(x**INT(2.D0)) + 24.D0*SW2*DB0(x, MC2, GaugeXiZ*MZ2)*DBLE(x**INT(2.D0)) + 16.D0*DB0(x, MC2, M&
  &Z2)*DBLE(SW**INT(4.D0))*DBLE(x**INT(2.D0)) - 16.D0*DB0(x, MC2, GaugeXiZ*MZ2)*DBLE(SW**INT(4.D0))*DBLE(x**INT(2.D0))))/(CW2*MZ2&
  &*PI2*SW2*x)

 amplitudes(13) = (-0.015625D0*CKM21*CKMC21*EL2*DBLE(x**INT(-2.D0))*(-2.D0*MW2*x - 2.D0*MW2*A0(MD2) + (MD2 + 2.D0*MW2 - 1.D0*x)*A&
  &0(MW2) - 1.D0*MD2*A0(GaugeXiW*MW2) + x*A0(GaugeXiW*MW2) + MD2*MW2*B0(x, MD2, MW2) - 2.D0*MD2*x*B0(x, MD2, MW2) + MW2*x*B0(x, M&
  &D2, MW2) + GaugeXiW*MD2*MW2*B0(x, MD2, GaugeXiW*MW2) + 2.D0*MD2*x*B0(x, MD2, GaugeXiW*MW2) + GaugeXiW*MW2*x*B0(x, MD2, GaugeXi&
  &W*MW2) + B0(x, MD2, MW2)*DBLE(MD**INT(4.D0)) - 1.D0*B0(x, MD2, GaugeXiW*MW2)*DBLE(MD**INT(4.D0)) - 2.D0*B0(x, MD2, MW2)*DBLE(M&
  &W**INT(4.D0)) + B0(x, MD2, MW2)*DBLE(x**INT(2.D0)) - 1.D0*B0(x, MD2, GaugeXiW*MW2)*DBLE(x**INT(2.D0))))/(MW2*PI2*SW2) + (0.015&
  &625D0*CKM21*CKMC21*EL2*(-2.D0*MW2 - 1.D0*A0(MW2) + A0(GaugeXiW*MW2) - 2.D0*MD2*B0(x, MD2, MW2) + MW2*B0(x, MD2, MW2) + 2.D0*x*&
  &B0(x, MD2, MW2) + 2.D0*MD2*B0(x, MD2, GaugeXiW*MW2) + GaugeXiW*MW2*B0(x, MD2, GaugeXiW*MW2) - 2.D0*x*B0(x, MD2, GaugeXiW*MW2) &
  &+ MD2*MW2*DB0(x, MD2, MW2) - 2.D0*MD2*x*DB0(x, MD2, MW2) + MW2*x*DB0(x, MD2, MW2) + GaugeXiW*MD2*MW2*DB0(x, MD2, GaugeXiW*MW2)&
  & + 2.D0*MD2*x*DB0(x, MD2, GaugeXiW*MW2) + GaugeXiW*MW2*x*DB0(x, MD2, GaugeXiW*MW2) + DB0(x, MD2, MW2)*DBLE(MD**INT(4.D0)) - 1.&
  &D0*DB0(x, MD2, GaugeXiW*MW2)*DBLE(MD**INT(4.D0)) - 2.D0*DB0(x, MD2, MW2)*DBLE(MW**INT(4.D0)) + DB0(x, MD2, MW2)*DBLE(x**INT(2.&
  &D0)) - 1.D0*DB0(x, MD2, GaugeXiW*MW2)*DBLE(x**INT(2.D0))))/(MW2*PI2*SW2*x)

 amplitudes(14) = (-0.015625D0*CKM22*CKMC22*EL2*DBLE(x**INT(-2.D0))*(-2.D0*MW2*x - 2.D0*MW2*A0(MS2) + (MS2 + 2.D0*MW2 - 1.D0*x)*A&
  &0(MW2) - 1.D0*MS2*A0(GaugeXiW*MW2) + x*A0(GaugeXiW*MW2) + MS2*MW2*B0(x, MS2, MW2) - 2.D0*MS2*x*B0(x, MS2, MW2) + MW2*x*B0(x, M&
  &S2, MW2) + GaugeXiW*MS2*MW2*B0(x, MS2, GaugeXiW*MW2) + 2.D0*MS2*x*B0(x, MS2, GaugeXiW*MW2) + GaugeXiW*MW2*x*B0(x, MS2, GaugeXi&
  &W*MW2) + B0(x, MS2, MW2)*DBLE(MS**INT(4.D0)) - 1.D0*B0(x, MS2, GaugeXiW*MW2)*DBLE(MS**INT(4.D0)) - 2.D0*B0(x, MS2, MW2)*DBLE(M&
  &W**INT(4.D0)) + B0(x, MS2, MW2)*DBLE(x**INT(2.D0)) - 1.D0*B0(x, MS2, GaugeXiW*MW2)*DBLE(x**INT(2.D0))))/(MW2*PI2*SW2) + (0.015&
  &625D0*CKM22*CKMC22*EL2*(-2.D0*MW2 - 1.D0*A0(MW2) + A0(GaugeXiW*MW2) - 2.D0*MS2*B0(x, MS2, MW2) + MW2*B0(x, MS2, MW2) + 2.D0*x*&
  &B0(x, MS2, MW2) + 2.D0*MS2*B0(x, MS2, GaugeXiW*MW2) + GaugeXiW*MW2*B0(x, MS2, GaugeXiW*MW2) - 2.D0*x*B0(x, MS2, GaugeXiW*MW2) &
  &+ MS2*MW2*DB0(x, MS2, MW2) - 2.D0*MS2*x*DB0(x, MS2, MW2) + MW2*x*DB0(x, MS2, MW2) + GaugeXiW*MS2*MW2*DB0(x, MS2, GaugeXiW*MW2)&
  & + 2.D0*MS2*x*DB0(x, MS2, GaugeXiW*MW2) + GaugeXiW*MW2*x*DB0(x, MS2, GaugeXiW*MW2) + DB0(x, MS2, MW2)*DBLE(MS**INT(4.D0)) - 1.&
  &D0*DB0(x, MS2, GaugeXiW*MW2)*DBLE(MS**INT(4.D0)) - 2.D0*DB0(x, MS2, MW2)*DBLE(MW**INT(4.D0)) + DB0(x, MS2, MW2)*DBLE(x**INT(2.&
  &D0)) - 1.D0*DB0(x, MS2, GaugeXiW*MW2)*DBLE(x**INT(2.D0))))/(MW2*PI2*SW2*x)

 amplitudes(15) = (-0.015625D0*CKM23*CKMC23*EL2*DBLE(x**INT(-2.D0))*(-2.D0*MW2*x - 2.D0*MW2*A0(MB2) + (MB2 + 2.D0*MW2 - 1.D0*x)*A&
  &0(MW2) - 1.D0*MB2*A0(GaugeXiW*MW2) + x*A0(GaugeXiW*MW2) + MB2*MW2*B0(x, MB2, MW2) - 2.D0*MB2*x*B0(x, MB2, MW2) + MW2*x*B0(x, M&
  &B2, MW2) + GaugeXiW*MB2*MW2*B0(x, MB2, GaugeXiW*MW2) + 2.D0*MB2*x*B0(x, MB2, GaugeXiW*MW2) + GaugeXiW*MW2*x*B0(x, MB2, GaugeXi&
  &W*MW2) + B0(x, MB2, MW2)*DBLE(MB**INT(4.D0)) - 1.D0*B0(x, MB2, GaugeXiW*MW2)*DBLE(MB**INT(4.D0)) - 2.D0*B0(x, MB2, MW2)*DBLE(M&
  &W**INT(4.D0)) + B0(x, MB2, MW2)*DBLE(x**INT(2.D0)) - 1.D0*B0(x, MB2, GaugeXiW*MW2)*DBLE(x**INT(2.D0))))/(MW2*PI2*SW2) + (0.015&
  &625D0*CKM23*CKMC23*EL2*(-2.D0*MW2 - 1.D0*A0(MW2) + A0(GaugeXiW*MW2) - 2.D0*MB2*B0(x, MB2, MW2) + MW2*B0(x, MB2, MW2) + 2.D0*x*&
  &B0(x, MB2, MW2) + 2.D0*MB2*B0(x, MB2, GaugeXiW*MW2) + GaugeXiW*MW2*B0(x, MB2, GaugeXiW*MW2) - 2.D0*x*B0(x, MB2, GaugeXiW*MW2) &
  &+ MB2*MW2*DB0(x, MB2, MW2) - 2.D0*MB2*x*DB0(x, MB2, MW2) + MW2*x*DB0(x, MB2, MW2) + GaugeXiW*MB2*MW2*DB0(x, MB2, GaugeXiW*MW2)&
  & + 2.D0*MB2*x*DB0(x, MB2, GaugeXiW*MW2) + GaugeXiW*MW2*x*DB0(x, MB2, GaugeXiW*MW2) + DB0(x, MB2, MW2)*DBLE(MB**INT(4.D0)) - 1.&
  &D0*DB0(x, MB2, GaugeXiW*MW2)*DBLE(MB**INT(4.D0)) - 2.D0*DB0(x, MB2, MW2)*DBLE(MW**INT(4.D0)) + DB0(x, MB2, MW2)*DBLE(x**INT(2.&
  &D0)) - 1.D0*DB0(x, MB2, GaugeXiW*MW2)*DBLE(x**INT(2.D0))))/(MW2*PI2*SW2*x)

  totalAmplitude = (0D0,0D0)
 do j=1,15
  totalAmplitude = totalAmplitude + amplitudes(j)
 end do
 DSelfCCLeft = totalAmplitude
end function DSelfCCLeft

