double complex function DSelfTTRight(x)
 use constants
 implicit none
#include "looptools.h"
 double precision, intent(in) :: x
 integer :: j
 double complex :: totalAmplitude
 double complex :: amplitudes(15)

 amplitudes(1) = (0.0078125D0*CA2*EL2*MT2*(B0(x, Mh02, MT2) - 1.D0*Mh02*DB0(x, Mh02, MT2) + MT2*DB0(x, Mh02, MT2) + x*DB0(x, Mh02&
  &, MT2)))/ (MW2*PI2*SB2*SW2*x) - (0.0078125D0*CA2*EL2*MT2*(A0(Mh02) - 1.D0*A0(MT2) - 1.D0*Mh02*B0(x, Mh02, MT2) + MT2*B0(x, Mh0&
  &2, MT2) + x*B0(x, Mh02, MT2))*DBLE(x**INT(-2.D0)))/(MW2*PI2*SB2*SW2)

 amplitudes(2) = (0.0078125D0*EL2*MT2*SA2*(B0(x, MHH2, MT2) - 1.D0*MHH2*DB0(x, MHH2, MT2) + MT2*DB0(x, MHH2, MT2) + x*DB0(x, MHH2&
  &, MT2)))/ (MW2*PI2*SB2*SW2*x) - (0.0078125D0*EL2*MT2*SA2*(A0(MHH2) - 1.D0*A0(MT2) - 1.D0*MHH2*B0(x, MHH2, MT2) + MT2*B0(x, MHH&
  &2, MT2) + x*B0(x, MHH2, MT2))*DBLE(x**INT(-2.D0)))/(MW2*PI2*SB2*SW2)

 amplitudes(3) = (0.0078125D0*EL2*MT2*(B0(x, MA02, MT2) - 1.D0*MA02*DB0(x, MA02, MT2) + MT2*DB0(x, MA02, MT2) + x*DB0(x, MA02, MT&
  &2)))/(MW2*PI2*SW2*TB2*x) - (0.0078125D0*EL2*MT2*(A0(MA02) - 1.D0*A0(MT2) - 1.D0*MA02*B0(x, MA02, MT2) + MT2*B0(x, MA02, MT2) +&
  & x*B0(x, MA02, MT2))*DBLE(x**INT(-2.D0)))/ (MW2*PI2*SW2*TB2)

 amplitudes(4) = (0.0078125D0*EL2*MT2*(B0(x, MT2, GaugeXiZ*MZ2) + MT2*DB0(x, MT2, GaugeXiZ*MZ2) - 1.D0*GaugeXiZ*MZ2*DB0(x, MT2, G&
  &augeXiZ*MZ2) + x*DB0(x, MT2, GaugeXiZ*MZ2)))/(MW2*PI2*SW2*x) - (0.0078125D0*EL2*MT2*(-1.D0*A0(MT2) + A0(GaugeXiZ*MZ2) + MT2*B0&
  &(x, MT2, GaugeXiZ*MZ2) - 1.D0*GaugeXiZ*MZ2*B0(x, MT2, GaugeXiZ*MZ2) + x*B0(x, MT2, GaugeXiZ*MZ2))*DBLE(x**INT(-2.D0)))/(MW2*PI&
  &2*SW2)

 amplitudes(5) = (0.015625D0*CKM31*CKMC31*EL2*(MT2*B0(x, MD2, MHp2) + MD2*MT2*DB0(x, MD2, MHp2) - 1.D0*MHp2*MT2*DB0(x, MD2, MHp2)&
  & + MT2*x*DB0(x, MD2, MHp2)))/ (MW2*PI2*SW2*TB2*x) - (0.015625D0*CKM31*CKMC31*EL2*(-1.D0*MT2*A0(MD2) + MT2*A0(MHp2) + MD2*MT2*B&
  &0(x, MD2, MHp2) - 1.D0*MHp2*MT2*B0(x, MD2, MHp2) + MT2*x*B0(x, MD2, MHp2))*DBLE(x**INT(-2.D0)))/(MW2*PI2*SW2*TB2)

 amplitudes(6) = (0.015625D0*CKM32*CKMC32*EL2*(MT2*B0(x, MHp2, MS2) - 1.D0*MHp2*MT2*DB0(x, MHp2, MS2) + MS2*MT2*DB0(x, MHp2, MS2)&
  & + MT2*x*DB0(x, MHp2, MS2)))/ (MW2*PI2*SW2*TB2*x) - (0.015625D0*CKM32*CKMC32*EL2*(MT2*A0(MHp2) - 1.D0*MT2*A0(MS2) - 1.D0*MHp2*&
  &MT2*B0(x, MHp2, MS2) + MS2*MT2*B0(x, MHp2, MS2) + MT2*x*B0(x, MHp2, MS2))*DBLE(x**INT(-2.D0)))/(MW2*PI2*SW2*TB2)

 amplitudes(7) = (0.015625D0*CKM33*CKMC33*EL2*(MT2*B0(x, MB2, MHp2) + MB2*MT2*DB0(x, MB2, MHp2) - 1.D0*MHp2*MT2*DB0(x, MB2, MHp2)&
  & + MT2*x*DB0(x, MB2, MHp2)))/ (MW2*PI2*SW2*TB2*x) - (0.015625D0*CKM33*CKMC33*EL2*(-1.D0*MT2*A0(MB2) + MT2*A0(MHp2) + MB2*MT2*B&
  &0(x, MB2, MHp2) - 1.D0*MHp2*MT2*B0(x, MB2, MHp2) + MT2*x*B0(x, MB2, MHp2))*DBLE(x**INT(-2.D0)))/(MW2*PI2*SW2*TB2)

 amplitudes(8) = (0.015625D0*CKM31*CKMC31*EL2*(MT2*B0(x, MD2, GaugeXiW*MW2) + MD2*MT2*DB0(x, MD2, GaugeXiW*MW2) - 1.D0*GaugeXiW*M&
  &T2*MW2*DB0(x, MD2, GaugeXiW*MW2) + MT2*x*DB0(x, MD2, GaugeXiW*MW2)))/(MW2*PI2*SW2*x) - (0.015625D0*CKM31*CKMC31*EL2*(-1.D0*MT2&
  &*A0(MD2) + MT2*A0(GaugeXiW*MW2) + MD2*MT2*B0(x, MD2, GaugeXiW*MW2) - 1.D0*GaugeXiW*MT2*MW2*B0(x, MD2, GaugeXiW*MW2) + MT2*x*B0&
  &(x, MD2, GaugeXiW*MW2))*DBLE(x**INT(-2.D0)))/(MW2*PI2*SW2)

 amplitudes(9) = (0.015625D0*CKM32*CKMC32*EL2*(MT2*B0(x, MS2, GaugeXiW*MW2) + MS2*MT2*DB0(x, MS2, GaugeXiW*MW2) - 1.D0*GaugeXiW*M&
  &T2*MW2*DB0(x, MS2, GaugeXiW*MW2) + MT2*x*DB0(x, MS2, GaugeXiW*MW2)))/(MW2*PI2*SW2*x) - (0.015625D0*CKM32*CKMC32*EL2*(-1.D0*MT2&
  &*A0(MS2) + MT2*A0(GaugeXiW*MW2) + MS2*MT2*B0(x, MS2, GaugeXiW*MW2) - 1.D0*GaugeXiW*MT2*MW2*B0(x, MS2, GaugeXiW*MW2) + MT2*x*B0&
  &(x, MS2, GaugeXiW*MW2))*DBLE(x**INT(-2.D0)))/(MW2*PI2*SW2)

 amplitudes(10) = (0.015625D0*CKM33*CKMC33*EL2*(MT2*B0(x, MB2, GaugeXiW*MW2) + MB2*MT2*DB0(x, MB2, GaugeXiW*MW2) - 1.D0*GaugeXiW*&
  &MT2*MW2*DB0(x, MB2, GaugeXiW*MW2) + MT2*x*DB0(x, MB2, GaugeXiW*MW2)))/(MW2*PI2*SW2*x) - (0.015625D0*CKM33*CKMC33*EL2*(-1.D0*MT&
  &2*A0(MB2) + MT2*A0(GaugeXiW*MW2) + MB2*MT2*B0(x, MB2, GaugeXiW*MW2) - 1.D0*GaugeXiW*MT2*MW2*B0(x, MB2, GaugeXiW*MW2) + MT2*x*B&
  &0(x, MB2, GaugeXiW*MW2))*DBLE(x**INT(-2.D0)))/(MW2*PI2*SW2)

 amplitudes(11) = (-0.013888888888888888D0*EL2*DBLE(x**INT(-2.D0))*(-2.D0*x - 2.D0*A0(MT2) + MT2*B0(x, 0.D0, MT2) + GaugeXiA*MT2*&
  &B0(x, 0.D0, MT2) + x*B0(x, 0.D0, MT2) + GaugeXiA*x*B0(x, 0.D0, MT2) - 2.D0*MT2*x*C0Mine(DBLE(0.D0), DBLE(x), DBLE(x), DBLE(0.D&
  &0), DBLE(0.D0), DBLE(MT2)) + 2.D0*GaugeXiA*MT2*x*C0Mine(DBLE(0.D0), DBLE(x), DBLE(x), DBLE(0.D0), DBLE(0.D0), DBLE(MT2)) + C0M&
  &ine(DBLE(0.D0), DBLE(x), DBLE(x), DBLE(0.D0), DBLE(0.D0), DBLE(MT2))*DBLE(MT**INT(4.D0)) - 1.D0*GaugeXiA*C0Mine(DBLE(0.D0), DB&
  &LE(x), DBLE(x), DBLE(0.D0), DBLE(0.D0), DBLE(MT2))*DBLE(MT**INT(4.D0)) + C0Mine(DBLE(0.D0), DBLE(x), DBLE(x), DBLE(0.D0), DBLE&
  &(0.D0), DBLE(MT2))*DBLE(x**INT(2.D0)) - 1.D0*GaugeXiA*C0Mine(DBLE(0.D0), DBLE(x), DBLE(x), DBLE(0.D0), DBLE(0.D0), DBLE(MT2))*&
  &DBLE(x**INT(2.D0))))/PI2 + (0.013888888888888888D0*EL2*(-2.D0 + B0(x, 0.D0, MT2) + GaugeXiA*B0(x, 0.D0, MT2) - 2.D0*MT2*C0Mine&
  &(DBLE(0.D0), DBLE(x), DBLE(x), DBLE(0.D0), DBLE(0.D0), DBLE(MT2)) + 2.D0*GaugeXiA*MT2*C0Mine(DBLE(0.D0), DBLE(x), DBLE(x), DBL&
  &E(0.D0), DBLE(0.D0), DBLE(MT2)) + 2.D0*x*C0Mine(DBLE(0.D0), DBLE(x), DBLE(x), DBLE(0.D0), DBLE(0.D0), DBLE(MT2)) - 2.D0*GaugeX&
  &iA*x*C0Mine(DBLE(0.D0), DBLE(x), DBLE(x), DBLE(0.D0), DBLE(0.D0), DBLE(MT2)) + MT2*DB0(x, 0.D0, MT2) + GaugeXiA*MT2*DB0(x, 0.D&
  &0, MT2) + x*DB0(x, 0.D0, MT2) + GaugeXiA*x*DB0(x, 0.D0, MT2) - 2.D0*MT2*x*(DC01Mine(DBLE(0.D0), DBLE(x), DBLE(x), DBLE(0.D0), &
  &DBLE(0.D0), DBLE(MT2)) + DC02Mine(DBLE(0.D0), DBLE(x), DBLE(x), DBLE(0.D0), DBLE(0.D0), DBLE(MT2))) + 2.D0*GaugeXiA*MT2*x*(DC0&
  &1Mine(DBLE(0.D0), DBLE(x), DBLE(x), DBLE(0.D0), DBLE(0.D0), DBLE(MT2)) + DC02Mine(DBLE(0.D0), DBLE(x), DBLE(x), DBLE(0.D0), DB&
  &LE(0.D0), DBLE(MT2))) + DBLE(MT**INT(4.D0))*(DC01Mine(DBLE(0.D0), DBLE(x), DBLE(x), DBLE(0.D0), DBLE(0.D0), DBLE(MT2)) + DC02M&
  &ine(DBLE(0.D0), DBLE(x), DBLE(x), DBLE(0.D0), DBLE(0.D0), DBLE(MT2))) - 1.D0*GaugeXiA*DBLE(MT**INT(4.D0))* (DC01Mine(DBLE(0.D0&
  &), DBLE(x), DBLE(x), DBLE(0.D0), DBLE(0.D0), DBLE(MT2)) + DC02Mine(DBLE(0.D0), DBLE(x), DBLE(x), DBLE(0.D0), DBLE(0.D0), DBLE(&
  &MT2))) + DBLE(x**INT(2.D0))*(DC01Mine(DBLE(0.D0), DBLE(x), DBLE(x), DBLE(0.D0), DBLE(0.D0), DBLE(MT2)) + DC02Mine(DBLE(0.D0), &
  &DBLE(x), DBLE(x), DBLE(0.D0), DBLE(0.D0), DBLE(MT2))) - 1.D0*GaugeXiA*DBLE(x**INT(2.D0))* (DC01Mine(DBLE(0.D0), DBLE(x), DBLE(&
  &x), DBLE(0.D0), DBLE(0.D0), DBLE(MT2)) + DC02Mine(DBLE(0.D0), DBLE(x), DBLE(x), DBLE(0.D0), DBLE(0.D0), DBLE(MT2)))))/(PI2*x)

 amplitudes(12) = (-0.0008680555555555555D0*EL2*DBLE(x**INT(-2.D0))*(-32.D0*MZ2*x*DBLE(SW**INT(4.D0)) - 32.D0*MZ2*A0(MT2)*DBLE(SW&
  &**INT(4.D0)) + 16.D0*MT2*A0(MZ2)*DBLE(SW**INT(4.D0)) + 32.D0*MZ2*A0(MZ2)*DBLE(SW**INT(4.D0)) - 16.D0*x*A0(MZ2)*DBLE(SW**INT(4.&
  &D0)) - 16.D0*MT2*A0(GaugeXiZ*MZ2)*DBLE(SW**INT(4.D0)) + 16.D0*x*A0(GaugeXiZ*MZ2)*DBLE(SW**INT(4.D0)) + 16.D0*MT2*MZ2*B0(x, MT2&
  &, MZ2)*DBLE(SW**INT(4.D0)) - 32.D0*MT2*x*B0(x, MT2, MZ2)*DBLE(SW**INT(4.D0)) + 16.D0*MZ2*x*B0(x, MT2, MZ2)*DBLE(SW**INT(4.D0))&
  & + 16.D0*GaugeXiZ*MT2*MZ2*B0(x, MT2, GaugeXiZ*MZ2)*DBLE(SW**INT(4.D0)) + 32.D0*MT2*x*B0(x, MT2, GaugeXiZ*MZ2)*DBLE(SW**INT(4.D&
  &0)) + 16.D0*GaugeXiZ*MZ2*x*B0(x, MT2, GaugeXiZ*MZ2)*DBLE(SW**INT(4.D0)) + 16.D0*B0(x, MT2, MZ2)*DBLE(MT**INT(4.D0))*DBLE(SW**I&
  &NT(4.D0)) - 16.D0*B0(x, MT2, GaugeXiZ*MZ2)*DBLE(MT**INT(4.D0))*DBLE(SW**INT(4.D0)) - 32.D0*B0(x, MT2, MZ2)*DBLE(MZ**INT(4.D0))&
  &*DBLE(SW**INT(4.D0)) + 16.D0*B0(x, MT2, MZ2)*DBLE(SW**INT(4.D0))*DBLE(x**INT(2.D0)) - 16.D0*B0(x, MT2, GaugeXiZ*MZ2)*DBLE(SW**&
  &INT(4.D0))*DBLE(x**INT(2.D0))))/(CW2*MZ2*PI2*SW2) + (0.0008680555555555555D0*EL2*(-32.D0*MZ2*DBLE(SW**INT(4.D0)) - 16.D0*A0(MZ&
  &2)*DBLE(SW**INT(4.D0)) + 16.D0*A0(GaugeXiZ*MZ2)*DBLE(SW**INT(4.D0)) - 32.D0*MT2*B0(x, MT2, MZ2)*DBLE(SW**INT(4.D0)) + 16.D0*MZ&
  &2*B0(x, MT2, MZ2)*DBLE(SW**INT(4.D0)) + 32.D0*x*B0(x, MT2, MZ2)*DBLE(SW**INT(4.D0)) + 32.D0*MT2*B0(x, MT2, GaugeXiZ*MZ2)*DBLE(&
  &SW**INT(4.D0)) + 16.D0*GaugeXiZ*MZ2*B0(x, MT2, GaugeXiZ*MZ2)*DBLE(SW**INT(4.D0)) - 32.D0*x*B0(x, MT2, GaugeXiZ*MZ2)*DBLE(SW**I&
  &NT(4.D0)) + 16.D0*MT2*MZ2*DB0(x, MT2, MZ2)*DBLE(SW**INT(4.D0)) - 32.D0*MT2*x*DB0(x, MT2, MZ2)*DBLE(SW**INT(4.D0)) + 16.D0*MZ2*&
  &x*DB0(x, MT2, MZ2)*DBLE(SW**INT(4.D0)) + 16.D0*GaugeXiZ*MT2*MZ2*DB0(x, MT2, GaugeXiZ*MZ2)*DBLE(SW**INT(4.D0)) + 32.D0*MT2*x*DB&
  &0(x, MT2, GaugeXiZ*MZ2)*DBLE(SW**INT(4.D0)) + 16.D0*GaugeXiZ*MZ2*x*DB0(x, MT2, GaugeXiZ*MZ2)*DBLE(SW**INT(4.D0)) + 16.D0*DB0(x&
  &, MT2, MZ2)*DBLE(MT**INT(4.D0))*DBLE(SW**INT(4.D0)) - 16.D0*DB0(x, MT2, GaugeXiZ*MZ2)*DBLE(MT**INT(4.D0))*DBLE(SW**INT(4.D0)) &
  &- 32.D0*DB0(x, MT2, MZ2)*DBLE(MZ**INT(4.D0))*DBLE(SW**INT(4.D0)) + 16.D0*DB0(x, MT2, MZ2)*DBLE(SW**INT(4.D0))*DBLE(x**INT(2.D0&
  &)) - 16.D0*DB0(x, MT2, GaugeXiZ*MZ2)*DBLE(SW**INT(4.D0))*DBLE(x**INT(2.D0))))/ (CW2*MZ2*PI2*SW2*x)

 amplitudes(13) = 0.D0

 amplitudes(14) = 0.D0

 amplitudes(15) = 0.D0

  totalAmplitude = (0D0,0D0)
 do j=1,15
  totalAmplitude = totalAmplitude + amplitudes(j)
 end do
 DSelfTTRight = totalAmplitude
end function DSelfTTRight

