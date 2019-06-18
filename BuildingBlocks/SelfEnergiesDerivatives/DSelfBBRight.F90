double complex function DSelfBBRight(x)
 use constants
 implicit none
#include "looptools.h"
 double precision, intent(in) :: x
 integer :: j
 double complex :: totalAmplitude
 double complex :: amplitudes(15)

 amplitudes(1) = (0.0078125D0*EL2*MB2*(B0(x, MB2, Mh02) + MB2*DB0(x, MB2, Mh02) - 1.D0*Mh02*DB0(x, MB2, Mh02) + x*DB0(x, MB2, Mh0&
  &2))*DBLE(Yuk1**INT(2.D0)))/ (MW2*PI2*SW2*x) - (0.0078125D0*EL2*MB2*(-1.D0*A0(MB2) + A0(Mh02) + MB2*B0(x, MB2, Mh02) - 1.D0*Mh0&
  &2*B0(x, MB2, Mh02) + x*B0(x, MB2, Mh02))* DBLE(x**INT(-2.D0))*DBLE(Yuk1**INT(2.D0)))/(MW2*PI2*SW2)

 amplitudes(2) = (0.0078125D0*EL2*MB2*(B0(x, MB2, MHH2) + MB2*DB0(x, MB2, MHH2) - 1.D0*MHH2*DB0(x, MB2, MHH2) + x*DB0(x, MB2, MHH&
  &2))*DBLE(Yuk2**INT(2.D0)))/ (MW2*PI2*SW2*x) - (0.0078125D0*EL2*MB2*(-1.D0*A0(MB2) + A0(MHH2) + MB2*B0(x, MB2, MHH2) - 1.D0*MHH&
  &2*B0(x, MB2, MHH2) + x*B0(x, MB2, MHH2))* DBLE(x**INT(-2.D0))*DBLE(Yuk2**INT(2.D0)))/(MW2*PI2*SW2)

 amplitudes(3) = (0.0078125D0*EL2*MB2*(B0(x, MA02, MB2) - 1.D0*MA02*DB0(x, MA02, MB2) + MB2*DB0(x, MA02, MB2) + x*DB0(x, MA02, MB&
  &2))*DBLE(Yuk3**INT(2.D0)))/ (MW2*PI2*SW2*x) - (0.0078125D0*EL2*MB2*(A0(MA02) - 1.D0*A0(MB2) - 1.D0*MA02*B0(x, MA02, MB2) + MB2&
  &*B0(x, MA02, MB2) + x*B0(x, MA02, MB2))* DBLE(x**INT(-2.D0))*DBLE(Yuk3**INT(2.D0)))/(MW2*PI2*SW2)

 amplitudes(4) = (0.0078125D0*EL2*MB2*(B0(x, MB2, GaugeXiZ*MZ2) + MB2*DB0(x, MB2, GaugeXiZ*MZ2) - 1.D0*GaugeXiZ*MZ2*DB0(x, MB2, G&
  &augeXiZ*MZ2) + x*DB0(x, MB2, GaugeXiZ*MZ2)))/(MW2*PI2*SW2*x) - (0.0078125D0*EL2*MB2*(-1.D0*A0(MB2) + A0(GaugeXiZ*MZ2) + MB2*B0&
  &(x, MB2, GaugeXiZ*MZ2) - 1.D0*GaugeXiZ*MZ2*B0(x, MB2, GaugeXiZ*MZ2) + x*B0(x, MB2, GaugeXiZ*MZ2))*DBLE(x**INT(-2.D0)))/(MW2*PI&
  &2*SW2)

 amplitudes(5) = (-0.015625D0*CKM13*CKMC13*EL2*DBLE(x**INT(-2.D0))*(MB2*TB2*A0(MHp2)*DBLE(Yuk3**INT(2.D0)) - 1.D0*MB2*TB2*A0(MU2)&
  &*DBLE(Yuk3**INT(2.D0)) - 1.D0*MB2*MHp2*TB2*B0(x, MHp2, MU2)*DBLE(Yuk3**INT(2.D0)) + MB2*MU2*TB2*B0(x, MHp2, MU2)*DBLE(Yuk3**IN&
  &T(2.D0)) + MB2*TB2*x*B0(x, MHp2, MU2)*DBLE(Yuk3**INT(2.D0))))/(MW2*PI2*SW2*TB2) + (0.015625D0*CKM13*CKMC13*EL2*(MB2*TB2*B0(x, &
  &MHp2, MU2)*DBLE(Yuk3**INT(2.D0)) - 1.D0*MB2*MHp2*TB2*DB0(x, MHp2, MU2)*DBLE(Yuk3**INT(2.D0)) + MB2*MU2*TB2*DB0(x, MHp2, MU2)*D&
  &BLE(Yuk3**INT(2.D0)) + MB2*TB2*x*DB0(x, MHp2, MU2)*DBLE(Yuk3**INT(2.D0))))/(MW2*PI2*SW2*TB2*x)

 amplitudes(6) = (-0.015625D0*CKM23*CKMC23*EL2*DBLE(x**INT(-2.D0))*(-1.D0*MB2*TB2*A0(MC2)*DBLE(Yuk3**INT(2.D0)) + MB2*TB2*A0(MHp2&
  &)*DBLE(Yuk3**INT(2.D0)) + MB2*MC2*TB2*B0(x, MC2, MHp2)*DBLE(Yuk3**INT(2.D0)) - 1.D0*MB2*MHp2*TB2*B0(x, MC2, MHp2)*DBLE(Yuk3**I&
  &NT(2.D0)) + MB2*TB2*x*B0(x, MC2, MHp2)*DBLE(Yuk3**INT(2.D0))))/(MW2*PI2*SW2*TB2) + (0.015625D0*CKM23*CKMC23*EL2*(MB2*TB2*B0(x,&
  & MC2, MHp2)*DBLE(Yuk3**INT(2.D0)) + MB2*MC2*TB2*DB0(x, MC2, MHp2)*DBLE(Yuk3**INT(2.D0)) - 1.D0*MB2*MHp2*TB2*DB0(x, MC2, MHp2)*&
  &DBLE(Yuk3**INT(2.D0)) + MB2*TB2*x*DB0(x, MC2, MHp2)*DBLE(Yuk3**INT(2.D0))))/(MW2*PI2*SW2*TB2*x)

 amplitudes(7) = (-0.015625D0*CKM33*CKMC33*EL2*DBLE(x**INT(-2.D0))*(MB2*TB2*A0(MHp2)*DBLE(Yuk3**INT(2.D0)) - 1.D0*MB2*TB2*A0(MT2)&
  &*DBLE(Yuk3**INT(2.D0)) - 1.D0*MB2*MHp2*TB2*B0(x, MHp2, MT2)*DBLE(Yuk3**INT(2.D0)) + MB2*MT2*TB2*B0(x, MHp2, MT2)*DBLE(Yuk3**IN&
  &T(2.D0)) + MB2*TB2*x*B0(x, MHp2, MT2)*DBLE(Yuk3**INT(2.D0))))/(MW2*PI2*SW2*TB2) + (0.015625D0*CKM33*CKMC33*EL2*(MB2*TB2*B0(x, &
  &MHp2, MT2)*DBLE(Yuk3**INT(2.D0)) - 1.D0*MB2*MHp2*TB2*DB0(x, MHp2, MT2)*DBLE(Yuk3**INT(2.D0)) + MB2*MT2*TB2*DB0(x, MHp2, MT2)*D&
  &BLE(Yuk3**INT(2.D0)) + MB2*TB2*x*DB0(x, MHp2, MT2)*DBLE(Yuk3**INT(2.D0))))/(MW2*PI2*SW2*TB2*x)

 amplitudes(8) = (0.015625D0*CKM13*CKMC13*EL2*(MB2*B0(x, MU2, GaugeXiW*MW2) + MB2*MU2*DB0(x, MU2, GaugeXiW*MW2) - 1.D0*GaugeXiW*M&
  &B2*MW2*DB0(x, MU2, GaugeXiW*MW2) + MB2*x*DB0(x, MU2, GaugeXiW*MW2)))/(MW2*PI2*SW2*x) - (0.015625D0*CKM13*CKMC13*EL2*(-1.D0*MB2&
  &*A0(MU2) + MB2*A0(GaugeXiW*MW2) + MB2*MU2*B0(x, MU2, GaugeXiW*MW2) - 1.D0*GaugeXiW*MB2*MW2*B0(x, MU2, GaugeXiW*MW2) + MB2*x*B0&
  &(x, MU2, GaugeXiW*MW2))*DBLE(x**INT(-2.D0)))/(MW2*PI2*SW2)

 amplitudes(9) = (0.015625D0*CKM23*CKMC23*EL2*(MB2*B0(x, MC2, GaugeXiW*MW2) + MB2*MC2*DB0(x, MC2, GaugeXiW*MW2) - 1.D0*GaugeXiW*M&
  &B2*MW2*DB0(x, MC2, GaugeXiW*MW2) + MB2*x*DB0(x, MC2, GaugeXiW*MW2)))/(MW2*PI2*SW2*x) - (0.015625D0*CKM23*CKMC23*EL2*(-1.D0*MB2&
  &*A0(MC2) + MB2*A0(GaugeXiW*MW2) + MB2*MC2*B0(x, MC2, GaugeXiW*MW2) - 1.D0*GaugeXiW*MB2*MW2*B0(x, MC2, GaugeXiW*MW2) + MB2*x*B0&
  &(x, MC2, GaugeXiW*MW2))*DBLE(x**INT(-2.D0)))/(MW2*PI2*SW2)

 amplitudes(10) = (0.015625D0*CKM33*CKMC33*EL2*(MB2*B0(x, MT2, GaugeXiW*MW2) + MB2*MT2*DB0(x, MT2, GaugeXiW*MW2) - 1.D0*GaugeXiW*&
  &MB2*MW2*DB0(x, MT2, GaugeXiW*MW2) + MB2*x*DB0(x, MT2, GaugeXiW*MW2)))/(MW2*PI2*SW2*x) - (0.015625D0*CKM33*CKMC33*EL2*(-1.D0*MB&
  &2*A0(MT2) + MB2*A0(GaugeXiW*MW2) + MB2*MT2*B0(x, MT2, GaugeXiW*MW2) - 1.D0*GaugeXiW*MB2*MW2*B0(x, MT2, GaugeXiW*MW2) + MB2*x*B&
  &0(x, MT2, GaugeXiW*MW2))*DBLE(x**INT(-2.D0)))/(MW2*PI2*SW2)

 amplitudes(11) = (-0.003472222222222222D0*EL2*DBLE(x**INT(-2.D0))*(-2.D0*x - 2.D0*A0(MB2) + MB2*B0(x, 0.D0, MB2) + GaugeXiA*MB2*&
  &B0(x, 0.D0, MB2) + x*B0(x, 0.D0, MB2) + GaugeXiA*x*B0(x, 0.D0, MB2) - 2.D0*MB2*x*C0Mine(DBLE(0.D0), DBLE(x), DBLE(x), DBLE(0.D&
  &0), DBLE(0.D0), DBLE(MB2)) + 2.D0*GaugeXiA*MB2*x*C0Mine(DBLE(0.D0), DBLE(x), DBLE(x), DBLE(0.D0), DBLE(0.D0), DBLE(MB2)) + C0M&
  &ine(DBLE(0.D0), DBLE(x), DBLE(x), DBLE(0.D0), DBLE(0.D0), DBLE(MB2))*DBLE(MB**INT(4.D0)) - 1.D0*GaugeXiA*C0Mine(DBLE(0.D0), DB&
  &LE(x), DBLE(x), DBLE(0.D0), DBLE(0.D0), DBLE(MB2))*DBLE(MB**INT(4.D0)) + C0Mine(DBLE(0.D0), DBLE(x), DBLE(x), DBLE(0.D0), DBLE&
  &(0.D0), DBLE(MB2))*DBLE(x**INT(2.D0)) - 1.D0*GaugeXiA*C0Mine(DBLE(0.D0), DBLE(x), DBLE(x), DBLE(0.D0), DBLE(0.D0), DBLE(MB2))*&
  &DBLE(x**INT(2.D0))))/PI2 + (0.003472222222222222D0*EL2*(-2.D0 + B0(x, 0.D0, MB2) + GaugeXiA*B0(x, 0.D0, MB2) - 2.D0*MB2*C0Mine&
  &(DBLE(0.D0), DBLE(x), DBLE(x), DBLE(0.D0), DBLE(0.D0), DBLE(MB2)) + 2.D0*GaugeXiA*MB2*C0Mine(DBLE(0.D0), DBLE(x), DBLE(x), DBL&
  &E(0.D0), DBLE(0.D0), DBLE(MB2)) + 2.D0*x*C0Mine(DBLE(0.D0), DBLE(x), DBLE(x), DBLE(0.D0), DBLE(0.D0), DBLE(MB2)) - 2.D0*GaugeX&
  &iA*x*C0Mine(DBLE(0.D0), DBLE(x), DBLE(x), DBLE(0.D0), DBLE(0.D0), DBLE(MB2)) + MB2*DB0(x, 0.D0, MB2) + GaugeXiA*MB2*DB0(x, 0.D&
  &0, MB2) + x*DB0(x, 0.D0, MB2) + GaugeXiA*x*DB0(x, 0.D0, MB2) - 2.D0*MB2*x*(DC01Mine(DBLE(0.D0), DBLE(x), DBLE(x), DBLE(0.D0), &
  &DBLE(0.D0), DBLE(MB2)) + DC02Mine(DBLE(0.D0), DBLE(x), DBLE(x), DBLE(0.D0), DBLE(0.D0), DBLE(MB2))) + 2.D0*GaugeXiA*MB2*x*(DC0&
  &1Mine(DBLE(0.D0), DBLE(x), DBLE(x), DBLE(0.D0), DBLE(0.D0), DBLE(MB2)) + DC02Mine(DBLE(0.D0), DBLE(x), DBLE(x), DBLE(0.D0), DB&
  &LE(0.D0), DBLE(MB2))) + DBLE(MB**INT(4.D0))*(DC01Mine(DBLE(0.D0), DBLE(x), DBLE(x), DBLE(0.D0), DBLE(0.D0), DBLE(MB2)) + DC02M&
  &ine(DBLE(0.D0), DBLE(x), DBLE(x), DBLE(0.D0), DBLE(0.D0), DBLE(MB2))) - 1.D0*GaugeXiA*DBLE(MB**INT(4.D0))* (DC01Mine(DBLE(0.D0&
  &), DBLE(x), DBLE(x), DBLE(0.D0), DBLE(0.D0), DBLE(MB2)) + DC02Mine(DBLE(0.D0), DBLE(x), DBLE(x), DBLE(0.D0), DBLE(0.D0), DBLE(&
  &MB2))) + DBLE(x**INT(2.D0))*(DC01Mine(DBLE(0.D0), DBLE(x), DBLE(x), DBLE(0.D0), DBLE(0.D0), DBLE(MB2)) + DC02Mine(DBLE(0.D0), &
  &DBLE(x), DBLE(x), DBLE(0.D0), DBLE(0.D0), DBLE(MB2))) - 1.D0*GaugeXiA*DBLE(x**INT(2.D0))* (DC01Mine(DBLE(0.D0), DBLE(x), DBLE(&
  &x), DBLE(0.D0), DBLE(0.D0), DBLE(MB2)) + DC02Mine(DBLE(0.D0), DBLE(x), DBLE(x), DBLE(0.D0), DBLE(0.D0), DBLE(MB2)))))/(PI2*x)

 amplitudes(12) = (-0.0008680555555555555D0*EL2*DBLE(x**INT(-2.D0))*(-8.D0*MZ2*x*DBLE(SW**INT(4.D0)) - 8.D0*MZ2*A0(MB2)*DBLE(SW**&
  &INT(4.D0)) + 4.D0*MB2*A0(MZ2)*DBLE(SW**INT(4.D0)) + 8.D0*MZ2*A0(MZ2)*DBLE(SW**INT(4.D0)) - 4.D0*x*A0(MZ2)*DBLE(SW**INT(4.D0)) &
  &- 4.D0*MB2*A0(GaugeXiZ*MZ2)*DBLE(SW**INT(4.D0)) + 4.D0*x*A0(GaugeXiZ*MZ2)*DBLE(SW**INT(4.D0)) + 4.D0*MB2*MZ2*B0(x, MB2, MZ2)*D&
  &BLE(SW**INT(4.D0)) - 8.D0*MB2*x*B0(x, MB2, MZ2)*DBLE(SW**INT(4.D0)) + 4.D0*MZ2*x*B0(x, MB2, MZ2)*DBLE(SW**INT(4.D0)) + 4.D0*Ga&
  &ugeXiZ*MB2*MZ2*B0(x, MB2, GaugeXiZ*MZ2)*DBLE(SW**INT(4.D0)) + 8.D0*MB2*x*B0(x, MB2, GaugeXiZ*MZ2)*DBLE(SW**INT(4.D0)) + 4.D0*G&
  &augeXiZ*MZ2*x*B0(x, MB2, GaugeXiZ*MZ2)*DBLE(SW**INT(4.D0)) + 4.D0*B0(x, MB2, MZ2)*DBLE(MB**INT(4.D0))*DBLE(SW**INT(4.D0)) - 4.&
  &D0*B0(x, MB2, GaugeXiZ*MZ2)*DBLE(MB**INT(4.D0))*DBLE(SW**INT(4.D0)) - 8.D0*B0(x, MB2, MZ2)*DBLE(MZ**INT(4.D0))*DBLE(SW**INT(4.&
  &D0)) + 4.D0*B0(x, MB2, MZ2)*DBLE(SW**INT(4.D0))*DBLE(x**INT(2.D0)) - 4.D0*B0(x, MB2, GaugeXiZ*MZ2)*DBLE(SW**INT(4.D0))*DBLE(x*&
  &*INT(2.D0))))/ (CW2*MZ2*PI2*SW2) + (0.0008680555555555555D0*EL2*(-8.D0*MZ2*DBLE(SW**INT(4.D0)) - 4.D0*A0(MZ2)*DBLE(SW**INT(4.D&
  &0)) + 4.D0*A0(GaugeXiZ*MZ2)*DBLE(SW**INT(4.D0)) - 8.D0*MB2*B0(x, MB2, MZ2)*DBLE(SW**INT(4.D0)) + 4.D0*MZ2*B0(x, MB2, MZ2)*DBLE&
  &(SW**INT(4.D0)) + 8.D0*x*B0(x, MB2, MZ2)*DBLE(SW**INT(4.D0)) + 8.D0*MB2*B0(x, MB2, GaugeXiZ*MZ2)*DBLE(SW**INT(4.D0)) + 4.D0*Ga&
  &ugeXiZ*MZ2*B0(x, MB2, GaugeXiZ*MZ2)*DBLE(SW**INT(4.D0)) - 8.D0*x*B0(x, MB2, GaugeXiZ*MZ2)*DBLE(SW**INT(4.D0)) + 4.D0*MB2*MZ2*D&
  &B0(x, MB2, MZ2)*DBLE(SW**INT(4.D0)) - 8.D0*MB2*x*DB0(x, MB2, MZ2)*DBLE(SW**INT(4.D0)) + 4.D0*MZ2*x*DB0(x, MB2, MZ2)*DBLE(SW**I&
  &NT(4.D0)) + 4.D0*GaugeXiZ*MB2*MZ2*DB0(x, MB2, GaugeXiZ*MZ2)*DBLE(SW**INT(4.D0)) + 8.D0*MB2*x*DB0(x, MB2, GaugeXiZ*MZ2)*DBLE(SW&
  &**INT(4.D0)) + 4.D0*GaugeXiZ*MZ2*x*DB0(x, MB2, GaugeXiZ*MZ2)*DBLE(SW**INT(4.D0)) + 4.D0*DB0(x, MB2, MZ2)*DBLE(MB**INT(4.D0))*D&
  &BLE(SW**INT(4.D0)) - 4.D0*DB0(x, MB2, GaugeXiZ*MZ2)*DBLE(MB**INT(4.D0))*DBLE(SW**INT(4.D0)) - 8.D0*DB0(x, MB2, MZ2)*DBLE(MZ**I&
  &NT(4.D0))*DBLE(SW**INT(4.D0)) + 4.D0*DB0(x, MB2, MZ2)*DBLE(SW**INT(4.D0))*DBLE(x**INT(2.D0)) - 4.D0*DB0(x, MB2, GaugeXiZ*MZ2)*&
  &DBLE(SW**INT(4.D0))*DBLE(x**INT(2.D0))))/(CW2*MZ2*PI2*SW2*x)

 amplitudes(13) = 0.D0

 amplitudes(14) = 0.D0

 amplitudes(15) = 0.D0

  totalAmplitude = (0D0,0D0)
 do j=1,15
  totalAmplitude = totalAmplitude + amplitudes(j)
 end do
 DSelfBBRight = totalAmplitude
end function DSelfBBRight

