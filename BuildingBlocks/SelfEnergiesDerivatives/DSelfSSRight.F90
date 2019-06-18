double complex function DSelfSSRight(x)
 use constants
 implicit none
#include "looptools.h"
 double precision, intent(in) :: x
 integer :: j
 double complex :: totalAmplitude
 double complex :: amplitudes(15)

 amplitudes(1) = (0.0078125D0*EL2*MS2*(B0(x, Mh02, MS2) - 1.D0*Mh02*DB0(x, Mh02, MS2) + MS2*DB0(x, Mh02, MS2) + x*DB0(x, Mh02, MS&
  &2))*DBLE(Yuk1**INT(2.D0)))/ (MW2*PI2*SW2*x) - (0.0078125D0*EL2*MS2*(A0(Mh02) - 1.D0*A0(MS2) - 1.D0*Mh02*B0(x, Mh02, MS2) + MS2&
  &*B0(x, Mh02, MS2) + x*B0(x, Mh02, MS2))* DBLE(x**INT(-2.D0))*DBLE(Yuk1**INT(2.D0)))/(MW2*PI2*SW2)

 amplitudes(2) = (0.0078125D0*EL2*MS2*(B0(x, MHH2, MS2) - 1.D0*MHH2*DB0(x, MHH2, MS2) + MS2*DB0(x, MHH2, MS2) + x*DB0(x, MHH2, MS&
  &2))*DBLE(Yuk2**INT(2.D0)))/ (MW2*PI2*SW2*x) - (0.0078125D0*EL2*MS2*(A0(MHH2) - 1.D0*A0(MS2) - 1.D0*MHH2*B0(x, MHH2, MS2) + MS2&
  &*B0(x, MHH2, MS2) + x*B0(x, MHH2, MS2))* DBLE(x**INT(-2.D0))*DBLE(Yuk2**INT(2.D0)))/(MW2*PI2*SW2)

 amplitudes(3) = (0.0078125D0*EL2*MS2*(B0(x, MA02, MS2) - 1.D0*MA02*DB0(x, MA02, MS2) + MS2*DB0(x, MA02, MS2) + x*DB0(x, MA02, MS&
  &2))*DBLE(Yuk3**INT(2.D0)))/ (MW2*PI2*SW2*x) - (0.0078125D0*EL2*MS2*(A0(MA02) - 1.D0*A0(MS2) - 1.D0*MA02*B0(x, MA02, MS2) + MS2&
  &*B0(x, MA02, MS2) + x*B0(x, MA02, MS2))* DBLE(x**INT(-2.D0))*DBLE(Yuk3**INT(2.D0)))/(MW2*PI2*SW2)

 amplitudes(4) = (0.0078125D0*EL2*MS2*(B0(x, MS2, GaugeXiZ*MZ2) + MS2*DB0(x, MS2, GaugeXiZ*MZ2) - 1.D0*GaugeXiZ*MZ2*DB0(x, MS2, G&
  &augeXiZ*MZ2) + x*DB0(x, MS2, GaugeXiZ*MZ2)))/(MW2*PI2*SW2*x) - (0.0078125D0*EL2*MS2*(-1.D0*A0(MS2) + A0(GaugeXiZ*MZ2) + MS2*B0&
  &(x, MS2, GaugeXiZ*MZ2) - 1.D0*GaugeXiZ*MZ2*B0(x, MS2, GaugeXiZ*MZ2) + x*B0(x, MS2, GaugeXiZ*MZ2))*DBLE(x**INT(-2.D0)))/(MW2*PI&
  &2*SW2)

 amplitudes(5) = (-0.015625D0*CKM12*CKMC12*EL2*DBLE(x**INT(-2.D0))*(MS2*TB2*A0(MHp2)*DBLE(Yuk3**INT(2.D0)) - 1.D0*MS2*TB2*A0(MU2)&
  &*DBLE(Yuk3**INT(2.D0)) - 1.D0*MHp2*MS2*TB2*B0(x, MHp2, MU2)*DBLE(Yuk3**INT(2.D0)) + MS2*MU2*TB2*B0(x, MHp2, MU2)*DBLE(Yuk3**IN&
  &T(2.D0)) + MS2*TB2*x*B0(x, MHp2, MU2)*DBLE(Yuk3**INT(2.D0))))/(MW2*PI2*SW2*TB2) + (0.015625D0*CKM12*CKMC12*EL2*(MS2*TB2*B0(x, &
  &MHp2, MU2)*DBLE(Yuk3**INT(2.D0)) - 1.D0*MHp2*MS2*TB2*DB0(x, MHp2, MU2)*DBLE(Yuk3**INT(2.D0)) + MS2*MU2*TB2*DB0(x, MHp2, MU2)*D&
  &BLE(Yuk3**INT(2.D0)) + MS2*TB2*x*DB0(x, MHp2, MU2)*DBLE(Yuk3**INT(2.D0))))/(MW2*PI2*SW2*TB2*x)

 amplitudes(6) = (-0.015625D0*CKM22*CKMC22*EL2*DBLE(x**INT(-2.D0))*(-1.D0*MS2*TB2*A0(MC2)*DBLE(Yuk3**INT(2.D0)) + MS2*TB2*A0(MHp2&
  &)*DBLE(Yuk3**INT(2.D0)) + MC2*MS2*TB2*B0(x, MC2, MHp2)*DBLE(Yuk3**INT(2.D0)) - 1.D0*MHp2*MS2*TB2*B0(x, MC2, MHp2)*DBLE(Yuk3**I&
  &NT(2.D0)) + MS2*TB2*x*B0(x, MC2, MHp2)*DBLE(Yuk3**INT(2.D0))))/(MW2*PI2*SW2*TB2) + (0.015625D0*CKM22*CKMC22*EL2*(MS2*TB2*B0(x,&
  & MC2, MHp2)*DBLE(Yuk3**INT(2.D0)) + MC2*MS2*TB2*DB0(x, MC2, MHp2)*DBLE(Yuk3**INT(2.D0)) - 1.D0*MHp2*MS2*TB2*DB0(x, MC2, MHp2)*&
  &DBLE(Yuk3**INT(2.D0)) + MS2*TB2*x*DB0(x, MC2, MHp2)*DBLE(Yuk3**INT(2.D0))))/(MW2*PI2*SW2*TB2*x)

 amplitudes(7) = (-0.015625D0*CKM32*CKMC32*EL2*DBLE(x**INT(-2.D0))*(MS2*TB2*A0(MHp2)*DBLE(Yuk3**INT(2.D0)) - 1.D0*MS2*TB2*A0(MT2)&
  &*DBLE(Yuk3**INT(2.D0)) - 1.D0*MHp2*MS2*TB2*B0(x, MHp2, MT2)*DBLE(Yuk3**INT(2.D0)) + MS2*MT2*TB2*B0(x, MHp2, MT2)*DBLE(Yuk3**IN&
  &T(2.D0)) + MS2*TB2*x*B0(x, MHp2, MT2)*DBLE(Yuk3**INT(2.D0))))/(MW2*PI2*SW2*TB2) + (0.015625D0*CKM32*CKMC32*EL2*(MS2*TB2*B0(x, &
  &MHp2, MT2)*DBLE(Yuk3**INT(2.D0)) - 1.D0*MHp2*MS2*TB2*DB0(x, MHp2, MT2)*DBLE(Yuk3**INT(2.D0)) + MS2*MT2*TB2*DB0(x, MHp2, MT2)*D&
  &BLE(Yuk3**INT(2.D0)) + MS2*TB2*x*DB0(x, MHp2, MT2)*DBLE(Yuk3**INT(2.D0))))/(MW2*PI2*SW2*TB2*x)

 amplitudes(8) = (0.015625D0*CKM12*CKMC12*EL2*(MS2*B0(x, MU2, GaugeXiW*MW2) + MS2*MU2*DB0(x, MU2, GaugeXiW*MW2) - 1.D0*GaugeXiW*M&
  &S2*MW2*DB0(x, MU2, GaugeXiW*MW2) + MS2*x*DB0(x, MU2, GaugeXiW*MW2)))/(MW2*PI2*SW2*x) - (0.015625D0*CKM12*CKMC12*EL2*(-1.D0*MS2&
  &*A0(MU2) + MS2*A0(GaugeXiW*MW2) + MS2*MU2*B0(x, MU2, GaugeXiW*MW2) - 1.D0*GaugeXiW*MS2*MW2*B0(x, MU2, GaugeXiW*MW2) + MS2*x*B0&
  &(x, MU2, GaugeXiW*MW2))*DBLE(x**INT(-2.D0)))/(MW2*PI2*SW2)

 amplitudes(9) = (0.015625D0*CKM22*CKMC22*EL2*(MS2*B0(x, MC2, GaugeXiW*MW2) + MC2*MS2*DB0(x, MC2, GaugeXiW*MW2) - 1.D0*GaugeXiW*M&
  &S2*MW2*DB0(x, MC2, GaugeXiW*MW2) + MS2*x*DB0(x, MC2, GaugeXiW*MW2)))/(MW2*PI2*SW2*x) - (0.015625D0*CKM22*CKMC22*EL2*(-1.D0*MS2&
  &*A0(MC2) + MS2*A0(GaugeXiW*MW2) + MC2*MS2*B0(x, MC2, GaugeXiW*MW2) - 1.D0*GaugeXiW*MS2*MW2*B0(x, MC2, GaugeXiW*MW2) + MS2*x*B0&
  &(x, MC2, GaugeXiW*MW2))*DBLE(x**INT(-2.D0)))/(MW2*PI2*SW2)

 amplitudes(10) = (0.015625D0*CKM32*CKMC32*EL2*(MS2*B0(x, MT2, GaugeXiW*MW2) + MS2*MT2*DB0(x, MT2, GaugeXiW*MW2) - 1.D0*GaugeXiW*&
  &MS2*MW2*DB0(x, MT2, GaugeXiW*MW2) + MS2*x*DB0(x, MT2, GaugeXiW*MW2)))/(MW2*PI2*SW2*x) - (0.015625D0*CKM32*CKMC32*EL2*(-1.D0*MS&
  &2*A0(MT2) + MS2*A0(GaugeXiW*MW2) + MS2*MT2*B0(x, MT2, GaugeXiW*MW2) - 1.D0*GaugeXiW*MS2*MW2*B0(x, MT2, GaugeXiW*MW2) + MS2*x*B&
  &0(x, MT2, GaugeXiW*MW2))*DBLE(x**INT(-2.D0)))/(MW2*PI2*SW2)

 amplitudes(11) = (-0.003472222222222222D0*EL2*DBLE(x**INT(-2.D0))*(-2.D0*x - 2.D0*A0(MS2) + MS2*B0(x, 0.D0, MS2) + GaugeXiA*MS2*&
  &B0(x, 0.D0, MS2) + x*B0(x, 0.D0, MS2) + GaugeXiA*x*B0(x, 0.D0, MS2) - 2.D0*MS2*x*C0Mine(DBLE(0.D0), DBLE(x), DBLE(x), DBLE(0.D&
  &0), DBLE(0.D0), DBLE(MS2)) + 2.D0*GaugeXiA*MS2*x*C0Mine(DBLE(0.D0), DBLE(x), DBLE(x), DBLE(0.D0), DBLE(0.D0), DBLE(MS2)) + C0M&
  &ine(DBLE(0.D0), DBLE(x), DBLE(x), DBLE(0.D0), DBLE(0.D0), DBLE(MS2))*DBLE(MS**INT(4.D0)) - 1.D0*GaugeXiA*C0Mine(DBLE(0.D0), DB&
  &LE(x), DBLE(x), DBLE(0.D0), DBLE(0.D0), DBLE(MS2))*DBLE(MS**INT(4.D0)) + C0Mine(DBLE(0.D0), DBLE(x), DBLE(x), DBLE(0.D0), DBLE&
  &(0.D0), DBLE(MS2))*DBLE(x**INT(2.D0)) - 1.D0*GaugeXiA*C0Mine(DBLE(0.D0), DBLE(x), DBLE(x), DBLE(0.D0), DBLE(0.D0), DBLE(MS2))*&
  &DBLE(x**INT(2.D0))))/PI2 + (0.003472222222222222D0*EL2*(-2.D0 + B0(x, 0.D0, MS2) + GaugeXiA*B0(x, 0.D0, MS2) - 2.D0*MS2*C0Mine&
  &(DBLE(0.D0), DBLE(x), DBLE(x), DBLE(0.D0), DBLE(0.D0), DBLE(MS2)) + 2.D0*GaugeXiA*MS2*C0Mine(DBLE(0.D0), DBLE(x), DBLE(x), DBL&
  &E(0.D0), DBLE(0.D0), DBLE(MS2)) + 2.D0*x*C0Mine(DBLE(0.D0), DBLE(x), DBLE(x), DBLE(0.D0), DBLE(0.D0), DBLE(MS2)) - 2.D0*GaugeX&
  &iA*x*C0Mine(DBLE(0.D0), DBLE(x), DBLE(x), DBLE(0.D0), DBLE(0.D0), DBLE(MS2)) + MS2*DB0(x, 0.D0, MS2) + GaugeXiA*MS2*DB0(x, 0.D&
  &0, MS2) + x*DB0(x, 0.D0, MS2) + GaugeXiA*x*DB0(x, 0.D0, MS2) - 2.D0*MS2*x*(DC01Mine(DBLE(0.D0), DBLE(x), DBLE(x), DBLE(0.D0), &
  &DBLE(0.D0), DBLE(MS2)) + DC02Mine(DBLE(0.D0), DBLE(x), DBLE(x), DBLE(0.D0), DBLE(0.D0), DBLE(MS2))) + 2.D0*GaugeXiA*MS2*x*(DC0&
  &1Mine(DBLE(0.D0), DBLE(x), DBLE(x), DBLE(0.D0), DBLE(0.D0), DBLE(MS2)) + DC02Mine(DBLE(0.D0), DBLE(x), DBLE(x), DBLE(0.D0), DB&
  &LE(0.D0), DBLE(MS2))) + DBLE(MS**INT(4.D0))*(DC01Mine(DBLE(0.D0), DBLE(x), DBLE(x), DBLE(0.D0), DBLE(0.D0), DBLE(MS2)) + DC02M&
  &ine(DBLE(0.D0), DBLE(x), DBLE(x), DBLE(0.D0), DBLE(0.D0), DBLE(MS2))) - 1.D0*GaugeXiA*DBLE(MS**INT(4.D0))* (DC01Mine(DBLE(0.D0&
  &), DBLE(x), DBLE(x), DBLE(0.D0), DBLE(0.D0), DBLE(MS2)) + DC02Mine(DBLE(0.D0), DBLE(x), DBLE(x), DBLE(0.D0), DBLE(0.D0), DBLE(&
  &MS2))) + DBLE(x**INT(2.D0))*(DC01Mine(DBLE(0.D0), DBLE(x), DBLE(x), DBLE(0.D0), DBLE(0.D0), DBLE(MS2)) + DC02Mine(DBLE(0.D0), &
  &DBLE(x), DBLE(x), DBLE(0.D0), DBLE(0.D0), DBLE(MS2))) - 1.D0*GaugeXiA*DBLE(x**INT(2.D0))* (DC01Mine(DBLE(0.D0), DBLE(x), DBLE(&
  &x), DBLE(0.D0), DBLE(0.D0), DBLE(MS2)) + DC02Mine(DBLE(0.D0), DBLE(x), DBLE(x), DBLE(0.D0), DBLE(0.D0), DBLE(MS2)))))/(PI2*x)

 amplitudes(12) = (-0.0008680555555555555D0*EL2*DBLE(x**INT(-2.D0))*(-8.D0*MZ2*x*DBLE(SW**INT(4.D0)) - 8.D0*MZ2*A0(MS2)*DBLE(SW**&
  &INT(4.D0)) + 4.D0*MS2*A0(MZ2)*DBLE(SW**INT(4.D0)) + 8.D0*MZ2*A0(MZ2)*DBLE(SW**INT(4.D0)) - 4.D0*x*A0(MZ2)*DBLE(SW**INT(4.D0)) &
  &- 4.D0*MS2*A0(GaugeXiZ*MZ2)*DBLE(SW**INT(4.D0)) + 4.D0*x*A0(GaugeXiZ*MZ2)*DBLE(SW**INT(4.D0)) + 4.D0*MS2*MZ2*B0(x, MS2, MZ2)*D&
  &BLE(SW**INT(4.D0)) - 8.D0*MS2*x*B0(x, MS2, MZ2)*DBLE(SW**INT(4.D0)) + 4.D0*MZ2*x*B0(x, MS2, MZ2)*DBLE(SW**INT(4.D0)) + 4.D0*Ga&
  &ugeXiZ*MS2*MZ2*B0(x, MS2, GaugeXiZ*MZ2)*DBLE(SW**INT(4.D0)) + 8.D0*MS2*x*B0(x, MS2, GaugeXiZ*MZ2)*DBLE(SW**INT(4.D0)) + 4.D0*G&
  &augeXiZ*MZ2*x*B0(x, MS2, GaugeXiZ*MZ2)*DBLE(SW**INT(4.D0)) + 4.D0*B0(x, MS2, MZ2)*DBLE(MS**INT(4.D0))*DBLE(SW**INT(4.D0)) - 4.&
  &D0*B0(x, MS2, GaugeXiZ*MZ2)*DBLE(MS**INT(4.D0))*DBLE(SW**INT(4.D0)) - 8.D0*B0(x, MS2, MZ2)*DBLE(MZ**INT(4.D0))*DBLE(SW**INT(4.&
  &D0)) + 4.D0*B0(x, MS2, MZ2)*DBLE(SW**INT(4.D0))*DBLE(x**INT(2.D0)) - 4.D0*B0(x, MS2, GaugeXiZ*MZ2)*DBLE(SW**INT(4.D0))*DBLE(x*&
  &*INT(2.D0))))/ (CW2*MZ2*PI2*SW2) + (0.0008680555555555555D0*EL2*(-8.D0*MZ2*DBLE(SW**INT(4.D0)) - 4.D0*A0(MZ2)*DBLE(SW**INT(4.D&
  &0)) + 4.D0*A0(GaugeXiZ*MZ2)*DBLE(SW**INT(4.D0)) - 8.D0*MS2*B0(x, MS2, MZ2)*DBLE(SW**INT(4.D0)) + 4.D0*MZ2*B0(x, MS2, MZ2)*DBLE&
  &(SW**INT(4.D0)) + 8.D0*x*B0(x, MS2, MZ2)*DBLE(SW**INT(4.D0)) + 8.D0*MS2*B0(x, MS2, GaugeXiZ*MZ2)*DBLE(SW**INT(4.D0)) + 4.D0*Ga&
  &ugeXiZ*MZ2*B0(x, MS2, GaugeXiZ*MZ2)*DBLE(SW**INT(4.D0)) - 8.D0*x*B0(x, MS2, GaugeXiZ*MZ2)*DBLE(SW**INT(4.D0)) + 4.D0*MS2*MZ2*D&
  &B0(x, MS2, MZ2)*DBLE(SW**INT(4.D0)) - 8.D0*MS2*x*DB0(x, MS2, MZ2)*DBLE(SW**INT(4.D0)) + 4.D0*MZ2*x*DB0(x, MS2, MZ2)*DBLE(SW**I&
  &NT(4.D0)) + 4.D0*GaugeXiZ*MS2*MZ2*DB0(x, MS2, GaugeXiZ*MZ2)*DBLE(SW**INT(4.D0)) + 8.D0*MS2*x*DB0(x, MS2, GaugeXiZ*MZ2)*DBLE(SW&
  &**INT(4.D0)) + 4.D0*GaugeXiZ*MZ2*x*DB0(x, MS2, GaugeXiZ*MZ2)*DBLE(SW**INT(4.D0)) + 4.D0*DB0(x, MS2, MZ2)*DBLE(MS**INT(4.D0))*D&
  &BLE(SW**INT(4.D0)) - 4.D0*DB0(x, MS2, GaugeXiZ*MZ2)*DBLE(MS**INT(4.D0))*DBLE(SW**INT(4.D0)) - 8.D0*DB0(x, MS2, MZ2)*DBLE(MZ**I&
  &NT(4.D0))*DBLE(SW**INT(4.D0)) + 4.D0*DB0(x, MS2, MZ2)*DBLE(SW**INT(4.D0))*DBLE(x**INT(2.D0)) - 4.D0*DB0(x, MS2, GaugeXiZ*MZ2)*&
  &DBLE(SW**INT(4.D0))*DBLE(x**INT(2.D0))))/(CW2*MZ2*PI2*SW2*x)

 amplitudes(13) = 0.D0

 amplitudes(14) = 0.D0

 amplitudes(15) = 0.D0

  totalAmplitude = (0D0,0D0)
 do j=1,15
  totalAmplitude = totalAmplitude + amplitudes(j)
 end do
 DSelfSSRight = totalAmplitude
end function DSelfSSRight

