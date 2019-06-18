double complex function DSelfElElRight(x)
 use constants
 implicit none
#include "looptools.h"
 double precision, intent(in) :: x
 integer :: j
 double complex :: totalAmplitude
 double complex :: amplitudes(9)

 amplitudes(1) = (0.0078125D0*EL2*ME2*(B0(x, ME2, Mh02) + ME2*DB0(x, ME2, Mh02) - 1.D0*Mh02*DB0(x, ME2, Mh02) + x*DB0(x, ME2, Mh0&
  &2))*DBLE(Yuk4**INT(2.D0)))/ (MW2*PI2*SW2*x) - (0.0078125D0*EL2*ME2*(-1.D0*A0(ME2) + A0(Mh02) + ME2*B0(x, ME2, Mh02) - 1.D0*Mh0&
  &2*B0(x, ME2, Mh02) + x*B0(x, ME2, Mh02))* DBLE(x**INT(-2.D0))*DBLE(Yuk4**INT(2.D0)))/(MW2*PI2*SW2)

 amplitudes(2) = (0.0078125D0*EL2*ME2*(B0(x, ME2, MHH2) + ME2*DB0(x, ME2, MHH2) - 1.D0*MHH2*DB0(x, ME2, MHH2) + x*DB0(x, ME2, MHH&
  &2))*DBLE(Yuk5**INT(2.D0)))/ (MW2*PI2*SW2*x) - (0.0078125D0*EL2*ME2*(-1.D0*A0(ME2) + A0(MHH2) + ME2*B0(x, ME2, MHH2) - 1.D0*MHH&
  &2*B0(x, ME2, MHH2) + x*B0(x, ME2, MHH2))* DBLE(x**INT(-2.D0))*DBLE(Yuk5**INT(2.D0)))/(MW2*PI2*SW2)

 amplitudes(3) = (0.0078125D0*EL2*ME2*(B0(x, MA02, ME2) - 1.D0*MA02*DB0(x, MA02, ME2) + ME2*DB0(x, MA02, ME2) + x*DB0(x, MA02, ME&
  &2))*DBLE(Yuk6**INT(2.D0)))/ (MW2*PI2*SW2*x) - (0.0078125D0*EL2*ME2*(A0(MA02) - 1.D0*A0(ME2) - 1.D0*MA02*B0(x, MA02, ME2) + ME2&
  &*B0(x, MA02, ME2) + x*B0(x, MA02, ME2))* DBLE(x**INT(-2.D0))*DBLE(Yuk6**INT(2.D0)))/(MW2*PI2*SW2)

 amplitudes(4) = (0.0078125D0*EL2*ME2*(B0(x, ME2, GaugeXiZ*MZ2) + ME2*DB0(x, ME2, GaugeXiZ*MZ2) - 1.D0*GaugeXiZ*MZ2*DB0(x, ME2, G&
  &augeXiZ*MZ2) + x*DB0(x, ME2, GaugeXiZ*MZ2)))/(MW2*PI2*SW2*x) - (0.0078125D0*EL2*ME2*(-1.D0*A0(ME2) + A0(GaugeXiZ*MZ2) + ME2*B0&
  &(x, ME2, GaugeXiZ*MZ2) - 1.D0*GaugeXiZ*MZ2*B0(x, ME2, GaugeXiZ*MZ2) + x*B0(x, ME2, GaugeXiZ*MZ2))*DBLE(x**INT(-2.D0)))/(MW2*PI&
  &2*SW2)

 amplitudes(5) = (0.015625D0*EL2*ME2*(B0(x, 0.D0, MHp2) + (-1.D0*MHp2 + x)*DB0(x, 0.D0, MHp2))*DBLE(Yuk6**INT(2.D0)))/(MW2*PI2*SW&
  &2*x) - (0.015625D0*EL2*ME2*(A0(MHp2) + (-1.D0*MHp2 + x)*B0(x, 0.D0, MHp2))*DBLE(x**INT(-2.D0))*DBLE(Yuk6**INT(2.D0)))/(MW2*PI2&
  &*SW2)

 amplitudes(6) = (0.015625D0*EL2*ME2*(B0(x, 0.D0, GaugeXiW*MW2) + (-1.D0*GaugeXiW*MW2 + x)*DB0(x, 0.D0, GaugeXiW*MW2)))/(MW2*PI2*&
  &SW2*x) - (0.015625D0*EL2*ME2*(A0(GaugeXiW*MW2) + (-1.D0*GaugeXiW*MW2 + x)*B0(x, 0.D0, GaugeXiW*MW2))*DBLE(x**INT(-2.D0)))/(MW2&
  &*PI2*SW2)

 amplitudes(7) = (-0.03125D0*EL2*DBLE(x**INT(-2.D0))*(-2.D0*x - 2.D0*A0(ME2) + ME2*B0(x, 0.D0, ME2) + GaugeXiA*ME2*B0(x, 0.D0, ME&
  &2) + x*B0(x, 0.D0, ME2) + GaugeXiA*x*B0(x, 0.D0, ME2) - 2.D0*ME2*x*C0Mine(DBLE(0.D0), DBLE(x), DBLE(x), DBLE(0.D0), DBLE(0.D0)&
  &, DBLE(ME2)) + 2.D0*GaugeXiA*ME2*x*C0Mine(DBLE(0.D0), DBLE(x), DBLE(x), DBLE(0.D0), DBLE(0.D0), DBLE(ME2)) + C0Mine(DBLE(0.D0)&
  &, DBLE(x), DBLE(x), DBLE(0.D0), DBLE(0.D0), DBLE(ME2))*DBLE(ME**INT(4.D0)) - 1.D0*GaugeXiA*C0Mine(DBLE(0.D0), DBLE(x), DBLE(x)&
  &, DBLE(0.D0), DBLE(0.D0), DBLE(ME2))*DBLE(ME**INT(4.D0)) + C0Mine(DBLE(0.D0), DBLE(x), DBLE(x), DBLE(0.D0), DBLE(0.D0), DBLE(M&
  &E2))*DBLE(x**INT(2.D0)) - 1.D0*GaugeXiA*C0Mine(DBLE(0.D0), DBLE(x), DBLE(x), DBLE(0.D0), DBLE(0.D0), DBLE(ME2))*DBLE(x**INT(2.&
  &D0))))/PI2 + (0.03125D0*EL2*(-2.D0 + B0(x, 0.D0, ME2) + GaugeXiA*B0(x, 0.D0, ME2) - 2.D0*ME2*C0Mine(DBLE(0.D0), DBLE(x), DBLE(&
  &x), DBLE(0.D0), DBLE(0.D0), DBLE(ME2)) + 2.D0*GaugeXiA*ME2*C0Mine(DBLE(0.D0), DBLE(x), DBLE(x), DBLE(0.D0), DBLE(0.D0), DBLE(M&
  &E2)) + 2.D0*x*C0Mine(DBLE(0.D0), DBLE(x), DBLE(x), DBLE(0.D0), DBLE(0.D0), DBLE(ME2)) - 2.D0*GaugeXiA*x*C0Mine(DBLE(0.D0), DBL&
  &E(x), DBLE(x), DBLE(0.D0), DBLE(0.D0), DBLE(ME2)) + ME2*DB0(x, 0.D0, ME2) + GaugeXiA*ME2*DB0(x, 0.D0, ME2) + x*DB0(x, 0.D0, ME&
  &2) + GaugeXiA*x*DB0(x, 0.D0, ME2) - 2.D0*ME2*x*(DC01Mine(DBLE(0.D0), DBLE(x), DBLE(x), DBLE(0.D0), DBLE(0.D0), DBLE(ME2)) + DC&
  &02Mine(DBLE(0.D0), DBLE(x), DBLE(x), DBLE(0.D0), DBLE(0.D0), DBLE(ME2))) + 2.D0*GaugeXiA*ME2*x*(DC01Mine(DBLE(0.D0), DBLE(x), &
  &DBLE(x), DBLE(0.D0), DBLE(0.D0), DBLE(ME2)) + DC02Mine(DBLE(0.D0), DBLE(x), DBLE(x), DBLE(0.D0), DBLE(0.D0), DBLE(ME2))) + DBL&
  &E(ME**INT(4.D0))*(DC01Mine(DBLE(0.D0), DBLE(x), DBLE(x), DBLE(0.D0), DBLE(0.D0), DBLE(ME2)) + DC02Mine(DBLE(0.D0), DBLE(x), DB&
  &LE(x), DBLE(0.D0), DBLE(0.D0), DBLE(ME2))) - 1.D0*GaugeXiA*DBLE(ME**INT(4.D0))* (DC01Mine(DBLE(0.D0), DBLE(x), DBLE(x), DBLE(0&
  &.D0), DBLE(0.D0), DBLE(ME2)) + DC02Mine(DBLE(0.D0), DBLE(x), DBLE(x), DBLE(0.D0), DBLE(0.D0), DBLE(ME2))) + DBLE(x**INT(2.D0))&
  &*(DC01Mine(DBLE(0.D0), DBLE(x), DBLE(x), DBLE(0.D0), DBLE(0.D0), DBLE(ME2)) + DC02Mine(DBLE(0.D0), DBLE(x), DBLE(x), DBLE(0.D0&
  &), DBLE(0.D0), DBLE(ME2))) - 1.D0*GaugeXiA*DBLE(x**INT(2.D0))* (DC01Mine(DBLE(0.D0), DBLE(x), DBLE(x), DBLE(0.D0), DBLE(0.D0),&
  & DBLE(ME2)) + DC02Mine(DBLE(0.D0), DBLE(x), DBLE(x), DBLE(0.D0), DBLE(0.D0), DBLE(ME2)))))/(PI2*x)

 amplitudes(8) = (-0.0078125D0*EL2*DBLE(x**INT(-2.D0))*(-8.D0*MZ2*x*DBLE(SW**INT(4.D0)) - 8.D0*MZ2*A0(ME2)*DBLE(SW**INT(4.D0)) + &
  &4.D0*ME2*A0(MZ2)*DBLE(SW**INT(4.D0)) + 8.D0*MZ2*A0(MZ2)*DBLE(SW**INT(4.D0)) - 4.D0*x*A0(MZ2)*DBLE(SW**INT(4.D0)) - 4.D0*ME2*A0&
  &(GaugeXiZ*MZ2)*DBLE(SW**INT(4.D0)) + 4.D0*x*A0(GaugeXiZ*MZ2)*DBLE(SW**INT(4.D0)) + 4.D0*ME2*MZ2*B0(x, ME2, MZ2)*DBLE(SW**INT(4&
  &.D0)) - 8.D0*ME2*x*B0(x, ME2, MZ2)*DBLE(SW**INT(4.D0)) + 4.D0*MZ2*x*B0(x, ME2, MZ2)*DBLE(SW**INT(4.D0)) + 4.D0*GaugeXiZ*ME2*MZ&
  &2*B0(x, ME2, GaugeXiZ*MZ2)*DBLE(SW**INT(4.D0)) + 8.D0*ME2*x*B0(x, ME2, GaugeXiZ*MZ2)*DBLE(SW**INT(4.D0)) + 4.D0*GaugeXiZ*MZ2*x&
  &*B0(x, ME2, GaugeXiZ*MZ2)*DBLE(SW**INT(4.D0)) + 4.D0*B0(x, ME2, MZ2)*DBLE(ME**INT(4.D0))*DBLE(SW**INT(4.D0)) - 4.D0*B0(x, ME2,&
  & GaugeXiZ*MZ2)*DBLE(ME**INT(4.D0))*DBLE(SW**INT(4.D0)) - 8.D0*B0(x, ME2, MZ2)*DBLE(MZ**INT(4.D0))*DBLE(SW**INT(4.D0)) + 4.D0*B&
  &0(x, ME2, MZ2)*DBLE(SW**INT(4.D0))*DBLE(x**INT(2.D0)) - 4.D0*B0(x, ME2, GaugeXiZ*MZ2)*DBLE(SW**INT(4.D0))*DBLE(x**INT(2.D0))))&
  &/ (CW2*MZ2*PI2*SW2) + (0.0078125D0*EL2*(-8.D0*MZ2*DBLE(SW**INT(4.D0)) - 4.D0*A0(MZ2)*DBLE(SW**INT(4.D0)) + 4.D0*A0(GaugeXiZ*MZ&
  &2)*DBLE(SW**INT(4.D0)) - 8.D0*ME2*B0(x, ME2, MZ2)*DBLE(SW**INT(4.D0)) + 4.D0*MZ2*B0(x, ME2, MZ2)*DBLE(SW**INT(4.D0)) + 8.D0*x*&
  &B0(x, ME2, MZ2)*DBLE(SW**INT(4.D0)) + 8.D0*ME2*B0(x, ME2, GaugeXiZ*MZ2)*DBLE(SW**INT(4.D0)) + 4.D0*GaugeXiZ*MZ2*B0(x, ME2, Gau&
  &geXiZ*MZ2)*DBLE(SW**INT(4.D0)) - 8.D0*x*B0(x, ME2, GaugeXiZ*MZ2)*DBLE(SW**INT(4.D0)) + 4.D0*ME2*MZ2*DB0(x, ME2, MZ2)*DBLE(SW**&
  &INT(4.D0)) - 8.D0*ME2*x*DB0(x, ME2, MZ2)*DBLE(SW**INT(4.D0)) + 4.D0*MZ2*x*DB0(x, ME2, MZ2)*DBLE(SW**INT(4.D0)) + 4.D0*GaugeXiZ&
  &*ME2*MZ2*DB0(x, ME2, GaugeXiZ*MZ2)*DBLE(SW**INT(4.D0)) + 8.D0*ME2*x*DB0(x, ME2, GaugeXiZ*MZ2)*DBLE(SW**INT(4.D0)) + 4.D0*Gauge&
  &XiZ*MZ2*x*DB0(x, ME2, GaugeXiZ*MZ2)*DBLE(SW**INT(4.D0)) + 4.D0*DB0(x, ME2, MZ2)*DBLE(ME**INT(4.D0))*DBLE(SW**INT(4.D0)) - 4.D0&
  &*DB0(x, ME2, GaugeXiZ*MZ2)*DBLE(ME**INT(4.D0))*DBLE(SW**INT(4.D0)) - 8.D0*DB0(x, ME2, MZ2)*DBLE(MZ**INT(4.D0))*DBLE(SW**INT(4.&
  &D0)) + 4.D0*DB0(x, ME2, MZ2)*DBLE(SW**INT(4.D0))*DBLE(x**INT(2.D0)) - 4.D0*DB0(x, ME2, GaugeXiZ*MZ2)*DBLE(SW**INT(4.D0))*DBLE(&
  &x**INT(2.D0))))/ (CW2*MZ2*PI2*SW2*x)

 amplitudes(9) = 0.D0

  totalAmplitude = (0D0,0D0)
 do j=1,9
  totalAmplitude = totalAmplitude + amplitudes(j)
 end do
 DSelfElElRight = totalAmplitude
end function DSelfElElRight

