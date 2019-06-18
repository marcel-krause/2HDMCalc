double complex function DSelfTauTauRight(x)
 use constants
 implicit none
#include "looptools.h"
 double precision, intent(in) :: x
 integer :: j
 double complex :: totalAmplitude
 double complex :: amplitudes(9)

 amplitudes(1) = (0.0078125D0*EL2*ML2*(B0(x, Mh02, ML2) - 1.D0*Mh02*DB0(x, Mh02, ML2) + ML2*DB0(x, Mh02, ML2) + x*DB0(x, Mh02, ML&
  &2))*DBLE(Yuk4**INT(2.D0)))/ (MW2*PI2*SW2*x) - (0.0078125D0*EL2*ML2*(A0(Mh02) - 1.D0*A0(ML2) - 1.D0*Mh02*B0(x, Mh02, ML2) + ML2&
  &*B0(x, Mh02, ML2) + x*B0(x, Mh02, ML2))* DBLE(x**INT(-2.D0))*DBLE(Yuk4**INT(2.D0)))/(MW2*PI2*SW2)

 amplitudes(2) = (0.0078125D0*EL2*ML2*(B0(x, MHH2, ML2) - 1.D0*MHH2*DB0(x, MHH2, ML2) + ML2*DB0(x, MHH2, ML2) + x*DB0(x, MHH2, ML&
  &2))*DBLE(Yuk5**INT(2.D0)))/ (MW2*PI2*SW2*x) - (0.0078125D0*EL2*ML2*(A0(MHH2) - 1.D0*A0(ML2) - 1.D0*MHH2*B0(x, MHH2, ML2) + ML2&
  &*B0(x, MHH2, ML2) + x*B0(x, MHH2, ML2))* DBLE(x**INT(-2.D0))*DBLE(Yuk5**INT(2.D0)))/(MW2*PI2*SW2)

 amplitudes(3) = (0.0078125D0*EL2*ML2*(B0(x, MA02, ML2) - 1.D0*MA02*DB0(x, MA02, ML2) + ML2*DB0(x, MA02, ML2) + x*DB0(x, MA02, ML&
  &2))*DBLE(Yuk6**INT(2.D0)))/ (MW2*PI2*SW2*x) - (0.0078125D0*EL2*ML2*(A0(MA02) - 1.D0*A0(ML2) - 1.D0*MA02*B0(x, MA02, ML2) + ML2&
  &*B0(x, MA02, ML2) + x*B0(x, MA02, ML2))* DBLE(x**INT(-2.D0))*DBLE(Yuk6**INT(2.D0)))/(MW2*PI2*SW2)

 amplitudes(4) = (0.0078125D0*EL2*ML2*(B0(x, ML2, GaugeXiZ*MZ2) + ML2*DB0(x, ML2, GaugeXiZ*MZ2) - 1.D0*GaugeXiZ*MZ2*DB0(x, ML2, G&
  &augeXiZ*MZ2) + x*DB0(x, ML2, GaugeXiZ*MZ2)))/(MW2*PI2*SW2*x) - (0.0078125D0*EL2*ML2*(-1.D0*A0(ML2) + A0(GaugeXiZ*MZ2) + ML2*B0&
  &(x, ML2, GaugeXiZ*MZ2) - 1.D0*GaugeXiZ*MZ2*B0(x, ML2, GaugeXiZ*MZ2) + x*B0(x, ML2, GaugeXiZ*MZ2))*DBLE(x**INT(-2.D0)))/(MW2*PI&
  &2*SW2)

 amplitudes(5) = (0.015625D0*EL2*ML2*(B0(x, 0.D0, MHp2) + (-1.D0*MHp2 + x)*DB0(x, 0.D0, MHp2))*DBLE(Yuk6**INT(2.D0)))/(MW2*PI2*SW&
  &2*x) - (0.015625D0*EL2*ML2*(A0(MHp2) + (-1.D0*MHp2 + x)*B0(x, 0.D0, MHp2))*DBLE(x**INT(-2.D0))*DBLE(Yuk6**INT(2.D0)))/(MW2*PI2&
  &*SW2)

 amplitudes(6) = (0.015625D0*EL2*ML2*(B0(x, 0.D0, GaugeXiW*MW2) + (-1.D0*GaugeXiW*MW2 + x)*DB0(x, 0.D0, GaugeXiW*MW2)))/(MW2*PI2*&
  &SW2*x) - (0.015625D0*EL2*ML2*(A0(GaugeXiW*MW2) + (-1.D0*GaugeXiW*MW2 + x)*B0(x, 0.D0, GaugeXiW*MW2))*DBLE(x**INT(-2.D0)))/(MW2&
  &*PI2*SW2)

 amplitudes(7) = (-0.03125D0*EL2*DBLE(x**INT(-2.D0))*(-2.D0*x - 2.D0*A0(ML2) + ML2*B0(x, 0.D0, ML2) + GaugeXiA*ML2*B0(x, 0.D0, ML&
  &2) + x*B0(x, 0.D0, ML2) + GaugeXiA*x*B0(x, 0.D0, ML2) - 2.D0*ML2*x*C0Mine(DBLE(0.D0), DBLE(x), DBLE(x), DBLE(0.D0), DBLE(0.D0)&
  &, DBLE(ML2)) + 2.D0*GaugeXiA*ML2*x*C0Mine(DBLE(0.D0), DBLE(x), DBLE(x), DBLE(0.D0), DBLE(0.D0), DBLE(ML2)) + C0Mine(DBLE(0.D0)&
  &, DBLE(x), DBLE(x), DBLE(0.D0), DBLE(0.D0), DBLE(ML2))*DBLE(ML**INT(4.D0)) - 1.D0*GaugeXiA*C0Mine(DBLE(0.D0), DBLE(x), DBLE(x)&
  &, DBLE(0.D0), DBLE(0.D0), DBLE(ML2))*DBLE(ML**INT(4.D0)) + C0Mine(DBLE(0.D0), DBLE(x), DBLE(x), DBLE(0.D0), DBLE(0.D0), DBLE(M&
  &L2))*DBLE(x**INT(2.D0)) - 1.D0*GaugeXiA*C0Mine(DBLE(0.D0), DBLE(x), DBLE(x), DBLE(0.D0), DBLE(0.D0), DBLE(ML2))*DBLE(x**INT(2.&
  &D0))))/PI2 + (0.03125D0*EL2*(-2.D0 + B0(x, 0.D0, ML2) + GaugeXiA*B0(x, 0.D0, ML2) - 2.D0*ML2*C0Mine(DBLE(0.D0), DBLE(x), DBLE(&
  &x), DBLE(0.D0), DBLE(0.D0), DBLE(ML2)) + 2.D0*GaugeXiA*ML2*C0Mine(DBLE(0.D0), DBLE(x), DBLE(x), DBLE(0.D0), DBLE(0.D0), DBLE(M&
  &L2)) + 2.D0*x*C0Mine(DBLE(0.D0), DBLE(x), DBLE(x), DBLE(0.D0), DBLE(0.D0), DBLE(ML2)) - 2.D0*GaugeXiA*x*C0Mine(DBLE(0.D0), DBL&
  &E(x), DBLE(x), DBLE(0.D0), DBLE(0.D0), DBLE(ML2)) + ML2*DB0(x, 0.D0, ML2) + GaugeXiA*ML2*DB0(x, 0.D0, ML2) + x*DB0(x, 0.D0, ML&
  &2) + GaugeXiA*x*DB0(x, 0.D0, ML2) - 2.D0*ML2*x*(DC01Mine(DBLE(0.D0), DBLE(x), DBLE(x), DBLE(0.D0), DBLE(0.D0), DBLE(ML2)) + DC&
  &02Mine(DBLE(0.D0), DBLE(x), DBLE(x), DBLE(0.D0), DBLE(0.D0), DBLE(ML2))) + 2.D0*GaugeXiA*ML2*x*(DC01Mine(DBLE(0.D0), DBLE(x), &
  &DBLE(x), DBLE(0.D0), DBLE(0.D0), DBLE(ML2)) + DC02Mine(DBLE(0.D0), DBLE(x), DBLE(x), DBLE(0.D0), DBLE(0.D0), DBLE(ML2))) + DBL&
  &E(ML**INT(4.D0))*(DC01Mine(DBLE(0.D0), DBLE(x), DBLE(x), DBLE(0.D0), DBLE(0.D0), DBLE(ML2)) + DC02Mine(DBLE(0.D0), DBLE(x), DB&
  &LE(x), DBLE(0.D0), DBLE(0.D0), DBLE(ML2))) - 1.D0*GaugeXiA*DBLE(ML**INT(4.D0))* (DC01Mine(DBLE(0.D0), DBLE(x), DBLE(x), DBLE(0&
  &.D0), DBLE(0.D0), DBLE(ML2)) + DC02Mine(DBLE(0.D0), DBLE(x), DBLE(x), DBLE(0.D0), DBLE(0.D0), DBLE(ML2))) + DBLE(x**INT(2.D0))&
  &*(DC01Mine(DBLE(0.D0), DBLE(x), DBLE(x), DBLE(0.D0), DBLE(0.D0), DBLE(ML2)) + DC02Mine(DBLE(0.D0), DBLE(x), DBLE(x), DBLE(0.D0&
  &), DBLE(0.D0), DBLE(ML2))) - 1.D0*GaugeXiA*DBLE(x**INT(2.D0))* (DC01Mine(DBLE(0.D0), DBLE(x), DBLE(x), DBLE(0.D0), DBLE(0.D0),&
  & DBLE(ML2)) + DC02Mine(DBLE(0.D0), DBLE(x), DBLE(x), DBLE(0.D0), DBLE(0.D0), DBLE(ML2)))))/(PI2*x)

 amplitudes(8) = (-0.0078125D0*EL2*DBLE(x**INT(-2.D0))*(-8.D0*MZ2*x*DBLE(SW**INT(4.D0)) - 8.D0*MZ2*A0(ML2)*DBLE(SW**INT(4.D0)) + &
  &4.D0*ML2*A0(MZ2)*DBLE(SW**INT(4.D0)) + 8.D0*MZ2*A0(MZ2)*DBLE(SW**INT(4.D0)) - 4.D0*x*A0(MZ2)*DBLE(SW**INT(4.D0)) - 4.D0*ML2*A0&
  &(GaugeXiZ*MZ2)*DBLE(SW**INT(4.D0)) + 4.D0*x*A0(GaugeXiZ*MZ2)*DBLE(SW**INT(4.D0)) + 4.D0*ML2*MZ2*B0(x, ML2, MZ2)*DBLE(SW**INT(4&
  &.D0)) - 8.D0*ML2*x*B0(x, ML2, MZ2)*DBLE(SW**INT(4.D0)) + 4.D0*MZ2*x*B0(x, ML2, MZ2)*DBLE(SW**INT(4.D0)) + 4.D0*GaugeXiZ*ML2*MZ&
  &2*B0(x, ML2, GaugeXiZ*MZ2)*DBLE(SW**INT(4.D0)) + 8.D0*ML2*x*B0(x, ML2, GaugeXiZ*MZ2)*DBLE(SW**INT(4.D0)) + 4.D0*GaugeXiZ*MZ2*x&
  &*B0(x, ML2, GaugeXiZ*MZ2)*DBLE(SW**INT(4.D0)) + 4.D0*B0(x, ML2, MZ2)*DBLE(ML**INT(4.D0))*DBLE(SW**INT(4.D0)) - 4.D0*B0(x, ML2,&
  & GaugeXiZ*MZ2)*DBLE(ML**INT(4.D0))*DBLE(SW**INT(4.D0)) - 8.D0*B0(x, ML2, MZ2)*DBLE(MZ**INT(4.D0))*DBLE(SW**INT(4.D0)) + 4.D0*B&
  &0(x, ML2, MZ2)*DBLE(SW**INT(4.D0))*DBLE(x**INT(2.D0)) - 4.D0*B0(x, ML2, GaugeXiZ*MZ2)*DBLE(SW**INT(4.D0))*DBLE(x**INT(2.D0))))&
  &/ (CW2*MZ2*PI2*SW2) + (0.0078125D0*EL2*(-8.D0*MZ2*DBLE(SW**INT(4.D0)) - 4.D0*A0(MZ2)*DBLE(SW**INT(4.D0)) + 4.D0*A0(GaugeXiZ*MZ&
  &2)*DBLE(SW**INT(4.D0)) - 8.D0*ML2*B0(x, ML2, MZ2)*DBLE(SW**INT(4.D0)) + 4.D0*MZ2*B0(x, ML2, MZ2)*DBLE(SW**INT(4.D0)) + 8.D0*x*&
  &B0(x, ML2, MZ2)*DBLE(SW**INT(4.D0)) + 8.D0*ML2*B0(x, ML2, GaugeXiZ*MZ2)*DBLE(SW**INT(4.D0)) + 4.D0*GaugeXiZ*MZ2*B0(x, ML2, Gau&
  &geXiZ*MZ2)*DBLE(SW**INT(4.D0)) - 8.D0*x*B0(x, ML2, GaugeXiZ*MZ2)*DBLE(SW**INT(4.D0)) + 4.D0*ML2*MZ2*DB0(x, ML2, MZ2)*DBLE(SW**&
  &INT(4.D0)) - 8.D0*ML2*x*DB0(x, ML2, MZ2)*DBLE(SW**INT(4.D0)) + 4.D0*MZ2*x*DB0(x, ML2, MZ2)*DBLE(SW**INT(4.D0)) + 4.D0*GaugeXiZ&
  &*ML2*MZ2*DB0(x, ML2, GaugeXiZ*MZ2)*DBLE(SW**INT(4.D0)) + 8.D0*ML2*x*DB0(x, ML2, GaugeXiZ*MZ2)*DBLE(SW**INT(4.D0)) + 4.D0*Gauge&
  &XiZ*MZ2*x*DB0(x, ML2, GaugeXiZ*MZ2)*DBLE(SW**INT(4.D0)) + 4.D0*DB0(x, ML2, MZ2)*DBLE(ML**INT(4.D0))*DBLE(SW**INT(4.D0)) - 4.D0&
  &*DB0(x, ML2, GaugeXiZ*MZ2)*DBLE(ML**INT(4.D0))*DBLE(SW**INT(4.D0)) - 8.D0*DB0(x, ML2, MZ2)*DBLE(MZ**INT(4.D0))*DBLE(SW**INT(4.&
  &D0)) + 4.D0*DB0(x, ML2, MZ2)*DBLE(SW**INT(4.D0))*DBLE(x**INT(2.D0)) - 4.D0*DB0(x, ML2, GaugeXiZ*MZ2)*DBLE(SW**INT(4.D0))*DBLE(&
  &x**INT(2.D0))))/ (CW2*MZ2*PI2*SW2*x)

 amplitudes(9) = 0.D0

  totalAmplitude = (0D0,0D0)
 do j=1,9
  totalAmplitude = totalAmplitude + amplitudes(j)
 end do
 DSelfTauTauRight = totalAmplitude
end function DSelfTauTauRight

