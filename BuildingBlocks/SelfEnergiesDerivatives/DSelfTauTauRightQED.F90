double complex function DSelfTauTauRightQED(x)
 use constants
 implicit none
#include "looptools.h"
 double precision, intent(in) :: x
 integer :: j
 double complex :: totalAmplitude
 double complex :: amplitudes(1)

 amplitudes(1) = (-0.03125D0*EL2*DBLE(x**INT(-2.D0))*(-2.D0*x - 2.D0*A0(ML2) + ML2*B0(x, 0.D0, ML2) + GaugeXiA*ML2*B0(x, 0.D0, ML&
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

  totalAmplitude = (0D0,0D0)
 do j=1,1
  totalAmplitude = totalAmplitude + amplitudes(j)
 end do
 DSelfTauTauRightQED = totalAmplitude
end function DSelfTauTauRightQED

