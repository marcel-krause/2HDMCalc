double complex function SelfTauTauLeftQEDUsual(x)
 use constants
 implicit none
#include "looptools.h"
 double precision, intent(in) :: x
 integer :: j
 double complex :: totalAmplitude
 double complex :: amplitudes(1)

 amplitudes(1) = (0.03125D0*EL2*(-2.D0*x - 2.D0*A0(ML2) + ML2*B0(x, 0.D0, ML2) + GaugeXiA*ML2*B0(x, 0.D0, ML2) + x*B0(x, 0.D0, ML&
  &2) + GaugeXiA*x*B0(x, 0.D0, ML2) - 2.D0*ML2*x*C0Mine(DBLE(0.D0), DBLE(x), DBLE(x), DBLE(0.D0), DBLE(0.D0), DBLE(ML2)) + 2.D0*G&
  &augeXiA*ML2*x*C0Mine(DBLE(0.D0), DBLE(x), DBLE(x), DBLE(0.D0), DBLE(0.D0), DBLE(ML2)) + C0Mine(DBLE(0.D0), DBLE(x), DBLE(x), D&
  &BLE(0.D0), DBLE(0.D0), DBLE(ML2))*DBLE(ML**INT(4.D0)) - 1.D0*GaugeXiA*C0Mine(DBLE(0.D0), DBLE(x), DBLE(x), DBLE(0.D0), DBLE(0.&
  &D0), DBLE(ML2))*DBLE(ML**INT(4.D0)) + C0Mine(DBLE(0.D0), DBLE(x), DBLE(x), DBLE(0.D0), DBLE(0.D0), DBLE(ML2))*DBLE(x**INT(2.D0&
  &)) - 1.D0*GaugeXiA*C0Mine(DBLE(0.D0), DBLE(x), DBLE(x), DBLE(0.D0), DBLE(0.D0), DBLE(ML2))*DBLE(x**INT(2.D0))))/(PI2*x)

  totalAmplitude = (0D0,0D0)
 do j=1,1
  totalAmplitude = totalAmplitude + amplitudes(j)
 end do
 SelfTauTauLeftQEDUsual = totalAmplitude
end function SelfTauTauLeftQEDUsual

