double complex function SelfMuMuLeftUsual(x)
 use constants
 implicit none
#include "looptools.h"
 double precision, intent(in) :: x
 integer :: j
 double complex :: totalAmplitude
 double complex :: amplitudes(9)

 amplitudes(1) = (0.0078125D0*EL2*MM2*(A0(Mh02) - 1.D0*A0(MM2) - 1.D0*Mh02*B0(x, Mh02, MM2) + MM2*B0(x, Mh02, MM2) + x*B0(x, Mh02&
  &, MM2))*DBLE(Yuk4**INT(2.D0)))/ (MW2*PI2*SW2*x)

 amplitudes(2) = (0.0078125D0*EL2*MM2*(A0(MHH2) - 1.D0*A0(MM2) - 1.D0*MHH2*B0(x, MHH2, MM2) + MM2*B0(x, MHH2, MM2) + x*B0(x, MHH2&
  &, MM2))*DBLE(Yuk5**INT(2.D0)))/ (MW2*PI2*SW2*x)

 amplitudes(3) = (0.0078125D0*EL2*MM2*(A0(MA02) - 1.D0*A0(MM2) - 1.D0*MA02*B0(x, MA02, MM2) + MM2*B0(x, MA02, MM2) + x*B0(x, MA02&
  &, MM2))*DBLE(Yuk6**INT(2.D0)))/ (MW2*PI2*SW2*x)

 amplitudes(4) = (0.0078125D0*EL2*MM2*(-1.D0*A0(MM2) + A0(GaugeXiZ*MZ2) + MM2*B0(x, MM2, GaugeXiZ*MZ2) - 1.D0*GaugeXiZ*MZ2*B0(x, &
  &MM2, GaugeXiZ*MZ2) + x*B0(x, MM2, GaugeXiZ*MZ2)))/(MW2*PI2*SW2*x)

 amplitudes(5) = 0.D0

 amplitudes(6) = 0.D0

 amplitudes(7) = (0.03125D0*EL2*(-2.D0*x - 2.D0*A0(MM2) + MM2*B0(x, 0.D0, MM2) + GaugeXiA*MM2*B0(x, 0.D0, MM2) + x*B0(x, 0.D0, MM&
  &2) + GaugeXiA*x*B0(x, 0.D0, MM2) - 2.D0*MM2*x*C0Mine(DBLE(0.D0), DBLE(x), DBLE(x), DBLE(0.D0), DBLE(0.D0), DBLE(MM2)) + 2.D0*G&
  &augeXiA*MM2*x*C0Mine(DBLE(0.D0), DBLE(x), DBLE(x), DBLE(0.D0), DBLE(0.D0), DBLE(MM2)) + C0Mine(DBLE(0.D0), DBLE(x), DBLE(x), D&
  &BLE(0.D0), DBLE(0.D0), DBLE(MM2))*DBLE(MM**INT(4.D0)) - 1.D0*GaugeXiA*C0Mine(DBLE(0.D0), DBLE(x), DBLE(x), DBLE(0.D0), DBLE(0.&
  &D0), DBLE(MM2))*DBLE(MM**INT(4.D0)) + C0Mine(DBLE(0.D0), DBLE(x), DBLE(x), DBLE(0.D0), DBLE(0.D0), DBLE(MM2))*DBLE(x**INT(2.D0&
  &)) - 1.D0*GaugeXiA*C0Mine(DBLE(0.D0), DBLE(x), DBLE(x), DBLE(0.D0), DBLE(0.D0), DBLE(MM2))*DBLE(x**INT(2.D0))))/(PI2*x)

 amplitudes(8) = (0.0078125D0*EL2*(-2.D0*MZ2*x + 8.D0*MZ2*SW2*x - 2.D0*MZ2*A0(MM2) + 8.D0*MZ2*SW2*A0(MM2) + MM2*A0(MZ2) + 2.D0*MZ&
  &2*A0(MZ2) - 4.D0*MM2*SW2*A0(MZ2) - 8.D0*MZ2*SW2*A0(MZ2) - 1.D0*x*A0(MZ2) + 4.D0*SW2*x*A0(MZ2) - 1.D0*MM2*A0(GaugeXiZ*MZ2) + 4.&
  &D0*MM2*SW2*A0(GaugeXiZ*MZ2) + x*A0(GaugeXiZ*MZ2) - 4.D0*SW2*x*A0(GaugeXiZ*MZ2) + MM2*MZ2*B0(x, MM2, MZ2) - 4.D0*MM2*MZ2*SW2*B0&
  &(x, MM2, MZ2) - 2.D0*MM2*x*B0(x, MM2, MZ2) + MZ2*x*B0(x, MM2, MZ2) + 8.D0*MM2*SW2*x*B0(x, MM2, MZ2) - 4.D0*MZ2*SW2*x*B0(x, MM2&
  &, MZ2) + GaugeXiZ*MM2*MZ2*B0(x, MM2, GaugeXiZ*MZ2) - 4.D0*GaugeXiZ*MM2*MZ2*SW2*B0(x, MM2, GaugeXiZ*MZ2) + 2.D0*MM2*x*B0(x, MM2&
  &, GaugeXiZ*MZ2) + GaugeXiZ*MZ2*x*B0(x, MM2, GaugeXiZ*MZ2) - 8.D0*MM2*SW2*x*B0(x, MM2, GaugeXiZ*MZ2) - 4.D0*GaugeXiZ*MZ2*SW2*x*&
  &B0(x, MM2, GaugeXiZ*MZ2) + B0(x, MM2, MZ2)*DBLE(MM**INT(4.D0)) - 4.D0*SW2*B0(x, MM2, MZ2)*DBLE(MM**INT(4.D0)) - 1.D0*B0(x, MM2&
  &, GaugeXiZ*MZ2)*DBLE(MM**INT(4.D0)) + 4.D0*SW2*B0(x, MM2, GaugeXiZ*MZ2)*DBLE(MM**INT(4.D0)) - 2.D0*B0(x, MM2, MZ2)*DBLE(MZ**IN&
  &T(4.D0)) + 8.D0*SW2*B0(x, MM2, MZ2)*DBLE(MZ**INT(4.D0)) - 8.D0*MZ2*x*DBLE(SW**INT(4.D0)) - 8.D0*MZ2*A0(MM2)*DBLE(SW**INT(4.D0)&
  &) + 4.D0*MM2*A0(MZ2)*DBLE(SW**INT(4.D0)) + 8.D0*MZ2*A0(MZ2)*DBLE(SW**INT(4.D0)) - 4.D0*x*A0(MZ2)*DBLE(SW**INT(4.D0)) - 4.D0*MM&
  &2*A0(GaugeXiZ*MZ2)*DBLE(SW**INT(4.D0)) + 4.D0*x*A0(GaugeXiZ*MZ2)*DBLE(SW**INT(4.D0)) + 4.D0*MM2*MZ2*B0(x, MM2, MZ2)*DBLE(SW**I&
  &NT(4.D0)) - 8.D0*MM2*x*B0(x, MM2, MZ2)*DBLE(SW**INT(4.D0)) + 4.D0*MZ2*x*B0(x, MM2, MZ2)*DBLE(SW**INT(4.D0)) + 4.D0*GaugeXiZ*MM&
  &2*MZ2*B0(x, MM2, GaugeXiZ*MZ2)*DBLE(SW**INT(4.D0)) + 8.D0*MM2*x*B0(x, MM2, GaugeXiZ*MZ2)*DBLE(SW**INT(4.D0)) + 4.D0*GaugeXiZ*M&
  &Z2*x*B0(x, MM2, GaugeXiZ*MZ2)*DBLE(SW**INT(4.D0)) + 4.D0*B0(x, MM2, MZ2)*DBLE(MM**INT(4.D0))*DBLE(SW**INT(4.D0)) - 4.D0*B0(x, &
  &MM2, GaugeXiZ*MZ2)*DBLE(MM**INT(4.D0))*DBLE(SW**INT(4.D0)) - 8.D0*B0(x, MM2, MZ2)*DBLE(MZ**INT(4.D0))*DBLE(SW**INT(4.D0)) + B0&
  &(x, MM2, MZ2)*DBLE(x**INT(2.D0)) - 4.D0*SW2*B0(x, MM2, MZ2)*DBLE(x**INT(2.D0)) - 1.D0*B0(x, MM2, GaugeXiZ*MZ2)*DBLE(x**INT(2.D&
  &0)) + 4.D0*SW2*B0(x, MM2, GaugeXiZ*MZ2)*DBLE(x**INT(2.D0)) + 4.D0*B0(x, MM2, MZ2)*DBLE(SW**INT(4.D0))*DBLE(x**INT(2.D0)) - 4.D&
  &0*B0(x, MM2, GaugeXiZ*MZ2)*DBLE(SW**INT(4.D0))*DBLE(x**INT(2.D0))))/ (CW2*MZ2*PI2*SW2*x)

 amplitudes(9) = (0.015625D0*EL2*(-2.D0*MW2*x + (2.D0*MW2 - 1.D0*x)*A0(MW2) + x*A0(GaugeXiW*MW2) + MW2*x*B0(x, 0.D0, MW2) + Gauge&
  &XiW*MW2*x*B0(x, 0.D0, GaugeXiW*MW2) - 2.D0*B0(x, 0.D0, MW2)*DBLE(MW**INT(4.D0)) + B0(x, 0.D0, MW2)*DBLE(x**INT(2.D0)) - 1.D0*B&
  &0(x, 0.D0, GaugeXiW*MW2)*DBLE(x**INT(2.D0))))/(MW2*PI2*SW2*x)

  totalAmplitude = (0D0,0D0)
 do j=1,9
  totalAmplitude = totalAmplitude + amplitudes(j)
 end do
 SelfMuMuLeftUsual = totalAmplitude
end function SelfMuMuLeftUsual

