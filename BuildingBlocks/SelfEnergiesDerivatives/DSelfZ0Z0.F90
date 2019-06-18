double complex function DSelfZ0Z0(x)
 use constants
 implicit none
#include "looptools.h"
 double precision, intent(in) :: x
 integer :: j
 double complex :: totalAmplitude
 double complex :: amplitudes(32)

 amplitudes(1) = 0.D0

 amplitudes(2) = 0.D0

 amplitudes(3) = 0.D0

 amplitudes(4) = 0.D0

 amplitudes(5) = 0.D0

 amplitudes(6) = 0.D0

 amplitudes(7) = 0.D0

 amplitudes(8) = (0.003472222222222222D0*EL2*(-1.D0 + 3.D0*B0(x, 0.D0, 0.D0)))/(CW2*PI2*SW2) + (0.010416666666666666D0*EL2*x*DB0(&
  &x, 0.D0, 0.D0))/(CW2*PI2*SW2)

 amplitudes(9) = (0.003472222222222222D0*EL2*(-1.D0 + 3.D0*B0(x, 0.D0, 0.D0)))/(CW2*PI2*SW2) + (0.010416666666666666D0*EL2*x*DB0(&
  &x, 0.D0, 0.D0))/(CW2*PI2*SW2)

 amplitudes(10) = (0.003472222222222222D0*EL2*(-1.D0 + 3.D0*B0(x, 0.D0, 0.D0)))/(CW2*PI2*SW2) + (0.010416666666666666D0*EL2*x*DB0&
  &(x, 0.D0, 0.D0))/(CW2*PI2*SW2)

 amplitudes(11) = (0.003472222222222222D0*EL2*(-1.D0 + 4.D0*SW2 - 8.D0*DBLE(SW**INT(4.D0)) + 3.D0*B0(x, ME2, ME2)*(1.D0 - 4.D0*SW&
  &2 + 8.D0*DBLE(SW**INT(4.D0))) + 3.D0*DB0(x, ME2, ME2)*(x*(1.D0 - 4.D0*SW2 + 8.D0*DBLE(SW**INT(4.D0))) + ME2*(-1.D0 - 8.D0*SW2 &
  &+ 16.D0*DBLE(SW**INT(4.D0))))))/(CW2*PI2*SW2)

 amplitudes(12) = (0.003472222222222222D0*EL2*(-1.D0 + 4.D0*SW2 - 8.D0*DBLE(SW**INT(4.D0)) + 3.D0*B0(x, MM2, MM2)*(1.D0 - 4.D0*SW&
  &2 + 8.D0*DBLE(SW**INT(4.D0))) + 3.D0*DB0(x, MM2, MM2)*(x*(1.D0 - 4.D0*SW2 + 8.D0*DBLE(SW**INT(4.D0))) + MM2*(-1.D0 - 8.D0*SW2 &
  &+ 16.D0*DBLE(SW**INT(4.D0))))))/(CW2*PI2*SW2)

 amplitudes(13) = (0.003472222222222222D0*EL2*(-1.D0 + 4.D0*SW2 - 8.D0*DBLE(SW**INT(4.D0)) + 3.D0*B0(x, ML2, ML2)*(1.D0 - 4.D0*SW&
  &2 + 8.D0*DBLE(SW**INT(4.D0))) + 3.D0*DB0(x, ML2, ML2)*(x*(1.D0 - 4.D0*SW2 + 8.D0*DBLE(SW**INT(4.D0))) + ML2*(-1.D0 - 8.D0*SW2 &
  &+ 16.D0*DBLE(SW**INT(4.D0))))))/(CW2*PI2*SW2)

 amplitudes(14) = (0.0011574074074074073D0*EL2*(-9.D0 + 24.D0*SW2 - 32.D0*DBLE(SW**INT(4.D0)) + 3.D0*B0(x, MU2, MU2)*(9.D0 - 24.D&
  &0*SW2 + 32.D0*DBLE(SW**INT(4.D0))) + 3.D0*DB0(x, MU2, MU2)*(x*(9.D0 - 24.D0*SW2 + 32.D0*DBLE(SW**INT(4.D0))) + MU2*(-9.D0 - 48&
  &.D0*SW2 + 64.D0*DBLE(SW**INT(4.D0))))))/(CW2*PI2*SW2)

 amplitudes(15) = (0.0011574074074074073D0*EL2*(-9.D0 + 24.D0*SW2 - 32.D0*DBLE(SW**INT(4.D0)) + 3.D0*B0(x, MC2, MC2)*(9.D0 - 24.D&
  &0*SW2 + 32.D0*DBLE(SW**INT(4.D0))) + 3.D0*DB0(x, MC2, MC2)*(x*(9.D0 - 24.D0*SW2 + 32.D0*DBLE(SW**INT(4.D0))) + MC2*(-9.D0 - 48&
  &.D0*SW2 + 64.D0*DBLE(SW**INT(4.D0))))))/(CW2*PI2*SW2)

 amplitudes(16) = (0.0011574074074074073D0*EL2*(-9.D0 + 24.D0*SW2 - 32.D0*DBLE(SW**INT(4.D0)) + 3.D0*B0(x, MT2, MT2)*(9.D0 - 24.D&
  &0*SW2 + 32.D0*DBLE(SW**INT(4.D0))) + 3.D0*DB0(x, MT2, MT2)*(x*(9.D0 - 24.D0*SW2 + 32.D0*DBLE(SW**INT(4.D0))) + MT2*(-9.D0 - 48&
  &.D0*SW2 + 64.D0*DBLE(SW**INT(4.D0))))))/(CW2*PI2*SW2)

 amplitudes(17) = (0.0011574074074074073D0*EL2*(-9.D0 + 12.D0*SW2 - 8.D0*DBLE(SW**INT(4.D0)) + 3.D0*B0(x, MD2, MD2)*(9.D0 - 12.D0&
  &*SW2 + 8.D0*DBLE(SW**INT(4.D0))) + 3.D0*DB0(x, MD2, MD2)*(x*(9.D0 - 12.D0*SW2 + 8.D0*DBLE(SW**INT(4.D0))) + MD2*(-9.D0 - 24.D0&
  &*SW2 + 16.D0*DBLE(SW**INT(4.D0))))))/(CW2*PI2*SW2)

 amplitudes(18) = (0.0011574074074074073D0*EL2*(-9.D0 + 12.D0*SW2 - 8.D0*DBLE(SW**INT(4.D0)) + 3.D0*B0(x, MS2, MS2)*(9.D0 - 12.D0&
  &*SW2 + 8.D0*DBLE(SW**INT(4.D0))) + 3.D0*DB0(x, MS2, MS2)*(x*(9.D0 - 12.D0*SW2 + 8.D0*DBLE(SW**INT(4.D0))) + MS2*(-9.D0 - 24.D0&
  &*SW2 + 16.D0*DBLE(SW**INT(4.D0))))))/(CW2*PI2*SW2)

 amplitudes(19) = (0.0011574074074074073D0*EL2*(-9.D0 + 12.D0*SW2 - 8.D0*DBLE(SW**INT(4.D0)) + 3.D0*B0(x, MB2, MB2)*(9.D0 - 12.D0&
  &*SW2 + 8.D0*DBLE(SW**INT(4.D0))) + 3.D0*DB0(x, MB2, MB2)*(x*(9.D0 - 12.D0*SW2 + 8.D0*DBLE(SW**INT(4.D0))) + MB2*(-9.D0 - 24.D0&
  &*SW2 + 16.D0*DBLE(SW**INT(4.D0))))))/(CW2*PI2*SW2)

 amplitudes(20) = (-0.001736111111111111D0*CBA2*EL2*DBLE(x**INT(-2.D0))*(-6.D0*MA02*x - 6.D0*Mh02*x - 3.D0*(MA02 - 1.D0*Mh02 + x)&
  &*A0(MA02) + 3.D0*(MA02 - 1.D0*Mh02 - 1.D0*x)*A0(Mh02) - 6.D0*MA02*Mh02*B0(x, MA02, Mh02) - 6.D0*MA02*x*B0(x, MA02, Mh02) - 6.D&
  &0*Mh02*x*B0(x, MA02, Mh02) + 3.D0*B0(x, MA02, Mh02)*DBLE(MA0**INT(4.D0)) + 3.D0*B0(x, MA02, Mh02)*DBLE(Mh0**INT(4.D0)) + 2.D0*&
  &DBLE(x**INT(2.D0)) + 3.D0*B0(x, MA02, Mh02)*DBLE(x**INT(2.D0))))/(CW2*PI2*SW2) + (0.001736111111111111D0*CBA2*EL2*(-6.D0*MA02 &
  &- 6.D0*Mh02 + 4.D0*x - 3.D0*A0(MA02) - 3.D0*A0(Mh02) - 6.D0*MA02*B0(x, MA02, Mh02) - 6.D0*Mh02*B0(x, MA02, Mh02) + 6.D0*x*B0(x&
  &, MA02, Mh02) - 6.D0*MA02*Mh02*DB0(x, MA02, Mh02) - 6.D0*MA02*x*DB0(x, MA02, Mh02) - 6.D0*Mh02*x*DB0(x, MA02, Mh02) + 3.D0*DB0&
  &(x, MA02, Mh02)*DBLE(MA0**INT(4.D0)) + 3.D0*DB0(x, MA02, Mh02)*DBLE(Mh0**INT(4.D0)) + 3.D0*DB0(x, MA02, Mh02)*DBLE(x**INT(2.D0&
  &))))/(CW2*PI2*SW2*x)

 amplitudes(21) = (-0.001736111111111111D0*EL2*SBA2*DBLE(x**INT(-2.D0))*(-6.D0*MA02*x - 6.D0*MHH2*x - 3.D0*(MA02 - 1.D0*MHH2 + x)&
  &*A0(MA02) + 3.D0*(MA02 - 1.D0*MHH2 - 1.D0*x)*A0(MHH2) - 6.D0*MA02*MHH2*B0(x, MA02, MHH2) - 6.D0*MA02*x*B0(x, MA02, MHH2) - 6.D&
  &0*MHH2*x*B0(x, MA02, MHH2) + 3.D0*B0(x, MA02, MHH2)*DBLE(MA0**INT(4.D0)) + 3.D0*B0(x, MA02, MHH2)*DBLE(MHH**INT(4.D0)) + 2.D0*&
  &DBLE(x**INT(2.D0)) + 3.D0*B0(x, MA02, MHH2)*DBLE(x**INT(2.D0))))/(CW2*PI2*SW2) + (0.001736111111111111D0*EL2*SBA2*(-6.D0*MA02 &
  &- 6.D0*MHH2 + 4.D0*x - 3.D0*A0(MA02) - 3.D0*A0(MHH2) - 6.D0*MA02*B0(x, MA02, MHH2) - 6.D0*MHH2*B0(x, MA02, MHH2) + 6.D0*x*B0(x&
  &, MA02, MHH2) - 6.D0*MA02*MHH2*DB0(x, MA02, MHH2) - 6.D0*MA02*x*DB0(x, MA02, MHH2) - 6.D0*MHH2*x*DB0(x, MA02, MHH2) + 3.D0*DB0&
  &(x, MA02, MHH2)*DBLE(MA0**INT(4.D0)) + 3.D0*DB0(x, MA02, MHH2)*DBLE(MHH**INT(4.D0)) + 3.D0*DB0(x, MA02, MHH2)*DBLE(x**INT(2.D0&
  &))))/(CW2*PI2*SW2*x)

 amplitudes(22) = (-0.001736111111111111D0*EL2*SBA2*DBLE(x**INT(-2.D0))*(-6.D0*Mh02*x - 6.D0*GaugeXiZ*MZ2*x - 3.D0*(Mh02 - 1.D0*G&
  &augeXiZ*MZ2 + x)*A0(Mh02) + 3.D0*(Mh02 - 1.D0*GaugeXiZ*MZ2 - 1.D0*x)*A0(GaugeXiZ*MZ2) - 6.D0*GaugeXiZ*Mh02*MZ2*B0(x, Mh02, Gau&
  &geXiZ*MZ2) - 6.D0*Mh02*x*B0(x, Mh02, GaugeXiZ*MZ2) - 6.D0*GaugeXiZ*MZ2*x*B0(x, Mh02, GaugeXiZ*MZ2) + 3.D0*B0(x, Mh02, GaugeXiZ&
  &*MZ2)*DBLE(Mh0**INT(4.D0)) + 3.D0*B0(x, Mh02, GaugeXiZ*MZ2)*DBLE(GaugeXiZ**INT(2.D0))*DBLE(MZ**INT(4.D0)) + 2.D0*DBLE(x**INT(2&
  &.D0)) + 3.D0*B0(x, Mh02, GaugeXiZ*MZ2)*DBLE(x**INT(2.D0))))/(CW2*PI2*SW2) + (0.001736111111111111D0*EL2*SBA2*(-6.D0*Mh02 - 6.D&
  &0*GaugeXiZ*MZ2 + 4.D0*x - 3.D0*A0(Mh02) - 3.D0*A0(GaugeXiZ*MZ2) - 6.D0*Mh02*B0(x, Mh02, GaugeXiZ*MZ2) - 6.D0*GaugeXiZ*MZ2*B0(x&
  &, Mh02, GaugeXiZ*MZ2) + 6.D0*x*B0(x, Mh02, GaugeXiZ*MZ2) - 6.D0*GaugeXiZ*Mh02*MZ2*DB0(x, Mh02, GaugeXiZ*MZ2) - 6.D0*Mh02*x*DB0&
  &(x, Mh02, GaugeXiZ*MZ2) - 6.D0*GaugeXiZ*MZ2*x*DB0(x, Mh02, GaugeXiZ*MZ2) + 3.D0*DB0(x, Mh02, GaugeXiZ*MZ2)*DBLE(Mh0**INT(4.D0)&
  &) + 3.D0*DB0(x, Mh02, GaugeXiZ*MZ2)*DBLE(GaugeXiZ**INT(2.D0))*DBLE(MZ**INT(4.D0)) + 3.D0*DB0(x, Mh02, GaugeXiZ*MZ2)*DBLE(x**IN&
  &T(2.D0))))/ (CW2*PI2*SW2*x)

 amplitudes(23) = (-0.001736111111111111D0*CBA2*EL2*DBLE(x**INT(-2.D0))*(-6.D0*MHH2*x - 6.D0*GaugeXiZ*MZ2*x - 3.D0*(MHH2 - 1.D0*G&
  &augeXiZ*MZ2 + x)*A0(MHH2) + 3.D0*(MHH2 - 1.D0*GaugeXiZ*MZ2 - 1.D0*x)*A0(GaugeXiZ*MZ2) - 6.D0*GaugeXiZ*MHH2*MZ2*B0(x, MHH2, Gau&
  &geXiZ*MZ2) - 6.D0*MHH2*x*B0(x, MHH2, GaugeXiZ*MZ2) - 6.D0*GaugeXiZ*MZ2*x*B0(x, MHH2, GaugeXiZ*MZ2) + 3.D0*B0(x, MHH2, GaugeXiZ&
  &*MZ2)*DBLE(MHH**INT(4.D0)) + 3.D0*B0(x, MHH2, GaugeXiZ*MZ2)*DBLE(GaugeXiZ**INT(2.D0))*DBLE(MZ**INT(4.D0)) + 2.D0*DBLE(x**INT(2&
  &.D0)) + 3.D0*B0(x, MHH2, GaugeXiZ*MZ2)*DBLE(x**INT(2.D0))))/(CW2*PI2*SW2) + (0.001736111111111111D0*CBA2*EL2*(-6.D0*MHH2 - 6.D&
  &0*GaugeXiZ*MZ2 + 4.D0*x - 3.D0*A0(MHH2) - 3.D0*A0(GaugeXiZ*MZ2) - 6.D0*MHH2*B0(x, MHH2, GaugeXiZ*MZ2) - 6.D0*GaugeXiZ*MZ2*B0(x&
  &, MHH2, GaugeXiZ*MZ2) + 6.D0*x*B0(x, MHH2, GaugeXiZ*MZ2) - 6.D0*GaugeXiZ*MHH2*MZ2*DB0(x, MHH2, GaugeXiZ*MZ2) - 6.D0*MHH2*x*DB0&
  &(x, MHH2, GaugeXiZ*MZ2) - 6.D0*GaugeXiZ*MZ2*x*DB0(x, MHH2, GaugeXiZ*MZ2) + 3.D0*DB0(x, MHH2, GaugeXiZ*MZ2)*DBLE(MHH**INT(4.D0)&
  &) + 3.D0*DB0(x, MHH2, GaugeXiZ*MZ2)*DBLE(GaugeXiZ**INT(2.D0))*DBLE(MZ**INT(4.D0)) + 3.D0*DB0(x, MHH2, GaugeXiZ*MZ2)*DBLE(x**IN&
  &T(2.D0))))/ (CW2*PI2*SW2*x)

 amplitudes(24) = (-0.001736111111111111D0*EL2*(-2.D0 - 3.D0*B0(x, MHp2, MHp2) + 3.D0*(4.D0*MHp2 - 1.D0*x)*DB0(x, MHp2, MHp2))*DB&
  &LE((CW2 - 1.D0*SW2)**INT(2.D0)))/ (CW2*PI2*SW2)

 amplitudes(25) = (0.001736111111111111D0*EL2*(2.D0 + 3.D0*B0(x, GaugeXiW*MW2, GaugeXiW*MW2) + 3.D0*(-4.D0*GaugeXiW*MW2 + x)*DB0(&
  &x, GaugeXiW*MW2, GaugeXiW*MW2))* DBLE((CW2 - 1.D0*SW2)**INT(2.D0)))/(CW2*PI2*SW2)

 amplitudes(26) = (0.001736111111111111D0*CW2*EL2*(-2.D0 - 3.D0*B0(x, GaugeXiW*MW2, GaugeXiW*MW2) - 3.D0*(-4.D0*GaugeXiW*MW2 + x)&
  &*DB0(x, GaugeXiW*MW2, GaugeXiW*MW2)))/(PI2*SW2)

 amplitudes(27) = (0.001736111111111111D0*CW2*EL2*(-2.D0 - 3.D0*B0(x, GaugeXiW*MW2, GaugeXiW*MW2) - 3.D0*(-4.D0*GaugeXiW*MW2 + x)&
  &*DB0(x, GaugeXiW*MW2, GaugeXiW*MW2)))/(PI2*SW2)

 amplitudes(28) = (0.001736111111111111D0*CW2*EL2*DBLE(MW**INT(-4.D0))*DBLE(x**INT(-2.D0))*(-3.D0*x*DBLE(MW**INT(6.D0)) - 12.D0*G&
  &augeXiW*x*DBLE(MW**INT(6.D0)) + 144.D0*x*B0(x, MW2, MW2)*DBLE(MW**INT(6.D0)) + 48.D0*x*B0(x, MW2, GaugeXiW*MW2)*DBLE(MW**INT(6&
  &.D0)) + 12.D0*GaugeXiW*x*B0(x, MW2, GaugeXiW*MW2)*DBLE(MW**INT(6.D0)) - 9.D0*x*DBLE(GaugeXiW**INT(2.D0))*DBLE(MW**INT(6.D0)) -&
  & 12.D0*x*B0(x, MW2, GaugeXiW*MW2)*DBLE(GaugeXiW**INT(2.D0))*DBLE(MW**INT(6.D0)) + 6.D0*B0(x, MW2, GaugeXiW*MW2)*DBLE(MW**INT(8&
  &.D0)) - 12.D0*GaugeXiW*B0(x, MW2, GaugeXiW*MW2)*DBLE(MW**INT(8.D0)) + 6.D0*B0(x, MW2, GaugeXiW*MW2)*DBLE(GaugeXiW**INT(2.D0))*&
  &DBLE(MW**INT(8.D0)) - 20.D0*DBLE(MW**INT(4.D0))*DBLE(x**INT(2.D0)) + 24.D0*GaugeXiW*DBLE(MW**INT(4.D0))*DBLE(x**INT(2.D0)) + 2&
  &04.D0*B0(x, MW2, MW2)*DBLE(MW**INT(4.D0))*DBLE(x**INT(2.D0)) - 108.D0*B0(x, MW2, GaugeXiW*MW2)*DBLE(MW**INT(4.D0))*DBLE(x**INT&
  &(2.D0)) + 12.D0*GaugeXiW*B0(x, MW2, GaugeXiW*MW2)*DBLE(MW**INT(4.D0))*DBLE(x**INT(2.D0)) + 6.D0*B0(x, MW2, GaugeXiW*MW2)*DBLE(&
  &GaugeXiW**INT(2.D0))* DBLE(MW**INT(4.D0))*DBLE(x**INT(2.D0)) + 6.D0*MW2*A0(MW2)*(4.D0*MW2*x - 1.D0*DBLE(MW**INT(4.D0)) + Gauge&
  &XiW*DBLE((MW2 - 1.D0*x)**INT(2.D0)) + 9.D0*DBLE(x**INT(2.D0))) - 6.D0*MW2*A0(GaugeXiW*MW2)*(-9.D0*MW2*x - 1.D0*DBLE(MW**INT(4.&
  &D0)) + 9.D0*DBLE(x**INT(2.D0)) + GaugeXiW*(-11.D0*MW2*x + DBLE(MW**INT(4.D0)) + DBLE(x**INT(2.D0)))) - 48.D0*MW2*B0(x, MW2, MW&
  &2)*DBLE(x**INT(3.D0)) + 48.D0*MW2*B0(x, MW2, GaugeXiW*MW2)*DBLE(x**INT(3.D0)) - 12.D0*GaugeXiW*MW2*B0(x, MW2, GaugeXiW*MW2)*DB&
  &LE(x**INT(3.D0)) + 12.D0*GaugeXiW*MW2*B0(x, GaugeXiW*MW2, GaugeXiW*MW2)*DBLE(x**INT(3.D0)) - 3.D0*B0(x, MW2, MW2)*DBLE(x**INT(&
  &4.D0)) + 6.D0*B0(x, MW2, GaugeXiW*MW2)*DBLE(x**INT(4.D0)) - 3.D0*B0(x, GaugeXiW*MW2, GaugeXiW*MW2)*DBLE(x**INT(4.D0))))/(PI2*S&
  &W2) - (0.001736111111111111D0*CW2*EL2*DBLE(MW**INT(-4.D0))*(6.D0*MW2*(4.D0*MW2 - 2.D0*GaugeXiW*(MW2 - 1.D0*x) + 18.D0*x)*A0(MW&
  &2) - 6.D0*MW2*(-9.D0*MW2 + 18.D0*x + GaugeXiW*(-11.D0*MW2 + 2.D0*x))*A0(GaugeXiW*MW2) - 40.D0*x*DBLE(MW**INT(4.D0)) + 48.D0*Ga&
  &ugeXiW*x*DBLE(MW**INT(4.D0)) + 408.D0*x*B0(x, MW2, MW2)*DBLE(MW**INT(4.D0)) - 216.D0*x*B0(x, MW2, GaugeXiW*MW2)*DBLE(MW**INT(4&
  &.D0)) + 24.D0*GaugeXiW*x*B0(x, MW2, GaugeXiW*MW2)*DBLE(MW**INT(4.D0)) + 12.D0*x*B0(x, MW2, GaugeXiW*MW2)*DBLE(GaugeXiW**INT(2.&
  &D0))* DBLE(MW**INT(4.D0)) - 3.D0*DBLE(MW**INT(6.D0)) - 12.D0*GaugeXiW*DBLE(MW**INT(6.D0)) + 144.D0*B0(x, MW2, MW2)*DBLE(MW**IN&
  &T(6.D0)) + 48.D0*B0(x, MW2, GaugeXiW*MW2)*DBLE(MW**INT(6.D0)) + 12.D0*GaugeXiW*B0(x, MW2, GaugeXiW*MW2)*DBLE(MW**INT(6.D0)) + &
  &144.D0*x*DB0(x, MW2, MW2)*DBLE(MW**INT(6.D0)) + 48.D0*x*DB0(x, MW2, GaugeXiW*MW2)*DBLE(MW**INT(6.D0)) + 12.D0*GaugeXiW*x*DB0(x&
  &, MW2, GaugeXiW*MW2)*DBLE(MW**INT(6.D0)) - 9.D0*DBLE(GaugeXiW**INT(2.D0))*DBLE(MW**INT(6.D0)) - 12.D0*B0(x, MW2, GaugeXiW*MW2)&
  &*DBLE(GaugeXiW**INT(2.D0))*DBLE(MW**INT(6.D0)) - 12.D0*x*DB0(x, MW2, GaugeXiW*MW2)*DBLE(GaugeXiW**INT(2.D0))* DBLE(MW**INT(6.D&
  &0)) + 6.D0*DB0(x, MW2, GaugeXiW*MW2)*DBLE(MW**INT(8.D0)) - 12.D0*GaugeXiW*DB0(x, MW2, GaugeXiW*MW2)*DBLE(MW**INT(8.D0)) + 6.D0&
  &*DB0(x, MW2, GaugeXiW*MW2)*DBLE(GaugeXiW**INT(2.D0))*DBLE(MW**INT(8.D0)) - 144.D0*MW2*B0(x, MW2, MW2)*DBLE(x**INT(2.D0)) + 144&
  &.D0*MW2*B0(x, MW2, GaugeXiW*MW2)*DBLE(x**INT(2.D0)) - 36.D0*GaugeXiW*MW2*B0(x, MW2, GaugeXiW*MW2)*DBLE(x**INT(2.D0)) + 36.D0*G&
  &augeXiW*MW2*B0(x, GaugeXiW*MW2, GaugeXiW*MW2)*DBLE(x**INT(2.D0)) + 204.D0*DB0(x, MW2, MW2)*DBLE(MW**INT(4.D0))*DBLE(x**INT(2.D&
  &0)) - 108.D0*DB0(x, MW2, GaugeXiW*MW2)*DBLE(MW**INT(4.D0))*DBLE(x**INT(2.D0)) + 12.D0*GaugeXiW*DB0(x, MW2, GaugeXiW*MW2)*DBLE(&
  &MW**INT(4.D0))* DBLE(x**INT(2.D0)) + 6.D0*DB0(x, MW2, GaugeXiW*MW2)*DBLE(GaugeXiW**INT(2.D0))*DBLE(MW**INT(4.D0))*DBLE(x**INT(&
  &2.D0)) - 12.D0*B0(x, MW2, MW2)*DBLE(x**INT(3.D0)) + 24.D0*B0(x, MW2, GaugeXiW*MW2)*DBLE(x**INT(3.D0)) - 12.D0*B0(x, GaugeXiW*M&
  &W2, GaugeXiW*MW2)*DBLE(x**INT(3.D0)) - 48.D0*MW2*DB0(x, MW2, MW2)*DBLE(x**INT(3.D0)) + 48.D0*MW2*DB0(x, MW2, GaugeXiW*MW2)*DBL&
  &E(x**INT(3.D0)) - 12.D0*GaugeXiW*MW2*DB0(x, MW2, GaugeXiW*MW2)*DBLE(x**INT(3.D0)) + 12.D0*GaugeXiW*MW2*DB0(x, GaugeXiW*MW2, Ga&
  &ugeXiW*MW2)*DBLE(x**INT(3.D0)) - 3.D0*DB0(x, MW2, MW2)*DBLE(x**INT(4.D0)) + 6.D0*DB0(x, MW2, GaugeXiW*MW2)*DBLE(x**INT(4.D0)) &
  &- 3.D0*DB0(x, GaugeXiW*MW2, GaugeXiW*MW2)*DBLE(x**INT(4.D0))))/(PI2*SW2*x)

 amplitudes(29) = (-0.005208333333333333D0*EL2*MW2*SBA2*DBLE(CW**INT(-4.D0))*DBLE(x**INT(-2.D0))*(-2.D0*MZ2*x + 2.D0*GaugeXiZ*MZ2&
  &*x - 1.D0*(-1.D0 + GaugeXiZ)*MZ2*A0(Mh02) + (Mh02 - 1.D0*MZ2 - 1.D0*x)*A0(MZ2) - 1.D0*Mh02*A0(GaugeXiZ*MZ2) + GaugeXiZ*MZ2*A0(&
  &GaugeXiZ*MZ2) + x*A0(GaugeXiZ*MZ2) - 2.D0*Mh02*MZ2*B0(x, Mh02, MZ2) - 2.D0*Mh02*x*B0(x, Mh02, MZ2) + 10.D0*MZ2*x*B0(x, Mh02, M&
  &Z2) + 2.D0*GaugeXiZ*Mh02*MZ2*B0(x, Mh02, GaugeXiZ*MZ2) + 2.D0*Mh02*x*B0(x, Mh02, GaugeXiZ*MZ2) + 2.D0*GaugeXiZ*MZ2*x*B0(x, Mh0&
  &2, GaugeXiZ*MZ2) + B0(x, Mh02, MZ2)*DBLE(Mh0**INT(4.D0)) - 1.D0*B0(x, Mh02, GaugeXiZ*MZ2)*DBLE(Mh0**INT(4.D0)) + B0(x, Mh02, M&
  &Z2)*DBLE(MZ**INT(4.D0)) - 1.D0*B0(x, Mh02, GaugeXiZ*MZ2)*DBLE(GaugeXiZ**INT(2.D0))*DBLE(MZ**INT(4.D0)) + B0(x, Mh02, MZ2)*DBLE&
  &(x**INT(2.D0)) - 1.D0*B0(x, Mh02, GaugeXiZ*MZ2)*DBLE(x**INT(2.D0))))/(MZ2*PI2*SW2) + (0.005208333333333333D0*EL2*MW2*SBA2*DBLE&
  &(CW**INT(-4.D0))*(-2.D0*MZ2 + 2.D0*GaugeXiZ*MZ2 - 1.D0*A0(MZ2) + A0(GaugeXiZ*MZ2) - 2.D0*Mh02*B0(x, Mh02, MZ2) + 10.D0*MZ2*B0(&
  &x, Mh02, MZ2) + 2.D0*x*B0(x, Mh02, MZ2) + 2.D0*Mh02*B0(x, Mh02, GaugeXiZ*MZ2) + 2.D0*GaugeXiZ*MZ2*B0(x, Mh02, GaugeXiZ*MZ2) - &
  &2.D0*x*B0(x, Mh02, GaugeXiZ*MZ2) - 2.D0*Mh02*MZ2*DB0(x, Mh02, MZ2) - 2.D0*Mh02*x*DB0(x, Mh02, MZ2) + 10.D0*MZ2*x*DB0(x, Mh02, &
  &MZ2) + 2.D0*GaugeXiZ*Mh02*MZ2*DB0(x, Mh02, GaugeXiZ*MZ2) + 2.D0*Mh02*x*DB0(x, Mh02, GaugeXiZ*MZ2) + 2.D0*GaugeXiZ*MZ2*x*DB0(x,&
  & Mh02, GaugeXiZ*MZ2) + DB0(x, Mh02, MZ2)*DBLE(Mh0**INT(4.D0)) - 1.D0*DB0(x, Mh02, GaugeXiZ*MZ2)*DBLE(Mh0**INT(4.D0)) + DB0(x, &
  &Mh02, MZ2)*DBLE(MZ**INT(4.D0)) - 1.D0*DB0(x, Mh02, GaugeXiZ*MZ2)*DBLE(GaugeXiZ**INT(2.D0))*DBLE(MZ**INT(4.D0)) + DB0(x, Mh02, &
  &MZ2)*DBLE(x**INT(2.D0)) - 1.D0*DB0(x, Mh02, GaugeXiZ*MZ2)*DBLE(x**INT(2.D0))))/(MZ2*PI2*SW2*x)

 amplitudes(30) = (-0.005208333333333333D0*CBA2*EL2*MW2*DBLE(CW**INT(-4.D0))*DBLE(x**INT(-2.D0))*(-2.D0*MZ2*x + 2.D0*GaugeXiZ*MZ2&
  &*x - 1.D0*(-1.D0 + GaugeXiZ)*MZ2*A0(MHH2) + (MHH2 - 1.D0*MZ2 - 1.D0*x)*A0(MZ2) - 1.D0*MHH2*A0(GaugeXiZ*MZ2) + GaugeXiZ*MZ2*A0(&
  &GaugeXiZ*MZ2) + x*A0(GaugeXiZ*MZ2) - 2.D0*MHH2*MZ2*B0(x, MHH2, MZ2) - 2.D0*MHH2*x*B0(x, MHH2, MZ2) + 10.D0*MZ2*x*B0(x, MHH2, M&
  &Z2) + 2.D0*GaugeXiZ*MHH2*MZ2*B0(x, MHH2, GaugeXiZ*MZ2) + 2.D0*MHH2*x*B0(x, MHH2, GaugeXiZ*MZ2) + 2.D0*GaugeXiZ*MZ2*x*B0(x, MHH&
  &2, GaugeXiZ*MZ2) + B0(x, MHH2, MZ2)*DBLE(MHH**INT(4.D0)) - 1.D0*B0(x, MHH2, GaugeXiZ*MZ2)*DBLE(MHH**INT(4.D0)) + B0(x, MHH2, M&
  &Z2)*DBLE(MZ**INT(4.D0)) - 1.D0*B0(x, MHH2, GaugeXiZ*MZ2)*DBLE(GaugeXiZ**INT(2.D0))*DBLE(MZ**INT(4.D0)) + B0(x, MHH2, MZ2)*DBLE&
  &(x**INT(2.D0)) - 1.D0*B0(x, MHH2, GaugeXiZ*MZ2)*DBLE(x**INT(2.D0))))/(MZ2*PI2*SW2) + (0.005208333333333333D0*CBA2*EL2*MW2*DBLE&
  &(CW**INT(-4.D0))*(-2.D0*MZ2 + 2.D0*GaugeXiZ*MZ2 - 1.D0*A0(MZ2) + A0(GaugeXiZ*MZ2) - 2.D0*MHH2*B0(x, MHH2, MZ2) + 10.D0*MZ2*B0(&
  &x, MHH2, MZ2) + 2.D0*x*B0(x, MHH2, MZ2) + 2.D0*MHH2*B0(x, MHH2, GaugeXiZ*MZ2) + 2.D0*GaugeXiZ*MZ2*B0(x, MHH2, GaugeXiZ*MZ2) - &
  &2.D0*x*B0(x, MHH2, GaugeXiZ*MZ2) - 2.D0*MHH2*MZ2*DB0(x, MHH2, MZ2) - 2.D0*MHH2*x*DB0(x, MHH2, MZ2) + 10.D0*MZ2*x*DB0(x, MHH2, &
  &MZ2) + 2.D0*GaugeXiZ*MHH2*MZ2*DB0(x, MHH2, GaugeXiZ*MZ2) + 2.D0*MHH2*x*DB0(x, MHH2, GaugeXiZ*MZ2) + 2.D0*GaugeXiZ*MZ2*x*DB0(x,&
  & MHH2, GaugeXiZ*MZ2) + DB0(x, MHH2, MZ2)*DBLE(MHH**INT(4.D0)) - 1.D0*DB0(x, MHH2, GaugeXiZ*MZ2)*DBLE(MHH**INT(4.D0)) + DB0(x, &
  &MHH2, MZ2)*DBLE(MZ**INT(4.D0)) - 1.D0*DB0(x, MHH2, GaugeXiZ*MZ2)*DBLE(GaugeXiZ**INT(2.D0))*DBLE(MZ**INT(4.D0)) + DB0(x, MHH2, &
  &MZ2)*DBLE(x**INT(2.D0)) - 1.D0*DB0(x, MHH2, GaugeXiZ*MZ2)*DBLE(x**INT(2.D0))))/(MZ2*PI2*SW2*x)

 amplitudes(31) = (-0.005208333333333333D0*EL2*SW2*DBLE(x**INT(-2.D0))*(-2.D0*MW2*x + 2.D0*GaugeXiW*MW2*x + (-1.D0*MW2 + GaugeXiW&
  &*MW2 - 1.D0*x)*A0(MW2) + (MW2 - 1.D0*GaugeXiW*MW2 + x)*A0(GaugeXiW*MW2) + 10.D0*MW2*x*B0(x, MW2, GaugeXiW*MW2) - 2.D0*GaugeXiW&
  &*MW2*x*B0(x, MW2, GaugeXiW*MW2) + 4.D0*GaugeXiW*MW2*x*B0(x, GaugeXiW*MW2, GaugeXiW*MW2) + B0(x, MW2, GaugeXiW*MW2)*DBLE(MW**IN&
  &T(4.D0)) - 2.D0*GaugeXiW*B0(x, MW2, GaugeXiW*MW2)*DBLE(MW**INT(4.D0)) + B0(x, MW2, GaugeXiW*MW2)*DBLE(GaugeXiW**INT(2.D0))*DBL&
  &E(MW**INT(4.D0)) + B0(x, MW2, GaugeXiW*MW2)*DBLE(x**INT(2.D0)) - 1.D0*B0(x, GaugeXiW*MW2, GaugeXiW*MW2)*DBLE(x**INT(2.D0))))/(&
  &CW2*PI2) + (0.005208333333333333D0*EL2*SW2*(-2.D0*MW2 + 2.D0*GaugeXiW*MW2 - 1.D0*A0(MW2) + A0(GaugeXiW*MW2) + 10.D0*MW2*B0(x, &
  &MW2, GaugeXiW*MW2) - 2.D0*GaugeXiW*MW2*B0(x, MW2, GaugeXiW*MW2) + 2.D0*x*B0(x, MW2, GaugeXiW*MW2) + 4.D0*GaugeXiW*MW2*B0(x, Ga&
  &ugeXiW*MW2, GaugeXiW*MW2) - 2.D0*x*B0(x, GaugeXiW*MW2, GaugeXiW*MW2) + 10.D0*MW2*x*DB0(x, MW2, GaugeXiW*MW2) - 2.D0*GaugeXiW*M&
  &W2*x*DB0(x, MW2, GaugeXiW*MW2) + 4.D0*GaugeXiW*MW2*x*DB0(x, GaugeXiW*MW2, GaugeXiW*MW2) + DB0(x, MW2, GaugeXiW*MW2)*DBLE(MW**I&
  &NT(4.D0)) - 2.D0*GaugeXiW*DB0(x, MW2, GaugeXiW*MW2)*DBLE(MW**INT(4.D0)) + DB0(x, MW2, GaugeXiW*MW2)*DBLE(GaugeXiW**INT(2.D0))*&
  &DBLE(MW**INT(4.D0)) + DB0(x, MW2, GaugeXiW*MW2)*DBLE(x**INT(2.D0)) - 1.D0*DB0(x, GaugeXiW*MW2, GaugeXiW*MW2)*DBLE(x**INT(2.D0)&
  &)))/(CW2*PI2*x)

 amplitudes(32) = (-0.005208333333333333D0*EL2*SW2*DBLE(x**INT(-2.D0))*(-2.D0*MW2*x + 2.D0*GaugeXiW*MW2*x + (-1.D0*MW2 + GaugeXiW&
  &*MW2 - 1.D0*x)*A0(MW2) + (MW2 - 1.D0*GaugeXiW*MW2 + x)*A0(GaugeXiW*MW2) + 10.D0*MW2*x*B0(x, MW2, GaugeXiW*MW2) - 2.D0*GaugeXiW&
  &*MW2*x*B0(x, MW2, GaugeXiW*MW2) + 4.D0*GaugeXiW*MW2*x*B0(x, GaugeXiW*MW2, GaugeXiW*MW2) + B0(x, MW2, GaugeXiW*MW2)*DBLE(MW**IN&
  &T(4.D0)) - 2.D0*GaugeXiW*B0(x, MW2, GaugeXiW*MW2)*DBLE(MW**INT(4.D0)) + B0(x, MW2, GaugeXiW*MW2)*DBLE(GaugeXiW**INT(2.D0))*DBL&
  &E(MW**INT(4.D0)) + B0(x, MW2, GaugeXiW*MW2)*DBLE(x**INT(2.D0)) - 1.D0*B0(x, GaugeXiW*MW2, GaugeXiW*MW2)*DBLE(x**INT(2.D0))))/(&
  &CW2*PI2) + (0.005208333333333333D0*EL2*SW2*(-2.D0*MW2 + 2.D0*GaugeXiW*MW2 - 1.D0*A0(MW2) + A0(GaugeXiW*MW2) + 10.D0*MW2*B0(x, &
  &MW2, GaugeXiW*MW2) - 2.D0*GaugeXiW*MW2*B0(x, MW2, GaugeXiW*MW2) + 2.D0*x*B0(x, MW2, GaugeXiW*MW2) + 4.D0*GaugeXiW*MW2*B0(x, Ga&
  &ugeXiW*MW2, GaugeXiW*MW2) - 2.D0*x*B0(x, GaugeXiW*MW2, GaugeXiW*MW2) + 10.D0*MW2*x*DB0(x, MW2, GaugeXiW*MW2) - 2.D0*GaugeXiW*M&
  &W2*x*DB0(x, MW2, GaugeXiW*MW2) + 4.D0*GaugeXiW*MW2*x*DB0(x, GaugeXiW*MW2, GaugeXiW*MW2) + DB0(x, MW2, GaugeXiW*MW2)*DBLE(MW**I&
  &NT(4.D0)) - 2.D0*GaugeXiW*DB0(x, MW2, GaugeXiW*MW2)*DBLE(MW**INT(4.D0)) + DB0(x, MW2, GaugeXiW*MW2)*DBLE(GaugeXiW**INT(2.D0))*&
  &DBLE(MW**INT(4.D0)) + DB0(x, MW2, GaugeXiW*MW2)*DBLE(x**INT(2.D0)) - 1.D0*DB0(x, GaugeXiW*MW2, GaugeXiW*MW2)*DBLE(x**INT(2.D0)&
  &)))/(CW2*PI2*x)

  totalAmplitude = (0D0,0D0)
 do j=1,32
  totalAmplitude = totalAmplitude + amplitudes(j)
 end do
 DSelfZ0Z0 = totalAmplitude
end function DSelfZ0Z0

