double complex function SelfAAAlter(x)
 use constants
 implicit none
#include "looptools.h"
 double precision, intent(in) :: x
 integer :: j
 double complex :: totalAmplitude
 double complex :: amplitudes(19)

 amplitudes(1) = (0.125D0*EL2*A0(MHp2))/PI2

 amplitudes(2) = (0.125D0*EL2*A0(GaugeXiW*MW2))/PI2

 amplitudes(3) = (-0.234375D0*EL2*MW2)/PI2 + (0.28125D0*EL2*A0(MW2))/PI2 + (0.09375D0*EL2*GaugeXiW*A0(GaugeXiW*MW2))/PI2 - (0.015&
  &625D0*EL2*MW2*DBLE(GaugeXiW**INT(2.D0)))/PI2

 amplitudes(4) = (0.16666666666666666D0*EL2*ME2)/PI2 - (0.027777777777777776D0*EL2*x)/PI2 - (0.16666666666666666D0*EL2*A0(ME2))/P&
  &I2 + (0.16666666666666666D0*EL2*ME2*B0(x, ME2, ME2))/PI2 + (0.125D0*EL2*x*B0(x, ME2, ME2))/PI2 + (0.08333333333333333D0*EL2*x*&
  &B1(x, ME2, ME2))/PI2

 amplitudes(5) = (0.16666666666666666D0*EL2*MM2)/PI2 - (0.027777777777777776D0*EL2*x)/PI2 - (0.16666666666666666D0*EL2*A0(MM2))/P&
  &I2 + (0.16666666666666666D0*EL2*MM2*B0(x, MM2, MM2))/PI2 + (0.125D0*EL2*x*B0(x, MM2, MM2))/PI2 + (0.08333333333333333D0*EL2*x*&
  &B1(x, MM2, MM2))/PI2

 amplitudes(6) = (0.16666666666666666D0*EL2*ML2)/PI2 - (0.027777777777777776D0*EL2*x)/PI2 - (0.16666666666666666D0*EL2*A0(ML2))/P&
  &I2 + (0.16666666666666666D0*EL2*ML2*B0(x, ML2, ML2))/PI2 + (0.125D0*EL2*x*B0(x, ML2, ML2))/PI2 + (0.08333333333333333D0*EL2*x*&
  &B1(x, ML2, ML2))/PI2

 amplitudes(7) = (0.2222222222222222D0*EL2*MU2)/PI2 - (0.037037037037037035D0*EL2*x)/PI2 - (0.2222222222222222D0*EL2*A0(MU2))/PI2&
  & + (0.2222222222222222D0*EL2*MU2*B0(x, MU2, MU2))/PI2 + (0.16666666666666666D0*EL2*x*B0(x, MU2, MU2))/PI2 + (0.111111111111111&
  &1D0*EL2*x*B1(x, MU2, MU2))/PI2

 amplitudes(8) = (0.2222222222222222D0*EL2*MC2)/PI2 - (0.037037037037037035D0*EL2*x)/PI2 - (0.2222222222222222D0*EL2*A0(MC2))/PI2&
  & + (0.2222222222222222D0*EL2*MC2*B0(x, MC2, MC2))/PI2 + (0.16666666666666666D0*EL2*x*B0(x, MC2, MC2))/PI2 + (0.111111111111111&
  &1D0*EL2*x*B1(x, MC2, MC2))/PI2

 amplitudes(9) = (0.2222222222222222D0*EL2*MT2)/PI2 - (0.037037037037037035D0*EL2*x)/PI2 - (0.2222222222222222D0*EL2*A0(MT2))/PI2&
  & + (0.2222222222222222D0*EL2*MT2*B0(x, MT2, MT2))/PI2 + (0.16666666666666666D0*EL2*x*B0(x, MT2, MT2))/PI2 + (0.111111111111111&
  &1D0*EL2*x*B1(x, MT2, MT2))/PI2

 amplitudes(10) = (0.05555555555555555D0*EL2*MD2)/PI2 - (0.009259259259259259D0*EL2*x)/PI2 - (0.05555555555555555D0*EL2*A0(MD2))/&
  &PI2 + (0.05555555555555555D0*EL2*MD2*B0(x, MD2, MD2))/PI2 + (0.041666666666666664D0*EL2*x*B0(x, MD2, MD2))/PI2 + (0.0277777777&
  &77777776D0*EL2*x*B1(x, MD2, MD2))/PI2

 amplitudes(11) = (0.05555555555555555D0*EL2*MS2)/PI2 - (0.009259259259259259D0*EL2*x)/PI2 - (0.05555555555555555D0*EL2*A0(MS2))/&
  &PI2 + (0.05555555555555555D0*EL2*MS2*B0(x, MS2, MS2))/PI2 + (0.041666666666666664D0*EL2*x*B0(x, MS2, MS2))/PI2 + (0.0277777777&
  &77777776D0*EL2*x*B1(x, MS2, MS2))/PI2

 amplitudes(12) = (0.05555555555555555D0*EL2*MB2)/PI2 - (0.009259259259259259D0*EL2*x)/PI2 - (0.05555555555555555D0*EL2*A0(MB2))/&
  &PI2 + (0.05555555555555555D0*EL2*MB2*B0(x, MB2, MB2))/PI2 + (0.041666666666666664D0*EL2*x*B0(x, MB2, MB2))/PI2 + (0.0277777777&
  &77777776D0*EL2*x*B1(x, MB2, MB2))/PI2

 amplitudes(13) = (-0.08333333333333333D0*EL2*MHp2)/PI2 + (0.013888888888888888D0*EL2*x)/PI2 - (0.041666666666666664D0*EL2*A0(MHp&
  &2))/PI2 - (0.08333333333333333D0*EL2*MHp2*B0(x, MHp2, MHp2))/PI2 - (0.041666666666666664D0*EL2*x*B1(x, MHp2, MHp2))/PI2

 amplitudes(14) = (-0.08333333333333333D0*EL2*GaugeXiW*MW2)/PI2 + (0.013888888888888888D0*EL2*x)/PI2 - (0.041666666666666664D0*EL&
  &2*A0(GaugeXiW*MW2))/PI2 - (0.08333333333333333D0*EL2*GaugeXiW*MW2*B0(x, GaugeXiW*MW2, GaugeXiW*MW2))/PI2 - (0.0416666666666666&
  &64D0*EL2*x*B1(x, GaugeXiW*MW2, GaugeXiW*MW2))/PI2

 amplitudes(15) = (0.020833333333333332D0*EL2*GaugeXiW*MW2)/PI2 - (0.003472222222222222D0*EL2*x)/PI2 + (0.010416666666666666D0*EL&
  &2*A0(GaugeXiW*MW2))/PI2 + (0.020833333333333332D0*EL2*GaugeXiW*MW2*B0(x, GaugeXiW*MW2, GaugeXiW*MW2))/PI2 + (0.010416666666666&
  &666D0*EL2*x*B1(x, GaugeXiW*MW2, GaugeXiW*MW2))/PI2

 amplitudes(16) = (0.020833333333333332D0*EL2*GaugeXiW*MW2)/PI2 - (0.003472222222222222D0*EL2*x)/PI2 + (0.010416666666666666D0*EL&
  &2*A0(GaugeXiW*MW2))/PI2 + (0.020833333333333332D0*EL2*GaugeXiW*MW2*B0(x, GaugeXiW*MW2, GaugeXiW*MW2))/PI2 + (0.010416666666666&
  &666D0*EL2*x*B1(x, GaugeXiW*MW2, GaugeXiW*MW2))/PI2

 amplitudes(17) = (0.005208333333333333D0*EL2*MW2)/PI2 + (0.020833333333333332D0*EL2*GaugeXiW*MW2)/PI2 + (0.034722222222222224D0*&
  &EL2*x)/PI2 - (0.041666666666666664D0*EL2*GaugeXiW*x)/PI2 - (0.03125D0*EL2*A0(MW2))/PI2 - (0.08333333333333333D0*EL2*x*A0(MW2))&
  &/(MW2*PI2) - (0.10416666666666667D0*EL2*A0(GaugeXiW*MW2))/PI2 - (0.09375D0*EL2*GaugeXiW*A0(GaugeXiW*MW2))/PI2 + (0.08333333333&
  &333333D0*EL2*x*A0(GaugeXiW*MW2))/(MW2*PI2) - (0.25D0*EL2*MW2*B0(x, MW2, MW2))/PI2 - (0.4166666666666667D0*EL2*x*B0(x, MW2, MW2&
  &))/PI2 - (0.08333333333333333D0*EL2*MW2*B0(x, MW2, GaugeXiW*MW2))/PI2 + (0.16666666666666666D0*EL2*x*B0(x, MW2, GaugeXiW*MW2))&
  &/PI2 - (0.125D0*EL2*x*B1(x, MW2, MW2))/PI2 + (0.020833333333333332D0*EL2*MW2*B1(x, MW2, GaugeXiW*MW2))/PI2 - (0.02083333333333&
  &3332D0*EL2*GaugeXiW*MW2*B1(x, MW2, GaugeXiW*MW2))/PI2 - (0.020833333333333332D0*EL2*x*B1(x, MW2, GaugeXiW*MW2))/PI2 + (0.04166&
  &6666666666664D0*EL2*GaugeXiW*x*B1(x, MW2, GaugeXiW*MW2))/PI2 + (0.015625D0*EL2*MW2*DBLE(GaugeXiW**INT(2.D0)))/PI2 + (0.1041666&
  &6666666667D0*EL2*B0(x, MW2, MW2)*DBLE(x**INT(2.D0)))/(MW2*PI2) - (0.08333333333333333D0*EL2*B0(x, MW2, GaugeXiW*MW2)*DBLE(x**I&
  &NT(2.D0)))/(MW2*PI2) - (0.020833333333333332D0*EL2*GaugeXiW*B0(x, GaugeXiW*MW2, GaugeXiW*MW2)*DBLE(x**INT(2.D0)))/(MW2*PI2) + &
  &(0.041666666666666664D0*EL2*B1(x, MW2, MW2)*DBLE(x**INT(2.D0)))/(MW2*PI2) - (0.020833333333333332D0*EL2*B1(x, MW2, GaugeXiW*MW&
  &2)*DBLE(x**INT(2.D0)))/(MW2*PI2) - (0.020833333333333332D0*EL2*GaugeXiW*B1(x, MW2, GaugeXiW*MW2)*DBLE(x**INT(2.D0)))/(MW2*PI2)&
  & - (0.010416666666666666D0*EL2*A0(MW2)*DBLE(MW**INT(-4.D0))*DBLE(x**INT(2.D0)))/PI2 + (0.010416666666666666D0*EL2*A0(GaugeXiW*&
  &MW2)*DBLE(MW**INT(-4.D0))*DBLE(x**INT(2.D0)))/PI2 - (0.010416666666666666D0*EL2*B1(x, MW2, MW2)*DBLE(MW**INT(-4.D0))*DBLE(x**I&
  &NT(3.D0)))/PI2 + (0.020833333333333332D0*EL2*B1(x, MW2, GaugeXiW*MW2)*DBLE(MW**INT(-4.D0))*DBLE(x**INT(3.D0)))/PI2 - (0.010416&
  &666666666666D0*EL2*B1(x, GaugeXiW*MW2, GaugeXiW*MW2)*DBLE(MW**INT(-4.D0))*DBLE(x**INT(3.D0)))/PI2

 amplitudes(18) = (-0.010416666666666666D0*EL2*MW2)/PI2 + (0.010416666666666666D0*EL2*GaugeXiW*MW2)/PI2 + (0.041666666666666664D0&
  &*EL2*MW2*B0(x, MW2, GaugeXiW*MW2))/PI2 + (0.020833333333333332D0*EL2*GaugeXiW*MW2*B0(x, GaugeXiW*MW2, GaugeXiW*MW2))/ PI2 - (0&
  &.010416666666666666D0*EL2*MW2*B1(x, MW2, GaugeXiW*MW2))/PI2 + (0.010416666666666666D0*EL2*GaugeXiW*MW2*B1(x, MW2, GaugeXiW*MW2&
  &))/ PI2 - (0.010416666666666666D0*EL2*x*B1(x, MW2, GaugeXiW*MW2))/PI2 + (0.010416666666666666D0*EL2*x*B1(x, GaugeXiW*MW2, Gaug&
  &eXiW*MW2))/PI2

 amplitudes(19) = (-0.010416666666666666D0*EL2*MW2)/PI2 + (0.010416666666666666D0*EL2*GaugeXiW*MW2)/PI2 + (0.041666666666666664D0&
  &*EL2*MW2*B0(x, MW2, GaugeXiW*MW2))/PI2 + (0.020833333333333332D0*EL2*GaugeXiW*MW2*B0(x, GaugeXiW*MW2, GaugeXiW*MW2))/ PI2 - (0&
  &.010416666666666666D0*EL2*MW2*B1(x, MW2, GaugeXiW*MW2))/PI2 + (0.010416666666666666D0*EL2*GaugeXiW*MW2*B1(x, MW2, GaugeXiW*MW2&
  &))/ PI2 - (0.010416666666666666D0*EL2*x*B1(x, MW2, GaugeXiW*MW2))/PI2 + (0.010416666666666666D0*EL2*x*B1(x, GaugeXiW*MW2, Gaug&
  &eXiW*MW2))/PI2

  totalAmplitude = (0D0,0D0)
 do j=1,19
  totalAmplitude = totalAmplitude + amplitudes(j)
 end do
 SelfAAAlter = totalAmplitude
end function SelfAAAlter

