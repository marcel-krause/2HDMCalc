double complex function SelfAZ0AlterZeroMom(x)
 use constants
 implicit none
#include "looptools.h"
 double precision, intent(in) :: x
 integer :: j
 double complex :: totalAmplitude
 double complex :: amplitudes(19)

 amplitudes(1) = (0.0625D0*EL2*(CW2 - 1.D0*SW2)*A0(MHp2))/(CW*PI2*SW)

 amplitudes(2) = (0.0625D0*EL2*(CW2 - 1.D0*SW2)*A0(GaugeXiW*MW2))/(CW*PI2*SW)

 amplitudes(3) = (0.015625D0*CW*EL2*(18.D0*A0(MW2) + 6.D0*GaugeXiW*A0(GaugeXiW*MW2) - 1.D0*MW2*(15.D0 + DBLE(GaugeXiW**INT(2.D0))&
  &)))/(PI2*SW)

 amplitudes(4) = (-0.041666666666666664D0*EL2*(-1.D0 + 4.D0*SW2)*(-1.D0*A0(ME2) + ME2*(1.D0 + B0(0.D0, ME2, ME2))))/(CW*PI2*SW)

 amplitudes(5) = (-0.041666666666666664D0*EL2*(-1.D0 + 4.D0*SW2)*(-1.D0*A0(MM2) + MM2*(1.D0 + B0(0.D0, MM2, MM2))))/(CW*PI2*SW)

 amplitudes(6) = (-0.041666666666666664D0*EL2*(-1.D0 + 4.D0*SW2)*(-1.D0*A0(ML2) + ML2*(1.D0 + B0(0.D0, ML2, ML2))))/(CW*PI2*SW)

 amplitudes(7) = (-0.027777777777777776D0*EL2*(-3.D0 + 8.D0*SW2)*(-1.D0*A0(MU2) + MU2*(1.D0 + B0(0.D0, MU2, MU2))))/(CW*PI2*SW)

 amplitudes(8) = (-0.027777777777777776D0*EL2*(-3.D0 + 8.D0*SW2)*(-1.D0*A0(MC2) + MC2*(1.D0 + B0(0.D0, MC2, MC2))))/(CW*PI2*SW)

 amplitudes(9) = (-0.027777777777777776D0*EL2*(-3.D0 + 8.D0*SW2)*(-1.D0*A0(MT2) + MT2*(1.D0 + B0(0.D0, MT2, MT2))))/(CW*PI2*SW)

 amplitudes(10) = (-0.013888888888888888D0*EL2*(-3.D0 + 4.D0*SW2)*(-1.D0*A0(MD2) + MD2*(1.D0 + B0(0.D0, MD2, MD2))))/(CW*PI2*SW)

 amplitudes(11) = (-0.013888888888888888D0*EL2*(-3.D0 + 4.D0*SW2)*(-1.D0*A0(MS2) + MS2*(1.D0 + B0(0.D0, MS2, MS2))))/(CW*PI2*SW)

 amplitudes(12) = (-0.013888888888888888D0*EL2*(-3.D0 + 4.D0*SW2)*(-1.D0*A0(MB2) + MB2*(1.D0 + B0(0.D0, MB2, MB2))))/(CW*PI2*SW)

 amplitudes(13) = (-0.020833333333333332D0*EL2*(CW2 - 1.D0*SW2)*(A0(MHp2) + 2.D0*MHp2*(1.D0 + B0(0.D0, MHp2, MHp2))))/(CW*PI2*SW)

 amplitudes(14) = (-0.020833333333333332D0*EL2*(CW2 - 1.D0*SW2)*(A0(GaugeXiW*MW2) + 2.D0*GaugeXiW*MW2*(1.D0 + B0(0.D0, GaugeXiW*M&
  &W2, GaugeXiW*MW2))))/(CW*PI2*SW)

 amplitudes(15) = (0.010416666666666666D0*CW*EL2*(A0(GaugeXiW*MW2) + 2.D0*GaugeXiW*MW2*(1.D0 + B0(0.D0, GaugeXiW*MW2, GaugeXiW*MW&
  &2))))/(PI2*SW)

 amplitudes(16) = (0.010416666666666666D0*CW*EL2*(A0(GaugeXiW*MW2) + 2.D0*GaugeXiW*MW2*(1.D0 + B0(0.D0, GaugeXiW*MW2, GaugeXiW*MW&
  &2))))/(PI2*SW)

 amplitudes(17) = (-0.005208333333333333D0*CW*EL2*(6.D0*A0(MW2) + 2.D0*(10.D0 + 9.D0*GaugeXiW)*A0(GaugeXiW*MW2) + MW2*(-1.D0 - 4.&
  &D0*GaugeXiW + 48.D0*B0(0.D0, MW2, MW2) + 16.D0*B0(0.D0, MW2, GaugeXiW*MW2) - 4.D0*B1(0.D0, MW2, GaugeXiW*MW2) + 4.D0*GaugeXiW*&
  &B1(0.D0, MW2, GaugeXiW*MW2) - 3.D0*DBLE(GaugeXiW**INT(2.D0)))))/(PI2*SW)

 amplitudes(18) = (-0.010416666666666666D0*EL2*MW2*SW*(-1.D0 + GaugeXiW + 4.D0*B0(0.D0, MW2, GaugeXiW*MW2) + 2.D0*GaugeXiW*B0(0.D&
  &0, GaugeXiW*MW2, GaugeXiW*MW2) + (-1.D0 + GaugeXiW)*B1(0.D0, MW2, GaugeXiW*MW2)))/(CW*PI2)

 amplitudes(19) = (-0.010416666666666666D0*EL2*MW2*SW*(-1.D0 + GaugeXiW + 4.D0*B0(0.D0, MW2, GaugeXiW*MW2) + 2.D0*GaugeXiW*B0(0.D&
  &0, GaugeXiW*MW2, GaugeXiW*MW2) + (-1.D0 + GaugeXiW)*B1(0.D0, MW2, GaugeXiW*MW2)))/(CW*PI2)

  totalAmplitude = (0D0,0D0)
 do j=1,19
  totalAmplitude = totalAmplitude + amplitudes(j)
 end do
 SelfAZ0AlterZeroMom = totalAmplitude
end function SelfAZ0AlterZeroMom

