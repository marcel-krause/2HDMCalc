double complex function DSelfCCScalar(x)
 use constants
 implicit none
#include "looptools.h"
 double precision, intent(in) :: x
 integer :: j
 double complex :: totalAmplitude
 double complex :: amplitudes(15)

 amplitudes(1) = (0.015625D0*CA2*EL2*MC2*DB0(x, MC2, Mh02))/(MW2*PI2*SB2*SW2)

 amplitudes(2) = (0.015625D0*EL2*MC2*SA2*DB0(x, MC2, MHH2))/(MW2*PI2*SB2*SW2)

 amplitudes(3) = (-0.015625D0*EL2*MC2*DB0(x, MA02, MC2))/(MW2*PI2*SW2*TB2)

 amplitudes(4) = (-0.015625D0*EL2*MC2*DB0(x, MC2, GaugeXiZ*MZ2))/(MW2*PI2*SW2)

 amplitudes(5) = (0.03125D0*CKM21*CKMC21*EL2*MD2*Yuk3*DB0(x, MD2, MHp2))/(MW2*PI2*SW2*TB)

 amplitudes(6) = (0.03125D0*CKM22*CKMC22*EL2*MS2*Yuk3*DB0(x, MHp2, MS2))/(MW2*PI2*SW2*TB)

 amplitudes(7) = (0.03125D0*CKM23*CKMC23*EL2*MB2*Yuk3*DB0(x, MB2, MHp2))/(MW2*PI2*SW2*TB)

 amplitudes(8) = (-0.03125D0*CKM21*CKMC21*EL2*MD2*DB0(x, MD2, GaugeXiW*MW2))/(MW2*PI2*SW2)

 amplitudes(9) = (-0.03125D0*CKM22*CKMC22*EL2*MS2*DB0(x, MS2, GaugeXiW*MW2))/(MW2*PI2*SW2)

 amplitudes(10) = (-0.03125D0*CKM23*CKMC23*EL2*MB2*DB0(x, MB2, GaugeXiW*MW2))/(MW2*PI2*SW2)

 amplitudes(11) = (-0.027777777777777776D0*EL2*(3.D0 + GaugeXiA)*DB0(x, 0.D0, MC2))/PI2

 amplitudes(12) = (-0.006944444444444444D0*EL2*(-3.D0 + 4.D0*SW2)*(3.D0*DB0(x, MC2, MZ2) + GaugeXiZ*DB0(x, MC2, GaugeXiZ*MZ2)))/(&
  &CW2*PI2)

 amplitudes(13) = 0.D0

 amplitudes(14) = 0.D0

 amplitudes(15) = 0.D0

  totalAmplitude = (0D0,0D0)
 do j=1,15
  totalAmplitude = totalAmplitude + amplitudes(j)
 end do
 DSelfCCScalar = totalAmplitude
end function DSelfCCScalar

