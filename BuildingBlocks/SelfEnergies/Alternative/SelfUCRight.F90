double complex function SelfUCRightAlter(x)
 use constants
 implicit none
#include "looptools.h"
 double precision, intent(in) :: x
 integer :: j
 double complex :: totalAmplitude
 double complex :: amplitudes(9)

 amplitudes(1) = (0.015625D0*CKM21*CKMC11*EL2*(-1.D0*MC*MU*A0(MD2) + MC*MU*A0(MHp2) + MC*MD2*MU*B0(x, MD2, MHp2) - 1.D0*MC*MHp2*M&
  &U*B0(x, MD2, MHp2) + MC*MU*x*B0(x, MD2, MHp2)))/(MW2*PI2*SW2*TB2*x)

 amplitudes(2) = (0.015625D0*CKM22*CKMC12*EL2*(MC*MU*A0(MHp2) - 1.D0*MC*MU*A0(MS2) - 1.D0*MC*MHp2*MU*B0(x, MHp2, MS2) + MC*MS2*MU&
  &*B0(x, MHp2, MS2) + MC*MU*x*B0(x, MHp2, MS2)))/(MW2*PI2*SW2*TB2*x)

 amplitudes(3) = (0.015625D0*CKM23*CKMC13*EL2*(-1.D0*MC*MU*A0(MB2) + MC*MU*A0(MHp2) + MB2*MC*MU*B0(x, MB2, MHp2) - 1.D0*MC*MHp2*M&
  &U*B0(x, MB2, MHp2) + MC*MU*x*B0(x, MB2, MHp2)))/(MW2*PI2*SW2*TB2*x)

 amplitudes(4) = (0.015625D0*CKM21*CKMC11*EL2*(-1.D0*MC*MU*A0(MD2) + MC*MU*A0(GaugeXiW*MW2) + MC*MD2*MU*B0(x, MD2, GaugeXiW*MW2) &
  &- 1.D0*GaugeXiW*MC*MU*MW2*B0(x, MD2, GaugeXiW*MW2) + MC*MU*x*B0(x, MD2, GaugeXiW*MW2)))/(MW2*PI2*SW2*x)

 amplitudes(5) = (0.015625D0*CKM22*CKMC12*EL2*(-1.D0*MC*MU*A0(MS2) + MC*MU*A0(GaugeXiW*MW2) + MC*MS2*MU*B0(x, MS2, GaugeXiW*MW2) &
  &- 1.D0*GaugeXiW*MC*MU*MW2*B0(x, MS2, GaugeXiW*MW2) + MC*MU*x*B0(x, MS2, GaugeXiW*MW2)))/(MW2*PI2*SW2*x)

 amplitudes(6) = (0.015625D0*CKM23*CKMC13*EL2*(-1.D0*MC*MU*A0(MB2) + MC*MU*A0(GaugeXiW*MW2) + MB2*MC*MU*B0(x, MB2, GaugeXiW*MW2) &
  &- 1.D0*GaugeXiW*MC*MU*MW2*B0(x, MB2, GaugeXiW*MW2) + MC*MU*x*B0(x, MB2, GaugeXiW*MW2)))/(MW2*PI2*SW2*x)

 amplitudes(7) = 0.D0

 amplitudes(8) = 0.D0

 amplitudes(9) = 0.D0

  totalAmplitude = (0D0,0D0)
 do j=1,9
  totalAmplitude = totalAmplitude + amplitudes(j)
 end do
 SelfUCRightAlter = totalAmplitude
end function SelfUCRightAlter

