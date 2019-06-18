double complex function DSelfCTRight(x)
 use constants
 implicit none
#include "looptools.h"
 double precision, intent(in) :: x
 integer :: j
 double complex :: totalAmplitude
 double complex :: amplitudes(9)

 amplitudes(1) = (0.015625D0*CKM31*CKMC21*EL2*(MC*MT*B0(x, MD2, MHp2) + MC*MD2*MT*DB0(x, MD2, MHp2) - 1.D0*MC*MHp2*MT*DB0(x, MD2,&
  & MHp2) + MC*MT*x*DB0(x, MD2, MHp2)))/(MW2*PI2*SW2*TB2*x) - (0.015625D0*CKM31*CKMC21*EL2*(-1.D0*MC*MT*A0(MD2) + MC*MT*A0(MHp2) &
  &+ MC*MD2*MT*B0(x, MD2, MHp2) - 1.D0*MC*MHp2*MT*B0(x, MD2, MHp2) + MC*MT*x*B0(x, MD2, MHp2))*DBLE(x**INT(-2.D0)))/(MW2*PI2*SW2*&
  &TB2)

 amplitudes(2) = (0.015625D0*CKM32*CKMC22*EL2*(MC*MT*B0(x, MHp2, MS2) - 1.D0*MC*MHp2*MT*DB0(x, MHp2, MS2) + MC*MS2*MT*DB0(x, MHp2&
  &, MS2) + MC*MT*x*DB0(x, MHp2, MS2)))/(MW2*PI2*SW2*TB2*x) - (0.015625D0*CKM32*CKMC22*EL2*(MC*MT*A0(MHp2) - 1.D0*MC*MT*A0(MS2) -&
  & 1.D0*MC*MHp2*MT*B0(x, MHp2, MS2) + MC*MS2*MT*B0(x, MHp2, MS2) + MC*MT*x*B0(x, MHp2, MS2))*DBLE(x**INT(-2.D0)))/(MW2*PI2*SW2*T&
  &B2)

 amplitudes(3) = (0.015625D0*CKM33*CKMC23*EL2*(MC*MT*B0(x, MB2, MHp2) + MB2*MC*MT*DB0(x, MB2, MHp2) - 1.D0*MC*MHp2*MT*DB0(x, MB2,&
  & MHp2) + MC*MT*x*DB0(x, MB2, MHp2)))/(MW2*PI2*SW2*TB2*x) - (0.015625D0*CKM33*CKMC23*EL2*(-1.D0*MC*MT*A0(MB2) + MC*MT*A0(MHp2) &
  &+ MB2*MC*MT*B0(x, MB2, MHp2) - 1.D0*MC*MHp2*MT*B0(x, MB2, MHp2) + MC*MT*x*B0(x, MB2, MHp2))*DBLE(x**INT(-2.D0)))/(MW2*PI2*SW2*&
  &TB2)

 amplitudes(4) = (0.015625D0*CKM31*CKMC21*EL2*(MC*MT*B0(x, MD2, GaugeXiW*MW2) + MC*MD2*MT*DB0(x, MD2, GaugeXiW*MW2) - 1.D0*GaugeX&
  &iW*MC*MT*MW2*DB0(x, MD2, GaugeXiW*MW2) + MC*MT*x*DB0(x, MD2, GaugeXiW*MW2)))/(MW2*PI2*SW2*x) - (0.015625D0*CKM31*CKMC21*EL2*(-&
  &1.D0*MC*MT*A0(MD2) + MC*MT*A0(GaugeXiW*MW2) + MC*MD2*MT*B0(x, MD2, GaugeXiW*MW2) - 1.D0*GaugeXiW*MC*MT*MW2*B0(x, MD2, GaugeXiW&
  &*MW2) + MC*MT*x*B0(x, MD2, GaugeXiW*MW2))*DBLE(x**INT(-2.D0)))/(MW2*PI2*SW2)

 amplitudes(5) = (0.015625D0*CKM32*CKMC22*EL2*(MC*MT*B0(x, MS2, GaugeXiW*MW2) + MC*MS2*MT*DB0(x, MS2, GaugeXiW*MW2) - 1.D0*GaugeX&
  &iW*MC*MT*MW2*DB0(x, MS2, GaugeXiW*MW2) + MC*MT*x*DB0(x, MS2, GaugeXiW*MW2)))/(MW2*PI2*SW2*x) - (0.015625D0*CKM32*CKMC22*EL2*(-&
  &1.D0*MC*MT*A0(MS2) + MC*MT*A0(GaugeXiW*MW2) + MC*MS2*MT*B0(x, MS2, GaugeXiW*MW2) - 1.D0*GaugeXiW*MC*MT*MW2*B0(x, MS2, GaugeXiW&
  &*MW2) + MC*MT*x*B0(x, MS2, GaugeXiW*MW2))*DBLE(x**INT(-2.D0)))/(MW2*PI2*SW2)

 amplitudes(6) = (0.015625D0*CKM33*CKMC23*EL2*(MC*MT*B0(x, MB2, GaugeXiW*MW2) + MB2*MC*MT*DB0(x, MB2, GaugeXiW*MW2) - 1.D0*GaugeX&
  &iW*MC*MT*MW2*DB0(x, MB2, GaugeXiW*MW2) + MC*MT*x*DB0(x, MB2, GaugeXiW*MW2)))/(MW2*PI2*SW2*x) - (0.015625D0*CKM33*CKMC23*EL2*(-&
  &1.D0*MC*MT*A0(MB2) + MC*MT*A0(GaugeXiW*MW2) + MB2*MC*MT*B0(x, MB2, GaugeXiW*MW2) - 1.D0*GaugeXiW*MC*MT*MW2*B0(x, MB2, GaugeXiW&
  &*MW2) + MC*MT*x*B0(x, MB2, GaugeXiW*MW2))*DBLE(x**INT(-2.D0)))/(MW2*PI2*SW2)

 amplitudes(7) = 0.D0

 amplitudes(8) = 0.D0

 amplitudes(9) = 0.D0

  totalAmplitude = (0D0,0D0)
 do j=1,9
  totalAmplitude = totalAmplitude + amplitudes(j)
 end do
 DSelfCTRight = totalAmplitude
end function DSelfCTRight

