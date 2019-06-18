double complex function DSelfCTLeft(x)
 use constants
 implicit none
#include "looptools.h"
 double precision, intent(in) :: x
 integer :: j
 double complex :: totalAmplitude
 double complex :: amplitudes(9)

 amplitudes(1) = (-0.015625D0*CKM31*CKMC21*EL2*DBLE(x**INT(-2.D0))*(-1.D0*MD2*TB2*A0(MD2)*DBLE(Yuk3**INT(2.D0)) + MD2*TB2*A0(MHp2&
  &)*DBLE(Yuk3**INT(2.D0)) - 1.D0*MD2*MHp2*TB2*B0(x, MD2, MHp2)*DBLE(Yuk3**INT(2.D0)) + MD2*TB2*x*B0(x, MD2, MHp2)*DBLE(Yuk3**INT&
  &(2.D0)) + TB2*B0(x, MD2, MHp2)*DBLE(MD**INT(4.D0))*DBLE(Yuk3**INT(2.D0))))/(MW2*PI2*SW2*TB2) + (0.015625D0*CKM31*CKMC21*EL2*(M&
  &D2*TB2*B0(x, MD2, MHp2)*DBLE(Yuk3**INT(2.D0)) - 1.D0*MD2*MHp2*TB2*DB0(x, MD2, MHp2)*DBLE(Yuk3**INT(2.D0)) + MD2*TB2*x*DB0(x, M&
  &D2, MHp2)*DBLE(Yuk3**INT(2.D0)) + TB2*DB0(x, MD2, MHp2)*DBLE(MD**INT(4.D0))*DBLE(Yuk3**INT(2.D0))))/(MW2*PI2*SW2*TB2*x)

 amplitudes(2) = (-0.015625D0*CKM32*CKMC22*EL2*DBLE(x**INT(-2.D0))*(MS2*TB2*A0(MHp2)*DBLE(Yuk3**INT(2.D0)) - 1.D0*MS2*TB2*A0(MS2)&
  &*DBLE(Yuk3**INT(2.D0)) - 1.D0*MHp2*MS2*TB2*B0(x, MHp2, MS2)*DBLE(Yuk3**INT(2.D0)) + MS2*TB2*x*B0(x, MHp2, MS2)*DBLE(Yuk3**INT(&
  &2.D0)) + TB2*B0(x, MHp2, MS2)*DBLE(MS**INT(4.D0))*DBLE(Yuk3**INT(2.D0))))/(MW2*PI2*SW2*TB2) + (0.015625D0*CKM32*CKMC22*EL2*(MS&
  &2*TB2*B0(x, MHp2, MS2)*DBLE(Yuk3**INT(2.D0)) - 1.D0*MHp2*MS2*TB2*DB0(x, MHp2, MS2)*DBLE(Yuk3**INT(2.D0)) + MS2*TB2*x*DB0(x, MH&
  &p2, MS2)*DBLE(Yuk3**INT(2.D0)) + TB2*DB0(x, MHp2, MS2)*DBLE(MS**INT(4.D0))*DBLE(Yuk3**INT(2.D0))))/(MW2*PI2*SW2*TB2*x)

 amplitudes(3) = (-0.015625D0*CKM33*CKMC23*EL2*DBLE(x**INT(-2.D0))*(-1.D0*MB2*TB2*A0(MB2)*DBLE(Yuk3**INT(2.D0)) + MB2*TB2*A0(MHp2&
  &)*DBLE(Yuk3**INT(2.D0)) - 1.D0*MB2*MHp2*TB2*B0(x, MB2, MHp2)*DBLE(Yuk3**INT(2.D0)) + MB2*TB2*x*B0(x, MB2, MHp2)*DBLE(Yuk3**INT&
  &(2.D0)) + TB2*B0(x, MB2, MHp2)*DBLE(MB**INT(4.D0))*DBLE(Yuk3**INT(2.D0))))/(MW2*PI2*SW2*TB2) + (0.015625D0*CKM33*CKMC23*EL2*(M&
  &B2*TB2*B0(x, MB2, MHp2)*DBLE(Yuk3**INT(2.D0)) - 1.D0*MB2*MHp2*TB2*DB0(x, MB2, MHp2)*DBLE(Yuk3**INT(2.D0)) + MB2*TB2*x*DB0(x, M&
  &B2, MHp2)*DBLE(Yuk3**INT(2.D0)) + TB2*DB0(x, MB2, MHp2)*DBLE(MB**INT(4.D0))*DBLE(Yuk3**INT(2.D0))))/(MW2*PI2*SW2*TB2*x)

 amplitudes(4) = (0.015625D0*CKM31*CKMC21*EL2*(MD2*B0(x, MD2, GaugeXiW*MW2) - 1.D0*GaugeXiW*MD2*MW2*DB0(x, MD2, GaugeXiW*MW2) + M&
  &D2*x*DB0(x, MD2, GaugeXiW*MW2) + DB0(x, MD2, GaugeXiW*MW2)*DBLE(MD**INT(4.D0))))/(MW2*PI2*SW2*x) - (0.015625D0*CKM31*CKMC21*EL&
  &2*(-1.D0*MD2*A0(MD2) + MD2*A0(GaugeXiW*MW2) - 1.D0*GaugeXiW*MD2*MW2*B0(x, MD2, GaugeXiW*MW2) + MD2*x*B0(x, MD2, GaugeXiW*MW2) &
  &+ B0(x, MD2, GaugeXiW*MW2)*DBLE(MD**INT(4.D0)))*DBLE(x**INT(-2.D0)))/(MW2*PI2*SW2)

 amplitudes(5) = (0.015625D0*CKM32*CKMC22*EL2*(MS2*B0(x, MS2, GaugeXiW*MW2) - 1.D0*GaugeXiW*MS2*MW2*DB0(x, MS2, GaugeXiW*MW2) + M&
  &S2*x*DB0(x, MS2, GaugeXiW*MW2) + DB0(x, MS2, GaugeXiW*MW2)*DBLE(MS**INT(4.D0))))/(MW2*PI2*SW2*x) - (0.015625D0*CKM32*CKMC22*EL&
  &2*(-1.D0*MS2*A0(MS2) + MS2*A0(GaugeXiW*MW2) - 1.D0*GaugeXiW*MS2*MW2*B0(x, MS2, GaugeXiW*MW2) + MS2*x*B0(x, MS2, GaugeXiW*MW2) &
  &+ B0(x, MS2, GaugeXiW*MW2)*DBLE(MS**INT(4.D0)))*DBLE(x**INT(-2.D0)))/(MW2*PI2*SW2)

 amplitudes(6) = (0.015625D0*CKM33*CKMC23*EL2*(MB2*B0(x, MB2, GaugeXiW*MW2) - 1.D0*GaugeXiW*MB2*MW2*DB0(x, MB2, GaugeXiW*MW2) + M&
  &B2*x*DB0(x, MB2, GaugeXiW*MW2) + DB0(x, MB2, GaugeXiW*MW2)*DBLE(MB**INT(4.D0))))/(MW2*PI2*SW2*x) - (0.015625D0*CKM33*CKMC23*EL&
  &2*(-1.D0*MB2*A0(MB2) + MB2*A0(GaugeXiW*MW2) - 1.D0*GaugeXiW*MB2*MW2*B0(x, MB2, GaugeXiW*MW2) + MB2*x*B0(x, MB2, GaugeXiW*MW2) &
  &+ B0(x, MB2, GaugeXiW*MW2)*DBLE(MB**INT(4.D0)))*DBLE(x**INT(-2.D0)))/(MW2*PI2*SW2)

 amplitudes(7) = (-0.015625D0*CKM31*CKMC21*EL2*DBLE(x**INT(-2.D0))*(-2.D0*MW2*x - 2.D0*MW2*A0(MD2) + (MD2 + 2.D0*MW2 - 1.D0*x)*A0&
  &(MW2) - 1.D0*MD2*A0(GaugeXiW*MW2) + x*A0(GaugeXiW*MW2) + MD2*MW2*B0(x, MD2, MW2) - 2.D0*MD2*x*B0(x, MD2, MW2) + MW2*x*B0(x, MD&
  &2, MW2) + GaugeXiW*MD2*MW2*B0(x, MD2, GaugeXiW*MW2) + 2.D0*MD2*x*B0(x, MD2, GaugeXiW*MW2) + GaugeXiW*MW2*x*B0(x, MD2, GaugeXiW&
  &*MW2) + B0(x, MD2, MW2)*DBLE(MD**INT(4.D0)) - 1.D0*B0(x, MD2, GaugeXiW*MW2)*DBLE(MD**INT(4.D0)) - 2.D0*B0(x, MD2, MW2)*DBLE(MW&
  &**INT(4.D0)) + B0(x, MD2, MW2)*DBLE(x**INT(2.D0)) - 1.D0*B0(x, MD2, GaugeXiW*MW2)*DBLE(x**INT(2.D0))))/(MW2*PI2*SW2) + (0.0156&
  &25D0*CKM31*CKMC21*EL2*(-2.D0*MW2 - 1.D0*A0(MW2) + A0(GaugeXiW*MW2) - 2.D0*MD2*B0(x, MD2, MW2) + MW2*B0(x, MD2, MW2) + 2.D0*x*B&
  &0(x, MD2, MW2) + 2.D0*MD2*B0(x, MD2, GaugeXiW*MW2) + GaugeXiW*MW2*B0(x, MD2, GaugeXiW*MW2) - 2.D0*x*B0(x, MD2, GaugeXiW*MW2) +&
  & MD2*MW2*DB0(x, MD2, MW2) - 2.D0*MD2*x*DB0(x, MD2, MW2) + MW2*x*DB0(x, MD2, MW2) + GaugeXiW*MD2*MW2*DB0(x, MD2, GaugeXiW*MW2) &
  &+ 2.D0*MD2*x*DB0(x, MD2, GaugeXiW*MW2) + GaugeXiW*MW2*x*DB0(x, MD2, GaugeXiW*MW2) + DB0(x, MD2, MW2)*DBLE(MD**INT(4.D0)) - 1.D&
  &0*DB0(x, MD2, GaugeXiW*MW2)*DBLE(MD**INT(4.D0)) - 2.D0*DB0(x, MD2, MW2)*DBLE(MW**INT(4.D0)) + DB0(x, MD2, MW2)*DBLE(x**INT(2.D&
  &0)) - 1.D0*DB0(x, MD2, GaugeXiW*MW2)*DBLE(x**INT(2.D0))))/(MW2*PI2*SW2*x)

 amplitudes(8) = (-0.015625D0*CKM32*CKMC22*EL2*DBLE(x**INT(-2.D0))*(-2.D0*MW2*x - 2.D0*MW2*A0(MS2) + (MS2 + 2.D0*MW2 - 1.D0*x)*A0&
  &(MW2) - 1.D0*MS2*A0(GaugeXiW*MW2) + x*A0(GaugeXiW*MW2) + MS2*MW2*B0(x, MS2, MW2) - 2.D0*MS2*x*B0(x, MS2, MW2) + MW2*x*B0(x, MS&
  &2, MW2) + GaugeXiW*MS2*MW2*B0(x, MS2, GaugeXiW*MW2) + 2.D0*MS2*x*B0(x, MS2, GaugeXiW*MW2) + GaugeXiW*MW2*x*B0(x, MS2, GaugeXiW&
  &*MW2) + B0(x, MS2, MW2)*DBLE(MS**INT(4.D0)) - 1.D0*B0(x, MS2, GaugeXiW*MW2)*DBLE(MS**INT(4.D0)) - 2.D0*B0(x, MS2, MW2)*DBLE(MW&
  &**INT(4.D0)) + B0(x, MS2, MW2)*DBLE(x**INT(2.D0)) - 1.D0*B0(x, MS2, GaugeXiW*MW2)*DBLE(x**INT(2.D0))))/(MW2*PI2*SW2) + (0.0156&
  &25D0*CKM32*CKMC22*EL2*(-2.D0*MW2 - 1.D0*A0(MW2) + A0(GaugeXiW*MW2) - 2.D0*MS2*B0(x, MS2, MW2) + MW2*B0(x, MS2, MW2) + 2.D0*x*B&
  &0(x, MS2, MW2) + 2.D0*MS2*B0(x, MS2, GaugeXiW*MW2) + GaugeXiW*MW2*B0(x, MS2, GaugeXiW*MW2) - 2.D0*x*B0(x, MS2, GaugeXiW*MW2) +&
  & MS2*MW2*DB0(x, MS2, MW2) - 2.D0*MS2*x*DB0(x, MS2, MW2) + MW2*x*DB0(x, MS2, MW2) + GaugeXiW*MS2*MW2*DB0(x, MS2, GaugeXiW*MW2) &
  &+ 2.D0*MS2*x*DB0(x, MS2, GaugeXiW*MW2) + GaugeXiW*MW2*x*DB0(x, MS2, GaugeXiW*MW2) + DB0(x, MS2, MW2)*DBLE(MS**INT(4.D0)) - 1.D&
  &0*DB0(x, MS2, GaugeXiW*MW2)*DBLE(MS**INT(4.D0)) - 2.D0*DB0(x, MS2, MW2)*DBLE(MW**INT(4.D0)) + DB0(x, MS2, MW2)*DBLE(x**INT(2.D&
  &0)) - 1.D0*DB0(x, MS2, GaugeXiW*MW2)*DBLE(x**INT(2.D0))))/(MW2*PI2*SW2*x)

 amplitudes(9) = (-0.015625D0*CKM33*CKMC23*EL2*DBLE(x**INT(-2.D0))*(-2.D0*MW2*x - 2.D0*MW2*A0(MB2) + (MB2 + 2.D0*MW2 - 1.D0*x)*A0&
  &(MW2) - 1.D0*MB2*A0(GaugeXiW*MW2) + x*A0(GaugeXiW*MW2) + MB2*MW2*B0(x, MB2, MW2) - 2.D0*MB2*x*B0(x, MB2, MW2) + MW2*x*B0(x, MB&
  &2, MW2) + GaugeXiW*MB2*MW2*B0(x, MB2, GaugeXiW*MW2) + 2.D0*MB2*x*B0(x, MB2, GaugeXiW*MW2) + GaugeXiW*MW2*x*B0(x, MB2, GaugeXiW&
  &*MW2) + B0(x, MB2, MW2)*DBLE(MB**INT(4.D0)) - 1.D0*B0(x, MB2, GaugeXiW*MW2)*DBLE(MB**INT(4.D0)) - 2.D0*B0(x, MB2, MW2)*DBLE(MW&
  &**INT(4.D0)) + B0(x, MB2, MW2)*DBLE(x**INT(2.D0)) - 1.D0*B0(x, MB2, GaugeXiW*MW2)*DBLE(x**INT(2.D0))))/(MW2*PI2*SW2) + (0.0156&
  &25D0*CKM33*CKMC23*EL2*(-2.D0*MW2 - 1.D0*A0(MW2) + A0(GaugeXiW*MW2) - 2.D0*MB2*B0(x, MB2, MW2) + MW2*B0(x, MB2, MW2) + 2.D0*x*B&
  &0(x, MB2, MW2) + 2.D0*MB2*B0(x, MB2, GaugeXiW*MW2) + GaugeXiW*MW2*B0(x, MB2, GaugeXiW*MW2) - 2.D0*x*B0(x, MB2, GaugeXiW*MW2) +&
  & MB2*MW2*DB0(x, MB2, MW2) - 2.D0*MB2*x*DB0(x, MB2, MW2) + MW2*x*DB0(x, MB2, MW2) + GaugeXiW*MB2*MW2*DB0(x, MB2, GaugeXiW*MW2) &
  &+ 2.D0*MB2*x*DB0(x, MB2, GaugeXiW*MW2) + GaugeXiW*MW2*x*DB0(x, MB2, GaugeXiW*MW2) + DB0(x, MB2, MW2)*DBLE(MB**INT(4.D0)) - 1.D&
  &0*DB0(x, MB2, GaugeXiW*MW2)*DBLE(MB**INT(4.D0)) - 2.D0*DB0(x, MB2, MW2)*DBLE(MW**INT(4.D0)) + DB0(x, MB2, MW2)*DBLE(x**INT(2.D&
  &0)) - 1.D0*DB0(x, MB2, GaugeXiW*MW2)*DBLE(x**INT(2.D0))))/(MW2*PI2*SW2*x)

  totalAmplitude = (0D0,0D0)
 do j=1,9
  totalAmplitude = totalAmplitude + amplitudes(j)
 end do
 DSelfCTLeft = totalAmplitude
end function DSelfCTLeft

