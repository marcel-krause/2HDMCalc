double complex function SelfUTLeftAlter(x)
 use constants
 implicit none
#include "looptools.h"
 double precision, intent(in) :: x
 integer :: j
 double complex :: totalAmplitude
 double complex :: amplitudes(9)

 amplitudes(1) = (0.015625D0*CKM31*CKMC11*EL2*(-1.D0*MD2*TB2*A0(MD2)*DBLE(Yuk3**INT(2.D0)) + MD2*TB2*A0(MHp2)*DBLE(Yuk3**INT(2.D0&
  &)) - 1.D0*MD2*MHp2*TB2*B0(x, MD2, MHp2)*DBLE(Yuk3**INT(2.D0)) + MD2*TB2*x*B0(x, MD2, MHp2)*DBLE(Yuk3**INT(2.D0)) + TB2*B0(x, M&
  &D2, MHp2)*DBLE(MD**INT(4.D0))*DBLE(Yuk3**INT(2.D0))))/(MW2*PI2*SW2*TB2*x)

 amplitudes(2) = (0.015625D0*CKM32*CKMC12*EL2*(MS2*TB2*A0(MHp2)*DBLE(Yuk3**INT(2.D0)) - 1.D0*MS2*TB2*A0(MS2)*DBLE(Yuk3**INT(2.D0)&
  &) - 1.D0*MHp2*MS2*TB2*B0(x, MHp2, MS2)*DBLE(Yuk3**INT(2.D0)) + MS2*TB2*x*B0(x, MHp2, MS2)*DBLE(Yuk3**INT(2.D0)) + TB2*B0(x, MH&
  &p2, MS2)*DBLE(MS**INT(4.D0))*DBLE(Yuk3**INT(2.D0))))/(MW2*PI2*SW2*TB2*x)

 amplitudes(3) = (0.015625D0*CKM33*CKMC13*EL2*(-1.D0*MB2*TB2*A0(MB2)*DBLE(Yuk3**INT(2.D0)) + MB2*TB2*A0(MHp2)*DBLE(Yuk3**INT(2.D0&
  &)) - 1.D0*MB2*MHp2*TB2*B0(x, MB2, MHp2)*DBLE(Yuk3**INT(2.D0)) + MB2*TB2*x*B0(x, MB2, MHp2)*DBLE(Yuk3**INT(2.D0)) + TB2*B0(x, M&
  &B2, MHp2)*DBLE(MB**INT(4.D0))*DBLE(Yuk3**INT(2.D0))))/(MW2*PI2*SW2*TB2*x)

 amplitudes(4) = (0.015625D0*CKM31*CKMC11*EL2*(-1.D0*MD2*A0(MD2) + MD2*A0(GaugeXiW*MW2) - 1.D0*GaugeXiW*MD2*MW2*B0(x, MD2, GaugeX&
  &iW*MW2) + MD2*x*B0(x, MD2, GaugeXiW*MW2) + B0(x, MD2, GaugeXiW*MW2)*DBLE(MD**INT(4.D0))))/(MW2*PI2*SW2*x)

 amplitudes(5) = (0.015625D0*CKM32*CKMC12*EL2*(-1.D0*MS2*A0(MS2) + MS2*A0(GaugeXiW*MW2) - 1.D0*GaugeXiW*MS2*MW2*B0(x, MS2, GaugeX&
  &iW*MW2) + MS2*x*B0(x, MS2, GaugeXiW*MW2) + B0(x, MS2, GaugeXiW*MW2)*DBLE(MS**INT(4.D0))))/(MW2*PI2*SW2*x)

 amplitudes(6) = (0.015625D0*CKM33*CKMC13*EL2*(-1.D0*MB2*A0(MB2) + MB2*A0(GaugeXiW*MW2) - 1.D0*GaugeXiW*MB2*MW2*B0(x, MB2, GaugeX&
  &iW*MW2) + MB2*x*B0(x, MB2, GaugeXiW*MW2) + B0(x, MB2, GaugeXiW*MW2)*DBLE(MB**INT(4.D0))))/(MW2*PI2*SW2*x)

 amplitudes(7) = (0.015625D0*CKM31*CKMC11*EL2*(-2.D0*MW2*x - 2.D0*MW2*A0(MD2) + (MD2 + 2.D0*MW2 - 1.D0*x)*A0(MW2) - 1.D0*MD2*A0(G&
  &augeXiW*MW2) + x*A0(GaugeXiW*MW2) + MD2*MW2*B0(x, MD2, MW2) - 2.D0*MD2*x*B0(x, MD2, MW2) + MW2*x*B0(x, MD2, MW2) + GaugeXiW*MD&
  &2*MW2*B0(x, MD2, GaugeXiW*MW2) + 2.D0*MD2*x*B0(x, MD2, GaugeXiW*MW2) + GaugeXiW*MW2*x*B0(x, MD2, GaugeXiW*MW2) + B0(x, MD2, MW&
  &2)*DBLE(MD**INT(4.D0)) - 1.D0*B0(x, MD2, GaugeXiW*MW2)*DBLE(MD**INT(4.D0)) - 2.D0*B0(x, MD2, MW2)*DBLE(MW**INT(4.D0)) + B0(x, &
  &MD2, MW2)*DBLE(x**INT(2.D0)) - 1.D0*B0(x, MD2, GaugeXiW*MW2)*DBLE(x**INT(2.D0))))/(MW2*PI2*SW2*x)

 amplitudes(8) = (0.015625D0*CKM32*CKMC12*EL2*(-2.D0*MW2*x - 2.D0*MW2*A0(MS2) + (MS2 + 2.D0*MW2 - 1.D0*x)*A0(MW2) - 1.D0*MS2*A0(G&
  &augeXiW*MW2) + x*A0(GaugeXiW*MW2) + MS2*MW2*B0(x, MS2, MW2) - 2.D0*MS2*x*B0(x, MS2, MW2) + MW2*x*B0(x, MS2, MW2) + GaugeXiW*MS&
  &2*MW2*B0(x, MS2, GaugeXiW*MW2) + 2.D0*MS2*x*B0(x, MS2, GaugeXiW*MW2) + GaugeXiW*MW2*x*B0(x, MS2, GaugeXiW*MW2) + B0(x, MS2, MW&
  &2)*DBLE(MS**INT(4.D0)) - 1.D0*B0(x, MS2, GaugeXiW*MW2)*DBLE(MS**INT(4.D0)) - 2.D0*B0(x, MS2, MW2)*DBLE(MW**INT(4.D0)) + B0(x, &
  &MS2, MW2)*DBLE(x**INT(2.D0)) - 1.D0*B0(x, MS2, GaugeXiW*MW2)*DBLE(x**INT(2.D0))))/(MW2*PI2*SW2*x)

 amplitudes(9) = (0.015625D0*CKM33*CKMC13*EL2*(-2.D0*MW2*x - 2.D0*MW2*A0(MB2) + (MB2 + 2.D0*MW2 - 1.D0*x)*A0(MW2) - 1.D0*MB2*A0(G&
  &augeXiW*MW2) + x*A0(GaugeXiW*MW2) + MB2*MW2*B0(x, MB2, MW2) - 2.D0*MB2*x*B0(x, MB2, MW2) + MW2*x*B0(x, MB2, MW2) + GaugeXiW*MB&
  &2*MW2*B0(x, MB2, GaugeXiW*MW2) + 2.D0*MB2*x*B0(x, MB2, GaugeXiW*MW2) + GaugeXiW*MW2*x*B0(x, MB2, GaugeXiW*MW2) + B0(x, MB2, MW&
  &2)*DBLE(MB**INT(4.D0)) - 1.D0*B0(x, MB2, GaugeXiW*MW2)*DBLE(MB**INT(4.D0)) - 2.D0*B0(x, MB2, MW2)*DBLE(MW**INT(4.D0)) + B0(x, &
  &MB2, MW2)*DBLE(x**INT(2.D0)) - 1.D0*B0(x, MB2, GaugeXiW*MW2)*DBLE(x**INT(2.D0))))/(MW2*PI2*SW2*x)

  totalAmplitude = (0D0,0D0)
 do j=1,9
  totalAmplitude = totalAmplitude + amplitudes(j)
 end do
 SelfUTLeftAlter = totalAmplitude
end function SelfUTLeftAlter

