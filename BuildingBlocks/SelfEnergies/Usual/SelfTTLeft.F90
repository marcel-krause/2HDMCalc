double complex function SelfTTLeftUsual(x)
 use constants
 implicit none
#include "looptools.h"
 double precision, intent(in) :: x
 integer :: j
 double complex :: totalAmplitude
 double complex :: amplitudes(15)

 amplitudes(1) = (0.0078125D0*CA2*EL2*MT2*(A0(Mh02) - 1.D0*A0(MT2) - 1.D0*Mh02*B0(x, Mh02, MT2) + MT2*B0(x, Mh02, MT2) + x*B0(x, &
  &Mh02, MT2)))/ (MW2*PI2*SB2*SW2*x)

 amplitudes(2) = (0.0078125D0*EL2*MT2*SA2*(A0(MHH2) - 1.D0*A0(MT2) - 1.D0*MHH2*B0(x, MHH2, MT2) + MT2*B0(x, MHH2, MT2) + x*B0(x, &
  &MHH2, MT2)))/ (MW2*PI2*SB2*SW2*x)

 amplitudes(3) = (0.0078125D0*EL2*MT2*(A0(MA02) - 1.D0*A0(MT2) - 1.D0*MA02*B0(x, MA02, MT2) + MT2*B0(x, MA02, MT2) + x*B0(x, MA02&
  &, MT2)))/(MW2*PI2*SW2*TB2*x)

 amplitudes(4) = (0.0078125D0*EL2*MT2*(-1.D0*A0(MT2) + A0(GaugeXiZ*MZ2) + MT2*B0(x, MT2, GaugeXiZ*MZ2) - 1.D0*GaugeXiZ*MZ2*B0(x, &
  &MT2, GaugeXiZ*MZ2) + x*B0(x, MT2, GaugeXiZ*MZ2)))/(MW2*PI2*SW2*x)

 amplitudes(5) = (0.015625D0*CKM31*CKMC31*EL2*(-1.D0*MD2*TB2*A0(MD2)*DBLE(Yuk3**INT(2.D0)) + MD2*TB2*A0(MHp2)*DBLE(Yuk3**INT(2.D0&
  &)) - 1.D0*MD2*MHp2*TB2*B0(x, MD2, MHp2)*DBLE(Yuk3**INT(2.D0)) + MD2*TB2*x*B0(x, MD2, MHp2)*DBLE(Yuk3**INT(2.D0)) + TB2*B0(x, M&
  &D2, MHp2)*DBLE(MD**INT(4.D0))*DBLE(Yuk3**INT(2.D0))))/(MW2*PI2*SW2*TB2*x)

 amplitudes(6) = (0.015625D0*CKM32*CKMC32*EL2*(MS2*TB2*A0(MHp2)*DBLE(Yuk3**INT(2.D0)) - 1.D0*MS2*TB2*A0(MS2)*DBLE(Yuk3**INT(2.D0)&
  &) - 1.D0*MHp2*MS2*TB2*B0(x, MHp2, MS2)*DBLE(Yuk3**INT(2.D0)) + MS2*TB2*x*B0(x, MHp2, MS2)*DBLE(Yuk3**INT(2.D0)) + TB2*B0(x, MH&
  &p2, MS2)*DBLE(MS**INT(4.D0))*DBLE(Yuk3**INT(2.D0))))/(MW2*PI2*SW2*TB2*x)

 amplitudes(7) = (0.015625D0*CKM33*CKMC33*EL2*(-1.D0*MB2*TB2*A0(MB2)*DBLE(Yuk3**INT(2.D0)) + MB2*TB2*A0(MHp2)*DBLE(Yuk3**INT(2.D0&
  &)) - 1.D0*MB2*MHp2*TB2*B0(x, MB2, MHp2)*DBLE(Yuk3**INT(2.D0)) + MB2*TB2*x*B0(x, MB2, MHp2)*DBLE(Yuk3**INT(2.D0)) + TB2*B0(x, M&
  &B2, MHp2)*DBLE(MB**INT(4.D0))*DBLE(Yuk3**INT(2.D0))))/(MW2*PI2*SW2*TB2*x)

 amplitudes(8) = (0.015625D0*CKM31*CKMC31*EL2*(-1.D0*MD2*A0(MD2) + MD2*A0(GaugeXiW*MW2) - 1.D0*GaugeXiW*MD2*MW2*B0(x, MD2, GaugeX&
  &iW*MW2) + MD2*x*B0(x, MD2, GaugeXiW*MW2) + B0(x, MD2, GaugeXiW*MW2)*DBLE(MD**INT(4.D0))))/(MW2*PI2*SW2*x)

 amplitudes(9) = (0.015625D0*CKM32*CKMC32*EL2*(-1.D0*MS2*A0(MS2) + MS2*A0(GaugeXiW*MW2) - 1.D0*GaugeXiW*MS2*MW2*B0(x, MS2, GaugeX&
  &iW*MW2) + MS2*x*B0(x, MS2, GaugeXiW*MW2) + B0(x, MS2, GaugeXiW*MW2)*DBLE(MS**INT(4.D0))))/(MW2*PI2*SW2*x)

 amplitudes(10) = (0.015625D0*CKM33*CKMC33*EL2*(-1.D0*MB2*A0(MB2) + MB2*A0(GaugeXiW*MW2) - 1.D0*GaugeXiW*MB2*MW2*B0(x, MB2, Gauge&
  &XiW*MW2) + MB2*x*B0(x, MB2, GaugeXiW*MW2) + B0(x, MB2, GaugeXiW*MW2)*DBLE(MB**INT(4.D0))))/(MW2*PI2*SW2*x)

 amplitudes(11) = (0.013888888888888888D0*EL2*(-2.D0*x - 2.D0*A0(MT2) + MT2*B0(x, 0.D0, MT2) + GaugeXiA*MT2*B0(x, 0.D0, MT2) + x*&
  &B0(x, 0.D0, MT2) + GaugeXiA*x*B0(x, 0.D0, MT2) - 2.D0*MT2*x*C0Mine(DBLE(0.D0), DBLE(x), DBLE(x), DBLE(0.D0), DBLE(0.D0), DBLE(&
  &MT2)) + 2.D0*GaugeXiA*MT2*x*C0Mine(DBLE(0.D0), DBLE(x), DBLE(x), DBLE(0.D0), DBLE(0.D0), DBLE(MT2)) + C0Mine(DBLE(0.D0), DBLE(&
  &x), DBLE(x), DBLE(0.D0), DBLE(0.D0), DBLE(MT2))*DBLE(MT**INT(4.D0)) - 1.D0*GaugeXiA*C0Mine(DBLE(0.D0), DBLE(x), DBLE(x), DBLE(&
  &0.D0), DBLE(0.D0), DBLE(MT2))*DBLE(MT**INT(4.D0)) + C0Mine(DBLE(0.D0), DBLE(x), DBLE(x), DBLE(0.D0), DBLE(0.D0), DBLE(MT2))*DB&
  &LE(x**INT(2.D0)) - 1.D0*GaugeXiA*C0Mine(DBLE(0.D0), DBLE(x), DBLE(x), DBLE(0.D0), DBLE(0.D0), DBLE(MT2))*DBLE(x**INT(2.D0))))/&
  &(PI2*x)

 amplitudes(12) = (0.0008680555555555555D0*EL2*(-18.D0*MZ2*x + 48.D0*MZ2*SW2*x - 18.D0*MZ2*A0(MT2) + 48.D0*MZ2*SW2*A0(MT2) + 9.D0&
  &*MT2*A0(MZ2) + 18.D0*MZ2*A0(MZ2) - 24.D0*MT2*SW2*A0(MZ2) - 48.D0*MZ2*SW2*A0(MZ2) - 9.D0*x*A0(MZ2) + 24.D0*SW2*x*A0(MZ2) - 9.D0&
  &*MT2*A0(GaugeXiZ*MZ2) + 24.D0*MT2*SW2*A0(GaugeXiZ*MZ2) + 9.D0*x*A0(GaugeXiZ*MZ2) - 24.D0*SW2*x*A0(GaugeXiZ*MZ2) + 9.D0*MT2*MZ2&
  &*B0(x, MT2, MZ2) - 24.D0*MT2*MZ2*SW2*B0(x, MT2, MZ2) - 18.D0*MT2*x*B0(x, MT2, MZ2) + 9.D0*MZ2*x*B0(x, MT2, MZ2) + 48.D0*MT2*SW&
  &2*x*B0(x, MT2, MZ2) - 24.D0*MZ2*SW2*x*B0(x, MT2, MZ2) + 9.D0*GaugeXiZ*MT2*MZ2*B0(x, MT2, GaugeXiZ*MZ2) - 24.D0*GaugeXiZ*MT2*MZ&
  &2*SW2*B0(x, MT2, GaugeXiZ*MZ2) + 18.D0*MT2*x*B0(x, MT2, GaugeXiZ*MZ2) + 9.D0*GaugeXiZ*MZ2*x*B0(x, MT2, GaugeXiZ*MZ2) - 48.D0*M&
  &T2*SW2*x*B0(x, MT2, GaugeXiZ*MZ2) - 24.D0*GaugeXiZ*MZ2*SW2*x*B0(x, MT2, GaugeXiZ*MZ2) + 9.D0*B0(x, MT2, MZ2)*DBLE(MT**INT(4.D0&
  &)) - 24.D0*SW2*B0(x, MT2, MZ2)*DBLE(MT**INT(4.D0)) - 9.D0*B0(x, MT2, GaugeXiZ*MZ2)*DBLE(MT**INT(4.D0)) + 24.D0*SW2*B0(x, MT2, &
  &GaugeXiZ*MZ2)*DBLE(MT**INT(4.D0)) - 18.D0*B0(x, MT2, MZ2)*DBLE(MZ**INT(4.D0)) + 48.D0*SW2*B0(x, MT2, MZ2)*DBLE(MZ**INT(4.D0)) &
  &- 32.D0*MZ2*x*DBLE(SW**INT(4.D0)) - 32.D0*MZ2*A0(MT2)*DBLE(SW**INT(4.D0)) + 16.D0*MT2*A0(MZ2)*DBLE(SW**INT(4.D0)) + 32.D0*MZ2*&
  &A0(MZ2)*DBLE(SW**INT(4.D0)) - 16.D0*x*A0(MZ2)*DBLE(SW**INT(4.D0)) - 16.D0*MT2*A0(GaugeXiZ*MZ2)*DBLE(SW**INT(4.D0)) + 16.D0*x*A&
  &0(GaugeXiZ*MZ2)*DBLE(SW**INT(4.D0)) + 16.D0*MT2*MZ2*B0(x, MT2, MZ2)*DBLE(SW**INT(4.D0)) - 32.D0*MT2*x*B0(x, MT2, MZ2)*DBLE(SW*&
  &*INT(4.D0)) + 16.D0*MZ2*x*B0(x, MT2, MZ2)*DBLE(SW**INT(4.D0)) + 16.D0*GaugeXiZ*MT2*MZ2*B0(x, MT2, GaugeXiZ*MZ2)*DBLE(SW**INT(4&
  &.D0)) + 32.D0*MT2*x*B0(x, MT2, GaugeXiZ*MZ2)*DBLE(SW**INT(4.D0)) + 16.D0*GaugeXiZ*MZ2*x*B0(x, MT2, GaugeXiZ*MZ2)*DBLE(SW**INT(&
  &4.D0)) + 16.D0*B0(x, MT2, MZ2)*DBLE(MT**INT(4.D0))*DBLE(SW**INT(4.D0)) - 16.D0*B0(x, MT2, GaugeXiZ*MZ2)*DBLE(MT**INT(4.D0))*DB&
  &LE(SW**INT(4.D0)) - 32.D0*B0(x, MT2, MZ2)*DBLE(MZ**INT(4.D0))*DBLE(SW**INT(4.D0)) + 9.D0*B0(x, MT2, MZ2)*DBLE(x**INT(2.D0)) - &
  &24.D0*SW2*B0(x, MT2, MZ2)*DBLE(x**INT(2.D0)) - 9.D0*B0(x, MT2, GaugeXiZ*MZ2)*DBLE(x**INT(2.D0)) + 24.D0*SW2*B0(x, MT2, GaugeXi&
  &Z*MZ2)*DBLE(x**INT(2.D0)) + 16.D0*B0(x, MT2, MZ2)*DBLE(SW**INT(4.D0))*DBLE(x**INT(2.D0)) - 16.D0*B0(x, MT2, GaugeXiZ*MZ2)*DBLE&
  &(SW**INT(4.D0))*DBLE(x**INT(2.D0))))/(CW2*MZ2*PI2*SW2*x)

 amplitudes(13) = (0.015625D0*CKM31*CKMC31*EL2*(-2.D0*MW2*x - 2.D0*MW2*A0(MD2) + (MD2 + 2.D0*MW2 - 1.D0*x)*A0(MW2) - 1.D0*MD2*A0(&
  &GaugeXiW*MW2) + x*A0(GaugeXiW*MW2) + MD2*MW2*B0(x, MD2, MW2) - 2.D0*MD2*x*B0(x, MD2, MW2) + MW2*x*B0(x, MD2, MW2) + GaugeXiW*M&
  &D2*MW2*B0(x, MD2, GaugeXiW*MW2) + 2.D0*MD2*x*B0(x, MD2, GaugeXiW*MW2) + GaugeXiW*MW2*x*B0(x, MD2, GaugeXiW*MW2) + B0(x, MD2, M&
  &W2)*DBLE(MD**INT(4.D0)) - 1.D0*B0(x, MD2, GaugeXiW*MW2)*DBLE(MD**INT(4.D0)) - 2.D0*B0(x, MD2, MW2)*DBLE(MW**INT(4.D0)) + B0(x,&
  & MD2, MW2)*DBLE(x**INT(2.D0)) - 1.D0*B0(x, MD2, GaugeXiW*MW2)*DBLE(x**INT(2.D0))))/(MW2*PI2*SW2*x)

 amplitudes(14) = (0.015625D0*CKM32*CKMC32*EL2*(-2.D0*MW2*x - 2.D0*MW2*A0(MS2) + (MS2 + 2.D0*MW2 - 1.D0*x)*A0(MW2) - 1.D0*MS2*A0(&
  &GaugeXiW*MW2) + x*A0(GaugeXiW*MW2) + MS2*MW2*B0(x, MS2, MW2) - 2.D0*MS2*x*B0(x, MS2, MW2) + MW2*x*B0(x, MS2, MW2) + GaugeXiW*M&
  &S2*MW2*B0(x, MS2, GaugeXiW*MW2) + 2.D0*MS2*x*B0(x, MS2, GaugeXiW*MW2) + GaugeXiW*MW2*x*B0(x, MS2, GaugeXiW*MW2) + B0(x, MS2, M&
  &W2)*DBLE(MS**INT(4.D0)) - 1.D0*B0(x, MS2, GaugeXiW*MW2)*DBLE(MS**INT(4.D0)) - 2.D0*B0(x, MS2, MW2)*DBLE(MW**INT(4.D0)) + B0(x,&
  & MS2, MW2)*DBLE(x**INT(2.D0)) - 1.D0*B0(x, MS2, GaugeXiW*MW2)*DBLE(x**INT(2.D0))))/(MW2*PI2*SW2*x)

 amplitudes(15) = (0.015625D0*CKM33*CKMC33*EL2*(-2.D0*MW2*x - 2.D0*MW2*A0(MB2) + (MB2 + 2.D0*MW2 - 1.D0*x)*A0(MW2) - 1.D0*MB2*A0(&
  &GaugeXiW*MW2) + x*A0(GaugeXiW*MW2) + MB2*MW2*B0(x, MB2, MW2) - 2.D0*MB2*x*B0(x, MB2, MW2) + MW2*x*B0(x, MB2, MW2) + GaugeXiW*M&
  &B2*MW2*B0(x, MB2, GaugeXiW*MW2) + 2.D0*MB2*x*B0(x, MB2, GaugeXiW*MW2) + GaugeXiW*MW2*x*B0(x, MB2, GaugeXiW*MW2) + B0(x, MB2, M&
  &W2)*DBLE(MB**INT(4.D0)) - 1.D0*B0(x, MB2, GaugeXiW*MW2)*DBLE(MB**INT(4.D0)) - 2.D0*B0(x, MB2, MW2)*DBLE(MW**INT(4.D0)) + B0(x,&
  & MB2, MW2)*DBLE(x**INT(2.D0)) - 1.D0*B0(x, MB2, GaugeXiW*MW2)*DBLE(x**INT(2.D0))))/(MW2*PI2*SW2*x)

  totalAmplitude = (0D0,0D0)
 do j=1,15
  totalAmplitude = totalAmplitude + amplitudes(j)
 end do
 SelfTTLeftUsual = totalAmplitude
end function SelfTTLeftUsual

