double complex function SelfSSLeftUsual(x)
 use constants
 implicit none
#include "looptools.h"
 double precision, intent(in) :: x
 integer :: j
 double complex :: totalAmplitude
 double complex :: amplitudes(15)

 amplitudes(1) = (0.0078125D0*EL2*MS2*(A0(Mh02) - 1.D0*A0(MS2) - 1.D0*Mh02*B0(x, Mh02, MS2) + MS2*B0(x, Mh02, MS2) + x*B0(x, Mh02&
  &, MS2))*DBLE(Yuk1**INT(2.D0)))/ (MW2*PI2*SW2*x)

 amplitudes(2) = (0.0078125D0*EL2*MS2*(A0(MHH2) - 1.D0*A0(MS2) - 1.D0*MHH2*B0(x, MHH2, MS2) + MS2*B0(x, MHH2, MS2) + x*B0(x, MHH2&
  &, MS2))*DBLE(Yuk2**INT(2.D0)))/ (MW2*PI2*SW2*x)

 amplitudes(3) = (0.0078125D0*EL2*MS2*(A0(MA02) - 1.D0*A0(MS2) - 1.D0*MA02*B0(x, MA02, MS2) + MS2*B0(x, MA02, MS2) + x*B0(x, MA02&
  &, MS2))*DBLE(Yuk3**INT(2.D0)))/ (MW2*PI2*SW2*x)

 amplitudes(4) = (0.0078125D0*EL2*MS2*(-1.D0*A0(MS2) + A0(GaugeXiZ*MZ2) + MS2*B0(x, MS2, GaugeXiZ*MZ2) - 1.D0*GaugeXiZ*MZ2*B0(x, &
  &MS2, GaugeXiZ*MZ2) + x*B0(x, MS2, GaugeXiZ*MZ2)))/(MW2*PI2*SW2*x)

 amplitudes(5) = (0.015625D0*CKM12*CKMC12*EL2*(MU2*A0(MHp2) - 1.D0*MU2*A0(MU2) - 1.D0*MHp2*MU2*B0(x, MHp2, MU2) + MU2*x*B0(x, MHp&
  &2, MU2) + B0(x, MHp2, MU2)*DBLE(MU**INT(4.D0))))/(MW2*PI2*SW2*TB2*x)

 amplitudes(6) = (0.015625D0*CKM22*CKMC22*EL2*(-1.D0*MC2*A0(MC2) + MC2*A0(MHp2) - 1.D0*MC2*MHp2*B0(x, MC2, MHp2) + MC2*x*B0(x, MC&
  &2, MHp2) + B0(x, MC2, MHp2)*DBLE(MC**INT(4.D0))))/(MW2*PI2*SW2*TB2*x)

 amplitudes(7) = (0.015625D0*CKM32*CKMC32*EL2*(MT2*A0(MHp2) - 1.D0*MT2*A0(MT2) - 1.D0*MHp2*MT2*B0(x, MHp2, MT2) + MT2*x*B0(x, MHp&
  &2, MT2) + B0(x, MHp2, MT2)*DBLE(MT**INT(4.D0))))/(MW2*PI2*SW2*TB2*x)

 amplitudes(8) = (0.015625D0*CKM12*CKMC12*EL2*(-1.D0*MU2*A0(MU2) + MU2*A0(GaugeXiW*MW2) - 1.D0*GaugeXiW*MU2*MW2*B0(x, MU2, GaugeX&
  &iW*MW2) + MU2*x*B0(x, MU2, GaugeXiW*MW2) + B0(x, MU2, GaugeXiW*MW2)*DBLE(MU**INT(4.D0))))/(MW2*PI2*SW2*x)

 amplitudes(9) = (0.015625D0*CKM22*CKMC22*EL2*(-1.D0*MC2*A0(MC2) + MC2*A0(GaugeXiW*MW2) - 1.D0*GaugeXiW*MC2*MW2*B0(x, MC2, GaugeX&
  &iW*MW2) + MC2*x*B0(x, MC2, GaugeXiW*MW2) + B0(x, MC2, GaugeXiW*MW2)*DBLE(MC**INT(4.D0))))/(MW2*PI2*SW2*x)

 amplitudes(10) = (0.015625D0*CKM32*CKMC32*EL2*(-1.D0*MT2*A0(MT2) + MT2*A0(GaugeXiW*MW2) - 1.D0*GaugeXiW*MT2*MW2*B0(x, MT2, Gauge&
  &XiW*MW2) + MT2*x*B0(x, MT2, GaugeXiW*MW2) + B0(x, MT2, GaugeXiW*MW2)*DBLE(MT**INT(4.D0))))/(MW2*PI2*SW2*x)

 amplitudes(11) = (0.003472222222222222D0*EL2*(-2.D0*x - 2.D0*A0(MS2) + MS2*B0(x, 0.D0, MS2) + GaugeXiA*MS2*B0(x, 0.D0, MS2) + x*&
  &B0(x, 0.D0, MS2) + GaugeXiA*x*B0(x, 0.D0, MS2) - 2.D0*MS2*x*C0Mine(DBLE(0.D0), DBLE(x), DBLE(x), DBLE(0.D0), DBLE(0.D0), DBLE(&
  &MS2)) + 2.D0*GaugeXiA*MS2*x*C0Mine(DBLE(0.D0), DBLE(x), DBLE(x), DBLE(0.D0), DBLE(0.D0), DBLE(MS2)) + C0Mine(DBLE(0.D0), DBLE(&
  &x), DBLE(x), DBLE(0.D0), DBLE(0.D0), DBLE(MS2))*DBLE(MS**INT(4.D0)) - 1.D0*GaugeXiA*C0Mine(DBLE(0.D0), DBLE(x), DBLE(x), DBLE(&
  &0.D0), DBLE(0.D0), DBLE(MS2))*DBLE(MS**INT(4.D0)) + C0Mine(DBLE(0.D0), DBLE(x), DBLE(x), DBLE(0.D0), DBLE(0.D0), DBLE(MS2))*DB&
  &LE(x**INT(2.D0)) - 1.D0*GaugeXiA*C0Mine(DBLE(0.D0), DBLE(x), DBLE(x), DBLE(0.D0), DBLE(0.D0), DBLE(MS2))*DBLE(x**INT(2.D0))))/&
  &(PI2*x)

 amplitudes(12) = (0.0008680555555555555D0*EL2*(-18.D0*MZ2*x + 24.D0*MZ2*SW2*x - 18.D0*MZ2*A0(MS2) + 24.D0*MZ2*SW2*A0(MS2) + 9.D0&
  &*MS2*A0(MZ2) + 18.D0*MZ2*A0(MZ2) - 12.D0*MS2*SW2*A0(MZ2) - 24.D0*MZ2*SW2*A0(MZ2) - 9.D0*x*A0(MZ2) + 12.D0*SW2*x*A0(MZ2) - 9.D0&
  &*MS2*A0(GaugeXiZ*MZ2) + 12.D0*MS2*SW2*A0(GaugeXiZ*MZ2) + 9.D0*x*A0(GaugeXiZ*MZ2) - 12.D0*SW2*x*A0(GaugeXiZ*MZ2) + 9.D0*MS2*MZ2&
  &*B0(x, MS2, MZ2) - 12.D0*MS2*MZ2*SW2*B0(x, MS2, MZ2) - 18.D0*MS2*x*B0(x, MS2, MZ2) + 9.D0*MZ2*x*B0(x, MS2, MZ2) + 24.D0*MS2*SW&
  &2*x*B0(x, MS2, MZ2) - 12.D0*MZ2*SW2*x*B0(x, MS2, MZ2) + 9.D0*GaugeXiZ*MS2*MZ2*B0(x, MS2, GaugeXiZ*MZ2) - 12.D0*GaugeXiZ*MS2*MZ&
  &2*SW2*B0(x, MS2, GaugeXiZ*MZ2) + 18.D0*MS2*x*B0(x, MS2, GaugeXiZ*MZ2) + 9.D0*GaugeXiZ*MZ2*x*B0(x, MS2, GaugeXiZ*MZ2) - 24.D0*M&
  &S2*SW2*x*B0(x, MS2, GaugeXiZ*MZ2) - 12.D0*GaugeXiZ*MZ2*SW2*x*B0(x, MS2, GaugeXiZ*MZ2) + 9.D0*B0(x, MS2, MZ2)*DBLE(MS**INT(4.D0&
  &)) - 12.D0*SW2*B0(x, MS2, MZ2)*DBLE(MS**INT(4.D0)) - 9.D0*B0(x, MS2, GaugeXiZ*MZ2)*DBLE(MS**INT(4.D0)) + 12.D0*SW2*B0(x, MS2, &
  &GaugeXiZ*MZ2)*DBLE(MS**INT(4.D0)) - 18.D0*B0(x, MS2, MZ2)*DBLE(MZ**INT(4.D0)) + 24.D0*SW2*B0(x, MS2, MZ2)*DBLE(MZ**INT(4.D0)) &
  &- 8.D0*MZ2*x*DBLE(SW**INT(4.D0)) - 8.D0*MZ2*A0(MS2)*DBLE(SW**INT(4.D0)) + 4.D0*MS2*A0(MZ2)*DBLE(SW**INT(4.D0)) + 8.D0*MZ2*A0(M&
  &Z2)*DBLE(SW**INT(4.D0)) - 4.D0*x*A0(MZ2)*DBLE(SW**INT(4.D0)) - 4.D0*MS2*A0(GaugeXiZ*MZ2)*DBLE(SW**INT(4.D0)) + 4.D0*x*A0(Gauge&
  &XiZ*MZ2)*DBLE(SW**INT(4.D0)) + 4.D0*MS2*MZ2*B0(x, MS2, MZ2)*DBLE(SW**INT(4.D0)) - 8.D0*MS2*x*B0(x, MS2, MZ2)*DBLE(SW**INT(4.D0&
  &)) + 4.D0*MZ2*x*B0(x, MS2, MZ2)*DBLE(SW**INT(4.D0)) + 4.D0*GaugeXiZ*MS2*MZ2*B0(x, MS2, GaugeXiZ*MZ2)*DBLE(SW**INT(4.D0)) + 8.D&
  &0*MS2*x*B0(x, MS2, GaugeXiZ*MZ2)*DBLE(SW**INT(4.D0)) + 4.D0*GaugeXiZ*MZ2*x*B0(x, MS2, GaugeXiZ*MZ2)*DBLE(SW**INT(4.D0)) + 4.D0&
  &*B0(x, MS2, MZ2)*DBLE(MS**INT(4.D0))*DBLE(SW**INT(4.D0)) - 4.D0*B0(x, MS2, GaugeXiZ*MZ2)*DBLE(MS**INT(4.D0))*DBLE(SW**INT(4.D0&
  &)) - 8.D0*B0(x, MS2, MZ2)*DBLE(MZ**INT(4.D0))*DBLE(SW**INT(4.D0)) + 9.D0*B0(x, MS2, MZ2)*DBLE(x**INT(2.D0)) - 12.D0*SW2*B0(x, &
  &MS2, MZ2)*DBLE(x**INT(2.D0)) - 9.D0*B0(x, MS2, GaugeXiZ*MZ2)*DBLE(x**INT(2.D0)) + 12.D0*SW2*B0(x, MS2, GaugeXiZ*MZ2)*DBLE(x**I&
  &NT(2.D0)) + 4.D0*B0(x, MS2, MZ2)*DBLE(SW**INT(4.D0))*DBLE(x**INT(2.D0)) - 4.D0*B0(x, MS2, GaugeXiZ*MZ2)*DBLE(SW**INT(4.D0))*DB&
  &LE(x**INT(2.D0))))/(CW2*MZ2*PI2*SW2*x)

 amplitudes(13) = (0.015625D0*CKM12*CKMC12*EL2*(-2.D0*MW2*x - 2.D0*MW2*A0(MU2) + (MU2 + 2.D0*MW2 - 1.D0*x)*A0(MW2) - 1.D0*MU2*A0(&
  &GaugeXiW*MW2) + x*A0(GaugeXiW*MW2) + MU2*MW2*B0(x, MU2, MW2) - 2.D0*MU2*x*B0(x, MU2, MW2) + MW2*x*B0(x, MU2, MW2) + GaugeXiW*M&
  &U2*MW2*B0(x, MU2, GaugeXiW*MW2) + 2.D0*MU2*x*B0(x, MU2, GaugeXiW*MW2) + GaugeXiW*MW2*x*B0(x, MU2, GaugeXiW*MW2) + B0(x, MU2, M&
  &W2)*DBLE(MU**INT(4.D0)) - 1.D0*B0(x, MU2, GaugeXiW*MW2)*DBLE(MU**INT(4.D0)) - 2.D0*B0(x, MU2, MW2)*DBLE(MW**INT(4.D0)) + B0(x,&
  & MU2, MW2)*DBLE(x**INT(2.D0)) - 1.D0*B0(x, MU2, GaugeXiW*MW2)*DBLE(x**INT(2.D0))))/(MW2*PI2*SW2*x)

 amplitudes(14) = (0.015625D0*CKM22*CKMC22*EL2*(-2.D0*MW2*x - 2.D0*MW2*A0(MC2) + (MC2 + 2.D0*MW2 - 1.D0*x)*A0(MW2) - 1.D0*MC2*A0(&
  &GaugeXiW*MW2) + x*A0(GaugeXiW*MW2) + MC2*MW2*B0(x, MC2, MW2) - 2.D0*MC2*x*B0(x, MC2, MW2) + MW2*x*B0(x, MC2, MW2) + GaugeXiW*M&
  &C2*MW2*B0(x, MC2, GaugeXiW*MW2) + 2.D0*MC2*x*B0(x, MC2, GaugeXiW*MW2) + GaugeXiW*MW2*x*B0(x, MC2, GaugeXiW*MW2) + B0(x, MC2, M&
  &W2)*DBLE(MC**INT(4.D0)) - 1.D0*B0(x, MC2, GaugeXiW*MW2)*DBLE(MC**INT(4.D0)) - 2.D0*B0(x, MC2, MW2)*DBLE(MW**INT(4.D0)) + B0(x,&
  & MC2, MW2)*DBLE(x**INT(2.D0)) - 1.D0*B0(x, MC2, GaugeXiW*MW2)*DBLE(x**INT(2.D0))))/(MW2*PI2*SW2*x)

 amplitudes(15) = (0.015625D0*CKM32*CKMC32*EL2*(-2.D0*MW2*x - 2.D0*MW2*A0(MT2) + (MT2 + 2.D0*MW2 - 1.D0*x)*A0(MW2) - 1.D0*MT2*A0(&
  &GaugeXiW*MW2) + x*A0(GaugeXiW*MW2) + MT2*MW2*B0(x, MT2, MW2) - 2.D0*MT2*x*B0(x, MT2, MW2) + MW2*x*B0(x, MT2, MW2) + GaugeXiW*M&
  &T2*MW2*B0(x, MT2, GaugeXiW*MW2) + 2.D0*MT2*x*B0(x, MT2, GaugeXiW*MW2) + GaugeXiW*MW2*x*B0(x, MT2, GaugeXiW*MW2) + B0(x, MT2, M&
  &W2)*DBLE(MT**INT(4.D0)) - 1.D0*B0(x, MT2, GaugeXiW*MW2)*DBLE(MT**INT(4.D0)) - 2.D0*B0(x, MT2, MW2)*DBLE(MW**INT(4.D0)) + B0(x,&
  & MT2, MW2)*DBLE(x**INT(2.D0)) - 1.D0*B0(x, MT2, GaugeXiW*MW2)*DBLE(x**INT(2.D0))))/(MW2*PI2*SW2*x)

  totalAmplitude = (0D0,0D0)
 do j=1,15
  totalAmplitude = totalAmplitude + amplitudes(j)
 end do
 SelfSSLeftUsual = totalAmplitude
end function SelfSSLeftUsual

