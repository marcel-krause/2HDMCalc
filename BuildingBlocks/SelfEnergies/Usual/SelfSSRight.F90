double complex function SelfSSRightUsual(x)
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

 amplitudes(5) = (0.015625D0*CKM12*CKMC12*EL2*(MS2*TB2*A0(MHp2)*DBLE(Yuk3**INT(2.D0)) - 1.D0*MS2*TB2*A0(MU2)*DBLE(Yuk3**INT(2.D0)&
  &) - 1.D0*MHp2*MS2*TB2*B0(x, MHp2, MU2)*DBLE(Yuk3**INT(2.D0)) + MS2*MU2*TB2*B0(x, MHp2, MU2)*DBLE(Yuk3**INT(2.D0)) + MS2*TB2*x*&
  &B0(x, MHp2, MU2)*DBLE(Yuk3**INT(2.D0))))/(MW2*PI2*SW2*TB2*x)

 amplitudes(6) = (0.015625D0*CKM22*CKMC22*EL2*(-1.D0*MS2*TB2*A0(MC2)*DBLE(Yuk3**INT(2.D0)) + MS2*TB2*A0(MHp2)*DBLE(Yuk3**INT(2.D0&
  &)) + MC2*MS2*TB2*B0(x, MC2, MHp2)*DBLE(Yuk3**INT(2.D0)) - 1.D0*MHp2*MS2*TB2*B0(x, MC2, MHp2)*DBLE(Yuk3**INT(2.D0)) + MS2*TB2*x&
  &*B0(x, MC2, MHp2)*DBLE(Yuk3**INT(2.D0))))/(MW2*PI2*SW2*TB2*x)

 amplitudes(7) = (0.015625D0*CKM32*CKMC32*EL2*(MS2*TB2*A0(MHp2)*DBLE(Yuk3**INT(2.D0)) - 1.D0*MS2*TB2*A0(MT2)*DBLE(Yuk3**INT(2.D0)&
  &) - 1.D0*MHp2*MS2*TB2*B0(x, MHp2, MT2)*DBLE(Yuk3**INT(2.D0)) + MS2*MT2*TB2*B0(x, MHp2, MT2)*DBLE(Yuk3**INT(2.D0)) + MS2*TB2*x*&
  &B0(x, MHp2, MT2)*DBLE(Yuk3**INT(2.D0))))/(MW2*PI2*SW2*TB2*x)

 amplitudes(8) = (0.015625D0*CKM12*CKMC12*EL2*(-1.D0*MS2*A0(MU2) + MS2*A0(GaugeXiW*MW2) + MS2*MU2*B0(x, MU2, GaugeXiW*MW2) - 1.D0&
  &*GaugeXiW*MS2*MW2*B0(x, MU2, GaugeXiW*MW2) + MS2*x*B0(x, MU2, GaugeXiW*MW2)))/(MW2*PI2*SW2*x)

 amplitudes(9) = (0.015625D0*CKM22*CKMC22*EL2*(-1.D0*MS2*A0(MC2) + MS2*A0(GaugeXiW*MW2) + MC2*MS2*B0(x, MC2, GaugeXiW*MW2) - 1.D0&
  &*GaugeXiW*MS2*MW2*B0(x, MC2, GaugeXiW*MW2) + MS2*x*B0(x, MC2, GaugeXiW*MW2)))/(MW2*PI2*SW2*x)

 amplitudes(10) = (0.015625D0*CKM32*CKMC32*EL2*(-1.D0*MS2*A0(MT2) + MS2*A0(GaugeXiW*MW2) + MS2*MT2*B0(x, MT2, GaugeXiW*MW2) - 1.D&
  &0*GaugeXiW*MS2*MW2*B0(x, MT2, GaugeXiW*MW2) + MS2*x*B0(x, MT2, GaugeXiW*MW2)))/(MW2*PI2*SW2*x)

 amplitudes(11) = (0.003472222222222222D0*EL2*(-2.D0*x - 2.D0*A0(MS2) + MS2*B0(x, 0.D0, MS2) + GaugeXiA*MS2*B0(x, 0.D0, MS2) + x*&
  &B0(x, 0.D0, MS2) + GaugeXiA*x*B0(x, 0.D0, MS2) - 2.D0*MS2*x*C0Mine(DBLE(0.D0), DBLE(x), DBLE(x), DBLE(0.D0), DBLE(0.D0), DBLE(&
  &MS2)) + 2.D0*GaugeXiA*MS2*x*C0Mine(DBLE(0.D0), DBLE(x), DBLE(x), DBLE(0.D0), DBLE(0.D0), DBLE(MS2)) + C0Mine(DBLE(0.D0), DBLE(&
  &x), DBLE(x), DBLE(0.D0), DBLE(0.D0), DBLE(MS2))*DBLE(MS**INT(4.D0)) - 1.D0*GaugeXiA*C0Mine(DBLE(0.D0), DBLE(x), DBLE(x), DBLE(&
  &0.D0), DBLE(0.D0), DBLE(MS2))*DBLE(MS**INT(4.D0)) + C0Mine(DBLE(0.D0), DBLE(x), DBLE(x), DBLE(0.D0), DBLE(0.D0), DBLE(MS2))*DB&
  &LE(x**INT(2.D0)) - 1.D0*GaugeXiA*C0Mine(DBLE(0.D0), DBLE(x), DBLE(x), DBLE(0.D0), DBLE(0.D0), DBLE(MS2))*DBLE(x**INT(2.D0))))/&
  &(PI2*x)

 amplitudes(12) = (0.0008680555555555555D0*EL2*(-8.D0*MZ2*x*DBLE(SW**INT(4.D0)) - 8.D0*MZ2*A0(MS2)*DBLE(SW**INT(4.D0)) + 4.D0*MS2&
  &*A0(MZ2)*DBLE(SW**INT(4.D0)) + 8.D0*MZ2*A0(MZ2)*DBLE(SW**INT(4.D0)) - 4.D0*x*A0(MZ2)*DBLE(SW**INT(4.D0)) - 4.D0*MS2*A0(GaugeXi&
  &Z*MZ2)*DBLE(SW**INT(4.D0)) + 4.D0*x*A0(GaugeXiZ*MZ2)*DBLE(SW**INT(4.D0)) + 4.D0*MS2*MZ2*B0(x, MS2, MZ2)*DBLE(SW**INT(4.D0)) - &
  &8.D0*MS2*x*B0(x, MS2, MZ2)*DBLE(SW**INT(4.D0)) + 4.D0*MZ2*x*B0(x, MS2, MZ2)*DBLE(SW**INT(4.D0)) + 4.D0*GaugeXiZ*MS2*MZ2*B0(x, &
  &MS2, GaugeXiZ*MZ2)*DBLE(SW**INT(4.D0)) + 8.D0*MS2*x*B0(x, MS2, GaugeXiZ*MZ2)*DBLE(SW**INT(4.D0)) + 4.D0*GaugeXiZ*MZ2*x*B0(x, M&
  &S2, GaugeXiZ*MZ2)*DBLE(SW**INT(4.D0)) + 4.D0*B0(x, MS2, MZ2)*DBLE(MS**INT(4.D0))*DBLE(SW**INT(4.D0)) - 4.D0*B0(x, MS2, GaugeXi&
  &Z*MZ2)*DBLE(MS**INT(4.D0))*DBLE(SW**INT(4.D0)) - 8.D0*B0(x, MS2, MZ2)*DBLE(MZ**INT(4.D0))*DBLE(SW**INT(4.D0)) + 4.D0*B0(x, MS2&
  &, MZ2)*DBLE(SW**INT(4.D0))*DBLE(x**INT(2.D0)) - 4.D0*B0(x, MS2, GaugeXiZ*MZ2)*DBLE(SW**INT(4.D0))*DBLE(x**INT(2.D0))))/(CW2*MZ&
  &2*PI2*SW2*x)

 amplitudes(13) = 0.D0

 amplitudes(14) = 0.D0

 amplitudes(15) = 0.D0

  totalAmplitude = (0D0,0D0)
 do j=1,15
  totalAmplitude = totalAmplitude + amplitudes(j)
 end do
 SelfSSRightUsual = totalAmplitude
end function SelfSSRightUsual

