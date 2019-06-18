double complex function SelfSBRightUsual(x)
 use constants
 implicit none
#include "looptools.h"
 double precision, intent(in) :: x
 integer :: j
 double complex :: totalAmplitude
 double complex :: amplitudes(9)

 amplitudes(1) = (0.015625D0*CKM12*CKMC13*EL2*(MB*MS*TB2*A0(MHp2)*DBLE(Yuk3**INT(2.D0)) - 1.D0*MB*MS*TB2*A0(MU2)*DBLE(Yuk3**INT(2&
  &.D0)) - 1.D0*MB*MHp2*MS*TB2*B0(x, MHp2, MU2)*DBLE(Yuk3**INT(2.D0)) + MB*MS*MU2*TB2*B0(x, MHp2, MU2)*DBLE(Yuk3**INT(2.D0)) + MB&
  &*MS*TB2*x*B0(x, MHp2, MU2)*DBLE(Yuk3**INT(2.D0))))/(MW2*PI2*SW2*TB2*x)

 amplitudes(2) = (0.015625D0*CKM22*CKMC23*EL2*(-1.D0*MB*MS*TB2*A0(MC2)*DBLE(Yuk3**INT(2.D0)) + MB*MS*TB2*A0(MHp2)*DBLE(Yuk3**INT(&
  &2.D0)) + MB*MC2*MS*TB2*B0(x, MC2, MHp2)*DBLE(Yuk3**INT(2.D0)) - 1.D0*MB*MHp2*MS*TB2*B0(x, MC2, MHp2)*DBLE(Yuk3**INT(2.D0)) + M&
  &B*MS*TB2*x*B0(x, MC2, MHp2)*DBLE(Yuk3**INT(2.D0))))/(MW2*PI2*SW2*TB2*x)

 amplitudes(3) = (0.015625D0*CKM32*CKMC33*EL2*(MB*MS*TB2*A0(MHp2)*DBLE(Yuk3**INT(2.D0)) - 1.D0*MB*MS*TB2*A0(MT2)*DBLE(Yuk3**INT(2&
  &.D0)) - 1.D0*MB*MHp2*MS*TB2*B0(x, MHp2, MT2)*DBLE(Yuk3**INT(2.D0)) + MB*MS*MT2*TB2*B0(x, MHp2, MT2)*DBLE(Yuk3**INT(2.D0)) + MB&
  &*MS*TB2*x*B0(x, MHp2, MT2)*DBLE(Yuk3**INT(2.D0))))/(MW2*PI2*SW2*TB2*x)

 amplitudes(4) = (0.015625D0*CKM12*CKMC13*EL2*(-1.D0*MB*MS*A0(MU2) + MB*MS*A0(GaugeXiW*MW2) + MB*MS*MU2*B0(x, MU2, GaugeXiW*MW2) &
  &- 1.D0*GaugeXiW*MB*MS*MW2*B0(x, MU2, GaugeXiW*MW2) + MB*MS*x*B0(x, MU2, GaugeXiW*MW2)))/(MW2*PI2*SW2*x)

 amplitudes(5) = (0.015625D0*CKM22*CKMC23*EL2*(-1.D0*MB*MS*A0(MC2) + MB*MS*A0(GaugeXiW*MW2) + MB*MC2*MS*B0(x, MC2, GaugeXiW*MW2) &
  &- 1.D0*GaugeXiW*MB*MS*MW2*B0(x, MC2, GaugeXiW*MW2) + MB*MS*x*B0(x, MC2, GaugeXiW*MW2)))/(MW2*PI2*SW2*x)

 amplitudes(6) = (0.015625D0*CKM32*CKMC33*EL2*(-1.D0*MB*MS*A0(MT2) + MB*MS*A0(GaugeXiW*MW2) + MB*MS*MT2*B0(x, MT2, GaugeXiW*MW2) &
  &- 1.D0*GaugeXiW*MB*MS*MW2*B0(x, MT2, GaugeXiW*MW2) + MB*MS*x*B0(x, MT2, GaugeXiW*MW2)))/(MW2*PI2*SW2*x)

 amplitudes(7) = 0.D0

 amplitudes(8) = 0.D0

 amplitudes(9) = 0.D0

  totalAmplitude = (0D0,0D0)
 do j=1,9
  totalAmplitude = totalAmplitude + amplitudes(j)
 end do
 SelfSBRightUsual = totalAmplitude
end function SelfSBRightUsual

