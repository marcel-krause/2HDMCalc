double complex function SelfDSRightAlter(x)
 use constants
 implicit none
#include "looptools.h"
 double precision, intent(in) :: x
 integer :: j
 double complex :: totalAmplitude
 double complex :: amplitudes(9)

 amplitudes(1) = (0.015625D0*CKM11*CKMC12*EL2*(MD*MS*TB2*A0(MHp2)*DBLE(Yuk3**INT(2.D0)) - 1.D0*MD*MS*TB2*A0(MU2)*DBLE(Yuk3**INT(2&
  &.D0)) - 1.D0*MD*MHp2*MS*TB2*B0(x, MHp2, MU2)*DBLE(Yuk3**INT(2.D0)) + MD*MS*MU2*TB2*B0(x, MHp2, MU2)*DBLE(Yuk3**INT(2.D0)) + MD&
  &*MS*TB2*x*B0(x, MHp2, MU2)*DBLE(Yuk3**INT(2.D0))))/(MW2*PI2*SW2*TB2*x)

 amplitudes(2) = (0.015625D0*CKM21*CKMC22*EL2*(-1.D0*MD*MS*TB2*A0(MC2)*DBLE(Yuk3**INT(2.D0)) + MD*MS*TB2*A0(MHp2)*DBLE(Yuk3**INT(&
  &2.D0)) + MC2*MD*MS*TB2*B0(x, MC2, MHp2)*DBLE(Yuk3**INT(2.D0)) - 1.D0*MD*MHp2*MS*TB2*B0(x, MC2, MHp2)*DBLE(Yuk3**INT(2.D0)) + M&
  &D*MS*TB2*x*B0(x, MC2, MHp2)*DBLE(Yuk3**INT(2.D0))))/(MW2*PI2*SW2*TB2*x)

 amplitudes(3) = (0.015625D0*CKM31*CKMC32*EL2*(MD*MS*TB2*A0(MHp2)*DBLE(Yuk3**INT(2.D0)) - 1.D0*MD*MS*TB2*A0(MT2)*DBLE(Yuk3**INT(2&
  &.D0)) - 1.D0*MD*MHp2*MS*TB2*B0(x, MHp2, MT2)*DBLE(Yuk3**INT(2.D0)) + MD*MS*MT2*TB2*B0(x, MHp2, MT2)*DBLE(Yuk3**INT(2.D0)) + MD&
  &*MS*TB2*x*B0(x, MHp2, MT2)*DBLE(Yuk3**INT(2.D0))))/(MW2*PI2*SW2*TB2*x)

 amplitudes(4) = (0.015625D0*CKM11*CKMC12*EL2*(-1.D0*MD*MS*A0(MU2) + MD*MS*A0(GaugeXiW*MW2) + MD*MS*MU2*B0(x, MU2, GaugeXiW*MW2) &
  &- 1.D0*GaugeXiW*MD*MS*MW2*B0(x, MU2, GaugeXiW*MW2) + MD*MS*x*B0(x, MU2, GaugeXiW*MW2)))/(MW2*PI2*SW2*x)

 amplitudes(5) = (0.015625D0*CKM21*CKMC22*EL2*(-1.D0*MD*MS*A0(MC2) + MD*MS*A0(GaugeXiW*MW2) + MC2*MD*MS*B0(x, MC2, GaugeXiW*MW2) &
  &- 1.D0*GaugeXiW*MD*MS*MW2*B0(x, MC2, GaugeXiW*MW2) + MD*MS*x*B0(x, MC2, GaugeXiW*MW2)))/(MW2*PI2*SW2*x)

 amplitudes(6) = (0.015625D0*CKM31*CKMC32*EL2*(-1.D0*MD*MS*A0(MT2) + MD*MS*A0(GaugeXiW*MW2) + MD*MS*MT2*B0(x, MT2, GaugeXiW*MW2) &
  &- 1.D0*GaugeXiW*MD*MS*MW2*B0(x, MT2, GaugeXiW*MW2) + MD*MS*x*B0(x, MT2, GaugeXiW*MW2)))/(MW2*PI2*SW2*x)

 amplitudes(7) = 0.D0

 amplitudes(8) = 0.D0

 amplitudes(9) = 0.D0

  totalAmplitude = (0D0,0D0)
 do j=1,9
  totalAmplitude = totalAmplitude + amplitudes(j)
 end do
 SelfDSRightAlter = totalAmplitude
end function SelfDSRightAlter

