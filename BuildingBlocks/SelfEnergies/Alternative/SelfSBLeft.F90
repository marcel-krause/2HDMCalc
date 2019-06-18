double complex function SelfSBLeftAlter(x)
 use constants
 implicit none
#include "looptools.h"
 double precision, intent(in) :: x
 integer :: j
 double complex :: totalAmplitude
 double complex :: amplitudes(9)

 amplitudes(1) = (0.015625D0*CKM12*CKMC13*EL2*(MU2*A0(MHp2) - 1.D0*MU2*A0(MU2) - 1.D0*MHp2*MU2*B0(x, MHp2, MU2) + MU2*x*B0(x, MHp&
  &2, MU2) + B0(x, MHp2, MU2)*DBLE(MU**INT(4.D0))))/(MW2*PI2*SW2*TB2*x)

 amplitudes(2) = (0.015625D0*CKM22*CKMC23*EL2*(-1.D0*MC2*A0(MC2) + MC2*A0(MHp2) - 1.D0*MC2*MHp2*B0(x, MC2, MHp2) + MC2*x*B0(x, MC&
  &2, MHp2) + B0(x, MC2, MHp2)*DBLE(MC**INT(4.D0))))/(MW2*PI2*SW2*TB2*x)

 amplitudes(3) = (0.015625D0*CKM32*CKMC33*EL2*(MT2*A0(MHp2) - 1.D0*MT2*A0(MT2) - 1.D0*MHp2*MT2*B0(x, MHp2, MT2) + MT2*x*B0(x, MHp&
  &2, MT2) + B0(x, MHp2, MT2)*DBLE(MT**INT(4.D0))))/(MW2*PI2*SW2*TB2*x)

 amplitudes(4) = (0.015625D0*CKM12*CKMC13*EL2*(-1.D0*MU2*A0(MU2) + MU2*A0(GaugeXiW*MW2) - 1.D0*GaugeXiW*MU2*MW2*B0(x, MU2, GaugeX&
  &iW*MW2) + MU2*x*B0(x, MU2, GaugeXiW*MW2) + B0(x, MU2, GaugeXiW*MW2)*DBLE(MU**INT(4.D0))))/(MW2*PI2*SW2*x)

 amplitudes(5) = (0.015625D0*CKM22*CKMC23*EL2*(-1.D0*MC2*A0(MC2) + MC2*A0(GaugeXiW*MW2) - 1.D0*GaugeXiW*MC2*MW2*B0(x, MC2, GaugeX&
  &iW*MW2) + MC2*x*B0(x, MC2, GaugeXiW*MW2) + B0(x, MC2, GaugeXiW*MW2)*DBLE(MC**INT(4.D0))))/(MW2*PI2*SW2*x)

 amplitudes(6) = (0.015625D0*CKM32*CKMC33*EL2*(-1.D0*MT2*A0(MT2) + MT2*A0(GaugeXiW*MW2) - 1.D0*GaugeXiW*MT2*MW2*B0(x, MT2, GaugeX&
  &iW*MW2) + MT2*x*B0(x, MT2, GaugeXiW*MW2) + B0(x, MT2, GaugeXiW*MW2)*DBLE(MT**INT(4.D0))))/(MW2*PI2*SW2*x)

 amplitudes(7) = (0.015625D0*CKM12*CKMC13*EL2*(-2.D0*MW2*x - 2.D0*MW2*A0(MU2) + (MU2 + 2.D0*MW2 - 1.D0*x)*A0(MW2) - 1.D0*MU2*A0(G&
  &augeXiW*MW2) + x*A0(GaugeXiW*MW2) + MU2*MW2*B0(x, MU2, MW2) - 2.D0*MU2*x*B0(x, MU2, MW2) + MW2*x*B0(x, MU2, MW2) + GaugeXiW*MU&
  &2*MW2*B0(x, MU2, GaugeXiW*MW2) + 2.D0*MU2*x*B0(x, MU2, GaugeXiW*MW2) + GaugeXiW*MW2*x*B0(x, MU2, GaugeXiW*MW2) + B0(x, MU2, MW&
  &2)*DBLE(MU**INT(4.D0)) - 1.D0*B0(x, MU2, GaugeXiW*MW2)*DBLE(MU**INT(4.D0)) - 2.D0*B0(x, MU2, MW2)*DBLE(MW**INT(4.D0)) + B0(x, &
  &MU2, MW2)*DBLE(x**INT(2.D0)) - 1.D0*B0(x, MU2, GaugeXiW*MW2)*DBLE(x**INT(2.D0))))/(MW2*PI2*SW2*x)

 amplitudes(8) = (0.015625D0*CKM22*CKMC23*EL2*(-2.D0*MW2*x - 2.D0*MW2*A0(MC2) + (MC2 + 2.D0*MW2 - 1.D0*x)*A0(MW2) - 1.D0*MC2*A0(G&
  &augeXiW*MW2) + x*A0(GaugeXiW*MW2) + MC2*MW2*B0(x, MC2, MW2) - 2.D0*MC2*x*B0(x, MC2, MW2) + MW2*x*B0(x, MC2, MW2) + GaugeXiW*MC&
  &2*MW2*B0(x, MC2, GaugeXiW*MW2) + 2.D0*MC2*x*B0(x, MC2, GaugeXiW*MW2) + GaugeXiW*MW2*x*B0(x, MC2, GaugeXiW*MW2) + B0(x, MC2, MW&
  &2)*DBLE(MC**INT(4.D0)) - 1.D0*B0(x, MC2, GaugeXiW*MW2)*DBLE(MC**INT(4.D0)) - 2.D0*B0(x, MC2, MW2)*DBLE(MW**INT(4.D0)) + B0(x, &
  &MC2, MW2)*DBLE(x**INT(2.D0)) - 1.D0*B0(x, MC2, GaugeXiW*MW2)*DBLE(x**INT(2.D0))))/(MW2*PI2*SW2*x)

 amplitudes(9) = (0.015625D0*CKM32*CKMC33*EL2*(-2.D0*MW2*x - 2.D0*MW2*A0(MT2) + (MT2 + 2.D0*MW2 - 1.D0*x)*A0(MW2) - 1.D0*MT2*A0(G&
  &augeXiW*MW2) + x*A0(GaugeXiW*MW2) + MT2*MW2*B0(x, MT2, MW2) - 2.D0*MT2*x*B0(x, MT2, MW2) + MW2*x*B0(x, MT2, MW2) + GaugeXiW*MT&
  &2*MW2*B0(x, MT2, GaugeXiW*MW2) + 2.D0*MT2*x*B0(x, MT2, GaugeXiW*MW2) + GaugeXiW*MW2*x*B0(x, MT2, GaugeXiW*MW2) + B0(x, MT2, MW&
  &2)*DBLE(MT**INT(4.D0)) - 1.D0*B0(x, MT2, GaugeXiW*MW2)*DBLE(MT**INT(4.D0)) - 2.D0*B0(x, MT2, MW2)*DBLE(MW**INT(4.D0)) + B0(x, &
  &MT2, MW2)*DBLE(x**INT(2.D0)) - 1.D0*B0(x, MT2, GaugeXiW*MW2)*DBLE(x**INT(2.D0))))/(MW2*PI2*SW2*x)

  totalAmplitude = (0D0,0D0)
 do j=1,9
  totalAmplitude = totalAmplitude + amplitudes(j)
 end do
 SelfSBLeftAlter = totalAmplitude
end function SelfSBLeftAlter

