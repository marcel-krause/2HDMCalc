double complex function SelfGpHpUsual(x)
 use constants
 implicit none
#include "looptools.h"
 double precision, intent(in) :: x
 integer :: j
 double complex :: totalAmplitude
 double complex :: amplitudes(24)

 amplitudes(1) = (0.0078125D0*CBA*(2.D0*CAB*(EL2*Mh02 - 2.D0*Lambda5*MW2*SW2) + SBA*(EL2*(Mh02*S2A - 1.D0*MHH2*S2A - 2.D0*MHp2*S2&
  &B) + 4.D0*Lambda5*MW2*S2B*SW2))* A0(Mh02))/(MW2*PI2*S2B*SW2)

 amplitudes(2) = (-0.0078125D0*SBA*(2.D0*SAB*(EL2*MHH2 - 2.D0*Lambda5*MW2*SW2) + CBA*(EL2*(Mh02*S2A - 1.D0*MHH2*S2A - 2.D0*MHp2*S&
  &2B) + 4.D0*Lambda5*MW2*S2B*SW2))* A0(MHH2))/(MW2*PI2*S2B*SW2)

 amplitudes(3) = (0.0078125D0*(2.D0*CAB*CBA*EL2*Mh02 + CBA*EL2*(-1.D0*Mh02 + MHH2)*S2B*SBA - 2.D0*(EL2*MHH2*SAB*SBA + 2.D0*C2B*La&
  &mbda5*MW2*SW2))*A0(MA02))/ (MW2*PI2*S2B*SW2)

 amplitudes(4) = (0.0078125D0*CBA*EL2*(Mh02 - 1.D0*MHH2)*SBA*A0(GaugeXiZ*MZ2))/(MW2*PI2*SW2)

 amplitudes(5) = (0.03125D0*(2.D0*CAB*CBA*EL2*Mh02 + CBA*EL2*(-1.D0*Mh02 + MHH2)*S2B*SBA - 2.D0*(EL2*MHH2*SAB*SBA + 2.D0*C2B*Lamb&
  &da5*MW2*SW2))*A0(MHp2))/ (MW2*PI2*S2B*SW2)

 amplitudes(6) = (0.03125D0*CBA*EL2*(Mh02 - 1.D0*MHH2)*SBA*A0(GaugeXiW*MW2))/(MW2*PI2*SW2)

 amplitudes(7) = (0.03125D0*EL2*ME2*Yuk6*(A0(ME2) + (ME2 - 1.D0*x)*B0(x, 0.D0, ME2)))/(MW2*PI2*SW2)

 amplitudes(8) = (0.03125D0*EL2*MM2*Yuk6*(A0(MM2) + (MM2 - 1.D0*x)*B0(x, 0.D0, MM2)))/(MW2*PI2*SW2)

 amplitudes(9) = (0.03125D0*EL2*ML2*Yuk6*(A0(ML2) + (ML2 - 1.D0*x)*B0(x, 0.D0, ML2)))/(MW2*PI2*SW2)

 amplitudes(10) = (-0.09375D0*CKM11*CKMC11*EL2*((MU2 - 1.D0*MD2*TB*Yuk3)*A0(MD2) + (MU2 - 1.D0*MD2*TB*Yuk3)*A0(MU2) + B0(x, MD2, &
  &MU2)*(-1.D0*MU2*x + MD2*(TB*x*Yuk3 + MU2*(-1.D0 + TB*Yuk3)) - 1.D0*TB*Yuk3*DBLE(MD**INT(4.D0)) + DBLE(MU**INT(4.D0)))))/ (MW2*&
  &PI2*SW2*TB)

 amplitudes(11) = (-0.09375D0*CKM21*CKMC21*EL2*((MC2 - 1.D0*MD2*TB*Yuk3)*A0(MC2) + (MC2 - 1.D0*MD2*TB*Yuk3)*A0(MD2) + B0(x, MC2, &
  &MD2)*(MD2*TB*(-1.D0*MD2 + x)*Yuk3 - 1.D0*MC2*(x + MD2*(1.D0 - 1.D0*TB*Yuk3)) + DBLE(MC**INT(4.D0)))))/(MW2*PI2*SW2*TB)

 amplitudes(12) = (-0.09375D0*CKM31*CKMC31*EL2*((MT2 - 1.D0*MD2*TB*Yuk3)*A0(MD2) + (MT2 - 1.D0*MD2*TB*Yuk3)*A0(MT2) + B0(x, MD2, &
  &MT2)*(-1.D0*MT2*x + MD2*(TB*x*Yuk3 + MT2*(-1.D0 + TB*Yuk3)) - 1.D0*TB*Yuk3*DBLE(MD**INT(4.D0)) + DBLE(MT**INT(4.D0)))))/ (MW2*&
  &PI2*SW2*TB)

 amplitudes(13) = (-0.09375D0*CKM12*CKMC12*EL2*((MU2 - 1.D0*MS2*TB*Yuk3)*A0(MS2) + (MU2 - 1.D0*MS2*TB*Yuk3)*A0(MU2) + B0(x, MS2, &
  &MU2)*(-1.D0*MU2*x + MS2*(TB*x*Yuk3 + MU2*(-1.D0 + TB*Yuk3)) - 1.D0*TB*Yuk3*DBLE(MS**INT(4.D0)) + DBLE(MU**INT(4.D0)))))/ (MW2*&
  &PI2*SW2*TB)

 amplitudes(14) = (-0.09375D0*CKM22*CKMC22*EL2*((MC2 - 1.D0*MS2*TB*Yuk3)*A0(MC2) + (MC2 - 1.D0*MS2*TB*Yuk3)*A0(MS2) + B0(x, MC2, &
  &MS2)*(MS2*TB*(-1.D0*MS2 + x)*Yuk3 - 1.D0*MC2*(x + MS2*(1.D0 - 1.D0*TB*Yuk3)) + DBLE(MC**INT(4.D0)))))/(MW2*PI2*SW2*TB)

 amplitudes(15) = (-0.09375D0*CKM32*CKMC32*EL2*((MT2 - 1.D0*MS2*TB*Yuk3)*A0(MS2) + (MT2 - 1.D0*MS2*TB*Yuk3)*A0(MT2) + B0(x, MS2, &
  &MT2)*(-1.D0*MT2*x + MS2*(TB*x*Yuk3 + MT2*(-1.D0 + TB*Yuk3)) - 1.D0*TB*Yuk3*DBLE(MS**INT(4.D0)) + DBLE(MT**INT(4.D0)))))/ (MW2*&
  &PI2*SW2*TB)

 amplitudes(16) = (-0.09375D0*CKM13*CKMC13*EL2*((MU2 - 1.D0*MB2*TB*Yuk3)*A0(MB2) + (MU2 - 1.D0*MB2*TB*Yuk3)*A0(MU2) + B0(x, MB2, &
  &MU2)*(-1.D0*MU2*x + MB2*(TB*x*Yuk3 + MU2*(-1.D0 + TB*Yuk3)) - 1.D0*TB*Yuk3*DBLE(MB**INT(4.D0)) + DBLE(MU**INT(4.D0)))))/ (MW2*&
  &PI2*SW2*TB)

 amplitudes(17) = (-0.09375D0*CKM23*CKMC23*EL2*((MC2 - 1.D0*MB2*TB*Yuk3)*A0(MB2) + (MC2 - 1.D0*MB2*TB*Yuk3)*A0(MC2) + B0(x, MB2, &
  &MC2)*(-1.D0*MC2*x + MB2*(TB*x*Yuk3 + MC2*(-1.D0 + TB*Yuk3)) - 1.D0*TB*Yuk3*DBLE(MB**INT(4.D0)) + DBLE(MC**INT(4.D0)))))/ (MW2*&
  &PI2*SW2*TB)

 amplitudes(18) = (-0.09375D0*CKM33*CKMC33*EL2*((MT2 - 1.D0*MB2*TB*Yuk3)*A0(MB2) + (MT2 - 1.D0*MB2*TB*Yuk3)*A0(MT2) + B0(x, MB2, &
  &MT2)*(-1.D0*MT2*x + MB2*(TB*x*Yuk3 + MT2*(-1.D0 + TB*Yuk3)) - 1.D0*TB*Yuk3*DBLE(MB**INT(4.D0)) + DBLE(MT**INT(4.D0)))))/ (MW2*&
  &PI2*SW2*TB)

 amplitudes(19) = (0.015625D0*CBA*(Mh02 - 1.D0*MHp2)*(-1.D0*EL2*(Mh02 - 2.D0*MHp2)*S2B*SBA + 2.D0*CAB*(EL2*Mh02 - 2.D0*Lambda5*MW&
  &2*SW2))*B0(x, Mh02, MHp2))/ (MW2*PI2*S2B*SW2)

 amplitudes(20) = (0.015625D0*(MHH2 - 1.D0*MHp2)*SBA*(CBA*EL2*(MHH2 - 2.D0*MHp2)*S2B - 2.D0*EL2*MHH2*SAB + 4.D0*Lambda5*MW2*SAB*S&
  &W2)*B0(x, MHH2, MHp2))/ (MW2*PI2*S2B*SW2)

 amplitudes(21) = (0.015625D0*CBA*EL2*Mh02*(Mh02 - 1.D0*MHp2)*SBA*B0(x, Mh02, GaugeXiW*MW2))/(MW2*PI2*SW2)

 amplitudes(22) = (-0.015625D0*CBA*EL2*MHH2*(MHH2 - 1.D0*MHp2)*SBA*B0(x, MHH2, GaugeXiW*MW2))/(MW2*PI2*SW2)

 amplitudes(23) = (0.015625D0*CBA*EL2*SBA*(MW2*A0(Mh02) + (Mh02 - 1.D0*MW2 - 1.D0*x)*A0(MW2) - 1.D0*Mh02*A0(GaugeXiW*MW2) - 1.D0*&
  &GaugeXiW*MW2*A0(GaugeXiW*MW2) + x*A0(GaugeXiW*MW2) - 2.D0*Mh02*MW2*B0(x, Mh02, MW2) - 2.D0*Mh02*x*B0(x, Mh02, MW2) - 2.D0*MW2*&
  &x*B0(x, Mh02, MW2) + 2.D0*Mh02*x*B0(x, Mh02, GaugeXiW*MW2) + B0(x, Mh02, MW2)*DBLE(Mh0**INT(4.D0)) - 1.D0*B0(x, Mh02, GaugeXiW&
  &*MW2)*DBLE(Mh0**INT(4.D0)) + B0(x, Mh02, MW2)*DBLE(MW**INT(4.D0)) + B0(x, Mh02, MW2)*DBLE(x**INT(2.D0)) - 1.D0*B0(x, Mh02, Gau&
  &geXiW*MW2)*DBLE(x**INT(2.D0))))/(MW2*PI2*SW2)

 amplitudes(24) = (0.015625D0*CBA*EL2*SBA*(-1.D0*MW2*A0(MHH2) + (-1.D0*MHH2 + MW2 + x)*A0(MW2) + MHH2*A0(GaugeXiW*MW2) + GaugeXiW&
  &*MW2*A0(GaugeXiW*MW2) - 1.D0*x*A0(GaugeXiW*MW2) + 2.D0*MHH2*MW2*B0(x, MHH2, MW2) + 2.D0*MHH2*x*B0(x, MHH2, MW2) + 2.D0*MW2*x*B&
  &0(x, MHH2, MW2) - 2.D0*MHH2*x*B0(x, MHH2, GaugeXiW*MW2) - 1.D0*B0(x, MHH2, MW2)*DBLE(MHH**INT(4.D0)) + B0(x, MHH2, GaugeXiW*MW&
  &2)*DBLE(MHH**INT(4.D0)) - 1.D0*B0(x, MHH2, MW2)*DBLE(MW**INT(4.D0)) - 1.D0*B0(x, MHH2, MW2)*DBLE(x**INT(2.D0)) + B0(x, MHH2, G&
  &augeXiW*MW2)*DBLE(x**INT(2.D0))))/ (MW2*PI2*SW2)

  totalAmplitude = (0D0,0D0)
 do j=1,24
  totalAmplitude = totalAmplitude + amplitudes(j)
 end do
 SelfGpHpUsual = totalAmplitude
end function SelfGpHpUsual

