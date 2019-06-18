double complex function DSelfGpHp(x)
 use constants
 implicit none
#include "looptools.h"
 double precision, intent(in) :: x
 integer :: j
 double complex :: totalAmplitude
 double complex :: amplitudes(24)

 amplitudes(1) = 0.D0

 amplitudes(2) = 0.D0

 amplitudes(3) = 0.D0

 amplitudes(4) = 0.D0

 amplitudes(5) = 0.D0

 amplitudes(6) = 0.D0

 amplitudes(7) = (0.03125D0*EL2*ME2*Yuk6*(-1.D0*B0(x, 0.D0, ME2) + (ME2 - 1.D0*x)*DB0(x, 0.D0, ME2)))/(MW2*PI2*SW2)

 amplitudes(8) = (0.03125D0*EL2*MM2*Yuk6*(-1.D0*B0(x, 0.D0, MM2) + (MM2 - 1.D0*x)*DB0(x, 0.D0, MM2)))/(MW2*PI2*SW2)

 amplitudes(9) = (0.03125D0*EL2*ML2*Yuk6*(-1.D0*B0(x, 0.D0, ML2) + (ML2 - 1.D0*x)*DB0(x, 0.D0, ML2)))/(MW2*PI2*SW2)

 amplitudes(10) = (-0.09375D0*CKM11*CKMC11*EL2*((-1.D0*MU2 + MD2*TB*Yuk3)*B0(x, MD2, MU2) + DB0(x, MD2, MU2)*(-1.D0*MU2*x + MD2*(&
  &TB*x*Yuk3 + MU2*(-1.D0 + TB*Yuk3)) - 1.D0*TB*Yuk3*DBLE(MD**INT(4.D0)) + DBLE(MU**INT(4.D0)))))/ (MW2*PI2*SW2*TB)

 amplitudes(11) = (-0.09375D0*CKM21*CKMC21*EL2*((-1.D0*MC2 + MD2*TB*Yuk3)*B0(x, MC2, MD2) + DB0(x, MC2, MD2)*(MD2*TB*(-1.D0*MD2 +&
  & x)*Yuk3 - 1.D0*MC2*(x + MD2*(1.D0 - 1.D0*TB*Yuk3)) + DBLE(MC**INT(4.D0)))))/(MW2*PI2*SW2*TB)

 amplitudes(12) = (-0.09375D0*CKM31*CKMC31*EL2*((-1.D0*MT2 + MD2*TB*Yuk3)*B0(x, MD2, MT2) + DB0(x, MD2, MT2)*(-1.D0*MT2*x + MD2*(&
  &TB*x*Yuk3 + MT2*(-1.D0 + TB*Yuk3)) - 1.D0*TB*Yuk3*DBLE(MD**INT(4.D0)) + DBLE(MT**INT(4.D0)))))/ (MW2*PI2*SW2*TB)

 amplitudes(13) = (-0.09375D0*CKM12*CKMC12*EL2*((-1.D0*MU2 + MS2*TB*Yuk3)*B0(x, MS2, MU2) + DB0(x, MS2, MU2)*(-1.D0*MU2*x + MS2*(&
  &TB*x*Yuk3 + MU2*(-1.D0 + TB*Yuk3)) - 1.D0*TB*Yuk3*DBLE(MS**INT(4.D0)) + DBLE(MU**INT(4.D0)))))/ (MW2*PI2*SW2*TB)

 amplitudes(14) = (-0.09375D0*CKM22*CKMC22*EL2*((-1.D0*MC2 + MS2*TB*Yuk3)*B0(x, MC2, MS2) + DB0(x, MC2, MS2)*(MS2*TB*(-1.D0*MS2 +&
  & x)*Yuk3 - 1.D0*MC2*(x + MS2*(1.D0 - 1.D0*TB*Yuk3)) + DBLE(MC**INT(4.D0)))))/(MW2*PI2*SW2*TB)

 amplitudes(15) = (-0.09375D0*CKM32*CKMC32*EL2*((-1.D0*MT2 + MS2*TB*Yuk3)*B0(x, MS2, MT2) + DB0(x, MS2, MT2)*(-1.D0*MT2*x + MS2*(&
  &TB*x*Yuk3 + MT2*(-1.D0 + TB*Yuk3)) - 1.D0*TB*Yuk3*DBLE(MS**INT(4.D0)) + DBLE(MT**INT(4.D0)))))/ (MW2*PI2*SW2*TB)

 amplitudes(16) = (-0.09375D0*CKM13*CKMC13*EL2*((-1.D0*MU2 + MB2*TB*Yuk3)*B0(x, MB2, MU2) + DB0(x, MB2, MU2)*(-1.D0*MU2*x + MB2*(&
  &TB*x*Yuk3 + MU2*(-1.D0 + TB*Yuk3)) - 1.D0*TB*Yuk3*DBLE(MB**INT(4.D0)) + DBLE(MU**INT(4.D0)))))/ (MW2*PI2*SW2*TB)

 amplitudes(17) = (-0.09375D0*CKM23*CKMC23*EL2*((-1.D0*MC2 + MB2*TB*Yuk3)*B0(x, MB2, MC2) + DB0(x, MB2, MC2)*(-1.D0*MC2*x + MB2*(&
  &TB*x*Yuk3 + MC2*(-1.D0 + TB*Yuk3)) - 1.D0*TB*Yuk3*DBLE(MB**INT(4.D0)) + DBLE(MC**INT(4.D0)))))/ (MW2*PI2*SW2*TB)

 amplitudes(18) = (-0.09375D0*CKM33*CKMC33*EL2*((-1.D0*MT2 + MB2*TB*Yuk3)*B0(x, MB2, MT2) + DB0(x, MB2, MT2)*(-1.D0*MT2*x + MB2*(&
  &TB*x*Yuk3 + MT2*(-1.D0 + TB*Yuk3)) - 1.D0*TB*Yuk3*DBLE(MB**INT(4.D0)) + DBLE(MT**INT(4.D0)))))/ (MW2*PI2*SW2*TB)

 amplitudes(19) = (0.015625D0*CBA*(Mh02 - 1.D0*MHp2)*(-1.D0*EL2*(Mh02 - 2.D0*MHp2)*S2B*SBA + 2.D0*CAB*(EL2*Mh02 - 2.D0*Lambda5*MW&
  &2*SW2))*DB0(x, Mh02, MHp2))/ (MW2*PI2*S2B*SW2)

 amplitudes(20) = (0.015625D0*(MHH2 - 1.D0*MHp2)*SBA*(CBA*EL2*(MHH2 - 2.D0*MHp2)*S2B - 2.D0*EL2*MHH2*SAB + 4.D0*Lambda5*MW2*SAB*S&
  &W2)*DB0(x, MHH2, MHp2))/ (MW2*PI2*S2B*SW2)

 amplitudes(21) = (0.015625D0*CBA*EL2*Mh02*(Mh02 - 1.D0*MHp2)*SBA*DB0(x, Mh02, GaugeXiW*MW2))/(MW2*PI2*SW2)

 amplitudes(22) = (-0.015625D0*CBA*EL2*MHH2*(MHH2 - 1.D0*MHp2)*SBA*DB0(x, MHH2, GaugeXiW*MW2))/(MW2*PI2*SW2)

 amplitudes(23) = (0.015625D0*CBA*EL2*SBA*(-1.D0*A0(MW2) + A0(GaugeXiW*MW2) - 2.D0*Mh02*B0(x, Mh02, MW2) - 2.D0*MW2*B0(x, Mh02, M&
  &W2) + 2.D0*x*B0(x, Mh02, MW2) + 2.D0*Mh02*B0(x, Mh02, GaugeXiW*MW2) - 2.D0*x*B0(x, Mh02, GaugeXiW*MW2) - 2.D0*Mh02*MW2*DB0(x, &
  &Mh02, MW2) - 2.D0*Mh02*x*DB0(x, Mh02, MW2) - 2.D0*MW2*x*DB0(x, Mh02, MW2) + 2.D0*Mh02*x*DB0(x, Mh02, GaugeXiW*MW2) + DB0(x, Mh&
  &02, MW2)*DBLE(Mh0**INT(4.D0)) - 1.D0*DB0(x, Mh02, GaugeXiW*MW2)*DBLE(Mh0**INT(4.D0)) + DB0(x, Mh02, MW2)*DBLE(MW**INT(4.D0)) +&
  & DB0(x, Mh02, MW2)*DBLE(x**INT(2.D0)) - 1.D0*DB0(x, Mh02, GaugeXiW*MW2)*DBLE(x**INT(2.D0))))/(MW2*PI2*SW2)

 amplitudes(24) = (0.015625D0*CBA*EL2*SBA*(A0(MW2) - 1.D0*A0(GaugeXiW*MW2) + 2.D0*MHH2*B0(x, MHH2, MW2) + 2.D0*MW2*B0(x, MHH2, MW&
  &2) - 2.D0*x*B0(x, MHH2, MW2) - 2.D0*MHH2*B0(x, MHH2, GaugeXiW*MW2) + 2.D0*x*B0(x, MHH2, GaugeXiW*MW2) + 2.D0*MHH2*MW2*DB0(x, M&
  &HH2, MW2) + 2.D0*MHH2*x*DB0(x, MHH2, MW2) + 2.D0*MW2*x*DB0(x, MHH2, MW2) - 2.D0*MHH2*x*DB0(x, MHH2, GaugeXiW*MW2) - 1.D0*DB0(x&
  &, MHH2, MW2)*DBLE(MHH**INT(4.D0)) + DB0(x, MHH2, GaugeXiW*MW2)*DBLE(MHH**INT(4.D0)) - 1.D0*DB0(x, MHH2, MW2)*DBLE(MW**INT(4.D0&
  &)) - 1.D0*DB0(x, MHH2, MW2)*DBLE(x**INT(2.D0)) + DB0(x, MHH2, GaugeXiW*MW2)*DBLE(x**INT(2.D0))))/(MW2*PI2*SW2)

  totalAmplitude = (0D0,0D0)
 do j=1,24
  totalAmplitude = totalAmplitude + amplitudes(j)
 end do
 DSelfGpHp = totalAmplitude
end function DSelfGpHp

