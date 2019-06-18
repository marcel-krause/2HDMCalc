double complex function SelfG0G0Usual(x)
 use constants
 implicit none
#include "looptools.h"
 double precision, intent(in) :: x
 integer :: j
 double complex :: totalAmplitude
 double complex :: amplitudes(27)

 amplitudes(1) = (-0.0078125D0*(-1.D0*EL2*Mh02*S2B + CBA2*(EL2*(Mh02*S2A - 1.D0*MHH2*S2A - 2.D0*MA02*S2B) + 4.D0*Lambda5*MW2*S2B*&
  &SW2))*A0(Mh02))/(MW2*PI2*S2B*SW2)

 amplitudes(2) = (0.0078125D0*(EL2*((-1.D0*Mh02*S2A + 2.D0*MA02*S2B)*SBA2 + MHH2*(S2B + S2A*SBA2)) - 4.D0*Lambda5*MW2*S2B*SBA2*SW&
  &2)*A0(MHH2))/(MW2*PI2*S2B*SW2)

 amplitudes(3) = (-0.0078125D0*(EL2*(Mh02*(S2A - 3.D0*CBA2*S2B) - 1.D0*MHH2*(S2A + 3.D0*S2B*SBA2)) + 4.D0*Lambda5*MW2*S2B*SW2)*A0&
  &(MA02))/(MW2*PI2*S2B*SW2)

 amplitudes(4) = (0.0234375D0*EL2*(CBA2*MHH2 + Mh02*SBA2)*A0(GaugeXiZ*MZ2))/(MW2*PI2*SW2)

 amplitudes(5) = (-0.015625D0*(CBA2*EL2*MHH2*S2B - 2.D0*CBA*EL2*MHH2*SAB + EL2*(-2.D0*MHp2*S2B + Mh02*SBA*(-2.D0*CAB + S2B*SBA)) &
  &+ 4.D0*Lambda5*MW2*S2B*SW2)* A0(MHp2))/(MW2*PI2*S2B*SW2)

 amplitudes(6) = (0.015625D0*EL2*(CBA2*MHH2 + Mh02*SBA2)*A0(GaugeXiW*MW2))/(MW2*PI2*SW2)

 amplitudes(7) = (0.015625D0*EL2*(-2.D0*MZ2 + 3.D0*A0(MZ2) + GaugeXiZ*A0(GaugeXiZ*MZ2)))/(CW2*PI2*SW2)

 amplitudes(8) = (0.03125D0*EL2*(-2.D0*MW2 + 3.D0*A0(MW2) + GaugeXiW*A0(GaugeXiW*MW2)))/(PI2*SW2)

 amplitudes(9) = (-0.03125D0*EL2*ME2*(2.D0*A0(ME2) - 1.D0*x*B0(x, ME2, ME2)))/(MW2*PI2*SW2)

 amplitudes(10) = (-0.03125D0*EL2*MM2*(2.D0*A0(MM2) - 1.D0*x*B0(x, MM2, MM2)))/(MW2*PI2*SW2)

 amplitudes(11) = (-0.03125D0*EL2*ML2*(2.D0*A0(ML2) - 1.D0*x*B0(x, ML2, ML2)))/(MW2*PI2*SW2)

 amplitudes(12) = (-0.09375D0*EL2*MU2*(2.D0*A0(MU2) - 1.D0*x*B0(x, MU2, MU2)))/(MW2*PI2*SW2)

 amplitudes(13) = (-0.09375D0*EL2*MC2*(2.D0*A0(MC2) - 1.D0*x*B0(x, MC2, MC2)))/(MW2*PI2*SW2)

 amplitudes(14) = (-0.09375D0*EL2*MT2*(2.D0*A0(MT2) - 1.D0*x*B0(x, MT2, MT2)))/(MW2*PI2*SW2)

 amplitudes(15) = (-0.09375D0*EL2*MD2*(2.D0*A0(MD2) - 1.D0*x*B0(x, MD2, MD2)))/(MW2*PI2*SW2)

 amplitudes(16) = (-0.09375D0*EL2*MS2*(2.D0*A0(MS2) - 1.D0*x*B0(x, MS2, MS2)))/(MW2*PI2*SW2)

 amplitudes(17) = (-0.09375D0*EL2*MB2*(2.D0*A0(MB2) - 1.D0*x*B0(x, MB2, MB2)))/(MW2*PI2*SW2)

 amplitudes(18) = (0.015625D0*CBA2*EL2*B0(x, MA02, Mh02)*DBLE((MA02 - 1.D0*Mh02)**INT(2.D0)))/(MW2*PI2*SW2)

 amplitudes(19) = (0.015625D0*EL2*SBA2*B0(x, MA02, MHH2)*DBLE((MA02 - 1.D0*MHH2)**INT(2.D0)))/(MW2*PI2*SW2)

 amplitudes(20) = (0.015625D0*EL2*SBA2*B0(x, Mh02, GaugeXiZ*MZ2)*DBLE(Mh0**INT(4.D0)))/(MW2*PI2*SW2)

 amplitudes(21) = (0.015625D0*CBA2*EL2*B0(x, MHH2, GaugeXiZ*MZ2)*DBLE(MHH**INT(4.D0)))/(MW2*PI2*SW2)

 amplitudes(22) = (0.015625D0*EL2*MW2*B0(x, GaugeXiW*MW2, GaugeXiW*MW2)*DBLE(GaugeXiW**INT(2.D0)))/(PI2*SW2)

 amplitudes(23) = (0.015625D0*EL2*MW2*B0(x, GaugeXiW*MW2, GaugeXiW*MW2)*DBLE(GaugeXiW**INT(2.D0)))/(PI2*SW2)

 amplitudes(24) = (0.015625D0*EL2*SBA2*(MZ2*A0(Mh02) + (Mh02 - 1.D0*MZ2 - 1.D0*x)*A0(MZ2) - 1.D0*Mh02*A0(GaugeXiZ*MZ2) - 1.D0*Gau&
  &geXiZ*MZ2*A0(GaugeXiZ*MZ2) + x*A0(GaugeXiZ*MZ2) - 2.D0*Mh02*MZ2*B0(x, Mh02, MZ2) - 2.D0*Mh02*x*B0(x, Mh02, MZ2) - 2.D0*MZ2*x*B&
  &0(x, Mh02, MZ2) + 2.D0*Mh02*x*B0(x, Mh02, GaugeXiZ*MZ2) + B0(x, Mh02, MZ2)*DBLE(Mh0**INT(4.D0)) - 1.D0*B0(x, Mh02, GaugeXiZ*MZ&
  &2)*DBLE(Mh0**INT(4.D0)) + B0(x, Mh02, MZ2)*DBLE(MZ**INT(4.D0)) + B0(x, Mh02, MZ2)*DBLE(x**INT(2.D0)) - 1.D0*B0(x, Mh02, GaugeX&
  &iZ*MZ2)*DBLE(x**INT(2.D0))))/ (CW2*MZ2*PI2*SW2)

 amplitudes(25) = (0.015625D0*CBA2*EL2*(MZ2*A0(MHH2) + (MHH2 - 1.D0*MZ2 - 1.D0*x)*A0(MZ2) - 1.D0*MHH2*A0(GaugeXiZ*MZ2) - 1.D0*Gau&
  &geXiZ*MZ2*A0(GaugeXiZ*MZ2) + x*A0(GaugeXiZ*MZ2) - 2.D0*MHH2*MZ2*B0(x, MHH2, MZ2) - 2.D0*MHH2*x*B0(x, MHH2, MZ2) - 2.D0*MZ2*x*B&
  &0(x, MHH2, MZ2) + 2.D0*MHH2*x*B0(x, MHH2, GaugeXiZ*MZ2) + B0(x, MHH2, MZ2)*DBLE(MHH**INT(4.D0)) - 1.D0*B0(x, MHH2, GaugeXiZ*MZ&
  &2)*DBLE(MHH**INT(4.D0)) + B0(x, MHH2, MZ2)*DBLE(MZ**INT(4.D0)) + B0(x, MHH2, MZ2)*DBLE(x**INT(2.D0)) - 1.D0*B0(x, MHH2, GaugeX&
  &iZ*MZ2)*DBLE(x**INT(2.D0))))/ (CW2*MZ2*PI2*SW2)

 amplitudes(26) = (0.015625D0*EL2*((-1.D0*MW2 + GaugeXiW*MW2 - 1.D0*x)*A0(MW2) + (MW2 - 2.D0*GaugeXiW*MW2 + x)*A0(GaugeXiW*MW2) -&
  & 2.D0*MW2*x*B0(x, MW2, GaugeXiW*MW2) - 2.D0*GaugeXiW*MW2*x*B0(x, MW2, GaugeXiW*MW2) + 2.D0*GaugeXiW*MW2*x*B0(x, GaugeXiW*MW2, &
  &GaugeXiW*MW2) + B0(x, MW2, GaugeXiW*MW2)*DBLE(MW**INT(4.D0)) - 2.D0*GaugeXiW*B0(x, MW2, GaugeXiW*MW2)*DBLE(MW**INT(4.D0)) + B0&
  &(x, MW2, GaugeXiW*MW2)*DBLE(GaugeXiW**INT(2.D0))*DBLE(MW**INT(4.D0)) - 1.D0*B0(x, GaugeXiW*MW2, GaugeXiW*MW2)*DBLE(GaugeXiW**I&
  &NT(2.D0))*DBLE(MW**INT(4.D0)) + B0(x, MW2, GaugeXiW*MW2)*DBLE(x**INT(2.D0)) - 1.D0*B0(x, GaugeXiW*MW2, GaugeXiW*MW2)*DBLE(x**I&
  &NT(2.D0))))/(MW2*PI2*SW2)

 amplitudes(27) = (0.015625D0*EL2*((-1.D0*MW2 + GaugeXiW*MW2 - 1.D0*x)*A0(MW2) + (MW2 - 2.D0*GaugeXiW*MW2 + x)*A0(GaugeXiW*MW2) -&
  & 2.D0*MW2*x*B0(x, MW2, GaugeXiW*MW2) - 2.D0*GaugeXiW*MW2*x*B0(x, MW2, GaugeXiW*MW2) + 2.D0*GaugeXiW*MW2*x*B0(x, GaugeXiW*MW2, &
  &GaugeXiW*MW2) + B0(x, MW2, GaugeXiW*MW2)*DBLE(MW**INT(4.D0)) - 2.D0*GaugeXiW*B0(x, MW2, GaugeXiW*MW2)*DBLE(MW**INT(4.D0)) + B0&
  &(x, MW2, GaugeXiW*MW2)*DBLE(GaugeXiW**INT(2.D0))*DBLE(MW**INT(4.D0)) - 1.D0*B0(x, GaugeXiW*MW2, GaugeXiW*MW2)*DBLE(GaugeXiW**I&
  &NT(2.D0))*DBLE(MW**INT(4.D0)) + B0(x, MW2, GaugeXiW*MW2)*DBLE(x**INT(2.D0)) - 1.D0*B0(x, GaugeXiW*MW2, GaugeXiW*MW2)*DBLE(x**I&
  &NT(2.D0))))/(MW2*PI2*SW2)

  totalAmplitude = (0D0,0D0)
 do j=1,27
  totalAmplitude = totalAmplitude + amplitudes(j)
 end do
 SelfG0G0Usual = totalAmplitude
end function SelfG0G0Usual

