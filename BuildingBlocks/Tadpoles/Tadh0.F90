double complex function Tadh0()
 use constants
 implicit none
#include "looptools.h"
 integer :: j
 double complex :: totalAmplitude
 double complex :: amplitudes(20)

 amplitudes(1) = (-0.125D0*EL*ME2*Yuk4*A0(ME2))/(MW*PI2*SW)

 amplitudes(2) = (-0.125D0*EL*MM2*Yuk4*A0(MM2))/(MW*PI2*SW)

 amplitudes(3) = (-0.125D0*EL*ML2*Yuk4*A0(ML2))/(MW*PI2*SW)

 amplitudes(4) = (-0.375D0*CA*EL*MU2*A0(MU2))/(MW*PI2*SB*SW)

 amplitudes(5) = (-0.375D0*CA*EL*MC2*A0(MC2))/(MW*PI2*SB*SW)

 amplitudes(6) = (-0.375D0*CA*EL*MT2*A0(MT2))/(MW*PI2*SB*SW)

 amplitudes(7) = (-0.375D0*EL*MD2*Yuk1*A0(MD2))/(MW*PI2*SW)

 amplitudes(8) = (-0.375D0*EL*MS2*Yuk1*A0(MS2))/(MW*PI2*SW)

 amplitudes(9) = (-0.375D0*EL*MB2*Yuk1*A0(MB2))/(MW*PI2*SW)

 amplitudes(10) = (0.046875D0*(2.D0*CAB*EL2*Mh02 + EL2*Mh02*S2A*SBA - 4.D0*CAB*CBA2*Lambda5*MW2*SW2)*A0(Mh02))/(EL*MW*PI2*S2B*SW)

 amplitudes(11) = (-0.015625D0*SBA*(EL2*(Mh02 + 2.D0*MHH2)*S2A - 2.D0*Lambda5*MW2*(3.D0*S2A + S2B)*SW2)*A0(MHH2))/(EL*MW*PI2*S2B*&
  &SW)

 amplitudes(12) = (0.015625D0*(EL2*(2.D0*MA02 - 1.D0*Mh02)*S2B*SBA + 2.D0*CAB*(EL2*Mh02 - 2.D0*Lambda5*MW2*SW2))*A0(MA02))/(EL*MW&
  &*PI2*S2B*SW)

 amplitudes(13) = (0.015625D0*EL*Mh02*SBA*A0(GaugeXiZ*MZ2))/(MW*PI2*SW)

 amplitudes(14) = (0.03125D0*(-1.D0*EL2*(Mh02 - 2.D0*MHp2)*S2B*SBA + 2.D0*CAB*(EL2*Mh02 - 2.D0*Lambda5*MW2*SW2))*A0(MHp2))/(EL*MW&
  &*PI2*S2B*SW)

 amplitudes(15) = (0.03125D0*EL*Mh02*SBA*A0(GaugeXiW*MW2))/(MW*PI2*SW)

 amplitudes(16) = (-0.03125D0*EL*GaugeXiZ*MW*SBA*A0(GaugeXiZ*MZ2))/(CW2*PI2*SW)

 amplitudes(17) = (-0.03125D0*EL*GaugeXiW*MW*SBA*A0(GaugeXiW*MW2))/(PI2*SW)

 amplitudes(18) = (-0.03125D0*EL*GaugeXiW*MW*SBA*A0(GaugeXiW*MW2))/(PI2*SW)

 amplitudes(19) = (0.03125D0*EL*MW*SBA*(-2.D0*MZ2 + 3.D0*A0(MZ2) + GaugeXiZ*A0(GaugeXiZ*MZ2)))/(CW2*PI2*SW)

 amplitudes(20) = (0.0625D0*EL*MW*SBA*(-2.D0*MW2 + 3.D0*A0(MW2) + GaugeXiW*A0(GaugeXiW*MW2)))/(PI2*SW)

  totalAmplitude = (0D0,0D0)
 do j=1,20
  totalAmplitude = totalAmplitude + amplitudes(j)
 end do
 Tadh0 = totalAmplitude
end function Tadh0

