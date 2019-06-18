double complex function h0toZ0Z0Tad()
 use constants
 implicit none
#include "looptools.h"
 integer :: j
 double complex :: totalAmplitude
 double complex :: amplitudes(20)

 amplitudes(1) = (-0.0625D0*ME2*SBA*Yuk4*A0(ME2)*DBLE(CW**INT(-4.D0))*DBLE(EL**INT(4.D0))*(3.D0 - (1.D0*Mh02)/MZ2 + 0.25D0*DBLE(M&
  &h0**INT(4.D0))*DBLE(MZ**INT(-4.D0)))* DBLE(SW**INT(-4.D0)))/(Mh02*PI2)

 amplitudes(2) = (-0.0625D0*MM2*SBA*Yuk4*A0(MM2)*DBLE(CW**INT(-4.D0))*DBLE(EL**INT(4.D0))*(3.D0 - (1.D0*Mh02)/MZ2 + 0.25D0*DBLE(M&
  &h0**INT(4.D0))*DBLE(MZ**INT(-4.D0)))* DBLE(SW**INT(-4.D0)))/(Mh02*PI2)

 amplitudes(3) = (-0.0625D0*ML2*SBA*Yuk4*A0(ML2)*DBLE(CW**INT(-4.D0))*DBLE(EL**INT(4.D0))*(3.D0 - (1.D0*Mh02)/MZ2 + 0.25D0*DBLE(M&
  &h0**INT(4.D0))*DBLE(MZ**INT(-4.D0)))* DBLE(SW**INT(-4.D0)))/(Mh02*PI2)

 amplitudes(4) = (-0.1875D0*CA*MU2*SBA*A0(MU2)*DBLE(CW**INT(-4.D0))*DBLE(EL**INT(4.D0))*(3.D0 - (1.D0*Mh02)/MZ2 + 0.25D0*DBLE(Mh0&
  &**INT(4.D0))*DBLE(MZ**INT(-4.D0)))* DBLE(SW**INT(-4.D0)))/(Mh02*PI2*SB)

 amplitudes(5) = (-0.1875D0*CA*MC2*SBA*A0(MC2)*DBLE(CW**INT(-4.D0))*DBLE(EL**INT(4.D0))*(3.D0 - (1.D0*Mh02)/MZ2 + 0.25D0*DBLE(Mh0&
  &**INT(4.D0))*DBLE(MZ**INT(-4.D0)))* DBLE(SW**INT(-4.D0)))/(Mh02*PI2*SB)

 amplitudes(6) = (-0.1875D0*CA*MT2*SBA*A0(MT2)*DBLE(CW**INT(-4.D0))*DBLE(EL**INT(4.D0))*(3.D0 - (1.D0*Mh02)/MZ2 + 0.25D0*DBLE(Mh0&
  &**INT(4.D0))*DBLE(MZ**INT(-4.D0)))* DBLE(SW**INT(-4.D0)))/(Mh02*PI2*SB)

 amplitudes(7) = (-0.1875D0*MD2*SBA*Yuk1*A0(MD2)*DBLE(CW**INT(-4.D0))*DBLE(EL**INT(4.D0))*(3.D0 - (1.D0*Mh02)/MZ2 + 0.25D0*DBLE(M&
  &h0**INT(4.D0))*DBLE(MZ**INT(-4.D0)))* DBLE(SW**INT(-4.D0)))/(Mh02*PI2)

 amplitudes(8) = (-0.1875D0*MS2*SBA*Yuk1*A0(MS2)*DBLE(CW**INT(-4.D0))*DBLE(EL**INT(4.D0))*(3.D0 - (1.D0*Mh02)/MZ2 + 0.25D0*DBLE(M&
  &h0**INT(4.D0))*DBLE(MZ**INT(-4.D0)))* DBLE(SW**INT(-4.D0)))/(Mh02*PI2)

 amplitudes(9) = (-0.1875D0*MB2*SBA*Yuk1*A0(MB2)*DBLE(CW**INT(-4.D0))*DBLE(EL**INT(4.D0))*(3.D0 - (1.D0*Mh02)/MZ2 + 0.25D0*DBLE(M&
  &h0**INT(4.D0))*DBLE(MZ**INT(-4.D0)))* DBLE(SW**INT(-4.D0)))/(Mh02*PI2)

 amplitudes(10) = (0.0234375D0*EL2*SBA*(2.D0*CAB*EL2*Mh02 + EL2*Mh02*S2A*SBA - 4.D0*CAB*CBA2*Lambda5*MW2*SW2)*A0(Mh02)*DBLE(CW**I&
  &NT(-4.D0))* (3.D0 - (1.D0*Mh02)/MZ2 + 0.25D0*DBLE(Mh0**INT(4.D0))*DBLE(MZ**INT(-4.D0)))*DBLE(SW**INT(-4.D0)))/(Mh02*PI2*S2B)

 amplitudes(11) = (-0.0078125D0*EL2*SBA2*(EL2*Mh02*S2A + 2.D0*EL2*MHH2*S2A - 6.D0*Lambda5*MW2*S2A*SW2 - 2.D0*Lambda5*MW2*S2B*SW2)&
  &*A0(MHH2)*DBLE(CW**INT(-4.D0))* (3.D0 - (1.D0*Mh02)/MZ2 + 0.25D0*DBLE(Mh0**INT(4.D0))*DBLE(MZ**INT(-4.D0)))*DBLE(SW**INT(-4.D0&
  &)))/(Mh02*PI2*S2B)

 amplitudes(12) = (0.0078125D0*EL2*SBA*(2.D0*CAB*EL2*Mh02 + 2.D0*EL2*MA02*S2B*SBA - 1.D0*EL2*Mh02*S2B*SBA - 4.D0*CAB*Lambda5*MW2*&
  &SW2)*A0(MA02)*DBLE(CW**INT(-4.D0))* (3.D0 - (1.D0*Mh02)/MZ2 + 0.25D0*DBLE(Mh0**INT(4.D0))*DBLE(MZ**INT(-4.D0)))*DBLE(SW**INT(-&
  &4.D0)))/(Mh02*PI2*S2B)

 amplitudes(13) = (0.0078125D0*SBA2*A0(GaugeXiZ*MZ2)*DBLE(CW**INT(-4.D0))*DBLE(EL**INT(4.D0))*(3.D0 - (1.D0*Mh02)/MZ2 + 0.25D0*DB&
  &LE(Mh0**INT(4.D0))*DBLE(MZ**INT(-4.D0)))* DBLE(SW**INT(-4.D0)))/PI2

 amplitudes(14) = (0.015625D0*EL2*SBA*(2.D0*CAB*EL2*Mh02 - 1.D0*EL2*Mh02*S2B*SBA + 2.D0*EL2*MHp2*S2B*SBA - 4.D0*CAB*Lambda5*MW2*S&
  &W2)*A0(MHp2)*DBLE(CW**INT(-4.D0))* (3.D0 - (1.D0*Mh02)/MZ2 + 0.25D0*DBLE(Mh0**INT(4.D0))*DBLE(MZ**INT(-4.D0)))*DBLE(SW**INT(-4&
  &.D0)))/(Mh02*PI2*S2B)

 amplitudes(15) = (0.015625D0*SBA2*A0(GaugeXiW*MW2)*DBLE(CW**INT(-4.D0))*DBLE(EL**INT(4.D0))*(3.D0 - (1.D0*Mh02)/MZ2 + 0.25D0*DBL&
  &E(Mh0**INT(4.D0))*DBLE(MZ**INT(-4.D0)))* DBLE(SW**INT(-4.D0)))/PI2

 amplitudes(16) = (-0.015625D0*GaugeXiZ*MW2*SBA2*A0(GaugeXiZ*MZ2)*DBLE(CW**INT(-6.D0))*DBLE(EL**INT(4.D0))* (3.D0 - (1.D0*Mh02)/M&
  &Z2 + 0.25D0*DBLE(Mh0**INT(4.D0))*DBLE(MZ**INT(-4.D0)))*DBLE(SW**INT(-4.D0)))/(Mh02*PI2)

 amplitudes(17) = (-0.015625D0*GaugeXiW*MW2*SBA2*A0(GaugeXiW*MW2)*DBLE(CW**INT(-4.D0))*DBLE(EL**INT(4.D0))* (3.D0 - (1.D0*Mh02)/M&
  &Z2 + 0.25D0*DBLE(Mh0**INT(4.D0))*DBLE(MZ**INT(-4.D0)))*DBLE(SW**INT(-4.D0)))/(Mh02*PI2)

 amplitudes(18) = (-0.015625D0*GaugeXiW*MW2*SBA2*A0(GaugeXiW*MW2)*DBLE(CW**INT(-4.D0))*DBLE(EL**INT(4.D0))* (3.D0 - (1.D0*Mh02)/M&
  &Z2 + 0.25D0*DBLE(Mh0**INT(4.D0))*DBLE(MZ**INT(-4.D0)))*DBLE(SW**INT(-4.D0)))/(Mh02*PI2)

 amplitudes(19) = ((0.D0 + 1.D0*I)*EL*MW*PI2*SBA*(3.D0 - (1.D0*Mh02)/MZ2 + 0.25D0*DBLE(Mh0**INT(4.D0))*DBLE(MZ**INT(-4.D0)))* (((&
  &0.D0 + 0.03125D0*I)*MW*MZ2*SBA*DBLE(CW**INT(-4.D0))*DBLE(EL**INT(3.D0))*DBLE(PI**INT(-4.D0))*DBLE(SW**INT(-3.D0)))/Mh02 - ((0.&
  &D0 + 0.046875D0*I)*MW*SBA*A0(MZ2)*DBLE(CW**INT(-4.D0))*DBLE(EL**INT(3.D0))*DBLE(PI**INT(-4.D0))*DBLE(SW**INT(-3.D0)))/Mh02 - (&
  &(0.D0 + 0.015625D0*I)*GaugeXiZ*MW*SBA*A0(GaugeXiZ*MZ2)*DBLE(CW**INT(-4.D0))*DBLE(EL**INT(3.D0))*DBLE(PI**INT(-4.D0))*DBLE(SW**&
  &INT(-3.D0)))/ Mh02))/(CW2*SW)

 amplitudes(20) = ((0.D0 + 1.D0*I)*EL*MW*PI2*SBA*(3.D0 - (1.D0*Mh02)/MZ2 + 0.25D0*DBLE(Mh0**INT(4.D0))*DBLE(MZ**INT(-4.D0)))* (((&
  &0.D0 - 0.09375D0*I)*MW*SBA*A0(MW2)*DBLE(EL**INT(3.D0))*DBLE(PI**INT(-4.D0))*DBLE(SW**INT(-3.D0)))/(CW2*Mh02) - ((0.D0 + 0.0312&
  &5D0*I)*GaugeXiW*MW*SBA*A0(GaugeXiW*MW2)*DBLE(EL**INT(3.D0))*DBLE(PI**INT(-4.D0))*DBLE(SW**INT(-3.D0)))/(CW2*Mh02) + ((0.D0 + 0&
  &.0625D0*I)*SBA*DBLE(EL**INT(3.D0))*DBLE(MW**INT(3.D0))*DBLE(PI**INT(-4.D0))*DBLE(SW**INT(-3.D0)))/(CW2*Mh02)))/(CW2*SW)

  totalAmplitude = (0D0,0D0)
 do j=1,20
  totalAmplitude = totalAmplitude + amplitudes(j)
 end do

 h0toZ0Z0Tad = totalAmplitude
end function h0toZ0Z0Tad