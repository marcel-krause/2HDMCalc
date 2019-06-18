double complex function A0toN2N2ProcDepRelVC()
 use constants
 implicit none
#include "looptools.h"
 integer :: j
 double complex :: totalAmplitude
! The following analytic expressions are presented in DOI:10.1007/JHEP11(2018)104 (1808.03466 [hep-ph]).
! We kindly thank A. Denner, S. Dittmaier and J.-N. Lang for sending us these expressions in analytic form.

 totalAmplitude = &
&(0.0625D0*EL2*B0(0.D0, 0.D0, MW2))/(PI2*SW2) + (0.03125D0*CA2*EL2*B0(0.D0, 0.D0, MZ2))/(CW2*PI2*SW2) + (0.03125D0*EL2*SA2*B0(0.D0&
  &, 0.D0, MZ2))/(CW2*PI2*SW2) - (0.015625D0*CA2*EL2*B0(MA02, Mh02, MZ2))/(CW2*PI2*SW2) - (0.015625D0*CA*EL2*SA*SB*B0(MA02, Mh02,&
  & MZ2))/(CB*CW2*PI2*SW2) - (0.015625D0*EL2*SA2*B0(MA02, MHH2, MZ2))/(CW2*PI2*SW2) + (0.015625D0*CA*EL2*SA*SB*B0(MA02, MHH2, MZ2&
  &))/(CB*CW2*PI2*SW2) - (0.03125D0*EL2*B0(MA02, MHp2, MW2))/(PI2*SW2) + (0.03125D0*CA2*EL2*Mh02*C0(0.D0, 0.D0, MA02, Mh02, 0.D0,&
  & MZ2))/(CW2*PI2*SW2) + (0.03125D0*CA*EL2*Mh02*SA*SB*C0(0.D0, 0.D0, MA02, Mh02, 0.D0, MZ2))/(CB*CW2*PI2*SW2) + (0.03125D0*EL2*M&
  &HH2*SA2*C0(0.D0, 0.D0, MA02, MHH2, 0.D0, MZ2))/(CW2*PI2*SW2) - (0.03125D0*CA*EL2*MHH2*SA*SB*C0(0.D0, 0.D0, MA02, MHH2, 0.D0, M&
  &Z2))/(CB*CW2*PI2*SW2) + (0.0625D0*EL2*MHp2*C0(0.D0, 0.D0, MA02, MHp2, 0.D0, MW2))/(PI2*SW2)

A0toN2N2ProcDepRelVC = totalAmplitude
end function A0toN2N2ProcDepRelVC