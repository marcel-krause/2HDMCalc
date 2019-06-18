double precision function h0toHHHHCT(x)
 use constants
 use counterterms
 implicit none
#include "looptools.h"
 integer, intent(in) :: x
 double precision :: totalAmplitude

 select case (x)
	case (1)
		totalAmplitude = (0.25D0*EL2*SBA2*DBLE(((Mh02 + 2.D0*MHH2)*S2A - (2.D0*Lambda5*MW2*(3.D0*S2A + S2B)*SW2)/EL2)&
		&**INT(2.D0)))/(MW2*S2B2*SW2)*( dZHHHHOS() + dZh0h0OS()/2D0 + dgAtMZ()/(EL/SW) - dMW2Usual()/(2D0*MW2) + &
		&CBA/SBA*(dBeta1KanUsual() - dAlphaKanUsual()) - 2D0*C2B/S2B*dBeta1KanUsual() ) + &
  &((EL*SBA*((Mh02 + 2D0*MHH2)*S2A - (2D0*Lambda5*MW2*(3D0*S2A + S2B)*SW2)/EL2))/(2D0*MW*S2B*SW))*(&
	&(3D0*EL/(2D0*MW*SW*S2B)*(4D0*Lambda5*MW2*SW2*SAB*SBA2/EL2 + MHH2*(CBA*S2A - 2D0*SAB)))*( dZHHh0OSUsual()/2D0 ) +&
	&(-(CBA*(EL2*(2D0*Mh02 + MHH2)*S2A + 2D0*Lambda5*MW2*(-3D0*S2A + S2B)*SW2))/(2D0*EL*MW*S2B*SW))*( dZh0HHOSUsual() ) + &
	&EL*SBA/(2D0*MW*S2B*SW)*(&
		& S2A*(dMh02OSUsual() + 2D0*dMHH2OSUsual()) + 2D0*C2A*dAlphaKanUsual()*(Mh02 + 2D0*MHH2) - &
		& 2D0*dm122MSBarUsual()/S2B*(3D0*S2A + S2B) + 4D0*Lambda5*MW2*SW2/EL2*C2B/S2B*(3D0*S2A + S2B)*dBeta1KanUsual() - &
		& 2D0*Lambda5*MW2*SW2/EL2*(6D0*C2A*dAlphaKanUsual() + 2D0*C2B*dBeta1KanUsual()) &
		&) &
  &)
	case (2)
		totalAmplitude = (0.25D0*EL2*SBA2*DBLE(((Mh02 + 2.D0*MHH2)*S2A - (2.D0*Lambda5*MW2*(3.D0*S2A + S2B)*SW2)/EL2)&
		&**INT(2.D0)))/(MW2*S2B2*SW2)*( dZHHHHOS() + dZh0h0OS()/2D0 + dgAtMZ()/(EL/SW) - dMW2Usual()/(2D0*MW2) + &
		&CBA/SBA*(dBeta2KanUsual() - dAlphaKanUsual()) - 2D0*C2B/S2B*dBeta2KanUsual() ) + &
  &((EL*SBA*((Mh02 + 2D0*MHH2)*S2A - (2D0*Lambda5*MW2*(3D0*S2A + S2B)*SW2)/EL2))/(2D0*MW*S2B*SW))*(&
	&(3D0*EL/(2D0*MW*SW*S2B)*(4D0*Lambda5*MW2*SW2*SAB*SBA2/EL2 + MHH2*(CBA*S2A - 2D0*SAB)))*( dZHHh0OSUsual()/2D0 ) +&
	&(-(CBA*(EL2*(2D0*Mh02 + MHH2)*S2A + 2D0*Lambda5*MW2*(-3D0*S2A + S2B)*SW2))/(2D0*EL*MW*S2B*SW))*( dZh0HHOSUsual() ) + &
	&EL*SBA/(2D0*MW*S2B*SW)*(&
		& S2A*(dMh02OSUsual() + 2D0*dMHH2OSUsual()) + 2D0*C2A*dAlphaKanUsual()*(Mh02 + 2D0*MHH2) - &
		& 2D0*dm122MSBarUsual()/S2B*(3D0*S2A + S2B) + 4D0*Lambda5*MW2*SW2/EL2*C2B/S2B*(3D0*S2A + S2B)*dBeta2KanUsual() - &
		& 2D0*Lambda5*MW2*SW2/EL2*(6D0*C2A*dAlphaKanUsual() + 2D0*C2B*dBeta2KanUsual()) &
		&) &
  &)
	case (3)
		totalAmplitude = (0.25D0*EL2*SBA2*DBLE(((Mh02 + 2.D0*MHH2)*S2A - (2.D0*Lambda5*MW2*(3.D0*S2A + S2B)*SW2)/EL2)&
		&**INT(2.D0)))/(MW2*S2B2*SW2)*( dZHHHHOS() + dZh0h0OS()/2D0 + dgAtMZ()/(EL/SW) - dMW2Alter()/(2D0*MW2) + &
		&CBA/SBA*(dBeta1KanAlter() - dAlphaKanAlter()) - 2D0*C2B/S2B*dBeta1KanAlter() ) + &
  &((EL*SBA*((Mh02 + 2D0*MHH2)*S2A - (2D0*Lambda5*MW2*(3D0*S2A + S2B)*SW2)/EL2))/(2D0*MW*S2B*SW))*(&
	&(3D0*EL/(2D0*MW*SW*S2B)*(4D0*Lambda5*MW2*SW2*SAB*SBA2/EL2 + MHH2*(CBA*S2A - 2D0*SAB)))*( dZHHh0OSAlter()/2D0 ) +&
	&(-(CBA*(EL2*(2D0*Mh02 + MHH2)*S2A + 2D0*Lambda5*MW2*(-3D0*S2A + S2B)*SW2))/(2D0*EL*MW*S2B*SW))*( dZh0HHOSAlter() ) + &
	&EL*SBA/(2D0*MW*S2B*SW)*(&
		& S2A*(dMh02OSAlter() + 2D0*dMHH2OSAlter()) + 2D0*C2A*dAlphaKanAlter()*(Mh02 + 2D0*MHH2) - &
		& 2D0*dm122MSBarAlter()/S2B*(3D0*S2A + S2B) + 4D0*Lambda5*MW2*SW2/EL2*C2B/S2B*(3D0*S2A + S2B)*dBeta1KanAlter() - &
		& 2D0*Lambda5*MW2*SW2/EL2*(6D0*C2A*dAlphaKanAlter() + 2D0*C2B*dBeta1KanAlter()) &
		&) &
  &)
	case (4)
		totalAmplitude = (0.25D0*EL2*SBA2*DBLE(((Mh02 + 2.D0*MHH2)*S2A - (2.D0*Lambda5*MW2*(3.D0*S2A + S2B)*SW2)/EL2)&
		&**INT(2.D0)))/(MW2*S2B2*SW2)*( dZHHHHOS() + dZh0h0OS()/2D0 + dgAtMZ()/(EL/SW) - dMW2Alter()/(2D0*MW2) + &
		&CBA/SBA*(dBeta2KanAlter() - dAlphaKanAlter()) - 2D0*C2B/S2B*dBeta2KanAlter() ) + &
  &((EL*SBA*((Mh02 + 2D0*MHH2)*S2A - (2D0*Lambda5*MW2*(3D0*S2A + S2B)*SW2)/EL2))/(2D0*MW*S2B*SW))*(&
	&(3D0*EL/(2D0*MW*SW*S2B)*(4D0*Lambda5*MW2*SW2*SAB*SBA2/EL2 + MHH2*(CBA*S2A - 2D0*SAB)))*( dZHHh0OSAlter()/2D0 ) +&
	&(-(CBA*(EL2*(2D0*Mh02 + MHH2)*S2A + 2D0*Lambda5*MW2*(-3D0*S2A + S2B)*SW2))/(2D0*EL*MW*S2B*SW))*( dZh0HHOSAlter() ) + &
	&EL*SBA/(2D0*MW*S2B*SW)*(&
		& S2A*(dMh02OSAlter() + 2D0*dMHH2OSAlter()) + 2D0*C2A*dAlphaKanAlter()*(Mh02 + 2D0*MHH2) - &
		& 2D0*dm122MSBarAlter()/S2B*(3D0*S2A + S2B) + 4D0*Lambda5*MW2*SW2/EL2*C2B/S2B*(3D0*S2A + S2B)*dBeta2KanAlter() - &
		& 2D0*Lambda5*MW2*SW2/EL2*(6D0*C2A*dAlphaKanAlter() + 2D0*C2B*dBeta2KanAlter()) &
		&) &
  &)
	case (5)
		totalAmplitude = (0.25D0*EL2*SBA2*DBLE(((Mh02 + 2.D0*MHH2)*S2A - (2.D0*Lambda5*MW2*(3.D0*S2A + S2B)*SW2)/EL2)&
		&**INT(2.D0)))/(MW2*S2B2*SW2)*( dZHHHHOS() + dZh0h0OS()/2D0 + dgAtMZ()/(EL/SW) - dMW2Alter()/(2D0*MW2) + &
		&CBA/SBA*(dBeta1PinchPStar() - dAlphaPinchPStar()) - 2D0*C2B/S2B*dBeta1PinchPStar() ) + &
  &((EL*SBA*((Mh02 + 2D0*MHH2)*S2A - (2D0*Lambda5*MW2*(3D0*S2A + S2B)*SW2)/EL2))/(2D0*MW*S2B*SW))*(&
	&(3D0*EL/(2D0*MW*SW*S2B)*(4D0*Lambda5*MW2*SW2*SAB*SBA2/EL2 + MHH2*(CBA*S2A - 2D0*SAB)))*( dZHHh0OSAlter()/2D0 ) +&
	&(-(CBA*(EL2*(2D0*Mh02 + MHH2)*S2A + 2D0*Lambda5*MW2*(-3D0*S2A + S2B)*SW2))/(2D0*EL*MW*S2B*SW))*( dZh0HHOSAlter() ) + &
	&EL*SBA/(2D0*MW*S2B*SW)*(&
		& S2A*(dMh02OSAlter() + 2D0*dMHH2OSAlter()) + 2D0*C2A*dAlphaPinchPStar()*(Mh02 + 2D0*MHH2) - &
		& 2D0*dm122MSBarAlter()/S2B*(3D0*S2A + S2B) + 4D0*Lambda5*MW2*SW2/EL2*C2B/S2B*(3D0*S2A + S2B)*dBeta1PinchPStar() - &
		& 2D0*Lambda5*MW2*SW2/EL2*(6D0*C2A*dAlphaPinchPStar() + 2D0*C2B*dBeta1PinchPStar()) &
		&) &
  &)
	case (6)
		totalAmplitude = (0.25D0*EL2*SBA2*DBLE(((Mh02 + 2.D0*MHH2)*S2A - (2.D0*Lambda5*MW2*(3.D0*S2A + S2B)*SW2)/EL2)&
		&**INT(2.D0)))/(MW2*S2B2*SW2)*( dZHHHHOS() + dZh0h0OS()/2D0 + dgAtMZ()/(EL/SW) - dMW2Alter()/(2D0*MW2) + &
		&CBA/SBA*(dBeta2PinchPStar() - dAlphaPinchPStar()) - 2D0*C2B/S2B*dBeta2PinchPStar() ) + &
  &((EL*SBA*((Mh02 + 2D0*MHH2)*S2A - (2D0*Lambda5*MW2*(3D0*S2A + S2B)*SW2)/EL2))/(2D0*MW*S2B*SW))*(&
	&(3D0*EL/(2D0*MW*SW*S2B)*(4D0*Lambda5*MW2*SW2*SAB*SBA2/EL2 + MHH2*(CBA*S2A - 2D0*SAB)))*( dZHHh0OSAlter()/2D0 ) +&
	&(-(CBA*(EL2*(2D0*Mh02 + MHH2)*S2A + 2D0*Lambda5*MW2*(-3D0*S2A + S2B)*SW2))/(2D0*EL*MW*S2B*SW))*( dZh0HHOSAlter() ) + &
	&EL*SBA/(2D0*MW*S2B*SW)*(&
		& S2A*(dMh02OSAlter() + 2D0*dMHH2OSAlter()) + 2D0*C2A*dAlphaPinchPStar()*(Mh02 + 2D0*MHH2) - &
		& 2D0*dm122MSBarAlter()/S2B*(3D0*S2A + S2B) + 4D0*Lambda5*MW2*SW2/EL2*C2B/S2B*(3D0*S2A + S2B)*dBeta2PinchPStar() - &
		& 2D0*Lambda5*MW2*SW2/EL2*(6D0*C2A*dAlphaPinchPStar() + 2D0*C2B*dBeta2PinchPStar()) &
		&) &
  &)
	case (7)
		totalAmplitude = (0.25D0*EL2*SBA2*DBLE(((Mh02 + 2.D0*MHH2)*S2A - (2.D0*Lambda5*MW2*(3.D0*S2A + S2B)*SW2)/EL2)&
		&**INT(2.D0)))/(MW2*S2B2*SW2)*( dZHHHHOS() + dZh0h0OS()/2D0 + dgAtMZ()/(EL/SW) - dMW2Alter()/(2D0*MW2) + &
		&CBA/SBA*(dBeta1PinchOS() - dAlphaPinchOS()) - 2D0*C2B/S2B*dBeta1PinchOS() ) + &
  &((EL*SBA*((Mh02 + 2D0*MHH2)*S2A - (2D0*Lambda5*MW2*(3D0*S2A + S2B)*SW2)/EL2))/(2D0*MW*S2B*SW))*(&
	&(3D0*EL/(2D0*MW*SW*S2B)*(4D0*Lambda5*MW2*SW2*SAB*SBA2/EL2 + MHH2*(CBA*S2A - 2D0*SAB)))*( dZHHh0OSAlter()/2D0 ) +&
	&(-(CBA*(EL2*(2D0*Mh02 + MHH2)*S2A + 2D0*Lambda5*MW2*(-3D0*S2A + S2B)*SW2))/(2D0*EL*MW*S2B*SW))*( dZh0HHOSAlter() ) + &
	&EL*SBA/(2D0*MW*S2B*SW)*(&
		& S2A*(dMh02OSAlter() + 2D0*dMHH2OSAlter()) + 2D0*C2A*dAlphaPinchOS()*(Mh02 + 2D0*MHH2) - &
		& 2D0*dm122MSBarAlter()/S2B*(3D0*S2A + S2B) + 4D0*Lambda5*MW2*SW2/EL2*C2B/S2B*(3D0*S2A + S2B)*dBeta1PinchOS() - &
		& 2D0*Lambda5*MW2*SW2/EL2*(6D0*C2A*dAlphaPinchOS() + 2D0*C2B*dBeta1PinchOS()) &
		&) &
  &)
	case (8)
		totalAmplitude = (0.25D0*EL2*SBA2*DBLE(((Mh02 + 2.D0*MHH2)*S2A - (2.D0*Lambda5*MW2*(3.D0*S2A + S2B)*SW2)/EL2)&
		&**INT(2.D0)))/(MW2*S2B2*SW2)*( dZHHHHOS() + dZh0h0OS()/2D0 + dgAtMZ()/(EL/SW) - dMW2Alter()/(2D0*MW2) + &
		&CBA/SBA*(dBeta2PinchOS() - dAlphaPinchOS()) - 2D0*C2B/S2B*dBeta2PinchOS() ) + &
  &((EL*SBA*((Mh02 + 2D0*MHH2)*S2A - (2D0*Lambda5*MW2*(3D0*S2A + S2B)*SW2)/EL2))/(2D0*MW*S2B*SW))*(&
	&(3D0*EL/(2D0*MW*SW*S2B)*(4D0*Lambda5*MW2*SW2*SAB*SBA2/EL2 + MHH2*(CBA*S2A - 2D0*SAB)))*( dZHHh0OSAlter()/2D0 ) +&
	&(-(CBA*(EL2*(2D0*Mh02 + MHH2)*S2A + 2D0*Lambda5*MW2*(-3D0*S2A + S2B)*SW2))/(2D0*EL*MW*S2B*SW))*( dZh0HHOSAlter() ) + &
	&EL*SBA/(2D0*MW*S2B*SW)*(&
		& S2A*(dMh02OSAlter() + 2D0*dMHH2OSAlter()) + 2D0*C2A*dAlphaPinchOS()*(Mh02 + 2D0*MHH2) - &
		& 2D0*dm122MSBarAlter()/S2B*(3D0*S2A + S2B) + 4D0*Lambda5*MW2*SW2/EL2*C2B/S2B*(3D0*S2A + S2B)*dBeta2PinchOS() - &
		& 2D0*Lambda5*MW2*SW2/EL2*(6D0*C2A*dAlphaPinchOS() + 2D0*C2B*dBeta2PinchOS()) &
		&) &
  &)
	case (9)
		totalAmplitude = (0.25D0*EL2*SBA2*DBLE(((Mh02 + 2.D0*MHH2)*S2A - (2.D0*Lambda5*MW2*(3.D0*S2A + S2B)*SW2)/EL2)&
		&**INT(2.D0)))/(MW2*S2B2*SW2)*( dZHHHHOS() + dZh0h0OS()/2D0 + dgAtMZ()/(EL/SW) - dMW2Alter()/(2D0*MW2) + &
		&CBA/SBA*(dBetaProcDep1Alter() - dAlphaProcDep1Alter()) - 2D0*C2B/S2B*dBetaProcDep1Alter() ) + &
  &((EL*SBA*((Mh02 + 2D0*MHH2)*S2A - (2D0*Lambda5*MW2*(3D0*S2A + S2B)*SW2)/EL2))/(2D0*MW*S2B*SW))*(&
	&(3D0*EL/(2D0*MW*SW*S2B)*(4D0*Lambda5*MW2*SW2*SAB*SBA2/EL2 + MHH2*(CBA*S2A - 2D0*SAB)))*( dZHHh0OSAlter()/2D0 ) +&
	&(-(CBA*(EL2*(2D0*Mh02 + MHH2)*S2A + 2D0*Lambda5*MW2*(-3D0*S2A + S2B)*SW2))/(2D0*EL*MW*S2B*SW))*( dZh0HHOSAlter() ) + &
	&EL*SBA/(2D0*MW*S2B*SW)*(&
		& S2A*(dMh02OSAlter() + 2D0*dMHH2OSAlter()) + 2D0*C2A*dAlphaProcDep1Alter()*(Mh02 + 2D0*MHH2) - &
		& 2D0*dm122MSBarAlter()/S2B*(3D0*S2A + S2B) + 4D0*Lambda5*MW2*SW2/EL2*C2B/S2B*(3D0*S2A + S2B)*dBetaProcDep1Alter() - &
		& 2D0*Lambda5*MW2*SW2/EL2*(6D0*C2A*dAlphaProcDep1Alter() + 2D0*C2B*dBetaProcDep1Alter()) &
		&) &
  &)
	case (10)
		totalAmplitude = (0.25D0*EL2*SBA2*DBLE(((Mh02 + 2.D0*MHH2)*S2A - (2.D0*Lambda5*MW2*(3.D0*S2A + S2B)*SW2)/EL2)&
		&**INT(2.D0)))/(MW2*S2B2*SW2)*( dZHHHHOS() + dZh0h0OS()/2D0 + dgAtMZ()/(EL/SW) - dMW2Alter()/(2D0*MW2) + &
		&CBA/SBA*(dBetaProcDep2Alter() - dAlphaProcDep2Alter()) - 2D0*C2B/S2B*dBetaProcDep2Alter() ) + &
  &((EL*SBA*((Mh02 + 2D0*MHH2)*S2A - (2D0*Lambda5*MW2*(3D0*S2A + S2B)*SW2)/EL2))/(2D0*MW*S2B*SW))*(&
	&(3D0*EL/(2D0*MW*SW*S2B)*(4D0*Lambda5*MW2*SW2*SAB*SBA2/EL2 + MHH2*(CBA*S2A - 2D0*SAB)))*( dZHHh0OSAlter()/2D0 ) +&
	&(-(CBA*(EL2*(2D0*Mh02 + MHH2)*S2A + 2D0*Lambda5*MW2*(-3D0*S2A + S2B)*SW2))/(2D0*EL*MW*S2B*SW))*( dZh0HHOSAlter() ) + &
	&EL*SBA/(2D0*MW*S2B*SW)*(&
		& S2A*(dMh02OSAlter() + 2D0*dMHH2OSAlter()) + 2D0*C2A*dAlphaProcDep2Alter()*(Mh02 + 2D0*MHH2) - &
		& 2D0*dm122MSBarAlter()/S2B*(3D0*S2A + S2B) + 4D0*Lambda5*MW2*SW2/EL2*C2B/S2B*(3D0*S2A + S2B)*dBetaProcDep2Alter() - &
		& 2D0*Lambda5*MW2*SW2/EL2*(6D0*C2A*dAlphaProcDep2Alter() + 2D0*C2B*dBetaProcDep2Alter()) &
		&) &
  &)
	case (11)
		totalAmplitude = (0.25D0*EL2*SBA2*DBLE(((Mh02 + 2.D0*MHH2)*S2A - (2.D0*Lambda5*MW2*(3.D0*S2A + S2B)*SW2)/EL2)&
		&**INT(2.D0)))/(MW2*S2B2*SW2)*( dZHHHHOS() + dZh0h0OS()/2D0 + dgAtMZ()/(EL/SW) - dMW2Alter()/(2D0*MW2) + &
		&CBA/SBA*(dBetaProcDep3Alter() - dAlphaProcDep3Alter()) - 2D0*C2B/S2B*dBetaProcDep3Alter() ) + &
  &((EL*SBA*((Mh02 + 2D0*MHH2)*S2A - (2D0*Lambda5*MW2*(3D0*S2A + S2B)*SW2)/EL2))/(2D0*MW*S2B*SW))*(&
	&(3D0*EL/(2D0*MW*SW*S2B)*(4D0*Lambda5*MW2*SW2*SAB*SBA2/EL2 + MHH2*(CBA*S2A - 2D0*SAB)))*( dZHHh0OSAlter()/2D0 ) +&
	&(-(CBA*(EL2*(2D0*Mh02 + MHH2)*S2A + 2D0*Lambda5*MW2*(-3D0*S2A + S2B)*SW2))/(2D0*EL*MW*S2B*SW))*( dZh0HHOSAlter() ) + &
	&EL*SBA/(2D0*MW*S2B*SW)*(&
		& S2A*(dMh02OSAlter() + 2D0*dMHH2OSAlter()) + 2D0*C2A*dAlphaProcDep3Alter()*(Mh02 + 2D0*MHH2) - &
		& 2D0*dm122MSBarAlter()/S2B*(3D0*S2A + S2B) + 4D0*Lambda5*MW2*SW2/EL2*C2B/S2B*(3D0*S2A + S2B)*dBetaProcDep3Alter() - &
		& 2D0*Lambda5*MW2*SW2/EL2*(6D0*C2A*dAlphaProcDep3Alter() + 2D0*C2B*dBetaProcDep3Alter()) &
		&) &
  &)
	case (12)
		totalAmplitude = (0.25D0*EL2*SBA2*DBLE(((Mh02 + 2.D0*MHH2)*S2A - (2.D0*Lambda5*MW2*(3.D0*S2A + S2B)*SW2)/EL2)&
		&**INT(2.D0)))/(MW2*S2B2*SW2)*( dZHHHHOS() + dZh0h0OS()/2D0 + dgAtMZ()/(EL/SW) - dMW2Alter()/(2D0*MW2) + &
		&CBA/SBA*(dBetaOS1Alter() - dAlphaOS1Alter()) - 2D0*C2B/S2B*dBetaOS1Alter() ) + &
  &((EL*SBA*((Mh02 + 2D0*MHH2)*S2A - (2D0*Lambda5*MW2*(3D0*S2A + S2B)*SW2)/EL2))/(2D0*MW*S2B*SW))*(&
	&(3D0*EL/(2D0*MW*SW*S2B)*(4D0*Lambda5*MW2*SW2*SAB*SBA2/EL2 + MHH2*(CBA*S2A - 2D0*SAB)))*( dZHHh0OSAlter()/2D0 ) +&
	&(-(CBA*(EL2*(2D0*Mh02 + MHH2)*S2A + 2D0*Lambda5*MW2*(-3D0*S2A + S2B)*SW2))/(2D0*EL*MW*S2B*SW))*( dZh0HHOSAlter() ) + &
	&EL*SBA/(2D0*MW*S2B*SW)*(&
		& S2A*(dMh02OSAlter() + 2D0*dMHH2OSAlter()) + 2D0*C2A*dAlphaOS1Alter()*(Mh02 + 2D0*MHH2) - &
		& 2D0*dm122MSBarAlter()/S2B*(3D0*S2A + S2B) + 4D0*Lambda5*MW2*SW2/EL2*C2B/S2B*(3D0*S2A + S2B)*dBetaOS1Alter() - &
		& 2D0*Lambda5*MW2*SW2/EL2*(6D0*C2A*dAlphaOS1Alter() + 2D0*C2B*dBetaOS1Alter()) &
		&) &
  &)
	case (13)
		totalAmplitude = (0.25D0*EL2*SBA2*DBLE(((Mh02 + 2.D0*MHH2)*S2A - (2.D0*Lambda5*MW2*(3.D0*S2A + S2B)*SW2)/EL2)&
		&**INT(2.D0)))/(MW2*S2B2*SW2)*( dZHHHHOS() + dZh0h0OS()/2D0 + dgAtMZ()/(EL/SW) - dMW2Alter()/(2D0*MW2) + &
		&CBA/SBA*(dBetaOS2Alter() - dAlphaOS2Alter()) - 2D0*C2B/S2B*dBetaOS2Alter() ) + &
  &((EL*SBA*((Mh02 + 2D0*MHH2)*S2A - (2D0*Lambda5*MW2*(3D0*S2A + S2B)*SW2)/EL2))/(2D0*MW*S2B*SW))*(&
	&(3D0*EL/(2D0*MW*SW*S2B)*(4D0*Lambda5*MW2*SW2*SAB*SBA2/EL2 + MHH2*(CBA*S2A - 2D0*SAB)))*( dZHHh0OSAlter()/2D0 ) +&
	&(-(CBA*(EL2*(2D0*Mh02 + MHH2)*S2A + 2D0*Lambda5*MW2*(-3D0*S2A + S2B)*SW2))/(2D0*EL*MW*S2B*SW))*( dZh0HHOSAlter() ) + &
	&EL*SBA/(2D0*MW*S2B*SW)*(&
		& S2A*(dMh02OSAlter() + 2D0*dMHH2OSAlter()) + 2D0*C2A*dAlphaOS2Alter()*(Mh02 + 2D0*MHH2) - &
		& 2D0*dm122MSBarAlter()/S2B*(3D0*S2A + S2B) + 4D0*Lambda5*MW2*SW2/EL2*C2B/S2B*(3D0*S2A + S2B)*dBetaOS2Alter() - &
		& 2D0*Lambda5*MW2*SW2/EL2*(6D0*C2A*dAlphaOS2Alter() + 2D0*C2B*dBetaOS2Alter()) &
		&) &
  &)
	case (14)
		totalAmplitude = (0.25D0*EL2*SBA2*DBLE(((Mh02 + 2.D0*MHH2)*S2A - (2.D0*Lambda5*MW2*(3.D0*S2A + S2B)*SW2)/EL2)&
		&**INT(2.D0)))/(MW2*S2B2*SW2)*( dZHHHHOS() + dZh0h0OS()/2D0 + dgAtMZ()/(EL/SW) - dMW2Alter()/(2D0*MW2) + &
		&CBA/SBA*(dBetaOS12Alter() - dAlphaOS12Alter()) - 2D0*C2B/S2B*dBetaOS12Alter() ) + &
  &((EL*SBA*((Mh02 + 2D0*MHH2)*S2A - (2D0*Lambda5*MW2*(3D0*S2A + S2B)*SW2)/EL2))/(2D0*MW*S2B*SW))*(&
	&(3D0*EL/(2D0*MW*SW*S2B)*(4D0*Lambda5*MW2*SW2*SAB*SBA2/EL2 + MHH2*(CBA*S2A - 2D0*SAB)))*( dZHHh0OSAlter()/2D0 ) +&
	&(-(CBA*(EL2*(2D0*Mh02 + MHH2)*S2A + 2D0*Lambda5*MW2*(-3D0*S2A + S2B)*SW2))/(2D0*EL*MW*S2B*SW))*( dZh0HHOSAlter() ) + &
	&EL*SBA/(2D0*MW*S2B*SW)*(&
		& S2A*(dMh02OSAlter() + 2D0*dMHH2OSAlter()) + 2D0*C2A*dAlphaOS12Alter()*(Mh02 + 2D0*MHH2) - &
		& 2D0*dm122MSBarAlter()/S2B*(3D0*S2A + S2B) + 4D0*Lambda5*MW2*SW2/EL2*C2B/S2B*(3D0*S2A + S2B)*dBetaOS12Alter() - &
		& 2D0*Lambda5*MW2*SW2/EL2*(6D0*C2A*dAlphaOS12Alter() + 2D0*C2B*dBetaOS12Alter()) &
		&) &
  &)
	case (15)
		totalAmplitude = (0.25D0*EL2*SBA2*DBLE(((Mh02 + 2.D0*MHH2)*S2A - (2.D0*Lambda5*MW2*(3.D0*S2A + S2B)*SW2)/EL2)&
		&**INT(2.D0)))/(MW2*S2B2*SW2)*( dZHHHHOS() + dZh0h0OS()/2D0 + dgAtMZ()/(EL/SW) - dMW2Alter()/(2D0*MW2) + &
		&CBA/SBA*(dBetaBFMSAlter() - dAlphaPinchOS()) - 2D0*C2B/S2B*dBetaBFMSAlter() ) + &
  &((EL*SBA*((Mh02 + 2D0*MHH2)*S2A - (2D0*Lambda5*MW2*(3D0*S2A + S2B)*SW2)/EL2))/(2D0*MW*S2B*SW))*(&
	&(3D0*EL/(2D0*MW*SW*S2B)*(4D0*Lambda5*MW2*SW2*SAB*SBA2/EL2 + MHH2*(CBA*S2A - 2D0*SAB)))*( dZHHh0OSAlter()/2D0 ) +&
	&(-(CBA*(EL2*(2D0*Mh02 + MHH2)*S2A + 2D0*Lambda5*MW2*(-3D0*S2A + S2B)*SW2))/(2D0*EL*MW*S2B*SW))*( dZh0HHOSAlter() ) + &
	&EL*SBA/(2D0*MW*S2B*SW)*(&
		& S2A*(dMh02OSAlter() + 2D0*dMHH2OSAlter()) + 2D0*C2A*dAlphaPinchOS()*(Mh02 + 2D0*MHH2) - &
		& 2D0*dm122MSBarAlter()/S2B*(3D0*S2A + S2B) + 4D0*Lambda5*MW2*SW2/EL2*C2B/S2B*(3D0*S2A + S2B)*dBetaBFMSAlter() - &
		& 2D0*Lambda5*MW2*SW2/EL2*(6D0*C2A*dAlphaPinchOS() + 2D0*C2B*dBetaBFMSAlter()) &
		&) &
  &)
	case (16)
		totalAmplitude = (0.25D0*EL2*SBA2*DBLE(((Mh02 + 2.D0*MHH2)*S2A - (2.D0*Lambda5*MW2*(3.D0*S2A + S2B)*SW2)/EL2)&
		&**INT(2.D0)))/(MW2*S2B2*SW2)*( dZHHHHOS() + dZh0h0OS()/2D0 + dgAtMZ()/(EL/SW) - dMW2Usual()/(2D0*MW2) + &
		&CBA/SBA*(dBetaMSBarUsual() - dAlphaMSBarUsual()) - 2D0*C2B/S2B*dBetaMSBarUsual() ) + &
  &((EL*SBA*((Mh02 + 2D0*MHH2)*S2A - (2D0*Lambda5*MW2*(3D0*S2A + S2B)*SW2)/EL2))/(2D0*MW*S2B*SW))*(&
	&(3D0*EL/(2D0*MW*SW*S2B)*(4D0*Lambda5*MW2*SW2*SAB*SBA2/EL2 + MHH2*(CBA*S2A - 2D0*SAB)))*( dZHHh0OSUsual()/2D0 ) +&
	&(-(CBA*(EL2*(2D0*Mh02 + MHH2)*S2A + 2D0*Lambda5*MW2*(-3D0*S2A + S2B)*SW2))/(2D0*EL*MW*S2B*SW))*( dZh0HHOSUsual() ) + &
	&EL*SBA/(2D0*MW*S2B*SW)*(&
		& S2A*(dMh02OSUsual() + 2D0*dMHH2OSUsual()) + 2D0*C2A*dAlphaMSBarUsual()*(Mh02 + 2D0*MHH2) - &
		& 2D0*dm122MSBarUsual()/S2B*(3D0*S2A + S2B) + 4D0*Lambda5*MW2*SW2/EL2*C2B/S2B*(3D0*S2A + S2B)*dBetaMSBarUsual() - &
		& 2D0*Lambda5*MW2*SW2/EL2*(6D0*C2A*dAlphaMSBarUsual() + 2D0*C2B*dBetaMSBarUsual()) &
		&) &
  &)
	case (17)
		totalAmplitude = (0.25D0*EL2*SBA2*DBLE(((Mh02 + 2.D0*MHH2)*S2A - (2.D0*Lambda5*MW2*(3.D0*S2A + S2B)*SW2)/EL2)&
		&**INT(2.D0)))/(MW2*S2B2*SW2)*( dZHHHHOS() + dZh0h0OS()/2D0 + dgAtMZ()/(EL/SW) - dMW2Alter()/(2D0*MW2) + &
		&CBA/SBA*(dBetaMSBarAlter() - dAlphaMSBarAlter()) - 2D0*C2B/S2B*dBetaMSBarAlter() ) + &
  &((EL*SBA*((Mh02 + 2D0*MHH2)*S2A - (2D0*Lambda5*MW2*(3D0*S2A + S2B)*SW2)/EL2))/(2D0*MW*S2B*SW))*(&
	&(3D0*EL/(2D0*MW*SW*S2B)*(4D0*Lambda5*MW2*SW2*SAB*SBA2/EL2 + MHH2*(CBA*S2A - 2D0*SAB)))*( dZHHh0OSAlter()/2D0 ) +&
	&(-(CBA*(EL2*(2D0*Mh02 + MHH2)*S2A + 2D0*Lambda5*MW2*(-3D0*S2A + S2B)*SW2))/(2D0*EL*MW*S2B*SW))*( dZh0HHOSAlter() ) + &
	&EL*SBA/(2D0*MW*S2B*SW)*(&
		& S2A*(dMh02OSAlter() + 2D0*dMHH2OSAlter()) + 2D0*C2A*dAlphaMSBarAlter()*(Mh02 + 2D0*MHH2) - &
		& 2D0*dm122MSBarAlter()/S2B*(3D0*S2A + S2B) + 4D0*Lambda5*MW2*SW2/EL2*C2B/S2B*(3D0*S2A + S2B)*dBetaMSBarAlter() - &
		& 2D0*Lambda5*MW2*SW2/EL2*(6D0*C2A*dAlphaMSBarAlter() + 2D0*C2B*dBetaMSBarAlter()) &
		&) &
  &)
	case default
		totalAmplitude = 0D0
 end select

 h0toHHHHCT = totalAmplitude
end function h0toHHHHCT
