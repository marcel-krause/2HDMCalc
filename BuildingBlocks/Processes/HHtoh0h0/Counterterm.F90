double precision function HHtoh0h0CT(x)
 use constants
 use counterterms
 implicit none
#include "looptools.h"
 integer, intent(in) :: x
 double precision :: totalAmplitude

 select case (x)
	case (1)
		totalAmplitude = (0.25D0*CBA2*DBLE((EL2*(2.D0*Mh02 + MHH2)*S2A + 2.D0*Lambda5*MW2*(-3.D0*S2A + S2B)*SW2)**INT(2.D0)))/&
  &(EL2*MW2*S2B2*SW2)*( dZh0h0OS() + dZHHHHOS()/2D0 + dgAtMZ()/(EL/SW) - dMW2Usual()/(2D0*MW2) - SBA/CBA*(dBeta1KanUsual()&
	& - dAlphaKanUsual()) - 2D0*C2B/S2B*dBeta1KanUsual() ) + &
  &(-(CBA*(EL2*(2*Mh02 + MHH2)*S2A + 2*Lambda5*MW2*(-3*S2A + S2B)*SW2))/(2*EL*MW*S2B*SW))*(&
	&((-3*EL*(Mh02*(2*CAB + S2A*SBA) - (4*CAB*CBA2*Lambda5*MW2*SW2)/EL2))/(2*MW*S2B*SW))*( dZh0HHOSUsual()/2D0 ) +&
	&((EL*SBA*((Mh02 + 2*MHH2)*S2A - (2*Lambda5*MW2*(3*S2A + S2B)*SW2)/EL2))/(2*MW*S2B*SW))*( dZHHh0OSUsual() ) - &
	&2D0*Lambda5*MW*CBA*(3D0*S2A-S2B)/((EL/SW)*S2B)*( dgAtMZ()/(EL/SW) - dMW2Usual()/(2D0*MW2) ) -&
	&(EL/SW)*CBA/(2D0*MW*S2B)*( 2D0*S2A*dMh02OSUsual() + S2A*dMHH2OSUsual() - 2D0*MW2*(3D0*S2A-S2B)/(EL2/SW2)*Lambda5*(&
	&dm122MSBarUsual()*(EL2/SW2)/(Lambda5*MW2*S2B) + 2D0*dgAtMZ()/(EL/SW) - dMW2Usual()/MW2 - 2D0*C2B/S2B*dBeta1KanUsual() ) &
	&) + (EL/SW)*CBA*C2A/(MW*S2B)*(6D0*MW2*Lambda5/(EL2/SW2)-2D0*Mh02-MHH2)*dAlphaKanUsual() -&
	& 2D0*C2B/S2B*Lambda5*MW*CBA/(EL/SW)*dBeta1KanUsual()&
  &)
	case (2)
		totalAmplitude = (0.25D0*CBA2*DBLE((EL2*(2.D0*Mh02 + MHH2)*S2A + 2.D0*Lambda5*MW2*(-3.D0*S2A + S2B)*SW2)**INT(2.D0)))/&
  &(EL2*MW2*S2B2*SW2)*( dZh0h0OS() + dZHHHHOS()/2D0 + dgAtMZ()/(EL/SW) - dMW2Usual()/(2D0*MW2) - SBA/CBA*(dBeta2KanUsual()&
	&- dAlphaKanUsual()) - 2D0*C2B/S2B*dBeta2KanUsual() ) + &
  &(-(CBA*(EL2*(2*Mh02 + MHH2)*S2A + 2*Lambda5*MW2*(-3*S2A + S2B)*SW2))/(2*EL*MW*S2B*SW))*(&
	&((-3*EL*(Mh02*(2*CAB + S2A*SBA) - (4*CAB*CBA2*Lambda5*MW2*SW2)/EL2))/(2*MW*S2B*SW))*( dZh0HHOSUsual()/2D0 ) +&
	&((EL*SBA*((Mh02 + 2*MHH2)*S2A - (2*Lambda5*MW2*(3*S2A + S2B)*SW2)/EL2))/(2*MW*S2B*SW))*( dZHHh0OSUsual() ) - &
	&2D0*Lambda5*MW*CBA*(3D0*S2A-S2B)/((EL/SW)*S2B)*( dgAtMZ()/(EL/SW) - dMW2Usual()/(2D0*MW2) ) -&
	&(EL/SW)*CBA/(2D0*MW*S2B)*( 2D0*S2A*dMh02OSUsual() + S2A*dMHH2OSUsual() - 2D0*MW2*(3D0*S2A-S2B)/(EL2/SW2)*Lambda5*(&
	&dm122MSBarUsual()*(EL2/SW2)/(Lambda5*MW2*S2B) + 2D0*dgAtMZ()/(EL/SW) - dMW2Usual()/MW2 - 2D0*C2B/S2B*dBeta2KanUsual() ) &
	&) + (EL/SW)*CBA*C2A/(MW*S2B)*(6D0*MW2*Lambda5/(EL2/SW2)-2D0*Mh02-MHH2)*dAlphaKanUsual() -&
	& 2D0*C2B/S2B*Lambda5*MW*CBA/(EL/SW)*dBeta2KanUsual()&
  &)
	case (3)
		totalAmplitude = (0.25D0*CBA2*DBLE((EL2*(2.D0*Mh02 + MHH2)*S2A + 2.D0*Lambda5*MW2*(-3.D0*S2A + S2B)*SW2)**INT(2.D0)))/&
  &(EL2*MW2*S2B2*SW2)*( dZh0h0OS() + dZHHHHOS()/2D0 + dgAtMZ()/(EL/SW) - dMW2Alter()/(2D0*MW2) - SBA/CBA*(dBeta1KanAlter()&
	& - dAlphaKanAlter()) - 2D0*C2B/S2B*dBeta1KanAlter() ) + &
  &(-(CBA*(EL2*(2*Mh02 + MHH2)*S2A + 2*Lambda5*MW2*(-3*S2A + S2B)*SW2))/(2*EL*MW*S2B*SW))*(&
	&((-3*EL*(Mh02*(2*CAB + S2A*SBA) - (4*CAB*CBA2*Lambda5*MW2*SW2)/EL2))/(2*MW*S2B*SW))*( dZh0HHOSAlter()/2D0 ) +&
	&((EL*SBA*((Mh02 + 2*MHH2)*S2A - (2*Lambda5*MW2*(3*S2A + S2B)*SW2)/EL2))/(2*MW*S2B*SW))*( dZHHh0OSAlter() ) - &
	&2D0*Lambda5*MW*CBA*(3D0*S2A-S2B)/((EL/SW)*S2B)*( dgAtMZ()/(EL/SW) - dMW2Alter()/(2D0*MW2) ) -&
	&(EL/SW)*CBA/(2D0*MW*S2B)*( 2D0*S2A*dMh02OSAlter() + S2A*dMHH2OSAlter() - 2D0*MW2*(3D0*S2A-S2B)/(EL2/SW2)*Lambda5*(&
	&dm122MSBarAlter()*(EL2/SW2)/(Lambda5*MW2*S2B) + 2D0*dgAtMZ()/(EL/SW) - dMW2Alter()/MW2 - 2D0*C2B/S2B*dBeta1KanAlter() ) &
	&) + (EL/SW)*CBA*C2A/(MW*S2B)*(6D0*MW2*Lambda5/(EL2/SW2)-2D0*Mh02-MHH2)*dAlphaKanAlter() -&
	& 2D0*C2B/S2B*Lambda5*MW*CBA/(EL/SW)*dBeta1KanAlter()&
  &)
	case (4)
		totalAmplitude = (0.25D0*CBA2*DBLE((EL2*(2.D0*Mh02 + MHH2)*S2A + 2.D0*Lambda5*MW2*(-3.D0*S2A + S2B)*SW2)**INT(2.D0)))/&
  &(EL2*MW2*S2B2*SW2)*( dZh0h0OS() + dZHHHHOS()/2D0 + dgAtMZ()/(EL/SW) - dMW2Alter()/(2D0*MW2) - SBA/CBA*(dBeta2KanAlter()&
	& - dAlphaKanAlter()) - 2D0*C2B/S2B*dBeta2KanAlter() ) + &
  &(-(CBA*(EL2*(2*Mh02 + MHH2)*S2A + 2*Lambda5*MW2*(-3*S2A + S2B)*SW2))/(2*EL*MW*S2B*SW))*(&
	&((-3*EL*(Mh02*(2*CAB + S2A*SBA) - (4*CAB*CBA2*Lambda5*MW2*SW2)/EL2))/(2*MW*S2B*SW))*( dZh0HHOSAlter()/2D0 ) +&
	&((EL*SBA*((Mh02 + 2*MHH2)*S2A - (2*Lambda5*MW2*(3*S2A + S2B)*SW2)/EL2))/(2*MW*S2B*SW))*( dZHHh0OSAlter() ) - &
	&2D0*Lambda5*MW*CBA*(3D0*S2A-S2B)/((EL/SW)*S2B)*( dgAtMZ()/(EL/SW) - dMW2Alter()/(2D0*MW2) ) -&
	&(EL/SW)*CBA/(2D0*MW*S2B)*( 2D0*S2A*dMh02OSAlter() + S2A*dMHH2OSAlter() - 2D0*MW2*(3D0*S2A-S2B)/(EL2/SW2)*Lambda5*(&
	&dm122MSBarAlter()*(EL2/SW2)/(Lambda5*MW2*S2B) + 2D0*dgAtMZ()/(EL/SW) - dMW2Alter()/MW2 - 2D0*C2B/S2B*dBeta2KanAlter() ) &
	&) + (EL/SW)*CBA*C2A/(MW*S2B)*(6D0*MW2*Lambda5/(EL2/SW2)-2D0*Mh02-MHH2)*dAlphaKanAlter() -&
	& 2D0*C2B/S2B*Lambda5*MW*CBA/(EL/SW)*dBeta2KanAlter()&
  &)
	case (5)
		totalAmplitude = (0.25D0*CBA2*DBLE((EL2*(2.D0*Mh02 + MHH2)*S2A + 2.D0*Lambda5*MW2*(-3.D0*S2A + S2B)*SW2)**INT(2.D0)))/&
  &(EL2*MW2*S2B2*SW2)*( dZh0h0OS() + dZHHHHOS()/2D0 + dgAtMZ()/(EL/SW) - dMW2Alter()/(2D0*MW2) - SBA/CBA*(dBeta1PinchPStar()&
	& - dAlphaPinchPStar()) - 2D0*C2B/S2B*dBeta1PinchPStar() ) + &
  &(-(CBA*(EL2*(2*Mh02 + MHH2)*S2A + 2*Lambda5*MW2*(-3*S2A + S2B)*SW2))/(2*EL*MW*S2B*SW))*(&
	&((-3*EL*(Mh02*(2*CAB + S2A*SBA) - (4*CAB*CBA2*Lambda5*MW2*SW2)/EL2))/(2*MW*S2B*SW))*( dZh0HHOSAlter()/2D0 ) +&
	&((EL*SBA*((Mh02 + 2*MHH2)*S2A - (2*Lambda5*MW2*(3*S2A + S2B)*SW2)/EL2))/(2*MW*S2B*SW))*( dZHHh0OSAlter() ) - &
	&2D0*Lambda5*MW*CBA*(3D0*S2A-S2B)/((EL/SW)*S2B)*( dgAtMZ()/(EL/SW) - dMW2Alter()/(2D0*MW2) ) -&
	&(EL/SW)*CBA/(2D0*MW*S2B)*( 2D0*S2A*dMh02OSAlter() + S2A*dMHH2OSAlter() - 2D0*MW2*(3D0*S2A-S2B)/(EL2/SW2)*Lambda5*(&
	&dm122MSBarAlter()*(EL2/SW2)/(Lambda5*MW2*S2B) + 2D0*dgAtMZ()/(EL/SW) - dMW2Alter()/MW2 - 2D0*C2B/S2B*dBeta1PinchPStar() ) &
	&) + (EL/SW)*CBA*C2A/(MW*S2B)*(6D0*MW2*Lambda5/(EL2/SW2)-2D0*Mh02-MHH2)*dAlphaPinchPStar() -&
	& 2D0*C2B/S2B*Lambda5*MW*CBA/(EL/SW)*dBeta1PinchPStar()&
  &)
	case (6)
		totalAmplitude = (0.25D0*CBA2*DBLE((EL2*(2.D0*Mh02 + MHH2)*S2A + 2.D0*Lambda5*MW2*(-3.D0*S2A + S2B)*SW2)**INT(2.D0)))/&
  &(EL2*MW2*S2B2*SW2)*( dZh0h0OS() + dZHHHHOS()/2D0 + dgAtMZ()/(EL/SW) - dMW2Alter()/(2D0*MW2) - SBA/CBA*(dBeta2PinchPStar()&
	& - dAlphaPinchPStar()) - 2D0*C2B/S2B*dBeta2PinchPStar() ) + &
  &(-(CBA*(EL2*(2*Mh02 + MHH2)*S2A + 2*Lambda5*MW2*(-3*S2A + S2B)*SW2))/(2*EL*MW*S2B*SW))*(&
	&((-3*EL*(Mh02*(2*CAB + S2A*SBA) - (4*CAB*CBA2*Lambda5*MW2*SW2)/EL2))/(2*MW*S2B*SW))*( dZh0HHOSAlter()/2D0 ) +&
	&((EL*SBA*((Mh02 + 2*MHH2)*S2A - (2*Lambda5*MW2*(3*S2A + S2B)*SW2)/EL2))/(2*MW*S2B*SW))*( dZHHh0OSAlter() ) - &
	&2D0*Lambda5*MW*CBA*(3D0*S2A-S2B)/((EL/SW)*S2B)*( dgAtMZ()/(EL/SW) - dMW2Alter()/(2D0*MW2) ) -&
	&(EL/SW)*CBA/(2D0*MW*S2B)*( 2D0*S2A*dMh02OSAlter() + S2A*dMHH2OSAlter() - 2D0*MW2*(3D0*S2A-S2B)/(EL2/SW2)*Lambda5*(&
	&dm122MSBarAlter()*(EL2/SW2)/(Lambda5*MW2*S2B) + 2D0*dgAtMZ()/(EL/SW) - dMW2Alter()/MW2 - 2D0*C2B/S2B*dBeta2PinchPStar() ) &
	&) + (EL/SW)*CBA*C2A/(MW*S2B)*(6D0*MW2*Lambda5/(EL2/SW2)-2D0*Mh02-MHH2)*dAlphaPinchPStar() -&
	& 2D0*C2B/S2B*Lambda5*MW*CBA/(EL/SW)*dBeta2PinchPStar()&
  &)
	case (7)
		totalAmplitude = (0.25D0*CBA2*DBLE((EL2*(2.D0*Mh02 + MHH2)*S2A + 2.D0*Lambda5*MW2*(-3.D0*S2A + S2B)*SW2)**INT(2.D0)))/&
  &(EL2*MW2*S2B2*SW2)*( dZh0h0OS() + dZHHHHOS()/2D0 + dgAtMZ()/(EL/SW) - dMW2Alter()/(2D0*MW2) - SBA/CBA*(dBeta1PinchOS()&
	& - dAlphaPinchOS()) - 2D0*C2B/S2B*dBeta1PinchOS() ) + &
  &(-(CBA*(EL2*(2*Mh02 + MHH2)*S2A + 2*Lambda5*MW2*(-3*S2A + S2B)*SW2))/(2*EL*MW*S2B*SW))*(&
	&((-3*EL*(Mh02*(2*CAB + S2A*SBA) - (4*CAB*CBA2*Lambda5*MW2*SW2)/EL2))/(2*MW*S2B*SW))*( dZh0HHOSAlter()/2D0 ) +&
	&((EL*SBA*((Mh02 + 2*MHH2)*S2A - (2*Lambda5*MW2*(3*S2A + S2B)*SW2)/EL2))/(2*MW*S2B*SW))*( dZHHh0OSAlter() ) - &
	&2D0*Lambda5*MW*CBA*(3D0*S2A-S2B)/((EL/SW)*S2B)*( dgAtMZ()/(EL/SW) - dMW2Alter()/(2D0*MW2) ) -&
	&(EL/SW)*CBA/(2D0*MW*S2B)*( 2D0*S2A*dMh02OSAlter() + S2A*dMHH2OSAlter() - 2D0*MW2*(3D0*S2A-S2B)/(EL2/SW2)*Lambda5*(&
	&dm122MSBarAlter()*(EL2/SW2)/(Lambda5*MW2*S2B) + 2D0*dgAtMZ()/(EL/SW) - dMW2Alter()/MW2 - 2D0*C2B/S2B*dBeta1PinchOS() ) &
	&) + (EL/SW)*CBA*C2A/(MW*S2B)*(6D0*MW2*Lambda5/(EL2/SW2)-2D0*Mh02-MHH2)*dAlphaPinchOS() -&
	& 2D0*C2B/S2B*Lambda5*MW*CBA/(EL/SW)*dBeta1PinchOS()&
  &)
	case (8)
		totalAmplitude = (0.25D0*CBA2*DBLE((EL2*(2.D0*Mh02 + MHH2)*S2A + 2.D0*Lambda5*MW2*(-3.D0*S2A + S2B)*SW2)**INT(2.D0)))/&
  &(EL2*MW2*S2B2*SW2)*( dZh0h0OS() + dZHHHHOS()/2D0 + dgAtMZ()/(EL/SW) - dMW2Alter()/(2D0*MW2) - SBA/CBA*(dBeta2PinchOS()&
	& - dAlphaPinchOS()) - 2D0*C2B/S2B*dBeta2PinchOS() ) + &
  &(-(CBA*(EL2*(2*Mh02 + MHH2)*S2A + 2*Lambda5*MW2*(-3*S2A + S2B)*SW2))/(2*EL*MW*S2B*SW))*(&
	&((-3*EL*(Mh02*(2*CAB + S2A*SBA) - (4*CAB*CBA2*Lambda5*MW2*SW2)/EL2))/(2*MW*S2B*SW))*( dZh0HHOSAlter()/2D0 ) +&
	&((EL*SBA*((Mh02 + 2*MHH2)*S2A - (2*Lambda5*MW2*(3*S2A + S2B)*SW2)/EL2))/(2*MW*S2B*SW))*( dZHHh0OSAlter() ) - &
	&2D0*Lambda5*MW*CBA*(3D0*S2A-S2B)/((EL/SW)*S2B)*( dgAtMZ()/(EL/SW) - dMW2Alter()/(2D0*MW2) ) -&
	&(EL/SW)*CBA/(2D0*MW*S2B)*( 2D0*S2A*dMh02OSAlter() + S2A*dMHH2OSAlter() - 2D0*MW2*(3D0*S2A-S2B)/(EL2/SW2)*Lambda5*(&
	&dm122MSBarAlter()*(EL2/SW2)/(Lambda5*MW2*S2B) + 2D0*dgAtMZ()/(EL/SW) - dMW2Alter()/MW2 - 2D0*C2B/S2B*dBeta2PinchOS() ) &
	&) + (EL/SW)*CBA*C2A/(MW*S2B)*(6D0*MW2*Lambda5/(EL2/SW2)-2D0*Mh02-MHH2)*dAlphaPinchOS() -&
	& 2D0*C2B/S2B*Lambda5*MW*CBA/(EL/SW)*dBeta2PinchOS()&
  &)
	case (9)
		totalAmplitude = (0.25D0*CBA2*DBLE((EL2*(2.D0*Mh02 + MHH2)*S2A + 2.D0*Lambda5*MW2*(-3.D0*S2A + S2B)*SW2)**INT(2.D0)))/&
  &(EL2*MW2*S2B2*SW2)*( dZh0h0OS() + dZHHHHOS()/2D0 + dgAtMZ()/(EL/SW) - dMW2Alter()/(2D0*MW2) - SBA/CBA*(dBetaProcDep1Alter()&
	& - dAlphaProcDep1Alter()) - 2D0*C2B/S2B*dBetaProcDep1Alter() ) + &
  &(-(CBA*(EL2*(2*Mh02 + MHH2)*S2A + 2*Lambda5*MW2*(-3*S2A + S2B)*SW2))/(2*EL*MW*S2B*SW))*(&
	&((-3*EL*(Mh02*(2*CAB + S2A*SBA) - (4*CAB*CBA2*Lambda5*MW2*SW2)/EL2))/(2*MW*S2B*SW))*( dZh0HHOSAlter()/2D0 ) +&
	&((EL*SBA*((Mh02 + 2*MHH2)*S2A - (2*Lambda5*MW2*(3*S2A + S2B)*SW2)/EL2))/(2*MW*S2B*SW))*( dZHHh0OSAlter() ) - &
	&2D0*Lambda5*MW*CBA*(3D0*S2A-S2B)/((EL/SW)*S2B)*( dgAtMZ()/(EL/SW) - dMW2Alter()/(2D0*MW2) ) -&
	&(EL/SW)*CBA/(2D0*MW*S2B)*( 2D0*S2A*dMh02OSAlter() + S2A*dMHH2OSAlter() - 2D0*MW2*(3D0*S2A-S2B)/(EL2/SW2)*Lambda5*(&
	&dm122MSBarAlter()*(EL2/SW2)/(Lambda5*MW2*S2B) + 2D0*dgAtMZ()/(EL/SW) - dMW2Alter()/MW2 - 2D0*C2B/S2B*dBetaProcDep1Alter() ) &
	&) + (EL/SW)*CBA*C2A/(MW*S2B)*(6D0*MW2*Lambda5/(EL2/SW2)-2D0*Mh02-MHH2)*dAlphaProcDep1Alter() -&
	& 2D0*C2B/S2B*Lambda5*MW*CBA/(EL/SW)*dBetaProcDep1Alter()&
  &)
	case (10)
		totalAmplitude = (0.25D0*CBA2*DBLE((EL2*(2.D0*Mh02 + MHH2)*S2A + 2.D0*Lambda5*MW2*(-3.D0*S2A + S2B)*SW2)**INT(2.D0)))/&
  &(EL2*MW2*S2B2*SW2)*( dZh0h0OS() + dZHHHHOS()/2D0 + dgAtMZ()/(EL/SW) - dMW2Alter()/(2D0*MW2) - SBA/CBA*(dBetaProcDep2Alter()&
	& - dAlphaProcDep2Alter()) - 2D0*C2B/S2B*dBetaProcDep2Alter() ) + &
  &(-(CBA*(EL2*(2*Mh02 + MHH2)*S2A + 2*Lambda5*MW2*(-3*S2A + S2B)*SW2))/(2*EL*MW*S2B*SW))*(&
	&((-3*EL*(Mh02*(2*CAB + S2A*SBA) - (4*CAB*CBA2*Lambda5*MW2*SW2)/EL2))/(2*MW*S2B*SW))*( dZh0HHOSAlter()/2D0 ) +&
	&((EL*SBA*((Mh02 + 2*MHH2)*S2A - (2*Lambda5*MW2*(3*S2A + S2B)*SW2)/EL2))/(2*MW*S2B*SW))*( dZHHh0OSAlter() ) - &
	&2D0*Lambda5*MW*CBA*(3D0*S2A-S2B)/((EL/SW)*S2B)*( dgAtMZ()/(EL/SW) - dMW2Alter()/(2D0*MW2) ) -&
	&(EL/SW)*CBA/(2D0*MW*S2B)*( 2D0*S2A*dMh02OSAlter() + S2A*dMHH2OSAlter() - 2D0*MW2*(3D0*S2A-S2B)/(EL2/SW2)*Lambda5*(&
	&dm122MSBarAlter()*(EL2/SW2)/(Lambda5*MW2*S2B) + 2D0*dgAtMZ()/(EL/SW) - dMW2Alter()/MW2 - 2D0*C2B/S2B*dBetaProcDep2Alter() ) &
	&) + (EL/SW)*CBA*C2A/(MW*S2B)*(6D0*MW2*Lambda5/(EL2/SW2)-2D0*Mh02-MHH2)*dAlphaProcDep2Alter() -&
	& 2D0*C2B/S2B*Lambda5*MW*CBA/(EL/SW)*dBetaProcDep2Alter()&
  &)
	case (11)
		totalAmplitude = (0.25D0*CBA2*DBLE((EL2*(2.D0*Mh02 + MHH2)*S2A + 2.D0*Lambda5*MW2*(-3.D0*S2A + S2B)*SW2)**INT(2.D0)))/&
  &(EL2*MW2*S2B2*SW2)*( dZh0h0OS() + dZHHHHOS()/2D0 + dgAtMZ()/(EL/SW) - dMW2Alter()/(2D0*MW2) - SBA/CBA*(dBetaProcDep3Alter()&
	& - dAlphaProcDep3Alter()) - 2D0*C2B/S2B*dBetaProcDep3Alter() ) + &
  &(-(CBA*(EL2*(2*Mh02 + MHH2)*S2A + 2*Lambda5*MW2*(-3*S2A + S2B)*SW2))/(2*EL*MW*S2B*SW))*(&
	&((-3*EL*(Mh02*(2*CAB + S2A*SBA) - (4*CAB*CBA2*Lambda5*MW2*SW2)/EL2))/(2*MW*S2B*SW))*( dZh0HHOSAlter()/2D0 ) +&
	&((EL*SBA*((Mh02 + 2*MHH2)*S2A - (2*Lambda5*MW2*(3*S2A + S2B)*SW2)/EL2))/(2*MW*S2B*SW))*( dZHHh0OSAlter() ) - &
	&2D0*Lambda5*MW*CBA*(3D0*S2A-S2B)/((EL/SW)*S2B)*( dgAtMZ()/(EL/SW) - dMW2Alter()/(2D0*MW2) ) -&
	&(EL/SW)*CBA/(2D0*MW*S2B)*( 2D0*S2A*dMh02OSAlter() + S2A*dMHH2OSAlter() - 2D0*MW2*(3D0*S2A-S2B)/(EL2/SW2)*Lambda5*(&
	&dm122MSBarAlter()*(EL2/SW2)/(Lambda5*MW2*S2B) + 2D0*dgAtMZ()/(EL/SW) - dMW2Alter()/MW2 - 2D0*C2B/S2B*dBetaProcDep3Alter() ) &
	&) + (EL/SW)*CBA*C2A/(MW*S2B)*(6D0*MW2*Lambda5/(EL2/SW2)-2D0*Mh02-MHH2)*dAlphaProcDep3Alter() -&
	& 2D0*C2B/S2B*Lambda5*MW*CBA/(EL/SW)*dBetaProcDep3Alter()&
  &)
	case (12)
		totalAmplitude = (0.25D0*CBA2*DBLE((EL2*(2.D0*Mh02 + MHH2)*S2A + 2.D0*Lambda5*MW2*(-3.D0*S2A + S2B)*SW2)**INT(2.D0)))/&
  &(EL2*MW2*S2B2*SW2)*( dZh0h0OS() + dZHHHHOS()/2D0 + dgAtMZ()/(EL/SW) - dMW2Alter()/(2D0*MW2) - SBA/CBA*(dBetaOS1Alter()&
	& - dAlphaOS1Alter()) - 2D0*C2B/S2B*dBetaOS1Alter() ) + &
  &(-(CBA*(EL2*(2*Mh02 + MHH2)*S2A + 2*Lambda5*MW2*(-3*S2A + S2B)*SW2))/(2*EL*MW*S2B*SW))*(&
	&((-3*EL*(Mh02*(2*CAB + S2A*SBA) - (4*CAB*CBA2*Lambda5*MW2*SW2)/EL2))/(2*MW*S2B*SW))*( dZh0HHOSAlter()/2D0 ) +&
	&((EL*SBA*((Mh02 + 2*MHH2)*S2A - (2*Lambda5*MW2*(3*S2A + S2B)*SW2)/EL2))/(2*MW*S2B*SW))*( dZHHh0OSAlter() ) - &
	&2D0*Lambda5*MW*CBA*(3D0*S2A-S2B)/((EL/SW)*S2B)*( dgAtMZ()/(EL/SW) - dMW2Alter()/(2D0*MW2) ) -&
	&(EL/SW)*CBA/(2D0*MW*S2B)*( 2D0*S2A*dMh02OSAlter() + S2A*dMHH2OSAlter() - 2D0*MW2*(3D0*S2A-S2B)/(EL2/SW2)*Lambda5*(&
	&dm122MSBarAlter()*(EL2/SW2)/(Lambda5*MW2*S2B) + 2D0*dgAtMZ()/(EL/SW) - dMW2Alter()/MW2 - 2D0*C2B/S2B*dBetaOS1Alter() ) &
	&) + (EL/SW)*CBA*C2A/(MW*S2B)*(6D0*MW2*Lambda5/(EL2/SW2)-2D0*Mh02-MHH2)*dAlphaOS1Alter() -&
	& 2D0*C2B/S2B*Lambda5*MW*CBA/(EL/SW)*dBetaOS1Alter()&
  &)
	case (13)
		totalAmplitude = (0.25D0*CBA2*DBLE((EL2*(2.D0*Mh02 + MHH2)*S2A + 2.D0*Lambda5*MW2*(-3.D0*S2A + S2B)*SW2)**INT(2.D0)))/&
  &(EL2*MW2*S2B2*SW2)*( dZh0h0OS() + dZHHHHOS()/2D0 + dgAtMZ()/(EL/SW) - dMW2Alter()/(2D0*MW2) - SBA/CBA*(dBetaOS2Alter()&
	& - dAlphaOS2Alter()) - 2D0*C2B/S2B*dBetaOS2Alter() ) + &
  &(-(CBA*(EL2*(2*Mh02 + MHH2)*S2A + 2*Lambda5*MW2*(-3*S2A + S2B)*SW2))/(2*EL*MW*S2B*SW))*(&
	&((-3*EL*(Mh02*(2*CAB + S2A*SBA) - (4*CAB*CBA2*Lambda5*MW2*SW2)/EL2))/(2*MW*S2B*SW))*( dZh0HHOSAlter()/2D0 ) +&
	&((EL*SBA*((Mh02 + 2*MHH2)*S2A - (2*Lambda5*MW2*(3*S2A + S2B)*SW2)/EL2))/(2*MW*S2B*SW))*( dZHHh0OSAlter() ) - &
	&2D0*Lambda5*MW*CBA*(3D0*S2A-S2B)/((EL/SW)*S2B)*( dgAtMZ()/(EL/SW) - dMW2Alter()/(2D0*MW2) ) -&
	&(EL/SW)*CBA/(2D0*MW*S2B)*( 2D0*S2A*dMh02OSAlter() + S2A*dMHH2OSAlter() - 2D0*MW2*(3D0*S2A-S2B)/(EL2/SW2)*Lambda5*(&
	&dm122MSBarAlter()*(EL2/SW2)/(Lambda5*MW2*S2B) + 2D0*dgAtMZ()/(EL/SW) - dMW2Alter()/MW2 - 2D0*C2B/S2B*dBetaOS2Alter() ) &
	&) + (EL/SW)*CBA*C2A/(MW*S2B)*(6D0*MW2*Lambda5/(EL2/SW2)-2D0*Mh02-MHH2)*dAlphaOS2Alter() -&
	& 2D0*C2B/S2B*Lambda5*MW*CBA/(EL/SW)*dBetaOS2Alter()&
  &)
	case (14)
		totalAmplitude = (0.25D0*CBA2*DBLE((EL2*(2.D0*Mh02 + MHH2)*S2A + 2.D0*Lambda5*MW2*(-3.D0*S2A + S2B)*SW2)**INT(2.D0)))/&
  &(EL2*MW2*S2B2*SW2)*( dZh0h0OS() + dZHHHHOS()/2D0 + dgAtMZ()/(EL/SW) - dMW2Alter()/(2D0*MW2) - SBA/CBA*(dBetaOS12Alter()&
	& - dAlphaOS12Alter()) - 2D0*C2B/S2B*dBetaOS12Alter() ) + &
  &(-(CBA*(EL2*(2*Mh02 + MHH2)*S2A + 2*Lambda5*MW2*(-3*S2A + S2B)*SW2))/(2*EL*MW*S2B*SW))*(&
	&((-3*EL*(Mh02*(2*CAB + S2A*SBA) - (4*CAB*CBA2*Lambda5*MW2*SW2)/EL2))/(2*MW*S2B*SW))*( dZh0HHOSAlter()/2D0 ) +&
	&((EL*SBA*((Mh02 + 2*MHH2)*S2A - (2*Lambda5*MW2*(3*S2A + S2B)*SW2)/EL2))/(2*MW*S2B*SW))*( dZHHh0OSAlter() ) - &
	&2D0*Lambda5*MW*CBA*(3D0*S2A-S2B)/((EL/SW)*S2B)*( dgAtMZ()/(EL/SW) - dMW2Alter()/(2D0*MW2) ) -&
	&(EL/SW)*CBA/(2D0*MW*S2B)*( 2D0*S2A*dMh02OSAlter() + S2A*dMHH2OSAlter() - 2D0*MW2*(3D0*S2A-S2B)/(EL2/SW2)*Lambda5*(&
	&dm122MSBarAlter()*(EL2/SW2)/(Lambda5*MW2*S2B) + 2D0*dgAtMZ()/(EL/SW) - dMW2Alter()/MW2 - 2D0*C2B/S2B*dBetaOS12Alter() ) &
	&) + (EL/SW)*CBA*C2A/(MW*S2B)*(6D0*MW2*Lambda5/(EL2/SW2)-2D0*Mh02-MHH2)*dAlphaOS12Alter() -&
	& 2D0*C2B/S2B*Lambda5*MW*CBA/(EL/SW)*dBetaOS12Alter()&
  &)
	case (15)
		totalAmplitude = (0.25D0*CBA2*DBLE((EL2*(2.D0*Mh02 + MHH2)*S2A + 2.D0*Lambda5*MW2*(-3.D0*S2A + S2B)*SW2)**INT(2.D0)))/&
  &(EL2*MW2*S2B2*SW2)*( dZh0h0OS() + dZHHHHOS()/2D0 + dgAtMZ()/(EL/SW) - dMW2Alter()/(2D0*MW2) - SBA/CBA*(dBetaBFMSAlter()&
	& - dAlphaPinchOS()) - 2D0*C2B/S2B*dBetaBFMSAlter() ) + &
  &(-(CBA*(EL2*(2*Mh02 + MHH2)*S2A + 2*Lambda5*MW2*(-3*S2A + S2B)*SW2))/(2*EL*MW*S2B*SW))*(&
	&((-3*EL*(Mh02*(2*CAB + S2A*SBA) - (4*CAB*CBA2*Lambda5*MW2*SW2)/EL2))/(2*MW*S2B*SW))*( dZh0HHOSAlter()/2D0 ) +&
	&((EL*SBA*((Mh02 + 2*MHH2)*S2A - (2*Lambda5*MW2*(3*S2A + S2B)*SW2)/EL2))/(2*MW*S2B*SW))*( dZHHh0OSAlter() ) - &
	&2D0*Lambda5*MW*CBA*(3D0*S2A-S2B)/((EL/SW)*S2B)*( dgAtMZ()/(EL/SW) - dMW2Alter()/(2D0*MW2) ) -&
	&(EL/SW)*CBA/(2D0*MW*S2B)*( 2D0*S2A*dMh02OSAlter() + S2A*dMHH2OSAlter() - 2D0*MW2*(3D0*S2A-S2B)/(EL2/SW2)*Lambda5*(&
	&dm122MSBarAlter()*(EL2/SW2)/(Lambda5*MW2*S2B) + 2D0*dgAtMZ()/(EL/SW) - dMW2Alter()/MW2 - 2D0*C2B/S2B*dBetaBFMSAlter() ) &
	&) + (EL/SW)*CBA*C2A/(MW*S2B)*(6D0*MW2*Lambda5/(EL2/SW2)-2D0*Mh02-MHH2)*dAlphaPinchOS() -&
	& 2D0*C2B/S2B*Lambda5*MW*CBA/(EL/SW)*dBetaBFMSAlter()&
  &)
	case (16)
		totalAmplitude = (0.25D0*CBA2*DBLE((EL2*(2.D0*Mh02 + MHH2)*S2A + 2.D0*Lambda5*MW2*(-3.D0*S2A + S2B)*SW2)**INT(2.D0)))/&
  &(EL2*MW2*S2B2*SW2)*( dZh0h0OS() + dZHHHHOS()/2D0 + dgAtMZ()/(EL/SW) - dMW2Usual()/(2D0*MW2) - SBA/CBA*(dBetaMSBarUsual()&
	& - dAlphaMSBarUsual()) - 2D0*C2B/S2B*dBetaMSBarUsual() ) + &
  &(-(CBA*(EL2*(2*Mh02 + MHH2)*S2A + 2*Lambda5*MW2*(-3*S2A + S2B)*SW2))/(2*EL*MW*S2B*SW))*(&
	&((-3*EL*(Mh02*(2*CAB + S2A*SBA) - (4*CAB*CBA2*Lambda5*MW2*SW2)/EL2))/(2*MW*S2B*SW))*( dZh0HHOSUsual()/2D0 ) +&
	&((EL*SBA*((Mh02 + 2*MHH2)*S2A - (2*Lambda5*MW2*(3*S2A + S2B)*SW2)/EL2))/(2*MW*S2B*SW))*( dZHHh0OSUsual() ) - &
	&2D0*Lambda5*MW*CBA*(3D0*S2A-S2B)/((EL/SW)*S2B)*( dgAtMZ()/(EL/SW) - dMW2Usual()/(2D0*MW2) ) -&
	&(EL/SW)*CBA/(2D0*MW*S2B)*( 2D0*S2A*dMh02OSUsual() + S2A*dMHH2OSUsual() - 2D0*MW2*(3D0*S2A-S2B)/(EL2/SW2)*Lambda5*(&
	&dm122MSBarUsual()*(EL2/SW2)/(Lambda5*MW2*S2B) + 2D0*dgAtMZ()/(EL/SW) - dMW2Usual()/MW2 - 2D0*C2B/S2B*dBetaMSBarUsual() ) &
	&) + (EL/SW)*CBA*C2A/(MW*S2B)*(6D0*MW2*Lambda5/(EL2/SW2)-2D0*Mh02-MHH2)*dAlphaMSBarUsual() -&
	& 2D0*C2B/S2B*Lambda5*MW*CBA/(EL/SW)*dBetaMSBarUsual()&
  &)
	case (17)
		totalAmplitude = (0.25D0*CBA2*DBLE((EL2*(2.D0*Mh02 + MHH2)*S2A + 2.D0*Lambda5*MW2*(-3.D0*S2A + S2B)*SW2)**INT(2.D0)))/&
  &(EL2*MW2*S2B2*SW2)*( dZh0h0OS() + dZHHHHOS()/2D0 + dgAtMZ()/(EL/SW) - dMW2Alter()/(2D0*MW2) - SBA/CBA*(dBetaMSBarAlter()&
	& - dAlphaMSBarAlter()) - 2D0*C2B/S2B*dBetaMSBarAlter() ) + &
  &(-(CBA*(EL2*(2*Mh02 + MHH2)*S2A + 2*Lambda5*MW2*(-3*S2A + S2B)*SW2))/(2*EL*MW*S2B*SW))*(&
	&((-3*EL*(Mh02*(2*CAB + S2A*SBA) - (4*CAB*CBA2*Lambda5*MW2*SW2)/EL2))/(2*MW*S2B*SW))*( dZh0HHOSAlter()/2D0 ) +&
	&((EL*SBA*((Mh02 + 2*MHH2)*S2A - (2*Lambda5*MW2*(3*S2A + S2B)*SW2)/EL2))/(2*MW*S2B*SW))*( dZHHh0OSAlter() ) - &
	&2D0*Lambda5*MW*CBA*(3D0*S2A-S2B)/((EL/SW)*S2B)*( dgAtMZ()/(EL/SW) - dMW2Alter()/(2D0*MW2) ) -&
	&(EL/SW)*CBA/(2D0*MW*S2B)*( 2D0*S2A*dMh02OSAlter() + S2A*dMHH2OSAlter() - 2D0*MW2*(3D0*S2A-S2B)/(EL2/SW2)*Lambda5*(&
	&dm122MSBarAlter()*(EL2/SW2)/(Lambda5*MW2*S2B) + 2D0*dgAtMZ()/(EL/SW) - dMW2Alter()/MW2 - 2D0*C2B/S2B*dBetaMSBarAlter() ) &
	&) + (EL/SW)*CBA*C2A/(MW*S2B)*(6D0*MW2*Lambda5/(EL2/SW2)-2D0*Mh02-MHH2)*dAlphaMSBarAlter() -&
	& 2D0*C2B/S2B*Lambda5*MW*CBA/(EL/SW)*dBetaMSBarAlter()&
  &)
	case default
		totalAmplitude = 0D0
 end select

 HHtoh0h0CT = totalAmplitude
end function HHtoh0h0CT
