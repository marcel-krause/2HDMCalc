double precision function HHtoA0A0CT(x)
 use constants
 use counterterms
 implicit none
#include "looptools.h"
 integer, intent(in) :: x
 double precision :: totalAmplitude

 select case (x)
	case (1)
		totalAmplitude = (0.25D0*EL2*DBLE((CBA*(2.D0*MA02 - 1.D0*MHH2) + (SAB*(2.D0*MHH2 - (4.D0*Lambda5*MW2*SW2)/EL2))/S2B)&
		&**INT(2.D0)))/(MW2*SW2)*( dZA0A0OS() + dZHHHHOS()/2D0 + dgAtMZ()/(EL/SW) - dMW2Usual()/(2D0*MW2) ) + &
		&(-(EL*(CBA*(2D0*MA02 - MHH2) + (SAB*(2D0*MHH2 - (4D0*Lambda5*MW2*SW2)/EL2))/S2B))/(2D0*MW*SW))*(-(EL*(SBA*&
		&(2D0*MA02 - Mh02) + (CAB*(2D0*Mh02 - (4D0*Lambda5*MW2*SW2)/EL2))/S2B))/(2D0*MW*SW))*( dZh0HHOSUsual()/2D0 ) + &
		&(-(EL*(CBA*(2D0*MA02 - MHH2) + (SAB*(2D0*MHH2 - (4D0*Lambda5*MW2*SW2)/EL2))/S2B))/(2D0*MW*SW))*(-EL*SBA*(MA02 &
		&- MHH2)/(2D0*MW*SW))*( dZG0A0OSUsual() ) + &
		&(-(EL*(CBA*(2D0*MA02 - MHH2) + (SAB*(2D0*MHH2 - (4D0*Lambda5*MW2*SW2)/EL2))/S2B))/(2D0*MW*SW))*( &
		& -4D0*MW*Lambda5*SAB*SW/(EL*S2B)*( dgAtMZ()/(EL/SW) - dMW2Usual()/(2D0*MW2) ) - &
		& EL/(2D0*MW*SW)*(&
		&	(2D0*SAB/S2B-CBA)*dMHH2OSUsual() + 2D0*CBA*dMA02OSUsual() - 4D0*MW2*SAB*SW2/(EL2*S2B)*Lambda5*(&
		&		2D0*dgAtMZ()/(EL/SW) - dMW2Usual()/MW2 - 2D0*C2B/S2B*dBeta1KanUsual() + EL2*dm122MSBarUsual()/(SW2*MW2*&
		&			S2B*Lambda5)&
		&		) + &
		&	(SBA*(2D0*MA02-MHH2) + CAB/S2B*(2D0*MHH2 - 4D0*SW2*MW2*Lambda5/EL2))*dAlphaKanUsual() + &
		&	(-SBA*(2D0*MA02-MHH2) + (CAB/S2B - 2D0*SAB*C2B/(S2B**2))*(2D0*MHH2-4D0*MW2*SW2*Lambda5/EL2))*dBeta1KanUsual()&
		&	)&
		& )
	case (2)
		totalAmplitude = (0.25D0*EL2*DBLE((CBA*(2.D0*MA02 - 1.D0*MHH2) + (SAB*(2.D0*MHH2 - (4.D0*Lambda5*MW2*SW2)/EL2))/&
		&S2B)**INT(2.D0)))/(MW2*SW2)*( dZA0A0OS() + dZHHHHOS()/2D0 + dgAtMZ()/(EL/SW) - dMW2Usual()/(2D0*MW2) ) + &
		&(-(EL*(CBA*(2D0*MA02 - MHH2) + (SAB*(2D0*MHH2 - (4D0*Lambda5*MW2*SW2)/EL2))/S2B))/(2D0*MW*SW))*(-(EL*(SBA*&
		&(2D0*MA02 - Mh02) + (CAB*(2D0*Mh02 - (4D0*Lambda5*MW2*SW2)/EL2))/S2B))/(2D0*MW*SW))*( dZh0HHOSUsual()/2D0 ) + &
		&(-(EL*(CBA*(2D0*MA02 - MHH2) + (SAB*(2D0*MHH2 - (4D0*Lambda5*MW2*SW2)/EL2))/S2B))/(2D0*MW*SW))*(-EL*SBA*(MA02 &
		&- MHH2)/(2D0*MW*SW))*( dZG0A0OSUsual() ) + &
		&(-(EL*(CBA*(2D0*MA02 - MHH2) + (SAB*(2D0*MHH2 - (4D0*Lambda5*MW2*SW2)/EL2))/S2B))/(2D0*MW*SW))*( &
		& -4D0*MW*Lambda5*SAB*SW/(EL*S2B)*( dgAtMZ()/(EL/SW) - dMW2Usual()/(2D0*MW2) ) - &
		& EL/(2D0*MW*SW)*(&
		&	(2D0*SAB/S2B-CBA)*dMHH2OSUsual() + 2D0*CBA*dMA02OSUsual() - 4D0*MW2*SAB*SW2/(EL2*S2B)*Lambda5*(&
		&		2D0*dgAtMZ()/(EL/SW) - dMW2Usual()/MW2 - 2D0*C2B/S2B*dBeta2KanUsual() + EL2*dm122MSBarUsual()/(SW2*MW2*&
		&			S2B*Lambda5)&
		&		) + &
		&	(SBA*(2D0*MA02-MHH2) + CAB/S2B*(2D0*MHH2 - 4D0*SW2*MW2*Lambda5/EL2))*dAlphaKanUsual() + &
		&	(-SBA*(2D0*MA02-MHH2) + (CAB/S2B - 2D0*SAB*C2B/(S2B**2))*(2D0*MHH2-4D0*MW2*SW2*Lambda5/EL2))*dBeta2KanUsual()&
		&	)&
		& )
	case (3)
		totalAmplitude = (0.25D0*EL2*DBLE((CBA*(2.D0*MA02 - 1.D0*MHH2) + (SAB*(2.D0*MHH2 - (4.D0*Lambda5*MW2*SW2)/EL2))/&
		&S2B)**INT(2.D0)))/(MW2*SW2)*( dZA0A0OS() + dZHHHHOS()/2D0 + dgAtMZ()/(EL/SW) - dMW2Alter()/(2D0*MW2) ) + &
		&(-(EL*(CBA*(2D0*MA02 - MHH2) + (SAB*(2D0*MHH2 - (4D0*Lambda5*MW2*SW2)/EL2))/S2B))/(2D0*MW*SW))*(-(EL*(SBA*&
		&(2D0*MA02 - Mh02) + (CAB*(2D0*Mh02 - (4D0*Lambda5*MW2*SW2)/EL2))/S2B))/(2D0*MW*SW))*( dZh0HHOSAlter()/2D0 ) + &
		&(-(EL*(CBA*(2D0*MA02 - MHH2) + (SAB*(2D0*MHH2 - (4D0*Lambda5*MW2*SW2)/EL2))/S2B))/(2D0*MW*SW))*(-EL*SBA*(MA02 &
		&- MHH2)/(2D0*MW*SW))*( dZG0A0OSAlter() ) + &
		&(-(EL*(CBA*(2D0*MA02 - MHH2) + (SAB*(2D0*MHH2 - (4D0*Lambda5*MW2*SW2)/EL2))/S2B))/(2D0*MW*SW))*( &
		& -4D0*MW*Lambda5*SAB*SW/(EL*S2B)*( dgAtMZ()/(EL/SW) - dMW2Alter()/(2D0*MW2) ) - &
		& EL/(2D0*MW*SW)*(&
		&	(2D0*SAB/S2B-CBA)*dMHH2OSAlter() + 2D0*CBA*dMA02OSAlter() - 4D0*MW2*SAB*SW2/(EL2*S2B)*Lambda5*(&
		&		2D0*dgAtMZ()/(EL/SW) - dMW2Alter()/MW2 - 2D0*C2B/S2B*dBeta1KanAlter() + EL2*dm122MSBarAlter()/(SW2*MW2*&
		&			S2B*Lambda5)&
		&		) + &
		&	(SBA*(2D0*MA02-MHH2) + CAB/S2B*(2D0*MHH2 - 4D0*SW2*MW2*Lambda5/EL2))*dAlphaKanAlter() + &
		&	(-SBA*(2D0*MA02-MHH2) + (CAB/S2B - 2D0*SAB*C2B/(S2B**2))*(2D0*MHH2-4D0*MW2*SW2*Lambda5/EL2))*dBeta1KanAlter()&
		&	)&
		& )
	case (4)
		totalAmplitude = (0.25D0*EL2*DBLE((CBA*(2.D0*MA02 - 1.D0*MHH2) + (SAB*(2.D0*MHH2 - (4.D0*Lambda5*MW2*SW2)/EL2))/&
		&S2B)**INT(2.D0)))/(MW2*SW2)*( dZA0A0OS() + dZHHHHOS()/2D0 + dgAtMZ()/(EL/SW) - dMW2Alter()/(2D0*MW2) ) + &
		&(-(EL*(CBA*(2D0*MA02 - MHH2) + (SAB*(2D0*MHH2 - (4D0*Lambda5*MW2*SW2)/EL2))/S2B))/(2D0*MW*SW))*(-(EL*(SBA*&
		&(2D0*MA02 - Mh02) + (CAB*(2D0*Mh02 - (4D0*Lambda5*MW2*SW2)/EL2))/S2B))/(2D0*MW*SW))*( dZh0HHOSAlter()/2D0 ) + &
		&(-(EL*(CBA*(2D0*MA02 - MHH2) + (SAB*(2D0*MHH2 - (4D0*Lambda5*MW2*SW2)/EL2))/S2B))/(2D0*MW*SW))*(-EL*SBA*(MA02 &
		&- MHH2)/(2D0*MW*SW))*( dZG0A0OSAlter() ) + &
		&(-(EL*(CBA*(2D0*MA02 - MHH2) + (SAB*(2D0*MHH2 - (4D0*Lambda5*MW2*SW2)/EL2))/S2B))/(2D0*MW*SW))*( &
		& -4D0*MW*Lambda5*SAB*SW/(EL*S2B)*( dgAtMZ()/(EL/SW) - dMW2Alter()/(2D0*MW2) ) - &
		& EL/(2D0*MW*SW)*(&
		&	(2D0*SAB/S2B-CBA)*dMHH2OSAlter() + 2D0*CBA*dMA02OSAlter() - 4D0*MW2*SAB*SW2/(EL2*S2B)*Lambda5*(&
		&		2D0*dgAtMZ()/(EL/SW) - dMW2Alter()/MW2 - 2D0*C2B/S2B*dBeta2KanAlter() + EL2*dm122MSBarAlter()/(SW2*MW2*&
		&			S2B*Lambda5)&
		&		) + &
		&	(SBA*(2D0*MA02-MHH2) + CAB/S2B*(2D0*MHH2 - 4D0*SW2*MW2*Lambda5/EL2))*dAlphaKanAlter() + &
		&	(-SBA*(2D0*MA02-MHH2) + (CAB/S2B - 2D0*SAB*C2B/(S2B**2))*(2D0*MHH2-4D0*MW2*SW2*Lambda5/EL2))*dBeta2KanAlter()&
		&	)&
		& )
	case (5)
		totalAmplitude = (0.25D0*EL2*DBLE((CBA*(2.D0*MA02 - 1.D0*MHH2) + (SAB*(2.D0*MHH2 - (4.D0*Lambda5*MW2*SW2)/EL2))/&
		&S2B)**INT(2.D0)))/(MW2*SW2)*( dZA0A0OS() + dZHHHHOS()/2D0 + dgAtMZ()/(EL/SW) - dMW2Alter()/(2D0*MW2) ) + &
		&(-(EL*(CBA*(2D0*MA02 - MHH2) + (SAB*(2D0*MHH2 - (4D0*Lambda5*MW2*SW2)/EL2))/S2B))/(2D0*MW*SW))*(-(EL*(SBA*&
		&(2D0*MA02 - Mh02) + (CAB*(2D0*Mh02 - (4D0*Lambda5*MW2*SW2)/EL2))/S2B))/(2D0*MW*SW))*( dZh0HHOSAlter()/2D0 ) + &
		&(-(EL*(CBA*(2D0*MA02 - MHH2) + (SAB*(2D0*MHH2 - (4D0*Lambda5*MW2*SW2)/EL2))/S2B))/(2D0*MW*SW))*(-EL*SBA*(MA02 &
		&- MHH2)/(2D0*MW*SW))*( dZG0A0OSAlter() ) + &
		&(-(EL*(CBA*(2D0*MA02 - MHH2) + (SAB*(2D0*MHH2 - (4D0*Lambda5*MW2*SW2)/EL2))/S2B))/(2D0*MW*SW))*( &
		& -4D0*MW*Lambda5*SAB*SW/(EL*S2B)*( dgAtMZ()/(EL/SW) - dMW2Alter()/(2D0*MW2) ) - &
		& EL/(2D0*MW*SW)*(&
		&	(2D0*SAB/S2B-CBA)*dMHH2OSAlter() + 2D0*CBA*dMA02OSAlter() - 4D0*MW2*SAB*SW2/(EL2*S2B)*Lambda5*(&
		&		2D0*dgAtMZ()/(EL/SW) - dMW2Alter()/MW2 - 2D0*C2B/S2B*dBeta1PinchPStar() + EL2*dm122MSBarAlter()/(SW2*MW2*&
		&			S2B*Lambda5)&
		&		) + &
		&	(SBA*(2D0*MA02-MHH2) + CAB/S2B*(2D0*MHH2 - 4D0*SW2*MW2*Lambda5/EL2))*dAlphaPinchPStar() + &
		&	(-SBA*(2D0*MA02-MHH2) + (CAB/S2B - 2D0*SAB*C2B/(S2B**2))*(2D0*MHH2-4D0*MW2*SW2*Lambda5/EL2))*dBeta1PinchPStar()&
		&	)&
		& )
	case (6)
		totalAmplitude = (0.25D0*EL2*DBLE((CBA*(2.D0*MA02 - 1.D0*MHH2) + (SAB*(2.D0*MHH2 - (4.D0*Lambda5*MW2*SW2)/EL2))/&
		&S2B)**INT(2.D0)))/(MW2*SW2)*( dZA0A0OS() + dZHHHHOS()/2D0 + dgAtMZ()/(EL/SW) - dMW2Alter()/(2D0*MW2) ) + &
		&(-(EL*(CBA*(2D0*MA02 - MHH2) + (SAB*(2D0*MHH2 - (4D0*Lambda5*MW2*SW2)/EL2))/S2B))/(2D0*MW*SW))*(-(EL*(SBA*&
		&(2D0*MA02 - Mh02) + (CAB*(2D0*Mh02 - (4D0*Lambda5*MW2*SW2)/EL2))/S2B))/(2D0*MW*SW))*( dZh0HHOSAlter()/2D0 ) + &
		&(-(EL*(CBA*(2D0*MA02 - MHH2) + (SAB*(2D0*MHH2 - (4D0*Lambda5*MW2*SW2)/EL2))/S2B))/(2D0*MW*SW))*(-EL*SBA*(MA02 &
		&- MHH2)/(2D0*MW*SW))*( dZG0A0OSAlter() ) + &
		&(-(EL*(CBA*(2D0*MA02 - MHH2) + (SAB*(2D0*MHH2 - (4D0*Lambda5*MW2*SW2)/EL2))/S2B))/(2D0*MW*SW))*( &
		& -4D0*MW*Lambda5*SAB*SW/(EL*S2B)*( dgAtMZ()/(EL/SW) - dMW2Alter()/(2D0*MW2) ) - &
		& EL/(2D0*MW*SW)*(&
		&	(2D0*SAB/S2B-CBA)*dMHH2OSAlter() + 2D0*CBA*dMA02OSAlter() - 4D0*MW2*SAB*SW2/(EL2*S2B)*Lambda5*(&
		&		2D0*dgAtMZ()/(EL/SW) - dMW2Alter()/MW2 - 2D0*C2B/S2B*dBeta2PinchPStar() + EL2*dm122MSBarAlter()/(SW2*MW2*&
		&			S2B*Lambda5)&
		&		) + &
		&	(SBA*(2D0*MA02-MHH2) + CAB/S2B*(2D0*MHH2 - 4D0*SW2*MW2*Lambda5/EL2))*dAlphaPinchPStar() + &
		&	(-SBA*(2D0*MA02-MHH2) + (CAB/S2B - 2D0*SAB*C2B/(S2B**2))*(2D0*MHH2-4D0*MW2*SW2*Lambda5/EL2))*dBeta2PinchPStar()&
		&	)&
		& )
	case (7)
		totalAmplitude = (0.25D0*EL2*DBLE((CBA*(2.D0*MA02 - 1.D0*MHH2) + (SAB*(2.D0*MHH2 - (4.D0*Lambda5*MW2*SW2)/EL2))/&
		&S2B)**INT(2.D0)))/(MW2*SW2)*( dZA0A0OS() + dZHHHHOS()/2D0 + dgAtMZ()/(EL/SW) - dMW2Alter()/(2D0*MW2) ) + &
		&(-(EL*(CBA*(2D0*MA02 - MHH2) + (SAB*(2D0*MHH2 - (4D0*Lambda5*MW2*SW2)/EL2))/S2B))/(2D0*MW*SW))*(-(EL*(SBA*&
		&(2D0*MA02 - Mh02) + (CAB*(2D0*Mh02 - (4D0*Lambda5*MW2*SW2)/EL2))/S2B))/(2D0*MW*SW))*( dZh0HHOSAlter()/2D0 ) + &
		&(-(EL*(CBA*(2D0*MA02 - MHH2) + (SAB*(2D0*MHH2 - (4D0*Lambda5*MW2*SW2)/EL2))/S2B))/(2D0*MW*SW))*(-EL*SBA*(MA02 &
		&- MHH2)/(2D0*MW*SW))*( dZG0A0OSAlter() ) + &
		&(-(EL*(CBA*(2D0*MA02 - MHH2) + (SAB*(2D0*MHH2 - (4D0*Lambda5*MW2*SW2)/EL2))/S2B))/(2D0*MW*SW))*( &
		& -4D0*MW*Lambda5*SAB*SW/(EL*S2B)*( dgAtMZ()/(EL/SW) - dMW2Alter()/(2D0*MW2) ) - &
		& EL/(2D0*MW*SW)*(&
		&	(2D0*SAB/S2B-CBA)*dMHH2OSAlter() + 2D0*CBA*dMA02OSAlter() - 4D0*MW2*SAB*SW2/(EL2*S2B)*Lambda5*(&
		&		2D0*dgAtMZ()/(EL/SW) - dMW2Alter()/MW2 - 2D0*C2B/S2B*dBeta1PinchOS() + EL2*dm122MSBarAlter()/(SW2*MW2*&
		&			S2B*Lambda5)&
		&		) + &
		&	(SBA*(2D0*MA02-MHH2) + CAB/S2B*(2D0*MHH2 - 4D0*SW2*MW2*Lambda5/EL2))*dAlphaPinchOS() + &
		&	(-SBA*(2D0*MA02-MHH2) + (CAB/S2B - 2D0*SAB*C2B/(S2B**2))*(2D0*MHH2-4D0*MW2*SW2*Lambda5/EL2))*dBeta1PinchOS()&
		&	)&
		& )
	case (8)
		totalAmplitude = (0.25D0*EL2*DBLE((CBA*(2.D0*MA02 - 1.D0*MHH2) + (SAB*(2.D0*MHH2 - (4.D0*Lambda5*MW2*SW2)/EL2))/&
		&S2B)**INT(2.D0)))/(MW2*SW2)*( dZA0A0OS() + dZHHHHOS()/2D0 + dgAtMZ()/(EL/SW) - dMW2Alter()/(2D0*MW2) ) + &
		&(-(EL*(CBA*(2D0*MA02 - MHH2) + (SAB*(2D0*MHH2 - (4D0*Lambda5*MW2*SW2)/EL2))/S2B))/(2D0*MW*SW))*(-(EL*(SBA*&
		&(2D0*MA02 - Mh02) + (CAB*(2D0*Mh02 - (4D0*Lambda5*MW2*SW2)/EL2))/S2B))/(2D0*MW*SW))*( dZh0HHOSAlter()/2D0 ) + &
		&(-(EL*(CBA*(2D0*MA02 - MHH2) + (SAB*(2D0*MHH2 - (4D0*Lambda5*MW2*SW2)/EL2))/S2B))/(2D0*MW*SW))*(-EL*SBA*(MA02 &
		&- MHH2)/(2D0*MW*SW))*( dZG0A0OSAlter() ) + &
		&(-(EL*(CBA*(2D0*MA02 - MHH2) + (SAB*(2D0*MHH2 - (4D0*Lambda5*MW2*SW2)/EL2))/S2B))/(2D0*MW*SW))*( &
		& -4D0*MW*Lambda5*SAB*SW/(EL*S2B)*( dgAtMZ()/(EL/SW) - dMW2Alter()/(2D0*MW2) ) - &
		& EL/(2D0*MW*SW)*(&
		&	(2D0*SAB/S2B-CBA)*dMHH2OSAlter() + 2D0*CBA*dMA02OSAlter() - 4D0*MW2*SAB*SW2/(EL2*S2B)*Lambda5*(&
		&		2D0*dgAtMZ()/(EL/SW) - dMW2Alter()/MW2 - 2D0*C2B/S2B*dBeta2PinchOS() + EL2*dm122MSBarAlter()/(SW2*MW2*&
		&			S2B*Lambda5)&
		&		) + &
		&	(SBA*(2D0*MA02-MHH2) + CAB/S2B*(2D0*MHH2 - 4D0*SW2*MW2*Lambda5/EL2))*dAlphaPinchOS() + &
		&	(-SBA*(2D0*MA02-MHH2) + (CAB/S2B - 2D0*SAB*C2B/(S2B**2))*(2D0*MHH2-4D0*MW2*SW2*Lambda5/EL2))*dBeta2PinchOS()&
		&	)&
		& )
	case (9)
		totalAmplitude = (0.25D0*EL2*DBLE((CBA*(2.D0*MA02 - 1.D0*MHH2) + (SAB*(2.D0*MHH2 - (4.D0*Lambda5*MW2*SW2)/EL2))/&
		&S2B)**INT(2.D0)))/(MW2*SW2)*( dZA0A0OS() + dZHHHHOS()/2D0 + dgAtMZ()/(EL/SW) - dMW2Alter()/(2D0*MW2) ) + &
		&(-(EL*(CBA*(2D0*MA02 - MHH2) + (SAB*(2D0*MHH2 - (4D0*Lambda5*MW2*SW2)/EL2))/S2B))/(2D0*MW*SW))*(-(EL*(SBA*&
		&(2D0*MA02 - Mh02) + (CAB*(2D0*Mh02 - (4D0*Lambda5*MW2*SW2)/EL2))/S2B))/(2D0*MW*SW))*( dZh0HHOSAlter()/2D0 ) + &
		&(-(EL*(CBA*(2D0*MA02 - MHH2) + (SAB*(2D0*MHH2 - (4D0*Lambda5*MW2*SW2)/EL2))/S2B))/(2D0*MW*SW))*(-EL*SBA*(MA02 &
		&- MHH2)/(2D0*MW*SW))*( dZG0A0OSAlter() ) + &
		&(-(EL*(CBA*(2D0*MA02 - MHH2) + (SAB*(2D0*MHH2 - (4D0*Lambda5*MW2*SW2)/EL2))/S2B))/(2D0*MW*SW))*( &
		& -4D0*MW*Lambda5*SAB*SW/(EL*S2B)*( dgAtMZ()/(EL/SW) - dMW2Alter()/(2D0*MW2) ) - &
		& EL/(2D0*MW*SW)*(&
		&	(2D0*SAB/S2B-CBA)*dMHH2OSAlter() + 2D0*CBA*dMA02OSAlter() - 4D0*MW2*SAB*SW2/(EL2*S2B)*Lambda5*(&
		&		2D0*dgAtMZ()/(EL/SW) - dMW2Alter()/MW2 - 2D0*C2B/S2B*dBetaProcDep1Alter() + EL2*dm122MSBarAlter()/(SW2*MW2*&
		&			S2B*Lambda5)&
		&		) + &
		&	(SBA*(2D0*MA02-MHH2) + CAB/S2B*(2D0*MHH2 - 4D0*SW2*MW2*Lambda5/EL2))*dAlphaProcDep1Alter() + &
		&	(-SBA*(2D0*MA02-MHH2) + (CAB/S2B - 2D0*SAB*C2B/(S2B**2))*(2D0*MHH2-4D0*MW2*SW2*Lambda5/EL2))*dBetaProcDep1Alter()&
		&	)&
		& )
	case (10)
		totalAmplitude = (0.25D0*EL2*DBLE((CBA*(2.D0*MA02 - 1.D0*MHH2) + (SAB*(2.D0*MHH2 - (4.D0*Lambda5*MW2*SW2)/EL2))/&
		&S2B)**INT(2.D0)))/(MW2*SW2)*( dZA0A0OS() + dZHHHHOS()/2D0 + dgAtMZ()/(EL/SW) - dMW2Alter()/(2D0*MW2) ) + &
		&(-(EL*(CBA*(2D0*MA02 - MHH2) + (SAB*(2D0*MHH2 - (4D0*Lambda5*MW2*SW2)/EL2))/S2B))/(2D0*MW*SW))*(-(EL*(SBA*&
		&(2D0*MA02 - Mh02) + (CAB*(2D0*Mh02 - (4D0*Lambda5*MW2*SW2)/EL2))/S2B))/(2D0*MW*SW))*( dZh0HHOSAlter()/2D0 ) + &
		&(-(EL*(CBA*(2D0*MA02 - MHH2) + (SAB*(2D0*MHH2 - (4D0*Lambda5*MW2*SW2)/EL2))/S2B))/(2D0*MW*SW))*(-EL*SBA*(MA02 &
		&- MHH2)/(2D0*MW*SW))*( dZG0A0OSAlter() ) + &
		&(-(EL*(CBA*(2D0*MA02 - MHH2) + (SAB*(2D0*MHH2 - (4D0*Lambda5*MW2*SW2)/EL2))/S2B))/(2D0*MW*SW))*( &
		& -4D0*MW*Lambda5*SAB*SW/(EL*S2B)*( dgAtMZ()/(EL/SW) - dMW2Alter()/(2D0*MW2) ) - &
		& EL/(2D0*MW*SW)*(&
		&	(2D0*SAB/S2B-CBA)*dMHH2OSAlter() + 2D0*CBA*dMA02OSAlter() - 4D0*MW2*SAB*SW2/(EL2*S2B)*Lambda5*(&
		&		2D0*dgAtMZ()/(EL/SW) - dMW2Alter()/MW2 - 2D0*C2B/S2B*dBetaProcDep2Alter() + EL2*dm122MSBarAlter()/(SW2*MW2*&
		&			S2B*Lambda5)&
		&		) + &
		&	(SBA*(2D0*MA02-MHH2) + CAB/S2B*(2D0*MHH2 - 4D0*SW2*MW2*Lambda5/EL2))*dAlphaProcDep2Alter() + &
		&	(-SBA*(2D0*MA02-MHH2) + (CAB/S2B - 2D0*SAB*C2B/(S2B**2))*(2D0*MHH2-4D0*MW2*SW2*Lambda5/EL2))*dBetaProcDep2Alter()&
		&	)&
		& )
	case (11)
		totalAmplitude = (0.25D0*EL2*DBLE((CBA*(2.D0*MA02 - 1.D0*MHH2) + (SAB*(2.D0*MHH2 - (4.D0*Lambda5*MW2*SW2)/EL2))/&
		&S2B)**INT(2.D0)))/(MW2*SW2)*( dZA0A0OS() + dZHHHHOS()/2D0 + dgAtMZ()/(EL/SW) - dMW2Alter()/(2D0*MW2) ) + &
		&(-(EL*(CBA*(2D0*MA02 - MHH2) + (SAB*(2D0*MHH2 - (4D0*Lambda5*MW2*SW2)/EL2))/S2B))/(2D0*MW*SW))*(-(EL*(SBA*&
		&(2D0*MA02 - Mh02) + (CAB*(2D0*Mh02 - (4D0*Lambda5*MW2*SW2)/EL2))/S2B))/(2D0*MW*SW))*( dZh0HHOSAlter()/2D0 ) + &
		&(-(EL*(CBA*(2D0*MA02 - MHH2) + (SAB*(2D0*MHH2 - (4D0*Lambda5*MW2*SW2)/EL2))/S2B))/(2D0*MW*SW))*(-EL*SBA*(MA02 &
		&- MHH2)/(2D0*MW*SW))*( dZG0A0OSAlter() ) + &
		&(-(EL*(CBA*(2D0*MA02 - MHH2) + (SAB*(2D0*MHH2 - (4D0*Lambda5*MW2*SW2)/EL2))/S2B))/(2D0*MW*SW))*( &
		& -4D0*MW*Lambda5*SAB*SW/(EL*S2B)*( dgAtMZ()/(EL/SW) - dMW2Alter()/(2D0*MW2) ) - &
		& EL/(2D0*MW*SW)*(&
		&	(2D0*SAB/S2B-CBA)*dMHH2OSAlter() + 2D0*CBA*dMA02OSAlter() - 4D0*MW2*SAB*SW2/(EL2*S2B)*Lambda5*(&
		&		2D0*dgAtMZ()/(EL/SW) - dMW2Alter()/MW2 - 2D0*C2B/S2B*dBetaProcDep3Alter() + EL2*dm122MSBarAlter()/(SW2*MW2*&
		&			S2B*Lambda5)&
		&		) + &
		&	(SBA*(2D0*MA02-MHH2) + CAB/S2B*(2D0*MHH2 - 4D0*SW2*MW2*Lambda5/EL2))*dAlphaProcDep3Alter() + &
		&	(-SBA*(2D0*MA02-MHH2) + (CAB/S2B - 2D0*SAB*C2B/(S2B**2))*(2D0*MHH2-4D0*MW2*SW2*Lambda5/EL2))*dBetaProcDep3Alter()&
		&	)&
		& )
	case (12)
		totalAmplitude = (0.25D0*EL2*DBLE((CBA*(2.D0*MA02 - 1.D0*MHH2) + (SAB*(2.D0*MHH2 - (4.D0*Lambda5*MW2*SW2)/EL2))/&
		&S2B)**INT(2.D0)))/(MW2*SW2)*( dZA0A0OS() + dZHHHHOS()/2D0 + dgAtMZ()/(EL/SW) - dMW2Alter()/(2D0*MW2) ) + &
		&(-(EL*(CBA*(2D0*MA02 - MHH2) + (SAB*(2D0*MHH2 - (4D0*Lambda5*MW2*SW2)/EL2))/S2B))/(2D0*MW*SW))*(-(EL*(SBA*&
		&(2D0*MA02 - Mh02) + (CAB*(2D0*Mh02 - (4D0*Lambda5*MW2*SW2)/EL2))/S2B))/(2D0*MW*SW))*( dZh0HHOSAlter()/2D0 ) + &
		&(-(EL*(CBA*(2D0*MA02 - MHH2) + (SAB*(2D0*MHH2 - (4D0*Lambda5*MW2*SW2)/EL2))/S2B))/(2D0*MW*SW))*(-EL*SBA*(MA02 &
		&- MHH2)/(2D0*MW*SW))*( dZG0A0OSAlter() ) + &
		&(-(EL*(CBA*(2D0*MA02 - MHH2) + (SAB*(2D0*MHH2 - (4D0*Lambda5*MW2*SW2)/EL2))/S2B))/(2D0*MW*SW))*( &
		& -4D0*MW*Lambda5*SAB*SW/(EL*S2B)*( dgAtMZ()/(EL/SW) - dMW2Alter()/(2D0*MW2) ) - &
		& EL/(2D0*MW*SW)*(&
		&	(2D0*SAB/S2B-CBA)*dMHH2OSAlter() + 2D0*CBA*dMA02OSAlter() - 4D0*MW2*SAB*SW2/(EL2*S2B)*Lambda5*(&
		&		2D0*dgAtMZ()/(EL/SW) - dMW2Alter()/MW2 - 2D0*C2B/S2B*dBetaOS1Alter() + EL2*dm122MSBarAlter()/(SW2*MW2*&
		&			S2B*Lambda5)&
		&		) + &
		&	(SBA*(2D0*MA02-MHH2) + CAB/S2B*(2D0*MHH2 - 4D0*SW2*MW2*Lambda5/EL2))*dAlphaOS1Alter() + &
		&	(-SBA*(2D0*MA02-MHH2) + (CAB/S2B - 2D0*SAB*C2B/(S2B**2))*(2D0*MHH2-4D0*MW2*SW2*Lambda5/EL2))*dBetaOS1Alter()&
		&	)&
		& )
	case (13)
		totalAmplitude = (0.25D0*EL2*DBLE((CBA*(2.D0*MA02 - 1.D0*MHH2) + (SAB*(2.D0*MHH2 - (4.D0*Lambda5*MW2*SW2)/EL2))/&
		&S2B)**INT(2.D0)))/(MW2*SW2)*( dZA0A0OS() + dZHHHHOS()/2D0 + dgAtMZ()/(EL/SW) - dMW2Alter()/(2D0*MW2) ) + &
		&(-(EL*(CBA*(2D0*MA02 - MHH2) + (SAB*(2D0*MHH2 - (4D0*Lambda5*MW2*SW2)/EL2))/S2B))/(2D0*MW*SW))*(-(EL*(SBA*&
		&(2D0*MA02 - Mh02) + (CAB*(2D0*Mh02 - (4D0*Lambda5*MW2*SW2)/EL2))/S2B))/(2D0*MW*SW))*( dZh0HHOSAlter()/2D0 ) + &
		&(-(EL*(CBA*(2D0*MA02 - MHH2) + (SAB*(2D0*MHH2 - (4D0*Lambda5*MW2*SW2)/EL2))/S2B))/(2D0*MW*SW))*(-EL*SBA*(MA02 &
		&- MHH2)/(2D0*MW*SW))*( dZG0A0OSAlter() ) + &
		&(-(EL*(CBA*(2D0*MA02 - MHH2) + (SAB*(2D0*MHH2 - (4D0*Lambda5*MW2*SW2)/EL2))/S2B))/(2D0*MW*SW))*( &
		& -4D0*MW*Lambda5*SAB*SW/(EL*S2B)*( dgAtMZ()/(EL/SW) - dMW2Alter()/(2D0*MW2) ) - &
		& EL/(2D0*MW*SW)*(&
		&	(2D0*SAB/S2B-CBA)*dMHH2OSAlter() + 2D0*CBA*dMA02OSAlter() - 4D0*MW2*SAB*SW2/(EL2*S2B)*Lambda5*(&
		&		2D0*dgAtMZ()/(EL/SW) - dMW2Alter()/MW2 - 2D0*C2B/S2B*dBetaOS2Alter() + EL2*dm122MSBarAlter()/(SW2*MW2*&
		&			S2B*Lambda5)&
		&		) + &
		&	(SBA*(2D0*MA02-MHH2) + CAB/S2B*(2D0*MHH2 - 4D0*SW2*MW2*Lambda5/EL2))*dAlphaOS2Alter() + &
		&	(-SBA*(2D0*MA02-MHH2) + (CAB/S2B - 2D0*SAB*C2B/(S2B**2))*(2D0*MHH2-4D0*MW2*SW2*Lambda5/EL2))*dBetaOS2Alter()&
		&	)&
		& )
	case (14)
		totalAmplitude = (0.25D0*EL2*DBLE((CBA*(2.D0*MA02 - 1.D0*MHH2) + (SAB*(2.D0*MHH2 - (4.D0*Lambda5*MW2*SW2)/EL2))/&
		&S2B)**INT(2.D0)))/(MW2*SW2)*( dZA0A0OS() + dZHHHHOS()/2D0 + dgAtMZ()/(EL/SW) - dMW2Alter()/(2D0*MW2) ) + &
		&(-(EL*(CBA*(2D0*MA02 - MHH2) + (SAB*(2D0*MHH2 - (4D0*Lambda5*MW2*SW2)/EL2))/S2B))/(2D0*MW*SW))*(-(EL*(SBA*&
		&(2D0*MA02 - Mh02) + (CAB*(2D0*Mh02 - (4D0*Lambda5*MW2*SW2)/EL2))/S2B))/(2D0*MW*SW))*( dZh0HHOSAlter()/2D0 ) + &
		&(-(EL*(CBA*(2D0*MA02 - MHH2) + (SAB*(2D0*MHH2 - (4D0*Lambda5*MW2*SW2)/EL2))/S2B))/(2D0*MW*SW))*(-EL*SBA*(MA02 &
		&- MHH2)/(2D0*MW*SW))*( dZG0A0OSAlter() ) + &
		&(-(EL*(CBA*(2D0*MA02 - MHH2) + (SAB*(2D0*MHH2 - (4D0*Lambda5*MW2*SW2)/EL2))/S2B))/(2D0*MW*SW))*( &
		& -4D0*MW*Lambda5*SAB*SW/(EL*S2B)*( dgAtMZ()/(EL/SW) - dMW2Alter()/(2D0*MW2) ) - &
		& EL/(2D0*MW*SW)*(&
		&	(2D0*SAB/S2B-CBA)*dMHH2OSAlter() + 2D0*CBA*dMA02OSAlter() - 4D0*MW2*SAB*SW2/(EL2*S2B)*Lambda5*(&
		&		2D0*dgAtMZ()/(EL/SW) - dMW2Alter()/MW2 - 2D0*C2B/S2B*dBetaOS12Alter() + EL2*dm122MSBarAlter()/(SW2*MW2*&
		&			S2B*Lambda5)&
		&		) + &
		&	(SBA*(2D0*MA02-MHH2) + CAB/S2B*(2D0*MHH2 - 4D0*SW2*MW2*Lambda5/EL2))*dAlphaOS12Alter() + &
		&	(-SBA*(2D0*MA02-MHH2) + (CAB/S2B - 2D0*SAB*C2B/(S2B**2))*(2D0*MHH2-4D0*MW2*SW2*Lambda5/EL2))*dBetaOS12Alter()&
		&	)&
		& )
	case (15)
		totalAmplitude = (0.25D0*EL2*DBLE((CBA*(2.D0*MA02 - 1.D0*MHH2) + (SAB*(2.D0*MHH2 - (4.D0*Lambda5*MW2*SW2)/EL2))/&
		&S2B)**INT(2.D0)))/(MW2*SW2)*( dZA0A0OS() + dZHHHHOS()/2D0 + dgAtMZ()/(EL/SW) - dMW2Alter()/(2D0*MW2) ) + &
		&(-(EL*(CBA*(2D0*MA02 - MHH2) + (SAB*(2D0*MHH2 - (4D0*Lambda5*MW2*SW2)/EL2))/S2B))/(2D0*MW*SW))*(-(EL*(SBA*&
		&(2D0*MA02 - Mh02) + (CAB*(2D0*Mh02 - (4D0*Lambda5*MW2*SW2)/EL2))/S2B))/(2D0*MW*SW))*( dZh0HHOSAlter()/2D0 ) + &
		&(-(EL*(CBA*(2D0*MA02 - MHH2) + (SAB*(2D0*MHH2 - (4D0*Lambda5*MW2*SW2)/EL2))/S2B))/(2D0*MW*SW))*(-EL*SBA*(MA02 &
		&- MHH2)/(2D0*MW*SW))*( dZG0A0OSAlter() ) + &
		&(-(EL*(CBA*(2D0*MA02 - MHH2) + (SAB*(2D0*MHH2 - (4D0*Lambda5*MW2*SW2)/EL2))/S2B))/(2D0*MW*SW))*( &
		& -4D0*MW*Lambda5*SAB*SW/(EL*S2B)*( dgAtMZ()/(EL/SW) - dMW2Alter()/(2D0*MW2) ) - &
		& EL/(2D0*MW*SW)*(&
		&	(2D0*SAB/S2B-CBA)*dMHH2OSAlter() + 2D0*CBA*dMA02OSAlter() - 4D0*MW2*SAB*SW2/(EL2*S2B)*Lambda5*(&
		&		2D0*dgAtMZ()/(EL/SW) - dMW2Alter()/MW2 - 2D0*C2B/S2B*dBetaBFMSAlter() + EL2*dm122MSBarAlter()/(SW2*MW2*&
		&			S2B*Lambda5)&
		&		) + &
		&	(SBA*(2D0*MA02-MHH2) + CAB/S2B*(2D0*MHH2 - 4D0*SW2*MW2*Lambda5/EL2))*dAlphaPinchOS() + &
		&	(-SBA*(2D0*MA02-MHH2) + (CAB/S2B - 2D0*SAB*C2B/(S2B**2))*(2D0*MHH2-4D0*MW2*SW2*Lambda5/EL2))*dBetaBFMSAlter()&
		&	)&
		& )
	case (16)
		totalAmplitude = (0.25D0*EL2*DBLE((CBA*(2.D0*MA02 - 1.D0*MHH2) + (SAB*(2.D0*MHH2 - (4.D0*Lambda5*MW2*SW2)/EL2))/S2B)&
		&**INT(2.D0)))/(MW2*SW2)*( dZA0A0OS() + dZHHHHOS()/2D0 + dgAtMZ()/(EL/SW) - dMW2Usual()/(2D0*MW2) ) + &
		&(-(EL*(CBA*(2D0*MA02 - MHH2) + (SAB*(2D0*MHH2 - (4D0*Lambda5*MW2*SW2)/EL2))/S2B))/(2D0*MW*SW))*(-(EL*(SBA*&
		&(2D0*MA02 - Mh02) + (CAB*(2D0*Mh02 - (4D0*Lambda5*MW2*SW2)/EL2))/S2B))/(2D0*MW*SW))*( dZh0HHOSUsual()/2D0 ) + &
		&(-(EL*(CBA*(2D0*MA02 - MHH2) + (SAB*(2D0*MHH2 - (4D0*Lambda5*MW2*SW2)/EL2))/S2B))/(2D0*MW*SW))*(-EL*SBA*(MA02 &
		&- MHH2)/(2D0*MW*SW))*( dZG0A0OSUsual() ) + &
		&(-(EL*(CBA*(2D0*MA02 - MHH2) + (SAB*(2D0*MHH2 - (4D0*Lambda5*MW2*SW2)/EL2))/S2B))/(2D0*MW*SW))*( &
		& -4D0*MW*Lambda5*SAB*SW/(EL*S2B)*( dgAtMZ()/(EL/SW) - dMW2Usual()/(2D0*MW2) ) - &
		& EL/(2D0*MW*SW)*(&
		&	(2D0*SAB/S2B-CBA)*dMHH2OSUsual() + 2D0*CBA*dMA02OSUsual() - 4D0*MW2*SAB*SW2/(EL2*S2B)*Lambda5*(&
		&		2D0*dgAtMZ()/(EL/SW) - dMW2Usual()/MW2 - 2D0*C2B/S2B*dBetaMSBarUsual() + EL2*dm122MSBarUsual()/(SW2*MW2*&
		&			S2B*Lambda5)&
		&		) + &
		&	(SBA*(2D0*MA02-MHH2) + CAB/S2B*(2D0*MHH2 - 4D0*SW2*MW2*Lambda5/EL2))*dAlphaMSBarUsual() + &
		&	(-SBA*(2D0*MA02-MHH2) + (CAB/S2B - 2D0*SAB*C2B/(S2B**2))*(2D0*MHH2-4D0*MW2*SW2*Lambda5/EL2))*dBetaMSBarUsual()&
		&	)&
		& )
	case (17)
		totalAmplitude = (0.25D0*EL2*DBLE((CBA*(2.D0*MA02 - 1.D0*MHH2) + (SAB*(2.D0*MHH2 - (4.D0*Lambda5*MW2*SW2)/EL2))/&
		&S2B)**INT(2.D0)))/(MW2*SW2)*( dZA0A0OS() + dZHHHHOS()/2D0 + dgAtMZ()/(EL/SW) - dMW2Alter()/(2D0*MW2) ) + &
		&(-(EL*(CBA*(2D0*MA02 - MHH2) + (SAB*(2D0*MHH2 - (4D0*Lambda5*MW2*SW2)/EL2))/S2B))/(2D0*MW*SW))*(-(EL*(SBA*&
		&(2D0*MA02 - Mh02) + (CAB*(2D0*Mh02 - (4D0*Lambda5*MW2*SW2)/EL2))/S2B))/(2D0*MW*SW))*( dZh0HHOSAlter()/2D0 ) + &
		&(-(EL*(CBA*(2D0*MA02 - MHH2) + (SAB*(2D0*MHH2 - (4D0*Lambda5*MW2*SW2)/EL2))/S2B))/(2D0*MW*SW))*(-EL*SBA*(MA02 &
		&- MHH2)/(2D0*MW*SW))*( dZG0A0OSAlter() ) + &
		&(-(EL*(CBA*(2D0*MA02 - MHH2) + (SAB*(2D0*MHH2 - (4D0*Lambda5*MW2*SW2)/EL2))/S2B))/(2D0*MW*SW))*( &
		& -4D0*MW*Lambda5*SAB*SW/(EL*S2B)*( dgAtMZ()/(EL/SW) - dMW2Alter()/(2D0*MW2) ) - &
		& EL/(2D0*MW*SW)*(&
		&	(2D0*SAB/S2B-CBA)*dMHH2OSAlter() + 2D0*CBA*dMA02OSAlter() - 4D0*MW2*SAB*SW2/(EL2*S2B)*Lambda5*(&
		&		2D0*dgAtMZ()/(EL/SW) - dMW2Alter()/MW2 - 2D0*C2B/S2B*dBetaMSBarAlter() + EL2*dm122MSBarAlter()/(SW2*MW2*&
		&			S2B*Lambda5)&
		&		) + &
		&	(SBA*(2D0*MA02-MHH2) + CAB/S2B*(2D0*MHH2 - 4D0*SW2*MW2*Lambda5/EL2))*dAlphaMSBarAlter() + &
		&	(-SBA*(2D0*MA02-MHH2) + (CAB/S2B - 2D0*SAB*C2B/(S2B**2))*(2D0*MHH2-4D0*MW2*SW2*Lambda5/EL2))*dBetaMSBarAlter()&
		&	)&
		& )
	case default
		totalAmplitude = 0D0
 end select

 HHtoA0A0CT = totalAmplitude
end function HHtoA0A0CT
