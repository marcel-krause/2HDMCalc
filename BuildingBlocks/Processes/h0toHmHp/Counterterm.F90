double precision function h0toHmHpCT(x)
 use constants
 use counterterms
 implicit none
#include "looptools.h"
 integer, intent(in) :: x
 double precision :: totalAmplitude

 select case (x)
	case (1)
		totalAmplitude = (0.25D0*EL2*DBLE(((Mh02 - 2.D0*MHp2)*SBA - (1.D0*CAB*(2.D0*Mh02 - (4.D0*Lambda5*MW2*SW2)&
		&/EL2))/S2B)**INT(2.D0)))/(MW2*SW2)*( dZh0h0OS()/2D0 + dZHpHpOS() + dgAtMZ()/(EL/SW) - dMW2Usual()/(2D0*MW2) ) + &
		&(-(EL*(-((Mh02 - 2*MHp2)*SBA) + (CAB*(2*Mh02 - (4*Lambda5*MW2*SW2)/EL2))/S2B))/(2D0*MW*SW))*(-EL/(2D0*MW*SW))*(&
			& SBA*(2D0*dMHp2OSUsual() - dMh02OSUsual() ) + CBA*(dBeta1KanUsual() - dAlphaKanUsual())*(2D0*MHp2 - Mh02) + &
			& CAB/S2B*(2D0*dMh02OSUsual() - 4D0*dm122MSBarUsual()/S2B + 8D0*Lambda5*MW2*SW2*C2B*dBeta1KanUsual()/(S2B*EL2)) +&
			& (- SAB/S2B*(dAlphaKanUsual() + dBeta1KanUsual()) - 2D0*CAB*C2B/S2B**2*dBeta1KanUsual())*(2D0*Mh02 - &
			& 4D0*Lambda5*MW2*SW2/EL2) &
		&) + (-(EL*(-((Mh02 - 2*MHp2)*SBA) + (CAB*(2*Mh02 - (4*Lambda5*MW2*SW2)/EL2))/S2B))/(2D0*MW*SW))*(&
			&(-EL*CBA/(2D0*MW*SW))*(Mh02 - MHp2)*dZGpHpOSUsual() + &
			&(-EL/(2D0*MW*SW)*(-CBA*(MHH2 - 2D0*MHp2) + SAB/S2B*(2D0*MHH2 - 4D0*Lambda5*MW2*SW2/EL2)))*dZHHh0OSUsual()/2D0 &
		&)
	case (2)
		totalAmplitude = (0.25D0*EL2*DBLE(((Mh02 - 2.D0*MHp2)*SBA - (1.D0*CAB*(2.D0*Mh02 - (4.D0*Lambda5*MW2*SW2)&
		&/EL2))/S2B)**INT(2.D0)))/(MW2*SW2)*( dZh0h0OS()/2D0 + dZHpHpOS() + dgAtMZ()/(EL/SW) - dMW2Usual()/(2D0*MW2) ) + &
		&(-(EL*(-((Mh02 - 2*MHp2)*SBA) + (CAB*(2*Mh02 - (4*Lambda5*MW2*SW2)/EL2))/S2B))/(2D0*MW*SW))*(-EL/(2D0*MW*SW))*(&
			& SBA*(2D0*dMHp2OSUsual() - dMh02OSUsual() ) + CBA*(dBeta2KanUsual() - dAlphaKanUsual())*(2D0*MHp2 - Mh02) + &
			& CAB/S2B*(2D0*dMh02OSUsual() - 4D0*dm122MSBarUsual()/S2B + 8D0*Lambda5*MW2*SW2*C2B*dBeta2KanUsual()/(S2B*EL2)) +&
			& (- SAB/S2B*(dAlphaKanUsual() + dBeta2KanUsual()) - 2D0*CAB*C2B/S2B**2*dBeta2KanUsual())*(2D0*Mh02 - &
			& 4D0*Lambda5*MW2*SW2/EL2) &
		&) + (-(EL*(-((Mh02 - 2*MHp2)*SBA) + (CAB*(2*Mh02 - (4*Lambda5*MW2*SW2)/EL2))/S2B))/(2D0*MW*SW))*(&
			&(-EL*CBA/(2D0*MW*SW))*(Mh02 - MHp2)*dZGpHpOSUsual() + &
			&(-EL/(2D0*MW*SW)*(-CBA*(MHH2 - 2D0*MHp2) + SAB/S2B*(2D0*MHH2 - 4D0*Lambda5*MW2*SW2/EL2)))*dZHHh0OSUsual()/2D0 &
		&)
	case (3)
		totalAmplitude = (0.25D0*EL2*DBLE(((Mh02 - 2.D0*MHp2)*SBA - (1.D0*CAB*(2.D0*Mh02 - (4.D0*Lambda5*MW2*SW2)&
		&/EL2))/S2B)**INT(2.D0)))/(MW2*SW2)*( dZh0h0OS()/2D0 + dZHpHpOS() + dgAtMZ()/(EL/SW) - dMW2Alter()/(2D0*MW2) ) + &
		&(-(EL*(-((Mh02 - 2*MHp2)*SBA) + (CAB*(2*Mh02 - (4*Lambda5*MW2*SW2)/EL2))/S2B))/(2D0*MW*SW))*(-EL/(2D0*MW*SW))*(&
			& SBA*(2D0*dMHp2OSAlter() - dMh02OSAlter() ) + CBA*(dBeta1KanAlter() - dAlphaKanAlter())*(2D0*MHp2 - Mh02) + &
			& CAB/S2B*(2D0*dMh02OSAlter() - 4D0*dm122MSBarAlter()/S2B + 8D0*Lambda5*MW2*SW2*C2B*dBeta1KanAlter()/(S2B*EL2)) +&
			& (- SAB/S2B*(dAlphaKanAlter() + dBeta1KanAlter()) - 2D0*CAB*C2B/S2B**2*dBeta1KanAlter())*(2D0*Mh02 - &
			& 4D0*Lambda5*MW2*SW2/EL2) &
		&) + (-(EL*(-((Mh02 - 2*MHp2)*SBA) + (CAB*(2*Mh02 - (4*Lambda5*MW2*SW2)/EL2))/S2B))/(2D0*MW*SW))*(&
			&(-EL*CBA/(2D0*MW*SW))*(Mh02 - MHp2)*dZGpHpOSAlter() + &
			&(-EL/(2D0*MW*SW)*(-CBA*(MHH2 - 2D0*MHp2) + SAB/S2B*(2D0*MHH2 - 4D0*Lambda5*MW2*SW2/EL2)))*dZHHh0OSAlter()/2D0 &
		&)
	case (4)
		totalAmplitude = (0.25D0*EL2*DBLE(((Mh02 - 2.D0*MHp2)*SBA - (1.D0*CAB*(2.D0*Mh02 - (4.D0*Lambda5*MW2*SW2)&
		&/EL2))/S2B)**INT(2.D0)))/(MW2*SW2)*( dZh0h0OS()/2D0 + dZHpHpOS() + dgAtMZ()/(EL/SW) - dMW2Alter()/(2D0*MW2) ) + &
		&(-(EL*(-((Mh02 - 2*MHp2)*SBA) + (CAB*(2*Mh02 - (4*Lambda5*MW2*SW2)/EL2))/S2B))/(2D0*MW*SW))*(-EL/(2D0*MW*SW))*(&
			& SBA*(2D0*dMHp2OSAlter() - dMh02OSAlter() ) + CBA*(dBeta2KanAlter() - dAlphaKanAlter())*(2D0*MHp2 - Mh02) + &
			& CAB/S2B*(2D0*dMh02OSAlter() - 4D0*dm122MSBarAlter()/S2B + 8D0*Lambda5*MW2*SW2*C2B*dBeta2KanAlter()/(S2B*EL2)) +&
			& (- SAB/S2B*(dAlphaKanAlter() + dBeta2KanAlter()) - 2D0*CAB*C2B/S2B**2*dBeta2KanAlter())*(2D0*Mh02 - &
			& 4D0*Lambda5*MW2*SW2/EL2) &
		&) + (-(EL*(-((Mh02 - 2*MHp2)*SBA) + (CAB*(2*Mh02 - (4*Lambda5*MW2*SW2)/EL2))/S2B))/(2D0*MW*SW))*(&
			&(-EL*CBA/(2D0*MW*SW))*(Mh02 - MHp2)*dZGpHpOSAlter() + &
			&(-EL/(2D0*MW*SW)*(-CBA*(MHH2 - 2D0*MHp2) + SAB/S2B*(2D0*MHH2 - 4D0*Lambda5*MW2*SW2/EL2)))*dZHHh0OSAlter()/2D0 &
		&)
	case (5)
		totalAmplitude = (0.25D0*EL2*DBLE(((Mh02 - 2.D0*MHp2)*SBA - (1.D0*CAB*(2.D0*Mh02 - (4.D0*Lambda5*MW2*SW2)&
		&/EL2))/S2B)**INT(2.D0)))/(MW2*SW2)*( dZh0h0OS()/2D0 + dZHpHpOS() + dgAtMZ()/(EL/SW) - dMW2Alter()/(2D0*MW2) ) + &
		&(-(EL*(-((Mh02 - 2*MHp2)*SBA) + (CAB*(2*Mh02 - (4*Lambda5*MW2*SW2)/EL2))/S2B))/(2D0*MW*SW))*(-EL/(2D0*MW*SW))*(&
			& SBA*(2D0*dMHp2OSAlter() - dMh02OSAlter() ) + CBA*(dBeta1PinchPStar() - dAlphaPinchPStar())*(2D0*MHp2 - Mh02) + &
			& CAB/S2B*(2D0*dMh02OSAlter() - 4D0*dm122MSBarAlter()/S2B + 8D0*Lambda5*MW2*SW2*C2B*dBeta1PinchPStar()/(S2B*EL2)) +&
			& (- SAB/S2B*(dAlphaPinchPStar() + dBeta1PinchPStar()) - 2D0*CAB*C2B/S2B**2*dBeta1PinchPStar())*(2D0*Mh02 - &
			& 4D0*Lambda5*MW2*SW2/EL2) &
		&) + (-(EL*(-((Mh02 - 2*MHp2)*SBA) + (CAB*(2*Mh02 - (4*Lambda5*MW2*SW2)/EL2))/S2B))/(2D0*MW*SW))*(&
			&(-EL*CBA/(2D0*MW*SW))*(Mh02 - MHp2)*dZGpHpOSAlter() + &
			&(-EL/(2D0*MW*SW)*(-CBA*(MHH2 - 2D0*MHp2) + SAB/S2B*(2D0*MHH2 - 4D0*Lambda5*MW2*SW2/EL2)))*dZHHh0OSAlter()/2D0 &
		&)
	case (6)
		totalAmplitude = (0.25D0*EL2*DBLE(((Mh02 - 2.D0*MHp2)*SBA - (1.D0*CAB*(2.D0*Mh02 - (4.D0*Lambda5*MW2*SW2)&
		&/EL2))/S2B)**INT(2.D0)))/(MW2*SW2)*( dZh0h0OS()/2D0 + dZHpHpOS() + dgAtMZ()/(EL/SW) - dMW2Alter()/(2D0*MW2) ) + &
		&(-(EL*(-((Mh02 - 2*MHp2)*SBA) + (CAB*(2*Mh02 - (4*Lambda5*MW2*SW2)/EL2))/S2B))/(2D0*MW*SW))*(-EL/(2D0*MW*SW))*(&
			& SBA*(2D0*dMHp2OSAlter() - dMh02OSAlter() ) + CBA*(dBeta2PinchPStar() - dAlphaPinchPStar())*(2D0*MHp2 - Mh02) + &
			& CAB/S2B*(2D0*dMh02OSAlter() - 4D0*dm122MSBarAlter()/S2B + 8D0*Lambda5*MW2*SW2*C2B*dBeta2PinchPStar()/(S2B*EL2)) +&
			& (- SAB/S2B*(dAlphaPinchPStar() + dBeta2PinchPStar()) - 2D0*CAB*C2B/S2B**2*dBeta2PinchPStar())*(2D0*Mh02 - &
			& 4D0*Lambda5*MW2*SW2/EL2) &
		&) + (-(EL*(-((Mh02 - 2*MHp2)*SBA) + (CAB*(2*Mh02 - (4*Lambda5*MW2*SW2)/EL2))/S2B))/(2D0*MW*SW))*(&
			&(-EL*CBA/(2D0*MW*SW))*(Mh02 - MHp2)*dZGpHpOSAlter() + &
			&(-EL/(2D0*MW*SW)*(-CBA*(MHH2 - 2D0*MHp2) + SAB/S2B*(2D0*MHH2 - 4D0*Lambda5*MW2*SW2/EL2)))*dZHHh0OSAlter()/2D0 &
		&)
	case (7)
		totalAmplitude = (0.25D0*EL2*DBLE(((Mh02 - 2.D0*MHp2)*SBA - (1.D0*CAB*(2.D0*Mh02 - (4.D0*Lambda5*MW2*SW2)&
		&/EL2))/S2B)**INT(2.D0)))/(MW2*SW2)*( dZh0h0OS()/2D0 + dZHpHpOS() + dgAtMZ()/(EL/SW) - dMW2Alter()/(2D0*MW2) ) + &
		&(-(EL*(-((Mh02 - 2*MHp2)*SBA) + (CAB*(2*Mh02 - (4*Lambda5*MW2*SW2)/EL2))/S2B))/(2D0*MW*SW))*(-EL/(2D0*MW*SW))*(&
			& SBA*(2D0*dMHp2OSAlter() - dMh02OSAlter() ) + CBA*(dBeta1PinchOS() - dAlphaPinchOS())*(2D0*MHp2 - Mh02) + &
			& CAB/S2B*(2D0*dMh02OSAlter() - 4D0*dm122MSBarAlter()/S2B + 8D0*Lambda5*MW2*SW2*C2B*dBeta1PinchOS()/(S2B*EL2)) +&
			& (- SAB/S2B*(dAlphaPinchOS() + dBeta1PinchOS()) - 2D0*CAB*C2B/S2B**2*dBeta1PinchOS())*(2D0*Mh02 - &
			& 4D0*Lambda5*MW2*SW2/EL2) &
		&) + (-(EL*(-((Mh02 - 2*MHp2)*SBA) + (CAB*(2*Mh02 - (4*Lambda5*MW2*SW2)/EL2))/S2B))/(2D0*MW*SW))*(&
			&(-EL*CBA/(2D0*MW*SW))*(Mh02 - MHp2)*dZGpHpOSAlter() + &
			&(-EL/(2D0*MW*SW)*(-CBA*(MHH2 - 2D0*MHp2) + SAB/S2B*(2D0*MHH2 - 4D0*Lambda5*MW2*SW2/EL2)))*dZHHh0OSAlter()/2D0 &
		&)
	case (8)
		totalAmplitude = (0.25D0*EL2*DBLE(((Mh02 - 2.D0*MHp2)*SBA - (1.D0*CAB*(2.D0*Mh02 - (4.D0*Lambda5*MW2*SW2)&
		&/EL2))/S2B)**INT(2.D0)))/(MW2*SW2)*( dZh0h0OS()/2D0 + dZHpHpOS() + dgAtMZ()/(EL/SW) - dMW2Alter()/(2D0*MW2) ) + &
		&(-(EL*(-((Mh02 - 2*MHp2)*SBA) + (CAB*(2*Mh02 - (4*Lambda5*MW2*SW2)/EL2))/S2B))/(2D0*MW*SW))*(-EL/(2D0*MW*SW))*(&
			& SBA*(2D0*dMHp2OSAlter() - dMh02OSAlter() ) + CBA*(dBeta2PinchOS() - dAlphaPinchOS())*(2D0*MHp2 - Mh02) + &
			& CAB/S2B*(2D0*dMh02OSAlter() - 4D0*dm122MSBarAlter()/S2B + 8D0*Lambda5*MW2*SW2*C2B*dBeta2PinchOS()/(S2B*EL2)) +&
			& (- SAB/S2B*(dAlphaPinchOS() + dBeta2PinchOS()) - 2D0*CAB*C2B/S2B**2*dBeta2PinchOS())*(2D0*Mh02 - &
			& 4D0*Lambda5*MW2*SW2/EL2) &
		&) + (-(EL*(-((Mh02 - 2*MHp2)*SBA) + (CAB*(2*Mh02 - (4*Lambda5*MW2*SW2)/EL2))/S2B))/(2D0*MW*SW))*(&
			&(-EL*CBA/(2D0*MW*SW))*(Mh02 - MHp2)*dZGpHpOSAlter() + &
			&(-EL/(2D0*MW*SW)*(-CBA*(MHH2 - 2D0*MHp2) + SAB/S2B*(2D0*MHH2 - 4D0*Lambda5*MW2*SW2/EL2)))*dZHHh0OSAlter()/2D0 &
		&)
	case (9)
		totalAmplitude = (0.25D0*EL2*DBLE(((Mh02 - 2.D0*MHp2)*SBA - (1.D0*CAB*(2.D0*Mh02 - (4.D0*Lambda5*MW2*SW2)&
		&/EL2))/S2B)**INT(2.D0)))/(MW2*SW2)*( dZh0h0OS()/2D0 + dZHpHpOS() + dgAtMZ()/(EL/SW) - dMW2Alter()/(2D0*MW2) ) + &
		&(-(EL*(-((Mh02 - 2*MHp2)*SBA) + (CAB*(2*Mh02 - (4*Lambda5*MW2*SW2)/EL2))/S2B))/(2D0*MW*SW))*(-EL/(2D0*MW*SW))*(&
			& SBA*(2D0*dMHp2OSAlter() - dMh02OSAlter() ) + CBA*(dBetaProcDep1Alter() - dAlphaProcDep1Alter())*(2D0*MHp2 - Mh02) +&
			& CAB/S2B*(2D0*dMh02OSAlter() - 4D0*dm122MSBarAlter()/S2B + 8D0*Lambda5*MW2*SW2*C2B*dBetaProcDep1Alter()/(S2B*EL2)) +&
			& (- SAB/S2B*(dAlphaProcDep1Alter() + dBetaProcDep1Alter()) - 2D0*CAB*C2B/S2B**2*dBetaProcDep1Alter())*(2D0*Mh02 - &
			& 4D0*Lambda5*MW2*SW2/EL2) &
		&) + (-(EL*(-((Mh02 - 2*MHp2)*SBA) + (CAB*(2*Mh02 - (4*Lambda5*MW2*SW2)/EL2))/S2B))/(2D0*MW*SW))*(&
			&(-EL*CBA/(2D0*MW*SW))*(Mh02 - MHp2)*dZGpHpOSAlter() + &
			&(-EL/(2D0*MW*SW)*(-CBA*(MHH2 - 2D0*MHp2) + SAB/S2B*(2D0*MHH2 - 4D0*Lambda5*MW2*SW2/EL2)))*dZHHh0OSAlter()/2D0 &
		&)
	case (10)
		totalAmplitude = (0.25D0*EL2*DBLE(((Mh02 - 2.D0*MHp2)*SBA - (1.D0*CAB*(2.D0*Mh02 - (4.D0*Lambda5*MW2*SW2)&
		&/EL2))/S2B)**INT(2.D0)))/(MW2*SW2)*( dZh0h0OS()/2D0 + dZHpHpOS() + dgAtMZ()/(EL/SW) - dMW2Alter()/(2D0*MW2) ) + &
		&(-(EL*(-((Mh02 - 2*MHp2)*SBA) + (CAB*(2*Mh02 - (4*Lambda5*MW2*SW2)/EL2))/S2B))/(2D0*MW*SW))*(-EL/(2D0*MW*SW))*(&
			& SBA*(2D0*dMHp2OSAlter() - dMh02OSAlter() ) + CBA*(dBetaProcDep2Alter() - dAlphaProcDep2Alter())*(2D0*MHp2 - Mh02) +&
			& CAB/S2B*(2D0*dMh02OSAlter() - 4D0*dm122MSBarAlter()/S2B + 8D0*Lambda5*MW2*SW2*C2B*dBetaProcDep2Alter()/(S2B*EL2)) +&
			& (- SAB/S2B*(dAlphaProcDep2Alter() + dBetaProcDep2Alter()) - 2D0*CAB*C2B/S2B**2*dBetaProcDep2Alter())*(2D0*Mh02 - &
			& 4D0*Lambda5*MW2*SW2/EL2) &
		&) + (-(EL*(-((Mh02 - 2*MHp2)*SBA) + (CAB*(2*Mh02 - (4*Lambda5*MW2*SW2)/EL2))/S2B))/(2D0*MW*SW))*(&
			&(-EL*CBA/(2D0*MW*SW))*(Mh02 - MHp2)*dZGpHpOSAlter() + &
			&(-EL/(2D0*MW*SW)*(-CBA*(MHH2 - 2D0*MHp2) + SAB/S2B*(2D0*MHH2 - 4D0*Lambda5*MW2*SW2/EL2)))*dZHHh0OSAlter()/2D0 &
		&)
	case (11)
		totalAmplitude = (0.25D0*EL2*DBLE(((Mh02 - 2.D0*MHp2)*SBA - (1.D0*CAB*(2.D0*Mh02 - (4.D0*Lambda5*MW2*SW2)&
		&/EL2))/S2B)**INT(2.D0)))/(MW2*SW2)*( dZh0h0OS()/2D0 + dZHpHpOS() + dgAtMZ()/(EL/SW) - dMW2Alter()/(2D0*MW2) ) + &
		&(-(EL*(-((Mh02 - 2*MHp2)*SBA) + (CAB*(2*Mh02 - (4*Lambda5*MW2*SW2)/EL2))/S2B))/(2D0*MW*SW))*(-EL/(2D0*MW*SW))*(&
			& SBA*(2D0*dMHp2OSAlter() - dMh02OSAlter() ) + CBA*(dBetaProcDep3Alter() - dAlphaProcDep3Alter())*(2D0*MHp2 - Mh02) +&
			& CAB/S2B*(2D0*dMh02OSAlter() - 4D0*dm122MSBarAlter()/S2B + 8D0*Lambda5*MW2*SW2*C2B*dBetaProcDep3Alter()/(S2B*EL2)) +&
			& (- SAB/S2B*(dAlphaProcDep3Alter() + dBetaProcDep3Alter()) - 2D0*CAB*C2B/S2B**2*dBetaProcDep3Alter())*(2D0*Mh02 - &
			& 4D0*Lambda5*MW2*SW2/EL2) &
		&) + (-(EL*(-((Mh02 - 2*MHp2)*SBA) + (CAB*(2*Mh02 - (4*Lambda5*MW2*SW2)/EL2))/S2B))/(2D0*MW*SW))*(&
			&(-EL*CBA/(2D0*MW*SW))*(Mh02 - MHp2)*dZGpHpOSAlter() + &
			&(-EL/(2D0*MW*SW)*(-CBA*(MHH2 - 2D0*MHp2) + SAB/S2B*(2D0*MHH2 - 4D0*Lambda5*MW2*SW2/EL2)))*dZHHh0OSAlter()/2D0 &
		&)	
	case (12)
		totalAmplitude = (0.25D0*EL2*DBLE(((Mh02 - 2.D0*MHp2)*SBA - (1.D0*CAB*(2.D0*Mh02 - (4.D0*Lambda5*MW2*SW2)&
		&/EL2))/S2B)**INT(2.D0)))/(MW2*SW2)*( dZh0h0OS()/2D0 + dZHpHpOS() + dgAtMZ()/(EL/SW) - dMW2Alter()/(2D0*MW2) ) + &
		&(-(EL*(-((Mh02 - 2*MHp2)*SBA) + (CAB*(2*Mh02 - (4*Lambda5*MW2*SW2)/EL2))/S2B))/(2D0*MW*SW))*(-EL/(2D0*MW*SW))*(&
			& SBA*(2D0*dMHp2OSAlter() - dMh02OSAlter() ) + CBA*(dBetaOS1Alter() - dAlphaOS1Alter())*(2D0*MHp2 - Mh02) +&
			& CAB/S2B*(2D0*dMh02OSAlter() - 4D0*dm122MSBarAlter()/S2B + 8D0*Lambda5*MW2*SW2*C2B*dBetaOS1Alter()/(S2B*EL2)) +&
			& (- SAB/S2B*(dAlphaOS1Alter() + dBetaOS1Alter()) - 2D0*CAB*C2B/S2B**2*dBetaOS1Alter())*(2D0*Mh02 - &
			& 4D0*Lambda5*MW2*SW2/EL2) &
		&) + (-(EL*(-((Mh02 - 2*MHp2)*SBA) + (CAB*(2*Mh02 - (4*Lambda5*MW2*SW2)/EL2))/S2B))/(2D0*MW*SW))*(&
			&(-EL*CBA/(2D0*MW*SW))*(Mh02 - MHp2)*dZGpHpOSAlter() + &
			&(-EL/(2D0*MW*SW)*(-CBA*(MHH2 - 2D0*MHp2) + SAB/S2B*(2D0*MHH2 - 4D0*Lambda5*MW2*SW2/EL2)))*dZHHh0OSAlter()/2D0 &
		&)
	case (13)
		totalAmplitude = (0.25D0*EL2*DBLE(((Mh02 - 2.D0*MHp2)*SBA - (1.D0*CAB*(2.D0*Mh02 - (4.D0*Lambda5*MW2*SW2)&
		&/EL2))/S2B)**INT(2.D0)))/(MW2*SW2)*( dZh0h0OS()/2D0 + dZHpHpOS() + dgAtMZ()/(EL/SW) - dMW2Alter()/(2D0*MW2) ) + &
		&(-(EL*(-((Mh02 - 2*MHp2)*SBA) + (CAB*(2*Mh02 - (4*Lambda5*MW2*SW2)/EL2))/S2B))/(2D0*MW*SW))*(-EL/(2D0*MW*SW))*(&
			& SBA*(2D0*dMHp2OSAlter() - dMh02OSAlter() ) + CBA*(dBetaOS2Alter() - dAlphaOS2Alter())*(2D0*MHp2 - Mh02) +&
			& CAB/S2B*(2D0*dMh02OSAlter() - 4D0*dm122MSBarAlter()/S2B + 8D0*Lambda5*MW2*SW2*C2B*dBetaOS2Alter()/(S2B*EL2)) +&
			& (- SAB/S2B*(dAlphaOS2Alter() + dBetaOS2Alter()) - 2D0*CAB*C2B/S2B**2*dBetaOS2Alter())*(2D0*Mh02 - &
			& 4D0*Lambda5*MW2*SW2/EL2) &
		&) + (-(EL*(-((Mh02 - 2*MHp2)*SBA) + (CAB*(2*Mh02 - (4*Lambda5*MW2*SW2)/EL2))/S2B))/(2D0*MW*SW))*(&
			&(-EL*CBA/(2D0*MW*SW))*(Mh02 - MHp2)*dZGpHpOSAlter() + &
			&(-EL/(2D0*MW*SW)*(-CBA*(MHH2 - 2D0*MHp2) + SAB/S2B*(2D0*MHH2 - 4D0*Lambda5*MW2*SW2/EL2)))*dZHHh0OSAlter()/2D0 &
		&)
	case (14)
		totalAmplitude = (0.25D0*EL2*DBLE(((Mh02 - 2.D0*MHp2)*SBA - (1.D0*CAB*(2.D0*Mh02 - (4.D0*Lambda5*MW2*SW2)&
		&/EL2))/S2B)**INT(2.D0)))/(MW2*SW2)*( dZh0h0OS()/2D0 + dZHpHpOS() + dgAtMZ()/(EL/SW) - dMW2Alter()/(2D0*MW2) ) + &
		&(-(EL*(-((Mh02 - 2*MHp2)*SBA) + (CAB*(2*Mh02 - (4*Lambda5*MW2*SW2)/EL2))/S2B))/(2D0*MW*SW))*(-EL/(2D0*MW*SW))*(&
			& SBA*(2D0*dMHp2OSAlter() - dMh02OSAlter() ) + CBA*(dBetaOS12Alter() - dAlphaOS12Alter())*(2D0*MHp2 - Mh02) +&
			& CAB/S2B*(2D0*dMh02OSAlter() - 4D0*dm122MSBarAlter()/S2B + 8D0*Lambda5*MW2*SW2*C2B*dBetaOS12Alter()/(S2B*EL2)) +&
			& (- SAB/S2B*(dAlphaOS12Alter() + dBetaOS12Alter()) - 2D0*CAB*C2B/S2B**2*dBetaOS12Alter())*(2D0*Mh02 - &
			& 4D0*Lambda5*MW2*SW2/EL2) &
		&) + (-(EL*(-((Mh02 - 2*MHp2)*SBA) + (CAB*(2*Mh02 - (4*Lambda5*MW2*SW2)/EL2))/S2B))/(2D0*MW*SW))*(&
			&(-EL*CBA/(2D0*MW*SW))*(Mh02 - MHp2)*dZGpHpOSAlter() + &
			&(-EL/(2D0*MW*SW)*(-CBA*(MHH2 - 2D0*MHp2) + SAB/S2B*(2D0*MHH2 - 4D0*Lambda5*MW2*SW2/EL2)))*dZHHh0OSAlter()/2D0 &
		&)
	case (15)
		totalAmplitude = (0.25D0*EL2*DBLE(((Mh02 - 2.D0*MHp2)*SBA - (1.D0*CAB*(2.D0*Mh02 - (4.D0*Lambda5*MW2*SW2)&
		&/EL2))/S2B)**INT(2.D0)))/(MW2*SW2)*( dZh0h0OS()/2D0 + dZHpHpOS() + dgAtMZ()/(EL/SW) - dMW2Alter()/(2D0*MW2) ) + &
		&(-(EL*(-((Mh02 - 2*MHp2)*SBA) + (CAB*(2*Mh02 - (4*Lambda5*MW2*SW2)/EL2))/S2B))/(2D0*MW*SW))*(-EL/(2D0*MW*SW))*(&
			& SBA*(2D0*dMHp2OSAlter() - dMh02OSAlter() ) + CBA*(dBetaBFMSAlter() - dAlphaPinchOS())*(2D0*MHp2 - Mh02) +&
			& CAB/S2B*(2D0*dMh02OSAlter() - 4D0*dm122MSBarAlter()/S2B + 8D0*Lambda5*MW2*SW2*C2B*dBetaBFMSAlter()/(S2B*EL2)) +&
			& (- SAB/S2B*(dAlphaPinchOS() + dBetaBFMSAlter()) - 2D0*CAB*C2B/S2B**2*dBetaBFMSAlter())*(2D0*Mh02 - &
			& 4D0*Lambda5*MW2*SW2/EL2) &
		&) + (-(EL*(-((Mh02 - 2*MHp2)*SBA) + (CAB*(2*Mh02 - (4*Lambda5*MW2*SW2)/EL2))/S2B))/(2D0*MW*SW))*(&
			&(-EL*CBA/(2D0*MW*SW))*(Mh02 - MHp2)*dZGpHpOSAlter() + &
			&(-EL/(2D0*MW*SW)*(-CBA*(MHH2 - 2D0*MHp2) + SAB/S2B*(2D0*MHH2 - 4D0*Lambda5*MW2*SW2/EL2)))*dZHHh0OSAlter()/2D0 &
		&)
	case (16)
		totalAmplitude = (0.25D0*EL2*DBLE(((Mh02 - 2.D0*MHp2)*SBA - (1.D0*CAB*(2.D0*Mh02 - (4.D0*Lambda5*MW2*SW2)&
		&/EL2))/S2B)**INT(2.D0)))/(MW2*SW2)*( dZh0h0OS()/2D0 + dZHpHpOS() + dgAtMZ()/(EL/SW) - dMW2Usual()/(2D0*MW2) ) + &
		&(-(EL*(-((Mh02 - 2*MHp2)*SBA) + (CAB*(2*Mh02 - (4*Lambda5*MW2*SW2)/EL2))/S2B))/(2D0*MW*SW))*(-EL/(2D0*MW*SW))*(&
			& SBA*(2D0*dMHp2OSUsual() - dMh02OSUsual() ) + CBA*(dBetaMSBarUsual() - dAlphaMSBarUsual())*(2D0*MHp2 - Mh02) + &
			& CAB/S2B*(2D0*dMh02OSUsual() - 4D0*dm122MSBarUsual()/S2B + 8D0*Lambda5*MW2*SW2*C2B*dBetaMSBarUsual()/(S2B*EL2)) +&
			& (- SAB/S2B*(dAlphaMSBarUsual() + dBetaMSBarUsual()) - 2D0*CAB*C2B/S2B**2*dBetaMSBarUsual())*(2D0*Mh02 - &
			& 4D0*Lambda5*MW2*SW2/EL2) &
		&) + (-(EL*(-((Mh02 - 2*MHp2)*SBA) + (CAB*(2*Mh02 - (4*Lambda5*MW2*SW2)/EL2))/S2B))/(2D0*MW*SW))*(&
			&(-EL*CBA/(2D0*MW*SW))*(Mh02 - MHp2)*dZGpHpOSUsual() + &
			&(-EL/(2D0*MW*SW)*(-CBA*(MHH2 - 2D0*MHp2) + SAB/S2B*(2D0*MHH2 - 4D0*Lambda5*MW2*SW2/EL2)))*dZHHh0OSUsual()/2D0 &
		&)
	case (17)
		totalAmplitude = (0.25D0*EL2*DBLE(((Mh02 - 2.D0*MHp2)*SBA - (1.D0*CAB*(2.D0*Mh02 - (4.D0*Lambda5*MW2*SW2)&
		&/EL2))/S2B)**INT(2.D0)))/(MW2*SW2)*( dZh0h0OS()/2D0 + dZHpHpOS() + dgAtMZ()/(EL/SW) - dMW2Alter()/(2D0*MW2) ) + &
		&(-(EL*(-((Mh02 - 2*MHp2)*SBA) + (CAB*(2*Mh02 - (4*Lambda5*MW2*SW2)/EL2))/S2B))/(2D0*MW*SW))*(-EL/(2D0*MW*SW))*(&
			& SBA*(2D0*dMHp2OSAlter() - dMh02OSAlter() ) + CBA*(dBetaMSBarAlter() - dAlphaMSBarAlter())*(2D0*MHp2 - Mh02) +&
			& CAB/S2B*(2D0*dMh02OSAlter() - 4D0*dm122MSBarAlter()/S2B + 8D0*Lambda5*MW2*SW2*C2B*dBetaMSBarAlter()/(S2B*EL2)) +&
			& (- SAB/S2B*(dAlphaMSBarAlter() + dBetaMSBarAlter()) - 2D0*CAB*C2B/S2B**2*dBetaMSBarAlter())*(2D0*Mh02 - &
			& 4D0*Lambda5*MW2*SW2/EL2) &
		&) + (-(EL*(-((Mh02 - 2*MHp2)*SBA) + (CAB*(2*Mh02 - (4*Lambda5*MW2*SW2)/EL2))/S2B))/(2D0*MW*SW))*(&
			&(-EL*CBA/(2D0*MW*SW))*(Mh02 - MHp2)*dZGpHpOSAlter() + &
			&(-EL/(2D0*MW*SW)*(-CBA*(MHH2 - 2D0*MHp2) + SAB/S2B*(2D0*MHH2 - 4D0*Lambda5*MW2*SW2/EL2)))*dZHHh0OSAlter()/2D0 &
		&)
	case default
		totalAmplitude = 0D0
 end select

 h0toHmHpCT = totalAmplitude
end function h0toHmHpCT
