double precision function h0toTTBarCT(x)
 use constants
 use counterterms
 implicit none
#include "looptools.h"
 integer, intent(in) :: x
 double precision :: totalAmplitude
 double precision :: dcLeft, dcRight

 select case (x)
	case (1)
		totalAmplitude = ( dgAtMZ()/(EL/SW) + dMTOSUsual()/MT - dMW2Usual()/(2D0*MW2) - dBeta1KanUsual()/TB - &
			& dAlphaKanUsual()*TA + dZh0h0OS()/2D0 + SA/CA * dZHHh0OSUsual()/2D0 + dZTTOSLeft()/2D0 + &
			& dZTTOSRight()/2D0 )*(1.5D0*EL2*MT2*CA2*(Mh02 - 4.D0*MT2))/(MW2*SB2*SW2)
	case (2)
		totalAmplitude = ( dgAtMZ()/(EL/SW) + dMTOSUsual()/MT - dMW2Usual()/(2D0*MW2) - dBeta2KanUsual()/TB - &
			& dAlphaKanUsual()*TA + dZh0h0OS()/2D0 + SA/CA * dZHHh0OSUsual()/2D0 + dZTTOSLeft()/2D0 + &
			& dZTTOSRight()/2D0 )*(1.5D0*EL2*MT2*CA2*(Mh02 - 4.D0*MT2))/(MW2*SB2*SW2)
	case (3)
		totalAmplitude = ( dgAtMZ()/(EL/SW) + dMTOSAlter()/MT - dMW2Alter()/(2D0*MW2) - dBeta1KanAlter()/TB - &
			& dAlphaKanAlter()*TA + dZh0h0OS()/2D0 + SA/CA * dZHHh0OSAlter()/2D0 + dZTTOSLeft()/2D0 + &
			& dZTTOSRight()/2D0 )*(1.5D0*EL2*MT2*CA2*(Mh02 - 4.D0*MT2))/(MW2*SB2*SW2)	
	case (4)
		totalAmplitude = ( dgAtMZ()/(EL/SW) + dMTOSAlter()/MT - dMW2Alter()/(2D0*MW2) - dBeta2KanAlter()/TB - &
			& dAlphaKanAlter()*TA + dZh0h0OS()/2D0 + SA/CA * dZHHh0OSAlter()/2D0 + dZTTOSLeft()/2D0 + &
			& dZTTOSRight()/2D0 )*(1.5D0*EL2*MT2*CA2*(Mh02 - 4.D0*MT2))/(MW2*SB2*SW2)
	case (5)
		totalAmplitude = ( dgAtMZ()/(EL/SW) + dMTOSAlter()/MT - dMW2Alter()/(2D0*MW2) - dBeta1PinchPStar()/TB - &
			& dAlphaPinchPStar()*TA + dZh0h0OS()/2D0 + SA/CA * dZHHh0OSAlter()/2D0 + dZTTOSLeft()/2D0 + &
			& dZTTOSRight()/2D0 )*(1.5D0*EL2*MT2*CA2*(Mh02 - 4.D0*MT2))/(MW2*SB2*SW2)
	case (6)
		totalAmplitude = ( dgAtMZ()/(EL/SW) + dMTOSAlter()/MT - dMW2Alter()/(2D0*MW2) - dBeta2PinchPStar()/TB - &
			& dAlphaPinchPStar()*TA + dZh0h0OS()/2D0 + SA/CA * dZHHh0OSAlter()/2D0 + dZTTOSLeft()/2D0 + &
			& dZTTOSRight()/2D0 )*(1.5D0*EL2*MT2*CA2*(Mh02 - 4.D0*MT2))/(MW2*SB2*SW2)
	case (7)
		totalAmplitude = ( dgAtMZ()/(EL/SW) + dMTOSAlter()/MT - dMW2Alter()/(2D0*MW2) - dBeta1PinchOS()/TB - &
			& dAlphaPinchOS()*TA + dZh0h0OS()/2D0 + SA/CA * dZHHh0OSAlter()/2D0 + dZTTOSLeft()/2D0 + &
			& dZTTOSRight()/2D0 )*(1.5D0*EL2*MT2*CA2*(Mh02 - 4.D0*MT2))/(MW2*SB2*SW2)
	case (8)
		totalAmplitude = ( dgAtMZ()/(EL/SW) + dMTOSAlter()/MT - dMW2Alter()/(2D0*MW2) - dBeta2PinchOS()/TB - &
			& dAlphaPinchOS()*TA + dZh0h0OS()/2D0 + SA/CA * dZHHh0OSAlter()/2D0 + dZTTOSLeft()/2D0 + &
			& dZTTOSRight()/2D0 )*(1.5D0*EL2*MT2*CA2*(Mh02 - 4.D0*MT2))/(MW2*SB2*SW2)
	case (9)
		totalAmplitude = ( dgAtMZ()/(EL/SW) + dMTOSAlter()/MT - dMW2Alter()/(2D0*MW2) - dBetaProcDep1Alter()/TB - &
			& dAlphaProcDep1Alter()*TA + dZh0h0OS()/2D0 + SA/CA * dZHHh0OSAlter()/2D0 + dZTTOSLeft()/2D0 + &
			& dZTTOSRight()/2D0 )*(1.5D0*EL2*MT2*CA2*(Mh02 - 4.D0*MT2))/(MW2*SB2*SW2)
	case (10)
		totalAmplitude = ( dgAtMZ()/(EL/SW) + dMTOSAlter()/MT - dMW2Alter()/(2D0*MW2) - dBetaProcDep2Alter()/TB - &
			& dAlphaProcDep2Alter()*TA + dZh0h0OS()/2D0 + SA/CA * dZHHh0OSAlter()/2D0 + dZTTOSLeft()/2D0 + &
			& dZTTOSRight()/2D0 )*(1.5D0*EL2*MT2*CA2*(Mh02 - 4.D0*MT2))/(MW2*SB2*SW2)
	case (11)
		totalAmplitude = ( dgAtMZ()/(EL/SW) + dMTOSAlter()/MT - dMW2Alter()/(2D0*MW2) - dBetaProcDep3Alter()/TB - &
			& dAlphaProcDep3Alter()*TA + dZh0h0OS()/2D0 + SA/CA * dZHHh0OSAlter()/2D0 + dZTTOSLeft()/2D0 + &
			& dZTTOSRight()/2D0 )*(1.5D0*EL2*MT2*CA2*(Mh02 - 4.D0*MT2))/(MW2*SB2*SW2)
	case (12)
		totalAmplitude = ( dgAtMZ()/(EL/SW) + dMTOSAlter()/MT - dMW2Alter()/(2D0*MW2) - dBetaOS1Alter()/TB - &
			& dAlphaOS1Alter()*TA + dZh0h0OS()/2D0 + SA/CA * dZHHh0OSAlter()/2D0 + dZTTOSLeft()/2D0 + &
			& dZTTOSRight()/2D0 )*(1.5D0*EL2*MT2*CA2*(Mh02 - 4.D0*MT2))/(MW2*SB2*SW2)
	case (13)
		totalAmplitude = ( dgAtMZ()/(EL/SW) + dMTOSAlter()/MT - dMW2Alter()/(2D0*MW2) - dBetaOS2Alter()/TB - &
			& dAlphaOS2Alter()*TA + dZh0h0OS()/2D0 + SA/CA * dZHHh0OSAlter()/2D0 + dZTTOSLeft()/2D0 + &
			& dZTTOSRight()/2D0 )*(1.5D0*EL2*MT2*CA2*(Mh02 - 4.D0*MT2))/(MW2*SB2*SW2)
	case (14)
		totalAmplitude = ( dgAtMZ()/(EL/SW) + dMTOSAlter()/MT - dMW2Alter()/(2D0*MW2) - dBetaOS12Alter()/TB - &
			& dAlphaOS12Alter()*TA + dZh0h0OS()/2D0 + SA/CA * dZHHh0OSAlter()/2D0 + dZTTOSLeft()/2D0 + &
			& dZTTOSRight()/2D0 )*(1.5D0*EL2*MT2*CA2*(Mh02 - 4.D0*MT2))/(MW2*SB2*SW2)
	case (15)
		totalAmplitude = ( dgAtMZ()/(EL/SW) + dMTOSAlter()/MT - dMW2Alter()/(2D0*MW2) - dBetaBFMSAlter()/TB - &
			& dAlphaPinchOS()*TA + dZh0h0OS()/2D0 + SA/CA * dZHHh0OSAlter()/2D0 + dZTTOSLeft()/2D0 + &
			& dZTTOSRight()/2D0 )*(1.5D0*EL2*MT2*CA2*(Mh02 - 4.D0*MT2))/(MW2*SB2*SW2)
	case (16)
		totalAmplitude = ( dgAtMZ()/(EL/SW) + dMTOSUsual()/MT - dMW2Usual()/(2D0*MW2) - dBetaMSBarUsual()/TB - &
			& dAlphaMSBarUsual()*TA + dZh0h0OS()/2D0 + SA/CA * dZHHh0OSUsual()/2D0 + dZTTOSLeft()/2D0 + &
			& dZTTOSRight()/2D0 )*(1.5D0*EL2*MT2*CA2*(Mh02 - 4.D0*MT2))/(MW2*SB2*SW2)
	case (17)
		totalAmplitude = ( dgAtMZ()/(EL/SW) + dMTOSAlter()/MT - dMW2Alter()/(2D0*MW2) - dBetaMSBarAlter()/TB - &
			& dAlphaMSBarAlter()*TA + dZh0h0OS()/2D0 + SA/CA * dZHHh0OSAlter()/2D0 + dZTTOSLeft()/2D0 + &
			& dZTTOSRight()/2D0 )*(1.5D0*EL2*MT2*CA2*(Mh02 - 4.D0*MT2))/(MW2*SB2*SW2)
	case default
		totalAmplitude = 0D0
 end select

 h0toTTBarCT = totalAmplitude
end function h0toTTBarCT
