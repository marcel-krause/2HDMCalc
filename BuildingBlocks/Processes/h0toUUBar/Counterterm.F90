double precision function h0toUUBarCT(x)
 use constants
 use counterterms
 implicit none
#include "looptools.h"
 integer, intent(in) :: x
 double precision :: totalAmplitude

 select case (x)
	case (1)
		totalAmplitude = ( dgAtMZ()/(EL/SW) + dMUOSUsual()/MU - dMW2Usual()/(2D0*MW2) - 1D0/TB*dBeta1KanUsual() - &
			& TA*dAlphaKanUsual() + dZh0h0OS()/2D0 + TA*dZHHh0OSUsual()/2D0 + dZUUOSLeft()/2D0 + &
			& dZUUOSRight()/2D0 )*(1.5D0*CA2*EL2*(Mh02 - 4.D0*MU2)*MU2)/(MW2*SB2*SW2)
	case (2)
		totalAmplitude = ( dgAtMZ()/(EL/SW) + dMUOSUsual()/MU - dMW2Usual()/(2D0*MW2) - 1D0/TB*dBeta2KanUsual() - &
			& TA*dAlphaKanUsual() + dZh0h0OS()/2D0 + TA*dZHHh0OSUsual()/2D0 + dZUUOSLeft()/2D0 + &
			& dZUUOSRight()/2D0 )*(1.5D0*CA2*EL2*(Mh02 - 4.D0*MU2)*MU2)/(MW2*SB2*SW2)
	case (3)
		totalAmplitude = ( dgAtMZ()/(EL/SW) + dMUOSAlter()/MU - dMW2Alter()/(2D0*MW2) - 1D0/TB*dBeta1KanAlter() - &
			& TA*dAlphaKanAlter() + dZh0h0OS()/2D0 + TA*dZHHh0OSAlter()/2D0 + dZUUOSLeft()/2D0 + &
			& dZUUOSRight()/2D0 )*(1.5D0*CA2*EL2*(Mh02 - 4.D0*MU2)*MU2)/(MW2*SB2*SW2)	
	case (4)
		totalAmplitude = ( dgAtMZ()/(EL/SW) + dMUOSAlter()/MU - dMW2Alter()/(2D0*MW2) - 1D0/TB*dBeta2KanAlter() - &
			& TA*dAlphaKanAlter() + dZh0h0OS()/2D0 + TA*dZHHh0OSAlter()/2D0 + dZUUOSLeft()/2D0 + &
			& dZUUOSRight()/2D0 )*(1.5D0*CA2*EL2*(Mh02 - 4.D0*MU2)*MU2)/(MW2*SB2*SW2)
	case (5)
		totalAmplitude = ( dgAtMZ()/(EL/SW) + dMUOSAlter()/MU - dMW2Alter()/(2D0*MW2) - 1D0/TB*dBeta1PinchPStar() - &
			& TA*dAlphaPinchPStar() + dZh0h0OS()/2D0 + TA*dZHHh0OSAlter()/2D0 + dZUUOSLeft()/2D0 + &
			& dZUUOSRight()/2D0 )*(1.5D0*CA2*EL2*(Mh02 - 4.D0*MU2)*MU2)/(MW2*SB2*SW2)
	case (6)
		totalAmplitude = ( dgAtMZ()/(EL/SW) + dMUOSAlter()/MU - dMW2Alter()/(2D0*MW2) - 1D0/TB*dBeta2PinchPStar() - &
			& TA*dAlphaPinchPStar() + dZh0h0OS()/2D0 + TA*dZHHh0OSAlter()/2D0 + dZUUOSLeft()/2D0 + &
			& dZUUOSRight()/2D0 )*(1.5D0*CA2*EL2*(Mh02 - 4.D0*MU2)*MU2)/(MW2*SB2*SW2)
	case (7)
		totalAmplitude = ( dgAtMZ()/(EL/SW) + dMUOSAlter()/MU - dMW2Alter()/(2D0*MW2) - 1D0/TB*dBeta1PinchOS() - &
			& TA*dAlphaPinchOS() + dZh0h0OS()/2D0 + TA*dZHHh0OSAlter()/2D0 + dZUUOSLeft()/2D0 + &
			& dZUUOSRight()/2D0 )*(1.5D0*CA2*EL2*(Mh02 - 4.D0*MU2)*MU2)/(MW2*SB2*SW2)
	case (8)
		totalAmplitude = ( dgAtMZ()/(EL/SW) + dMUOSAlter()/MU - dMW2Alter()/(2D0*MW2) - 1D0/TB*dBeta2PinchOS() - &
			& TA*dAlphaPinchOS() + dZh0h0OS()/2D0 + TA*dZHHh0OSAlter()/2D0 + dZUUOSLeft()/2D0 + &
			& dZUUOSRight()/2D0 )*(1.5D0*CA2*EL2*(Mh02 - 4.D0*MU2)*MU2)/(MW2*SB2*SW2)
	case (9)
		totalAmplitude = ( dgAtMZ()/(EL/SW) + dMUOSAlter()/MU - dMW2Alter()/(2D0*MW2) - 1D0/TB*dBetaProcDep1Alter() - &
			& TA*dAlphaProcDep1Alter() + dZh0h0OS()/2D0 + TA*dZHHh0OSAlter()/2D0 + dZUUOSLeft()/2D0 + &
			& dZUUOSRight()/2D0 )*(1.5D0*CA2*EL2*(Mh02 - 4.D0*MU2)*MU2)/(MW2*SB2*SW2)
	case (10)
		totalAmplitude = ( dgAtMZ()/(EL/SW) + dMUOSAlter()/MU - dMW2Alter()/(2D0*MW2) - 1D0/TB*dBetaProcDep2Alter() - &
			& TA*dAlphaProcDep2Alter() + dZh0h0OS()/2D0 + TA*dZHHh0OSAlter()/2D0 + dZUUOSLeft()/2D0 + &
			& dZUUOSRight()/2D0 )*(1.5D0*CA2*EL2*(Mh02 - 4.D0*MU2)*MU2)/(MW2*SB2*SW2)
	case (11)
		totalAmplitude = ( dgAtMZ()/(EL/SW) + dMUOSAlter()/MU - dMW2Alter()/(2D0*MW2) - 1D0/TB*dBetaProcDep3Alter() - &
			& TA*dAlphaProcDep3Alter() + dZh0h0OS()/2D0 + TA*dZHHh0OSAlter()/2D0 + dZUUOSLeft()/2D0 + &
			& dZUUOSRight()/2D0 )*(1.5D0*CA2*EL2*(Mh02 - 4.D0*MU2)*MU2)/(MW2*SB2*SW2)
	case (12)
		totalAmplitude = ( dgAtMZ()/(EL/SW) + dMUOSAlter()/MU - dMW2Alter()/(2D0*MW2) - 1D0/TB*dBetaOS1Alter() - &
			& TA*dAlphaOS1Alter() + dZh0h0OS()/2D0 + TA*dZHHh0OSAlter()/2D0 + dZUUOSLeft()/2D0 + &
			& dZUUOSRight()/2D0 )*(1.5D0*CA2*EL2*(Mh02 - 4.D0*MU2)*MU2)/(MW2*SB2*SW2)
	case (13)
		totalAmplitude = ( dgAtMZ()/(EL/SW) + dMUOSAlter()/MU - dMW2Alter()/(2D0*MW2) - 1D0/TB*dBetaOS2Alter() - &
			& TA*dAlphaOS2Alter() + dZh0h0OS()/2D0 + TA*dZHHh0OSAlter()/2D0 + dZUUOSLeft()/2D0 + &
			& dZUUOSRight()/2D0 )*(1.5D0*CA2*EL2*(Mh02 - 4.D0*MU2)*MU2)/(MW2*SB2*SW2)
	case (14)
		totalAmplitude = ( dgAtMZ()/(EL/SW) + dMUOSAlter()/MU - dMW2Alter()/(2D0*MW2) - 1D0/TB*dBetaOS12Alter() - &
			& TA*dAlphaOS12Alter() + dZh0h0OS()/2D0 + TA*dZHHh0OSAlter()/2D0 + dZUUOSLeft()/2D0 + &
			& dZUUOSRight()/2D0 )*(1.5D0*CA2*EL2*(Mh02 - 4.D0*MU2)*MU2)/(MW2*SB2*SW2)
	case (15)
		totalAmplitude = ( dgAtMZ()/(EL/SW) + dMUOSAlter()/MU - dMW2Alter()/(2D0*MW2) - 1D0/TB*dBetaBFMSAlter() - &
			& TA*dAlphaPinchOS() + dZh0h0OS()/2D0 + TA*dZHHh0OSAlter()/2D0 + dZUUOSLeft()/2D0 + &
			& dZUUOSRight()/2D0 )*(1.5D0*CA2*EL2*(Mh02 - 4.D0*MU2)*MU2)/(MW2*SB2*SW2)
	case (16)
		totalAmplitude = ( dgAtMZ()/(EL/SW) + dMUOSUsual()/MU - dMW2Usual()/(2D0*MW2) - 1D0/TB*dBetaMSBarUsual() - &
			& TA*dAlphaMSBarUsual() + dZh0h0OS()/2D0 + TA*dZHHh0OSUsual()/2D0 + dZUUOSLeft()/2D0 + &
			& dZUUOSRight()/2D0 )*(1.5D0*CA2*EL2*(Mh02 - 4.D0*MU2)*MU2)/(MW2*SB2*SW2)
	case (17)
		totalAmplitude = ( dgAtMZ()/(EL/SW) + dMUOSAlter()/MU - dMW2Alter()/(2D0*MW2) - 1D0/TB*dBetaMSBarAlter() - &
			& TA*dAlphaMSBarAlter() + dZh0h0OS()/2D0 + TA*dZHHh0OSAlter()/2D0 + dZUUOSLeft()/2D0 + &
			& dZUUOSRight()/2D0 )*(1.5D0*CA2*EL2*(Mh02 - 4.D0*MU2)*MU2)/(MW2*SB2*SW2)
 end select

 h0toUUBarCT = totalAmplitude
end function h0toUUBarCT
