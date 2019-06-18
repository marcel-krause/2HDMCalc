double precision function h0toZ0Z0CT(x)
 use constants
 use counterterms
 implicit none
#include "looptools.h"
 integer, intent(in) :: x
 double precision :: totalAmplitude

 select case (x)
	case (1)
		totalAmplitude = ( dgAtMZ()/(EL/SW) + dMZ2Usual()/MZ2 - dMW2Usual()/(2D0*MW2) + dZh0h0OS()/2D0 + dZZ0Z0OS() + &
            & CBA/SBA*(dZHHh0OSUsual()/2D0 - dAlphaKanUsual() + dBeta1KanUsual()) ) * (EL2/SW2)*SBA2*MW2/(CW2**2) * &
            & (3D0 + Mh02**2/(4D0*MZ2**2) - Mh02/MZ2)
	case (2)
        totalAmplitude = ( dgAtMZ()/(EL/SW) + dMZ2Usual()/MZ2 - dMW2Usual()/(2D0*MW2) + dZh0h0OS()/2D0 + dZZ0Z0OS() + &
            & CBA/SBA*(dZHHh0OSUsual()/2D0 - dAlphaKanUsual() + dBeta2KanUsual()) ) * (EL2/SW2)*SBA2*MW2/(CW2**2) * &
            & (3D0 + Mh02**2/(4D0*MZ2**2) - Mh02/MZ2)
    case (3)
        totalAmplitude = ( dgAtMZ()/(EL/SW) + dMZ2Alter()/MZ2 - dMW2Alter()/(2D0*MW2) + dZh0h0OS()/2D0 + dZZ0Z0OS() + &
            & CBA/SBA*(dZHHh0OSAlter()/2D0 - dAlphaKanAlter() + dBeta1KanAlter()) ) * (EL2/SW2)*SBA2*MW2/(CW2**2) * &
            & (3D0 + Mh02**2/(4D0*MZ2**2) - Mh02/MZ2)
    case (4)
        totalAmplitude = ( dgAtMZ()/(EL/SW) + dMZ2Alter()/MZ2 - dMW2Alter()/(2D0*MW2) + dZh0h0OS()/2D0 + dZZ0Z0OS() + &
            & CBA/SBA*(dZHHh0OSAlter()/2D0 - dAlphaKanAlter() + dBeta2KanAlter()) ) * (EL2/SW2)*SBA2*MW2/(CW2**2) * &
            & (3D0 + Mh02**2/(4D0*MZ2**2) - Mh02/MZ2)
    case (5)
        totalAmplitude = ( dgAtMZ()/(EL/SW) + dMZ2Alter()/MZ2 - dMW2Alter()/(2D0*MW2) + dZh0h0OS()/2D0 + dZZ0Z0OS() + &
            & CBA/SBA*(dZHHh0OSAlter()/2D0 - dAlphaPinchPStar() + dBeta1PinchPStar()) ) * (EL2/SW2)*SBA2*MW2/(CW2**2) * &
            & (3D0 + Mh02**2/(4D0*MZ2**2) - Mh02/MZ2)
    case (6)
        totalAmplitude = ( dgAtMZ()/(EL/SW) + dMZ2Alter()/MZ2 - dMW2Alter()/(2D0*MW2) + dZh0h0OS()/2D0 + dZZ0Z0OS() + &
            & CBA/SBA*(dZHHh0OSAlter()/2D0 - dAlphaPinchPStar() + dBeta2PinchPStar()) ) * (EL2/SW2)*SBA2*MW2/(CW2**2) * &
            & (3D0 + Mh02**2/(4D0*MZ2**2) - Mh02/MZ2)
    case (7)
        totalAmplitude = ( dgAtMZ()/(EL/SW) + dMZ2Alter()/MZ2 - dMW2Alter()/(2D0*MW2) + dZh0h0OS()/2D0 + dZZ0Z0OS() + &
            & CBA/SBA*(dZHHh0OSAlter()/2D0 - dAlphaPinchOS() + dBeta1PinchOS()) ) * (EL2/SW2)*SBA2*MW2/(CW2**2) * &
            & (3D0 + Mh02**2/(4D0*MZ2**2) - Mh02/MZ2)
    case (8)
        totalAmplitude = ( dgAtMZ()/(EL/SW) + dMZ2Alter()/MZ2 - dMW2Alter()/(2D0*MW2) + dZh0h0OS()/2D0 + dZZ0Z0OS() + &
            & CBA/SBA*(dZHHh0OSAlter()/2D0 - dAlphaPinchOS() + dBeta2PinchOS()) ) * (EL2/SW2)*SBA2*MW2/(CW2**2) * &
            & (3D0 + Mh02**2/(4D0*MZ2**2) - Mh02/MZ2)
    case (9)
        totalAmplitude = ( dgAtMZ()/(EL/SW) + dMZ2Alter()/MZ2 - dMW2Alter()/(2D0*MW2) + dZh0h0OS()/2D0 + dZZ0Z0OS() + &
            & CBA/SBA*(dZHHh0OSAlter()/2D0 - dAlphaProcDep1Alter() + dBetaProcDep1Alter()) ) * (EL2/SW2)*SBA2*MW2/(CW2**2) * &
            & (3D0 + Mh02**2/(4D0*MZ2**2) - Mh02/MZ2)
    case (10)
        totalAmplitude = ( dgAtMZ()/(EL/SW) + dMZ2Alter()/MZ2 - dMW2Alter()/(2D0*MW2) + dZh0h0OS()/2D0 + dZZ0Z0OS() + &
            & CBA/SBA*(dZHHh0OSAlter()/2D0 - dAlphaProcDep2Alter() + dBetaProcDep2Alter()) ) * (EL2/SW2)*SBA2*MW2/(CW2**2) * &
            & (3D0 + Mh02**2/(4D0*MZ2**2) - Mh02/MZ2)
    case (11)
        totalAmplitude = ( dgAtMZ()/(EL/SW) + dMZ2Alter()/MZ2 - dMW2Alter()/(2D0*MW2) + dZh0h0OS()/2D0 + dZZ0Z0OS() + &
            & CBA/SBA*(dZHHh0OSAlter()/2D0 - dAlphaProcDep2Alter() + dBetaProcDep3Alter()) ) * (EL2/SW2)*SBA2*MW2/(CW2**2) * &
            & (3D0 + Mh02**2/(4D0*MZ2**2) - Mh02/MZ2)
    case (12)
        totalAmplitude = ( dgAtMZ()/(EL/SW) + dMZ2Alter()/MZ2 - dMW2Alter()/(2D0*MW2) + dZh0h0OS()/2D0 + dZZ0Z0OS() + &
            & CBA/SBA*(dZHHh0OSAlter()/2D0 - dAlphaOS1Alter() + dBetaOS1Alter()) ) * (EL2/SW2)*SBA2*MW2/(CW2**2) * &
            & (3D0 + Mh02**2/(4D0*MZ2**2) - Mh02/MZ2)
    case (13)
        totalAmplitude = ( dgAtMZ()/(EL/SW) + dMZ2Alter()/MZ2 - dMW2Alter()/(2D0*MW2) + dZh0h0OS()/2D0 + dZZ0Z0OS() + &
            & CBA/SBA*(dZHHh0OSAlter()/2D0 - dAlphaOS2Alter() + dBetaOS2Alter()) ) * (EL2/SW2)*SBA2*MW2/(CW2**2) * &
            & (3D0 + Mh02**2/(4D0*MZ2**2) - Mh02/MZ2)
    case (14)
        totalAmplitude = ( dgAtMZ()/(EL/SW) + dMZ2Alter()/MZ2 - dMW2Alter()/(2D0*MW2) + dZh0h0OS()/2D0 + dZZ0Z0OS() + &
            & CBA/SBA*(dZHHh0OSAlter()/2D0 - dAlphaOS12Alter() + dBetaOS12Alter()) ) * (EL2/SW2)*SBA2*MW2/(CW2**2) * &
            & (3D0 + Mh02**2/(4D0*MZ2**2) - Mh02/MZ2)
    case (15)
        totalAmplitude = ( dgAtMZ()/(EL/SW) + dMZ2Alter()/MZ2 - dMW2Alter()/(2D0*MW2) + dZh0h0OS()/2D0 + dZZ0Z0OS() + &
            & CBA/SBA*(dZHHh0OSAlter()/2D0 - dAlphaPinchOS() + dBetaBFMSAlter()) ) * (EL2/SW2)*SBA2*MW2/(CW2**2) * &
            & (3D0 + Mh02**2/(4D0*MZ2**2) - Mh02/MZ2)
    case (16)
		totalAmplitude = ( dgAtMZ()/(EL/SW) + dMZ2Usual()/MZ2 - dMW2Usual()/(2D0*MW2) + dZh0h0OS()/2D0 + dZZ0Z0OS() + &
            & CBA/SBA*(dZHHh0OSUsual()/2D0 - dAlphaMSBarUsual() + dBetaMSBarUsual()) ) * (EL2/SW2)*SBA2*MW2/(CW2**2) * &
            & (3D0 + Mh02**2/(4D0*MZ2**2) - Mh02/MZ2)
    case (17)
        totalAmplitude = ( dgAtMZ()/(EL/SW) + dMZ2Alter()/MZ2 - dMW2Alter()/(2D0*MW2) + dZh0h0OS()/2D0 + dZZ0Z0OS() + &
            & CBA/SBA*(dZHHh0OSAlter()/2D0 - dAlphaMSBarAlter() + dBetaMSBarAlter()) ) * (EL2/SW2)*SBA2*MW2/(CW2**2) * &
            & (3D0 + Mh02**2/(4D0*MZ2**2) - Mh02/MZ2)
 end select

 h0toZ0Z0CT = totalAmplitude
end function h0toZ0Z0CT
