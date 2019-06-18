double precision function h0toA0Z0CT(x)
 use constants
 use counterterms
 implicit none
#include "looptools.h"
 integer, intent(in) :: x
 double precision :: totalAmplitude

 select case (x)
	case (1)
        totalAmplitude = ( dgAtMZ()/(EL/SW) + (dMZ2Usual()/MZ2 - dMW2Usual()/MW2)/2D0 + dZh0h0OS()/2D0 + dZZ0Z0OS()/2D0 + &
            & dZA0A0OS()/2D0 - SBA/CBA*( dBeta1KanUsual() - dAlphaKanUsual() - dZG0A0OSUsual()/2D0 + dZHHh0OSUsual()/2D0 ) ) &
            & * (EL2/SW2)*CBA2/CW2 * ( (Mh02**2 + MZ2**2 + MA02**2 - 2D0*Mh02*MZ2 - 2D0*Mh02*MA02 - 2D0*MZ2*MA02)/(4D0*MZ2) )
    case (2)
        totalAmplitude = ( dgAtMZ()/(EL/SW) + (dMZ2Usual()/MZ2 - dMW2Usual()/MW2)/2D0 + dZh0h0OS()/2D0 + dZZ0Z0OS()/2D0 + &
            & dZA0A0OS()/2D0 - SBA/CBA*( dBeta2KanUsual() - dAlphaKanUsual() - dZG0A0OSUsual()/2D0 + dZHHh0OSUsual()/2D0 ) ) &
            & * (EL2/SW2)*CBA2/CW2 * ( (Mh02**2 + MZ2**2 + MA02**2 - 2D0*Mh02*MZ2 - 2D0*Mh02*MA02 - 2D0*MZ2*MA02)/(4D0*MZ2) )
    case (3)
        totalAmplitude = ( dgAtMZ()/(EL/SW) + (dMZ2Alter()/MZ2 - dMW2Alter()/MW2)/2D0 + dZh0h0OS()/2D0 + dZZ0Z0OS()/2D0 + &
            & dZA0A0OS()/2D0 - SBA/CBA*( dBeta1KanAlter() - dAlphaKanAlter() - dZG0A0OSAlter()/2D0 + dZHHh0OSAlter()/2D0 ) ) &
            & * (EL2/SW2)*CBA2/CW2 * ( (Mh02**2 + MZ2**2 + MA02**2 - 2D0*Mh02*MZ2 - 2D0*Mh02*MA02 - 2D0*MZ2*MA02)/(4D0*MZ2) )
    case (4)
        totalAmplitude = ( dgAtMZ()/(EL/SW) + (dMZ2Alter()/MZ2 - dMW2Alter()/MW2)/2D0 + dZh0h0OS()/2D0 + dZZ0Z0OS()/2D0 + &
            & dZA0A0OS()/2D0 - SBA/CBA*( dBeta2KanAlter() - dAlphaKanAlter() - dZG0A0OSAlter()/2D0 + dZHHh0OSAlter()/2D0 ) ) &
            & * (EL2/SW2)*CBA2/CW2 * ( (Mh02**2 + MZ2**2 + MA02**2 - 2D0*Mh02*MZ2 - 2D0*Mh02*MA02 - 2D0*MZ2*MA02)/(4D0*MZ2) )
    case (5)
        totalAmplitude = ( dgAtMZ()/(EL/SW) + (dMZ2Alter()/MZ2 - dMW2Alter()/MW2)/2D0 + dZh0h0OS()/2D0 + dZZ0Z0OS()/2D0 + &
            & dZA0A0OS()/2D0 - SBA/CBA*( dBeta1PinchPStar() - dAlphaPinchPStar() - dZG0A0OSAlter()/2D0 + dZHHh0OSAlter()/2D0 ) ) &
            & * (EL2/SW2)*CBA2/CW2 * ( (Mh02**2 + MZ2**2 + MA02**2 - 2D0*Mh02*MZ2 - 2D0*Mh02*MA02 - 2D0*MZ2*MA02)/(4D0*MZ2) )
    case (6)
        totalAmplitude = ( dgAtMZ()/(EL/SW) + (dMZ2Alter()/MZ2 - dMW2Alter()/MW2)/2D0 + dZh0h0OS()/2D0 + dZZ0Z0OS()/2D0 + &
            & dZA0A0OS()/2D0 - SBA/CBA*( dBeta2PinchPStar() - dAlphaPinchPStar() - dZG0A0OSAlter()/2D0 + dZHHh0OSAlter()/2D0 ) ) &
            & * (EL2/SW2)*CBA2/CW2 * ( (Mh02**2 + MZ2**2 + MA02**2 - 2D0*Mh02*MZ2 - 2D0*Mh02*MA02 - 2D0*MZ2*MA02)/(4D0*MZ2) )
    case (7)
        totalAmplitude = ( dgAtMZ()/(EL/SW) + (dMZ2Alter()/MZ2 - dMW2Alter()/MW2)/2D0 + dZh0h0OS()/2D0 + dZZ0Z0OS()/2D0 + &
            & dZA0A0OS()/2D0 - SBA/CBA*( dBeta1PinchOS() - dAlphaPinchOS() - dZG0A0OSAlter()/2D0 + dZHHh0OSAlter()/2D0 ) ) &
            & * (EL2/SW2)*CBA2/CW2 * ( (Mh02**2 + MZ2**2 + MA02**2 - 2D0*Mh02*MZ2 - 2D0*Mh02*MA02 - 2D0*MZ2*MA02)/(4D0*MZ2) )
    case (8)
        totalAmplitude = ( dgAtMZ()/(EL/SW) + (dMZ2Alter()/MZ2 - dMW2Alter()/MW2)/2D0 + dZh0h0OS()/2D0 + dZZ0Z0OS()/2D0 + &
            & dZA0A0OS()/2D0 - SBA/CBA*( dBeta2PinchOS() - dAlphaPinchOS() - dZG0A0OSAlter()/2D0 + dZHHh0OSAlter()/2D0 ) ) &
            & * (EL2/SW2)*CBA2/CW2 * ( (Mh02**2 + MZ2**2 + MA02**2 - 2D0*Mh02*MZ2 - 2D0*Mh02*MA02 - 2D0*MZ2*MA02)/(4D0*MZ2) )
    case (9)
        totalAmplitude = ( dgAtMZ()/(EL/SW) + (dMZ2Alter()/MZ2 - dMW2Alter()/MW2)/2D0 + dZh0h0OS()/2D0 + dZZ0Z0OS()/2D0 + &
        &dZA0A0OS()/2D0 - SBA/CBA*( dBetaProcDep1Alter() - dAlphaProcDep1Alter() - dZG0A0OSAlter()/2D0 + dZHHh0OSAlter()/2D0))&
            & * (EL2/SW2)*CBA2/CW2 * ( (Mh02**2 + MZ2**2 + MA02**2 - 2D0*Mh02*MZ2 - 2D0*Mh02*MA02 - 2D0*MZ2*MA02)/(4D0*MZ2) )
    case (10)
        totalAmplitude = ( dgAtMZ()/(EL/SW) + (dMZ2Alter()/MZ2 - dMW2Alter()/MW2)/2D0 + dZh0h0OS()/2D0 + dZZ0Z0OS()/2D0 + &
        &dZA0A0OS()/2D0 - SBA/CBA*( dBetaProcDep2Alter() - dAlphaProcDep2Alter() - dZG0A0OSAlter()/2D0 + dZHHh0OSAlter()/2D0))&
            & * (EL2/SW2)*CBA2/CW2 * ( (Mh02**2 + MZ2**2 + MA02**2 - 2D0*Mh02*MZ2 - 2D0*Mh02*MA02 - 2D0*MZ2*MA02)/(4D0*MZ2) )
    case (11)
        totalAmplitude = ( dgAtMZ()/(EL/SW) + (dMZ2Alter()/MZ2 - dMW2Alter()/MW2)/2D0 + dZh0h0OS()/2D0 + dZZ0Z0OS()/2D0 + &
        &dZA0A0OS()/2D0 - SBA/CBA*( dBetaProcDep3Alter() - dAlphaProcDep3Alter() - dZG0A0OSAlter()/2D0 + dZHHh0OSAlter()/2D0))&
            & * (EL2/SW2)*CBA2/CW2 * ( (Mh02**2 + MZ2**2 + MA02**2 - 2D0*Mh02*MZ2 - 2D0*Mh02*MA02 - 2D0*MZ2*MA02)/(4D0*MZ2) )
    case (12)
        totalAmplitude = ( dgAtMZ()/(EL/SW) + (dMZ2Alter()/MZ2 - dMW2Alter()/MW2)/2D0 + dZh0h0OS()/2D0 + dZZ0Z0OS()/2D0 + &
        &dZA0A0OS()/2D0 - SBA/CBA*( dBetaOS1Alter() - dAlphaOS1Alter() - dZG0A0OSAlter()/2D0 + dZHHh0OSAlter()/2D0))&
            & * (EL2/SW2)*CBA2/CW2 * ( (Mh02**2 + MZ2**2 + MA02**2 - 2D0*Mh02*MZ2 - 2D0*Mh02*MA02 - 2D0*MZ2*MA02)/(4D0*MZ2) )
    case (13)
        totalAmplitude = ( dgAtMZ()/(EL/SW) + (dMZ2Alter()/MZ2 - dMW2Alter()/MW2)/2D0 + dZh0h0OS()/2D0 + dZZ0Z0OS()/2D0 + &
        &dZA0A0OS()/2D0 - SBA/CBA*( dBetaOS2Alter() - dAlphaOS2Alter() - dZG0A0OSAlter()/2D0 + dZHHh0OSAlter()/2D0))&
            & * (EL2/SW2)*CBA2/CW2 * ( (Mh02**2 + MZ2**2 + MA02**2 - 2D0*Mh02*MZ2 - 2D0*Mh02*MA02 - 2D0*MZ2*MA02)/(4D0*MZ2) )
    case (14)
        totalAmplitude = ( dgAtMZ()/(EL/SW) + (dMZ2Alter()/MZ2 - dMW2Alter()/MW2)/2D0 + dZh0h0OS()/2D0 + dZZ0Z0OS()/2D0 + &
        &dZA0A0OS()/2D0 - SBA/CBA*( dBetaOS12Alter() - dAlphaOS12Alter() - dZG0A0OSAlter()/2D0 + dZHHh0OSAlter()/2D0))&
            & * (EL2/SW2)*CBA2/CW2 * ( (Mh02**2 + MZ2**2 + MA02**2 - 2D0*Mh02*MZ2 - 2D0*Mh02*MA02 - 2D0*MZ2*MA02)/(4D0*MZ2) )
    case (15)
        totalAmplitude = ( dgAtMZ()/(EL/SW) + (dMZ2Alter()/MZ2 - dMW2Alter()/MW2)/2D0 + dZh0h0OS()/2D0 + dZZ0Z0OS()/2D0 + &
        &dZA0A0OS()/2D0 - SBA/CBA*( dBetaBFMSAlter() - dAlphaPinchOS() - dZG0A0OSAlter()/2D0 + dZHHh0OSAlter()/2D0))&
            & * (EL2/SW2)*CBA2/CW2 * ( (Mh02**2 + MZ2**2 + MA02**2 - 2D0*Mh02*MZ2 - 2D0*Mh02*MA02 - 2D0*MZ2*MA02)/(4D0*MZ2) )
    case (16)
        totalAmplitude = ( dgAtMZ()/(EL/SW) + (dMZ2Usual()/MZ2 - dMW2Usual()/MW2)/2D0 + dZh0h0OS()/2D0 + dZZ0Z0OS()/2D0 + &
            & dZA0A0OS()/2D0 - SBA/CBA*( dBetaMSBarUsual() - dAlphaMSBarUsual() - dZG0A0OSUsual()/2D0 + dZHHh0OSUsual()/2D0 ) ) &
            & * (EL2/SW2)*CBA2/CW2 * ( (Mh02**2 + MZ2**2 + MA02**2 - 2D0*Mh02*MZ2 - 2D0*Mh02*MA02 - 2D0*MZ2*MA02)/(4D0*MZ2) )
    case (17)
        totalAmplitude = ( dgAtMZ()/(EL/SW) + (dMZ2Alter()/MZ2 - dMW2Alter()/MW2)/2D0 + dZh0h0OS()/2D0 + dZZ0Z0OS()/2D0 + &
        &dZA0A0OS()/2D0 - SBA/CBA*( dBetaMSBarAlter() - dAlphaMSBarAlter() - dZG0A0OSAlter()/2D0 + dZHHh0OSAlter()/2D0))&
            & * (EL2/SW2)*CBA2/CW2 * ( (Mh02**2 + MZ2**2 + MA02**2 - 2D0*Mh02*MZ2 - 2D0*Mh02*MA02 - 2D0*MZ2*MA02)/(4D0*MZ2) )
 end select

 h0toA0Z0CT = totalAmplitude
end function h0toA0Z0CT
