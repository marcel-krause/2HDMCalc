double precision function A0toHHZ0CT(x)
 use constants
 use counterterms
 implicit none
#include "looptools.h"
 integer, intent(in) :: x
 double precision :: totalAmplitude

 select case (x)
    case (1)
        totalAmplitude = ( dgAtMZ()/(EL/SW) + (dMZ2Usual()/MZ2 - dMW2Usual()/MW2)/2D0 + dZHHHHOS()/2D0 + dZZ0Z0OS()/2D0 + &
            & dZA0A0OS()/2D0 + CBA/SBA*( dBeta1KanUsual() - dAlphaKanUsual() - dZG0A0OSUsual()/2D0 - dZh0HHOSUsual()/2D0 ) ) &
            & * (EL2/SW2)*SBA2/CW2 * ( (MHH2**2 + MZ2**2 + MA02**2 - 2D0*MHH2*MZ2 - 2D0*MHH2*MA02 - 2D0*MZ2*MA02)/(4D0*MZ2) )
    case (2)
        totalAmplitude = ( dgAtMZ()/(EL/SW) + (dMZ2Usual()/MZ2 - dMW2Usual()/MW2)/2D0 + dZHHHHOS()/2D0 + dZZ0Z0OS()/2D0 + &
            & dZA0A0OS()/2D0 + CBA/SBA*( dBeta2KanUsual() - dAlphaKanUsual() - dZG0A0OSUsual()/2D0 - dZh0HHOSUsual()/2D0 ) ) &
            & * (EL2/SW2)*SBA2/CW2 * ( (MHH2**2 + MZ2**2 + MA02**2 - 2D0*MHH2*MZ2 - 2D0*MHH2*MA02 - 2D0*MZ2*MA02)/(4D0*MZ2) )
    case (3)
        totalAmplitude = ( dgAtMZ()/(EL/SW) + (dMZ2Alter()/MZ2 - dMW2Alter()/MW2)/2D0 + dZHHHHOS()/2D0 + dZZ0Z0OS()/2D0 + &
            & dZA0A0OS()/2D0 + CBA/SBA*( dBeta1KanAlter() - dAlphaKanAlter() - dZG0A0OSAlter()/2D0 - dZh0HHOSAlter()/2D0 ) ) &
            & * (EL2/SW2)*SBA2/CW2 * ( (MHH2**2 + MZ2**2 + MA02**2 - 2D0*MHH2*MZ2 - 2D0*MHH2*MA02 - 2D0*MZ2*MA02)/(4D0*MZ2) )
    case (4)
        totalAmplitude = ( dgAtMZ()/(EL/SW) + (dMZ2Alter()/MZ2 - dMW2Alter()/MW2)/2D0 + dZHHHHOS()/2D0 + dZZ0Z0OS()/2D0 + &
            & dZA0A0OS()/2D0 + CBA/SBA*( dBeta2KanAlter() - dAlphaKanAlter() - dZG0A0OSAlter()/2D0 - dZh0HHOSAlter()/2D0 ) ) &
            & * (EL2/SW2)*SBA2/CW2 * ( (MHH2**2 + MZ2**2 + MA02**2 - 2D0*MHH2*MZ2 - 2D0*MHH2*MA02 - 2D0*MZ2*MA02)/(4D0*MZ2) )
    case (5)
        totalAmplitude = ( dgAtMZ()/(EL/SW) + (dMZ2Alter()/MZ2 - dMW2Alter()/MW2)/2D0 + dZHHHHOS()/2D0 + dZZ0Z0OS()/2D0 + &
            & dZA0A0OS()/2D0 + CBA/SBA*( dBeta1PinchPStar() - dAlphaPinchPStar() - dZG0A0OSAlter()/2D0 - dZh0HHOSAlter()/2D0 ) ) &
            & * (EL2/SW2)*SBA2/CW2 * ( (MHH2**2 + MZ2**2 + MA02**2 - 2D0*MHH2*MZ2 - 2D0*MHH2*MA02 - 2D0*MZ2*MA02)/(4D0*MZ2) )
    case (6)
        totalAmplitude = ( dgAtMZ()/(EL/SW) + (dMZ2Alter()/MZ2 - dMW2Alter()/MW2)/2D0 + dZHHHHOS()/2D0 + dZZ0Z0OS()/2D0 + &
            & dZA0A0OS()/2D0 + CBA/SBA*( dBeta2PinchPStar() - dAlphaPinchPStar() - dZG0A0OSAlter()/2D0 - dZh0HHOSAlter()/2D0 ) ) &
            & * (EL2/SW2)*SBA2/CW2 * ( (MHH2**2 + MZ2**2 + MA02**2 - 2D0*MHH2*MZ2 - 2D0*MHH2*MA02 - 2D0*MZ2*MA02)/(4D0*MZ2) )
    case (7)
        totalAmplitude = ( dgAtMZ()/(EL/SW) + (dMZ2Alter()/MZ2 - dMW2Alter()/MW2)/2D0 + dZHHHHOS()/2D0 + dZZ0Z0OS()/2D0 + &
            & dZA0A0OS()/2D0 + CBA/SBA*( dBeta1PinchOS() - dAlphaPinchOS() - dZG0A0OSAlter()/2D0 - dZh0HHOSAlter()/2D0 ) ) &
            & * (EL2/SW2)*SBA2/CW2 * ( (MHH2**2 + MZ2**2 + MA02**2 - 2D0*MHH2*MZ2 - 2D0*MHH2*MA02 - 2D0*MZ2*MA02)/(4D0*MZ2) )
    case (8)
        totalAmplitude = ( dgAtMZ()/(EL/SW) + (dMZ2Alter()/MZ2 - dMW2Alter()/MW2)/2D0 + dZHHHHOS()/2D0 + dZZ0Z0OS()/2D0 + &
            & dZA0A0OS()/2D0 + CBA/SBA*( dBeta2PinchOS() - dAlphaPinchOS() - dZG0A0OSAlter()/2D0 - dZh0HHOSAlter()/2D0 ) ) &
            & * (EL2/SW2)*SBA2/CW2 * ( (MHH2**2 + MZ2**2 + MA02**2 - 2D0*MHH2*MZ2 - 2D0*MHH2*MA02 - 2D0*MZ2*MA02)/(4D0*MZ2) )
    case (9)
        totalAmplitude = ( dgAtMZ()/(EL/SW) + (dMZ2Alter()/MZ2 - dMW2Alter()/MW2)/2D0 + dZHHHHOS()/2D0 + dZZ0Z0OS()/2D0 + &
            &dZA0A0OS()/2D0 + CBA/SBA*( dBetaProcDep1Alter() - dAlphaProcDep1Alter() - dZG0A0OSAlter()/2D0 - dZh0HHOSAlter()/2D0))&
            & * (EL2/SW2)*SBA2/CW2 * ( (MHH2**2 + MZ2**2 + MA02**2 - 2D0*MHH2*MZ2 - 2D0*MHH2*MA02 - 2D0*MZ2*MA02)/(4D0*MZ2) )
    case (10)
        totalAmplitude = ( dgAtMZ()/(EL/SW) + (dMZ2Alter()/MZ2 - dMW2Alter()/MW2)/2D0 + dZHHHHOS()/2D0 + dZZ0Z0OS()/2D0 + &
            &dZA0A0OS()/2D0 + CBA/SBA*( dBetaProcDep2Alter() - dAlphaProcDep2Alter() - dZG0A0OSAlter()/2D0 - dZh0HHOSAlter()/2D0))&
            & * (EL2/SW2)*SBA2/CW2 * ( (MHH2**2 + MZ2**2 + MA02**2 - 2D0*MHH2*MZ2 - 2D0*MHH2*MA02 - 2D0*MZ2*MA02)/(4D0*MZ2) )
    case (11)
        totalAmplitude = ( dgAtMZ()/(EL/SW) + (dMZ2Alter()/MZ2 - dMW2Alter()/MW2)/2D0 + dZHHHHOS()/2D0 + dZZ0Z0OS()/2D0 + &
            &dZA0A0OS()/2D0 + CBA/SBA*( dBetaProcDep3Alter() - dAlphaProcDep3Alter() - dZG0A0OSAlter()/2D0 - dZh0HHOSAlter()/2D0))&
            & * (EL2/SW2)*SBA2/CW2 * ( (MHH2**2 + MZ2**2 + MA02**2 - 2D0*MHH2*MZ2 - 2D0*MHH2*MA02 - 2D0*MZ2*MA02)/(4D0*MZ2) )   
    case (12)
        totalAmplitude = ( dgAtMZ()/(EL/SW) + (dMZ2Alter()/MZ2 - dMW2Alter()/MW2)/2D0 + dZHHHHOS()/2D0 + dZZ0Z0OS()/2D0 + &
            &dZA0A0OS()/2D0 + CBA/SBA*( dBetaOS1Alter() - dAlphaOS1Alter() - dZG0A0OSAlter()/2D0 - dZh0HHOSAlter()/2D0))&
            & * (EL2/SW2)*SBA2/CW2 * ( (MHH2**2 + MZ2**2 + MA02**2 - 2D0*MHH2*MZ2 - 2D0*MHH2*MA02 - 2D0*MZ2*MA02)/(4D0*MZ2) )
    case (13)
        totalAmplitude = ( dgAtMZ()/(EL/SW) + (dMZ2Alter()/MZ2 - dMW2Alter()/MW2)/2D0 + dZHHHHOS()/2D0 + dZZ0Z0OS()/2D0 + &
            &dZA0A0OS()/2D0 + CBA/SBA*( dBetaOS2Alter() - dAlphaOS2Alter() - dZG0A0OSAlter()/2D0 - dZh0HHOSAlter()/2D0))&
            & * (EL2/SW2)*SBA2/CW2 * ( (MHH2**2 + MZ2**2 + MA02**2 - 2D0*MHH2*MZ2 - 2D0*MHH2*MA02 - 2D0*MZ2*MA02)/(4D0*MZ2) )
    case (14)
        totalAmplitude = ( dgAtMZ()/(EL/SW) + (dMZ2Alter()/MZ2 - dMW2Alter()/MW2)/2D0 + dZHHHHOS()/2D0 + dZZ0Z0OS()/2D0 + &
            &dZA0A0OS()/2D0 + CBA/SBA*( dBetaOS12Alter() - dAlphaOS12Alter() - dZG0A0OSAlter()/2D0 - dZh0HHOSAlter()/2D0))&
            & * (EL2/SW2)*SBA2/CW2 * ( (MHH2**2 + MZ2**2 + MA02**2 - 2D0*MHH2*MZ2 - 2D0*MHH2*MA02 - 2D0*MZ2*MA02)/(4D0*MZ2) )
    case (15)
        totalAmplitude = ( dgAtMZ()/(EL/SW) + (dMZ2Alter()/MZ2 - dMW2Alter()/MW2)/2D0 + dZHHHHOS()/2D0 + dZZ0Z0OS()/2D0 + &
            &dZA0A0OS()/2D0 + CBA/SBA*( dBetaBFMSAlter() - dAlphaPinchOS() - dZG0A0OSAlter()/2D0 - dZh0HHOSAlter()/2D0))&
            & * (EL2/SW2)*SBA2/CW2 * ( (MHH2**2 + MZ2**2 + MA02**2 - 2D0*MHH2*MZ2 - 2D0*MHH2*MA02 - 2D0*MZ2*MA02)/(4D0*MZ2) )
    case (16)
        totalAmplitude = ( dgAtMZ()/(EL/SW) + (dMZ2Usual()/MZ2 - dMW2Usual()/MW2)/2D0 + dZHHHHOS()/2D0 + dZZ0Z0OS()/2D0 + &
            & dZA0A0OS()/2D0 + CBA/SBA*( dBetaMSBarUsual() - dAlphaMSBarUsual() - dZG0A0OSUsual()/2D0 - dZh0HHOSUsual()/2D0 ) ) &
            & * (EL2/SW2)*SBA2/CW2 * ( (MHH2**2 + MZ2**2 + MA02**2 - 2D0*MHH2*MZ2 - 2D0*MHH2*MA02 - 2D0*MZ2*MA02)/(4D0*MZ2) )
    case (17)
        totalAmplitude = ( dgAtMZ()/(EL/SW) + (dMZ2Alter()/MZ2 - dMW2Alter()/MW2)/2D0 + dZHHHHOS()/2D0 + dZZ0Z0OS()/2D0 + &
            &dZA0A0OS()/2D0 + CBA/SBA*( dBetaMSBarAlter() - dAlphaMSBarAlter() - dZG0A0OSAlter()/2D0 - dZh0HHOSAlter()/2D0))&
            & * (EL2/SW2)*SBA2/CW2 * ( (MHH2**2 + MZ2**2 + MA02**2 - 2D0*MHH2*MZ2 - 2D0*MHH2*MA02 - 2D0*MZ2*MA02)/(4D0*MZ2) )
    case default
        totalAmplitude = 0D0
 end select
 A0toHHZ0CT = totalAmplitude
end function A0toHHZ0CT
