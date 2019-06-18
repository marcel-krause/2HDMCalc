program decayWidth
    use constants
    use counterterms
    implicit none
#include "looptools.h"
    character(len=20) :: tempVal
    character(len=32) :: arg
    character(len=50) :: fileName, fileNameFilled, targetName
    character(len=600000) :: outputFileContent
    character(300), parameter :: pathToOutputFiles = 'PLACEHOLDERTEMPRESULTPATH'
    integer arguments(5)
    integer, parameter :: maxNumberSchemes = 17
    logical :: debugModeOn = .false.
    logical :: isIRDivergent = .false.
    logical :: isUVDivergent = .false.
    logical :: isGaugeDependent = .false.
    logical :: isGaugeDependentAList(maxNumberSchemes), isGaugeDependentWList(maxNumberSchemes), &
        & isGaugeDependentZList(maxNumberSchemes)
    logical :: isIRDivergentList(maxNumberSchemes), isUVDivergentList(maxNumberSchemes)
    character isIRDivergentContinue, isUVDivergentContinue, isGaugeDependentContinue
    double precision, parameter :: GaugeDependenceThreshold = 1D-5
    double precision, parameter :: IRDivergenceThreshold = 1D-11
    double precision, parameter :: UVDivergenceThreshold = 1D-3
    double precision prefactor, treeLevelWidth, NLOWidth(maxNumberSchemes), fullamplitude(maxNumberSchemes)
    double precision NLOVCwidth, NLOVCwoIRwidth, NLOIRonlywidth
    double precision GaugeXiAChecks(maxNumberSchemes), GaugeXiWChecks(maxNumberSchemes), GaugeXiZChecks(maxNumberSchemes), &
        & IRChecks(maxNumberSchemes), UVChecks(maxNumberSchemes)
    double precision PLACEHOLDERPROCESSTree, PLACEHOLDERPROCESSCT, PLACEHOLDERPROCESSReal, treeLevelTemp, realCorrectionsTemp
    double complex PLACEHOLDERPROCESSVC, PLACEHOLDERPROCESSTad, vertexCorrectionsTemp, vertexTadpolesTemp
    integer m, n, o, p, q, r, fileNameLength, point, statWrite

    ! Get the command line arguments standing for the different running options
    ! Argument 1: perform UV divergence check (1: true, 0: false; default: 0)
    ! Argument 2: perform IR divergence check (1: true, 0: false; default: 0)
    ! Argument 3: perform gauge dependence check (2: true (prompt for continuation of program if gauge-dependence is detected), 1: true (no prompts for continuation), 0: false; default: 0)
    ! Argument 4: perform numerical evaluation (1: true, 0: false; default: 1)
    ! Argument 5: relative path to the 2HDM input parameter file, starting from the Parameters directory of 2HDMCalc
    ! Argument 6: relative path to the target file containing the results of the calculation, starting from the Temp/Results directory of 2HDMCalc
    do o = 1, iargc()
        call getarg(o, arg)
        if (arg == '1') then
            arguments(o) = 1
        else if (arg == '2') then
            arguments(o) = 2
        else if (arg == '0') then
            arguments(o) = 0
        else
            if (o == 5) then
                fileName = arg
            else if (o == 6) then
                targetName = arg
            end if
        end if
    end do

    ! Perform the check for UV divergence
    if (arguments(1) == 1) then
        print *, "Checking for UV divergences ..."

        ! Use this hack to "fill up" the string to the maximum length with whitespace characters so that it can be passed to the subroutine call
        fileName = fileName // ' '

        ! Get all parameters
        call getParameters(fileName)

        ! Set the 2HDM parameters according to the first point in phase-space (random choice)
        MHH = MHHList(1)
        MA0 = MA0List(1)
        MHp = MHpList(1)
        MHH = MHHList(1)
        alpha = alphaList(1)
        beta = betaList(1)
        CA = CAList(1)
        SA = SAList(1)
        TA = TAList(1)
        CB = CBList(1)
        SB = SBList(1)
        TB = TBList(1)
        S2A = S2AList(1)
        C2A = C2AList(1)
        S2B = S2BList(1)
        C2B = C2BList(1)
        CAB = CABList(1)
        SAB = SABList(1)
        CBA = CBAList(1)
        SBA = SBAList(1)
        Yuk1 = Yuk1List(1)
        Yuk2 = Yuk2List(1)
        Yuk3 = Yuk3List(1)
        Yuk4 = Yuk4List(1)
        Yuk5 = Yuk5List(1)
        Yuk6 = Yuk6List(1)
        m12squared = m12squaredList(1)
        Lambda5 = Lambda5List(1)

        ! Calculate the square of the 2HDM input parameters
        MHH2 = MHH**2
        MA02 = MA0**2
        MHp2 = MHp**2
        CA2 = CA**2
        SA2 = SA**2
        TA2 = TA**2
        TB2 = TB**2
        SB2 = SB**2
        CB2 = CB**2
        C2A2 = C2A**2
        S2A2 = S2A**2
        C2B2 = C2B**2
        S2B2 = S2B**2
        CAB2 = CAB**2
        SAB2 = SAB**2
        CBA2 = CBA**2
        SBA2 = SBA**2

        call ltini
            call setmudim(1D0)
            call setlambda(1D0)
            call setdelta(0D0)
            IRLambda = getlambda()

            ! Set the UV scale 
            UVDelta = getdelta()

            ! Perform the check for UV divergence for all schemes
            do n = 1, maxNumberSchemes, 1
                ! Reset the checking arrays for all schemes
                isUVDivergentList(n) = .false.
                isUVDivergent = .false.

                ! Info header
                print *, " "
                print *, "==========="
                print *, "UV divergence of scheme ", n
                print *, "==========="

                ! Calculate the amplitude for several different Delta values
                ! Schemes 1 and 2 are without tadpoles
                if ((n == 1) .OR. (n == 2)) then
                    UVChecks(1) = DBLE(PLACEHOLDERPROCESSVC()) + PLACEHOLDERPROCESSCT(n)
                else
                    UVChecks(1) = DBLE(PLACEHOLDERPROCESSVC() + PLACEHOLDERPROCESSTad()) + &
                                & PLACEHOLDERPROCESSCT(n)
                end if
                do m = 2, 10, 1
                    call clearcache
                    call setdelta(DBLE(m-1))
                    UVDelta = getdelta()
                    ! Schemes 1 and 2 are without tadpoles
                    if ((n == 1) .OR. (n == 2)) then
                        UVChecks(m) = ( DBLE(PLACEHOLDERPROCESSVC()) + PLACEHOLDERPROCESSCT(n) - &
                                    & UVChecks(1) )/UVChecks(1)
                    else
                        UVChecks(m) = ( DBLE(PLACEHOLDERPROCESSVC() + PLACEHOLDERPROCESSTad()) + &
                                    & PLACEHOLDERPROCESSCT(n) - UVChecks(1) &
                                    & )/UVChecks(1)
                    end if
                    print *, "Delta: ", UVDelta, ", Difference from Delta=0: ", UVChecks(m)
                    if (abs(UVChecks(m)) > UVDivergenceThreshold) then
                        isUVDivergent = .true.
                        isUVDivergentList(n) = .true.
                    end if
                end do
                call clearcache
                call setdelta(0D0)
                UVDelta = getdelta()
                print *, "==========="

                ! Exception handling: if UV divergences are found, inform the user and ask if the program shall be terminated
                if (isUVDivergent) then
                    write (*, '(/,A)') "WARNING: potential UV divergence found! Please check the vertex corrections and &
                        &the counterterm!"
                    do
                        print *, ">>> Do you want to continue with the evaluation of the program? [y/n]"
                        read (*,*) isUVDivergentContinue
                        if (isUVDivergentContinue == 'n') then
                            print *, "Termination requested by user. 2HDMCalc will be terminated now."
                            stop
                        else if (isUVDivergentContinue == 'y') then
                            exit
                        else
                            print *, "Invalid character. Enter y or n."
                        end if
                    end do
                end if
            end do
        call ltexi

        ! Print the summary of the checks for UV divergence
        print *, "==========="
        print *, "Results of the check on UV divergence"
        print *, "==========="
        do n = 1, maxNumberSchemes, 1
            if (isUVDivergentList(n)) then
                print *, "Scheme ", n, ": potentially UV-divergent."
            else
                print *, "Scheme ", n, ": most likely UV-finite."
            end if
        end do
        print *, "==========="

    end if

    ! Perform the check for IR divergence
    if (arguments(2) == 1) then
        print *, "Checking for IR divergences ..."

        ! Use this hack to "fill up" the string to the maximum length with whitespace characters so that it can be passed to the subroutine call
        fileName = fileName // ' '

        ! Get all parameters
        call getParameters(fileName)

        ! Set the 2HDM parameters according to the first point in phase-space (random choice)
        MHH = MHHList(1)
        MA0 = MA0List(1)
        MHp = MHpList(1)
        MHH = MHHList(1)
        alpha = alphaList(1)
        beta = betaList(1)
        CA = CAList(1)
        SA = SAList(1)
        TA = TAList(1)
        CB = CBList(1)
        SB = SBList(1)
        TB = TBList(1)
        S2A = S2AList(1)
        C2A = C2AList(1)
        S2B = S2BList(1)
        C2B = C2BList(1)
        CAB = CABList(1)
        SAB = SABList(1)
        CBA = CBAList(1)
        SBA = SBAList(1)
        Yuk1 = Yuk1List(1)
        Yuk2 = Yuk2List(1)
        Yuk3 = Yuk3List(1)
        Yuk4 = Yuk4List(1)
        Yuk5 = Yuk5List(1)
        Yuk6 = Yuk6List(1)
        m12squared = m12squaredList(1)
        Lambda5 = Lambda5List(1)

        ! Calculate the square of the 2HDM input parameters
        MHH2 = MHH**2
        MA02 = MA0**2
        MHp2 = MHp**2
        CA2 = CA**2
        SA2 = SA**2
        TA2 = TA**2
        TB2 = TB**2
        SB2 = SB**2
        CB2 = CB**2
        C2A2 = C2A**2
        S2A2 = S2A**2
        C2B2 = C2B**2
        S2B2 = S2B**2
        CAB2 = CAB**2
        SAB2 = SAB**2
        CBA2 = CBA**2
        SBA2 = SBA**2

        ! Calculate the full amplitude for different lambda
        call ltini
            call setdelta(0D0)
            call setmudim(1D0)
            call setlambda(1D0)
            call clearcache
            IRLambda = getlambda()

            ! Reset the UV scale 
            UVDelta = 0D0

            ! Kinematic prefactor together with the symmetry factor of the process
            prefactor = 1D0/PLACEHOLDERSYMMETRYD0 * DSQRT(PLACEHOLDERKINEMATIC1 &
                        & PLACEHOLDERKINEMATIC2 )/(16D0*PI*PLACEHOLDERKINEMATIC3**3)

            ! Perform the check for gauge dependence for all schemes
            do n = 1, maxNumberSchemes, 1
                ! Reset the checking arrays for all schemes
                isIRDivergentList(n) = .false.
                isIRDivergent = .false.

                ! Info header
                print *, " "
                print *, "==========="
                print *, "IR divergence of scheme ", n
                print *, "==========="

                ! Calculate the amplitude for several different lambda values
                ! Schemes 1 and 2 are without tadpoles
                if ((n == 1) .OR. (n == 2)) then
                    IRChecks(1) = prefactor*( 2D0*DBLE(PLACEHOLDERPROCESSVC()) + &
                                & 2D0*PLACEHOLDERPROCESSCT(n) + &
                                & PLACEHOLDERPROCESSReal() )
                else
                    IRChecks(1) = prefactor*( 2D0*DBLE(PLACEHOLDERPROCESSVC() + &
                                & PLACEHOLDERPROCESSTad()) + 2D0*PLACEHOLDERPROCESSCT(n) + &
                                & PLACEHOLDERPROCESSReal() )
                end if
                do m = 2, 10, 1
                    call clearcache
                    call setlambda(DBLE(10**(m-1)))
                    IRLambda = getlambda()
                    ! Schemes 1 and 2 are without tadpoles
                    if ((n == 1) .OR. (n == 2)) then
                        IRChecks(m) = ( prefactor*( 2D0*DBLE(PLACEHOLDERPROCESSVC()) + &
                                    & 2D0*PLACEHOLDERPROCESSCT(n) + PLACEHOLDERPROCESSReal() ) &
                                    & - IRChecks(1))/IRChecks(1)
                    else
                        IRChecks(m) = ( prefactor*( 2D0*DBLE(PLACEHOLDERPROCESSVC() + &
                                    & PLACEHOLDERPROCESSTad()) + 2D0*PLACEHOLDERPROCESSCT(n) + &
                                    & PLACEHOLDERPROCESSReal() ) - IRChecks(1))/IRChecks(1)
                    end if
                    print *, "Lambda: ", getlambda(), ", Difference from lambda=1: ", IRChecks(m)
                    print *, "Real: ", PLACEHOLDERPROCESSReal()
                    if (abs(IRChecks(m)) > IRDivergenceThreshold) then
                        isIRDivergent = .true.
                        isIRDivergentList(n) = .true.
                    end if
                end do
                call clearcache
                call setlambda(1D0)
                IRLambda = getlambda()
                print *, "==========="

                ! Exception handling: if IR divergences are found, inform the user and ask if the program shall be terminated
                if (isIRDivergent) then
                    write (*, '(/,A)') "WARNING: potential IR divergence found! Please check the vertex corrections, &
                        &the counterterm and the real corrections!"
                    do
                        print *, ">>> Do you want to continue with the evaluation of the program? [y/n]"
                        read (*,*) isIRDivergentContinue
                        if (isIRDivergentContinue == 'n') then
                            print *, "Termination requested by user. 2HDMCalc will be terminated now."
                            stop
                        else if (isIRDivergentContinue == 'y') then
                            exit
                        else
                            print *, "Invalid character. Enter y or n."
                        end if
                    end do
                end if
            end do
        call ltexi

        ! Print the summary of the checks for IR divergence
        print *, "==========="
        print *, "Results of the check on IR divergence"
        print *, "==========="
        do n = 1, maxNumberSchemes, 1
            if (isIRDivergentList(n)) then
                print *, "Scheme ", n, ": potentially IR-divergent."
            else
                print *, "Scheme ", n, ": most likely IR-finite."
            end if
        end do
        print *, "==========="

    end if

    ! Perform the check for gauge dependence
    if (arguments(3) >= 1) then
        print *, "Checking for gauge dependences ..."

        ! Use this hack to "fill up" the string to the maximum length with whitespace characters so that it can be passed to the subroutine call
        fileName = fileName // ' '

        ! Get all parameters
        call getParameters(fileName)

        ! Set the 2HDM parameters according to the first point in phase-space (random choice)
        MHH = MHHList(1)
        MA0 = MA0List(1)
        MHp = MHpList(1)
        MHH = MHHList(1)
        alpha = alphaList(1)
        beta = betaList(1)
        CA = CAList(1)
        SA = SAList(1)
        TA = TAList(1)
        CB = CBList(1)
        SB = SBList(1)
        TB = TBList(1)
        S2A = S2AList(1)
        C2A = C2AList(1)
        S2B = S2BList(1)
        C2B = C2BList(1)
        CAB = CABList(1)
        SAB = SABList(1)
        CBA = CBAList(1)
        SBA = SBAList(1)
        Yuk1 = Yuk1List(1)
        Yuk2 = Yuk2List(1)
        Yuk3 = Yuk3List(1)
        Yuk4 = Yuk4List(1)
        Yuk5 = Yuk5List(1)
        Yuk6 = Yuk6List(1)
        m12squared = m12squaredList(1)
        Lambda5 = Lambda5List(1)

        ! Calculate the square of the 2HDM input parameters
        MHH2 = MHH**2
        MA02 = MA0**2
        MHp2 = MHp**2
        CA2 = CA**2
        SA2 = SA**2
        TA2 = TA**2
        TB2 = TB**2
        SB2 = SB**2
        CB2 = CB**2
        C2A2 = C2A**2
        S2A2 = S2A**2
        C2B2 = C2B**2
        S2B2 = S2B**2
        CAB2 = CAB**2
        SAB2 = SAB**2
        CBA2 = CBA**2
        SBA2 = SBA**2

        ! Calculate the full amplitude for different gauge-fixing parameters
        call ltini
            ! Reset all settings
            call setdelta(0D0)
            call setmudim(1D0)
            call setlambda(1D0)
            call clearcache
            IRLambda = getlambda()

            ! Reset the gauge-fixing parameters
            GaugeXiA = 1.0D0
            GaugeXiW = 1.0D0
            GaugeXiZ = 1.0D0

            ! Reset the UV scale 
            UVDelta = 0D0

            ! Perform the check for gauge dependence for all schemes
            do n = 1, maxNumberSchemes, 1
                ! Reset the checking arrays for all schemes
                isGaugeDependentAList(n) = .false.
                isGaugeDependentWList(n) = .false.
                isGaugeDependentZList(n) = .false.
                isGaugeDependent = .false.

                ! Values for all gauge-fixing parameters = 1 for comparison
                ! Schemes 1 and 2 are without tadpoles
                if ((n == 1) .OR. (n == 2)) then
                    call clearcache
                    GaugeXiAChecks(5) = PLACEHOLDERPROCESSTree() + &
                        & 2D0*DBLE(PLACEHOLDERPROCESSVC()) + PLACEHOLDERPROCESSReal() + &
                        & 2D0*PLACEHOLDERPROCESSCT(n)
                    call clearcache
                    GaugeXiWChecks(5) = PLACEHOLDERPROCESSTree() + &
                        & 2D0*DBLE(PLACEHOLDERPROCESSVC()) + PLACEHOLDERPROCESSReal() + &
                        & 2D0*PLACEHOLDERPROCESSCT(n)
                    call clearcache
                    GaugeXiZChecks(5) = PLACEHOLDERPROCESSTree() + &
                        & 2D0*DBLE(PLACEHOLDERPROCESSVC()) + PLACEHOLDERPROCESSReal() + &
                        & 2D0*PLACEHOLDERPROCESSCT(n)
                else
                    call clearcache
                    GaugeXiAChecks(5) = PLACEHOLDERPROCESSTree() + &
                        & 2D0*DBLE(PLACEHOLDERPROCESSVC() + PLACEHOLDERPROCESSTad()) + &
                        & PLACEHOLDERPROCESSReal() + 2D0*PLACEHOLDERPROCESSCT(n)
                    call clearcache
                    GaugeXiWChecks(5) = PLACEHOLDERPROCESSTree() + &
                        & 2D0*DBLE(PLACEHOLDERPROCESSVC() + PLACEHOLDERPROCESSTad()) + &
                        & PLACEHOLDERPROCESSReal() + 2D0*PLACEHOLDERPROCESSCT(n)
                    call clearcache
                    GaugeXiZChecks(5) = PLACEHOLDERPROCESSTree() + &
                        & 2D0*DBLE(PLACEHOLDERPROCESSVC() + PLACEHOLDERPROCESSTad()) + &
                        & PLACEHOLDERPROCESSReal() + 2D0*PLACEHOLDERPROCESSCT(n)
                end if

                ! Info header
                print *, " "
                print *, "==========="
                print *, "Gauge-dependence check of scheme ", n
                print *, "==========="

                ! GaugeXiA check
                do m = 1, 10, 1
                    if (m == 5) cycle

                    if (m < 6) then
                        GaugeXiA = 2D0*DBLE(m)/10D0
                    else
                        GaugeXiA = 10D0**(m-5)
                    end if
                    call clearcache

                    ! Schemes 1 and 2 are without tadpoles
                    if ((n == 1) .OR. (n == 2)) then
                        GaugeXiAChecks(m) = ( PLACEHOLDERPROCESSTree() + &
                            & 2D0*DBLE(PLACEHOLDERPROCESSVC()) + 2D0*PLACEHOLDERPROCESSCT(n) + &
                            & PLACEHOLDERPROCESSReal() - GaugeXiAChecks(5) )/GaugeXiAChecks(5)
                    else
                        GaugeXiAChecks(m) = ( PLACEHOLDERPROCESSTree() + &
                            & 2D0*DBLE(PLACEHOLDERPROCESSVC() + PLACEHOLDERPROCESSTad()) + &
                            & 2D0*PLACEHOLDERPROCESSCT(n) + PLACEHOLDERPROCESSReal() - &
                            & GaugeXiAChecks(5) )/GaugeXiAChecks(5)
                    end if

                    print *, "GaugeXiA: ", GaugeXiA, ", Difference from GaugeXiA=1: ", GaugeXiAChecks(m)
                    if (abs(GaugeXiAChecks(m)) > GaugeDependenceThreshold) then
                        isGaugeDependent = .true.
                        isGaugeDependentAList(n) = .true.
                    end if
                end do
                GaugeXiA = 1D0
                print *, "==========="

                ! GaugeXiW check
                do m = 1, 10, 1
                    if (m == 5) cycle

                    if (m < 6) then
                        GaugeXiW = 2D0*DBLE(m)/10D0
                    else
                        GaugeXiW = 10D0**(m-5)
                    end if
                    call clearcache

                    ! Schemes 1 and 2 are without tadpoles
                    if ((n == 1) .OR. (n == 2)) then
                        GaugeXiWChecks(m) = ( PLACEHOLDERPROCESSTree() + &
                            & 2D0*DBLE(PLACEHOLDERPROCESSVC()) + 2D0*PLACEHOLDERPROCESSCT(n) + &
                            & PLACEHOLDERPROCESSReal() - GaugeXiWChecks(5) )/GaugeXiWChecks(5)
                    else
                        GaugeXiWChecks(m) = ( PLACEHOLDERPROCESSTree() + &
                            & 2D0*DBLE(PLACEHOLDERPROCESSVC() + PLACEHOLDERPROCESSTad()) + &
                            & 2D0*PLACEHOLDERPROCESSCT(n) + PLACEHOLDERPROCESSReal() - &
                            & GaugeXiWChecks(5) )/GaugeXiWChecks(5)
                    end if

                    print *, "GaugeXiW: ", GaugeXiW, ", Difference from GaugeXiW=1: ", GaugeXiWChecks(m)
                    if (abs(GaugeXiWChecks(m)) > GaugeDependenceThreshold) then
                        isGaugeDependent = .true.
                        isGaugeDependentWList(n) = .true.
                    end if
                end do
                GaugeXiW = 1D0
                print *, "==========="

                ! GaugeXiZ check
                do m = 1, 10, 1
                    if (m == 5) cycle

                    if (m < 6) then
                        GaugeXiZ = 2D0*DBLE(m)/10D0
                    else
                        GaugeXiZ = 10D0**(m-5)
                    end if
                    call clearcache

                    ! Schemes 1 and 2 are without tadpoles
                    if ((n == 1) .OR. (n == 2)) then
                        GaugeXiZChecks(m) = ( PLACEHOLDERPROCESSTree() + &
                            & 2D0*DBLE(PLACEHOLDERPROCESSVC()) + 2D0*PLACEHOLDERPROCESSCT(n) + &
                            & PLACEHOLDERPROCESSReal() - GaugeXiZChecks(5) )/GaugeXiZChecks(5)
                    else
                        GaugeXiZChecks(m) = ( PLACEHOLDERPROCESSTree() + &
                            & 2D0*DBLE(PLACEHOLDERPROCESSVC() + PLACEHOLDERPROCESSTad()) + &
                            & 2D0*PLACEHOLDERPROCESSCT(n) + PLACEHOLDERPROCESSReal() - &
                            & GaugeXiZChecks(5) )/GaugeXiZChecks(5)
                    end if

                    print *, "GaugeXiZ: ", GaugeXiZ, ", Difference from GaugeXiZ=1: ", GaugeXiZChecks(m)
                    if (abs(GaugeXiZChecks(m)) > GaugeDependenceThreshold) then
                        isGaugeDependent = .true.
                        isGaugeDependentZList(n) = .true.
                    end if
                end do
                GaugeXiZ = 1D0

                ! Exception handling: if gauge dependences are found, inform the user and ask if the program shall be terminated
                if (isGaugeDependent .AND. (arguments(3) == 2)) then
                    print *, "WARNING: potential gauge dependence in scheme ", n, " found! Please check the vertex corrections &
                        &and the counterterm!"
                    do
                        print *, ">>> Do you want to continue with the evaluation of the program? [y/n]"
                        read (*,*) isGaugeDependentContinue
                        if (isGaugeDependentContinue == 'n') then
                            print *, "Termination requested by user. 2HDMCalc will be terminated now."
                            stop
                        else if (isGaugeDependentContinue == 'y') then
                            exit
                        else
                            print *, "Invalid character. Enter y or n."
                        end if
                    end do
                    isGaugeDependent = .false.
                end if

                call clearcache
            end do

        call ltexi

        ! Print the summary of the gauge-dependence checks for GaugeXiA
        print *, "==========="
        print *, "Results of the check on gauge-dependence"
        print *, "==========="
        print *, "Gauge-Dependence on GaugeXiA: "
        print *, "-----------"
        do n = 1, maxNumberSchemes, 1
            if (isGaugeDependentAList(n)) then
                print *, "Scheme ", n, ": potentially gauge-dependent."
            else
                print *, "Scheme ", n, ": most likely gauge-independent."
            end if
        end do

        ! Print the summary of the gauge-dependence checks for GaugeXiW
        print *, "==========="
        print *, "Gauge-Dependence on GaugeXiW: "
        print *, "-----------"
        do n = 1, maxNumberSchemes, 1
            if (isGaugeDependentWList(n)) then
                print *, "Scheme ", n, ": potentially gauge-dependent."
            else
                print *, "Scheme ", n, ": most likely gauge-independent."
            end if
        end do

        ! Print the summary of the gauge-dependence checks for GaugeXiZ
        print *, "==========="
        print *, "Gauge-Dependence on GaugeXiZ: "
        print *, "-----------"
        do n = 1, maxNumberSchemes, 1
            if (isGaugeDependentZList(n)) then
                print *, "Scheme ", n, ": potentially gauge-dependent."
            else
                print *, "Scheme ", n, ": most likely gauge-independent."
            end if
        end do
        print *, "==========="

    end if

    ! Perform the numerical evaluation
    if (arguments(4) == 1) then
        print *, "Starting the numerical evaluation ..."

        ! Reset all values
        GaugeXiA = 1D0
        GaugeXiW = 1D0
        GaugeXiZ = 1D0

        ! Calculate all values
        call ltini
            ! Set default values for the loop calculations
            call setlambda(1D0)
            call setdelta(0D0)
            IRLambda = getlambda()

            ! Use this hack to "fill up" the string to the maximum length with whitespace characters so that it can be passed to the subroutine call
            fileName = fileName // ' '
            targetName = targetName // ' '

            ! Get all parameters
            call getParameters(fileName)
            call setmudim(InputScale**2)

            ! Prepare the output file header
            outputFileContent = "MHH,"
            outputFileContent = trim(outputFileContent) // "Mh0,"
            outputFileContent = trim(outputFileContent) // "MA0,"
            outputFileContent = trim(outputFileContent) // "MHp,"
            outputFileContent = trim(outputFileContent) // "alpha,"
            outputFileContent = trim(outputFileContent) // "beta,"
            outputFileContent = trim(outputFileContent) // "m122,"
            outputFileContent = trim(outputFileContent) // "Lambda5,"
            outputFileContent = trim(outputFileContent) // "2HDMType,"
            outputFileContent = trim(outputFileContent) // "InputScale,"
            outputFileContent = trim(outputFileContent) // "WidthLO,"
            if (debugModeOn) then
                outputFileContent = trim(outputFileContent) // "WidthNLOVC,"
                outputFileContent = trim(outputFileContent) // "WidthNLOVCwoIR,"
                outputFileContent = trim(outputFileContent) // "WidthIRonly,"
                outputFileContent = trim(outputFileContent) // "dMW2Usual,"
                outputFileContent = trim(outputFileContent) // "dMW2Alter,"
                outputFileContent = trim(outputFileContent) // "dMZ2Usual,"
                outputFileContent = trim(outputFileContent) // "dMZ2Alter,"
                outputFileContent = trim(outputFileContent) // "dMLOSUsual,"
                outputFileContent = trim(outputFileContent) // "dMLOSAlter,"
                outputFileContent = trim(outputFileContent) // "dMBOSUsual,"
                outputFileContent = trim(outputFileContent) // "dMBOSAlter,"
                outputFileContent = trim(outputFileContent) // "dZHHHHOS,"
                outputFileContent = trim(outputFileContent) // "dZHHh0OSUsual,"
                outputFileContent = trim(outputFileContent) // "dZHHh0OSAlter,"
                outputFileContent = trim(outputFileContent) // "dZh0HHOSUsual,"
                outputFileContent = trim(outputFileContent) // "dZh0HHOSAlter,"
                outputFileContent = trim(outputFileContent) // "dZh0h0OS,"
                outputFileContent = trim(outputFileContent) // "dZA0A0OS,"
                outputFileContent = trim(outputFileContent) // "dZG0A0OSUsual,"
                outputFileContent = trim(outputFileContent) // "dZG0A0OSAlter,"
                outputFileContent = trim(outputFileContent) // "dZBotBotOSLeft,"
                outputFileContent = trim(outputFileContent) // "dZBotBotOSRight,"
                outputFileContent = trim(outputFileContent) // "dZTauTauOSLeft,"
                outputFileContent = trim(outputFileContent) // "dZTauTauOSRight,"
                outputFileContent = trim(outputFileContent) // "dBeta1KanUsual,"
                outputFileContent = trim(outputFileContent) // "dBeta1KanAlter,"
                outputFileContent = trim(outputFileContent) // "dBeta1PinchPStar,"
                outputFileContent = trim(outputFileContent) // "dBeta1PinchOS,"
            end if
            outputFileContent = trim(outputFileContent) // "WidthKanOdd,"
            outputFileContent = trim(outputFileContent) // "WidthKanChar,"
            outputFileContent = trim(outputFileContent) // "WidthTadOdd,"
            outputFileContent = trim(outputFileContent) // "WidthTadChar,"
            outputFileContent = trim(outputFileContent) // "WidthPStarOdd,"
            outputFileContent = trim(outputFileContent) // "WidthPStarChar,"
            outputFileContent = trim(outputFileContent) // "WidthOSPinOdd,"
            outputFileContent = trim(outputFileContent) // "WidthOSPinChar,"
            outputFileContent = trim(outputFileContent) // "WidthUsuProcDep1,"
            outputFileContent = trim(outputFileContent) // "WidthAltProcDep1,"
            outputFileContent = trim(outputFileContent) // "WidthUsuProcDep2,"
            outputFileContent = trim(outputFileContent) // "WidthAltProcDep2,"
            outputFileContent = trim(outputFileContent) // "WidthUsuProcDep3,"
            outputFileContent = trim(outputFileContent) // "WidthAltProcDep3,"
            outputFileContent = trim(outputFileContent) // "DifWidthLO,"
            outputFileContent = trim(outputFileContent) // "DifWidthKanOdd,"
            outputFileContent = trim(outputFileContent) // "DifWidthKanChar,"
            outputFileContent = trim(outputFileContent) // "DifWidthTadOdd,"
            outputFileContent = trim(outputFileContent) // "DifWidthTadChar,"
            outputFileContent = trim(outputFileContent) // "DifWidthPStarOdd,"
            outputFileContent = trim(outputFileContent) // "DifWidthPStarChar,"
            outputFileContent = trim(outputFileContent) // "DifWidthOSPinOdd,"
            outputFileContent = trim(outputFileContent) // "DifWidthOSPinChar,"
            outputFileContent = trim(outputFileContent) // "DifWidthUsuProcDep1,"
            outputFileContent = trim(outputFileContent) // "DifWidthAltProcDep1,"
            outputFileContent = trim(outputFileContent) // "DifWidthUsuProcDep2,"
            outputFileContent = trim(outputFileContent) // "DifWidthAltProcDep2,"
            outputFileContent = trim(outputFileContent) // "DifWidthUsuProcDep3,"
            outputFileContent = trim(outputFileContent) // "DifWidthAltProcDep3\n"

            do point = 1, maxPoint, 1
                ! Set the 2HDM parameters according to the current point in phase-space
                MHH = MHHList(point)
                MA0 = MA0List(point)
                MHp = MHpList(point)
                MHH = MHHList(point)
                alpha = alphaList(point)
                beta = betaList(point)
                CA = CAList(point)
                SA = SAList(point)
                TA = TAList(point)
                CB = CBList(point)
                SB = SBList(point)
                TB = TBList(point)
                S2A = S2AList(point)
                C2A = C2AList(point)
                S2B = S2BList(point)
                C2B = C2BList(point)
                CAB = CABList(point)
                SAB = SABList(point)
                CBA = CBAList(point)
                SBA = SBAList(point)
                Yuk1 = Yuk1List(point)
                Yuk2 = Yuk2List(point)
                Yuk3 = Yuk3List(point)
                Yuk4 = Yuk4List(point)
                Yuk5 = Yuk5List(point)
                Yuk6 = Yuk6List(point)
                m12squared = m12squaredList(point)
                Lambda5 = Lambda5List(point)
                TypeOf2HDM = TypeOf2HDMList(point)

                ! Print out the current point in phase-space (debug mode only)
                if (debugModeOn) then
                    write (*,*) "MW: ", MW
                    write (*,*) "MZ: ", MZ
                    write (*,*) "SW: ", SW
                    write (*,*) "CW: ", CW
                    write (*,*) "EL: ", EL
                    write (*,*) "vev: ", (2D0*MW*SW/EL)
                    write (*,*) "ME: ", ME
                    write (*,*) "MM: ", MM
                    write (*,*) "ML: ", ML
                    write (*,*) "MU: ", MU
                    write (*,*) "MD: ", MD
                    write (*,*) "MS: ", MS
                    write (*,*) "MC: ", MC
                    write (*,*) "MB: ", MB
                    write (*,*) "MT: ", MT
                    write (*,*) "Mh0: ", Mh0
                    write (*,*) "MHH: ", MHH
                    write (*,*) "MA0: ", MA0
                    write (*,*) "MHp: ", MHp
                    write (*,*) "alpha: ", alpha
                    write (*,*) "beta: ", beta
                    write (*,*) "CA: ", CA
                    write (*,*) "CB: ", CB
                    write (*,*) "Yuk1: ", Yuk1
                    write (*,*) "Yuk2: ", Yuk2
                    write (*,*) "Yuk3: ", Yuk3
                    write (*,*) "Yuk4: ", Yuk4
                    write (*,*) "Yuk5: ", Yuk5
                    write (*,*) "Yuk6: ", Yuk6
                    write (*,*) "m12squared: ", m12squared
                    write (*,*) "2HDM Type: ", TypeOf2HDM
                    write (*,*) "InputScale: ", InputScale
                end if

                ! Calculate the square of the 2HDM input parameters
                MHH2 = MHH**2
            	MA02 = MA0**2
            	MHp2 = MHp**2
                CA2 = CA**2
            	SA2 = SA**2
            	TA2 = TA**2
            	TB2 = TB**2
            	SB2 = SB**2
            	CB2 = CB**2
            	C2A2 = C2A**2
            	S2A2 = S2A**2
            	C2B2 = C2B**2
            	S2B2 = S2B**2
            	CAB2 = CAB**2
            	SAB2 = SAB**2
            	CBA2 = CBA**2
            	SBA2 = SBA**2

                ! Kinematic prefactor together with the symmetry factor of the process
                prefactor = 1D0/PLACEHOLDERSYMMETRYD0 * DSQRT(PLACEHOLDERKINEMATIC1 &
                            & PLACEHOLDERKINEMATIC2 )/(16D0*PI*PLACEHOLDERKINEMATIC3**3)

                ! Get the full tree-level decay width
                call clearcache
                treeLevelWidth = prefactor*PLACEHOLDERPROCESSTree()

                ! Calculate the NLO ingredients
                treeLevelTemp = PLACEHOLDERPROCESSTree()
                vertexCorrectionsTemp = PLACEHOLDERPROCESSVC()
                vertexTadpolesTemp = PLACEHOLDERPROCESSTad()
                realCorrectionsTemp = PLACEHOLDERPROCESSReal()

                ! Calculate the NLO width w/o counterterm contributions (debug mode only)
                if (debugModeOn) then
                    call clearcache
                    NLOVCwidth = prefactor*( 2D0*DBLE(vertexCorrectionsTemp) + realCorrectionsTemp )
                    call clearcache
                    NLOVCwoIRwidth = prefactor*( 2D0*DBLE(vertexCorrectionsTemp) )
                    NLOIRonlywidth = prefactor*( realCorrectionsTemp )
                end if

                ! Get the full NLO decay width for all schemes
                do m = 1, maxNumberSchemes, 1
                    call clearcache

                    ! Schemes 1 and 2 are without tadpoles
                    if ((m == 1) .OR. (m == 2)) then
                        fullamplitude(m) = treeLevelTemp + 2D0*DBLE(vertexCorrectionsTemp) + &
                                            & 2D0*PLACEHOLDERPROCESSCT(m)
                        call clearcache
                        NLOWidth(m) = prefactor*( fullamplitude(m) + realCorrectionsTemp )
                    else
                        fullamplitude(m) = treeLevelTemp + 2D0*DBLE(vertexCorrectionsTemp + vertexTadpolesTemp) + &
                                            & 2D0*PLACEHOLDERPROCESSCT(m)
                        call clearcache
                        NLOWidth(m) = prefactor*( fullamplitude(m) + realCorrectionsTemp )
                    end if
                end do

                ! Write the results to the output string
                ! Format: we use 18 characters in total for double precision. 3 are reserved for the exponent, 1 for the sign of the exponent,
                ! 1 for the symbol E denoting the exponent, 1 for the dot, 1 for a possible negative sign, 1 for the digit before the comma
                ! and 10 for the digits after the comma
                write( tempVal, '(ES18.10E3)' ) MHH
                outputFileContent = trim(outputFileContent) // (trim(tempVal) // ",")
                write( tempVal, '(ES18.10E3)' ) Mh0
                outputFileContent = trim(outputFileContent) // (trim(tempVal) // ",")
                write( tempVal, '(ES18.10E3)' ) MA0
                outputFileContent = trim(outputFileContent) // (trim(tempVal) // ",")
                write( tempVal, '(ES18.10E3)' ) MHp
                outputFileContent = trim(outputFileContent) // (trim(tempVal) // ",")
                write( tempVal, '(ES18.10E3)' ) alpha
                outputFileContent = trim(outputFileContent) // (trim(tempVal) // ",")
                write( tempVal, '(ES18.10E3)' ) beta
                outputFileContent = trim(outputFileContent) // (trim(tempVal) // ",")
                write( tempVal, '(ES18.10E3)' ) m12squared
                outputFileContent = trim(outputFileContent) // (trim(tempVal) // ",")
                write( tempVal, '(ES18.10E3)' ) Lambda5
                outputFileContent = trim(outputFileContent) // (trim(tempVal) // ",")
                write( tempVal, '(I1)' ) TypeOf2HDM
                outputFileContent = trim(outputFileContent) // (trim(tempVal) // ",")
                write( tempVal, '(ES18.10E3)' ) InputScale
                outputFileContent = trim(outputFileContent) // (trim(tempVal) // ",")
                write( tempVal, '(ES18.10E3)' ) treeLevelWidth
                outputFileContent = trim(outputFileContent) // (trim(tempVal) // ",")
                if (debugModeOn) then
                    write( tempVal, '(ES18.10E3)' ) NLOVCwidth
                    outputFileContent = trim(outputFileContent) // (trim(tempVal) // ",")
                    write( tempVal, '(ES18.10E3)' ) NLOVCwoIRwidth
                    outputFileContent = trim(outputFileContent) // (trim(tempVal) // ",")
                    write( tempVal, '(ES18.10E3)' ) NLOIRonlywidth
                    outputFileContent = trim(outputFileContent) // (trim(tempVal) // ",")
                    write( tempVal, '(ES18.10E3)' ) dMW2Usual()
                    outputFileContent = trim(outputFileContent) // (trim(tempVal) // ",")
                    write( tempVal, '(ES18.10E3)' ) dMW2Alter()
                    outputFileContent = trim(outputFileContent) // (trim(tempVal) // ",")
                    write( tempVal, '(ES18.10E3)' ) dMZ2Usual()
                    outputFileContent = trim(outputFileContent) // (trim(tempVal) // ",")
                    write( tempVal, '(ES18.10E3)' ) dMZ2Alter()
                    outputFileContent = trim(outputFileContent) // (trim(tempVal) // ",")
                    write( tempVal, '(ES18.10E3)' ) dMLOSUsual()
                    outputFileContent = trim(outputFileContent) // (trim(tempVal) // ",")
                    write( tempVal, '(ES18.10E3)' ) dMLOSAlter()
                    outputFileContent = trim(outputFileContent) // (trim(tempVal) // ",")
                    write( tempVal, '(ES18.10E3)' ) dMBOSUsual()
                    outputFileContent = trim(outputFileContent) // (trim(tempVal) // ",")
                    write( tempVal, '(ES18.10E3)' ) dMBOSAlter()
                    outputFileContent = trim(outputFileContent) // (trim(tempVal) // ",")
                    write( tempVal, '(ES18.10E3)' ) dZHHHHOS()
                    outputFileContent = trim(outputFileContent) // (trim(tempVal) // ",")
                    write( tempVal, '(ES18.10E3)' ) dZHHh0OSUsual()
                    outputFileContent = trim(outputFileContent) // (trim(tempVal) // ",")
                    write( tempVal, '(ES18.10E3)' ) dZHHh0OSAlter()
                    outputFileContent = trim(outputFileContent) // (trim(tempVal) // ",")
                    write( tempVal, '(ES18.10E3)' ) dZh0HHOSUsual()
                    outputFileContent = trim(outputFileContent) // (trim(tempVal) // ",")
                    write( tempVal, '(ES18.10E3)' ) dZh0HHOSAlter()
                    outputFileContent = trim(outputFileContent) // (trim(tempVal) // ",")
                    write( tempVal, '(ES18.10E3)' ) dZh0h0OS()
                    outputFileContent = trim(outputFileContent) // (trim(tempVal) // ",")
                    write( tempVal, '(ES18.10E3)' ) dZA0A0OS()
                    outputFileContent = trim(outputFileContent) // (trim(tempVal) // ",")
                    write( tempVal, '(ES18.10E3)' ) dZG0A0OSUsual()
                    outputFileContent = trim(outputFileContent) // (trim(tempVal) // ",")
                    write( tempVal, '(ES18.10E3)' ) dZG0A0OSAlter()
                    outputFileContent = trim(outputFileContent) // (trim(tempVal) // ",")
                    write( tempVal, '(ES18.10E3)' ) dZTauTauOSLeft()
                    outputFileContent = trim(outputFileContent) // (trim(tempVal) // ",")
                    write( tempVal, '(ES18.10E3)' ) dZTauTauOSRight()
                    outputFileContent = trim(outputFileContent) // (trim(tempVal) // ",")
                    write( tempVal, '(ES18.10E3)' ) dZBBOSLeft()
                    outputFileContent = trim(outputFileContent) // (trim(tempVal) // ",")
                    write( tempVal, '(ES18.10E3)' ) dZBBOSRight()
                    outputFileContent = trim(outputFileContent) // (trim(tempVal) // ",")
                    write( tempVal, '(ES18.10E3)' ) dBeta1KanUsual()
                    outputFileContent = trim(outputFileContent) // (trim(tempVal) // ",")
                    write( tempVal, '(ES18.10E3)' ) dBeta1KanAlter()
                    outputFileContent = trim(outputFileContent) // (trim(tempVal) // ",")
                    write( tempVal, '(ES18.10E3)' ) dBeta1PinchPStar()
                    outputFileContent = trim(outputFileContent) // (trim(tempVal) // ",")
                    write( tempVal, '(ES18.10E3)' ) dBeta1PinchOS()
                    outputFileContent = trim(outputFileContent) // (trim(tempVal) // ",")
                end if
                do m = 1, maxNumberSchemes, 1
                    write( tempVal, '(ES18.10E3)') NLOWidth(m)
                    outputFileContent = trim(outputFileContent) // (trim(tempVal) // ",")
                end do
                write( tempVal, '(ES18.10E3)' ) ((treeLevelWidth - treeLevelWidth)*100D0/treeLevelWidth)
                outputFileContent = trim(outputFileContent) // (trim(tempVal) // ",")
                do m = 1, maxNumberSchemes, 1
                    write( tempVal, '(ES18.10E3)') ((NLOWidth(m) - treeLevelWidth)*100D0/treeLevelWidth)
                    if (m == maxNumberSchemes) then
                        outputFileContent = trim(outputFileContent) // (trim(tempVal) // "\n")
                    else
                        outputFileContent = trim(outputFileContent) // (trim(tempVal) // ",")
                    end if
                end do
            end do

            ! Write the results to the output file
            open(unit=44, file=trim(pathToOutputFiles)//trim(targetName), status='new', &
            &action='write', iostat=statWrite)
                if ( statWrite == 0) then
                    write(44,*) trim(outputFileContent)
                else
                   write(*,*) 'ERROR: could not create output file for writing!'
                end if
           close(unit=44)
        call ltexi
    end if

end program decayWidth
