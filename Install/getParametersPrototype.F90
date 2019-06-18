subroutine getParameters(filePath)
    use constants
    implicit none
    character(6) dump
    double precision dump2
    double precision MHHTemp, MA0Temp, MHpTemp, alphaTemp, TBTemp, m12squaredTemp
    integer TypeOf2HDMTemp    
    integer statOpen, statRead
    integer :: currentLine = 1
    logical fileExistsSM, fileExists2HDM
    character isContinue
    character(50), intent(in) :: filePath
    character(300), parameter :: pathToInputFiles = 'PLACEHOLDERRESULTPATH'

    ! Check if the SM input file exists
    inquire(file=trim(pathToInputFiles)//'ParametersSM.txt', exist=fileExistsSM)
    if (.not. fileExistsSM) then
        do
            print *, "ERROR: Could not find ParametersSM.txt!"
            print *, ">>> Do you want to continue with the evaluation of the program? [y/n]"
            read (*,*) isContinue
            if (isContinue == 'n') then
                print *, "Termination requested by user. 2HDMCalc will be terminated now."
                stop
            else if (isContinue == 'y') then
                exit
            else
                print *, "Invalid character. Enter y or n."
            end if
        end do
    end if

    ! Read the SM input parameters
    open(unit=42, file=trim(pathToInputFiles)//'ParametersSM.txt', iostat=statOpen)
        if (statOpen == 0) then
            read(42, *) dump, Mh0
            read(42, *) dump, MW
            read(42, *) dump, MZ
            read(42, *) dump, ME
            read(42, *) dump, MM
            read(42, *) dump, ML
            read(42, *) dump, MU
            read(42, *) dump, MC
            read(42, *) dump, MT
            read(42, *) dump, MD
            read(42, *) dump, MS
            read(42, *) dump, MB
            read(42, *) dump, EL
            read(42, *) dump, SW
            read(42, *) dump, CW
            read(42, *) dump, CKM11
            read(42, *) dump, CKM12
            read(42, *) dump, CKM13
            read(42, *) dump, CKM21
            read(42, *) dump, CKM22
            read(42, *) dump, CKM23
            read(42, *) dump, CKM31
            read(42, *) dump, CKM32
            read(42, *) dump, CKM33
            read(42, *) dump, CKMC11
            read(42, *) dump, CKMC12
            read(42, *) dump, CKMC13
            read(42, *) dump, CKMC21
            read(42, *) dump, CKMC22
            read(42, *) dump, CKMC23
            read(42, *) dump, CKMC31
            read(42, *) dump, CKMC32
            read(42, *) dump, CKMC33
            read(42, *) dump, InputScale
        else
            do
                print *, "ERROR: Generic error when reading file ParametersSM.txt!"
                print *, ">>> Do you want to continue with the evaluation of the program? [y/n]"
                read (*,*) isContinue
                if (isContinue == 'n') then
                    print *, "Termination requested by user. 2HDMCalc will be terminated now."
                    stop
                else if (isContinue == 'y') then
                    exit
                else
                    print *, "Invalid character. Enter y or n."
                end if
            end do
        end if
    close(42)

    ! Generate the square of the SM input parameters
    Mh02 = Mh0**2
    MW2 = MW**2
    MZ2 = MZ**2
    ME2 = ME**2
    MM2 = MM**2
    ML2 = ML**2
    MU2 = MU**2
    MC2 = MC**2
    MT2 = MT**2
    MD2 = MD**2
    MS2 = MS**2
    MB2 = MB**2
    EL2 = EL**2
    SW2 = SW**2
    CW2 = CW**2

    ! Check if the 2HDM input file exists
    inquire(file=trim(pathToInputFiles)//trim(filePath), exist=fileExists2HDM)
    if (.not. fileExists2HDM) then
        do
            print *, "ERROR: Could not find the 2HDM input parameter file!"
            print *, ">>> Do you want to continue with the evaluation of the program? [y/n]"
            read (*,*) isContinue
            if (isContinue == 'n') then
                print *, "Termination requested by user. 2HDMCalc will be terminated now."
                stop
            else if (isContinue == 'y') then
                exit
            else
                print *, "Invalid character. Enter y or n."
            end if
        end do
    end if

    ! Read the 2HDM input parameters
    open(unit=43, file=trim(pathToInputFiles)//trim(filePath), iostat=statOpen)
        if (statOpen == 0) then
            do
                ! Read the parameters from the current line
                ! read(43, *, iostat=statRead) MHHTemp, dump2, MA0Temp, MHpTemp, alphaTemp, TBTemp, dump2, dump2, dump2, &
                !     & dump2, dump2, dump2, dump2, m12squaredTemp
                read(43, *, iostat=statRead) MHHTemp, MA0Temp, MHpTemp, alphaTemp, TBTemp, m12squaredTemp, TypeOf2HDMTemp

                ! Stop reading the file at end-of-file
                if (statRead /= 0) exit
                !if (currentLine == 1000) exit

                !print *, "Reading line ", currentLine, "..."

                ! If end-of-file is not reached yet, copy the values into the lists
                MHHList(currentLine) = MHHTemp
                MA0List(currentLine) = MA0Temp
                MHpList(currentLine) = MHpTemp
                alphaList(currentLine) = alphaTemp
                TBList(currentLine) = TBTemp
                m12squaredList(currentLine) = m12squaredTemp
                TypeOf2HDMList(currentLine) = TypeOf2HDMTemp

                ! Calculate the current beta with the given tan(beta)
                betaList(currentLine) = datan(TBList(currentLine))

                ! Calculate the rest of the 2HDM parameters
                CAList(currentLine) = dcos(alphaList(currentLine))
                SAList(currentLine) = dsin(alphaList(currentLine))
                TAList(currentLine) = dtan(alphaList(currentLine))
                CBList(currentLine) = dcos(betaList(currentLine))
                SBList(currentLine) = dsin(betaList(currentLine))
                S2AList(currentLine) = dsin(2D0*alphaList(currentLine))
                C2AList(currentLine) = dcos(2D0*alphaList(currentLine))
                S2BList(currentLine) = dsin(2D0*betaList(currentLine))
                C2BList(currentLine) = dcos(2D0*betaList(currentLine))
                CABList(currentLine) = dcos(alphaList(currentLine) + betaList(currentLine))
                SABList(currentLine) = dsin(alphaList(currentLine) + betaList(currentLine))
                CBAList(currentLine) = dcos(betaList(currentLine) - alphaList(currentLine))
                SBAList(currentLine) = dsin(betaList(currentLine) - alphaList(currentLine))
                Lambda5List(currentLine) = EL2*m12squaredList(currentLine)/(2D0*SW2*MW2*SBList(currentLine)*CBList(currentLine))
                if (TypeOf2HDMList(currentLine) == 1) then
                    Yuk1List(currentLine) = CAList(currentLine)/SBList(currentLine)
                    Yuk2List(currentLine) = SAList(currentLine)/SBList(currentLine)
                    Yuk3List(currentLine) = - 1D0/TBList(currentLine)
                    Yuk4List(currentLine) = CAList(currentLine)/SBList(currentLine)
                    Yuk5List(currentLine) = SAList(currentLine)/SBList(currentLine)
                    Yuk6List(currentLine) = - 1D0/TBList(currentLine)
                else if (TypeOf2HDMList(currentLine) == 2) then
                    Yuk1List(currentLine) = - SAList(currentLine)/CBList(currentLine)
                    Yuk2List(currentLine) = CAList(currentLine)/CBList(currentLine)
                    Yuk3List(currentLine) = TBList(currentLine)
                    Yuk4List(currentLine) = - SAList(currentLine)/CBList(currentLine)
                    Yuk5List(currentLine) = CAList(currentLine)/CBList(currentLine)
                    Yuk6List(currentLine) = TBList(currentLine)
                else if (TypeOf2HDMList(currentLine) == 3) then
                    Yuk1List(currentLine) = CAList(currentLine)/SBList(currentLine)
                    Yuk2List(currentLine) = SAList(currentLine)/SBList(currentLine)
                    Yuk3List(currentLine) = - 1D0/TBList(currentLine)
                    Yuk4List(currentLine) = - SAList(currentLine)/CBList(currentLine)
                    Yuk5List(currentLine) = CAList(currentLine)/CBList(currentLine)
                    Yuk6List(currentLine) = TBList(currentLine)
                else
                    Yuk1List(currentLine) = - SAList(currentLine)/CBList(currentLine)
                    Yuk2List(currentLine) = CAList(currentLine)/CBList(currentLine)
                    Yuk3List(currentLine) = TBList(currentLine)
                    Yuk4List(currentLine) = CAList(currentLine)/SBList(currentLine)
                    Yuk5List(currentLine) = SAList(currentLine)/SBList(currentLine)
                    Yuk6List(currentLine) = - 1D0/TBList(currentLine)
                end if

                ! Set the maximum number of points contained in the file to the current line number
                maxPoint = currentLine

                ! Increment the line counter
                currentLine = currentLine + 1
            end do
            numberOfPoints = currentLine - 1
        else
            do
                print *, "ERROR: Generic error when reading the 2HDM input parameter file!"
                print *, ">>> Do you want to continue with the evaluation of the program? [y/n]"
                read (*,*) isContinue
                if (isContinue == 'n') then
                    print *, "Termination requested by user. 2HDMCalc will be terminated now."
                    stop
                else if (isContinue == 'y') then
                    exit
                else
                    print *, "Invalid character. Enter y or n."
                end if
            end do
        end if
    close(43)

end subroutine getParameters
