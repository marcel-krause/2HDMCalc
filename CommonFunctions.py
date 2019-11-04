#!/usr/bin/env python
#Filename: CommonFunctions.py


 ###################################################################################
#																					#
#								CommonFunctions 									#
#																					#
#	Purpose:	Function library for 2HDMCalc. Contains often used functions.		#
#																					#
 ###################################################################################


#------------------------------#
#		 Import Modules		   #
#------------------------------#
import sys
import os
import errno

#-------------------------#
#		 Functions		  #
#-------------------------#
def queryBoolean(question):
	'''
		For a given yes/no question, check the validity of the answer and return the corresponding Boolean value.
	'''
	validTrue = {"yes": True, "y": True, "ye": True, "j": True, "ja": True, "1": True}
	validFalse = {"no": False, "n": False, "nein": False, "0": False}
	prompt = " [y/n] "

	while True:
		sys.stdout.write(question + prompt)
		# Compatibility for Python 2 and 3
		if(sys.version_info > (3,0)):
			choice = input().lower()
		else:
			choice = raw_input().lower()
		if choice in validTrue:
			return True
		elif choice in validFalse:
			return False
		else:
			sys.stdout.write('Error: invalid input. Enter "y" or "n".\n\n')

def create_dir_if_not_exist(path):
    try:
        os.makedirs(path)
    except OSError as exception:
        if exception.errno != errno.EEXIST:
            raise

def createMakefile(pathToMakefile, relativePathToLoopTools, relativePathToLoopToolsLibs, useRelativePath, processId):
	# Check whether the OS is Windows or not for giving the decayWidth application the correct file ending 
	applicationEnding = ''
	if os.name == 'nt':
		applicationEnding = '.exe'

	makefile = open(pathToMakefile, 'w')
	makefile.truncate()
	makefile.write("###################################\n")
	makefile.write("#       Variables and Paths       #\n")
	makefile.write("###################################\n")
	if useRelativePath:
		makefile.write("PWD=$(strip $(shell pwd))\n")
		makefile.write("LT=$(PWD)/" + relativePathToLoopTools + "\n")
	else:
		makefile.write("LT=" + relativePathToLoopTools + "\n")
	makefile.write("LTCOMP = $(LT)/bin/fcc\n")
	makefile.write("FCOMP = gfortran\n")
	makefile.write("IFlags = -I$(LT)/include\n")
	makefile.write("LFlags = -L$(LT)/" + relativePathToLoopToolsLibs + " -looptools\n")
	makefile.write("SELFENERGIESUSU = BuildingBlocks/SelfEnergies/Usual\n")
	makefile.write("SELFENERGIESALT = BuildingBlocks/SelfEnergies/Alternative\n")
	makefile.write("SELFENERGIESDERIV = BuildingBlocks/SelfEnergiesDerivatives\n")
	makefile.write("PROCESSDEPENDENTSCHEME = BuildingBlocks/ProcessDependentScheme\n")
	makefile.write("PARAMETERS = Parameters\n")
	makefile.write("TADPOLES = BuildingBlocks/Tadpoles\n")
	makefile.write("PROCESS = BuildingBlocks/Processes/" + processId + "\n\n")
	makefile.write("%.o: %.F90\n")
	makefile.write("\t$(LTCOMP) -c -o $@ $< $(IFlags)\n\n")
	makefile.write("$(SELFENERGIESALT)/%.o: $(SELFENERGIESALT)/%.F90 constants.o\n")
	makefile.write("\t$(LTCOMP) -c -o $@ $< $(IFlags)\n\n")
	makefile.write("$(SELFENERGIESUSU)/%.o: $(SELFENERGIESUSU)/%.F90 constants.o\n")
	makefile.write("\t$(LTCOMP) -c -o $@ $< $(IFlags)\n\n")
	makefile.write("$(SELFENERGIESDERIV)/%.o: $(SELFENERGIESDERIV)/%.F90 constants.o\n")
	makefile.write("\t$(LTCOMP) -c -o $@ $< $(IFlags)\n\n")
	makefile.write("$(PROCESSDEPENDENTSCHEME)/%.o: $(PROCESSDEPENDENTSCHEME)/%.F90 constants.o\n")
	makefile.write("\t$(LTCOMP) -c -o $@ $< $(IFlags)\n\n")
	makefile.write("$(TADPOLES)/%.o: $(TADPOLES)/%.F90 constants.o\n")
	makefile.write("\t$(LTCOMP) -c -o $@ $< $(IFlags)\n\n")
	makefile.write("$(PROCESS)/%.o: $(PROCESS)/%.F90 constants.o counterterms.o\n")
	makefile.write("\t$(LTCOMP) -c -o $@ $< $(IFlags)\n\n")
	makefile.write("$(PARAMETERS)/%.o: $(PARAMETERS)/%.F90 constants.o\n")
	makefile.write("\t$(LTCOMP) -c -o $@ $< $(IFlags)\n\n")
	makefile.write("$(PROCESS)/decayWidth: constants.o $(SELFENERGIESUSU)/SelfAA.o $(SELFENERGIESUSU)/SelfAALight.o $(SELFENERGIESUSU)/SelfWpWp.o $(SELFENERGIESUSU)/SelfZ0Z0.o \\\n")
	makefile.write("\t$(SELFENERGIESUSU)/SelfAZ0.o $(SELFENERGIESUSU)/SelfAZ0ZeroMom.o $(SELFENERGIESALT)/SelfAA.o $(SELFENERGIESALT)/SelfWpWp.o $(SELFENERGIESALT)/SelfZ0Z0.o \\\n")
	makefile.write("\t$(SELFENERGIESALT)/SelfAZ0.o $(SELFENERGIESALT)/SelfAZ0ZeroMom.o $(SELFENERGIESUSU)/SelfA0A0.o $(SELFENERGIESUSU)/SelfG0A0.o \\\n")
	makefile.write("\t$(SELFENERGIESUSU)/SelfG0G0.o $(SELFENERGIESUSU)/SelfGpGp.o $(SELFENERGIESUSU)/SelfGpHp.o $(SELFENERGIESUSU)/Selfh0h0.o $(SELFENERGIESUSU)/SelfHHh0.o \\\n")
	makefile.write("\t$(SELFENERGIESUSU)/SelfHHHH.o $(SELFENERGIESUSU)/SelfHpHp.o $(SELFENERGIESALT)/SelfA0A0.o $(SELFENERGIESALT)/SelfG0A0.o $(SELFENERGIESALT)/SelfG0G0.o \\\n")
	makefile.write("\t$(SELFENERGIESALT)/SelfGpGp.o $(SELFENERGIESALT)/SelfGpHp.o $(SELFENERGIESALT)/Selfh0h0.o $(SELFENERGIESALT)/SelfHHh0.o $(SELFENERGIESALT)/SelfHHHH.o \\\n")
	makefile.write("\t$(SELFENERGIESALT)/SelfHpHp.o $(SELFENERGIESDERIV)/DSelfAA.o $(SELFENERGIESDERIV)/DSelfAALight.o $(SELFENERGIESDERIV)/DSelfWpWp.o $(SELFENERGIESDERIV)/DSelfZ0Z0.o \\\n")
	makefile.write("\t$(SELFENERGIESDERIV)/DSelfAZ0.o $(SELFENERGIESDERIV)/DSelfA0A0.o $(SELFENERGIESDERIV)/DSelfG0A0.o $(SELFENERGIESDERIV)/DSelfG0G0.o $(SELFENERGIESDERIV)/DSelfGpGp.o \\\n")
	makefile.write("\t$(SELFENERGIESDERIV)/DSelfGpHp.o $(SELFENERGIESDERIV)/DSelfHpHp.o $(SELFENERGIESDERIV)/DSelfh0h0.o $(SELFENERGIESDERIV)/DSelfHHh0.o $(SELFENERGIESDERIV)/DSelfHHHH.o \\\n")
	makefile.write("\t$(SELFENERGIESALT)/SelfHHh0Add.o $(SELFENERGIESALT)/SelfG0A0Add.o $(SELFENERGIESALT)/SelfGpHpAdd.o $(TADPOLES)/TadHH.o $(TADPOLES)/Tadh0.o \\\n")
	makefile.write("\t$(SELFENERGIESUSU)/SelfNeuENeuELeft.o $(SELFENERGIESUSU)/SelfNeuENeuERight.o $(SELFENERGIESUSU)/SelfNeuENeuEScalar.o \\\n")
	makefile.write("\t$(SELFENERGIESUSU)/SelfNeuMNeuMLeft.o $(SELFENERGIESUSU)/SelfNeuMNeuMRight.o $(SELFENERGIESUSU)/SelfNeuMNeuMScalar.o \\\n")
	makefile.write("\t$(SELFENERGIESUSU)/SelfNeuTNeuTLeft.o $(SELFENERGIESUSU)/SelfNeuTNeuTRight.o $(SELFENERGIESUSU)/SelfNeuTNeuTScalar.o \\\n")
	makefile.write("\t$(SELFENERGIESUSU)/SelfElElLeft.o $(SELFENERGIESUSU)/SelfElElRight.o $(SELFENERGIESUSU)/SelfElElScalar.o \\\n")
	makefile.write("\t$(SELFENERGIESUSU)/SelfMuMuLeft.o $(SELFENERGIESUSU)/SelfMuMuRight.o $(SELFENERGIESUSU)/SelfMuMuScalar.o \\\n")
	makefile.write("\t$(SELFENERGIESUSU)/SelfTauTauLeft.o $(SELFENERGIESUSU)/SelfTauTauRight.o $(SELFENERGIESUSU)/SelfTauTauScalar.o \\\n")
	makefile.write("\t$(SELFENERGIESUSU)/SelfTauTauLeftQED.o $(SELFENERGIESUSU)/SelfTauTauRightQED.o $(SELFENERGIESUSU)/SelfTauTauScalarQED.o \\\n")
	makefile.write("\t$(SELFENERGIESUSU)/SelfTauTauLeftWeak.o $(SELFENERGIESUSU)/SelfTauTauRightWeak.o $(SELFENERGIESUSU)/SelfTauTauScalarWeak.o \\\n")
	makefile.write("\t$(SELFENERGIESUSU)/SelfDDLeft.o $(SELFENERGIESUSU)/SelfDDRight.o $(SELFENERGIESUSU)/SelfDDScalar.o \\\n")
	makefile.write("\t$(SELFENERGIESUSU)/SelfUULeft.o $(SELFENERGIESUSU)/SelfUURight.o $(SELFENERGIESUSU)/SelfUUScalar.o \\\n")
	makefile.write("\t$(SELFENERGIESUSU)/SelfSSLeft.o $(SELFENERGIESUSU)/SelfSSRight.o $(SELFENERGIESUSU)/SelfSSScalar.o \\\n")
	makefile.write("\t$(SELFENERGIESUSU)/SelfCCLeft.o $(SELFENERGIESUSU)/SelfCCRight.o $(SELFENERGIESUSU)/SelfCCScalar.o \\\n")
	makefile.write("\t$(SELFENERGIESUSU)/SelfBBLeft.o $(SELFENERGIESUSU)/SelfBBRight.o $(SELFENERGIESUSU)/SelfBBScalar.o \\\n")
	makefile.write("\t$(SELFENERGIESUSU)/SelfTTLeft.o $(SELFENERGIESUSU)/SelfTTRight.o $(SELFENERGIESUSU)/SelfTTScalar.o \\\n")
	makefile.write("\t$(SELFENERGIESUSU)/SelfUCLeft.o $(SELFENERGIESUSU)/SelfUCRight.o $(SELFENERGIESUSU)/SelfUCScalar.o \\\n")
	makefile.write("\t$(SELFENERGIESUSU)/SelfUTLeft.o $(SELFENERGIESUSU)/SelfUTRight.o $(SELFENERGIESUSU)/SelfUTScalar.o \\\n")
	makefile.write("\t$(SELFENERGIESUSU)/SelfCTLeft.o $(SELFENERGIESUSU)/SelfCTRight.o $(SELFENERGIESUSU)/SelfCTScalar.o \\\n")
	makefile.write("\t$(SELFENERGIESUSU)/SelfDSLeft.o $(SELFENERGIESUSU)/SelfDSRight.o $(SELFENERGIESUSU)/SelfDSScalar.o \\\n")
	makefile.write("\t$(SELFENERGIESUSU)/SelfDBLeft.o $(SELFENERGIESUSU)/SelfDBRight.o $(SELFENERGIESUSU)/SelfDBScalar.o \\\n")
	makefile.write("\t$(SELFENERGIESUSU)/SelfSBLeft.o $(SELFENERGIESUSU)/SelfSBRight.o $(SELFENERGIESUSU)/SelfSBScalar.o \\\n")
	makefile.write("\t$(SELFENERGIESALT)/SelfNeuENeuELeft.o $(SELFENERGIESALT)/SelfNeuENeuERight.o $(SELFENERGIESALT)/SelfNeuENeuEScalar.o \\\n")
	makefile.write("\t$(SELFENERGIESALT)/SelfNeuMNeuMLeft.o $(SELFENERGIESALT)/SelfNeuMNeuMRight.o $(SELFENERGIESALT)/SelfNeuMNeuMScalar.o \\\n")
	makefile.write("\t$(SELFENERGIESALT)/SelfNeuTNeuTLeft.o $(SELFENERGIESALT)/SelfNeuTNeuTRight.o $(SELFENERGIESALT)/SelfNeuTNeuTScalar.o \\\n")
	makefile.write("\t$(SELFENERGIESALT)/SelfElElLeft.o $(SELFENERGIESALT)/SelfElElRight.o $(SELFENERGIESALT)/SelfElElScalar.o \\\n")
	makefile.write("\t$(SELFENERGIESALT)/SelfMuMuLeft.o $(SELFENERGIESALT)/SelfMuMuRight.o $(SELFENERGIESALT)/SelfMuMuScalar.o \\\n")
	makefile.write("\t$(SELFENERGIESALT)/SelfTauTauLeft.o $(SELFENERGIESALT)/SelfTauTauRight.o $(SELFENERGIESALT)/SelfTauTauScalar.o \\\n")
	makefile.write("\t$(SELFENERGIESALT)/SelfTauTauLeftQED.o $(SELFENERGIESALT)/SelfTauTauRightQED.o $(SELFENERGIESALT)/SelfTauTauScalarQED.o \\\n")
	makefile.write("\t$(SELFENERGIESALT)/SelfTauTauLeftWeak.o $(SELFENERGIESALT)/SelfTauTauRightWeak.o $(SELFENERGIESALT)/SelfTauTauScalarWeak.o \\\n")
	makefile.write("\t$(SELFENERGIESALT)/SelfDDLeft.o $(SELFENERGIESALT)/SelfDDRight.o $(SELFENERGIESALT)/SelfDDScalar.o \\\n")
	makefile.write("\t$(SELFENERGIESALT)/SelfUULeft.o $(SELFENERGIESALT)/SelfUURight.o $(SELFENERGIESALT)/SelfUUScalar.o \\\n")
	makefile.write("\t$(SELFENERGIESALT)/SelfSSLeft.o $(SELFENERGIESALT)/SelfSSRight.o $(SELFENERGIESALT)/SelfSSScalar.o \\\n")
	makefile.write("\t$(SELFENERGIESALT)/SelfCCLeft.o $(SELFENERGIESALT)/SelfCCRight.o $(SELFENERGIESALT)/SelfCCScalar.o \\\n")
	makefile.write("\t$(SELFENERGIESALT)/SelfBBLeft.o $(SELFENERGIESALT)/SelfBBRight.o $(SELFENERGIESALT)/SelfBBScalar.o \\\n")
	makefile.write("\t$(SELFENERGIESALT)/SelfTTLeft.o $(SELFENERGIESALT)/SelfTTRight.o $(SELFENERGIESALT)/SelfTTScalar.o \\\n")
	makefile.write("\t$(SELFENERGIESALT)/SelfUCLeft.o $(SELFENERGIESALT)/SelfUCRight.o $(SELFENERGIESALT)/SelfUCScalar.o \\\n")
	makefile.write("\t$(SELFENERGIESALT)/SelfUTLeft.o $(SELFENERGIESALT)/SelfUTRight.o $(SELFENERGIESALT)/SelfUTScalar.o \\\n")
	makefile.write("\t$(SELFENERGIESALT)/SelfCTLeft.o $(SELFENERGIESALT)/SelfCTRight.o $(SELFENERGIESALT)/SelfCTScalar.o \\\n")
	makefile.write("\t$(SELFENERGIESALT)/SelfDSLeft.o $(SELFENERGIESALT)/SelfDSRight.o $(SELFENERGIESALT)/SelfDSScalar.o \\\n")
	makefile.write("\t$(SELFENERGIESALT)/SelfDBLeft.o $(SELFENERGIESALT)/SelfDBRight.o $(SELFENERGIESALT)/SelfDBScalar.o \\\n")
	makefile.write("\t$(SELFENERGIESALT)/SelfSBLeft.o $(SELFENERGIESALT)/SelfSBRight.o $(SELFENERGIESALT)/SelfSBScalar.o \\\n")
	makefile.write("\t$(SELFENERGIESDERIV)/DSelfNeuENeuELeft.o $(SELFENERGIESDERIV)/DSelfNeuENeuERight.o $(SELFENERGIESDERIV)/DSelfNeuENeuEScalar.o \\\n")
	makefile.write("\t$(SELFENERGIESDERIV)/DSelfNeuMNeuMLeft.o $(SELFENERGIESDERIV)/DSelfNeuMNeuMRight.o $(SELFENERGIESDERIV)/DSelfNeuMNeuMScalar.o \\\n")
	makefile.write("\t$(SELFENERGIESDERIV)/DSelfNeuTNeuTLeft.o $(SELFENERGIESDERIV)/DSelfNeuTNeuTRight.o $(SELFENERGIESDERIV)/DSelfNeuTNeuTScalar.o \\\n")
	makefile.write("\t$(SELFENERGIESDERIV)/DSelfElElLeft.o $(SELFENERGIESDERIV)/DSelfElElRight.o $(SELFENERGIESDERIV)/DSelfElElScalar.o \\\n")
	makefile.write("\t$(SELFENERGIESDERIV)/DSelfMuMuLeft.o $(SELFENERGIESDERIV)/DSelfMuMuRight.o $(SELFENERGIESDERIV)/DSelfMuMuScalar.o \\\n")
	makefile.write("\t$(SELFENERGIESDERIV)/DSelfTauTauLeft.o $(SELFENERGIESDERIV)/DSelfTauTauRight.o $(SELFENERGIESDERIV)/DSelfTauTauScalar.o \\\n")
	makefile.write("\t$(SELFENERGIESDERIV)/DSelfTauTauLeftQED.o $(SELFENERGIESDERIV)/DSelfTauTauRightQED.o $(SELFENERGIESDERIV)/DSelfTauTauScalarQED.o \\\n")
	makefile.write("\t$(SELFENERGIESDERIV)/DSelfTauTauLeftWeak.o $(SELFENERGIESDERIV)/DSelfTauTauRightWeak.o $(SELFENERGIESDERIV)/DSelfTauTauScalarWeak.o \\\n")
	makefile.write("\t$(SELFENERGIESDERIV)/DSelfDDLeft.o $(SELFENERGIESDERIV)/DSelfDDRight.o $(SELFENERGIESDERIV)/DSelfDDScalar.o \\\n")
	makefile.write("\t$(SELFENERGIESDERIV)/DSelfUULeft.o $(SELFENERGIESDERIV)/DSelfUURight.o $(SELFENERGIESDERIV)/DSelfUUScalar.o \\\n")
	makefile.write("\t$(SELFENERGIESDERIV)/DSelfSSLeft.o $(SELFENERGIESDERIV)/DSelfSSRight.o $(SELFENERGIESDERIV)/DSelfSSScalar.o \\\n")
	makefile.write("\t$(SELFENERGIESDERIV)/DSelfCCLeft.o $(SELFENERGIESDERIV)/DSelfCCRight.o $(SELFENERGIESDERIV)/DSelfCCScalar.o \\\n")
	makefile.write("\t$(SELFENERGIESDERIV)/DSelfBBLeft.o $(SELFENERGIESDERIV)/DSelfBBRight.o $(SELFENERGIESDERIV)/DSelfBBScalar.o \\\n")
	makefile.write("\t$(SELFENERGIESDERIV)/DSelfTTLeft.o $(SELFENERGIESDERIV)/DSelfTTRight.o $(SELFENERGIESDERIV)/DSelfTTScalar.o \\\n")
	makefile.write("\t$(SELFENERGIESDERIV)/DSelfUCLeft.o $(SELFENERGIESDERIV)/DSelfUCRight.o $(SELFENERGIESDERIV)/DSelfUCScalar.o \\\n")
	makefile.write("\t$(SELFENERGIESDERIV)/DSelfUTLeft.o $(SELFENERGIESDERIV)/DSelfUTRight.o $(SELFENERGIESDERIV)/DSelfUTScalar.o \\\n")
	makefile.write("\t$(SELFENERGIESDERIV)/DSelfCTLeft.o $(SELFENERGIESDERIV)/DSelfCTRight.o $(SELFENERGIESDERIV)/DSelfCTScalar.o \\\n")
	makefile.write("\t$(SELFENERGIESDERIV)/DSelfDSLeft.o $(SELFENERGIESDERIV)/DSelfDSRight.o $(SELFENERGIESDERIV)/DSelfDSScalar.o \\\n")
	makefile.write("\t$(SELFENERGIESDERIV)/DSelfDBLeft.o $(SELFENERGIESDERIV)/DSelfDBRight.o $(SELFENERGIESDERIV)/DSelfDBScalar.o \\\n")
	makefile.write("\t$(SELFENERGIESDERIV)/DSelfSBLeft.o $(SELFENERGIESDERIV)/DSelfSBRight.o $(SELFENERGIESDERIV)/DSelfSBScalar.o \\\n")
	makefile.write("\t$(PROCESSDEPENDENTSCHEME)/A0toTauPTauMProcDepVC.o $(PROCESSDEPENDENTSCHEME)/HHtoTauPTauMProcDepVC.o $(PROCESSDEPENDENTSCHEME)/h0toTauPTauMProcDepVC.o \\\n")
	makefile.write("\t$(PROCESSDEPENDENTSCHEME)/A0toN1N1ProcDepRelVC.o $(PROCESSDEPENDENTSCHEME)/h0toN1N1ProcDepRelVC.o $(PROCESSDEPENDENTSCHEME)/HHtoN1N1ProcDepRelVC.o \\\n")
	makefile.write("\t$(PROCESSDEPENDENTSCHEME)/A0toN2N2ProcDepRelVC.o $(PROCESSDEPENDENTSCHEME)/h0toN2N2ProcDepRelVC.o $(PROCESSDEPENDENTSCHEME)/HHtoN2N2ProcDepRelVC.o \\\n")
	makefile.write("\t$(PROCESS)/TreeLevelWidthRed.o $(PROCESS)/NLOWidthRed.o $(PROCESS)/NLOTadWidthRed.o $(PROCESS)/Counterterm.o $(PROCESS)/RealCorrections.o \\\n")
	makefile.write("\tcounterterms.o $(PARAMETERS)/getParameters.o $(PROCESS)/decayWidth.o\n")
	makefile.write("\t$(LTCOMP) $(IFlags) constants.o $(SELFENERGIESUSU)/SelfAA.o $(SELFENERGIESUSU)/SelfAALight.o $(SELFENERGIESUSU)/SelfWpWp.o $(SELFENERGIESUSU)/SelfZ0Z0.o \\\n")
	makefile.write("\t$(SELFENERGIESUSU)/SelfAZ0.o $(SELFENERGIESUSU)/SelfAZ0ZeroMom.o $(SELFENERGIESALT)/SelfAA.o $(SELFENERGIESALT)/SelfWpWp.o $(SELFENERGIESALT)/SelfZ0Z0.o \\\n")
	makefile.write("\t$(SELFENERGIESALT)/SelfAZ0.o $(SELFENERGIESALT)/SelfAZ0ZeroMom.o $(SELFENERGIESUSU)/SelfA0A0.o \\\n")
	makefile.write("\t$(SELFENERGIESUSU)/SelfG0A0.o $(SELFENERGIESUSU)/SelfG0G0.o $(SELFENERGIESUSU)/SelfGpGp.o $(SELFENERGIESUSU)/SelfGpHp.o $(SELFENERGIESUSU)/Selfh0h0.o \\\n")
	makefile.write("\t$(SELFENERGIESUSU)/SelfHHh0.o $(SELFENERGIESUSU)/SelfHHHH.o $(SELFENERGIESUSU)/SelfHpHp.o $(SELFENERGIESALT)/SelfA0A0.o $(SELFENERGIESALT)/SelfG0A0.o \\\n")
	makefile.write("\t$(SELFENERGIESALT)/SelfG0G0.o $(SELFENERGIESALT)/SelfGpGp.o $(SELFENERGIESALT)/SelfGpHp.o $(SELFENERGIESALT)/Selfh0h0.o $(SELFENERGIESALT)/SelfHHh0.o \\\n")
	makefile.write("\t$(SELFENERGIESALT)/SelfHHHH.o $(SELFENERGIESALT)/SelfHpHp.o $(SELFENERGIESDERIV)/DSelfAA.o $(SELFENERGIESDERIV)/DSelfAALight.o $(SELFENERGIESDERIV)/DSelfWpWp.o \\\n")
	makefile.write("\t$(SELFENERGIESDERIV)/DSelfZ0Z0.o $(SELFENERGIESDERIV)/DSelfAZ0.o $(SELFENERGIESDERIV)/DSelfA0A0.o $(SELFENERGIESDERIV)/DSelfG0A0.o $(SELFENERGIESDERIV)/DSelfG0G0.o \\\n")
	makefile.write("\t$(SELFENERGIESDERIV)/DSelfGpGp.o $(SELFENERGIESDERIV)/DSelfGpHp.o $(SELFENERGIESDERIV)/DSelfHpHp.o $(SELFENERGIESDERIV)/DSelfh0h0.o $(SELFENERGIESDERIV)/DSelfHHh0.o \\\n")
	makefile.write("\t$(SELFENERGIESDERIV)/DSelfHHHH.o $(SELFENERGIESALT)/SelfHHh0Add.o $(SELFENERGIESALT)/SelfG0A0Add.o $(SELFENERGIESALT)/SelfGpHpAdd.o \\\n")
	makefile.write("\t$(TADPOLES)/TadHH.o $(TADPOLES)/Tadh0.o \\\n")
	makefile.write("\t$(SELFENERGIESUSU)/SelfNeuENeuELeft.o $(SELFENERGIESUSU)/SelfNeuENeuERight.o $(SELFENERGIESUSU)/SelfNeuENeuEScalar.o \\\n")
	makefile.write("\t$(SELFENERGIESUSU)/SelfNeuMNeuMLeft.o $(SELFENERGIESUSU)/SelfNeuMNeuMRight.o $(SELFENERGIESUSU)/SelfNeuMNeuMScalar.o \\\n")
	makefile.write("\t$(SELFENERGIESUSU)/SelfNeuTNeuTLeft.o $(SELFENERGIESUSU)/SelfNeuTNeuTRight.o $(SELFENERGIESUSU)/SelfNeuTNeuTScalar.o \\\n")
	makefile.write("\t$(SELFENERGIESUSU)/SelfElElLeft.o $(SELFENERGIESUSU)/SelfElElRight.o $(SELFENERGIESUSU)/SelfElElScalar.o \\\n")
	makefile.write("\t$(SELFENERGIESUSU)/SelfMuMuLeft.o $(SELFENERGIESUSU)/SelfMuMuRight.o $(SELFENERGIESUSU)/SelfMuMuScalar.o \\\n")
	makefile.write("\t$(SELFENERGIESUSU)/SelfTauTauLeft.o $(SELFENERGIESUSU)/SelfTauTauRight.o $(SELFENERGIESUSU)/SelfTauTauScalar.o \\\n")
	makefile.write("\t$(SELFENERGIESUSU)/SelfTauTauLeftQED.o $(SELFENERGIESUSU)/SelfTauTauRightQED.o $(SELFENERGIESUSU)/SelfTauTauScalarQED.o \\\n")
	makefile.write("\t$(SELFENERGIESUSU)/SelfTauTauLeftWeak.o $(SELFENERGIESUSU)/SelfTauTauRightWeak.o $(SELFENERGIESUSU)/SelfTauTauScalarWeak.o \\\n")
	makefile.write("\t$(SELFENERGIESUSU)/SelfDDLeft.o $(SELFENERGIESUSU)/SelfDDRight.o $(SELFENERGIESUSU)/SelfDDScalar.o \\\n")
	makefile.write("\t$(SELFENERGIESUSU)/SelfUULeft.o $(SELFENERGIESUSU)/SelfUURight.o $(SELFENERGIESUSU)/SelfUUScalar.o \\\n")
	makefile.write("\t$(SELFENERGIESUSU)/SelfSSLeft.o $(SELFENERGIESUSU)/SelfSSRight.o $(SELFENERGIESUSU)/SelfSSScalar.o \\\n")
	makefile.write("\t$(SELFENERGIESUSU)/SelfCCLeft.o $(SELFENERGIESUSU)/SelfCCRight.o $(SELFENERGIESUSU)/SelfCCScalar.o \\\n")
	makefile.write("\t$(SELFENERGIESUSU)/SelfBBLeft.o $(SELFENERGIESUSU)/SelfBBRight.o $(SELFENERGIESUSU)/SelfBBScalar.o \\\n")
	makefile.write("\t$(SELFENERGIESUSU)/SelfTTLeft.o $(SELFENERGIESUSU)/SelfTTRight.o $(SELFENERGIESUSU)/SelfTTScalar.o \\\n")
	makefile.write("\t$(SELFENERGIESUSU)/SelfUCLeft.o $(SELFENERGIESUSU)/SelfUCRight.o $(SELFENERGIESUSU)/SelfUCScalar.o \\\n")
	makefile.write("\t$(SELFENERGIESUSU)/SelfUTLeft.o $(SELFENERGIESUSU)/SelfUTRight.o $(SELFENERGIESUSU)/SelfUTScalar.o \\\n")
	makefile.write("\t$(SELFENERGIESUSU)/SelfCTLeft.o $(SELFENERGIESUSU)/SelfCTRight.o $(SELFENERGIESUSU)/SelfCTScalar.o \\\n")
	makefile.write("\t$(SELFENERGIESUSU)/SelfDSLeft.o $(SELFENERGIESUSU)/SelfDSRight.o $(SELFENERGIESUSU)/SelfDSScalar.o \\\n")
	makefile.write("\t$(SELFENERGIESUSU)/SelfDBLeft.o $(SELFENERGIESUSU)/SelfDBRight.o $(SELFENERGIESUSU)/SelfDBScalar.o \\\n")
	makefile.write("\t$(SELFENERGIESUSU)/SelfSBLeft.o $(SELFENERGIESUSU)/SelfSBRight.o $(SELFENERGIESUSU)/SelfSBScalar.o \\\n")
	makefile.write("\t$(SELFENERGIESALT)/SelfNeuENeuELeft.o $(SELFENERGIESALT)/SelfNeuENeuERight.o $(SELFENERGIESALT)/SelfNeuENeuEScalar.o \\\n")
	makefile.write("\t$(SELFENERGIESALT)/SelfNeuMNeuMLeft.o $(SELFENERGIESALT)/SelfNeuMNeuMRight.o $(SELFENERGIESALT)/SelfNeuMNeuMScalar.o \\\n")
	makefile.write("\t$(SELFENERGIESALT)/SelfNeuTNeuTLeft.o $(SELFENERGIESALT)/SelfNeuTNeuTRight.o $(SELFENERGIESALT)/SelfNeuTNeuTScalar.o \\\n")
	makefile.write("\t$(SELFENERGIESALT)/SelfElElLeft.o $(SELFENERGIESALT)/SelfElElRight.o $(SELFENERGIESALT)/SelfElElScalar.o \\\n")
	makefile.write("\t$(SELFENERGIESALT)/SelfMuMuLeft.o $(SELFENERGIESALT)/SelfMuMuRight.o $(SELFENERGIESALT)/SelfMuMuScalar.o \\\n")
	makefile.write("\t$(SELFENERGIESALT)/SelfTauTauLeft.o $(SELFENERGIESALT)/SelfTauTauRight.o $(SELFENERGIESALT)/SelfTauTauScalar.o \\\n")
	makefile.write("\t$(SELFENERGIESALT)/SelfTauTauLeftQED.o $(SELFENERGIESALT)/SelfTauTauRightQED.o $(SELFENERGIESALT)/SelfTauTauScalarQED.o \\\n")
	makefile.write("\t$(SELFENERGIESALT)/SelfTauTauLeftWeak.o $(SELFENERGIESALT)/SelfTauTauRightWeak.o $(SELFENERGIESALT)/SelfTauTauScalarWeak.o \\\n")
	makefile.write("\t$(SELFENERGIESALT)/SelfDDLeft.o $(SELFENERGIESALT)/SelfDDRight.o $(SELFENERGIESALT)/SelfDDScalar.o \\\n")
	makefile.write("\t$(SELFENERGIESALT)/SelfUULeft.o $(SELFENERGIESALT)/SelfUURight.o $(SELFENERGIESALT)/SelfUUScalar.o \\\n")
	makefile.write("\t$(SELFENERGIESALT)/SelfSSLeft.o $(SELFENERGIESALT)/SelfSSRight.o $(SELFENERGIESALT)/SelfSSScalar.o \\\n")
	makefile.write("\t$(SELFENERGIESALT)/SelfCCLeft.o $(SELFENERGIESALT)/SelfCCRight.o $(SELFENERGIESALT)/SelfCCScalar.o \\\n")
	makefile.write("\t$(SELFENERGIESALT)/SelfBBLeft.o $(SELFENERGIESALT)/SelfBBRight.o $(SELFENERGIESALT)/SelfBBScalar.o \\\n")
	makefile.write("\t$(SELFENERGIESALT)/SelfTTLeft.o $(SELFENERGIESALT)/SelfTTRight.o $(SELFENERGIESALT)/SelfTTScalar.o \\\n")
	makefile.write("\t$(SELFENERGIESALT)/SelfUCLeft.o $(SELFENERGIESALT)/SelfUCRight.o $(SELFENERGIESALT)/SelfUCScalar.o \\\n")
	makefile.write("\t$(SELFENERGIESALT)/SelfUTLeft.o $(SELFENERGIESALT)/SelfUTRight.o $(SELFENERGIESALT)/SelfUTScalar.o \\\n")
	makefile.write("\t$(SELFENERGIESALT)/SelfCTLeft.o $(SELFENERGIESALT)/SelfCTRight.o $(SELFENERGIESALT)/SelfCTScalar.o \\\n")
	makefile.write("\t$(SELFENERGIESALT)/SelfDSLeft.o $(SELFENERGIESALT)/SelfDSRight.o $(SELFENERGIESALT)/SelfDSScalar.o \\\n")
	makefile.write("\t$(SELFENERGIESALT)/SelfDBLeft.o $(SELFENERGIESALT)/SelfDBRight.o $(SELFENERGIESALT)/SelfDBScalar.o \\\n")
	makefile.write("\t$(SELFENERGIESALT)/SelfSBLeft.o $(SELFENERGIESALT)/SelfSBRight.o $(SELFENERGIESALT)/SelfSBScalar.o \\\n")
	makefile.write("\t$(SELFENERGIESDERIV)/DSelfNeuENeuELeft.o $(SELFENERGIESDERIV)/DSelfNeuENeuERight.o $(SELFENERGIESDERIV)/DSelfNeuENeuEScalar.o \\\n")
	makefile.write("\t$(SELFENERGIESDERIV)/DSelfNeuMNeuMLeft.o $(SELFENERGIESDERIV)/DSelfNeuMNeuMRight.o $(SELFENERGIESDERIV)/DSelfNeuMNeuMScalar.o \\\n")
	makefile.write("\t$(SELFENERGIESDERIV)/DSelfNeuTNeuTLeft.o $(SELFENERGIESDERIV)/DSelfNeuTNeuTRight.o $(SELFENERGIESDERIV)/DSelfNeuTNeuTScalar.o \\\n")
	makefile.write("\t$(SELFENERGIESDERIV)/DSelfElElLeft.o $(SELFENERGIESDERIV)/DSelfElElRight.o $(SELFENERGIESDERIV)/DSelfElElScalar.o \\\n")
	makefile.write("\t$(SELFENERGIESDERIV)/DSelfMuMuLeft.o $(SELFENERGIESDERIV)/DSelfMuMuRight.o $(SELFENERGIESDERIV)/DSelfMuMuScalar.o \\\n")
	makefile.write("\t$(SELFENERGIESDERIV)/DSelfTauTauLeft.o $(SELFENERGIESDERIV)/DSelfTauTauRight.o $(SELFENERGIESDERIV)/DSelfTauTauScalar.o \\\n")
	makefile.write("\t$(SELFENERGIESDERIV)/DSelfTauTauLeftQED.o $(SELFENERGIESDERIV)/DSelfTauTauRightQED.o $(SELFENERGIESDERIV)/DSelfTauTauScalarQED.o \\\n")
	makefile.write("\t$(SELFENERGIESDERIV)/DSelfTauTauLeftWeak.o $(SELFENERGIESDERIV)/DSelfTauTauRightWeak.o $(SELFENERGIESDERIV)/DSelfTauTauScalarWeak.o \\\n")
	makefile.write("\t$(SELFENERGIESDERIV)/DSelfDDLeft.o $(SELFENERGIESDERIV)/DSelfDDRight.o $(SELFENERGIESDERIV)/DSelfDDScalar.o \\\n")
	makefile.write("\t$(SELFENERGIESDERIV)/DSelfUULeft.o $(SELFENERGIESDERIV)/DSelfUURight.o $(SELFENERGIESDERIV)/DSelfUUScalar.o \\\n")
	makefile.write("\t$(SELFENERGIESDERIV)/DSelfSSLeft.o $(SELFENERGIESDERIV)/DSelfSSRight.o $(SELFENERGIESDERIV)/DSelfSSScalar.o \\\n")
	makefile.write("\t$(SELFENERGIESDERIV)/DSelfCCLeft.o $(SELFENERGIESDERIV)/DSelfCCRight.o $(SELFENERGIESDERIV)/DSelfCCScalar.o \\\n")
	makefile.write("\t$(SELFENERGIESDERIV)/DSelfBBLeft.o $(SELFENERGIESDERIV)/DSelfBBRight.o $(SELFENERGIESDERIV)/DSelfBBScalar.o \\\n")
	makefile.write("\t$(SELFENERGIESDERIV)/DSelfTTLeft.o $(SELFENERGIESDERIV)/DSelfTTRight.o $(SELFENERGIESDERIV)/DSelfTTScalar.o \\\n")
	makefile.write("\t$(SELFENERGIESDERIV)/DSelfUCLeft.o $(SELFENERGIESDERIV)/DSelfUCRight.o $(SELFENERGIESDERIV)/DSelfUCScalar.o \\\n")
	makefile.write("\t$(SELFENERGIESDERIV)/DSelfUTLeft.o $(SELFENERGIESDERIV)/DSelfUTRight.o $(SELFENERGIESDERIV)/DSelfUTScalar.o \\\n")
	makefile.write("\t$(SELFENERGIESDERIV)/DSelfCTLeft.o $(SELFENERGIESDERIV)/DSelfCTRight.o $(SELFENERGIESDERIV)/DSelfCTScalar.o \\\n")
	makefile.write("\t$(SELFENERGIESDERIV)/DSelfDSLeft.o $(SELFENERGIESDERIV)/DSelfDSRight.o $(SELFENERGIESDERIV)/DSelfDSScalar.o \\\n")
	makefile.write("\t$(SELFENERGIESDERIV)/DSelfDBLeft.o $(SELFENERGIESDERIV)/DSelfDBRight.o $(SELFENERGIESDERIV)/DSelfDBScalar.o \\\n")
	makefile.write("\t$(SELFENERGIESDERIV)/DSelfSBLeft.o $(SELFENERGIESDERIV)/DSelfSBRight.o $(SELFENERGIESDERIV)/DSelfSBScalar.o \\\n")
	makefile.write("\t$(PROCESSDEPENDENTSCHEME)/A0toTauPTauMProcDepVC.o $(PROCESSDEPENDENTSCHEME)/HHtoTauPTauMProcDepVC.o $(PROCESSDEPENDENTSCHEME)/h0toTauPTauMProcDepVC.o \\\n")
	makefile.write("\t$(PROCESSDEPENDENTSCHEME)/A0toN1N1ProcDepRelVC.o $(PROCESSDEPENDENTSCHEME)/h0toN1N1ProcDepRelVC.o $(PROCESSDEPENDENTSCHEME)/HHtoN1N1ProcDepRelVC.o \\\n")
	makefile.write("\t$(PROCESSDEPENDENTSCHEME)/A0toN2N2ProcDepRelVC.o $(PROCESSDEPENDENTSCHEME)/h0toN2N2ProcDepRelVC.o $(PROCESSDEPENDENTSCHEME)/HHtoN2N2ProcDepRelVC.o \\\n")
	makefile.write("\t$(PROCESS)/TreeLevelWidthRed.o $(PROCESS)/NLOWidthRed.o $(PROCESS)/NLOTadWidthRed.o $(PROCESS)/Counterterm.o $(PROCESS)/RealCorrections.o \\\n")
	makefile.write("\tcounterterms.o $(PARAMETERS)/getParameters.o $(PROCESS)/decayWidth.o $(LFlags) -o $(PROCESS)/decayWidth" + applicationEnding + "\n\n")
	makefile.write("clean:\n")
	makefile.write("\trm -f *.o\n")
	makefile.close()

def createRealCorrections(pathToFile, processId, m1, m2, m3):
	# If any of the masses vanish, we comment the corresponding integrals out, since the logarithms would diverge (note: massless particles are not supported!)
	I22Comment = I33Comment = I12Comment = I13Comment = I23Comment = ""
	if m1 == "0":
		sys.exit("Error: massless incoming particles are not supported.")
	if m2 == "0":
		I22Comment = "!"
		I12Comment = "!"
		I23Comment = "!"
	if m3 == "0":
		I33Comment = "!"
		I13Comment = "!"
		I23Comment = "!"
	
	fileContent = open(pathToFile, 'w')
	fileContent.truncate()
	fileContent.write("double precision function " + processId + "Real()\n")
	fileContent.write(" use constants\n")
	fileContent.write(" use counterterms\n")
	fileContent.write(" implicit none\n")
	fileContent.write('#include "looptools.h"')
	fileContent.write("\n\n double precision :: totalAmplitude\n")
	fileContent.write(" double precision :: p2, p3, E2, E3, m1, m2, m3, kappa, beta1, beta2, beta3\n")
	fileContent.write(" double precision :: I11, I22, I33, I12, I13, I23\n")
	fileContent.write(" double precision :: IFin, Id1Fin, Id2Fin, Id3Fin, Iu2d1Fin, Iu3d1Fin, Iu1d2Fin, Iu1d3Fin, Iu2d3Fin, Iu3d2Fin, Iu22d33Fin\n")
	fileContent.write(" double precision :: Iu11d22Fin, Osvv11, Osvv12, Osvv22\n")
	fileContent.write(" double precision :: OLL00, OLL01, OLL02, OLL11, OLL12, OLL22, OLR00, OLR01, OLR02, OLR11, OLR12, OLR22, OLLFull, OLRFull\n")
	fileContent.write(" double precision :: Ossv00, Ossv01, Ossv11, Ossv02, Ossv12, Ossv22, Ossv03, Ossv13, Ossv23, Ossv33, OssvFull\n")
	fileContent.write(" double precision :: Qssv0, Qssv1, Qssv2, Qsvv1, Qsvv2, QsvvFull, Osss11, Osss12, Osss22, OsssFull, Qsss1, Qsss2\n\n")

	fileContent.write(" m1 = " + m1 + "\n")
	fileContent.write(" m2 = " + m2 + "\n")
	fileContent.write(" m3 = " + m3 + "\n\n")

	fileContent.write(" kappa = DSQRT(m1**4 + m2**4 + m3**4 - 2D0*m1**2*m2**2 - 2D0*m1**2*m3**2 - 2D0*m2**2*m3**2)\n")
	fileContent.write(" " + I22Comment + "beta1 = (m1**2 - m2**2 - m3**2 + kappa)/(2D0*m2*m3)\n")
	fileContent.write(" " + I33Comment + "beta2 = (m1**2 - m2**2 + m3**2 - kappa)/(2D0*m1*m3)\n")	
	fileContent.write(" " + I22Comment + "beta3 = (m1**2 + m2**2 - m3**2 - kappa)/(2D0*m1*m2)\n\n")	

	fileContent.write(" p2 = kappa/(2D0*m1)\n")
	fileContent.write(" p3 = kappa/(2D0*m1)\n")
	fileContent.write(" E2 = DSQRT( m2**2 + p2**2 )\n")	
	fileContent.write(" E3 = DSQRT( m3**2 + p3**2 )\n\n")	

	fileContent.write(" I11 = ( kappa*DLOG(kappa**2/(DSQRT(IRLambda)*m1*m2*m3)) - kappa - (m2**2-m3**2)*DLOG(beta2/beta3) - m1**2*DLOG(beta1) )&\n")
	fileContent.write("     &/(4D0*m1**4)\n")
	fileContent.write(" " + I22Comment + "I22 = ( kappa*DLOG(kappa**2/(DSQRT(IRLambda)*m1*m2*m3)) - kappa - (m1**2-m3**2)*DLOG(beta1/beta3) - m2**2*DLOG(beta2) )&\n")
	fileContent.write(" " + I22Comment + "    &/(4D0*m1**2*m2**2)\n")
	fileContent.write(" " + I33Comment + "I33 = ( kappa*DLOG(kappa**2/(DSQRT(IRLambda)*m1*m2*m3)) - kappa - (m1**2-m2**2)*DLOG(beta1/beta2) - m3**2*DLOG(beta3) )&\n")
	fileContent.write(" " + I33Comment + "    &/(4D0*m1**2*m3**2)\n")
	fileContent.write(" " + I12Comment + "I12 = ( - 2D0*DLOG(DSQRT(IRLambda)*m1*m2*m3/kappa**2)*DLOG(beta3) + 2D0*DLOG(beta3)**2 - DLOG(beta1)**2 - DLOG(beta2)**2 + &\n")
	fileContent.write(" " + I12Comment + "    & 2D0*Li2(1D0 - beta3**2) - Li2(1D0 - beta1**2) - Li2(1D0 - beta2**2) )/(4D0*m1**2)\n")
	fileContent.write(" " + I13Comment + "I13 = ( - 2D0*DLOG(DSQRT(IRLambda)*m1*m2*m3/kappa**2)*DLOG(beta2) + 2D0*DLOG(beta2)**2 - DLOG(beta1)**2 - DLOG(beta3)**2 + &\n")
	fileContent.write(" " + I13Comment + "    & 2D0*Li2(1D0 - beta2**2) - Li2(1D0 - beta1**2) - Li2(1D0 - beta3**2) )/(4D0*m1**2)\n")
	fileContent.write(" " + I23Comment + "I23 = ( - 2D0*DLOG(DSQRT(IRLambda)*m1*m2*m3/kappa**2)*DLOG(beta1) + 2D0*DLOG(beta1)**2 - DLOG(beta2)**2 - DLOG(beta3)**2 + &\n")
	fileContent.write(" " + I23Comment + "    & 2D0*Li2(1D0 - beta1**2) - Li2(1D0 - beta2**2) - Li2(1D0 - beta3**2) )/(4D0*m1**2)\n")
	fileContent.write(" IFin = ( kappa/2D0*(m1**2 + m2**2 + m3**2) + 2D0*m1**2*m2**2*DLOG(beta3) + 2D0*m1**2*m3**2*DLOG(beta2) + &\n")
	fileContent.write("     & 2D0*m2**2*m3**2*DLOG(beta1) )/(4D0*m1**2)\n")
	fileContent.write(" Id1Fin = ( - 2D0*m2**2*DLOG(beta3) - 2D0*m3**2*DLOG(beta2) - kappa )/(4D0*m1**2)\n")
	fileContent.write(" " + I22Comment + "Id2Fin = ( - 2D0*m1**2*DLOG(beta3) - 2D0*m3**2*DLOG(beta1) - kappa )/(4D0*m1**2)\n")
	fileContent.write(" " + I33Comment + "Id3Fin = ( - 2D0*m1**2*DLOG(beta2) - 2D0*m2**2*DLOG(beta1) - kappa )/(4D0*m1**2)\n")
	fileContent.write(" " + I12Comment + "Iu2d1Fin = ( m2**4*DLOG(beta3) - m3**2*(2D0*m1**2 - 2D0*m2**2 + m3**2)*DLOG(beta2) &\n")
	fileContent.write(" " + I12Comment + "    & - kappa/4D0*(m1**2 - 3D0*m2**2 + 5D0*m3**2) )/(4D0*m1**2)\n")
	fileContent.write(" " + I13Comment + "Iu3d1Fin = ( m3**4*DLOG(beta2) - m2**2*(2D0*m1**2 - 2D0*m3**2 + m2**2)*DLOG(beta3) &\n")
	fileContent.write(" " + I13Comment + "    & - kappa/4D0*(m1**2 - 3D0*m3**2 + 5D0*m2**2) )/(4D0*m1**2)\n")
	fileContent.write(" " + I12Comment + "Iu1d2Fin = ( m1**4*DLOG(beta3) - m3**2*(2D0*m2**2 - 2D0*m1**2 + m3**2)*DLOG(beta1) &\n")
	fileContent.write(" " + I12Comment + "    & - kappa/4D0*(m2**2 - 3D0*m1**2 + 5D0*m3**2) )/(4D0*m1**2)\n")
	fileContent.write(" " + I13Comment + "Iu1d3Fin = ( m1**4*DLOG(beta2) - m2**2*(2D0*m3**2 - 2D0*m1**2 + m2**2)*DLOG(beta1) &\n")
	fileContent.write(" " + I13Comment + "    & - kappa/4D0*(m3**2 - 3D0*m1**2 + 5D0*m2**2) )/(4D0*m1**2)\n")
	fileContent.write(" " + I23Comment + "Iu2d3Fin = ( m2**4*DLOG(beta1) - m1**2*(2D0*m3**2 - 2D0*m2**2 + m1**2)*DLOG(beta2) &\n")
	fileContent.write(" " + I23Comment + "    & - kappa/4D0*(m3**2 - 3D0*m2**2 + 5D0*m1**2) )/(4D0*m1**2)\n")
	fileContent.write(" " + I23Comment + "Iu3d2Fin = ( m3**4*DLOG(beta1) - m1**2*(2D0*m2**2 - 2D0*m3**2 + m1**2)*DLOG(beta3) &\n")
	fileContent.write(" " + I23Comment + "    & - kappa/4D0*(m2**2 - 3D0*m3**2 + 5D0*m1**2) )/(4D0*m1**2)\n")
	fileContent.write(" " + I23Comment + "Iu11d22Fin = ( 2D0*m3**2*(m2**2 + m3**2 - m1**2)*DLOG(beta1) &\n")
	fileContent.write(" " + I23Comment + "    & + kappa**3/(6D0*m2**2) + 2D0*kappa*m3**2 )/(4D0*m1**2)\n")
	fileContent.write(" " + I23Comment + "Iu22d33Fin = ( 2D0*m1**2*(m1**2 + m3**2 - m2**2)*DLOG(beta2) &\n")
	fileContent.write(" " + I23Comment + "    & + kappa**3/(6D0*m3**2) + 2D0*kappa*m1**2 )/(4D0*m1**2)\n\n")

	fileContent.write("! OLR00 = 16D0*I11*m1**2*m2*m3\n")
	fileContent.write("! OLR01 = -16D0*Id1Fin*m2*m3 - 16D0*Id2Fin*m2*m3 + I12*(-16D0*m1**2*m2*m3 - 16D0*m2**3*m3 + 16D0*m2*m3**3)\n")
	fileContent.write("! OLR02 = 16D0*Id1Fin*m2*m3 + I13*(16D0*m1**2*m2*m3 - 16D0*m2**3*m3 + 16D0*m2*m3**3)\n")
	fileContent.write("! OLR11 = 16D0*I22*m2**3*m3\n")
	fileContent.write("! OLR12 = 16D0*Id2Fin*m2*m3 + I23*(-16D0*m1**2*m2*m3 + 16D0*m2**3*m3 + 16D0*m2*m3**3)\n")
	fileContent.write("! OLR22 = 16D0*Id3Fin*m2*m3 + 16D0*I33*m2*m3**3\n")
	fileContent.write("! OLL00 = -8D0*Id1Fin*m1**2 + I11*(-8D0*m1**4 + 8D0*m1**2*m2**2 + 8D0*m1**2*m3**2)\n")
	fileContent.write("! OLL01 = 4D0*IFin + 4D0*Iu1d2Fin + Id2Fin*(12D0*m1**2 - 4D0*m2**2 - 12D0*m3**2) + Id1Fin*(-8D0*m2**2 - 8D0*m3**2) + &\n")
	fileContent.write("!     & I12*(8D0*m1**4 - 8D0*m2**4 - 16D0*m1**2*m3**2 + 8D0*m3**4)\n")
	fileContent.write("! OLL02 = -4D0*IFin - 4D0*Iu1d3Fin + Id3Fin*(-12D0*m1**2 + 12D0*m2**2 + 4D0*m3**2) + Id1Fin*(8D0*m2**2 + 8D0*m3**2) + &\n")
	fileContent.write("!     & I13*(-8D0*m1**4 + 16D0*m1**2*m2**2 - 8D0*m2**4 + 8D0*m3**4)\n")
	fileContent.write("! OLL11 = Id2Fin*(4D0*m1**2 + 4D0*m2**2 - 4D0*m3**2) + I22*(-8D0*m1**2*m2**2 + 8D0*m2**4 + 8D0*m2**2*m3**2)\n")
	fileContent.write("! OLL12 = 8D0*IFin + 4D0*Iu3d2Fin + 4D0*Iu2d3Fin + Id3Fin*(-12D0*m1**2 + 12D0*m2**2 + 4D0*m3**2) + Id2Fin*(-12D0*m1**2 + &\n")
	fileContent.write("!     & 4D0*m2**2 + 12D0*m3**2 ) + I23*(8D0*m1**4 - 16D0*m1**2*m2**2 + 8D0*m2**4 - 16D0*m1**2*m3**2 + 16D0*m2**2*m3**2&\n")
	fileContent.write("!     & + 8D0*m3**4)\n")
	fileContent.write("! OLL22 = Id3Fin*(4D0*m1**2 - 4D0*m2**2 + 4D0*m3**2) + I33*(-8D0*m1**2*m3**2 + 8D0*m2**2*m3**2 + 8D0*m3**4)\n\n")

	fileContent.write("! Ossv00 = -4D0*IFin*m1**2 + Id1Fin*(-8D0*m1**4 + 8D0*m1**2*m2**2 + 8D0*m1**2*m3**2) + I11*(-4D0*m1**6 + 8D0*m1**4*m2**2 - &\n")
	fileContent.write("!     & 4D0*m1**2*m2**4 + 8D0*m1**4*m3**2 + 8D0*m1**2*m2**2*m3**2 - 4D0*m1**2*m3**4 )\n")
	fileContent.write("! Ossv01 = Iu2d1Fin*(4D0*m1**2 - 4D0*m2**2 + 4D0*m3**2) + Iu1d2Fin*(-4D0*m1**2 + 4D0*m2**2 + 4D0*m3**2) + IFin*(4D0*m1**2 +&\n")
	fileContent.write("!     & 4D0*m2**2 + 4D0*m3**2 ) + Id2Fin*(-8D0*m1**4 + 8D0*m1**2*m2**2 + 16D0*m1**2*m3**2 + 8D0*m2**2*m3**2 - 8D0*m3**4) + &\n")
	fileContent.write("!     & Id1Fin*(8D0*m1**2*m2**2 - 8D0*m2**4 + 8D0*m1**2*m3**2 + 16D0*m2**2*m3**2 - 8D0*m3**4) + I12*(-4D0*m1**6 + &\n")
	fileContent.write("!     & 4D0*m1**4*m2**2 + 4D0*m1**2*m2**4 - 4D0*m2**6 + 12D0*m1**4*m3**2 + 8D0*m1**2*m2**2*m3**2 + 12D0*m2**4*m3**2 - &\n")
	fileContent.write("!     & 12D0*m1**2*m3**4 - 12D0*m2**2*m3**4 + 4D0*m3**6)\n")
	fileContent.write("! Ossv02 = Iu3d1Fin*(-2D0*m1**2 + 2D0*m2**2 - 2D0*m3**2) + IFin*(2D0*m1**2 + 6D0*m2**2 + 2D0*m3**2) + Id1Fin*(2D0*m1**4 + &\n")
	fileContent.write("!     & 4D0*m1**2*m2**2 - 6D0*m2**4 + 4D0*m1**2*m3**2 + 12D0*m2**2*m3**2 - 6D0*m3**4 ) + Id3Fin*(-4D0*m1**4 + &\n")
	fileContent.write("!     & 8D0*m1**2*m2**2 - 4D0*m2**4 + 4D0*m3**4 ) + I13*(-4D0*m1**6 + 12D0*m1**4*m2**2 - 12D0*m1**2*m2**4 + 4D0*m2**6 + &\n")
	fileContent.write("!     & 4D0*m1**4*m3**2 + 8D0*m1**2*m2**2*m3**2 - 12D0*m2**4*m3**2 + 4D0*m1**2*m3**4 + 12D0*m2**2*m3**4 - 4D0*m3**6 )\n")
	fileContent.write("! Ossv03 = IFin*(2D0*m1**2 - 2D0*m2**2 + 2D0*m3**2) + Iu3d1Fin*(2D0*m1**2 - 2D0*m2**2 + 2D0*m3**2) + Id1Fin*(2D0*m1**4 - &\n")
	fileContent.write("!     & 4D0*m1**2*m2**2 + 2D0*m2**4 - 4D0*m1**2*m3**2 - 4D0*m2**2*m3**2 + 2D0*m3**4 )\n")
	fileContent.write("! Ossv11 = -4D0*IFin*m2**2 + Id2Fin*(8D0*m1**2*m2**2 - 8D0*m2**4 + 8D0*m2**2*m3**2) + I22*(-4D0*m1**4*m2**2 + &\n")
	fileContent.write("!     & 8D0*m1**2*m2**4 - 4D0*m2**6 + 8D0*m1**2*m2**2*m3**2 + 8D0*m2**4*m3**2 - 4D0*m2**2*m3**4 )\n")
	fileContent.write("! Ossv12 = IFin*(-6D0*m1**2 - 2D0*m2**2 - 2D0*m3**2) + Iu3d2Fin*(-2D0*m1**2 + 2D0*m2**2 + 2D0*m3**2) + Id3Fin*(4D0*m1**4 - &\n")
	fileContent.write("!     & 8D0*m1**2*m2**2 + 4D0*m2**4 - 4D0*m3**4 ) + Id2Fin*(6D0*m1**4 - 4D0*m1**2*m2**2 - 2D0*m2**4 - 12D0*m1**2*m3**2 - &\n")
	fileContent.write("!     & 4D0*m2**2*m3**2 + 6D0*m3**4 ) + I23*(-4D0*m1**6 + 12D0*m1**4*m2**2 - 12D0*m1**2*m2**4 + 4D0*m2**6 + 12D0*m1**4*m3**2 &\n")
	fileContent.write("!     & - 8D0*m1**2*m2**2*m3**2 - 4D0*m2**4*m3**2 - 12D0*m1**2*m3**4 - 4D0*m2**2*m3**4 + 4D0*m3**6 )\n")
	fileContent.write("! Ossv13 = IFin*(-2D0*m1**2 + 2D0*m2**2 + 2D0*m3**2) + Iu3d2Fin*(-2D0*m1**2 + 2D0*m2**2 + 2D0*m3**2) + Id2Fin*(2D0*m1**4 - &\n")
	fileContent.write("!     & 4D0*m1**2*m2**2 + 2D0*m2**4 - 4D0*m1**2*m3**2 - 4D0*m2**2*m3**2 + 2D0*m3**4 )\n")
	fileContent.write("! Ossv22 = IFin*(-2D0*m1**2 - 2D0*m2**2 - m3**2) + 8D0*Iu2d3Fin*m3**2 + 8D0*Iu22d33Fin*m3**2 + Id3Fin*(8D0*m1**2*m3**2 + &\n")
	fileContent.write("!     & 8D0*m2**2*m3**2 - 8D0*m3**4 ) + I33*(-4D0*m1**4*m3**2 + 8D0*m1**2*m2**2*m3**2 - 4D0*m2**4*m3**2 + 8D0*m1**2*m3**4 + &\n")
	fileContent.write("!     & 8D0*m2**2*m3**4 - 4D0*m3**6 )\n")
	fileContent.write("! Ossv23 = IFin*(2D0*m1**2 - 2D0*m2**2 - 4D0*m3**2) - 8D0*Iu2d3Fin*m3**2\n")
	fileContent.write("! Ossv33 = IFin*m3**2\n\n")

	fileContent.write("! Osvv11 = -2D0*IFin*m1**2 + 2D0*Iu1d2Fin*m1**2 + 2D0*Id2Fin*m1**4 + 4D0*IFin*m2**2 + 6D0*Iu1d2Fin*m2**2 + &\n")
	fileContent.write("!     & 4D0*Iu11d22Fin*m2**2 - 2D0*I22*m1**4*m2**2 - 2D0*Id2Fin*m2**4 + 4D0*I22*m1**2*m2**4 - 2D0*I22*m2**6 + 4D0*m3**2 - &\n")
	fileContent.write("!     & 2D0*Iu1d2Fin*m3**2 - 4D0*Id2Fin*m1**2*m3**2 + 4D0*I22*m1**2*m2**2*m3**2 - 20D0*I22*m2**4*m3**2 + 2D0*Id2Fin*m3**4 - &\n")
	fileContent.write("!     & 2D0*I22*m2**2*m3**4\n")
	fileContent.write("! Osvv12 = -8D0*IFin*m1**2 - 2D0*Iu3d2Fin*m1**2 - 2D0*Iu2d3Fin*m1**2 + 4D0*Id2Fin*m1**4 + 4D0*Id3Fin*m1**4 - 2D0*I23*m1**6 &\n")
	fileContent.write("!     & + 4D0*IFin*m2**2 - 6D0*Iu3d2Fin*m2**2 + 2D0*Iu2d3Fin*m2**2 - 4D0*Id2Fin*m1**2*m2**2 - 8D0*Id3Fin*m1**2*m2**2 + &\n")
	fileContent.write("!     & 6D0*I23*m1**4*m2**2 + 4D0*Id3Fin*m2**4 - 6D0*I23*m1**2*m2**4 + 2D0*I23*m2**6 + 4D0*IFin*m3**2 + 2D0*Iu3d2Fin*m2**2 - &\n")
	fileContent.write("!     & 6D0*Iu2d3Fin*m3**2 - 8D0*Id2Fin*m1**2*m3**2 - 4D0*Id3Fin*m1**2*m3**2 + 6D0*I23*m1**4*m3**2 + 20D0*Id2Fin*m2**2*m3**2 &\n")
	fileContent.write("!     & + 20D0*Id3Fin*m2**2*m3**2 - 28D0*I23*m1**2*m2**2*m3**2 + 22D0*I23*m2**4*m3**2 + 4D0*Id2Fin*m3**4 - &\n")
	fileContent.write("!     & 6D0*I23*m1**2*m3**4 + 22D0*I23*m2**2*m3**4 + 2D0*I23*m3**6\n")
	fileContent.write("! Osvv22 = -4D0*IFin*m1**2 - 2D0*Iu2d3Fin*m1**2 + 2D0*Id3Fin*m1**4 + 6D0*IFin*m2**2 + 2D0*Iu2d3Fin*m2**2 - &\n")
	fileContent.write("!     & 4D0*Id3Fin*m1**2*m2**2 + 2D0*Id3Fin*m2**4 + 2D0*IFin*m3**2 + 2D0*Iu2d3Fin*m2**2 + 4D0*Iu22d33Fin*m3**2 - &\n")
	fileContent.write("!     & 2D0*I33*m1**4*m3**2 + 4D0*I33*m1**2*m2**2*m3**2 - 2D0*I33*m2**4*m3**2 - 2D0*Id3Fin*m3**4 + 4D0*I33*m1**2*m3**4 - &\n")
	fileContent.write("!     & 20D0*I33*m2**2*m3**4 - 2D0*I33*m3**6\n\n")

	fileContent.write("! Osss11 = -4D0*Id2Fin - 8D0*I22*m2**2\n")
	fileContent.write("! Osss12 = 4D0*Id2Fin + 4D0*Id3Fin + I23*(-8D0*m1**2 + 8D0*m2**2 + 8D0*m3**2)\n")
	fileContent.write("! Osss22 = -4D0*Id3Fin - 8D0*I33*m3**2\n\n")

	fileContent.write("! OLLFull = 0D0\n")
	fileContent.write("! OLRFull = 0D0\n\n")

	fileContent.write("! Qssv0 = 0D0\n")
	fileContent.write("! Qssv1 = 0D0\n")
	fileContent.write("! Qssv2 = 0D0\n\n")

	fileContent.write("! OssvFull = Qssv0*Qssv0*Ossv00 + Qssv0*Qssv1*Ossv01 + Qssv1*Qssv1*Ossv11 + Qssv0*Qssv2*Ossv02 + Qssv1*Qssv2*Ossv12 + &\n")
	fileContent.write("!    & Qssv2*Qssv2*Ossv22 + Qssv0*(Qssv0 + Qssv1)*Ossv03 + Qssv1*(Qssv0 + Qssv1)*Ossv13 + Qssv2*(Qssv0 + Qssv1)*Ossv23 + &\n")
	fileContent.write("!    & (Qssv0 + Qssv1)**2*Ossv33\n\n")
	
	fileContent.write("! Qsvv1 = 0D0\n")
	fileContent.write("! Qsvv2 = 0D0\n\n")

	fileContent.write("! QsvvFull = Qsvv1*Qsvv1*Osvv11 + Qsvv1*Qsvv2*Osvv12 + Qsvv2*Qsvv2*Osvv22\n\n")

	fileContent.write("! Qsss1 = 0D0\n")
	fileContent.write("! Qsss2 = 0D0\n\n")

	fileContent.write("! OsssFull = Qsss1*Qsss1*Osss11 + Qsss1*Qsss2*Osss12 + Qsss2*Qsss2*Osss22\n\n")

	fileContent.write(" totalAmplitude = 1D0*1D0*(EL2/(2D0*(4D0*PI)**3*m1))*&\n")
	fileContent.write("     &(16D0*PI*m1**3)/DSQRT(m1**4 + m2**4 + m3**4 - 2D0*m1**2*m2**2 - 2D0*m1**2*m3**2 - 2D0*m2**2*m3**2 )*&\n")
	fileContent.write("     &( 1D0*( OLLFull ) + 1D0*( OLRFull ) )\n\n")

	fileContent.write(" " + processId + "Real = totalAmplitude\n")
	fileContent.write("end function " + processId + "Real\n")
	fileContent.close()

def createCountertermFile(pathToFile, processId):
	fileContent = open(pathToFile, 'w')
	fileContent.truncate()
	fileContent.write("double precision function " + processId + "CT(x)\n")
	fileContent.write(" use constants\n")
	fileContent.write(" use counterterms\n")
	fileContent.write(" implicit none\n")
	fileContent.write('#include "looptools.h"')
	fileContent.write("\n integer, intent(in) :: x\n")
	fileContent.write(" double precision :: totalAmplitude\n\n")
	fileContent.write(" select case (x)\n")
	for m in range(1,15):
		fileContent.write("\tcase (" + str(m) + ")\n")
		fileContent.write("\t\ttotalAmplitude = 0D0\n")
	fileContent.write("\tcase default\n")
	fileContent.write("\t\ttotalAmplitude = 0D0\n")
	fileContent.write(" end select\n\n")
	fileContent.write(" " + processId + "CT = totalAmplitude\n")
	fileContent.write("end function " + processId + "CT\n")
	fileContent.close()

def createDecayWidthFile(pathToFile, processId, symmetryFactor, kinematicFactor1, kinematicFactor2, kinematicFactor3, pathToTempResults):
	sourceFile = "Install" + os.sep + "decayWidthPrototype.F90"
	with open(sourceFile, "rt") as fin:
		with open(pathToFile, "wt") as fout:
			for line in fin:
				templine = line.replace('PLACEHOLDERPROCESS', processId)
				templine = templine.replace('PLACEHOLDERSYMMETRY', symmetryFactor)
				templine = templine.replace('PLACEHOLDERKINEMATIC1', kinematicFactor1)
				templine = templine.replace('PLACEHOLDERKINEMATIC2', kinematicFactor2)
				templine = templine.replace('PLACEHOLDERKINEMATIC3', kinematicFactor3)
				templine = templine.replace('PLACEHOLDERTEMPRESULTPATH', pathToTempResults)
				fout.write(templine)

def createGetParameterFile(pathToParameters):
	sourceFile = "Install" + os.sep + "getParametersPrototype.F90"
	targetFile = "Parameters" + os.sep + "getParameters.F90"
	with open(sourceFile, "rt") as fin:
		with open(targetFile, "wt") as fout:
			for line in fin:
				templine = line.replace('PLACEHOLDERRESULTPATH', pathToParameters)
				fout.write(templine)

def particlesToFeynarts(incoming,outgoing):
	'''
		Transforms lists of incoming and outgoing particles in FeynArts text form to FeynArts input form (e.g. HH -> S[2], Wm -> V[3], and so on).
		Return: lists of incoming and outgoing particles in FeynArts input form.
	'''
	replacer = {"A": "V[1]", "Z0": "V[2]", "Wm": "V[3]", "Wp": "-V[3]", "h0": "S[1]", "HH": "S[2]", "A0": "S[3]", "G0": "S[4]", "Hm": "S[5]", "Gm": "S[6]", "Hp": "-S[5]", "Gp": "-S[6]", "NeuE": "F[1,{1}]", "NeuEBar": "-F[1,{1}]", "NeuM": "F[1,{2}]", "NeuMBar": "-F[1,{2}]", "NeuT": "F[1,{3}]", "NeuTBar": "-F[1,{3}]", "El": "F[2,{1}]", "ElBar": "-F[2,{1}]", "Mu": "F[2,{2}]", "MuBar": "-F[2,{2}]", "Tau": "F[2,{3}]", "TauBar": "-F[2,{3}]", "D": "F[4,{1}]", "DBar": "-F[4,{1}]", "U": "F[3,{1}]", "UBar": "-F[3,{1}]", "S": "F[4,{2}]", "SBar": "-F[4,{2}]", "C": "F[3,{2}]", "CBar": "-F[3,{2}]", "B": "F[4,{3}]", "BBar": "-F[4,{3}]", "T": "F[3,{3}]", "TBar": "-F[3,{3}]"}
	incomingFA = []
	outgoingFA = []

	for i in range(0,len(incoming)):
		incomingFA.append(replacer[incoming[i]])
	for i in range(0,len(outgoing)):
		outgoingFA.append(replacer[outgoing[i]])

	return([incomingFA,outgoingFA])
