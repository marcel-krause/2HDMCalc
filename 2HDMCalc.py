#!/usr/bin/env python
#Filename: 2HDMCalc.py


 ###################################################################################
#                                                                                   #
#                                    2HDMCalc                                       #
#                                                                                   #
#    Purpose:    Calculate amplitudes and decay rates for 2HDM processes at NLO.    #
#    Author:     Dr. Marcel Krause (marcel.krause@alumni.kit.edu)                   #
#    Version:    1.0.3                                                              #
#    Date:       06.12.2019                                                         #
#                                                                                   #
 ###################################################################################


#------------------------------#
#         Import Modules       #
#------------------------------#
import sys
import os
from shutil import copyfile, rmtree
import subprocess
import multiprocessing
import CommonFunctions            # Provides common, often used functions for different scripts of 2HDMCalc
from Configuration import *       # Load the configuration file
from ParticleContent import particles2HDM, particleMasses    # Load the particle content and masses of the model


#-------------------------#
#         Functions       #
#-------------------------#
def validInput(candidateInput):
	'''
		Check if the user input is a valid process for 2HDMCalc. This checks if
			1. all particles entered are part of the 2HDM,
			2. it is a valid topology for 2HDMCalc (at the moment, only 1 -> 2 is supported).
		Returns a list of lists of the following form: {{AAParticle, BBParticle, ...}, {XXParticle, YYParticle, ...}, {AAValid, BBValid, ...}, {XXValid, YYValid, ...}},
		where the first two lists, the "Particle" entries, are strings with the respective incoming and outgoing particles (or empty strings if the particle is not specified)
		and the second two lists, the "Valid" entries, are Boolean values indicating if the particles exist within the 2HDM or not.

		Note that this function does NOT explicitly check if the process itself is possible in the 2HDM, i.e. if the necessary couplings exists, if the (electric) charge is conserved, and so on.
		It just checks the validity of the input with respect to syntax and 2HDMCalc functionality.
	'''
	# Remove all whitespaces from the input
	candidateProcess = candidateInput.replace(" ","")

	# Get the incoming and outgoing particles
	candidateList = candidateProcess.split("to")
	incomingParticles = (candidateList[0]).split(",")
	outgoingParticles = (candidateList[1]).split(",")

	# Check if incoming and outgoing particles are present
	noError = True
	if incomingParticles[0] == "":
		print("Error: no incoming particle(s) specified. Expected at least one paricle.\n")
	if outgoingParticles[0] == "":
		print("Error: no outgoing particle(s) specified. Expected at least one paricle.\n")

	# If no incoming or outgoing particles are specified, we tell the user and continue to the next while loop instance.
	if not noError:
		return({False,{},{}})

	# If incoming and outgoing particles are specified, check if all particles are part of the 2HDM
	for i in range(0,len(incomingParticles)):
		if incomingParticles[i] not in particles2HDM:
			noError = False
			print('\nError: the incoming particle {0} does not live inside the 2HDM.'.format(incomingParticles[i]))
	for i in range(0,len(outgoingParticles)):
		if outgoingParticles[i] not in particles2HDM:
			noError = False
			print('\nError: the outgoing particle {0} does not live inside the 2HDM.'.format(outgoingParticles[i]))

	# If at least one particle is not part of the 2HDM, we tell the user and continue to the next while loop instance. Otherwise, we return the particles.
	if noError:
		return([True,incomingParticles,outgoingParticles])
	else:
		return([False,[],[]])

def calcTreeLevelAmplitude(processFAID, processId):
	'''
		Calculate the tree-level amplitude to a given process, defined by processFAID in FeynArts input format.
		Generates the tree-level amplitude with FeynArts.
		Calculates and simplifies the amplitude with FeynCalc and saves it in the directory specified by processId.
	'''
	# Create the temporary amplitude folders
	if os.path.exists("Temp" + os.sep + "tree"):	# Check if the tree folder exists (normally at this point, it should not) and if so, delete it
		delPath = "Temp" + os.sep + "tree"
		rmtree(delPath)
	os.makedirs("Temp" + os.sep + "tree")		# Recreate the tree folder
	if os.path.exists("Temp" + os.sep + "treeRes"):	# Check if the treeRes folder exists (normally at this point, it should not) and if so, delete it
		delPath = "Temp" + os.sep + "treeRes"
		rmtree(delPath)
	os.makedirs("Temp" + os.sep + "treeRes")		# Recreate the treeRes folder

	# Create the amplitude with FeynArts
	print('Generating the tree-level amplitude ...\n')
	prompt = [pathMathematica, '-noprompt', '-run', "<<Install" + os.sep + "TreeLevelFA.m", '-' + processFAID]
	subprocess.call(prompt, stdin=None, stdout=None, stderr=None, shell=False, timeout=None)
	print('\nTree-level amplitude generated.\n')

	# Calculate the amplitude with FeynCalc
	print('Calculating the tree-level amplitude ...\n')
	prompt = [pathMathematica, '-noprompt', '-run', "<<Install" + os.sep + "TreeLevelFC.m"]
	subprocess.call(prompt, stdin=None, stdout=None, stderr=None, shell=False, timeout=None)
	print('\nCalculation of the tree-level amplitude is finished.\n')

	# Move the calculated files to the process directory
	os.rename("Temp" + os.sep + "treeRes" + os.sep + "amp.txt", "BuildingBlocks" + os.sep + "Processes" + os.sep + processId + os.sep + "TreeLevel" + os.sep + "tree.txt")
	os.rename("Temp" + os.sep + "treeRes" + os.sep + "ampConj.txt", "BuildingBlocks" + os.sep + "Processes" + os.sep + processId + os.sep + "TreeLevel" + os.sep + "treeConj.txt")

	# Delete the temporary directories
	delPath = "Temp" + os.sep + "tree"
	rmtree(delPath)
	delPath = "Temp" + os.sep + "treeRes"
	rmtree(delPath)

def calcTreeLevelAmpSquared(processId, mIn1, mOut1, mOut2):
	'''
		Calculate the reduced tree-level decay width to a given process, defined by processId in our directory format.
		The result is the complex-squared tree-level amplitude with polarization sums inserted.
		Note that NEITHER the kinematic NOR the symmetry factor are included in this result!
	'''
	# Check if the tree-level amplitude and its complex-conjugate already exist; if not, end the calculation of the decay width
	if not os.path.isfile("BuildingBlocks" + os.sep + "Processes" + os.sep + processId + os.sep + "TreeLevel" + os.sep + "tree.txt"):
		print("ERROR: tree-level amplitude not found. Please calculate the tree-level amplitude before calculating the tree-level decay width.\n")
		return None
	if not os.path.isfile("BuildingBlocks" + os.sep + "Processes" + os.sep + processId + os.sep + "TreeLevel" + os.sep + "treeConj.txt"):
		print("ERROR: complex-conjugated tree-level amplitude not found. Please calculate the tree-level amplitude before calculating the tree-level decay width.\n")
		return None

	# Calculate the decay width with FeynCalc
	prompt = [pathMathematica, '-noprompt', '-run', "<<Install" + os.sep + "TreeLevelDecayWidthFC.m", '-' + mIn1, '-' + mOut1, '-' + mOut2, '-' + processId]
	subprocess.call(prompt, stdin=None, stdout=None, stderr=None, shell=False, timeout=None)
	print('\nCalculation of the tree-level decay width is finished.\n')

def calcNLOAmpSquared(processId, mIn1, mOut1, mOut2):
	'''
		Calculate the reduced NLO decay width to a given process, defined by processId in our directory format.
		The result is the product of the complex-conjugated tree-level amplitude with the NLO amplitude with polarization sums inserted.
		Note that NEITHER the kinematic NOR the symmetry factor are included in this result!
		Note that 2*real() of the result of calcNLOAmpSquared() has to be taken in order to get the correct result in form of the tree-level-NLO-interference term.
	'''
	# Check if the complex-conjugated tree-level amplitude and NLO amplitudes already exist; if not, end the calculation of the decay width
	if not os.path.isfile("BuildingBlocks" + os.sep + "Processes" + os.sep + processId + os.sep + "TreeLevel" + os.sep + "treeConj.txt"):
		print("ERROR: complex-conjugated tree-level amplitude not found. Please calculate the tree-level amplitude before calculating the NLO decay width.\n")
		return None
	if len(os.listdir("BuildingBlocks" + os.sep + "Processes" + os.sep + processId + os.sep + "VertexCorrections")) < 1:
		print("ERROR: NLO amplitudes not found. Please calculate the NLO amplitudes before calculating the NLO decay width.\n")
		return None

	# Calculate the decay width with FeynCalc
	prompt = [pathMathematica, '-noprompt', '-run', "<<Install" + os.sep + "NLODecayWidthFC.m", '-' + mIn1, '-' + mOut1, '-' + mOut2, '-' + processId]
	subprocess.call(prompt, stdin=None, stdout=None, stderr=None, shell=False, timeout=None)
	print('\nCalculation of the NLO decay width is finished.\n')

def calcOneLoopCorrectionsFC(ampFile):
	fileNumber = (((ampFile.split(".txt"))[0]).split("amp"))[1]
	print('Calculating sub-amplitude ' + fileNumber + '...\n')
	prompt = [pathMathematica, '-noprompt', '-run', "<<Install" + os.sep + "OneLoopFC.m", '-' + ampFile]
	subprocess.call(prompt, stdin=None, stdout=None, stderr=None, shell=False, timeout=None)
	print('Sub-amplitude ' + fileNumber + ' done.\n')

def calcOneLoopCorrectionsTadpolesFC(ampFile):
	fileNumber = (((ampFile.split(".txt"))[0]).split("tads"))[1]
	print('Calculating sub-amplitude ' + fileNumber + '...\n')
	prompt = [pathMathematica, '-noprompt', '-run', "<<Install" + os.sep + "OneLoopTadpolesFC.m", '-' + ampFile]
	subprocess.call(prompt, stdin=None, stdout=None, stderr=None, shell=False, timeout=None)
	print('Sub-amplitude ' + fileNumber + ' done.\n')

def calcOneLoopCorrections(processFAID, processId, massesSaver):
	'''
		Calculate the one-loop corrections to a given process, defined by processFAID in FeynArts input format.
		Generates all one-loop amplitudes as well as tadpole contributions separately with FeynArts.
		Calculates and simplifies all amplitudes with FeynCalc and saves it in the directory specified by processId.
	'''
	# Create the temporary amplitude folders
	if os.path.exists("Temp" + os.sep + "amp"):				# Check if the amp folder exists (normally at this point, it should not) and if so, delete it
		delPath = "Temp" + os.sep + "amp"
		rmtree(delPath)
	os.makedirs("Temp" + os.sep + "amp")		# Recreate the amp folder
	if os.path.exists("Temp" + os.sep + "tads"):			# Check if the tads folder exists (normally at this point, it should not) and if so, delete it
		delPath = "Temp" + os.sep + "tads"
		rmtree(delPath)
	os.makedirs("Temp" + os.sep + "tads")		# Recreate the tads folder
	if os.path.exists("Temp" + os.sep + "ampRes"):			# Check if the ampRes folder exists (normally at this point, it should not) and if so, delete it
		delPath = "Temp" + os.sep + "ampRes"
		rmtree(delPath)
	os.makedirs("Temp" + os.sep + "ampRes")		# Recreate the ampRes folder
	if os.path.exists("Temp" + os.sep + "tadsRes"):			# Check if the tadsRes folder exists (normally at this point, it should not) and if so, delete it
		delPath = "Temp" + os.sep + "tadsRes"
		rmtree(delPath)
	os.makedirs("Temp" + os.sep + "tadsRes")		# Recreate the tadsRes folder

	# Create the amplitudes with FeynArts
	prompt = [pathMathematica, '-noprompt', '-run', "<<Install" + os.sep + "OneLoopFA.m", '-' + processFAID]
	subprocess.call(prompt, stdin=None, stdout=None, stderr=None, shell=False, timeout=None)
	prompt = [pathMathematica, '-noprompt', '-run', "<<Install" + os.sep + "OneLoopTadpolesFA.m", '-' + processFAID]
	subprocess.call(prompt, stdin=None, stdout=None, stderr=None, shell=False, timeout=None)

	# Get a list of generated amplitudes
	generatedAmps = os.listdir("Temp" + os.sep + "amp")
	generatedTads = os.listdir("Temp" + os.sep + "tads")
	generatedAmpsMod = [s + massesSaver for s in generatedAmps]
	generatedTadsMod = [t + massesSaver for t in generatedTads]

	# Get the number of cores of the system
	if useMaximumLicenses:
		numberCores = maximumMathematicaLicenses
	else:
		numberCores = multiprocessing.cpu_count()

	# Calculate all 1PI contributions in parallel
	for _ in multiprocessing.Pool(numberCores).imap_unordered(calcOneLoopCorrectionsFC, generatedAmpsMod):
		pass

	# Calculate all tadpole contributions in parallel
	for _ in multiprocessing.Pool(numberCores).imap_unordered(calcOneLoopCorrectionsTadpolesFC, generatedTadsMod):
		pass

	# Create the correct path to the result files
	# fileAmps = []
	# fileTads = []
	for partAmp in generatedAmps:
		# fileAmps.append("Temp" + os.sep + "ampRes" + os.sep + partAmp)
		copyfile("Temp" + os.sep + "ampRes" + os.sep + partAmp, "BuildingBlocks" + os.sep + "Processes" + os.sep + processId + os.sep + "VertexCorrections" + os.sep + partAmp)
	for partTads in generatedTads:
		# fileTads.append("Temp" + os.sep + "tadsRes" + os.sep + partTads)
		copyfile("Temp" + os.sep + "tadsRes" + os.sep + partTads, "BuildingBlocks" + os.sep + "Processes" + os.sep + processId + os.sep + "VertexTadpoles" + os.sep + partTads)

	# Save all 1PI results in a single file
	# counter = 1
	# with open("OneLoopResults.txt", 'w') as outfile:
	# 	for fname in fileAmps:
	# 		with open(fname) as infile:
	# 			for line in infile:
	# 				outfile.write(line)
	# 		if counter < len(fileAmps):
	# 			outfile.write(" + ")
	# 		counter += 1

	# Save all tadpole results in a single file
	# counter = 1
	# with open("OneLoopTadpolesResults.txt", 'w') as outfile:
	# 	for fname in fileTads:
	# 		with open(fname) as infile:
	# 			for line in infile:
	# 				outfile.write(line)
	# 		if counter < len(fileTads):
	# 			outfile.write(" + ")
	# 		counter += 1

	# Remove the temporary files
	delPath = "Temp" + os.sep + "amp"
	rmtree(delPath)
	delPath = "Temp" + os.sep + "ampRes"
	rmtree(delPath)
	delPath = "Temp" + os.sep + "tads"
	rmtree(delPath)
	delPath = "Temp" + os.sep + "tadsRes"
	rmtree(delPath)

def calcDecayWidthLT(parameterFile):
	# Extract the processId from the file name
	processIdExtractor = parameterFile.split('PROCESSSEPARATOR')
	processIdCurrent = processIdExtractor[0]
	parameterFileProcessed = processIdExtractor[1]

	# Extract the 2HDM type of the file
	# typeExtractor = parameterFileProcessed.split('_')
	# typeOf2HDM = typeExtractor[0].replace('type', '')

	# Create the result file name
	resultFileName = processIdCurrent + '_' + parameterFileProcessed.replace('.out', '.txt')

	# Perform the numerical evaluation
	print('Calculating decay widths for parameter set ' + parameterFileProcessed + ' ...\n')
	prompt = ['BuildingBlocks' + os.sep + 'Processes' + os.sep + processIdCurrent + os.sep + 'decayWidth', '0', '0', '0', '1', processIdCurrent + os.sep + parameterFileProcessed, resultFileName]
	subprocess.call(prompt, stdin=None, stdout=None, stderr=None, shell=False, timeout=None)
	print(parameterFileProcessed + ' done.\n')


def calcDecayUVDivergence(processId):
	'''
		Checks a given process, defined by processFAID in FeynArts input format, for UV divergences.
	'''
	# Get a list of parameter files
	parameterList = os.listdir("Parameters" + os.sep + processId)

	# If there are no parameter files given, give out an error and return to the main program
	if (len(parameterList) < 1):
		print('ERROR: no parameter files given!\n')
		return

	# Extract the 2HDM type of the file
	# typeExtractor = parameterList[0].split('_')
	# typeOf2HDM = typeExtractor[0].replace('type', '')

	# Grab the first parameter file
	parameterFile = processId + os.sep + parameterList[0]

	# Check for UV divergences
	prompt = ['BuildingBlocks' + os.sep + 'Processes' + os.sep + processId + os.sep + 'decayWidth', '1', '0', '0', '0', parameterFile]
	subprocess.call(prompt, stdin=None, stdout=None, stderr=None, shell=False, timeout=None)


def calcDecayIRDivergence(processId):
	'''
		Checks a given process, defined by processFAID in FeynArts input format, for IR divergences.
	'''
	# Get a list of parameter files
	parameterList = os.listdir("Parameters" + os.sep + processId)

	# If there are no parameter files given, give out an error and return to the main program
	if (len(parameterList) < 1):
		print('ERROR: no parameter files given!\n')
		return

	# Extract the 2HDM type of the file
	# typeExtractor = parameterList[0].split('_')
	# typeOf2HDM = typeExtractor[0].replace('type', '')

	# Grab the first parameter file
	parameterFile = processId + os.sep + parameterList[0]

	# Check for UV divergences
	prompt = ['BuildingBlocks' + os.sep + 'Processes' + os.sep + processId + os.sep + 'decayWidth', '0', '1', '0', '0', parameterFile]
	subprocess.call(prompt, stdin=None, stdout=None, stderr=None, shell=False, timeout=None)


def calcDecayGaugeDependence(processId):
	'''
		Checks a given process, defined by processFAID in FeynArts input format, for gauge dependences.
	'''
	# Get a list of parameter files
	parameterList = os.listdir("Parameters" + os.sep + processId)

	# If there are no parameter files given, give out an error and return to the main program
	if (len(parameterList) < 1):
		print('ERROR: no parameter files given!\n')
		return

	# Extract the 2HDM type of the file
	# typeExtractor = parameterList[0].split('_')
	# typeOf2HDM = typeExtractor[0].replace('type', '')

	# Grab the first parameter file
	parameterFile = processId + os.sep + parameterList[0]

	# Check for UV divergences
	prompt = ['BuildingBlocks' + os.sep + 'Processes' + os.sep + processId + os.sep + 'decayWidth', '0', '0', '1', '0', parameterFile]
	subprocess.call(prompt, stdin=None, stdout=None, stderr=None, shell=False, timeout=None)


def calcDecayWidthFull(processId):
	'''
		Calculate the decay width to a given process, defined by processFAID in FeynArts input format.
		Calculates the decay width with LoopTools.
	'''
	# Check if the temporary results folder exists (normally at this point, it should not) and if so, delete it
	if os.path.exists("Temp" + os.sep + "Results"):
		delPath = "Temp" + os.sep + "Results"
		rmtree(delPath)
	os.makedirs("Temp" + os.sep + "Results")		# Recreate the results folder

	# Create the result directory
	CommonFunctions.create_dir_if_not_exist("Results")
	CommonFunctions.create_dir_if_not_exist("Results" + os.sep + processId)

	# Get a list of parameter files
	parameterList = os.listdir("Parameters" + os.sep + processId)

	# Modify the parameter names so that they contain the processId as well
	for i in range(0, len(parameterList)):
		parameterList[i] = processId + "PROCESSSEPARATOR" + parameterList[i]

	# Get the number of cores of the system (TODO: hyperthreading issue? Actually slowing down?)
	numberCores = multiprocessing.cpu_count()

	# Calculate all 1PI contributions in parallel
	for _ in multiprocessing.Pool(numberCores).imap_unordered(calcDecayWidthLT, parameterList):
		pass

	# Get the list of all results files
	resultsFiles = os.listdir("Temp" + os.sep + "Results")

	# Convert all result files and store them in the correct directory
	resultsFilesComplete = ''
	for i in range(0, len(resultsFiles)):
		# Create the file name
		filename = "Temp" + os.sep + "Results" + os.sep + resultsFiles[i]

		# Replace the newline character in each file with a proper newline
		fileHandler = open(filename, "r")
		convertedFile = ''
		convertedFileToAll = ''
		for line in fileHandler:
			# Convert the literal newlines to actual ones
			convertedFile += line.replace('\\n', '\n')

			# Skip the first line in all other files than the first, since it contains only the key which is already present
			if (i < 1):
				numberOfLinebreaks = len(line.split('\\n'))
				convertedFileToAll += (line.replace('\\n', '\n', numberOfLinebreaks-2)).replace('\\n', '')
			else:
				numberOfLinebreaks = len(line.split('\\n'))
				convertedFileToAll += (((line.split('\\n', 1))[1]).replace('\\n', '\n', numberOfLinebreaks-3)).replace('\\n', '')
		fileHandler.close()

		# Store the results file in the correct directory
		fileHandler = open("Results" + os.sep + processId + os.sep + resultsFiles[i], "w+")
		fileHandler.write(convertedFile)
		fileHandler.close()

		# Store the content of all files in a single variable
		resultsFilesComplete += convertedFileToAll

	# Save the complete result file
	filenameAddendum = (resultsFiles[i].split('_'))[0] + '_' + (resultsFiles[i].split('_'))[1] + '.txt'
	fileHandler = open("Results" + os.sep + processId + os.sep + filenameAddendum, "w+")
	fileHandler.write(resultsFilesComplete)
	fileHandler.close()

	# Remove the temporary files
	delPath = "Temp"
	rmtree(delPath)

#----------------------------#
#		 Main Program		 #
#----------------------------#

if __name__ == "__main__":		# This is necessary for correct parallelisation under Windows (Windows does not know fork)
	# Print the welcome screen
	print('''
	+---------------------------------------+
	|                                       |
	|             2HDMCalc 1.0.3            |
	|                                       |
	|                             /         |
	|                            /          |
	|                           /           |
	|                     ---- /            |
	|      _____________/ 2HDM \            |
	|                   \ CALC /            |
	|                     ---- \            |
	|                           \           |
	|                            \          |
	|                             \         |
	|                                       |
	+---------------------------------------+
	''')

	# Tell the user if the program is running in silent mode
	if silentMode2HDMCalc:
		print("SilentMode is activated. Using default settings.\n")

	# Call the installation script, if required.
	if silentMode2HDMCalc:
		execInstall = defExecInstall
	else:
		execInstall = CommonFunctions.queryBoolean(">>> Do you want to execute the installation script?")

	if execInstall:
		import Install
	else:
		print("Installation script skipped.\n")

	# Ask for the process that shall be calculated. If none is given, 2HDMCalc will terminate since there is nothing to do.
	if silentMode2HDMCalc:
		askProcess = defAskProcess
	else:
		askProcess = CommonFunctions.queryBoolean(">>> Do you want to specify a process to be calculated?")

	if not askProcess:
		print("\nNo process specified. 2HDMCalc will terminate now.\n")
		sys.exit()
	else:
		quitter = {"q", "quit", "exit", "leave"}
		while True:			# Loop to ask the user for the process they want to calculate.
			processCandidate = input('\n>>> Enter the process you want to calculate. Type "help" for showing the syntax.\n')
			if processCandidate.lower() in quitter:		# Escape sequence defined in quitter, gives the user the chance to quit the script.
					print("No process specified. 2HDMCalc will terminate now.\n")
					sys.exit()
			elif processCandidate.lower() == "help":	# Prints a small help showing the input syntax and particles of the 2HDM.
				print('''
	A process has to be entered in the following way:
		AA to XX,YY

		AA: incoming particle.
		XX, YY: outgoing particles.

	The following particles can be used only (FeynArts notation):
		Higgs sector: HH, h0, A0, G0, Hp, Hm, Gp, Gm
		Vector bosons: A, Z0, Wp, Wm
		Fermions: NeuE, NeuM, NeuT, El, Mu, Tau, D, U, S, C, B, T
		Antifermions: NeuEBar, NeuMBar, NeuTBar, ElBar, MuBar, TauBar, DBar, UBar, SBar, CBar, BBar, TBar

	Example 1. To calculate the decay of a Z boson to the scalars A0 and h0, enter:
		Z0 to A0,h0

	Example 2. To calculate the decay of a Higgs particle H0 to a pair of W bosons, enter:
		H0 to Wp,Wm
	
	Example 3. To calculate the decay of a charged Higgs Hp to an anti-Tau and a tau-neutrino, enter:
		Hp to TauBar,NeuT

	Enter 'q' to terminate 2HDMCalc.
				''')
			else:		# If no escape sequence or help is entered, it is assumed that the user actually provided a possible process to calculate.
				processQuery = validInput(processCandidate)
				if processQuery[0]:
					break
				else:
					print('\nInvalid process. Type "help" for more information.\n')

		# If we reach this point, the user has entered a process with valid syntax and we can continue.
		if os.path.exists("Temp"):			# Check if the Temp folder exists (normally at this point, it should not) and if so, delete it
			delPath = "Temp"
			rmtree(delPath)
		os.makedirs("Temp")		# Recreate the Temp folder

		# Sort the incoming and outgoing particles in a unique way so that an interchange of two particles does not matter.
		processQuery[1].sort()
		processQuery[2].sort()

		# Generate the process for input in Mathematica
		processFA = CommonFunctions.particlesToFeynarts(processQuery[1],processQuery[2])

		# Generate the output for the user to confirm the process (this output is also used as the "id" for the process)
		incomingForm = ""
		outgoingForm = ""
		incomingFAForm = ""
		outgoingFAForm = ""
		massesSaver = "SEPARATORINC"
		massesIn = []
		massesOut = []
		for i in range(0,len(processQuery[1])):
			incomingForm += processQuery[1][i]
			if i > 0:
				incomingFAForm += ","
			massesSaver += particleMasses[processQuery[1][i]]
			massesIn.append(particleMasses[processQuery[1][i]])
			incomingFAForm += processFA[0][i]
		massesSaver += "OUT"
		for i in range(0,len(processQuery[2])):
			outgoingForm += processQuery[2][i]
			if i > 0:
				outgoingFAForm += ","
				massesSaver += "MASSSEP"
			massesSaver += particleMasses[processQuery[2][i]]
			massesOut.append(particleMasses[processQuery[2][i]])
			outgoingFAForm += processFA[1][i]

		# Create a string containing process information that will be saved in a text file in the process folder (used in 2HDECAY for the automated calculation of the decay widths)
		processString = ""
		for pickParticle in processQuery[1]:
			processString += pickParticle + ","
		processString = processString[:-1] + " -> "
		for pickParticle in processQuery[2]:
			processString += pickParticle + ","
		processString = processString[:-1] + '\n'
		for pickMass in massesIn:
			processString += pickMass + ","
		for pickMass in massesOut:
			processString += pickMass + ","
		processString = processString[:-1]

		# Get the symmetry factor (two identical outgoing particles: 2!)
		if processQuery[2][0] == processQuery[2][1]:
			symmetryFactor = 2
		else:
			symmetryFactor = 1

		print('\nThe process {0} -> {1} has valid syntax.\n'.format(incomingForm,outgoingForm))
		processId = incomingForm + "to" + outgoingForm		# Unique process id in the form AABBtoXXYY
		processToMathematica = incomingFAForm + "to" + outgoingFAForm	# Same process id, but in the input format for FeynArts

		# Create a unique dir for the process
		CommonFunctions.create_dir_if_not_exist("BuildingBlocks" + os.sep + "Processes" + os.sep + processId)
		CommonFunctions.create_dir_if_not_exist("BuildingBlocks" + os.sep + "Processes" + os.sep + processId + os.sep + "TreeLevel")
		CommonFunctions.create_dir_if_not_exist("BuildingBlocks" + os.sep + "Processes" + os.sep + processId + os.sep + "VertexCorrections")
		CommonFunctions.create_dir_if_not_exist("BuildingBlocks" + os.sep + "Processes" + os.sep + processId + os.sep + "VertexTadpoles")

		# Print the process information to a text file
		tempFile = open("BuildingBlocks" + os.sep + "Processes" + os.sep + processId + os.sep + "processDescription.txt",'w')
		tempFile.write(processString)
		tempFile.close()

		##########################
		#  Tree-Level Amplitude  #
		##########################
		# Check if the tree-level amplitude was already calculated before
		calcTreeLevelAnew = False
		calcTreeLevelIfNotExists = False
		if os.path.isfile("BuildingBlocks" + os.sep + "Processes" + os.sep + processId + os.sep + "TreeLevel" + os.sep + "tree.txt"):
			# Ask the user if they want to recalculate the tree-level amplitude
			if silentModeCalcTreeLevel:
				calcTreeLevelAnew = defCalcTreeLevelAnew
			else:
				calcTreeLevelAnew = CommonFunctions.queryBoolean(">>> The tree-level amplitude was calculated before. Do you want to calculate it anew?")
		else:
			# Ask the user if they want to calculate the tree-level amplitude
			if silentModeCalcTreeLevel:
				calcTreeLevelIfNotExists = defCalcTreeLevelIfNotExists
			else:
				calcTreeLevelIfNotExists = CommonFunctions.queryBoolean(">>> The tree-level amplitude of this process was not yet calculated. Do you want to calculate it now?")

		# Calculate the tree-level amplitude, if necessary; otherwise, turn to the one-loop corrections
		if calcTreeLevelAnew or calcTreeLevelIfNotExists:
			print("Calculating the tree-level amplitude ...\n")
			if calcTreeLevelAnew:
				delPath = "BuildingBlocks" + os.sep + "Processes" + os.sep + processId + os.sep + "TreeLevel" + os.sep + "tree.txt"
				os.remove(delPath)
				delPath = "BuildingBlocks" + os.sep + "Processes" + os.sep + processId + os.sep + "TreeLevel" + os.sep + "treeConj.txt"
				os.remove(delPath)
			calcTreeLevelAmplitude(processToMathematica, processId)
		else:
			print("The tree-level amplitude will not be calculated.\n")


		##########################
		#  One-Loop Corrections  #
		##########################
		# Check if the tree-level amplitude was already calculated before
		calcOneLoopAnew = False
		calcOneLoopIfNotExists = False
		if len(os.listdir("BuildingBlocks" + os.sep + "Processes" + os.sep + processId + os.sep + "VertexCorrections")) >= 1:
			# Ask the user if they want to recalculate the one-loop amplitude
			if silentModeCalcOneLoop:
				calcOneLoopAnew = defCalcOneLoopAnew
			else:
				calcOneLoopAnew = CommonFunctions.queryBoolean(">>> The one-loop amplitudes were calculated before. Do you want to calculate them anew?")
		else:
			# Ask the user if they want to calculate the one-loop amplitude
			if silentModeCalcOneLoop:
				calcOneLoopIfNotExists = defCalcOneLoopIfNotExists
			else:
				calcOneLoopIfNotExists = CommonFunctions.queryBoolean(">>> The one-loop amplitudes of this process were not yet calculated. Do you want to calculate them now?")

		# Calculate the one-loop amplitude, if necessary; otherwise, turn to the creation of the counterterm
		if calcOneLoopAnew or calcOneLoopIfNotExists:
			print("Calculating the one-loop amplitudes ...\n")
			if calcOneLoopAnew:
				# prompt = "BuildingBlocks" + os.sep + "Processes" + os.sep + processId + os.sep + "VertexCorrections" + os.sep + "*.txt"
				# os.remove(delPath)
				# prompt = "BuildingBlocks" + os.sep + "Processes" + os.sep + processId + os.sep + "VertexTadpoles" + os.sep + "*.txt"
				# os.remove(delPath)

				# Delete all files created before if the amplitude shall be calculated anew
				folder = "BuildingBlocks" + os.sep + "Processes" + os.sep + processId + os.sep + "VertexCorrections"
				for theFile in os.listdir(folder):
				    filePath = os.path.join(folder, theFile)
				    try:
				        if os.path.isfile(filePath):
				            os.unlink(filePath)
				    except Exception as e:
				        print(e)
				folder = "BuildingBlocks" + os.sep + "Processes" + os.sep + processId + os.sep + "VertexTadpoles"
				for theFile in os.listdir(folder):
				    filePath = os.path.join(folder, theFile)
				    try:
				        if os.path.isfile(filePath):
				            os.unlink(filePath)
				    except Exception as e:
				        print(e)
			calcOneLoopCorrections(processToMathematica, processId, massesSaver)
		else:
			print("The one-loop amplitudes will not be calculated.\n")


		##################
		#  Counter-Term  #
		##################
		# Construct the full NLO amplitude

		# Delete the Temp folder again
		# delPath = "Temp"
		# rmtree(delPath)


		##############################################################
		#  Add real corrections, polarization rules and Counterterm  #
		##############################################################
		# Prompt the user to add the real corrections, counterterm and polarization rules by hand to the process folder and ask if the program should continue
		# WARNING: without the polarization sum, the real corrections and the counterterm, the compilation of the next step in the program will fail
		pathToFiles = "BuildingBlocks" + os.sep + "Processes" + os.sep + processId + os.sep
		if not (os.path.isfile(pathToFiles + "Counterterm.F90")):
			CommonFunctions.createCountertermFile(pathToFiles + "Counterterm.F90", processId)
		if not (os.path.isfile(pathToFiles + "RealCorrections.F90")):
			CommonFunctions.createRealCorrections(pathToFiles + "RealCorrections.F90", processId, massesIn[0], massesOut[0], massesOut[1])
		# Ask the user if they want to continue
		if silentModeCalcProceed:
			calcWantProceed = defCalcProceed
		else:
			calcWantProceed = CommonFunctions.queryBoolean(">>> Please add the real corrections, counterterm and polarization rules by hand to the process folder. Do you want to proceed?")
		if not calcWantProceed:
			print("Program stopped by user. 2HDMCalc will be terminated now.\n")
			sys.exit()


		####################################
		#  Reduced Tree-Level Decay Width  #
		####################################
		print('')
		# Check if the tree-level decay width was already calculated before
		calcTreeLevelDecayWidthAnew = False
		calcTreeLevelDecayWidthIfNotExists = False
		if os.path.isfile("BuildingBlocks" + os.sep + "Processes" + os.sep + processId + os.sep + "TreeLevelWidthRed.F90"):
			# Ask the user if they want to recalculate the tree-level decay width
			if silentModeCalcTreeLevelDecayWidth:
				calcTreeLevelDecayWidthAnew = defCalcTreeLevelDecayWidthAnew
			else:
				calcTreeLevelDecayWidthAnew = CommonFunctions.queryBoolean(">>> The reduced tree-level decay width was calculated before. Do you want to calculate it anew?")
		else:
			# Ask the user if they want to calculate the tree-level decay width
			if silentModeCalcTreeLevelDecayWidth:
				calcTreeLevelDecayWidthIfNotExists = defCalcTreeLevelDecayWidthIfNotExists
			else:
				calcTreeLevelDecayWidthIfNotExists = CommonFunctions.queryBoolean(">>> The reduced tree-level decay width of this process was not yet calculated. Do you want to calculate it now?")

		# Calculate the reduced tree-level decay width, if necessary; otherwise, turn to the one-loop corrections
		if calcTreeLevelDecayWidthAnew or calcTreeLevelDecayWidthIfNotExists:
			print("Calculating the reduced tree-level decay width ...\n")
			if calcTreeLevelDecayWidthAnew:
				delPath = "BuildingBlocks" + os.sep + "Processes" + os.sep + processId + os.sep + "TreeLevelWidthRed.F90"
				os.remove(delPath)
			calcTreeLevelAmpSquared(processId, massesIn[0], massesOut[0], massesOut[1])
		else:
			print("The reduced tree-level decay width will not be calculated.\n")


		#############################
		#  Reduced NLO Decay Width  #
		#############################
		# Check if the NLO decay width was already calculated before
		calcNLODecayWidthAnew = False
		calcNLODecayWidthIfNotExists = False
		if os.path.isfile("BuildingBlocks" + os.sep + "Processes" + os.sep + processId + os.sep + "NLOWidthRed.F90"):
			# Ask the user if they want to recalculate the NLO decay width
			if silentModeCalcNLODecayWidth:
				calcNLODecayWidthAnew = defCalcNLODecayWidthAnew
			else:
				calcNLODecayWidthAnew = CommonFunctions.queryBoolean(">>> The reduced NLO decay width was calculated before. Do you want to calculate it anew?")
		else:
			# Ask the user if they want to calculate the NLO decay width
			if silentModeCalcNLODecayWidth:
				calcNLODecayWidthIfNotExists = defCalcNLODecayWidthIfNotExists
			else:
				calcNLODecayWidthIfNotExists = CommonFunctions.queryBoolean(">>> The reduced NLO decay width of this process was not yet calculated. Do you want to calculate it now?")

		# Calculate the reduced NLO decay width, if necessary; otherwise, turn to the one-loop corrections
		if calcNLODecayWidthAnew or calcNLODecayWidthIfNotExists:
			print("Calculating the reduced NLO decay width ...\n")
			if calcNLODecayWidthAnew:
				delPath = "BuildingBlocks" + os.sep + "Processes" + os.sep + processId + os.sep + "NLOWidthRed.F90"
				os.remove(delPath)
			calcNLOAmpSquared(processId, massesIn[0], massesOut[0], massesOut[1])
		else:
			print("The reduced NLO decay width will not be calculated.\n")


		##########################
		#  Full NLO Decay Width  #
		##########################
		# Check if the full NLO decay width was already calculated before
		calcFullDecayWidthAnew = False
		calcFullDecayWidthIfNotExists = False
		if os.path.isfile("BuildingBlocks" + os.sep + "Processes" + os.sep + processId + os.sep + "decayWidth.F90"):
			# Ask the user if they want to recalculate the tree-level decay width
			if silentModeCalcTreeLevelDecayWidth:
				calcFullDecayWidthAnew = defCalcFullDecayWidthAnew
			else:
				calcFullDecayWidthAnew = CommonFunctions.queryBoolean(">>> The full NLO decay width was calculated before. Do you want to calculate it anew?")
		else:
			# Ask the user if they want to calculate the tree-level decay width
			if silentModeCalcTreeLevelDecayWidth:
				calcFullDecayWidthIfNotExists = defCalcFullDecayWidthIfNotExists
			else:
				calcFullDecayWidthIfNotExists = CommonFunctions.queryBoolean(">>> The full NLO decay width of this process was not yet calculated. Do you want to calculate it now?")

		# Calculate the full decay width, if necessary
		if calcFullDecayWidthAnew or calcFullDecayWidthIfNotExists:
			print("Calculating the full NLO decay width ...\n")

			pathToFile = "BuildingBlocks" + os.sep + "Processes" + os.sep + processId + os.sep + "decayWidth.F90"

			if calcFullDecayWidthAnew:
				os.remove(pathToFile)

			# Specify the kinematic factors needed in the decayWidth.F90 prototype
			kinematicFactor1 = massesIn[0] + "**4 + " + massesOut[0] + "**4 + " + massesOut[1] + "**4 - 2D0*" + massesIn[0] + "**2*" + massesOut[0] + "**2 - 2D0*" + massesIn[0] + "**2*" + massesOut[1] + "**2"
			kinematicFactor2 = " - 2D0*" + massesOut[0] + "**2*" + massesOut[1] + "**2"
			kinematicFactor3 = massesIn[0]

			# Create decayWidth.F90
			if os.name == 'nt':
				pathToResTemp = 'Temp\\\\Results\\\\'
			else:
				pathToResTemp = 'Temp' + os.sep + 'Results' + os.sep
			CommonFunctions.createDecayWidthFile(pathToFile, processId, str(symmetryFactor), kinematicFactor1, kinematicFactor2, kinematicFactor3, pathToResTemp)

			# Set the path of the getParameters.F90 file accordingly to the current OS 
			if os.name == 'nt':
				pathToParameters = 'Parameters\\\\'
			else:
				pathToParameters = 'Parameters' + os.sep
			CommonFunctions.createGetParameterFile(pathToParameters)

			# Create the makefile and execute it
			CommonFunctions.createMakefile("makefile", pathLoopTools, pathLoopToolsLibs, useRelativeLoopToolsPath, processId)
			prompt = ['make']
			subprocess.call(prompt, stdin=None, stdout=None, stderr=None, shell=False, timeout=None)
			print("\nCalculation of the full NLO decay width completed.\n")
		else:
			print("The full NLO decay width will not be calculated.\n")


		#############################
		#  Add the parameter files  #
		#############################
		# Prompt the user to add the parameter files needed for the checks for UV, IR divergence and gauge dependence and for the numerical evaluation
		# WARNING: without the polarization sum, the real corrections and the counterterm, the execution of the next steps in the program will fail
		pathToParams = "Parameters" + os.sep + processId
		CommonFunctions.create_dir_if_not_exist(pathToParams)
		calcWantProceed = CommonFunctions.queryBoolean(">>> Please add now the parameter files to the Parameters folder of the process by hand. Do you want to proceed now?")
		if not calcWantProceed:
			print("Program stopped by user. 2HDMCalc will be terminated now.\n")
			sys.exit()


		#########################
		#  UV divergence check  #
		#########################
		if silentModeCalcUVDivergence:
			calcUVDivergence = defCalcUVDivergence
		else:
			calcUVDivergence = CommonFunctions.queryBoolean("\n>>> Do you want to perform the check for UV divergences?")

		if calcUVDivergence:
			print('\nStarting the check for UV divergences...\n')
			calcDecayUVDivergence(processId)
			print('\nUV divergence check done.\n')


		#########################
		#  IR divergence check  #
		#########################
		print('')
		if silentModeCalcIRDivergence:
			calcIRDivergence = defCalcIRDivergence
		else:
			calcIRDivergence = CommonFunctions.queryBoolean(">>> Do you want to perform the check for IR divergences?")

		if calcIRDivergence:
			print('\nStarting the check for IR divergences...\n')
			calcDecayIRDivergence(processId)
			print('\nIR divergence check done.\n')


		############################
		#  Gauge dependence check  #
		############################
		print('')
		if silentModeCalcGaugeDependence:
			calcGaugeDependence = defCalcGaugeDependence
		else:
			calcGaugeDependence = CommonFunctions.queryBoolean(">>> Do you want to perform the check for gauge dependences?")

		if calcGaugeDependence:
			print('\nStarting the check for gauge dependences...\n')
			calcDecayGaugeDependence(processId)
			print('\nGauge dependence check done.\n')


		##########################
		#  Numerical Evaluation  #
		##########################
		print('')
		if silentModeCalcNumericalEvaluation:
			calcNumericalEvaluation = defCalcNumericalEvaluation
		else:
			calcNumericalEvaluation = CommonFunctions.queryBoolean(">>> Do you want to perform the numerical evaluation?")

		if calcNumericalEvaluation:
			print('\nStarting the numerical evaluation...\n')
			calcDecayWidthFull(processId)
			print('\nNumerical evaluation done.\n')

		sys.exit()
