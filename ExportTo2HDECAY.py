#!/usr/bin/env python
#Filename: ExportTo2HDECAY.py


 ###################################################################################
#																					#
#							ExportTo2HDECAY.py										#
#																					#
#	Purpose:	Mirrors all relevant files of 2HDMCalc to the 2HDECAY folder. 		#
#				WARNING: this UPDATES all files in the 2HDECAY folder. Changes 		#
#				to the relevant files should only be made in the 2HDMCalc folder!	#
#	Application: This file should be saved in the 2HDMCalc folder. The 2HDECAY		#
#				folder HAS TO BE a parallel folder of the 2HDMCalc folder, i.e. 	#
#				both the 2HDMCalc and 2HDECAY folders should be in the same 		#
#				directory.															#
#	Author: 	Marcel Krause (marcel.krause@kit.edu)								#
#																					#
 ###################################################################################


#------------------------------#
#		 Import Modules		   #
#------------------------------#
import sys
import os
from shutil import copyfile, rmtree


#-------------------------#
#		 Functions		  #
#-------------------------#

# def testFunc(testArg):
# 	'''
# 		Descr
# 	'''
	

#----------------------------#
#		 Main Program		 #
#----------------------------#

if __name__ == "__main__":
	# Exclude list for file extensions
	excludeExtensions = ['.o', '.exe', '.a', '.mod', '.stackdump', '.gch']

	# List of relevant folders
	rootFolders = ['BuildingBlocks', 'BuildingBlocks' + os.sep + 'SelfEnergies', 'BuildingBlocks' + os.sep + 'Processes', 'Parameters']
	relevantFolders = ['BuildingBlocks' + os.sep + 'ProcessDependentScheme', 'BuildingBlocks' + os.sep + 'SelfEnergies' + os.sep + 'Usual', 'BuildingBlocks' + os.sep + 'SelfEnergies' + os.sep + 'Alternative', 'BuildingBlocks' + os.sep + 'SelfEnergiesDerivatives', 'BuildingBlocks' + os.sep + 'Tadpoles']
	processFolder = 'BuildingBlocks' + os.sep + 'Processes'
	processFiles = ['Counterterm.F90', 'NLOTadWidthRed.F90', 'NLOWidthRed.F90', 'RealCorrections.F90', 'TreeLevelWidthRed.F90', 'processDescription.txt']
	additionalFiles = ['counterterms.F90']
	rootDir2HDECAY = '..' + os.sep + '2HDECAY'

	# Converter for HDECAY conventions
	convertList = [["h0 -> B,BBar", "h -> bb", True],["h0 -> Tau,TauBar", "h -> tau tau", True],["h0 -> Mu,MuBar", "h -> mu mu", True],["h0 -> S,SBar", "h -> ss", True],["h0 -> C,CBar", "h -> cc", True],["h0 -> T,TBar", "h -> tt", True],["h0 -> Wm,Wp", "h -> WW", True],["h0 -> Z0,Z0", "h -> ZZ", True],["h0 -> HH,HH", "h -> HH", False],["h0 -> A0,A0", "h -> AA", True],["h0 -> A0,Z0", "h -> ZA", True],["h0 -> Hp,Wm", "h -> H+ W-", True],["h0 -> Hm,Hp", "h -> H+ H-", True],["HH -> B,BBar", "H -> bb", True],["HH -> Tau,TauBar", "H -> tautau", True],["HH -> Mu,MuBar", "H -> mu mu", True],["HH -> S,SBar", "H -> ss", True],["HH -> C,CBar", "H -> cc", True],["HH -> T,TBar", "H -> tt", True],["HH -> Wm,Wp", "H -> WW", True],["HH -> Z0,Z0", "H -> ZZ", True],["HH -> h0,h0", "H -> hh", True],["HH -> A0,A0", "H -> AA", True],["HH -> A0,Z0", "H -> ZA", True],["HH -> Hp,Wm", "H -> H+ W-", True],["HH -> Hm,Hp", "H -> H+ H-", True],["A0 -> B,BBar", "A -> bb", True],["A0 -> Tau,TauBar", "A -> tau,tau", True],["A0 -> Mu,MuBar", "A -> mu,mu", True],["A0 -> S,SBar", "A -> ss", True],["A0 -> C,CBar", "A -> cc", True],["A0 -> T,TBar", "A -> tt", True],["A0 -> Z0,h0", "A -> Zh", True],["A0 -> HH,Z0", "A -> ZH", True],["A0 -> Hm,Wp", "A -> H- W+", True],["Hp -> BBar,C", "H+ -> bbar c", True],["Hp -> NeuT,TauBar", "H+ -> taubar nu", True],["Hp -> MuBar,NeuM", "H+ -> mubar nu", True],["Hp -> SBar,U", "H+ -> sbar u", True],["Hp -> C,SBar", "H+ -> sbar c", True],["Hp -> BBar,T", "H+ -> bbar t", True],["Hp -> C,DBar", "H+ -> dbar c", True],["Hp -> BBar,U", "H+ -> bbar u", True],["Hp -> SBar,T", "H+ -> sbar t", True],["Hp -> DBar,T", "H+ -> dbar t", True],["Hp -> Wp,h0", "H+ -> W h", True],["Hp -> HH,Wp", "H+ -> W H", True],["Hp -> A0,Wp", "H+ -> W A", True],["A0 -> D,DBar", "A0 -> D,DBar", False],["A0 -> El,ElBar", "A0 -> El,ElBar", False],["A0 -> U,UBar", "A0 -> U,UBar", False],["h0 -> D,DBar", "h0 -> D,DBar", False],["h0 -> El,ElBar", "h0 -> El,ElBar", False],["h0 -> U,UBar", "h0 -> U,UBar", False],["HH -> D,DBar", "HH -> D,DBar", False],["HH -> El,ElBar", "HH -> El,ElBar", False],["HH -> U,UBar", "HH -> U,UBar", False],["Hp -> DBar,U", "Hp -> DBar,U", False],["Hp -> ElBar,NeuE", "Hp -> ElBar,NeuE", False]]

	# Start message 
	print('Start of the copying process.\n')

	# Check if the 2HDECAY root dir exists and is in the right location
	print('Checking for 2HDECAY folder...')
	if os.path.isdir(rootDir2HDECAY):
		print('  2HDECAY folder found.\n')
	else:
		print('  Critical error: 2HDECAY folder not found!\n')
		sys.exit()

	# Check if the necessary subfolders exists and if not, create them 
	print('Checking for main subfolders...')
	changedFolder = False
	for folderPath in rootFolders:
		tempPath = rootDir2HDECAY + os.sep + folderPath
		if not os.path.isdir(tempPath):
			os.makedirs(tempPath)
			print('  Folder "{}" created.'.format(tempPath))
			changedFolder = True
		# else:
		# 	print('  Folder "{}" already exists.'.format(tempPath))
	for folderPath in relevantFolders:
		tempPath = rootDir2HDECAY + os.sep + folderPath
		if not os.path.isdir(tempPath):
			os.makedirs(tempPath)
			print('  Folder "{}" created.'.format(tempPath))
			changedFolder = True
		# else:
		# 	print('  Folder "{}" already exists.'.format(tempPath))
	for folderPath in os.listdir(processFolder):
		tempPath = rootDir2HDECAY + os.sep + processFolder + os.sep + folderPath
		if not os.path.isdir(tempPath):
			os.makedirs(tempPath)
			print('  Folder "{}" created.'.format(tempPath))
			changedFolder = True
		# else:
		# 	print('  Folder "{}" already exists.'.format(tempPath))
	if not changedFolder:
		print('  All necessary folders already exist.')

	# Iterate through the relevant folders and update all files that have changed
	print('\nCopying relevant files...')
	for folderPath in relevantFolders:
		dirList = os.listdir(folderPath)
		targetList = []

		# Remove all files which have the file extensions as given in the extension list
		for fileCandidate in dirList:
			for excludeCandidate in excludeExtensions:
				if excludeCandidate in fileCandidate:
					dirList.remove(fileCandidate)
		dirListFinal = [folderPath + os.sep + s for s in dirList]

		# Iterate through the final file list; if the file does not exist, create it. Else, check if the two files differ (compare with latest modification timestamp) and if so, copy the file
		fileCreated = False
		fileUpdated = False
		fileCreatedCounter = 0
		fileUpdatedCounter = 0
		print('  Copying files from folder "{}".'.format(folderPath))
		for fileIn2HDMCalc in dirListFinal:
			fileIn2HDECAY = rootDir2HDECAY + os.sep + fileIn2HDMCalc
			# Check if the file does not exist yet 
			if not os.path.isfile(fileIn2HDECAY):
				copyfile(fileIn2HDMCalc, fileIn2HDECAY)
				print('    File "{}" was created.'.format(fileIn2HDECAY))
				fileCreated = True
				fileCreatedCounter += 1
			else:
				# Compare the timestamps of the two files to check if the file in the 2HDMCalc folder has changed w.r.t. the file in the 2HDECAY folder
				timestamp2HDMCalc = os.path.getmtime(fileIn2HDMCalc)
				timestamp2HDECAY = os.path.getmtime(fileIn2HDECAY)
				if timestamp2HDMCalc > timestamp2HDECAY:
					copyfile(fileIn2HDMCalc, fileIn2HDECAY)
					print('    File "{}" was updated.'.format(fileIn2HDECAY))
					fileUpdated = True
					fileUpdatedCounter += 1
				# else:
				# 	print('    File "{}" is already up-to-date.'.format(fileIn2HDMCalc))
		if fileCreated:
			print('    {} files were created.'.format(fileCreatedCounter))
		if fileUpdated:
			print('    {} files were updated.'.format(fileUpdatedCounter))
		if (not fileCreated) and (not fileUpdated):
			print('    All files are already up-to-date. No files were copied.')
		
	# Copy the additional files
	print('  Copying additional files.')
	fileCreated = False
	fileUpdated = False
	fileCreatedCounter = 0
	fileUpdatedCounter = 0
	for fileIn2HDMCalc in additionalFiles:
		fileIn2HDECAY = rootDir2HDECAY + os.sep + fileIn2HDMCalc
		# Check if the file does not exist yet 
		if not os.path.isfile(fileIn2HDECAY):
			copyfile(fileIn2HDMCalc, fileIn2HDECAY)
			print('    File "{}" was created.'.format(fileIn2HDECAY))
			fileCreated = True
			fileCreatedCounter += 1
		else:
			# Compare the timestamps of the two files to check if the file in the 2HDMCalc folder has changed w.r.t. the file in the 2HDECAY folder
			timestamp2HDMCalc = os.path.getmtime(fileIn2HDMCalc)
			timestamp2HDECAY = os.path.getmtime(fileIn2HDECAY)
			if timestamp2HDMCalc > timestamp2HDECAY:
				copyfile(fileIn2HDMCalc, fileIn2HDECAY)
				print('    File "{}" was updated.'.format(fileIn2HDECAY))
				fileUpdated = True
				fileUpdatedCounter += 1
	if fileCreated:
		print('    {} files were created.'.format(fileCreatedCounter))
	if fileUpdated:
		print('    {} files were updated.'.format(fileUpdatedCounter))
	if (not fileCreated) and (not fileUpdated):
		print('    All files are already up-to-date. No files were copied.')

	# Copy all process files 
	processList = []
	for item in convertList:
		processList.append(item[0])
	fileCreated = False
	fileUpdated = False
	fileCreatedCounter = 0
	fileUpdatedCounter = 0
	print('  Copying process files.')
	for folderPath in os.listdir(processFolder):
		processPath2HDMCalc = processFolder + os.sep + folderPath
		processPath2HDECAY = rootDir2HDECAY + os.sep + processPath2HDMCalc
		for processFile in processFiles:
			processFile2HDMCalc = processPath2HDMCalc + os.sep + processFile
			processFile2HDECAY = processPath2HDECAY + os.sep + processFile

			# Check if the file does not exist yet 
			if not os.path.isfile(processFile2HDECAY):
				if processFile == 'processDescription.txt':
					fileHandler = open(processFile2HDMCalc, "r")
					convertedFile = ''
					lineCount = 1
					for line in fileHandler:
						if lineCount == 1:
							finalParticles = (line.split())[2].split(',')
							if finalParticles[0] == finalParticles[1]:
								symmetryFactor = '2'
							else:
								symmetryFactor = '1'
							processPosition = processList.index((line.split())[0] + ' ' + (line.split())[1] + ' ' + (line.split())[2])
							convertedFile += convertList[processPosition][1] + '\n'
						else:
							convertedFile += line
						lineCount += 1
					if convertList[processPosition][2]:
						convertedFile += '\n1\n' + str(processPosition + 1) + '\n' + symmetryFactor
					else:
						convertedFile += '\n0\n' + '0' + '\n' + symmetryFactor
					fileHandler.close()
					fileHandler = open(processFile2HDECAY, "w")
					fileHandler.write(convertedFile)
					fileHandler.close()
				else:
					copyfile(processFile2HDMCalc, processFile2HDECAY)
				print('    File "{}" was created.'.format(processFile2HDECAY))
				fileCreated = True 
				fileCreatedCounter += 1
			else:
				# Compare the timestamps of the two files to check if the file in the 2HDMCalc folder has changed w.r.t. the file in the 2HDECAY folder
				timestamp2HDMCalc = os.path.getmtime(processFile2HDMCalc)
				timestamp2HDECAY = os.path.getmtime(processFile2HDECAY)
				if timestamp2HDMCalc > timestamp2HDECAY:
					if processFile == 'processDescription.txt':
						fileHandler = open(processFile2HDMCalc, "r")
						convertedFile = ''
						lineCount = 1
						for line in fileHandler:
							if lineCount == 1:
								finalParticles = (line.split())[2].split(',')
								if finalParticles[0] == finalParticles[1]:
									symmetryFactor = '2'
								else:
									symmetryFactor = '1'
								processPosition = processList.index((line.split())[0] + ' ' + (line.split())[1] + ' ' + (line.split())[2])
								convertedFile += convertList[processPosition][1] + '\n'
							else:
								convertedFile += line
							lineCount += 1
						if convertList[processPosition][2]:
							convertedFile += '\n1\n' + str(processPosition + 1) + '\n' + symmetryFactor
						else:
							convertedFile += '\n0\n' + '0' + '\n' + symmetryFactor
						fileHandler.close()
						fileHandler = open(processFile2HDECAY, "w")
						fileHandler.write(convertedFile)
						fileHandler.close()
					else:
						copyfile(processFile2HDMCalc, processFile2HDECAY)
					print('    File "{}" was updated.'.format(processFile2HDECAY))
					fileUpdated = True 
					fileUpdatedCounter += 1
				# else:
				# 	print('    File "{}" is already up-to-date.'.format(processFile2HDMCalc))	
	if fileCreated:
		print('    {} files were created.'.format(fileCreatedCounter))
	if fileUpdated:
		print('    {} files were updated.'.format(fileUpdatedCounter))
	if (not fileCreated) and (not fileUpdated):
		print('    All files are already up-to-date. No files were copied.')


	# End message 
	print('\nCopying process completed successfully.\n')