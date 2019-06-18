#!/usr/bin/env python
#Filename: Install.py


 ###################################################################################
#																					#
#									Install 										#
#																					#
#	Purpose:	Sets up 2HDMCalc. Calculates the generic 2HDM Lagrangian, all		#
#				Tadpole CTs and the Self-Energies.									#
#																					#
 ###################################################################################


#------------------------------#
#		 Import Modules		   #
#------------------------------#
import subprocess
import os
import CommonFunctions			# Provides common, often used functions for different scripts of 2HDMCalc
from Configuration import *		# Load the configuration file


#----------------------------#
#		 Main Program		 #
#----------------------------#
print("Starting the installation script.\n")

# Ask the user what to do
if silentModeInstall:
	createFolders = defCreateFolders
	calcLagrangian = defCalcLagrangian
	calcSelfEnergies = defCalcSelfEnergies
	makeSelfEnergies = defMakeSelfEnergies
	calcTadpoles = defCalcTadpoles
	makeTadpoles = defMakeTadpoles
	calcProcDep = defCalcProcDep
else:
	createFolders = CommonFunctions.queryBoolean("Do you want to create all necessary folders?")
	calcLagrangian = CommonFunctions.queryBoolean("Do you want to calculate the generic 2HDM Lagrangian?")
	calcSelfEnergies = CommonFunctions.queryBoolean("Do you want to calculate the 2HDM Self-Energies?")
	calcTadpoles = CommonFunctions.queryBoolean("Do you want to calculate the 2HDM Tadpoles?")
	calcProcDep = CommonFunctions.queryBoolean("Do you want to calculate the vertex corrections for the process-dependent definition of alpha and beta?")

#Create all folders
if createFolders:
	CommonFunctions.create_dir_if_not_exist("BuildingBlocks")		# Contains all calculated Self-Energies, Generic Counterterms and the 2HDM Lagrangian
	CommonFunctions.create_dir_if_not_exist("BuildingBlocks" + os.sep + "SelfEnergiesDerivatives")
	CommonFunctions.create_dir_if_not_exist("BuildingBlocks" + os.sep + "SelfEnergies")
	CommonFunctions.create_dir_if_not_exist("BuildingBlocks" + os.sep + "SelfEnergies" + os.sep + "Alternative")
	CommonFunctions.create_dir_if_not_exist("BuildingBlocks" + os.sep + "SelfEnergies" + os.sep + "Usual")
	CommonFunctions.create_dir_if_not_exist("BuildingBlocks" + os.sep + "Tadpoles")
	CommonFunctions.create_dir_if_not_exist("BuildingBlocks" + os.sep + "Lagrangian")
	CommonFunctions.create_dir_if_not_exist("BuildingBlocks" + os.sep + "ProcessDependentScheme")
	CommonFunctions.create_dir_if_not_exist("BuildingBlocks" + os.sep + "Processes")
	CommonFunctions.create_dir_if_not_exist("Temp")
	CommonFunctions.create_dir_if_not_exist("Temp" + os.sep + "procDepAlpha")
	CommonFunctions.create_dir_if_not_exist("Temp" + os.sep + "procDepAlpha2")
	CommonFunctions.create_dir_if_not_exist("Temp" + os.sep + "procDepBeta")

# Calculate the generic 2HDM Lagrangian
if calcLagrangian:
	print("Calculating the generic 2HDM Lagrangian...")
	prompt = pathMathematica + ' -noprompt -run "<<Install' + os.sep + 'GenericLagrangian.m"'
	subprocess.call(prompt)
	print("\nSuccess: generic 2HDM Lagrangian calculated.\n")

# Calculate the 2HDM Self-Energies (command line arguments: [sector: 1=Scalar, 2=Vector, 3=Leptons] [tadpole scheme: 1=Usual, 2=Alternative])
if calcSelfEnergies:
	print("Calculating the 2HDM Self-Energies...")
	# Alternative Self-Energies
	print("Calculating Alternative Self-Energies in RXi gauge ...")
	prompt = pathMathematica + ' -noprompt -run "<<Install' + os.sep + 'SelfEnergiesFA.m 1 2"'
	subprocess.call(prompt)
	prompt = pathMathematica + ' -noprompt -run "<<Install' + os.sep + 'SelfEnergiesFA.m 2 2"'
	subprocess.call(prompt)
	prompt = pathMathematica + ' -noprompt -run "<<Install' + os.sep + 'SelfEnergiesFA.m 3 2"'
	subprocess.call(prompt)
	prompt = pathMathematica + ' -noprompt -run "<<Install' + os.sep + 'SelfEnergiesFC.m 1 2"'
	subprocess.call(prompt)
	prompt = pathMathematica + ' -noprompt -run "<<Install' + os.sep + 'SelfEnergiesFC.m 2 2"'
	subprocess.call(prompt)
	prompt = pathMathematica + ' -noprompt -run "<<Install' + os.sep + 'SelfEnergiesFC.m 3 2"'
	subprocess.call(prompt)
	print("Calculating Standard Self-Energies in RXi gauge ...")
	prompt = pathMathematica + ' -noprompt -run "<<Install' + os.sep + 'SelfEnergiesFA.m 1 1"'
	subprocess.call(prompt)
	prompt = pathMathematica + ' -noprompt -run "<<Install' + os.sep + 'SelfEnergiesFA.m 2 1"'
	subprocess.call(prompt)
	prompt = pathMathematica + ' -noprompt -run "<<Install' + os.sep + 'SelfEnergiesFA.m 3 1"'
	subprocess.call(prompt)
	prompt = pathMathematica + ' -noprompt -run "<<Install' + os.sep + 'SelfEnergiesFC.m 1 1"'
	subprocess.call(prompt)
	prompt = pathMathematica + ' -noprompt -run "<<Install' + os.sep + 'SelfEnergiesFC.m 2 1"'
	subprocess.call(prompt)
	prompt = pathMathematica + ' -noprompt -run "<<Install' + os.sep + 'SelfEnergiesFC.m 3 1"'
	subprocess.call(prompt)
	print("Calculating light fermion contributions to the photon self-energy ...")
	prompt = pathMathematica + ' -noprompt -run "<<Install' + os.sep + 'SelfEnergyAALightFA.m"'
	subprocess.call(prompt)
	prompt = pathMathematica + ' -noprompt -run "<<Install' + os.sep + 'SelfEnergyAALightFC.m"'
	subprocess.call(prompt)
	print("Calculating tau tau self-energy contributions with separated QED and weak-only parts ...")
	prompt = pathMathematica + ' -noprompt -run "<<Install' + os.sep + 'SelfEnergyTauTauQEDFA.m 3 1"'
	subprocess.call(prompt)
	prompt = pathMathematica + ' -noprompt -run "<<Install' + os.sep + 'SelfEnergyTauTauQEDFA.m 3 2"'
	subprocess.call(prompt)
	prompt = pathMathematica + ' -noprompt -run "<<Install' + os.sep + 'SelfEnergyTauTauWeakFA.m 3 1"'
	subprocess.call(prompt)
	prompt = pathMathematica + ' -noprompt -run "<<Install' + os.sep + 'SelfEnergyTauTauWeakFA.m 3 2"'
	subprocess.call(prompt)
	prompt = pathMathematica + ' -noprompt -run "<<Install' + os.sep + 'SelfEnergyTauTauQEDFC.m 3 1"'
	subprocess.call(prompt)
	prompt = pathMathematica + ' -noprompt -run "<<Install' + os.sep + 'SelfEnergyTauTauQEDFC.m 3 2"'
	subprocess.call(prompt)
	prompt = pathMathematica + ' -noprompt -run "<<Install' + os.sep + 'SelfEnergyTauTauWeakFC.m 3 1"'
	subprocess.call(prompt)
	prompt = pathMathematica + ' -noprompt -run "<<Install' + os.sep + 'SelfEnergyTauTauWeakFC.m 3 2"'
	subprocess.call(prompt)
	print("Calculating Additional Self-Energy contributions for the pinched schemes ...")
	prompt = pathMathematica + ' -noprompt -run "<<Install' + os.sep + 'SelfEnergiesAddFC.m"'
	subprocess.call(prompt)
	print("\nSuccess: all 2HDM Self-Energies calculated.\n")

# Calculate the 2HDM Tadpole Counterterms (command line arguments: [tadpole scheme: 1=Usual, 2=Alternative])
if calcTadpoles:
	print("Calculating the 2HDM Tadpoles...")
	prompt = pathMathematica + ' -noprompt -run "<<Install' + os.sep + 'TadpolesFA.m"'
	subprocess.call(prompt)
	prompt = pathMathematica + ' -noprompt -run "<<Install' + os.sep + 'TadpolesFC.m"'
	subprocess.call(prompt)
	print("\nSuccess: all 2HDM Tadpoles calculated.\n")

# Calculate the vertex corrections used for the process-dependent scheme of alpha and beta
if calcProcDep:
	print("Calculating the vertex corrections for the process-dependent scheme of alpha and beta...")
	prompt = pathMathematica + ' -noprompt -run "<<Install' + os.sep + 'ProcDepBetaFA.m"'
	subprocess.call(prompt)
	prompt = pathMathematica + ' -noprompt -run "<<Install' + os.sep + 'ProcDepBetaFC.m"'
	subprocess.call(prompt)
	prompt = pathMathematica + ' -noprompt -run "<<Install' + os.sep + 'ProcDepAlphaFA.m"'
	subprocess.call(prompt)
	prompt = pathMathematica + ' -noprompt -run "<<Install' + os.sep + 'ProcDepAlphaFC.m"'
	subprocess.call(prompt)
	prompt = pathMathematica + ' -noprompt -run "<<Install' + os.sep + 'ProcDepAlpha2FA.m"'
	subprocess.call(prompt)
	prompt = pathMathematica + ' -noprompt -run "<<Install' + os.sep + 'ProcDepAlpha2FC.m"'
	subprocess.call(prompt)
	print("\nSuccess: all vertex corrections for the process-dependent scheme of alpha and beta calculated.\n")

print("\nInstallation finished.\n")
