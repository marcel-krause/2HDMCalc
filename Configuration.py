#!/usr/bin/env python
#Filename: Configuration.py


 ###################################################################################
#																					#
#								Configuration 										#
#																					#
#	Purpose:	Main configuration file of 2HDMCalc. Contains all configuration		#
#				settings needed to change the program.								#
#																					#
 ###################################################################################


#---------------------#
#		Shared		  #
#---------------------#
pathMathematica = 'math'	# Specify the path to Mathematica's math executable (note: use math, not wolfram or MathKernel!). If you added Mathematica to the $PATH variable of your OS, you can simply use the default value 'math'.
useRelativeLoopToolsPath = True      # Set True if you want to set the path to LoopTools relative to the 2HDMCalc installation path (useful if you installed LoopTools e.g. in a subdirectory of the 2HDMCalc folder) or False if you want to use an absolute path to LoopTools
pathLoopTools = 'LoopTools-2.14/i686-CYGWIN_NT-10.0-WOW'     # Specify the path to the LoopTools root folder (IMPORTANT: the path must never *end* with '/' and if useRelativePath is True, it must not *start* with '/' either! If useRelativePath is False, it depends on your OS if the full absolute path starts with '/' or not: on Windows, it typically does not, on Linux, it typically does)
pathLoopToolsLibs = 'lib'                                   # Specify the LoopTools subfolder where the LoopTools libraries are contained (NOTE: this depends on your OS and chip architecture; on Windows, this is mostly 'lib', on Linux, it is mostly 'lib64')
useMaximumLicenses = True              # Set True if you want to set the maximum number of parallel amplitude computations to the number of available parallel Kernels by your Mathematica license (NOTE: please close ALL Mathematica sessions during the usage of 2HDMCalc)
maximumMathematicaLicenses = 6          # Specifies the maximum parallel Mathematica sessions that are allowed by the user's license; is used only if useMaximumLicenses is set to True

#-----------------------------#
#		 2HDMCalc.py		  #
#-----------------------------#
silentMode2HDMCalc = True		# Set True if you directly want to use the default settings for 2HDMCalc.py. In this case, you will not get any prompts during the run of the program.
loggingActive = False			# Set True if you want to activate logging of all calculation steps (useful for debugging only).

# Following now are the default values that will be used if silentMode2HDMCalc is set to True.
defExecInstall = False			# Set True if you want to run the installation script, recalculating all self-energies, tadpole CTs and the generic 2HDM Lagrangian.
defAskProcess = True			# Set True if you want to ask for the process that shall be calculated. If this is set to False, the program will teminate after the installation script since there is nothing to do.

#-------------------------#
#		 Install.py		  #
#-------------------------#
silentModeInstall = True			# Set True if you directly want to use the default settings below. Default: True.

# Following now are the default values that will be used if silentModeInstall is set to True.
defCreateFolders = False            # Set True if you want to create all necessary folders for the usage of 2HDMCalc
defCalcLagrangian = False			# Set True if you want to calculate the generic 2HDM Lagrangian. Default: False.
defCalcSelfEnergies = False			# Set True if you want to calculate the 2HDM Self-Energies. Default: False.
defMakeSelfEnergies = False			# Set True if you want to execute the makefile of the 2HDM Self-Energies. Default: True.
defCalcTadpoleCTs = False			# Set True if you want to calculate the 2HDM Tadpoles. Default: False.
defMakeTadpoles = False			# Set True if you want to execute the makefile of the 2HDM Tadpoles. Default: True.
defCalcProcDep = False			# Set True if you want to calculate the vertex corrections for the process-dependent definition of alpha and beta. Default: False.

#---------------------------------#
#		 CalcTreeLevel.py		  #
#---------------------------------#
silentModeCalcTreeLevel = False			# Set True if you directly want to use the default settings below. Default: True.
silentModeCalcTreeLevelDecayWidth = False			# Set True if you directly want to use the default settings below. Default: True.

# Following now are the default values that will be used if silentModeInstall is set to True.
defCalcTreeLevelAnew = False			# Set True if you want to calculate the tree-level amplitude in general. Default: True.
defCalcTreeLevelIfNotExists = True		# Set True if you want to calculate the tree-level amplitude even if it already exists. Default: False.
defCalcTreeLevelDecayWidthAnew = False			# Set True if you want to calculate the tree-level decay width in general. Default: True.
defCalcTreeLevelDecayWidthIfNotExists = True		# Set True if you want to calculate the tree-level decay width even if it already exists. Default: False.

#-----------------------------#
#		 CalcOneLoop.py		  #
#-----------------------------#
silentModeCalcOneLoop = False			# Set True if you directly want to use the default settings below. Default: True.
silentModeCalcNLODecayWidth = False			# Set True if you directly want to use the default settings below. Default: True.

# Following now are the default values that will be used if silentModeInstall is set to True.
defCalcOneLoopAnew = False				# Set True if you want to calculate the one-loop corrections in general. Default: True.
defCalcOneLoopIfNotExists = True		# Set True if you want to calculate the one-loop corrections even if they already exist. Default: False.
defCalcNLODecayWidthAnew = False			# Set True if you want to calculate the NLO decay width in general. Default: True.
defCalcNLODecayWidthIfNotExists = True		# Set True if you want to calculate the NLO decay width even if it already exists. Default: False.

#-------------------------#
#		 Proceed		  #
#-------------------------#
silentModeCalcProceed = False			# Set True if you directly want to use the default settings below. Default: True.
defCalcProceed = True       # Set true if you want to skip the prompt for adding real corrections, polarization rules and counterterms

#-----------------------------#
#		 DecayWidth.py		  #
#-----------------------------#
silentModeDecayWidth = True			# Set True if you directly want to use the default settings below. Default: True.
defCalcDecayWidth = True			# Set True if you want to calculate the decay width. Default: True.
defCalcFullDecayWidthAnew = False			# Set True if you want to calculate the tree-level decay width in general. Default: True.
defCalcFullDecayWidthIfNotExists = True		# Set True if you want to calculate the tree-level decay width even if it already exists. Default: False.

#-------------------------------------#
#		 UV divergence check		  #
#-------------------------------------#
silentModeCalcUVDivergence = False          # Set True if you directly want to use the default settings below. Default: True.
defCalcUVDivergence = False                  # Set True if you want to check for UV divergences. Default: True.

#-------------------------------------#
#		 IR divergence check		  #
#-------------------------------------#
silentModeCalcIRDivergence = False          # Set True if you directly want to use the default settings below. Default: True.
defCalcIRDivergence = False                  # Set True if you want to check for IR divergences. Default: True.

#-------------------------------------#
#		 Gauge dependence check		  #
#-------------------------------------#
silentModeCalcGaugeDependence = False          # Set True if you directly want to use the default settings below. Default: True.
defCalcGaugeDependence = False                  # Set True if you want to check for gauge dependences. Default: True.

#-------------------------------------#
#		 Numerical Evaluation		  #
#-------------------------------------#
silentModeCalcNumericalEvaluation = False			# Set True if you directly want to use the default settings below. Default: True.
defCalcNumericalEvaluation = False			# Set True if you want to numerically evaluate the decay width. Default: True.
