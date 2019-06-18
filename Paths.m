(* ::Package:: *)

direc = DirectoryName[$InputFileName]; 
FeynartsDirectory = FileNameJoin[{direc, "FeynArts", "FeynArts39.m"}]; 
FeyncalcDirectory = FileNameJoin[{direc, "HighEnergyPhysics", "fc.m"}];
LoopToolsDirectory = FileNameJoin[{direc, "LoopTools", "LoopTools.exe"}];  
FeynartsparserDirectory = FileNameJoin[{direc, "FeynArts", "FeynArtsParser.txt"}]; 
FeynArtsFeynCalcToolsDirectory = FileNameJoin[{direc, "FeynArtsFeynCalcTools.m"}]; 
ParticleContentFile = FileNameJoin[{direc, "ParticleContent.py"}]; 
UVtestParameterDirectory = FileNameJoin[{direc, "Parameters", "UVtestParameters.txt"}];
