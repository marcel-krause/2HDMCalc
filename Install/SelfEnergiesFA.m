(* ::Package:: *)

dir = DirectoryName[$InputFileName]; 
commandLine = StringSplit[$CommandLine[[4]],".m "][[2]];
argv = StringSplit[commandLine, " "];
ImportPaths = FileNameJoin[{dir, "..", "Paths.m"}]; 
Get[ImportPaths]; 
$CKM = True; 
Get[FeynartsDirectory]; 
Get[FeynArtsFeynCalcToolsDirectory];
Switch[argv[[1]],
"1", selfies := scalarSelfEnergiesFAamp; particlenumbers := scalarSelfEnergies; particleTypes := scalarSelfTypes; targetfile = "SelfEnergiesScalarFA.txt";,
"2", selfies := vectorSelfEnergiesFAamp; particlenumbers := vectorSelfEnergies; particleTypes := vectorSelfTypes; targetfile = "SelfEnergiesVectorFA.txt";,
"3", selfies := fermionSelfEnergiesFAamp; particlenumbers := fermionSelfEnergies; particleTypes := fermionSelfTypes; targetfile = "SelfEnergiesFermionFA.txt";,
_,Exit[];]
Switch[argv[[2]],
"1", targetDir = "Usual";topology = CreateTopologies[1, 1 -> 1, ExcludeTopologies -> {Internal}];,
"2", targetDir = "Alternative";topology = CreateTopologies[1, 1 -> 1];,
_, Exit[];]
getParticleContent[ParticleContentFile,FileNameJoin[{dir, "..", "BuildingBlocks", "SelfEnergies", targetDir}]];
export = {}; 
For[counter=1, counter <= Length[selfies], counter++,
Switch[argv[[1]],
"1", If[particlenumbers[[counter]][[1]] == 5 || particlenumbers[[counter]][[1]] == 6, 
   AA = InsertFields[topology, -Evaluate[ToExpression[particleTypes[[counter]][[1]]]][particlenumbers[[counter]][[1]]] -> 
      -Evaluate[ToExpression[particleTypes[[counter]][[1]]]][particlenumbers[[counter]][[2]]], Model -> "THDM2", InsertionLevel -> Particles], 
   AA = InsertFields[topology, Evaluate[ToExpression[particleTypes[[counter]][[1]]]][particlenumbers[[counter]][[1]]] -> 
      Evaluate[ToExpression[particleTypes[[counter]][[1]]]][particlenumbers[[counter]][[2]]], Model -> "THDM2", InsertionLevel -> Particles]];,
"2", AA = InsertFields[topology, Evaluate[ToExpression[particleTypes[[counter]][[1]]]][particlenumbers[[counter]][[1]]] -> 
     Evaluate[ToExpression[particleTypes[[counter]][[1]]]][particlenumbers[[counter]][[2]]], Model -> "THDM2", InsertionLevel -> Particles];,
"3", AA = InsertFields[topology, Evaluate[ToExpression[particleTypes[[counter]][[1]]]][particlenumbers[[counter]][[1]][[1]],{particlenumbers[[counter]][[1]][[2]]}] -> 
     Evaluate[ToExpression[particleTypes[[counter]][[1]]]][particlenumbers[[counter]][[2]][[1]],{particlenumbers[[counter]][[2]][[2]]}], Model -> "THDM2", InsertionLevel -> Particles];,
_, Exit[];];
amp = CreateFeynAmp[AA, GaugeRules -> {}]; 
amplitudes = PickLevel[Particles][amp]; 
amplituderange = Range[1, Length[amplitudes]]; 
FAamp = ""; 
ToExpression[Import[FeynartsparserDirectory]]; 
export = Append[export, StringJoin[selfies[[counter]], "={", FAamp, "}"]]; 
Clear[AA, amp, amplitudes, amplituderange, FAamp]; 
]
ExportFile = FileNameJoin[{dir, "..", "BuildingBlocks", "SelfEnergies", targetDir, targetfile}]; 
Export[ExportFile, export]; 
Exit[]; 
