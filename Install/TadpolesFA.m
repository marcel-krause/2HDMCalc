(* ::Package:: *)

dir = DirectoryName[$InputFileName]; 
ImportPaths = FileNameJoin[{dir, "..", "Paths.m"}]; 
Get[ImportPaths]; 
$CKM = True; 
Get[FeynartsDirectory]; 
Get[FeynArtsFeynCalcToolsDirectory];
selfies := scalarTadpolesFAamp; 
particlenumbers := scalarTadpoles; 
particleTypes := scalarTadpoleTypes; 
targetfile = "ScalarTadpolesFA.txt";
topology = CreateTopologies[1, 1 -> 0,ExcludeTopologies->{Internal}];
getParticleContent[ParticleContentFile,FileNameJoin[{dir, "..", "BuildingBlocks", "Tadpoles"}]];
export = {}; 
For[counter=1, counter <= Length[selfies], counter++,
AA = InsertFields[topology, Evaluate[ToExpression[particleTypes[[counter]][[1]]]][particlenumbers[[counter]][[1]]] -> 
     {}, Model -> "THDM2", InsertionLevel -> Particles];
amp = CreateFeynAmp[AA, GaugeRules -> {}]; 
amplitudes = PickLevel[Particles][amp]; 
amplituderange = Range[1, Length[amplitudes]]; 
FAamp = ""; 
ToExpression[Import[FeynartsparserDirectory]]; 
export = Append[export, StringJoin[selfies[[counter]], "={", FAamp, "}"]]; 
Clear[AA, amp, amplitudes, amplituderange, FAamp]; 
]
ExportFile = FileNameJoin[{dir, "..", "BuildingBlocks", "Tadpoles", targetfile}]; 
Export[ExportFile, export]; 
Exit[]; 
