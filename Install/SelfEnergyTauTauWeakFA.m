(* ::Package:: *)

dir = DirectoryName[$InputFileName]; 
commandLine = StringSplit[$CommandLine[[4]],".m "][[2]];
argv = StringSplit[commandLine, " "];
ImportPaths = FileNameJoin[{dir, "..", "Paths.m"}]; 
Get[ImportPaths]; 
$CKM = True; 
Get[FeynartsDirectory]; 
Get[FeynArtsFeynCalcToolsDirectory];
Switch[argv[[2]],
"1", targetDir = "Usual";topology = CreateTopologies[1, 1 -> 1, ExcludeTopologies -> {Internal}];,
"2", targetDir = "Alternative";topology = CreateTopologies[1, 1 -> 1];,
_, Exit[];]
export = {}; 
AA = InsertFields[topology, F[2,{3}] -> F[2,{3}], Model -> "THDM2", LastSelections->{!V[1]}, InsertionLevel -> Particles];
amp = CreateFeynAmp[AA, GaugeRules -> {}]; 
amplitudes = PickLevel[Particles][amp]; 
amplituderange = Range[1, Length[amplitudes]]; 
FAamp = ""; 
ToExpression[Import[FeynartsparserDirectory]]; 
export = Append[export, StringJoin["SelfTauTauWeak={", FAamp, "}"]]; 
ExportFile = FileNameJoin[{dir, "..", "BuildingBlocks", "SelfEnergies", targetDir, "SelfEnergyTauTauWeakFA.txt"}]; 
Export[ExportFile, export]; 
Exit[]; 
