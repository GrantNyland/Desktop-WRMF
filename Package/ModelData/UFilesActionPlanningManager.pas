//
//
//  UNIT      : Contains TFilesActionPlanningManager Class
//  AUTHOR    : Dziedzi Ramulondi (PDNA)
//  DATE      : 04/01/2006
//  COPYRIGHT : Copyright © 2006 DWAF
//
//
unit UFilesActionPlanningManager;

interface

uses
  classes,
  Contnrs,
  VCL.Controls,
  VCL.Dialogs,
  UDataFileObjects,
  UAbstractObject,
  UFileNames,
  UFilesActionAbstractManager,
  UFilesActionYieldManager,
  UPlanningFileDataObjects,
  UFileAllocationDefinitionAgent,
  UFileAllocationDefinitionDatabaseAgent,
  UFileReservoirImplementationAgent,
  UFileReservoirImplementationDatabaseAgent,
  UFilePumpingChannelControlAgent,
  UFilePumpingChannelControlDatabaseAgent,
  UFileGeneralChannelControlAgent,
  UFileGeneralChannelControlDatabaseAgent,
  UFileChannelSwitchControlAgent,
  UFileChannelSwitchControlDatabaseAgent,
  UFileGrowthFactorsAgent,
  UFileGrowthFactorslDatabaseAgent,
  UFileDisbenefitAgent,
  UFileDisbenefitDatabaseAgent,
  UFileReturnFlowChannelAgent,
  UFileReturnFlowChannelDatabaseAgent,

  UFileReclamationPlantAgent,
  UFileReclamationPlantDatabaseAgent,
  UFileMonthlyWaterRequirementAgent,
  UFileMonthlyWaterRequirementDatabaseAgent,
  UFileAllocationChannelControlAgent,
  UFileAllocationChannelControlDatabseAgent,
  UFileHydroPowerAllocationControlAgent,
  UFileHydroPowerAllocationControlDatabaseAgent,
  UFileControlledReleaseStructureAgent,
  UFileControlledReleaseStructureDatabaseAgent,
  UFileTariffCalculationAgent,
  UFileTariffCalculationDatabseAgent,
  UAbstractFileNamesObject,
  URunPlanningModelAgent,
  UFileCurtailAgent,
  UFileCurtailDatabaseAgent,
  UFileSaltWashOffAgent,
  UFileSaltWashOffDatabaseAgent,
  UFileMIMMAgent,
  UFileMIMMDatabaseAgent;



type
  TFilesActionPlanningManager = class(TFilesActionYieldManager)
  protected
    FFileAllocationDefinitionAgent: TFileAllocationDefinitionAgent;
    FFileAllocationDefinitionDatabaseAgent: TFileAllocationDefinitionDatabaseAgent;
    FFileReservoirImplementationAgent: TFileReservoirImplementationAgent;
    FFileReservoirImplementationDatabaseAgent: TFileReservoirImplementationDatabaseAgent;
    FFilePumpingChannelControlAgent:TFilePumpingChannelControlAgent;
    FFilePumpingChannelControlDatabaseAgent:TFilePumpingChannelControlDatabaseAgent;
    FFileGeneralChannelControlAgent:TFileGeneralChannelControlAgent;
    FFileGeneralChannelControlDatabaseAgent:TFileGeneralChannelControlDatabaseAgent;
    FFileChannelSwitchControlAgent:TFileChannelSwitchControlAgent;
    FFileChannelSwitchControlDatabaseAgent:TFileChannelSwitchControlDatabaseAgent;
    FFileGrowthFactorsAgent:TFileGrowthFactorsAgent;
    FFileGrowthFactorslDatabaseAgent:TFileGrowthFactorslDatabaseAgent;
    FFileDisbenefitAgent:TFileDisbenefitAgent;
    FFileDisbenefitDatabaseAgent:TFileDisbenefitDatabaseAgent;
    FFileReturnFlowChannelAgent:TFileReturnFlowChannelAgent;
    FFileReturnFlowChannelDatabaseAgent:TFileReturnFlowChannelDatabaseAgent;
    FFileAllocationChannelControlAgent: TFileAllocationChannelControlAgent;
    FFileAllocationChannelControlDatabseAgent : TFileAllocationChannelControlDatabseAgent;
    FFileHydroPowerAllocationControlAgent: TFileHydroPowerAllocationControlAgent;
    FFileHydroPowerAllocationControlDatabaseAgent: TFileHydroPowerAllocationControlDatabaseAgent;
    FFileControlledReleaseStructureAgent: TFileControlledReleaseStructureAgent;
    FFileControlledReleaseStructureDatabaseAgent: TFileControlledReleaseStructureDatabaseAgent;
    FFileTariffCalculationAgent:TFileTariffCalculationAgent;
    FFileTariffCalculationDatabseAgent:TFileTariffCalculationDatabseAgent;
    FFileMonthlyWaterRequirementAgent:TFileMonthlyWaterRequirementAgent;
    FFileMonthlyWaterRequirementDatabaseAgent:TFileMonthlyWaterRequirementDatabaseAgent;
    FFileReclamationPlantAgent:TFileReclamationPlantAgent;
    FFileReclamationPlantDatabaseAgent:TFileReclamationPlantDatabaseAgent;
    FFileCurtailAgent:TFileCurtailAgent;
    FFileCurtailDatabaseAgent:TFileCurtailDatabaseAgent;
    FFileSaltWashOffAgent:TFileSaltWashOffAgent;
    FFileSaltWashOffDatabaseAgent:TFileSaltWashOffDatabaseAgent;

    FFileMIMMAgent : TFileMIMMAgent;
    FFileMIMMDatabaseAgent : TFileMIMMDatabaseAgent;

    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    procedure CreateDataFileObject; override;
    procedure CreateRunYieldModelAgent; override;

    function ExecRunModel(AProgressUpdateFuntion:TProgressUpdateFuntion):boolean; override;
    function CheckModelFilesAreComplete(AProgressUpdateFuntion:TProgressUpdateFuntion): boolean; override;
    function IncludeAllocationDefinitionFile(AFileName: TAbstractModelFileName): boolean;virtual;
    function IncludeChannelSwitchFile(AFileName: TAbstractModelFileName): boolean;virtual;
    function IncludeSaltWashOffFile(AFileName: TAbstractModelFileName): boolean;virtual;
	function IncludeMIMMFile(AFileName: TAbstractModelFileName): boolean;
    function ExecValidateModelData(AProgressUpdateFuntion:TProgressUpdateFuntion):boolean;override;
    function ExecLoadDataFromFiles(AProgressUpdateFuntion:TProgressUpdateFuntion):boolean; override;
    function ExecSaveDataToFiles(AProgressUpdateFuntion:TProgressUpdateFuntion):boolean; override;
    function ExecLoadDataFromDatabase(AProgressUpdateFuntion:TProgressUpdateFuntion):boolean; override;
    function ExecSaveDataToDatabase(AProgressUpdateFuntion:TProgressUpdateFuntion):boolean; override;
    function ExecClearModelData(AProgressUpdateFuntion:TProgressUpdateFuntion):boolean;override;
  public
  end;

implementation

uses
  SysUtils,
  UUtilities,
  UConstants,
  UPlanningModelDataObject,
  UErrorHandlingOperations;


{ TFilesActionPlanningManager }

procedure TFilesActionPlanningManager.CreateMemberObjects;
const OPNAME = 'TFilesActionPlanningManager.CreateMemberObjects';
begin
  inherited  CreateMemberObjects;
  try
    FFileAllocationDefinitionAgent            := TFileAllocationDefinitionAgent.Create(FAppModules);
    FFileAllocationDefinitionDatabaseAgent    := TFileAllocationDefinitionDatabaseAgent.Create(FAppModules);
    FFileReservoirImplementationAgent         := TFileReservoirImplementationAgent.Create(FAppModules);
    FFileReservoirImplementationDatabaseAgent := TFileReservoirImplementationDatabaseAgent.Create(FAppModules);
    FFilePumpingChannelControlAgent           := TFilePumpingChannelControlAgent.Create(FAppModules);
    FFilePumpingChannelControlDatabaseAgent   := TFilePumpingChannelControlDatabaseAgent.Create(FAppModules);
    FFileGeneralChannelControlAgent           := TFileGeneralChannelControlAgent.Create(FAppModules);
    FFileGeneralChannelControlDatabaseAgent   := TFileGeneralChannelControlDatabaseAgent.Create(FAppModules);
    FFileChannelSwitchControlAgent            := TFileChannelSwitchControlAgent.Create(FAppModules);
    FFileChannelSwitchControlDatabaseAgent    := TFileChannelSwitchControlDatabaseAgent.Create(FAppModules);
    FFileGrowthFactorsAgent                   := TFileGrowthFactorsAgent.Create(FAppModules);
    FFileGrowthFactorslDatabaseAgent          := TFileGrowthFactorslDatabaseAgent.Create(FAppModules);
    FFileDisbenefitAgent                      := TFileDisbenefitAgent.Create(FAppModules);
    FFileDisbenefitDatabaseAgent              := TFileDisbenefitDatabaseAgent.Create(FAppModules);
    FFileReturnFlowChannelAgent               := TFileReturnFlowChannelAgent.Create(FAppModules);
    FFileReturnFlowChannelDatabaseAgent       := TFileReturnFlowChannelDatabaseAgent.Create(FAppModules);
    FFileAllocationChannelControlAgent        := TFileAllocationChannelControlAgent.Create(FAppModules);
    FFileAllocationChannelControlDatabseAgent := TFileAllocationChannelControlDatabseAgent.Create(FAppModules);
    FFileHydroPowerAllocationControlAgent     := TFileHydroPowerAllocationControlAgent.Create(FAppModules);
    FFileHydroPowerAllocationControlDatabaseAgent := TFileHydroPowerAllocationControlDatabaseAgent.Create(FAppModules);
    FFileControlledReleaseStructureAgent          := TFileControlledReleaseStructureAgent.Create(FAppModules);
    FFileControlledReleaseStructureDatabaseAgent  := TFileControlledReleaseStructureDatabaseAgent.Create(FAppModules);
    FFileTariffCalculationAgent                   := TFileTariffCalculationAgent.Create(FAppModules);
    FFileTariffCalculationDatabseAgent            := TFileTariffCalculationDatabseAgent.Create(FAppModules);
    FFileMonthlyWaterRequirementAgent             := TFileMonthlyWaterRequirementAgent.Create(FAppModules);
    FFileMonthlyWaterRequirementDatabaseAgent     := TFileMonthlyWaterRequirementDatabaseAgent.Create(FAppModules);
    FFileReclamationPlantAgent                    := TFileReclamationPlantAgent.Create(FAppModules);
    FFileReclamationPlantDatabaseAgent            := TFileReclamationPlantDatabaseAgent.Create(FAppModules);
    FFileCurtailAgent                             := TFileCurtailAgent.Create(FAppModules);
    FFileCurtailDatabaseAgent                     := TFileCurtailDatabaseAgent.Create(FAppModules);
    FFileSaltWashOffAgent                         := TFileSaltWashOffAgent.Create(FAppmodules);
    FFileSaltWashOffDatabaseAgent                 := TFileSaltWashOffDatabaseAgent.Create(FAppmodules);
  
    FFileMIMMAgent                                := TFileMIMMAgent.Create(FAppModules);
    FFileMIMMDatabaseAgent                        := TFileMIMMDatabaseAgent.Create(FAppModules);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFilesActionPlanningManager.DestroyMemberObjects;
const OPNAME = 'TFilesActionPlanningManager.DestroyMemberObjects';
begin
  inherited  DestroyMemberObjects;
  try
    FreeAndNil(FFileAllocationDefinitionAgent);
    FreeAndNil(FFileAllocationDefinitionDatabaseAgent);
    FreeAndNil(FFileReservoirImplementationAgent);
    FreeAndNil(FFileReservoirImplementationDatabaseAgent);
    FreeAndNil(FFilePumpingChannelControlAgent);
    FreeAndNil(FFilePumpingChannelControlDatabaseAgent);
    FreeAndNil(FFileGeneralChannelControlAgent);
    FreeAndNil(FFileGeneralChannelControlDatabaseAgent);
    FreeAndNil(FFileChannelSwitchControlAgent);
    FreeAndNil(FFileChannelSwitchControlDatabaseAgent);
    FreeAndNil(FFileGrowthFactorsAgent);
    FreeAndNil(FFileGrowthFactorslDatabaseAgent);
    FreeAndNil(FFileDisbenefitAgent);
    FreeAndNil(FFileDisbenefitDatabaseAgent);
    FreeAndNil(FFileReturnFlowChannelAgent);
    FreeAndNil(FFileReturnFlowChannelDatabaseAgent);
    FreeAndNil(FFileAllocationChannelControlAgent);
    FreeAndNil(FFileAllocationChannelControlDatabseAgent);
    FreeAndNil(FFileHydroPowerAllocationControlAgent);
    FreeAndNil(FFileHydroPowerAllocationControlDatabaseAgent);
    FreeAndNil(FFileControlledReleaseStructureAgent);
    FreeAndNil(FFileControlledReleaseStructureDatabaseAgent);
    FreeAndNil(FFileTariffCalculationAgent);
    FreeAndNil(FFileTariffCalculationDatabseAgent);
    FreeAndNil(FFileMonthlyWaterRequirementAgent);
    FreeAndNil(FFileMonthlyWaterRequirementDatabaseAgent);
    FreeAndNil(FFileReclamationPlantAgent);
    FreeAndNil(FFileReclamationPlantDatabaseAgent);
    FreeAndNil(FFileCurtailAgent);
    FreeAndNil(FFileCurtailDatabaseAgent);
    FreeAndNil(FFileSaltWashOffAgent);
    FreeAndNil(FFileSaltWashOffDatabaseAgent);

    FreeAndNil(FFileMIMMAgent);
    FreeAndNil(FFileMIMMDatabaseAgent);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFilesActionPlanningManager.CreateDataFileObject;
const OPNAME = 'TFilesActionPlanningManager.CreateDataFileObject';
begin
  try
    FDataFileObjects := TPlanningFileDataObjects.Create;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFilesActionPlanningManager.ExecLoadDataFromFiles(AProgressUpdateFuntion: TProgressUpdateFuntion): boolean;
const OPNAME = 'TFilesActionPlanningManager.ExecLoadDataFromFiles';
var
  LCurrentFileNames: TAbstractFileNamesList;
  LCount: integer;
  LResult: boolean;
begin
  Result := inherited ExecLoadDataFromFiles(AProgressUpdateFuntion);
  try
    if (not Result) and FAppModules.GlobalData.StopOnFirstErr then
      Exit;

    Result := True;
    //AllocationDefinition files(SW*.dat ...)
    LCurrentFileNames := FileNamesObject.ChannelSwitchControlFileNames;
    for LCount := 0 to LCurrentFileNames.Count - 1 do
    begin
      if not IncludeChannelSwitchFile(LCurrentFileNames.FileNameObject[LCount]) then
         Continue;
      if ValidateFileName(LCurrentFileNames.FileNameObject[LCount],AProgressUpdateFuntion) then
      begin
        LResult := FFileChannelSwitchControlAgent.ReadModelDataFromFile(LCurrentFileNames.FileNameObject[LCount],
                   FDataFileObjects,AProgressUpdateFuntion);
        TFileNameObject(LCurrentFileNames.FileNameObject[LCount]).HintsSet := True;
        Result := Result and LResult;
        if not UpdateProgress(LResult) then Exit;
      end;
    end;

    //AllocationDefinition files(FM*.dat ...)
    LCurrentFileNames := FileNamesObject.AllocationDefinitionFileNames;
    for LCount := 0 to LCurrentFileNames.Count - 1 do
    begin
      if not IncludeAllocationDefinitionFile(LCurrentFileNames.FileNameObject[LCount]) then
         Continue;
      if ValidateFileName(LCurrentFileNames.FileNameObject[LCount],AProgressUpdateFuntion) then
      begin
        LResult := FFileAllocationDefinitionAgent.ReadModelDataFromFile(LCurrentFileNames.FileNameObject[LCount],
                   FDataFileObjects,AProgressUpdateFuntion);
        TFileNameObject(LCurrentFileNames.FileNameObject[LCount]).HintsSet := True;
        Result := Result and LResult;
        if not UpdateProgress(LResult) then Exit;
      end;
    end;

    //Reservoir Implementation file(DAM.dat)
    LCurrentFileNames :=   FileNamesObject.ReservoirImplementationFileNames;
    if ValidateFileName(LCurrentFileNames.FileNameObject[00],AProgressUpdateFuntion) then
    begin
      LResult := FFileReservoirImplementationAgent.ReadModelDataFromFile(LCurrentFileNames.FileNameObject[00],
                 FDataFileObjects,AProgressUpdateFuntion);
      TFileNameObject(LCurrentFileNames.FileNameObject[00]).HintsSet := True;
      Result := Result and LResult;
      if not UpdateProgress(LResult) then Exit;
    end;

    //Pumping channel control file(PMP.dat)
    LCurrentFileNames :=   FileNamesObject.PumpingChannelControlFileNames;
    if ValidateFileName(LCurrentFileNames.FileNameObject[00],AProgressUpdateFuntion) then
    begin
      LResult := FFilePumpingChannelControlAgent.ReadModelDataFromFile(LCurrentFileNames.FileNameObject[00],
                 FDataFileObjects,AProgressUpdateFuntion);
      TFileNameObject(LCurrentFileNames.FileNameObject[00]).HintsSet := True;
      Result := Result and LResult;
      if not UpdateProgress(LResult) then Exit;
    end;

    //General channel control file(PUR.dat)
    LCurrentFileNames :=   FileNamesObject.GeneralChannelControlFileNames;
    if ValidateFileName(LCurrentFileNames.FileNameObject[00],AProgressUpdateFuntion) then
    begin
      LResult := FFileGeneralChannelControlAgent.ReadModelDataFromFile(LCurrentFileNames.FileNameObject[00],
                 FDataFileObjects,AProgressUpdateFuntion);
      TFileNameObject(LCurrentFileNames.FileNameObject[00]).HintsSet := True;
      Result := Result and LResult;
      if not UpdateProgress(LResult) then Exit;
    end;

    //Annual Growth factors file(GTH.dat)
    LCurrentFileNames :=   FileNamesObject.GrowthFactorsFileNames;
    if ValidateFileName(LCurrentFileNames.FileNameObject[00],AProgressUpdateFuntion) then
    begin
      LResult := FFileGrowthFactorsAgent.ReadModelDataFromFile(LCurrentFileNames.FileNameObject[00],
                 FDataFileObjects,AProgressUpdateFuntion);
      TFileNameObject(LCurrentFileNames.FileNameObject[00]).HintsSet := True;
      Result := Result and LResult;
      if not UpdateProgress(LResult) then Exit;
    end;

    // Disbenefit (DBF.dat)
    LCurrentFileNames :=   FileNamesObject.DisbenefitDefinitionFileNames;
    if ValidateFileName(LCurrentFileNames.FileNameObject[00],AProgressUpdateFuntion) then
    begin
      LResult := FFileDisbenefitAgent.ReadModelDataFromFile(LCurrentFileNames.FileNameObject[00],
                 FDataFileObjects,AProgressUpdateFuntion);
      TFileNameObject(LCurrentFileNames.FileNameObject[00]).HintsSet := True;
      Result := Result and LResult;
      if not UpdateProgress(LResult) then Exit;
    end;

    //  Return Flow Channel (RET.dat)
    LCurrentFileNames :=   FileNamesObject.ReturnFlowChannelFileNames;
    if ValidateFileName(LCurrentFileNames.FileNameObject[00],AProgressUpdateFuntion) then
    begin
      LResult := FFileReturnFlowChannelAgent.ReadModelDataFromFile(LCurrentFileNames.FileNameObject[00],
                 FDataFileObjects,AProgressUpdateFuntion);
      TFileNameObject(LCurrentFileNames.FileNameObject[00]).HintsSet := True;
      Result := Result and LResult;
      if not UpdateProgress(LResult) then Exit;
    end;

    //Allocation Channel file(ALO.dat)
    LCurrentFileNames := FileNamesObject.AllocationChannelFileNames;
    if ValidateFileName(LCurrentFileNames.FileNameObject[00],AProgressUpdateFuntion) then
    begin
      LResult := FFileAllocationChannelControlAgent.ReadModelDataFromFile(LCurrentFileNames.FileNameObject[00],
                 FDataFileObjects,AProgressUpdateFuntion);
      TFileNameObject(LCurrentFileNames.FileNameObject[00]).HintsSet := True;
      Result := Result and LResult;
      if not UpdateProgress(LResult) then Exit;
    end;

    //Hydro Allocation file(HYD.dat)
    LCurrentFileNames := FileNamesObject.HydropowerAllocationFileNames;
    if ValidateFileName(LCurrentFileNames.FileNameObject[00],AProgressUpdateFuntion) then
    begin
      LResult := FFileHydroPowerAllocationControlAgent.ReadModelDataFromFile(LCurrentFileNames.FileNameObject[00],
                 FDataFileObjects,AProgressUpdateFuntion);
      TFileNameObject(LCurrentFileNames.FileNameObject[00]).HintsSet := True;
      Result := Result and LResult;
      if not UpdateProgress(LResult) then Exit;
    end;

    //Control Release file(REL.dat)
    LCurrentFileNames :=   FileNamesObject.ReleaseStructureFileNames;
    if ValidateFileName(LCurrentFileNames.FileNameObject[00],AProgressUpdateFuntion) then
    begin
      LResult := FFileControlledReleaseStructureAgent.ReadModelDataFromFile(LCurrentFileNames.FileNameObject[00],
                 FDataFileObjects,AProgressUpdateFuntion);
      TFileNameObject(LCurrentFileNames.FileNameObject[00]).HintsSet := True;
      Result := Result and LResult;
      if not UpdateProgress(LResult) then Exit;
    end;

    // Tarrif calculations (Tar.dat)
    LCurrentFileNames :=   FileNamesObject.TariffCalculationFileNames;
    if ValidateFileName(LCurrentFileNames.FileNameObject[00],AProgressUpdateFuntion) then
    begin
      LResult := FFileTariffCalculationAgent.ReadModelDataFromFile(LCurrentFileNames.FileNameObject[00],
                 FDataFileObjects,AProgressUpdateFuntion);
      TFileNameObject(LCurrentFileNames.FileNameObject[00]).HintsSet := True;
      Result := Result and LResult;
      if not UpdateProgress(LResult) then Exit;
    end;

    // Monthly Water Requirement (Hst.dat)
    LCurrentFileNames :=   FileNamesObject.MonthlyWaterRequirementFileNames;
    if ValidateFileName(LCurrentFileNames.FileNameObject[00],AProgressUpdateFuntion) then
    begin
      LResult := FFileMonthlyWaterRequirementAgent.ReadModelDataFromFile(LCurrentFileNames.FileNameObject[00],
                 FDataFileObjects,AProgressUpdateFuntion);
      TFileNameObject(LCurrentFileNames.FileNameObject[00]).HintsSet := True;
      Result := Result and LResult;
      if not UpdateProgress(LResult) then Exit;
    end;

    // ReclamationPlant Control (Rec.dat)
    LCurrentFileNames :=   FileNamesObject.ReclamationPlantControlFileNames;
    if ValidateFileName(LCurrentFileNames.FileNameObject[00],AProgressUpdateFuntion) then
    begin
      LResult := FFileReclamationPlantAgent.ReadModelDataFromFile(LCurrentFileNames.FileNameObject[00],
                 FDataFileObjects,AProgressUpdateFuntion);
      TFileNameObject(LCurrentFileNames.FileNameObject[00]).HintsSet := True;
      Result := Result and LResult;
      if not UpdateProgress(LResult) then Exit;
    end;

     // Restriction definition file (cur.dat)
    LCurrentFileNames :=   FileNamesObject.CurtailFileNames;
    if ValidateFileName(LCurrentFileNames.FileNameObject[00],AProgressUpdateFuntion) then
    begin
      LResult := FFileCurtailAgent.ReadModelDataFromFile(LCurrentFileNames.FileNameObject[00],
                 FDataFileObjects,AProgressUpdateFuntion);
      TFileNameObject(LCurrentFileNames.FileNameObject[00]).HintsSet := True;
      Result := Result and LResult;
      if not UpdateProgress(LResult) then Exit;
    end;

    // Salts Wash Off files (MISW...dat)
    LCurrentFileNames :=   FileNamesObject.SaltsWashOffFileNames;
    for LCount := 0 to LCurrentFileNames.Count - 1 do
    begin
     if not IncludeSaltWashOffFile(LCurrentFileNames.FileNameObject[LCount]) then
         Continue;
      if ValidateFileName(LCurrentFileNames.FileNameObject[LCount],AProgressUpdateFuntion) then
      begin
      LResult := FFileSaltWashOffAgent.ReadModelDataFromFile(LCurrentFileNames.FileNameObject[LCount],
                 FDataFileObjects,AProgressUpdateFuntion);
      TFileNameObject(LCurrentFileNames.FileNameObject[LCount]).HintsSet := True;
      Result := Result and LResult;
      if not UpdateProgress(LResult) then Exit;
      end;
    end;
	
    // Mine files MIMM*.DAT
    LCurrentFileNames := FileNamesObject.MineFileNames;
    for LCount := 0 to LCurrentFileNames.Count - 1 do
    begin
      if not IncludeMIMMFile(LCurrentFileNames.FileNameObject[LCount]) then
         Continue;
      if ValidateFileName(LCurrentFileNames.FileNameObject[LCount],AProgressUpdateFuntion) then
      begin
        LResult := FFileMIMMAgent.ReadModelDataFromFile(LCurrentFileNames.FileNameObject[LCount],
                   FDataFileObjects,AProgressUpdateFuntion);
        TFileNameObject(LCurrentFileNames.FileNameObject[LCount]).HintsSet := True;
        Result := Result and LResult;
        if not UpdateProgress(LResult) then Exit;
      end;
    end;

    TPlanningFileDataObjects(FDataFileObjects).UpdateChannelSwitchStartDates;

    TPlanningFileDataObjects(FDataFileObjects).AddPlanningNodeWitoutInflow;
    TPlanningFileDataObjects(FDataFileObjects).UpdatePlanningNetworkElementsType;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFilesActionPlanningManager.ExecSaveDataToFiles(AProgressUpdateFuntion: TProgressUpdateFuntion): boolean;
const OPNAME = 'TFilesActionPlanningManager.ExecSaveDataToFiles';
var
  LCurrentFileNames : TAbstractFileNamesList;
  LCount: integer;
  LResult: boolean;
  LFileAction: TFileActionType;
begin
  Result := inherited ExecSaveDataToFiles(AProgressUpdateFuntion);
  try
    if (not Result) and FAppModules.GlobalData.StopOnFirstErr then
      Exit;
    Result := True;

    LFileAction := FFileAction;
    if (FFileAction = fatRunModel) then
      FFileAction := fatExportAll;
    try
      //AllocationDefinition files(SW*.dat ...)
      LCurrentFileNames := FileNamesObject.ChannelSwitchControlFileNames;
      for LCount := 0 to LCurrentFileNames.Count - 1 do
      begin
        if not IncludeChannelSwitchFile(LCurrentFileNames.FileNameObject[LCount]) then
           Continue;
        if ValidateFileName(LCurrentFileNames.FileNameObject[LCount],AProgressUpdateFuntion) then
        begin
          LResult := FFileChannelSwitchControlAgent.WriteModelDataToFile(LCurrentFileNames.FileNameObject[LCount],
                     FDataFileObjects,AProgressUpdateFuntion);
          Result := Result and LResult;
          if not UpdateProgress(LResult) then Exit;
        end;
      end;

      //AllocationDefinition files(FM*.dat ...)
      LCurrentFileNames := FileNamesObject.AllocationDefinitionFileNames;
      for LCount := 0 to LCurrentFileNames.Count - 1 do
      begin
        if not IncludeAllocationDefinitionFile(LCurrentFileNames.FileNameObject[LCount]) then
           Continue;
        if ValidateFileName(LCurrentFileNames.FileNameObject[LCount],AProgressUpdateFuntion) then
        begin
          LResult := FFileAllocationDefinitionAgent.WriteModelDataToFile(LCurrentFileNames.FileNameObject[LCount],
                     FDataFileObjects,AProgressUpdateFuntion);
          Result := Result and LResult;
          if not UpdateProgress(LResult) then Exit;
        end;
      end;

      //Reservoir Implementation file(DAM.dat)
      LCurrentFileNames := FileNamesObject.ReservoirImplementationFileNames;
      if ValidateFileName(LCurrentFileNames.FileNameObject[00],AProgressUpdateFuntion) then
      begin
        LResult := FFileReservoirImplementationAgent.WriteModelDataToFile(LCurrentFileNames.FileNameObject[00],
                   FDataFileObjects,AProgressUpdateFuntion);
        Result := Result and LResult;
        if not UpdateProgress(LResult) then Exit;
      end;

    //Pumping channel control file(PMP.dat)
    LCurrentFileNames :=   FileNamesObject.PumpingChannelControlFileNames;
    if ValidateFileName(LCurrentFileNames.FileNameObject[00],AProgressUpdateFuntion) then
    begin
      LResult := FFilePumpingChannelControlAgent.WriteModelDataToFile(LCurrentFileNames.FileNameObject[00],
                 FDataFileObjects,AProgressUpdateFuntion);
      Result := Result and LResult;
      if not UpdateProgress(LResult) then Exit;
    end;

    //General channel control file(PUR.dat)
    LCurrentFileNames :=   FileNamesObject.GeneralChannelControlFileNames;
    if ValidateFileName(LCurrentFileNames.FileNameObject[00],AProgressUpdateFuntion) then
    begin
      LResult := FFileGeneralChannelControlAgent.WriteModelDataToFile(LCurrentFileNames.FileNameObject[00],
                 FDataFileObjects,AProgressUpdateFuntion);
      Result := Result and LResult;
      if not UpdateProgress(LResult) then Exit;
    end;

    //Annual Growth factors file(GTH.dat)
    LCurrentFileNames :=   FileNamesObject.GrowthFactorsFileNames;
    if ValidateFileName(LCurrentFileNames.FileNameObject[00],AProgressUpdateFuntion) then
    begin
      LResult := FFileGrowthFactorsAgent.WriteModelDataToFile(LCurrentFileNames.FileNameObject[00],
                 FDataFileObjects,AProgressUpdateFuntion);
      Result := Result and LResult;
      if not UpdateProgress(LResult) then Exit;
    end;

    // Disbenefit (DBF.dat)
    LCurrentFileNames :=   FileNamesObject.DisbenefitDefinitionFileNames;
    if ValidateFileName(LCurrentFileNames.FileNameObject[00],AProgressUpdateFuntion) then
    begin
      LResult := FFileDisbenefitAgent.WriteModelDataToFile(LCurrentFileNames.FileNameObject[00],
                 FDataFileObjects,AProgressUpdateFuntion);
      Result := Result and LResult;
      if not UpdateProgress(LResult) then Exit;
    end;

    // Return Flow Channel (RET.dat)
    LCurrentFileNames :=   FileNamesObject.ReturnFlowChannelFileNames;
    if ValidateFileName(LCurrentFileNames.FileNameObject[00],AProgressUpdateFuntion) then
    begin
      LResult := FFileReturnFlowChannelAgent.WriteModelDataToFile(LCurrentFileNames.FileNameObject[00],
                 FDataFileObjects,AProgressUpdateFuntion);
      Result := Result and LResult;
      if not UpdateProgress(LResult) then Exit;
    end;

    //Allocation Channel file(ALO.dat)
    LCurrentFileNames :=   FileNamesObject.AllocationChannelFileNames;
    if ValidateFileName(LCurrentFileNames.FileNameObject[00],AProgressUpdateFuntion) then
    begin
      LResult := FFileAllocationChannelControlAgent.WriteModelDataToFile(LCurrentFileNames.FileNameObject[00],
                 FDataFileObjects,AProgressUpdateFuntion);
      Result := Result and LResult;
      if not UpdateProgress(LResult) then Exit;
    end;

    //Control Release file(REL.dat)
    LCurrentFileNames :=   FileNamesObject.ReleaseStructureFileNames;
    if ValidateFileName(LCurrentFileNames.FileNameObject[00],AProgressUpdateFuntion) then
    begin
      LResult := FFileControlledReleaseStructureAgent.WriteModelDataToFile(LCurrentFileNames.FileNameObject[00],
                 FDataFileObjects,AProgressUpdateFuntion);
      Result := Result and LResult;
      if not UpdateProgress(LResult) then Exit;
    end;

    //Hydro Allocation file(HYD.dat)
     LCurrentFileNames := FileNamesObject.HydropowerAllocationFileNames;
     if ValidateFileName(LCurrentFileNames.FileNameObject[00],AProgressUpdateFuntion) then
     begin
       LResult := FFileHydroPowerAllocationControlAgent.WriteModelDataToFile(LCurrentFileNames.FileNameObject[00],
                  FDataFileObjects, AProgressUpdateFuntion);
       Result := Result and LResult;
     end;

     // Tarrif calculations (Tar.dat)
     LCurrentFileNames := FileNamesObject.TariffCalculationFileNames;
     if ValidateFileName(LCurrentFileNames.FileNameObject[00],AProgressUpdateFuntion) then
     begin
       LResult := FFileTariffCalculationAgent.WriteModelDataToFile(LCurrentFileNames.FileNameObject[00],
                  FDataFileObjects, AProgressUpdateFuntion);
       Result := Result and LResult;
     end;

    // Monthly Water Requirement (Hst.dat)
    LCurrentFileNames :=   FileNamesObject.MonthlyWaterRequirementFileNames;
    if ValidateFileName(LCurrentFileNames.FileNameObject[00],AProgressUpdateFuntion) then
    begin
      LResult := FFileMonthlyWaterRequirementAgent.WriteModelDataToFile(LCurrentFileNames.FileNameObject[00],
                 FDataFileObjects,AProgressUpdateFuntion);
      Result := Result and LResult;
      if not UpdateProgress(LResult) then Exit;
    end;

    // ReclamationPlant Control (Rec.dat)
    LCurrentFileNames :=   FileNamesObject.ReclamationPlantControlFileNames;
    if ValidateFileName(LCurrentFileNames.FileNameObject[00],AProgressUpdateFuntion) then
    begin
      LResult := FFileReclamationPlantAgent.WriteModelDataToFile(LCurrentFileNames.FileNameObject[00],
                 FDataFileObjects,AProgressUpdateFuntion);
      Result := Result and LResult;
      if not UpdateProgress(LResult) then Exit;
    end;

    // Restriction definition file (cur.dat)
    LCurrentFileNames :=   FileNamesObject.CurtailFileNames;
    if ValidateFileName(LCurrentFileNames.FileNameObject[00],AProgressUpdateFuntion) then
    begin
      LResult := FFileCurtailAgent.WriteModelDataToFile(LCurrentFileNames.FileNameObject[00],FDataFileObjects,AProgressUpdateFuntion);
      Result := Result and LResult;
      if not UpdateProgress(LResult) then Exit;
    end;

    // Salts Wash Off files (MISW...dat)
    LCurrentFileNames :=   FileNamesObject.SaltsWashoffFileNames;
    For LCount := 0 to LCurrentFileNames.Count - 1 do
    Begin
      if not IncludeSaltWashOffFile(LCurrentFileNames.FileNameObject[LCount]) then
         Continue;
      if ValidateFileName(LCurrentFileNames.FileNameObject[LCount],AProgressUpdateFuntion) then
      begin
      LResult := FFileSaltWashOffAgent.WriteModelDataToFile(LCurrentFileNames.FileNameObject[LCount],FDataFileObjects,AProgressUpdateFuntion);
      Result := Result and LResult;
      if not UpdateProgress(LResult) then Exit;
      end;
    end;
	 
	//Mine files(MIMM*.dat ...)
      LCurrentFileNames := FileNamesObject.MineFileNames;
      for LCount := 0 to LCurrentFileNames.Count - 1 do
      begin
        if not IncludeMIMMFile(LCurrentFileNames.FileNameObject[LCount]) then
           Continue;
        if ValidateFileName(LCurrentFileNames.FileNameObject[LCount],AProgressUpdateFuntion) then
        begin
          LResult := FFileMIMMAgent.WriteModelDataToFile(LCurrentFileNames.FileNameObject[LCount],
                     FDataFileObjects,AProgressUpdateFuntion);
          Result := Result and LResult;
          if not UpdateProgress(LResult) then Exit;
        end;
      end;

    finally
     FFileAction := LFileAction;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


function TFilesActionPlanningManager.ExecLoadDataFromDatabase(AProgressUpdateFuntion: TProgressUpdateFuntion): boolean;
const OPNAME = 'TFilesActionPlanningManager.ExecLoadDataFromDatabase';
var
  LCurrentFileNames: TAbstractFileNamesList;
  LCount: integer;
  LResult: boolean;
  LFileAction : TFileActionType;
begin
  Result := inherited ExecLoadDataFromDatabase(AProgressUpdateFuntion);
  try
    if (not Result) and FAppModules.GlobalData.StopOnFirstErr then
      Exit;
    Result := True;

    LFileAction := FFileAction;
    if (FFileAction = fatRunModel) then
      FFileAction := fatExportAll;
    try
      //AllocationDefinition files(SW*.dat ...)
      LCurrentFileNames := FileNamesObject.ChannelSwitchControlFileNames;
      for LCount := 0 to LCurrentFileNames.Count - 1 do
      begin
        if not IncludeChannelSwitchFile(LCurrentFileNames.FileNameObject[LCount]) then
           Continue;
        if ValidateFileName(LCurrentFileNames.FileNameObject[LCount],AProgressUpdateFuntion) then
        begin
          LResult := FFileChannelSwitchControlDatabaseAgent.ReadModelDataFromDatabase(TFileNameObject(LCurrentFileNames.FileNameObject[LCount]),
                     FDataFileObjects,AProgressUpdateFuntion);
          Result := Result and LResult;
          if not UpdateProgress(LResult) then Exit;
        end;
      end;

      //AllocationDefinition files(FM*.dat ...)
      LCurrentFileNames := FileNamesObject.AllocationDefinitionFileNames;
      for LCount := 0 to LCurrentFileNames.Count - 1 do
      begin
        if not IncludeAllocationDefinitionFile(LCurrentFileNames.FileNameObject[LCount]) then
           Continue;
        if ValidateFileName(LCurrentFileNames.FileNameObject[LCount],AProgressUpdateFuntion) then
        begin
          LResult := FFileAllocationDefinitionDatabaseAgent.ReadModelDataFromDatabase(TFileNameObject(LCurrentFileNames.FileNameObject[LCount]),
                     FDataFileObjects,AProgressUpdateFuntion);
          Result := Result and LResult;
          if not UpdateProgress(LResult) then Exit;
        end;
      end;

      //Reservoir Implementation file(DAM.dat)
      LCurrentFileNames := FileNamesObject.ReservoirImplementationFileNames;
      if ValidateFileName(LCurrentFileNames.FileNameObject[00],AProgressUpdateFuntion) then
      begin
        LResult := FFileReservoirImplementationDatabaseAgent.ReadModelDataFromDatabase(TFileNameObject(LCurrentFileNames.FileNameObject[00]),FDataFileObjects,AProgressUpdateFuntion);
        Result := Result and LResult;
        if not UpdateProgress(LResult) then Exit;
      end;

      //Pumping channel control file(PMP.dat)
      LCurrentFileNames := FileNamesObject.PumpingChannelControlFileNames;
      if ValidateFileName(LCurrentFileNames.FileNameObject[00],AProgressUpdateFuntion) then
      begin
        LResult := FFilePumpingChannelControlDatabaseAgent.ReadModelDataFromDatabase(TFileNameObject(LCurrentFileNames.FileNameObject[00]),FDataFileObjects,AProgressUpdateFuntion);
        Result := Result and LResult;
        if not UpdateProgress(LResult) then Exit;
      end;

      //General channel control file(PUR.dat)
      LCurrentFileNames := FileNamesObject.GeneralChannelControlFileNames;
      if ValidateFileName(LCurrentFileNames.FileNameObject[00],AProgressUpdateFuntion) then
      begin
        LResult := FFileGeneralChannelControlDatabaseAgent.ReadModelDataFromDatabase(TFileNameObject(LCurrentFileNames.FileNameObject[00]),FDataFileObjects,AProgressUpdateFuntion);
        Result := Result and LResult;
        if not UpdateProgress(LResult) then Exit;
      end;

      //Annual growth factors file(PUR.dat)
      LCurrentFileNames := FileNamesObject.GrowthFactorsFileNames;
      if ValidateFileName(LCurrentFileNames.FileNameObject[00],AProgressUpdateFuntion) then
      begin
        LResult := FFileGrowthFactorslDatabaseAgent.ReadModelDataFromDatabase(TFileNameObject(LCurrentFileNames.FileNameObject[00]),FDataFileObjects,AProgressUpdateFuntion);
        Result := Result and LResult;
        if not UpdateProgress(LResult) then Exit;
      end;


      // Disbenefit (DBF.dat)
      LCurrentFileNames := FileNamesObject.DisbenefitDefinitionFileNames;
      if ValidateFileName(LCurrentFileNames.FileNameObject[00],AProgressUpdateFuntion) then
      begin
        LResult := FFileDisbenefitDatabaseAgent.ReadModelDataFromDatabase(TFileNameObject(LCurrentFileNames.FileNameObject[00]),FDataFileObjects,AProgressUpdateFuntion);
        Result := Result and LResult;
        if not UpdateProgress(LResult) then Exit;
      end;
      //Return Flow Channel (RET.dat)
      LCurrentFileNames := FileNamesObject.ReturnFlowChannelFileNames;
      if ValidateFileName(LCurrentFileNames.FileNameObject[00],AProgressUpdateFuntion) then
      begin
        LResult := FFileReturnFlowChannelDatabaseAgent.ReadModelDataFromDatabase(TFileNameObject(LCurrentFileNames.FileNameObject[00]),FDataFileObjects,AProgressUpdateFuntion);
        Result := Result and LResult;
        if not UpdateProgress(LResult) then Exit;
      end;

    //Allocation Channel file(ALO.dat)
    LCurrentFileNames :=   FileNamesObject.AllocationChannelFileNames;
    if ValidateFileName(LCurrentFileNames.FileNameObject[00],AProgressUpdateFuntion) then
    begin
      LResult := FFileAllocationChannelControlDatabseAgent.ReadModelDataFromDatabase(TFileNameObject(LCurrentFileNames.FileNameObject[00]),FDataFileObjects,AProgressUpdateFuntion);
      Result := Result and LResult;
      if not UpdateProgress(LResult) then Exit;
    end;

    //Control Release file(REL.dat)
    LCurrentFileNames :=   FileNamesObject.ReleaseStructureFileNames;
    if ValidateFileName(LCurrentFileNames.FileNameObject[00],AProgressUpdateFuntion) then
    begin
      LResult := FFileControlledReleaseStructureDatabaseAgent.ReadModelDataFromDatabase(TFileNameObject(LCurrentFileNames.FileNameObject[00]),FDataFileObjects,AProgressUpdateFuntion);
      Result := Result and LResult;
      if not UpdateProgress(LResult) then Exit;
    end;

    //Hydro Allocation file(HYD.dat)
     LCurrentFileNames := FileNamesObject.HydropowerAllocationFileNames;
     if ValidateFileName(LCurrentFileNames.FileNameObject[00],AProgressUpdateFuntion) then
     begin
       LResult := FFileHydroPowerAllocationControlDatabaseAgent.ReadModelDataFromDatabase(TFileNameObject(LCurrentFileNames.FileNameObject[00]),FDataFileObjects,AProgressUpdateFuntion);
       Result := Result and LResult;
     end;

     // Tarrif calculations (Tar.dat)
     LCurrentFileNames := FileNamesObject.TariffCalculationFileNames;
     if ValidateFileName(LCurrentFileNames.FileNameObject[00],AProgressUpdateFuntion) then
     begin
       LResult := FFileTariffCalculationDatabseAgent.ReadModelDataFromDatabase(TFileNameObject(LCurrentFileNames.FileNameObject[00]),FDataFileObjects,AProgressUpdateFuntion);
       Result := Result and LResult;
     end;

     // Monthly Water Requirement (Hst.dat)
     LCurrentFileNames := FileNamesObject.MonthlyWaterRequirementFileNames;
     if ValidateFileName(LCurrentFileNames.FileNameObject[00],AProgressUpdateFuntion) then
     begin
       LResult := FFileMonthlyWaterRequirementDatabaseAgent.ReadModelDataFromDatabase(TFileNameObject(LCurrentFileNames.FileNameObject[00]),FDataFileObjects,AProgressUpdateFuntion);
       Result := Result and LResult;
     end;

    // ReclamationPlant Control (Rec.dat)
     LCurrentFileNames := FileNamesObject.ReclamationPlantControlFileNames;
     if ValidateFileName(LCurrentFileNames.FileNameObject[00],AProgressUpdateFuntion) then
     begin
       LResult := FFileReclamationPlantDatabaseAgent.ReadModelDataFromDatabase(TFileNameObject(LCurrentFileNames.FileNameObject[00]),FDataFileObjects,AProgressUpdateFuntion);
       Result := Result and LResult;
     end;

     // Restriction definition file (cur.dat)
     LCurrentFileNames := FileNamesObject.CurtailFileNames;
     if ValidateFileName(LCurrentFileNames.FileNameObject[00],AProgressUpdateFuntion) then
     begin
       LResult := FFileCurtailDatabaseAgent.ReadModelDataFromDatabase(TFileNameObject(LCurrentFileNames.FileNameObject[00]),FDataFileObjects,AProgressUpdateFuntion);
       Result := Result and LResult;
     end;

     // Salts Wash Off files (MISW...dat)
     LCurrentFileNames := FileNamesObject.SaltsWashOffFileNames;
     for LCount := 0 to LCurrentFileNames.Count - 1 do
     Begin
       if not IncludeSaltWashOffFile(LCurrentFileNames.FileNameObject[LCount]) then
         Continue;
       if ValidateFileName(LCurrentFileNames.FileNameObject[LCount],AProgressUpdateFuntion) then
       begin
       LResult := FFileSaltWashOffDatabaseAgent.ReadModelDataFromDatabase(TFileNameObject(LCurrentFileNames.FileNameObject[LCount]),FDataFileObjects,AProgressUpdateFuntion);
       Result := Result and LResult;
       if LResult then
       end;
     end;
	 
     //Mine files(MIMM*.dat ...)
      LCurrentFileNames := FileNamesObject.MineFileNames;
      for LCount := 0 to LCurrentFileNames.Count - 1 do
      begin
        if not IncludeMIMMFile(LCurrentFileNames.FileNameObject[LCount]) then
           Continue;
        if ValidateFileName(LCurrentFileNames.FileNameObject[LCount],AProgressUpdateFuntion) then
        begin
          LResult := FFileMIMMDatabaseAgent.ReadModelDataFromDatabase(TFileNameObject(LCurrentFileNames.FileNameObject[LCount]),
                     FDataFileObjects,AProgressUpdateFuntion);
          Result := Result and LResult;
          if not UpdateProgress(LResult) then Exit;
        end;

      end;
      TPlanningFileDataObjects(FDataFileObjects).AddPlanningNodeWitoutInflow;
      TPlanningFileDataObjects(FDataFileObjects).UpdatePlanningNetworkElementsType;
    finally
     FFileAction := LFileAction;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFilesActionPlanningManager.ExecSaveDataToDatabase(AProgressUpdateFuntion: TProgressUpdateFuntion): boolean;
const OPNAME = 'TFilesActionPlanningManager.ExecSaveDataToDatabase';
var
  LCurrentFileNames: TAbstractFileNamesList;
  LCount: integer;
  LResult: boolean;
  LImportDate: TDateTime;
begin
  Result := inherited ExecSaveDataToDatabase(AProgressUpdateFuntion);
  try
    if (not Result) and FAppModules.GlobalData.StopOnFirstErr then
      Exit;
    Result := True;
    // Set study import date to now
    LImportDate := Now();

    //AllocationDefinition files(SW*.dat ...)
    LCurrentFileNames := FileNamesObject.ChannelSwitchControlFileNames;
    for LCount := 0 to LCurrentFileNames.Count - 1 do
    begin
      if not IncludeChannelSwitchFile(LCurrentFileNames.FileNameObject[LCount]) then
         Continue;
      if ValidateFileName(LCurrentFileNames.FileNameObject[LCount],AProgressUpdateFuntion) then
      begin
        TFileNameObject(LCurrentFileNames.FileNameObject[LCount]).ImportDate := LImportDate;
        LResult := FFileChannelSwitchControlDatabaseAgent.WriteModelDataToDatabase(TFileNameObject(LCurrentFileNames.FileNameObject[LCount]),
                   FDataFileObjects,AProgressUpdateFuntion);
        Result := Result and LResult;
        if not UpdateProgress(LResult) then Exit;
      end;
    end;


    //AllocationDefinition files(FM*.dat ...)
    LCurrentFileNames := FileNamesObject.AllocationDefinitionFileNames;
    for LCount := 0 to LCurrentFileNames.Count - 1 do
    begin
      if not IncludeAllocationDefinitionFile(LCurrentFileNames.FileNameObject[LCount]) then
         Continue;
      if ValidateFileName(LCurrentFileNames.FileNameObject[LCount],AProgressUpdateFuntion) then
      begin
        TFileNameObject(LCurrentFileNames.FileNameObject[LCount]).ImportDate := LImportDate;
        LResult := FFileAllocationDefinitionDatabaseAgent.WriteModelDataToDatabase(TFileNameObject(LCurrentFileNames.FileNameObject[LCount]),
                   FDataFileObjects,AProgressUpdateFuntion);
        Result := Result and LResult;
        if not UpdateProgress(LResult) then Exit;
      end;
    end;

    //Reservoir Implementation file(DAM.dat)
    LCurrentFileNames := FileNamesObject.ReservoirImplementationFileNames;
    if ValidateFileName(LCurrentFileNames.FileNameObject[00],AProgressUpdateFuntion) then
    begin
      TFileNameObject(LCurrentFileNames.FileNameObject[00]).ImportDate := LImportDate;
      LResult := FFileReservoirImplementationDatabaseAgent.WriteModelDataToDatabase(TFileNameObject(LCurrentFileNames.FileNameObject[00]),
                 FDataFileObjects,AProgressUpdateFuntion);
      Result := Result and LResult;
      if not UpdateProgress(LResult) then Exit;
    end;

    //Pumping channel control file(PMP.dat)
    LCurrentFileNames := FileNamesObject.PumpingChannelControlFileNames;
    if ValidateFileName(LCurrentFileNames.FileNameObject[00],AProgressUpdateFuntion) then
    begin
      LResult := FFilePumpingChannelControlDatabaseAgent.WriteModelDataToDatabase(TFileNameObject(LCurrentFileNames.FileNameObject[00]),FDataFileObjects,AProgressUpdateFuntion);
      Result := Result and LResult;
      if not UpdateProgress(LResult) then Exit;
    end;

    //General channel control file(PUR.dat)
    LCurrentFileNames := FileNamesObject.GeneralChannelControlFileNames;
    if ValidateFileName(LCurrentFileNames.FileNameObject[00],AProgressUpdateFuntion) then
    begin
      LResult := FFileGeneralChannelControlDatabaseAgent.WriteModelDataToDatabase(TFileNameObject(LCurrentFileNames.FileNameObject[00]),FDataFileObjects,AProgressUpdateFuntion);
      Result := Result and LResult;
      if not UpdateProgress(LResult) then Exit;
    end;

    //Annual growth factors file(PUR.dat)
    LCurrentFileNames := FileNamesObject.GrowthFactorsFileNames;
    if ValidateFileName(LCurrentFileNames.FileNameObject[00],AProgressUpdateFuntion) then
    begin
      LResult := FFileGrowthFactorslDatabaseAgent.WriteModelDataToDatabase(TFileNameObject(LCurrentFileNames.FileNameObject[00]),FDataFileObjects,AProgressUpdateFuntion);
      Result := Result and LResult;
      if not UpdateProgress(LResult) then Exit;
    end;

    // Disbenefit (DBF.dat)
    LCurrentFileNames := FileNamesObject.DisbenefitDefinitionFileNames;
    if ValidateFileName(LCurrentFileNames.FileNameObject[00],AProgressUpdateFuntion) then
    begin
      LResult := FFileDisbenefitDatabaseAgent.WriteModelDataToDatabase(TFileNameObject(LCurrentFileNames.FileNameObject[00]),FDataFileObjects,AProgressUpdateFuntion);
      Result := Result and LResult;
      if not UpdateProgress(LResult) then Exit;
    end;

    // Return Flow Channel (RET.dat)
    LCurrentFileNames := FileNamesObject.ReturnFlowChannelFileNames;
    if ValidateFileName(LCurrentFileNames.FileNameObject[00],AProgressUpdateFuntion) then
    begin
      LResult := FFileReturnFlowChannelDatabaseAgent.WriteModelDataToDatabase(TFileNameObject(LCurrentFileNames.FileNameObject[00]),FDataFileObjects,AProgressUpdateFuntion);
      Result := Result and LResult;
      if not UpdateProgress(LResult) then Exit;
    end;

    //Allocation Channel file(ALO.dat)
    LCurrentFileNames :=   FileNamesObject.AllocationChannelFileNames;
    if ValidateFileName(LCurrentFileNames.FileNameObject[00],AProgressUpdateFuntion) then
    begin
      LResult := FFileAllocationChannelControlDatabseAgent.WriteModelDataToDatabase(TFileNameObject(LCurrentFileNames.FileNameObject[00]),FDataFileObjects,AProgressUpdateFuntion);
      Result := Result and LResult;
      if not UpdateProgress(LResult) then Exit;
    end;

    //Control Release file(REL.dat)
    LCurrentFileNames :=   FileNamesObject.ReleaseStructureFileNames;
    if ValidateFileName(LCurrentFileNames.FileNameObject[00],AProgressUpdateFuntion) then
    begin
      LResult := FFileControlledReleaseStructureDatabaseAgent.WriteModelDataToDatabase(TFileNameObject(LCurrentFileNames.FileNameObject[00]),FDataFileObjects,AProgressUpdateFuntion);
      Result := Result and LResult;
      if not UpdateProgress(LResult) then Exit;
    end;

    //Hydro Allocation file(HYD.dat)
     LCurrentFileNames := FileNamesObject.HydropowerAllocationFileNames;
     if ValidateFileName(LCurrentFileNames.FileNameObject[00],AProgressUpdateFuntion) then
     begin
       LResult := FFileHydroPowerAllocationControlDatabaseAgent.WriteModelDataToDatabase(TFileNameObject(LCurrentFileNames.FileNameObject[00]),FDataFileObjects,AProgressUpdateFuntion);
       Result := Result and LResult;
     end;

     // Tarrif calculations (Tar.dat)
     LCurrentFileNames := FileNamesObject.TariffCalculationFileNames;
     if ValidateFileName(LCurrentFileNames.FileNameObject[00],AProgressUpdateFuntion) then
     begin
       LResult := FFileTariffCalculationDatabseAgent.WriteModelDataToDatabase(TFileNameObject(LCurrentFileNames.FileNameObject[00]),FDataFileObjects,AProgressUpdateFuntion);
       Result := Result and LResult;
     end;

     // Monthly Water Requirement (Hst.dat)
     LCurrentFileNames := FileNamesObject.MonthlyWaterRequirementFileNames;
     if ValidateFileName(LCurrentFileNames.FileNameObject[00],AProgressUpdateFuntion) then
     begin
       LResult := FFileMonthlyWaterRequirementDatabaseAgent.WriteModelDataToDatabase(TFileNameObject(LCurrentFileNames.FileNameObject[00]),FDataFileObjects,AProgressUpdateFuntion);
       Result := Result and LResult;
     end;

    // ReclamationPlant Control (Rec.dat)
     LCurrentFileNames := FileNamesObject.ReclamationPlantControlFileNames;
     if ValidateFileName(LCurrentFileNames.FileNameObject[00],AProgressUpdateFuntion) then
     begin
       LResult := FFileReclamationPlantDatabaseAgent.WriteModelDataToDatabase(TFileNameObject(LCurrentFileNames.FileNameObject[00]),FDataFileObjects,AProgressUpdateFuntion);
       Result := Result and LResult;
     end;

     // Restriction definition file (cur.dat)
     LCurrentFileNames := FileNamesObject.CurtailFileNames;
     if ValidateFileName(LCurrentFileNames.FileNameObject[00],AProgressUpdateFuntion) then
     begin
       LResult := FFileCurtailDatabaseAgent.WriteModelDataToDatabase(TFileNameObject(LCurrentFileNames.FileNameObject[00]),FDataFileObjects,AProgressUpdateFuntion);
       Result := Result and LResult;
     end;

     // Salts Wash Off file (MISW...dat)
     LCurrentFileNames := FileNamesObject.SaltsWashOffFileNames;
     for LCount := 0 to LCurrentFileNames.Count - 1 do
     Begin
       if not IncludeSaltWashOffFile(LCurrentFileNames.FileNameObject[LCount]) then
         Continue;
       if ValidateFileName(LCurrentFileNames.FileNameObject[LCount],AProgressUpdateFuntion) then
       begin
       LResult := FFileSaltWashOffDatabaseAgent.WriteModelDataToDatabase(TFileNameObject(LCurrentFileNames.FileNameObject[LCount]),FDataFileObjects,AProgressUpdateFuntion);
       Result := Result and LResult;
       if LResult then
       end;
     end;
	 
  //Mine files(MIMM*.dat ...)
    LCurrentFileNames := FileNamesObject.MineFileNames;
    for LCount := 0 to LCurrentFileNames.Count - 1 do
    begin
      if not IncludeMIMMFile(LCurrentFileNames.FileNameObject[LCount]) then
         Continue;
      if ValidateFileName(LCurrentFileNames.FileNameObject[LCount],AProgressUpdateFuntion) then
      begin
        TFileNameObject(LCurrentFileNames.FileNameObject[LCount]).ImportDate := LImportDate;
        LResult := FFileMIMMDatabaseAgent.WriteModelDataToDatabase(TFileNameObject(LCurrentFileNames.FileNameObject[LCount]),
                   FDataFileObjects,AProgressUpdateFuntion);
        Result := Result and LResult;
        if not UpdateProgress(LResult) then Exit;
      end;
    end;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFilesActionPlanningManager.ExecClearModelData(AProgressUpdateFuntion: TProgressUpdateFuntion): boolean;
const OPNAME = 'TFilesActionPlanningManager.ExecClearModelData';
var
  LCurrentFileNames: TAbstractFileNamesList;
  LCount: integer;
  LResult: boolean;
begin
  Result := inherited  ExecClearModelData(AProgressUpdateFuntion);
  try
    if (not Result) and FAppModules.GlobalData.StopOnFirstErr then
      Exit;
    Result := True;

    //AllocationDefinition files(SW*.dat ...)
    LCurrentFileNames := FileNamesObject.ChannelSwitchControlFileNames;
    for LCount := 0 to LCurrentFileNames.Count - 1 do
    begin
      if not IncludeChannelSwitchFile(LCurrentFileNames.FileNameObject[LCount]) then
         Continue;
      if ValidateFileName(LCurrentFileNames.FileNameObject[LCount],AProgressUpdateFuntion) then
      begin
        LResult := FFileChannelSwitchControlDatabaseAgent.ClearModelDataInDatabase(TFileNameObject(LCurrentFileNames.FileNameObject[LCount]),AProgressUpdateFuntion);
        Result := Result and LResult;
        if not UpdateProgress(LResult) then Exit;
      end;
    end;

    //AllocationDefinition files(FM*.dat ...)
    LCurrentFileNames := FileNamesObject.AllocationDefinitionFileNames;
    for LCount := 0 to LCurrentFileNames.Count - 1 do
    begin
      if not IncludeAllocationDefinitionFile(LCurrentFileNames.FileNameObject[LCount]) then
         Continue;
      if ValidateFileName(LCurrentFileNames.FileNameObject[LCount],AProgressUpdateFuntion) then
      begin
        LResult := FFileAllocationDefinitionDatabaseAgent.ClearModelDataInDatabase(TFileNameObject(LCurrentFileNames.FileNameObject[LCount]),AProgressUpdateFuntion);
        Result := Result and LResult;
        if not UpdateProgress(LResult) then Exit;
      end;
    end;

    //Reservoir Implementation file(DAM.dat)
    LCurrentFileNames := FileNamesObject.ReservoirImplementationFileNames;
    if ValidateFileName(LCurrentFileNames.FileNameObject[00],AProgressUpdateFuntion) then
    begin
      LResult := FFileReservoirImplementationDatabaseAgent.ClearModelDataInDatabase(TFileNameObject(LCurrentFileNames.FileNameObject[00]),AProgressUpdateFuntion);
      Result := Result and LResult;
      if not UpdateProgress(LResult) then Exit;
    end;

    //Pumping channel control file(PMP.dat)
    LCurrentFileNames := FileNamesObject.PumpingChannelControlFileNames;
    if ValidateFileName(LCurrentFileNames.FileNameObject[00],AProgressUpdateFuntion) then
    begin
      LResult := FFilePumpingChannelControlDatabaseAgent.ClearModelDataInDatabase(TFileNameObject(LCurrentFileNames.FileNameObject[00]),AProgressUpdateFuntion);
      Result := Result and LResult;
      if not UpdateProgress(LResult) then Exit;
    end;

    //General channel control file(PUR.dat)
    LCurrentFileNames := FileNamesObject.GeneralChannelControlFileNames;
    if ValidateFileName(LCurrentFileNames.FileNameObject[00],AProgressUpdateFuntion) then
    begin
      LResult := FFileGeneralChannelControlDatabaseAgent.ClearModelDataInDatabase(TFileNameObject(LCurrentFileNames.FileNameObject[00]),AProgressUpdateFuntion);
      Result := Result and LResult;
      if not UpdateProgress(LResult) then Exit;
    end;

    //Annual growth factors file(PUR.dat)
    LCurrentFileNames := FileNamesObject.GrowthFactorsFileNames;
    if ValidateFileName(LCurrentFileNames.FileNameObject[00],AProgressUpdateFuntion) then
    begin
      LResult := FFileGrowthFactorslDatabaseAgent.ClearModelDataInDatabase(TFileNameObject(LCurrentFileNames.FileNameObject[00]),AProgressUpdateFuntion);
      Result := Result and LResult;
      if not UpdateProgress(LResult) then Exit;
    end;

    // Disbenefit (DBF.dat)
    LCurrentFileNames := FileNamesObject.DisbenefitDefinitionFileNames;
    if ValidateFileName(LCurrentFileNames.FileNameObject[00],AProgressUpdateFuntion) then
    begin
      LResult := FFileDisbenefitDatabaseAgent.ClearModelDataInDatabase(TFileNameObject(LCurrentFileNames.FileNameObject[00]),AProgressUpdateFuntion);
      Result := Result and LResult;
      if not UpdateProgress(LResult) then Exit;
    end;
   // Return Flow Channel (RET.dat)
    LCurrentFileNames := FileNamesObject.ReturnFlowChannelFileNames;
    if ValidateFileName(LCurrentFileNames.FileNameObject[00],AProgressUpdateFuntion) then
    begin
      LResult := FFileReturnFlowChannelDatabaseAgent.ClearModelDataInDatabase(TFileNameObject(LCurrentFileNames.FileNameObject[00]),AProgressUpdateFuntion);
      Result := Result and LResult;
      if not UpdateProgress(LResult) then Exit;
    end;

   //Allocation Channel file(ALO.dat)
    LCurrentFileNames := FileNamesObject.AllocationChannelFileNames;
    if ValidateFileName(LCurrentFileNames.FileNameObject[00],AProgressUpdateFuntion) then
    begin
      LResult := FFileAllocationChannelControlDatabseAgent.ClearModelDataInDatabase(TFileNameObject(LCurrentFileNames.FileNameObject[00]),AProgressUpdateFuntion);
      Result := Result and LResult;
      if not UpdateProgress(LResult) then Exit;
    end;

   //Controlled Realesed Structure file(REL.dat)
    LCurrentFileNames := FileNamesObject.ReleaseStructureFileNames;
    if ValidateFileName(LCurrentFileNames.FileNameObject[00],AProgressUpdateFuntion) then
    begin
      LResult := FFileControlledReleaseStructureDatabaseAgent.ClearModelDataInDatabase(TFileNameObject(LCurrentFileNames.FileNameObject[00]),AProgressUpdateFuntion);
      Result := Result and LResult;
      if not UpdateProgress(LResult) then Exit;
    end;

    //Hydro Power Allocation file(HYD.dat)
    LCurrentFileNames := FileNamesObject.HydropowerAllocationFileNames;
    if ValidateFileName(LCurrentFileNames.FileNameObject[00],AProgressUpdateFuntion) then
    begin
      LResult := FFileAllocationChannelControlDatabseAgent.ClearModelDataInDatabase(TFileNameObject(LCurrentFileNames.FileNameObject[00]),AProgressUpdateFuntion);
      Result := Result and LResult;
      if not UpdateProgress(LResult) then Exit;
    end;

     // Tarrif calculations (Tar.dat)
     LCurrentFileNames := FileNamesObject.TariffCalculationFileNames;
    if ValidateFileName(LCurrentFileNames.FileNameObject[00],AProgressUpdateFuntion) then
    begin
      LResult := FFileTariffCalculationDatabseAgent.ClearModelDataInDatabase(TFileNameObject(LCurrentFileNames.FileNameObject[00]),AProgressUpdateFuntion);
      Result := Result and LResult;
      if not UpdateProgress(LResult) then Exit;
    end;

     // Monthly Water Requirement (Hst.dat)
     LCurrentFileNames := FileNamesObject.MonthlyWaterRequirementFileNames;
     if ValidateFileName(LCurrentFileNames.FileNameObject[00],AProgressUpdateFuntion) then
     begin
       LResult := FFileMonthlyWaterRequirementDatabaseAgent.ClearModelDataInDatabase(TFileNameObject(LCurrentFileNames.FileNameObject[00]),AProgressUpdateFuntion);
       Result := Result and LResult;
     end;

     // ReclamationPlant Control (Rec.dat)
     LCurrentFileNames := FileNamesObject.ReclamationPlantControlFileNames;
     if ValidateFileName(LCurrentFileNames.FileNameObject[00],AProgressUpdateFuntion) then
     begin
       LResult := FFileReclamationPlantDatabaseAgent.ClearModelDataInDatabase(TFileNameObject(LCurrentFileNames.FileNameObject[00]),AProgressUpdateFuntion);
       Result := Result and LResult;
     end;

     // Restriction definition file (cur.dat)
     LCurrentFileNames := FileNamesObject.CurtailFileNames;
     if ValidateFileName(LCurrentFileNames.FileNameObject[00],AProgressUpdateFuntion) then
     begin
       LResult := FFileCurtailDatabaseAgent.ClearModelDataInDatabase(TFileNameObject(LCurrentFileNames.FileNameObject[00]),AProgressUpdateFuntion);
       Result := Result and LResult;
     end;

     // Salts Wash Off files (MISW...dat)
     LCurrentFileNames := FilenamesObject.SaltsWashoffFileNames;
     for Lcount := 0 to LCurrentFileNames.Count - 1 do
     Begin
       if not IncludeSaltWashOffFile(LCurrentFileNames.FileNameObject[LCount]) then
         Continue;
       if ValidateFileName(LCurrentFileNames.FileNameObject[LCount],AProgressUpdateFuntion) then
       begin
       LResult := FFileSaltWashOffDatabaseAgent.ClearModelDataInDatabase(TFileNameObject(LCurrentFileNames.FileNameObject[LCount]),AProgressUpdateFuntion);
       Result := Result and LResult;
       end;
     end;
	 
 //Mining files(MIMM*.dat ...)
    LCurrentFileNames := FileNamesObject.MineFileNames;
    for LCount := 0 to LCurrentFileNames.Count - 1 do
    begin
      if not IncludeMIMMFile(LCurrentFileNames.FileNameObject[LCount]) then
         Continue;
      if ValidateFileName(LCurrentFileNames.FileNameObject[LCount],AProgressUpdateFuntion) then
      begin
        LResult := FFileMIMMDatabaseAgent.ClearModelDataInDatabase(TFileNameObject(LCurrentFileNames.FileNameObject[LCount]),AProgressUpdateFuntion);
        Result := Result and LResult;
        if not UpdateProgress(LResult) then Exit;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFilesActionPlanningManager.ExecValidateModelData(AProgressUpdateFuntion: TProgressUpdateFuntion): boolean;
const OPNAME = 'TFilesActionPlanningManager.ExecValidateModelData';
var
  LColumns,
  LValidationErrors: TStringList;
  LErrors: WideString;
  LCount: integer;
  LStop: boolean;
begin
  Result := False;
  try
    if not Assigned(AProgressUpdateFuntion) then
      AProgressUpdateFuntion := DummyShowProgress;
    LColumns := TStringList.Create;
    LValidationErrors := TStringList.Create;
    try
      AProgressUpdateFuntion('Started validating model data',ptNone,LStop);
      Result := TPlanningModelDataObject(FAppModules.Model.ModelData).Validate(LErrors,'');
      while (LErrors <> '') do
      begin
        LErrors := CutErrorsAndColumns(LErrors,LValidationErrors,LColumns);
        for LCount := 0 to LValidationErrors.Count -1 do
        begin
          if(Pos('WARNING:',LValidationErrors[LCount]) = 1) then
            AProgressUpdateFuntion(LValidationErrors[LCount],ptWarning,LStop)
          else
            AProgressUpdateFuntion(LValidationErrors[LCount],ptError,LStop);
        end;
      end;
      AProgressUpdateFuntion('Completed validating model data',ptNone,LStop);
    finally
      LColumns.Free;
      LValidationErrors.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFilesActionPlanningManager.IncludeAllocationDefinitionFile(AFileName: TAbstractModelFileName): boolean;
const OPNAME = 'TFilesActionPlanningManager.IncludeAllocationDefinitionFile';
begin
  Result := False;
  try
    if Assigned(AFileName) and (Trim(AFileName.FileName) <> '')then
    begin
      case FFileAction of
        fatValidateAll,fatClearModelData, fatImportAll, fatExportAll,fatRunModel:
          begin
            Result := True;
          end;

        fatValidateSingle, fatExportSingle, fatImportSingle:
          begin
            Result := AFileName.Selected;
          end;

        fatImportSumOut, fatSaveOutputFiles:
          begin
            Result := AFileName.Selected;
          end;
      end;//case
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFilesActionPlanningManager.IncludeChannelSwitchFile(AFileName: TAbstractModelFileName): boolean;
const OPNAME = 'TFilesActionPlanningManager.IncludeChannelSwitchFile';
begin
  Result := False;
  try
    if Assigned(AFileName) and (Trim(AFileName.FileName) <> '')then
    begin
      case FFileAction of
        fatValidateAll,fatClearModelData, fatImportAll, fatExportAll,fatRunModel:
          begin
            Result := True;
          end;

        fatValidateSingle, fatExportSingle, fatImportSingle:
          begin
            Result := AFileName.Selected;
          end;

        fatImportSumOut, fatSaveOutputFiles:
          begin
            Result := AFileName.Selected;
          end;
      end;//case
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFilesActionPlanningManager.IncludeSaltWashOffFile(AFileName: TAbstractModelFileName): boolean;
const OPNAME = 'TFilesActionPlanningManager.IncludeSaltWashOffFile';
begin
  Result := False;
  try
    if Assigned(AFileName) and (Trim(AFileName.FileName) <> '')then
    begin
      case FFileAction of
        fatValidateAll,fatClearModelData, fatImportAll, fatExportAll,fatRunModel:
          begin
            Result := True;
          end;

        fatValidateSingle, fatExportSingle, fatImportSingle:
          begin
            Result := AFileName.Selected;
          end;

        fatImportSumOut, fatSaveOutputFiles:
          begin
            Result := AFileName.Selected;
          end;
      end;//case
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFilesActionPlanningManager.IncludeMIMMFile(AFileName: TAbstractModelFileName): boolean;
const OPNAME = 'TFilesActionPlanningManager.IncludeChannelSwitchFile';
begin
  Result := False;
  try
    if Assigned(AFileName) and (Trim(AFileName.FileName) <> '')then
    begin
      case FFileAction of
        fatValidateAll,fatClearModelData, fatImportAll, fatExportAll,fatRunModel:
          begin
            Result := True;
          end;

        fatValidateSingle, fatExportSingle, fatImportSingle:
          begin
            Result := AFileName.Selected;
          end;

        fatImportSumOut, fatSaveOutputFiles:
          begin
            Result := AFileName.Selected;
          end;
      end;//case
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFilesActionPlanningManager.CheckModelFilesAreComplete(AProgressUpdateFuntion: TProgressUpdateFuntion): boolean;
const OPNAME = 'TFilesActionPlanningManager.CheckModelFilesAreComplete';
begin
  Result := False;
  try
    if (FFileAction in [fatValidateSingle,fatImportSingle] ) then
    begin
      if (FileNamesObject.GrowthFactorsFileNames.SelectedCount > 0) then
      begin
        FileNamesObject.CastConfigFileNames.FileNameObject[0].Selected := True;
        FileNamesObject.CastParamFileNames.FileNameObject[0].Selected := True;
      end;
    end;

    Result := inherited CheckModelFilesAreComplete(AProgressUpdateFuntion);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFilesActionPlanningManager.CreateRunYieldModelAgent;
const OPNAME = 'TFilesActionPlanningManager.CreateRunYieldModelAgent';
begin
  try
    FRunModelAgent  := TRunPlanningModelAgent.Create(FAppModules);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFilesActionPlanningManager.ExecRunModel(AProgressUpdateFuntion: TProgressUpdateFuntion): boolean;
const OPNAME = 'TFilesActionPlanningManager.ExecRunModel';
var
  LCurrentFileNames: TAbstractFileNamesList;
  LWQTOutputPath : string;
begin
  Result := False;
  try
    if not CheckModelFilesAreComplete(FProgressDialog.ShowProgress) then Exit;
    LCurrentFileNames := FileNamesObject.DirectoryFileNames;
    Result := ValidateFileName(LCurrentFileNames.FileNameObject[00],AProgressUpdateFuntion);
    Result := Result and  FFilePathsAgent.ReadModelDataFromFile(LCurrentFileNames.FileNameObject[00],
                   FDataFileObjects,AProgressUpdateFuntion);
    if not UpdateProgress(Result) then Exit;

    Result := Result and FRunModelAgent.CheckInputFiles(FDataFileObjects,AProgressUpdateFuntion);
    if not UpdateProgress(Result) then Exit;

    if(FAppModules.User.UserType <> utExpert) then
      FileNamesObject.OutputFileNames.DeleteAllFiles;

    LWQTOutputPath := FileNamesObject.OutputFilesPath+'\'+CWQT;
    if not DirectoryExists(LWQTOutputPath) then
      ForceDirectories(LWQTOutputPath);

    FProgressDialog.FStopButton.Enabled := False;
    Result := Result and FRunModelAgent.RunModel(FDataFileObjects,AProgressUpdateFuntion,FProgressDialog.Handle);
    FProgressDialog.FStopButton.Enabled := True;
    if not UpdateProgress(Result) then Exit;

    Result := Result and FRunModelAgent.CheckOutputFiles(FDataFileObjects,AProgressUpdateFuntion);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.

