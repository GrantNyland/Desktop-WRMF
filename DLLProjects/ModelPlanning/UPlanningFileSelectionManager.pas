//
//
//  UNIT      : Contains TPlanningFileSelectionManager Class
//  AUTHOR    : Dziedzi Ramulondi
//  DATE      : 04/01/2006
//  COPYRIGHT : Copyright © 2006 DWAF
//
//
unit UPlanningFileSelectionManager;

interface

uses
  Classes,
  VCL.ComCtrls,
  UFileNames,
  UAbstractObject,
  UDataFileObjects,
  UAbstractFileNamesObject,
  UYieldFileSelectionManager,
  UPlanningFileNamesFileAgent,
  UPlanningFileNamesDatabaseAgent;

type
  TPlanningFileSelectionManager = class(TYieldFileSelectionManager)
  protected
    procedure CreateFileNamesAgents; override;
    function CreateAllSingleFileNames(ADataFileObjects: TDataFileObjects;AFileNamesObject: TModelFileNames): boolean;
    function CreateOutputFileNames(ADataFileObjects: TDataFileObjects;AFileNamesObject: TModelFileNames): boolean; override;
  public
    function PopulateFileNames(ADataFileObjects: TDataFileObjects;AFileNamesObject :TModelFileNames): boolean; override;
    function PopulateTreeView(ATreeView: TTreeView;AFileNamesObject :TModelFileNames): boolean; override;
    function StudyDataHasChanged(AContext: TChangeContext; AFieldName: string; AOldValue: string; ANewValue: string): Boolean; override;
  end;

implementation

uses
  SysUtils,
  UUtilities,
  VoaimsCom_TLB,
  UPlanningModelDataObject,
  UProgressDialog,
  UErrorHandlingOperations;


{ TPlanningFileSelectionManager }

function TPlanningFileSelectionManager.CreateAllSingleFileNames(ADataFileObjects: TDataFileObjects;
  AFileNamesObject: TModelFileNames): boolean;
const OPNAME = 'TPlanningFileSelectionManager.CreateAllSingleFileNames';
var
  LFileName,
  LFilePrefix: string;
begin
  Result := False;
  try
    if Assigned(ADataFileObjects) and Assigned(AFileNamesObject) and
       ADataFileObjects.FPathsObject.InputFilesPath.FInitalised then
    begin
      LFilePrefix := Trim(ADataFileObjects.FPathsObject.InputFilesPath.FData);
      LFilePrefix := IncludeTrailingPathDelimiter(LFilePrefix);
      LFilePrefix := LFilePrefix + Trim(ADataFileObjects.FPathsObject.FileNamePrefix.FData);

      LFileName := LFilePrefix + 'DAM.dat';
      AFileNamesObject.UpdateReservoirImplementationFileName(0,LFileName,False,0.0,0.0);

      LFileName := LFilePrefix + 'DBF.dat';
      AFileNamesObject.UpdateDisbenefitDefinitionFileName(0,LFileName,False,0.0,0.0);

      LFileName := LFilePrefix + 'GTH.dat';
      AFileNamesObject.UpdateGrowthFactorsFileName(0,LFileName,False,0.0,0.0);

      LFileName := LFilePrefix + 'HST.dat';
      AFileNamesObject.UpdateMonthlyWaterRequirementFileName(0,LFileName,False,0.0,0.0);

      LFileName := LFilePrefix + 'HYD.dat';
      AFileNamesObject.UpdateHydropowerAllocationFileName(0,LFileName,False,0.0,0.0);

      LFileName := LFilePrefix + 'PMP.dat';
      AFileNamesObject.UpdatePumpingChannelControlFileName(0,LFileName,False,0.0,0.0);

      LFileName := LFilePrefix + 'PUR.dat';
      AFileNamesObject.UpdateGeneralChannelControlFileName(0,LFileName,False,0.0,0.0);

      LFileName := LFilePrefix + 'REC.dat';
      AFileNamesObject.UpdateReclamationPlantControlFileName(0,LFileName,False,0.0,0.0);

      LFileName := LFilePrefix + 'RET.dat';
      AFileNamesObject.UpdateReturnFlowChannelFileName(0,LFileName,False,0.0,0.0);

      LFileName := LFilePrefix + 'TAR.dat';
      AFileNamesObject.UpdateTariffCalculationFileName(0,LFileName,False,0.0,0.0);

      LFileName := LFilePrefix + 'ALO.dat';
      AFileNamesObject.UpdateAllocationChannelFileName(0,LFileName,False,0.0,0.0);

      LFileName := LFilePrefix + 'REL.dat';
      AFileNamesObject.UpdateReleaseStructureFileName(0,LFileName,False,0.0,0.0);

      LFileName := LFilePrefix + 'CUR.dat';
      AFileNamesObject.UpdateCurtailFileName(0,LFileName,False,0.0,0.0);

    end;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TPlanningFileSelectionManager.CreateFileNamesAgents;
const OPNAME = 'TPlanningFileSelectionManager.CreateFileNamesAgents';
begin
  try
    FFileNamesFileAgent     := TPlanningFileNamesFileAgent.Create(FAppModules);
    FFileNamesDatabaseAgent := TPlanningFileNamesDatabaseAgent.Create(FAppModules);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TPlanningFileSelectionManager.CreateOutputFileNames(ADataFileObjects: TDataFileObjects;AFileNamesObject: TModelFileNames): boolean;
const OPNAME = 'TPlanningFileSelectionManager.CreateOutputFileNames';
{var
  LCurrentFile,
  LFilePrefix: string;
  //LCount: Integer;}
begin
  Result := True;
  try
  {Result := inherited CreateOutputFileNames(ADataFileObjects,AFileNamesObject);
  try
    if Assigned(ADataFileObjects) and Assigned(AFileNamesObject) and
       ADataFileObjects.FPathsObject.OutputFilesPath.FInitalised then
    begin
      LFilePrefix := Trim(ADataFileObjects.FPathsObject.OutputFilesPath.FData);
      LFilePrefix := IncludeTrailingPathDelimiter(LFilePrefix);
      LFilePrefix := LFilePrefix + Trim(ADataFileObjects.FPathsObject.FileNamePrefix.FData);

      LCurrentFile := LFilePrefix + 'Res.out';
      if FileExists(LCurrentFile) then
        AFileNamesObject.UpdateOutputFileName(oftRes,LCurrentFile,False,0.0,
        FileLastWriteDate(LCurrentFile))
      else
        AFileNamesObject.UpdateOutputFileName(oftRes,LCurrentFile,False,0.0,0.0);

      LCurrentFile := LFilePrefix + 'Sys.out';
      if FileExists(LCurrentFile) then
        AFileNamesObject.UpdateOutputFileName(oftSys,LCurrentFile,False,0.0,
        FileLastWriteDate(LCurrentFile))
      else
        AFileNamesObject.UpdateOutputFileName(oftSys,LCurrentFile,False,0.0,0.0);

      LCurrentFile := LFilePrefix + 'Pmp.out';
      if FileExists(LCurrentFile) then
        AFileNamesObject.UpdateOutputFileName(oftPmp,LCurrentFile,False,0.0,
        FileLastWriteDate(LCurrentFile))
      else
        AFileNamesObject.UpdateOutputFileName(oftPmp,LCurrentFile,False,0.0,0.0);
      Result := True;
    end;}
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TPlanningFileSelectionManager.PopulateFileNames(ADataFileObjects: TDataFileObjects;AFileNamesObject: TModelFileNames): boolean;
const OPNAME = 'TPlanningFileSelectionManager.PopulateFileNames';
var
  LResult : boolean;
begin
  Result := False;
  try
    if not Assigned(AFileNamesObject) then
      raise Exception.Create('File object parameter is not yet assigned.');

    AFileNamesObject.Reset;
    Result := inherited PopulateFileNames(ADataFileObjects,AFileNamesObject);

    if Result then
    begin
      CreateAllSingleFileNames(ADataFileObjects,AFileNamesObject);

      TPlanningFileNamesDatabaseAgent(FFileNamesDatabaseAgent).ReadAllocationDefinitionFileNames(ADataFileObjects,AFileNamesObject);
      TPlanningFileNamesFileAgent(FFileNamesFileAgent).ReadAllocationDefinitionFileNames(ADataFileObjects,AFileNamesObject,nil);

      LResult := TPlanningFileNamesDatabaseAgent(FFileNamesDatabaseAgent).ReadReservoirImplementationFileName(ADataFileObjects,AFileNamesObject);
      if not LResult then
        TPlanningFileNamesFileAgent(FFileNamesFileAgent).ReadReservoirImplementationFileName(ADataFileObjects,AFileNamesObject,nil);

      LResult := TPlanningFileNamesDatabaseAgent(FFileNamesDatabaseAgent).ReadDisbenefitDefinitionFileName(ADataFileObjects,AFileNamesObject);
      if not LResult then
        TPlanningFileNamesFileAgent(FFileNamesFileAgent).ReadDisbenefitDefinitionFileName(ADataFileObjects,AFileNamesObject,nil);

      LResult := TPlanningFileNamesDatabaseAgent(FFileNamesDatabaseAgent).ReadGrowthFactorsFileName(ADataFileObjects,AFileNamesObject);
      if not LResult then
        TPlanningFileNamesFileAgent(FFileNamesFileAgent).ReadGrowthFactorsFileName(ADataFileObjects,AFileNamesObject,nil);

      LResult := TPlanningFileNamesDatabaseAgent(FFileNamesDatabaseAgent).ReadMonthlyWaterRequirementFileName(ADataFileObjects,AFileNamesObject);
      if not LResult then
        TPlanningFileNamesFileAgent(FFileNamesFileAgent).ReadMonthlyWaterRequirementFileName(ADataFileObjects,AFileNamesObject,nil);

      LResult := TPlanningFileNamesDatabaseAgent(FFileNamesDatabaseAgent).ReadHydropowerAllocationFileName(ADataFileObjects,AFileNamesObject);
      if not LResult then
        TPlanningFileNamesFileAgent(FFileNamesFileAgent).ReadHydropowerAllocationFileName(ADataFileObjects,AFileNamesObject,nil);

      LResult := TPlanningFileNamesDatabaseAgent(FFileNamesDatabaseAgent).ReadPumpingChannelControlFileName(ADataFileObjects,AFileNamesObject);
      if not LResult then
        TPlanningFileNamesFileAgent(FFileNamesFileAgent).ReadPumpingChannelControlFileName(ADataFileObjects,AFileNamesObject,nil);

      LResult := TPlanningFileNamesDatabaseAgent(FFileNamesDatabaseAgent).ReadGeneralChannelControlFileName(ADataFileObjects,AFileNamesObject);
      if not LResult then
        TPlanningFileNamesFileAgent(FFileNamesFileAgent).ReadGeneralChannelControlFileName(ADataFileObjects,AFileNamesObject,nil);

      LResult := TPlanningFileNamesDatabaseAgent(FFileNamesDatabaseAgent).ReadReclamationPlantControlFileName(ADataFileObjects,AFileNamesObject);
      if not LResult then
        TPlanningFileNamesFileAgent(FFileNamesFileAgent).ReadReclamationPlantControlFileName(ADataFileObjects,AFileNamesObject,nil);

      LResult := TPlanningFileNamesDatabaseAgent(FFileNamesDatabaseAgent).ReadReturnFlowChannelFileName(ADataFileObjects,AFileNamesObject);
      if not LResult then
        TPlanningFileNamesFileAgent(FFileNamesFileAgent).ReadReturnFlowChannelFileName(ADataFileObjects,AFileNamesObject,nil);


      TPlanningFileNamesDatabaseAgent(FFileNamesDatabaseAgent).ReadChannelSwitchControlFileNames(ADataFileObjects,AFileNamesObject);
      TPlanningFileNamesFileAgent(FFileNamesFileAgent).ReadChannelSwitchControlFileNames(ADataFileObjects,AFileNamesObject,nil);


      LResult := TPlanningFileNamesDatabaseAgent(FFileNamesDatabaseAgent).ReadTariffCalculationFileName(ADataFileObjects,AFileNamesObject);
      if not LResult then
        TPlanningFileNamesFileAgent(FFileNamesFileAgent).ReadTariffCalculationFileName(ADataFileObjects,AFileNamesObject,nil);

      LResult := TPlanningFileNamesDatabaseAgent(FFileNamesDatabaseAgent).ReadAllocationChannelFileName(ADataFileObjects,AFileNamesObject);
      if not LResult then
        TPlanningFileNamesFileAgent(FFileNamesFileAgent).ReadAllocationChannelFileName(ADataFileObjects,AFileNamesObject,nil);

      LResult := TPlanningFileNamesDatabaseAgent(FFileNamesDatabaseAgent).ReadReleaseStructureFileName(ADataFileObjects,AFileNamesObject);
      if not LResult then
        TPlanningFileNamesFileAgent(FFileNamesFileAgent).ReadReleaseStructureFileName(ADataFileObjects,AFileNamesObject,nil);

      LResult := TPlanningFileNamesDatabaseAgent(FFileNamesDatabaseAgent).ReadCurtailFileName(ADataFileObjects,AFileNamesObject);
      if not LResult then
        TPlanningFileNamesFileAgent(FFileNamesFileAgent).ReadCurtailFileName(ADataFileObjects,AFileNamesObject,nil);



      TPlanningFileNamesDatabaseAgent(FFileNamesDatabaseAgent).ReadMineFileNames(ADataFileObjects,AFileNamesObject);
      TPlanningFileNamesFileAgent(FFileNamesFileAgent).ReadMineFileNames(ADataFileObjects,AFileNamesObject,nil);

      TPlanningFileNamesDatabaseAgent(FFileNamesDatabaseAgent).ReadSaltsWashoffFileNames(ADataFileObjects,AFileNamesObject);
      TPlanningFileNamesFileAgent(FFileNamesFileAgent).ReadSaltsWashoffFileNames(ADataFileObjects,AFileNamesObject,nil);

      TPlanningFileNamesDatabaseAgent(FFileNamesDatabaseAgent).ReadMineRainfallFileNames(ADataFileObjects,AFileNamesObject);
      TPlanningFileNamesFileAgent(FFileNamesFileAgent).ReadMineRainfallFileNames(ADataFileObjects,AFileNamesObject,nil);

      Result := True;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TPlanningFileSelectionManager.PopulateTreeView(ATreeView: TTreeView; AFileNamesObject: TModelFileNames): boolean;
const OPNAME = 'TPlanningFileSelectionManager.PopulateTreeView';
var
  LMainNode: TTreeNode;
  LIndex: integer;
  LFileName: TAbstractModelFileName;
  LOnTreeViewNodeChangedEvent  :TTVChangedEvent;
  LOnTreeViewNodeChangingEvent :TTVChangingEvent;
begin
  Result :=  inherited PopulateTreeView(ATreeView,AFileNamesObject);
  try

    if not Assigned(AFileNamesObject) then
      raise Exception.Create('File object parameter is not yet assigned.');

    if not Assigned(ATreeView) then
      raise Exception.Create('Tree View object parameter is not yet assigned.');

    if Result then
    begin
      LOnTreeViewNodeChangedEvent  := ATreeView.OnChange;
      LOnTreeViewNodeChangingEvent := ATreeView.OnChanging;
      try
        ATreeView.OnChange   := nil;
        ATreeView.OnChanging := nil;
        ATreeView.Selected   := nil;

        //AllocationDefinitionFileNames
        LMainNode := ATreeView.Items.AddObject(nil,AFileNamesObject.AllocationDefinitionFileNames.CaptionStr,AFileNamesObject.AllocationDefinitionFileNames);
        for LIndex := 0 to AFileNamesObject.AllocationDefinitionFileNames.Count - 1 do
        begin
           LFileName := AFileNamesObject.AllocationDefinitionFileNames.FileNameObject[LIndex];
           ATreeView.Items.AddChildObject(LMainNode,LFileName.ShortName,LFileName);
        end;

        //ReservoirImplementationFileNames
        LMainNode := ATreeView.Items.AddObject(nil,AFileNamesObject.ReservoirImplementationFileNames.CaptionStr,AFileNamesObject.ReservoirImplementationFileNames);
        for LIndex := 0 to AFileNamesObject.ReservoirImplementationFileNames.Count - 1 do
        begin
           LFileName := AFileNamesObject.ReservoirImplementationFileNames.FileNameObject[LIndex];
           ATreeView.Items.AddChildObject(LMainNode,LFileName.ShortName,LFileName);
        end;

        //DisbenefitDefinitionFileNames
        LMainNode := ATreeView.Items.AddObject(nil,AFileNamesObject.DisbenefitDefinitionFileNames.CaptionStr,AFileNamesObject.DisbenefitDefinitionFileNames);
        for LIndex := 0 to AFileNamesObject.DisbenefitDefinitionFileNames.Count - 1 do
        begin
           LFileName := AFileNamesObject.DisbenefitDefinitionFileNames.FileNameObject[LIndex];
           ATreeView.Items.AddChildObject(LMainNode,LFileName.ShortName,LFileName);
        end;

        //GrowthFactorsFileNames
        LMainNode := ATreeView.Items.AddObject(nil,AFileNamesObject.GrowthFactorsFileNames.CaptionStr,AFileNamesObject.GrowthFactorsFileNames);
        for LIndex := 0 to AFileNamesObject.ReservoirImplementationFileNames.Count - 1 do
        begin
           LFileName := AFileNamesObject.GrowthFactorsFileNames.FileNameObject[LIndex];
           ATreeView.Items.AddChildObject(LMainNode,LFileName.ShortName,LFileName);
        end;

        //MonthlyWaterRequirement
        LMainNode := ATreeView.Items.AddObject(nil,AFileNamesObject.MonthlyWaterRequirementFileNames.CaptionStr,AFileNamesObject.MonthlyWaterRequirementFileNames);
        for LIndex := 0 to AFileNamesObject.MonthlyWaterRequirementFileNames.Count - 1 do
        begin
           LFileName := AFileNamesObject.MonthlyWaterRequirementFileNames.FileNameObject[LIndex];
           ATreeView.Items.AddChildObject(LMainNode,LFileName.ShortName,LFileName);
        end;
        //HydropowerAllocation
        LMainNode := ATreeView.Items.AddObject(nil,AFileNamesObject.HydropowerAllocationFileNames.CaptionStr,AFileNamesObject.HydropowerAllocationFileNames);
        for LIndex := 0 to AFileNamesObject.MonthlyWaterRequirementFileNames.Count - 1 do
        begin
           LFileName := AFileNamesObject.HydropowerAllocationFileNames.FileNameObject[LIndex];
           ATreeView.Items.AddChildObject(LMainNode,LFileName.ShortName,LFileName);
        end;

        //PumpingChannelControlFileNames
        LMainNode := ATreeView.Items.AddObject(nil,AFileNamesObject.PumpingChannelControlFileNames.CaptionStr,AFileNamesObject.PumpingChannelControlFileNames);
        for LIndex := 0 to AFileNamesObject.PumpingChannelControlFileNames.Count - 1 do
        begin
           LFileName := AFileNamesObject.PumpingChannelControlFileNames.FileNameObject[LIndex];
           ATreeView.Items.AddChildObject(LMainNode,LFileName.ShortName,LFileName);
        end;

        //GeneralChannelControlFileNames
        LMainNode := ATreeView.Items.AddObject(nil,AFileNamesObject.GeneralChannelControlFileNames.CaptionStr,AFileNamesObject.GeneralChannelControlFileNames);
        for LIndex := 0 to AFileNamesObject.GeneralChannelControlFileNames.Count - 1 do
        begin
           LFileName := AFileNamesObject.GeneralChannelControlFileNames.FileNameObject[LIndex];
           ATreeView.Items.AddChildObject(LMainNode,LFileName.ShortName,LFileName);
        end;

        //ReclamationPlantControlFileNames
        LMainNode := ATreeView.Items.AddObject(nil,AFileNamesObject.ReclamationPlantControlFileNames.CaptionStr,AFileNamesObject.ReclamationPlantControlFileNames);
        for LIndex := 0 to AFileNamesObject.ReclamationPlantControlFileNames.Count - 1 do
        begin
           LFileName := AFileNamesObject.ReclamationPlantControlFileNames.FileNameObject[LIndex];
           ATreeView.Items.AddChildObject(LMainNode,LFileName.ShortName,LFileName);
        end;
        //ReturnFlowChannel
        LMainNode := ATreeView.Items.AddObject(nil,AFileNamesObject.ReturnFlowChannelFileNames.CaptionStr,AFileNamesObject.ReturnFlowChannelFileNames);
        for LIndex := 0 to AFileNamesObject.ReturnFlowChannelFileNames.Count - 1 do
        begin
           LFileName := AFileNamesObject.ReturnFlowChannelFileNames.FileNameObject[LIndex];
           ATreeView.Items.AddChildObject(LMainNode,LFileName.ShortName,LFileName);
        end;

       //ChannelSwitchControlFileNames
        LMainNode := ATreeView.Items.AddObject(nil,AFileNamesObject.ChannelSwitchControlFileNames.CaptionStr,AFileNamesObject.ChannelSwitchControlFileNames);
        for LIndex := 0 to AFileNamesObject.ChannelSwitchControlFileNames.Count - 1 do
        begin
           LFileName := AFileNamesObject.ChannelSwitchControlFileNames.FileNameObject[LIndex];
           ATreeView.Items.AddChildObject(LMainNode,LFileName.ShortName,LFileName);
        end;
        //TariffCalculationFileNames
        LMainNode := ATreeView.Items.AddObject(nil,AFileNamesObject.TariffCalculationFileNames.CaptionStr,AFileNamesObject.TariffCalculationFileNames);
        for LIndex := 0 to AFileNamesObject.TariffCalculationFileNames.Count - 1 do
        begin
           LFileName := AFileNamesObject.TariffCalculationFileNames.FileNameObject[LIndex];
           ATreeView.Items.AddChildObject(LMainNode,LFileName.ShortName,LFileName);
        end;

        //AllocationChannelFileNames
        LMainNode := ATreeView.Items.AddObject(nil,AFileNamesObject.AllocationChannelFileNames.CaptionStr,AFileNamesObject.AllocationChannelFileNames);
        for LIndex := 0 to AFileNamesObject.AllocationChannelFileNames.Count - 1 do
        begin
           LFileName := AFileNamesObject.AllocationChannelFileNames.FileNameObject[LIndex];
           ATreeView.Items.AddChildObject(LMainNode,LFileName.ShortName,LFileName);
        end;
       //ReleaseStructureFileNames
        LMainNode := ATreeView.Items.AddObject(nil,AFileNamesObject.ReleaseStructureFileNames.CaptionStr,AFileNamesObject.ReleaseStructureFileNames);
        for LIndex := 0 to AFileNamesObject.ReleaseStructureFileNames.Count - 1 do
        begin
           LFileName := AFileNamesObject.ReleaseStructureFileNames.FileNameObject[LIndex];
           ATreeView.Items.AddChildObject(LMainNode,LFileName.ShortName,LFileName);
        end;

         //CurtailFileNames
        LMainNode := ATreeView.Items.AddObject(nil,AFileNamesObject.CurtailFileNames.CaptionStr,AFileNamesObject.CurtailFileNames);
        for LIndex := 0 to AFileNamesObject.CurtailFileNames.Count - 1 do
        begin
           LFileName := AFileNamesObject.CurtailFileNames.FileNameObject[LIndex];
           ATreeView.Items.AddChildObject(LMainNode,LFileName.ShortName,LFileName);
        end;

         // RainfallFilenames
        LMainNode := ATreeView.Items.AddObject(nil,AFileNamesObject.MineRainfallFileNames.CaptionStr,AFileNamesObject.MineRainfallFileNames);
        for LIndex := 0 to AFileNamesObject.MineRainfallFileNames.Count - 1 do
        begin
           LFileName := AFileNamesObject.MineRainfallFileNames.FileNameObject[LIndex];
           ATreeView.Items.AddChildObject(LMainNode,LFileName.ShortName,LFileName);
        end;

        // MineFilenames
        LMainNode := ATreeView.Items.AddObject(nil,AFileNamesObject.MineFileNames.CaptionStr,AFileNamesObject.MineFileNames);
        for LIndex := 0 to AFileNamesObject.MineFileNames.Count - 1 do
        begin
           LFileName := AFileNamesObject.MineFileNames.FileNameObject[LIndex];
           ATreeView.Items.AddChildObject(LMainNode,LFileName.ShortName,LFileName);
        end;


         //SaltsWashoffFileNames
        LMainNode := ATreeView.Items.AddObject(nil,AFileNamesObject.SaltsWashoffFileNames.CaptionStr,AFileNamesObject.SaltsWashoffFileNames);
        for LIndex := 0 to AFileNamesObject.SaltsWashoffFileNames.Count - 1 do
        begin
           LFileName := AFileNamesObject.SaltsWashoffFileNames.FileNameObject[LIndex];
           ATreeView.Items.AddChildObject(LMainNode,LFileName.ShortName,LFileName);
        end;

      finally
        ATreeView.OnChange   := LOnTreeViewNodeChangedEvent;
        ATreeView.OnChanging := LOnTreeViewNodeChangingEvent;
      end;
      Result := True;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;


function TPlanningFileSelectionManager.StudyDataHasChanged(AContext: TChangeContext; AFieldName,AOldValue,ANewValue: string): boolean;
const OPNAME = 'TPlanningFileSelectionManager.StudyDataHasChanged';
var
  LSwitchDef : ISwitchDefinition;
  LSwitchDefList : ISwitchDefinitionsList;
  LFileNamesObject: TModelFileNames;
  LIndex : integer;
  LDataFilePrefix : string;
begin
  Result := inherited StudyDataHasChanged(AContext,AFieldName,AOldValue,ANewValue);
  try
    if (AFieldName = 'SwitchDefFileName') and (AContext = sdccAdd) then
    begin
      LSwitchDefList := (FAppModules.Model.ModelData as IPlanningModelData).SwitchDefinitionsList;
      LDataFilePrefix := Uppercase((FAppModules.Model.ModelData as IYieldModelData).DataFilePaths.DataFilePrefix);
      if (LSwitchDefList <> nil) then
      begin
        for LIndex := 0 to LSwitchDefList.SwitchDefinitionCount-1 do
        begin
          LSwitchDef := LSwitchDefList.SwitchDefinitionByIndex[LIndex];
          if (LSwitchDef <> nil) then
          begin
            LFileNamesObject := TPlanningModelDataObject(FAppModules.Model.ModelData).CastFileNamesObject;
            if (LFileNamesObject <> nil) then
            begin
              if (LFileNamesObject.CastChannelSwitchControlFileNames.FindFile(LDataFilePrefix+LSwitchDef.SwitchDefFileName)  = nil) then
                 LFileNamesObject.AddChannelSwitchControlFileName(LFileNamesObject.CastChannelSwitchControlFileNames.Count+1,
                 LDataFilePrefix+LSwitchDef.SwitchDefFileName,False,0.0,0.0);
            end;
          end;
        end;
      end;

    end
    else
    if (AFieldName = 'SwitchDefFileName') and (AContext = sdccDelete) then
    begin
      LFileNamesObject := TPlanningModelDataObject(FAppModules.Model.ModelData).CastFileNamesObject;
      if (LFileNamesObject <> nil) then
      begin
         if (StrToInt(ANewValue)>=0) then
           LFileNamesObject.DeleteChannelSwitchControlFileName(StrToInt(ANewValue));
      end;
    end;


  except on E: Exception do HandleError(E, OPNAME) end;
end;


end.
