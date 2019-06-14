//
//
//  UNIT      : Contains TYieldFileSelectionManager Class
//  AUTHOR    : Dziedzi Ramulondi
//  DATE      : 2002/12/12
//  COPYRIGHT : Copyright © 2002 DWAF
//
//
unit UYieldFileSelectionManager;

interface

uses
  Classes,
  VCL.ComCtrls,
  UFileNames,
  VoaimsCom_TLB,
  UAbstractObject,
  UDataFileObjects,
  UAbstractFileNamesObject,
  VCL.Dialogs,
  UFileselectionManager,
  UYieldFileNamesFileAgent,
  UYieldFileNamesDatabaseAgent;

type
  TYieldFileSelectionManager = class(TFileSelectionManager)
  protected
    FFileNamesFileAgent     :TYieldFileNamesFileAgent;
    FFileNamesDatabaseAgent :TYieldFileNamesDatabaseAgent;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    procedure CreateFileNamesAgents; virtual;
    function CreateConfigFileNames(ADataFileObjects: TDataFileObjects;AFileNamesObject: TModelFileNames): boolean;
    function CreateOutputFileNames(ADataFileObjects: TDataFileObjects;AFileNamesObject: TModelFileNames): boolean; virtual;

    //function PopulateFileNamesFromFiles(AFileNamesObject :TModelFileNames): boolean;
    //function PopulateFileNamesFromDB(AFileNamesObject :TModelFileNames): boolean;
  public
    function PopulateSpecifiedDemandFileNames(ASpecifiedDemandFeatureList: ISpecifiedDemandFeatureList;AFileNamesObject :TModelFileNames): boolean;
    function PopulateFileNames(ADataFileObjects: TDataFileObjects;AFileNamesObject :TModelFileNames): boolean; override;
    function PopulateTreeView(ATreeView: TTreeView;AFileNamesObject :TModelFileNames): boolean; override;
    //function SaveFileNamesToDB(AFileNamesObject :TModelFileNames): boolean; override;
    function ReselectHydrologyFileNames(AFileNamesObject :TModelFileNames): boolean;
  end;

implementation

uses
  UUtilities,
  SysUtils,
  UYieldModelDataObject,
  UProgressDialog,
  UErrorHandlingOperations;


{ TYieldFileSelectionManager }

procedure TYieldFileSelectionManager.CreateMemberObjects;
const OPNAME = 'TYieldFileSelectionManager.CreateMemberObjects';
begin
  try
    FFileNamesFileAgent     := nil;
    FFileNamesDatabaseAgent := nil;
    CreateFileNamesAgents;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TYieldFileSelectionManager.DestroyMemberObjects;
const OPNAME = 'TYieldFileSelectionManager.DestroyMemberObjects';
begin
  try
    FreeAndNil(FFileNamesFileAgent);
    FreeAndNil(FFileNamesDatabaseAgent);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TYieldFileSelectionManager.CreateFileNamesAgents;
const OPNAME = 'TYieldFileSelectionManager.CreateFileNamesAgents';
begin
  try
    FFileNamesFileAgent     := TYieldFileNamesFileAgent.Create(FAppModules);
    FFileNamesDatabaseAgent := TYieldFileNamesDatabaseAgent.Create(FAppModules);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TYieldFileSelectionManager.PopulateFileNames(ADataFileObjects: TDataFileObjects;AFileNamesObject: TModelFileNames): boolean;
const OPNAME = 'TYieldFileSelectionManager.PopulateFileNames';
var
  //LStopOnFirstError: boolean;
  LProgressDialog: TProgressDialog;
  LResult : boolean;
begin
  inherited PopulateFileNames(ADataFileObjects,AFileNamesObject);
  Result := False;
  try
    if not Assigned(AFileNamesObject) then
      raise Exception.Create('File object parameter is not yet assigned.');

    //LStopOnFirstError := FAppModules.GlobalData.StopOnFirstErr;
    //FAppModules.GlobalData.SetStopOnFirstErr(False);
    //try
      AFileNamesObject.Reset;
      LProgressDialog := TProgressDialog.Create(nil,FAppModules);
      try
        LResult := FFileNamesDatabaseAgent.ReadDirectoryFileContents(ADataFileObjects,AFileNamesObject);
        if not LResult then
          LResult := FFileNamesFileAgent.ReadDirectoryFileContents(ADataFileObjects,AFileNamesObject,LProgressDialog.ShowProgress);

        if LResult  then
        begin
          CreateConfigFileNames(ADataFileObjects,AFileNamesObject);
          CreateOutputFileNames(ADataFileObjects,AFileNamesObject);
        end;

        LResult :=  FFileNamesDatabaseAgent.ReadDirectoryFileName(ADataFileObjects,AFileNamesObject);
        if not LResult then
          FFileNamesFileAgent.ReadDirectoryFileName(ADataFileObjects,AFileNamesObject,LProgressDialog.ShowProgress);

        LResult :=  FFileNamesDatabaseAgent.ReadConfigFileNames(ADataFileObjects,AFileNamesObject);
        if not LResult then
          FFileNamesFileAgent.ReadConfigFileNames(ADataFileObjects,AFileNamesObject,LProgressDialog.ShowProgress);

        LResult :=  FFileNamesDatabaseAgent.ReadParamFileName(ADataFileObjects,AFileNamesObject);
        if not LResult then
          FFileNamesFileAgent.ReadParamFileName(ADataFileObjects,AFileNamesObject,LProgressDialog.ShowProgress);

        LResult :=  FFileNamesDatabaseAgent.ReadAltParamFileName(ADataFileObjects,AFileNamesObject);
        if not LResult then
          FFileNamesFileAgent.ReadAltParamFileName(ADataFileObjects,AFileNamesObject,LProgressDialog.ShowProgress);

        if(Trim(AFileNamesObject.HydrologyFilesPath) = '') then
          FFileNamesFileAgent.ReadHydrologyFilesPath(ADataFileObjects,AFileNamesObject,LProgressDialog.ShowProgress);

        LResult :=  FFileNamesDatabaseAgent.ReadDemandFileNames(ADataFileObjects,AFileNamesObject);
        if not LResult then
          FFileNamesFileAgent.ReadDemandFileNames(ADataFileObjects,AFileNamesObject,LProgressDialog.ShowProgress);

        LResult :=  FFileNamesDatabaseAgent.ReadHydrologyFileNames(ADataFileObjects,AFileNamesObject);
        if not LResult then
          FFileNamesFileAgent.ReadHydrologyFileNames(ADataFileObjects,AFileNamesObject,LProgressDialog.ShowProgress);

        LResult :=  FFileNamesDatabaseAgent.ReadOutputFileNames(ADataFileObjects,AFileNamesObject);
        if not LResult then
          FFileNamesFileAgent.ReadOutputFileNames(ADataFileObjects,AFileNamesObject,LProgressDialog.ShowProgress);

        FFileNamesDatabaseAgent.ReadDamLevelFileNames(ADataFileObjects,AFileNamesObject);


        if (LProgressDialog.ErrorCount > 0)  and Assigned(FAppModules.MainForm()) and
           (not AFileNamesObject.ParamFileNames.FileNameObject[0].SavedInDB) then
        begin
          LProgressDialog.FStartButton.Visible := False;
          LProgressDialog.FStopButton.Visible := False;
          LProgressDialog.ActionProgressBar.Visible := False;
          LProgressDialog.ShowModal;
        end;
        AFileNamesObject.CastParamFileNames.SetFileHintsSet(True);
        AFileNamesObject.CastAltParamFileNames.SetFileHintsSet(True);
        AFileNamesObject.CastDemandFileNames.SetFileHintsSet(True);
        AFileNamesObject.CastHydrologyFileNames.SetFileHintsSet(True);
        AFileNamesObject.CastOutputFileNames.SetFileHintsSet(True);
      finally
        FreeAndNil(LProgressDialog);
      end;
    //finally
    //  FAppModules.GlobalData.SetStopOnFirstErr(LStopOnFirstError);
    //end;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TYieldFileSelectionManager.ReselectHydrologyFileNames(
  AFileNamesObject: TModelFileNames): boolean;
const OPNAME = 'TYieldFileSelectionManager.ReselectHydrologyFileNames';
var
  LDataFileObjects: TDataFileObjects;
  //LStopOnFirstError: boolean;
  LProgressDialog: TProgressDialog;
begin
  Result := False;
  try
    if not Assigned(AFileNamesObject) then
      raise Exception.Create('File object parameter is not yet assigned.');

    if(AFileNamesObject.ParamFileName <> '') then
    begin
      LDataFileObjects := TDataFileObjects.Create;
      LProgressDialog := TProgressDialog.Create(nil,FAppModules);
      try
        FFileNamesFileAgent.ReadHydrologyFileNames(LDataFileObjects,AFileNamesObject,LProgressDialog.ShowProgress);
      finally
        FreeAndNil(LDataFileObjects);
        FreeAndNil(LProgressDialog);
      end;
    end;

    Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TYieldFileSelectionManager.CreateConfigFileNames(ADataFileObjects: TDataFileObjects;
         AFileNamesObject: TModelFileNames): boolean;
const OPNAME = 'TYieldFileSelectionManager.CreateConfigFileNames';
var
  LFileName,
  LFilePrefix: string;
  LCount: Integer;
begin
  Result := False;
  try
    if Assigned(ADataFileObjects) and Assigned(AFileNamesObject) and
       ADataFileObjects.FPathsObject.InputFilesPath.FInitalised then
    begin
      LFilePrefix := Trim(ADataFileObjects.FPathsObject.InputFilesPath.FData);
      LFilePrefix := IncludeTrailingPathDelimiter(LFilePrefix);
      LFilePrefix := LFilePrefix + Trim(ADataFileObjects.FPathsObject.FileNamePrefix.FData);

      for LCount := 0 to 21 do
      begin
        LFileName := LFilePrefix + 'F' + Format('%2.2d',[LCount + 1]) + '.dat';
        AFileNamesObject.UpdateConfigFileName(LCount,LFileName,False,0.0,0.0);
      end;
    end;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TYieldFileSelectionManager.CreateOutputFileNames(ADataFileObjects: TDataFileObjects;
         AFileNamesObject: TModelFileNames): boolean;
const OPNAME = 'TYieldFileSelectionManager.CreateOutputFileNames';
var
  LCurrentFile,
  LFilePrefix: string;
  //LCount: Integer;
begin
  Result := False;
  try
    if Assigned(ADataFileObjects) and Assigned(AFileNamesObject) and
       ADataFileObjects.FPathsObject.OutputFilesPath.FInitalised then
    begin
      LFilePrefix := Trim(ADataFileObjects.FPathsObject.OutputFilesPath.FData);
      LFilePrefix := IncludeTrailingPathDelimiter(LFilePrefix);
      LFilePrefix := LFilePrefix + Trim(ADataFileObjects.FPathsObject.FileNamePrefix.FData);

      LCurrentFile := LFilePrefix + 'Hyd.out';
      if FileExists(LCurrentFile) then
        AFileNamesObject.UpdateOutputFileName(oftHydroPower,LCurrentFile,False,0.0,
        FileLastWriteDate(LCurrentFile))
      else
        AFileNamesObject.UpdateOutputFileName(oftHydroPower,LCurrentFile,False,0.0,0.0);

      LCurrentFile := LFilePrefix + 'Plt.out';
      if FileExists(LCurrentFile) then
        AFileNamesObject.UpdateOutputFileName(oftPlot,LCurrentFile,False,0.0,
        FileLastWriteDate(LCurrentFile))
      else
        AFileNamesObject.UpdateOutputFileName(oftPlot,LCurrentFile,False,0.0,0.0);

      LCurrentFile := LFilePrefix + 'Dat.out';
      if FileExists(LCurrentFile) then
        AFileNamesObject.UpdateOutputFileName(oftData,LCurrentFile,False,0.0,
        FileLastWriteDate(LCurrentFile))
      else
        AFileNamesObject.UpdateOutputFileName(oftData,LCurrentFile,False,0.0,0.0);

      LCurrentFile := LFilePrefix + 'Dbg.out';
      if FileExists(LCurrentFile) then
        AFileNamesObject.UpdateOutputFileName(oftDebug,LCurrentFile,False,0.0,
        FileLastWriteDate(LCurrentFile))
      else
        AFileNamesObject.UpdateOutputFileName(oftDebug,LCurrentFile,False,0.0,0.0);

      LCurrentFile := LFilePrefix + 'Sum.out';
      if FileExists(LCurrentFile) then
        AFileNamesObject.UpdateOutputFileName(oftSum,LCurrentFile,False,0.0,
        FileLastWriteDate(LCurrentFile))
      else
        AFileNamesObject.UpdateOutputFileName(oftSum,LCurrentFile,False,0.0,0.0);

      LCurrentFile := LFilePrefix + 'Yld.out';
      if FileExists(LCurrentFile) then
        AFileNamesObject.UpdateOutputFileName(oftYield,LCurrentFile,False,0.0,
        FileLastWriteDate(LCurrentFile))
      else
        AFileNamesObject.UpdateOutputFileName(oftYield,LCurrentFile,False,0.0,0.0);

      LCurrentFile := LFilePrefix + 'DEM.OUT';
      if FileExists(LCurrentFile) then
        AFileNamesObject.UpdateOutputFileName(oftDemand,LCurrentFile,False,0.0,
        FileLastWriteDate(LCurrentFile))
      else
        AFileNamesObject.UpdateOutputFileName(oftDemand,LCurrentFile,False,0.0,0.0);
      Result := True;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TYieldFileSelectionManager.PopulateTreeView(ATreeView: TTreeView; AFileNamesObject: TModelFileNames): boolean;
const OPNAME = 'TYieldFileSelectionManager.PopulateTreeView';
var
  LMainNode: TTreeNode;
  LIndex: integer;
  LFileName: TAbstractModelFileName;
  LOnTreeViewNodeChangedEvent  :TTVChangedEvent;
  LOnTreeViewNodeChangingEvent :TTVChangingEvent;
begin
  inherited PopulateTreeView(ATreeView,AFileNamesObject);
  Result := False;
  try
    if not Assigned(AFileNamesObject) then
      raise Exception.Create('File object parameter is not yet assigned.');

    if not Assigned(ATreeView) then
      raise Exception.Create('Tree View object parameter is not yet assigned.');

    LOnTreeViewNodeChangedEvent  := ATreeView.OnChange;
    LOnTreeViewNodeChangingEvent := ATreeView.OnChanging;
    try
      ATreeView.OnChange   := nil;
      ATreeView.OnChanging := nil;
      ATreeView.Selected   := nil;

      //DirectoryFileName
      ATreeView.Items.Clear;

      //DirectoryFileName
      //if (FAppModules.StudyArea.ModelVersion <> '7') then
      //begin
        LMainNode := ATreeView.Items.AddObjectFirst(nil,AFileNamesObject.DirectoryFileNames.CaptionStr,AFileNamesObject.DirectoryFileNames);
        for LIndex := 0 to AFileNamesObject.DirectoryFileNames.Count - 1 do
        begin
          LFileName := AFileNamesObject.DirectoryFileNames.FileNameObject[LIndex];
          ATreeView.Items.AddChildObject(LMainNode,LFileName.ShortName,LFileName);
        end;
      //end;

      //ConfigFileNames
      if (FAppModules.StudyArea.ModelVersion = '7') then
      begin
        LMainNode := ATreeView.Items.AddObject(nil,AFileNamesObject.ConfigFileNames.CaptionStr,AFileNamesObject.ConfigFileNames);
        for LIndex := 0 to AFileNamesObject.ConfigFileNames.Count - 1 do
        begin
          LFileName := AFileNamesObject.ConfigFileNames.FileNameObject[LIndex];
          if (LFileName.FileNumber = 22) then
          begin
            if not TYieldModelDataObject(FAppModules.Model.ModelData).ImplementedNetworkFeatures.GroundWaterFeatureImplemented then
              Continue;
          end;
          ATreeView.Items.AddChildObject(LMainNode,LFileName.ShortName,LFileName);
        end;
      end
      else
      if (FAppModules.StudyArea.ModelVersion <> '7') then
      begin
        LMainNode := ATreeView.Items.AddObject(nil,AFileNamesObject.ConfigFileNames.CaptionStr,AFileNamesObject.ConfigFileNames);
        for LIndex := 0 to AFileNamesObject.ConfigFileNames.Count - 1 do
        begin
          LFileName := AFileNamesObject.ConfigFileNames.FileNameObject[LIndex];
          if (LFileName.FileNumber < 17) then
            ATreeView.Items.AddChildObject(LMainNode,LFileName.ShortName,LFileName);
        end;
      end;

      //ParamFileName
      LMainNode := ATreeView.Items.AddObject(nil,AFileNamesObject.ParamFileNames.CaptionStr,AFileNamesObject.ParamFileNames);
      for LIndex := 0 to AFileNamesObject.ParamFileNames.Count - 1 do
      begin
        LFileName := AFileNamesObject.ParamFileNames.FileNameObject[LIndex];
        ATreeView.Items.AddChildObject(LMainNode,LFileName.ShortName,LFileName);
      end;

      //AltParamFileName
      LFileName := AFileNamesObject.AltParamFileNames.FileNameObject[0];
      if(LFileName <> nil) and (LFileName.FileFound or LFileName.SavedInDB) then
      begin
        LMainNode := ATreeView.Items.AddObject(nil,AFileNamesObject.AltParamFileNames.CaptionStr,AFileNamesObject.AltParamFileNames);
        for LIndex := 0 to AFileNamesObject.AltParamFileNames.Count - 1 do
        begin
          LFileName := AFileNamesObject.AltParamFileNames.FileNameObject[LIndex];
          ATreeView.Items.AddChildObject(LMainNode,LFileName.ShortName,LFileName);
        end;
      end;

      //DemandFileNames
      if (AFileNamesObject.DemandFileNames.Count > 0) then
      begin
        LMainNode := ATreeView.Items.AddObject(nil,AFileNamesObject.DemandFileNames.CaptionStr,AFileNamesObject.DemandFileNames);
        for LIndex := 0 to AFileNamesObject.DemandFileNames.Count - 1 do
        begin
           LFileName := AFileNamesObject.DemandFileNames.FileNameObject[LIndex];
           ATreeView.Items.AddChildObject(LMainNode,LFileName.ShortName,LFileName);
        end;
      end;

      //HydrologyFileNames
      if (AFileNamesObject.HydrologyFileNames.Count > 0) then
      begin
        LMainNode := ATreeView.Items.AddObject(nil,AFileNamesObject.HydrologyFileNames.CaptionStr,AFileNamesObject.HydrologyFileNames);
        for LIndex := 0 to AFileNamesObject.HydrologyFileNames.Count - 1 do
        begin
           LFileName := AFileNamesObject.CastHydrologyFileNames[LIndex];
           ATreeView.Items.AddChildObject(LMainNode,LFileName.ShortName,LFileName);
        end;
      end;

      //Dam levels
      if (AFileNamesObject.DamLevelsFileNames.Count > 0) then
      begin
        LMainNode := ATreeView.Items.AddObject(nil,AFileNamesObject.DamLevelsFileNames.CaptionStr,AFileNamesObject.DamLevelsFileNames);
        for LIndex := 0 to AFileNamesObject.DamLevelsFileNames.Count - 1 do
        begin
          LFileName := AFileNamesObject.CastDamLevelsFileNames[LIndex];
          ATreeView.Items.AddChildObject(LMainNode,LFileName.ShortName,LFileName);
        end;
      end;

      //OutputFileNames
      if (AFileNamesObject.OutputFileNames.Count > 0) then
      begin
        LMainNode := ATreeView.Items.AddObject(nil,AFileNamesObject.OutputFileNames.CaptionStr,AFileNamesObject.OutputFileNames);
        for LIndex := 0 to AFileNamesObject.OutputFileNames.Count - 1 do
        begin
           LFileName := AFileNamesObject.CastOutputFileNames[LIndex];
           ATreeView.Items.AddChildObject(LMainNode,LFileName.ShortName,LFileName);
        end;
      end;
    finally
      ATreeView.OnChange   := LOnTreeViewNodeChangedEvent;
      ATreeView.OnChanging := LOnTreeViewNodeChangingEvent;
    end;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

{function TYieldFileSelectionManager.PopulateFileNamesFromDB(AFileNamesObject: TModelFileNames): boolean;
const OPNAME = 'TYieldFileSelectionManager.PopulateTreeView';
var
  LFileNamesDatabaseAgent:TYieldFileNamesDatabaseAgent;
begin
  Result := False;
  try
    try
      LFileNamesDatabaseAgent.ReadYieldFileNamesFromDatabase(AFileNamesObject,nil);
    finally
      LFileNamesDatabaseAgent.Free;
    end;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TYieldFileSelectionManager.PopulateFileNamesFromFiles(AFileNamesObject: TModelFileNames): boolean;
const OPNAME = 'TYieldFileSelectionManager.PopulateFileNamesFromFiles';
var
  LFileNamesFileAgent:TYieldFileNamesFileAgent;
begin
  Result := False;
  try
    try
      LFileNamesFileAgent.ReadYieldFileNamesFromFiles(AFileNamesObject,nil);
    finally
      LFileNamesFileAgent.Free;
    end;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TYieldFileSelectionManager.SaveFileNamesToDB(AFileNamesObject: TModelFileNames): boolean;
const OPNAME = 'TYieldFileSelectionManager.SaveFileNamesToDB';
begin
  Result := inherited SaveFileNamesToDB(AFileNamesObject);
  try
    Result := Assigned(AFileNamesObject);
  except on E: Exception do HandleError(E, OPNAME); end;
end;}

function TYieldFileSelectionManager.PopulateSpecifiedDemandFileNames(ASpecifiedDemandFeatureList: ISpecifiedDemandFeatureList;
         AFileNamesObject: TModelFileNames): boolean;
const OPNAME = 'TYieldFileSelectionManager.PopulateSpecifiedDemandFileNames';
var
  LDemandFeature: ISpecifiedDemandFeature;
  LIndex : integer;
begin
  Result := False;
  try
    if(ASpecifiedDemandFeatureList <> nil) and (AFileNamesObject <> nil) then
    begin
      for LIndex := 0 to ASpecifiedDemandFeatureList.SpecifiedDemandFeatureCount-1 do
      begin
        LDemandFeature := ASpecifiedDemandFeatureList.SpecifiedDemandFeatureByIndex[LIndex];
        if(Trim(LDemandFeature.SpecifiedDemandFileName) <> '') then
        begin
          if(AFileNamesObject.CastDemandFileNames.FindFile(LDemandFeature.SpecifiedDemandFileName) = nil) then
          begin

            AFileNamesObject.AddDemandFileName(AFileNamesObject.CastDemandFileNames.Count+1,
                             LDemandFeature.SpecifiedDemandFileName,False,0.0,0.0);

          end;
        end;
      end;
      Result := True;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

end.
