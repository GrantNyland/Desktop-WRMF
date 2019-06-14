unit UFilesActionHydrologyManager;

interface
uses
  classes,
  Contnrs,
  UDataFileObjects,
  UAbstractObject,
  UProgressDialog,
  UFileNames,
  UFilesActionAbstractManager,
  UHydrologyFileAgent,
  UDemandFileAgent,
  UDemandDatabaseAgent,
  UHydrologyDatabaseAgent,
  UAbstractYieldModelDataObject,
  UUknownOutputFilesDatabaseAgent,
  UFilePathsAgent,
  UFilePathsDatabaseAgent,
  URunYieldModelAgent,
  UFileParamDatabaseAgent,
  UScenarioDatabaseAgent,
  UAbstractFileNamesObject,
  UFileParamAgent;

//  UAbstractHydrologyModelData;
type

{  TFileActionType = (fatNone,fatValidateAll,fatValidateSingle,fatImportAll,fatExportAll,fatExportSingle,fatImportSingle,
                     fatImportSumOut,fatSaveOutputFiles,fatClearModelData,fatRunModel,fatReadYRCFile,fatReadYRCDB,
                     fatSaveYRCDB);
}
  TFilesActionHydrologyManager = class ( TFilesActionAbstractManager ) //TAbstractHydrologyModelDataAgent
  protected

{    FFileAction : TFileActionType;
    FProgressDialog: TProgressDialog;
    FExtraSteps: integer;
    FDataFileObjects: TDataFileObjects;
}
    FFilePathsAgent: TFilePathsAgent;
    FFilePathsDatabaseAgent:TFilePathsDatabaseAgent;

    FDemandFileAgent: TDemandFileAgent;
    FHydrologyFileAgent: THydrologyFileAgent;
    FRunYieldModelAgent: TRunYieldModelAgent;

    FFileParamDatabaseAgent: TFileParamDatabaseAgent;
    FScenarioDatabaseAgent: TScenarioDatabaseAgent;

    FDemandDatabaseAgent: TDemandDatabaseAgent;
    FHydrologyDatabaseAgent: THydrologyDatabaseAgent;
    FOutputFilesUknownDatabaseAgent: TUknownOutputFilesDatabaseAgent;

    FFileParamAgent: TFileParamAgent;
    
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;

    function CheckModelFilesAreComplete ( AProgressUpdateFuntion: TProgressUpdateFuntion ): boolean; override;
    function AdvanceProgressBar(ASteps: integer): boolean;
    function ExecValidateModelData(AProgressUpdateFuntion:TProgressUpdateFuntion):boolean; override;
    function ExecValidateFiles(AProgressUpdateFuntion:TProgressUpdateFuntion):boolean; override;
    function ExecLoadDataFromFiles(AProgressUpdateFuntion:TProgressUpdateFuntion):boolean; override;
    function ExecSaveDataToFiles(AProgressUpdateFuntion:TProgressUpdateFuntion):boolean; override;
    function ExecLoadDataFromDatabase(AProgressUpdateFuntion:TProgressUpdateFuntion):boolean; override;
    function ExecSaveDataToDatabase(AProgressUpdateFuntion:TProgressUpdateFuntion):boolean; override;
    function ExecClearModelData(AProgressUpdateFuntion:TProgressUpdateFuntion):boolean; override;
    function ExecRunModel(AProgressUpdateFuntion:TProgressUpdateFuntion):boolean; override;
    function GetFileNamesObjectCast: TModelFileNames; override;
//    function UpdateProgress(AResult: boolean): boolean;
//    property FileNamesObject: TModelFileNames read GetFileNamesObjectCast;


  end;

implementation
uses
  SysUtils,
  UUtilities,
  UErrorHandlingOperations;

{ TFilesActionHydrologyManager }

function TFilesActionHydrologyManager.AdvanceProgressBar ( ASteps: integer ): boolean;
const OPNAME = 'TFilesActionHydrologyManager.AdvanceProgressBar';
var
  LCount: integer;
begin

  Result := False;
  try

    for LCount := 1 to ASteps do
      UpdateProgress(True);

    Result := True;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

{function TFilesActionHydrologyManager.UpdateProgress(AResult: boolean): boolean;
const OPNAME = 'TFilesActionHydrologyManager.UpdateProgress';
begin
  Result := False;
  try
    FProgressDialog.UpdateProgressBar;
    if FProgressDialog.Stopped  or ((not AResult) and FAppModules.GlobalData.StopOnFirstErr)then
      Result := False
    else
      Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;
 }

function TFilesActionHydrologyManager.CheckModelFilesAreComplete ( AProgressUpdateFuntion: TProgressUpdateFuntion ): boolean;
const OPNAME = 'TFilesActionHydrologyManager.CheckModelFilesAreComplete';
var
  LIndex: integer;
  LMessage: string;
  LStop: boolean;
begin
  Result := False;
  try
    FExtraSteps := 0;
    if FFileAction in [fatValidateAll,fatImportAll,fatImportSingle,fatRunModel] then
    begin

      if(FFileAction = fatImportSingle) then
      begin
        if (FileNamesObject.ConfigFileNames.SelectedCount > 0) then
          FileNamesObject.ConfigFileNames.SetAllSelected(True);
      end;

      CheckFileModifiedDate(FileNamesObject.DirectoryFileNames.FileNameObject[0],AProgressUpdateFuntion);
      if not FileNamesObject.DirectoryFileNames.FileNameObject[0].FileFound then
      begin
        LMessage := FAppModules.Language.GetString('TFilesActionHydrologyManager.strHydrologyModelFileNoExist');
        LMessage := Format(LMessage,[FileNamesObject.DirectoryFileNames.FileNameObject[0].ShortName]);
        AProgressUpdateFuntion(LMessage,ptError,LStop);
        Exit;
      end;

      CheckFileModifiedDate(FileNamesObject.ParamFileNames.FileNameObject[0],AProgressUpdateFuntion);
      if not FileNamesObject.ParamFileNames.FileNameObject[0].FileFound then
      begin
        LMessage := FAppModules.Language.GetString('TFilesActionHydrologyManager.strHydrologyModelFileNoExist');
        LMessage := Format(LMessage,[FileNamesObject.ParamFileNames.FileNameObject[0].ShortName]);
        AProgressUpdateFuntion(LMessage,ptError,LStop);
        Exit;
      end;

      for LIndex := 0 to 12 do
      begin
        CheckFileModifiedDate(FileNamesObject.ConfigFileNames.FileNameObject[LIndex],AProgressUpdateFuntion);
        if not FileNamesObject.ConfigFileNames.FileNameObject[LIndex].FileFound then
        begin
          LMessage := FAppModules.Language.GetString('TFilesActionHydrologyManager.strHydrologyModelFileNoExist');
          LMessage := Format(LMessage,[FileNamesObject.ConfigFileNames.FileNameObject[LIndex].ShortName]);
          AProgressUpdateFuntion(LMessage,ptError,LStop);
          Exit;
        end;
      end;

      if (FFileAction = fatRunModel) then
      begin
        for LIndex := 0 to FileNamesObject.DemandFileNames.FilesCount -1 do
        begin
          CheckFileModifiedDate(FileNamesObject.DemandFileNames.FileNameObject[LIndex],AProgressUpdateFuntion);
          if not FileNamesObject.DemandFileNames.FileNameObject[LIndex].FileFound then
          begin
            LMessage := FAppModules.Language.GetString('TFilesActionHydrologyManager.strHydrologyModelFileNoExist');
            LMessage := Format(LMessage,[FileNamesObject.ConfigFileNames.FileNameObject[LIndex].ShortName]);
            AProgressUpdateFuntion(LMessage,ptError,LStop);
            Exit;
          end;
        end;

        for LIndex := 0 to FileNamesObject.HydrologyFileNames.FilesCount -1 do
        begin
          CheckFileModifiedDate(FileNamesObject.HydrologyFileNames.FileNameObject[LIndex],AProgressUpdateFuntion);
          if not FileNamesObject.HydrologyFileNames.FileNameObject[LIndex].FileFound then
          begin
            LMessage := FAppModules.Language.GetString('TFilesActionHydrologyManager.strHydrologyModelFileNoExist');
            LMessage := Format(LMessage,[FileNamesObject.ConfigFileNames.FileNameObject[LIndex].ShortName]);
            AProgressUpdateFuntion(LMessage,ptError,LStop);
            Exit;
          end;
        end;
      end;
      Result := True;
    end
    else
      Result := True;

    if Result then
    begin
      // This is used to make the track bar show progress when readind/saving network visualiser data
      if (FFileAction = fatClearModelData) then
        FExtraSteps := 0;
      if (FFileAction = fatExportAll) then
        FExtraSteps := 0;
      if (FFileAction = fatValidateAll) then
        FExtraSteps := 0;
      if (FFileAction = fatImportAll) then
        FExtraSteps := 1;
      if (FFileAction = fatImportSingle) then
      begin
        if ( TAbstractYieldModelDataObject ( FAppModules.Model.ModelData ).FileNamesObject.ConfigFileNames.SelectedCount >= 12 ) then
            {TAbstractHygrologyModelDataObject}
          FExtraSteps := FExtraSteps + 1;
      end;
      if ( FFileAction = fatValidateSingle ) then
      begin
        if ( TAbstractYieldModelDataObject ( FAppModules.Model.ModelData).FileNamesObject.ConfigFileNames.SelectedCount >= 12 ) then
            {TAbstractHygrologyModelDataObject}
          FExtraSteps := FExtraSteps + 1;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;

end;

procedure TFilesActionHydrologyManager.CreateMemberObjects;
const OPNAME = 'TFilesActionHydrologyManager.CreateMemberObjects';
begin
  inherited;
  try
{    FFileAction      := fatNone;
    FProgressDialog  := TProgressDialog.Create ( nil,FAppModules );
    FDataFileObjects := nil;
 }

    FFilePathsAgent                   := TFilePathsAgent.Create ( FAppModules );
    FDemandFileAgent                  := TDemandFileAgent.Create ( FAppModules );
    FHydrologyFileAgent               := THydrologyFileAgent.Create ( FAppModules );

    FDemandDatabaseAgent              := TDemandDatabaseAgent.Create ( FAppModules );
    FHydrologyDatabaseAgent           := THydrologyDatabaseAgent.Create ( FAppModules );
    FOutputFilesUknownDatabaseAgent   := TUknownOutputFilesDatabaseAgent.Create ( FAppModules );
    FRunYieldModelAgent               := TRunYieldModelAgent.Create ( FAppModules );
    FFileParamDatabaseAgent           := TFileParamDatabaseAgent.Create ( FAppModules );

    FFileParamDatabaseAgent           := TFileParamDatabaseAgent.Create ( FAppModules );
    FScenarioDatabaseAgent            := TScenarioDatabaseAgent.Create ( FAppModules );
    FFileParamAgent                   := TFileParamAgent.Create ( FAppModules );
    
  except on E: Exception do HandleError(E, OPNAME) end;

end;

procedure TFilesActionHydrologyManager.DestroyMemberObjects;
const OPNAME = 'TFilesActionHydrologyManager.DestroyMemberObjects';
begin
  try
    FreeAndNil ( FProgressDialog );
    FreeAndNil ( FDemandFileAgent );
    FreeAndNil ( FHydrologyFileAgent );

    FreeAndNil ( FDemandDatabaseAgent );
    FreeAndNil ( FHydrologyDatabaseAgent );
    FreeAndNil ( FOutputFilesUknownDatabaseAgent );
    FreeAndNil ( FRunYieldModelAgent );
    FreeAndNil ( FFileParamDatabaseAgent );

    FreeAndNil ( FFileParamDatabaseAgent );
    FreeAndNil ( FScenarioDatabaseAgent );
    FreeAndNil ( FFileParamAgent );

    inherited  DestroyMemberObjects;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFilesActionHydrologyManager.ExecClearModelData ( AProgressUpdateFuntion: TProgressUpdateFuntion ): boolean;
const OPNAME = 'TFilesActionHydrologyManager.ExecClearModelData';
var
  LCurrentFileNames: TAbstractFileNamesList;
  LCount: integer;
  LResult: boolean;
begin
  Result := False;
  try
    Result := True;

    //Directory file(Wrym.dat)
    LCurrentFileNames := FileNamesObject.DirectoryFileNames;
    if ValidateFileName(LCurrentFileNames.FileNameObject[00],AProgressUpdateFuntion) then
    begin
      LResult := FFilePathsDatabaseAgent.ClearModelDataInDatabase(TFileNameObject(LCurrentFileNames.FileNameObject[00]),AProgressUpdateFuntion);
      Result := Result and LResult;
      if not UpdateProgress(LResult) then Exit;
    end;

     //Parameter file(Param.dat)
    LCurrentFileNames := FileNamesObject.ParamFileNames;
    if ValidateFileName(LCurrentFileNames.FileNameObject[00],AProgressUpdateFuntion) then
    begin
      LResult := FFileParamDatabaseAgent.ClearModelDataInDatabase(TFileNameObject(LCurrentFileNames.FileNameObject[00]),AProgressUpdateFuntion);
      Result := Result and LResult;
      if not UpdateProgress(LResult) then Exit;
    end;

    //Demand files(*.abs,*.dem, ...)
    LCurrentFileNames := FileNamesObject.DemandFileNames;
    FDataFileObjects.FDemandFilesObject.Reset;
    for LCount := 0 to LCurrentFileNames.Count - 1 do
    begin
      if ValidateFileName(LCurrentFileNames.FileNameObject[LCount],AProgressUpdateFuntion) then
      begin
        LResult := FDemandDatabaseAgent.ClearModelDataInDatabase(TFileNameObject(LCurrentFileNames.FileNameObject[LCount]),AProgressUpdateFuntion);
        Result := Result and LResult;
        if not UpdateProgress(LResult) then Exit;
      end;
    end;

    //Hydrology files(*.irr,*.inc, ...)
    LCurrentFileNames := FileNamesObject.HydrologyFileNames;
    FDataFileObjects.FHydrologyFilesObject.Reset;
    for LCount := 0 to LCurrentFileNames.Count - 1 do
    begin
      if ValidateFileName(LCurrentFileNames.FileNameObject[LCount],AProgressUpdateFuntion) then
      begin
        LResult := FHydrologyDatabaseAgent.ClearModelDataInDatabase(TFileNameObject(LCurrentFileNames.FileNameObject[LCount]),AProgressUpdateFuntion);
        Result := Result and LResult;
        if not UpdateProgress(LResult) then Exit;
      end;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TFilesActionHydrologyManager.ExecLoadDataFromDatabase( AProgressUpdateFuntion: TProgressUpdateFuntion): boolean;
const OPNAME = 'TFilesActionHydrologyManager.ExecLoadDataFromDatabase';
var
  LCurrentFileNames: TAbstractFileNamesList;
  LCount: integer;
  LResult: boolean;
begin
  Result := False;
  try
    Result := True;

    //Directory file(Wrym.dat)
    LCurrentFileNames := FileNamesObject.DirectoryFileNames;
    if ValidateFileName(LCurrentFileNames.FileNameObject[00],AProgressUpdateFuntion) then
    begin
      LResult := FFilePathsDatabaseAgent.ReadModelDataFromDatabase(TFileNameObject(LCurrentFileNames.FileNameObject[00]),FDataFileObjects,AProgressUpdateFuntion);
      Result := Result and LResult;
      if not UpdateProgress(LResult) then Exit;
    end;

   //Parameter file(Param.dat)
    LCurrentFileNames := FileNamesObject.ParamFileNames;
    if ValidateFileName(LCurrentFileNames.FileNameObject[00],AProgressUpdateFuntion) then
    begin
      LResult := FFileParamDatabaseAgent.ReadModelDataFromDatabase(TFileNameObject(LCurrentFileNames.FileNameObject[00]),FDataFileObjects,AProgressUpdateFuntion);
      Result := Result and LResult;
      if not UpdateProgress(LResult) then Exit;
    end;

    //Demand files(*.abs,*.dem, ...)
    LCurrentFileNames := FileNamesObject.DemandFileNames;
    FDataFileObjects.FDemandFilesObject.Reset;
    for LCount := 0 to LCurrentFileNames.Count - 1 do
    begin
      if ValidateFileName(LCurrentFileNames.FileNameObject[LCount],AProgressUpdateFuntion) then
      begin
        LResult := FDemandDatabaseAgent.ReadModelDataFromDatabase(TFileNameObject(LCurrentFileNames.FileNameObject[LCount]),
                   FDataFileObjects,AProgressUpdateFuntion);
        Result := Result and LResult;
        if not UpdateProgress(LResult) then Exit;
      end;
    end;

    //Hydrology files(*.irr,*.inc, ...)
    LCurrentFileNames := FileNamesObject.HydrologyFileNames;
    FDataFileObjects.FHydrologyFilesObject.Reset;
    for LCount := 0 to LCurrentFileNames.Count - 1 do
    begin
      if ValidateFileName(LCurrentFileNames.FileNameObject[LCount],AProgressUpdateFuntion) then
      begin
        LResult := FHydrologyDatabaseAgent.ReadModelDataFromDatabase(TFileNameObject(LCurrentFileNames.FileNameObject[LCount]),
                   FDataFileObjects,AProgressUpdateFuntion);
        Result := Result and LResult;
        if not UpdateProgress(LResult) then Exit;
      end;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TFilesActionHydrologyManager.ExecLoadDataFromFiles ( AProgressUpdateFuntion : TProgressUpdateFuntion ): boolean;
const OPNAME = 'TFilesActionHydrologyManager.ExecLoadDataFromFiles';
var
  LCurrentFileNames: TAbstractFileNamesList;
  LCount: integer;
  LResult: boolean;
begin

  Result := False;
  try
    Result := True;

    //Parameter file(Param.dat)
    LCurrentFileNames :=   FileNamesObject.ParamFileNames;
    if ValidateFileName(LCurrentFileNames.FileNameObject[00],AProgressUpdateFuntion) then
    begin
      LResult := FFileParamAgent.ReadModelDataFromFile(LCurrentFileNames.FileNameObject[00],
                 FDataFileObjects,AProgressUpdateFuntion);
      TFileNameObject(LCurrentFileNames.FileNameObject[00]).HintsSet := True;
      Result := Result and LResult;
      if not UpdateProgress(LResult) then Exit;
    end;

    //Demand files(*.abs,*.dem, ...)
    LCurrentFileNames := FileNamesObject.DemandFileNames;
    FDataFileObjects.FDemandFilesObject.Reset;
    for LCount := 0 to LCurrentFileNames.Count - 1 do
    begin
      if ValidateFileName(LCurrentFileNames.FileNameObject[LCount],AProgressUpdateFuntion) then
      begin
        LResult := FDemandFileAgent.ReadModelDataFromFile(LCurrentFileNames.FileNameObject[LCount],
                   FDataFileObjects,AProgressUpdateFuntion);
        Result := Result and LResult;
        if not UpdateProgress(LResult) then Exit;
      end;
    end;

    //Hydrology files(*.irr,*.inc, ...)
    LCurrentFileNames := FileNamesObject.HydrologyFileNames;
    FDataFileObjects.FHydrologyFilesObject.Reset;
    for LCount := 0 to LCurrentFileNames.Count - 1 do
    begin
      if ValidateFileName(LCurrentFileNames.FileNameObject[LCount],AProgressUpdateFuntion) then
      begin
        LResult := FHydrologyFileAgent.ReadModelDataFromFile(LCurrentFileNames.FileNameObject[LCount],
                   FDataFileObjects,AProgressUpdateFuntion);
        Result := Result and LResult;
        if not UpdateProgress(LResult) then Exit;
      end;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;


function TFilesActionHydrologyManager.ExecRunModel ( AProgressUpdateFuntion: TProgressUpdateFuntion ): boolean;
const OPNAME = 'TFilesActionHydrologyManager.ExecRunModel';
var
  LCurrentFileNames: TAbstractFileNamesList;
begin
  Result := False;
  try
    //Read Directory file(Wrym.dat) (1)
    LCurrentFileNames := FileNamesObject.DirectoryFileNames;
    Result := ValidateFileName(LCurrentFileNames.FileNameObject[00],AProgressUpdateFuntion);
    Result := Result and  FFilePathsAgent.ReadModelDataFromFile(LCurrentFileNames.FileNameObject[00],
                 FDataFileObjects,AProgressUpdateFuntion);
    if not UpdateProgress(Result) then Exit;

    Result := Result and FRunYieldModelAgent.CheckInputFiles(FDataFileObjects,AProgressUpdateFuntion);
    if not UpdateProgress(Result) then Exit;

    FProgressDialog.FStopButton.Enabled := False;
    Result := Result and FRunYieldModelAgent.RunModel(FDataFileObjects,AProgressUpdateFuntion,FProgressDialog.Handle);
    FProgressDialog.FStopButton.Enabled := True;
    if not UpdateProgress(Result) then Exit;

    Result := Result and FRunYieldModelAgent.CheckOutputFiles(FDataFileObjects,AProgressUpdateFuntion);
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;

end;

function TFilesActionHydrologyManager.ExecSaveDataToDatabase (AProgressUpdateFuntion: TProgressUpdateFuntion): boolean;
const OPNAME = 'TFilesActionHydrologyManager.ExecSaveDataToDatabase';
var
  LCurrentFileNames: TAbstractFileNamesList;
  LCount: integer;
  LResult: boolean;
begin
  Result := False;
  try

    Result := True;
    //Directory file(Wrym.dat)
    LCurrentFileNames := FileNamesObject.DirectoryFileNames;
    if ValidateFileName(LCurrentFileNames.FileNameObject[00],AProgressUpdateFuntion) then
    begin
      LResult := FFilePathsDatabaseAgent.WriteModelDataToDatabase(TFileNameObject(LCurrentFileNames.FileNameObject[00]),
                 FDataFileObjects,AProgressUpdateFuntion);
      Result := Result and LResult;
      if not UpdateProgress(LResult) then Exit;
    end;
     //Parameter file(Param.dat)
    LCurrentFileNames := FileNamesObject.ParamFileNames;
    if ValidateFileName(LCurrentFileNames.FileNameObject[00],AProgressUpdateFuntion) then
    begin
      LResult := FFileParamDatabaseAgent.WriteModelDataToDatabase(TFileNameObject(LCurrentFileNames.FileNameObject[00]),
                 FDataFileObjects,AProgressUpdateFuntion);
      Result := Result and LResult;
      if not UpdateProgress(LResult) then Exit;
    end;

    //Demand files(*.abs,*.dem, ...)
    LCurrentFileNames := FileNamesObject.DemandFileNames;
    for LCount := 0 to LCurrentFileNames.Count - 1 do
    begin
      if ValidateFileName(TFileNameObject(LCurrentFileNames.FileNameObject[LCount]),AProgressUpdateFuntion) then
      begin
        LResult := FDemandDatabaseAgent.WriteModelDataToDatabase(TFileNameObject(LCurrentFileNames.FileNameObject[LCount]),
                   FDataFileObjects,AProgressUpdateFuntion);
        Result := Result and LResult;
        if not UpdateProgress(LResult) then Exit;
      end;
    end;

    //Hydrology files(*.irr,*.inc, ...)
    LCurrentFileNames := FileNamesObject.HydrologyFileNames;
    for LCount := 0 to LCurrentFileNames.Count - 1 do
    begin
      if ValidateFileName(LCurrentFileNames.FileNameObject[LCount],AProgressUpdateFuntion) then
      begin
        LResult := FHydrologyDatabaseAgent.WriteModelDataToDatabase(TFileNameObject(LCurrentFileNames.FileNameObject[LCount]),
                   FDataFileObjects,AProgressUpdateFuntion);
        Result := Result and LResult;
        if not UpdateProgress(LResult) then Exit;
      end;
    end;

    if Result then
      Result := FScenarioDatabaseAgent.SetFilesLoaded(True);

    if Result then
       FProgressDialog.KeepStartBtnDisabled := True;

    if Result then
      FProgressDialog.ActionProgressBar.Position := FProgressDialog.ActionProgressBar.Max;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFilesActionHydrologyManager.ExecSaveDataToFiles ( AProgressUpdateFuntion: TProgressUpdateFuntion): boolean;
const OPNAME = 'TFilesActionHydrologyManager.ExecSaveDataToFiles';
var
  LCurrentFileNames: TAbstractFileNamesList;
  LCount: integer;
  LResult: boolean;
begin

  Result := False;
  try
    Result := True;

    //Directory file(Wrym.dat)
    LCurrentFileNames :=  FileNamesObject.DirectoryFileNames;
    if ValidateFileName(LCurrentFileNames.FileNameObject[00],AProgressUpdateFuntion) then
    begin
      LResult := FFilePathsAgent.WriteModelDataToFile(LCurrentFileNames.FileNameObject[00],
                 FDataFileObjects, AProgressUpdateFuntion);
      TFileNameObject(LCurrentFileNames.FileNameObject[00]).HintsSet := False;
      Result := Result and LResult;
      if not UpdateProgress(LResult) then Exit;
    end;

    //Parameter file(Param.dat)
    LCurrentFileNames := FileNamesObject.ParamFileNames;
    if ValidateFileName(LCurrentFileNames.FileNameObject[00],AProgressUpdateFuntion) then
    begin
      LResult := FFileParamAgent.WriteModelDataToFile(LCurrentFileNames.FileNameObject[00],
                 FDataFileObjects,AProgressUpdateFuntion);
      TFileNameObject(LCurrentFileNames.FileNameObject[00]).HintsSet := False;
      Result := Result and LResult;
      if not UpdateProgress(LResult) then Exit;
    end;

    //Demand files(*.abs,*.dem, ...)
    LCurrentFileNames :=FileNamesObject.DemandFileNames;
    for LCount := 0 to LCurrentFileNames.Count - 1 do
    begin
      if ValidateFileName(LCurrentFileNames.FileNameObject[LCount],AProgressUpdateFuntion) then
      begin
        LResult := FDemandFileAgent.WriteModelDataToFile(LCurrentFileNames.FileNameObject[LCount],
                   FDataFileObjects,AProgressUpdateFuntion);
        Result := Result and LResult;
        if not UpdateProgress(LResult) then Exit;
      end;
    end;

    //Hydrology files(*.irr,*.inc, ...)
    LCurrentFileNames := FileNamesObject.HydrologyFileNames;
    for LCount := 0 to LCurrentFileNames.Count - 1 do
    begin
      if ValidateFileName(LCurrentFileNames.FileNameObject[LCount],AProgressUpdateFuntion) then
      begin
        LResult := FHydrologyFileAgent.WriteModelDataToFile(LCurrentFileNames.FileNameObject[LCount],
                   FDataFileObjects,AProgressUpdateFuntion);
        Result := Result and LResult;
        if not UpdateProgress(LResult) then Exit;
      end;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TFilesActionHydrologyManager.ExecValidateFiles ( AProgressUpdateFuntion: TProgressUpdateFuntion ): boolean;
const OPNAME = 'TFilesActionHydrologyManager.ExecValidateFiles';
begin
  Result := False;
  try
    Result := ExecLoadDataFromFiles(AProgressUpdateFuntion);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFilesActionHydrologyManager.ExecValidateModelData ( AProgressUpdateFuntion: TProgressUpdateFuntion ): boolean;
const OPNAME = 'TFilesActionHydrologyManager.ExecValidateModelData';
var
  LValidationErrors: TStringList;
  LCount: integer;
  LStop: boolean;
begin
  Result := False;
  try
    if not Assigned(AProgressUpdateFuntion) then
      AProgressUpdateFuntion := DummyShowProgress;
    LValidationErrors := TStringList.Create;
    try
      AProgressUpdateFuntion('Started validating model data',ptNone,LStop);
      Result := FAppModules.Model.ValidateBusinessRule({Ord(brvAll)} 0,[],LValidationErrors);
      if not Result then
      begin
         for LCount := 0 to LValidationErrors.Count -1 do
           AProgressUpdateFuntion(LValidationErrors[LCount],ptError,LStop);
      end;
      AProgressUpdateFuntion('Completed validating model data',ptNone,LStop);
    finally
      LValidationErrors.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFilesActionHydrologyManager.GetFileNamesObjectCast: TModelFileNames;
const OPNAME = 'TFilesActionHydrologyManager.GetFileNamesObjectCast';
begin
  Result := nil;
  try
    Result := TModelFileNames ( ModelData.FileNamesObject );
  except on E: Exception do HandleError( E, OPNAME ) end;
end;

end.
