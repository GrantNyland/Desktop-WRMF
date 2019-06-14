//
//  UNIT      : Contains TFileMonthlyWaterRequirementAgent Class
//  AUTHOR    : Kholofelo Malokane(Cornastone)
//  DATE      : 03/06/2006
//  COPYRIGHT : Copyright © 2006 DWAF
//
//
unit UFileMonthlyWaterRequirementAgent;

interface
uses
  Classes, sysutils,
  UConstants,
  UFileNames,
  UAbstractObject,
  UDataFileObjects,
  UAbstractFileNamesObject,
  UPlanningFileDataObjects,
  UMonthlyWaterRequirementFileDataObjects,
  UAbstractFileAgent,
  UFilesActionAbstractManager;

Type
  TFileMonthlyWaterRequirementAgent = class(TAbstractFileAgent)
  public
    function ReadModelDataFromFile(AFileName:TAbstractModelFileName;ADataObject: TDataFileObjects;
             AProgressFunction:TProgressUpdateFuntion): Boolean; override;
    function WriteModelDataToFile(AFilename: TAbstractModelFileName;ADataObject: TDataFileObjects;
             AProgressFunction: TProgressUpdateFuntion): Boolean; override;
  end;

implementation
uses
   UUtilities,
   UFilesLineTypeObject,
   UErrorHandlingOperations,
   UBasicObjects,
   UFilesActionYieldManager;

function TFileMonthlyWaterRequirementAgent.ReadModelDataFromFile(AFileName: TAbstractModelFileName;
         ADataObject: TDataFileObjects;AProgressFunction: TProgressUpdateFuntion): Boolean;
const OPNAME = 'TFileMonthlyWaterRequirementAgent.ReadModelDataFromFile';
Var
  LFileData  : TStringList;
  LMessage   : String;
  LFileAge   : TDateTime;
  LFileName  : string;
  LStop      : boolean;
  LStart     : Integer;
  LPlanningFileDataObject : TPlanningFileDataObjects;
  LWaterRequirementObject : TMonthlyWaterRequirementFileDataObjects;
begin
  Result := False;
  try
    //Check if file exists.
    If not FileExists(AFilename.FileName) then
    begin
      //LMessage := FAppModules.Language.GetString('TFileMonthlyWaterRequirementAgent.strFileNoExist'); // Update
      //LMessage := Format(LMessage,[ExtractFileName(AFilename.FileName)]);
      //AProgressFunction(LMessage,ptWarning,LStop);
      Result := True;
      Exit;
    end;

    if not Assigned(AProgressFunction) then
      AProgressFunction := DummyShowProgress;

    LMessage := FAppModules.Language.GetString('TFileMonthlyWaterRequirementAgent.strReadingStarted'); // Update
    LMessage := Format(LMessage,[ExtractFileName(AFilename.FileName)]);
    AProgressFunction(LMessage,ptNone,LStop);

    if not Assigned(ADataObject) then
      raise Exception.Create('File object parameter is not yet assigned.');

    if (AFilename.FileName = '') then
      raise Exception.Create('File name parameter is blank.');

    if not FilePathIsDosCompatible(FAppModules,AFilename.FileName) then
    begin
      LMessage := FAppModules.Language.GetString('TFileMonthlyWaterRequirementAgent.strPathNotDosCompatible');
      LMessage := Format(LMessage,[AFilename.FileName]);
      AProgressFunction(LMessage,ptError,LStop);
    end;

    //TFileNameObject(AFilename).FileDate := FileDateToDateTime(FileAge(AFileName.FileName));
    LFileName := AFileName.FileName;
    FileAge(LFileName,LFileAge);
    TFileNameObject(AFilename).FileDate := LFileAge;

    LPlanningFileDataObject    := TPlanningFileDataObjects(ADataObject);
    LWaterRequirementObject := LPlanningFileDataObject.MonthlyWaterRequirementFileDataObjects;
    if not LWaterRequirementObject.Initialise then
      Exit;

    LFileData := TStringList.Create;
    try
      LFileData.LoadFromFile(AFilename.FileName);
      for LStart := 0 to LFileData.Count -1 do
        LWaterRequirementObject.HDextraLines.Add(LFileData[LStart]);

      LMessage := FAppModules.Language.GetString('TFileMonthlyWaterRequirementAgent.strReadingCompleted');
      LMessage := Format(LMessage,[ExtractFileName(AFilename.FileName)]);
      AProgressFunction(LMessage,ptNone,LStop);
      Result := True;
    finally
      LFileData.Free;
    end;
  except on E:Exception do  HandleError(E,OPNAME) end;
end;

function TFileMonthlyWaterRequirementAgent.WriteModelDataToFile(AFilename: TAbstractModelFileName;
         ADataObject: TDataFileObjects;AProgressFunction: TProgressUpdateFuntion): Boolean;
const OPNAME = 'TFileMonthlyWaterRequirementAgent.WriteModelDataToFile';
Var
  LDataFile  : TStringList;
  LMessage   : String;
  LCount     : Integer;
  LStop      : boolean;
  LPlanningFileDataObject  : TPlanningFileDataObjects;
  LWaterRequirementObject  : TMonthlyWaterRequirementFileDataObjects;
begin
  Result := False;
  try
    if FileExists(AFilename.FileName) then
      DeleteFile(AFilename.FileName);

    LPlanningFileDataObject  := TPlanningFileDataObjects(ADataObject);
    LWaterRequirementObject :=  LPlanningFileDataObject.MonthlyWaterRequirementFileDataObjects;

    if LWaterRequirementObject.HDextraLines.Count = 0 then
    begin
      //LMessage := FAppModules.Language.GetString('TFileMonthlyWaterRequirementAgent.strNoDataReturned');
      //LMessage := Format(LMessage,[AFilename.FileName]);
      //AProgressFunction(LMessage,ptError,LStop);
      Result := True;
      Exit;
    end;

    if not Assigned(AProgressFunction) then
      AProgressFunction := DummyShowProgress;

    LMessage := FAppModules.Language.GetString('TFileMonthlyWaterRequirementAgent.strWritingStarted'); //Update
    LMessage := Format(LMessage,[ExtractFileName(AFilename.FileName)]);
    AProgressFunction(LMessage,ptNone,LStop);

    if not Assigned(ADataObject) then
      raise Exception.Create('File object parameter is not yet assigned.');

    if (AFilename.FileName = '') then
      raise Exception.Create('File name parameter is blank.');

    if not FilePathIsDosCompatible(FAppModules,AFilename.FileName) then
    begin
      LMessage := FAppModules.Language.GetString('TAbstractFileAgent.strPathNotDosCompatible');
      LMessage := Format(LMessage,[AFilename.FileName]);
      AProgressFunction(LMessage,ptError,LStop);
      if FAppModules.GlobalData.StopOnFirstErr then Exit ;
    end;

    LDataFile:= TStringList.Create;
    try
      for LCount := 0 to LWaterRequirementObject.HDextraLines.Count - 1 do
        LDataFile.Add(LWaterRequirementObject.HDextraLines[LCount]);

      LDataFile.SaveToFile(AFilename.FileName);
      SetFileDate(AFileName);

      LMessage := FAppModules.Language.GetString('TFileMonthlyWaterRequirementAgent.strWritingCompleted'); // Update
      LMessage := Format(LMessage,[ExtractFileName(AFilename.FileName)]);
      AProgressFunction(LMessage,ptNone,LStop);
      Result := True;
    finally
      LDataFile.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;



end.
