//
//  UNIT      : Contains TFileReclamationPlantAgent Class
//  AUTHOR    : Dziedzi Ramulondi(Cornastone)
//  DATE      : 03/06/2006
//  COPYRIGHT : Copyright © 2006 DWAF
//
//
unit UFileReclamationPlantAgent;

interface
uses
  Classes,
  UConstants,
  UFileNames,
  UAbstractObject,
  UDataFileObjects,
  UAbstractFileNamesObject,
  UPlanningFileDataObjects,
  UReclamationPlantFileDataObjects,
  UAbstractFileAgent,
  UFilesActionAbstractManager;

Type
  TFileReclamationPlantAgent = class(TAbstractFileAgent)
  public
    function ReadModelDataFromFile(AFileName:TAbstractModelFileName;ADataObject: TDataFileObjects;
             AProgressFunction:TProgressUpdateFuntion): Boolean; override;
    function WriteModelDataToFile(AFilename: TAbstractModelFileName;ADataObject: TDataFileObjects;
             AProgressFunction: TProgressUpdateFuntion): Boolean; override;
  end;

implementation
uses
   SysUtils,
   UUtilities,
   UBasicObjects,
   UFilesActionYieldManager,
   UErrorHandlingOperations;

function TFileReclamationPlantAgent.ReadModelDataFromFile(AFileName: TAbstractModelFileName;
         ADataObject: TDataFileObjects;AProgressFunction: TProgressUpdateFuntion): Boolean;
const OPNAME = 'TFileReclamationPlantAgent.ReadModelDataFromFile';
Var
  LFileData  : TStringList;
  LFileAge   : TDateTime;
  LFileName  : string;
  LMessage   : String;
  LStop      : boolean;
  LStart     : Integer;
  LPlanningFileDataObject : TPlanningFileDataObjects;
  LReclamationPlantObject : TReclamationPlantFileDataObjects;
begin
  Result := False;
  try
    //Check if file exists.
    If not FileExists(AFilename.FileName) then
    begin
      //LMessage := FAppModules.Language.GetString('TFileReclamationPlantAgent.strFileNoExist'); // Update
      //LMessage := Format(LMessage,[ExtractFileName(AFilename.FileName)]);
      //AProgressFunction(LMessage,ptWarning,LStop);
      Result := True;
      Exit;
    end;

    if not Assigned(AProgressFunction) then
      AProgressFunction := DummyShowProgress;

    LMessage := FAppModules.Language.GetString('TFileReclamationPlantAgent.strReadingStarted'); // Update
    LMessage := Format(LMessage,[ExtractFileName(AFilename.FileName)]);
    AProgressFunction(LMessage,ptNone,LStop);

    if not Assigned(ADataObject) then
      raise Exception.Create('File object parameter is not yet assigned.');

    if (AFilename.FileName = '') then
      raise Exception.Create('File name parameter is blank.');

    if not FilePathIsDosCompatible(FAppModules,AFilename.FileName) then
    begin
      LMessage := FAppModules.Language.GetString('TFileReclamationPlantAgent.strPathNotDosCompatible');
      LMessage := Format(LMessage,[AFilename.FileName]);
      AProgressFunction(LMessage,ptError,LStop);
    end;

    //TFileNameObject(AFilename).FileDate := FileDateToDateTime(FileAge(AFileName.FileName));
    LFileName := AFileName.FileName;
    FileAge(LFileName,LFileAge);
    TFileNameObject(AFilename).FileDate := LFileAge;

    LPlanningFileDataObject    := TPlanningFileDataObjects(ADataObject);
    LReclamationPlantObject := LPlanningFileDataObject.ReclamationPlantFileDataObjects;
    if not LReclamationPlantObject.Initialise then
      Exit;

    LFileData := TStringList.Create;
    try
      LFileData.LoadFromFile(AFilename.FileName);
      for LStart := 0 to LFileData.Count -1 do
        LReclamationPlantObject.HDextraLines.Add(LFileData[LStart]);

      LMessage := FAppModules.Language.GetString('TFileReclamationPlantAgent.strReadingCompleted');
      LMessage := Format(LMessage,[ExtractFileName(AFilename.FileName)]);
      AProgressFunction(LMessage,ptNone,LStop);
      Result := True;
    finally
      LFileData.Free;
    end;
  except on E:Exception do  HandleError(E,OPNAME) end;
end;

function TFileReclamationPlantAgent.WriteModelDataToFile(AFilename: TAbstractModelFileName;
         ADataObject: TDataFileObjects;AProgressFunction: TProgressUpdateFuntion): Boolean;
const OPNAME = 'TFileReclamationPlantAgent.WriteModelDataToFile';
Var
  LDataFile  : TStringList;
  LMessage   : String;
  LCount     : Integer;
  LStop      : boolean;
  LPlanningFileDataObject  : TPlanningFileDataObjects;
  LReclamationPlantObject  : TReclamationPlantFileDataObjects;
begin
  Result := False;
  try
    if FileExists(AFilename.FileName) then
      DeleteFile(AFilename.FileName);

    LPlanningFileDataObject  := TPlanningFileDataObjects(ADataObject);
    LReclamationPlantObject :=  LPlanningFileDataObject.ReclamationPlantFileDataObjects;

    if LReclamationPlantObject.HDextraLines.Count = 0 then
    begin
      //LMessage := FAppModules.Language.GetString('TFileReclamationPlantAgent.strNoDataReturned');
      //LMessage := Format(LMessage,[AFilename.FileName]);
      //AProgressFunction(LMessage,ptError,LStop);
      Result := True;
      Exit;
    end;

    if not Assigned(AProgressFunction) then
      AProgressFunction := DummyShowProgress;

    LMessage := FAppModules.Language.GetString('TFileReclamationPlantAgent.strWritingStarted'); //Update
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
      for LCount := 0 to LReclamationPlantObject.HDextraLines.Count - 1 do
        LDataFile.Add(LReclamationPlantObject.HDextraLines[LCount]);

      LDataFile.SaveToFile(AFilename.FileName);
      SetFileDate(AFileName);

      LMessage := FAppModules.Language.GetString('TFileReclamationPlantAgent.strWritingCompleted'); // Update
      LMessage := Format(LMessage,[ExtractFileName(AFilename.FileName)]);
      AProgressFunction(LMessage,ptNone,LStop);
      Result := True;
    finally
      LDataFile.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;



end.
