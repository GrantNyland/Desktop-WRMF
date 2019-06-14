//
//  UNIT      : Contains TFileHydroPowerAllocationControlAgent Class
//  AUTHOR    : Kholofelo Malokane(Cornastone)
//  DATE      : 03/06/2006
//  COPYRIGHT : Copyright © 2006 DWAF
//
//

unit UFileHydroPowerAllocationControlAgent;

interface
uses
  Classes, sysutils,
  UConstants,
  UFileNames,
  UAbstractObject,
  UDataFileObjects,
  UAbstractFileNamesObject,
  UPlanningFileDataObjects,
  UHydroPowerAllocationControlFileDataObject,
  UAbstractFileAgent,
  UFilesActionAbstractManager;

Type
  TFileHydroPowerAllocationControlAgent = class(TAbstractFileAgent)
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
   UErrorHandlingOperations, UBasicObjects,
   UFilesActionYieldManager;

function TFileHydroPowerAllocationControlAgent.ReadModelDataFromFile(AFileName: TAbstractModelFileName;
         ADataObject: TDataFileObjects;AProgressFunction: TProgressUpdateFuntion): Boolean;
const OPNAME = 'TFileHydroPowerAllocationControlAgent.ReadModelDataFromFile';
Var
  LFileData  : TStringList;
  LFileAge   : TDateTime;
  LFileName  : string;
  LMessage   : String;
  LStop      : boolean;
  LStart     : Integer;
  LHydroAllocation : THydroPowerAllocationControlFileDataObject;
  LPlanningFileDataObject : TPlanningFileDataObjects;
begin
  Result := False;
  try
    //Check if file exists.
    If not FileExists(AFilename.FileName) then
    begin
      //LMessage := FAppModules.Language.GetString('TFileHydroPowerAllocationControlAgent.strFileNoExist'); // Update
      //LMessage := Format(LMessage,[ExtractFileName(AFilename.FileName)]);
      //AProgressFunction(LMessage,ptWarning,LStop);
      Result := True;
      Exit;
    end;

    if not Assigned(AProgressFunction) then
      AProgressFunction := DummyShowProgress;

    LMessage := FAppModules.Language.GetString('TFileHydroPowerAllocationControlAgent.strReadingStarted');
    LMessage := Format(LMessage,[ExtractFileName(AFilename.FileName)]);
    AProgressFunction(LMessage,ptNone,LStop);

    if not Assigned(ADataObject) then
      raise Exception.Create('File object parameter is not yet assigned.');

    if (AFilename.FileName = '') then
      raise Exception.Create('File name parameter is blank.');

    if not FilePathIsDosCompatible(FAppModules,AFilename.FileName) then
    begin
      LMessage := FAppModules.Language.GetString('TFileHydroPowerAllocationControlAgent.strPathNotDosCompatible');
      LMessage := Format(LMessage,[AFilename.FileName]);
      AProgressFunction(LMessage,ptError,LStop);
    end;

    //TFileNameObject(AFilename).FileDate := FileDateToDateTime(FileAge(AFileName.FileName));
    LFileName := AFileName.FileName;
    FileAge(LFileName,LFileAge);
    TFileNameObject(AFilename).FileDate := LFileAge;

    LPlanningFileDataObject    := TPlanningFileDataObjects(ADataObject);
    LHydroAllocation := LPlanningFileDataObject.HydroAllocationDataObject;
    if not LHydroAllocation.Initialise then
      Exit;

    LFileData := TStringList.Create;
    try
      LFileData.LoadFromFile(AFilename.FileName);
      for LStart := 0 to LFileData.Count -1 do
      begin
        LHydroAllocation.HDextraLines.Add(LFileData[LStart]);
      end;

      LMessage := FAppModules.Language.GetString('TFileHydroPowerAllocationControlAgent.strReadingCompleted');
      LMessage := Format(LMessage,[ExtractFileName(AFilename.FileName)]);
      AProgressFunction(LMessage,ptNone,LStop);
      Result := True;
    finally
      LFileData.Free;
    end;
  except on E:Exception do  HandleError(E,OPNAME) end;
end;

function TFileHydroPowerAllocationControlAgent.WriteModelDataToFile(AFilename: TAbstractModelFileName;
         ADataObject: TDataFileObjects;AProgressFunction: TProgressUpdateFuntion): Boolean;
const OPNAME = 'TFileHydroPowerAllocationControlAgent.WriteModelDataToFile';
Var
  LStop      : boolean;
  LDataFile  : TStringList;
  LMessage   : String;
  LCount     : Integer;
  LPlanningFileDataObject  : TPlanningFileDataObjects;
  LHydroAllocation : THydroPowerAllocationControlFileDataObject;
begin
  Result := False;
  try
    if FileExists(AFilename.FileName) then
      DeleteFile(AFilename.FileName);

    LPlanningFileDataObject  := TPlanningFileDataObjects(ADataObject);
    LHydroAllocation :=  LPlanningFileDataObject.HydroAllocationDataObject;

    if LHydroAllocation.HDextraLines.Count = 0 then
    begin
      //LMessage := FAppModules.Language.GetString('TFileHydroPowerAllocationControlAgent.strNoDataReturned');
      //LMessage := Format(LMessage,[AFilename.FileName]);
      //AProgressFunction(LMessage,ptError,LStop);
      Result := True;
      Exit;
    end;

    if not Assigned(AProgressFunction) then
      AProgressFunction := DummyShowProgress;

    LMessage := FAppModules.Language.GetString('TFileHydroPowerAllocationControlAgent.strWritingStarted'); //Update
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
      for LCount := 0 to LHydroAllocation.HDextraLines.Count - 1 do
        LDataFile.Add(LHydroAllocation.HDextraLines[LCount]);

      LDataFile.SaveToFile(AFilename.FileName);
      SetFileDate(AFileName);

      LMessage := FAppModules.Language.GetString('TFileHydroPowerAllocationControlAgent.strWritingCompleted');
      LMessage := Format(LMessage,[ExtractFileName(AFilename.FileName)]);
      AProgressFunction(LMessage,ptNone,LStop);
      Result := True;
    finally
      LDataFile.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.

