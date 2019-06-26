//
//  UNIT      : Contains TFileAllocationChannelControlAgent Class
//  AUTHOR    : Kholofelo Malokane(Cornastone)
//  DATE      : 03/06/2006
//  COPYRIGHT : Copyright © 2006 DWAF
//
//
unit UFileAllocationChannelControlAgent;

interface
uses
  Classes, sysutils,
  UConstants,
  UFileNames,
  UAbstractObject,
  UDataFileObjects,
  UAbstractFileNamesObject,
  UPlanningFileDataObjects,
  UAllocationChannelControlFileDataObject,
  UAbstractFileAgent,
  UFilesActionAbstractManager;

Type
  TFileAllocationChannelControlAgent = class(TAbstractFileAgent)
  public
    function ReadModelDataFromFile(AFileName:TAbstractModelFileName;ADataObject: TDataFileObjects;
             AProgressFunction:TProgressUpdateFuntion): Boolean; override;
    function WriteModelDataToFile(AFilename: TAbstractModelFileName;ADataObject: TDataFileObjects;
             AProgressFunction: TProgressUpdateFuntion): Boolean; override;
  end;

implementation
uses
   UUtilities,
   UBasicObjects,
   UErrorHandlingOperations,
   UFilesActionYieldManager;

function TFileAllocationChannelControlAgent.ReadModelDataFromFile(AFileName: TAbstractModelFileName;
         ADataObject: TDataFileObjects;AProgressFunction: TProgressUpdateFuntion): Boolean;
const OPNAME = 'TFileAllocationChannelControlAgent.ReadModelDataFromFile';
Var
  LFileData  : TStringList;
  LFileAge   : TDateTime;
  LFileName  : string;
  LMessage   : String;
  LStop      : boolean;
  LStart     : Integer;
  LPlanningFileDataObject : TPlanningFileDataObjects;
  LAllocationChannel : TAllocationChannelControlFileDataObject;
begin
  Result := False;
  try
    //Check if file exists.
    If not FileExists(AFilename.FileName) then
    begin
      //LMessage := FAppModules.Language.GetString('TFileAllocationChannelControlAgent.strFileNoExist'); // Update
      //LMessage := Format(LMessage,[ExtractFileName(AFilename.FileName)]);
      //AProgressFunction(LMessage,ptWarning,LStop);
      Result := True;
      Exit;
    end;

    if not Assigned(AProgressFunction) then
      AProgressFunction := DummyShowProgress;

    LMessage := FAppModules.Language.GetString('TFileAllocationChannelControlAgent.strReadingStarted'); // Update
    LMessage := Format(LMessage,[ExtractFileName(AFilename.FileName)]);
    AProgressFunction(LMessage,ptNone,LStop);

    if not Assigned(ADataObject) then
      raise Exception.Create('File object parameter is not yet assigned.');

    if (AFilename.FileName = '') then
      raise Exception.Create('File name parameter is blank.');

    if not FilePathIsDosCompatible(FAppModules,AFilename.FileName) then
    begin
      LMessage := FAppModules.Language.GetString('TFileAllocationChannelControlAgent.strPathNotDosCompatible');
      LMessage := Format(LMessage,[AFilename.FileName]);
      AProgressFunction(LMessage,ptError,LStop);
    end;

    //TFileNameObject(AFilename).FileDate := FileDateToDateTime(FileAge(AFileName.FileName));
    LFileName := AFileName.FileName;
    FileAge(LFileName,LFileAge);
    TFileNameObject(AFilename).FileDate := LFileAge;

    LPlanningFileDataObject    := TPlanningFileDataObjects(ADataObject);
    LAllocationChannel := LPlanningFileDataObject.AllocationChannelObject;
    if not LAllocationChannel.Initialise then
      Exit;

    LFileData := TStringList.Create;
    try
      LFileData.LoadFromFile(AFilename.FileName);
      for LStart := 0 to LFileData.Count -1 do
        LAllocationChannel.HDextraLines.Add(LFileData[LStart]);

      LMessage := FAppModules.Language.GetString('TFileAllocationChannelControlAgent.strReadingCompleted');
      LMessage := Format(LMessage,[ExtractFileName(AFilename.FileName)]);
      AProgressFunction(LMessage,ptNone,LStop);
      Result := True;
    finally
      LFileData.Free;
    end;
  except on E:Exception do  HandleError(E,OPNAME) end;
end;

function TFileAllocationChannelControlAgent.WriteModelDataToFile(AFilename: TAbstractModelFileName;
         ADataObject: TDataFileObjects;AProgressFunction: TProgressUpdateFuntion): Boolean;
const OPNAME = 'TFileAllocationChannelControlAgent.WriteModelDataToFile';
Var
  LDataFile  : TStringList;
  LMessage   : String;
  LCount     : Integer;
  LStop      : boolean;
  LPlanningFileDataObject  : TPlanningFileDataObjects;
  LAllocationChannel : TAllocationChannelControlFileDataObject;
begin
  Result := False;
  try
    if FileExists(AFilename.FileName) then
      DeleteFile(AFilename.FileName);

    LPlanningFileDataObject  := TPlanningFileDataObjects(ADataObject);
    LAllocationChannel :=  LPlanningFileDataObject.AllocationChannelObject;

    if LAllocationChannel.HDextraLines.Count = 0 then
    begin
      //LMessage := FAppModules.Language.GetString('TFileAllocationChannelControlAgent.strNoDataReturned');
      //LMessage := Format(LMessage,[AFilename.FileName]);
      //AProgressFunction(LMessage,ptError,LStop);
      Result := True;
      Exit;
    end;

    if not Assigned(AProgressFunction) then
      AProgressFunction := DummyShowProgress;

    LMessage := FAppModules.Language.GetString('TFileAllocationChannelControlAgent.strWritingStarted'); //Update
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
      for LCount := 0 to LAllocationChannel.HDextraLines.Count - 1 do
        LDataFile.Add(LAllocationChannel.HDextraLines[LCount]);

      LDataFile.SaveToFile(AFilename.FileName);
      SetFileDate(AFileName);

      LMessage := FAppModules.Language.GetString('TFileAllocationChannelControlAgent.strWritingCompleted'); // Update
      LMessage := Format(LMessage,[ExtractFileName(AFilename.FileName)]);
      AProgressFunction(LMessage,ptNone,LStop);
      Result := True;
    finally
      LDataFile.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.


