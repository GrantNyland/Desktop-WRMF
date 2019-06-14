//
//
//  UNIT      : Contains TFileSaltWashOffAgent Class
//  AUTHOR    : Lethabo Phatedi(Cornastone)
//  DATE      : 10/14/2016
//  COPYRIGHT : Copyright © 2016 DWS
//
//
unit UFileSaltWashOffAgent;

interface

uses
  Classes, sysutils,

  //  DWS VCL
  UFileNames,
  UConstants,
  UAbstractObject,
  UAbstractFileAgent,
  UDataFileObjects,
  USaltWashOffObject,
  UAbstractFileNamesObject,
  UYieldModelDataObject,
  UPlanningFileDataObjects;

type

  TFileSaltWashOffAgent = class(TAbstractFileAgent)
  public
    { Public declarations }
    function ReadModelDataFromFile(AFileName:TAbstractModelFileName;  ADataObject: TDataFileObjects;
             AProgressFunction: TProgressUpdateFuntion): boolean; override;
    function WriteModelDataToFile(AFileName:TAbstractModelFileName;  ADataObject: TDataFileObjects;
             AProgressFunction: TProgressUpdateFuntion): boolean; override;
  end;


implementation

uses UUtilities,
     UFilesLineTypeObject,
     UErrorHandlingOperations;

function TFileSaltWashOffAgent.ReadModelDataFromFile(AFileName:TAbstractModelFileName; ADataObject: TDataFileObjects;
             AProgressFunction: TProgressUpdateFuntion): boolean;
const OPNAME = 'TFileSaltWashOffAgent.ReadModelDataFromFile';
var
  LSaltWashOffData           : TStringList;
  LMessage                   : String;
  LStop                      : boolean;
  LPlanningFileDataObjects   : TPlanningFileDataObjects;
  LSaltWashOff               : TSaltWashOffObject;
  LStart                     : Integer;

Begin
   Result := False;
  try
    //Check if file exists.
    If not FileExists(AFilename.FileName) then
    begin
      LMessage := FAppModules.Language.GetString('TFileSaltWashOffAgent.strFileNoExist');
      LMessage := Format(LMessage,[ExtractFileName(AFilename.FileName)]);
      AProgressFunction(LMessage,ptNone,LStop);
      Result := True;
      Exit;
    end;

    if not Assigned(AProgressFunction) then
      AProgressFunction := DummyShowProgress;

    LMessage := FAppModules.Language.GetString('TFileSaltWashOffAgent.strReadingStarted');
    LMessage := Format(LMessage,[ExtractFileName(AFilename.FileName)]);
    AProgressFunction(LMessage,ptNone,LStop);

    if not Assigned(ADataObject) then
      raise Exception.Create('File object parameter is not yet assigned.');

    if (AFilename.FileName = '') then
      raise Exception.Create('File name parameter is blank.');

    if not FilePathIsDosCompatible(FAppModules,AFilename.FileName) then
    begin
      LMessage := FAppModules.Language.GetString('TFileSaltWashOffAgent.strPathNotDosCompatible');
      LMessage := Format(LMessage,[AFilename.FileName]);
      AProgressFunction(LMessage,ptError,LStop);
    end;

    //LSaltWashOff := TPlanningFileDataObjects(ADataObject).SaltWashOffObject;
    LPlanningFileDataObjects  := TPlanningFileDataObjects(ADataObject);
    LSaltWashOff    := LPlanningFileDataObjects.AddSaltWashOffObject(AFilename.FileNumber);

    TFileNameObject(AFilename).FileDate := FileLastWriteDate(AFileName.FileName);

    if not LSaltWashOff.Initialise then
    Exit;

    LSaltWashOff.FSaltWashFile.FData       := AFilename.FileName;
    LSaltWashOff.FSaltWashFile.FLength     := Length(AFilename.FileName);
    LSaltWashOff.FSaltWashFile.FInitalised := True;

    LSaltWashOffData := TStringList.Create;
    try
        //File Read
        LSaltWashOffData.LoadFromFile(AFilename.FileName);
      begin

       for LStart := 0 to LSaltWashOffData.Count -1 do
           LSaltWashOff.FFileSaltWashOff01ExtraLines.Add(LSaltWashOffData[LStart]);

           LMessage := FAppModules.Language.GetString('TFileSaltWashOffAgent.strReadingCompleted');
           LMessage := Format(LMessage,[ExtractFileName(AFilename.FileName)]);
           AProgressFunction(LMessage,ptNone,LStop);
           Result := True;
       end;

    finally
      LSaltWashOffData.Free;
    end;
  except on E:Exception do  HandleError(E,OPNAME) end;
End;

function TFileSaltWashOffAgent.WriteModelDataToFile(AFileName:TAbstractModelFileName; ADataObject: TDataFileObjects;
             AProgressFunction: TProgressUpdateFuntion): boolean;
const OPNAME = 'TFileSaltWashOffAgent.WriteModelDataToFile';
var
  LStop                        : boolean;
  LMessage                     : String;
  LFileSaltWOffData            : TStringlist;
  LPlanningFiledataObject      : TPlanningFileDataObjects;
  LSaltWashOffObj              : TSaltWashOffObject;
  LCount                       : Integer;
begin
  Result := False;
  try

    //LSaltWashOffObj := TPlanningFileDataObjects(ADataObject).SaltWashOffObject;

    LPlanningFileDataObject  := TPlanningFileDataObjects(ADataObject);
    LSaltWashOffObj  := LPlanningFileDataObject.SaltWashOffObjectByFileNumber[AFilename.FileNumber];


    if LSaltWashOffObj.FFileSaltWashOff01ExtraLines.Count = 0 then
    begin
      Result := True;
      Exit;
    end;

    if not Assigned(AProgressFunction) then
      AProgressFunction := DummyShowProgress;

    LMessage := FAppModules.Language.GetString('TFileSaltWashOffAgent.strWritingStarted');
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

    LFileSaltWOffData := TStringList.Create;
    try
      for LCount := 0 to LSaltWashOffObj.FFileSaltWashOff01ExtraLines.Count - 1 do
      begin
        LFileSaltWOffData.Add(LSaltWashOffObj.FFileSaltWashOff01ExtraLines[LCount]);
      end;

      LFileSaltWOffData.SaveToFile(AFilename.FileName);
      SetFileDate(AFileName);

      LMessage := FAppModules.Language.GetString('TFileSaltWashOffAgent.strWritingCompleted');
      LMessage := Format(LMessage,[ExtractFileName(AFilename.FileName)]);
      AProgressFunction(LMessage,ptNone,LStop);
      Result := True;
    finally
      LFileSaltWOffData.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.

