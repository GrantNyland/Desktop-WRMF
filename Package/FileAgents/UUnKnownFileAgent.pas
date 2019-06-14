//
//
//  UNIT      : Contains TRunParametersObject Class
//  AUTHOR    : Titi Ngubane(PDNA)
//  DATE      : 20/03/2002
//  COPYRIGHT : Copyright © 2002 DWAF
//
//
unit UUnKnownFileAgent;

interface

uses
  Classes, sysutils,contnrs,

  //  DWAF VCL
  UConstants,
  UFileNames,
  UAbstractObject,
  UDataFileObjects,
  UAbstractFileNamesObject,
  UAbstractFileAgent,
  UYieldModelDataObject;

type
  TUnKnownFileAgent = class(TAbstractFileAgent)
  public
    function ReadModelDataFromFile(AFileName:TAbstractModelFileName; AFileData: TStrings;
             AProgressFunction: TProgressUpdateFuntion): boolean;  reintroduce; virtual;
    function WriteModelDataToFile(AFileName:TAbstractModelFileName; AFileData: TStrings;
             AProgressFunction: TProgressUpdateFuntion): boolean; reintroduce;  virtual;
   end;

implementation



uses UUtilities,
     UFileNameConstants,
     UErrorHandlingOperations,
     UFilesLineTypeObject;

function TUnKnownFileAgent.ReadModelDataFromFile(AFileName:TAbstractModelFileName; AFileData: TStrings;
         AProgressFunction: TProgressUpdateFuntion): boolean;
const OPNAME = 'TUnKnownFileAgent.ReadModelDataFromFile';
var
  LMessage: string;
  LFileLineTypesObject: TAbstractFileLineTypesObject;
  LStop: boolean;
begin
  Result := False;
  try
    if not Assigned(AProgressFunction) then
      AProgressFunction := DummyShowProgress;

     LMessage := FAppModules.Language.GetString('TUnKnownFileAgent.strReadStarted');
     LMessage := Format(LMessage,[ExtractFileName(AFilename.FileName)]);
     AProgressFunction(LMessage,ptNone,LStop);

    if not Assigned(AFileData) then
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

    //Check if file exists.
    If not FileExists(AFilename.FileName) then
    begin
      LMessage := FAppModules.Language.GetString('TUnKnownFileAgent.strFileNoExist');
      LMessage := Format(LMessage,[ExtractFileName(AFilename.FileName)]);
      AProgressFunction(LMessage,ptError,LStop);
      Exit;
    end;

    //Read Unknown file
    AFileData.LoadFromFile(AFilename.FileName);
    TFileNameObject(AFilename).FileDate := FileLastWriteDate(AFileName.FileName);

    if (AFileName.FileGroup = fgDirectories) and (AFileData.Count >= 3) then
    begin
      LFileLineTypesObject := ModelData.FilesLineTypes.FileLineTypesObject[AFileName];
      if Assigned(LFileLineTypesObject) then
      begin
        TFileLineTypesObject(LFileLineTypesObject).ClearLineTypes;
        TFileLineTypesObject(LFileLineTypesObject).AddLineType(1,'1');
        TFileLineTypesObject(LFileLineTypesObject).AddLineType(2,'2');
        TFileLineTypesObject(LFileLineTypesObject).AddLineType(3,'3');
      end;
    end;

    LMessage := FAppModules.Language.GetString('TUnKnownFileAgent.strReadingCompleted');
    LMessage := Format(LMessage,[ExtractFileName(AFilename.FileName)]);
    AProgressFunction(LMessage,ptNone,LStop);

    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TUnKnownFileAgent.WriteModelDataToFile(AFileName:TAbstractModelFileName; AFileData: TStrings;
         AProgressFunction: TProgressUpdateFuntion): boolean;
const OPNAME = 'TUnKnownFileAgent.WriteModelDataToFile';
var
  LMessage : string;
  LStop: boolean;
begin
  Result := False;
  try
    if not Assigned(AProgressFunction) then
      AProgressFunction := DummyShowProgress;

    LMessage := FAppModules.Language.GetString('TUnKnownFileAgent.strWritingStarted');
    LMessage := Format(LMessage,[ExtractFileName(AFilename.FileName)]);
    AProgressFunction(LMessage,ptNone,LStop);
    if not Assigned(AFileData) then
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

    if(AFileData.Count > 0) then
    begin
      AFileData.SaveToFile(AFilename.FileName);
      SetFileDate(AFileName);
    end;
    
    LMessage := FAppModules.Language.GetString('TUnKnownFileAgent.strWritingCompleted');
    LMessage := Format(LMessage,[ExtractFileName(AFilename.FileName)]);
    AProgressFunction(LMessage,ptNone,LStop);

    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;
end.


