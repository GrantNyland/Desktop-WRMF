//
//
//  UNIT      : Contains TFileAltParamAgent Class
//  AUTHOR    : Dziedzi Ramulondi(Cornastone)
//  DATE      : 04/08/2009
//  COPYRIGHT : Copyright © 2009 DWAF
//
//
unit UFileAltParamAgent;

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
  TFileAltParamAgent = class(TAbstractFileAgent)
  public
    function ReadModelDataFromFile(AFileName:TAbstractModelFileName; ADataObject: TDataFileObjects;
             AProgressFunction: TProgressUpdateFuntion): boolean;  override;
    function WriteModelDataToFile(AFileName:TAbstractModelFileName; ADataObject: TDataFileObjects;
             AProgressFunction: TProgressUpdateFuntion): boolean; override;
   end;

implementation



uses UUtilities,
     UFileNameConstants,
     UErrorHandlingOperations;

function TFileAltParamAgent.ReadModelDataFromFile(AFileName:TAbstractModelFileName; ADataObject: TDataFileObjects;
         AProgressFunction: TProgressUpdateFuntion): boolean;
const OPNAME = 'TFileAltParamAgent.ReadModelDataFromFile';
var
  LMessage: string;
  LStop: boolean;
begin
  Result := False;
  try
    if not Assigned(AProgressFunction) then
      AProgressFunction := DummyShowProgress;

    If AFilename.FileFound then
    begin
     LMessage := FAppModules.Language.GetString('TFileAltParamAgent.strReadStarted');
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

      //Check if file exists.
      If not FileExists(AFilename.FileName) then
      begin
        LMessage := FAppModules.Language.GetString('TFileAltParamAgent.strFileNoExist');
        LMessage := Format(LMessage,[ExtractFileName(AFilename.FileName)]);
        AProgressFunction(LMessage,ptError,LStop);
        Exit;
      end;

      //Read Unknown file
      ADataObject.FAltParamObject.LoadFromFile(AFilename.FileName);
      TFileNameObject(AFilename).FileDate := FileLastWriteDate(AFileName.FileName);

      LMessage := FAppModules.Language.GetString('TFileAltParamAgent.strReadingCompleted');
      LMessage := Format(LMessage,[ExtractFileName(AFilename.FileName)]);
      AProgressFunction(LMessage,ptNone,LStop);
    end;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFileAltParamAgent.WriteModelDataToFile(AFileName:TAbstractModelFileName; ADataObject: TDataFileObjects;
         AProgressFunction: TProgressUpdateFuntion): boolean;
const OPNAME = 'TFileAltParamAgent.WriteModelDataToFile';
var
  LMessage : string;
  LStop: boolean;
begin
  Result := False;
  try
    if not Assigned(AProgressFunction) then
      AProgressFunction := DummyShowProgress;

    if not Assigned(ADataObject) then
      raise Exception.Create('File object parameter is not yet assigned.');

    if(ADataObject.FAltParamObject.Count > 0) then
    begin
      LMessage := FAppModules.Language.GetString('TFileAltParamAgent.strWritingStarted');
      LMessage := Format(LMessage,[ExtractFileName(AFilename.FileName)]);
      AProgressFunction(LMessage,ptNone,LStop);

      if (AFilename.FileName = '') then
        raise Exception.Create('File name parameter is blank.');

      if not FilePathIsDosCompatible(FAppModules,AFilename.FileName) then
      begin
        LMessage := FAppModules.Language.GetString('TAbstractFileAgent.strPathNotDosCompatible');
        LMessage := Format(LMessage,[AFilename.FileName]);
        AProgressFunction(LMessage,ptError,LStop);
        if FAppModules.GlobalData.StopOnFirstErr then Exit ;
      end;

      ADataObject.FAltParamObject.SaveToFile(AFilename.FileName);
      SetFileDate(AFileName);

      LMessage := FAppModules.Language.GetString('TFileAltParamAgent.strWritingCompleted');
      LMessage := Format(LMessage,[ExtractFileName(AFilename.FileName)]);
      AProgressFunction(LMessage,ptNone,LStop);
    end;

    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;
end.


