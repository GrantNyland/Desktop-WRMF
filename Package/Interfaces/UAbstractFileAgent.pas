//
//
//  UNIT      : Contains TAbstractFileAgent Class
//  AUTHOR    : Dziedzi Ramulondi(PDNA)
//  DATE      : 21/01/2002
//  COPYRIGHT : Copyright © 2001 DWAF
//
//
unit UAbstractFileAgent;

interface

uses
  Classes, sysutils,

  //  DWAF VCL
  UConstants,
  UFileNames,
  UAbstractObject,
  UDataFileObjects,
  UAbstractFileNamesObject,
  UAbstractModelData;

type
  TAbstractFileAgent = class(TAbstractAppObject)
  public
    function ReadModelDataFromFile(AFileName:TAbstractModelFileName;  ADataObject: TDataFileObjects;
             AProgressFunction: TProgressUpdateFuntion): boolean; virtual ; abstract;
    function WriteModelDataToFile(AFileName:TAbstractModelFileName;  ADataObject: TDataFileObjects;
             AProgressFunction: TProgressUpdateFuntion): boolean; virtual ; abstract;
    function SetFileDate(AFileName:TAbstractModelFileName): boolean;
    function ModelData: TAbstractModelData;
  end;


implementation

uses UUtilities,
     UErrorHandlingOperations;


{ TAbstractFileAgent }

function TAbstractFileAgent.ModelData: TAbstractModelData;
const OPNAME = 'TAbstractFileAgent.ModelData';
begin
  Result := Nil;
  try
    Result := TAbstractModelData(FAppModules.Model.ModelData);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAbstractFileAgent.SetFileDate(AFileName: TAbstractModelFileName): boolean;
const OPNAME = 'TAbstractFileAgent.SetFileDate';
begin
  Result := False;
  try
    if FileExists(AFileName.FileName) then
    begin
      if(TFileNameObject(AFileName).FileDate <= 0.0) then
        TFileNameObject(AFileName).FileDate := FileLastWriteDate(AFileName.FileName)
      else
      begin
        if(FAppModules.StudyArea.GetLastUpdateDate > FAppModules.StudyArea.GetStudyImportDate) then
          FileSetDate(AFileName.FileName,DateTimeToFileDate(FAppModules.StudyArea.GetLastUpdateDate))
        else
          FileSetDate(AFileName.FileName,DateTimeToFileDate(TFileNameObject(AFileName).FileDate));
      end;
      Result := True;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
