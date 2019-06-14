//
//
//  UNIT      : Contains TDDTSOutputCSVFileAgent Class
//  AUTHOR    : Dziedzi Ramulondi(BCX)
//  DATE      : 14/08/2014
//  COPYRIGHT : Copyright © 2014 DWAF
//
//
unit UDDTSOutputCSVFileAgent;

interface

uses
  System.Classes,
  System.sysutils,
  System.contnrs,

  //  DWAF VCL
  UConstants,
  UFileNames,
  UAbstractObject,
  UDataFileObjects,
  UAbstractFileNamesObject,
  UAbstractFileAgent,
  UDDTSData,
  UDDTSDataObject;

type
  TDDTSOutputCSVFileAgent = class(TAbstractFileAgent)
  public
    function ReadModelDataFromFile(AFileName:TAbstractModelFileName;  ADataObject: TDataFileObjects;
             AProgressFunction: TProgressUpdateFuntion): boolean; override;
    function WriteModelDataToFile(AFileName:TAbstractModelFileName;  ADataObject: TDataFileObjects;
             AProgressFunction: TProgressUpdateFuntion): boolean; override;
    function GetFileStatistics(AFileName:TAbstractModelFileName; var AStarYear, AEndYear, ARowCount : integer): boolean;
  end;

implementation



uses UUtilities,
     System.DateUtils,
     UErrorHandlingOperations;


function TDDTSOutputCSVFileAgent.GetFileStatistics(AFileName:TAbstractModelFileName; var AStarYear, AEndYear, ARowCount : integer): boolean;
const OPNAME = 'TDDTSOutputCSVFileAgent.GetFileStatistics';
var
  LDate        : TDate;
  LReadString  : string;
  LCurrentline : integer;
  LLineData    : TStringList;
  LFileData    : TStringList;
begin
  Result := False;
  try
    AStarYear := 0;
    AEndYear  := 0;
    ARowCount := 0;

    if (AFilename.FileName = '') then Exit;
    If not FileExists(AFilename.FileName) then Exit;

    LLineData := TStringList.Create;
    LFileData := TStringList.Create;
    try
      //Read  file
      LFileData.LoadFromFile(AFilename.FileName);
      for LCurrentline := 0 to LFileData.Count - 1 do
      begin
        LReadString := LFileData[LCurrentline];
        LLineData.CommaText := LReadString;
        if(LLineData.Count >= 2) then
        begin
          LReadString  := LFileData[0];
          LDate := StrToDateDef(LReadString,0.0);
          if(LDate <> 0.0) then
          begin
          if(AStarYear = 0) then
            AStarYear   := YearOf(LDate);
            AEndYear    := YearOf(LDate);
            ARowCount   := ARowCount + 1;
          end;
        end;
      end;
      Result := True;
    finally
      LLineData.Free;
      LFileData.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDDTSOutputCSVFileAgent.ReadModelDataFromFile(AFileName:TAbstractModelFileName;ADataObject: TDataFileObjects;
                                               AProgressFunction: TProgressUpdateFuntion): boolean;
const OPNAME = 'TDDTSOutputCSVFileAgent.ReadModelDataFromFile';
var
  LMessage  : string;
  LIndex: Integer;
  LSOutputDataList : TDDTSOutputDataList;
  LSOutputData     : TDDTSOutputData;
  LFileData        : TStringlist;
  LStop: boolean;
begin
  Result := False;
  try
    if not Assigned(AProgressFunction) then
      AProgressFunction := DummyShowProgress;

    LMessage := FAppModules.Language.GetString('TDDTSOutputCSVFileAgent.strReadingStarted');
    LMessage := Format(LMessage,[ExtractFileName(AFilename.FileName)]);
    AProgressFunction(LMessage,ptNone,LStop);

    if (AFilename.FileName = '') then
      raise Exception.Create('File name parameter is blank.');


    //Check if file exists.
    If not FileExists(AFilename.FileName) then
    begin
      LMessage := FAppModules.Language.GetString('TDDTSOutputCSVFileAgent.strFileNoExist');
      LMessage := Format(LMessage,[ExtractFileName(AFilename.FileName)]);
      AProgressFunction(LMessage,ptError,LStop);
      Exit;
    end;

    LSOutputDataList :=  TDDTSDataObject(FAppModules.Model.ModelData).DDTSOutputDataList;

    if not LSOutputDataList.Initialise then
      Exit;

    //Read  file
    LFileData  := TStringlist.Create;
    try
      LFileData.LoadFromFile(AFilename.FileName);
      for LIndex := 0 to LFileData.Count - 1 do
      begin
        LSOutputData := LSOutputDataList.AddOutputData;
        LSOutputData.LoadFromSVString(LFileData[LIndex]);
        LSOutputData.RowIndex := LIndex;
        LSOutputData.Identifier := LIndex; //not 100% sure
        if((LIndex mod 1000) = 0) then
        begin
          AProgressFunction('',ptNone,LStop);
          if LStop then Exit;
        end;
      end;
    finally
      LFileData.Free;
    end;

    LMessage := FAppModules.Language.GetString('TDDTSOutputCSVFileAgent.strReadingCompleted');
    LMessage := Format(LMessage,[ExtractFileName(AFilename.FileName)]);
    AProgressFunction(LMessage,ptNone,LStop);

    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDDTSOutputCSVFileAgent.WriteModelDataToFile(AFileName:TAbstractModelFileName; ADataObject: TDataFileObjects;
                              AProgressFunction: TProgressUpdateFuntion): boolean;
const OPNAME = 'TDDTSOutputCSVFileAgent.WriteModelDataToFile';
var
  LMessage : String;
  LStop    : boolean;
  LIndex: Integer;
  LSOutputDataList : TDDTSOutputDataList;
  LSOutputData     : TDDTSOutputData;
  LFileData        : TStringlist;
begin
  Result := False;
  try
    if not Assigned(AProgressFunction) then
      AProgressFunction := DummyShowProgress;

    LMessage := FAppModules.Language.GetString('TDDTSOutputCSVFileAgent.strWritingStarted');
    LMessage := Format(LMessage,[ExtractFileName(AFilename.FileName)]);
    AProgressFunction(LMessage,ptNone,LStop);

    if (AFilename.FileName = '') then
      raise Exception.Create('File name parameter is blank.');


    LSOutputDataList :=  TDDTSDataObject(FAppModules.Model.ModelData).DDTSOutputDataList;
    LFileData  := TStringlist.Create;
    try
      for LIndex := 0 to LSOutputDataList.Count - 1 do
      begin
        LSOutputData := LSOutputDataList.OutputDataByIndex[LIndex];
        LFileData.Add(LSOutputData.SaveToCSVString);
        if((LIndex mod 1000) = 0) then
        begin
          AProgressFunction('',ptNone,LStop);
          if LStop then Exit;
        end;
      end;


      if(LFileData.Count > 0) then
      begin
        LFileData.SaveToFile(AFilename.FileName);
        SetFileDate(AFilename);
      end;
    finally
      LFileData.Free;
    end;
    LMessage := FAppModules.Language.GetString('TDDTSOutputCSVFileAgent.strWritingCompleted');
    LMessage := Format(LMessage,[ExtractFileName(AFilename.FileName)]);
    AProgressFunction(LMessage,ptNone,LStop);

    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.


