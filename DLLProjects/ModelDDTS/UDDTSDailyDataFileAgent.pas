//
//
//  UNIT      : Contains TDDTSDailyDataFileAgent Class
//  AUTHOR    : Titi Ngubane(PDNA)
//  DATE      : 20/03/2002
//  COPYRIGHT : Copyright © 2002 DWAF
//
//
unit UDDTSDailyDataFileAgent;

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
  UDDTSDailyDataObject;

type
  TDDTSDailyDataFileAgent = class(TAbstractFileAgent)
  protected
    FFileType : TDailyFileType;
  public
    function ReadModelDataFromFile(AFileName:TAbstractModelFileName;  ADataObject: TDataFileObjects;
             AProgressFunction: TProgressUpdateFuntion): boolean; override;
    function WriteModelDataToFile(AFileName:TAbstractModelFileName;  ADataObject: TDataFileObjects;
             AProgressFunction: TProgressUpdateFuntion): boolean; override;
    function GetFileStatistics(AFileName:TAbstractModelFileName; var AStarYear, AEndYear, ARowCount : integer): boolean;

    property FileType : TDailyFileType read FFileType write FFileType;
  end;

implementation



uses UUtilities,
     System.DateUtils,
     UErrorHandlingOperations;


function TDDTSDailyDataFileAgent.GetFileStatistics(AFileName:TAbstractModelFileName; var AStarYear, AEndYear, ARowCount : integer): boolean;
const OPNAME = 'TDDTSDailyDataFileAgent.GetFileStatistics';
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

function TDDTSDailyDataFileAgent.ReadModelDataFromFile(AFileName:TAbstractModelFileName;ADataObject: TDataFileObjects;
                                               AProgressFunction: TProgressUpdateFuntion): boolean;
const OPNAME = 'TDDTSDailyDataFileAgent.ReadModelDataFromFile';
Var
  LPrevDate,
  LCurrDate : TDateTime;
  LValue    : Double;
  LMessage  : string;
  LIndex: Integer;
  LDailyDataObject: TDDTSDailyDataObject;
  LFileDataObject : TDailyDataObject;
  LStop: boolean;
begin
  Result := False;
  try
    if not Assigned(AProgressFunction) then
      AProgressFunction := DummyShowProgress;

    LMessage := FAppModules.Language.GetString('TDDTSDailyDataFileAgent.strReadingStarted');
    LMessage := Format(LMessage,[ExtractFileName(AFilename.FileName)]);
    AProgressFunction(LMessage,ptNone,LStop);

    if not Assigned(ADataObject) then
      raise Exception.Create('File object parameter is not yet assigned.');

    if (AFilename.FileName = '') then
      raise Exception.Create('File name parameter is blank.');


    //Check if file exists.
    If not FileExists(AFilename.FileName) then
    begin
      LMessage := FAppModules.Language.GetString('TDDTSDailyDataFileAgent.strFileNoExist');
      LMessage := Format(LMessage,[ExtractFileName(AFilename.FileName)]);
      AProgressFunction(LMessage,ptError,LStop);
      Exit;
    end;

    LDailyDataObject := ADataObject.FDDTSDailyDataObject;
    LFileDataObject  := nil;
    case FileType of
      dftRunoff             : LFileDataObject := LDailyDataObject.RunoffFileData;
      dftOtherInflow        : LFileDataObject := LDailyDataObject.OtherInflowFileData;
      dftIncreamentalRunoff : LFileDataObject := LDailyDataObject.IncreamentalRunoffFileData;
      dftEWR                : LFileDataObject := LDailyDataObject.EWRFileData;
      dftRainfall           : LFileDataObject := LDailyDataObject.RainfallFileData;
      dftEvaporation        : LFileDataObject := LDailyDataObject.EvaporationFileData;
    end;

    if (LFileDataObject = nil) then
      raise Exception.Create('DDTS FileType not yet initialised.');

    TFileNameObject(AFilename).FileDate := FileLastWriteDate(AFileName.FileName);

    if not LFileDataObject.Initialise then
      Exit;

    //Read  file
    LFileDataObject.ReadFile(AFilename.FileName);
    if(LFileDataObject.Count = 0) then
    begin
      LMessage := FAppModules.Language.GetString('TFileDailyFlowDataAgent.strNoDataReturned');
        LMessage := Format(LMessage,[AFilename.FileName]);
      AProgressFunction(LMessage,ptError,LStop);
      if FAppModules.GlobalData.StopOnFirstErr then Exit ;
    end;

    LPrevDate  := 0.0;
    LCurrDate  := 0.0;
    LValue     := 0.0;
    for LIndex := 0 to LFileDataObject.Count - 1 do
    begin
      if not LFileDataObject.ReadDailyValues(LIndex,LCurrDate,LValue) then
        raise Exception.Create('Could not read file('+AFilename.FileName+') Line number number: '+ IntToStr(LIndex+1)+' Check that the format is yyyy/mm/dd, real value.');

      if(LIndex = 0) then
        LPrevDate  := LCurrDate-1;


      if((LIndex mod 1000) = 0) then
      begin
        AProgressFunction('',ptNone,LStop);
        if LStop then Exit;
      end;

      if(LCurrDate = NullFloat) then
      begin
        LMessage := FAppModules.Language.GetString('TFileDailyFlowDataAgent.strDateIsEmpty');
        LMessage := Format(LMessage,[LIndex,1,10]);
        AProgressFunction(LMessage,ptError,LStop);
        if FAppModules.GlobalData.StopOnFirstErr then Exit ;
      end
      else if(LCurrDate <> (LPrevDate+1)) then
      begin
        LMessage := FAppModules.Language.GetString('TFileDailyFlowDataAgent.strGapsInDailyData');
        LMessage := Format(LMessage,[LIndex]);
        AProgressFunction(LMessage,ptError,LStop);
        if FAppModules.GlobalData.StopOnFirstErr then Exit ;
      end
      else
        LPrevDate  := LCurrDate;

      if(LValue = NullFloat) then
      begin
        LMessage := FAppModules.Language.GetString('TFileDailyFlowDataAgent.strNullValue');
        LMessage := Format(LMessage,[LIndex,11,17]);
        AProgressFunction(LMessage,ptError,LStop);
        if FAppModules.GlobalData.StopOnFirstErr then Exit ;
      end
    end;

    LMessage := FAppModules.Language.GetString('TDDTSDailyDataFileAgent.strReadingCompleted');
    LMessage := Format(LMessage,[ExtractFileName(AFilename.FileName)]);
    AProgressFunction(LMessage,ptNone,LStop);

    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDDTSDailyDataFileAgent.WriteModelDataToFile(AFileName:TAbstractModelFileName; ADataObject: TDataFileObjects;
                              AProgressFunction: TProgressUpdateFuntion): boolean;
const OPNAME = 'TDDTSDailyDataFileAgent.WriteModelDataToFile';
var
  LMessage : String;
  LStop    : boolean;
  LDailyDataObject: TDDTSDailyDataObject;
  LFileDataObject : TDailyDataObject;
begin
  Result := False;
  try
    if not Assigned(AProgressFunction) then
      AProgressFunction := DummyShowProgress;

    LMessage := FAppModules.Language.GetString('TDDTSDailyDataFileAgent.strWritingStarted');
    LMessage := Format(LMessage,[ExtractFileName(AFilename.FileName)]);
    AProgressFunction(LMessage,ptNone,LStop);

    if not Assigned(ADataObject) then
      raise Exception.Create('File object parameter is not yet assigned.');

    if (AFilename.FileName = '') then
      raise Exception.Create('File name parameter is blank.');


    LDailyDataObject := ADataObject.FDDTSDailyDataObject;
    LFileDataObject  := nil;
    case FileType of
      dftRunoff             : LFileDataObject := LDailyDataObject.RunoffFileData;
      dftOtherInflow        : LFileDataObject := LDailyDataObject.OtherInflowFileData;
      dftIncreamentalRunoff : LFileDataObject := LDailyDataObject.IncreamentalRunoffFileData;
      dftEWR                : LFileDataObject := LDailyDataObject.EWRFileData;
      dftRainfall           : LFileDataObject := LDailyDataObject.RainfallFileData;
      dftEvaporation        : LFileDataObject := LDailyDataObject.EvaporationFileData;
    end;

    if(LFileDataObject.Count > 0) then
    begin
      LFileDataObject.SaveToFile(AFilename.FileName);
      SetFileDate(AFilename);
    end;

    LMessage := FAppModules.Language.GetString('TDDTSDailyDataFileAgent.strWritingCompleted');
    LMessage := Format(LMessage,[ExtractFileName(AFilename.FileName)]);
    AProgressFunction(LMessage,ptNone,LStop);

    Result := True;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.


