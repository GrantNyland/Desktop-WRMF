//
//
//  UNIT      : Contains TPlotFileManager Class
//  AUTHOR    : Dziedzi Ramulondi(PDNA)
//  DATE      : 02/07/2002
//  COPYRIGHT : Copyright © 2002 DWAF
//
//
unit UPlotFileManager;

interface

uses
  Classes, sysutils,contnrs,DB,VCL.Dialogs,

  //  DWAF VCL
  VoaimsCom_TLB,
  UFileNames,
  UAbstractObject,
  UPlotFileObject,
  UDataFileObjects,
  UAbstractDatabaseAgent,
  UAbstractFileNamesObject,
  UOutputData,
  UYieldModelDataObject;
type

  TPlotFileManager = class(TAbstractDatabaseAgent)
  protected
    function ReadCounters(var ACurrentLineNumber : integer;var AFile : TextFile;ACountObject:TPlotCountObject;
             AFileName: TAbstractModelFileName;AProgressFunction: TProgressUpdateFuntion): boolean;
    function ReadReservoirElement(var ACurrentLineNumber : integer;var AFile : TextFile;AElementObject:TPlotElementObject;
             AFileName: TAbstractModelFileName;AProgressFunction: TProgressUpdateFuntion): boolean;
    function ReadChannelElement(var ACurrentLineNumber : integer;var AFile : TextFile;AElementObject:TPlotElementObject;
             AFileName: TAbstractModelFileName;AProgressFunction: TProgressUpdateFuntion): boolean;
    function ReadElementLineData(var ACurrentLineNumber : integer;var AFile : TextFile;APlotLineData : TPlotLineDataObject;
             AElementObject:TPlotElementObject; AFileName: TAbstractModelFileName;
             AProgressFunction: TProgressUpdateFuntion;ALoadCaseNumber,ASequenceNumber: integer): boolean;
    function WriteCountersToDB(ADataSet: TAbstractModelDataset;ACounts:TPlotCountObject;AProgressFunction: TProgressUpdateFuntion): boolean;
    function WriteElementsToDB(ADataSet: TAbstractModelDataset;AElementList:TPlotElementListObject;AProgressFunction: TProgressUpdateFuntion): boolean;
    function WriteElementDataToDB(ADataSet: TAbstractModelDataset;APlotLineData:TPlotLineDataObject;AProgressFunction: TProgressUpdateFuntion): boolean;
    function ReadCountersFromDB(ADataSet: TAbstractModelDataset;ACounts:TPlotCountObject;AProgressFunction: TProgressUpdateFuntion): boolean;
    function ReadElementsFromDB(ADataSet: TAbstractModelDataset;AElementList:TPlotElementListObject;AProgressFunction: TProgressUpdateFuntion): boolean;
    function ReadElementDataFromDB(ADataSet: TAbstractModelDataset;APlotLineData:TPlotLineDataObject;AProgressFunction: TProgressUpdateFuntion): boolean;

  public
    function PopulatePlotOutData(AFileName: TAbstractModelFileName; var AOutputData: TOutputData;
                                 AYieldModelData: TYieldModelDataObject;AProgressFunction: TProgressUpdateFuntion): boolean;
    function ValidateFile(AFileName: TAbstractModelFileName; AProgressFunction: TProgressUpdateFuntion): boolean;
    function ImportFile(AFileName: TAbstractModelFileName; AProgressFunction: TProgressUpdateFuntion): boolean;
    function ExportFile(AFileName: TAbstractModelFileName; AProgressFunction: TProgressUpdateFuntion): boolean;
    function ReadModelDataFromDatabase(AFileName:TFileNameObject; ADataObject: TDataFileObjects;
             AProgressFunction: TProgressUpdateFuntion): boolean; override;
    function WriteModelDataToDatabase(AFileName:TFileNameObject; ADataObject: TDataFileObjects;
             AProgressFunction: TProgressUpdateFuntion): boolean; override;
    function ClearModelDataInDatabase(AFileName:TFileNameObject;AProgressFunction: TProgressUpdateFuntion;
             AQuetly: boolean = False): boolean; override;

  end;

implementation


{ TPlotFileManager }
uses UDataSetType,
UConstants,
     UUtilities,
     UErrorHandlingOperations;

function TPlotFileManager.ReadCountersFromDB(ADataSet: TAbstractModelDataset; ACounts: TPlotCountObject;
         AProgressFunction: TProgressUpdateFuntion): boolean;
const OPNAME = 'TPlotFileManager.ReadCountersFromDB';
var
  LSQL: string;
begin
  Result := False;
  try
    LSQL := 'SELECT Model,StudyAreaName,SubArea,Scenario,ReservoirCount,ChannelCount,LoadCaseCount,SequenceCount,MonthCount,Comment'+
         ' FROM pltElementCount'+
         ' WHERE  (Model            ='+ QuotedStr(FAppModules.StudyArea.ModelCode)    +') AND'+
         '        (StudyAreaName    ='+QuotedStr(FAppModules.StudyArea.StudyAreaCode) +') AND'+
         '        (SubArea          ='+QuotedStr(FAppModules.StudyArea.SubAreaCode)   +') AND'+
         '        (Scenario         ='+QuotedStr(FAppModules.StudyArea.ScenarioCode)  +')';
    ACounts.Initialise;
    ADataSet.DataSet.Close;
    ADataSet.SetSQL(LSQL);
    ADataSet.DataSet.Open;
    if not ADataSet.DataSet.Eof then
    begin
      ACounts.FReservoirCount.FData       := ADataSet.DataSet.FieldByName('ReservoirCount').AsInteger;
      ACounts.FReservoirCount.FInitalised := True;
      ACounts.FChannelCount.FData         := ADataSet.DataSet.FieldByName('ChannelCount').AsInteger;
      ACounts.FChannelCount.FInitalised   := True;
      ACounts.FLoadCaseCount.FData        := ADataSet.DataSet.FieldByName('LoadCaseCount').AsInteger;
      ACounts.FLoadCaseCount.FInitalised  := True;
      ACounts.FSequenceCount.FData        := ADataSet.DataSet.FieldByName('SequenceCount').AsInteger;
      ACounts.FSequenceCount.FInitalised  := True;
      ACounts.FMonthsCount.FData          := ADataSet.DataSet.FieldByName('MonthCount').AsInteger;
      ACounts.FMonthsCount.FInitalised    := True;
      ACounts.FComment.FData              := Trim(ADataSet.DataSet.FieldByName('Comment').AsString);
      ACounts.FComment.FInitalised        := True;
    end;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TPlotFileManager.ReadElementsFromDB(ADataSet: TAbstractModelDataset; AElementList: TPlotElementListObject;
         AProgressFunction: TProgressUpdateFuntion): boolean;
const OPNAME = 'TPlotFileManager.ReadElementsFromDB';
var
  LSQL: string;
  LElementType:TPlotElementType;
  LPlotElement:TPlotElementObject;
begin
  Result := False;
  try
    LSQL := 'SELECT Model,StudyAreaName,SubArea,Scenario,Identifier,ElementType,ElementNumber,ElementName'+
         ' FROM pltElement'+
         ' WHERE  (Model            ='+ QuotedStr(FAppModules.StudyArea.ModelCode)    +') AND'+
         '        (StudyAreaName    ='+QuotedStr(FAppModules.StudyArea.StudyAreaCode) +') AND'+
         '        (SubArea          ='+QuotedStr(FAppModules.StudyArea.SubAreaCode)   +') AND'+
         '        (Scenario         ='+QuotedStr(FAppModules.StudyArea.ScenarioCode)  +')'+
         ' ORDER BY Model,StudyAreaName,SubArea,Scenario,Identifier';
    AElementList.Initialise;
    ADataSet.DataSet.Close;
    ADataSet.SetSQL(LSQL);
    ADataSet.DataSet.Open;
    if not ADataSet.DataSet.Eof then
    begin
      while not ADataSet.DataSet.Eof do
      begin
        LElementType := TPlotElementType(ADataSet.DataSet.FieldByName('ElementType').AsInteger);
        LPlotElement := AElementList.AddElement(LElementType);
        LPlotElement.FElementNumber.FData       := ADataSet.DataSet.FieldByName('ElementNumber').AsInteger;
        LPlotElement.FElementNumber.FInitalised := True;
        LPlotElement.FElementName.FData         := Trim(ADataSet.DataSet.FieldByName('ElementName').AsString);
        LPlotElement.FElementName.FInitalised   := True;
        ADataSet.DataSet.Next;
      end;
    end;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TPlotFileManager.ReadElementDataFromDB(ADataSet: TAbstractModelDataset; APlotLineData: TPlotLineDataObject;
         AProgressFunction: TProgressUpdateFuntion): boolean;
const OPNAME = 'TPlotFileManager.ReadElementDataFromDB';
var
  LFieldName: string;
  LIndex: integer;
begin
  Result := False;
  try
    APlotLineData.Initialise;
    if not ADataSet.DataSet.Eof then
    begin
      APlotLineData.FElementType     := TPlotElementType(ADataSet.DataSet.FieldByName('ElementType').AsInteger);
      APlotLineData.FElementNumber   := ADataSet.DataSet.FieldByName('ElementNumber').AsInteger;
      APlotLineData.FLoadCaseNumber  := ADataSet.DataSet.FieldByName('LoadCaseNumber').AsInteger;
      APlotLineData.FSequenceNumber  := ADataSet.DataSet.FieldByName('SequenceNumber').AsInteger;
      for LIndex := MinMonths to MaxMonths do
      begin
        LFieldName := Format('%s%2.2d',['Value',LIndex]);
        APlotLineData.FMonthlyValues[LIndex].FData := ADataSet.DataSet.FieldByName(LFieldName).AsFloat;
        APlotLineData.FMonthlyValues[LIndex].FInitalised := True;
      end;
      Result := True;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TPlotFileManager.WriteCountersToDB(ADataSet: TAbstractModelDataset;ACounts: TPlotCountObject;AProgressFunction: TProgressUpdateFuntion): boolean;
const OPNAME = 'TPlotFileManager.WriteCountersToDB';
var
  LSQL: string;
begin
  Result := False;
  try
    LSQL := 'INSERT INTO pltElementCount'+
              ' (Model,StudyAreaName,SubArea,Scenario,ReservoirCount,ChannelCount,LoadCaseCount,SequenceCount,MonthCount,Comment)'+
              ' Values(:Model,:StudyAreaName,:SubArea,:Scenario,:ReservoirCount,:ChannelCount,:LoadCaseCount,:SequenceCount,:MonthCount,:Comment)';
    ADataSet.DataSet.Close;
    ADataSet.SetSQL(LSQL);
    ADataSet.SetParams(
      ['Model','StudyAreaName','SubArea','Scenario'],
      [FAppModules.StudyArea.ModelCode,FAppModules.StudyArea.StudyAreaCode,
       FAppModules.StudyArea.SubAreaCode,FAppModules.StudyArea.ScenarioCode]);
    ADataSet.SetParams(['ReservoirCount'], [IntToStr(ACounts.FReservoirCount.FData)]);
    ADataSet.SetParams(['ChannelCount'], [IntToStr(ACounts.FChannelCount.FData)]);
    ADataSet.SetParams(['LoadCaseCount'], [IntToStr(ACounts.FLoadCaseCount.FData)]);
    ADataSet.SetParams(['SequenceCount'], [IntToStr(ACounts.FSequenceCount.FData)]);
    ADataSet.SetParams(['MonthCount'], [IntToStr(ACounts.FMonthsCount.FData)]);
    ADataSet.SetParams(['Comment'], [ACounts.FComment.FData]);
    ADataSet.ExecSQL;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TPlotFileManager.WriteElementsToDB(ADataSet: TAbstractModelDataset;AElementList: TPlotElementListObject; AProgressFunction: TProgressUpdateFuntion): boolean;
const OPNAME = 'TPlotFileManager.WriteElementsToDB';
var
  LSQL: string;
  LIndex : integer;
  LPlotElement: TPlotElementObject;
begin
  Result := False;
  try
    LSQL := 'INSERT INTO pltElement'+
              ' (Model,StudyAreaName,SubArea,Scenario,Identifier,ElementType,ElementNumber,ElementName)'+
              ' Values(:Model,:StudyAreaName,:SubArea,:Scenario,:Identifier,:ElementType,:ElementNumber,:ElementName)';
    ADataSet.DataSet.Close;
    for LIndex := 1 to AElementList.ElementsCount do
    begin
      LPlotElement := AElementList.ElementByIndex(LIndex-1);
      ADataSet.SetSQL(LSQL);
      ADataSet.SetParams(
        ['Model','StudyAreaName','SubArea','Scenario'],
        [FAppModules.StudyArea.ModelCode,FAppModules.StudyArea.StudyAreaCode,
         FAppModules.StudyArea.SubAreaCode,FAppModules.StudyArea.ScenarioCode]);
      ADataSet.SetParams(['Identifier'], [IntToStr(LIndex)]);
      ADataSet.SetParams(['ElementType'], [IntToStr(Ord(LPlotElement.FElementType))]);
      ADataSet.SetParams(['ElementNumber'], [IntToStr(LPlotElement.FElementNumber.FData)]);
      ADataSet.SetParams(['ElementName'], [LPlotElement.FElementName.FData]);
      ADataSet.ExecSQL;
      ADataSet.DataSet.Close;
    end;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TPlotFileManager.WriteElementDataToDB(ADataSet: TAbstractModelDataset;APlotLineData: TPlotLineDataObject;AProgressFunction: TProgressUpdateFuntion): boolean;
const OPNAME = 'TPlotFileManager.WriteElementDataToDB';
var
  LSQL: string;
  LFieldName: string;
  LIndex: integer;
begin
  Result := False;
  try
    LSQL := 'INSERT INTO pltFileData'+
              ' (Model,StudyAreaName,SubArea,Scenario,Identifier,LoadCaseNumber,SequenceNumber,ElementType,ElementNumber,'+
              ' Value01,Value02,Value03,Value04,Value05,Value06,Value07,Value08,Value09,Value10,Value11,Value12)'+
              ' Values(:Model,:StudyAreaName,:SubArea,:Scenario,:Identifier,:LoadCaseNumber,:SequenceNumber,:ElementType,:ElementNumber,'+
              ' :Value01,:Value02,:Value03,:Value04,:Value05,:Value06,:Value07,:Value08,:Value09,:Value10,:Value11,:Value12)';
    ADataSet.DataSet.Close;
    ADataSet.SetSQL(LSQL);
    ADataSet.SetParams(
      ['Model','StudyAreaName','SubArea','Scenario'],
      [FAppModules.StudyArea.ModelCode,FAppModules.StudyArea.StudyAreaCode,
       FAppModules.StudyArea.SubAreaCode,FAppModules.StudyArea.ScenarioCode]);
    ADataSet.SetParams(['Identifier'], [IntToStr(APlotLineData.FIdentifier)]);
    ADataSet.SetParams(['LoadCaseNumber'], [IntToStr(APlotLineData.FLoadCaseNumber)]);
    ADataSet.SetParams(['SequenceNumber'], [IntToStr(APlotLineData.FSequenceNumber)]);
    ADataSet.SetParams(['ElementType'], [IntToStr(Ord(APlotLineData.FElementType))]);
    ADataSet.SetParams(['ElementNumber'], [IntToStr(APlotLineData.FElementNumber)]);
    for LIndex := MinMonths to MaxMonths do
    begin
      LFieldName := Format('%s%2.2d',['Value',LIndex]);
      ADataSet.SetParams([LFieldName], [FormatFloat('#####0.000',APlotLineData.FMonthlyValues[LIndex].FData)]);
    end;
    ADataSet.ExecSQL;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TPlotFileManager.ReadCounters(var ACurrentLineNumber : integer;var AFile: TextFile; ACountObject: TPlotCountObject;
         AFileName: TAbstractModelFileName;AProgressFunction: TProgressUpdateFuntion): boolean;
const OPNAME = 'TPlotFileManager.ReadCounters';
var
  LReadString,
  LTempString,
  LMessage : string;
  LStop: boolean;
  LReadInteger,
  LErrorCode : Integer;
begin
  Result := False;
  try
    //line1 +++++++++++++++++++++++++
    if Eof(AFile) then
    begin
      LMessage := FAppModules.Language.GetString('TPlotFileManager.strFileEmptyErr');
      LMessage := Format(LMessage,[ExtractFileName(AFileName.FileName)]);
      AProgressFunction(LMessage,ptError,LStop);
      if FAppModules.GlobalData.StopOnFirstErr then Exit ;
    end
    else
    begin
      Readln(AFile, LReadString);
      ACurrentLineNumber := ACurrentLineNumber + 1;
      LTempString:=GetSubstring(LReadString,1,5);
      LTempString := Trim(LTempString);
      Val(LTempString,LReadInteger,LErrorCode);
      if(LErrorCode <> 0) then
      begin
        LMessage := FAppModules.Language.GetString('TPlotFileManager.strReservoirCountErr');
        LMessage := Format(LMessage,[ACurrentLineNumber,1,5]);
        AProgressFunction(LMessage,ptError,LStop);
        if FAppModules.GlobalData.StopOnFirstErr then Exit ;
      end
      else
      begin
        ACountObject.FReservoirCount.FData :=LReadInteger;
        ACountObject.FReservoirCount.FInitalised := True;
      end;

      LTempString:=GetSubstring(LReadString,6,5);
      LTempString := Trim(LTempString);
      Val(LTempString,LReadInteger,LErrorCode);
      if(LErrorCode <> 0) then
      begin
        LMessage := FAppModules.Language.GetString('TPlotFileManager.strSequenceCountErr');
        LMessage := Format(LMessage,[ACurrentLineNumber,6,10]);
        AProgressFunction(LMessage,ptError,LStop);
        if FAppModules.GlobalData.StopOnFirstErr then Exit ;
      end
      else
      begin
        ACountObject.FSequenceCount.FData :=LReadInteger;
        ACountObject.FSequenceCount.FInitalised := True;
      end;

      LTempString:=GetSubstring(LReadString,11,5);
      LTempString := Trim(LTempString);
      Val(LTempString,LReadInteger,LErrorCode);
      if(LErrorCode <> 0) then
      begin
        LMessage := FAppModules.Language.GetString('TPlotFileManager.strMonthsCountErr');
        LMessage := Format(LMessage,[ACurrentLineNumber,11,15]);
        AProgressFunction(LMessage,ptError,LStop);
        if FAppModules.GlobalData.StopOnFirstErr then Exit ;
      end
      else
      begin
        ACountObject.FMonthsCount.FData :=LReadInteger;
        ACountObject.FMonthsCount.FInitalised := True;
      end;

      LTempString:=GetSubstring(LReadString,16,5);
      LTempString := Trim(LTempString);
      Val(LTempString,LReadInteger,LErrorCode);
      if(LErrorCode <> 0) then
      begin
        LMessage := FAppModules.Language.GetString('TPlotFileManager.strMonthsCountErr');
        LMessage := Format(LMessage,[ACurrentLineNumber,16,20]);
        AProgressFunction(LMessage,ptError,LStop);
        if FAppModules.GlobalData.StopOnFirstErr then Exit ;
      end
      else
      begin
        ACountObject.FChannelCount.FData :=LReadInteger;
        ACountObject.FChannelCount.FInitalised := True;
      end;
    end;

    //line2 +++++++++++++++++++++++++
    if(FAppModules.StudyArea.ModelVersion <> '7' ) then
    begin
      if Eof(AFile) then
      begin
        LMessage := FAppModules.Language.GetString('TPlotFileManager.strFileNoLine2Err');
        LMessage := Format(LMessage,[ExtractFileName(AFileName.FileName)]);
        AProgressFunction(LMessage,ptError,LStop);
        if FAppModules.GlobalData.StopOnFirstErr then Exit ;
      end
      else
      begin
        Readln(AFile, LReadString);
        ACurrentLineNumber := ACurrentLineNumber + 1;
        LTempString:=Trim(GetSubstring(LReadString,21,10));
        if(LTempString <> '') then
        begin
          ACountObject.FComment.FData :=LTempString;
          ACountObject.FComment.FInitalised := True;
        end;
      end;
    end;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TPlotFileManager.ReadReservoirElement(var ACurrentLineNumber: integer; var AFile: TextFile; AElementObject: TPlotElementObject;
  AFileName: TAbstractModelFileName; AProgressFunction: TProgressUpdateFuntion): boolean;
const OPNAME = 'TPlotFileManager.ReadReservoirElement';
var
  LReadString,
  LTempString,
  LMessage : string;
  LStop: boolean;
  LReadInteger,
  LErrorCode : Integer;
begin
  Result := False;
  try
    //line3 Reservoirs
    if Eof(AFile) then
    begin
      LMessage := FAppModules.Language.GetString('TPlotFileManager.strFileResevoirLessThanCountErr');
      LMessage := Format(LMessage,[ExtractFileName(AFileName.FileName)]);
      AProgressFunction(LMessage,ptError,LStop);
      if FAppModules.GlobalData.StopOnFirstErr then Exit ;
    end
    else
    begin
      Readln(AFile, LReadString);
      ACurrentLineNumber := ACurrentLineNumber + 1;
      LTempString:=GetSubstring(LReadString,1,32);
      if(Trim(LTempString) <> '') then
      begin
        AElementObject.FElementName.FData:= Trim(LTempString);
        AElementObject.FElementName.FInitalised := True;

        LTempString:=GetSubstring(LReadString,33,4);
        if(Trim(LTempString) <> '') then
        begin
          LTempString := Trim(LTempString);
          Val(LTempString,LReadInteger,LErrorCode);
          if(LErrorCode <> 0) then
          begin
            LMessage := FAppModules.Language.GetString('TPlotFileManager.strReservoirNumberErr');
            LMessage := Format(LMessage,[ACurrentLineNumber,33,36]);
            AProgressFunction(LMessage,ptError,LStop);
            if FAppModules.GlobalData.StopOnFirstErr then Exit ;
          end
          else
          begin
            AElementObject.FElementNumber.FData :=LReadInteger;
            AElementObject.FElementNumber.FInitalised := True;
          end;
        end;
        Result := True;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TPlotFileManager.ReadChannelElement(var ACurrentLineNumber: integer; var AFile: TextFile; AElementObject: TPlotElementObject;
  AFileName: TAbstractModelFileName; AProgressFunction: TProgressUpdateFuntion): boolean;
const OPNAME = 'TPlotFileManager.ReadChannelElement';
var
  LReadString,
  LTempString,
  LMessage : string;
  LStop: boolean;
  LReadInteger,
  LErrorCode : Integer;
begin
  Result := False;
  try
    //line4 Channels
    if Eof(AFile) then
    begin
      LMessage := FAppModules.Language.GetString('TPlotFileManager.strFileChannelLessThanCountErr');
      LMessage := Format(LMessage,[ExtractFileName(AFileName.FileName)]);
      AProgressFunction(LMessage,ptError,LStop);
      if FAppModules.GlobalData.StopOnFirstErr then Exit ;
    end
    else
    begin
      Readln(AFile, LReadString);
      ACurrentLineNumber := ACurrentLineNumber + 1;
      LTempString:=GetSubstring(LReadString,1,32);
      if(Trim(LTempString) <> '') then
      begin
        AElementObject.FElementName.FData:= Trim(LTempString);
        AElementObject.FElementName.FInitalised := True;

        LTempString:=GetSubstring(LReadString,33,4);
        if(Trim(LTempString) <> '') then
        begin
          LTempString := Trim(LTempString);
          Val(LTempString,LReadInteger,LErrorCode);
          if(LErrorCode <> 0) then
          begin
            LMessage := FAppModules.Language.GetString('TPlotFileManager.strChannelNumberErr');
            LMessage := Format(LMessage,[ACurrentLineNumber,33,36]);
            AProgressFunction(LMessage,ptError,LStop);
            if FAppModules.GlobalData.StopOnFirstErr then Exit ;
          end
          else
          begin
            AElementObject.FElementNumber.FData :=LReadInteger;
            AElementObject.FElementNumber.FInitalised := True;
          end;
        end;
        Result := True;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TPlotFileManager.ReadElementLineData(var ACurrentLineNumber: integer; var AFile: TextFile;
         APlotLineData: TPlotLineDataObject;AElementObject: TPlotElementObject; AFileName: TAbstractModelFileName;
         AProgressFunction: TProgressUpdateFuntion;ALoadCaseNumber,ASequenceNumber: integer): boolean;
const OPNAME = 'TPlotFileManager.ReadElementLineData';
var
  LReadString,
  LTempString,
  LMessage : string;
  LStop: boolean;
  LCount,
  LPos,
  LErrorCode : Integer;
  LReadReal : double;
begin
  Result := False;
  try
    APlotLineData.Initialise;
    //Line data
    if Eof(AFile) then
    begin
      LMessage := '';
      case AElementObject.FElementType of
        pltelReservoir: LMessage :=  FAppModules.Language.GetString('TPlotFileManager.strFileEofReservoirErr');
        pltelChannel  : LMessage :=  FAppModules.Language.GetString('TPlotFileManager.strFileEofChannelErr');
        pltelMaster   : LMessage :=  FAppModules.Language.GetString('TPlotFileManager.strFileEofMasterErr');
        pltelTotal    : LMessage :=  FAppModules.Language.GetString('TPlotFileManager.strFileEofTotalErr');
      end;//case
      LMessage := Format(LMessage,[ExtractFileName(AFileName.FileName),ACurrentLineNumber+1]);
      AProgressFunction(LMessage,ptError,LStop);
      if FAppModules.GlobalData.StopOnFirstErr then Exit ;
    end
    else
    begin
      Readln(AFile, LReadString);
      ACurrentLineNumber := ACurrentLineNumber + 1;
      if(Trim(LReadString) <> '') then
      begin
        APlotLineData.FElementType    := AElementObject.FElementType;
        APlotLineData.FElementNumber  := AElementObject.FElementNumber.FData;
        APlotLineData.FLoadCaseNumber := ALoadCaseNumber;
        APlotLineData.FSequenceNumber := ASequenceNumber;
        LPos := 1;
        LMessage := '';
        case AElementObject.FElementType of
          pltelReservoir: LMessage :=  FAppModules.Language.GetString('TPlotFileManager.strFileResevoirVolumeErr');
          pltelChannel  : LMessage :=  FAppModules.Language.GetString('TPlotFileManager.strFileChannelFlowErr');
          pltelMaster   : LMessage :=  FAppModules.Language.GetString('TPlotFileManager.strFileMasterFlowErr');
          pltelTotal    : LMessage :=  FAppModules.Language.GetString('TPlotFileManager.strFileTotalVolumeErr');
        end;//case
        for LCount := MinMonths to MaxMonths do
        begin
          LTempString:=GetSubstring(LReadString,LPos,11);
          LTempString := Trim(LTempString);
          Val(LTempString,LReadReal,LErrorCode);
          if(LErrorCode <> 0) then
          begin
            LMessage := Format(LMessage,[ACurrentLineNumber,LPos,LPos+11]);
            AProgressFunction(LMessage,ptError,LStop);
            if FAppModules.GlobalData.StopOnFirstErr then Exit ;
          end
          else
          begin
            APlotLineData.FMonthlyValues[LCount].FData:=LReadReal;
            APlotLineData.FMonthlyValues[LCount].FInitalised:= True;
          end;
          Inc(LPos,11);
        end;
        Result := True;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TPlotFileManager.PopulatePlotOutData(AFileName: TAbstractModelFileName; var AOutputData: TOutputData;
                                 AYieldModelData: TYieldModelDataObject;AProgressFunction: TProgressUpdateFuntion): boolean;
const OPNAME = 'TPlotFileManager.PopulatePlotOutData';
var
  LMessage : string;
  LStop: boolean;
  LCountObject:TPlotCountObject;
  LElementObject:TPlotElementObject;
  LElementListObject:TPlotElementListObject;
  LLineDataObject:TPlotLineDataObject;
  LFile : TextFile;
  LCount        : integer;
  LLineNumber   : integer;
begin
  Result := False;
  try
    //Check if file exists.
    if not FileExists(AFilename.FileName) then
    begin
      Result := True;
      Exit;
    end;
    if not Assigned(AProgressFunction) then
      AProgressFunction := DummyShowProgress;

    if not Assigned(AFileName) then
      raise Exception.Create('Plot file names parameter is not yet assigned.');

    //Check if file exists.
    if not FileExists(AFilename.FileName) then
    begin
      LMessage := FAppModules.Language.GetString('TPlotFileManager.strFileNoExist');
      LMessage := Format(LMessage,[ExtractFileName(AFileName.FileName)]);
      AProgressFunction(LMessage,ptError,LStop);
      Exit;
    end;
    LCountObject      := TPlotCountObject.Create;
    LElementListObject := TPlotElementListObject.Create;
    LLineDataObject    := TPlotLineDataObject.Create;
    try
      LCountObject.Initialise;
      LElementListObject.Initialise;
      LLineDataObject.Initialise;
      AssignFile(LFile, AFileName.FileName);
      try
        Reset(LFile);
        LLineNumber := 0;
        //Line 1 & 2 (Counters);
        if ReadCounters(LLineNumber,LFile,LCountObject,AFileName,AProgressFunction) then
        begin
          //Read all reservoirs names & numbers
          for LCount := 1 to LCountObject.FReservoirCount.FData do
          begin
            LElementObject := LElementListObject.AddElement(pltelReservoir);
            if not ReadReservoirElement(LLineNumber,LFile,LElementObject,AFileName,AProgressFunction) then
              Break;
            AOutputData.CastPlottingOutputData.Populate(votReservoir,btMonthEndReservoirVolume,
                                 LElementObject.FElementNumber.FData,LElementObject.FElementName.FData);

            AOutputData.CastPlottingOutputData.Populate(votReservoir,btMonthEndReservoirVolume,
                                 LElementObject.FElementNumber.FData,LElementObject.FElementName.FData);
          end;
          //Read all channel names & numbers
          for LCount := 1 to LCountObject.FChannelCount.FData do
          begin
            LElementObject := LElementListObject.AddElement(pltelChannel);
            if not ReadChannelElement(LLineNumber,LFile,LElementObject,AFileName,AProgressFunction) then
              Break;
            AOutputData.CastPlottingOutputData.Populate(votChannel,btMonthlyAverageChannelFlow,
                                 LElementObject.FElementNumber.FData,LElementObject.FElementName.FData);
          end;

        end;
      finally
        CloseFile(LFile);
      end;
    finally
      FreeAndNil(LCountObject);
      FreeAndNil(LElementListObject);
      FreeAndNil(LLineDataObject);
    end;
    LMessage := FAppModules.Language.GetString('TPlotFileManager.strFileValidationCompleted');
    LMessage := Format(LMessage,[ExtractFileName(AFileName.FileName)]);
    AProgressFunction(LMessage,ptNone,LStop);
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


function TPlotFileManager.ValidateFile(AFileName: TAbstractModelFileName;AProgressFunction: TProgressUpdateFuntion): boolean;
const OPNAME = 'TPlotFileManager.ValidateFile';
var
  LMessage : string;
  LStop: boolean;
  LCountObject:TPlotCountObject;
  LElementObject:TPlotElementObject;
  LElementListObject:TPlotElementListObject;
  LLineDataObject:TPlotLineDataObject;
  LFile : TextFile;
  LMonthCount    : integer;
  LSequenceCount : integer;

  LCount        : integer;
  LLineNumber   : integer;
  LMonthIndex   : integer;
  LSequenceIndex: integer;
  LLoadCaseIndex : integer;
  LElementIndex : integer;
  LIdentifier  : integer;
begin
  Result := False;
  try
    //Check if file exists.
    If not FileExists(AFilename.FileName) then
    begin
      Result := True;
      Exit;
    end;

    LMessage := FAppModules.Language.GetString('TPlotFileManager.strFileValidationStarted');
    LMessage := Format(LMessage,[ExtractFileName(AFilename.FileName)]);
    AProgressFunction(LMessage,ptNone,LStop);

    if not Assigned(AFileName) then
      raise Exception.Create('Plot file names parameter is not yet assigned.');

    if not Assigned(AProgressFunction) then
      AProgressFunction := DummyShowProgress;

    //Check if file exists.
    If not FileExists(AFilename.FileName) then
    begin
      LMessage := FAppModules.Language.GetString('TPlotFileManager.strFileNoExist');
      LMessage := Format(LMessage,[ExtractFileName(AFileName.FileName)]);
      AProgressFunction(LMessage,ptError,LStop);
      Exit;
    end;

    LCountObject      := TPlotCountObject.Create;
    LElementListObject := TPlotElementListObject.Create;
    LLineDataObject    := TPlotLineDataObject.Create;
    try
      LCountObject.Initialise;
      LElementListObject.Initialise;
      LLineDataObject.Initialise;
      AssignFile(LFile, AFileName.FileName);
      try
        Reset(LFile);
        LLineNumber := 0;
        //Line 1 & 2 (Counters);
        if ReadCounters(LLineNumber,LFile,LCountObject,AFileName,AProgressFunction) then
        begin
          //Read all reservoirs names & numbers
          for LCount := 1 to LCountObject.FReservoirCount.FData do
          begin
            LElementObject := LElementListObject.AddElement(pltelReservoir);
            if not ReadReservoirElement(LLineNumber,LFile,LElementObject,AFileName,AProgressFunction) then
              Break;
          end;

          //Read all channel names & numbers
          for LCount := 1 to LCountObject.FChannelCount.FData do
          begin
            LElementObject := LElementListObject.AddElement(pltelChannel);
            if not ReadChannelElement(LLineNumber,LFile,LElementObject,AFileName,AProgressFunction) then
              Break;
          end;

          LElementObject := LElementListObject.AddElement(pltelMaster);
          LElementObject.FElementNumber.FData := 0;
          LElementObject.FElementNumber.FInitalised := True;

          LElementObject := LElementListObject.AddElement(pltelTotal);
          LElementObject.FElementNumber.FData := 0;
          LElementObject.FElementNumber.FInitalised := True;

          LIdentifier    := 0;
          LLoadCaseIndex := 1;
          LSequenceCount := LCountObject.FSequenceCount.FData;
          LMonthCount    := LCountObject.FMonthsCount.FData div 12;
          while not Eof(LFile) do
          begin
            for LSequenceIndex := 1 to LSequenceCount do
            begin
              for LMonthIndex := 1 to LMonthCount do
              begin
                for LElementIndex := 1 to LElementListObject.ElementsCount do
                begin
                  LElementObject := LElementListObject.ElementByIndex(LElementIndex-1);
                  if not ReadElementLineData(LLineNumber,LFile,LLineDataObject,LElementObject,AFileName,
                     AProgressFunction,LLoadCaseIndex,LSequenceIndex) then Break;
                  LIdentifier := LIdentifier + 1;
                  LLineDataObject.FIdentifier := LIdentifier;
                  if((LLineNumber mod 50) = 0) then
                  begin
                    AProgressFunction('',ptNone,LStop);
                    if LStop then Break;
                  end;
                end;
              end;
            end;
            if not Eof(LFile) then
              LLoadCaseIndex := LLoadCaseIndex + 1;
          end;
          LCountObject.FLoadCaseCount.FData := LLoadCaseIndex;
          LCountObject.FLoadCaseCount.FInitalised := True;
        end;
      finally
        CloseFile(LFile);
      end;
    finally
      FreeAndNil(LCountObject);
      FreeAndNil(LElementListObject);
      FreeAndNil(LLineDataObject);
    end;

    LMessage := FAppModules.Language.GetString('TPlotFileManager.strFileValidationCompleted');
    LMessage := Format(LMessage,[ExtractFileName(AFileName.FileName)]);
    AProgressFunction(LMessage,ptNone,LStop);
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TPlotFileManager.ImportFile(AFileName: TAbstractModelFileName;AProgressFunction: TProgressUpdateFuntion): boolean;
const OPNAME = 'TPlotFileManager.ImportFile';
var
  LMessage : string;
  LStop: boolean;
  LCountObject:TPlotCountObject;
  LElementObject:TPlotElementObject;
  LElementListObject:TPlotElementListObject;
  LLineDataObject:TPlotLineDataObject;
  LFile : TextFile;
  LMonthCount    : integer;
  LSequenceCount : integer;

  LCount        : integer;
  LLineNumber   : integer;
  LMonthIndex   : integer;
  LSequenceIndex: integer;
  LLoadCaseIndex : integer;
  LElementIndex : integer;
  LDataSet      : TAbstractModelDataset;
  LIdentifier  : integer;
begin
  Result := False;
  try
    //Check if file exists.
    If not FileExists(AFilename.FileName) then
    begin
      Result := True;
      Exit;
    end;
    LMessage := FAppModules.Language.GetString('TPlotFileManager.strFileImportStarted');
    LMessage := Format(LMessage,[ExtractFileName(AFilename.FileName)]);
    AProgressFunction(LMessage,ptNone,LStop);

    if not Assigned(AFileName) then
      raise Exception.Create('Plot file names parameter is not yet assigned.');

    if not Assigned(AProgressFunction) then
      AProgressFunction := DummyShowProgress;

    //Check if file exists.
    If not FileExists(AFilename.FileName) then
    begin
      LMessage := FAppModules.Language.GetString('TPlotFileManager.strFileNoExist');
      LMessage := Format(LMessage,[ExtractFileName(AFileName.FileName)]);
      AProgressFunction(LMessage,ptError,LStop);
      Exit;
    end;

    if  ClearModelDataInDatabase(TFileNameObject(AFileName),AProgressFunction,True) then
    begin
      LCountObject      := TPlotCountObject.Create;
      LElementListObject := TPlotElementListObject.Create;
      LLineDataObject    := TPlotLineDataObject.Create;
      FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
      try
        LCountObject.Initialise;
        LElementListObject.Initialise;
        LLineDataObject.Initialise;
        AssignFile(LFile, AFileName.FileName);
        try
          Reset(LFile);
          LLineNumber := 0;
          //Line 1 & 2 (Counters);
          if ReadCounters(LLineNumber,LFile,LCountObject,AFileName,AProgressFunction) then
          begin

            //Read all reservoirs names & numbers
            for LCount := 1 to LCountObject.FReservoirCount.FData do
            begin
              LElementObject := LElementListObject.AddElement(pltelReservoir);
              if not ReadReservoirElement(LLineNumber,LFile,LElementObject,AFileName,AProgressFunction) then
               Break;
            end;

            //Read all channel names & numbers
            for LCount := 1 to LCountObject.FChannelCount.FData do
            begin
              LElementObject := LElementListObject.AddElement(pltelChannel);
              if not ReadChannelElement(LLineNumber,LFile,LElementObject,AFileName,AProgressFunction) then
               Break;
            end;

            LElementObject := LElementListObject.AddElement(pltelMaster);
            LElementObject.FElementNumber.FData := 0;
            LElementObject.FElementNumber.FInitalised := True;

            LElementObject := LElementListObject.AddElement(pltelTotal);
            LElementObject.FElementNumber.FData := 0;
            LElementObject.FElementNumber.FInitalised := True;

            if WriteElementsToDB(LDataSet,LElementListObject,AProgressFunction) then
            begin
              LIdentifier    := 0;
              LLoadCaseIndex := 1;
              LSequenceCount := LCountObject.FSequenceCount.FData;
              LMonthCount    := LCountObject.FMonthsCount.FData div 12;
              while not Eof(LFile) do
              begin
                for LSequenceIndex := 1 to LSequenceCount do
                begin
                  for LMonthIndex := 1 to LMonthCount do
                  begin
                    for LElementIndex := 1 to LElementListObject.ElementsCount do
                    begin
                      LElementObject := LElementListObject.ElementByIndex(LElementIndex-1);
                      if not ReadElementLineData(LLineNumber,LFile,LLineDataObject,LElementObject,AFileName,
                         AProgressFunction,LLoadCaseIndex,LSequenceIndex) then
                         Break;
                      LIdentifier := LIdentifier + 1;
                      LLineDataObject.FIdentifier := LIdentifier;
                      if not WriteElementDataToDB(LDataSet,LLineDataObject,AProgressFunction) then
                        Break;
                      if((LLineNumber mod 50) = 0) then
                      begin
                        AProgressFunction('',ptNone,LStop);
                        if LStop then Break;
                      end;
                    end;
                  end;
                end;
                if not Eof(LFile) then
                  LLoadCaseIndex := LLoadCaseIndex + 1;
              end;
              LCountObject.FLoadCaseCount.FData := LLoadCaseIndex;
              LCountObject.FLoadCaseCount.FInitalised := True;
              WriteCountersToDB(LDataSet,LCountObject,AProgressFunction);
            end;
          end;
        finally
          CloseFile(LFile);
        end;
        Result := InsertFileName(TFileNameObject(AFileName));
      finally
        FreeAndNil(LCountObject);
        FreeAndNil(LElementListObject);
        FreeAndNil(LLineDataObject);
        LDataSet.Free;
      end;
    end;
    LMessage := FAppModules.Language.GetString('TPlotFileManager.strFileValidationCompleted');
    LMessage := Format(LMessage,[ExtractFileName(AFileName.FileName)]);
    AProgressFunction(LMessage,ptNone,LStop);
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TPlotFileManager.ExportFile(AFileName: TAbstractModelFileName;AProgressFunction: TProgressUpdateFuntion): boolean;
const OPNAME = 'TPlotFileManager.ExportFile';
var
  LSQL,
  LMessage : string;
  LOutString     : string;
  LTempString    : string;
  LStop: boolean;
  LCountObject:TPlotCountObject;
  LElementObject:TPlotElementObject;
  LElementListObject:TPlotElementListObject;
  LLineDataObject:TPlotLineDataObject;
  LFile : TextFile;

  LCount        : integer;
  LLineNumber   : integer;
  LDataSet      : TAbstractModelDataset;
begin
  Result := False;
  try
    LMessage := FAppModules.Language.GetString('TPlotFileManager.strFileExportStarted');
    LMessage := Format(LMessage,[ExtractFileName(AFilename.FileName)]);
    AProgressFunction(LMessage,ptNone,LStop);

    if not Assigned(AFileName) then
      raise Exception.Create('Plot file names parameter is not yet assigned.');

    if not Assigned(AProgressFunction) then
      AProgressFunction := DummyShowProgress;

    LCountObject      := TPlotCountObject.Create;
    LElementListObject := TPlotElementListObject.Create;
    LLineDataObject    := TPlotLineDataObject.Create;
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      LCountObject.Initialise;
      LElementListObject.Initialise;
      LLineDataObject.Initialise;
      //Line 1 & 2 (Counters);
      if ReadCountersFromDB(LDataSet,LCountObject,AProgressFunction) and
        (LCountObject.FReservoirCount.FData > 0) then
      begin
        AssignFile(LFile, AFileName.FileName);
        try
          Rewrite(LFile);
          LOutString  := '';
          LTempString := PadInt(LCountObject.FReservoirCount);
          LOutString  := LOutString + LTempString;
          LTempString := PadInt(LCountObject.FSequenceCount);
          LOutString  := LOutString + LTempString;
          LTempString := PadInt(LCountObject.FMonthsCount);
          LOutString  := LOutString + LTempString;
          LTempString := PadInt(LCountObject.FChannelCount);
          LOutString  := LOutString + LTempString;
          Writeln(LFile,LOutString);

          if(LCountObject.FComment.FData <> '') then
           LOutString := LOutString + LCountObject.FComment.FData;
          Writeln(LFile,LOutString);

          if ReadElementsFromDB(LDataSet,LElementListObject,AProgressFunction) and
             (LElementListObject.ElementsCount > 0) then
          begin
            //Read all channel names & numbers
            for LCount := 1 to LElementListObject.ElementsCount-2 do
            begin
              LElementObject := LElementListObject.ElementByIndex(LCount-1);
              LOutString  := '';
              LTempString := PadString(LElementObject.FElementName);
              LOutString  := LOutString + LTempString;
              LTempString := PadInt(LElementObject.FElementNumber);
              LOutString  := LOutString + LTempString;
              Writeln(LFile,LOutString);
            end;
          end;

          LSQL := 'SELECT Model,StudyAreaName,SubArea,Scenario,Identifier,LoadCaseNumber,SequenceNumber,ElementType,ElementNumber,'+
              ' Value01,Value02,Value03,Value04,Value05,Value06,Value07,Value08,Value09,Value10,Value11,Value12'+
              ' FROM pltFileData'+
              ' WHERE  (Model            ='+ QuotedStr(FAppModules.StudyArea.ModelCode)    +') AND'+
              '        (StudyAreaName    ='+QuotedStr(FAppModules.StudyArea.StudyAreaCode) +') AND'+
              '        (SubArea          ='+QuotedStr(FAppModules.StudyArea.SubAreaCode)   +') AND'+
              '        (Scenario         ='+QuotedStr(FAppModules.StudyArea.ScenarioCode)  +')'+
              ' ORDER BY Model,StudyAreaName,SubArea,Scenario,Identifier';
          LDataSet.DataSet.Close;
          LDataSet.SetSQL(LSQL);
          LDataSet.DataSet.Open;
          LLineNumber := 0;
          while not LDataSet.DataSet.Eof do
          begin
            if ReadElementDataFromDB(LDataSet,LLineDataObject,AProgressFunction) then
            begin
              LOutString  := '';
              for LCount := MinMonths to MaxMonths do
              begin
                LTempString := PadDouble(LLineDataObject.FMonthlyValues[LCount]);
                LOutString  := LOutString + LTempString;
              end;
              Writeln(LFile,LOutString);

              LLineNumber := LLineNumber + 1;
              if((LLineNumber mod 50) = 0) then
              begin
                AProgressFunction('',ptNone,LStop);
                if LStop then Break;
              end;
            end;
            LDataSet.DataSet.Next;
          end;
        finally
          CloseFile(LFile);
        end;
      end;
    finally
      FreeAndNil(LCountObject);
      FreeAndNil(LElementListObject);
      FreeAndNil(LLineDataObject);
      LDataSet.Free;
    end;

    LMessage := FAppModules.Language.GetString('TPlotFileManager.strFileExportCompleted');
    LMessage := Format(LMessage,[ExtractFileName(AFileName.FileName)]);
    AProgressFunction(LMessage,ptNone,LStop);
    Result := True;

  except on E: Exception do HandleError(E, OPNAME) end;
end;


function TPlotFileManager.ClearModelDataInDatabase(AFileName: TFileNameObject; AProgressFunction: TProgressUpdateFuntion;
         AQuetly: boolean): boolean;
const OPNAME = 'TPlotFileManager.ClearModelDataInDatabase';
var
  LTableNames,
  LMessage: string;
  LStop: boolean;
begin
  Result := False;
  try
    if not Assigned(AProgressFunction) then
      AProgressFunction := DummyShowProgress;

    if not AQuetly then
    begin
      LMessage := FAppModules.Language.GetString('TAbstractDatabaseAgent.strClearModelDataInDatabaseStarted');
      AProgressFunction(LMessage,ptNone,LStop);
    end;

    if not Assigned(AFileName) then
      raise Exception.Create('File name object parameter is not yet assigned.');

    LTableNames := 'pltElement,pltElementCount,pltFileData';
    Result := DeleteModelData(LTableNames,'',AProgressFunction,AQuetly);
    Result := Result and DeleteFileName(AFileName);


    if Result and (not AQuetly)then
    begin
      LMessage := FAppModules.Language.GetString('TAbstractDatabaseAgent.strClearModelDataInDatabaseEnded');
      AProgressFunction(LMessage,ptNone,LStop);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TPlotFileManager.ReadModelDataFromDatabase(AFileName: TFileNameObject; ADataObject: TDataFileObjects;
         AProgressFunction: TProgressUpdateFuntion): boolean;
const OPNAME = 'TPlotFileManager.ReadModelDataFromDatabase';
begin
   Result := False;
   try
     raise Exception.Create('You sould never call this function as it does nothing.');
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TPlotFileManager.WriteModelDataToDatabase(AFileName: TFileNameObject; ADataObject: TDataFileObjects;
         AProgressFunction: TProgressUpdateFuntion): boolean;
const OPNAME = 'TPlotFileManager.WriteModelDataToDatabase';
begin
   Result := False;
  try
     raise Exception.Create('You sould never call this function as it does nothing.');
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
