//
//  Unit      : TFileCurtailDatabaseAgent
//  AUTHOR    : Phatedi Lethabo
//  DATE      :
//  COPYRIGHT : Copyright © 2016 DWS
//
unit UFileCurtailDatabaseAgent;

interface

uses
  Classes, sysutils,Db,

  VoaimsCom_TLB,
  UFileNames,
  UConstants,
  UDatabaseUtilities,
  UAbstractObject,
  UDataFileObjects,
  UAbstractFileNamesObject,
  UCurtailObject,
  UAbstractDatabaseAgent,
  UPlanningFileDataObjects,
  UYieldModelDataObject;

type

  TFileCurtailDatabaseAgent = class(TAbstractDatabaseAgent)
  protected
  function ReadCurtailKnownDataSQL: string;
  function ReadFFileCurtailUnkownDataSQL: string;
  function WriteCurtailUnknownDataSQL: string;
  function WriteNoOfChannelMonthDataSQL: string;
  function WriteChannelElevationDataSQL: string;
  function WriteFactorDataSQL: string;

  public
  { Public Declarations }

  function ReadModelDataFromDatabase(AFileName:TFileNameObject; ADataObject: TDataFileObjects;
             AProgressFunction: TProgressUpdateFuntion): boolean; override;
  function WriteModelDataToDatabase(AFileName:TFileNameObject;ADataObject: TDataFileObjects;
           AProgressFunction: TProgressUpdateFuntion): boolean; override;
  function ClearModelDataInDatabase(AFileName:TFileNameObject;AProgressFunction: TProgressUpdateFuntion;
             AQuetly: boolean = False): boolean; override;
end;

implementation

uses UUtilities,
     UDataSetType,
     UErrorHandlingOperations;

function TFileCurtailDatabaseAgent.ReadCurtailKnownDataSQL: string;
const OPNAME = 'TFileCurtailDatabaseAgent.ReadCurtailKnownDataSQL';
begin

  Result := '';
  try
    Result := 'SELECT'+
      '  MultiResChannelCurtail.ChannelNo,MultiResChannelCurtail.ReservoirNo,MultiResChannelCurtail.Identifier'+
      ' ,DecisionMonth,StartMonth'+
      ' ,Elevation01,Elevation02,Elevation03,Elevation04,Elevation05,Elevation06,Elevation07,Elevation08,Elevation09,Elevation10'+
      ' ,Factor01,Factor02,Factor03,Factor04,Factor05,Factor06,Factor07,Factor08,Factor09,Factor10'+
      '  FROM '+
      '  MultiResChannelCurtail'+
      ' ,MultiResChannelElevation'+
      ' ,MultiResChannelFactor'+
      ' WHERE MultiResChannelCurtail.Model            =  '+QuotedStr(FAppModules.StudyArea.ModelCode)+
      ' AND MultiResChannelCurtail.StudyAreaName      =  '+QuotedStr(FAppModules.StudyArea.StudyAreaCode)+
      ' AND MultiResChannelCurtail.SubArea            =  '+QuotedStr(FAppModules.StudyArea.SubAreaCode)+
      ' AND MultiResChannelCurtail.Scenario           =  '+QuotedStr(FAppModules.StudyArea.ScenarioCode)+
      ' AND MultiResChannelCurtail.ChannelNo          =  MultiResChannelElevation.ChannelNo'+
      ' AND MultiResChannelCurtail.ReservoirNo        =  MultiResChannelElevation.ReservoirNo'+
      ' AND MultiResChannelCurtail.Identifier         =  MultiResChannelElevation.Identifier'+
      ' AND MultiResChannelElevation.Model         = MultiResChannelCurtail.Model'+
      ' AND MultiResChannelElevation.StudyAreaName = MultiResChannelCurtail.StudyAreaName'+
      ' AND MultiResChannelElevation.SubArea       = MultiResChannelCurtail.SubArea'+
      ' AND MultiResChannelElevation.Scenario      = MultiResChannelCurtail.Scenario'+
      ' AND MultiResChannelElevation.ChannelNo          = MultiResChannelFactor.ChannelNo'+
      ' AND MultiResChannelElevation.ReservoirNo        = MultiResChannelFactor.ReservoirNo'+
      ' AND MultiResChannelElevation.Identifier         = MultiResChannelFactor.Identifier'+
      ' AND MultiResChannelFactor.Model         = MultiResChannelCurtail.Model'+
      ' AND MultiResChannelFactor.StudyAreaName = MultiResChannelCurtail.StudyAreaName'+
      ' AND MultiResChannelFactor.SubArea       = MultiResChannelCurtail.SubArea '+
      ' AND MultiResChannelFactor.Scenario      = MultiResChannelCurtail.Scenario';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFileCurtailDatabaseAgent.ReadFFileCurtailUnkownDataSQL: string;
const OPNAME = 'TFileCurtailDatabaseAgent.ReadFFileCurtailUnkownDataSQL';
begin

  Result := '';
  try
    Result := 'SELECT Model,StudyAreaName,SubArea,Scenario,LineNumber,LineData  '+
              ' FROM WRYMFileLines'+
              ' WHERE Model       =  '+QuotedStr(FAppModules.StudyArea.ModelCode)+
              ' AND StudyAreaName =  '+QuotedStr(FAppModules.StudyArea.StudyAreaCode)+
              ' AND SubArea       =  '+QuotedStr(FAppModules.StudyArea.SubAreaCode)+
              ' AND Scenario      =  '+QuotedStr(FAppModules.StudyArea.ScenarioCode)+
              ' AND FileGroup     = :FileGroup' +
              ' AND FileType      = :FileType'+
              ' ORDER BY Model,StudyAreaName,SubArea,Scenario,LineNumber,LineSection';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFileCurtailDatabaseAgent.WriteCurtailUnknownDataSQL: string;
const OPNAME = 'TFileCurtailDatabaseAgent.WriteCurtailUnknownDataSQL';
begin
  Result := '';
  try
    Result := 'INSERT INTO WRYMFileLines'+
              ' (Model,StudyAreaName,SubArea,Scenario,FileType,FileGroup,LineNumber,LineSection,LineData)'+
              ' Values(:Model,:StudyAreaName,:SubArea,:Scenario,:FileType,:FileGroup,:LineNumber,:LineSection,:LineData)';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFileCurtailDatabaseAgent.WriteNoOfChannelMonthDataSQL: string;
const OPNAME = 'TFileCurtailDatabaseAgent.WriteNoOfChannelMonthDataSQL';
begin
  Result := '';
  try
    Result := 'INSERT INTO MultiResChannelCurtail (Model,StudyAreaName,SubArea,Scenario,Identifier,'+
              ' ChannelNo,ReservoirNo,DecisionMonth,StartMonth)'+
              ' Values(:Model,:StudyAreaName,:SubArea,:Scenario,:Identifier,'+
              ' :ChannelNo,:ReservoirNo,:DecisionMonth, :StartMonth)';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFileCurtailDatabaseAgent.WriteChannelElevationDataSQL: string;
const OPNAME = 'TFileCurtailDatabaseAgent.WriteNoOfChannelMonthDataSQL';
begin
  Result := '';
  try
    Result := 'INSERT INTO MultiResChannelElevation (Model,StudyAreaName,SubArea,Scenario,ChannelNo,ReservoirNo,Identifier,'+
              ' Elevation01,Elevation02,Elevation03,Elevation04,Elevation05,Elevation06,Elevation07,Elevation08,Elevation09,Elevation10)'+
              ' Values(:Model,:StudyAreaName,:SubArea,:Scenario,:ChannelNo,:ReservoirNo,:Identifier,'+
              ' :Elevation01, :Elevation02, :Elevation03, :Elevation04, :Elevation05, :Elevation06, :Elevation07, :Elevation08, :Elevation09, :Elevation10)';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFileCurtailDatabaseAgent.WriteFactorDataSQL: string;
const OPNAME = 'TFileCurtailDatabaseAgent.WriteNoOfChannelMonthDataSQL';
begin
  Result := '';
  try
    Result := 'INSERT INTO MultiResChannelFactor (Model,StudyAreaName,SubArea,Scenario,ChannelNo,ReservoirNo,Identifier,'+
    ' Factor01,Factor02,Factor03,Factor04,Factor05,Factor06,Factor07,Factor08,Factor09,Factor10)'+
    ' Values(:Model,:StudyAreaName,:SubArea,:Scenario,:ChannelNo,:ReservoirNo,:Identifier,'+
    ' :Factor01, :Factor02, :Factor03, :Factor04, :Factor05, :Factor06, :Factor07, :Factor08, :Factor09, :Factor10)';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFileCurtailDatabaseAgent.ReadModelDataFromDatabase(AFileName:TFileNameObject; ADataObject: TDataFileObjects;
         AProgressFunction: TProgressUpdateFuntion): boolean;
const OPNAME = 'TFileCurtailDatabaseAgent.ReadModelDataFromDatabase';
var
  LCount         : integer;
  LFieldName,
  LMessage       : string;
  LDataSet       : TAbstractModelDataset;
  LCurtail       : TCurtailDataObject;
  LCurtailObj    : TCurtailObject;

  LStop          : boolean;
begin
  Result := False;
  try
    if not Assigned(AProgressFunction) then
      AProgressFunction := DummyShowProgress;

    LMessage := FAppModules.Language.GetString('TFileCurtailDatabaseAgent.strReadStarted');
    AProgressFunction(LMessage,ptNone,LStop);

    if not Assigned(ADataObject) then
      raise Exception.Create('File object parameter is not yet assigned.');

    LCurtail := TPlanningFileDataObjects(ADataObject).CurtailDataObject;
    LCurtailObj := LCurtail.AddCurtailData;

    if not LCurtail.Initialise then
      Exit;

    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try

      LDataSet.SetSQL(ReadCurtailKnownDataSQL);
      LDataSet.DataSet.Open;

      //Check if there is any data.
      if (LDataSet.DataSet.RecordCount = 0) then
      begin
        LMessage := FAppModules.Language.GetString('TFileCurtailDatabaseAgent.strNoDataReturned');
        AProgressFunction(LMessage,ptWarning,LStop);
        Exit;
      end;

      LCurtail.FNumberOfChannel.FData := LDataSet.DataSet.RecordCount;
      LCurtail.FNumberOfChannel.FInitalised := True;

      while not LDataSet.DataSet.Eof do
      begin

        LCurtailObj := LCurtail.AddCurtailData;
        LCurtailObj.Initialise;

        if not LDataSet.DataSet.FieldByName('ChannelNo').IsNull then
        begin
          LCurtailObj.FChannelNumber.FData := LDataSet.DataSet.FieldByName('ChannelNo').AsInteger;
          LCurtailObj.FChannelNumber.FInitalised := True;
        end;
        if not LDataSet.DataSet.FieldByName('ReservoirNo').IsNull then
        begin
          LCurtailObj.FReservoirNumber.FData := LDataSet.DataSet.FieldByName('ReservoirNo').AsInteger;
          LCurtailObj.FReservoirNumber.FInitalised := True;
        end;
        if not LDataSet.DataSet.FieldByName('DecisionMonth').IsNull then
        begin
          LCurtailObj.FDecisionMonth.FData := LDataSet.DataSet.FieldByName('DecisionMonth').AsInteger;
          LCurtailObj.FDecisionMonth.FInitalised := True;
        end;
        if not LDataSet.DataSet.FieldByName('StartMonth').IsNull then
        begin
          LCurtailObj.FStartMonth.FData := LDataSet.DataSet.FieldByName('StartMonth').AsInteger;
          LCurtailObj.FStartMonth.FInitalised := True;
        end;

        //Line 3 Reservior Elevation Restriction

        for LCount := 1 to 10 do
        begin
          LFieldName := Format('Elevation%2.2d',[LCount]);
          if not LDataSet.DataSet.FieldByName(LFieldName).IsNull then
          begin
            LCurtailObj.FReservoirElevation[LCount].FData := LDataSet.DataSet.FieldByName(LFieldName).AsFloat;
            LCurtailObj.FReservoirElevation[LCount].FInitalised := True;
          end;
        end;

        //Line 4 Start Month

        for LCount := 1 to 10 do
        begin
          LFieldName := Format('Factor%2.2d',[LCount]);
          if not LDataSet.DataSet.FieldByName(LFieldName).IsNull then
          begin
            LCurtailObj.FMultiplicationRestriction[LCount].FData := LDataSet.DataSet.FieldByName(LFieldName).AsFloat;
            LCurtailObj.FMultiplicationRestriction[LCount].FInitalised := True;
          end;
        end;

        LDataSet.DataSet.Next;

      end;

      LDataSet.DataSet.Close;
      LDataSet.SetSQL(ReadFFileCurtailUnkownDataSQL);
      LDataSet.SetParams(['FileType'], [IntToStr(AFileName.FileNumber)]);
      LDataSet.SetParams(['FileGroup'], [IntToStr(AFileName.FileGroup)]);
      LDataSet.DataSet.Open;
      //Read FFileCurtailExtraLines

      while not LDataSet.DataSet.EOF do
      begin
        LCurtailObj.FFileCurtailExtraLines.Add(TrimRight(LDataSet.DataSet.FieldByName('LineData').AsString));
        LDataSet.DataSet.Next;
      end;
      LDataSet.DataSet.Close;

     LMessage := FAppModules.Language.GetString('TFileCurtailDatabaseAgent.strReadEnded');
     AProgressFunction(LMessage,ptNone,LStop);
     Result :=  True;

     finally
      LDataSet.Free;
     end;
    except on E: Exception do HandleError(E, OPNAME) end;
   end;

function TFileCurtailDatabaseAgent.WriteModelDataToDatabase(AFileName:TFileNameObject;ADataObject: TDataFileObjects;
         AProgressFunction: TProgressUpdateFuntion): boolean;
const OPNAME = 'TFileCurtailDatabaseAgent.WriteModelDataToDatabase';
var
  LMessage       : string;
  LDataSet       : TAbstractModelDataset;
  LCurtail       : TCurtailDataObject;
  LCurtailObj    : TCurtailObject;
  LStop          : boolean;
  LIndex         : Integer;
  LField         : string;
 // LFieldValue    : string;
  LCount         : integer;
begin
  Result := False;
  try
    if not Assigned(AProgressFunction) then
      AProgressFunction := DummyShowProgress;

    LMessage := FAppModules.Language.GetString('TFileCurtailDatabaseAgent.strWriteStarted');
    AProgressFunction(LMessage,ptNone,LStop);

    if not Assigned(ADataObject) then
      raise Exception.Create('File object parameter is not yet assigned.');

    if not ClearModelDataInDatabase(TFileNameObject(AFileName),AProgressFunction,True) then
      Exit;

    LCurtail := TPlanningFileDataObjects(ADataObject).CurtailDataObject;
    LCurtailObj := LCurtail.AddCurtailData;

    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try

      //Channel Number Line 2

     for LIndex := 0 to LCurtail.FNumberOfChannel.FData -1 do
     begin

      LCurtailObj := LCurtail.GetCurtailObjectByIndex(LIndex);

      LDataSet.DataSet.Close;
      LDataSet.SetSQL(WriteNoOfChannelMonthDataSQL);

      LDataset.ClearQueryParams();
      LDataSet.SetParams(['Model'], [FAppModules.StudyArea.ModelCode]);
      LDataSet.SetParams(['StudyAreaName'], [FAppModules.StudyArea.StudyAreaCode]);
      LDataSet.SetParams(['SubArea'], [FAppModules.StudyArea.SubAreaCode]);
      LDataSet.SetParams(['Scenario'], [FAppModules.StudyArea.ScenarioCode]);
      LDataSet.SetParams(['Identifier'], [IntToStr(LIndex+1)]);

      if LCurtailObj.FChannelNumber.FInitalised then
         LDataSet.SetParams(['ChannelNo'], [IntToStr(LCurtailObj.FChannelNumber.FData)]);

      if LCurtailObj.FReservoirNumber.FInitalised then
         LDataSet.SetParams(['ReservoirNo'], [IntToStr(LCurtailObj.FReservoirNumber.FData)]);

      if LCurtailObj.FDecisionMonth.FInitalised then
         LDataSet.SetParams(['DecisionMonth'], [IntToStr(LCurtailObj.FDecisionMonth.FData)]);

      if LCurtailObj.FStartMonth.FInitalised then
         LDataSet.SetParams(['StartMonth'], [IntToStr(LCurtailObj.FStartMonth.FData)]);

      LDataSet.ExecSQL;

      //Elevation Restrictions

      LDataSet.DataSet.Close;
      LDataSet.SetSQL(WriteChannelElevationDataSQL);
      LDataset.ClearQueryParams();
      LDataSet.SetParams(['Model'], [FAppModules.StudyArea.ModelCode]);
      LDataSet.SetParams(['StudyAreaName'], [FAppModules.StudyArea.StudyAreaCode]);
      LDataSet.SetParams(['SubArea'], [FAppModules.StudyArea.SubAreaCode]);
      LDataSet.SetParams(['Scenario'], [FAppModules.StudyArea.ScenarioCode]);
      LDataSet.SetParams(['ChannelNo'], [IntToStr(LCurtailObj.FChannelNumber.FData)]);
      LDataSet.SetParams(['ReservoirNo'],[IntTostr(LCurtailObj.FReservoirNumber.FData)]);
      LDataSet.SetParams(['Identifier'], [IntToStr(LIndex+1)]);

      for LCount := 1 to 10 do
      begin
        if LCurtailObj.FReservoirElevation[LCount].FInitalised then
        begin
          LField :=  Format('Elevation%2.2d',[LCount]);
          LDataSet.SetParams([LField],[FloatToStr(LCurtailObj.FReservoirElevation[LCount].FData)]);
        end;
      end;

      LDataSet.ExecSQL;

      // Multiplication Restriction Factors

      LDataSet.DataSet.Close;
      LDataSet.SetSQL(WriteFactorDataSQL);
      LDataset.ClearQueryParams();
      LDataSet.SetParams(['Model'], [FAppModules.StudyArea.ModelCode]);
      LDataSet.SetParams(['StudyAreaName'], [FAppModules.StudyArea.StudyAreaCode]);
      LDataSet.SetParams(['SubArea'], [FAppModules.StudyArea.SubAreaCode]);
      LDataSet.SetParams(['Scenario'], [FAppModules.StudyArea.ScenarioCode]);
      LDataSet.SetParams(['ChannelNo'], [IntToStr(LCurtailObj.FChannelNumber.FData)]);
      LDataSet.SetParams(['ReservoirNo'], [IntToStr(LCurtailObj.FReservoirNumber.FData)]);
      LDataSet.SetParams(['Identifier'], [IntToStr(LIndex+1)]);

      for LCount := 1 to 10 do
      begin
        if LCurtailObj.FMultiplicationRestriction[LCount].FInitalised then
        begin
          LField :=  Format('Factor%2.2d',[LCount]);
          LDataSet.SetParams([LField],[FloatToStr(LCurtailObj.FMultiplicationRestriction[LCount].FData)]);
        end;
      end;

         LDataSet.ExecSQL;

        LDataSet.DataSet.Close;
      //Index Loop end
     end;
     //Write FFileCurtailExtraLines

     for LCount := 0 to LCurtailObj.FFileCurtailExtraLines.Count - 1 do
      begin
        LDataSet.DataSet.Close;
        LDataSet.SetSQL(WriteCurtailUnknownDataSQL);
        LDataset.ClearQueryParams();
        LDataSet.SetParams(['Model'], [FAppModules.StudyArea.ModelCode]);
        LDataSet.SetParams(['StudyAreaName'], [FAppModules.StudyArea.StudyAreaCode]);
        LDataSet.SetParams(['SubArea'], [FAppModules.StudyArea.SubAreaCode]);
        LDataSet.SetParams(['Scenario'], [FAppModules.StudyArea.ScenarioCode]);
        LDataSet.SetParams(['FileType'], [IntToStr(AFileName.FileNumber)]);
        LDataSet.SetParams(['FileGroup'], [IntToStr(AFileName.FileGroup)]);
        LDataSet.SetParams(['LineNumber'], [IntToStr(11+LCount)]);
        LDataSet.SetParams(['LineSection'], [IntToStr(0)]);
        LDataSet.SetParams(['LineData'], [LCurtailObj.FFileCurtailExtraLines[LCount]]);

        LDataSet.ExecSQL;
      end;
       LDataSet.DataSet.Close;

         Result := InsertFileName(TFileNameObject(AFileName));
         if Result then
      begin
        LMessage := FAppModules.Language.GetString('TFileCurtailDatabaseAgent.strWriteEnded');
        AProgressFunction(LMessage,ptNone,LStop);
      end;
      finally
        LDataSet.Free;
      end;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFileCurtailDatabaseAgent.ClearModelDataInDatabase(AFileName: TFileNameObject; AProgressFunction: TProgressUpdateFuntion;
          AQuetly: boolean = False): boolean;
const OPNAME = 'TFileCurtailDatabaseAgent.ClearModelDataInDatabase';
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

    LTableNames := 'MultiResChannelCurtail,MultiResChannelElevation,MultiResChannelFactor';
    if(FAppModules.Model.ModelName = CPlanning) then
      LTableNames := LTableNames + ',MultiResChannelCurtail,MultiResChannelElevation,MultiResChannelFactor';
    Result := DeleteModelData(LTableNames,'',AProgressFunction,AQuetly);
    Result := Result and DeleteUnknownModelData(AFileName,AProgressFunction,AQuetly);
    Result := Result and DeleteFileName(AFileName);


    if Result and (not AQuetly)then
    begin
      LMessage := FAppModules.Language.GetString('TAbstractDatabaseAgent.strClearModelDataInDatabaseEnded');
      AProgressFunction(LMessage,ptNone,LStop);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.


