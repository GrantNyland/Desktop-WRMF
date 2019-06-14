//
//
//  UNIT      : Contains  TGrowthFactorDataSQLAgent   Class
//  AUTHOR    : Sam Dhlamini(Cornastone)
//  DATE      : 11/05/2006
//  COPYRIGHT : Copyright © 2004 DWAF
//
//

unit UGrowthFactorDataSQLAgent;

interface
uses
  Classes,
  Contnrs,
  VoaimsCom_TLB,
  UAbstractObject;

type
  TGrowthFactorDataSQLAgent = class(TAbstractSQLAgent)
  protected
    function GetScenarioWhereClause: string;
    function GetLastDemandCentreIdentifier : integer;
    function GetLastMinMaxChannelIdentifier : integer;
    function GetLastHydrologyIdentifier : integer;
  public
    procedure LoadContextData(AContextData : TStringList;AIdentifier: string);
    procedure LoadConfigContextData(AContextData : TStringList);

    function InsertDemandCentreGrowthFactorsSQL : string;
    function InsertMinMaxChannelGrowthFactorsSQL : string;
    function InsertHydrologyGrowthFactorsSQL : string;
    function InsertDemandCentreGrowthFactors(var AIdentifier,AChannelNumber : integer) : boolean;
    function InsertMinMaxChannelGrowthFactors(var AIdentifier,AChannelNumber : integer) : boolean;
    function InsertHydrologyGrowthFactors(var AIdentifier: integer) : boolean;

    function InsertDemandCentreGrowthProjectionSQL : string;
    function InsertMinMaxChannelGrowthProjectionSQL : string;
    function InsertHydrologyGrowthProjectionSQL : string;
    function InsertDemandCentreGrowthProjection(var AIdentifier : integer) : boolean;
    function InsertMinMaxChannelGrowthProjection(var AIdentifier : integer) : boolean;
    function InsertHydrologyGrowthProjection(var AIdentifier : integer) : boolean;

    function DeleteDemandGrowthFactorByChannelSQL(AChannelNumber : integer) : string;
    function DeleteMinMaxChannelGrowthFactorByChannelSQL(AMinMaxChannel:integer) : string;
    function DeleteHydrologyGrowthFactorsByGaugeSQL(AGaugeNumber:integer) : string;
    function DeleteProjectionMinMaxChannelSQL(AMinMaxChannel:integer) : string;

    function DeleteAllGrowthFactorDemand: boolean;
    function DeleteAllGrowthFactorDemandSQL: string;
    function DeleteAllGrowthFactorHydrology: boolean;
    function DeleteAllGrowthFactorHydrologySQL: string;
    function DeleteAllGrowthFactorMinMax: boolean;
    function DeleteAllGrowthFactorMinMaxSQL: string;
    function DeleteAllGrowthFactorConfig: boolean;
    function DeleteAllGrowthFactorConfigSQL: string;

    function DeleteAllGrowthProjectionDemand: boolean;
    function DeleteAllGrowthProjectionDemandSQL: string;
    function DeleteAllGrowthProjectionHydrology: boolean;
    function DeleteAllGrowthProjectionHydrologySQL: string;
    function DeleteAllGrowthProjectionMinMax: boolean;
    function DeleteAllGrowthProjectionMinMaxSQL: string;
    function DeleteAllGrowthProjectionConfig: boolean;
    function DeleteAllGrowthProjectionConfigSQL: string;


    function AddDemandGrowthFactorsToDB(AGrowthFactors: TObjectlist) : boolean;
    function AddDemandGrowthFactorsToDBSQL : string;
    function AddMinMaxChannelGrowthFactorsToDB(AGrowthFactors: TObjectlist) : boolean;
    function AddMinMaxChannelGrowthFactorsToDBSQL : string;
    function AddHydrologyGrowthFactorsToDB(AGrowthFactors: TObjectlist) : boolean;
    function AddHydrologyGrowthFactorsToDBSQL : string;

    function AddGrowthFactorsConfigDataToDB(AYearsCount: Integer) :boolean;
    function AddGrowthProjectionConfDataToDB(ABaseYear, AStartYear,AYearsCount,ADataStartYear : Integer): boolean;

    function AddDemandGrowthProjectionsToDB(AGrowthFactors: TObjectlist) : boolean;
    function AddDemandGrowthProjectionsToDBSQL : string;
    function AddMinMaxChannelGrowthProjectionsToDB(AGrowthFactors: TObjectlist) : boolean;
    function AddMinMaxChannelGrowthProjectionsToDBSQL : string;
    function AddHydrologyGrowthProjectionsToDB(AGrowthFactors: TObjectlist) : boolean;
    function AddHydrologyGrowthProjectionsToDBSQL : string;

    function DeleteDemandGrowthFactorByChannel(AChannelNumber : integer) : boolean;
    function DeleteMinMaxChannelGrowthFactorByChannel(AMinMaxChannel:integer):boolean;
    function DeleteHydrologyGrowthFactorsByGauge(AGaugeNumber: integer):boolean;
    function DeleteProjectionMinMaxChannel(AMinMaxChannel:integer):boolean;

    function GetGrowthFactorConfigSQL : string;
    function GetGrowthFactorDemandSQL : string;
    function GetGrowthFactorHydrologySQL : string;
    function GetGrowthFactorMinMaxSQL : string;
  

    function GetLastDemandCentreIdentifierSQL : string;
    function GetLastMinMaxChannelIdentifierSQL : string;
    function GetLastHydrologyIdentifierSQL : string;
  end;

implementation

uses
  Math,
  SysUtils,
  UConstants,
  UDataSetType,
  UGrowthFactorData,
  UGrowthFactorsExcelData,
  UErrorHandlingOperations;

{ TGrowthFactorDataSQLAgent }

function TGrowthFactorDataSQLAgent.GetGrowthFactorConfigSQL: string;
const OPNAME = 'TGrowthFactorDataSQLAgent.GetGrowthFactorConfigSQL';
begin
  Result := '';
  try
    Result := ' SELECT Model,StudyAreaName,SubArea,Scenario,YearsCount '+
              ' FROM GrowthFactorConfig A WHERE '+
              GetScenarioWhereClause;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TGrowthFactorDataSQLAgent.GetGrowthFactorDemandSQL: string;
const OPNAME = 'TGrowthFactorDataSQLAgent.GetGrowthFactorDemandSQL';
begin
  Result := '';
  try
    Result := ' SELECT * '+
              ' FROM GrowthFactorDemand A WHERE '+
              GetScenarioWhereClause;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TGrowthFactorDataSQLAgent.GetGrowthFactorHydrologySQL: string;
const OPNAME = 'TGrowthFactorDataSQLAgent.GetGrowthFactorHydrologySQL';
begin
  Result := '';
  try
    Result := ' SELECT Model,StudyAreaName,SubArea,Scenario,Identifier,GaugeNumber,'+
              ' AFFFactors,IRRFactors,URBFactors '+
              ' FROM GrowthFactorHydrology A WHERE '+
              GetScenarioWhereClause;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TGrowthFactorDataSQLAgent.GetGrowthFactorMinMaxSQL: string;
const OPNAME = 'TGrowthFactorDataSQLAgent.GetGrowthFactorMinMaxSQL';
begin
  Result := '';
  try
    Result := ' SELECT Model,StudyAreaName,SubArea,Scenario,Identifier,ChannelNumber,ArcNumber, Factors, ValidFactors '+
              ' FROM GrowthFactorMinMax A WHERE '+
              GetScenarioWhereClause;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TGrowthFactorDataSQLAgent.GetLastDemandCentreIdentifierSQL: string;
const OPNAME = 'TGrowthFactorDataSQLAgent.GetLastDemandCentreIdentifierSQL';
begin
  Result := '';
  try
    Result := 'SELECT MAX(A.Identifier) AS LastID FROM GrowthFactorDemand A WHERE ' +
               GetScenarioWhereClause;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TGrowthFactorDataSQLAgent.GetLastHydrologyIdentifierSQL: string;
const OPNAME = 'TGrowthFactorDataSQLAgent.GetLastHydrologyIdentifierSQL';
begin
  Result := '';
  try
    Result := 'SELECT MAX(A.Identifier) AS LastID FROM GrowthFactorHydrology A WHERE ' +
               GetScenarioWhereClause;
  except on E: Exception do HandleError ( E, OPNAME ) end;

end;

function TGrowthFactorDataSQLAgent.GetLastMinMaxChannelIdentifierSQL: string;
const OPNAME = 'TGrowthFactorDataSQLAgent.GetLastMinMaxChannelIdentifierSQL';
begin
  Result := '';
  try
    Result := 'SELECT MAX(A.Identifier) AS LastID FROM GrowthFactorMinMax A WHERE ' +
               GetScenarioWhereClause;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TGrowthFactorDataSQLAgent.GetScenarioWhereClause: string;
const OPNAME = 'TGrowthFactorDataSQLAgent.GetScenarioWhereClause';
begin
  try
    Result :=
      ' (A.Model         = ' + QuotedStr(FAppModules.StudyArea.ModelCode)     + ') AND ' +
      ' (A.StudyAreaName = ' + QuotedStr(FAppModules.StudyArea.StudyAreaCode) + ') AND ' +
      ' (A.SubArea       = ' + QuotedStr(FAppModules.StudyArea.SubAreaCode)   + ') AND ' +
      ' (A.Scenario      = ' + QuotedStr(FAppModules.StudyArea.ScenarioCode)  + ') ';
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TGrowthFactorDataSQLAgent.InsertDemandCentreGrowthFactorsSQL: string;
const OPNAME = 'TGrowthFactorDataSQLAgent.InsertDemandCentreGrowthFactorsSQL';
begin
  Result := '';
  try
    Result := 'INSERT  INTO GrowthFactorDemand ' +
              '(Model, StudyAreaName, SubArea, Scenario, Identifier, ChannelNumber) ' +
              'VALUES (';
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TGrowthFactorDataSQLAgent.InsertHydrologyGrowthFactorsSQL: string;
const OPNAME = 'TGrowthFactorDataSQLAgent.InsertHydrologyGrowthFactorsSQL';
begin
  Result := '';
  try
    Result := 'INSERT  INTO GrowthFactorHydrology ' +
              '(Model, StudyAreaName, SubArea, Scenario, Identifier) ' +
              'VALUES ('
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TGrowthFactorDataSQLAgent.InsertMinMaxChannelGrowthFactorsSQL: string;
const OPNAME = 'TGrowthFactorDataSQLAgent.InsertMinMaxChannelGrowthFactorsSQL';
begin
  Result := '';
  try
    Result := 'INSERT  INTO GrowthFactorMinMax ' +
              '(Model, StudyAreaName, SubArea, Scenario, Identifier, ChannelNumber) ' +
              'VALUES (';
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TGrowthFactorDataSQLAgent.LoadConfigContextData(AContextData: TStringList);
const OPNAME = 'TGrowthFactorDataSQLAgent.LoadConfigContextData';
begin
  try
    AContextData.Clear;
    AContextData.Add('Model=' +FAppModules.StudyArea.ModelCode);
    AContextData.Add('StudyAreaName=' +FAppModules.StudyArea.StudyAreaCode);
    AContextData.Add('SubArea=' +FAppModules.StudyArea.SubAreaCode);
    AContextData.Add('Scenario=' +FAppModules.StudyArea.ScenarioCode);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TGrowthFactorDataSQLAgent.LoadContextData(AContextData: TStringList; AIdentifier: string);
const OPNAME = 'TGrowthFactorDataSQLAgent.LoadContextData';
begin
  try
    AContextData.Clear;
    AContextData.Add('Model='          + FAppModules.StudyArea.ModelCode);
    AContextData.Add('StudyAreaName='  + FAppModules.StudyArea.StudyAreaCode);
    AContextData.Add('SubArea='        + FAppModules.StudyArea.SubAreaCode);
    AContextData.Add('Scenario='       + FAppModules.StudyArea.ScenarioCode);
    AContextData.Add('Identifier='     + AIdentifier);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TGrowthFactorDataSQLAgent.DeleteDemandGrowthFactorByChannelSQL(AChannelNumber : integer) : string;
const OPNAME = 'TGrowthFactorDataSQLAgent.DeleteDemandGrowthFactorByChannelSQL';
begin
  Result := '';
  try
    Result := 'DELETE * FROM GrowthFactorDemand A WHERE ' +
               GetScenarioWhereClause +
               ' AND (A.ChannelNumber = ' + QuotedStr(IntToStr(AChannelNumber))+' )';
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TGrowthFactorDataSQLAgent.DeleteMinMaxChannelGrowthFactorByChannelSQL(AMinMaxChannel:integer) : string;
const OPNAME = 'TGrowthFactorDataSQLAgent.DeleteMinMaxChannelGrowthFactorByChannelSQL';
begin
  Result := '';
  try
    Result := 'DELETE * FROM GrowthFactorMinMax A WHERE ' +
               GetScenarioWhereClause +
               'AND (A.ChannelNumber = ' + IntToStr(AMinMaxChannel)+' )';
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TGrowthFactorDataSQLAgent.DeleteProjectionMinMaxChannelSQL(AMinMaxChannel:integer) : string;
const OPNAME = 'TGrowthFactorDataSQLAgent.DeleteProjectionMinMaxChannelSQL';
begin
  Result := '';
  try
    Result := 'DELETE * FROM GrowthFactorExcelMinMax A WHERE ' +
               GetScenarioWhereClause +
               'AND (A.ChannelNumber = ' + IntToStr(AMinMaxChannel)+' )';
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;


function TGrowthFactorDataSQLAgent.DeleteHydrologyGrowthFactorsByGaugeSQL(AGaugeNumber:integer) : string;
const OPNAME = 'TGrowthFactorDataSQLAgent.DeleteHydrologyGrowthFactorsByGaugeSQL';
begin
  Result := '';
  try
    Result := 'DELETE FROM GrowthFactorHydrology WHERE ' +
               GetScenarioWhereClause +
               'AND A.GaugeNumber = ' +  QuotedStr(IntToStr(AGaugeNumber));
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;
//_____________________________________________________________________________________________//

function TGrowthFactorDataSQLAgent.InsertDemandCentreGrowthFactors(var AIdentifier,AChannelNumber : integer) : boolean;
const OPNAME = 'TGrowthFactorDataSQLAgent.InsertDemandCentreGrowthFactors';
var
  LDataSet : TAbstractModelDataset;
  LIdentifier : integer;
  LImportDate : TDateTime;
  LSQL : string;
begin
  Result := FALSE;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      if Assigned(LDataSet) then
      begin
        LIdentifier := GetLastDemandCentreIdentifier + 1;
        lSQL:= InsertDemandCentreGrowthFactorsSQL +
              QuotedStr(FAppModules.StudyArea.ModelCode) + ','+
              QuotedStr(FAppModules.StudyArea.StudyAreaCode) + ','+
              QuotedStr(FAppModules.StudyArea.SubAreaCode) + ','+
              QuotedStr(FAppModules.StudyArea.ScenarioCode) + ','+
              IntToStr(LIdentifier) +','+IntToStr(AChannelNumber)+ ')';
        lDataSet.SetSQL(lSQL);
        lDataset.ExecSQL;
        lDataset.DataSet.Close;

        AIdentifier := LIdentifier;
        FAppModules.StudyArea.LastUpdateDate := Now();

        lImportDate := FAppModules.StudyArea.GetStudyImportDate;
        if (lImportDate = NullDateTime) then
          FAppModules.StudyArea.SetStudyImportDate(FAppModules.StudyArea.LastUpdateDate);

        Result := TRUE;
      end;
    finally
      lDataset.Free;
    end;
  except on E: Exception do HandleErrorFunction(E, OPNAME, Result) end;
end;

function TGrowthFactorDataSQLAgent.DeleteAllGrowthFactorDemand: boolean;
const OPNAME = 'TGrowthFactorDataSQLAgent.DeleteAllGrowthFactorDemand';
var
  LDataSet    : TAbstractModelDataset;
  LImportDate : TDateTime;
  LSQL        : string;
begin
  Result := False;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      if Assigned(LDataSet) then
      begin
        LSQL := DeleteAllGrowthFactorDemandSQL;
        LDataSet.SetSQL(lSQL);
        LDataset.ExecSQL;
        LDataset.DataSet.Close;

        FAppModules.StudyArea.LastUpdateDate := Now();

        LImportDate := FAppModules.StudyArea.GetStudyImportDate;
        if (lImportDate = NullDateTime) then
          FAppModules.StudyArea.SetStudyImportDate(FAppModules.StudyArea.LastUpdateDate);

        Result := True;
      end;
    finally
      LDataset.Free;
    end;
  except on E: Exception do HandleErrorFunction(E, OPNAME, Result) end;
end;

function TGrowthFactorDataSQLAgent.DeleteAllGrowthFactorDemandSQL: string;
const OPNAME = 'TGrowthFactorDataSQLAgent.DeleteAllGrowthFactorDemandSQL';
begin
  Result := '';
  try
    Result := 'DELETE * FROM GrowthFactorDemand A WHERE'  +
               GetScenarioWhereClause;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TGrowthFactorDataSQLAgent.DeleteAllGrowthFactorHydrology: boolean;
const OPNAME = 'TGrowthFactorDataSQLAgent.DeleteAllGrowthFactorHydrology';
var
  LDataSet    : TAbstractModelDataset;
  LImportDate : TDateTime;
  LSQL        : string;
begin
  Result := False;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      if Assigned(LDataSet) then
      begin
        LSQL := DeleteAllGrowthFactorHydrologySQL;
        LDataSet.SetSQL(lSQL);
        LDataset.ExecSQL;
        LDataset.DataSet.Close;

        FAppModules.StudyArea.LastUpdateDate := Now();

        LImportDate := FAppModules.StudyArea.GetStudyImportDate;
        if (lImportDate = NullDateTime) then
          FAppModules.StudyArea.SetStudyImportDate(FAppModules.StudyArea.LastUpdateDate);

        Result := True;
      end;
    finally
      LDataset.Free;
    end;
  except on E: Exception do HandleErrorFunction(E, OPNAME, Result) end;
end;

function TGrowthFactorDataSQLAgent.DeleteAllGrowthFactorHydrologySQL: string;
const OPNAME = 'TGrowthFactorDataSQLAgent.DeleteAllGrowthFactorHydrologySQL';
begin
  Result := '';
  try
    Result := 'DELETE * FROM GrowthFactorHydrology A WHERE' +
               GetScenarioWhereClause;
  except on E: Exception do HandleError (E, OPNAME) end;
end;

function TGrowthFactorDataSQLAgent.DeleteAllGrowthFactorMinMax: boolean;
const OPNAME = 'TGrowthFactorDataSQLAgent.DeleteAllGrowthFactorMinMax';
var
  LDataSet    : TAbstractModelDataset;
  LImportDate : TDateTime;
  LSQL        : string;
begin
  Result := False;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      if Assigned(LDataSet) then
      begin
        LSQL := DeleteAllGrowthFactorMinMaxSQL;
        LDataSet.SetSQL(lSQL);
        LDataset.ExecSQL;
        LDataset.DataSet.Close;

        FAppModules.StudyArea.LastUpdateDate := Now();

        LImportDate := FAppModules.StudyArea.GetStudyImportDate;
        if (lImportDate = NullDateTime) then
          FAppModules.StudyArea.SetStudyImportDate(FAppModules.StudyArea.LastUpdateDate);

        Result := True;
      end;
    finally
      LDataset.Free;
    end;
  except on E: Exception do HandleErrorFunction(E, OPNAME, Result) end;
end;

function TGrowthFactorDataSQLAgent.DeleteAllGrowthFactorMinMaxSQL: string;
const OPNAME = 'TGrowthFactorDataSQLAgent.DeleteAllGrowthFactorMinMaxSQL';
begin
  Result := '';
  try
    Result := 'DELETE * FROM GrowthFactorMinMax A WHERE ' +
               GetScenarioWhereClause;
  except on E: Exception do HandleError (E, OPNAME) end;
end;

function TGrowthFactorDataSQLAgent.DeleteAllGrowthFactorConfig: boolean;
const OPNAME = 'TGrowthFactorDataSQLAgent.DeleteAllGrowthFactorConfig';
var
  LDataSet    : TAbstractModelDataset;
  LImportDate : TDateTime;
  LSQL        : string;
begin
  Result := False;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      if Assigned(LDataSet) then
      begin
        LSQL := DeleteAllGrowthFactorConfigSQL;
        LDataSet.SetSQL(lSQL);
        LDataset.ExecSQL;
        LDataset.DataSet.Close;

        FAppModules.StudyArea.LastUpdateDate := Now();

        LImportDate := FAppModules.StudyArea.GetStudyImportDate;
        if (lImportDate = NullDateTime) then
          FAppModules.StudyArea.SetStudyImportDate(FAppModules.StudyArea.LastUpdateDate);

        Result := True;
      end;
    finally
      LDataset.Free;
    end;
  except on E: Exception do HandleErrorFunction(E, OPNAME, Result) end;
end;

function TGrowthFactorDataSQLAgent.DeleteAllGrowthFactorConfigSQL: string;
const OPNAME = 'TGrowthFactorDataSQLAgent.DeleteAllGrowthFactorConfigSQL';
begin
  Result := '';
  try
    Result := 'DELETE * FROM GrowthFactorConfig A WHERE ' +
               GetScenarioWhereClause;
  except on E: Exception do HandleError (E, OPNAME) end;
end;

function TGrowthFactorDataSQLAgent.AddDemandGrowthFactorsToDB(AGrowthFactors : TObjectList): boolean;
const OPNAME = 'TGrowthFactorDataSQLAgent.AddDemandGrowthFactorsToDB';
var
  LDataSet             : TAbstractModelDataset;
  LImportDate          : TDateTime;
  LSQL                 : string;
  LIndex               : integer;
  LDemandGrowthFactors : TDemandCentreGrowthFactors;
begin
  Result := False;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      if Assigned(LDataSet) then
      begin
        for LIndex := 0 to AGrowthFactors.Count -1 do
        begin
          LDemandGrowthFactors := TDemandCentreGrowthFactors(AGrowthFactors.Items[LIndex]);
          LDataSet.DataSet.Close;
          LDataSet.ClearSQL;
          LDataSet.ClearQueryParams();
          lSQL := GetGrowthFactorDemandSQL +
                  ' ORDER BY Model,StudyAreaName,SubArea,Scenario,Identifier';
          LDataSet.SetSQL(lSQL);
          LDataSet.SetReadOnly(False);
          LDataSet.DataSet.Open;
          LDataSet.DataSet.Insert;
          LDataSet.DataSet.FieldByName('Model').AsString          := FAppModules.StudyArea.ModelCode;
          LDataSet.DataSet.FieldByName('StudyAreaName').AsString  := FAppModules.StudyArea.StudyAreaCode;
          LDataSet.DataSet.FieldByName('SubArea').AsString        := FAppModules.StudyArea.SubAreaCode;
          LDataSet.DataSet.FieldByName('Scenario').AsString       := FAppModules.StudyArea.ScenarioCode;
          LDataSet.DataSet.FieldByName('Identifier').AsInteger    := LDemandGrowthFactors.Identifier;
          LDataSet.DataSet.FieldByName('ChannelNumber').AsInteger := LDemandGrowthFactors.ChannelNumber;
          LDataSet.DataSet.FieldByName('ValidFactors').AsString   := 'Y';
          LDataSet.DataSet.FieldByName('Factors').AsString        := LDemandGrowthFactors.GrowthFactors;
          LDataSet.DataSet.Post;
          FAppModules.StudyArea.LastUpdateDate := Now();

          LImportDate := FAppModules.StudyArea.GetStudyImportDate;
          if (lImportDate = NullDateTime) then
            FAppModules.StudyArea.SetStudyImportDate(FAppModules.StudyArea.LastUpdateDate);

          Result := True;
        end;
      end;
    finally
      LDataset.Free;
    end;
  except on E: Exception do HandleErrorFunction(E, OPNAME, Result) end;
end;

function TGrowthFactorDataSQLAgent.AddDemandGrowthFactorsToDBSQL: string;
const OPNAME = 'TGrowthFactorDataSQLAgent.AddDemandGrowthFactorsToDBSQL';
begin
  Result := '';
  try
    Result := 'INSERT INTO GrowthFactorDemand'+
              '(Model, StudyAreaName, SubArea, Scenario, Identifier, ChannelNumber)'+
              ' VALUES'+
              '(:Model, :StudyAreaName, :SubArea, :Scenario, :Identifier, :ChannelNumber)';
  except on E: Exception do HandleError (E, OPNAME) end;
end;

function TGrowthFactorDataSQLAgent.AddMinMaxChannelGrowthFactorsToDB(AGrowthFactors : TObjectList): boolean;
const OPNAME = 'TGrowthFactorDataSQLAgent.AddMinMaxChannelGrowthFactorsToDB';
var
  LDataSet             : TAbstractModelDataset;
  LImportDate          : TDateTime;
  LWhereClause         : string;
  LIndex               : integer;
  LMinMaxGrowthFactors : TMinMaxChannelGrowthFactors;
begin
  Result := False;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      if Assigned(LDataSet) then
      begin
        for LIndex := 0 to AGrowthFactors.Count -1 do
        begin
          LMinMaxGrowthFactors := TMinMaxChannelGrowthFactors(AGrowthFactors.Items[LIndex]);
          LDataSet.DataSet.Close;
          LDataSet.ClearSQL;
          LDataSet.SetSQL(AddMinMaxChannelGrowthFactorsToDBSQL);
          LDataSet.SetParams(
          ['Model','StudyAreaName','SubArea','Scenario','Identifier','ChannelNumber','ArcNumber','ValidFactors'],
          [FAppModules.StudyArea.ModelCode,FAppModules.StudyArea.StudyAreaCode,
          FAppModules.StudyArea.SubAreaCode,FAppModules.StudyArea.ScenarioCode,
          IntToStr(LMinMaxGrowthFactors.Identifier),IntToStr(LMinMaxGrowthFactors.MinMaxChannel),
          IntToStr(LMinMaxGrowthFactors.ArcNumber),'Y']);
          LDataSet.ExecSQL;

          if (LMinMaxGrowthFactors <> nil) then
          begin
            LWhereClause := ' WHERE Model         =  '+QuotedStr(FAppModules.StudyArea.ModelCode)+
                            ' AND StudyAreaName   =  '+QuotedStr(FAppModules.StudyArea.StudyAreaCode)+
                            ' AND SubArea         =  '+QuotedStr(FAppModules.StudyArea.SubAreaCode)+
                            ' AND Scenario        =  '+QuotedStr(FAppModules.StudyArea.ScenarioCode)+
                            ' AND Identifier      =  '+IntToStr(LMinMaxGrowthFactors.Identifier);
            FAppModules.Database.UpdateMemoField('GrowthFactorMinMax','Factors',LWhereClause,
                                             LMinMaxGrowthFactors.GrowthFactors);
            FAppModules.StudyArea.LastUpdateDate := Now();
            LImportDate := FAppModules.StudyArea.GetStudyImportDate;
            if (lImportDate = NullDateTime) then
              FAppModules.StudyArea.SetStudyImportDate(FAppModules.StudyArea.LastUpdateDate);

            Result := True;
          end;
        end;
      end;
    finally
      LDataset.Free;
    end;
  except on E: Exception do HandleErrorFunction(E, OPNAME, Result) end;
end;

function TGrowthFactorDataSQLAgent.AddMinMaxChannelGrowthFactorsToDBSQL: string;
const OPNAME = 'TGrowthFactorDataSQLAgent.AddMinMaxChannelGrowthFactorsToDBSQL';
begin
  Result := '';
  try
    Result := 'INSERT INTO GrowthFactorMinMax'+
                  '(Model, StudyAreaName, SubArea, Scenario, Identifier, ChannelNumber, ArcNumber,ValidFactors)'+
                  ' VALUES'+
                  '(:Model, :StudyAreaName, :SubArea, :Scenario, :Identifier, :ChannelNumber,:ArcNumber, :ValidFactors)';
  except on E: Exception do HandleError (E, OPNAME) end;
end;

function TGrowthFactorDataSQLAgent.AddHydrologyGrowthFactorsToDB(AGrowthFactors: TObjectlist) : boolean;
const OPNAME = 'TGrowthFactorDataSQLAgent.AddHydrologyGrowthFactorsToDB';
var
  LDataSet                : TAbstractModelDataset;
  LImportDate             : TDateTime;

  LWhereClause            : string;
  LIndex                  : integer;
  LHydrologyGrowthFactors : THydrologyGrowthFactors;
begin
  Result := False;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      if Assigned(LDataSet) then
      begin
        for LIndex := 0 to AGrowthFactors.Count -1 do
        begin
          LHydrologyGrowthFactors := THydrologyGrowthFactors(AGrowthFactors.Items[LIndex]);
          LDataSet.DataSet.Close;
          LDataSet.ClearSQL;
          LDataSet.ClearQueryParams();
          LDataSet.SetSQL(AddHydrologyGrowthFactorsToDBSQL);
          LDataset.ClearQueryParams();
          LDataSet.SetParams(
            ['Model', 'StudyAreaName', 'SubArea', 'Scenario'],
            [FAppModules.StudyArea.ModelCode, FAppModules.StudyArea.StudyAreaCode,
             FAppModules.StudyArea.SubAreaCode, FAppModules.StudyArea.ScenarioCode]);

          LDataSet.SetParams(['Identifier'], [IntToStr(LHydrologyGrowthFactors.Identifier)]);
          LDataSet.SetParams(['GaugeNumber'], [IntToStr(LHydrologyGrowthFactors.GaugeNumber)]);
          LDataSet.ExecSQL;
          if (LHydrologyGrowthFactors <> nil) then
          begin
            LWhereClause := ' WHERE Model         =  '+QuotedStr(FAppModules.StudyArea.ModelCode)+
                            ' AND StudyAreaName   =  '+QuotedStr(FAppModules.StudyArea.StudyAreaCode)+
                            ' AND SubArea         =  '+QuotedStr(FAppModules.StudyArea.SubAreaCode)+
                            ' AND Scenario        =  '+QuotedStr(FAppModules.StudyArea.ScenarioCode)+
                            ' AND Identifier      =  '+IntToStr(LHydrologyGrowthFactors.Identifier);

            FAppModules.Database.UpdateMemoField('GrowthFactorHydrology','AFFFactors',LWhereClause,
                                             LHydrologyGrowthFactors.AFFGrowthFactors);
            FAppModules.Database.UpdateMemoField('GrowthFactorHydrology','IRRFactors',LWhereClause,
                                             LHydrologyGrowthFactors.IRRGrowthFactors);
            FAppModules.Database.UpdateMemoField('GrowthFactorHydrology','URBFactors',LWhereClause,
                                             LHydrologyGrowthFactors.URBGrowthFactors);
          end;
          LDataSet.DataSet.Close;
          FAppModules.StudyArea.LastUpdateDate := Now();
          LImportDate := FAppModules.StudyArea.GetStudyImportDate;
          if (lImportDate = NullDateTime) then
            FAppModules.StudyArea.SetStudyImportDate(FAppModules.StudyArea.LastUpdateDate);

          Result := True;
        end;
      end;

    finally
      LDataset.Free;
    end;
  except on E: Exception do HandleErrorFunction(E, OPNAME, Result) end;
end;

function TGrowthFactorDataSQLAgent.AddHydrologyGrowthFactorsToDBSQL: string;
const OPNAME = 'TGrowthFactorDataSQLAgent.AddHydrologyGrowthFactorsToDBSQL';
begin
  Result := '';
  try
    Result := 'INSERT INTO GrowthFactorHydrology'+
                  '(Model, StudyAreaName, SubArea, Scenario, Identifier, GaugeNumber)'+ //, AFFFactors, IRRFactors, URBFactors
                  ' VALUES'+
                  '(:Model, :StudyAreaName, :SubArea, :Scenario, :Identifier, :GaugeNumber)'; //,:AFFFactors, :IRRFactors, :URBFactors
  except on E: Exception do HandleError (E, OPNAME) end;
end;



function TGrowthFactorDataSQLAgent.InsertMinMaxChannelGrowthFactors(var AIdentifier,AChannelNumber : integer) : boolean;
const OPNAME = 'TGrowthFactorDataSQLAgent.InsertMinMaxChannelGrowthFactors';
var
  LDataSet : TAbstractModelDataset;
  LIdentifier : integer;
  LImportDate : TDateTime;
  LSQL : string;
begin
  Result := False;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      if Assigned(LDataSet) then
      begin
        LIdentifier := GetLastMinMaxChannelIdentifier + 1;
        LSQL:= InsertMinMaxChannelGrowthFactorsSQL +
              QuotedStr(FAppModules.StudyArea.ModelCode) + ','+
              QuotedStr(FAppModules.StudyArea.StudyAreaCode) + ','+
              QuotedStr(FAppModules.StudyArea.SubAreaCode) + ','+
              QuotedStr(FAppModules.StudyArea.ScenarioCode) + ','+
              IntToStr(LIdentifier) + ','+IntToStr(AChannelNumber)+')';

        LDataSet.SetSQL(lSQL);
        LDataset.ExecSQL;
        LDataset.DataSet.Close;

        AIdentifier := LIdentifier;
        FAppModules.StudyArea.LastUpdateDate := Now();

        LImportDate := FAppModules.StudyArea.GetStudyImportDate;
        if (lImportDate = NullDateTime) then
          FAppModules.StudyArea.SetStudyImportDate(FAppModules.StudyArea.LastUpdateDate);

        Result := True;
      end;
    finally
      LDataset.Free;
    end;
  except on E: Exception do HandleErrorFunction(E, OPNAME, Result) end;
end;

function TGrowthFactorDataSQLAgent.InsertHydrologyGrowthFactors(var AIdentifier : integer) : boolean;
const OPNAME = 'TGrowthFactorDataSQLAgent.InsertHydrologyGrowthFactors';
var
  LDataSet : TAbstractModelDataset;
  LIdentifier : integer;
  LImportDate : TDateTime;
  LSQL : string;
begin
  Result := False;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      if Assigned(LDataSet) then
      begin
        LIdentifier := GetLastHydrologyIdentifier + 1;
        LSQL:= InsertHydrologyGrowthFactorsSQL +
              QuotedStr(FAppModules.StudyArea.ModelCode) + ','+
              QuotedStr(FAppModules.StudyArea.StudyAreaCode) + ','+
              QuotedStr(FAppModules.StudyArea.SubAreaCode) + ','+
              QuotedStr(FAppModules.StudyArea.ScenarioCode) + ','+
              IntToStr(LIdentifier)+')';
        LDataSet.SetSQL(lSQL);
        LDataset.ExecSQL;
        LDataset.DataSet.Close;
        AIdentifier := LIdentifier;
        FAppModules.StudyArea.LastUpdateDate := Now();
        LImportDate := FAppModules.StudyArea.GetStudyImportDate;
        if (lImportDate = NullDateTime) then
          FAppModules.StudyArea.SetStudyImportDate(FAppModules.StudyArea.LastUpdateDate);
        Result := True;
      end;
    finally
      LDataset.Free;
    end;
  except on E: Exception do HandleErrorFunction(E, OPNAME, Result) end;
end;

function TGrowthFactorDataSQLAgent.GetLastDemandCentreIdentifier: integer;
const OPNAME = 'TGrowthFactorDataSQLAgent.GetLastDemandCentreIdentifier';
var
  LDataSet : TAbstractModelDataset;
  LSQL     : string;
begin
  Result := 0;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      if Assigned(LDataSet) then
      begin
        LSQL := GetLastDemandCentreIdentifierSQL;      
        LDataSet.SetSQL(LSQL);
        LDataset.DataSet.Open;
        Result := LDataset.DataSet.FieldByName('LastID').AsInteger;
        LDataset.DataSet.Close;
      end;
      finally
        LDataset.Free;
      end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TGrowthFactorDataSQLAgent.GetLastHydrologyIdentifier: integer;
const OPNAME = 'TGrowthFactorDataSQLAgent.GetLastHydrologyIdentifier';
var
  LDataSet : TAbstractModelDataset;
  LSQL     : string;
begin
  Result := 0;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      if Assigned(LDataSet) then
      begin
        LSQL := GetLastHydrologyIdentifierSQL;
        LDataSet.SetSQL(LSQL);
        LDataset.DataSet.Open;
        Result := LDataset.DataSet.FieldByName('LastID').AsInteger;
        LDataset.DataSet.Close;
      end;
      finally
        LDataset.Free;
      end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TGrowthFactorDataSQLAgent.GetLastMinMaxChannelIdentifier: integer;
const OPNAME = 'TGrowthFactorDataSQLAgent.GetLastMinMaxChannelIdentifier';
var
  LDataSet : TAbstractModelDataset;
  LSQL     : string;
begin
  Result := 0;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      if Assigned(LDataSet) then
      begin
        LSQL := GetLastMinMaxChannelIdentifierSQL;
        LDataSet.SetSQL(LSQL);
        LDataset.DataSet.Open;
        Result := LDataset.DataSet.FieldByName('LastID').AsInteger;
        LDataset.DataSet.Close;
      end;
      finally
        LDataset.Free;
      end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TGrowthFactorDataSQLAgent.DeleteDemandGrowthFactorByChannel(AChannelNumber : integer) : boolean;
const OPNAME = 'TGrowthFactorDataSQLAgent.DeleteDemandGrowthFactorByChannel';
var
  LDataSet    : TAbstractModelDataset;
  LImportDate : TDateTime;
  lSQL        : string;
begin
  Result := False;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      if Assigned(LDataSet) then
      begin
        lSQL := DeleteDemandGrowthFactorByChannelSQL(AChannelNumber);
        LDataSet.SetSQL(lSQL);
        LDataset.ExecSQL;
        LDataset.DataSet.Close;

        FAppModules.StudyArea.LastUpdateDate := Now();

        LImportDate := FAppModules.StudyArea.GetStudyImportDate;
        if LImportDate = NullDateTime then
          FAppModules.StudyArea.SetStudyImportDate(FAppModules.StudyArea.LastUpdateDate);

        Result := True;
      end;
    finally
      LDataset.Free;
    end;
  except on E: Exception do HandleErrorFunction(E, OPNAME, Result) end;
end;

function TGrowthFactorDataSQLAgent.DeleteMinMaxChannelGrowthFactorByChannel(AMinMaxChannel:integer):boolean;
const OPNAME = 'TGrowthFactorDataSQLAgent.DeleteMinMaxChannelGrowthFactorByChannel';
var
  LDataSet    : TAbstractModelDataset;
  LImportDate : TDateTime;
  lSQL        : string;
begin
  Result := False;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      if Assigned(LDataSet) then
      begin
        lSQL := DeleteMinMaxChannelGrowthFactorByChannelSQL(AMinMaxChannel);
        LDataSet.SetSQL(lSQL);
        LDataset.ExecSQL;
        LDataset.DataSet.Close;

        FAppModules.StudyArea.LastUpdateDate := Now();

        LImportDate := FAppModules.StudyArea.GetStudyImportDate;
        if LImportDate = NullDateTime then
          FAppModules.StudyArea.SetStudyImportDate(FAppModules.StudyArea.LastUpdateDate);

        Result := True;
      end;
    finally
      LDataset.Free;
    end;
  except on E: Exception do HandleErrorFunction(E, OPNAME, Result) end;
end;

function TGrowthFactorDataSQLAgent.DeleteProjectionMinMaxChannel(AMinMaxChannel:integer):boolean;
const OPNAME = 'TGrowthFactorDataSQLAgent.DeleteProjectionMinMaxChannel';
var
  LDataSet    : TAbstractModelDataset;
  LImportDate : TDateTime;
  lSQL        : string;
begin
  Result := False;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      if Assigned(LDataSet) then
      begin
        lSQL := DeleteProjectionMinMaxChannelSQL(AMinMaxChannel);
        LDataSet.SetSQL(lSQL);
        LDataset.ExecSQL;
        LDataset.DataSet.Close;

        FAppModules.StudyArea.LastUpdateDate := Now();

        LImportDate := FAppModules.StudyArea.GetStudyImportDate;
        if LImportDate = NullDateTime then
          FAppModules.StudyArea.SetStudyImportDate(FAppModules.StudyArea.LastUpdateDate);

        Result := True;
      end;
    finally
      LDataset.Free;
    end;
  except on E: Exception do HandleErrorFunction(E, OPNAME, Result) end;
end;

function TGrowthFactorDataSQLAgent.DeleteHydrologyGrowthFactorsByGauge(AGaugeNumber: integer):boolean;
const OPNAME = 'TGrowthFactorDataSQLAgent.DeleteHydrologyGrowthFactorsByGauge';
var
  LDataSet    : TAbstractModelDataset;
  LImportDate : TDateTime;
  lSQL        : string;
begin
  Result := False;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      if Assigned(LDataSet) then
      begin
        lSQL := DeleteHydrologyGrowthFactorsByGaugeSQL(AGaugeNumber);
        LDataSet.SetSQL(lSQL);
        LDataset.ExecSQL;
        LDataset.DataSet.Close;

        FAppModules.StudyArea.LastUpdateDate := Now();

        LImportDate := FAppModules.StudyArea.GetStudyImportDate;
        if LImportDate = NullDateTime then
          FAppModules.StudyArea.SetStudyImportDate(FAppModules.StudyArea.LastUpdateDate);

        Result := True;
      end;
    finally
      LDataset.Free;
    end;
  except on E: Exception do HandleErrorFunction(E, OPNAME, Result) end;
end;


function TGrowthFactorDataSQLAgent.DeleteAllGrowthProjectionConfig: boolean;
const OPNAME = 'TGrowthFactorDataSQLAgent.DeleteAllGrowthProjectionConfig';
var
  LDataSet    : TAbstractModelDataset;
  LImportDate : TDateTime;
  LSQL        : string;
begin
  Result := False;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      if Assigned(LDataSet) then
      begin
        LSQL := DeleteAllGrowthProjectionConfigSQL;
        LDataSet.SetSQL(lSQL);
        LDataset.ExecSQL;
        LDataset.DataSet.Close;

        FAppModules.StudyArea.LastUpdateDate := Now();

        LImportDate := FAppModules.StudyArea.GetStudyImportDate;
        if (lImportDate = NullDateTime) then
          FAppModules.StudyArea.SetStudyImportDate(FAppModules.StudyArea.LastUpdateDate);

        Result := True;
      end;
    finally
      LDataset.Free;
    end;
  except on E: Exception do HandleErrorFunction(E, OPNAME, Result) end;
end;

function TGrowthFactorDataSQLAgent.DeleteAllGrowthProjectionConfigSQL: string;
const OPNAME = 'TGrowthFactorDataSQLAgent.DeleteAllGrowthProjectionConfigSQL';
begin
  Result := '';
  try
    Result := 'DELETE * FROM GrowthFactorExcelConfig A WHERE ' +
               GetScenarioWhereClause;
  except on E: Exception do HandleError (E, OPNAME) end;

end;

function TGrowthFactorDataSQLAgent.DeleteAllGrowthProjectionDemand: boolean;
const OPNAME = 'TGrowthFactorDataSQLAgent.DeleteAllGrowthProjectionDemand';
var
  LDataSet    : TAbstractModelDataset;
  LImportDate : TDateTime;
  LSQL        : string;
begin
  Result := False;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      if Assigned(LDataSet) then
      begin
        LSQL := DeleteAllGrowthProjectionDemandSQL;
        LDataSet.SetSQL(lSQL);
        LDataset.ExecSQL;
        LDataset.DataSet.Close;

        FAppModules.StudyArea.LastUpdateDate := Now();

        LImportDate := FAppModules.StudyArea.GetStudyImportDate;
        if (lImportDate = NullDateTime) then
          FAppModules.StudyArea.SetStudyImportDate(FAppModules.StudyArea.LastUpdateDate);

        Result := True;
      end;
    finally
      LDataset.Free;
    end;
  except on E: Exception do HandleErrorFunction(E, OPNAME, Result) end;

end;

function TGrowthFactorDataSQLAgent.DeleteAllGrowthProjectionDemandSQL: string;
const OPNAME = 'TGrowthFactorDataSQLAgent.DeleteAllGrowthProjectionDemandSQL';
begin
  Result := '';
  try
    Result := 'DELETE * FROM GrowthFactorExcelDemand A WHERE'  +
               GetScenarioWhereClause;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TGrowthFactorDataSQLAgent.DeleteAllGrowthProjectionHydrology: boolean;
const OPNAME = 'TGrowthFactorDataSQLAgent.DeleteAllGrowthProjectionHydrology';
var
  LDataSet    : TAbstractModelDataset;
  LImportDate : TDateTime;
  LSQL        : string;
begin
  Result := False;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      if Assigned(LDataSet) then
      begin
        LSQL := DeleteAllGrowthProjectionHydrologySQL;
        LDataSet.SetSQL(lSQL);
        LDataset.ExecSQL;
        LDataset.DataSet.Close;

        FAppModules.StudyArea.LastUpdateDate := Now();

        LImportDate := FAppModules.StudyArea.GetStudyImportDate;
        if (lImportDate = NullDateTime) then
          FAppModules.StudyArea.SetStudyImportDate(FAppModules.StudyArea.LastUpdateDate);

        Result := True;
      end;
    finally
      LDataset.Free;
    end;
  except on E: Exception do HandleErrorFunction(E, OPNAME, Result) end;
end;

function TGrowthFactorDataSQLAgent.DeleteAllGrowthProjectionHydrologySQL: string;
const OPNAME = 'TGrowthFactorDataSQLAgent.DeleteAllGrowthProjectionHydrologySQL';
begin
  Result := '';
  try
    Result := 'DELETE * FROM GrowthFactorExcelHydrology A WHERE' +
               GetScenarioWhereClause;
  except on E: Exception do HandleError (E, OPNAME) end;
end;

function TGrowthFactorDataSQLAgent.DeleteAllGrowthProjectionMinMax: boolean;
const OPNAME = 'TGrowthFactorDataSQLAgent.DeleteAllGrowthProjectionMinMax';
var
  LDataSet    : TAbstractModelDataset;
  LImportDate : TDateTime;
  LSQL        : string;
begin
  Result := False;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      if Assigned(LDataSet) then
      begin
        LSQL := DeleteAllGrowthProjectionMinMaxSQL;
        LDataSet.SetSQL(lSQL);
        LDataset.ExecSQL;
        LDataset.DataSet.Close;

        FAppModules.StudyArea.LastUpdateDate := Now();

        LImportDate := FAppModules.StudyArea.GetStudyImportDate;
        if (lImportDate = NullDateTime) then
          FAppModules.StudyArea.SetStudyImportDate(FAppModules.StudyArea.LastUpdateDate);

        Result := True;
      end;
    finally
      LDataset.Free;
    end;
  except on E: Exception do HandleErrorFunction(E, OPNAME, Result) end;
end;

function TGrowthFactorDataSQLAgent.DeleteAllGrowthProjectionMinMaxSQL: string;
const OPNAME = 'TGrowthFactorDataSQLAgent.DeleteAllGrowthProjectionMinMaxSQL';
begin
  Result := '';
  try
    Result := 'DELETE A.* FROM GrowthFactorExcelMinMax A WHERE ' +
               GetScenarioWhereClause;
  except on E: Exception do HandleError (E, OPNAME) end;
end;

function TGrowthFactorDataSQLAgent.AddDemandGrowthProjectionsToDB(AGrowthFactors: TObjectlist): boolean;
const OPNAME = 'TGrowthFactorDataSQLAgent.AddDemandGrowthProjectionsToDB';
var
  LDataSet                 : TAbstractModelDataset;
  LImportDate              : TDateTime;
  LWhereClause             : string;
  LIndex                   : integer;
  LDemandGrowthProjections : TExelDemandChannelGrowthFactors;
begin
  Result := False;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      if Assigned(LDataSet) then
      begin
        for LIndex := 0 to AGrowthFactors.Count -1 do
        begin
          LDemandGrowthProjections := TExelDemandChannelGrowthFactors(AGrowthFactors.Items[LIndex]);
          LDataSet.DataSet.Close;
//          LDataSet.ClearSQL;
          LDataSet.ClearQueryParams();
          LDataSet.SetSQL(AddDemandGrowthProjectionsToDBSQL);
          LDataSet.SetParams(
          ['Model','StudyAreaName','SubArea','Scenario','Identifier','ChannelNumber','Institution','WaterUser'],
          [FAppModules.StudyArea.ModelCode,FAppModules.StudyArea.StudyAreaCode,
          FAppModules.StudyArea.SubAreaCode,FAppModules.StudyArea.ScenarioCode,
          IntToStr(LDemandGrowthProjections.Identifier),IntToStr(LDemandGrowthProjections.ChannelNumber),
          LDemandGrowthProjections.Institution, LDemandGrowthProjections.WaterUser]);
          LDataSet.ExecSQL;

          if (LDemandGrowthProjections <> nil) then
          begin
            LWhereClause := ' WHERE Model         =  '+QuotedStr(FAppModules.StudyArea.ModelCode)+
                            ' AND StudyAreaName   =  '+QuotedStr(FAppModules.StudyArea.StudyAreaCode)+
                            ' AND SubArea         =  '+QuotedStr(FAppModules.StudyArea.SubAreaCode)+
                            ' AND Scenario        =  '+QuotedStr(FAppModules.StudyArea.ScenarioCode)+
                            ' AND Identifier      =  '+IntToStr(LDemandGrowthProjections.Identifier);
            FAppModules.Database.UpdateMemoField('GrowthFactorExcelDemand','Factors',LWhereClause,
                                             LDemandGrowthProjections.GrowthFactors);
            FAppModules.StudyArea.LastUpdateDate := Now();

            LImportDate := FAppModules.StudyArea.GetStudyImportDate;
            if (lImportDate = NullDateTime) then
              FAppModules.StudyArea.SetStudyImportDate(FAppModules.StudyArea.LastUpdateDate);

            Result := True;
          end;
        end;
      end;
    finally
      LDataset.Free;
    end;
  except on E: Exception do HandleErrorFunction(E, OPNAME, Result) end;
end;

function TGrowthFactorDataSQLAgent.AddDemandGrowthProjectionsToDBSQL: string;
const OPNAME = 'TGrowthFactorDataSQLAgent.AddDemandGrowthProjectionsToDBSQL';
begin
  Result := '';
  try
    Result := 'INSERT INTO GrowthFactorExcelDemand'+
                  '(Model, StudyAreaName, SubArea, Scenario, Identifier, ChannelNumber,Institution, WaterUser)'+
                  ' VALUES'+
                  '(:Model, :StudyAreaName, :SubArea, :Scenario, :Identifier, :ChannelNumber,:Institution,:WaterUser)';
  except on E: Exception do HandleError (E, OPNAME) end;
end;

function TGrowthFactorDataSQLAgent.AddHydrologyGrowthProjectionsToDB(AGrowthFactors: TObjectlist): boolean;
const OPNAME = 'TGrowthFactorDataSQLAgent.AddHydrologyGrowthProjectionsToDB';
var
  LDataSet                 : TAbstractModelDataset;
  LImportDate              : TDateTime;
  LWhereClause,
  LSQL                     : string;
  LIndex                   : integer;
  LHydrologyProjections    : TExelHydrologyGrowthFactors;
begin
  Result := False;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      if Assigned(LDataSet) then
      begin
        for LIndex := 0 to AGrowthFactors.Count -1 do
        begin
          LHydrologyProjections := TExelHydrologyGrowthFactors(AGrowthFactors.Items[LIndex]);
          LDataSet.DataSet.Close;
          LDataSet.ClearSQL;
          LDataSet.ClearQueryParams();
          LSQL := 'INSERT INTO GrowthFactorExcelHydrology'+
                  '(Model, StudyAreaName, SubArea, Scenario, Identifier, GaugeNumber, Institution, Institution1, Institution2, WaterUser, WaterUser1, WaterUser2 )'+  //, AFFFactors,IRRFactors,URBFactors)'+
                  ' VALUES'+
                  '(:Model, :StudyAreaName, :SubArea, :Scenario, :Identifier, :GaugeNumber, :Institution, :Institution1, :Institution2, :WaterUser, :WaterUser1, :WaterUser2)';  //, :AFFFactors, :IRRFactors, :URBFactors)';
          LDataSet.SetSQL(lSQL);
          LDataset.ClearQueryParams();
          LDataSet.SetParams(
            ['Model', 'StudyAreaName', 'SubArea', 'Scenario'],
            [FAppModules.StudyArea.ModelCode, FAppModules.StudyArea.StudyAreaCode,
             FAppModules.StudyArea.SubAreaCode, FAppModules.StudyArea.ScenarioCode]);

          LDataSet.SetParams(['Identifier'], [IntToStr(LHydrologyProjections.Identifier)]);
          LDataSet.SetParams(['GaugeNumber'], [IntToStr(LHydrologyProjections.GaugeNumber)]);
          LDataSet.SetParams(['Institution'], [LHydrologyProjections.Institution]);
          LDataSet.SetParams(['Institution1'], [LHydrologyProjections.Institution1]);
          LDataSet.SetParams(['Institution2'], [LHydrologyProjections.Institution2]);
          LDataSet.SetParams(['WaterUser'], [LHydrologyProjections.WaterUser]);
          LDataSet.SetParams(['WaterUser1'], [LHydrologyProjections.WaterUser1]);
          LDataSet.SetParams(['WaterUser2'], [LHydrologyProjections.WaterUser2]);
          LDataSet.ExecSQL;
          if (LHydrologyProjections <> nil) then
          begin
            LWhereClause := ' WHERE Model         =  '+QuotedStr(FAppModules.StudyArea.ModelCode)+
                            ' AND StudyAreaName   =  '+QuotedStr(FAppModules.StudyArea.StudyAreaCode)+
                            ' AND SubArea         =  '+QuotedStr(FAppModules.StudyArea.SubAreaCode)+
                            ' AND Scenario        =  '+QuotedStr(FAppModules.StudyArea.ScenarioCode)+
                            ' AND Identifier      =  '+IntToStr(LHydrologyProjections.Identifier);
            FAppModules.Database.UpdateMemoField('GrowthFactorExcelHydrology','AFFFactors',LWhereClause,
                                             LHydrologyProjections.AFFGrowthFactors);
            FAppModules.Database.UpdateMemoField('GrowthFactorExcelHydrology','IRRFactors',LWhereClause,
                                             LHydrologyProjections.IRRGrowthFactors);
            FAppModules.Database.UpdateMemoField('GrowthFactorExcelHydrology','URBFactors',LWhereClause,
                                             LHydrologyProjections.URBGrowthFactors);
          end;
          LDataSet.DataSet.Close;
          FAppModules.StudyArea.LastUpdateDate := Now();

            LImportDate := FAppModules.StudyArea.GetStudyImportDate;
            if (lImportDate = NullDateTime) then
              FAppModules.StudyArea.SetStudyImportDate(FAppModules.StudyArea.LastUpdateDate);

            Result := True;
        end;
      end;
    finally
    LDataset.Free;
    end;
  except on E: Exception do HandleErrorFunction(E, OPNAME, Result) end;
end;

function TGrowthFactorDataSQLAgent.AddHydrologyGrowthProjectionsToDBSQL: string;
const OPNAME = 'TGrowthFactorDataSQLAgent.AddHydrologyGrowthProjectionsToDBSQL';
begin
  Result := '';
  try
    Result := 'INSERT INTO GrowthFactorExcelHydrology'+
                  '(Model, StudyAreaName, SubArea, Scenario, Identifier, GaugeNumber, Institution, Institution1, Institution2, WaterUser, WaterUser1, WaterUser2, AFFFactors,IRRFactors,URBFactors)'+
                  ' VALUES'+
                  '(:Model, :StudyAreaName, :SubArea, :Scenario, :Identifier, :GaugeNumber, :Institution, :Institution1, :Institution2, :WaterUser, :WaterUser1, :WaterUser2, :AFFFactors, :IRRFactors, :URBFactors)';

  except on E: Exception do HandleError (E, OPNAME) end;
end;

function TGrowthFactorDataSQLAgent.AddMinMaxChannelGrowthProjectionsToDB(AGrowthFactors: TObjectlist): boolean;
const OPNAME = 'TGrowthFactorDataSQLAgent.AddMinMaxChannelGrowthProjectionsToDB';
var
  LDataSet                  : TAbstractModelDataset;
  LImportDate               : TDateTime;
  LWhereClause              : string;
  LIndex                    : integer;
  LMinMaxChannelProjections : TExelMinMaxChannelGrowthFactors;
begin
  Result := False;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      if Assigned(LDataSet) then
      begin
        for LIndex := 0 to AGrowthFactors.Count -1 do
        begin
          LMinMaxChannelProjections := TExelMinMaxChannelGrowthFactors(AGrowthFactors.Items[LIndex]);
          LDataSet.DataSet.Close;
          LDataSet.ClearSQL;
          LDataSet.ClearQueryParams();
          LDataSet.SetSQL(AddMinMaxChannelGrowthProjectionsToDBSQL);
          LDataSet.SetParams(
          ['Model','StudyAreaName','SubArea','Scenario','Identifier','ChannelNumber','ArcNumber','Institution','WaterUser'],
          [FAppModules.StudyArea.ModelCode,FAppModules.StudyArea.StudyAreaCode,
          FAppModules.StudyArea.SubAreaCode,FAppModules.StudyArea.ScenarioCode,
          IntToStr(LMinMaxChannelProjections.Identifier),IntToStr(LMinMaxChannelProjections.ChannelNumber),
          IntToStr(LMinMaxChannelProjections.ArcNumber),
          LMinMaxChannelProjections.Institution, LMinMaxChannelProjections.WaterUser]);
          LDataSet.ExecSQL;

          if (LMinMaxChannelProjections <> nil) then
          begin
            LWhereClause := ' WHERE Model         =  '+QuotedStr(FAppModules.StudyArea.ModelCode)+
                            ' AND StudyAreaName   =  '+QuotedStr(FAppModules.StudyArea.StudyAreaCode)+
                            ' AND SubArea         =  '+QuotedStr(FAppModules.StudyArea.SubAreaCode)+
                            ' AND Scenario        =  '+QuotedStr(FAppModules.StudyArea.ScenarioCode)+
                            ' AND Identifier      =  '+IntToStr(LMinMaxChannelProjections.Identifier);
            FAppModules.Database.UpdateMemoField('GrowthFactorExcelMinMax','Factors',LWhereClause,
                                             LMinMaxChannelProjections.GrowthFactors);
            FAppModules.StudyArea.LastUpdateDate := Now();

            LImportDate := FAppModules.StudyArea.GetStudyImportDate;
            if (lImportDate = NullDateTime) then
              FAppModules.StudyArea.SetStudyImportDate(FAppModules.StudyArea.LastUpdateDate);

            Result := True;
          end;
        end;
      end;
    finally
      LDataset.Free;
    end;
  except on E: Exception do HandleErrorFunction(E, OPNAME, Result) end;
end;

function TGrowthFactorDataSQLAgent.AddMinMaxChannelGrowthProjectionsToDBSQL: string;
const OPNAME = 'TGrowthFactorDataSQLAgent.AddMinMaxChannelGrowthProjectionsToDBSQL';
begin
  Result := '';
  try
    Result := 'INSERT INTO GrowthFactorExcelMinMax'+
                  '(Model, StudyAreaName, SubArea, Scenario, Identifier, ChannelNumber,ArcNumber,Institution, WaterUser)'+
                  ' VALUES'+
                  '(:Model, :StudyAreaName, :SubArea, :Scenario, :Identifier, :ChannelNumber,:ArcNumber,:Institution,:WaterUser)';
  except on E: Exception do HandleError (E, OPNAME) end;
end;

function TGrowthFactorDataSQLAgent.InsertDemandCentreGrowthProjection(var AIdentifier: integer): boolean;
const OPNAME = 'TGrowthFactorDataSQLAgent.InsertDemandCentreGrowthProjection';
begin
  Result := false;
end;

function TGrowthFactorDataSQLAgent.InsertDemandCentreGrowthProjectionSQL: string;
const OPNAME = 'TGrowthFactorDataSQLAgent.InsertDemandCentreGrowthProjectionSQL';
var
  LSQL : string;
begin
   LSQL := '';
end;

function TGrowthFactorDataSQLAgent.InsertHydrologyGrowthProjection(var AIdentifier: integer): boolean;
const OPNAME = 'TGrowthFactorDataSQLAgent.InsertHydrologyGrowthProjection';
begin
    Result := false;
end;

function TGrowthFactorDataSQLAgent.InsertHydrologyGrowthProjectionSQL: string;
const OPNAME = 'TGrowthFactorDataSQLAgent.InsertHydrologyGrowthProjectionSQL';
var
  LSQL : string;
begin
   LSQL := '';
end;

function TGrowthFactorDataSQLAgent.InsertMinMaxChannelGrowthProjection(var AIdentifier: integer): boolean;
const OPNAME = 'TGrowthFactorDataSQLAgent.InsertMinMaxChannelGrowthProjection';
begin
   Result := false;
end;

function TGrowthFactorDataSQLAgent.InsertMinMaxChannelGrowthProjectionSQL: string;
const OPNAME = 'TGrowthFactorDataSQLAgent.InsertMinMaxChannelGrowthProjectionSQL';
var
  LSQL : string;
begin
   LSQL := '';
end;

function TGrowthFactorDataSQLAgent.AddGrowthProjectionConfDataToDB(ABaseYear, AStartYear,AYearsCount,ADataStartYear : Integer): boolean;
const OPNAME = 'TGrowthFactorDataSQLAgent.AddGrowthProjectionConfDataToDB';
var
  lSQL                  : string;
  LDataSet              : TAbstractModelDataset;
begin
  Result := False;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      if Assigned(LDataSet) then
      begin
       lSQL := 'DELETE FROM GrowthFactorExcelConfig WHERE ' +
                  ' (Model         = ' + QuotedStr(FAppModules.StudyArea.ModelCode)     + ') AND ' +
                  ' (StudyAreaName = ' + QuotedStr(FAppModules.StudyArea.StudyAreaCode) + ') AND ' +
                  ' (SubArea       = ' + QuotedStr(FAppModules.StudyArea.SubAreaCode)   + ') AND ' +
                  ' (Scenario      = ' + QuotedStr(FAppModules.StudyArea.ScenarioCode)  + ') ';

        LDataSet.SetSQL(lSQL);
        LDataSet.ExecSQL;
        LDataSet.DataSet.Close;
        LDataSet.ClearSQL;
        LDataSet.ClearQueryParams();

        lSQL := 'INSERT INTO GrowthFactorExcelConfig'+
                '(Model, StudyAreaName, SubArea, Scenario, BaseYear, StartYear,  YearsCount, DataStartYear)'+
                ' VALUES'+
                '(:Model, :StudyAreaName, :SubArea, :Scenario, :BaseYear,:StartYear,:YearsCount, :DataStartYear)';
        LDataSet.SetSQL(lSQL);
        LDataset.ClearQueryParams();
        LDataSet.SetParams(
          ['Model', 'StudyAreaName', 'SubArea', 'Scenario'],
          [FAppModules.StudyArea.ModelCode, FAppModules.StudyArea.StudyAreaCode,
           FAppModules.StudyArea.SubAreaCode, FAppModules.StudyArea.ScenarioCode]);

        LDataSet.SetParams(['BaseYear'],[IntToStr(ABaseYear)]);
        LDataSet.SetParams(['StartYear'], [IntToStr(AStartYear)]);
        LDataSet.SetParams(['YearsCount'], [IntToStr(AYearsCount)]);
        LDataSet.SetParams(['DataStartYear'], [IntToStr(ADataStartYear)]);

        LDataSet.ExecSQL;
        LDataSet.DataSet.Close;
        Result := True;
      end;
    finally
      LDataSet.Free;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;



function TGrowthFactorDataSQLAgent.AddGrowthFactorsConfigDataToDB(AYearsCount: Integer):boolean;
const OPNAME = 'TGrowthFactorDataSQLAgent.AddGrowthFactorsConfigDataToDB';
var
  lSQL                  : string;
  LDataSet              : TAbstractModelDataset;
begin
  Result := False;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      if Assigned(LDataSet) then
      begin
         lSQL := 'DELETE FROM GrowthFactorConfig WHERE ' +
                  ' (Model         = ' + QuotedStr(FAppModules.StudyArea.ModelCode)     + ') AND ' +
                  ' (StudyAreaName = ' + QuotedStr(FAppModules.StudyArea.StudyAreaCode) + ') AND ' +
                  ' (SubArea       = ' + QuotedStr(FAppModules.StudyArea.SubAreaCode)   + ') AND ' +
                  ' (Scenario      = ' + QuotedStr(FAppModules.StudyArea.ScenarioCode)  + ') ';

        LDataSet.SetSQL(lSQL);
        LDataSet.ExecSQL;
        LDataSet.DataSet.Close;
        LDataSet.ClearSQL;
        LDataSet.ClearQueryParams();

        lSQL := 'INSERT INTO GrowthFactorConfig'+
                '(Model, StudyAreaName, SubArea, Scenario, YearsCount)'+
                ' VALUES'+
                '(:Model, :StudyAreaName, :SubArea, :Scenario, :YearsCount )';
        LDataSet.SetSQL(lSQL);
        LDataset.ClearQueryParams();
        LDataSet.SetParams(
          ['Model', 'StudyAreaName', 'SubArea','Scenario','YearsCount'],
          [FAppModules.StudyArea.ModelCode, FAppModules.StudyArea.StudyAreaCode,
           FAppModules.StudyArea.SubAreaCode, FAppModules.StudyArea.ScenarioCode,
           IntToStr(AYearsCount)]);

        LDataSet.ExecSQL;
        LDataSet.DataSet.Close;
        Result := True;
      end;
    finally
      LDataSet.Free;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;



end.
