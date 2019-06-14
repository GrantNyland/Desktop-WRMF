{
  UNIT      : Contains TYMDemandCentreSQLAgent Class
  AUTHOR    : Maurice Marinus
  DATE      : 2006/11/02
  COPYRIGHT : Copyright © 2006 DWAF
}

unit UYMDemandCentreSQLAgent;

interface

uses
  Classes,
  UYMDemandCentre,
  UAbstractObject;

type
  TYMDemandCentreSQLAgent = class(TAbstractSQLAgent)
  protected
    function GetScenarioWhereClause: string;
  public
    procedure LoadContextData_FeatureID(AContextData : TStringList; AFeatureID: string);
    procedure LoadContextData_EvapoTranspiration(AContextData : TStringList; AFeatureID, AFieldNameID : string);
    procedure LoadContextData_FeatureIDReturnFlow(AContextData : TStringList; AFeatureID, ASubFeatureID : string);

    function GetMaxIdentifierSQL(ATableName: string)  : string;
    function GetMaxIdentifier(ATableName : string)    : Integer;
    function GetMaxNodeNumberSQL(ATableName: string)  : string;
    function GetMaxNodeNumber(ATableName: string)     : Integer;

    function GetYMDemandCentreSQL                           : string;

    function DeleteYMDemandCentreSQL(ANodeNumber: Integer)  : string;
    function DeleteYMDemandCentre(ANodeNumber: Integer)             : Boolean;

    function InsertYMDemandCentreSQL                                : string;
    function InsertYMDemandCentre(AYMDemandCentre: TYMDemandCentre) : Boolean;

    function GetReturnFlowFeatureSQL(ADemandCentreID: Integer)  : string;
    function GetMaxReturnFlowFeatureID(ADemandCentreID : Integer) : integer;
    function GetMaxReturnFlowFeatureIDSQL(ADemandCentreID: Integer) : string;
    function InsertReturnFlowFeatureSQL : string;
    function DeleteReturnFlowFeatureSQL (AChannelNr : integer) : string;
    function InsertReturnFlowFeature (AReturnFlow: TYMDemandCentreReturnFlowFeature): boolean;
    function DeleteReturnFlowFeature (AChannelNr : Integer): boolean;
    function CopyYMDemandCentreFromScenario(ASourceStudyAreaName,ASourceSubArea,ASourceScenario: string;
             AYMDemandCentreList: TStrings;AProgressUpdateFuntion: TProgressUpdateFuntion): boolean;

  end;

implementation

uses
  SysUtils,
  UDataSetType,
  UConstants,
  UReservoirDataSQLAgent,
  UChannelDataSQLAgent,
  UErrorHandlingOperations, DB;

function TYMDemandCentreSQLAgent.GetScenarioWhereClause: string;
const OPNAME = 'TYMDemandCentreSQLAgent.GetScenarioWhereClause';
begin
  Result := '';
  try
    Result :=
      ' (A.Model         = ' + QuotedStr(FAppModules.StudyArea.ModelCode)     + ') AND ' +
      ' (A.StudyAreaName = ' + QuotedStr(FAppModules.StudyArea.StudyAreaCode) + ') AND ' +
      ' (A.SubArea       = ' + QuotedStr(FAppModules.StudyArea.SubAreaCode)   + ') AND ' +
      ' (A.Scenario      = ' + QuotedStr(FAppModules.StudyArea.ScenarioCode)  + ') ';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYMDemandCentreSQLAgent.LoadContextData_FeatureID (AContextData : TStringList; AFeatureID: string);
const OPNAME = 'TYMDemandCentreSQLAgent.LoadContextData_FeatureID';
begin
  try
    AContextData.Clear;
    AContextData.Add('Model='              + FAppModules.StudyArea.ModelCode);
    AContextData.Add('StudyAreaName='      + FAppModules.StudyArea.StudyAreaCode);
    AContextData.Add('SubArea='            + FAppModules.StudyArea.SubAreaCode);
    AContextData.Add('Scenario='           + FAppModules.StudyArea.ScenarioCode);
    AContextData.Add('Identifier='         + AFeatureID);
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TYMDemandCentreSQLAgent.GetYMDemandCentreSQL: string;
const OPNAME = 'TYMDemandCentreSQLAgent.GetYMDemandCentreSQL';
begin
  Result := '';
  try
    Result := 'SELECT Model, StudyAreaName, SubArea, Scenario, '+
              ' Identifier, NodeNumber, CentreName, NodeRefNr, '+
              ' AveReturnFlowFactor, AveEvaporation, StdDeviationFactor, RoutingConstant, '+
              ' RainfallScalingFactor, TotalFlowLost, '+
              ' EvapoTranspiration01, EvapoTranspiration02, EvapoTranspiration03, EvapoTranspiration04, '+
              ' EvapoTranspiration05, EvapoTranspiration06, EvapoTranspiration07, EvapoTranspiration08, '+
              ' EvapoTranspiration09, EvapoTranspiration10, EvapoTranspiration11, EvapoTranspiration12, CentreDescription,ConsumptiveChannelNr,ReclaimationChannelNr '+
              ' FROM YMDemandCentre A WHERE '+
    GetScenarioWhereClause +
    ' ORDER BY A.Identifier ';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYMDemandCentreSQLAgent.DeleteYMDemandCentreSQL(ANodeNumber: Integer): string;
const OPNAME = 'TYMDemandCentreSQLAgent.DeleteYMDemandCentreSQL';
begin
  Result := '';
  try
    Result := 'DELETE FROM YMDemandCentre A WHERE ' +
                GetScenarioWhereClause +
                'AND A.NodeNumber = '+ IntToStr(ANodeNumber);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYMDemandCentreSQLAgent.GetMaxIdentifierSQL(ATableName : string): string;
const OPNAME = 'TYMDemandCentreSQLAgent.GetMaxIdentifierSQL';
begin
  Result := '';
  try
    Result := 'SELECT MAX(Identifier) AS MaxIdentifier FROM '+ ATableName +' A WHERE ' +
              GetScenarioWhereClause;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYMDemandCentreSQLAgent.InsertYMDemandCentreSQL: string;
const OPNAME = 'TYMDemandCentreSQLAgent.InsertYMDemandCentreSQL';
begin
  Result := '';
  try
    Result :=
      'INSERT INTO YMDemandCentre '+
      '(Model, StudyAreaName, SubArea, Scenario, Identifier, NodeNumber, CentreName, '+
      ' NodeRefNr, AveReturnFlowFactor, AveEvaporation, StdDeviationFactor, RoutingConstant, '+
      ' RainfallScalingFactor, TotalFlowLost, '+
      ' EvapoTranspiration01, EvapoTranspiration02, EvapoTranspiration03, EvapoTranspiration04, '+
      ' EvapoTranspiration05, EvapoTranspiration06, EvapoTranspiration07, EvapoTranspiration08, '+
      ' EvapoTranspiration09, EvapoTranspiration10, EvapoTranspiration11, EvapoTranspiration12, '+
      ' CentreDescription,ConsumptiveChannelNr,ReclaimationChannelNr) '+
      'Values '+
      '(:Model ,:StudyAreaName, :SubArea, :Scenario, :Identifier, :NodeNumber, :CentreName, '+
      ' :NodeRefNr, :AveReturnFlowFactor, :AveEvaporation, :StdDeviationFactor, :RoutingConstant, '+
      ' :RainfallScalingFactor, :TotalFlowLost, '+
      ' :EvapoTranspiration01, :EvapoTranspiration02, :EvapoTranspiration03, :EvapoTranspiration04, '+
      ' :EvapoTranspiration05, :EvapoTranspiration06, :EvapoTranspiration07, :EvapoTranspiration08, '+
      ' :EvapoTranspiration09, :EvapoTranspiration10, :EvapoTranspiration11, :EvapoTranspiration12, '+
      ' :CentreDescription, :ConsumptiveChannelNr, :ReclaimationChannelNr) ';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYMDemandCentreSQLAgent.GetMaxIdentifier(ATableName : string): Integer;
const OPNAME = 'TYMDemandCentreSQLAgent.GetMaxIdentifier';
var
  LDataSet: TAbstractModelDataset;
begin
  Result := 0;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      if Assigned(LDataSet) then
      begin
        LDataSet.SetSQL(GetMaxIdentifierSQL(ATableName));
        LDataset.DataSet.Open;
        Result := LDataset.DataSet.FieldByName('MaxIdentifier').AsInteger;
        LDataset.DataSet.Close;
      end;
      finally
        LDataset.Free;
      end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYMDemandCentreSQLAgent.DeleteYMDemandCentre(ANodeNumber: Integer): Boolean;
const OPNAME = 'TYMDemandCentreSQLAgent.DeleteYMDemandCentre';
var
  LDataSet    : TAbstractModelDataset;
  LImportDate : TDateTime;
begin
  Result := False;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      if Assigned(LDataSet) then
      begin
        FAppModules.Database.StartTransaction;
        try
          LDataSet.SetSQL(DeleteYMDemandCentreSQL(ANodeNumber));
          LDataset.ExecSQL;
          LDataset.DataSet.Close;

          FAppModules.StudyArea.LastUpdateDate := Now();

           LImportDate := FAppModules.StudyArea.GetStudyImportDate;
           if LImportDate = nullDateTime then
             FAppModules.StudyArea.SetStudyImportDate(FAppModules.StudyArea.LastUpdateDate);

          FAppModules.Database.Commit;
          Result := TRUE;
        except
          FAppModules.Database.Rollback;
          raise;
        end;
      end;
    finally
      LDataset.Free;
    end;
  except on E: Exception do HandleErrorFunction(E, OPNAME, Result) end;
end;


function TYMDemandCentreSQLAgent.InsertYMDemandCentre(AYMDemandCentre: TYMDemandCentre) :  Boolean;
const OPNAME = 'TYMDemandCentreSQLAgent.InsertYMDemandCentre';
var
  LDataSet    : TAbstractModelDataset;
  LImportDate : TDateTime;
  LIdentifier : integer;
  lName       : string;
begin
  Result := False;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      if Assigned(LDataSet) then
      begin
        LIdentifier := GetMaxIdentifier('YMDemandCentre') + 1;
        lName       := 'DEMAND CENTRE (' + IntToStr(LIdentifier)+ ')';
        AYMDemandCentre.PopulateIDs(LIdentifier,lName);
        FAppModules.Database.StartTransaction;
        try
          LDataSet.SetSQL(InsertYMDemandCentreSQL);
          LDataSet.SetParams(['Model'],         [FAppModules.StudyArea.ModelCode]);
          LDataSet.SetParams(['StudyAreaName'], [FAppModules.StudyArea.StudyAreaCode]);
          LDataSet.SetParams(['SubArea'],       [FAppModules.StudyArea.SubAreaCode]);
          LDataSet.SetParams(['Scenario'],      [FAppModules.StudyArea.ScenarioCode]);
          LDataSet.SetParams(['Identifier'],    [IntToStr(AYMDemandCentre.Identifier)]);

          LDataSet.SetParams(['NodeNumber']           , [IntToStr(AYMDemandCentre.NodeNumber)]);
          LDataSet.SetParams(['CentreName']           , [AYMDemandCentre.Name]);
          LDataSet.SetParams(['NodeRefNr']            , [IntToStr(AYMDemandCentre.NodeRefNr)]);
          LDataSet.SetParams(['AveReturnFlowFactor']  , [FloatToStr(AYMDemandCentre.AveReturnFlowFactor)]);
          LDataSet.SetParams(['AveEvaporation']       , [FloatToStr(AYMDemandCentre.AveEvaporation)]);
          LDataSet.SetParams(['StdDeviationFactor']   , [FloatToStr(AYMDemandCentre.StdDeviationFactor)]);
          LDataSet.SetParams(['RoutingConstant']      , [FloatToStr(AYMDemandCentre.RoutingConstant)]);
          LDataSet.SetParams(['RainfallScalingFactor'], [FloatToStr(AYMDemandCentre.RainfallScalingFactor)]);
          LDataSet.SetParams(['TotalFlowLost']        , [FloatToStr(AYMDemandCentre.TotalFlowLost)]);

          LDataSet.SetParams(['EvapoTranspiration01'],  [FloatToStr(AYMDemandCentre.EvapoTranspiration[1])]);
          LDataSet.SetParams(['EvapoTranspiration02'],  [FloatToStr(AYMDemandCentre.EvapoTranspiration[2])]);
          LDataSet.SetParams(['EvapoTranspiration03'],  [FloatToStr(AYMDemandCentre.EvapoTranspiration[3])]);
          LDataSet.SetParams(['EvapoTranspiration04'],  [FloatToStr(AYMDemandCentre.EvapoTranspiration[4])]);
          LDataSet.SetParams(['EvapoTranspiration05'],  [FloatToStr(AYMDemandCentre.EvapoTranspiration[5])]);
          LDataSet.SetParams(['EvapoTranspiration06'],  [FloatToStr(AYMDemandCentre.EvapoTranspiration[6])]);
          LDataSet.SetParams(['EvapoTranspiration07'],  [FloatToStr(AYMDemandCentre.EvapoTranspiration[7])]);
          LDataSet.SetParams(['EvapoTranspiration08'],  [FloatToStr(AYMDemandCentre.EvapoTranspiration[8])]);
          LDataSet.SetParams(['EvapoTranspiration09'],  [FloatToStr(AYMDemandCentre.EvapoTranspiration[9])]);
          LDataSet.SetParams(['EvapoTranspiration10'],  [FloatToStr(AYMDemandCentre.EvapoTranspiration[10])]);
          LDataSet.SetParams(['EvapoTranspiration11'],  [FloatToStr(AYMDemandCentre.EvapoTranspiration[11])]);
          LDataSet.SetParams(['EvapoTranspiration12'],  [FloatToStr(AYMDemandCentre.EvapoTranspiration[12])]);

          LDataSet.SetParams(['CentreDescription']      , [AYMDemandCentre.Description]);
          LDataSet.SetParams(['ConsumptiveChannelNr']   , [IntToStr(AYMDemandCentre.ConsumptiveUseChannelNr)]);
          LDataSet.SetParams(['ReclaimationChannelNr']  , [IntToStr(AYMDemandCentre.ReclaimationChannelNr)]);

          LDataset.ExecSQL;
          LDataset.DataSet.Close;

          FAppModules.StudyArea.LastUpdateDate := Now();

          LImportDate := FAppModules.StudyArea.GetStudyImportDate;
          if LImportDate = nullDateTime then
            FAppModules.StudyArea.SetStudyImportDate(FAppModules.StudyArea.LastUpdateDate);

          FAppModules.Database.Commit;
          Result := True;
        except
          FAppModules.Database.Rollback;
          raise;
        end;
      end;
    finally
      LDataset.Free;
    end;
  except on E: Exception do HandleErrorFunction(E, OPNAME, Result) end;
end;

function TYMDemandCentreSQLAgent.GetMaxNodeNumber(ATableName: string): Integer;
const OPNAME = 'TYMDemandCentreSQLAgent.GetMaxNodeNumber';
var
  LDataSet: TAbstractModelDataset;
begin
  Result := 0;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      if Assigned(LDataSet) then
      begin
        LDataSet.SetSQL(GetMaxNodeNumberSQL(ATableName));
        LDataset.DataSet.Open;
        Result := LDataset.DataSet.FieldByName('MaxNodeNumber').AsInteger;
        LDataset.DataSet.Close;
      end;
      finally
        LDataset.Free;
      end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYMDemandCentreSQLAgent.GetMaxNodeNumberSQL(ATableName: string): string;
const OPNAME = 'TYMDemandCentreSQLAgent.GetMaxNodeNumberSQL';
begin
  Result := '';
  try
    Result := 'SELECT MAX(NodeNumber) AS MaxNodeNumber FROM '+ ATableName +' A WHERE' + GetScenarioWhereClause;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYMDemandCentreSQLAgent.LoadContextData_EvapoTranspiration(AContextData: TStringList; AFeatureID, AFieldNameID: string);
const OPNAME = 'TYMDemandCentreSQLAgent.LoadContextData_EvapoTranspiration';  
begin
  try
    AContextData.Clear;
    AContextData.Add('Model='              + FAppModules.StudyArea.ModelCode);
    AContextData.Add('StudyAreaName='      + FAppModules.StudyArea.StudyAreaCode);
    AContextData.Add('SubArea='            + FAppModules.StudyArea.SubAreaCode);
    AContextData.Add('Scenario='           + FAppModules.StudyArea.ScenarioCode);
    AContextData.Add('Identifier='         + AFeatureID);
    AContextData.Add('FieldNameIdentifier='+ AFieldNameID);
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TYMDemandCentreSQLAgent.GetMaxReturnFlowFeatureID(ADemandCentreID: Integer): integer;
const OPNAME = 'TYMDemandCentreSQLAgent.GetMaxReturnFlowFeatureID';
var
  LDataSet: TAbstractModelDataset;
begin
  Result := 0;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      if Assigned(LDataSet) then
      begin
        LDataSet.SetSQL(GetMaxReturnFlowFeatureIDSQL(ADemandCentreID));
        LDataset.DataSet.Open;
        Result := LDataset.DataSet.FieldByName('MaxIdentifier').AsInteger;
        LDataset.DataSet.Close;
      end;
      finally
        LDataset.Free;
      end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYMDemandCentreSQLAgent.DeleteReturnFlowFeature(AChannelNr : Integer): boolean;
const OPNAME = 'TYMDemandCentreSQLAgent.DeleteReturnFlowFeature';
var
  LDataSet       : TAbstractModelDataset;
  LImportDate    : TDateTime;
begin
  Result := False;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      if Assigned(LDataSet) then
      begin
        LDataSet.SetSQL(DeleteReturnFlowFeatureSQL(AChannelNr));
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

function TYMDemandCentreSQLAgent.DeleteReturnFlowFeatureSQL(AChannelNr: integer): string;
const OPNAME = 'TYMDemandCentreSQLAgent.DeleteReturnFlowFeatureSQL';
begin
  Result := '';
  try
    Result := 'DELETE FROM YMDemandCentreReturnFlowChannel A WHERE ' +
                GetScenarioWhereClause +
                ' AND A.ChannelNr = '+ IntToStr(AChannelNr);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYMDemandCentreSQLAgent.GetMaxReturnFlowFeatureIDSQL(ADemandCentreID: Integer): string;
const OPNAME = 'TYMDemandCentreSQLAgent.GetMaxReturnFlowFeatureIDSQL';
begin
  Result := '';
  try
    Result := 'SELECT MAX(Identifier) AS MaxIdentifier FROM YMDemandCentreReturnFlowChannel A WHERE ' +
              GetScenarioWhereClause + ' AND A.DemandCentreID = '+IntToStr(ADemandCentreID);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYMDemandCentreSQLAgent.InsertReturnFlowFeature(AReturnFlow: TYMDemandCentreReturnFlowFeature): boolean;
const OPNAME = 'TYMDemandCentreSQLAgent.InsertReturnFlowFeature';
var
  LDataSet    : TAbstractModelDataset;
  LFeatureID  : integer;
  LImportDate : TDateTime;
begin
  Result := False;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      if Assigned(LDataSet) then
      begin
        LFeatureID := GetMaxReturnFlowFeatureID(AReturnFlow.DemandCentreID) + 1;
        AReturnFlow.PopulateIDs(LFeatureID);

        LDataSet.SetSQL(InsertReturnFlowFeatureSQL);
        LDataSet.SetParams(['Model'],          [FAppModules.StudyArea.ModelCode]);
        LDataSet.SetParams(['StudyAreaName'],  [FAppModules.StudyArea.StudyAreaCode]);
        LDataSet.SetParams(['SubArea'],        [FAppModules.StudyArea.SubAreaCode]);
        LDataSet.SetParams(['Scenario'],       [FAppModules.StudyArea.ScenarioCode]);
        LDataSet.SetParams(['DemandCentreID'], [IntToStr(AReturnFlow.DemandCentreID)]);
        LDataSet.SetParams(['Identifier'],     [IntToStr(AReturnFlow.FeatureID)]);
        LDataSet.SetParams(['ChannelNr'],      [IntToStr(AReturnFlow.ChannelNr)]);
        LDataSet.SetParams(['TotalReturnFlow'],     [FloatToStr(AReturnFlow.TotalReturnFlow)]);
        LDataSet.SetParams(['FlowDiversion'],     [FloatToStr(AReturnFlow.FlowDiversion)]);

        //if LDataset.AreAllParamsBound then
        begin
          LDataset.ExecSQL;
          LDataset.DataSet.Close;
          FAppModules.StudyArea.LastUpdateDate := Now();

          LImportDate := FAppModules.StudyArea.GetStudyImportDate;
          if LImportDate = NullDateTime then
            FAppModules.StudyArea.SetStudyImportDate(FAppModules.StudyArea.LastUpdateDate);
          Result := True;
        end;
      end;
    finally
      LDataset.Free;
    end;
  except on E: Exception do HandleErrorFunction(E, OPNAME, Result) end;
end;

function TYMDemandCentreSQLAgent.InsertReturnFlowFeatureSQL: string;
const OPNAME = 'TYMDemandCentreSQLAgent.InsertReturnFlowFeatureSQL';
begin
  Result := '';
  try
    Result :=
      ' INSERT INTO YMDemandCentreReturnFlowChannel '+
      ' (Model, StudyAreaName, SubArea, Scenario, DemandCentreID, Identifier, ' +
      ' ChannelNr, TotalReturnFlow, FlowDiversion) ' +
      ' VALUES (:Model, :StudyAreaName, :SubArea, :Scenario, :DemandCentreID, :Identifier, ' +
      ' :ChannelNr, :TotalReturnFlow, :FlowDiversion)';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYMDemandCentreSQLAgent.GetReturnFlowFeatureSQL(ADemandCentreID: Integer): string;
const OPNAME = 'TYMDemandCentreSQLAgent.GetReturnFlowFeatureSQL';
begin
  Result := '';
  try
    Result := ' SELECT Model, StudyAreaName, SubArea, Scenario, '+
              ' DemandCentreID, Identifier, ChannelNr, TotalReturnFlow, FlowDiversion '+
              ' FROM YMDemandCentreReturnFlowChannel A WHERE '+
    GetScenarioWhereClause + ' AND (A.DemandCentreID = '+ IntToStr(ADemandCentreID) + ') ' +
    ' ORDER BY A.Identifier ';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYMDemandCentreSQLAgent.LoadContextData_FeatureIDReturnFlow(AContextData: TStringList; AFeatureID, ASubFeatureID: string);
const OPNAME = 'TYMDemandCentreSQLAgent.LoadContextData_FeatureIDReturnFlow';
begin
  try
    AContextData.Clear;
    AContextData.Add('Model='              + FAppModules.StudyArea.ModelCode);
    AContextData.Add('StudyAreaName='      + FAppModules.StudyArea.StudyAreaCode);
    AContextData.Add('SubArea='            + FAppModules.StudyArea.SubAreaCode);
    AContextData.Add('Scenario='           + FAppModules.StudyArea.ScenarioCode);
    AContextData.Add('DemandCentreID='     + AFeatureID);
    AContextData.Add('Identifier='         + ASubFeatureID);
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TYMDemandCentreSQLAgent.CopyYMDemandCentreFromScenario(ASourceStudyAreaName,ASourceSubArea,ASourceScenario: string;
                                 AYMDemandCentreList: TStrings;AProgressUpdateFuntion: TProgressUpdateFuntion): boolean;
const OPNAME = 'TYMDemandCentreSQLAgent.CopyYMDemandCentreFromScenario';
      YMDemandCentreSQL = 'SELECT * FROM YMDemandCentre WHERE  ';
      YMDemandCentreReturnFlowChannelSQL = 'SELECT * FROM YMDemandCentreReturnFlowChannel WHERE  ';
      ChannelDetailsSQL = 'SELECT * FROM ChannelDetails WHERE ';
var
  LOldConsumptiveChannelNumber      : integer;
  LNewConsumptiveChannelNumber      : integer;
  LNewNodeNumber                    : integer;
  LNodeNumber                       : integer;
  LNewYMDemandCentreIdentifier      : integer;
  LStop                             : boolean;
  LIndex                            : integer;
  LSourceDataSet                    : TAbstractModelDataset;
  LDestinationDataSet               : TAbstractModelDataset;
  LChannelSourceDataSet             : TAbstractModelDataset;
  LChannelDestinationDataSet        : TAbstractModelDataset;
  LImportDate                       : TDateTime;
  LReservoirDataSQLAgent            : TReservoirDataSQLAgent;
  LChannelDataSQLAgent              : TChannelDataSQLAgent;
  LModel                            : string;
  LFieldName                        : string;
  LDestinationStudyAreaName         : string;
  LDestinationSubArea               : string;
  LDestinationScenario              : string;
  LSourceSQL                        : string;
  LDestinationSQL                   : string;
  LSourceWhereClause                : string;
  LDestinationWhereClause           : string;
  LMessage                          : string;
  LYMDemandCentreName               : string;
  LYMDemandCentreIndex              : integer;
  LNewChannelID                     : integer;
  LCurrentIdentifier                : integer;
  LOldChannelNr                     : integer;
  LNewChannelNr                     : integer;
begin
  Result := False;
  try
    if not Assigned(AYMDemandCentreList) then Exit;
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LSourceDataSet);
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDestinationDataSet);
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LChannelSourceDataSet);
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LChannelDestinationDataSet);
    LReservoirDataSQLAgent := TReservoirDataSQLAgent.Create(FAppModules);
    LChannelDataSQLAgent   := TChannelDataSQLAgent.Create(FAppModules);
    try
      if Assigned(LSourceDataSet) and Assigned(LDestinationDataSet) and Assigned(LChannelSourceDataSet)
        and Assigned(LChannelDestinationDataSet) then
      begin
        LModel                    := FAppModules.StudyArea.ModelCode;
        LDestinationStudyAreaName := FAppModules.StudyArea.StudyAreaCode;
        LDestinationSubArea       := FAppModules.StudyArea.SubAreaCode;
        LDestinationScenario      := FAppModules.StudyArea.ScenarioCode;

        LSourceWhereClause  :=     ' (Model         = ' + QuotedStr(LModel)                   + ') AND ' +
                                   ' (StudyAreaName = ' + QuotedStr(ASourceStudyAreaName)     + ') AND ' +
                                   ' (SubArea       = ' + QuotedStr(ASourceSubArea)           + ') AND ' +
                                   ' (Scenario      = ' + QuotedStr(ASourceScenario)          + ')';
        LDestinationWhereClause := ' (Model         = ' + QuotedStr(LModel)                   + ') AND ' +
                                   ' (StudyAreaName = ' + QuotedStr(LDestinationStudyAreaName)+ ') AND ' +
                                   ' (SubArea       = ' + QuotedStr(LDestinationSubArea)      + ') AND ' +
                                   ' (Scenario      = ' + QuotedStr(LDestinationScenario)     + ')';
      end;
      try
        FAppModules.Database.StartTransaction;
        LNewYMDemandCentreIdentifier := GetMaxIdentifier('YMDemandCentre');
        //LCurrentIdentifier := 0;
        LNewNodeNumber := LReservoirDataSQLAgent.GetMaxReservoirNumber;
        LReservoirDataSQLAgent.CopyReservoirsFromScenario(ASourceStudyAreaName,ASourceSubArea,ASourceScenario,
                               AYMDemandCentreList,AProgressUpdateFuntion);
        LNewConsumptiveChannelNumber := 0;
        LNewChannelNr := 0;
        for LYMDemandCentreIndex := 0 to AYMDemandCentreList.Count-1 do
        begin
          LNodeNumber := integer(AYMDemandCentreList.Objects[LYMDemandCentreIndex]);
          LYMDemandCentreName := AYMDemandCentreList[LYMDemandCentreIndex];
          LNewNodeNumber := LNewNodeNumber + 1;
          LNewYMDemandCentreIdentifier := LNewYMDemandCentreIdentifier + 1;
          LMessage := 'Copying YMDemand Centre ('+LYMDemandCentreName+') ' + IntToStr(LYMDemandCentreIndex+1) + ' of '+ IntToStr(AYMDemandCentreList.Count);
          AProgressUpdateFuntion(LMessage,ptNone,LStop,True);
          if LStop then
          begin
            if FAppModules.Database.InTransaction then
              FAppModules.Database.Rollback;
            Exit;
          end;
          //________________________________________________________ YMDemandCentre ____________________________
          LSourceSQL := YMDemandCentreSQL + LSourceWhereClause + ' AND NodeNumber = '+ IntToStr(LNodeNumber);
          LSourceDataSet.DataSet.Close;
          LSourceDataSet.SetSQL(LSourceSQL);
          LSourceDataSet.DataSet.Open;
          if LSourceDataSet.DataSet.Eof then Continue;

          LDestinationSQL := YMDemandCentreSQL + LDestinationWhereClause;
          LDestinationDataSet.DataSet.Close;
          LDestinationDataSet.SetSQL(LDestinationSQL);
          LDestinationDataSet.SetReadOnly(False);
          LDestinationDataSet.DataSet.Open;
          if LDestinationDataSet.IsReadOnly  then
          begin
            LDestinationDataSet.DataSet.Close;
            raise Exception.Create('Query to table YMDemandCentre cannot be set to updatable.');
          end
          else
          begin
            LDestinationDataSet.DataSet.Append;
            LOldConsumptiveChannelNumber := LSourceDataSet.DataSet.FieldByName('ConsumptiveChannelNr').AsInteger;
            LCurrentIdentifier := LSourceDataSet.DataSet.FieldByName('Identifier').AsInteger;
            for LIndex := 0 to  LDestinationDataSet.DataSet.FieldCount-1 do
            begin
              LFieldName := LDestinationDataSet.DataSet.Fields[LIndex].FieldName;
              LDestinationDataSet.DataSet.FieldByName(LFieldName).Value :=
              LSourceDataSet.DataSet.FieldByName(LFieldName).Value;
            end;

            //________________________________________________________ ChannelDetails ____________________________
            LSourceSQL := ChannelDetailsSQL + LSourceWhereClause + ' AND ChannelNumber = '+ IntToStr(LOldConsumptiveChannelNumber);
            LChannelSourceDataSet.DataSet.Close;
            LChannelSourceDataSet.SetSQL(LSourceSQL);
            LChannelSourceDataSet.DataSet.Open;
            if not LChannelSourceDataSet.DataSet.Eof then
            begin
              LDestinationSQL := ChannelDetailsSQL + LDestinationWhereClause;
              LChannelDestinationDataSet.DataSet.Close;
              LChannelDestinationDataSet.SetSQL(LDestinationSQL);
              LChannelDestinationDataSet.SetReadOnly(False);
              LChannelDestinationDataSet.DataSet.Open;
              if LChannelDestinationDataSet.IsReadOnly  then
              begin
                LChannelDestinationDataSet.DataSet.Close;
                raise Exception.Create('Query to table ChannelDetails cannot be set to updatable.');
              end
              else
              begin
                LNewChannelID := LChannelDataSQLAgent.GetMaxChannelIdentifier+1;
                LNewConsumptiveChannelNumber := LChannelDataSQLAgent.GetMaxChannelNumber+1;
                LChannelDestinationDataSet.DataSet.Append;
                for LIndex := 0 to  LChannelDestinationDataSet.DataSet.FieldCount-1 do
                begin
                  LFieldName := LChannelDestinationDataSet.DataSet.Fields[LIndex].FieldName;
                  LChannelDestinationDataSet.DataSet.FieldByName(LFieldName).Value :=
                  LChannelSourceDataSet.DataSet.FieldByName(LFieldName).Value;
                end;
                LChannelDestinationDataSet.DataSet.FieldByName('Model').AsString                     := LModel;
                LChannelDestinationDataSet.DataSet.FieldByName('StudyAreaName').AsString             := LDestinationStudyAreaName;
                LChannelDestinationDataSet.DataSet.FieldByName('SubArea').AsString                   := LDestinationSubArea;
                LChannelDestinationDataSet.DataSet.FieldByName('Scenario').AsString                  := LDestinationScenario;
                LChannelDestinationDataSet.DataSet.FieldByName('Identifier').AsInteger               := LNewChannelID;
                LChannelDestinationDataSet.DataSet.FieldByName('ChannelNumber').AsInteger            := LNewConsumptiveChannelNumber;
                LChannelDestinationDataSet.DataSet.FieldByName('PenaltyNumber').AsInteger            := 0;
                LChannelDestinationDataSet.DataSet.FieldByName('DownNodeNumber').AsInteger           := 0;
                LChannelDestinationDataSet.DataSet.FieldByName('UpNodeNumber').AsInteger             := LNewNodeNumber;
                LChannelDestinationDataSet.DataSet.Post;
              end;
            end;
            LDestinationDataSet.DataSet.FieldByName('Model').AsString                     := LModel;
            LDestinationDataSet.DataSet.FieldByName('StudyAreaName').AsString             := LDestinationStudyAreaName;
            LDestinationDataSet.DataSet.FieldByName('SubArea').AsString                   := LDestinationSubArea;
            LDestinationDataSet.DataSet.FieldByName('Scenario').AsString                  := LDestinationScenario;
            LDestinationDataSet.DataSet.FieldByName('Identifier').AsInteger               := LNewYMDemandCentreIdentifier;
            LDestinationDataSet.DataSet.FieldByName('NodeNumber').AsInteger               := LNewNodeNumber;
            LDestinationDataSet.DataSet.FieldByName('NodeRefNr').AsInteger                := 0;
            LDestinationDataSet.DataSet.FieldByName('ConsumptiveChannelNr').AsInteger     := LNewConsumptiveChannelNumber;
            LDestinationDataSet.DataSet.FieldByName('ReclaimationChannelNr').AsInteger    := 0;
            LDestinationDataSet.DataSet.Post;
          end;
          //____________________________________________________ YMDemandCentreReturnFlowChannel __________________________________
          LSourceSQL := YMDemandCentreReturnFlowChannelSQL + LSourceWhereClause + ' AND DemandCentreID = '+ IntToStr(LCurrentIdentifier);
          LSourceDataSet.DataSet.Close;
          LSourceDataSet.SetSQL(LSourceSQL);
          LSourceDataSet.DataSet.Open;
          while not LSourceDataSet.DataSet.Eof do
          begin
            LOldChannelNr := LSourceDataSet.DataSet.FieldByName('ChannelNr').AsInteger;
            LDestinationSQL := YMDemandCentreReturnFlowChannelSQL + LDestinationWhereClause;
            LDestinationDataSet.DataSet.Close;
            LDestinationDataSet.SetSQL(LDestinationSQL);
            LDestinationDataSet.SetReadOnly(False);
            LDestinationDataSet.DataSet.Open;
            if LDestinationDataSet.IsReadOnly  then
            begin
              LDestinationDataSet.DataSet.Close;
              raise Exception.Create('Query to table YMDemandCentreReturnFlowChannel cannot be set to updatable.');
            end
            else
            begin
              LDestinationDataSet.DataSet.Append;
              for LIndex := 0 to  LDestinationDataSet.DataSet.FieldCount-1 do
              begin
                LFieldName := LDestinationDataSet.DataSet.Fields[LIndex].FieldName;
                LDestinationDataSet.DataSet.FieldByName(LFieldName).Value :=
                LSourceDataSet.DataSet.FieldByName(LFieldName).Value;
              end;
              if (LOldChannelNr > 0) then
              begin
                //________________________________________________________ ChannelDetails ____________________________
                LSourceSQL := ChannelDetailsSQL + LSourceWhereClause + ' AND ChannelNumber = '+ IntToStr(LOldChannelNr);
                LChannelSourceDataSet.DataSet.Close;
                LChannelSourceDataSet.SetSQL(LSourceSQL);
                LChannelSourceDataSet.DataSet.Open;
                if not LChannelSourceDataSet.DataSet.Eof then
                begin
                  LDestinationSQL := ChannelDetailsSQL + LDestinationWhereClause;
                  LChannelDestinationDataSet.DataSet.Close;
                  LChannelDestinationDataSet.SetSQL(LDestinationSQL);
                  LChannelDestinationDataSet.SetReadOnly(False);
                  LChannelDestinationDataSet.DataSet.Open;
                  if LChannelDestinationDataSet.IsReadOnly  then
                  begin
                    LChannelDestinationDataSet.DataSet.Close;
                    raise Exception.Create('Query to table ChannelDetails cannot be set to updatable.');
                  end
                  else
                  begin
                    LNewChannelID := LChannelDataSQLAgent.GetMaxChannelIdentifier+1;
                    LNewChannelNr := LChannelDataSQLAgent.GetMaxChannelNumber+1;
                    LChannelDestinationDataSet.DataSet.Append;
                    for LIndex := 0 to  LChannelDestinationDataSet.DataSet.FieldCount-1 do
                    begin
                      LFieldName := LChannelDestinationDataSet.DataSet.Fields[LIndex].FieldName;
                      LChannelDestinationDataSet.DataSet.FieldByName(LFieldName).Value :=
                      LChannelSourceDataSet.DataSet.FieldByName(LFieldName).Value;
                    end;
                    LChannelDestinationDataSet.DataSet.FieldByName('Model').AsString                     := LModel;
                    LChannelDestinationDataSet.DataSet.FieldByName('StudyAreaName').AsString             := LDestinationStudyAreaName;
                    LChannelDestinationDataSet.DataSet.FieldByName('SubArea').AsString                   := LDestinationSubArea;
                    LChannelDestinationDataSet.DataSet.FieldByName('Scenario').AsString                  := LDestinationScenario;
                    LChannelDestinationDataSet.DataSet.FieldByName('Identifier').AsInteger               := LNewChannelID;
                    LChannelDestinationDataSet.DataSet.FieldByName('ChannelNumber').AsInteger            := LNewChannelNr;
                    LChannelDestinationDataSet.DataSet.FieldByName('PenaltyNumber').AsInteger            := 0;
                    LChannelDestinationDataSet.DataSet.FieldByName('DownNodeNumber').AsInteger           := 0;
                    LChannelDestinationDataSet.DataSet.FieldByName('UpNodeNumber').AsInteger             := LNewNodeNumber;
                    LChannelDestinationDataSet.DataSet.Post;
                  end;
                end;  
                LDestinationDataSet.DataSet.FieldByName('Model').AsString                     := LModel;
                LDestinationDataSet.DataSet.FieldByName('StudyAreaName').AsString             := LDestinationStudyAreaName;
                LDestinationDataSet.DataSet.FieldByName('SubArea').AsString                   := LDestinationSubArea;
                LDestinationDataSet.DataSet.FieldByName('Scenario').AsString                  := LDestinationScenario;
                LDestinationDataSet.DataSet.FieldByName('Identifier').AsInteger               := GetMaxReturnFlowFeatureID(LNewYMDemandCentreIdentifier)+1;
                LDestinationDataSet.DataSet.FieldByName('ChannelNr').AsInteger                := LNewChannelNr;
                LDestinationDataSet.DataSet.FieldByName('DemandCentreID').AsInteger           := LNewYMDemandCentreIdentifier;
                LDestinationDataSet.DataSet.Post;
              end;
              LSourceDataSet.DataSet.Next;
            end;
          end;
        end;
        FAppModules.StudyArea.LastUpdateDate := Now();
        LImportDate := FAppModules.StudyArea.GetStudyImportDate;
        if LImportDate = NullDateTime then
          FAppModules.StudyArea.SetStudyImportDate(FAppModules.StudyArea.LastUpdateDate);
        FAppModules.Database.Commit;
        Result := True;
      except
        FAppModules.Database.Rollback;
        raise;
      end;
    finally
      LSourceDataSet.Free;
      LDestinationDataSet.Free;
      LChannelSourceDataSet.Free;
      LChannelDestinationDataSet.Free;
      LReservoirDataSQLAgent.Free;
      LChannelDataSQLAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.




