//
//
//  UNIT      : Contains TReservoirDataLoadAgent Class
//  AUTHOR    : Titi Ngubane (PDNA)
//  DATE      : 2003/02/25
//  COPYRIGHT : Copyright © 2003 DWAF
//
//
unit UChannelPlanningLoadAgent;

interface

uses
  Classes,
  UAbstractObject,
  UChannelData;

type
  TChannelPlanningLoadAgent = class(TAbstractAppObject)
  private
  protected
    function GetScenarioWhereClause: string;
    function GetMaxChannelTimeControlID: integer;
  public
    function LoadChannelTimeControl(AChannelList: TChannelList): boolean;
    procedure LoadContextData_ChannelTimeControl(AContextData   : TStringList;
                                                 AIdentifier,AChannelNumber : string);
    function InsertChannelTimeControl (AIdentifier,AChannelNr   : integer;
                                       AChannelType : integer): boolean;
    function DeleteChannelTimeControl (AIdentifier,AChannelNr : integer): boolean;
    function LoadChannelSwitchControl(AChannelList: TChannelList): boolean;
    procedure LoadContextData_ChannelSwitchControl(AContextData     : TStringList;
                                                   AChannelSwitchID : string);
    function InsertChannelSwitchControl (AChannelNr    : integer;
                                         var ASwitchID : integer): boolean;
    function DeleteChannelSwitchControl (AChannelSwitchID : integer): boolean;
  end;

implementation

uses
  SysUtils,
  VoaimsCom_TLB,
  UConstants,
  UDataSetType,
  UChannelPlanningData,
  UErrorHandlingOperations, DB;

function TChannelPlanningLoadAgent.GetScenarioWhereClause: string;
const OPNAME = 'TChannelPlanningLoadAgent.GetScenarioWhereClause';
begin
  Result := '';
  try
    Result :=
      ' (Model         = ' + QuotedStr(FAppModules.StudyArea.ModelCode)     + ') AND ' +
      ' (StudyAreaName = ' + QuotedStr(FAppModules.StudyArea.StudyAreaCode) + ') AND ' +
      ' (SubArea       = ' + QuotedStr(FAppModules.StudyArea.SubAreaCode)   + ') AND ' +
      ' (Scenario      = ' + QuotedStr(FAppModules.StudyArea.ScenarioCode)  + ') ';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TChannelPlanningLoadAgent.LoadChannelTimeControl (AChannelList : TChannelList): boolean;
const OPNAME = 'TChannelPlanningLoadAgent.LoadChannelTimeControl';
var
  LChannelTimeControl         : TChannelTimeControl;
  LDataSet                    : TAbstractModelDataset;
  LIdentifier                 : integer;
  LChannelNumber              : integer;
  LChannelStartYear           : integer;
  LChannelStartMonth          : integer;
  LChannelEndYear             : integer;
  LChannelEndMonth            : integer;
  LChannelEconomicLife        : integer;
  LChannelCapitalCost         : double;
  LChannelFixedOMCost         : double;
  LChannelVariableOMCost      : double;
  LChannelCostSchedule        : string;
  LChannelEscalationCost      : string;
  lSQL                        : string;
  lChannel                    : TGeneralFlowChannel;
begin
  Result := False;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      if Assigned(LDataSet) then
      begin
        lSQL := 'SELECT * FROM ChannelTimeControl WHERE ' +
        GetScenarioWhereClause ;
        LDataSet.SetSQL(lSQL);
        LDataSet.DataSet.Open;
        Result := TRUE;
        while not LDataSet.DataSet.Eof do
        begin
          LIdentifier                  := LDataSet.DataSet.FieldbyName('Identifier').AsInteger;
          LChannelNumber               := LDataSet.DataSet.FieldbyName('ChannelNumber').AsInteger;
          LChannelStartYear            := LDataset.DataSet.FieldByName('ChannelStartYear').AsInteger;
          LChannelStartMonth           := LDataset.DataSet.FieldByName('ChannelStartMonth').AsInteger;
          LChannelEndYear              := LDataset.DataSet.FieldByName('ChannelEndYear').AsInteger;
          LChannelEndMonth             := LDataset.DataSet.FieldByName('ChannelEndMonth').AsInteger;
          LChannelEconomicLife         := LDataset.DataSet.FieldByName('ChannelEconomicLife').AsInteger;
          LChannelCapitalCost          := LDataset.DataSet.FieldByName('ChannelCapitalCost').AsFloat;
          LChannelFixedOMCost          := LDataset.DataSet.FieldByName('ChannelFixedOMCost').AsFloat;
          LChannelVariableOMCost       := LDataset.DataSet.FieldByName('ChannelVariableOMCost').AsFloat;
          LChannelCostSchedule         := Trim(LDataset.DataSet.FieldByName('ChannelCostSchedule').AsString);
          LChannelEscalationCost       := Trim(LDataset.DataSet.FieldByName('ChannelEscalationCost').AsString);

          lChannel                     := AChannelList.CastChannelByChannelNumber[LChannelNumber];
          if Assigned(lChannel) then
          begin
            LChannelTimeControl        := lChannel.CreateTimeControl;
            if Assigned(LChannelTimeControl) then
            begin
              LChannelTimeControl.Populate(LIdentifier,LChannelNumber,LChannelStartYear,LChannelStartMonth,LChannelEndYear,
                                           LChannelEndMonth,LChannelEconomicLife,LChannelCapitalCost,LChannelFixedOMCost,
                                           LChannelVariableOMCost,LChannelCostSchedule,LChannelEscalationCost);
            end;
          end;
          LDataSet.DataSet.Next;
        end;
        Result := TRUE;
        LDataSet.DataSet.Close;
      end;
    finally
      LDataSet.Free;
    end;
  except on E: Exception do HandleErrorFunction(E, OPNAME,Result) end;
end;

procedure TChannelPlanningLoadAgent.LoadContextData_ChannelTimeControl(AContextData  : TStringList;
                                                                       AIdentifier,AChannelNumber: string);
const OPNAME = 'TChannelPlanningLoadAgent.LoadContextData_ChannelTimeControl';
begin
  try
    AContextData.Clear;
    AContextData.Add('Model=' +FAppModules.StudyArea.ModelCode);
    AContextData.Add('StudyAreaName=' +FAppModules.StudyArea.StudyAreaCode);
    AContextData.Add('SubArea=' +FAppModules.StudyArea.SubAreaCode);
    AContextData.Add('Scenario=' +FAppModules.StudyArea.ScenarioCode);
    AContextData.Add('Identifier=' + AIdentifier);
    AContextData.Add('ChannelNumber=' + AChannelNumber);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TChannelPlanningLoadAgent.GetMaxChannelTimeControlID: integer;
const OPNAME = 'TChannelPlanningLoadAgent.GetMaxChannelTimeControlID';
var
  lDataSet : TAbstractModelDataset;
  lSQL     : string;
begin
  Result := 0;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), lDataSet);
    try
      if Assigned(lDataSet) then
      begin
        lSQL := 'SELECT MAX(ChannelSwitchID) AS MaxIdentifier FROM ChannelSwitchControl WHERE ' +
                GetScenarioWhereClause;
        lDataSet.SetSQL(lSQL);
        lDataset.DataSet.Open;
        Result := lDataset.DataSet.FieldByName('MaxIdentifier').AsInteger;
        lDataset.DataSet.Close;
      end;
      finally
        lDataset.Free;
      end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TChannelPlanningLoadAgent.InsertChannelTimeControl (AIdentifier,AChannelNr   : integer;
                                                             AChannelType : integer): boolean;
const OPNAME = 'TChannelPlanningLoadAgent.InsertChannelTimeControl';
var
  LDataSet     : TAbstractModelDataset;
  LImportDate  : TDateTime;
  lSQL         : string;
begin
  Result := False;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      if Assigned(LDataSet) then
      begin
        lSQL := 'INSERT INTO ChannelTimeControl '+
                  '(Model, StudyAreaName, SubArea, Scenario,Identifier, ChannelNumber, ChannelType, ' +
                  'ChannelStartYear, ChannelStartMonth, ChannelEndYear, ChannelEndMonth, ' +
                  'ChannelEconomicLife, ChannelCapitalCost, ChannelFixedOMCost,' +
                  'ChannelVariableOMCost) VALUES (' +
                  QuotedStr(FAppModules.StudyArea.ModelCode) + ','+
                  QuotedStr(FAppModules.StudyArea.StudyAreaCode) + ','+
                  QuotedStr(FAppModules.StudyArea.SubAreaCode) + ','+
                  QuotedStr(FAppModules.StudyArea.ScenarioCode) + ','+
                  IntToStr(AIdentifier) + ',' + IntToStr(AChannelNr) + ',' + IntToStr(AChannelType) + ','+
                  '0,0,0,0,0,0.0,0.0,0.0' + ')';

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

function TChannelPlanningLoadAgent.DeleteChannelTimeControl (AIdentifier,AChannelNr : integer): boolean;
const OPNAME = 'TChannelPlanningLoadAgent.DeleteChannelTimeControl';
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
        lSQL := 'DELETE FROM ChannelTimeControl WHERE ' +
                GetScenarioWhereClause +
                ' AND Identifier = ' + IntToStr(AIdentifier) +
                ' AND ChannelNumber = ' + IntToStr(AChannelNr);

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

function TChannelPlanningLoadAgent.LoadChannelSwitchControl (AChannelList : TChannelList): boolean;
const OPNAME = 'TChannelPlanningLoadAgent.LoadChannelSwitchControl';
var
  lChannelSwitchControl : TChannelSwitchControl;
  lDataSet              : TAbstractModelDataset;
  lChannelSwitchID      : integer;
  lChannelNumber        : integer;
  lSwitchDefID          : integer;
  lAssociatedNodeNr     : integer;
  lWaterLevel           : double;
  lSwitchType           : integer;
  lInitialStatus        : integer;
  lSQL                  : string;
  lChannel              : TGeneralFlowChannel;
begin
  Result := FALSE;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), lDataSet);
    try
      if Assigned(lDataSet) then
      begin
        lSQL := 'SELECT * FROM ChannelSwitchControl WHERE ' +
        GetScenarioWhereClause ;
        LDataSet.SetSQL(lSQL);
        LDataSet.DataSet.Open;
        Result := TRUE;
        while not LDataSet.DataSet.Eof do
        begin
          lChannelSwitchID  := LDataSet.DataSet.FieldbyName('ChannelSwitchID').AsInteger;
          lChannelNumber    := LDataSet.DataSet.FieldbyName('ChannelNumber').AsInteger;
          lSwitchDefID      := LDataset.DataSet.FieldByName('SwitchDefinitionID').AsInteger;
          lAssociatedNodeNr := LDataset.DataSet.FieldByName('SwitchAssociatedNodeNr').AsInteger;
          lWaterLevel       := LDataset.DataSet.FieldByName('SwitchWaterLevel').AsFloat;
          lSwitchType       := LDataset.DataSet.FieldByName('SwitchType').AsInteger;
          lInitialStatus    := LDataset.DataSet.FieldByName('SwitchInitialStatus').AsInteger;

          lChannel          := AChannelList.CastChannelByChannelNumber[lChannelNumber];
          if Assigned(lChannel) then
          begin
            lChannelSwitchControl := lChannel.CreateSwitchControl(lChannelSwitchID);
            if Assigned(lChannelSwitchControl) then
            begin
              lChannelSwitchControl.Populate(lChannelSwitchID, lChannelNumber, lSwitchDefID,
                                             lAssociatedNodeNr, lWaterLevel, lSwitchType, lInitialStatus);
            end;
          end;
          lDataSet.DataSet.Next;
        end;
        Result := TRUE;
        lDataSet.DataSet.Close;
      end;
    finally
      lDataSet.Free;
    end;
  except on E: Exception do HandleErrorFunction(E, OPNAME,Result) end;
end;

procedure TChannelPlanningLoadAgent.LoadContextData_ChannelSwitchControl(AContextData     : TStringList;
                                                                         AChannelSwitchID : string);
const OPNAME = 'TChannelPlanningLoadAgent.LoadContextData_ChannelSwitchControl';
begin
  try
    AContextData.Clear;
    AContextData.Add('Model=' +FAppModules.StudyArea.ModelCode);
    AContextData.Add('StudyAreaName=' +FAppModules.StudyArea.StudyAreaCode);
    AContextData.Add('SubArea=' +FAppModules.StudyArea.SubAreaCode);
    AContextData.Add('Scenario=' +FAppModules.StudyArea.ScenarioCode);
    AContextData.Add('ChannelSwitchID=' + AChannelSwitchID);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TChannelPlanningLoadAgent.InsertChannelSwitchControl (AChannelNr    : integer;
                                                               var ASwitchID : integer): boolean;
const OPNAME = 'TChannelPlanningLoadAgent.InsertChannelSwitchControl';
var
  lDataSet    : TAbstractModelDataset;
  lImportDate : TDateTime;
  lSQL        : string;
begin
  Result := False;
  try
    ASwitchID := GetMaxChannelTimeControlID + 1;
    FAppModules.Database.CreateDataset(integer(dtExecSQL), lDataSet);
    try
      if Assigned(lDataSet) then
      begin
        lSQL := 'INSERT INTO ChannelSwitchControl '+
                  '(Model, StudyAreaName, SubArea, Scenario, ChannelSwitchID, ChannelNumber, ' +
                  'SwitchDefinitionID, SwitchAssociatedNodeNr, SwitchWaterLevel, SwitchType, ' +
                  'SwitchInitialStatus) VALUES (' +
                  QuotedStr(FAppModules.StudyArea.ModelCode) + ','+
                  QuotedStr(FAppModules.StudyArea.StudyAreaCode) + ','+
                  QuotedStr(FAppModules.StudyArea.SubAreaCode) + ','+
                  QuotedStr(FAppModules.StudyArea.ScenarioCode) + ','+
                  IntToStr(ASwitchID) + ',' +
                  IntToStr(AChannelNr) +
                  ',0,0,0.0,0,0)';

        lDataSet.SetSQL(lSQL);
        lDataSet.ExecSQL;
        lDataSet.DataSet.Close;

        FAppModules.StudyArea.LastUpdateDate := Now();

       lImportDate := FAppModules.StudyArea.GetStudyImportDate;
        if lImportDate = NullDateTime then
          FAppModules.StudyArea.SetStudyImportDate(FAppModules.StudyArea.LastUpdateDate);

        Result := TRUE;
      end;
    finally
      lDataset.Free;
    end;
  except on E: Exception do HandleErrorFunction(E, OPNAME, Result) end;
end;

function TChannelPlanningLoadAgent.DeleteChannelSwitchControl (AChannelSwitchID : integer): boolean;
const OPNAME = 'TChannelPlanningLoadAgent.DeleteChannelSwitchControl';
var
  lDataSet    : TAbstractModelDataset;
  lImportDate : TDateTime;
  lSQL        : string;
begin
  Result := FALSE;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), lDataSet);
    try
      if Assigned(LDataSet) then
      begin
        lSQL := 'DELETE FROM ChannelSwitchControl WHERE ' +
                GetScenarioWhereClause +
                'AND ChannelSwitchID = ' + IntToStr(AChannelSwitchID);

        lDataSet.SetSQL(lSQL);
        lDataSet.ExecSQL;
        lDataSet.DataSet.Close;

        FAppModules.StudyArea.LastUpdateDate := Now();

        lImportDate := FAppModules.StudyArea.GetStudyImportDate;
        if lImportDate = NullDateTime then
          FAppModules.StudyArea.SetStudyImportDate(FAppModules.StudyArea.LastUpdateDate);

        Result := TRUE;
      end;
    finally
      LDataset.Free;
    end;
  except on E: Exception do HandleErrorFunction(E, OPNAME, Result) end;
end;

end.

