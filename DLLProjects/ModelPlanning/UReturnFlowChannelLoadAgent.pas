//
//
//  UNIT      : Contains TReturnFlowChannelDataLoadAgent Class
//  AUTHOR    : Sam Dhlamini(Cornastone)
//  DATE      : 19/06/2004
//  COPYRIGHT : Copyright © 2004 DWAF
//
//

unit UReturnFlowChannelLoadAgent;

interface
uses
  Classes,
  UAbstractObject,
  UReturnFlowChannelSQLAgent,
  UReturnFlowChannelData,
  VoaimsCom_TLB,
  UChannelData;
type
  TReturnFlowChannelDataLoadAgent = class(TAbstractAppObject)
  protected
    FSQLAgent: TReturnFlowChannelSQLAgent;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    function LoadMonthlyEvaporation(ADemandChannel:integer; var AMonthlyEvap : TMonthlyDoubleArray) : boolean;
    function LoadCorrespondingChannel(ADemandChannel,AIdentifier:integer;AReturnFlowChannel:TReturnFlowChannel) : boolean;
  public
    function LoadReturnFlowChannelData(AReturnFlowChannelObject:TReturnFlowChannelData) : boolean;
  end;

implementation
uses
  SysUtils,
  UDataSetType,
  UConstants,
  UErrorHandlingOperations, DB;

{ TReturnFlowChannelDataLoadAgent }

procedure TReturnFlowChannelDataLoadAgent.CreateMemberObjects;
const OPNAME = 'TReturnFlowChannelDataLoadAgent.CreateMemberObjects';
begin
  inherited CreateMemberObjects;
  try
    FSQLAgent := TReturnFlowChannelSQLAgent.Create(FAppModules);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TReturnFlowChannelDataLoadAgent.DestroyMemberObjects;
const OPNAME = 'TReturnFlowChannelDataLoadAgent.DestroyMemberObjects';
begin
  try
    FreeAndNil(FSQLAgent);
    inherited DestroyMemberObjects;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TReturnFlowChannelDataLoadAgent.LoadCorrespondingChannel(ADemandChannel, AIdentifier: integer;
                                                                  AReturnFlowChannel: TReturnFlowChannel): boolean;
const OPNAME = 'TReturnFlowChannelDataLoadAgent.LoadCorrespondingChannel';
var
  LCorrespondingChannel : TCorrespondingChannel;
  LDataSet : TAbstractModelDataset;
  LSQL : string;
begin
  Result := False;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      if Assigned(LDataSet) then
      begin
        LSQL := FSQLAgent.GetReturnFlowCorrespondingChannelSQL;
        LDataSet.SetSQL(LSQL +' AND A.DemandChannel = '+IntToStr(ADemandChannel));
        LDataset.DataSet.Open;
        while (not LDataset.DataSet.EOF) do
        begin
          LCorrespondingChannel := AReturnFlowChannel.CreateCorrespondingChannel;
          LCorrespondingChannel.Populate(AIdentifier,ADemandChannel,
                                         LDataset.DataSet.FieldByName('ChannelNumber').AsInteger,
                                         LDataset.DataSet.FieldByName('AbstractionChannel').AsInteger,
                                         LDataset.DataSet.FieldByName('AssumedFactor').AsFloat);
          LDataset.DataSet.Next;
        end;
      end;
      Result := True;
    finally
      FreeAndNil(LDataSet);
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TReturnFlowChannelDataLoadAgent.LoadMonthlyEvaporation(ADemandChannel: integer;
                                                                var AMonthlyEvap: TMonthlyDoubleArray): boolean;
const OPNAME = 'TReturnFlowChannelDataLoadAgent.LoadMonthlyEvaporation';
var
  LDataSet : TAbstractModelDataset;
  LFieldName,
  LSQL : string;
  LIndex : integer;
begin
  Result := False;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      if Assigned(LDataSet) then
      begin
        LSQL := FSQLAgent.GetReturnFlowMonthlyEvaporationSQL;
        LDataSet.SetSQL(LSQL +' AND A.DemandChannel = '+IntToStr(ADemandChannel));
        LDataset.DataSet.Open;
        if (not LDataset.DataSet.EOF) and (LDataset.DataSet.Bof) then
        begin
          for LIndex := MinMonths to MaxMonths do
          begin
            LFieldName := Format('%s%2.2d',['PotentialMonthlyEvap',LIndex]);
            if not LDataset.DataSet.FieldByName(LFieldName).IsNull then
              AMonthlyEvap[LIndex] := LDataset.DataSet.FieldByName(LFieldName).AsFloat;
          end;
        end;
      end;
    Result := True;
    finally
      FreeAndNil(LDataSet);
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TReturnFlowChannelDataLoadAgent.LoadReturnFlowChannelData(AReturnFlowChannelObject: TReturnFlowChannelData): boolean;
const OPNAME = 'TReturnFlowChannelDataLoadAgent.LoadReturnFlowChannelData';
var
  LReturnFlowChannel : TReturnFlowChannel;
  LDataSet : TAbstractModelDataset;
  LSQL : string;
  LIdentifier : integer;
  LDemandChannel : integer;
//  LNumOfCorrChannel : integer;
  LGaugeNumber : integer;
  LMonthlyAvrgFactor : double;
  LCalibrationFactor : double;
  LMonthlyAvrgFactorEvap : double;
  LRoutingConstant : double;
  LCurtailmentFactor : double;
  LMultiplicationFactor : double;
  LMonthlyEvap : TMonthlyDoubleArray;
begin
  Result := False;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      if Assigned(LDataSet) then
      begin
        LSQL := FSQLAgent.GetReturnFlowChannelSQL;
        LDataSet.SetSQL(LSQL);
        LDataset.DataSet.Open;
        while (not LDataset.DataSet.EOF) do
        begin
          LDemandChannel := LDataset.DataSet.FieldByName('DemandChannel').AsInteger;
          LGaugeNumber := LDataset.DataSet.FieldByName('GaugeNumber').AsInteger;
//          LNumOfCorrChannel := LDataset.DataSet.FieldByName('CorrespondingChannels').AsInteger;
          LIdentifier := LDataset.DataSet.FieldByName('Identifier').AsInteger;
          LMonthlyAvrgFactor := LDataset.DataSet.FieldByName('MonthlyAvrgFactor').AsFloat;
          LCalibrationFactor := LDataset.DataSet.FieldByName('CalibrationFactor').AsFloat;
          LMonthlyAvrgFactorEvap := LDataset.DataSet.FieldByName('MonthlyAvrgFactorEvap').AsFloat;
          LRoutingConstant := LDataset.DataSet.FieldByName('RoutingConstant').AsFloat;
          LCurtailmentFactor := LDataset.DataSet.FieldByName('CurtailmentFactor').AsFloat;
          LMultiplicationFactor := LDataset.DataSet.FieldByName('MultiplicationFactor').AsFloat;
          LReturnFlowChannel := AReturnFlowChannelObject.CreateReturnFlowChannel;
          LoadMonthlyEvaporation(LDemandChannel,LMonthlyEvap);
//          if LNumOfCorrChannel > 0 then
          LoadCorrespondingChannel(LDemandChannel,LIdentifier,LReturnFlowChannel);
//          LNumOfCorrChannel := LReturnFlowChannel.NumOfCorrespondingChannels;
          LReturnFlowChannel.Populate(LDemandChannel,LIdentifier,{LNumOfCorrChannel,}LGaugeNumber,
                                      LMonthlyAvrgFactor,LCalibrationFactor,LMonthlyAvrgFactorEvap,
                                      LRoutingConstant,LCurtailmentFactor,LMultiplicationFactor,
                                      LMonthlyEvap);
          LDataset.DataSet.Next;
        end;
        Result := True;
      end;
    finally
      FreeandNil(LDataSet);
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

end.
