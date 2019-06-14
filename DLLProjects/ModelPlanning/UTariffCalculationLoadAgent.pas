//
//
//  UNIT      : Contains TTariffCalculationLoadAgent Class
//  AUTHOR    : Sam Dhlamini(Cornastone)
//  DATE      : 19/06/2004
//  COPYRIGHT : Copyright © 2004 DWAF
//
//

unit UTariffCalculationLoadAgent;

interface
uses
  Classes,
  UAbstractObject,
  UTariffCalculationSQLAgent,
  UTariffCalculationData,
  VoaimsCom_TLB,
  UChannelData;

type
  TTariffCalculationLoadAgent = class(TAbstractAppObject)
  protected
    FSQLAgent: TTariffCalculationSQLAgent;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
  public
    function LoadTariffCalculationData(ATariffCalculationData:TTariffCalculationData) : boolean;
  end;

implementation
uses
  SysUtils,
  UDataSetType,
  UConstants,
  UErrorHandlingOperations, DB;

{ TTariffCalculationLoadAgent }

procedure TTariffCalculationLoadAgent.CreateMemberObjects;
const OPNAME = 'TTariffCalculationLoadAgent.CreateMemberObjects';
begin
  inherited CreateMemberObjects;
  try
    FSQLAgent := TTariffCalculationSQLAgent.Create(FAppModules);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TTariffCalculationLoadAgent.DestroyMemberObjects;
const OPNAME = 'TTariffCalculationLoadAgent.DestroyMemberObjects';
begin
  try
    FreeAndNil(FSQLAgent);
    inherited DestroyMemberObjects;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TTariffCalculationLoadAgent.LoadTariffCalculationData(ATariffCalculationData:TTariffCalculationData): boolean;
const OPNAME = 'TTariffCalculationLoadAgent.LoadReturnFlowChannelData';
var
  LChannelTariff     : TChannelTariff;
  LDataSet           : TAbstractModelDataset;
  LSQL               : string;
  LIdentifier        : integer;
  LChannelNumber     : Integer;
  LDataYears         : Integer;
  LTariff            : Double;
  LEscalationFactors : string;
begin
  Result := False;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      if Assigned(LDataSet) then
      begin
        LSQL := FSQLAgent.GetTariffConfigurationSQL;
        LDataSet.SetSQL(LSQL);
        LDataset.DataSet.Open;
        if not LDataset.DataSet.EOF then
        begin
          LDataYears := LDataset.DataSet.FieldByName('DataYears').AsInteger;
          ATariffCalculationData.Populate(LDataYears);
        end;

        LDataSet.DataSet.Close;
        LSQL := FSQLAgent.GetTariffCalculationSQL;
        LDataSet.SetSQL(LSQL);
        LDataset.DataSet.Open;
        while (not LDataset.DataSet.EOF) do
        begin
          LIdentifier         := LDataset.DataSet.FieldByName('Identifier').AsInteger;
          LChannelNumber      := LDataset.DataSet.FieldByName('ChannelNumber').AsInteger;
          LTariff             := LDataset.DataSet.FieldByName('Tariff').AsFloat;
          LEscalationFactors  := Trim(LDataset.DataSet.FieldByName('EscalationFactors').AsString);

          LChannelTariff := ATariffCalculationData.CreateChannelTariff;
          LChannelTariff.Populate(LIdentifier,LChannelNumber,LTariff,LEscalationFactors);
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
