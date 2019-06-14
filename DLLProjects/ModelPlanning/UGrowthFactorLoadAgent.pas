//
//
//  UNIT      : Contains TGrowthFactorDataLoadAgent Class
//  AUTHOR    : Sam Dhlamini(Cornastone)
//  DATE      : 11/05/2006
//  COPYRIGHT : Copyright © 2004 DWAF
//
//


unit UGrowthFactorLoadAgent;

interface

uses
  Classes,
  UAbstractObject,
  UGrowthFactorDataSQLAgent,
  UGrowthFactorData,
  VoaimsCom_TLB,
  UChannelData;
type
  TGrowthFactorDataLoadAgent = class(TAbstractAppObject)
  protected
    FSQLAgent: TGrowthFactorDataSQLAgent;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    function LoadGrowthFactorConfigData(AGrowthFactors:TGrowthFactors): boolean;
    function LoadDemandGrowthFactorData(AGrowthFactors:TGrowthFactors): boolean;
    function LoadMinMaxChannelGrowthFactorData(AGrowthFactors:TGrowthFactors): boolean;
    function LoadHydrologyGrowthFactorData(AGrowthFactors:TGrowthFactors): boolean;
  public
    function LoadGrowthFactors(AGrowthFactors:TGrowthFactors) : boolean;
  end;

implementation

uses
  SysUtils,
  UDataSetType,
  UConstants,
  UGrowthFactorsExcelData,
  UPlanningModelDataObject,
  UErrorHandlingOperations;

{ TGrowthFactorDataLoadAgent }

procedure TGrowthFactorDataLoadAgent.CreateMemberObjects;
const OPNAME = 'TGrowthFactorDataLoadAgent.CreateMemberObjects';
begin
  try
    FSQLAgent :=  TGrowthFactorDataSQLAgent.Create(FAppModules);
    inherited CreateMemberObjects;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TGrowthFactorDataLoadAgent.DestroyMemberObjects;
const OPNAME = 'TGrowthFactorDataLoadAgent.DestroyMemberObjects';
begin
  try
    FreeandNil(FSQLAgent);
    inherited DestroyMemberObjects;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TGrowthFactorDataLoadAgent.LoadGrowthFactors(AGrowthFactors: TGrowthFactors): boolean;
const OPNAME = 'TGrowthFactorDataLoadAgent.LoadGrowthFactors';
var
  LGrowthFactors : TExelGrowthFactors;
begin
  Result := False;
  try
    Result := LoadGrowthFactorConfigData(AGrowthFactors) and
              LoadDemandGrowthFactorData(AGrowthFactors) and
              LoadMinMaxChannelGrowthFactorData(AGrowthFactors) and
              LoadHydrologyGrowthFactorData(AGrowthFactors);

   LGrowthFactors := TPlanningModelDataObject(FAppModules.Model.ModelData).CastExelGrowthFactors;
    if not LGrowthFactors.Populated  then
      LGrowthFactors.LoadFromDB;

  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TGrowthFactorDataLoadAgent.LoadDemandGrowthFactorData(AGrowthFactors:TGrowthFactors): boolean;
const OPNAME = 'TGrowthFactorDataLoadAgent.LoadDemandGrowthFactorData';
var
  LDemandCentreGrowthFactors: TDemandCentreGrowthFactors;
  LDataSet     : TAbstractModelDataset;
  LSQL         : string;
  LIdentifier  : integer;
  LChannelNumber: integer;
  LGrowthFactor: string;
  LValidFactors : boolean;
begin
  Result := False;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), lDataSet);
    try
      if Assigned(LDataSet) then
      begin
        LSQL := FSQLAgent.GetGrowthFactorDemandSQL;
        LDataSet.SetSQL(lSQL);
        LDataset.DataSet.Open;
        while (not LDataset.DataSet.EOF) do
        begin
          LIdentifier   := LDataset.DataSet.FieldByName('Identifier').AsInteger;
          LChannelNumber:= LDataset.DataSet.FieldByName('ChannelNumber').AsInteger;
          LGrowthFactor := Trim(LDataset.DataSet.FieldByName('Factors').AsString);
          LValidFactors := (Trim(LDataset.DataSet.FieldByName('ValidFactors').AsString) = 'Y');
          LDemandCentreGrowthFactors := AGrowthFactors.CreateDemandGrowthFactor;
          LDemandCentreGrowthFactors.Populate(LIdentifier,LChannelNumber,AGrowthFactors.NumberOfYears,LGrowthFactor,LValidFactors);
          //if (not LDemandCentreGrowthFactors.Populate(LIdentifier,LChannelNumber,LGrowthFactor)) then
          //  AGrowthFactors.DeleteDemandGrowthFactorByIndex(AGrowthFactors.DemandCentresGrowthFactorsCount-1);
          LDataset.DataSet.Next;
        end;
        LDataset.DataSet.Close;
      end;
      Result := True;
    finally
      LDataset.Free;
    end;
  except on E: Exception do HandleErrorFunction(E, OPNAME, Result) end;
end;

function TGrowthFactorDataLoadAgent.LoadHydrologyGrowthFactorData(AGrowthFactors:TGrowthFactors): boolean;
const OPNAME = 'TGrowthFactorDataLoadAgent.LoadHydrologyGrowthFactorData';
var
  LHydrologyGrowthFactors: THydrologyGrowthFactors;
  LDataSet     : TAbstractModelDataset;
  LSQL         : string;
  LIdentifier  : integer;
  LGaugeNumber : integer;
  LGrowthFactorsAFF: string;
  LGrowthFactorsIRR: string;
  LGrowthFactorsURB: string;
begin
  Result := False;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), lDataSet);
    try
      if Assigned(LDataSet) then
      begin
        LSQL := FSQLAgent.GetGrowthFactorHydrologySQL;
        LDataSet.SetSQL(lSQL);
        LDataset.DataSet.Open;
        while (not LDataset.DataSet.EOF) do
        begin
          LIdentifier   := LDataset.DataSet.FieldByName('Identifier').AsInteger;
          LGaugeNumber:= LDataset.DataSet.FieldByName('GaugeNumber').AsInteger;
          LGrowthFactorsAFF := Trim(LDataset.DataSet.FieldByName('AFFFactors').AsString);
          LGrowthFactorsIRR := Trim(LDataset.DataSet.FieldByName('IRRFactors').AsString);
          LGrowthFactorsURB := Trim(LDataset.DataSet.FieldByName('URBFactors').AsString);
          LHydrologyGrowthFactors := AGrowthFactors.CreateHydrologyGrowthFactor;
          LHydrologyGrowthFactors.Populate(LIdentifier,LGaugeNumber,AGrowthFactors.NumberOfYears,LGrowthFactorsAFF,LGrowthFactorsIRR,LGrowthFactorsURB);
          //if (not LHydrologyGrowthFactors.PopulateDemandCenterGrowthFactor(LIdentifier,LChannelNumber,LGrowthFactor)) then
          //  AGrowthFactors.DeleteDemandGrowthFactorByIndex(AGrowthFactors.DemandCentresGrowthFactorsCount-1);
          LDataset.DataSet.Next;
        end;
        LDataset.DataSet.Close;
      end;
      Result := True;
    finally
      LDataset.Free;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TGrowthFactorDataLoadAgent.LoadMinMaxChannelGrowthFactorData(AGrowthFactors:TGrowthFactors): boolean;
const OPNAME = 'TGrowthFactorDataLoadAgent.LoadMinMaxChannelGrowthFactorData';
var
  LMinMaxChannelGrowthFactors: TMinMaxChannelGrowthFactors;
  LDataSet     : TAbstractModelDataset;
  LSQL         : string;
  LIdentifier  : integer;
  LChannelNumber: integer;
  LArcNumber : integer;
  LGrowthFactor: string;
  LValidFactors: boolean;
begin
  Result := False;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), lDataSet);
    try
      if Assigned(LDataSet) then
      begin
        LSQL := FSQLAgent.GetGrowthFactorMinMaxSQL;
        LDataSet.SetSQL(lSQL);
        LDataset.DataSet.Open;
        while (not LDataset.DataSet.EOF) do
        begin
          LIdentifier   := LDataset.DataSet.FieldByName('Identifier').AsInteger;
          LChannelNumber:= LDataset.DataSet.FieldByName('ChannelNumber').AsInteger;
          LArcNumber       :=  LDataset.DataSet.FieldByName('ArcNumber').AsInteger;
          LGrowthFactor := Trim(LDataset.DataSet.FieldByName('Factors').AsString);
          LValidFactors := (Trim(LDataset.DataSet.FieldByName('ValidFactors').AsString) = 'Y');
          LMinMaxChannelGrowthFactors := AGrowthFactors.CreateMinMaxChannelGrowthFactor;
          LMinMaxChannelGrowthFactors.Populate(LIdentifier,LChannelNumber,LArcNumber,AGrowthFactors.NumberOfYears,LGrowthFactor,LValidFactors);
          //if (not LMinMaxChannelGrowthFactors.Populate(LIdentifier,LChannelNumber,LBoundChannel,LGrowthFactor)) then
          //  AGrowthFactors.DeleteMinMaxChannelGrowthFactorByIndex(AGrowthFactors.MinMaxChannelGrowthFactorsCount-1);
          LDataset.DataSet.Next;
        end;
        LDataset.DataSet.Close;
      end;
      Result := True;
    finally
      LDataset.Free;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TGrowthFactorDataLoadAgent.LoadGrowthFactorConfigData(AGrowthFactors: TGrowthFactors): boolean;
const OPNAME = 'TGrowthFactorDataLoadAgent.LoadGrowthFactorConfigData';
var
  LDataSet     : TAbstractModelDataset;
  LSQL         : string;
  LNumberOfYears  : integer;
begin
  Result := False;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), lDataSet);
    try
      if Assigned(LDataSet) then
      begin
        LSQL := FSQLAgent.GetGrowthFactorConfigSQL;
        LDataSet.SetSQL(lSQL);
        LDataset.DataSet.Open;
        if (not LDataset.DataSet.EOF) then
        begin
          LNumberOfYears := LDataset.DataSet.FieldByName('YearsCount').AsInteger;
          AGrowthFactors.Populate(LNumberOfYears);
        end;
        LDataset.DataSet.Close;
      end;
      Result := True;
    finally
      LDataset.Free;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

end.
