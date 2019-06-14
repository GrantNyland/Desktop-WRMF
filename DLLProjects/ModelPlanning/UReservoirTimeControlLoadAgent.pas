//
//
//  UNIT      : Contains TReservoirDataLoadAgent Class
//  AUTHOR    : Titi Ngubane (PDNA)
//  DATE      : 2003/02/25
//  COPYRIGHT : Copyright © 2003 DWAF
//
//
unit UReservoirTimeControlLoadAgent;

interface

uses
  Classes,
  UChannelData,
  UReservoirData,
  UAbstractObject,
  UReservoirTimeControlSQLAgent,
  UReservoirZoneElevationData;

type
  TReservoirTimeControlLoadAgent = class(TAbstractAppObject)
  protected
    FSQLAgent: TReservoirTimeControlSQLAgent;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    procedure PopulateReplacements (AReservoirList : TReservoirDataList);
  public
    function ConstructData(AReservoirDataList: TReservoirDataList): boolean;
    function LoadReservoirTimeControl(AReservoirDataList: TReservoirDataList): boolean;
  end;

implementation

uses
  SysUtils,
  VoaimsCom_TLB,
  UDataSetType,
  UErrorHandlingOperations;

procedure TReservoirTimeControlLoadAgent.CreateMemberObjects;
const OPNAME = 'TReservoirTimeControlLoadAgent.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
    FSQLAgent := TReservoirTimeControlSQLAgent.Create(FAppModules);
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TReservoirTimeControlLoadAgent.DestroyMemberObjects;
const OPNAME = 'TReservoirTimeControlLoadAgent.DestroyMemberObjects';
begin
  try
    FreeAndNil(FSQLAgent);
    inherited DestroyMemberObjects;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TReservoirTimeControlLoadAgent.ConstructData(AReservoirDataList: TReservoirDataList): boolean;
const OPNAME = 'TReservoirTimeControlLoadAgent.ConstructData';
begin
  Result := True;
  try
    AReservoirDataList.Initialise;
    LoadReservoirTimeControl(AReservoirDataList);
  except on E : Exception do HandleErrorFunction(E, OPNAME, Result); end;
end;

function TReservoirTimeControlLoadAgent.LoadReservoirTimeControl(AReservoirDataList: TReservoirDataList): boolean;
const OPNAME = 'TReservoirTimeControlLoadAgent.LoadReservoirTimeControl';
var
  LDataSet                   : TAbstractModelDataset;
  LReservoirNr               : integer;
  LReservoirData             : TReservoirData;
  LResTimeControl            : TReservoirTimeControl;
  LBaseNodeNumber            : integer;
  LReservoirStartYear        : integer;
  LReservoirStartMonth       : integer;
  LReservoirEndYear          : integer;
  LReservoirEndMonth         : integer;
  LReservoirEconomicLife     : integer;
  LReservoirCapitalCost      : double;
  LReservoirOMCost           : double;
  LReservoirCostSchedule     : string;
begin
  Result := False;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      if Assigned(LDataSet) then
      begin
        LDataSet.SetSQL(FSQLAgent.GetReservoirTimeControlSQL);
        LDataSet.DataSet.Open;
        Result := TRUE;
        while not LDataSet.DataSet.Eof do
        begin
          LReservoirNr := LDataSet.DataSet.FieldbyName('ReservoirNumber').AsInteger;
          LReservoirStartYear        := LDataset.DataSet.FieldByName('ReservoirStartYear').AsInteger;
          LReservoirStartMonth       := LDataset.DataSet.FieldByName('ReservoirStartMonth').AsInteger;
          LReservoirEndYear          := LDataset.DataSet.FieldByName('ReservoirEndYear').AsInteger;
          LReservoirEndMonth         := LDataset.DataSet.FieldByName('ReservoirEndMonth').AsInteger;
          LReservoirEconomicLife     := LDataset.DataSet.FieldByName('ReservoirEconomicLife').AsInteger;
          LReservoirCapitalCost      := LDataset.DataSet.FieldByName('ReservoirCapitalCost').AsFloat;
          LReservoirOMCost           := LDataset.DataSet.FieldByName('ReservoirOMCost').AsFloat;
          LReservoirCostSchedule     := Trim(LDataSet.DataSet.FieldByName('ReservoirCostSchedule').AsString);
          LBaseNodeNumber            := LDataset.DataSet.FieldByName('BaseNodeNumber').AsInteger;
          LReservoirData := AReservoirDataList.CastReservoirByIdentifier[LReservoirNr];
          if Assigned(LReservoirData) then
          begin
            LResTimeControl := LReservoirData.CreateTimeControl;
            if Assigned(LResTimeControl) then
            begin
              LResTimeControl.Populate(LReservoirNr,LBaseNodeNumber,LReservoirStartYear,LReservoirStartMonth,LReservoirEndYear,
                                       LReservoirEndMonth,LReservoirEconomicLife,LReservoirCapitalCost,LReservoirOMCost,
                                       LReservoirCostSchedule);
            end;
          end;
          LDataSet.DataSet.Next;
        end;
        Result := TRUE;
        LDataSet.DataSet.Close;
        PopulateReplacements(AReservoirDataList);
      end;
    finally
      LDataSet.Free;
    end;
  except on E: Exception do HandleErrorFunction(E, OPNAME,Result) end;
end;

procedure TReservoirTimeControlLoadAgent.PopulateReplacements (AReservoirList : TReservoirDataList);
const OPNAME = 'TReservoirTimeControlLoadAgent.PopulateReplacements';
var
  LDataSet : TAbstractModelDataset;
  LReservoir : TReservoirData;
  LIndex : integer;
  LReservoirNr : integer;
begin
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      for LIndex := 0 to AReservoirList.ReservoirCount - 1 do
      begin
        LReservoirNr := 0;
        LReservoir   := AReservoirList.CastReservoirByIndex[LIndex];
        if LReservoir <> nil then
          LReservoirNr := LReservoir.ReservoirConfigurationData.ReservoirIdentifier;
        if LReservoir.TimeControl <> nil then
        begin
          if Assigned(LDataSet) then
          begin
            LDataSet.SetSQL(FSQLAgent.GetReservoirTimeControlByBaseResevoirNrSQL(IntToStr(LReservoirNr)));
            LDataSet.DataSet.Open;
            while not LDataSet.DataSet.eof do
            begin
              LReservoir.TimeControl.AddReplacement(LDataSet.DataSet.FieldByName('ReservoirNumber').AsInteger);
              LDataSet.DataSet.Next;
            end;
          end;
        end;
       end;
    finally
      LDataSet.Free;
    end;
  except on E : Exception do HandleError(E,OPNAME); end;
end;


{
procedure TReservoirTimeControlLoadAgent.PopulateReplacements (AReservoirList : TReservoirDataList);
const OPNAME = 'TReservoirTimeControlLoadAgent.PopulateReplacements';
var
  lIndex       : integer;
  lCount       : integer;
  lReservoir   : TReservoirData;
  lOther       : TReservoirData;
  lReservoirNr : integer;
  lOtherNr     : integer;
begin
  try
    for lIndex := 0 to AReservoirList.ReservoirCount - 1 do
    begin
      lReservoir   := AReservoirList.CastReservoirByIndex[lIndex];
      lReservoirNr := lReservoir.ReservoirConfigurationData.ReservoirIdentifier;
      if (lReservoir.TimeControl <> nil) then
      begin
        if (lReservoir.TimeControl.BaseNodeNumber = lReservoirNr) then
        begin
          lCount := 0;
          while (lCount < AReservoirList.ReservoirCount) do
          begin
            if (lCount <> lIndex) then
            begin
              lOther := AReservoirList.CastReservoirByIndex[lCount];
              lOtherNr := lOther.ReservoirConfigurationData.ReservoirIdentifier;
              if (lOther.TimeControl <> nil) AND
                 (lOther.TimeControl.BaseNodeNumber = lReservoirNr) then
                lReservoir.TimeControl.AddReplacement(lOtherNr);
            end;
            lCount := lCount + 1;
          end;
        end;
      end;
    end;
  except on E : Exception do HandleError(E,OPNAME); end;
end;
}

end.

