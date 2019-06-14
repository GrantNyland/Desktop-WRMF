//
//
//  UNIT      : Contains TReservoirPenaltyStructureDataLoadAgent Class
//  AUTHOR    : Titi Ngubane (PDNA)
//  DATE      : 2003/03/07
//  COPYRIGHT : Copyright © 2003 DWAF
//
//
unit UReservoirPenaltyStructureDataLoadAgent;

interface

uses
  Classes,
  UReservoirData,
  UAbstractObject,
  UReservoirPenaltyStructureData,
  UReservoirPenaltyStructureDataSQLAgent;

type
  TReservoirPenaltyStructureDataLoadAgent = class(TAbstractAppObject)
  protected
    FSQLAgent: TReservoirPenaltyStructureDataSQLAgent;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    function LoadReservoirPenaltyStructures(
      AReservoirPenaltyStructureList: TReservoirPenaltyStructureList): boolean;
    function LoadReservoirPenaltyCounts(
      AReservoirPenaltyCounts: TReservoirPenaltyCounts): boolean;
  public
    function ConstructData(
      AReservoirPenaltyStructureList: TReservoirPenaltyStructureList): boolean;
    procedure LoadStorageZonesContextData(AContextData: TStringList);
    procedure LoadStorageZoneDetailsContextData(AContextData: TStringList; ARecordIdentifier: string);
    procedure LoadStorageZoneNamesContextData(AContextData: TStringList; ARecordIdentifier,APenaltyStruct: string);
  end;

implementation

uses
  SysUtils,
  UDataSetType,
  UConstants,
  UErrorHandlingOperations;

procedure TReservoirPenaltyStructureDataLoadAgent.CreateMemberObjects;
const OPNAME = 'TReservoirPenaltyStructureDataLoadAgent.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
    FSQLAgent := TReservoirPenaltyStructureDataSQLAgent.Create(FAppModules);
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TReservoirPenaltyStructureDataLoadAgent.DestroyMemberObjects;
const OPNAME = 'TReservoirPenaltyStructureDataLoadAgent.DestroyMemberObjects';
begin
  try
    FreeAndNil(FSQLAgent);
    inherited DestroyMemberObjects;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TReservoirPenaltyStructureDataLoadAgent.ConstructData(
  AReservoirPenaltyStructureList: TReservoirPenaltyStructureList): boolean;
const OPNAME = 'TReservoirPenaltyStructureDataLoadAgent.ConstructData';
var
  LIndex: integer;
begin
  Result := False;
  try
    AReservoirPenaltyStructureList.Initialise;
    Result :=  LoadReservoirPenaltyStructures(AReservoirPenaltyStructureList);
    Result := Result and LoadReservoirPenaltyCounts(AReservoirPenaltyStructureList.CastReservoirPenaltyCounts);
    if Result then
    begin

    // Remove penalty structures which do not have any penalty values.
    for LIndex := 19 downto AReservoirPenaltyStructureList.CastReservoirPenaltyCounts.PenaltyStructureCount do
        AReservoirPenaltyStructureList.DeleteReservoirPenaltyStructure(LIndex);

    end;
  except on E : Exception do HandleErrorFunction(E, OPNAME, Result); end;
end;

function TReservoirPenaltyStructureDataLoadAgent.LoadReservoirPenaltyStructures(
  AReservoirPenaltyStructureList: TReservoirPenaltyStructureList): boolean;
const OPNAME = 'TReservoirPenaltyStructureDataLoadAgent.LoadReservoirPenaltyStructures';
var
  LIndex: integer;
  LDataSet: TAbstractModelDataset;
  LReservoirPenaltyStructureData: TReservoirPenalty;
  LReservoirPenaltyZoneData: TReservoirPenaltyZoneData;
  LPenaltyValueArray: array[0..19] of double;
begin
  Result := False;
  try
    // Always create five penalty structures at start.
    for LIndex := 0 to 19 do
    begin
      LReservoirPenaltyStructureData := TReservoirPenalty.Create(FAppModules);
      LReservoirPenaltyStructureData.Initialise;
      LReservoirPenaltyStructureData.PopulatePenaltyStructureIdentifier(LIndex+1);
      AReservoirPenaltyStructureList.AddReservoirPenaltyStructure(LReservoirPenaltyStructureData);
    end;

    // Create the storage zone levels dataset.
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      if Assigned(LDataSet) then
      begin
        LDataSet.SetSQL(FSQLAgent.GetPenaltyStructureSQL);
        LDataset.DataSet.Open;
        while (not LDataset.DataSet.Eof) do
        begin
          for LIndex := 0 to 19 do
            LPenaltyValueArray[LIndex] := NullFloat;
          // Attempt to populate the data object and add it to the list.
          LReservoirPenaltyZoneData := TReservoirPenaltyZoneData.Create(FAppModules);
          if LReservoirPenaltyZoneData.PopulatePenaltyZone(LDataset, LPenaltyValueArray) then
          begin
            AReservoirPenaltyStructureList.AddReservoirPenaltyZone(LReservoirPenaltyZoneData);
          end else begin
            FreeAndNil(LReservoirPenaltyZoneData);
          end;
          // Add the zone penalty values.
          for LIndex := 0 to 19 do
          begin
            AReservoirPenaltyStructureList.CastReservoirPenaltyStructureByIndex[LIndex].
              AddPenaltyValue(LReservoirPenaltyZoneData.RecordIdentifier, LPenaltyValueArray[LIndex]);
          end;
          LDataset.DataSet.Next;
        end;
        LDataset.DataSet.Close;
        Result := True;
      end;
    finally
      LDataset.Free;
    end;
  except on E: Exception do HandleErrorFunction(E, OPNAME, Result) end;
end;

function TReservoirPenaltyStructureDataLoadAgent.LoadReservoirPenaltyCounts(
  AReservoirPenaltyCounts: TReservoirPenaltyCounts): boolean;
const OPNAME = 'TReservoirPenaltyStructureDataLoadAgent.LoadReservoirPenaltyCounts';
var
  LDataSet: TAbstractModelDataset;
begin
  Result := False;
  try
    // Create the storage zones dataset.
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      if Assigned(LDataSet) then
      begin
        LDataSet.SetSQL(FSQLAgent.GetPenaltyCountsSQL);
        LDataset.DataSet.Open;
        if (not LDataset.DataSet.Eof) then
          AReservoirPenaltyCounts.PopulatePenaltyCounts(LDataset);
        LDataset.DataSet.Close;
        Result := True;
      end;
    finally
      LDataset.Free;
    end;
  except on E: Exception do HandleErrorFunction(E, OPNAME, Result) end;
end;

procedure TReservoirPenaltyStructureDataLoadAgent.LoadStorageZonesContextData(AContextData: TStringList);
const OPNAME = 'TReservoirPenaltyStructureDataLoadAgent.LoadStorageZonesContextData';
begin
  try
    FSQLAgent.LoadStorageZonesContextData(AContextData);
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TReservoirPenaltyStructureDataLoadAgent.LoadStorageZoneNamesContextData(AContextData: TStringList; ARecordIdentifier,APenaltyStruct: string);
const OPNAME = 'TReservoirPenaltyStructureDataLoadAgent.LoadStorageZoneNamesContextData';
begin
  try
    FSQLAgent.LoadStorageZoneNamesContextData(AContextData,ARecordIdentifier,APenaltyStruct);
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TReservoirPenaltyStructureDataLoadAgent.LoadStorageZoneDetailsContextData(AContextData: TStringList; ARecordIdentifier: string);
const OPNAME = 'TReservoirPenaltyStructureDataLoadAgent.LoadStorageZoneDetailsContextData';
begin
  try
    FSQLAgent.LoadStorageZoneDetailsContextData(AContextData,ARecordIdentifier);
  except on E : Exception do HandleError(E,OPNAME); end;
end;

end.
