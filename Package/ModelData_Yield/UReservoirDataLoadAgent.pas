//
//
//  UNIT      : Contains TReservoirDataLoadAgent Class
//  AUTHOR    : Titi Ngubane (PDNA)
//  DATE      : 2003/02/25
//  COPYRIGHT : Copyright © 2003 DWAF
//
//
unit UReservoirDataLoadAgent;

interface

uses
  Classes,
  UChannelData,
  UReservoirData,
  UReservoirAreaGroup,
  UAbstractObject,
  UReservoirDataSQLAgent,
  UReservoirZoneElevationData;

type
  TReservoirDataLoadAgent = class(TAbstractAppObject)
  protected
    FSQLAgent: TReservoirDataSQLAgent;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    function LoadReservoirNames(AReservoirDataList: TReservoirDataList): boolean;
    function LoadReservoirAreas(AReservoirDataList: TReservoirDataList): boolean;
    function LoadReservoirVolumes(AReservoirDataList: TReservoirDataList): boolean;
    function LoadReservoirEvaporations(AReservoirDataList: TReservoirDataList): boolean;
    function LoadReservoirInitialLevels(AReservoirDataList: TReservoirDataList): boolean;
    function LoadReservoirElevations(AReservoirDataList: TReservoirDataList): boolean;
    function LoadDrawDownLevels(AReservoirData: TReservoirData;ADrawDownLevelsDataSet: TAbstractModelDataset): boolean;
    function AddReservoirToList(AReservoirDataList: TReservoirDataList;LDataset: TAbstractModelDataset;var AReservoirData: TReservoirData): boolean;
    function LoadReservoirDownStreamChannels(AReservoirDataList: TReservoirDataList): boolean;
    function LoadReservoirAreaGroup(AReservoirAreaGroupList: TReservoirAreaGroupList): boolean;

  public
    function ConstructData(AReservoirDataList: TReservoirDataList;
                           AReservoirAreaGroupList: TReservoirAreaGroupList): boolean;
    procedure LoadReservoirNamesContextData(AContextData: TStringList;
      ARecordIdentifier, ANodeCount, APenaltyStructure: string);
    procedure LoadFixedElevationsContextData(AContextData: TStringList;
      AReservoirIdentifier, AFieldNameIdentifier: string);
    procedure LoadDrawDownElevationsContextData(AContextData: TStringList;
      AReservoirIdentifier, ALevelIdentifier, AFieldNameIdentifier: string);
    procedure LoadNodesDetailsContextData(AContextData: TStringList;ARecordIdentifier: string);
    procedure LoadInitialLevelsContextData(AContextData: TStringList; AReservoirIdentifier, AFieldNameIdentifier: string);
    procedure LoadEvaporationsContextData(AContextData: TStringList; ARecordIdentifier, AFieldNameIdentifier: string);
    procedure LoadElevationsContextData(AContextData: TStringList; ARecordIdentifier, AFieldNameIdentifier: string);
    procedure LoadVolumesContextData(AContextData: TStringList; ARecordIdentifier, AFieldNameIdentifier: string);
    procedure LoadAreasContextData(AContextData: TStringList; ARecordIdentifier,AFieldNameIdentifier: string);
    //function LoadInferedNodesData(AReservoirDataList: TReservoirDataList;AChannelDataList: TChannelList): boolean;
    procedure LoadHistoricWaterLevelContextData(AContextData: TStringList; ARecordIdentifier,AFieldNameIdentifier: string);

  end;

implementation

uses
  SysUtils,
  VoaimsCom_TLB,
  UDataSetType,
  UErrorHandlingOperations;

procedure TReservoirDataLoadAgent.CreateMemberObjects;
const OPNAME = 'TReservoirDataLoadAgent.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
    FSQLAgent := TReservoirDataSQLAgent.Create(FAppModules);
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TReservoirDataLoadAgent.DestroyMemberObjects;
const OPNAME = 'TReservoirDataLoadAgent.DestroyMemberObjects';
begin
  try
    FreeAndNil(FSQLAgent);
    inherited DestroyMemberObjects;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TReservoirDataLoadAgent.ConstructData(AReservoirDataList: TReservoirDataList;
                                               AReservoirAreaGroupList: TReservoirAreaGroupList): boolean;
const OPNAME = 'TReservoirDataLoadAgent.ConstructData';
begin
  Result := True;
  try
    AReservoirDataList.Initialise;
    LoadReservoirNames(AReservoirDataList);
    LoadReservoirAreas(AReservoirDataList);
    LoadReservoirVolumes(AReservoirDataList);
    LoadReservoirEvaporations(AReservoirDataList);
    LoadReservoirElevations(AReservoirDataList);
    LoadReservoirInitialLevels(AReservoirDataList);
    LoadReservoirDownStreamChannels(AReservoirDataList);
    LoadReservoirAreaGroup(AReservoirAreaGroupList);
  except on E : Exception do HandleErrorFunction(E, OPNAME, Result); end;
end;

function TReservoirDataLoadAgent.LoadReservoirNames(AReservoirDataList: TReservoirDataList): boolean;
const OPNAME = 'TReservoirDataLoadAgent.LoadReservoirNames';
var
  LDrawDownLevelsDataSet,
  LDataset: TAbstractModelDataset;
  LReservoirData: TReservoirData;
begin
  Result := False;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      if Assigned(LDataSet) then
      begin
        // Load reservoirs.
        LDataSet.SetSQL(FSQLAgent.GetReservoirNamesDataSQL);
        LDataset.DataSet.Open;
        FAppModules.Database.CreateDataset(integer(dtExecSQL), LDrawDownLevelsDataSet);
        try
          LDrawDownLevelsDataSet.SetSQL(FSQLAgent.GetDrawDownElevationsSQL);
          LDrawDownLevelsDataSet.DataSet.Open;
          while (not LDataset.DataSet.Eof) do
          begin
            if Assigned(LDrawDownLevelsDataSet) then
              if AddReservoirToList(AReservoirDataList, LDataset, LReservoirData) then
              begin
                if (LReservoirData.ReservoirConfigurationData.NodeType in ReservoirsSet)then
                  // Load the draw down levels for this reservoir.
                  while LoadDrawDownLevels(LReservoirData, LDrawDownLevelsDataSet) do ;
              end;
            // Goto the next reservoir record.
            LDataset.DataSet.Next;
          end;
          LDrawDownLevelsDataSet.Dataset.Close;

          // Load nodes with inflow.
          LDataSet.Dataset.Close;
          LDataSet.SetSQL(FSQLAgent.GetNodesWithInflowNamesDataSQL);
          LDataset.DataSet.Open;
          while (not LDataset.DataSet.Eof) do
          begin
              AddReservoirToList(AReservoirDataList, LDataset, LReservoirData);
            // Goto the next node record.
            LDataset.DataSet.Next;
          end;

          // Add the zero node.
          AReservoirDataList.AddZeroNode;

          // Load  nodes without  inflow nodes.
          LDataSet.Dataset.Close;
          LDataSet.SetSQL(FSQLAgent.GetNodesWithoutInflowNamesDataSQL);
          LDataset.DataSet.Open;
          while (not LDataset.DataSet.Eof) do
          begin
              AddReservoirToList(AReservoirDataList, LDataset, LReservoirData);
            // Goto the next node record.
            LDataset.DataSet.Next;
          end;

          // Load  irrigation nodes.
          {LDataSet.Dataset.Close;
          LDataSet.SetSQL(FSQLAgent.GetIrrigationNodesNamesDataSQL);
          LDataset.DataSet.Open;
          while (not LDataset.DataSet.Eof) do
          begin
              AddReservoirToList(AReservoirDataList, LDataset, LReservoirData);
            // Goto the next node record.
            LDataset.DataSet.Next;
          end;

          // Load wetland reservoir nodes
          LDataSet.Dataset.Close;
          LDataSet.SetSQL(FSQLAgent.GetWetlandsNodesNamesDataSQL);
          LDataset.DataSet.Open;
          while (not LDataset.DataSet.Eof) do
          begin
              AddReservoirToList(AReservoirDataList, LDataset, LReservoirData);
            // Goto the next node record.
            LDataset.DataSet.Next;
          end;}

          LDataset.DataSet.Close;
        finally
          LDrawDownLevelsDataSet.Dataset.Close;
          LDrawDownLevelsDataSet.Free;
        end;
      end;
    finally
      LDataset.DataSet.Close;
      LDataset.Free;
    end;

  // Done.
  Result := True;

  // Handle exceptions.
  except on E: Exception do HandleErrorFunction(E, OPNAME, Result) end;
end;

function TReservoirDataLoadAgent.AddReservoirToList(AReservoirDataList: TReservoirDataList;LDataset: TAbstractModelDataset;var AReservoirData: TReservoirData): boolean;
const OPNAME = 'TReservoirDataLoadAgent.AddReservoirToList';
begin
  Result := False;
  try
    AReservoirData := TReservoirData.Create(FAppModules);
    if (not AReservoirData.PopulateReservoirConfigurationData(LDataset)) then
    begin
      FreeAndNil(AReservoirData);
    end else begin
      AReservoirDataList.AddReservoirOrNodeToList(AReservoirData);
      Result := True;
    end;
  except on E: Exception do HandleErrorFunction(E, OPNAME,Result) end;
end;

function TReservoirDataLoadAgent.LoadDrawDownLevels(AReservoirData: TReservoirData; ADrawDownLevelsDataSet: TAbstractModelDataset): boolean;
const OPNAME = 'TReservoirDataLoadAgent.LoadDrawDownLevels';
var LDrawDownElevationData: TDrawDownElevation;
begin
  Result := False;
  try
    if (AReservoirData.ReservoirConfigurationData.ReservoirIdentifier =
        ADrawDownLevelsDataSet.DataSet.FieldByName('ReservoirIdentifier').AsInteger) and
       (not ADrawDownLevelsDataSet.DataSet.Eof) then
    begin
      LDrawDownElevationData := TDrawDownElevation.Create(FAppModules);
      if LDrawDownElevationData.PopulateDrawDownElevations(ADrawDownLevelsDataSet) then
      begin
        AReservoirData.CastReservoirZoneElevationsData.AddDrawDownElevation(LDrawDownElevationData);
      end else begin
        FreeAndNil(LDrawDownElevationData);
      end;
    end;
    ADrawDownLevelsDataSet.DataSet.Next;
    if (AReservoirData.ReservoirConfigurationData.ReservoirIdentifier =
          ADrawDownLevelsDataSet.DataSet.FieldByName('ReservoirIdentifier').AsInteger) and
       (not ADrawDownLevelsDataSet.DataSet.Eof) then
    begin
      Result := True;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirDataLoadAgent.LoadReservoirAreas(AReservoirDataList: TReservoirDataList): boolean;
const OPNAME = 'TReservoirDataLoadAgent.LoadReservoirAreas';
var
  LDataSet: TAbstractModelDataset;
  LRecordIdentifier, LAreaIndex: integer;
  LReservoirData: TReservoirData;
  LMaximumArea: double;
begin
  Result := False;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      if Assigned(LDataSet) then
      begin
        LDataSet.SetSQL(FSQLAgent.GetReservoirAreaDataSQL);
        LDataset.DataSet.Open;
        Result := True;
        while (not LDataset.DataSet.Eof) do
        begin

          // Calculate the maximum area.
          LMaximumArea := -1;
          for LAreaIndex := 1 to 15 do
            if (LDataset.DataSet.FieldByName(Format('Area%2.2d', [LAreaIndex])).AsFloat > LMaximumArea) then
              LMaximumArea := LDataset.DataSet.FieldByName(Format('Area%2.2d', [LAreaIndex])).AsFloat;

          // Add the data value.
          LRecordIdentifier := LDataset.DataSet.FieldByName('RecordIdentifier').AsInteger;
          LReservoirData := AReservoirDataList.CastReservoirByRecordIdentifier[LRecordIdentifier];
          if Assigned(LReservoirData) then
          begin
            LReservoirData.CastReservoirConfigurationData.PopulateMaxArea(LMaximumArea);
            Result := Result and LReservoirData.CastReservoirAreasData.LoadAreasFromDataset(LDataset);
          end;
          LDataset.DataSet.Next;
        end;
        LDataset.DataSet.Close;

        // Done.
        Result := True;
      end;
    finally
      LDataset.Free;
    end;

  // Handle exceptions.
  except on E: Exception do HandleErrorFunction(E, OPNAME, Result) end;
end;

function TReservoirDataLoadAgent.LoadReservoirVolumes(AReservoirDataList: TReservoirDataList): boolean;
const OPNAME = 'TReservoirDataLoadAgent.LoadReservoirVolumes';
var
  LDataSet: TAbstractModelDataset;
  LRecordIdentifier, LVolumeIndex: integer;
  LMaximumVolume: double;
  LReservoirData: TReservoirData;
begin
  Result := False;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      if Assigned(LDataSet) then
      begin
        LDataSet.SetSQL(FSQLAgent.GetReservoirVolumeDataSQL);
        LDataset.DataSet.Open;
        Result := True;
        while (not LDataset.DataSet.Eof) do
        begin

          // Calculate the maximum volume.
          LMaximumVolume := -1;
          for LVolumeIndex := 1 to 15 do
            if (LDataset.DataSet.FieldByName(Format('Volume%2.2d', [LVolumeIndex])).AsFloat > LMaximumVolume) then
              LMaximumVolume := LDataset.DataSet.FieldByName(Format('Volume%2.2d', [LVolumeIndex])).AsFloat;

          // Add the data value.
          LRecordIdentifier := LDataset.DataSet.FieldByName('RecordIdentifier').AsInteger;
          LReservoirData := AReservoirDataList.CastReservoirByRecordIdentifier[LRecordIdentifier];
          if Assigned(LReservoirData) then
          begin
            LReservoirData.CastReservoirConfigurationData.PopulateMaxVolume(LMaximumVolume);
            Result := Result and LReservoirData.CastReservoirVolumesData.LoadVolumesFromDataset(LDataset);
          end;
          LDataset.DataSet.Next;
        end;
        LDataset.DataSet.Close;

        // Done.
        Result := True;
      end;
    finally
      LDataset.Free;
    end;

  // Handle exceptions.
  except on E: Exception do HandleErrorFunction(E, OPNAME, Result) end;
end;

function TReservoirDataLoadAgent.LoadReservoirEvaporations(AReservoirDataList: TReservoirDataList): boolean;
const OPNAME = 'TReservoirDataLoadAgent.LoadReservoirEvaporations';
var
  LReservoirID : integer;
  LReservoirData: TReservoirData;
  LDataSet: TAbstractModelDataset;
begin
  Result := False;
  try

    // Loop for all the records in the table.
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      if Assigned(LDataSet) then
      begin
        LDataSet.SetSQL(FSQLAgent.GetReservoirEvaporationDataSQL);
        LDataset.DataSet.Open;
        Result := True;
        while not LDataset.DataSet.EOF do
        begin
          LReservoirID := LDataset.DataSet.FieldByName('RecordIdentifier').AsInteger;
          LReservoirData := AReservoirDataList.CastReservoirByRecordIdentifier[LReservoirID];
          if Assigned(LReservoirData) then
            Result := Result and LReservoirData.CastReservoirEvaporationsData.LoadMonthlyEvaporationsFromDataset(LDataset);
          LDataset.DataSet.Next;
        end;
        LDataset.DataSet.Close;
        // Done.
      end;
    finally
      LDataset.Free;
    end;

  // Handle exceptions.
  except on E: Exception do HandleErrorFunction(E, OPNAME, Result) end;
end;

function TReservoirDataLoadAgent.LoadReservoirInitialLevels(AReservoirDataList: TReservoirDataList): boolean;
const OPNAME = 'TReservoirDataLoadAgent.LoadReservoirInitialLevels';
var
  LReservoirID : integer;
  LReservoirData: TReservoirData;
  LDataSet: TAbstractModelDataset;
begin
  Result := False;
  try

    // Loop for all the records in the table.
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      if Assigned(LDataSet) then
      begin
        LDataSet.SetSQL(FSQLAgent.GetReservoirInitialLevelsDataSQL);
        LDataset.DataSet.Open;
        Result := True;
        while not LDataset.DataSet.EOF do
        begin
          LReservoirID := LDataset.DataSet.FieldByName('ReservoirIdentifier').AsInteger;
          LReservoirData := AReservoirDataList.CastReservoirByIdentifier[LReservoirID];
          if Assigned(LReservoirData) then
            Result := Result and LReservoirData.CastReservoirZoneElevationsData.CastInitialLevelsData.LoadInitialLevelsFromDataset(LDataset);
          LDataset.DataSet.Next;
        end;
        LDataset.DataSet.Close;
        // Done.
      end;
    finally
      LDataset.Free;
    end;

  // Handle exceptions.
  except on E: Exception do HandleErrorFunction(E, OPNAME, Result) end;
end;

function TReservoirDataLoadAgent.LoadReservoirElevations(AReservoirDataList: TReservoirDataList): boolean;
const OPNAME = 'TReservoirDataLoadAgent.LoadReservoirElevations';
var
  LReservoirID   : integer;
  LReservoirData : TReservoirData;
  LDataSet       : TAbstractModelDataset;
begin
  Result := False;
  try

    // Loop for all the records in the table.
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      if Assigned(LDataSet) then
      begin
        LDataSet.SetSQL(FSQLAgent.GetReservoirElevationsDataSQL);
        LDataset.DataSet.Open;
        Result := True;
        while not LDataset.DataSet.EOF do
        begin
          LReservoirID := LDataset.DataSet.FieldByName('RecordIdentifier').AsInteger;
          LReservoirData := AReservoirDataList.CastReservoirByRecordIdentifier[LReservoirID];
          if Assigned(LReservoirData) then
          begin
            Result := Result and LReservoirData.CastReservoirElevationsData.LoadMonthlyElevationsFromDataset(LDataset);
            LReservoirData.RecalculateVolumeWhenFull;
          end;
          LDataset.DataSet.Next;
        end;
        Result := True;
        LDataset.DataSet.Close;
        // Done.
      end;
    finally
      LDataset.Free;
    end;

  // Handle exceptions.
  except on E: Exception do HandleErrorFunction(E, OPNAME, Result) end;
end;

procedure TReservoirDataLoadAgent.LoadReservoirNamesContextData(AContextData: TStringList;
  ARecordIdentifier, ANodeCount, APenaltyStructure: string);
const OPNAME = 'TReservoirDataLoadAgent.LoadReservoirNamesContextData';
begin
  try
    FSQLAgent.LoadReservoirNamesContextData(AContextData, ARecordIdentifier, ANodeCount, APenaltyStructure);
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TReservoirDataLoadAgent.LoadFixedElevationsContextData(
  AContextData: TStringList; AReservoirIdentifier, AFieldNameIdentifier: string);
const OPNAME = 'TReservoirDataLoadAgent.LoadFixedElevationsContextData';
begin
  try
    FSQLAgent.LoadFixedElevationsContextData(AContextData, AReservoirIdentifier, AFieldNameIdentifier);
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TReservoirDataLoadAgent.LoadDrawDownElevationsContextData(AContextData: TStringList;
      AReservoirIdentifier, ALevelIdentifier, AFieldNameIdentifier: string);
const OPNAME = 'TReservoirDataLoadAgent.LoadDrawDownElevationsContextData';
begin
  try
    FSQLAgent.LoadDrawDownElevationsContextData(AContextData, AReservoirIdentifier, ALevelIdentifier, AFieldNameIdentifier);
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TReservoirDataLoadAgent.LoadNodesDetailsContextData(AContextData: TStringList; ARecordIdentifier: string);
const OPNAME = 'TReservoirDataLoadAgent.LoadNodesDetailsContextData';
begin
  try
    FSQLAgent.LoadNodesDetailsContextData(AContextData, ARecordIdentifier);
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TReservoirDataLoadAgent.LoadInitialLevelsContextData(AContextData: TStringList; AReservoirIdentifier, AFieldNameIdentifier: string);
const OPNAME = 'TReservoirDataLoadAgent.LoadInitialLevelsContextData';
begin
  try
    FSQLAgent.LoadInitialLevelsContextData(AContextData, AReservoirIdentifier, AFieldNameIdentifier);
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TReservoirDataLoadAgent.LoadEvaporationsContextData(AContextData: TStringList; ARecordIdentifier, AFieldNameIdentifier: string);
const OPNAME = 'TReservoirDataLoadAgent.LoadEvaporationsContextData';
begin
  try
    FSQLAgent.LoadEvaporationsContextData(AContextData, ARecordIdentifier,AFieldNameIdentifier);
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TReservoirDataLoadAgent.LoadAreasContextData(AContextData: TStringList; ARecordIdentifier,AFieldNameIdentifier: string);
const OPNAME = 'TReservoirDataLoadAgent.LoadAreasContextData';
begin
  try
    FSQLAgent.LoadAreasContextData(AContextData, ARecordIdentifier,AFieldNameIdentifier);
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TReservoirDataLoadAgent.LoadElevationsContextData(AContextData: TStringList; ARecordIdentifier, AFieldNameIdentifier: string);
const OPNAME = 'TReservoirDataLoadAgent.LoadElevationsContextData';
begin
  try
    FSQLAgent.LoadElevationsContextData(AContextData, ARecordIdentifier,AFieldNameIdentifier);
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TReservoirDataLoadAgent.LoadVolumesContextData(AContextData: TStringList; ARecordIdentifier, AFieldNameIdentifier: string);
const OPNAME = 'TReservoirDataLoadAgent.LoadVolumesContextData';
begin
  try
    FSQLAgent.LoadVolumesContextData(AContextData, ARecordIdentifier,AFieldNameIdentifier);
  except on E : Exception do HandleError(E,OPNAME); end;
end;

{function TReservoirDataLoadAgent.LoadInferedNodesData(AReservoirDataList: TReservoirDataList;
         AChannelDataList: TChannelList): boolean;
const OPNAME = 'TReservoirDataLoadAgent.LoadInferedNodesData';
var
  LCount: integer;
  LNodeNumber: integer;
begin
  Result := False;
  try
    if(Assigned(AReservoirDataList) and Assigned(AChannelDataList)) then
    begin
      for LCount := 0 to AChannelDataList.ChannelCount -1 do
      begin
        LNodeNumber := AChannelDataList.CastChannelByIndex[LCount].UpStreamNodeNumber;
        AReservoirDataList.AddInferedNode(LNodeNumber);
        LNodeNumber := AChannelDataList.CastChannelByIndex[LCount].DownStreamNodeNumber;
        AReservoirDataList.AddInferedNode(LNodeNumber);
      end;
      Result := True;
    end;
  except on E : Exception do HandleError(E,OPNAME); end;
end;}



procedure TReservoirDataLoadAgent.LoadHistoricWaterLevelContextData(AContextData: TStringList; ARecordIdentifier,
                                                                    AFieldNameIdentifier: string);
const OPNAME = 'TReservoirDataLoadAgent.LoadHistoricWaterLevelContextData';
begin
  try
    FSQLAgent.LoadHistoricDamLevel(AContextData, ARecordIdentifier,AFieldNameIdentifier);
  except on E : Exception do HandleError(E,OPNAME); end;
end;


function TReservoirDataLoadAgent.LoadReservoirDownStreamChannels(AReservoirDataList: TReservoirDataList): boolean;
const OPNAME = 'TReservoirDataLoadAgent.LoadReservoirDownStreamChannels';
var
  lIndex         : integer;
  lChannelNr     : integer;
  lChannelNrs    : TStringList;
  lReservoirData : TReservoirData;
  LReservoirID   : integer;
  lDataSet       : TAbstractModelDataset;
begin
  Result := FALSE;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      if (Assigned(lDataSet)) then
      begin
        try
          lChannelNrs   := TStringList.Create;
          LDataSet.SetSQL(FSQLAgent.GetDownStreamPowerChannelSQL);
          LDataset.DataSet.Open;
          while (NOT LDataSet.DataSet.EOF) do
          begin
            lChannelNrs.Clear;
            lIndex := 1;
            LReservoirID := LDataset.DataSet.FieldByName('Identifier').AsInteger;
            while (lIndex <= 20) do
            begin
              lChannelNr := lDataSet.DataSet.FieldByName(Format('Channel%2.2d',[lIndex])).AsInteger;
              LChannelNrs.Add(IntToStr(lChannelNr));
              lIndex := lIndex + 1;
            end;
            LReservoirData := AReservoirDataList.CastReservoirByRecordIdentifier[LReservoirID];
            if Assigned(LReservoirData) then
            begin
                LReservoirData.PopulateDownStreamPowerChannels(lChannelNrs);
            end;
            lDataSet.DataSet.Next;
          end;
        finally
          FreeAndNil(lChannelNrs);
        end;
        LDataset.DataSet.Close;
        Result := TRUE;
      end;
    finally
      LDataset.Free;
    end;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TReservoirDataLoadAgent.LoadReservoirAreaGroup(AReservoirAreaGroupList: TReservoirAreaGroupList): boolean;
const OPNAME = 'TReservoirDataLoadAgent.LoadReservoirAreaGroup';
var
  LDataSet                : TAbstractModelDataset;
  LResult                 : Boolean;
  LReservoirAreaGroup     : TReservoirAreaGroup;
  LGroupID                : integer;
  LGroupName              : string;
begin
  Result := FALSE;
  try
    lResult := TRUE;
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      if Assigned(LDataSet) then
      begin
        LDataSet.SetSQL(FSQLAgent.GetReservoirAreaGroupSQL);
        LDataset.DataSet.Open;
        while ((NOT LDataset.DataSet.EOF) AND lResult) do
        begin
          LGroupID       := LDataset.DataSet.FieldByName('GroupID').AsInteger;
          LGroupName     := Trim(LDataset.DataSet.FieldByName('GroupName').AsString);
          LReservoirAreaGroup     := AReservoirAreaGroupList.NewReservoirAreaGroup;
          LReservoirAreaGroup.Initialise;
          lResult := LReservoirAreaGroup.Populate(LGroupID, LGroupName);
          LDataset.DataSet.Next;
        end;
      end;
    finally
      LDataset.DataSet.Close;
      LDataset.Free;
    end;
    Result := lResult;
  except on E: Exception do HandleErrorFunction(E, OPNAME, Result) end;
end;

end.
