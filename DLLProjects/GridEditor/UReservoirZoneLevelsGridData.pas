unit UReservoirZoneLevelsGridData;

interface

uses
  Classes,
  UViewDataItem,
  UGridFieldData,
  UReservoirZoneLevelsSQLAgent;

type
  TDrawDownArray = array of Double;
  TPenaltyArray = array of Double;
  TReservoirZoneLevelsGridData = class(TGridData)
  protected
    FSQLAgent: TReservoirZoneLevelsSQLAgent;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    function GetReservoirIdentifier(AViewDataNode: TViewDataNode; var AReservoirIdentifier: integer): boolean;
    function GetPenaltyStructure(AReservoirIdentifier: integer; var APenaltyStructure: integer): boolean;
    function LoadZoneNames(AZoneNames: TStringList): boolean;
    function LoadDrawDownElevations(AReservoirIdentifier:integer; var ADrawDownArray: TDrawDownArray): boolean;
    procedure PopulateZoneNames(AZoneNames: TStringList; ACountOfPenaltyLevels: integer);
    procedure PopulatePenalties(AZoneNames: TStringList; APenaltyStruct, ACountOfPenaltyLevels: integer);
    function LoadPenalties(APenaltyStruct: integer; var APenaltyArray: TPenaltyArray): boolean;
    procedure PopulateElevations(AZoneNames: TStringList; AReservoirIdentifier: integer; ADrawDownArray: TDrawDownArray);
    function LoadFixedElevations(AReservoirIdentifier:integer;var ABottomOfReservoir, ADeadStorage, AFullSupply: string): boolean;
  public
    procedure ConstructData(AViewDataNode: TViewDataNode);
  end;

implementation

uses
  SysUtils,
  UDataSetType,
  UAbstractObject,
  UErrorHandlingOperations;
const
  CFixedLevels = 3;

procedure TReservoirZoneLevelsGridData.CreateMemberObjects;
const OPNAME = 'TReservoirZoneLevelsGridData.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
    FSQLAgent := TReservoirZoneLevelsSQLAgent.Create(FAppModules);
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TReservoirZoneLevelsGridData.DestroyMemberObjects;
const OPNAME = 'TReservoirZoneLevelsGridData.DestroyMemberObjects';
begin
  try
    FreeAndNil(FSQLAgent);
    inherited DestroyMemberObjects;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TReservoirZoneLevelsGridData.ConstructData(AViewDataNode: TViewDataNode);
const OPNAME = 'TReservoirZoneLevelsGridData.ConstructData';
var
  LAllZoneNames: TStringList;
  LReservoirIdentifier, LPenaltyStructure,
  LCountOfPenaltyLevels: integer;
  LDrawDownArray: TDrawDownArray;
begin
  try

    // Get the context data.
    if GetReservoirIdentifier(AViewDataNode, LReservoirIdentifier) then
    begin
      if GetPenaltyStructure(LReservoirIdentifier, LPenaltyStructure) then
      begin

        // Load back ground grid data.
        LAllZoneNames := TStringList.Create;
        try
          if LoadZoneNames(LAllZoneNames) then
          begin
            if LoadDrawDownElevations(LReservoirIdentifier, LDrawDownArray) then
            begin
              LCountOfPenaltyLevels := CFixedLevels + Length(LDrawDownArray);

              // Populate grid data.
              PopulateZoneNames(LAllZoneNames, LCountOfPenaltyLevels);
              PopulatePenalties(LAllZoneNames, LPenaltyStructure, LCountOfPenaltyLevels);
              PopulateElevations(LAllZoneNames, LReservoirIdentifier, LDrawDownArray);
            end;

            // Done.
            Finalize(LDrawDownArray);
          end;
        finally
          LAllZoneNames.Free;
        end;
      end;
    end;

  // Handle exceptions.
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TReservoirZoneLevelsGridData.GetReservoirIdentifier(AViewDataNode: TViewDataNode; var AReservoirIdentifier: integer): boolean;
const OPNAME = 'TReservoirZoneLevelsGridData.GetReservoirIdentifier';
begin
  Result := False;
  try
    if Assigned(AViewDataNode) and
       (AViewDataNode.ViewDataSetCount > 0) and
       (Length(AViewDataNode.ViewDataSet[0].ParamValues) > 0) then
    begin

      // This means that this is the main node.
      try
        AReservoirIdentifier := StrToInt(AViewDataNode.ViewDataSet[0].ParamValues[0]);
        Result := True;
      except end;
    end
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TReservoirZoneLevelsGridData.GetPenaltyStructure(
  AReservoirIdentifier: integer; var APenaltyStructure: integer): boolean;
const OPNAME = 'TReservoirZoneLevelsGridData.GetPenaltyStructure';
var
  LPenaltyStructure: integer;
  LDataSet: TAbstractModelDataset;
begin
  Result := False;
  APenaltyStructure := -1;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      if Assigned(LDataSet) then
      begin
        LDataSet.SetSQL(FSQLAgent.GetPenaltyStructureSQL(AReservoirIdentifier));
        LDataset.DataSet.Open;
        if (not LDataset.DataSet.Eof) then
        begin
          LPenaltyStructure := LDataset.DataSet.FieldByName('PenaltyStruct').AsInteger;
          APenaltyStructure := LPenaltyStructure;
        end;
        LDataset.DataSet.Close;
        Result := True;
      end;
    finally
      LDataset.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirZoneLevelsGridData.LoadZoneNames(AZoneNames: TStringList): boolean;
const OPNAME = 'TReservoirZoneLevelsGridData.LoadZoneNames';
var LDataSet: TAbstractModelDataset;
begin
  Result := False;
  try

    // Load the generic information.
    AZoneNames.Clear;

    // Loop for all the records in the table.
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      if Assigned(LDataSet) then
      begin
        LDataSet.SetSQL(FSQLAgent.GetReservoirZoneNamesDataSQL);
        LDataset.DataSet.Open;
        while (not LDataset.DataSet.Eof) do
        begin
          AZoneNames.Add(
            Trim(LDataset.DataSet.FieldByName('Identifier').AsString) + '=' +
            Trim(LDataset.DataSet.FieldByName('ReservoirZoneName').AsString));
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

function TReservoirZoneLevelsGridData.LoadDrawDownElevations(AReservoirIdentifier: integer;
  var ADrawDownArray: TDrawDownArray): boolean;
const OPNAME = 'TReservoirZoneLevelsGridData.LoadDrawDownElevations';
var LDataSet: TAbstractModelDataset;
begin
  Result := False;
  SetLength(ADrawDownArray, 0);
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      if Assigned(LDataSet) then
      begin
        LDataSet.SetSQL(FSQLAgent.GetDrawDownElevationsSQL(AReservoirIdentifier));
        LDataset.DataSet.Open;
        while (not LDataset.DataSet.Eof) do
        begin
          SetLength(ADrawDownArray, length(ADrawDownArray) + 1);
          ADrawDownArray[Length(ADrawDownArray) - 1] := LDataset.DataSet.FieldByName('DrawDown').AsFloat;
          LDataset.DataSet.Next;
          Result := True;
        end;
        LDataset.DataSet.Close;
      end;
    finally
      LDataset.Free;
    end;
  except on E: Exception do HandleErrorFunction(E, OPNAME, Result) end;
end;

procedure TReservoirZoneLevelsGridData.PopulateZoneNames(AZoneNames: TStringList; ACountOfPenaltyLevels: integer);
const OPNAME = 'TReservoirZoneLevelsGridData.PopulateZoneNames';
var
  LContextData: TStringList;
  LLevelIndex: integer;
begin
  try

    // Add the grid field to contain the data.
    AddGridField('ReservoirZoneName');

    // Loop for every zone name.
    LContextData := TStringList.Create;
    try
      for LLevelIndex := 0 to AZoneNames.Count - 1 do
      begin
        FSQLAgent.LoadZoneNamesContextData(LContextData,
          AZoneNames.Names[LLevelIndex], IntToStr(ACountOfPenaltyLevels - LLevelIndex));
        GridField[0].AddFieldData(AZoneNames.Values[AZoneNames.Names[LLevelIndex]], LContextData, -1);
      end;
    finally
      LContextData.Free;
    end;

  // Handle exceptions.
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TReservoirZoneLevelsGridData.PopulatePenalties(AZoneNames: TStringList; APenaltyStruct, ACountOfPenaltyLevels: integer);
const OPNAME = 'TReservoirZoneLevelsGridData.PopulatePenalties';
var
  LContextData: TStringList;
  LLevelIndex: integer;
  LPenaltyArray: TPenaltyArray;
begin
  try
    LContextData := TStringList.Create;
    try
      if LoadPenalties(APenaltyStruct, LPenaltyArray) then
      begin
        AddGridField('ReservoirPenalty');
        for LLevelIndex := 0 to AZoneNames.Count - 1 do
        begin
          FSQLAgent.LoadPenaltyContextData(LContextData,
            AZoneNames.Names[LLevelIndex], IntToStr(APenaltyStruct),
            IntToStr(ACountOfPenaltyLevels - LLevelIndex));
          GridField[1].AddFieldData(FloatToStr(LPenaltyArray[LLevelIndex]), LContextData, -1);
        end;
      end;
    finally
      LContextData.Free;
    end;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TReservoirZoneLevelsGridData.LoadPenalties(APenaltyStruct: integer; var APenaltyArray: TPenaltyArray): boolean;
const OPNAME = 'TReservoirZoneLevelsGridData.LoadPenalties';
var LDataSet: TAbstractModelDataset;
begin
  Result := False;
  try
    SetLength(APenaltyArray, 0);
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      if Assigned(LDataSet) then
      begin
        LDataSet.SetSQL(FSQLAgent.GetPenaltiesSQL);
        LDataset.DataSet.Open;
        while (not LDataset.DataSet.Eof) do
        begin
          SetLength(APenaltyArray, Length(APenaltyArray) + 1);
          case APenaltyStruct of
            1 : APenaltyArray[High(APenaltyArray)] := LDataset.DataSet.FieldByName('BalRef01').AsFloat;
            2 : APenaltyArray[High(APenaltyArray)] := LDataset.DataSet.FieldByName('BalRef02').AsFloat;
            3 : APenaltyArray[High(APenaltyArray)] := LDataset.DataSet.FieldByName('BalRef03').AsFloat;
            4 : APenaltyArray[High(APenaltyArray)] := LDataset.DataSet.FieldByName('BalRef04').AsFloat;
            5 : APenaltyArray[High(APenaltyArray)] := LDataset.DataSet.FieldByName('BalRef05').AsFloat;
          end;
          LDataset.DataSet.Next;
        end;
        LDataset.DataSet.Close;
        Result := true;
      end;
    finally
      LDataset.Free;
    end;
  except on E: Exception do HandleErrorFunction(E, OPNAME, Result) end;
end;

procedure TReservoirZoneLevelsGridData.PopulateElevations(
   AZoneNames: TStringList; AReservoirIdentifier: integer; ADrawDownArray: TDrawDownArray);
const OPNAME = 'TReservoirZoneLevelsGridData.PopulateElevations';
  CFixedLevels = 3;
var
  LContextData: TStringList;
  LBottomOfReservoir, LDeadStorage, LFullSupply: string;
  LLevelIndex: integer;
  LNVLinkTableFieldNameIdentifier: integer;
begin
  try
    LContextData := TStringList.Create;
    try
      if LoadFixedElevations(AReservoirIdentifier, LBottomOfReservoir, LDeadStorage, LFullSupply) then
      begin

        // Add the grid field to contain the data.
        AddGridField('SurfaceElevation');

        // Process the full supply level of the reservoir.
        LNVLinkTableFieldNameIdentifier := (CFixedLevels - 0) + Length(ADrawDownArray);
        FSQLAgent.LoadFixedElevationsContextData(LContextData, IntToStr(AReservoirIdentifier), IntToStr(LNVLinkTableFieldNameIdentifier));
        GridField[2].AddFieldData(LFullSupply, LContextData, -1);
        GridField[2].FieldData[0].FieldProperty := FAppModules.FieldProperties.FieldProperty('FullSupplyLevel');

        // Process the multiple draw down levels of the reservoir.
        for LLevelIndex := 0 to High(ADrawDownArray) do
        begin
          LNVLinkTableFieldNameIdentifier := (CFixedLevels - 1) + Length(ADrawDownArray) - LLevelIndex;
          FSQLAgent.LoadDrawDownElevationsContextData(LContextData,
            IntToStr(AReservoirIdentifier), AZoneNames.Names[LLevelIndex], IntToStr(LNVLinkTableFieldNameIdentifier));
          GridField[2].AddFieldData(FloatToStr(ADrawDownArray[LLevelIndex]), LContextData, -1);
        end;

        // Process the dead storage level of the reservoir.
        LNVLinkTableFieldNameIdentifier := CFixedLevels - 1;
        FSQLAgent.LoadFixedElevationsContextData(LContextData, IntToStr(AReservoirIdentifier), IntToStr(LNVLinkTableFieldNameIdentifier));
        GridField[2].AddFieldData(LDeadStorage, LContextData, -1);
        GridField[2].FieldData[1 + Length(ADrawDownArray)].FieldProperty :=
          FAppModules.FieldProperties.FieldProperty('DeadStorageLevel');

        // Process the bottom of the reservoir.
        LNVLinkTableFieldNameIdentifier := CFixedLevels - 2;
        FSQLAgent.LoadFixedElevationsContextData(LContextData, IntToStr(AReservoirIdentifier), IntToStr(LNVLinkTableFieldNameIdentifier));
        GridField[2].AddFieldData(LBottomOfReservoir, LContextData, -1);
        GridField[2].FieldData[2 + Length(ADrawDownArray)].FieldProperty :=
          FAppModules.FieldProperties.FieldProperty('BottomOfReservoir');
      end;
    finally
      LContextData.Free;
    end;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TReservoirZoneLevelsGridData.LoadFixedElevations(AReservoirIdentifier:integer;
  var ABottomOfReservoir, ADeadStorage, AFullSupply: string): boolean;
const OPNAME = 'TReservoirZoneLevelsGridData.LoadFixedElevations';
var LDataSet: TAbstractModelDataset;
begin
  Result := False;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      if Assigned(LDataSet) then
      begin
        LDataSet.SetSQL(FSQLAgent.GetFixedElevationsSQL(AReservoirIdentifier));
        LDataset.DataSet.Open;
        if (not LDataset.DataSet.Eof) then
        begin
          AFullSupply := Trim(LDataset.DataSet.FieldByName('FullSupplyLevel').AsString);
          ADeadStorage := Trim(LDataset.DataSet.FieldByName('DeadStorageLevel').AsString);
          ABottomOfReservoir := Trim(LDataset.DataSet.FieldByName('BottomOfReservoir').AsString);
          Result := True;
        end;
        Result := True;
        LDataset.DataSet.Close;
      end;
    finally
      LDataset.Free;
    end;
  except on E: Exception do HandleErrorFunction(E, OPNAME, Result) end;
end;

end.
