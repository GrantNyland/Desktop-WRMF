//
//
//  UNIT      : Contains TReservoirZoneLevelsData Class
//  AUTHOR    : Titi Ngubane (PDNA)
//  DATE      : 2003/03/05
//  COPYRIGHT : Copyright © 2003 DWAF
//
//
unit UReservoirZoneElevationData;

interface

uses
  Contnrs,
  Classes,
  UAbstractObject,
  VoaimsCom_TLB;

type
  TFixedElevation = class(TAbstractAppObject,IFixedElevation)
  protected
    FElevation : double;
    FElevationName: string;
    FReservoirID: integer;
    FPenaltyStructureID: integer;
    FPenaltyValueID: integer;

    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    function RecalculateAreaWhenFull: WordBool;
    function UpdateDrawDownLevelFromDSL: WordBool;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    function CopyFrom (AResID  : integer;
                       ASource : TFixedElevation) : boolean;
  public
    procedure Assign(ASource : TFixedElevation); virtual;
    procedure Set_Elevation(ANewValue: double); safecall;
    function Get_Elevation: double; safecall;
    function Get_PenaltyValue: double; safecall;
    function Get_ElevationName: WideString; safecall;
    function Get_ReservoirData: IReservoirData; safecall;
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; safecall;

    function Initialise: boolean; override;
    function PopulateElevationName(AElevationName: string): boolean;
    function PopulateElevation(AElevation : double): boolean;
    function PopulateReservoirData(AReservoirData: IReservoirData): boolean;
    procedure InitialiseNewFixedElevation(AReservoirData: IReservoirData);

    property Elevation: Double read Get_Elevation write Set_Elevation;
    property ElevationName: WideString read Get_ElevationName;
    property PenaltyValue: double read Get_PenaltyValue;
    property ReservoirData: IReservoirData read Get_ReservoirData;
  end;

  TDrawDownElevation = class(TAbstractAppObject,IDrawDownElevation)
  protected
    FMonthlyElevations: array of double;
    FLevelIdentifier,
    FReservoirIdentifier : integer;
    FAverageElevations: double;

    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    function CopyFrom (AResID  : integer;
                       ASource : TDrawDownElevation) : boolean;
  public
    procedure Assign(ASource : TDrawDownElevation); virtual;
    function Initialise: boolean; override;
    procedure CalculateAvarageElevation;safecall;
    function PopulateDrawDownElevations(ADataSet: TAbstractModelDataset): boolean;
    function PopulateReservoirData(AReservoirData: IReservoirData): boolean;
    procedure InitialiseNewDrawDownElevation(ALevelIdentifier: integer;AReservoirData: IReservoirData);

    function Get_AverageElevations : double; safecall;
    function Get_MonthlyElevationByIndex(AMonthIndex: integer): double; safecall;
    procedure Set_MonthlyElevationByIndex(AMonthIndex: integer;ANewValue: double); safecall;
    function Get_ReservoirData: IReservoirData;  safecall;
    function Get_LevelIdentifier: integer; safecall;
    function Get_ReservoirIdentifier: integer;  safecall;
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; safecall;
    function GetKeyValues (const AParamField : WideString; const AFieldIndex: WideString) : WideString; safecall;

    property AverageElevations: Double read Get_AverageElevations;
    property MonthlyElevationByIndex[AMonthIndex: Integer]: Double read Get_MonthlyElevationByIndex write Set_MonthlyElevationByIndex;
    property ReservoirData: IReservoirData read Get_ReservoirData;
    property LevelIdentifier: Integer read Get_LevelIdentifier;
    property ReservoirIdentifier: Integer read Get_ReservoirIdentifier;
  end;

  TInitialLevelsData = class(TAbstractAppObject,IInitialLevelsData)
  protected
    FInitialLevels: array of double;
    FReservoirIdentifier : integer;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    function CopyFrom (AResID  : integer;
                       ASource : TInitialLevelsData) : boolean;
  public
    procedure Assign(ASource : TInitialLevelsData); virtual;
    function Initialise: boolean; override;
    procedure UpdateInitialLevelValue(ALevelIndex: integer;ANewValue: double);safecall;
    function LoadInitialLevelsFromDataset(ADataSet: TAbstractModelDataset): boolean;
    procedure InitialiseNewInitialLevels(AReservoirData: IReservoirData);

    procedure Set_InitialLevelsByIndex(ALevelIndex: integer;ANewValue: double); safecall;
    function Get_InitialLevelsByIndex(ALevelIndex: integer): double; safecall;
    function Get_ReservoirIdentifier: integer;  safecall;
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; safecall;
    function GetKeyValues (const AParamField : WideString; const AFieldIndex: WideString) : WideString; safecall;

    property ReservoirIdentifier: Integer read Get_ReservoirIdentifier;
    property InitialLevelsByIndex[ALevelIndex: Integer]: Double read Get_InitialLevelsByIndex write Set_InitialLevelsByIndex;
  end;

  TReservoirZoneElevationData = class(TAbstractAppObject,IReservoirZoneElevationsData)
  protected
    FInitialLevelsData     : TInitialLevelsData;
    FDrawDownLevelData     : TObjectList;
    FBottomOfReservoirData : TFixedElevation;
    FDeadStorageLevelData  : TFixedElevation;
    FFullSupplyLevelData   : TFixedElevation;
    FReservoirIdentifier   : integer;

    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;

    function GetInitialLevelsDataCast: TInitialLevelsData;
    function GetDrawDownLevelByIndexCast(ALevelIndex: integer): TDrawDownElevation;
    procedure SetReservoirData(AReservoirData: IReservoirData);
    procedure InsertDrawDownElevation(ADrawDownElevation:TDrawDownElevation; ACurrentReservoir: boolean);
  public
    procedure Assign(ASource : TReservoirZoneElevationData); virtual;
    procedure SortDrawDownLevels;
    function Initialise: boolean; override;

    function CreateDrawDownLevel(AZoneLevel: integer; AReservoirData: IReservoirData; ACurrentReservoir: boolean):TDrawDownElevation ;
    function DeleteDrawDownLevel(AZoneLevel: integer;ADeleteAction: TDeleteAction):boolean ;

    function PopulateReservoirZoneElevationData(ADataSet: TAbstractModelDataset): boolean;
    function PopulateReservoirData(AReservoirData: IReservoirData): boolean;
    procedure AddDrawDownElevation(ADrawDownElevation:TDrawDownElevation);
    procedure InitialiseNewZoneElevation(AReservoirData: IReservoirData);
    procedure UpdateDrawDownLevelFromDSL; safecall;

    function Get_BottomOfReservoir: IFixedElevation; safecall;
    function Get_DeadStorageLevel: IFixedElevation; safecall;
    function Get_FullSupplyLevel: IFixedElevation; safecall;
    function Get_DrawDownLevelByIndex(ALevelIndex: integer): IDrawDownElevation; safecall;
    function Get_ReservoirZoneLevelsCount: integer; safecall;
    function Get_ReservoirDrawDownLevelsCount: integer; safecall;
    function Get_InitialLevelsData: IInitialLevelsData; safecall;
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; safecall;
    function GetKeyValues (const AParamField : WideString; const AFieldIndex: WideString) : WideString; safecall;
    function CopyFrom (AResID  : integer;
                       ASource : TReservoirZoneElevationData) : boolean;

    property ReservoirZoneLevelsCount: Integer read Get_ReservoirZoneLevelsCount;
    property ReservoirDrawDownLevelsCount: Integer read Get_ReservoirDrawDownLevelsCount;
    property BottomOfReservoir: IFixedElevation read Get_BottomOfReservoir;
    property DeadStorageLevel: IFixedElevation read Get_DeadStorageLevel;
    property FullSupplyLevel: IFixedElevation read Get_FullSupplyLevel;
    property InitialLevelsData: IInitialLevelsData read Get_InitialLevelsData;
    property DrawDownLevelByIndex[ALevelIndex: Integer]: IDrawDownElevation read Get_DrawDownLevelByIndex;

    property CastInitialLevelsData: TInitialLevelsData read GetInitialLevelsDataCast;
  end;

implementation

uses
  System.Types,
  SysUtils,
  UConstants,
  UYieldModelDataObject,
  UDDTSDataObject,
  UReservoirDataSQLAgent,
  UErrorHandlingOperations;

const
  CReservoirNameFieldIdentifier = 'ReservoirName';
  CFixedLevels = 3;

{ TReservoirZoneElevationData }

procedure TReservoirZoneElevationData.CreateMemberObjects;
const OPNAME = 'TReservoirZoneElevationData.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
    FReservoirIdentifier :=  0;
    FDrawDownLevelData := TObjectList.Create;
    FBottomOfReservoirData := TFixedElevation.Create(FAppModules);
    FBottomOfReservoirData.PopulateElevationName('BottomOfReservoir');
    FDeadStorageLevelData := TFixedElevation.Create(FAppModules);
    FDeadStorageLevelData.PopulateElevationName('DeadStorageLevel');
    FFullSupplyLevelData := TFixedElevation.Create(FAppModules);
    FFullSupplyLevelData.PopulateElevationName('FullSupplyLevel');
    FInitialLevelsData  :=  TInitialLevelsData.Create(AppModules);

    Initialise;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TReservoirZoneElevationData.DestroyMemberObjects;
const OPNAME = 'TReservoirZoneElevationData.DestroyMemberObjects';
begin
  try
    FreeAndNil(FDrawDownLevelData);
    FreeAndNil(FBottomOfReservoirData);
    FreeAndNil(FDeadStorageLevelData);
    FreeAndNil(FFullSupplyLevelData);
    FreeAndNil(FInitialLevelsData);
    inherited DestroyMemberObjects;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TReservoirZoneElevationData.Initialise: boolean;
const OPNAME = 'TReservoirZoneElevationData.Initialise';
begin
  Result := False;
  try
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirZoneElevationData.Get_BottomOfReservoir: IFixedElevation;
const OPNAME = 'TReservoirZoneElevationData.Get_BottomOfReservoir';
begin
  Result := nil;
  try
    Result := FBottomOfReservoirData;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TReservoirZoneElevationData.Get_DeadStorageLevel: IFixedElevation;
const OPNAME = 'TReservoirZoneElevationData.Get_DeadStorageLevel';
begin
  Result := nil;
  try
    Result := FDeadStorageLevelData;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TReservoirZoneElevationData.Get_FullSupplyLevel: IFixedElevation;
const OPNAME = 'TReservoirZoneElevationData.Get_FullSupplyLevel';
begin
  Result := nil;
  try
    Result := FFullSupplyLevelData;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TReservoirZoneElevationData.Get_ReservoirZoneLevelsCount: integer;
const OPNAME = 'TReservoirZoneElevationData.Get_ReservoirZoneLevelsCount';
begin
  Result := 0;
  try
    Result := FDrawDownLevelData.Count + CFixedLevels;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirZoneElevationData.Get_ReservoirDrawDownLevelsCount: integer;
const OPNAME = 'TReservoirZoneElevationData.Get_ReservoirDrawDownLevelsCount';
begin
  Result := 0;
  try
    Result := FDrawDownLevelData.Count;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirZoneElevationData.AddDrawDownElevation(ADrawDownElevation: TDrawDownElevation);
const OPNAME = 'TReservoirZoneElevationData.AddDrawDownElevation';
begin
  try
    if Assigned(ADrawDownElevation) then
    begin
      FDrawDownLevelData.Add(ADrawDownElevation);
      SortDrawDownLevels;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirZoneElevationData.InsertDrawDownElevation(
          ADrawDownElevation: TDrawDownElevation; ACurrentReservoir: boolean);
const OPNAME = 'TReservoirZoneElevationData.InsertDrawDownElevation';
var
  LIndex,LCount: integer;
  LBeforeDrawDownElevation,
  LAfterDrawDownElevation,
  LDrawDownElevation: TDrawDownElevation;
begin
  try
    if Assigned(ADrawDownElevation) then
    begin
      LDrawDownElevation       := ADrawDownElevation;
      LBeforeDrawDownElevation := nil;
      LAfterDrawDownElevation  := nil;
      if(FDrawDownLevelData.Count > 0) then
      begin
        if((LDrawDownElevation.FLevelIdentifier -2) >= 0) and
          ((LDrawDownElevation.FLevelIdentifier -2) < FDrawDownLevelData.Count) then
          LBeforeDrawDownElevation := GetDrawDownLevelByIndexCast(LDrawDownElevation.FLevelIdentifier -2);
        if((LDrawDownElevation.FLevelIdentifier -1) >= 0) and
          ((LDrawDownElevation.FLevelIdentifier -1) < FDrawDownLevelData.Count) then
          LAfterDrawDownElevation := GetDrawDownLevelByIndexCast(LDrawDownElevation.FLevelIdentifier-1);

        for LIndex := 0 to FDrawDownLevelData.Count -1 do
        begin
          LDrawDownElevation := GetDrawDownLevelByIndexCast(LIndex);
          if(LDrawDownElevation.FLevelIdentifier >= ADrawDownElevation.FLevelIdentifier) then
             LDrawDownElevation.FLevelIdentifier := LDrawDownElevation.FLevelIdentifier + 1;
        end;
      end;
      AddDrawDownElevation(ADrawDownElevation);
      SortDrawDownLevels;


      if Assigned(LBeforeDrawDownElevation) and Assigned(LAfterDrawDownElevation) then
      begin
        for LCount := Low(LBeforeDrawDownElevation.FMonthlyElevations) to
                      High(LBeforeDrawDownElevation.FMonthlyElevations) do
        begin
          if ACurrentReservoir then
             ADrawDownElevation.FMonthlyElevations[LCount] :=
              (LBeforeDrawDownElevation.FMonthlyElevations[LCount] +
              LAfterDrawDownElevation.FMonthlyElevations[LCount]) / 2.0
          else
             ADrawDownElevation.FMonthlyElevations[LCount] :=
              LAfterDrawDownElevation.FMonthlyElevations[LCount];
        end;
      end else
      if Assigned(LBeforeDrawDownElevation) and (not Assigned(LAfterDrawDownElevation)) then
      begin
        for LCount := Low(LBeforeDrawDownElevation.FMonthlyElevations) to High(LBeforeDrawDownElevation.FMonthlyElevations) do
        begin
          ADrawDownElevation.FMonthlyElevations[LCount] := LBeforeDrawDownElevation.FMonthlyElevations[LCount];
        end;
      end else
      if (not Assigned(LBeforeDrawDownElevation)) and Assigned(LAfterDrawDownElevation) then
      begin
        for LCount := Low(LAfterDrawDownElevation.FMonthlyElevations) to High(LAfterDrawDownElevation.FMonthlyElevations) do
        begin
          ADrawDownElevation.FMonthlyElevations[LCount] := LAfterDrawDownElevation.FMonthlyElevations[LCount];
        end;
      end;
      ADrawDownElevation.CalculateAvarageElevation;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirZoneElevationData.Get_DrawDownLevelByIndex(ALevelIndex: integer): IDrawDownElevation;
const OPNAME = 'TReservoirZoneElevationData.Get_DrawDownLevelByIndex';
begin
  Result := nil;
  try
    Result := TDrawDownElevation(FDrawDownLevelData[ALevelIndex]);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TReservoirZoneElevationData.GetDrawDownLevelByIndexCast(ALevelIndex: integer): TDrawDownElevation;
const OPNAME = 'TReservoirZoneElevationData.GetDrawDownLevelByIndexCast';
var
  Lindex : integer;
begin
  Result := nil;
  try
    if(ALevelIndex = FDrawDownLevelData.Count) then
      ALevelIndex := ALevelIndex - 1;
      
    for Lindex := 0 to FDrawDownLevelData.Count -1 do
    begin
      if Lindex = ALevelIndex then
        Result := TDrawDownElevation(FDrawDownLevelData[ALevelIndex]);
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TReservoirZoneElevationData.GetInitialLevelsDataCast: TInitialLevelsData;
const OPNAME = 'TReservoirZoneElevationData.GetInitialLevelsDataCast';
begin
  Result := nil;
  try
    Result := FInitialLevelsData;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

{ TDrawDownLevels }

procedure TDrawDownELevation.CreateMemberObjects;
const OPNAME = 'TDrawDownELevation.CreateMemberObjects';
var
  LMonthlyElevations: TAbstractFieldProperty;
begin
  inherited;
  try
    LMonthlyElevations := FAppModules.FieldProperties.FieldProperty('ReservoirLev');
    if not Assigned(LMonthlyElevations) then
      raise Exception.Create('Field (ReservoirLev) not found in field properties');
    SetLength(FMonthlyElevations,LMonthlyElevations.ArrayLength);

    Initialise;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TDrawDownELevation.DestroyMemberObjects;
const OPNAME = 'TDrawDownELevation.DestroyMemberObjects';
begin
  try
    Finalize(FMonthlyElevations);
    inherited;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TDrawDownElevation.Assign(ASource: TDrawDownElevation);
const OPNAME = 'TDrawDownElevation.Assign';
var
  LIndex         : integer;
  LDrawDownElevation : TAbstractFieldProperty;
begin
  try
    if (ASource <> nil) then
    begin
      LDrawDownElevation := FAppModules.FieldProperties.FieldProperty('ReservoirLev');
      for LIndex := LDrawDownElevation.ArrayLow to LDrawDownElevation.ArrayHigh do
        MonthlyElevationByIndex[LIndex] := ASource.FMonthlyElevations[LIndex];
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;


function TDrawDownELevation.Initialise:boolean;
const OPNAME = 'TDrawDownELevation.Initialise';
var
  LIndex: integer;
begin
  Result := False;
  try
    FLevelIdentifier     := 0;
    FReservoirIdentifier := 0;
    FAverageElevations   := 0.0;
    for LIndex := Low(FMonthlyElevations) to High(FMonthlyElevations) do
      FMonthlyElevations[LIndex] := 0.0;

    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDrawDownELevation.Get_MonthlyElevationByIndex(AMonthIndex: integer): double;
const OPNAME = 'TDrawDownELevation.Get_MonthlyElevationByIndex';
begin
  Result := -1.0;
  try
    Result := FMonthlyElevations[AMonthIndex];
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TDrawDownELevation.PopulateDrawDownElevations(ADataSet: TAbstractModelDataset): boolean;
const OPNAME = 'TDrawDownELevation.PopulateDrawDownElevations';
begin
  Result := False;
  try
    FReservoirIdentifier :=    ADataset.DataSet.FieldByName('ReservoirIdentifier').AsInteger;
    FLevelIdentifier :=    ADataset.DataSet.FieldByName('LevelIdentifier').AsInteger;
    FMonthlyElevations[1]  := ADataset.DataSet.FieldByName('ReservoirLev01').AsFloat;
    FMonthlyElevations[2]  := ADataset.DataSet.FieldByName('ReservoirLev02').AsFloat;
    FMonthlyElevations[3]  := ADataset.DataSet.FieldByName('ReservoirLev03').AsFloat;
    FMonthlyElevations[4]  := ADataset.DataSet.FieldByName('ReservoirLev04').AsFloat;
    FMonthlyElevations[5]  := ADataset.DataSet.FieldByName('ReservoirLev05').AsFloat;
    FMonthlyElevations[6]  := ADataset.DataSet.FieldByName('ReservoirLev06').AsFloat;
    FMonthlyElevations[7]  := ADataset.DataSet.FieldByName('ReservoirLev07').AsFloat;
    FMonthlyElevations[8]  := ADataset.DataSet.FieldByName('ReservoirLev08').AsFloat;
    FMonthlyElevations[9]  := ADataset.DataSet.FieldByName('ReservoirLev09').AsFloat;
    FMonthlyElevations[10] := ADataset.DataSet.FieldByName('ReservoirLev10').AsFloat;
    FMonthlyElevations[11] := ADataset.DataSet.FieldByName('ReservoirLev11').AsFloat;
    FMonthlyElevations[12] := ADataset.DataSet.FieldByName('ReservoirLev12').AsFloat;
    CalculateAvarageElevation;
    Result := True;
  except on E: Exception do HandleErrorFunction(E, OPNAME, Result) end;
end;

procedure TReservoirZoneElevationData.SetReservoirData(AReservoirData: IReservoirData);
const OPNAME = 'TReservoirZoneElevationData.SetReservoirData';
begin
  FBottomOfReservoirData.PopulateReservoirData(AReservoirData);
  FDeadStorageLevelData.PopulateReservoirData(AReservoirData);
  FFullSupplyLevelData.PopulateReservoirData(AReservoirData);
end;

function TReservoirZoneElevationData.Get_InitialLevelsData: IInitialLevelsData;
const OPNAME = 'TReservoirZoneElevationData.Get_InitialLevelsData';
begin
  Result := nil;
  try
    Result := FInitialLevelsData;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TReservoirZoneElevationData.PopulateReservoirZoneElevationData(ADataSet: TAbstractModelDataset): boolean;
const OPNAME = 'TReservoirZoneElevationData.PopulateReservoirZoneElevationData';
begin
  Result := False;
  try
    FBottomOfReservoirData.PopulateElevation(ADataset.DataSet.FieldByName('BottomOfReservoir').AsFloat);
    FDeadStorageLevelData.PopulateElevation(ADataset.DataSet.FieldByName('DeadStorageLevel').AsFloat);
    FFullSupplyLevelData.PopulateElevation(ADataset.DataSet.FieldByName('FullSupplyLevel').AsFloat);
    Result := True;
  except on E: Exception do HandleErrorFunction(E, OPNAME, Result) end;
end;

function TReservoirZoneElevationData.PopulateReservoirData(AReservoirData: IReservoirData): boolean;
const OPNAME = 'TReservoirZoneElevationData.PopulateReservoirData';
begin
  Result := False;
  try
    FReservoirIdentifier := AReservoirData.ReservoirConfigurationData.ReservoirIdentifier;
    FBottomOfReservoirData.PopulateReservoirData(AReservoirData);
    FDeadStorageLevelData.PopulateReservoirData(AReservoirData);
    FFullSupplyLevelData.PopulateReservoirData(AReservoirData);
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirZoneElevationData.InitialiseNewZoneElevation(AReservoirData: IReservoirData);
const OPNAME = 'TReservoirZoneElevationData.InitialiseNewZoneElevation';
var
  LIndex,
  LDrawDownCount: integer;
  LDrawDownElevation:TDrawDownElevation;
begin
  try
    FReservoirIdentifier := AReservoirData.ReservoirConfigurationData.ReservoirIdentifier;

    FInitialLevelsData.InitialiseNewInitialLevels(AReservoirData);
    FBottomOfReservoirData.InitialiseNewFixedElevation(AReservoirData);
    FDeadStorageLevelData.InitialiseNewFixedElevation(AReservoirData);
    FFullSupplyLevelData.InitialiseNewFixedElevation(AReservoirData);

    FDrawDownLevelData.Clear;
    FInitialLevelsData.FReservoirIdentifier := AReservoirData.ReservoirConfigurationData.ReservoirIdentifier;
    if(FAppModules.Model.ModelName <> CDDTS) then
    begin
      LDrawDownCount := TYieldModelDataObject(FAppModules.Model.ModelData).
                        NetworkElementData.ReservoirPenaltyStructureList.
                        ReservoirPenaltyCounts.StorageZoneCount;
      LDrawDownCount := LDrawDownCount - 3;
      for LIndex := 1 to LDrawDownCount do
      begin
        LDrawDownElevation := TDrawDownElevation.Create(FAppModules);
        LDrawDownElevation.InitialiseNewDrawDownElevation(LIndex,AReservoirData);
        AddDrawDownElevation(LDrawDownElevation);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirZoneElevationData.CreateDrawDownLevel(AZoneLevel: integer;
  AReservoirData: IReservoirData; ACurrentReservoir: boolean): TDrawDownElevation;
const OPNAME = 'TReservoirZoneElevationData.CreateDrawDownLevel';
var
  LDrawDownElevation:TDrawDownElevation;
begin
  Result := nil;
  try
    LDrawDownElevation := TDrawDownElevation.Create(FAppModules);
    LDrawDownElevation.InitialiseNewDrawDownElevation(AZoneLevel,AReservoirData);
    InsertDrawDownElevation(LDrawDownElevation,ACurrentReservoir);
    Result := LDrawDownElevation;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirZoneElevationData.DeleteDrawDownLevel(AZoneLevel: integer;ADeleteAction: TDeleteAction): boolean;
const OPNAME = 'TReservoirZoneElevationData.DeleteDrawDownLevel';
var
  LIndex: integer;
  LCurrentDrawDownElevation,
  LDrawDownElevation:TDrawDownElevation;
begin
  Result := False;
  try
    LDrawDownElevation := GetDrawDownLevelByIndexCast(AZoneLevel-1);
    if Assigned(LDrawDownElevation) then
    begin
      for LIndex := 0 to FDrawDownLevelData.Count -1 do
      begin
        LCurrentDrawDownElevation := GetDrawDownLevelByIndexCast(LIndex);
        if(LCurrentDrawDownElevation.FLevelIdentifier > AZoneLevel) then
           LCurrentDrawDownElevation.FLevelIdentifier := LCurrentDrawDownElevation.FLevelIdentifier -1;
      end;
      if(ADeleteAction = daClearData) then
      begin
        for LIndex := Low(LDrawDownElevation.FMonthlyElevations) to High(LDrawDownElevation.FMonthlyElevations) do
          LDrawDownElevation.FMonthlyElevations[LIndex] := 0.0;
        Result := True;
      end
      else
      if(ADeleteAction in [daContinue,daDeleteAll]) then
      begin
        FDrawDownLevelData.Remove(LDrawDownElevation);
        SortDrawDownLevels;
        Result := True;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirZoneElevationData.SortDrawDownLevels;
const OPNAME = 'TReservoirZoneElevationData.SortDrawDownLevels';
var
  LIndex: integer;
  LTempContainer: TStringList;
  LOwn: boolean;
  LIdentifier: string;
begin
  try
  LOwn := FDrawDownLevelData.OwnsObjects;
  try
    FDrawDownLevelData.OwnsObjects := False;
    LTempContainer := TStringList.Create;
    try
      LTempContainer.Sorted := True;
      LTempContainer.Duplicates := dupAccept;
      for LIndex := 0 to FDrawDownLevelData.Count - 1 do
      begin
        LIdentifier := Format('%2.2d',[DrawDownLevelByIndex[LIndex].LevelIdentifier]);
        LTempContainer.AddObject(LIdentifier, FDrawDownLevelData[LIndex]);
      end;
      FDrawDownLevelData.Clear;
      for LIndex := 0 to LTempContainer.Count - 1 do
        FDrawDownLevelData.Add(LTempContainer.Objects[LIndex]);

    finally
      LTempContainer.Free;
    end;

  finally
    FDrawDownLevelData.OwnsObjects := LOwn;
  end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirZoneElevationData.UpdateDrawDownLevelFromDSL;
const OPNAME = 'TReservoirZoneElevationData.UpdateDrawDownLevelFromDSL';
var
  LIndex : integer;
  LAllZero : boolean;
  LDrawDownElevation : TDrawDownElevation;
begin
  try
    if(FDrawDownLevelData.Count = 1) then
    begin
      LAllZero           := True;
      LDrawDownElevation := TDrawDownElevation(FDrawDownLevelData[0]);
      for LIndex := 1 to 12 do
      begin
        if(LDrawDownElevation.FMonthlyElevations[LIndex] > 0.0) then
        begin
          LAllZero  := False;
          Break;
        end;
      end;
      if LAllZero then
      begin
        for LIndex := 1 to 12 do
          LDrawDownElevation.Set_MonthlyElevationByIndex(LIndex,FDeadStorageLevelData.FElevation);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirZoneElevationData.Validate(var AErrors: WideString; const AContext: WideString): WordBool;
const OPNAME = 'TReservoirZoneElevationData.Validate';
begin
  Result := False;
  try
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirZoneElevationData._AddRef: Integer;
const OPNAME = 'TReservoirZoneElevationData._AddRef';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TReservoirZoneElevationData._Release: Integer;
const OPNAME = 'TReservoirZoneElevationData._Release';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TReservoirZoneElevationData.GetKeyValues(const AParamField,AFieldIndex: WideString): WideString;
const OPNAME = 'TReservoirZoneElevationData.GetKeyValues';
begin
  Result := '';
  try
    Result := GetDefaultKey;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirZoneElevationData.CopyFrom(AResID  : integer;
                                              ASource : TReservoirZoneElevationData): boolean;
const OPNAME = 'TReservoirZoneElevationData.CopyFrom';
var
  LIndex : integer;
begin
  Result := FALSE;
  try

    FInitialLevelsData.CopyFrom(AResID, ASource.FInitialLevelsData);
    FBottomOfReservoirData.CopyFrom(AResID,ASource.FBottomOfReservoirData);
    FDeadStorageLevelData.CopyFrom(AResID,ASource.FDeadStorageLevelData);
    FFullSupplyLevelData.CopyFrom(AResID,ASource.FFullSupplyLevelData);
    for LIndex := 0 to FDrawDownLevelData.Count -1 do
      TDrawDownElevation(FDrawDownLevelData[LIndex]).CopyFrom(AResID, ASource.GetDrawDownLevelByIndexCast(LIndex))
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirZoneElevationData.Assign(ASource: TReservoirZoneElevationData);
const OPNAME = 'TReservoirZoneElevationData.Assign';
var
  LIndex : integer;
begin
  try
    if (ASource <> nil) then
    begin
      FInitialLevelsData.Assign(ASource.FInitialLevelsData);
      FBottomOfReservoirData.Assign(ASource.FBottomOfReservoirData);
      FDeadStorageLevelData.Assign(ASource.FDeadStorageLevelData);
      FFullSupplyLevelData.Assign(ASource.FFullSupplyLevelData);
      for LIndex := 0 to FDrawDownLevelData.Count -1 do
        TDrawDownElevation(FDrawDownLevelData[LIndex]).Assign(ASource.GetDrawDownLevelByIndexCast(LIndex))


    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{ TFixedElevation }

procedure TFixedElevation.CreateMemberObjects;
const OPNAME = 'TFixedElevation.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
    Initialise;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TFixedElevation.DestroyMemberObjects;
const OPNAME = 'TFixedElevation.DestroyMemberObjects';
begin
  try
    inherited DestroyMemberObjects;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TFixedElevation.Initialise: boolean;
const OPNAME = 'TFixedElevation.Initialise';
begin
  Result := False;
  try
    FElevation            := 0.0;
    FReservoirID          := 0;
    FPenaltyValueID       := 0;
    FPenaltyStructureID   := 0;
    FElevationName        := '';
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFixedElevation.Set_Elevation(ANewValue: double);
const OPNAME = 'TFixedElevation.Set_Elevation';
var
  LLoadAgent: TReservoirDataSQLAgent;
  LContextData: TStringList;
  LPrevValue: double;
begin
  try
    LLoadAgent := TReservoirDataSQLAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        LLoadAgent.LoadNodesDetailsContextData(LContextData,IntToStr(ReservoirData.ReservoirConfigurationData.ReservoirIdentifier));
        if FAppModules.FieldProperties.UpdateFieldValue(
             ElevationName, FloatToStr(ANewValue), FloatToStr(Elevation), LContextData) then
        begin
          LPrevValue := FElevation;
          FElevation := ANewValue;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,ElevationName,FloatToStr(LPrevValue),FloatToStr(ANewValue));
        end;
        RecalculateAreaWhenFull;
        UpdateDrawDownLevelFromDSL;
      finally
        LContextData.Free;
      end;
    finally
      LLoadAgent.Free;
    end;

  except on E: Exception do HandleError(E, OPNAME) end;
end;


function TFixedElevation.Get_Elevation: double;
const OPNAME = 'TFixedElevation.Get_Elevation';
begin
  Result := -1.0;
  try
    Result := FElevation;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDrawDownElevation.Get_AverageElevations: double;
const OPNAME = 'TDrawDownElevation.Get_AverageElevations';
begin
  Result := -1.0;
  try
    Result := FAverageElevations;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFixedElevation.Get_ElevationName: WideString;
const OPNAME = 'TFixedElevation.Get_ElevationName';
begin
  Result := '';
  try
    Result := FElevationName;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFixedElevation.Get_PenaltyValue: double;
const OPNAME = 'TFixedElevation.Get_PenaltyValue';
begin
  Result := NullFloat;
  try
    Result := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.
              ReservoirPenaltyStructureList.ReservoirPenaltyByIdentifier[FPenaltyStructureID].
              ReservoirPenaltyValueByIndex[FPenaltyValueID];
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFixedElevation.PopulateElevationName(AElevationName: string): boolean;
const OPNAME = 'TFixedElevation.PopulateElevationName';
begin
  Result := False;
  try
    FElevationName := AElevationName;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFixedElevation.PopulateReservoirData(AReservoirData: IReservoirData): boolean;
const OPNAME = 'TFixedElevation.PopulateReservoirData';
begin
  Result := False;
  try
    FReservoirID := AReservoirData.ReservoirConfigurationData.ReservoirIdentifier;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFixedElevation.Get_ReservoirData: IReservoirData;
const OPNAME = 'TFixedElevation.Get_ReservoirData';
begin
  Result := nil;
  try
    if(FAppModules.Model.ModelName <> CDDTS) then
        Result := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.ReservoirList.ReservoirByIdentifier[FReservoirID]
      else
        Result := TDDTSDataObject(FAppModules.Model.ModelData).DDTSDamDataList.CastReservoirList.ReservoirByIdentifier[FReservoirID];

   // Result := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.ReservoirList.
   //           ReservoirByIdentifier[FReservoirID];
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFixedElevation.PopulateElevation(AElevation: double): boolean;
const OPNAME = 'TFixedElevation.PopulateElevation';
begin
  Result := False;
  try
    FElevation := AElevation;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFixedElevation.InitialiseNewFixedElevation(AReservoirData: IReservoirData);
const OPNAME = 'TFixedElevation.InitialiseNewFixedElevation';
begin
  try
    FElevation           := 0.0;
    FReservoirID         := AReservoirData.ReservoirConfigurationData.ReservoirIdentifier;
    FPenaltyStructureID  := 0;
    FPenaltyValueID      := 0;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFixedElevation.Validate(var AErrors: WideString; const AContext: WideString): WordBool;
const OPNAME = 'TFixedElevation.Validate';
begin
  Result := False;
  try
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFixedElevation._AddRef: Integer;
const OPNAME = 'TFixedElevation._AddRef';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TFixedElevation._Release: Integer;
const OPNAME = 'TFixedElevation._Release';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TFixedElevation.CopyFrom(AResID: integer; ASource: TFixedElevation): boolean;
const OPNAME = 'TFixedElevation.CopyFrom';
begin
  Result := FALSE;
  try
    FElevation          := ASource.FElevation;
    FReservoirID        := AResID;
    FPenaltyStructureID := ASource.FPenaltyStructureID;
    FPenaltyValueID     := ASource.FPenaltyValueID;
    Result := FALSE;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


function TFixedElevation.RecalculateAreaWhenFull: WordBool;
const OPNAME = 'TFixedElevation.RecalculateAreaWhenFull';
var
  lReservoir     : IReservoirData;
begin
  Result := False;
  try
    if(FElevationName <> 'FullSupplyLevel') then
    begin
      Result := True;
      Exit;
    end;
    lReservoir := ReservoirData;
    if (lReservoir <> nil) then
    begin
      lReservoir.RecalculateAreaWhenFull;
      Result := True;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFixedElevation.UpdateDrawDownLevelFromDSL: WordBool;
const OPNAME = 'TFixedElevation.RecalculateAreaWhenFull';
var
  lReservoir     : IReservoirData;
begin
  Result := False;
  try
    if(FElevationName <> 'DeadStorageLevel') then
    begin
      Result := True;
      Exit;
    end;
    lReservoir := ReservoirData;
    if (lReservoir <> nil) then
    begin
      lReservoir.ReservoirZoneElevationsData.UpdateDrawDownLevelFromDSL;
      Result := True;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFixedElevation.Assign(ASource: TFixedElevation);
const OPNAME = 'TFixedElevation.Assign';
begin
  try
    if (ASource <> nil) then
    begin
      Elevation := ASource.FElevation;
      FElevationName := ASource.FElevationName;
      FPenaltyStructureID := ASource.FPenaltyStructureID;
      FPenaltyValueID := ASource.FPenaltyValueID;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

{ TInitialLevelsData }

procedure TInitialLevelsData.CreateMemberObjects;
const OPNAME = 'TInitialLevelsData.CreateMemberObjects';
var
  LInitialLevels: TAbstractFieldProperty;
begin
  inherited;
  try
    LInitialLevels := FAppModules.FieldProperties.FieldProperty('ResInitialLevelsLev');
    if not assigned(LInitialLevels) then
      raise Exception.Create('Field (ResInitialLevelsLev) not found in field properties');
    SetLength(FInitialLevels, LInitialLevels.ArrayLength);
    Initialise;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TInitialLevelsData.Get_InitialLevelsByIndex(ALevelIndex: integer): double;
const OPNAME = 'TInitialLevelsData.Get_InitialLevelsByIndex';
begin
  Result := -1.0;
  try
    Result := FInitialLevels[ALevelIndex];
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TInitialLevelsData.LoadInitialLevelsFromDataset(ADataSet: TAbstractModelDataset): boolean;
const OPNAME = 'TInitialLevelsData.LoadInitialLevelsFromDataset';
begin
  Result := False;
  try
    FReservoirIdentifier := ADataset.DataSet.FieldByName('ReservoirIdentifier').AsInteger;
    FInitialLevels[1]    := ADataset.DataSet.FieldByName('ResInitialLevelsLev01').AsFloat;
    FInitialLevels[2]    := ADataset.DataSet.FieldByName('ResInitialLevelsLev02').AsFloat;
    FInitialLevels[3]    := ADataset.DataSet.FieldByName('ResInitialLevelsLev03').AsFloat;
    FInitialLevels[4]    := ADataset.DataSet.FieldByName('ResInitialLevelsLev04').AsFloat;
    FInitialLevels[5]    := ADataset.DataSet.FieldByName('ResInitialLevelsLev05').AsFloat;
    FInitialLevels[6]    := ADataset.DataSet.FieldByName('ResInitialLevelsLev06').AsFloat;
    FInitialLevels[7]    := ADataset.DataSet.FieldByName('ResInitialLevelsLev07').AsFloat;
    FInitialLevels[8]    := ADataset.DataSet.FieldByName('ResInitialLevelsLev08').AsFloat;

    Result := True;
  except on E: Exception do HandleErrorFunction(E, OPNAME, Result) end;
end;

function TInitialLevelsData.Initialise: boolean;
const OPNAME = 'TInitialLevelsData.Initialise';
var
  LIndex: integer;
  LInitialLevels: TAbstractFieldProperty;
begin
  Result := False;
  try
    LInitialLevels := FAppModules.FieldProperties.FieldProperty('ResInitialLevelsLev');
    for LIndex := LInitialLevels.ArrayLow to LInitialLevels.ArrayHigh do
      FInitialLevels[LIndex] := 0.0;
    Result := True;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TDrawDownElevation.Set_MonthlyElevationByIndex(AMonthIndex: integer;ANewValue: double);
const OPNAME = 'TDrawDownElevation.Set_MonthlyElevationByIndex';
var
  LLoadAgent: TReservoirDataSQLAgent;
  LContextData: TStringList;
  LFieldName : string;
begin
  try
    LLoadAgent := TReservoirDataSQLAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      LFieldName := Format('%s%2.2d',['ReservoirLev',AMonthIndex]);
      try
        LLoadAgent.LoadDrawDownElevationsContextData(LContextData,
          IntToStr(FReservoirIdentifier), IntToStr(FLevelIdentifier), IntToStr(AMonthIndex));
        if FAppModules.FieldProperties.UpdateFieldValue(
          LFieldName, FloatToStr(ANewValue), FloatToStr(FMonthlyElevations[AMonthIndex]), LContextData) then
        begin
          FMonthlyElevations[AMonthIndex] := ANewValue;
        end;
      finally
        LContextData.Free;
      end;
    finally
      LLoadAgent.Free;
    end;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TInitialLevelsData.Set_InitialLevelsByIndex(ALevelIndex: integer; ANewValue: double);
const OPNAME = 'TInitialLevelsData.Set_InitialLevelsByIndex';
var
  LLoadAgent: TReservoirDataSQLAgent;
  LContextData: TStringList;
begin
  try
    LLoadAgent := TReservoirDataSQLAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        LLoadAgent.LoadInitialLevelsContextData(LContextData,IntToStr(FReservoirIdentifier),IntToStr(ALevelIndex));
        if FAppModules.FieldProperties.UpdateFieldValue(
          'ResInitialLevelsLev', FloatToStr(ANewValue), FloatToStr(FInitialLevels[ALevelIndex]), LContextData) then
        begin
          FInitialLevels[ALevelIndex] := ANewValue;
        end;
      finally
        LContextData.Free;
      end;
    finally
      LLoadAgent.Free;
    end;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TInitialLevelsData.UpdateInitialLevelValue(ALevelIndex: integer; ANewValue: double);
var
  LInitialLevels: TAbstractFieldProperty;
const OPNAME = 'TInitialLevelsData.UpdateInitialLevelValue';
begin
  LInitialLevels := FAppModules.FieldProperties.FieldProperty('ResInitialLevelsLev');
  if(ALevelIndex >= LInitialLevels.ArrayLow) and (ALevelIndex < LInitialLevels.ArrayHigh) then
    FInitialLevels[ALevelIndex] := ANewValue;
end;

function TDrawDownElevation.PopulateReservoirData(AReservoirData: IReservoirData): boolean;
const OPNAME = 'TDrawDownElevation.PopulateReservoirData';
begin
  Result := False;
  try
    FReservoirIdentifier := AReservoirData.ReservoirConfigurationData.ReservoirIdentifier;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TDrawDownElevation.Get_LevelIdentifier: integer;
const OPNAME = 'TDrawDownElevation.Get_LevelIdentifier';
begin
  Result := 0;
  try
    Result := FLevelIdentifier;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TDrawDownElevation.Get_ReservoirData: IReservoirData;
const OPNAME = 'TDrawDownElevation.Get_ReservoirData';
begin
  Result := nil;
  try
    Result := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.ReservoirList.
              ReservoirByIdentifier[FReservoirIdentifier];
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TDrawDownElevation.Get_ReservoirIdentifier: integer;
const OPNAME = 'TDrawDownElevation.Get_ReservoirIdentifier';
begin
  Result := 0;
  try
    Result := FReservoirIdentifier;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TInitialLevelsData.Get_ReservoirIdentifier: integer;
const OPNAME = 'TInitialLevelsData.Get_ReservoirIdentifier';
begin
  Result := 0;
  try
    Result := FReservoirIdentifier;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TInitialLevelsData.DestroyMemberObjects;
const OPNAME = 'TInitialLevelsData.DestroyMemberObjects';
begin
  try
    Finalize(FInitialLevels);
    inherited;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TDrawDownElevation.InitialiseNewDrawDownElevation(ALevelIdentifier: integer;
          AReservoirData: IReservoirData);
const OPNAME = 'TDrawDownElevation.InitialiseNewDrawDownElevation';
var
  LCount: integer;
begin
  try
    for LCount := Low(FMonthlyElevations) to High(FMonthlyElevations) do
      FMonthlyElevations[LCount] := 0.0;

    FLevelIdentifier     := ALevelIdentifier;
    FReservoirIdentifier := AReservoirData.ReservoirConfigurationData.ReservoirIdentifier;
    FAverageElevations   := 0.0;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TInitialLevelsData.InitialiseNewInitialLevels(AReservoirData: IReservoirData);
const OPNAME = 'TInitialLevelsData.InitialiseNewInitialLevels';
var
  LCount: integer;
begin
  try
    FReservoirIdentifier := AReservoirData.ReservoirConfigurationData.ReservoirIdentifier;
    for LCount := Low(FInitialLevels) to High(FInitialLevels) do
      FInitialLevels[LCount] := 0.0;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TDrawDownElevation.CalculateAvarageElevation;
const OPNAME = 'TDrawDownElevation.CalculateAvarageElevation';
var
  LElevationIndex : integer;
  LAverageElevation: double;
begin
  try
    // Calculate the average Elevation.
    LAverageElevation := 0.0;
    for LElevationIndex := 1 to 12 do
      LAverageElevation := LAverageElevation + FMonthlyElevations[LElevationIndex];
    LAverageElevation := (LAverageElevation/12);
    FAverageElevations := LAverageElevation;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TDrawDownElevation.Validate(var AErrors: WideString;
  const AContext: WideString): WordBool;
const OPNAME = 'TDrawDownElevation.Validate';
begin
  Result := False;
  try
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TInitialLevelsData.Validate(var AErrors: WideString;
  const AContext: WideString): WordBool;
const OPNAME = 'TInitialLevelsData.Validate';
begin
  Result := False;
  try
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDrawDownElevation._AddRef: Integer;
const OPNAME = 'TDrawDownElevation._AddRef';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TDrawDownElevation._Release: Integer;
const OPNAME = 'TDrawDownElevation._Release';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TInitialLevelsData._AddRef: Integer;
const OPNAME = 'TInitialLevelsData._AddRef';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TInitialLevelsData._Release: Integer;
const OPNAME = 'TInitialLevelsData._Release';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TInitialLevelsData.CopyFrom(AResID: integer; ASource: TInitialLevelsData): boolean;
const OPNAME = 'TInitialLevelsData.CopyFrom';
var
  LIndex : integer;
begin
  Result := FALSE;
  try
    FReservoirIdentifier := AResID;
    for lIndex := Low(FInitialLevels) to High(FInitialLevels) do
      FInitialLevels[lIndex] := ASource.InitialLevelsByIndex[lIndex];
    Result := FALSE;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDrawDownElevation.GetKeyValues(const AParamField,AFieldIndex: WideString): WideString;
const OPNAME = 'TDrawDownElevation.GetKeyValues';
begin
  Result := '';
  try
    Result := GetDefaultKey;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TInitialLevelsData.GetKeyValues(const AParamField,AFieldIndex: WideString): WideString;
const OPNAME = 'TInitialLevelsData.GetKeyValues';
begin
  Result := '';
  try
    Result := GetDefaultKey;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDrawDownElevation.CopyFrom(AResID: integer;ASource: TDrawDownElevation): boolean;
const OPNAME = 'TDrawDownElevation.CopyFrom';
var
  LIndex : integer;
begin
  Result := FALSE;
  try
    for lIndex := Low(FMonthlyElevations) to High(FMonthlyElevations) do
      FMonthlyElevations[lIndex] := ASource.MonthlyElevationByIndex[lIndex];
    FReservoirIdentifier := AResID;
    FLevelIdentifier := ASource.FLevelIdentifier;
    FAverageElevations := ASource.FAverageElevations;
    Result := FALSE;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


procedure TInitialLevelsData.Assign(ASource: TInitialLevelsData);
const OPNAME = 'TInitialLevelsData.Assign';
var
  LIndex         : integer;
  LInitialLevels : TAbstractFieldProperty;
begin
  try
    if (ASource <> nil) then
    begin
      LInitialLevels := FAppModules.FieldProperties.FieldProperty('ResInitialLevelsLev');
      for LIndex := LInitialLevels.ArrayLow to LInitialLevels.ArrayHigh do
       InitialLevelsByIndex[LIndex] := ASource.FInitialLevels[LIndex];
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

end.
