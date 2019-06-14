//
//
//  UNIT      : Contains TCatchmentZone Class
//  AUTHOR    : Sam Dlamini(ARIVIA)
//  DATE      : 10/01/2007
//  COPYRIGHT : Copyright © 2004 DWAF
//
//

unit UCatchmentZone;

interface
uses
  Classes,
  VCL.Controls,
  Contnrs,
  UUtilities,
  UAbstractObject,
  UYearlyStationData,
  RainfallCom_TLB;

type

  TSourceType = (stRaw, stSplit, stPatchR, stWRC);
  TPercentageOfMAP = array[1..12] of double;
  TDetailOfRainfallStationsUsed = class(TAbstractAppObject)
  protected
    FStationNumber : string;
    FStationID : integer;
    FSection : string;
    FPosition : string;
    FMAPInmm : integer;
    FPeriodOfRecord : string;
    FLatitude : string;
    FLongitude : string;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
  public
    property StationNumber : string read FStationNumber write FStationNumber;
    property StationID : integer read FStationID write FStationID;
    property Section : string read FSection write FSection;
    property Position : string read FPosition write FPosition;
    property MAPInmm : integer read FMAPInmm write FMAPInmm;
    property PeriodOfRecord : string read FPeriodOfRecord write FPeriodOfRecord;
    property Latitude : string read FLatitude write FLatitude;
    property Longitude : string read FLongitude write FLongitude;
  end;

  TRainfallAsPercentMAP = class(TAbstractAppObject)
  protected
    FHydroYear : integer;
    FNoOfGaugesUsed : integer;
    FGaugesUsed : TStringList;
    FAverageMAP : double;
    FHydroYearTotal : double;
    FPercentageOfMAP : TPercentageOfMAP;
    FPercentageOfMAPCount : TPercentageOfMAP;
    function Get_PercentageOfMAPByIndex(AIndex: integer): double;
    procedure Set_PercentageOfMAPByIndex(AIndex: integer; const Value: double);
    function Get_NoOfGaugesUsed : integer;
    function Get_GaugesUsed: string;
    procedure Set_GaugesUsed(const Value: string);
    function Get_PercentageOfMAPCountByIndex(AIndex: integer): double;
    procedure Set_PercentageOfMAPCountByIndex(AIndex: integer;const Value: double);
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
  public
    function Initialise: Boolean; override;
    property HydroYear : integer read FHydroYear write FHydroYear;
    property NoOfGaugesUsed : integer read Get_NoOfGaugesUsed;
    property PercentageOfMAPByIndex[AIndex : integer] : double read Get_PercentageOfMAPByIndex write
             Set_PercentageOfMAPByIndex;
    property PercentageOfMAPCountByIndex[AIndex : integer] : double read Get_PercentageOfMAPCountByIndex write
             Set_PercentageOfMAPCountByIndex;
    property GaugesUsed : string read Get_GaugesUsed write Set_GaugesUsed;
    property AverageMAP : double read FAverageMAP write FAverageMAP;
    property HydroYearTotal : double read FHydroYearTotal write FHydroYearTotal;
  end;

  TCatchmentOutputFileData = class(TAbstractAppObject)
  protected
    FCatchmentDesc : string;
    FCatchmentID : integer;
    FDetailOfRainfallStationsUsed : TObjectList;
    FRainfallAsPercentMAP : TObjectList;
    FTotalPercentageOfMAP : TPercentageOfMAP;
    FAverage : TPercentageOfMAP;
    FAjusted : TPercentageOfMAP;
    FStdDev : TPercentageOfMAP;
    function Get_AjustedByIndex(AIndex: integer): double;
    function Get_AverageByIndex(AIndex: integer): double;
    function Get_StdDevByIndex(AIndex: integer): double;
    procedure Set_AjustedByIndex(AIndex: integer; const Value: double);
    procedure Set_AverageByIndex(AIndex: integer; const Value: double);
    procedure Set_StdDevByIndex(AIndex: integer; const Value: double);
    function Get_TDetailOfRainfallStationsUsedByID(AStationID: integer): TDetailOfRainfallStationsUsed;
    function Get_TDetailOfRainfallStationsUsedByIndex(AIndex: integer): TDetailOfRainfallStationsUsed;
    function Get_RainfallAsPercentMAPCount : integer;
    function Get_DetailOfRainfallStationsUsedCount : integer;
    function Get_RainfallAsPercentMAPByHydroYear(AHydroYear : integer): TRainfallAsPercentMAP;
    function Get_RainfallAsPercentMAPByIndex(AIndex : integer): TRainfallAsPercentMAP;
    function Get_AjustedPercentageOfMAPByMonth(AMonth: integer): double;
    function Get_AveragePercentageOfMAPByMonth(AMonth: integer): double;
    function Get_TotalPercentageOfMAPByMonth(AMonth: integer): double;
    procedure Set_TotalPercentageOfMAPByMonth(AMonth: integer;const Value: double);
    function Get_StdDevByMonth(AMonth: integer): double;
    function Get_GrandAverage: double;
    function Get_GrandSTDDEV: double;
    function Get_GrandAjusted: double;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
  public
    function Initialise: Boolean; override;
    property CatchmentDesc : string read FCatchmentDesc write FCatchmentDesc;
    function AddDetailOfRainfallStationsUsed : TDetailOfRainfallStationsUsed;
    function AddRainfallAsPercentMAP : TRainfallAsPercentMAP;
    property RainfallAsPercentMAPCount : integer read Get_RainfallAsPercentMAPCount;
    property DetailOfRainfallStationsUsedCount : integer read Get_DetailOfRainfallStationsUsedCount;
    property AverageByIndex[AIndex : integer]  : double read Get_AverageByIndex write Set_AverageByIndex;
    property AjustedByIndex[AIndex : integer] : double read Get_AjustedByIndex write Set_AjustedByIndex;
    property StdDevByIndex[AIndex : integer] : double read Get_StdDevByIndex write Set_StdDevByIndex;
    property DetailOfRainfallStationsUsedByID[AStationID : integer] : TDetailOfRainfallStationsUsed read Get_TDetailOfRainfallStationsUsedByID;
    property DetailOfRainfallStationsUsedByIndex[AIndex : integer] : TDetailOfRainfallStationsUsed read Get_TDetailOfRainfallStationsUsedByIndex;
    property RainfallAsPercentMAPByIndex[AIndex : integer] : TRainfallAsPercentMAP read Get_RainfallAsPercentMAPByIndex;
    property RainfallAsPercentMAPByHydroYear[AHydroYear : integer] : TRainfallAsPercentMAP read Get_RainfallAsPercentMAPByHydroYear;
    property CatchmentID : integer read FCatchmentID write FCatchmentID;
    property TotalPercentageOfMAPByMonth[AMonth : integer] : double read Get_TotalPercentageOfMAPByMonth write Set_TotalPercentageOfMAPByMonth;
    property AveragePercentageOfMAPByMonth[AMonth : integer] : double read Get_AveragePercentageOfMAPByMonth;
    property AjustedPercentageOfMAPByMonth[AMonth : integer] : double read Get_AjustedPercentageOfMAPByMonth;
    property StdDevByMonth[AMonth : integer] : double read Get_StdDevByMonth;
    property GrandAverage : double read Get_GrandAverage;
    property GrandSTDDEV : double read Get_GrandSTDDEV;
    property GrandAjusted : double read Get_GrandAjusted;
  end;

  TCatchmentFileData = class(TAbstractAppObject)
  protected
    FCatchmentZone : string;
    FHydroYear : integer;
    FPercentageOfMAP : TPercentageOfMAP;
    function Get_PercentageOfMAPByIndex(AIndex: integer): double;
    procedure Set_PercentageOfMAPByIndex(AIndex: integer;const Value: double);
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
  public
    property CatchmentZone : string read FCatchmentZone write FCatchmentZone;
    property HydroYear : integer read FHydroYear write FHydroYear;
    property PercentageOfMAPByIndex[AIndex : integer] : double read Get_PercentageOfMAPByIndex write
             Set_PercentageOfMAPByIndex;

  end;
  TCatchmentSource = class(TAbstractAppObject)
  protected
    FSourceType : TSourceType;
    FCatchmentID : integer;
    FStationID : integer;
    FSplitIndex : integer;
    FPatchID : integer;
    FStationData : TStationData;
    FPatch       : TPatchData;
    FTotalMAP : double;
    FHydroStartYear : integer;
    FHydroEndYear : integer;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
  public
    property SourceType : TSourceType read FSourceType write FSourceType;
    property CatchmentID : integer read FCatchmentID write FCatchmentID;
    property StationID : integer read FStationID write FStationID;
    property SplitIndex : integer read FSplitIndex write FSplitIndex;
    property PatchID : integer read FPatchID write FPatchID;
    property HydroStartYear : integer read FHydroStartYear write FHydroStartYear;
    property HydroEndYear : integer read FHydroEndYear write FHydroEndYear;
  end;

  TCatchmentZone = class(TAbstractAppObject)
  protected
    FCatchmentID : integer;
    FChangeDate : TDateTime;
    FRunDate : TDateTime;
    FOutputFileName : string;
    FCatchmentFileName : string;
    FCatchmentOutputFileData : TCatchmentOutputFileData;
    FCatchmentFileData : TObjectList;
    FCatchmentSource : TObjectList;
    FCatchmentStartYear : integer;
    FCatchmentEndYear : integer;
    FGrandMAP : double;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    function Get_CatchmentFileDataByIndex(AIndex: integer): TCatchmentFileData;
    function Get_CatchmentFileDataCount: integer;
    function Get_CatchmentSourceCount : integer;
    function GetCatchmentSourceByIndex(AIndex: integer): TCatchmentSource;
    function GetCatchmentSourceByStationID(AStationID: integer): TCatchmentSource;
    function GetCatchmentSourceBySplitStartEndYear(AStationID,AStartYear, AEndYear: integer): TCatchmentSource;
    function GetCatchmentSourceByPatchID(APatchID: integer): TCatchmentSource;
    function GetAverageMAP(AStartYear,AEndYear : integer;ARainfallData : IRainfallData) : integer;
    function ValidateSource(ASourceStationID,ASourcePatchID,ASplitIndex : integer;var AMessage : string) : boolean;
    function ValidateSplit(ASourceStationID,ASplitIndex : integer;var AMessage : string) : boolean;
    function ValidatePatch(ASourceStationID,ASourcePatchID : integer;var AMessage : string) : boolean;
  public
    function Initialise: Boolean; override;
    function Populate(ACatchmentID : integer;AChangeDate : TDateTime;ARunDate : TDateTime;AOutputFileName : string;
                      ACatchmentFileName : string): boolean;
    function AddCatchmentFileData : TCatchmentFileData;
    function AddCatchmentSource : TCatchmentSource;
    function DeleteCatchmentSource(AStationID : integer; APatchID : integer) : boolean;
    function GetCatchmentOutputFileDataByPeriod(var AStartYear : integer; var AEndYear : integer) : TCatchmentOutputFileData;
    function CalcRainfallAsPercentMAP(ARainfallAsPercentMAP : TRainfallAsPercentMAP) : boolean;
    function Validate(ASourceStationID : Integer;ASplitIndex : integer;ASourcePatchID,ALevel:Integer;var AMessage : string) : boolean;
    property CatchmentID : integer read FCatchmentID write FCatchmentID;
    property ChangeDate : TDateTime read FChangeDate write FChangeDate;
    property RunDate : TDateTime read FRunDate write FRunDate;
    property OutputFileName : string read FOutputFileName write FOutputFileName;
    property CatchmentFileName : string read FCatchmentFileName write FCatchmentFileName;
    property CatchmentStartYear : integer read FCatchmentStartYear write FCatchmentStartYear;
    property CatchmentEndYear : integer read FCatchmentEndYear write FCatchmentEndYear;
    property CatchmentOutputFileData : TCatchmentOutputFileData read FCatchmentOutputFileData;
    property CatchmentFileDataByIndex[AIndex : integer] : TCatchmentFileData read Get_CatchmentFileDataByIndex;
    property CatchmentFileDataCount : integer read Get_CatchmentFileDataCount;
    property CatchmentSourceCount : integer read Get_CatchmentSourceCount;
    property CatchmentSourceByIndex[AIndex : integer] : TCatchmentSource read GetCatchmentSourceByIndex;
    property CatchmentSourceByStationID[AIndex : integer] : TCatchmentSource read GetCatchmentSourceByStationID;
    property GrandMAP : double read FGrandMAP write FGrandMAP;
end;

implementation
uses
  System.Types,
  DB,
  Math,
  DateUtils,
  VCL.Dialogs,
  SysUtils,
  UConstants,
  UDataSetType,
  //ZLibEx,
  System.ZLib,
  UErrorHandlingOperations;


{ TRainfallAsPercentMAP }

procedure TRainfallAsPercentMAP.CreateMemberObjects;
const OPNAME = 'TRainfallAsPercentMAP.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
    FGaugesUsed := TStringList.Create;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TRainfallAsPercentMAP.DestroyMemberObjects;
const OPNAME = 'TRainfallAsPercentMAP.DestroyMemberObjects';
begin
  try
    FreeAndNil(FGaugesUsed);
    inherited DestroyMemberObjects;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TRainfallAsPercentMAP.Get_GaugesUsed: string;
const OPNAME = 'TRainfallAsPercentMAP.Get_GaugesUsed';
begin
  Result := '';
  try
    Result := FGaugesUsed.CommaText;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TRainfallAsPercentMAP.Get_NoOfGaugesUsed: integer;
const OPNAME = 'TRainfallAsPercentMAP.Get_NoOfGaugesUsed';
begin
  Result := 0;
  try
    Result := FGaugesUsed.Count;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TRainfallAsPercentMAP.Get_PercentageOfMAPByIndex(AIndex: integer): double;
const OPNAME = 'TRainfallAsPercentMAP.Get_PercentageOfMAPByIndex';
begin
  Result := 0;
  try
    Result := FPercentageOfMAP[AIndex];
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TRainfallAsPercentMAP.Get_PercentageOfMAPCountByIndex(AIndex: integer): double;
const OPNAME = 'TRainfallAsPercentMAP.Get_PercentageOfMAPCountByIndex';
begin
  Result := 0.0;
  try
    Result := FPercentageOfMAPCount[AIndex];
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TRainfallAsPercentMAP.Initialise: Boolean;
const OPNAME = 'TRainfallAsPercentMAP.Initialise';
var
  LIndex : integer;
begin
  Result := False;
  try
    FHydroYear := 0;
    FNoOfGaugesUsed := 0;
    for LIndex := 1 to 12 do
    begin
      FPercentageOfMAP[LIndex] := 0;
      FPercentageOfMAPCount[LIndex] := 0;
    end;
    FGaugesUsed.Clear;
    FAverageMAP := 0;  
    Result := True;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TRainfallAsPercentMAP.Set_GaugesUsed(const Value: string);
const OPNAME = 'TRainfallAsPercentMAP.Set_GaugesUsed';
begin
  try
    FGaugesUsed.CommaText := Value;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TRainfallAsPercentMAP.Set_PercentageOfMAPByIndex(AIndex: integer;const Value: double);
const OPNAME = 'TRainfallAsPercentMAP.Set_PercentageOfMAPByIndex';
begin
  try
    FPercentageOfMAP[AIndex] := Value;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TRainfallAsPercentMAP.Set_PercentageOfMAPCountByIndex(AIndex: integer; const Value: double);
const OPNAME = 'TRainfallAsPercentMAP.Set_PercentageOfMAPCountByIndex';
begin
  try
    FPercentageOfMAPCount[AIndex] := Value;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

{ TCatchmentOutputFileData }

procedure TCatchmentOutputFileData.CreateMemberObjects;
const OPNAME = 'TCatchmentOutputFileData.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
    FDetailOfRainfallStationsUsed := TObjectList.Create;
    FRainfallAsPercentMAP := TObjectList.Create;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TCatchmentOutputFileData.DestroyMemberObjects;
const OPNAME = 'TCatchmentOutputFileData.DestroyMemberObjects';
begin
  try
    FreeAndNil(FDetailOfRainfallStationsUsed);
    FreeAndNil(FRainfallAsPercentMAP);
    inherited CreateMemberObjects;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TCatchmentOutputFileData.AddDetailOfRainfallStationsUsed: TDetailOfRainfallStationsUsed;
const OPNAME = 'TCatchmentOutputFileData.AddDetailOfRainfallStationsUsed';
begin
  Result := nil;
  try
    Result := TDetailOfRainfallStationsUsed.Create(FAppModules);
    FDetailOfRainfallStationsUsed.Add(Result);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TCatchmentOutputFileData.AddRainfallAsPercentMAP: TRainfallAsPercentMAP;
const OPNAME = 'TCatchmentOutputFileData.AddRainfallAsPercentMAP';
begin
  Result := nil;
  try
    Result := TRainfallAsPercentMAP.Create(FAppModules);
    FRainfallAsPercentMAP.Add(Result);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TCatchmentOutputFileData.Get_AjustedByIndex(AIndex: integer): double;
const OPNAME = 'TCatchmentOutputFileData.Get_AjustedByIndex';
begin
  Result := 0.0;
  try
    Result := FAjusted[AIndex];
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TCatchmentOutputFileData.Get_AverageByIndex(AIndex: integer): double;
const OPNAME = 'TCatchmentOutputFileData.Get_AverageByIndex';
begin
  Result := 0.0;
  try
    Result := FAverage[AIndex];
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TCatchmentOutputFileData.Get_StdDevByIndex(AIndex: integer): double;
const OPNAME = 'TCatchmentOutputFileData.Get_StdDevByIndex';
begin
  Result := 0.0;
  try
    Result := FStdDev[AIndex];
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TCatchmentOutputFileData.Set_AjustedByIndex(AIndex: integer;const Value: double);
const OPNAME = 'TCatchmentOutputFileData.Set_AjustedByIndex';
begin
  try
    FAjusted[AIndex] := Value;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TCatchmentOutputFileData.Set_AverageByIndex(AIndex: integer;const Value: double);
const OPNAME = 'TCatchmentOutputFileData.Set_AverageByIndex';
begin
  try
    FAverage[AIndex] := Value;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TCatchmentOutputFileData.Set_StdDevByIndex(AIndex: integer;const Value: double);
const OPNAME = 'TCatchmentOutputFileData.Set_StdDevByIndex';
begin
  try
    FStdDev[AIndex] := Value;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TCatchmentOutputFileData.Get_TDetailOfRainfallStationsUsedByID(AStationID: integer): TDetailOfRainfallStationsUsed;
const OPNAME = 'TCatchmentOutputFileData.Get_TDetailOfRainfallStationsUsedByID';
var
  LIndex : integer;
begin
  Result := nil;
  try
    for LIndex := 0 to FDetailOfRainfallStationsUsed.Count-1 do
    begin
      if TDetailOfRainfallStationsUsed(FDetailOfRainfallStationsUsed.Items[LIndex]).StationID = AStationID then
      begin
        Result := TDetailOfRainfallStationsUsed(FDetailOfRainfallStationsUsed.Items[LIndex]);
        Break;
      end;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TCatchmentOutputFileData.Get_TDetailOfRainfallStationsUsedByIndex(AIndex: integer): TDetailOfRainfallStationsUsed;
const OPNAME = 'TCatchmentOutputFileData.Get_TDetailOfRainfallStationsUsedByIndex';
begin
  Result := nil;
  try
    if (AIndex >= 0) and (AIndex <= FDetailOfRainfallStationsUsed.Count-1) then
      Result := TDetailOfRainfallStationsUsed(FDetailOfRainfallStationsUsed.Items[AIndex]);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TCatchmentOutputFileData.Get_RainfallAsPercentMAPCount: integer;
const OPNAME = 'TCatchmentOutputFileData.Get_RainfallAsPercentMAPCount';
begin
  Result := 0;
  try
    Result := FRainfallAsPercentMAP.Count;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TCatchmentOutputFileData.Get_DetailOfRainfallStationsUsedCount: integer;
const OPNAME = 'TCatchmentOutputFileData.Get_DetailOfRainfallStationsUsedCount';
begin
  Result := 0;
  try
    Result := FDetailOfRainfallStationsUsed.Count;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TCatchmentOutputFileData.Get_RainfallAsPercentMAPByHydroYear(AHydroYear : integer): TRainfallAsPercentMAP;
const OPNAME = 'TCatchmentOutputFileData.Get_RainfallAsPercentMAPByHydroYear';
var
  LIndex : integer;
begin
  Result := nil;
  try
    for LIndex := 0 to FRainfallAsPercentMAP.Count-1 do
    begin
      if (TRainfallAsPercentMAP(FRainfallAsPercentMAP.Items[LIndex]).HydroYear = AHydroYear) then
      begin
        Result := TRainfallAsPercentMAP(FRainfallAsPercentMAP.Items[LIndex]);
        Break;
      end;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TCatchmentOutputFileData.Get_RainfallAsPercentMAPByIndex(AIndex : integer): TRainfallAsPercentMAP;
const OPNAME = 'TCatchmentOutputFileData.Get_RainfallAsPercentMAPByIndex';
begin
  Result := nil;
  try
    if (AIndex >= 0) and (AIndex <= FRainfallAsPercentMAP.Count-1) then
      Result := TRainfallAsPercentMAP(FRainfallAsPercentMAP.Items[AIndex])
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TCatchmentOutputFileData.Initialise: Boolean;
const OPNAME = 'TCatchmentOutputFileData.Initialise';
var
  LIndex : integer;
begin
  Result := False;
  try
    FCatchmentDesc := '';
    FDetailOfRainfallStationsUsed.Clear;
    FRainfallAsPercentMAP.Clear;
    for LIndex := 1 to 12 do
    begin
      FTotalPercentageOfMAP[LIndex] := 0;
      FAverage[LIndex] := 0;
      FAjusted[LIndex] := 0;
      FStdDev[LIndex] := 0;
    end;
    Result := True;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TCatchmentOutputFileData.Get_AjustedPercentageOfMAPByMonth(AMonth: integer): double;
const OPNAME = 'TCatchmentOutputFileData.Get_AjustedPercentageOfMAPByMonth';
begin
  Result := 0.0;
  try
    if (GrandAverage > 0 ) then
      Result := (AveragePercentageOfMAPByMonth[AMonth]/GrandAverage)*100;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TCatchmentOutputFileData.Get_AveragePercentageOfMAPByMonth(AMonth: integer): double;
const OPNAME = 'TCatchmentOutputFileData.Get_AveragePercentageOfMAPByMonth';
var
  LIndex : integer;
  LMonthAvrg : double;
begin
  Result := 0.0;
  try
    LMonthAvrg := 0;
    for LIndex := 0 to RainfallAsPercentMAPCount-1 do
      LMonthAvrg := LMonthAvrg + TRainfallAsPercentMAP(FRainfallAsPercentMAP.Items[LIndex]).PercentageOfMAPByIndex[AMonth];
    Result := (LMonthAvrg/RainfallAsPercentMAPCount);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TCatchmentOutputFileData.Get_TotalPercentageOfMAPByMonth(AMonth: integer): double;
const OPNAME = 'TCatchmentOutputFileData.Get_TotalPercentageOfMAPByMonth';
begin
  Result := 0.0;
  try
    Result := FTotalPercentageOfMAP[AMonth];
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TCatchmentOutputFileData.Set_TotalPercentageOfMAPByMonth(AMonth: integer; const Value: double);
const OPNAME = 'TCatchmentOutputFileData.Set_TotalPercentageOfMAPByMonth';
begin
  try
    FTotalPercentageOfMAP[AMonth] := FTotalPercentageOfMAP[AMonth] + Value;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TCatchmentOutputFileData.Get_StdDevByMonth(AMonth: integer): double;
const OPNAME = 'TCatchmentOutputFileData.Get_StdDevByMonth';
var
  LIndex : integer;
  LTotal : double;
begin
  Result := 0.0;
  try
    LTotal := 0;
    for LIndex := 0 to RainfallAsPercentMAPCount-1 do
      LTotal := LTotal + power(AveragePercentageOfMAPByMonth[AMonth]-TRainfallAsPercentMAP(FRainfallAsPercentMAP.Items[LIndex]).PercentageOfMAPByIndex[AMonth],2);
    if ((RainfallAsPercentMAPCount-1) = 1) or ((RainfallAsPercentMAPCount-1) = 0) then
      Result := Sqrt(LTotal/(RainfallAsPercentMAPCount))
    else
    if (RainfallAsPercentMAPCount-1) > 1 then
      Result := Sqrt(LTotal/(RainfallAsPercentMAPCount))
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TCatchmentOutputFileData.Get_GrandAverage: double;
const OPNAME = 'TCatchmentOutputFileData.Get_GrandAverage';
var
  LMonth : integer;
begin
  Result := 0;
  try
    for LMonth := MinMonths to MaxMonths do
      Result := Result + AveragePercentageOfMAPByMonth[LMonth];
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TCatchmentOutputFileData.Get_GrandSTDDEV: double;
const OPNAME = 'TCatchmentOutputFileData.Get_GrandSTDDEV';
var
  LIndex : integer;
  LTotal : double;
begin
  Result := 0.0;
  try
    LTotal := 0;
    for LIndex := 0 to RainfallAsPercentMAPCount-1 do
      LTotal := LTotal + power(GrandAverage-TRainfallAsPercentMAP(FRainfallAsPercentMAP.Items[LIndex]).HydroYearTotal,2);
    if ((RainfallAsPercentMAPCount-1) = 1) or ((RainfallAsPercentMAPCount-1) = 0) then
      Result := Sqrt(LTotal/(RainfallAsPercentMAPCount))
    else
    if (RainfallAsPercentMAPCount-1) > 1 then
      Result := Sqrt(LTotal/(RainfallAsPercentMAPCount))
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;


function TCatchmentOutputFileData.Get_GrandAjusted: double;
const OPNAME = 'TCatchmentOutputFileData.Get_GrandAjusted';
var
  LMonth : integer;
begin
  Result := 0;
  try
    for LMonth := MinMonths to MaxMonths do
      Result := Result + AjustedPercentageOfMAPByMonth[LMonth];
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

{ TDetailOfRainfallStationsUsed }

procedure TDetailOfRainfallStationsUsed.CreateMemberObjects;
const OPNAME = 'TDetailOfRainfallStationsUsed.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TDetailOfRainfallStationsUsed.DestroyMemberObjects;
const OPNAME = 'TDetailOfRainfallStationsUsed.DestroyMemberObjects';
begin
  try
    inherited CreateMemberObjects;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

{ TCatchmentFileData }

procedure TCatchmentFileData.CreateMemberObjects;
const OPNAME = 'TCatchmentFileData.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TCatchmentFileData.DestroyMemberObjects;
const OPNAME = 'TCatchmentFileData.DestroyMemberObjects';
begin
  try
    inherited CreateMemberObjects;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TCatchmentFileData.Get_PercentageOfMAPByIndex(AIndex: integer): double;
const OPNAME = 'TCatchmentFileData.Get_PercentageOfMAPByIndex';
begin
  Result := 0.0;
  try
    Result := FPercentageOfMAP[AIndex];
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TCatchmentFileData.Set_PercentageOfMAPByIndex(AIndex: integer;const Value: double);
const OPNAME = 'TCatchmentFileData.Set_PercentageOfMAPByIndex';
begin
  try
    FPercentageOfMAP[AIndex] := Value;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

{ TCatchmentZone }

function TCatchmentZone.Populate(ACatchmentID : integer;AChangeDate : TDateTime;ARunDate : TDateTime;AOutputFileName : string;
                                 ACatchmentFileName : string): boolean;
const OPNAME = 'TCatchmentZone.Populate';                                 
begin
  Result := False;
  try
    FCatchmentID := ACatchmentID;
    FChangeDate := AChangeDate;
    FRunDate := ARunDate;
    FOutputFileName := AOutputFileName;
    FCatchmentFileName := ACatchmentFileName;
    Result := True;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TCatchmentZone.AddCatchmentFileData: TCatchmentFileData;
const OPNAME = 'TCatchmentZone.AddCatchmentFileData';
begin
  Result := nil;
  try
    Result := TCatchmentFileData.Create(FAppModules);
    FCatchmentFileData.Add(Result);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

{function TCatchmentZone.AddCatchmentOutputFileData: TCatchmentOutputFileData;
const OPNAME = 'TCatchmentZone.AddCatchmentOutputFileData';
begin
  Result := nil;
  try
    Result := TCatchmentOutputFileData.Create(FAppModules);
    FCatchmentOutputFileData.Add(Result);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;
 }
function TCatchmentZone.AddCatchmentSource: TCatchmentSource;
const OPNAME = 'TCatchmentZone.AddCatchmentSource';
begin
  Result := nil;
  try
    Result := TCatchmentSource.Create(FAppModules);
    FCatchmentSource.Add(Result);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TCatchmentZone.DeleteCatchmentSource(AStationID : integer; APatchID : integer) : boolean;
const OPNAME = 'TCatchmentZone.DeleteCatchmentSource';
var
  LIndex : integer;
begin
  Result := False;
  try
    for LIndex := 0 to FCatchmentSource.Count -1 do
    begin
      if (TCatchmentSource(FCatchmentSource.Items[LIndex]).StationID = AStationID) and
        (TCatchmentSource(FCatchmentSource.Items[LIndex]).PatchID = APatchID) then
      begin
        FCatchmentSource.Remove(TCatchmentSource(FCatchmentSource.Items[LIndex]));
        Break;
      end;
    end;
    Result := True;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TCatchmentZone.GetCatchmentOutputFileDataByPeriod(var AStartYear : integer; var AEndYear : integer) : TCatchmentOutputFileData;
const OPNAME = 'TCatchmentZone.GetCatchmentOutputFileDataByPeriod';
var
  LIndex : integer;
  LCatchmentSource : TCatchmentSource;
  LStation : IStationData;
  LSplit : IRainfallDataSplit;
  LPatch : IPatchData;
  LLatStr : WideString;
  LLonStr : WideString;
  LCatchmentOutputFileData : TCatchmentOutputFileData;
  LDetailOfRainfallStationsUsed : TDetailOfRainfallStationsUsed;
begin
  Result := nil;
  try
    AStartYear := CatchmentStartYear;
    AEndYear := CatchmentEndYear;
    LCatchmentOutputFileData := FCatchmentOutputFileData;
    if not (LCatchmentOutputFileData.Initialise) then
      Exit;
    LCatchmentOutputFileData.FCatchmentDesc := 'AVERAGE RAINFALL ON CATCHMENT OF GAUGE ' + FCatchmentFileName;
    LCatchmentOutputFileData.CatchmentID := FCatchmentID;
    for LIndex := 0 to FCatchmentSource.Count -1 do
    begin
      LCatchmentSource := GetCatchmentSourceByIndex(LIndex);
      if LCatchmentSource <> nil then
      begin
        LStation := (FAppModules.Model.ModelData as IRainfallModelData).GetStationDataByID(LCatchmentSource.FStationID);
        LStation.LatLong(LLatStr, lLonStr);
        if LStation <> nil then
        begin
          LPatch := LStation.GetPatchWithID(LCatchmentSource.FPatchID);
          LSplit := LStation.GetSplitForYears(LCatchmentSource.FHydroStartYear,LCatchmentSource.FHydroEndYear);
          if (CatchmentStartYear = 0) or (CatchmentEndYear = 0) then
          begin
            if (LCatchmentSource.PatchID = 0) and (LSplit <> nil) then
            begin
              if (LSplit.HydroStartYear < AStartYear) or (AStartYear = 0) then
                AStartYear := LSplit.HydroStartYear;
              if (LSplit.HydroEndYear > AEndYear) then
                AEndYear := LSplit.HydroEndYear;
            end;
            if LCatchmentSource.PatchID > 0 then
            begin
              if (LPatch <> nil) then
              begin
                if (LPatch.RainfallData.StartYear < AStartYear) or (AStartYear = 0) then
                  AStartYear := LPatch.RainfallData.StartYear;//LPatch.PatchStartYear;
                if (LPatch.RainfallData.EndYear > AEndYear) or (AEndYear = 0) then
                begin
                  if LPatch.PatchTypeID = 1 then
                    AEndYear := LPatch.RainfallData.EndYear
                  else
                  if LPatch.PatchTypeID = 2 then
                    AEndYear := LPatch.RainfallData.HydroEndYear;
                end;
              end;
            end;
          end;
          LDetailOfRainfallStationsUsed := LCatchmentOutputFileData.AddDetailOfRainfallStationsUsed;
          LDetailOfRainfallStationsUsed.StationID := LCatchmentSource.StationID;
          LDetailOfRainfallStationsUsed.Latitude := LLatStr;
          LDetailOfRainfallStationsUsed.Longitude := LLonStr;
          if LCatchmentSource.PatchID > 0 then
          begin
            LDetailOfRainfallStationsUsed.Section := Copy(LStation.RainfallData.StationNumber, 2, 3);
            LDetailOfRainfallStationsUsed.Position:= Copy(LStation.RainfallData.StationNumber, 5, 3);

            if LPatch.PatchTypeID = 1 then
            begin
              LDetailOfRainfallStationsUsed.MAPInmm := Trunc(GetAverageMAP(LPatch.RainfallData.StartYear,LPatch.RainfallData.EndYear,LPatch.RainfallData));
              LDetailOfRainfallStationsUsed.PeriodOfRecord := IntToStr(LPatch.RainfallData.StartYear)+' TO '+IntToStr(LPatch.RainfallData.EndYear);
            end
            else
            if LPatch.PatchTypeID = 2 then
            begin
              LDetailOfRainfallStationsUsed.MAPInmm := Trunc(GetAverageMAP(LPatch.RainfallData.StartYear,LPatch.RainfallData.HydroEndYear,LPatch.RainfallData));
              LDetailOfRainfallStationsUsed.PeriodOfRecord := IntToStr(LPatch.RainfallData.StartYear)+' TO '+IntToStr(LPatch.RainfallData.HydroEndYear);
            end;


            FGrandMAP := FGrandMAP + LPatch.RainfallData.MAP;
          end
          else
          if (LCatchmentSource.PatchID = 0) and (LSplit <> nil) then
          begin
            LDetailOfRainfallStationsUsed.Section := Copy(LStation.RainfallData.StationNumber, 2, 3);
            LDetailOfRainfallStationsUsed.Position:= Copy(LStation.RainfallData.StationNumber, 5, 3);
            LDetailOfRainfallStationsUsed.MAPInmm := Trunc(GetAverageMAP(LSplit.HydroStartYear,LSplit.HydroEndYear,LStation.RainfallData));
            LDetailOfRainfallStationsUsed.PeriodOfRecord := IntToStr(LSplit.HydroStartYear)+' TO '+IntToStr(LSplit.HydroEndYear);
            FGrandMAP := FGrandMAP + LStation.RainfallData.MAP;
          end;
        end;
      end;
    end;
    Result := LCatchmentOutputFileData;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TCatchmentZone.CalcRainfallAsPercentMAP(ARainfallAsPercentMAP : TRainfallAsPercentMAP) : boolean;
const OPNAME = 'TCatchmentZone.CalcRainfallAsPercentMAP';
var
  LIndex : integer;
  LStation : IStationData;
  LSplit : IRainfallDataSplit;
  LPatch : IPatchData;
  LHydroYearlyData : IYearlyData;
  LCatchmentSource : TCatchmentSource;
  LMonth : integer;
  LGaugesUsed : TStringList;
  LHydroIndex : integer;
  LAverageMAP : integer;
  LValue : double;
begin
  Result := False;
  try
    if (ARainfallAsPercentMAP <>  nil) then
    begin
      LGaugesUsed := TStringList.Create;
      try
        for LIndex := 0 to FCatchmentSource.Count-1 do
        begin
          LCatchmentSource := GetCatchmentSourceByIndex(LIndex);
          if LCatchmentSource <> nil then
          begin
            LStation := (FAppModules.Model.ModelData as IRainfallModelData).GetStationDataByID(LCatchmentSource.FStationID);
            if (LStation <> nil) then
            begin
              LSplit :=  LStation.GetSplitForYears(LCatchmentSource.FHydroStartYear,LCatchmentSource.FHydroEndYear);
              if (LCatchmentSource.PatchID = 0) and (LSplit <> nil) then
              begin
                LHydroYearlyData := nil;
                for LHydroIndex := 0 to LStation.RainfallData.HydroYearsCount-1 do
                begin
                  if (ARainfallAsPercentMAP.HydroYear = LStation.RainfallData.GetHydroYearDataByIndex(LHydroIndex).Year) then
                  begin
                    if (LStation.RainfallData.GetHydroYearDataByIndex(LHydroIndex).Year >= LSplit.HydroStartYear) and
                      (LStation.RainfallData.GetHydroYearDataByIndex(LHydroIndex).Year <= LSplit.HydroEndYear) then
                    begin
                      LHydroYearlyData := LStation.RainfallData.GetHydroYearDataByIndex(LHydroIndex);
                      Break;
                    end;
                  end;
                end;
                if LHydroYearlyData <> nil then
                begin
                  LGaugesUsed.Add(IntToStr(LCatchmentSource.StationID)+'_'+IntToStr(LCatchmentSource.SplitIndex) +
                                                      '_'+IntToStr(LCatchmentSource.PatchID));
                  ARainfallAsPercentMAP.GaugesUsed := LGaugesUsed.CommaText;
                  LAverageMAP :=  Trunc(GetAverageMAP(LSplit.HydroStartYear,LSplit.HydroEndYear,LStation.RainfallData));
                  ARainfallAsPercentMAP.AverageMAP := ARainfallAsPercentMAP.AverageMAP + LAverageMAP;
                  for LMonth := 1 to 12 do
                  begin
                    if LHydroYearlyData <> nil then
                    begin
                      if (LHydroYearlyData.MonthlyRainfall[LMonth] >= 0) then
                        LValue := ((LHydroYearlyData.MonthlyRainfall[LMonth])/LAverageMAP)
                      else
                        LValue := 0;
                      ARainfallAsPercentMAP.PercentageOfMAPByIndex[LMonth] :=
                      ARainfallAsPercentMAP.PercentageOfMAPByIndex[LMonth] + LValue;
                      ARainfallAsPercentMAP.PercentageOfMAPCountByIndex[LMonth] :=
                      ARainfallAsPercentMAP.PercentageOfMAPCountByIndex[LMonth] + 1;
                    end;
                  end;
                end;
              end
              else
              if LCatchmentSource.PatchID > 0 then
              begin
                LPatch := LStation.GetPatchWithID(LCatchmentSource.FPatchID);
                if (LPatch <> nil) then
                begin
                  LHydroYearlyData := nil;
                  for LHydroIndex := 0 to LPatch.RainfallData.HydroYearsCount-1 do
                  begin
                    if (ARainfallAsPercentMAP.HydroYear = LPatch.RainfallData.GetHydroYearDataByIndex(LHydroIndex).Year) then
                    begin
                      LHydroYearlyData := LPatch.RainfallData.GetHydroYearDataByIndex(LHydroIndex);
                      Break;
                    end;
                  end;
                  if LHydroYearlyData <> nil then
                  begin
                    LGaugesUsed.Add(IntToStr(LCatchmentSource.StationID)+'_'+IntToStr(LCatchmentSource.SplitIndex) +
                                             '_'+IntToStr(LCatchmentSource.PatchID));
                    ARainfallAsPercentMAP.GaugesUsed := LGaugesUsed.CommaText;
                    LAverageMAP :=  Trunc(GetAverageMAP(LPatch.RainfallData.StartYear,LPatch.RainfallData.EndYear, LPatch.RainfallData));
                    ARainfallAsPercentMAP.AverageMAP := ARainfallAsPercentMAP.AverageMAP + LAverageMAP;
                    for LMonth := 1 to 12 do
                    begin
                      if LHydroYearlyData <> nil then
                      begin
                        if (LHydroYearlyData.MonthlyRainfall[LMonth] >= 0) then

                          LValue := (LHydroYearlyData.MonthlyRainfall[LMonth])/LAverageMAP
                        else
                          LValue := 0;
                        ARainfallAsPercentMAP.PercentageOfMAPByIndex[LMonth] :=
                        ARainfallAsPercentMAP.PercentageOfMAPByIndex[LMonth] + LValue;
                        ARainfallAsPercentMAP.PercentageOfMAPCountByIndex[LMonth] :=
                        ARainfallAsPercentMAP.PercentageOfMAPCountByIndex[LMonth] + 1;
                      end;
                    end;
                  end
                end;
              end;
            end;
          end;
        end;
        for LMonth := 1 to 12 do
        begin
          if (ARainfallAsPercentMAP.AverageMAP > 0) then
          begin
            LValue := ((ARainfallAsPercentMAP.PercentageOfMAPByIndex[LMonth]/ARainfallAsPercentMAP.NoOfGaugesUsed)*100)*100;
            LValue := Trunc(LValue);
            ARainfallAsPercentMAP.PercentageOfMAPByIndex[LMonth] := StrToFloat(FormatFloat('##0.00',LValue/100));
            ARainfallAsPercentMAP.HydroYearTotal := ARainfallAsPercentMAP.HydroYearTotal + ARainfallAsPercentMAP.PercentageOfMAPByIndex[LMonth];
            FCatchmentOutputFileData.TotalPercentageOfMAPByMonth[LMonth] := ARainfallAsPercentMAP.PercentageOfMAPByIndex[LMonth];
          end;
        end;
      finally
        FreeAndNil(LGaugesUsed)
      end;
    end;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TCatchmentZone.GetAverageMAP(AStartYear,AEndYear : integer;ARainfallData : IRainfallData) : integer;
const OPNAME = 'TCatchmentZone.GetAverageMAP';
var
  LYearIndex : integer;
  LYearlyData : IYearlyData;
  LYearCount : integer;
  LGrandTotal : integer;
begin
  Result := 0;
  try
    if (ARainfallData <> nil) then
    begin
      LYearCount := 0;
      LGrandTotal := 0;
      for LYearIndex := 0 to ARainfallData.HydroYearsCount - 1 do
      begin
        LYearlyData := ARainfallData.GetHydroYearDataByIndex(LYearIndex);
        if ((LYearlyData.Year >=  AStartYear) and (LYearlyData.Year <= AEndYear)) then
        begin
          LYearCount := LYearCount + 1;
          LGrandTotal := LGrandTotal + Round(LYearlyData.Total*10);
        end;
      end;
      if (LYearCount > 0) then
        Result := Trunc((LGrandTotal/LYearCount)/10);
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TCatchmentZone.CreateMemberObjects;
const OPNAME = 'TCatchmentZone.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
    FCatchmentOutputFileData := TCatchmentOutputFileData.Create(FAppModules);
    FCatchmentFileData := TObjectList.Create;
    FCatchmentSource := TObjectList.Create;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TCatchmentZone.DestroyMemberObjects;
const OPNAME = 'TCatchmentZone.DestroyMemberObjects';
begin
  try
    FreeAndNil(FCatchmentOutputFileData);
    FreeAndNil(FCatchmentFileData);
    FreeAndNil(FCatchmentSource);
    inherited CreateMemberObjects;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TCatchmentZone.Get_CatchmentFileDataByIndex(AIndex: integer): TCatchmentFileData;
const OPNAME = 'TCatchmentZone.Get_CatchmentFileDataByIndex';
begin
  Result := nil;
  try
    if (AIndex >=0) and (AIndex < FCatchmentFileData.Count) then
      Result := TCatchmentFileData(FCatchmentFileData.Items[AIndex]);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TCatchmentZone.Get_CatchmentFileDataCount: integer;
const OPNAME = 'TCatchmentZone.Get_CatchmentFileDataCount';
begin
  Result := 0;
  try
    Result := FCatchmentFileData.Count;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TCatchmentZone.Get_CatchmentSourceCount : integer;
const OPNAME = 'TCatchmentZone.Get_CatchmentSourceCount';
begin
  Result := 0;
  try
    Result := FCatchmentSource.Count;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

{function TCatchmentZone.Get_CatchmentOutputFileDataByIndex(AIndex: integer): TCatchmentOutputFileData;
const OPNAME = 'TCatchmentZone.Get_CatchmentOutputFileDataByIndex';
begin
  Result := nil;
  try
    if (AIndex >=0) and (AIndex < FCatchmentOutputFileData.Count) then
      Result := TCatchmentOutputFileData(FCatchmentOutputFileData.Items[AIndex]);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TCatchmentZone.Get_CatchmentOutputFileDataCount: integer;
const OPNAME = 'TCatchmentZone.Get_CatchmentOutputFileDataCount';
begin
  Result := 0;
  try
    Result := FCatchmentOutputFileData.Count;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;
}
function TCatchmentZone.GetCatchmentSourceByIndex(AIndex: integer): TCatchmentSource;
const OPNAME = 'TCatchmentZone.GetCatchmentSourceByIndex';
begin
  Result := nil;
  try
    if (AIndex >= 0) and (AIndex <= FCatchmentSource.Count-1) then
      Result := TCatchmentSource(FCatchmentSource.Items[AIndex])
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TCatchmentZone.GetCatchmentSourceByStationID(AStationID: integer): TCatchmentSource;
const OPNAME = 'TCatchmentZone.GetCatchmentSourceByStationID';
var
  LIndex : integer;
begin
  Result := nil;
  try
    for LIndex := 0 to FCatchmentSource.Count-1 do
    begin
      if TCatchmentSource(FCatchmentSource.Items[LIndex]).StationID = AStationID then
      begin
        Result := TCatchmentSource(FCatchmentSource.Items[LIndex]);
        Break;
      end;

    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TCatchmentZone.GetCatchmentSourceBySplitStartEndYear(AStationID,AStartYear, AEndYear: integer): TCatchmentSource;
const OPNAME = 'TCatchmentZone.GetCatchmentSourceBySplitStartEndYear';
var
  LIndex : integer;
begin
  Result := nil;
  try
    for LIndex := 0 to FCatchmentSource.Count-1 do
    begin
      if (TCatchmentSource(FCatchmentSource.Items[LIndex]).HydroStartYear = AStartYear) and
         (TCatchmentSource(FCatchmentSource.Items[LIndex]).HydroEndYear = AEndYear) and
         (TCatchmentSource(FCatchmentSource.Items[LIndex]).StationID = AStationID) then
      begin
        Result := TCatchmentSource(FCatchmentSource.Items[LIndex]);
        Break;
      end;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TCatchmentZone.GetCatchmentSourceByPatchID(APatchID: integer): TCatchmentSource;
const OPNAME = 'TCatchmentZone.GetCatchmentSourceByPatchID';
var
  LIndex : integer;
begin
  Result := nil;
  try
    for LIndex := 0 to FCatchmentSource.Count-1 do
    begin
      if (TCatchmentSource(FCatchmentSource.Items[LIndex]).PatchID = APatchID) then
      begin
        Result := TCatchmentSource(FCatchmentSource.Items[LIndex]);
        Break;
      end;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TCatchmentZone.Initialise: Boolean;
const OPNAME = 'TCatchmentZone.Initialise';
begin
  Result := False;
  try
    FCatchmentFileData.Clear;
    FGrandMAP := 0;
    Result := True;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TCatchmentZone.Validate(ASourceStationID,ASplitIndex, ASourcePatchID,ALevel: Integer;var AMessage : string): boolean;
const OPNAME = 'TCatchmentZone.Validate';
begin
  Result := False;
  try
    if (ALevel = 0) and (ASourcePatchID = 0) and (ASplitIndex = 0) then
      Result := ValidateSource(ASourceStationID,ASourcePatchID,ASplitIndex,AMessage)
    else
    if (ALevel = 1) and (ASourcePatchID = 0) and (ASplitIndex = 0) then
      Result := ValidateSource(ASourceStationID,ASourcePatchID,ASplitIndex,AMessage)
    else
    if (ALevel = 1) and (ASourcePatchID = 0) and (ASplitIndex > 0) then
      Result := ValidateSplit(ASourceStationID,ASplitIndex,AMessage)
    else
    if (ALevel = 2) and (ASourcePatchID >= 0) and ((ASplitIndex = 0) or (ASplitIndex > 0)) then
      Result := ValidatePatch(ASourceStationID,ASourcePatchID,AMessage);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TCatchmentZone.ValidatePatch(ASourceStationID,ASourcePatchID: integer;var AMessage: string): boolean;
const OPNAME = 'TCatchmentZone.ValidatePatch';
var
  LIndex : integer;
begin
  Result := False;
  try
    for LIndex := 0 to FCatchmentSource.Count -1 do
    begin
      if (TCatchmentSource(FCatchmentSource.Items[LIndex]).StationID = ASourceStationID) then
      begin
        if (TCatchmentSource(FCatchmentSource.Items[LIndex]).FPatchID = ASourcePatchID) then
        begin
          AMessage := FAppModules.Language.GetString('Rainfall.CatchmentContainsSourceFromSameGauge');
          Break;
        end;
      end;
    end;
    Result := True;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TCatchmentZone.ValidateSource(ASourceStationID,ASourcePatchID,ASplitIndex: integer;var AMessage: string): boolean;
const OPNAME = 'TCatchmentZone.ValidateSource';
var
  LStation : IStationData;
  LSplit : IRainfallDataSplit;
  LCatchmentSource : TCatchmentSource;
begin
  Result := False;
  try
    LCatchmentSource := GetCatchmentSourceByStationID(ASourceStationID);
    if LCatchmentSource <> nil then
      AMessage :=  FAppModules.Language.GetString('Rainfall.CatchmentContainsSourceFromSameGauge')
    else
    begin
      LStation := (FAppModules.Model.ModelData as IRainfallModelData).GetStationDataByID(ASourceStationID);
      if (LStation <> nil) then
      begin
        LSplit := LStation.GetSplitWithIndex(ASplitIndex);
        if LSplit <> nil then
          if (LSplit.NrOfMissingMonths > 0) then
            AMessage := FAppModules.Language.GetString('Rainfall.MissingDataInRecordWarning')+#10#13+
                      FAppModules.Language.GetString('Rainfall.UsePatchedRecordForRainfallFile');
      end;
    end;
    Result := True;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TCatchmentZone.ValidateSplit(ASourceStationID,ASplitIndex: integer;var AMessage: string): boolean;
const OPNAME = 'TCatchmentZone.ValidateSplit';
var
  LStation : IStationData;
  LOtherSplit : IRainfallDataSplit;
  LCurrentSplit : IRainfallDataSplit;
  LIndex : integer;
  LCatchmentSource : TCatchmentSource;
begin
  Result := False;
  try
    LCatchmentSource := GetCatchmentSourceByStationID(ASourceStationID);
    if LCatchmentSource <> nil then
    begin
      if (LCatchmentSource.PatchID > 0) and (GetCatchmentSourceByPatchID(LCatchmentSource.PatchID) <> nil) then
      begin
        AMessage := FAppModules.Language.GetString('Rainfall.CatchmentContainsSourceFromSameGauge');
        Result := True;
        Exit;
      end;
      LStation := (FAppModules.Model.ModelData as IRainfallModelData).GetStationDataByID(ASourceStationID);
      if (LStation <> nil) then
      begin
        LCurrentSplit := LStation.GetSplitWithIndex(ASplitIndex);
        for LIndex := 0 to LStation.SplitCount-1 do
        begin
          if (LIndex <> ASplitIndex) then
          begin
            LOtherSplit := LStation.GetSplitWithIndex(LIndex);
            if (LOtherSplit <> nil) and (LCurrentSplit <> nil) then
            begin
              LCatchmentSource := GetCatchmentSourceBySplitStartEndYear(ASourceStationID,LOtherSplit.HydroStartYear,LOtherSplit.HydroEndYear);
              if (LCatchmentSource <> nil) then
              begin
                if ((LOtherSplit.HydroStartYear >= LCurrentSplit.HydroStartYear) and
                   (LOtherSplit.HydroEndYear <= LCurrentSplit.HydroEndYear)) or
                   ((LOtherSplit.HydroStartYear <= LCurrentSplit.HydroStartYear) and
                   (LOtherSplit.HydroEndYear >= LCurrentSplit.HydroEndYear)) then
                begin
                  AMessage := FAppModules.Language.GetString('Rainfall.SplitOverlapping');
                  Break;
                end;
              end;
            end;
          end
          else
            if (LIndex = ASplitIndex) and  (LCurrentSplit <> nil) and
              (GetCatchmentSourceBySplitStartEndYear(ASourceStationID,LCurrentSplit.HydroStartYear,LCurrentSplit.HydroEndYear) <> nil) then
            begin
              AMessage := FAppModules.Language.GetString('Rainfall.SplitAlreadyUsedInCatchment');
              Break;
            end
        end;
      end;
    end;
    Result := True;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

{ TCatchmentSource }

procedure TCatchmentSource.CreateMemberObjects;
const OPNAME = 'TCatchmentSource.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TCatchmentSource.DestroyMemberObjects;
const OPNAME = 'TCatchmentSource.DestroyMemberObjects';
begin
  try
    inherited DestroyMemberObjects;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

end.
