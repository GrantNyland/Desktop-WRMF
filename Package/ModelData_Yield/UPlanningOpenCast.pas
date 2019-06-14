unit UPlanningOpenCast;

interface

uses
  Classes,
   System.Generics.Collections,
  SysUtils,
  Contnrs,
  UMiningData,
  UPlanningMineGrowthFactor,
  ULoadGeneration,
  VoaimsCom_TLB;

type
  TPlanningOpenCast = class(TOpenCast, IPlanningOpenCast)
  protected
    FAbstractedonPopulate: boolean;
    FAbstraction: boolean;
    FPCDIniConcentration: double;
    FWorkingCommYear: integer;
    FWorkingDecommYear: integer;
    FWorkingCommMonth: integer;
    FWorkingDecommMonth: integer;
    FRunoffSaltWashOffEfficiencyFactor: double;
    FIniSaltStore: double;
    FRechargeRate: double;
    FAbstractToEvap: double;
    FAbstractToRiver: double;
    FAbstractToCPD: double;
    FAbstractMonthTimeSeriesFile: string;
    FGrowthFactorList : TObjectList;
    FLoadGenerationList : TObjectList;
    FAcceptedGrowthFactorType : TList<integer>;
    FAcceptedLoadGenType : TList<integer>;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    function Get_PCDIniConcentration: double; safecall;
    procedure Set_PCDIniConcentration(Value: double); safecall;
    function ValidatePCDIniConcentration(AErrorMessages: TStrings): wordbool;

    function Get_WorkingCommYear: integer; safecall;
    procedure Set_WorkingCommYear(Value: integer); safecall;
    function ValidateWorkingCommYear(AErrorMessages: TStrings): wordbool;

    function Get_WorkingDecommYear: integer; safecall;
    procedure Set_WorkingDecommYear(Value: integer); safecall;
    function ValidateWorkingDecommYear(AErrorMessages: TStrings): wordbool;

    function Get_WorkingCommMonth: integer; safecall;
    procedure Set_WorkingCommMonth(Value: integer); safecall;
    function ValidateWorkingCommMonth(AErrorMessages: TStrings): wordbool;

    function Get_WorkingDecommMonth: integer; safecall;
    procedure Set_WorkingDecommMonth(Value: integer); safecall;
    function ValidateWorkingDecommMonth(AErrorMessages: TStrings): wordbool;

    function Get_RunOffSaltWashOffEfficiencyFactor: double; safecall;
    procedure Set_RunOffSaltWashOffEfficiencyFactor(Value: double); safecall;
    function ValidateRunOffSaltWashOffEfficiencyFactor(AErrorMessages: TStrings): wordbool;

    function Get_IniSaltStore: double; safecall;
    procedure Set_IniSaltStore(Value: double); safecall;
    function ValidateIniSaltStore(AErrorMessages: TStrings): wordbool;

    function Get_ReChargeRate: double; safecall;
    procedure Set_ReChargeRate(Value: double); safecall;
    function ValidateReCharge(AErrorMessages: TStrings): wordbool;

    function Get_AbstractToEvap: double; safecall;
    procedure Set_AbstractToEvap(Value: double); safecall;
    function ValidateAbstractToEvap(AErrorMessages: TStrings): wordbool;

    function Get_AbstractToRiver: double; safecall;
    procedure Set_AbstractToRiver(Value: double); safecall;
    function ValidateAbstractToRiver(AErrorMessages: TStrings): wordbool;

    function Get_AbstractToCPD: double; safecall;
    procedure Set_AbstractToCPD(Value: double); safecall;
    function ValidateAbstractToCPD(AErrorMessages: TStrings): wordbool;

    function Get_AbstractMonthTimeSeriesFile: WideString; safecall;
    procedure Set_AbstractMonthTimeSeriesFile(const Value: WideString);
      safecall;
    function ValidateAbstractionMonthTimeSeriesFile(AErrorMessages: TStrings): wordbool;
    function Get_Abstraction: WordBool; safecall;
    procedure Set_Abstraction(Value: WordBool); safecall;
    function ValidateAbstraction(AErrorMessages: TStrings): wordbool;

    function Get_GrowthFactorCount: integer;
    function Get_LoadGenerationCount: integer;


  public
    function  NewGrowthFactor: TPlanningMineGrowthFactor;
    function  CreateGrowthFactor(AType: integer): IPlanningMineGrowthFactor;
    function NewLoadGeneration: TLoadGeneration;
    function CreateLoadGeneration(AType : integer): ILoadGeneration;
    function  GrowthFactorByType(AType:integer): TPlanningMineGrowthFactor;
    function  GrowthFactorByIndex(AIndex:integer): TPlanningMineGrowthFactor;
    function  LoadGenerationByType(AType: integer): TLoadGeneration;
    procedure Populate(AMineIdentifier              : integer;
                       AIdentifier                  : integer;
                       APitName                     : WideString;
                       ACoalReserveArea             : double;
                       AWorkingsArea                : double;
                       ADisturbedWorkingsArea       : double;
                       ADisturbedArea               : double;
                       AWaterSurfaceEvapArea        : double;
                       ADisturbedAreaRunoff         : double;
                       ADisturbedWorkingsAreaRunoff : double;
                       ADecantVolume                : double;
                       ASeepageVolume               : double;
                       AAnalysisStartVolume         : double;
                       AMaximumSeepageRate          : double;
                       ASeepageExponent             : double;
                       APCDSurfaceArea              : double;
                       APCDStorageCapacity          : double;
                       APCDAnalysisStartVolume      : double;
                       AAbstraction                 : integer;
                       APCDIniConcentration         : double;
                       AWorkingCommYear             : integer;
                       AWorkingCommMonth            : integer;
                       AWorkingDecommYear           : integer;
                       AWorkingDecommMonth          : integer;
                       ARunoffSaltWashOffEfficiencyFactor : double;
                       AIniSaltStore                : double;
                       AReChargeRate                : double;
                       AAbstractToEvap              : double;
                       AAbstractToRiver             : double;
                       AAbstractToCPD               : double;
                       AAbstractMonthTimeSeriesFile  : string); overload;
    procedure Populate( AAbstraction                : integer;
                        APCDIniConcentration        : double;
                        AWorkingCommYear            : integer;
                        AWorkingCommMonth           : integer;
                        AWorkingDecommYear          : integer;
                        AWorkingDecommMonth         : integer;
                        ARunoffSaltWashOffEfficiencyFactor : double;
                        AIniSaltStore               : double;
                        AReChargeRate               : double;
                        AAbstractToEvap             : double;
                        AAbstractToRiver            : double;
                        AAbstractToCPD              : double;
                        AAbstractMonthTimeSeriesFile : string) overload;
    function Initialise: boolean; override;
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; reintroduce;
    function BoolToStr(BoolValue: Wordbool): string;
    property PCDIniConcentration: double read Get_PCDIniConcentration
      write Set_PCDIniConcentration;
    property WorkingCommYear: integer read Get_WorkingCommYear
      write Set_WorkingCommYear;
    property WorkingDecommYear: integer read Get_WorkingDecommYear
      write Set_WorkingDecommYear;
    property WorkingCommMonth: integer read Get_WorkingCommMonth
      write Set_WorkingCommMonth;
    property WorkingDecommMonth: integer read Get_WorkingDecommMonth
      write Set_WorkingDecommMonth;
    property RunOffSaltWashOffEfficiencyFactor: double
      read Get_RunOffSaltWashOffEfficiencyFactor
      write Set_RunOffSaltWashOffEfficiencyFactor;
    property IniSaltStore: double read Get_IniSaltStore write Set_IniSaltStore;
    property ReChargeRate: double read Get_ReChargeRate write Set_ReChargeRate;
    property AbstractToEvap: double read Get_AbstractToEvap
      write Set_AbstractToEvap;
    property AbstractToRiver: double read Get_AbstractToRiver
      write Set_AbstractToRiver;
    property AbstractToCPD: double read Get_AbstractToCPD
      write Set_AbstractToCPD;
    property AbstractMonthTimeSeriesFile: WideString
      read Get_AbstractMonthTimeSeriesFile
      write Set_AbstractMonthTimeSeriesFile;
    function RemoveGrowthFactorByIndex(AIndex: integer): wordbool;
    function RemoveLoadGenerationByType(AType: integer): wordbool;
    property Abstraction: WordBool read Get_Abstraction write Set_Abstraction;
    property GrowthFactorCount: integer read Get_GrowthFactorCount;
    property LoadGenerationCount: integer read Get_LoadGenerationCount;
    property AcceptedGrowthTypes: TList<integer> read FAcceptedGrowthFactorType;
    property AcceptedLoadGenTypes: TList<integer> read FAcceptedLoadGenType;

  end;

implementation

uses
  System.Types,
  UPlanningMineSQLAgent,
  UConstants,
  UConditionalOpp,
  UAbstractObject,
  UErrorHandlingOperations;

{ TPlanningOpenCast }

function TPlanningOpenCast.CreateGrowthFactor(AType: integer): IPlanningMineGrowthFactor;
const OPNAME = 'TPlanningOpenCast.CreateGrowthFactor';
var
  LSQLAgent : TPlanningMineSQLAgent;
  LNoOfYears: string;
  LDescription : string;
begin
  Result := nil;
  try
    Result :=  NewGrowthFactor;
    if Result <> nil then
    begin
      LSQLAgent := TPlanningMineSQLAgent.Create(FAppModules);
      LDescription := LSQLAgent.getGrowthTypeDescription(AType);
      LNoOfYears := FloatToStr(System.SysUtils.CurrentYear);
      TPlanningMineGrowthFactor(Result).Populate(0,FMineIdentifier,FIdentifier,0,0,1,AType,1,LNoOfYears,FloatToStr(1.0),LDescription);
      if not LSQLAgent.AddGrowthFactor(TPlanningMineGrowthFactor(Result)) then
      begin
        FGrowthFactorList.Remove(TPlanningMineGrowthFactor(Result));
        FreeAndNil(Result);
      end;
    end;
  except on E: Exception do HandleError(E,OPNAME); end;
end;

function TPlanningOpenCast.CreateLoadGeneration(AType : integer): ILoadGeneration;
const OPNAME = 'TPlanningOpenCast.CreateLoadGeneration';
var
  LSQLAgent :  TPlanningMineSQLAgent;
  LFlow : TElevationsArray;
  LMeanOfSalt : TElevationsArray;
  LDescription : string;
begin
  Result := nil;
 try
  Result := NewLoadGeneration;
  if Result <> nil then
  begin
    LSQLAgent := TPlanningMineSQLAgent.Create(FAppModules);
    LDescription := LSQLAgent.getLoadGenTypeDescription(AType);
    SetLength(LFlow,10);
    SetLength(LMeanOfSalt,10);
    TLoadGeneration(Result).Populate(0,FMineIdentifier,FIdentifier,0,0,AType, 0.0,LFlow,LMeanOfSalt,LDescription);
    if not LSQLAgent.AddLoadGeneration(TLoadGeneration(Result)) then
    begin
      FGrowthFactorList.Remove(TLoadGeneration(Result));
    end;
  end;
 except on E: Exception do HandleError(E,OPNAME); end;
end;

procedure TPlanningOpenCast.CreateMemberObjects;
const OPNAME = 'TPlanningOpenCast.CreateMemberObjects';
begin
  inherited;
  try
    FGrowthFactorList := TObjectList.Create;
    //for LType := 2 to 7 do
    //  begin
    //   LGrowthFactor :=  NewGrowthFactor;
    //   LGrowthFactor.Populate(0,FMineIdentifier,Identifier,-1,-1,0,LType,1,'','');
    //  end;
    FLoadGenerationList  := TObjectList.Create;
    //for LType := 1 to 2 do
    //  begin
    //    LLoadGeneration := NewLoadGeneration;
    //    LFlow := FAppModules.FieldProperties.FieldProperty('Flow');
    //    if not Assigned(LFlow) then raise Exception.Create(' Field (Flow) not found in field properties');
    //    SetLength(LFlowArray,LFlow.ArrayLength);

    //    LMeanOfSalt := FAppModules.FieldProperties.FieldProperty('MeanOfSalt');
    //    if not Assigned(LMeanOfSalt) then raise Exception.Create('Field (MeanOfSalt) not found in field properties)');
    //    SetLength(LMeanOfSaltArray,LMeanOfSalt.ArrayLength);
    //    LLoadGeneration.Populate(0,FMineIdentifier,FIdentifier,-1,-1,LType,0.0,LFlowArray,LMeanOfSaltArray);
    //  end;
    FAcceptedGrowthFactorType :=  TList<integer>.Create;
    FAcceptedGrowthFactorType.Capacity := 6;
    FAcceptedGrowthFactorType.Add(2);
    FAcceptedGrowthFactorType.Add(3);
    FAcceptedGrowthFactorType.Add(4);
    FAcceptedGrowthFactorType.Add(5);
    FAcceptedGrowthFactorType.Add(6);
    FAcceptedGrowthFactorType.Add(7);

    FAcceptedLoadGenType := TList<integer>.Create;
    FAcceptedLoadGenType.Capacity := 2;
    FAcceptedLoadGenType.Add(1);
    FAcceptedLoadGenType.Add(2);
    Initialise;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TPlanningOpenCast.DestroyMemberObjects;
const OPNAME = 'TPlanningOpenCast.DestroyMemberObjects';
begin
  inherited;
  try
    FreeAndNil(FGrowthFactorList);
  except on E: Exception do HandleError(E,OPNAME); end;

end;

function TPlanningOpenCast.BoolToStr(BoolValue: Wordbool): string;
const OPNAME ='TPlanningOpenCast.BoolToStr';
begin
  Result := '';
  try
    case BoolValue of
      True: Result := '1';
      False: Result := ' ';
    end;
  except on E: Exception do handleError(E, OPNAME); end;
end;

function TPlanningOpenCast.Get_Abstraction: WordBool;
const
  OPNAME = 'TPlanningOpenCast.Get_Abstraction';
begin
  try
    Result := FAbstraction;
  except
    on E: Exception do
      HandleError(E, OPNAME);
  end;
end;

function TPlanningOpenCast.Get_AbstractMonthTimeSeriesFile: WideString;
const
  OPNAME = 'TPlanningOpenCast.Get_AbstractMonthTimeSeriesFile';
begin
  try
    Result := FAbstractMonthTimeSeriesFile;
  except
    on E: Exception do
      HandleError(E, OPNAME);
  end;
end;

function TPlanningOpenCast.Get_AbstractToCPD: double;
const
  OPNAME = 'TPlanningOpenCast.Get_AbstractToCPD';
begin
  try
    Result := FAbstractToCPD;
  except
    on E: Exception do
      HandleError(E, OPNAME);
  end;
end;

function TPlanningOpenCast.Get_AbstractToEvap: double;
const
  OPNAME = 'TPlanningOpenCast.Get_AbstractToEvap';
begin
  try
    Result := FAbstractToEvap;
  except
    on E: Exception do
      HandleError(E, OPNAME);
  end;
end;

function TPlanningOpenCast.Get_AbstractToRiver: double;
const
  OPNAME = 'TPlanningOpenCast.Get_AbstractToRiver';
begin
  try
    Result := FAbstractToRiver;
  except
    on E: Exception do
      HandleError(E, OPNAME);
  end;
end;

function TPlanningOpenCast.Get_GrowthFactorCount: integer;
const OPNAME = 'TPlanningOpenCast.Get_GrowthFactorCount';
begin
  Result := 0;
  try
    Result := FGrowthFactorList.Count;
  except on E: Exception do HandleError(E,OPNAME); end;
end;

function TPlanningOpenCast.Get_IniSaltStore: double;
const
  OPNAME = 'TPlanningOpenCast.Get_IniSaltStore';
begin
  try
    Result := FIniSaltStore;
  except
    on E: Exception do
      HandleError(E, OPNAME);
  end;
end;

function TPlanningOpenCast.Get_LoadGenerationCount: integer;
const OPNAME = 'TPlanningOpenCast.Get_LoadGenerationCount';
begin
  Result := 0;
  try
    Result := FLoadGenerationList.Count;
  except on E: Exception do HandleError(E,OPNAME); end;
end;

function TPlanningOpenCast.Get_PCDIniConcentration: double;
const
  OPNAME = 'TPlanningOpenCast.Get_PCDIniConcentration';
begin
  try
    Result := FPCDIniConcentration;
  except
    on E: Exception do
      HandleError(E, OPNAME)
  end;
end;

function TPlanningOpenCast.Get_ReChargeRate: double;
const
  OPNAME = 'TPlanningOpenCast.Get_ReChargeRate';
begin
  try
    Result := FRechargeRate;
  except
    on E: Exception do
      HandleError(E, OPNAME);
  end;

end;

function TPlanningOpenCast.Get_RunOffSaltWashOffEfficiencyFactor: double;
const
  OPNAME = 'TPlanningOpenCast.Get_RunOffSaltWashOffEfficiencyFactor';
begin
  try
    Result := FRunoffSaltWashOffEfficiencyFactor;
  except
    on E: Exception do
      HandleError(E, OPNAME);
  end;
end;

function TPlanningOpenCast.Get_WorkingCommMonth: integer;
const
  OPNAME = 'TPlanningOpenCast.Get_WorkingCommMonth';
begin
  try
    Result := FWorkingCommMonth;
  except
    on E: Exception do
      HandleError(E, OPNAME);
  end;
end;

function TPlanningOpenCast.Get_WorkingCommYear: integer;
const
  OPNAME = 'TPlanningOpenCast.Get_WorkingCommYear';
begin
  try
    Result := FWorkingCommYear;
  except
    on E: Exception do
      HandleError(E, OPNAME);
  end;
end;

function TPlanningOpenCast.Get_WorkingDecommMonth: integer;
const
  OPNAME = 'TPlanningOpenCast.Get_WorkingDecommMonth';
begin
  try
    Result := FWorkingDecommMonth;
  except
    on E: Exception do
      HandleError(E, OPNAME);
  end;
end;

function TPlanningOpenCast.Get_WorkingDecommYear: integer;
const
  OPNAME = 'TPlanningOpenCast.Get_WorkingDecommYear';
begin
  try
    Result := FWorkingDecommYear;
  except
    on E: Exception do
      HandleError(E, OPNAME);
  end;
end;

function TPlanningOpenCast.Initialise: boolean;
const OPNAME = 'TPlanningOpenCast.Initialise';
begin
  Result := inherited;
  try
    FAbstraction := FALSE;
    FPCDIniConcentration := 0.0;
    FWorkingCommYear := 0;
    FWorkingDecommYear := 0;
    FWorkingCommMonth   := 1;
    FWorkingDecommMonth :=1;
    FRunoffSaltWashOffEfficiencyFactor := 0.0;
    FIniSaltStore := 0.0;
    FRechargeRate := 0.0;
    FAbstractToEvap := 0.0;
    FAbstractToRiver := 0.0;
    FAbstractToCPD := 0.0;
    FAbstractMonthTimeSeriesFile := '';
    Result := true;
  except on E: Exception do HandleError(E,OPNAME); end;
end;

function TPlanningOpenCast.LoadGenerationByType(
  AType: integer): TLoadGeneration;
const OPNAME = 'TPlanningOpenCast.LoadGenerationByType';
var
  LCount : integer;
begin
  Result := nil;
  try
    for LCount := 0 to FLoadGenerationList.Count -1 do
      begin
      Result := TLoadGeneration(FLoadGenerationList[LCount]);
      if Result.type_ = AType then break
      else
        Result := nil;
      end;
  except on E:Exception do HandleError(E,OPNAME) end;
end;

function TPlanningOpenCast.NewGrowthFactor: TPlanningMineGrowthFactor;
const OPNAME = 'TPlanningOpenCast.NewGrowthFactor';
begin
  Result := nil;
  try
    Result := TPlanningMineGrowthFactor.Create(FAppModules);
    FGrowthFactorList.Add(Result);
  except on E: Exception do HandleError(E,OPNAME); end;
end;

function TPlanningOpenCast.NewLoadGeneration: TLoadGeneration;
const OPNAME = 'TPlanningOpenCast.NewLoadGeneration';
begin
  Result := nil;
  try
    Result := TLoadGeneration.Create(FAppModules);
    FLoadGenerationList.Add(Result);
  except on E: Exception do HandleError(E,OPNAME); end;
end;

procedure TPlanningOpenCast.Populate(AAbstraction: integer;
  APCDIniConcentration: double; AWorkingCommYear, AWorkingCommMonth,
  AWorkingDecommYear, AWorkingDecommMonth: integer;
  ARunoffSaltWashOffEfficiencyFactor, AIniSaltStore, AReChargeRate,
  AAbstractToEvap: double; AAbstractToRiver: double;
  AAbstractToCPD: double; AAbstractMonthTimeSeriesFile: string);
const OPNAME = 'TPlanningOpenCast.Populate';
begin
  try
    case AAbstraction > 0 of
    True:
    begin
      FAbstraction := True;
      FAbstractToEvap := AAbstractToEvap;
      FAbstractToRiver := AAbstractToRiver;
      FAbstractToCPD := AAbstractToCPD;
      FAbstractMonthTimeSeriesFile  := AAbstractMonthTimeSeriesFile;
      FAbstractedonPopulate := true;
    end;

    False: FAbstraction := False;
    end;

    FPCDIniConcentration := APCDIniConcentration;
    FWorkingCommYear := AWorkingCommYear;
    FWorkingCommMonth := AWorkingCommMonth;
    FWorkingDecommYear := AWorkingDecommYear;
    FWorkingDecommMonth := AWorkingDecommMonth;
    FRunoffSaltWashOffEfficiencyFactor := ARunoffSaltWashOffEfficiencyFactor;
    FIniSaltStore := AIniSaltStore;
    FRechargeRate := AReChargeRate;

    // Update children that do not know the new ID
    (*for LCount := 0 to FGrowthFactorList.Count - 1 do
    begin
      LGrowthFactor := TPlanningMineGrowthFactor(FGrowthFactorList[LCount]);
      LGrowthFactor.Populate(LGrowthFactor.Identifier,
      FMineIdentifier,
      FIdentifier,LGrowthFactor.SlurryDumpIdentifier,
      LGrowthFactor.UnderGroundIdentifier,LGrowthFactor.NoOfPoints,LGrowthFactor.FactorType,
      StrToInt(ConditionalOpp(LGrowthFactor.InterpolationMethod,2,1)),
      LGrowthFactor.ListToStringYears,LGrowthFactor.ListToStringGrowthFactor,'');
    end;

    for LCount := 0 to FLoadGenerationList.Count - 1 do
      begin
        LLoadGeneration := TLoadGeneration(FLoadGenerationList[LCount]);
        SetLength(LFlowArray,10);
        SetLength(LMeanArray,10);
        for LIndex := 0 to 9 do
          begin
            LFlowArray[LIndex] := LLoadGeneration.FlowByIndex[LIndex];
            LMeanArray[LIndex] := LLoadGeneration.MeanOfSaltByIndex[LIndex];
          end;
        LLoadGeneration.Populate(LLoadGeneration.Identifier,FMineIdentifier,FIdentifier,-1,-1,
        LLoadGeneration.type_,LLoadGeneration.StandardDeviation,LFlowArray,LMeanArray);
      end;*)
  except on E: Exception do HandleError(E,OPNAME); end;
end;

function TPlanningOpenCast.RemoveGrowthFactorByIndex(AIndex: integer): wordbool;
const OPNAME = 'TPlanningOpenCast.RemoveGrowthFactorByIndex';
var
  LSQLAgent : TPlanningMineSQLAgent;
  LGrowthFactor : TPlanningMineGrowthFactor;
begin
  Result := false;
  try
    LSQLAgent := TPlanningMineSQLAgent.Create(FAppModules);
    LGrowthFactor :=  TPlanningMineGrowthFactor(Self.GrowthFactorByIndex(AIndex));
    if LGrowthFactor <> nil then
    begin
      if LSQLAgent.DeleteGrowthFactor(LGrowthFactor) then
      begin
        FGrowthFactorList.Remove(LGrowthFactor);
        Result := true;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TPlanningOpenCast.RemoveLoadGenerationByType(AType: integer): wordbool;
const OPNAME = 'TPlanningOpenCast.RemoveLoadGenerationByType';
var
  LSQLAgent : TPlanningMineSQLAgent;
  LLoadGeneration : TLoadGeneration;
begin
  Result := false;
  try
    LSQLAgent := TPlanningMineSQLAgent.Create(FAppModules);
    LLoadGeneration :=  TLoadGeneration(Self.LoadGenerationByType(AType));
    if LLoadGeneration <> nil then
    begin
      if LSQLAgent.DeleteLoadGeneration(LLoadGeneration) then
      begin
        FLoadGenerationList.Remove(LLoadGeneration);
        Result := true;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TPlanningOpenCast.Populate(AMineIdentifier, AIdentifier: integer;
  APitName: WideString; ACoalReserveArea, AWorkingsArea, ADisturbedWorkingsArea,
  ADisturbedArea, AWaterSurfaceEvapArea, ADisturbedAreaRunoff,
  ADisturbedWorkingsAreaRunoff, ADecantVolume, ASeepageVolume,
  AAnalysisStartVolume, AMaximumSeepageRate, ASeepageExponent, APCDSurfaceArea,
  APCDStorageCapacity, APCDAnalysisStartVolume: double; AAbstraction: integer;
  APCDIniConcentration: double; AWorkingCommYear, AWorkingCommMonth,
  AWorkingDecommYear, AWorkingDecommMonth: integer;
  ARunoffSaltWashOffEfficiencyFactor, AIniSaltStore, AReChargeRate,
  AAbstractToEvap, AAbstractToRiver, AAbstractToCPD: double;
  AAbstractMonthTimeSeriesFile: string);
const OPNAME = 'TPlanningOpenCast.Populate';
begin
  try
    inherited Populate(AMineIdentifier, AIdentifier, APitName, ACoalReserveArea, AWorkingsArea,
                        ADisturbedWorkingsArea, ADisturbedArea, AWaterSurfaceEvapArea, ADisturbedAreaRunoff,
                        ADisturbedWorkingsAreaRunoff,ADecantVolume, ASeepageVolume, AAnalysisStartVolume,
                        AMaximumSeepageRate, ASeepageExponent, APCDSurfaceArea, APCDStorageCapacity, APCDAnalysisStartVolume);
    Populate(AAbstraction, APCDIniConcentration, AWorkingCommYear, AWorkingCommMonth, AWorkingDecommYear,
             AWorkingDecommMonth, ARunoffSaltWashOffEfficiencyFactor, AIniSaltStore, AReChargeRate, AAbstractToEvap,
             AAbstractToRiver, AAbstractToCPD, AAbstractMonthTimeSeriesFile);

  except on E:Exception do HandleError(E,OPNAME); end;
end;

procedure TPlanningOpenCast.Set_Abstraction(Value: WordBool);
const
  OPNAME = 'TPlanningOpenCast.Set_Abstraction';
var
  LSQLAgent: TPlanningMineSQLAgent;
  LContextData: TStringList;
  LOldValue: string;
begin
  try
    LSQLAgent := TPlanningMineSQLAgent.Create(FAppModules);
    LContextData := TStringList.Create;
    try
      LSQLAgent.LoadContextData_OpenCast(LContextData, FMineIdentifier,
        FIdentifier);
      if (FAppModules.FieldProperties.UpdateFieldValue('AbstractionIndicator',
        BoolToStr(Value),BoolToStr(FAbstraction),
        LContextData)) then
      begin
        LOldValue := BoolToStr(FAbstraction);
        FAbstraction := Value;
        if not FAbstractedonPopulate then
        begin
          Set_AbstractToEvap(FAbstractToEvap);
          Set_AbstractToRiver(FAbstractToRiver);
          Set_AbstractToCPD(FAbstractToCPD);
          FAbstractedonPopulate := false;
        end;
        FAppModules.Model.StudyDataHasChanged(sdccEdit, 'AbstractionIndicator',
          LOldValue, BoolToStr(FAbstraction));
      end;
    finally
      FreeAndNil(LSQLAgent);
      FreeAndNil(LContextData);
    end;
  except
    on E: Exception do
      HandleError(E, OPNAME);
  end;
end;

procedure TPlanningOpenCast.Set_AbstractMonthTimeSeriesFile
  (const Value: WideString);
const
  OPNAME = 'TPlanningOpenCast.Set_AbstractMonthTimeSeriesFile';
var
  LSQLAgent: TPlanningMineSQLAgent;
  LContextData: TStringList;
  LOldValue: string;
begin
  try
    LSQLAgent := TPlanningMineSQLAgent.Create(FAppModules);
    LContextData := TStringList.Create;
    LSQLAgent.LoadContextData_OpenCast(LContextData,FMineIdentifier,FIdentifier);
    try
      if (FAppModules.FieldProperties.UpdateFieldValue
        ('AbstractMonthTimeSeriesFile', Value, FAbstractMonthTimeSeriesFile,
        LContextData)) then
      begin
        LOldValue := FAbstractMonthTimeSeriesFile;
        FAbstractMonthTimeSeriesFile := Value;
        FAppModules.Model.StudyDataHasChanged(sdccEdit,
          'AbstractMonthTimeSeriesFile', LOldValue,
          FAbstractMonthTimeSeriesFile);
      end;
    finally
      FreeAndNil(LSQLAgent);
      FreeAndNil(LContextData);
    end;
  except
    on E: Exception do
      HandleError(E, OPNAME);
  end;
end;

procedure TPlanningOpenCast.Set_AbstractToCPD(Value: double);
const OPNAME = 'TPlanningOpenCast.Set_AbstractToCPD';
var
  LSQLAgent : TPlanningMineSQLAgent;
  LContextData : TStringList;
  LOldValue : string;
begin
  try
    try
      LSQLAgent := TPlanningMineSQLAgent.Create(FAppModules);
      LContextData := TStringList.Create;
      LSQLAgent.LoadContextData_OpenCast(LContextData,FMineIdentifier, FIdentifier);
      if (FAppModules.FieldProperties.UpdateFieldValue('AbstractToCPD',FloatToStr(Value), FloatToStr(FAbstractToCPD),LContextData)) then
      begin
        LOldValue := FloatToStr(FAbstractToCPD);
        FAbstractToCPD := Value;
        FAppModules.Model.StudyDataHasChanged(sdccEdit,'AbstractToCPD',LOldValue,FloatToStr(FAbstractToCPD));
      end;
    finally
      FreeAndNil(LSQLAgent);
      FreeAndNil(LContextData)
    end;
  except on E: Exception do HandleError(E,OPNAME); end;
end;

procedure TPlanningOpenCast.Set_AbstractToEvap(Value: double);
const OPNAME = 'TPlanningOpenCast.Set_AbstractToEvap';
var
  LSQLAgent : TPlanningMineSQLAgent;
  LContextData : TStringList;
  LOldValue : string;
begin
  try
    try
      LSQLAgent := TPlanningMineSQLAgent.Create(FAppModules);
      LContextData := TStringList.Create;
      LSQLAgent.LoadContextData_OpenCast(LContextData,FMineIdentifier,FIdentifier);
      if (FAppModules.FieldProperties.UpdateFieldValue('AbstractToEvap',FloatToStr(Value),FloatToStr(FAbstractToEvap),LContextData)) then
      begin
        LOldValue  := FloatToStr(FAbstractToEvap);
        FAbstractToEvap := Value;
        FAppModules.Model.StudyDataHasChanged(sdccEdit,'AbstractToEvap',LOldValue,FloatToStr(FAbstractToEvap));

      end;
    finally
      FreeAndNil(LSQLAgent);
      FreeAndNil(LContextData);
    end;
  except on E: Exception do HandleError(E,OPNAME); end;
end;

procedure TPlanningOpenCast.Set_AbstractToRiver(Value: double);
const OPNAME = 'TPlanningOpenCast.Set_AbstractToRiver';
var
  LSQLAgent : TPlanningMineSQLAgent;
  LContextData : TStringList;
  LOldValue : string;
begin
  try
    try
      LSQLAgent := TPlanningMineSQLAgent.Create(FAppModules);
      LContextData := TStringList.Create;
      LSQLAgent.LoadContextData_OpenCast(LContextData, FMineIdentifier, FIdentifier);
      if (FAppModules.FieldProperties.UpdateFieldValue('AbstractToRiver',FloatToStr(Value),FloatToStr(FAbstractToRiver),LContextData)) then
      begin
        LOldValue := FloatToStr(FAbstractToRiver);
        FAbstractToRiver := Value;
        FAppModules.Model.StudyDataHasChanged(sdccEdit,'AbstractToRiver',LOldValue,FloatToStr(FAbstractToRiver))
      end;
    finally
      FreeAndNil(LSQLAgent);
      FreeAndNil(LContextData);
    end;
  except on E: Exception do HandleError(E,OPNAME); end;
end;

procedure TPlanningOpenCast.Set_IniSaltStore(Value: double);
const
  OPNAME = 'TPlanningOpenCast.Set_IniSaltStore';
var
  LSQLAgent: TPlanningMineSQLAgent;
  LContextData: TStringList;
  LOldValue: string;
begin
  try
    LSQLAgent := TPlanningMineSQLAgent.Create(FAppModules);
    LContextData := TStringList.Create;
    LSQLAgent.LoadContextData_OpenCast(LContextData, FMineIdentifier,
      FIdentifier);
    try
      if (FAppModules.FieldProperties.UpdateFieldValue('OpenCastIniSaltStore',
        FloatToStr(Value), FloatToStr(FIniSaltStore), LContextData)) then
      begin
        LOldValue := FloatToStr(FIniSaltStore);
        FIniSaltStore := Value;
        FAppModules.Model.StudyDataHasChanged(sdccEdit, 'OpenCastIniSaltStore',
          LOldValue, FloatToStr(Value));
      end;
    finally
      FreeAndNil(LSQLAgent);
      FreeAndNil(LContextData)
    end;
  except
    on E: Exception do
      HandleError(E, OPNAME);
  end;
end;

procedure TPlanningOpenCast.Set_PCDIniConcentration(Value: double);
const
  OPNAME = 'TPlanningOpenCast.Set_PCDIniConcentration';
var
  LSQLAgent: TPlanningMineSQLAgent;
  LContextData: TStringList;
  LOldValue : string;
begin
  try
    try
      LSQLAgent := TPlanningMineSQLAgent.Create(FAppModules);
      LContextData := TStringList.Create;
      LSQLAgent.LoadContextData_OpenCast(LContextData, FMineIdentifier,
        FIdentifier);
      if (FAppModules.FieldProperties.UpdateFieldValue('PCDIniConcentration',
        FloatToStr(Value), FloatToStr(FPCDIniConcentration), LContextData)) then
      begin
        LOldValue := FloatToStr(FPCDIniConcentration);
        FPCDIniConcentration := Value;
        FAppModules.Model.StudyDataHasChanged(sdccEdit, 'PCDIniConcentration',
          LOldValue, FloatToStr(FPCDIniConcentration));
      end;
    finally
      FreeAndNil(LSQLAgent);
      FreeAndNil(LContextData);
    end;
  except
    on E: Exception do
      HandleError(E, OPNAME);
  end;

end;

procedure TPlanningOpenCast.Set_ReChargeRate(Value: double);
const
  OPNAME = 'TPlanningOpenCast.Set_ReChargeRate';
var
  LSQLAgent: TPlanningMineSQLAgent;
  LContextData: TStringList;
  LOldValue: string;
begin
  try
    try
      LSQLAgent := TPlanningMineSQLAgent.Create(FAppModules);
      LContextData := TStringList.Create;
      LSQLAgent.LoadContextData_OpenCast(LContextData, FMineIdentifier,
        FIdentifier);
      if (FAppModules.FieldProperties.UpdateFieldValue('RechargeRate',
        FloatToStr(Value), FloatToStr(FRechargeRate), LContextData)) then
      begin
        LOldValue := FloatToStr(FRechargeRate);
        FRechargeRate := Value;
        FAppModules.Model.StudyDataHasChanged(sdccEdit, 'RechargeRate',
          LOldValue, FloatToStr(FRechargeRate));
      end;
    finally
      FreeAndNil(LSQLAgent);
      FreeAndNil(LContextData);
    end;
  except
    on E: Exception do
      HandleError(E, OPNAME);
  end;
end;

procedure TPlanningOpenCast.Set_RunOffSaltWashOffEfficiencyFactor
  (Value: double);
const
  OPNAME = 'TPlanningOpenCast.Set_RunOffSaltWashOffEfficiencyFactor';
var
  LSQLAgent: TPlanningMineSQLAgent;
  LContextData: TStringList;
  LOldValue: string;
begin
  try
    try
      LSQLAgent := TPlanningMineSQLAgent.Create(FAppModules);
      LContextData := TStringList.Create;
      LSQLAgent.LoadContextData_OpenCast(LContextData, FMineIdentifier,
        FIdentifier);
      if (FAppModules.FieldProperties.UpdateFieldValue
        ('RunoffSaltWashOffEfficiencyFactor', FloatToStr(Value),
        FloatToStr(FRunoffSaltWashOffEfficiencyFactor), LContextData)) then
      begin
        LOldValue := FloatToStr(FRunoffSaltWashOffEfficiencyFactor);
        FRunoffSaltWashOffEfficiencyFactor := Value;
        FAppModules.Model.StudyDataHasChanged(sdccEdit,
          'RunoffSaltWashOffEfficiencyFactor', LOldValue,
          FloatToStr(FRunoffSaltWashOffEfficiencyFactor));
      end;
    finally
      FreeAndNil(LSQLAgent);
      FreeAndNil(LContextData);
    end;
  except
    on E: Exception do
      HandleError(E, OPNAME);
  end;
end;

procedure TPlanningOpenCast.Set_WorkingCommMonth(Value: integer);
const OPNAME = 'TPlanningOpenCast.Set_WorkingCommMonth';
var
  LSQLAgent : TPlanningMineSQLAgent;
  LContextData : TStringList;
  LOldValue : string;
begin
  try
    try
      LSQLAgent := TPlanningMineSQLAgent.Create(FAppModules);
      LContextData := TStringList.Create;
      LSQLAgent.LoadContextData_OpenCast(LContextData,FMineIdentifier, FIdentifier);
      if (FAppModules.FieldProperties.UpdateFieldValue('WorkingCommMonth',FloatToStr(Value),FloatToStr(FWorkingCommMonth),LContextData)) then
      begin
        LOldValue := FloatToStr(FWorkingCommMonth);
        FWorkingCommMonth := Value;
        FAppModules.Model.StudyDataHasChanged(sdccEdit,'WorkingCommMonth',LOldValue,FloatToStr(FWorkingCommMonth));
      end;
    finally
      FreeAndNil(LSQLAgent);
      FreeAndNil(LContextData);
    end;
  except on E: Exception do HandleError(E,OPNAME); end;
end;

procedure TPlanningOpenCast.Set_WorkingCommYear(Value: integer);
const OPNAME = 'Set_WorkingCommYear';
var
  LSQLAgent : TPlanningMineSQLAgent;
  LContextData : TStringList;
  LOldValue : string;
begin
  try
    try
      LSQLAgent := TPlanningMineSQLAgent.Create(FAppModules);
      LContextData := TStringList.Create;
      LSQLAgent.LoadContextData_OpenCast(LContextData,FMineIdentifier, FIdentifier);
      if (FAppModules.FieldProperties.UpdateFieldValue('WorkingCommYear',IntToStr(Value),IntToStr(FWorkingCommYear),LContextData)) then
      begin
        LOldValue := IntToStr(FWorkingCommYear);
        FWorkingCommYear := Value;
        FAppModules.Model.StudyDataHasChanged(sdccEdit,'WorkingCommYear',LOldValue,FloatToStr(FWorkingCommYear));
      end;
    finally
      FreeAndNil(LSQLAgent);
      FreeAndNil(LContextData);
    end;
  except on E: Exception do HandleError(E,OPNAME); end;
end;

procedure TPlanningOpenCast.Set_WorkingDecommMonth(Value: integer);
const OPNAME = 'TPlanningOpenCast.Set_WorkingDecommMonth';
var
  LSQLAgent : TPlanningMineSQLAgent;
  LContextData : TStringList;
  LOldValue : string;
begin
  try
    try
      LSQLAgent := TPlanningMineSQLAgent.Create(FAppModules);
      LContextData := TStringList.Create;
      LSQLAgent.LoadContextData_OpenCast(LContextData,FMineIdentifier,FIdentifier);
      if (FAppModules.FieldProperties.UpdateFieldValue('WorkingDecommMonth',IntToStr(Value),IntToStr(FWorkingDecommMonth),LContextData)) then
      begin
        LOldValue := IntToStr(FWorkingDecommMonth);
        FWorkingDecommMonth := Value;
        FAppModules.Model.StudyDataHasChanged(sdccEdit,'WorkingDecommMonth',LOldValue,IntToStr(FWorkingDecommMonth));
      end;
    finally
      FreeAndNil(LSQLAgent);
      FreeAndNil(LContextData);
    end;
  except on E: Exception do HandleError(E,OPNAME); end;
end;

procedure TPlanningOpenCast.Set_WorkingDecommYear(Value: integer);
const OPNAME = 'TPlanningOpenCast.Set_WorkingDecommYear';
var
  LSQLAgent  : TPlanningMineSQLAgent;
  LContextData : TStringList;
  LOldValue : string;
begin
  try
    try
      LSQLAgent := TPlanningMineSQLAgent.Create(FAppModules);
      LContextData :=TStringList.Create;
      LSQLAgent.LoadContextData_OpenCast(LContextData,FMineIdentifier,FIdentifier);
      if (FAppModules.FieldProperties.UpdateFieldValue('WorkingDecommYear',IntToStr(Value),IntToStr(FWorkingDecommYear),LContextData)) then
      begin
        LOldValue := IntToStr(FWorkingDecommYear);
        FWorkingDecommYear := Value;
        FAppModules.Model.StudyDataHasChanged(sdccEdit,'WorkingDecommYear',LOldValue,IntToStr(FWorkingDecommYear));
      end;
    finally
      FreeAndNil(LSQLAgent);
      FreeAndNil(LContextData);
    end;
  except on E: Exception do HandleError(E,OPNAME); end;

end;

function TPlanningOpenCast.Validate(var AErrors: WideString;
  const AContext: WideString): WordBool;
const OPNAME = 'TPlanningMineGrowthFactor.Validate';
var
  //  LErrorMessages : TStringList;
  LErrorCols        : TStringList;
  LErrorMsgs        : TStringList;
  LIndex  : integer;
  LGrowthFactor : TPlanningMineGrowthFactor;
  LLoadGeneration : TLoadGeneration;
begin
  Result:= inherited Validate(AErrors,AContext);
  try
    LErrorCols := TStringList.Create;
    LErrorMsgs := TStringList.Create;
    try
      if(AContext = 'AbstractionIndicator') then
        Result := ValidateAbstraction(LErrorMsgs)
      else if(AContext = 'WorkingCommYear') then
        Result := ValidateWorkingCommYear(LErrorMsgs)
      else if(AContext = 'WorkingCommMonth') then
        Result := ValidateWorkingCommMonth(LErrorMsgs)

      else if(AContext = 'WorkingDecommYear') then
        Result := ValidateWorkingDecommYear(LErrorMsgs)
      else if(AContext = 'WorkingDecommMonth') then
        Result := ValidateWorkingDecommMonth(LErrorMsgs)
      else if(AContext = 'PCDIniConcentration') then
        Result := ValidatePCDIniConcentration(LErrorMsgs)
      else if(AContext = 'RunoffSaltWashOffEfficiencyFactor') then
        Result := ValidateRunOffSaltWashOffEfficiencyFactor(LErrorMsgs)
      else if(AContext = 'OpenCastIniSaltStore') then
        Result := ValidateIniSaltStore(LErrorMsgs)
      else if(AContext = 'RechargeRate') then
        Result := ValidateReCharge(LErrorMsgs)
      else if(AContext = 'AbstractToEvap') then
        Result := ValidateAbstractToEvap(LErrorMsgs)
      else if(AContext = 'AbstractToRiver') then
        Result := ValidateAbstractToRiver(LErrorMsgs)
      else if(AContext = 'AbstractToCPD') then
        Result := ValidateAbstractToCPD(LErrorMsgs)
      else if(AContext = 'AbstractMonthTimeSeriesFile') then
        Result := ValidateAbstractionMonthTimeSeriesFile(LErrorMsgs)
      else
      begin
        Result := ValidatePCDIniConcentration(LErrorCols) and ValidateWorkingCommYear(LErrorCols) and
        ValidateWorkingDecommYear(LErrorMsgs) and ValidateWorkingCommMonth(LErrorMsgs) and ValidateWorkingDecommMonth(LErrorMsgs) and
        ValidateRunOffSaltWashOffEfficiencyFactor(LErrorMsgs) and ValidateIniSaltStore(LErrorCols) and ValidateReCharge(LErrorMsgs);
        for LIndex := 0 to FGrowthFactorList.Count-1 do
        begin
           LGrowthFactor := TPlanningMineGrowthFactor(FGrowthFactorList[LIndex]);
           if not LGrowthFactor.Validate(AErrors,AContext) then Result := false;
        end;

        for LIndex  := 0 to FLoadGenerationList.Count - 1 do
        begin
          LLoadGeneration := TLoadGeneration(FLoadGenerationList[LIndex]);
          if not LLoadGeneration.Validate(AErrors,AContext) then Result := false;
        end;
      end;

      if (not Result) then
      begin
      if (LErrorCols.Count = 0) then
        AErrors := AErrors + LErrorMsgs.Text
      else
        AErrors := AErrors + CTStringsSeparator + LErrorMsgs.Text +
                   CTStringsSeparator + LErrorCols.Text + CTStringsSeparator;
      LErrorMsgs.Clear;
      LErrorCols.Clear;
      end;
    finally
      FreeAndNil(LErrorCols);
      FreeAndNil(LErrorMsgs);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TPlanningOpenCast.ValidateAbstraction(
  AErrorMessages: TStrings): wordbool;
const OPNAME = 'TPlanningOpenCast.ValidateAbstraction';
var
  lMessage : string;
begin
  Result := False;
  try
if (not (FAppModules.FieldProperties.ValidateFieldProperty('AbstractionIndicator',BoolToStr(FAbstraction),lMessage))) then
        AErrorMessages.Add(lMessage)
      else
        Result := true;
  except on E: Exception do HandleError(E, OPNAME)  end;
end;

function TPlanningOpenCast.ValidateAbstractionMonthTimeSeriesFile(
  AErrorMessages: TStrings): wordbool;
const OPNAME = 'TPlanningOpenCast.ValidateAbstractionMonthTimeSeriesFile';
var
  lMessage : string;
begin
  Result := False;
  try
      if (not (FAppModules.FieldProperties.ValidateFieldProperty('AbstractMonthTimeSeriesFile',FAbstractMonthTimeSeriesFile,lMessage))) then
        AErrorMessages.Add(lMessage)
      else
        Result := true;
  except on E: Exception do HandleError(E, OPNAME)  end;
end;

function TPlanningOpenCast.ValidateAbstractToCPD(
  AErrorMessages: TStrings): wordbool;
const OPNAME = 'TPlanningOpenCast.ValidateAbstractToCPD';
var
  lMessage : string;
begin
  Result := False;
  try
      if (not (FAppModules.FieldProperties.ValidateFieldProperty('AbstractToCPD',FloatToStr(AbstractToCPD),lMessage))) then
        AErrorMessages.Add(lMessage)
      else
        Result := true;
  except on E: Exception do HandleError(E, OPNAME)  end;
end;

function TPlanningOpenCast.ValidateAbstractToEvap(
  AErrorMessages: TStrings): wordbool;
const OPNAME = 'TPlanningOpenCast.ValidateAbstractToEvap';
var
  lMessage : string;
begin
  Result := False;
  try
      if (not (FAppModules.FieldProperties.ValidateFieldProperty('AbstractToEvap',FloatToStr(AbstractToEvap),lMessage))) then
        AErrorMessages.Add(lMessage)
      else
        Result := true;
  except on E: Exception do HandleError(E, OPNAME)  end;
end;

function TPlanningOpenCast.ValidateAbstractToRiver(
  AErrorMessages: TStrings): wordbool;
const OPNAME = 'TPlanningOpenCast.ValidateAbstractToRiver';
var
  lMessage : string;
begin
  Result := False;
  try
      if (not (FAppModules.FieldProperties.ValidateFieldProperty('AbstractToRiver',FloatToStr(AbstractToRiver),lMessage))) then
        AErrorMessages.Add(lMessage)
      else
        Result := true;
  except on E: Exception do HandleError(E, OPNAME)  end;
end;

function TPlanningOpenCast.ValidateIniSaltStore(
  AErrorMessages: TStrings): wordbool;
const OPNAME = 'TPlanningOpenCast.ValidateIniSaltStore';
var
  lMessage : string;
begin
  Result := False;
  try
      if (not (FAppModules.FieldProperties.ValidateFieldProperty('OpenCastIniSaltStore',FloatToStr(FIniSaltStore),lMessage))) then
        AErrorMessages.Add(lMessage)
      else
        Result := true;
  except on E: Exception do HandleError(E, OPNAME)  end;
end;


function TPlanningOpenCast.ValidatePCDIniConcentration(
  AErrorMessages: TStrings): wordbool;
const OPNAME = 'TPlanningOpenCast.ValidatePCDIniConcentration';
var
  lMessage : string;
begin
  Result := False;
  try
      if (not (FAppModules.FieldProperties.ValidateFieldProperty('PCDIniConcentration',FloatToStr(FPCDIniConcentration),lMessage))) then
        AErrorMessages.Add(lMessage)
      else
        Result := true;
  except on E: Exception do HandleError(E, OPNAME)  end;
end;

function TPlanningOpenCast.ValidateReCharge(AErrorMessages: TStrings): wordbool;
const OPNAME = 'TPlanningOpenCast.ValidateReCharge';
var
  lMessage : string;
begin
  Result := False;
  try
      if (not (FAppModules.FieldProperties.ValidateFieldProperty('RechargeRate',FloatToStr(FRechargeRate),lMessage))) then
        AErrorMessages.Add(lMessage)
      else
        Result := true;
  except on E: Exception do HandleError(E, OPNAME)  end;
end;

function TPlanningOpenCast.ValidateRunOffSaltWashOffEfficiencyFactor(
  AErrorMessages: TStrings): wordbool;
const OPNAME = 'TPlanningOpenCast.ValidateRunOffSaltWashOffEfficiencyFactor';
var
  lMessage : string;
begin
  Result := False;
  try
      if (not (FAppModules.FieldProperties.ValidateFieldProperty('RunoffSaltWashOffEfficiencyFactor',FloatToStr(FRunoffSaltWashOffEfficiencyFactor),lMessage))) then
        AErrorMessages.Add(lMessage)
      else
        Result := true;
  except on E: Exception do HandleError(E, OPNAME)  end;
end;

function TPlanningOpenCast.ValidateWorkingCommMonth(
  AErrorMessages: TStrings): wordbool;
const OPNAME = 'TPlanningOpenCast.ValidateWorkingCommMonth';
var
  lMessage : string;
begin
  Result := False;
  try
      if (not (FAppModules.FieldProperties.ValidateFieldProperty('WorkingCommMonth',FloatToStr(FWorkingCommMonth),lMessage))) then
        AErrorMessages.Add(lMessage)
      else
        Result := true;
  except on E: Exception do HandleError(E, OPNAME)  end;
end;

function TPlanningOpenCast.ValidateWorkingCommYear(
  AErrorMessages: TStrings): wordbool;
const OPNAME = 'TPlanningOpenCast.ValidateWorkingCommYear';
var
  lMessage : string;
begin
  Result := False;
  try
      if (not (FAppModules.FieldProperties.ValidateFieldProperty('WorkingCommYear',FloatToStr(FWorkingCommYear),lMessage))) then
        AErrorMessages.Add(lMessage)
      else
        Result := true;
  except on E: Exception do HandleError(E, OPNAME)  end;
end;

function TPlanningOpenCast.ValidateWorkingDecommMonth(
  AErrorMessages: TStrings): wordbool;
const OPNAME = 'TPlanningOpenCast.ValidateWorkingDecommMonth';
var
  lMessage : string;
begin
  Result := False;
  try
      if (not (FAppModules.FieldProperties.ValidateFieldProperty('WorkingDecommMonth',FloatToStr(FWorkingDecommMonth),lMessage))) then
        AErrorMessages.Add(lMessage)
      else
        Result := true;
  except on E: Exception do HandleError(E, OPNAME)  end;
end;

function TPlanningOpenCast.ValidateWorkingDecommYear(
  AErrorMessages: TStrings): wordbool;
const OPNAME = 'TPlanningOpenCast.ValidateWorkingDecommYear';
var
  lMessage : string;
begin
  Result := False;
  try
      if (not (FAppModules.FieldProperties.ValidateFieldProperty('WorkingDecommYear',FloatToStr(FWorkingDecommYear),lMessage))) then
        AErrorMessages.Add(lMessage)
      else
        Result := true;
  except on E: Exception do HandleError(E, OPNAME)  end;
end;
function TPlanningOpenCast.GrowthFactorByType(AType: integer): TPlanningMineGrowthFactor;
const OPNAME = 'TPlanningOpenCast.GrowthFactorByType';
var
  LIndex : integer;
  LTempGrowthFactorObject :   TPlanningMineGrowthFactor;
begin
  Result := nil;
  try
    for LIndex := 0 to FGrowthFactorList.Count - 1 do
    begin
      LTempGrowthFactorObject := TPlanningMineGrowthFactor(FGrowthFactorList[LIndex]);
      if LTempGrowthFactorObject.FactorType = AType then
        Result := LTempGrowthFactorObject;
    end;
  except on E: Exception do HandleError(E,OPNAME); end;
end;

function  TPlanningOpenCast.GrowthFactorByIndex(AIndex:integer): TPlanningMineGrowthFactor;
const OPNAME = 'TPlanningOpenCast.GrowthFactorByType';
begin
  Result := nil;
  try
    if (AIndex >= 0) and (FGrowthFactorList.Count > 0) and (AIndex < FGrowthFactorList.Count) then
      Result := TPlanningMineGrowthFactor(FGrowthFactorList[AIndex]);
  except on E: Exception do HandleError(E,OPNAME); end;
end;

end.
