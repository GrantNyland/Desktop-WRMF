//
//
//  UNIT      : Contains TYieldModelIterationTracker Class
//  AUTHOR    : Dziedzi Ramulondi(Cornastone)
//  DATE      : 21/05/2007
//  COPYRIGHT : Copyright © 2007 DWAF
//
//
unit UYieldModelIterationTracker;

interface

uses
  Classes,
  UAbstractObject,
  VoaimsCom_TLB;

type

  TIterationEventHandler = class(TInterfacedObject,IIterationEventHandler)
  public
    function OnIterationEvent(const AIterationName: WideString): WordBool; safecall;
  end;

  TYieldModelIterationTracker = class(TAbstractAppObject,IYieldModelIterationTracker)
  protected

    // Iteration counts.
    FIntervalCount: integer;
    FMonthCount: integer;
    FYearCount: integer;
    FTargetDraftCount: integer;
    FSequenceCount: integer;

    // Current iteration indexes.
    FCurrentMonth: integer;
    FCurrentInterval: integer;
    FCurrentYearGregorian: integer;
    FCurrentYearIndex: integer;
    FCurrentTargetDraft: integer;
    FCurrentSequence: integer;
    FCurrentGood: integer;

    // Control variables.
    FSimulationInProgress: boolean;
    FAbort: boolean;

    // Event handler.
    FIterationEventHandler: IIterationEventHandler;

    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;

    function Get_IntervalCount: Integer; safecall;
    procedure Set_IntervalCount(Value: Integer); safecall;
    function Get_MonthCount: Integer; safecall;
    procedure Set_MonthCount(Value: Integer); safecall;
    function Get_YearCount: Integer; safecall;
    procedure Set_YearCount(Value: Integer); safecall;
    function Get_TargetDraftCount: Integer; safecall;
    procedure Set_TargetDraftCount(Value: Integer); safecall;
    function Get_SequenceCount: Integer; safecall;
    procedure Set_SequenceCount(Value: Integer); safecall;
    function Get_CurrentMonth: Integer; safecall;
    procedure Set_CurrentMonth(Value: Integer); safecall;
    function Get_CurrentInterval: Integer; safecall;
    procedure Set_CurrentInterval(Value: Integer); safecall;
    function Get_CurrentYearGregorian: Integer; safecall;
    procedure Set_CurrentYearGregorian(Value: Integer); safecall;
    function Get_CurrentYearIndex: Integer; safecall;
    procedure Set_CurrentYearIndex(Value: Integer); safecall;
    function Get_CurrentTargetDraft: Integer; safecall;
    procedure Set_CurrentTargetDraft(Value: Integer); safecall;
    function Get_CurrentSequence: Integer; safecall;
    procedure Set_CurrentSequence(Value: Integer); safecall;
    function Get_CurrentGood: Integer; safecall;
    procedure Set_CurrentGood(Value: Integer); safecall;
    function Get_Abort: WordBool; safecall;
    procedure Set_Abort(Value: WordBool); safecall;
    function Get_PreviousMonth: Integer; safecall;
    procedure Set_PreviousMonth(Value: Integer); safecall;
    function Get_SubsequentMonth: Integer; safecall;
    procedure Set_SubsequentMonth(Value: Integer); safecall;
    function Get_SimulationInProgress: WordBool; safecall;
    procedure Set_SimulationInProgress(Value: WordBool); safecall;
    function Get_IterationEventHandler: IIterationEventHandler; safecall;
    procedure Set_IterationEventHandler(const Value: IIterationEventHandler); safecall;
  public
    function Initialise: boolean; override;
    procedure SaveToINI; safecall;
    property IntervalCount     : Integer read Get_IntervalCount write Set_IntervalCount;
    property MonthCount        : Integer read Get_MonthCount write Set_MonthCount;
    property YearCount         : Integer read Get_YearCount write Set_YearCount;
    property TargetDraftCount  : Integer read Get_TargetDraftCount write Set_TargetDraftCount;
    property SequenceCount     : Integer read Get_SequenceCount write Set_SequenceCount;
    property CurrentMonth      : Integer read Get_CurrentMonth write Set_CurrentMonth;
    property CurrentInterval   : Integer read Get_CurrentInterval write Set_CurrentInterval;
    property CurrentYearGregorian       : Integer read Get_CurrentYearGregorian write Set_CurrentYearGregorian;
    property CurrentYearIndex  : Integer read Get_CurrentYearIndex write Set_CurrentYearIndex;
    property CurrentTargetDraft: Integer read Get_CurrentTargetDraft write Set_CurrentTargetDraft;
    property CurrentSequence   : Integer read Get_CurrentSequence write Set_CurrentSequence;
    property CurrentGood       : Integer read Get_CurrentGood write Set_CurrentGood;
    property Abort             : WordBool read Get_Abort write Set_Abort;
    property PreviousMonth     : Integer read Get_PreviousMonth write Set_PreviousMonth;
    property SubsequentMonth   : Integer read Get_SubsequentMonth write Set_SubsequentMonth;
    property SimulationInProgress : WordBool read Get_SimulationInProgress write Set_SimulationInProgress;
    property IterationEventHandler: IIterationEventHandler read Get_IterationEventHandler write Set_IterationEventHandler;
  end;

implementation

uses
  SysUtils,
  UErrorHandlingOperations;


{ TIterationEventHandler }

function TYieldModelIterationTracker._AddRef: Integer;
const OPNAME = 'TYieldModelIterationTracker._AddRef';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TYieldModelIterationTracker._Release: Integer;
const OPNAME = 'TYieldModelIterationTracker._Release';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIterationEventHandler.OnIterationEvent(const AIterationName: WideString): WordBool;
const OPNAME = 'TIterationEventHandler.OnIterationEvent';
begin
  Result := False;
  try
    //Do nothing in here
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{ TYieldModelIterationTracker }

procedure TYieldModelIterationTracker.CreateMemberObjects;
const OPNAME = 'TYieldModelIterationTracker.CreateMemberObjects';
begin
  inherited;
  try
    FIntervalCount    := 0;
    FMonthCount       := 0;
    FYearCount        := 0;
    FTargetDraftCount := 0;
    FSequenceCount    := 0;

    FCurrentMonth       := 0;
    FCurrentInterval    := 0;
    FCurrentYearGregorian        := 0;
    FCurrentYearIndex   := 0;
    FCurrentTargetDraft := 0;
    FCurrentSequence    := 0;
    FCurrentGood        := 0;

    FSimulationInProgress := False;
    FAbort                := False;

    FIterationEventHandler := nil;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYieldModelIterationTracker.DestroyMemberObjects;
const OPNAME = 'TYieldModelIterationTracker.DestroyMemberObjects';
begin
  inherited;
  try
    SaveToINI;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldModelIterationTracker.Initialise: boolean;
const OPNAME = 'TYieldModelIterationTracker.Initialise';
begin
  Result := inherited Initialise;
  try
    FIntervalCount    := FAppModules.IniFile.ReadInteger(ClassName,'IntervalCount', 0);
    FMonthCount       := FAppModules.IniFile.ReadInteger(ClassName,'MonthCount', 0);
    FYearCount        := FAppModules.IniFile.ReadInteger(ClassName,'YearCount', 0);
    FTargetDraftCount := FAppModules.IniFile.ReadInteger(ClassName,'TargetDraftCount', 0);
    FSequenceCount    := FAppModules.IniFile.ReadInteger(ClassName,'SequenceCount', 0);

    FCurrentMonth       := FAppModules.IniFile.ReadInteger(ClassName,'CurrentMonth', 0);
    FCurrentInterval    := FAppModules.IniFile.ReadInteger(ClassName,'CurrentInterval', 0);
    FCurrentYearGregorian        := FAppModules.IniFile.ReadInteger(ClassName,'CurrentYearGregorian', 0);
    FCurrentYearIndex   := FAppModules.IniFile.ReadInteger(ClassName,'CurrentYearIndex', 0);
    FCurrentTargetDraft := FAppModules.IniFile.ReadInteger(ClassName,'CurrentTargetDraft', 0);
    FCurrentSequence    := FAppModules.IniFile.ReadInteger(ClassName,'CurrentSequence', 0);
    FCurrentGood        := FAppModules.IniFile.ReadInteger(ClassName,'CurrentGood', 0);

    FSimulationInProgress := Boolean(FAppModules.IniFile.ReadInteger(ClassName,'SimulationInProgress', 0));
    FAbort                := Boolean(FAppModules.IniFile.ReadInteger(ClassName,'Abort', 0));
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYieldModelIterationTracker.SaveToINI;
const OPNAME = 'TYieldModelIterationTracker.SaveToINI';
begin
  inherited;
  try
    FAppModules.IniFile.WriteInteger(ClassName,'IntervalCount', FIntervalCount);
    FAppModules.IniFile.WriteInteger(ClassName,'MonthCount'     , FMonthCount);
    FAppModules.IniFile.WriteInteger(ClassName,'YearCount', FYearCount);
    FAppModules.IniFile.WriteInteger(ClassName,'TargetDraftCount', FTargetDraftCount);
    FAppModules.IniFile.WriteInteger(ClassName,'SequenceCount', FSequenceCount);
    FAppModules.IniFile.WriteInteger(ClassName,'CurrentMonth', FCurrentMonth);
    FAppModules.IniFile.WriteInteger(ClassName,'CurrentInterval', FCurrentInterval);
    FAppModules.IniFile.WriteInteger(ClassName,'CurrentYearGregorian', FCurrentYearGregorian);
    FAppModules.IniFile.WriteInteger(ClassName,'CurrentYearIndex', FCurrentYearIndex);
    FAppModules.IniFile.WriteInteger(ClassName,'CurrentTargetDraft', FCurrentTargetDraft);
    FAppModules.IniFile.WriteInteger(ClassName,'CurrentSequence', FCurrentSequence);
    FAppModules.IniFile.WriteInteger(ClassName,'CurrentGood', FCurrentGood);

    FAppModules.IniFile.WriteInteger(ClassName,'SimulationInProgress', Ord(FSimulationInProgress));
    FAppModules.IniFile.WriteInteger(ClassName,'Abort', Ord(FAbort));
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldModelIterationTracker.Get_PreviousMonth: Integer;
const OPNAME = 'TYieldModelIterationTracker.Get_PreviousMonth';
begin
  Result := 0;
  try
    if (FCurrentMonth <= 1) then
      Result := FMonthCount
    else
      Result := FCurrentMonth - 1;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYieldModelIterationTracker.Set_PreviousMonth(Value: Integer);
const OPNAME = 'TYieldModelIterationTracker.Set_PreviousMonth';
begin
  try
    FCurrentMonth := Value+1;
    if (FCurrentMonth >= FMonthCount) then
      FCurrentMonth := 1;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYieldModelIterationTracker.Set_SubsequentMonth(Value: Integer);
const OPNAME = 'TYieldModelIterationTracker.Set_SubsequentMonth';
begin
  try
    FCurrentMonth := Value-1;
    if (FCurrentMonth >= FMonthCount) then
      FCurrentMonth := 1;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


function TYieldModelIterationTracker.Get_SubsequentMonth: Integer;
const OPNAME = 'TYieldModelIterationTracker.Get_SubsequentMonth';
begin
  Result := 0;
  try
    if (FCurrentMonth >= FMonthCount) then
      Result := 1
    else
      Result := FCurrentMonth + 1;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


function TYieldModelIterationTracker.Get_Abort: WordBool;
const OPNAME = 'TYieldModelIterationTracker.Get_Abort';
begin
  Result := False;
  try
    Result := FAbort;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldModelIterationTracker.Get_CurrentGood: Integer;
const OPNAME = 'TYieldModelIterationTracker.Get_CurrentGood';
begin
  Result := 0;
  try
    Result := FCurrentGood;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldModelIterationTracker.Get_CurrentInterval: Integer;
const OPNAME = 'TYieldModelIterationTracker.Get_CurrentInterval';
begin
  Result := 0;
  try
    Result := FCurrentInterval;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldModelIterationTracker.Get_CurrentMonth: Integer;
const OPNAME = 'TYieldModelIterationTracker.Get_CurrentMonth';
begin
  Result := 0;
  try
    Result := FCurrentMonth;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldModelIterationTracker.Get_CurrentSequence: Integer;
const OPNAME = 'TYieldModelIterationTracker.Get_CurrentSequence';
begin
  Result := 0;
  try
    Result := FCurrentSequence;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldModelIterationTracker.Get_CurrentTargetDraft: Integer;
const OPNAME = 'TYieldModelIterationTracker.Get_CurrentTargetDraft';
begin
  Result := 0;
  try
    Result := FCurrentTargetDraft;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldModelIterationTracker.Get_CurrentYearGregorian: Integer;
const OPNAME = 'TYieldModelIterationTracker.Get_CurrentYearGregorian';
begin
  Result := 0;
  try
    Result := FCurrentYearGregorian;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldModelIterationTracker.Get_CurrentYearIndex: Integer;
const OPNAME = 'TYieldModelIterationTracker.Get_CurrentYearIndex';
begin
  Result := 0;
  try
    Result := FCurrentYearIndex;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldModelIterationTracker.Get_IntervalCount: Integer;
const OPNAME = 'TYieldModelIterationTracker.Get_IntervalCount';
begin
  Result := 0;
  try
    Result := FIntervalCount;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldModelIterationTracker.Get_IterationEventHandler: IIterationEventHandler;
const OPNAME = 'TYieldModelIterationTracker.Get_IterationEventHandler';
begin
  Result := nil;
  try
    Result :=FIterationEventHandler;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldModelIterationTracker.Get_MonthCount: Integer;
const OPNAME = 'TYieldModelIterationTracker.Get_MonthCount';
begin
  Result := 0;
  try
    Result := FMonthCount;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldModelIterationTracker.Get_SequenceCount: Integer;
const OPNAME = 'TYieldModelIterationTracker.Get_SequenceCount';
begin
  Result := 0;
  try
    Result := FSequenceCount;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldModelIterationTracker.Get_SimulationInProgress: WordBool;
const OPNAME = 'TYieldModelIterationTracker.Get_SimulationInProgress';
begin
  Result := False;
  try
    Result := FSimulationInProgress;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldModelIterationTracker.Get_TargetDraftCount: Integer;
const OPNAME = 'TYieldModelIterationTracker.Get_TargetDraftCount';
begin
  Result := 0;
  try
    Result := FTargetDraftCount;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldModelIterationTracker.Get_YearCount: Integer;
const OPNAME = 'TYieldModelIterationTracker.Get_YearCount';
begin
  Result := 0;
  try
    Result := FYearCount;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYieldModelIterationTracker.Set_Abort(Value: WordBool);
const OPNAME = 'TYieldModelIterationTracker.Set_Abort';
begin
  try
    FAbort := Value;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYieldModelIterationTracker.Set_CurrentGood(Value: Integer);
const OPNAME = 'TYieldModelIterationTracker.Set_CurrentGood';
begin
  try
    FCurrentGood := Value;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYieldModelIterationTracker.Set_CurrentInterval(Value: Integer);
const OPNAME = 'TYieldModelIterationTracker.Set_CurrentInterval';
begin
  try
    FCurrentInterval := Value;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYieldModelIterationTracker.Set_CurrentMonth(Value: Integer);
const OPNAME = 'TYieldModelIterationTracker.Set_CurrentMonth';
begin
  try
    FCurrentMonth := Value;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYieldModelIterationTracker.Set_CurrentSequence(Value: Integer);
const OPNAME = 'TYieldModelIterationTracker.Set_CurrentSequence';
begin
  try
    FCurrentSequence := Value;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYieldModelIterationTracker.Set_CurrentTargetDraft(Value: Integer);
const OPNAME = 'TYieldModelIterationTracker.Set_CurrentTargetDraft';
begin
  try
    FCurrentTargetDraft := Value;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYieldModelIterationTracker.Set_CurrentYearGregorian(Value: Integer);
const OPNAME = 'TYieldModelIterationTracker.Set_CurrentYearGregorian';
begin
  try
    FCurrentYearGregorian := Value;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYieldModelIterationTracker.Set_CurrentYearIndex(Value: Integer);
const OPNAME = 'TYieldModelIterationTracker.Set_CurrentYearIndex';
begin
  try
    FCurrentYearIndex := Value;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYieldModelIterationTracker.Set_IntervalCount(Value: Integer);
const OPNAME = 'TYieldModelIterationTracker.Set_IntervalCount';
begin
  try
    FIntervalCount := Value;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYieldModelIterationTracker.Set_IterationEventHandler(const Value: IIterationEventHandler);
const OPNAME = 'TYieldModelIterationTracker.Set_IterationEventHandler';
begin
  try
    FIterationEventHandler := Value;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYieldModelIterationTracker.Set_MonthCount(Value: Integer);
const OPNAME = 'TYieldModelIterationTracker.Set_MonthCount';
begin
  try
    FMonthCount := Value;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYieldModelIterationTracker.Set_SequenceCount(Value: Integer);
const OPNAME = 'TYieldModelIterationTracker.Set_SequenceCount';
begin
  try
    FSequenceCount := Value;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYieldModelIterationTracker.Set_SimulationInProgress(Value: WordBool);
const OPNAME = 'TYieldModelIterationTracker.Set_SimulationInProgress';
begin
  try
    FSimulationInProgress := Value;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYieldModelIterationTracker.Set_TargetDraftCount(Value: Integer);
const OPNAME = 'TYieldModelIterationTracker.Set_TargetDraftCount';
begin
  try
    FTargetDraftCount := Value;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYieldModelIterationTracker.Set_YearCount(Value: Integer);
const OPNAME = 'TYieldModelIterationTracker.Set_YearCount';
begin
  try
    FYearCount := Value;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
