{******************************************************************************}
{*  UNIT      : Contains the class TCurtailmentAndDrought.                              *}
{*  AUTHOR    : Presley Mudau                                                 *}
{*  DATE      : 2007/06/06                                                    *}
{*  COPYRIGHT : Copyright © 2007 DWAF                                         *}
{******************************************************************************}

unit UCurtailmentAndDrought;

interface

uses
  Classes,
  Contnrs,
  UAbstractObject,
  VoaimsCom_TLB;

type

{******************************************************************************}
{* Curtailed Channels                                                         *}
{******************************************************************************}

  TCurtailedChannel = class(TAbstractAppObject,ICurtailedChannel)
  protected
    FIdentifier         : Integer;
    FChannelNumber      : Integer;
    FAllocationFactors  : TAllocationFactorsArray;
    procedure CreateMemberObjects;  override;
    procedure DestroyMemberObjects; override;

    function _AddRef: Integer;      stdcall;
    function _Release: Integer;     stdcall;
  public
    function Initialise : boolean; override;
    function Populate (AIdentifier : integer; AChannelNumber : integer;
                       AAllocationFactors: TAllocationFactorsArray): WordBool;
    function PopulateSome (AIdentifier: integer): WordBool;
    function Get_Identifier: Integer; safecall;
    function Get_ChannelNumber: Integer; safecall;

    function Get_AllocationFactors(AIndex: Integer): Double; safecall;
    procedure Set_AllocationFactors(AIndex: Integer; Value: Double); safecall;
    function Get_AllocationFactorsCount: Integer; safecall;
    procedure Set_AllocationFactorsCount(ACount: Integer); safecall;
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; safecall;


    property Identifier: Integer read Get_Identifier;
    property ChannelNumber: Integer read Get_ChannelNumber;
    property AllocationFactorsCount: Integer read Get_AllocationFactorsCount write Set_AllocationFactorsCount;
    property AllocationFactors[AIndex: integer]: double read Get_AllocationFactors write Set_AllocationFactors;
  end;

  TDroughtRestriction = class(TAbstractAppObject,IDroughtRestriction)
  protected
    FIdentifier        : Integer;
    FName              : string;
    FChannelNumbers    : TStringList;
    FReservoirNumbers  : TStringList;
    FStorageVolumes    : TStorageVolumesArray;
    FAllocationFactors : TAllocationFactorsArray;

    procedure CreateMemberObjects;  override;
    procedure DestroyMemberObjects; override;

    function _AddRef: Integer;      stdcall;
    function _Release: Integer;     stdcall;
  public
    function Initialise : boolean; override;
    procedure Populate (AIdentifier : integer; AName: string; AReservoirNumbers: string; AChannelNumbers:string);
    procedure PopulateAllocationFactors(AAllocationFactors: TAllocationFactorsArray);
    procedure PopulateStorageVolumes (AStorageVolumes: TStorageVolumesArray);
    function Get_Identifier: Integer; safecall;
    function Get_ReservoirCount: Integer; safecall;
    function Get_ChannelCount: Integer; safecall;
    function Get_ChannelNumbers: WideString; safecall;
    procedure Set_ChannelNumbers(const Value: WideString); safecall;
    function Get_ReservoirNumbers: WideString; safecall;
    procedure Set_ReservoirNumbers(const Value: WideString); safecall;
    function Get_DroughtRestrictionName: WideString; safecall;
    procedure Set_DroughtRestrictionName(const Value: WideString); safecall;
    function Get_ReservoirNumberByIndex(AIndex: Integer): Integer; safecall;
    function Get_ChannelNumberByIndex(AIndex: Integer): Integer; safecall;
    function Get_ReferenceStorageVolumes(AIndex: Integer): Double; safecall;
    procedure Set_ReferenceStorageVolumes(AIndex: Integer; Value: Double); safecall;
    function Get_AllocationFactors(AIndex: Integer): Double; safecall;
    procedure Set_AllocationFactors(AIndex: Integer; Value: Double); safecall;
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; safecall;

    property Identifier     : Integer read Get_Identifier;
    property ReservoirCount : Integer read Get_ReservoirCount;
    property ChannelCount   : Integer read Get_ChannelCount;
    property ChannelNumbers    : WideString read Get_ChannelNumbers  write Set_ChannelNumbers;
    property ReservoirNumbers  : WideString read Get_ReservoirNumbers write Set_ReservoirNumbers;
    property DroughtRestrictionName: WideString read Get_DroughtRestrictionName write Set_DroughtRestrictionName;
    property ReservoirNumberByIndex[AIndex: Integer]: Integer read Get_ReservoirNumberByIndex;
    property ChannelNumberByIndex[AIndex: Integer]: Integer read Get_ChannelNumberByIndex;
    property ReferenceStorageVolumes[AIndex : integer] : double
             read Get_ReferenceStorageVolumes write Set_ReferenceStorageVolumes;
    property AllocationFactors[AIndex : integer] : double
             read Get_AllocationFactors write Set_AllocationFactors;
  end;

  TCurtailmentAndDrought = class(TAbstractAppObject, ICurtailmentAndDrought)
  protected
    FCurtailmentPeriodCount   : integer;
    FStartMonthsArray         : TStartMonthArray;
    FCurtailedChannelList     : TObjectList;
    FDroughtRestrictionList   : TObjectList;
    FImplementCurtailmentFile : Boolean;

    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
  public
    function Initialise: boolean; override;
    function Populate(AStartMonthsArray: TStartMonthArray;
                      ACurtailmentPeriodCount: integer;
                      AImplementCurtailmentFile: integer): boolean;

// Curtailment
    function Get_CurtailmentCount: Integer; safecall;
    procedure Set_CurtailmentCount(Value: Integer); safecall;
    function Get_CurtailmentPeriodCount: Integer; safecall;
    procedure Set_CurtailmentPeriodCount(Value: Integer); safecall;
    procedure SetCurtailmentPeriod(Value: Integer);
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; safecall;
    function Get_StartMonthsByIndex(AIndex: Integer): Integer; safecall;
    procedure Set_StartMonthsByIndex(AIndex: Integer; Value: Integer); safecall;
    procedure Set_ImplementCurtailmentFile(Value: Wordbool); safecall;
    function Get_ImplementCurtailmentFile: Wordbool; safecall;

    function CreateChannelCurtailment(AChannelNumber: Integer): ICurtailedChannel; safecall;
    function CreateNewChannelCurtailment(AChannelNumber: Integer): TCurtailedChannel;
    function CreateDroughtRestriction: IDroughtRestriction; safecall;
    function CreateNewDroughtRestriction : TDroughtRestriction;


    function RemoveChannelCurtailment(AChannelNumber: Integer): WordBool; safecall;
    function DeleteCurtailmentChannelByChannelNumber(AChannelNumber : integer) : WordBool;
    function DeleteCurtailmentChannelWithIndex(AIndex : integer) : WordBool;


    function RemoveDroughtRestriction(AIdentifier: Integer): WordBool; safecall;
    function DeleteDroughtRestrictionWithID(AIdentifier : integer) : WordBool;
    function DeleteDroughtRestrictionWithIndex(AIndex : integer) : WordBool;


    function NewDroughtRestriction : TDroughtRestriction;
    function NewCurtailedChannel : TCurtailedChannel;

    //Curtailed Channel
    function Get_CurtailedChannelCount: Integer; safecall;
    function Get_CurtailedChannelByIndex(AIndex: Integer): ICurtailedChannel; safecall;
    function Get_CurtailedChannelByID(AIdentifier: Integer): ICurtailedChannel; safecall;
    function Get_CurtailedChannelByChannelNumber(AChannelNumber: Integer): ICurtailedChannel; safecall;
    function Get_CastCurtailedChannelByIndex(AIndex: Integer): TCurtailedChannel;
    function Get_CastCurtailedChannelByID(AIdentifier: Integer): TCurtailedChannel;
    function Get_CastCurtailedChannelByChannelNumber(AChannelNumber: Integer): TCurtailedChannel;

    // Drought Restriction
    function Get_DroughtRestrictionCount: Integer; safecall;
    function Get_DroughtRestrictionByIndex(AIndex: Integer): IDroughtRestriction; safecall;
    function Get_DroughtRestrictionByID(AIdentifier: Integer): IDroughtRestriction; safecall;
    function Get_CastDroughtRestrictionByIndex(AIndex: Integer): TDroughtRestriction;
    function Get_CastDroughtRestrictionByID(AIdentifier: Integer): TDroughtRestriction;
    function Get_DroughtRestrictionByChannelNumber(AChannelNumber: Integer): WideString; safecall;
    function Get_DroughtRestrictionByReservoirNumber(AReservoirNumber: Integer): WideString; safecall;


    property DroughtRestrictionCount: Integer read Get_DroughtRestrictionCount;
    property DroughtRestrictionByIndex[AIndex: Integer]: IDroughtRestriction read Get_DroughtRestrictionByIndex;
    property DroughtRestrictionByID[AIdentifier: Integer]: IDroughtRestriction read Get_DroughtRestrictionByID;
    property DroughtRestrictionByChannelNumber[AChannelNumber: Integer]: WideString read Get_DroughtRestrictionByChannelNumber;
    property DroughtRestrictionByReservoirNumber[AReservoirNumber: Integer]: WideString read Get_DroughtRestrictionByReservoirNumber;

    property CastDroughtRestrictionByIndex[AIndex: Integer]: TDroughtRestriction read Get_CastDroughtRestrictionByIndex;
    property CastDroughtRestrictionByID[AIdentifier: Integer]:TDroughtRestriction read Get_CastDroughtRestrictionByID;
    property CastCurtailedChannelByIndex[AIndex: Integer]: TCurtailedChannel read Get_CastCurtailedChannelByIndex;
    property CastCurtailedChannelByID[AIdentifier: Integer]:TCurtailedChannel read Get_CastCurtailedChannelByID;
    property CastCurtailedChannelByChannelNumber[AChannelNumber: Integer]:TCurtailedChannel read Get_CastCurtailedChannelByChannelNumber;

    property CurtailmentCount: Integer read Get_CurtailmentCount write Set_CurtailmentCount;
    property CurtailedChannelByIndex[AIndex: Integer]: ICurtailedChannel read Get_CurtailedChannelByIndex;
    property CurtailedChannelByID[ACurtailedChannelID: Integer]: ICurtailedChannel read Get_CurtailedChannelByID;
    property StartMonthsByIndex[AIndex: Integer]: Integer read Get_StartMonthsByIndex write Set_StartMonthsByIndex;
    property CurtailmentPeriodCount: Integer read Get_CurtailmentPeriodCount write Set_CurtailmentPeriodCount;
    property CurtailedChannelByChannelNumber[AChannelNumber: Integer]: ICurtailedChannel read Get_CurtailedChannelByChannelNumber;
    property IncludeInSummary: WordBool read Get_ImplementCurtailmentFile write Set_ImplementCurtailmentFile;


  end;

implementation

uses
  SysUtils,
  Math,
  UConstants,
  UNetworkFeaturesSQLAgent,
  UYieldModelDataObject,
  UErrorHandlingOperations;

{ TCurtailmentAndDrought }

function TCurtailmentAndDrought.CreateChannelCurtailment(AChannelNumber: Integer): ICurtailedChannel;
const OPNAME = 'TCurtailmentAndDrought.CreateChannelCurtailment';
begin
  try
    Result := CreateNewChannelCurtailment(AChannelNumber);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TCurtailmentAndDrought.CreateNewChannelCurtailment(AChannelNumber: Integer): TCurtailedChannel;
const OPNAME = 'TCurtailmentAndDrought.CreateNewChannelCurtailment';
var
  lLoadAgent          : TNetworkFeaturesSQLAgent;
  LCurtailedChannel   : TCurtailedChannel;
begin
  Result := nil;
  try
    if (FCurtailedChannelList.Count >= 50) then
      Exit;

    LLoadAgent := TNetworkFeaturesSQLAgent.Create(FAppModules);
    try
      LCurtailedChannel := TCurtailedChannel.Create(FAppModules);
      try
        LCurtailedChannel.Initialise;
        LCurtailedChannel.FChannelNumber := AChannelNumber;

        if LLoadAgent.InsertCurtailmentChannel(LCurtailedChannel) then
        begin
           FCurtailedChannelList.Add(LCurtailedChannel);
           Result := LCurtailedChannel;
        end
        else
        begin
          FreeAndNil(LCurtailedChannel);
        end;
      except
        FreeAndNil(LCurtailedChannel);
      end;
    finally
      FreeAndNil(LLoadAgent);
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TCurtailmentAndDrought.CreateDroughtRestriction: IDroughtRestriction;
const OPNAME = 'TCurtailmentAndDrought.CreateDroughtRestriction';
begin
  Result := nil;
  try
    Result := CreateNewDroughtRestriction;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TCurtailmentAndDrought.CreateNewDroughtRestriction: TDroughtRestriction;
const OPNAME = 'TCurtailmentAndDrought.CreateNewDroughtRestriction';
var
  LNewIdentifier      : integer;
  lLoadAgent          : TNetworkFeaturesSQLAgent;
  LDroughtRestriction : TDroughtRestriction;
begin
  Result := nil;
  if FDroughtRestrictionList.Count >= 20 then
    Exit;
  try
    LLoadAgent := TNetworkFeaturesSQLAgent.Create(FAppModules);
    try
      LDroughtRestriction := TDroughtRestriction.Create(FAppModules);
      try
        LNewIdentifier := lLoadAgent.GetMaxDroughtRestrictionID + 1;
        LDroughtRestriction := NewDroughtRestriction;

        if (LDroughtRestriction <> nil) then
        begin
          LDroughtRestriction.FIdentifier := LNewIdentifier;
          LDroughtRestriction.FName := 'DROUGHT RESTRICTION '+IntToStr(LNewIdentifier);
          if (lLoadAgent.InsertDroughtRestriction(LDroughtRestriction)) then
            Result := LDroughtRestriction;
        end
      except
        FreeAndNil(LDroughtRestriction);
      end;
    finally
      FreeAndNil(LLoadAgent);
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;


procedure TCurtailmentAndDrought.CreateMemberObjects;
const OPNAME = 'TCurtailmentAndDrought.CreateMemberObjects';
begin
  try
    FCurtailedChannelList    := TObjectList.Create(True);
    FDroughtRestrictionList  := TObjectList.Create(True);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TCurtailmentAndDrought.DestroyMemberObjects;
const OPNAME = 'TCurtailmentAndDrought.DestroyMemberObjects';
begin
  try
    FreeAndNil(FCurtailedChannelList);
    FreeAndNil(FDroughtRestrictionList);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TCurtailmentAndDrought._AddRef: Integer;
const OPNAME = 'TCurtailmentAndDrought._AddRef';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TCurtailmentAndDrought._Release: Integer;
const OPNAME = 'TCurtailmentAndDrought._Release';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TCurtailmentAndDrought.Get_CastDroughtRestrictionByID(AIdentifier: Integer): TDroughtRestriction;
const OPNAME = 'TCurtailmentAndDrought.Get_CastDroughtRestrictionByID';
var
  LIndex : Integer;
  LDroughtRestriction  : TDroughtRestriction;
begin
  Result := nil;
  try
    LIndex := 0;
    while ((Result = nil) and (LIndex < FDroughtRestrictionList.Count)) do
    begin
      LDroughtRestriction := TDroughtRestriction(FDroughtRestrictionList.Items[LIndex]);
      if (LDroughtRestriction.Identifier = AIdentifier) then
        Result := LDroughtRestriction
      else
        LIndex := LIndex + 1;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TCurtailmentAndDrought.Get_CastDroughtRestrictionByIndex(AIndex: Integer): TDroughtRestriction;
const OPNAME = 'TCurtailmentAndDrought.Get_CastDroughtRestrictionByIndex';
begin
  Result := nil;
  try
    if(AIndex >= 0) and (AIndex < FDroughtRestrictionList.Count) then
      Result := TDroughtRestriction(FDroughtRestrictionList[AIndex]);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TCurtailmentAndDrought.Get_CurtailedChannelByChannelNumber(AChannelNumber: Integer): ICurtailedChannel;
const OPNAME = 'TCurtailmentAndDrought.Get_CurtailedChannelByChannelNumber';
begin
  Result := nil;
  try
    Result := Get_CastCurtailedChannelByChannelNumber(AChannelNumber);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TCurtailmentAndDrought.Get_CurtailedChannelByID(AIdentifier: Integer): ICurtailedChannel;
const OPNAME = 'TCurtailmentAndDrought.Get_CurtailedChannelByID';
begin
  Result := nil;
  try
    Result := CastCurtailedChannelByID[AIdentifier];
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TCurtailmentAndDrought.Get_CurtailedChannelByIndex(AIndex: Integer): ICurtailedChannel;
const OPNAME = 'TCurtailmentAndDrought.Get_CurtailedChannelByIndex';
begin
  Result := nil;
  try
    Result := CastCurtailedChannelByIndex[AIndex] ;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TCurtailmentAndDrought.Get_CurtailedChannelCount: Integer;
const OPNAME = 'TCurtailmentAndDrought.Get_CurtailedChannelCount';
begin
  Result := NullInteger;
  try
    Result := FCurtailedChannelList.Count;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TCurtailmentAndDrought.Get_CurtailmentCount: Integer;
const OPNAME = 'TCurtailmentAndDrought.Get_CurtailmentCount';
begin
  Result := NullInteger;
  try
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TCurtailmentAndDrought.Get_CurtailmentPeriodCount: Integer;
const OPNAME = 'TCurtailmentAndDrought.Get_CurtailmentPeriodCount';
begin
  Result := NullInteger;
  try
    Result := FCurtailmentPeriodCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TCurtailmentAndDrought.Get_DroughtRestrictionByID(AIdentifier: Integer): IDroughtRestriction;
const OPNAME = 'TCurtailmentAndDrought.Get_DroughtRestrictionByID';
begin
  Result := nil;
  try
    Result := CastDroughtRestrictionByID[AIdentifier];
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TCurtailmentAndDrought.Get_DroughtRestrictionByIndex(AIndex: Integer): IDroughtRestriction;
const OPNAME = 'TCurtailmentAndDrought.Get_DroughtRestrictionByIndex';
begin
  Result := nil;
  try
    Result := CastDroughtRestrictionByIndex[AIndex];
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TCurtailmentAndDrought.Get_DroughtRestrictionCount: Integer;
const OPNAME = 'TCurtailmentAndDrought.Get_DroughtRestrictionCount';
begin
  Result := NullInteger;
  try
    Result := FDroughtRestrictionList.Count;
  except on E: Exception do HandleError(E, OPNAME); end;
end;


function TCurtailmentAndDrought.Get_StartMonthsByIndex(AIndex: Integer): Integer;
const OPNAME = 'TCurtailmentAndDrought.Get_StartMonthsByIndex';
begin
  Result := NullInteger;
  try
    Result := FStartMonthsArray[AIndex];
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TCurtailmentAndDrought.Initialise: boolean;
const OPNAME = 'TCurtailmentAndDrought.Initialise';
var
  LIndex : integer;
begin
  Result := inherited Initialise;
  try
    for LIndex := Low(FStartMonthsArray) to High(FStartMonthsArray) do
      FStartMonthsArray[LIndex] := NullInteger;

    FCurtailmentPeriodCount := 0;
    FImplementCurtailmentFile := False;
    FCurtailedChannelList.Clear;
    FDroughtRestrictionList.Clear;
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TCurtailmentAndDrought.Populate(AStartMonthsArray: TStartMonthArray;
                                         ACurtailmentPeriodCount: integer;
                                         AImplementCurtailmentFile: integer): boolean;
const OPNAME = 'TCurtailmentAndDrought.Populate';
var
  LIndex : integer;
  LStartMonth : TAbstractFieldProperty;
begin
  Result := False;
  try
    LStartMonth := FAppModules.FieldProperties.FieldProperty('CurtailmentStartMonthValue');
    for LIndex := LStartMonth.ArrayLow to LStartMonth.ArrayHigh do
      FStartMonthsArray[LIndex] := AStartMonthsArray[LIndex];

    FCurtailmentPeriodCount := ACurtailmentPeriodCount;

    if AImplementCurtailmentFile = 1 then
       FImplementCurtailmentFile := True
    else
       FImplementCurtailmentFile := False;

    Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TCurtailmentAndDrought.RemoveChannelCurtailment(AChannelNumber: Integer): WordBool;
const OPNAME = 'TCurtailmentAndDrought.RemoveChannelCurtailment';
var
  lLoadAgent : TNetworkFeaturesSQLAgent;
begin
  Result := FALSE;
  try
    if (AChannelNumber > 0) then
    begin
      LLoadAgent := TNetworkFeaturesSQLAgent.Create(FAppModules);
      try
        if LLoadAgent.DeleteCurtailmentChannel(AChannelNumber) then
        begin
          DeleteCurtailmentChannelByChannelNumber(AChannelNumber);
          Result := TRUE;
        end;
      finally
        LLoadAgent.Free;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TCurtailmentAndDrought.DeleteCurtailmentChannelByChannelNumber(AChannelNumber: integer): WordBool;
const OPNAME = 'TCurtailmentAndDrought.DeleteCurtailmentChannelByChannelNumber';
var
  lCurtailedChannel : TCurtailedChannel;
  LIndex            : integer;
begin
  Result := FALSE;
  try
    lCurtailedChannel := CastCurtailedChannelByChannelNumber[AChannelNumber];
    if (lCurtailedChannel <> nil) then
    begin
      LIndex := FCurtailedChannelList.IndexOf(lCurtailedChannel);
      Result := DeleteCurtailmentChannelWithIndex(LIndex);
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TCurtailmentAndDrought.DeleteCurtailmentChannelWithIndex(AIndex: integer): WordBool;
const OPNAME = 'TCurtailmentAndDrought.DeleteCurtailmentChannelWithIndex';
begin
  Result := FALSE;
  try
    if (AIndex >= 0) then
    begin
      FCurtailedChannelList.Delete(AIndex);
      Result := TRUE;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TCurtailmentAndDrought.RemoveDroughtRestriction(AIdentifier: Integer): WordBool;
const OPNAME = 'TCurtailmentAndDrought.RemoveDroughtRestriction';
var
  lLoadAgent : TNetworkFeaturesSQLAgent;
begin
  Result := FALSE;
  try
    if (AIdentifier > 0) then
    begin
      LLoadAgent := TNetworkFeaturesSQLAgent.Create(FAppModules);
      try
        if LLoadAgent.DeleteDroughtRestriction(AIdentifier) then
        begin
          DeleteDroughtRestrictionWithID(AIdentifier);
          Result := TRUE;
        end;
      finally
        LLoadAgent.Free;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TCurtailmentAndDrought.DeleteDroughtRestrictionWithID(AIdentifier: integer): WordBool;
const OPNAME = 'TCurtailmentAndDrought.DeleteDroughtRestrictionWithID';
var
  LDroughtRestriction : TDroughtRestriction;
  LIndex              : integer;
begin
  Result := FALSE;
  try
    LDroughtRestriction := CastDroughtRestrictionByID[AIdentifier];
    if (LDroughtRestriction <> nil) then
    begin
      LIndex := FDroughtRestrictionList.IndexOf(LDroughtRestriction);
      Result := DeleteDroughtRestrictionWithIndex(LIndex);
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TCurtailmentAndDrought.DeleteDroughtRestrictionWithIndex(AIndex: integer): WordBool;
const OPNAME = 'TCurtailmentAndDrought.DeleteDroughtRestrictionWithIndex';
begin
  Result := FALSE;
  try
    if (AIndex >= 0) then
    begin
      FDroughtRestrictionList.Delete(AIndex);
      Result := TRUE;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TCurtailmentAndDrought.NewCurtailedChannel: TCurtailedChannel;
const OPNAME = 'TCurtailmentAndDrought.NewCurtailedChannel';
begin
  Result := nil;
  try
    Result := TCurtailedChannel.Create(FAppModules);
    FCurtailedChannelList.Add(Result);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TCurtailmentAndDrought.NewDroughtRestriction: TDroughtRestriction;
const OPNAME = 'TCurtailmentAndDrought.NewDroughtRestriction';
begin
  Result := nil;
  try
    Result := TDroughtRestriction.Create(FAppModules);
    FDroughtRestrictionList.Add(Result);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TCurtailmentAndDrought.Get_CastCurtailedChannelByID(AIdentifier: Integer): TCurtailedChannel;
const OPNAME = 'TCurtailmentAndDrought.Get_CastCurtailedChannelByID';
var
  LIndex : Integer;
  LCurtailedChannel  : TCurtailedChannel;
begin
  Result := nil;
  try
    LIndex := 0;
    while ((Result = nil) and (LIndex < FCurtailedChannelList.Count)) do
    begin
      LCurtailedChannel := TCurtailedChannel(FCurtailedChannelList.Items[LIndex]);
      if (LCurtailedChannel.Identifier = AIdentifier) then
        Result := LCurtailedChannel
      else
        LIndex := LIndex + 1;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TCurtailmentAndDrought.Get_CastCurtailedChannelByIndex(AIndex: Integer): TCurtailedChannel;
const OPNAME = 'TCurtailmentAndDrought.Get_CastCurtailedChannelByIndex';
begin
  Result := nil;
  try
    if(AIndex >= 0) and (AIndex < FCurtailedChannelList.Count) then
      Result := TCurtailedChannel(FCurtailedChannelList[AIndex]);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TCurtailmentAndDrought.Get_CastCurtailedChannelByChannelNumber(AChannelNumber: Integer): TCurtailedChannel;
const OPNAME = 'TCurtailmentAndDrought.Get_CastCurtailedChannelByChannelNumber';
var
  LIndex : Integer;
  LCurtailedChannel  : TCurtailedChannel;
begin
  Result := nil;
  try
    LIndex := 0;
    while ((Result = nil) and (LIndex < FCurtailedChannelList.Count)) do
    begin
      LCurtailedChannel := TCurtailedChannel(FCurtailedChannelList.Items[LIndex]);
      if (LCurtailedChannel.ChannelNumber = AChannelNumber) then
        Result := LCurtailedChannel
      else
        LIndex := LIndex + 1;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;


function TCurtailmentAndDrought.Validate(var AErrors: WideString;const AContext: WideString): WordBool;
const OPNAME = 'TCurtailmentAndDrought.Validate';
begin

end;


procedure TCurtailmentAndDrought.Set_CurtailmentCount(Value: Integer);
const OPNAME = 'TCurtailmentAndDrought.Set_CurtailmentCount';
begin

end;

procedure TCurtailmentAndDrought.Set_CurtailmentPeriodCount(Value: Integer);
const OPNAME = 'TCurtailmentAndDrought.Set_CurtailmentPeriodCount';
begin
  try
    SetCurtailmentPeriod(Value)
  except on E: Exception do HandleError(E, OPNAME); end;
end;


procedure TCurtailmentAndDrought.Set_StartMonthsByIndex(AIndex,Value: Integer);
const OPNAME = 'TCurtailmentAndDrought.Set_StartMonthsByIndex';
var
  LLoadAgent: TNetworkFeaturesSQLAgent;
  LContextData: TStringList;
begin
  try
    LLoadAgent := TNetworkFeaturesSQLAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        LLoadAgent.LoadCurtailmentDataContextData(LContextData, IntToStr(AIndex));
        if FAppModules.FieldProperties.UpdateFieldValue(
          'CurtailmentStartMonthValue', IntToStr(Value), IntToStr(FStartMonthsArray[AIndex]), LContextData) then
        begin
          FStartMonthsArray[AIndex] := Value;
        end;
      finally
        LContextData.Free;
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TCurtailmentAndDrought.SetCurtailmentPeriod(Value: Integer);
const OPNAME = 'TCurtailmentAndDrought.SetCurtailmentPeriod';
var
  LLoadAgent   : TNetworkFeaturesSQLAgent;
  LContextData : TStringList;
  LOldValue    : integer;
  LIndex       : integer;
begin
  try
    LLoadAgent := TNetworkFeaturesSQLAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        LLoadAgent.LoadContextData(LContextData);
        if FCurtailmentPeriodCount > 0 then
        begin
          LOldValue := FCurtailmentPeriodCount;
          if Value > 0 then
          begin
            if FAppModules.FieldProperties.UpdateFieldValue(
                 'CurtailmentPeriodCount', IntToStr(Value), IntToStr(FCurtailmentPeriodCount), LContextData) then
            begin
              FCurtailmentPeriodCount := Value;
              FAppModules.Model.StudyDataHasChanged(sdccEdit,'CurtailmentPeriodCount',IntToStr(LOldValue), IntToStr(Value));
            end;

            if Value > LOldValue then
            begin
              for LIndex := LOldValue+1 to Value do
                StartMonthsByIndex[LIndex] := 1;
            end;

            if Value < LOldValue then
            begin
              for LIndex := LOldValue downto Value+1 do
                StartMonthsByIndex[LIndex] := NullInteger;
            end;
          end
          else
          begin
            if LLoadAgent.DeleteCurtailment then
             Initialise;
          end;
        end
        else
        begin
          if LLoadAgent.InsertCurtailment(Value) then
          begin
            FCurtailmentPeriodCount := Value;
            FImplementCurtailmentFile := True;
            for LIndex := 1 to Value do
              StartMonthsByIndex[LIndex] := 1;
          end;
        end
      finally
        LContextData.Free;
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E : Exception do HandleError(E, OPNAME); end;
end;

function TCurtailmentAndDrought.Get_DroughtRestrictionByChannelNumber(AChannelNumber: Integer): WideString; safecall;
const OPNAME = 'TCurtailmentAndDrought.Get_DroughtRestrictionByChannelNumber';
var
  LCount : integer;
  LIndex                  : Integer;
  LDroughtRestriction     : TDroughtRestriction;
  LChannelNumberContainer : TStringList;
  LIdentifierContainer    : TStringList;
begin
  Result := '';
  try
    LChannelNumberContainer := TStringList.Create;
    LIdentifierContainer := TStringList.Create;
    try
      for LIndex := 0 to FDroughtRestrictionList.Count-1 do
      begin
        LDroughtRestriction := TDroughtRestriction(FDroughtRestrictionList.Items[LIndex]);
        LChannelNumberContainer.CommaText := LDroughtRestriction.FChannelNumbers.CommaText;
        if LChannelNumberContainer.Count > 0 then
        begin
          for LCount := 0 to LChannelNumberContainer.Count-1 do
          begin
            if AChannelNumber = StrToInt(LChannelNumberContainer[LCount]) then
               LIdentifierContainer.Add(IntToStr(LDroughtRestriction.FIdentifier));
          end;
        end;
      end;
      Result := LIdentifierContainer.CommaText;
    finally
      LChannelNumberContainer.Free;
      LIdentifierContainer.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TCurtailmentAndDrought.Get_DroughtRestrictionByReservoirNumber(AReservoirNumber: Integer): WideString; safecall;
const OPNAME = 'TCurtailmentAndDrought.Get_DroughtRestrictionByReservoirNumber';
var
  LCount : integer;
  LIndex                    : Integer;
  LDroughtRestriction       : TDroughtRestriction;
  LReservoirNumberContainer : TStringList;
  LIdentifierContainer      : TStringList;
begin
  Result := '';
  try
    LReservoirNumberContainer := TStringList.Create;
    LIdentifierContainer      := TStringList.Create;
    try
      for LIndex := 0 to FDroughtRestrictionList.Count-1 do
      begin
        LDroughtRestriction := TDroughtRestriction(FDroughtRestrictionList.Items[LIndex]);
        LReservoirNumberContainer.CommaText := LDroughtRestriction.FReservoirNumbers.CommaText;
        if LReservoirNumberContainer.Count > 0 then
        begin
          for LCount := 0 to LReservoirNumberContainer.Count-1 do
          begin
            if AReservoirNumber = StrToInt(LReservoirNumberContainer[LCount]) then
               LIdentifierContainer.Add(IntToStr(LDroughtRestriction.FIdentifier));
          end;
        end;
      end;
      Result := LIdentifierContainer.CommaText;
    finally
      LReservoirNumberContainer.Free;
      LIdentifierContainer.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TCurtailmentAndDrought.Get_ImplementCurtailmentFile: WordBool;
const OPNAME = 'TCurtailmentAndDrought.Get_ImplementCurtailmentFile';
begin
  Result := False;
  try
    Result := FImplementCurtailmentFile;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TCurtailmentAndDrought.Set_ImplementCurtailmentFile(Value: WordBool);
const OPNAME = 'TCurtailmentAndDrought.Set_ImplementCurtailmentFile';
var
  LLoadAgent   : TNetworkFeaturesSQLAgent;
  LContextData : TStringList;
  LOldValue    : string;
  LNewValue    : string;
begin
  try
    if FImplementCurtailmentFile then
      LOldValue := '1'
    else
      LOldValue := '0';

    if Value then
      LNewValue := '1'
    else
      LNewValue := '0';

    LLoadAgent := TNetworkFeaturesSQLAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        LLoadAgent.LoadContextData(LContextData);
        if FAppModules.FieldProperties.UpdateFieldValue(
             'CurtailmentFileCreate', LNewValue, LOldValue, LContextData) then
        begin
          FImplementCurtailmentFile := Value;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'CurtailmentFileCreate',LOldValue, LNewValue);
        end;
      finally
        LContextData.Free;
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E : Exception do HandleError(E, OPNAME); end;
end;

{ TCurtailedChannel }

function TCurtailedChannel._AddRef: Integer;
const OPNAME = 'TCurtailedChannel._AddRef';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TCurtailedChannel._Release: Integer;
const OPNAME = 'TCurtailedChannel._Release';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TCurtailedChannel.CreateMemberObjects;
const OPNAME = 'TCurtailedChannel.CreateMemberObjects';
begin
  inherited;
  try
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TCurtailedChannel.DestroyMemberObjects;
const OPNAME = 'TCurtailedChannel.DestroyMemberObjects';
begin
  inherited;
  try
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TCurtailedChannel.Get_AllocationFactors(AIndex: Integer): Double; safecall;
const OPNAME = 'TCurtailedChannel.Get_AllocationFactors';
begin
  Result := NullFloat;
  try
    Result := FAllocationFactors[AIndex];
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TCurtailedChannel.Get_AllocationFactorsCount: Integer;
const OPNAME = 'TCurtailedChannel.Get_AllocationFactorsCount';
begin
  Result := 0;
  try
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TCurtailedChannel.Get_ChannelNumber: Integer;
const OPNAME = 'TCurtailedChannel.Get_ChannelNumber';
begin
  Result := 0;
  try
    Result := FChannelNumber;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TCurtailedChannel.Get_Identifier: Integer;
const OPNAME = 'TCurtailedChannel.Get_Identifier';
begin
  Result := 0;
  try
    Result := FIdentifier;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TCurtailedChannel.Initialise: boolean;
const OPNAME = 'TCurtailedChannel.Initialise';
var
  LIndex : integer;
begin
  Result := inherited Initialise;
  try
    FIdentifier := 0;
    FChannelNumber := 0;

    for LIndex := Low(FAllocationFactors) to High(FAllocationFactors) do
      FAllocationFactors[LIndex] := 0.0;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TCurtailedChannel.Populate(AIdentifier, AChannelNumber: integer;
                                   AAllocationFactors: TAllocationFactorsArray): WordBool;
const OPNAME = 'TCurtailedChannel.Populate';
var
  LIndex : integer;
  LAllocationFactors : TAbstractFieldProperty;
begin
  Result := False;
  try
    FIdentifier := AIdentifier;
    FChannelNumber := AChannelNumber;

    LAllocationFactors:= FAppModules.FieldProperties.FieldProperty('CurtailmentFactors');
    for LIndex := LAllocationFactors.ArrayLow to LAllocationFactors.ArrayHigh do
      FAllocationFactors[LIndex] := AAllocationFactors[LIndex];

    Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TCurtailedChannel.Validate(var AErrors: WideString;const AContext: WideString): WordBool;
const OPNAME = 'TCurtailedChannel.Validate';
begin

end;

procedure TCurtailedChannel.Set_AllocationFactors(AIndex: Integer;Value: Double);
const OPNAME = 'TCurtailedChannel.Set_AllocationFactors';
var
  LLoadAgent   : TNetworkFeaturesSQLAgent;
  LContextData : TStringList;
begin
  try
    LLoadAgent := TNetworkFeaturesSQLAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        LLoadAgent.LoadContextData_DroughtRestrictionIdentifier(LContextData, IntToStr(FIdentifier),IntToStr(AIndex));
        if FAppModules.FieldProperties.UpdateFieldValue(
          'CurtailmentFactors', FloatToStr(Value), FloatToStr(FAllocationFactors[AIndex]), LContextData) then
        begin
          FAllocationFactors[AIndex] := Value;
        end;
      finally
        LContextData.Free;
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TCurtailedChannel.Set_AllocationFactorsCount(ACount: Integer);
const OPNAME = 'TCurtailedChannel.Set_AllocationFactorsCount';
begin

end;

function TCurtailedChannel.PopulateSome(AIdentifier: integer): WordBool;
const OPNAME = 'TCurtailedChannel.PopulateSome';
begin
  Result := False;
  try
    FIdentifier := AIdentifier;
    Result := True
  except on E: Exception do HandleError(E, OPNAME); end;
end;

{ TDroughtRestriction }

function TDroughtRestriction._AddRef: Integer;
const OPNAME = 'TDroughtRestriction._AddRef';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TDroughtRestriction._Release: Integer;
const OPNAME = 'TDroughtRestriction._Release';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TDroughtRestriction.CreateMemberObjects;
const OPNAME = 'TDroughtRestriction.CreateMemberObjects';
begin
  inherited;
  try
    FChannelNumbers   := TStringList.Create;
    //FChannelNumbers.Sorted := true;
    //FChannelNumbers.Duplicates := dupIgnore;

    FReservoirNumbers := TStringList.Create;
    //FReservoirNumbers.Sorted := true;
    //FReservoirNumbers.Duplicates := dupIgnore;

  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TDroughtRestriction.DestroyMemberObjects;
const OPNAME = 'TDroughtRestriction.DestroyMemberObjects';
begin
  inherited;
  try
    FChannelNumbers.Free;
    FReservoirNumbers.free;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TDroughtRestriction.Get_AllocationFactors(AIndex: Integer): Double;
const OPNAME = 'TDroughtRestriction.Get_AllocationFactors';
begin
  Result := NullFloat;
  try
    Result := FAllocationFactors[AIndex];
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TDroughtRestriction.Get_ChannelCount: Integer;
const OPNAME = 'TDroughtRestriction.Get_ChannelCount';
begin
  Result := NullInteger;
  try
    Result := FChannelNumbers.Count;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TDroughtRestriction.Get_ChannelNumberByIndex(AIndex: Integer): Integer;
const OPNAME = 'TDroughtRestriction.Get_ChannelNumberByIndex';
begin
  Result := -1;
  try
    Result := StrToInt(FChannelNumbers.Strings[AIndex]);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TDroughtRestriction.Get_ChannelNumbers: WideString;
const OPNAME = 'TDroughtRestriction.Get_ChannelNumbers';
begin
  Result := '';
  try
    Result := FChannelNumbers.CommaText;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TDroughtRestriction.Get_DroughtRestrictionName: WideString;
const OPNAME = 'TDroughtRestriction.Get_DroughtRestrictionName';
begin
  Result := '';
  try
    Result := FName;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TDroughtRestriction.Get_Identifier: Integer;
const OPNAME = 'TDroughtRestriction.Get_Identifier';
begin
  Result := NullInteger;
  try
    Result := FIdentifier;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TDroughtRestriction.Get_ReferenceStorageVolumes(AIndex: Integer): Double;
const OPNAME = 'TDroughtRestriction.Get_ReferenceStorageVolumes';
begin
  Result := NullFloat;
  try
    Result := FStorageVolumes[AIndex];
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TDroughtRestriction.Get_ReservoirCount: Integer;
const OPNAME = 'TDroughtRestriction.Get_ReservoirCount';
begin
  Result := NullInteger;
  try
    Result := FReservoirNumbers.Count;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TDroughtRestriction.Get_ReservoirNumberByIndex(AIndex: Integer): Integer;
const OPNAME = 'TDroughtRestriction.Get_ReservoirNumberByIndex';
begin
  Result := -1;
  try
    Result := StrToInt(FReservoirNumbers.Strings[AIndex]);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TDroughtRestriction.Get_ReservoirNumbers: WideString;
const OPNAME = 'TDroughtRestriction.Get_ReservoirNumbers';
begin
  Result := '';
  try
    Result := FReservoirNumbers.CommaText;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TDroughtRestriction.Initialise: boolean;
const OPNAME = 'TDroughtRestriction.Initialise';
var
  LIndex: integer;
begin
  Result := inherited Initialise;
  try
    FIdentifier := 0;
    FName := '';
    FChannelNumbers.Clear;
    FReservoirNumbers.Clear;
    for LIndex := Low(FStorageVolumes) to High(FStorageVolumes) do
      FStorageVolumes[LIndex] := 0.0;
    for LIndex := Low(FAllocationFactors) to High(FAllocationFactors) do
      FAllocationFactors[LIndex] := 0.0;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TDroughtRestriction.Populate(AIdentifier: integer; AName,
                                      AReservoirNumbers, AChannelNumbers: string);
const OPNAME = 'TDroughtRestriction.Populate';
begin
  try
    FIdentifier := AIdentifier;
    FName := AName;
    FReservoirNumbers.CommaText := AReservoirNumbers;
    FChannelNumbers.CommaText := AChannelNumbers;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TDroughtRestriction.PopulateAllocationFactors(AAllocationFactors: TAllocationFactorsArray);
const OPNAME = 'TDroughtRestriction.PopulateAllocationFactors';
var
  LIndex : integer;
  LAllocationFactors : TAbstractFieldProperty;
begin
  try
    LAllocationFactors:= FAppModules.FieldProperties.FieldProperty('AllocationFactors');
    for LIndex := LAllocationFactors.ArrayLow to LAllocationFactors.ArrayHigh do
      FAllocationFactors[LIndex] := AAllocationFactors[LIndex];
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TDroughtRestriction.PopulateStorageVolumes(AStorageVolumes: TStorageVolumesArray);
const OPNAME = 'TDroughtRestriction.PopulateStorageVolumes';
var
  LIndex : integer;
  LStorageVolumes : TAbstractFieldProperty;
begin
  try
    LStorageVolumes:= FAppModules.FieldProperties.FieldProperty('ReferenceStorageVolumes');
    for LIndex := LStorageVolumes.ArrayLow to LStorageVolumes.ArrayHigh do
      FStorageVolumes[LIndex] := AStorageVolumes[LIndex];
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TDroughtRestriction.Set_AllocationFactors(AIndex: Integer;Value: Double);
const OPNAME = 'TDroughtRestriction.Set_AllocationFactors';
var
  LLoadAgent   : TNetworkFeaturesSQLAgent;
  LContextData : TStringList;
begin
  try
    LLoadAgent := TNetworkFeaturesSQLAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        LLoadAgent.LoadContextData_DroughtRestrictionIdentifier(LContextData, IntToStr(FIdentifier),IntToStr(AIndex));
        if FAppModules.FieldProperties.UpdateFieldValue(
          'AllocationFactors', FloatToStr(Value), FloatToStr(FAllocationFactors[AIndex]), LContextData) then
        begin
          FAllocationFactors[AIndex] := Value;
        end;
      finally
        LContextData.Free;
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TDroughtRestriction.Set_ChannelNumbers(const Value: WideString);
const OPNAME = 'TDroughtRestriction.Set_ChannelNumbers';
var
  LLoadAgent   : TNetworkFeaturesSQLAgent;
  LContextData : TStringList;
  LChannelStr  : TStringList;
  lOldVal      : string;
  lNewVal      : string;
begin
  try
    LLoadAgent := TNetworkFeaturesSQLAgent.Create(FAppModules);
    LContextData := TStringList.Create;
    try
      LChannelStr  := TStringList.Create;
      try
        LChannelStr.Sorted := true;
        LChannelStr.Duplicates := dupIgnore;
        LChannelStr.CommaText := Value;
        lNewVal               := LChannelStr.CommaText;
      finally
        LChannelStr.Free;
      end;

      if (lNewVal <> FChannelNumbers.CommaText) then
      begin
        lOldVal := FChannelNumbers.CommaText;
        LContextData.Clear;
        LLoadAgent.LoadContextData_FeatureID(LContextData, IntToStr(FIdentifier));
        if (FAppModules.FieldProperties.UpdateFieldValue
             ('DroughtRestrictionChannelNumber', lNewVal, lOldVal, LContextData)) then
        begin
          FChannelNumbers.Clear;
          FChannelNumbers.CommaText := lNewVal;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'DroughtRestrictionChannelNumber', lOldVal, FChannelNumbers.CommaText);
        end;
      end;
    finally
      LContextData.Free;
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TDroughtRestriction.Set_DroughtRestrictionName(const Value: WideString);
const OPNAME = 'TDroughtRestriction.Set_DroughtRestrictionName';
var
  LLoadAgent   : TNetworkFeaturesSQLAgent;
  LContextData : TStringList;
  LOldValue    : string;
begin
  try
    LLoadAgent := TNetworkFeaturesSQLAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        LLoadAgent.LoadContextData_FeatureID(LContextData, IntToStr(FIdentifier));
        if FAppModules.FieldProperties.UpdateFieldValue(
           'DroughtRestrictionName', Value, FName, LContextData) then
        begin
          LOldValue := FName;
          FName := Value;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'DroughtRestrictionName',LOldValue,Value);
        end;
      finally
        LContextData.Free;
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TDroughtRestriction.Set_ReferenceStorageVolumes(AIndex: Integer;Value: Double);
const OPNAME = 'TDroughtRestriction.Set_ReferenceStorageVolumes';
var
  LLoadAgent   : TNetworkFeaturesSQLAgent;
  LContextData : TStringList;
begin
  try
    LLoadAgent := TNetworkFeaturesSQLAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        LLoadAgent.LoadContextData_DroughtRestrictionIdentifier(LContextData, IntToStr(FIdentifier), IntToStr(AIndex));
        if FAppModules.FieldProperties.UpdateFieldValue(
          'ReferenceStorageVolumes', FloatToStr(Value), FloatToStr(FStorageVolumes[AIndex]), LContextData) then
        begin
          FStorageVolumes[AIndex] := Value;
        end;
      finally
        LContextData.Free;
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TDroughtRestriction.Set_ReservoirNumbers(const Value: WideString);
const OPNAME = 'TDroughtRestriction.Set_ReservoirNumbers';
var
  LLoadAgent   : TNetworkFeaturesSQLAgent;
  LContextData : TStringList;
  LReservoirStr: TStringList;
  lOldVal      : string;
  lNewVal      : string;
begin
  try
    LLoadAgent := TNetworkFeaturesSQLAgent.Create(FAppModules);
    LContextData := TStringList.Create;
    try
      LReservoirStr  := TStringList.Create;
      try
        LReservoirStr.Sorted     := true;
        LReservoirStr.Duplicates := dupIgnore;
        LReservoirStr.CommaText  := Value;
        lNewVal                  := LReservoirStr.CommaText;
      finally
        LReservoirStr.Free;
      end;

      if (lNewVal <> FReservoirNumbers.CommaText) then
      begin
        lOldVal := FReservoirNumbers.CommaText;
        LContextData.Clear;
        LLoadAgent.LoadContextData_FeatureID(LContextData, IntToStr(FIdentifier));
        if (FAppModules.FieldProperties.UpdateFieldValue
             ('DroughtRestrictionReservoirNumber', lNewVal, lOldVal, LContextData)) then
        begin
          FReservoirNumbers.Clear;
          FReservoirNumbers.CommaText := lNewVal;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'DroughtRestrictionReservoirNumber', lOldVal, FReservoirNumbers.CommaText);
        end;
      end;
    finally
      LContextData.Free;
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TDroughtRestriction.Validate(var AErrors: WideString;const AContext: WideString): WordBool;
const OPNAME = 'TDroughtRestriction.Validate';
begin

end;

end.
