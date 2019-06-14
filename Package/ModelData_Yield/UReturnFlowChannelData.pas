//
//
//  UNIT      : Contains TReturnFlowChannelData Class
//  AUTHOR    : Sam Dhlamini(Cornastone)
//  DATE      : 13/06/2006
//  COPYRIGHT : Copyright © 2004 DWAF
//
//

unit UReturnFlowChannelData;

interface
uses
  SysUtils,
  Classes,
  Contnrs,
  VoaimsCom_TLB,
  UAbstractObject,
  UYieldModelDataObject,
  UReturnFlowChannelSQLAgent;
type

  TCorrespondingChannel = class(TAbstractAppObject,ICorrespondingChannel)
  protected
    FDemandChannel : Integer;
    FIdentifier : Integer;
    FChannelNumber : Integer;
    FAbstractionChannel : Integer;
    FAssumedFactor : Double;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;

    function Get_ChannelNumber : integer;safecall;
    procedure Set_ChannelNumber(AValue : integer);safecall;
    function Get_AbstractionChannel : integer; safecall;
    procedure Set_AbstractionChannel(AValue: integer);safecall;
    function Get_AssumedFactor : double;safecall;
    procedure Set_AssumedFactor(AValue : double);safecall;

    function ValidateChannelNumber(AErrorMessages : TStrings) : WordBool;
    function ValidateAbstractionChannel(AErrorMessages : TStrings) : WordBool;
    function ValidateAssumedFactor(AErrorMessages : TStrings) : WordBool;
  public
    function Initialise: boolean;override;
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool;safecall;
    function Populate(AIdentifier,ADemandChannel,AChannelNumber,AAbstractionChannel : integer;
                      AAssumedFactor:double) : boolean;
    property Identifier    : integer read FIdentifier;
    property ChannelNumber : Integer read Get_ChannelNumber write Set_ChannelNumber;
    property AbstractionChannel : Integer read Get_AbstractionChannel write Set_AbstractionChannel;
    property AssumedFactor : Double read Get_AssumedFactor write Set_AssumedFactor;
  end;

  TReturnFlowChannel = class(TAbstractAppObject,IReturnFlowChannel)
  protected
    FDemandChannel : Integer;
    FIdentifier : integer;
//    FNumOfCorrespondingChannels : Integer;
    FGaugeNumber : Integer;
    FMonthlyAvrgFactor : Double;
    FCalibrationFactor : Double;
    FMonthlyAvrgNetEvap : Double;
    FRoutingConstant : Double;
    FCurtailmentFactor : Double;
    FMultiplicationFactor : Double;
    FMonthlyPotentialEvap : TMonthlyDoubleArray;
    FCorrespondingChannelList : TObjectList;

    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;

    function Get_DemandChannel : integer;safecall;
    procedure Set_DemandChannel(AValue:integer);safecall;
    function Get_NumOfCorrespondingChannels : integer;safecall;
    procedure Set_NumOfCorrespondingChannels(AValue : integer);safecall;
    function Get_GaugeNumber : integer;safecall;
    procedure Set_GaugeNumber(AValue : integer);safecall;
    function Get_MonthlyAvrgFactor : double;safecall;
    procedure Set_MonthlyAvrgFactor(AValue : double);safecall;
    function Get_CalibrationFactor : double;safecall;
    procedure Set_CalibrationFactor(AValue : double);safecall;
    function Get_MonthlyAvrgNetEvap : double;safecall;
    procedure Set_MonthlyAvrgNetEvap(AValue : double);safecall;
    function Get_RoutingConstant : double;safecall;
    procedure Set_RoutingConstant(AValue : double);safecall;
    function Get_CurtailmentFactor : double;safecall;
    procedure Set_CurtailmentFactor(AValue : double);safecall;
    function Get_MultiplicationFactor : double;safecall;
    procedure Set_MultiplicationFactor(AValue : double);safecall;
    function Get_MonthlyPotentialEvapByIndex(AIndex : integer) : double;safecall;
    procedure Set_MonthlyPotentialEvapByIndex(AIndex : integer; AValue : double);safecall;
    function Get_CorrespondingChannelByIndex(AIndex: Integer): ICorrespondingChannel; safecall;

    function ValidateDemandChannel(AErrorMessages : TStrings) : WordBool;
//    function ValidateNumOfCorrespondingChannels(AErrorMessages : TStrings) : WordBool;
    function ValidateGaugeNumber(AErrorMessages : TStrings) : WordBool;
    function ValidateMonthlyAvrgFactor(AErrorMessages : TStrings) : WordBool;
    function ValidateCalibrationFactor(AErrorMessages : TStrings) : WordBool;
    function ValidateMonthlyAvrgNetEvap(AErrorMessages : TStrings) : WordBool;
    function ValidateRoutingConstant(AErrorMessages : TStrings) : WordBool;
    function ValidateCurtailmentFactor(AErrorMessages : TStrings) : WordBool;
    function ValidateMultiplicationFactor(AErrorMessages : TStrings) : WordBool;
    function ValidateMonthlyPotentialEvap(AErrorMessages: TStrings; AErrorColumns: TStringList) : WordBool;
  public
    function Initialise: boolean;override;
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; safecall;
    function CreateCorrespondingChannel : TCorrespondingChannel;
    function NewCorrespondingChannel(ACorrespondingChannel:integer): ICorrespondingChannel;safecall;
    function RemoveCorrespondingChannelByChannel(AChannel : integer) : WordBool;safecall;
    function DeleteCorrespondingChannelByChannel(AChannel:integer) : WordBool;
    function Populate(ADemandChannel,AIdentifier,{ANumOfCorrespondingChannels,}
                      AGaugeNumber : Integer; AMonthlyAvrgFactor,ACalibrationFactor,
                      AMonthlyAvrgNetEvap,ARoutingConstant,ACurtailmentFactor,
                      AMultiplicationFactor : Double;
                      AMonthlyPotentialEvap : TMonthlyDoubleArray) : boolean;
    property DemandChannel : Integer read Get_DemandChannel write Set_DemandChannel;
    property Identifier    : integer read FIdentifier;
    property NumOfCorrespondingChannels : Integer read Get_NumOfCorrespondingChannels write Set_NumOfCorrespondingChannels;
    property GaugeNumber : Integer read Get_GaugeNumber write Set_GaugeNumber;
    property MonthlyAvrgFactor : Double read Get_MonthlyAvrgFactor write Set_MonthlyAvrgFactor;
    property CalibrationFactor : Double read Get_CalibrationFactor write Set_CalibrationFactor;
    property MonthlyAvrgNetEvap : Double read Get_MonthlyAvrgNetEvap write Set_MonthlyAvrgNetEvap;
    property RoutingConstant : Double read Get_RoutingConstant write Set_RoutingConstant;
    property CurtailmentFactor : Double read Get_CurtailmentFactor write Set_CurtailmentFactor;
    property MultiplicationFactor : Double read Get_MultiplicationFactor write Set_MultiplicationFactor;
    property MonthlyPotentialEvapByIndex[AIndex :integer] : double read
             Get_MonthlyPotentialEvapByIndex write Set_MonthlyPotentialEvapByIndex;
    property CorrespondingChannelByIndex[AIndex: Integer]: ICorrespondingChannel read Get_CorrespondingChannelByIndex;
  end;

  TReturnFlowChannelData = class(TAbstractAppObject,IReturnFlowChannelData)
  protected
    FReturnFlowList  : TObjectList;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    function Get_ReturnFlowChannelCount : integer;safecall;
    function Get_ReturnFlowChannelByIndex(AIndex: Integer): IReturnFlowChannel; safecall;
    function Get_ReturnFlowChannelByChannel(ADemandChannel: Integer): IReturnFlowChannel; safecall;
  public
    function Initialise: boolean;override;
    function NewReturnFlowChannel(ADemandChannel : integer) : IReturnFlowChannel;safecall;
    function RemoveReturnFlowByChannel(ADemandChannel : integer):WordBool;safecall;
    function CreateReturnFlowChannel : TReturnFlowChannel;
    function DeleteReturnFlowByChannel(AChannel : integer) : boolean;
    property ReturnFlowChannelCount : integer read Get_ReturnFlowChannelCount;
    property ReturnFlowChannelByIndex[AIndex: Integer]: IReturnFlowChannel read Get_ReturnFlowChannelByIndex;
    property ReturnFlowChannelByChannel[ADemandChannel: Integer]: IReturnFlowChannel read Get_ReturnFlowChannelByChannel;
  end;

implementation
uses
  System.Types,
  UConstants,
  UDataSetType,
  UErrorHandlingOperations;

{ TReturnFlowChannelData }

function TReturnFlowChannelData._AddRef: Integer;
const OPNAME = 'TReturnFlowChannelData._AddRef';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TReturnFlowChannelData._Release: Integer;
const OPNAME = 'TReturnFlowChannelData._Release';
begin
   Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TReturnFlowChannelData.CreateMemberObjects;
const OPNAME = 'TReturnFlowChannelData.CreateMemberObjects';
begin
  inherited CreateMemberObjects;
  try
    FReturnFlowList := TObjectList.Create(False);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TReturnFlowChannelData.DestroyMemberObjects;
const OPNAME = 'TReturnFlowChannelData.DestroyMemberObjects';
begin
  try
    inherited DestroyMemberObjects;
    FreeAndNil(FReturnFlowList);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TReturnFlowChannelData.CreateReturnFlowChannel: TReturnFlowChannel;
const OPNAME = 'TReturnFlowChannelData.CreateReturnFlowChannel';
begin
  Result := nil;
  try
    Result := TReturnFlowChannel.Create(FAppModules);
    FReturnFlowList.Add(Result);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TReturnFlowChannelData.DeleteReturnFlowByChannel(AChannel: integer): boolean;
const OPNAME = 'TReturnFlowChannelData.DeleteReturnFlowByChannel';
var
  LIndex: integer;
begin
  Result := False;
  try
    for LIndex := 0 to FReturnFlowList.Count -1 do
    begin
      if TReturnFlowChannel(FReturnFlowList.Items[LIndex]).FDemandChannel = AChannel then
      begin
        FReturnFlowList.Remove(TReturnFlowChannel(FReturnFlowList.Items[LIndex]));
        Result := True;
        Break;
      end;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TReturnFlowChannelData.Get_ReturnFlowChannelCount: integer;
const OPNAME = 'TReturnFlowChannelData.Get_ReturnFlowChannelCount';
begin
  Result := 0;
  try
    Result := FReturnFlowList.Count;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TReturnFlowChannelData.Initialise: boolean;
const OPNAME = 'TReturnFlowChannelData.Initialise';
begin
  Result := False;
  try
    FReturnFlowList.Clear;
    Result := True;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TReturnFlowChannelData.NewReturnFlowChannel(ADemandChannel : integer): IReturnFlowChannel;
const OPNAME = 'TReturnFlowChannelData.NewReturnFlowChannel';
var
  LLoadAgent : TReturnFlowChannelSQLAgent;
  LIdentifier : integer;
  LReturnFlowChannel : TReturnFlowChannel;
begin
  try
    LLoadAgent := TReturnFlowChannelSQLAgent.Create(FAppModules);
    if LLoadAgent.InsertReturnFlowChannelDetail(ADemandChannel,LIdentifier) then
    begin
      LReturnFlowChannel := CreateReturnFlowChannel;
      LReturnFlowChannel.FDemandChannel := ADemandChannel;
      LReturnFlowChannel.FIdentifier := LIdentifier;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TReturnFlowChannelData.RemoveReturnFlowByChannel(ADemandChannel: integer): WordBool;
const OPNAME = 'TReturnFlowChannelData.RemoveReturnFlowByChannel';
var
  LLoadAgent : TReturnFlowChannelSQLAgent;
begin
  Result := False;
  try
    LLoadAgent := TReturnFlowChannelSQLAgent.Create(FAppModules);
    if LLoadAgent.DeleteReturnFlowByChannel(ADemandChannel) then
      Result := DeleteReturnFlowByChannel(ADemandChannel);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;


function TReturnFlowChannelData.Get_ReturnFlowChannelByChannel(ADemandChannel: Integer): IReturnFlowChannel;
const OPNAME = 'TReturnFlowChannelData.Get_ReturnFlowChannelByChannel';
var
  LIndex : integer;
begin
  Result := nil;
  try
    LIndex := 0;
    while (Result = nil) and (LIndex < FReturnFlowList.Count) do
    begin
      if TReturnFlowChannel(FReturnFlowList.Items[LIndex]).FDemandChannel = ADemandChannel then
        Result := TReturnFlowChannel(FReturnFlowList.Items[LIndex])
      else
      inc(LIndex);
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TReturnFlowChannelData.Get_ReturnFlowChannelByIndex(AIndex: Integer): IReturnFlowChannel;
const OPNAME = 'TReturnFlowChannelData.Get_ReturnFlowChannelByIndex';
begin
  Result := nil;
  try
    if (AIndex >=0) and (AIndex < FReturnFlowList.Count) then
      Result := TReturnFlowChannel(FReturnFlowList.Items[AIndex]);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

{ TReturnFlowChannel }

function TReturnFlowChannel._AddRef: Integer;
const OPNAME = 'TReturnFlowChannel._AddRef';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TReturnFlowChannel._Release: Integer;
const OPNAME = 'TReturnFlowChannel._Release';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TReturnFlowChannel.CreateCorrespondingChannel: TCorrespondingChannel;
const OPNAME = 'TReturnFlowChannel.CreateCorrespondingChannel';
begin
  Result := nil;
  try
    Result := TCorrespondingChannel.Create(FAppModules);
    FCorrespondingChannelList.Add(Result);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TReturnFlowChannel.CreateMemberObjects;
const OPNAME = 'TReturnFlowChannel.CreateMemberObjects';
begin
  inherited CreateMemberObjects;
  try
    FCorrespondingChannelList := TObjectList.Create(False);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TReturnFlowChannel.DestroyMemberObjects;
const OPNAME = 'TReturnFlowChannel.DestroyMemberObjects';
begin
  try
    inherited DestroyMemberObjects;
    FreeAndNil(FCorrespondingChannelList);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TReturnFlowChannel.Get_CalibrationFactor: double;
const OPNAME = 'TReturnFlowChannel.Get_CalibrationFactor';
begin
  Result := NullFloat;
  try
    Result := FCalibrationFactor;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TReturnFlowChannel.Get_CurtailmentFactor: double;
const OPNAME = 'TReturnFlowChannel.Get_CurtailmentFactor';
begin
  Result := NullFloat;
  try
    Result := FCurtailmentFactor; 
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TReturnFlowChannel.Get_DemandChannel: integer;
const OPNAME = 'TReturnFlowChannel.Get_DemandChannel';
begin
  Result := 0;
  try
    Result := FDemandChannel;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TReturnFlowChannel.Get_GaugeNumber: integer;
const OPNAME = 'TReturnFlowChannel.Get_GaugeNumber';
begin
  Result := 0;
  try
    Result := FGaugeNumber;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TReturnFlowChannel.Get_MonthlyAvrgFactor: double;
const OPNAME = 'TReturnFlowChannel.Get_MonthlyAvrgFactor';
begin
  Result := NullFloat;
  try
    Result := FMonthlyAvrgFactor
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TReturnFlowChannel.Get_MonthlyAvrgNetEvap: double;
const OPNAME = 'TReturnFlowChannel.Get_MonthlyAvrgNetEvap';
begin
  Result := NullFloat;
  try
    Result := FMonthlyAvrgNetEvap;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TReturnFlowChannel.Get_MonthlyPotentialEvapByIndex(AIndex: integer): double;
const OPNAME = 'TReturnFlowChannel.Get_MonthlyPotentialEvapByIndex';
var
  LFieldProperty : TAbstractFieldProperty;
begin
  Result := NullFloat;
  try
    LFieldProperty := FAppModules.FieldProperties.FieldProperty('PotentialMonthlyEvap');
    if (AIndex >= LFieldProperty.ArrayLow) AND (AIndex <= LFieldProperty.ArrayHigh) then
    begin
      if FMonthlyPotentialEvap [AIndex] = NullFloat then
        Result := 0.
      else
        Result := FMonthlyPotentialEvap[AIndex];
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TReturnFlowChannel.Get_MultiplicationFactor: double;
const OPNAME = 'TReturnFlowChannel.Get_MultiplicationFactor';
begin
  Result := NullFloat;
  try
    Result := FMultiplicationFactor;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TReturnFlowChannel.Set_NumOfCorrespondingChannels(AValue: integer);
const OPNAME = 'TReturnFlowChannel.Set_NumOfCorrespondingChannels';
{var
  LLoadAgent : TReturnFlowChannelSQLAgent;
  LContextData : TStringList;
  LOldValue : string;}
begin
  try
  {
    LLoadAgent := TReturnFlowChannelSQLAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        LLoadAgent.LoadContextData(LContextData, IntToStr(FIdentifier));
        if (AValue <> FNumOfCorrespondingChannels) then
        begin
          if (FAppModules.FieldProperties.UpdateFieldValue(
             'NumOfCorrespondingChannels', IntToStr(AValue), IntToStr(FNumOfCorrespondingChannels), LContextData)) then
          begin
            LOldValue := IntToStr(FNumOfCorrespondingChannels);
            FNumOfCorrespondingChannels := AValue;

            FAppModules.Model.StudyDataHasChanged(sdccEdit,'NumOfCorrespondingChannelsl',LOldValue,IntToStr(AValue));
          end;
        end;
      finally
        LContextData.Free;
      end;
    finally
      LLoadAgent.Free;
    end;
    }
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TReturnFlowChannel.Get_NumOfCorrespondingChannels: integer;
const OPNAME = 'TReturnFlowChannel.Get_NumOfCorrespondingChannels';
begin
  Result := 0;
  try
    Result := FCorrespondingChannelList.Count;//FNumOfCorrespondingChannels;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TReturnFlowChannel.Get_RoutingConstant: double;
const OPNAME = 'TReturnFlowChannel.Get_RoutingConstant';
begin
  Result := NullFloat;
  try
    Result := FRoutingConstant;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TReturnFlowChannel.Initialise: boolean;
const OPNAME = 'TReturnFlowChannel.Initialise';
var
  LIndex : integer;
begin
  Result := False;
  try
    FDemandChannel := 0;
    FIdentifier := 0;
//    FNumOfCorrespondingChannels := 0;
    FGaugeNumber := 0;
    FMonthlyAvrgFactor := NullFloat;
    FCalibrationFactor := NullFloat;
    FMonthlyAvrgNetEvap := NullFloat;
    FRoutingConstant := NullFloat;
    FCurtailmentFactor := NullFloat;
    FMultiplicationFactor := NullFloat;
    for LIndex := MinMonths to MaxMonths do
      FMonthlyPotentialEvap[LIndex] := NullFloat;
    FCorrespondingChannelList.Clear;
    Result := True;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TReturnFlowChannel.NewCorrespondingChannel(ACorrespondingChannel:integer): ICorrespondingChannel;
const OPNAME = 'TReturnFlowChannel.NewCorrespondingChannel';
var
  LCorrespondingChannels : TCorrespondingChannel;
  LLoadAgent : TReturnFlowChannelSQLAgent;
begin
  Result := nil;
  try
    LLoadAgent := TReturnFlowChannelSQLAgent.Create(FAppModules);
    if LLoadAgent.InsertReturnFlowCorrespondingChannel(FDemandChannel,ACorrespondingChannel,FIdentifier) then
    begin
      LCorrespondingChannels := CreateCorrespondingChannel;
      LCorrespondingChannels.FDemandChannel := FDemandChannel;
      LCorrespondingChannels.FIdentifier := FIdentifier;
      LCorrespondingChannels.FChannelNumber := ACorrespondingChannel;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TReturnFlowChannel.Populate(ADemandChannel, AIdentifier,{ANumOfCorrespondingChannels,}
                                     AGaugeNumber: Integer; AMonthlyAvrgFactor,
                                     ACalibrationFactor, AMonthlyAvrgNetEvap, ARoutingConstant,
                                     ACurtailmentFactor, AMultiplicationFactor: Double;
                                     AMonthlyPotentialEvap: TMonthlyDoubleArray): boolean;
const OPNAME = 'TReturnFlowChannel.Populate';
var
  LIndex : integer;
  LFieldProperty : TAbstractFieldProperty;
begin
  Result := False;
  try
    FDemandChannel := ADemandChannel;
    FIdentifier  := AIdentifier;
//    FNumOfCorrespondingChannels := ANumOfCorrespondingChannels;
    FGaugeNumber := AGaugeNumber;
    FMonthlyAvrgFactor := AMonthlyAvrgFactor;
    FCalibrationFactor := ACalibrationFactor;
    FMonthlyAvrgNetEvap := AMonthlyAvrgNetEvap;
    FRoutingConstant := ARoutingConstant;
    FCurtailmentFactor := ACurtailmentFactor;
    FMultiplicationFactor := AMultiplicationFactor;
    LFieldProperty :=  FAppModules.FieldProperties.FieldProperty('PotentialMonthlyEvap');
    for LIndex := LFieldProperty.ArrayLow to LFieldProperty.ArrayHigh do
    begin
      if AMonthlyPotentialEvap[LIndex] <> NullFloat then
        FMonthlyPotentialEvap[LIndex] := AMonthlyPotentialEvap[LIndex];
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TReturnFlowChannel.RemoveCorrespondingChannelByChannel(AChannel: integer): WordBool;
const OPNAME = 'TReturnFlowChannel.RemoveCorrespondingChannelByChannel';
var
  LLoadAgent : TReturnFlowChannelSQLAgent;

begin
  Result := False;
  try
    LLoadAgent := TReturnFlowChannelSQLAgent.Create(FAppModules);
    if LLoadAgent.DeleteCorrespondingChannelByChannel(AChannel) then
      Result := DeleteCorrespondingChannelByChannel(AChannel);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TReturnFlowChannel.DeleteCorrespondingChannelByChannel(AChannel:integer): WordBool;
const OPNAME = 'TReturnFlowChannel.DeleteCorrespondingChannelByChannel';
var
  LIndex : integer;
  LCorrespondingChannel : TCorrespondingChannel;
begin
  Result := False;
  try
    for LIndex := 0 to FCorrespondingChannelList.Count - 1 do
    begin
      LCorrespondingChannel := TCorrespondingChannel(FCorrespondingChannelList.Items[LIndex]);
      if LCorrespondingChannel.ChannelNumber = AChannel then
      begin
        FCorrespondingChannelList.Delete(LIndex);
        Result := True;
        Break;
      end;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TReturnFlowChannel.Set_CalibrationFactor(AValue: double);
const OPNAME = 'TReturnFlowChannel.Set_CalibrationFactor';
var
  LLoadAgent : TReturnFlowChannelSQLAgent;
  LContextData : TStringList;
  LOldValue : string;
begin
  try
    LLoadAgent := TReturnFlowChannelSQLAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        LLoadAgent.LoadContextData(LContextData, IntToStr(FIdentifier));
        if (FAppModules.FieldProperties.UpdateFieldValue(
           'CalibrationFactor', FloatToStr(AValue), FloatToStr(FCalibrationFactor), LContextData)) then
        begin
          LOldValue := FloatToStr(FCalibrationFactor);
          FCalibrationFactor := AValue;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'CalibrationFactor',LOldValue,FloatToStr(AValue));
        end;
      finally
        LContextData.Free;
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TReturnFlowChannel.Set_CurtailmentFactor(AValue: double);
const OPNAME = 'TReturnFlowChannel.Set_CurtailmentFactor';
var
  LLoadAgent : TReturnFlowChannelSQLAgent;
  LContextData : TStringList;
  LOldValue : string;
begin
  try
    LLoadAgent := TReturnFlowChannelSQLAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        LLoadAgent.LoadContextData(LContextData, IntToStr(FIdentifier));
        if (FAppModules.FieldProperties.UpdateFieldValue(
           'CurtailmentFactor', FloatToStr(AValue), FloatToStr(FCurtailmentFactor), LContextData)) then
        begin
          LOldValue := FloatToStr(FCurtailmentFactor);
          FCurtailmentFactor := AValue;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'CurtailmentFactor',LOldValue,FloatToStr(AValue));
        end;
      finally
        LContextData.Free;
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TReturnFlowChannel.Set_DemandChannel(AValue: integer);
const OPNAME = 'TReturnFlowChannel.Set_DemandChannel';
var
  LLoadAgent : TReturnFlowChannelSQLAgent;
  LContextData : TStringList;
  LOldValue : string;
begin
  try
    LLoadAgent := TReturnFlowChannelSQLAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        LLoadAgent.LoadContextData(LContextData, IntToStr(FIdentifier));
        if (FAppModules.FieldProperties.UpdateFieldValue(
           'DemandChannel', IntToStr(AValue), IntToStr(FDemandChannel), LContextData)) then
        begin
          LOldValue := IntToStr(FDemandChannel);
          FDemandChannel := AValue;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'DemandChannel',LOldValue,IntToStr(AValue));
        end;
      finally
        LContextData.Free;
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TReturnFlowChannel.Set_GaugeNumber(AValue: integer);
const OPNAME = 'TReturnFlowChannel.Set_GaugeNumber';
var
  LLoadAgent : TReturnFlowChannelSQLAgent;
  LContextData : TStringList;
  LOldValue : string;
begin
  try
    LLoadAgent := TReturnFlowChannelSQLAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        LLoadAgent.LoadContextData(LContextData, IntToStr(FIdentifier));
        if (FAppModules.FieldProperties.UpdateFieldValue(
           'ReturnFlowGaugeNumber', IntToStr(AValue), IntToStr(FGaugeNumber), LContextData)) then
        begin
          LOldValue := IntToStr(FGaugeNumber);
          FGaugeNumber := AValue;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'ReturnFlowGaugeNumber',LOldValue,IntToStr(AValue));
        end;
      finally
        LContextData.Free;
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TReturnFlowChannel.Set_MonthlyAvrgFactor(AValue: double);
const OPNAME = 'TReturnFlowChannel.Set_MonthlyAvrgFactor';
var
  LLoadAgent : TReturnFlowChannelSQLAgent;
  LContextData : TStringList;
  LOldValue : string;
begin
  try
    LLoadAgent := TReturnFlowChannelSQLAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        LLoadAgent.LoadContextData(LContextData, IntToStr(FIdentifier));
        if (FAppModules.FieldProperties.UpdateFieldValue(
           'MonthlyAvrgFactor', FloatToStr(AValue), FloatToStr(FMonthlyAvrgFactor), LContextData)) then
        begin
          LOldValue := FloatToStr(FMonthlyAvrgFactor);
          FMonthlyAvrgFactor := AValue;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'MonthlyAvrgFactor',LOldValue,FloatToStr(AValue));
        end;
      finally
        LContextData.Free;
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TReturnFlowChannel.Set_MonthlyAvrgNetEvap(AValue: double);
const OPNAME = 'TReturnFlowChannel.Set_MonthlyAvrgNetEvap';
var
  LLoadAgent : TReturnFlowChannelSQLAgent;
  LContextData : TStringList;
  LOldValue : string;
begin
  try
    LLoadAgent := TReturnFlowChannelSQLAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        LLoadAgent.LoadContextData(LContextData, IntToStr(FIdentifier));
        if (FAppModules.FieldProperties.UpdateFieldValue(
           'MonthlyAvrgNetEvap', FloatToStr(AValue), FloatToStr(FMonthlyAvrgNetEvap), LContextData)) then
        begin
          LOldValue := FloatToStr(FMonthlyAvrgNetEvap);
          FMonthlyAvrgNetEvap := AValue;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'MonthlyAvrgNetEvap',LOldValue,FloatToStr(AValue));
        end;
      finally
        LContextData.Free;
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TReturnFlowChannel.Set_MonthlyPotentialEvapByIndex(AIndex: integer; AValue: double);
const OPNAME = 'TReturnFlowChannel.Set_MonthlyPotentialEvapByIndex';
var
  LLoadAgent : TReturnFlowChannelSQLAgent;
  LContextData : TStringList;
  LNewValue,
  LOldValue : string;
  LFieldProperty : TAbstractFieldProperty;
begin
  try
    LFieldProperty := FAppModules.FieldProperties.FieldProperty('PotentialMonthlyEvap');
    if (AIndex >= LFieldProperty.ArrayLow) and (AIndex <= LFieldProperty.ArrayHigh) then
    begin
      if FMonthlyPotentialEvap[AIndex] <> AValue then
      begin
        LLoadAgent := TReturnFlowChannelSQLAgent.Create(FAppModules);
        try
          LContextData := TStringList.Create;
          try
            LLoadAgent.LoadContextData_DemandChannel(LContextData, IntToStr(FIdentifier),
                                                     IntToStr(FDemandChannel),IntToStr(AIndex));
            if (AValue = NullFloat) then
              LNewValue := ''
            else
              LNewValue := FloatToStr(AValue);
            if (FMonthlyPotentialEvap[AIndex] = NullFloat) then
              LOldValue := ''
            else
              LOldValue := FloatToStr(FMonthlyPotentialEvap[AIndex]);
            if (FAppModules.FieldProperties.UpdateFieldValue(
               'PotentialMonthlyEvap', LNewValue, LOldValue, LContextData)) then
            begin
              LOldValue := FloatToStr(FMonthlyPotentialEvap[AIndex]);
              FMonthlyPotentialEvap[AIndex] := AValue;
              FAppModules.Model.StudyDataHasChanged(sdccEdit,'PotentialMonthlyEvap',LOldValue,FloatToStr(AValue));
            end;
          finally
            LContextData.Free;
          end;
        finally
          LLoadAgent.Free;
        end;
      end;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TReturnFlowChannel.Set_MultiplicationFactor(AValue: double);
const OPNAME = 'TReturnFlowChannel.Set_MultiplicationFactor';
var
  LLoadAgent : TReturnFlowChannelSQLAgent;
  LContextData : TStringList;
  LOldValue : string;
begin
  try
    LLoadAgent := TReturnFlowChannelSQLAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        LLoadAgent.LoadContextData(LContextData, IntToStr(FIdentifier));
        if (FAppModules.FieldProperties.UpdateFieldValue(
           'MultiplicationFactor', FloatToStr(AValue), FloatToStr(FMultiplicationFactor), LContextData)) then
        begin
          LOldValue := FloatToStr(FMultiplicationFactor);
          FMultiplicationFactor := AValue;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'MultiplicationFactor',LOldValue,FloatToStr(AValue));
        end;
      finally
        LContextData.Free;
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TReturnFlowChannel.Set_RoutingConstant(AValue: double);
const OPNAME = 'TReturnFlowChannel.Set_RoutingConstant';
var
  LLoadAgent : TReturnFlowChannelSQLAgent;
  LContextData : TStringList;
  LOldValue : string;
begin
  try
    LLoadAgent := TReturnFlowChannelSQLAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        LLoadAgent.LoadContextData(LContextData, IntToStr(FIdentifier));
        if (FAppModules.FieldProperties.UpdateFieldValue(
           'RoutingConstant', FloatToStr(AValue), FloatToStr(FRoutingConstant), LContextData)) then
        begin
          LOldValue := FloatToStr(FRoutingConstant);
          FRoutingConstant := AValue;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'RoutingConstant',LOldValue,FloatToStr(AValue));
        end;
      finally
        LContextData.Free;
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TReturnFlowChannel.Validate(var AErrors: WideString;const AContext: WideString): WordBool;
const OPNAME = 'TReturnFlowChannel.Validate';
var
  LErrorCols        : TStringList;
  LErrorMsgs        : TStringList;
begin
  Result := False;
  try
    LErrorCols := TStringList.Create;
    LErrorMsgs := TStringList.Create;
    try
      if (AContext = 'DemandChannel') then
        Result := ValidateDemandChannel(LErrorMsgs)
      else
      if (AContext = 'ReturnFlowGaugeNumber' ) then
        Result := ValidateGaugeNumber(LErrorMsgs)
      else
      if (AContext = 'MonthlyAvrgFactor' ) then
        Result := ValidateMonthlyAvrgFactor(LErrorMsgs)
      else
      if (AContext = 'CalibrationFactor' ) then
        Result := ValidateCalibrationFactor(LErrorMsgs)
      else
      if (AContext = 'MonthlyAvrgNetEvap' ) then
        Result := ValidateMonthlyAvrgNetEvap(LErrorMsgs)
      else
      if (AContext = 'RoutingConstant' ) then
        Result := ValidateRoutingConstant(LErrorMsgs)
      else
      if (AContext = 'CurtailmentFactor' ) then
        Result := ValidateCurtailmentFactor(LErrorMsgs)
      else
      if (AContext = 'MultiplicationFactor' ) then
        Result := ValidateMultiplicationFactor(LErrorMsgs)
      else
      if (AContext = 'MultiplicationFactor' ) then
        Result := ValidateMonthlyPotentialEvap(LErrorMsgs,LErrorCols);
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
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TReturnFlowChannel.ValidateCalibrationFactor(AErrorMessages: TStrings): WordBool;
const OPNAME = 'TReturnFlowChannel.ValidateCalibrationFactor';
var
  LMessage : string;
begin
  Result := False;
  try
       LMessage := '';
    if (not FAppModules.FieldProperties.ValidateFieldProperty('CalibrationFactor',
            FloatToStr(FCalibrationFactor), LMessage)) then
      AErrorMessages.Add( 'ERROR:'+FloatToStr(FCalibrationFactor) + ':'+LMessage)
    else
      Result := True;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TReturnFlowChannel.ValidateCurtailmentFactor(AErrorMessages: TStrings): WordBool;
const OPNAME = 'TReturnFlowChannel.ValidateCurtailmentFactor';
var
  LMessage : string;
begin
  Result := False;
  try
       LMessage := '';
    if (not FAppModules.FieldProperties.ValidateFieldProperty('CurtailmentFactor',
            FloatToStr(FCurtailmentFactor), LMessage)) then
      AErrorMessages.Add('ERROR:'+ FloatToStr(FCurtailmentFactor) + ':'+LMessage)
    else
      Result := True;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TReturnFlowChannel.ValidateDemandChannel(AErrorMessages: TStrings): WordBool;
const OPNAME = 'TReturnFlowChannel.ValidateDemandChannel';
var
  LMessage : string;
begin
  Result := False;
  try
       LMessage := '';
    if (not FAppModules.FieldProperties.ValidateFieldProperty('DemandChannel',
            IntToStr(FDemandChannel), LMessage)) then
      AErrorMessages.Add('ERROR:'+IntToStr(FDemandChannel) + ':'+LMessage)
    else
      Result := True;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TReturnFlowChannel.ValidateGaugeNumber(AErrorMessages: TStrings): WordBool;
const OPNAME = 'TReturnFlowChannel.ValidateGaugeNumber';
var
  LMessage : string;
begin
  Result := False;
  try
       LMessage := '';
    if (not FAppModules.FieldProperties.ValidateFieldProperty('ReturnFlowGaugeNumber',
            IntToStr(FGaugeNumber), LMessage)) then
      AErrorMessages.Add('ERROR:'+IntToStr(FGaugeNumber) + ':'+LMessage)
    else
      Result := True;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TReturnFlowChannel.ValidateMonthlyAvrgFactor(AErrorMessages: TStrings): WordBool;
const OPNAME = 'TReturnFlowChannel.ValidateMonthlyAvrgFactor';
var
  LMessage : string;
begin
  Result := False;
  try
    LMessage := '';
    if (not FAppModules.FieldProperties.ValidateFieldProperty('MonthlyAvrgFactor',
            FloatToStr(FMonthlyAvrgFactor), LMessage)) then
      AErrorMessages.Add('ERROR:'+FloatToStr(FMonthlyAvrgFactor) + ':'+LMessage)
    else
      Result := True;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TReturnFlowChannel.ValidateMonthlyAvrgNetEvap(AErrorMessages: TStrings): WordBool;
const OPNAME = 'TReturnFlowChannel.ValidateMonthlyAvrgNetEvap';
var
  LMessage : string;
begin
  Result := False;
  try
    LMessage := '';
    if (not FAppModules.FieldProperties.ValidateFieldProperty('MonthlyAvrgNetEvap',
            FloatToStr(FMonthlyAvrgNetEvap), LMessage)) then
      AErrorMessages.Add('ERROR:'+FloatToStr(FMonthlyAvrgNetEvap) + ':'+LMessage)
    else
      Result := True;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TReturnFlowChannel.ValidateMonthlyPotentialEvap(AErrorMessages: TStrings; AErrorColumns: TStringList): WordBool;
const OPNAME = 'TReturnFlowChannel.ValidateMonthlyPotentialEvap';
var
  LIndex            : integer;
  LStopOnFirstError : Boolean;
  LMessage          : string;
  LResult           : Boolean;
  LFieldProperties: TAbstractFieldProperty;
begin
  Result := False;
  try
    lResult := True;
    LFieldProperties := FAppModules.FieldProperties.FieldProperty('PotentialMonthlyEvap');
    LStopOnFirstError := FAppModules.GlobalData.StopOnFirstErr;
    for LIndex := LFieldProperties.ArrayLow to LFieldProperties.ArrayHigh do
    begin
      lMessage := '';
      if (not FAppModules.FieldProperties.ValidateFieldProperty
                  ('PotentialMonthlyEvap', FloatToStr(MonthlyPotentialEvapByIndex[LIndex]),
                   LMessage, LIndex)) then
      begin
        lResult := False;
        AErrorMessages.Add('ERROR:'+LMessage);
        AErrorColumns.Add(IntToStr(LIndex));
        if (LStopOnFirstError) then
          Break;
      end;
    end;
    Result := lResult;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TReturnFlowChannel.ValidateMultiplicationFactor(AErrorMessages: TStrings): WordBool;
const OPNAME = 'TReturnFlowChannel.ValidateMultiplicationFactor';
var
  LMessage : string;
begin
  Result := False;
  try
    LMessage := '';
    if (not FAppModules.FieldProperties.ValidateFieldProperty('MultiplicationFactor',
            FloatToStr(FMultiplicationFactor), LMessage)) then
      AErrorMessages.Add('ERROR:'+FloatToStr(FMultiplicationFactor) + ':'+LMessage)
    else
      Result := True;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;
(*
function TReturnFlowChannel.ValidateNumOfCorrespondingChannels(AErrorMessages: TStrings): WordBool;
const OPNAME = 'TReturnFlowChannel.ValidateNumOfCorrespondingChannels';
var
  LMessage : string;
begin
  Result := False;
  try

       LMessage := '';
    if (not FAppModules.FieldProperties.ValidateFieldProperty('NumOfCorrespondingChannels',
            IntToStr(FNumOfCorrespondingChannels), LMessage)) then
      AErrorMessages.Add('ERROR:'+IntToStr(FNumOfCorrespondingChannels) + ':'+LMessage)
    else
      Result := True;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;
*)

function TReturnFlowChannel.ValidateRoutingConstant(AErrorMessages: TStrings): WordBool;
const OPNAME = 'TReturnFlowChannel.ValidateRoutingConstant';
var
  LMessage : string;
begin
  Result := False;
  try
       LMessage := '';
    if (not FAppModules.FieldProperties.ValidateFieldProperty('RoutingConstant',
            FloatToStr(FRoutingConstant), LMessage)) then
      AErrorMessages.Add('ERROR:'+ FloatToStr(FRoutingConstant) + ':'+LMessage)
    else
      Result := True;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TReturnFlowChannel.Get_CorrespondingChannelByIndex(AIndex: Integer): ICorrespondingChannel;
const OPNAME = 'TReturnFlowChannel.Get_CorrespondingChannelByIndex';
begin
  Result := nil;
  try
    if (AIndex >=0) and (AIndex < FCorrespondingChannelList.Count) then
      Result := TCorrespondingChannel(FCorrespondingChannelList.Items[AIndex]);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

{ TCorrespondingChannels }

function TCorrespondingChannel._AddRef: Integer;
const OPNAME = 'TCorrespondingChannel._AddRef';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TCorrespondingChannel._Release: Integer;
const OPNAME = 'TCorrespondingChannel._Release';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TCorrespondingChannel.CreateMemberObjects;
const OPNAME = 'TCorrespondingChannel.CreateMemberObjects';
begin
  inherited CreateMemberObjects;
  try

  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TCorrespondingChannel.DestroyMemberObjects;
const OPNAME = 'TCorrespondingChannel.DestroyMemberObjects';
begin
  try
    inherited DestroyMemberObjects;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TCorrespondingChannel.Get_AbstractionChannel: integer;
const OPNAME = 'TCorrespondingChannel.Get_AbstractionChannel';
begin
  Result := 0;
  try
    Result := FAbstractionChannel;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TCorrespondingChannel.Get_AssumedFactor: double;
const OPNAME = 'TCorrespondingChannel.Get_AssumedFactor';
begin
  Result := NullFloat;
  try
    Result := FAssumedFactor;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TCorrespondingChannel.Get_ChannelNumber: integer;
const OPNAME = 'TCorrespondingChannel.Get_ChannelNumber';
begin
  Result := 0;
  try
    Result := FChannelNumber;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TCorrespondingChannel.Initialise: boolean;
const OPNAME = 'TCorrespondingChannel.Initialise';
begin
  Result := False;
  try
    FDemandChannel := 0;
    FIdentifier := 0;
    FChannelNumber := 0;
    FAbstractionChannel := 0;
    FAssumedFactor := NullFloat;
    Result := True;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TCorrespondingChannel.Populate(AIdentifier, ADemandChannel,
                                         AChannelNumber, AAbstractionChannel : integer;
                                         AAssumedFactor: double): boolean;
const OPNAME = 'TCorrespondingChannel.Populate';
begin
  Result := False;
  try
    FDemandChannel := ADemandChannel;
    FIdentifier := AIdentifier;
    FChannelNumber := AChannelNumber;
    FAbstractionChannel := AAbstractionChannel;
    FAssumedFactor := AAssumedFactor;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TCorrespondingChannel.Set_AbstractionChannel(AValue: integer);
const OPNAME = 'TCorrespondingChannel.Set_AbstractionChannel';
var
  LLoadAgent : TReturnFlowChannelSQLAgent;
  LContextData : TStringList;
  LOldValue : string;
begin
  try
    LLoadAgent := TReturnFlowChannelSQLAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        if FAbstractionChannel <> AValue then
        begin
          LLoadAgent.LoadContextData_CorrespondingChannels(LContextData, IntToStr(FIdentifier),
                                                           IntToStr(FDemandChannel),IntToStr(FChannelNumber));
          if (FAppModules.FieldProperties.UpdateFieldValue(
            'AbstractionChannel', IntToStr(AValue), IntToStr(FAbstractionChannel), LContextData)) then
          begin
            LOldValue := IntToStr(FAbstractionChannel);
            FAbstractionChannel := AValue;
            FAppModules.Model.StudyDataHasChanged(sdccEdit,'AbstractionChannel',LOldValue,IntToStr(AValue));
          end;
        end;
      finally
        LContextData.Free;
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TCorrespondingChannel.Set_AssumedFactor(AValue: double);
const OPNAME = 'TCorrespondingChannel.Set_AssumedFactor';
var
  LLoadAgent : TReturnFlowChannelSQLAgent;
  LContextData : TStringList;
  LOldValue : string;
begin
  try
    LLoadAgent := TReturnFlowChannelSQLAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        if FChannelNumber <> AValue then
        begin
          LLoadAgent.LoadContextData_CorrespondingChannels(LContextData, IntToStr(FIdentifier),
                                                           IntToStr(FDemandChannel),IntToStr(FChannelNumber));
          if (FAppModules.FieldProperties.UpdateFieldValue(
            'AssumedFactor', FloatToStr(AValue), FloatToStr(FAssumedFactor), LContextData)) then
          begin
            LOldValue := FloatToStr(FChannelNumber);
            FAssumedFactor := AValue;
            FAppModules.Model.StudyDataHasChanged(sdccEdit,'AssumedFactor',LOldValue,FloatToStr(AValue));
          end;
        end;
      finally
        LContextData.Free;
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TCorrespondingChannel.Set_ChannelNumber(AValue: integer);
const OPNAME = 'TCorrespondingChannel.Set_ChannelNumber';
var
  LLoadAgent : TReturnFlowChannelSQLAgent;
  LContextData : TStringList;
  LOldValue : string;
begin
  try
    LLoadAgent := TReturnFlowChannelSQLAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        if FChannelNumber <> AValue then
        begin
          LLoadAgent.LoadContextData_CorrespondingChannels(LContextData, IntToStr(FIdentifier),
                                                           IntToStr(FDemandChannel),IntToStr(FChannelNumber));
          if (FAppModules.FieldProperties.UpdateFieldValue(
            'CorrespondingChannel', IntToStr(AValue), IntToStr(FChannelNumber), LContextData)) then
          begin
            LOldValue := IntToStr(FChannelNumber);
            FChannelNumber := AValue;
            FAppModules.Model.StudyDataHasChanged(sdccEdit,'CorrespondingChannel',LOldValue,IntToStr(AValue));
          end;
        end;
      finally
        LContextData.Free;
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TCorrespondingChannel.Validate(var AErrors: WideString;const AContext: WideString): WordBool;
const OPNAME = 'TCorrespondingChannel.Validate';
var
  LErrorMsgs        : TStringList;
begin
  Result := False;
  try
    LErrorMsgs := TStringList.Create;
    try
      if AContext = 'CorrespondingChannel' then
        ValidateChannelNumber(LErrorMsgs)
      else
      if AContext = 'AbstractionChannel' then
        ValidateAbstractionChannel(LErrorMsgs)
      else
      if AContext = 'AssumedFactor' then
        ValidateAssumedFactor(LErrorMsgs);
    finally
      FreeAndNil(LErrorMsgs);
    end;

  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TCorrespondingChannel.ValidateAbstractionChannel(AErrorMessages: TStrings): WordBool;
const OPNAME = 'TCorrespondingChannel.ValidateAbstractionChannel';
var
  LMessage : string;
begin
  Result := False;
  try
       LMessage := '';
    if (not FAppModules.FieldProperties.ValidateFieldProperty('AbstractionChannel',
            IntToStr(FAbstractionChannel), LMessage)) then
      AErrorMessages.Add('ERROR:'+IntToStr(FAbstractionChannel) + ':'+LMessage)
    else
      Result := True;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TCorrespondingChannel.ValidateAssumedFactor(AErrorMessages: TStrings): WordBool;
const OPNAME = 'TCorrespondingChannel.ValidateAssumedFactor';
var
  LMessage : string;
begin
  Result := False;
  try
       LMessage := '';
    if (not FAppModules.FieldProperties.ValidateFieldProperty('AssumedFactor',
            FloatToStr(FAssumedFactor), LMessage)) then
      AErrorMessages.Add('ERROR:'+FloatToStr(FAssumedFactor) + ':'+LMessage)
    else
      Result := True;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TCorrespondingChannel.ValidateChannelNumber(AErrorMessages: TStrings): WordBool;
const OPNAME = 'TCorrespondingChannel.ValidateChannelNumber';
var
  LMessage : string;
  LChannelList : IChannelList;
  LChannel : IGeneralFlowChannel;
  LCorrespondingChannel : ICorrespondingChannel;
  LReturnFlowChannel : IReturnFlowChannel;
  LIndex : integer;
begin
  Result := False;
  try
    LMessage := '';
    Result := FAppModules.FieldProperties.ValidateFieldProperty('CorrespondingChannel',
            IntToStr(FChannelNumber), LMessage);
    if (not Result) then
      AErrorMessages.Add('ERROR:'+IntToStr(FChannelNumber) + ':'+LMessage)
    else
    begin
      LChannelList := TYieldModelDataObject(FAppModules.Model.ModelData).
                        NetworkElementData.ChannelList;
      LChannel     := LChannelList.ChannelByChannelNumber[FDemandChannel];
      LReturnFlowChannel := LChannel.ReturnFlowChannel;
      for LIndex := 0 to LReturnFlowChannel.NumOfCorrespondingChannels -1 do
      begin
        LCorrespondingChannel := LReturnFlowChannel.CorrespondingChannelByIndex[LIndex];
        if LCorrespondingChannel <> nil then
        begin
          if (LCorrespondingChannel.ChannelNumber = FChannelNumber) then
          begin
            LMessage := FAppModules.Language.GetString('ContextValidation.DuplicateChannel');
            Result := False;
          end
          else
            Result := True;
        end
      end;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

end.
