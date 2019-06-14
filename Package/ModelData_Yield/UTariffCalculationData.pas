//
//
//  UNIT      : Contains TTariffCalculationData Class
//  AUTHOR    : Sam Dhlamini(Cornastone)
//  DATE      : 13/06/2006
//  COPYRIGHT : Copyright © 2004 DWAF
//
//

unit UTariffCalculationData;

interface
uses
  SysUtils,
  Classes,
  Contnrs,
  VoaimsCom_TLB,
  UAbstractObject,
  UYieldModelDataObject,
  UTariffCalculationSQLAgent;
type

  TChannelTariff = class(TAbstractAppObject,IChannelTariff)
  protected
    FIdentifier        : integer;
    FChannelNumber     : Integer;
    FTariff     : Double;
    FEscalationFactors : string;

    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;

    function Get_Identifier: Integer; safecall;
    function Get_ChannelNumber: Integer; safecall;
    procedure Set_ChannelNumber(AValue: Integer); safecall;
    function Get_Tariff: Double; safecall;
    procedure Set_Tariff(AValue: Double); safecall;
    function Get_EscalationFactors: WideString; safecall;
    procedure Set_EscalationFactors(const AValue: WideString); safecall;

    function ValidateChannelNumber(AErrorMessages : TStrings) : WordBool;
    function ValidateTariff(AErrorMessages : TStrings) : WordBool;
    function ValidateEscalationFactors(AErrorMessages : TStrings) : WordBool;
  public
    function Initialise: boolean;override;
    function Populate(AIdentifier: Integer; AChannelNumber: Integer; ATariff: Double;
                      const AEscalationFactors: WideString): WordBool; stdcall;
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; stdcall;
    property Identifier    : integer read FIdentifier;
    property ChannelNumber : Integer read Get_ChannelNumber write Set_ChannelNumber;
    property Tariff : double read Get_Tariff write Set_Tariff;
    property EscalationFactors : WideString read Get_EscalationFactors write Set_EscalationFactors;
  end;

  TTariffCalculationData = class(TAbstractAppObject,ITariffCalculationData)
  protected
    FDataYears : integer;
    FChannelTariffList  : TObjectList;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    function Get_ChannelIdentifierByChannelNumber(AChannelNumber: Integer): Integer;
    function Get_ChannelTariffCount: Integer; safecall;
    function Get_ChannelTariffByIndex(AIndex: Integer): IChannelTariff; safecall;
    function Get_ChannelTariffByChannelNumber(AChannelNumber: Integer): IChannelTariff; safecall;
    function Get_DataYears: Integer; safecall;
    procedure Set_DataYears(Value: Integer); safecall;
  public
    function Initialise: boolean;override;
    function CreateChannelTariff : TChannelTariff;
    function DeleteChannelTariff(AChannelNumber : integer) : boolean;
    function NewChannelTariff(AChannelNumber: Integer): WordBool; safecall;
    function RemoveChannelTariffByChannelNumber(AChannelNumber: Integer): WordBool; safecall;
    function Populate(ADataYears: Integer): WordBool; safecall;
    property ChannelTariffCount: Integer read Get_ChannelTariffCount;
    property ChannelTariffByIndex[AIndex: Integer]: IChannelTariff read Get_ChannelTariffByIndex;
    property ChannelTariffByChannelNumber[AChannelNumber: Integer]: IChannelTariff read Get_ChannelTariffByChannelNumber;
    property DataYears: Integer read Get_DataYears write Set_DataYears;
  end;

implementation
uses
  System.Types,
  UConstants,
  UDataSetType,
//  UPlanningModelDataObject,
  UErrorHandlingOperations;


{ TChannelTariff }

function TChannelTariff._AddRef: Integer;
const OPNAME = 'TChannelTariff._AddRef';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TChannelTariff._Release: Integer;
const OPNAME = 'TChannelTariff._Release';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TChannelTariff.CreateMemberObjects;
const OPNAME = 'TChannelTariff.CreateMemberObjects';
begin
  inherited CreateMemberObjects;
  try
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TChannelTariff.DestroyMemberObjects;
const OPNAME = 'TChannelTariff.DestroyMemberObjects';
begin
  try
    inherited DestroyMemberObjects;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TChannelTariff.Get_EscalationFactors: WideString;
const OPNAME = 'TChannelTariff.Get_EscalationFactors';
begin
  Result := '';
  try
    Result := FEscalationFactors;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TChannelTariff.Get_Identifier: Integer;
const OPNAME = 'TChannelTariff.Get_ChannelNumber';
begin
  Result := 0;
  try
    Result := FIdentifier;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TChannelTariff.Get_ChannelNumber: integer;
const OPNAME = 'TChannelTariff.Get_ChannelNumber';
begin
  Result := 0;
  try
    Result := FChannelNumber;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TChannelTariff.Get_Tariff: double;
const OPNAME = 'TChannelTariff.Get_Tariff';
begin
  Result := NullFloat;
  try
    Result := FTariff;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TChannelTariff.Initialise: boolean;
const OPNAME = 'TChannelTariff.Initialise';
begin
  Result := False;
  try
    FIdentifier        := NullInteger;
    FChannelNumber     := NullInteger;
    FTariff            := NullFloat;
    FEscalationFactors := '';
    Result := True;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TChannelTariff.Populate(AIdentifier: Integer; AChannelNumber: Integer;
         ATariff: Double; const AEscalationFactors: WideString): WordBool;
const OPNAME = 'TChannelTariff.Populate';
begin
  Result := False;
  try
    FIdentifier        := AIdentifier;
    FChannelNumber     := AChannelNumber;
    FTariff            := ATariff;
    FEscalationFactors := AEscalationFactors;
    Result := True;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TChannelTariff.Set_EscalationFactors(const AValue: WideString);
const OPNAME = 'TChannelTariff.Set_EscalationFactors';
var
  LLoadAgent : TTariffCalculationSQLAgent;
  LContextData : TStringList;
  LOldValue : string;
begin
  try
    LLoadAgent := TTariffCalculationSQLAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        LLoadAgent.LoadContextData(LContextData, IntToStr(FIdentifier));
        if (FAppModules.FieldProperties.UpdateFieldValue(
           'EscalationFactors', AValue, FEscalationFactors, LContextData)) then
        begin
          LOldValue := FEscalationFactors;
          FEscalationFactors := AValue;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'EscalationFactors',LOldValue,AValue);
        end;
      finally
        LContextData.Free;
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TChannelTariff.Set_ChannelNumber(AValue: integer);
const OPNAME = 'TChannelTariff.Set_ChannelNumber';
var
  LLoadAgent : TTariffCalculationSQLAgent;
  LContextData : TStringList;
  LOldValue : string;
begin
  try
    LLoadAgent := TTariffCalculationSQLAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        LLoadAgent.LoadContextData(LContextData, IntToStr(FIdentifier));
        if (FAppModules.FieldProperties.UpdateFieldValue(
           'ChannelNumber', IntToStr(AValue), IntToStr(FChannelNumber), LContextData)) then
        begin
          LOldValue := IntToStr(FChannelNumber);
          FChannelNumber := AValue;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'ChannelNumber',LOldValue,IntToStr(AValue));
        end;
      finally
        LContextData.Free;
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TChannelTariff.Set_Tariff(AValue: double);
const OPNAME = 'TChannelTariff.Set_Tariff';
var
  LLoadAgent : TTariffCalculationSQLAgent;
  LContextData : TStringList;
  LOldValue : string;
begin
  try
    LLoadAgent := TTariffCalculationSQLAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        LLoadAgent.LoadContextData(LContextData, IntToStr(FIdentifier));
        if (FAppModules.FieldProperties.UpdateFieldValue(
           'Tariff', FloatToStr(AValue), FloatToStr(FTariff), LContextData)) then
        begin
          LOldValue := FloatToStr(FTariff);
          FTariff := AValue;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'Tariff',LOldValue,FloatToStr(AValue));
        end;
      finally
        LContextData.Free;
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TChannelTariff.Validate(var AErrors: WideString;const AContext: WideString): WordBool;
const OPNAME = 'TChannelTariff.Validate';
var
  LErrorCols        : TStringList;
  LErrorMsgs        : TStringList;
begin
  Result := True;
  try
    LErrorCols := TStringList.Create;
    LErrorMsgs := TStringList.Create;
    try
      {if (AContext = 'DemandChannel') then
        Result := ValidateDemandChannel(LErrorMsgs)
      else
      if (AContext = 'ReturnFlowGaugeNumber' ) then
        Result := ValidateChannelNumber(LErrorMsgs)
      else
      if (AContext = 'MonthlyAvrgFactor' ) then
        Result := ValidateChannelTariff(LErrorMsgs);

      if (not Result) then
      begin
        if (LErrorCols.Count = 0) then
          AErrors := AErrors + LErrorMsgs.Text
        else
          AErrors := AErrors + CTStringsSeparator + LErrorMsgs.Text +
                     CTStringsSeparator + LErrorCols.Text + CTStringsSeparator;
        LErrorMsgs.Clear;
        LErrorCols.Clear;
      end;}
    finally
      FreeAndNil(LErrorCols);
      FreeAndNil(LErrorMsgs);
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TChannelTariff.ValidateChannelNumber(AErrorMessages: TStrings): WordBool;
const OPNAME = 'TChannelTariff.ValidateChannelNumber';
{var
  LMessage : string; }
begin
  Result := True;
  try
    {LMessage := '';
    if (not FAppModules.FieldProperties.ValidateFieldProperty('ChannelNumber',
            FloatToStr(FChannelNumber), LMessage)) then
      AErrorMessages.Add( 'ERROR:'+FloatToStr(FChannelNumber) + ':'+LMessage)
    else
      Result := True;}
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TChannelTariff.ValidateTariff(AErrorMessages: TStrings): WordBool;
const OPNAME = 'TChannelTariff.ValidateTariff';
{var
  LMessage : string; }
begin
  Result := True;
  try
    {LMessage := '';
    if (not FAppModules.FieldProperties.ValidateFieldProperty('Tariff',
            FloatToStr(FTariff), LMessage)) then
      AErrorMessages.Add( 'ERROR:'+FloatToStr(FTariff) + ':'+LMessage)
    else
      Result := True;}
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TChannelTariff.ValidateEscalationFactors(AErrorMessages: TStrings): WordBool;
const OPNAME = 'TChannelTariff.ValidateEscalationFactors';
{var
  LMessage : string; }
begin
  Result := True;
  try
    {LMessage := '';
    if (not FAppModules.FieldProperties.ValidateFieldProperty('CalibrationFactor',
            FloatToStr(FCalibrationFactor), LMessage)) then
      AErrorMessages.Add( 'ERROR:'+FloatToStr(FCalibrationFactor) + ':'+LMessage)
    else
      Result := True;}
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;


{ TTariffCalculationData }

function TTariffCalculationData._AddRef: Integer;
const OPNAME = 'TTariffCalculationData._AddRef';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TTariffCalculationData._Release: Integer;
const OPNAME = 'TTariffCalculationData._Release';
begin
   Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TTariffCalculationData.CreateMemberObjects;
const OPNAME = 'TTariffCalculationData.CreateMemberObjects';
begin
  inherited CreateMemberObjects;
  try
    FChannelTariffList := TObjectList.Create(False);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TTariffCalculationData.DestroyMemberObjects;
const OPNAME = 'TTariffCalculationData.DestroyMemberObjects';
begin
  try
    inherited DestroyMemberObjects;
    FreeAndNil(FChannelTariffList);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TTariffCalculationData.CreateChannelTariff: TChannelTariff;
const OPNAME = 'TTariffCalculationData.CreateChannelTariff';
begin
  Result := nil;
  try
    Result := TChannelTariff.Create(FAppModules);
    FChannelTariffList.Add(Result);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TTariffCalculationData.DeleteChannelTariff(AChannelNumber: integer): boolean;
const OPNAME = 'TTariffCalculationData.DeleteChannelTariff';
var
  LIndex: integer;
begin
  Result := False;
  try
    for LIndex := 0 to FChannelTariffList.Count -1 do
    begin
      if TChannelTariff(FChannelTariffList.Items[LIndex]).FChannelNumber = AChannelNumber then
      begin
        FChannelTariffList.Remove(TChannelTariff(FChannelTariffList.Items[LIndex]));
        Result := True;
        Break;
      end;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;


function TTariffCalculationData.Get_ChannelIdentifierByChannelNumber(AChannelNumber: Integer): Integer;
const OPNAME = 'TTariffCalculationData.Get_ChannelIdentifierByChannelNumber';
var
  LIndex: integer;
begin
  Result := NullInteger;
  try
    for LIndex := 0 to FChannelTariffList.Count -1 do
    begin
      if(TChannelTariff(FChannelTariffList.Items[LIndex]).FChannelNumber = AChannelNumber) then
      begin
        Result := TChannelTariff(FChannelTariffList.Items[LIndex]).FIdentifier;
        Break;
      end;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TTariffCalculationData.Get_ChannelTariffCount: integer;
const OPNAME = 'TTariffCalculationData.Get_ChannelTariffCount';
begin
  Result := 0;
  try
    Result := FChannelTariffList.Count;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TTariffCalculationData.Get_DataYears: Integer;
const OPNAME = 'TTariffCalculationData.Get_DataYears';
begin
  Result := 0;
  try
    Result := FDataYears;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TTariffCalculationData.Set_DataYears(Value: Integer);
const OPNAME = 'TTariffCalculationData.Set_DataYears';
begin
  try
    FDataYears := Value;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TTariffCalculationData.Populate(ADataYears: Integer): WordBool;
const OPNAME = 'TTariffCalculationData.Populate';
begin
  Result := False;
  try
    FDataYears := ADataYears;
    Result     := True;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TTariffCalculationData.Initialise: boolean;
const OPNAME = 'TTariffCalculationData.Initialise';
begin
  Result := False;
  try
    FDataYears := 0;
    FChannelTariffList.Clear;
    Result := True;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TTariffCalculationData.NewChannelTariff(AChannelNumber: Integer): WordBool;
const OPNAME = 'TTariffCalculationData.NewChannelTariff';
var
  LLoadAgent : TTariffCalculationSQLAgent;
  LIdentifier : integer;
  LChannelTariff : TChannelTariff;
begin
  try
    LLoadAgent := TTariffCalculationSQLAgent.Create(FAppModules);
    try
      if LLoadAgent.InsertTariffCalculation(AChannelNumber,LIdentifier) then
      begin
        LChannelTariff := CreateChannelTariff;
        LChannelTariff.FChannelNumber     := AChannelNumber;
        LChannelTariff.FIdentifier        := LIdentifier;
        LChannelTariff.Tariff             :=  0.50;
        LChannelTariff.EscalationFactors  := IntToStr(FDataYears)+'*0.0';
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TTariffCalculationData.RemoveChannelTariffByChannelNumber(AChannelNumber: integer): WordBool;
const OPNAME = 'TTariffCalculationData.RemoveChannelTariffByChannelNumber';
var
  LLoadAgent : TTariffCalculationSQLAgent;
  LChannelIDentifier : integer;
begin
  Result := False;
  try
    LChannelIDentifier := Get_ChannelIdentifierByChannelNumber(AChannelNumber);
    if(LChannelIDentifier > 0) then
    begin
      LLoadAgent := TTariffCalculationSQLAgent.Create(FAppModules);
      try
        if LLoadAgent.DeleteTariffCalculation(LChannelIDentifier) then
          Result := DeleteChannelTariff(AChannelNumber);
      finally
        LLoadAgent.Free;
      end;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TTariffCalculationData.Get_ChannelTariffByChannelNumber(AChannelNumber: Integer): IChannelTariff;
const OPNAME = 'TTariffCalculationData.Get_ChannelTariffByChannelNumber';
var
  LIndex : integer;
begin
  Result := nil;
  try
    LIndex := 0;
    while (Result = nil) and (LIndex < FChannelTariffList.Count) do
    begin
      if(TChannelTariff(FChannelTariffList.Items[LIndex]).FChannelNumber = AChannelNumber) then
        Result := TChannelTariff(FChannelTariffList.Items[LIndex])
      else
      inc(LIndex);
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TTariffCalculationData.Get_ChannelTariffByIndex(AIndex: Integer): IChannelTariff;
const OPNAME = 'TTariffCalculationData.Get_ChannelTariffByIndex';
begin
  Result := nil;
  try
    if (AIndex >=0) and (AIndex < FChannelTariffList.Count) then
      Result := TChannelTariff(FChannelTariffList.Items[AIndex]);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;


end.
