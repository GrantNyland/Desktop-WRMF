unit UMultiResChannelCurtailmentData;

interface
uses
  Classes,
  Contnrs,
  UAbstractObject,
  VoaimsCom_TLB;

type
  TMultiResChannelCurtail = class(TAbstractAppObject, IMultiResMultiChannelCurtail)

  protected
    FIdentifier : integer;
    FChannelNo: Integer;
    FReservoirNo: Integer;

    FStartMonth: Integer;
    FDecisionMonth: Integer;

    FElevation : TElevationsArray;
    FRestrictionFactor : TElevationsArray;

    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;

    function Get_StartMonth: Integer; safecall;
    procedure Set_StartMonth(Value: Integer); safecall;
    function Get_ReservoirNo: Integer; safecall;
    procedure Set_ReservoirNo(Value: Integer); safecall;
    function Get_ChannelNo: Integer; safecall;
    procedure Set_ChannelNo(Value: Integer); safecall;
    function Get_DecisionMonth: Integer; safecall;
    procedure Set_DecisionMonth(Value: Integer); safecall;
    function Get_ElevationByIndex(AIndex: Integer): Double; safecall;
    procedure Set_ElevationByIndex(AIndex: Integer; Value: Double); safecall;
    function Get_FactorByIndex(AIndex: Integer): Double; safecall;
    procedure Set_FactorByIndex(AIndex: Integer; Value: Double); safecall;
    function Get_Identifier: integer; safecall;
    procedure Set_Identifier(Value: integer); safecall;

    function ValidateChannelNo(AErrorMessages: TStrings): wordbool;
    function ValidateReservoirNo(AErrorMessages: TStrings): wordbool;
    function ValidateDecisionMonth(AErrorMessages: TStrings): wordbool;
    function ValidateStartMonth(AErrorMessages: TStrings): wordbool;
    function ValidateElevation(AErrorMessages: TStrings; AErrorColumns: TStrings): wordbool;
    function ValidateRestrictionFactor(AErrorMessages: TStrings; AErrorColumns: TStrings): wordbool;

  public
    function populate( AIdentifier,
                       AChannelNo,
                       AReservoirNo,
                       AStartMonth,
                       ADecisionMonth: Integer;
                       AElevation : TElevationsArray;
                       ARestrictionFactor : TElevationsArray) : boolean;

    function Validate(var AError: WideString; const AContext: WideString): WordBool; safecall;
    function ValidateGrid(var AError: WideString; const AContext: Widestring; ACol, ARow: Integer): WordBool; safecall;
    function Initialise: Boolean; override;
    property Identifier: Integer read Get_Identifier write FIdentifier;
    property StartMonth: Integer read Get_StartMonth write Set_StartMonth;

    property ReservoirNo: Integer read Get_ReservoirNo write Set_ReservoirNo;
    property ChannelNo: Integer read Get_ChannelNo write Set_ChannelNo;

    property DecisionMonth: Integer read Get_DecisionMonth write Set_DecisionMonth;

    property ElevationByIndex[AIndex: Integer]: Double read Get_ElevationByIndex write Set_ElevationByIndex;
    property FactorByIndex[AIndex: Integer]: Double read Get_FactorByIndex write Set_FactorByIndex;

end;

type
  TMultiResChannelCurtailmentList = class(TAbstractAppObject,  IMultiResMultiChannelCurtailList)
  protected
    FMultiCurtailData : TObjectList;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    function Get_RestrictionByReservoirNo(AReservoirNo: Integer): IMultiResMultiChannelCurtail; safecall;
    function Get_RestrictionByChannelNo(AChannelNo: Integer): IMultiResMultiChannelCurtail; safecall;
    function Get_RestrictionByIndentifier(AIdentifier: Integer): IMultiResMultiChannelCurtail; safecall;



  public
    function AddRestriction : TMultiResChannelCurtail;
    function DeleteRestriction(AIdentifier : integer): boolean;
    function NewRestriction(AChannelNo : integer): IMultiResMultiChannelCurtail; safecall;
    function RemoveRestriction(AChannelNo : integer): WordBool; safecall;
    property RestrictionByReservoirNo[AIndex: Integer]: IMultiResMultiChannelCurtail read Get_RestrictionByReservoirNo;
    property RestrictionByChannelNo[AIndex: Integer]: IMultiResMultiChannelCurtail read Get_RestrictionByChannelNo;
    property RestrictionByIndentifier[AIdentifier: Integer]: IMultiResMultiChannelCurtail read Get_RestrictionByIndentifier;
end;



implementation
uses
      SysUtils,
       UConstants,
      //UAbstractAppObject,
      UChannelData,
      UYieldModelDataObject,
      UMultiRestrictionSQLAgent,
      UMultiRestrictionLoadAgent,
      UErrorHandlingOperations;


function TMultiResChannelCurtailmentList.AddRestriction: TMultiResChannelCurtail;
const OPNAME = 'TMultiResChannelCurtailmentList.CreateMemberObjects';
begin
  Result := nil;
  try
    Result := TMultiResChannelCurtail.Create(FAppModules);
    FMultiCurtailData.Add(Result);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TMultiResChannelCurtailmentList.CreateMemberObjects;
const OPNAME = 'TMultiResChannelCurtailmentList.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;

    FMultiCurtailData := TObjectList.Create(False);

  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TMultiResChannelCurtailmentList.DestroyMemberObjects;
const OPNAME = 'TMultiResChannelCurtailmentList.DestroyMemberObjects';
begin
  try
    inherited DestroyMemberObjects;
    FreeAndNil(FMultiCurtailData);
  except on E: Exception do HandleError(E, OPNAME); end;
end;


function TMultiResChannelCurtailmentList._AddRef: Integer;
const OPNAME = 'TMultiResChannelCurtailmentList._AddRef';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TMultiResChannelCurtailmentList._Release: Integer;
const OPNAME = 'TMultiResChannelCurtailmentList._Release';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;



function TMultiResChannelCurtailmentList.Get_RestrictionByReservoirNo(AReservoirNo: Integer):IMultiResMultiChannelCurtail;
const OPNAME = 'TMultiResChannelCurtailmentList.Get_RestrictionByReservoirNo';
var LCount : integer;
begin
  Result := nil;
  try
    for LCount := 0 to FMultiCurtailData.Count -1 do
    begin
      if TMultiResChannelCurtail(FMultiCurtailData[LCount]).ReservoirNo = AReservoirNo then
      begin
        Result := TMultiResChannelCurtail(FMultiCurtailData[LCount]);
        break;
      end;
    end;

  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TMultiResChannelCurtailmentList.Get_RestrictionByChannelNo(AChannelNo: Integer): IMultiResMultiChannelCurtail;
const OPNAME = 'TMultiResChannelCurtailmentList.Get_RestrictionByChannelNo';
var LCount : integer;
begin
  Result := nil;
  try
    for LCount := 0 to FMultiCurtailData.Count-1 do
    begin
      if (TMultiResChannelCurtail(FMultiCurtailData[LCount]).FChannelNo = AChannelNo) then
      begin
        Result := TMultiResChannelCurtail(FMultiCurtailData[LCount]);
        break;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TMultiResChannelCurtailmentList.Get_RestrictionByIndentifier(AIdentifier: Integer): IMultiResMultiChannelCurtail;
  const OPNAME = 'TMultiResChannelCurtailmentList.Get_RestrictionByIndentifier';
var
  LCount : integer;
begin
  Result := nil;
  try
    for LCount := 0 to FMultiCurtailData.Count - 1 do
    begin
      if TMultiResChannelCurtail(FMultiCurtailData[LCount]).FIdentifier = AIdentifier then
      begin
        Result :=TMultiResChannelCurtail(FMultiCurtailData[LCount]);
        Break;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMultiResChannelCurtailmentList.NewRestriction(AChannelNo : integer): IMultiResMultiChannelCurtail;
const OPNAME = 'TMultiResChannelCurtailmentList.NewRestriction';
var
  LFeatureID : integer;
  LLoadAgent : TMultiRestrictionLoadAgent;
  LFeature   : TMultiResChannelCurtail;
  LConfig    : IRunConfigurationData;
  LElev : TElevationsArray;
  LFactor : TElevationsArray;
  LChannel : IGeneralFlowChannel;
  LReservoirNo : integer;
  LDecisionMonth : integer;
begin
  Result := nil;
  try
    LLoadAgent := TMultiRestrictionLoadAgent.Create(FAppModules);
    try
      LFeature :=  AddRestriction;
      LFeatureID   := LLoadAgent.GetLastMultiRestrictionID+1;
      LConfig    := TYieldModelDataObject(FAppModules.Model.ModelData).RunConfigurationData;
      LChannel   := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.ChannelList.ChannelByChannelNumber[AChannelNo];
      if (LFeatureID > 0) and (LChannel <> nil) then
      begin
        SetLength(LElev,10);
        SetLength(LFactor,10);
        LReservoirNo := 0;
        LDecisionMonth := 1;
        if LChannel.UpStreamNode <> nil then
        begin
          LReservoirNo := LChannel.UpStreamNode.ReservoirConfigurationData.ReservoirIdentifier;
          if (LConfig.NrOfDecisionMonths > 0) then
            LDecisionMonth := LConfig.DecisionMonthByIndex[1];
        end;
        LFeature.Initialise;
        if Assigned(LConfig) then

          if (LConfig.AllocationControlOption = 'I')  then
          begin
            LFeature.Populate(LFeatureID, AChannelNo,LReservoirNo,LConfig.StartMonthNumber,LDecisionMonth,LElev,LFactor);

            if not LLoadAgent.InsertMultiRestriction(LFeature) then
              DeleteRestriction(LFeatureID)

          end
          else
            DeleteRestriction(LFeatureID);

          Result := LFeature;
      end;
    finally
      Finalize(LFactor);
      Finalize(LElev);
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TMultiResChannelCurtailmentList.RemoveRestriction(AChannelNo : integer): wordbool;
const OPNAME = 'TMultiResChannelCurtailmentList.RemoveRestriction';
var
  lFeature  :  TMultiResChannelCurtail;
  LLoadAgent :  TMultiRestrictionLoadAgent;
  LChannelNo,
  LResevoirNo,
  LID : integer;
begin
  Result := FALSE;
  try
    lFeature    :=   TMultiResChannelCurtail(RestrictionByChannelNo[AChannelNo]);
    LLoadAgent   :=   TMultiRestrictionLoadAgent.Create(FAppModules);
    try
      if lFeature <> nil then
      begin
        LID := lFeature.Identifier;
        LChannelNo := lFeature.ChannelNo;
        LResevoirNo := lFeature.ReservoirNo;
        if LLoadAgent.DeleteMultiRestriction(LID, LChannelNo,LResevoirNo) then
          Result := DeleteRestriction(LID);
      end;
    finally
      FreeAndNil(LLoadAgent)
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;


//===========================================================================================


procedure TMultiResChannelCurtail.CreateMemberObjects;
const OPNAME = 'TMultiResChannelCurtailmentList.CreateMemberObjects';
var
  LElevation : TAbstractFieldProperty;
  LFactor : TAbstractFieldProperty;
begin
  try
    inherited CreateMemberObjects;

    LElevation := FAppModules.FieldProperties.FieldProperty('MultiCurElevation');
    if not Assigned(LElevation) then
      raise Exception.Create('Field (MultiCurElevation) not found in field properties');
    SetLength(FElevation,LElevation.ArrayLength-1);

    LFactor := FAppModules.FieldProperties.FieldProperty('MultiCurFactor');
    if not Assigned(LFactor) then
      raise Exception.Create('Field (MultiCurFactor) not found in field properties');
    SetLength(FRestrictionFactor,LFactor.ArrayLength-1);

  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TMultiResChannelCurtailmentList.DeleteRestriction(AIdentifier: integer): boolean;
const OPNAME = 'TMultiResChannelCurtailmentList.DestroyMemberObjects';
var
  LIndex : integer;
  LRestriction : TMultiResChannelCurtail;
begin
  Result := False;
  try
    for LIndex := 0 to FMultiCurtailData.Count -1 do
    begin
      LRestriction := TMultiResChannelCurtail(FMultiCurtailData[LIndex]);
      if (LRestriction <> nil) then
      begin
        if (LRestriction.FIdentifier = AIdentifier) then
        begin
          FMultiCurtailData.Delete(LIndex);
          Result := True;
          Break;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;




function TMultiResChannelCurtail._AddRef: Integer;
const OPNAME = ' TMultiResChannelCurtail._AddRef';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TMultiResChannelCurtail._Release: Integer;
const OPNAME = 'TMultiResChannelCurtail._Release';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;




function TMultiResChannelCurtail.Get_StartMonth: integer;
const OPNAME = 'TMultiResChannelCurtail.Get_StartMonth';
begin
  Result := 0;
  try
    Result := FStartMonth;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TMultiResChannelCurtail.DestroyMemberObjects;
const OPNAME = 'TMultiResChannelCurtail.Initialise';
begin
  try

    Finalize(FElevation);
    Finalize(FRestrictionFactor);

    inherited DestroyMemberObjects;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TMultiResChannelCurtail.Initialise: Boolean;
const OPNAME = 'TMultiResChannelCurtail.Initialise';
var
  LElevation : TAbstractFieldProperty;
  LFactor : TAbstractFieldProperty;
  LCount : integer;
begin
  Result := False;
  try
    LElevation := FAppModules.FieldProperties.FieldProperty('MultiCurElevation');
    for LCount := LElevation.ArrayLow to LElevation.ArrayHigh do
      FElevation[LCount-1] :=  NullFloat;

    LFactor := FAppModules.FieldProperties.FieldProperty('MultiCurFactor');

    for LCount := LFactor.ArrayLow to LFactor.ArrayHigh do
      FRestrictionFactor[LCount-1] :=  NullFloat;

  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TMultiResChannelCurtail.populate(AIdentifier, AChannelNo,
                       AReservoirNo,
                       AStartMonth,
                       ADecisionMonth: Integer;
                       AElevation : TElevationsArray;
                       ARestrictionFactor : TElevationsArray) : boolean;
const OPNAME = 'TMultiResChannelCurtail.populate';
var
 // LElevation : TAbstractFieldProperty;
 // LFactor : TAbstractFieldProperty;
  LCount : integer;
begin
  Result := False;
  try

    FIdentifier := AIdentifier;
    FChannelNo := AChannelNo;
    FReservoirNo := AReservoirNo;
    FStartMonth := AStartMonth;
    FDecisionMonth := ADecisionMonth;

   // LElevation := FAppModules.FieldProperties.FieldProperty('MultiCurElevation');
    for LCount := 0 to  9 do
      FElevation[LCount] :=  AElevation[LCount];

    //LFactor := FAppModules.FieldProperties.FieldProperty('MultiCurFactor');
    for LCount :=  0 to  9 do
      FRestrictionFactor[LCount] :=  ARestrictionFactor[LCount];

  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TMultiResChannelCurtail.Get_ChannelNo: integer;
const OPNAME = 'TMultiResChannelCurtail.Get_ChannelNo';
begin
  Result := 0;
  try
    Result := FChannelNo;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TMultiResChannelCurtail.Get_ReservoirNo: integer;
const OPNAME = 'TMultiResChannelCurtail.Get_ReservoirNo';
begin
  Result := 0;
  try
    Result := FReservoirNo;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TMultiResChannelCurtail.Get_DecisionMonth: integer;
const OPNAME = 'TMultiResChannelCurtail.Get_DecisionMonth';
begin
  Result := 0;
  try
    Result := FDecisionMonth;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TMultiResChannelCurtail.Get_ElevationByIndex(AIndex: Integer): double;
const OPNAME = 'TMultiResChannelCurtail.Get_ElevationByIndex';
var
LHigh: integer;
begin
  Result := NullFloat;
  try
    LHigh:=High(FElevation);
    if(AIndex >= Low(FElevation) ) And (AIndex <= LHigh)  then
    begin
      Result := FElevation[AIndex];
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TMultiResChannelCurtail.Get_FactorByIndex(AIndex: Integer): double;
const OPNAME = 'TMultiResChannelCurtail.Get_FactorByIndex';
begin
  Result := 0;
  try
    if(AIndex >= Low(FRestrictionFactor)) and (AIndex <= High(FRestrictionFactor) ) then
    begin
      Result := FRestrictionFactor[AIndex];
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TMultiResChannelCurtail.Set_StartMonth(Value: Integer);
const OPNAME = 'TMultiResChannelCurtail.Set_StartMonth';
var
  LSQLAgent : TMultiRestrictionSQLAgent;
  LContextData : TStringList;
  LOldValue : string;
begin
  try
    LSQLAgent := TMultiRestrictionSQLAgent.Create(FAppModules);
    LContextData := TStringList.Create;
    try
      LSQLAgent.LoadContextData(LContextData, IntToStr(FIdentifier));
      if (FAppModules.FieldProperties.UpdateFieldValue(
           'MultiCurStartMonth', IntToStr(Value), IntToStr(FStartMonth), LContextData)) then
      begin
        LOldValue := IntToStr(FStartMonth);
        FStartMonth := Value;
        FAppModules.Model.StudyDataHasChanged(sdccEdit,'MultiCurStartMonth',LOldValue,IntToStr(Value));
      end;
    finally
      FreeAndNil(LSQLAgent);
      FreeAndNil(LContextData);
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TMultiResChannelCurtail.Set_ReservoirNo(Value: Integer);
const OPNAME = 'TMultiResChannelCurtail.Set_ReservoirNo';
var
  LSQLAgent : TMultiRestrictionSQLAgent;
  LContextData : TStringList;
  LOldValue : string;
begin
  try
    LSQLAgent := TMultiRestrictionSQLAgent.Create(FAppModules);
    LContextData := TStringList.Create;
    try
      LSQLAgent.LoadContextData(LContextData, IntToStr(FIdentifier));
      if (FAppModules.FieldProperties.UpdateFieldValue(
           'MultiCurReservoir', IntToStr(Value), IntToStr(FReservoirNo), LContextData)) then
      begin
        LOldValue := IntToStr(FReservoirNo);
        FReservoirNo := Value;
        FAppModules.Model.StudyDataHasChanged(sdccEdit,'MultiCurReservoir',LOldValue,IntToStr(Value));
      end;
    finally
      FreeAndNil(LSQLAgent);
      FreeAndNil(LContextData);
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TMultiResChannelCurtail.Set_ChannelNo(Value: Integer);
const OPNAME = 'TMultiResChannelCurtail.Set_ReservoirNo';
var
  LSQLAgent : TMultiRestrictionSQLAgent;
  LContextData : TStringList;
  LOldValue : string;
begin
  try
    LSQLAgent := TMultiRestrictionSQLAgent.Create(FAppModules);
    LContextData := TStringList.Create;
    try
      LSQLAgent.LoadContextData(LContextData, IntToStr(FIdentifier));
      if (FAppModules.FieldProperties.UpdateFieldValue(
           'MultiCurChannel', IntToStr(Value), IntToStr(FChannelNo), LContextData)) then
      begin
        LOldValue := IntToStr(FChannelNo);
        FChannelNo := Value;
        FAppModules.Model.StudyDataHasChanged(sdccEdit,'MultiCurChannel',LOldValue,IntToStr(Value));
      end;
    finally
      FreeAndNil(LSQLAgent);
      FreeAndNil(LContextData);
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TMultiResChannelCurtail.Set_DecisionMonth(Value: Integer);
const OPNAME = 'TMultiResChannelCurtail.Set_DecisionMonth';
var
  LSQLAgent : TMultiRestrictionSQLAgent;
  LContextData : TStringList;
  LOldValue : string;
begin
  try
    LSQLAgent := TMultiRestrictionSQLAgent.Create(FAppModules);
    LContextData := TStringList.Create;
    try
      LSQLAgent.LoadContextData(LContextData, IntToStr(FIdentifier));
      if (FAppModules.FieldProperties.UpdateFieldValue(
           'MultiCurDecisionMonth', IntToStr(Value), IntToStr(FDecisionMonth), LContextData)) then
      begin
        LOldValue := IntToStr(FDecisionMonth);
        FDecisionMonth := Value;
        FAppModules.Model.StudyDataHasChanged(sdccEdit,'MultiCurDecisionMonth',LOldValue,IntToStr(Value));
      end;
    finally
      FreeAndNil(LSQLAgent);
      FreeAndNil(LContextData);
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TMultiResChannelCurtail.Set_ElevationByIndex(AIndex: Integer; Value: Double);
const OPNAME = 'TMultiResChannelCurtail.Set_ElevationByIndex';
var
  LSQLAgent : TMultiRestrictionSQLAgent;
  LContextData : TStringList;
  LOldValue : string;
begin
  try
    LSQLAgent := TMultiRestrictionSQLAgent.Create(FAppModules);
    LContextData := TStringList.Create;
    try
      LSQLAgent.LoadContextData(LContextData, IntToStr(FIdentifier),IntToStr(AIndex+1));
      if (FAppModules.FieldProperties.UpdateFieldValue(
           'MultiCurElevation', FloatToStr(Value), FloatToStr(FElevation[AIndex]), LContextData)) then
      begin
        LOldValue := FloatToStr(FElevation[AIndex]);
        FElevation[AIndex] := Value;
        FAppModules.Model.StudyDataHasChanged(sdccEdit,'MultiCurElevation',LOldValue,FloatToStr(Value));
      end;
    finally
      FreeAndNil(LSQLAgent);
      FreeAndNil(LContextData);
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TMultiResChannelCurtail.Set_FactorByIndex(AIndex: Integer; Value: Double);
const OPNAME = 'TMultiResChannelCurtail.Set_FactorByIndex';
var
  LSQLAgent : TMultiRestrictionSQLAgent;
  LContextData : TStringList;
  LOldValue : string;
begin
  try
    LSQLAgent := TMultiRestrictionSQLAgent.Create(FAppModules);
    LContextData := TStringList.Create;
    try
      LSQLAgent.LoadContextData(LContextData, IntToStr(FIdentifier),IntToStr(AIndex+1));
      if (FAppModules.FieldProperties.UpdateFieldValue(
           'MultiCurFactor', FloatToStr(Value), FloatToStr(FRestrictionFactor[AIndex]), LContextData)) then
      begin
        LOldValue := FloatToStr(FRestrictionFactor[AIndex]);
        FRestrictionFactor[AIndex] := Value;
        FAppModules.Model.StudyDataHasChanged(sdccEdit,'MultiCurFactor',LOldValue,FloatToStr(Value));
      end;
    finally
      FreeAndNil(LSQLAgent);
      FreeAndNil(LContextData);
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TMultiResChannelCurtail.Validate(var AError: WideString;const AContext: WideString): WordBool;
const   OPNAME = 'TMultiResChannelCurtail.Validate';
var
  //  LErrorMessages : TStringList;
  LErrorCols        : TStringList;
  LErrorMsgs        : TStringList;
begin
  Result := False;
  try
    LErrorCols := TStringList.Create;
    LErrorMsgs := TStringList.Create;
    try
      if(AContext = 'DecisionMonth') then
        Result := ValidateDecisionMonth(LErrorMsgs)
      else if(AContext = 'StartMonth') then
        Result := ValidateStartMonth(LErrorMsgs)
      else if(AContext = 'ChannelNo') then
        Result := ValidateChannelNo(LErrorMsgs)
      else if(AContext = 'ReservoirNo') then
        Result := ValidateReservoirNo(LErrorMsgs)
      else if (AContext = 'Factor' ) then
        Result := ValidateRestrictionFactor(LErrorMsgs,LErrorCols)
      else if (AContext = 'Elevation' ) then
        Result := ValidateElevation(LErrorMsgs,LErrorCols);

      if (not Result) then
      begin
      if (LErrorCols.Count = 0) then
        AError := AError + LErrorMsgs.Text
      else
        AError := AError + CTStringsSeparator + LErrorMsgs.Text +
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

function TMultiResChannelCurtail.ValidateGrid(var AError: WideString; const AContext: WideString;  ACol, ARow: Integer): wordbool;
const OPNAME = 'TMultiResChannelCurtail.Validate';
begin
(*  Result := FALSE;
  try
  lAErrorMessages := TStringList.Create;
    if (AContext = 'Elevation') then
    begin
      Result :=  ValidateElevation(lAErrorMessages, ACol);
      AError := lAErrorMessages.Text;
    end;

    if(AContext = 'Factor') then
    begin
      Result := ValidateRestrictionFactor(lAErrorMessages, ACol);
      AError := lAErrorMessages.Text;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
  FreeAndNil(lAErrorMessages);
  *)
end;


function TMultiResChannelCurtail.ValidateDecisionMonth(AErrorMessages: TStrings): wordbool;
const OPNAME = 'TMultiResChannelCurtail.ValidateDecisionMonth';
var
  LMessage: string;
begin
  Result := False;
  try
    if(not(FAppModules.FieldProperties.ValidateFieldProperty('MultiCurDecisionMonth'
      ,IntToStr(FDecisionMonth),lMessage))) then
        AErrorMessages.Add(LMessage)
    else
    Result := TRUE

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMultiResChannelCurtail.ValidateStartMonth(AErrorMessages: TStrings): wordbool;
const OPNAME = 'TMultiResChannelCurtail.ValidateStartMonth';
var
  lMessages : string;
begin
  Result := False;
  try
    if(not (FAppModules.FieldProperties.ValidateFieldProperty('MultiCurStartMonth',IntToStr(FStartMonth),
                        lMessages))) then
      AErrorMessages.Add(lMessages)
    else
      Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMultiResChannelCurtail.ValidateElevation(AErrorMessages: TStrings; AErrorColumns: TStrings): wordbool;
const OPNAME = 'TMultiResChannelCurtail.ValidateElevation';
var
  LMessage : string;
  LResult : boolean;
  LIndex : integer;
  LFieldProperties: TAbstractFieldProperty;
begin
  Result := FALSE;
  try
    lResult := True;
    lMessage := '';
    LFieldProperties := FAppModules.FieldProperties.FieldProperty('MultiCurElevation');
    for LIndex := LFieldProperties.ArrayLow to LFieldProperties.ArrayHigh do
    begin
      if(not (FAppModules.FieldProperties.ValidateFieldProperty('MultiCurElevation',FloatToStr(FElevation[LIndex]),
                  lMessage,LIndex))) then
      begin
        lResult := False;
        AErrorMessages.Add('ERROR:'+LMessage);
        AErrorColumns.Add(IntToStr(LIndex));
        if (FAppModules.GlobalData.StopOnFirstErr) then
          Break;
      end;
    end;
    Result := LResult;
  except on E: Exception do  HandleError(E,OPNAME) end;
end;

function TMultiResChannelCurtail.ValidateRestrictionFactor(AErrorMessages: TStrings; AErrorColumns: TStrings): wordbool;
  const OPNAME = 'TMultiResChannelCurtail.ValidateRestrictionFactor';
var
  LMessage : string;
  LResult : boolean;
  LIndex : integer;
  LFieldProperties: TAbstractFieldProperty;
begin
    Result := FALSE;
    try
      lResult := True;
      lMessage := '';
      LFieldProperties := FAppModules.FieldProperties.FieldProperty('MultiCurFactor');
      for LIndex := LFieldProperties.ArrayLow to LFieldProperties.ArrayHigh do
      begin
        if(not (FAppModules.FieldProperties.ValidateFieldProperty('MultiCurFactor',FloatToStr(FRestrictionFactor[LIndex]),
                    lMessage,LIndex))) then
        begin
          lResult := False;
          AErrorMessages.Add('ERROR:'+LMessage);
          AErrorColumns.Add(IntToStr(LIndex));
          if (FAppModules.GlobalData.StopOnFirstErr) then
            Break;
        end;
      end;
      Result := LResult;
    except on E: Exception do  HandleError(E,OPNAME) end;
end;

function TMultiResChannelCurtail.ValidateChannelNo(AErrorMessages: TStrings): wordbool;
const OPNAME = 'TMultiResChannelCurtail.ValidateChannelNo';
var
  lMessage : string;
begin
  Result := False;
  try
      if (not (FAppModules.FieldProperties.ValidateFieldProperty('MultiCurChannelNo',IntToStr(FChannelNo),lMessage))) then
        AErrorMessages.Add(lMessage)
      else
        Result := true;
  except on E: Exception do HandleError(E, OPNAME)  end;
end;

function TMultiResChannelCurtail.ValidateReservoirNo(AErrorMessages: TStrings): wordbool;
const OPNAME = 'TMultiResChannelCurtail.ValidateReservoirNo';
var
  lMessage : string;
begin
  Result := False;
  try
    if( not (FAppModules.FieldProperties.ValidateFieldProperty('MultiCurReservoirNo',
                                            IntToStr(FReservoirNo),lMessage))) then
        AErrorMessages.Add(lMessage)
    else
        Result := TRUE;
  except on E: Exception do HandleError(E,OPNAME);

  end;
end;

function TMultiResChannelCurtail.Get_Identifier: integer;
const OPNAME = 'TMultiResChannelCurtail.Get_Identifier';
begin
  Result := 0;
  try
    Result := FIdentifier;
  except on E: Exception do HandleError(E,OPNAME)  end;
end;

procedure TMultiResChannelCurtail.Set_Identifier(Value: Integer);
const OPNAME = 'TMultiResChannelCurtail.SetIdentifier';
begin

end;

end.

