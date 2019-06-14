//
//
//  UNIT      : Contains  TWQConstriantData   Class
//  AUTHOR    : Sam Dhlamini(Cornastone)
//  DATE      : 17/02/2014
//  COPYRIGHT : Copyright © 2004 DWAF
//
//


unit UWQConstraintData;

interface
uses
  SysUtils,
  Classes,
  Contnrs,
  VoaimsCom_TLB,
  UAbstractObject,
  UYieldModelDataObject,
  UWQConstraintSQLAgent;
type

  TMinMaxUpperBoundChannel = class(TAbstractAppObject, IMinMaxUpperBoundChannel)
  protected
    FIdentifier : Integer;
    FChannelNumber : Integer;
    FBoundChannels : TStringList;

    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;

    function Get_ChannelNumber: Integer; safecall;
    procedure Set_ChannelNumber(Value: Integer); safecall;
    function Get_NoOfBoundedChannels: Integer; safecall;
    function Get_BoundedChannels: WideString; safecall;
    procedure Set_BoundedChannels(const Value: WideString); safecall;
    function ValidateChannelNumber(AErrorMessages : TStrings) : WordBool;
    function ValidateBoundedChannels(AErrorMessages : TStrings) : WordBool;
    procedure UpdateNoOfBoundChannels(AValue : Integer);

  public
    function Initialise: boolean;override;
    function Populate(AIdentifier ,AChannelNumber : integer; ABoundedChannels : WideString) : boolean;
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; safecall;
    property ChannelNumber: Integer read Get_ChannelNumber write Set_ChannelNumber;
    property NoOfBoundedChannels: Integer read Get_NoOfBoundedChannels;
    property BoundedChannels: WideString read Get_BoundedChannels write Set_BoundedChannels;
  end;

  TWQConstriantsChannel = class(TAbstractAppObject, IWQConstriantsChannel)
  protected
    FIdentifier : Integer;
    FChannelNumber : Integer;
    FTarget : double;
    FNoOfRefChannelsBlending :integer;
    FReservoirRef : integer;
    FWQConType : integer;
    FReferenceChannel : TStringList;
    FRefChannelFactor : TStringList;
    FSlopeLimit : integer;
    FEstimatedRelease : TStringList;
    FConcentration : TStringList;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    function Get_ChannelNumber: Integer; safecall;
    procedure Set_ChannelNumber(Value: Integer); safecall;
    function Get_Target: Double; safecall;
    procedure Set_Target(Value: Double); safecall;
    function Get_NoOfRefChannelsBlending: Integer; safecall;
    procedure Set_NoOfRefChannelsBlending(Value: Integer); safecall;
    function Get_ReservoirRef: Integer; safecall;
    procedure Set_ReservoirRef(Value: Integer); safecall;
    function Get_WQConType: Integer; safecall;
    procedure Set_WQConType(Value: Integer); safecall;
    function Get_BlendingChannels: WideString; safecall;
    procedure Set_BlendingChannels(const Value: WideString); safecall;
    function Get_BlendingChannelFactors: WideString; safecall;
    procedure Set_BlendingChannelFactors(const Value: WideString); safecall;
    function Get_LimitingSlope: Integer; safecall;
    procedure Set_LimitingSlope(Value: Integer); safecall;
    function Get_EstimatedRelease: WideString; safecall;
    procedure Set_EstimatedRelease(const Value: WideString); safecall;
    function Get_Concentration: WideString; safecall;
    procedure Set_Concentration(const Value: WideString); safecall;
    function ValidateChannelNumber(AErrorMessages : TStrings) : WordBool;
  public
    function Initialise: boolean;override;
    function Populate(AIdentifier ,AChannelNumber : integer; ATarget : double; AReservoirRef : integer;
                      AWQConType : integer; AReferenceChannel : WideString; ARefChannelFactor : WideString;
                      ASlopeLimit : integer;AEstimatedRelease, AConcentration : WideString  ) : boolean;
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; safecall;
    property ChannelNumber: Integer read Get_ChannelNumber write Set_ChannelNumber;
    property Target: Double read Get_Target write Set_Target;
    property NoOfRefChannelsBlending: Integer read Get_NoOfRefChannelsBlending write Set_NoOfRefChannelsBlending;
    property ReservoirRef: Integer read Get_ReservoirRef write Set_ReservoirRef;
    property WQConType: Integer read Get_WQConType write Set_WQConType;
    property BlendingChannels: WideString read Get_BlendingChannels write Set_BlendingChannels;
    property BlendingChannelFactors: WideString read Get_BlendingChannelFactors write Set_BlendingChannelFactors;
    property LimitingSlope: Integer read Get_LimitingSlope write Set_LimitingSlope;
    property EstimatedRelease: WideString read Get_EstimatedRelease write Set_EstimatedRelease;
    property Concentration: WideString read Get_Concentration write Set_Concentration;


  end;

   TWQConstriantData = class(TAbstractAppObject, IWQConstraintData)
   protected
    FWQConstriantsChannelList    : TObjectList;
    FMinMaxUpperBoundChannelList : TObjectList;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    function Get_WQConstraintsChannelCount: Integer; safecall;
    function Get_WQConstraintsChannelByIndex(AIndex: Integer): IWQConstriantsChannel; safecall;
    function Get_WQConstraintsChannelByChannelNo(AChannelNo: Integer): IWQConstriantsChannel; safecall;

    function Get_MinMaxUpperBoundChannelCount: Integer; safecall;
    function Get_MinMaxUpperBoundChannelByIndex(AIndex: Integer): IMinMaxUpperBoundChannel; safecall;
    function Get_MinMaxUpperBoundChannelNo(AChannelNo: Integer): IMinMaxUpperBoundChannel; safecall;

  public
    function Initialise: boolean;override;
    function NewWQConstraintsChannels(AChannel: Integer): IWQConstriantsChannel; safecall;
    function CreateWQConstraintsChannels: TWQConstriantsChannel;
    function RemoveWQConstriantsChannel(AChannelNo: Integer): WordBool; safecall;
    function DeleteWQConstriantsChannel(AChannelNo: Integer): WordBool;
    function NewMinMaxUpperBoundChannel(AChannelNo: Integer): IMinMaxUpperBoundChannel; safecall;
    function CreateMinMaxUpperBoundChannel: TMinMaxUpperBoundChannel;
    function RemoveMinMaxUpperBoundChannel(AChannelNo: Integer): WordBool; safecall;
    function DeleteMinMaxUpperBoundChannel(AChannelNo: Integer): WordBool;

    property WQConstraintsChannelCount: Integer read Get_WQConstraintsChannelCount;
    property WQConstraintsChannelByIndex[AIndex: Integer]: IWQConstriantsChannel read Get_WQConstraintsChannelByIndex;
    property WQConstraintsChannelByChannelNo[AChannelNo: Integer]: IWQConstriantsChannel read Get_WQConstraintsChannelByChannelNo;
    property MinMaxUpperBoundChannelCount: Integer read Get_MinMaxUpperBoundChannelCount;
    property MinMaxUpperBoundChannelByIndex[AIndex: Integer]: IMinMaxUpperBoundChannel read Get_MinMaxUpperBoundChannelByIndex;
    property MinMaxUpperBoundChannelNo[AChannelNo: Integer]: IMinMaxUpperBoundChannel read Get_MinMaxUpperBoundChannelNo;


   end;

implementation

uses
  System.Types,
  UConstants,
  UDataSetType,
  UErrorHandlingOperations;

{ TWQConstriantData }

procedure TWQConstriantData.CreateMemberObjects;
const OPNAME = 'TWQConstriantData.CreateMemberObjects';
begin
  inherited CreateMemberObjects;
  try
    FWQConstriantsChannelList    := TObjectList.Create;
    FMinMaxUpperBoundChannelList := TObjectList.Create;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TWQConstriantData.CreateMinMaxUpperBoundChannel: TMinMaxUpperBoundChannel;
const OPNAME = 'TWQConstriantData.CreateMinMaxUpperBoundChannel';
begin
  Result := nil;
  try
    Result := TMinMaxUpperBoundChannel.Create(FAppModules);
    FMinMaxUpperBoundChannelList.Add(Result);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TWQConstriantData.CreateWQConstraintsChannels: TWQConstriantsChannel;
const OPNAME = 'TWQConstriantData.CreateWQConstraintsChannels';
begin
  Result := nil;
  try
    Result := TWQConstriantsChannel.Create(FAppModules);
    FWQConstriantsChannelList.Add(Result);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TWQConstriantData.DeleteMinMaxUpperBoundChannel(AChannelNo: Integer): WordBool;
const OPNAME = 'TWQConstriantData.DeleteMinMaxUpperBoundChannel';
var
  LIndex: integer;
begin
  Result := False;
  try
    for LIndex := 0 to FMinMaxUpperBoundChannelList.Count -1 do
    begin
      if TMinMaxUpperBoundChannel(FMinMaxUpperBoundChannelList.Items[LIndex]).FChannelNumber = AChannelNo then
      begin
        FMinMaxUpperBoundChannelList.Remove(TMinMaxUpperBoundChannel(FMinMaxUpperBoundChannelList.Items[LIndex]));
        Result := True;
        Break;
      end;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TWQConstriantData.DeleteWQConstriantsChannel(AChannelNo: Integer): WordBool;
const OPNAME = 'TWQConstriantData.DeleteMinMaxUpperBoundChannel';
var
  LIndex: integer;
begin
  Result := False;
  try
    for LIndex := 0 to FWQConstriantsChannelList.Count -1 do
    begin
      if TWQConstriantsChannel(FWQConstriantsChannelList.Items[LIndex]).FChannelNumber = AChannelNo then
      begin
        FWQConstriantsChannelList.Remove(TWQConstriantsChannel(FWQConstriantsChannelList.Items[LIndex]));
        Result := True;
        Break;
      end;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TWQConstriantData.DestroyMemberObjects;
const OPNAME = 'TWQConstriantData.DestroyMemberObjects';
begin
  try
    inherited DestroyMemberObjects;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TWQConstriantData.Get_MinMaxUpperBoundChannelByIndex(AIndex: Integer): IMinMaxUpperBoundChannel;
const OPNAME = 'TWQConstriantData.Get_MinMaxUpperBoundChannelByIndex';
begin
  Result := nil;
  try
    if (AIndex >=0) and (AIndex < FMinMaxUpperBoundChannelList.Count) then
      Result := TMinMaxUpperBoundChannel(FMinMaxUpperBoundChannelList.Items[AIndex]);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TWQConstriantData.Get_MinMaxUpperBoundChannelCount: Integer;
const OPNAME = 'TWQConstriantData.Get_MinMaxUpperBoundChannelCount';
begin
  Result := 0;
  try
    Result := FWQConstriantsChannelList.Count;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TWQConstriantData.Get_MinMaxUpperBoundChannelNo(AChannelNo: Integer): IMinMaxUpperBoundChannel;
const OPNAME = 'TWQConstriantData.Get_MinMaxUpperBoundChannelNo';
var
 LIndex : integer;
begin
  Result := nil;
  try
    for LIndex := 0 to  FMinMaxUpperBoundChannelList.Count -1 do
      if (TMinMaxUpperBoundChannel(FMinMaxUpperBoundChannelList[LIndex]).FChannelNumber = AChannelNo) then
      begin
        Result := TMinMaxUpperBoundChannel(FMinMaxUpperBoundChannelList[LIndex]);
        Break;
      end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TWQConstriantData.Get_WQConstraintsChannelByChannelNo( AChannelNo: Integer): IWQConstriantsChannel;
const OPNAME = 'TWQConstriantData.Get_MinMaxUpperBoundChannelNo';
var
 LIndex : integer;
begin
  Result := nil;
  try
    for LIndex := 0 to  FWQConstriantsChannelList.Count -1 do
      if (TMinMaxUpperBoundChannel(FWQConstriantsChannelList[LIndex]).FChannelNumber = AChannelNo) then
      begin
        Result := TWQConstriantsChannel(FWQConstriantsChannelList[LIndex]);
        Break;
      end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;


function TWQConstriantData.Get_WQConstraintsChannelByIndex(AIndex: Integer): IWQConstriantsChannel;
const OPNAME = 'TWQConstriantData.Get_MinMaxUpperBoundChannelByIndex';
begin
  Result := nil;
  try
    if (AIndex >=0) and (AIndex < FWQConstriantsChannelList.Count) then
      Result := TWQConstriantsChannel(FWQConstriantsChannelList.Items[AIndex]);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TWQConstriantData.Get_WQConstraintsChannelCount: Integer;
const OPNAME = 'TWQConstriantData.Get_WQConstraintsChannelCount';
begin
  Result := 0;
  try
    Result := FMinMaxUpperBoundChannelList.Count;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;


function TWQConstriantData.Initialise: boolean;
const OPNAME = 'TWQConstriantData.Get_NoOfBoundedChannels';
begin
  Result := inherited Initialise;
  try
    FWQConstriantsChannelList.Clear;
    FMinMaxUpperBoundChannelList.Clear;
    Result :=  True;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TWQConstriantData.NewMinMaxUpperBoundChannel(AChannelNo: Integer): IMinMaxUpperBoundChannel;
const OPNAME = 'TReturnFlowChannelData.NewReturnFlowChannel';
var
  LLoadAgent : TWQConstraintSQLAgent;
  LIdentifier : integer;
  LMinMaxUpperBoundChannel : TMinMaxUpperBoundChannel;
begin
  Result := nil;
  try
    LLoadAgent := TWQConstraintSQLAgent.Create(FAppModules);
    if LLoadAgent.InsertMinMaxBoundChannel(LIdentifier,AChannelNo) then
    begin
      LMinMaxUpperBoundChannel := CreateMinMaxUpperBoundChannel;
      LMinMaxUpperBoundChannel.FChannelNumber := AChannelNo;
      LMinMaxUpperBoundChannel.FIdentifier := LIdentifier;
      Result := LMinMaxUpperBoundChannel;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TWQConstriantData.NewWQConstraintsChannels(AChannel: Integer): IWQConstriantsChannel;
const OPNAME = 'TReturnFlowChannelData.NewReturnFlowChannel';
var
  LLoadAgent : TWQConstraintSQLAgent;
  LIdentifier : integer;
  LWQConstriantsChannel : TWQConstriantsChannel;
begin
  Result := nil;
  try
    LLoadAgent := TWQConstraintSQLAgent.Create(FAppModules);
    if LLoadAgent.InsertWQConChannel(LIdentifier,AChannel) then
    begin
      LWQConstriantsChannel := CreateWQConstraintsChannels;
      LWQConstriantsChannel.FChannelNumber := AChannel;
      LWQConstriantsChannel.FIdentifier := LIdentifier;
      Result := LWQConstriantsChannel;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TWQConstriantData.RemoveMinMaxUpperBoundChannel(AChannelNo: Integer): WordBool;
const OPNAME = 'TReturnFlowChannelData.RemoveReturnFlowByChannel';
var
  LLoadAgent : TWQConstraintSQLAgent;
begin
  Result := False;
  try
    LLoadAgent := TWQConstraintSQLAgent.Create(FAppModules);
    if LLoadAgent.DeleteMinMaxBoundChannel(AChannelNo) then
      Result := DeleteMinMaxUpperBoundChannel(AChannelNo);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TWQConstriantData.RemoveWQConstriantsChannel(AChannelNo: Integer): WordBool;
const OPNAME = 'TReturnFlowChannelData.RemoveReturnFlowByChannel';
var
  LLoadAgent : TWQConstraintSQLAgent;
begin
  Result := False;
  try
    LLoadAgent := TWQConstraintSQLAgent.Create(FAppModules);
    if LLoadAgent.DeleteMinMaxWQConstrainChannelByChannel(AChannelNo) then
      Result := DeleteWQConstriantsChannel(AChannelNo);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TWQConstriantData._AddRef: Integer;
const OPNAME = 'TWQConstriantData._AddRef';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TWQConstriantData._Release: Integer;
const OPNAME = 'TWQConstriantData._Release';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

{ TWQConstriantsChannel }

procedure TWQConstriantsChannel.CreateMemberObjects;
const OPNAME = 'TMinMaxUpperBoundChannel.CreateMemberObjects';
begin
  inherited CreateMemberObjects;
  try
    FReferenceChannel := TStringList.Create;
    FRefChannelFactor := TStringList.Create;
    FEstimatedRelease := TStringList.Create;
    FConcentration := TStringList.Create;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TWQConstriantsChannel.DestroyMemberObjects;
const OPNAME = 'TWQConstriantsChannel.DestroyMemberObjects';
begin
  try
    inherited DestroyMemberObjects;
    FreeAndNil(FRefChannelFactor);
    FreeAndNil(FReferenceChannel);
    FreeAndNil(FConcentration);
    FreeAndNil(FEstimatedRelease);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TWQConstriantsChannel.Get_BlendingChannelFactors: WideString;
const OPNAME = 'TWQConstriantsChannel.Get_BlendingChannelFactors';
begin
  Result := '';
  try
    Result := FRefChannelFactor.CommaText;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TWQConstriantsChannel.Get_BlendingChannels: WideString;
const OPNAME = 'TWQConstriantsChannel.Get_BlendingChannels';
begin
  Result := '';
  try
    Result := FReferenceChannel.CommaText;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TWQConstriantsChannel.Get_ChannelNumber: Integer;
const OPNAME = 'TWQConstriantsChannel.Get_ChannelNumber';
begin
  Result := 0;
  try
    Result := FChannelNumber;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TWQConstriantsChannel.Get_Concentration: WideString;
const OPNAME = 'TWQConstriantsChannel.Get_Concentration';
begin
  Result := '';
  try
    Result := FConcentration.CommaText;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TWQConstriantsChannel.Get_EstimatedRelease: WideString;
const OPNAME = 'TWQConstriantsChannel.Get_EstimatedRelease';
begin
  Result := '';
  try
    Result := FEstimatedRelease.CommaText;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;


function TWQConstriantsChannel.Get_LimitingSlope: Integer;
const OPNAME = 'TWQConstriantsChannel.Get_LimitingSlope';
begin
  Result := 0;
  try
    Result := FSlopeLimit;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TWQConstriantsChannel.Get_NoOfRefChannelsBlending: Integer;
const OPNAME = 'TWQConstriantsChannel.Get_ChannelNumber';
begin
  Result := 0;
  try
    Result := FReferenceChannel.Count;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TWQConstriantsChannel.Get_ReservoirRef: Integer;
const OPNAME = 'TWQConstriantsChannel.Get_ReservoirRef';
begin
  Result := 0;
  try
    Result := FReservoirRef;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TWQConstriantsChannel.Get_Target: Double;
const OPNAME = 'TWQConstriantsChannel.Get_ChannelNumber';
begin
  Result := 0;
  try
    Result := FTarget;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TWQConstriantsChannel.Get_WQConType: Integer;
const OPNAME = 'TWQConstriantsChannel.Get_WQConType';
begin
  Result := 0;
  try
    Result := FWQConType;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TWQConstriantsChannel.Initialise: boolean;
const OPNAME = 'TMinMaxUpperBoundChannel.Get_NoOfBoundedChannels';
begin
  Result := inherited Initialise;
  try
    FIdentifier := 0;
    FChannelNumber := 0;
    FReferenceChannel.Clear;
    FRefChannelFactor.Clear;
    FEstimatedRelease.Clear;
    FConcentration.Clear;
    Result :=  True;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TWQConstriantsChannel.Populate(AIdentifier ,AChannelNumber : integer; ATarget : double; AReservoirRef : integer;
                      AWQConType : integer; AReferenceChannel : WideString;ARefChannelFactor : WideString;
                      ASlopeLimit : integer;AEstimatedRelease, AConcentration : WideString  ) : boolean;
const OPNAME = 'TWQConstriantsChannel.Populate';
begin
  Result := False;
  try
    FIdentifier := AIdentifier;
    FChannelNumber := AChannelNumber;
    FTarget := ATarget;
    FReservoirRef := AReservoirRef;
    FWQConType := AWQConType;
    FReferenceChannel.CommaText := AReferenceChannel;
    FRefChannelFactor.CommaText := ARefChannelFactor;
    FNoOfRefChannelsBlending := FReferenceChannel.Count;
    FSlopeLimit := ASlopeLimit;
    FEstimatedRelease.CommaText := AEstimatedRelease;
    FConcentration.CommaText := AConcentration;
    Result :=  True;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TWQConstriantsChannel.Set_BlendingChannelFactors(const Value: WideString);
const OPNAME = 'TWQConstriantsChannel.Set_BlendingChannelFactors';
var
  LSQLAgent : TWQConstraintSQLAgent;
  LOldValue: string;
  LContextData : TStringList;
begin
  try
    if Value = FRefChannelFactor.CommaText then Exit;
    LSQLAgent := TWQConstraintSQLAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        LSQLAgent.LoadContextData(LContextData, IntToStr(FIdentifier));
        LOldValue := FRefChannelFactor.CommaText;
        if
           FAppModules.FieldProperties.UpdateFieldValue('RefChannelFactor', Value,LOldValue, LContextData) then
        Begin
          FRefChannelFactor.CommaText := Value;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'RefChannelFactor',LOldValue,Value);
        end;
      finally
        FreeAndNil(LContextData);
      end;
    finally
      FreeAndNil(LSQLAgent);
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TWQConstriantsChannel.Set_BlendingChannels(const Value: WideString);
const OPNAME = 'TWQConstriantsChannel.Set_BlendingChannels';
var
  LSQLAgent : TWQConstraintSQLAgent;
  LOldValue: string;
  LContextData : TStringList;
begin
  try
    if Value = FReferenceChannel.CommaText then Exit;
    LSQLAgent := TWQConstraintSQLAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        LSQLAgent.LoadContextData(LContextData, IntToStr(FIdentifier));
        LOldValue := FReferenceChannel.CommaText;
        if
           FAppModules.FieldProperties.UpdateFieldValue('ReferenceChannel', Value,LOldValue, LContextData) then
        Begin
          FReferenceChannel.CommaText := Value;
          Set_NoOfRefChannelsBlending(FReferenceChannel.Count);
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'ReferenceChannel',LOldValue,Value);
        end;
      finally
        FreeAndNil(LContextData);
      end;
    finally
      FreeAndNil(LSQLAgent);
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TWQConstriantsChannel.Set_ChannelNumber(Value: Integer);
const OPNAME = 'TWQConstriantsChannel.Set_ChannelNumber';
var
  LLoadAgent : TWQConstraintSQLAgent;
  LContextData : TStringList;
  LOldValue : string;
begin
  try
    LLoadAgent := TWQConstraintSQLAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        if FChannelNumber <> Value then
        begin
          LLoadAgent.LoadContextData(LContextData, IntToStr(FIdentifier));
          if (FAppModules.FieldProperties.UpdateFieldValue(
            'WQConstriantsChannel', IntToStr(Value), IntToStr(FChannelNumber), LContextData)) then
          begin
            LOldValue := IntToStr(FChannelNumber);
            FChannelNumber := Value;
            FAppModules.Model.StudyDataHasChanged(sdccEdit,'WQConstriantsChannel',LOldValue,IntToStr(Value));
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

procedure TWQConstriantsChannel.Set_Concentration(const Value: WideString);
const OPNAME = 'TWQConstriantsChannel.Set_Concentration';
var
  LSQLAgent : TWQConstraintSQLAgent;
  LOldValue: string;
  LContextData : TStringList;
begin
  try
    if Value = FConcentration.CommaText then Exit;
    LSQLAgent := TWQConstraintSQLAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        LSQLAgent.LoadContextData(LContextData, IntToStr(FIdentifier));
        LOldValue := FConcentration.CommaText;
        if FAppModules.FieldProperties.UpdateFieldValue('Concentration', Value,LOldValue, LContextData) then
        Begin
          FConcentration.CommaText := Value;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'Concentration',LOldValue,Value);
        end;
      finally
        FreeAndNil(LContextData);
      end;
    finally
      FreeAndNil(LSQLAgent);
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TWQConstriantsChannel.Set_EstimatedRelease(const Value: WideString);
const OPNAME = 'TWQConstriantsChannel.Set_EstimatedRelease';
var
  LSQLAgent : TWQConstraintSQLAgent;
  LOldValue: string;
  LContextData : TStringList;
begin
  try
    if Value = FEstimatedRelease.CommaText then Exit;
    LSQLAgent := TWQConstraintSQLAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        LSQLAgent.LoadContextData(LContextData, IntToStr(FIdentifier));
        LOldValue := FEstimatedRelease.CommaText;
        if FAppModules.FieldProperties.UpdateFieldValue('EstimatedRelease', Value,LOldValue, LContextData) then
        Begin
          FEstimatedRelease.CommaText := Value;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'EstimatedRelease',LOldValue,Value);
        end;
      finally
        FreeAndNil(LContextData);
      end;
    finally
      FreeAndNil(LSQLAgent);
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;


procedure TWQConstriantsChannel.Set_LimitingSlope(Value: Integer);
const OPNAME = 'TWQConstriantsChannel.Set_LimitingSlope';
var
  LLoadAgent : TWQConstraintSQLAgent;
  LContextData : TStringList;
  LOldValue : string;
begin
  try
    LLoadAgent := TWQConstraintSQLAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        if FSlopeLimit <> Value then
        begin
          LLoadAgent.LoadContextData(LContextData, IntToStr(FIdentifier));
          if (FAppModules.FieldProperties.UpdateFieldValue(
            'SlopeLimit', IntToStr(Value), IntToStr(FSlopeLimit), LContextData)) then
          begin
            LOldValue := IntToStr(FSlopeLimit);
            FSlopeLimit := Value;
            FAppModules.Model.StudyDataHasChanged(sdccEdit,'SlopeLimit',LOldValue,IntToStr(Value));
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

procedure TWQConstriantsChannel.Set_NoOfRefChannelsBlending(Value: Integer);
const OPNAME = 'TWQConstriantsChannel.Set_NoOfRefChannelsBlending';
var
  LLoadAgent : TWQConstraintSQLAgent;
  LContextData : TStringList;
  LOldValue : string;
begin
  try
    LLoadAgent := TWQConstraintSQLAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        if FNoOfRefChannelsBlending <> Value then
        begin
          LLoadAgent.LoadContextData(LContextData, IntToStr(FIdentifier));
          if (FAppModules.FieldProperties.UpdateFieldValue(
            'NoOfRefChannelsBlending', IntToStr(Value), IntToStr(FNoOfRefChannelsBlending), LContextData)) then
          begin
            LOldValue := IntToStr(FNoOfRefChannelsBlending);
            FNoOfRefChannelsBlending := Value;
            FAppModules.Model.StudyDataHasChanged(sdccEdit,'NoOfRefChannelsBlending',LOldValue,IntToStr(Value));
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

procedure TWQConstriantsChannel.Set_ReservoirRef(Value: Integer);
const OPNAME = 'TWQConstriantsChannel.Set_ChannelNumber';
var
  LLoadAgent : TWQConstraintSQLAgent;
  LContextData : TStringList;
  LOldValue : string;
begin
  try
    LLoadAgent := TWQConstraintSQLAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        if FReservoirRef <> Value then
        begin
           LLoadAgent.LoadContextData(LContextData, IntToStr(FIdentifier));
          if (FAppModules.FieldProperties.UpdateFieldValue(
            'ReservoirRef', IntToStr(Value), IntToStr(FReservoirRef), LContextData)) then
          begin
            LOldValue := IntToStr(FReservoirRef);
            FReservoirRef := Value;
            FAppModules.Model.StudyDataHasChanged(sdccEdit,'ReservoirRef',LOldValue,IntToStr(Value));
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

procedure TWQConstriantsChannel.Set_Target(Value: Double);
const OPNAME = 'TWQConstriantsChannel.Set_Target';
var
  LLoadAgent : TWQConstraintSQLAgent;
  LContextData : TStringList;
  LOldValue : string;
begin
  try
    LLoadAgent := TWQConstraintSQLAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        if FTarget <> Value then
        begin
           LLoadAgent.LoadContextData(LContextData, IntToStr(FIdentifier));
          if (FAppModules.FieldProperties.UpdateFieldValue(
            'WQConstTarget', FloatToStr(Value), FloatToStr(FTarget), LContextData)) then
          begin
            LOldValue := FloatToStr(FTarget);
            FTarget := Value;
            FAppModules.Model.StudyDataHasChanged(sdccEdit,'WQConstTarget',LOldValue,FloatToStr(Value));
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

procedure TWQConstriantsChannel.Set_WQConType(Value: Integer);
const OPNAME = 'TWQConstriantsChannel.Set_WQConType';
var
  LLoadAgent : TWQConstraintSQLAgent;
  LContextData : TStringList;
  LEstimatedData : TStringList;
  LConcentrationData : TStringList;
  LOldValue : string;
  LCount : integer;
begin
  try
    LLoadAgent := TWQConstraintSQLAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      LEstimatedData := TStringList.Create;
      LConcentrationData:= TStringList.Create;
      try
        if FWQConType <> Value then
        begin
          LLoadAgent.LoadContextData(LContextData, IntToStr(FIdentifier));
          if (FAppModules.FieldProperties.UpdateFieldValue(
            'WQConType', IntToStr(Value), IntToStr(FWQConType), LContextData)) then
          begin
            LOldValue := IntToStr(FWQConType);
            FWQConType := Value;
            if FWQConType = 2 then
            begin
              for LCount := 1 to 10 do
              begin
                LEstimatedData.Add('0.00');
                LConcentrationData.Add('0.00');
              end;
              Set_LimitingSlope(0);
              Set_EstimatedRelease(LEstimatedData.CommaText);
              Set_Concentration(LConcentrationData.CommaText);
            end
            else
            begin
              Set_EstimatedRelease('');
              Set_Concentration('');
            end;
            FAppModules.Model.StudyDataHasChanged(sdccEdit,'WQConType',LOldValue,IntToStr(Value));
          end;
        end;
      finally
        LContextData.Free;
        LEstimatedData.Free;
        LConcentrationData.Create;
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TWQConstriantsChannel.Validate(var AErrors: WideString; const AContext: WideString): WordBool;
const OPNAME = 'TWQConstriantsChannel.Validate';
var
  LErrorList : TStringList;
begin
  Result := FALSE;
  try
    LErrorList := TStringList.Create;
    try
      if (AContext = 'ForChannels') then
        Result := ValidateChannelNumber(lErrorList)
      else
      begin
        Result := True;
        //if (not ValidateChannelNumber(lErrorList)) then
        //  Result := False;
      end;
      AErrors := AErrors + LErrorList.Text;
    finally
      FreeAndNil(LErrorList);
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TWQConstriantsChannel.ValidateChannelNumber( AErrorMessages: TStrings): WordBool;
const OPNAME = 'TMinMaxUpperBoundChannel.ValidateChannelNumber';
var
  LMessage : string;
  LChannelList : IChannelList;
  LChannel : IGeneralFlowChannel;
  LChannelNo : integer;
  LMinMaxFlowConstraint : IMinMaxFlowConstraint;
  LIndex : integer;
begin
  Result := False;
  try
    LMessage := '';
    Result := FAppModules.FieldProperties.ValidateFieldProperty('WQConstriantsChannel',
            IntToStr(FChannelNumber), LMessage);
    if (not Result) then
      AErrorMessages.Add('ERROR:'+IntToStr(FChannelNumber) + ':'+LMessage)
    else
    begin
      LChannelList := TYieldModelDataObject(FAppModules.Model.ModelData).
                        NetworkElementData.ChannelList;
      LChannel     := LChannelList.ChannelByChannelNumber[FChannelNumber];
      for LIndex := 0 to FReferenceChannel.Count -1 do
      begin

        LChannelNo := StrToInt(Trim(FReferenceChannel[LIndex]));

        if LChannelNo>0 then
        begin

           LMinMaxFlowConstraint := TYieldModelDataObject(FAppModules.Model.ModelData).
                       NetworkFeaturesData.MinMaxFlowConstraintList.MinMaxFlowConstraintByID[LChannelNo];

          if LMinMaxFlowConstraint = nil then
          begin
            LMessage := Format('Channel %d is not a Min-Max Channel.',[LChannelNo]);
            Result := False;
          end
          else
            Result := True;

          if (LChannelNo = FChannelNumber) then
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

function TWQConstriantsChannel._AddRef: Integer;
const OPNAME = 'TWQConstriantsChannel._AddRef';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TWQConstriantsChannel._Release: Integer;
const OPNAME = 'TWQConstriantsChannel._Release';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

{ TMinMaxUpperBoundChannel }

procedure TMinMaxUpperBoundChannel.CreateMemberObjects;
const OPNAME = 'TMinMaxUpperBoundChannel.CreateMemberObjects';
begin
  inherited CreateMemberObjects;
  try
     FBoundChannels := TStringList.Create;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TMinMaxUpperBoundChannel.DestroyMemberObjects;
const OPNAME = 'TMinMaxUpperBoundChannel.DestroyMemberObjects';
begin
  try
    inherited DestroyMemberObjects;
    FreeAndNil(FBoundChannels);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TMinMaxUpperBoundChannel.Get_BoundedChannels: WideString;
const OPNAME = 'TMinMaxUpperBoundChannel.Get_BoundedChannels';
begin
  Result := '';
  try
    Result := FBoundChannels.CommaText;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TMinMaxUpperBoundChannel.Get_ChannelNumber: Integer;
const OPNAME = 'TMinMaxUpperBoundChannel.Get_ChannelNumber';
begin
  Result := 0;
  try
    Result := FChannelNumber;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TMinMaxUpperBoundChannel.Get_NoOfBoundedChannels: Integer;
const OPNAME = 'TMinMaxUpperBoundChannel.Get_NoOfBoundedChannels';
begin
  Result := 0;
  try
    Result := FBoundChannels.Count;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TMinMaxUpperBoundChannel.Initialise: boolean;
const OPNAME = 'TMinMaxUpperBoundChannel.Initialise';
begin
  Result := inherited Initialise;
  try
    FIdentifier := 0;
    FChannelNumber := 0;
    FBoundChannels.Clear;
    Result :=  True;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TMinMaxUpperBoundChannel.Populate(AIdentifier, AChannelNumber : integer; ABoundedChannels: WideString): boolean;
const OPNAME = 'TMinMaxUpperBoundChannel.Populate';
begin
  Result := False;
  try
    FIdentifier := AIdentifier;
    FChannelNumber := AChannelNumber;
    FBoundChannels.CommaText := ABoundedChannels;
    Result := True;;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TMinMaxUpperBoundChannel.Set_BoundedChannels(const Value: WideString);
const OPNAME = 'TMinMaxUpperBoundChannel.Set_BoundedChannels';
var
  LSQLAgent : TWQConstraintSQLAgent;
  LOldValue: string;
  LContextData : TStringList;
begin
  try
    if Value = FBoundChannels.CommaText then Exit;
    LSQLAgent := TWQConstraintSQLAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        LSQLAgent.LoadContextData(LContextData, IntToStr(FIdentifier));
        LOldValue := FBoundChannels.CommaText;
        if FAppModules.FieldProperties.UpdateFieldValue('ReferenceChannels', Value,LOldValue, LContextData) then
        Begin
          FBoundChannels.CommaText := Value;
          UpdateNoOfBoundChannels(FBoundChannels.Count);
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'ReferenceChannels',LOldValue,Value);
        end;
      finally
        FreeAndNil(LContextData);
      end;
    finally
      FreeAndNil(LSQLAgent);
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TMinMaxUpperBoundChannel.UpdateNoOfBoundChannels(AValue : Integer);
const OPNAME = 'TMinMaxUpperBoundChannel.UpdateNoOfBoundChannels';
var
  LLoadAgent : TWQConstraintSQLAgent;
  LContextData : TStringList;
  LOldValue : string;
begin
  try
    LLoadAgent := TWQConstraintSQLAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        LLoadAgent.LoadContextData(LContextData, IntToStr(FIdentifier));
        if (FAppModules.FieldProperties.UpdateFieldValue(
          'BoundedChannels', IntToStr(AValue), '0', LContextData)) then
        begin
          LOldValue := '0';
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'BoundedChannels',LOldValue,IntToStr(AValue));
        end;
      finally
        LContextData.Free;
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;


procedure TMinMaxUpperBoundChannel.Set_ChannelNumber(Value: Integer);
const OPNAME = 'TMinMaxUpperBoundChannel.Set_ChannelNumber';
var
  LLoadAgent : TWQConstraintSQLAgent;
  LContextData : TStringList;
  LOldValue : string;
begin
  try
    LLoadAgent := TWQConstraintSQLAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        if FChannelNumber <> Value then
        begin
          LLoadAgent.LoadContextData(LContextData, IntToStr(FIdentifier));
          if (FAppModules.FieldProperties.UpdateFieldValue(
            'MinMaxUpperBoundChannel', IntToStr(Value), IntToStr(FChannelNumber), LContextData)) then
          begin
            LOldValue := IntToStr(FChannelNumber);
            FChannelNumber := Value;
            FAppModules.Model.StudyDataHasChanged(sdccEdit,'MinMaxUpperBoundChannel',LOldValue,IntToStr(Value));
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

function TMinMaxUpperBoundChannel.Validate(var AErrors: WideString; const AContext: WideString): WordBool;
const OPNAME = 'TMinMaxUpperBoundChannel.Validate';
var
  LErrorMsgs : TStringList;
begin
  Result := False;
  try
    LErrorMsgs := TStringList.Create;
    try
      if AContext = 'MinMaxUpperBoundChannel' then
        ValidateChannelNumber(LErrorMsgs)
      else
      if AContext = 'BoundChannels' then
        ValidateBoundedChannels(LErrorMsgs);
    finally
      FreeAndNil(LErrorMsgs);
    end;

  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TMinMaxUpperBoundChannel.ValidateChannelNumber(AErrorMessages: TStrings): WordBool;
const OPNAME = 'TMinMaxUpperBoundChannel.ValidateChannelNumber';
var
  LMessage : string;
  LChannelList : IChannelList;
  LChannel : IGeneralFlowChannel;
  LChannelNo : integer;
  LMinMaxFlowConstraint : IMinMaxFlowConstraint;
  LIndex : integer;
begin
  Result := False;
  try
    LMessage := '';
    Result := FAppModules.FieldProperties.ValidateFieldProperty('MinMaxUpperBoundChannel',
            IntToStr(FChannelNumber), LMessage);
    if (not Result) then
      AErrorMessages.Add('ERROR:'+IntToStr(FChannelNumber) + ':'+LMessage)
    else
    begin
      LChannelList := TYieldModelDataObject(FAppModules.Model.ModelData).
                        NetworkElementData.ChannelList;
      LChannel     := LChannelList.ChannelByChannelNumber[FChannelNumber];
      for LIndex := 0 to FBoundChannels.Count -1 do
      begin

        LChannelNo := StrToInt(Trim(Copy(FBoundChannels[LIndex],1,Length(FBoundChannels[LIndex])-1)));

        if LChannelNo>0 then
        begin

           LMinMaxFlowConstraint := TYieldModelDataObject(FAppModules.Model.ModelData).
                       NetworkFeaturesData.MinMaxFlowConstraintList.MinMaxFlowConstraintByID[LChannelNo];

          if LMinMaxFlowConstraint = nil then
          begin
            LMessage := Format('Channel %d is not a Min-Max Channel.',[LChannelNo]);
            Result := False;
          end
          else
            Result := True;

          if (LChannelNo = FChannelNumber) then
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

function TMinMaxUpperBoundChannel.ValidateBoundedChannels(AErrorMessages: TStrings): WordBool;
const OPNAME = 'TMinMaxUpperBoundChannel.ValidateBoundedChannels';
begin
  Result := False;
  try
    Result := ValidateChannelNumber(AErrorMessages);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TMinMaxUpperBoundChannel._AddRef: Integer;
const OPNAME = 'TMinMaxUpperBoundChannel._AddRef';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TMinMaxUpperBoundChannel._Release: Integer;
const OPNAME = 'TMinMaxUpperBoundChannel._Release';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

end.
