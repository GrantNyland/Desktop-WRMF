{******************************************************************************}
{*  UNIT      : Contains the class TWetland                                   *}
{*  AUTHOR    : Maurice Marinus                                               *}
{*  DATE      : 2006/08/07                                                    *}
{*  COPYRIGHT : Copyright © 2006 DWAF                                         *}
{******************************************************************************}

unit UWetland;

interface

uses
  Classes,
  Contnrs,
  UAbstractObject,
  VoaimsCom_TLB,
  UConstants,
  VCL.Dialogs,
  UReservoirData;

type
  TWetland = class(TAbstractAppObject, IWetland)
  protected
    FInflowChannelNr     : Integer;
    FOutflowChannelNr    : Integer;
    FUpstreamThreshold   : Double;
    FInflowProportion    : Double;
    FStorageVolume       : Double;
    FOutflowProportion   : Double;
    FIdentifier          : Integer;
    FNodeNumber          : Integer;
    FName                : WideString;

    function Get_Identifier: Integer; safecall;
    function Get_InflowProportion: Double; safecall;
    function Get_Name: WideString; safecall;
    function Get_NodeNumber: Integer; safecall;
    function Get_OutflowProportion: Double; safecall;
    function Get_StorageVolume: Double; safecall;
    function Get_UpstreamThreshold: Double; safecall;
    function Get_InflowChannel: IGeneralFlowChannel; safecall;
    function Get_OutflowChannel: IGeneralFlowChannel; safecall;
    function Get_ReservoirDetails: IReservoirData; safecall;

    procedure Set_InflowProportion(Value: Double); safecall;
    procedure Set_Name(const Value: WideString); safecall;
    procedure Set_NodeNumber(Value: Integer); safecall;
    procedure Set_OutflowProportion(Value: Double); safecall;
    procedure Set_StorageVolume(Value: Double); safecall;
    procedure Set_UpstreamThreshold(Value: Double); safecall;

    function ValidateNodeNumber(AErrorMessages: TStrings)       : Boolean;
    function ValidateWetlandName(AErrorMessages: TStrings)      : Boolean;
    function ValidateUpstreamThreshold(AErrorMessages: TStrings): Boolean;
    function ValidateInflowProportion(AErrorMessages: TStrings) : Boolean;
    function ValidateStorageVolume(AErrorMessages: TStrings)    : Boolean;
    function ValidateOutflowProportion(AErrorMessages: TStrings): Boolean;
    function ValidateInflowChannelNode(AErrorMessages : TStrings) : WordBool;
    function ValidateOutflowChannelNode(AErrorMessages : TStrings): WordBool;

    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
  public
    procedure Assign(ASource: TWetland);virtual;
    function Initialise : Boolean; override;
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; safecall;
    //function Assign(AWetland: TWetland) : Boolean;
    function Populate(AIdentifier, ANodeNumber: Integer; AName: string;
                           AUpstreamThreshold, AInflowProportion,
                           AStorageVolume, AOutflowProportion : Double;
                           AInflowChannelNr, AOutflowChannelNr: Integer): Boolean;

    property Identifier         : Integer     read Get_Identifier;
    property NodeNumber         : Integer     read Get_NodeNumber write Set_NodeNumber;
    property Name               : WideString  read Get_Name write Set_Name;
    property StorageVolume      : Double      read Get_StorageVolume write Set_StorageVolume;
    property InflowProportion   : Double      read Get_InflowProportion write Set_InflowProportion;
    property OutflowProportion  : Double      read Get_OutflowProportion write Set_OutflowProportion;
    property UpstreamThreshold  : Double      read Get_UpstreamThreshold write Set_UpstreamThreshold;

    property InflowChannelNr    : Integer             read FInflowChannelNr;
    property OutflowChannelNr   : Integer             read FOutflowChannelNr;
    property InflowChannel      : IGeneralFlowChannel read Get_InflowChannel;
    property OutflowChannel     : IGeneralFlowChannel read Get_OutflowChannel;
    property ReservoirDetails   : IReservoirData      read Get_ReservoirDetails;
  end;

  TWetlandList = class(TAbstractAppObject,IWetlandList)
  protected
    FWetlandList  : TObjectList;

    function Get_WetlandCount: Integer; safecall;

    function AddWetland(AFeature : TWetland): Boolean;
    function CreateNewWetland: TWetland;
    function DeleteWetlandWithID(AWetlandID : Integer) : WordBool;
    function DeleteWetlandWithIndex(AIndex : Integer) : WordBool;

    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;

    function GetInflowChannelID(AWetlandID : Integer): Integer;
    function GetOutflowChannelID(AWetlandID : Integer): Integer;
  public
    function Initialise : Boolean; override;
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; safecall;


    function Get_WetlandByID(AWetlandID: integer): IWetland; safecall;
    function Get_WetlandByIndex(AIndex: integer): IWetland; safecall;
    function Get_WetlandByNodeNumber(AWetlandNumber: Integer): IWetland; safecall;

    function CastWetlandByID(AWetlandID: Integer): TWetland;
    function CastWetlandByIndex(AIndex : Integer): TWetland;
    function CastWetlandByNodeNumber(ANodeNumber : Integer): TWetland;

    function NewWetland : TWetland;
    function CreateWetland : IWetland; safecall;
    function CopyCreate(AWetlandID : integer) : IWetland; safecall;
    function RemoveWetland(ANodeNumber : integer) : WordBool; safecall;

    property WetlandByIndex[AIndex: Integer]:               IWetland read Get_WetlandByIndex;
    property WetlandByID[AWetlandID: Integer]:              IWetland read Get_WetlandByID;
    property WetlandByNodeNumber[AWetlandNumber: Integer]:  IWetland read Get_WetlandByNodeNumber;

    function WetlandByName(const AName: WideString): IWetland; safecall;

    property WetLandCount : Integer read Get_WetlandCount;
    procedure GetChannelNumbers(AWetlandID: Integer; var AInflowChannelNumber, AOutflowChannelNumber: Integer);
  end;

implementation

uses
  SysUtils,
  Math,
  UYieldModelDataObject,
  UNetworkElementData,
  UWetlandSQLAgent,
  UNetworkFeaturesSQLAgent,
  UErrorHandlingOperations;
  
{ TWetland }

procedure TWetland.CreateMemberObjects;
const OPNAME = 'TWetland.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
    Initialise;  
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TWetland.DestroyMemberObjects;
const OPNAME = 'TWetland.DestroyMemberObjects';
begin
  try
    inherited DestroyMemberObjects;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TWetland.Get_Identifier: Integer;
const OPNAME = 'TWetland.Get_Identifier';
begin
  try
    Result := FIdentifier;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TWetland.Get_InflowChannel: IGeneralFlowChannel;
const OPNAME = 'TWetland.Get_InflowChannel';
begin
  Result := nil;
  try
    Result := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.ChannelList.ChannelByChannelNumber[FInflowChannelNr];
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TWetland.Get_InflowProportion: Double;
const OPNAME = 'TWetland.Get_InflowProportion';
begin
  try
    Result := FInflowProportion;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TWetland.Get_Name: WideString;
const OPNAME = 'TWetland.Get_Name';
begin
  try
    Result := FName;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TWetland.Get_NodeNumber: Integer;
const OPNAME = 'TWetland.Get_NodeNumber';
begin
  try
    Result := FNodeNumber;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TWetland.Get_OutflowProportion: Double;
const OPNAME = 'TWetland.Get_OutflowProportion';
begin
  try
    Result := FOutflowProportion;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TWetland.Get_OutflowChannel: IGeneralFlowChannel;
const OPNAME = 'TWetland.Get_OutflowChannel';
begin
  Result := nil;
  try
    Result := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.ChannelList.ChannelByChannelNumber[FOutflowChannelNr];
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TWetland.Get_StorageVolume: Double;
const OPNAME = 'TWetland.Get_StorageVolume';
begin
  try
    Result := FStorageVolume;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TWetland.Get_UpstreamThreshold: Double;
const OPNAME = 'TWetland.Get_UpstreamThreshold';
begin
  try
    Result := FUpstreamThreshold;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TWetland.Initialise: Boolean;
const OPNAME = 'TWetland.Initialise';
begin
  Result := inherited Initialise;
  try
    FIdentifier         := NullInteger;
    FNodeNumber         := NullInteger;
    FName               := '';
    FUpstreamThreshold  := 0.0;
    FInflowProportion   := 0.0;
    FStorageVolume      := 0.0;
    FOutflowProportion  := 0.0;
    FInflowChannelNr    := 0;
    FOutflowChannelNr   := 0;
  except on E: Exception do HandleError(E, OPNAME); end;
end;
{
function TWetland.Assign(AWetland: TWetland): Boolean;
const OPNAME = 'TWetland.Assign';
begin
  Result := False;
  try
    FIdentifier         := AWetland.Identifier;
    FName               := AWetland.Name;
    FNodeNumber         := AWetland.NodeNumber;
    FUpstreamThreshold  := AWetland.UpstreamThreshold;
    FInflowProportion   := AWetland.InflowProportion;
    FStorageVolume      := AWetland.StorageVolume;
    FOutflowProportion  := AWetland.OutflowProportion;
    if AWetland.InflowChannel <> nil then
      FInflowChannelNr    := AWetland.InflowChannel.ChannelNumber;
    if AWetland.OutflowChannel <> nil then
      FOutflowChannelNr   := AWetland.OutflowChannel.ChannelNumber;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;
 }
function TWetland.Populate(AIdentifier, ANodeNumber: Integer; AName: string;
                           AUpstreamThreshold, AInflowProportion,
                           AStorageVolume, AOutflowProportion : Double;
                           AInflowChannelNr, AOutflowChannelNr: Integer): Boolean;
const OPNAME = 'TWetland.Populate';
begin
  Result := False;
  try
    FIdentifier         := AIdentifier;
    FNodeNumber         := ANodeNumber;
    FName               := AName;
    FUpstreamThreshold  := AUpstreamThreshold;
    FInflowProportion   := AInflowProportion;
    FStorageVolume      := AStorageVolume;
    FOutflowProportion  := AOutflowProportion;
    FInflowChannelNr    := AInflowChannelNr;
    FOutflowChannelNr   := AOutflowChannelNr;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TWetland.Set_InflowProportion(Value: Double);
const OPNAME = 'TWetland.Set_InflowProportion';
var
  LLoadAgent    : TWetlandSQLAgent;
  LContextData  : TStringList;
  LOldValue     : string;
begin
  try
    if FInflowProportion <> Value then
    begin
      LContextData  := TStringList.Create;
      LLoadAgent    := TWetlandSQLAgent.Create(FAppModules);
      try
        LOldValue := FloatToStr(FInflowProportion);
        LLoadAgent.LoadContextData_FeatureID(LContextData, IntToStr(FIdentifier));
        if FAppModules.FieldProperties.UpdateFieldValue('InflowProportion', FloatToStr(Value), LOldValue, LContextData ) then
        begin
          FInflowProportion := Value;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'InflowProportion',LOldValue,FloatToStr(Value));
        end;
      finally
        FreeAndNil(LLoadAgent);
        FreeAndNil(LContextData);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TWetland.Set_Name(const Value: WideString);
const OPNAME = 'TWetland.Set_Name';
var
  LLoadAgent    : TWetlandSQLAgent;
  LContextData  : TStringList;
  LOldValue     : string;
  LReservoirDetails    : IReservoirData;
begin
  try
    if FName <> Value then
    begin
      LContextData  := TStringList.Create;
      LLoadAgent    := TWetlandSQLAgent.Create(FAppModules);
      try
        LOldValue := FName;
        LLoadAgent.LoadContextData_FeatureID(LContextData, IntToStr(FIdentifier));
        if FAppModules.FieldProperties.UpdateFieldValue('WetlandName', Value, LOldValue, LContextData ) then
        begin
          FName := Value;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'WetlandName',LOldValue,Value);
          LReservoirDetails := ReservoirDetails;
          if(LReservoirDetails <> nil) then
            LReservoirDetails.ReservoirConfigurationData.ReservoirName := Value;
        end;
      finally
        FreeAndNil(LLoadAgent);
        FreeAndNil(LContextData);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TWetland.Set_NodeNumber(Value: Integer);
const OPNAME = 'TWetland.Set_NodeNumber';
var
  LLoadAgent    : TWetlandSQLAgent;
  LContextData  : TStringList;
  LOldValue     : string;
begin
  try
    if FNodeNumber <> Value then
    begin
      LContextData  := TStringList.Create;
      LLoadAgent    := TWetlandSQLAgent.Create(FAppModules);
      try
        LOldValue := IntToStr(FNodeNumber);
        LLoadAgent.LoadContextData_FeatureID(LContextData, IntToStr(FIdentifier));
        if FAppModules.FieldProperties.UpdateFieldValue('NodeNumber', IntToStr(Value), LOldValue, LContextData ) then
        begin
          FNodeNumber := Value;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'NodeNumber', LOldValue, IntToStr(Value));
        end;
      finally
        FreeAndNil(LLoadAgent);
        FreeAndNil(LContextData);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TWetland.Set_OutflowProportion(Value: Double);
const OPNAME = 'TWetland.Set_OutflowProportion';
var
  LLoadAgent    : TWetlandSQLAgent;
  LContextData  : TStringList;
  LOldValue     : string;
begin
  try
    if FOutflowProportion <> Value then
    begin
      LContextData  := TStringList.Create;
      LLoadAgent    := TWetlandSQLAgent.Create(FAppModules);
      try
        LOldValue := FloatToStr(FOutflowProportion);
        LLoadAgent.LoadContextData_FeatureID(LContextData, IntToStr(FIdentifier));
        if FAppModules.FieldProperties.UpdateFieldValue('OutflowProportion', FloatToStr(Value), LOldValue, LContextData ) then
        begin
          FOutflowProportion := Value;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'OutflowProportion',LOldValue,FloatToStr(Value));
        end;
      finally
        FreeAndNil(LLoadAgent);
        FreeAndNil(LContextData);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TWetland.Set_StorageVolume(Value: Double);
const OPNAME = 'TWetland.Set_StorageVolume';
var
  LLoadAgent    : TWetlandSQLAgent;
  LContextData  : TStringList;
  LOldValue     : string;
begin
  try
    if FStorageVolume <> Value then
    begin
      LContextData  := TStringList.Create;
      LLoadAgent    := TWetlandSQLAgent.Create(FAppModules);
      try
        LOldValue := FloatToStr(FStorageVolume);
        LLoadAgent.LoadContextData_FeatureID(LContextData, IntToStr(FIdentifier));
        if FAppModules.FieldProperties.UpdateFieldValue('StorageVolume', FloatToStr(Value), LOldValue, LContextData ) then
        begin
          FStorageVolume := Value;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'StorageVolume',LOldValue,FloatToStr(Value));
        end;
      finally
        FreeAndNil(LLoadAgent);
        FreeAndNil(LContextData);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TWetland.Set_UpstreamThreshold(Value: Double);
const OPNAME = 'TWetland.Set_UpstreamThreshold';
var
  LLoadAgent    : TWetlandSQLAgent;
  LContextData  : TStringList;
  LOldValue     : string;
begin
  try
    if FUpstreamThreshold <> Value then
    begin
      LContextData  := TStringList.Create;
      LLoadAgent    := TWetlandSQLAgent.Create(FAppModules);
      try
        LOldValue := FloatToStr(FUpstreamThreshold);
        LLoadAgent.LoadContextData_FeatureID(LContextData, IntToStr(FIdentifier));
        if FAppModules.FieldProperties.UpdateFieldValue('UpstreamThreshold', FloatToStr(Value), LOldValue, LContextData ) then
        begin
          FUpstreamThreshold := Value;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'UpstreamThreshold',LOldValue,FloatToStr(Value));
        end;
      finally
        FreeAndNil(LLoadAgent);
        FreeAndNil(LContextData);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TWetland.Validate(var AErrors: WideString; const AContext: WideString): WordBool;
const OPNAME = 'TWetland.Validate';
var
  LErrorMessage : TStringList;
  LErrorCols    : TStringList;
  lStopOnFirstError : Boolean;
begin
  Result := False;
  try
    LErrorMessage := TStringList.Create;
    LErrorCols    := TStringList.Create;
    try
      if (AContext = 'WetlandName') then
        Result := ValidateWetlandName(lErrorMessage)
      else
      if (AContext = 'WetlandNodeNumber') then
        Result := ValidateNodeNumber(lErrorMessage)
      else
      if (AContext = 'UpstreamThreshold') then
        Result := ValidateUpstreamThreshold(lErrorMessage)
      else
      if (AContext = 'InflowProportion') then
        Result := ValidateInflowProportion(lErrorMessage)
      else
      if (AContext = 'StorageVolume') then
        Result := ValidateStorageVolume(lErrorMessage)
      else
      if (AContext = 'OutflowProportion') then
        Result := ValidateOutflowProportion(lErrorMessage)
      else
      if (AContext = 'DiversionUpstreamNode') then
        Result := ValidateInflowChannelNode(lErrorMessage)
      else
      if (AContext = 'ReturnFlowDownstreamNode') then
        Result := ValidateOutflowChannelNode(lErrorMessage)
      else
      begin
        Result := True;
        lStopOnFirstError := FAppModules.GlobalData.StopOnFirstErr;
        if (not ValidateWetlandName(lErrorMessage))           then Result := False;
        if (Result OR (NOT lStopOnFirstError)) then
        begin
          if (not ValidateNodeNumber(lErrorMessage))          then Result := False;
        end;
        if (Result OR (NOT lStopOnFirstError)) then
        begin
          if (not ValidateUpstreamThreshold(lErrorMessage))   then Result := False;
        end;
        if (Result OR (NOT lStopOnFirstError)) then
        begin
          if (not ValidateInflowProportion(lErrorMessage))    then Result := False;
        end;
        if (Result OR (NOT lStopOnFirstError)) then
        begin
          if (not ValidateStorageVolume(lErrorMessage))       then Result := False;
        end;
        if (Result OR (NOT lStopOnFirstError)) then
        begin
          if (not ValidateOutflowProportion(lErrorMessage))   then Result := False;
        end;
        if (Result OR (NOT lStopOnFirstError)) then
        begin
          if (not ValidateInflowChannelNode(lErrorMessage))   then Result := False;
        end;
        if (Result OR (NOT lStopOnFirstError)) then
        begin
          if (not ValidateOutflowChannelNode(lErrorMessage))  then Result := False;
        end;  
      end;
      AErrors := AErrors + lErrorMessage.Text;
    finally
      LErrorMessage.Free;
      LErrorCols.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TWetland.ValidateInflowChannelNode(AErrorMessages: TStrings): WordBool;
const OPNAME = 'TWetland.ValidateInflowChannelNode';
var
  lMessage       : WideString;
  lInflowChannel : IGeneralFlowChannel;
begin
  Result := True;
  try
    if (FInflowChannelNr <> 0) then
    begin
      lInflowChannel := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.
                             ChannelList.ChannelByChannelNumber[FInflowChannelNr];
      if (lInflowChannel.UpStreamNodeNumber = 0) then
      begin
        lMessage := FAppModules.Language.GetString('ContextValidation.InvalidUpstreamNodeNumber');
        AErrorMessages.Add('ERROR:' +Format(lMessage, [FName]));
        Result := False;
      end
      else
      begin
        if (not lInflowChannel.Validate(lMessage,'UpNodeNumber')) then
        begin
          Result := False;
          AErrorMessages.Add('ERROR:' +FName+ ':'+lMessage);
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TWetland.ValidateInflowProportion(AErrorMessages: TStrings): Boolean;
const OPNAME = 'TWetland.ValidateInflowProportion';
var
  lMessage : string;
begin
  Result := False;
  try
    lMessage := '';
    if (not FAppModules.FieldProperties.ValidateFieldProperty('InflowProportion', FloatToStr(FInflowProportion), lMessage)) then
      AErrorMessages.Add('ERROR:' +FloatToStr(FInflowProportion)+ ':'+lMessage)
    else
      Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TWetland.ValidateNodeNumber(AErrorMessages: TStrings): Boolean;
const OPNAME = 'TWetland.ValidateNodeNumber';
var
  lMessage : string;
begin
  Result := False;
  try
    lMessage := '';
    if (not FAppModules.FieldProperties.ValidateFieldProperty('WetlandNodeNumber', IntToStr(FNodeNumber), lMessage)) then
      AErrorMessages.Add('ERROR:' +FloatToStr(FNodeNumber)+ ':'+lMessage)
    else
      Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TWetland.ValidateOutflowChannelNode(AErrorMessages: TStrings): WordBool;
const OPNAME = 'TWetland.ValidateOutflowChannelNode';
var
  lMessage       : WideString;
  lOutflowChannel : IGeneralFlowChannel;
begin
  Result := True;
  try
    if (FOutflowChannelNr <> 0) then
    begin
      lOutflowChannel := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.
                             ChannelList.ChannelByChannelNumber[FOutflowChannelNr];
      if (lOutflowChannel.UpStreamNodeNumber = 0) then
      begin
        lMessage := FAppModules.Language.GetString('ContextValidation.InvalidDownstreamNodeNumber');
        AErrorMessages.Add('ERROR:' +Format(lMessage, [FName]));
        Result := False;
      end
      else
      begin
        if (not lOutflowChannel.Validate(lMessage,'DownNodeNumber')) then
        begin
          Result := False;
          AErrorMessages.Add('ERROR:' +FName+ ':'+lMessage);
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TWetland.ValidateOutflowProportion(AErrorMessages: TStrings): Boolean;
const OPNAME = 'TWetland.ValidateOutflowProportion';
var
  lMessage : string;
begin
  Result := False;
  try
    lMessage := '';
    if (not FAppModules.FieldProperties.ValidateFieldProperty('OutflowProportion', FloatToStr(FOutflowProportion), lMessage)) then
      AErrorMessages.Add('ERROR:' +FloatToStr(FOutflowProportion)+ ':'+lMessage)
    else
      Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TWetland.ValidateStorageVolume(AErrorMessages: TStrings): Boolean;
const OPNAME = 'TWetland.ValidateStorageVolume';
var
  lMessage : string;
begin
  Result := False;
  try
    lMessage := '';
    if (not FAppModules.FieldProperties.ValidateFieldProperty('StorageVolume', FloatToStr(FStorageVolume), lMessage)) then
      AErrorMessages.Add('ERROR:' +FloatToStr(FStorageVolume)+ ':'+lMessage)
    else
      Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TWetland.ValidateUpstreamThreshold(AErrorMessages: TStrings): Boolean;
const OPNAME = 'TWetland.ValidateUpstreamThreshold';
var
  lMessage : string;
begin
  Result := False;
  try
    lMessage := '';
    if (not FAppModules.FieldProperties.ValidateFieldProperty('UpstreamThreshold', FloatToStr(FUpstreamThreshold), lMessage)) then
      AErrorMessages.Add('ERROR:' +FloatToStr(FUpstreamThreshold)+ ':'+lMessage)
    else
      Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TWetland.ValidateWetlandName(AErrorMessages: TStrings): Boolean;
const OPNAME = 'TWetland.ValidateWetlandName';
var
  lMessage : string;
begin
  Result := True;
  try
    lMessage := '';
    if (not FAppModules.FieldProperties.ValidateFieldProperty('WetLandName', FName, lMessage)) then
      AErrorMessages.Add('WARNING:' +FName + ':'+lMessage);
    //else
    //  Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TWetland._AddRef: Integer;
const OPNAME = 'TWetland._AddRef';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TWetland._Release: Integer;
const OPNAME = 'TWetland._Release';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TWetland.Get_ReservoirDetails: IReservoirData;
const OPNAME = 'TWetland.Get_ReservoirDetails';
begin
  Result := nil;
  try
    Result := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.ReservoirList.ReservoirByIdentifier[FNodeNumber];
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TWetland.Assign(ASource: TWetland);
const OPNAME = 'TWetland.Assign';
var
  LDestNode : TReservoirData;
  LDestInflow : IGeneralFlowChannel;
  LDestOutFlow : IGeneralFlowChannel;
  LChannelList : IChannelList;
  LSourceOutFlow : IGeneralFlowChannel;
  LSourceInflow : IGeneralFlowChannel;
  LSourceNode : TReservoirData;
  LChnnelPenalty : IChannelPenalty;
begin
  try
    Name               := 'Copy of '+ASource.Name;
    StorageVolume      := ASource.StorageVolume;
    InflowProportion   := ASource.InflowProportion;
    OutflowProportion  := ASource.OutflowProportion;
    UpstreamThreshold  := ASource.UpstreamThreshold;

    LSourceNode := TYieldModelDataObject(FAppModules.Model.ModelData).CastNetworkElementData.
                            CastReservoirList.CastReservoirOrNodeByIdentifier[ASource.FNodeNumber];
    LDestNode := TYieldModelDataObject(FAppModules.Model.ModelData).CastNetworkElementData.
                             CastReservoirList.CastReservoirOrNodeByIdentifier[FNodeNumber];
    if (LDestNode <> nil) and (LSourceNode <> nil) then
      LDestNode.Assign(LSourceNode);
    LChannelList := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.ChannelList;
    if LChannelList <> nil then
    begin
      LDestInflow := LChannelList.ChannelByChannelNumber[FInflowChannelNr];
      LSourceInflow := LChannelList.ChannelByChannelNumber[ASource.InflowChannelNr];
      if (LDestInflow <> nil) and (LSourceInflow <> nil) then
      begin
        LDestInflow.ChannelName := 'Copy of '+ LSourceInflow.ChannelName;
        LDestInflow.ChannelType := LSourceInflow.ChannelType;
        LDestInflow.ChannelSubType := LSourceInflow.ChannelSubType;
        LChnnelPenalty := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.ChannelPenaltyList.
                            ChannelPenaltyByIdentifier[LSourceInflow.ChannelPenaltyNumber];
        if LChnnelPenalty <> nil then
          LDestInflow.ChannelPenalty := LChnnelPenalty;
      end;
      LDestOutFlow := LChannelList.ChannelByChannelNumber[FOutflowChannelNr];
      LSourceOutFlow := LChannelList.ChannelByChannelNumber[ASource.OutflowChannelNr];
      if (LDestOutFlow <> nil) and (LSourceOutFlow <> nil) then
      begin
        LDestOutFlow.ChannelName := 'Copy of '+ LSourceOutFlow.ChannelName;
        LDestOutFlow.ChannelType := LSourceOutFlow.ChannelType;
        LDestOutFlow.ChannelSubType := LSourceOutFlow.ChannelSubType;
        LChnnelPenalty := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.ChannelPenaltyList.
                            ChannelPenaltyByIdentifier[LSourceOutFlow.ChannelPenaltyNumber];
        if LChnnelPenalty <> nil then
          LDestOutFlow.ChannelPenalty := LChnnelPenalty;
      end;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

{ TWetlandList }

function TWetlandList._AddRef: Integer;
const OPNAME = 'TWetlandList._AddRef';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TWetlandList._Release: Integer;
const OPNAME = 'TWetlandList._Release';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TWetlandList.CreateMemberObjects;
const OPNAME = 'TWetlandList.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
    FWetlandList := TObjectList.Create(True);
    Initialise;
 except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TWetlandList.DestroyMemberObjects;
const OPNAME = 'TWetlandList.DestroyMemberObjects';
begin
  try
    FreeAndNil(FWetlandList);
    inherited;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TWetlandList.Initialise: Boolean;
const OPNAME = 'TWetlandList.Initialise';
begin
  Result := inherited Initialise;
  try
    FWetlandList.Clear;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TWetlandList.Get_WetlandCount: Integer;
const OPNAME = 'TWetlandList.Get_WetlandCount';
begin
  Result := 0;
  try
    Result := FWetlandList.Count;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TWetlandList.CastWetlandByID(AWetlandID: Integer): TWetland;
const OPNAME = 'TWetlandList.CastWetlandByID';
var
 LIndex   : Integer;
 LWetland : TWetland;
begin
  Result := nil;
  try
    LIndex := 0;
    while ((Result = nil) and (LIndex < FWetlandList.Count)) do
    begin
      LWetland := TWetland(FWetlandList.Items[LIndex]);
      if (LWetland.FIdentifier = AWetlandID) then
        Result := LWetland
      else
        LIndex := LIndex + 1;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TWetlandList.CastWetlandByIndex(AIndex: Integer): TWetland;
const OPNAME = 'TWetlandList.CastWetlandByIndex';
begin
  Result := nil;
  try
    if (AIndex >= 0) and (AIndex < FWetlandList.Count) then
      Result := TWetland(FWetlandList.Items[AIndex]);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TWetlandList.Get_WetlandByNodeNumber(AWetlandNumber: Integer): IWetland;
const OPNAME = 'TWetlandList.Get_WetlandByNodeNumber';
var
  lIndex    : Integer;
  LWetland  : TWetland;
begin
  Result := nil;
  try
    lIndex := 0;
    while ((Result = nil) and (lIndex < FWetlandList.Count)) do
    begin
      LWetland := TWetland(FWetlandList.Items[lIndex]);
      if (LWetland.NodeNumber = AWetlandNumber) then
        Result := LWetland
      else
        lIndex := lIndex + 1;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TWetlandList.Get_WetlandByID(AWetlandID: integer): IWetland;
const OPNAME = 'TWetlandList.Get_WetlandByID';
var
  lIndex    : Integer;
  LWetland  : TWetland;
begin
  Result := nil;
  try
    lIndex := 0;
    while ((Result = nil) and (lIndex < FWetlandList.Count)) do
    begin
      LWetland := TWetland(FWetlandList.Items[lIndex]);
      if (LWetland.FIdentifier = AWetlandID) then
        Result := LWetland
      else
        lIndex := lIndex + 1;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TWetlandList.Get_WetlandByIndex(AIndex: integer): IWetland;
const OPNAME = 'TWetlandList.Get_WetlandByIndex';
begin
  Result := nil;
  try
    if ((AIndex >= 0) and (AIndex < FWetlandList.Count)) then
      Result := TWetland(FWetlandList.Items[AIndex]);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TWetlandList.CastWetlandByNodeNumber(ANodeNumber: Integer): TWetland;
const OPNAME = 'TWetlandList.CastWetlandByNodeNumber';
var
  lIndex    : Integer;
  LWetland  : TWetland;
begin
  Result := nil;
  try
    lIndex := 0;
    while ((Result = nil) and (lIndex < FWetlandList.Count)) do
    begin
      LWetland := TWetland(FWetlandList.Items[lIndex]);
      if (LWetland.NodeNumber = ANodeNumber) then
        Result := LWetland
      else
        lIndex := lIndex + 1;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TWetlandList.WetlandByName(const AName: WideString): IWetland;
const OPNAME = 'TWetlandList.WetlandByName';
var
  lIndex    : Integer;
  LWetland  : TWetland;
begin
  Result := nil;
  try
    lIndex := 0;
    while ((Result = nil) and (lIndex < FWetlandList.Count)) do
    begin
      LWetland := TWetland(FWetlandList.Items[lIndex]);
      if (LWetland.FName = AName) then
        Result := LWetland
      else
        lIndex := lIndex + 1;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TWetlandList.NewWetland: TWetland;
const OPNAME = 'TWetlandList.NewWetland';
begin
  Result := nil;
  try
    Result := TWetland.Create(FAppModules);
    FWetlandList.Add(Result);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TWetlandList.CreateWetland: IWetland;
const OPNAME = 'TWetlandList.CreateWetland';
begin
  Result := nil;
  try
    Result := CreateNewWetland;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TWetlandList.CreateNewWetland: TWetland;
const OPNAME = 'TWetlandList.CreateNewWetland';
var
  lLoadAgent          : TWetlandSQLAgent;
  lInflowChannel      : IGeneralFlowChannel;
  lOutflowChannel     : IGeneralFlowChannel;
  lReservoir          : IReservoirData;
  lWetland            : TWetland;
  lNetworkElementData : TNetworkElementData;
  LNewIdentifier      : integer;
begin
  Result := nil;
  try
    LLoadAgent := TWetlandSQLAgent.Create(FAppModules);
    try
      lInflowChannel  := nil;
      lOutflowChannel := nil;
      lReservoir      := nil;
      lWetland        := nil;
      lWetland        := TWetland.Create(FAppModules);
      lNetworkElementData := TYieldModelDataObject(FAppModules.Model.ModelData).CastNetworkElementData;
      try
        lInflowChannel      := lNetworkElementData.CastChannelList.CreateNewChannel;
        lOutflowChannel     := lNetworkElementData.CastChannelList.CreateNewChannel;
        lReservoir          := lNetworkElementData.CastReservoirList.CreateReservoir(ntWetlandNode);
        LNewIdentifier      := lLoadAgent.GetMaxIdentifier;

        lWetland.Initialise;
        lWetland.FNodeNumber        := lReservoir.ReservoirConfigurationData.ReservoirIdentifier;
        lWetland.FInflowChannelNr   := lInflowChannel.ChannelNumber;
        lWetland.FOutflowChannelNr  := lOutflowChannel.ChannelNumber;
        lWetland.FIdentifier        := LNewIdentifier + 1;
        lWetland.FName              := 'Wetland('+IntToStr(lWetland.FIdentifier)+')';

        if LLoadAgent.InsertWetland(lWetland) then
        begin
          lInflowChannel.ChannelType          := 16;
          lInflowChannel.DownStreamNodeNumber := lWetland.FNodeNumber;
          lOutflowChannel.UpStreamNodeNumber  := lWetland.FNodeNumber;
          lOutflowChannel.ChannelType         := 17;

          lReservoir.ReservoirConfigurationData.ReservoirName := lWetland.FName;
          FWetlandList.Add(lWetland);
          Result := lWetland;
        end
        else
        begin
          FreeAndNil(lWetland);
          lNetworkElementData.CastChannelList.DeleteGeneralFlowChannelWithID(lInflowChannel.ChannelID);
          lNetworkElementData.CastChannelList.DeleteGeneralFlowChannelWithID(lOutflowChannel.ChannelID);
          lNetworkElementData.CastReservoirList.DeleteReservoir(lReservoir.ReservoirConfigurationData.ReservoirIdentifier);
        end;
      except
        if(lWetland <> nil) then
          FreeAndNil(lWetland);
        if(lInflowChannel <> nil) then
          lNetworkElementData.CastChannelList.DeleteGeneralFlowChannelWithID(lInflowChannel.ChannelID);
        if(lOutflowChannel <> nil) then
          lNetworkElementData.CastChannelList.DeleteGeneralFlowChannelWithID(lOutflowChannel.ChannelID);
        if(lReservoir <> nil) then
          lNetworkElementData.CastReservoirList.DeleteReservoir(lReservoir.ReservoirConfigurationData.ReservoirIdentifier);
      end;
    finally
      FreeAndNil(LLoadAgent);
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TWetlandList.RemoveWetland(ANodeNumber: integer): WordBool;
const OPNAME = 'TWetlandList.RemoveWetland';
var
  lLoadAgent             : TWetlandSQLAgent;
  lNetworkElementData    : TNetworkElementData;
  lInflowChannelNumber   : Integer;
  lOutflowChannelNumber  : Integer;
  LWetland               : TWetland;
  LWetlandID             : Integer;
begin
  Result   := False;
  try
    LWetland    := CastWetlandByNodeNumber(ANodeNumber);
    if (LWetland = nil) then  Exit;

    lNetworkElementData := TYieldModelDataObject(FAppModules.Model.ModelData).CastNetworkElementData;

    LWetlandID  := LWetland.Identifier;
    lNetworkElementData.CastReservoirList.DeleteReservoir(ANodeNumber);

    lInflowChannelNumber  := GetInflowChannelID(LWetlandID);
    lOutflowChannelNumber := GetOutflowChannelID(LWetlandID);

    if(lInflowChannelNumber >= 0) then
      lNetworkElementData.ChannelList.RemoveChannelWithID(lInflowChannelNumber);
    if(lOutflowChannelNumber >= 0) then
      lNetworkElementData.ChannelList.RemoveChannelWithID(lOutflowChannelNumber);

    LLoadAgent := TWetlandSQLAgent.Create(FAppModules);
    try
      if LLoadAgent.DeleteWetland(ANodeNumber) then
      begin
        DeleteWetlandWithID(LWetlandID);
        Result := True;
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TWetlandList.DeleteWetlandWithID(AWetlandID: Integer): WordBool;
const OPNAME = 'TWetlandList.DeleteWetlandWithID';
var
  lWetland  : TWetland;
  lIndex    : Integer;
begin
  Result := False;
  try
    lWetland := CastWetlandByID(AWetlandID);
    if (lWetland <> nil) then
    begin
      lIndex := FWetlandList.IndexOf(lWetland);
      Result := DeleteWetlandWithIndex(lIndex);
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TWetlandList.DeleteWetlandWithIndex(AIndex: Integer): WordBool;
const OPNAME = 'TWetlandList.DeleteWetlandWithIndex';
begin
  Result := False;
  try
    if (AIndex >= 0) then
    begin
      FWetlandList.Delete(AIndex);
      Result := True;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TWetlandList.AddWetland(AFeature: TWetland): Boolean;
const OPNAME = 'TWetlandList.AddWetland';
begin
  Result := False;
  try
    if (AFeature <> nil) then
    begin
      FWetlandList.Add(AFeature);
      Result := True;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TWetlandList.Validate(var AErrors: WideString; const AContext: WideString): WordBool;
const OPNAME = 'TWetlandList.Validate';
var
  LIndex: integer;
begin
  Result := True;
  try
    if (FAppModules.StudyArea.ModelVersion <> '7') then
      Exit;
    for LIndex := 0 to FWetlandList.Count -1 do
    begin
      if not TWetland(FWetlandList[LIndex]).Validate(AErrors,AContext) then
      begin
        Result := False;
        if FAppModules.GlobalData.StopOnFirstErr then
          Break;
      end
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TWetlandList.GetChannelNumbers(AWetlandID: Integer; var AInflowChannelNumber, AOutflowChannelNumber: Integer);
const OPNAME = 'TWetlandList.GetChannelNumbers';
var
 LWetland : TWetland;
begin
  try
    AInflowChannelNumber  := NullInteger;
    AOutflowChannelNumber := NullInteger;
    LWetland              := CastWetlandByID(AWetlandID);
    if (LWetland <> nil) then
    begin
      AInflowChannelNumber   := LWetland.InflowChannel.ChannelNumber;
      AOutflowChannelNumber  := LWetland.OutflowChannel.ChannelNumber;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TWetlandList.GetInflowChannelID(AWetlandID: Integer): Integer;
const OPNAME = 'TWetlandList.GetInflowChannelID';
var
  LWetland  : TWetland;
  LChannel  : IGeneralFlowChannel;
begin
  Result := NullInteger;
  try
    LWetland := CastWetlandByID(AWetlandID);
    if(LWetland <> nil) then
    begin
      LChannel := LWetland.InflowChannel;
      if(LChannel <> nil) then
         Result := LChannel.ChannelID;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TWetlandList.GetOutflowChannelID(AWetlandID: Integer): Integer;
const OPNAME = 'TWetlandList.GetOutflowChannelID';
var
  LWetland  : TWetland;
  LChannel  : IGeneralFlowChannel;
begin
  Result := NullInteger;
  try
    LWetland := CastWetlandByID(AWetlandID);
    if(LWetland <> nil) then
    begin
      LChannel := LWetland.OutflowChannel;
      if(LChannel <> nil) then
         Result := LChannel.ChannelID;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TWetlandList.CopyCreate(AWetlandID : integer): IWetland;
const OPNAME = 'TWetlandList.CopyCreate';
var
  LSourceWetland : TWetland;
  LDestWetland   : TWetland;
begin
  Result := nil;
  try
    LSourceWetland := CastWetlandByNodeNumber(AWetlandID);
    if LSourceWetland <> nil then
    begin
      LDestWetland := CreateNewWetland;
      if LDestWetland <> nil then
      begin
        LDestWetland.Assign(LSourceWetland);
        Result := LDestWetland;
      end;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

end.



