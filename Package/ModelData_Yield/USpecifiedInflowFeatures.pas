{******************************************************************************}
{*  UNIT      : Contains the class TSpecifiedInflowFeature.                   *}
{*  AUTHOR    : Riana Steyn                                                   *}
{*  DATE      : 2004/03/10                                                    *}
{*  COPYRIGHT : Copyright © 2003 DWAF                                         *}
{******************************************************************************}

unit USpecifiedInflowFeatures;

interface

uses
  Classes,
  Contnrs,
  UAbstractObject,
  VoaimsCom_TLB;

type

{******************************************************************************}
{* SpecifiedInflow Feature                                                    *}
{******************************************************************************}

  TSpecifiedInflowFeature = class(TAbstractAppObject, ISpecifiedInflowFeature)
  protected
    FFeatureID                : integer;
    FFeatureName              : string;
    FChannelNr                : integer;
    FFeatureType              : integer;
    FFeatureSubType           : integer;
    FInflowFileName           : string;

    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    function ValidateFeatureName(AErrorMessages : TStrings) : WordBool;
    function ValidateInflowFileName(AErrorMessages : TStrings) : WordBool;
    function GetInflowFileRecordCountDB : integer;
  public
    procedure Assign(AChannelNumber : integer; ASpecifiedInflowFeature:TSpecifiedInflowFeature);
    function Initialise : boolean; override;
    function Populate (AFeatureID          : integer;
                       AFeatureName        : WideString;
                       AInflowFileName     : WideString;
                       AChannelNr          : integer;
                       AFeatureType        : integer;
                       AFeatureSubType     : integer) : WordBool;
    function Get_FeatureID : integer; safecall;
    function Get_FeatureName : WideString; safecall;
    procedure Set_FeatureName (const AName : WideString); safecall;
    function Get_Channel : IGeneralFlowChannel; safecall;
    procedure Set_Channel (const AChannel : IGeneralFlowChannel); safecall;
    function Get_FeatureType : integer; safecall;
    procedure Set_FeatureType (AType : integer); safecall;
    function Get_FeatureSubType : integer; safecall;
    procedure Set_FeatureSubType (ASubType : integer); safecall;
    function Get_InflowFileName : WideString; safecall;
    procedure Set_InflowFileName (const AFileName : WideString); safecall;
    function Get_InflowNodeInflowFilesCommaText : WideString; safecall;
    function Validate (var AErrors : WideString; const AContext    : WideString='') : WordBool; safecall;
    function GetKeyValues (const AParamField : WideString; const AFieldIndex: WideString) : WideString; safecall;
    property FeatureID      : integer read Get_FeatureID;
    property FeatureName    : WideString read Get_FeatureName write Set_FeatureName;
    property Channel        : IGeneralFlowChannel read Get_Channel write Set_Channel;
    property FeatureType    : integer read Get_FeatureType write Set_FeatureType;
    property FeatureSubType : integer read Get_FeatureSubType write Set_FeatureSubType;
    property InflowFileName    : WideString read Get_InflowFileName write Set_InflowFileName;
    property InflowNodeInflowFilesCommaText : WideString read Get_InflowNodeInflowFilesCommaText;
  end;

  TSpecifiedInflowFeatureList = class(TAbstractAppObject, ISpecifiedInflowFeatureList)
  protected
    FSpecifiedInflowFeatureList : TList;

    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    function AddSpecifiedInflowFeature (AFeature : TSpecifiedInflowFeature): boolean;
  public
    function Initialise : boolean; override;
    function NewSpecifiedInflowFeature : TSpecifiedInflowFeature;
    function CastSpecifiedInflowFeatureByIndex (AIndex : integer): TSpecifiedInflowFeature;
    function CastSpecifiedInflowFeatureByID(AFeatureID : integer): TSpecifiedInflowFeature;
    function CastSpecifiedInflowFeatureByChannelNr(AChannelNr : integer): TSpecifiedInflowFeature;

    function DeleteSpecifiedInflowFeatureWithID(AFeatureID : integer) : WordBool;
    function DeleteSpecifiedInflowFeatureWithIndex(AIndex : integer) : WordBool;
    function CreateNewSpecifiedInflowFeature : TSpecifiedInflowFeature;
    function CopySpecifiedInflowFeature(ANewChannelNumber : integer;AOldChannelNumber : integer) : ISpecifiedInflowFeature; safecall;
    function CreateSpecifiedInflowFeature : ISpecifiedInflowFeature; safecall;
    function RemoveSpecifiedInflowFeatureWithID (AFeatureID : integer) : WordBool; safecall;
    function Get_SpecifiedInflowFeatureByID(AFeatureID : integer): ISpecifiedInflowFeature; safecall;
    function Get_SpecifiedInflowFeatureByIndex(AIndex: integer): ISpecifiedInflowFeature; safecall;
    function Get_SpecifiedInflowFeatureCount : integer; safecall;
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; safecall;

    property SpecifiedInflowFeatureByIndex[AIndex : integer]: ISpecifiedInflowFeature
      read Get_SpecifiedInflowFeatureByIndex;
    property SpecifiedInflowFeatureByID[AFeatureID: integer]: ISpecifiedInflowFeature
      read Get_SpecifiedInflowFeatureByID;
    property SpecifiedInflowFeatureCount: integer read Get_SpecifiedInflowFeatureCount;
  end;

implementation

uses
  System.UITypes,
  SysUtils,
  Math,
  UConstants,
  UParameterData,
  UUtilities,
  VCL.Dialogs,
  VCL.Controls,
  UMainMenuEventType,
  URunConfigurationDataSQLAgent,
  UNetworkFeaturesSQLAgent,
  UYieldModelDataObject,
  UErrorHandlingOperations,
  UAbstractFileNamesObject,
  UAbstractModelData;

{******************************************************************************}
{ TDiversionFeature                                                            }
{******************************************************************************}

function TSpecifiedInflowFeature._AddRef: Integer;
const OPNAME = 'TSpecifiedInflowFeature._AddRef';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TSpecifiedInflowFeature._Release: Integer;
const OPNAME = 'TSpecifiedInflowFeature._Release';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TSpecifiedInflowFeature.CreateMemberObjects;
const OPNAME = 'TSpecifiedInflowFeature.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TSpecifiedInflowFeature.DestroyMemberObjects;
const OPNAME = 'TSpecifiedInflowFeature.DestroyMemberObjects';
begin
  try
    inherited DestroyMemberObjects;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TSpecifiedInflowFeature.Initialise: boolean;
const OPNAME = 'TSpecifiedInflowFeature.Initialise';
begin
  Result := inherited Initialise;
  try
    FChannelNr                := 0;
    FFeatureID                := -1;
    FFeatureName              := '';
    FFeatureType              := -1;
    FFeatureSubType           := -1;
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TSpecifiedInflowFeature.Populate (AFeatureID          : integer;
                                           AFeatureName        : WideString;
                                           AInflowFileName     : WideString;
                                           AChannelNr          : integer;
                                           AFeatureType        : integer;
                                           AFeatureSubType     : integer) : WordBool;
const OPNAME = 'TSpecifiedInflowFeature.Populate';
begin
  Result := FALSE;
  try
    FFeatureID                := AFeatureID;
    FFeatureName              := AFeatureName;
    FInflowFileName           := AInflowFileName;
    FChannelNr                := AChannelNr;
    FFeatureType              := AFeatureType;
    FFeatureSubType           := AFeatureSubType;
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TSpecifiedInflowFeature.Get_FeatureID : integer;
const OPNAME = 'TSpecifiedInflowFeature.Get_FeatureID';
begin
  Result := 0;
  try
    Result := FFeatureID;
  except on E : Exception do HandleError(E, OPNAME); end;
end;

function TSpecifiedInflowFeature.Get_FeatureName : WideString;
const OPNAME = 'TSpecifiedInflowFeature.Get_FeatureName';
begin
  Result := '';
  try
    Result := FFeatureName;
  except on E : Exception do HandleError(E, OPNAME); end;
end;

function TSpecifiedInflowFeature.Get_Channel : IGeneralFlowChannel;
const OPNAME = 'TSpecifiedInflowFeature.Get_Channel';
begin
  Result := nil;
  try
    Result := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.ChannelList.
              ChannelByChannelNumber[FChannelNr];
  except on E : Exception do HandleError(E, OPNAME); end;
end;

function TSpecifiedInflowFeature.Get_FeatureType : integer;
const OPNAME = 'TSpecifiedInflowFeature.Get_FeatureType';
begin
  Result := 0;
  try
    Result := FFeatureType;
  except on E : Exception do HandleError(E, OPNAME); end;
end;

function TSpecifiedInflowFeature.Get_FeatureSubType : integer;
const OPNAME = 'TSpecifiedInflowFeature.Get_FeatureSubType';
begin
  Result := 0;
  try
    Result := FFeatureSubType;
  except on E : Exception do HandleError(E, OPNAME); end;
end;

procedure TSpecifiedInflowFeature.Set_FeatureType (AType : integer);
const OPNAME = 'TSpecifiedInflowFeature.Set_FeatureType';
var
  LLoadAgent   : TNetworkFeaturesSQLAgent;
  LContextData : TStringList;
  LOldValue: integer;
begin
  try
    LLoadAgent := TNetworkFeaturesSQLAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
      {RHS comeback = no DB field to store type of feature}
{
        LLoadAgent.LoadChannelPenaltyContextData(LContextData,
          IntToStr(FPenaltyStructType), IntToStr(FChannelPenaltyArcNumber));
        if FAppModules.FieldProperties.UpdateFieldValue(
             'Penalty', FloatToStr(APenaltyValue), FloatToStr(FPenaltyValue), LContextData) then
        begin
}         LOldValue := FFeatureType;
          FFeatureType := AType;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'FeatureType',IntToStr(LOldValue),IntToStr(AType));
//        end;
      finally
        LContextData.Free;
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TSpecifiedInflowFeature.Set_FeatureSubType (ASubType : integer);
const OPNAME = 'TSpecifiedInflowFeature.Set_FeatureSubType';
var
  LLoadAgent   : TNetworkFeaturesSQLAgent;
  LContextData : TStringList;
  LOldValue: integer;
begin
  try
    LLoadAgent := TNetworkFeaturesSQLAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
      {RHS comeback = no DB field to store subtype of feature}
{
        LLoadAgent.LoadChannelPenaltyContextData(LContextData,
          IntToStr(FPenaltyStructType), IntToStr(FChannelPenaltyArcNumber));
        if FAppModules.FieldProperties.UpdateFieldValue(
             'Penalty', FloatToStr(APenaltyValue), FloatToStr(FPenaltyValue), LContextData) then
        begin
}         LOldValue := FFeatureSubType;
          FFeatureSubType := ASubType;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'FeatureSubType',IntToStr(LOldValue),IntToStr(ASubType));
//        end;
      finally
        LContextData.Free;
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TSpecifiedInflowFeature.Set_FeatureName (const AName : WideString);
const OPNAME = 'TSpecifiedInflowFeature.Set_FeatureName';
var
  LLoadAgent   : TNetworkFeaturesSQLAgent;
  LContextData : TStringList;
  LOldValue: string;
begin
  try
    LLoadAgent := TNetworkFeaturesSQLAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        LLoadAgent.LoadContextData_FeatureID(LContextData, IntToStr(FFeatureID));
        if FAppModules.FieldProperties.UpdateFieldValue(
           'SpecifiedInflowFeatureName', AName, FFeatureName, LContextData) then
        begin
          LOldValue := FFeatureName;
          FFeatureName := AName;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'SpecifiedInflowFeatureName',LOldValue,AName);
        end;
      finally
        LContextData.Free;
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TSpecifiedInflowFeature.Set_Channel (const AChannel : IGeneralFlowChannel);
const OPNAME = 'TSpecifiedInflowFeature.Set_Channel';
var
  LLoadAgent   : TNetworkFeaturesSQLAgent;
  LContextData : TStringList;
  LOldValue    : string;
  LNewValue    : string;
begin
  try
    LLoadAgent := TNetworkFeaturesSQLAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        LOldValue := IntToStr(FChannelNr);
        if (AChannel <> nil) then
          LNewValue := IntToStr(AChannel.ChannelNumber)
        else
          LNewValue := '0';
        LLoadAgent.LoadContextData_FeatureID(LContextData, IntToStr(FFeatureID));
        if FAppModules.FieldProperties.UpdateFieldValue(
           'SpecifiedInflowFeatureChannelNumber', LNewValue, LOldValue, LContextData) then
        begin
          FChannelNr := AChannel.ChannelNumber;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'SpecifiedInflowChannelNumber',LOldValue,lNewValue);
        end;
      finally
        LContextData.Free;
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TSpecifiedInflowFeature.GetKeyValues (const AParamField : WideString;
                                               const AFieldIndex : WideString) : WideString;
const OPNAME = 'TSpecifiedInflowFeature.GetKeyValues';
begin
  Result := '';
  try
    Result := 'Model='           + QuotedStr(FAppModules.StudyArea.ModelCode) +
              ',StudyAreaName='  + QuotedStr(FAppModules.StudyArea.StudyAreaCode) +
              ',SubArea='        + QuotedStr(FAppModules.StudyArea.SubAreaCode) +
              ',Scenario='       + QuotedStr(FAppModules.StudyArea.ScenarioCode) +
              ',Identifier='     + IntToStr(FFeatureID);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{******************************************************************************}
{ TSpecifiedInflowFeatureList                                                  }
{******************************************************************************}

function TSpecifiedInflowFeatureList._AddRef: Integer;
const OPNAME = 'TSpecifiedInflowFeatureList._AddRef';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TSpecifiedInflowFeatureList._Release: Integer;
const OPNAME = 'TSpecifiedInflowFeatureList._Release';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
 except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TSpecifiedInflowFeatureList.CreateMemberObjects;
const OPNAME = 'TSpecifiedInflowFeatureList.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
    FSpecifiedInflowFeatureList := TList.Create;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TSpecifiedInflowFeatureList.DestroyMemberObjects;
const OPNAME = 'TSpecifiedInflowFeatureList.DestroyMemberObjects';
begin
  try
    while (FSpecifiedInflowFeatureList.Count > 0) do
      DeleteSpecifiedInflowFeatureWithIndex(0);
    FreeAndNil(FSpecifiedInflowFeatureList);
    inherited DestroyMemberObjects;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TSpecifiedInflowFeatureList.Initialise: boolean;
const OPNAME = 'TSpecifiedInflowFeatureList.Initialise';
begin
  Result := inherited Initialise;
  try
    FSpecifiedInflowFeatureList.Clear;
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TSpecifiedInflowFeatureList.AddSpecifiedInflowFeature (AFeature : TSpecifiedInflowFeature): boolean;
const OPNAME = 'TSpecifiedInflowFeatureList.AddSpecifiedInflowFeature';
begin
  Result := False;
  try
    if (AFeature <> nil) then
    begin
      FSpecifiedInflowFeatureList.Add(AFeature);
      Result := TRUE;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TSpecifiedInflowFeatureList.NewSpecifiedInflowFeature : TSpecifiedInflowFeature;
const OPNAME = 'TSpecifiedInflowFeatureList.NewSpecifiedInflowFeature';
var
  lFeature : TSpecifiedInflowFeature;
begin
  Result := nil;
  try
    lFeature := TSpecifiedInflowFeature.Create(FAppModules);
    AddSpecifiedInflowFeature(lFeature);
    Result := lFeature;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TSpecifiedInflowFeatureList.CreateNewSpecifiedInflowFeature : TSpecifiedInflowFeature;
const OPNAME = 'TSpecifiedInflowFeatureList.CreateNewSpecifiedInflowFeature';
var
  LFeatureID  : integer;
  LLoadAgent  : TNetworkFeaturesSQLAgent;
  LFeature    : TSpecifiedInflowFeature;
begin
  Result := nil;
  try
    LLoadAgent := TNetworkFeaturesSQLAgent.Create(FAppModules);
    try
      LFeatureID := 0;
      if (LLoadAgent.InsertSpecifiedInflowFeature(LFeatureID)) then
      begin
        LFeature := NewSpecifiedInflowFeature;
        LFeature.Initialise;
        LFeature.Populate
          (LFeatureID,
           UpperCase(FAppModules.Language.GetString('NetworkFeatures.SpecifiedInflow')) + ' ' + IntToStr(LFeatureID),'',
           0, 10, 0);
        Result := LFeature;
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TSpecifiedInflowFeatureList.CreateSpecifiedInflowFeature : ISpecifiedInflowFeature;
const OPNAME = 'TSpecifiedInflowFeatureList.CreateSpecifiedInflowFeature';
var
  LFeature : ISpecifiedInflowFeature;
begin
  Result := nil;
  try
    lFeature := CreateNewSpecifiedInflowFeature;
    Result := lFeature;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TSpecifiedInflowFeatureList.RemoveSpecifiedInflowFeatureWithID (AFeatureID : integer) : WordBool;
const OPNAME = 'TSpecifiedInflowFeatureList.RemoveSpecifiedInflowFeatureWithID';
var
  lLoadAgent : TNetworkFeaturesSQLAgent;
begin
  Result := FALSE;
  try
    if (AFeatureID > 0) then
    begin
      LLoadAgent := TNetworkFeaturesSQLAgent.Create(FAppModules);
      try
        if LLoadAgent.DeleteSpecifiedInflowFeature(AFeatureID) then
        begin
          DeleteSpecifiedInflowFeatureWithID(AFeatureID);
          Result := TRUE;
        end;
      finally
        LLoadAgent.Free;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TSpecifiedInflowFeatureList.DeleteSpecifiedInflowFeatureWithID(AFeatureID : integer) : WordBool;
const OPNAME = 'TSpecifiedInflowFeatureList.DeleteSpecifiedInflowFeatureWithID';
var
  lFeature : TSpecifiedInflowFeature;
  lIndex   : integer;
begin
  Result := FALSE;
  try
    lFeature := CasTSpecifiedInflowFeatureByID(AFeatureID);
    if (lFeature <> nil) then
    begin
      lIndex := FSpecifiedInflowFeatureList.IndexOf(lFeature);
      Result := DeleteSpecifiedInflowFeatureWithIndex(lIndex);
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TSpecifiedInflowFeatureList.DeleteSpecifiedInflowFeatureWithIndex(AIndex : integer) : WordBool;
const OPNAME = 'TSpecifiedInflowFeatureList.DeleteSpecifiedInflowFeatureWithIndex';
var
  lFeature : TSpecifiedInflowFeature;
begin
  Result := FALSE;
  try
    if (AIndex >= 0) then
    begin
      lFeature := TSpecifiedInflowFeature(FSpecifiedInflowFeatureList.Items[AIndex]);
      FSpecifiedInflowFeatureList.Delete(AIndex);
      FreeAndNil(lFeature);
      Result := TRUE;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TSpecifiedInflowFeatureList.Get_SpecifiedInflowFeatureByIndex(AIndex: integer): ISpecifiedInflowFeature;
const OPNAME = 'TSpecifiedInflowFeatureList.Get_SpecifiedInflowFeatureByIndex';
begin
  Result := nil;
  try
    if (AIndex >= 0) and (AIndex < FSpecifiedInflowFeatureList.Count) then
      Result := TSpecifiedInflowFeature(FSpecifiedInflowFeatureList.Items[AIndex]);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TSpecifiedInflowFeatureList.Get_SpecifiedInflowFeatureByID(AFeatureID : integer): ISpecifiedInflowFeature;
const OPNAME = 'TSpecifiedInflowFeatureList.Get_SpecifiedInflowFeatureByID';
var
 lIndex   : integer;
 lFeature : TSpecifiedInflowFeature;
begin
  Result := nil;
  try
    lIndex := 0;
    while ((Result = nil) AND (lIndex < FSpecifiedInflowFeatureList.Count)) do
    begin
      lFeature := TSpecifiedInflowFeature(FSpecifiedInflowFeatureList.Items[lIndex]);
      if (lFeature.FeatureID = AFeatureID) then
        Result := lFeature
      else
        lIndex := lIndex + 1;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TSpecifiedInflowFeatureList.CastSpecifiedInflowFeatureByID(AFeatureID : integer): TSpecifiedInflowFeature;
const OPNAME = 'TSpecifiedInflowFeatureList.CastSpecifiedInflowFeatureByID';
var
 lIndex   : integer;
 lFeature : TSpecifiedInflowFeature;
begin
  Result := nil;
  try
    lIndex := 0;
    while ((Result = nil) AND (lIndex < FSpecifiedInflowFeatureList.Count)) do
    begin
      lFeature := TSpecifiedInflowFeature(FSpecifiedInflowFeatureList.Items[lIndex]);
      if (lFeature.FeatureID = AFeatureID) then
        Result := lFeature
      else
        lIndex := lIndex + 1;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TSpecifiedInflowFeatureList.CastSpecifiedInflowFeatureByChannelNr(AChannelNr : integer): TSpecifiedInflowFeature;
const OPNAME = 'TSpecifiedInflowFeatureList.CastSpecifiedInflowFeatureByChannelNr';
var
 lIndex   : integer;
 lFeature : TSpecifiedInflowFeature;
begin
  Result := nil;
  try
    lIndex := 0;
    while ((Result = nil) AND (lIndex < FSpecifiedInflowFeatureList.Count)) do
    begin
      lFeature := TSpecifiedInflowFeature(FSpecifiedInflowFeatureList.Items[lIndex]);
      if (lFeature.FeatureID = AChannelNr) then
        Result := lFeature
      else
        lIndex := lIndex + 1;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;


function TSpecifiedInflowFeatureList.CastSpecifiedInflowFeatureByIndex (AIndex: integer): TSpecifiedInflowFeature;
const OPNAME = 'TSpecifiedInflowFeatureList.CastSpecifiedInflowFeatureByIndex';
begin
  Result := nil;
  try
    if (AIndex >= 0) and (AIndex < FSpecifiedInflowFeatureList.Count) then
      Result := TSpecifiedInflowFeature(FSpecifiedInflowFeatureList.Items[AIndex]);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TSpecifiedInflowFeatureList.Get_SpecifiedInflowFeatureCount: integer;
const OPNAME = 'TSpecifiedInflowFeatureList.Get_SpecifiedInflowFeatureCount';
begin
  Result := 0;
  try
    Result := FSpecifiedInflowFeatureList.Count;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TSpecifiedInflowFeature.ValidateFeatureName(AErrorMessages: TStrings): WordBool;
const OPNAME = 'TSpecifiedInflowFeature.ValidateFeatureName';
var
  lMessage     : String;
  lUnique      : boolean;
  lIndex       : integer;
  lFeatureList : TSpecifiedInflowFeatureList;
  lFeature     : TSpecifiedInflowFeature;
begin
  Result := True;
  try
    lMessage := '';
    if (NOT FAppModules.FieldProperties.ValidateFieldProperty('SpecifiedInflowFeatureName', FFeatureName, lMessage)) then
      AErrorMessages.Add('WARNING:' +lMessage)
    else
    begin
      lFeatureList := TYieldModelDataObject(FAppModules.Model.ModelData).
                      CastNetworkFeaturesData.CastSpecifiedInflowDataList;
      lUnique := TRUE;
      lIndex  := 0;
      while (lUnique AND (lIndex < lFeatureList.SpecifiedInflowFeatureCount)) do
      begin
        lFeature := lFeatureList.CastSpecifiedInflowFeatureByIndex(lIndex);
        if ((FFeatureID <> lFeature.FFeatureID) AND
            (UpperCase(trim(lFeature.FFeatureName)) = UpperCase(trim(FFeatureName)))) then
        begin
          lMessage := FAppModules.Language.GetString('ContextValidation.DuplicateSpecifiedInflowFeatureName');
          AErrorMessages.Add('WARNING:' +Format(lMessage, [IntToStr(Channel.ChannelNumber)]));
          lUnique := FALSE;
        end
        else
          lIndex := lIndex + 1;
      end;
      //result := lUnique;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TSpecifiedInflowFeature.Validate (var AErrors : WideString; const AContext    : WideString='') : WordBool;
const OPNAME = 'TSpecifiedInflowFeature.Validate';
var
  lErrorList        : TStringList;
begin
  Result := FALSE;
  try
    Result := TRUE;
    lErrorList := TStringList.Create;
    try
      if (AContext = 'SpecifiedInflowFeatureName') then
        Result := ValidateFeatureName(lErrorList)
      else
      if (AContext = 'InflowFileName') then
        Result := ValidateInflowFileName(lErrorList)
      else
      begin
        if (NOT ValidateFeatureName(lErrorList)) then
          Result := FALSE;
        if (NOT ValidateInflowFileName(lErrorList)) then
          Result := FALSE;
      end;
      AErrors := AErrors + lErrorList.Text;
    finally
      FreeAndNil(lErrorList);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TSpecifiedInflowFeatureList.Validate(var AErrors: WideString; const AContext: WideString): WordBool;
const OPNAME = 'TSpecifiedInflowFeatureList.Validate';
var
  lStopOnFirstError : Boolean;
  LIndex            : integer;
begin
  Result := FALSE;
  try
    Result := TRUE;
    lStopOnFirstError := FAppModules.GlobalData.StopOnFirstErr;
    for LIndex := 0 to FSpecifiedInflowFeatureList.Count - 1 do
    begin
      if (NOT SpecifiedInflowFeatureByIndex[LIndex].Validate(AErrors,AContext)) then
        Result := FALSE;
      if ((NOT Result) AND lStopOnFirstError) then
        Break;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TSpecifiedInflowFeature.Get_InflowFileName: WideString;
const OPNAME = 'TSpecifiedInflowFeature.Get_InflowFileName';
var
  LPath: string;
begin
  Result := '';
  try
    if (Trim(FInflowFileName) <> '') then
    begin
      LPath := Trim(TYieldModelDataObject(FAppModules.Model.ModelData).CastFileNamesObject.HydrologyFilesPath);
      Result := LPath + ExtractFileName(FInflowFileName);
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TSpecifiedInflowFeature.Set_InflowFileName(const AFileName: WideString);
const OPNAME = 'TSpecifiedInflowFeature.Set_InflowFileName';
var
  LLoadAgent    : TNetworkFeaturesSQLAgent;
  LContextData  : TStringList;
  LOldValue,
  LMesg         : string;
  LFileName     : TAbstractModelFileName;
  LNewFileIndex : integer;
begin
  try
    LLoadAgent   := TNetworkFeaturesSQLAgent.Create(FAppModules);
    LContextData := TStringList.Create;
    try
      LLoadAgent.LoadContextData_FeatureID(LContextData, IntToStr(FFeatureID));
      if FAppModules.FieldProperties.UpdateFieldValue(
         'InflowFileName', AFilename, FInflowFileName, LContextData) then
      begin
        LNewFileIndex := TYieldModelDataObject(FAppModules.Model.ModelData).CastFileNamesObject.HydrologyFileNames.Count + 1;
        LFileName     := TYieldModelDataObject(FAppModules.Model.ModelData).CastFileNamesObject.HydrologyFileNames.FindFile(AFileName);
        if(LFileName = nil) then
          TYieldModelDataObject(FAppModules.Model.ModelData).CastFileNamesObject.AddHydrologyFileName(LNewFileIndex,AFileName,False,0.0,
            FileLastWriteDate(AFileName));
        LFileName := TYieldModelDataObject(FAppModules.Model.ModelData).CastFileNamesObject.HydrologyFileNames.FindFile(AFileName);
        if (LFileName <> nil) and (not LFileName.SavedInDB) then
        begin
          LMesg := FAppModules.Language.GetString('SelectedFile.ImportFile');
          if (MessageDlg(LMesg,mtConfirmation,[mbYes, mbNo],0) = mrYes) then
            FAppModules.Model.ProcessEvent(CmeImportHydrologyFile,LFileName);
        end;
        LOldValue       := FInflowFileName;
        FInflowFileName := AFilename;
        FAppModules.Model.StudyDataHasChanged(sdccEdit,'InflowFileName',LOldValue,AFilename);
      end;
    finally
      LLoadAgent.Free;
      LContextData.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

{function TSpecifiedInflowFeature.Get_InflowNodeInflowFilesCommaText: WideString;
const OPNAME = 'TSpecifiedInflowFeature.Get_InflowNodeInflowFilesCommaText';
var
  LIndex,
  lNodeNumber     : integer;
  lFileName       : string;
  lNode           : IReservoirData;
  lCatchRefList   : TObjectList;
  lCatchmentRef   : TParamReference;
  lFileNamesContainer : TStringList;
begin
  Result := '';
  try
    if (Channel <> nil) then
    begin
      lNodeNumber := Channel.UpStreamNodeNumber;
      lNode := TYieldModelDataObject(FAppModules.Model.ModelData).
                 NetworkElementData.ReservoirList.ReservoirOrNodeByIdentifier[lNodeNumber];
      if (lNode = nil) or ((lNode <> nil) and (lNode.ReservoirConfigurationData.CatchmentRef = 0))then
      begin
        lNodeNumber := Channel.DownStreamNodeNumber;
        lNode := TYieldModelDataObject(FAppModules.Model.ModelData).
                   NetworkElementData.ReservoirList.ReservoirOrNodeByIdentifier[lNodeNumber];
      end;

      if ((lNode <> nil) AND
          (lNode.ReservoirConfigurationData <> nil) AND
          (not (lNode.ReservoirConfigurationData.NodeType in [ntNodeWithoutInflow,ntIrrigationNode,ntDemandCentreNode,ntMineNode,ntGroundWater]))) then
      begin
        lCatchRefList := TYieldModelDataObject(FAppModules.Model.ModelData).
                           CastCastParameterData.AllReferenceData;
        if (lCatchRefList <> nil) then
        begin
          if (lNode.ReservoirConfigurationData.CatchmentRef > 0) and
              (lNode.ReservoirConfigurationData.CatchmentRef < lCatchRefList.Count)  then
          begin
            lCatchmentRef := TParamReference(lCatchRefList.Items[lNode.ReservoirConfigurationData.CatchmentRef - 1]);
            lFileName     := lCatchmentRef.FileReference;
            lFileNamesContainer := TStringList.Create;
            try
              TYieldModelDataObject(FAppModules.Model.ModelData).CastFileNamesObject.CastHydrologyFileNames.
                FindFilesFromPrefix('','.inf',lFileNamesContainer);
              for LIndex := 0 to lFileNamesContainer.Count - 1 do
                lFileNamesContainer[LIndex] := ExtractFileName(lFileNamesContainer[LIndex]);
              Result :=  lFileNamesContainer.CommaText;
            finally
              lFileNamesContainer.Free;
            end;
          end;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;}

function TSpecifiedInflowFeature.ValidateInflowFileName(AErrorMessages: TStrings): WordBool;
const OPNAME = 'TSpecifiedInflowFeature.ValidateInflowFileName';
var
  lMessage     : String;
  lStochasticYearsCount,
  lYearsCount  : integer;
  lRunConfigurationData:IRunConfigurationData;
begin
  Result := True;
  try
    lMessage := '';
    if (NOT FAppModules.FieldProperties.ValidateFieldProperty('InflowFileName', FInflowFileName, lMessage)) then
      AErrorMessages.Add('WARNING:' +lMessage)
    else
    begin
      lYearsCount := GetInflowFileRecordCountDB;
      //if(lYearsCount > 0) then
      //   lYearsCount := lYearsCount - 1;
      lRunConfigurationData := TYieldModelDataObject(FAppModules.Model.ModelData).RunConfigurationData;
      if(lRunConfigurationData.RunSequenceType = 'H') then
      begin
        if (lRunConfigurationData.YearsInAnalysis > lYearsCount) then
        begin
          Result := FALSE;
          lMessage := FAppModules.Language.GetString('Message.YearsInAnalysisIsGeater')+
                      FAppModules.Language.GetString('Message.InflowSequence') + IntToStr(lYearsCount) + ' .';
          AErrorMessages.Add(lMessage);
        end
        else
        if (lRunConfigurationData.YearsInAnalysis < lYearsCount) then
        begin
          Result := FALSE;
          lMessage := FAppModules.Language.GetString('Message.YearsInAnalysisIsLess') +
                      FAppModules.Language.GetString('Message.InflowSequence') + IntToStr(lYearsCount) + ' .';
          AErrorMessages.Add(lMessage);
        end;
      end;

      if(TYieldModelDataObject(FAppModules.Model.ModelData).RunConfigurationData.RunSequenceType = 'S') then
      begin
        lStochasticYearsCount := lRunConfigurationData.YearsInAnalysis;// * lRunConfigurationData.NumberOfSequencesInAnalysis;
        if (lStochasticYearsCount > lYearsCount) then
        begin
          Result := FALSE;
          lMessage := FAppModules.Language.GetString('Message.YearsInAnalysisIsGeater')+
                      FAppModules.Language.GetString('Message.InflowSequence') + IntToStr(lYearsCount) + ' .';
          AErrorMessages.Add(lMessage);
        end
        else
        if (lStochasticYearsCount < lYearsCount) then
        begin
          Result := FALSE;
          lMessage := FAppModules.Language.GetString('Message.YearsInAnalysisIsLess') +
                      FAppModules.Language.GetString('Message.InflowSequence') + IntToStr(lYearsCount) + ' .';
          AErrorMessages.Add(lMessage);
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TSpecifiedInflowFeature.GetInflowFileRecordCountDB: integer;
const OPNAME = 'TSpecifiedInflowFeature.GetInflowFileRecordCountDB';
var
  LSQLAgent: TRunConfigurationDataSQLAgent;
begin
  Result := 0;
  try
    LSQLAgent := TRunConfigurationDataSQLAgent.Create(FAppModules);
    try
      Result := LSQLAgent.GetInflowFileRecordCount(ExtractFileName(FInflowFileName));
    finally
      LSQLAgent.Free;
    end;
  except on E : Exception do HandleError(E, OPNAME); end;
end;

function TSpecifiedInflowFeatureList.CopySpecifiedInflowFeature(ANewChannelNumber, AOldChannelNumber: integer): ISpecifiedInflowFeature;
const OPNAME = 'TSpecifiedInflowFeatureList.CopySpecifiedInflowFeature';
var
  LSpecifiedInflowFeature : TSpecifiedInflowFeature;
  LSpecifiedInflowFeatureCopy : TSpecifiedInflowFeature;
begin
  Result := nil;
  try
    LSpecifiedInflowFeature := CastSpecifiedInflowFeatureByChannelNr(AOldChannelNumber);
    if (LSpecifiedInflowFeature <> nil) then
    begin
      LSpecifiedInflowFeatureCopy := CreateNewSpecifiedInflowFeature;
      if LSpecifiedInflowFeatureCopy <> nil then
      begin
        LSpecifiedInflowFeatureCopy.Assign(ANewChannelNumber,LSpecifiedInflowFeature);
        Result := LSpecifiedInflowFeatureCopy;
      end;
    end;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TSpecifiedInflowFeature.Assign(AChannelNumber: integer;ASpecifiedInflowFeature: TSpecifiedInflowFeature);
const OPNAME = 'TSpecifiedInflowFeature.Assign';
var
  LChannel : IGeneralFlowChannel;
begin
  try
    if (ASpecifiedInflowFeature <> nil) and (AChannelNumber > 0) then
    begin
      LChannel := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.ChannelList.ChannelByChannelNumber[AChannelNumber];
      if LChannel <> nil then
        Channel := LChannel;
      FeatureName := 'Copy of '+ASpecifiedInflowFeature.FeatureName;
      FeatureType := ASpecifiedInflowFeature.FeatureType;
      FeatureSubType := ASpecifiedInflowFeature.FeatureSubType;
      InflowFileName :=ASpecifiedInflowFeature.InflowFileName;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TSpecifiedInflowFeature.Get_InflowNodeInflowFilesCommaText: WideString;
const OPNAME = 'TSpecifiedInflowFeature.Get_InflowNodeInflowFilesCommaText';
begin
  Result := '';
  try
    if(FInflowFileName <> '') then
      Result := TYieldModelDataObject(FAppModules.Model.ModelData).HydrologyFileData[FInflowFileName];
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

end.
