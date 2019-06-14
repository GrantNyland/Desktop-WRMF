{******************************************************************************}
{*  UNIT      : Contains the class TSpecifiedDemandFeature.                   *}
{*  AUTHOR    : Riana Steyn                                                   *}
{*  DATE      : 2004/03/29                                                    *}
{*  COPYRIGHT : Copyright © 2003 DWAF                                         *}
{******************************************************************************}

unit USpecifiedDemandFeatures;

interface

uses
  Classes,
  Contnrs,
  UAbstractObject,
  VoaimsCom_TLB;

type

{******************************************************************************}
{* Demand Features                                                            *}
{******************************************************************************}

  TSpecifiedDemandFeature = class(TAbstractAppObject, ISpecifiedDemandFeature)
  protected
    FFeatureID                : integer;
    FFeatureName              : string;
    FChannelNr                : integer;
    FFeatureType              : integer;
    FFeatureSubType           : integer;
    FSpecifiedDemandFileName  : string;
    FCatchmentRefNumber       : integer;
    FStochasticIndicator      : string;
    FDemandFileData           : TStringList;

    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    function ValidateFeatureName (AErrorMessages : TStrings) : WordBool;
    function ValidateStochasticIndicator (AErrorMessages : TStrings) : WordBool;
    function ValidateCatchmentRefNumber (AErrorMessages : TStrings) : WordBool;
    function ValidateFileName (AErrorMessages : TStrings) : WordBool;
    procedure LoadDemandFileData;
  public
    procedure Assign(AChannelNumber : integer; ASpecifiedDemandFeature : TSpecifiedDemandFeature);
    function Initialise : boolean; override;
    function Populate (AFeatureID           : integer;
                       AFeatureName         : WideString;
                       AChannelNr           : integer;
                       AFeatureType         : integer;
                       AFeatureSubType      : integer;
                       ACatchmentRefNumber  : integer;
                       AStochasticIndicator : WideString;
                       ADemandFileName      : WideString): WordBool;
    function Get_FeatureID : integer; safecall;
    function Get_FeatureName : WideString; safecall;
    procedure Set_FeatureName (const AName : WideString); safecall;
    function Get_Channel : IGeneralFlowChannel; safecall;
    procedure Set_Channel (const AChannel : IGeneralFlowChannel); safecall;
    function Get_FeatureType : integer; safecall;
    procedure Set_FeatureType (AType : integer); safecall;
    function Get_FeatureSubType : integer; safecall;
    procedure Set_FeatureSubType (ASubType : integer); safecall;
    function Get_SpecifiedDemandFileName : WideString; safecall;
    procedure Set_SpecifiedDemandFileName(const ADemandFileName : WideString); safecall;
    function Get_CatchmentRefNumber: integer; safecall;
    procedure Set_CatchmentRefNumber(ACatchmentRefNumber : integer); safecall;
    function Get_StochasticIndicator: WideString; safecall;
    procedure Set_StochasticIndicator(const AStochasticIndicator : WideString); safecall;
    function GetMonthlyDemand (ATimeStep   : integer;
                               var ADemand : WideString): WordBool; safecall;
    function GetAnnualDemand (AYear       : Integer;
                              var ADemand : WideString): WordBool; safecall;
    function Validate(var AErrors: WideString; const AContext: WideString = ''): WordBool; safecall;
    function GetKeyValues (const AParamField : WideString; const AFieldIndex: WideString) : WideString; safecall;

    property FeatureID      : integer read Get_FeatureID;
    property FeatureName    : WideString read Get_FeatureName write Set_FeatureName;
    property Channel        : IGeneralFlowChannel read Get_Channel write Set_Channel;
    property FeatureType    : integer read Get_FeatureType write Set_FeatureType;
    property FeatureSubType : integer read Get_FeatureSubType write Set_FeatureSubType;
    property CatchmentRefNumber: integer  read Get_CatchmentRefNumber write Set_CatchmentRefNumber;
    property StochasticIndicator: WideString  read Get_StochasticIndicator write Set_StochasticIndicator;
    property SpecifiedDemandFileName: WideString  read Get_SpecifiedDemandFileName write Set_SpecifiedDemandFileName;
  end;

  TSpecifiedDemandFeatureList = class(TAbstractAppObject, ISpecifiedDemandFeatureList)
  protected
    FSpecifiedDemandFeatureList : TList;

    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    function AddDemandFeature (AFeature : TSpecifiedDemandFeature): boolean;
  public
    function Initialise : boolean; override;
    function NewSpecifiedDemandFeature : TSpecifiedDemandFeature;
    function CreateNewSpecifiedDemandFeature : TSpecifiedDemandFeature;
    function DeleteSpecifiedDemandFeatureWithID(AFeatureID : integer) : WordBool;
    function DeleteSpecifiedDemandFeatureWithIndex(AIndex : integer) : WordBool;
    function CastSpecifiedDemandFeatureByIndex (AIndex : integer): TSpecifiedDemandFeature;
    function CastSpecifiedDemandFeatureByID(AFeatureID : integer): TSpecifiedDemandFeature;
    function CastSpecifiedDemandFeatureByChannelNr(AChannelNr : integer): TSpecifiedDemandFeature;

    function CopySpecifiedDemandFeature(ANewChannelNumber, AOldChannelNumber: Integer): ISpecifiedDemandFeature; safecall;
    function CreateSpecifiedDemandFeature : ISpecifiedDemandFeature; safecall;
    function RemoveSpecifiedDemandFeatureWithID (AFeatureID : integer) : WordBool; safecall;
    function Get_SpecifiedDemandFeatureByIndex(AIndex: integer): ISpecifiedDemandFeature; safecall;
    function Get_SpecifiedDemandFeatureByID(AFeatureID : integer): ISpecifiedDemandFeature; safecall;
    function Get_SpecifiedDemandFeatureCount : integer; safecall;
    function Validate(var AErrors: WideString; const AContext: WideString = ''): WordBool; safecall;

    property SpecifiedDemandFeatureByIndex[AIndex : integer]: ISpecifiedDemandFeature
      read Get_SpecifiedDemandFeatureByIndex;
    property SpecifiedDemandFeatureByID[AFeatureID: integer]: ISpecifiedDemandFeature
      read Get_SpecifiedDemandFeatureByID;
    property SpecifiedDemandFeatureCount: integer read Get_SpecifiedDemandFeatureCount;
  end;

implementation

uses
  System.UITypes,
  SysUtils,
  UUtilities,
  Math,
  VCL.Controls,
  VCL.Dialogs,
  UConstants,
  UFileNames,
  UDataSetType,
  UMainMenuEventType,
  UAbstractFileNamesObject,
  UChannelDataSQLAgent,
  UYieldModelDataObject,
  UErrorHandlingOperations;

{******************************************************************************}
{ TSpecifiedDemandFeature                                                      }
{******************************************************************************}

function TSpecifiedDemandFeature._AddRef: Integer;
const OPNAME = 'TSpecifiedDemandFeature._AddRef';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TSpecifiedDemandFeature._Release: Integer;
const OPNAME = 'TSpecifiedDemandFeature._Release';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TSpecifiedDemandFeature.CreateMemberObjects;
const OPNAME = 'TSpecifiedDemandFeature.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
    FDemandFileData := TStringList.Create;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TSpecifiedDemandFeature.DestroyMemberObjects;
const OPNAME = 'TSpecifiedDemandFeature.DestroyMemberObjects';
begin
  try
    FreeAndNil(FDemandFileData);
    inherited DestroyMemberObjects;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TSpecifiedDemandFeature.Initialise: boolean;
const OPNAME = 'TSpecifiedDemandFeature.Initialise';
begin
  Result := inherited Initialise;
  try
    FChannelNr            := 0;
    FFeatureID            := -1;
    FFeatureName          := '';
    FFeatureType          := -1;
    FFeatureSubType       := -1;
    FCatchmentRefNumber   := NullInteger;
    FStochasticIndicator  := '';
    FSpecifiedDemandFileName := '';
    FDemandFileData.Clear;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TSpecifiedDemandFeature.Get_FeatureID : integer;
const OPNAME = 'TSpecifiedDemandFeature.Get_FeatureID';
begin
  Result := 0;
  try
    Result := FFeatureID;
  except on E : Exception do HandleError(E, OPNAME); end;
end;

function TSpecifiedDemandFeature.Get_FeatureName : WideString;
const OPNAME = 'TSpecifiedDemandFeature.Get_FeatureName';
begin
  Result := '';
  try
    Result := FFeatureName;
  except on E : Exception do HandleError(E, OPNAME); end;
end;

function TSpecifiedDemandFeature.Get_Channel : IGeneralFlowChannel;
const OPNAME = 'TSpecifiedDemandFeature.Get_Channel';
begin
  Result := nil;
  try
    Result := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.ChannelList.
              ChannelByChannelNumber[FChannelNr];
  except on E : Exception do HandleError(E, OPNAME); end;
end;

function TSpecifiedDemandFeature.Get_FeatureType : integer;
const OPNAME = 'TSpecifiedDemandFeature.Get_FeatureType';
begin
  Result := 0;
  try
    Result := FFeatureType;
  except on E : Exception do HandleError(E, OPNAME); end;
end;

function TSpecifiedDemandFeature.Get_FeatureSubType : integer;
const OPNAME = 'TSpecifiedDemandFeature.Get_FeatureSubType';
begin
  Result := 0;
  try
    Result := FFeatureSubType;
  except on E : Exception do HandleError(E, OPNAME); end;
end;

function TSpecifiedDemandFeature.Get_SpecifiedDemandFileName: WideString;
const OPNAME = 'TSpecifiedDemandFeature.Get_SpecifiedDemandFileName';
var
  LPath: string;
begin
  Result := '';
  try
    if (Trim(FSpecifiedDemandFileName) <> '') then
    begin
      LPath := Trim(TYieldModelDataObject(FAppModules.Model.ModelData).CastFileNamesObject.DemandFilesPath);
      Result := LPath + ExtractFileName(FSpecifiedDemandFileName);
    end;
    //Result := FSpecifiedDemandFileName;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TSpecifiedDemandFeature.Get_CatchmentRefNumber: integer;
const OPNAME = 'TSpecifiedDemandFeature.Get_CatchmentRefNumber';
begin
  Result := NullInteger;
  try
    Result := FCatchmentRefNumber;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TSpecifiedDemandFeature.Get_StochasticIndicator: WideString;
const OPNAME = 'TSpecifiedDemandFeature.Get_StochasticIndicator';
begin
  Result := '';
  try
    Result := FStochasticIndicator;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TSpecifiedDemandFeature.Set_FeatureName (const AName : WideString);
const OPNAME = 'TSpecifiedDemandFeature.Set_FeatureName';
var
  LLoadAgent   : TChannelDataSQLAgent;
  LContextData : TStringList;
  LOldValue    : string;
begin
  try
    LLoadAgent := TChannelDataSQLAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        LLoadAgent.LoadContextData_FeatureID(LContextData, IntToStr(FFeatureID));
        if FAppModules.FieldProperties.UpdateFieldValue(
           'SpecifiedDemandFeatureName', AName, FFeatureName, LContextData) then
        begin
          LOldValue    := FFeatureName;
          FFeatureName := AName;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'SpecifiedDemandFeatureName',LOldValue,AName);
        end;
      finally
        LContextData.Free;
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TSpecifiedDemandFeature.Set_Channel (const AChannel : IGeneralFlowChannel);
const OPNAME = 'TSpecifiedDemandFeature.Set_Channel';
var
  LLoadAgent   : TChannelDataSQLAgent;
  LContextData : TStringList;
  LOldValue    : string;
  LNewValue    : string;
begin
  try
    LLoadAgent := TChannelDataSQLAgent.Create(FAppModules);
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
           'SpecifiedDemandFeatureChannelNumber', LNewValue, LOldValue, LContextData) then
        begin
          FChannelNr := AChannel.ChannelNumber;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'SpecifiedDemandFeatureChannelNumber',LOldValue,LNewValue);
        end;
      finally
        LContextData.Free;
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TSpecifiedDemandFeature.Set_FeatureType (AType : integer);
const OPNAME = 'TSpecifiedDemandFeature.Set_FeatureType';
var
  LLoadAgent   : TChannelDataSQLAgent;
  LContextData : TStringList;
  LOldValue: integer;
begin
  try
    LLoadAgent := TChannelDataSQLAgent.Create(FAppModules);
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

procedure TSpecifiedDemandFeature.Set_FeatureSubType (ASubType : integer);
const OPNAME = 'TSpecifiedDemandFeature.Set_FeatureSubType';
var
  LLoadAgent   : TChannelDataSQLAgent;
  LContextData : TStringList;
  LOldValue: integer;
begin
  try
    LLoadAgent := TChannelDataSQLAgent.Create(FAppModules);
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

procedure TSpecifiedDemandFeature.Set_CatchmentRefNumber(ACatchmentRefNumber: integer);
const OPNAME = 'TSpecifiedDemandFeature.Set_CatchmentRefNumber';
var
  LLoadAgent: TChannelDataSQLAgent;
  LContextData: TStringList;
  LOldValue: integer;
begin
  try
    LLoadAgent := TChannelDataSQLAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        LLoadAgent.LoadContextData_FeatureID(LContextData, IntToStr(FFeatureID));
        if FAppModules.FieldProperties.UpdateFieldValue(
             'GaugeNumber', IntToStr(ACatchmentRefNumber), IntToStr(FCatchmentRefNumber), LContextData) then
        begin
          LOldValue := FCatchmentRefNumber;
          FCatchmentRefNumber := ACatchmentRefNumber;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'GaugeNumber',IntToStr(LOldValue), IntToStr(ACatchmentRefNumber)) ;
        end;
      finally
        LContextData.Free;
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TSpecifiedDemandFeature.Set_SpecifiedDemandFileName(const ADemandFileName: WideString);
const OPNAME = 'TSpecifiedDemandFeature.Set_SpecifiedDemandFileName';
var
  LLoadAgent: TChannelDataSQLAgent;
  LContextData: TStringList;
  LDemandFileName,
  LMesg,
  LOldValue: string;
  LFileName: TAbstractModelFileName;
  LNewFileIndex: integer;
begin
  try
    LLoadAgent := TChannelDataSQLAgent.Create(FAppModules);
    LContextData := TStringList.Create;
    try
      LDemandFileName := ExtractFileName(ADemandFileName);
      LLoadAgent.LoadContextData_FeatureID(LContextData, IntToStr(FFeatureID));
      if FAppModules.FieldProperties.UpdateFieldValue(
           'Fullname', ADemandFileName, FSpecifiedDemandFileName, LContextData) then
      begin
        LNewFileIndex := TYieldModelDataObject(FAppModules.Model.ModelData).CastFileNamesObject.DemandFileNames.Count+1;
        LFileName := TYieldModelDataObject(FAppModules.Model.ModelData).CastFileNamesObject.DemandFileNames.FindFile(LDemandFileName);
        if (LFileName = nil) then
          TYieldModelDataObject(FAppModules.Model.ModelData).CastFileNamesObject.AddDemandFileName(LNewFileIndex,LDemandFileName,False,0.0,
            FileLastWriteDate(LDemandFileName));
        LFileName := TYieldModelDataObject(FAppModules.Model.ModelData).CastFileNamesObject.DemandFileNames.FindFile(LDemandFileName);
        if (LFileName <> nil) and (not LFileName.SavedInDB) then
        begin
          LMesg := FAppModules.Language.GetString('SelectedFile.ImportFile');
          if (MessageDlg(LMesg,mtConfirmation,[mbYes, mbNo],0) = mrYes) then
            FAppModules.Model.ProcessEvent(CmeImportDemandFile,LFileName);
        end;
        LOldValue := FSpecifiedDemandFileName;
        FSpecifiedDemandFileName := ADemandFileName;
        FAppModules.Model.StudyDataHasChanged(sdccEdit,'Fullname',LOldValue,LDemandFileName);
      end;
    finally
      LContextData.Free;
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TSpecifiedDemandFeature.Set_StochasticIndicator(const AStochasticIndicator: WideString);
const OPNAME = 'TSpecifiedDemandFeature.Set_StochasticIndicator';
var
  LLoadAgent: TChannelDataSQLAgent;
  LContextData: TStringList;
  LOldValue: string;
begin
  try
    LLoadAgent := TChannelDataSQLAgent.Create(FAppModules);
    try
      LContextData := TStringList.Create;
      try
        LLoadAgent.LoadContextData_FeatureID(LContextData, IntToStr(FFeatureID));
        if FAppModules.FieldProperties.UpdateFieldValue(
             'Stochastic', AStochasticIndicator, FStochasticIndicator, LContextData) then
        begin
          LOldValue := FStochasticIndicator;
          FStochasticIndicator := AStochasticIndicator;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'Stochastic',LOldValue,AStochasticIndicator);
        end;
      finally
        LContextData.Free;
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TSpecifiedDemandFeature.Populate (AFeatureID           : integer;
                                           AFeatureName         : WideString;
                                           AChannelNr           : integer;
                                           AFeatureType         : integer;
                                           AFeatureSubType      : integer;
                                           ACatchmentRefNumber  : integer;
                                           AStochasticIndicator : WideString;
                                           ADemandFileName      : WideString): WordBool;
const OPNAME = 'TSpecifiedDemandFeature.Populate';
begin
  Result := FALSE;
  try
    FChannelNr          := AChannelNr;
    FFeatureID          := AFeatureID;
    FFeatureName        := AFeatureName;
    FFeatureType        := AFeatureType;
    FFeatureSubType     := AFeatureSubType;
    FCatchmentRefNumber := ACatchmentRefNumber;
    FStochasticIndicator := AStochasticIndicator;
    FSpecifiedDemandFileName := ADemandFileName;
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TSpecifiedDemandFeature.GetKeyValues (const AParamField : WideString;
                                               const AFieldIndex : WideString) : WideString;
const OPNAME = 'TSpecifiedDemandFeature.GetKeyValues';
begin
  Result := '';
  try
    Result := GetDefaultKey;
    if (Result = '') then
      Result := 'Identifier=' + IntToStr(FFeatureID)
    else
      Result := Result + ',Identifier=' + IntToStr(FFeatureID);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TSpecifiedDemandFeature.ValidateFeatureName(AErrorMessages: TStrings): WordBool;
const OPNAME = 'TSpecifiedDemandFeature.ValidateFeatureName';
var
  lIndex       : integer;
  lUnique      : Boolean;
  lMessage     : string;
  lFeature     : TSpecifiedDemandFeature;
  lFeatureList : TSpecifiedDemandFeatureList;
begin
  Result := True;
  try
    lMessage := '';
    if (NOT FAppModules.FieldProperties.ValidateFieldProperty('SpecifiedDemandFeatureName', FFeatureName, lMessage)) then
      AErrorMessages.Add('WARNING:'+Channel.ChannelName +':'+lMessage)
    else
    begin
      lFeatureList := TYieldModelDataObject(FAppModules.Model.ModelData).
                        CastNetworkFeaturesData.CastSpecifiedDemandFeatureList;
      lUnique := True;
      lIndex  := 0;
      while (lUnique AND (lIndex < lFeatureList.SpecifiedDemandFeatureCount)) do
      begin
        lFeature := lFeatureList.CastSpecifiedDemandFeatureByIndex(lIndex);
        if ((FFeatureID <> lFeature.FeatureID) AND
            (UpperCase(Trim(FFeatureName)) = UpperCase(Trim(lFeature.FeatureName)))) then
        begin
          lMessage := FAppModules.language.GetString('ContextValidation.DuplicateSpecifiedDemandName');
          AErrorMessages.Add('WARNING:'+Format(lMessage, [Channel.ChannelName]));
          lUnique := False;
        end
        else
          lIndex := lIndex + 1;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TSpecifiedDemandFeature.ValidateStochasticIndicator(AErrorMessages: TStrings): WordBool;
const OPNAME = 'TSpecifiedDemandFeature.ValidateStochasticIndicator';
var
  lMessage          : string;
  lValid            : Boolean;
  lResult           : Boolean;
  lFieldProperty    : TAbstractFieldProperty;
begin
  Result := FALSE;
  try
    lResult := TRUE;
    lFieldProperty := FAppModules.FieldProperties.FieldProperty('Stochastic');
    if (lFieldProperty <> nil) then
    begin
      lMessage := '';
      lValid := FAppModules.FieldProperties.ValidateFieldProperty
                ('Stochastic', FStochasticIndicator, lMessage);
      if (NOT lValid) then
      begin
        lResult := FALSE;
        lMessage := FAppModules.language.GetString('ContextValidation.InvalidStochasticIndicator');
        AErrorMessages.Add('ERROR:' +Format(lMessage,[StochasticIndicator, lFieldProperty.FieldAcceptedValues]));
      end;
    end;
    Result := lResult;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TSpecifiedDemandFeature.ValidateCatchmentRefNumber(AErrorMessages: TStrings): WordBool;
const OPNAME = 'TSpecifiedDemandFeature.ValidateCatchmentRefNumber';
var
  lMessage : string;
  lMaxNum  : integer;
begin
  Result := False;
  try
    lMessage := '';
    Result := FAppModules.FieldProperties.ValidateFieldProperty('GaugeNumber', IntToStr(FCatchmentRefNumber),
              lMessage);
    if (NOT Result) then
      AErrorMessages.Add('ERROR:' +Channel.ChannelName+ ':'+lMessage)
    else
    begin
      lMaxNum := TYieldModelDataObject(FAppModules.Model.ModelData).ParamSetup.ReferenceCount;
      if (FCatchmentRefNumber > lMaxNum) then
      begin
        Result := FALSE;
        lMessage := FAppModules.language.GetString('ContextValidation.InvalidCatchmentRefNumber');
        AErrorMessages.Add('ERROR:' +Format(lMessage, [IntToStr(FCatchmentRefNumber), IntToStr(lMaxNum)]));
      end
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TSpecifiedDemandFeature.ValidateFileName(AErrorMessages: TStrings): WordBool;
const OPNAME = 'TSpecifiedDemandFeature.ValidateFileName';
var
  lMessage          : string;
  lValid            : Boolean;
  //lResult           : Boolean;
  lFieldProperty    : TAbstractFieldProperty;
  LSpecifiedDemandFile : TAbstractModelFileName;
begin
  Result := True;
  try
    //lResult := TRUE;
    lFieldProperty := FAppModules.FieldProperties.FieldProperty('Fullname');
    if (lFieldProperty <> nil) then
    begin
      lMessage := '';
      lValid := FAppModules.FieldProperties.ValidateFieldProperty
                ('Fullname', FSpecifiedDemandFileName, lMessage);
      if (NOT lValid) then
      begin
        //lResult := FALSE;
        lMessage := FAppModules.Language.GetString('ContextValidation.FullNamelength');
        AErrorMessages.Add('WARNING:' +Format(lMessage, [lFieldProperty.FieldMaximumValue]));
      end
      else
      begin
        if(FSpecifiedDemandFileName <> '') then
        begin
          LSpecifiedDemandFile := TYieldModelDataObject(FAppModules.Model.ModelData).
                                  FileNamesObject.DemandFileNames.FindFile(FSpecifiedDemandFileName);
          if (LSpecifiedDemandFile = nil) then
          begin
            //lResult := FALSE;
            lMessage := FAppModules.language.GetString('ContextValidation.InvalidFileName');
            AErrorMessages.Add('WARNING:' +Format(lMessage, [lFieldProperty.FieldMaximumValue]));
          end;
        end;
      end;
    end;
    //Result := lResult;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TSpecifiedDemandFeature.Validate (var AErrors: WideString; const AContext: WideString = '') : WordBool;
const OPNAME = 'TSpecifiedDemandFeature.Validate';
var
  lStopOnFirstError : Boolean;
  lErrorList        : TStringList;
begin
  Result := FALSE;
  try
    lErrorList := TStringList.Create;
    try
      if (AContext = 'FeatureName') then
        Result := ValidateFeatureName(lErrorList)
      else
      if (AContext = 'StochasticIndicator') then
        Result := ValidateStochasticIndicator(lErrorList)
      else
      if (AContext = 'CatchmentRefNumber') then
        Result := ValidateCatchmentRefNumber(lErrorList)
      else
      if (AContext = 'FileName') then
        Result := ValidateFileName(lErrorList)
      else
      begin
        Result := TRUE;
        lStopOnFirstError := FAppModules.GlobalData.StopOnFirstErr;
        if (NOT ValidateFeatureName(lErrorList)) then
          Result := FALSE;
        if (Result OR (NOT lStopOnFirstError)) then
        begin
          if (NOT ValidateStochasticIndicator(lErrorList)) then
            Result := FALSE;
        end;
        if (Result OR (NOT lStopOnFirstError)) then
        begin
          if (NOT ValidateCatchmentRefNumber(lErrorList)) then
            Result := FALSE;
        end;
        if (Result OR (NOT lStopOnFirstError)) then
        begin
          if (NOT ValidateFileName(lErrorList)) then
            Result := FALSE;
        end;
      end;
      AErrors := AErrors + lErrorList.Text;
    finally;
      FreeAndNil(lErrorList);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TSpecifiedDemandFeature.GetMonthlyDemand (ATimeStep   : integer;
                                                   var ADemand : WideString): WordBool;
const OPNAME = 'TSpecifiedDemandFeature.GetMonthlyDemand';
var
  lFileStartYear : integer;
  lRunStartYear  : integer;
  lYearList      : TStringList;
  lYearIndex     : integer;
  lMonthIndex    : integer;
begin
  Result := FALSE;
  try
    if (FDemandFileData.Count = 0) then
      LoadDemandFileData;
    if (FDemandFileData.Count > 0) then
    begin
      lFileStartYear := StrToInt(Copy(FDemandFileData[0], 1, 4));
      lRunStartYear  := (FAppModules.Model.ModelData as IYieldModelData).
                          RunConfigurationData.StartYearOther;
      lYearIndex    := lRunStartYear - lFileStartYear + (ATimeStep div 12);
      lMonthIndex   := ATimeStep mod 12;
      if (lMonthIndex = 0) then
      begin
        lMonthIndex := 12;
        lYearIndex  := lYearIndex - 1;
      end;
      if (lYearIndex >= 0) AND (lYearIndex < FDemandFileData.Count) then
      begin
        lYearList := TStringList.Create;
        try
          lYearList.CommaText := FDemandFileData.Strings[lYearIndex];
          ADemand             := lYearList.Strings[lMonthIndex];
          Result              := TRUE;
        finally
          FreeAndNil(lYearList);
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TSpecifiedDemandFeature.GetAnnualDemand (AYear       : Integer;
                                                  var ADemand : WideString): WordBool;
const OPNAME = 'TSpecifiedDemandFeature.GetAnnualDemand';
var
  lFileStartYear : integer;
  lYearList      : TStringList;
  lYearIndex     : integer;
begin
  Result := FALSE;
  try
    if (FDemandFileData.Count = 0) then
      LoadDemandFileData;
    if (FDemandFileData.Count > 0) then
    begin
      lFileStartYear := StrToInt(Copy(FDemandFileData[0], 1, 4));
      lYearIndex     := AYear - lFileStartYear;

      if (lYearIndex >= 0) AND (lYearIndex < FDemandFileData.Count) then
      begin
        lYearList := TStringList.Create;
        try
          lYearList.CommaText := FDemandFileData.Strings[lYearIndex];
          ADemand             := lYearList.Strings[13];
          Result              := TRUE;
        finally
          FreeAndNil(lYearList);
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TSpecifiedDemandFeature.LoadDemandFileData;
const OPNAME = 'TSpecifiedDemandFeature.LoadDemandFileData';
var
  lIndex          : integer;
  lFieldName      : string;
  lLineData       : string;
  lSQL            : string;
  lMonthData      : double;
  lFileNamesList  : TFileNamesList;
  lFileNameObject : TAbstractModelFileName;
  lDataSet        : TAbstractModelDataset;
begin
  try
    FDemandFileData.Clear;
    lFileNameObject := nil;
    lFileNamesList  := TYieldModelDataObject(FAppModules.Model.ModelData).CastFileNamesObject.CastDemandFileNames;
    for lIndex:= 0 to lFileNamesList.FilesCount-1 do
    begin
      if (UpperCase(FSpecifiedDemandFileName) = UpperCase(lFileNamesList.FileNameObject[lIndex].FileName)) then
      begin
        lFileNameObject := lFileNamesList.FileNameObject[lIndex];
        Break;
      end;
    end;
    if (lFileNameObject <> nil) then
    begin
      lSQL := 'SELECT DemandYearValue as [Year], ' +
              'DemandMonthValue01 as Value01, ' +
              'DemandMonthValue02 as Value02, ' +
              'DemandMonthValue03 as Value03, ' +
              'DemandMonthValue04 as Value04, ' +
              'DemandMonthValue05 as Value05, ' +
              'DemandMonthValue06 as Value06, ' +
              'DemandMonthValue07 as Value07, ' +
              'DemandMonthValue08 as Value08, ' +
              'DemandMonthValue09 as Value09, ' +
              'DemandMonthValue10 as Value10, ' +
              'DemandMonthValue11 as Value11, ' +
              'DemandMonthValue12 as Value12,  ' +
              'DemandTotalValue ' +
              'FROM DemandFileData A WHERE ' +
              'Model         = ' + QuotedStr(FAppModules.StudyArea.ModelCode)     + ' AND ' +
              'StudyAreaName = ' + QuotedStr(FAppModules.StudyArea.StudyAreaCode) + ' AND ' +
              'SubArea       = ' + QuotedStr(FAppModules.StudyArea.SubAreaCode)   + ' AND ' +
              'Scenario      = ' + QuotedStr(FAppModules.StudyArea.ScenarioCode)  + ' AND ' +
              'FileNumber    = ' + IntToStr(lFileNameObject.FileNumber);
      FAppModules.Database.CreateDataset(integer(dtExecSQL), lDataSet);
      try
        lDataSet.SetSQL(lSQL);
        lDataSet.DataSet.Open;
        while (NOT LDataSet.DataSet.Eof) do
        begin
          lLineData := Trim(lDataSet.DataSet.FieldByName('Year').AsString);
          for lIndex := 1 to 12 do
          begin
            lFieldName := Format('%s%2.2d',['Value', lIndex]);
            lMonthData := lDataSet.DataSet.FieldByName(lFieldName).AsFloat;
            lLineData  := lLineData + ',' + FormatFloat('######0.000', lMonthData);
          end;
          lMonthData := lDataSet.DataSet.FieldByName('DemandTotalValue').AsFloat;
          lLineData  := lLineData + ',' + FormatFloat('######0.000', lMonthData);
          FDemandFileData.Add(lLineData);
          lDataSet.DataSet.Next;
        end;
      finally
        LDataSet.Free;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{******************************************************************************}
{ TSpecifiedDemandFeatureList                                                 *}
{******************************************************************************}

function TSpecifiedDemandFeatureList._AddRef: Integer;
const OPNAME = 'TSpecifiedDemandFeatureList._AddRef';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TSpecifiedDemandFeatureList._Release: Integer;
const OPNAME = 'TSpecifiedDemandFeatureList._Release';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
 except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TSpecifiedDemandFeatureList.CreateMemberObjects;
const OPNAME = 'TSpecifiedDemandFeatureList.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
    FSpecifiedDemandFeatureList := TList.Create;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TSpecifiedDemandFeatureList.DestroyMemberObjects;
const OPNAME = 'TSpecifiedDemandFeatureList.DestroyMemberObjects';
begin
  try
    while (FSpecifiedDemandFeatureList.Count > 0) do
      DeleteSpecifiedDemandFeatureWithIndex(0);
    FreeAndNil(FSpecifiedDemandFeatureList);
    inherited DestroyMemberObjects;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TSpecifiedDemandFeatureList.Initialise: boolean;
const OPNAME = 'TSpecifiedDemandFeatureList.Initialise';
begin
  Result := inherited Initialise;
  try
    FSpecifiedDemandFeatureList.Clear;
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TSpecifiedDemandFeatureList.AddDemandFeature (AFeature : TSpecifiedDemandFeature): boolean;
const OPNAME = 'TSpecifiedDemandFeatureList.AddDemandFeature';
begin
  Result := False;
  try
    if (AFeature <> nil) then
    begin
      FSpecifiedDemandFeatureList.Add(AFeature);
      Result := TRUE;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TSpecifiedDemandFeatureList.NewSpecifiedDemandFeature : TSpecifiedDemandFeature;
const OPNAME = 'TSpecifiedDemandFeatureList.NewSpecifiedDemandFeature';
var
  lFeature : TSpecifiedDemandFeature;
begin
  Result := nil;
  try
    lFeature := TSpecifiedDemandFeature.Create(FAppModules);
    AddDemandFeature(lFeature);
    Result := lFeature;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TSpecifiedDemandFeatureList.CreateSpecifiedDemandFeature : ISpecifiedDemandFeature;
const OPNAME = 'TSpecifiedDemandFeatureList.CreateSpecifiedDemandFeature';
var
  LFeature : TSpecifiedDemandFeature;
begin
  Result := nil;
  try
    LFeature := CreateNewSpecifiedDemandFeature;
    Result := lFeature;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TSpecifiedDemandFeatureList.RemoveSpecifiedDemandFeatureWithID (AFeatureID : integer) : WordBool;
const OPNAME = 'TSpecifiedDemandFeatureList.RemoveSpecifiedDemandFeatureWithID';
var
  lLoadAgent : TChannelDataSQLAgent;
begin
  Result := FALSE;
  try
    if (AFeatureID > 0) then
    begin
      LLoadAgent := TChannelDataSQLAgent.Create(FAppModules);
      try
        if LLoadAgent.DeleteSpecifiedDemandFeature(AFeatureID) then
        begin
          DeleteSpecifiedDemandFeatureWithID(AFeatureID);
          Result := TRUE;
        end;
      finally
        LLoadAgent.Free;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TSpecifiedDemandFeatureList.DeleteSpecifiedDemandFeatureWithID(AFeatureID : integer) : WordBool;
const OPNAME = 'TSpecifiedDemandFeatureList.DeleteSpecifiedDemandFeatureWithID';
var
  lFeature : TSpecifiedDemandFeature;
  lIndex   : integer;
begin
  Result := FALSE;
  try
    lFeature := CastSpecifiedDemandFeatureByID(AFeatureID);
    if (lFeature <> nil) then
    begin
      lIndex := FSpecifiedDemandFeatureList.IndexOf(lFeature);
      Result := DeleteSpecifiedDemandFeatureWithIndex(lIndex);
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TSpecifiedDemandFeatureList.DeleteSpecifiedDemandFeatureWithIndex(AIndex : integer) : WordBool;
const OPNAME = 'TSpecifiedDemandFeatureList.DeleteSpecifiedDemandFeatureWithIndex';
var
  lFeature : TSpecifiedDemandFeature;
begin
  Result := FALSE;
  try
    if (AIndex >= 0) then
    begin
      lFeature := TSpecifiedDemandFeature(FSpecifiedDemandFeatureList.Items[AIndex]);
      FSpecifiedDemandFeatureList.Delete(AIndex);
      FreeAndNil(lFeature);
      Result := TRUE;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TSpecifiedDemandFeatureList.Get_SpecifiedDemandFeatureByIndex(AIndex: integer): ISpecifiedDemandFeature;
const OPNAME = 'TSpecifiedDemandFeatureList.Get_SpecifiedDemandFeatureByIndex';
begin
  Result := nil;
  try
    if (AIndex >= 0) and (AIndex < FSpecifiedDemandFeatureList.Count) then
      Result := TSpecifiedDemandFeature(FSpecifiedDemandFeatureList.Items[AIndex]);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TSpecifiedDemandFeatureList.Get_SpecifiedDemandFeatureByID(AFeatureID : integer): ISpecifiedDemandFeature;
const OPNAME = 'TSpecifiedDemandFeatureList.Get_SpecifiedDemandFeatureByID';
var
 lIndex   : integer;
 lFeature : TSpecifiedDemandFeature;
begin
  Result := nil;
  try
    lIndex := 0;
    while ((Result = nil) AND (lIndex < FSpecifiedDemandFeatureList.Count)) do
    begin
      lFeature := TSpecifiedDemandFeature(FSpecifiedDemandFeatureList.Items[lIndex]);
      if (lFeature.FeatureID = AFeatureID) then
        Result := lFeature
      else
        lIndex := lIndex + 1;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TSpecifiedDemandFeatureList.CastSpecifiedDemandFeatureByIndex(AIndex: integer): TSpecifiedDemandFeature;
const OPNAME = 'TSpecifiedDemandFeatureList.CastSpecifiedDemandFeatureByIndex';
begin
  Result := nil;
  try
    if (AIndex >= 0) and (AIndex < FSpecifiedDemandFeatureList.Count) then
      Result := TSpecifiedDemandFeature(FSpecifiedDemandFeatureList.Items[AIndex]);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TSpecifiedDemandFeatureList.CastSpecifiedDemandFeatureByID(AFeatureID : integer): TSpecifiedDemandFeature;
const OPNAME = 'TSpecifiedDemandFeatureList.CastSpecifiedDemandFeatureByID';
var
 lIndex   : integer;
 lFeature : TSpecifiedDemandFeature;
begin
  Result := nil;
  try
    lIndex := 0;
    while ((Result = nil) AND (lIndex < FSpecifiedDemandFeatureList.Count)) do
    begin
      lFeature := TSpecifiedDemandFeature(FSpecifiedDemandFeatureList.Items[lIndex]);
      if (lFeature.FeatureID = AFeatureID) then
        Result := lFeature
      else
        lIndex := lIndex + 1;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TSpecifiedDemandFeatureList.CastSpecifiedDemandFeatureByChannelNr(AChannelNr : integer): TSpecifiedDemandFeature;
const OPNAME = 'TSpecifiedDemandFeatureList.CastSpecifiedDemandFeatureByChannelNr';
var
 lIndex   : integer;
 lFeature : TSpecifiedDemandFeature;
begin
  Result := nil;
  try
    lIndex := 0;
    while ((Result = nil) AND (lIndex < FSpecifiedDemandFeatureList.Count)) do
    begin
      lFeature := TSpecifiedDemandFeature(FSpecifiedDemandFeatureList.Items[lIndex]);
      if (lFeature.FChannelNr = AChannelNr) then
        Result := lFeature
      else
        lIndex := lIndex + 1;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;


function TSpecifiedDemandFeatureList.Get_SpecifiedDemandFeatureCount: integer;
const OPNAME = 'TSpecifiedDemandFeatureList.Get_SpecifiedDemandFeatureCount';
begin
  Result := 0;
  try
    Result := FSpecifiedDemandFeatureList.Count;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TSpecifiedDemandFeatureList.CreateNewSpecifiedDemandFeature: TSpecifiedDemandFeature;
const OPNAME = 'TSpecifiedDemandFeatureList.CreateNewSpecifiedDemandFeature';
var
  LFeatureID           : integer;
  LLoadAgent           : TChannelDataSQLAgent;
  LFeature             : TSpecifiedDemandFeature;
begin
  Result := nil;
  try
    LLoadAgent := TChannelDataSQLAgent.Create(FAppModules);
    try
      LFeatureID := 0;
      if (LLoadAgent.InsertSpecifiedDemandFeature(LFeatureID)) then
      begin
          LFeature := NewSpecifiedDemandFeature;
          LFeature.Initialise;
          LFeature.Populate
            (LFeatureID,
             UpperCase(FAppModules.Language.GetString('NetworkFeatures.SpecifiedDemand')) + ' ' + IntToStr(LFeatureID),
             0, 4, 0, 0, 'H', '');

          Result := LFeature;
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TSpecifiedDemandFeatureList.Validate(var AErrors: WideString; const AContext: WideString): WordBool;
const OPNAME = 'TSpecifiedDemandFeatureList.Validate';
var
  LIndex            : integer;
  lStopOnFirstError : Boolean;
begin
  Result := FALSE;
  try
    Result := TRUE;
    lStopOnFirstError := FAppModules.GlobalData.StopOnFirstErr;
    for LIndex := 0 to FSpecifiedDemandFeatureList.Count - 1 do
    begin
      if (NOT SpecifiedDemandFeatureByIndex[LIndex].Validate(AErrors,AContext)) then
        Result := FALSE;
      if ((NOT Result) AND lStopOnFirstError) then
        Break;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TSpecifiedDemandFeatureList.CopySpecifiedDemandFeature(ANewChannelNumber, AOldChannelNumber: Integer): ISpecifiedDemandFeature;
const OPNAME = 'TSpecifiedDemandFeatureList.CopySpecifiedDemandFeature';
var
  LSpecifiedDemandFeature : TSpecifiedDemandFeature;
  LSpecifiedDemandFeatureCopy : TSpecifiedDemandFeature;
begin
  Result := nil;
  try
    LSpecifiedDemandFeature := CastSpecifiedDemandFeatureByChannelNr(AOldChannelNumber);
    if (LSpecifiedDemandFeature <> nil) then
    begin
      LSpecifiedDemandFeatureCopy := CreateNewSpecifiedDemandFeature;
      if LSpecifiedDemandFeatureCopy <> nil then
      begin
        LSpecifiedDemandFeatureCopy.Assign(ANewChannelNumber,LSpecifiedDemandFeature);
        Result := LSpecifiedDemandFeatureCopy;
      end;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TSpecifiedDemandFeature.Assign(AChannelNumber: integer;ASpecifiedDemandFeature: TSpecifiedDemandFeature);
const OPNAME = 'TSpecifiedDemandFeature.Assign';
var
  LChannel : IGeneralFlowChannel;
begin
  try
    if (ASpecifiedDemandFeature <> nil) and (AChannelNumber > 0) then
    begin
      LChannel := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.ChannelList.ChannelByChannelNumber[AChannelNumber];
      if LChannel <> nil then
        Channel := LChannel;
      FeatureName := 'Copy of '+ASpecifiedDemandFeature.FeatureName;
      FeatureType := ASpecifiedDemandFeature.FeatureType;
      FeatureSubType := ASpecifiedDemandFeature.FeatureSubType;
      CatchmentRefNumber := ASpecifiedDemandFeature.CatchmentRefNumber;
      StochasticIndicator := ASpecifiedDemandFeature.StochasticIndicator;
      SpecifiedDemandFileName := ASpecifiedDemandFeature.SpecifiedDemandFileName;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

end.
