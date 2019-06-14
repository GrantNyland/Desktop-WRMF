//
//
//  UNIT      : Contains TStreamFlowReduction Class
//  AUTHOR    : Dziedzi Ramulondi(PDNA)
//  DATE      : 22/11/2006
//  COPYRIGHT : Copyright © 2006 DWAF
//
//

unit UStreamFlowReduction;

interface

uses
  Classes,
  Contnrs,
  UAbstractObject,
  VoaimsCom_TLB,
  UConstants,
  VCL.Dialogs,
  VCL.Controls;

type
  TStreamFlowReduction = class(TAbstractAppObject, IStreamFlowReduction)
  protected
    FIdentifier           : Integer;
    FInflowNodeNumber     : Integer;
    FCoveredArea          : Double;
    FUnitRunoffFileName   : WideString;
    FSoilMoistureFileName : WideString;
    FSFRName              : WideString;
    FSFRDescription       : WideString;

    function Get_Identifier: Integer; safecall;
    function Get_InflowNodeNumber: Integer; safecall;
    function Get_CoveredArea: Double; safecall;
    function Get_UnitRunoffFileName: WideString; safecall;
    function Get_SoilMoistureFileName: WideString; safecall;
    function Get_SFRName: WideString; safecall;
    function Get_SFRDescription: WideString; safecall;
    function Get_UnitRunoffFileData: WideString; safecall;
    function Get_SoilMoistureFileData: WideString; safecall;
    function Get_InflowNodeDetails: IReservoirData; safecall;

    procedure Set_InflowNodeNumber(Value: Integer); safecall;
    procedure Set_CoveredArea(Value: Double); safecall;
    procedure Set_UnitRunoffFileName(const Value: WideString); safecall;
    procedure Set_SoilMoistureFileName(const Value: WideString); safecall;
    procedure Set_SFRName(const Value: WideString); safecall;
    procedure Set_SFRDescription(const Value: WideString); safecall;

    function ValidateInflowNodeNumber(AErrorMessages: TStrings) : Boolean;
    function ValidateCoveredArea(AErrorMessages: TStrings): Boolean;
    function ValidateUnitRunoffFileName(AErrorMessages: TStrings)    : Boolean;
    function ValidateSoilMoistureFileName(AErrorMessages: TStrings): Boolean;
    function ValidateSFRName(AErrorMessages: TStrings)      : Boolean;
    function ValidateSFRDescription(AErrorMessages : TStrings) : WordBool;

    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
  public
    procedure Assign(AStreamFlowReduction : TStreamFlowReduction);
    function Initialise : Boolean; override;
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; safecall;
    function Populate(AIdentifier,AInflowNodeNumber : Integer; ACoveredArea : Double;
                      AUnitRunoffFileName,ASoilMoistureFileName,ASFRName,ASFRDescription: WideString): Boolean;
    property Identifier            : Integer     read Get_Identifier;
    property InflowNodeNumber      : Integer     read Get_InflowNodeNumber write Set_InflowNodeNumber;
    property CoveredArea           : Double      read Get_CoveredArea      write Set_CoveredArea;
    property UnitRunoffFileName    : WideString  read Get_UnitRunoffFileName write Set_UnitRunoffFileName;
    property SoilMoistureFileName  : WideString  read Get_SoilMoistureFileName write Set_SoilMoistureFileName;
    property SFRName               : WideString  read Get_SFRName write Set_SFRName;
    property SFRDescription        : WideString  read Get_SFRDescription write Set_SFRDescription;
    property UnitRunoffFileData: WideString read Get_UnitRunoffFileData;
    property SoilMoistureFileData: WideString read Get_SoilMoistureFileData;
  end;

  TStreamFlowReductionList = class(TAbstractAppObject,IStreamFlowReductionList)
  protected
    FStreamFlowReductionList  : TObjectList;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    function Get_StreamFlowReductionCount: Integer; safecall;
    function Get_StreamFlowReductionByIndex(AIndex: integer): IStreamFlowReduction; safecall;
    function Get_StreamFlowReductionByID(AStreamFlowReductionID: integer): IStreamFlowReduction; safecall;
  public
    function Initialise : Boolean; override;
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; safecall;

    function CastStreamFlowReductionByID(AStreamFlowReductionID: Integer): TStreamFlowReduction;
    function CastStreamFlowReductionByIndex(AIndex : Integer): TStreamFlowReduction;
    function NewStreamFlowReduction : TStreamFlowReduction;
    function CopyCreate(AStreamFlowReductionID : integer) : IStreamFlowReduction; safecall;
    function CreateStreamFlowReduction : TStreamFlowReduction; safecall;
    function RemoveStreamFlowReduction(AStreamFlowReductionID : integer) : WordBool; safecall;
    function StreamFlowReductionIDsPerInflowNode(AInflowNodeNumber: Integer): WideString; safecall;
    function GetStreamFlowReductionsPerInflowNodeNumber(AInflowNodeNumber: integer; AStreamFlowReductionsList: TObjectList): boolean; virtual;

    property StreamFlowReductionByIndex[AIndex: Integer]              : IStreamFlowReduction read Get_StreamFlowReductionByIndex;
    property StreamFlowReductionByID[AStreamFlowReductionID: Integer] : IStreamFlowReduction read Get_StreamFlowReductionByID;
    property StreamFlowReductionCount                                 : Integer              read Get_StreamFlowReductionCount;
  end;

implementation

uses
  System.Types,
  System.UITypes,
  SysUtils,
  Math,
  UUtilities,
  UYieldModelDataObject,
  UNetworkElementData,
  UMainMenuEventType,
  UAbstractFileNamesObject,
  UStreamFlowReductionSQLAgent,
  UNetworkFeaturesSQLAgent,
  UErrorHandlingOperations;
  
{ TStreamFlowReduction }

function TStreamFlowReduction._AddRef: Integer;
const OPNAME = 'TStreamFlowReduction._AddRef';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TStreamFlowReduction._Release: Integer;
const OPNAME = 'TStreamFlowReduction._Release';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TStreamFlowReduction.CreateMemberObjects;
const OPNAME = 'TStreamFlowReduction.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TStreamFlowReduction.DestroyMemberObjects;
const OPNAME = 'TStreamFlowReduction.DestroyMemberObjects';
begin
  try
    inherited DestroyMemberObjects;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TStreamFlowReduction.Initialise: Boolean;
const OPNAME = 'TStreamFlowReduction.Initialise';
begin
  Result := inherited Initialise;
  try
    FIdentifier            := NullInteger;
    FInflowNodeNumber      := 0;
    FCoveredArea           := 0.0;
    FUnitRunoffFileName    := '';
    FSoilMoistureFileName  := '';
    FSFRName               := '';
    FSFRDescription        := '';
    Result                 := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;
{
function TStreamFlowReduction.Assign(AStreamFlowReduction: TStreamFlowReduction): Boolean;
const OPNAME = 'TStreamFlowReduction.Assign';
begin
  Result := False;
  try
    FIdentifier           := AStreamFlowReduction.Identifier;
    FInflowNodeNumber     := AStreamFlowReduction.InflowNodeNumber;
    FCoveredArea          := AStreamFlowReduction.CoveredArea;
    FUnitRunoffFileName   := AStreamFlowReduction.UnitRunoffFileName;
    FSoilMoistureFileName := AStreamFlowReduction.SoilMoistureFileName;
    FSFRName              := AStreamFlowReduction.SFRName;
    FSFRDescription       := AStreamFlowReduction.SFRDescription;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;
 }

function TStreamFlowReduction.Populate(AIdentifier,AInflowNodeNumber : Integer; ACoveredArea : Double;
         AUnitRunoffFileName,ASoilMoistureFileName,ASFRName,ASFRDescription: WideString): Boolean;
const OPNAME = 'TStreamFlowReduction.Populate';
begin
  Result := False;
  try
    FIdentifier           := AIdentifier;
    FInflowNodeNumber     := AInflowNodeNumber;
    FCoveredArea          := ACoveredArea;
    FUnitRunoffFileName   := AUnitRunoffFileName;
    FSoilMoistureFileName := ASoilMoistureFileName ;
    FSFRName              := ASFRName;
    FSFRDescription       := ASFRDescription;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TStreamFlowReduction.Get_Identifier: Integer;
const OPNAME = 'TStreamFlowReduction.Get_Identifier';
begin
  Result := NullInteger;
  try
    Result := FIdentifier;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TStreamFlowReduction.Get_InflowNodeNumber: Integer;
const OPNAME = 'TStreamFlowReduction.Get_InflowNodeNumber';
begin
  Result := NullInteger;
  try
    Result := FInflowNodeNumber;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TStreamFlowReduction.Get_CoveredArea: Double;
const OPNAME = 'TStreamFlowReduction.Get_CoveredArea';
begin
  Result := NullFloat;
  try
    Result := FCoveredArea;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TStreamFlowReduction.Get_UnitRunoffFileName: WideString;
const OPNAME = 'TStreamFlowReduction.Get_UnitRunoffFileName';
var
  LPath: string;
begin
  Result := '';
  try
    if (Trim(FUnitRunoffFileName) <> '') then
    begin
      Result := Trim(FUnitRunoffFileName);
      if(ExtractFileExt(FUnitRunoffFileName) <> '') then
      begin
        LPath := Trim(TYieldModelDataObject(FAppModules.Model.ModelData).CastFileNamesObject.HydrologyFilesPath);
        Result := LPath + ExtractFileName(FUnitRunoffFileName);
      end
    end;
    //Result := FUnitRunoffFileName;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TStreamFlowReduction.Get_SoilMoistureFileName: WideString;
const OPNAME = 'TStreamFlowReduction.Get_SoilMoistureFileName';
var
  LPath: string;
begin
  Result := '';
  try
    if (Trim(FSoilMoistureFileName) <> '') then
    begin
      Result := Trim(FSoilMoistureFileName);
      if(ExtractFileExt(FSoilMoistureFileName) <> '') then
      begin
        LPath := Trim(TYieldModelDataObject(FAppModules.Model.ModelData).CastFileNamesObject.HydrologyFilesPath);
        Result := LPath + ExtractFileName(FSoilMoistureFileName);
      end;
    end;
    //Result := FSoilMoistureFileName;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TStreamFlowReduction.Get_SFRName: WideString;
const OPNAME = 'TStreamFlowReduction.Get_SFRName';
begin
  Result := '';
  try
    Result := FSFRName;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TStreamFlowReduction.Get_SFRDescription: WideString;
const OPNAME = 'TStreamFlowReduction.Get_SFRDescription';
begin
  Result := '';
  try
    Result := FSFRDescription;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TStreamFlowReduction.Set_InflowNodeNumber(Value: Integer);
const OPNAME = 'TStreamFlowReduction.Set_InflowNodeNumber';
var
  LLoadAgent    : TStreamFlowReductionSQLAgent;
  LContextData  : TStringList;
  LOldValue     : string;
begin
  try
    if FInflowNodeNumber <> Value then
    begin
      LContextData  := TStringList.Create;
      LLoadAgent    := TStreamFlowReductionSQLAgent.Create(FAppModules);
      try
        LOldValue := IntToStr(FInflowNodeNumber);
        LLoadAgent.LoadContextData(LContextData, IntToStr(FIdentifier));
        if FAppModules.FieldProperties.UpdateFieldValue('InflowNodeNumber', FloatToStr(Value), LOldValue, LContextData ) then
        begin
          FInflowNodeNumber := Value;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'InflowNodeNumber',LOldValue,FloatToStr(Value));
        end;
      finally
        FreeAndNil(LLoadAgent);
        FreeAndNil(LContextData);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TStreamFlowReduction.Set_CoveredArea(Value: Double);
const OPNAME = 'TStreamFlowReduction.Set_CoveredArea';
var
  LLoadAgent    : TStreamFlowReductionSQLAgent;
  LContextData  : TStringList;
  LOldValue     : string;
begin
  try
    if FCoveredArea <> Value then
    begin
      LContextData  := TStringList.Create;
      LLoadAgent    := TStreamFlowReductionSQLAgent.Create(FAppModules);
      try
        LOldValue := FloatToStr(FCoveredArea);
        LLoadAgent.LoadContextData(LContextData, IntToStr(FIdentifier));
        if FAppModules.FieldProperties.UpdateFieldValue('CoveredArea', FloatToStr(Value), LOldValue, LContextData ) then
        begin
          FCoveredArea := Value;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'CoveredArea',LOldValue,FloatToStr(Value));
        end;
      finally
        FreeAndNil(LLoadAgent);
        FreeAndNil(LContextData);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TStreamFlowReduction.Set_UnitRunoffFileName(const Value: WideString);
const OPNAME = 'TStreamFlowReduction.Set_UnitRunoffFileName';
var
  LLoadAgent    : TStreamFlowReductionSQLAgent;
  LContextData  : TStringList;
  LValue,
  LMesg         : string;
  LOldValue     : string;
  LFileName     : string;
  LFileNameObject: TAbstractModelFileName;
  LNewFileIndex: integer;
begin
  try
    LValue := ExtractFileName(Value);
    if (FUnitRunoffFileName <> LValue) then
    begin
      LContextData  := TStringList.Create;
      LLoadAgent    := TStreamFlowReductionSQLAgent.Create(FAppModules);
      try
        LOldValue := FUnitRunoffFileName;
        LLoadAgent.LoadContextData(LContextData, IntToStr(FIdentifier));
        if FAppModules.FieldProperties.UpdateFieldValue('UnitRunoffFileName', LValue, LOldValue, LContextData ) then
        begin
          LFileName     := TYieldModelDataObject(FAppModules.Model.ModelData).CastFileNamesObject.HydrologyFilesPath+LValue;
          LFileNameObject := TYieldModelDataObject(FAppModules.Model.ModelData).CastFileNamesObject.HydrologyFileNames.FindFile(LFileName);
          if (LFileNameObject = nil) then
          begin
            LNewFileIndex := TYieldModelDataObject(FAppModules.Model.ModelData).CastFileNamesObject.HydrologyFileNames.Count+1;
            TYieldModelDataObject(FAppModules.Model.ModelData).CastFileNamesObject.AddHydrologyFileName(LNewFileIndex,LFileName,False,0.0,
              FileLastWriteDate(LFileName));
          end;
          LFileNameObject := TYieldModelDataObject(FAppModules.Model.ModelData).CastFileNamesObject.HydrologyFileNames.FindFile(LFileName);
          if (LFileNameObject <> nil) and (not LFileNameObject.SavedInDB) then
          begin
            LMesg := FAppModules.Language.GetString('SelectedFile.ImportFile');
            if (MessageDlg(LMesg,mtConfirmation,[mbYes, mbNo],0) = mrYes) then
              FAppModules.Model.ProcessEvent(CmeImportHydrologyFile,LFileNameObject);
          end;
          FUnitRunoffFileName := LValue;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'UnitRunoffFileName',LOldValue,LValue);
        end;
      finally
        FreeAndNil(LLoadAgent);
        FreeAndNil(LContextData);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TStreamFlowReduction.Set_SoilMoistureFileName(const Value: WideString);
const OPNAME = 'TStreamFlowReduction.Set_SoilMoistureFileName';
var
  LLoadAgent    : TStreamFlowReductionSQLAgent;
  LContextData  : TStringList;
  LValue,
  LOldValue     : string;
  LMesg         : string;
  LFileName     : string;
  LFileNameObject: TAbstractModelFileName;
  LNewFileIndex: integer;
begin
  try
    LValue := ExtractFileName(Value);
    if (FSoilMoistureFileName <> LValue) then
    begin
      LContextData  := TStringList.Create;
      LLoadAgent    := TStreamFlowReductionSQLAgent.Create(FAppModules);
      try
        LOldValue := FSoilMoistureFileName;
        LLoadAgent.LoadContextData(LContextData, IntToStr(FIdentifier));
        if FAppModules.FieldProperties.UpdateFieldValue('SoilMoistureFileName', LValue, LOldValue, LContextData ) then
        begin
          LFileName     := TYieldModelDataObject(FAppModules.Model.ModelData).CastFileNamesObject.HydrologyFilesPath+LValue;
          LFileNameObject := TYieldModelDataObject(FAppModules.Model.ModelData).CastFileNamesObject.HydrologyFileNames.FindFile(LFileName);
          if (LFileNameObject = nil) then
          begin
            LNewFileIndex := TYieldModelDataObject(FAppModules.Model.ModelData).CastFileNamesObject.HydrologyFileNames.Count+1;
            TYieldModelDataObject(FAppModules.Model.ModelData).CastFileNamesObject.AddHydrologyFileName(LNewFileIndex,LFileName,False,0.0,
              FileLastWriteDate(LFileName));
          end;
          LFileNameObject := TYieldModelDataObject(FAppModules.Model.ModelData).CastFileNamesObject.HydrologyFileNames.FindFile(LFileName);
          if (LFileNameObject <> nil) and (not LFileNameObject.SavedInDB) then
          begin
            LMesg := FAppModules.Language.GetString('SelectedFile.ImportFile');
            if (MessageDlg(LMesg,mtConfirmation,[mbYes, mbNo],0) = mrYes) then
              FAppModules.Model.ProcessEvent(CmeImportHydrologyFile,LFileNameObject);
          end;
          FSoilMoistureFileName := LValue;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'SoilMoistureFileName',LOldValue,LValue);
        end;
      finally
        FreeAndNil(LLoadAgent);
        FreeAndNil(LContextData);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TStreamFlowReduction.Set_SFRName(const Value: WideString);
const OPNAME = 'TStreamFlowReduction.Set_SFRName';
var
  LLoadAgent    : TStreamFlowReductionSQLAgent;
  LContextData  : TStringList;
  LOldValue     : string;
begin
  try
    if FSFRName <> Value then
    begin
      LContextData  := TStringList.Create;
      LLoadAgent    := TStreamFlowReductionSQLAgent.Create(FAppModules);
      try
        LOldValue := FSFRName;
        LLoadAgent.LoadContextData(LContextData, IntToStr(FIdentifier));
        if FAppModules.FieldProperties.UpdateFieldValue('SFRName', Value, LOldValue, LContextData ) then
        begin
          FSFRName := Value;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'SFRName',LOldValue,Value);
        end;
      finally
        FreeAndNil(LLoadAgent);
        FreeAndNil(LContextData);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TStreamFlowReduction.Set_SFRDescription(const Value: WideString);
const OPNAME = 'TStreamFlowReduction.Set_SFRDescription';
var
  LLoadAgent    : TStreamFlowReductionSQLAgent;
  LContextData  : TStringList;
  LOldValue     : string;
begin
  try
    if FSFRDescription <> Value then
    begin
      LContextData  := TStringList.Create;
      LLoadAgent    := TStreamFlowReductionSQLAgent.Create(FAppModules);
      try
        LOldValue := FSFRDescription;
        LLoadAgent.LoadContextData(LContextData, IntToStr(FIdentifier));
        if FAppModules.FieldProperties.UpdateFieldValue('SFRDescr', Value, LOldValue, LContextData ) then
        begin
          FSFRDescription := Value;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'SFRDescr',LOldValue,Value);
        end;
      finally
        FreeAndNil(LLoadAgent);
        FreeAndNil(LContextData);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TStreamFlowReduction.Validate(var AErrors: WideString; const AContext: WideString): WordBool;
const OPNAME = 'TStreamFlowReduction.Validate';
var
  LErrorMessages : TStringList;
  LStopOnFirstError : Boolean;
begin
  Result := True;
  try
    LErrorMessages := TStringList.Create;
    try
      if (AContext = 'InflowNodeNumber') then
        Result := ValidateInflowNodeNumber(LErrorMessages)
      else
      if (AContext = 'CoveredArea') then
        Result := ValidateCoveredArea(LErrorMessages)
      else
      if (AContext = 'UnitRunoffFileName') then
        Result := ValidateUnitRunoffFileName(LErrorMessages)
      else
      if (AContext = 'SoilMoistureFileName') then
        Result := ValidateSoilMoistureFileName(LErrorMessages)
      else
      if (AContext = 'SFRName') then
        Result := ValidateSFRName(LErrorMessages)
      else
      if (AContext = 'SFRDescr') then
        Result := ValidateSFRDescription(LErrorMessages)
      else
      begin
        lStopOnFirstError := FAppModules.GlobalData.StopOnFirstErr;
        if not ValidateInflowNodeNumber(LErrorMessages) then
          Result := False;
        if (not Result) and lStopOnFirstError then
        begin
          AErrors := AErrors + LErrorMessages.Text;
          Exit;
        end;

        if not ValidateCoveredArea(LErrorMessages) then
          Result := False;
        if (not Result) and lStopOnFirstError then
        begin
          AErrors := AErrors + LErrorMessages.Text;
          Exit;
        end;

        if not ValidateUnitRunoffFileName(LErrorMessages) then
          Result := False;
        if (not Result) and lStopOnFirstError then
        begin
          AErrors := AErrors + LErrorMessages.Text;
          Exit;
        end;

        if not ValidateSoilMoistureFileName(LErrorMessages) then
          Result := False;
        if (not Result) and lStopOnFirstError then
        begin
          AErrors := AErrors + LErrorMessages.Text;
          Exit;
        end;

        if not ValidateSFRName(LErrorMessages) then
          Result := False;
        if (not Result) and lStopOnFirstError then
        begin
          AErrors := AErrors + LErrorMessages.Text;
          Exit;
        end;

        if not ValidateSFRDescription(LErrorMessages) then
          Result := False;
        if (not Result) and lStopOnFirstError then
        begin
          AErrors := AErrors + LErrorMessages.Text;
          Exit;
        end;
      end;

      AErrors := AErrors + LErrorMessages.Text;
    finally
      LErrorMessages.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;

end;

function TStreamFlowReduction.ValidateInflowNodeNumber(AErrorMessages: TStrings): Boolean;
const OPNAME = 'TStreamFlowReduction.ValidateInflowNodeNumber';
var
  lMessage : string;
begin
  Result := False;
  try
    lMessage := '';
    if (not FAppModules.FieldProperties.ValidateFieldProperty('InflowNodeNumber', IntToStr(FInflowNodeNumber), lMessage)) then
      AErrorMessages.Add('ERROR:' +IntToStr(FInflowNodeNumber)+ ':'+lMessage)
    else
      Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TStreamFlowReduction.ValidateCoveredArea(AErrorMessages: TStrings): Boolean;
const OPNAME = 'TStreamFlowReduction.ValidateCoveredArea';
var
  LNode : IReservoirData;
  LParamReference : IParamReference;
  lMessage1 : string;
  lMessage : WideString;
begin
  Result := False;
  try
    lMessage := '';
    if (not FAppModules.FieldProperties.ValidateFieldProperty('CoveredArea', FloatToStr(FCoveredArea), lMessage1)) then
      AErrorMessages.Add('ERROR:' +FloatToStr(FCoveredArea)+ ':'+lMessage1)
    else
    begin
      LNode := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.ReservoirList.ReservoirOrNodeByIdentifier[FInflowNodeNumber];
      if(LNode <> nil) and (LNode.ReservoirConfigurationData.CatchmentRef <> 0) then
      begin
        LParamReference := TYieldModelDataObject(FAppModules.Model.ModelData).ParamSetup.
                           ReferenceDataByCatchNumber[LNode.ReservoirConfigurationData.CatchmentRef];
        if(LParamReference <> nil) then
        begin
          LParamReference.Validate(lMessage,'CatchmentAreaParam');
          if(lMessage <> '') then
           AErrorMessages.Add(lMessage);
        end;
      end;
    end;
    Result := lMessage = '';
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TStreamFlowReduction.ValidateUnitRunoffFileName(AErrorMessages: TStrings): Boolean;
const OPNAME = 'TStreamFlowReduction.ValidateUnitRunoffFileName';
var
  lMessage : string;
begin
  Result := False;
  try
    lMessage := '';
    if (not FAppModules.FieldProperties.ValidateFieldProperty('UnitRunoffFileName', FUnitRunoffFileName, lMessage)) then
      AErrorMessages.Add('ERROR:' +FUnitRunoffFileName+ ':'+lMessage)
    else
      Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TStreamFlowReduction.ValidateSoilMoistureFileName(AErrorMessages: TStrings): Boolean;
const OPNAME = 'TStreamFlowReduction.ValidateSoilMoistureFileName';
var
  lMessage : string;
begin
  Result := False;
  try
    lMessage := '';
    if (not FAppModules.FieldProperties.ValidateFieldProperty('SoilMoistureFileName', FSoilMoistureFileName, lMessage)) then
      AErrorMessages.Add('ERROR:' +FSoilMoistureFileName+ ':'+lMessage)
    else
      Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TStreamFlowReduction.ValidateSFRName(AErrorMessages: TStrings): Boolean;
const OPNAME = 'TStreamFlowReduction.ValidateSFRName';
var
  LIndex : integer;
  LStreamFlowReduction:TStreamFlowReduction;
  lMessage : string;
begin
  Result := True;
  try
    lMessage := '';
    if (not FAppModules.FieldProperties.ValidateFieldProperty('SFRName', FSFRName, lMessage)) then
      AErrorMessages.Add('WARNING:' +FSFRName + ':'+lMessage)
    else
    begin
      for LIndex := 0 to TYieldModelDataObject(FAppModules.Model.ModelData).CastNetworkFeaturesData.CastStreamFlowReductionList.StreamFlowReductionCount-1 do
      begin
        LStreamFlowReduction := TYieldModelDataObject(FAppModules.Model.ModelData).CastNetworkFeaturesData.CastStreamFlowReductionList.CastStreamFlowReductionByIndex(LIndex);
        if(LStreamFlowReduction.Identifier = Self.Identifier) then Continue;
        if(UpperCase(Self.SFRName) = UpperCase(LStreamFlowReduction.SFRName)) then
        begin
          lMessage := FAppModules.Language.GetString('ContextValidation.DuplicateSFRName');
          AErrorMessages.Add('WARNING:' +lMessage);
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TStreamFlowReduction.ValidateSFRDescription(AErrorMessages: TStrings): WordBool;
const OPNAME = 'TStreamFlowReduction.ValidateSFRDescription';
var
  lMessage : string;
begin
  Result := True;
  try
    lMessage := '';
    if (not FAppModules.FieldProperties.ValidateFieldProperty('SFRDescr', FSFRDescription, lMessage)) then
      AErrorMessages.Add('WARNING:' +FSFRDescription + ':'+lMessage);
    //else
    //  Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TStreamFlowReduction.Get_SoilMoistureFileData: WideString;
const OPNAME = 'TStreamFlowReduction.Get_SoilMoistureFileData';
begin
  Result := '';
  try
    Result := (FAppModules.Model.ModelData as IYieldModelData).HydrologyFileData[SoilMoistureFileName];
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TStreamFlowReduction.Get_UnitRunoffFileData: WideString;
const OPNAME = 'TStreamFlowReduction.Get_UnitRunoffFileData';
begin
  Result := '';
  try
    Result := (FAppModules.Model.ModelData as IYieldModelData).HydrologyFileData[UnitRunoffFileName];
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TStreamFlowReduction.Get_InflowNodeDetails: IReservoirData;
const OPNAME = 'TStreamFlowReduction.Get_InflowNodeDetails';
begin
  Result := nil;
  try
    Result := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.ReservoirList.ReservoirByIdentifier[FInflowNodeNumber];
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TStreamFlowReduction.Assign(AStreamFlowReduction: TStreamFlowReduction);
const OPNAME = 'TStreamFlowReduction.Assign';
begin
  try
    CoveredArea           := AStreamFlowReduction.CoveredArea;
    UnitRunoffFileName    := AStreamFlowReduction.UnitRunoffFileName;
    SoilMoistureFileName  := AStreamFlowReduction.SoilMoistureFileName;
    SFRName               := 'Copy of '+AStreamFlowReduction.SFRName;
    SFRDescription        := AStreamFlowReduction.SFRDescription;
    InflowNodeNumber      := AStreamFlowReduction.InflowNodeNumber;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

{ TStreamFlowReductionList }

function TStreamFlowReductionList._AddRef: Integer;
const OPNAME = 'TStreamFlowReductionList._AddRef';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TStreamFlowReductionList._Release: Integer;
const OPNAME = 'TStreamFlowReductionList._Release';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TStreamFlowReductionList.CreateMemberObjects;
const OPNAME = 'TStreamFlowReductionList.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
    FStreamFlowReductionList := TObjectList.Create(True);
 except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TStreamFlowReductionList.DestroyMemberObjects;
const OPNAME = 'TStreamFlowReductionList.DestroyMemberObjects';
begin
  try
    FreeAndNil(FStreamFlowReductionList);
    inherited;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TStreamFlowReductionList.Initialise: Boolean;
const OPNAME = 'TStreamFlowReductionList.Initialise';
begin
  Result := inherited Initialise;
  try
    FStreamFlowReductionList.Clear;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TStreamFlowReductionList.Get_StreamFlowReductionCount: Integer;
const OPNAME = 'TStreamFlowReductionList.Get_StreamFlowReductionCount';
begin
  Result := 0;
  try
    Result := FStreamFlowReductionList.Count;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TStreamFlowReductionList.CastStreamFlowReductionByID(AStreamFlowReductionID: Integer): TStreamFlowReduction;
const OPNAME = 'TStreamFlowReductionList.CastStreamFlowReductionByID';
var
 LIndex   : Integer;
 LStreamFlowReduction : TStreamFlowReduction;
begin
  Result := nil;
  try
    for LIndex := 0 to FStreamFlowReductionList.Count -1 do
    begin
      LStreamFlowReduction := TStreamFlowReduction(FStreamFlowReductionList.Items[LIndex]);
      if (LStreamFlowReduction.FIdentifier = AStreamFlowReductionID) then
      begin
        Result := LStreamFlowReduction;
        Break;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TStreamFlowReductionList.CastStreamFlowReductionByIndex(AIndex: Integer): TStreamFlowReduction;
const OPNAME = 'TStreamFlowReductionList.CastStreamFlowReductionByIndex';
begin
  Result := nil;
  try
    if (AIndex >= 0) and (AIndex < FStreamFlowReductionList.Count) then
      Result := TStreamFlowReduction(FStreamFlowReductionList.Items[AIndex]);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TStreamFlowReductionList.Get_StreamFlowReductionByID(AStreamFlowReductionID: integer): IStreamFlowReduction;
const OPNAME = 'TStreamFlowReductionList.Get_StreamFlowReductionByID';
begin
  Result := nil;
  try
    Result := CastStreamFlowReductionByID(AStreamFlowReductionID);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TStreamFlowReductionList.Get_StreamFlowReductionByIndex(AIndex: integer): IStreamFlowReduction;
const OPNAME = 'TStreamFlowReductionList.Get_StreamFlowReductionByIndex';
begin
  Result := nil;
  try
    Result := CastStreamFlowReductionByIndex(AIndex);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TStreamFlowReductionList.NewStreamFlowReduction: TStreamFlowReduction;
const OPNAME = 'TStreamFlowReductionList.NewStreamFlowReduction';
begin
  Result := nil;
  try
    Result := TStreamFlowReduction.Create(FAppModules);
    FStreamFlowReductionList.Add(Result);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

{function TStreamFlowReductionList.CastStreamFlowReductionByInflowNodeNumber(AInflowNodeNumber: integer): TStreamFlowReduction;
const OPNAME = 'TStreamFlowReductionList.CastStreamFlowReductionByInflowNodeNumber';
var
  LIndex: integer;
begin
  Result := nil;
  try
    for LIndex := 0 to FStreamFlowReductionList.Count -1 do
    begin
      if(TStreamFlowReduction(FStreamFlowReductionList[LIndex]).InflowNodeNumber = AInflowNodeNumber) then
      begin
        Result := TStreamFlowReduction(FStreamFlowReductionList[LIndex]);
        Break;
      end
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;}

{function TStreamFlowReductionList.Get_StreamFlowReductionByInflowNodeNumber(AInflowNodeNumber: integer): TStreamFlowReduction;
const OPNAME = 'TStreamFlowReductionList.Get_StreamFlowReductionByIndex';
begin
  Result := nil;
  try
    Result := CastStreamFlowReductionByInflowNodeNumber(AInflowNodeNumber);
  except on E: Exception do HandleError(E, OPNAME); end;
end;}

function TStreamFlowReductionList.CreateStreamFlowReduction: TStreamFlowReduction;
const OPNAME = 'TStreamFlowReductionList.CreateStreamFlowReduction';
var
  LLoadAgent : TStreamFlowReductionSQLAgent;
begin
  Result := nil;
  try
    LLoadAgent := TStreamFlowReductionSQLAgent.Create(FAppModules);
    try
      Result := TStreamFlowReduction.Create(FAppModules);
      try
        Result.Initialise;
        Result.FIdentifier      := LLoadAgent.GetMaxIdentifier + 1;
        Result.FSFRName         := IntToStr(Result.FIdentifier) + ' SFR';
        Result.FSFRDescription  := 'SFR ' + IntToStr(Result.FIdentifier);
        if LLoadAgent.InsertStreamFlowReduction(Result) then
          FStreamFlowReductionList.Add(Result)
        else
          FreeAndNil(Result);
      except on E: Exception do
        FreeAndNil(Result);
      end;
    finally
      FreeAndNil(LLoadAgent);
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TStreamFlowReductionList.CopyCreate(AStreamFlowReductionID : integer) : IStreamFlowReduction; safecall;
const OPNAME = 'TStreamFlowReductionList.CopyCreate';
var
  LDestStreamFlowReduction : TStreamFlowReduction;
  LSourceStreamFlowReduction : TStreamFlowReduction;
begin
  Result := nil;
  try
    LSourceStreamFlowReduction := CastStreamFlowReductionByID(AStreamFlowReductionID);
    if LSourceStreamFlowReduction <> nil then
    begin
      LDestStreamFlowReduction := CreateStreamFlowReduction;
      if LDestStreamFlowReduction <> nil then
      begin
        LDestStreamFlowReduction.Assign(LSourceStreamFlowReduction);
        Result := LDestStreamFlowReduction;
      end;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TStreamFlowReductionList.RemoveStreamFlowReduction(AStreamFlowReductionID: integer): WordBool;
const OPNAME = 'TStreamFlowReductionList.RemoveStreamFlowReduction';
var
  lLoadAgent             : TStreamFlowReductionSQLAgent;
  LStreamFlowReduction   : TStreamFlowReduction;
begin
  Result   := False;
  try
    LStreamFlowReduction :=  CastStreamFlowReductionByID(AStreamFlowReductionID);
    if(LStreamFlowReduction = nil) then   Exit;
    LLoadAgent := TStreamFlowReductionSQLAgent.Create(FAppModules);
    try
      if LLoadAgent.DeleteStreamFlowReduction(LStreamFlowReduction) then
      begin
        FStreamFlowReductionList.Remove(LStreamFlowReduction);
        Result := True;
      end;
    finally
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TStreamFlowReductionList.Validate(var AErrors: WideString; const AContext: WideString): WordBool;
const OPNAME = 'TStreamFlowReductionList.Validate';
var
  LIndex: integer;
begin
  Result := True;
  try
    if (FAppModules.StudyArea.ModelVersion <> '7') then
      Exit;
    for LIndex := 0 to FStreamFlowReductionList.Count -1 do
    begin
      if not TStreamFlowReduction(FStreamFlowReductionList[LIndex]).Validate(AErrors,AContext) then
      begin
        Result := False;
        if FAppModules.GlobalData.StopOnFirstErr then
          Break;
      end
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TStreamFlowReductionList.StreamFlowReductionIDsPerInflowNode(AInflowNodeNumber: Integer): WideString;
const OPNAME = 'TStreamFlowReductionList.StreamFlowReductionIDsPerInflowNode';
var
  LIndex: integer;
  LNodeNumbers: TStringList;
begin
  Result := '';
  try
    if (FAppModules.StudyArea.ModelVersion <> '7') then
      Exit;
    LNodeNumbers := TStringList.Create;
    try
      for LIndex := 0 to FStreamFlowReductionList.Count -1 do
      begin
        if (TStreamFlowReduction(FStreamFlowReductionList[LIndex]).InflowNodeNumber = AInflowNodeNumber) then
        begin
          LNodeNumbers.Add(IntToStr(TStreamFlowReduction(FStreamFlowReductionList[LIndex]).FIdentifier));
        end
      end;
      Result := LNodeNumbers.CommaText;
    finally
      LNodeNumbers.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TStreamFlowReductionList.GetStreamFlowReductionsPerInflowNodeNumber(AInflowNodeNumber: integer;
         AStreamFlowReductionsList: TObjectList): boolean;
const OPNAME = 'TStreamFlowReductionList.GetStreamFlowReductionsPerInflowNodeNumber';
var
  LIndex: integer;
begin
  Result := False;
  try
    AStreamFlowReductionsList.Clear;
    for LIndex := 0 to FStreamFlowReductionList.Count -1 do
    begin
      if (TStreamFlowReduction(FStreamFlowReductionList[LIndex]).InflowNodeNumber = AInflowNodeNumber) then
      begin
        AStreamFlowReductionsList.Add(FStreamFlowReductionList[LIndex]);
      end
    end;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

end.



