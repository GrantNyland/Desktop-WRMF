unit ULoadGeneration;

interface
  uses
    Classes,
    SysUtils,
    VoaimsCom_TLB,
    UAbstractObject;
type
  TLoadGeneration = class(TAbstractAppObject, ILoadGeneration)
    protected
      Fidentifier : integer;
      FMineIdentifier : integer;
      FOpenCastIdentifier : integer;
      FUnderGroundIdentifier : integer;
      FSlurryDumpIdentifier : integer;
      FLoadGenerationType : integer;
      FDescription : string;
      FSTDDeviation : double;
      FFlow : TElevationsArray;
      FMeanOfSalt : TElevationsArray;
      procedure CreateMemberObjects; override;
      procedure DestroyMemberObjects; override;

      function Get_FlowByIndex(AIndex: Integer): Double; safecall;
      procedure Set_FlowByIndex(AIndex: Integer; Value: Double); safecall;
      function Get_MeanOfSaltByIndex(AIndex: Integer): Double; safecall;
      procedure Set_MeanOfSaltByIndex(AIndex: Integer; Value: Double); safecall;
      function Get_StandardDeviation: Double; safecall;
      procedure Set_StandardDeviation(Value: Double); safecall;
      function Get_type_: Integer; safecall;
      function ValidateStandardDeviation(AErrorMessages: TStrings): wordbool;
      function ValidateFlow(AErrorMessages, AErrorColumns: TStrings): wordbool;
      function ValidateMeanOfSalt(AErrorMessages, AErrorColumns: TStrings): wordbool;
    public
      procedure Populate(AIdentifier,AMineIdentifier, AOpenCastIdentifier, AUnderGroundIdentifier, ASlurryDumpIdentifier,
                         ALoadGenerationType: integer; AStandardDeviation: double; AFlow, AMeanOfSalt: TElevationsArray; ADescription: String);
      function Validate(var AErrors: WideString; const AContext: WideString): WordBool; safecall;
      property FlowByIndex[AIndex: Integer]: Double read Get_FlowByIndex write Set_FlowByIndex;
      property MeanOfSaltByIndex[AIndex: Integer]: Double read Get_MeanOfSaltByIndex write Set_MeanOfSaltByIndex;
      property StandardDeviation: Double read Get_StandardDeviation write Set_StandardDeviation;
      property type_: Integer read Get_type_;
      property Identifier : integer read Fidentifier write Fidentifier;
      property MineIdentifier : integer read FMineIdentifier;
      property OpenCastIdentifier : integer read FOpenCastIdentifier;
      property UnderGroudIdentifier : integer read FUnderGroundIdentifier;
      property SlurryDumpIdentifier : integer read FSlurryDumpIdentifier;
      property Description: string read FDescription;
  end;

implementation
uses
  UPlanningMineSQLAgent,
  UErrorHandlingOperations;

procedure TLoadGeneration.CreateMemberObjects;
const OPNAME ='TLoadGeneration.CreateMemberObjects';
var
  LMeanOfSalt : TAbstractFieldProperty;
  LFlow : TAbstractFieldProperty;
begin
   try
    inherited CreateMemberObjects;
    LMeanOfSalt := FAppModules.FieldProperties.FieldProperty('MeanOfSalt');
    if not Assigned(LMeanOfSalt) then
      raise Exception.Create('Field (MeanOfSalt) not found in field properties');
    SetLength(FMeanOfSalt,LMeanOfSalt.ArrayLength-1);

    LFlow := FAppModules.FieldProperties.FieldProperty('Flow');
    if not Assigned(LFlow) then
      raise Exception.Create('Field (Flow) not found in field properties');
    SetLength( FFlow,LFlow.ArrayLength-1);
  except on E: Exception do HandleError(E,OPNAME); end;
end;

procedure TLoadGeneration.DestroyMemberObjects;
const OPNAME = 'TLoadGeneration.DestroyMemberObjects';
begin
  inherited;

  Finalize(FFlow);
  Finalize(FMeanOfSalt);
end;

procedure TLoadGeneration.Populate(AIdentifier,AMineIdentifier: Integer; AOpenCastIdentifier: Integer; AUnderGroundIdentifier: Integer; ASlurryDumpIdentifier, ALoadGenerationType: Integer; AStandardDeviation: double; AFlow: TElevationsArray; AMeanOfSalt: TElevationsArray; ADescription: String);
const OPNAME = 'TLoadGeneration.Populate';
var
  LIndex : integer;
begin
  try
    Fidentifier := AIdentifier;
    FMineIdentifier := AMineIdentifier;
    FOpenCastIdentifier := AOpenCastIdentifier;
    FUnderGroundIdentifier := AUnderGroundIdentifier;
    FSlurryDumpIdentifier := ASlurryDumpIdentifier;
    FDescription := ADescription;
    FLoadGenerationType := ALoadGenerationType;
    FSTDDeviation := AStandardDeviation;
    for LIndex := 0 to High(FFlow) do
    begin
      FFlow[LIndex] := AFlow[LIndex];
      FMeanOfSalt[LIndex] := AMeanOfSalt[LIndex];
    end;
  except on E: Exception do HandleError(E,OPNAME); end;
end;

function TLoadGeneration.Get_FlowByIndex(AIndex: Integer): Double;
const OPNAME = 'TLoadGeneration.Get_FlowByIndex';
begin
  Result := 0.0;
  try
    if((AIndex >= 0) AND (AIndex <= HIGH(FFlow))) then
      Result := FFlow[AIndex];
  except on E: Exception do HandleError(E,OPNAME); end;
end;

procedure TLoadGeneration.Set_FlowByIndex(AIndex: Integer; Value: Double);
const OPNAME = 'TLoadGeneration.Set_FlowByIndex';
var
  LSQLAgent : TPlanningMineSQLAgent;
  LContextData : TStringList;
  LOldValue : string;
begin
  try
    LSQLAgent := TPlanningMineSQLAgent.Create(FAppModules);
    LContextData := TStringList.Create;
    {case FLoadGenerationType of
      1: LSQLAgent.LoadContextData_LoadGenerationOpenCast(LContextData,FMineIdentifier,FOpenCastIdentifier, FLoadGenerationType, IntToStr(AIndex+1));
    end;}
    LSQLAgent.LoadContextData_LoadGeneration(LContextData,Fidentifier, FMineIdentifier, FOpenCastIdentifier, FUnderGroundIdentifier,FSlurryDumpIdentifier,AIndex);

    try
      if (FAppModules.FieldProperties.UpdateFieldValue('Flow',FloatToStr(Value),FloatToStr(FFlow[AIndex]), LContextData)) then
      begin
        LOldValue := FloatToStr(FFlow[AIndex]);
        FFlow[AIndex] := Value;
        FAppModules.Model.StudyDataHasChanged(sdccEdit,'Flow',LOldValue,FloatToStr(FFlow[AIndex]));
      end;
    finally
      FreeAndNil(LSQLAgent);
      FreeAndNil(LContextData);
    end;
  except on E: Exception do HandleError(E,OPNAME); end;
end;

function TLoadGeneration.Get_MeanOfSaltByIndex(AIndex: Integer): double;
const OPNAME = 'TLoadGeneration.Get_MeanOfSaltByIndex';
begin
  Result := 0.0;
  try
    if ((AIndex >= 0) and (AIndex <= High(FMeanOfSalt))) then
      Result := FMeanOfSalt[AIndex];
  except on E: Exception do HandleError(E,OPNAME); end;
end;

procedure TLoadGeneration.Set_MeanOfSaltByIndex(AIndex: Integer; Value: Double);
const OPNAME = 'TLoadGeneration.Set_MeanOfSaltByIndex';
var
  LSQLAgent : TPlanningMineSQLAgent;
  LContextData  : TStringList;
  LOldValue : string;
begin
  try
    LSQLAgent := TPlanningMineSQLAgent.Create(FAppModules);
    LContextData := TStringList.Create;
    {case FLoadGenerationType of
      1,2: LSQLAgent.LoadContextData_LoadGenerationOpenCast(LContextData,FMineIdentifier,FOpenCastIdentifier, FLoadGenerationType, IntToStr(AIndex+1));
      3: LSQLAgent.LoadContextData_LoadGenerationUnderGround(LContextData,FMineIdentifier,FUnderGroundIdentifier,FLoadGenerationType,IntToStr(AIndex+1));
      4: LSQLAgent.LoadContextData_LoadGenerationSlurryDump(LContextData,FMineIdentifier,FSlurryDumpIdentifier,FLoadGenerationType,IntToStr(AIndex+1));
      end;}
      LSQLAgent.LoadContextData_LoadGeneration(LContextData,Fidentifier,FMineIdentifier,FOpenCastIdentifier,FUnderGroundIdentifier,FSlurryDumpIdentifier,AIndex);

      try
        if (FAppModules.FieldProperties.UpdateFieldValue('MeanOfSalt',FloatToStr(Value),FloatToStr(FMeanOfSalt[AIndex]),LContextData)) then
        begin
          LOldValue := FloatToStr(FMeanOfSalt[AIndex]);
          FMeanOfSalt[AIndex] := Value;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'MeanOfSalt',LOldValue,FloatToStr(FMeanOfSalt[AIndex]));
        end;
      finally
        FreeAndNil(LSQLAgent);
        FreeAndNil(LContextData);
      end;

  except on E: Exception do HandleError(E,OPNAME); end;
end;

function TLoadGeneration.Get_StandardDeviation: double;
const OPNAME = 'TLoadGeneration.Get_StandardDeviation';
begin
  Result := 0.0;
  try
    Result := FSTDDeviation;
  except on E: Exception do HandleError(E,OPNAME); end;
end;

procedure TLoadGeneration.Set_StandardDeviation(Value: Double);
const OPNAME = 'TLoadGeneration.Set_StandardDeviation';
var
  LSQLAgent : TPlanningMineSQLAgent;
  LContextData : TStringList;
  LOldValue : string;
begin
  try
    LSQLAgent := TPlanningMineSQLAgent.Create(FAppModules);
    LContextData := TStringList.Create;
    {case FLoadGenerationType of
      1,2: LSQLAgent.LoadContextData_LoadGenerationOpenCast(LContextData, FMineIdentifier, FOpenCastIdentifier, FLoadGenerationType);
      3: LSQLAgent.LoadContextData_LoadGenerationUnderGround(LContextData, FMineIdentifier,FUnderGroundIdentifier,FLoadGenerationType);
      4: LSQLAgent.LoadContextData_LoadGenerationSlurryDump(LContextData,FMineIdentifier,FSlurryDumpIdentifier,FLoadGenerationType);
    end;}
    LSQLAgent.LoadContextData_LoadGeneration(LContextData,Fidentifier,FMineIdentifier,FOpenCastIdentifier,FUnderGroundIdentifier,FSlurryDumpIdentifier);
    try
      if (FAppModules.FieldProperties.UpdateFieldValue('StdDeviation',FloatToStr(Value),FloatToStr(FSTDDeviation),LContextData)) then
      begin
        LOldValue := FloatToStr(FSTDDeviation);
        FSTDDeviation := Value;
        FAppModules.Model.StudyDataHasChanged(sdccEdit,'StdDeviation',LOldValue,FloatToStr(FSTDDeviation));
      end;
    finally
      FreeAndNil(LSQLAgent);
      FreeAndNil(LContextData);
    end;
  except on E: Exception do HandleError(E,OPNAME); end;
end;

function TLoadGeneration.Validate(var AErrors: WideString;
  const AContext: WideString): WordBool;
const OPNAME = 'TLoadGeneration.Validate';
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
      if(AContext = 'StdDeviation') then
        Result := ValidateStandardDeviation(LErrorMsgs)
      else if (AContext = 'Flow' ) then
        Result := ValidateFlow(LErrorMsgs,LErrorCols)
      else if (AContext = 'MeanOfSalt' ) then
        Result := ValidateMeanOfSalt(LErrorMsgs,LErrorCols);

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
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TLoadGeneration.ValidateFlow(AErrorMessages,
  AErrorColumns: TStrings): wordbool;
const OPNAME = 'TLoadGeneration.ValidateFlow';
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
    LFieldProperties := FAppModules.FieldProperties.FieldProperty('Flow');
    for LIndex := LFieldProperties.ArrayLow to LFieldProperties.ArrayHigh do
    begin
      if(not (FAppModules.FieldProperties.ValidateFieldProperty('Flow',FloatToStr(FFlow[LIndex]),
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

function TLoadGeneration.ValidateMeanOfSalt(AErrorMessages,
  AErrorColumns: TStrings): wordbool;
const OPNAME = 'TLoadGeneration.ValidateMeanOfSalt';
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
    LFieldProperties := FAppModules.FieldProperties.FieldProperty('MeanOfSalt');
    for LIndex := LFieldProperties.ArrayLow to LFieldProperties.ArrayHigh do
    begin
      if(not (FAppModules.FieldProperties.ValidateFieldProperty('MeanOfSalt',FloatToStr(FMeanOfSalt[LIndex]),
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

function TLoadGeneration.ValidateStandardDeviation(
  AErrorMessages: TStrings): wordbool;
const OPNAME = 'TLoadGeneration.ValidateStandardDeviation';
var
  lMessage : string;
begin
  Result := False;
  try
      if (not (FAppModules.FieldProperties.ValidateFieldProperty('StdDeviation',FloatToStr(FSTDDeviation),lMessage))) then
        AErrorMessages.Add(lMessage)
      else
        Result := true;
  except on E: Exception do HandleError(E, OPNAME)  end;
end;

function TLoadGeneration.Get_type_: integer;
const OPNAME = 'TLoadGeneration.Get_type_';
begin
  try
    Result:= FLoadGenerationType;
  except on E: Exception do HandleError(E,OPNAME); end;
end;

end.
