unit UPlanningMineGrowthFactor;

interface
uses
  System.Generics.Collections,
  Classes,
  Contnrs,
  VoaimsCom_TLB,
  UAbstractObject;

type
  TPlanningMineGrowthFactor = class(TAbstractAppObject,IPlanningMineGrowthFactor)
  private
    FMine, FOpenCast, FSlurryDump, FUnderground: integer;
    FNoOfPoints : Integer;
    FFactorType : integer;
    FInterpolationMethod : integer;
    FNoOfYears : string;
    FGrowthFactorsString: string;
    FGrowthFactors: TList<double>;
    FYears: TList<Integer>;
    FIdentifier : integer;
    FDescription : string;

  protected
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;

    function Get_NoOfPoints: Integer; safecall;
    procedure Set_NoOfPoints(Value: Integer); safecall;
    function Get_NoOfYearsByIndex(AIndex: Integer): Integer; safecall;
    procedure Set_NoOfYearsByIndex(AIndex: Integer; Value: Integer); safecall;
    function Get_InterpolationMethod: Integer; safecall;
    procedure Set_InterpolationMethod(Value: Integer); safecall;
    function Get_GrowthFactorByIndex(AIndex: Integer): Double; safecall;
    procedure Set_GrowthFactorByIndex(AIndex: Integer; Value: Double); safecall;
    function ValidateNoOfPoints(AErrorMessages: TStrings): wordbool;
    function ValidateInterpolation(AErrorMessages: TStrings): wordbool;
    function ValidateNoOfYears(AErrorMessages, AErrorColumns: TStrings): wordbool;
    function ValidateGrowthFactor(AErrorMessages, AErrorColumns: Tstrings): wordbool;


  public
    function Initialise: boolean; override;
    procedure Populate( AIdentifier, AMineID, AOpenCastID, ASlurryDumpID, AUnderground, ANoOfPoints, AFactorType,
                        AInterpolation: integer; ANoOfYears, AGrowthFactors, ADescription: string);
    function Validate(var AError: WideString; const AContext: WideString): WordBool; safecall;
    property NoOfPoints: Integer read Get_NoOfPoints write Set_NoOfPoints;
    property NoOfYearsByIndex[AIndex: Integer]: Integer read Get_NoOfYearsByIndex write Set_NoOfYearsByIndex;
    property InterpolationMethod: Integer read Get_InterpolationMethod write Set_InterpolationMethod;
    property GrowthFactorByIndex[AIndex: Integer]: Double read Get_GrowthFactorByIndex write Set_GrowthFactorByIndex;
    property FactorType: integer read FFactorType;
    property Identifier: integer read FIdentifier write FIdentifier;
    property MineIdentifier: integer read FMine;
    property OpenCastIdentifier: integer read FOpenCast;
    property SlurryDumpIdentifier: integer read FSlurryDump;
    property UnderGroundIdentifier: integer read FUnderground;
    property Description: string read FDescription;
    function ListToStringGrowthFactor: string;
    function ListToStringYears: string;
  end;

implementation
uses
  System.DateUtils,
  SysUtils,
  UPlanningMineSQLAgent,
  UErrorHandlingOperations;
{ TPlanningMineGrowthFactor }

procedure TPlanningMineGrowthFactor.CreateMemberObjects;
const OPNAME = 'TPlanningMineGrowthFactor.CreateMemberObjects';
begin
  inherited;
  try
    FGrowthFactors := TList<double>.Create;
    FYears := TList<integer>.Create;
  except on E: Exception do HandleError(E,OPNAME) end;
end;

procedure TPlanningMineGrowthFactor.DestroyMemberObjects;
const OPNAME = 'TPlanningMineGrowthFactor.DestroyMemberObjects';
begin
  inherited;
  try
    FreeAndNil(FGrowthFactors);
    FreeAndNil(FYears);
  except on E: Exception do HandleError(E,OPNAME) end;
end;

function TPlanningMineGrowthFactor.Get_GrowthFactorByIndex(
  AIndex: Integer): Double;
const OPNAME = 'TPlanningMineGrowthFactor.Get_GrowthFactorByIndex';
begin
  Result := 0.0;
  try
    if ((AIndex >= 0) AND (AIndex < FGrowthFactors.Count)) then
      Result := double(FGrowthFactors[AIndex]);
  except on E: Exception do HandleError(E,OPNAME); end;
end;

function TPlanningMineGrowthFactor.Get_InterpolationMethod: integer;
const OPNAME = 'TPlanningMineGrowthFactor.Get_InterpolationMethod';
begin
  try
    Result := FInterpolationMethod;
  except on E: Exception do HandleError(E,OPNAME); end;
end;

function TPlanningMineGrowthFactor.Get_NoOfPoints: Integer;
const OPNAME = 'TPlanningMineGrowthFactor.Get_NoOfPoints';
begin
  try
    Result:= FNoOfPoints;
  except on E: Exception do HandleError(E,OPNAME); end;
end;

function TPlanningMineGrowthFactor.Get_NoOfYearsByIndex(
  AIndex: Integer): Integer;
const OPNAME = 'TPlanningMineGrowthFactor.Get_NoOfYearsByIndex';
begin
  Result := 0;
  try
    if ((AIndex >= 0) AND (AIndex < FYears.Count)) then
      Result := FYears[AIndex];
  except on E: Exception do HandleError(E,OPNAME); end;
end;

function TPlanningMineGrowthFactor.Initialise: boolean;
const OPNAME ='TPlanningMineGrowthFactor.Initialise';
begin
  Result := inherited Initialise;
  try
    FMine := 0;
    FOpenCast := 0;
    FSlurryDump := 0;
    FUnderground := 0;
    FNoOfPoints := 1;
    FFactorType := 0;
    FInterpolationMethod := 0;
    FNoOfYears := '0';
    FGrowthFactorsString := '1.0';
    Result:= true;
  except on E: Exception do HandleError(E,OPNAME); end;
end;

function TPlanningMineGrowthFactor.ListToStringGrowthFactor: string;
const OPNAME = 'TPlanningMineGrowthFactor.ListToStringGrowthFactor';
var
  LIndex: Integer;
begin
  Result := '';
  try
     for LIndex := 0 to FGrowthFactors.Count -1 do
      begin
       if LIndex = 0 then Result := FloatToStr(FGrowthFactors[LIndex])
       else
         Result := Result + ',' + FloatToStr(FGrowthFactors[LIndex]);
      end;
  except on E: Exception do HandleError(E,OPNAME); end;
end;

function TPlanningMineGrowthFactor.ListToStringYears;
const OPNAME = 'TPlanningMineGrowthFactor.ListToStringYears';
var
  LIndex: integer;
begin
  Result := '';
  try
    for LIndex := 0 to FYears.Count - 1 do
      begin
        if LIndex = 0 then Result := FloatToStr(FYears[LIndex])
        else
          Result := Result + ',' +  FloatToStr(FYears[LIndex]);

      end;
  except on E: Exception do HandleError(E,OPNAME); end;
end;

procedure TPlanningMineGrowthFactor.Populate(AIdentifier,AMineID, AOpenCastID,
  ASlurryDumpID, AUnderground, ANoOfPoints,AFactorType, AInterpolation: integer; ANoOfYears,
  AGrowthFactors,ADescription: string);
const OPNAME = 'TPlanningMineGrowthFactor.Populate';
var
  ListString : TStringList;
  LIndex : integer;
begin
  try
    FIdentifier := AIdentifier;
    FMine := AMineID;
    FOpenCast := AOpenCastID;
    FSlurryDump := ASlurryDumpID;
    FUnderground := AUnderground;
    FNoOfPoints := ANoOfPoints;
    FFactorType := AFactorType;
    FDescription := ADescription;
    FInterpolationMethod := AInterpolation;
    {
    case AInterpolation of
      1: FInterpolationMethod := false;
      2: FInterpolationMethod := true;
    end;
     }

    FNoOfYears := ANoOfYears;
    ListString := TStringList.Create;
    ListString.Clear;
    ListString.Delimiter := ',';
    ListString.StrictDelimiter := true;
    ListString.DelimitedText := ANoOfYears;
    for LIndex := 0 to ListString.Count -1 do
    begin
      FYears.Add(StrToInt(ListString[LIndex]));
    end;
    FGrowthFactorsString := AGrowthFactors;
    ListString.Clear;
    ListString.DelimitedText := AGrowthFactors;
    for LIndex  := 0 to ListString.Count -1 do
    begin
      FGrowthFactors.Add(StrToFloat(ListString[LIndex]));
    end;
  except on E: Exception do HandleError(E,OPNAME) end;
end;

procedure TPlanningMineGrowthFactor.Set_GrowthFactorByIndex(AIndex: Integer;
  Value: Double);
const OPNAME = 'TPlanningMineGrowthFactor.Set_GrowthFactorByIndex';
var
  LSQLAgent : TPlanningMineSQLAgent;
  LContextData : TStringList;
  LOldValue : string;
  LNewValue : string;
  LBackUp : double;
begin
  try
    LSQLAgent := TPlanningMineSQLAgent.Create(FAppModules);
    LContextData := TStringList.Create;
    LBackUp := 0.0;
    try
      {case FFactorType of
        1: LSQLAgent.LoadContextData_GrowthFactorMine(LContextData,FMine,FFactorType);
        2..7: LSQLAgent.LoadContextData_GrowthFactorOpenCast(LContextData,FMine, FOpenCast, FFactorType);
        8,9: LSQLAgent.LoadContextData_GrowthFactorUnderGround(LContextData,FMine,FUnderground, FFactorType);
        10: LSQLAgent.LoadContextData_GrowthFactorSlurryDump(LContextData,FMine,FSlurryDump,FFactorType);
      end;}
      LSQLAgent.LoadLoadContextData_GrowthFactor(LContextData,FIdentifier,FMine,FOpenCast,FUnderground,FSlurryDump);
      try
        if((AIndex >= 0) AND (AIndex <FGrowthFactors.Count )) then
        begin
          LBackup := FGrowthFactors[AIndex];
          FGrowthFactors[AIndex] :=  Value;
          LNewValue := ListToStringGrowthFactor;
          if (FAppModules.FieldProperties.UpdateFieldValue('GrowthFactors', LNewValue, FGrowthFactorsString, LContextData)) then
          begin
            LOldValue := FGrowthFactorsString;
            FGrowthFactorsString := LNewValue;
            FAppModules.Model.StudyDataHasChanged(sdccEdit,'GrowthFactors',LOldValue,FGrowthFactorsString);
          end;
        end;
      except on E: Exception do FGrowthFactors[AIndex] := LBackup;  end;
    finally
      FreeAndNil(LSQLAgent);
      FreeAndNil(LContextData);
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TPlanningMineGrowthFactor.Set_InterpolationMethod(Value: integer);
const OPNAME = 'TPlanningMineGrowthFactor.Set_InterpolationMethod';
var
  LSQLAgent : TPlanningMineSQLAgent;
  LContextData : TStringList;
  LOldValue : string;
begin
  try
    LSQLAgent := TPlanningMineSQLAgent.Create(FAppModules);
    LContextData := TStringList.Create;
    try
      {case FFactorType of
        1: LSQLAgent.LoadContextData_GrowthFactorMine(LContextData,FMine,FFactorType);
        2..7: LSQLAgent.LoadContextData_GrowthFactorOpenCast(LContextData,FMine,FOpenCast, FFactorType);
        8,9: LSQLAgent.LoadContextData_GrowthFactorUnderGround(LContextData,FMine,FUnderground,FFactorType);
        10: LSQLAgent.LoadContextData_GrowthFactorSlurryDump(LContextData, FMine, FSlurryDump, FFactorType);
      end;}
      {if (Value) then
        if(FAppModules.FieldProperties.UpdateFieldValue('InterpolationMethod','2',
          ConditionalOpp(FInterpolationMethod,1,0),LContextData)) then
          begin
            LOldValue := ConditionalOpp(FInterpolationMethod,1,0);
            FInterpolationMethod := Value;
            FAppModules.Model.StudyDataHasChanged(sdccEdit,'InterpolationMethod',LOldValue,ConditionalOpp(FInterpolationMethod,2,1));
          end
      else}
        LSQLAgent.LoadLoadContextData_GrowthFactor(LContextData, FIdentifier,FMine,FOpenCast,FUnderground,FSlurryDump);
        if(FAppModules.FieldProperties.UpdateFieldValue('InterpolationMethod',IntToStr(Value),IntToStr(FInterpolationMethod),LContextData)) then
          //ConditionalOpp(FInterpolationMethod,1,0),LContextData)) then
          begin
            LOldValue := IntToStr(FInterpolationMethod); //ConditionalOpp(FInterpolationMethod,1,0);
            FInterpolationMethod := Value;
            FAppModules.Model.StudyDataHasChanged(sdccEdit,'InterpolationMethod',LOldValue, IntToStr(FInterpolationMethod)); // {ConditionalOpp(FInterpolationMethod,2,1)}
          end;
    finally
      FreeAndNil(LSQLAgent);
      FreeAndNil(LContextData);
    end;
  except on E: Exception do HandleError(E,OPNAME); end;
end;

procedure TPlanningMineGrowthFactor.Set_NoOfPoints(Value: Integer);
const OPNAME = 'TPlanningMineGrowthFactor.Set_NoOfPoints';
var
  LSQLAgent : TPlanningMineSQLAgent;
  LContextData : TStringList;
  LOldValue : integer;
  LCount : integer;
  LDummyYear : integer;
begin
  try
    if Value <> FNoOfPoints then
    begin
      LSQLAgent := TPlanningMineSQLAgent.Create(FAppModules);
      LContextData := TStringList.Create;
      try
        LSQLAgent.LoadLoadContextData_GrowthFactor(LContextData,FIdentifier,FMine,FOpenCast,FUnderground,FSlurryDump);
        if(FAppModules.FieldProperties.UpdateFieldValue('NoOfPoints',IntToStr(Value), IntToStr(FNoOfPoints),LContextData)) then
        begin
          LOldValue := FNoOfPoints;
          FNoOfPoints := Value;
           LDummyYear := System.SysUtils.CurrentYear;
          if FNoOfPoints > LOldValue then
          begin
            for LCount := LOldValue to FNoOfPoints-1 do
            begin
              FYears.Add(LDummyYear);
              Set_NoOfYearsByIndex(FYears.Count-1, LDummyYear);
              FGrowthFactors.Add(1.0);
              Set_GrowthFactorByIndex(FGrowthFactors.Count-1,1.0);
              LDummyYear := LDummyYear + 1;
            end;
          end
          else
          begin
            for LCount := LOldValue-1 downto FNoOfPoints do
            begin
              FGrowthFactors.Delete(LCount);
              FYears.Delete(LCount)
            end;
            FYears.TrimExcess;
            FGrowthFactors.TrimExcess;
            for LCount := 0 to FYears.Count - 1 do
            begin
              Set_NoOfYearsByIndex(LCount,FYears[LCount]);
            end;
            for LCount := 0 to FGrowthFactors.Count - 1 do
            begin
              Set_GrowthFactorByIndex(LCount,FGrowthFactors[LCount]);
            end;
          end;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'NoOfPoints',IntToStr(LOldValue),IntToStr(FNoOfPoints));
        end;
      finally
        FreeAndNil(LSQLAgent);
        FreeAndNil(LContextData)
      end;
    end;
  except on E: Exception do HandleError(E,OPNAME); end;
end;

procedure TPlanningMineGrowthFactor.Set_NoOfYearsByIndex(AIndex,
  Value: Integer);
const OPNAME = 'TPlanningMineGrowthFactor.Set_NoOfYearsByIndex';
var
  LSQLAgent : TPlanningMineSQLAgent;
  LContextData : TStringList;
  LOldValue : string;
  LNewValue : string;
  LBackUp : double;
begin
  try
    LSQLAgent := TPlanningMineSQLAgent.Create(FAppModules);
    LContextData := TStringList.Create;
     LBackUp := 0.0;
    try
      {case FFactorType of
        1: LSQLAgent.LoadContextData_GrowthFactorMine(LContextData,FMine,FFactorType);
        2..7: LSQLAgent.LoadContextData_GrowthFactorOpenCast(LContextData,FMine, FOpenCast, FFactorType);
        8,9: LSQLAgent.LoadContextData_GrowthFactorUnderGround(LContextData,FMine, FUnderground,FFactorType);
        10: LSQLAgent.LoadContextData_GrowthFactorSlurryDump(LContextData,FMine, FSlurryDump, FFactorType);
      end;}

      LSQLAgent.LoadLoadContextData_GrowthFactor(LContextData,FIdentifier,FMine,FOpenCast,FUnderground,FSlurryDump);
      try
        if((AIndex >= 0) AND (AIndex <FYears.Count )) then
        begin
          LBackup := FYears[AIndex];
          FYears[AIndex] :=  Value;
          LNewValue := ListToStringYears;
          if (FAppModules.FieldProperties.UpdateFieldValue('NYR', LNewValue, FNoOfYears, LContextData)) then
          begin
            LOldValue := FNoOfYears;
            FNoOfYears := LNewValue;
            FAppModules.Model.StudyDataHasChanged(sdccEdit,'NYR',LOldValue,FNoOfYears);
          end;
        end;
      except on E: Exception do FGrowthFactors[AIndex] := LBackup;  end;
    finally
      FreeAndNil(LSQLAgent);
      FreeAndNil(LContextData);
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;


function TPlanningMineGrowthFactor.Validate(var AError: WideString;
  const AContext: WideString): WordBool;
const OPNAME = 'TPlanningMineGrowthFactor.Validate';
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
      if(AContext = 'NoOfPoints') then
        Result := ValidateNoOfPoints(LErrorMsgs)
      else if(AContext = 'InterpolationMethod') then
        Result := ValidateInterpolation(LErrorMsgs)
      else if (AContext = 'NYR' ) then
        Result := ValidateNoOfYears(LErrorMsgs,LErrorCols)
      else if (AContext = 'GrowthFactors' ) then
        Result := ValidateGrowthFactor(LErrorMsgs,LErrorCols)
      else
        Result := ValidateNoOfPoints(LErrorMsgs) and ValidateInterpolation(LErrorMsgs)
          and ValidateNoOfYears(LErrorMsgs,LErrorCols) and ValidateGrowthFactor(LErrorMsgs,LErrorCols);

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

function TPlanningMineGrowthFactor.ValidateGrowthFactor(
  AErrorMessages: Tstrings;  AErrorColumns: TStrings): wordbool;
const OPNAME = 'TPlanningMineGrowthFactor.ValidateGrowthFactor';
var
  LMessage : string;
  LResult : boolean;
  LIndex : integer;
begin
    Result := FALSE;
    try
      lResult := True;
      lMessage := '';
      for LIndex := 0 to FGrowthFactors.Count -1 do
      begin
        if(not (FAppModules.FieldProperties.ValidateFieldProperty('GrowthFactors',FloatToStr(FGrowthFactors[LIndex]),
                    lMessage))) then
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

function TPlanningMineGrowthFactor.ValidateInterpolation(
  AErrorMessages: TStrings): wordbool;
const OPNAME = 'TPlanningMineGrowthFactor.ValidateInterpolation';
var
  lMessage : string;
begin
  Result := False;
  try
      if (not (FAppModules.FieldProperties.ValidateFieldProperty('InterpolationMethod',IntToStr(FInterpolationMethod),lMessage))) then
        AErrorMessages.Add(lMessage)
      else
        Result := true;
  except on E: Exception do HandleError(E, OPNAME)  end;
end;

function TPlanningMineGrowthFactor.ValidateNoOfPoints(
  AErrorMessages: TStrings): wordbool;
const OPNAME = 'TPlanningMineGrowthFactor.ValidateNoOfPoints';
var
  lMessage : string;
begin
  Result := False;
  try
      if (not (FAppModules.FieldProperties.ValidateFieldProperty('NoOfPoints',IntToStr(FNoOfPoints),lMessage))) then
        AErrorMessages.Add(lMessage)
      else
        Result := true;
  except on E: Exception do HandleError(E, OPNAME)  end;
end;

function TPlanningMineGrowthFactor.ValidateNoOfYears(
  AErrorMessages: TStrings; AErrorColumns: TStrings): wordbool;
const OPNAME = 'TPlanningMineGrowthFactor.ValidateNoOfYears';
var
  LMessage : string;
  LResult : boolean;
  LIndex : integer;
  //LFieldProperties: TAbstractFieldProperty;
begin
  Result := FALSE;
  try
    lResult := True;
    lMessage := '';
    //LFieldProperties := FAppModules.FieldProperties.FieldProperty('NYR');
    for LIndex := 0 to FYears.Count -1 do
    begin
      if(not (FAppModules.FieldProperties.ValidateFieldProperty('NYR',FloatToStr(FYears[LIndex]),
                  lMessage))) then
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

function TPlanningMineGrowthFactor._AddRef: Integer;
const OPNAME = 'TPlanningMineGrowthFactor._AddRef';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E,OPNAME); end;

end;

function TPlanningMineGrowthFactor._Release: Integer;
const OPNAME = 'TPlanningMineGrowthFactor._Release';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result := FRefCount;
  except on E: Exception do HandleError(E,OPNAME); end;

end;
end.
