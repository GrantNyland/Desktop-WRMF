unit UPlanningSlurryDump;

interface
 uses
   Classes,
   Contnrs,
   SysUtils,
   System.Generics.Collections,
   UMiningData,
   UPlanningMineGrowthFactor,
   ULoadGeneration,
   VoaimsCom_TLB;
 type
    TPlanningSlurryDump = class(TSlurryDump, IPlanningSlurryDump)
  private
   // function DeleteGrowthFactorByType(AType: integer): boolean;
    protected
      FSaltConcentration : double;
      FLoadGeneration : TLoadGeneration;
      FGrowthFactor : TPlanningMineGrowthFactor;
      FAccpetedGrowthFactorType: integer;
      FAcceptedLoadGenerationType: integer;
      function Get_SaltConcentration: Double; safecall;
      procedure Set_SaltConcentration(Value: Double); safecall;
      function ValidateSaltConcentration(AErrorMessages: TStrings): wordbool;
      procedure CreateMemberObjects; override;
      procedure DestroyMemberObjects; override;
     public
      function NewGrowthFactor: TPlanningMineGrowthFactor;
      function CreateGrowthFactor: IPlanningMineGrowthFactor;
      function NewLoadGeneration : TLoadGeneration;
      function CreateLoadGeneration : ILoadGeneration;
      function Validate(var AErrors: WideString; const AContext: WideString): WordBool; safecall;
      procedure Populate(ASaltConcentration: double);overload;
      procedure Populate(AMineIdentifier: integer; AIdentifier: integer; ADumpName : WideString;
                       ADumpSurfaceArea: double; ARunoffFactorToPCD: double; ASeepageSplitFactor: double;
                       APCDStorageCapacity: double;APCDSurfaceArea : double; APCDAnalysisStartVolume : double; ASaltConcentration: double); overload;
      function Initialise: boolean; override;
      function DeleteLoadGeneration: boolean;
      function DeleteGrowthFactor : TPlanningMineGrowthFactor;

      property SaltConcentration: Double read Get_SaltConcentration write Set_SaltConcentration;
      property LoadGeneration: TLoadGeneration read FLoadGeneration;
      property GrowthFactor: TPlanningMineGrowthFactor read  FGrowthFactor;


    end;

implementation
  uses
    UConstants,
    UPlanningMineSQLAgent,
    UAbstractObject,
    UErrorHandlingOperations;

{ TPlanningSlurryDump }

function TPlanningSlurryDump.CreateGrowthFactor: IPlanningMineGrowthFactor;
const OPNAME = 'TPlanningSlurryDump.CreateGrowthFactor';
var
  LSQLAgent : TPlanningMineSQLAgent;
  LDescription : string;

begin
  Result := nil;
  try
    Result :=  NewGrowthFactor;
    if Result <> nil then
    begin
      LSQLAgent := TPlanningMineSQLAgent.Create(FAppModules);
      LDescription := LSQLAgent.getGrowthTypeDescription(FAccpetedGrowthFactorType);
      TPlanningMineGrowthFactor(Result).Populate(0,MineIdentifier,0,Identifier,0,1,FAccpetedGrowthFactorType,1,FloatToStr(System.SysUtils.CurrentYear),'1.0',LDescription);
      if not LSQLAgent.AddGrowthFactor(TPlanningMineGrowthFactor(Result)) then
      begin
       FreeAndNil(Result);
      end;
    end;
  except on E: Exception do HandleError(E,OPNAME); end;
end;

function TPlanningSlurryDump.CreateLoadGeneration: ILoadGeneration;
const OPNAME ='TPlanningSlurryDump.CreateLoadGeneration';
var
  LSQLAgent : TPlanningMineSQLAgent;
  LFlow : TElevationsArray;
  LMeanOfSalt : TElevationsArray;
  LDescription : string;
begin
  Result := nil;
  try
    Result := NewLoadGeneration;
    if Result <> nil then
    begin
      SetLength(LFlow,10);
      SetLength(LMeanOfSalt,10);
      LSQLAgent := TPlanningMineSQLAgent.Create(FAppModules);
      LDescription := LSQLAgent.getLoadGenTypeDescription(FAcceptedLoadGenerationType);
      TLoadGeneration(Result).Populate(0,FMineIdentifier,0,0,FIdentifier,FAcceptedLoadGenerationType,0.0,LFlow,LMeanOfSalt,LDescription);
      if not LSQLAgent.AddLoadGeneration(TLoadGeneration(Result)) then
        FreeAndNil(Result);
    end;
  except on E: Exception do HandleError(E,OPNAME); end;
end;

function TPlanningSlurryDump.DeleteGrowthFactor: TPlanningMineGrowthFactor;
const OPNAME = 'TPlanningSlurryDump.DeleteGrowthFactor';
var
  LSQLAgent : TPlanningMineSQLAgent;
begin
  Result := GrowthFactor;
  try
     LSQLAgent := TPlanningMineSQLAgent.Create(FAppModules);
    try
      if(GrowthFactor <> nil) then
      begin
        if LSQLAgent.DeleteGrowthFactor(GrowthFactor) then
          FGrowthFactor := nil;
        Result := GrowthFactor;
      end;
    finally
      LSQLAgent.Free;
    end;
  except on E: Exception do HandleError(E,OPNAME); end;
end;

{*function TPlanningSlurryDump.DeleteGrowthFactorByType(AType: integer): boolean;
const OPNAME = 'TPlanningSlurryDump.DeleteGrowthFactorByType';
var
  LSQLAgent : TPlanningMineSQLAgent;
begin
  Result := false;
  try
    LSQLAgent := TPlanningMineSQLAgent.Create(FAppModules);
    if LSQLAgent.DeleteGrowthFactor(Self.GrowthFactorByType(AType)) then
    begin
      FGrowthFactorList.Remove(Self.GrowthFactorByType(AType));
      Result := true;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;*}

function TPlanningSlurryDump.DeleteLoadGeneration: boolean;
const OPNAME = 'TPlanningSlurryDump.DeleteLoadGeneration';
var
  LSQLAgent :   TPlanningMineSQLAgent;
begin
  Result := false;
  try
  LSQLAgent := TPlanningMineSQLAgent.Create(FAppModules);
  if  LSQLAgent.DeleteLoadGeneration(FLoadGeneration) then
  begin
      FLoadGeneration := nil;
      Result := true;
  end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TPlanningSlurryDump.CreateMemberObjects;
const OPNAME = 'TPlanningSlurryDump.CreateMemberObjects';
begin
  inherited;
  try

     FAccpetedGrowthFactorType := 10;
     FAcceptedLoadGenerationType := 4;
  except on E: Exception do HandleError(E,OPNAME); end;
end;

procedure TPlanningSlurryDump.DestroyMemberObjects;
const OPNAME = 'TPlanningSlurryDump.DestroyMemberObjects';
begin
  inherited;
  try
    FreeAndNil(FLoadGeneration);
    FreeAndNil(FGrowthFactor);
  except on E: Exception do HandleError(E,OPNAME); end;

end;

function TPlanningSlurryDump.Get_SaltConcentration: Double;
const OPNAME = 'TPlanningSlurryDump.Get_SaltConcentration';
begin
  Result := 0.0;
  try
    Result := FSaltConcentration;
  except on E: Exception do HandleError(E,OPNAME); end;
end;

function TPlanningSlurryDump.Initialise: boolean;
const OPNAME='TPlanningSlurryDump.Initialise';
begin
  Result := inherited Initialise;
  try
    FSaltConcentration:= 0.0;
    Result:= true;
  except on E: Exception do HandleError(E,OPNAME); end;

end;

function TPlanningSlurryDump.NewGrowthFactor: TPlanningMineGrowthFactor;
const OPNAME = 'TPlanningSlurryDump.NewGrowthFactor';
var
  LCurrentYear: string;
begin
  Result := nil;
  try
    FGrowthFactor := TPlanningMineGrowthFactor.Create(FAppModules);
    LCurrentYear :=  IntToStr(CurrentYear);  //FloatToStrF(System.SysUtils.CurrentYear,ffNumber,5,2);
    FGrowthFactor.Populate(0,FMineIdentifier,0,FIdentifier,0,0,10,1,LCurrentYear,FloatToStr(1.0),'');
    Result := FGrowthFactor;
  except on E: Exception do HandleError(E,OPNAME); end;
end;

function TPlanningSlurryDump.NewLoadGeneration: TLoadGeneration;
const OPNAME = 'TPlanningSlurryDump.NewLoadGeneration';
var
  LFlow, LMeanOFSalt :  TElevationsArray;
begin
  Result := nil;
  try
    FLoadGeneration := TLoadGeneration.Create(FAppModules);
    Result := FLoadGeneration;
    SetLength(LMeanOFSalt,10);
    SetLength(LFlow,10);
    FLoadGeneration.Populate(0,FMineIdentifier,0,0,FIdentifier,4,0.0,LFlow,LMeanOFSalt,'');
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TPlanningSlurryDump.Populate(AMineIdentifier: integer; AIdentifier: integer; ADumpName : WideString;
                       ADumpSurfaceArea: double; ARunoffFactorToPCD: double; ASeepageSplitFactor: double;
                       APCDStorageCapacity: double;APCDSurfaceArea : double; APCDAnalysisStartVolume : double;
                       ASaltConcentration: double);
const OPNAME = 'TPlanningSlurryDump.Populate';
begin
  try
    inherited Populate(AMineIdentifier,AIdentifier,ADumpName,ADumpSurfaceArea,ARunoffFactorToPCD,ASeepageSplitFactor,
    APCDStorageCapacity,APCDSurfaceArea,APCDAnalysisStartVolume);
    Populate(ASaltConcentration);
  except on E: Exception do HandleError(E,OPNAME); end;
end;

procedure TPlanningSlurryDump.Populate(ASaltConcentration: double);
const OPNAME = 'TPlanningSlurryDump.Populate';
begin
  try
    FSaltConcentration := ASaltConcentration;
  except on E: Exception do HandleError(E,OPNAME); end;
end;

procedure TPlanningSlurryDump.Set_SaltConcentration(Value: Double);
const OPNAME = 'TPlanningSlurryDump.Set_SaltConcentration';
var
  LSQLAgent : TPlanningMineSQLAgent;
  LContextData: TStringList;
  LOldValue: string;
begin
  try
    LSQLAgent := TPlanningMineSQLAgent.Create(FAppModules);
    LContextData := TStringList.Create;
    LSQLAgent.LoadContextData_SlurryDump(LContextData, FMineIdentifier, FIdentifier);
    if (FAppModules.FieldProperties.UpdateFieldValue('SaltConcentration',FloatToStr(Value),FloatToStr(FSaltConcentration),LContextData)) then
    begin
      LOldValue := FloatToStr(FSaltConcentration);
      FSaltConcentration := Value;
      FAppModules.Model.StudyDataHasChanged(sdccEdit,'SaltConcentration',LOldValue, FloatToStr(FSaltConcentration));
    end;

  except on E: Exception do HandleError(E,OPNAME); end;
end;

function TPlanningSlurryDump.Validate(var AErrors: WideString;
  const AContext: WideString): WordBool;
const OPNAME = 'TPlanningSlurryDump.Validate';
var
  LErrorMsgs: TStringList;

begin
  Result := inherited Validate(AErrors,AContext);
  try
    try
      LErrorMsgs := TStringList.Create;
      if AContext = 'SaltConcentration' then Result := ValidateSaltConcentration(LErrorMsgs)
      else
      begin
        Result := ValidateSaltConcentration(LErrorMsgs);
        if FLoadGeneration <> nil then Result := FLoadGeneration.Validate(AErrors,AContext);
        if FGrowthFactor <> nil then Result := FGrowthFactor.Validate(AErrors,AContext);
      end;

      if (not Result) then
      begin
        AErrors := AErrors + LErrorMsgs.Text;
      end;
    finally
      FreeAndNil(LErrorMsgs);
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TPlanningSlurryDump.ValidateSaltConcentration(
  AErrorMessages: TStrings): wordbool;
const OPNAME  = 'TPlanningMine.ValidateSaltConcentration';
var
  lMessage : string;
begin
  Result := False;
  try
      if (not (FAppModules.FieldProperties.ValidateFieldProperty('SaltConcentration',FloatToStr(FSaltConcentration),lMessage))) then
        AErrorMessages.Add(lMessage)
      else
        Result := true;
  except on E: Exception do HandleError(E, OPNAME)  end;
end;

end.
