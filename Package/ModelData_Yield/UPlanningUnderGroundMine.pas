unit UPlanningUnderGroundMine;

interface
  uses
    Classes,
     Contnrs,
    UPlanningMineGrowthFactor,
    ULoadGeneration,
    VoaimsCom_TLB,
    UMiningData,
    System.Generics.Collections;
  type
    TPlanningUnderGroundMine = class(TUnderground)
      protected
        FGrowthFactorList:   TObjectList;
        FLoadGeneration: TLoadGeneration;
        FLoadGenerationList : TObjectList;
        FAcceptedGrowthFactorType : TList<integer>;
        FAcceptedLoadGenType : integer;
        procedure CreateMemberObjects; override;
        procedure DestroyMemberObjects; override;
        function Get_GrowthFactorCount: integer;
      public
        function NewGrowthFactor: TPlanningMineGrowthFactor;
        function CreateGrowthFactor(AType : integer): IPlanningMineGrowthFactor;
        function Validate(var AError: WideString; const AContext: WideString): WordBool; reintroduce;
        procedure Populate(AMineIdentifier : integer;AIdentifier : integer;
                       AUndergroundSectionName           : WideString;
                       AChannelNumberToUGDam             : integer;
                       AUpstreamCatchmentArea            : double;
                       ABoardPillarCatchmentArea         : double;
                       AHighExtractionCatchmentArea      : double;
                       AHighExtractionAreaRunoffFactor   : double); reintroduce;
        function GrowthFactorByType(AType: Integer) : TPlanningMineGrowthFactor;
        function GrowthFactorByIndex(AIndex: Integer) : TPlanningMineGrowthFactor;
        property LoadGeneration: TLoadGeneration read FLoadGeneration;
        property GrowthFactorCount : integer  read Get_GrowthFactorCount;

        property AcceptedLoadGenType : integer read FAcceptedLoadGenType;
        property AcceptedGrowthTypes: TList<integer> read FAcceptedGrowthFactorType;

        function CreateLoadGeneration: ILoadGeneration;  overload;
        function NewLoadGeneration: TLoadGeneration;
        function DeleteLoadGeneration: boolean;
        function DeleteGrowthFactorByType(AType : integer): boolean;
        function RemoveGrowthFactorByIndex(AIndex: integer): wordbool;


    end;

implementation
uses
    SysUtils,
    System.Types,
    UAbstractObject,
    UConditionalOpp,
    UPlanningMineSQLAgent,
    UErrorHandlingOperations;

{ TPlanningUnderGroundMine }

function TPlanningUnderGroundMine.CreateGrowthFactor(AType : integer): IPlanningMineGrowthFactor;
const OPNAME = 'TPlanningUnderGroundMine.CreateGrowthFactor';
var
  LSQLAgent : TPlanningMineSQLAgent;
  LDescription : string;
begin
  Result := nil;
  try
    Result := NewGrowthFactor;
    if Result <> nil then
    begin
       LSQLAgent := TPlanningMineSQLAgent.Create(FAppModules);
       LDescription := LSQLAgent.getGrowthTypeDescription(AType);
       TPlanningMineGrowthFactor(Result).Populate(0,FMineIdentifier,0,0,FIdentifier,1,AType,1,FloatToStr(System.SysUtils.CurrentYear),'1.0',LDescription);
      if not LSQLAgent.AddGrowthFactor(TPlanningMineGrowthFactor(Result)) then
        FreeAndNil(Result);
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TPlanningUnderGroundMine.CreateLoadGeneration: ILoadGeneration;
const OPNAME = 'TPlanningUnderGroundMine.CreateLoadGeneration';
var
  LSQLAgent :  TPlanningMineSQLAgent;
  LFlow : TElevationsArray;
  LMeanOfSalt : TElevationsArray;
  LDescription : string;
begin
  Result := nil;
 try
  Result := NewLoadGeneration;
  if Result <> nil then
  begin
    LSQLAgent := TPlanningMineSQLAgent.Create(FAppModules);
    LDescription := LSQLAgent.getLoadGenTypeDescription(FAcceptedLoadGenType);
    SetLength(LFlow,10);
    SetLength(LMeanOfSalt,10);
    TLoadGeneration(Result).Populate(0,FMineIdentifier,0,Identifier,0,FAcceptedLoadGenType, 0.0,LFlow,LMeanOfSalt,LDescription);
    if not LSQLAgent.AddLoadGeneration(TLoadGeneration(Result)) then
    begin
      FGrowthFactorList.Remove(TLoadGeneration(Result));
    end;
  end;
 except on E: Exception do HandleError(E,OPNAME); end;
end;

procedure TPlanningUnderGroundMine.CreateMemberObjects;
const OPNAME = 'TPlanningUnderGroundMine.CreateMemberObjects';
begin
  inherited;
  try
    FGrowthFactorList := TObjectList.Create;
    FAcceptedLoadGenType := 3;
    //FGrowthFactor := TPlanningMineGrowthFactor.Create(FAppModules);
    //FLoadGeneration := TLoadGeneration.Create(FAppModules);
    FAcceptedGrowthFactorType := TList<integer>.Create;
     FAcceptedGrowthFactorType.Add(8);
     FAcceptedGrowthFactorType.Add(9);
  except on E:Exception do HandleError(E,OPNAME); end;

end;

function TPlanningUnderGroundMine.DeleteGrowthFactorByType(
  AType: integer): boolean;
const OPNAME = ' TPlanningUnderGroundMine.DeleteGrowthFactorByType';
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

  except on E: Exception do HandleError(E,OPNAME); end;
end;

function TPlanningUnderGroundMine.DeleteLoadGeneration: boolean;
const OPNAME = 'TPlanningUnderGroundMine.DeleteLoadGeneration';
var
  LSQLAgent :   TPlanningMineSQLAgent;
begin
  Result := false;
  try
    LSQLAgent :=  TPlanningMineSQLAgent.Create(FAppModules);
  if  LSQLAgent.DeleteLoadGeneration(FLoadGeneration) then
  begin
      FLoadGeneration := nil;
      Result := true;
  end;

  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TPlanningUnderGroundMine.DestroyMemberObjects;
const OPNAME = 'TPlanningUnderGroundMine.DestroyMemberObjects';
begin
  inherited;
  try
    FreeAndNil(FGrowthFactorList);
    FreeAndNil(FLoadGeneration);
  except on E: Exception do HandleError(E,OPNAME); end;

end;

function TPlanningUnderGroundMine.Get_GrowthFactorCount: integer;
const OPNAME = 'TPlanningUnderGroundMine.Get_GrowthFactorCount';
begin
  Result := 0;
  try
    Result := FGrowthFactorList.Count;
  except on E: Exception do HandleError(E,OPNAME); end;
end;

function TPlanningUnderGroundMine.GrowthFactorByIndex(
  AIndex: integer): TPlanningMineGrowthFactor;
const OPNAME = 'TPlanningUnderGroundMine.GrowthFactorByIndex';
begin
  Result := nil;
  try
    if ((AIndex >= 0) AND ( AIndex < FGrowthFactorList.Count)) then
    begin
      Result := TPlanningMineGrowthFactor(FGrowthFactorList[AIndex]);
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TPlanningUnderGroundMine.GrowthFactorByType(
  AType: Integer): TPlanningMineGrowthFactor;
const OPNAME = 'TPlanningUnderGroundMine.GrowthFactorByType';
var
  LIndex: integer;
begin
  Result := nil;
  try
    if FGrowthFactorList.Count > 0 then
    begin
      for LIndex := 0 to FGrowthFactorList.Count -1 do
      begin
        Result :=  TPlanningMineGrowthFactor(FGrowthFactorList[LIndex]);
        if Result <> nil then
          if Result.FactorType = AType then Exit
          else Result := nil;
      end;
    end;
  except on E: Exception do HandleError(E,OPNAME); end;
end;


function TPlanningUnderGroundMine.NewGrowthFactor: TPlanningMineGrowthFactor;
const OPNAME = 'TPlanningUnderGroundMine.NewGrowthFactor';
begin
  Result := nil;
  try
    Result := TPlanningMineGrowthFactor.Create(FAppModules);
    FGrowthFactorList.Add(Result);
  except on E: Exception do HandleError(E,OPNAME); end;
end;

function TPlanningUnderGroundMine.NewLoadGeneration: TLoadGeneration;
const OPNAME = 'TPlanningUnderGroundMine.NewLoadGeneration';
begin
  Result := nil;
  try
    if FLoadGeneration = nil then
    begin
      FLoadGeneration := TLoadGeneration.Create(FAppModules);
      Result := FLoadGeneration
    end
    else
    begin
      Result := FLoadGeneration;
    end;
  except on E: Exception do HandleError(E,OPNAME); end;
end;

procedure TPlanningUnderGroundMine.Populate(AMineIdentifier,
  AIdentifier: integer; AUndergroundSectionName: WideString;
  AChannelNumberToUGDam: integer; AUpstreamCatchmentArea,
  ABoardPillarCatchmentArea, AHighExtractionCatchmentArea,
  AHighExtractionAreaRunoffFactor: double);
const OPNAME = 'TPlanningUnderGroundMine.Populate';
begin
  try
    inherited;
    //FGrowthFactor.Populate(FGrowthFactor.Identifier,AMineIdentifier,FGrowthFactor.OpenCastIdentifier,FGrowthFactor.SlurryDumpIdentifier,
    //                       AIdentifier,FGrowthFactor.NoOfPoints, FGrowthFactor.FactorType,StrToInt(ConditionalOpp(FGrowthFactor.InterpolationMethod,2,1)),FGrowthFactor.ListToStringYears,
    //                       FGrowthFactor.ListToStringGrowthFactor);
    //SetLength(LFlowArray,10);
    //    SetLength(LMeanArray,10);
    //    for LIndex := 0 to 9 do
    //      begin
    //        LFlowArray[LIndex] := FLoadGeneration.FlowByIndex[LIndex];
    //        LMeanArray[LIndex] := FLoadGeneration.MeanOfSaltByIndex[LIndex];
    //      end;
    //    FLoadGeneration.Populate(FLoadGeneration.Identifier,FMineIdentifier,FIdentifier,-1,AIdentifier,
    //    FLoadGeneration.type_,FLoadGeneration.StandardDeviation,LFlowArray,LMeanArray);
  except on E: Exception do HandleError(E,OPNAME); end;
end;

function TPlanningUnderGroundMine.RemoveGrowthFactorByIndex(
  AIndex: integer): wordbool;
const OPNAME = 'TPlanningUnderGroundMine.RemoveGrowthFactorByIndex';
var
  LSQLAgent : TPlanningMineSQLAgent;
  LGrowthFactor : TPlanningMineGrowthFactor;
begin
  Result := false;
  try
    LSQLAgent := TPlanningMineSQLAgent.Create(FAppModules);
    LGrowthFactor :=  TPlanningMineGrowthFactor(Self.GrowthFactorByIndex(AIndex));
    if LGrowthFactor <> nil then
    begin
      if LSQLAgent.DeleteGrowthFactor(LGrowthFactor) then
      begin
        FGrowthFactorList.Remove(LGrowthFactor);
        Result := true;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;


function TPlanningUnderGroundMine.Validate(var AError: WideString;
  const AContext: WideString): WordBool;
const OPNAME = 'TPlanningUnderGroundMine.Validate';
var
  LGrowthFactor : TPlanningMineGrowthFactor;
  LIndex : integer;
begin
  Result := inherited Validate(AError,AContext);
  try
    for LIndex := 0 to FGrowthFactorList.Count-1 do
        begin
           LGrowthFactor := TPlanningMineGrowthFactor(FGrowthFactorList[LIndex]);
           if not LGrowthFactor.Validate(AError,AContext) then Result := false;
        end;
    if LoadGeneration <> nil then LoadGeneration.Validate(AError,AContext);
    
  except on E: Exception do HandleError(E, OPNAME); end;
end;

end.
