unit UPlanningMineData;

interface
uses
  Classes,
  Contnrs,
  UConstants,
  UAbstractObject,
  UMiningData,
  UPlanningMineGrowthFactor,
  UPlanningOpenCast,
  UPlanningSlurryDump,
  UPlanningUnderGroundMine,
  VoaimsCom_TLB;

type
  TPlanningMine = class(TMine,IPlanningMine)
  private
    function Get_OpenCastByIndex(AIndex: integer): IPlanningOpenCast;
  protected
    FAssocSaltWashoff: Integer;
    FRainfallFileName: Widestring;
    FMeanAnnualPrecipitation: double;
    FSaltBuildUpRate: double;
    FSaltWashOffEfficiencyFactor: double;
    FIniSaltStore: double;
    FGrowthFactor: TPlanningMineGrowthFactor;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    function Get_RainfallFileName: WideString; safecall;
    procedure Set_RainfallFileName(const Value: WideString); safecall;
    function ValidateRainfallFileName(AErrorMessages: TStrings): Boolean;
    function Get_AssocSaltWashoff: Integer; safecall;
    procedure Set_AssocSaltWashoff(Value: Integer); safecall;
    function ValidateAssocSaltWashoff(AErrorMessages: TStrings): Boolean;
    function Get_MeanAnnualPrecipitation: Double; safecall;
    procedure Set_MeanAnnualPrecipitation(Value: Double); safecall;
    function ValidateMeanAnnualPrecipitation(AErrorMessages: TStrings): Boolean;
    function Get_SaltBuildUpRate: Double; safecall;
    procedure Set_SaltBuildUpRate(Value: Double); safecall;
    function  ValidateSaltBuildUpRate(AErrorMessages: TStrings): Boolean;
    function Get_SaltWashOffEfficiencyFactor: Double; safecall;
    procedure Set_SaltWashOffEfficiencyFactor(Value: Double); safecall;
    function ValidateSaltWashOffEfficiencyFactor(AErrorMessages: TStrings): boolean;
    function Get_IniSaltStore: Double; safecall;
    procedure Set_IniSaltStore(Value: Double); safecall;
    function ValidateIniSaltStore(AErrorMessages: TStrings): boolean;
    function Get_CastOpenCastByIndex(AIndex: integer)  : TPlanningOpenCast;
    function Get_CastOpenCastByID(AID: integer): TPlanningOpenCast;
    function Get_CastSlurryDumpByIndex(AIndex: integer): TPlanningSlurryDump;
    function Get_CastUnderGroundByIndex(AIndex: integer): TPlanningUnderGroundMine;
    function Get_CastUnderGroundByID(AID: integer):   TPlanningUnderGroundMine;
    function Get_CastSlurryDumpByID(AIdentifier: integer): TPlanningSlurryDump;
  public
    procedure Populate(AAssocSaltWashoff: integer;
                       ARainfallFileName: string;
                       AMeanAnnualPrecipitation: Single;
                       ASaltBuildUpRate: Single;
                       ASaltWashOffEfficiencyFactor: Single;
                       AIniSaltStore: Single); overload;
    procedure Populate(AIdentifier: Integer;
                       ANodeNumber: Integer;
                       AMineName : WideString;
                       ARiverChannelNumber: Integer;
                       APCDChannelNumber: Integer;
                       AHydrologyNodeNumber: integer;
                       ABeneficiationPlantArea: double;
                       ABeneficiationRunOffFactor: double;
                       AAssocSaltWashoff: integer;
                       ARainfallFileName: string;
                       AMeanAnnualPrecipitation: Single;
                       ASaltBuildUpRate: Single;
                       ASaltWashOffEfficiencyFactor: Single;
                       AIniSaltStore: Single); overload;
    function Initialise: boolean; override;
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; reintroduce;
    function NewOpenCast    : TPlanningOpenCast; reintroduce;
    function CreateOpenCast : IPlanningOpenCast; reintroduce;
    function RemoveOpenCast(AOpenCastID : integer) : WordBool;
    function RemoveSlurryDump(ASlurryDumpID : integer) : WordBool;
    function NewSlurryDump  : TPlanningSlurryDump;
    function CreateSlurryDump : IPlanningSlurryDump;
    function NewUnderground : TPlanningUnderGroundMine;
    function CreateUnderGround : IUnderground;
    function NewGrowthFactor : TPlanningMineGrowthFactor;
    function DeleteGrowthFactor : TPlanningMineGrowthFactor;
    function CreatGrowthFactor : IPlanningMineGrowthFactor;
    property RainfallFileName: WideString read Get_RainfallFileName write Set_RainfallFileName;
    property AssocSaltWashoff: Integer read Get_AssocSaltWashoff write Set_AssocSaltWashoff;
    property MeanAnnualPrecipitation: Double read Get_MeanAnnualPrecipitation write Set_MeanAnnualPrecipitation;
    property SaltBuildUpRate: Double read Get_SaltBuildUpRate write Set_SaltBuildUpRate;
    property SaltWashOffEfficiencyFactor: Double read Get_SaltWashOffEfficiencyFactor write Set_SaltWashOffEfficiencyFactor;
    property IniSaltStore: Double read Get_IniSaltStore write Set_IniSaltStore;
    property GrowthFactor: TPlanningMineGrowthFactor read FGrowthFactor;
    property CastOpenCastByIndex[AIndex: integer] :TPlanningOpenCast read Get_CastOpenCastByIndex;
    property CastOpenCastByID[AIdentifier: integer]   : TPlanningOpenCast    read Get_CastOpenCastByID;
    property CastSlurryDumpByIndex[AIndex: integer] : TPlanningSlurryDump read Get_CastSlurryDumpByIndex;
    property CastUnderGroundByIndex[AIndex: integer]  : TPlanningUnderGroundMine read Get_CastUnderGroundByIndex;
    property CastUnderGroundByID[AIdentifier: integer]: TPlanningUnderGroundMine read Get_CastUnderGroundByID;
    property CastSlurryDumpByID[AIdentifier: integer] : TPlanningSlurryDump  read Get_CastSlurryDumpByID;
  end;

 (* TPlanningMineList = class(TMineList)
  protected
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;

    function Get_MineByIdentifier(AMineID: integer): IPlanningMine;
    function Get_MineByIndex(AIndex: integer): IPlanningMine;
    function Get_MineByNodeNumber(ANodeNumber: Integer): IPlanningMine;
    function Get_MinebyPCDNumber(APCDNumber: Integer): IPlanningMine;

    function Get_CastMinebyID(AMineID: Integer): TPlanningMine;
    function Get_CastMineByIndex(AIndex: Integer): TPlanningMine;
    function Get_CastMineByNodeNumber(ANodeNumber: Integer): TPlanningMine;
    function Get_CastMineByPCDNumber(APCDNumber: Integer): TPlanningMine;
  public
    function NewMine : TPlanningMine; reintroduce;
    function CreateMine : IPlanningMine;
    function CopyCreate(AMineNumber : integer) : IPlanningMine;
    function RemoveMine(AMineNumber : integer) : WordBool;
    property MineByIndex[AIndex: Integer]: IPlanningMine read Get_MineByIndex;
    property MineByID[AMineID: Integer]: IPlanningMine read Get_MineByIdentifier;
    property MineByNodeNumber[AMineNumber: Integer]: IPlanningMine read Get_MineByNodeNumber;

    property CastMineByIndex[AIndex: Integer]: TPlanningMine read Get_CastMinebyID;
    property CastMineByID[AMineID: Integer]: TPlanningMine read Get_CastMineByID;
    property CastMinebyNodeNumber[AMineNumber: Integer]: TPlanningMine read Get_CastMineByNodeNumber;
  end;*)


implementation
uses
  SysUtils,
  System.Types,
  USystemModelManager,
  UPlanningMineSQLAgent,
  UYieldModelDataObject,
  UReservoirData,
  UParameterData,
  UConditionalOpp,
  UErrorHandlingOperations,
  UPlanningModelDataObject,
  Math, UNetworkFeaturesData, UNetworkElementData;

procedure TPlanningMine.CreateMemberObjects;
const OPNAME = 'TPlanningMine.CreateMemberObjects';
begin
  inherited;
  try
    //FGrowthFactor := TPlanningMineGrowthFactor.Create(FAppModules);
    //FGrowthFactor.Populate(0,FIdentifier,-1,-1,-1,0,1,1,'','');
  except on E: Exception do HandleError(E,OPNAME); end;
end;

function TPlanningMine.CreateOpenCast: IPlanningOpenCast;
const OPNAME = 'TPlanningMine.CreateOpenCast';
var
  LSQLAgent   : TPlanningMineSQLAgent;
  LOpencast   : TPlanningOpenCast;
  LName : string;
  LMaxID : integer;
begin
  Result := Nil;
  try
    if (FOpenCastObjectContainer.Count >= 10) then
      Exit;

    LSQLAgent := TPlanningMineSQLAgent.Create(FAppModules);
    try
      LOpencast := NewOpenCast;
      LMaxID :=     LSQLAgent.GetMaxOpenCastID+1;
      LName :=        FMineName + ' - Opencast Pit '+IntToStr(LMaxID);
      LOpencast.Populate(FIdentifier,LMaxID,LName,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0,0.0,1900,1,1900,1,0.0,0.0,0.0,0.3,0.3,0.4,'');
      if LSQLAgent.AddOpenCast(LOpencast) then
        Result := LOpencast
      else
        FOpenCastObjectContainer.Remove(LOpencast);
    finally
      LSQLAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TPlanningMine.CreateSlurryDump: IPlanningSlurryDump;
const OPNAME = 'TPlanningMine.CreateSlurryDump';
var
  LSQLAgent    : TPlanningMineSQLAgent;
  LSlurryDump : TPlanningSlurryDump;
  LMaxID : integer;
  LName : string;
begin
  Result := Nil;
  try
    if (FSlurryDumpObjectContainer.Count >= 10) then
      Exit;

    LSQLAgent := TPlanningMineSQLAgent.Create(FAppModules);
    try
      LSlurryDump := NewSlurryDump;
      LMaxID    := LSQLAgent.GetMaxSlurryDumpID+1;;
      LName       := FMineName + ' - Slurry Dump '+IntToStr(LMaxID);
      LSlurryDump.Populate(FIdentifier,LMaxID,LName,0.0,0.0,0.0,0.0,0.0,0.0,0.0);
      if LSQLAgent.AddSlurryDump(LSlurryDump) then
        Result := LSlurryDump
      else
        FSlurryDumpObjectContainer.Remove(LSlurryDump);
    finally
      LSQLAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TPlanningMine.CreateUnderGround: IUnderground;
const OPNAME = 'TPlanningMine.CreateUnderGround';
var
  LIdentifier      : integer;
  LSQLAgent        : TPlanningMineSQLAgent;
  LUnderground     : TPlanningUnderGroundMine;
  LMineNode,
  LUndergroundDam  : IReservoirData;
  LMinMaxFeature   : IMinMaxFlowConstraint;
  LChannelToUndergroundDam : IGeneralFlowChannel;
  LContinue                : boolean;
  LMaxID : integer;
  LChannelNoToDam : integer;
begin
  Result := Nil;
  try
    if (FUnderGroundObjectContainer.Count >= 10) then
      Exit;

    LSQLAgent := TPlanningMineSQLAgent.Create(FAppModules);
    try
      LUndergroundDam  := nil;
      LMinMaxFeature   := nil;
      LChannelToUndergroundDam := nil;

      LMineNode := MineNode;
      LContinue := (LMineNode <> nil);

      //Create UndergroundDam
      if LContinue  then
      begin
        LUndergroundDam := (FAppModules.Model.ModelData as IYieldModelData).NetworkElementData.ReservoirList.CreateReservoir(ntMineUndergroundDam);
        LContinue := (LUndergroundDam <> nil)
      end;

      //Create channel to UndergroundDam
      if LContinue  then
      begin
        LIdentifier := LUndergroundDam.ReservoirConfigurationData.ReservoirIdentifier;
        LUndergroundDam.ReservoirConfigurationData.ReservoirName := IntToStr(LIdentifier) + ' - '+ FMineName +' U/G Dam' ;
        LChannelToUndergroundDam := (FAppModules.Model.ModelData as IYieldModelData).NetworkElementData.ChannelList.CreateChannel;
        LContinue := (LChannelToUndergroundDam <> nil);
      end;

      //Create channel to PCD min-max feature
      if LContinue  then
      begin
        LMinMaxFeature :=  (FAppModules.Model as IYieldModel).DoCreateMinMaxFlowFeature(LChannelToUndergroundDam.ChannelNumber);
        LChannelToUndergroundDam.ChannelType          := ctMineToUndergroundChannel;
        LChannelToUndergroundDam.UpStreamNodeNumber   := LMineNode.ReservoirConfigurationData.ReservoirIdentifier;
        LChannelToUndergroundDam.DownStreamNodeNumber := LUndergroundDam.ReservoirConfigurationData.ReservoirIdentifier;
        LChannelNoToDam                               :=  LChannelToUndergroundDam.ChannelNumber;
        LChannelToUndergroundDam.ChannelName          := IntToStr(LChannelNoToDam)+ ' - '+ FMineName + ' To '+
                                                          LUndergroundDam.ReservoirConfigurationData.ReservoirName;
        LContinue := (LMinMaxFeature <> nil);
      end;

      //Create channel to underground section
      if LContinue  then
      begin
        LUnderground := NewUnderground;
        LMaxID   := LSQLAgent.GetMaxUndergroundID+1;

        LUnderground.Populate(FIdentifier,LMaxID,FMineName + ' - Underground Section '+IntToStr(LMaxID),
                              LChannelToUndergroundDam.ChannelNumber,0.0,0.0,0.0,0.0);
        LContinue :=  LSQLAgent.AddUnderground(LUnderground);
        if LContinue then
          Result := LUnderground
        else
          FUndergroundObjectContainer.Remove(LUnderground);
      end;

      if not LContinue then
      begin
        if (LUndergroundDam  <> nil) then
        begin
          LIdentifier      := LUndergroundDam.ReservoirConfigurationData.ReservoirIdentifier;
          LUndergroundDam  := nil;
          (FAppModules.Model.ModelData as IYieldModelData).NetworkElementData.ReservoirList.DeleteReservoir(LIdentifier);
        end;

        if (LChannelToUndergroundDam  <> nil) then
        begin
          LIdentifier := LChannelToUndergroundDam.ChannelNumber;
          LChannelToUndergroundDam := nil;
          (FAppModules.Model as IYieldModel).DoConvertChannel(LIdentifier);
          (FAppModules.Model.ModelData as IYieldModelData).NetworkElementData.ChannelList.RemoveChannelWithNumber(LIdentifier);
        end;
      end;
    finally
      LSQLAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TPlanningMine.CreatGrowthFactor: IPlanningMineGrowthFactor;
const OPNAME = 'TPlanningMine.CreatGrowthFactor';
var
 LSQLAgent : TPlanningMineSQLAgent;
  LCurrentYear: string;
begin
  try
    Result := NewGrowthFactor;
    if Result <> nil then
    begin
      LSQLAgent := TPlanningMineSQLAgent.Create(FAppModules);
      LCurrentYear := FloatToStrF(System.SysUtils.CurrentYear,ffGeneral,4,0);
       TPlanningMineGrowthFactor(Result).Populate(0,FIdentifier,0,0,0,1,1,1,LCurrentYear,FloatToStr(0.0),LSQLAgent.getGrowthTypeDescription(1));
      if not LSQLAgent.AddGrowthFactor(TPlanningMineGrowthFactor(Result)) then
      begin
        FreeAndNil(Result);
      end;

    end;
  except on E: Exception do HandleError(E,OPNAME); end;
end;

function TPlanningMine.DeleteGrowthFactor: TPlanningMineGrowthFactor;
const OPNAME ='TPlanningMine.RemoveOpenCast';
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

procedure TPlanningMine.DestroyMemberObjects;
const OPNAME = 'TPlanningMine.DestroyMemberObjects';
begin
  inherited;
  try
    FreeAndNil(FGrowthFactor);
  except on E: Exception do HandleError(E,OPNAME); end;

end;

function TPlanningMine.Get_AssocSaltWashoff: Integer;
const OPNAME = 'TPlanningMine.Get_AssocSaltWashoff:';
begin
  Result:= 0;
  try
    Result := FAssocSaltWashoff;
  except on E: Exception do HandleError(E,OPNAME) end;
end;

function TPlanningMine.Get_CastOpenCastByID(AID: integer): TPlanningOpenCast;
const OPNAME = 'TPlanningMine.Get_CastOpenCastByID';
var
  LIndex : integer;
begin
  Result:= nil;
  try
    for LIndex := 0 to FOpenCastObjectContainer.Count -1 do
      begin
        Result :=  TPlanningOpenCast(FOpenCastObjectContainer[LIndex]);
        if Result.Identifier = AID then break
        else
          Result := nil;
      end;
  except on E: Exception do HandleError(E,OPNAME); end;
end;

function TPlanningMine.Get_CastOpenCastByIndex(
  AIndex: integer): TPlanningOpenCast;
const OPNAME = 'TPlanningMine.Get_CastOpenCastByIndex';
begin
  Result := nil;
  try
    if(AIndex >= 0) and (AIndex < FOpenCastObjectContainer.Count) then
      Result := TPlanningOpenCast(FOpenCastObjectContainer[AIndex]);
  except on E: Exception do HandleError(E,OPNAME); end;
end;

function TPlanningMine.Get_CastSlurryDumpByID(
  AIdentifier: integer): TPlanningSlurryDump;
const OPNAME ='TPlanningMine.Get_CastSlurryDumpByID';
var
  LCount : integer;
begin
  Result := nil;
  try
    for LCount := 0 to FSlurryDumpObjectContainer.Count -1 do
    begin
      Result := TPlanningSlurryDump(FSlurryDumpObjectContainer[LCount]);
      if Result.Identifier = AIdentifier then break
      else
        Result := nil;
    end;

  except on E: Exception do HandleError(E,OPNAME); end;
end;

function TPlanningMine.Get_CastSlurryDumpByIndex(
  AIndex: integer): TPlanningSlurryDump;
const OPNAME = 'TPlanningMine.Get_CastSlurryDumpByIndex';
begin
  Result := nil;
  try
    if ((AIndex >= 0) AND (AIndex < FSlurryDumpObjectContainer.Count)) then
      Result := TPlanningSlurryDump(FSlurryDumpObjectContainer[AIndex]);

  except on E: Exception do HandleError(E,OPNAME); end;
end;

function TPlanningMine.Get_CastUnderGroundByID(AID: integer): TPlanningUnderGroundMine;
const OPNAME = 'TPlanningMine.Get_CastUnderGroundByID';
var
  LCount : integer;
begin
  Result := nil;
  try
    for LCount := 0 to FUnderGroundObjectContainer.Count -1 do
    begin
        Result :=  TPlanningUnderGroundMine(FUnderGroundObjectContainer[LCount]);
        if Result.Identifier = AID then break;
    end;
  except on E:Exception do HandleError(E,OPNAME); end;
end;

function TPlanningMine.Get_CastUnderGroundByIndex(
  AIndex: integer): TPlanningUnderGroundMine;
const OPNAME = 'TPlanningMine.Get_CastUnderGroundByIndex';
begin
  Result := nil;
  try
    if ((AIndex >= 0) AND (AIndex < FUnderGroundObjectContainer.Count)) then
      Result :=  TPlanningUnderGroundMine(FUnderGroundObjectContainer[AIndex]);
  except on E: Exception do HandleError(E,OPNAME); end;
end;

function TPlanningMine.Get_RainfallFileName: WideString;
const OPNAME = 'TPlanningMine.Get_RainfallFileName';
begin
  Result := '';
  try
    Result := FRainfallFileName;
  except on E: Exception do HandleError(E,OPNAME)  end;
end;

function TPlanningMine.Get_MeanAnnualPrecipitation: double;
const OPNAME = 'TPlanningMine.Get_MeanAnnualPrecipitation';
begin
  Result := 0.0;
  try
    Result := FMeanAnnualPrecipitation;
  except on E: Exception do HandleError(E,OPNAME)  end;
end;

function TPlanningMine.Get_OpenCastByIndex(AIndex: integer): IPlanningOpenCast;
begin

end;

function TPlanningMine.Get_SaltBuildUpRate: double;
const OPNAME= 'TPlanningMine.Get_SaltBuildUpRate';
begin
  Result := 0.0;
  try
    Result := FSaltBuildUpRate;
  except on E: Exception do HandleError(E,OPNAME)  end;
end;

function TPlanningMine.Get_SaltWashOffEfficiencyFactor: double;
const OPNAME = 'TPlanningMine.Get_SaltWashOffEfficiencyFactor';
begin
  Result := 0.0;
  try
    Result := FSaltWashOffEfficiencyFactor;
  except on E: Exception do HandleError(E,OPNAME)  end;
end;

function TPlanningMine.Initialise: boolean;
const  OPNAME = 'TPlanningMine.Initialise';
begin
  Result := inherited Initialise;
  try
    FAssocSaltWashoff := 0;
    FRainfallFileName := '';
    FMeanAnnualPrecipitation := 0.0;
    FSaltBuildUpRate := 0.0;
    FSaltWashOffEfficiencyFactor := 0.0;
    FIniSaltStore := 0.0;
    //Result := FGrowthFactor.Initialise;
  except on E: Exception do HandleError(E,OPNAME); end;
end;

function TPlanningMine.NewGrowthFactor: TPlanningMineGrowthFactor;
const OPNAME = 'TPlanningMine.NewGrowthFactor';

begin
  Result := nil;
  try

    FGrowthFactor := TPlanningMineGrowthFactor.Create(FAppModules);
    Result := FGrowthFactor;
  except on E: Exception do HandleError(E,OPNAME); end;
end;

function TPlanningMine.NewOpenCast: TPlanningOpenCast;
const OPNAME = 'TPlanningMine.NewOpenCast';
begin
  Result := nil;
  try

    Result := TPlanningOpenCast.Create(FAppModules);
    FOpenCastObjectContainer.Add(Result);
    Result.Initialise;
  except on E: Exception do HandleError(E,OPNAME); end;
end;


function TPlanningMine.NewUnderground: TPlanningUnderGroundMine;
const OPNAME = 'TPlanningMine.NewUnderground';
begin
  Result := nil;
  try
    Result := TPlanningUnderGroundMine.Create(FAppModules);
    Result.Initialise;
    FUnderGroundObjectContainer.Add(Result);
  except on E: Exception do HandleError(E,OPNAME); end;
end;

function TPlanningMine.NewSlurryDump: TPlanningSlurryDump;
const OPNAME = 'TPlanningMine.NewSlurryDump';
begin
  Result := nil;
  try
    Result :=  TPlanningSlurryDump.Create(FAppModules);
    FSlurryDumpObjectContainer.Add(Result);
    Result.Initialise;
  except on E: Exception do HandleError(E,OPNAME); end;
end;

function TPlanningMine.Get_IniSaltStore: double;
const OPNAME = 'TPlanningMine.Get_IniSaltStore';
begin
  Result := 0.0;
  try
    Result := FIniSaltStore;
  except on E: Exception do HandleError(E, OPNAME)  end;

end;

procedure TPlanningMine.Set_AssocSaltWashoff(Value: Integer);
const OPNAME  = ' TPlanningMine.Set_AssocSaltWashoff(Value: Integer);';
var
  LSQLAgent : TPlanningMineSQLAgent;
  LContextData : TStringList;
  LOldValue : string;
begin
  try
     LSQLAgent := TPlanningMineSQLAgent.Create(FAppModules);
     LContextData := TStringList.Create();
     try
       LSQLAgent.LoadContextData_MineData(LContextData,FIdentifier);
       if(FAppModules.FieldProperties.UpdateFieldValue('SaltWashOffNo',FloatToStr(Value),FloatToStr(FAssocSaltWashoff), LContextData)) then
       begin
         LOldValue := FloatToStr(FAssocSaltWashoff);
         FAssocSaltWashoff := Value;
         FAppModules.Model.StudyDataHasChanged(sdccEdit,'SaltWashOffNo',LOldValue, FloatToStr(Value));
       end;
     finally
       FreeAndNil(LSQLAgent);
       FreeAndNil(LContextData);
     end;
  except on E: Exception do HandleError(E,OPNAME)  end;
end;

procedure TPlanningMine.Set_IniSaltStore(Value: double);
const OPNAME = 'TPlanningMine.Set_IniSaltStore';
var
  LSQLAgent : TPlanningMineSQLAgent;
  LContextData : TStringList;
  LOldValue : string;
begin
  try
     LSQLAgent := TPlanningMineSQLAgent.Create(FAppModules);
     LContextData := TStringList.Create();
     try
       LSQLAgent.LoadContextData_MineData(LContextData,FIdentifier);
       if(FAppModules.FieldProperties.UpdateFieldValue('IniSaltStore',FloatToStr(Value),FloatToStr(FIniSaltStore), LContextData)) then
       begin
         LOldValue := FloatToStr(FIniSaltStore);
         FIniSaltStore := Value;
         FAppModules.Model.StudyDataHasChanged(sdccEdit,'IniSaltStore',LOldValue, FloatToStr(Value));
       end;
     finally
       FreeAndNil(LSQLAgent);
       FreeAndNil(LContextData);
     end;
  except on E: Exception do HandleError(E,OPNAME)  end;
end;

procedure TPlanningMine.Set_MeanAnnualPrecipitation(Value: Double);
const OPNAME = 'TPlanningMine.Set_MeanAnnualPrecipitation';
var
  LSQLAgent : TPlanningMineSQLAgent;
  LContextData : TStringList;
  LOldValue : string;
begin
  try
     LSQLAgent := TPlanningMineSQLAgent.Create(FAppModules);
     LContextData := TStringList.Create();
     try
       LSQLAgent.LoadContextData_MineData(LContextData,FIdentifier);
       if(FAppModules.FieldProperties.UpdateFieldValue('MeanAnnualPrecipitation',FloatToStr(Value),FloatToStr(FMeanAnnualPrecipitation), LContextData)) then
       begin
         LOldValue := FloatToStr(FMeanAnnualPrecipitation);
         FMeanAnnualPrecipitation := Value;
         FAppModules.Model.StudyDataHasChanged(sdccEdit,'MeanAnnualPrecipitation',LOldValue, FloatToStr(Value));
       end;
     finally
       FreeAndNil(LSQLAgent);
       FreeAndNil(LContextData);
     end;
  except on E: Exception do HandleError(E,OPNAME)  end;
end;

procedure TPlanningMine.Set_RainfallFileName(const Value: WideString);
const OPNAME ='TPlanningMine.Set_RainfallFileName';
var
  LSQLAgent : TPlanningMineSQLAgent;
  LContextData : TStringList;
  LOldValue : string;
begin
  try
     LSQLAgent := TPlanningMineSQLAgent.Create(FAppModules);
     LContextData := TStringList.Create();
     try
       LSQLAgent.LoadContextData_MineData(LContextData,FIdentifier);
       if(FAppModules.FieldProperties.UpdateFieldValue('PlanningMineRainfallFile',Value,FRainfallFileName, LContextData)) then
       begin
         LOldValue := FRainfallFileName;
         FRainfallFileName := Value;
         FAppModules.Model.StudyDataHasChanged(sdccEdit,'PlanningMineRainfallFile',LOldValue, Value);
       end;
     finally
       FreeAndNil(LSQLAgent);
       FreeAndNil(LContextData);
     end;
  except on E: Exception do HandleError(E,OPNAME)  end;
end;

procedure TPlanningMine.Set_SaltBuildUpRate(Value: double);
const  OPNAME = 'TPlanningMine.Set_SaltBuildUpRate';
var
  LSQLAgent : TPlanningMineSQLAgent;
  LContextData : TStringList;
  LOldValue : string;
begin
  try
     LSQLAgent := TPlanningMineSQLAgent.Create(FAppModules);
     LContextData := TStringList.Create();
     try
       LSQLAgent.LoadContextData_MineData(LContextData,FIdentifier);
       if(FAppModules.FieldProperties.UpdateFieldValue('SaltBuildUpRate',FloatToStr(Value),FloatToStr(FSaltBuildUpRate), LContextData)) then
       begin
         LOldValue := FloatToStr(FSaltBuildUpRate);
         FSaltBuildUpRate := Value;
         FAppModules.Model.StudyDataHasChanged(sdccEdit,'SaltBuildUpRate',LOldValue, FloatToStr(Value));
       end;
     finally
       FreeAndNil(LSQLAgent);
       FreeAndNil(LContextData);
     end;
  except on E: Exception do HandleError(E,OPNAME)  end;
end;

procedure TPlanningMine.Set_SaltWashOffEfficiencyFactor(Value: double);
const OPNAME = 'TPlanningMine.Set_SaltWashOffEfficiencyFactor';
var
  LSQLAgent : TPlanningMineSQLAgent;
  LContextData : TStringList;
  LOldValue : string;
begin
  try
     LSQLAgent := TPlanningMineSQLAgent.Create(FAppModules);
     LContextData := TStringList.Create();
     try
       LSQLAgent.LoadContextData_MineData(LContextData,FIdentifier);
       if(FAppModules.FieldProperties.UpdateFieldValue('SaltWashOffEfficiencyFactor',FloatToStr(Value),FloatToStr(FSaltWashOffEfficiencyFactor), LContextData)) then
       begin
         LOldValue := FloatToStr(FSaltWashOffEfficiencyFactor);
         FSaltWashOffEfficiencyFactor := Value;
         FAppModules.Model.StudyDataHasChanged(sdccEdit,'SaltWashOffEfficiencyFactor',LOldValue, FloatToStr(Value));
       end;
     finally
       FreeAndNil(LSQLAgent);
       FreeAndNil(LContextData);
     end;
  except on E: Exception do HandleError(E,OPNAME)  end;
end;

function TPlanningMine.Validate(var AErrors: WideString;
  const AContext: WideString): WordBool;
const OPNAME = 'TPlanningMine.Validate';
var
  LIndex : integer;
  LErrorMsgs: TStringList;
begin
  Result := True;
  try
    for LIndex := 0 to FOpenCastObjectContainer.Count-1 do
    begin
      if not TPlanningOpenCast(FOpenCastObjectContainer.Items[LIndex]).Validate(AErrors,AContext) then
        Result := False;
    end;
    for LIndex := 0 to FUnderGroundObjectContainer.Count-1 do
    begin
      if not TPlanningUnderGroundMine(FUnderGroundObjectContainer.Items[LIndex]).Validate(AErrors,AContext) then
        Result := False;
    end;
    for LIndex := 0 to FSlurryDumpObjectContainer.Count-1 do
    begin
      if not TPlanningSlurryDump(FSlurryDumpObjectContainer.Items[LIndex]).Validate(AErrors,AContext) then
        Result := False;
    end;
    try
      LErrorMsgs := TStringList.Create;
      if AContext = 'RainfallFileName' then Result := ValidateRainfallFileName(LErrorMsgs)
      else if AContext = 'AssocSaltWashoff' then Result := ValidateAssocSaltWashoff(LErrorMsgs)
      else if AContext = 'MeanAnnualPrecipitation' then       Result := ValidateMeanAnnualPrecipitation(LErrorMsgs)
      else if AContext = 'SaltBuildUpRate' then  Result := ValidateSaltBuildUpRate(LErrorMsgs)
      else if AContext = 'SaltWashOffEfficiencyFactor' then Result := ValidateSaltWashOffEfficiencyFactor(LErrorMsgs)
      else if AContext = 'IniSaltStore' then  Result := ValidateIniSaltStore(LErrorMsgs)
      else
      begin
        Result := ValidateRainfallFileName(LErrorMsgs) and ValidateAssocSaltWashoff(LErrorMsgs) and ValidateMeanAnnualPrecipitation(LErrorMsgs)
        and ValidateSaltBuildUpRate(LErrorMsgs) and ValidateSaltWashOffEfficiencyFactor(LErrorMsgs) and ValidateIniSaltStore(LErrorMsgs);
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

function TPlanningMine.ValidateAssocSaltWashoff(
  AErrorMessages: TStrings): Boolean;
const OPNAME  = 'TPlanningMine.ValidateAssocSaltWashoff';
var
  lMessage : string;
begin
  Result := False;
  try
      if (not (FAppModules.FieldProperties.ValidateFieldProperty('SaltWashOffNo',IntToStr(FAssocSaltWashoff),lMessage))) then
        AErrorMessages.Add(lMessage)
      else
        Result := true;
  except on E: Exception do HandleError(E, OPNAME)  end;
end;

function TPlanningMine.ValidateIniSaltStore(AErrorMessages: TStrings): boolean;
const OPNAME  = 'TPlanningMine.ValidateIniSaltStore';
var
  lMessage : string;
begin
  Result := False;
  try
      if (not (FAppModules.FieldProperties.ValidateFieldProperty('IniSaltStore',FloatToStr(FIniSaltStore),lMessage))) then
        AErrorMessages.Add(lMessage)
      else
        Result := true;
  except on E: Exception do HandleError(E, OPNAME)  end;
end;

function TPlanningMine.ValidateMeanAnnualPrecipitation(
  AErrorMessages: TStrings): Boolean;
const OPNAME  = 'TPlanningMine.ValidateMeanAnnualPrecipitation';
var
  lMessage : string;
begin
  Result := False;
  try
      if (not (FAppModules.FieldProperties.ValidateFieldProperty('MeanAnnualPrecipitation',FloatToStr(FMeanAnnualPrecipitation),lMessage))) then
        AErrorMessages.Add(lMessage)
      else
        Result := true;
  except on E: Exception do HandleError(E, OPNAME)  end;
end;

function TPlanningMine.ValidateRainfallFileName(
  AErrorMessages: TStrings): Boolean;
const OPNAME  = 'TPlanningMine.ValidateRainfallFileName';
var
  lMessage : string;
begin
  Result := False;
  try
      if (not (FAppModules.FieldProperties.ValidateFieldProperty('PlanningMineRainfallFile',FRainfallFileName,lMessage))) then
        AErrorMessages.Add(lMessage)
      else
        Result := true;
  except on E: Exception do HandleError(E, OPNAME)  end;
end;

function TPlanningMine.ValidateSaltBuildUpRate(
  AErrorMessages: TStrings): Boolean;
const OPNAME  = 'TPlanningMine.ValidateSaltBuildUpRate';
var
  lMessage : string;
begin
  Result := False;
  try
      if (not (FAppModules.FieldProperties.ValidateFieldProperty('SaltBuildUpRate',FloatToStr(FSaltBuildUpRate),lMessage))) then
        AErrorMessages.Add(lMessage)
      else
        Result := true;
  except on E: Exception do HandleError(E, OPNAME)  end;
end;

function TPlanningMine.ValidateSaltWashOffEfficiencyFactor(
  AErrorMessages: TStrings): boolean;
const OPNAME  = 'TPlanningMine.ValidateSaltWashOffEfficiencyFactor';
var
  lMessage : string;
begin
  Result := False;
  try
      if (not (FAppModules.FieldProperties.ValidateFieldProperty('SaltWashOffEfficiencyFactor',FloatToStr(FSaltWashOffEfficiencyFactor),lMessage))) then
        AErrorMessages.Add(lMessage)
      else
        Result := true;
  except on E: Exception do HandleError(E, OPNAME)  end;
end;

procedure TPlanningMine.Populate(AIdentifier: Integer;
                       ANodeNumber: Integer;
                       AMineName : WideString;
                       ARiverChannelNumber: Integer;
                       APCDChannelNumber: Integer;
                       AHydrologyNodeNumber: integer;
                       ABeneficiationPlantArea: double;
                       ABeneficiationRunOffFactor: double;
                       AAssocSaltWashoff: integer;
                       ARainfallFileName: string;
                       AMeanAnnualPrecipitation: Single;
                       ASaltBuildUpRate: Single;
                       ASaltWashOffEfficiencyFactor: Single;
                       AIniSaltStore: Single);
const OPNAME = 'TPlanningMine.Populate Public overload';
begin
  try
    inherited Populate(AIdentifier,ANodeNumber,AMineName,ARiverChannelNumber,APCDChannelNumber,
                       AHydrologyNodeNumber,ABeneficiationPlantArea,ABeneficiationRunOffFactor);
    Populate(AAssocSaltWashoff,ARainFallFileName, AMeanAnnualPrecipitation, ASaltBuildUpRate,
             ASaltWashOffEfficiencyFactor,AIniSaltStore);
  except on E: Exception do HandleError(E,OPNAME); end;
end;


function TPlanningMine.RemoveOpenCast(AOpenCastID: integer): WordBool;
const OPNAME ='TPlanningMine.RemoveOpenCast';
var
  LOpenCast: TPlanningOpenCast;
  LSQLAgent : TPlanningMineSQLAgent;
begin
  Result := False;
  try
     LSQLAgent := TPlanningMineSQLAgent.Create(FAppModules);
    try
      LOpenCast := Get_CastOpenCastByID(AOpenCastID);
      if(LOpenCast <> nil) then
      begin
        if LSQLAgent.DeleteOpenCast(LOpenCast) then
        begin
          FOpenCastObjectContainer.Remove(LOpenCast);
          Result := True;
        end;
      end;
    finally
      LSQLAgent.Free;
    end;
  except on E: Exception do HandleError(E,OPNAME); end;
end;

function TPlanningMine.RemoveSlurryDump(ASlurryDumpID: integer): WordBool;
const OPNAME ='TPlanningMine.RemoveOpenCast';
var
  LSlurry: TPlanningSlurryDump;
  LSQLAgent : TPlanningMineSQLAgent;
begin
  Result := False;
  try
     LSQLAgent := TPlanningMineSQLAgent.Create(FAppModules);
    try
      LSlurry := Get_CastSlurryDumpByID(ASlurryDumpID);
      if(LSlurry <> nil) then
      begin
        if LSQLAgent.DeleteSlurryDump(LSlurry) then
        begin
          FSlurryDumpObjectContainer.Remove(LSlurry);
          Result := True;
        end;
      end;
    finally
      LSQLAgent.Free;
    end;
  except on E: Exception do HandleError(E,OPNAME); end;
end;


procedure TPlanningMine.Populate(AAssocSaltWashoff: integer;
                       ARainfallFileName: string;
                       AMeanAnnualPrecipitation: Single;
                       ASaltBuildUpRate: Single;
                       ASaltWashOffEfficiencyFactor: Single;
                       AIniSaltStore: Single);
const OPNAME = 'TPlanningMine.Populate overload';
begin
  try
    FAssocSaltWashoff := AAssocSaltWashoff;
    FRainfallFileName :=  ARainfallFileName;
    FMeanAnnualPrecipitation := AMeanAnnualPrecipitation;
    FSaltBuildUpRate := ASaltBuildUpRate;
    FSaltWashOffEfficiencyFactor := ASaltWashOffEfficiencyFactor;
    FIniSaltStore := AIniSaltStore;
    //FGrowthFactor.Populate(FGrowthFactor.Identifier,Identifier,FGrowthFactor.OpenCastIdentifier,FGrowthFactor.SlurryDumpIdentifier,
    //                      FGrowthFactor.UnderGroundIdentifier,FGrowthFactor.NoOfPoints,FGrowthFactor.FactorType,
    //                     StrToInt(ConditionalOpp( FGrowthFactor.InterpolationMethod,2,1)),FGrowthFactor.ListToStringYears,
    //                      FGrowthFactor.ListToStringGrowthFactor);
  except on E: Exception do HandleError(E,OPNAME); end;
end;

//===================================================================================//
//                            TPlanningMineList                                      //
//===================================================================================//

(*procedure TPlanningMineList.CreateMemberObjects;
const OPNAME = 'TPlanningMineList.CreateMemberObjects';
begin
  try
     inherited CreateMemberObjects;
  except on E: Exception do HandleError(E,OPNAME); end;
end;

procedure TPlanningMineList.DestroyMemberObjects;
const OPNAME = ' TPlanningMineList.DestroyMemberObjects';
begin
  try
    inherited DestroyMemberObjects;
  except on E: Exception do HandleError(E,OPNAME); end;

end;

function TPlanningMineList.Get_MineByIdentifier(AMineID: Integer): IPlanningMine;
const OPNAME = 'TPlanningMineList.Get_MineByIdentifier';
begin
  Result := nil;
  try
    Result := Get_CastMineByID(AMineID);
  except on E: Exception do HandleError(E,OPNAME); end;
end;

function TPlanningMineList.Get_MineByIndex(AIndex: Integer): IPlanningMine;
const OPNAME = 'TPlanningMineList.Get_MineByIndex';
begin
  Result := nil;
  try
    Result := Get_CastMineByIndex(AIndex);
  except on E: Exception do HandleError(E,OPNAME);  end;
end;

function TPlanningMineList.Get_MineByNodeNumber(ANodeNumber: Integer): IPlanningMine;
const OPNAME = 'TPlanningMineList.Get_MineByNodeNumber';
begin
  Result := nil;
  try
    Result := Get_CastMineByNodeNumber(ANodeNumber);
  except on E: Exception do HandleError(E,OPNAME); end;
end;

function TPlanningMineList.Get_MineByPCDNumber(APCDNumber: Integer): IPlanningMine;
const OPNAME ='TPlanningMineList.Get_MineByPCDNumber';
begin
  Result := nil;
  try
    Result := Get_CastMineByPCDNumber(APCDNumber);
  except on E: Exception do HandleError(E,OPNAME);  end;
end;

function TPlanningMineList.Get_CastMineByPCDNumber(APCDNumber: Integer) :TPlanningMine;
const OPNAME = 'TPlanningMineList.Get_CastMineByPCDNumber';
var
  LIndex : Integer;
  LMine : TPlanningMine;
begin
  Result := nil;
  try
  LIndex := 0;
  while ((Result = nil) and (LIndex < FMinesObjectContainer.Count)) do
  begin
    LMine := TPlanningMine(FMinesObjectContainer.Items[LIndex]);
    if (LMine.PCDNodelNumber = APCDNumber) then
      Result := LMine
    else
      LIndex := LIndex +1;
  end;
  except on E: Exception do HandleError(E,OPNAME);  end;
end;

function TPlanningMineList.Get_CastMineByID(AMineID: Integer): TPlanningMine;
const OPNAME = 'TPlanningMineList.Get_CastMineByID';
var
  LIndex    : Integer;
  LMine  : TPlanningMine;
begin
  Result := nil;
  try
    LIndex := 0;
    while ((Result = nil) and (LIndex < FMinesObjectContainer.Count)) do
    begin
      LMine := TPlanningMine(FMinesObjectContainer.Items[LIndex]);
      if (LMine.Identifier = AMineID) then
        Result := LMine
      else
        LIndex := LIndex + 1;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TPlanningMineList.Get_CastMineByIndex(AIndex: Integer): TPlanningMine;
const OPNAME = 'TPlanningMineList.Get_CastMineByIndex';
begin
  Result := nil;
  try
    if(AIndex >= 0) and (AIndex < FMinesObjectContainer.Count) then
      Result := TPlanningMine(FMinesObjectContainer[AIndex]);
  except on  E: Exception do HandleError(E,OPNAME); end;
end;

function TPlanningMineList.Get_CastMineByNodeNumber(ANodeNumber: Integer): TPlanningMine;
const OPNAME = 'TPlanningMineList.Get_CastMineByNodeNumber';
var
  LIndex    : Integer;
//  LMine     : TPlanningMine;
begin
  Result := nil;
  try
    LIndex := 0;
    while ((Result = nil) and (LIndex < FMinesObjectContainer.Count)) do
    begin
      Result := TPlanningMine(FMinesObjectContainer.Items[LIndex]);
      if(Result.NodeNumber = ANodeNumber ) then Exit
      else
        LIndex := LIndex +1;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TPlanningMineList.CreateMine;
const OPNAME = ' TPlanningMineList.CreateMine';
var
  LSQLAgent      : TPlanningMineSQLAgent;
  LMine          : TPlanningMine;
  LMineNode      : IReservoirData;
  LMineToRiver   : IGeneralFlowChannel;
  LMinMaxFeature : IMinMaxFlowConstraint;
  LContinue      : boolean;
  LMineNodeNumber,
  LNewMineID,
  LIdentifier    : integer;
begin
  Result := nil;
  try
    if(FMinesObjectContainer.Count >= 100) then
      Exit;

    LSQLAgent := TPlanningMineSQLAgent.Create(FAppModules);
    try
      LMine           := nil;
      LMineNode       := nil;
      LMineToRiver    := nil;
      LMinMaxFeature  := nil;
      LContinue       := True;
      LMineNodeNumber := 0;
      LNewMineID      := LSQLAgent.GetMaxMineID + 1;

      //Create mine node
      if LContinue  then
      begin
        LMineNode := (FAppModules.Model.ModelData as IYieldModelData).NetworkElementData.ReservoirList.CreateNodeWithoutInflow(ntMineNode);
        LContinue := (LMineNode <> nil);
      end;


      //Create channel to River
      if LContinue  then
      begin
        LMineToRiver := (FAppModules.Model.ModelData as IYieldModelData).NetworkElementData.ChannelList.CreateChannel;
        LContinue := (LMineToRiver <> nil);
      end;

      //Create channel to River min-max feature
      if LContinue  then
      begin
        LMineToRiver.UpStreamNodeNumber   := LMineNode.ReservoirConfigurationData.ReservoirIdentifier;
        LMinMaxFeature :=  (FAppModules.Model as IYieldModel).DoCreateMinMaxFlowFeature(LMineToRiver.ChannelNumber);
        LMineToRiver.ChannelType := ctMineToRiverDChannel;
        LMineToRiver.ChannelName := IntToStr(LMineToRiver.ChannelNumber)+ ' - Mine '+ IntToStr(LMineNodeNumber) +' To River';
        LContinue := (LMinMaxFeature <> nil);
      end;

      //Create mine
      if LContinue  then
      begin
        LMine := NewMine;
        LContinue := (LMine <> nil)
      end;

      //Populate mine/channels
      if LContinue  then
      begin
        LMine.FIdentifier                := LNewMineID;
        LMine.FRiverChannelNumber        := LMineToRiver.ChannelNumber;
        LMine.FNodeNumber                := LMineNode.ReservoirConfigurationData.ReservoirIdentifier;
        LMine.FPCDChannelNumber          := 0;
        LMine.FMineName                  := 'Mine '+IntToStr(LMine.FNodeNumber);
        LMine.Populate(0,'',0.0,0.0,0.0,0.0);
        LContinue := LSQLAgent.AddMine(LMine);
      end;

      if not LContinue  then
      begin
        if(LMine <> nil) then
        begin
          LSQLAgent.DeleteMine(LMine);
          FMinesObjectContainer.Remove(LMine)
        end;

        if(LMineNode <> nil) then
        begin
          LIdentifier := LMineNode.ReservoirConfigurationData.ReservoirIdentifier;
          LMineNode := nil;
          (FAppModules.Model.ModelData as IYieldModelData).NetworkElementData.ReservoirList.DeleteNodeWithoutInflow(LIdentifier);
        end;

        if(LMineToRiver <> nil) then
        begin
          LIdentifier := LMineToRiver.ChannelNumber;
          LMineToRiver := nil;
          (FAppModules.Model as IYieldModel).DoConvertChannel(LIdentifier);
          (FAppModules.Model.ModelData as IYieldModelData).NetworkElementData.ChannelList.RemoveChannelWithNumber(LIdentifier);
        end;
      end
      else
        Result := LMine;
    finally
      LSQLAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TPlanningMineList.NewMine: TPlanningMine;
const OPNAME ='TPlanningMineList.NewMine';
begin
  Result := nil;
  try
    Result := TPlanningMine.Create(FAppModules);
    Result.Initialise;
    FMinesObjectContainer.Add(Result);
  except on E: Exception do HandleError(E,OPNAME) end;

end;

function TPlanningMineList.RemoveMine(AMineNumber: integer): WordBool;
const OPNAME = 'TPlanningMineList.RemoveMine';
var
  LMine     : TPlanningMine;
  LSQLAgent : TPlanningMineSQLAgent;
  LIndex    : integer;
begin
  Result := False;
  try
    LSQLAgent := TPlanningMineSQLAgent.Create(FAppModules);
    try
      LMine := Get_CastMineByNodeNumber(AMineNumber);

      if(LMine <> nil) then
      begin
        LMine.RemovePolutionControlDam;
        (FAppModules.Model.ModelData as IYieldModelData).NetworkElementData.ReservoirList.DeleteNodeWithoutInflow(LMine.NodeNumber);
        (FAppModules.Model as IYieldModel).DoConvertChannel(LMine.RiverChannelNumber);
        (FAppModules.Model.ModelData as IYieldModelData).NetworkElementData.ChannelList.RemoveChannelWithNumber(LMine.RiverChannelNumber);

        for LIndex := LMine.OpenCastCount-1 downto 0 do
        begin
          LMine.RemoveOpenCast(LMine.OpenCastIdentifierByIndex[LIndex])
        end;
        for LIndex := LMine.UndergroundCount-1 downto 0 do
        begin
          LMine.RemoveUnderGround(LMine.UnderGroundIdentifierByIndex[LIndex])
        end;
        for LIndex := LMine.SlurryDumpCount-1 downto 0 do
        begin
          LMine.RemoveSlurryDump(LMine.SlurryDumpIdentifierByIndex[LIndex])
        end;

        LSQLAgent.DeleteMine(LMine);
        FMinesObjectContainer.Remove(LMine);
        Result := True;
      end;
    finally
      LSQLAgent.Free;
    end;
  except on E: Exception do HandleError(E,OPNAME); end;
end;

function TPlanningMineList.CopyCreate(AMineNumber : integer): IPlanningMine;
const OPNAME = 'TPlanningMineList.CopyCreate';
begin
  Result := nil;
  try
  except on E: Exception do HandleError(E,OPNAME) end;
end;*)
end.

