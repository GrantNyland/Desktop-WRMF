(******************************************************************************)
(* Contains : TRunOffDBManager.                                               *)
(* Contains Database functionality for RunOffs.                               *)
(******************************************************************************)

unit URunOffDBManager;

interface

uses
  Classes, Contnrs,

  UAbstractObject,
  UModuleDBManager,
  URunOffModule;

type

  TRunOffDBManager = class(TModuleDBManager)
  protected
    function UpdateAfforestationParamsInDB (AModuleID                 : Integer;
                                            AAfforestationAlgorithm   : Integer;
                                            APineAreaPercentage       : Double;
                                            APineRotationPeriod       : Integer;
                                            AEucalyptusAreaPercentage : Double;
                                            AEucalyptusRotationPeriod : Integer;
                                            AWattleAreaPercentage     : Double;
                                            AWattleRotationPeriod     : Integer;
                                            AOptimalAreaPercentage    : Double;
                                            ASFRReductionMAR          : Double;
                                            ASFRReductionLowFlows     : Double): Boolean;
    function UpdateAlienVegetationParamsInDB (AModuleID                  : Integer;
                                              AAlienVegetationAlgorithm  : Integer;
                                              ARiparianVegetationArea    : Double;
                                              ATallTreeAreaPercentage    : Double;
                                              ATallTreeAge               : Double;
                                              AMediumTreeAreaPercentage  : Double;
                                              AMediumTreeAge             : Double;
                                              ATallSchrubAreaPercentage  : Double;
                                              ATallSchrubAge             : Double;
                                              AOptimalAreaPercentage     : Double): Boolean;
    function UpdateRunOffOutflowRouteInDB (AModuleID, ARouteNo : Integer; AOutflowPercentage : Double) : Boolean;
  public
    function LoadRunOffModulesFromDB (ANetworkID : Integer) : Boolean;
    function LoadRunOffSlavesFromDB (ARunOff : TRunOffModule) : Boolean;
    function LoadAfforestationFromDB (ARunOff : TRunOffModule) : Boolean;
    function LoadAlienVegetationFromDB (ARunOff : TRunOffModule) : Boolean;
    function LoadPavedAreaFromDB (ARunOff : TRunOffModule) : Boolean;
    function LoadGroundWaterAbstractionFromDB (ARunOff : TRunOffModule) : Boolean;
    function LoadHughesModelFromDB (ARunOff : TRunOffModule) : Boolean;
    function LoadSamiModelFromDB (ARunOff : TRunOffModule) : Boolean;
    function LoadOutflowRoutesFromDB (ARunOff : TRunOffModule) : Boolean;

    function InsertRunOffOutflowRouteInDB(AModuleID, ARouteNo : Integer; AOutflowPerc: Double) : Boolean;
    function InsertAfforestationAreaDataInDB (AModuleID, AYear : Integer; AArea : Double) : Boolean;
    function InsertAlienVegetationAreaDataInDB (AModuleID, AYear : Integer; AArea : Double) : Boolean;
    function InsertPavedAreaDataInDB (AModuleID, AYear : Integer; AProportion : Double) : Boolean;
    function InsertGroundwaterAbstractionDataInDB (AModuleID, AYear : Integer; AAbstraction : Double) : Boolean;
    function InsertSlaveInDB (AModuleID, ASlaveModuleNo : Integer): Boolean;
    function InsertRunOffIntoDB (AModuleID : Integer; AName : String) : Boolean;
    function InsertAfforestationParamsIntoDB (AModuleID : Integer): Boolean;
    function InsertAlienVegetationParamsIntoDB (AModuleID : Integer): Boolean;
    function InsertHughesModelIntoDB (AModuleID : Integer): Boolean;
    function InsertSamiModelIntoDB (AModuleID : Integer): Boolean;

    function DeleteRunOffModuleFromDB (AModuleID : Integer) : Boolean;
    function DeleteRunOffSlavesFromDB (AModuleID : Integer) : Boolean;
    function DeleteAfforestationParamsFromDB (AModuleID : Integer) : Boolean;
    function DeleteAfforestationDataFromDB (AModuleID : Integer) : Boolean;
    function DeleteAlienVegetationParamsFromDB (AModuleID : Integer) : Boolean;
    function DeleteAlienVegetationDataFromDB (AModuleID : Integer) : Boolean;
    function DeletePavedAreaFromDB (AModuleID : Integer) : Boolean;
    function DeleteGroundWaterAbstractionFromDB (AModuleID : Integer) : Boolean;
    function DeleteHughesModelFromDB (AModuleID : Integer) : Boolean;
    function DeleteSamiModelFromDB (AModuleID : Integer) : Boolean;
    function DeleteRunOffModuleOutflowRoutesFromDB (AModuleID : Integer) : Boolean;
    function DeleteRunOffOutflowRouteFromDB(AModuleID, ARouteNo : Integer) : Boolean;

    function UpdatePropertiesDataInDB (AModuleID         : Integer;
                                       AActive           : String;
                                       ALatitude         : Double;
                                       ALongitude        : Double;
                                       ARunOffName       : String;
                                       AVersionNo        : Integer;
                                       ACatchmentMAP     : Double;
                                       ACatchmentArea    : Double;
                                       ARainfallFileName : String;
                                       AProduceNatFlows  : Integer;
                                       AAPanFactor       : TMonthlyDoubleArray): Boolean;
    function UpdatePitmanDataInDB (AModuleID   : Integer;
                                   APitmanPOW  : Double;
                                   APitmanSL   : Integer;
                                   APitmanST   : Integer;
                                   APitmanFT   : Double;
                                   APitmanGW   : Double;
                                   APitmanZMIN : Integer;
                                   APitmanZMAX : Integer;
                                   APitmanPI   : Double;
                                   APitmanTL   : Double;
                                   APitmanGL   : Double;
                                   APitmanR    : Double;
                                   APitmanFF   : Double): Boolean;
    function UpdateAfforestationDataInDB (AModuleID                 : Integer;
                                          AAfforestationAlgorithm   : Integer;
                                          APineAreaPercentage       : Double;
                                          APineRotationPeriod       : Integer;
                                          AEucalyptusAreaPercentage : Double;
                                          AEucalyptusRotationPeriod : Integer;
                                          AWattleAreaPercentage     : Double;
                                          AWattleRotationPeriod     : Integer;
                                          AOptimalAreaPercentage    : Double;
                                          ASFRReductionMAR          : Double;
                                          ASFRReductionLowFlows     : Double;
                                          AYearList                 : TStringList;
                                          AAreaList                 : TStringList): Boolean;
    function UpdateAlienVegetationDataInDB (AModuleID                  : Integer;
                                            AAlienVegetationAlgorithm  : Integer;
                                            ARiparianVegetationArea    : Double;
                                            ATallTreeAreaPercentage    : Double;
                                            ATallTreeAge               : Double;
                                            AMediumTreeAreaPercentage  : Double;
                                            AMediumTreeAge             : Double;
                                            ATallSchrubAreaPercentage  : Double;
                                            ATallSchrubAge             : Double;
                                            AOptimalAreaPercentage     : Double;
                                            AYearList                  : TStringList;
                                            AAreaList                  : TStringList): Boolean;
    function UpdatePavedAreaDataInDB (AModuleID       : Integer;
                                      AYearList       : TStringList;
                                      AProportionList : TStringList) : Boolean;
    function UpdateGroundwaterAbstractionDataInDB (AModuleID        : Integer;
                                                   AYearList        : TStringList;
                                                   AAbstractionList : TStringList) : Boolean;
    function UpdateOutflowRoutesDataInDB (AModuleID       : Integer;
                                          ARouteNoList    : TStringList;
                                          APercentageList : TStringList) : Boolean;
    function UpdateHughesDataInDB (AModuleID                      : Integer;
                                   AInflowRouteNo                 : Integer;
                                   AInfluenceROMNo                : Integer;
                                   AGroundWaterModel              : Integer;
                                   AHughesHGSL                    : Double;
                                   AHughesGPOW                    : Double;
                                   AHughesTLGMax                  : Double;
                                   AHughesHGGW                    : Double;
                                   AHughesPOW                     : Double;
                                   AHughesSL                      : Integer;
                                   AHughesST                      : Integer;
                                   AHughesFT                      : Double;
                                   AHughesGW                      : Double;
                                   AHughesZMIN                    : Integer;
                                   AHughesZMAX                    : Integer;
                                   AHughesPI                      : Double;
                                   AHughesTL                      : Double;
                                   AHughesGL                      : Double;
                                   AHughesR                       : Double;
                                   AHughesFF                      : Double;
                                   AUseNoOfReaches                : Integer;
                                   ADrainageDensity               : Double;
                                   ANumberOfReaches               : Integer;
                                   ARiparianAreaWidthPercentage   : Double;
                                   ARiparianStripFactor           : Double;
                                   ARestWaterlevel                : Double;
                                   ATransmissivity                : Double;
                                   AStorativity                   : Double;
                                   AGroundwaterSlope              : Double;
                                   AAnnualUpperZoneAbstraction    : Double;
                                   AAnnualRiparianZoneAbstraction : Double;
                                   AUpperZoneDemand               : TMonthlyDoubleArray;
                                   ARiparianZoneDemand            : TMonthlyDoubleArray): Boolean;
    function UpdateSamiDataInDB (AModuleID                      : Integer;
                                 AAquiferThickness              : Double;
                                 AStorativity                   : Double;
                                 AInitialAquiferStorage         : Double;
                                 AStaticWaterLevel              : Double;
                                 AUnsaturatedStorage            : Double;
                                 AInitialUnsaturatedZoneStorage : Double;
                                 APerculationPower              : Double;
                                 AMaxDischarge                  : Double;
                                 AInteractionCurvePower         : Double;
                                 AMaxHydrologicalGradient       : Double;
                                 ATransmissivity                : Double;
                                 ABoreholeDistanceToRiver       : Double;
                                 AGroundWaterEvaporationArea    : Double;
                                 AInterflowLag                  : Double;
                                 ARechargeAveragedNoMonths      : Double;
                                 AUseAbstractions               : Integer;
                                 ASamiGPOW                      : Double;
                                 ASamiHGSL                      : Double;
                                 ASamiHGGW                      : Double;
                                 ASamiK2                        : Double;
                                 ASamiK3                        : Double;
                                 ASamiPOW                       : Double;
                                 ASamiSL                        : Integer;
                                 ASamiST                        : Integer;
                                 ASamiFT                        : Double;
                                 ASamiGW                        : Double;
                                 ASamiZMIN                      : Integer;
                                 ASamiZMAX                      : Integer;
                                 ASamiPI                        : Double;
                                 ASamiTL                        : Double;
                                 ASamiGL                        : Double;
                                 ASamiR                         : Double;
                                 ASamiFF                        : Double): Boolean;
    function UpdateSlavesDataInDB (AModuleID : Integer; ASlaveNoList : TStringList): Boolean;
    function CreateNewRunOffModuleInDB (ANetworkID: Integer) : TRunOffModule;
    function RemoveRunOffModuleFromDB (AModuleID : Integer) : Boolean;

  end;

var
  GRunOffDBManager : TRunOffDBManager;

implementation


uses
  SysUtils,
  Windows,
  DB,

  UHydroDBAgent,
  UErrorHandlingOperations;

(* TRunOffDBManager ***********************************************************)

function TRunOffDBManager.LoadRunOffModulesFromDB (ANetworkID : Integer) : Boolean;
const OPNAME = 'TRunOffDBManager.LoadRunOffModulesFromDB';
var
  LQuery        : TDataSet;
  LSQL          : String;
  LRunOffModule : TRunOffModule;
  LModuleID     : Integer;
  LAPanFactor   : WideString;
  LIndex        : Integer;
  LFieldName    : String;
begin
  Result := FALSE;
  try
    LSQL := 'SELECT A.ModuleNumber, A.NetworkSequence, A.Active, B.*, C.ModuleType, C.Longitude, C.Latitude ' +
            'FROM (NetworkModules AS A LEFT JOIN RunOffModules AS B ON A.ModuleID = B.ModuleID) ' +
            'LEFT JOIN Modules AS C ON A.ModuleID = C.ModuleID WHERE C.ModuleType = ' +  QuotedStr('RU') +
            ' AND A.NetworkID = ' + IntToStr(ANetworkID);
    LQuery := GHydroDBAgent.CreateQuery(LSQL);
    try
      LQuery.Open;
      while (NOT LQuery.Eof) do
      begin
        LModuleID := LQuery.FieldByName('ModuleID').AsInteger;
        LRunOffModule := TRunOffModuleAgent(ModuleAgent).AddRunOffModule;
        LAPanFactor := '';
        for LIndex := 1 to 12 do
        begin
          LFieldName := Format('APanFactor%2.2d', [LIndex]);
          LAPanFactor := LAPanFactor + ',' + Trim(LQuery.FieldByName(LFieldName).AsString);
        end;
        LAPanFactor := Copy(LAPanFactor, 2, Length(LAPanFactor)-1);

        LRunOffModule.Populate(ANetworkID,
                               LModuleID,
                               Trim(LQuery.FieldByName('ModuleType').AsString),
                               LQuery.FieldByName('ModuleNumber').AsInteger,
                               LQuery.FieldByName('NetworkSequence').AsInteger,
                               Trim(LQuery.FieldByName('Active').AsString),
                               Trim(LQuery.FieldByName('RunOffName').AsString),
                               LQuery.FieldByName('VersionNo').AsInteger,
                               LQuery.FieldByName('CatchmentArea').AsFloat,
                               LQuery.FieldByName('CatchmentMAP').AsInteger,
                               Trim(LQuery.FieldByName('RainfallFileName').AsString),
                               LQuery.FieldByName('ProduceNaturalisedFlows').AsInteger,
                               LQuery.FieldByName('Longitude').AsFloat,
                               LQuery.FieldByName('Latitude').AsFloat,
                               LAPanFactor);
        GRunOffDBManager.LoadPanDataFromDB(LRunOffModule);
        LRunOffModule.PitmanModel.Populate(LQuery.FieldByName('PitmanPOW').AsFloat,
                                           LQuery.FieldByName('PitmanSL').AsInteger,
                                           LQuery.FieldByName('PitmanST').AsInteger,
                                           LQuery.FieldByName('PitmanFT').AsFloat,
                                           LQuery.FieldByName('PitmanGW').AsFloat,
                                           LQuery.FieldByName('PitmanZMIN').AsInteger,
                                           LQuery.FieldByName('PitmanZMAX').AsInteger,
                                           LQuery.FieldByName('PitmanPI').AsFloat,
                                           LQuery.FieldByName('PitmanTL').AsFloat,
                                           LQuery.FieldByName('PitmanGL').AsFloat,
                                           LQuery.FieldByName('PitmanR').AsFloat,
                                           LQuery.FieldByName('FF').AsFloat);
        LoadAfforestationFromDB(LRunOffModule);
        LoadAlienVegetationFromDB(LRunOffModule);
        LoadPavedAReaFromDB(LRunOffModule);
        LoadOutflowRoutesFromDB(LRunOffModule);
        LoadGroundWaterAbstractionFromDB(LRunOffModule);
        LoadRunOffSlavesFromDB(LRunOffModule);
        LoadHughesModelFromDB(LRunOffModule);
        LoadSamiModelFromDB(LRunOffModule);

        LQuery.Next;
      end;
    finally
      LQuery.Close;
      LQuery.Free;
    end;
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TRunOffDBManager.InsertRunOffOutflowRouteInDB(AModuleID, ARouteNo : Integer; AOutflowPerc: Double) : Boolean;
const OPNAME = 'TRunOffDBManager.InsertRunOffOutflowRouteInDB';
var
  LSQL   : String;
begin
  Result := FALSE;
  try
    LSQL := 'INSERT INTO RunOffOutflowRoutes (ModuleID, RouteNo, OutflowPercentage) VALUES (' +
            IntToStr(AModuleID) + ', ' + IntToStr(ARouteNo) + ', ' + FloatToStr(AOutflowPerc) + ')';
    Result := GHydroDBAgent.ExecuteSQL(LSQL, True);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TRunOffDBManager.DeleteRunOffOutflowRouteFromDB(AModuleID, ARouteNo : Integer) : Boolean;
const OPNAME = 'TRunOffDBManager.DeleteRunOffOutflowRouteFromDB';
var
  LSQL : String;
begin
  Result := FALSE;
  try
    LSQL := 'DELETE * FROM RunOffOutflowRoutes WHERE ModuleID = ' + IntToStr(AModuleID) +
            ' AND RouteNo = ' + IntToStr(ARouteNo);
    Result := GHydroDBAgent.ExecuteSQL(LSQL, True);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TRunOffDBManager.UpdatePropertiesDataInDB (AModuleID         : Integer;
                                                    AActive           : String;
                                                    ALatitude         : Double;
                                                    ALongitude        : Double;
                                                    ARunOffName       : String;
                                                    AVersionNo        : Integer;
                                                    ACatchmentMAP     : Double;
                                                    ACatchmentArea    : Double;
                                                    ARainfallFileName : String;
                                                    AProduceNatFlows  : Integer;
                                                    AAPanFactor       : TMonthlyDoubleArray): Boolean;
const OPNAME = 'TRunOffDBManager.UpdatePropertiesDataInDB';
var
  LSQL       : String;
  LMonth     : Integer;
  LFieldName : String;
begin
  Result := FALSE;
  try
    LSQL := 'UPDATE (Modules INNER JOIN RunOffModules ON Modules.ModuleID = RunOffModules.ModuleID) ' +
            'INNER JOIN NetworkModules ON Modules.ModuleID = NetworkModules.ModuleID SET' +
            ', NetworkModules.Active = '                 + QuotedStr(AActive) +
            ', Modules.Latitude = '                      + FloatToStr(ALatitude) +
            ', Modules.Longitude = '                     + FloatToStr(ALongitude) +
            ', RunOffModules.RunOffName = '              + QuotedStr(ARunOffName) +
            ', RunOffModules.VersionNo = '               + IntToStr(AVersionNo) +
            ', RunOffModules.CatchmentArea = '           + FloatToStr(ACatchmentArea) +
            ', RunOffModules.CatchmentMAP = '            + FloatToStr(ACatchmentMAP) +
            ', RunOffModules.ProduceNaturalisedFlows = ' + IntToStr(AProduceNatFlows);
    if (ARainfallFileName = '') then
      LSQL := LSQL + ', RunOffModules.RainfallFileName = NULL'
    else
      LSQL := LSQL + ', RunOffModules.RainfallFileName = ' + QuotedStr(ARainfallFileName);

    for LMonth := 1 to 12 do
    begin
      LFieldName := Format('APanFactor%2.2d', [LMonth]);
      LSQL := LSQL + ', RunOffModules.' + LFieldName + ' = ' + FloatToStr(AAPanFactor[LMonth]);
    end;
    LSQL := LSQL + ' WHERE Modules.ModuleID = ' + IntToStr(AModuleID);
    Result := GHydroDBAgent.ExecuteSQL(LSQL, True);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRunOffDBManager.InsertSlaveInDB (AModuleID, ASlaveModuleNo : Integer): Boolean;
const OPNAME = 'TRunOffDBManager.InsertSlaveInDB';
var
  LSQL : String;
begin
  Result := FALSE;
  try
    LSQL := 'INSERT INTO RunOffModuleSlaves VALUES (' + IntToStr(AModuleID) + ', ' + IntToStr(ASlaveModuleNo) + ')';
    Result := GHydroDBAgent.ExecuteSQL(LSQL, True);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRunOffDBManager.UpdatePitmanDataInDB (AModuleID   : Integer;
                                                APitmanPOW  : Double;
                                                APitmanSL   : Integer;
                                                APitmanST   : Integer;
                                                APitmanFT   : Double;
                                                APitmanGW   : Double;
                                                APitmanZMIN : Integer;
                                                APitmanZMAX : Integer;
                                                APitmanPI   : Double;
                                                APitmanTL   : Double;
                                                APitmanGL   : Double;
                                                APitmanR    : Double;
                                                APitmanFF   : Double): Boolean;
const OPNAME = 'TRunOffDBManager.UpdatePitmanData';
var
  LSQL         : String;
begin
  Result := TRUE;
  try
    LSQL := 'UPDATE RunOffModules SET' +
            ' PitmanPOW = '   + FloatToStr(APitmanPOW) +
            ', PitmanSL = '   + IntToStr(APitmanSL) +
            ', PitmanST = '   + IntToStr(APitmanST) +
            ', PitmanFT = '   + FloatToStr(APitmanFT) +
            ', PitmanGW = '   + FloatToStr(APitmanGW) +
            ', PitmanZMIN = ' + IntToStr(APitmanZMIN) +
            ', PitmanZMAX = ' + IntToStr(APitmanZMAX) +
            ', PitmanPI = '   + FloatToStr(APitmanPI) +
            ', PitmanTL = '   + FloatToStr(APitmanTL) +
            ', PitmanGL = '   + FloatToStr(APitmanGL) +
            ', PitmanR = '    + FloatToStr(APitmanR) +
            ', FF = '         + FloatToStr(APitmanFF) +
            ' WHERE ModuleID = ' + IntToStr(AModuleID);
    Result := GHydroDBAgent.ExecuteSQL(LSQL, True);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRunOffDBManager.UpdateAfforestationDataInDB (AModuleID                 : Integer;
                                                       AAfforestationAlgorithm   : Integer;
                                                       APineAreaPercentage       : Double;
                                                       APineRotationPeriod       : Integer;
                                                       AEucalyptusAreaPercentage : Double;
                                                       AEucalyptusRotationPeriod : Integer;
                                                       AWattleAreaPercentage     : Double;
                                                       AWattleRotationPeriod     : Integer;
                                                       AOptimalAreaPercentage    : Double;
                                                       ASFRReductionMAR          : Double;
                                                       ASFRReductionLowFlows     : Double;
                                                       AYearList                 : TStringList;
                                                       AAreaList                 : TStringList): Boolean;
const OPNAME = 'TRunOffDBManager.UpdateAfforestationDataInDB';
var
  LIndex, LYear : Integer;
  LArea : Double;
begin
  Result := FALSE;
  try
    GHydroDBAgent.StartTransaction;
    try
      Result := UpdateAfforestationParamsInDB(AModuleID, AAfforestationAlgorithm, APineAreaPercentage,
                                              APineRotationPeriod, AEucalyptusAreaPercentage, AEucalyptusRotationPeriod,
                                              AWattleAreaPercentage, AWattleRotationPeriod, AOptimalAreaPercentage,
                                              ASFRReductionMAR, ASFRReductionLowFlows) AND
                DeleteAfforestationDataFromDB(AModuleID);
      LIndex := 0;
      while (Result AND (LIndex < AYearList.Count)) do
      begin
        LYear  := StrToInt(AYearList.Strings[LIndex]);
        LArea  := StrToFloat(AAreaList.Strings[LIndex]);
        Result := InsertAfforestationAreaDataInDB(AModuleID, LYear, LArea);
        LIndex := LIndex + 1;
      end;
      if (Result) then
        GHydroDBAgent.CommitTransaction
      else
        GHydroDBAgent.RollbackTransaction;
    except
      GHydroDBAgent.RollbackTransaction;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRunOffDBManager.UpdateAfforestationParamsInDB (AModuleID                 : Integer;
                                                         AAfforestationAlgorithm   : Integer;
                                                         APineAreaPercentage       : Double;
                                                         APineRotationPeriod       : Integer;
                                                         AEucalyptusAreaPercentage : Double;
                                                         AEucalyptusRotationPeriod : Integer;
                                                         AWattleAreaPercentage     : Double;
                                                         AWattleRotationPeriod     : Integer;
                                                         AOptimalAreaPercentage    : Double;
                                                         ASFRReductionMAR          : Double;
                                                         ASFRReductionLowFlows     : Double): Boolean;
const OPNAME = 'TRunOffDBManager.UpdateAfforestationParamsInDB';
var
  LSQL : String;
begin
  Result := FALSE;
  try
    LSQL := 'UPDATE RunOffAfforestationParams SET' +
            ' AfforestationAlgorithm = '    + IntToStr(AAfforestationAlgorithm) +
            ', PineAreaPercentage = '       + FloatToStr(APineAreaPercentage) +
            ', PineRotationPeriod = '       + IntToStr(APineRotationPeriod) +
            ', EucalyptusAreaPercentage = ' + FloatToStr(AEucalyptusAreaPercentage) +
            ', EucalyptusRotationPeriod = ' + FloatToStr(AEucalyptusRotationPeriod) +
            ', WattleAreaPercentage = '     + FloatToStr(AWattleAreaPercentage) +
            ', WattleRotationPeriod = '     + FloatToStr(AWattleRotationPeriod) +
            ', OptimalAreaPercentage = '    + FloatToStr(AOptimalAreaPercentage) +
            ', SFRReductionMAR = '          + FloatToStr(ASFRReductionMAR) +
            ', SFRReductionLowFlows = '     + FloatToStr(ASFRReductionLowFlows) +
            ' WHERE ModuleID = ' + IntToStr(AModuleID);
    Result := GHydroDBAgent.ExecuteSQL(LSQL, True);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRunOffDBManager.UpdateAlienVegetationDataInDB (AModuleID                  : Integer;
                                                         AAlienVegetationAlgorithm  : Integer;
                                                         ARiparianVegetationArea    : Double;
                                                         ATallTreeAreaPercentage    : Double;
                                                         ATallTreeAge               : Double;
                                                         AMediumTreeAreaPercentage  : Double;
                                                         AMediumTreeAge             : Double;
                                                         ATallSchrubAreaPercentage  : Double;
                                                         ATallSchrubAge             : Double;
                                                         AOptimalAreaPercentage     : Double;
                                                         AYearList                  : TStringList;
                                                         AAreaList                  : TStringList): Boolean;
const OPNAME = 'TRunOffDBManager.UpdateAlienVegetationDataInDB';
var
  LIndex, LYear : Integer;
  LArea : Double;
begin
  Result := FALSE;
  try
    GHydroDBAgent.StartTransaction;
    try
      Result := UpdateAlienVegetationParamsInDB(AModuleID, AAlienVegetationAlgorithm, ARiparianVegetationArea,
                                                ATallTreeAreaPercentage, ATallTreeAge, AMediumTreeAreaPercentage,
                                                AMediumTreeAge, ATallSchrubAreaPercentage, ATallSchrubAge,
                                                AOptimalAreaPercentage) AND
                DeleteAlienVegetationDataFromDB(AModuleID);
      LIndex := 0;
      while (Result AND (LIndex < AYearList.Count)) do
      begin
        LYear  := StrToInt(AYearList.Strings[LIndex]);
        LArea  := StrToFloat(AAreaList.Strings[LIndex]);
        Result := InsertAlienVegetationAreaDataInDB(AModuleID, LYear, LArea);
        LIndex := LIndex + 1;
      end;
      if (Result) then
        GHydroDBAgent.CommitTransaction
      else
        GHydroDBAgent.RollbackTransaction;
    except
      GHydroDBAgent.RollbackTransaction;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRunOffDBManager.UpdateAlienVegetationParamsInDB (AModuleID                  : Integer;
                                                           AAlienVegetationAlgorithm  : Integer;
                                                           ARiparianVegetationArea    : Double;
                                                           ATallTreeAreaPercentage    : Double;
                                                           ATallTreeAge               : Double;
                                                           AMediumTreeAreaPercentage  : Double;
                                                           AMediumTreeAge             : Double;
                                                           ATallSchrubAreaPercentage  : Double;
                                                           ATallSchrubAge             : Double;
                                                           AOptimalAreaPercentage     : Double): Boolean;
const OPNAME = 'TRunOffDBManager.UpdateAlienVegetationParamsInDB';
var
  LSQL                       : String;
begin
  Result := TRUE;
  try
    LSQL := 'UPDATE RunOffAlienVegetationParams SET' +
            ' VegetationAlgoritm = '        + IntToStr(AAlienVegetationAlgorithm) +
            ', RiparianVegetationArea = '   + FloatToStr(ARiparianVegetationArea) +
            ', TallTreeAreaPercentage = '   + FloatToStr(ATallTreeAreaPercentage) +
            ', TallTreeAge = '              + FloatToStr(ATallTreeAge) +
            ', MediumTreeAreaPercentage = ' + FloatToStr(AMediumTreeAreaPercentage) +
            ', MediumTreeAge = '            + FloatToStr(AMediumTreeAge) +
            ', TallSchrubAreaPercentage = ' + FloatToStr(ATallSchrubAreaPercentage) +
            ', TallSchrubAge = '            + FloatToStr(ATallSchrubAge) +
            ', OptimalAreaPercentage = '    + FloatToStr(AOptimalAreaPercentage) +
            ' WHERE ModuleID = ' + IntToStr(AModuleID);
    Result := GHydroDBAgent.ExecuteSQL(LSQL, True);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRunOffDBManager.UpdatePavedAreaDataInDB (AModuleID       : Integer;
                                                   AYearList       : TStringList;
                                                   AProportionList : TStringList) : Boolean;
const OPNAME = 'TRunOffDBManager.UpdatePavedAreaDataInDB';
var
  LIndex, LYear : Integer;
  LProportion : Double;
begin
  Result := FALSE;
  try
    GHydroDBAgent.StartTransaction;
    try
      Result := DeletePavedAreaFromDB(AModuleID);
      LIndex := 0;
      while (Result AND (LIndex < AYearList.Count)) do
      begin
        LYear       := StrToInt(AYearList.Strings[LIndex]);
        LProportion := StrToFloat(AProportionList.Strings[LIndex]);
        Result      := InsertPavedAreaDataInDB(AModuleID, LYear, LProportion);
        LIndex      := LIndex + 1;
      end;
      if (Result) then
        GHydroDBAgent.CommitTransaction
      else
        GHydroDBAgent.RollbackTransaction;
    except
      GHydroDBAgent.RollbackTransaction;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRunOffDBManager.UpdateGroundwaterAbstractionDataInDB (AModuleID        : Integer;
                                                                AYearList        : TStringList;
                                                                AAbstractionList : TStringList) : Boolean;
const OPNAME = 'TRunOffDBManager.UpdateGroundwaterAbstractionDataInDB';
var
  LIndex, LYear : Integer;
  LAbstraction : Double;
begin
  Result := FALSE;
  try
    GHydroDBAgent.StartTransaction;
    try
      Result := DeleteGroundWaterAbstractionFromDB(AModuleID);
      LIndex := 0;
      while (Result AND (LIndex < AYearList.Count)) do
      begin
        LYear        := StrToInt(AYearList.Strings[LIndex]);
        LAbstraction := StrToFloat(AAbstractionList.Strings[LIndex]);
        Result       := InsertGroundwaterAbstractionDataInDB(AModuleID, LYear, LAbstraction);
        LIndex       := LIndex + 1;
      end;
      if (Result) then
        GHydroDBAgent.CommitTransaction
      else
        GHydroDBAgent.RollbackTransaction;
    except
      GHydroDBAgent.RollbackTransaction;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRunOffDBManager.UpdateOutflowRoutesDataInDB (AModuleID       : Integer;
                                                       ARouteNoList    : TStringList;
                                                       APercentageList : TStringList) : Boolean;
const OPNAME = 'TRunOffDBManager.UpdateOutflowRoutesDataInDB';
var
  LIndex, LRouteNo : Integer;
  LPercentage : Double;
begin
  Result := FALSE;
  try
    GHydroDBAgent.StartTransaction;
    try
      Result := TRUE;
      LIndex := 0;
      while (Result AND (LIndex < ARouteNoList.Count)) do
      begin
        LRouteNo    := StrToInt(ARouteNoList.Strings[LIndex]);
        LPercentage := StrToFloat(APercentageList.Strings[LIndex]);
        Result      := UpdateRunOffOutflowRouteInDB(AModuleID, LRouteNo, LPercentage);
        LIndex      := LIndex + 1;
      end;
      if (Result) then
        GHydroDBAgent.CommitTransaction
      else
        GHydroDBAgent.RollbackTransaction;
    except
      GHydroDBAgent.RollbackTransaction;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRunOffDBManager.UpdateHughesDataInDB (AModuleID                      : Integer;
                                                AInflowRouteNo                 : Integer;
                                                AInfluenceROMNo                : Integer;
                                                AGroundWaterModel              : Integer;
                                                AHughesHGSL                    : Double;
                                                AHughesGPOW                    : Double;
                                                AHughesTLGMax                  : Double;
                                                AHughesHGGW                    : Double;
                                                AHughesPOW                     : Double;
                                                AHughesSL                      : Integer;
                                                AHughesST                      : Integer;
                                                AHughesFT                      : Double;
                                                AHughesGW                      : Double;
                                                AHughesZMIN                    : Integer;
                                                AHughesZMAX                    : Integer;
                                                AHughesPI                      : Double;
                                                AHughesTL                      : Double;
                                                AHughesGL                      : Double;
                                                AHughesR                       : Double;
                                                AHughesFF                      : Double;
                                                AUseNoOfReaches                : Integer;
                                                ADrainageDensity               : Double;
                                                ANumberOfReaches               : Integer;
                                                ARiparianAreaWidthPercentage   : Double;
                                                ARiparianStripFactor           : Double;
                                                ARestWaterlevel                : Double;
                                                ATransmissivity                : Double;
                                                AStorativity                   : Double;
                                                AGroundwaterSlope              : Double;
                                                AAnnualUpperZoneAbstraction    : Double;
                                                AAnnualRiparianZoneAbstraction : Double;
                                                AUpperZoneDemand               : TMonthlyDoubleArray;
                                                ARiparianZoneDemand            : TMonthlyDoubleArray): Boolean;
const OPNAME = 'TRunOffDBManager.UpdateHughesDataInDB';
var
  //LIndex     : Integer;
  LMonth     : Integer;
  LFieldName : String;
  LSQL       : String;
begin
  Result := FALSE;
  try
    LSQL := 'UPDATE RunOffHughesGWParams SET' +
            ' InflowRouteNo = '                  + FloatToStr(AInflowRouteNo) +
            ', InfluenceROMNo = '                + FloatToStr(AInfluenceROMNo) +
            ', GroundWaterModel = '              + FloatToStr(AGroundWaterModel) +
            ', HughesHGSL = '                    + FloatToStr(AHughesHGSL) +
            ', HughesGPOW = '                    + FloatToStr(AHughesGPOW) +
            ', HughesTLGMax = '                  + FloatToStr(AHughesTLGMax) +
            ', HughesHGGW = '                    + FloatToStr(AHughesHGGW) +
            ', HughesPOW = '                     + FloatToStr(AHughesPOW) +
            ', HughesSL = '                      + IntToStr(AHughesSL) +
            ', HughesST = '                      + IntToStr(AHughesST) +
            ', HughesFT = '                      + FloatToStr(AHughesFT) +
            ', HughesGW = '                      + FloatToStr(AHughesGW) +
            ', HughesZMIN = '                    + IntToStr(AHughesZMIN) +
            ', HughesZMAX = '                    + IntToStr(AHughesZMAX) +
            ', HughesPI = '                      + FloatToStr(AHughesPI) +
            ', HughesTL = '                      + FloatToStr(AHughesTL) +
            ', HughesGL = '                      + FloatToStr(AHughesGL) +
            ', HughesR = '                       + FloatToStr(AHughesR) +
            ', FF = '                            + FloatToStr(AHughesFF) +
            ', UseNoOfReaches = '                + IntToStr(AUseNoOfReaches) +
            ', DrainageDensity = '               + FloatToStr(ADrainageDensity) +
            ', NumberOfReaches = '               + FloatToStr(ANumberOfReaches) +
            ', RiparianAreaWidthPercentage = '   + FloatToStr(ARiparianAreaWidthPercentage) +
            ', RiparianStripFactor = '           + FloatToStr(ARiparianStripFactor) +
            ', RestWaterlevel = '                + FloatToStr(ARestWaterlevel) +
            ', Transmissivity = '                + FloatToStr(ATransmissivity) +
            ', Storativity = '                   + FloatToStr(AStorativity) +
            ', GroundwaterSlope = '              + FloatToStr(AGroundwaterSlope) +
            ', AnnualUpperZoneAbstraction = '    + FloatToStr(AAnnualUpperZoneAbstraction) +
            ', AnnualRaparianZoneAbstraction = ' + FloatToStr(AAnnualRiparianZoneAbstraction);

    for LMonth := 1 to 12 do
    begin
      LFieldName := Format('UpperZoneAbstraction%2.2d', [LMonth]);
      LSQL := LSQL + ', ' + LFieldName + ' = ' + FloatToStr(AUpperZoneDemand[LMonth]);
    end;
    for LMonth := 1 to 12 do
    begin
      LFieldName := Format('RiparianZoneAbstraction%2.2d', [LMonth]);
      LSQL := LSQL + ', ' + LFieldName + ' = ' + FloatToStr(ARiparianZoneDemand[LMonth]);
    end;

    LSQL := LSQL + ' WHERE ModuleID = ' + IntToStr(AModuleID);

    Result := GHydroDBAgent.ExecuteSQL(LSQL, True);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRunOffDBManager.UpdateSamiDataInDB (AModuleID                      : Integer;
                                              AAquiferThickness              : Double;
                                              AStorativity                   : Double;
                                              AInitialAquiferStorage         : Double;
                                              AStaticWaterLevel              : Double;
                                              AUnsaturatedStorage            : Double;
                                              AInitialUnsaturatedZoneStorage : Double;
                                              APerculationPower              : Double;
                                              AMaxDischarge                  : Double;
                                              AInteractionCurvePower         : Double;
                                              AMaxHydrologicalGradient       : Double;
                                              ATransmissivity                : Double;
                                              ABoreholeDistanceToRiver       : Double;
                                              AGroundWaterEvaporationArea    : Double;
                                              AInterflowLag                  : Double;
                                              ARechargeAveragedNoMonths      : Double;
                                              AUseAbstractions               : Integer;
                                              ASamiGPOW                      : Double;
                                              ASamiHGSL                      : Double;
                                              ASamiHGGW                      : Double;
                                              ASamiK2                        : Double;
                                              ASamiK3                        : Double;
                                              ASamiPOW                       : Double;
                                              ASamiSL                        : Integer;
                                              ASamiST                        : Integer;
                                              ASamiFT                        : Double;
                                              ASamiGW                        : Double;
                                              ASamiZMIN                      : Integer;
                                              ASamiZMAX                      : Integer;
                                              ASamiPI                        : Double;
                                              ASamiTL                        : Double;
                                              ASamiGL                        : Double;
                                              ASamiR                         : Double;
                                              ASamiFF                        : Double): Boolean;
const OPNAME = 'TRunOffDBManager.UpdateSamiDataInDB';
var
  LSQL : String;
begin
  Result := FALSE;
  try
    LSQL := 'UPDATE RunOffSamiGWParams SET'      +
            ' AquiferThickness = '               + FloatToStr(AAquiferThickness) +
            ', Storativity = '                   + FloatToStr(AStorativity) +
            ', InitialAquiferStorage = '         + FloatToStr(AInitialAquiferStorage) +
            ', StaticWaterLevel = '              + FloatToStr(AStaticWaterLevel) +
            ', UnsaturatedStorage = '            + FloatToStr(AUnsaturatedStorage) +
            ', InitialUnsaturatedZoneStorage = ' + FloatToStr(AInitialUnsaturatedZoneStorage) +
            ', PerculationPower = '              + FloatToStr(APerculationPower) +
            ', MaxDischarge = '                  + FloatToStr(AMaxDischarge) +
            ', InteractionCurvePower = '         + FloatToStr(AInteractionCurvePower) +
            ', MaxHydrologicalGradient = '       + FloatToStr(AMaxHydrologicalGradient) +
            ', Transmissivity = '                + FloatToStr(ATransmissivity) +
            ', BoreholeDistanceToRiver = '       + FloatToStr(ABoreholeDistanceToRiver) +
            ', GroundWaterEvaporationArea = '    + FloatToStr(AGroundWaterEvaporationArea) +
            ', InterflowLag = '                  + FloatToStr(AInterflowLag) +
            ', RechargeAveragedNoMonths = '      + FloatToStr(ARechargeAveragedNoMonths) +
            ', UseAbstractions = '               + FloatToStr(AUseAbstractions) +
            ', GPOW = '                          + FloatToStr(ASamiGPOW) +
            ', HGSL = '                          + FloatToStr(ASamiHGSL) +
            ', HGGW = '                          + FloatToStr(ASamiHGGW) +
            ', K2 = '                            + FloatToStr(ASamiK2) +
            ', K3 = '                            + FloatToStr(ASamiK3) +
            ', SamiPOW = '                       + FloatToStr(ASamiPOW) +
            ', SamiSL = '                        + IntToStr(ASamiSL) +
            ', SamiST = '                        + IntToStr(ASamiST) +
            ', SamiFT = '                        + FloatToStr(ASamiFT) +
            ', SamiGW = '                        + FloatToStr(ASamiGW) +
            ', SamiZMIN = '                      + IntToStr(ASamiZMIN) +
            ', SamiZMAX = '                      + IntToStr(ASamiZMAX) +
            ', SamiPI = '                        + FloatToStr(ASamiPI) +
            ', SamiTL = '                        + FloatToStr(ASamiTL) +
            ', SamiGL = '                        + FloatToStr(ASamiGL) +
            ', SamiR = '                         + FloatToStr(ASamiR) +
            ', FF = '                            + FloatToStr(ASamiFF) +
            ' WHERE ModuleID = ' + IntToStr(AModuleID);

    Result := GHydroDBAgent.ExecuteSQL(LSQL, True);
  except on E: Exception do HandleError(E, OPNAME) end;
end;


function TRunOffDBManager.DeleteRunOffModuleFromDB (AModuleID : Integer) : Boolean;
const OPNAME = 'TRunOffDBManager.DeleteRunOffModuleFromDB';
var
  LSQL : String;
begin
  Result := FALSE;
  try
    LSQL := 'DELETE FROM RunOffModules WHERE ModuleID = ' + IntToStr(AModuleID);
    Result := GHydroDBAgent.ExecuteSQL(LSQL, True);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TRunOffDBManager.DeleteHughesModelFromDB (AModuleID : Integer) : Boolean;
const OPNAME = 'TRunOffDBManager.DeleteHughesModelFromDB';
var
  LSQL : String;
begin
  Result := FALSE;
  try
    LSQL := 'DELETE FROM RunOffHughesGWParams WHERE ModuleID = ' + IntToStr(AModuleID);
    Result := GHydroDBAgent.ExecuteSQL(LSQL, True);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TRunOffDBManager.DeleteSamiModelFromDB (AModuleID : Integer) : Boolean;
const OPNAME = 'TRunOffDBManager.DeleteSamiModelFromDB';
var
  LSQL : String;
begin
  Result := FALSE;
  try
    LSQL := 'DELETE FROM RunOffSamiGWParams WHERE ModuleID = ' + IntToStr(AModuleID);
    Result := GHydroDBAgent.ExecuteSQL(LSQL, True);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TRunOffDBManager.DeleteRunOffSlavesFromDB (AModuleID : Integer) : Boolean;
const OPNAME = 'TRunOffDBManager.DeleteRunOffSlavesFromDB';
var
  LSQL : String;
begin
  Result := FALSE;
  try
    LSQL := 'DELETE * FROM RunOffModuleSlaves WHERE ModuleID = ' + IntToStr(AModuleID);
    Result := GHydroDBAgent.ExecuteSQL(LSQL, True);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TRunOffDBManager.DeleteAfforestationParamsFromDB (AModuleID : Integer) : Boolean;
const OPNAME = 'TRunOffDBManager.DeleteAfforestationFromDB';
var
  LSQL : String;
begin
  Result := FALSE;
  try
    LSQL := 'DELETE * FROM RunOffAfforestationParams WHERE ModuleID = ' + IntToStr(AModuleID);
    Result := GHydroDBAgent.ExecuteSQL(LSQL, True);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TRunOffDBManager.DeleteAfforestationDataFromDB (AModuleID : Integer) : Boolean;
const OPNAME = 'TRunOffDBManager.DeleteAfforestationDataFromDB';
var
  LSQL : String;
begin
  Result := FALSE;
  try
    LSQL := 'DELETE * FROM RunOffAfforestationAreaData WHERE ModuleID = ' + IntToStr(AModuleID);
    Result := GHydroDBAgent.ExecuteSQL(LSQL, True);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TRunOffDBManager.DeleteAlienVegetationParamsFromDB (AModuleID : Integer) : Boolean;
const OPNAME = 'TRunOffDBManager.DeleteAlienVegetationParamsFromDB';
var
  LSQL : String;
begin
  Result := FALSE;
  try
    LSQL := 'DELETE * FROM RunOffAlienVegetationParams WHERE ModuleID = ' + IntToStr(AModuleID);
    Result := GHydroDBAgent.ExecuteSQL(LSQL, True);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TRunOffDBManager.DeleteAlienVegetationDataFromDB (AModuleID : Integer) : Boolean;
const OPNAME = 'TRunOffDBManager.DeleteAlienVegetationDataFromDB';
var
  LSQL : String;
begin
  Result := FALSE;
  try
    LSQL := 'DELETE * FROM RunOffAlienVegetationAreaData WHERE ModuleID = ' + IntToStr(AModuleID);
    Result := GHydroDBAgent.ExecuteSQL(LSQL, True);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TRunOffDBManager.DeletePavedAreaFromDB (AModuleID : Integer) : Boolean;
const OPNAME = 'TRunOffDBManager.DeletePavedAreaFromDB';
var
  LSQL : String;
begin
  Result := FALSE;
  try
    LSQL := 'DELETE * FROM RunOffPavedAreaData WHERE ModuleID = ' + IntToStr(AModuleID);
    Result := GHydroDBAgent.ExecuteSQL(LSQL, True);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TRunOffDBManager.DeleteGroundWaterAbstractionFromDB (AModuleID : Integer) : Boolean;
const OPNAME = 'TRunOffDBManager.DeleteGroundWaterAbstractionFromDB';
var
  LSQL : String;
begin
  Result := FALSE;
  try
    LSQL := 'DELETE * FROM RunOffGroundwaterAbstractionData WHERE ModuleID = ' + IntToStr(AModuleID);
    Result := GHydroDBAgent.ExecuteSQL(LSQL, True);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TRunOffDBManager.DeleteRunOffModuleOutflowRoutesFromDB (AModuleID : Integer) : Boolean;
const OPNAME = 'TRunOffDBManager.DeleteRunOffModuleOutflowRoutesFromDB';
var
  LSQL : String;
begin
  Result := FALSE;
  try
    LSQL := 'DELETE * FROM RunOffOutflowRoutes WHERE ModuleID = ' + IntToStr(AModuleID);
    Result := GHydroDBAgent.ExecuteSQL(LSQL, True);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TRunOffDBManager.InsertAfforestationAreaDataInDB (AModuleID, AYear : Integer; AArea : Double) : Boolean;
const OPNAME = 'TRunOffDBManager.InsertAfforestationAreaDataInDB';
var
  LSQL : String;
begin
  Result := FALSE;
  try
    LSQL := 'INSERT INTO RunOffAfforestationAreaData VALUES (' +
            IntToStr(AModuleID) + ', ' + IntToStr(AYear) + ', ' + FloatToStr(AArea) + ')';
    Result := GHydroDBAgent.ExecuteSQL(LSQL, True);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TRunOffDBManager.InsertAlienVegetationAreaDataInDB (AModuleID, AYear : Integer; AArea : Double) : Boolean;
const OPNAME = 'TRunOffDBManager.InsertAlienVegetationAreaDataInDB';
var
  LSQL : String;
begin
  Result := FALSE;
  try
    LSQL := 'INSERT INTO RunOffAlienVegetationAreaData VALUES (' +
            IntToStr(AModuleID) + ', ' + IntToStr(AYear) + ', ' + FloatToStr(AArea) + ')';
    Result := GHydroDBAgent.ExecuteSQL(LSQL, True);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TRunOffDBManager.InsertPavedAreaDataInDB (AModuleID, AYear : Integer; AProportion : Double) : Boolean;
const OPNAME = 'TRunOffDBManager.InsertPavedAreaDataInDB';
var
  LSQL : String;
begin
  Result := FALSE;
  try
    LSQL := 'INSERT INTO RunOffPavedAreaData VALUES (' +
            IntToStr(AModuleID) + ', ' + IntToStr(AYear) + ', ' + FloatToStr(AProportion) + ')';
    Result := GHydroDBAgent.ExecuteSQL(LSQL, True);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TRunOffDBManager.InsertGroundwaterAbstractionDataInDB (AModuleID, AYear : Integer; AAbstraction : Double) : Boolean;
const OPNAME = 'TRunOffDBManager.InsertGroundwaterAbstractionDataInDB';
var
  LSQL : String;
begin
  Result := FALSE;
  try
    LSQL := 'INSERT INTO RunOffGroundWaterAbstractionData VALUES (' +
            IntToStr(AModuleID) + ', ' + IntToStr(AYear) + ', ' + FloatToStr(AAbstraction) + ')';
    Result := GHydroDBAgent.ExecuteSQL(LSQL, True);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TRunOffDBManager.UpdateRunOffOutflowRouteInDB (AModuleID, ARouteNo : Integer; AOutflowPercentage : Double) : Boolean;
const OPNAME = 'TRunOffDBManager.UpdateRunOffOutflowRouteInDB';
var
  LSQL : String;
begin
  Result := FALSE;
  try
    LSQL := 'UPDATE RunOffOutflowRoutes SET OutflowPercentage = ' + FloatToStr(AOutflowPercentage) +
            ' WHERE ModuleID = ' + IntToStr(AModuleID) +
            ' AND RouteNo = ' + IntToStr(ARouteNo);
    Result := GHydroDBAgent.ExecuteSQL(LSQL, True);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TRunOffDBManager.InsertRunOffIntoDB (AModuleID : Integer; AName     : String) : Boolean;
const OPNAME = 'TRunOffDBManager.InsertRunOffIntoDB';
var
  LSQL : String;
begin
  Result := FALSE;
  try
    LSQL := 'INSERT INTO RunOffModules (ModuleID, RunOffName) VALUES (' +
            IntToStr(AModuleID) + ', ' + QuotedStr(AName) +  ')';
    Result := GHydroDBAgent.ExecuteSQL(LSQL, True);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRunOffDBManager.InsertAfforestationParamsIntoDB (AModuleID : Integer): Boolean;
const OPNAME = 'TRunOffDBManager.InsertAfforestationParamsIntoDB';
var
  LSQL : String;
begin
  Result := FALSE;
  try
    LSQL := 'INSERT INTO RunOffAfforestationParams (ModuleID) VALUES (' +
            IntToStr(AModuleID) + ')';
    Result := GHydroDBAgent.ExecuteSQL(LSQL, True);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRunOffDBManager.InsertAlienVegetationParamsIntoDB (AModuleID : Integer): Boolean;
const OPNAME = 'TRunOffDBManager.InsertAlienVegetationParamsIntoDB';
var
  LSQL : String;
begin
  Result := FALSE;
  try
    LSQL := 'INSERT INTO RunOffAlienVegetationParams (ModuleID) VALUES (' +
            IntToStr(AModuleID) + ')';
    Result := GHydroDBAgent.ExecuteSQL(LSQL, True);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRunOffDBManager.InsertHughesModelIntoDB (AModuleID : Integer): Boolean;
const OPNAME = 'TRunOffDBManager.InsertHughesModelIntoDB';
var
  LSQL : String;
begin
  Result := FALSE;
  try
    LSQL := 'INSERT INTO RunOffHughesGWParams (ModuleID) VALUES (' +
            IntToStr(AModuleID) + ')';
    Result := GHydroDBAgent.ExecuteSQL(LSQL, True);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRunOffDBManager.InsertSamiModelIntoDB (AModuleID : Integer): Boolean;
const OPNAME = 'TRunOffDBManager.InsertSamiModelIntoDB';
var
  LSQL : String;
begin
  Result := FALSE;
  try
    LSQL := 'INSERT INTO RunOffSamiGWParams (ModuleID) VALUES (' +
            IntToStr(AModuleID) + ')';
    Result := GHydroDBAgent.ExecuteSQL(LSQL, True);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRunOffDBManager.CreateNewRunOffModuleInDB (ANetworkID: Integer) : TRunOffModule;
const OPNAME = 'TRunOffDBManager.CreateNewRunOffModuleInDB';
var
  LRunOffModule    : TRunOffModule;
  LModuleID        : Integer;
  LNetworkSequence : Integer;
  LModuleNumber    : Integer;
  LName            : String;
begin
  Result := nil;
  try
    LModuleID        := GHydroDBAgent.GetNextID('Modules', 'ModuleID');
    LNetworkSequence := GetNextNetworkSequence(ANetworkID);
    LModuleNumber    := GetNextModuleNumber(ANetworkID, 'RU');
    LName            := 'RU' + IntToStr(LModuleNumber);

    GHydroDBAgent.StartTransaction;
    try
      if (InsertModuleIntoDB(LModuleID, 'RU', 0, 0)) AND
         (InsertPanIntoDB(LModuleID)) AND
         (InsertNetworkModuleIntoDB(ANetworkID, LModuleID, LModuleNumber, LNetworkSequence, 'Y')) AND
         (InsertRunOffIntoDB(LModuleID, LName)) AND
         (InsertAfforestationParamsIntoDB(LModuleID)) AND
         (InsertAlienVegetationParamsIntoDB(LModuleID)) AND
         (InsertHughesModelIntoDB(LModuleID)) AND
         (InsertSamiModelIntoDB(LModuleID)) then
      begin
        GHydroDBAgent.CommitTransaction;
        LRunOffModule := TRunOffModuleAgent(ModuleAgent).AddRunOffModule;
        LRunOffModule.Populate(ANetworkID, LModuleID, 'RU', LModuleNumber, LNetworkSequence, 'Y', LName,
                               0, 0, 0, '', 0, 0, 0, '0,0,0,0,0,0,0,0,0,0,0,0');
        GRunOffDBManager.LoadPanDataFromDB(LRunOffModule);
        Result := LRunOffModule;
      end
      else
        GHydroDBAgent.RollbackTransaction;
    except
      GHydroDBAgent.RollbackTransaction;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TRunOffDBManager.RemoveRunOffModuleFromDB (AModuleID : Integer) : Boolean;
const OPNAME = 'TRunOffDBManager.RemoveRunOffModuleFromDB';
begin
  Result := FALSE;
  try
    GHydroDBAgent.StartTransaction;
    try
      if (DeleteRunOffModuleOutflowRoutesFromDB(AModuleID)) AND
         (DeleteAfforestationDataFromDB(AModuleID)) AND
         (DeleteAfforestationParamsFromDB(AModuleID)) AND
         (DeleteAlienVegetationParamsFromDB(AModuleID)) AND
         (DeleteAlienVegetationDataFromDB(AModuleID)) AND
         (DeleteGroundWaterAbstractionFromDB(AModuleID)) AND
         (DeletePavedAreaFromDB(AModuleID)) AND
         (DeletePanFromDB(AModuleID)) AND
         (DeleteRunOffSlavesFromDB(AModuleID)) AND
         (DeleteHughesModelFromDB(AModuleID)) AND
         (DeleteSamiModelFromDB(AModuleID)) AND
         (DeleteRunOffModuleFromDB(AModuleID)) AND
         (DeleteNetworkModuleFromDB(AModuleID)) AND
         (DeleteModuleFromDB(AModuleID)) then
      begin
        Result := TRUE;
        GHydroDBAgent.CommitTransaction;
      end
      else
        GHydroDBAgent.RollbackTransaction;
    except
      GHydroDBAgent.RollbackTransaction;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TRunOffDBManager.LoadRunOffSlavesFromDB (ARunOff : TRunOffModule) : Boolean;
const OPNAME = 'TRunOffDBManager.LoadRunOffSlavesFromDB';
var
  LQuery    : TDataSet;
  LSQL      : String;
begin
  Result := FALSE;
  try
    ARunOff.ClearSlaveModuleNumbers;
    LSQL    := 'SELECT * FROM RunOffModuleSlaves WHERE ModuleID = ' + IntToStr(ARunOff.ModuleID);
    LQuery  := GHydroDBAgent.CreateQuery(LSQL);
    try
      LQuery.Open;
      while (NOT LQuery.Eof) do
      begin
        ARunOff.AddSlaveModuleNo(LQuery.FieldByName('SlaveModuleNo').AsInteger);
        LQuery.Next;
      end;
      LQuery.Close;
    finally
      LQuery.Free;
    end;
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TRunOffDBManager.LoadAfforestationFromDB (ARunOff : TRunOffModule) : Boolean;
const OPNAME = 'TRunOffDBManager.LoadAfforestationFromDB';
var
  LQuery        : TDataSet;
  LParamQuery       : TDataSet;
  LSQL          : String;
  LParamSQL         : String;
begin
  Result := FALSE;
  try
    LParamSQL := 'SELECT * FROM RunOffAfforestationParams WHERE ModuleID = ' + IntToStr(ARunOff.ModuleID);
    LParamQuery := GHydroDBAgent.CreateQuery(LParamSQL);
    try
      LParamQuery.Open;
      if (NOT LParamQuery.IsEmpty) then
      begin
        ARunOff.Afforestation.Populate(LParamQuery.FieldByName('AfforestationAlgorithm').AsInteger,
                                LParamQuery.FieldByName('PineAreaPercentage').AsFloat,
                                LParamQuery.FieldByName('PineRotationPeriod').AsInteger,
                                LParamQuery.FieldByName('EucalyptusAreaPercentage').AsFloat,
                                LParamQuery.FieldByName('EucalyptusRotationPeriod').AsInteger,
                                LParamQuery.FieldByName('WattleAreaPercentage').AsFloat,
                                LParamQuery.FieldByName('WattleRotationPeriod').AsInteger,
                                LParamQuery.FieldByName('OptimalAreaPercentage').AsFloat,
                                LParamQuery.FieldByName('SFRReductionMAR').AsFloat,
                                LParamQuery.FieldByName('SFRReductionLowFlows').AsFloat);
        LParamQuery.Close;
      end;
    finally
      LParamQuery.Free;
    end;

    ARunOff.ClearAfforestationAreaData;
    LSQL := 'SELECT * FROM RunOffAfforestationAreaData WHERE ModuleID = ' + IntToStr(ARunOff.ModuleID);
    LQuery  := GHydroDBAgent.CreateQuery(LSQL);
    try
      LQuery.Open;
      while (NOT LQuery.Eof) do
      begin
        ARunOff.Afforestation.AddAreaData(LQuery.FieldByName('Year').AsInteger,
                                          LQuery.FieldByName('Area').AsFloat);
        LQuery.Next;
      end;
      LQuery.Close;
    finally
      LQuery.Free;
    end;
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TRunOffDBManager.LoadAlienVegetationFromDB (ARunOff : TRunOffModule) : Boolean;
const OPNAME = 'TRunOffDBManager.LoadAlienVegetationFromDB';
var
  LQuery        : TDataSet;
  LParamQuery       : TDataSet;
  LSQL          : String;
  LParamSQL         : String;
begin
  Result := FALSE;
  try
    LParamSQL   := 'SELECT * FROM RunOffAlienVegetationParams WHERE ModuleID = ' + IntToStr(ARunOff.ModuleID);
    LParamQuery := GHydroDBAgent.CreateQuery(LParamSQL);
    try
      LParamQuery.Open;
      if (NOT LParamQuery.IsEmpty) then
      begin
        ARunOff.AlienVegetation.Populate(LParamQuery.FieldByName('VegetationAlgoritm').AsInteger,
                                  LParamQuery.FieldByName('RiparianVegetationArea').AsFloat,
                                  LParamQuery.FieldByName('TallTreeAreaPercentage').AsFloat,
                                  LParamQuery.FieldByName('TallTreeAge').AsFloat,
                                  LParamQuery.FieldByName('MediumTreeAreaPercentage').AsFloat,
                                  LParamQuery.FieldByName('MediumTreeAge').AsFloat,
                                  LParamQuery.FieldByName('TallSchrubAreaPercentage').AsFloat,
                                  LParamQuery.FieldByName('TallSchrubAge').AsFloat,
                                  LParamQuery.FieldByName('OptimalAreaPercentage').AsFloat);
        LParamQuery.Close;
      end;
    finally
      LParamQuery.Free;
    end;

    ARunOff.ClearAlienVegetationAreaData;
    LSQL    := 'SELECT * FROM RunOffAlienVegetationAreaData WHERE ModuleID = ' + IntToStr(ARunOff.ModuleID);
    LQuery  := GHydroDBAgent.CreateQuery(LSQL);
    try
      LQuery.Open;

      while (NOT LQuery.Eof) do
      begin
        ARunOff.AlienVegetation.AddAreaData(LQuery.FieldByName('Year').AsInteger,
                                            LQuery.FieldByName('Area').AsFloat);
        LQuery.Next;
      end;
      LQuery.Close;
    finally
      LQuery.Free;
    end;
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TRunOffDBManager.LoadPavedAreaFromDB (ARunOff : TRunOffModule) : Boolean;
const OPNAME = 'TRunOffDBManager.LoadPavedAreaFromDB';
var
  LQuery    : TDataSet;
  LSQL      : String;
begin
  Result := FALSE;
  try
    ARunOff.ClearPavedAreaData;
    LSQL      := 'SELECT * FROM RunOffPavedAreaData WHERE ModuleID = ' + IntToStr(ARunOff.ModuleID);
    LQuery    := GHydroDBAgent.CreateQuery(LSQL);
    try
      LQuery.Open;
      while (NOT LQuery.Eof) do
      begin
        ARunOff.PavedArea.AddAreaData(LQuery.FieldByName('Year').AsInteger,
                                      LQuery.FieldByName('PavedAreaProportion').AsFloat);
        LQuery.Next;
      end;
      LQuery.Close;
    finally
      LQuery.Free;
    end;
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TRunOffDBManager.LoadGroundWaterAbstractionFromDB (ARunOff : TRunOffModule) : Boolean;
const OPNAME = 'TRunOffDBManager.LoadGroundWaterAbstractionFromDB';
var
  LQuery     : TDataSet;
  LSQL       : String;
begin
  Result := FALSE;
  try
    ARunOff.ClearGroundwaterAbstractionData;
    LSQL    := 'SELECT * FROM RunOffGroundwaterAbstractionData WHERE ModuleID = ' + IntToStr(ARunOff.ModuleID);
    LQuery  := GHydroDBAgent.CreateQuery(LSQL);
    try
      LQuery.Open;
      while (NOT LQuery.Eof) do
      begin
        ARunOff.GroundWaterAbstraction.AddAbstractionData(LQuery.FieldByName('Year').AsInteger,
                                                          LQuery.FieldByName('AbstractionValue').AsFloat);
        LQuery.Next;
      end;
      LQuery.Close;
    finally
      LQuery.Free;
    end;
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TRunOffDBManager.LoadHughesModelFromDB (ARunOff : TRunOffModule) : Boolean;
const OPNAME = 'TRunOffDBManager.LoadHughesModelFromDB';
var
  LQuery     : TDataSet;
  LSQL       : String;
  LTempList  : TStringList;
  LUpperZone : String;
  LRiparian  : String;
  LFieldName : String;
  LIndex     : Integer;
begin
  Result := FALSE;
  try
    LSQL    := 'SELECT * FROM RunOffHughesGWParams WHERE ModuleID = ' + IntToStr(ARunOff.ModuleID);
    LQuery  := GHydroDBAgent.CreateQuery(LSQL);
    try
      LQuery.Open;
      if (NOT LQuery.Eof) then
      begin
        LTempList := TStringList.Create;
        try
          for LIndex := 1 to 12 do
          begin
            LFieldName := Format('UpperZoneAbstraction%2.2d', [LIndex]);
            LTempList.Add(FloatToStr(LQuery.FieldByName(LFieldName).AsFloat));
          end;
          LUpperZone := LTempList.CommaText;
          LTempList.Clear;
          for LIndex := 1 to 12 do
          begin
            LFieldName := Format('RiparianZoneAbstraction%2.2d', [LIndex]);
            LTempList.Add(FloatToStr(LQuery.FieldByName(LFieldName).AsFloat));
          end;
          LRiparian := LTempList.CommaText;
        finally
          LTempList.Free;
        end;
        ARunOff.HughesModel.Populate(LQuery.FieldByName('InflowRouteNo').AsInteger,
                              LQuery.FieldByName('InfluenceROMNo').AsInteger,
                              LQuery.FieldByName('GroundWaterModel').AsInteger,
                              LQuery.FieldByName('HughesHGSL').AsFloat,
                              LQuery.FieldByName('HughesGPOW').AsFloat,
                              LQuery.FieldByName('HughesTLGMax').AsFloat,
                              LQuery.FieldByName('HughesHGGW').AsFloat,
                              LQuery.FieldByName('HughesPOW').AsFloat,
                              LQuery.FieldByName('HughesSL').AsInteger,
                              LQuery.FieldByName('HughesST').AsInteger,
                              LQuery.FieldByName('HughesFT').AsFloat,
                              LQuery.FieldByName('HughesGW').AsFloat,
                              LQuery.FieldByName('HughesZMIN').AsInteger,
                              LQuery.FieldByName('HughesZMAX').AsInteger,
                              LQuery.FieldByName('HughesPI').AsFloat,
                              LQuery.FieldByName('HughesTL').AsFloat,
                              LQuery.FieldByName('HughesGL').AsFloat,
                              LQuery.FieldByName('HughesR').AsFloat,
                              LQuery.FieldByName('FF').AsFloat,
                              LQuery.FieldByName('UseNoOfReaches').AsInteger,
                              LQuery.FieldByName('DrainageDensity').AsFloat,
                              LQuery.FieldByName('NumberOfReaches').AsInteger,
                              LQuery.FieldByName('RiparianAreaWidthPercentage').AsFloat,
                              LQuery.FieldByName('RiparianStripFactor').AsFloat,
                              LQuery.FieldByName('RestWaterlevel').AsFloat,
                              LQuery.FieldByName('Transmissivity').AsFloat,
                              LQuery.FieldByName('Storativity').AsFloat,
                              LQuery.FieldByName('GroundwaterSlope').AsFloat,
                              LQuery.FieldByName('AnnualUpperZoneAbstraction').AsFloat,
                              LUpperZone,
                              LQuery.FieldByName('AnnualRaparianZoneAbstraction').AsFloat,
                              LRiparian);
      end;
      LQuery.Close;
    finally
      LQuery.Free;
    end;
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TRunOffDBManager.LoadSamiModelFromDB (ARunOff : TRunOffModule) : Boolean;
const OPNAME = 'TRunOffDBManager.LoadSamiModelFromDB';
var
  LQuery    : TDataSet;
  LSQL      : String;
begin
  Result := FALSE;
  try
    LSQL    := 'SELECT * FROM RunOffSamiGWParams WHERE ModuleID = ' + IntToStr(ARunOff.ModuleID);
    LQuery  := GHydroDBAgent.CreateQuery(LSQL);
    try
      LQuery.Open;
      if (NOT LQuery.Eof) then
      begin
        ARunOff.SamiModel.Populate(LQuery.FieldByName('AquiferThickness').AsFloat,
                            LQuery.FieldByName('Storativity').AsFloat,
                            LQuery.FieldByName('InitialAquiferStorage').AsFloat,
                            LQuery.FieldByName('StaticWaterLevel').AsFloat,
                            LQuery.FieldByName('UnsaturatedStorage').AsFloat,
                            LQuery.FieldByName('InitialUnsaturatedZoneStorage').AsFloat,
                            LQuery.FieldByName('PerculationPower').AsFloat,
                            LQuery.FieldByName('GPOW').AsFloat,
                            LQuery.FieldByName('MaxDischarge').AsFloat,
                            LQuery.FieldByName('InteractionCurvePower').AsFloat,
                            LQuery.FieldByName('HGSL').AsFloat,
                            LQuery.FieldByName('HGGW').AsFloat,
                            LQuery.FieldByName('MaxHydrologicalGradient').AsFloat,
                            LQuery.FieldByName('Transmissivity').AsFloat,
                            LQuery.FieldByName('BoreholeDistanceToRiver').AsFloat,
                            LQuery.FieldByName('GroundWaterEvaporationArea').AsFloat,
                            LQuery.FieldByName('K2').AsFloat,
                            LQuery.FieldByName('K3').AsFloat,
                            LQuery.FieldByName('InterflowLag').AsFloat,
                            LQuery.FieldByName('RechargeAveragedNoMonths').AsFloat,
                            LQuery.FieldByName('UseAbstractions').AsInteger,
                            LQuery.FieldByName('SamiPOW').AsFloat,
                            LQuery.FieldByName('SamiSL').AsInteger,
                            LQuery.FieldByName('SamiST').AsInteger,
                            LQuery.FieldByName('SamiFT').AsFloat,
                            LQuery.FieldByName('SamiGW').AsFloat,
                            LQuery.FieldByName('SamiZMIN').AsInteger,
                            LQuery.FieldByName('SamiZMAX').AsInteger,
                            LQuery.FieldByName('SamiPI').AsFloat,
                            LQuery.FieldByName('SamiTL').AsFloat,
                            LQuery.FieldByName('SamiGL').AsFloat,
                            LQuery.FieldByName('SamiR').AsFloat,
                            LQuery.FieldByName('FF').AsFloat);
      end;
      LQuery.Close;
    finally
      LQuery.Free;
    end;
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TRunOffDBManager.LoadOutflowRoutesFromDB (ARunOff : TRunOffModule) : Boolean;
const OPNAME = 'TRunOffDBManager.LoadOutflowRoutesFromDB';
var
  LQuery    : TDataSet;
  LSQL      : String;
begin
  Result := FALSE;
  try
    ARunOff.ClearOutFlowRoutes;
    LSQL    := 'SELECT * FROM RunOffOutflowRoutes WHERE ModuleID = ' + IntToStr(ARunOff.ModuleID);
    LQuery  := GHydroDBAgent.CreateQuery(LSQL);
    try
      LQuery.Open;
      while (NOT LQuery.Eof) do
      begin
        ARunOff.AddOutflowRoute(LQuery.FieldByName('RouteNo').AsInteger,
                                LQuery.FieldByName('OutflowPercentage').AsFloat);
        LQuery.Next;
      end;
      LQuery.Close;
    finally
      LQuery.Free;
    end;
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TRunOffDBManager.UpdateSlavesDataInDB (AModuleID : Integer; ASlaveNoList : TStringList): Boolean;
const OPNAME = 'TRunOffDBManager.UpdateSlavesData';
var
  LIndex         : Integer;
  LSlaveModuleNo : String;
begin
  Result := FALSE;
  try
    GHydroDBAgent.StartTransaction;
    try
      Result := GRunOffDBManager.DeleteRunOffSlavesFromDB(AModuleID);
      LIndex := 0;
      while (Result AND (LIndex < ASlaveNoList.Count)) do
      begin
        LSlaveModuleNo := ASlaveNoList.Strings[LIndex];
        Result := InsertSlaveInDB(AModuleID, StrToInt(LSlaveModuleNo));
        LIndex := LIndex + 1;
      end;
      if (Result) then
        GHydroDBAgent.CommitTransaction
      else
        GHydroDBAgent.RollbackTransaction;  
    except
      GHydroDBAgent.RollbackTransaction;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


end.

