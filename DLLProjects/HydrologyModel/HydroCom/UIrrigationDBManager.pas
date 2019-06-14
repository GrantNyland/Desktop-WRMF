(******************************************************************************)
(* Contains : TIrrigationDBManager.                                           *)
(* Contains Database functionality for Irrigation.                            *)
(******************************************************************************)

unit UIrrigationDBManager;

interface

uses
  Classes, Contnrs,

  UAbstractObject,
  UModuleDBManager,
  UIrrigationModule;

type

  TIrrigationDBManager = class(TModuleDBManager)
  protected
  public
    function LoadIrrigationModulesFromDB (ANetworkID: Integer) : Boolean;
    function LoadIrrigationMonthlyDataFromDB (AIrrigationModule : TIrrigationModule) : Boolean;
    function LoadIrrigationAreaDataFromDB (AIrrigationModule : TIrrigationModule) : Boolean;
    function LoadIrrigationAllocationGrowthDataFromDB (AIrrigationModule : TIrrigationModule) : Boolean;
    function LoadIrrigationCropsFromDB (AIrrigationModule : TIrrigationModule) : Boolean;
    function LoadIrrigationEfficiencyFromDB (AIrrigationModule : TIrrigationModule) : Boolean;
    function LoadIrrigationReturnFlowFromDB (AIrrigationModule : TIrrigationModule) : Boolean;

    function DeleteAreaFromDB (AModuleID : Integer) : Boolean;
    function DeleteCropsFromDB (AModuleID : Integer) : Boolean;
    function DeleteEfficiencyFromDB (AModuleID : Integer) : Boolean;
    function DeleteMonthlyFactorsFromDB (AModuleID : Integer) : Boolean;
    function DeleteReturnFlowFromDB (AModuleID : Integer) : Boolean;
    function DeleteIrrigationModuleFromDB (AModuleID : Integer) : Boolean;
    function DeleteAllocationGrowthFromDB (AModuleID : Integer) : Boolean;

    function InsertIrrigationIntoDB (AModuleID : Integer; AName : String) : Boolean;
    function InsertMonthlyFactorsIntoDB (AModuleID : Integer) : Boolean;
    function InsertAreaDataIntoDB (AModuleID : Integer; AYear, AArea : String) : Boolean;
    function InsertAllocationGrowthDataIntoDB (AModuleID : Integer; AYear, AGrowth : String) : Boolean;
    function InsertReturnFlowDataIntoDB (AModuleID : Integer; AYear, AGrowth : String) : Boolean;
    function InsertEfficiencyDataIntoDB (AModuleID : Integer; AYear, AEfficiency : String) : Boolean;
    function InsertCropsDataIntoDB (AModuleID : Integer; ACropNo, APercentage, AName : String;
                                    ACropFactors : TMonthlyDoubleArray) : Boolean;

    function UpdatePropertiesDataInDB (AModuleID               : Integer;
                                       AActive                 : String;
                                       ALatitude               : Double;
                                       ALongitude              : Double;
                                       AIrrigationName         : String;
                                       AVersionNo              : Integer;
                                       AMAP                    : Double;
                                       AModelType              : Integer;
                                       ALastUsedModelType      : Integer;
                                       ARainfallFileName       : String;
                                       AMaxAnnualIrrAllocation : Double;
                                       AAbstractionRouteNo     : Integer;
                                       AReturnFlowRouteNo      : Integer;
                                       AReturnFlowPercentage   : Double) : Boolean;
    function UpdateWQTDataInDB (AModuleID                         : Integer;
                                AProduceNetReturnFlows            : Integer;
                                AIrrigationEfficiencyFactor       : Double;
                                ATransferCanalSaltLossProportion  : Double;
                                ATransferCanalFlowLossProportion  : Double;
                                ATransferCanalSeepage             : Double;
                                ARunOffModuleNo                   : Integer;
                                AWaterAllocationInterpolationType : Integer;
                                AMaxWaterAllocation               : Double;
                                AReturnFlowFactor                 : Double;
                                AUpperZoneReturnFlowProportion    : Double;
                                ALowerZoneReturnFlowProportion    : Double;
                                ASaltConcentrationFactor          : Double;
                                ALandSaltLossProportion           : Double;
                                ASaltLoad1                        : Double;
                                ASaltLoad2                        : Double;
                                AInitialSaltLoadLowerZone         : Double;
                                AInitialSaltLoadUpperZone         : Double;
                                ASoilMoistureCapacityLowerZone    : Double;
                                ASoilMoistureCapacityUpperZone    : Double;
                                AInitialSoilMoisture              : Double;
                                ATargetSoilMoisture               : Double;
                                AEffectiveRainfallFactor1         : Double;
                                AEffectiveRainfallFactor2         : Double): Boolean;
    function UpdateFactorsDataInDB (AModuleID : Integer;
                                    AMonthList  : TStringList;
                                    APindexList : TStringList;
                                    ARainList   : TStringList;
                                    ACropList   : TStringList;
                                    AAPanList   : TStringList): Boolean;
    function UpdateAreaDataInDB (AModuleID          : Integer;
                                 AInterpolationType : Integer;
                                 AYearList          : TStringList;
                                 AAreaList          : TStringList) : Boolean;
    function UpdateAllocationGrowthDataInDB (AModuleID          : Integer;
                                             AInterpolationType : Integer;
                                             AYearList          : TStringList;
                                             AGrowthList        : TStringList): Boolean;
    function UpdateReturnFlowDataInDB (AModuleID          : Integer;
                                       AInterpolationType : Integer;
                                       AYearList          : TStringList;
                                       AGrowthList        : TStringList): Boolean;
    function UpdateEfficiencyDataInDB (AModuleID          : Integer;
                                       AInterpolationType : Integer;
                                       AYearList          : TStringList;
                                       AEfficiencyList    : TStringList): Boolean;
    function UpdateCropsDataInDB (AModuleID       : Integer;
                                  ACropNoList     : TStringList;
                                  APercentageList : TStringList;
                                  AFactorsList    : TStringList): Boolean;

    function CreateNewIrrigationModuleInDB (ANetworkID: Integer) : TIrrigationModule;
    function RemoveIrrigationModuleFromDB (AModuleID : Integer) : Boolean;
  end;

var
  GIrrigationDBManager : TIrrigationDBManager;

implementation


uses
  SysUtils,
  Windows,
  DB,

  UFileReader,
  UHydroDBAgent,
  UErrorHandlingOperations;

(* TIrrigationDBManager *******************************************************)

function TIrrigationDBManager.DeleteAreaFromDB (AModuleID : Integer) : Boolean;
const OPNAME = 'TIrrigationDBManager.DeleteAreaFromDB';
var
  LSQL : String;
begin
  Result := FALSE;
  try
    LSQL := 'DELETE * FROM IrrigationArea WHERE ModuleID = ' + IntToStr(AModuleID);
    Result := GHydroDBAgent.ExecuteSQL(LSQL, True);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIrrigationDBManager.DeleteCropsFromDB (AModuleID : Integer) : Boolean;
const OPNAME = 'TIrrigationDBManager.DeleteCropsFromDB';
var
  LSQL : String;
begin
  Result := FALSE;
  try
    LSQL := 'DELETE * FROM IrrigationCrops WHERE ModuleID = ' + IntToStr(AModuleID);
    Result := GHydroDBAgent.ExecuteSQL(LSQL, True);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIrrigationDBManager.DeleteEfficiencyFromDB (AModuleID : Integer) : Boolean;
const OPNAME = 'TIrrigationDBManager.DeleteEfficiencyFromDB';
var
  LSQL : String;
begin
  Result := FALSE;
  try
    LSQL := 'DELETE * FROM IrrigationEfficiency WHERE ModuleID = ' + IntToStr(AModuleID);
    Result := GHydroDBAgent.ExecuteSQL(LSQL, True);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIrrigationDBManager.DeleteMonthlyFactorsFromDB (AModuleID : Integer) : Boolean;
const OPNAME = 'TIrrigationDBManager.DeleteMonthlyFactorsFromDB';
var
  LSQL : String;
begin
  Result := FALSE;
  try
    LSQL := 'DELETE * FROM IrrigationModulesMonthlyData WHERE ModuleID = ' + IntToStr(AModuleID);
    Result := GHydroDBAgent.ExecuteSQL(LSQL, True);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIrrigationDBManager.DeleteReturnFlowFromDB (AModuleID : Integer) : Boolean;
const OPNAME = 'TIrrigationDBManager.DeleteReturnFlowFromDB';
var
  LSQL : String;
begin
  Result := FALSE;
  try
    LSQL := 'DELETE * FROM IrrigationReturnFlow WHERE ModuleID = ' + IntToStr(AModuleID);
    Result := GHydroDBAgent.ExecuteSQL(LSQL, True);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIrrigationDBManager.DeleteAllocationGrowthFromDB (AModuleID : Integer) : Boolean;
const OPNAME = 'TIrrigationDBManager.DeleteAllocationGrowthFromDB';
var
  LSQL : String;
begin
  Result := FALSE;
  try
    LSQL := 'DELETE * FROM IrrigationAllocationGrowth WHERE ModuleID = ' + IntToStr(AModuleID);
    Result := GHydroDBAgent.ExecuteSQL(LSQL, True);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIrrigationDBManager.DeleteIrrigationModuleFromDB (AModuleID : Integer) : Boolean;
const OPNAME = 'TIrrigationDBManager.DeleteIrrigationModuleFromDB';
var
  LSQL : String;
begin
  Result := FALSE;
  try
    LSQL := 'DELETE * FROM IrrigationModules WHERE ModuleID = ' + IntToStr(AModuleID);
    Result := GHydroDBAgent.ExecuteSQL(LSQL, True);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIrrigationDBManager.InsertIrrigationIntoDB (AModuleID : Integer; AName : String) : Boolean;
const OPNAME = 'TIrrigationDBManager.InsertIrrigationIntoDB';
var
  LSQL : String;
begin
  Result := FALSE;
  try
    LSQL := 'INSERT INTO IrrigationModules (ModuleID, IrrigationName) VALUES (' +
            IntToStr(AModuleID) + ', ' + QuotedStr(AName) +  ')';
    Result := GHydroDBAgent.ExecuteSQL(LSQL, True);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TIrrigationDBManager.InsertMonthlyFactorsIntoDB (AModuleID : Integer) : Boolean;
const OPNAME = 'TIrrigationDBManager.InsertMonthlyFactorsIntoDB';
var
  LSQL    : String;
  LMonth  : Integer;
  LResult : Boolean;
begin
  Result := FALSE;
  try
    LResult := TRUE;
    for LMonth := 1 to 12 do
    begin
      LSQL := 'INSERT INTO IrrigationModulesMonthlyData VALUES (' +
              IntToStr(AModuleID) + ', ' + IntToStr(LMonth) + ', 0, 0, 0, 0)';
      LResult := LResult AND GHydroDBAgent.ExecuteSQL(LSQL, True);
    end;
    Result := LResult;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TIrrigationDBManager.InsertAreaDataIntoDB (AModuleID : Integer; AYear, AArea : String) : Boolean;
const OPNAME = 'TIrrigationDBManager.InsertAreaDataIntoDB';
var
  LSQL : String;
begin
  Result := FALSE;
  try
    LSQL := 'INSERT INTO IrrigationArea VALUES (' + IntToStr(AModuleID) + ', ' + AYear + ', ' + AArea + ')';
    Result := GHydroDBAgent.ExecuteSQL(LSQL, True);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIrrigationDBManager.InsertAllocationGrowthDataIntoDB (AModuleID : Integer; AYear, AGrowth : String) : Boolean;
const OPNAME = 'TIrrigationDBManager.InsertAllocationGrowthDataIntoDB';
var
  LSQL : String;
begin
  Result := FALSE;
  try
    LSQL := 'INSERT INTO IrrigationAllocationGrowth VALUES (' + IntToStr(AModuleID) + ', ' + AYear + ', ' + AGrowth + ')';
    Result := GHydroDBAgent.ExecuteSQL(LSQL, True);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIrrigationDBManager.InsertReturnFlowDataIntoDB (AModuleID : Integer; AYear, AGrowth : String) : Boolean;
const OPNAME = 'TIrrigationDBManager.InsertReturnFlowDataIntoDB';
var
  LSQL : String;
begin
  Result := FALSE;
  try
    LSQL := 'INSERT INTO IrrigationReturnFlow VALUES (' + IntToStr(AModuleID) + ', ' + AYear + ', ' + AGrowth + ')';
    Result := GHydroDBAgent.ExecuteSQL(LSQL, True);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIrrigationDBManager.InsertEfficiencyDataIntoDB (AModuleID : Integer; AYear, AEfficiency : String) : Boolean;
const OPNAME = 'TIrrigationDBManager.InsertEfficiencyDataIntoDB';
var
  LSQL : String;
begin
  Result := FALSE;
  try
    LSQL := 'INSERT INTO IrrigationEfficiency VALUES (' + IntToStr(AModuleID) + ', ' + AYear + ', ' + AEfficiency + ')';
    Result := GHydroDBAgent.ExecuteSQL(LSQL, True);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function  TIrrigationDBManager.InsertCropsDataIntoDB (AModuleID : Integer; ACropNo, APercentage, AName : String;
                                                      ACropFactors : TMonthlyDoubleArray) : Boolean;
const OPNAME = 'TIrrigationDBManager.InsertCropsDataIntoDB';
var
  LSQL : String;
  LMonth : Integer;
begin
  Result := FALSE;
  try
    LSQL := 'INSERT INTO IrrigationCrops VALUES (' + IntToStr(AModuleID) + ', ' + ACropNo + ', ' + APercentage;
    for LMonth := 1 to 12 do
      LSQL := LSQL + ', ' + FloatToStr(ACropFactors[LMonth]);
    LSQL := LSQL + ',' + QuotedStr(AName) + ')';
    Result := GHydroDBAgent.ExecuteSQL(LSQL, True);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIrrigationDBManager.UpdatePropertiesDataInDB (AModuleID               : Integer;
                                                        AActive                 : String;
                                                        ALatitude               : Double;
                                                        ALongitude              : Double;
                                                        AIrrigationName         : String;
                                                        AVersionNo              : Integer;
                                                        AMAP                    : Double;
                                                        AModelType              : Integer;
                                                        ALastUsedModelType      : Integer;
                                                        ARainfallFileName       : String;
                                                        AMaxAnnualIrrAllocation : Double;
                                                        AAbstractionRouteNo     : Integer;
                                                        AReturnFlowRouteNo      : Integer;
                                                        AReturnFlowPercentage   : Double) : Boolean;
const OPNAME = 'TIrrigationDBManager.UpdatePropertiesDataInDB';
var
  LSQL : String;
begin
  Result := FALSE;
  try
    LSQL := 'UPDATE (Modules INNER JOIN IrrigationModules ON Modules.ModuleID = IrrigationModules.ModuleID) ' +
            'INNER JOIN NetworkModules ON Modules.ModuleID = NetworkModules.ModuleID SET' +
            ' Modules.Latitude = '                          + FloatToStr(ALatitude) +
            ', Modules.Longitude = '                        + FloatToStr(ALongitude) +
            ', NetworkModules.Active = '                    + QuotedStr(AActive) +
            ', IrrigationModules.VersionNo = '              + IntToStr(AVersionNo) +
            ', IrrigationModules.MAP = '                    + FloatToStr(AMAP) +
            ', IrrigationModules.ModelType = '              + IntToStr(AModelType) +
            ', IrrigationModules.LastUsedModelType = '      + IntToStr(ALastUsedModelType) +
            ', IrrigationModules.MaxAnnualIrrAllocation = ' + FloatToStr(AMaxAnnualIrrAllocation) +
            ', IrrigationModules.AbstractionRouteNo = '     + IntToStr(AAbstractionRouteNo) +
            ', IrrigationModules.ReturnFlowRouteNo = '      + IntToStr(AReturnFlowRouteNo) +
            ', IrrigationModules.PercentageReturn = '       + FloatToStr(AReturnFlowPercentage);
    if (AIrrigationName = '') then
      LSQL := LSQL + ', IrrigationModules.IrrigationName = NULL'
    else
      LSQL := LSQL + ', IrrigationModules.IrrigationName = ' + QuotedStr(AIrrigationName);
    if (ARainfallFileName = '') then
      LSQL := LSQL + ', IrrigationModules.RainfallFileName = NULL'
    else
      LSQL := LSQL + ', IrrigationModules.RainfallFileName = ' + QuotedStr(ARainfallFileName);

    LSQL := LSQL + ' WHERE Modules.ModuleID = ' + IntToStr(AModuleID);
    Result := GHydroDBAgent.ExecuteSQL(LSQL, True);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TIrrigationDBManager.UpdateWQTDataInDB (AModuleID : Integer;
                                                 AProduceNetReturnFlows            : Integer;
                                                 AIrrigationEfficiencyFactor       : Double;
                                                 ATransferCanalSaltLossProportion  : Double;
                                                 ATransferCanalFlowLossProportion  : Double;
                                                 ATransferCanalSeepage             : Double;
                                                 ARunOffModuleNo                   : Integer;
                                                 AWaterAllocationInterpolationType : Integer;
                                                 AMaxWaterAllocation               : Double;
                                                 AReturnFlowFactor                 : Double;
                                                 AUpperZoneReturnFlowProportion    : Double;
                                                 ALowerZoneReturnFlowProportion    : Double;
                                                 ASaltConcentrationFactor          : Double;
                                                 ALandSaltLossProportion           : Double;
                                                 ASaltLoad1                        : Double;
                                                 ASaltLoad2                        : Double;
                                                 AInitialSaltLoadLowerZone         : Double;
                                                 AInitialSaltLoadUpperZone         : Double;
                                                 ASoilMoistureCapacityLowerZone    : Double;
                                                 ASoilMoistureCapacityUpperZone    : Double;
                                                 AInitialSoilMoisture              : Double;
                                                 ATargetSoilMoisture               : Double;
                                                 AEffectiveRainfallFactor1         : Double;
                                                 AEffectiveRainfallFactor2         : Double): Boolean;
const OPNAME = 'TIrrigationDBManager.UpdateWQTDataInDB';
var
  LSQL : String;
begin
  Result := TRUE;
  try
    LSQL := 'UPDATE IrrigationModules SET' +
            ' ProduceNetReturnFlows = '             + IntToStr(AProduceNetReturnFlows) +
            ', IrrigationEfficiencyFactor = '       + FloatToStr(AIrrigationEfficiencyFactor) +
            ', CanalSaltLossProp = '                + FloatToStr(ATransferCanalSaltLossProportion) +
            ', CanalFlowLossProp = '                + FloatToStr(ATransferCanalFlowLossProportion) +
            ', TransferCanalSeepage = '             + FloatToStr(ATransferCanalSeepage) +
            ', RunOffModuleNo = '                   + IntToStr(ARunOffModuleNo) +
            ', WaterAllocationInterpolationType = ' + IntToStr(AWaterAllocationInterpolationType) +
            ', MaxWaterAllocation = '               + FloatToStr(AMaxWaterAllocation) +
            ', ReturnFlowFactor = '                 + FloatToStr(AReturnFlowFactor) +
            ', UpperZoneReturnFlowProp = '          + FloatToStr(AUpperZoneReturnFlowProportion) +
            ', LowerZoneReturnFlowProp = '          + FloatToStr(ALowerZoneReturnFlowProportion) +
            ', SaltConcentrationFactor = '          + FloatToStr(ASaltConcentrationFactor) +
            ', LandSaltLossProp = '                 + FloatToStr(ALandSaltLossProportion) +
            ', SaltLoad1 = '                        + FloatToStr(ASaltLoad1) +
            ', SaltLoad2 = '                        + FloatToStr(ASaltLoad2) +
            ', SaltLoadLowerZone = '                + FloatToStr(AInitialSaltLoadLowerZone) +
            ', SaltLoadUpperZone = '                + FloatToStr(AInitialSaltLoadUpperZone) +
            ', SoilMoistureLowerZone = '            + FloatToStr(ASoilMoistureCapacityLowerZone) +
            ', SoilMoistureUpperZone = '            + FloatToStr(ASoilMoistureCapacityUpperZone) +
            ', InitialSoilMoisture = '              + FloatToStr(AInitialSoilMoisture) +
            ', TargetSoilMoisture = '               + FloatToStr(ATargetSoilMoisture) +
            ', EffectiveRainfallFactor1 = '         + FloatToStr(AEffectiveRainfallFactor1) +
            ', EffectiveRainfallFactor2 = '         + FloatToStr(AEffectiveRainfallFactor2);
    LSQL := LSQL + ' WHERE ModuleID = ' + IntToStr(AModuleID);
    Result := GHydroDBAgent.ExecuteSQL(LSQL, True);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TIrrigationDBManager.UpdateFactorsDataInDB (AModuleID : Integer;
                                                     AMonthList  : TStringList;
                                                     APindexList : TStringList;
                                                     ARainList   : TStringList;
                                                     ACropList   : TStringList;
                                                     AAPanList   : TStringList): Boolean;
const OPNAME = 'TIrrigationDBManager.UpdateFactorsDataInDB';
var
  LSQL            : String;
  LIndex          : Integer;
  LMonth          : Integer;
  LPIndexFactor   : Double;
  LRainfallFactor : Double;
  LCropFactor     : Double;
  LAPanFactor     : Double;
begin
  Result := FALSE;
  try
    LIndex := 0;
    Result := TRUE;
    GHydroDBAgent.StartTransaction;
    try
      while (Result AND (LIndex < AMonthList.Count)) do
      begin
        LMonth          := StrToInt(AMonthList.Strings[LIndex]);
        LPIndexFactor   := StrToFloat(APindexList.Strings[LIndex]);
        LRainfallFactor := StrToFloat(ARainList.Strings[LIndex]);
        LCropFactor     := StrToFloat(ACropList.Strings[LIndex]);
        LAPanFactor     := StrToFloat(AAPanList.Strings[LIndex]);

        LSQL := 'UPDATE IrrigationModulesMonthlyData SET' +
                ' Pindex = '                 + FloatToStr(LPIndexFactor) +
                ', MonthlyRainfallFactor = ' + FloatToStr(LRainfallFactor) +
                ', MonthlyCropFactor = '     + FloatToStr(LCropFactor) +
                ', APanFactor = '            + FloatToStr(LAPanFactor) +
                ' WHERE ModuleID = ' + IntToStr(AModuleID) + ' AND Month = ' + IntToStr(LMonth);
        Result := GHydroDBAgent.ExecuteSQL(LSQL, True);
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

function TIrrigationDBManager.UpdateAreaDataInDB (AModuleID          : Integer;
                                                  AInterpolationType : Integer;
                                                  AYearList          : TStringList;
                                                  AAreaList          : TStringList) : Boolean;
const OPNAME = 'TIrrigationDBManager.UpdateAreaDataInDB';
var
  LSQL   : String;
  LIndex : Integer;
begin
  Result := FALSE;
  try
    GHydroDBAgent.StartTransaction;
    try
      LSQL := 'UPDATE IrrigationModules SET AreaInterpolationType = ' + IntToStr(AInterpolationType) +
              ' WHERE ModuleID = ' + IntToStr(AModuleID);
      Result := GHydroDBAgent.ExecuteSQL(LSQL, True);

      if (Result) then
        Result := DeleteAreaFromDB(AModuleID);

      LIndex := 0;
      while (Result AND (LIndex < AYearList.Count)) do
      begin
        Result := GIrrigationDBManager.InsertAreaDataIntoDB(AModuleID, AYearList.Strings[LIndex], AAreaList.Strings[LIndex]);
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

function TIrrigationDBManager.UpdateAllocationGrowthDataInDB (AModuleID          : Integer;
                                                              AInterpolationType : Integer;
                                                              AYearList          : TStringList;
                                                              AGrowthList        : TStringList): Boolean;
const OPNAME = 'TIrrigationDBManager.UpdateAllocationGrowthDataInDB';
var
  LSQL   : String;
  LIndex : Integer;
begin
  Result := FALSE;
  try
    GHydroDBAgent.StartTransaction;
    try
      LSQL := 'UPDATE IrrigationModules SET GrowthInterpolationType = ' + IntToStr(AInterpolationType) +
              ' WHERE ModuleID = ' + IntToStr(AModuleID);
      Result := GHydroDBAgent.ExecuteSQL(LSQL, True);

      if (Result) then
        Result := DeleteAllocationGrowthFromDB(AModuleID);

      LIndex := 0;
      while (Result AND (LIndex < AYearList.Count)) do
      begin
        Result := GIrrigationDBManager.InsertAllocationGrowthDataIntoDB(AModuleID, AYearList.Strings[LIndex], AGrowthList.Strings[LIndex]);
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

function TIrrigationDBManager.UpdateReturnFlowDataInDB (AModuleID          : Integer;
                                                        AInterpolationType : Integer;
                                                        AYearList          : TStringList;
                                                        AGrowthList        : TStringList): Boolean;
const OPNAME = 'TIrrigationDBManager.UpdateReturnFlowDataInDB';
var
  LSQL    : String;
  LIndex  : Integer;
begin
  Result := FALSE;
  try
    GHydroDBAgent.StartTransaction;
    try
      LSQL := 'UPDATE IrrigationModules SET ReturnFlowInterpolationType = ' + IntToStr(AInterpolationType) +
              ' WHERE ModuleID = ' + IntToStr(AModuleID);
      Result := GHydroDBAgent.ExecuteSQL(LSQL, True);

      if (Result) then
        Result := GIrrigationDBManager.DeleteReturnFlowFromDB(AModuleID);

      LIndex := 0;
      while (Result AND (LIndex < AYearList.Count)) do
      begin
        Result := InsertReturnFlowDataIntoDB(AModuleID, AYearList.Strings[LIndex], AGrowthList.Strings[LIndex]);
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

function TIrrigationDBManager.UpdateEfficiencyDataInDB (AModuleID          : Integer;
                                                        AInterpolationType : Integer;
                                                        AYearList          : TStringList;
                                                        AEfficiencyList    : TStringList): Boolean;
const OPNAME = 'TIrrigationDBManager.UpdateEfficiencyDataInDB';
var
  LSQL   : String;
  LIndex : Integer;
begin
  Result := FALSE;
  try
    GHydroDBAgent.StartTransaction;
    try
      LSQL := 'UPDATE IrrigationModules SET EfficiencyInterpolationType = ' + IntToStr(AInterpolationType) +
              ' WHERE ModuleID = ' + IntToStr(AModuleID);
      Result := GHydroDBAgent.ExecuteSQL(LSQL, True);

      if (Result) then
        Result := GIrrigationDBManager.DeleteEfficiencyFromDB(AModuleID);

      LIndex := 0;
      while (Result AND (LIndex < AYearList.Count)) do
      begin
        Result := InsertEfficiencyDataIntoDB(AModuleID, AYearList.Strings[LIndex], AEfficiencyList.Strings[LIndex]);
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

function TIrrigationDBManager.UpdateCropsDataInDB (AModuleID       : Integer;
                                                   ACropNoList     : TStringList;
                                                   APercentageList : TStringList;
                                                   AFactorsList    : TStringList): Boolean;
const OPNAME = 'TIrrigationDBManager.UpdateCropsDataInDB';
var
  LIndex           : Integer;
  LCount           : Integer;
  LCropNo          : String;
  LCropPercentage  : String;
  LCropFactors     : String;
  LFactorsList     : TStringList;
  LFactors         : TMonthlyDoubleArray;
begin
  Result := FALSE;
  try
    GHydroDBAgent.StartTransaction;
    try
      Result := GIrrigationDBManager.DeleteCropsFromDB(AModuleID);

      LFactorsList := TStringList.Create;
      try
        LIndex := 0;
        while (Result AND (LIndex < ACropNoList.Count)) do
        begin
          LCropNo          := ACropNoList.Strings[LIndex];
          LCropPercentage  := APercentageList.Strings[LIndex];
          LCropFactors     := AFactorsList.Strings[LIndex];
          LFactorsList.Clear;
          LFactorsList.CommaText := LCropFactors;
          for LCount := 0 to LFactorsList.Count - 1 do
            LFactors[LCount + 1] := StrToFloat(LFactorsList.Strings[LCount]);

          Result := InsertCropsDataIntoDB(AModuleID, LCropNo, LCropPercentage, 'Composite', LFactors);
          LIndex := LIndex + 1;
        end;
        if (Result) then
          GHydroDBAgent.CommitTransaction
        else
          GHydroDBAgent.RollbackTransaction;  
      finally
        LFactorsList.Free;
      end;
    except
      GHydroDBAgent.RollbackTransaction;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TIrrigationDBManager.LoadIrrigationModulesFromDB (ANetworkID : Integer) : Boolean;
const OPNAME = 'TIrrigationDBManager.LoadIrrigationModulesFromDB';
var
  LQuery            : TDataSet;
  LSQL              : String;
  LIrrigationModule : TIrrigationModule;
  LModuleID         : Integer;
begin
  Result := FALSE;
  try
    LSQL := 'SELECT A.ModuleNumber, A.NetworkSequence, A.Active, B.*, C.ModuleType, C.Longitude, C.Latitude ' +
            'FROM (NetworkModules AS A LEFT JOIN IrrigationModules AS B ON A.ModuleID = B.ModuleID) ' +
            'LEFT JOIN Modules AS C ON A.ModuleID = C.ModuleID WHERE C.ModuleType = ' +  QuotedStr('RR') +
            ' AND A.NetworkID = ' + IntToStr(ANetworkID);
    LQuery := GHydroDBAgent.CreateQuery(LSQL);
    try
      LQuery.Open;
      while (NOT LQuery.Eof) do
      begin
        LIrrigationModule := TIrrigationModuleAgent(ModuleAgent).AddIrrigationModule;
        LModuleID         := LQuery.FieldByName('ModuleID').AsInteger;

        LIrrigationModule.Populate(ANetworkID,
                                   LModuleID,
                                   Trim(LQuery.FieldByName('ModuleType').AsString),
                                   LQuery.FieldByName('ModuleNumber').AsInteger,
                                   LQuery.FieldByName('NetworkSequence').AsInteger,
                                   Trim(LQuery.FieldByName('Active').AsString),
                                   LQuery.FieldByName('VersionNo').AsInteger,
                                   Trim(LQuery.FieldByName('IrrigationName').AsString),
                                   LQuery.FieldByName('ModelType').AsInteger,
                                   LQuery.FieldByName('LastUsedModelType').AsInteger,
                                   LQuery.FieldByName('MAP').AsFloat,
                                   Trim(LQuery.FieldByName('RainfallFileName').AsString),
                                   LQuery.FieldByName('MaxAnnualIrrAllocation').AsFloat,
                                   LQuery.FieldByName('AbstractionRouteNo').AsInteger,
                                   LQuery.FieldByName('ReturnFlowRouteNo').AsInteger,
                                   LQuery.FieldByName('PercentageReturn').AsFloat,
                                   LQuery.FieldByName('AreaInterpolationType').AsInteger,
                                   LQuery.FieldByName('MaxWaterAllocation').AsFloat,
                                   LQuery.FieldByName('NumberOfDataPoints').AsInteger,
                                   LQuery.FieldByName('WaterAllocationInterpolationType').AsInteger,
                                   LQuery.FieldByName('RunOffModuleNo').AsInteger,
                                   LQuery.FieldByName('TransferCanalSeepage').AsFloat,
                                   LQuery.FieldByName('ProduceNetReturnFlows').AsInteger,
                                   LQuery.FieldByName('CanalFlowLossProp').AsFloat,
                                   LQuery.FieldByName('CanalSaltLossProp').AsFloat,
                                   LQuery.FieldByName('IrrigationEfficiencyFactor').AsFloat,
                                   LQuery.FieldByName('ReturnFlowFactor').AsFloat,
                                   LQuery.FieldByName('UpperZoneReturnFlowProp').AsFloat,
                                   LQuery.FieldByName('LowerZoneReturnFlowProp').AsFloat,
                                   LQuery.FieldByName('SaltConcentrationFactor').AsFloat,
                                   LQuery.FieldByName('LandSaltLossProp').AsFloat,
                                   LQuery.FieldByName('SaltLoad1').AsFloat,
                                   LQuery.FieldByName('SaltLoad2').AsFloat,
                                   LQuery.FieldByName('SaltLoadUpperZone').AsFloat,
                                   LQuery.FieldByName('SaltLoadLowerZone').AsFloat,
                                   LQuery.FieldByName('SoilMoistureUpperZone').AsFloat,
                                   LQuery.FieldByName('SoilMoistureLowerZone').AsFloat,
                                   LQuery.FieldByName('TargetSoilMoisture').AsFloat,
                                   LQuery.FieldByName('InitialSoilMoisture').AsFloat,
                                   LQuery.FieldByName('EffectiveRainfallFactor1').AsFloat,
                                   LQuery.FieldByName('EffectiveRainfallFactor2').AsFloat,
                                   LQuery.FieldByName('GrowthInterpolationType').AsInteger,
                                   LQuery.FieldByName('ReturnFlowInterpolationType').AsInteger,
                                   LQuery.FieldByName('EfficiencyInterpolationType').AsInteger,
                                   LQuery.FieldByName('Longitude').AsFloat,
                                   LQuery.FieldByName('Latitude').AsFloat);

        GIrrigationDBManager.LoadPanDataFromDB(LIrrigationModule);
        GIrrigationDBManager.LoadIrrigationMonthlyDataFromDB(LIrrigationModule);
        GIrrigationDBManager.LoadIrrigationAreaDataFromDB(LIrrigationModule);
        GIrrigationDBManager.LoadIrrigationAllocationGrowthDataFromDB(LIrrigationModule);
        GIrrigationDBManager.LoadIrrigationCropsFromDB(LIrrigationModule);
        GIrrigationDBManager.LoadIrrigationEfficiencyFromDB(LIrrigationModule);
        GIrrigationDBManager.LoadIrrigationReturnFlowFromDB(LIrrigationModule);
        LQuery.Next;
      end;
    finally
      LQuery.Close;
      LQuery.Free;
    end;
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIrrigationDBManager.LoadIrrigationMonthlyDataFromDB (AIrrigationModule : TIrrigationModule) : Boolean;
const OPNAME = 'TIrrigationDBManager.LoadIrrigationMonthlyDataFromDB';
var
  LQuery  : TDataSet;
  LSQL    : String;
  LMonth  : Integer;
begin
  Result := FALSE;
  try
    LSQL := 'SELECT * FROM IrrigationModulesMonthlyData WHERE ModuleID = ' + IntToStr(AIrrigationModule.ModuleID) +
            ' ORDER BY Month';
    LQuery := GHydroDBAgent.CreateQuery(LSQL);
    try
      LQuery.Open;
      while (NOT LQuery.Eof) do
      begin
        LMonth := LQuery.FieldByName('Month').AsInteger;
        AIrrigationModule.PIndexFactorByMonth[LMonth]   := LQuery.FieldByName('Pindex').AsFloat;
        AIrrigationModule.RainfallFactorByMonth[LMonth] := LQuery.FieldByName('MonthlyRainfallFactor').AsFloat;
        AIrrigationModule.CropFactorByMonth[LMonth]     := LQuery.FieldByName('MonthlyCropFactor').AsFloat;
        AIrrigationModule.APanFactorByMonth[LMonth]     := LQuery.FieldByName('APanFactor').AsFloat;
        LQuery.Next;
      end;
    finally
      LQuery.Close;
      LQuery.Free;
    end;
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIrrigationDBManager.LoadIrrigationAreaDataFromDB (AIrrigationModule : TIrrigationModule) : Boolean;
const OPNAME = 'TIrrigationDBManager.LoadIrrigationAreaDataFromDB';
var
  LQuery : TDataSet;
  LSQL   : String;
begin
  Result := FALSE;
  try
    AIrrigationModule.ClearAreaData;
    // Load IrrigationArea data
    LSQL := 'SELECT * FROM IrrigationArea WHERE ModuleID = ' + IntToStr(AIrrigationModule.ModuleID);
    LQuery := GHydroDBAgent.CreateQuery(LSQL);
    try
      LQuery.Open;
      while (NOT LQuery.Eof) do
      begin
        AIrrigationModule.AddAreaData(LQuery.FieldByName('Year').AsInteger,
                                      LQuery.FieldByName('Area').AsFloat);
        LQuery.Next;
      end;
    finally
      LQuery.Close;
      LQuery.Free;
    end;

    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TIrrigationDBManager.LoadIrrigationAllocationGrowthDataFromDB (AIrrigationModule : TIrrigationModule) : Boolean;
const OPNAME = 'TIrrigationDBManager.LoadIrrigationAllocationGrowthDataFromDB';
var
  LQuery  : TDataSet;
  LSQL    : String;
begin
  Result := FALSE;
  try
    AIrrigationModule.ClearAllocationGrowthData;
    // Load IrrigationAllocationGrowth data
    LSQL := 'SELECT * FROM IrrigationAllocationGrowth WHERE ModuleID = ' + IntToStr(AIrrigationModule.ModuleID);
    LQuery := GHydroDBAgent.CreateQuery(LSQL);
    try
      LQuery.Open;
      while (NOT LQuery.Eof) do
      begin
        AIrrigationModule.AddAllocationGrowthData(LQuery.FieldByName('Year').AsInteger,
                                                  LQuery.FieldByName('Growth').AsFloat);
        LQuery.Next;
      end;
    finally
      LQuery.Close;
      LQuery.Free;
    end;

    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TIrrigationDBManager.LoadIrrigationCropsFromDB (AIrrigationModule : TIrrigationModule) : Boolean;
const OPNAME = 'TIrrigationDBManager.LoadIrrigationCropsFromDB';
var
  LQuery     : TDataSet;
  LSQL       : String;
  LCrop      : TIrrigationCrop;
  LTempsStr  : String;
begin
  Result := FALSE;
  try
    AIrrigationModule.ClearCropsData;
    // Load IrrigationCrops data
    LSQL := 'SELECT * FROM IrrigationCrops WHERE ModuleID = ' + IntToStr(AIrrigationModule.ModuleID);
    LQuery := GHydroDBAgent.CreateQuery(LSQL);
    try
      LQuery.Open;
      while (NOT LQuery.Eof) do
      begin
        LTempsStr := FloatToStr(LQuery.FieldByName('CropFactor01').AsFloat) + ',' +
                     FloatToStr(LQuery.FieldByName('CropFactor02').AsFloat) + ',' +
                     FloatToStr(LQuery.FieldByName('CropFactor03').AsFloat) + ',' +
                     FloatToStr(LQuery.FieldByName('CropFactor04').AsFloat) + ',' +
                     FloatToStr(LQuery.FieldByName('CropFactor05').AsFloat) + ',' +
                     FloatToStr(LQuery.FieldByName('CropFactor06').AsFloat) + ',' +
                     FloatToStr(LQuery.FieldByName('CropFactor07').AsFloat) + ',' +
                     FloatToStr(LQuery.FieldByName('CropFactor08').AsFloat) + ',' +
                     FloatToStr(LQuery.FieldByName('CropFactor09').AsFloat) + ',' +
                     FloatToStr(LQuery.FieldByName('CropFactor10').AsFloat) + ',' +
                     FloatToStr(LQuery.FieldByName('CropFactor11').AsFloat) + ',' +
                     FloatToStr(LQuery.FieldByName('CropFactor12').AsFloat);
        LCrop := AIrrigationModule.AddIrrigationCrop;
        LCrop.Populate(LQuery.FieldByName('CropNo').AsInteger,
                       Trim(LQuery.FieldByName('CropName').AsString),
                       LQuery.FieldByName('CropPercentage').AsFloat,
                       LTempsStr);
        LQuery.Next;
      end;
    finally
      LQuery.Close;
      LQuery.Free;
    end;

    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TIrrigationDBManager.LoadIrrigationEfficiencyFromDB (AIrrigationModule : TIrrigationModule) : Boolean;
const OPNAME = 'TIrrigationDBManager.LoadIrrigationEfficiencyFromDB';
var
  LQuery  : TDataSet;
  LSQL    : String;
begin
  Result := FALSE;
  try
    AIrrigationModule.ClearEfficiencyData;
    // Load IrrigationEfficiency data
    LSQL := 'SELECT * FROM IrrigationEfficiency WHERE ModuleID = ' + IntToStr(AIrrigationModule.ModuleID);
    LQuery := GHydroDBAgent.CreateQuery(LSQL);
    try
      LQuery.Open;
      while (NOT LQuery.Eof) do
      begin
        AIrrigationModule.AddEfficiencyData(LQuery.FieldByName('Year').AsInteger,
                                            LQuery.FieldByName('Efficiency').AsFloat);
        LQuery.Next;
      end;
    finally
      LQuery.Close;
      LQuery.Free;
    end;

    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TIrrigationDBManager.LoadIrrigationReturnFlowFromDB (AIrrigationModule : TIrrigationModule) : Boolean;
const OPNAME = 'TIrrigationDBManager.LoadIrrigationReturnFlowFromDB';
var
  LQuery : TDataSet;
  LSQL   : String;
begin
  Result := FALSE;
  try
    AIrrigationModule.ClearReturnFlowData;
    // Load IrrigationReturnFlow data
    LSQL := 'SELECT * FROM IrrigationReturnFlow WHERE ModuleID = ' + IntToStr(AIrrigationModule.ModuleID);
    LQuery := GHydroDBAgent.CreateQuery(LSQL);
    try
      LQuery.Open;
      while (NOT LQuery.Eof) do
      begin
        AIrrigationModule.AddReturnFlowData(LQuery.FieldByName('Year').AsInteger,
                                            LQuery.FieldByName('Growth').AsFloat);
        LQuery.Next;
      end;
    finally
      LQuery.Close;
      LQuery.Free;
    end;

    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TIrrigationDBManager.CreateNewIrrigationModuleInDB (ANetworkID: Integer): TIrrigationModule;
const OPNAME = 'TIrrigationDBManager.CreateNewIrrigationModuleInDB';
var
  LIrrModule       : TIrrigationModule;
  LModuleID        : Integer;
  LNetworkSequence : Integer;
  LModuleNumber    : Integer;
  LName            : String;
begin
  Result := nil;
  try
    LModuleID        := GHydroDBAgent.GetNextID('Modules', 'ModuleID');
    LNetworkSequence := GetNextNetworkSequence(ANetworkID);
    LModuleNumber    := GetNextModuleNumber(ANetworkID, 'RR');
    LName            := 'RR' + IntToStr(LModuleNumber);

    GHydroDBAgent.StartTransaction;
    try
      if (InsertModuleIntoDB(LModuleID, 'RR', 0, 0)) AND
         (InsertPanIntoDB(LModuleID)) AND
         (InsertNetworkModuleIntoDB(ANetworkID, LModuleID, LModuleNumber, LNetworkSequence, 'Y')) AND
         (InsertIrrigationIntoDB(LModuleID, LName)) AND
         (InsertMonthlyFactorsIntoDB(LModuleID)) then
      begin
        GHydroDBAgent.CommitTransaction;
        LIrrModule := TIrrigationModuleAgent(ModuleAgent).AddIrrigationModule;
        LIrrModule.Populate(ANetworkID, LModuleID, 'RR', LModuleNumber, LNetworkSequence, 'Y', 0, LName,
                            0, 0, 0, '', 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 );
        LoadPanDataFromDB(LIrrModule);
        LoadIrrigationMonthlyDataFromDB(LIrrModule);
        Result := LIrrModule;
      end
      else
        GHydroDBAgent.RollbackTransaction;
    except
      GHydroDBAgent.RollbackTransaction;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIrrigationDBManager.RemoveIrrigationModuleFromDB (AModuleID : Integer) : Boolean;
const OPNAME = 'TIrrigationDBManager.RemoveIrrigationModuleFromDB';
begin
  Result := FALSE;
  try
    GHydroDBAgent.StartTransaction;
    try
      if (DeleteReturnFlowFromDB(AModuleID)) AND
         (DeleteMonthlyFactorsFromDB(AModuleID)) AND
         (DeleteEfficiencyFromDB(AModuleID)) AND
         (DeleteCropsFromDB(AModuleID)) AND
         (DeleteAreaFromDB(AModuleID)) AND
         (DeleteAllocationGrowthFromDB(AModuleID)) AND
         (DeletePanFromDB(AModuleID)) AND
         (DeleteIrrigationModuleFromDB(AModuleID)) AND
         (DeleteNetworkModuleFromDB(AModuleID)) AND
         (DeleteModuleFromDB(AModuleID)) then
      begin
        Result := TRUE;
        GHydroDBAgent.CommitTransaction
      end
      else
        GHydroDBAgent.RollbackTransaction;
    except
      GHydroDBAgent.RollbackTransaction;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;


end.

