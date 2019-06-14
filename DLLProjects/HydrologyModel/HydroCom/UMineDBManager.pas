(******************************************************************************)
(* Contains : TMineDBManager.                                                 *)
(* Contains Database functionality for Mines.                                 *)
(******************************************************************************)

unit UMineDBManager;

interface

uses
  Classes, Contnrs,

  UModuleDBManager,
  UMineModule;

type

  TMineDBManager = class(TModuleDBManager)
  protected
  public
    function DeleteGrowthDataFromDB (AModuleID : Integer; ASectionNo : Integer = 0; AParameter : String = '') : Boolean;
    function DeleteQvsSLDDataFromDB (AModuleID : Integer; ASectionNo : Integer = 0; AParameter : String = '') : Boolean;

    function DeleteSlurryDumpMonthlyDataFromDB (AModuleID : Integer; ASectionNo : Integer = 0) : Boolean;
    function DeleteSlurryDumpFromDB (AModuleID : Integer; ASectionNo : Integer = 0) : Boolean;

    function DeleteUndergroundSectionMonthlyDataFromDB (AModuleID : Integer; ASectionNo : Integer = 0) : Boolean;
    function DeleteUndergroundSectionFromDB (AModuleID : Integer; ASectionNo : Integer = 0) : Boolean;

    function DeleteOpencastPitMonthlyDataFromDB (AModuleID : Integer; ASectionNo : Integer = 0) : Boolean;
    function DeleteOpencastPitFromDB (AModuleID : Integer; ASectionNo : Integer = 0) : Boolean;

    function DeleteMineModuleFromDB (AModuleID : Integer) : Boolean;

    function InsertGrowthDataIntoDB (AModuleID, ASectionNo : Integer; AParameter, AYear, AGrowth : String) : Boolean;
    function InsertQvsSLDDataIntoDB (AModuleID, ASectionNo : Integer; AParameter, AIndex, AFlowRef, ALoad : String) : Boolean;

    function LoadGrowthDataFromDB (ASlurryDump : TSlurryDump) : Boolean;
    function LoadMonthlyDataFromDB (ASlurryDump : TSlurryDump) : Boolean; overload;
    function LoadQvsSLDDataFromDB (ASlurryDump : TSlurryDump) : Boolean; overload;
    function LoadSlurryDumpDataFromDB (AModuleID: Integer): Boolean;

    function LoadBoardPillarGrowthDataFromDB (AUndergroundSection : TUndergroundSection) : Boolean;
    function LoadHighExtractionGrowthDataFromDB (AUndergroundSection : TUndergroundSection) : Boolean;
    function LoadMonthlyDataFromDB (AUndergroundSection : TUndergroundSection) : Boolean; overload;
    function LoadQvsSLDDataFromDB (AUndergroundSection : TUndergroundSection) : Boolean; overload;
    function LoadUndergroundSectionDataFromDB (AModuleID: Integer): Boolean;

    function LoadWorkingAreaGrowthDataFromDB (AOpencastPit : TOpencastPit) : Boolean;
    function LoadDisturbedAreaGrowthDataFromDB (AOpencastPit : TOpencastPit) : Boolean;
    function LoadRehabilitatedAreaGrowthDataFromDB (AOpencastPit : TOpencastPit) : Boolean;
    function LoadPitEvaporationAreaGrowthDataFromDB (AOpencastPit : TOpencastPit) : Boolean;
    function LoadInspoilsDecantGrowthDataFromDB (AOpencastPit : TOpencastPit) : Boolean;
    function LoadInspoilsSeepageGrowthDataFromDB (AOpencastPit : TOpencastPit) : Boolean;
    function LoadWorkingsQvsSLDDataFromDB (AOpencastPit : TOpencastPit) : Boolean;
    function LoadSeepDecantQvsSLDDataFromDB (AOpencastPit : TOpencastPit) : Boolean;
    function LoadMonthlyDataFromDB (AOpencastPit : TOpencastPit) : Boolean; overload;
    function LoadOpencastPitDataFromDB (AModuleID: Integer): WordBool;

    function LoadMinePlantAreaGrowthDataFromDB (AMine : TMineModule): Boolean;
    function LoadMineModulesFromDB (ANetworkID : Integer) : Boolean;

    function GetNextOpencastPitSectionNo (AModuleID : Integer) : Integer;
    function InsertOpencastPitIntoDB (AModuleID, ASectionNo : Integer) : Boolean;
    function InsertOpencastPitMonthlyDataIntoDB (AModuleID, ASectionNo : Integer) : Boolean;
    function CreateNewOpencastPitInDB (AMineModule : TMineModule) : TOpencastPit;
    function RemoveOpencastPitFromDB (AModuleID, ASectionNo : Integer) : Boolean;

    function GetNextUndergroundSectionNo (AModuleID : Integer) : Integer;
    function InsertUndergroundSectionIntoDB (AModuleID, ASectionNo : Integer) : Boolean;
    function InsertUndergroundSectionMonthlyDataIntoDB (AModuleID, ASectionNo : Integer) : Boolean;
    function CreateNewUndergroundSectionInDB (AMineModule : TMineModule) : TUndergroundSection;
    function RemoveUndergroundSectionFromDB (AModuleID, ASectionNo : Integer) : Boolean;

    function GetNextSlurryDumpSectionNo (AModuleID : Integer) : Integer;
    function InsertSlurryDumpIntoDB (AModuleID, ASectionNo : Integer) : Boolean;
    function InsertSlurryDumpMonthlyDataIntoDB (AModuleID, ASectionNo : Integer) : Boolean;
    function CreateNewSlurryDumpInDB (AMineModule : TMineModule) : TSlurryDump;
    function RemoveSlurryDumpFromDB (AModuleID, ASectionNo : Integer) : Boolean;

    function InsertMineIntoDB (AModuleID : Integer; AName : String) : Boolean;
    function CreateNewMineModuleInDB (ANetworkID: Integer) : TMineModule;
    function RemoveMineModuleFromDB(AModuleID : Integer): Boolean;

    function UpdateSlurryDumpPropertiesDataInDB (AModuleID                  : Integer;
                                                 ASectionNo                 : Integer;
                                                 ASectionName               : String;
                                                 ASlurryDumpArea            : Double;
                                                 ASlurryDumpRunOffFactor    : Double;
                                                 ASlurrySeepProportion      : Double;
                                                 ASlurryPCDFullSupplyVolume : Double;
                                                 ASlurryPCDFullSupplyArea   : Double;
                                                 ASlurryPCDInitialVolume    : Double): Boolean;
    function UpdateSlurryDumpGrowthDataInDB (AModuleID          : Integer;
                                             ASectionNo         : Integer;
                                             AInterpolationType : Integer;
                                             AYearList          : TStringList;
                                             AGrowthList        : TStringList) : Boolean;
    function UpdateSlurryDumpMonthlyDataInDB (AModuleID   : Integer;
                                              ASectionNo  : Integer;
                                              AMonthList  : TStringList;
                                              AFactorList : TStringList): Boolean;
    function UpdateSlurryDumpQSLDDataInDB (AModuleID    : Integer;
                                           ASectionNo   : Integer;
                                           AStdDev      : Double;
                                           AFlowRefList : TStringList;
                                           ALoadList    : TStringList): Boolean;

    function UpdateUndergroundSectionPropertiesDataInDB (AModuleID                  : Integer;
                                                         ASectionNo                 : Integer;
                                                         ASectionName               : String;
                                                         AUndergroundOutflowRouteNo : Integer;
                                                         AUpstreamCatchmentArea     : Double;
                                                         ABoardAndPillarArea        : Double;
                                                         AHighExtractionArea        : Double;
                                                         ASurfaceRunOffFactor       : Double): Boolean;
    function UpdateUndergroundSectionBoardPillarDataInDB (AModuleID          : Integer;
                                                          ASectionNo         : Integer;
                                                          AInterpolationType : Integer;
                                                          AYearList          : TStringList;
                                                          AGrowthList        : TStringList): Boolean;
    function UpdateUndergroundSectionHighExtractionDataInDB (AModuleID          : Integer;
                                                             ASectionNo         : Integer;
                                                             AInterpolationType : Integer;
                                                             AYearList          : TStringList;
                                                             AGrowthList        : TStringList): Boolean;
    function UpdateUndergroundSectionMonthlyDataInDB (AModuleID     : Integer;
                                                      ASectionNo    : Integer;
                                                      AMonthList    : TStringList;
                                                      ABPFactorList : TStringList;
                                                      AHEFactorList : TStringList): Boolean;
    function UpdateUndergroundSectionQSLDDataInDB (AModuleID    : Integer;
                                                   ASectionNo   : Integer;
                                                   AStdDev      : Double;
                                                   AFlowRefList : TStringList;
                                                   ALoadList    : TStringList): Boolean;

    function UpdateOpencastPitPropertiesDataInDB (AModuleID                         : Integer;
                                                  ASectionNo                        : Integer;
                                                  ASectionName                      : String;
                                                  ACoalReserveArea                  : Double;
                                                  AWorkingArea                      : Double;
                                                  ACommissionYear                   : Integer;
                                                  ACommissionMonth                  : Integer;
                                                  ADeCommissionYear                 : Integer;
                                                  ADeCommissionMonth                : Integer;
                                                  ADisturbedArea                    : Double;
                                                  ARehabilitatedArea                : Double;
                                                  AEvaporationArea                  : Double;
                                                  ADisturbedAreaRunOffFactor        : Double;
                                                  ADisturbedWorkingAreaRunOffFactor : Double;
                                                  AWashOffParameter                 : Double;
                                                  ASulphateBuildUpRate              : Double;
                                                  AInitialSaltMass                  : Double;
                                                  AInspoilsStorageDecant            : Double;
                                                  AInspoilsStorageSeepage           : Double;
                                                  ASeepageEquationExponent          : Double;
                                                  AMaxSeepageRate                   : Double;
                                                  AInspoilsStorageInitialVolume     : Double;
                                                  AInspoilsDamConcentration         : Double;
                                                  APCDInitialVolume                 : Double;
                                                  APCDCapacity                      : Double;
                                                  APCDFullSurfaceArea               : Double): Boolean;
    function UpdateOpencastPitWorkingsAreaDataInDB (AModuleID          : Integer;
                                                    ASectionNo         : Integer;
                                                    AInterpolationType : Integer;
                                                    AYearList          : TStringList;
                                                    AGrowthList        : TStringList): Boolean;
    function UpdateOpencastPitDisturbedAreaDataInDB (AModuleID          : Integer;
                                                     ASectionNo         : Integer;
                                                     AInterpolationType : Integer;
                                                     AYearList          : TStringList;
                                                     AGrowthList        : TStringList): Boolean;
    function UpdateOpencastPitRehabilitatedAreaDataInDB (AModuleID          : Integer;
                                                         ASectionNo         : Integer;
                                                         AInterpolationType : Integer;
                                                         AYearList          : TStringList;
                                                         AGrowthList        : TStringList): Boolean;
    function UpdateOpencastPitEvaporationAreaDataInDB (AModuleID          : Integer;
                                                       ASectionNo         : Integer;
                                                       AInterpolationType : Integer;
                                                       AYearList          : TStringList;
                                                       AGrowthList        : TStringList): Boolean;
    function UpdateOpencastPitInspoilsDecantDataInDB (AModuleID          : Integer;
                                                      ASectionNo         : Integer;
                                                      AInterpolationType : Integer;
                                                      AYearList          : TStringList;
                                                      AGrowthList        : TStringList): Boolean;
    function UpdateOpencastPitInspoilsSeepageDataInDB (AModuleID          : Integer;
                                                       ASectionNo         : Integer;
                                                       AInterpolationType : Integer;
                                                       AYearList          : TStringList;
                                                       AGrowthList        : TStringList): Boolean;
    function UpdateOpencastPitMonthlyDataInDB (AModuleID     : Integer;
                                               ASectionNo    : Integer;
                                               AMonthList    : TStringList;
                                               ADFactorList  : TStringList;
                                               ADWFactorList : TStringList): Boolean;
    function UpdateOpencastPitWorkingsQSLDDataInDB (AModuleID    : Integer;
                                                    ASectionNo   : Integer;
                                                    AStdDev      : Double;
                                                    AFlowRefList : TStringList;
                                                    ALoadList    : TStringList): Boolean;
    function UpdateOpencastPitSeepDecantQSLDDataInDB (AModuleID    : Integer;
                                                      ASectionNo   : Integer;
                                                      AStdDev      : Double;
                                                      AFlowRefList : TStringList;
                                                      ALoadList    : TStringList): Boolean;

    function UpdateMinePropertiesDataInDB (AModuleID              : Integer;
                                           AActive                : String;
                                           ALatitude              : Double;
                                           ALongitude             : Double;
                                           AMineName              : String;
                                           AVersionNo             : Integer;
                                           AMAP                   : Double;
                                           ARainfallFileName      : String;
                                           ARunOffModuleNo        : Integer;
                                           AOutflowRouteNoToRiver : Integer;
                                           AOutflowRouteToPCD     : Integer;
                                           APlantArea             : Double;
                                           APlantAreaRunOffFactor : Double;
                                           ASaltBuildUpRate       : Double;
                                           ASaltWashOffFactor     : Double;
                                           AInitialSaltStore      : Double): Boolean;
    function UpdateMinePlantAreaDataInDB (AModuleID          : Integer;
                                          AInterpolationType : Integer;
                                          AYearList          : TStringList;
                                          AGrowthList        : TStringList): Boolean;
  end;

var
  GMineDBManager : TMineDBManager;

implementation


uses
  SysUtils,
  Windows,
  DB,

  UFileReader,
  UHydroDBAgent,
  UErrorHandlingOperations;

(* TMineDBManager *************************************************************)

function TMineDBManager.DeleteGrowthDataFromDB (AModuleID : Integer; ASectionNo : Integer = 0; AParameter : String = '') : Boolean;
const OPNAME = 'TMineDBManager.DeleteGrowthDataFromDB';
var
  LSQL : String;
begin
  Result := FALSE;
  try
    LSQL := 'DELETE FROM MineModulesGrowthData WHERE ModuleID = ' + IntToStr(AModuleID);
    if (ASectionNo <> 0) then
      LSQL := LSQL + ' AND SectionNo = ' + IntToStr(ASectionNo);
    if (AParameter <> '') then
      LSQL := LSQL + ' AND Parameter = ' + QuotedStr(AParameter);
    Result := GHydroDBAgent.ExecuteSQL(LSQL, True);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMineDBManager.DeleteQvsSLDDataFromDB (AModuleID : Integer; ASectionNo : Integer = 0; AParameter : String = '') : Boolean;
const OPNAME = 'TMineDBManager.DeleteQvsSLDDataFromDB';
var
  LSQL : String;
begin
  Result := FALSE;
  try
    LSQL := 'DELETE FROM MineQvsSLDData WHERE ModuleID = ' + IntToStr(AModuleID);
    if (ASectionNo <> 0) then
      LSQL := LSQL + ' AND SectionNo = ' + IntToStr(ASectionNo);
    if (AParameter <> '') then
      LSQL := LSQL + ' AND Parameter = ' + QuotedStr(AParameter);
    Result := GHydroDBAgent.ExecuteSQL(LSQL, True);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMineDBManager.InsertGrowthDataIntoDB (AModuleID, ASectionNo : Integer; AParameter, AYear, AGrowth : String) : Boolean;
const OPNAME = 'TMineDBManager.InsertGrowthDataIntoDB';
var
  LSQL : String;
begin
  Result := FALSE;
  try
    LSQL := 'INSERT INTO MineModulesGrowthData VALUES (' +
            IntToStr(AModuleID) + ', ' + QuotedStr(AParameter) + ', ' + IntToStr(ASectionNo) + ', ' + AYear + ', ' + AGrowth + ')';
    Result := GHydroDBAgent.ExecuteSQL(LSQL, True);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMineDBManager.InsertQvsSLDDataIntoDB (AModuleID, ASectionNo : Integer; AParameter, AIndex, AFlowRef, ALoad : String) : Boolean;
const OPNAME = 'TMineDBManager.InsertQvsSLDDataIntoDB';
var
  LSQL : String;
begin
  Result := FALSE;
  try
    LSQL := 'INSERT INTO MineQvsSLDData VALUES (' +
            IntToStr(AModuleID) + ', ' + QuotedStr(AParameter) + ', ' + IntToStr(ASectionNo) + ', ' +
            AIndex + ', ' + AFlowRef + ', ' + ALoad + ')';
    Result := GHydroDBAgent.ExecuteSQL(LSQL, True);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{ SlurryDump ******************************************************************}

function TMineDBManager.UpdateSlurryDumpPropertiesDataInDB (AModuleID                  : Integer;
                                                            ASectionNo                 : Integer;
                                                            ASectionName               : String;
                                                            ASlurryDumpArea            : Double;
                                                            ASlurryDumpRunOffFactor    : Double;
                                                            ASlurrySeepProportion      : Double;
                                                            ASlurryPCDFullSupplyVolume : Double;
                                                            ASlurryPCDFullSupplyArea   : Double;
                                                            ASlurryPCDInitialVolume    : Double): Boolean;
const OPNAME = 'TMineDBManager.UpdateSlurryDumpPropertiesDataInDB';
var
  LSQL : String;
begin
  Result := FALSE;
  try
    LSQL := 'UPDATE MineSlurryData SET' +
            ' SlurryDumpArea = '                 + FloatToStr(ASlurryDumpArea) +
            ', SlurryDumpRunOffFactor = '        + FloatToStr(ASlurryDumpRunOffFactor) +
            ', SlurrySeepProportion = '          + FloatToStr(ASlurrySeepProportion) +
            ', SlurryControlDamVolume = '        + FloatToStr(ASlurryPCDFullSupplyVolume) +
            ', SlurryControlDamSurfaceArea = '   + FloatToStr(ASlurryPCDFullSupplyArea) +
            ', SlurryControlDamInitialVolume = ' + FloatToStr(ASlurryPCDInitialVolume);
    if (ASectionName = '') then
      LSQL := LSQL + ', DiscardSlurryName = NULL'
    else
      LSQL := LSQL + ', DiscardSlurryName = ' + QuotedStr(ASectionName);
    LSQL := LSQL + ' WHERE ModuleID = ' + IntToStr(AModuleID) + ' AND SectionNo = ' + IntToStr(ASectionNo);

    Result := GHydroDBAgent.ExecuteSQL(LSQL, True);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMineDBManager.UpdateSlurryDumpGrowthDataInDB (AModuleID          : Integer;
                                                        ASectionNo         : Integer;
                                                        AInterpolationType : Integer;
                                                        AYearList          : TStringList;
                                                        AGrowthList        : TStringList) : Boolean;
const OPNAME = 'TMineDBManager.UpdateSlurryDumpGrowthDataInDB';
var
  LSQL    : String;
  LIndex  : Integer;
begin
  Result := FALSE;
  try
    GHydroDBAgent.StartTransaction;
    try
      LSQL := 'UPDATE MineSlurryData SET SlurryDumpInterpolationOption = ' + IntToStr(AInterpolationType) +
              ' WHERE ModuleID = ' + IntToStr(AModuleID) +
              ' AND SectionNo = ' + IntToStr(ASectionNo);
      Result := GHydroDBAgent.ExecuteSQL(LSQL, True);

      if (Result) then
        Result := DeleteGrowthDataFromDB(AModuleID, ASectionNo, 'SlurryDump');

      LIndex := 0;
      while (Result AND (LIndex < AYearList.Count)) do
      begin
        Result := GMineDBManager.InsertGrowthDataIntoDB
                                   (AModuleID, ASectionNo, 'SlurryDump', AYearList.Strings[LIndex], AGrowthList.Strings[LIndex]);
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

function TMineDBManager.UpdateSlurryDumpMonthlyDataInDB (AModuleID   : Integer;
                                                         ASectionNo  : Integer;
                                                         AMonthList  : TStringList;
                                                         AFactorList : TStringList): Boolean;
const OPNAME = 'TMineDBManager.UpdateSlurryDumpMonthlyDataInDB';
var
  LIndex  : Integer;
  LSQL    : String;
begin
  Result := FALSE;
  try
    GHydroDBAgent.StartTransaction;
    try
      Result := TRUE;

      LIndex := 0;
      while (Result AND (LIndex < AMonthList.Count)) do
      begin
        LSQL := 'UPDATE MineSlurryMonthlyData SET SlurryRecharge = ' + AFactorList.Strings[LIndex] +
                ' WHERE ModuleID = ' + IntToStr(AModuleID) + ' AND SectionNo = ' + IntToStr(ASectionNo) +
                ' AND Month = ' + AMonthList.Strings[LIndex];
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

function TMineDBManager.UpdateSlurryDumpQSLDDataInDB (AModuleID    : Integer;
                                                      ASectionNo   : Integer;
                                                      AStdDev      : Double;
                                                      AFlowRefList : TStringList;
                                                      ALoadList    : TStringList): Boolean;
const OPNAME = 'TMineDBManager.UpdateSlurryDumpQSLDDataInDB';
var
  LSQL     : String;
  LIndex   : Integer;
begin
  Result := FALSE;
  try
    GHydroDBAgent.StartTransaction;
    try
      LSQL := 'UPDATE MineSlurryData SET SlurryStdDeviation = ' + FloatToStr(AStdDev) +
              ' WHERE ModuleID = ' + IntToStr(AModuleID) + ' AND SectionNo = ' + IntToStr(ASectionNo);
      Result := GHydroDBAgent.ExecuteSQL(LSQL, True);

      if (Result) then
        Result := DeleteQvsSLDDataFromDB(AModuleID, ASectionNo, 'Slurry');

      LIndex := 0;
      while (Result AND (LIndex < AFlowRefList.Count)) do
      begin
        Result := InsertQvsSLDDataIntoDB(AModuleID, ASectionNo, 'Slurry', IntToStr(LIndex+1),
                                         AFlowRefList.Strings[LIndex], ALoadList.Strings[LIndex]);
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

function TMineDBManager.LoadGrowthDataFromDB (ASlurryDump : TSlurryDump) : Boolean;
const OPNAME = 'TMineDBManager.LoadGrowthDataFromDB';
var
  LQuery      : TDataSet;
  LSQL        : String;
begin
  Result := FALSE;
  try
    ASlurryDump.ClearAreaGrowthData;
    // Load SlurryDump growth data
    LSQL := 'SELECT * FROM MineModulesGrowthData WHERE Parameter = ' + QuotedStr('SlurryDump') +
            ' AND ModuleID = ' + IntToStr(ASlurryDump.ModuleID) + ' AND SectionNo = ' + IntToStr(ASlurryDump.SectionNo);
    LQuery := GHydroDBAgent.CreateQuery(LSQL);
    try
      LQuery.Open;
      while (NOT LQuery.Eof) do
      begin
        ASlurryDump.AddAreaGrowthData(LQuery.FieldByName('Year').AsInteger,
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

function TMineDBManager.LoadMonthlyDataFromDB (ASlurryDump : TSlurryDump) : Boolean;
const OPNAME = 'TMineDBManager.LoadMonthlyDataFromDB';
var
  LQuery      : TDataSet;
  LSQL        : String;
  LMonth      : Integer;
begin
  Result := FALSE;
  try
    LSQL := 'SELECT * FROM MineSlurryMonthlyData WHERE ModuleID = ' + IntToStr(ASlurryDump.ModuleID) +
            ' AND SectionNo = ' + IntToStr(ASlurryDump.SectionNo) + ' ORDER BY Month';
    LQuery := GHydroDBAgent.CreateQuery(LSQL);
    try
      LQuery.Open;
      while (NOT LQuery.Eof) do
      begin
        LMonth := LQuery.FieldByName('Month').AsInteger;
        ASlurryDump.RechargeFactorByMonth[LMonth] := LQuery.FieldByName('SlurryRecharge').AsFloat;
        LQuery.Next;
      end;
    finally
      LQuery.Close;
      LQuery.Free;
    end;
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMineDBManager.LoadQvsSLDDataFromDB (ASlurryDump : TSlurryDump) : Boolean;
const OPNAME = 'TMineDBManager.LoadQvsSLDDataFromDB';
var
  LQuery      : TDataSet;
  LSQL        : String;
begin
  Result := FALSE;
  try
    ASlurryDump.ClearQvsSLDData;
    LSQL := 'SELECT * FROM MineQvsSLDData WHERE ModuleID = ' + IntToStr(ASlurryDump.ModuleID) +
            ' AND Parameter = ' + QuotedStr('Slurry') +
            ' AND SectionNo = ' + IntToStr(ASlurryDump.SectionNo) + ' ORDER BY Index';
    LQuery := GHydroDBAgent.CreateQuery(LSQL);
    try
      LQuery.Open;
      while (NOT LQuery.Eof) do
      begin
        ASlurryDump.AddLoadFlowRefPair(LQuery.FieldByName('Load').AsFloat,
                                       LQuery.FieldByName('FlowRef').AsFloat);
        LQuery.Next;
      end;
    finally
      LQuery.Close;
      LQuery.Free;
    end;
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMineDBManager.DeleteSlurryDumpMonthlyDataFromDB (AModuleID : Integer; ASectionNo : Integer = 0) : Boolean;
const OPNAME = 'TMineDBManager.DeleteSlurryDumpMonthlyDataFromDB';
var
  LSQL : String;
begin
  Result := FALSE;
  try
    LSQL := 'DELETE * FROM MineSlurryMonthlyData WHERE ModuleID = ' + IntToStr(AModuleID);
    if (ASectionNo <> 0) then
      LSQL := LSQL + ' AND SectionNo = ' + IntToStr(ASectionNo);
    Result := GHydroDBAgent.ExecuteSQL(LSQL, True);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMineDBManager.DeleteSlurryDumpFromDB (AModuleID : Integer; ASectionNo : Integer = 0) : Boolean;
const OPNAME = 'TMineDBManager.DeleteSlurryDumpFromDB';
var
  LSQL : String;
begin
  Result := FALSE;
  try
    LSQL := 'DELETE * FROM MineSlurryData WHERE ModuleID = ' + IntToStr(AModuleID);
    if (ASectionNo <> 0) then
      LSQL := LSQL + ' AND SectionNo = ' + IntToStr(ASectionNo);
    Result := GHydroDBAgent.ExecuteSQL(LSQL, True);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMineDBManager.LoadSlurryDumpDataFromDB (AModuleID: Integer): Boolean;
const OPNAME = 'TMineDBManager.LoadSlurryDumpDataFromDB';
var
  LQuery      : TDataSet;
  LSQL        : String;
  LMineModule : TMineModule;
  LSlurryDump : TSlurryDump;
  LSectionNo  : Integer;
begin
  Result := FALSE;
  try
    LMineModule := TMineModuleAgent(ModuleAgent).FindMineModuleByID(AModuleID);
    LSQL := 'SELECT * FROM MineSlurryData WHERE ModuleID = ' + IntToStr(AModuleID);
    LQuery := GHydroDBAgent.CreateQuery(LSQL);
    try
      LQuery.Open;
      while (NOT LQuery.Eof) do
      begin
        LSlurryDump := LMineModule.AddSlurryDump;
        LSectionNo   := LQuery.FieldByName('SectionNo').AsInteger;
        LSlurryDump.Populate(AModuleID,
                             LSectionNo,
                             Trim(LQuery.FieldByName('DiscardSlurryName').AsString),
                             LQuery.FieldByName('SlurryDumpArea').AsFloat,
                             LQuery.FieldByName('SlurryDumpRunOffFactor').AsFloat,
                             LQuery.FieldByName('SlurrySeepProportion').AsFloat,
                             LQuery.FieldByName('SlurryControlDamVolume').AsFloat,
                             LQuery.FieldByName('SlurryControlDamSurfaceArea').AsFloat,
                             LQuery.FieldByName('SlurryControlDamInitialVolume').AsFloat,
                             LQuery.FieldByName('SlurryDumpInterpolationOption').AsInteger,
                             LQuery.FieldByName('SlurryStdDeviation').AsFloat);
        GMineDBManager.LoadGrowthDataFromDB(LSlurryDump);
        GMineDBManager.LoadMonthlyDataFromDB(LSlurryDump);
        GMineDBManager.LoadQvsSLDDataFromDB(LSlurryDump);
        LQuery.Next;
      end;
    finally
      LQuery.Close;
      LQuery.Free;
    end;
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMineDBManager.GetNextSlurryDumpSectionNo (AModuleID : Integer) : Integer;
const OPNAME = 'TMineDBManager.GetNextSlurryDumpSectionNo';
var
  LSQL    : String;
  LQuery  : TDataSet;
begin
  Result := 0;
  try
    LSQL := 'SELECT MAX(SectionNo) AS LastSectionNo FROM MineSlurryData WHERE ModuleID = ' + IntToStr(AModuleID);
    LQuery := GHydroDBAgent.CreateQuery(LSQL);
    try
      LQuery.Open;
      if (NOT LQuery.Eof) then
        Result := LQuery.FieldByName('LastSectionNo').AsInteger + 1
      else
        Result := 1;
    finally
      LQuery.Close;
      LQuery.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMineDBManager.InsertSlurryDumpIntoDB (AModuleID, ASectionNo : Integer) : Boolean;
const OPNAME = 'TMineDBManager.InsertSlurryDumpIntoDB';
var
  LSQL    : String;
begin
  Result := FALSE;
  try
    LSQL := 'INSERT INTO MineSlurryData (ModuleID, SectionNo, DiscardSlurryName) VALUES (' +
            IntToStr(AModuleID) + ', ' + IntToStr(ASectionNo) + ', ' +
            QuotedStr('Slurry ' + IntToStr(ASectionNo)) + ')';
    Result := GHydroDBAgent.ExecuteSQL(LSQL, True);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMineDBManager.InsertSlurryDumpMonthlyDataIntoDB (AModuleID, ASectionNo : Integer) : Boolean;
const OPNAME = 'TMineDBManager.InsertSlurryDumpMonthlyDataIntoDB';
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
      LSQL := 'INSERT INTO MineSlurryMonthlyData VALUES (' +
              IntToStr(AModuleID) + ', ' + IntToStr(ASectionNo) + ', ' + IntToStr(LMonth) +  ',0)';
      LResult := LResult AND GHydroDBAgent.ExecuteSQL(LSQL, True);
    end;
    Result := LResult;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMineDBManager.CreateNewSlurryDumpInDB (AMineModule : TMineModule) : TSlurryDump;
const OPNAME = 'TMineDBManager.CreateNewSlurryDumpInDB';
var
  LSectionNo   : Integer;
  LSlurryDump  : TSlurryDump;
begin
  Result := nil;
  try
    LSectionNo := GetNextSlurryDumpSectionNo(AMineModule.ModuleID);

    GHydroDBAgent.StartTransaction;
    try
      if (InsertSlurryDumpIntoDB(AMineModule.ModuleID, LSectionNo)) AND
         (InsertSlurryDumpMonthlyDataIntoDB(AMineModule.ModuleID, LSectionNo)) then
      begin
        GHydroDBAgent.CommitTransaction;
        LSlurryDump := AMineModule.AddSlurryDump;
        LSlurryDump.Populate(AMineModule.ModuleID, LSectionNo, 'Slurry ' + IntToStr(LSectionNo),
                              0,0,0,0,0,0,0,0);
        GMineDBManager.LoadGrowthDataFromDB(LSlurryDump);
        GMineDBManager.LoadMonthlyDataFromDB(LSlurryDump);
        GMineDBManager.LoadQvsSLDDataFromDB(LSlurryDump);
        Result := LSlurryDump;
      end
      else
        GHydroDBAgent.RollbackTransaction;
    except
      GHydroDBAgent.RollbackTransaction;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TMineDBManager.RemoveSlurryDumpFromDB (AModuleID, ASectionNo : Integer) : Boolean;
const OPNAME = 'TMineDBManager.RemoveSlurryDumpFromDB';
begin
  Result := FALSE;
  try
    GHydroDBAgent.StartTransaction;
    try
      if (DeleteGrowthDataFromDB(AModuleID, ASectionNo, 'SlurryDump')) AND
         (DeleteSlurryDumpMonthlyDataFromDB(AModuleID, ASectionNo))    AND
         (DeleteQvsSLDDataFromDB(AModuleID, ASectionNo, 'Slurry'))     AND
         (DeleteSlurryDumpFromDB(AModuleID, ASectionNo)) then
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

{ Underground Section *********************************************************}

function TMineDBManager.UpdateUndergroundSectionPropertiesDataInDB (AModuleID                  : Integer;
                                                                    ASectionNo                 : Integer;
                                                                    ASectionName               : String;
                                                                    AUndergroundOutflowRouteNo : Integer;
                                                                    AUpstreamCatchmentArea     : Double;
                                                                    ABoardAndPillarArea        : Double;
                                                                    AHighExtractionArea        : Double;
                                                                    ASurfaceRunOffFactor       : Double): Boolean;
const OPNAME = 'TMineDBManager.UpdateUndergroundSectionPropertiesDataInDB';
var
  LSQL : String;
begin
  Result := FALSE;
  try
    LSQL := 'UPDATE MineUndergroundData SET' +
            ' UndergroundOutflowRouteNo = ' + IntToStr(AUndergroundOutflowRouteNo) +
            ', UpstreamCatchmentArea = '    + FloatToStr(AUpstreamCatchmentArea) +
            ', BoardAndPillarArea = '       + FloatToStr(ABoardAndPillarArea) +
            ', HighExtractionArea = '       + FloatToStr(AHighExtractionArea) +
            ', SurfaceRunOffFactor = '      + FloatToStr(ASurfaceRunOffFactor);
    if (ASectionName = '') then
      LSQL := LSQL + ', UndergroundSectionName = NULL'
    else
      LSQL := LSQL + ', UndergroundSectionName = ' + QuotedStr(ASectionName);

    LSQL := LSQL + ' WHERE ModuleID = ' + IntToStr(AModuleID) + ' AND SectionNo = ' + IntToStr(ASectionNo);
    Result := GHydroDBAgent.ExecuteSQL(LSQL, True);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMineDBManager.UpdateUndergroundSectionBoardPillarDataInDB (AModuleID          : Integer;
                                                                     ASectionNo         : Integer;
                                                                     AInterpolationType : Integer;
                                                                     AYearList          : TStringList;
                                                                     AGrowthList        : TStringList): Boolean;
const OPNAME = 'TMineDBManager.UpdateUndergroundSectionBoardPillarDataInDB';
var
  LSQL   : String;
  LIndex : Integer;
begin
  Result := FALSE;
  try
    GHydroDBAgent.StartTransaction;
    try
      LSQL := 'UPDATE MineUndergroundData SET BoardAndPillarInterpolationOption = ' + IntToStr(AInterpolationType) +
              ' WHERE ModuleID = ' + IntToStr(AModuleID) + ' AND SectionNo = ' + IntToStr(ASectionNo);
      Result := GHydroDBAgent.ExecuteSQL(LSQL, True);

      if (Result) then
        Result := DeleteGrowthDataFromDB(AModuleID, ASectionNo, 'BoardPillar');

      LIndex := 0;
      while (Result AND (LIndex < AYearList.Count)) do
      begin
        Result := InsertGrowthDataIntoDB(AModuleID, ASectionNo, 'BoardPillar',
                                         AYearList.Strings[LIndex], AGrowthList.Strings[LIndex]);
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

function TMineDBManager.UpdateUndergroundSectionHighExtractionDataInDB (AModuleID          : Integer;
                                                                        ASectionNo         : Integer;
                                                                        AInterpolationType : Integer;
                                                                        AYearList          : TStringList;
                                                                        AGrowthList        : TStringList): Boolean;
const OPNAME = 'TMineDBManager.UpdateUndergroundSectionHighExtractionDataInDB';
var
  LSQL    : String;
  LIndex  : Integer;
begin
  Result := FALSE;
  try
    GHydroDBAgent.StartTransaction;
    try
      LSQL := 'UPDATE MineUndergroundData SET HighExtractionInterpolationOption = ' + IntToStr(AInterpolationType) +
              ' WHERE ModuleID = ' + IntToStr(AModuleID) +
              ' AND SectionNo = ' + IntToStr(ASectionNo);
      Result := GHydroDBAgent.ExecuteSQL(LSQL, True);

      if (Result) then
        Result := DeleteGrowthDataFromDB(AModuleID, ASectionNo, 'HighExtraction');

      LIndex := 0;
      while (Result AND (LIndex < AYearList.Count)) do
      begin
        Result := InsertGrowthDataIntoDB(AModuleID, ASectionNo, 'HighExtraction',
                                         AYearList.Strings[LIndex], AGrowthList.Strings[LIndex]);
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

function TMineDBManager.UpdateUndergroundSectionMonthlyDataInDB (AModuleID     : Integer;
                                                                 ASectionNo    : Integer;
                                                                 AMonthList    : TStringList;
                                                                 ABPFactorList : TStringList;
                                                                 AHEFactorList : TStringList): Boolean;
const OPNAME = 'TMineDBManager.UpdateUndergroundSectionMonthlyDataInDB';
var
  LSQL   : String;
  LIndex : Integer;
begin
  Result := FALSE;
  try
    Result := TRUE;

    GHydroDBAgent.StartTransaction;
    try
      LIndex := 0;
      while (Result AND (LIndex < AMonthList.Count)) do
      begin
        LSQL := 'UPDATE MineUndergroundMonthlyData SET' +
                ' BoardAndPillarRecharge = ' + ABPFactorList.Strings[LIndex] +
                ', HighExtractionRecharge = ' + AHEFactorList.Strings[LIndex] +
                ' WHERE ModuleID = ' + IntToStr(AModuleID) + ' AND SectionNo = ' + IntToStr(ASectionNo) +
                ' AND Month = ' + AMonthList.Strings[LIndex];
        Result := GHydroDBAgent.ExecuteSQL(LSQL, True);
        LIndex := LIndex + 1;
      end;
      if (Result) then
       GHydroDBAgent.CommitTransaction
     else
       GHydroDBAgent.RollbackTransaction;  
    except
      GHydroDBAgent.RollbackTransaction
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMineDBManager.UpdateUndergroundSectionQSLDDataInDB (AModuleID    : Integer;
                                                              ASectionNo   : Integer;
                                                              AStdDev      : Double;
                                                              AFlowRefList : TStringList;
                                                              ALoadList    : TStringList): Boolean;
const OPNAME = 'TMineDBManager.UpdateUndergroundSectionQSLDDataInDB';
var
  LSQL     : String;
  LIndex   : Integer;
begin
  Result := FALSE;
  try
    GHydroDBAgent.StartTransaction;
    try
      LSQL := 'UPDATE MineUndergroundData SET UndergroundStdDeviation = ' + FloatToStr(AStdDev) +
              ' WHERE ModuleID = ' + IntToStr(AModuleID) +
              ' AND SectionNo = ' + IntToStr(ASectionNo);
      Result := GHydroDBAgent.ExecuteSQL(LSQL, True);

      if (Result) then
        Result := DeleteQvsSLDDataFromDB(AModuleID, ASectionNo, 'Underground');

      LIndex := 0;
      while (Result AND (LIndex < AFlowRefList.Count)) do
      begin
        Result := InsertQvsSLDDataIntoDB(AModuleID, ASectionNo, 'Underground', IntToStr(LIndex+1),
                                         AFlowRefList.Strings[LIndex], ALoadList.Strings[Lindex]);
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

function TMineDBManager.LoadBoardPillarGrowthDataFromDB (AUndergroundSection : TUndergroundSection) : Boolean;
const OPNAME = 'TMineDBManager.LoadBoardPillarGrowthDataFromDB';
var
  LQuery : TDataSet;
  LSQL   : String;
begin
  Result := FALSE;
  try
    AUndergroundSection.ClearBoardAndPillarGrowthData;
    // Load BoardPillar growth data
    LSQL := 'SELECT * FROM MineModulesGrowthData WHERE Parameter = ' + QuotedStr('BoardPillar') +
            ' AND ModuleID = ' + IntToStr(AUndergroundSection.ModuleID) +
            ' AND SectionNo = ' + IntToStr(AUndergroundSection.SectionNo);
    LQuery := GHydroDBAgent.CreateQuery(LSQL);
    try
      LQuery.Open;
      while (NOT LQuery.Eof) do
      begin
        AUndergroundSection.AddBoardAndPillarGrowthData(LQuery.FieldByName('Year').AsInteger,
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

function TMineDBManager.LoadHighExtractionGrowthDataFromDB (AUndergroundSection : TUndergroundSection) : Boolean;
const OPNAME = 'TMineDBManager.LoadHighExtractionGrowthDataFromDB';
var
  LQuery : TDataSet;
  LSQL   : String;
begin
  Result := FALSE;
  try
    AUndergroundSection.ClearHighExtractionGrowthData;

    // Load HighExtraction growth data
    LSQL := 'SELECT * FROM MineModulesGrowthData WHERE Parameter = ' + QuotedStr('HighExtraction') +
            ' AND ModuleID = ' + IntToStr(AUndergroundSection.ModuleID) +
            ' AND SectionNo = ' + IntToStr(AUndergroundSection.SectionNo);
    LQuery := GHydroDBAgent.CreateQuery(LSQL);
    try
      LQuery.Open;
      while (NOT LQuery.Eof) do
      begin
        AUndergroundSection.AddHighExtractionGrowthData(LQuery.FieldByName('Year').AsInteger,
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

function TMineDBManager.LoadQvsSLDDataFromDB (AUndergroundSection : TUndergroundSection) : Boolean;
const OPNAME = 'TMineDBManager.LoadQvsSLDDataFromDB';
var
  LQuery  : TDataSet;
  LSQL    : String;
begin
  Result := FALSE;
  try
    AUndergroundSection.ClearQvsSLDData;

    LSQL := 'SELECT * FROM MineQvsSLDData WHERE ModuleID = ' + IntToStr(AUndergroundSection.ModuleID) +
            ' AND Parameter = ' + QuotedStr('Underground') +
            ' AND SectionNo = ' + IntToStr(AUndergroundSection.SectionNo) + ' ORDER BY Index';
    LQuery := GHydroDBAgent.CreateQuery(LSQL);
    try
      LQuery.Open;
      while (NOT LQuery.Eof) do
      begin
        AUndergroundSection.AddLoadFlowRefPair(LQuery.FieldByName('Load').AsFloat,
                                               LQuery.FieldByName('FlowRef').AsFloat);
        LQuery.Next;
      end;
    finally
      LQuery.Close;
      LQuery.Free;
    end;
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMineDBManager.LoadMonthlyDataFromDB (AUndergroundSection : TUndergroundSection) : Boolean;
const OPNAME = 'TMineDBManager.LoadMonthlyDataFromDB';
var
  LQuery  : TDataSet;
  LSQL    : String;
  LMonth  : Integer;
begin
  Result := FALSE;
  try
    LSQL := 'SELECT * FROM MineUndergroundMonthlyData WHERE ModuleID = ' + IntToStr(AUndergroundSection.ModuleID) +
            ' AND SectionNo = ' + IntToStr(AUndergroundSection.SectionNo) + ' ORDER BY Month';
    LQuery := GHydroDBAgent.CreateQuery(LSQL);
    try
      LQuery.Open;
      while (NOT LQuery.Eof) do
      begin
        LMonth := LQuery.FieldByName('Month').AsInteger;
        AUndergroundSection.UndergroundWaterRechargePortionByMonth[LMonth] := LQuery.FieldByName('UndergroundWaterRecharge').AsFloat;
        AUndergroundSection.BoardAndPillarRechargeFactorByMonth[LMonth]    := LQuery.FieldByName('BoardAndPillarRecharge').AsFloat;
        AUndergroundSection.HighExtractionRechargeFactorByMonth[LMonth]    := LQuery.FieldByName('HighExtractionRecharge').AsFloat;
        LQuery.Next;
      end;
    finally
      LQuery.Close;
      LQuery.Free;
    end;
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMineDBManager.DeleteUndergroundSectionMonthlyDataFromDB (AModuleID : Integer; ASectionNo : Integer = 0) : Boolean;
const OPNAME = 'TMineDBManager.DeleteUndergroundSectionMonthlyDataFromDB';
var
  LSQL : String;
begin
  Result := FALSE;
  try
    LSQL := 'DELETE * FROM MineUndergroundMonthlyData WHERE ModuleID = ' + IntToStr(AModuleID);
    if (ASectionNo <> 0) then
      LSQL := LSQL + ' AND SectionNo = ' + IntToStr(ASectionNo);
    Result := GHydroDBAgent.ExecuteSQL(LSQL, True);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMineDBManager.DeleteUndergroundSectionFromDB (AModuleID : Integer; ASectionNo : Integer = 0) : Boolean;
const OPNAME = 'TMineDBManager.DeleteUndergroundSectionFromDB';
var
  LSQL : String;
begin
  Result := FALSE;
  try
    LSQL := 'DELETE * FROM MineUndergroundData WHERE ModuleID = ' + IntToStr(AModuleID);
    if (ASectionNo <> 0) then
      LSQL := LSQL + ' AND SectionNo = ' + IntToStr(ASectionNo);
    Result := GHydroDBAgent.ExecuteSQL(LSQL, True);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMineDBManager.LoadUndergroundSectionDataFromDB (AModuleID: Integer): Boolean;
const OPNAME = 'TMineDBManager.LoadUndergroundSectionDataFromDB';
var
  LQuery              : TDataSet;
  LSQL                : String;
  LMineModule         : TMineModule;
  LUndergroundSection : TUndergroundSection;
  LSectionNo          : Integer;
begin
  Result := FALSE;
  try
    LMineModule := TMineModuleAgent(ModuleAgent).FindMineModuleByID(AModuleID);
    LSQL := 'SELECT * FROM MineUndergroundData WHERE ModuleID = ' + IntToStr(AModuleID);
    LQuery := GHydroDBAgent.CreateQuery(LSQL);
    try
      LQuery.Open;
      while (NOT LQuery.Eof) do
      begin
        LUndergroundSection := LMineModule.AddUndergroundSection;
        LSectionNo   := LQuery.FieldByName('SectionNo').AsInteger;
        LUndergroundSection.Populate(AModuleID,
                                     LSectionNo,
                                     Trim(LQuery.FieldByName('UndergroundSectionName').AsString),
                                     LQuery.FieldByName('UndergroundOutflowRouteNo').AsInteger,
                                     LQuery.FieldByName('UpstreamCatchmentArea').AsFloat,
                                     LQuery.FieldByName('BoardAndPillarArea').AsFloat,
                                     LQuery.FieldByName('HighExtractionArea').AsFloat,
                                     LQuery.FieldByName('SurfaceRunOffFactor').AsFloat,
                                     LQuery.FieldByName('BoardAndPillarInterpolationOption').AsInteger,
                                     LQuery.FieldByName('HighExtractionInterpolationOption').AsInteger,
                                     LQuery.FieldByName('UndergroundStdDeviation').AsFloat);
        LoadBoardPillarGrowthDataFromDB(LUndergroundSection);
        LoadHighExtractionGrowthDataFromDB(LUndergroundSection);
        LoadMonthlyDataFromDB(LUndergroundSection);
        LoadQvsSLDDataFromDB(LUndergroundSection);
        LQuery.Next;
      end;
    finally
      LQuery.Close;
      LQuery.Free;
    end;
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMineDBManager.GetNextUndergroundSectionNo (AModuleID : Integer) : Integer;
const OPNAME = 'TMineDBManager.GetNextUndergroundSectionNo';
var
  LSQL    : String;
  LQuery  : TDataSet;
begin
  Result := 0;
  try
    LSQL := 'SELECT MAX(SectionNo) AS LastSectionNo FROM MineUndergroundData WHERE ModuleID = ' + IntToStr(AModuleID);
    LQuery := GHydroDBAgent.CreateQuery(LSQL);
    try
      LQuery.Open;
      if (NOT LQuery.Eof) then
        Result := LQuery.FieldByName('LastSectionNo').AsInteger + 1
      else
        Result := 1;
    finally
      LQuery.Close;
      LQuery.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMineDBManager.InsertUndergroundSectionIntoDB (AModuleID, ASectionNo : Integer) : Boolean;
const OPNAME = 'TMineDBManager.InsertUndergroundSectionIntoDB';
var
  LSQL    : String;
begin
  Result := FALSE;
  try
    LSQL := 'INSERT INTO MineUndergroundData (ModuleID, SectionNo, UndergroundSectionName) VALUES (' +
            IntToStr(AModuleID) + ', ' + IntToStr(ASectionNo) + ', ' +
            QuotedStr('U/G section ' + IntToStr(ASectionNo)) + ')';
    Result := GHydroDBAgent.ExecuteSQL(LSQL, True);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMineDBManager.InsertUndergroundSectionMonthlyDataIntoDB (AModuleID, ASectionNo : Integer) : Boolean;
const OPNAME = 'TMineDBManager.InsertUndergroundSectionMonthlyDataIntoDB';
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
      LSQL := 'INSERT INTO MineUndergroundMonthlyData VALUES (' +
              IntToStr(AModuleID) + ', ' + IntToStr(ASectionNo) + ', ' + IntToStr(LMonth) +  ',0,0,0)';
      LResult := LResult AND GHydroDBAgent.ExecuteSQL(LSQL, True);
    end;
    Result := LResult;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMineDBManager.CreateNewUndergroundSectionInDB (AMineModule : TMineModule) : TUndergroundSection;
const OPNAME = 'TMineDBManager.CreateNewUndergroundSectionInDB';
var
  LSectionNo   : Integer;
  LUnderground : TUndergroundSection;
begin
  Result := nil;
  try
    LSectionNo := GetNextUndergroundSectionNo(AMineModule.ModuleID);

    GHydroDBAgent.StartTransaction;
    try
      if (InsertUndergroundSectionIntoDB(AMineModule.ModuleID, LSectionNo)) AND
         (InsertUndergroundSectionMonthlyDataIntoDB(AMineModule.ModuleID, LSectionNo)) then
      begin
        GHydroDBAgent.CommitTransaction;
        LUnderground := AMineModule.AddUndergroundSection;
        LUnderground.Populate(AMineModule.ModuleID, LSectionNo, 'U/G section ' + IntToStr(LSectionNo),
                              0,0,0,0,0,0,0,0);
        GMineDBManager.LoadBoardPillarGrowthDataFromDB(LUnderground);
        GMineDBManager.LoadHighExtractionGrowthDataFromDB(LUnderground);
        GMineDBManager.LoadQvsSLDDataFromDB(LUnderground);
        GMineDBManager.LoadMonthlyDataFromDB(LUnderground);
        Result := LUnderground;
      end
      else
        GHydroDBAgent.RollbackTransaction;
    except
      GHydroDBAgent.RollbackTransaction;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TMineDBMAnager.RemoveUndergroundSectionFromDB (AModuleID, ASectionNo : Integer) : Boolean;
const OPNAME = 'TMineModule.RemoveUndergroundSection';
begin
  Result := FALSE;
  try
    GHydroDBAgent.StartTransaction;
    try
      if (DeleteGrowthDataFromDB(AModuleID, ASectionNo, 'BoardPillar'))     AND
         (DeleteGrowthDataFromDB(AModuleID, ASectionNo, 'HighExtraction'))  AND
         (DeleteQvsSLDDataFromDB(AModuleID, ASectionNo, 'Underground'))     AND
         (DeleteUndergroundSectionMonthlyDataFromDB(AModuleID, ASectionNo)) AND
         (DeleteUndergroundSectionFromDB(AModuleID, ASectionNo)) then
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

{ Opencast Pit ****************************************************************}

function TMineDBManager.UpdateOpencastPitPropertiesDataInDB (AModuleID                         : Integer;
                                                             ASectionNo                        : Integer;
                                                             ASectionName                      : String;
                                                             ACoalReserveArea                  : Double;
                                                             AWorkingArea                      : Double;
                                                             ACommissionYear                   : Integer;
                                                             ACommissionMonth                  : Integer;
                                                             ADeCommissionYear                 : Integer;
                                                             ADeCommissionMonth                : Integer;
                                                             ADisturbedArea                    : Double;
                                                             ARehabilitatedArea                : Double;
                                                             AEvaporationArea                  : Double;
                                                             ADisturbedAreaRunOffFactor        : Double;
                                                             ADisturbedWorkingAreaRunOffFactor : Double;
                                                             AWashOffParameter                 : Double;
                                                             ASulphateBuildUpRate              : Double;
                                                             AInitialSaltMass                  : Double;
                                                             AInspoilsStorageDecant            : Double;
                                                             AInspoilsStorageSeepage           : Double;
                                                             ASeepageEquationExponent          : Double;
                                                             AMaxSeepageRate                   : Double;
                                                             AInspoilsStorageInitialVolume     : Double;
                                                             AInspoilsDamConcentration         : Double;
                                                             APCDInitialVolume                 : Double;
                                                             APCDCapacity                      : Double;
                                                             APCDFullSurfaceArea               : Double): Boolean;
const OPNAME = 'TMineDBManager.UpdateOpencastPitPropertiesDataInDB';
var
  LSQL : String;
begin
  Result := FALSE;
  try
    LSQL := 'UPDATE MineOpencastData SET' +
            ' CoalReserveArea = '                   + FloatToStr(ACoalReserveArea) +
            ', WorkingArea = '                      + FloatToStr(AWorkingArea) +
            ', CommissionYear = '                   + IntToStr(ACommissionYear) +
            ', CommissionMonth = '                  + IntToStr(ACommissionMonth) +
            ', DeCommissionYear = '                 + IntToStr(ADeCommissionYear) +
            ', DeCommissionMonth = '                + IntToStr(ADeCommissionMonth) +
            ', DisturbedArea = '                    + FloatToStr(ADisturbedArea) +
            ', RehabilitatedArea = '                + FloatToStr(ARehabilitatedArea) +
            ', EvaporationArea = '                  + FloatToStr(AEvaporationArea) +
            ', DisturbedAreaRunOffFactor = '        + FloatToStr(ADisturbedAreaRunOffFactor) +
            ', DisturbedWorkingAreaRunOffFactor = ' + FloatToStr(ADisturbedWorkingAreaRunOffFactor) +
            ', WashOffParameter = '                 + FloatToStr(AWashOffParameter) +
            ', SulphateBuilUpRate = '               + FloatToStr(ASulphateBuildUpRate) +
            ', InitialSaltMass = '                  + FloatToStr(AInitialSaltMass) +
            ', InspoilsStorageDecant = '            + FloatToStr(AInspoilsStorageDecant) +
            ', InspoilsStorageSeepage = '           + FloatToStr(AInspoilsStorageSeepage) +
            ', SeepageEquationExponent = '          + FloatToStr(ASeepageEquationExponent) +
            ', MaxSeepageRate = '                   + FloatToStr(AMaxSeepageRate) +
            ', InspoilsStorageInitialVolume = '     + FloatToStr(AInspoilsStorageInitialVolume) +
            ', InspoilsDamConcentration = '         + FloatToStr(AInspoilsDamConcentration) +
            ', ControlDamInitialVolume = '          + FloatToStr(APCDInitialVolume) +
            ', ControlDamCapacity = '               + FloatToStr(APCDCapacity) +
            ', ControlDamFullSurfaceArea = '        + FloatToStr(APCDFullSurfaceArea);
    if (ASectionName = '') then
      LSQL := LSQL + ', OpenCastSectionName = NULL'
    else
      LSQL := LSQL + ', OpenCastSectionName = ' + QuotedStr(ASectionName);
    LSQL := LSQL + ' WHERE ModuleID = ' + IntToStr(AModuleID) + ' AND SectionNo = ' + IntToStr(ASectionNo);
    Result := GHydroDBAgent.ExecuteSQL(LSQL, True);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMineDBManager.UpdateOpencastPitWorkingsAreaDataInDB (AModuleID          : Integer;
                                                               ASectionNo         : Integer;
                                                               AInterpolationType : Integer;
                                                               AYearList          : TStringList;
                                                               AGrowthList        : TStringList): Boolean;
const OPNAME = 'TMineDBManager.UpdateOpencastPitWorkingsAreaDataInDB';
var
  LSQL   : String;
  LIndex : Integer;
begin
  Result := FALSE;
  try
    GHydroDBAgent.StartTransaction;
    try
      LSQL := 'UPDATE MineOpencastData SET WorkingAreaInterpolationOption = ' + IntToStr(AInterpolationType) +
              ' WHERE ModuleID = ' + IntToStr(AModuleID) +
              ' AND SectionNo = ' + IntToStr(ASectionNo);
      Result := GHydroDBAgent.ExecuteSQL(LSQL, True);

      if (Result) then
        Result := DeleteGrowthDataFromDB(AModuleID, ASectionNo, 'WorkingArea');

      LIndex := 0;
      while (Result AND (LIndex < AYearList.Count)) do
      begin
        Result := InsertGrowthDataIntoDB(AModuleID, ASectionNo, 'WorkingArea',
                                         AYearList.Strings[LIndex], AGrowthList.Strings[LIndex]);
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

function TMineDBManager.UpdateOpencastPitDisturbedAreaDataInDB (AModuleID          : Integer;
                                                                ASectionNo         : Integer;
                                                                AInterpolationType : Integer;
                                                                AYearList          : TStringList;
                                                                AGrowthList        : TStringList): Boolean;
const OPNAME = 'TMineDBManager.UpdateOpencastPitDisturbedAreaDataInDB';
var
  LSQL   : String;
  LIndex : Integer;
begin
  Result := FALSE;
  try
    GHydroDBAgent.StartTransaction;
    try
      LSQL := 'UPDATE MineOpencastData SET DisturbedAreaInterpolationOption = ' + IntToStr(AInterpolationType) +
              ' WHERE ModuleID = ' + IntToStr(AModuleID) +
              ' AND SectionNo = ' + IntToStr(AsectionNo);
      Result := GHydroDBAgent.ExecuteSQL(LSQL, True);

      if (Result) then
        Result := DeleteGrowthDataFromDB(AModuleID, AsectionNo, 'DisturbedArea');

      LIndex := 0;
      while ( Result AND (LIndex < AYearList.Count)) do
      begin
        Result := InsertGrowthDataIntoDB(AModuleID, AsectionNo, 'DisturbedArea',
                                         AYearList.Strings[LIndex], AGrowthList.Strings[LIndex]);
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

function TMineDBManager.UpdateOpencastPitRehabilitatedAreaDataInDB (AModuleID          : Integer;
                                                                    ASectionNo         : Integer;
                                                                    AInterpolationType : Integer;
                                                                    AYearList          : TStringList;
                                                                    AGrowthList        : TStringList): Boolean;
const OPNAME = 'TMineDBManager.UpdateOpencastPitRehabilitatedAreaDataInDB';
var
  LSQL   : String;
  LIndex : Integer;
begin
  Result := FALSE;
  try
    GHydroDBAgent.StartTransaction;
    try
      LSQL := 'UPDATE MineOpencastData SET RehabilitatedAreaInterpolationOption = ' + IntToStr(AInterpolationType) +
              ' WHERE ModuleID = ' + IntToStr(AModuleID) +
              ' AND SectionNo = ' + IntToStr(ASectionNo);
      Result := GHydroDBAgent.ExecuteSQL(LSQL, True);

      if (Result) then
        Result := DeleteGrowthDataFromDB(AModuleID, ASectionNo, 'RehabilitatedArea');

      LIndex := 0;
      while (Result AND (LIndex < AYearList.Count)) do
      begin
        Result := InsertGrowthDataIntoDB(AModuleID, ASectionNo, 'RehabilitatedArea',
                                         AYearList.Strings[LIndex], AGrowthList.Strings[LIndex]);
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

function TMineDBManager.UpdateOpencastPitEvaporationAreaDataInDB (AModuleID          : Integer;
                                                                  ASectionNo         : Integer;
                                                                  AInterpolationType : Integer;
                                                                  AYearList          : TStringList;
                                                                  AGrowthList        : TStringList): Boolean;
const OPNAME = 'TMineDBManager.UpdateOpencastPitEvaporationAreaDataInDB';
var
  LSQL   : String;
  LIndex : Integer;
begin
  Result := FALSE;
  try
    GHydroDBAgent.StartTransaction;
    try
      LSQL := 'UPDATE MineOpencastData SET OpenSurfaceAreaInterpolationOption = ' + IntToStr(AInterpolationType) +
              ' WHERE ModuleID = ' + IntToStr(AModuleID) +
              ' AND SectionNo = ' + IntToStr(ASectionNo);
      Result := GHydroDBAgent.ExecuteSQL(LSQL, True);

      if (Result) then
        Result := DeleteGrowthDataFromDB(AModuleID, ASectionNo, 'PitEvaporation');

      LIndex := 0;
      while (Result AND (LIndex < AYearList.Count)) do
      begin
        Result := InsertGrowthDataIntoDB(AModuleID, ASectionNo, 'PitEvaporation', AYearList.Strings[LIndex], AGrowthList.Strings[Lindex]);
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

function TMineDBManager.UpdateOpencastPitInspoilsDecantDataInDB (AModuleID          : Integer;
                                                                 ASectionNo         : Integer;
                                                                 AInterpolationType : Integer;
                                                                 AYearList          : TStringList;
                                                                 AGrowthList        : TStringList): Boolean;
const OPNAME = 'TMineDBManager.UpdateOpencastPitInspoilsDecantDataInDB';
var
  LSQL   : String;
  LIndex : Integer;
begin
  Result := FALSE;
  try
    GHydroDBAgent.StartTransaction;
    try
      LSQL := 'UPDATE MineOpencastData SET InspoilsDecantInterpolationOption = ' + IntToStr(AInterpolationType) +
              ' WHERE ModuleID = ' + IntToStr(AModuleID) +
              ' AND SectionNo = ' + IntToStr(ASectionNo);
      Result := GHydroDBAgent.ExecuteSQL(LSQL, True);

      if (Result) then
        Result := DeleteGrowthDataFromDB(AModuleID, ASectionNo, 'InspoilsDecant');

      LIndex := 0;
      while (Result AND (LIndex < AYearList.Count)) do
      begin
        Result := InsertGrowthDataIntoDB(AModuleID, ASectionNo, 'InspoilsDecant', AYearList.Strings[Lindex], AGrowthList.Strings[Lindex]);
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

function TMineDBManager.UpdateOpencastPitInspoilsSeepageDataInDB (AModuleID          : Integer;
                                                                  ASectionNo         : Integer;
                                                                  AInterpolationType : Integer;
                                                                  AYearList          : TStringList;
                                                                  AGrowthList        : TStringList): Boolean;
const OPNAME = 'TMineDBManager.UpdateOpencastPitInspoilsSeepageDataInDB';
var
  LSQL   : String;
  LIndex : Integer;
begin
  Result := FALSE;
  try
    GHydroDBAgent.StartTransaction;
    try
      LSQL := 'UPDATE MineOpencastData SET InspoilsSeepageInterpolationOption = ' + IntToStr(AInterpolationType) +
              ' WHERE ModuleID = ' + IntToStr(AModuleID) +
              ' AND SectionNo = ' + IntToStr(ASectionNo);
      Result := GHydroDBAgent.ExecuteSQL(LSQL, True);

      if (Result) then
        Result := DeleteGrowthDataFromDB(AModuleID, ASectionNo, 'InspoilsSeepage');

      LIndex := 0;
      while (Result AND (LIndex < AYearList.Count)) do
      begin
        Result := InsertGrowthDataIntoDB(AModuleID, ASectionNo, 'InspoilsSeepage', AYearList.Strings[LIndex], AGrowthList.Strings[LIndex]);
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

function TMineDBManager.UpdateOpencastPitMonthlyDataInDB (AModuleID     : Integer;
                                                          ASectionNo    : Integer;
                                                          AMonthList    : TStringList;
                                                          ADFactorList  : TStringList;
                                                          ADWFactorList : TStringList): Boolean;
const OPNAME = 'TMineDBManager.UpdateOpencastPitMonthlyDataInDB';
var
  LSQL   : String;
  LIndex : Integer;
begin
  Result := FALSE;
  try
    Result := TRUE;

    GHydroDBAgent.StartTransaction;
    try
      LIndex := 0;
      while (Result AND (LIndex < AMonthList.Count)) do
      begin
        LSQL := 'UPDATE MineOpencastMonthlyData SET' +
                ' DisturbedAreaRecharge = ' + ADFactorList.Strings[LIndex] +
                ', DisturbedWorkAreaRecharge = ' + ADWFactorList.Strings[LIndex] +
                ' WHERE ModuleID = ' + IntToStr(AModuleID) + ' AND SectionNo = ' + IntToStr(ASectionNo) +
                ' AND Month = ' + AMonthList.Strings[LIndex];
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

function TMineDBManager.UpdateOpencastPitWorkingsQSLDDataInDB (AModuleID    : Integer;
                                                               ASectionNo   : Integer;
                                                               AStdDev      : Double;
                                                               AFlowRefList : TStringList;
                                                               ALoadList    : TStringList): Boolean;
const OPNAME = 'TMineDBManager.UpdateOpencastPitWorkingsQSLDDataInDB';
var
  LSQL   : String;
  LIndex : Integer;
begin
  Result := FALSE;
  try
    GHydroDBAgent.StartTransaction;
    try
      LSQL := 'UPDATE MineOpencastData SET StandardDeviationWorkingsArea = ' + FloatToStr(AStdDev) +
              ' WHERE ModuleID = ' + IntToStr(AModuleID) +
              ' AND SectionNo = ' + IntToStr(ASectionNo);
      Result := GHydroDBAgent.ExecuteSQL(LSQL, True);

      if (Result) then
        Result := DeleteQvsSLDDataFromDB(AModuleID, ASectionNo, 'Workings');

      LIndex := 0;
      while (Result AND (LIndex < AFlowRefList.Count)) do
      begin
        Result := InsertQvsSLDDataIntoDB(AModuleID, ASectionNo, 'Workings', IntToStr(LIndex+1),
                                         AFlowRefList.Strings[LIndex], ALoadList.Strings[LIndex]);
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

function TMineDBManager.UpdateOpencastPitSeepDecantQSLDDataInDB (AModuleID    : Integer;
                                                                 ASectionNo   : Integer;
                                                                 AStdDev      : Double;
                                                                 AFlowRefList : TStringList;
                                                                 ALoadList    : TStringList): Boolean;
const OPNAME = 'TMineDBManager.UpdateOpencastPitSeepDecantQSLDDataInDB';
var
  LSQL   : String;
  LIndex : Integer;
begin
  Result := FALSE;
  try
    GHydroDBAgent.StartTransaction;
    try
      LSQL := 'UPDATE MineOpencastData SET StandardDeviationSeepageDecant = ' + FloatToStr(AStdDev) +
              ' WHERE ModuleID = ' + IntToStr(AModuleID) +
              ' AND SectionNo = ' + IntToStr(ASectionNo);
      Result := GHydroDBAgent.ExecuteSQL(LSQL, True);

      if (Result) then
        Result := DeleteQvsSLDDataFromDB(AModuleID, ASectionNo, 'SeepDecant');

      LIndex := 0;
      while (Result AND (LIndex < AFlowRefList.Count)) do
      begin
        Result := InsertQvsSLDDataIntoDB(AModuleID, ASectionNo, 'SeepDecant', IntToStr(LIndex+1),
                                         AFlowRefList.Strings[Lindex], ALoadList.Strings[Lindex]);
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

function TMineDBManager.LoadWorkingAreaGrowthDataFromDB (AOpencastPit : TOpencastPit) : Boolean;
const OPNAME = 'TMineDBManager.LoadWorkingAreaGrowthDataFromDB';
var
  LQuery         : TDataSet;
  LSQL           : String;
begin
  Result := FALSE;
  try
    AOpencastPit.ClearWorkingsAreaGrowthData;
    // Load WorkingsArea growth data
    LSQL := 'SELECT * FROM MineModulesGrowthData WHERE Parameter = ' + QuotedStr('WorkingArea') +
            ' AND ModuleID = ' + IntToStr(AOpencastPit.ModuleID) + ' AND SectionNo = ' + IntToStr(AOpencastPit.SectionNo);
    LQuery := GHydroDBAgent.CreateQuery(LSQL);
    try
      LQuery.Open;
      while (NOT LQuery.Eof) do
      begin
        AOpencastPit.AddWorkingsAreaGrowthData(LQuery.FieldByName('Year').AsInteger,
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

function TMineDBManager.LoadDisturbedAreaGrowthDataFromDB (AOpencastPit : TOpencastPit) : Boolean;
const OPNAME = 'TMineDBManager.LoadDisturbedAreaGrowthDataFromDB';
var
  LQuery         : TDataSet;
  LSQL           : String;
begin
  Result := FALSE;
  try
    AOpencastPit.ClearDisturbedAreaGrowthData;
    // Load DisturbedArea growth data
    LSQL := 'SELECT * FROM MineModulesGrowthData WHERE Parameter = ' + QuotedStr('DisturbedArea') +
            ' AND ModuleID = ' + IntToStr(AOpencastPit.ModuleID) + ' AND SectionNo = ' + IntToStr(AOpencastPit.SectionNo);
    LQuery := GHydroDBAgent.CreateQuery(LSQL);
    try
      LQuery.Open;
      while (NOT LQuery.Eof) do
      begin
        AOpencastPit.AddDisturbedAreaGrowthData(LQuery.FieldByName('Year').AsInteger,
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

function TMineDBManager.LoadRehabilitatedAreaGrowthDataFromDB (AOpencastPit : TOpencastPit) : Boolean;
const OPNAME = 'TMineDBManager.LoadRehabilitatedAreaGrowthDataFromDB';
var
  LQuery         : TDataSet;
  LSQL           : String;
begin
  Result := FALSE;
  try
    AOpencastPit.ClearRehabilitatedAreaGrowthData;
    // Load RehabilitatedArea growth data
    LSQL := 'SELECT * FROM MineModulesGrowthData WHERE Parameter = ' + QuotedStr('RehabilitatedArea') +
            ' AND ModuleID = ' + IntToStr(AOpencastPit.ModuleID) + ' AND SectionNo = ' + IntToStr(AOpencastPit.SectionNo);
    LQuery := GHydroDBAgent.CreateQuery(LSQL);
    try
      LQuery.Open;
      while (NOT LQuery.Eof) do
      begin
        AOpencastPit.AddRehabilitatedAreaGrowthData(LQuery.FieldByName('Year').AsInteger,
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

function TMineDBManager.LoadPitEvaporationAreaGrowthDataFromDB (AOpencastPit : TOpencastPit) : Boolean;
const OPNAME = 'TMineDBManager.LoadPitEvaporationAreaGrowthDataFromDB';
var
  LQuery         : TDataSet;
  LSQL           : String;
begin
  Result := FALSE;
  try
    AOpencastPit.ClearPitEvaporationGrowthData;
    // Load PitEvaporation growth data
    LSQL := 'SELECT * FROM MineModulesGrowthData WHERE Parameter = ' + QuotedStr('PitEvaporation') +
            ' AND ModuleID = ' + IntToStr(AOpencastPit.ModuleID) + ' AND SectionNo = ' + IntToStr(AOpencastPit.SectionNo);
    LQuery := GHydroDBAgent.CreateQuery(LSQL);
    try
      LQuery.Open;
      while (NOT LQuery.Eof) do
      begin
        AOpencastPit.AddPitEvaporationGrowthData(LQuery.FieldByName('Year').AsInteger,
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

function TMineDBManager.LoadInspoilsDecantGrowthDataFromDB (AOpencastPit : TOpencastPit) : Boolean;
const OPNAME = 'TMineDBManager.LoadInspoilsDecantGrowthDataFromDB';
var
  LQuery         : TDataSet;
  LSQL           : String;
begin
  Result := FALSE;
  try
    AOpencastPit.ClearInspoilsDecantGrowthData;
    // Load InspoilsDecant growth data
    LSQL := 'SELECT * FROM MineModulesGrowthData WHERE Parameter = ' + QuotedStr('InspoilsDecant') +
            ' AND ModuleID = ' + IntToStr(AOpencastPit.ModuleID) + ' AND SectionNo = ' + IntToStr(AOpencastPit.SectionNo);
    LQuery := GHydroDBAgent.CreateQuery(LSQL);
    try
      LQuery.Open;
      while (NOT LQuery.Eof) do
      begin
        AOpencastPit.AddInspoilsDecantGrowthData(LQuery.FieldByName('Year').AsInteger,
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

function TMineDBManager.LoadInspoilsSeepageGrowthDataFromDB (AOpencastPit : TOpencastPit) : Boolean;
const OPNAME = 'TMineDBManager.LoadInspoilsSeepageGrowthDataFromDB';
var
  LQuery         : TDataSet;
  LSQL           : String;
begin
  Result := FALSE;
  try
    AOpencastPit.ClearInspoilsSeepageGrowthData;
    // Load InspoilsSeepage growth data
    LSQL := 'SELECT * FROM MineModulesGrowthData WHERE Parameter = ' + QuotedStr('InspoilsSeepage') +
            ' AND ModuleID = ' + IntToStr(AOpencastPit.ModuleID) + ' AND SectionNo = ' + IntToStr(AOpencastPit.SectionNo);
    LQuery := GHydroDBAgent.CreateQuery(LSQL);
    try
      LQuery.Open;
      while (NOT LQuery.Eof) do
      begin
        AOpencastPit.AddInspoilsSeepageGrowthData(LQuery.FieldByName('Year').AsInteger,
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

function TMineDBManager.LoadWorkingsQvsSLDDataFromDB (AOpencastPit : TOpencastPit) : Boolean;
const OPNAME = 'TMineDBManager.LoadWorkingsQvsSLDDataFromDB';
var
  LQuery           : TDataSet;
  LSQL             : String;
begin
  Result := FALSE;
  try
    AOpencastPit.ClearWorkingsAreaQvsSLD;
    LSQL := 'SELECT * FROM MineQvsSLDData WHERE ModuleID = ' + IntToStr(AOpencastPit.ModuleID) +
            ' AND Parameter = ' + QuotedStr('Workings') +
            ' AND SectionNo = ' + IntToStr(AOpencastPit.SectionNo) + ' ORDER BY Index';
    LQuery := GHydroDBAgent.CreateQuery(LSQL);
    try
      LQuery.Open;
      while (NOT LQuery.Eof) do
      begin
        AOpencastPit.AddWorkingsAreaLoadFlowRefPair(LQuery.FieldByName('Load').AsFloat,
                                                    LQuery.FieldByName('FlowRef').AsFloat);
        LQuery.Next;
      end;
    finally
      LQuery.Close;
      LQuery.Free;
    end;
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMineDBManager.LoadSeepDecantQvsSLDDataFromDB (AOpencastPit : TOpencastPit) : Boolean;
const OPNAME = 'TMineDBManager.LoadSeepDecantQvsSLDDataFromDB';
var
  LQuery           : TDataSet;
  LSQL             : String;
begin
  Result := FALSE;
  try
    AOpencastPit.ClearSeepageDecantQvsSLD;
    LSQL := 'SELECT * FROM MineQvsSLDData WHERE ModuleID = ' + IntToStr(AOpencastPit.ModuleID) +
            ' AND Parameter = ' + QuotedStr('SeepDecant') +
            ' AND SectionNo = ' + IntToStr(AOpencastPit.SectionNo) + ' ORDER BY Index';
    LQuery := GHydroDBAgent.CreateQuery(LSQL);
    try
      LQuery.Open;
      while (NOT LQuery.Eof) do
      begin
        AOpencastPit.AddSeepageDecantLoadFlowRefPair(LQuery.FieldByName('Load').AsFloat,
                                                     LQuery.FieldByName('FlowRef').AsFloat);
        LQuery.Next;
      end;
    finally
      LQuery.Close;
      LQuery.Free;
    end;

    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMineDBManager.LoadMonthlyDataFromDB (AOpencastPit : TOpencastPit) : Boolean;
const OPNAME = 'TMineDBManager.LoadMonthlyDataFromDB';
var
  LQuery         : TDataSet;
  LSQL           : String;
  LMonth         : Integer;
begin
  Result := FALSE;
  try
    LSQL := 'SELECT * FROM MineOpencastMonthlyData WHERE ModuleID = ' + IntToStr(AOpencastPit.ModuleID) +
            ' AND SectionNo = ' + IntToStr(AOpencastPit.SectionNo) + ' ORDER BY Month';
    LQuery := GHydroDBAgent.CreateQuery(LSQL);
    try
      LQuery.Open;
      while (NOT LQuery.Eof) do
      begin
        LMonth := LQuery.FieldByName('Month').AsInteger;
        AOpencastPit.DisturbedAreaRechargeFactorByMonth[LMonth] := LQuery.FieldByName('DisturbedAreaRecharge').AsFloat;
        AOpencastPit.DisturbedWorkingsAreaRechargeFactorByMonth[LMonth] := LQuery.FieldByName('DisturbedWorkAreaRecharge').AsFloat;
        LQuery.Next;
      end;
    finally
      LQuery.Close;
      LQuery.Free;
    end;
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMineDBManager.DeleteOpencastPitMonthlyDataFromDB (AModuleID : Integer; ASectionNo : Integer = 0) : Boolean;
const OPNAME = 'TMineDBManager.DeleteOpencastPitMonthlyDataFromDB';
var
  LSQL : String;
begin
  Result := FALSE;
  try
    LSQL := 'DELETE * FROM MineOpencastMonthlyData WHERE ModuleID = ' + IntToStr(AModuleID);
    if (ASectionNo <> 0) then
      LSQL := LSQL + ' AND SectionNo = ' + IntToStr(ASectionNo);
    Result := GHydroDBAgent.ExecuteSQL(LSQL, True);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMineDBManager.DeleteOpencastPitFromDB (AModuleID : Integer; ASectionNo : Integer = 0) : Boolean;
const OPNAME = 'TMineDBManager.DeleteOpencastPitFromDB';
var
  LSQL : String;
begin
  Result := FALSE;
  try
    LSQL := 'DELETE * FROM MineOpencastData WHERE ModuleID = ' + IntToStr(AModuleID);
    if (ASectionNo <> 0) then
      LSQL := LSQL + ' AND SectionNo = ' + IntToStr(ASectionNo);
    Result := GHydroDBAgent.ExecuteSQL(LSQL, True);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMineDBManager.LoadOpencastPitDataFromDB (AModuleID: Integer): WordBool;
const OPNAME = 'TMineDBManager.LoadOpencastPitDataFromDB';
var
  LQuery         : TDataSet;
  LSQL           : String;
  LMineModule    : TMineModule;
  LOpencastPit   : TOpencastPit;
  LSectionNo     : Integer;
begin
  Result := FALSE;
  try
    LMineModule := TMineModuleAgent(ModuleAgent).FindMineModuleByID(AModuleID);
    LSQL := 'SELECT * FROM MineOpencastData WHERE ModuleID = ' + IntToStr(AModuleID);
    LQuery := GHydroDBAgent.CreateQuery(LSQL);
    try
      LQuery.Open;
      while (NOT LQuery.Eof) do
      begin
        LOpencastPit := LMineModule.AddOpencastPit;
        LSectionNo   := LQuery.FieldByName('SectionNo').AsInteger;
        LOpencastPit.Populate(AModuleID,
                              LSectionNo,
                              Trim(LQuery.FieldByName('OpenCastSectionName').AsString),
                              LQuery.FieldByName('CoalReserveArea').AsFloat,
                              LQuery.FieldByName('WorkingArea').AsFloat,
                              LQuery.FieldByName('CommissionYear').AsInteger,
                              LQuery.FieldByName('CommissionMonth').AsInteger,
                              LQuery.FieldByName('DeCommissionYear').AsInteger,
                              LQuery.FieldByName('DeCommissionMonth').AsInteger,
                              LQuery.FieldByName('DisturbedArea').AsFloat,
                              LQuery.FieldByName('RehabilitatedArea').AsFloat,
                              LQuery.FieldByName('EvaporationArea').AsFloat,
                              LQuery.FieldByName('WorkingAreaInterpolationOption').AsInteger,
                              LQuery.FieldByName('DisturbedAreaInterpolationOption').AsInteger,
                              LQuery.FieldByName('RehabilitatedAreaInterpolationOption').AsInteger,
                              LQuery.FieldByName('OpenSurfaceAreaInterpolationOption').AsInteger,
                              LQuery.FieldByName('DisturbedAreaRunOffFactor').AsFloat,
                              LQuery.FieldByName('DisturbedWorkingAreaRunOffFactor').AsFloat,
                              LQuery.FieldByName('WashOffParameter').AsFloat,
                              LQuery.FieldByName('SulphateBuilUpRate').AsFloat,
                              LQuery.FieldByName('InitialSaltMass').AsFloat,
                              LQuery.FieldByName('InspoilsStorageDecant').AsFloat,
                              LQuery.FieldByName('InspoilsStorageSeepage').AsFloat,
                              LQuery.FieldByName('InspoilsStorageInitialVolume').AsFloat,
                              LQuery.FieldByName('InspoilsDecantInterpolationOption').AsInteger,
                              LQuery.FieldByName('InspoilsSeepageInterpolationOption').AsInteger,
                              LQuery.FieldByName('MaxSeepageRate').AsFloat,
                              LQuery.FieldByName('SeepageEquationExponent').AsFloat,
                              LQuery.FieldByName('ControlDamFullSurfaceArea').AsFloat,
                              LQuery.FieldByName('ControlDamCapacity').AsFloat,
                              LQuery.FieldByName('ControlDamInitialVolume').AsFloat,
                              LQuery.FieldByName('InspoilsDamConcentration').AsFloat,
                              LQuery.FieldByName('StandardDeviationWorkingsArea').AsFloat,
                              LQuery.FieldByName('StandardDeviationSeepageDecant').AsFloat);
        GMineDBManager.LoadMonthlyDataFromDB(LOpencastPit);
        GMineDBManager.LoadWorkingAreaGrowthDataFromDB(LOpencastPit);
        GMineDBManager.LoadDisturbedAreaGrowthDataFromDB(LOpencastPit);
        GMineDBManager.LoadRehabilitatedAreaGrowthDataFromDB(LOpencastPit);
        GMineDBManager.LoadPitEvaporationAreaGrowthDataFromDB(LOpencastPit);
        GMineDBManager.LoadInspoilsDecantGrowthDataFromDB(LOpencastPit);
        GMineDBManager.LoadInspoilsSeepageGrowthDataFromDB(LOpencastPit);
        GMineDBManager.LoadWorkingsQvsSLDDataFromDB(LOpencastPit);
        GMineDBManager.LoadSeepDecantQvsSLDDataFromDB(LOpencastPit);
        LQuery.Next;
      end;
    finally
      LQuery.Close;
      LQuery.Free;
    end;
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMineDBManager.GetNextOpencastPitSectionNo (AModuleID : Integer) : Integer;
const OPNAME = 'TMineDBManager.GetNextOpencastPitSectionNo';
var
  LSQL    : String;
  LQuery  : TDataSet;
begin
  Result := 0;
  try
    LSQL := 'SELECT MAX(SectionNo) AS LastSectionNo FROM MineOpencastData WHERE ModuleID = ' + IntToStr(AModuleID);
    LQuery := GHydroDBAgent.CreateQuery(LSQL);
    try
      LQuery.Open;
      if (NOT LQuery.Eof) then
        Result := LQuery.FieldByName('LastSectionNo').AsInteger + 1
      else
        Result := 1;
    finally
      LQuery.Close;
      LQuery.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMineDBManager.InsertOpencastPitIntoDB (AModuleID, ASectionNo : Integer) : Boolean;
const OPNAME = 'TMineDBManager.InsertOpencastPitIntoDB';
var
  LSQL    : String;
begin
  Result := FALSE;
  try
    LSQL := 'INSERT INTO MineOpencastData (ModuleID, SectionNo, OpenCastSectionName) VALUES (' +
            IntToStr(AModuleID) + ', ' + IntToStr(ASectionNo) + ', ' +
            QuotedStr('Pit ' + IntToStr(ASectionNo)) + ')';
    Result := GHydroDBAgent.ExecuteSQL(LSQL, True);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMineDBManager.InsertOpencastPitMonthlyDataIntoDB (AModuleID, ASectionNo : Integer) : Boolean;
const OPNAME = 'TMineDBManager.InsertOpencastPitMonthlyDataIntoDB';
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
      LSQL := 'INSERT INTO MineOpencastMonthlyData VALUES (' +
              IntToStr(AModuleID) + ', ' + IntToStr(ASectionNo) + ', ' + IntToStr(LMonth) +  ',0,0)';
      LResult := LResult AND GHydroDBAgent.ExecuteSQL(LSQL, True);
    end;
    Result := LResult;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMineDBManager.CreateNewOpencastPitInDB (AMineModule : TMineModule) : TOpencastPit;
const OPNAME = 'TMineDBManager.CreateNewOpencastPitInDB';
var
  LSectionNo   : Integer;
  LOpencastPit : TOpencastPit;
begin
  Result := nil;
  try
    LSectionNo := GetNextOpencastPitSectionNo(AMineModule.ModuleID);

    GHydroDBAgent.StartTransaction;
    try
      if (InsertOpencastPitIntoDB(AMineModule.ModuleID, LSectionNo)) AND
         (InsertOpencastPitMonthlyDataIntoDB(AMineModule.ModuleID, LSectionNo)) then
      begin
        GHydroDBAgent.CommitTransaction;
        LOpencastPit := AMineModule.AddOpencastPit;
        LOpencastPit.Populate(AMineModule.ModuleID, LSectionNo, 'Pit ' + IntToStr(LSectionNo),
                              0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0);
        LoadMonthlyDataFromDB(LOpencastPit);
        LoadWorkingAreaGrowthDataFromDB(LOpencastPit);
        LoadDisturbedAreaGrowthDataFromDB(LOpencastPit);
        LoadRehabilitatedAreaGrowthDataFromDB(LOpencastPit);
        LoadPitEvaporationAreaGrowthDataFromDB(LOpencastPit);
        LoadInspoilsDecantGrowthDataFromDB(LOpencastPit);
        LoadInspoilsSeepageGrowthDataFromDB(LOpencastPit);
        LoadWorkingsQvsSLDDataFromDB(LOpencastPit);
        LoadSeepDecantQvsSLDDataFromDB(LOpencastPit);
        Result := LOpencastPit;
      end
      else
        GHydroDBAgent.RollbackTransaction;
    except
      GHydroDBAgent.RollbackTransaction;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TMineDBManager.RemoveOpencastPitFromDB (AModuleID, ASectionNo : Integer) : Boolean;
const OPNAME = 'TMineDBManager.RemoveOpencastPitFromDB';
begin
  Result := FALSE;
  try
    GHydroDBAgent.StartTransaction;
    try
      if (DeleteGrowthDataFromDB(AModuleID, ASectionNo, 'WorkingArea'))       AND
         (DeleteGrowthDataFromDB(AModuleID, ASectionNo, 'DisturbedArea'))     AND
         (DeleteGrowthDataFromDB(AModuleID, ASectionNo, 'RehabilitatedArea')) AND
         (DeleteGrowthDataFromDB(AModuleID, ASectionNo, 'PitEvaporation'))    AND
         (DeleteGrowthDataFromDB(AModuleID, ASectionNo, 'InspoilsDecant'))    AND
         (DeleteGrowthDataFromDB(AModuleID, ASectionNo, 'InspoilsSeepage'))   AND
         (DeleteQvsSLDDataFromDB(AModuleID, ASectionNo, 'Workings'))          AND
         (DeleteQvsSLDDataFromDB(AModuleID, ASectionNo, 'SeepDecant'))        AND
         (DeleteOpencastPitMonthlyDataFromDB(AModuleID, ASectionNo))          AND
         (DeleteOpencastPitFromDB(AModuleID, ASectionNo)) then
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

(* Mine ***********************************************************************)

function TMineDBManager.UpdateMinePropertiesDataInDB (AModuleID                 : Integer;
                                                      AActive                   : String;
                                                      ALatitude                 : Double;
                                                      ALongitude                : Double;
                                                      AMineName                 : String;
                                                      AVersionNo                : Integer;
                                                      AMAP                      : Double;
                                                      ARainfallFileName         : String;
                                                      ARunOffModuleNo           : Integer;
                                                      AOutflowRouteNoToRiver    : Integer;
                                                      AOutflowRouteToPCD        : Integer;
                                                      APlantArea                : Double;
                                                      APlantAreaRunOffFactor    : Double;
                                                      ASaltBuildUpRate          : Double;
                                                      ASaltWashOffFactor        : Double;
                                                      AInitialSaltStore         : Double): Boolean;
const OPNAME = 'TMineDBManager.UpdateMinePropertiesDataInDB';
var
  LSQL : String;
begin
  Result := TRUE;
  try
    LSQL := 'UPDATE (Modules INNER JOIN MineModules ON Modules.ModuleID = MineModules.ModuleID) ' +
            'INNER JOIN NetworkModules ON Modules.ModuleID = NetworkModules.ModuleID SET' +
            ' Modules.Latitude = '                      + FloatToStr(ALatitude) +
            ', Modules.Longitude = '                    + FloatToStr(ALongitude) +
            ', NetworkModules.Active = '                + QuotedStr(AActive) +
            ', MineModules.VersionNo = '                + FloatToStr(AVersionNo) +
            ', MineModules.MAP = '                      + FloatToStr(AMAP) +
            ', MineModules.RunOffModuleNo = '           + IntToStr(ARunOffModuleNo) +
            ', MineModules.OutflowRouteNoToRiver = '    + IntToStr(AOutflowRouteNoToRiver) +
            ', MineModules.OutflowRouteToControlDam = ' + IntToStr(AOutflowRouteToPCD) +
            ', MineModules.PlantArea = '                + FloatToStr(APlantArea) +
            ', MineModules.PlantAreaRunOffFactor = '    + FloatToStr(APlantAreaRunOffFactor) +
            ', MineModules.SaltBuildUpRate = '          + FloatToStr(ASaltBuildUpRate) +
            ', MineModules.SaltWashOffFactor = '        + FloatToStr(ASaltWashOffFactor) +
            ', MineModules.InitialSaltStore = '         + FloatToStr(AInitialSaltStore);
    if (AMineName = '') then
      LSQL := LSQL + ', MineModules.MineName = NULL'
    else
      LSQL := LSQL + ', MineModules.MineName = ' + QuotedStr(AMineName);
    if (ARainfallFileName = '') then
      LSQL := LSQL + ', MineModules.RainfallFileName = NULL'
    else
      LSQL := LSQL + ', MineModules.RainfallFileName = ' + QuotedStr(ARainfallFileName);

    LSQL := LSQL + ' WHERE Modules.ModuleID = ' + IntToStr(AModuleID);
    Result := GHydroDBAgent.ExecuteSQL(LSQL, True);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMineDBManager.UpdateMinePlantAreaDataInDB (AModuleID          : Integer;
                                                     AInterpolationType : Integer;
                                                     AYearList          : TStringList;
                                                     AGrowthList        : TStringList): Boolean;
const OPNAME = 'TMineDBManager.UpdateMinePlantAreaDataInDB';
var
  LSQL   : String;
  LIndex : Integer;
begin
  Result := FALSE;
  try
    GHydroDBAgent.StartTransaction;
    try
      LSQL := 'UPDATE MineModules SET PlantAreaGrowthInterpolationType = ' + IntToStr(AInterpolationType) +
              ' WHERE ModuleID = ' + IntToStr(AModuleID);
      Result := GHydroDBAgent.ExecuteSQL(LSQL, True);

      if (Result) then
        Result := DeleteGrowthDataFromDB(AModuleID, 0, 'PlantArea');

      LIndex := 0;
      while (Result AND (LIndex < AYearList.Count)) do
      begin
        Result := InsertGrowthDataIntoDB(AModuleID, 0, 'PlantArea', AYearList.Strings[Lindex], AGrowthList.Strings[LIndex]);
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

function TMineDBManager.LoadMinePlantAreaGrowthDataFromDB (AMine : TMineModule): Boolean;
const OPNAME = 'TMineDBManager.LoadMinePlantAreaGrowthDataFromDB';
var
  LQuery         : TDataSet;
  LSQL           : String;
begin
  Result := FALSE;
  try
    AMine.ClearPlantAreaGrowthData;
    LSQL := 'SELECT * FROM MineModulesGrowthData WHERE Parameter = ' + QuotedStr('PlantArea') +
            ' AND ModuleID = ' + IntToStr(AMine.ModuleID);
    LQuery := GHydroDBAgent.CreateQuery(LSQL);
    try
      LQuery.Open;
      while (NOT LQuery.Eof) do
      begin
        AMine.AddPlantAreaGrowthData(LQuery.FieldByName('Year').AsInteger,
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

function TMineDBManager.DeleteMineModuleFromDB (AModuleID : Integer) : Boolean;
const OPNAME = 'TMineDBManager.DeleteMineModuleFromDB';
var
  LSQL : String;
begin
  Result := FALSE;
  try
    LSQL := 'DELETE * FROM MineModules WHERE ModuleID = ' + IntToStr(AModuleID);
    Result := GHydroDBAgent.ExecuteSQL(LSQL, True);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TMineDBManager.InsertMineIntoDB (AModuleID : Integer; AName : String) : Boolean;
const OPNAME = 'TMineDBManager.InsertMineIntoDB';
var
  LSQL : String;
begin
  Result := FALSE;
  try
    LSQL := 'INSERT INTO MineModules (ModuleID, MineName) VALUES (' +
            IntToStr(AModuleID) + ', ' + QuotedStr(AName) +  ')';
    Result := GHydroDBAgent.ExecuteSQL(LSQL, True);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMineDBManager.CreateNewMineModuleInDB (ANetworkID: Integer) : TMineModule;
const OPNAME = 'TMineDBManager.CreateNewMineModuleInDB';
var
  LMineModule      : TMineModule;
  LModuleID        : Integer;
  LNetworkSequence : Integer;
  LModuleNumber    : Integer;
  LName            : String;
begin
  Result := nil;
  try
    LModuleID        := GHydroDBAgent.GetNextID('Modules', 'ModuleID');
    LNetworkSequence := GetNextNetworkSequence(ANetworkID);
    LModuleNumber    := GetNextModuleNumber(ANetworkID, 'MM');
    LName            := 'MM' + IntToStr(LModuleNumber);

    GHydroDBAgent.StartTransaction;
    try
      if (InsertModuleIntoDB(LModuleID, 'MM', 0, 0)) AND
         (InsertPanIntoDB(LModuleID)) AND
         (InsertNetworkModuleIntoDB(ANetworkID, LModuleID, LModuleNumber, LNetworkSequence, 'Y')) AND
         (InsertMineIntoDB(LModuleID, LName)) then
      begin
        GHydroDBAgent.CommitTransaction;
        LMineModule := TMineModuleAgent(ModuleAgent).AddMineModule;
        LMineModule.Populate(ANetworkID, LModuleID, 'MM', LModuleNumber, LNetworkSequence, 'Y', LName,
                             0, 0, 0, 0, '', 0, 0, 0, 0, 0, 0, 0, 0, 0);
        LoadPanDataFromDB(LMineModule);
        Result := LMineModule;
      end
      else
        GHydroDBAgent.RollbackTransaction;
    except
      GHydroDBAgent.RollbackTransaction;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TMineDBManager.RemoveMineModuleFromDB(AModuleID : Integer): Boolean;
const OPNAME = 'TMineDBManager.RemoveMineModuleFromDB';
begin
  Result := FALSE;
  try
    GHydroDBAgent.StartTransaction;
    try
      if (DeleteUndergroundSectionMonthlyDataFromDB(AModuleID)) AND
         (DeleteUndergroundSectionFromDB(AModuleID))            AND
         (DeleteSlurryDumpMonthlyDataFromDB(AModuleID))         AND
         (DeleteSlurryDumpFromDB(AModuleID))                    AND
         (DeleteOpencastPitMonthlyDataFromDB(AModuleID))        AND
         (DeleteOpencastPitFromDB(AModuleID))                   AND
         (DeleteQvsSLDDataFromDB(AModuleID))                    AND
         (DeleteGrowthDataFromDB(AModuleID))                    AND
         (DeleteMineModuleFromDB(AModuleID))                    AND
         (DeletePanFromDB(AModuleID))                           AND
         (DeleteNetworkModuleFromDB(AModuleID))                 AND
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

function TMineDBManager.LoadMineModulesFromDB (ANetworkID : Integer) : Boolean;
const OPNAME = 'TMineDBManager.LoadMineModulesFromDB';
var
  LQuery       : TDataSet;
  LSQL         : String;
  LMineModule  : TMineModule;
  LModuleID    : Integer;
begin
  Result := FALSE;
  try
    LSQL := 'SELECT A.ModuleNumber, A.NetworkSequence, A.Active, B.*, C.ModuleType, C.Longitude, C.Latitude ' +
            'FROM (NetworkModules AS A LEFT JOIN MineModules AS B ON A.ModuleID = B.ModuleID) ' +
            'LEFT JOIN Modules AS C ON A.ModuleID = C.ModuleID WHERE C.ModuleType = ' +  QuotedStr('MM') +
            ' AND A.NetworkID = ' + IntToStr(ANetworkID);
    LQuery := GHydroDBAgent.CreateQuery(LSQL);
    try
      LQuery.Open;
      while (NOT LQuery.Eof) do
      begin
        LMineModule := TMineModuleAgent(ModuleAgent).AddMineModule;
        LModuleID   := LQuery.FieldByName('ModuleID').AsInteger;
        LMineModule.Populate(ANetworkID,
                             LModuleID,
                             Trim(LQuery.FieldByName('ModuleType').AsString),
                             LQuery.FieldByName('ModuleNumber').AsInteger,
                             LQuery.FieldByName('NetworkSequence').AsInteger,
                             Trim(LQuery.FieldByName('Active').AsString),
                             Trim(LQuery.FieldByName('MineName').AsString),
                             LQuery.FieldByName('VersionNo').AsInteger,
                             LQuery.FieldByName('RunOffModuleNo').AsInteger,
                             LQuery.FieldByName('OutflowRouteNoToRiver').AsInteger,
                             LQuery.FieldByName('OutflowRouteToControlDam').AsInteger,
                             Trim(LQuery.FieldByName('RainfallFileName').AsString),
                             LQuery.FieldByName('MAP').AsFloat,
                             LQuery.FieldByName('PlantArea').AsFloat,
                             LQuery.FieldByName('PlantAreaRunOffFactor').AsFloat,
                             LQuery.FieldByName('SaltBuildUpRate').AsFloat,
                             LQuery.FieldByName('SaltWashOffFactor').AsFloat,
                             LQuery.FieldByName('InitialSaltStore').AsFloat,
                             LQuery.FieldByName('PlantAreaGrowthInterpolationType').AsInteger,
                             LQuery.FieldByName('Longitude').AsFloat,
                             LQuery.FieldByName('Latitude').AsFloat);
        GMineDBManager.LoadPanDataFromDB(LMineModule);
        GMineDBManager.LoadMinePlantAreaGrowthDataFromDB(LMineModule);
        GMineDBManager.LoadOpencastPitDataFromDB(LModuleID);
        GMineDBManager.LoadUndergroundSectionDataFromDB(LModuleID);
        GMineDBManager.LoadSlurryDumpDataFromDB(LModuleID);
        LQuery.Next;
      end;
    finally
      LQuery.Close;
      LQuery.Free;
    end;
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME); end;
end;


end.

