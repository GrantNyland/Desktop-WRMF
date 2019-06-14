//
//
//  UNIT      : Contains TDataObjectRainfall Class
//  AUTHOR    : VGN(Arivia)
//  DATE      : 2003/09/18
//  COPYRIGHT : Copyright © 2004 DWAF                 
//
//

unit UDataObjectRainfall;

interface

uses
  VCL.Dialogs,
  Classes,
  SysUtils,
  VCL.Controls,
  Contnrs,
  VCL.ComCtrls,

  UAbstractObject,
  UViewDataItem,
  UAbstractFileNamesObject,
  UAbstractModelData,
  UGaugeList,
  RainfallCom_TLB,
  UCatchmentZone,
  UYearlyStationData,
  UViewModelDataObject,
  UUtilities,
  UBasicObjects;

type
  TDataObjectRainfall = class(TAbstractModelData, IRainfallModelData)
  protected
    FZoneStations       : TStringList;
    FZoneChangeDate     : TDateTime;
    FZoneRunDate        : TDateTime;
    FGaugeList          : TRainGaugeList;
    FStationDataList    : TObjectList;
    FCatchmentZoneList  : TObjectList;
    FCurrentStationID   : integer;
    FCurrentSplitIndex  : integer;
    FCurrentPatchID     : integer;
    FPatchRStationID    : integer;
    FPatchRSplitIndex   : integer;
    FPatchRPatchID      : integer;

    FFileData           : TStringList;
    FHydroMonthValue    : array[1..12] of double;
    FHydroPatchSign     : array[1..12] of char;
//    FHydroMonthYear     : integer;
    FCurrentCatchment   : integer;
    FDefaultDir         : string;
    FHydroStartMonth    : integer;
    FRAWFlags           : TStringList;
    FIncludeUnreliable  : boolean;

    FHighlightWetSeasonZeros               : Boolean;
    FWetSeasonMonths                       : string;
    FHighlightMonthlyGreaterThanProportion : Boolean;
    FMonthlyGreaterThanProportionValue     : double;
    FHighlightAnnualGreaterThanAverage     : boolean;
    FHighlightAnnualLessThanAverage        : boolean;
    FHighlightMonthlyGreaterThanAbsolute   : Boolean;
    FMonthlyGreaterThanAbsoluteValue       : double;
    FHighlightRepeatingValues              : boolean;
    FHighlightRoundedValues                : boolean;
    FSelectedCatchmentZone                 : string;
    FReArangeData                          : boolean;
    FAppendData                            : boolean;
    FHydroStart,
    FOverwrite                             : boolean;
    function LoadRAWFlags : boolean;
    function LoadZones : boolean;
    function LoadCatchmentSource(ACatchmentZone : TCatchmentZone) : boolean;
    function LoadCatchmentFileData(ACatchmentZone : TCatchmentZone) : boolean;
    function LoadStationsUsedSummary(ACatchmentOutputFileData : TCatchmentOutputFileData) : boolean;
    function LoadProjectGauges (AProjectGauges : TStringList) : boolean;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    procedure CopyClassRAndPatRUtiliesToNewFolder(aOldDirFile, aNewDirFile : string);
    procedure CopyUtilities(aOldDirFile, aNewDirFile : string);
    procedure GetStationDailyData (AStationID : Integer;
                                   AData      : TStringList);
    procedure GetDailyDataByMonthYear (ADailyData    : TStringList;
                                       AYear, AMonth : integer);
    procedure DoReArangeData;
    function PopulateNodeStationData (AStationId    : string;
                                      AStartYear    : integer;
                                      AEndYear      : integer;
                                      ATreeNodeData : TViewDataTreeNodeData):boolean;
    function PopulateNodePatchData(AStationId,APatchID: string; ATreeNodeData : TViewDataTreeNodeData):boolean;
    function PopulateNodeWRCData(APatchID: string; ATreeNodeData : TViewDataTreeNodeData):boolean;
    function CheckAndCreateZone : boolean;
    function CastStationDataByNumber (AStationNumber : string) : TStationData;
    function CastStationDataByID (AStationID : integer) : TStationData;
    function CastStationDataByIndex (AIndex : integer) : TStationData;
    function DeleteProjectGauge (AStationID : integer): Boolean;
    procedure WriteFileHeadings(ACatchmentName       : string;
                                AHeadingPos          : integer;
                                ACatchmentZoneValues :TStringList);
    function GetStationFromDB(AProgressUpdateFuntion : TProgressUpdateFuntion;AStationNumber: string; var ASource : integer) : integer;
    function GetDwafStationIDFromHydtra(AProgressUpdateFuntion : TProgressUpdateFuntion;AStationNumber: string; var ASource : integer) : integer;
    function WriteSAWSDWAFRawDataToDBHydroYear(AStationNumber : integer;AYear : string; ASource : integer;AMonth : array of double;
                                                      APatchSign : array of char): boolean;
    function WriteSAWSDWAFRawDataToDB(AStationNumber : integer;AYear : string; ASource : integer;AMonth : array of double;
                                                      APatchSign : array of char): boolean;

    function InsertSAWSDWAFRawDataToDB(AStationNumber : integer;AYear : string; ASource : integer;AMonth : array of double;
                                                      APatchSign : array of char): boolean;

    function InsertSAWSDWAFRawDataSQL : string;
    function UpdateSAWSDWAFRawDataSQL : string;

    function StationYearVerified(AStationID : integer; AYear : string) : boolean;
    function GetFilesLineTypes: TAbstractFilesLineTypes; override;
    function GetFileNamesObject: TAbstractModelFileNameList; override;


  public

    function PopulateTreeviewWithSelectedProjectGauges(ATreeView: TTreeview): boolean;
    function GetViewDataItems (AViewId      : string;
                               AItemsList   : TViewModelDataItemsList;
                               var AHandled : boolean): boolean; override;
    function GetRainfallStationControlViewDataItems(AViewID: string; AItemsList: TViewModelDataItemsList): boolean;
    function Get_DefaultDir: WideString; safecall;
    procedure Set_DefaultDir(const Value: WideString); safecall;
    function Get_CurrentStationID: Integer; safecall;
    procedure Set_CurrentStationID(Value: Integer); safecall;
    function Get_CurrentSplitIndex: Integer; safecall;
    procedure Set_CurrentSplitIndex(Value: Integer); safecall;
    function Get_CurrentPatchID: Integer; safecall;
    procedure Set_CurrentPatchID(Value: Integer); safecall;
    function Get_PatchRStationID: Integer; safecall;
    procedure Set_PatchRStationID(Value: Integer); safecall;
    function Get_PatchRSplitIndex: Integer; safecall;
    procedure Set_PatchRSplitIndex(Value: Integer); safecall;
    function Get_PatchRPatchID: Integer; safecall;
    procedure Set_PatchRPatchID(Value: Integer); safecall;
    function Get_HydroStartMonth: Integer; safecall;
    procedure Set_HydroStartMonth (Value: Integer); safecall;
    function Get_CatchmentFileName: WideString; safecall;
    procedure Set_CatchmentFileName(const Value: WideString); safecall;
    function Get_ZoneChangeDate: TDateTime; safecall;
    procedure Set_ZoneChangeDate(Value: TDateTime); safecall;
    function Get_ZoneRunDate: TDateTime; safecall;
    procedure Set_ZoneRunDate(Value: TDateTime); safecall;
    function Get_OutputFileName: WideString; safecall;
    procedure Set_OutputFileName(const Value: WideString); safecall;
    function Get_CatchmentFileData: WideString; safecall;
    procedure Set_CatchmentFileData (const Value: WideString); safecall;
    function Get_OutputFileData: WideString; safecall;
    procedure Set_OutputFileData (const Value: WideString); safecall;
    function Get_RAWFlags: WideString; safecall;
    procedure Set_RAWFlags(const Value: WideString); safecall;
    function Get_HighlightWetSeasonZeros: WordBool; safecall;
    procedure Set_HighlightWetSeasonZeros(Value: WordBool); safecall;
    function Get_WetSeasonMonths: WideString; safecall;
    procedure Set_WetSeasonMonths(const Value: WideString); safecall;
    function Get_HighlightMonthlyGreaterThanProportion: WordBool; safecall;
    procedure Set_HighlightMonthlyGreaterThanProportion(Value: WordBool); safecall;
    function Get_MonthlyGreaterThanProportionValue: Double; safecall;
    procedure Set_MonthlyGreaterThanProportionValue(Value: Double); safecall;
    function Get_HighlightAnnualGreaterThanAverage: WordBool; safecall;
    procedure Set_HighlightAnnualGreaterThanAverage(Value: WordBool); safecall;
    function Get_HighlightAnnualLessThanAverage: WordBool; safecall;
    procedure Set_HighlightAnnualLessThanAverage(Value: WordBool); safecall;
    function Get_HighlightMonthlyGreaterThanAbsolute: WordBool; safecall;
    procedure Set_HighlightMonthlyGreaterThanAbsolute(Value: WordBool); safecall;
    function Get_MonthlyGreaterThanAbsoluteValue: Double; safecall;
    procedure Set_MonthlyGreaterThanAbsoluteValue(Value: Double); safecall;
    function Get_HighlightRepeatingValues: WordBool; safecall;
    procedure Set_HighlightRepeatingValues(Value: WordBool); safecall;
    function Get_HighlightRoundedValues: WordBool; safecall;
    procedure Set_HighlightRoundedValues(Value: WordBool); safecall;
    function Get_IncludeUnreliableData: WordBool; safecall;
    procedure Set_IncludeUnreliableData(Value: WordBool); safecall;

    function GetStationDataByNumber(const ANumber: WideString): IStationData; safecall;
    function GetStationDataByID(AStationID: Integer): IStationData; safecall;
    function GetStationDataByIndex(AIndex: Integer): IStationData; safecall;
    function SaveRAWDataFiles : WordBool; safecall;
    function SaveMPDataFiles : WordBool; safecall;
    function SaveProjectGauges : WordBool; safecall;
    function LoadData : WordBool; safecall;
    function LoadMonthlyData: WordBool; safecall;
    function CreateAPatch (APatchTypeID       : integer;
                           AStationID         : integer;
                           const ADescription : WideString;
                           AStartYear         : integer;
                           AEndYear           : integer): integer; safecall;
    function DeleteAPatch(APatchID: Integer): WordBool; safecall;
    function CreateASplit (AStationID : integer;
                           AStartYear : integer;
                           AEndYear   : integer): WordBool; safecall;
    function DeleteASplit (AStationID : integer;
                           AStartYear : integer;
                           AEndYear   : integer): WordBool; safecall;
    function UpdateSplit (AStationID : integer;
                          AOldStartYear,AStartYear : integer;
                          AOldEndYear, AEndYear   : integer): WordBool; safecall;

    function ModifyPatchDescription (AStationID      : Integer;
                                     APatchID        : Integer;
                                     const ANewDescr : WideString): WordBool; safecall;
    function AddToPatch (AStationID       : Integer;
                         APatchID         : Integer;
                         ASourceStationID : Integer;
                         ASourcePatchID   : Integer;
                         AStartYear       : integer;
                         AEndYear         : integer): WordBool; safecall;
    function RemoveFromPatch (APatchID        : Integer;
                              ASourceStationID: Integer;
                              ASourcePatchID  : Integer): WordBool; safecall;
    function GaugeList: IRainGaugeList; safecall;
    function CreateReport: WideString; safecall;
    function Get_StationCount: Integer; safecall;
    function GetDailyDataByMonthAndYear (AStationID : integer;
                                         APatchID   : integer;
                                         AMonth     : integer;
                                         AYear      : integer) : WideString; safecall;
    procedure LatLong (const AStationNumber : WideString;
                       var ALat             : double;
                       var ALong            : double); safecall;
    function GetZoneStations: WideString; safecall;
    function GetZoneCount: Integer; safecall;

    function AddCatchmentZone: TCatchmentZone;
    function AddToCatchmentZone(ACatchmentZone : TCatchmentZone;AStationId : integer;ASplitIndex : integer;
                                APatchID   : Integer;AStartYear : integer;AEndYear   : integer): TCatchmentSource;
    function InsertCatchmentSource(ACatchmentZone : TCatchmentZone;AStationId : integer; ASplitIndex : integer;
                                APatchID   : Integer;AStartYear : integer;AEndYear   : integer): boolean;
    function CreateCatchmentZone(ACatchmentFileName : string) : TCatchmentZone;
    function GetCatchmentZoneListCount : integer;
    function InsertRainfallCatchment(var AIdentifier : integer;ACatchmentFileName : string) : boolean;
    function DeleteCatchmentZoneByIndex(AIndex : integer) : boolean;
    function DeleteCatchmentZoneByName(ACatchmentName : string) : boolean;
    function RemoveCatchmentZone(ACatchmentName : string) : boolean;
    function GetCatchmentZoneByIndex(AIndex : integer) : TCatchmentZone;
    function GetCatchmentZoneByName(ACatchmentName : string) : TCatchmentZone;
    function ExecHDYP08(AProgressUpdateFuntion : TProgressUpdateFuntion) : boolean;
    function WriteCatchmentToDB(ACatchmentZone : TCatchmentZone) : boolean;
    function WriteCatchmentToFile(ACatchmentZone  : TCatchmentZone;
                                  const ADirectory: WideString;
                                  ARANSelected    : boolean;
                                  AOUTPUTSelected : boolean;
                                  AStartYearIndex : integer;
                                  AEndYearIndex   : integer):boolean;
    procedure SetSelectedCatchmentZone(ASelectedCatchmentZone : string);
    procedure SetCatchmentStartEndYear(AStartYear, AEndYear : integer);
    function SetImportRawStationData(AFileData: TStrings;AReArangeData,AAppendData,AOverwrite: boolean):boolean;
    function SetImportProperties(AReArangeData,AAppendData,AOverwrite,AHydroStartMonth: boolean):boolean;
    function ImportRawStationData(AProgressUpdateFuntion : TProgressUpdateFuntion): boolean;
    function AddToRainfallZone(AStationID : Integer;
                               APatchID   : Integer;
                               AStartYear : integer;
                               AEndYear   : integer): WordBool; safecall;
    function RemoveFromRainfallZone(AStationID: Integer; APatchID: Integer): WordBool; safecall;
    function DeleteFromRainfallZone (AStationID : integer;
                                     APatchID   : integer; ACatchmentZone : TCatchmentZone) : WordBool;

    function GetMonthlyData (const AStationNumber : WideString;
                             const APatchName     : WideString) : WideString;
    function GetDailyData (const AStationNumber : WideString;
                           const APatchName     : WideString) : WideString;
    function RefreshStationData : WordBool;
    function RefreshUserGauges : WordBool;
    property DefaultDir        : WideString read Get_DefaultDir        write Set_DefaultDir;
    property CurrentStationID  : Integer    read Get_CurrentStationID  write Set_CurrentStationID;
    property CurrentSplitIndex : Integer    read Get_CurrentSplitIndex write Set_CurrentSplitIndex;

    property CurrentPatchID    : Integer    read Get_CurrentPatchID    write Set_CurrentPatchID;
    property StationCount      : Integer    read Get_StationCount;
    property HydroStartMonth   : Integer    read Get_HydroStartMonth   write Set_HydroStartMonth;
    property CatchmentFileName : WideString read Get_CatchmentFileName write Set_CatchmentFileName;
    property ZoneChangeDate    : TDateTime  read Get_ZoneChangeDate    write Set_ZoneChangeDate;
    property ZoneRunDate       : TDateTime  read Get_ZoneRunDate       write Set_ZoneRunDate;
    property OutputFileName    : WideString read Get_OutputFileName    write Set_OutputFileName;
    property CatchmentFileData : WideString read Get_CatchmentFileData write Set_CatchmentFileData;
    property OutputFileData    : WideString read Get_OutputFileData    write Set_OutputFileData;
    property RAWFlags          : WideString read Get_RAWFlags          write Set_RAWFlags;
    property HighlightWetSeasonZeros               : WordBool   read Get_HighlightWetSeasonZeros               write Set_HighlightWetSeasonZeros;
    property WetSeasonMonths                       : WideString read Get_WetSeasonMonths                       write Set_WetSeasonMonths;
    property HighlightMonthlyGreaterThanProportion : WordBool   read Get_HighlightMonthlyGreaterThanProportion write Set_HighlightMonthlyGreaterThanProportion;
    property MonthlyGreaterThanProportionValue     : Double     read Get_MonthlyGreaterThanProportionValue     write Set_MonthlyGreaterThanProportionValue;
    property HighlightAnnualGreaterThanAverage     : WordBool   read Get_HighlightAnnualGreaterThanAverage     write Set_HighlightAnnualGreaterThanAverage;
    property HighlightAnnualLessThanAverage        : WordBool   read Get_HighlightAnnualLessThanAverage        write Set_HighlightAnnualLessThanAverage;
    property HighlightMonthlyGreaterThanAbsolute   : WordBool   read Get_HighlightMonthlyGreaterThanAbsolute   write Set_HighlightMonthlyGreaterThanAbsolute;
    property MonthlyGreaterThanAbsoluteValue       : Double     read Get_MonthlyGreaterThanAbsoluteValue       write Set_MonthlyGreaterThanAbsoluteValue;
    property HighlightRoundedValues                : WordBool   read Get_HighlightRoundedValues                write Set_HighlightRoundedValues;
    property IncludeUnreliableData                 : WordBool   read Get_IncludeUnreliableData                 write Set_IncludeUnreliableData;
    function Initialise: Boolean; override;

  end;

implementation

uses
  System.Types,
  DB,
  Math,
  //ZLibEx,
  System.ZLib,
  VCL.Graphics,
  VCL.Forms,
  UConstants,
  UDataSetType,
  UDataObjectRainfallLoadAgent,
  UErrorHandlingOperations,
  DateUtils, Variants;

{ TDataObjectRainfall }

procedure TDataObjectRainfall.CreateMemberObjects;
const OPNAME = 'TDataObjectRainfall.CreateMemberObjects';
begin
  inherited CreateMemberObjects;
  try
    FGaugeList       := TRainGaugeList.Create(FAppModules);
    FZoneStations    := TStringList.Create;
    FStationDataList := TObjectList.Create;
    FCatchmentZoneList := TObjectList.Create;
    FRAWFlags        := TStringList.Create;
    FZoneChangeDate  := 0;
    FZoneRunDate     := 0;
    FHydroStartMonth := 10;
    FHighlightWetSeasonZeros               := FALSE;
    FWetSeasonMonths                       := '';
    FHighlightMonthlyGreaterThanProportion := FALSE;
    FMonthlyGreaterThanProportionValue     := 0;
    FHighLightAnnualGreaterThanAverage     := FALSE;
    FHighLightAnnualLessThanAverage        := FALSE;
    FHighlightMonthlyGreaterThanAbsolute   := FALSE;
    FMonthlyGreaterThanAbsoluteValue       := 0;
    FHighlightRepeatingValues              := FALSE;
    FHighlightRoundedValues                := FALSE;
    FIncludeUnreliable                     := FALSE;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TDataObjectRainfall.DestroyMemberObjects;
const OPNAME = 'TDataObjectRainfall.DestroyMemberObjects';
begin
  try
    if Assigned(FGaugeList) then
      FreeAndNil(FGaugeList);
    if Assigned(FZoneStations) then
      FreeAndNil(FZoneStations);
    if (Assigned(FStationDataList)) then
      FreeAndNil(FStationDataList);
    if Assigned(FRAWFlags) then
      FreeAndNil(FRAWFlags);
    if Assigned(FCatchmentZoneList) then
      FreeAndNil(FCatchmentZoneList);  
    inherited DestroyMemberObjects;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TDataObjectRainfall.LoadData : WordBool;
const OPNAME = 'TDataObjectRainfall.LoadData';
var
  LLoadAgent        : TDataObjectRainfallLoadAgent;
begin
  Result := False;
  try
    LLoadAgent        := TDataObjectRainfallLoadAgent.Create(FAppModules);
    try
      {FDefaultDir  := '';//FAppModules.StudyArea.DataFilesPath;
      if (Trim(FDefaultDir) = '') then
      }
      FDefaultDir := GetAppDataLocalDir+'\WRCDATA\';  //ExtractFilePath(ApplicationExeName) + 'WRCDATA\';
      if (not DirectoryExists(FDefaultDir)) or (FDefaultDir[Length(FDefaultDir)] <> '\') then
      begin
        if (FDefaultDir[Length(FDefaultDir)] <> '\') then
        begin
          FDefaultDir := FDefaultDir + '\';
          FAppModules.StudyArea.DataFilesPath := FDefaultDir;
        end;
      end;
      //if (UpperCase(ExtractFilePath(ApplicationExeName) + 'WRCDATA\') <> UpperCase(FDefaultDir)) then
      if (UpperCase(GetAppDataLocalDir+'\WRCDATA\') <> UpperCase(FDefaultDir)) then
      begin
        ForceDirectories(FDefaultDir);
        //CopyClassRAndPatRUtiliesToNewFolder(ExtractFilePath(ApplicationExeName) + 'WRCDATA\',FDefaultDir);
        CopyClassRAndPatRUtiliesToNewFolder(GetAppDataLocalDir+'\WRCDATA\',FDefaultDir);
      end;
      LoadRAWFlags;
      CheckAndCreateZone;
      if not FGaugeList.FileLoaded then
        FGaugeList.LoadFromDatabase();
      RefreshUserGauges;
      FStationDataList.Clear;
      LLoadAgent.LoadData(FStationDataList);
      LoadZones;
      FCurrentPatchID    := 0;
      FCurrentStationID  := 0;
      FCurrentSplitIndex := -1;
      FPatchRStationID   := 0;
      FPatchRStationID  := 0;
      FPatchRSplitIndex := -1;
      FCurrentCatchment  := 0;
      Result := True;
    finally
      FreeAndNil(LLoadAgent);
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TDataObjectRainfall.GaugeList: IRainGaugeList;
const OPNAME = 'TDataObjectRainfall.GaugeList';
begin
  Result := nil;
  try
    if Assigned(FGaugeList) then
      Result := FGaugeList;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TDataObjectRainfall.InsertRainfallCatchment(var AIdentifier : integer; ACatchmentFileName : string) : boolean;
const OPNAME = 'TDataObjectRainfall.InsertRainfallCatchment';
var
  LDataset : TAbstractModelDataset;
  LSQL : string;
begin
  Result := False;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataset);
    try
      LSQL := ' SELECT Max(CatchmentID) AS LastCatchmentID FROM RainfallCatchment WHERE '+
               QuotedStr(FAppModules.StudyArea.ModelCode) + ' AND ' +
               QuotedStr(FAppModules.StudyArea.StudyAreaCode) + ' AND ' +
               QuotedStr(FAppModules.StudyArea.SubAreaCode) + ' AND ' +
               QuotedStr(FAppModules.StudyArea.ScenarioCode);
      LDataset.DataSet.Close;
      LDataset.SetSQL(LSQL);
      LDataset.DataSet.Open;
      AIdentifier := LDataset.DataSet.FieldByName('LastCatchmentID').AsInteger + 1;
      LDataset.DataSet.Close;
      LDataset.ClearSQL;
      LDataset.ClearQueryParams();
      LSQL := 'INSERT INTO RainfallCatchment ' +
              ' (Model, StudyAreaName, SubArea, Scenario, CatchmentID, CatchmentFileName ) VALUES (' +
               QuotedStr(FAppModules.StudyArea.ModelCode) + ',' +
               QuotedStr(FAppModules.StudyArea.StudyAreaCode) + ',' +
               QuotedStr(FAppModules.StudyArea.SubAreaCode) + ',' +
               QuotedStr(FAppModules.StudyArea.ScenarioCode) + ','+
               IntToStr(AIdentifier) +','+QuotedStr(ACatchmentFileName)+')';
      LDataset.SetSQL(LSQL);
      LDataset.ExecSQL;
    finally
      FreeAndNil(LDataset);
    end;
    Result := True;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;


function TDataObjectRainfall.AddToCatchmentZone(ACatchmentZone : TCatchmentZone;AStationId : integer; ASplitIndex : integer;
                                                APatchID   : Integer;AStartYear : integer;AEndYear   : integer): TCatchmentSource;
const OPNAME = 'TDataObjectRainfall.AddToCatchmentZone';
//var
//    LCatchmentSource : TCatchmentSource;
begin
  Result := nil;
  try
    if ACatchmentZone <> nil then
    begin
      {LCatchmentSource := ACatchmentZone.CatchmentSourceByStationID[AStationId];
      if (LCatchmentSource <> nil) then
      begin
        ShowMessage(FAppModules.Language.GetString('Rainfall.CatchmentContainsSourceFromSameGauge'));
        Exit;
      end;
      }
      if InsertCatchmentSource(ACatchmentZone,AStationId,ASplitIndex,APatchID,AStartYear,AEndYear) then
      begin
        Result := ACatchmentZone.AddCatchmentSource;
        Result.CatchmentID := ACatchmentZone.CatchmentID;
        Result.StationID := AStationId;
        Result.SplitIndex := ASplitIndex;
        Result.PatchID := APatchID;
        Result.HydroStartYear :=  AStartYear;
        Result.HydroEndYear := AEndYear;
      end;
    end;
  except on E:Exception do HandleError(E,OPNAME) end;
end;

function TDataObjectRainfall.InsertCatchmentSource(ACatchmentZone : TCatchmentZone;AStationId : integer; ASplitIndex : integer;
                                                   APatchID   : Integer;AStartYear : integer;AEndYear   : integer): boolean;
const OPNAME = 'TDataObjectRainfall.InsertCatchmentSource';
var
  LDataset : TAbstractModelDataset;
  LSQL : string;
begin
  Result := False;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataset);
    try
      LDataset.ClearSQL;
      LSQL := ' INSERT INTO RainfallCatchmentSource ' +
                  ' (Model, StudyAreaName, SubArea, Scenario, CatchmentID,SourcePatchID, StationID,SplitID,' +
                  ' HydroStartYear, HydroEndYear) VALUES (' +
                  QuotedStr(FAppModules.StudyArea.ModelCode)     + ',' +
                  QuotedStr(FAppModules.StudyArea.StudyAreaCode) + ',' +
                  QuotedStr(FAppModules.StudyArea.SubAreaCode)   + ',' +
                  QuotedStr(FAppModules.StudyArea.ScenarioCode)  + ',' +
                  IntToStr(ACatchmentZone.CatchmentID)           + ',' +
                  IntToStr(APatchID)                             + ',' +
                  IntToStr(AStationId)                           + ',' +
                  IntToStr(ASplitIndex)                          + ',' +
                  IntToStr(AStartYear)                           + ',' +
                  IntToStr(AEndYear)                             + ')';
          LDataset.SetSQL(lSQL);
          LDataset.ExecSQL;
          ZoneChangeDate := Now;
          Result := True;
    finally
      FreeAndNil(LDataset);
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDataObjectRainfall.AddToRainFallZone(AStationId : integer;
                                               APatchID   : Integer;
                                               AStartYear : integer;
                                               AEndYear   : integer): WordBool;
const OPNAME = 'TDataObjectRainfall.AddToRainFallZone';
var
  lDataset        : TAbstractModelDataset;
  LDuplicateGauge : boolean;
  LErrorFree      : boolean;
  LIndex          : integer;
  lSQL            : string;
begin
  Result := False;
  try
    LDuplicateGauge := False;
    if Assigned(FZoneStations) then
    begin
      for LIndex := 0 to FZoneStations.Count - 1 do
      begin
        if StrToInt(Trim(Copy(FZoneStations[LIndex],1,6))) = AStationId then
        begin
          Result := False;
          LDuplicateGauge := True;
          Break;
        end;
      end;
    end;

    if not LDuplicateGauge then
    begin
      LErrorFree := FAppModules.Database.CreateDataset(integer(dtExecSQL), lDataset);
      try
        if LErrorFree then
        begin
          lDataset.DataSet.Close;
          lSQL := ' INSERT INTO RainfallCatchmentSource ' +
                  ' (Model, StudyAreaName, SubArea, Scenario, SourcePatchID, StationID,' +
                  ' HydroStartYear, HydroEndYear) VALUES (' +
                  QuotedStr(FAppModules.StudyArea.ModelCode)     + ',' +
                  QuotedStr(FAppModules.StudyArea.StudyAreaCode) + ',' +
                  QuotedStr(FAppModules.StudyArea.SubAreaCode)   + ',' +
                  QuotedStr(FAppModules.StudyArea.ScenarioCode)  + ',' +
                  IntToStr(APatchID)                             + ',' +
                  IntToStr(AStationId)                           + ',' +
                  IntToStr(AStartYear)                           + ',' +
                  IntToStr(AEndYear)                             + ')';
          lDataset.SetSQL(lSQL);
          lDataset.ExecSQL;
        end;
        ZoneChangeDate := Now;
        LoadZones;
        Result := True;
      finally
        if Assigned(lDataset) then
        begin
          lDataset.Dataset.Close;
          FreeAndNil(lDataset);
        end;
      end;
    end;
  except on E:Exception do HandleError(E,OPNAME) end;
end;

function TDataObjectRainfall.CreateAPatch (APatchTypeID       : integer;
                                           AStationID         : integer;
                                           const ADescription : WideString;
                                           AStartYear         : integer;
                                           AEndYear           : integer): Integer;
const OPNAME = 'TDataObjectRainfall.CreateAPatch';
var
  LDataset     : TAbstractModelDataset;
  LErrorFree   : boolean;
  LPatchID     : integer;
  lSQL         : string;
  lStationData : TStationData;
  lPatchData   : TPatchData;
begin
  Result := 0;
  try
    LErrorFree := FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataset);
    try
      if LErrorFree then
      begin
        LDataset.DataSet.Close;
        lSQL := 'SELECT Max(PatchID) AS NewPatchID FROM RainfallPatchR';
        LDataset.SetSQL(lSQL);
        LDataset.Dataset.Open;
        LPatchID := 0;
        if (NOT LDataset.DataSet.Eof) then
          LPatchID := LDataset.Dataset.FieldByName('NewPatchID').AsInteger;
        if (LPatchID = 0) then
          LPatchID := 20000;

        LPatchID := LPatchID + 1;
        LDataset.DataSet.Close;
        lSQL := 'INSERT INTO RainfallPatchR ' +
                '(StudyAreaName, SubArea, PatchID, PatchTypeID, Description) ' +
                'VALUES (' +
                QuotedStr(FAppModules.StudyArea.StudyAreaCode) + ',' +
                QuotedStr(FAppModules.StudyArea.SubAreaCode) + ',' +
                IntToStr(LPatchID) + ',' +
                IntToStr(APatchTypeID) + ',' +
                QuotedStr(ADescription) + ')';
        LDataset.SetSQL(lSQL);
        LDataset.ExecSQL;

        LDataset.DataSet.Close;
        lSQL := 'INSERT INTO RainfallPatchSource ' +
                '(PatchID, SourceStationID, SourcePatchID, TargetStation, HydroStartYear, HydroEndYear) ' +
                'VALUES (' +
                IntToStr(LPatchID) + ',' +
                IntToStr(AStationID) + ',' +
                IntToStr(0) + ',' +
                QuotedStr('Y') + ',' +
                IntToStr(AStartYear) + ',' +
                IntToStr(AEndYear) + ')';
        LDataset.SetSQL(lSQL);
        LDataset.ExecSQL;

        lStationData := CastStationDataByID(AStationID);
        if (lStationData <> nil) then
        begin
          lPatchData := TPatchData.Create(FAppModules);
          lPatchData.Initialise;
          lPatchData.PatchID       := lPatchID;
          lPatchData.PatchTypeID   := APatchTypeID;
          lPatchData.PatchName     := ADescription;
          lPatchData.RainfallData.StationNumber := lStationData.RainfallData.StationNumber;
          lPatchData.RainfallData.StationID     := AStationID;
          lPatchData.AddSource(AStationID, 0, TRUE, AStartYear, AEndYear);
          lStationData.Patches.Add(lPatchData);
        end;
        Result := LPatchID;
      end;
    finally
      if Assigned(LDataset) then
      begin
        LDataset.Dataset.Close;
        FreeAndNil(LDataset);
      end;
    end;
  except on E:Exception do HandleError(E,OPNAME) end;
end;

function TDataObjectRainfall.DeleteAPatch (APatchID : integer) : WordBool;
const OPNAME = 'TDataObjectRainfall.DeleteAPatch';
var
  lDataset    : TAbstractModelDataset;
  LErrorFree  : boolean;
  lSQL        : string;
  lStation    : TStationData;
  lStationID  : integer;
  lSearchKey  : string;
begin
  Result := False;
  try
    LErrorFree := FAppModules.Database.CreateDataset(integer(dtExecSQL), lDataset);
    try
      if LErrorFree then
      begin
        lDataset.DataSet.Close;
        lSQL := 'SELECT * FROM RainfallPatchSource ' +
                ' WHERE PatchID = ' + IntToStr(APatchID);
        lDataset.SetSQL(lSQL);
        lDataset.DataSet.Open;
        while (NOT lDataset.DataSet.Eof) do
        begin
          lStationID := lDataset.DataSet.FieldByName('SourceStationID').AsInteger;
          lStation   := CastStationDataByID(lStationID);
          if (lStation <> nil) then
            lStation.DeletePatchWithID(APatchID);
          lDataset.DataSet.Next;
        end;

        lDataset.DataSet.Close;
        lSQL := 'DELETE FROM RainfallCatchmentSource ' +
                ' WHERE SourcePatchID = ' + IntToStr(APatchID);
        lDataset.SetSQL(lSQL);
        lDataset.ExecSQL;

        lDataset.DataSet.Close;
        lSQL := 'DELETE FROM RainfallPatchSource ' +
                ' WHERE PatchID = ' + IntToStr(APatchID);
        lDataset.SetSQL(lSQL);
        lDataset.ExecSQL;

        lDataset.DataSet.Close;
        lSQL := 'DELETE FROM RainfallPatchR ' +
                ' WHERE PatchID = ' + IntToStr(APatchID);
        lDataset.SetSQL(lSQL);
        lDataset.ExecSQL;

        lDataset.DataSet.Close;
        lSQL := 'DELETE FROM RainfallMonthlyPatchData ' +
                ' WHERE PatchID = ' + IntToStr(APatchID);
        lDataset.SetSQL(lSQL);
        lDataset.ExecSQL;

        lSearchKey := 'PatchID=' + IntToStr(APatchID) + '%';

        // Delete ChangeParameter
        lDataSet.DataSet.Close;
        lSQL := 'DELETE * FROM ChangeParameter WHERE KeyValues LIKE ' + QuotedStr(lSearchKey);
        lDataSet.DataSet.Close;
        lDataSet.SetSQL(lSQL);
        lDataSet.ExecSQL;

        // Delete MetaDataItem
        lDataSet.DataSet.Close;
        lSQL := 'DELETE * FROM MetaDataItem WHERE KeyValues LIKE ' + QuotedStr(lSearchKey);
        lDataSet.DataSet.Close;
        lDataSet.SetSQL(lSQL);
        lDataSet.ExecSQL;

        Result := True;
      end;
    finally
      if Assigned(lDataset) then
      begin
        lDataset.Dataset.Close;
        FreeAndNil(lDataset);
      end;
    end;
  except on E:Exception do HandleError(E,OPNAME) end;
end;

function TDataObjectRainfall.Get_CurrentPatchID : Integer;
const OPNAME = 'TDataObjectRainfall.Get_CurrentPatchID';
begin
  Result := 0;
  try
    Result := FCurrentPatchID;
  except on E:Exception do HandleError(E,OPNAME) end;
end;

function TDataObjectRainfall.Get_CurrentStationID : Integer;
const OPNAME = 'TDataObjectRainfall.Get_CurrentStationID';
begin
  Result := 0;
  try
    Result := FCurrentStationID;
  except on E:Exception do HandleError(E,OPNAME) end;
end;

function TDataObjectRainfall.Get_CurrentSplitIndex : Integer;
const OPNAME = 'TDataObjectRainfall.Get_CurrentSplitIndex';
begin
  Result := 0;
  try
    Result := FCurrentSplitIndex;
  except on E:Exception do HandleError(E,OPNAME) end;
end;

function TDataObjectRainfall.Get_PatchRPatchID : Integer;
const OPNAME = 'TDataObjectRainfall.Get_PatchRPatchID';
begin
  Result := 0;
  try
    Result := FPatchRPatchID;
  except on E:Exception do HandleError(E,OPNAME) end;
end;

function TDataObjectRainfall.Get_PatchRStationID : Integer;
const OPNAME = 'TDataObjectRainfall.Get_PatchRStationID';
begin
  Result := 0;
  try
    Result := FPatchRStationID;
  except on E:Exception do HandleError(E,OPNAME) end;
end;

function TDataObjectRainfall.Get_PatchRSplitIndex : Integer;
const OPNAME = 'TDataObjectRainfall.Get_PatchRSplitIndex';
begin
  Result := 0;
  try
    Result := FPatchRSplitIndex;
  except on E:Exception do HandleError(E,OPNAME) end;
end;

procedure TDataObjectRainfall.Set_PatchRPatchID (Value: Integer);
const OPNAME = 'TDataObjectRainfall.Set_PatchRPatchID';
begin
  try
    FPatchRPatchID := Value;
  except on E:Exception do HandleError(E,OPNAME) end;
end;

procedure TDataObjectRainfall.Set_PatchRStationID (Value: Integer);
const OPNAME = 'TDataObjectRainfall.Set_PatchRStationID';
begin
  try
    FPatchRStationID := Value;
  except on E:Exception do HandleError(E,OPNAME) end;
end;

procedure TDataObjectRainfall.Set_PatchRSplitIndex (Value: Integer);
const OPNAME = 'TDataObjectRainfall.Set_PatchRSplitIndex';
begin
  try
    FPatchRSplitIndex := Value;
  except on E:Exception do HandleError(E,OPNAME) end;
end;

function TDataObjectRainfall.Get_HydroStartMonth : Integer;
const OPNAME = 'TDataObjectRainfall.Get_HydroStartMonth';
begin
  Result := -1;
  try
    Result := FHydroStartMonth;
  except on E:Exception do HandleError(E,OPNAME) end;
end;

procedure TDataObjectRainfall.Set_HydroStartMonth (Value : Integer);
const OPNAME = 'TDataObjectRainfall.Set_HydroStartMonth';
begin
  try
    FHydroStartMonth := Value;
  except on E:Exception do HandleError(E,OPNAME) end;
end;

function TDataObjectRainfall.ModifyPatchDescription (AStationID      : Integer;
                                                     APatchID        : Integer;
                                                     const ANewDescr : WideString) : WordBool;
const OPNAME = 'TDataObjectRainfall.ModifyPatchDescription';
var
  LDataset   : TAbstractModelDataset;
  LErrorFree : boolean;
  lSQL       : string;
  lStation   : TStationData;
  lPatch     : TPatchData;
begin
  Result := False;
  try
    LErrorFree := FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataset);
    try
      if LErrorFree then
      begin
        LDataset.DataSet.Close;
        lSQL := 'UPDATE RainfallPatchR SET Description = :APatchDescription ' +
                ' WHERE PatchID = :APatchID';
        LDataset.SetSQL(lSQL);
        LDataset.SetParams(['APatchID', 'APatchDescription' ],
          [IntToStr(APatchID), ANewDescr]);
        LDataset.ExecSQL;
        lStation := CastStationDataByID(AStationID);
        if (lStation <> nil) then
        begin
          lPatch := lStation.CastPatchWithID(APatchID);
          if (lPatch <> nil) then
            lPatch.PatchName := ANewDescr;
        end;
      end;
      Result := True;
    finally
      if Assigned(LDataset) then
      begin
        LDataset.Dataset.Close;
        FreeAndNil(LDataset);
      end;
    end;
  except on E:Exception do HandleError(E,OPNAME) end;
end;

procedure TDataObjectRainfall.Set_CurrentPatchID (Value: Integer);
const OPNAME = 'TDataObjectRainfall.Set_CurrentPatchID';
begin
  try
    FCurrentPatchID := Value;
  except on E:Exception do HandleError(E,OPNAME) end;
end;

procedure TDataObjectRainfall.Set_CurrentStationID (Value: Integer);
const OPNAME = 'TDataObjectRainfall.Set_CurrentStationID';
begin
  try
    FCurrentStationID := Value;
  except on E:Exception do HandleError(E,OPNAME) end;
end;

procedure TDataObjectRainfall.Set_CurrentSplitIndex (Value: Integer);
const OPNAME = 'TDataObjectRainfall.Set_CurrentSplitIndex';
begin
  try
    FCurrentSplitIndex := Value;
  except on E:Exception do HandleError(E,OPNAME) end;
end;

function TDataObjectRainfall.AddToPatch(AStationID       : integer;
                                        APatchID         : integer;
                                        ASourceStationID : integer;
                                        ASourcePatchID   : Integer;
                                        AStartYear       : integer;
                                        AEndYear         : integer): WordBool;
const OPNAME = 'TDataObjectRainfall.AddToPatch';
var
  lDataset         : TAbstractModelDataset;
  lSQL             : string;
  lPatch           : TPatchData;
  lSourceStation   : TStationData;
  lSourceStationID : integer;
begin
  Result := False;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), lDataset);
    try
      if (Assigned(lDataset)) then
      begin
        lDataset.DataSet.Close;
        lSQL := 'INSERT INTO RainfallPatchSource ' +
                '(PatchID, SourceStationID, SourcePatchID, HydroStartYear, HydroEndYear) ' +
                'VALUES (' +
                IntToStr(APatchID) + ',' +
                IntToStr(ASourceStationID) + ',' +
                IntToStr(ASourcePatchID) + ',' +
                IntToStr(AStartYear) + ',' +
                IntToStr(AEndYear) + ')';
        lDataset.SetSQL(lSQL);
        lDataset.ExecSQL;

        lDataset.DataSet.Close;
        lSQL := 'SELECT * FROM RainfallPatchSource ' +
                'WHERE PatchID = ' + IntToStr(APatchID) +
                ' ORDER BY TargetStation DESC';
        lDataset.SetSQL(lSQL);
        lDataset.DataSet.Open;
        while (NOT lDataset.DataSet.Eof) do
        begin
          lSourceStationID := lDataset.DataSet.FieldByName('SourceStationID').AsInteger;
          lSourceStation := CastStationDataByID(lSourceStationID);
          if (lSourceStation <> nil) then
          begin
            lPatch := lSourceStation.CastPatchWithID(APatchID);
            if (lPatch <> nil) then
            begin
              lPatch.AddSource(ASourceStationID, ASourcePatchID, FALSE, AStartYear, AEndYear);
              lPatch.ChangeDate := Now;
            end;
          end;
          lDataset.DataSet.Next;
        end;
        Result := True;
      end;
    finally
      if Assigned(lDataset) then
      begin
        lDataset.Dataset.Close;
        FreeAndNil(lDataset);
      end;
    end;
  except on E:Exception do HandleError(E,OPNAME) end;
end;

function TDataObjectRainfall.RemoveFromPatch (APatchID         : integer;
                                              ASourceStationID : integer;
                                              ASourcePatchID   : Integer): WordBool;
const OPNAME = 'TDataObjectRainfall.RemoveFromPatch';
var
  lDataset         : TAbstractModelDataset;
  lSQL             : string;
  lPatch           : TPatchData;
  lSourceStation   : TStationData;
  lSourceStationID : integer;
begin
  Result := False;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), lDataset);
    try
      if (Assigned(lDataset)) then
      begin
        lDataset.DataSet.Close;
        lSQL := 'SELECT * FROM RainfallPatchSource ' +
                'WHERE PatchID = ' + IntToStr(APatchID) +
                ' ORDER BY TargetStation DESC';
        lDataset.SetSQL(lSQL);
        lDataset.DataSet.Open;
        while (NOT lDataset.DataSet.Eof) do
        begin
          lSourceStationID := lDataset.DataSet.FieldByName('SourceStationID').AsInteger;
          lSourceStation := CastStationDataByID(lSourceStationID);
          if (lSourceStation <> nil) then
          begin
            lPatch := lSourceStation.CastPatchWithID(APatchID);
            if (lPatch <> nil) then
            begin
              if (lSourceStationID = ASourceStationID) then
                lSourceStation.DeletePatchWithID(APatchID)
              else
                lPatch.RemoveSource(ASourceStationID);
            end;
          end;
          lDataset.DataSet.Next;
        end;
        lDataset.DataSet.Close;
        lSQL := 'DELETE * FROM RainfallPatchSource ' +
                'WHERE PatchID = ' + IntToStr(APatchID) +
                ' AND SourceStationID = ' + IntToStr(ASourceStationID) +
                ' AND SourcePatchID = ' + IntToStr(ASourcePatchID);
        lDataset.SetSQL(lSQL);
        lDataset.ExecSQL;
      end;
      Result := True;
    finally
      if Assigned(lDataset) then
      begin
        lDataset.Dataset.Close;
        FreeAndNil(lDataset);
      end;
    end;
  except on E:Exception do HandleError(E,OPNAME) end;
end;

function TDataObjectRainfall.LoadRAWFlags : boolean;
const OPNAME = 'TDataObjectRainfall.LoadRAWFlags';
var
  LDataset   : TAbstractModelDataset;
  LErrorFree : boolean;
  lSQL       : string;
begin
  Result := False;
  try
    LErrorFree := FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataset);
    try
      if LErrorFree then
      begin
        LDataset.DataSet.Close;
        lSQL := 'SELECT * FROM RainfallRAWFlags';
        LDataset.SetSQL(lSQL);
        LDataset.Dataset.Open;
        LDataset.Dataset.First;
        FRAWFlags.Clear;
        if (NOT LDataset.DataSet.Eof) then
        begin
          FRAWFlags.CommaText := Trim(LDataset.Dataset.FieldByName('RAWFlags').AsString);

          FHighlightWetSeasonZeros
            := (Uppercase(Trim(LDataset.Dataset.FieldByName('WetSeasonZeros').AsString)) = 'Y');
          FWetSeasonMonths
            := Trim(LDataset.Dataset.FieldByName('WetSeasonMonths').AsString);
          FHighlightMonthlyGreaterThanProportion
            := (Uppercase(Trim(LDataset.Dataset.FieldByName('MonthlyGreaterProportion').AsString)) = 'Y');
          FMonthlyGreaterThanProportionValue
            := LDataset.Dataset.FieldByName('MonthlyProportionValue').AsFloat;
          FHighlightAnnualGreaterThanAverage
            := (Uppercase(Trim(LDataset.Dataset.FieldByName('AnnualGreater').AsString)) = 'Y');
          FHighlightAnnualLessThanAverage
            := (Uppercase(Trim(LDataset.Dataset.FieldByName('AnnualLess').AsString)) = 'Y');
          FHighlightMonthlyGreaterThanAbsolute
            := (Uppercase(Trim(LDataset.Dataset.FieldByName('MontlyGreaterAbsolute').AsString)) = 'Y');
          FMonthlyGreaterThanAbsoluteValue
            := LDataset.Dataset.FieldByName('MonthlyGreaterAbsoluteValue').AsFloat;
          FHighlightRepeatingValues
            := (Uppercase(Trim(LDataset.Dataset.FieldByName('RepeatingValues').AsString)) = 'Y');
          FHighlightRoundedValues
            := (Uppercase(Trim(LDataset.Dataset.FieldByName('RoundedValues').AsString)) = 'Y');
        end;
      end;
    finally
      if Assigned(LDataset) then
      begin
        LDataset.Dataset.Close;
        FreeAndNil(LDataset);
      end;
    end;
    Result := True;
  except on E:Exception do HandleError(E,OPNAME) end;
end;

function TDataObjectRainfall.LoadCatchmentSource(ACatchmentZone : TCatchmentZone) : boolean;
const OPNAME = 'TDataObjectRainfall.LoadCatchmentSource';
var
  LDataset   : TAbstractModelDataset;
  LCatchmentSource : TCatchmentSource;
begin
  Result := False;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataset);
    try
      if (LDataset <> nil) and (ACatchmentZone <> nil) then
      begin
        FZoneStations.Clear;
        lDataset.DataSet.Close;
        lDataset.SetSQL('SELECT CatchmentID,SourcePatchID, StationID, HydroStartYear, HydroEndYear ' +
                        ' FROM RainfallCatchmentSource ' +
                        ' WHERE Model = :AModel ' +
                        ' AND StudyAreaName = :AStudyAreaName ' +
                        ' AND SubArea = :ASubArea ' +
                        ' AND Scenario = :AScenario ' +
                        ' AND CatchmentID = :ACatchmentID ');

        lDataset.SetParams(['AModel','AStudyAreaName','ASubArea','AScenario','ACatchmentID'],
          [FAppModules.StudyArea.ModelCode,
           FAppModules.StudyArea.StudyAreaCode,
           FAppModules.StudyArea.SubAreaCode,
           FAppModules.StudyArea.ScenarioCode,
           IntToStr(ACatchmentZone.CatchmentID)]);

        lDataset.Dataset.Open();

        while not lDataset.DataSet.Eof do
        begin
          LCatchmentSource := ACatchmentZone.AddCatchmentSource;
          LCatchmentSource.CatchmentID := ACatchmentZone.CatchmentID;
          LCatchmentSource.StationID := LDataset.Dataset.FieldByName('StationID').AsInteger;
          LCatchmentSource.PatchID   := lDataset.Dataset.FieldByName('SourcePatchID').AsInteger;
          LCatchmentSource.HydroStartYear := lDataset.Dataset.FieldByName('HydroStartYear').AsInteger;
          LCatchmentSource.HydroEndYear := lDataset.Dataset.FieldByName('HydroEndYear').AsInteger;
          LCatchmentSource.CatchmentID := ACatchmentZone.CatchmentID;
          lDataset.DataSet.Next;
        end;
      end;
    finally
      FreeAndNil(LDataset);
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDataObjectRainfall.LoadStationsUsedSummary(ACatchmentOutputFileData : TCatchmentOutputFileData) : boolean;
const OPNAME = 'TDataObjectRainfall.LoadStationsUsedSummary';
var
  LDataset   : TAbstractModelDataset;
  LDetailOfRainfallStationsUsed : TDetailOfRainfallStationsUsed;
begin
  Result := False;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataset);
    try
      if (LDataset <> nil) and (ACatchmentOutputFileData <> nil) then
      begin
        LDataset.DataSet.Close;
        LDataset.SetSQL('SELECT CatchmentID,StationID,[Section],[Position],MAP,PeriodOfRecord,Longitude,Latitude'+
                        ' FROM RainfallCatchmentFileDetail ' +
                        ' WHERE Model = :AModel ' +
                        ' AND StudyAreaName = :AStudyAreaName ' +
                        ' AND SubArea = :ASubArea ' +
                        ' AND Scenario = :AScenario ' +
                        ' AND CatchmentID = :ACatchmentID ');

        LDataset.SetParams(['AModel','AStudyAreaName','ASubArea','AScenario','ACatchmentID'],
          [FAppModules.StudyArea.ModelCode,
           FAppModules.StudyArea.StudyAreaCode,
           FAppModules.StudyArea.SubAreaCode,
           FAppModules.StudyArea.ScenarioCode,
           IntToStr(ACatchmentOutputFileData.CatchmentID)]);

        LDataset.Dataset.Open();
        while not LDataset.Dataset.Eof do
        begin
          LDetailOfRainfallStationsUsed                 := ACatchmentOutputFileData.AddDetailOfRainfallStationsUsed;
          LDetailOfRainfallStationsUsed.StationID       := LDataset.Dataset.FieldByName('StationID').AsInteger;
          LDetailOfRainfallStationsUsed.Section         := Trim(LDataset.Dataset.FieldByName('Section').AsString);
          LDetailOfRainfallStationsUsed.Position        := Trim(LDataset.Dataset.FieldByName('Position').AsString);
          LDetailOfRainfallStationsUsed.MAPInmm         := Trunc(LDataset.Dataset.FieldByName('MAP').AsFloat);
          LDetailOfRainfallStationsUsed.PeriodOfRecord  := Trim(LDataset.Dataset.FieldByName('PeriodOfRecord').AsString);
          LDetailOfRainfallStationsUsed.Longitude       := Trim(LDataset.Dataset.FieldByName('Longitude').AsString);
          LDetailOfRainfallStationsUsed.Latitude        := Trim(LDataset.Dataset.FieldByName('Latitude').AsString);
          LDataset.Dataset.Next;
        end;
      end;
    finally
      FreeAndNil(LDataset);
    end;
    Result := True;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDataObjectRainfall.LoadCatchmentFileData(ACatchmentZone : TCatchmentZone) : boolean;
const OPNAME = 'TDataObjectRainfall.LoadCatchmentFileData';
var
  LDataset   : TAbstractModelDataset;
  LCatchmentOutputFileData : TCatchmentOutputFileData;
  LRainfallAsPercentMAP : TRainfallAsPercentMAP;
  LCount : integer;
  LFieldName : string;
begin
  Result := False;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataset);
    try
      if (LDataset <> nil) and (ACatchmentZone <> nil) then
      begin
        LDataset.DataSet.Close;
        LDataset.SetSQL('SELECT CatchmentID,[Year],NoOfGauges,GaugesUsed,'+
                        ' PercentageOfMAP01,PercentageOfMAP02,PercentageOfMAP03,PercentageOfMAP04,'+
                        ' PercentageOfMAP05,PercentageOfMAP06,PercentageOfMAP07,PercentageOfMAP08,'+
                        ' PercentageOfMAP09,PercentageOfMAP10,PercentageOfMAP11,PercentageOfMAP12 '+
                        ' FROM RainfallCatchmentFileData ' +
                        ' WHERE Model = :AModel ' +
                        ' AND StudyAreaName = :AStudyAreaName ' +
                        ' AND SubArea = :ASubArea ' +
                        ' AND Scenario = :AScenario ' +
                        ' AND CatchmentID = :ACatchmentID ' +
                        ' ORDER BY Year ' );
        LDataset.SetParams(['AModel','AStudyAreaName','ASubArea','AScenario','ACatchmentID'],
          [FAppModules.StudyArea.ModelCode,
           FAppModules.StudyArea.StudyAreaCode,
           FAppModules.StudyArea.SubAreaCode,
           FAppModules.StudyArea.ScenarioCode,
           IntToStr(ACatchmentZone.CatchmentID)]);

        LDataset.Dataset.Open();
        LCatchmentOutputFileData := ACatchmentZone.CatchmentOutputFileData;
        if not LCatchmentOutputFileData.Initialise then
          Exit;
        LCatchmentOutputFileData.CatchmentID :=  ACatchmentZone.CatchmentID;
        while not LDataset.DataSet.Eof do
        begin
          LRainfallAsPercentMAP := LCatchmentOutputFileData.AddRainfallAsPercentMAP;
          if LRainfallAsPercentMAP <> nil then
          begin
            LRainfallAsPercentMAP.HydroYear := LDataset.Dataset.FieldByName('Year').AsInteger;
            LRainfallAsPercentMAP.GaugesUsed := Trim(LDataset.Dataset.FieldByName('GaugesUsed').AsString);
            for LCount := MinMonths to MaxMonths do
            begin
              LFieldName := Format('%s%2.2d',['PercentageOfMAP',LCount]);
              if not (LDataset.Dataset.FieldByName(LFieldName).IsNull) then
              begin
                LRainfallAsPercentMAP.PercentageOfMAPByIndex[LCount] := LDataset.Dataset.FieldByName(LFieldName).AsFloat;
                LRainfallAsPercentMAP.HydroYearTotal := LRainfallAsPercentMAP.HydroYearTotal + LRainfallAsPercentMAP.PercentageOfMAPByIndex[LCount];
              end;
            end;
          end;
          LDataset.DataSet.Next;
        end;
        LoadStationsUsedSummary(LCatchmentOutputFileData);
      end;
    finally
      FreeAndNil(LDataset);
    end;
    Result := True;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDataObjectRainfall.LoadZones: boolean;
const OPNAME = 'TDataObjectRainfall.LoadZones';
var
  lDataset   : TAbstractModelDataset;
  lSQl       : string;
  LCatchmentZone : TCatchmentZone;
begin
  Result := False;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), lDataset);
    try
      if (lDataset <> nil) then
      begin
        FZoneStations.Clear;

        lDataset.DataSet.Close;
        lSQL := 'SELECT CatchmentID, CatchmentFileName,ChangeDate, RunDate,OutputFileName FROM RainfallCatchment ' +
                ' WHERE Model = :AModel ' +
                ' AND StudyAreaName = :AStudyAreaName ' +
                ' AND SubArea = :ASubArea ' +
                ' AND Scenario = :AScenario ';

        lDataset.SetSQL(lSQl);
        lDataset.SetParams(['AModel','AStudyAreaName','ASubArea','AScenario'],
          [FAppModules.StudyArea.ModelCode,
           FAppModules.StudyArea.StudyAreaCode,
           FAppModules.StudyArea.SubAreaCode,
           FAppModules.StudyArea.ScenarioCode]);
        lDataset.Dataset.Open;
        if not Initialise then
          Exit;
        while not (lDataset.DataSet.Eof) do
        begin
          FZoneChangeDate := lDataset.Dataset.FieldByName('ChangeDate').AsDateTime;
          FZoneRunDate    := lDataset.Dataset.FieldByName('RunDate').AsDateTime;

          LCatchmentZone :=  AddCatchmentZone;
          if not (LCatchmentZone.Initialise) then
            Exit;
          if LCatchmentZone.Populate(LDataset.Dataset.FieldByName('CatchmentID').AsInteger,
                                  FZoneChangeDate,FZoneRunDate,
                                  Trim(LDataset.Dataset.FieldByName('OutputFileName').AsString),
                                  Trim(LDataset.Dataset.FieldByName('CatchmentFileName').AsString)) then
            LoadCatchmentSource(LCatchmentZone);
            LoadCatchmentFileData(LCatchmentZone);
          LDataset.DataSet.Next;
        end;
      end;
    finally
      if Assigned(lDataset) then
      begin
        lDataset.Dataset.Close;
        FreeAndNil(lDataset);
      end;
    end;
    Result := True;
  except on E:Exception do HandleError(E,OPNAME) end;
end;

function TDataObjectRainfall.Get_DefaultDir : WideString;
const OPNAME = 'TDataObjectRainfall.Get_DefaultDir';
begin
  Result := '';
  try
    Result := fDefaultDir;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TDataObjectRainfall.Set_DefaultDir (const Value : WideString);
const OPNAME = 'TDataObjectRainfall.Set_DefaultDir';
begin
  try
    if (Value = fDefaultDir) then
      Exit
    else
      FDefaultDir := Value;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDataObjectRainfall.SaveRAWDataFiles : WordBool;
const OPNAME = 'TDataObjectRainfall.SaveRAWDataFiles';
var
  lIndex           : integer;
  lClassRInputFile : TFileStream;
  lClassRFiles     : TStringList;
  lDirectory       : string;
  lStation         : TStationData;
  lPatch           : TPatchData;
  lStationName     : string;
  lSourceStationID : integer;
  lSourcePatchID   : integer;
  lTargetStation   : WideString;
  lSourceStation   : TStationData;
  lSourcePatch     : TPatchData;
  lStartYear       : integer;
  lEndYear         : integer;
begin
  Result := True;
  try
    if (fDefaultDir = '') then
      lDirectory := GetAppDataLocalDir+'\wrcdata\'   //ExtractFilePath(ApplicationExeName) + 'wrcdata\'
    else
      lDirectory := fDefaultDir;
    if (NOT DirectoryExists(lDirectory)) then
      CreateDir(lDirectory);
    lClassRFiles := TStringList.Create;

    try
      lStation := CastStationDataByID(FPatchRStationID);
      if (lStation <> nil) then
      begin
        lPatch := lStation.CastPatchWithID(FPatchRPatchID);
        if (lPatch <> nil) then
        begin
          for lIndex := 0 to lPatch.SourcesCount - 1 do
          begin
            lPatch.GetSourceInfoByIndex(lIndex, lSourceStationID, lSourcePatchID,
                                        lTargetStation, lStartYear, lEndYear);
            lSourceStation := CastStationDataByID(lSourceStationID);
            if (lSourceStation <> nil) then
            begin
              lSourcePatch := nil;
              if (lSourcePatchID <> 0) then
                lSourcePatch := lSourceStation.CastPatchWithID(lSourcePatchID);
              if (lSourcePatch <> nil) then
                lSourcePatch.SaveRAWFile(0, 0, lDirectory)
              else
                lSourceStation.SaveRAWFile(lStartYear, lEndYear, lDirectory);
              lStationName := lSourceStation.RainfallData.StationNumber;
              while (Pos(' ', lStationName) > 0) do
                Delete(lStationName, Pos(' ', lStationName), 1);
              lStationName := Copy(lStationName, 1, 8);
              lClassRFiles.Add(lStationName + '.RAW');
            end;
          end;
          if (FileExists(lDirectory + lPatch.ClassRInputFileName)) then
            DeleteFile(lDirectory + lPatch.ClassRInputFileName);
          lClassRInputFile := TFileStream.Create(lDirectory + lPatch.ClassRInputFileName, fmCreate);
          lClassRFiles.SaveToStream(lClassRInputFile);
          lPatch.ClassRInputData := lClassRFiles.Text;
        end;
      end;
    finally
      FreeAndNil(lClassRInputFile );
      FreeAndNil(lClassRFiles);
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDataObjectRainfall.SaveMPDataFiles : WordBool;
const OPNAME = 'TDataObjectRainfall.SaveMPDataFiles';
var
  lIndex       : integer;
  lStation     : TStationData;
  lPatch       : TPatchData;
  lDirectory   : string;
  lStationID   : integer;
  lPatchID     : integer;
  lStartYear   : integer;
  lEndYear     : integer;
  lZoneStr     : string;
begin
  Result := True;
  try
    if (fDefaultDir = '') then
      lDirectory := GetAppDataLocalDir+'\wrcdata\'   //ExtractFilePath(ApplicationExeName) + 'wrcdata\'
    else
      lDirectory := fDefaultDir;
    if (NOT DirectoryExists(lDirectory)) then
      CreateDir(lDirectory);
    for lIndex := 0 to FZoneStations.Count - 1 do
    begin
      lZoneStr  := FZoneStations.Strings[lIndex];
      lStationID := StrToInt(Trim(Copy(lZoneStr,  1, 6)));
      lPatchID   := StrToInt(Trim(Copy(lZoneStr,  7, 6)));
      lStartYear := StrToInt(Trim(Copy(lZoneStr, 13, 4)));
      lEndYear   := StrToInt(Trim(Copy(lZoneStr, 17, 4)));
      lStation   := CastStationDataByID(lStationID);
      if (lStation <> nil) then
      begin
        if (lPatchID <> 0) then
        begin
          lPatch := lStation.CastPatchWithID(lPatchID);
          if (lPatch <> nil) then
            lPatch.SaveMPFile(0, 0, lDirectory);
        end
        else
        begin
          lStation.SaveMPFile(lStartYear, lEndYear, lDirectory);  
        end;
      end;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TDataObjectRainfall.CopyClassRAndPatRUtiliesToNewFolder ( aOldDirFile, aNewDirFile : string );
const OPNAME = 'TDataObjectRainfall.CopyClassRAndPatRUtiliesToNewFolder';
var
 lStrClassr : string;
 lStrPatchr : string;
 lStrHdyp   : string;
 lStrTnt    : string;
 lStrLf     : string;
begin
  try
    lStrClassr := FAppModules.Language.GetString('Rainfall.Classr');
    lStrPatchr := FAppModules.Language.GetString('Rainfall.Patchr');
    lStrHdyp   := FAppModules.Language.GetString('Rainfall.HDYP08');
    lStrTnt    := FAppModules.Language.GetString('Rainfall.tnt');
    lStrLf     := FAppModules.Language.GetString('Rainfall.LF90');

    CopyUtilities ( aOldDirFile  + lStrClassr, aNewDirFile + lStrClassr );
    CopyUtilities ( aOldDirFile  + lStrPatchr, aNewDirFile + lStrPatchr );
    CopyUtilities ( aOldDirFile  + lStrHdyp, aNewDirFile + lStrHdyp );
    CopyUtilities ( aOldDirFile  + lStrTnt, aNewDirFile + lStrTnt );
    CopyUtilities ( aOldDirFile  + lStrLf, aNewDirFile + lStrLf );
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TDataObjectRainfall.CopyUtilities ( aOldDirFile, aNewDirFile : string );
const OPNAME = 'TDataObjectRainfall.CopyUtilities';
var
  lNewFile: TFileStream;
  lOldFile: TFileStream;
begin
  try
    lOldFile := TFileStream.Create ( aOldDirFile, fmOpenRead or fmShareDenyWrite );
    try
      lNewFile := TFileStream.Create( aNewDirFile, fmCreate or fmShareExclusive );
      try
        lNewFile.CopyFrom ( lOldFile, lOldFile.Size );
      finally
        FreeAndNil ( lNewFile );
      end;
    finally
      FreeAndNil ( lOldFile );
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDataObjectRainfall.CastStationDataByNumber (AStationNumber : string) : TStationData;
const OPNAME = 'TDataObjectRainfall.CastStationDataByNumber';
var
  lIndex   : integer;
  lStation : TStationData;
begin
  Result := nil;
  try
    lIndex := 0;
    while ((Result = nil) AND (lIndex < FStationDataList.Count)) do
    begin
      lStation := TStationData(FStationDataList.Items[lIndex]);
      if (lStation.RainfallData.StationNumber = AStationNumber) then
        Result := lStation
      else
        lIndex := lIndex + 1;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDataObjectRainfall.GetStationDataByNumber (const ANumber: WideString) : IStationData;
const OPNAME = 'TDataObjectRainfall.GetStationDataByNumber';
begin
  Result := nil;
  try
    Result := CastStationDataByNumber(ANumber);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDataObjectRainfall.CastStationDataByID (AStationID : integer) : TStationData;
const OPNAME = 'TDataObjectRainfall.CastStationDataByID';
var
  lIndex   : integer;
  lStation : TStationData;
begin
  Result := nil;
  try
    lIndex := 0;
    while ((Result = nil) AND (lIndex < FStationDataList.Count)) do
    begin
      lStation := TStationData(FStationDataList.Items[lIndex]);
      if (lStation.RainfallData.StationID = AStationID) then
        Result := lStation
      else
        lIndex := lIndex + 1;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDataObjectRainfall.GetStationDataByID (AStationID : integer) : IStationData;
const OPNAME = 'TDataObjectRainfall.GetStationDataByID';
begin
  Result := nil;
  try
    Result := CastStationDataByID(AStationID);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDataObjectRainfall.CastStationDataByIndex (AIndex : integer) : TStationData;
const OPNAME = 'TDataObjectRainfall.CastStationDataByIndex';
begin
  Result := nil;
  try
    if (AIndex < FStationDataList.Count) then
      Result := TStationData(FStationDataList.Items[AIndex]);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDataObjectRainfall.GetStationDataByIndex (AIndex : integer) : IStationData;
const OPNAME = 'TDataObjectRainfall.GetStationDataByIndex';
begin
  Result := nil;
  try
    Result := CastStationDataByIndex(AIndex);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDataObjectRainfall.Get_StationCount : integer;
const OPNAME = 'TDataObjectRainfall.Get_StationCount';
begin
  Result := 0;
  try
    Result := FStationDataList.Count;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDataObjectRainfall.LoadMonthlyData : WordBool;
const OPNAME = 'TDataObjectRainfall.LoadMonthlyData';
var
  lDataset          : TAbstractModelDataset;
  lStationID        : integer;
  lStationNumber    : string;
  lSQL              : string;
  lStationData      : TStationData;
  lIndex            : integer;
  lStation          : TStationData;
  lProjectGauges    : TStringList;
  lCount            : integer;
  lDone             : boolean;
begin
  Result := TRUE;
  try
    lProjectGauges := TStringList.Create;
    try
      LoadProjectGauges(lProjectGauges);
      FAppModules.Database.CreateDataset(integer(dtExecSQL), lDataset);
      try
        {lIndex := 0;
        while (lIndex < FStationDataList.Count) do
        begin
          lStation := CastStationDataByIndex(lIndex);
          if (lProjectGauges.IndexOf(IntToStr(lStation.RainfallData.StationID)) < 0) then
            FStationDataList.Delete(lIndex)
          else
            lIndex := lIndex + 1;
        end;
        }
        FStationDataList.Clear;
        for lIndex := 0 to lProjectGauges.Count - 1 do
        begin
          lStationID := StrToInt(lProjectGauges.Strings[lIndex]);
          lStation   := CastStationDataByID(lStationID);
          if ((lStation = nil) AND Assigned(lDataset)) then
          begin
            if (lStationID > 100000) then
              lSQL := 'SELECT * FROM RainfallUserStations WHERE StationID = ' +
                       IntToStr(lStationID)
            else
              lSQL := 'SELECT * FROM RainfallStations WHERE StationID = ' +
                      IntToStr(lStationID);
            lDataset.DataSet.Close;
            lDataset.SetSQL(lSQL);
            lDataset.DataSet.Open;
            if (NOT lDataset.DataSet.Eof) then
            begin
              lStationNumber := Trim(lDataset.DataSet.FieldByName('StationNumber').AsString);
              lStationData   := TStationData.Create(FAppModules);
              lStationData.Initialise;
              lStationData.RainfallData.StationNumber := lStationNumber;
              lStationData.RainfallData.StationID     := lStationID;
              lStationData.StationName   := Trim(lDataset.DataSet.FieldByName('StationName').AsString);
              lStationData.Latitude      := lDataset.DataSet.FieldByName('StationLatitude').AsInteger;
              lStationData.Longitude     := lDataset.DataSet.FieldByName('StationLongitude').AsInteger;
              if (lDataset.DataSet.FieldByName('StationHeight').IsNull) then
                lStationData.Height      := NullInteger
              else
                lStationData.Height      := lDataset.DataSet.FieldByName('StationHeight').AsInteger;
              lStationData.StationType   := Trim(lDataset.DataSet.FieldByName('StationType').AsString);
              if (lStationID < 100000) then
                lStationData.IsInWR90 := (Trim(lDataset.DataSet.FieldByName('WR90').AsString) = 'Y')
              else
                lStationData.IsInWR90 := FALSE;

              lCount := 0;
              lDone  := FALSE;
              while (NOT lDone) AND (lCount < FStationDataList.Count) do
              begin
                if (lStationNumber < TStationData(FStationDataList.Items[lCount]).RainfallData.StationNumber) then
                begin
                  FStationDataList.Insert(lCount, lStationData);
                  lDone := TRUE;
                end
                else
                  lCount := lCount + 1;
              end;
              if (NOT lDone) then
                FStationDataList.Add(lStationData);
              lStationData.LoadMonthlyData;
            end
            else
            begin
              Result := FALSE;
              raise Exception.Create ('StationNumber not found ' + lStationNumber);
            end;
          end;
        end;
      finally
        lDataset.Free;
      end;
    finally
      FreeAndNil(lProjectGauges);
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDataObjectRainfall.LoadProjectGauges (AProjectGauges : TStringList) : boolean;
const OPNAME = 'TDataObjectRainfall.LoadProjectGauges';
var
  LDataset   : TAbstractModelDataset;
  LErrorFree : boolean;
  lSQL       : string;
begin
  Result := False;
  try
    LErrorFree := FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataset);
    try
      if LErrorFree then
      begin
        LDataset.DataSet.Close;
        lSQL := '  SELECT * FROM RainfallProjectGauges                         ' +
                '  WHERE                                                       ' +
                '    RainfallProjectGauges.Model = :AModel                     ' +
                '    AND RainfallProjectGauges.StudyAreaName = :AStudyAreaName ' +
                '    AND RainfallProjectGauges.SubArea = :ASubArea             ' +
                '    AND RainfallProjectGauges.Scenario = :AScenario           ';
        LDataset.SetSQL(lSQL);
        LDataset.SetParams(['AModel','AStudyAreaName','ASubArea','AScenario'],
          [FAppModules.StudyArea.ModelCode,
           FAppModules.StudyArea.StudyAreaCode,
           FAppModules.StudyArea.SubAreaCode,
           UAbstractObject.CProjectGauges]);
        LDataset.Dataset.Open();
        AProjectGauges.Clear;
        while not LDataset.DataSet.Eof do
        begin
          AProjectGauges.Add(Trim(LDataset.DataSet.FieldByName('StationID').AsString));
          LDataset.DataSet.Next;
        end;
      end;
    finally
      if Assigned(LDataset) then
      begin
        LDataset.Dataset.Close;
        FreeAndNil(LDataset);
      end;
    end;
    Result := True;
  except on E:Exception do HandleError(E,OPNAME) end;
end;

function TDataObjectRainfall.CheckAndCreateZone: boolean;
const OPNAME = 'TDataObjectRainfall.CheckAndCreateZone';
var
  lDataset   : TAbstractModelDataset;
  LErrorFree : boolean;
begin
  Result := False;
  try
    if CompareText(FAppModules.StudyArea.ScenarioCode, UAbstractObject.CProjectGauges) <> 0 then
    begin
      LErrorFree := FAppModules.Database.CreateDataset(integer(dtExecSQL), lDataset);
      try
        if LErrorFree then
        begin
          lDataset.DataSet.Close;
          lDataset.SetSQL(
            '  SELECT CatchmentID FROM RainfallCatchment               ' +
            '  WHERE                                                       ' +
            '    RainfallCatchment.Model = :AModel                     ' +
            '    AND RainfallCatchment.StudyAreaName = :AStudyAreaName ' +
            '    AND RainfallCatchment.SubArea = :ASubArea             ' +
            '    AND RainfallCatchment.Scenario = :AScenario           ' );
          lDataset.SetParams(['AModel','AStudyAreaName','ASubArea','AScenario'],
            [FAppModules.StudyArea.ModelCode,
             FAppModules.StudyArea.StudyAreaCode,
             FAppModules.StudyArea.SubAreaCode,
             FAppModules.StudyArea.ScenarioCode]);
          lDataset.DataSet.Open;

          if not lDataset.DataSet.Eof then
          begin
            FCurrentCatchment := lDataset.DataSet.FieldByName('CatchmentID').AsInteger;
          end
          else
          begin
            lDataset.DataSet.Close;
            lDataset.SetSQL(
              '  SELECT MAX(CatchmentID) as MaxID FROM RainfallCatchment   ' +
              '  WHERE                                                         ' +
              '    RainfallCatchment.Model = :AModel                       ' +
              '    AND RainfallCatchment.StudyAreaName = :AStudyAreaName   ' +
              '    AND RainfallCatchment.SubArea = :ASubArea               ' +
              '    AND RainfallCatchment.Scenario = :AScenario             ' );
            lDataset.SetParams(['AModel','AStudyAreaName','ASubArea','AScenario'],
              [FAppModules.StudyArea.ModelCode,
               FAppModules.StudyArea.StudyAreaCode,
               FAppModules.StudyArea.SubAreaCode,
               FAppModules.StudyArea.ScenarioCode]);
            lDataset.DataSet.Open;

            FCurrentCatchment := lDataset.DataSet.FieldByName('MaxID').AsInteger;

            lDataset.DataSet.Close;
            lDataset.SetSQL(
              '  INSERT INTO RainfallCatchment ' +
              '    (Model, StudyAreaName, SubArea, Scenario, CatchmentID, OutputFileName, CatchmentFileName) ' +
              '  VALUES                                                                                                ' +
              '    (:AModel, :AStudyAreaName, :ASubArea, :AScenario, :ACatchmentID, :ACatchmentFile, :ACatchmentCode); ' );
            lDataset.SetParams(['AModel','AStudyAreaName','ASubArea','AScenario',
              'ACatchmentID','ACatchmentFile','ACatchmentCode'],
              [FAppModules.StudyArea.ModelCode,
               FAppModules.StudyArea.StudyAreaCode,
               FAppModules.StudyArea.SubAreaCode,
               FAppModules.StudyArea.ScenarioCode,
               IntToStr(FCurrentCatchment),
               ' ',
               ' ']);
             lDataset.ExecSQL;
             Result := True;
          end;
        end; {if}
      Result := True;
      finally
        if Assigned(lDataset) then
        begin
          lDataset.Dataset.Close;
          FreeAndNil(lDataset);
        end;
      end;
    end;
  except on E:Exception do HandleError(E,OPNAME) end;
end;

procedure TDataObjectRainfall.Set_ZoneChangeDate (Value : TDateTime);
const OPNAME = 'TDataObjectRainfall.Set_ZoneChangeDate';
var
  lDataset   : TAbstractModelDataset;
  lSQL       : string;
begin
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), lDataset);
    try
      if (lDataset <> nil) then
      begin
        lDataset.DataSet.Close;
        lSQL := 'UPDATE RainfallCatchment SET ChangeDate = :ADate ' +
                'WHERE RainfallCatchment.Model = :AModel ' +
                ' AND RainfallCatchment.StudyAreaName = :AStudyAreaName ' +
                ' AND RainfallCatchment.SubArea = :ASubArea ' +
                ' AND RainfallCatchment.Scenario = :AScenario ';
        lDataset.SetSQL(lSQL);
        lDataset.SetParams(['AModel','AStudyAreaName','ASubArea','AScenario',
          'ADate'],
          [FAppModules.StudyArea.ModelCode,
           FAppModules.StudyArea.StudyAreaCode,
           FAppModules.StudyArea.SubAreaCode,
           FAppModules.StudyArea.ScenarioCode,
           DateTimeToStr(Value)]);
        lDataset.ExecSQL();
      end;
    finally
      if Assigned(lDataset) then
      begin
        lDataset.DataSet.Close;
        FreeAndNil(lDataset);
      end;
    end;
  except on E:Exception do HandleError(E,OPNAME) end;
end;

function TDataObjectRainfall.GetViewDataItems (AViewId      : string;
                                               AItemsList   : TViewModelDataItemsList;
                                               var AHandled : boolean): boolean;
const OPNAME = 'TDataObjectRainfall.GetViewDataItems';
var
  LUpperViewId: string;
begin
  Result := False;//inherited GetViewDataItems(AViewId, AItemsList, AHandled);
  try

    if (not AHandled) then
    begin
      if (Trim(AViewId) <> '') and Assigned(AItemsList) then
      begin
        LUpperViewId := UpperCase(Trim(AViewId));
        if (Pos('MONTHLYRAW',LUpperViewId) = 1) then
          AHandled := GetRainfallStationControlViewDataItems(AViewId,AItemsList);
        Result := True;
      end;
    end;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDataObjectRainfall.GetRainfallStationControlViewDataItems(AViewID: string; AItemsList: TViewModelDataItemsList): boolean;
const OPNAME = 'TDataObjectRainfall.GetRainfallStationControlViewDataItems';
var
  LIndex             : integer;
  LViewModelDataItem : TViewModelDataItem;
  LStationData : TStationData;
Begin
  Result := False;
  try
    AItemsList.Reset;
    AViewID := UpperCase(Trim(AViewID));
    if (Trim(AViewID) <> '') and Assigned(AItemsList) then
    begin
      for LIndex := 0 to FStationDataList.Count - 1 do
      begin
        LStationData := CastStationDataByIndex(lIndex);
        if Assigned(LStationData) then
        begin
          LViewModelDataItem := AItemsList.AddViewModelDataItem;
          if Assigned(LViewModelDataItem) then
          begin
            LViewModelDataItem.Caption     := Trim(LStationData.RainfallData.StationNumber);
            LViewModelDataItem.Weighting   := LStationData.RainfallData.StationID;
            LViewModelDataItem.ParamNames  :=  'Identifier' ;
            LViewModelDataItem.ParamValues := IntToStr(LStationData.RainfallData.StationID);
            LViewModelDataItem.DataType    := 'MONTHLYRAW';
          end;
        end;
      end;
      Result := True;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


function TDataObjectRainfall.CreateReport : WideString;
const OPNAME = 'TDataObjectRainfall.CreateReport';
var
  lIndex            : integer;
  lStationID        : integer;
  lStation          : TStationData;
  lPatchID          : integer;
  lPatch            : TPatchData;
  lSector           : string;
  lPosition         : string;
  lLatStr           : WideString;
  lLonStr           : WideString;
  lLength           : integer;
  lMissing          : double;
  lDescr            : string;
  lLine             : string;
  lBreakPos         : integer;
  lFirstLine        : boolean;
  lName             : string;
  lDatasetA         : TAbstractModelDataset;
  lDatasetB         : TAbstractModelDataset;
  lSQL              : string;
  lScenario         : string;
  lDataList         : TStringList;
begin
  Result := '';
  try

    lDataList := TStringList.Create;
    try
      lDataList.Add('Model       : ' + FAppModules.Model.ModelName);
      lDataList.Add('--------------------------------------------------------------------------------');
      lDataList.Add('');
      lDataList.Add('Study       : ' + FAppModules.StudyArea.StudyAreaCode);
      lDataList.Add('--------------------------------------------------------------------------------');
      lDataList.Add('Name        : ' + FAppModules.StudyArea.StudyLabel);
      lDataList.Add('Client      : ' + FAppModules.StudyArea.Client);
      lDataList.Add('Consultant  : ' + FAppModules.StudyArea.Consultant);
      lDataList.Add('Study number: ' + FAppModules.StudyArea.StudyNumber);
      lDataList.Add('Study date  : ' + DateToStr(FAppModules.StudyArea.StudyDate));
      lDescr := FAppModules.StudyArea.StudyAreaDescription;
      lDescr := WrapText(lDescr, #13#10, [' ','-'], 66);
      lFirstLine := TRUE;
      while (Length(lDescr) > 0) do
      begin
        lBreakPos := Pos(#13#10, lDescr);
        if (lBreakPos > 0) then
          lLine := Copy(lDescr, 1, lBreakPos - 1)
        else
          lLine := lDescr;
        if (lFirstLine) then
        begin
          lDataList.Add('Description : ' + lLine);
          lFirstLine := FALSE;
        end
        else
          lDataList.Add('              ' + lLine);
        lDescr := Copy(lDescr, Length(lLine) + 3, Length(lDescr) - Length(lLine) - 2);
      end;
      lDataList.Add('');
      lDataList.Add('Sub Area    : ' + FAppModules.StudyArea.SubAreaCode);
      lDataList.Add('--------------------------------------------------------------------------------');
      lDataList.Add('Name        : ' + FAppModules.StudyArea.SubAreaLabel);
      lDescr := FAppModules.StudyArea.SubAreaDescription;
      lDescr := WrapText(lDescr, #13#10, [' ','-'], 66);
      lFirstLine := TRUE;
      while (Length(lDescr) > 0) do
      begin
        lBreakPos := Pos(#13#10, lDescr);
        if (lBreakPos > 0) then
          lLine := Copy(lDescr, 1, lBreakPos - 1)
        else
          lLine := lDescr;
        if (lFirstLine) then
        begin
          lDataList.Add('Description : ' + lLine);
          lFirstLine := FALSE;
        end
        else
          lDataList.Add('              ' + lLine);
        lDescr := Copy(lDescr, Length(lLine) + 3, Length(lDescr) - Length(lLine) - 2);
      end;
      lDataList.Add('');
      lDataList.Add('Rainfall Stations');
      lDataList.Add('-----------------------------------------------------------------------------------');
      lDataList.Add('Gauge      Station Name                    Lat    Long   Start End Len  %Mis  MAP  ');
      lDataList.Add('Number                                                   Year Year            (mm) ');
      lDataList.Add('-----------------------------------------------------------------------------------');

      for lIndex := 0 to FStationDataList.Count - 1 do
      begin
        lStation  := CastStationDataByIndex(lIndex);
        lSector   := Copy(lStation.RainfallData.StationNumber, 1, 4);
        lPosition := Copy(lStation.RainfallData.StationNumber, 5, 5);
        lStation.LatLong(lLatStr, lLonStr);
        lLength   := lStation.RainfallData.HydroEndYear - lStation.RainfallData.HydroStartYear + 1;
        lMissing  := lStation.RainfallData.NrOfMissingMonths * 100 / (lLength * 12);
        lName     := Trim(lStation.StationName);
        lDataList.Add(Format(
          '%4s %5s %-30s %6s %6s %4s %4s %3s %5.1f %6.1f ',
          [lSector, lPosition, lName, lLatStr, lLonStr, IntToStr(lStation.RainfallData.HydroStartYear),
          IntToStr(lStation.RainfallData.HydroEndYear), IntToStr(lLength), lMissing, lStation.RainfallData.MAP]));
      end;

      lDataList.Add('');
      lDataList.Add('Rainfall Zones');
      lDataList.Add('--------------------------------------------------------------------------------');

      FAppModules.Database.CreateDataset(integer(dtExecSQL), lDatasetA);
      FAppModules.Database.CreateDataset(integer(dtExecSQL), lDatasetB);
      try
        if (Assigned(lDatasetA) AND Assigned(lDatasetB)) then
        begin
          lDatasetA.DataSet.Close;
          lSQL := 'SELECT * FROM StudyScenario ' +
                  ' WHERE Model = '       + QuotedStr(FAppModules.StudyArea.ModelCode) +
                  ' AND StudyAreaName = ' + QuotedStr(FAppModules.StudyArea.StudyAreaCode) +
                  ' AND SubArea       = ' + QuotedStr(FAppModules.StudyArea.SubAreaCode);
          lDatasetA.SetSQL(lSQL);
          lDatasetA.DataSet.Open;
          lDatasetA.DataSet.First;
          while (NOT lDatasetA.DataSet.EOF) do
          begin
            lScenario := Trim(lDatasetA.DataSet.FieldByName('Scenario').AsString);
            if (lScenario <> 'Project Gauges') then
            begin
              lName  := Trim(lDatasetA.DataSet.FieldByName('ScenarioLabel').AsString);
              lDescr := Trim(lDatasetA.DataSet.FieldByName('ScenarioDescr').AsString);
              lDataList.Add('');
              lDataList.Add('Zone        : ' + lScenario);
              lDataList.Add('--------------------------------------------------------------------------------');
              lDataList.Add('Name        : ' + lName);
              lDescr := WrapText(lDescr, #13#10, [' ','-'], 66);
              lFirstLine := TRUE;
              while (Length(lDescr) > 0) do
              begin
                lBreakPos := Pos(#13#10, lDescr);
                if (lBreakPos > 0) then
                  lLine := Copy(lDescr, 1, lBreakPos - 1)
                else
                  lLine := lDescr;
                if (lFirstLine) then
                begin
                  lDataList.Add('Description : ' + lLine);
                  lFirstLine := FALSE;
                end
                else
                  lDataList.Add('              ' + lLine);
                lDescr := Copy(lDescr, Length(lLine) + 3, Length(lDescr) - Length(lLine) - 2);
              end;
              lDataList.Add('--------------------------------------------------------------------------------');
              lDataList.Add('Section Position  Type              MAP   Start End  Latitude Longitude');
              lDataList.Add('                                    (mm)  Year  Year                   ');
              lDataList.Add('--------------------------------------------------------------------------------');

              lDatasetB.DataSet.Close;
              lSQL := 'SELECT * FROM RainfallCatchmentSource WHERE Scenario = ' +
                      QuotedStr(lScenario);
              lDatasetB.SetSQL(lSQL);
              lDatasetB.DataSet.Open;
              lDatasetB.DataSet.First;
              while (NOT lDatasetB.DataSet.EOF) do
              begin
                lStationID := lDatasetB.DataSet.FieldByName('StationID').AsInteger;
                lPatchID   := lDatasetB.DataSet.FieldByName('SourcePatchID').AsInteger;
                lStation   := CastStationDataByID(lStationID);
                if (lStation <> nil) then
                begin
                  lPatch := nil;
                  if (lPatchID <> 0) then
                    lPatch := lStation.CastPatchWithID(lPatchID);
                  lSector   := Copy(lStation.RainfallData.StationNumber, 1, 4);
                  lPosition := Copy(lStation.RainfallData.StationNumber, 5, 5);
                  lStation.LatLong(lLatStr, lLonStr);
                  if (lPatch <> nil) then
                    lDataList.Add(Format(
                      '  %4s    %5s    %-15s %6.1f  %4s  %4s  %6s   %6s',
                      [lSector, lPosition, lPatch.PatchName, lPatch.RainfallData.MAP,
                      IntToStr(lPatch.RainfallData.HydroStartYear),
                      IntToStr(lPatch.RainfallData.HydroEndYear), lLatStr, lLonStr]))
                  else
                    lDataList.Add(Format(
                      '  %4s    %3s    %-15s %6.1f  %4s  %4s  %6s   %6s',
                      [lSector, lPosition, 'RAW', lStation.RainfallData.MAP, IntToStr(lStation.RainfallData.HydroStartYear),
                      IntToStr(lStation.RainfallData.HydroEndYear), lLatStr, lLonStr]));
                end;
                lDatasetB.DataSet.Next;
              end;
            end;
            lDatasetA.DataSet.Next;
          end;
        end;
      finally
        lDatasetA.Free;
        lDatasetB.Free;
      end;
      Result := lDataList.CommaText;
    finally
      FreeAndNil(lDataList);
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TDataObjectRainfall.GetStationDailyData (AStationID : Integer;
                                                   AData      : TStringList);

const OPNAME = 'TDataObjectRainfall.GetStationDailyData';
var
  LDecompressionStream : TZDecompressionStream;
  LBlobStream          : TStream;
  lDataset             : TAbstractModelDataset;
  LErrorFree           : boolean;
begin
  try
    AData.Clear;
    LErrorFree := FAppModules.Database.CreateDataset(integer(dtExecSQL), lDataset);
    try
      if LErrorFree then
      begin
        lDataset.DataSet.Close;
        lDataset.SetSQL('SELECT StationID, RainfallData FROM RainfallDailyData ' +
                        'WHERE StationID = :AStationID' );

        lDataset.SetParams(['AStationID'], [IntToStr(AStationID)]);
        lDataset.Dataset.Open;

        if not lDataset.DataSet.Eof then
        begin
          LBlobStream := lDataset.Dataset.CreateBlobStream(lDataset.Dataset.FieldByName('RainfallData'), bmRead);
          LDecompressionStream := TZDecompressionStream.Create(LBlobStream);
          try
            AData.LoadFromStream(LDecompressionStream);
          finally
            FreeAndNil(LBlobStream);
            FreeAndNil(LDecompressionStream);
          end;
        end;
      end;
    finally
      if Assigned(lDataset) then
      begin
        lDataset.Dataset.Close;
        FreeAndNil(lDataset);
      end;
    end;
  except on E:Exception do HandleError(E,OPNAME) end;
end;

procedure TDataObjectRainfall.GetDailyDataByMonthYear (ADailyData    : TStringList;
                                                       AYear, AMonth : integer );
const OPNAME = 'TDataObjectRainfall.GetDailyDataByMonthYear';
var
  lIndex : integer;
  lYear  : integer;
  lMonth : integer;
begin
  try
    lIndex := 0;
    while (lIndex < ADailyData.Count) do
    begin
      lYear  := StrToInt(Trim(Copy(ADailyData[lIndex], 11, 4)));
      lMonth := StrToInt(Trim(Copy(ADailyData[lIndex], 15, 2)));
      if ((AYear = lYear) AND (AMonth = lMonth)) then
        lIndex := lIndex + 1
      else
        ADailyData.Delete(lIndex)
     end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDataObjectRainfall.GetDailyDataByMonthAndYear (AStationID : integer;
                                                         APatchID   : integer;
                                                         AMonth     : integer;
                                                         AYear      : integer) : WideString;
const OPNAME = 'TDataObjectRainfall.GetDailyDataByMonthAndYear';
var
  lIndexA             : integer;
  lIndexB             : integer;
  lDailyData          : TStringlist;
  lStation            : TStationData;
  lPatch              : TPatchData;
  lSourcePatchID      : integer;
  lSourceStationID    : integer;
  lTargetStation      : WideString;
  lDataList           : TStringList;
  lStartYear          : integer;
  lEndYear            : integer;
begin
  Result := '';
  try
    lStation := CastStationDataByID(AStationID);
    if (lStation <> nil) then
    begin
      lDataList  := TStringList.Create;
      lDailyData := TStringlist.Create;
      try
        if (Assigned(lDataList) AND Assigned(lDailyData)) then
        begin
          GetStationDailyData(lStation.RainfallData.StationID, lDailyData);
          GetDailyDataByMonthYear(lDailyData, AYear, AMonth);
          for lIndexB := 0 to lDailyData.Count - 1 do
            lDataList.Add(lDailyData[lIndexB]);
          if (APatchID <> 0) then
          begin
            lPatch := lStation.CastPatchWithID(APatchID);
            if (lPatch <> nil) then
            begin
              for lIndexA := 0 to lPatch.SourcesCount - 1 do
              begin
                lPatch.GetSourceInfoByIndex(lIndexA, lSourceStationID, lSourcePatchID,
                                            lTargetStation, lStartYear, lEndYear);
                GetStationDailyData(lSourceStationID, lDailyData);
                GetDailyDataByMonthYear(lDailyData, AYear, AMonth);
                for lIndexB := 0 to lDailyData.Count - 1 do
                  lDataList.Add(lDailyData[lIndexB]);
              end;
            end;
          end;
        end;
        Result := lDataList.CommaText;
      finally
        FreeAndNil(lDataList);
        FreeAndNil(lDailyData);
      end;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TDataObjectRainfall.LatLong (const AStationNumber : WideString;
                                       var ALat             : double;
                                       var ALong            : double);
const OPNAME = 'TDataObjectRainfall.LatLong';
const
  lLHR  : array [1..37] of integer =
          (   1,   4,  20,  39,  60,  83, 106, 130, 156, 184, 212, 242,
            273, 306, 341, 377, 413, 449, 486, 523, 562, 604, 647, 691,
            734, 778, 822, 866, 910, 955, 999,1045,1092,1141,1192,1244,1296);
  lPHIW : array [1..36] of integer =
          (1140,1080,1080,1050,1050,1080,1080,1050,1020,1020, 990, 990,
            960, 930, 900, 900, 900, 870, 870 ,870, 870, 840, 840, 840,
            840, 840, 810, 810, 780, 780, 750, 750, 720, 690, 690, 690);
var
  lIndex   : integer;
  lSec     : integer;
  lPos     : integer;
  lLats    : integer;
  lLongs   : integer;
begin
  try
    ALat  := 0;
    ALong := 0;
    lSec := StrToInt(Trim(Copy(AStationNumber, 1, 4)));
    lPos := StrToInt(Trim(Copy(AStationNumber, 5, 3)));
    lIndex := 1;
    while (lSec >= lLHR[lIndex]) do
      lIndex := lIndex + 1;
    lLats    := 2130 - (lIndex * 30);
    lLongs   := lPHIW[lIndex-1] + (lSec - lLHR[lIndex-1]) * 30;
    ALong    := Trunc(lLongs + lPos / 30.0 + 0.97);
    ALat     := lLats + lPos + 30 - (ALong - lLongs) * 30;
    ALong    := ALong / 60;
    ALat     := ALat / 60 * (-1); // Can only convert South East station numbers
  except on E:Exception do HandleError(E,OPNAME) end;
end;

function TDataObjectRainfall.GetZoneStations : WideString;
const OPNAME = 'TDataObjectRainfall.GetZoneStations';
begin
  Result := '';
  try
    if Assigned(FZoneStations) then
      Result := FZoneStations.CommaText;
  except on E:Exception do HandleError(E,OPNAME) end;
end;

function TDataObjectRainfall.GetZoneCount : integer;
const OPNAME = 'TDataObjectRainfall.GetZoneCount';
begin
  Result := 0;
  try
    if Assigned(FZoneStations) then
      Result := FZoneStations.Count;
  except on E:Exception do HandleError(E,OPNAME) end;
end;

function TDataObjectRainfall.Get_CatchmentFileName : WideString;
const OPNAME = 'TDataObjectRainfall.Get_CatchmentFileName';
var
  lDataset   : TAbstractModelDataset;
  LErrorFree : boolean;
begin
  Result := '';
  try
    LErrorFree := FAppModules.Database.CreateDataset(integer(dtExecSQL), lDataset);
    try
      if LErrorFree then
      begin
        lDataset.DataSet.Close;
        lDataset.SetSQL(
          '  SELECT CatchmentFileName FROM RainfallCatchment             ' +
          '  WHERE                                                       ' +
          '    RainfallCatchment.Model = :AModel                     ' +
          '    AND RainfallCatchment.StudyAreaName = :AStudyAreaName ' +
          '    AND RainfallCatchment.SubArea = :ASubArea             ' +
          '    AND RainfallCatchment.Scenario = :AScenario           ' );
        lDataset.SetParams(['AModel','AStudyAreaName','ASubArea','AScenario'],
          [FAppModules.StudyArea.ModelCode,
           FAppModules.StudyArea.StudyAreaCode,
           FAppModules.StudyArea.SubAreaCode,
           FAppModules.StudyArea.ScenarioCode]);
        lDataset.DataSet.Open;

        if not lDataset.DataSet.Eof then
          Result := Trim(lDataset.DataSet.FieldByName('CatchmentFileName').AsString);
      end; {if}
    finally
      if Assigned(lDataset) then
      begin
        lDataset.DataSet.Close;
        FreeAndNil(lDataset);
      end;
    end;
  except on E:Exception do HandleError(E,OPNAME) end;
end;

procedure TDataObjectRainfall.Set_CatchmentFileName(const Value: WideString);
const OPNAME = 'TDataObjectRainfall.Set_CatchmentFileName';
var
  lDataset   : TAbstractModelDataset;
  LErrorFree : boolean;
begin
  try
    LErrorFree := FAppModules.Database.CreateDataset(integer(dtExecSQL), lDataset);
    try
      if LErrorFree then
      begin
        lDataset.DataSet.Close;
        lDataset.SetSQL(
          '  UPDATE  RainfallCatchment                               ' +
          '  SET                                                         ' +
          '    CatchmentFileName = :ACatchmentCode                           ' +
          '  WHERE                                                       ' +
          '    RainfallCatchment.Model = :AModel                     ' +
          '    AND RainfallCatchment.StudyAreaName = :AStudyAreaName ' +
          '    AND RainfallCatchment.SubArea = :ASubArea             ' +
          '    AND RainfallCatchment.Scenario = :AScenario           ' );
        lDataset.SetParams(['AModel','AStudyAreaName','ASubArea','AScenario',
          'ACatchmentCode'],
          [FAppModules.StudyArea.ModelCode,
           FAppModules.StudyArea.StudyAreaCode,
           FAppModules.StudyArea.SubAreaCode,
           FAppModules.StudyArea.ScenarioCode,
           Value]);
        lDataset.ExecSQL();
      end;
    finally
      if Assigned(lDataset) then
      begin
        lDataset.DataSet.Close;
        FreeAndNil(lDataset);
      end;
    end;
  except on E:Exception do HandleError(E,OPNAME) end;
end;

function TDataObjectRainfall.RemoveFromRainfallZone (AStationID : integer;
                                                     APatchID   : integer) : WordBool;
const OPNAME = 'TDataObjectRainfall.RemoveFromRainfallZone';
var
  lDataset   : TAbstractModelDataset;
  LErrorFree : boolean;
begin
  Result := False;
  try
    LErrorFree := FAppModules.Database.CreateDataset(integer(dtExecSQL), lDataset);
    try
      if LErrorFree then
      begin
        lDataset.DataSet.Close;
        lDataset.SetSQL(
          '  DELETE FROM RainfallCatchmentSource                     ' +
          '  WHERE                                                       ' +
          '    Model = :AModel                                           ' +
          '    AND StudyAreaName = :AStudyAreaName                       ' +
          '    AND SubArea = :ASubArea                                   ' +
          '    AND Scenario = :AScenario                                 ' +
          '    AND SourcePatchID = :ASourcePatchID                       ' +
          '    AND StationID = :AStationID');

        lDataset.SetParams(['AModel','AStudyAreaName','ASubArea','AScenario',
          'ASourcePatchID', 'AStationID'],
          [FAppModules.StudyArea.ModelCode, FAppModules.StudyArea.StudyAreaCode, FAppModules.StudyArea.SubAreaCode, FAppModules.StudyArea.ScenarioCode,
           IntToStr(APatchID), IntToStr(AStationId)]);
        lDataset.ExecSQL;
      end;
      ZoneChangeDate := Now;
      LoadZones;
      Result := True;
    finally
      if Assigned(lDataset) then
      begin
        lDataset.Dataset.Close;
        FreeAndNil(lDataset);
      end;
    end;
  except on E:Exception do HandleError(E,OPNAME) end;
end;

function TDataObjectRainfall.DeleteFromRainfallZone (AStationID : integer;
                                                     APatchID   : integer; ACatchmentZone : TCatchmentZone) : WordBool;
const OPNAME = 'TDataObjectRainfall.DeleteFromRainfallZone';
var
  lDataset   : TAbstractModelDataset;
  LErrorFree : boolean;
begin
  Result := False;
  try
    if (ACatchmentZone <> nil) then
    begin
      LErrorFree := FAppModules.Database.CreateDataset(integer(dtExecSQL), lDataset);
      try
        if LErrorFree then
        begin
          lDataset.DataSet.Close;
          lDataset.SetSQL(
            '  DELETE FROM RainfallCatchmentSource                     ' +
            '  WHERE                                                       ' +
            '    Model = :AModel                                           ' +
            '    AND StudyAreaName = :AStudyAreaName                       ' +
            '    AND SubArea = :ASubArea                                   ' +
            '    AND Scenario = :AScenario                                 ' +
            '    AND CatchmentID = :ACatchmentID                       ' +
            '    AND SourcePatchID = :ASourcePatchID                       ' +
            '    AND StationID = :AStationID');

          lDataset.SetParams(['AModel','AStudyAreaName','ASubArea','AScenario',
            'ACatchmentID','ASourcePatchID', 'AStationID'],
            [FAppModules.StudyArea.ModelCode, FAppModules.StudyArea.StudyAreaCode, FAppModules.StudyArea.SubAreaCode, FAppModules.StudyArea.ScenarioCode,
             IntToStr(ACatchmentZone.CatchmentID),IntToStr(APatchID), IntToStr(AStationId)]);
          lDataset.ExecSQL;
        end;
        ZoneChangeDate := Now;
        ACatchmentZone.DeleteCatchmentSource(AStationId,APatchID);
        Result := True;
      finally
        if Assigned(lDataset) then
        begin
          lDataset.Dataset.Close;
          FreeAndNil(lDataset);
        end;
      end;
    end;
  except on E:Exception do HandleError(E,OPNAME) end;
end;


function TDataObjectRainfall.Get_ZoneChangeDate : TDateTime;
const OPNAME = 'TDataObjectRainfall.Get_ZoneChangeDate';
var
  lDataset   : TAbstractModelDataset;
  lSQL       : string;
begin
  Result := 0;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), lDataset);
    try
      if (lDataset <> nil) then
      begin
        lDataset.DataSet.Close;
        lSQL := 'SELECT ChangeDate FROM RainfallCatchment ' +
                'WHERE RainfallCatchment.Model = '        + QuotedStr(FAppModules.StudyArea.ModelCode) +
                ' AND RainfallCatchment.StudyAreaName = ' + QuotedStr(FAppModules.StudyArea.StudyAreaCode) +
                ' AND RainfallCatchment.SubArea = '       + QuotedStr(FAppModules.StudyArea.SubAreaCode) +
                ' AND RainfallCatchment.Scenario = '      + QuotedStr(FAppModules.StudyArea.ScenarioCode);
        lDataset.SetSQL(lSQL);
        lDataset.DataSet.Open;
        if (NOT lDataset.DataSet.Eof) then
          Result := lDataset.DataSet.FieldByName('ChangeDate').AsDateTime;
      end;
    finally
      if Assigned(lDataset) then
      begin
        lDataset.DataSet.Close;
        FreeAndNil(lDataset);
      end;
    end;
  except on E:Exception do HandleError(E,OPNAME) end;
end;

function TDataObjectRainfall.Get_ZoneRunDate : TDateTime;
const OPNAME = 'TDataObjectRainfall.Get_ZoneRunDate';
var
  lDataset   : TAbstractModelDataset;
  lSQL       : string;
begin
  Result := 0;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), lDataset);
    try
      if (lDataset <> nil) then
      begin
        lDataset.DataSet.Close;
        lSQL := 'SELECT RunDate FROM RainfallCatchment ' +
                'WHERE RainfallCatchment.Model = '        + QuotedStr(FAppModules.StudyArea.ModelCode) +
                ' AND RainfallCatchment.StudyAreaName = ' + QuotedStr(FAppModules.StudyArea.StudyAreaCode) +
                ' AND RainfallCatchment.SubArea = '       + QuotedStr(FAppModules.StudyArea.SubAreaCode) +
                ' AND RainfallCatchment.Scenario = '      + QuotedStr(FAppModules.StudyArea.ScenarioCode);
        lDataset.SetSQL(lSQL);
        lDataset.DataSet.Open;
        if (NOT lDataset.DataSet.Eof) then
          Result := lDataset.DataSet.FieldByName('RunDate').AsDateTime;
      end;
    finally
      if Assigned(lDataset) then
      begin
        lDataset.DataSet.Close;
        FreeAndNil(lDataset);
      end;
    end;
  except on E:Exception do HandleError(E,OPNAME) end;
end;

procedure TDataObjectRainfall.Set_ZoneRunDate (Value : TDateTime);
const OPNAME = 'TDataObjectRainfall.Set_ZoneRunDate';
var
  lDataset   : TAbstractModelDataset;
  lSQL       : string;
begin
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), lDataset);
    try
      if (lDataset <> nil) then
      begin
        lDataset.DataSet.Close;
        lSQL := 'UPDATE RainfallCatchment SET RunDate = :ADate ' +
                'WHERE RainfallCatchment.Model = :AModel ' +
                ' AND RainfallCatchment.StudyAreaName = :AStudyAreaName ' +
                ' AND RainfallCatchment.SubArea = :ASubArea ' +
                ' AND RainfallCatchment.Scenario = :AScenario ';
        lDataset.SetSQL(lSQL);
        lDataset.SetParams(['AModel','AStudyAreaName','ASubArea','AScenario',
          'ADate'],
          [FAppModules.StudyArea.ModelCode,
           FAppModules.StudyArea.StudyAreaCode,
           FAppModules.StudyArea.SubAreaCode,
           FAppModules.StudyArea.ScenarioCode,
           DateTimeToStr(Value)]);
        lDataset.ExecSQL();
      end;
    finally
      if Assigned(lDataset) then
      begin
        lDataset.DataSet.Close;
        FreeAndNil(lDataset);
      end;
    end;
  except on E:Exception do HandleError(E,OPNAME) end;
end;

function TDataObjectRainfall.Get_OutputFileName: WideString;
const OPNAME = 'TDataObjectRainfall.Get_OutputFileName';
var
  lDataset   : TAbstractModelDataset;
  LErrorFree : boolean;
begin
  Result := '';
  try
    LErrorFree := FAppModules.Database.CreateDataset(integer(dtExecSQL), lDataset);
    try
      if LErrorFree then
      begin
        lDataset.DataSet.Close;
        lDataset.SetSQL(
          '  SELECT OutputFileName FROM RainfallCatchment             ' +
          '  WHERE                                                       ' +
          '    RainfallCatchment.Model = :AModel                     ' +
          '    AND RainfallCatchment.StudyAreaName = :AStudyAreaName ' +
          '    AND RainfallCatchment.SubArea = :ASubArea             ' +
          '    AND RainfallCatchment.Scenario = :AScenario           ' );
        lDataset.SetParams(['AModel','AStudyAreaName','ASubArea','AScenario'],
          [FAppModules.StudyArea.ModelCode,
           FAppModules.StudyArea.StudyAreaCode,
           FAppModules.StudyArea.SubAreaCode,
           FAppModules.StudyArea.ScenarioCode]);
        lDataset.DataSet.Open;

        if not lDataset.DataSet.Eof then
          Result := Trim(lDataset.DataSet.FieldByName('OutputFileName').AsString);
      end;
    finally
      if Assigned(lDataset) then
      begin
        lDataset.DataSet.Close;
        FreeAndNil(lDataset);
      end;
    end;
  except on E:Exception do HandleError(E,OPNAME) end;
end;

procedure TDataObjectRainfall.Set_OutputFileName(const Value: WideString);
const OPNAME = 'TDataObjectRainfall.Set_OutputFileName';
var
  lDataset   : TAbstractModelDataset;
  LErrorFree : boolean;
begin
  try
    LErrorFree := FAppModules.Database.CreateDataset(integer(dtExecSQL), lDataset);
    try
      if LErrorFree then
      begin
        lDataset.DataSet.Close;
        lDataset.SetSQL(
          '  UPDATE  RainfallCatchment                               ' +
          '  SET                                                         ' +
          '    OutputFileName = :ACatchmentFile                           ' +
          '  WHERE                                                       ' +
          '    RainfallCatchment.Model = :AModel                     ' +
          '    AND RainfallCatchment.StudyAreaName = :AStudyAreaName ' +
          '    AND RainfallCatchment.SubArea = :ASubArea             ' +
          '    AND RainfallCatchment.Scenario = :AScenario           ' );
        lDataset.SetParams(['AModel','AStudyAreaName','ASubArea','AScenario',
          'ACatchmentFile'],
          [FAppModules.StudyArea.ModelCode,
           FAppModules.StudyArea.StudyAreaCode,
           FAppModules.StudyArea.SubAreaCode,
           FAppModules.StudyArea.ScenarioCode,
           Value]);
        lDataset.ExecSQL();
      end;
    finally
      if Assigned(lDataset) then
      begin
        lDataset.DataSet.Close;
        FreeAndNil(lDataset);
      end;
    end;
  except on E:Exception do HandleError(E,OPNAME) end;
end;

function TDataObjectRainfall.Get_CatchmentFileData : WideString;
const OPNAME = 'TDataObjectRainfall.Get_CatchmentFileData';
var
  lDataset      : TAbstractModelDataset;
  LErrorFree    : boolean;
  LBlobStream   : TStream;
  lDataList     : TStringList;
begin
  Result := '';
  try
    LErrorFree := FAppModules.Database.CreateDataset(integer(dtExecSQL), lDataset);
    try
      if LErrorFree then
      begin
        lDataset.DataSet.Close;
        lDataset.SetSQL('  SELECT CatchmentFileData ' +
                               '  FROM                                                          ' +
                               '    RainfallCatchment                                       ' +
                               '  WHERE                                                         ' +
                               '    RainfallCatchment.Model = :AModel                       ' +
                               '    AND RainfallCatchment.StudyAreaName = :AStudyAreaName   ' +
                               '    AND RainfallCatchment.SubArea = :ASubArea               ' +
                               '    AND RainfallCatchment.Scenario = :AScenario             ' );

        lDataset.SetParams(['AModel','AStudyAreaName','ASubArea','AScenario'],
          [FAppModules.StudyArea.ModelCode,
           FAppModules.StudyArea.StudyAreaCode,
           FAppModules.StudyArea.SubAreaCode,
           FAppModules.StudyArea.ScenarioCode]);
        lDataset.Dataset.Open;

        if not lDataset.DataSet.Eof then
        begin
          LBlobStream := lDataset.Dataset.CreateBlobStream(lDataset.Dataset.FieldByName('CatchmentFileData'), bmRead);
          lDataList   := TStringList.Create;
          try
            lDataList.LoadFromStream(LBlobStream);
            Result := lDataList.CommaText;
          finally
            FreeAndNil(LBlobStream);
            FreeAndNil(lDataList);
          end;
        end;
      end;
    finally
      if Assigned(lDataset) then
      begin
        lDataset.Dataset.Close;
        FreeAndNil(lDataset);
      end;
    end;
  except on E:Exception do HandleError(E,OPNAME) end;
end;

function TDataObjectRainfall.Get_OutputFileData : WideString;
const OPNAME = 'TDataObjectRainfall.Get_OutputFileData';
var
  lDataset      : TAbstractModelDataset;
  LErrorFree    : boolean;
  LBlobStream   : TStream;
  lDataList     : TStringList;
begin
  Result := '';
  try
    LErrorFree := FAppModules.Database.CreateDataset(integer(dtExecSQL), lDataset);
    try
      if LErrorFree then
      begin
        lDataset.DataSet.Close;
        lDataset.SetSQL('  SELECT OutputFileData                                          ' +
                               '  FROM                                                          ' +
                               '    RainfallCatchment                                       ' +
                               '  WHERE                                                         ' +
                               '    RainfallCatchment.Model = :AModel                       ' +
                               '    AND RainfallCatchment.StudyAreaName = :AStudyAreaName   ' +
                               '    AND RainfallCatchment.SubArea = :ASubArea               ' +
                               '    AND RainfallCatchment.Scenario = :AScenario             ' );

        lDataset.SetParams(['AModel','AStudyAreaName','ASubArea','AScenario'],
          [FAppModules.StudyArea.ModelCode,
           FAppModules.StudyArea.StudyAreaCode,
           FAppModules.StudyArea.SubAreaCode,
           FAppModules.StudyArea.ScenarioCode]);
        lDataset.Dataset.Open;

        if not lDataset.DataSet.Eof then
        begin
          LBlobStream := lDataset.Dataset.CreateBlobStream(lDataset.Dataset.FieldByName('OutputFileData'), bmRead);
          lDataList   := TStringList.Create;
          try
            lDataList.LoadFromStream(LBlobStream);
            Result := lDataList.CommaText;
          finally
            FreeAndNil(LBlobStream);
            FreeAndNil(lDataList);
          end;
        end;
      end;
    finally
      if Assigned(lDataset) then
      begin
        lDataset.Dataset.Close;
        FreeAndNil(lDataset);
      end;
    end;
  except on E:Exception do HandleError(E,OPNAME) end;
end;

procedure TDataObjectRainfall.Set_CatchmentFileData (const Value: WideString);
const OPNAME = 'TDataObjectRainfall.Set_CatchmentFileData';
var
  lDataset      : TAbstractModelDataset;
  LErrorFree    : boolean;
  LBlobStream   : TStream;
  lDataList     : TStringList;
begin
  try
    LErrorFree := FAppModules.Database.CreateDataset(integer(dtExecSQL), lDataset);
    try
      if LErrorFree then
      begin
        lDataset.DataSet.Close;
        lDataset.SetSQL('  SELECT * ' +
                               '  FROM                                                          ' +
                               '    RainfallCatchment                                       ' +
                               '  WHERE                                                         ' +
                               '    RainfallCatchment.Model = :AModel                       ' +
                               '    AND RainfallCatchment.StudyAreaName = :AStudyAreaName   ' +
                               '    AND RainfallCatchment.SubArea = :ASubArea               ' +
                               '    AND RainfallCatchment.Scenario = :AScenario             ' );

        lDataset.SetParams(['AModel','AStudyAreaName','ASubArea','AScenario'],
          [FAppModules.StudyArea.ModelCode,
           FAppModules.StudyArea.StudyAreaCode,
           FAppModules.StudyArea.SubAreaCode,
           FAppModules.StudyArea.ScenarioCode]);
        lDataset.SetReadOnly(False);
        lDataset.Dataset.Open;

        if not lDataset.DataSet.Eof then
        begin
          lDataset.DataSet.Edit;
          LBlobStream := lDataset.Dataset.CreateBlobStream(lDataset.Dataset.FieldByName('CatchmentFileData'), bmWrite);
          lDataList   := TStringList.Create;
          try
            lDataList.CommaText := Value;
            lDataList.SaveToStream(LBlobStream);
          finally
            FreeAndNil(LBlobStream);
            FreeAndNil(lDataList);
          end;
          lDataset.DataSet.Post;
        end;
      end;
    finally
      if Assigned(lDataset) then
      begin
        lDataset.Dataset.Close;
        FreeAndNil(lDataset);
      end;
    end;
  except on E:Exception do HandleError(E,OPNAME) end;
end;

procedure TDataObjectRainfall.Set_OutputFileData (const Value : WideString);
const OPNAME = 'TDataObjectRainfall.Set_OutputFileData';
var
  lDataset      : TAbstractModelDataset;
  LErrorFree    : boolean;
  LBlobStream   : TStream;
  lDataList     : TStringList;
begin
  try
    LErrorFree := FAppModules.Database.CreateDataset(integer(dtExecSQL), lDataset);
    try
      if LErrorFree then
      begin
        lDataset.DataSet.Close;
        lDataset.SetSQL('  SELECT *                                          ' +
                               '  FROM                                                          ' +
                               '    RainfallCatchment                                       ' +
                               '  WHERE                                                         ' +
                               '    RainfallCatchment.Model = :AModel                       ' +
                               '    AND RainfallCatchment.StudyAreaName = :AStudyAreaName   ' +
                               '    AND RainfallCatchment.SubArea = :ASubArea               ' +
                               '    AND RainfallCatchment.Scenario = :AScenario             ' );

        lDataset.SetParams(['AModel','AStudyAreaName','ASubArea','AScenario'],
          [FAppModules.StudyArea.ModelCode,
           FAppModules.StudyArea.StudyAreaCode,
           FAppModules.StudyArea.SubAreaCode,
           FAppModules.StudyArea.ScenarioCode]);
        lDataset.SetReadOnly(False);
        lDataset.Dataset.Open;

        if not lDataset.DataSet.Eof then
        begin
          lDataset.DataSet.Edit;
          LBlobStream := lDataset.Dataset.CreateBlobStream(lDataset.Dataset.FieldByName('OutputFileData'), bmWrite);
          lDataList   := TStringList.Create;
          try
            lDataList.CommaText := Value;
            lDataList.SaveToStream(LBlobStream);
          finally
            FreeAndNil(LBlobStream);
            FreeAndNil(lDataList);
          end;
          lDataset.DataSet.Post;
        end;
      end;
    finally
      if Assigned(lDataset) then
      begin
        lDataset.Dataset.Close;
        FreeAndNil(lDataset);
      end;
    end;
  except on E:Exception do HandleError(E,OPNAME) end;
end;

function TDataObjectRainfall.Get_RAWFlags : WideString;
const OPNAME = 'TDataObjectRainfall.Get_RAWFlags';
begin
  Result := '';
  try
    Result := FRAWFlags.CommaText;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDataObjectRainfall.Set_RAWFlags (const Value: WideString);
const OPNAME = 'TDataObjectRainfall.Set_RAWFlags';
var
  lDataset : TAbstractModelDataset;
  lSQL     : string;
begin
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), lDataset);
    try
      if Assigned(lDataset) then
      begin
        lSQL := 'UPDATE RainfallRAWFlags SET RAWFlags = ' + QuotedStr(Value);
        lDataset.DataSet.Close;
        lDataset.SetSQL(lSQL);
        lDataset.ExecSQL;
        FRAWFlags.CommaText := Value;
      end;
    finally
      if Assigned(lDataset) then
      begin
        lDataset.DataSet.Close;
        FreeAndNil(lDataset);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDataObjectRainfall.Get_HighlightWetSeasonZeros : WordBool;
const OPNAME = 'TDataObjectRainfall.Get_HighlightWetSeasonZeros';
begin
  Result := FALSE;
  try
    Result := FHighlightWetSeasonZeros;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDataObjectRainfall.Set_HighlightWetSeasonZeros(Value: WordBool);
const OPNAME = 'TDataObjectRainfall.Set_HighlightWetSeasonZeros';
var
  lDataset : TAbstractModelDataset;
  lSQL     : string;
begin
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), lDataset);
    try
      if Assigned(lDataset) then
      begin
        lSQL := 'UPDATE RainfallRAWFlags SET WetSeasonZeros = ';
        if (Value) then
          lSQL := lSQL + QuotedStr('Y')
        else
          lSQL := lSQL + QuotedStr('N');
        lDataset.DataSet.Close;
        lDataset.SetSQL(lSQL);
        lDataset.ExecSQL;
        FHighlightWetSeasonZeros := Value;
      end;
    finally
      if Assigned(lDataset) then
      begin
        lDataset.DataSet.Close;
        FreeAndNil(lDataset);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDataObjectRainfall.Get_WetSeasonMonths: WideString;
const OPNAME = 'TDataObjectRainfall.Get_WetSeasonMonths';
begin
  Result := '';
  try
    Result := FWetSeasonMonths;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDataObjectRainfall.Set_WetSeasonMonths(const Value: WideString);
const OPNAME = 'TDataObjectRainfall.Set_WetSeasonMonths';
var
  lDataset : TAbstractModelDataset;
  lSQL     : string;
begin
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), lDataset);
    try
      if Assigned(lDataset) then
      begin
        lSQL := 'UPDATE RainfallRAWFlags SET WetSeasonMonths = ' + QuotedStr(Value);
        lDataset.DataSet.Close;
        lDataset.SetSQL(lSQL);
        lDataset.ExecSQL;
        FWetSeasonMonths := Value;
      end;
    finally
      if Assigned(lDataset) then
      begin
        lDataset.DataSet.Close;
        FreeAndNil(lDataset);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDataObjectRainfall.Get_HighlightMonthlyGreaterThanProportion: WordBool;
const OPNAME = 'TDataObjectRainfall.Get_HighlightMonthlyGreaterThanProportion';
begin
  Result := FALSE;
  try
    Result := FHighlightMonthlyGreaterThanProportion;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDataObjectRainfall.Set_HighlightMonthlyGreaterThanProportion(Value: WordBool);
const OPNAME = 'TDataObjectRainfall.Set_HighlightMonthlyGreaterThanProportion';
var
  lDataset : TAbstractModelDataset;
  lSQL     : string;
begin
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), lDataset);
    try
      if Assigned(lDataset) then
      begin
        lSQL := 'UPDATE RainfallRAWFlags SET MonthlyGreaterProportion = ';
        if (Value) then
          lSQL := lSQL + QuotedStr('Y')
        else
          lSQL := lSQL + QuotedStr('N');
        lDataset.DataSet.Close;
        lDataset.SetSQL(lSQL);
        lDataset.ExecSQL;
        FHighlightMonthlyGreaterThanProportion := Value;
      end;
    finally
      if Assigned(lDataset) then
      begin
        lDataset.DataSet.Close;
        FreeAndNil(lDataset);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDataObjectRainfall.Get_MonthlyGreaterThanProportionValue: Double;
const OPNAME = 'TDataObjectRainfall.Get_MonthlyGreaterThanProportionValue';
begin
  Result := 0;
  try
    Result := FMonthlyGreaterThanProportionValue;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDataObjectRainfall.Set_MonthlyGreaterThanProportionValue(Value: Double);
const OPNAME = 'TDataObjectRainfall.Set_MonthlyGreaterThanProportionValue';
var
  lDataset : TAbstractModelDataset;
  lSQL     : string;
begin
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), lDataset);
    try
      if Assigned(lDataset) then
      begin
        lSQL := 'UPDATE RainfallRAWFlags SET MonthlyProportionValue = ' +
                FloatToStrF(Value, ffFixed, 10, 2);
        lDataset.DataSet.Close;
        lDataset.SetSQL(lSQL);
        lDataset.ExecSQL;
        FMonthlyGreaterThanProportionValue := Value;
      end;
    finally
      if Assigned(lDataset) then
      begin
        lDataset.DataSet.Close;
        FreeAndNil(lDataset);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDataObjectRainfall.Get_HighlightAnnualGreaterThanAverage: WordBool;
const OPNAME = 'TDataObjectRainfall.Get_HighlightAnnualGreaterThanAverage';
begin
  Result := FALSE;
  try
    Result := FHighlightAnnualGreaterThanAverage;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDataObjectRainfall.Set_HighlightAnnualGreaterThanAverage(Value: WordBool);
const OPNAME = 'TDataObjectRainfall.Set_HighlightAnnualGreaterThanAverage';
var
  lDataset : TAbstractModelDataset;
  lSQL     : string;
begin
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), lDataset);
    try
      if Assigned(lDataset) then
      begin
        lSQL := 'UPDATE RainfallRAWFlags SET AnnualGreater = ';
        if (Value) then
          lSQL := lSQL + QuotedStr('Y')
        else
          lSQL := lSQL + QuotedStr('N');
        lDataset.DataSet.Close;
        lDataset.SetSQL(lSQL);
        lDataset.ExecSQL;
        FHighlightAnnualGreaterThanAverage := Value;
      end;
    finally
      if Assigned(lDataset) then
      begin
        lDataset.DataSet.Close;
        FreeAndNil(lDataset);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDataObjectRainfall.Get_HighlightAnnualLessThanAverage: WordBool;
const OPNAME = 'TDataObjectRainfall.Get_HighlightAnnualLessThanAverage';
begin
  Result := FALSE;
  try
    Result := FHighlightAnnualLessThanAverage;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDataObjectRainfall.Set_HighlightAnnualLessThanAverage(Value: WordBool);
const OPNAME = 'TDataObjectRainfall.Set_HighlightAnnualLessThanAverage';
var
  lDataset : TAbstractModelDataset;
  lSQL     : string;
begin
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), lDataset);
    try
      if Assigned(lDataset) then
      begin
        lSQL := 'UPDATE RainfallRAWFlags SET AnnualLess = ';
        if (Value) then
          lSQL := lSQL + QuotedStr('Y')
        else
          lSQL := lSQL + QuotedStr('N');
        lDataset.DataSet.Close;
        lDataset.SetSQL(lSQL);
        lDataset.ExecSQL;
        FHighlightAnnualLessThanAverage := Value;
      end;
    finally
      if Assigned(lDataset) then
      begin
        lDataset.DataSet.Close;
        FreeAndNil(lDataset);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDataObjectRainfall.Get_HighlightMonthlyGreaterThanAbsolute: WordBool;
const OPNAME = 'TDataObjectRainfall.Get_HighlightMonthlyGreaterThanAbsolute';
begin
  Result := FALSE;
  try
    Result := FHighlightMonthlyGreaterThanAbsolute;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDataObjectRainfall.Set_HighlightMonthlyGreaterThanAbsolute(Value: WordBool);
const OPNAME = 'TDataObjectRainfall.Set_HighlightMonthlyGreaterThanAbsolute';
var
  lDataset : TAbstractModelDataset;
  lSQL     : string;
begin
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), lDataset);
    try
      if Assigned(lDataset) then
      begin
        lSQL := 'UPDATE RainfallRAWFlags SET MontlyGreaterAbsolute = ';
        if (Value) then
          lSQL := lSQL + QuotedStr('Y')
        else
          lSQL := lSQL + QuotedStr('N');
        lDataset.DataSet.Close;
        lDataset.SetSQL(lSQL);
        lDataset.ExecSQL;
        FHighlightMonthlyGreaterThanAbsolute := Value;
      end;
    finally
      if Assigned(lDataset) then
      begin
        lDataset.DataSet.Close;
        FreeAndNil(lDataset);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDataObjectRainfall.Get_MonthlyGreaterThanAbsoluteValue: Double;
const OPNAME = 'TDataObjectRainfall.Get_MonthlyGreaterThanAbsoluteValue';
begin
  Result := 0;
  try
    Result := FMonthlyGreaterThanAbsoluteValue;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDataObjectRainfall.Set_MonthlyGreaterThanAbsoluteValue(Value: Double);
const OPNAME = 'TDataObjectRainfall.Set_MonthlyGreaterThanAbsoluteValue';
var
  lDataset : TAbstractModelDataset;
  lSQL     : string;
begin
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), lDataset);
    try
      if Assigned(lDataset) then
      begin
        lSQL := 'UPDATE RainfallRAWFlags SET MonthlyGreaterAbsoluteValue = ' +
                FloatToStrF(Value, ffFixed, 10, 2);
        lDataset.DataSet.Close;
        lDataset.SetSQL(lSQL);
        lDataset.ExecSQL;
        FMonthlyGreaterThanAbsoluteValue := Value;
      end;
    finally
      if Assigned(lDataset) then
      begin
        lDataset.DataSet.Close;
        FreeAndNil(lDataset);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDataObjectRainfall.Get_HighlightRepeatingValues: WordBool;
const OPNAME = 'TDataObjectRainfall.Get_HighlightRepeatingValues';
begin
  Result := FALSE;
  try
    Result := FHighlightRepeatingValues;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDataObjectRainfall.Set_HighlightRepeatingValues(Value: WordBool);
const OPNAME = 'TDataObjectRainfall.Set_HighlightRepeatingValues';
var
  lDataset : TAbstractModelDataset;
  lSQL     : string;
begin
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), lDataset);
    try
      if Assigned(lDataset) then
      begin
        lSQL := 'UPDATE RainfallRAWFlags SET RepeatingValues = ';
        if (Value) then
          lSQL := lSQL + QuotedStr('Y')
        else
          lSQL := lSQL + QuotedStr('N');
        lDataset.DataSet.Close;
        lDataset.SetSQL(lSQL);
        lDataset.ExecSQL;
        FHighlightRepeatingValues := Value;
      end;
    finally
      if Assigned(lDataset) then
      begin
        lDataset.DataSet.Close;
        FreeAndNil(lDataset);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDataObjectRainfall.Get_HighlightRoundedValues: WordBool;
const OPNAME = 'TDataObjectRainfall.Get_HighlightRoundedValues';
begin
  Result := FALSE;
  try
    Result := FHighlightRoundedValues;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDataObjectRainfall.Set_HighlightRoundedValues(Value: WordBool);
const OPNAME = 'TDataObjectRainfall.Set_HighlightRoundedValues';
var
  lDataset : TAbstractModelDataset;
  lSQL     : string;
begin
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), lDataset);
    try
      if Assigned(lDataset) then
      begin
        lSQL := 'UPDATE RainfallRAWFlags SET RoundedValues = ';
        if (Value) then
          lSQL := lSQL + QuotedStr('Y')
        else
          lSQL := lSQL + QuotedStr('N');
        lDataset.DataSet.Close;
        lDataset.SetSQL(lSQL);
        lDataset.ExecSQL;
        FHighlightRoundedValues := Value;
      end;
    finally
      if Assigned(lDataset) then
      begin
        lDataset.DataSet.Close;
        FreeAndNil(lDataset);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDataObjectRainfall.Get_IncludeUnreliableData : WordBool;
const OPNAME = 'TDataObjectRainfall.Get_IncludeUnreliableData';
begin
  Result := FALSE;
  try
    Result := FIncludeUnreliable;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDataObjectRainfall.Set_IncludeUnreliableData (Value : WordBool);
const OPNAME = 'TDataObjectRainfall.Set_IncludeUnreliableData';
begin
  try
    FIncludeUnreliable := Value;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDataObjectRainfall.PopulateTreeviewWithSelectedProjectGauges (ATreeView : TTreeView): boolean;
const OPNAME = 'TDataObjectRainfall.PopulateTreeviewWithSelectedProjectGauges';
var
  LIndex         : Integer;
  LPatchIndex    : Integer;
  lSplitIndex    : integer;
  LMainNode      : TTreeNode;
  lStationNode   : TTreeNode;
  LStationID     : integer;
  lStation       : TStationData;
  lPatch         : TPatchData;
  lPatchID       : integer;
  lSelectedNode  : TTreeNode;
  lPatchNode     : TTreeNode;
  lSplitNode     : TTreeNode;
  lStationNumber : string;
  lSplit         : IRainfallDataSplit;
  lSplitName     : string;
  lFoundSplit    : boolean;
  lSrcPatchID    : integer;
  lTarget        : WideString;
  lStartYear     : integer;
  lEndYear       : integer;
  LViewDataTreeNodeData: TViewDataTreeNodeData;
begin
  Result := False;
  try
    ATreeView.Items.Clear;
    lSelectedNode := nil;
    LMainNode := nil;

    for LIndex := 0 to FStationDataList.Count - 1 do
    begin
       if (LIndex = 0) then
         LMainNode := ATreeView.Items.Add(nil,'Rainfall Time Series');

      lStation := CastStationDataByIndex(lIndex);
      if (lStation <> nil) then
      begin
        LStationID := lStation.RainfallData.StationID;
        lStationNumber := lStation.RainfallData.StationNumber;
        if (lStation.IsInWR90) then
          lStationNumber := lStationNumber + ' *';

        LViewDataTreeNodeData := TViewDataTreeNodeData.Create;
        PopulateNodeStationData(IntToStr(lStation.RainfallData.StationID),
                                lStation.RainfallData.HydroStartYear,
                                lStation.RainfallData.HydroEndYear,
                                LViewDataTreeNodeData);

        LViewDataTreeNodeData.ViewDataNode.ViewID            := IntToStr(LStationID);
        LViewDataTreeNodeData.ViewDataNode.ParentID          := LMainNode.Text;
        LViewDataTreeNodeData.ViewDataNode.TopParentID       := '';
        LViewDataTreeNodeData.ViewDataNode.Weighting         := 10;
        LViewDataTreeNodeData.ViewDataNode.OverrideCaption   := lStationNumber;
        LViewDataTreeNodeData.ViewDataNode.ShowSQL           := True;
        LViewDataTreeNodeData.ViewDataNode.BitmapName        := '';
        LViewDataTreeNodeData.ViewDataNode.DatasetIDCommaText:= '';
        LViewDataTreeNodeData.ViewDataNode.SubNodesDatasetID := '';
        LViewDataTreeNodeData.ViewDataNode.SubNodesDataSet   := nil;
        LViewDataTreeNodeData.ViewDataNode.IDValues          := '';
        LViewDataTreeNodeData.ViewDataNode.DataType          := '';
        lStationNode := ATreeView.Items.AddChildObject(LMainNode, lStationNumber, LViewDataTreeNodeData);
        LViewDataTreeNodeData.TreeNode := lStationNode;

        if ((FCurrentStationID <> 0) AND (FCurrentStationID = LStationID)) then
          lSelectedNode := lStationNode;

        for lSplitIndex := 0 to lStation.SplitCount - 1 do
        begin
          lSplit     := lStation.GetSplitWithIndex(lSplitIndex);
          lSplitName := IntToStr(lSplit.HydroStartYear) + ' - ' + IntToStr(lSplit.HydroEndYear);

          LViewDataTreeNodeData := TViewDataTreeNodeData.Create;
          PopulateNodeStationData(IntToStr(lStation.RainfallData.StationID),
                                  lSplit.HydroStartYear,
                                  lSplit.HydroEndYear,
                                  LViewDataTreeNodeData);

          LViewDataTreeNodeData.ViewDataNode.ViewID            := IntToStr(LStationID);
          LViewDataTreeNodeData.ViewDataNode.ParentID          := lStationNode.Text;
          LViewDataTreeNodeData.ViewDataNode.TopParentID       := LMainNode.Text;
          LViewDataTreeNodeData.ViewDataNode.Weighting         := 20;
          LViewDataTreeNodeData.ViewDataNode.OverrideCaption   := lSplitName;
          LViewDataTreeNodeData.ViewDataNode.ShowSQL           := True;
          LViewDataTreeNodeData.ViewDataNode.BitmapName        := '';
          LViewDataTreeNodeData.ViewDataNode.DatasetIDCommaText:= '';
          LViewDataTreeNodeData.ViewDataNode.SubNodesDatasetID := '';
          LViewDataTreeNodeData.ViewDataNode.SubNodesDataSet   := nil;
          LViewDataTreeNodeData.ViewDataNode.IDValues          := '';
          LViewDataTreeNodeData.ViewDataNode.DataType          := '';

          lSplitNode := ATreeView.Items.AddChildObject(lStationNode, lSplitName, LViewDataTreeNodeData);
          LViewDataTreeNodeData.TreeNode := lSplitNode;
          if ((FCurrentStationID <> 0) AND (FCurrentStationID = LStationID) AND
              (FCurrentSplitIndex <> -1) AND (FCurrentSplitIndex = lSplitIndex)) then
            lSelectedNode := lSplitNode;
        end;
        for LPatchIndex := 0 to lStation.Patches.Count - 1 do
        begin
          lPatch     := TPatchData(lStation.Patches.Items[lPatchIndex]);
          lPatchID   := lPatch.PatchID;
          lSplitNode  := nil;
          lFoundSplit := FALSE;
          if (lPatch.PatchTypeID = 1) then
          begin
            lSplitNode  := lStationNode.Item[0];
            lFoundSplit := TRUE;
          end
          else
          begin
            lPatch.GetSourceInfoByStationID(LStationID, lSrcPatchID, lTarget, lStartYear, lEndYear);
            if ((lStartYear = 0) AND (lEndYear = 0)) then
            begin
              lSplitNode  := lStationNode.Item[0];
              lFoundSplit := TRUE;
            end
            else
            begin
              lSplitIndex := 0;
              while ((lSplitIndex < lStation.SplitCount) AND (NOT lFoundSplit)) do
              begin
                lSplitNode := lStationNode.Item[lSplitIndex];
                if (lSplitNode.Text = IntToStr(lStartYear) + ' - ' + IntToStr(lEndYear)) then
                  lFoundSplit := TRUE
                else
                  lSplitIndex := lSplitIndex + 1;
              end;
            end;
          end;
          if (lFoundSplit) then
          begin
            LViewDataTreeNodeData := TViewDataTreeNodeData.Create;
            if (lPatch.PatchTypeID = 1) then
              PopulateNodeWRCData(IntToStr(lPatchID), lViewDataTreeNodeData)
            else
              PopulateNodePatchData(IntToStr(LStationID), IntToStr(lPatchID), lViewDataTreeNodeData);
            LViewDataTreeNodeData.ViewDataNode.ViewID            := IntToStr(lPatchID);
            LViewDataTreeNodeData.ViewDataNode.ParentID          := lSplitNode.Text;
            LViewDataTreeNodeData.ViewDataNode.TopParentID       := lStationNode.Text;
            LViewDataTreeNodeData.ViewDataNode.Weighting         := 20;
            LViewDataTreeNodeData.ViewDataNode.OverrideCaption   := lPatch.PatchName;
            LViewDataTreeNodeData.ViewDataNode.ShowSQL           := True;
            LViewDataTreeNodeData.ViewDataNode.BitmapName        := '';
            LViewDataTreeNodeData.ViewDataNode.DatasetIDCommaText:= '';
            LViewDataTreeNodeData.ViewDataNode.SubNodesDatasetID := '';
            LViewDataTreeNodeData.ViewDataNode.SubNodesDataSet   := nil;
            LViewDataTreeNodeData.ViewDataNode.IDValues          := '';
            LViewDataTreeNodeData.ViewDataNode.DataType          := '';

            lPatchNode := ATreeView.Items.AddChildObject(lSplitNode, lPatch.PatchName, LViewDataTreeNodeData);
            LViewDataTreeNodeData.TreeNode := lPatchNode;

            if ((FCurrentPatchID <> 0) AND (FCurrentPatchID = lPatchID)) AND
               ((FCurrentStationID <> 0) AND (FCurrentStationID = LStationID)) then
              lSelectedNode := lPatchNode;
            Result := True;
          end;
        end;
      end;
    end;
    ATreeView.FullExpand;
    if Assigned(lSelectedNode) then
    begin
      ATreeView.Selected := lSelectedNode;

    end;

  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDataObjectRainfall.PopulateNodePatchData(AStationId,APatchID: string; ATreeNodeData: TViewDataTreeNodeData): boolean;
const OPNAME = 'TDataObjectRainfall.PopulateNodePatchData';
var
  LViewDataNode:TViewDataNode;
  LViewDataSet: TViewDataSet;
begin
  Result := False;
  try
    if Assigned(ATreeNodeData) then
    begin
      LViewDataNode := TViewDataNode.Create;
      ATreeNodeData.ViewDataNode := LViewDataNode;

      LViewDataSet := TViewDataSet.Create;
      LViewDataNode.AddViewDataset(LViewDataSet);
      LViewDataSet.ParamCount    := 0;
      LViewDataSet.DatasetID     := APatchID;
      LViewDataSet.Editable      := False;
      LViewDataSet.SQLType       := 1;
      LViewDataSet.NoDataMessage := 'strNoDataReturned';
      LViewDataSet.ViewSQL       := 'SELECT  ( select Description '+
                                    '          from RainfallPatchR rp '+
                                    '          where rp.PatchID = RainfallMonthlyPatchData.PatchID ) AS SeriesName, [Year],'+
                                    QuotedStr(' Value01,Value02,Value03,Value04,Value05,Value06,Value07,Value08,Value09,Value10,Value11,Value12')+
                                    ' AS XValues,'+
                                    ' Value01, Value02, Value03, Value04, Value05, Value06, Value07, Value08, Value09, Value10, Value11, Value12'+
                                    ' FROM RainfallMonthlyPatchData'+
                                    ' WHERE  PatchID = '+ APatchID +
                                    ' AND StationID = '+ AStationId +
                                    ' ORDER BY  Year ';
      Result := True;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDataObjectRainfall.PopulateNodeWRCData(APatchID: string; ATreeNodeData: TViewDataTreeNodeData): boolean;
const OPNAME = 'TDataObjectRainfall.PopulateNodeWRCData';
var
  LViewDataNode : TViewDataNode;
  LViewDataSet  : TViewDataSet;
begin
  Result := False;
  try
    if Assigned(ATreeNodeData) then
    begin
      LViewDataNode := TViewDataNode.Create;
      ATreeNodeData.ViewDataNode := LViewDataNode;

      LViewDataSet := TViewDataSet.Create;
      LViewDataNode.AddViewDataset(LViewDataSet);
      LViewDataSet.ParamCount    := 0;
      LViewDataSet.DatasetID     := APatchID;
      LViewDataSet.Editable      := False;
      LViewDataSet.SQLType       := 1;
      LViewDataSet.NoDataMessage := 'strNoDataReturned';
      LViewDataSet.ViewSQL       := 'SELECT  ( select Description '+
                                    '          from RainfallPatchWRC rp '+
                                    '          where rp.PatchID = RainfallMonthlyWRCData.PatchID ) AS SeriesName, [Year],'+
                                    QuotedStr(' Value01,Value02,Value03,Value04,Value05,Value06,Value07,Value08,Value09,Value10,Value11,Value12')+
                                    ' AS XValues,'+
                                    ' Value01, Value02, Value03, Value04, Value05, Value06, Value07, Value08, Value09, Value10, Value11, Value12'+
                                    ' FROM RainfallMonthlyWRCData'+
                                    ' WHERE  PatchID = '+ APatchID +
                                    ' ORDER BY  Year ';
      Result := True;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDataObjectRainfall.PopulateNodeStationData (AStationId    : string;
                                                      AStartYear    : integer;
                                                      AEndYear      : integer;
                                                      ATreeNodeData : TViewDataTreeNodeData): boolean;
const OPNAME = 'TDataObjectRainfall.PopulateNodeStationData';
var
  LViewDataNode:TViewDataNode;
  LViewDataSet: TViewDataSet;
begin
  Result := False;
  try
    if Assigned(ATreeNodeData) then
    begin
      LViewDataNode := TViewDataNode.Create;
      ATreeNodeData.ViewDataNode      := LViewDataNode;

      LViewDataSet := TViewDataSet.Create;
      LViewDataNode.AddViewDataset(LViewDataSet);
      LViewDataSet.ParamCount    := 0;
      LViewDataSet.DatasetID     := AStationId;
      LViewDataSet.Editable      := False;
      LViewDataSet.SQLType       := 1;
      LViewDataSet.NoDataMessage := 'strNoDataReturned';

      if (StrToInt(AStationId) > 100000) then
        LViewDataSet.ViewSQL     := 'SELECT  ( select StationNumber '+
                                    '          from RainfallUserStations rs '+
                                    '          where rs.StationID = RainfallUserMonthlyData.StationID ) AS SeriesName, [Year],'+
                                    QuotedStr(' Value01,Value02,Value03,Value04,Value05,Value06,Value07,Value08,Value09,Value10,Value11,Value12')+
                                    ' AS XValues,'+
                                    ' Value01, Value02, Value03, Value04, Value05, Value06, Value07, Value08, Value09, Value10, Value11, Value12'+
                                    ' FROM RainfallUserMonthlyData'+
                                    ' WHERE StationID = ' + AStationId +
                                    ' AND Year >= ' + IntToStr(AStartYear) +
                                    ' AND Year <= ' + IntToStr(AEndYear) +
                                    ' ORDER BY Year '

      else
      LViewDataSet.ViewSQL       := 'SELECT  ( select StationNumber '+
                                    '          from RainfallStations rs '+
                                    '          where rs.StationID = RainfallMonthlyRAWData.StationID ) AS SeriesName, [Year],'+
                                    QuotedStr(' Value01,Value02,Value03,Value04,Value05,Value06,Value07,Value08,Value09,Value10,Value11,Value12')+
                                    ' AS XValues,'+
                                    ' Value01, Value02, Value03, Value04, Value05, Value06, Value07, Value08, Value09, Value10, Value11, Value12'+
                                    ' FROM RainfallMonthlyRAWData'+
                                    ' WHERE StationID = ' + AStationId +
                                    ' AND Year >= ' + IntToStr(AStartYear) +
                                    ' AND Year <= ' + IntToStr(AEndYear) +
                                    ' ORDER BY Year ';
      Result := True;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDataObjectRainfall.GetMonthlyData (const AStationNumber : WideString;
                                             const APatchName     : WideString) : WideString;
const OPNAME = 'TDataObjectRainfall.GetMonthlyData';
var
  lSQL           : string;
  lDataset       : TAbstractModelDataset;
  lStationID     : integer;
  lStation       : TStationData;
  lPatch         : TPatchData;
  lTitle         : string;
begin
  Result := '';
  try
    if (AStationNumber <> '') then
    begin
      FAppModules.Database.CreateDataset(integer(dtExecSQL), lDataset);
      try
        if (Assigned(lDataset)) then
        begin
          lSQL := 'SELECT * FROM RainfallStations WHERE StationNumber = ' +
                  QuotedStr(AStationNumber);
          lDataset.DataSet.Close;
          lDataset.SetSQL(lSQL);
          lDataset.DataSet.Open;
          if (NOT lDataset.DataSet.Eof) then
          begin
            lStationID := lDataset.DataSet.FieldByName('StationID').AsInteger;
            lStation   := TStationData.Create(FAppModules);
            try
              lStation.Initialise;
              lStation.RainfallData.StationNumber := AStationNumber;
              lStation.RainfallData.StationID     := lStationID;
              lStation.StationName   := Trim(lDataset.DataSet.FieldByName('StationName').AsString);
              lStation.Latitude      := lDataset.DataSet.FieldByName('StationLatitude').AsInteger;
              lStation.Longitude     := lDataset.DataSet.FieldByName('StationLongitude').AsInteger;
              if (lDataset.DataSet.FieldByName('StationHeight').IsNull) then
                lStation.Height      := NullInteger
              else
                lStation.Height      := lDataset.DataSet.FieldByName('StationHeight').AsInteger;
              lStation.StationType   := Trim(lDataset.DataSet.FieldByName('StationType').AsString);
              lStation.IsInWR90      := (Trim(lDataset.DataSet.FieldByName('WR90').AsString) = 'Y');
              lStation.LoadMonthlyData;
              if (APatchName <> '') then
              begin
                lPatch := lStation.CastPatchWithName(APatchName);
                if (lPatch <> nil) then
                begin
                  lTitle := lPatch.RainfallData.StationNumber + ' (' + lPatch.PatchName + ')';
                  Result := lPatch.CastRainfallData.StreamOut(lTitle);
                end;
              end
              else
              begin
                lTitle := lStation.RainfallData.StationNumber + ' (RAW)';
                Result := lStation.CastRainfallData.StreamOut(lTitle);
              end;
            finally
              FreeAndNil(lStation);
            end;
          end;
        end;
      finally
        if Assigned(lDataset) then
        begin
          lDataset.Dataset.Close;
          FreeAndNil(lDataset);
        end;
      end;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDataObjectRainfall.GetDailyData (const AStationNumber : WideString;
                                           const APatchName     : WideString) : WideString;
const OPNAME = 'TDataObjectRainfall.GetDailyData';
var
  lSQL       : string;
  lDataset   : TAbstractModelDataset;
  lStationID : integer;
  lDailyData : TStringlist;
  lIndex     : integer;
  lTempStr   : string;
begin
  Result := '';
  try
    if (AStationNumber <> '') AND (APatchName = 'WRC') then
    begin
      FAppModules.Database.CreateDataset(integer(dtExecSQL), lDataset);
      try
        if (Assigned(lDataset)) then
        begin
          lSQL := 'SELECT * FROM RainfallStations WHERE StationNumber = ' +
                  QuotedStr(AStationNumber);
          lDataset.DataSet.Close;
          lDataset.SetSQL(lSQL);
          lDataset.DataSet.Open;
          if (NOT lDataset.DataSet.Eof) then
          begin
            lStationID := lDataset.DataSet.FieldByName('StationID').AsInteger;
            lDailyData := TStringlist.Create;
            try
              GetStationDailyData(lStationID, lDailyData);
              for lIndex := 0 to lDailyData.Count - 1 do
              begin
                lTempStr := lDailyData[lIndex];
                Result := Result + lTempStr + #13#10
              end;
            finally
              FreeAndNil(lDailyData);
            end;
          end;
        end;
      finally
        if Assigned(lDataset) then
        begin
          lDataset.Dataset.Close;
          FreeAndNil(lDataset);
        end;
      end;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDataObjectRainfall.RefreshStationData : WordBool;
const OPNAME = 'TDataObjectRainfall.RefreshStationData';
var
  lIndex     : integer;
  lStation   : TStationData;
begin
  Result := FALSE;
  try
    for lIndex := 0 to FStationDataList.Count - 1 do
    begin
      lStation := CastStationDataByIndex(lIndex);
      lStation.LoadMonthlyData;
    end;
    Result := TRUE;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDataObjectRainfall.RefreshUserGauges : WordBool;
const OPNAME = 'TDataObjectRainfall.RefreshUserGauges';
begin
  Result := FALSE;
  try
    Result := FGaugeList.LoadUserGaugesFromDatabase;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDataObjectRainfall.CreateASplit (AStationID : integer;
                                           AStartYear : integer;
                                           AEndYear   : integer): WordBool;
const OPNAME = 'TDataObjectRainfall.CreateASplit';
var
  LDataset     : TAbstractModelDataset;
  lSQL         : string;
  lStationData : TStationData;
begin
  Result := FALSE;
  try
    if (FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataset)) then
    begin
      try
        LDataset.DataSet.Close;
        lSQL := 'INSERT INTO RainfallRAWSplits ' +
                '(StationID, HydroStartYear, HydroEndYear) ' +
                'VALUES (' +
                IntToStr(AStationID) + ',' +
                IntToStr(AStartYear) + ',' +
                IntToStr(AEndYear) + ')';
        LDataset.SetSQL(lSQL);
        LDataset.ExecSQL;

        lStationData := CastStationDataByID(AStationID);
        if (lStationData <> nil) then
        begin
          lStationData.CreateAndPopulateSplit(AStartYear, AEndYear);
          Result := TRUE;
        end;
      finally
        if Assigned(LDataset) then
        begin
          LDataset.Dataset.Close;
          FreeAndNil(LDataset);
        end;
      end;
    end;
  except on E:Exception do HandleError(E,OPNAME) end;
end;

function TDataObjectRainfall.UpdateSplit (AStationID : integer;
                                         AOldStartYear,AStartYear : integer;
                                         AOldEndYear, AEndYear   : integer): WordBool; safecall;
const OPNAME = 'TDataObjectRainfall.UpdateSplit';
var
  LDataset    : TAbstractModelDataset;
  LSQL        : string;
  LStation    : TStationData;
begin
  Result := False;
  try
    if (FAppModules.Database.CreateDataset(integer(dtExecSQL), lDataset)) then
    begin
      try
        LDataset.DataSet.Close;
        LSQL := ' Update RainfallRAWSplits ' +
                ' SET HydroStartYear = ' + IntToStr(AStartYear) +
                '    ,HydroEndYear = '   + IntToStr(AEndYear) +
                ' WHERE StationID = '    + IntToStr(AStationID) +
                ' AND HydroStartYear = ' + IntToStr(AOldStartYear) +
                ' AND HydroEndYear = '   + IntToStr(AOldEndYear);
        LDataset.SetSQL(lSQL);
        LDataset.ExecSQL;

        LStation   := CastStationDataByID(AStationID);
        if (LStation <> nil) then
        begin
          LStation.UpdateSplitForYears(AOldStartYear,AStartYear,AOldEndYear,AEndYear);
          Result := True;
        end;
      finally
        if Assigned(LDataset) then
        begin
          LDataset.Dataset.Close;
          FreeAndNil(LDataset);
        end;
      end;
    end;
  except on E:Exception do HandleError(E,OPNAME) end;
end;



function TDataObjectRainfall.DeleteASplit (AStationID : integer;
                                           AStartYear : integer;
                                           AEndYear   : integer): WordBool;
const OPNAME = 'TDataObjectRainfall.DeleteASplit';
var
  lDataset    : TAbstractModelDataset;
  lSQL        : string;
  lStation    : TStationData;
begin
  Result := False;
  try
    if (FAppModules.Database.CreateDataset(integer(dtExecSQL), lDataset)) then
    begin
      try
        lDataset.DataSet.Close;
        lSQL := 'DELETE FROM RainfallRAWSplits ' +
                ' WHERE StationID = '    + IntToStr(AStationID) +
                ' AND HydroStartYear = ' + IntToStr(AStartYear) +
                ' AND HydroEndYear = '   + IntToStr(AEndYear);
        lDataset.SetSQL(lSQL);
        lDataset.ExecSQL;

        lStation   := CastStationDataByID(AStationID);
        if (lStation <> nil) then
        begin
          lStation.DeleteSplitForYears(AStartYear, AEndYear);
          Result := True;
        end;
      finally
        if Assigned(lDataset) then
        begin
          lDataset.Dataset.Close;
          FreeAndNil(lDataset);
        end;
      end;
    end;
  except on E:Exception do HandleError(E,OPNAME) end;
end;

function TDataObjectRainfall.SaveProjectGauges : WordBool;
const OPNAME = 'TDataObjectRainfall.SaveProjectGauges';
var
  lDataset   : TAbstractModelDataset;
  lSQL       : string;
  lStation   : TStationData;
  lIndex     : integer;
  lGauge     : IRainGauge;
  lGaugeID   : integer;
begin
  Result := FALSE;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), lDataset);
    try
      if (Assigned(lDataset)) then
      begin
        for lIndex := 0 to FGaugeList.TotalCount - 1 do
        begin
          lGauge   := FGaugeList.GetGaugeByIndex(lIndex);
          lGaugeID := lGauge.GaugeID;
          lStation := CastStationDataByID(lGaugeID);
          if (NOT lGauge.Selected) AND (lStation <> nil) then
            DeleteProjectGauge(lGaugeID)
          else
          if (lGauge.Selected) AND (lStation = nil) then
          begin
            lSQL := 'INSERT INTO RainfallProjectGauges ' +
                    '(Model, StudyAreaName, SubArea, Scenario, StationID) ' +
                    'VALUES (' +
                    QuotedStr(FAppModules.StudyArea.ModelCode)     + ',' +
                    QuotedStr(FAppModules.StudyArea.StudyAreaCode) + ',' +
                    QuotedStr(FAppModules.StudyArea.SubAreaCode)   + ',' +
                    QuotedStr(FAppModules.StudyArea.ScenarioCode)  + ',' +
                    IntToStr(lGaugeID) + ')';
            lDataset.DataSet.Close;
            lDataset.SetSQL(lSQL);
            lDataset.ExecSQL();
          end;
        end;
        Result := TRUE;
      end;
    finally
      lDataset.DataSet.Close;
      FreeAndNil(lDataset);
    end;
  except on E:Exception do HandleError(E,OPNAME) end;
end;

function TDataObjectRainfall.DeleteProjectGauge (AStationID : integer) : Boolean;
const OPNAME = 'TDataObjectRainfall.DeleteProjectGauge';
var
  lDataset    : TAbstractModelDataset;
  lSQL        : string;
begin
  Result := FALSE;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), lDataset);
    try
      if (Assigned(lDataset)) then
      begin
        lDataset.DataSet.Close;
        lSQL := 'DELETE * FROM RainfallProjectGauges ' +
                ' WHERE Model = '       + QuotedStr(FAppModules.StudyArea.ModelCode) +
                ' AND StudyAreaName = ' + QuotedStr(FAppModules.StudyArea.StudyAreaCode) +
                ' AND SubArea = '       + QuotedStr(FAppModules.StudyArea.SubAreaCode) +
                ' AND Scenario = '      + QuotedStr(FAppModules.StudyArea.ScenarioCode) +
                ' AND StationID = '     + IntToStr(AStationID);
        lDataset.SetSQL(lSQL);
        lDataset.ExecSQL;
        Result := TRUE;
      end;
    finally
      if Assigned(lDataset) then
      begin
        lDataset.Dataset.Close;
        FreeAndNil(lDataset);
      end;
    end;
  except on E:Exception do HandleError(E,OPNAME) end;
end;

function TDataObjectRainfall.CreateCatchmentZone(ACatchmentFileName : string): TCatchmentZone;
const OPNAME = 'TDataObjectRainfall.CreateCatchmentZone';
var
  LIdentifier : integer;
begin
  Result := nil;
  try
    if InsertRainfallCatchment(LIdentifier,ACatchmentFileName) then
    begin
      Result := AddCatchmentZone;
      Result.CatchmentID := LIdentifier;
      Result.CatchmentFileName := ACatchmentFileName;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDataObjectRainfall.AddCatchmentZone: TCatchmentZone;
const OPNAME = 'TDataObjectRainfall.AddCatchmentZone';
begin
  Result := nil;
  try
    Result := TCatchmentZone.Create(FAppModules);
    FCatchmentZoneList.Add(Result);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDataObjectRainfall.GetCatchmentZoneByIndex(AIndex : integer) : TCatchmentZone;
const OPNAME = 'TDataObjectRainfall.GetCatchmentZoneByIndex';
begin
  Result := nil;
  try
    if (AIndex >= 0) and (AIndex <= FCatchmentZoneList.Count-1) then
      Result := TCatchmentZone(FCatchmentZoneList.Items[AIndex]);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDataObjectRainfall.GetCatchmentZoneByName(ACatchmentName : string) : TCatchmentZone;
const OPNAME = 'TDataObjectRainfall.GetCatchmentZoneByName';
var
  LIndex : integer;
begin
  Result := nil;
  try
    for LIndex := 0 to FCatchmentZoneList.Count -1 do
    begin
      if TCatchmentZone(FCatchmentZoneList.Items[LIndex]).CatchmentFileName = ACatchmentName then
      begin
        Result := TCatchmentZone(FCatchmentZoneList.Items[LIndex]);
        Break;
      end;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDataObjectRainfall.GetCatchmentZoneListCount : integer;
const OPNAME = 'TDataObjectRainfall.GetCatchmentZoneListCount';
begin
  Result := 0;
  try
    Result := FCatchmentZoneList.Count;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDataObjectRainfall.DeleteCatchmentZoneByIndex(AIndex : integer) : boolean;
const OPNAME = 'TDataObjectRainfall.DeleteCatchmentZoneByIndex';
begin
  Result := False;
  try
    if (AIndex >= 0) and (AIndex < FCatchmentZoneList.Count) then
      FCatchmentZoneList.Remove(TCatchmentZone(FCatchmentZoneList.Items[AIndex]));
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDataObjectRainfall.DeleteCatchmentZoneByName(ACatchmentName : string) : boolean;
const OPNAME = 'TDataObjectRainfall.DeleteCatchmentZoneByName';
var
  LIndex : integer;
  LCatchmentZone : TCatchmentZone;
begin
  Result := False;
  try
    LCatchmentZone := GetCatchmentZoneByName(ACatchmentName);
    if LCatchmentZone <> nil then
    begin
      if RemoveCatchmentZone(ACatchmentName) then
      begin
        for LIndex := 0 to FCatchmentZoneList.Count -1 do
        begin
          if TCatchmentZone(FCatchmentZoneList.Items[LIndex]).CatchmentFileName = ACatchmentName then
          begin
            FCatchmentZoneList.Remove(TCatchmentZone(FCatchmentZoneList.Items[LIndex]));
            Break;
          end;
        end;
        Result := True;
      end;
    end;  
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDataObjectRainfall.RemoveCatchmentZone(ACatchmentName : string) : boolean;
const OPNAME = 'TDataObjectRainfall.RemoveCatchmentZone';
var
  LDataset : TAbstractModelDataset;
  LSQL : string;
  LCatchmentID : integer;
begin
  Result := False;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataset);
    try
      LSQL := ' SELECT CatchmentID FROM RainfallCatchment '+
              ' WHERE Model = '       + QuotedStr(FAppModules.StudyArea.ModelCode) +
              ' AND StudyAreaName = ' + QuotedStr(FAppModules.StudyArea.StudyAreaCode) +
              ' AND SubArea = '       + QuotedStr(FAppModules.StudyArea.SubAreaCode) +
              ' AND Scenario = '      + QuotedStr(FAppModules.StudyArea.ScenarioCode) +
              ' AND CatchmentFileName = ' + QuotedStr(ACatchmentName);
      LDataset.DataSet.Close;
      LDataset.SetSQL(LSQL);
      LDataset.DataSet.Open;

      LCatchmentID := LDataset.DataSet.FieldByName('CatchmentID').AsInteger;
      LSQL := ' DELETE * FROM RainfallCatchmentSource '+
              ' WHERE Model = '       + QuotedStr(FAppModules.StudyArea.ModelCode) +
              ' AND StudyAreaName = ' + QuotedStr(FAppModules.StudyArea.StudyAreaCode) +
              ' AND SubArea = '       + QuotedStr(FAppModules.StudyArea.SubAreaCode) +
              ' AND Scenario = '      + QuotedStr(FAppModules.StudyArea.ScenarioCode) +
              ' AND CatchmentID = '   + Trim(LDataset.DataSet.FieldByName('CatchmentID').AsString);
      LDataset.DataSet.Close;
      LDataset.ClearSQL;
      LDataset.SetSQL(LSQL);
      LDataset.ExecSQL;

      LSQL := ' DELETE * FROM RainfallCatchment  '+
              ' WHERE Model = '       + QuotedStr(FAppModules.StudyArea.ModelCode) +
              ' AND StudyAreaName = ' + QuotedStr(FAppModules.StudyArea.StudyAreaCode) +
              ' AND SubArea = '       + QuotedStr(FAppModules.StudyArea.SubAreaCode) +
              ' AND Scenario = '      + QuotedStr(FAppModules.StudyArea.ScenarioCode) +
              ' AND CatchmentFileName = ' + QuotedStr(ACatchmentName);
      LDataset.DataSet.Close;
      LDataset.SetSQL(LSQL);
      LDataset.ExecSQL;

      LSQL := ' DELETE * FROM RainfallCatchmentFileDetail  '+
              ' WHERE Model = '       + QuotedStr(FAppModules.StudyArea.ModelCode) +
              ' AND StudyAreaName = ' + QuotedStr(FAppModules.StudyArea.StudyAreaCode) +
              ' AND SubArea = '       + QuotedStr(FAppModules.StudyArea.SubAreaCode) +
              ' AND Scenario = '      + QuotedStr(FAppModules.StudyArea.ScenarioCode) +
              ' AND CatchmentID = ' + IntToStr(LCatchmentID);

      LDataset.DataSet.Close;
      LDataset.ClearSQL;
      LDataset.SetSQL(LSQL);
      LDataset.ExecSQL;

      LSQL := ' DELETE * FROM RainfallCatchmentFileData  '+
              ' WHERE Model = '       + QuotedStr(FAppModules.StudyArea.ModelCode) +
              ' AND StudyAreaName = ' + QuotedStr(FAppModules.StudyArea.StudyAreaCode) +
              ' AND SubArea = '       + QuotedStr(FAppModules.StudyArea.SubAreaCode) +
              ' AND Scenario = '      + QuotedStr(FAppModules.StudyArea.ScenarioCode) +
              ' AND CatchmentID = ' + IntToStr(LCatchmentID);

      LDataset.DataSet.Close;
      LDataset.ClearSQL;
      LDataset.SetSQL(LSQL);
      LDataset.ExecSQL;
    finally
      FreeAndNil(LDataset);
    end;
    Result := True;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDataObjectRainfall.InsertSAWSDWAFRawDataSQL : string;
const OPNAME = 'TDataObjectRainfall.InsertSAWSDWAFRawDataSQL';
begin
  Result := '';
  try
    Result := 'INSERT INTO RainfallMonthlyRAWData ' +
              '(StationID, [Year],Source, Value01,Flag01,Value02,Flag02,Value03,Flag03,Value04,Flag04,Value05,Flag05'+
              ' ,Value06,Flag06,Value07,Flag07,Value08,Flag08,Value09,Flag09,Value10,Flag10,Value11,Flag11,Value12,Flag12) ' +
              ' VALUES ( :AStationID, :AYear, :ASource, :AValue01,:AFlag01,:AValue02,:AFlag02,:AValue03,:AFlag03,:AValue04,:AFlag04,:AValue05,:AFlag05'+
              ' ,:AValue06,:AFlag06,:AValue07,:AFlag07,:AValue08,:AFlag08,:AValue09,:AFlag09,:AValue10,:AFlag10,:AValue11,:AFlag11,:AValue12,:AFlag12)';

  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDataObjectRainfall.UpdateSAWSDWAFRawDataSQL : string;
const OPNAME = 'TDataObjectRainfall.UpdateSAWSDWAFRawDataSQL';
begin
  Result := '';
  try
    Result := 'UPDATE RainfallMonthlyRAWData ' +
              ' SET  Source  = :ASource,     ' +
              '      Value01 = :AValue01,    ' +
              '      Flag01  = :AFlag01,     ' +
              '      Value02 = :AValue02,    ' +
              '      Flag02  = :AFlag02,     ' +
              '      Value03 = :AValue03,    ' +
              '      Flag03  = :AFlag03,     ' +
              '      Value04 = :AValue04,    ' +
              '      Flag04  = :AFlag04,     ' +
              '      Value05 = :AValue05,    ' +
              '      Flag05  = :AFlag05,     ' +
              '      Value06 = :AValue06,    ' +
              '      Flag06  = :AFlag06,     ' +
              '      Value07 = :AValue07,    ' +
              '      Flag07  = :AFlag07,     ' +
              '      Value08 = :AValue08,    ' +
              '      Flag08  = :AFlag08,     ' +
              '      Value09 = :AValue09,    ' +
              '      Flag09  = :AFlag09,     ' +
              '      Value10 = :AValue10,    ' +
              '      Flag10  = :AFlag10,     ' +
              '      Value11 = :AValue11,    ' +
              '      Flag11  = :AFlag11,     ' +
              '      Value12 = :AValue12,    ' +
              '      Flag12  = :AFlag12      ' +
              'WHERE StationID = :AStationID ' +
              'AND Year    = :AYear ';

  except on E: Exception do HandleError ( E, OPNAME ) end;
end;


function TDataObjectRainfall.StationYearVerified(AStationID : integer; AYear : string) : boolean;
const OPNAME = 'TDataObjectRainfall.StationYearVerified';
var
  LDataset : TAbstractModelDataset;
begin
  Result := False;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataset);
    try
      LDataset.DataSet.Close;
      LDataset.ClearSQL;
      LDataset.SetSQL( 'SELECT * FROM RainfallMonthlyRAWData WHERE '+
                       ' StationID = '+IntToStr(AStationID)+' AND '+
                       ' Year = '+ AYear );
      LDataset.DataSet.Open;
      LDataset.DataSet.Last;
      LDataset.DataSet.First;
      Result := (LDataset.DataSet.RecordCount = 0);
    finally
      FreeAndNil(LDataset);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDataObjectRainfall.GetFilesLineTypes: TAbstractFilesLineTypes;
const OPNAME = 'TDataObjectRainfall.GetFilesLineTypes';
begin
  Result := nil;
  try

  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDataObjectRainfall.GetFileNamesObject: TAbstractModelFileNameList;
const OPNAME = 'TDataObjectRainfall.GetFileNamesObject';
begin
  Result := nil;
  try

  except on E: Exception do HandleError ( E, OPNAME ) end;
end;


function TDataObjectRainfall.WriteSAWSDWAFRawDataToDB(AStationNumber : integer;AYear : string;ASource : integer; AMonth : array of double;
                                                      APatchSign : array of char): boolean;
const OPNAME = 'TDataObjectRainfall.WriteSAWSDWAFRawDataToDB';
var
  LDataset : TAbstractModelDataset;
  LIndex : integer;
begin
  Result := False;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataset);
    try
      LDataset.DataSet.Close;
      LDataset.ClearSQL;
      LDataset.SetSQL(InsertSAWSDWAFRawDataSQL);
      LDataset.ClearQueryParams();
      LDataSet.SetParams(['AStationID', 'AYear','ASource'],[IntToStr(AStationNumber),AYear,IntToStr(ASource)]);
      for LIndex := 0 to 11 do
      begin
        if AMonth[LIndex] <> NullFloat then
          LDataSet.SetParamValue(Format('AValue%2.2d',[LIndex+1]),FloatToStr(AMonth[LIndex]),ftFloat);
        if APatchSign[LIndex] <> ' ' then
          LDataSet.SetParamValue(Format('AFlag%2.2d',[LIndex+1]),APatchSign[LIndex],ftFixedChar);
      end;
      LDataset.ExecSQL;

    finally
      FreeAndNil(LDataset);
    end;
    Result := True;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDataObjectRainfall.WriteSAWSDWAFRawDataToDBHydroYear(AStationNumber : integer;AYear : string;ASource : integer; AMonth : array of double;
                                                               APatchSign : array of char): boolean;
const OPNAME = 'TDataObjectRainfall.WriteSAWSDWAFRawDataToDBHydroYear';
var
  LDataset : TAbstractModelDataset;
  LIndex : integer;
  LMonthIndex : integer;
  LMonth : array[1..12] of double;
  LPatchSign : array[1..12] of char;
begin
  Result := False;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataset);
    try
      for LIndex := 1 to 12 do
      begin
        LMonth[LIndex] := NullFloat;
        LPatchSign[LIndex] := ' ';
      end;

      for LIndex := 0 to 11 do
      begin
        if (LIndex < 9) then
        begin
          LMonthIndex := LIndex+4;
          if (FHydroMonthValue[LMonthIndex] <> NullFloat) then
          begin
            LMonth[LIndex+1] := FHydroMonthValue[LMonthIndex];
            if FHydroPatchSign[LMonthIndex] <> ' ' then
              LPatchSign[LIndex+1] := FHydroPatchSign[LMonthIndex];
          end;
        end
        else
        begin
          LMonthIndex := LIndex-9;
          if (AMonth[LMonthIndex] <> NullFloat) then
          begin
            LMonth[LIndex+1] := AMonth[LMonthIndex];
            if APatchSign[LMonthIndex] <> ' ' then
              LPatchSign[LIndex+1] := APatchSign[LMonthIndex];
          end;
        end;
      end;
      LDataset.DataSet.Close;
      LDataset.ClearSQL;
      LDataset.SetSQL(InsertSAWSDWAFRawDataSQL);
      LDataset.ClearQueryParams();
      LDataSet.SetParams(['AStationID', 'AYear','ASource'],[IntToStr(AStationNumber),AYear,IntToStr(ASource)]);
      for LIndex := 1 to 12 do
      begin
        if LMonth[LIndex] <> NullFloat then
          LDataSet.SetParamValue(Format('AValue%2.2d',[LIndex]),FloatToStr(LMonth[LIndex]),ftFloat);
        if LPatchSign[LIndex] <> ' ' then
          LDataSet.SetParamValue(Format('AFlag%2.2d',[LIndex]),LPatchSign[LIndex],ftFixedChar);
      end;
      LDataset.ExecSQL;
    finally
      FreeAndNil(LDataset);
    end;
    Result := True;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDataObjectRainfall.InsertSAWSDWAFRawDataToDB(AStationNumber : integer;AYear : string;ASource : integer; AMonth : array of double;
                                                      APatchSign : array of char): boolean;
const OPNAME = 'TDataObjectRainfall.InsertSAWSDWAFRawDataToDB';
var
  LDataset : TAbstractModelDataset;
  LIndex : integer;
begin
  Result := False;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataset);
    try
      LDataset.DataSet.Close;
      LDataset.ClearSQL;
      LDataset.SetSQL(UpdateSAWSDWAFRawDataSQL);
      LDataset.ClearQueryParams();
      LDataSet.SetParams(['AStationID', 'AYear','ASource'],[IntToStr(AStationNumber),AYear,IntToStr(ASource)]);
      for LIndex := 0 to 11 do
      begin
        if AMonth[LIndex] <> NullFloat then
          LDataSet.SetParamValue(Format('AValue%2.2d',[LIndex+1]),FloatToStr(AMonth[LIndex]),ftFloat);
        if APatchSign[LIndex] <> ' ' then
          LDataSet.SetParamValue(Format('AFlag%2.2d',[LIndex+1]),APatchSign[LIndex],ftFixedChar);
      end;
      LDataset.ExecSQL;

    finally
      FreeAndNil(LDataset);
    end;
    Result := True;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDataObjectRainfall.ImportRawStationData(AProgressUpdateFuntion : TProgressUpdateFuntion): boolean;
const OPNAME = 'TDataObjectRainfall.ImportRawStationData';
var
  LStop : boolean;
  LCount,
  LIndex : integer;
  LCurrStationNumber,
  LStationNumber : string;
  LData : string;
  LStationID : integer;
  LSource : integer;

  procedure GetRawStationData(AStationID: integer;AData: string);
  var
    LCharIndex,
    LIndex : integer;
    LFloatValue : string;
    LCharValue : string;
    LMonth : array[1..12] of double;
    LPatchSign : array[1..12] of char;
    LYear : string;
    LUnreliable : boolean;
  begin
    if Trim(AData) <> '' then
    begin
      LYear := Trim(GetSubstring(AData,1,4));
      LCharIndex := Length(LYear)+5;
      LUnreliable := False;
      for LIndex := 1 to 12 do
      begin
        LMonth[LIndex] := NullFloat;
        LPatchSign[LIndex] := ' ';
      end;

      for LIndex := 1 to 12 do
      begin
        LFloatValue := Trim(GetSubstring(LData,LCharIndex,7));
        LCharValue := Trim(Copy(LData,LCharIndex+7,1));
        if LFloatValue <> '' then
        begin
          LUnreliable := False;
          while Pos('[',LFloatValue)>0 do
            delete(LFloatValue,Pos('[',LFloatValue),1);
          while Pos(']',LFloatValue)>0 do
          begin
            delete(LFloatValue,Pos(']',LFloatValue),1);
            if  Trim(LFloatValue) <> '' then
              LUnreliable := True;
          end;
          if Trim(LFloatValue) <> ''  then
            LMonth[LIndex] := StrToFloat(LFloatValue);
        end;
        if LCharValue <> '' then
        begin
          if LCharValue = 'M' then
            LCharValue := '[';
          if LUnreliable then
            LCharValue := ']';

          LPatchSign[LIndex] := LCharValue[1];
        end;
        LCharIndex := LCharIndex+8;
      end;
      if StationYearVerified(AStationID,LYear) then
      begin
        if FHydroStart then
        begin
          WriteSAWSDWAFRawDataToDBHydroYear(AStationID,LYear,LSource,LMonth,LPatchSign);
        end
        else
          WriteSAWSDWAFRawDataToDB(AStationID,LYear,LSource,LMonth,LPatchSign);
      end
      else
        AProgressUpdateFuntion(Format('Station No. : %s. Year : %s is in the database already....',[LStationNumber,Trim(GetSubstring(LData,1,5))]),ptWarning, LStop );
    end;

    for LIndex := 1 to 12 do
    begin
        FHydroMonthValue[LIndex] := NullFloat;
        FHydroPatchSign[LIndex] := ' ';
    end;

    for LIndex := 1 to 12 do
    begin
      if LMonth[LIndex] <> NullFloat then
        FHydroMonthValue[LIndex] := LMonth[LIndex];
      if LPatchSign[LIndex] <> ' ' then
        FHydroPatchSign[LIndex] := LPatchSign[LIndex];
    end;

  end;
  procedure InserCommatextRawStationData(AStationID: integer;AData: string);
  var
    LMonth : array[1..12] of double;
    LPatchSign : array[1..12] of char;
    LFloatValue : string;
    LCharValue : string;
    LStrValue,
    LYear : string;
    LIndex : integer;
  begin
    try
      LStrValue := AData;
      LYear := Trim(ExtractDelemetedFirstSubstring(',',LStrValue));
      for LIndex := 1 to 12 do
      begin
        LMonth[LIndex] :=  NullFloat;
        LPatchSign[LIndex] := NullChar;
      end;
      for LIndex := 1 to 12 do
      begin
        LFloatValue := GetCommaDelimetedValue(LStrValue);
        LCharValue  := GetCommaDelimetedValue(LStrValue);
        if (LFloatValue <> '') then
          LMonth[LIndex] := StrToFloat(LFloatValue);
        if LCharValue <> '' then
          LPatchSign[LIndex] := LCharValue[1];
      end;

      if StationYearVerified(AStationID,LYear) then
        WriteSAWSDWAFRawDataToDB(AStationID,LYear,LSource,LMonth,LPatchSign)
      else
      if FOverwrite then
        InsertSAWSDWAFRawDataToDB(AStationID,LYear,LSource,LMonth,LPatchSign)
      else
        AProgressUpdateFuntion(Format('Station No. : %s. Year : %s is in the database already....',[LStationNumber,LYear]),ptWarning, LStop );
    finally
    end;
  end;
begin
  Result := False;
  try
    if FFileData <> nil then
    begin
      if FFileData.Count > 0 then
      begin
        try
          if FReArangeData then
            DoReArangeData;
          AProgressUpdateFuntion('Importing ....',ptNone, LStop );
          for LIndex := 1 to 12 do
          begin
            FHydroMonthValue[LIndex] := NullFloat;
            FHydroPatchSign[LIndex] := ' ';
          end;
          for LIndex := 0 to FFileData.Count-1 do
          begin
            LStop := False;
            if (LStop) then
              Exit;
            LData := FFileData[LIndex];
            if (Trim(LData) = '') or (Pos('Total',LData)>0)
              or (Pos('Mean',LData)>0) or (Pos('Figures',LData)>0)
              or (Pos('Year',LData)>0) or (Pos('Department',LData)>0)
              or (Pos('Notes',LData)>0) or (Pos('recorded',LData)>0)
              or (Pos('except',LData)>0) or (Pos('Rainy Day',LData)>0)
              or (Pos('Accumulated',LData)>0) or (Pos('Drops',LData)>0)
              or (Pos('Missing Data',LData)>0) or (Pos('Med',LData)>0)
              or (Pos('Max',LData)>0) or (Pos('Min',LData)>0)
              or (Pos('OK',LData)>0) or (Pos('Cnt',LData)>0) or (Pos('Data',LData)>0) then
              Continue;
            if Pos('Site',LData)>0 then
            begin
              LStationNumber := Trim(GetSubstring(LData,11,8));
              Continue;
            end
            else
            if Trim(LStationNumber) <> '' then
            begin
              if (LCurrStationNumber <>  LStationNumber) then
              begin
                for LCount := 1 to 12 do
                begin
                  FHydroMonthValue[LCount] := NullFloat;
                  FHydroPatchSign[LCount] := ' ';
                end;
              end;
              LStationID := GetStationFromDB(AProgressUpdateFuntion,LStationNumber,LSource);
              if (LStationID > 0) then
              begin
                AProgressUpdateFuntion(Format('Importing  Station No. : %s. Year : %s  ....',[LStationNumber,Trim(GetSubstring(LData,1,5))]),ptNone, LStop );
                if FReArangeData then
                  InserCommatextRawStationData(LStationID,LData)
                else
                  GetRawStationData(LStationID,LData);
              end
              else
              if LStationID = 0 then
                AProgressUpdateFuntion(Format('Unknown Station No. : %s.',[LStationNumber,Trim(GetSubstring(LData,1,5))]),ptError, LStop );
              LCurrStationNumber :=  LStationNumber;
            end;
          end;
        finally
          FreeAndNil(FFileData);
        end;
      end;
    end;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDataObjectRainfall.GetDwafStationIDFromHydtra(AProgressUpdateFuntion : TProgressUpdateFuntion;AStationNumber: string; var ASource : integer) : integer;
const OPNAME = 'TDataObjectRainfall.GetDwafStationIDFromHydtra';
var
  LModifiedStations : TStringList;
  LStationData : TStringList;
  LData : string;
  LIndex : integer;
  LCount : integer;
  LSQL : string;
  LDataset : TAbstractModelDataset;
  LStationNumber : string;
  LFound : boolean;
begin
  Result := 0;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataset);
    LModifiedStations := TStringList.Create;
    LStationData := TStringList.Create;
    try
      LModifiedStations.LoadFromFile(ExtractFilePath(ApplicationExeName)+'\WRCData\Modified_Rainfall_IMS.csv');
      if LModifiedStations.Count > 0 then
      begin
        LFound := False;
        for LIndex := 0 to LModifiedStations.Count-1 do
        begin

          {if FReArangeData then
          begin
             LStationData.CommaText := LModifiedStations[LIndex];
             if LStationData.Count > 5 then
             begin
               LData := LStationData[6];
               if (AStationNumber = LData) then
               begin
                 LStationNumber := LStationData[1];
                 LStationNumber := LStationNumber +' '+ LStationData[2];
                 LFound := True;
                 Break;
               end;
             end
             else
               Continue;
          end
          else
          begin
          }
            LData := LModifiedStations[LIndex];
            while Length(LData) > 0 do
            begin
              LStationData.Add(Copy(LData,1,Pos(',',LData)-1) );
              if Pos(',',LData) > 0 then
                Delete(LData,1,Pos(',',LData))
              else
                LData := '';
            end;
            LCount := LStationData.IndexOf(AStationNumber);
            if LCount >=0 then
            begin
              LStationNumber := LStationData[LCount+1];
              LFound := True;
              Break;
            end;
          //end;
        end;

        if LFound then
        begin
          LStationNumber := '%'+Trim(LStationNumber)+'%';
          LSQL := ' SELECT * FROM RainfallStations '+
                  ' WHERE StationNumber like '+QuotedStr(LStationNumber) ;
          LDataset.DataSet.Close;
          LDataset.SetSQL(LSQL);
          LDataset.DataSet.Open;
          LDataset.DataSet.Last;
          LDataset.DataSet.First;
          if LDataset.DataSet.RecordCount > 1 then
          begin

            while not LDataset.DataSet.Eof do
            begin
              //AProgressUpdateFuntion(Format('There are Duplicate. Station No. : %s is used by station ID %d',[AStationNumber,LDataset.DataSet.FieldByName('StationID').AsInteger]),ptNone, LStop );
              if not (Pos('A',Trim(LDataset.DataSet.FieldByName('StationNumber').AsString)) > 0) then
              begin
                ASource := LDataset.DataSet.FieldByName('Source').AsInteger;
                Result := LDataset.DataSet.FieldByName('StationID').AsInteger;
                Break;
              end;
              LDataset.DataSet.Next;
            end;
          end
          else
          begin
            ASource := LDataset.DataSet.FieldByName('Source').AsInteger;
            Result := LDataset.DataSet.FieldByName('StationID').AsInteger;
          end;
          //LDataset.DataSet.FieldByName('StationID').AsInteger 13144 To be updated
          //LDataset.DataSet.FieldByName('StationID').AsInteger  13198
          //LDataset.DataSet.FieldByName('StationID').AsInteger   13069

        end;
      end;
    finally
      FreeAndNil(LDataset);
      FreeAndNil(LModifiedStations);
      FreeAndNil(LStationData);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDataObjectRainfall.GetStationFromDB(AProgressUpdateFuntion : TProgressUpdateFuntion;AStationNumber: string; var ASource : integer) : integer;
const OPNAME = 'TDataObjectRainfall.GetStationFromDB';
var
  LDataset : TAbstractModelDataset;
  LSQL : string;
  LStationID : integer;
  LStationNumber : string;
begin
  Result := 0;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataset);
    try

      LStationNumber := '%'+Trim(AStationNumber)+'%';
      LSQL := ' SELECT * FROM RainfallStations '+
              ' WHERE StationNumber like '+QuotedStr(LStationNumber) ;
      LDataset.DataSet.Close;
      LDataset.SetSQL(LSQL);
      LDataset.DataSet.Open;
      LDataset.DataSet.Last;
      LDataset.DataSet.First;
      ASource := LDataset.DataSet.FieldByName('Source').AsInteger;
      if LDataset.DataSet.RecordCount = 0 then
        LStationID := GetDwafStationIDFromHydtra(AProgressUpdateFuntion,AStationNumber,ASource)
      else
      begin
        {if LDataset.DataSet.RecordCount > 1 then
        begin
          while not LDataset.DataSet.Eof do
          begin
            AProgressUpdateFuntion(Format('There are Duplicate. Station No. : %s is used by station ID %d',[AStationNumber,LDataset.DataSet.FieldByName('StationID').AsInteger]),ptNone, LStop );
            LDataset.DataSet.Next;
          end;
        end
        else}
          LStationID := LDataset.DataSet.FieldByName('StationID').AsInteger;
      end;
      Result := LStationID;
    finally
      FreeandNil(LDataset);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDataObjectRainfall.SetImportRawStationData(AFileData: TStrings;AReArangeData,AAppendData,AOverwrite: boolean):boolean;
const OPNAME = 'TDataObjectRainfall.SetImportRawStationData';
begin
  Result := False;
  try
    if AFileData <> nil then
    begin
      if AFileData.Count > 0 then
      begin
        FFileData := TStringList.Create;
        FFileData.Text := AFileData.Text;
        Result := (FFileData.Count > 0);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDataObjectRainfall.SetImportProperties(AReArangeData,AAppendData,AOverwrite,AHydroStartMonth: boolean):boolean;
const OPNAME = 'TDataObjectRainfall.SetImportRawStationData';
begin
  Result := False;
  try
    FReArangeData    := AReArangeData;
    FAppendData      := AAppendData;
    FOverwrite       := AOverwrite;
    FHydroStart      := AHydroStartMonth;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDataObjectRainfall.DoReArangeData;
const OPNAME = 'TDataObjectRainfall.DoReArangeData';
var
  LIndex : integer;
  LMonth : integer;
  LCurrYear,
  LYear : string;
  LMonthValue : array[1..12] of double;
  LMonthPatch : array[1..12] of char;
  LData  : TStringList;
  LValues  : TStringList;
  LSite,
  LRainStation,
  LValuesStr : string;
  procedure InitializeArrays;
  var
    LIndex : integer;
  begin
    for LIndex := 1 to 12 do
    begin
      LMonthValue[LIndex] := NullFloat;
      LMonthPatch[LIndex] := NullChar;
    end;
  end;
  procedure AddMonthlyData;
  var
    LIndex : integer;
  begin
    for LIndex := 1 to 12 do
    begin
      if LMonthValue[LIndex] <> NullFloat then
        LData.Add(FormatFloat('0.000',LMonthValue[LIndex]))
      else
        LData.Add('');
      if LMonthPatch[LIndex] <> ' ' then
        LData.Add(LMonthPatch[LIndex])
      else
        LData.Add('');
    end;
  end;
begin
  try
    LData  := TStringList.Create;
    LValues  := TStringList.Create;
    try
      LRainStation := '';
      LSite := '';
      LCurrYear := '0';
      InitializeArrays;
      for LIndex := 0 to FFileData.Count-1 do
      begin
        LSite := Trim(copy(FFileData[LIndex],1,9));
        if (LSite <> LRainStation) then
        begin
          if LRainStation <> '' then
          begin
            AddMonthlyData;
            LData.Insert(0,LCurrYear);
            LValues.Add(LData.CommaText);
            LData.Clear;
            InitializeArrays;
          end;
          LValues.Add(' Site     '+LSite);
          LRainStation := LSite;
        end;
        LYear := copy(FFileData[LIndex],10,4);
        LMonth := StrToInt(copy(FFileData[LIndex],14,2));
        LValuesStr := Trim(copy(FFileData[LIndex],23,11));
        LMonthValue[LMonth] := StrToFloat(LValuesStr);
        LValuesStr := Trim(copy(FFileData[LIndex],34,4));
        if (LValuesStr = '170')or (LValuesStr = '151') or (LValuesStr = '255') then
          LMonthPatch[LMonth] := '[';
        if (LValuesStr = '171')or (LValuesStr = '173') or (LValuesStr = '201') then
          LMonthPatch[LMonth] := ']';

        if (LCurrYear <> LYear) then
          LCurrYear := LYear;
        if (LMonth = 12) then
        begin
          AddMonthlyData;
          LData.Insert(0,LCurrYear);
          LValues.Add(LData.CommaText);
          LData.Clear;
          InitializeArrays;
        end;
      end;
    finally

      FFileData.Assign(LValues);
      FreeAndNil(LData);
      FreeAndNil(LValues);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDataObjectRainfall.ExecHDYP08(AProgressUpdateFuntion : TProgressUpdateFuntion) : boolean;
const OPNAME = 'TDataObjectRainfall.ExecHDYP08';
var
  LCatchmentZone : TCatchmentZone;
  LRainfallAsPercentMAP : TRainfallAsPercentMAP;
  LCatchmentOutputFileData : TCatchmentOutputFileData;
  LStartYear : integer;
  LEndYear : integer;
  LStop : boolean;
  LYearIndex : integer;
begin
  Result := False;
  try
    LStop := False;
    if Trim(FSelectedCatchmentZone) <> '' then
    begin
      AProgressUpdateFuntion(FAppModules.Language.GetString('Rainfall.ProcessingCatchment')+ FSelectedCatchmentZone , ptNone, LStop );
      if (LStop) then
        Exit;
      LCatchmentZone := GetCatchmentZoneByName(FSelectedCatchmentZone);
      if LCatchmentZone <> nil then
      begin
        if not LCatchmentZone.Initialise then
          Exit;
        AProgressUpdateFuntion(FAppModules.Language.GetString('Rainfall.DeterminingPeriodOfRecords'),ptNone, LStop );
        if (LStop) then
          Exit;
        LCatchmentOutputFileData := LCatchmentZone.GetCatchmentOutputFileDataByPeriod(LStartYear,LEndYear);
        if LCatchmentOutputFileData <> nil then
        begin
          AProgressUpdateFuntion(FAppModules.Language.GetString('Rainfall.CalculatingPercentMAP'),ptNone, LStop );
          if (LStop) then
            Exit;
          for LYearIndex := LStartYear to LEndYear do
          begin
            AProgressUpdateFuntion(Format(FAppModules.Language.GetString('Rainfall.AddingData'),[LYearIndex]),ptNone, LStop );
            if (LStop) then
              Exit;
            LRainfallAsPercentMAP := LCatchmentOutputFileData.AddRainfallAsPercentMAP;
            if not LRainfallAsPercentMAP.Initialise then
              Exit;
            LRainfallAsPercentMAP.HydroYear := LYearIndex;
            AProgressUpdateFuntion(Format(FAppModules.Language.GetString('Rainfall.PrecessingDataAverage'),[LYearIndex]),ptNone, LStop );
            if (LStop) then
              Exit;
            LCatchmentZone.CalcRainfallAsPercentMAP(LRainfallAsPercentMAP);
          end;
          AProgressUpdateFuntion(FAppModules.Language.GetString('Rainfall.WrittingDataToDataBase'),ptNone, LStop );
          if (LStop) then
            Exit;
          WriteCatchmentToDB(LCatchmentZone);
          AProgressUpdateFuntion(FAppModules.Language.GetString('Rainfall.Done'),ptNone, LStop );
        end;
      end;
    end
    else
      ShowMessage(FAppModules.Language.GetString('Rainfall.SelectCatchmentFirst'));
    Result := True;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TDataObjectRainfall.SetSelectedCatchmentZone(ASelectedCatchmentZone: string);
const OPNAME = 'TDataObjectRainfall.SetSelectedCatchmentZone';
begin
  try
    FSelectedCatchmentZone := ASelectedCatchmentZone;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TDataObjectRainfall.SetCatchmentStartEndYear(AStartYear, AEndYear : integer);
const OPNAME = 'TDataObjectRainfall.SetCatchmentStartEndYear';
var
  LCatchmentZone : TCatchmentZone;
begin
  try
    if Trim(FSelectedCatchmentZone) <> '' then
    begin
      LCatchmentZone := GetCatchmentZoneByName(FSelectedCatchmentZone);
      if LCatchmentZone <> nil then
      begin
        LCatchmentZone.CatchmentStartYear  := AStartYear;
        LCatchmentZone.CatchmentEndYear := AEndYear;
      end;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDataObjectRainfall.Initialise: Boolean;
const OPNAME = 'TDataObjectRainfall.Initialise';
begin
  Result := False;
  try
    FCatchmentZoneList.Clear;
    FSelectedCatchmentZone := '';
    Result := True;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDataObjectRainfall.WriteCatchmentToDB(ACatchmentZone: TCatchmentZone): boolean;
const OPNAME = 'TDataObjectRainfall.WriteCatchmentToDB';
var
  LDataset : TAbstractModelDataset;
  LSQL : string;
  LRainfallAsPercentMAP : TRainfallAsPercentMAP;
  LCatchmentOutputFileData : TCatchmentOutputFileData;
  LDetailOfRainfallStationsUsed : TDetailOfRainfallStationsUsed;
  LCount,
  LIndex : integer;
  LFieldName : string;
begin
  Result := False;
  try
    if (ACatchmentZone <> nil) then
    begin
      LCatchmentOutputFileData := ACatchmentZone.CatchmentOutputFileData;
      FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataset);
      try
        if LCatchmentOutputFileData <> nil then
        begin
          LSQL := ' DELETE * FROM RainfallCatchmentFileData ' +
              ' WHERE Model = '       + QuotedStr(FAppModules.StudyArea.ModelCode) +
              ' AND StudyAreaName = ' + QuotedStr(FAppModules.StudyArea.StudyAreaCode) +
              ' AND SubArea = '       + QuotedStr(FAppModules.StudyArea.SubAreaCode) +
              ' AND Scenario = '      + QuotedStr(FAppModules.StudyArea.ScenarioCode) +
              ' AND CatchmentID = '   + IntToStr(ACatchmentZone.CatchmentID);
          LDataset.SetSQL(LSQL);
          LDataset.ExecSQL;
          
          LDataset.ClearSQL;
          LDataset.DataSet.Close;
          LSQL := ' INSERT INTO RainfallCatchmentFileData ( '+
                  ' Model,StudyAreaName,SubArea,Scenario,CatchmentID,[Year],NoOfGauges,GaugesUsed,'+
                  ' PercentageOfMAP01,PercentageOfMAP02,PercentageOfMAP03,PercentageOfMAP04,'+
                  ' PercentageOfMAP05,PercentageOfMAP06,PercentageOfMAP07,PercentageOfMAP08,'+
                  ' PercentageOfMAP09,PercentageOfMAP10,PercentageOfMAP11,PercentageOfMAP12 ) VALUES '+
                  ' ( :Model,:StudyAreaName,:SubArea,:Scenario,:CatchmentID,:Year,:NoOfGauges,:GaugesUsed,'+
                  '   :PercentageOfMAP01,:PercentageOfMAP02,:PercentageOfMAP03,:PercentageOfMAP04,'+
                  '   :PercentageOfMAP05,:PercentageOfMAP06,:PercentageOfMAP07,:PercentageOfMAP08,'+
                  '   :PercentageOfMAP09,:PercentageOfMAP10,:PercentageOfMAP11,:PercentageOfMAP12 )';

          LDataset.SetSQL(LSQL);
          for LIndex := 0 to LCatchmentOutputFileData.RainfallAsPercentMAPCount-1 do
          begin
            LDataSet.ClearQueryParams();
            LDataset.SetParams(['Model','StudyAreaName','SubArea','Scenario','CatchmentID'],
              [FAppModules.StudyArea.ModelCode,
               FAppModules.StudyArea.StudyAreaCode,
               FAppModules.StudyArea.SubAreaCode,
               FAppModules.StudyArea.ScenarioCode,IntToStr(ACatchmentZone.CatchmentID)]);

            LRainfallAsPercentMAP := LCatchmentOutputFileData.RainfallAsPercentMAPByIndex[LIndex];
            if LRainfallAsPercentMAP <> nil then
            begin
               LDataset.SetParams(['Year'], [IntToStr(LRainfallAsPercentMAP.HydroYear)]);
               LDataset.SetParams(['NoOfGauges'], [IntToStr(LRainfallAsPercentMAP.NoOfGaugesUsed)]);
               LDataset.SetParams(['GaugesUsed'],[LRainfallAsPercentMAP.GaugesUsed]);
               for LCount := MinMonths to MaxMonths do
               begin
                 LFieldName := Format('%s%2.2d',['PercentageOfMAP',LCount]);
                 if LRainfallAsPercentMAP.PercentageOfMAPByIndex[LCount] <> NullFloat then
                   LDataset.SetParams([LFieldName],[FormatFloat('###0.00',LRainfallAsPercentMAP.PercentageOfMAPByIndex[LCount])]);
               end;
            end;
            LDataset.ExecSQL;
          end;
          LDataset.ClearSQL;
          LSQL := ' DELETE * FROM RainfallCatchmentFileDetail ' +
              ' WHERE Model = '       + QuotedStr(FAppModules.StudyArea.ModelCode) +
              ' AND StudyAreaName = ' + QuotedStr(FAppModules.StudyArea.StudyAreaCode) +
              ' AND SubArea = '       + QuotedStr(FAppModules.StudyArea.SubAreaCode) +
              ' AND Scenario = '      + QuotedStr(FAppModules.StudyArea.ScenarioCode) +
              ' AND CatchmentID = '   + IntToStr(ACatchmentZone.CatchmentID);
          LDataset.SetSQL(LSQL);
          LDataset.ExecSQL;

          LDataset.ClearSQL;
          LDataset.DataSet.Close;
          LSQL := ' INSERT INTO RainfallCatchmentFileDetail ( '+
                  ' Model,StudyAreaName,SubArea,Scenario,CatchmentID,StationID,SourceID,[Section],'+
                  ' [Position],MAP,PeriodOfRecord,Longitude,Latitude ) VALUES ('+
                  ' :Model,:StudyAreaName,:SubArea,:Scenario,:CatchmentID,:StationID,:SourceID,:Section,'+
                  ' :Position,:MAP,:PeriodOfRecord,:Longitude,:Latitude )';

          LDataset.SetSQL(LSQL);
          for LIndex := 0 to LCatchmentOutputFileData.DetailOfRainfallStationsUsedCount-1 do
          begin
            LDataSet.ClearQueryParams();
            LDataset.SetParams(['Model','StudyAreaName','SubArea','Scenario','CatchmentID'],
              [FAppModules.StudyArea.ModelCode,
               FAppModules.StudyArea.StudyAreaCode,
               FAppModules.StudyArea.SubAreaCode,
               FAppModules.StudyArea.ScenarioCode,IntToStr(ACatchmentZone.CatchmentID)]);

            LDetailOfRainfallStationsUsed := LCatchmentOutputFileData.DetailOfRainfallStationsUsedByIndex[LIndex];
            if LDetailOfRainfallStationsUsed <> nil then
            begin
              LDataset.SetParams(['StationID'], [IntToStr(LDetailOfRainfallStationsUsed.StationID)]);
              LDataset.SetParams(['SourceID'], [IntToStr(LIndex+1)]);
              LDataset.SetParams(['Section'], [LDetailOfRainfallStationsUsed.Section]);
              LDataset.SetParams(['Position'], [LDetailOfRainfallStationsUsed.Position]);
              LDataset.SetParams(['MAP'], [FloatToStr(LDetailOfRainfallStationsUsed.MAPInmm)]);
              LDataset.SetParams(['PeriodOfRecord'], [LDetailOfRainfallStationsUsed.PeriodOfRecord]);
              LDataset.SetParams(['Longitude'], [LDetailOfRainfallStationsUsed.Longitude]);
              LDataset.SetParams(['Latitude'], [LDetailOfRainfallStationsUsed.Latitude]);
            end;
            LDataset.ExecSQL;
          end;

        end;
      finally
        FreeAndNil(LDataset);
      end;
    end;
    Result := True;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDataObjectRainfall.WriteCatchmentToFile(ACatchmentZone  : TCatchmentZone;
                                                  const ADirectory: WideString;
                                                  ARANSelected    : boolean;
                                                  AOUTPUTSelected : boolean;
                                                  AStartYearIndex : integer;
                                                  AEndYearIndex   : integer):boolean;
const OPNAME = 'TDataObjectRainfall.WriteCatchmentToFile';
var
  LIndex                         : integer;
  LCatchmentOutputFileData       : TCatchmentOutputFileData;
  LDetailOfRainfallStationsUsed  : TDetailOfRainfallStationsUsed;
  LRainfallAsPercentMAP          : TRainfallAsPercentMAP;
  LCatchmentZoneValues           : Tstringlist;
  LZoneValues                    : string;
  LMonth                         : integer;
  LOutputFile                    : TFileStream;
  LRANFile                       : TFileStream;
  LIntNumber                     : Tinteger;
  LValue                         : TString;
  LYear                          : string;
begin
  Result := False;
  LZoneValues := '';
  try
    if ACatchmentZone <> nil then
    begin
      LCatchmentOutputFileData := ACatchmentZone.CatchmentOutputFileData;
      if LCatchmentOutputFileData <> nil then
      begin
        LCatchmentZoneValues     := TStringList.Create;
        LIntNumber               := Tinteger.Create;
        LIntNumber.FInitalised   := true;
        LValue                   := TString.Create;
        LValue.FInitalised       := true;

        try
          if (AOUTPUTSelected)then
          begin
            WriteFileHeadings(ACatchmentZone.CatchmentFileName,1,LCatchmentZoneValues );
            for LIndex := 0 to LCatchmentOutputFileData.DetailOfRainfallStationsUsedCount -1 do
            begin
              LDetailOfRainfallStationsUsed := LCatchmentOutputFileData.DetailOfRainfallStationsUsedByIndex[LIndex];
              if LDetailOfRainfallStationsUsed <> nil then
              begin
                LValue.FData   :=  LDetailOfRainfallStationsUsed.Section;
                LValue.FLength := 10;
                LZoneValues    := LZoneValues + ' ' + PadString(LValue);

                LValue.FData   :=  LDetailOfRainfallStationsUsed.Position;
                LValue.FLength := 14;
                LZoneValues    := LZoneValues + ' ' + PadString(LValue);

                LIntNumber.FData   :=  LDetailOfRainfallStationsUsed.MAPInmm;
                LIntNumber.FLength := 10;
                LZoneValues    := LZoneValues + ' ' + PadInt(LIntNumber);

                LValue.FData   :=  LDetailOfRainfallStationsUsed.PeriodOfRecord;
                LValue.FLength := 19;
                LZoneValues    := LZoneValues + ' ' + PadString(LValue);

                LValue.FData   := LDetailOfRainfallStationsUsed.Latitude;
                LValue.FLength := 11;
                LZoneValues    := LZoneValues + ' ' + PadString(LValue);

                LValue.FData   := LDetailOfRainfallStationsUsed.Longitude;
                LValue.FLength := 10;
                LZoneValues    := LZoneValues + ' ' + PadString(LValue);

                LCatchmentZoneValues.Add(LZoneValues);
                LZoneValues := '';
              end;
            end;
            WriteFileHeadings(ACatchmentZone.CatchmentFileName,2,LCatchmentZoneValues );
            for Lindex := AStartYearIndex to AEndYearIndex do
            begin
              LRainfallAsPercentMAP      := LCatchmentOutputFileData.RainfallAsPercentMAPByIndex[Lindex];

              LIntNumber.FData           := LRainfallAsPercentMAP.HydroYear;
              LIntNumber.FLength         := 4;
              LZoneValues := LZoneValues + ' ' +PadInt(LIntNumber);

              LIntNumber.FData           := LRainfallAsPercentMAP.NoOfGaugesUsed;
              LIntNumber.FLength         := 3;
              LZoneValues := LZoneValues + ' ' + PadInt(LIntNumber);

              for LMonth := MinMonths to MaxMonths do
              begin
                if LMonth = 1 then
                begin
                  LValue.FData   :=  FormatFloat('##0.00',LRainfallAsPercentMAP.PercentageOfMAPByIndex[LMonth]);
                  LValue.FLength := 8;
                  LZoneValues    := LZoneValues + ' ' + PadString(LValue);
  //                 LZoneValues := LZoneValues + ' ' +
  //                 SmartFloatFormatForFiles(LRainfallAsPercentMAP.PercentageOfMAPByIndex[LMonth],8,2);
                end
                else
                begin
                  LValue.FData   :=  FormatFloat('##0.00',LRainfallAsPercentMAP.PercentageOfMAPByIndex[LMonth]);
                  LValue.FLength := 6;
                  LZoneValues    := LZoneValues + ' ' + PadString(LValue);
                end;
              end;
              LZoneValues := LZoneValues + SmartFloatFormatForFiles(LRainfallAsPercentMAP.HydroYearTotal,8,2);
              LCatchmentZoneValues.Add(LZoneValues);
              LZoneValues := '';

              if (Lindex = AEndYearIndex) then
              begin
                LCatchmentZoneValues.Add(#13#10);
                LValue.FData   :=  'AVERAGE   ';
                LValue.FLength := 10;
                LZoneValues    := LZoneValues + ' ' + PadString(LValue);

                for LMonth := MinMonths to MaxMonths do
                begin
                  LValue.FData   :=  FormatFloat('##0.00',LCatchmentOutputFileData.AveragePercentageOfMAPByMonth[LMonth]);
                  LValue.FLength := 6;
                  LZoneValues    := LZoneValues + ' ' + PadString(LValue);
//                  LValue.FData   :=  FormatFloat('##0.00',LCatchmentOutputFileData.AveragePercentageOfMAPByMonth[LMonth]);
//                  LValue.FLength := 8;
//                  LZoneValues    := LZoneValues + ' ' + PadString(LValue);
                end;
                LValue.FData   :=  FormatFloat('##0.00',LCatchmentOutputFileData.GrandAverage);
                LValue.FLength := 7;
                LZoneValues    := LZoneValues + ' ' + PadString(LValue);
                LCatchmentZoneValues.Add(LZoneValues);
                LZoneValues := '';

                LCatchmentZoneValues.Add(#13#10);
                LValue.FData   := 'ADJUSTED  ';
                LValue.FLength := 10;
                LZoneValues    := LZoneValues + ' ' + PadString(LValue);

                for LMonth := MinMonths to MaxMonths do
                begin
                  LValue.FData   :=  FormatFloat('##0.00',LCatchmentOutputFileData.AjustedPercentageOfMAPByMonth[LMonth]);
                  LValue.FLength := 6;
                  LZoneValues    := LZoneValues + ' ' + PadString(LValue);
                end;
                LValue.FData   :=  FormatFloat('##0.00',LCatchmentOutputFileData.GrandAjusted);
                LValue.FLength := 7;
                LZoneValues    := LZoneValues + ' ' + PadString(LValue);
                LCatchmentZoneValues.Add(LZoneValues);
                LZoneValues := '';

                LValue.FData   := 'STD.DEV.  ';
                LValue.FLength := 10;
                LZoneValues    := LZoneValues + ' ' + PadString(LValue);

                for LMonth := MinMonths to MaxMonths do
                begin
                  LValue.FData   :=  FormatFloat('##0.00',LCatchmentOutputFileData.StdDevByMonth[LMonth]);
                  LValue.FLength := 6;
                  LZoneValues    := LZoneValues + ' ' + PadString(LValue);
                end;
                LValue.FData   :=  FormatFloat('##0.00',LCatchmentOutputFileData.GrandSTDDEV);
                LValue.FLength := 7;
                LZoneValues    := LZoneValues + ' ' + PadString(LValue);
                LCatchmentZoneValues.Add(LZoneValues);
                LZoneValues := '';
              end;
            end;

            LOutputFile := TFileStream.Create(ADirectory + ACatchmentZone.CatchmentFileName + '.COUT', fmCreate );
            LCatchmentZoneValues.SaveToStream(lOutputFile);
          end;

          LCatchmentZoneValues.Clear;
          LZoneValues := '';
          if(ARANSelected)then
          begin
            for LIndex := AStartYearIndex to AEndYearIndex do
            begin
              LRainfallAsPercentMAP      := LCatchmentOutputFileData.RainfallAsPercentMAPByIndex[Lindex];
              LYear                      := IntToStr(LRainfallAsPercentMAP.HydroYear);
              LValue.FData               := Copy(ACatchmentZone.CatchmentFileName,1,5);
              LValue.FLength             := 5;
              LZoneValues                := {LZoneValues + ' ' + }PadString(LValue);

              LValue.FData               :=  LYear;//Copy(LYear,3,2);
              LValue.FLength             :=  6;
              LZoneValues := LZoneValues +{ ' ' + }PadString(LValue);

              for LMonth := MinMonths to MaxMonths do
              begin
                LValue.FData   :=  FormatFloat('##0.00',LRainfallAsPercentMAP.PercentageOfMAPByIndex[LMonth]);
                LValue.FLength := 6;
                LZoneValues    := LZoneValues + ' ' + PadString(LValue);
//                 LZoneValues := LZoneValues + ' ' +
//                 SmartFloatFormatForFiles(LRainfallAsPercentMAP.PercentageOfMAPByIndex[LMonth],8,2);
              end;
              LCatchmentZoneValues.Add(LZoneValues);
              LZoneValues := '';
            end;

            LRANFile := TFileStream.Create(ADirectory + ACatchmentZone.CatchmentFileName + '.RAN', fmCreate );
            LCatchmentZoneValues.SaveToStream(LRANFile);
          end;
        finally
          FreeAndNil(LIntNumber);
          FreeAndNil(LCatchmentZoneValues);
          FreeAndNil(LOutputFile);
          FreeAndNil(LRANFile);
        end;
      end;
      ShowMessage(FAppModules.Language.GetString('Rainfall.ExportDone'));
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TDataObjectRainfall.WriteFileHeadings(ACatchmentName      : string;
                                                AHeadingPos         : integer;
                                                ACatchmentZoneValues: Tstringlist);
const
  OPNAME      = 'TDataObjectRainfall.WriteFileHeadings';
  LMonthNames :  array[0..12] of string = ('Oct','Nov','Dec','Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','YEAR');
var
  LHeadings   : string;
  LValue      : TString;
  LIndex     : integer;
begin
  try
    LValue             := TString.Create;
    LValue.FInitalised := true;

    try
      if (AHeadingPos = 1 )then
      begin
        LValue.FData   := 'AVERAGE RAINFALL ON CATCHMENT OF GAUGE ' +UpperCase(ACatchmentName) ;
        LValue.FLength := 58;
        LHeadings      := LHeadings  + ' ' + PadString(LValue);
        LHeadings      := LHeadings + #13#10 + #13#10 ;

        LValue.FData   := 'DETAILS OF RAINFALL STATIONS USED'+ #13#10;
        LValue.FLength := 54;
        LHeadings      := LHeadings + ' ' + PadString(LValue);

        LValue.FData   := UpperCase('Section');
        LValue.FLength := 13;
        LHeadings      := LHeadings + ' ' + PadString(LValue);

        LValue.FData   := UpperCase('Position');
        LValue.FLength := 13;
        LHeadings      := LHeadings + ' ' + PadString(LValue);

        LValue.FData   := UpperCase('Map(mm)');
        LValue.FLength := 10;
        LHeadings      := LHeadings + ' ' + PadString(LValue);

        LValue.FData   := UpperCase('Period of Record');
        LValue.FLength := 19;
        LHeadings      := LHeadings + ' ' + PadString(LValue);

        LValue.FData   := UpperCase('Latitude');
        LValue.FLength := 10;
        LHeadings      := LHeadings + ' ' + PadString(LValue);

        LValue.FData   := UpperCase('Longitude');
        LValue.FLength := 10;
        LHeadings      := LHeadings + ' ' + PadString(LValue);

        LHeadings      := LHeadings + #13#10;
        ACatchmentZoneValues.Add(LHeadings);
      end
      else
        if (AHeadingPos = 2 )then
        begin
          LHeadings      := LHeadings + #13#10;

          LValue.FData   := 'RAINFALL INPUT AS PERCENT M.A.P.'+ #13#10;
          LValue.FLength := 63;
          LHeadings      := LHeadings + ' ' + PadString(LValue);

          LValue.FData   := 'YEAR';
          LValue.FLength := 4;
          LHeadings      := LHeadings + ' ' + PadString(LValue);

          LValue.FData   := 'GAUGE';
          LValue.FLength := 6;
          LHeadings      := LHeadings + ' ' + PadString(LValue);

          for LIndex := 0 to 12 do
          begin
            if (LIndex = 0) OR (LIndex = 1) then
            begin
              LValue.FData   := UpperCase(LMonthNames[LIndex]);
              LValue.FLength := 5;
              LHeadings      := LHeadings + ' ' + PadString(LValue);
            end
            else if LIndex = 12 then
            begin
              LValue.FData   := UpperCase(LMonthNames[LIndex]);
              LValue.FLength := 7;
              LHeadings      := LHeadings + ' ' + PadString(LValue);
            end
            else
              begin
                LValue.FData   :=UpperCase(LMonthNames[LIndex]);
                LValue.FLength := 6;
                LHeadings      := LHeadings + ' ' + PadString(LValue);
              end;
          end;
          LHeadings      := LHeadings + #13#10;
          ACatchmentZoneValues.Add(LHeadings);
        end;
    finally
      FreeAndNil(LValue );
    end;

  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

end.



