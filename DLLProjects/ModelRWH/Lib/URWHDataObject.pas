//
//
//  UNIT      : Contains TRWHSiteDataObject Class
//  AUTHOR    : Dziedzi Ramulondi (arivia.kom)
//  DATE      : 09/01/2007
//  COPYRIGHT : Copyright © 2007 DWAF
//
//
unit URWHDataObject;

interface
uses
  Classes,
  SysUtils,
  Contnrs,
  DB,
  UAbstractObject,
  UDWADBComponents;
type
  TDailyDataSource = (ddsDwaSwas,ddsWRCPatch);
  TRainfallStation = class(TAbstractAppObject)
  protected
    FStationID : integer;
    FStationNumber : string;
    FStationOwner : string;
    FStationLongitude : integer;
    FStationLatitude : integer;
    FStationName : string;
    FSource : integer;
    FWR90 : string;
    FStationHeight : integer;
    FStationType : string;
    FExcludeFromRun : boolean;
    function _AddRef: Integer;
    function _Release: Integer;
  public
    function Initialise : boolean; override;
    function PopulateFromDataset(ADataset: TDataset): boolean;

    property StationID : integer read FStationID write FStationID;
    property StationNumber : string read FStationNumber write FStationNumber;
    property StationOwner : string read FStationOwner write FStationOwner;
    property StationLongitude : integer read FStationLongitude write FStationLongitude;
    property StationLatitude : integer read FStationLatitude write FStationLatitude;
    property StationName : string read FStationName write FStationName;
    property Source : integer read FSource write FSource;
    property WR90 : string read FWR90 write FWR90;
    property StationHeight : integer read FStationHeight write FStationHeight;
    property StationType : string read FStationType write FStationType;
    property ExcludeFromRun : boolean read FExcludeFromRun write FExcludeFromRun;
  end;

  TRainfallStationList = class(TAbstractAppObject)
  protected
    FContainer : TObjectList;
    function Get_Count : integer;
    function Get_RainfallStationByIndex(AIndex : integer) : TRainfallStation;
    function Get_RainfallStationByID(AStationID : integer) : TRainfallStation;
    function Get_RainfallStationByName(AStationName : string) : TRainfallStation;
    function Get_RainfallStationByStationNumber(AStationNumber : string) : TRainfallStation;
    function _AddRef: Integer;
    function _Release: Integer;
    function CreateRainfallStation : TRainfallStation; virtual;
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    procedure Set_ContainerOwnsObjects(AOwnObjects : boolean);
    function AddRainfallStation(ARainfallStation : TRainfallStation):boolean;
    function PopulateFromDataset(ADataset: TDataset): boolean;
    function Initialise : boolean; override;
    property Count : integer read Get_Count;
    property RainfallStationByIndex[AIndex : integer]  : TRainfallStation read Get_RainfallStationByIndex;
    property RainfallStationByID[AStationID : integer]  : TRainfallStation read Get_RainfallStationByID;
    property RainfallStationByName[StationName : string]  : TRainfallStation read Get_RainfallStationByName;
    property RainfallStationByStationNumber[StationNumber : string]  : TRainfallStation read Get_RainfallStationByStationNumber;
  end;

  TRWHRunConfig = class(TAbstractAppObject)
  protected
    FIdentifier : integer;
    FRunName : string;
    FPeriodStartDate : TDateTime;
    FPeriodEndDate : TDateTime;
    FRunTypeID : integer;
    FRunStartVolume : double;
    FRunStartLevel : double;
    FRunStopLevel : double;
    FRoofArea : double;
    FRoofRunoffCoef : double;
    FHouseHoldNumber : integer;
    FHouseHoldMembers : integer;
    FHouseHoldDemandPP : double;
    FTankSize01 : double;
    FTankSize02 : double;
    FTankSize03 : double;
    FTankSize04 : double;
    FTankSize05 : double;
    FTankSize06 : double;
    FTankSize07 : double;
    FTankSize08 : double;
    FTankSize09 : double;
    FTankSize10 : double;
    FChanged    : boolean;
    FSavedInDB  : boolean;
    FPopulated  : boolean;
    function _AddRef: Integer;
    function _Release: Integer;
    function Get_TankCount: integer;
  public
    procedure Assign(ARunConfig : TRWHRunConfig);
    function Initialise : boolean; override;
    function PopulateFromDataset(ADataset: TDataset): boolean;
    function PopulateWithDefaults: boolean;
    function YearsInAnalysis: integer;
    function DaysInAnalysis: integer;

    property TankCount : integer read Get_TankCount;
    property Identifier : integer read FIdentifier write FIdentifier;
    property RunName : string read FRunName write FRunName;
    property PeriodStartDate : TDateTime read FPeriodStartDate write FPeriodStartDate;
    property PeriodEndDate : TDateTime read FPeriodEndDate write FPeriodEndDate;
    property RunTypeID : integer read FRunTypeID write FRunTypeID;
    property RunStartVolume : double read FRunStartVolume write FRunStartVolume;
    property RunStartLevel : double read FRunStartLevel write FRunStartLevel;
    property RunStopLevel : double read FRunStopLevel write FRunStopLevel;
    property RoofArea : double read FRoofArea write FRoofArea;
    property RoofRunoffCoef : double read FRoofRunoffCoef write FRoofRunoffCoef;
    property HouseHoldNumber : integer read FHouseHoldNumber write FHouseHoldNumber;
    property HouseHoldMembers : integer read FHouseHoldMembers write FHouseHoldMembers;
    property HouseHoldDemandPP : double read FHouseHoldDemandPP write FHouseHoldDemandPP;
    property TankSize01 : double read FTankSize01 write FTankSize01;
    property TankSize02 : double read FTankSize02 write FTankSize02;
    property TankSize03 : double read FTankSize03 write FTankSize03;
    property TankSize04 : double read FTankSize04 write FTankSize04;
    property TankSize05 : double read FTankSize05 write FTankSize05;
    property TankSize06 : double read FTankSize06 write FTankSize06;
    property TankSize07 : double read FTankSize07 write FTankSize07;
    property TankSize08 : double read FTankSize08 write FTankSize08;
    property TankSize09 : double read FTankSize09 write FTankSize09;
    property TankSize10 : double read FTankSize10 write FTankSize10;
    property Populated  : boolean read FPopulated write FPopulated;
    property Changed    : boolean read FChanged   write FChanged;
    property SavedInDB  : boolean read FSavedInDB write FSavedInDB;
  end;

  TRWHRunConfigList = class(TAbstractAppObject)
  protected
    FContainer : TObjectList;
    function Get_Count : integer;
    function Get_RWHRunConfigByIndex(AIndex : integer) : TRWHRunConfig;
    function Get_RWHRunConfigByID(AIdentifier : integer) : TRWHRunConfig;
    function Get_RWHRunConfigByName(ARunName : string) : TRWHRunConfig;
    function CreateRWHRunConfig : TRWHRunConfig; virtual;
    function _AddRef: Integer;
    function _Release: Integer;
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    function Initialise : boolean; override;
    function PopulateFromDataset(ADataset: TDataset): boolean;
    property Count : integer read Get_Count;
    property RWHRunConfigByIndex[AIndex : integer]    : TRWHRunConfig read Get_RWHRunConfigByIndex;
    property RWHRunConfigByID[AIdentifier : integer]  : TRWHRunConfig read Get_RWHRunConfigByID;
    property RWHRunConfigByName[ARunName : string]    : TRWHRunConfig read Get_RWHRunConfigByName;
  end;

  TRWHModelData = class(TAbstractAppObject)
  protected
    FAllRainfallStationList      : TRainfallStationList;
    FSelectedRainfallStationList : TRainfallStationList;
    FRunConfigList               : TRWHRunConfigList;

    FSelectedConfigID            : integer;
    FSelectedStationNumber       : string;
    FDailyDataSource             : TDailyDataSource;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    function _AddRef: Integer;
    function _Release: Integer;
    function InputFilesPath: string;
    function OutputFilesPath: string;

    function Get_SelectedRunConfig : TRWHRunConfig;
    procedure Set_SelectedRunConfig(ARunConfig: TRWHRunConfig);
    function Get_SelectedRainfallStation : TRainfallStation;
    procedure Set_SelectedRainfallStation(ARainfallStation: TRainfallStation);
  public
    function Initialise: boolean; override;

    function PopulateAllRainfallStationListFromDataset(ADataset: TDataset): boolean;
    function PopulateSelectedRainfallStationListFromDataset(ADataset: TDataset): boolean;
    function PopulateRunConfigurationDataFromDataset(ADataset: TDataset): boolean;

    function AddSelectedRainfallStationList(AStationID : integer): boolean;overload;
    function AddSelectedRainfallStationList(AStationNum : string): boolean;overload;
    function RemoveRainfallStationFromSelection(AStationID : integer): boolean;overload;
    function RemoveRainfallStationFromSelection(AStationName : string): boolean;overload;

    function RemoveAllRainfallStationFromSelection: boolean;

    function AddRunConfigurationData(ARunConfig :TRWHRunConfig): boolean;
    function UpdateRunConfigurationData(ARunConfig :TRWHRunConfig): boolean;
    function DeleteRunConfigurationData(ARunConfig :TRWHRunConfig): boolean;
    function GetDailyDataList (AContainer : TStrings; AStationNumber: string) : boolean; overload;
    function GetDailyDataList (AContainer : TStrings; AStationNumber,APatchName : string) : boolean;  overload;
    function GetMonthlyData (const AStationNumber : WideString; const APatchName     : WideString) : WideString;
    function GetDailyData (const AStationNumber : WideString;  const APatchName     : WideString) : WideString; overload;
    function GetDailyData (const AStationNumber : WideString;  AContainer : TStrings) : boolean; overload;
    function GetRainfallStationInputFileName(const AStationNumber : String) : String;
    function GetRainfallStationOutputDailyFileName(const AStationNumber : String) : String;
    function GetRainfallStationOutputMonthlyFileName(const AStationNumber : String) : String;
    function GetRainfallStationOutputAnnualFileName(const AStationNumber : String) : String;
    function GetRainfallStationOutputBestTankFileName(const AStationNumber : String) : String;
    function DecodeDailyDataLine (AContainer : TStrings; ALineData : string) : boolean;

    function ClearInputFiles: boolean;
    function ClearOutputFiles: boolean;
    function CreateDailyDataInputFile(const AStationNumber : WideString) : boolean;
    property AllRainfallStationList       : TRainfallStationList read FAllRainfallStationList;
    property SelectedRainfallStationList  : TRainfallStationList read FSelectedRainfallStationList;
    property RunConfigList                : TRWHRunConfigList    read FRunConfigList;
    property DailyDataSource              : TDailyDataSource     read FDailyDataSource write FDailyDataSource;
    property SelectedRunConfig            : TRWHRunConfig    read Get_SelectedRunConfig       write Set_SelectedRunConfig;
    property SelectedRainfallStation      : TRainfallStation read Get_SelectedRainfallStation write Set_SelectedRainfallStation;
  end;


var
  RWHModelData : TRWHModelData;


implementation

uses
  VCL.Controls,
  UConstants,
  UUtilities,
  System.DateUtils,
  URWHDataSQLAgent,
  UErrorHandlingOperations;

{ TRWHModelData }

procedure TRWHModelData.CreateMemberObjects;
const OPNAME = 'TRWHModelData.CreateMemberObjects';
begin
  inherited;
  try
    FSelectedConfigID            := NullInteger;
    FSelectedStationNumber       := '';
    FAllRainfallStationList      := TRainfallStationList.Create(FAppModules);
    FSelectedRainfallStationList := TRainfallStationList.Create(FAppModules);
    FRunConfigList               := TRWHRunConfigList.Create(FAppModules);

    FSelectedRainfallStationList.Set_ContainerOwnsObjects(False);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TRWHModelData.DestroyMemberObjects;
const OPNAME = 'TRWHModelData.DestroyMemberObjects';
begin
  inherited;
  try
    FreeAndNil(FAllRainfallStationList);
    FreeAndNil(FSelectedRainfallStationList);
    FreeAndNil(FRunConfigList);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TRWHModelData.Set_SelectedRunConfig(ARunConfig: TRWHRunConfig);
const OPNAME = 'TRWHModelData.Set_SelectedRunConfig';
begin
  try
    if(ARunConfig = nil) then
      FSelectedConfigID := NullInteger
    else
      FSelectedConfigID := ARunConfig.Identifier;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRWHModelData.Get_SelectedRunConfig: TRWHRunConfig;
const OPNAME = 'TRWHModelData.Get_SelectedRunConfig';
begin
  Result := nil;
  try
    if(FSelectedConfigID <> NullInteger) then
      Result := FRunConfigList.RWHRunConfigByID[FSelectedConfigID];
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRWHModelData.Set_SelectedRainfallStation(ARainfallStation: TRainfallStation);
const OPNAME = 'TRWHModelData.Set_SelectedRainfallStation';
begin
  try
    if(ARainfallStation = nil) then
      FSelectedStationNumber := ''
    else
      FSelectedStationNumber := ARainfallStation.StationNumber;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRWHModelData.Get_SelectedRainfallStation: TRainfallStation;
const OPNAME = 'TRWHModelData.Get_SelectedRainfallStation';
begin
  Result := nil;
  try
    if(FSelectedStationNumber <> '') then
      Result := FSelectedRainfallStationList.RainfallStationByStationNumber[FSelectedStationNumber];
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRWHModelData._AddRef: Integer;
const OPNAME = 'TRWHModelData._AddRef';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TRWHModelData._Release: Integer;
const OPNAME = 'TRWHModelData._Release';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TRWHModelData.Initialise: boolean;
const OPNAME = 'TRWHModelData.Initialise';
begin
  Result := inherited Initialise;
  try
    FAllRainfallStationList.Initialise;
    FSelectedRainfallStationList.Initialise;
    FRunConfigList.Initialise;
    FDailyDataSource := TDailyDataSource(FAppModules.ViewIni.ReadInteger(ClassName,'TDailyDataSource',0));
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRWHModelData.PopulateAllRainfallStationListFromDataset(ADataset: TDataset): boolean;
const OPNAME = 'TRWHModelData.PopulateAllRainfallStationListFromDataset';
begin
  Result := False;
  try
    Result    := FAllRainfallStationList.PopulateFromDataset(ADataset);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TRWHModelData.PopulateRunConfigurationDataFromDataset(ADataset: TDataset): boolean;
const OPNAME = 'TRWHModelData.PopulateRunConfigurationDataFromDataset';
begin
  Result := False;
  try
    Result    := FRunConfigList.PopulateFromDataset(ADataset);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TRWHModelData.PopulateSelectedRainfallStationListFromDataset(ADataset: TDataset): boolean;
const OPNAME = 'TRWHModelData.PopulateSelectedRainfallStationListFromDataset';
var
  LRainfallStation : TRainfallStation;
  LStationID : integer;
  LStationNumber : string;
  LStationName : string;
begin
  Result := False;
  try
    if(ADataset = nil) then
      raise Exception.Create('Dataset parameter is not yet assigned.');

    FSelectedRainfallStationList.Initialise;
    ADataset.Open;
    try
      while not ADataset.Eof do
      begin
        LStationID         := ADataset.FieldByName('StationID').AsInteger;
        LStationNumber     := ADataset.FieldByName('StationNumber').AsString;
        LStationName       := ADataset.FieldByName('StationName').AsString;


        LRainfallStation  := FAllRainfallStationList.RainfallStationByID[LStationID];
        if(LRainfallStation = nil) then
          LRainfallStation  := FAllRainfallStationList.RainfallStationByName[LStationName];
        if(LRainfallStation = nil) then
          LRainfallStation  := FAllRainfallStationList.RainfallStationByStationNumber[LStationNumber];

        //if(LRainfallStation <> nil) and (LStationNumber = '0766509 W') then
        if(LRainfallStation <> nil) then
          FSelectedRainfallStationList.AddRainfallStation(LRainfallStation);

        ADataset.Next;
      end;
    finally
      ADataset.Close;
    end;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRWHModelData.AddSelectedRainfallStationList(AStationID: integer): boolean;
const OPNAME = 'TRWHModelData.AddSelectedRainfallStationList';
var
    LSQLAgent : TRWHDataSQLAgent;
    LStation  : TRainfallStation;
begin
  Result := False;
  try
    LStation :=  FSelectedRainfallStationList.RainfallStationByID[AStationID];
    if(LStation = nil) then
    begin
      LSQLAgent := TRWHDataSQLAgent.Create(FAppModules);
      try
        //Result := LSQLAgent.AddSelectedRainfallStationList(LStation);
      finally
        FreeAndNil(LSQLAgent);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TRWHModelData.AddSelectedRainfallStationList(AStationNum: string): boolean;
const OPNAME = 'TRWHModelData.AddSelectedRainfallStationList';
var
    LSQLAgent : TRWHDataSQLAgent;
    LStation  : TRainfallStation;
begin
  Result := False;
  try
    LStation :=  FSelectedRainfallStationList.RainfallStationByStationNumber[AStationNum];
    if(LStation = nil) then
    begin
      LSQLAgent := TRWHDataSQLAgent.Create(FAppModules);
      try
        LStation :=  FAllRainfallStationList.RainfallStationByStationNumber[AStationNum];
        if LStation <> nil then
        begin
          Result := LSQLAgent.AddSelectedRainfallStation(LStation);
          if Result then
            FSelectedRainfallStationList.AddRainfallStation(LStation);
        end;
      finally
        FreeAndNil(LSQLAgent);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TRWHModelData.RemoveRainfallStationFromSelection(AStationID: integer): boolean;
const OPNAME = 'TRWHModelData.RemoveRainfallStationFromSelection';
var
    LSQLAgent : TRWHDataSQLAgent;
    LStation  : TRainfallStation;
    LIndex    : integer;
begin
  Result := False;
  try
    LStation :=  FSelectedRainfallStationList.RainfallStationByID[AStationID];
    if(LStation <> nil) then
    begin
      LSQLAgent := TRWHDataSQLAgent.Create(FAppModules);
      try
        if LSQLAgent.RemoveRainfallStationFromSelection(LStation) then
        begin
          LIndex := FSelectedRainfallStationList.FContainer.IndexOf(LStation);
          if LIndex > -1 then
            FSelectedRainfallStationList.FContainer.Delete(LIndex);
        end;
      finally
        FreeAndNil(LSQLAgent);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TRWHModelData.RemoveRainfallStationFromSelection(AStationName: string): boolean;
const OPNAME = 'TRWHModelData.RemoveRainfallStationFromSelection';
var
    LSQLAgent : TRWHDataSQLAgent;
    LStation  : TRainfallStation;
begin
  Result := False;
  try
    LStation :=  FSelectedRainfallStationList.RainfallStationByName[AStationName];
    if(LStation = nil) then
    begin
      LSQLAgent := TRWHDataSQLAgent.Create(FAppModules);
      try
        //Result := LSQLAgent.RemoveRainfallStationFromSelection(LStation);
      finally
        FreeAndNil(LSQLAgent);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TRWHModelData.RemoveAllRainfallStationFromSelection: boolean;
const OPNAME = 'TRWHModelData.RemoveAllRainfallStationFromSelection';
var
    LSQLAgent : TRWHDataSQLAgent;
   // LStation  : TRainfallStation;
begin
  Result := False;
  try
    LSQLAgent := TRWHDataSQLAgent.Create(FAppModules);
    try
      if LSQLAgent.RemoveAllRainfallStationFromSelection then
        FSelectedRainfallStationList.FContainer.Clear;
    finally
      FreeAndNil(LSQLAgent);
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;


function TRWHModelData.GetDailyData(const AStationNumber, APatchName: WideString): WideString;
const OPNAME = 'TRWHModelData.GetDailyData';
var
    LSQLAgent : TRWHDataSQLAgent;
begin
  Result := '';
  try
    LSQLAgent := TRWHDataSQLAgent.Create(FAppModules);
    try
      Result := LSQLAgent.GetDailyData(AStationNumber, APatchName);
    finally
      FreeAndNil(LSQLAgent);
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TRWHModelData.GetDailyData (const AStationNumber : WideString;  AContainer : TStrings) : boolean;
const OPNAME = 'TRWHModelData.GetDailyData';
var
    LSQLAgent : TRWHDataSQLAgent;
begin
  Result := False;
  try
    LSQLAgent := TRWHDataSQLAgent.Create(FAppModules);
    try
      Result := LSQLAgent.GetRAWDailyData(AStationNumber, AContainer);
    finally
      FreeAndNil(LSQLAgent);
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;


function TRWHModelData.GetMonthlyData(const AStationNumber, APatchName: WideString): WideString;
const OPNAME = 'TRWHModelData.GetMonthlyData';
var
    LSQLAgent : TRWHDataSQLAgent;
begin
  Result := '';
  try
    LSQLAgent := TRWHDataSQLAgent.Create(FAppModules);
    try
      Result := LSQLAgent.GetMonthlyData(AStationNumber, APatchName);
    finally
      FreeAndNil(LSQLAgent);
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TRWHModelData.ClearInputFiles: boolean;
const OPNAME = 'TRWHModelData.ClearInputFiles';
var
  LDirectory : string;
begin
  Result := False;
  try
    LDirectory := InputFilesPath;
    if DirectoryExists(LDirectory)then
      Result :=  DeleteDirectory(LDirectory);
    ForceDirectories(LDirectory);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TRWHModelData.ClearOutputFiles: boolean;
const OPNAME = 'TRWHModelData.ClearOutputFiles';
var
  LDirectory : string;
begin
  Result := False;
  try
    LDirectory := OutputFilesPath;
    if DirectoryExists(LDirectory)then
      Result :=  DeleteDirectory(LDirectory);
    ForceDirectories(LDirectory);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TRWHModelData.CreateDailyDataInputFile(const AStationNumber: WideString): boolean;
const OPNAME = 'TRWHModelData.CreateDailyDataInputFile';
var
  LData : TStringList;
  LFileName : string;
begin
  Result := False;
  try
    LFileName := GetRainfallStationInputFileName(AStationNumber);
    if FileExists(LFileName) then
      DeleteFile(LFileName);

    LData := TStringList.Create;
    try

     case DailyDataSource of
       ddsWRCPatch : GetDailyDataList(LData,AStationNumber,'WRC');
       ddsDwaSwas : GetDailyDataList(LData,AStationNumber);
     end;
      //GetDailyDataList(LData,AStationNumber,'');
      if(LData.Count > 0) then
        LData.SaveToFile(LFileName);
      Result := True;
    finally
      LData.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TRWHModelData.DecodeDailyDataLine(AContainer: TStrings; ALineData: string): boolean;
const OPNAME = 'TRWHModelData.DecodeDailyDataLine';
var
  LStart,LCol       : integer;
  LYear,LMonth,LDay : integer;
  LValue            : string;
  LDate             :  TDateTime;
  LRainfall         : double;
begin
  Result := False;
  try
    LDay := 1;
    if(Length(ALineData) > 16) then
    begin
      LValue := Copy(ALineData,11,4);
      LYear  := StrToIntDef(LValue,0);
      if(LYear > 0) then
      begin
        LValue  := Copy(ALineData,15,2);
        LMonth  := StrToIntDef(LValue,0);
        LStart  := 17;
        for LCol := 1 to 31 do
        begin
          if(LStart >= Length(ALineData)) then Break;
          if TryEncodeDate(LYear,LMonth,LDay,LDate) then
          begin
             LValue := Copy(ALineData,LStart,5);
             if(LValue = '-7777') then
               AContainer.Add(DateToStr(LDate)+',0.00')
             else if(LValue = '-5555') then
               AContainer.Add(DateToStr(LDate)+',    ')
             else
             begin
               LValue := Copy(LValue,2,4);
               LRainfall := StrToFloatDef(LValue,0.0);
               AContainer.Add(DateToStr(LDate)+','+ FormatFloat('###0.00',LRainfall))
             end;
          end;
          LStart := LStart + 5;
          LDay := LDay + 1;
        end;
        Result := True;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TRWHModelData.GetDailyDataList (AContainer : TStrings; AStationNumber: string) : boolean;
const OPNAME = 'TRWHModelData.GetDailyDataList';
begin
  Result := False;
  try
    AContainer.Clear;
    Result := GetDailyData(AStationNumber,AContainer);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TRWHModelData.GetDailyDataList(AContainer: TStrings; AStationNumber, APatchName: string): boolean;
const OPNAME = 'TRWHModelData.CreateDailyDataInputFile';
var
  LData     : TStringList;
  LLineData : string;
  LFileData : WideString;
  LIndex    : integer;
begin
  Result := False;
  try
    AContainer.Clear;
    LFileData := GetDailyData(AStationNumber,APatchName);
    if(LFileData <> '') and (APatchName = 'WRC') then
    begin
      LData := TStringList.Create;
      try
        LData.Text := LFileData;
        for LIndex := 0 to LData.Count-1 do
        begin
          LLineData := LData.Strings[LIndex];
          DecodeDailyDataLine(AContainer,LLineData);
        end;
      finally
        LData.Free;
      end;
      Result := True;
    end;

  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TRWHModelData.InputFilesPath: string;
const OPNAME = 'TRWHModelData.InputFilesPath';
begin
  Result := '';
  try
    Result := GetAppDataLocalDir+'\Data\Input\StationData\'; //ExtractFilePath(ApplicationExeName)+'Data\Input\StationData\';
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TRWHModelData.OutputFilesPath: string;
const OPNAME = 'TRWHModelData.OutputFilesPath';
begin
  Result := '';
  try
    Result := GetAppDataLocalDir+'\Data\Output\StationData\'; // ExtractFilePath(ApplicationExeName)+'Data\Output\StationData\';
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TRWHModelData.GetRainfallStationInputFileName(const AStationNumber: String): String;
const OPNAME = 'TRWHModelData.GetRainfallStationInputFileName';
begin
  Result := '';
  try
    Result := InputFilesPath+AStationNumber+'.csv';
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TRWHModelData.GetRainfallStationOutputDailyFileName(const AStationNumber: String): String;
const OPNAME = 'TRWHModelData.GetRainfallStationOutputDailyFileName';
begin
  Result := '';
  try
    Result := OutputFilesPath+AStationNumber+'Daily.csv';
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TRWHModelData.GetRainfallStationOutputMonthlyFileName(const AStationNumber: String): String;
const OPNAME = 'TRWHModelData.GetRainfallStationOutputMonthlyFileName';
begin
  Result := '';
  try
    Result := OutputFilesPath+AStationNumber+'Monthly.csv';
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TRWHModelData.GetRainfallStationOutputAnnualFileName(const AStationNumber: String): String;
const OPNAME = 'TRWHModelData.GetRainfallStationOutputAnnualFileName';
begin
  Result := '';
  try
    Result := OutputFilesPath+AStationNumber+'Annual.csv';
  except on E: Exception do HandleError(E, OPNAME); end;
end;


function TRWHModelData.GetRainfallStationOutputBestTankFileName(const AStationNumber: String): String;
const OPNAME = 'TRWHModelData.GetRainfallStationOutputBestTankFileName';
begin
  Result := '';
  try
    Result := OutputFilesPath+AStationNumber+'BestTankSize.csv';
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TRWHModelData.AddRunConfigurationData(ARunConfig: TRWHRunConfig): boolean;
const OPNAME = 'TRWHModelData.AddRunConfigurationData';
var
    LSQLAgent : TRWHDataSQLAgent;
    LRunConfig: TRWHRunConfig;
begin
  Result := False;
  try
    if(ARunConfig <> nil) and (FRunConfigList.RWHRunConfigByID[ARunConfig.Identifier] = nil) then
    begin
      LSQLAgent := TRWHDataSQLAgent.Create(FAppModules);
      try
        Result := LSQLAgent.AddRunConfigurationData(ARunConfig);
        if Result then
        begin
          LRunConfig := FRunConfigList.CreateRWHRunConfig;
          LRunConfig.Assign(ARunConfig);
        end;
      finally
        FreeAndNil(LSQLAgent);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRWHModelData.UpdateRunConfigurationData(ARunConfig: TRWHRunConfig): boolean;
const OPNAME = 'TRWHModelData.UpdateRunConfigurationData';
var
  LSQLAgent : TRWHDataSQLAgent;
  LRunConfig: TRWHRunConfig;
begin
  Result := False;
  try
    if(ARunConfig <> nil) then
    begin
      LRunConfig := FRunConfigList.RWHRunConfigByID[ARunConfig.Identifier];
      if(LRunConfig <> nil) then
      begin
        LSQLAgent := TRWHDataSQLAgent.Create(FAppModules);
        try
          Result := LSQLAgent.UpdateRunConfigurationData(ARunConfig);
          if Result then
          begin
            ARunConfig.SavedInDB  := True;
            ARunConfig.Changed    := False;
            ARunConfig.Populated  := True;
            LRunConfig.Assign(ARunConfig);
          end;
        finally
          FreeAndNil(LSQLAgent);
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRWHModelData.DeleteRunConfigurationData(ARunConfig: TRWHRunConfig): boolean;
const OPNAME = 'TRWHModelData.DeleteRunConfigurationData';
var
  LSQLAgent : TRWHDataSQLAgent;
  LRunConfig       : TRWHRunConfig;
begin
  Result := False;
  try
    if(ARunConfig <> nil) then
    begin
      LRunConfig := FRunConfigList.RWHRunConfigByID[ARunConfig.Identifier];
      LSQLAgent := TRWHDataSQLAgent.Create(FAppModules);
      try
        Result := LSQLAgent.DeleteRunConfigurationData(ARunConfig);
        if Result and (FRunConfigList.FContainer.IndexOf(LRunConfig) >= 0) then
          FRunConfigList.FContainer.Delete(FRunConfigList.FContainer.IndexOf(LRunConfig));
      finally
        FreeAndNil(LSQLAgent);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{ TRainfallStation }

function TRainfallStation._AddRef: Integer;
const OPNAME = 'TRainfallStation._AddRef';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TRainfallStation._Release: Integer;
const OPNAME = 'TRainfallStation._Release';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TRainfallStation.Initialise: boolean;
const OPNAME = 'TRainfallStation.Initialise';
begin
  Result := False;
  try
    FStationID         := NullInteger;
    FStationNumber     := '';
    FStationOwner      := '';
    FStationLongitude  := NullInteger;
    FStationLatitude   := NullInteger;
    FStationName       := '';
    FSource            := NullInteger;
    FWR90              := '';
    FStationHeight     := NullInteger;
    FStationType       := '';
    FExcludeFromRun    := False;
    Result             := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRainfallStation.PopulateFromDataset(ADataset: TDataset): boolean;
const OPNAME = 'TRainfallStation.PopulateFromDataset';
begin
  Result := False;
  try
    if(ADataset <> nil) and  (not ADataset.Eof) then
    begin
      FStationID         := ADataset.FieldByName('StationID').AsInteger;
      FStationNumber     := ADataset.FieldByName('StationNumber').AsString;
      FStationOwner      := ADataset.FieldByName('StationOwner').AsString;
      FStationLongitude  := ADataset.FieldByName('StationLongitude').AsInteger;
      FStationLatitude   := ADataset.FieldByName('StationLatitude').AsInteger;
      FStationName       := ADataset.FieldByName('StationName').AsString;
      FSource            := ADataset.FieldByName('Source').AsInteger;
      FWR90              := ADataset.FieldByName('WR90').AsString;
      FStationHeight     := ADataset.FieldByName('StationHeight').AsInteger;
      FStationType       := ADataset.FieldByName('StationType').AsString;
      Result             := True;
    end
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{ TRainfallStationList }

procedure TRainfallStationList.AfterConstruction;
const OPNAME = 'TRainfallStationList.AfterConstruction';
begin
  try
    FContainer    := TObjectList.Create(True);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRainfallStationList.BeforeDestruction;
const OPNAME = 'TRainfallStationList.BeforeDestruction';
begin
  try
    FreeAndNil(FContainer);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRainfallStationList._AddRef: Integer;
const OPNAME = 'TRainfallStationList._AddRef';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TRainfallStationList._Release: Integer;
const OPNAME = 'TRainfallStationList._Release';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TRainfallStationList.CreateRainfallStation: TRainfallStation;
const OPNAME = 'TRainfallStationList.CreateRainfallStation';
begin
  Result := nil;
  try
    Result := TRainfallStation.Create(FAppModules);
    FContainer.Add(Result);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRainfallStationList.AddRainfallStation(ARainfallStation: TRainfallStation): boolean;
const OPNAME = 'TRainfallStationList.AddRainfallStation';
begin
  Result := False;
  try
    if(ARainfallStation <> nil) and (FContainer.IndexOf(ARainfallStation) < 0) then
    begin
      FContainer.Add(ARainfallStation);
      Result := True;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRainfallStationList.Initialise: boolean;
const OPNAME = 'TRainfallStationList.Initialise';
begin
  Result := False;
  try
    FContainer.Clear;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRainfallStationList.Get_Count: integer;
const OPNAME = 'TRainfallStationList.Get_Count';
begin
  Result := 0;
  try
    Result := FContainer.Count;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRainfallStationList.Set_ContainerOwnsObjects(AOwnObjects: boolean);
const OPNAME = 'TRainfallStationList.Get_Count';
begin
  try
    FContainer.OwnsObjects := AOwnObjects;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRainfallStationList.Get_RainfallStationByID(AStationID: integer): TRainfallStation;
const OPNAME = 'TRainfallStationList.Get_RainfallStationByID';
var
  LIndex : integer;
begin
  Result := nil;
  try
    for LIndex := 0 to FContainer.Count-1 do
    begin
    if(TRainfallStation(FContainer.Items[LIndex]).StationID = AStationID) then
      begin
        Result := TRainfallStation(FContainer.Items[LIndex]);
        Break;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRainfallStationList.Get_RainfallStationByIndex(AIndex: integer): TRainfallStation;
const OPNAME = 'TRainfallStationList.Get_RainfallStationByIndex';
begin
  Result := nil;
  try
    if (AIndex >= 0) and (AIndex < FContainer.Count) then
    begin
      Result := TRainfallStation(FContainer[AIndex]);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRainfallStationList.Get_RainfallStationByName(AStationName: string): TRainfallStation;
const OPNAME = 'TRainfallStationList.Get_RainfallStationByName';
var
  LIndex : integer;
begin
  Result := nil;
  try
    for LIndex := 0 to FContainer.Count-1 do
    begin
    if(TRainfallStation(FContainer.Items[LIndex]).StationName = AStationName) then
      begin
        Result := TRainfallStation(FContainer.Items[LIndex]);
        Break;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRainfallStationList.Get_RainfallStationByStationNumber(AStationNumber: string): TRainfallStation;
const OPNAME = 'TRainfallStationList.Get_RainfallStationByName';
var
  LIndex : integer;
begin
  Result := nil;
  try
    for LIndex := 0 to FContainer.Count-1 do
    begin
    if(TRainfallStation(FContainer.Items[LIndex]).StationNumber = AStationNumber) then
      begin
        Result := TRainfallStation(FContainer.Items[LIndex]);
        Break;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRainfallStationList.PopulateFromDataset(ADataset: TDataset): boolean;
const OPNAME = 'TRainfallStationList.PopulateFromDataset';
var
  LRainfallStation : TRainfallStation;
begin
  Result := False;
  try
    if(ADataset = nil) then
      raise Exception.Create('Dataset parameter is not yet assigned.');

    FContainer.Clear;
    ADataset.Open;
    try
      while not ADataset.Eof do
      begin
        LRainfallStation := CreateRainfallStation;
        LRainfallStation.PopulateFromDataset(ADataset);
        ADataset.Next;
      end;
    finally
      ADataset.Close;
    end;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{ TRWHRunConfig }

function TRWHRunConfig._AddRef: Integer;
const OPNAME = 'TRWHRunConfig._AddRef';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TRWHRunConfig._Release: Integer;
const OPNAME = 'TRWHRunConfig._Release';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TRWHRunConfig.Assign(ARunConfig: TRWHRunConfig);
const OPNAME = 'TRWHRunConfig.Assign';
begin
  if(ARunConfig = nil) then Exit;
  try
    FIdentifier             := ARunConfig.Identifier;
    FRunName                := ARunConfig.RunName;
    FPeriodStartDate        := ARunConfig.PeriodStartDate;
    FPeriodEndDate          := ARunConfig.PeriodEndDate;
    FRunTypeID              := ARunConfig.RunTypeID;
    FRunStartVolume         := ARunConfig.RunStartVolume;
    FRunStartLevel          := ARunConfig.RunStartLevel;
    FRunStopLevel           := ARunConfig.RunStopLevel;
    FRoofArea               := ARunConfig.RoofArea;
    FRoofRunoffCoef         := ARunConfig.RoofRunoffCoef;
    FHouseHoldNumber        := ARunConfig.HouseHoldNumber;
    FHouseHoldMembers       := ARunConfig.HouseHoldMembers;
    FHouseHoldDemandPP      := ARunConfig.HouseHoldDemandPP;
    FTankSize01             := ARunConfig.TankSize01;
    FTankSize02             := ARunConfig.TankSize02;
    FTankSize03             := ARunConfig.TankSize03;
    FTankSize04             := ARunConfig.TankSize04;
    FTankSize05             := ARunConfig.TankSize05;
    FTankSize06             := ARunConfig.TankSize06;
    FTankSize07             := ARunConfig.TankSize07;
    FTankSize08             := ARunConfig.TankSize08;
    FTankSize09             := ARunConfig.TankSize09;
    FTankSize10             := ARunConfig.TankSize10;
    FPopulated              := ARunConfig.Populated;
    FChanged                := ARunConfig.Changed;
    FSavedInDB              := ARunConfig.SavedInDB;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRWHRunConfig.Get_TankCount: integer;
const OPNAME = 'TRWHRunConfig._Release';
begin
  Result := 0;
  try
    if(FTankSize01             = NullFloat) then
    begin
      Result := 0;
      Exit;
    end;
    if(FTankSize02             = NullFloat) then
    begin
      Result := 1;
      Exit;
    end;
    if(FTankSize03             = NullFloat) then
    begin
      Result := 2;
      Exit;
    end;
    if(FTankSize04             = NullFloat) then
    begin
      Result := 3;
      Exit;
    end;
    if(FTankSize05             = NullFloat) then
    begin
      Result := 4;
      Exit;
    end;
    if(FTankSize06             = NullFloat) then
    begin
      Result := 5;
      Exit;
    end;
    if(FTankSize07             = NullFloat) then
    begin
      Result := 6;
      Exit;
    end;
    if(FTankSize08             = NullFloat) then
    begin
      Result := 7;
      Exit;
    end;
    if(FTankSize09             = NullFloat) then
    begin
      Result := 8;
      Exit;
    end;
    if(FTankSize10             = NullFloat) then
    begin
      Result := 9;
      Exit;
    end;
      Result := 10;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TRWHRunConfig.Initialise: boolean;
const OPNAME = 'TRWHRunConfig.Initialise';
begin
  Result := False;
  try
    FIdentifier             := NullInteger;
    FRunName                := '';
    FPeriodStartDate        := NullDateTime;
    FPeriodEndDate          := NullDateTime;
    FRunTypeID              := NullInteger;
    FRunStartVolume         := NullFloat;
    FRunStartLevel          := NullFloat;
    FRunStopLevel           := NullFloat;
    FRoofArea               := NullFloat;
    FRoofRunoffCoef         := NullFloat;
    FHouseHoldNumber        := NullInteger;
    FHouseHoldMembers       := NullInteger;
    FHouseHoldDemandPP      := NullFloat;
    FTankSize01             := NullFloat;
    FTankSize02             := NullFloat;
    FTankSize03             := NullFloat;
    FTankSize04             := NullFloat;
    FTankSize05             := NullFloat;
    FTankSize06             := NullFloat;
    FTankSize07             := NullFloat;
    FTankSize08             := NullFloat;
    FTankSize09             := NullFloat;
    FTankSize10             := NullFloat;
    FPopulated              := False;
    FChanged                := False;
    FSavedInDB              := False;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRWHRunConfig.PopulateFromDataset(ADataset: TDataset): boolean;
const OPNAME = 'TRWHRunConfig.PopulateFromDataset';
begin
  Result := False;
  try
    Initialise;
    if(ADataset <> nil) and  (not ADataset.Eof) then
    begin
      if not ADataset.FieldByName('Identifier').IsNull then
        FIdentifier          := ADataset.FieldByName('Identifier').AsInteger;
      if not ADataset.FieldByName('RunName').IsNull then
        FRunName             := ADataset.FieldByName('RunName').AsString;
      if not ADataset.FieldByName('PeriodStartDate').IsNull then
        FPeriodStartDate     := ADataset.FieldByName('PeriodStartDate').AsDateTime;
      if not ADataset.FieldByName('PeriodEndDate').IsNull then
        FPeriodEndDate       := ADataset.FieldByName('PeriodEndDate').AsDateTime;
      if not ADataset.FieldByName('RunTypeID').IsNull then
        FRunTypeID           := ADataset.FieldByName('RunTypeID').AsInteger;
      if not ADataset.FieldByName('RunStartVolume').IsNull then
        FRunStartVolume      := ADataset.FieldByName('RunStartVolume').AsFloat;
      if not ADataset.FieldByName('RunStartLevel').IsNull then
        FRunStartLevel       := ADataset.FieldByName('RunStartLevel').AsFloat;
      if not ADataset.FieldByName('RunStopLevel').IsNull then
        FRunStopLevel        := ADataset.FieldByName('RunStopLevel').AsFloat;
      if not ADataset.FieldByName('RoofArea').IsNull then
        FRoofArea            := ADataset.FieldByName('RoofArea').AsFloat;
      if not ADataset.FieldByName('RoofRunoffCoef').IsNull then
        FRoofRunoffCoef      := ADataset.FieldByName('RoofRunoffCoef').AsFloat;
      if not ADataset.FieldByName('HouseHoldNumber').IsNull then
        FHouseHoldNumber     := ADataset.FieldByName('HouseHoldNumber').AsInteger;
      if not ADataset.FieldByName('HouseHoldMembers').IsNull then
        FHouseHoldMembers    := ADataset.FieldByName('HouseHoldMembers').AsInteger;
      if not ADataset.FieldByName('HouseHoldDemandPP').IsNull then
        FHouseHoldDemandPP   := ADataset.FieldByName('HouseHoldDemandPP').AsFloat;
      if not ADataset.FieldByName('TankSize01').IsNull then
        FTankSize01          := ADataset.FieldByName('TankSize01').AsFloat;
      if not ADataset.FieldByName('TankSize02').IsNull then
        FTankSize02          := ADataset.FieldByName('TankSize02').AsFloat;
      if not ADataset.FieldByName('TankSize03').IsNull then
        FTankSize03          := ADataset.FieldByName('TankSize03').AsFloat;
      if not ADataset.FieldByName('TankSize04').IsNull then
        FTankSize04          := ADataset.FieldByName('TankSize04').AsFloat;
      if not ADataset.FieldByName('TankSize05').IsNull then
        FTankSize05          := ADataset.FieldByName('TankSize05').AsFloat;
      if not ADataset.FieldByName('TankSize06').IsNull then
        FTankSize06          := ADataset.FieldByName('TankSize06').AsFloat;
      if not ADataset.FieldByName('TankSize07').IsNull then
        FTankSize07          := ADataset.FieldByName('TankSize07').AsFloat;
      if not ADataset.FieldByName('TankSize08').IsNull then
        FTankSize08          := ADataset.FieldByName('TankSize08').AsFloat;
      if not ADataset.FieldByName('TankSize09').IsNull then
        FTankSize09          := ADataset.FieldByName('TankSize09').AsFloat;
      if not ADataset.FieldByName('TankSize10').IsNull then
        FTankSize10          := ADataset.FieldByName('TankSize10').AsFloat;
      FPopulated           := True;
      FChanged             := False;
      FSavedInDB           := True;
      Result               := True;
    end
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRWHRunConfig.PopulateWithDefaults: boolean;
const OPNAME = 'TRWHRunConfig.PopulateFromDataset';
begin
  Result := False;
  try
    FIdentifier             := NullInteger;
    FRunName                := '';
    //FPeriodStartDate        := EncodeDate(1920,01,01);
    //FPeriodEndDate          := EncodeDate(YearOf(Now),MonthOf(Now),DayOf(Now));
    //FPeriodStartDate        := EncodeDate(1800,01,01);
    //FPeriodEndDate          := EncodeDate(9999,12,31);
    FPeriodStartDate        := NullDateTime;
    FPeriodEndDate          := NullDateTime;
    FRunTypeID              := 4;
    FRunStartVolume         := 0.00;
    FRunStartLevel          := -1.20;
    FRunStopLevel           := -1.5;
    FRoofArea               := 150;
    FRoofRunoffCoef         := 0.8;
    FHouseHoldNumber        := 1;
    FHouseHoldMembers       := 6;
    FHouseHoldDemandPP      := 0.025;
    FTankSize01             := NullFloat;
    FTankSize02             := NullFloat;
    FTankSize03             := NullFloat;
    FTankSize04             := NullFloat;
    FTankSize05             := NullFloat;
    FTankSize06             := NullFloat;
    FTankSize07             := NullFloat;
    FTankSize08             := NullFloat;
    FTankSize09             := NullFloat;
    FTankSize10             := NullFloat;
    FPopulated              := True;
    FChanged                := True;
    FSavedInDB              := False;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRWHRunConfig.DaysInAnalysis: integer;
const OPNAME = 'TRWHRunConfig.DaysInAnalysis';
begin
  Result := 0;
  try
    Result := DaysBetween(FPeriodEndDate,FPeriodStartDate)+1;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRWHRunConfig.YearsInAnalysis: integer;
const OPNAME = 'TRWHRunConfig.YearsInAnalysis';
begin
  Result := 0;
  try
    Result := YearsBetween(FPeriodEndDate,FPeriodStartDate)+1;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{ TRWHRunConfigList }

procedure TRWHRunConfigList.AfterConstruction;
const OPNAME = 'TRWHRunConfigList.AfterConstruction';
begin
  try
    FContainer    := TObjectList.Create(True);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRWHRunConfigList.BeforeDestruction;
const OPNAME = 'TRWHRunConfigList.BeforeDestruction';
begin
  try
    FreeAndNil(FContainer);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRWHRunConfigList.Initialise: boolean;
const OPNAME = 'TRWHRunConfigList.Initialise';
begin
  Result := False;
  try
    FContainer.Clear;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRWHRunConfigList._AddRef: Integer;
const OPNAME = 'TRWHRunConfigList._AddRef';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TRWHRunConfigList._Release: Integer;
const OPNAME = 'TRWHRunConfigList._Release';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TRWHRunConfigList.Get_Count: integer;
const OPNAME = 'TRWHRunConfigList.Get_Count';
begin
  Result := 0;
  try
    Result := FContainer.Count;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRWHRunConfigList.CreateRWHRunConfig: TRWHRunConfig;
const OPNAME = 'TRWHRunConfigList.CreateRWHRunConfig';
begin
  Result := nil;
  try
    Result := TRWHRunConfig.Create(FAppModules);
    FContainer.Add(Result);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRWHRunConfigList.Get_RWHRunConfigByID(AIdentifier: integer): TRWHRunConfig;
const OPNAME = 'TRWHRunConfigList.Get_RWHRunConfigByID';
var
  LIndex : integer;
begin
  Result := nil;
  try
    for LIndex := 0 to FContainer.Count-1 do
    begin
    if(TRWHRunConfig(FContainer.Items[LIndex]).Identifier = AIdentifier) then
      begin
        Result := TRWHRunConfig(FContainer.Items[LIndex]);
        Break;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRWHRunConfigList.Get_RWHRunConfigByIndex(AIndex: integer): TRWHRunConfig;
const OPNAME = 'TRWHRunConfigList.Get_RWHRunConfigByIndex';
begin
  Result := nil;
  try
    if (AIndex >= 0) and (AIndex < FContainer.Count) then
    begin
      Result := TRWHRunConfig(FContainer[AIndex]);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRWHRunConfigList.Get_RWHRunConfigByName(ARunName: string): TRWHRunConfig;
const OPNAME = 'TRWHRunConfigList.Get_RWHRunConfigByName';
var
  LIndex : integer;
begin
  Result := nil;
  try
    for LIndex := 0 to FContainer.Count-1 do
    begin
    if(TRWHRunConfig(FContainer.Items[LIndex]).RunName = ARunName) then
      begin
        Result := TRWHRunConfig(FContainer.Items[LIndex]);
        Break;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRWHRunConfigList.PopulateFromDataset(ADataset: TDataset): boolean;
const OPNAME = 'TRWHRunConfigList.PopulateFromDataset';
var
  LRWHRunConfig : TRWHRunConfig;
begin
  Result := False;
  try
    if(ADataset = nil) then
      raise Exception.Create('Dataset parameter is not yet assigned.');

    FContainer.Clear;
    ADataset.Open;
    try
      while not ADataset.Eof do
      begin
        LRWHRunConfig := CreateRWHRunConfig;
        LRWHRunConfig.PopulateFromDataset(ADataset);
        ADataset.Next;
      end;
    finally
      ADataset.Close;
    end;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
