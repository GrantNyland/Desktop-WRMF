//
//
//  UNIT      : Contains link classes used by any model.
//  AUTHOR    : Grant Nyland (PDNA)
//  DATE      : 2002/02/27
//  COPYRIGHT : Copyright © 2002 DWAF
//
//
unit UGenericModelLinkClasses;

interface
uses  contnrs, System.Classes,
      Vcl.comctrls;

//const
//   MaxHydrologyFiles = 4;
//   MaxOutputFiles = 3;
//   MaxDataFiles = 16;

type
  TModelMenuAction = (meSaveFile,meEnableImport,meEnableExport,meDisableImport,meDisableExport,//TFileEdit Actions
                      meAddChart, meRemoveChart, meChartName, meSaveView, meAddSeries, meRemoveSeries,meSeriesColor,
                      meTSCToggleIndividualSeries, meShowChartLegendDialog,meOutputComparitorShowChartLegendDialog, //TTimeSeriesComparitor Actions
                      meCreateChart,meRenameChart,meDeleteChart,meCreateView,meRenameView,meDeleteView,
                      meResetChartData, meTogglePlaneMode, meToggleChartMode, meYRCChartDataLoaded,
                      meToggleCurveManipulation, meDeleteTargetDraft, meEditYValue,meLoadCoefficientFile,
                      meStartRegressionEdit,meEndRegressionEdit,meStartDeterministicEdit,meEndDeterministicEdit,// TYRC Actions
                      meFirmYieldNewChart,meFirmYieldOpenChart,meFirmYieldSaveChart,meFirmYieldAddSeries,meFirmYieldDeleteSeries,
                      MeNone,
                      MeFile,
                      MeFileReport,MeFileExit,
                      MeEdit,
                      MeView,
                      MeViewToolBar, MeViewStatusBar, MeViewTreeView, MeViewListBox, MeViewGIS,
                      MeSelect,
                      MeSelectAll, MeInvertSelection, MeUnSelectAll,MeUnSelect,
                      MeSelectFromFile,
                      MeSelectRect, MeSelectByStationNumber, MeSelectByStationName, MeSelectByDistance,
                      MeOption,
                      MeOptionAddToSelection,
                      MeOptionUpdateGISLive,
                      MeHelp, MeHelpContents, MeHelpAbout,
                      meHideFileEditTab,
                      meShowFileEditTab,

                      { Network visulaiser }
                      meNewDrawingGroup,
                      meDeleteDrawingGroup,
                      meRenameDrawingGroup,
                      meNewDrawing,
                      meDeleteDrawing,
                      meRenameDrawing,
                      meEditDrawing,
                      meViewDrawing,
                      meCopyDrawing,

                      { Rainfall Actions }
                      meCreatePatch, meDeletePatch, meRenamePatch, meAddGaugeToPatch, meRemoveGaugeFromPatch,
                      meSelectRAWFlags,
                      meCreateFiles, meCreateSplit, meUpdateSplit,meDeleteSplit,
                      meToggleRainAdminGrid, meToggleRainAdminGraph,  meToggleRainAdminTree,
                      meToggleRainStatsGrid, meToggleRainStatsGraph,  meToggleRainStatsTree,
                      meToggleRainGraphGrid, meToggleRainGraphGraph,  meToggleRainGraphTree,
                      meHighLightOutliers,   meFlagRainfallDataBlock, meUnFlagRainfallDataBlock,
                      meFlagSetup,           meFlagClick,
                      meWeatherEvents,       mePatchChangeList,
                      meCreatePATFiles,
                      meImportUserData,      meClearUserData,meImportSawsDwafData,
                      meToggleRainZoneTree,  meCreateCatchmentZone, meDeleteCatchmentZone,
                      meToggleRainGaugeTree,
                      meExportCatchmentData, meToggleStationUsedGrid, meToggleCatchmentOutputGrid, meToggleRUNGrid,
                      meAddGaugeToZone, meRemoveGaugeFromZone, MeSelectBySAWSBlocks,MeSelectByQuatenary,

                      {HydroNV}  //RianaHydro
                      meHydroNVNewDrawing,
                      meHydroNVDeleteDrawing,
                      meHydroNVRenameDrawing,
                      meHydroNVEditDrawing,
                      meHydroNVViewDrawing,
                      meHydroNVCopyDrawing                      
                      );


  TModelMenuData = class(TObject)
  public
    Action: TModelMenuAction;
    constructor Create(AAction: TModelMenuAction);
  end;

  TAbstractModelLinkClass = class(TObject)
  public
    constructor Create;
    procedure Reset;virtual;abstract;
  end;

  {TFileName = class(TAbstractModelLinkClass)
  protected
    FDefaultName: string;
    FCurrentPath: string;
    FCurrentName: string;
    FCurrentFullName: string;
    FPreviousName: string;
    FLangDescription: string;
    FLoaded: boolean;
    procedure SetFileName(AFileName: string);
  public
    constructor Create;
    procedure Reset; override;
    property DefaultName: string       read FDefaultName      write FDefaultName;
    property CurrentName: string       read FCurrentName      write FCurrentName;
    property CurrentPath: string       read FCurrentPath      write FCurrentPath;
    property FileName   : string       read FCurrentFullName  write SetFileName;
    property PreviousName: string      read FPreviousName     write FPreviousName;
    property LangDescription: string   read FLangDescription  write FLangDescription;
    property Loaded: boolean           read FLoaded           write FLoaded;
    function CopyFromFile(AFileName: TFileName): boolean;
  end;

  TFileNames = class(TObjectList)
  public
    procedure Reset;
    function GetFileObject(AIndex: integer):TFileName;
    function AssignFrom(AFileNames: TFileNames): boolean;
    function AddFileObject(AFileObject: TFileName): boolean;
    function AddFileName(AFileName: string): boolean;
    function CountFiles: integer;
    property DataFileNames[AIndex: integer]:TFileName read GetFileObject;default;

  end;}

  TTreeNodesList = class(TObjectList)
  public
    constructor Create;
    function GetNode(AIndex: integer):TTreeNode;
    property Node[AIndex: integer]:TTreeNode read GetNode;default;
  end;

implementation

uses SysUtils,UErrorHandlingOperations;

{ TAbstractModelLinkClass }

constructor TAbstractModelLinkClass.Create;
const OPNAME = 'TAbstractModelLinkClass.Create';
begin
  inherited Create;
  try
    Reset;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


{ TFileName }
{
function TFileName.CopyFromFile(AFileName: TFileName): boolean;
const OPNAME = 'TFileName.CopyFromFile';
begin
  Result := False;
  try
   FDefaultName     := AFileName.DefaultName;
   FCurrentFullName := AFileName.FileName;
   FCurrentName     := AFileName.CurrentName;
   FCurrentPath     := AFileName.CurrentPath;
   FPreviousName    := AFileName.PreviousName;
   FLangDescription := AFileName.LangDescription;
   FLoaded          := AFileName.Loaded;
   Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

constructor TFileName.Create;
const OPNAME = 'TFileName.Create';
begin
  try
    inherited Create;
    FDefaultName := '';
    FCurrentPath := '';
    FCurrentName := '';
    FCurrentFullName := '';
    FPreviousName := '';
    FLangDescription := '';
    FLoaded := False;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFileName.Reset;
const OPNAME = 'TFileName.Reset';
begin
  try
    FCurrentPath := '';
    FCurrentName := '';
    FCurrentFullName := '';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFileName.SetFileName(AFileName: string);
const OPNAME = 'TFileName.SetFileName';
begin
  try
    if(FCurrentFullName <> '') then
      FPreviousName := FCurrentFullName;

    if(AFileName = '') then
    begin
      FCurrentPath := '';
      FCurrentName := '';
      FCurrentFullName := '';
    end
    else
    begin
      FCurrentFullName  := AFileName;
      FCurrentPath := ExtractFilePath(AFileName);
      FCurrentName := ExtractFileName(AFileName);
    end;

    if(FPreviousName = '') then
      FPreviousName := FCurrentFullName;
  except on E: Exception do HandleError(E, OPNAME) end;
end;
}
{ TFileNames }
{
function TFileNames.AddFileName(AFileName: string): boolean;
const OPNAME = 'TFileNames.AddFileName';
var
  LFileName:TFileName;
begin
  Result := False;
  try
    LFileName := TFileName.Create;
    LFileName.FileName := AFileName;
    Add(LFileName);
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFileNames.AddFileObject(AFileObject: TFileName): boolean;
const OPNAME = 'TFileNames.AddFileObject';
var
  LFileName:TFileName;
begin
  Result := False;
  try
    LFileName := TFileName.Create;
    LFileName.CopyFromFile(AFileObject);
    Add(LFileName);
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFileNames.AssignFrom(AFileNames: TFileNames): boolean;
const OPNAME = 'TFileNames.AssignFrom';
var
  LCount: integer;
  LFileName:TFileName;
begin
  Result := False;
  try
    Clear;
    for LCount := 0 to AFileNames.Count-1 do
    begin
      LFileName := TFileName.Create;
      LFileName.CopyFromFile(AFileNames.DataFileNames[LCount]);
      Add(LFileName);
    end;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFileNames.CountFiles: integer;
const OPNAME = 'TFileNames.CountFiles';
var
  LCount: integer;
begin
  Result := 0;
  try
    for LCount := 0 to Count-1 do
     if (TFileName(Items[LCount]).FileName <> '') then
        Result := Result + 1;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFileNames.GetFileObject(AIndex: integer): TFileName;
const OPNAME = 'TFileNames.GetFileObject';
begin
  Result := nil;
  try
    if (AIndex < Self.Count) and Assigned(Items[AIndex]) then
      Result := TFileName(Items[AIndex]);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFileNames.Reset;
const OPNAME = 'TFileNames.Reset';
var
  LCount: integer;
begin
  try
    for LCount := 0 to Count-1 do
     TFileName(Items[LCount]).Reset;
  except on E: Exception do HandleError(E, OPNAME) end;
end;
}
{ TModelMenuData }

constructor TModelMenuData.Create(AAction: TModelMenuAction);
const OPNAME = 'TModelMenuData.Create';
begin
  try
    inherited Create;
    Action := AAction;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{ TTreeNodesList }

constructor TTreeNodesList.Create;
const OPNAME = 'TTreeNodesList.Create';
begin
  try
    inherited Create(False);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TTreeNodesList.GetNode(AIndex: integer): TTreeNode;
const OPNAME = 'TTreeNodesList.GetNode';
begin
  Result := nil;
  try
    if (AIndex < Count) then
      Result := TTreeNode(Items[AIndex]);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
