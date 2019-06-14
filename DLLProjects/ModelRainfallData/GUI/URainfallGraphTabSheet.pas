//  UNIT      : Contains TRainfallGraphTabSheetManager,TRainfallGraphTabSheet Class
//  AUTHOR    : Sam Dhlamini(Cornastone)
//  DATE      : 04/06/2004
//  COPYRIGHT : Copyright © 2004 DWAF
//
//

unit URainfallGraphTabSheet;
                                                       
interface

uses
  VCL.Controls,
  UAbstractObject,
  UAbstractComponent,
  UMenuItemManager,
  URainfallGraphMenuItemManager,
  URainfallGraphValidator,
  UGenericModelLinkClasses;

type
  TRainfallGraphTabSheet = class(TAbstractTabSheet)
  protected
    FMenuItemManager        : TMenuItemManager;
    FRainfallGraphValidator : TRainfallGraphValidator;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    function GetMenuItemManager : TRainfallGraphMenuItemManager;
    function GetToolBar : TAbstractToolBar; override;
  public
    function Initialise: boolean; override;
    function LanguageHasChanged: boolean; override;
    function StudyHasChanged: boolean; override;
    function StudyDataHasChanged (AContext   : TChangeContext;
                                  AFieldName : string;
                                  AOldValue  : string;
                                  ANewValue  : string): Boolean; override;
    procedure TabHasChanged; override;
    function CanTabChange: boolean; override;
    procedure SetMenuVisible ( AVisible: Boolean );override;
    function ProcessParameterChangeEvent : boolean; override;
    function ProcessMetaDataEvent : boolean; override;
    function CanCopyToCLipboard: boolean; override;
    function CanExport: boolean; override;
    function CanPrint: boolean; override;
    procedure DoCopyToCLipboard; override;
    procedure DoExport(AFileName: string = ''); override;
    procedure DoPrint; override;
    function DoCustomTabSheetEvent (ACustomModelEvent : TModelMenuData ): Boolean; override;
    property MenuItemManager: TRainfallGraphMenuItemManager read GetMenuItemManager;
  end;

implementation

uses
  SysUtils,
  UErrorHandlingOperations;

{ TRainfallGraphTabSheet }

procedure TRainfallGraphTabSheet.CreateMemberObjects;
const OPNAME = 'TRainfallGraphTabSheet.CreateMemberObjects';
begin
  inherited;
  try
    FTabCaptionKey := 'Data/Graph';
    
    FRainfallGraphValidator := TRainfallGraphValidator.Create(Self, FAppModules);
    FRainfallGraphValidator.Panel.Parent := Self;
    FRainfallGraphValidator.Panel.Align  := alClient;
    
    FMenuItemManager := TRainfallGraphMenuItemManager.Create(FAppModules);
    FRainfallGraphValidator.MenuItemManager := TRainfallGraphMenuItemManager(FMenuItemManager);
    MenuItemManager.ToolBar.ToggleGridBtn.OnClick     := FRainfallGraphValidator.DoToggleGrid;
    MenuItemManager.ToolBar.ToggleGraphBtn.OnClick    := FRainfallGraphValidator.DoToggleGraph;
    MenuItemManager.ToolBar.ToggleTreeBtn.OnClick     := FRainfallGraphValidator.DoToggleTree;
    MenuItemManager.ToolBar.CreatePATFilesBtn.OnClick := FRainfallGraphValidator.DoCreatePATFiles;
    MenuItemManager.ToolBar.HighLightBtn.OnClick      := FRainfallGraphValidator.DoHighLightOutliers;
    MenuItemManager.ToolBar.SelectRAWFlagsBtn.OnClick := FRainfallGraphValidator.DoSelectRAWFlags;
    MenuItemManager.ToolBar.FlagDataBtn.OnClick       := FRainfallGraphValidator.DoFlagDataBlock;
    MenuItemManager.ToolBar.UnFlagDataBtn.OnClick     := FRainfallGraphValidator.DoUnFlagDataBlock;
    MenuItemManager.ToolBar.FlagSetupBtn.OnClick      := FRainfallGraphValidator.DoFlagSetup;
    MenuItemManager.ToolBar.FlagClickBtn.OnClick      := FRainfallGraphValidator.DoFlagClick;
    MenuItemManager.ToolBar.WeatherEventsBtn.OnClick  := FRainfallGraphValidator.DoWeatherEvents;
    MenuItemManager.ToolBar.PatchChangelistBtn.OnClick:= FRainfallGraphValidator.DoPatchChangeList;

    MenuItemManager.ToolBar.FirstRecordBtn.OnClick  := FRainfallGraphValidator.DoFirstWeatherRecord;
    MenuItemManager.ToolBar.PrevRecordBtn.OnClick   := FRainfallGraphValidator.DoPrevWeatherRecord;
    MenuItemManager.ToolBar.NextRecordBtn.OnClick   := FRainfallGraphValidator.DoNextWeatherRecord;
    MenuItemManager.ToolBar.LastRecordBtn.OnClick   := FRainfallGraphValidator.DoLastWeatherRecord;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TRainfallGraphTabSheet.DestroyMemberObjects;
const OPNAME = 'TRainfallGraphTabSheet.DestroyMemberObjects';
begin
  inherited DestroyMemberObjects;
  try
    FreeAndNil(FMenuItemManager);
    FreeAndNil(FRainfallGraphValidator);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TRainfallGraphTabSheet.Initialise: Boolean;
const OPNAME = 'TRainfallGraphTabSheet.Initialise';
begin
  Result := inherited initialise;
  try
    FRainfallGraphValidator.Initialise;

    FAppModules.SetMenuItem(CToggleGrid, msChecked);
    FAppModules.SetMenuItem(CToggleGraph, msChecked);
    FAppModules.SetMenuItem(CToggleTree, msChecked);

    SetMenuVisible(FALSE);
    Result := TRUE;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TRainfallGraphTabSheet.LanguageHasChanged: boolean;
const OPNAME = 'TRainfallGraphTabSheet.LanguageHasChanged';
begin
  Result := inherited LanguageHasChanged;;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRainfallGraphTabSheet.StudyHasChanged: boolean;
const OPNAME = 'TRainfallGraphTabSheet.StudyHasChanged';
begin
  Result := inherited StudyHasChanged;
  try
    Result := FRainfallGraphValidator.StudyHasChanged;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TRainfallGraphTabSheet.StudyDataHasChanged (AContext   : TChangeContext;
                                                     AFieldName : string;
                                                     AOldValue  : string;
                                                     ANewValue  : string): boolean;
const OPNAME = 'TRainfallGraphTabSheet.StudyDataHasChanged';
begin
  Result := inherited StudyDataHasChanged(AContext, AFieldName, AOldValue, ANewValue);
  try
    Result := FRainfallGraphValidator.StudyDataHasChanged(AContext, AFieldName, AOldValue, ANewValue);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TRainfallGraphTabSheet.TabHasChanged;
const OPNAME = 'TRainfallGraphTabSheet.TabHasChanged';
begin
  inherited;
  try
    FRainfallGraphValidator.PopulateDataViewer;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TRainfallGraphTabSheet.CanTabChange: Boolean;
const OPNAME = 'TRainfallGraphTabSheet.CanTabChange';
begin
  Result := TRUE;
  try
    FRainfallGraphValidator.SetQuickFlagOff;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TRainfallGraphTabSheet.CanPrint: Boolean;
const OPNAME = 'TRainfallGraphTabSheet.CanPrint';
begin
  Result := TRUE;
  try
  except on E:Exception do HandleError(E,OPNAME) end;
end;

function TRainfallGraphTabSheet.CanCopyToClipboard: Boolean;
const OPNAME = 'TRainfallGraphTabSheet.CanCopyToClipboard';
begin
  Result := TRUE;
  try
  except on E:Exception do HandleError(E,OPNAME) end;
end;

function TRainfallGraphTabSheet.CanExport: Boolean;
const OPNAME = 'TRainfallGraphTabSheet.CanExport';
begin
  Result := TRUE;
  try
  except on E:Exception do HandleError(E,OPNAME) end;
end;

procedure TRainfallGraphTabSheet.DoPrint;
const OPNAME = 'TRainfallGraphTabSheet.DoPrint';
begin
  try
    FRainfallGraphValidator.DoPrint;
  except on E:Exception do HandleError(E,OPNAME) end;
end;

procedure TRainfallGraphTabSheet.DoCopyToClipboard;
const OPNAME = 'TRainfallGraphTabSheet.DoCopyToClipboard';
begin
  try
    FRainfallGraphValidator.DoCopyToClipboard;
  except on E:Exception do HandleError(E,OPNAME) end;
end;

procedure TRainfallGraphTabSheet.DoExport(AFileName: string = '');
const OPNAME = 'TRainfallGraphTabSheet.DoExport';
begin
  try
    FRainfallGraphValidator.DoExport(AFileName);
  except on E:Exception do HandleError(E,OPNAME) end;
end;

function TRainfallGraphTabSheet.GetMenuItemManager : TRainfallGraphMenuItemManager;
const OPNAME = 'TRainfallGraphTabSheet.GetMenuItemManager';
begin
  Result := nil;
  try
    Result := TRainfallGraphMenuItemManager(FMenuItemManager);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TRainfallGraphTabSheet.GetToolBar: TAbstractToolBar;
const OPNAME = 'TRainfallGraphTabSheet.GetToolBar';
begin
  Result := nil;
  try
    Result :=  MenuItemManager.ToolBar;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TRainfallGraphTabSheet.SetMenuVisible (AVisible : Boolean);
const OPNAME = 'TRainfallGraphTabSheet.SetMenuVisible';
begin
  inherited SetMenuVisible(AVisible);
  try
    if Assigned(MenuItemManager) then
    begin
      if AVisible then
        MenuItemManager.Show
      else
        MenuItemManager.Hide;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TRainfallGraphTabSheet.DoCustomTabSheetEvent (ACustomModelEvent : TModelMenuData): boolean;
const OPNAME = 'TRainfallGraphTabSheet.DoCustomTabSheetEvent';
begin
  Result := False;
  try
    with FRainfallGraphValidator do
    begin
      if (ACustomModelEvent.Action = meWeatherEvents) then
      begin
        FRainfallGraphValidator.DoWeatherEvents(Self);
        Result := True;
      end
      else
      if (ACustomModelEvent.Action = meCreatePATFiles) then
      begin
        FRainfallGraphValidator.DoCreatePATFiles(Self);
        Result := True;
      end
      else
      if (ACustomModelEvent.Action = meHighLightOutliers) then
      begin
        FRainfallGraphValidator.DoHighLightOutliers(Self);
        Result := True;
      end
      else
      if (ACustomModelEvent.Action = meSelectRAWFlags) then
      begin
        FRainfallGraphValidator.DoSelectRAWFlags(Self);
        Result := True;
      end
      else
      if (ACustomModelEvent.Action = meFlagRainfallDataBlock) then
      begin
        FRainfallGraphValidator.DoFlagDataBlock(Self);
        Result := True;
      end
      else
      if (ACustomModelEvent.Action = meUnFlagRainfallDataBlock) then
      begin
        FRainfallGraphValidator.DoUnFlagDataBlock(Self);
        Result := True;
      end
      else
      if (ACustomModelEvent.Action = meFlagSetup) then
      begin
        FRainfallGraphValidator.DoFlagSetup(Self);
        Result := True;
      end
      else
      if (ACustomModelEvent.Action = meFlagClick) then
      begin
        FRainfallGraphValidator.DoFlagClick(Self);
        Result := True;
      end
      else
      if (ACustomModelEvent.Action = meToggleRainGraphGrid) then
      begin
        FRainfallGraphValidator.DoToggleGrid(Self);
        Result := True;
      end
      else
      if (ACustomModelEvent.Action = meToggleRainGraphGraph) then
      begin
        FRainfallGraphValidator.DoToggleGraph(Self);
        Result := True;
      end
      else
      if (ACustomModelEvent.Action = meToggleRainGraphTree) then
      begin
        FRainfallGraphValidator.DoToggleTree(Self);
        Result := True;
      end
      else
      if (ACustomModelEvent.Action = mePatchChangeList) then
      begin
        FRainfallGraphValidator.DoPatchChangeList(Self);
        Result := True;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRainfallGraphTabSheet.ProcessParameterChangeEvent : Boolean ;
const OPNAME = 'TRainfallGraphTabSheet.ProcessParameterChangeEvent';
begin
  Result := FALSE;
  try
    Result := FRainfallGraphValidator.ProcessParameterChangeEvent;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRainfallGraphTabSheet.ProcessMetaDataEvent : Boolean ;
const OPNAME = 'TRainfallGraphTabSheet.ProcessMetaDataEvent';
begin
  Result := FALSE;
  try
    Result := FRainfallGraphValidator.ProcessMetaDataEvent;
  except on E: Exception do HandleError(E, OPNAME) end;
end;
end.

