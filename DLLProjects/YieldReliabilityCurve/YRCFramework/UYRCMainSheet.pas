//
//
//  UNIT      : Contains TYRCMainSheet Class
//  AUTHOR    : Dziedzi (PDNA)
//  DATE      : 2002/03/02
//  COPYRIGHT : Copyright © 2002 DWAF
//
//
unit UYRCMainSheet;

interface

uses
  DB,
  VCL.Menus,
  Classes,
  VCL.stdctrls,
  VCL.ComCtrls,
  contnrs,
  VCL.extctrls,
  VCL.CheckLst,
  VCL.Dialogs,
  UAbstractObject,
  UAbstractComponent,
  UGenericModelLinkClasses,
  UMenuItemManager,
  UResultYRCSheet,
  UYRCFirmYieldSheet;
type
  TYRCMainSheet = class(TAbstractTabSheet)
  protected
    FPageControl    : TAbstractPageControl;
    FYRCSheet       : TResultYRCSheet;
    FFirmYieldSheet : TYRCFirmYieldSheet;
    procedure CreateMemberObjects; override;
    function GetToolBar: TAbstractToolBar; override;
    function GetMenuItemManager: TMenuItemManager;
    procedure PageControlPageChange(Sender: TObject);
    procedure OnTabChangeRequest(Sender: TObject; var AllowChange: Boolean);
  public
    procedure Resize; override;
    procedure SetMenuVisible(AVisible: boolean); override;
    function Initialise: boolean; override;
    function LanguageHasChanged: boolean; override;
    function StudyHasChanged: boolean; override;
    function StudyDataHasChanged(AContext: TChangeContext; AFieldName,AOldValue,ANewValue: string): boolean; override;
    function CanCopyToCLipboard: boolean; override;
    function CanExport: boolean; override;
    function CanPrint: boolean; override;
    procedure DoCopyToCLipboard; override;
    procedure DoExport(AFileName: string = ''); override;
    procedure DoPrint; override;
    function DoCustomTabSheetEvent(ACustomModelEvent: TModelMenuData): boolean; override;
    property MenuItemManager  : TMenuItemManager read GetMenuItemManager;
  end;

implementation

{$WARN UNIT_PLATFORM OFF}

uses
  windows,
  VCL.Forms,
  VCL.Graphics,
  SysUtils,
  VCL.Controls,
  VCL.FileCtrl,
  UUtilities,
  UConstants,
  UFileNameConstants,
  UMainMenuEventType,
  UErrorHandlingOperations;

{ TYRCMainSheet }

procedure TYRCMainSheet.CreateMemberObjects;
const OPNAME = 'TYRCMainSheet.CreateMemberObjects';
begin
  inherited CreateMemberObjects;
  try
    FPageControl    := TAbstractPageControl.Create(Self,FAppModules);
    FYRCSheet       := TResultYRCSheet.Create(Self, FAppModules,False);
    FFirmYieldSheet := TYRCFirmYieldSheet.Create(Self, FAppModules);
    FTabCaptionKey  := 'YRC';
    FPageControl.Parent := Self;
    FYRCSheet.Parent     := Self;
    FFirmYieldSheet.Parent := Self;
    FPageControl.OnChange       := PageControlPageChange;
    FPageControl.OnChanging     := OnTabChangeRequest;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TYRCMainSheet.Initialise: boolean;
const OPNAME = 'TYRCMainSheet.Initialise';
begin
  Result := inherited Initialise;
  try
    FYRCSheet.PageControl       := FPageControl;
    FFirmYieldSheet.PageControl := FPageControl;
    FYRCSheet.Initialise;
    FFirmYieldSheet.Initialise;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYRCMainSheet.StudyHasChanged: boolean;
const OPNAME = 'TYRCMainSheet.StudyHasChanged';
begin
  Result := inherited StudyHasChanged;
  try
    FYRCSheet.StudyHasChanged;
    FFirmYieldSheet.StudyHasChanged;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYRCMainSheet.LanguageHasChanged: boolean;
const OPNAME = 'TYRCMainSheet.LanguageHasChanged';
begin
  Result := inherited LanguageHasChanged;;
  try
    FYRCSheet.LanguageHasChanged;
    FFirmYieldSheet.LanguageHasChanged;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYRCMainSheet.StudyDataHasChanged(AContext: TChangeContext; AFieldName,AOldValue,ANewValue: string): boolean;
const OPNAME = 'TYRCMainSheet.StudyDataHasChanged';
begin
  Result := inherited StudyDataHasChanged(AContext,AFieldName,AOldValue,ANewValue);
  try
    FYRCSheet.StudyDataHasChanged(AContext,AFieldName,AOldValue,ANewValue);
    FFirmYieldSheet.StudyDataHasChanged(AContext,AFieldName,AOldValue,ANewValue);
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYRCMainSheet.Resize;
const OPNAME = 'TYRCMainSheet.Resize';
begin
  inherited;
  try
    FPageControl.Align    := alClient;
    FYRCSheet.Align       := alClient;
    FFirmYieldSheet.Align := alClient;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYRCMainSheet.PageControlPageChange(Sender: TObject);
const OPNAME = 'TYRCMainSheet.PageControlPageChange';
begin
  try
    FAppModules.Model.ProcessEvent(CmeResultPageControlChanged,nil);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYRCMainSheet.OnTabChangeRequest(Sender: TObject; var AllowChange: Boolean);
const OPNAME = 'TYRCMainSheet.OnTabChangeRequest';
begin
  try
    FAppModules.Model.ProcessEvent(CmeResultPageControlChanging,nil)
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYRCMainSheet.GetMenuItemManager: TMenuItemManager;
const OPNAME = 'GetMenuItemManager: TMenuItemManager';
begin
  Result := nil;
  try
    case FPageControl.TabIndex of
      0: Result := FYRCSheet.MenuItemManager;
      1: Result := FFirmYieldSheet.MenuItemManager;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYRCMainSheet.GetToolBar: TAbstractToolBar;
const OPNAME = 'TYRCMainSheet.GetToolBar';
begin
  Result := nil;
  try
    case FPageControl.TabIndex of
      0: Result := FYRCSheet.ToolBar;
      1: Result := FFirmYieldSheet.ToolBar;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYRCMainSheet.CanCopyToClipboard: boolean;
const OPNAME = 'TYRCMainSheet.CanCopyToClipboard';
begin
  Result := False;
  try
    case FPageControl.TabIndex of
      0: Result := FYRCSheet.CanCopyToClipboard;
      1: Result := FFirmYieldSheet.CanCopyToClipboard;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYRCMainSheet.CanExport: boolean;
const OPNAME = 'TYRCMainSheet.CanExport';
begin
  Result := False;
  try
    case FPageControl.TabIndex of
      0: Result := FYRCSheet.CanExport;
      1: Result := FFirmYieldSheet.CanExport;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYRCMainSheet.CanPrint: boolean;
const OPNAME = 'TYRCMainSheet.CanPrint';
begin
  Result := False;
  try
    case FPageControl.TabIndex of
      0: Result := FYRCSheet.CanPrint;
      1: Result := FFirmYieldSheet.CanPrint;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYRCMainSheet.DoCopyToClipboard;
const OPNAME = 'TYRCMainSheet.DoCopyToClipboard';
begin
  try
    case FPageControl.TabIndex of
      0: FYRCSheet.DoCopyToClipboard;
      1: FFirmYieldSheet.DoCopyToClipboard;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYRCMainSheet.DoExport(AFileName: string = '');
const OPNAME = 'TYRCMainSheet.DoExport';
begin
  try
    case FPageControl.TabIndex of
      0: FYRCSheet.DoExport(AFileName);
      1: FFirmYieldSheet.DoExport(AFileName);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYRCMainSheet.DoPrint;
const OPNAME = 'TYRCMainSheet.DoPrint';
begin
  try
    case FPageControl.TabIndex of
      0: FYRCSheet.DoPrint;
      1: FFirmYieldSheet.DoPrint;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYRCMainSheet.DoCustomTabSheetEvent(ACustomModelEvent: TModelMenuData): boolean;
const OPNAME = 'TYRCMainSheet.DoCustomTabSheetEvent';
  function GetSenderForChartPopup(ACustomModelEvent: TModelMenuData): TMenuItem;
  const OPNAME = 'UYRCMainSheet.GetSenderForChartPopup';
  var
    LSender : TMenuItem;
  begin
    Result := nil;
    try
      LSender := TMenuItem.Create(nil);
      if Assigned(ACustomModelEvent) then
      case ACustomModelEvent.Action of
        meStartRegressionEdit     : LSender.Tag := 1;
        meEndRegressionEdit       : LSender.Tag := 2;
        meStartDeterministicEdit  : LSender.Tag := 3;
        meEndDeterministicEdit    : LSender.Tag := 4;
      end;
      Result := LSender;
    except on E: Exception do HandleError(E, OPNAME); end;
  end;
begin
  Result := False;
  try
    if Assigned(ACustomModelEvent) then
    begin
      Result := True;
      case ACustomModelEvent.Action of
         meResetChartData     :  FYRCSheet.ResetChartData;
         meTogglePlaneMode    :  FYRCSheet.TogglePlaneMode;
         meToggleChartMode    :  FYRCSheet.ToggleChartMode;
         meYRCChartDataLoaded :  FYRCSheet.LoadChart;
         meDeleteTargetDraft       : FYRCSheet.DeleteSeletedTargetDraft;
         meToggleCurveManipulation : FYRCSheet.ToggleCurveManipulation;
         meLoadCoefficientFile     : FYRCSheet.LoadCoefficientFile;
         meEditYValue              : FYRCSheet.OnEditYValue(nil);
         meStartRegressionEdit,
         meEndRegressionEdit,
         meStartDeterministicEdit,
         meEndDeterministicEdit    : FYRCSheet.OnChartEditPopupClick(GetSenderForChartPopup(ACustomModelEvent));

         meFirmYieldNewChart       : FFirmYieldSheet.DoNewChart;
         meFirmYieldOpenChart      : FFirmYieldSheet.DoOpenChart;
         meFirmYieldSaveChart      : FFirmYieldSheet.DoSaveChart;
         meFirmYieldAddSeries      : FFirmYieldSheet.DoAddSeries;
         meFirmYieldDeleteSeries   : FFirmYieldSheet.DoDeleteSeries;
      else
       Result := False;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TYRCMainSheet.SetMenuVisible(AVisible: boolean);
const OPNAME = 'TYRCMainSheet.SetMenuVisible';
begin
  inherited;
  try
    case FPageControl.TabIndex of
      0: FYRCSheet.SetMenuVisible(AVisible);
      1: FFirmYieldSheet.SetMenuVisible(AVisible);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.

