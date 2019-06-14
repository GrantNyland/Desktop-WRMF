//
//
//  UNIT      : Contains TOutputComparisonSheet Class
//  AUTHOR    : Sam Dhlamini (ARIVIA)
//  DATE      : 2007/06/11
//  COPYRIGHT : Copyright © 2007 DWAF
//
//
unit UOutputComparisonSheet;

interface

uses
  // Delphi
  Classes,
  VCL.Controls,
  VCL.ComCtrls,
  // DWAF
  UAbstractObject,
  UViewDataItem,
  UDataViewerSheet,
  UDynamicTreeViewTabSheet,
  UAbstractGridData,
  UGridActionObject,
  UAbstractComponent,
  UOutputComparitorMenuItemManager,
  //UOutputComparitorChartLegendDialog,
  UHelpContexts;

type
  TOutputComparisonSheet = class(TDynamicTreeViewTabSheet)
  protected
    FViewersCreated: boolean;
    procedure CreateMemberObjects; override;
    procedure DoTreeNodeAboutToChange(ASender: TObject; ANode: TTreeNode; var AAllowChange: Boolean); override;
    procedure DoTreeNodeHasChanged(ASender: TObject; ANode: TTreeNode); override;
    procedure PopulateDataViewer(ADataObject: TViewDataTreeNodeData); override;
    procedure AssignHelpContext; override;
    function GetToolBar: TAbstractToolBar; override;
    function GetMenuItemManager: TOutputComparitorMenuItemManager;
  public
    procedure DoOnHint; override;
    procedure DoCopyToCLipboard; override;
    procedure DoExport(AFileName: string = ''); override;
    procedure DoPrint; override;
    procedure ShowChartLegendDialog;
    function CanCopyToCLipboard: boolean; override;
    function CanExport: boolean; override;
    function CanPrint: boolean; override;

    function StudyHasChanged: boolean; override;
    function LanguageHasChanged: boolean; override;
    function StudyDataHasChanged(AContext: TChangeContext; AFieldName,AOldValue,ANewValue: string): boolean; override;
    function Initialise: boolean; override;
    property MenuItemManager: TOutputComparitorMenuItemManager read GetMenuItemManager;
    property ToolBar: TAbstractToolBar read GetToolBar;

  end;

implementation

uses
  // Delphi
  SysUtils,
  Variants,
   VCL.Dialogs,
  // DWAF
  UUtilities,
  UConstants,
  VoaimsCom_TLB,
  UDataComponent,
  UErrorHandlingOperations,
  UTreeViewTabSheet;

procedure TOutputComparisonSheet.CreateMemberObjects;
const OPNAME = 'TOutputComparisonSheet.CreateMemberObjects';
begin
  inherited CreateMemberObjects;
  try
    FViewersCreated := False;

    // Set ancestor properties.
    ShowHint := False;
    ParentShowHint := False;

    // Set defaults.
    FTabCaptionKey    := 'OutputComparisonSheet';
    FViewTypeConstant := 'Comparison';
    FModelTabSheetName := mtsnOutput;

    FMenuItemManager := TOutputComparitorMenuItemManager.Create(FAppModules);

  // Handle exceptions.
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TOutputComparisonSheet.LanguageHasChanged: boolean;
const OPNAME = 'TOutputComparisonSheet.LanguageHasChanged';
begin
  Result := inherited LanguageHasChanged;
  try
    FMenuItemManager.LanguageHasChanged;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputComparisonSheet.AssignHelpContext;
const OPNAME = 'TOutputComparisonSheet.AssignHelpContext';
begin
  try
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TOutputComparisonSheet.Initialise: boolean;
const OPNAME = 'TOutputComparisonSheet.Initialise';
begin
  Result := inherited Initialise;
  try
    FMenuItemManager.Initialise;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TOutputComparisonSheet.DoTreeNodeAboutToChange(ASender: TObject; ANode: TTreeNode; var AAllowChange: Boolean);
const OPNAME = 'TOutputComparisonSheet.DoTreeNodeAboutToChange';
begin
  inherited DoTreeNodeAboutToChange(ASender, ANode, AAllowChange);
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputComparisonSheet.DoOnHint;
const OPNAME = 'TOutputComparisonSheet.DoOnHint';
begin
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOutputComparisonSheet.StudyHasChanged: boolean;
const OPNAME = 'TOutputComparisonSheet.StudyHasChanged';
begin
  Result := False;
  try
    Result := inherited StudyHasChanged;
    FMenuItemManager.StudyHasChanged;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOutputComparisonSheet.CanCopyToClipboard: boolean;
const OPNAME = 'TOutputComparisonSheet.CanCopyToClipboard';
begin
  Result := False;
  try
    Result := FAppModules.Model.CanCopyToCLipboard;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOutputComparisonSheet.CanExport: boolean;
const OPNAME = 'TOutputComparisonSheet.CanExport';
begin
  Result := False;
  try
    Result := FAppModules.Model.CanExport;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOutputComparisonSheet.CanPrint: boolean;
const OPNAME = 'TOutputComparisonSheet.CanPrint';
begin
  Result := False;
  try
    Result := FAppModules.Model.CanPrint;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputComparisonSheet.DoCopyToClipboard;
const OPNAME = 'TOutputComparisonSheet.DoCopyToClipboard';
begin
  try
    FAppModules.Model.DoCopyToClipboard;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputComparisonSheet.DoExport(AFileName: string = '');
const OPNAME = 'TOutputComparisonSheet.DoExport';
begin
  try
    FAppModules.Model.DoExport(AFileName);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TOutputComparisonSheet.DoPrint;
const OPNAME = 'TOutputComparisonSheet.DoPrint';
begin
  try
    FAppModules.Model.DoPrint;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TOutputComparisonSheet.StudyDataHasChanged(AContext: TChangeContext;
  AFieldName, AOldValue, ANewValue: string): boolean;
const OPNAME = 'TOutputComparisonSheet.StudyDataHasChanged';
begin
  Result := inherited StudyDataHasChanged(AContext,AFieldName,AOldValue,ANewValue);
  try
    Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TOutputComparisonSheet.DoTreeNodeHasChanged(ASender: TObject;ANode: TTreeNode);
const OPNAME = 'TOutputComparisonSheet.DoTreeNodeHasChanged';
begin
  inherited DoTreeNodeHasChanged(ASender,ANode);
  try
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TOutputComparisonSheet.PopulateDataViewer(ADataObject: TViewDataTreeNodeData);
const OPNAME = 'TOutputComparisonSheet.PopulateDataViewer';
var
  LContextData : string;
  LModelElementID : integer;
begin
  inherited PopulateDataViewer(ADataObject);
  try
    LModelElementID := ADataObject.ViewDataNode.Weighting;
    if (ADataObject.ViewDataNode.ViewID = 'ComparisonFileSelection') and (FTreeView.Selected.Level = 0) then
    begin
      LContextData := ViewModelDataContextDataCommaText(mdvnOutputComparisonFileSelection,LModelElementID);
      FAppModules.Model.ViewOutputComparisonDialog(self, LContextData, nil);
      GetMenuItemManager.SetMenuShowChartLegendDialog(msDisable);
    end
    else
    if (ADataObject.ViewDataNode.ViewID = 'ComparisonAReservoir') and (FTreeView.Selected.Level = 0) then
    begin
      LContextData := ViewModelDataContextDataCommaText(mdvnOutputReservoirComparison,LModelElementID);
      FAppModules.Model.ViewOutputComparisonDialog(self, LContextData, nil);
      GetMenuItemManager.SetMenuShowChartLegendDialog(msEnable);
    end
    else
    if (ADataObject.ViewDataNode.ViewID = 'ComparisonChannelHeading') and (FTreeView.Selected.Level = 0) then
    begin
      LContextData := ViewModelDataContextDataCommaText(mdvnOutputChannelComparison,LModelElementID);
      FAppModules.Model.ViewOutputComparisonDialog(self, LContextData, nil);
      GetMenuItemManager.SetMenuShowChartLegendDialog(msEnable);
    end
    else
      ShowNoDataMessage;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TOutputComparisonSheet.GetToolBar: TAbstractToolBar;
const OPNAME = 'TOutputComparisonSheet.GetToolBar';
begin
  Result := nil;
  try
    Result := MenuItemManager.ToolBar;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TOutputComparisonSheet.GetMenuItemManager: TOutputComparitorMenuItemManager;
const OPNAME = 'TOutputComparisonSheet.GetMenuItemManager';
begin
  Result := nil;
  try
    Result := TOutputComparitorMenuItemManager(FMenuItemManager);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TOutputComparisonSheet.ShowChartLegendDialog;
const OPNAME = 'TOutputComparisonSheet.ShowChartLegendDialog';
var
  LIndex : integer;
  LAbstractDataPageControl : TAbstractDataPageControl;
  LDialogValidator : TAbstractDataDialogValidator;
begin
  try
    for LIndex := 0 to Self.ControlCount-1 do
    begin
      LAbstractDataPageControl := nil;
      if Self.Controls[LIndex].ClassName = 'TAbstractDataPageControl' then
        LAbstractDataPageControl := TAbstractDataPageControl(Self.Controls[LIndex]);
      if LAbstractDataPageControl <> nil then
      begin
        LDialogValidator := LAbstractDataPageControl.GetValidatorByClassName('TOutputComparisonReservoirValidator');
        if LDialogValidator <> nil then
        begin
          LDialogValidator.OnDataChange(nil);
          Exit;
        end
        else
        begin
          LDialogValidator := LAbstractDataPageControl.GetValidatorByClassName('TOutputComparisonChannelValidator');
          if LDialogValidator <> nil then
          begin
            LDialogValidator.OnDataChange(nil);
            Exit;
          end;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
