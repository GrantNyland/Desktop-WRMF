{******************************************************************************}
{*  UNIT      : Contains TRainfallGaugeSelectionDialog Class                  *}
{*  AUTHOR    : RH Steyn(Cornastone)                                          *}
{*  DATE      : 04/05/2005                                                    *}
{*  COPYRIGHT : Copyright © 2005 DWAF                                         *}
{******************************************************************************}

unit URainfallGaugeSelectionDialog;

interface
{$WARN UNIT_PLATFORM OFF}
uses
  Classes,
  VCL.stdctrls,
  VCL.ComCtrls,
  VCL.extctrls,
  VCL.CheckLst,
  Math,
  VCL.Dialogs,
  VCL.Forms,
  Windows,

  UDataComponent,


  UAbstractObject,
  UAbstractComponent,
  URainfallGaugeSelectionMenuItemManager,
  URaingaugeGISPanel,
  URGProgressDialog,
  USearchDialog,
  UShellExecuteObject;

type
  { TRainfallGaugeSelectionDialog class }
  TRainfallGaugeSelectionDialog = class(TAbstractScrollablePanel)
  protected
    FPnlGaugeSelection     : TPanel;
    FStatusBar             : TStatusBar;
    FPnlTreeView           : TPanel;
    FPnlSelectedList       : TPanel;
    FPanelForGISViewer     : TPanel;
    FPnlGaugeInfo          : TPanel;
    FLblTreeview           : TLabel;
    FLblSelectedList       : TLabel;
    FSplVerticalMain       : TSplitter;
    FSplVerticalSub        : TSplitter;
    FSplHorisontal         : TSplitter;
    FGaugeCheckListBox     : TCheckListBox;
    FTrvGaugeList          : TTreeView;
    FSelectedGaugesListBox : TListBox;
    FLblStationNumber      : TLabel;
    FLblStationName        : TLabel;
    FLblStationLongitude   : TLabel;
    FLblStationLatitude    : TLabel;
    FLblStationNumberValue : TLabel;
    FLblStationNameValue   : TLabel;
    FBtnUpdateGISViewer    : TButton;
    FRaingaugeGISPanel     : TRaingaugeGISPanel;

    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    procedure AssignHelpContext; override;
    procedure Resize; override;
    procedure SetupSystemMenuState;

  public
    function Initialise: boolean; override;
    property RaingaugeGISPanel     : TRaingaugeGISPanel read FRaingaugeGISPanel;
    property GaugeSelectionPanel   : TPanel             read FPnlGaugeSelection;
    property GaugeCheckListBox     : TCheckListBox      read FGaugeCheckListBox;
    property SelectedGaugesListBox : TListBox           read FSelectedGaugesListBox;
    property StatusBar             : TStatusBar         read FStatusBar;
    property GaugeListTreeview     : TTreeView          read FTrvGaugeList;
    property BtnUpdateGISViewer    : TButton            read FBtnUpdateGISViewer;
    property LblStationLongitude   : TLabel             read FLblStationLongitude;
    property LblStationLatitude    : TLabel             read FLblStationLatitude;
    property LblStationNumberValue : TLabel             read FLblStationNumberValue;
    property LblStationNameValue   : TLabel             read FLblStationNameValue;
    property GaugeInfoPanel        : TPanel             read FPnlGaugeInfo;

end;

implementation

uses
  SysUtils,
  VCL.Controls,
  VCL.FileCtrl,
  VCL.Graphics,
  VCL.Clipbrd,
  VCL.Printers,
  UHelpContexts,
  UGenericModelLinkClasses,
  UErrorHandlingOperations;

{ TRainfallGaugeSelectionDialog }

procedure TRainfallGaugeSelectionDialog.CreateMemberObjects;
const OPNAME = 'TRainfallGaugeSelectionDialog.CreateMemberObjects';
const LPracticalHeight : integer = 300;
begin
  inherited CreateMemberObjects;

  try
    FPnlGaugeSelection        := TPanel.Create(Self);
    FPnlGaugeSelection.Parent := Self;

    FSplVerticalMain          := TSplitter.Create(Self);
    FSplVerticalMain.Parent   := Self;

    FPanelForGISViewer        := TPanel.Create(Self);
    FPanelForGISViewer.Parent := Self;
    FPanelForGISViewer.BevelOuter := bvNone;

    FPnlTreeView              := TPanel.Create(FPnlGaugeSelection);
    FPnlTreeView.Parent       := FPnlGaugeSelection;

    FSplVerticalSub           := TSplitter.Create(FPnlGaugeSelection);
    FSplVerticalSub.Parent    := FPnlGaugeSelection;

    FPnlSelectedList          := TPanel.Create(FPnlGaugeSelection);
    FPnlSelectedList.Parent   := FPnlGaugeSelection;

    FLblTreeview              := TLabel.Create(FPnlTreeView);
    FLblTreeview.Parent       := FPnlTreeView;

    FTrvGaugeList             := TTreeView.Create(FPnlTreeView);
    FTrvGaugeList.Parent      := FPnlTreeView;

    FSplHorisontal            := TSplitter.Create(FPnlTreeView);
    FSplHorisontal.Parent     := FPnlTreeView;

    FGaugeCheckListBox        := TCheckListBox.Create(FPnlTreeView);
    FGaugeCheckListBox.Parent := FPnlTreeView;

    FLblSelectedList          := TLabel.Create(FPnlSelectedList);
    FLblSelectedList.Parent   := FPnlSelectedList;

    FSelectedGaugesListBox        := TListBox.Create(FPnlSelectedList);
    FSelectedGaugesListBox.Parent := FPnlSelectedList;

    FPnlGaugeInfo             := TPanel.Create(FPnlSelectedList);
    FPnlGaugeInfo.Parent      := FPnlSelectedList;
    FPnlGaugeInfo.BevelOuter  := bvNone;

    FLblStationNumber      := TLabel.Create(FPnlGaugeInfo);
    FLblStationNumberValue := TLabel.Create(FPnlGaugeInfo);
    FLblStationName        := TLabel.Create(FPnlGaugeInfo);
    FLblStationNameValue   := TLabel.Create(FPnlGaugeInfo);
    FLblStationLongitude   := TLabel.Create(FPnlGaugeInfo);
    FLblStationLatitude    := TLabel.Create(FPnlGaugeInfo);

    FLblStationNumber.Parent      := FPnlGaugeInfo;
    FLblStationNumberValue.Parent := FPnlGaugeInfo;
    FLblStationName.Parent        := FPnlGaugeInfo;
    FLblStationNameValue.Parent   := FPnlGaugeInfo;
    FLblStationLongitude.Parent   := FPnlGaugeInfo;
    FLblStationLatitude.Parent    := FPnlGaugeInfo;

    FBtnUpdateGISViewer        := TButton.Create(FPanelForGISViewer);
    FBtnUpdateGISViewer.Parent := FPanelForGISViewer;

    FRaingaugeGISPanel     := TRaingaugeGISPanel.Create(nil, FAppModules);

    FStatusBar         := TStatusBar.Create(Self);
    FStatusBar.Parent  := Self;

  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TRainfallGaugeSelectionDialog.DestroyMemberObjects;
const OPNAME = 'TRainfallGaugeSelectionDialog.DestroyMemberObjects';
begin
  inherited DestroyMemberObjects;
  try
    SaveState;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TRainfallGaugeSelectionDialog.Initialise: boolean;
const OPNAME = 'TRainfallGaugeSelectionDialog.Initialise';
var
  LIndex              : integer;
  LTrvGaugeListHeight : integer;
  LCaption            : String;
begin
  Result := False;
  try
    LCaption := FAppModules.Language.GetString('ButtonCaption.Rainfall');
    FStatusBar.Panels := TStatusPanels.Create(FStatusBar);
    for LIndex := 0 to 3 do
    begin
      FStatusBar.Panels.Insert(LIndex);
      FStatusBar.Panels[LIndex].Width := 120;
    end;

    FPnlGaugeSelection.Left   := 0;
    FPnlGaugeSelection.Width  := 220;
    FPnlGaugeSelection.Align  := alLeft;

    FSplVerticalMain.Left  := FPnlGaugeSelection.Left + FPnlGaugeSelection.Width + 1;
    FSplVerticalMain.Align := alLeft;

    FPanelForGISViewer.Align := alClient;

    FPnlTreeView.Width := 110;
    FPnlTreeView.Align := alLeft;

    FSplVerticalSub.Left  := FPnlTreeView.Left + FPnlTreeView.Width + 1;
    FSplVerticalSub.Align := alLeft;

    FPnlSelectedList.Left  := FSplVerticalSub.Left + FSplVerticalSub.Width + 1;
    FPnlSelectedList.Align := alClient;

    FLblTreeview.Caption     := FAppModules.Language.GetString('LabelText.SAWSBlocks');
    FLblTreeview.Align       := alTop;
    FLblTreeview.Alignment   := taCenter;

    FLblSelectedList.Caption   := FAppModules.Language.GetString('LabelText.SelectedGauges');
    FLblSelectedList.Align     := alTop;
    FLblSelectedList.Alignment := taCenter;

    FLblStationNumber.Caption      := FAppModules.Language.GetString('LabelText.StationNumber');
    FLblStationNumber.Top          := 5;
    FLblStationNumber.Left         := 5;

    FLblStationNumberValue.Caption := '';
    FLblStationNumberValue.Top     := FLblStationNumber.Top + FLblStationNumber.Height + 5;
    FLblStationNumberValue.Left    := 5;

    FLblStationName.Caption        := FAppModules.Language.GetString('LabelText.StationName');
    FLblStationName.Top            := FLblStationNumberValue.Top + FLblStationNumberValue.Height + 5;
    FLblStationName.Left           := 5;

    FLblStationNameValue.Caption   := '';
    FLblStationNameValue.Top       := FLblStationName.Top + FLblStationName.Height + 5;
    FLblStationNameValue.Left      := 5;

    FLblStationLongitude.Caption   := FAppModules.Language.GetString('LabelText.StationLongitute');
    FLblStationLongitude.Top       := FLblStationNameValue.Top + FLblStationNameValue.Height + 5;
    FLblStationLongitude.Left      := 5;

    FLblStationLatitude.Caption    := FAppModules.Language.GetString('LabelText.StationLatitude');
    FLblStationLatitude.Top        := FLblStationLongitude.Top + FLblStationLongitude.Height + 5;
    FLblStationLatitude.Left       := 5;

    LTrvGaugeListHeight    := FAppModules.ViewIni.ReadInteger(ClassName,'PanelHeight', Height);
    FTrvGaugeList.Height   := Max(230, Min(LTrvGaugeListHeight, Height div 2));
    FTrvGaugeList.Align    := alTop;

    FSplHorisontal.Top    := FTrvGaugeList.Top + FTrvGaugeList.Height + 1;
    FSplHorisontal.Height := 3;
    FSplHorisontal.Align  := alTop;

    FGaugeCheckListBox.Align  := alClient;

    FPnlGaugeInfo.Left   := FSplVerticalMain.Left + FSplVerticalMain.Width + 1;
    FPnlGaugeInfo.Height := 160;
    FPnlGaugeInfo.Align  := alBottom;

    FSelectedGaugesListBox.Align  := alClient;
    FSelectedGaugesListBox.Sorted := true;
    if (Parent <> nil) then
    begin
      FSelectedGaugesListBox.Height := Self.ClientHeight - FLblSelectedList.Top - FLblSelectedList.Height - FPnlGaugeInfo.Height - 2;
      FSelectedGaugesListBox.Height := Self.ClientHeight - FLblSelectedList.Top - FLblSelectedList.Height - FPnlGaugeInfo.Height - 2;
    end;

    FBtnUpdateGISViewer.Align := alBottom;
    FBtnUpdateGISViewer.Caption := LCaption;

    FBtnUpdateGISViewer.Caption  := LCaption;
    FSelectedGaugesListBox.MultiSelect := True;
    FSelectedGaugesListBox.Sorted := true;

    if Assigned ( FRaingaugeGISPanel ) then
    begin
      FRaingaugeGISPanel.Parent := FPanelForGISViewer;
      FRaingaugeGISPanel.Align := alClient;
      FRaingaugeGISPanel.Visible := True;
    end;

    Result := FRaingaugeGISPanel.Initialise;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TRainfallGaugeSelectionDialog.AssignHelpContext;
const OPNAME = 'TRainfallGaugeSelectionDialog.AssignHelpContext';
begin
  inherited;
  try
    SetControlHelpContext(FGaugeCheckListBox, HC_GaugeSelection);
    SetControlHelpContext(FRaingaugeGISPanel, HC_LayerSelection);
    SetControlHelpContext(FTrvGaugeList, HC_SAWSBlocks);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TRainfallGaugeSelectionDialog.Resize;
const OPNAME = 'TRainfallGaugeSelectionDialog.Resize';
begin
  try
    inherited Resize;
    SetupSystemMenuState;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TRainfallGaugeSelectionDialog.SetupSystemMenuState;
const OPNAME = 'TRainfallGaugeSelectionDialog.SetupSystemMenuState';
begin
  try
    if Assigned(FAppModules.MainForm()) then
    begin
      FAppModules.MainForm.MenuItemManager.SetClipboardEnabled(True);
      FAppModules.MainForm.MenuItemManager.SetExportEnabled(True);
      FAppModules.PrintManager.SetPrintEnabled(True);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.


