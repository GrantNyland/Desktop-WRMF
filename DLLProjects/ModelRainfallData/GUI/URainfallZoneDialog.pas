{******************************************************************************}
{*  UNIT      : Contains TRainfallZoneDialog Class                            *}
{*  AUTHOR    : RH Steyn(Cornastone)                                          *}
{*  DATE      : 10/10/2005                                                    *}
{*  COPYRIGHT : Copyright © 2005 DWAF                                         *}
{******************************************************************************}

unit URainfallZoneDialog;

interface
{$WARN UNIT_PLATFORM OFF}
uses
  Classes,
  VCLTee.Chart,
  VCLTee.Series,
  VCLTee.TeeProcs,
  VCLTee.TeEngine,
  VCL.Graphics,
  VCL.stdctrls,
  VCL.ComCtrls,
  VCL.Controls,
  VCL.extctrls,
  VCL.CheckLst,
  Math,
  VCL.Dialogs,
  VCL.Forms,
  Windows,
  Contnrs,

  UHelpContexts,
  UAbstractObject,
  UAbstractComponent,
  URainfallCommonGaugeSheet,
  URainfallGaugeStatsMenuItemManager,
  UMenuItemManager,
  UGenericModelLinkClasses;

type

  TRainfallZoneDialog = class (TRainfallCommonGaugeSheet)
  protected
    FPanelButton : TPanel;
    FPanelSpacer : TPanel;
    FGridPanel : TPanel;
    FIncludeUnreliableChkBox : TCheckBox;
    FZoneListBox : TListBox;
    FZoneListPanel : TPanel;
    FPanelHDYP08 : TPanel;
    FHorSplitter : TSplitter;
    FVerSplitter : TSplitter;
    FlblFirstYear : TLabel;
    FCbxFirstYear : TComboBox;
    FlblLastYear : TLabel;
    FCbxLastYear : TComboBox;
    FgbHDYP08 : TGroupBox;
    FbtnRunHDYP08 : TButton;
    FPeriodRadioGroup : TRadioGroup;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    procedure AssignHelpContext; override;
    procedure Resize; override;
  public

    function Initialise: boolean; override;
    property PanelButton           : TPanel         read FPanelButton;
    property IncludeUnreliableChkBox : TCheckBox    read FIncludeUnreliableChkBox;
    property ZoneListBox : TListBox read FZoneListBox;
    property PanelHDYP08 : TPanel read FPanelHDYP08;
    property CbxFirstYear : TComboBox read FCbxFirstYear;
    property CbxLastYear : TComboBox read FCbxLastYear;
    property btnRunHDYP08 : TButton read FbtnRunHDYP08;
    property gbHDYP08 : TGroupBox read FgbHDYP08;
    property PeriodRadioGroup : TRadioGroup read FPeriodRadioGroup;
    property lblLastYear : TLabel read FlblLastYear;
    property lblFirstYear : TLabel read FlblFirstYear;
  end;

implementation

uses
  SysUtils,
  VCL.ImgList,
  VCL.FileCtrl,
  UDatasetType,
  VCL.Printers,
  UConstants,
  UErrorHandlingOperations;

{ TRainfallZoneDialog }

procedure TRainfallZoneDialog.CreateMemberObjects;
const OPNAME = 'TRainfallZoneDialog.CreateMemberObjects';
begin
  inherited;
  try
    FPanelSpacer             := TPanel.Create(Self);
    FPanelSpacer.Parent      := FPanelLeft;
    FPanelSpacer.Align       := alTop;
    FPanelSpacer.Height      := 20;
    FPanelSpacer.BorderStyle := bsNone;
    FPanelSpacer.BevelInner  := bvNone;
    FPanelSpacer.BevelOuter  := bvNone;

    FPanelButton             := TPanel.Create(Self);
    FPanelButton.Parent      := FPanelClient;
    FPanelButton.Align       := alTop;
    FPanelButton.Height      := 20;
    FPanelButton.Top         := 0;
    FPanelButton.BorderStyle := bsNone;
    FPanelButton.BevelInner  := bvNone;
    FPanelButton.BevelOuter  := bvNone;

    FPanelGrid.Top       := FPanelButton.Top + FPanelButton.Height;
    FPanelGrid.Align     := alClient;

    
    FTvwGauges.MultiSelect := False;
    FTvwGauges.ReadOnly    := True;

    FZoneListPanel := TPanel.Create(Self);
    FZoneListPanel.Parent := FPanelClient;
    FZoneListPanel.Align       := alLeft;
    FZoneListPanel.Width       := 230;

    FZoneListBox             := TListBox.Create(FZoneListPanel);
    FZoneListBox.Parent      := FZoneListPanel;
    FZoneListBox.Align       := alTop;
    FZoneListBox.Height       := 500;

    FIncludeUnreliableChkBox           := TCheckBox.Create(Self);
    FIncludeUnreliableChkBox.Parent    := FPanelButton;
    FIncludeUnreliableChkBox.Left      := 1 + FZoneListPanel.Width;
    FIncludeUnreliableChkBox.Top       := 3;
    FIncludeUnreliableChkBox.Width     := 200;
    FIncludeUnreliableChkBox.Height    := 13;
    FIncludeUnreliableChkBox.Alignment := taRightJustify;
    FIncludeUnreliableChkBox.Caption   := FAppModules.Language.GetString('CheckBox.UnreliableData');

    FVerSplitter         := TSplitter.Create(FPanelClient);
    FVerSplitter.Parent  := FPanelClient;
    FVerSplitter.Align   := alLeft;
    FVerSplitter.Top     := FPanelGrid.Top + FPanelGrid.Height;
    FVerSplitter.Left    := FZoneListBox.Width + 1;
    FVerSplitter.Height  := 4;
    FVerSplitter.Beveled := TRUE;

    FGrdZone.Parent          := FPanelClient;
    FGrdZone.Align           := alClient;

    FPanelHDYP08             := TPanel.Create(FPanelClient);
    FPanelHDYP08.Parent      := FZoneListPanel;
    FPanelHDYP08.Height      := 250;
//    FPanelHDYP08.Top         := FZoneListBox.Height + 3;
    FPanelHDYP08.Left        := FZoneListBox.Width + 1;
    FPanelHDYP08.Align       := alBottom;

    FHorSplitter         := TSplitter.Create(Self);
    FHorSplitter.Parent  := FZoneListPanel;
    FHorSplitter.Align   := alBottom;
    FHorSplitter.Left    := FZoneListBox.Width + 1;
    FHorSplitter.Height  := 4;
    FHorSplitter.Beveled := TRUE;

//HDYP08+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

    FPeriodRadioGroup := TRadioGroup.Create(Self);
    FPeriodRadioGroup.Parent := FPanelHDYP08;
    FPeriodRadioGroup.Left   := 10;
    FPeriodRadioGroup.Height := 40;
    FPeriodRadioGroup.Top    := 10;
    FPeriodRadioGroup.Width  := 130;
    FPeriodRadioGroup.Columns := 2;

    FgbHDYP08 := TGroupBox.Create(Self);
    FgbHDYP08.Parent := FPanelHDYP08;
    FgbHDYP08.Left   := 10;
    FgbHDYP08.Height := 70;
    FgbHDYP08.Top    := 60;
    FgbHDYP08.Width  := 200;

    FlblFirstYear         := TLabel.Create(Self);
    FlblFirstYear.Parent  := FgbHDYP08;
    FlblFirstYear.Caption := FAppModules.Language.GetString('LabelText.FirstYear');
    FlblFirstYear.Left    := 10;
    FlblFirstYear.Top     := 10;

    FCbxFirstYear        := TComboBox.Create(Self);
    FCbxFirstYear.Parent := FgbHDYP08;
    FCbxFirstYear.Top    := 10;
    FCbxFirstYear.Width  := 80;
    FCbxFirstYear.Left   := 100;
    FCbxFirstYear.Style  := csDropDown;
    FCbxFirstYear.AutoComplete := FALSE;

    FlblLastYear         := TLabel.Create(Self);
    FlblLastYear.Parent  := FgbHDYP08;
    FlblLastYear.Caption := FAppModules.Language.GetString('LabelText.LastYear');
    FlblLastYear.Left    := 10;
    FlblLastYear.Top     := 40;

    FCbxLastYear        := TComboBox.Create(Self);
    FCbxLastYear.Parent := FgbHDYP08;
    FCbxLastYear.Top    := 40;
    FCbxLastYear.Width  := 80;
    FCbxLastYear.Left   := 100;
    FCbxLastYear.Style  := csDropDown;
    FCbxLastYear.AutoComplete := FALSE;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

    FbtnRunHDYP08               := TButton.Create(Self);
    FbtnRunHDYP08.Parent        := FPanelHDYP08;
    FbtnRunHDYP08.Caption       := FAppModules.Language.GetString('ButtonCaption.CreateCatchment');//FAppModules.Language.GetString('ButtonCaption.Run');
    FbtnRunHDYP08.Width         := 100;
    FbtnRunHDYP08.Left          := 100;
    FbtnRunHDYP08.Top           := 140;
//    FbtnRunHDYP08.OnClick       := RunHDYP08;

  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TRainfallZoneDialog.DestroyMemberObjects;
const OPNAME = 'TRainfallZoneDialog.DestroyMemberObjects';
begin
  inherited DestroyMemberObjects;
  try
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TRainfallZoneDialog.Initialise: boolean;
const OPNAME = 'TRainfallZoneDialog.Initialise';
var
  LIndex : integer;
begin
  Result := inherited Initialise;
  try
    FPeriodRadioGroup.Caption := FAppModules.Language.GetString('RadioGroupCaption.ChangeYears');
    FPeriodRadioGroup.Items.Clear;
    FPeriodRadioGroup.Items.Add('No');
    FPeriodRadioGroup.Items.Add('Yes');
    FPeriodRadioGroup.ItemIndex := 0;
    for LIndex := 1900 to 2040 do
    begin
      FCbxFirstYear.Items.Add(IntToStr(LIndex));
      FCbxLastYear.Items.Add(IntToStr(LIndex));
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TRainfallZoneDialog.AssignHelpContext;
const OPNAME = 'TRainfallZoneDialog.AssignHelpContext';
begin
  inherited;
  try
     SetControlHelpContext(FTvwGauges, HC_DisplayMultipleGauges);
  except on E : Exception do HandleError(E,OPNAME); end;
end;


procedure TRainfallZoneDialog.Resize;
const OPNAME = 'TRainfallZoneDialog.Resize';
begin
  try
    inherited;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

end.



