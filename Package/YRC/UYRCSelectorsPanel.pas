//
//
//  UNIT      : Contains TYRCSelectorsPanel Classes
//  AUTHOR    : Dziedzi Ramulondi(PDNA)
//  DATE      : 25/09/2002
//  COPYRIGHT : Copyright © 2002 DWAF
//
//
unit UYRCSelectorsPanel;

interface
uses                                       
  Classes,
  VCL.Controls,
  VCL.Forms,
  VCL.Buttons,
  VCL.StdCtrls,
  VCL.ExtCtrls,
  VCL.ComCtrls,
  VCL.CheckLst,
  UAbstractComponent,
  UDataComponent,
  //USelector,
  UTargetDraftSelector;


type
  TYRCSelectorsPanel = class(TAbstractPanel)
  protected
    FErrorMesg            : TRichEdit;
    FPlaneSelector        : TSelector;
    FPlottingBaseSelector : TSelector;
    FTargetDraftSelector  : TTargetDraftSelector;
    FbtnSetYMax           : TButton;
    FTargetDraftFormula   : TLabel;
    FShowTargetDraftsLabel: TLabel;
    FShowTargetDrafts     : TComboBox;
    FChartZoomLabel       : TLabel;
    FChartZoom            : TComboBox;
    FbtnAssuranceInterval : TButton;
    FChkBoxLabelOff       : TCheckBox;
    FChkBoxShowXY         : TCheckBox;
    procedure CreateMemberObjects; override;
    procedure AssignHelpContext; override;
    procedure SetTargetDraftSelectorMode(ASelectorMode:TSelectorMode);
    procedure SetPlaneSelectorChange(APlaneSelectionChange:TSelectionChange);
    procedure SetPlottingBaseSelectorChange(APlottingBaseSelectionChange:TSelectionChange);
    procedure SetTargetDraftSelectionChange(ATargetDraftSelectionChange:TTargetDraftSelectionChange);
  public
    // Overriden from Delphi.
    function LanguageHasChanged: boolean; override;
    procedure Resize; override;
    procedure ShowSelector(AVisible: boolean);
    procedure ShowTargetDraftSelector(AVisible: boolean);
    procedure SelectAllTargetDrafts;
    function PopulatePlaneSelector(AMinValue,AMaxValue: integer): boolean;
    function PopulatePlottingBaseSelector(AMinValue,AMaxValue: integer): boolean;
    function PopulateTargetDraft(ATargetDrafts: array of Double): boolean;
    procedure PopulateChartZoom;
    procedure SelectPlane(AYearNumber: integer);
    procedure SelectPlottingBase(AYearNumber: integer);
    property OnPlaneSelectorChange: TSelectionChange write SetPlaneSelectorChange;
    property OnPlottingBaseSelectorChange: TSelectionChange write SetPlottingBaseSelectorChange;
    property OnTargetDraftSelectionChange: TTargetDraftSelectionChange write SetTargetDraftSelectionChange;
    property TargetDraftSelectorMode: TSelectorMode  write SetTargetDraftSelectorMode;
    property ErrorMesg : TRichEdit              read FErrorMesg;
    property PlaneSelector: TSelector           read  FPlaneSelector;
    property PlottingBaseSelector : TSelector   read FPlottingBaseSelector;
    property btnSetYMax: TButton                read FbtnSetYMax;
    property AssuranceIntervalSelector: TButton read FbtnAssuranceInterval;
    property ChartZoom : TComboBox              read FChartZoom;
    property ShowTargetDrafts : TComboBox       read FShowTargetDrafts;
    property TargetDraftFormula : TLabel        read FTargetDraftFormula;
    property ChkBoxLabelOff       : TCheckBox   read FChkBoxLabelOff;
    property ChkBoxShowXY         : TCheckBox   read FChkBoxShowXY;
    property TargetDraftSelector: TTargetDraftSelector read  FTargetDraftSelector;
  end;

implementation

uses
  VCL.Graphics,
  SysUtils,
  UHelpContexts,
  UErrorHandlingOperations;

{ TYRCSelectorsPanel }

procedure TYRCSelectorsPanel.CreateMemberObjects;
const OPNAME = 'TYRCSelectorsPanel.CreateMemberObjects';
begin
  inherited;
  try
    Self.BevelOuter                := bvRaised;
    Self.Height                    := 64;

    FErrorMesg                     := TRichEdit.Create(Self);

    FPlaneSelector                 := TSelector.Create(Self,FAppModules);
    FPlottingBaseSelector          := TSelector.Create(Self,FAppModules);
    FTargetDraftSelector           := TTargetDraftSelector.Create(Self,FAppModules);
    FPlaneSelector.Editable        := False;
    FPlottingBaseSelector.Editable := False;

    FbtnAssuranceInterval        := TButton.Create(Self);
    FbtnAssuranceInterval.Parent := Self;

    FbtnSetYMax                  := TButton.Create(Self);
    FbtnSetYMax.Parent           := Self;

    FChartZoomLabel              := TLabel.Create(Self);
    FChartZoomLabel.Parent       := Self;
    FChartZoomLabel.Transparent  := True;

    FChartZoom                   := TComboBox.Create(Self);
    FChartZoom.Parent            := Self;
    FChartZoom.Style             := csDropDownList;

    FTargetDraftFormula                 := TLabel.Create(Self);
    FTargetDraftFormula.Parent          := Self;
    FTargetDraftFormula.Transparent     := True;
    FTargetDraftFormula.Font.Size       := 6;

    FShowTargetDraftsLabel              := TLabel.Create(Self);
    FShowTargetDraftsLabel.Parent       := Self;
    FShowTargetDraftsLabel.Transparent  := True;

    FShowTargetDrafts                   := TComboBox.Create(Self);
    FShowTargetDrafts.Parent            := Self;
    FShowTargetDrafts.Style             := csDropDownList;

    FChkBoxLabelOff                     := TCheckBox.Create(Self);
    FChkBoxLabelOff.Parent              := Self;
    FChkBoxLabelOff.Parent              := Self;

    FChkBoxShowXY                       := TCheckBox.Create(Self);
    FChkBoxShowXY.Parent                := Self;
    FChkBoxShowXY.Parent                := Self;

    FTargetDraftSelector.Parent         := Self;
    FErrorMesg.Parent                   := Self;
    FErrorMesg.ScrollBars               := ssBoth;

    FPlaneSelector.Parent               := Self;
    FPlottingBaseSelector.Parent        := Self;

    FErrorMesg.Visible                  := True;
    FTargetDraftSelector.Visible        := False;
    FPlaneSelector.Visible              := False;
    FPlottingBaseSelector.Visible       := False;
    FbtnSetYMax.Visible                 := False;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYRCSelectorsPanel.Resize;
const OPNAME = 'TYRCSelectorsPanel.Resize';
var
  LWidth: integer;
begin
  inherited;
  try
    FErrorMesg.Align            := alClient;
    LWidth                      := Self.ClientWidth div 4;

    FPlottingBaseSelector.Top   := 0;
    FPlottingBaseSelector.Left  := 7;
    FPlottingBaseSelector.Width  := LWidth;
    FPlottingBaseSelector.Height := 30;

    FPlaneSelector.Top          := 0;
    FPlaneSelector.Left         := FPlottingBaseSelector.Left + FPlottingBaseSelector.Width + 5;
    FPlaneSelector.Width        := LWidth;
    FPlaneSelector.Height       := 30;

    FTargetDraftSelector.Top    := 30;
    FTargetDraftSelector.Left   := 2;
    FTargetDraftSelector.Width  := LWidth;
    FTargetDraftSelector.Height := 30;
    FTargetDraftSelector.Resize;
    
    FTargetDraftFormula.Top     := FTargetDraftSelector.Top + 5;
    FTargetDraftFormula.Left    := FTargetDraftSelector.Left + FTargetDraftSelector.Width + 5;

    FbtnAssuranceInterval.Width  := (Self.ClientWidth div 10);
    FbtnAssuranceInterval.Height := (Self.ClientHeight div 2) - 10;
    FbtnAssuranceInterval.Top    := 5;
    FbtnAssuranceInterval.Left   := Self.ClientWidth - FbtnAssuranceInterval.Width -5;

    FbtnSetYMax.Top              := FbtnAssuranceInterval.Top + FbtnAssuranceInterval.Height + 2;
    FbtnSetYMax.Left             := FbtnAssuranceInterval.Left;
    FbtnSetYMax.Height           := FbtnAssuranceInterval.Height;
    FbtnSetYMax.Width            := FbtnAssuranceInterval.Width;

    FChartZoom.Left              := FbtnSetYMax.Left -  FChartZoom.Width - 5;
    FChartZoom.Top               := FTargetDraftSelector.Top;

    FChartZoomLabel.Left         := FChartZoom.Left - FChartZoomLabel.Width - 5;
    FChartZoomLabel.Top          := FTargetDraftSelector.Top + 5;

    FShowTargetDrafts.Left       := FChartZoom.Left;
    FShowTargetDrafts.Top        := FPlottingBaseSelector.Top;

    FShowTargetDraftsLabel.Left  := FShowTargetDrafts.Left - FShowTargetDraftsLabel.Width - 5;
    FShowTargetDraftsLabel.Top   := FShowTargetDrafts.Top + 5;

    FChkBoxLabelOff.Left         := FPlaneSelector.Left + FPlaneSelector.Width;
    FChkBoxLabelOff.Top          := FShowTargetDraftsLabel.Top;

    FChkBoxShowXY.Left           := FChkBoxLabelOff.Left;
    FChkBoxShowXY.Top            := FChartZoomLabel.Top;;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYRCSelectorsPanel.PopulatePlaneSelector(AMinValue,AMaxValue: integer): boolean;
const OPNAME = 'TYRCSelectorsPanel.PopulatePlaneSelector';
begin
  Result := False;
  try
    Result := FPlaneSelector.Populate(AMinValue,AMaxValue);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYRCSelectorsPanel.PopulatePlottingBaseSelector(AMinValue,AMaxValue: integer): boolean;
const OPNAME = 'TYRCSelectorsPanel.PopulatePlottingBaseSelector';
begin
  Result := False;
  try
    Result := FPlottingBaseSelector.Populate(AMinValue,AMaxValue);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYRCSelectorsPanel.PopulateTargetDraft(ATargetDrafts: array of Double): boolean;
const OPNAME = 'TYRCSelectorsPanel.PopulateTargetDraft';
begin
  Result := False;
  try
    Result := FTargetDraftSelector.PopulateTargetDraft(ATargetDrafts);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYRCSelectorsPanel.SetPlaneSelectorChange(APlaneSelectionChange:TSelectionChange);
const OPNAME = 'TYRCSelectorsPanel.SetPlaneSelectorChange';
begin
  try
    FPlaneSelector.OnSelectionChange := APlaneSelectionChange;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYRCSelectorsPanel.SetPlottingBaseSelectorChange(APlottingBaseSelectionChange:TSelectionChange);
const OPNAME = 'TYRCSelectorsPanel.SetPlottingBaseSelectorChange';
begin
  try
    FPlottingBaseSelector.OnSelectionChange := APlottingBaseSelectionChange;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYRCSelectorsPanel.SetTargetDraftSelectorMode(ASelectorMode: TSelectorMode);
const OPNAME = 'TYRCSelectorsPanel.SetTargetDraftSelectorMode';
begin
  try
    FTargetDraftSelector.SelectorMode := ASelectorMode;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYRCSelectorsPanel.SetTargetDraftSelectionChange(ATargetDraftSelectionChange: TTargetDraftSelectionChange);
const OPNAME = 'TYRCSelectorsPanel.SetTargetDraftSelectionChange';
begin
  try
    FTargetDraftSelector.OnTargetDraftSelectionChange := ATargetDraftSelectionChange;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYRCSelectorsPanel.ShowSelector(AVisible: boolean);
const OPNAME = 'TYRCSelectorsPanel.ShowSelector';
begin
  try
    FPlaneSelector.Visible         := AVisible;
    FPlottingBaseSelector.Visible  := AVisible;
    FbtnSetYMax.Visible            := AVisible;
    if AVisible then
    begin
      Self.Caption := '';
      FErrorMesg.Visible := False;
      Resize;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYRCSelectorsPanel.ShowTargetDraftSelector(AVisible: boolean);
const OPNAME = 'TYRCSelectorsPanel.ShowTargetDraftSelector';
begin
  try
    FTargetDraftSelector.Visible := AVisible;
    if AVisible then
    begin
      Self.Caption := '';
      FErrorMesg.Visible := False;
      Resize;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYRCSelectorsPanel.LanguageHasChanged: boolean;
const OPNAME = 'TYRCSelectorsPanel.LanguageHasChanged';
var
  LIndex: integer;
begin
  Result := inherited LanguageHasChanged;
  try
    FTargetDraftSelector.LanguageHasChanged;
    FPlaneSelector.SetCaption(FAppModules.Language.GetString('TYRCSelectorsPanel.PlaneCaption'));
    FPlottingBaseSelector.SetCaption(FAppModules.Language.GetString('TYRCSelectorsPanel.PlotPlaneCaption'));
    FbtnSetYMax.Caption           := FAppModules.Language.GetString('ButtonCaption.Muximum');
    FbtnAssuranceInterval.Caption := FAppModules.Language.GetString('TTargetDraftSelector.Assurance');
    FChartZoomLabel.Caption       := FAppModules.Language.GetString('TTargetDraftSelector.ZoomSelection');
    FChkBoxLabelOff.Caption       := FAppModules.Language.GetString('YieldReliability.LabelOff');
    FChkBoxShowXY.Caption         := FAppModules.Language.GetString('YieldReliability.ShowXY');

    FShowTargetDraftsLabel.Caption := FAppModules.Language.GetString('YieldReliability.LabelCaption');
    LIndex := FShowTargetDrafts.ItemIndex;
    FShowTargetDrafts.Items.Clear;
    FShowTargetDrafts.Items.Add(FAppModules.Language.GetString('YieldReliability.ItemAllTarget'));
    FShowTargetDrafts.Items.Add(FAppModules.Language.GetString('YieldReliability.ItemAdjacent'));
    FShowTargetDrafts.Items.Add(FAppModules.Language.GetString('YieldReliability.ItemSelected'));
    if(LIndex >= 0) then
      FShowTargetDrafts.ItemIndex := LIndex
    else
      FShowTargetDrafts.ItemIndex := 0;

    if(FPlaneSelector.Visible or FTargetDraftSelector.Visible) then
    begin
      FErrorMesg.Lines.Clear;
      FErrorMesg.Visible := False;
    end
    else
    begin
      FErrorMesg.Visible := True;
      if (FErrorMesg.Lines.Count = 1) and
         (FErrorMesg.Lines[0] <> '') and
         (Pos(' ',Trim(FErrorMesg.Lines[0])) = 0)then
         Self.Caption := FAppModules.Language.GetString('TYRCSelectorsPanel.'+FErrorMesg.Lines[0]);
    end;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYRCSelectorsPanel.PopulateChartZoom;
const OPNAME = 'TYRCSelectorsPanel.PopulateChartZoom';
begin
  try
    if Assigned(FChartZoom) then
    begin
      FChartZoom.Items.Clear;
      FChartZoom.Items.CommaText := FAppModules.Language.GetString('TTargetDraftSelector.ViewToggle');
      FChartZoom.ItemIndex := 0;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYRCSelectorsPanel.SelectAllTargetDrafts;
const OPNAME = 'TYRCSelectorsPanel.SelectAllTargetDrafts';
begin
  try
    FTargetDraftSelector.SelectAllTargetDrafts;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYRCSelectorsPanel.SelectPlottingBase(AYearNumber: integer);
const OPNAME = 'TYRCSelectorsPanel.SelectPlottingBase';
begin
  try
    FPlottingBaseSelector.Select(AYearNumber);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYRCSelectorsPanel.SelectPlane(AYearNumber: integer);
const OPNAME = 'TYRCSelectorsPanel.SelectPlane';
begin
  try
    FPlaneSelector.Select(AYearNumber);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYRCSelectorsPanel.AssignHelpContext;
const OPNAME = 'TYRCSelectorsPanel.AssignHelpContext';
begin
  try
    SetControlHelpContext(Self,                  HC_YieldReliabilityCurves);
    SetControlHelpContext(FErrorMesg,            HC_YieldReliabilityCurves);
    SetControlHelpContext(FPlaneSelector,        HC_YieldReliabilityCurves);
    SetControlHelpContext(FPlottingBaseSelector, HC_YieldReliabilityCurves);
    SetControlHelpContext(FTargetDraftSelector,  HC_YieldReliabilityCurves);
    SetControlHelpContext(FShowTargetDrafts,     HC_YieldReliabilityCurves);
    SetControlHelpContext(FChartZoom,            HC_YieldReliabilityCurves);
    SetControlHelpContext(FChkBoxLabelOff,       HC_YieldReliabilityCurves);
    SetControlHelpContext(FChkBoxShowXY,         HC_YieldReliabilityCurves);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
