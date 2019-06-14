//
//
//  UNIT      : Contains the class TReservoirPenaltyDialog.
//  AUTHOR    : Valentino Naicker (arivia.kom)
//  DATE      : 2003/06/27
//  COPYRIGHT : Copyright © 2003 DWAF
//
///

unit UReservoirPenaltyDialog;

interface

uses

  // Delphi VCL, RTL
  Classes,
  VCL.Controls,
  VCL.ComCtrls,
  VCL.ExtCtrls,
  VCL.StdCtrls,
  VCL.Grids,
  VCL.Graphics,
  Windows,

  // arivia.com
  UAbstractObject,
  UAbstractComponent,
  UDataComponent,
  UDataEditComponent;

const
  C_ReservoirPenaltySize = 170;

type
  TColumnSelectGrid = class(TFieldStringGrid)
  protected
    FViewMode : TViewMode;
    FSelectedColumn: integer;
    procedure Paint; override;
  public
    function ShowSelectColumn: boolean;
    property ViewMode    : TViewMode read FViewMode write FViewMode;
    property SelectedColumn : integer read FSelectedColumn write FSelectedColumn;
  end;

  TReservoirPenaltyTreeView = Class(TFieldTreeView)
  protected
    FPennaltyStructureNumber: integer;
  public
    property PennaltyStructureNumber: integer read FPennaltyStructureNumber write FPennaltyStructureNumber;
  end;

  TReservoirPenaltyDialog = class(TAbstractScrollablePanel)
  protected
    {protected}
    FSelectedPenaltyStructureCol,
    FRuleCurveLevel          : integer;
    FOnColumnTextChange      : TNotifyEvent;
    FOnDeleteColumn          : TNotifyEvent;
    FOnAddColumn             : TNotifyEvent;

    FNumberOfPenaltyStructures : integer;  // This can be replaced
    FNumberOfStorageZones : integer;  // This can be replaced

    // Panel Containers
    FPanRadioGroup            : TAbstractPanel;
    FPanReservoir             : TAbstractPanel;
    FPanPenalty               : TAbstractPanel;

    // Group Panel
    FRadMode                   : TFieldRadioGroup;
    FGbxRule                   : TGroupBox;
    FGbxZone                   : TGroupBox;
    FGbxZoneButtons            : TGroupBox;
    FGbxPenaltyButtons         : TGroupBox;
    FEdtRuleCurve              : TFieldEdit;
    FChkBalancingData          : TFieldChkBox;
    FLblRuleCurve              : TLabel;
    FBtnAddPenalty             : TFieldBitBtn;
    FBtnDeletePenalty          : TFieldBitBtn;
    FBtnAddZone                : TFieldBitBtn;
    FBtnDeleteZone             : TFieldBitBtn;

    // Reservoir Panel
    FBtnAddReservior           : TFieldBitBtn;
    FBtnDeleteReservior        : TFieldBitBtn;


    FComboBoxArray             : array of TFieldComboBox;
    FGrdReservoir              : TFieldStringGrid;
    FTvwReservoir              : array of TReservoirPenaltyTreeView;

    // Penalty Panel
    FGrdPenalty                : TColumnSelectGrid;
    FTvwPenalty                : array of TReservoirPenaltyTreeView;

    FOnTreeViewStartDrag: TStartDragEvent;
    FOnTreeViewDragDrop: TDragDropEvent;
    FOnTreeViewDragOver: TDragOverEvent;

    procedure SetRuleCurveLevel(const Value: integer);
    procedure DrawRuleCurveDivision(ASender: TObject; ACol, ARow: Integer; ARect: TRect; AState: TGridDrawState);

    // Event Handler Setters
    procedure SetOnAddColumn(const Value: TNotifyEvent);
    procedure SetOnColumnTextChange(const Value: TNotifyEvent);
    procedure SetOnDeleteColumn(const Value: TNotifyEvent);
    procedure SetViewMode(AViewMode: TViewMode);override;
    procedure SetSelectedPenaltyStructureCol(AValue : integer);

    function GetNumberOfComboBoxes : integer;
    procedure SetNumberOfComboBoxes(const Value: integer);

    function GetPenaltyTreeViewByNumber(AIndex: integer): TReservoirPenaltyTreeView;
    function GetReservoirTreeViewByNumber(AIndex : integer) : TReservoirPenaltyTreeView;
    function GetComboBoxByIndex(AIndex: integer): TFieldComboBox;

    procedure ResizeGroupBoxControls;
    procedure ResizeReservoirModeControls;
    procedure ResizePenaltyModeControls;

    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    procedure InitialiseMemberObjects;
    procedure AssignHelpContext; override;

    // Property Setters
    procedure SetNumberOfStorageZones(const Value: integer);
    procedure SetNumberOfPenaltyStructures(const Value: integer);

    // Control Event Handler(s)
    procedure RadModeClickHandler(ASender : TObject);
    procedure ChkBalancingDataClickHandler(ASender : TObject);

    procedure AddButtonClicked(ASender : TObject);
    procedure RemoveButtonClicked(ASender : TObject);
    procedure ComboBoxChange(ASender : TObject);

    procedure PerformResizeControls;
  public

    {public}
    procedure RestoreColourState; override;

    procedure Resize; override;
    procedure ResetButtonState;
    // Framework issues
    function Initialise: boolean; override;
    function LanguageHasChanged: boolean; override;
    function SaveState: boolean; override;
    function ShowSelectPenaltyStructure: boolean;

    // Event Handlers
    property OnAddColumn        : TNotifyEvent read FOnAddColumn        write SetOnAddColumn;
    property OnDeleteColumn     : TNotifyEvent read FOnDeleteColumn     write SetOnDeleteColumn;
    property OnColumnTextChange : TNotifyEvent read FOnColumnTextChange write SetOnColumnTextChange;

    // Read write properties
    property NumberOfStorageZones              : integer           read FNumberOfStorageZones    write SetNumberOfStorageZones;
    property NumberOfPenaltyStructures         : integer           read FNumberOfPenaltyStructures write SetNumberOfPenaltyStructures;
    property RuleCurveLevel                    : integer           read FRuleCurveLevel            write SetRuleCurveLevel;
    property SelectedPenaltyStructureCol       : integer           read FSelectedPenaltyStructureCol  write SetSelectedPenaltyStructureCol;

    // Read only properties : Returns GUI Elements whose properties will be changed
    property EdtRuleCurve                      : TFieldEdit        read FEdtRuleCurve;
    property GrdPenalty                        : TColumnSelectGrid read FGrdPenalty;
    property GrdReservoir                      : TFieldStringGrid  read FGrdReservoir;

    property PenaltyTreeViewByIndex[AIndex : integer]   : TReservoirPenaltyTreeView read GetPenaltyTreeViewByNumber;
    property ReservoirTreeViewByIndex[AIndex : integer] : TReservoirPenaltyTreeView read GetReservoirTreeViewByNumber;
    property ComboBoxByIndex[AIndex : integer]          : TFieldComboBox            read GetComboBoxByIndex;

    property NumberOfComboBoxes                : integer read GetNumberOfComboBoxes write SetNumberOfComboBoxes;

    property OnTreeViewStartDrag: TStartDragEvent read FOnTreeViewStartDrag write FOnTreeViewStartDrag;
    property OnTreeViewDragOver: TDragOverEvent   read FOnTreeViewDragOver  write FOnTreeViewDragOver;
    property OnTreeViewDragDrop: TDragDropEvent   read FOnTreeViewDragDrop  write FOnTreeViewDragDrop;

    property RadMode: TFieldRadioGroup  read FRadMode;
    property ChkBalancingData   : TFieldChkBox read FChkBalancingData;
    property BtnAddReservior    : TFieldBitBtn read FBtnAddReservior;
    property BtnDeleteReservior : TFieldBitBtn read FBtnDeleteReservior;
    property BtnAddPenalty      : TFieldBitBtn read FBtnAddPenalty;
    property BtnDeletePenalty   : TFieldBitBtn read FBtnDeletePenalty;
    property BtnAddZone         : TFieldBitBtn read FBtnAddZone;
    property BtnDeleteZone      : TFieldBitBtn read FBtnDeleteZone;
  end;

implementation

uses

  // Delphi VCL, RTL, etc
  VCL.Forms,
  Math,
  UHelpContexts,
  UConstants,
  SysUtils,

  // arivia.kom
  UErrorHandlingOperations;

{ TReservoirPenaltyDialog }

procedure TReservoirPenaltyDialog.CreateMemberObjects;
const OPNAME = 'TReservoirPenaltyDialog.CreateMemberObjects';
begin
  inherited CreateMemberObjects;
  try
    FNumberOfPenaltyStructures := 0;
    FNumberOfStorageZones      := 0;

    FOnTreeViewStartDrag := nil;
    FOnTreeViewDragDrop  := nil;
    FOnTreeViewDragOver  := nil;

    // ScrollBox
    FScrollBox.AutoScroll := True;

    // Panel Containers
    FPanRadioGroup            := TAbstractPanel.Create(ControlsOwner, FAppModules);
    FPanReservoir             := TAbstractPanel.Create(ControlsOwner, FAppModules);
    FPanPenalty               := TAbstractPanel.Create(ControlsOwner, FAppModules);
    FPanRadioGroup.Parent     := ControlsParent;
    FPanReservoir.Parent      := ControlsParent;
    FPanPenalty.Parent        := ControlsParent;

    // Group Panel
    FRadMode                   := TFieldRadioGroup.Create(ControlsOwner,FAppModules);
    FGbxRule                   := TGroupBox.Create(ControlsOwner);
    FGbxZoneButtons            := TGroupBox.Create(ControlsOwner);
    FGbxPenaltyButtons         := TGroupBox.Create(ControlsOwner);
    FEdtRuleCurve              := TFieldEdit.Create(ControlsOwner, FAppModules);
    FChkBalancingData          := TFieldChkBox.Create(ControlsOwner, FAppModules);
    FLblRuleCurve              := TLabel.Create(ControlsOwner);

    FBtnAddPenalty             := TFieldBitBtn.Create(ControlsOwner, FAppModules);
    FBtnDeletePenalty          := TFieldBitBtn.Create(ControlsOwner, FAppModules);
    FBtnAddZone                := TFieldBitBtn.Create(ControlsOwner, FAppModules);
    FBtnDeleteZone             := TFieldBitBtn.Create(ControlsOwner, FAppModules);

    FRadMode.Parent            := FPanRadioGroup;
    FGbxRule.Parent            := FPanRadioGroup;
    FGbxZoneButtons.Parent     := FPanRadioGroup;
    FGbxPenaltyButtons.Parent  := FPanRadioGroup;

    FBtnAddPenalty.Parent      := FGbxPenaltyButtons;
    FBtnDeletePenalty.Parent   := FGbxPenaltyButtons;
    FBtnAddZone.Parent         := FGbxZoneButtons;
    FBtnDeleteZone.Parent      := FGbxZoneButtons;

    FEdtRuleCurve.Parent       := FGbxRule;
    FChkBalancingData.Parent   := FGbxRule;
    FLblRuleCurve.Parent       := FGbxRule;

    // Reservoir Panel
    SetLength(FComboBoxArray, 0);
    FBtnAddReservior           := TFieldBitBtn.Create(ControlsOwner, FAppModules);
    FBtnDeleteReservior        := TFieldBitBtn.Create(ControlsOwner, FAppModules);
    FGrdReservoir              := TFieldStringGrid.Create(ControlsOwner, FAppModules);
    SetLength(FTvwReservoir, 0);

    FBtnAddReservior.Parent := FPanReservoir;
    FBtnDeleteReservior.Parent := FPanReservoir;
    FGrdReservoir.Parent := FPanReservoir;

    // Penalty Panel
    FGrdPenalty                := TColumnSelectGrid.Create(ControlsOwner, FAppModules);
    SetLength(FTvwPenalty,0);
    FGrdPenalty.Parent         := FPanPenalty;

    InitialiseMemberObjects;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirPenaltyDialog.InitialiseMemberObjects;
const OPNAME = 'TReservoirPenaltyDialog.InitialiseMemberObjects';
begin
  try
    FPanRadioGroup.Align := alTop;
    //FHintDisplay.Align := alBottom;

    FPanRadioGroup.Height := 4 * C_ControlBorder + C_EditBoxHeight;

    FRadMode.Columns := 2;
    FRadMode.Height := C_EditBoxHeight + 2 * C_ControlBorder;
    FRadMode.Top := C_ControlBorder;
    FRadMode.Left := C_ControlBorder;

    //FBtnAddPenalty.Height := C_EditBoxHeight + 2 * C_ControlBorder;
    //FBtnAddPenalty.Top := C_ControlBorder;
    //FBtnAddPenalty.Left := C_ControlBorder;

    //FBtnDeletePenalty.Height := C_EditBoxHeight + 2 * C_ControlBorder;
    //FBtnDeletePenalty.Top := C_ControlBorder;
    //FBtnDeletePenalty.Left := C_ControlBorder;

    FGbxRule.Top := C_ControlBorder;
    FGbxRule.Height := C_EditBoxHeight + 2 * C_ControlBorder;

    FEdtRuleCurve.Top := Trunc(1.5 * C_ControlBorder);
    FChkBalancingData.Top := Trunc(1.92 * C_ControlBorder);
    FLblRuleCurve.Top := (2 * C_ControlBorder);
    FLblRuleCurve.Layout := tlCenter;

    // Reservoir Mode
    FGrdReservoir.DefaultRowHeight := 18;
    FGrdReservoir.FixedCols := 0;
    FGrdReservoir.ColCount := 4;
    FGrdReservoir.RowCount := 2;
    FGrdReservoir.Left := C_ControlBorder;
    // FGrdReservoir.Align := alTop;

    FBtnAddReservior.Top := C_ControlBorder;
    FBtnDeleteReservior.Top := C_ControlBorder;

    FBtnAddReservior.Width := 75;
    FBtnDeleteReservior.Width := 88;

    FBtnAddReservior.Left := C_ControlBorder;
    FBtnDeleteReservior.Left := FBtnAddReservior.Left + FBtnAddReservior.Width + C_ControlBorder;
    FGrdReservoir.Top := FBtnAddReservior.Top + FBtnAddReservior.Height + C_ControlBorder;

    FBtnAddReservior.OnClick := AddButtonClicked;
    FBtnDeleteReservior.OnClick := RemoveButtonClicked;

    // Penalty Mode
    FGrdPenalty.DefaultRowHeight := 18;
    FGrdPenalty.FixedCols := 0;
    FGrdPenalty.ColCount := 4;
    FGrdPenalty.RowCount := 2;
    FGrdPenalty.Top := C_ControlBorder;
    FGrdPenalty.Left := C_ControlBorder;

    FChkBalancingData.Checked := FAppModules.ViewIni.ReadInteger(Self.ClassName, 'SBD', 0) = 1;

    FGrdPenalty.OnDrawCell := DrawRuleCurveDivision;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirPenaltyDialog.DestroyMemberObjects;
const OPNAME = 'TReservoirPenaltyDialog.DestroyMemberObjects';
var
  LIndex : integer;
begin
  try

    for LIndex := 0 to Length(FTvwPenalty) - 1 do
      if Assigned(FTvwPenalty[LIndex]) then
        FreeAndNil(FTvwPenalty[LIndex]);

    for LIndex := 0 to Length(FTvwReservoir) - 1 do
      if Assigned(FTvwReservoir[LIndex]) then
        FreeAndNil(FTvwReservoir[LIndex]);

    for LIndex := 0 to Length(FComboBoxArray) - 1 do
      if Assigned(FComboBoxArray[LIndex]) then
        FreeAndNil(FComboBoxArray[LIndex]);

    FTvwPenalty := nil;     // Finalise Array
    FTvwReservoir := nil;   // Finalise Array
    FComboBoxArray := nil;  // Finalise Array

    FAppModules.ViewIni.WriteInteger(Self.ClassName, 'SBD', Ord(FChkBalancingData.Checked));
    FAppModules.ViewIni.WriteInteger(Self.ClassName, 'RAD', FRadMode.ItemIndex);

  except on E: Exception do HandleError(E, OPNAME) end;
  inherited DestroyMemberObjects;
end;

function TReservoirPenaltyDialog.Initialise: boolean;
const OPNAME = 'TReservoirPenaltyDialog.Initialise';
begin
  Result := inherited Initialise;
  try
    FNumberOfPenaltyStructures := 0;
    FNumberOfStorageZones      := 0;

    FRadMode.Items.Clear;
    FRadMode.Items.Add('');
    FRadMode.Items.Add('');
    Result := True;

    FBtnAddZone.Glyph.LoadFromResourceName(HImagesInstance, 'TIMECOMPARITORADDSERIES');
    FBtnAddPenalty.Glyph.LoadFromResourceName(HImagesInstance, 'TIMECOMPARITORADDSERIES');
    FBtnAddReservior.Glyph.LoadFromResourceName(HImagesInstance, 'TIMECOMPARITORADDSERIES');
    FBtnDeleteZone.Glyph.LoadFromResourceName(HImagesInstance, 'TIMECOMPARITORREMOVESERIES');
    FBtnDeletePenalty.Glyph.LoadFromResourceName(HImagesInstance, 'TIMECOMPARITORREMOVESERIES');
    FBtnDeleteReservior.Glyph.LoadFromResourceName(HImagesInstance, 'TIMECOMPARITORREMOVESERIES');
    FBtnAddZone.IsEnabled := False;
    FBtnAddPenalty.IsEnabled := False;
    FBtnAddReservior.IsEnabled := False;
    FBtnDeleteZone.IsEnabled := False;
    FBtnDeletePenalty.IsEnabled := False;
    FBtnDeleteReservior.IsEnabled := False;

    // Event handlers
    FRadMode.OnClick := RadModeClickHandler;
    FChkBalancingData.OnClick := ChkBalancingDataClickHandler;
    FGrdReservoir.DblClickColAutoSize := False;
    FGrdPenalty.DblClickColAutoSize := False;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirPenaltyDialog.Resize;
const OPNAME = 'TReservoirPenaltyDialog.Resize';
begin
  // Call the ancestor.

  inherited Resize;

  try
    PerformResizeControls;
  // Handle exceptions.

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirPenaltyDialog.LanguageHasChanged: boolean;
const OPNAME = 'TReservoirPenaltyDialog.LanguageHasChanged';
var
  LIndex : integer;
begin
  Result := inherited LanguageHasChanged;
  try

    FLblRuleCurve.Caption := FAppModules.Language.GetString('LabelText.RuleCurveLevel');
    FChkBalancingData.Caption := FAppModules.Language.GetString('CheckBoxText.BalancingData');

    FGrdPenalty.Cells[0, 0] := FAppModules.Language.GetString('GridHeading.ZoneName');
    for LIndex := FNumberOfPenaltyStructures downto 1 do
      FGrdPenalty.Cells[FGrdPenalty.ColCount - LIndex, 0] :=
         FAppModules.Language.GetString('GridHeading.PenaltyStructure') + Format(' %d',[FNumberOfPenaltyStructures - LIndex + 1]);

    FGrdPenalty.Cells[1, 0] := FAppModules.Language.GetString('GridHeading.BalancingDataColumn.S');
    FGrdPenalty.Cells[2, 0] := FAppModules.Language.GetString('GridHeading.BalancingDataColumn.V');
    FGrdPenalty.Cells[3, 0] := FAppModules.Language.GetString('GridHeading.BalancingDataColumn.R');

    FGrdReservoir.Cells[0, 0] := FAppModules.Language.GetString('GridHeading.ZoneName');
    FGrdReservoir.Cells[1, 0] := FAppModules.Language.GetString('GridHeading.BalancingDataColumn.S');
    FGrdReservoir.Cells[2, 0] := FAppModules.Language.GetString('GridHeading.BalancingDataColumn.V');
    FGrdReservoir.Cells[3, 0] := FAppModules.Language.GetString('GridHeading.BalancingDataColumn.R');

    FBtnAddReservior.Caption    := FAppModules.language.GetString('ButtonCaption.Column');
    FBtnDeleteReservior.Caption := FAppModules.language.GetString('ButtonCaption.Column');

    FRadMode.Caption          := FAppModules.language.GetString('RandMode.View');
    FRadMode.Items.Strings[0] := FAppModules.Language.GetString('TField.PenaltyMode');
    FRadMode.Items.Strings[1] := FAppModules.Language.GetString('TField.ReservoirMode');

    FRadMode.Hints.Clear;
    FRadMode.Hints.Add(FAppModules.language.GetString('TReservoirPenaltyDialog.PenaltyMode'));
    FRadMode.Hints.Add(FAppModules.language.GetString('TReservoirPenaltyDialog.ReservoirMode'));
    FChkBalancingData.Hint    := FAppModules.language.GetString('TReservoirPenaltyDialog.Balancing');

    {FBtnAddZone.Caption    := 'Zone';
    FBtnDeleteZone.Caption := 'Zone';
    FBtnAddPenalty.Caption    := 'Penalty';
    FBtnDeletePenalty.Caption := 'Penalty';
    }
    FGbxPenaltyButtons.Caption :=  FAppModules.language.GetString('GrBox.Penalty');
    FGbxZoneButtons.Caption    :=  FAppModules.language.GetString('GrBox.Zone');

    if FRadMode.ItemIndex = -1 then
      FRadMode.ItemIndex := 0;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirPenaltyDialog.SaveState: boolean;
const OPNAME = 'TReservoirPenaltyDialog.SaveState';
begin
  Result := inherited SaveState;
  try
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirPenaltyDialog.SetNumberOfStorageZones(const Value: integer);
const OPNAME = 'TReservoirPenaltyDialog.SetNumberOfStorageZones';
var
  LIndex: integer;
begin
  try
    FNumberOfStorageZones      := Value;
    if Value > 0 then
    begin
      FGrdPenalty.RowCount := Value + 1;
      FGrdReservoir.RowCount := Value + 1;
      if (FAppModules.User.UserRights in CUR_EditData) and
         (FAppModules.StudyArea <> nil) and
         (not (FAppModules.StudyArea.ScenarioLocked)) then
      begin
        FGrdPenalty.Options := FGrdReservoir.Options + [goEditing];
        FGrdReservoir.Options := FGrdReservoir.Options + [goEditing];
      end
      else
      begin
        FGrdPenalty.Options := FGrdReservoir.Options - [goEditing];
        FGrdReservoir.Options := FGrdReservoir.Options - [goEditing];
      end;
    end
    else
    begin
      FGrdPenalty.RowCount   := 2;
      FGrdReservoir.RowCount := 2;
      for LIndex := 0 to FGrdReservoir.ColCount -1 do
      begin
        FGrdPenalty.Cells[LIndex,1] := '';
        FGrdReservoir.Cells[LIndex,1] := '';
      end;
      FGrdPenalty.Options   := FGrdPenalty.Options - [goEditing];
      FGrdReservoir.Options := FGrdReservoir.Options - [goEditing];
    end;
    PerformResizeControls;
    LanguageHasChanged;
    ResetButtonState;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirPenaltyDialog.SetNumberOfPenaltyStructures(const Value: integer);
const OPNAME = 'TReservoirPenaltyDialog.SetNumberOfPenaltyStructures';
var
  LIndex : integer;
begin
  try
    FNumberOfPenaltyStructures := Value;
    if Value > 0 then
    begin
      if Value > (Length(FTvwPenalty) - 1) then
      begin
        for LIndex := Length(FTvwPenalty) + 1 to Value + 1 do
        begin
          SetLength(FTvwPenalty, Length(FTvwPenalty) + 1);
          FTvwPenalty[Length(FTvwPenalty) - 1] := TReservoirPenaltyTreeView.Create(FPanReservoir, FAppModules);
          FTvwPenalty[Length(FTvwPenalty) - 1].Parent := FPanPenalty;
          FTvwPenalty[Length(FTvwPenalty) - 1].PennaltyStructureNumber := LIndex;
          if FAppModules.FieldProperties.ModeIsEditable('ReservoirPenalty') then
          begin
            FTvwPenalty[Length(FTvwPenalty) - 1].OnStartDrag := FOnTreeViewStartDrag;
            FTvwPenalty[Length(FTvwPenalty) - 1].OnDragOver  := FOnTreeViewDragOver;
            FTvwPenalty[Length(FTvwPenalty) - 1].OnDragDrop  := FOnTreeViewDragDrop;
            FTvwPenalty[Length(FTvwPenalty) - 1].DragMode    := dmAutomatic;
          end;
          FTvwPenalty[Length(FTvwPenalty) - 1].SortType    := stText;
          FTvwPenalty[Length(FTvwPenalty) - 1].HelpContext  := HC_ReservoirPenaltyStructures;
       end
      end
      else
      begin
        if Value < (Length(FTvwPenalty) - 1) then
        begin
          for LIndex := Value to Length(FTvwPenalty) - 1 do
            FreeAndNil(FTvwPenalty[LIndex]);
          SetLength(FTvwPenalty, Value);
        end;
      end;
    end;
    FGrdPenalty.ColCount := 1 + 3 + FNumberOfPenaltyStructures;
    PerformResizeControls;
    LanguageHasChanged;
    ResetButtonState;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirPenaltyDialog.PerformResizeControls;
const OPNAME = 'TReservoirPenaltyDialog.PerformResizeControls';
begin
  try
    ResizeGroupBoxControls;
    ResizeReservoirModeControls;
    ResizePenaltyModeControls;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirPenaltyDialog.ResetButtonState;
const OPNAME = 'TReservoirPenaltyDialog.ResetButtonState';
var
  LIndex: integer;
  lFieldProperty : TAbstractFieldProperty;

begin
  try
    if (FViewMode = vmSelect) then
    //or (FViewMode = vmEditableSelect))  then
    begin
      FBtnAddZone.IsEnabled         := False;
      FBtnDeleteZone.IsEnabled      := False;
      FBtnAddPenalty.IsEnabled      := False;
      FBtnDeletePenalty.IsEnabled   := False;
      FBtnAddReservior.IsEnabled    := False;
      FBtnDeleteReservior.IsEnabled := False;
      FRadMode.IsEnabled            := False;
      FEdtRuleCurve.IsEnabled       := False;
      FLblRuleCurve.Enabled         := False;
      FChkBalancingData.IsEnabled   := False;
      FGrdPenalty.Color             := clSilver;
      for LIndex := Low(FTvwReservoir) to High(FTvwReservoir) do
         FTvwReservoir[LIndex].Enabled := False;
      for LIndex := Low(FTvwPenalty) to High(FTvwPenalty) do
         FTvwPenalty[LIndex].Enabled := False;
    end
    else
    begin

      lFieldProperty := FAppModules.FieldProperties.FieldProperty('StorageZoneCount');
      FBtnAddZone.IsEnabled         := (FRadMode.ItemIndex = 0) and
                                       (FNumberOfStorageZones <  StrToInt(lFieldProperty.FieldMaximumValue)) and
                                       ((FNumberOfStorageZones = 0) or ((GrdPenalty.Row > 1) and
                                       (GrdPenalty.Row < GrdPenalty.RowCount-1))) and
                                       (FAppModules.User.UserRights in CUR_EditData);
      FBtnDeleteZone.IsEnabled      := (FRadMode.ItemIndex = 0) and (GrdPenalty.Row > 1) and
                                       (GrdPenalty.Row <= GrdPenalty.RowCount-2) and
                                       (GrdPenalty.RowCount > 4) and
                                       (FAppModules.User.UserRights in CUR_EditData);
      lFieldProperty := FAppModules.FieldProperties.FieldProperty('PenaltyStruct');
      FBtnAddPenalty.IsEnabled      := (FRadMode.ItemIndex = 0) and (FNumberOfStorageZones > 0) and
                                       (FNumberOfPenaltyStructures < StrToInt(lFieldProperty.FieldMaximumValue)) and
                                       (FAppModules.User.UserRights in CUR_EditData);
      FBtnDeletePenalty.IsEnabled   := (FRadMode.ItemIndex = 0) and
                                       (GrdPenalty.Col > 3) and
                                       (FAppModules.User.UserRights in CUR_EditData);
      FBtnAddReservior.IsEnabled    := (FRadMode.ItemIndex = 1) and
                                       (FNumberOfPenaltyStructures > 0) and
                                       (FAppModules.User.UserRights in CUR_EditData);
      FBtnDeleteReservior.IsEnabled := (FRadMode.ItemIndex = 1) and
                                       (FNumberOfPenaltyStructures > 0) and
                                       (FGrdReservoir.Col > 3) and
                                       (FAppModules.User.UserRights in CUR_EditData);
      if (FAppModules.StudyArea <> nil) then
      begin
        FBtnAddZone.IsEnabled := FBtnAddZone.IsEnabled and (not FAppModules.StudyArea.ScenarioLocked);
        FBtnDeleteZone.IsEnabled := FBtnDeleteZone.IsEnabled and (not FAppModules.StudyArea.ScenarioLocked);
        FBtnAddPenalty.IsEnabled := FBtnAddPenalty.IsEnabled and (not FAppModules.StudyArea.ScenarioLocked);
        FBtnDeletePenalty.IsEnabled := FBtnDeletePenalty.IsEnabled and (not FAppModules.StudyArea.ScenarioLocked);
        FBtnAddReservior.IsEnabled := FBtnAddReservior.IsEnabled and (not FAppModules.StudyArea.ScenarioLocked);
        FBtnDeleteReservior.IsEnabled := FBtnDeleteReservior.IsEnabled and (not FAppModules.StudyArea.ScenarioLocked);
      end;
      FGrdPenalty.Color := clSilver;
      for LIndex := Low(FTvwReservoir) to High(FTvwReservoir) do
         FTvwReservoir[LIndex].Enabled := True;
      for LIndex := Low(FTvwPenalty) to High(FTvwPenalty) do
         FTvwPenalty[LIndex].Enabled := True;
    end;
    if (FViewMode = vmEditable) then
    begin
      FGrdPenalty.Color := clWindow;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirPenaltyDialog.GetPenaltyTreeViewByNumber(AIndex : integer): TReservoirPenaltyTreeView;
const OPNAME = 'TReservoirPenaltyDialog.GetPenaltyTreeViewByNumber';
begin
  Result := nil;
  try
    if (AIndex < Length(FTvwPenalty)) and (AIndex >= 0) then
      Result := FTvwPenalty[AIndex]
    else
      Exception.Create('Attempting to access invalid treeview in ' + OPNAME);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirPenaltyDialog.RadModeClickHandler(ASender: TObject);
const OPNAME = 'TReservoirPenaltyDialog.RadModeClickHandler';
begin
  try
    FPanReservoir.Visible := FRadMode.ItemIndex = 1;
    FPanPenalty.Visible :=  not FPanReservoir.Visible;


    PerformResizeControls;
    ResetButtonState;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirPenaltyDialog.ChkBalancingDataClickHandler(
  ASender: TObject);
const OPNAME = 'TReservoirPenaltyDialog.ChkBalancingDataClickHandler';
begin
  try

    PerformResizeControls;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirPenaltyDialog.SetRuleCurveLevel;
const OPNAME = 'TReservoirPenaltyDialog.SetRuleCurveLevel';
begin
  try

    FRuleCurveLevel := Value;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirPenaltyDialog.DrawRuleCurveDivision;
const OPNAME = 'TReservoirPenaltyDialog.DrawRuleCurveDivision';
begin
  try

    if (ARow > 0) and (ARow = FRuleCurveLevel) then
    begin
      FGrdPenalty.Canvas.Pen.Color := clGray;
      FGrdPenalty.Canvas.Pen.Style := psSolid;
      FGrdPenalty.Canvas.Brush.Color := clBlue;
      FGrdPenalty.Canvas.Brush.Style := bsSolid;
      FGrdPenalty.Canvas.Rectangle(Rect(ARect.Left, ARect.Bottom - 4, ARect.Right, ARect.Bottom));
    end;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirPenaltyDialog.SetOnAddColumn(
  const Value: TNotifyEvent);
const OPNAME = 'TReservoirPenaltyDialog.SetOnAddColumn';
begin
  try
    FOnAddColumn := Value;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirPenaltyDialog.SetOnColumnTextChange(
  const Value: TNotifyEvent);
const OPNAME = 'TReservoirPenaltyDialog.SetOnColumnTextChange';
begin
  try
    FOnColumnTextChange := Value;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirPenaltyDialog.SetOnDeleteColumn(
  const Value: TNotifyEvent);
const OPNAME = 'TReservoirPenaltyDialog.SetOnDeleteColumn';
begin
  try
    FOnDeleteColumn := Value;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirPenaltyDialog.GetComboBoxByIndex(
  AIndex: integer): TFieldComboBox;
const OPNAME = 'TReservoirPenaltyDialog.GetComboBoxByIndex';
begin
  Result := nil;
  try
    if AIndex < Length(FComboBoxArray) then
      Result := FComboBoxArray[AIndex];
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirPenaltyDialog.SetNumberOfComboBoxes(
  const Value: integer);
const OPNAME = 'TReservoirPenaltyDialog.SetNumberOfComboBoxes';
var
  LIndex : integer;
  LRowIndex: integer;
begin
  try
    if Value >= 0 then
    begin

      // Add new components
      for LIndex := Length(FComboBoxArray) + 1 to Value do
      begin
        SetLength(FComboBoxArray, Length(FComboBoxArray) + 1);
        FComboBoxArray[Length(FComboBoxArray) - 1] := TFieldComboBox.Create(Owner, FAppModules);
        FComboBoxArray[Length(FComboBoxArray) - 1].Style := csDropDownList;
        FComboBoxArray[Length(FComboBoxArray) - 1].OnChange := ComboBoxChange;
        FComboBoxArray[Length(FComboBoxArray) - 1].Parent := FPanReservoir;
      end;

      for LIndex := Length(FTvwReservoir) + 1 to Value do
      begin
        SetLength(FTvwReservoir, Length(FTvwReservoir) + 1);
        FTvwReservoir[Length(FTvwReservoir) - 1] := TReservoirPenaltyTreeView.Create(FPanReservoir, FAppModules);
        FTvwReservoir[Length(FTvwReservoir) - 1].Parent := FPanReservoir;
        FTvwReservoir[Length(FTvwReservoir) - 1].PennaltyStructureNumber := LIndex + 1;
        FTvwReservoir[Length(FTvwReservoir) - 1].SortType  := stText;
        SetControlHelpContext(FTvwReservoir[Length(FTvwReservoir) - 1],   HC_ReservoirPenaltyStructures);
      end;

      // Delete unused components
      for LIndex := Value to (Length(FComboBoxArray) - 1) do
        FreeAndNil(FComboBoxArray[LIndex]);

      for LIndex := Value to (Length(FTvwReservoir) - 1) do
        FreeAndNil(FTvwReservoir[LIndex]);

      // Finalise the counts
      SetLength(FComboBoxArray, Value);
      SetLength(FTvwReservoir, Value);

    end;

    for LIndex := FGrdReservoir.ColCount to 4 + Length(FComboBoxArray) do
      for LRowIndex := 0 to FGrdReservoir.RowCount - 1 do
        FGrdReservoir.Cells[LIndex, LRowIndex] := '' ;

    FGrdReservoir.ColCount := 4 + Length(FComboBoxArray);

    PerformResizeControls;
    LanguageHasChanged;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirPenaltyDialog.GetReservoirTreeViewByNumber(AIndex: integer): TReservoirPenaltyTreeView;
const OPNAME = 'TReservoirPenaltyDialog.GetReservoirTreeViewByNumber';
begin
  Result := nil;
  try
    if (AIndex >= 0) and (AIndex < Length(FTvwReservoir)) then
      Result := FTvwReservoir[AIndex];

  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirPenaltyDialog.ResizeGroupBoxControls;
const OPNAME = 'TReservoirPenaltyDialog.ResizeGroupBoxControls';
var
  //LThird,
  LWidth : integer;
  LHeight: integer;
  LTop: integer;
begin
  try
    //LWidth := FPanRadioGroup.ClientWidth - (4 * C_GroupBoxOffset);

    FGbxZoneButtons.Top    := 2;
    FGbxPenaltyButtons.Top := 2;
    FRadMode.Top           := 2;
    FGbxRule.Top           := 2;

    FGbxZoneButtons.Height   := FPanRadioGroup.ClientHeight -4;
    FGbxPenaltyButtons.Height:= FPanRadioGroup.ClientHeight -4;
    FRadMode.Height          := FPanRadioGroup.ClientHeight -4;
    FGbxRule.Height          := FPanRadioGroup.ClientHeight -4;

    //FRadMode.Width           := LWidth div 3;
    //FGbxRule.Width           := LWidth div 3;
    //FGbxZoneButtons.Width    := LWidth div 3;
    //FGbxZoneButtons.Width    := FGbxZoneButtons.Width div 2;
    //FGbxPenaltyButtons.Width := FGbxZoneButtons.Width;

    FRadMode.Width           := 235;
    FGbxRule.Width           := 235;
    FGbxZoneButtons.Width    := 117;
    FGbxPenaltyButtons.Width := 117;

    //LThird                 := FRadMode.Width div 4;
    //FGbxZoneButtons.Width  := FGbxZoneButtons.Width + LThird;
    //FRadMode.Width         :=  FRadMode.Width - LThird;


    FGbxZoneButtons.Left    := C_GroupBoxOffset;
    FGbxPenaltyButtons.Left := FGbxZoneButtons.Left + FGbxZoneButtons.Width;
    FRadMode.Left           := FGbxPenaltyButtons.Left + FGbxPenaltyButtons.Width + C_GroupBoxOffset;;
    FGbxRule.Left           := FRadMode.Left + FRadMode.Width + C_GroupBoxOffset;


    FBtnAddPenalty.Top    := FGbxZoneButtons.Top + (2 * C_GroupBoxOffset) + 2;
    FBtnDeletePenalty.Top := FBtnAddPenalty.Top;
    FBtnAddZone.Top       := FBtnAddPenalty.Top;
    FBtnDeleteZone.Top    := FBtnAddPenalty.Top;
    LTop                  := FBtnAddPenalty.Top;

    FBtnAddPenalty.Height    := FGbxZoneButtons.Height - (3 *C_GroupBoxOffset)-2;
    FBtnDeletePenalty.Height := FBtnAddPenalty.Height;
    FBtnAddZone.Height       := FBtnAddPenalty.Height;
    FBtnDeleteZone.Height    := FBtnAddPenalty.Height;
    LHeight                  := FBtnAddPenalty.Height;

    FBtnAddPenalty.Width     := (FGbxZoneButtons.Width div 2) -  C_GroupBoxOffset - 1;
    //(2 * C_GroupBoxOffset)-2;
    FBtnDeletePenalty.Width  := FBtnAddPenalty.Width;
    FBtnAddZone.Width       := FBtnAddPenalty.Width;
    FBtnDeleteZone.Width    := FBtnAddPenalty.Width;

    FBtnAddZone.Left     := C_GroupBoxOffset;
    FBtnDeleteZone.Left  := FBtnAddZone.Left +  FBtnAddZone.Width + C_GroupBoxOffset;
    FBtnAddPenalty.Left  := C_GroupBoxOffset;
    FBtnDeletePenalty.Left  := FBtnAddPenalty.Left +  FBtnAddPenalty.Width + C_GroupBoxOffset;

    FLblRuleCurve.Top      := LTop;
    FEdtRuleCurve.Top      := LTop;
    FChkBalancingData.Top  := LTop;

    FLblRuleCurve.Height      := LHeight;
    //FEdtRuleCurve.Height      := LHeight;
    FChkBalancingData.Height  := LHeight;

    LWidth := FGbxRule.Width - (2 * C_GroupBoxOffset);

    FLblRuleCurve.Width      := LWidth div 3;
    LWidth := LWidth - (LWidth div 3);
    FEdtRuleCurve.Width      := LWidth div 5;
    LWidth := LWidth - (LWidth div 5);
    FChkBalancingData.Width  := LWidth-2;

    FLblRuleCurve.Left      := C_GroupBoxOffset;
    FEdtRuleCurve.Left      := FLblRuleCurve.Left + FLblRuleCurve.Width;
    FChkBalancingData.Left  := FEdtRuleCurve.Left + FEdtRuleCurve.Width+ C_GroupBoxOffset;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirPenaltyDialog.ResizePenaltyModeControls;
const OPNAME = 'TReservoirPenaltyDialog.ResizePenaltyModeControls';
var
  LIndex : integer;
  LClientWidth : integer;
  LTotaWidth : integer;
  LColWidth  : integer;
  LFourColWidth: integer;
begin
  try
    LClientWidth := ControlsParent.ClientWidth;

    FPanPenalty.Top := FPanRadioGroup.Top + FPanRadioGroup.Height + 1;
    FPanPenalty.Height := ControlsParent.ClientHeight - FPanRadioGroup.Height - FPanRadioGroup.Top - 1;

    if NumberOfStorageZones <= 10 then
      FGrdPenalty.Height := FGrdPenalty.DefaultRowHeight * FGrdPenalty.RowCount + 2 * FGrdPenalty.RowCount
    else
      FGrdPenalty.Height := 180;

    FGrdPenalty.Width := LClientWidth - C_ControlBorder;

     FGrdPenalty.ColWidths[1] := 15 * Ord(FChkBalancingData.Checked);
    FGrdPenalty.ColWidths[2] := 15 * Ord(FChkBalancingData.Checked);
    FGrdPenalty.ColWidths[3] := 15 * Ord(FChkBalancingData.Checked);

    FGrdPenalty.ColWidths[0] := (FGrdPenalty.ClientWidth div (FGrdPenalty.ColCount - 3)) -
      FGrdPenalty.ColWidths[1] - FGrdPenalty.ColWidths[2] - FGrdPenalty.ColWidths[3] - (C_ControlBorder-2);

    LFourColWidth := 0;
    for LIndex := 0 to 3 do
      LFourColWidth := LFourColWidth + FGrdPenalty.ColWidths[LIndex];


    if(FNumberOfPenaltyStructures > 0) and (FNumberOfPenaltyStructures <= 5) then
      LColWidth := (LClientWidth - LFourColWidth-(2 * C_ControlBorder)) div FNumberOfPenaltyStructures
    else
      LColWidth := C_ReservoirPenaltySize;

    for LIndex := 4 to FGrdPenalty.ColCount - 1 do
      FGrdPenalty.ColWidths[LIndex] := LColWidth;
    for LIndex := 0 to Length(FTvwPenalty) - 1 do
    begin
      if Assigned(FTvwPenalty[LIndex]) then
      begin
        FTvwPenalty[LIndex].Top    := FGrdPenalty.Top + FGrdPenalty.Height + C_ControlBorder;
        FTvwPenalty[LIndex].Height := FPanPenalty.Height - FGrdPenalty.Top - FGrdPenalty.Height - 2 * C_ControlBorder;
        if(LIndex = 0) then
        begin
          FTvwPenalty[LIndex].Left   := C_ControlBorder;
          FTvwPenalty[LIndex].Width  := LFourColWidth;
        end
        else
        begin
          FTvwPenalty[LIndex].Width  := LColWidth;
          FTvwPenalty[LIndex].Left   := FTvwPenalty[LIndex-1].Left + FTvwPenalty[LIndex-1].Width + 2;
        end;
      end;
    end;

    LTotaWidth := 0;
    for LIndex := 0 to FGrdPenalty.ColCount - 1 do
      LTotaWidth := LTotaWidth + FGrdPenalty.ColWidths[LIndex];

    if (FNumberOfPenaltyStructures = 0) then
      FGrdPenalty.ClientWidth := FPanReservoir.ClientWidth
    else
      FGrdPenalty.ClientWidth := LTotaWidth + 13;

    if (FNumberOfPenaltyStructures > 5) then
      FPanPenalty.ClientWidth := FGrdPenalty.ClientWidth + 40
    else
      FPanPenalty.ClientWidth := FPanRadioGroup.ClientWidth;


    FGrdPenalty.Width       := FGrdPenalty.ClientWidth;

    FBtnDeleteReservior.Enabled       := FGrdReservoir.ColCount > 4;
    FScrollBox.AutoScroll             := FGrdPenalty.ClientWidth > FScrollBox.Width;
    FScrollBox.VertScrollBar.Visible  :=  FScrollBox.AutoScroll

  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirPenaltyDialog.ResizeReservoirModeControls;
const OPNAME = 'TReservoirPenaltyDialog.ResizeReservoirModeControls';
var
  LClientWidth : integer;
  LClientHeight : integer;
  LIndex : integer;
begin
  try
    // FScrollBox.AutoScroll := False;

    LClientWidth := ControlsParent.ClientWidth;
    LClientHeight := ControlsParent.ClientHeight;

    FPanReservoir.Top := FPanRadioGroup.Top + FPanRadioGroup.Height + 1;
    FPanReservoir.ClientWidth := LClientWidth - FPanReservoir.Left;
    FPanReservoir.Height := LClientHeight - FPanRadioGroup.Height - FPanRadioGroup.Top - 1;

    FGrdReservoir.Height := FGrdReservoir.DefaultRowHeight * FGrdReservoir.RowCount + 2 * FGrdReservoir.RowCount;

    FGrdReservoir.Width := C_ReservoirPenaltySize * (FGrdReservoir.ColCount - 3) + 2 * C_ControlBorder;

    FGrdReservoir.ColWidths[1] := 15 * Ord(FChkBalancingData.Checked);
    FGrdReservoir.ColWidths[2] := 15 * Ord(FChkBalancingData.Checked);
    FGrdReservoir.ColWidths[3] := 15 * Ord(FChkBalancingData.Checked);

    FGrdReservoir.ColWidths[0] := C_ReservoirPenaltySize - 2 -
      FGrdReservoir.ColWidths[1] - FGrdReservoir.ColWidths[2] - FGrdReservoir.ColWidths[3];

     for LIndex := 4 to FGrdReservoir.ColCount - 1 do
     FGrdReservoir.ColWidths[LIndex] := C_ReservoirPenaltySize;

    for LIndex := 0 to Length(FTvwReservoir) - 1 do
    begin
      if Assigned(FTvwReservoir[LIndex]) then
        begin
          FTvwReservoir[LIndex].Top := FGrdReservoir.Top + FGrdReservoir.Height + C_ControlBorder;
          FTvwReservoir[LIndex].Width := C_ReservoirPenaltySize;
          FTvwReservoir[LIndex].Left := Trunc(1.90 * C_ControlBorder) + (LIndex + 1) * C_ReservoirPenaltySize;
          FTvwReservoir[LIndex].Height := FPanReservoir.Height - FGrdReservoir.Top - FGrdReservoir.Height - 2 * C_ControlBorder;
        end;
    end;

    for LIndex := 0 to Length(FComboBoxArray) - 1 do
    begin
      if Assigned(FComboBoxArray[LIndex]) then
        begin
          FComboBoxArray[LIndex].Top := C_ControlBorder;
          FComboBoxArray[LIndex].Width := C_ReservoirPenaltySize;
          FComboBoxArray[LIndex].Left := Trunc(1.5 * C_ControlBorder) + (LIndex + 1) * C_ReservoirPenaltySize;
        end;
    end;

    FPanReservoir.Width := Max(C_ReservoirPenaltySize * (FGrdReservoir.ColCount - 3), LClientWidth);
    FScrollBox.AutoScroll := FGrdReservoir.Width > FScrollBox.Width;
    //FScrollBox.AutoScroll := LClientWidth < (FGrdReservoir.ColCount - 3) * C_ReservoirPenaltySize;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirPenaltyDialog.AddButtonClicked(
  ASender: TObject);
const OPNAME = 'TReservoirPenaltyDialog.AddButtonClicked';
begin
  try
    if Assigned(FOnAddColumn) then
      FOnAddColumn(ASender);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirPenaltyDialog.RemoveButtonClicked(
  ASender: TObject);
const OPNAME = 'TReservoirPenaltyDialog.RemoveButtonClicked';
begin
  try
    if Assigned(FOnDeleteColumn) then
      FOnDeleteColumn(ASender);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirPenaltyDialog.GetNumberOfComboBoxes : integer;
const OPNAME = 'TReservoirPenaltyDialog.GetNumberOfComboBoxes';
begin
  Result := 0;
  try
    if Assigned(FGrdReservoir) then
      Result := FGrdReservoir.ColCount - 4;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirPenaltyDialog.ComboBoxChange(ASender: TObject);
const OPNAME = 'TReservoirPenaltyDialog.ComboBoxChange';
begin
  try
    if Assigned(FOnColumnTextChange) then
      FOnColumnTextChange(ASender);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirPenaltyDialog.SetViewMode(AViewMode: TViewMode);
const OPNAME = 'TReservoirPenaltyDialog.SetViewMode';
begin
  inherited;
  try
    FGrdPenalty.ViewMode := AViewMode;
    case AViewMode of
    vmNone:
      begin
      end;
    vmEditable :
      begin
        if (FAppModules.User.UserRights in CUR_EditData) and
           (FAppModules.StudyArea <> nil) and
           (not (FAppModules.StudyArea.ScenarioLocked)) then
          FGrdPenalty.Options := FGrdPenalty.Options + [goEditing];
          if (NumberOfStorageZones = 0) then
            GrdPenalty.Options := FGrdPenalty.Options - [goEditing];

      end;
    vmEditableSelect:
      begin
        if (FAppModules.User.UserRights in CUR_EditData) and
           (FAppModules.StudyArea <> nil) and
           (not (FAppModules.StudyArea.ScenarioLocked)) then
          FGrdPenalty.Options := FGrdPenalty.Options + [goEditing];
          if (NumberOfStorageZones = 0) then
            GrdPenalty.Options := FGrdPenalty.Options - [goEditing];

      end;
    vmSelect:
      begin
        if(FRadMode.ItemIndex <> 0) then
          FRadMode.ItemIndex := 0;
        if not FChkBalancingData.Checked then
           FChkBalancingData.Checked := True;
        if(FViewMode = vmSelect) then
          FGrdPenalty.Options := FGrdPenalty.Options - [goEditing]
        else
        begin
          if (FAppModules.User.UserRights in CUR_EditData) and
             (FAppModules.StudyArea <> nil) and
             (not (FAppModules.StudyArea.ScenarioLocked)) then
            FGrdPenalty.Options := FGrdPenalty.Options + [goEditing]
          else
            FGrdPenalty.Options := FGrdPenalty.Options - [goEditing];
        end;
        FGrdPenalty.FixedCols := Min(4,FGrdPenalty.ColCount -1);
      end;
    end;
    ResetButtonState;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirPenaltyDialog.RestoreColourState;
const OPNAME = 'TReservoirPenaltyDialog.RestoreColourState';
var
  LIndex : integer;
begin
  inherited RestoreColourState;
  try
    for LIndex := 0 to ControlsOwner.ComponentCount - 1 do
      if ControlsOwner.Components[LIndex].ClassName = TFieldEdit.ClassName then
        if TFieldEdit(ControlsOwner.Components[LIndex]).Color = clRed then
          TFieldEdit(ControlsOwner.Components[LIndex]).Color := clWindow;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirPenaltyDialog.ShowSelectPenaltyStructure: boolean;
const OPNAME = 'TReservoirPenaltyDialog.ShowSelectPenaltyStructure';
{var
  LRowIndex,
  LColIndex : integer;
  LCellRect: TRect;
}
begin
  Result := False;
  try
    Result := FGrdPenalty.ShowSelectColumn;
    {if(ViewMode <> vmSelect) then
    begin
      Result := True;
      Exit;
    end;}
    {for LColIndex := 4 to FGrdPenalty.ColCount -1  do
    begin
      if(FGrdPenalty.ColWidths[LColIndex] <= 0) then Continue;
      for LRowIndex := 1 to FGrdPenalty.RowCount -1 do
      begin
        LCellRect := FGrdPenalty.CellRect(LColIndex, LRowIndex);
        if(LColIndex = FSelectedPenaltyStructureCol) then
        begin
          FGrdPenalty.Canvas.Brush.Style := bsClear;
          FGrdPenalty.Canvas.Brush.Color := clBlue;
        end
        else
        begin
          FGrdPenalty.Canvas.Brush.Style := bsClear;
          FGrdPenalty.Canvas.Brush.Color := clWindow;
        end;
        FGrdPenalty.Canvas.FrameRect(LCellRect);
      end;
    end;
    Result := True;
    }
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirPenaltyDialog.SetSelectedPenaltyStructureCol(AValue: integer);
const OPNAME = 'TReservoirPenaltyDialog.SetSelectedPenaltyStructureCol';
begin
  try
    FSelectedPenaltyStructureCol := AValue;
    FGrdPenalty.SelectedColumn := AValue;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirPenaltyDialog.AssignHelpContext;
const OPNAME = 'TReservoirPenaltyDialog.AssignHelpContext';
begin
  try
    SetControlHelpContext(Self,                HC_ReservoirPenaltyStructures);

    SetControlHelpContext(FRadMode,            HC_ReservoirPenaltyStructures);
    SetControlHelpContext(FGbxRule,            HC_ReservoirRuleCurve);
    SetControlHelpContext(FEdtRuleCurve,       HC_ReservoirRuleCurve);
    SetControlHelpContext(FChkBalancingData,   HC_ReservoirPenaltyStructures);

    SetControlHelpContext(FBtnAddReservior,    HC_ReservoirPenaltyStructures);
    SetControlHelpContext(FBtnDeleteReservior, HC_ReservoirPenaltyStructures);

    SetControlHelpContext(FBtnAddReservior,    HC_ReservoirPenaltyStructures);
    SetControlHelpContext(FBtnDeleteReservior, HC_ReservoirPenaltyStructures);

    SetControlHelpContext(FGrdPenalty,         HC_ReservoirPenaltyStructures);
    SetControlHelpContext(FGrdReservoir,       HC_ReservoirPenaltyStructures);

  except on E: Exception do HandleError(E, OPNAME) end;
end;

{ TColumnSelectGrid }

procedure TColumnSelectGrid.Paint;
const OPNAME = 'TColumnSelectGrid.Paint';
begin
  inherited;
  try
    ShowSelectColumn;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TColumnSelectGrid.ShowSelectColumn: boolean;
const OPNAME = 'TColumnSelectGrid.ShowSelectColumn';
var
  LRowIndex,
  LColIndex : integer;
  LCellRect: TRect;
begin
  Result := False;
  try
    {if(ViewMode <> vmSelect) then
    begin
      Result := True;
      Exit;
    end;}
    for LColIndex := 4 to Self.ColCount -1  do
    begin
      if(Self.ColWidths[LColIndex] <= 0) then Continue;
      for LRowIndex := 1 to Self.RowCount -1 do
      begin
        LCellRect := Self.CellRect(LColIndex, LRowIndex);
        if(LColIndex = FSelectedColumn) then
        begin
          Self.Canvas.Brush.Style := bsClear;
          Self.Canvas.Brush.Color := clBlue;
          Self.IsColumnEnabled[LColIndex] := True;
          Self.Color := clWindow;
        end
        else
        begin
          Self.Canvas.Brush.Style := bsClear;
          Self.Canvas.Brush.Color := clWindow;
          if (ViewMode = vmEditableSelect) then
            Self.IsColumnEnabled[LColIndex] := False;
        end;
        Self.Canvas.FrameRect(LCellRect);
      end;
    end;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.

