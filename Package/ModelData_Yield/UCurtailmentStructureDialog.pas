{******************************************************************************}
{*  UNIT      : Contains the class TCurtailmentStructureDialog.             *}
{*  AUTHOR    : Riana Steyn                                                   *}
{*  DATE      : 2003/09/01                                                    *}
{*  COPYRIGHT : Copyright © 2003 DWAF                                         *}
{******************************************************************************}

unit UCurtailmentStructureDialog;

interface

uses
  Classes,
  VCL.Controls,
  VCL.ComCtrls,
  VCL.ExtCtrls,
  VCL.StdCtrls,
  VCL.Buttons,
  VCL.Graphics,
  UAbstractObject,
  UAbstractComponent,
  UDataEditComponent,
  UDataComponent;

type

  TCurtailmentStructureDialog = class(TAbstractScrollablePanel)
  private
  protected
    FCurtailmentCheck : TFieldChkBox;




    FNrOfCurtailmentPeriodLabel    : TLabel;
    FCurtailmentMonthNumberLabel   : TLabel;
    FNrOfCurtailmentChannelLabel   : TLabel;
    FCurtailmentChannelNumberLabel : TLabel;

    FNrOfCurtailmentPeriodEdit     : TFieldEdit;
    FCurtailmentMonthNumberGrid    : TFieldStringGrid;
    FNrOfCurtailmentChannelEdit    : TFieldEdit;
    FBtnAddChannel                 : TFieldBitBtn;
    FBtnDeleteChannel              : TFieldBitBtn;
    FCurtailmentChannelNumberCbx   : TFieldComboBox;
    FCurtailmentFactorsGrid        : TFieldStringGrid;

    procedure CreateMemberObjects; override;
    procedure AssignHelpContext; override;
  public
    procedure Resize; override;
    procedure RestoreColourState; override;
    function LanguageHasChanged: boolean; override;
    function Initialise: boolean; override;

    property CurtailmentCheck : TFieldChkBox     read FCurtailmentCheck;


    property NrOfCurtailmentPeriodEdit   : TFieldEdit       read FNrOfCurtailmentPeriodEdit;
    property CurtailmentMonthNumberGrid  : TFieldStringGrid read FCurtailmentMonthNumberGrid;
    property BtnAddChannel               : TFieldBitBtn     read FBtnAddChannel;
    property BtnDeleteChannel            : TFieldBitBtn     read FBtnDeleteChannel;
    property NrOfCurtailmentChannelEdit  : TFieldEdit       read FNrOfCurtailmentChannelEdit;
    property CurtailmentChannelNumberCbx : TFieldComboBox   read FCurtailmentChannelNumberCbx;
    property CurtailmentFactorsGrid      : TFieldStringGrid read FCurtailmentFactorsGrid;
  end;

  implementation

uses
  VCL.Grids,
  UConstants,
  VCL.Forms,
  SysUtils,
  UHelpContexts,
  UErrorHandlingOperations,
  UControlCreationUtilities;

const
  C_ControlBorder  = 10;
  C_LabelOffset    = 3;

{******************************************************************************}
{* TCurtailmentStructureDialog                                              *}
{******************************************************************************}

procedure TCurtailmentStructureDialog.CreateMemberObjects;
const OPNAME = 'TCurtailmentStructureDialog.CreateMemberObjects';
var
  lOwner  : TComponent;
  lParent : TWinControl;
begin
  inherited;
  try
    lOwner  := ControlsOwner;
    lParent := ControlsParent;
    //                                                                Left  Top Width Height

    FCurtailmentCheck := TFieldChkBox.Create(lOwner, FAppModules);
    FCurtailmentCheck.Parent := lParent;
    FCurtailmentCheck.Left   := 10;
    FCurtailmentCheck.Top    := 10;
    FCurtailmentCheck.Width  := 215;
    FCurtailmentCheck.Alignment := taLeftJustify;



    FNrOfCurtailmentPeriodLabel    := CreateFieldLabel(lOwner, lParent,  10,  35, 190, 21);
    FNrOfCurtailmentPeriodEdit     := CreateFieldEdit(FAppModules, lOwner, lParent, 210,  35,  30,  21, 0, TRUE);

    FCurtailmentMonthNumberLabel   := CreateFieldLabel(lOwner, lParent,  10,  60, 190, 21);
    FCurtailmentMonthNumberGrid    := CreateFieldStringGrid(FAppModules, lOwner, lParent, 210,  60,  30,  21, 1, TRUE);
    with FCurtailmentMonthNumberGrid do
    begin
      ScrollBars       := ssNone;
      ColCount         := 2;
      DefaultColWidth  := 40;
      DefaultRowHeight := 20;
      FixedCols        := 0;
      RowCount         := 1;
      FixedRows        := 0;
      Options          := [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goEditing];
    end;

    FNrOfCurtailmentChannelLabel   := CreateFieldLabel(lOwner, lParent,  10,  85, 190, 21);
    FNrOfCurtailmentChannelEdit    := CreateFieldEdit(FAppModules, lOwner, lParent, 210,  85,  30,  21, 1, TRUE);;

    FCurtailmentChannelNumberLabel := CreateFieldLabel(lOwner, lParent,  10,  110,  96, 26);
    FCurtailmentChannelNumberCbx   := CreateFieldComboBox(FAppModules, lOwner, lParent, 210,  110,  205,  21, 1, TRUE,csDropDownList);;

    FBtnAddChannel := TFieldBitBtn.Create(lOwner, FAppModules);
    FBtnAddChannel.Glyph.LoadFromResourceName(HImagesInstance, 'TIMECOMPARITORCREATEVIEW');
    with FBtnAddChannel do
    begin
      Parent  := lParent;
      Left    := 10;
      Top     := 135;
      Width   := 110;
      Height  := 25;
      ShowHint:= True;
    end;

    FBtnDeleteChannel := TFieldBitBtn.Create(lOwner, FAppModules);
    FBtnDeleteChannel.Glyph.LoadFromResourceName(HImagesInstance, 'TIMECOMPARITORDELETEVIEW');
    with FBtnDeleteChannel do
    begin
      Parent  := lParent;
      Left    := 130;
      Top     := 135;
      Width   := 110;
      Height  := 25;
      ShowHint:= True;
    end;

    FCurtailmentFactorsGrid := CreateFieldStringGrid(FAppModules, lOwner, lParent, 10, 165, 610, 176, 3, TRUE);
    with FCurtailmentFactorsGrid do
    begin
      BorderStyle      := bsSingle;
      FixedRows        := 1;
      ColCount         := 6;
      DefaultColWidth  := 40;
      DefaultRowHeight := 20;
      RowCount         := 4;
      ColWidths[0]     := 60;
      ColWidths[1]     := 60;
      ColWidths[2]     := 200;
      FixedCols        := 3;
      Options          := [goColSizing,goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goEditing];
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TCurtailmentStructureDialog.Resize;
const OPNAME = 'TCurtailmentStructureDialog.Resize';
begin
  inherited Resize;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TCurtailmentStructureDialog.Initialise: boolean;
const OPNAME = 'TCurtailmentStructureDialog.Initialise';
begin
  Result := inherited Initialise;
  try
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TCurtailmentStructureDialog.LanguageHasChanged: boolean;
const OPNAME = 'TCurtailmentStructureDialog.LanguageHasChanged';
begin
  Result := inherited LanguageHasChanged;
  try

    FCurtailmentCheck.Caption := FAppModules.Language.GetString('TField.CurtailmentAnalysis')    + ' :';

    CurtailmentFactorsGrid.Cells[0,0]   := 'Number';
    CurtailmentFactorsGrid.Cells[1,0]   := FAppModules.Language.GetString('GridHeading.ChannelNr');
    CurtailmentFactorsGrid.Cells[2,0]   := FAppModules.Language.GetString('GridHeading.ChannelName');


    FNrOfCurtailmentPeriodLabel.Caption   := FAppModules.Language.GetString('TField.CurtailmentPeriodCount')    + ' :';
    FCurtailmentMonthNumberLabel.Caption  := FAppModules.Language.GetString('TField.CurtailmentMonthNumber')    + ' :';
    FNrOfCurtailmentChannelLabel.Caption  := FAppModules.Language.GetString('TField.CurtailmentChannelCount')    + ' :';
    FCurtailmentChannelNumberLabel.Caption:= FAppModules.Language.GetString('TField.CurtailmentChannelNumber')    + ' :';
    FBtnAddChannel.Caption                := FAppModules.Language.GetString('ButtonCaption.AddChannel');
    FBtnAddChannel.Hint                   := FAppModules.Language.GetString('ButtonHint.AddSubSystem');
    FBtnDeleteChannel.Caption             := FAppModules.Language.GetString('ButtonCaption.DeleteChannel');
    FBtnDeleteChannel.Hint                := FAppModules.Language.GetString('ButtonHint.DeleteSubSystem');
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TCurtailmentStructureDialog.RestoreColourState;
const OPNAME = 'TCurtailmentStructureDialog.RestoreColourState';
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

procedure TCurtailmentStructureDialog.AssignHelpContext;
const OPNAME = 'TCurtailmentStructureDialog.AssignHelpContext';
begin
  try
    SetControlHelpContext(Self, HC_WaterResourcesYieldModel);
    SetControlHelpContext(FNrOfCurtailmentPeriodEdit, HC_WaterResourcesYieldModel);
    SetControlHelpContext(FCurtailmentMonthNumberGrid, HC_WaterResourcesYieldModel);
    SetControlHelpContext(FNrOfCurtailmentChannelEdit,     HC_WaterResourcesYieldModel);
    SetControlHelpContext(FCurtailmentChannelNumberCbx, HC_WaterResourcesYieldModel);
    SetControlHelpContext(FCurtailmentFactorsGrid,    HC_WaterResourcesYieldModel);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

end.
