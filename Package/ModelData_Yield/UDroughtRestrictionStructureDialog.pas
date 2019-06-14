{******************************************************************************}
{*  UNIT      : Contains the class TDroughtRestrictionStructureDialog         *}
{*  AUTHOR    : Presley Mudau                                                 *}
{*  DATE      : 2007/06/14                                                    *}
{*  COPYRIGHT : Copyright © 2007 DWAF                                         *}
{******************************************************************************}

unit UDroughtRestrictionStructureDialog;

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

  TDroughtRestrictionStructureDialog = class(TAbstractScrollablePanel)
  private
  protected
    FNrOfChanInSummaryLabel    : TLabel;
    FNrOfResInSummaryLabel     : TLabel;
    FNrOfActiveReservoirsLabel : TLabel;

    FDroughtRestrictionIdLabel   : TLabel;
    FDroughtRestrictionNameLabel : TLabel;
    FDroughtRestrictionIdEdit    : TFieldEdit;
    FDroughtRestrictionNameEdit  : TFieldEdit;

    FNrOfReservoirsEdit        : TFieldEdit;
    FNrOfReservorsInRefEdit    : TFieldEdit;
    FNrOfRestrictedChannelEdit : TFieldEdit;

    FClickResActiveLabel       : TLabel;
    FClickChanFirmYieldLabel   : TLabel;
    FClickResActiveImage       : TImage;
    FClickChanFirmYieldImage   : TImage;

    FReservoirGroupBox         : TGroupBox;
    FChannelGroupBox           : TGroupBox;
    FDroughtGroupBox           : TGroupBox;

    FReservoirCheckLbx         : TFieldCheckListBox;
    FChannelCheckLbx           : TFieldCheckListBox;
    FDroughtRestrictionGrid    : TFieldStringGrid;

    procedure CreateMemberObjects; override;
    procedure AssignHelpContext; override;
  public
    procedure Resize; override;
    procedure RestoreColourState; override;
    function LanguageHasChanged: boolean; override;
    function Initialise: boolean; override;

    property DroughtRestrictionIdEdit   : TFieldEdit        read FDroughtRestrictionIdEdit;
    property DroughtRestrictionNameEdit : TFieldEdit        read FDroughtRestrictionNameEdit;
    property NrOfChanInSummaryLabel    : TLabel             read FNrOfChanInSummaryLabel;
    property NrOfResInSummaryLabel     : TLabel             read FNrOfResInSummaryLabel;
    property NrOfActiveReservoirsLabel : TLabel             read FNrOfActiveReservoirsLabel;
    property NrOfReservorsInRefEdit    : TFieldEdit         read FNrOfReservorsInRefEdit;
    property NrOfReservoirsEdit        : TFieldEdit         read FNrOfReservoirsEdit ;
    property NrOfRestrictedChannelEdit : TFieldEdit         read FNrOfRestrictedChannelEdit;
    property ReservoirGroupBox         : TGroupBox          read FReservoirGroupBox;
    property ChannelGroupBox           : TGroupBox          read FChannelGroupBox;
    property DroughtGroupBox           : TGroupBox          read FDroughtGroupBox;
    property ReservoirCheckLbx         : TFieldCheckListBox read FReservoirCheckLbx;
    property ChannelCheckLbx           : TFieldCheckListBox read FChannelCheckLbx;
    property DroughtRestrictionGrid    : TFieldStringGrid   read FDroughtRestrictionGrid;

  end;

  implementation

uses
  SysUtils,
  VCL.Grids,
  UHelpContexts,
  UErrorHandlingOperations,
  UControlCreationUtilities,
  VCL.ImgList;

{******************************************************************************}
{* TDroughtRestrictionStructureDialog                                                 *}
{******************************************************************************}

procedure TDroughtRestrictionStructureDialog.CreateMemberObjects;
const OPNAME = 'TDroughtRestrictionStructureDialog.CreateMemberObjects';
var
  lOwner  : TComponent;
  lParent : TWinControl;
begin
  inherited;
  try
    lOwner  := ControlsOwner;
    lParent := ControlsParent;
    //                                                                 Left  Top  Width Height
    FReservoirGroupBox          := CreateFieldGroupBox(lOwner,lParent, 10, 55, 300, 350,0,FALSE);
    FChannelGroupBox            := CreateFieldGroupBox(lOwner,lParent, 330, 55, 330, 350,0,FALSE);
    FDroughtGroupBox            := CreateFieldGroupBox(lOwner,lParent, 10, 410, 650, 80,0,FALSE);
    FNrOfResInSummaryLabel      := CreateFieldLabel  (lOwner, FReservoirGroupBox,  10, 280, 200,  26);
    FNrOfActiveReservoirsLabel  := CreateFieldLabel  (lOwner, FReservoirGroupBox,  10, 310, 200,  26);
    FNrOfChanInSummaryLabel     := CreateFieldLabel  (lOwner, FChannelGroupBox, 10, 280, 197,  26);
                                                                                                                      
    FDroughtRestrictionIdLabel   := CreateFieldLabel  (lOwner, lParent,  10, 10, 210,  26);
    FDroughtRestrictionNameLabel := CreateFieldLabel  (lOwner, lParent,  10, 35, 210,  26);
    FDroughtRestrictionIdEdit    := CreateFieldEdit   (FAppModules, lOwner, lParent, 150, 10,  40,  21, 0, FALSE);
    FDroughtRestrictionNameEdit  := CreateFieldEdit   (FAppModules, lOwner, lParent, 150, 35,  180,  21, 0, FALSE);

    FNrOfReservoirsEdit   := CreateFieldEdit   (FAppModules, lOwner, FReservoirGroupBox, 220, 280,  55,  21, 0, FALSE);
    FNrOfRestrictedChannelEdit      := CreateFieldEdit   (FAppModules, lOwner, FChannelGroupBox, 220, 280,  40,  21, 0, FALSE);
    FNrOfReservorsInRefEdit       := CreateFieldEdit   (FAppModules, lOwner, FReservoirGroupBox, 220, 310,  55,  21, 0, FALSE);

    FNrOfActiveReservoirsLabel.WordWrap := TRUE;
    FNrOfResInSummaryLabel.WordWrap     := TRUE;
    FNrOfChanInSummaryLabel.WordWrap    := TRUE;

    FClickResActiveLabel       := CreateFieldLabel  (lOwner, FReservoirGroupBox,  32, 30, 260,  30);
    FClickResActiveLabel.WordWrap := True;
    FClickResActiveImage       := TImage.Create(Self);
    with FClickResActiveImage do
    begin
      Parent   := FReservoirGroupBox;
      Left     := 10;
      Top      := 30;
      Width    := 16;
      Height   := 16;
      AutoSize := True;
    end;
    FClickResActiveImage.Picture.Bitmap.LoadFromResourceName(HImagesInstance, 'UNCHECKCYAN');


    FClickChanFirmYieldLabel   := CreateFieldLabel  (lOwner, FChannelGroupBox, 32, 30, 290,  13);
    FClickChanFirmYieldImage   := TImage.Create(Self);
    with FClickChanFirmYieldImage do
    begin
      Parent   := FChannelGroupBox;
      Left     := 10;
      Top      := 30;
      Width    := 16;
      Height   := 16;
      AutoSize := True;
    end;
    FClickChanFirmYieldImage.Picture.Bitmap.LoadFromResourceName(HImagesInstance, 'UNCHECKYELLOW');

    FReservoirCheckLbx := CreateFieldCheckListBox(FAppModules, lOwner, FReservoirGroupBox, 10,  70, 250, 200, 7, TRUE);
    FChannelCheckLbx   := CreateFieldCheckListBox(FAppModules, lOwner, FChannelGroupBox, 10,  70, 250, 200, 7, TRUE);

    FDroughtRestrictionGrid := CreateFieldStringGrid(FAppModules, lOwner, FDroughtGroupBox, 10, 10, 620, 60, 3, TRUE);
    with FDroughtRestrictionGrid do
    begin
      ColCount         := 11;
      FixedCols        := 1;
      FixedRows        := 0;
      DefaultColWidth  := 45;
      DefaultRowHeight := 20;
      RowCount         := 2;
      ColWidths[0]     := 150;
      Options          := [goColSizing,goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goEditing];
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDroughtRestrictionStructureDialog.Resize;
const OPNAME = 'TDroughtRestrictionStructureDialog.Resize';
begin
  inherited Resize;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDroughtRestrictionStructureDialog.Initialise: boolean;
const OPNAME = 'TDroughtRestrictionStructureDialog.Initialise';
begin
  Result := inherited Initialise;
  try
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDroughtRestrictionStructureDialog.LanguageHasChanged: boolean;
const OPNAME = 'TDroughtRestrictionStructureDialog.LanguageHasChanged';
begin
  Result := inherited LanguageHasChanged;
  try

    FDroughtRestrictionIdLabel.Caption   := FAppModules.Language.GetString('TField.DroughtRestrictionID');
    FDroughtRestrictionNameLabel.Caption := FAppModules.Language.GetString('TField.DroughtRestrictionName');

    FClickResActiveLabel.Caption       := FAppModules.Language.GetString('DroughtRestriction.ClickReservoirDroughtRestriction');
    FClickChanFirmYieldLabel.Caption   := FAppModules.Language.GetString('DroughtRestriction.ClickChannelDroughtRestriction');

    FNrOfActiveReservoirsLabel.Caption := FAppModules.Language.GetString('DroughtRestriction.DroughtNrOfActiveReservoirs') + ' :';
    FNrOfResInSummaryLabel.Caption     := FAppModules.Language.GetString('DroughtRestriction.DroughtNrOfResInSummary') + ' :';

    FNrOfChanInSummaryLabel.Caption    := FAppModules.Language.GetString('DroughtRestriction.DroughtNrOfChanInSummary') + ' :';

    FDroughtRestrictionGrid.Cells[0,0] := FAppModules.Language.GetString('TField.ReferenceStorageVolumes');
    FDroughtRestrictionGrid.Cells[0,1] := FAppModules.Language.GetString('TField.AllocationFactors');
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDroughtRestrictionStructureDialog.RestoreColourState;
const OPNAME = 'TDroughtRestrictionStructureDialog.RestoreColourState';
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

procedure TDroughtRestrictionStructureDialog.AssignHelpContext;
const OPNAME = 'TDroughtRestrictionStructureDialog.AssignHelpContext';
begin
  try
    SetControlHelpContext(Self,                      HC_ChannelOutputOptions);
    SetControlHelpContext(FNrOfReservoirsEdit,     HC_ReservoirOutputOptions);
    SetControlHelpContext(FNrOfRestrictedChannelEdit, HC_ReservoirOutputOptions);
    SetControlHelpContext(FNrOfReservorsInRefEdit,   HC_ChannelOutputOptions);
    SetControlHelpContext(FChannelGroupBox,          HC_ChannelOutputOptions);
    SetControlHelpContext(FReservoirGroupBox,        HC_ReservoirOutputOptions);
    SetControlHelpContext(FDroughtGroupBox,          HC_ReservoirOutputOptions);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

end.
