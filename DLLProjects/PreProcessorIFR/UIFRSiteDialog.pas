//
//
//  UNIT      : Contains TIFRSiteDialog Class
//  AUTHOR    : Dziedzi Ramulondi (arivia.kom)
//  DATE      : 09/01/2007
//  COPYRIGHT : Copyright © 2007 DWAF
//
//

unit UIFRSiteDialog;

interface

uses
  Classes,
  VCLTee.Chart,
  VCLTee.Series,
  VCLTee.TeeProcs,
  VCLTee.TeEngine,
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

  TIFRSiteDialog = class(TAbstractScrollablePanel)
  protected
    FTopPanel                : TPanel;
    FNameLabel               : TLabel;
    FNameEdit                : TFieldEdit;

    FLocationGroupBox        : TGroupBox;
    FXCoordLabel             : TLabel;
    FXCoordEdit              : TFieldEdit;
    FYCoordLabel             : TLabel;
    FYCoordEdit              : TFieldEdit;
    FQuaternaryLabel         : TLabel;
    FQuaternaryEdit          : TFieldEdit;
    FRiverLabel              : TLabel;
    FRiverEdit               : TFieldEdit;
    FDescriptionLabel        : TLabel;
    FDescriptionEdit         : TFieldEdit;

    FRightGroupBox           : TGroupBox;
    FAssociatedEMCLabel      : TLabel;
    FAssociatedEMCCbx        : TFieldComboBox;
    FDetailLevelLabel        : TLabel;
    FDetailLevelCbx          : TFieldComboBox;
    FConfidenceLevelLabel    : TLabel;
    FConfidenceLevelCbx      : TFieldComboBox;
    FInfoSourceLabel         : TLabel;
    FInfoSourceEdit          : TFieldEdit;

    FIFRPageControl          : TPageControl;
    FTableTabSheet           : TTabSheet;
    FGraphTabSheet           : TTabSheet;
    FGraphGroupBox           : TGroupBox;
    FGraphViewTypeLabel      : TLabel;
    FGraphViewTypeCbx        : TFieldComboBox;
    FIFRGraph                : TAbstractChart;
    FValuesGrid              : TFieldStringGrid;
    procedure CreateMemberObjects; override;
  public
    procedure AssignHelpContext; override;
    procedure Resize; override;
    function LanguageHasChanged: boolean; override;
    function Initialise: boolean; override;

    property NameEdit           : TFieldEdit       read FNameEdit;
    property XCoordEdit         : TFieldEdit       read FXCoordEdit;
    property YCoordEdit         : TFieldEdit       read FYCoordEdit;
    property QuaternaryEdit     : TFieldEdit       read FQuaternaryEdit;
    property RiverEdit          : TFieldEdit       read FRiverEdit;
    property DescriptionEdit    : TFieldEdit       read FDescriptionEdit;
    property AssociatedEMCCbx   : TFieldComboBox   read FAssociatedEMCCbx;
    property DetailLevelCbx     : TFieldComboBox   read FDetailLevelCbx;
    property ConfidenceLevelCbx : TFieldComboBox   read FConfidenceLevelCbx;
    property InfoSourceEdit     : TFieldEdit       read FInfoSourceEdit;
    property GraphViewTypeCbx   : TFieldComboBox   read FGraphViewTypeCbx;
    property IFRGraph           : TAbstractChart   read FIFRGraph;
    property ValuesGrid         : TFieldStringGrid read FValuesGrid;
  end;

  implementation

uses
  SysUtils,
  UHelpContexts,
  VCL.Forms,
  VCL.Grids,
  UErrorHandlingOperations,
  UControlCreationUtilities;

{******************************************************************************}
{* TIFRSiteDialog                                                          *}
{******************************************************************************}

procedure TIFRSiteDialog.CreateMemberObjects;
const OPNAME = 'TIFRSiteDialog.CreateMemberObjects';
var
  lOwner  : TComponent;
  lParent : TWinControl;
  //LTop    : integer;
begin
  inherited;
  try
    lOwner  := ControlsOwner;
    lParent := ControlsParent;

    FTopPanel                := TPanel.Create(lOwner);
    FTopPanel.Parent         := lParent;
    FTopPanel.Align          := alTop;

    //                                                                               Left  Top  Width Height
    FNameLabel               := CreateFieldLabel       (             lOwner, FTopPanel,  10,  5,   130,    21);
    FNameEdit                := CreateFieldEdit        (FAppModules, lOwner, FTopPanel, 140,  5,   160,    21, 0, TRUE);

    FLocationGroupBox        := TGroupBox.Create(lOwner);
    FLocationGroupBox.Parent := FTopPanel;
    FLocationGroupBox.Left   := 10;
    FLocationGroupBox.Top    := FNameEdit.Top  + FNameEdit.Height + 5;

    FXCoordLabel             := CreateFieldLabel       (             lOwner, FLocationGroupBox,  10,  15,   130,    21);
    FXCoordEdit              := CreateFieldEdit        (FAppModules, lOwner, FLocationGroupBox, 130,  15,   160,    21, 0, TRUE);
    FYCoordLabel             := CreateFieldLabel       (             lOwner, FLocationGroupBox,  10,  40,   130,    21);
    FYCoordEdit              := CreateFieldEdit        (FAppModules, lOwner, FLocationGroupBox, 130,  40,   160,    21, 0, TRUE);
    FQuaternaryLabel         := CreateFieldLabel       (             lOwner, FLocationGroupBox,  10,  65,   130,    21);
    FQuaternaryEdit          := CreateFieldEdit        (FAppModules, lOwner, FLocationGroupBox, 130,  65,   160,    21, 0, TRUE);
    FRiverLabel              := CreateFieldLabel       (             lOwner, FLocationGroupBox,  10,  90,   130,    21);
    FRiverEdit               := CreateFieldEdit        (FAppModules, lOwner, FLocationGroupBox, 130,  90,   160,    21, 0, TRUE);
    FDescriptionLabel        := CreateFieldLabel       (             lOwner, FLocationGroupBox,  10, 115,   130,    21);
    FDescriptionEdit         := CreateFieldEdit        (FAppModules, lOwner, FLocationGroupBox, 130, 115,   160,    21, 0, TRUE);

    FLocationGroupBox.Height := FDescriptionEdit.Top + FDescriptionEdit.Height + 10;
    FLocationGroupBox.Width  := FDescriptionEdit.Left + FDescriptionEdit.Width + 10;

    FRightGroupBox           := TGroupBox.Create(lOwner);
    FRightGroupBox.Parent    := FTopPanel;
    FRightGroupBox.Left      := FLocationGroupBox.Width + 20;
    FRightGroupBox.Top       := FNameEdit.Top  + FNameEdit.Height + 5;

    FAssociatedEMCLabel      := CreateFieldLabel       (             lOwner, FRightGroupBox,  10, 15, 130,  21);
    FAssociatedEMCCbx        := CreateFieldComboBox    (FAppModules, lOwner, FRightGroupBox, 140, 15, 160,  21, 0, TRUE,csDropDown);
    FDetailLevelLabel        := CreateFieldLabel       (             lOwner, FRightGroupBox,  10, 40, 130,  21);
    FDetailLevelCbx          := CreateFieldComboBox    (FAppModules, lOwner, FRightGroupBox, 140, 40, 160,  21 ,0,True,csDropDown);
    FConfidenceLevelLabel    := CreateFieldLabel       (             lOwner, FRightGroupBox,  10, 65, 130,  21);
    FConfidenceLevelCbx      := CreateFieldComboBox    (FAppModules, lOwner, FRightGroupBox, 140, 65, 160,  21, 0,True,csDropDown);
    FInfoSourceLabel         := CreateFieldLabel       (             lOwner, FRightGroupBox,  10, 90, 130,  21);
    FInfoSourceEdit          := CreateFieldEdit        (FAppModules, lOwner, FRightGroupBox, 140, 90, 160,  21, 0, TRUE);

    FRightGroupBox.Height    := FDescriptionEdit.Top + FDescriptionEdit.Height + 10;
    FRightGroupBox.Width     := FDescriptionEdit.Left + FDescriptionEdit.Width + 20;

    FTopPanel.Height         := FRightGroupBox.Top + FRightGroupBox.Height + 5;

    FIFRPageControl          := TPageControl.Create(lOwner);
    FIFRPageControl.Parent   := lParent;
    FIFRPageControl.Align    := alClient;

    FTableTabSheet           := TTabSheet.Create(lOwner);
    FTableTabSheet.Parent    := FIFRPageControl;
    FTableTabSheet.Align     := alClient;

    FGraphTabSheet           := TTabSheet.Create(lOwner);
    FTableTabSheet.Parent    := FIFRPageControl;
    FGraphTabSheet.Align     := alClient;

    FGraphGroupBox           := TGroupBox.Create(lOwner);
    FGraphGroupBox.Parent    := FGraphTabSheet;
    FGraphGroupBox.Align     := alTop;
    FGraphGroupBox.Height    := 35;

    FGraphViewTypeLabel      := CreateFieldLabel       (             lOwner, FGraphGroupBox,  10, 10, 130,  21);
    FGraphViewTypeCbx        := CreateFieldComboBox    (FAppModules, lOwner, FGraphGroupBox, 140, 10, 160,  21,0,True,csDropDown);
    FIFRGraph                := TAbstractChart.Create(lOwner,FAppModules);
    FIFRGraph.Parent         := FGraphTabSheet;
    FIFRGraph.Align          := alClient;
    FGraphViewTypeLabel.Alignment := taRightJustify;

    FValuesGrid              := CreateFieldStringGrid(FAppModules,lOwner,FTableTabSheet,0,0,10,10,0,True);
    FValuesGrid.Align        := alClient;
    FValuesGrid.RowCount     := 2;
    FValuesGrid.ColCount     := 13;
    FValuesGrid.FixedRows    := 1;
    FValuesGrid.FixedCols    := 0;
    //FValuesGrid.ColWidths[0]     := 15;
    FValuesGrid.DefaultRowHeight := 20;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIFRSiteDialog.Resize;
const OPNAME = 'TIFRSiteDialog.Resize';
begin
  inherited Resize;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TIFRSiteDialog.Initialise: boolean;
const OPNAME = 'TIFRSiteDialog.Initialise';
begin
  Result := inherited Initialise;
  try
    FTableTabSheet.PageControl := FIFRPageControl;
    FGraphTabSheet.PageControl := FIFRPageControl;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TIFRSiteDialog.LanguageHasChanged: boolean;
const OPNAME = 'TIFRSiteDialog.LanguageHasChanged';
begin
  Result := inherited LanguageHasChanged;
  try
    FNameLabel.Caption               := FAppModules.Language.GetString('TIFRSiteDialog.IFRSiteName') + ' :';
    FLocationGroupBox.Caption        := FAppModules.Language.GetString('TIFRSiteDialog.IFRSiteLocationGroup') + ' :';
    FXCoordLabel.Caption             := FAppModules.Language.GetString('TIFRSiteDialog.IFRSiteXCoord') + ' :';
    FYCoordLabel.Caption             := FAppModules.Language.GetString('TIFRSiteDialog.IFRSiteYCoord') + ' :';
    FQuaternaryLabel.Caption         := FAppModules.Language.GetString('TIFRSiteDialog.IFRSiteQuaternary') + ' :';
    FRiverLabel.Caption              := FAppModules.Language.GetString('TIFRSiteDialog.IFRSiteRiver') + ' :';
    FDescriptionLabel.Caption        := FAppModules.Language.GetString('TIFRSiteDialog.IFRSiteDescription') + ' :';
    FAssociatedEMCLabel.Caption      := FAppModules.Language.GetString('TIFRSiteDialog.IFRSiteAssociatedEMC') + ' :';
    FDetailLevelLabel.Caption        := FAppModules.Language.GetString('TIFRSiteDialog.IFRSiteDetailLevel') + ' :';
    FConfidenceLevelLabel.Caption    := FAppModules.Language.GetString('TIFRSiteDialog.IFRSiteConfidenceLevel') + ' :';
    FInfoSourceLabel.Caption         := FAppModules.Language.GetString('TIFRSiteDialog.IFRSiteInfoSource') + ' :';
    FTableTabSheet.Caption           := FAppModules.Language.GetString('TIFRSiteDialog.IFRSiteTableTabSheet')+ ' (Mm3/mon) ';
    FGraphTabSheet.Caption           := FAppModules.Language.GetString('TIFRSiteDialog.IFRSiteGraphTabSheet');
    FGraphGroupBox.Caption           := '';
    FGraphViewTypeLabel.Caption      := FAppModules.Language.GetString('TIFRSiteDialog.IFRSiteGraphViewType') + ' :';
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIFRSiteDialog.AssignHelpContext;
const OPNAME = 'TIFRSiteDialog.AssignHelpContext';
begin
  inherited;
  try
    SetControlHelpContext(Self,                HC_CreatingIFRSites);
    SetControlHelpContext(FNameEdit,           HC_CreatingIFRSites);
    SetControlHelpContext(FXCoordEdit,         HC_CreatingIFRSites);
    SetControlHelpContext(FYCoordEdit,         HC_CreatingIFRSites);
    SetControlHelpContext(FQuaternaryEdit,     HC_CreatingIFRSites);
    SetControlHelpContext(FRiverEdit,          HC_CreatingIFRSites);
    SetControlHelpContext(FDescriptionEdit,    HC_CreatingIFRSites);
    SetControlHelpContext(FAssociatedEMCCbx,   HC_CreatingIFRSites);
    SetControlHelpContext(FDetailLevelCbx,     HC_CreatingIFRSites);
    SetControlHelpContext(FConfidenceLevelCbx, HC_CreatingIFRSites);
    SetControlHelpContext(FInfoSourceEdit,     HC_CreatingIFRSites);
    SetControlHelpContext(FValuesGrid,         HC_CreatingIFRSites);
    SetControlHelpContext(FIFRGraph,           HC_CreatingIFRSites);
  except on E : Exception do HandleError(E,OPNAME); end;
end;

end.

