//
//
//  UNIT      : Contains the class TGrowthProjectionsDialog.
//  AUTHOR    : Dziedzi Ramulondi (Cornastone)
//  DATE      : 2006/05/11
//  COPYRIGHT : Copyright © 2006 DWAF
//
//

unit UGrowthProjectionsDialog;

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
  TGrowthProjectionsDialog = class(TAbstractScrollablePanel)
  protected
    FgbEdits                : TGroupBox;
    FlblNumYears            : TLabel;
    FlblBaseYear            : TLabel;
    FlblStartYear           : TLabel;

    FedtNumYears            : TFieldEdit;
    //FBaseYearCbx            : TFieldComboBox;
    FedtBaseYear            : TFieldEdit;
    FedtStartYear           : TFieldEdit;

    //FbtnGenMinMaxFactors    : TFieldBitBtn;
    FbtnGenerateFactors     : TFieldBitBtn;
    FbtnValidateProjections : TFieldBitBtn;

    FstrgrdDemandFactors    : TFieldStringGrid;
    FstrgrdMinMaxFactors    : TFieldStringGrid;
    FstrgrdHydrologyFactors : TFieldStringGrid;

    FtbsDemandFactors       : TTabSheet;
    FtbsMinMaxFactors       : TTabSheet;
    FtbsHydrologyFactors    : TTabSheet;

    FFormulaPanel           : TAbstractPanel;
    FlblGrowthFormula       : TLabel;

    FpgcProjections         : TAbstractPageControl;
    procedure CreateMemberObjects; override;
    procedure AssignHelpContext; override;
  public
    procedure Resize; override;
    function Initialise: boolean; override;
    function LanguageHasChanged: boolean; override;
    function StudyHasChanged: boolean; override;
    function SaveState: boolean; override;

    property edtNumYears        : TFieldEdit       read FedtNumYears;
    property edtBaseYear        : TFieldEdit       read FedtBaseYear;
   // property BaseYearCbx        : TFieldComboBox   read FBaseYearCbx;
    property edtStartYear       : TFieldEdit       read FedtStartYear;
    property btnGenerateFactors : TFieldBitBtn     read FbtnGenerateFactors;
    property btnValidateProjections : TFieldBitBtn     read FbtnValidateProjections;
 //   property btnGenMinMaxFactors    : TFieldBitBtn     read FbtnGenMinMaxFactors;

    property tbsDemandFactors    : TTabSheet read FtbsDemandFactors;
    property tbsMinMaxFactors    : TTabSheet read FtbsMinMaxFactors;
    property tbsHydrologyFactors : TTabSheet read FtbsHydrologyFactors;

    property strgrdDemandFactors      : TFieldStringGrid read FstrgrdDemandFactors;
    property strgrdMinMaxFactors      : TFieldStringGrid read FstrgrdMinMaxFactors;
    property strgrdHydrologyFactors   : TFieldStringGrid read FstrgrdHydrologyFactors;
    property pgcProjections           : TAbstractPageControl read FpgcProjections;
  end;

  implementation

uses
  SysUtils,
  UHelpContexts,
  UControlCreationUtilities,
  UErrorHandlingOperations;

const
  C_ControlBorder  = 5;
  C_LabelOffset    = 3;
  C_GroupBoxOffset = 5;


{ TGrowthProjectionsDialog }

procedure TGrowthProjectionsDialog.CreateMemberObjects;
const OPNAME = 'TGrowthProjectionsDialog.CreateMemberObjects';
begin
  inherited;
  try
    FgbEdits                         := TGroupBox.Create(ControlsOwner);
    FlblNumYears                     := TLabel.Create(ControlsOwner);
    FlblBaseYear                     := TLabel.Create(ControlsOwner);
    FlblStartYear                    := TLabel.Create(ControlsOwner);

    FedtNumYears                     := TFieldEdit.Create(ControlsOwner,FAppModules);
    //FBaseYearCbx                     := TFieldComboBox.Create(ControlsOwner,FAppModules);
    FedtBaseYear                     := TFieldEdit.Create(ControlsOwner,FAppModules);
    FedtStartYear                    := TFieldEdit.Create(ControlsOwner,FAppModules);

    FstrgrdDemandFactors             := TFieldStringGrid.Create(ControlsOwner,FAppModules);
    FstrgrdMinMaxFactors             := TFieldStringGrid.Create(ControlsOwner,FAppModules);
    FstrgrdHydrologyFactors          := TFieldStringGrid.Create(ControlsOwner,FAppModules);

    FpgcProjections                  := TAbstractPageControl.Create(ControlsOwner,FAppModules);
    FtbsDemandFactors                := TTabSheet.Create(ControlsOwner);
    FtbsMinMaxFactors                := TTabSheet.Create(ControlsOwner);
    FtbsHydrologyFactors             := TTabSheet.Create(ControlsOwner);

    FpgcProjections.Parent           := ControlsParent;
    FtbsDemandFactors.Parent         := FpgcProjections;
    FtbsMinMaxFactors.Parent         := FpgcProjections;
    FtbsHydrologyFactors.Parent      := FpgcProjections;

    FstrgrdDemandFactors.Parent      := FtbsDemandFactors;
    FstrgrdMinMaxFactors.Parent      := FtbsMinMaxFactors;
    FstrgrdHydrologyFactors.Parent   := FtbsHydrologyFactors;

    FbtnGenerateFactors              := TFieldBitBtn.Create(ControlsOwner,FAppModules);
    FbtnValidateProjections          := TFieldBitBtn.Create(ControlsOwner,FAppModules);
 //   FbtnGenMinMaxFactors             := TFieldBitBtn.Create(ControlsOwner,FAppModules);

    FgbEdits.Parent                  := ControlsParent;
    FlblNumYears.Parent              := FgbEdits;
    FlblBaseYear.Parent              := FgbEdits;
    FlblStartYear.Parent             := FgbEdits;


    FedtNumYears.Parent              := FgbEdits;
//    FBaseYearCbx.Parent              := FgbEdits;
    FedtBaseYear.Parent              := FgbEdits;
    FedtStartYear.Parent             := FgbEdits;

    FbtnGenerateFactors.Parent       := FgbEdits;
    FbtnValidateProjections.Parent   := FgbEdits;
  //  FbtnGenMinMaxFactors.parent      := FgbEdits;

    FFormulaPanel                    := TAbstractPanel.Create(ControlsOwner, FAppModules);
    FFormulaPanel.Parent             := ControlsParent;

    FlblGrowthFormula                := TLabel.Create(FFormulaPanel);
    FlblGrowthFormula.Parent         := FFormulaPanel;

  except on E: Exception do HandleError(E, OPNAME) end;
end;


function TGrowthProjectionsDialog.Initialise: boolean;
const OPNAME = 'TGrowthProjectionsDialog.Initialise';
begin
  Result := inherited Initialise;
  try
    FlblNumYears.Alignment         := taRightJustify;
    FlblBaseYear.Alignment         := taRightJustify;
    FlblStartYear.Alignment        := taRightJustify;

    FedtNumYears.Text              := '';
//    FBaseYearCbx.ItemIndex         := -1;
    FedtBaseYear.Text              := '';
    FedtStartYear.Text             := '';

    FbtnGenerateFactors.Glyph.LoadFromResourceName(HImagesInstance,'EXPORTFILE');
    FbtnGenerateFactors.NumGlyphs  := 2;
    FbtnGenerateFactors.Enabled    := False;

    FbtnValidateProjections.Glyph.LoadFromResourceName(HImagesInstance,'VALIDATEFILE');
    FbtnValidateProjections.NumGlyphs  := 2;
    FbtnValidateProjections.Enabled    := False;

//    FbtnGenMinMaxFactors.Enabled := False;
//    FbtnGenMinMaxFactors.Visible := False;

    FstrgrdDemandFactors.RowCount  := 2;
    FstrgrdDemandFactors.ColCount  := 3;
    FstrgrdDemandFactors.FixedCols := 0;
    FstrgrdDemandFactors.FixedRows := 1;
    FstrgrdDemandFactors.Rows[0].Clear;
    FstrgrdDemandFactors.Rows[1].Clear;
    FstrgrdDemandFactors.DefaultRowHeight  := 15;
    FstrgrdDemandFactors.ShowGridPopupMenu := True;
    FstrgrdDemandFactors.AllowPasteFromExcel := True;

    FstrgrdMinMaxFactors.RowCount  := 2;
    FstrgrdMinMaxFactors.ColCount  := 4;
    FstrgrdMinMaxFactors.FixedCols := 0;
    FstrgrdMinMaxFactors.FixedRows := 1;
    FstrgrdMinMaxFactors.Rows[0].Clear;
    FstrgrdMinMaxFactors.Rows[1].Clear;
    FstrgrdMinMaxFactors.DefaultRowHeight  := 15;
    FstrgrdMinMaxFactors.ShowGridPopupMenu := True;
    FstrgrdMinMaxFactors.AllowPasteFromExcel := True;

    FstrgrdHydrologyFactors.RowCount  := 2;
    FstrgrdHydrologyFactors.ColCount  := 4;
    FstrgrdHydrologyFactors.FixedCols := 0;
    FstrgrdHydrologyFactors.FixedRows := 1;
    FstrgrdHydrologyFactors.Rows[0].Clear;
    FstrgrdHydrologyFactors.Rows[1].Clear;
    FstrgrdHydrologyFactors.DefaultRowHeight  := 15;
    FstrgrdHydrologyFactors.ShowGridPopupMenu := True;
    FstrgrdHydrologyFactors.AllowPasteFromExcel := True;

    FtbsDemandFactors.Caption    := FAppModules.language.GetString('TabCaption.DemandGrowthProjections');
    FtbsMinMaxFactors.Caption    := FAppModules.language.GetString('TabCaption.MinMaxGrowthProjections');
    FtbsHydrologyFactors.Caption := FAppModules.language.GetString('TabCaption.HydrologyGrowthProjections');

    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TGrowthProjectionsDialog.LanguageHasChanged: boolean;
const OPNAME = 'TGrowthProjectionsDialog.LanguageHasChanged';
begin
  Result := inherited LanguageHasChanged;
  try
    FgbEdits.Caption     := '';
    FlblNumYears.Caption := FAppModules.language.GetString('LabelText.NumberOfYears');
    FlblBaseYear.Caption := FAppModules.language.GetString('LabelText.BaseYear');
    FlblStartYear.Caption:= 'End Year'; //FAppModules.language.GetString('LabelText.StartYear');

    FbtnGenerateFactors.Caption     :=  FAppModules.language.GetString('ButtonCaption.GenerateFactors');
    FbtnValidateProjections.Caption :=  FAppModules.language.GetString('ButtonCaption.ValidateProjections');
    //FbtnGenMinMaxFactors.Caption    :=  'Generate Min-Max Factors';

    FstrgrdDemandFactors.Cells[0,0] := FAppModules.language.GetString('GridHeading.Institutions');
    FstrgrdDemandFactors.Cells[1,0] := FAppModules.language.GetString('GridHeading.Water Users');
    FstrgrdDemandFactors.Cells[2,0] := FAppModules.language.GetString('TField.MinMaxChannelNumber');;
  //  FstrgrdDemandFactors.Cells[3,0] := FAppModules.language.GetString('GridHeading.BaseYearDemand');

    FstrgrdMinMaxFactors.Cells[0,0] := FAppModules.language.GetString('GridHeading.Institutions');
    FstrgrdMinMaxFactors.Cells[1,0] := FAppModules.language.GetString('GridHeading.Water Users');
    FstrgrdMinMaxFactors.Cells[2,0] := FAppModules.language.GetString('TField.MinMaxChannelNumber');
    FstrgrdMinMaxFactors.Cells[3,0] := FAppModules.language.GetString('GridHeading.ArcNumber');
//    FstrgrdMinMaxFactors.Cells[4,0] := FAppModules.language.GetString('GridHeading.BaseYearDemand');

    FstrgrdHydrologyFactors.Cells[0,0] := FAppModules.language.GetString('GridHeading.Institutions');
    FstrgrdHydrologyFactors.Cells[1,0] := FAppModules.language.GetString('GridHeading.Water Users');
    FstrgrdHydrologyFactors.Cells[2,0] := FAppModules.language.GetString('GridHeading.GaugeNumber');
    FstrgrdHydrologyFactors.Cells[3,0] := FAppModules.language.GetString('GridHeading.FactorType');
//    FstrgrdHydrologyFactors.Cells[4,0] := FAppModules.language.GetString('GridHeading.BaseYearDemand');

    FlblGrowthFormula.Caption   :='Demand Projection = Base Year Demand * (Growth Factor + 1)' + #10+#13;
                                  //'Base Year Demand = (Projection Average per year)*((60*60*24*364.25)/ 1000000)';

   Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TGrowthProjectionsDialog.AssignHelpContext;
const OPNAME = 'TGrowthProjectionsDialog.AssignHelpContext';
begin
  try
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TGrowthProjectionsDialog.SaveState: boolean;
const OPNAME = 'TGrowthProjectionsDialog.SaveState';
begin
  Result := inherited SaveState;
  try
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TGrowthProjectionsDialog.StudyHasChanged: boolean;
const OPNAME = 'TGrowthProjectionsDialog.StudyHasChanged';
begin
  Result := inherited StudyHasChanged;
  try
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TGrowthProjectionsDialog.Resize;
const OPNAME = 'TGrowthProjectionsDialog.Resize';
begin
  inherited Resize;
  try
    FgbEdits.Height                := 40;
    FgbEdits.Align                 := alTop;
    FpgcProjections.Align          := alClient;
    FtbsDemandFactors.Align        := alClient;
    FtbsMinMaxFactors.Align        := alClient;
    FtbsHydrologyFactors.Align     := alClient;
    FstrgrdDemandFactors.Align     := alClient;
    FstrgrdMinMaxFactors.Align     := alClient;
    FstrgrdHydrologyFactors.Align  := alClient;
    FFormulaPanel.Align            := alBottom;

    FlblNumYears.Top               := 15;
    FlblNumYears.Width             := 85;

    FlblBaseYear.Top               := 15;
    FlblBaseYear.Width             := 55;

    FlblStartYear.Top              := 15;
    FlblStartYear.Width            := 55;

    FedtNumYears.Top               := 12;
    FedtNumYears.Width             := 35;

 //   FBaseYearCbx.Top               := 12;
    FedtBaseYear.Top               := 12;
   // FBaseYearCbx.Width             := 100;
    FedtBaseYear.Width             := 50;

    FedtStartYear.Top              := 12;
    FedtStartYear.Width            := 50;

    FtbsDemandFactors.PageControl    := FpgcProjections;
    FtbsMinMaxFactors.PageControl    := FpgcProjections;
    FtbsHydrologyFactors.PageControl := FpgcProjections;

    FlblNumYears.Left                := C_ControlOffset;
    FedtNumYears.Left                := FlblNumYears.Left   + FlblNumYears.Width   + C_LabelOffset;
    FlblBaseYear.Left                := FedtNumYears.Left   + FedtNumYears.Width   + C_ControlOffset;
//    FBaseYearCbx.Left                := FlblBaseYear.Left   + FlblBaseYear.Width   + C_ControlOffset;
    FedtBaseYear.Left                := FlblBaseYear.Left   + FlblBaseYear.Width   + C_LabelOffset;
    FlblStartYear.Left               := FedtBaseYear.Left   + FedtBaseYear.Width   + C_ControlOffset;
    //FlblStartYear.Left               := FBaseYearCbx.Left   + FBaseYearCbx.Width   + C_ControlOffset;
    FedtStartYear.Left               := FlblStartYear.Left  + FlblStartYear.Width  + C_LabelOffset;

    FbtnGenerateFactors.Top          := 10;
    FbtnGenerateFactors.Width        := 110;
    FbtnGenerateFactors.Left         := Self.ClientWidth - FbtnGenerateFactors.Width - 10;

    FbtnValidateProjections.Top      := 10;
    FbtnValidateProjections.Width    := 130;
    FbtnValidateProjections.Left     := FbtnGenerateFactors.Left - 5 - FbtnValidateProjections.Width ;

   // FbtnGenMinMaxFactors.Top         := 10;
//    FbtnGenMinMaxFactors.Width       := 150;
  //  FbtnGenMinMaxFactors.Left        := FbtnValidateProjections.Left - 5 - FbtnGenMinMaxFactors.Width ;

    FFormulaPanel.Height             := 28;
    FFormulaPanel.Width              := Self.ClientWidth - 10;

    FlblGrowthFormula.Top            := 1;
    FlblGrowthFormula.AutoSize       := True;
    FlblGrowthFormula.Alignment      := taCenter;
    FlblGrowthFormula.Align          := alClient;


  except on E: Exception do HandleError(E, OPNAME) end;

end;

end.
