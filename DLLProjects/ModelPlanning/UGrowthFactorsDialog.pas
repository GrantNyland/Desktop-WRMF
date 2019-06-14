//
//
//  UNIT      : Contains the class TAnnualGrowthFactorsDialog.
//  AUTHOR    : Dziedzi Ramulondi (Cornastone)
//  DATE      : 2006/05/11
//  COPYRIGHT : Copyright © 2006 DWAF
//
//

unit UGrowthFactorsDialog;

interface

uses
  Classes,
  VCL.Controls,
  VCL.ComCtrls,
  VCL.ExtCtrls,
  VCL.StdCtrls,
  VCL.Buttons,
  VCL.Graphics,
  VCL.Grids,
  UAbstractObject,
  UAbstractComponent,
  UDataEditComponent,
  UDataComponent;

type
  TGrowthFactorsDialog = class(TAbstractScrollablePanel)
  protected
    FPnlBottom              : TPanel;
    FgbEdits                : TGroupBox;

    FlblNumYears            : TLabel;
    FlblStartYear           : TLabel;
    FlblEndYear             : TLabel;
    FedtNumYears            : TFieldEdit;
    FedtStartYear           : TFieldEdit;
    FedtEndYear             : TFieldEdit;

    FbtnGenerateProjections : TFieldBitBtn;

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
    property edtStartYear       : TFieldEdit       read FedtStartYear;
    property edtEndYear         : TFieldEdit       read FedtEndYear;

    property tbsDemandFactors    : TTabSheet read FtbsDemandFactors;
    property tbsMinMaxFactors    : TTabSheet read FtbsMinMaxFactors;
    property tbsHydrologyFactors : TTabSheet read FtbsHydrologyFactors;

    property strgrdDemandFactors      : TFieldStringGrid read FstrgrdDemandFactors;
    property strgrdMinMaxFactors      : TFieldStringGrid read FstrgrdMinMaxFactors;
    property strgrdHydrologyFactors   : TFieldStringGrid read FstrgrdHydrologyFactors;
    property pgcProjections           : TAbstractPageControl read FpgcProjections;
    property btnGenerateProjections   : TFieldBitBtn     read FbtnGenerateProjections;

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


{ TGrowthFactorsDialog }

procedure TGrowthFactorsDialog.CreateMemberObjects;
const OPNAME = 'TGrowthFactorsDialog.CreateMemberObjects';
begin
  inherited;
  try
    FPnlBottom                       := TPanel.Create(ControlsOwner);
    FPnlBottom.Parent                := ControlsParent;

    FgbEdits                         := TGroupBox.Create(ControlsOwner);
    FlblNumYears                     := TLabel.Create(ControlsOwner);
    FlblStartYear                    := TLabel.Create(ControlsOwner);
    FlblEndYear                      := TLabel.Create(ControlsOwner);

    FedtNumYears                     := TFieldEdit.Create(ControlsOwner,FAppModules);
    FedtStartYear                    := TFieldEdit.Create(ControlsOwner,FAppModules);
    FedtEndYear                      := TFieldEdit.Create(ControlsOwner,FAppModules);

    FstrgrdDemandFactors             := TFieldStringGrid.Create(ControlsOwner,FAppModules);
    FstrgrdMinMaxFactors             := TFieldStringGrid.Create(ControlsOwner,FAppModules);
    FstrgrdHydrologyFactors          := TFieldStringGrid.Create(ControlsOwner,FAppModules);

    FpgcProjections                  := TAbstractPageControl.Create(ControlsOwner,FAppModules);
    FtbsDemandFactors                := TTabSheet.Create(ControlsOwner);
    FtbsMinMaxFactors                := TTabSheet.Create(ControlsOwner);
    FtbsHydrologyFactors             := TTabSheet.Create(ControlsOwner);

    FgbEdits.Parent                  := ControlsParent;
    FlblNumYears.Parent              := FgbEdits;
    FlblStartYear.Parent             := FgbEdits;
    FlblEndYear.Parent               := FgbEdits;

    FedtNumYears.Parent              := FgbEdits;
    FedtStartYear.Parent             := FgbEdits;
    FedtEndYear.Parent               := FgbEdits;

    FpgcProjections.Parent           := FPnlBottom;
    FtbsDemandFactors.Parent         := FPnlBottom;
    FstrgrdDemandFactors.Parent      := FtbsDemandFactors;


    FtbsMinMaxFactors.Parent         := FPnlBottom;
    FstrgrdMinMaxFactors.Parent      := FtbsMinMaxFactors;

    FtbsHydrologyFactors.Parent      := FPnlBottom;
    FstrgrdHydrologyFactors.Parent   := FtbsHydrologyFactors;

    FbtnGenerateProjections          := TFieldBitBtn.Create(ControlsOwner,FAppModules);
    FbtnGenerateProjections.Parent   := FgbEdits;

    FFormulaPanel                    := TAbstractPanel.Create(ControlsOwner, FAppModules);
    FFormulaPanel.Parent             := ControlsParent;

    FlblGrowthFormula                := TLabel.Create(FFormulaPanel);
    FlblGrowthFormula.Parent         := FFormulaPanel;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TGrowthFactorsDialog.Initialise: boolean;
const OPNAME = 'TGrowthFactorsDialog.Initialise';
begin
  Result := inherited Initialise;
  try
    FlblNumYears.Alignment    := taRightJustify;

    FedtNumYears.Text    := '';
    FedtStartYear.Text  := '';
    FedtEndYear.Text    := '';

    FbtnGenerateProjections.Glyph.LoadFromResourceName(HImagesInstance,'EXPORTFILE');
    FbtnGenerateProjections.NumGlyphs := 2;
    FbtnGenerateProjections.Enabled  := False;

    FstrgrdDemandFactors.RowCount  := 2;
    FstrgrdDemandFactors.ColCount  := 1;
    FstrgrdDemandFactors.FixedCols := 0;
    FstrgrdDemandFactors.FixedRows := 1;
    FstrgrdDemandFactors.Rows[0].Clear;
    FstrgrdDemandFactors.Rows[1].Clear;
    FstrgrdDemandFactors.DefaultRowHeight  := 15;
    FstrgrdDemandFactors.AllowPasteFromExcel   := True;
    FstrgrdDemandFactors.ShowGridPopupMenu := True;
//    FstrgrdDemandFactors.Options := [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goEditing];

    FstrgrdMinMaxFactors.RowCount  := 2;
    FstrgrdMinMaxFactors.ColCount  := 1;
    FstrgrdMinMaxFactors.FixedCols := 0;
    FstrgrdMinMaxFactors.FixedRows := 1;
    FstrgrdMinMaxFactors.Rows[0].Clear;
    FstrgrdMinMaxFactors.Rows[1].Clear;
    FstrgrdMinMaxFactors.DefaultRowHeight  := 15;
    FstrgrdMinMaxFactors.AllowPasteFromExcel   := True;
    FstrgrdMinMaxFactors.ShowGridPopupMenu := True;
//    FstrgrdMinMaxFactors.Options := [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goEditing];

    FstrgrdHydrologyFactors.RowCount  := 2;
    FstrgrdHydrologyFactors.ColCount  := 1;
    FstrgrdHydrologyFactors.FixedCols := 0;
    FstrgrdHydrologyFactors.FixedRows := 1;
    FstrgrdHydrologyFactors.Rows[0].Clear;
    FstrgrdHydrologyFactors.Rows[1].Clear;
    FstrgrdHydrologyFactors.DefaultRowHeight  := 15;
    FstrgrdHydrologyFactors.AllowPasteFromExcel   := True;
    FstrgrdHydrologyFactors.ShowGridPopupMenu     := True;
//    FstrgrdHydrologyFactors.Options := [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goEditing];
    FpgcProjections.ActivePage := FtbsDemandFactors;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TGrowthFactorsDialog.LanguageHasChanged: boolean;
const OPNAME = 'TGrowthFactorsDialog.LanguageHasChanged';
begin
  Result := inherited LanguageHasChanged;
  try
   FPnlBottom.Align                    := alClient;
   FPnlBottom.BevelOuter               := bvNone;

    FgbEdits.Caption                   := '';
    FlblNumYears.Caption               := FAppModules.language.GetString('LabelText.NumberOfYears');
    FlblStartYear.Caption              := FAppModules.language.GetString('LabelText.StartYear');
    FlblEndYear.Caption                := FAppModules.language.GetString('LabelText.EndYear');

    FtbsDemandFactors.Caption          :=  FAppModules.language.GetString('TabCaption.DemandGrowthFactors');
    FtbsMinMaxFactors.Caption          :=  FAppModules.language.GetString('TabCaption.MinMaxGrowthFactors');
    FtbsHydrologyFactors.Caption       :=  FAppModules.language.GetString('TabCaption.HydrologyGrowthFactors');

    FstrgrdHydrologyFactors.Cells[0,0] :=  FAppModules.language.GetString('GridHeading.Institutions');
    FstrgrdHydrologyFactors.Cells[1,0] :=  FAppModules.language.GetString('GridHeading.Water Users');
    FstrgrdHydrologyFactors.Cells[2,0] :=  FAppModules.language.GetString('TField.MinMaxChannelNumber');;

    FstrgrdMinMaxFactors.Cells[0,0]    :=  FAppModules.language.GetString('GridHeading.Institutions');
    FstrgrdMinMaxFactors.Cells[1,0]    :=  FAppModules.language.GetString('GridHeading.Water Users');
    FstrgrdMinMaxFactors.Cells[2,0]    :=  FAppModules.language.GetString('TField.MinMaxChannelNumber');;


    FstrgrdDemandFactors.Cells[0,0]    :=  FAppModules.language.GetString('GridHeading.Institutions');
    FstrgrdDemandFactors.Cells[1,0]    :=  FAppModules.language.GetString('GridHeading.Water Users');
    FstrgrdDemandFactors.Cells[2,0]    :=  FAppModules.language.GetString('TField.MinMaxChannelNumber');;

    FlblGrowthFormula.Caption          :=  'Growth Factor = (Demand Projection / Base Year Demand) -1 ' + #10 +#13 ;
                                           // 'Base Year Demand = (Projection Average per year)*((60*60*24*364.25)/ 1000000)';

    FbtnGenerateProjections.Caption    := FAppModules.language.GetString('ButtonCaption.GenerateProjections');

    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TGrowthFactorsDialog.AssignHelpContext;
const OPNAME = 'TGrowthFactorsDialog.AssignHelpContext';
begin
  try
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TGrowthFactorsDialog.SaveState: boolean;
const OPNAME = 'TGrowthFactorsDialog.SaveState';
begin
  Result := inherited SaveState;
  try
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TGrowthFactorsDialog.StudyHasChanged: boolean;
const OPNAME = 'TGrowthFactorsDialog.StudyHasChanged';
begin
  Result := inherited StudyHasChanged;
  try
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TGrowthFactorsDialog.Resize;
const OPNAME = 'TGrowthFactorsDialog.Resize';
begin
  inherited Resize;
  try
    FgbEdits.Height                   := 40;
    FgbEdits.Align                    := alTop;

    FpgcProjections.Align             := alClient;

    FlblNumYears.Top                  := 15;
    FlblNumYears.Width                := 85;

    FlblStartYear.Top                 := 15;
    FlblEndYear.Top                   := 15;

    FedtNumYears.Top                  := 12;
    FedtNumYears.Width                := 35;

    FedtEndYear.Top                   := 12;
    FedtEndYear.Width                 := 40;

    FedtStartYear.Top                 := 12;
    FedtStartYear.Width               := 40;

    FlblGrowthFormula.Top            := 1;
    FlblGrowthFormula.AutoSize       := True;
    FlblGrowthFormula.Align          := alClient;
    FlblGrowthFormula.Alignment      := taCenter;

    FstrgrdDemandFactors.Align       := alClient;
    FstrgrdMinMaxFactors.Align       := alClient;
    FstrgrdHydrologyFactors.Align    := alClient;

    FtbsDemandFactors.PageControl    := FpgcProjections;
    FtbsMinMaxFactors.PageControl    := FpgcProjections;
    FtbsHydrologyFactors.PageControl := FpgcProjections;


    FlblNumYears.Left               := C_ControlOffset;
    FedtNumYears.Left               := FlblNumYears.Left  + FlblNumYears.Width  + C_LabelOffset;
    FlblStartYear.Left              := FedtNumYears.Left  + FedtNumYears.Width  + C_ControlOffset;
    FedtStartYear.Left              := FlblStartYear.Left + FlblStartYear.Width + C_LabelOffset;
    FlblEndYear.Left                := FedtStartYear.Left + FedtStartYear.Width + C_ControlOffset;
    FedtEndYear.Left                := FlblEndYear.Left   + FlblEndYear.Width   + C_LabelOffset;

    FbtnGenerateProjections.Top     := 10;
    FbtnGenerateProjections.Width   := 130;
    FbtnGenerateProjections.Left    :=  Self.ClientWidth - FbtnGenerateProjections.Width - 10;

    FFormulaPanel.Height            := 28;
    FFormulaPanel.Align             := alBottom;
    FFormulaPanel.Width             := Self.ClientWidth;

  except on E: Exception do HandleError(E, OPNAME) end;

end;

end.
