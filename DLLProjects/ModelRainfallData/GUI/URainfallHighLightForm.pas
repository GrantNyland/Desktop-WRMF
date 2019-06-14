{******************************************************************************}
{*  UNIT      : Contains the class TRainfallHighlightForm                     *}
{*  AUTHOR    : Riana Steyn                                                   *}
{*  DATE      : 2005/06/05                                                    *}
{*  COPYRIGHT : Copyright © 2005 DWAF                                         *}
{******************************************************************************}

unit URainfallHighlightForm;

interface                                              

uses
  Windows, Messages, SysUtils, Variants, Classes, VCL.Graphics, VCL.Controls, VCL.Forms,
  VCL.Dialogs, VCL.StdCtrls, VCL.ExtCtrls, VCL.ComCtrls, VCL.Grids,VCL.Buttons,
  UAbstractComponent,
  UAbstractObject,
  RainfallCom_TLB;

type
  TRainfallHighlightForm = class(TAbstractForm)
  private
    FScrollBox          : TScrollBox;
    FPnlButtons         : TAbstractPanel;
    FBtnOK              : TSpeedButton;
    FBtnCancel          : TSpeedButton;
    FPnlClient          : TAbstractPanel;

    FWetSeasonZerosChk            : TCheckBox;
    FWetSeasonGrid                : TStringGrid;
    FMonthlyGreaterProportionChk  : TCheckBox;
    FMonthlyGreaterProportionEdt  : TEdit;
    FMonthlyGreaterProportionLbl  : TLabel;
    FAnnualGreaterChk             : TCheckBox;
    FAnnualLessChk                : TCheckBox;
    FMonthlyGreaterAbsoluteChk    : TCheckBox;
    FMonthlyGreaterAbsoluteEdt    : TEdit;
    FRepeatingValuesChk           : TCheckBox;
    FRoundedValuesChk             : TCheckBox;

    procedure PopulateControls;
    function InputValid (var AHighlightWetSeasonZeros               : Boolean;
                         var AWetSeasonMonths                       : string;
                         var AHighlightMonthlyGreaterThanProportion : Boolean;
                         var AMonthlyGreaterThanProportionValue     : double;
                         var AHighlightAnnualGreaterThanAverage     : boolean;
                         var AHighlightAnnualLessThanAverage        : boolean;
                         var AHighlightMonthlyGreaterThanAbsolute   : Boolean;
                         var AMonthlyGreaterThanAbsoluteValue       : double;
                         var AHighlightRepeatingValues              : boolean;
                         var AHighlightRoundedValues                : boolean): Boolean;
    procedure DoProportionClicked (Sender : TObject);
    procedure DoAbsoluteClicked (Sender : TObject);
    procedure DoSelectGridCell (Sender        : TObject;
                                ACol          : longint;
                                ARow          : Longint;
                                var CanSelect : Boolean);
  protected
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    procedure FormClose (Sender     : TObject;
                         var Action : TCloseAction);
    procedure FormShow (Sender: TObject);
    procedure OnCancelClick(Sender: TObject);
    procedure OnOKClick(Sender: TObject);
  public
    function StudyDataHasChanged(AContext: TChangeContext;
                                 AFieldName,AOldValue,ANewValue: string): boolean; override;
    function LanguageHasChanged: boolean; override;
    function Initialise: boolean; override;

  end;

implementation

uses
  UDataSetType,
  UConstants,
  UDBConstants,
  UErrorHandlingOperations;

{******************************************************************************}
{* TRainfallHighlightForm                                                     *}
{******************************************************************************}

procedure TRainfallHighlightForm.CreateMemberObjects;
const OPNAME = 'TRainfallHighlightForm.CreateMemberObjects';
begin
  try
    Position := poScreenCenter;
    OnClose  := FormClose;
    OnShow   := FormShow;
    ClientHeight := 300;
    ClientWidth  := 490;

    FScrollBox := TScrollBox.Create(Self);
    with FScrollBox do
    begin
      Parent     := Self;
      Left       := 0;
      Top        := 0;
      Align      := alClient;
      BevelInner := bvNone;
      TabOrder   := 0;
    end;
    FPnlButtons := TAbstractPanel.Create(Self, FAppModules);
    with FPnlButtons do
    begin
      Parent     := FScrollBox;
      Left       := 0;
      Top        := 0;
      Height     := 30;
      Align      := alTop;
      BevelOuter := bvNone;
      TabOrder   := 0;
    end;
    FBtnOK := TSpeedButton.Create(FPnlButtons);
    FBtnOK.Glyph.LoadFromResourceName(HImagesInstance, UpperCase('CPOK'));
    with FBtnOK do
    begin
      Parent   := FPnlButtons;
      Left     := 0;
      Top      := 0;
      Width    := 30;
      Height   := 30;
      TabOrder := 0;
      ShowHint := TRUE;
      OnClick  := OnOKClick;
    end;
    FBtnCancel := TSpeedButton.Create(FPnlButtons);
    FBtnCancel.Glyph.LoadFromResourceName(HImagesInstance, UpperCase('CPCancel'));
    with FBtnCancel do
    begin
      Parent   := FPnlButtons;
      Left     := 30;
      Top      := 0;
      Width    := 30;
      Height   := 30;
      TabOrder := 1;
      ShowHint := TRUE;
      OnClick  := OnCancelClick;
    end;
    FPnlClient := TAbstractPanel.Create(Self, FAppModules);
    with FPnlClient do
    begin
      Parent     := FScrollBox;
      Left       := 0;
      Top        := 30;
      Width      := 420;
      Height     := 280;
      BevelOuter := bvNone;
      TabOrder   := 1;
    end;

    FWetSeasonZerosChk := TCheckBox.Create(Self);
    with FWetSeasonZerosChk do
    begin
      Parent     := FPnlClient;
      Left       := 10;
      Top        := 10;
      Width      := 450;
      Height     := 15;
      Alignment  := taRightJustify;
      Caption    := FAppModules.Language.GetString('Rainfall.HighlightZeros');
    end;

    FWetSeasonGrid := TStringGrid.Create(Self);
    with FWetSeasonGrid do
    begin
      Parent     := FPnlClient;
      Left       := 10;
      Top        := 35;
      FixedRows        := 1;
      FixedCols        := 0;
      DefaultRowHeight := 20;
      RowCount         := 2;
      DefaultColWidth  := 25;
      ColCount         := 12;
      Width            := 315;
      Height           := 45;
      Options          := [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine];
      OnSelectCell     := DoSelectGridCell;
    end;

    FMonthlyGreaterProportionChk  := TCheckBox.Create(Self);
    with FMonthlyGreaterProportionChk do
    begin
      Parent     := FPnlClient;
      Left       := 10;
      Top        := 90;
      Width      := 200;
      Height     := 15;
      Alignment  := taRightJustify;
      Caption    := FAppModules.Language.GetString('Rainfall.HighlightMonthly');
      OnClick    := DoProportionClicked;
    end;

    FMonthlyGreaterProportionEdt := TEdit.Create(Self);
    with FMonthlyGreaterProportionEdt do
    begin
      Parent     := FPnlClient;
      Left       := 210;
      Top        := 90;
      Width      := 40;
    end;

    FMonthlyGreaterProportionLbl := TLabel.Create(Self);
    with FMonthlyGreaterProportionLbl do
    begin
      Parent     := FPnlClient;
      Left       := 260;
      Top        := 90;
      Width      := 110;
      Caption    := FAppModules.Language.GetString('Rainfall.TimesMonthly');
    end;

    FAnnualGreaterChk := TCheckBox.Create(Self);
    with FAnnualGreaterChk do
    begin
      Parent     := FPnlClient;
      Left       := 10;
      Top        := 120;
      Width      := 450;
      Height     := 15;
      Alignment  := taRightJustify;
      Caption    := FAppModules.Language.GetString('Rainfall.HighlightAnnual');
    end;

    FAnnualLessChk  := TCheckBox.Create(Self);
    with FAnnualLessChk do
    begin
      Parent     := FPnlClient;
      Left       := 10;
      Top        := 150;
      Width      := 450;
      Height     := 15;
      Alignment  := taRightJustify;
      Caption    := FAppModules.Language.GetString('Rainfall.HighlightAnnualLess');
    end;

    FMonthlyGreaterAbsoluteChk := TCheckBox.Create(Self);
    with FMonthlyGreaterAbsoluteChk do
    begin
      Parent     := FPnlClient;
      Left       := 10;
      Top        := 180;
      Width      := 200;
      Height     := 15;
      Alignment  := taRightJustify;
      Caption    := FAppModules.Language.GetString('Rainfall.HighlightMonthlyGreater');
      OnClick    := DoAbsoluteClicked;
    end;

    FMonthlyGreaterAbsoluteEdt := TEdit.Create(Self);
    with FMonthlyGreaterAbsoluteEdt do
    begin
      Parent     := FPnlClient;
      Left       := 210;
      Top        := 180;
      Width      := 60;
    end;

    FRepeatingValuesChk := TCheckBox.Create(Self);
    with FRepeatingValuesChk do
    begin
      Parent     := FPnlClient;
      Left       := 10;
      Top        := 210;
      Width      := 450;
      Height     := 15;
      Alignment  := taRightJustify;
      Caption    := FAppModules.Language.GetString('Rainfall.HighlightValues');
    end;

    FRoundedValuesChk := TCheckBox.Create(Self);
    with FRoundedValuesChk do
    begin
      Parent     := FPnlClient;
      Left       := 10;
      Top        := 240;
      Width      := 450;
      Height     := 15;
      Alignment  := taRightJustify;
      Caption    := FAppModules.Language.GetString('Rainfall.HighlightRounded');
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TRainfallHighlightForm.Initialise: boolean;
const OPNAME = 'TRainfallHighlightForm.Initialise';
begin
  Result := FALSE;
  try
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRainfallHighlightForm.FormShow(Sender: TObject);
const OPNAME = 'TRainfallHighlightForm.FormShow';
begin
  try
    PopulateControls;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TRainfallHighlightForm.DestroyMemberObjects;
const OPNAME = 'TRainfallHighlightForm.DestroyMemberObjects';
begin
  try
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TRainfallHighlightForm.LanguageHasChanged: boolean;
const OPNAME = 'TRainfallHighlightForm.LanguageHasChanged';
begin
  Result := inherited LanguageHasChanged;
  try
    Caption          := FAppModules.Language.GetString('ButtonHint.RDHighLightOutliers');
    FBtnOK.Hint      := FAppModules.Language.GetString('ButtonHint.CPOK');
    FBtnCancel.Hint  := FAppModules.Language.GetString('ButtonHint.CPCancel');
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRainfallHighlightForm.StudyDataHasChanged (AContext: TChangeContext;
                                          AFieldName,AOldValue,ANewValue: string): boolean;
const OPNAME = 'TRainfallHighlightForm.StudyDataHasChanged';
begin
  Result := False;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRainfallHighlightForm.FormClose (Sender     : TObject;
                                          var Action : TCloseAction);
const OPNAME = 'TRainfallHighlightForm.FormClose';
begin
  try
    Action := caFree;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRainfallHighlightForm.PopulateControls;
const OPNAME = 'TRainfallHighlightForm.PopulateControls';
var
  LUpdateUser  : boolean;
  lRainfallObj : IRainfallModelData;
  lWetMonths   : string;
  lCol         : integer;
  lMonth       : integer;
begin
  try
    LUpdateUser := (FAppModules.User <> nil) and (FAppModules.User.UserRights in CUR_EditData);
    LUpdateUser := LUpdateUser and (FAppModules.StudyArea <> nil) and (not(FAppModules.StudyArea.ScenarioLocked));

    FBtnOK.Enabled     := LUpdateUser;
    FBtnCancel.Enabled := LUpdateUser;

    lRainfallObj := (FAppModules.Model.ModelData as IRainfallModelData);
    FWetSeasonZerosChk.Checked           := lRainfallObj.HighlightWetSeasonZeros;
    FMonthlyGreaterProportionChk.Checked := lRainfallObj.HighlightMonthlyGreaterThanProportion;
    FAnnualGreaterChk.Checked            := lRainfallObj.HighlightAnnualGreaterThanAverage;
    FAnnualLessChk.Checked               := lRainfallObj.HighlightAnnualLessThanAverage;
    FMonthlyGreaterAbsoluteChk.Checked   := lRainfallObj.HighlightMonthlyGreaterThanAbsolute;
    FRepeatingValuesChk.Checked          := lRainfallObj.HighlightRepeatingValues;
    FRoundedValuesChk.Checked            := lRainfallObj.HighlightRoundedValues;
    FMonthlyGreaterProportionEdt.Text    := FloatToStrF(lRainfallObj.MonthlyGreaterThanProportionValue, ffFixed, 10, 2);
    FMonthlyGreaterAbsoluteEdt.Text      := FloatToStrF(lRainfallObj.MonthlyGreaterThanAbsoluteValue, ffFixed, 10, 2);
    FMonthlyGreaterProportionEdt.Enabled := FMonthlyGreaterProportionChk.Checked;
    FMonthlyGreaterAbsoluteEdt.Enabled   := FMonthlyGreaterAbsoluteChk.Checked;
    lWetMonths                           := lRainfallObj.WetSeasonMonths;
    FWetSeasonGrid.Cells[0, 0] := FAppModules.Language.GetString('SeasonGrid.Oct');
    FWetSeasonGrid.Cells[1, 0] := FAppModules.Language.GetString('SeasonGrid.Nov');
    FWetSeasonGrid.Cells[2, 0] := FAppModules.Language.GetString('SeasonGrid.Dec');
    FWetSeasonGrid.Cells[3, 0] := FAppModules.Language.GetString('SeasonGrid.Jan');
    FWetSeasonGrid.Cells[4, 0] := FAppModules.Language.GetString('SeasonGrid.Feb');
    FWetSeasonGrid.Cells[5, 0] := FAppModules.Language.GetString('SeasonGrid.Mar');
    FWetSeasonGrid.Cells[6, 0] := FAppModules.Language.GetString('SeasonGrid.Apr');
    FWetSeasonGrid.Cells[7, 0] := FAppModules.Language.GetString('SeasonGrid.May');
    FWetSeasonGrid.Cells[8, 0] := FAppModules.Language.GetString('SeasonGrid.Jun');
    FWetSeasonGrid.Cells[9, 0] := FAppModules.Language.GetString('SeasonGrid.Jul');
    FWetSeasonGrid.Cells[10, 0] := FAppModules.Language.GetString('SeasonGrid.Aug');
    FWetSeasonGrid.Cells[11, 0] := FAppModules.Language.GetString('SeasonGrid.Sep');

    for lCol := 0 to 11 do
    begin
      if (lCol < 3) then
        lMonth := lCol + 10
      else
        lMonth := lCol - 2;
      if (Pos(IntToStr(lMonth), lWetMonths) > 0) then
        FWetSeasonGrid.Cells[lCol, 1] := 'X'
      else
        FWetSeasonGrid.Cells[lCol, 1] := '';
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRainfallHighlightForm.DoSelectGridCell (Sender        : TObject;
                                                   ACol          : longint;
                                                   ARow          : Longint;
                                                   var CanSelect : Boolean);
const OPNAME = 'TRainfallHighlightForm.DoSelectGridCell';
begin
  try
    if (FWetSeasonGrid.Cells[ACol, ARow] = 'X') then
      FWetSeasonGrid.Cells[ACol, ARow] := ''
    else
      FWetSeasonGrid.Cells[ACol, ARow] := 'X';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRainfallHighlightForm.DoProportionClicked (Sender : TObject);
const OPNAME = 'TRainfallHighlightForm.DoProportionClicked';
begin
  try
    FMonthlyGreaterProportionEdt.Enabled := FMonthlyGreaterProportionChk.Checked;
    if (NOT FMonthlyGreaterProportionEdt.Enabled) then
      FMonthlyGreaterProportionEdt.Text :=
        FloatToStrF((FAppModules.Model.ModelData as IRainfallModelData).MonthlyGreaterThanProportionValue, ffFixed, 10, 2);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRainfallHighlightForm.DoAbsoluteClicked (Sender : TObject);
const OPNAME = 'TRainfallHighlightForm.DoAbsoluteClicked';
begin
  try
    FMonthlyGreaterAbsoluteEdt.Enabled := FMonthlyGreaterAbsoluteChk.Checked;
    if (NOT FMonthlyGreaterAbsoluteEdt.Enabled) then
      FMonthlyGreaterAbsoluteEdt.Text :=
        FloatToStrF((FAppModules.Model.ModelData as IRainfallModelData).MonthlyGreaterThanAbsoluteValue, ffFixed, 10, 2);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRainfallHighlightForm.InputValid (var AHighlightWetSeasonZeros               : Boolean;
                                            var AWetSeasonMonths                       : string;
                                            var AHighlightMonthlyGreaterThanProportion : Boolean;
                                            var AMonthlyGreaterThanProportionValue     : double;
                                            var AHighlightAnnualGreaterThanAverage     : boolean;
                                            var AHighlightAnnualLessThanAverage        : boolean;
                                            var AHighlightMonthlyGreaterThanAbsolute   : Boolean;
                                            var AMonthlyGreaterThanAbsoluteValue       : double;
                                            var AHighlightRepeatingValues              : boolean;
                                            var AHighlightRoundedValues                : boolean): Boolean;
const OPNAME = 'TRainfallHighlightForm.InputValid';
var
  lIndex       : integer;
  lMonth       : integer;
begin
  Result := FALSE;
  try
    Result   := TRUE;

    AHighlightWetSeasonZeros := FWetSeasonZerosChk.Checked;
    AWetSeasonMonths         := '';
    for lIndex := 0 to 11 do
    begin
      if (lIndex < 3) then
        lMonth := lIndex + 10
      else
        lMonth := lIndex - 2;
      if (FWetSeasonGrid.Cells[lIndex, 1] = 'X') then
      begin
        if (AWetSeasonMonths = '') then
          AWetSeasonMonths := IntToStr(lMonth)
        else
          AWetSeasonMonths := AWetSeasonMonths + ',' + IntToStr(lMonth);
      end;
    end;
    if (AHighlightWetSeasonZeros AND (AWetSeasonMonths = '')) then
    begin
      Result := FALSE;
      ShowMessage(FAppModules.Language.GetString('Rainfall.SpecifyMonthsWetSeason'));
    end;
    if (Result) then
    begin
      AHighlightMonthlyGreaterThanProportion := FMonthlyGreaterProportionChk.Checked;
      if (AHighlightMonthlyGreaterThanProportion) then
      begin
        try
          AMonthlyGreaterThanProportionValue := StrToFloat(Trim(FMonthlyGreaterProportionEdt.Text));
        except
          Result := FALSE;
          ShowMessage(FAppModules.Language.GetString('Message.InvalidInput'));
          FMonthlyGreaterProportionEdt.SetFocus;
        end;
      end;
    end;
    if (Result) then
    begin
      AHighlightAnnualGreaterThanAverage     := FAnnualGreaterChk.Checked;
      AHighlightAnnualLessThanAverage        := FAnnualLessChk.Checked;
      AHighlightMonthlyGreaterThanAbsolute   := FMonthlyGreaterAbsoluteChk.Checked;
      if (AHighlightMonthlyGreaterThanAbsolute) then
      begin
        try
          AMonthlyGreaterThanAbsoluteValue := StrToFloat(Trim(FMonthlyGreaterAbsoluteEdt.Text));
        except
          Result := FALSE;
          ShowMessage(FAppModules.Language.GetString('Message.InvalidInput'));
          FMonthlyGreaterAbsoluteEdt.SetFocus;
        end;
      end;
    end;
    AHighlightRepeatingValues := FRepeatingValuesChk.Checked;
    AHighlightRoundedValues   := FRoundedValuesChk.Checked;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRainfallHighlightForm.OnOKClick(Sender: TObject);
const OPNAME = 'TRainfallHighlightForm.OnOKClick';
var
  lRainfallObj                           : IRainfallModelData;
  lHighlightWetSeasonZeros               : Boolean;
  lWetSeasonMonths                       : string;
  lHighlightMonthlyGreaterThanProportion : Boolean;
  lMonthlyGreaterThanProportionValue     : double;
  lHighlightAnnualGreaterThanAverage     : boolean;
  lHighlightAnnualLessThanAverage        : boolean;
  lHighlightMonthlyGreaterThanAbsolute   : Boolean;
  lMonthlyGreaterThanAbsoluteValue       : double;
  lHighlightRepeatingValues              : boolean;
  lHighlightRoundedValues                : boolean;
begin
  try
    lRainfallObj := (FAppModules.Model.ModelData as IRainfallModelData);
    lHighlightWetSeasonZeros               := lRainfallObj.HighlightWetSeasonZeros;
    lWetSeasonMonths                       := lRainfallObj.WetSeasonMonths;
    lHighlightMonthlyGreaterThanProportion := lRainfallObj.HighlightMonthlyGreaterThanProportion;
    lMonthlyGreaterThanProportionValue     := lRainfallObj.MonthlyGreaterThanProportionValue;
    lHighlightAnnualGreaterThanAverage     := lRainfallObj.HighlightAnnualGreaterThanAverage;
    lHighlightAnnualLessThanAverage        := lRainfallObj.HighlightAnnualLessThanAverage;
    lHighlightMonthlyGreaterThanAbsolute   := lRainfallObj.HighlightMonthlyGreaterThanAbsolute;
    lMonthlyGreaterThanAbsoluteValue       := lRainfallObj.MonthlyGreaterThanAbsoluteValue;
    lHighlightRepeatingValues              := lRainfallObj.HighlightRepeatingValues;
    lHighlightRoundedValues                := lRainfallObj.HighlightRoundedValues;

    if (InputValid(lHighlightWetSeasonZeros,
                   lWetSeasonMonths,
                   lHighlightMonthlyGreaterThanProportion,
                   lMonthlyGreaterThanProportionValue,
                   lHighlightAnnualGreaterThanAverage,
                   lHighlightAnnualLessThanAverage,
                   lHighlightMonthlyGreaterThanAbsolute,
                   lMonthlyGreaterThanAbsoluteValue,
                   lHighlightRepeatingValues,
                   lHighlightRoundedValues)) then
    begin
      lRainfallObj.HighlightWetSeasonZeros               := lHighlightWetSeasonZeros;
      lRainfallObj.WetSeasonMonths                       := lWetSeasonMonths;
      lRainfallObj.HighlightMonthlyGreaterThanProportion := lHighlightMonthlyGreaterThanProportion;
      lRainfallObj.MonthlyGreaterThanProportionValue     := lMonthlyGreaterThanProportionValue;
      lRainfallObj.HighlightAnnualGreaterThanAverage     := lHighlightAnnualGreaterThanAverage;
      lRainfallObj.HighlightAnnualLessThanAverage        := lHighlightAnnualLessThanAverage;
      lRainfallObj.HighlightMonthlyGreaterThanAbsolute   := lHighlightMonthlyGreaterThanAbsolute;
      lRainfallObj.MonthlyGreaterThanAbsoluteValue       := lMonthlyGreaterThanAbsoluteValue;
      lRainfallObj.HighlightRepeatingValues              := lHighlightRepeatingValues;
      lRainfallObj.HighlightRoundedValues                := lHighlightRoundedValues;

      ModalResult := mrOk;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRainfallHighlightForm.OnCancelClick(Sender: TObject);
const OPNAME = 'TRainfallHighlightForm.OnCancelClick';
begin
  try
    ModalResult := mrCancel;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
