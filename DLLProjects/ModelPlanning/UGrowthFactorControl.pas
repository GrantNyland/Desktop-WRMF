unit UGrowthFactorControl;

interface
uses
  Classes,
  UDataEditComponent,
  UAbstractObject,
  Vcl.Controls,
  Vcl.Samples.Spin,
  VCL.stdCtrls;

type

  TGrowthFactorControl = class(TGroupBox)
  protected
    //FAppModules: TAppModules;
    FFactorTypeLabel : TLabel;
    FFactorTypeCombo : TFieldComboBox;
    FNoofPointsLabel : TLabel;
    FNoOfPointsSpinEdit : TSpinEdit;
    FInterpolationLabel, FLinearInterpolationLabel, FExponentialInterpolation :  Tlabel;
    FInterpolotionCheckBox : TFieldChkBox;
    FYearAndFactorGrid: TFieldStringGrid;
    procedure InsertControl(AControl: TControl); reintroduce;
  public
    constructor Create(AOwner: TComponent; AAppModules: TAppModules); reintroduce; virtual;
    destructor Destroy; override;
    property FactorTypeLabel: TLabel read FFactorTypeLabel;
    property FactorTypeCombo: TFieldComboBox read FFactorTypeCombo;
    property NoofPointsLabel: TLabel read  FNoofPointsLabel;
    property NoOfPointsSpinEdit: TSpinEdit read FNoOfPointsSpinEdit;
    property InterpolationLabel: TLabel read FInterpolationLabel;
    property LinearInterpolationLabel: TLabel read FLinearInterpolationLabel;
    property ExponentialInterpolationLabel: TLabel read FExponentialInterpolation;
    property InterpolationCheckBox: TFieldChkBox read FInterpolotionCheckBox;
    property YearAndFactorGrid: TFieldStringGrid read FYearAndFactorGrid;

  end;

implementation
uses
  SysUtils,
  UErrorHandlingOperations;

{ TGrowthFactorControl }



constructor TGrowthFactorControl.Create(AOwner: TComponent;
  AAppModules: TAppModules);
const OPNAME = 'TGrowthFactorControl.Create';
begin
  inherited Create(AOwner);
  try
    //FAppModules := AAppModules;
    FFactorTypeLabel := TLabel.Create(AOwner);
    FFactorTypeLabel.Parent := Parent;
    FFactorTypeLabel.Left := 10;
    FFactorTypeLabel.Top := 15;
    FFactorTypeCombo := TFieldComboBox.Create(AOwner,AAppModules);
    FFactorTypeCombo.Parent := Parent;
    FFactorTypeCombo.Top := FFactorTypeLabel.Top;
    FFactorTypeCombo.Left := 210;
    FFactorTypeCombo.Width := 150;
    FFactorTypeCombo.Style := TComboBoxStyle.csDropDownList;
    FNoofPointsLabel := TLabel.Create(AOwner);
    FNoofPointsLabel.Parent := Parent;
    FNoofPointsLabel.Top := FFactorTypeLabel.Top + FFactorTypeLabel.Height + 10;
    FNoofPointsLabel.Left := FactorTypeLabel.Left;
    FNoOfPointsSpinEdit := TSpinEdit.Create(AOwner);
    FNoOfPointsSpinEdit.Parent := Parent;
    FNoOfPointsSpinEdit.Top := FNoofPointsLabel.Top;
    FNoOfPointsSpinEdit.Left := FFactorTypeCombo.Left;
    FNoOfPointsSpinEdit.Width := 50;
    FInterpolationLabel := TLabel.Create(AOwner);
    FInterpolationLabel.Parent := Parent;
    FInterpolationLabel.Top := FNoofPointsLabel.Top + FNoofPointsLabel.Height + 10;
    FInterpolationLabel.Left := FFactorTypeLabel.Left;
    FInterpolotionCheckBox := TFieldChkBox.Create(AOwner,AAppModules);
    FInterpolotionCheckBox.Parent := Parent;
    FInterpolotionCheckBox.Top := FInterpolationLabel.Top;
    FInterpolotionCheckBox.Left := FFactorTypeCombo.Left;
    FLinearInterpolationLabel := TLabel.Create(AOwner);
    FLinearInterpolationLabel.Parent := Parent;
    FLinearInterpolationLabel.Top := FInterpolationLabel.Top;
    FLinearInterpolationLabel.Left := FInterpolotionCheckBox.Left + FInterpolotionCheckBox.Width;

    FExponentialInterpolation := TLabel.Create(AOwner);
    FExponentialInterpolation.Parent := Parent;
    FExponentialInterpolation.Top := FInterpolationLabel.Top + FInterpolationLabel.Height + 10;
    FExponentialInterpolation.Left := FLinearInterpolationLabel.Left;
    FYearAndFactorGrid := TFieldStringGrid.Create(AOwner,AAppModules);
    FYearAndFactorGrid.Parent := Parent;
    FYearAndFactorGrid.Top := FExponentialInterpolation.Top + FExponentialInterpolation.Height + 10;
    FYearAndFactorGrid.Left := 10;
    FYearAndFactorGrid.Width := FLinearInterpolationLabel.Left + FLinearInterpolationLabel.Width - FNoofPointsLabel.Left;
    FYearAndFactorGrid.ColCount := 2;
    Self.Width := FExponentialInterpolation.Left+ FExponentialInterpolation.Width + 120;
    Self.Height := FYearAndFactorGrid.Top + FYearAndFactorGrid.Height + 10;
    InsertControl(FFactorTypeLabel);
    InsertControl(FFactorTypeCombo);
    InsertControl(FNoofPointsLabel);
    InsertControl(FNoOfPointsSpinEdit);
    InsertControl(FInterpolationLabel);
    InsertControl(FLinearInterpolationLabel);
    InsertControl(FExponentialInterpolation);
    InsertControl(FInterpolotionCheckBox);
    InsertControl(FYearAndFactorGrid);
  except on E: Exception do HandleError(E, OPNAME); end;
end;


destructor TGrowthFactorControl.Destroy;
const OPNAME = 'TGrowthFactorControl.Destroy';
begin
  inherited;
  try

  except on E: Exception do HandleError(E,OPNAME); end;
end;

procedure TGrowthFactorControl.InsertControl(AControl: TControl);
const OPNAME = 'TGrowthFactorControl.InsertControl';
begin
  try
    inherited InsertControl(AControl);
  except on E: Exception do HandleError(E,OPNAME); end;
end;

end.
