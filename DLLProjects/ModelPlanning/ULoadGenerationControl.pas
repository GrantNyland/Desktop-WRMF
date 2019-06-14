unit ULoadGenerationControl;

interface
uses
  Classes,
  UDataEditComponent,
  UAbstractObject,
  Vcl.Controls,
  Vcl.Samples.Spin,
  VCL.stdCtrls;
type
  TLoadGenerationControl = class(TGroupBox)
  protected
    FLoadGenerationTypeLabel : TLabel;
    FLoadGenerationTypeCombo : TComboBox;
    FStdDeviationLabel : TLabel;
    FStdDeviationEdit : TFieldEdit;
    FFactorAndMeanGrid : TFieldStringGrid;
    procedure InsertControl(AControl: TControl); reintroduce;
  public
    constructor Create(AOwner: TComponent; AAppModules: TAppModules); reintroduce; virtual;
    destructor Destroy; override;
    property LoadGenerationLabel : TLabel read  FLoadGenerationTypeLabel;
    property LoadGenerationCombo : TComboBox read FLoadGenerationTypeCombo;
    property StdDeviationLabel  : TLabel read FStdDeviationLabel;
    property StdDeviationEdit  : TFieldEdit read FStdDeviationEdit;
    property FactorAndMeanGrid : TFieldStringGrid read FFactorAndMeanGrid;
  end;

implementation
  uses
    SysUtils,
    UErrorHandlingOperations;
constructor TLoadGenerationControl.Create(AOwner: TComponent; AAppModules: TAppModules);
const OPNAME = 'TLoadGenerationControl.Create';
begin
  inherited Create(AOwner);
  try
    FLoadGenerationTypeLabel := TLabel.Create(AOwner);
    FLoadGenerationTypeLabel.Parent := Parent;
    FLoadGenerationTypeLabel.Top := 15;
    FLoadGenerationTypeLabel.Left := 10;
    FLoadGenerationTypeLabel.Width := 70;

    FLoadGenerationTypeCombo := TComboBox.Create(AOwner);
    FLoadGenerationTypeCombo.Parent := Parent;
     FLoadGenerationTypeCombo.Top := FLoadGenerationTypeLabel.Top;
    FLoadGenerationTypeCombo.Left := 210;
    FLoadGenerationTypeCombo.Width := 150;
    FLoadGenerationTypeCombo.Style := TComboBoxStyle.csDropDownList;


    FStdDeviationLabel := TLabel.Create(AOwner);
    FStdDeviationLabel.Parent := Parent;
    FStdDeviationLabel.Top := FLoadGenerationTypeLabel.Top + FLoadGenerationTypeLabel.Height + 10;
    FStdDeviationLabel.Left := 10;

    FStdDeviationEdit := TFieldEdit.Create(AOwner,AAppModules);
    FStdDeviationEdit.Parent := Parent;
    FStdDeviationEdit.Top := FStdDeviationLabel.Top;
    FStdDeviationEdit.Left := FLoadGenerationTypeCombo.Left;
    FStdDeviationEdit.Width := 50;

    FFactorAndMeanGrid := TFieldStringGrid.Create(AOwner,AAppModules);
    FFactorAndMeanGrid.Parent :=  Parent;
    FFactorAndMeanGrid.Top := FStdDeviationLabel.Top + FStdDeviationLabel.Height + 10;
    FFactorAndMeanGrid.Left := 10;
    FFactorAndMeanGrid.ColAutoSizeIgnoreHeading := true;
    FFactorAndMeanGrid.ColCount := 2;

    Self.Width := 450;
    Self.Height := FFactorAndMeanGrid.Top + FFactorAndMeanGrid.Height + 20;
    InsertControl(FLoadGenerationTypeLabel);
    InsertControl(FLoadGenerationTypeCombo);
    InsertControl(FStdDeviationLabel);
    InsertControl(FStdDeviationEdit);
    InsertControl(FFactorAndMeanGrid);

  except on E: Exception do HandleError(E,OPNAME); end;
end;

destructor TLoadGenerationControl.Destroy;
const OPNAME = 'TLoadGenerationControl.Destroy';
begin
   inherited;
  try
         //FreeAndNil(FFactorAndMeanGrid);
  except on E: Exception do HandleError(E,OPNAME); end;
end;

procedure TLoadGenerationControl.InsertControl(AControl: TControl);
const OPNAME = 'TLoadGenerationControl.InsertControl';
begin
  try
    inherited InsertControl(AControl);
  except on E: Exception do HandleError(E,OPNAME); end;
end;
end.
