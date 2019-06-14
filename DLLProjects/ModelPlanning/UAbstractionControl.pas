unit UAbstractionControl;

interface
uses
  Classes,
  UDataEditComponent,
  UAbstractObject,
  Vcl.Controls,
  Vcl.Samples.Spin,
  VCL.stdCtrls;

type TAbstractionControl = class(TGroupBox)
  protected
    FAbstractionLabel : TLabel;
    FAbstractionCheckBox : TFieldChkBox;
    FAbstractionGrid: TFieldStringGrid;
    FTimeSeriesLabel : TLabel;
    FTimeSeriesEdit : TFieldEdit;
    procedure InsertControl(AControl: TControl); reintroduce;
  public
    constructor Create(AOwner: TComponent; AAppModules: TAppModules); reintroduce; virtual;
    destructor Destroy; override;
    property AbstractionLabel : TLabel read FAbstractionLabel;
    property AbstractionCheckBox : TFieldChkBox read FAbstractionCheckBox;
    property AbstractionGrid : TFieldStringGrid read FAbstractionGrid;
    property TimeSeriesLabel : TLabel read FTimeSeriesLabel;
    property TimeSeriesEdit : TFieldEdit read FTimeSeriesEdit;
end;

implementation
uses
   SysUtils,
    UErrorHandlingOperations;

{ TAbstractionControl }

constructor TAbstractionControl.Create(AOwner: TComponent;
  AAppModules: TAppModules);
const OPNAME = 'TAbstractionControl.Create';
begin
  inherited Create(AOwner);
  try
    FAbstractionLabel := TLabel.Create(AOwner);
    FAbstractionLabel.Parent := Parent;
    FAbstractionLabel.Top := 15;
    FAbstractionLabel.Left := 10;
    FAbstractionCheckBox := TFieldChkBox.Create(AOwner,AAppModules);
    FAbstractionCheckBox.Parent := Parent;
    FAbstractionCheckBox.Top := FAbstractionLabel.Top;
    FAbstractionCheckBox.Left := 150;
    FAbstractionGrid := TFieldStringGrid.Create(AOwner,AAppModules);
    FAbstractionGrid.Parent := Parent;
    FAbstractionGrid.Top :=FAbstractionLabel.Top + FAbstractionLabel.Height + 10;
    FAbstractionGrid.Left := 10;
    FAbstractionGrid.ColCount := 2;
    FAbstractionGrid.AutoSizeFixedCols := true;
    FAbstractionGrid.Width := FAbstractionGrid.DefaultColWidth * FAbstractionGrid.ColCount + 10;
    FTimeSeriesLabel := TLabel.Create(AOwner);
    FTimeSeriesLabel.Parent := Parent;
    FTimeSeriesLabel.Top := FAbstractionGrid.Top + FAbstractionGrid.Height + 10;
    FTimeSeriesLabel.Left := 10;
    FTimeSeriesEdit :=  TFieldEdit.Create(AOwner,AAppModules);
    FTimeSeriesEdit.Parent :=  Parent;
    FTimeSeriesEdit.Top := FTimeSeriesLabel.Top;
    FTimeSeriesEdit.Left := FAbstractionCheckBox.Left;
    FTimeSeriesEdit.Width := FTimeSeriesEdit.Width - 20;
    InsertControl(FAbstractionLabel);
    InsertControl(FAbstractionCheckBox);
    InsertControl(FAbstractionGrid);
    InsertControl(FTimeSeriesLabel);
    InsertControl(FTimeSeriesEdit);
    Self.Width := 300;
    Self.Height := FTimeSeriesLabel.Top + FTimeSeriesLabel.Height + 10;
  except on E: Exception do HandleError(E,OPNAME);  end;

end;

destructor TAbstractionControl.Destroy;
const OPNAME = 'TAbstractionControl.Destroy';
begin

  inherited;
end;

procedure TAbstractionControl.InsertControl(AControl: TControl);
const OPNAME = 'TAbstractionControl.InsertControl';
begin
  try
  inherited  InsertControl(AControl);
  except on E: Exception do HandleError(E,OPNAME); end;
end;

end.
