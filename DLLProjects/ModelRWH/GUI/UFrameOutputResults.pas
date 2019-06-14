unit UFrameOutputResults;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Variants,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.ComCtrls,
  Vcl.Grids,
  Vcl.StdCtrls,
  Vcl.ExtCtrls,
  UColourButtons,
  UAbstractObject,
  URWHDataObject;
type
  TfrmOutputResults = class(TFrame)
    PageControl1: TPageControl;
    tsSummary: TTabSheet;
    tsAnnual: TTabSheet;
    tsMonthly: TTabSheet;
    tsDaily: TTabSheet;
    tsViewFile: TTabSheet;
  private
    { Private declarations }
  public
    { Public declarations }
    procedure AfterConstruction; override;
    function Initialise : boolean;
    function AfterInitialise : boolean;
    function Finalise   : boolean;
    function LanguageHasChanged : boolean;
  end;

var
  frmOutputResults : TfrmOutputResults;

implementation

{$R *.dfm}

uses
  UUtilities,
  UConstants,
  UFrameOutputViewFile,
  UFrameOutputSummary,
  UFrameOutputAnnual,
  UFrameOutputMonthly,
  UFrameOutputDaily,
  UErrorHandlingOperations;

{ TfrmInputConfiguration }

procedure TfrmOutputResults.AfterConstruction;
const OPNAME = 'TfrmInputHydrology.AfterConstruction';
begin
  inherited;
  try
    frmOutputViewFile := TfrmOutputViewFile.Create(Self);
    frmOutputViewFile.Parent := tsViewFile;

    frmOutputSummary := TfrmOutputSummary.Create(Self);
    frmOutputSummary.Parent := tsSummary;

    frmOutputAnnual := TfrmOutputAnnual.Create(Self);
    frmOutputAnnual.Parent := tsAnnual;

    frmOutputMonthly := TfrmOutputMonthly.Create(Self);
    frmOutputMonthly.Parent := tsMonthly;

    frmOutputDaily := TfrmOutputDaily.Create(Self);
    frmOutputDaily.Parent := tsDaily;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TfrmOutputResults.AfterInitialise: boolean;
const OPNAME = 'TfrmInputHydrology.AfterInitialise';
begin
  Result := False;
  try
    frmOutputViewFile.AfterInitialise;
    frmOutputSummary.AfterInitialise;
    frmOutputAnnual.AfterInitialise;
    frmOutputMonthly.AfterInitialise;
    frmOutputDaily.AfterInitialise;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TfrmOutputResults.Finalise: boolean;
const OPNAME = 'TfrmInputHydrology.Finalise';
begin
  Result := False;
  try
    frmOutputViewFile.Finalise;
    frmOutputSummary.Finalise;
    frmOutputAnnual.Finalise;
    frmOutputMonthly.Finalise;
    frmOutputDaily.Finalise;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TfrmOutputResults.Initialise: boolean;
const OPNAME = 'TfrmInputHydrology.Initialise';
begin
  Result := False;
  try
    frmOutputViewFile.Initialise;
    frmOutputSummary.Initialise;
    frmOutputAnnual.Initialise;
    frmOutputMonthly.Initialise;
    frmOutputDaily.Initialise;
    tsViewFile.TabVisible := True;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TfrmOutputResults.LanguageHasChanged: boolean;
const OPNAME = 'TfrmInputHydrology.LanguageHasChanged';
begin
  Result := False;
  try
    frmOutputViewFile.LanguageHasChanged;
    frmOutputSummary.LanguageHasChanged;
    frmOutputAnnual.LanguageHasChanged;
    frmOutputMonthly.LanguageHasChanged;
    frmOutputDaily.LanguageHasChanged;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
