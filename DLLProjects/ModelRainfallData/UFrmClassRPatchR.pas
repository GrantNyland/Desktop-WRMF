unit UFrmClassRPatchR;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, VCL.Graphics, VCL.Controls, VCL.Forms,
  VCL.Dialogs,

  URainFallFortranDLL, VCL.ExtCtrls, VCL.StdCtrls;

type
  TFrmClassRPatchR = class(TForm)
    PnlTop    : TPanel;
    ScrClient : TScrollBox;
    MmoText    : TMemo;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
  protected
    FFortranDLL : TRainFallFortranDLL;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Initialise;
    procedure RunClassR;
    procedure RunPatchR;
  end;

var
  FrmClassRPatchR : TFrmClassRPatchR;

implementation

uses
  UErrorHandlingOperations;
  
{$R *.dfm}

constructor TFrmClassRPatchR.Create(AOwner: TComponent);
const OPNAME = 'TFrmClassRPatchR.Create';
begin
  try
    inherited Create(AOwner);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

destructor TFrmClassRPatchR.Destroy;
const OPNAME = 'TFrmClassRPatchR.Destroy';
begin
  try
    FreeAndNil(FFortranDLL);
    inherited Destroy;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TFrmClassRPatchR.Initialise;
const OPNAME = 'TFrmClassRPatchR.Initialise';
begin
  try
    FFortranDLL := TRainFallFortranDLL.Create;
    FFortranDLL.LoadRainFallFortranDLL(FALSE);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TFrmClassRPatchR.RunClassR;
const OPNAME = 'TFrmClassRPatchR.RunClassR';
begin
  try
    FFortranDLL.DoFortranRunClassR;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TFrmClassRPatchR.RunPatchR;
const OPNAME = 'TFrmClassRPatchR.RunPatchR';
begin
  try
    FFortranDLL.DoFortranRunPatchR;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TFrmClassRPatchR.FormClose(Sender: TObject;
  var Action: TCloseAction);
const OPNAME = 'TFrmClassRPatchR.FormClose';
begin
  try
    Action := caFree;
    FrmClassRPatchR := nil;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

end.
