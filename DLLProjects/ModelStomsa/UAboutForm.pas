unit UAboutForm;

interface

uses
  Windows, Messages, SysUtils, Classes, VCL.Graphics, VCL.Controls, VCL.Forms, VCL.Dialogs,
  VCL.StdCtrls, VCL.ExtCtrls, VCL.Imaging.jpeg, VCL.ComCtrls, UAbstractObject;

type
  TfmAbout = class(TFrame)
    btnClose: TButton;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    mmoApplicationDetails: TMemo;
    lblDescription: TLabel;
    imgAbout: TImage;
    rchReleaseNotes: TRichEdit;
  protected
    { Private declarations }
    FAppModules: TAppModules;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent; AAppModules: TAppModules); reintroduce; virtual;
  end;

var
  fmAbout: TfmAbout;

implementation

uses
  UErrorHandlingOperations;

{$R *.DFM}

constructor TfmAbout.Create(AOwner: TComponent;AAppModules: TAppModules);
const OPNAME = 'TfmAbout.Create';
begin
  inherited Create(AOwner);
  try
    FAppModules := AAppModules;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
