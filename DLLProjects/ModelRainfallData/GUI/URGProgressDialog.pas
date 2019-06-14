unit URGProgressDialog;

interface

uses
  VCL.Forms,
  VCL.ComCtrls,
  VCL.StdCtrls;

type
  TRGProgressDialog = class(TObject)
  private
    FForm : TForm;
    FProgress : TProgressBar;
    FLabel : TLabel;
  protected
    procedure CreateMemberObjects;
    procedure DestroyMemberObjects;
    procedure Initialise;
    function GetPosition : integer;
    procedure SetPosition(AValue : integer);
    function GetMaxProgress : integer;
    procedure SetMaxProgress(AValue : integer);
  public
    constructor Create;
    destructor Destroy; override;
    procedure ShowLabel;
    procedure ShowProgressBar;
    procedure ShowForm;
    procedure HideForm;
    function SetLabelCaption(AValue : string) : boolean;
    property Position : integer    read GetPosition     write SetPosition;
    property MaxProgress : integer read GetMaxProgress  write SetMaxProgress;
  end;

implementation

uses
  // Deplhi
  Math,
  Sysutils,
  // DWAF - PDNA
  UErrorHandlingOperations;

{ TRGProgressDialog }

constructor TRGProgressDialog.Create;
const OPNAME = 'TRGProgressDialog.Create';
begin
  try
    inherited create;
    CreateMemberObjects;
    Initialise;
  except on E:Exception do HandleError(E,OPNAME) end;
end;

procedure TRGProgressDialog.CreateMemberObjects;
const OPNAME = 'TRGProgressDialog.CreateMemberObjects';
begin
  try
    FForm := TForm.Create(nil);
    FProgress := TProgressBar.Create(FForm);
    FLabel := TLabel.Create(FForm);
  except on E:Exception do HandleError(E,OPNAME) end;
end;

destructor TRGProgressDialog.Destroy;
const OPNAME = 'TRGProgressDialog.Destroy';
begin
  try
    DestroyMemberObjects;
    inherited Destroy;
  except on E:Exception do HandleError(E,OPNAME) end;
end;

procedure TRGProgressDialog.DestroyMemberObjects;
const OPNAME = 'TRGProgressDialog.DestroyMemberObjects';
begin
  try
    if Assigned(FForm) then
    begin
      FreeAndNil(FLabel);
      FreeAndNil(FProgress);
      FForm.Release;
    end;
  except on E:Exception do HandleError(E,OPNAME) end;
end;

function TRGProgressDialog.GetMaxProgress: integer;
const OPNAME = 'TRGProgressDialog.GetMaxProgress';
begin
  Result := 0;
  try
    if Assigned(FProgress) then
      Result := FProgress.Max;
  except on E:Exception do HandleError(E,OPNAME) end;
end;

function TRGProgressDialog.GetPosition: integer;
const OPNAME = 'TRGProgressDialog.GetPosition';
begin
  Result := 0;
  try
    if Assigned(FProgress) then
      Result := FProgress.Position;
  except on E:Exception do HandleError(E,OPNAME) end;
end;

procedure TRGProgressDialog.HideForm;
const OPNAME = 'TRGProgressDialog.HideForm';
begin
  try
    if Assigned(FForm) then
      FForm.Hide;
  except on E:Exception do HandleError(E,OPNAME) end;
end;

procedure TRGProgressDialog.Initialise;
const OPNAME = 'TRGProgressDialog.Initialise';
begin
  try
    FForm.FormStyle := fsStayOnTop;
    FForm.BorderStyle := bsToolWindow;
    FForm.ClientWidth := 200;
    FForm.ClientHeight := 40;
    FForm.Position := poDesktopCenter;
    FForm.Caption := Application.Title;

    FProgress.Parent := FForm;
    FProgress.Width := 180; 
    FProgress.Height := 20;
    FProgress.Top := 10;
    FProgress.Left := 10;
    FProgress.Visible := false;

    FLabel.Parent := FForm;
    FLabel.Top := 10;
    FLabel.Left := 25;
    FLabel.Width := 100;
    FLabel.Height := 21;

    FLabel.Visible := true;
    FLabel.Autosize := true;
    FLabel.Caption := 'Please wait while the GIS Viewer Initialises.';//FAppModules.Language.GetString('Rainfall.WaitWhileGISViewerInitialises');
  except on E:Exception do HandleError(E,OPNAME) end;
end;

function TRGProgressDialog.SetLabelCaption(AValue: string): boolean;
const OPNAME = 'TRGProgressDialog.SetLabelCaption';
begin
  Result := false;
  try
    if Assigned(FLabel) then
    begin
      FLabel.Caption := AValue;
      FLabel.Update;
      FForm.Width := Max(FForm.Width, FLabel.Left + FLabel.Width + 10);
      Result := true;
    end;
  except on E:Exception do HandleError(E,OPNAME) end;
end;

procedure TRGProgressDialog.SetMaxProgress(AValue: integer);
const OPNAME = 'TRGProgressDialog.SetMaxProgress';
begin
  try
    if Assigned(FProgress) then
      FProgress.Max := AValue;
  except on E:Exception do HandleError(E,OPNAME) end;
end;

procedure TRGProgressDialog.SetPosition(AValue: integer);
const OPNAME = 'TRGProgressDialog.SetPosition';
begin
  try
    if Assigned(FProgress) then
    begin
      FProgress.Position := AValue;
      FProgress.Update;
    end;
  except on E:Exception do HandleError(E,OPNAME) end;
end;

procedure TRGProgressDialog.ShowForm;
const OPNAME = 'TRGProgressDialog.ShowForm';
begin
  try
    if Assigned(FForm) then
      FForm.Show;
  except on E:Exception do HandleError(E,OPNAME) end;
end;

procedure TRGProgressDialog.ShowLabel;
const OPNAME = 'TRGProgressDialog.ShowLabel';
begin
  try
    if Assigned(FLabel) then
      FLabel.Visible := true;
    if Assigned(FProgress) then
      FProgress.Visible := false;
  except on E:Exception do HandleError(E,OPNAME) end;
end;

procedure TRGProgressDialog.ShowProgressBar;
const OPNAME = 'TRGProgressDialog.ShowProgressBar';
begin
  try
    if Assigned(FLabel) then
      FLabel.Visible := false;
    if Assigned(FProgress) then
      FProgress.Visible := true;
  except on E:Exception do HandleError(E,OPNAME) end;
end;

end.




