unit UDailyDivesionProgressBar;

interface

uses
  VCL.Forms,
  VCL.ComCtrls,
  VCL.StdCtrls,
  UAbstractComponent;

type
  TDailyDivesionProgressBar = class(TAbstractForm)
  protected
    FProgress : TProgressBar;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    function GetPosition : integer;
    procedure SetPosition(AValue : integer);
    function GetMaxProgress : integer;
    procedure SetMaxProgress(AValue : integer);
  public
    function Initialise : boolean; override;
    procedure ShowForm;
    property ProgressPosition : integer    read GetPosition     write SetPosition;
    property MaxProgress : integer read GetMaxProgress  write SetMaxProgress;
  end;

implementation

uses
  // Deplhi
  Math,
  Sysutils,
  // DWAF
  UErrorHandlingOperations;

{ TDailyDivesionProgressBar }

procedure TDailyDivesionProgressBar.CreateMemberObjects;
const OPNAME = 'TDailyDivesionProgressBar.CreateMemberObjects';
begin
  inherited CreateMemberObjects;
  try
    FProgress := TProgressBar.Create(self);
   except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TDailyDivesionProgressBar.DestroyMemberObjects;
const OPNAME = 'TDailyDivesionProgressBar.DestroyMemberObjects';
begin
  try
    inherited DestroyMemberObjects;
   except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDailyDivesionProgressBar.GetMaxProgress: integer;
const OPNAME = 'TDailyDivesionProgressBar.GetMaxProgress';
begin
  Result := 0;
  try
    if Assigned(FProgress) then
      Result := FProgress.Max;
  except on E:Exception do HandleError(E,OPNAME) end;
end;

function TDailyDivesionProgressBar.GetPosition: integer;
const OPNAME = 'TDailyDivesionProgressBar.GetPosition';
begin
  Result := 0;
  try
    if Assigned(FProgress) then
      Result := FProgress.Position;
  except on E:Exception do HandleError(E,OPNAME) end;
end;

function TDailyDivesionProgressBar.Initialise : boolean;
const OPNAME = 'TDailyDivesionProgressBar.Initialise';
begin
  Result := False;
  try
    FormStyle := fsStayOnTop;
    BorderStyle := bsToolWindow;
    Width := 400;
    Height := 70;
    Position := poDesktopCenter;
    Caption := 'Calculating and Populating Monthly Values From Daily data...';

    FProgress.Parent := Self;
    FProgress.Width := 380;
    FProgress.Height := 20;
    FProgress.Top := 10;
    FProgress.Left := 10;
    FProgress.Visible := True;
    Result := True;
  except on E:Exception do HandleError(E,OPNAME) end;
end;

procedure TDailyDivesionProgressBar.SetMaxProgress(AValue: integer);
const OPNAME = 'TDailyDivesionProgressBar.SetMaxProgress';
begin
  try
    if Assigned(FProgress) then
      FProgress.Max := AValue;
  except on E:Exception do HandleError(E,OPNAME) end;
end;

procedure TDailyDivesionProgressBar.SetPosition(AValue: integer);
const OPNAME = 'TDailyDivesionProgressBar.SetPosition';
begin
  try
    if Assigned(FProgress) then
    begin
      FProgress.Position := AValue;
      FProgress.Update;
    end;
  except on E:Exception do HandleError(E,OPNAME) end;
end;

procedure TDailyDivesionProgressBar.ShowForm;
const OPNAME = 'TDailyDivesionProgressBar.ShowForm';
begin
  try
    Show;
  except on E:Exception do HandleError(E,OPNAME) end;
end;

end.




