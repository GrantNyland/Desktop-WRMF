unit ULoadingProgressDialog;

interface

uses
  // De[phi
  Controls, Forms, StdCtrls;

type
  TLoadingProgressDialog = class(TObject)
  private
    FMemo : TMemo;
  protected
    function CreateMemberObjects(AParent : TWinControl) : boolean;
    function DestroyMemberObjects : boolean;
  public
    destructor Destroy; override;
    constructor Create(AParent : TWinControl);
    procedure AddProgressString(AString : string);
  end;

implementation

uses
  // Delphi
  Sysutils,
  // DWAF
  UErrorHandlingOperations;

{ TLoadingProgressDialog }

procedure TLoadingProgressDialog.AddProgressString(AString: string);
const OPNAME = 'TLoadingProgressDialog.AddProgressString';
begin
  try
    if Assigned(FMemo) then
    begin
      FMemo.Lines.Add(AString);
      FMemo.Update;
    end;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

constructor TLoadingProgressDialog.Create(AParent : TWinControl);
const OPNAME = 'TLoadingProgressDialog.Create';
begin
  try
    CreateMemberObjects(AParent);
    inherited Create;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TLoadingProgressDialog.CreateMemberObjects(AParent : TWinControl): boolean;
const OPNAME = 'TLoadingProgressDialog.CreateMemberObjects';
begin
  Result := false;
  try
    FMemo := TMemo.Create(AParent);
    FMemo.Parent := AParent;
    FMemo.Align := alBottom;
    FMemo.Lines.Clear;
    FMemo.Readonly := true;
    if Assigned(AParent) then
    begin
      TForm(AParent).ClientHeight := TForm(AParent).ClientHeight + FMemo.Height;
    end;
    Result := true;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

destructor TLoadingProgressDialog.Destroy;
const OPNAME = 'TLoadingProgressDialog.Destroy';
begin
  try
    DestroyMemberObjects;
    inherited Destroy;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TLoadingProgressDialog.DestroyMemberObjects: boolean;
const OPNAME = 'TLoadingProgressDialog.DestroyMemberObjects';
begin
  Result := false;
  try
    FreeAndNil(FMemo);
    Result := true;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

end.
