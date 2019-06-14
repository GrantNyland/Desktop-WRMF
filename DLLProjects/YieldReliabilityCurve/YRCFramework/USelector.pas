//
//
//  UNIT      : Contains TPlaneSelector Classes
//  AUTHOR    : Dziedzi Ramulondi(PDNA)
//  DATE      : 25/09/2002
//  COPYRIGHT : Copyright © 2002 DWAF
//
//
unit USelector;

interface
uses
  Classes,
  Controls,
  Forms,
  ExtCtrls,
  StdCtrls,
  Spin,
  UAbstractComponent;

type
  TSelectionChange = procedure(ASelection: integer) of object;
  TSelector = class(TAbstractPanel)
  protected
    FName: TLabel;
    FSelector: TSpinEdit;
    FSelectionChange: TSelectionChange;
    procedure CreateMemberObjects; override;
    procedure OnSelectorChanged(Sender: TObject);
    procedure CenterControls;
  public
    // Overriden from Delphi.
    procedure Resize; override;
    procedure SetCaption(ACaption: string);
    procedure Select(AIndex: integer);
    function Populate(AMinValue,AMaxValue: integer): boolean;
    property OnSelectionChange: TSelectionChange read FSelectionChange write FSelectionChange;
  end;

implementation

uses
  Graphics,
  SysUtils,
  UErrorHandlingOperations;

{ Telector }

procedure TSelector.CreateMemberObjects;
const OPNAME = 'TSelector.CreateMemberObjects';
begin
  inherited;
  try
    Self.BevelOuter := bvNone;
    OnSelectionChange := nil;

    FName          := TLabel.Create(Self);
    FName.AutoSize := True;
    FSelector      := TSpinEdit.Create(Self);

    FName.Parent     := Self;
    FSelector.Parent := Self;

    FName.Visible     := True;
    FSelector.Visible := True;

    FName.Layout      := tlCenter;
    FName.Alignment   := taCenter;

    FSelector.OnChange := OnSelectorChanged;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TSelector.Resize;
const OPNAME = 'TSelector.Resize';
begin
  inherited;
  try
    FName.Align      := alNone;
    FSelector.Align  := alNone;
    FName.Width      := Self.ClientWidth div 2;

    FName.Align  := alLeft;
    FSelector.Align  := alClient;
    CenterControls;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TSelector.CenterControls;
const OPNAME = 'TSelector.CenterControls';
var
  LWidth,
  LLeft: integer;
begin
  try
    LLeft            := FSelector.Left;
    LWidth           := FSelector.Width;
    FName.Align      := alNone;
    FSelector.Align  := alNone;

    FName.Height     := 22;
    FSelector.Height := 22;

    FName.Top        := (Self.Height - FName.Height) div 2;
    FSelector.Top    := FName.Top;
    FSelector.Left   := LLeft;
    FSelector.Width   := LWidth;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TSelector.OnSelectorChanged(Sender: TObject);
const OPNAME = 'TSelector.OnSelectorChanged';
begin
  try
    if Assigned(OnSelectionChange) then
       OnSelectionChange(FSelector.Value);

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TSelector.Populate(AMinValue,AMaxValue: integer): boolean;
const OPNAME = 'TSelector.Populate';
begin
  Result := False;
  try
    //FSelector.OnChange := nil;
    //try
      FSelector.Increment := 1;
      FSelector.MaxValue       := AMaxValue;
      FSelector.MinValue       := AMinValue;
      FSelector.Enabled        := (AMinValue <> AMaxValue);
      FSelector.Value          := AMinValue;
      Result := True;
    //finally
    //  FSelector.OnChange := OnSelectorChanged;
    //end;

    //Put it here so that the event get fired.
    //FSelector.Value          := AMinValue;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TSelector.Select(AIndex: integer);
const OPNAME = 'TSelector.Select';
begin
  try
    if(AIndex >= FSelector.MinValue) and (AIndex <= FSelector.MaxValue) then
      FSelector.Value := AIndex;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TSelector.SetCaption(ACaption: string);
const OPNAME = 'TSelector.SetCaption';
begin
  try
    FName.Caption := ACaption;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
