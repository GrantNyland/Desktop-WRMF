//
//
//  UNIT      : Contains TPlaneSelector Classes
//  AUTHOR    : Dziedzi Ramulondi(PDNA)
//  DATE      : 25/09/2002
//  COPYRIGHT : Copyright © 2002 DWAF
//
//
unit UPlaneSelector;

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
  TPlaneSelectionChange = procedure(ASelection: integer) of object;
  TPlaneSelector = class(TAbstractPanel)
  protected
    FName: TLabel;
    FSelector: TSpinEdit;
    FPlaneSelectionChange: TPlaneSelectionChange;
    procedure CreateMemberObjects; override;
    procedure OnSelectorChanged(Sender: TObject);
    procedure CenterControls;
  public
    // Overriden from Delphi.
    procedure Resize; override;
    procedure SetCaption(ACaption: string);
    procedure SelectPlane(APlaneIndex: integer);
    function PopulatePlane(AMinValue,AMaxValue: integer): boolean;
    property OnPlaneSelectionChange: TPlaneSelectionChange read FPlaneSelectionChange write FPlaneSelectionChange;
  end;

implementation

uses
  Graphics,
  SysUtils,
  UErrorHandlingOperations;

{ TPlaneSelector }

procedure TPlaneSelector.CreateMemberObjects;
const OPNAME = 'TPlaneSelector.CreateMemberObjects';
begin
  inherited;
  try
    Self.BevelOuter := bvNone;
    OnPlaneSelectionChange := nil;

    FName     := TLabel.Create(Self);
    FSelector := TSpinEdit.Create(Self);

    FName.Parent     := Self;
    FSelector.Parent := Self;

    FName.Visible     := True;
    FSelector.Visible := True;

    FName.Layout      := tlCenter;
    FName.Alignment   := taCenter;

    FSelector.OnChange := OnSelectorChanged;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TPlaneSelector.Resize;
const OPNAME = 'TPlaneSelector.Resize';
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

procedure TPlaneSelector.CenterControls;
const OPNAME = 'TPlaneSelector.CenterControls';
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

procedure TPlaneSelector.OnSelectorChanged(Sender: TObject);
const OPNAME = 'TPlaneSelector.OnSelectorChanged';
begin
  try
    if Assigned(OnPlaneSelectionChange) then
       OnPlaneSelectionChange(FSelector.Value);

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TPlaneSelector.PopulatePlane(AMinValue,AMaxValue: integer): boolean;
const OPNAME = 'TPlaneSelector.PopulatePlane';
begin
  Result := False;
  try
    //FSelector.OnChange := nil;
    //try
      FSelector.Increment := 1;
      FSelector.MaxValue       := AMaxValue;
      FSelector.MinValue       := AMinValue;
      FSelector.Enabled        := (AMinValue <> AMaxValue);
      Result := True;
    //finally
    //  FSelector.OnChange := OnSelectorChanged;
    //end;

    //Put it here so that the event get fired.
    //FSelector.Value          := AMinValue;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TPlaneSelector.SelectPlane(APlaneIndex: integer);
const OPNAME = 'TPlaneSelector.SelectPlane';
begin
  try
    if(APlaneIndex >= FSelector.MinValue) and (APlaneIndex <= FSelector.MaxValue) then
      FSelector.Value := APlaneIndex;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TPlaneSelector.SetCaption(ACaption: string);
const OPNAME = 'TPlaneSelector.SetCaption';
begin
  try
    FName.Caption := ACaption;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
