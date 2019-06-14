//
//
//  UNIT      : Contains the class TChildToolBar.
//  AUTHOR    : Grant Nyland
//  DATE      : 2002/03/02
//  COPYRIGHT : Copyright © 2002 DWAF
//
//
unit UChildToolBar;

interface

uses
  UAbstractComponent;

const
  C_ButtonSize = 28;
  C_ButtonGap  = 8;

type
  TChildToolBar = class(TAbstractToolBar)
  protected
    function CreateButton(AButtonKey: string): TAbstractSpeedButton; virtual;
    procedure CreateMemberObjects; override;
    procedure SetButtonHorizontalPosition(
      AButton: TAbstractSpeedButton; AVisible, AGap: boolean; var AButtonCount, AGaps: integer); virtual;
  end;

implementation

uses
  SysUtils,
  Vcl.Controls,
  UAbstractObject,
  UErrorHandlingOperations;

procedure TChildToolBar.CreateMemberObjects;
const OPNAME = 'TChildToolBar.CreateMemberObjects';
begin
  try
    BevelOuter := bvNone;
    Height := C_ButtonSize;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TChildToolBar.CreateButton(AButtonKey: string): TAbstractSpeedButton;
const OPNAME = 'TChildToolBar.CreateButton';
begin
  Result := nil;
  try
    Result := TAbstractSpeedButton.Create(self, FAppModules, AButtonKey);
    Result.Parent    := self;
    Result.Top       := 0;
    Result.Height    := C_ButtonSize;
    Result.Width     := C_ButtonSize;
    Result.Name      := AButtonKey;
    Result.Glyph.LoadFromResourceName(HImagesInstance, UpperCase(AButtonKey));
    Result.NumGlyphs := Result.Glyph.Width div Result.Glyph.Height;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TChildToolBar.SetButtonHorizontalPosition(
  AButton: TAbstractSpeedButton; AVisible, AGap: boolean; var AButtonCount, AGaps: integer);
const OPNAME = 'TChildToolBar.SetButtonHorizontalPosition';
begin
  try
    if Assigned(AButton) and AVisible then
    begin
      Inc(AButtonCount);
      if AGap then
        Inc(AGaps, C_ButtonGap);
      AButton.Left := AGaps + C_ButtonSize * AButtonCount;
      AButton.Visible := True;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
