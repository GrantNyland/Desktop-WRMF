//
//
//  UNIT      : Contains the class TGridEditorToolBar.
//  AUTHOR    : Grant Nyland
//  DATE      : 2002/03/02
//  COPYRIGHT : Copyright © 2002 DWAF
//
//
unit UGridEditorToolBar;

interface

uses
  UAbstractComponent,
  UHelpContexts,
  UChildToolBar;

type
  TGridEditorToolBar = class(TChildToolBar)
  protected
    FPivot                       : TAbstractSpeedButton;
    procedure CreateMemberObjects; override;
    procedure SetHorizontalPositions; override;
    procedure AssignHelpContext; override;

  public
    function LanguageHasChanged: boolean; override;
    procedure SetPivotState(AEnabled: boolean);
    property PivotBtn: TAbstractSpeedButton read FPivot;
  end;

implementation

uses
  SysUtils,
  UAbstractObject,
  UMainMenuEventType,
  UErrorHandlingOperations;

procedure TGridEditorToolBar.CreateMemberObjects;
const OPNAME = 'TGridEditorToolBar.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
    FPivot                   := CreateButton('RotateView');
    SetHorizontalPositions;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TGridEditorToolBar.AssignHelpContext;
const OPNAME = 'TGridEditorToolBar.AssignHelpContext';
begin
  try
    SetControlHelpContext(Self,   HC_GridView);
    SetControlHelpContext(FPivot, HC_DataRotateView);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TGridEditorToolBar.SetHorizontalPositions;
const OPNAME = 'TGridEditorToolBar.SetHorizontalPositions';
var LButtonCount, LGaps: integer;
begin
  try
    inherited SetHorizontalPositions;
    LButtonCount := -1;
    LGaps :=  0;
    SetButtonHorizontalPosition(FPivot, True,  True, LButtonCount, LGaps);
    Width := C_ButtonSize * (1 + LButtonCount) + C_ButtonGap * LGaps;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TGridEditorToolBar.LanguageHasChanged: boolean;
const OPNAME = 'TGridEditorToolBar.LanguageHasChanged';
begin
  Result := True;
  try
    inherited LanguageHasChanged;
    FPivot.LanguageHasChanged;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TGridEditorToolBar.SetPivotState(AEnabled: boolean);
const OPNAME = 'TGridEditorToolBar.SetPivotState';
begin
  try
    SetButtonEnabled(FPivot, AEnabled, 'ActionPivotDisabled');
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
