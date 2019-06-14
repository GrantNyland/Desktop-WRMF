{******************************************************************************}
{* UNIT : Contains the class UHydroNVToolbar.                                 *}
{*        (Network Visualiser for Hydrology model)                            *}
{******************************************************************************}

unit UHydroNVToolbar;

interface

uses
  UAbstractComponent,
  UHelpContexts,
  UChildToolBar;

type

  THydroNVToolbar = class(TChildToolBar)
  protected
    FNewDrawing         : TAbstractSpeedButton;
    FDeleteDrawing      : TAbstractSpeedButton;
    FRenameDrawing      : TAbstractSpeedButton;
    FEditDrawing        : TAbstractSpeedButton;
    FViewDrawing        : TAbstractSpeedButton;
    FCopyDrawing        : TAbstractSpeedButton;
    procedure CreateMemberObjects; override;
    procedure SetHorizontalPositions; override;
    procedure OnNewDrawingClick(Sender: TObject);
    procedure OnDeleteDrawingClick(Sender: TObject);
    procedure OnRenameDrawingClick(Sender: TObject);
    procedure OnViewDrawingClick(Sender: TObject);
    procedure OnEditDrawingClick(Sender: TObject);
    procedure OnCopyDrawingClick(Sender: TObject);
    procedure AssignHelpContext; override;
  public
    function LanguageHasChanged: boolean; override;
    procedure SetNewDrawingState(AState: boolean);
    procedure SetDeleteDrawingState(AState: boolean);
    procedure SetRenameDrawingState(AState: boolean);
    procedure SetViewDrawingState(AState: boolean);
    procedure SetEditDrawingState(AState: boolean);
    procedure SetCopyDrawingState(AState: boolean);
  end;

implementation

uses
  SysUtils,
  UGenericModelLinkClasses,
  UMainMenuEventType,
  UErrorHandlingOperations;

procedure THydroNVToolbar.CreateMemberObjects;
const OPNAME = 'THydroNVToolbar.CreateMemberObjects';
begin
  inherited CreateMemberObjects;
  try
    FNewDrawing := CreateButton('VNVNewDrawing');
    FNewDrawing.OnClick := OnNewDrawingClick;
    FNewDrawing.Enabled := False;

    FDeleteDrawing := CreateButton('VNVDeleteDrawing');
    FDeleteDrawing.OnClick := OnDeleteDrawingClick;
    FDeleteDrawing.Enabled := False;

    FRenameDrawing := CreateButton('VNVRenameDrawing');
    FRenameDrawing.OnClick := OnRenameDrawingClick;
    FRenameDrawing.Enabled := False;

    FViewDrawing := CreateButton('VNVViewDrawing');
    FViewDrawing.OnClick := OnViewDrawingClick;
    FViewDrawing.Enabled := False;

    FEditDrawing := CreateButton('VNVEditDrawing');
    FEditDrawing.OnClick := OnEditDrawingClick;
    FEditDrawing.Enabled := False;

    FCopyDrawing := CreateButton('VNVCopyDrawing');
    FCopyDrawing.OnClick := OnCopyDrawingClick;
    FCopyDrawing.Enabled := False;

    SetHorizontalPositions;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure THydroNVToolbar.AssignHelpContext;
const OPNAME = 'THydroNVToolbar.AssignHelpContext';
begin
  try
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure THydroNVToolbar.SetHorizontalPositions;
const OPNAME = 'THydroNVToolbar.SetHorizontalPositions';
var
 LButtonCount,
 LGaps :integer;
begin
  try
    inherited SetHorizontalPositions;
    LButtonCount := -1;
    LGaps :=  0;
    SetButtonHorizontalPosition(FNewDrawing,         True, False, LButtonCount, LGaps);
    SetButtonHorizontalPosition(FDeleteDrawing,      True, False, LButtonCount, LGaps);
    SetButtonHorizontalPosition(FRenameDrawing,      True, False, LButtonCount, LGaps);
    SetButtonHorizontalPosition(FViewDrawing,        True, False, LButtonCount, LGaps);
    SetButtonHorizontalPosition(FEditDrawing,        True, False, LButtonCount, LGaps);
    SetButtonHorizontalPosition(FCopyDrawing,        True, False, LButtonCount, LGaps);
    Width := FCopyDrawing.Left + FCopyDrawing.Width;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydroNVToolbar.LanguageHasChanged: boolean;
const OPNAME = 'THydroNVToolbar.LanguageHasChanged';
begin
  Result := True;
  try
    inherited LanguageHasChanged;
    if Assigned(FRenameDrawing) then
      FRenameDrawing.LanguageHasChanged;
    if Assigned(FDeleteDrawing) then
      FDeleteDrawing.LanguageHasChanged;
    if Assigned(FNewDrawing) then
      FNewDrawing.LanguageHasChanged;
    if Assigned(FEditDrawing) then
      FEditDrawing.LanguageHasChanged;
    if Assigned(FViewDrawing) then
      FViewDrawing.LanguageHasChanged;
    if Assigned(FCopyDrawing) then
      FCopyDrawing.LanguageHasChanged;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure THydroNVToolbar.SetNewDrawingState(AState: boolean);
const OPNAME = 'THydroNVToolbar.SetNewDrawingState';
begin
  try
    SetButtonEnabled(FNewDrawing, AState, 'VNVNewDrawingDisabled');
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure THydroNVToolbar.SetDeleteDrawingState(AState: boolean);
const OPNAME = 'THydroNVToolbar.SetDeleteDrawingState';
begin
  try
    SetButtonEnabled(FDeleteDrawing, AState, 'VNVDeleteDrawingDisabled');
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure THydroNVToolbar.SetRenameDrawingState(AState: boolean);
const OPNAME = 'THydroNVToolbar.SetRenameDrawingState';
begin
  try
    SetButtonEnabled(FRenameDrawing, AState, 'VNVDeleteDrawingDisabled');
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure THydroNVToolbar.SetEditDrawingState(AState: boolean);
const OPNAME = 'THydroNVToolbar.SetEditDrawingState';
begin
  try
    SetButtonEnabled(FEditDrawing, AState, 'VNVEditDrawingDisabled');
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure THydroNVToolbar.SetViewDrawingState(AState: boolean);
const OPNAME = 'THydroNVToolbar.SetViewDrawingState';
begin
  try
    SetButtonEnabled(FViewDrawing, AState, 'VNVViewDrawingDisabled');
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure THydroNVToolbar.SetCopyDrawingState(AState: boolean);
const OPNAME = 'THydroNVToolbar.SetCopyDrawingState';
begin
  try
    SetButtonEnabled(FCopyDrawing, AState, 'VNVVCopyDrawingDisabled');
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure THydroNVToolbar.OnNewDrawingClick(Sender: TObject);
const OPNAME = 'THydroNVToolbar.OnNewDrawingClick';
begin
  try
    FAppModules.Model.ProcessEvent(CmeCustomModelEvent, TModelMenuData.Create(meHydroNVNewDrawing));
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure THydroNVToolbar.OnDeleteDrawingClick(Sender: TObject);
const OPNAME = 'THydroNVToolbar.OnDeleteDrawingClick';
begin
  try
    FAppModules.Model.ProcessEvent(CmeCustomModelEvent, TModelMenuData.Create(meHydroNVDeleteDrawing));
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure THydroNVToolbar.OnRenameDrawingClick(Sender: TObject);
const OPNAME = 'THydroNVToolbar.OnRenameDrawingClick';
begin
  try
    FAppModules.Model.ProcessEvent(CmeCustomModelEvent, TModelMenuData.Create(meHydroNVRenameDrawing));
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure THydroNVToolbar.OnEditDrawingClick(Sender: TObject);
const OPNAME = 'THydroNVToolbar.OnEditDrawingClick';
begin
  try
    FAppModules.Model.ProcessEvent(CmeCustomModelEvent, TModelMenuData.Create(meHydroNVEditDrawing));
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure THydroNVToolbar.OnViewDrawingClick(Sender: TObject);
const OPNAME = 'THydroNVToolbar.OnViewDrawingClick';
begin
  try
    FAppModules.Model.ProcessEvent(CmeCustomModelEvent, TModelMenuData.Create(meHydroNVViewDrawing));
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure THydroNVToolbar.OnCopyDrawingClick(Sender: TObject);
const OPNAME = 'THydroNVToolbar.OnCopyDrawingClick';
begin
  try
    FAppModules.Model.ProcessEvent(CmeCustomModelEvent, TModelMenuData.Create(meHydroNVCopyDrawing));
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
