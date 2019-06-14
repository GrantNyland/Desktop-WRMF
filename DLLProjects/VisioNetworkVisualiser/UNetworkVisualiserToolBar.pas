//
//
//  UNIT      : Contains TNetworkVisualiserToolBar Class
//  AUTHOR    : Dziedzi Ramulondi
//  DATE      : 2005/03/01
//  COPYRIGHT : Copyright © 2005 DWAF
//
//
unit UNetworkVisualiserToolBar;

interface

uses
  UAbstractComponent,
  UHelpContexts,
  UChildToolBar;

type

  TNetworkVisualiserToolBar = class(TChildToolBar)
  protected
    FNewDrawingGroup    : TAbstractSpeedButton;
    FDeleteDrawingGroup : TAbstractSpeedButton;
    FRenameDrawingGroup : TAbstractSpeedButton;
    FNewDrawing         : TAbstractSpeedButton;
    FDeleteDrawing      : TAbstractSpeedButton;
    FRenameDrawing      : TAbstractSpeedButton;
    FEditDrawing        : TAbstractSpeedButton;
    FViewDrawing        : TAbstractSpeedButton;
    FCopyDrawing        : TAbstractSpeedButton;
    procedure CreateMemberObjects; override;
    procedure SetHorizontalPositions; override;
    procedure OnNewDrawingGroupClick(Sender: TObject);
    procedure OnDeleteDrawingGroupClick(Sender: TObject);
    procedure OnRenameDrawingGroupClick(Sender: TObject);
    procedure OnNewDrawingClick(Sender: TObject);
    procedure OnDeleteDrawingClick(Sender: TObject);
    procedure OnRenameDrawingClick(Sender: TObject);
    procedure OnViewDrawingClick(Sender: TObject);
    procedure OnEditDrawingClick(Sender: TObject);
    procedure OnCopyDrawingClick(Sender: TObject);
    procedure AssignHelpContext; override;
  public
    function LanguageHasChanged: boolean; override;
    procedure SetNewDrawingGroupState(AState: boolean);
    procedure SetDeleteDrawingGroupState(AState: boolean);
    procedure SetRenameDrawingGroupState(AState: boolean);
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

procedure TNetworkVisualiserToolBar.CreateMemberObjects;
const OPNAME = 'TNetworkVisualiserToolBar.CreateMemberObjects';
begin
  inherited CreateMemberObjects;
  try
    FNewDrawingGroup := CreateButton('VNVNewDrawingGroup');
    FNewDrawingGroup.OnClick := OnNewDrawingGroupClick;
    FNewDrawingGroup.Enabled := False;

    FDeleteDrawingGroup := CreateButton('VNVDeleteDrawingGroup');
    FDeleteDrawingGroup.OnClick := OnDeleteDrawingGroupClick;
    FDeleteDrawingGroup.Enabled := False;

    FRenameDrawingGroup := CreateButton('VNVRenameDrawingGroup');
    FRenameDrawingGroup.OnClick := OnRenameDrawingGroupClick;
    FRenameDrawingGroup.Enabled := False;

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

procedure TNetworkVisualiserToolBar.AssignHelpContext;
const OPNAME = 'TNetworkVisualiserToolBar.AssignHelpContext';
begin
  try
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TNetworkVisualiserToolBar.SetHorizontalPositions;
const OPNAME = 'TNetworkVisualiserToolBar.SetHorizontalPositions';
var
 LButtonCount,
 LGaps :integer;
begin
  try
    inherited SetHorizontalPositions;
    LButtonCount := -1;
    LGaps :=  0;
    //SetButtonHorizontalPosition(FNewDrawingGroup,    True, False, LButtonCount, LGaps);
    //SetButtonHorizontalPosition(FDeleteDrawingGroup, True, False, LButtonCount, LGaps);
    //SetButtonHorizontalPosition(FRenameDrawingGroup, True, False, LButtonCount, LGaps);
    SetButtonHorizontalPosition(FNewDrawing,         True, False, LButtonCount, LGaps);
    SetButtonHorizontalPosition(FDeleteDrawing,      True, False, LButtonCount, LGaps);
    SetButtonHorizontalPosition(FRenameDrawing,      True, False, LButtonCount, LGaps);
    SetButtonHorizontalPosition(FViewDrawing,        True, False, LButtonCount, LGaps);
    SetButtonHorizontalPosition(FEditDrawing,        True, False, LButtonCount, LGaps);
    SetButtonHorizontalPosition(FCopyDrawing,        True, False, LButtonCount, LGaps);
    Width := FCopyDrawing.Left + FCopyDrawing.Width;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TNetworkVisualiserToolBar.LanguageHasChanged: boolean;
const OPNAME = 'TNetworkVisualiserToolBar.LanguageHasChanged';
begin
  Result := True;
  try
    inherited LanguageHasChanged;
    if Assigned(FRenameDrawingGroup) then
      FRenameDrawingGroup.LanguageHasChanged;
    if Assigned(FDeleteDrawingGroup) then
      FDeleteDrawingGroup.LanguageHasChanged;
    if Assigned(FNewDrawingGroup) then
      FNewDrawingGroup.LanguageHasChanged;
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

procedure TNetworkVisualiserToolBar.SetNewDrawingGroupState(AState: boolean);
const OPNAME = 'TNetworkVisualiserToolBar.SetNewDrawingGroupState';
begin
  try
    SetButtonEnabled(FNewDrawingGroup, AState, 'VNVNewDrawingGroupDisabled');
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TNetworkVisualiserToolBar.SetRenameDrawingGroupState(AState: boolean);
const OPNAME = 'TNetworkVisualiserToolBar.SetRenameDrawingGroupState';
begin
  try
    SetButtonEnabled(FRenameDrawingGroup, AState, 'VNVDeleteDrawingGroupDisabled');
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TNetworkVisualiserToolBar.SetDeleteDrawingGroupState(AState: boolean);
const OPNAME = 'TNetworkVisualiserToolBar.SetDeleteDrawingGroupState';
begin
  try
    SetButtonEnabled(FDeleteDrawingGroup, AState, 'VNVDeleteDrawingGroupDisabled');
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TNetworkVisualiserToolBar.SetNewDrawingState(AState: boolean);
const OPNAME = 'TNetworkVisualiserToolBar.SetNewDrawingState';
begin
  try
    SetButtonEnabled(FNewDrawing, AState, 'VNVNewDrawingDisabled');
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TNetworkVisualiserToolBar.SetDeleteDrawingState(AState: boolean);
const OPNAME = 'TNetworkVisualiserToolBar.SetDeleteDrawingState';
begin
  try
    SetButtonEnabled(FDeleteDrawing, AState, 'VNVDeleteDrawingDisabled');
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TNetworkVisualiserToolBar.SetRenameDrawingState(AState: boolean);
const OPNAME = 'TNetworkVisualiserToolBar.SetRenameDrawingState';
begin
  try
    SetButtonEnabled(FRenameDrawing, AState, 'VNVDeleteDrawingDisabled');
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TNetworkVisualiserToolBar.SetEditDrawingState(AState: boolean);
const OPNAME = 'TNetworkVisualiserToolBar.SetEditDrawingState';
begin
  try
    SetButtonEnabled(FEditDrawing, AState, 'VNVEditDrawingDisabled');
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TNetworkVisualiserToolBar.SetViewDrawingState(AState: boolean);
const OPNAME = 'TNetworkVisualiserToolBar.SetViewDrawingState';
begin
  try
    SetButtonEnabled(FViewDrawing, AState, 'VNVViewDrawingDisabled');
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TNetworkVisualiserToolBar.SetCopyDrawingState(AState: boolean);
const OPNAME = 'TNetworkVisualiserToolBar.SetCopyDrawingState';
begin
  try
    SetButtonEnabled(FCopyDrawing, AState, 'VNVVCopyDrawingDisabled');
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TNetworkVisualiserToolBar.OnNewDrawingGroupClick(Sender: TObject);
const OPNAME = 'TNetworkVisualiserToolBar.OnNewDrawingGroupClick';
begin
  try
    FAppModules.Model.ProcessEvent(CmeCustomModelEvent, TModelMenuData.Create(meNewDrawingGroup));
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TNetworkVisualiserToolBar.OnDeleteDrawingGroupClick(Sender: TObject);
const OPNAME = 'TNetworkVisualiserToolBar.OnDeleteDrawingGroupClick';
begin
  try
    FAppModules.Model.ProcessEvent(CmeCustomModelEvent, TModelMenuData.Create(meDeleteDrawingGroup));
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TNetworkVisualiserToolBar.OnRenameDrawingGroupClick(Sender: TObject);
const OPNAME = 'TNetworkVisualiserToolBar.OnRenameDrawingGroupClick';
begin
  try
    FAppModules.Model.ProcessEvent(CmeCustomModelEvent, TModelMenuData.Create(meRenameDrawingGroup));
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TNetworkVisualiserToolBar.OnNewDrawingClick(Sender: TObject);
const OPNAME = 'TNetworkVisualiserToolBar.OnNewDrawingClick';
begin
  try
    FAppModules.Model.ProcessEvent(CmeCustomModelEvent, TModelMenuData.Create(meNewDrawing));
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TNetworkVisualiserToolBar.OnDeleteDrawingClick(Sender: TObject);
const OPNAME = 'TNetworkVisualiserToolBar.OnDeleteDrawingClick';
begin
  try
    FAppModules.Model.ProcessEvent(CmeCustomModelEvent, TModelMenuData.Create(meDeleteDrawing));
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TNetworkVisualiserToolBar.OnRenameDrawingClick(Sender: TObject);
const OPNAME = 'TNetworkVisualiserToolBar.OnRenameDrawingClick';
begin
  try
    FAppModules.Model.ProcessEvent(CmeCustomModelEvent, TModelMenuData.Create(meRenameDrawing));
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TNetworkVisualiserToolBar.OnEditDrawingClick(Sender: TObject);
const OPNAME = 'TNetworkVisualiserToolBar.OnEditDrawingClick';
begin
  try
    FAppModules.Model.ProcessEvent(CmeCustomModelEvent, TModelMenuData.Create(meEditDrawing));
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TNetworkVisualiserToolBar.OnViewDrawingClick(Sender: TObject);
const OPNAME = 'TNetworkVisualiserToolBar.OnViewDrawingClick';
begin
  try
    FAppModules.Model.ProcessEvent(CmeCustomModelEvent, TModelMenuData.Create(meViewDrawing));
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TNetworkVisualiserToolBar.OnCopyDrawingClick(Sender: TObject);
const OPNAME = 'TNetworkVisualiserToolBar.OnCopyDrawingClick';
begin
  try
    FAppModules.Model.ProcessEvent(CmeCustomModelEvent, TModelMenuData.Create(meCopyDrawing));
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
