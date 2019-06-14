//
//
//  UNIT      : Contains the class TOutputComparitorToolBar.
//  AUTHOR    : Sam Dhlamini (ARAVIA)
//  DATE      : 2002/03/02
//  COPYRIGHT : Copyright © 2002 DWAF
//
//
unit UOutputComparitorToolBar;

interface

uses
  UAbstractComponent,
  UHelpContexts,
  UChildToolBar;

type
  TOutputComparitorToolBar = class(TChildToolBar)
  protected
    FShowChartLegendDialog  : TAbstractSpeedButton;
    procedure CreateMemberObjects; override;
    procedure AssignHelpContext; override;
    procedure SetHorizontalPositions; override;
    procedure OnShowChartLegendDialog(Sender: TObject);
  public
    procedure SetShowChartLegendDialogState(AEnabled: boolean);
    function LanguageHasChanged: boolean; override;
    function StudyHasChanged: boolean; override;
  end;

implementation

uses
  SysUtils,
  UGenericModelLinkClasses,
  UMainMenuEventType,
  UErrorHandlingOperations;

procedure TOutputComparitorToolBar.CreateMemberObjects;
const OPNAME = 'TOutputComparitorToolBar.CreateMemberObjects';
begin
  inherited CreateMemberObjects;
  try

    FShowChartLegendDialog         := CreateButton('OutputComparitorShowChartLegendDialog');
    FShowChartLegendDialog.OnClick := OnShowChartLegendDialog;
    FShowChartLegendDialog.Enabled := False;
    SetHorizontalPositions;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputComparitorToolBar.AssignHelpContext;
const OPNAME = 'TOutputComparitorToolBar.AssignHelpContext';
begin
  try
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TOutputComparitorToolBar.SetHorizontalPositions;
const OPNAME = 'TOutputComparitorToolBar.SetHorizontalPositions';
var
  LButtonCount, LGaps: integer;
begin
  try
    inherited SetHorizontalPositions;
    LButtonCount := -1;
    LGaps :=  0;
    SetButtonHorizontalPosition(FShowChartLegendDialog,  True, False, LButtonCount, LGaps);
    Width := FShowChartLegendDialog.Left + FShowChartLegendDialog.Width;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOutputComparitorToolBar.LanguageHasChanged: boolean;
const OPNAME = 'TOutputComparitorToolBar.LanguageHasChanged';
begin
  Result := True;
  try
    inherited LanguageHasChanged;

    if Assigned(FShowChartLegendDialog) then
      FShowChartLegendDialog.LanguageHasChanged;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOutputComparitorToolBar.StudyHasChanged: boolean;
const OPNAME = 'TOutputComparitorToolBar.StudyHasChanged';
begin
  Result := inherited StudyHasChanged;
  try

  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputComparitorToolBar.OnShowChartLegendDialog(Sender: TObject);
const OPNAME = 'TOutputComparitorToolBar.OnShowChartLegendDialog';
begin
  try
    FAppModules.Model.ProcessEvent(CmeCustomModelEvent, TModelMenuData.Create(meOutputComparitorShowChartLegendDialog));
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputComparitorToolBar.SetShowChartLegendDialogState(AEnabled: boolean);
const OPNAME = 'TOutputComparitorToolBar.SetShowChartLegendDialogState';
begin
  try
    SetButtonEnabled(FShowChartLegendDialog, AEnabled, 'ShowChartLegendDialogDisabled');
  except on E: Exception do HandleError(E, OPNAME) end;
end;


end.
