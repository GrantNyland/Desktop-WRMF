//
//
//  UNIT      : Contains the class TGridOutputEditorToolBar.
//  AUTHOR    : Grant Nyland
//  DATE      : 2002/03/02
//  COPYRIGHT : Copyright © 2002 DWAF
//
//
unit UGridOutputEditorToolBar;

interface

uses
  UAbstractComponent,
  UHelpContexts,
  UChildToolBar;

type
  TGridOutputEditorToolBar = class(TChildToolBar)
  protected
    FPivot : TAbstractSpeedButton;
{
    FCreateReservoir : TAbstractSpeedButton;
    FDeleteReservoir : TAbstractSpeedButton;
    FCreateNodeWithInflow : TAbstractSpeedButton;
    FDeleteNodeWithInflow : TAbstractSpeedButton;
    FCreateNodeWithoutInflow : TAbstractSpeedButton;
    FDeleteNodeWithoutInflow : TAbstractSpeedButton;
    FCreateChannel : TAbstractSpeedButton;
    FDeleteChannel : TAbstractSpeedButton;
}
    procedure CreateMemberObjects; override;
    procedure SetHorizontalPositions; override;
    procedure AssignHelpContext; override;
{
    procedure OnClickCreateReservoir(Sender: TObject);
    procedure OnClickDeleteReservoir(Sender: TObject);
    procedure OnClickCreateNodeWithInflow(Sender: TObject);
    procedure OnClickDeleteNodeWithInflow(Sender: TObject);
    procedure OnClickCreateNodeWithoutInflow(Sender: TObject);
    procedure OnClickDeleteNodeWithoutInflow(Sender: TObject);
    procedure OnClickCreateChannel(Sender: TObject);
    procedure OnClickDeleteChannel(Sender: TObject);
}
  public
    function LanguageHasChanged: boolean; override;
    procedure SetPivotState(AEnabled: boolean);
{
    procedure SetCreateReservoir(AEnabled: boolean);
    procedure SetDeleteReservoir(AEnabled: boolean);
    procedure SetCreateNodeWithInflow(AEnabled: boolean);
    procedure SetDeleteNodeWithInflow(AEnabled: boolean);
    procedure SetCreateNodeWithoutInflow(AEnabled: boolean);
    procedure SetDeleteNodeWithoutInflow(AEnabled: boolean);
    procedure SetCreateChannel(AEnabled: boolean);
    procedure SetDeleteChannel(AEnabled: boolean);
}
    property PivotBtn: TAbstractSpeedButton read FPivot;
  end;

implementation

uses
  SysUtils,
  UMainMenuEventType,
  UErrorHandlingOperations;

procedure TGridOutputEditorToolBar.CreateMemberObjects;
const OPNAME = 'TGridOutputEditorToolBar.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
    FPivot                   := CreateButton('RotateView');
{
    if (FAppModules.StudyArea.ModelSubCode = 'Yield') then
    begin
      FCreateReservoir         := CreateButton('CreateReservoir');
      FDeleteReservoir         := CreateButton('DeleteReservoir');
      FCreateNodeWithInflow    := CreateButton('CreateNodeWithInflow');
      FDeleteNodeWithInflow    := CreateButton('DeleteNodeWithInflow');
      FCreateNodeWithoutInflow := CreateButton('CreateNodeWithoutInflow');
      FDeleteNodeWithoutInflow := CreateButton('DeleteNodeWithoutInflow');
      FCreateChannel           := CreateButton('CreateChannel');;
      FDeleteChannel           := CreateButton('DeleteChannel');;

      FCreateReservoir.OnClick         := OnClickCreateReservoir;
      FDeleteReservoir.OnClick         := OnClickDeleteReservoir;
      FCreateNodeWithInflow.OnClick    := OnClickCreateNodeWithInflow;
      FDeleteNodeWithInflow.OnClick    := OnClickDeleteNodeWithInflow;
      FCreateNodeWithoutInflow.OnClick := OnClickCreateNodeWithoutInflow;
      FDeleteNodeWithoutInflow.OnClick := OnClickDeleteNodeWithoutInflow;
      FCreateChannel.OnClick           := OnClickCreateChannel;
      FDeleteChannel.OnClick           := OnClickDeleteChannel;
    end;
}    
    SetHorizontalPositions;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TGridOutputEditorToolBar.AssignHelpContext;
const OPNAME = 'TGridOutputEditorToolBar.AssignHelpContext';
begin
  try
    SetControlHelpContext(Self,   HC_GridView);
    SetControlHelpContext(FPivot, HC_DataRotateView);
    //SetControlHelpContext(FPivot, HC_CreateReservoir);
    //SetControlHelpContext(FPivot, HC_DeleteReservoir);
    //SetControlHelpContext(FPivot, HC_CreateNodeWithInflow);
    //SetControlHelpContext(FPivot, HC_DeleteNodeWithInflow);
    //SetControlHelpContext(FPivot, HC_CreateNodeWithoutInflow);
    //SetControlHelpContext(FPivot, HC_DeleteNodeWithoutInflow);
    //SetControlHelpContext(FPivot, HC_CreateChannel);
    //SetControlHelpContext(FPivot, HC_DeleteChannel);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TGridOutputEditorToolBar.SetHorizontalPositions;
const OPNAME = 'TGridOutputEditorToolBar.SetHorizontalPositions';
var LButtonCount, LGaps: integer;
begin
  try
    inherited SetHorizontalPositions;
    LButtonCount := -1;
    LGaps :=  0;
    SetButtonHorizontalPosition(FPivot, True,  True, LButtonCount, LGaps);
{
    if (FAppModules.StudyArea.ModelSubCode = 'Yield') then
    begin
      SetButtonHorizontalPosition(FCreateReservoir, True,  False, LButtonCount, LGaps);
      SetButtonHorizontalPosition(FDeleteReservoir, True,  False, LButtonCount, LGaps);
      SetButtonHorizontalPosition(FCreateNodeWithInflow, True,  False, LButtonCount, LGaps);
      SetButtonHorizontalPosition(FDeleteNodeWithInflow, True,  False, LButtonCount, LGaps);
      SetButtonHorizontalPosition(FCreateNodeWithoutInflow, True,  False, LButtonCount, LGaps);
      SetButtonHorizontalPosition(FDeleteNodeWithoutInflow, True,  False, LButtonCount, LGaps);
      SetButtonHorizontalPosition(FCreateChannel, True,  False, LButtonCount, LGaps);
      SetButtonHorizontalPosition(FDeleteChannel, True,  False, LButtonCount, LGaps);
    end;
}
    Width := C_ButtonSize * (1 + LButtonCount) + C_ButtonGap * LGaps;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TGridOutputEditorToolBar.LanguageHasChanged: boolean;
const OPNAME = 'TGridOutputEditorToolBar.LanguageHasChanged';
begin
  Result := True;
  try
    inherited LanguageHasChanged;
    FPivot.LanguageHasChanged;
{
    if (FAppModules.StudyArea.ModelSubCode = 'Yield') then
    begin
      FCreateReservoir.LanguageHasChanged;
      FDeleteReservoir.LanguageHasChanged;
      FCreateNodeWithInflow.LanguageHasChanged;
      FDeleteNodeWithInflow.LanguageHasChanged;
      FCreateNodeWithoutInflow.LanguageHasChanged;
      FDeleteNodeWithoutInflow.LanguageHasChanged;
      FCreateChannel.LanguageHasChanged;
      FDeleteChannel.LanguageHasChanged;
    end;
}
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TGridOutputEditorToolBar.SetPivotState(AEnabled: boolean);
const OPNAME = 'TGridOutputEditorToolBar.SetPivotState';
begin
  try
    SetButtonEnabled(FPivot, AEnabled, 'ActionPivotDisabled');
  except on E: Exception do HandleError(E, OPNAME) end;
end;
{
procedure TGridOutputEditorToolBar.SetCreateChannel(AEnabled: boolean);
const OPNAME = 'TGridOutputEditorToolBar.SetCreateChannel';
begin
  try
    SetButtonEnabled(FCreateChannel, AEnabled, 'ActionCreateChannelDisabled');
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TGridOutputEditorToolBar.SetCreateNodeWithInflow(AEnabled: boolean);
const OPNAME = 'TGridOutputEditorToolBar.SetCreateNodeWithInflow';
begin
  try
    SetButtonEnabled(FCreateNodeWithInflow, AEnabled, 'ActionCreateNodeWithInflowDisabled');
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TGridOutputEditorToolBar.SetCreateNodeWithoutInflow(AEnabled: boolean);
const OPNAME = 'TGridOutputEditorToolBar.SetCreateNodeWithoutInflow';
begin
  try
    SetButtonEnabled(FCreateNodeWithoutInflow, AEnabled, 'ActionCreateNodeWithoutInflowDisabled');
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TGridOutputEditorToolBar.SetCreateReservoir(AEnabled: boolean);
const OPNAME = 'TGridOutputEditorToolBar.SetCreateReservoir';
begin
  try
    SetButtonEnabled(FCreateReservoir, AEnabled, 'ActionCreateReservoirDisabled');
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TGridOutputEditorToolBar.SetDeleteChannel(AEnabled: boolean);
const OPNAME = 'TGridOutputEditorToolBar.SetDeleteChannel';
begin
  try
    SetButtonEnabled(FDeleteChannel, AEnabled, 'ActionDeleteChannelDisabled');
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TGridOutputEditorToolBar.SetDeleteNodeWithInflow(AEnabled: boolean);
const OPNAME = 'TGridOutputEditorToolBar.SetDeleteNodeWithInflow';
begin
  try
    SetButtonEnabled(FDeleteNodeWithInflow, AEnabled, 'ActionDeleteNodeWithInflowDisabled');
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TGridOutputEditorToolBar.SetDeleteNodeWithoutInflow(AEnabled: boolean);
const OPNAME = 'TGridOutputEditorToolBar.SetDeleteNodeWithoutInflow';
begin
  try
    SetButtonEnabled(FDeleteNodeWithoutInflow, AEnabled, 'ActionDeleteNodeWithoutInflowDisabled');
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TGridOutputEditorToolBar.SetDeleteReservoir(AEnabled: boolean);
const OPNAME = 'TGridOutputEditorToolBar.SetDeleteReservoir';
begin
  try
    SetButtonEnabled(FDeleteReservoir, AEnabled, 'ActionDeleteReservoirDisabled');
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TGridOutputEditorToolBar.OnClickCreateChannel(Sender: TObject);
const OPNAME = 'TGridOutputEditorToolBar.OnClickCreateChannel';
begin
  try
    FAppModules.Model.ProcessEvent(CmeCreateChannel, nil);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TGridOutputEditorToolBar.OnClickCreateNodeWithInflow(Sender: TObject);
const OPNAME = 'TGridOutputEditorToolBar.OnClickCreateNodeWithInflow';
begin
  try
    FAppModules.Model.ProcessEvent(CmeCreateNodeWithInflow, nil);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TGridOutputEditorToolBar.OnClickCreateNodeWithoutInflow(Sender: TObject);
const OPNAME = 'TGridOutputEditorToolBar.OnClickCreateNodeWithoutInflow';
begin
  try
    FAppModules.Model.ProcessEvent(CmeCreateNodeWithoutInflow, nil);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TGridOutputEditorToolBar.OnClickCreateReservoir(Sender: TObject);
const OPNAME = 'TGridOutputEditorToolBar.OnClickCreateReservoir';
begin
  try
    FAppModules.Model.ProcessEvent(CmeCreateReservoir, nil);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TGridOutputEditorToolBar.OnClickDeleteChannel(Sender: TObject);
const OPNAME = 'TGridOutputEditorToolBar.OnClickDeleteChannel';
begin
  try
    FAppModules.Model.ProcessEvent(CmeDeleteChannel, nil);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TGridOutputEditorToolBar.OnClickDeleteNodeWithInflow(Sender: TObject);
const OPNAME = 'TGridOutputEditorToolBar.OnClickDeleteNodeWithInflow';
begin
  try
    FAppModules.Model.ProcessEvent(CmeDeleteNodeWithInflow, nil);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TGridOutputEditorToolBar.OnClickDeleteNodeWithoutInflow(Sender: TObject);
const OPNAME = 'TGridOutputEditorToolBar.OnClickDeleteNodeWithoutInflow';
begin
  try
    FAppModules.Model.ProcessEvent(CmeDeleteNodeWithoutInflow, nil);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TGridOutputEditorToolBar.OnClickDeleteReservoir(Sender: TObject);
const OPNAME = 'TGridOutputEditorToolBar.OnClickDeleteReservoir';
begin
  try
    FAppModules.Model.ProcessEvent(CmeDeleteReservoir, nil);
  except on E: Exception do HandleError(E, OPNAME) end;
end;
      }
end.
