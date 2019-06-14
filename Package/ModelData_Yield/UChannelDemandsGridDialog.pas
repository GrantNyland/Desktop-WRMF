//
//
//  UNIT      : Contains the class TChannelDemandsGridDialog.
//  AUTHOR    : Dziedzi Ramulondi (ARIVIA)
//  DATE      : 2005/05/03
//  COPYRIGHT : Copyright © 2005 DWAF
//
//

unit UChannelDemandsGridDialog;

interface

uses
  Classes,
  Windows,
  VCL.Controls,
  VCL.ComCtrls,
  VCL.ExtCtrls,
  VCL.StdCtrls,
  VCL.Buttons,
  VCL.CheckLst,
  VCL.Forms,
  UAbstractObject,
  UAbstractComponent,
  UDataEditComponent,
  UDataComponent;

type

  TChannelDemandsGridDialog = class(TAbstractScrollablePanel)
  protected
    FSelectorPanel         : TPanel;
    FBtnDataSelection      : TFieldButton;

    FgboxLimits            : TGroupBox;
    FchkboxLimits          : TCheckBox;
    FLimitsGrid            : TFieldStringGrid;

    FgboxDemands           : TGroupBox;
    FDemandsGrid           : TAbstractStringGrid;
    procedure CreateMemberObjects; override;
    procedure AssignHelpContext; override;
  public
    procedure Resize; override;
    procedure DoExport(AFileName: string = ''); override;
    procedure DoPrint; override;
    function CanExport : boolean; override;
    function CanPrint  : boolean; override;
    function StudyHasChanged: boolean; override;
    function LanguageHasChanged: boolean; override;
    function Initialise: boolean; override;

    property BtnDataSelection : TFieldButton        read FBtnDataSelection;
    property gboxLimits       : TGroupBox           read FgboxLimits;
    property chkboxLimits     : TCheckBox           read FchkboxLimits;
    property LimitsGrid       : TFieldStringGrid    read FLimitsGrid;
    property gboxDemands      : TGroupBox           read FgboxDemands;
    property DemandsGrid      : TAbstractStringGrid read FDemandsGrid;
  end;

  implementation

uses
  SysUtils,
  VCL.Graphics,
  VCL.Grids,
  UHelpContexts,
  UControlCreationUtilities,
  UErrorHandlingOperations;

  { TChannelDemandsGridDialog }

procedure TChannelDemandsGridDialog.CreateMemberObjects;
const OPNAME = 'TChannelDemandsGridDialog.CreateMemberObjects';
begin
  inherited CreateMemberObjects;
  try
    FSelectorPanel              := TPanel.Create(ControlsOwner);
    FSelectorPanel.Parent       := ControlsParent;
    FSelectorPanel.Align        := alTop;

    FbtnDataSelection           := TFieldButton.Create(ControlsOwner, FAppModules,'');
    FbtnDataSelection.Parent    := FSelectorPanel;

    FgboxLimits                 := TGroupBox.Create(Self);
    FgboxLimits.Parent          := ControlsParent;
    FgboxLimits.Align           := alTop;

    FchkboxLimits               := TCheckBox.Create(Self);
    FchkboxLimits.Parent        := FgboxLimits;
    FchkboxLimits.Align         := alTop;

    FLimitsGrid                 := TFieldStringGrid.Create(ControlsOwner, FAppModules);
    FLimitsGrid.Parent          := FgboxLimits;
    FLimitsGrid.Align           := alClient;
    FLimitsGrid.FixedCols       := 1;
    FLimitsGrid.FixedRows       := 1;
    FLimitsGrid.ScrollBars      := ssBoth;
    FLimitsGrid.Alignment       := taRightJustify;
    FLimitsGrid.Options         := [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine,goEditing,goAlwaysShowEditor];


    FgboxDemands                := TGroupBox.Create(Self);
    FgboxDemands.Parent         := ControlsParent;
    FgboxDemands.Align          := alClient;

    FDemandsGrid                   := TAbstractStringGrid.Create(ControlsOwner, FAppModules);
    FDemandsGrid.Parent            := FgboxDemands;
    FDemandsGrid.Align             := alClient;
    FDemandsGrid.ScrollBars        := ssBoth;
    FDemandsGrid.FixedCols         := 0;
    FDemandsGrid.FixedRows         := 1;
    FDemandsGrid.Alignment         := taRightJustify;
    FDemandsGrid.Options           := [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine];
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TChannelDemandsGridDialog.Initialise: boolean;
const OPNAME = 'TChannelDemandsGridDialog.Initialise';
begin
  Result := inherited Initialise;
  try
    FDemandsGrid.Initialise;
    FSelectorPanel.BorderStyle := bsSingle;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TChannelDemandsGridDialog.StudyHasChanged: boolean;
const OPNAME = 'TChannelDemandsGridDialog.StudyHasChanged';
begin
  Result := inherited StudyHasChanged;
  try
    FDemandsGrid.StudyHasChanged;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TChannelDemandsGridDialog.LanguageHasChanged: boolean;
const OPNAME = 'TChannelDemandsGridDialog.LanguageHasChanged';
begin
  Result := inherited LanguageHasChanged;
  try
    FDemandsGrid.LanguageHasChanged;
    FchkboxLimits.Caption     := 'Apply limits';
    FbtnDataSelection.Caption := FAppModules.Language.GetString('LabelText.SelectData');
    FgboxLimits.Caption    := 'Demand values limits';
    FgboxDemands.Caption    := 'Channel demands';
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TChannelDemandsGridDialog.AssignHelpContext;
const OPNAME = 'TChannelDemandsGridDialog.AssignHelpContext';
begin
  try
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TChannelDemandsGridDialog.Resize;
const OPNAME = 'TChannelDemandsGridDialog.Resize';
begin
  try
    FSelectorPanel.ClientHeight := 30;
    FBtnDataSelection.Align     := alRight;
    FBtnDataSelection.Width     := 80;
    FgboxLimits.Height          := 120;
    inherited;
  except on E: Exception do HandleError(E, OPNAME); end;

end;

procedure TChannelDemandsGridDialog.DoExport(AFileName: string = '');
const OPNAME = 'TChannelDemandsGridDialog.DoExport';
begin
  try
    if FDemandsGrid.Visible then FDemandsGrid.DoExport(AFileName);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TChannelDemandsGridDialog.DoPrint;
const OPNAME = 'TChannelDemandsGridDialog.DoPrint';
begin
  try
    if FDemandsGrid.Visible then FDemandsGrid.DoPrint('');
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TChannelDemandsGridDialog.CanExport: boolean;
const OPNAME = 'TChannelDemandsGridDialog.CanExport';
begin
  Result := False;
  try
    Result := (Assigned(FDemandsGrid)            and (FDemandsGrid.Visible));
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TChannelDemandsGridDialog.CanPrint: boolean;
const OPNAME = 'TChannelDemandsGridDialog.CanPrint';
begin
  Result := False;
  try
    Result := (Assigned(FDemandsGrid)            and (FDemandsGrid.Visible));
  except on E: Exception do HandleError(E, OPNAME); end;
end;

end.

