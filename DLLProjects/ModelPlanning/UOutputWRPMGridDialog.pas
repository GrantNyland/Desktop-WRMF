//
//
//  UNIT      : Contains the class TOutputWRPMGridDialog.
//  AUTHOR    : Dziedzi Ramulondi (ARIVIA)
//  DATE      : 2005/05/03
//  COPYRIGHT : Copyright © 2005 DWAF
//
//

unit UOutputWRPMGridDialog;

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
  VCL.Grids,
  VCL.Forms,
  UAbstractObject,
  UAbstractComponent,
  UDataEditComponent,
  UDataComponent;

type

  TOutputWRPMGridDialog = class(TAbstractScrollablePanel)
  protected
    FSelectorPanel     : TPanel;
    FViewDataType      : TFieldComboBox;
    FViewDataLabel     : TLabel;
    FUnitsLabel        : TLabel;
    FBtnDataSelection  : TFieldButton;
    FDataGrid          : TAbstractStringGrid;
    procedure CreateMemberObjects; override;
    procedure AssignHelpContext; override;
    procedure BeforeDrawindACell(Sender: TObject; ACol, ARow: Longint; Rect: TRect; State: TGridDrawState);
  public
    procedure Resize; override;
    procedure DoExport(AFileName: string = ''); override;
    procedure DoPrint; override;
    function CanExport : boolean; override;
    function CanPrint  : boolean; override;
    function StudyHasChanged: boolean; override;
    function LanguageHasChanged: boolean; override;
    function Initialise: boolean; override;

    property grdDataGrid        : TAbstractStringGrid  read FDataGrid;
    property cmbViewDataType    : TFieldComboBox       read FViewDataType;
    property ViewDataLabel      : TLabel               read FViewDataLabel;
    property BtnDataSelection   : TFieldButton         read FBtnDataSelection;
    property UnitsLabel         : TLabel               read FUnitsLabel;
  end;

  implementation

uses
  SysUtils,
  VCL.Graphics,
  UHelpContexts,
  UControlCreationUtilities,
  UErrorHandlingOperations;

  { TOutputWRPMGridDialog }

procedure TOutputWRPMGridDialog.CreateMemberObjects;
const OPNAME = 'TOutputWRPMGridDialog.CreateMemberObjects';
begin
  inherited CreateMemberObjects;
  try
    FSelectorPanel              := TPanel.Create(ControlsOwner);
    FSelectorPanel.Parent       := ControlsParent;

    FViewDataLabel              := TLabel.Create(ControlsOwner);
    FViewDataLabel.Parent       := FSelectorPanel;
    FViewDataLabel.Alignment    := taCenter;
    FViewDataLabel.AutoSize     := False;

    FUnitsLabel                 := TLabel.Create(ControlsOwner);
    FUnitsLabel.Parent          := FSelectorPanel;

    FViewDataType               := TFieldComboBox.Create(ControlsOwner,FAppModules);
    FViewDataType.Parent        := FSelectorPanel;
    FViewDataType.Style         := csDropDownList;

    FbtnDataSelection           := TFieldButton.Create(ControlsOwner, FAppModules,'');
    FbtnDataSelection.Parent    := FSelectorPanel;

    FDataGrid                   := TAbstractStringGrid.Create(ControlsOwner, FAppModules);
    FDataGrid.Parent            := ControlsParent;
    FDataGrid.FixedCols         := 0;
    FDataGrid.FixedRows         := 0;
    FDataGrid.Alignment         := taRightJustify;
    FDataGrid.Options           := [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine];
    FDataGrid.OnDrawCell        := BeforeDrawindACell;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOutputWRPMGridDialog.Initialise: boolean;
const OPNAME = 'TOutputWRPMGridDialog.Initialise';
begin
  Result := inherited Initialise;
  try
    FViewDataType.Clear;
    FDataGrid.Initialise;
    FSelectorPanel.BorderStyle := bsSingle;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOutputWRPMGridDialog.StudyHasChanged: boolean;
const OPNAME = 'TOutputWRPMGridDialog.StudyHasChanged';
begin
  Result := inherited StudyHasChanged;
  try
    FDataGrid.StudyHasChanged;
    FViewDataType.StudyHasChanged;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOutputWRPMGridDialog.LanguageHasChanged: boolean;
const OPNAME = 'TOutputWRPMGridDialog.LanguageHasChanged';
begin
  Result := inherited LanguageHasChanged;
  try
    FDataGrid.LanguageHasChanged;
    FViewDataType.LanguageHasChanged;
    FViewDataLabel.Caption := FAppModules.Language.GetString('LabelText.ViewData');
    FbtnDataSelection.Caption := FAppModules.Language.GetString('LabelText.SelectData');
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputWRPMGridDialog.AssignHelpContext;
const OPNAME = 'TOutputWRPMGridDialog.AssignHelpContext';
begin
  try
    SetControlHelpContext(Self,           HC_ResultOutputControl);
    SetControlHelpContext(FViewDataType,  HC_ResultOutputControl);
    SetControlHelpContext(FDataGrid,      HC_ResultOutputControl);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TOutputWRPMGridDialog.Resize;
const OPNAME = 'TOutputWRPMGridDialog.Resize';
begin
  try
    LockWindowUpdate(Self.Handle);
    try
      FSelectorPanel.Align        := alTop;
      FSelectorPanel.ClientHeight := 30;

      FViewDataType.Align         := alLeft;
      FViewDataType.Width         := 300;

      FViewDataLabel.Align        := alLeft;
      FViewDataLabel.Width        := 60;
      FViewDataLabel.Layout       := tlCenter;

      FUnitsLabel.Top             := 5;
      FUnitsLabel.Left            := 370;
      FUnitsLabel.Width           := 60;
      FUnitsLabel.Font.Style      := [fsBold];

      FbtnDataSelection.Align     := alRight;
      FbtnDataSelection.Width     := 80;

      FDataGrid.Align             := alClient;
    finally
      LockWindowUpdate(0);
    end;
    inherited;
  except on E: Exception do HandleError(E, OPNAME); end;

end;

procedure TOutputWRPMGridDialog.DoExport(AFileName: string = '');
const OPNAME = 'TOutputWRPMGridDialog.DoExport';
begin
  try
    if FDataGrid.Visible then FDataGrid.DoExport(AFileName);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TOutputWRPMGridDialog.DoPrint;
const OPNAME = 'TOutputWRPMGridDialog.DoPrint';
begin
  try
    if FDataGrid.Visible then FDataGrid.DoPrint('');
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TOutputWRPMGridDialog.CanExport: boolean;
const OPNAME = 'TOutputWRPMGridDialog.CanExport';
begin
  Result := False;
  try
    Result := (Assigned(FDataGrid)            and (FDataGrid.Visible));
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TOutputWRPMGridDialog.CanPrint: boolean;
const OPNAME = 'TOutputWRPMGridDialog.CanPrint';
begin
  Result := False;
  try
    Result := (Assigned(FDataGrid)            and (FDataGrid.Visible));
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TOutputWRPMGridDialog.BeforeDrawindACell(Sender: TObject; ACol, ARow: Integer; Rect: TRect; State: TGridDrawState);
begin
  if(FDataGrid.FixedRows = 0) then
    FDataGrid.FixedRows := 1;
end;

end.

