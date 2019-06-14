//
//
//  UNIT      : Contains the class TInputGridDialog.
//  AUTHOR    : Dziedzi Ramulondi (ARIVIA)
//  DATE      : 2005/05/03
//  COPYRIGHT : Copyright © 2005 DWAF
//
//

unit UInputGridDialog;

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

  TInputGridDialog = class(TAbstractScrollablePanel)
  protected
    FDataGrid          : TFieldStringGrid;
    procedure CreateMemberObjects; override;
    procedure AssignHelpContext; override;
  public
    procedure Resize;override;
    function StudyHasChanged: boolean; override;
    function LanguageHasChanged: boolean; override;
    function Initialise: boolean; override;
    property grdDataGrid        : TFieldStringGrid        read FDataGrid;
  end;

  implementation

uses
  SysUtils,
  VCL.Graphics,
  UHelpContexts,
  UControlCreationUtilities,
  UErrorHandlingOperations;



{ TInputGridDialog }

procedure TInputGridDialog.CreateMemberObjects;
const OPNAME = 'TInputGridDialog.CreateMemberObjects';
begin
  inherited CreateMemberObjects;
  try
    FDataGrid                   := TFieldStringGrid.Create(ControlsOwner, FAppModules);
    FDataGrid.Parent            := ControlsParent;
    FDataGrid.FixedCols         := 0;
    FDataGrid.FixedRows         := 0;
    FDataGrid.DisabledColor     := clBtnFace;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TInputGridDialog.Initialise: boolean;
const OPNAME = 'TInputGridDialog.Initialise';
begin
  Result := inherited Initialise;
  try
    FDataGrid.Initialise;
    FDataGrid.ColCount := 1;
    FDataGrid.RowCount := 1;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TInputGridDialog.StudyHasChanged: boolean;
const OPNAME = 'TInputGridDialog.StudyHasChanged';
begin
  Result := inherited StudyHasChanged;
  try
    FDataGrid.StudyHasChanged;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TInputGridDialog.LanguageHasChanged: boolean;
const OPNAME = 'TInputGridDialog.LanguageHasChanged';
begin
  Result := inherited LanguageHasChanged;
  try
    FDataGrid.LanguageHasChanged;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TInputGridDialog.AssignHelpContext;
const OPNAME = 'TInputGridDialog.AssignHelpContext';
begin
  try
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TInputGridDialog.Resize;
const OPNAME = 'TInputGridDialog.Resize';
begin
  inherited;
  try
    FDataGrid.Align := alClient;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

end.
