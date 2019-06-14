//
//
//  UNIT      : Contains the class TAbstractModelCapabilityPanel.
//  AUTHOR    : Dziedzi Ramulondi(Aravia)
//  DATE      : 2004/12/22
//  COPYRIGHT : Copyright © 2004 DWAF
//
//

unit UAbstractModelCapabilityPanel;

interface

uses
  VCL.Grids,
  Classes,
  VCL.Controls,
  VCL.ComCtrls,
  VCL.ExtCtrls,
  VCL.StdCtrls,
  VCL.Graphics,
  UAbstractObject,
  UAbstractComponent,
  UDataEditComponent,
  UDataComponent;

type

  TAbstractModelCapabilityPanel = class(TAbstractScrollablePanel)
  protected
    FCaption   : String;
    FDataGrid: TFieldStringGrid;
    procedure CreateMemberObjects; override;
    procedure OnCellEnter(Sender: TObject; ACol, ARow: Longint; var CanSelect: Boolean);virtual;
  public
    function Initialise: boolean; override;
    procedure ClearDataViewer; virtual;
    property DataGrid : TFieldStringGrid read FDataGrid;
    property Caption: String  read FCaption write FCaption;
  end;

  implementation

uses
  SysUtils,
  UHelpContexts,
  UErrorHandlingOperations;


{ TAbstractModelCapabilityPanel }

procedure TAbstractModelCapabilityPanel.CreateMemberObjects;
const OPNAME = 'TAbstractModelCapabilityPanel.CreateMemberObjects';
begin
  inherited;
  try
    FDataGrid := TFieldStringGrid.Create(ControlsOwner,FAppModules);
    FDataGrid.Parent := ControlsParent;
    FDataGrid.Align := alClient;
    FDataGrid.ColCount  := 4;
    FDataGrid.FixedCols := 0;
    FDataGrid.FixedRows := 1;
    FDataGrid.DefaultRowHeight := 18;
    FDataGrid.Color := clSilver;
    FDataGrid.OnSelectCell := OnCellEnter;
    FDataGrid.Options := FDataGrid.Options - [ goEditing,goRangeSelect];
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TAbstractModelCapabilityPanel.ClearDataViewer;
const OPNAME = 'TAbstractModelCapabilityPanel.ClearDataViewer';
var
  LColIndex,
  LRowIndex: integer;
begin
  inherited;
  try
    for LRowIndex := 1 to FDataGrid.RowCount -1 do
    begin
      for LColIndex := 0 to FDataGrid.ColCount -1 do
      begin
        FDataGrid.Cells[LColIndex,LRowIndex]:= '';
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TAbstractModelCapabilityPanel.OnCellEnter(Sender: TObject; ACol, ARow: Longint;
          var CanSelect: Boolean);
const OPNAME = 'TAbstractModelCapabilityPanel.OnCellEnter';
var
  LGridHintText,
  LFieldName: string;
  LFieldProperty: TAbstractFieldProperty;
begin
    try
      LGridHintText := '';
      LFieldName := FDataGrid.Cells[0, ARow];
      LFieldName := Trim(LFieldName);
      if LFieldName <> '' then
      begin
        LFieldProperty := FAppModules.Fieldproperties.FieldProperty(LFieldName);
        if assigned(LFieldProperty) then
        begin
          LGridHintText := LFieldProperty.FieldDescription;
          LGridHintText := FAppModules.language.GetString(LGridHintText);
        end;
      end;
    self.Hint := LGridHintText;
    ShowCurrentHint;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAbstractModelCapabilityPanel.Initialise: boolean;
const OPNAME = 'TAbstractModelCapabilityPanel.Initialise';
begin
  Result := inherited Initialise;
  try
    FDataGrid.RowCount  := 2;
    ClearDataViewer;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
