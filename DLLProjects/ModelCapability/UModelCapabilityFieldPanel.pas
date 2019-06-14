//
//
//  UNIT      : Contains the class TModelCapabilityFieldPanel.
//  AUTHOR    : Presley Mudau
//  DATE      : 2003/11/18
//  COPYRIGHT : Copyright © 2003 DWAF
//
//

unit UModelCapabilityFieldPanel;

interface

uses
  VCL.Grids,
  Classes,
  VCL.Controls,
  VCL.ComCtrls,
  VCL.ExtCtrls,
  VCL.StdCtrls,
  VCL.Buttons,
  VCL.Graphics,
  UAbstractObject,
  UAbstractComponent,
  UDataEditComponent,
  UDataComponent,
  UAbstractModelCapabilityPanel;

type

  TModelCapabilityFieldPanel = class(TAbstractModelCapabilityPanel)
  protected
    procedure CreateMemberObjects; override;
  public
    procedure AssignHelpContext; override;
    procedure Resize; override;
    function LanguageHasChanged: boolean; override;
  end;

  implementation

uses
  SysUtils,
  UHelpContexts,
  UErrorHandlingOperations;

{ TModelCapabilityFieldPanel }

procedure TModelCapabilityFieldPanel.CreateMemberObjects;
const OPNAME = 'TModelCapabilityFieldPanel.CreateMemberObjects';
begin
  inherited;
   try
    FDataGrid.ColCount := 5;
   except on E: Exception do HandleError(E, OPNAME) end;
end;

function TModelCapabilityFieldPanel.LanguageHasChanged: boolean;
const OPNAME = 'TModelCapabilityFieldPanel.LanguageHasChanged';
begin
  Result := True;
  try
    Caption                      := FAppModules.Language.GetString('TabCaption.ModelCapabilityField');
    FDataGrid.Rows[0].Strings[0] := FAppModules.language.GetString('GridHeading.FieldName');
    FDataGrid.Rows[0].Strings[1] := FAppModules.language.GetString('GridHeading.FieldDescription');
    FDataGrid.Rows[0].Strings[2] := FAppModules.language.GetString('GridHeading.FileName');
    FDataGrid.Rows[0].Strings[3] := FAppModules.language.GetString('GridHeading.MinimumValue');
    FDataGrid.Rows[0].Strings[4] := FAppModules.language.GetString('GridHeading.MaximumValue');
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TModelCapabilityFieldPanel.Resize;
const OPNAME = 'TModelCapabilityFieldPanel.Resize';
var
  LStartWidth: integer;
begin
  inherited;
  try
    LStartWidth := FDataGrid.ClientWidth;
    FDataGrid.ColWidths[4] := 0;
    FDataGrid.ColWidths[0] := (FDataGrid.ClientWidth * 25) div 100;
    FDataGrid.ColWidths[1] := (FDataGrid.ClientWidth * 32) div 100;
    FDataGrid.ColWidths[2] := (FDataGrid.ClientWidth * 14) div 100;
    FDataGrid.ColWidths[3] := (FDataGrid.ClientWidth * 14) div 100;
    FDataGrid.ColWidths[4] := LStartWidth - (FDataGrid.ColWidths[0] + FDataGrid.ColWidths[1] + FDataGrid.ColWidths[2] + FDataGrid.ColWidths[3] + 5 );
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TModelCapabilityFieldPanel.AssignHelpContext;
const OPNAME = 'TModelCapabilityFieldPanel.AssignHelpContext';
begin
   try
     SetControlHelpContext(FDataGrid,       HC_YieldCapacityTest);
   except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
