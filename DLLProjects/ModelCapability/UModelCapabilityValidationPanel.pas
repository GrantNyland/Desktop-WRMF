//
//
//  UNIT      : Contains the class TModelCapabilityValidationPanel.
//  AUTHOR    : Presley Mudau
//  DATE      : 2003/11/18
//  COPYRIGHT : Copyright © 2003 DWAF
//
//

unit UModelCapabilityValidationPanel;

interface

uses
  VCL.grids,
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

  TModelCapabilityValidationPanel = class(TAbstractModelCapabilityPanel)
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


{ TModelCapabilityValidationPanel }

function TModelCapabilityValidationPanel.LanguageHasChanged: boolean;
const OPNAME = 'TModelCapabilityValidationPanel.LanguageHasChanged';
begin
  Result := True;
  try
    Caption                      := FAppModules.Language.GetString('TabCaption.ModelCapabilityValidations');
    FDataGrid.Rows[0].Strings[0] := FAppModules.language.GetString('GridHeading.ModelFieldName');
    FDataGrid.Rows[0].Strings[1] := FAppModules.language.GetString('GridHeading.ModelFieldDescription');
    FDataGrid.Rows[0].Strings[2] := FAppModules.language.GetString('GridHeading.FileName');
    FDataGrid.Rows[0].Strings[3] := FAppModules.language.GetString('GridHeading.AcceptableValues');
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TModelCapabilityValidationPanel.Resize;
const OPNAME = 'TModelCapabilityValidationPanel.Resize';
var
  LStartWidth: integer;
begin
  inherited;
  try
    LStartWidth := FDataGrid.ClientWidth;
    FDataGrid.ColWidths[3] := 0;
    FDataGrid.ColWidths[0] := (FDataGrid.ClientWidth * 30) div 100;
    FDataGrid.ColWidths[1] := (FDataGrid.ClientWidth * 37) div 100;
    FDataGrid.ColWidths[2] := (FDataGrid.ClientWidth * 15) div 100;
    FDataGrid.ColWidths[3] := LStartWidth - (FDataGrid.ColWidths[0] + FDataGrid.ColWidths[1] + FDataGrid.ColWidths[2]) - 3;

  except on E: Exception do HandleError(E, OPNAME) end;

end;

procedure TModelCapabilityValidationPanel.AssignHelpContext;
const OPNAME = 'TModelCapabilityValidationPanel.AssignHelpContext';
begin
   try
     SetControlHelpContext(FDataGrid,       HC_YieldCapacityTest);
   except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
