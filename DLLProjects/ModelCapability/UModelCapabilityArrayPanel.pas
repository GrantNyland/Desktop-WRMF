//
//
//  UNIT      : Contains the class TModelCapabilityArrayPanel.
//  AUTHOR    : Presley Mudau
//  DATE      : 2003/11/18
//  COPYRIGHT : Copyright © 2003 DWAF
//
//

unit UModelCapabilityArrayPanel;

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

  TModelCapabilityArrayPanel = class(TAbstractModelCapabilityPanel)
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


{ TModelCapabilityArrayPanel }

function TModelCapabilityArrayPanel.LanguageHasChanged: boolean;
const OPNAME = 'TModelCapabilityArrayPanel.LanguageHasChanged';
begin
  Result := True;
  try
    Caption := FAppModules.Language.GetString('TabCaption.ModelCapabilityArray');
    FDataGrid.Rows[0].Strings[0] := FAppModules.language.GetString('GridHeading.ModelFieldName');
    FDataGrid.Rows[0].Strings[1] := FAppmodules.language.GetString('GridHeading.ModelFieldDescription');
    FDataGrid.Rows[0].Strings[2] := FAppmodules.language.GetString('GridHeading.FileName');
    FDataGrid.Rows[0].Strings[3] := FAppmodules.language.GetString('GridHeading.ArrayLength');
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TModelCapabilityArrayPanel.Resize;
const OPNAME = 'TModelCapabilityArrayPanel.Resize';
var
  LStartWidth: integer;
begin
  inherited;
  try
    LStartWidth := FDataGrid.ClientWidth;
    FDataGrid.ColWidths[3] := 0;
    FDataGrid.ColWidths[0] := (FDataGrid.ClientWidth * 40) div 100;
    FDataGrid.ColWidths[1] := (FDataGrid.ClientWidth * 30) div 100;
    FDataGrid.ColWidths[2] := (FDataGrid.ClientWidth * 15) div 100;
    FDataGrid.ColWidths[3] := LStartWidth - (FDataGrid.ColWidths[0] + FDataGrid.ColWidths[1] + FDataGrid.ColWidths[2] + 3);

  except on E: Exception do HandleError(E, OPNAME) end;

end;

procedure TModelCapabilityArrayPanel.AssignHelpContext;
const OPNAME = 'TModelCapabilityArrayPanel.AssignHelpContext';
begin
   try
     SetControlHelpContext(FDataGrid,       HC_YieldCapacityTest);
   except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
