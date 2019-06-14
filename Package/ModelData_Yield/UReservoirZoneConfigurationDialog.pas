//
//
//  UNIT      : Contains the class TReservoirZoneElevationsDialog.
//  AUTHOR    : Valentino Naicker (arivia.kom)
//  DATE      : 2003/06/27
//  COPYRIGHT : Copyright © 2003 DWAF
//
//

unit UReservoirZoneConfigurationDialog;

interface

uses
  Classes,
  Controls,
  ComCtrls,
  ExtCtrls,
  StdCtrls,
  UAbstractObject,
  UAbstractComponent,
  UDataComponent;

type

  TReservoirZoneConfigurationDialog = class(TAbstractScrollablePanel)
  protected
    procedure CreateMemberObjects; override;
  public
    procedure Resize; override;
    function Initialise: boolean; override;
    function LanguageHasChanged: boolean; override;
    function SaveState: boolean; override;

  end;

implementation

uses
  SysUtils,
  //UDBConstants,
  UErrorHandlingOperations;

{ TReservoirEvaporationDialog }

procedure TReservoirZoneConfigurationDialog.CreateMemberObjects;
const OPNAME = 'TReservoirZoneConfigurationDialog.CreateMemberObjects';
begin
  inherited CreateMemberObjects;
  try

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirZoneConfigurationDialog.Initialise: boolean;
const OPNAME = 'TReservoirZoneConfigurationDialog.Initialise';
begin
  Result := inherited Initialise;
  try

    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirZoneConfigurationDialog.Resize;
const OPNAME = 'TReservoirZoneConfigurationDialog.Resize';
var
  LClientWidth : integer;
begin
  // Call the ancestor.
  inherited Resize;
  try

  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirZoneConfigurationDialog.LanguageHasChanged: boolean;
const OPNAME = 'TReservoirZoneConfigurationDialog.LanguageHasChanged';
begin
  Result := inherited LanguageHasChanged;
  try

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirZoneConfigurationDialog.SaveState: boolean;
const OPNAME = 'TReservoirZoneConfigurationDialog.SaveState';
begin
  Result := inherited SaveState;
  try
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.






