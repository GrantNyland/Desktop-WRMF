//
//  UNIT      : Contains TTablePropertyList Class
//  AUTHOR    : Grant Nyland (PDNA)
//  DATE      : 2002/03/16
//  COPYRIGHT : Copyright © 2002 DWAF
//
unit UTablePropertyList;

interface

uses
  Classes,
  UTableProperty,
  UAbstractObject;

type
  TTablePropertyList = class(TAbstractAppObject)
  protected
    FTables: TStringList;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    procedure Clear;
    procedure AddTableProperty(ATableName, APrimaryKeys: string);
    function GetTableProperty(ATableName: string): TTableProperty;
  public
    function Initialise: boolean; override;
    property TableProperty[ATableName: string]: TTableProperty read GetTableProperty; default;
  end;

implementation

uses
  SysUtils,
  UTableData,
  UErrorHandlingOperations;

procedure TTablePropertyList.CreateMemberObjects;
const OPNAME = 'TTablePropertyList.CreateMemberObjects';
begin
  try
    FTables := TStringList.Create;
    FTables.Sorted := True;
    FTables.Duplicates := dupIgnore;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TTablePropertyList.DestroyMemberObjects;
const OPNAME = 'TTablePropertyList.DestroyMemberObjects';
begin
  try
    Clear;
    FreeAndNil(FTables);
    inherited DestroyMemberObjects;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TTablePropertyList.Clear;
const OPNAME = 'TTablePropertyList.Clear';
var LIndex: integer;
begin
  try
    for LIndex := 0 to FTables.Count - 1 do
    begin
      FTables.Objects[LIndex].Free;
      FTables.Objects[LIndex] := nil;
    end;
    FTables.Clear;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TTablePropertyList.GetTableProperty(ATableName: string): TTableProperty;
const OPNAME = 'TTablePropertyList.GetTableProperty';
var LIndex: integer;
begin
  Result := nil;
  try
    if (Trim(ATableName) <> '') then
    begin
      LIndex := FTables.IndexOf(ATableName);
      if (LIndex >= 0) then
        Result := TTableProperty(FTables.Objects[LIndex]);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TTablePropertyList.Initialise: boolean;
const OPNAME = 'TTablePropertyList.Initialise';
begin
  Result := False;
  try

    // Load the table property data.
    Clear;
    LoadTablePropertyData(AddTableProperty);

    // Done.
    Result := True;

  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TTablePropertyList.AddTableProperty(ATableName, APrimaryKeys: string);
const OPNAME = 'TTablePropertyList.AddTableProperty';
var LTableProperty: TTableProperty;
begin
  try

    // Set the basic values.
    LTableProperty := TTableProperty.Create;
    LTableProperty.SetMemberVariables(ATableName, APrimaryKeys);

    // Add the object to the list.
    FTables.AddObject(ATableName, LTableProperty);

  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
