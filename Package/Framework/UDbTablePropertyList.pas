//
//  UNIT      : Contains TDbTablePropertyList Class
//  AUTHOR    : Philemon Setshedi(PDNA)
//  DATE      : 2004/03/23
//  COPYRIGHT : Copyright © 2004 DWAF
//
unit UDbTablePropertyList;

interface

uses
  Classes,
  UDbTableProperty,
  UAbstractObject;

type
  TDbTablePropertyList = class(TAbstractAppObject)
  protected
    FTables: TStringList;

    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;

    procedure Clear;
    procedure AddTableProperty(ATableName, AModelNames : string; ATableIndex : integer;
              AIndexFieldNames: string; AFieldNames: String; ATableGroup : integer;
              ATableFilter: string);

    function GetTablePropertyByName(ATableName: string): TAbstractDbTableProperty;
    function GetTablePropertyByIndex(AIndex: integer): TAbstractDbTableProperty;
    function GetTablePropertyCount : integer;
  public
    function Initialise: boolean; override;
    function TableCountPerGroup(AGroup: integer): integer;
    property TablePropertyByName[ATableName: string]: TAbstractDbTableProperty read GetTablePropertyByName;
    property TablePropertyByIndex[AIndex: integer]: TAbstractDbTableProperty read GetTablePropertyByIndex;
    property TableCount : integer read GetTablePropertyCount;
  end;

implementation

uses
  SysUtils,
  UErrorHandlingOperations;

procedure TDbTablePropertyList.CreateMemberObjects;
const OPNAME = 'TDbTablePropertyList.CreateMemberObjects';
begin
  try
    FTables := TStringList.Create;
    //FTables.Sorted := True;
    //FTables.Duplicates := dupError;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDbTablePropertyList.DestroyMemberObjects;
const OPNAME = 'TDbTablePropertyList.DestroyMemberObjects';
begin
  try
    Clear;
    FreeAndNil(FTables);
    inherited DestroyMemberObjects;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDbTablePropertyList.AddTableProperty(ATableName, AModelNames : string; ATableIndex : integer;
  AIndexFieldNames: string; AFieldNames: String; ATableGroup : integer;ATableFilter      : string);
const OPNAME = 'TDbTablePropertyList.AddTableProperty';
var LTableIndexProperty: TDbTableProperty;
begin
  try
    LTableIndexProperty := TDbTableProperty.Create;
    LTableIndexProperty.SetMemberVariables(ATableName, AModelNames, ATableIndex,
                        AIndexFieldNames, AFieldNames, ATableGroup,ATableFilter);
    FTables.AddObject(ATableName, LTableIndexProperty);

  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDbTablePropertyList.Initialise: boolean;
const OPNAME = 'TDbTablePropertyList.Initialise';
begin
  Result := False;
  try

    // Load the table index property data.
    Clear;
    LoadTablePropertyData(AddTableProperty);

    // Done.
    Result := True;

  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDbTablePropertyList.Clear;
const OPNAME = 'TDbTablePropertyList.Clear';
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

function TDbTablePropertyList.GetTablePropertyByName(ATableName: string): TAbstractDbTableProperty;
const OPNAME = 'TDbTablePropertyList.GetTablePropertyByName';
var LIndex: integer;
begin
  Result := nil;
  try
    if (Trim(ATableName) <> '') then
    begin
      LIndex := FTables.IndexOf(ATableName);
      if (LIndex >= 0) then
        Result := TAbstractDbTableProperty(FTables.Objects[LIndex]);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDbTablePropertyList.GetTablePropertyByIndex(AIndex: integer): TAbstractDbTableProperty;
const OPNAME = 'TDbTablePropertyList.GetTablePropertyByIndex';
begin
  Result := nil;
  try
    if(AIndex >= 0) and (AIndex < FTables.Count) then
      Result := TAbstractDbTableProperty(FTables.Objects[AIndex]);

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDbTablePropertyList.GetTablePropertyCount: integer;
const OPNAME = 'TDbTablePropertyList.GetTablePropertyCount';
begin
  Result := -1;
  try
    Result := FTables.Count;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDbTablePropertyList.TableCountPerGroup(AGroup: integer): integer;
const OPNAME = 'TDbTablePropertyList.TableCountPerGroup';
var LIndex: integer;
begin
  Result := 0;
  try
      for LIndex := 0 to FTables.Count -1 do
        if(TAbstractDbTableProperty(FTables.Objects[LIndex]).TableGroup = AGroup) then
          Result := Result + 1;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
