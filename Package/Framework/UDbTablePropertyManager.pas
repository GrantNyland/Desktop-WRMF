//
//  UNIT      : Contains TDbTablePropertyManager Class
//  AUTHOR    : Philemon Setshedi(PDNA)
//  DATE      : 2004/03/23
//  COPYRIGHT : Copyright © 2004 DWAF
//
unit UDbTablePropertyManager;

interface
uses
  Classes,
  Contnrs,
  UDbTablePropertyList,
  UDbTableProperty,
  UAbstractObject;

type
  TDbTablePropertyManager = class(TAbstractDBTablePropertyManager)
  protected
    FTableProperties : TDbTablePropertyList;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    function GetTablePropertyByName(ATableName: string): TAbstractDbTableProperty; override;
    function GetTablePropertyByIndex(AIndex: integer): TAbstractDbTableProperty; override;
    function GetTablePropertyCount : integer; override;
  public
    function Initialise: boolean; override;
    function GetTablesPerModel(AModel: string; AContainer: TObjectList): boolean; override;
    function GetTablesPerGroup( AGroup : integer; AContainer : TObjectList ) : boolean; override;
    property TablePropertyByName[ATableName: string]: TAbstractDbTableProperty read GetTablePropertyByName;
    property TablePropertyByIndex[AIndex: integer]: TAbstractDbTableProperty read GetTablePropertyByIndex;
    property TablesCount : integer read GetTablePropertyCount;
  end;
implementation

uses
  SysUtils,
  UErrorHandlingOperations;

{ TDbTablePropertyManager }

procedure TDbTablePropertyManager.CreateMemberObjects;
const OPNAME = 'TDbTablePropertyManager.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
    FTableProperties := TDbTablePropertyList.Create(FAppModules);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDbTablePropertyManager.DestroyMemberObjects;
const OPNAME = 'TDbTablePropertyManager.DestroyMemberObjects';
begin
  try
    FreeAndNil(FTableProperties);
    inherited DestroyMemberObjects;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDbTablePropertyManager.Initialise: boolean;
const OPNAME = 'TDbTablePropertyManager.Initialise';
begin
  Result := inherited Initialise;
  try
    Result := FTableProperties.Initialise;;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDbTablePropertyManager.GetTablePropertyByIndex(AIndex: integer): TAbstractDbTableProperty;
const OPNAME = 'TDbTablePropertyManager.GetTablePropertyByIndex';
begin
  Result := nil;
  try
    Result := FTableProperties.TablePropertyByIndex[AIndex];
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDbTablePropertyManager.GetTablePropertyByName(ATableName: string): TAbstractDbTableProperty;
const OPNAME = 'TDbTablePropertyManager.GetTablePropertyByName';
begin
  Result := nil;
  try
    Result := FTableProperties.TablePropertyByName[ATableName];
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDbTablePropertyManager.GetTablePropertyCount: integer;
const OPNAME = 'TDbTablePropertyManager.GetTablePropertyCount';
begin
  Result := -1;
  try
    Result := FTableProperties.TableCount;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDbTablePropertyManager.GetTablesPerModel(AModel: string;
         AContainer: TObjectList): boolean;
const OPNAME = 'TDbTablePropertyManager.GetTablesPerModel';
var
  LIndex: integer;
begin
  Result := False;
  try
    if Assigned(AContainer) then
    begin
      AContainer.Clear;
      for LIndex := 0 to TablesCount -1 do
      begin
        if (TablePropertyByIndex[LIndex].ModelNames.IndexOf(AModel) >= 0 ) then
          AContainer.Add(TablePropertyByIndex[LIndex])
      end;
      Result := True;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDbTablePropertyManager.GetTablesPerGroup ( AGroup : integer;
         AContainer: TObjectList): boolean;
const OPNAME = 'TDbTablePropertyManager.GetTablesPerGroup';
var
  LIndex: integer;
begin
  Result := False;
  try
    if Assigned(AContainer) then
    begin
      AContainer.Clear;
      for LIndex := 0 to TablesCount -1 do
      begin
        if (TablePropertyByIndex[LIndex].TableGroup = AGroup ) then
          AContainer.Add(TablePropertyByIndex[LIndex])
      end;
      Result := True;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
