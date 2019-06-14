//
//
//  UNIT      : Contains TSQLDatabaseLayer Class
//  AUTHOR    : Grant Nyland (PDNA)
//  DATE      : 2002/02/06
//  COPYRIGHT : Copyright © 2002 DWAF
//
//
unit USQLDatabaseLayer;

interface

uses
  DB,
  Classes,
  Contnrs,
  UAbstractObject,
  UDWADBComponents;

type
  TAbstractDataSetConstructor = class(TAbstractAppObject)
  public
    function CreateDataset(ADataSetType: integer; var ADataSet: TAbstractModelDataset): boolean; virtual; abstract;
  end;
  TSQLDatabaseLayer = class(TAbstractDatabaseLayer)
  protected
    FDataSetConstructors: TObjectList;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    function GetDropTableSQL(ATableName: string): string; virtual;
    function GetCreateTableSQL(ATableName: string; AFieldNames: array of string): string; virtual;
  public
    function CreateDataset(ADataSetType: integer; var ADataSet: TAbstractModelDataset): boolean; override;
    procedure AddDataSetConstructor(ADataSetConstructor: TAbstractDataSetConstructor); virtual;
    procedure DeleteDataSetConstructorsOfType(AClassType: TClass); virtual;
    function CreateQueryDataset: TDWAQuery; virtual; abstract;
    function CreateTableDataset: TDWATable; virtual; abstract;
    function CreateDatasetDataset: TDWADataSet; virtual; abstract;
  end;

implementation

uses
  System.Types,
  UDataSetType,
  SysUtils,
  UErrorHandlingOperations;

procedure TSQLDatabaseLayer.CreateMemberObjects;
const OPNAME = 'TSQLDatabaseLayer.CreateMemberObjects';
begin
  try
    inherited;
    FDataSetConstructors := TObjectList.Create;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TSQLDatabaseLayer.DestroyMemberObjects;
const OPNAME = 'TSQLDatabaseLayer.DestroyMemberObjects';
begin
  try
    FreeAndNil(FDataSetConstructors);
    inherited;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TSQLDatabaseLayer.AddDataSetConstructor(ADataSetConstructor: TAbstractDataSetConstructor);
const OPNAME = 'TSQLDatabaseLayer.AddDataSetConstructor';
begin
  try
    FDataSetConstructors.Add(ADataSetConstructor);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TSQLDatabaseLayer.DeleteDataSetConstructorsOfType(AClassType: TClass);
const OPNAME = 'TSQLDatabaseLayer.DeleteDataSetConstructorsOfType';
var
  LIndex: integer;
  LObject: TObject;
begin
  try
    for LIndex := FDataSetConstructors.Count - 1 downto 0 do
    begin
      if (FDataSetConstructors[LIndex] is AClassType) then
      begin
        LObject := FDataSetConstructors.Extract(FDataSetConstructors[LIndex]);
        FreeAndNil(LObject);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TSQLDatabaseLayer.GetDropTableSQL(ATableName: string): string;
const
  OPNAME = 'TSQLDatabaseLayer.GetDropTableSQL';
begin
  Result := '';
  try
    Result := 'DROP TABLE ' + ATableName + ';';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TSQLDatabaseLayer.GetCreateTableSQL(ATableName: string; AFieldNames: array of string): string;
const
  OPNAME = 'TSQLDatabaseLayer.GetCreateTableSQL';
var
  LIndex: integer;
begin
  Result := '';
  try
    Result := 'CREATE TABLE ' + ATableName + ' (' +  AFieldNames[Low(AFieldNames)];
    for LIndex := Low(AFieldNames) + 1 to High(AFieldNames) do
      Result := Result + ', ' + AFieldNames[LIndex];
    Result := Result + ');';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TSQLDatabaseLayer.CreateDataSet(ADataSetType: integer; var ADataSet: TAbstractModelDataset): boolean;
const OPNAME = 'TSQLDatabaseLayer.CreateDataSet';
var LIndex: integer;
begin
  Result := False;
  try
    ADataSet := nil;
    for LIndex := 0 to FDataSetConstructors.Count - 1 do
      if TAbstractDataSetConstructor(FDataSetConstructors[LIndex]).CreateDataset(ADataSetType, ADataSet) then
        if Assigned(ADataSet) then
          break;
    Result := Assigned(ADataSet);
    if (not Result) then
      raise Exception.CreateFmt('Unknown data set type [%d].', [ADataSetType]);
  except on E: Exception do HandleError(E, OPNAME) end;
end;


end.
