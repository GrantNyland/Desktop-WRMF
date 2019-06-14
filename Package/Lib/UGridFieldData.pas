//
//
//  UNIT      : Contains TGridFieldData Class
//  AUTHOR    : Grant Nyland (PDNA)
//  DATE      : 2002/03/02
//  COPYRIGHT : Copyright © 2002 DWAF
//
//
unit UGridFieldData;

interface

uses
  Contnrs,
  Classes,
  UAbstractGridData,
  UAbstractObject;

type
  TGridField = class(TAbstractGridField)
  protected
    FFieldData: TObjectList;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    function GetFieldData(ARecordIndex: integer): TAbstractGridFieldData; override;
  public
    procedure AddFieldData(ADisplayText: string; AContextData: TStringList; ASubFieldIndex: integer); override;
    function RecordCount: integer; override;
    procedure DeleteRecord(ARecordIndex: integer); override;
  end;
  TGridData = class(TAbstractGridData)
  protected
    FGridFields: TObjectList;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    function GetGridField(AFieldIndex: integer): TAbstractGridField; override;
    function GetFieldData(AFieldIndex, ARecordIndex: integer): TAbstractGridFieldData; override;
  public
    procedure Clear; override;
    function FieldCount: integer; override;
    function DataIDCommaText: string; override;
    procedure AddGridField(AFieldName: string); override;
    procedure DeleteRecord(ARecordIndex: integer); override;
  end;

implementation

uses
  SysUtils,
  UErrorHandlingOperations;

{ Implementation classes }

type
  TGridFieldData = class(TAbstractGridFieldData)
  public
    constructor Create;
    destructor Destroy; override;
    property FieldProperty: TAbstractFieldProperty read FFieldProperty write FFieldProperty;
  end;

{ TGridFieldData }

constructor TGridFieldData.Create;
const OPNAME = 'TGridFieldData.Create';
begin
  try
    inherited Create;
    FContextData := TStringList.Create;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

destructor TGridFieldData.Destroy;
const OPNAME = 'TGridFieldData.Destroy';
begin
  try
    FreeAndNil(FContextData);
    inherited Destroy;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

{ TGridField }

procedure TGridField.CreateMemberObjects;
const OPNAME = 'TGridField.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
    FFieldData := TObjectList.Create;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TGridField.DestroyMemberObjects;
const OPNAME = 'TGridField.DestroyMemberObjects';
begin
  try
    FreeAndNil(FFieldData);
    inherited DestroyMemberObjects;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TGridField.GetFieldData(ARecordIndex: integer): TAbstractGridFieldData;
const OPNAME = 'TGridField.GetFieldData';
begin
  Result := nil;
  try
    if assigned(FFieldData) then
      if(ARecordIndex >= 0) and (ARecordIndex < FFieldData.Count) then
         Result := TAbstractGridFieldData(FFieldData[ARecordIndex]);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TGridField.RecordCount: integer;
const OPNAME = 'TGridField.RecordCount';
begin
  Result := 0;
  try
    Result := FFieldData.Count;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TGridField.DeleteRecord(ARecordIndex: integer);
const OPNAME = 'TGridField.DeleteRecord';
begin
  try
    if(ARecordIndex >= 0) and (ARecordIndex < FFieldData.Count) then
      FFieldData.Delete(ARecordIndex);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TGridField.AddFieldData(ADisplayText: string; AContextData: TStringList; ASubFieldIndex: integer);
const OPNAME = 'TGridField.AddFieldData';
var
  LGridFieldData: TGridFieldData;
  LSubFieldName: string;
begin
  try
    if Assigned(FAppModules.FieldProperties()) then
    begin
      LGridFieldData := TGridFieldData.Create;
      LGridFieldData.DisplayText := ADisplayText;
      LGridFieldData.ContextData.AddStrings(AContextData);
      if (ASubFieldIndex < 0) then
      begin
        LGridFieldData.FieldProperty := FFieldProperty;
      end else begin
        LSubFieldName := FFieldProperty.FieldName + Format('%2.2d', [ASubFieldIndex]);
        LGridFieldData.FieldProperty := FAppModules.FieldProperties.FieldProperty(LSubFieldName);
      end;
      FFieldData.Add(LGridFieldData);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{ TGridData }

procedure TGridData.CreateMemberObjects;
const OPNAME = 'TGridData.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
    FGridFields := TObjectList.Create;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TGridData.DestroyMemberObjects;
const OPNAME = 'TGridData.DestroyMemberObjects';
begin
  try
    Clear;
    FreeAndNil(FGridFields);
    inherited DestroyMemberObjects;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TGridData.Clear;
const OPNAME = 'TGridData.Clear';
begin
  try
    FGridFields.Clear;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TGridData.GetGridField(AFieldIndex: integer): TAbstractGridField;
const OPNAME = 'TGridData.GetGridField';
begin
  Result := nil;
  try
    if(AFieldIndex >= 0) and (AFieldIndex < FGridFields.Count) then
      Result := TAbstractGridField(FGridFields[AFieldIndex]);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TGridData.GetFieldData(AFieldIndex, ARecordIndex: integer): TAbstractGridFieldData;
const OPNAME = 'TGridData.GetFieldData';
var
  LGridFieldData:TAbstractGridField;
begin
  Result := nil;
  try
    LGridFieldData := GridField[AFieldIndex];
    if(LGridFieldData <> nil) then
      Result := LGridFieldData.FieldData[ARecordIndex];
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TGridData.FieldCount: integer;
const OPNAME = 'TGridData.FieldCount';
begin
  Result := 0;
  try
    Result := FGridFields.Count;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TGridData.DataIDCommaText: string;
const OPNAME = 'TGridData.DataIDCommaText';
begin
  Result := ClassName;
end;

procedure TGridData.AddGridField(AFieldName: string);
const OPNAME = 'TGridData.AddGridField';
var
  LFieldProperty: TAbstractFieldProperty;
  LGridField: TGridField;
begin
  try
    if Assigned(FAppModules.FieldProperties()) then
    begin
      LFieldProperty := FAppModules.FieldProperties.FieldProperty(AFieldName);
      if (not Assigned(LFieldProperty)) then
      begin
        raise Exception.CreateFmt(
          'Could not find a field properties object for field [%s].', [AFieldName]);
      end else begin
        LGridField := TGridField.Create(FAppModules);
        LGridField.FieldProperty := LFieldProperty;
        FGridFields.Add(LGridField);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TGridData.DeleteRecord(ARecordIndex: integer);
const OPNAME = 'TGridData.DeleteRecord';
var LFieldIndex: integer;
begin
  try
    for LFieldIndex := 0 to FGridFields.Count - 1 do
      GridField[LFieldIndex].DeleteRecord(ARecordIndex);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

end.
