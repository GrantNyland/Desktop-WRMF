//
//
//  UNIT      : Contains     Class
//  AUTHOR    : Sam Dhlamini(Cornastone)
//  DATE      : 21/06/2005
//  COPYRIGHT : Copyright © 2004 DWAF
//
//


unit UDbTableFieldsList;

interface

uses
  Classes,
  UDbTableField,
  UAbstractObject;

type
  TTableFieldsDefList = class ( TAbstractTableFieldsDefList )
  protected
    FTableFieldsDefContainer : TStringList;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    procedure AddTableFields ( ATableName : string; AFieldNames : String;
                               AFieldType : String; AFieldLength : integer );
    function GetTableFieldsDefByName ( ATableName : string ) : TAbstractTableFieldsDef; override;
    function GetTableFieldsDefByIndex ( AIndex: integer ) : TAbstractTableFieldsDef; override;
    function GetTableFieldsDefCount  : integer; override;
  public
    procedure Reset;
    function Initialise : boolean; override;
    property TableFieldsDefByIndex [ AIndex: integer ] : TAbstractTableFieldsDef read GetTableFieldsDefByIndex;
    property TableFieldsDefByName [ ATableName : string ] : TAbstractTableFieldsDef read GetTableFieldsDefByName;
    property TableFieldsDefCount : integer read  GetTableFieldsDefCount;
  end;

implementation

uses
  SysUtils,
  UConstants,
  UErrorHandlingOperations;

procedure TTableFieldsDefList.CreateMemberObjects;
const OPNAME = 'TTableFieldsDefList.CreateMemberObjects';
begin
  try
    FTableFieldsDefContainer := TStringList.Create;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TTableFieldsDefList.DestroyMemberObjects;
const OPNAME = 'TTableFieldsDefList.DestroyMemberObjects';
begin
  inherited DestroyMemberObjects;
  try
    FreeAndNil ( FTableFieldsDefContainer );
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TTableFieldsDefList.AddTableFields ( ATableName : string; AFieldNames : String;
                                              AFieldType : String; AFieldLength : integer );
const OPNAME = 'TTableFieldsDefList.AddTableFields';
var
  LTable : TTableFieldsDef;
  LField : TTableFieldDef;
begin
  try
    if(FTableFieldsDefContainer.IndexOf(ATableName) >= 0) then
      LTable := TTableFieldsDef(FTableFieldsDefContainer.Objects[FTableFieldsDefContainer.IndexOf(ATableName)])
    else
    begin
      LTable := TTableFieldsDef.Create;
      FTableFieldsDefContainer.AddObject(ATableName,LTable);
    end;

    LField := TTableFieldDef.Create;
    LField.SetMemberVariables( ATableName, AFieldNames, AFieldType, AFieldLength );
    LTable.AddTableFieldDef(LField);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TTableFieldsDefList.Initialise: boolean;
const OPNAME = 'TTableFieldsDefList.Initialise';
begin
  Result := False;
  try
    Reset;
    LoadTableFieldsData ( AddTableFields );
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TTableFieldsDefList.Reset;
const OPNAME = 'TTableFieldsDefList.Reset';
var LIndex: integer;
begin
  try
    for LIndex := 0 to FTableFieldsDefContainer.Count - 1 do
    begin
      FTableFieldsDefContainer.Objects [ LIndex ].Free;
      FTableFieldsDefContainer.Objects [ LIndex ] := nil;
    end;
    FTableFieldsDefContainer.Clear;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TTableFieldsDefList.GetTableFieldsDefByName ( ATableName : string ): TAbstractTableFieldsDef;
const OPNAME = 'TTableFieldsDefList.GetTableFieldsDefByName';
var
  LIndex : integer;
begin
  Result := nil;
  try
    if (Trim(ATableName) <> '' ) then
    begin
      LIndex := FTableFieldsDefContainer.IndexOf(ATableName);
      if(LIndex >= 0) then
      begin
        Result := TAbstractTableFieldsDef(FTableFieldsDefContainer.Objects[ LIndex ]);
      end
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TTableFieldsDefList.GetTableFieldsDefByIndex(AIndex: integer): TAbstractTableFieldsDef;
const OPNAME = 'TTableFieldsDefList.GetTableFieldsDefByIndex';
begin
  Result := nil;
  try
   if(AIndex >= 0) and (AIndex < FTableFieldsDefContainer.Count) then
     Result := TAbstractTableFieldsDef(FTableFieldsDefContainer.Objects[AIndex]);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TTableFieldsDefList.GetTableFieldsDefCount: integer;
const OPNAME = 'TTableFieldsDefList.GetTableFieldsDefCount';
begin
  Result := NullInteger;
  try
    Result := FTableFieldsDefContainer.Count;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
