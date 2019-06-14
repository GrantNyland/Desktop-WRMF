//
//  UNIT      : Contains TTableProperty Class
//  AUTHOR    : Grant Nyland (PDNA)
//  DATE      : 2002/03/16
//  COPYRIGHT : Copyright © 2002 DWAF
//
unit UTableProperty;

interface

uses
  Classes,
  UAbstractObject;

type
  TTableProperty = class(TAbstractTableProperty)
  protected
    FTableName: string;
  public
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    procedure Reset;
    procedure SetMemberVariables(ATableName, APrimaryKeys: string);
    property TableName: string read FTableName;
  end;

implementation

uses
  SysUtils,
  UErrorHandlingOperations;

procedure TTableProperty.CreateMemberObjects;
const OPNAME = 'TTableProperty.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
    FPrimaryKeys := TStringList.Create;
    Reset;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TTableProperty.DestroyMemberObjects;
const OPNAME = 'TTableProperty.DestroyMemberObjects';
begin
  try
    Reset;
    FreeAndNil(FPrimaryKeys);
    inherited DestroyMemberObjects;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TTableProperty.Reset;
const OPNAME = 'TTableProperty.Reset';
begin
  try
    FTableName := '';
    FPrimaryKeys.Clear;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TTableProperty.SetMemberVariables(ATableName, APrimaryKeys: string);
const OPNAME = 'TTableProperty.SetMemberVariables';
begin
  try
    FTableName := ATableName;
    FPrimaryKeys.CommaText := APrimaryKeys;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
