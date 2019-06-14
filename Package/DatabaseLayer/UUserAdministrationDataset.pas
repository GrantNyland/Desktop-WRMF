//
//
//  UNIT      : Contains TUserAdministrationDataset Class
//  AUTHOR    : Presley Mudau
//  DATE      : 2005/09/26
//  COPYRIGHT : Copyright © 2002 DWAF
//
//
unit UUserAdministrationDataset;

interface

uses
  Classes, UModelDataset;

type
  TUserAdministrationDataset = class(TModelDataset)
  protected
    function GetSelectClause: string; override;
    function GetFromClause: string; override;
    function GetWhereClause: string; override;
    function GetGroupOrOrderClause: string; override;
  public
    function DataSetType: integer; override;
  end;

implementation

uses SysUtils, UDataSetType, UErrorHandlingOperations;

function TUserAdministrationDataset.DataSetType: integer;
const OPNAME = 'TUserAdministrationDataset.DataSetType';
begin
  Result := integer(dtUserAdministration);
end;

function TUserAdministrationDataset.GetSelectClause: string;
const OPNAME = 'TUserAdministrationDataset.GetSelectClause';
begin
  Result := '';
  try
    Result :=
      'SELECT              ' +
      '  UserId,           ' +
      '  [Password],         ' +
      '  Initials,         ' +
      '  FirstName,        ' +
      '  SecondName,       ' +
      '  LastName,         ' +
      '  UserRights,       ' +
      '  PreferedLanguage, ' +
      '  UserType,         ' +
      '  Model,            ' +
      '  AutoLogon,        ' +
      '  AutoSelectStudy,  ' +
      '  CreatedBy         ' ;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TUserAdministrationDataset.GetFromClause: string;
const OPNAME = 'TUserAdministrationDataset.GetFromClause';
begin
  Result := '';
  try
    Result := ' FROM Users ';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TUserAdministrationDataset.GetWhereClause: string;
const OPNAME = 'TUserAdministrationDataset.GetWhereClause';
begin
  Result := '';
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TUserAdministrationDataset.GetGroupOrOrderClause: string;
const OPNAME = 'TUserAdministrationDataset.GetGroupOrOrderClause';
begin
  Result := '';
  try
    Result := ' ORDER BY  UserId; ';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
