//
//
//  UNIT      : Contains TUserDataset Class
//  AUTHOR    : Grant Nyland (PDNA)
//  DATE      : 2002/02/06
//  COPYRIGHT : Copyright © 2002 DWAF
//
//
unit UUserDataset;

interface

uses
  Classes, UModelDataset;

type
  TUserDataset = class(TModelDataset)
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

function TUserDataset.DataSetType: integer;
const OPNAME = 'TUserDataset.DataSetType';
begin
  Result := integer(dtUser);
end;

function TUserDataset.GetSelectClause: string;
const OPNAME = 'TUserDataset.GetSelectClause';
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

function TUserDataset.GetFromClause: string;
const OPNAME = 'TUserDataset.GetFromClause';
begin
  Result := '';
  try
    Result := ' FROM Users ';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TUserDataset.GetWhereClause: string;
const OPNAME = 'TUserDataset.GetWhereClause';
begin
  Result := '';
  try
    Result := '';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TUserDataset.GetGroupOrOrderClause: string;
const OPNAME = 'TUserDataset.GetGroupOrOrderClause';
begin
  Result := '';
  try
    Result := ' ORDER BY  UserId; ';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
