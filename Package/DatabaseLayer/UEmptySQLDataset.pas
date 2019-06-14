//
//
//  UNIT      : Contains TEmptySQLDataset Class
//  AUTHOR    : Dziedzi Ramulondi(PDNA)
//  DATE      : 05/12/2001
//  COPYRIGHT : Copyright © 2001 DWAF
//
//
unit UEmptySQLDataset;

interface

uses
  UModelDataset;

type
  TEmptySQLDataset = class(TModelDataset)
  protected
    function GetSelectClause: string; override;
  public
    function DataSetType: integer; override;
  end;

implementation

uses SysUtils, UDataSetType, UErrorHandlingOperations;

function TEmptySQLDataset.DataSetType: integer;
const OPNAME = 'TEmptySQLDataset.DataSetType';
begin
  Result := integer(dtExecSQL);
end;

function TEmptySQLDataset.GetSelectClause: string;
const OPNAME = 'TEmptySQLDataset.GetSelectClause';
begin
  Result := '';
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
