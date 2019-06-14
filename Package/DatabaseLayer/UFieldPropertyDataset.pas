//
//
//  UNIT      : Contains TFieldPropertyDataset Class
//  AUTHOR    : Dziedzi Ramulondi(PDNA)
//  DATE      : 03/13/2001
//  COPYRIGHT : Copyright © 2002 DWAF
//
//
unit UFieldPropertyDataset;

interface

uses
  UModelDataset;

type
  TFieldPropertyDataset = class(TModelDataset)
  protected
    function GetSelectClause: string; override;
    function GetFromClause: string; override;
    function GetGroupOrOrderClause: string; override;
  public
    function DataSetType: integer; override;
  end;

implementation

uses SysUtils, UDataSetType, UErrorHandlingOperations;

{ TFieldPropertyDataset }

function TFieldPropertyDataset.DataSetType: integer;
const OPNAME = 'TFieldPropertyDataset.DataSetType';
begin
  Result := integer(dtFieldProperty);
end;

function TFieldPropertyDataset.GetSelectClause: string;
const OPNAME = 'TFieldPropertyDataset.GetSelectClause';
begin
  Result := '';
  try
    Result :=
      ' SELECT                            ' +
      '   TableFields.Name,               ' +
      '   TableFields.Type,               ' +
      '   TableFields.DataType,           ' +
      '   TableFields.Width,              ' +
      '   TableFields.FormatStringGrid,   ' +
      '   TableFields.FormatStringGraph,  ' +
      '   TableFields.IsIndex,            ' +
      '   TableFields.IsEditable,         ' +
      '   TableFields.IsDerived,          ' +
      '   TableFields.FieldUnits,         ' +
      '   TableFields.FieldDescription,   ' +
      '   TableFields.FieldSource,        ' +
      '   FieldUpdateSQL.FieldName,       ' +
      '   FieldUpdateSQL.StepNo,          ' +
      '   FieldUpdateSQL.NameOfTable,     ' +
      '   FieldUpdateSQL.FieldInTable,    ' +
      '   FieldUpdateSQL.UpdateSQL,       ' +
      '   TableName.PrimaryKeys           ';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFieldPropertyDataset.GetFromClause: string;
const OPNAME = 'TFieldPropertyDataset.GetFromClause';
begin
  Result := '';
  try
    Result :=
      ' FROM               ' +
      ' (TableFields LEFT JOIN FieldUpdateSQL ON TableFields.Name = FieldUpdateSQL.FieldName) ' +
      '    LEFT JOIN TableName ON FieldUpdateSQL.NameOfTable = TableName.NameOfTable  ';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFieldPropertyDataset.GetGroupOrOrderClause: string;
const OPNAME = 'TFieldPropertyDataset.GetGroupOrOrderClause';
begin
  Result := '';
  try
    Result :=
      ' ORDER BY                    ' +
      '   FieldUpdateSQL.FieldName, ' +
      '   FieldUpdateSQL.StepNo     ' ;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
