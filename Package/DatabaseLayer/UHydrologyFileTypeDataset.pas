//
//
//  UNIT      : Contains THydrologyFileTypeDataset Class
//  AUTHOR    : Dziedzi Ramulondi (PDNA)
//  DATE      : 2002/05/10
//  COPYRIGHT : Copyright © 2001 DWAF
//
//
unit UHydrologyFileTypeDataset;

interface

uses
  UModelDataset;

type
  THydrologyFileTypeDataset = class(TModelDataset)
  protected
    function GetSelectClause: string; override;
    function GetFromClause: string; override;
  public
    function DataSetType: integer; override;
  end;

implementation

uses SysUtils, UDataSetType, UErrorHandlingOperations;

function THydrologyFileTypeDataset.DataSetType: integer;
const OPNAME = 'THydrologyFileTypeDataset.DataSetType';
begin
  Result := integer(dtHydrologyFileType);
end;

function THydrologyFileTypeDataset.GetSelectClause: string;
const OPNAME = 'THydrologyFileTypeDataset.GetSelectClause';
begin
  Result := '';
  try
    Result :=
      'SELECT         ' +
      '  FileType,    ' +
      '  FileExtention,    ' +
      '  EFileName,    ' +
      '  FileTypeDescr      ';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydrologyFileTypeDataset.GetFromClause: string;
const OPNAME = 'THydrologyFileTypeDataset.GetFromClause';
begin
  Result := '';
  try
    Result :=
      ' FROM          ' +
      '  FileTypes ';

  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
