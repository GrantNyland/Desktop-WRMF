//
//
//  UNIT      : Contains TLanguageTextDataset Class
//  AUTHOR    : Dziedzi Ramulondi(PDNA)
//  DATE      : 05/12/2001
//  COPYRIGHT : Copyright © 2001 DWAF
//
//
unit ULanguageDataset;

interface

uses
  UModelDataset;

type
  TAvailableLanguagesDataset = class(TModelDataset)
  protected
    function GetSelectClause: string; override;
    function GetFromClause: string; override;
    function GetGroupOrOrderClause: string; override;
  public
    function DataSetType: integer; override;
  end;
  TLanguageTextDataset = class(TModelDataset)
  protected
    function GetSelectClause: string; override;
    function GetFromClause: string; override;
    function GetWhereClause: string; override;
  public
    function DataSetType: integer; override;
  end;

implementation

uses SysUtils, UDataSetType, UErrorHandlingOperations;

{ TAvailableLanguagesDataset }

function TAvailableLanguagesDataset.DataSetType: integer;
const OPNAME = 'TAvailableLanguagesDataset.DataSetType';
begin
  Result := integer(dtAvailableLanguage);
end;

function TAvailableLanguagesDataset.GetSelectClause: string;
const OPNAME = 'TAvailableLanguagesDataset.GetSelectClause';
begin
  Result := '';
  try
    Result :=
      'SELECT       ' +
      '  LangID,    ' +
      '  LangDescr  ';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAvailableLanguagesDataset.GetFromClause: string;
const OPNAME = 'TAvailableLanguagesDataset.GetFromClause';
begin
  Result := '';
  try
    Result :=
      'FROM         ' +
      '  Language   ';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAvailableLanguagesDataset.GetGroupOrOrderClause: string;
const OPNAME = 'TAvailableLanguagesDataset.GetGroupOrOrderClause';
begin
  Result := '';
  try
    Result :=  
      'ORDER BY     ' +
      '  LangID;    ';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{ TLanguageTextDataset }

function TLanguageTextDataset.DataSetType: integer;
const OPNAME = 'TLanguageTextDataset.DataSetType';
begin
  Result := integer(dtLanguageText);
end;

function TLanguageTextDataset.GetSelectClause: string;
const OPNAME = 'TLanguageTextDataset.GetSelectClause';
begin
  Result := '';
  try
    Result :=
      'SELECT         ' +
      '  LangID,      ' +
      '  CONTEXT,     ' +
      '  STRCONSTANT, ' +
      '  STRTEXT      ';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TLanguageTextDataset.GetFromClause: string;
const OPNAME = 'TLanguageTextDataset.GetFromClause';
begin
  Result := '';
  try
    Result :=
      ' FROM          ' +
      '  LanguageText ';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TLanguageTextDataset.GetWhereClause: string;
const OPNAME = 'TLanguageTextDataset.GetWhereClause';
begin
  Result := '';
  try
    Result :=
      ' WHERE          ' +
      '  LangID =:CurrentLanguage ';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
