//
//
//  UNIT      : Contains TStudyDocumentsDataset Class
//  AUTHOR    : Grant Nyland (PDNA)
//  DATE      : 2002/02/27
//  COPYRIGHT : Copyright © 2002 DWAF
//
//
unit UStudyDocumentsDataset;

interface

uses
  UModelDataset;

type
  TStudyDocumentsMenuDataset = class(TModelDataset)
  protected
    function GetSelectClause: string; override;
    function GetFromClause: string; override;
    function GetWhereClause: string; override;
    function GetGroupOrOrderClause: string; override;
  public
    function DataSetType: integer; override;
  end;

  TStudyDocumentsLinksDataset = class(TModelDataset)
  protected
    function GetSelectClause: string; override;
    function GetFromClause: string; override;
    function GetWhereClause: string; override;
  public
    function DataSetType: integer; override;
  end;

implementation

uses
  SysUtils, UDataSetType, UErrorHandlingOperations;

{ TStudyDocumentsMenuDataset }

function TStudyDocumentsMenuDataset.DataSetType: integer;
const OPNAME = 'TStudyDocumentsMenuDataset.DataSetType';
begin
  Result := integer(dtStudyDocumentsMenu);
end;

function TStudyDocumentsMenuDataset.GetSelectClause: string;
const OPNAME = 'TStudyDocumentsMenuDataset.GetSelectClause';
begin
  Result := '';
  try
    Result :=
      'SELECT                                  ' +
      '  StudyAreaName,                        ' +
      '  Category,                             ' +
      '  Identifier,                           ' +
      '  Filename,                             ' +
      '  MenuCaption                           ' ;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TStudyDocumentsMenuDataset.GetFromClause: string;
const OPNAME = 'TStudyDocumentsMenuDataset.GetFromClause';
begin
  Result := '';
  try
    Result :=
      'FROM                                    ' +
      '  StudyDocuments                        ';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TStudyDocumentsMenuDataset.GetWhereClause: string;
const OPNAME = 'TStudyDocumentsMenuDataset.GetWhereClause';
begin
  Result := '';
  try
    Result :=
      'WHERE                                   ' +
      '  StudyAreaName = :AStudyAreaCode       ';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TStudyDocumentsMenuDataset.GetGroupOrOrderClause: string;
const OPNAME = 'TStudyDocumentsMenuDataset.GetGroupOrOrderClause';
begin
  Result := '';
  try
    Result :=  
      'ORDER BY                                ' +
      '  StudyAreaName, Category, Identifier;  ';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{ TStudyDocumentsLinksDataset }

function TStudyDocumentsLinksDataset.DataSetType: integer;
const OPNAME = 'TStudyDocumentsLinksDataset.DataSetType';
begin
  Result := integer(dtStudyDocumentsLinks);
end;

function TStudyDocumentsLinksDataset.GetSelectClause: string;
const OPNAME = 'TStudyDocumentsLinksDataset.GetSelectClause';
begin
  Result := '';
  try
    Result :=
      'SELECT                                                     ' +
      '  A.StudyAreaName,                                         ' +
      '  A.Model,                                                 ' +
      '  A.SubArea,                                               ' +
      '  A.Scenario,                                              ' +
      '  A.Category,                                              ' +
      '  A.Identifier,                                            ' +
      '  B.Filename,                                              ' +
      '  B.MenuCaption,                                           ' +
      '  A.Bookmark,                                              ' +
      '  A.PageNumber                                             ' ;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TStudyDocumentsLinksDataset.GetFromClause: string;
const OPNAME = 'TStudyDocumentsLinksDataset.GetFromClause';
begin
  Result := '';
  try
    Result :=
      'FROM                                                       ' +
      '  StudyScenarioDocuments A, StudyDocuments B               ';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TStudyDocumentsLinksDataset.GetWhereClause: string;
const OPNAME = 'TStudyDocumentsLinksDataset.GetWhereClause';
begin
  Result := '';
  try
    Result :=
      'WHERE                                                      ' +
      '  A.StudyAreaName = B.StudyAreaName  AND                   ' +
      '  A.Category = B.Category            AND                   ' +
      '  A.Identifier = B.Identifier                              ' ;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
