//
//
//  UNIT      : Contains TStudyAreaDataset Class
//  AUTHOR    : Grant Nyland (PDNA)
//  DATE      : 2002/02/06
//  COPYRIGHT : Copyright © 2002 DWAF
//
//
unit UStudyAreaDataset;

interface

uses
  UModelDataset;

type
  TStudyAreaDataset = class(TModelDataset)
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

function TStudyAreaDataset.DataSetType: integer;
const OPNAME = 'TStudyAreaDataset.DataSetType';
begin
  Result := integer(dtSelectStudyArea);
end;

function TStudyAreaDataset.GetSelectClause: string;
const OPNAME = 'TStudyAreaDataset.GetSelectClause';
begin
  Result := '';
  try
    Result :=
      ' SELECT                                      ' +
      '   Mdl.Model,                                ' +
      '   Mdl.ModelLabel,                           ' +
      '   Mdl.ModelDescr,                           ' +
//      '   Std.Model,                                ' +
      '   Std.StudyAreaName,                        ' +
      '   Std.StudyDate,                            ' +
      '   Std.Consultant,                           ' +
      '   Std.Client,                               ' +
      '   Std.StudyNumber,                          ' +
      '   Std.StudyLabel,                           ' +
      '   Std.StudyAreaDescr,                       ' +
      '   Std.ShapeFileName,                        ' +
//      '   Sub.Model,                                ' +
//      '   Sub.StudyAreaName,                        ' +
      '   Sub.SubArea,                              ' +
      '   Sub.SubAreaLabel,                         ' +
      '   Sub.SubAreaDescr,                         ' +
      '   Sub.ShapeFileName As SubAreaShapeFileName,' +
      '   Sub.TopLeftCoord ,                        ' +
      '   Sub.TopRightCoord ,                       ' +
      '   Sub.BottomLeftCoord ,                     ' +
      '   Sub.BottomRightCoord ,                    ' +
//      '   Scn.Model,                                ' +
//      '   Scn.StudyAreaName,                        ' +
//      '   Scn.SubArea,                              ' +
      '   Scn.Scenario,                             ' +
      '   Scn.ScenarioLabel,                        ' +
      '   Scn.ScenarioDescr,                        ' +
      '   Scn.DataFilesPrefix,                      ' +
      '   Scn.DataFilesPath,                        ' +
      '   Scn.FilesLoaded,                          ' +
      '   Scn.CalenderStartMonth,                   ' +
      '   Scn.Version,                              ' +
      '   Scn.DataImported                          ';

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TStudyAreaDataset.GetFromClause: string;
const OPNAME = 'TStudyAreaDataset.GetFromClause';
begin
  Result := '';
  try
    Result :=
      ' FROM                                        ' +
      '   StudyArea Std,                            ' +
      '   StudyModel Mdl,                           ' +
      '   StudySubArea Sub,                         ' +
      '   StudyScenario Scn                         ';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TStudyAreaDataset.GetWhereClause: string;
const OPNAME = 'TStudyAreaDataset.GetWhereClause';
begin
  Result := '';
  try
    Result :=
      ' WHERE                                       ' +
      '   Std.Model = Mdl.Model AND                 ' +
      '   Sub.Model = Std.Model AND                 ' +
      '   Sub.StudyAreaName = Std.StudyAreaName AND ' +
      '   Scn.Model = Sub.Model AND                 ' +
      '   Scn.StudyAreaName = Sub.StudyAreaName AND ' +
      '   Scn.SubArea = Sub.SubArea                 ';

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TStudyAreaDataset.GetGroupOrOrderClause: string;
const OPNAME = 'TStudyAreaDataset.GetGroupOrOrderClause';
begin
  Result := '';
  try
    Result :=
      ' ORDER BY                                    ' +
      '   Std.StudyAreaName,                        ' +
      '   Std.Model,                                ' +
      '   Sub.SubArea,                              ' +
      '   Scn.Scenario;                             ' ;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
