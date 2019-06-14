//
//
//  UNIT      : Contains TViewDataDataset Class
//  AUTHOR    : Grant Nyland (PDNA)
//  DATE      : 2002/02/06
//  COPYRIGHT : Copyright © 2002 DWAF
//
//
unit UViewDataDataset;

interface

uses
  UModelDataset;

type
  TViewDataSetsDataset = class(TModelDataset)
  protected
    function GetSelectClause: string; override;
    function GetFromClause: string; override;
    function GetGroupOrOrderClause: string; override;
  public
    function DataSetType: integer; override;
  end;
  TViewDataNodesDataset = class(TModelDataset)
  protected
    function GetSelectClause: string; override;
    function GetFromClause: string; override;
    function GetWhereClause: string; override;
    function GetGroupOrOrderClause: string; override;
  public
    function DataSetType: integer; override;
  end;
  TViewDataJumpsDataset = class(TModelDataset)
  protected
    function GetSelectClause: string; override;
    function GetFromClause: string; override;
    function GetGroupOrOrderClause: string; override;
  public
    function DataSetType: integer; override;
  end;

implementation

uses SysUtils, UDataSetType, UErrorHandlingOperations;

{ TViewDataSetsDataset }

function TViewDataSetsDataset.DataSetType: integer;
const OPNAME = 'TViewDataSetsDataset.DataSetType';
begin
  Result := integer(dtViewDataSets);
end;

function TViewDataSetsDataset.GetSelectClause: string;
const OPNAME = 'TViewDataSetsDataset.GetSelectClause';
begin
  Result := '';
  try
    Result :=
      ' SELECT          ' +
      '   DatasetID,    ' +
      '   Editable,     ' +
      '   SQLType,      ' +
      '   NoDataMessage,' +
      '   ViewSQL       ' ;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TViewDataSetsDataset.GetFromClause: string;
const OPNAME = 'TViewDataSetsDataset.GetFromClause';
begin
  Result := '';
  try
    Result := ' FROM ViewDataSet ';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TViewDataSetsDataset.GetGroupOrOrderClause: string;
const OPNAME = 'TViewDataSetsDataset.GetGroupOrOrderClause';
begin
  Result := '';
  try
    Result := ' ORDER BY DatasetID '
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{ TViewDataNodesDataset }

function TViewDataNodesDataset.DataSetType: integer;
const OPNAME = 'TViewDataNodesDataset.DataSetType';
begin
  Result := integer(dtViewDataNodes);
end;

function TViewDataNodesDataset.GetSelectClause: string;
const OPNAME = 'TViewDataNodesDataset.GetSelectClause';
begin
  Result := '';
  try
    Result :=
      ' SELECT                ' +
      '   Model,              ' +
      '   ParentID,           ' +
      '   ViewID,             ' +
      '   Weighting,          ' +
      '   ShowSQL,            ' +
      '   BitmapName,         ' +
      '   DatasetID,          ' +
      '   SubNodesDatasetID   ' ;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TViewDataNodesDataset.GetFromClause: string;
const OPNAME = 'TViewDataNodesDataset.GetFromClause';
begin
  Result := '';
  try
    Result := ' FROM ViewDataNode ';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TViewDataNodesDataset.GetWhereClause: string;
const OPNAME = 'TViewDataNodesDataset.GetWhereClause';
begin
  Result := '';
  try
    Result := ' WHERE Model = :AModelCode ';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TViewDataNodesDataset.GetGroupOrOrderClause: string;
const OPNAME = 'TViewDataNodesDataset.GetGroupOrOrderClause';
begin
  Result := '';
  try
    Result := ' ORDER BY Weighting '
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{ TViewDataJumpsDataset }

function TViewDataJumpsDataset.DataSetType: integer;
const OPNAME = 'TViewDataJumpsDataset.DataSetType';
begin
  Result := integer(dtViewDataJumps);
end;

function TViewDataJumpsDataset.GetSelectClause: string;
const OPNAME = 'TViewDataJumpsDataset.GetSelectClause';
begin
  Result := '';
  try
    Result :=
      ' SELECT      ' +
      '   ViewID,   ' +
      '   JumpToID  ' ;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TViewDataJumpsDataset.GetFromClause: string;
const OPNAME = 'TViewDataJumpsDataset.GetFromClause';
begin
  Result := '';
  try
    Result := ' FROM ViewDataJump ';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TViewDataJumpsDataset.GetGroupOrOrderClause: string;
const OPNAME = 'TViewDataJumpsDataset.GetGroupOrOrderClause';
begin
  Result := '';
  try
    Result := ' ORDER BY ViewID '
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
