{******************************************************************************}
{*  UNIT      : UFieldUpdateSQL_ChangeLists.                                  *}
{*  AUTHOR    : Riana Steyn                                                   *}
{*  DATE      : 2005/02/15                                                    *}
{*  COPYRIGHT : Copyright © 2005 DWAF                                         *}
{******************************************************************************}

unit UFieldUpdateSQL_ChangeLists;

interface

type TFieldUpdateSQLStepItemAddFunction = procedure (
  AStepNo: integer; AFieldName, ATableName, AFieldInTable, AUpdateSQL, AGetValueSQL: string) of object;

procedure LoadFieldPropertyUpdateSQLSteps(AAdd: TFieldUpdateSQLStepItemAddFunction);

implementation

uses
  UFieldUpdateSQLCommonClauses;

const
  CChangeList =
    ' FROM ChangeList A WHERE A.ChangeListID = :AChangeListID';

  CChangeGroup =
    ' FROM ChangeGroup A ' +
    ' WHERE A.GroupID = :AGroupID';

  CChangeGroupElement =
    ' FROM ChangeGroupElement A ' +
    ' WHERE A.GroupID        = :AGroupID' +
    '   AND A.ElementID      = :AElementID' +
    '   AND A.IsElementGroup = :AIsElementGroup';

  CChangeParameter =
    ' FROM ChangeParameter A ' +
    'WHERE (A.ChangeListID = :AChangeListID)' +
    '  AND (A.ParamField   = :AParamField)' +
    '  AND (A.KeyValues    = :AKeyValues)' +
    '  AND (A.FieldIndex   = :AFieldIndex)';

  CMetaDataItem =
    ' FROM MetaDataItem A ' +
    'WHERE (A.MetaDataListID = :AMetaDataListID) ' +
    '  AND (A.ParamField     = :AParamField)' +
    '  AND (A.KeyValues      = :AKeyValues)' +
    '  AND (A.FieldIndex     = :AFieldIndex)';

procedure LoadFieldPropertyUpdateSQLSteps(AAdd: TFieldUpdateSQLStepItemAddFunction);
const OPNAME = 'LoadFieldPropertyUpdateSQLSteps';
begin
{
    procedure AddUpdateSQLStep(
      AStepNo: integer; AFieldName, ATableName, AFieldInTable, AUpdateSQL, AGetValueSQL: string);
    const OPNAME = 'AddUpdateSQLStep';
}
  AAdd(0, 'ChangeGroupName',   'ChangeGroup'    ,'GroupName',
       'SELECT * '     + CChangeGroup, '');

  AAdd(0, 'ElementOrder',      'ChangeGroupElement'    ,'ElementOrder',
       'SELECT * '  + CChangeGroupElement, '');
  AAdd(0, 'ElementActive',     'ChangeGroupElement'    ,'ElementActive',
       'SELECT * ' + CChangeGroupElement, '');

  AAdd(0, 'ChangeListName',        'ChangeList'    ,'ListName',
       'SELECT * '    + CChangeList, '');
  AAdd(0, 'ChangeListCreatedBy',   'ChangeList'    ,'CreatedBy',
       'SELECT * '   + CChangeList, '');
  AAdd(0, 'ChangeListDescription', 'ChangeList'    ,'ListDescr',
       'SELECT * '   + CChangeList, '');
  AAdd(0, 'ChangeParamAbsolut',    'ChangeParameter' ,'Absolut',
       'SELECT * '     + CChangeParameter, '');
  AAdd(0, 'ChangeParamChange' ,    'ChangeParameter' ,'Change',
       'SELECT *  '     + CChangeParameter, '');
  AAdd(0, 'ChangeParamDescr' ,    'ChangeParameter' ,'ParamDescr',
       'SELECT *  '     + CChangeParameter, '');

  AAdd(0, 'MetaDataCreatedBy',   'MetaDataItem'    ,'CreatedBy',
       'SELECT * '   + CMetaDataItem, '');
  AAdd(0, 'MetaDataDateCreated', 'MetaDataItem'    ,'DateCreated',
       'SELECT * ' + CMetaDataItem, '');
  AAdd(0, 'MetaDataComment',     'MetaDataItem'    ,'Comment',
       'SELECT * '     + CMetaDataItem, '');

end;

end.



