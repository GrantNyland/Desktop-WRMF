{******************************************************************************}
{*  UNIT      : Contains the class TChangeManager.                            *}
{*  AUTHOR    : Riana Steyn                                                   *}
{*  DATE      : 2005/02/10                                                    *}
{*  COPYRIGHT : Copyright © 2005 DWAF                                         *}
{******************************************************************************}

unit UChangeManager;

interface

uses
  Classes,
  Vcl.ComCtrls,
  UAbstractObject,
  UMenuItemManager,
  UChangeMenuItemManager,
  UChangeData,
  UChangeAdminTabSheet,
  UChangeListValidator,
  UChangeParameterForm,
  UAbstractModelObjects,
  VoaimsCom_TLB;

type
  TChangeManager = class(TAbstractChangeManager)
  protected
    FMasterGroup         : TChangeGroup;
    FChangeGroupsList    : TList;
    FChangeListsList     : TStringList; //TList;
    FModelChangeLists    : TStringList;
    FAllParamChanges     : TStringList;
    FMenuItemManager     : TChangeMenuItemManager;
    FChangeAdminTabSheet : TChangeAdminTabSheet;
    FChangeListValidator : TChangeListValidator;
    FSystemFlag          : boolean;
    FShowAllChangeLists  : boolean;
    FChangeParameterForm : TChangeParameterForm;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    procedure ClearChangeGroups;
    procedure ClearChangeLists;
    procedure RefreshModelChangeLists;
    function LoadChanges : boolean;
    function LoadChangeLists : boolean;
    function LoadParamChange : boolean;
    function LoadChangeGroups : boolean;
    function SetGroupParents : boolean;
    function ModelSupportsChangeLists:boolean;

    function CreateChangeGroup : TChangeGroup;
    function DeleteChangeGroupWithID (AGroupID : integer) : boolean;
    function CastChangeGroupWithID (AGroupID : integer) : TChangeGroup;

    function CreateChangeList(AListID : integer)  : TChangeList;
    function DeleteChangeListWithID (AListID : integer) : boolean;
    function CastChangeListWithID (AListID : integer) : TChangeList;
    function GetLastChangeGroupID: integer;
    function DBCreateNewChangeGroup (AGroupID  : integer;
                                     AName    : string) : Boolean;
    function DBDeleteChangeGroup (AGroupID : integer): boolean;
    function GetLastChangeListID: integer;
    function DBCreateNewChangeList (AListID  : integer;
                                    AListKey : string;
                                    AName    : string;
                                    ADate    : TDateTime;
                                    APerson  : string;
                                    ADescr   : string) : Boolean;
    function DBDeleteChangeList (AListID : integer): boolean;
    function GetShowAllChangeLists : boolean; override;
    procedure SetShowAllChangeLists (AShowAll : boolean); override;
    function GetParameterValueFloat (AFieldProperty : TAbstractFieldProperty;
                                     AKeyValues     : string;
                                     ABaseValue     : double;
                                     AFieldIndex    : string) : double;
    function GetParameterValueInt (AFieldProperty : TAbstractFieldProperty;
                                   AKeyValues     : string;
                                   ABaseValue     : integer;
                                   AFieldIndex    : string) : integer;
    function GetParameterValueStr (AParamField : string;
                                   AKeyValues  : string;
                                   ABaseValue  : string;
                                   AFieldIndex : string) : string;
    procedure PopulateDescendantsList (AGroupID : integer;
                                       AGroups  : TStringList;
                                       ALists   : TStringList);
  procedure ParameterChangeAdded(AParamField, AKeyValues, AFieldIndex : WideString; AParameterChange : TParameterChange);
  procedure ParameterChangeDeleted(AParamField, AKeyValues, AFieldIndex : WideString; AParameterChange : TParameterChange);
  public
    function Initialise: boolean; override;
    function LanguageHasChanged: boolean; override;
    function StudyHasChanged: boolean; override;
    function StudyDataHasChanged(AContext: TChangeContext; AFieldName,AOldValue,ANewValue: string): boolean; override;
    function ProcessMetaDataEvent : boolean; override;
    procedure TabHasChanged; override;
    function ChangeGroups : TList; override;
    function ChangeGroupWithID (AGroupID : integer) : IChangeGroup; override;
    function ChangeGroupWithIndex (AIndex : integer) : IChangeGroup; override;
    function ChangeLists : TStringList{TList}; override;
    function Get_MasterGroup : IChangeGroup; override;
    procedure GetChangeListIDsInOrder (AGroupID : integer;
                                       ALists   : TStringList); override;
    function IsChangeListActive (AListID : integer) : WordBool; override;
    function ChangeListWithID (AListID : integer) : IChangeList; override;
    function ChangeListWithIndex (AIndex : integer) : IChangeList; override;
    function DoCreateNewChangeGroup : IChangeGroup; override;
    function DoDeleteChangeGroup (AChangeGroupID : integer) : WordBool; override;
    function MayCreateNewChangeGroupElement (AOldParentID    : integer;
                                             ANewParentID    : integer;
                                             AElementID      : integer;
                                             AIsElementGroup : WordBool;
                                             var AErrorMsg   : string) : WordBool; override;
    function DoCreateNewChangeGroupElement (AParentGroupID  : integer;
                                            AElementID      : integer;
                                            AIsElementGroup : WordBool) : IChangeGroupElement; override;
    function DoDeleteChangeGroupElement (AParentGroupID  : integer;
                                         AElementID      : integer;
                                         AIsElementGroup : WordBool) : WordBool; override;
    function DoCreateNewChangeList : IChangeList; override;
    function DoDeleteChangeList (AChangeListID : integer) : WordBool; override;
    function DoCopyChangeList (AChangeListID : integer) : IChangeList; override;
    function DoMoveUpChangeElement (AParentGroupID  : integer;
                                    AElementID      : integer;
                                    AIsElementGroup : Boolean) : WordBool; override;
    function DoMoveDownChangeElement (AParentGroupID  : integer;
                                      AElementID      : integer;
                                      AIsElementGroup : Boolean) : WordBool; override;
    function DoActivateChangeElement (AParentGroupID  : integer;
                                      AElementID      : integer;
                                      AIsElementGroup : Boolean) : WordBool; override;
    function DoDeactivateChangeElement (AParentGroupID  : integer;
                                        AElementID      : integer;
                                        AIsElementGroup : Boolean) : WordBool; override;
    function DoApplyChangeList (AChangeListID : integer) : WordBool; override;
    function DoImportChangeList (AChangeListID : integer) : WordBool; override;
    function DoExportChangeList (AChangeListID : integer) : WordBool; override;
    function DoStationFilter(AChangeListID : integer) : WordBool; override;
    procedure SetCreateNewChangeGroup (AEnabled : boolean); override;
    procedure SetDeleteChangeGroup (AEnabled : boolean); override;
    procedure SetCreateNewChangeList (AEnabled : boolean); override;
    procedure SetDeleteChangeList (AEnabled : boolean); override;
    procedure SetCopyChangeList (AEnabled : boolean); override;
    procedure SetMoveUpChangeElement (AEnabled : boolean); override;
    procedure SetMoveDownChangeElement (AEnabled : boolean); override;
    procedure SetActivateChangeElement (AEnabled : boolean); override;
    procedure SetDeactivateChangeElement (AEnabled : boolean); override;
    procedure SetApplyChangeList (AEnabled : boolean); override;
    procedure SetImportChangeList (AEnabled : boolean); override;
    procedure SetExportChangeList (AEnabled : boolean); override;
    procedure SetStationFilter (AEnabled : boolean); override;
    procedure SetParameterChanges (AEnabled : boolean); override;
    procedure ShowParameterChanges (AParamField     : string;
                                    AKeyValues      : string;
                                    AFieldIndex     : string); override;
    procedure AddTabsheetToPageControl; override;
    procedure RemoveTabsheetFromPageControl; override;
    procedure GetIndexes (AFieldIndex  : string;
                          var lDim1Idx : integer;
                          var lDim2Idx : integer); override;
    function GetKeyValue (AKeyName   : string;
                          AKeyValues : string) : string; override;
    function GetParameterValue (AParamField : string;
                                AKeyValues  : string;
                                ABaseValue  : string;
                                AFieldIndex : string) : string; override;
    function HasParamChange (AParamField : WideString;
                             AKeyValues  : WideString;
                             AFieldIndex : WideString) : boolean; override;
    function EntityDescription (AFieldPropName : string;
                                AKeyValues     : string;
                                AFieldIndex    : string) : string; override;
    property MasterGroup  : IChangeGroup read Get_MasterGroup;
  end;

implementation

uses
  System.UITypes,
  DBClient,
  Vcl.Controls,
  Provider,
  SysUtils,
  UUtilities,
  UDataSetType,
  UConstants,
  UDBConstants,
  Vcl.Dialogs,
  Vcl.Forms,
  DateUtils,
  UExportChangeListForm,
  UErrorHandlingOperations;

{******************************************************************************}
{* TChangeManager                                                             *}
{******************************************************************************}

procedure TChangeManager.CreateMemberObjects;
const OPNAME = 'TChangeManager.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
    FSystemFlag          := FALSE;
    FShowAllChangeLists  := TRUE;
    FChangeGroupsList    := TList.Create;
    FChangeListsList     := TStringList.Create; //TList.Create;
    FModelChangeLists    := TStringList.Create;
    FMasterGroup         := TChangeGroup.Create(FAppModules);
    FAllParamChanges     := TStringList.Create;
    FChangeListValidator := nil;
    FChangeAdminTabSheet := nil;
    FChangeParameterForm := nil;
    FMenuItemManager     := nil;
    if(FAppModules.MainForm <> nil) and (FAppModules.MainForm.MainForm <> nil) then
    begin
      FChangeListValidator := TChangeListValidator.Create(nil,FAppModules);
      FChangeAdminTabSheet := TChangeAdminTabSheet.Create(nil, FAppModules);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TChangeManager.DestroyMemberObjects;
const OPNAME = 'TChangeManager.DestroyMemberObjects';
begin
  try
    ClearChangeGroups;
    ClearChangeLists;
    FreeAndNil(FModelChangeLists);
    FreeAndNil(FMasterGroup);
    FreeAndNil(FChangeGroupsList);
    FreeAndNil(FChangeListsList);
    FreeAndNil(FAllParamChanges);
    if Assigned(FChangeListValidator) then
    begin
//      FChangeListValidator.Panel.Parent := nil;
      FreeAndNil(FChangeListValidator);
    end;

    if Assigned(FChangeAdminTabSheet) then
    begin
      FChangeAdminTabSheet.Parent := nil;
      FreeAndNil(FChangeAdminTabSheet);
    end;
    if Assigned(FMenuItemManager) then
      FreeAndNil(FMenuItemManager);
    inherited DestroyMemberObjects;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TChangeManager.Initialise: boolean;
const OPNAME = 'TChangeManager.Initialise';
begin
  Result := FALSE;
  try
    FAllParamChanges.Sorted     := True;
    FAllParamChanges.Duplicates := dupIgnore;
    FMasterGroup.Initialise;
    FMasterGroup.Populate(0, 'MasterGroup');
    ClearChangeGroups;
    ClearChangeLists;
    if (Assigned(FAppModules.MainForm()) AND Assigned(FAppModules.MainForm.PageControl)) then
    begin
      if Assigned(FChangeAdminTabSheet) then
        FChangeAdminTabSheet.Initialise;
      if Assigned(FChangeListValidator) then
        FChangeListValidator.Initialise;
    end;
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TChangeManager.AddTabsheetToPageControl;
const OPNAME = 'TChangeManager.AddTabsheetToPageControl';
begin
  try
    if ModelSupportsChangeLists then
    begin
      if Assigned(FChangeAdminTabSheet) then
        FChangeAdminTabSheet.Validator := nil;
      if (Assigned(FAppModules.MainForm()) AND
          Assigned(FAppModules.MainForm.PageControl))  and
          (Assigned(FChangeAdminTabSheet)) and
          (not Assigned(FChangeAdminTabSheet.PageControl)) then
      begin
        FChangeAdminTabSheet.PageControl := FAppModules.Mainform.PageControl;
        FChangeAdminTabSheet.PageIndex   := 0;
        if Assigned(FChangeListValidator) then
        begin
          FChangeListValidator.Panel.Parent := FChangeAdminTabSheet;
          FChangeListValidator.Panel.Align  := alClient;
          FChangeAdminTabSheet.Validator    := FChangeListValidator;
        end;
        FMenuItemManager     := TChangeMenuItemManager.Create(FAppModules);
        FMenuItemManager.Initialise;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TChangeManager.RemoveTabsheetFromPageControl;
const OPNAME = 'TChangeManager.RemoveTabsheetFromPageControl';
begin
  try
    if Assigned(FChangeAdminTabSheet) then
      FChangeAdminTabSheet.PageControl := nil;
    if Assigned(FChangeListValidator) then
      FChangeListValidator.Panel.Parent := nil;
    FreeAndNil(FMenuItemManager);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TChangeManager.ClearChangeLists;
const OPNAME = 'TChangeManager.ClearChangeLists';
{var
  lChangeList : TChangeList;}
begin
  try
    FModelChangeLists.Clear;
    FAllParamChanges.Clear;
{    while (FChangeListsList.Count > 0) do
    begin
      lChangeList := TChangeList(FChangeListsList.Objects[0]);//TChangeList(FChangeListsList.Items[0]);
      FChangeListsList.Delete(0);
      FreeAndNil(lChangeList);
    end;
    }
    FChangeListsList.Clear;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TChangeManager.ClearChangeGroups;
const OPNAME = 'TChangeManager.ClearChangeGroups';
var
  lChangeGroup : TChangeGroup;
begin
  try
    FMasterGroup.ClearGroupElements;
    while (FChangeGroupsList.Count > 0) do
    begin
      lChangeGroup := TChangeGroup(FChangeGroupsList.Items[0]);
      FChangeGroupsList.Delete(0);
      FreeAndNil(lChangeGroup);
    end;
    FChangeGroupsList.Clear;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TChangeManager.LoadChanges : boolean;
const OPNAME = 'TChangeManager.LoadChanges';
begin
  Result := FALSE;
  try
    if ModelSupportsChangeLists then
    begin
      if (LoadChangeLists) then
        LoadParamChange;
      LoadChangeGroups;
      RefreshModelChangeLists;
      SetGroupParents;
    end;
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TChangeManager.RefreshModelChangeLists;
const OPNAME = 'TChangeManager.RefreshModelChangeLists';
begin
  try
    FModelChangeLists.Clear;
    GetChangeListIDsInOrder(0, FModelChangeLists);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TChangeManager.LoadChangeLists : boolean;
const OPNAME = 'TChangeManager.LoadChangeLists';
var
  LDataSet      : TAbstractModelDataset;
  lChangeList    : TChangeList;
  lChangeListID  : integer;
  lSQL           : string;
begin
  Result := FALSE;
  try
    ClearChangeLists;
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      if (Assigned(LDataSet)) then
      begin
        lSQL := 'SELECT Model,StudyAreaName,SubArea,Scenario,ChangeListID,ListName,DateCreated,CreatedBy,'+
                ' ListDescr,ChangeListKey'+
                ' FROM ChangeList '+
                ' WHERE  '+ AppModules.Model.GetChangeListWhereClause+
                ' AND (ChangeListID IS NOT NULL)';
        LDataSet.SetSQL(lSQL);
        LDataSet.DataSet.Open;
        while (NOT LDataSet.DataSet.EOF) do
        begin
          lChangeListID  := LDataSet.DataSet.FieldByName('ChangeListID').AsInteger;
          lChangeList    := CreateChangeList(LChangeListID);
          if (lChangeList <> nil) then
          begin
            lChangeList.Populate
                          (lChangeListID,
                           Trim(LDataSet.DataSet.FieldByName('ChangeListKey').AsString),
                           True,
                           Trim(LDataSet.DataSet.FieldByName('ListName').AsString),
                           LDataSet.DataSet.FieldByName('DateCreated').AsDateTime,
                           Trim(LDataSet.DataSet.FieldByName('CreatedBy').AsString),
                           Trim(LDataSet.DataSet.FieldByName('ListDescr').AsString));
          end;
          LDataSet.DataSet.Next;
        end;
        LDataSet.DataSet.Close;
      end;
    finally
      LDataSet.Free;
    end;
    Result := (FChangeListsList.Count > 0);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TChangeManager.LoadParamChange : boolean;
const OPNAME = 'TChangeManager.LoadParamChange';
var
  LDataSet : TAbstractModelDataset;
  LParamChange : TParameterChange;
  LChangeList : TChangeList;
  LChangeListID : integer;
  LSQL : string;
  LIndex : integer;
begin
  Result := False;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      LSQL := ' SELECT Model,StudyAreaName,SubArea,Scenario,ChangeListID,ParamField,KeyValues,'+
              ' FieldIndex,Absolut,Change,ParamDescr,Filtered'+
              ' FROM ChangeParameter'+
                ' WHERE  '+ AppModules.Model.GetChangeListWhereClause+
                ' AND (ChangeListID IS NOT NULL)'+
                ' AND (ParamField IS NOT NULL)'+
                ' AND (KeyValues IS NOT NULL)'+
                ' AND (FieldIndex IS NOT NULL)';
      LDataSet.SetSQL(LSQL);
      LDataSet.DataSet.Open;
      while (NOT LDataSet.DataSet.EOF) do
      begin
        LChangeListID := LDataSet.DataSet.FieldByName('ChangeListID').AsInteger;
        LIndex        := FChangeListsList.IndexOf(IntToStr(LChangeListID));
        if(LIndex >= 0) then
        begin
          LChangeList  := TChangeList(FChangeListsList.Objects[LIndex]);
          LParamChange := LChangeList.CreateParamChange(Trim(LDataSet.DataSet.FieldByName('ParamField').AsString),
                                                        Trim(LDataSet.DataSet.FieldByName('KeyValues').AsString),
                                                        Trim(LDataSet.DataSet.FieldByName('FieldIndex').AsString));
          LParamChange.Populate(LDataSet.DataSet.FieldByName('ChangeListID').AsInteger,
                                Trim(LDataSet.DataSet.FieldByName('ParamField').AsString),
                                Trim(LDataSet.DataSet.FieldByName('KeyValues').AsString),
                                Trim(LDataSet.DataSet.FieldByName('FieldIndex').AsString),
                                (Trim(LDataSet.DataSet.FieldByName('Absolut').AsString) = 'Y'),
                                Trim(LDataSet.DataSet.FieldByName('Change').AsString),
                                Trim(LDataSet.DataSet.FieldByName('ParamDescr').AsString),
                                (Trim(LDataSet.DataSet.FieldByName('Filtered').AsString) = 'Y'));
          ParameterChangeAdded(LParamChange.ParamField,LParamChange.KeyValues,LParamChange.FieldIndex, LParamChange);
        end;
        LDataSet.DataSet.Next;
      end;
      LDataSet.DataSet.Close;
    finally
      FreeAndNil(LDataSet);
    end;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TChangeManager.LoadChangeGroups : boolean;
const OPNAME = 'TChangeManager.LoadChangeGroups';
var
  LDataSet       : TAbstractModelDataset;
  LDataSetA       : TAbstractModelDataset;
  lChangeGroup   : TChangeGroup;
  lChangeGroupID : integer;
  lGroupName     : string;
  lSQL           : string;
  lElement       : TChangeGroupElement;
begin
  Result := FALSE;
  try
    ClearChangeGroups;
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSetA);
    try
      lSQL := 'SELECT * FROM ChangeGroup';
      LDataSet.SetSQL(lSQL);
      LDataSet.DataSet.Open;
      Result := True;

      while (NOT LDataSet.DataSet.EOF) do
      begin
        lChangeGroupID := LDataSet.DataSet.FieldByName('GroupID').AsInteger;
        lGroupName     := Trim(LDataSet.DataSet.FieldByName('GroupName').AsString);
        lChangeGroup   := CreateChangeGroup;
        if (lChangeGroup <> nil) then
        begin
          lChangeGroup.Populate(lChangeGroupID, lGroupName);

          lSQL := 'SELECT * FROM ChangeGroupElement ' +
                  ' WHERE  '+ AppModules.Model.GetChangeListWhereClause+
                  ' AND GroupID            =  '+IntToStr(lChangeGroupID) +
                  ' ORDER BY ElementOrder';
          LDataSetA.SetSQL(lSQL);
          LDataSetA.DataSet.Open;
          while (NOT LDataSetA.DataSet.EOF) do
          begin
            lElement := lChangeGroup.CreateChangeGroupElement;
            if (lElement <> nil) then
            begin
              lElement.Populate
                             (lChangeGroupID,
                              LDataSetA.DataSet.FieldByName('ElementID').AsInteger,
                              (Trim(LDataSetA.DataSet.FieldByName('IsElementGroup').AsString) = 'Y'),
                              LDataSetA.DataSet.FieldByName('ElementOrder').AsInteger,
                              (Trim(LDataSetA.DataSet.FieldByName('ElementActive').AsString) = 'Y')
                              );
            end;
            LDataSetA.DataSet.Next;
          end;
          LDataSetA.DataSet.Close;
        end;
        LDataSet.DataSet.Next;
      end;
      LDataSet.DataSet.Close;

      lSQL := 'SELECT * FROM ChangeGroupElement'+
              ' WHERE  '+ AppModules.Model.GetChangeListWhereClause+
              ' AND GroupID = 0 ' +
              ' ORDER BY ElementOrder';
      LDataSet.SetSQL(lSQL);
      LDataSet.DataSet.Open;
      while (NOT LDataSet.DataSet.EOF) do
      begin
        lElement := FMasterGroup.CreateChangeGroupElement;
        if (lElement <> nil) then
        begin
          lElement.Populate
                         (0,
                          LDataSet.DataSet.FieldByName('ElementID').AsInteger,
                          (Trim(LDataSet.DataSet.FieldByName('IsElementGroup').AsString) = 'Y'),
                          LDataSet.DataSet.FieldByName('ElementOrder').AsInteger,
                          (Trim(LDataSet.DataSet.FieldByName('ElementActive').AsString) = 'Y')
                          );
        end;
        LDataSet.DataSet.Next;
      end;
      LDataSet.DataSet.Close;
    finally
      LDataSet.Free;
    end;
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TChangeManager.GetLastChangeGroupID: integer;
const OPNAME = 'TChangeManager.GetLastChangeGroupID';
var
  lDataSet : TAbstractModelDataset;
  lSQL     : string;
begin
  Result := 0;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), lDataSet);
    try
      if Assigned(lDataSet) then
      begin
        lDataset.DataSet.Close;
        lSQL := 'SELECT Max(GroupID) AS LastID FROM ChangeGroup';
        lDataSet.SetSQL(lSQL);
        lDataSet.Dataset.Open;
        if (NOT lDataSet.DataSet.Eof) then
          Result := LDataset.Dataset.FieldByName('LastID').AsInteger;
        lDataSet.DataSet.Close;
      end;
    finally
      LDataset.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TChangeManager.GetLastChangeListID: integer;
const OPNAME = 'TChangeManager.GetLastChangeListID';
var
  lDataSet : TAbstractModelDataset;
  lSQL     : string;
begin
  Result := 0;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), lDataSet);
    try
      if Assigned(lDataSet) then
      begin
        lDataset.DataSet.Close;
        lSQL := 'SELECT Max(ChangeListID) AS LastID FROM ChangeList';
        lDataSet.SetSQL(lSQL);
        lDataSet.Dataset.Open;
        if (NOT lDataSet.DataSet.Eof) then
          Result := LDataset.Dataset.FieldByName('LastID').AsInteger;
        lDataSet.DataSet.Close;
      end;
    finally
      LDataset.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TChangeManager.SetGroupParents : boolean;
const OPNAME = 'TChangeManager.SetGroupParents';
var
  lIndex       : integer;
  lCount       : integer;
  lParentGroup : TChangeGroup;
  lChildGroup  : TChangeGroup;
  lElement     : TChangeGroupElement;
begin
  Result := FALSE;
  try
    for lIndex := 0 to FChangeGroupsList.Count - 1 do
    begin
      lParentGroup := TChangeGroup(FChangeGroupsList.Items[lIndex]);
      for lCount := 0 to lParentGroup.ElementCount do
      begin
        lElement := lParentGroup.CastChangeGroupElementByIndex(lCount);
        if (lElement <> nil) AND (lElement.IsElementGroup) then
        begin
          lChildGroup := CastChangeGroupWithID(lElement.ElementID);
          if (lChildGroup <> nil) then
            lChildGroup.ParentGroupID := lElement.GroupID;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TChangeManager.LanguageHasChanged: boolean;
const OPNAME = 'TChangeManager.LanguageHasChanged';
begin
  Result := inherited LanguageHasChanged;
  try
    if Assigned(FChangeAdminTabSheet) then
      FChangeAdminTabSheet.LanguageHasChanged;
    if Assigned(FChangeListValidator) then
      FChangeListValidator.LanguageHasChanged;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TChangeManager.StudyDataHasChanged (AContext : TChangeContext;
                                             AFieldName, AOldValue, ANewValue: string): boolean;
const OPNAME = 'TChangeManager.StudyDataHasChanged';
begin
  Result := inherited StudyDataHasChanged(AContext, AFieldName, AOldValue, ANewValue);
  try
    if (NOT FSystemFlag) then
      if Assigned(FChangeListValidator) then
        FChangeListValidator.StudyDataHasChanged(AContext, AFieldName, AOldValue, ANewValue);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TChangeManager.StudyHasChanged: boolean;
const OPNAME = 'TChangeManager.StudyHasChanged';
begin
  Result := inherited StudyHasChanged;
  try
    LoadChanges;
    if Assigned(FChangeAdminTabSheet) and
       (Assigned(FChangeAdminTabSheet.PageControl)) then
      if Assigned(FChangeListValidator) then
        FChangeListValidator.StudyHasChanged;
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TChangeManager.TabHasChanged;
const OPNAME = 'TChangeManager.TabHasChanged';
begin
  try
    if Assigned(FChangeListValidator) then
      FChangeListValidator.PopulateDataViewer;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TChangeManager.ChangeGroups : TList;
const OPNAME = 'TChangeManager.ChangeGroups';
begin
  Result := nil;
  try
    Result := FChangeGroupsList;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TChangeManager.CastChangeGroupWithID (AGroupID : integer) : TChangeGroup;
const OPNAME = 'TChangeManager.CastChangeGroupWithID';
var
  lChangeGroup : TChangeGroup;
  lIndex       : integer;
begin
  Result := nil;
  try
    if (AGroupID = 0) then
      Result := FMasterGroup
    else
    begin
      lIndex := 0;
      while ((Result = nil) AND (lIndex < FChangeGroupsList.Count)) do
      begin
        lChangeGroup := TChangeGroup(FChangeGroupsList.Items[lIndex]);
        if (lChangeGroup.GroupID = AGroupID) then
          Result := lChangeGroup
        else
          lIndex := lIndex + 1;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TChangeManager.ChangeGroupWithID (AGroupID : integer) : IChangeGroup;
const OPNAME = 'TChangeManager.ChangeGroupWithID';
begin
  Result := nil;
  try
    Result := CastChangeGroupWithID(AGroupID);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TChangeManager.ChangeGroupWithIndex (AIndex : integer) : IChangeGroup;
const OPNAME = 'TChangeManager.ChangeGroupWithIndex';
begin
  Result := nil;
  try
    if (AIndex < FChangeGroupsList.Count) AND (AIndex >= 0) then
      Result := TChangeGroup(FChangeGroupsList.Items[AIndex]);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TChangeManager.ChangeLists : TStringList;{TList;}
const OPNAME = 'TChangeManager.ChangeLists';
begin
  Result := nil;
  try
    Result := FChangeListsList;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TChangeManager.Get_MasterGroup : IChangeGroup;
const OPNAME = 'TChangeManager.Get_MasterGroup';
begin
  Result := nil;
  try
    Result := FMasterGroup;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TChangeManager.CastChangeListWithID (AListID : integer) : TChangeList;
const OPNAME = 'TChangeManager.CastChangeListWithID';
var
//  lChangeList : TChangeList;
  lIndex      : integer;
begin
  Result := nil;
  try
    lIndex := FChangeListsList.IndexOf(IntToStr(AListID));
    if lIndex >= 0 then
      Result :=  TChangeList(FChangeListsList.Objects[LIndex]);
  {
    lIndex := 0;
    while ((Result = nil) AND (lIndex < FChangeListsList.Count)) do
    begin
      lChangeList := TChangeList(FChangeListsList.Items[lIndex]);
      if (lChangeList.ChangeListID = AListID) then
        Result := lChangeList
      else
        lIndex := lIndex + 1;
    end;
   }
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TChangeManager.IsChangeListActive (AListID : integer) : WordBool;
const OPNAME = 'TChangeManager.IsChangeListActive';
var
  lIndex      : integer;
  lCount      : integer;
  lGroup      : TChangeGroup;
  lElement    : TChangeGroupElement;
  lFound      : boolean;
  lChangeList : IChangeList;
begin
  Result := FALSE;
  try
    lChangeList := ChangeListWithID(AListID);
    if (lChangeList <> nil) AND (lChangeList.IsResident) then
    begin
      lIndex := 0;
      lFound := FALSE;
      while (NOT lFound) AND (lIndex < FChangeGroupsList.Count) do
      begin
        lGroup := TChangeGroup(FChangeGroupsList.Items[lIndex]);
        lCount := 0;
        while (NOT lFound) AND (lCount < lGroup.ElementCount) do
        begin
          lElement := lGroup.CastChangeGroupElementByIndex(lCount);
          if (lElement <> nil) AND (NOT lElement.IsElementGroup) AND (lElement.ElementID = AListID) then
          begin
            Result := lElement.ElementActive;
            lFound := Result;
          end;
          lCount := lCount + 1;
        end;
        lIndex := lIndex + 1;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TChangeManager.ChangeListWithID (AListID : integer) : IChangeList;
const OPNAME = 'TChangeManager.ChangeListWithID';
begin
  Result := nil;
  try
    Result := CastChangeListWithID(AListID);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TChangeManager.ChangeListWithIndex (AIndex : integer) : IChangeList;
const OPNAME = 'TChangeManager.ChangeListWithIndex';
begin
  Result := nil;
  try
    if (AIndex < FChangeListsList.Count) AND (AIndex >= 0) then
//      Result := TChangeList(FChangeListsList.Items[AIndex]);
      Result :=  TChangeList(FChangeListsList.Objects[AIndex]);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TChangeManager.DoCreateNewChangeGroup : IChangeGroup;
const OPNAME = 'TChangeManager.DoCreateNewChangeGroup';
var
  lChangeGroup  : TChangeGroup;
  lGroupID      : integer;
  lNewStr       : string;
begin
  Result := nil;
  try
    if ModelSupportsChangeLists then
    begin
      lGroupID := GetLastChangeGroupID + 1;
      lNewStr  := FAppModules.Language.GetString('ChangeLists.NewChangeGroup');
      if (DBCreateNewChangeGroup(lGroupID, lNewStr)) then
      begin
        lChangeGroup := CreateChangeGroup;
        lChangeGroup.Populate(lGroupID, lNewStr);
        FMasterGroup.NewChangeGroupElement(lGroupID, TRUE);
        RefreshModelChangeLists;
        Result := lChangeGroup;
        StudyDataHasChanged(sdccAdd, 'ChangeGroup', '0', IntToStr(lChangeGroup.GroupID));
      end;
    end
    else
      ShowMessage(Format(FAppModules.Language.GetString('ChangeLists.NotYetImplementedForModel'),
                        [FAppModules.StudyArea.ModelCode]));
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TChangeManager.CreateChangeGroup : TChangeGroup;
const OPNAME = 'TChangeManager.CreateChangeGroup';
var
  lChangeGroup   : TChangeGroup;
begin
  Result := nil;
  try
    lChangeGroup := TChangeGroup.Create(FAppModules);
    FChangeGroupsList.Add(lChangeGroup);
    Result := lChangeGroup;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TChangeManager.DoDeleteChangeGroup  (AChangeGroupID : integer) : WordBool;
const OPNAME = 'TChangeManager.DoDeleteChangeGroup';
var
  lGroupID : integer;
begin
  Result := FALSE;
  try
    if ModelSupportsChangeLists then
    begin
      lGroupID := 0;
      if (AChangeGroupID > 0) then
        lGroupID := AChangeGroupID
      else
      if (Assigned(FChangeAdminTabSheet) AND Assigned(FChangeListValidator) AND
          (FChangeListValidator.ElementID > 0) AND
          (FChangeListValidator.ElementType in [cetInactiveGroup, cetActiveGroup])) then
        lGroupID := FChangeListValidator.ElementID;
      if (lGroupID > 0) then
      begin
        Result := DeleteChangeGroupWithID(lGroupID);
        if (Result) then
        begin
          RefreshModelChangeLists;
          FAppModules.Model.StudyDataHasChanged(sdccDelete, 'ChangeGroup', '0', IntToStr(lGroupID));
        end;
      end;
    end
    else
      ShowMessage(Format(FAppModules.Language.GetString('ChangeLists.NotYetImplementedForModel'),
                        [FAppModules.StudyArea.ModelCode]));
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TChangeManager.DeleteChangeGroupWithID (AGroupID : integer) : boolean;
const OPNAME = 'TChangeManager.DeleteChangeGroupWithID';
var
  lIndex         : integer;
  lDone          : boolean;
  lChangeGroup   : TChangeGroup;
  lElement       : TChangeGroupElement;
  lCount         : integer;
  lParentGroupID : integer;
  lParentGroup   : TChangeGroup;
begin
  Result := FALSE;
  try
      lDone   := FALSE;
      lIndex  := 0;
      lParentGroup := nil;
      while ((NOT lDone) AND (lIndex < FChangeGroupsList.Count)) do
      begin
        lChangeGroup := TChangeGroup(FChangeGroupsList.Items[lIndex]);
        if (lChangeGroup.GroupID = AGroupID) then
        begin
          lCount := 0;
          while (lCount < lChangeGroup.ElementCount) do
          begin
            lElement := lChangeGroup.CastChangeGroupElementByIndex(lCount);
            if (lElement <> nil) AND (lElement.IsElementGroup) then
              DeleteChangeGroupWithID(lElement.ElementID)
            else
              lCount := lCount + 1;
          end;
          lParentGroupID := lChangeGroup.ParentGroupID;
          lParentGroup   := CastChangeGroupWithID(lParentGroupID);
          DBDeleteChangeGroup(AGroupID);
          FChangeGroupsList.Delete(lIndex);
          FreeAndNil(lChangeGroup);
          lDone := TRUE;
        end
        else
          lIndex := lIndex + 1;
      end;
      if (lParentGroup <> nil) then
        lParentGroup.DeleteChangeGroupElementByID(AGroupID, TRUE);
      Result := lDone;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TChangeManager.GetChangeListIDsInOrder (AGroupID : integer;
                                                  ALists   : TStringList);
const OPNAME = 'TChangeManager.GetChangeListIDsInOrder';
var
  lGroup      : IChangeGroup;
  lElement    : IChangeGroupElement;
  lIndex      : integer;
  lChangeList : IChangeList;
begin
  try
    lGroup := ChangeGroupWithID(AGroupID);
    if (lGroup <> nil) then
    begin
      for lIndex := 0 to lGroup.ElementCount do
      begin
        lElement := lGroup.ChangeGroupElementByIndex(lIndex);
        if (lElement <> nil) then
        begin
          if (lElement.IsElementGroup) then
            GetChangeListIDsInOrder(lElement.ElementID, ALists)
          else
          begin
            lChangeList := ChangeListWithID(lElement.ElementID);
            if (lChangeList <> nil) AND (lChangeList.IsResident) and
               (ALists.IndexOf(IntToStr(lChangeList.ChangeListID)) < 0) then
              ALists.Add(IntToStr(lElement.ElementID));
          end;
        end;
      end;
      if (AGroupID = 0) then
      begin
        for lIndex := 0 to FChangeListsList.Count - 1 do
        begin
          lChangeList := ChangeListWithIndex(lIndex);
          if (lChangeList.IsResident) AND
             (ALists.IndexOf(IntToStr(lChangeList.ChangeListID)) < 0) then
            ALists.Add(IntToStr(lChangeList.ChangeListID));
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TChangeManager.PopulateDescendantsList (AGroupID : integer;
                                                  AGroups  : TStringList;
                                                  ALists   : TStringList);
const OPNAME = 'TChangeManager.PopulateDescendantsList';
var
  lGroup   : IChangeGroup;
  lElement : IChangeGroupElement;
  lIndex   : integer;
begin
  try
    AGroups.Add(IntToStr(AGroupID));
    lGroup := ChangeGroupWithID(AGroupID);
    if (lGroup <> nil) then
    begin
      for lIndex := 0 to lGroup.ElementCount do
      begin
        lElement := lGroup.ChangeGroupElementByIndex(lIndex);
        if (lElement <> nil) then
        begin
          if (lElement.IsElementGroup) then
            PopulateDescendantsList(lElement.ElementID, AGroups, ALists)
          else
            ALists.Add(IntToStr(lElement.ElementID));
        end;      
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TChangeManager.MayCreateNewChangeGroupElement (AOldParentID    : integer;
                                                        ANewParentID    : integer;
                                                        AElementID      : integer;
                                                        AIsElementGroup : WordBool;
                                                        var AErrorMsg   : string) : WordBool;
const OPNAME = 'TChangeManager.MayCreateNewChangeGroupElement';
var
  lElementLists  : TStringlist;
  lElementGroups : TStringlist;
  lParentLists   : TStringlist;
  lParentGroups  : TStringlist;
  lIndex         : integer;
  lCount         : integer;
  lParentGroup   : IChangeGroup;
  lElement       : IChangeGroupElement;
  lRootGroup     : TChangeGroup;
  lFound         : Boolean;
begin
  Result := TRUE;
  try
    AErrorMsg := '';
    if (ANewParentID >= 0) then
    begin
      lParentGroup := FAppModules.Changes.ChangeGroupWithID(ANewParentID);
      if (lParentGroup <> nil) then
      begin
        if (AIsElementGroup) AND (ANewParentID = 0) then
        begin
          { It is OK to drag a group to the top level. }
        end
        else
        if (AIsElementGroup) AND (ANewParentID = AElementID) then
        begin
          Result := FALSE;
          AErrorMsg := FAppModules.Language.GetString('Message.CannotAddChangeGroupToItself');
        end
        else
        if (AIsElementGroup) AND (lParentGroup.ElementCount > 0) AND
           (lParentGroup.ContainsChangeLists) then
        begin
          Result := FALSE;
          AErrorMsg := FAppModules.Language.GetString('Message.CannotAddChangeGroupToGroupWithChangeList');
        end
        else
        if (NOT AIsElementGroup) AND (ANewParentID = 0) then
        begin
          Result := FALSE;
          AErrorMsg := FAppModules.Language.GetString('Message.ChangeListMayNotBeAddedAtTopLevel');
        end
        else
        if (NOT AIsElementGroup) AND (lParentGroup.ElementCount > 0) AND
           (NOT lParentGroup.ContainsChangeLists) then
        begin
          Result := FALSE;
          AErrorMsg := FAppModules.Language.GetString('Message.CannotAddChangeGroupToGroupWithOtherGroups');
        end
        else
        begin
          lElementLists  := TStringList.Create;
          lElementGroups := TStringList.Create;
          lParentLists   := TStringList.Create;
          lParentGroups  := TStringList.Create;
          try
            { Element is a List }
            if (NOT AIsElementGroup) then
            begin
              lIndex := 0;
              lFound := FALSE;
              { Check if the list is already included in the descendants of its new master group }
              while (NOT lFound) AND (lIndex < FMasterGroup.ElementCount) do
              begin
                lElement := FMasterGroup.CastChangeGroupElementByIndex(lIndex);
                lRootGroup := CastChangeGroupWithID(lElement.ElementID);
                lParentGroups.Clear;
                lParentLists.Clear;
                PopulateDescendantsList(lRootGroup.GroupID, lParentGroups, lParentLists);
                if { NEW parent is in this mastergroup }
                   (lParentGroups.IndexOf(IntToStr(ANewParentID)) >= 0) AND
                   ({ List is dragged from OUTSIDE of group tree }
                    (AOldParentID = -1) OR
                    { OLD parent is NOT in this mastergroup }
                    (lParentGroups.IndexOf(IntToStr(AOldParentID)) < 0)) AND
                   { List is already in this mastergroup }
                   (lParentLists.IndexOf(IntToStr(AElementID)) >= 0) then
                begin
                  lFound := TRUE;
                  Result := FALSE;
                  AErrorMsg := FAppModules.Language.GetString('Message.DuplicateListInclusion');
                end
                else
                  lIndex := lIndex + 1;
              end;
            end
            else
            { Element is a Group }
            if (AIsElementGroup) then
            begin
              { Check that group is not added to one of its descendants }
              lParentGroup := ChangeGroupWithID(AElementID);
              if (lParentGroup <> nil) then
              begin
                lParentGroups.Clear;
                lParentLists.Clear;
                PopulateDescendantsList(lParentGroup.GroupID, lParentGroups, lParentLists);
                if (lParentGroups.IndexOf(IntToStr(ANewParentID)) >= 0) then
                begin
                  Result := FALSE;
                  AErrorMsg := FAppModules.Language.GetString('Message.CannotAddChangeGroupToDescedants');
                end;
              end;

              lIndex := 0;
              lFound := FALSE;
              { Search for the new master group element (root element) }
              while (Result) AND (NOT lFound) AND (lIndex < FMasterGroup.ElementCount) do
              begin
                lElement := FMasterGroup.CastChangeGroupElementByIndex(lIndex);
                lRootGroup := CastChangeGroupWithID(lElement.ElementID);
                if (lRootGroup.GroupID <> AElementID) then
                begin
                  lParentGroups.Clear;
                  lParentLists.Clear;
                  PopulateDescendantsList(lRootGroup.GroupID, lParentGroups, lParentLists);
                  if (lParentGroups.IndexOf(IntToStr(ANewParentID)) >= 0) then
                  begin
                    lFound := TRUE;
                    if (lParentGroups.IndexOf(IntToStr(AOldParentID)) < 0) then
                    { Element's old parent and new parent are not from same family - it is an outsider!}
                    begin
                      PopulateDescendantsList(AElementID, lElementGroups, lElementLists);
                      lCount := 0;
                      while (Result) AND (lCount < lElementLists.Count) do
                      begin
                        if (lParentLists.IndexOf(lElementLists.Strings[lCount]) >= 0) then
                        begin
                          Result := FALSE;
                          AErrorMsg := FAppModules.Language.GetString('Message.DuplicateListInclusion');
                        end
                        else
                          lCount := lCount + 1;
                      end;
                    end;
                  end;
                end;
                lIndex := lIndex + 1;
              end;
            end;
          finally
            FreeAndNil(lElementLists);
            FreeAndNil(lElementGroups);
            FreeAndNil(lParentLists);
            FreeAndNil(lParentGroups);
          end;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TChangeManager.DoCreateNewChangeGroupElement (AParentGroupID  : integer;
                                                       AElementID      : integer;
                                                       AIsElementGroup : WordBool) : IChangeGroupElement;
const OPNAME = 'TChangeManager.DoCreateNewChangeGroupElement';
var
  lElement     : IChangeGroupElement;
  lParentGroup : IChangeGroup;
  lChildGroup  : IChangeGroup;
begin
  Result := nil;
  try
    if ModelSupportsChangeLists and (AParentGroupID >= 0) then
    begin
      lParentGroup := Self.ChangeGroupWithID(AParentGroupID);
      if (lParentGroup <> nil) then
      begin
        lElement := lParentGroup.NewChangeGroupElement(AElementID, AIsElementGroup);
        if (AIsElementGroup) then
        begin
          lChildGroup := Self.ChangeGroupWithID(AElementID);
          if (lChildGroup <> nil) then
            lChildGroup.ParentGroupID := AParentGroupID;
        end;
        RefreshModelChangeLists;
        StudyDataHasChanged(sdccAdd, 'ChangeGroup', '0', IntToStr(AParentGroupID));
      end;
    end
    else
      ShowMessage(Format(FAppModules.Language.GetString('ChangeLists.NotYetImplementedForModel'),
                        [FAppModules.StudyArea.ModelCode]));
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TChangeManager.DoDeleteChangeGroupElement (AParentGroupID  : integer;
                                                    AElementID      : integer;
                                                    AIsElementGroup : WordBool) : WordBool;
const OPNAME = 'TChangeManager.DoDeleteChangeGroupElement';
var
  lParentGroup : IChangeGroup;
  lChildGroup  : IChangeGroup;
begin
  Result := FALSE;
  try
    if ModelSupportsChangeLists and (AParentGroupID >= 0) then
    begin
      lParentGroup := Self.ChangeGroupWithID(AParentGroupID);
      if (lParentGroup <> nil) then
      begin
        lParentGroup.RemoveChangeGroupElementByID(AElementID, AIsElementGroup);
        if (AIsElementGroup) then
        begin
          lChildGroup := Self.ChangeGroupWithID(AElementID);
          if (lChildGroup <> nil) then
            lChildGroup.ParentGroupID := 0;
        end;
        RefreshModelChangeLists;
        StudyDataHasChanged(sdccAdd, 'ChangeGroup', '0', IntToStr(AParentGroupID));
      end;
    end
    else
      ShowMessage(Format(FAppModules.Language.GetString('ChangeLists.NotYetImplementedForModel'),
                        [FAppModules.StudyArea.ModelCode]));
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TChangeManager.DoCreateNewChangeList : IChangeList;
const OPNAME = 'TChangeManager.DoCreateNewChangeList';
var
  lChangeList  : TChangeList;
  lListID      : integer;
  lDateCreated : TDateTime;
  lNewStr      : string;
  lNoneStr     : string;
  lListKey     : string;
begin
  Result := nil;
  try
    if ModelSupportsChangeLists then
    begin
      lListID      := GetLastChangeListID + 1;
      lDateCreated := Date;
      lNewStr      := FAppModules.Language.GetString('ChangeLists.NewChangeList');
      lNoneStr     := FAppModules.Language.GetString('ChangeLists.None');
      lListKey     := Trim(FAppModules.Model.GetModelDataSetKey);
      if (DBCreateNewChangeList(lListID, lListKey, lNewStr, lDateCreated, lNoneStr, lNoneStr)) then
      begin
        lChangeList := CreateChangeList(LListID);
        lChangeList.Populate(lListID, lListKey, TRUE, lNewStr, lDateCreated, lNoneStr, lNoneStr);
        RefreshModelChangeLists;
        Result := lChangeList;
        StudyDataHasChanged(sdccAdd, 'ChangeList', '0', IntToStr(lChangeList.ChangeListID));
      end;
    end
    else
      ShowMessage(Format(FAppModules.Language.GetString('ChangeLists.NotYetImplementedForModel'),
                        [FAppModules.StudyArea.ModelCode]));
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TChangeManager.CreateChangeList(AListID : integer)  : TChangeList;
const OPNAME = 'TChangeManager.CreateChangeList';
var
  lChangeList : TChangeList;
begin
  Result := nil;
  try
    lChangeList := TChangeList.Create(FAppModules);
    FChangeListsList.AddObject(IntToStr(AListID),LChangeList);
    lChangeList.OnParameterChangeAdded := ParameterChangeAdded;
    lChangeList.OnParameterChangeDeleted := ParameterChangeDeleted;
    Result := lChangeList;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TChangeManager.DoDeleteChangeList(AChangeListID: integer): WordBool;
const OPNAME = 'TChangeManager.DoDeleteChangeList';
begin
  Result := FALSE;
  try
    if ModelSupportsChangeLists then
    begin
      if (AChangeListID <= 0) AND
         Assigned(FChangeAdminTabSheet) AND Assigned(FChangeListValidator) AND
         (FChangeListValidator.ElementID > 0) AND (FChangeListValidator.ElementType = cetChangeList) then
        AChangeListID := FChangeListValidator.ElementID;
      if (AChangeListID > 0) then
      begin
        Result := DeleteChangeListWithID(AChangeListID);
        if (Result) then
        begin
          RefreshModelChangeLists;
          FAppModules.Model.StudyDataHasChanged(sdccDelete, 'ChangeList', '0', IntToStr(AChangeListID));
        end;
      end;
    end
    else
      ShowMessage(Format(FAppModules.Language.GetString('ChangeLists.NotYetImplementedForModel'),
                        [FAppModules.StudyArea.ModelCode]));
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TChangeManager.DeleteChangeListWithID (AListID : integer) : boolean;
const OPNAME = 'TChangeManager.DeleteChangeListWithID';
var
  lIndex      : integer;
  lItem       : integer;
  lDone       : boolean;
  lChangeList : TChangeList;
  lCount      : integer;
  lGroup      : TChangeGroup;
  lElement    : TChangeGroupElement;
  lFound      : boolean;
begin
  Result := FALSE;
  try
    if (DBDeleteChangeList(AListID)) then
    begin
      lDone  := FALSE;
      lIndex := 0;
      while ((NOT lDone) AND (lIndex < FChangeListsList.Count)) do
      begin
        lChangeList := TChangeList(FChangeListsList.Objects[lIndex]); //TChangeList(FChangeListsList.Items[lIndex]);
        if (lChangeList.ChangeListID = AListID) then
        begin
          for lCount := 0 to FChangeGroupsList.Count - 1 do
          begin
            lGroup := TChangeGroup(FChangeGroupsList.Items[lCount]);
            lFound := FALSE;
            lItem  := 0;
            while ((NOT lFound) AND (lItem < lGroup.ElementCount)) do
            begin
              lElement := lGroup.CastChangeGroupElementByIndex(lItem);
              if (NOT lElement.IsElementGroup) AND (lElement.ElementID = AListID) then
              begin
                lGroup.DeleteChangeGroupElementByID(AListID, FALSE);
                lFound := TRUE;
              end
              else
                lItem := lItem + 1;
            end;
          end;

          FChangeListsList.Delete(lIndex);
          FreeAndNil(lChangeList);
          lDone := TRUE;
        end
        else
          lIndex := lIndex + 1;
      end;
      Result := lDone;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TChangeManager.DoCopyChangeList(AChangeListID: integer): IChangeList;
const OPNAME = 'TChangeManager.DoCopyChangeList';
var
  lChangeList    : TChangeList;
  lNewChangeList : TChangeList;
  lDateCreated   : TDateTime;
  lName          : string;
  lDescr         : string;
  lPerson        : string;
  lIndex         : integer;
  lParamChange   : IParameterChange;
  lAbsolut       : string;
  lListKey       : string;
begin
  Result := nil;
  try
    if ModelSupportsChangeLists then
    begin
      if (AChangeListID <= 0) AND
         Assigned(FChangeAdminTabSheet) AND Assigned(FChangeListValidator) AND
         (FChangeListValidator.ElementID > 0) AND
         (FChangeListValidator.ElementType = cetChangeList) then
        AChangeListID := FChangeListValidator.ElementID;
      if (AChangeListID > 0) then
      begin
        lChangeList := CastChangeListWithID(AChangeListID);
        if (lChangeList <> nil) then
        begin
          AChangeListID := GetLastChangeListID + 1;
          lDateCreated  := Date;
          lName         := Format(FAppModules.Language.GetString('ChangeLists.CopyOf'), [lChangeList.ChangeListName]);
          lDescr        := lChangeList.Description;
          lPerson       := lChangeList.CreatedBy;
          lListKey      := lChangeList.GetKeyValues('', '');
          if (DBCreateNewChangeList(AChangeListID, lListKey, lName, lDateCreated, lPerson, lDescr)) then
          begin
            lNewChangeList := CreateChangeList(AChangeListID);
            lNewChangeList.Populate(AChangeListID, lListKey, TRUE, lName, lDateCreated, lPerson, lDescr);
            for lIndex := 0 to lChangeList.ParamChangeCount - 1 do
            begin
              lParamChange := lChangeList.ParamChangeByIndex(lIndex);
              if (lParamChange.Absolut) then lAbsolut := 'Y' else lAbsolut := 'N';
              lNewChangeList.CreateNewParamChange(lParamChange.ParamField,
                                                  lParamChange.KeyValues,
                                                  lParamChange.FieldIndex,
                                                  lAbsolut,
                                                  lParamChange.Change,
                                                  lParamChange.ParamDescr,False);
            end;
            Result := lNewChangeList;
            FAppModules.Model.StudyDataHasChanged(sdccAdd, 'ChangeListCopy', '0', IntToStr(lNewChangeList.ChangeListID));
          end;
        end;
      end;
    end
    else
      ShowMessage(Format(FAppModules.Language.GetString('ChangeLists.NotYetImplementedForModel'),
                        [FAppModules.StudyArea.ModelCode]));
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TChangeManager.DoActivateChangeElement(AParentGroupID  : integer;
                                                AElementID      : integer;
                                                AIsElementGroup : Boolean): WordBool;
const OPNAME = 'TChangeManager.DoActivateChangeElement';
var
  lElement : IChangeGroupElement;
  lParent  : IChangeGroup;
  lIndex   : integer;
begin
  Result := FALSE;
  try
    if ModelSupportsChangeLists then
    begin
      if (AElementID = 0) AND
         Assigned(FChangeAdminTabSheet) AND Assigned(FChangeListValidator) AND
         (FChangeListValidator.ElementID > 0) AND
         (FChangeListValidator.ElementType in [cetInactiveGroup, cetInactiveList]) then
      begin
        AElementID      := FChangeListValidator.ElementID;
        AParentGroupID  := FChangeListValidator.ParentGroupID;
        AIsElementGroup := FChangeListValidator.ElementType = cetInactiveGroup;
      end;
      if (AElementID <> 0) AND (AParentGroupID >= 0) then
      begin

         if (AParentGroupID = 0) then
        { Only 1 top level group may be active at a time. }
        begin
          for lIndex := 0 to FMasterGroup.ElementCount - 1 do
          begin
            lElement := FMasterGroup.ChangeGroupElementByIndex(lIndex);
            if (lElement <> nil) AND (lElement.ElementID <> AElementID) AND
               (lElement.ElementActive) then
              lElement.ElementActive := FALSE;
          end;
        end;

        lParent := ChangeGroupWithID(AParentGroupID);
        if (lParent <> nil) then
        begin
          lElement := lParent.ChangeGroupElementByID(AElementID, AIsElementGroup);
          if (lElement <> nil) then
          begin
            lElement.ElementActive := TRUE;
            Result := TRUE;
          end;


        end;
        FAppModules.Model.StudyDataHasChanged(sdccEdit, 'ElementActive', 'N', 'Y');
      end;
    end
    else
      ShowMessage(Format(FAppModules.Language.GetString('ChangeLists.NotYetImplementedForModel'),
                        [FAppModules.StudyArea.ModelCode]));
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TChangeManager.DoDeactivateChangeElement(AParentGroupID  : integer;
                                                  AElementID      : integer;
                                                  AIsElementGroup : Boolean): WordBool;
const OPNAME = 'TChangeManager.DoDeactivateChangeElement';
var
  lElement : IChangeGroupElement;
  lParent  : IChangeGroup;
begin
  Result := FALSE;
  try
    if ModelSupportsChangeLists then
    begin
      if (AElementID = 0) AND
         Assigned(FChangeAdminTabSheet) AND Assigned(FChangeListValidator) AND
         (FChangeListValidator.ElementID > 0) AND
         (FChangeListValidator.ElementType in [cetActiveList, cetActiveGroup]) then
      begin
        AElementID      := FChangeListValidator.ElementID;
        AParentGroupID  := FChangeListValidator.ParentGroupID;
        AIsElementGroup := FChangeListValidator.ElementType = cetActiveGroup;
      end;
      if (AElementID <> 0) AND (AParentGroupID >= 0) then
      begin
        lParent := ChangeGroupWithID(AParentGroupID);
        if (lParent <> nil) then
        begin
          lElement := lParent.ChangeGroupElementByID(AElementID, AIsElementGroup);
          if (lElement <> nil) then
          begin
            lElement.ElementActive := FALSE;
            FAppModules.Model.StudyDataHasChanged(sdccEdit, 'ElementActive', 'Y', 'N');
            Result := TRUE;
          end;
        end;
      end;
    end
    else
      ShowMessage(Format(FAppModules.Language.GetString('ChangeLists.NotYetImplementedForModel'),
                        [FAppModules.StudyArea.ModelCode]));
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TChangeManager.DoMoveUpChangeElement(AParentGroupID  : integer;
                                              AElementID      : integer;
                                              AIsElementGroup : Boolean): WordBool;
const OPNAME = 'TChangeManager.DoMoveUpChangeElement';
var
  lParent  : IChangeGroup;
begin
  Result := FALSE;
  try
    if ModelSupportsChangeLists then
    begin
      if (AElementID = 0) AND
         Assigned(FChangeAdminTabSheet) AND Assigned(FChangeListValidator) AND
         (FChangeListValidator.ElementID > 0) AND
         (FChangeListValidator.ElementType in [cetInactiveList, cetActiveList, cetInactiveGroup, cetActiveGroup]) then
      begin
        AElementID      := FChangeListValidator.ElementID;
        AParentGroupID  := FChangeListValidator.ParentGroupID;
        AIsElementGroup := FChangeListValidator.ElementType in [cetInactiveGroup, cetActiveGroup];
      end;
      if (AElementID <> 0) AND (AParentGroupID >= 0) then
      begin
        lParent := ChangeGroupWithID(AParentGroupID);
        if (lParent <> nil) then
        begin
          lParent.MoveUpChangeGroupElement(AElementID, AIsElementGroup);
          FAppModules.Model.StudyDataHasChanged(sdccEdit, 'ElementOrder', '', '');
          Result := TRUE;
        end;
      end;
    end
    else
      ShowMessage(Format(FAppModules.Language.GetString('ChangeLists.NotYetImplementedForModel'),
                        [FAppModules.StudyArea.ModelCode]));
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TChangeManager.DoMoveDownChangeElement(AParentGroupID  : integer;
                                                AElementID      : integer;
                                                AIsElementGroup : Boolean): WordBool;
const OPNAME = 'TChangeManager.DoMoveDownChangeElement';
var
  lParent  : IChangeGroup;
begin
  Result := FALSE;
  try
    if ModelSupportsChangeLists then
    begin
      if (AElementID = 0) AND
         Assigned(FChangeAdminTabSheet) AND Assigned(FChangeListValidator) AND
         (FChangeListValidator.ElementID > 0) AND
         (FChangeListValidator.ElementType in [cetInactiveList, cetActiveList, cetInactiveGroup, cetActiveGroup]) then
      begin
        AElementID      := FChangeListValidator.ElementID;
        AParentGroupID  := FChangeListValidator.ParentGroupID;
        AIsElementGroup := FChangeListValidator.ElementType in [cetInactiveGroup, cetActiveGroup];
      end;
      if (AElementID <> 0) AND (AParentGroupID >= 0) then
      begin
        lParent := ChangeGroupWithID(AParentGroupID);
        if (lParent <> nil) then
        begin
          lParent.MoveDownChangeGroupElement(AElementID, AIsElementGroup);
          FAppModules.Model.StudyDataHasChanged(sdccEdit, 'ElementOrder', '', '');
          Result := TRUE;
        end;
      end;
    end
    else
      ShowMessage(Format(FAppModules.Language.GetString('ChangeLists.NotYetImplementedForModel'),
                        [FAppModules.StudyArea.ModelCode]));
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TChangeManager.DoApplyChangeList (AChangeListID : integer) : WordBool;
const OPNAME = 'TChangeManager.DoApplyChangeList';
var
  lChangeList : TChangeList;
  lConfirmMsg : string;
begin
  Result := FALSE;
  try
    if (FAppModules.StudyArea.ModelCode = CYield) then
    begin
      lConfirmMsg := FAppModules.Language.GetString('Message.ActiveChangelists');
      if (MessageDlg(lConfirmMsg, mtConfirmation, [mbYes, mbNo], 0) = mrYes) then
      begin
        if (AChangeListID <= 0) AND
           Assigned(FChangeAdminTabSheet) AND  Assigned(FChangeListValidator) AND
           (FChangeListValidator.ElementID > 0) AND
           (FChangeListValidator.ElementType = cetChangeList) then
          AChangeListID := FChangeListValidator.ElementID;
        if (AChangeListID > 0) then
        begin
          lChangeList := CastChangeListWithID(AChangeListID);
          if (lChangeList <> nil) then
          begin
            lChangeList.ApplyAllParamChanges;
            DeleteChangeListWithID(lChangeList.ChangeListID);
            FAppModules.Model.StudyDataHasChanged(sdccDelete, 'ChangeListApply', '', '');
            Result := TRUE;
          end;
        end;
      end;
    end
    else
      ShowMessage(Format(FAppModules.Language.GetString('ChangeLists.NotYetImplementedForModel'),
                        [FAppModules.StudyArea.ModelCode]));
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TChangeManager.SetCreateNewChangeGroup(AEnabled: boolean);
const OPNAME = 'TChangeManager.SetCreateNewChangeGroup';
begin
  try
    if (Assigned(FChangeAdminTabSheet)) then
      FChangeAdminTabSheet.MenuItemManager.SetCreateNewChangeGroup(AEnabled);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TChangeManager.SetDeleteChangeGroup(AEnabled: boolean);
const OPNAME = 'TChangeManager.SetDeleteChangeGroup';
begin
  try
    if (Assigned(FChangeAdminTabSheet)) then
      FChangeAdminTabSheet.MenuItemManager.SetDeleteChangeGroup(AEnabled);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TChangeManager.SetCreateNewChangeList(AEnabled: boolean);
const OPNAME = 'TChangeManager.SetCreateNewChangeList';
begin
  try
    if (Assigned(FChangeAdminTabSheet)) then
      FChangeAdminTabSheet.MenuItemManager.SetCreateNewChangeList(AEnabled);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TChangeManager.SetDeleteChangeList(AEnabled: boolean);
const OPNAME = 'TChangeManager.SetDeleteChangeList';
begin
  try
    if (Assigned(FChangeAdminTabSheet)) then
      FChangeAdminTabSheet.MenuItemManager.SetDeleteChangeList(AEnabled);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TChangeManager.SetCopyChangeList(AEnabled: boolean);
const OPNAME = 'TChangeManager.SetCopyChangeList';
begin
  try
    if (Assigned(FChangeAdminTabSheet)) then
      FChangeAdminTabSheet.MenuItemManager.SetCopyChangeList(AEnabled);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TChangeManager.SetMoveUpChangeElement(AEnabled: boolean);
const OPNAME = 'TChangeManager.SetMoveUpChangeElement';
begin
  try
    if (Assigned(FChangeAdminTabSheet)) then
      FChangeAdminTabSheet.MenuItemManager.SetMoveUpChangeElement(AEnabled);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TChangeManager.SetMoveDownChangeElement(AEnabled: boolean);
const OPNAME = 'TChangeManager.SetMoveDownChangeElement';
begin
  try
    if (Assigned(FChangeAdminTabSheet)) then
      FChangeAdminTabSheet.MenuItemManager.SetMoveDownChangeElement(AEnabled);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TChangeManager.SetActivateChangeElement(AEnabled: boolean);
const OPNAME = 'TChangeManager.SetActivateChangeElement';
begin
  try
    if (Assigned(FChangeAdminTabSheet)) then
      FChangeAdminTabSheet.MenuItemManager.SetActivateChangeElement(AEnabled);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TChangeManager.SetDeactivateChangeElement(AEnabled: boolean);
const OPNAME = 'TChangeManager.SetDeactivateChangeElement';
begin
  try
    if (Assigned(FChangeAdminTabSheet)) then
      FChangeAdminTabSheet.MenuItemManager.SetDeactivateChangeElement(AEnabled);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TChangeManager.SetApplyChangeList(AEnabled: boolean);
const OPNAME = 'TChangeManager.SetApplyChangeList';
begin
  try
    if (Assigned(FChangeAdminTabSheet)) then
      FChangeAdminTabSheet.MenuItemManager.SetApplyChangeList(AEnabled);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TChangeManager.SetParameterChanges(AEnabled: boolean);
const OPNAME = 'TChangeManager.SetParameterChanges';
begin
  try
    if (FMenuItemManager <> nil) then
      FMenuItemManager.SetParameterChanges(AEnabled);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TChangeManager.ShowParameterChanges (AParamField     : string;
                                               AKeyValues      : string;
                                               AFieldIndex     : string);
const OPNAME = 'TChangeManager.ShowParameterChanges';
begin
  try
    if (FChangeListsList.Count > 0) then
    begin
      FChangeParameterForm := TChangeParameterForm.CreateWithoutDFM(nil, FAppModules);
      try
        FChangeParameterForm.Initialise;
        FChangeParameterForm.LanguageHasChanged;
        FChangeParameterForm.SetData(AParamField, AKeyValues, AFieldIndex);
        FChangeParameterForm.ShowModal;
      finally
        FreeAndNil(FChangeParameterForm);
      end;
    end
    else
      ShowMessage(FAppModules.Language.GetString('ChangeLists.PleaseCreateChangeList'));
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TChangeManager.DBCreateNewChangeGroup (AGroupID  : integer;
                                                AName    : string) : Boolean;
const OPNAME = 'TChangeManager.DBCreateNewChangeGroup';
var
  lDataSet       : TAbstractModelDataset;
  lSQL           : string;
begin
  Result := FALSE;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), lDataSet);
    try
      if Assigned(lDataSet) then
      begin
        lDataset.DataSet.Close;
        lSQL := 'INSERT INTO ChangeGroup'+
                '(Model,StudyAreaName,SubArea,Scenario,GroupID, GroupName)'+
                ' VALUES '+
                '(:Model,:StudyAreaName,:SubArea,:Scenario,:GroupID, :GroupName)';

        LDataSet.SetSQL(lSQL);
        LDataset.ClearQueryParams();
        LDataSet.SetParams(['Model'], [FAppModules.StudyArea.ModelCode]);
        LDataSet.SetParams(['StudyAreaName'], [FAppModules.StudyArea.StudyAreaCode]);
        LDataSet.SetParams(['SubArea'], [FAppModules.StudyArea.SubAreaCode]);
        LDataSet.SetParams(['Scenario'], [FAppModules.StudyArea.ScenarioCode]);
        LDataSet.SetParams(['GroupID'], [IntToStr(AGroupID)]);
        LDataSet.SetParams(['GroupName'], [AName]);
        LDataset.ExecSQL;
        LDataset.DataSet.Close;
        Result := TRUE;
      end;
    finally
      LDataset.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TChangeManager.DBDeleteChangeGroup (AGroupID : integer): boolean;
const OPNAME = 'TChangeManager.DBDeleteChangeGroup';
var
  lDataSet : TAbstractModelDataset;
  lSQL     : string;
begin
  Result := FALSE;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), lDataSet);
    try
      if Assigned(lDataSet) then
      begin
        FAppModules.Database.StartTransaction;
        try
          lDataset.DataSet.Close;

          lSQL := 'DELETE FROM ChangeGroupElement '+
                  ' WHERE  '+ AppModules.Model.GetChangeListWhereClause+
                  ' AND (GroupID = ' + IntToStr(AGroupID) +
                  ' OR ((IsElementGroup = ' + QuotedStr('Y') +') AND (ElementID = ' + IntToStr(AGroupID)+ ')))';
          LDataSet.SetSQL(lSQL);
          LDataset.ExecSQL;

          lDataset.DataSet.Close;
          lSQL := 'DELETE FROM ChangeGroup' +
                  ' WHERE  '+ AppModules.Model.GetChangeListWhereClause+
                  ' AND GroupID = ' + IntToStr(AGroupID);
          LDataSet.SetSQL(lSQL);
          LDataset.ExecSQL;
          LDataset.DataSet.Close;

          FAppModules.Database.Commit;
          Result := TRUE;
        except
          FAppModules.Database.Rollback;
          raise;
        end;
      end;
    finally
      LDataset.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TChangeManager.DBCreateNewChangeList (AListID  : integer;
                                               AListKey : string;
                                               AName    : string;
                                               ADate    : TDateTime;
                                               APerson  : string;
                                               ADescr   : string) : Boolean;
const OPNAME = 'TChangeManager.DBCreateNewChangeList';
var
  lDataSet       : TAbstractModelDataset;
  lSQL           : string;
begin
  Result := FALSE;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), lDataSet);
    try
      if Assigned(lDataSet) then
      begin
        lDataset.DataSet.Close;
        lSQL := 'INSERT INTO ChangeList'+
                '(Model,StudyAreaName,SubArea,Scenario,ChangeListID,ListName,DateCreated,CreatedBy,ListDescr,ChangeListKey)'+
                ' VALUES '+
                '(:Model,:StudyAreaName,:SubArea,:Scenario,:ChangeListID,:ListName,:DateCreated,:CreatedBy,:ListDescr,:ChangeListKey)';
        LDataSet.SetSQL(lSQL);
        LDataset.ClearQueryParams();
        LDataSet.SetParams(['Model'], [FAppModules.StudyArea.ModelCode]);
        LDataSet.SetParams(['StudyAreaName'], [FAppModules.StudyArea.StudyAreaCode]);
        LDataSet.SetParams(['SubArea'], [FAppModules.StudyArea.SubAreaCode]);
        LDataSet.SetParams(['Scenario'], [FAppModules.StudyArea.ScenarioCode]);
        LDataSet.SetParams(['ChangeListID'], [IntToStr(AListID)]);
        LDataSet.SetParams(['ListName'], [AName]);
        LDataSet.SetParams(['DateCreated'], [DateToStr(ADate)]);
        LDataSet.SetParams(['CreatedBy'], [APerson]);
        LDataSet.SetParams(['ListDescr'], [ADescr]);
        LDataSet.SetParams(['ChangeListKey'], [AListKey]);
        LDataset.ExecSQL;
        LDataset.DataSet.Close;
        Result := TRUE;
      end;
    finally
      LDataset.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TChangeManager.DBDeleteChangeList (AListID : integer): boolean;
const OPNAME = 'TChangeManager.DBDeleteChangeList';
var
  lDataSet : TAbstractModelDataset;
  lSQL     : string;
begin
  Result := FALSE;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), lDataSet);
    try
      if Assigned(lDataSet) then
      begin
        FAppModules.Database.StartTransaction;
        try
          lSQL := 'DELETE FROM ChangeGroupElement ' +
                  ' WHERE  '+ AppModules.Model.GetChangeListWhereClause+
                  ' AND ElementID = ' + IntToStr(AListID) +
                  ' AND IsElementGroup = ' + QuotedStr('N');
          LDataSet.SetSQL(lSQL);
          LDataset.ExecSQL;

          lDataset.DataSet.Close;
          lSQL := 'DELETE FROM ChangeParameter ' +
                  ' WHERE  '+ AppModules.Model.GetChangeListWhereClause+
                  ' AND ChangeListID = ' + IntToStr(AListID);
          LDataSet.SetSQL(lSQL);
          LDataset.ExecSQL;

          lDataset.DataSet.Close;
          lSQL := 'DELETE FROM ChangeList ' +
                  ' WHERE  '+ AppModules.Model.GetChangeListWhereClause+
                  ' AND ChangeListID = ' + IntToStr(AListID);
          LDataSet.SetSQL(lSQL);
          LDataset.ExecSQL;
          LDataset.DataSet.Close;

          FAppModules.Database.Commit;
          Result := TRUE;
        except
          FAppModules.Database.Rollback;
          raise;
        end;
      end;
    finally
      LDataset.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TChangeManager.GetShowAllChangeLists : boolean;
const OPNAME = 'TChangeManager.GetShowAllChangeLists';
begin
  Result := FALSE;
  try
    Result := FShowAllChangeLists;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TChangeManager.SetShowAllChangeLists (AShowAll : boolean);
const OPNAME = 'TChangeManager.SetShowAllChangeLists';
begin
  try
    FShowAllChangeLists := AShowAll;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TChangeManager.GetKeyValue (AKeyName   : string;
                                     AKeyValues : string) : string;
const OPNAME = 'TChangeManager.GetKeyValue';
var
  lPos : integer;
begin
  Result := '';
  try
    lPos := Pos(AKeyName, AKeyValues);
    if (lPos > 0) then
    begin
      AKeyValues := Copy(AKeyValues, lPos, Length(AKeyValues) - lPos + 1);
      lPos       := Pos('=', AKeyValues);
      if (lPos > 0) then
      begin
        AKeyValues := Copy(AKeyValues, lPos+1, Length(AKeyValues) - lPos);
        lPos       := Pos(',', AKeyValues);
        if (lPos > 0) then
          Result := Copy(AKeyValues, 1, lPos - 1)
        else
          Result := AKeyValues;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TChangeManager.GetParameterValue (AParamField : string;
                                           AKeyValues  : string;
                                           ABaseValue  : string;
                                           AFieldIndex : string) : string;
const OPNAME = 'TChangeManager.GetParameterValue';
var
  lFieldProperty : TAbstractFieldProperty;
  lFloatValue    : double;
  lIntValue      : integer;
begin
  Result := '';
  try
    lFieldProperty := FAppModules.FieldProperties.FieldProperty(AParamField);
    case lFieldProperty.FieldDataType of
      FieldStringType,
      FieldCharType  :
        Result := GetParameterValueStr(AParamField, AKeyValues, ABaseValue, AFieldIndex);
      FieldFloatType   :
        begin
          try
            lFloatValue := StrToFloat(ABaseValue);
            lFloatValue := GetParameterValueFloat(lFieldProperty, AKeyValues, lFloatValue, AFieldIndex);
            if lFieldProperty.SmartFormat then
              Result := SmartFloatFormat(lFloatValue,lFieldProperty.FieldWidth,lFieldProperty.NumberOfDecimals)
            else
            if (lFieldProperty.FormatStringGrid = '') then
              Result := FloatToStr(lFloatValue)
            else
              Result := Format(lFieldProperty.FormatStringGrid, [lFloatValue]);
          except
          end;
        end;
      FieldIntegerType :
        begin
          try
            lIntValue := StrToInt(ABaseValue);
            lIntValue := GetParameterValueInt(lFieldProperty, AKeyValues, lIntValue, AFieldIndex);
            if (lFieldProperty.FormatStringGrid = '') then
              Result := IntToStr(lIntValue)
            else
              Result := Format(lFieldProperty.FormatStringGrid, [lIntValue]);
          except
          end;
        end;
      else
        Result := ABaseValue;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;
{
function TChangeManager.GetParameterValueFloat (AFieldProperty : TAbstractFieldProperty;
                                                AKeyValues     : string;
                                                ABaseValue     : double;
                                                AFieldIndex    : string) : double;
const OPNAME = 'TChangeManager.GetParameterValueFloat';
var
  lChangeList    : IChangeList;
  lChangeListID  : integer;
  lIndex         : integer;
  lParamChange   : IParameterChange;
begin
  Result := 0;
  try
    for lIndex := 0 to FModelChangeLists.Count - 1 do
    begin

      lChangeListID := StrToInt(FModelChangeLists.Strings[lIndex]);
      lChangeList   := ChangeListWithID(lChangeListID);
      if (FAppModules.Changes.IsChangeListActive(lChangeListID)) then
      begin
        lParamChange := lChangeList.FindParamChange(AFieldProperty.FieldName, AKeyValues, AFieldIndex);
        if (lParamChange <> nil) then
        begin
          try
            if (lParamChange.Absolut) then
              ABaseValue := StrToFloat(Trim(lParamChange.Change))
            else
              ABaseValue := (1 + StrToFloat(Trim(lParamChange.Change)) / 100) * ABaseValue;
          except
          end;
          Break;
        end;
      end;
    end;
    Result := ABaseValue;
  except on E: Exception do HandleError(E, OPNAME) end;
end;
}

function TChangeManager.GetParameterValueFloat (AFieldProperty : TAbstractFieldProperty;
                                                AKeyValues     : string;
                                                ABaseValue     : double;
                                                AFieldIndex    : string) : double;
const OPNAME = 'TChangeManager.GetParameterValueFloat';
var
  LIndex         : integer;
  LParamChange   : IParameterChange;
  LKey   : string;
  LData  : TStringList;
begin
  Result := 0;
  try
    LData := TStringList.Create;
    try
      LData.Add(AFieldProperty.FieldName);
      LData.Add(AKeyValues);
      LData.Add(AFieldIndex);
      LKey := UpperCase(LData.CommaText);
      LIndex := FAllParamChanges.IndexOf(LKey);
      if (LIndex >= 0) then
        LParamChange := TParameterChange(FAllParamChanges.Objects[LIndex]);
      if (LParamChange <> nil) then
      begin
        if (FAppModules.Changes.IsChangeListActive(LParamChange.ChangeListID)) then
        begin
          try
            if (LParamChange.Absolut) then
              ABaseValue := StrToFloat(Trim(LParamChange.Change))
            else
              ABaseValue := (1 + StrToFloat(Trim(LParamChange.Change)) / 100) * ABaseValue;
          except
          end;
        end;
      end;
    finally
      FreeAndNil(LData);
    end;
    Result := ABaseValue;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


{
function TChangeManager.GetParameterValueInt (AFieldProperty : TAbstractFieldProperty;
                                              AKeyValues     : string;
                                              ABaseValue     : integer;
                                              AFieldIndex    : string) : integer;
const OPNAME = 'TChangeManager.GetParameterValueInt';
var
  lChangeList    : IChangeList;
  lChangeListID  : integer;
  lIndex         : integer;
  lParamChange   : IParameterChange;
begin
  Result := 0;
  try
    for lIndex := 0 to FModelChangeLists.Count - 1 do
    begin
      lChangeListID := StrToInt(FModelChangeLists.Strings[lIndex]);
      lChangeList   := ChangeListWithID(lChangeListID);
      if (FAppModules.Changes.IsChangeListActive(lChangeListID)) then
      begin
        lParamChange := lChangeList.FindParamChange(AFieldProperty.FieldName, AKeyValues, AFieldIndex);
        if (lParamChange <> nil) then
        begin
          try
            if (lParamChange.Absolut) then
              ABaseValue := StrToInt(Trim(lParamChange.Change))
            else
              ABaseValue := Round((1 + StrToInt(Trim(lParamChange.Change)) / 100) * ABaseValue);
          except
          end;
          Break;
        end;
      end;
    end;
    Result := ABaseValue;
  except on E: Exception do HandleError(E, OPNAME) end;
end;
}

function TChangeManager.GetParameterValueInt (AFieldProperty : TAbstractFieldProperty;
                                              AKeyValues     : string;
                                              ABaseValue     : integer;
                                              AFieldIndex    : string) : integer;
const OPNAME = 'TChangeManager.GetParameterValueInt';
var
  LIndex         : integer;
  LParamChange   : IParameterChange;
  LKey   : string;
  LData  : TStringList;
begin
  Result := 0;
  try
    LData := TStringList.Create;
    try
      LData.Add(AFieldProperty.FieldName);
      LData.Add(AKeyValues);
      LData.Add(AFieldIndex);
      LKey := UpperCase(LData.CommaText);
      LIndex := FAllParamChanges.IndexOf(LKey);
      if (LIndex >= 0) then
        LParamChange := TParameterChange(FAllParamChanges.Objects[LIndex]);
      if (LParamChange <> nil) then
      begin
        if (FAppModules.Changes.IsChangeListActive(LParamChange.ChangeListID)) then
        begin
          try
            if (LParamChange.Absolut) then
              ABaseValue := StrToInt(Trim(LParamChange.Change))
            else
              ABaseValue := Round((1 + StrToInt(Trim(LParamChange.Change)) / 100) * ABaseValue);
          except
          end;
        end;
      end;
    finally
      FreeAndNil(LData);
    end;
    Result := ABaseValue;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TChangeManager.GetParameterValueStr (AParamField : string;
                                              AKeyValues  : string;
                                              ABaseValue  : string;
                                              AFieldIndex : string) : string;
const OPNAME = 'TChangeManager.GetParameterValueStr';
var
  lIndex         : integer;
  lParamChange   : IParameterChange;
  LKey   : string;
  LData  : TStringList;
begin
  Result := '';
  try
    LData := TStringList.Create;
    try
      LData.Add(AParamField);
      LData.Add(AKeyValues);
      LData.Add(AFieldIndex);
      LKey := UpperCase(LData.CommaText);
      LIndex := FAllParamChanges.IndexOf(LKey);
      if (LIndex >= 0) then
        lParamChange := TParameterChange(FAllParamChanges.Objects[LIndex]);
      if (lParamChange <> nil) then
      begin
        if (FAppModules.Changes.IsChangeListActive(lParamChange.ChangeListID)) then
        begin
          ABaseValue := Trim(lParamChange.Change);
        end;
      end;
    finally
      FreeAndNil(LData);
    end;
    Result := ABaseValue;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TChangeManager.GetIndexes (AFieldIndex  : string;
                                     var lDim1Idx : integer;
                                     var lDim2Idx : integer);
const OPNAME = 'TChangeManager.GetIndexes';
var
  lPos : integer;
begin
  try
    lDim1Idx := 0;
    lDim2Idx := 0;
    lPos := Pos(',', AFieldIndex);
    try
      if (lPos > 0) then
      begin
        lDim1Idx := StrToInt(Copy(AFieldIndex, 1, lPos-1));
        lDim2Idx := StrToInt(Copy(AFieldIndex, lPos+1, Length(AFieldIndex)-lPos));
      end
      else
      begin
        lDim1Idx := StrToInt(AFieldIndex);
        lDim2Idx := 0;
      end;
    except
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TChangeManager.HasParamChange (AParamField : WideString;
                                        AKeyValues  : WideString;
                                        AFieldIndex : WideString) : boolean;
const OPNAME = 'TChangeManager.HasParamChange';
var
  //lChangeList : TChangeList;
  LKey   : string;
  lIndex : integer;
  LData  : TStringList;
begin
  Result := FALSE;
  try
    LData := TStringList.Create;
    try
      LData.Add(AParamField);
      LData.Add(AKeyValues);
      LData.Add(AFieldIndex);
      LKey := UpperCase(LData.CommaText);
      LIndex := FAllParamChanges.IndexOf(LKey);
      Result := (LIndex >= 0);
    finally
      LData.Free;
    end;

    {lIndex := 0;
    while ((NOT Result) AND (lIndex < FChangeListsList.Count)) do
    begin
      lChangeList := TChangeList(FChangeListsList.Items[lIndex]);
      if (lChangeList.FindParamChange(AParamField, AKeyValues, AFieldIndex) <> nil) then
        Result := TRUE
      else
        lIndex := lIndex + 1;
    end;}
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TChangeManager.ProcessMetaDataEvent : boolean;
const OPNAME = 'TChangeManager.ProcessMetaDataEvent';
begin
  Result := FALSE;
  try
    if (FAppModules.MainForm.PageControl.ActivePage = FChangeAdminTabSheet) then
      if Assigned(FChangeListValidator) then
        Result := FChangeListValidator.ProcessMetaDataEvent;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TChangeManager.EntityDescription (AFieldPropName : string;
                                           AKeyValues     : string;
                                           AFieldIndex    : string) : string;
const OPNAME = 'TChangeManager.EntityDescription';
var
  lChangeList       : IChangeList;
  lFieldProperty    : TAbstractFieldProperty;
  lKeyValue         : string;
  lItemID           : integer;
begin
  Result := '';
  try
    if (AFieldPropName <> '') then
    begin
      lFieldProperty := FAppModules.FieldProperties.FieldProperty(Trim(AFieldPropName));
      if (lFieldProperty <> nil) then
      begin
        if (lFieldProperty.DataClassName = 'TChangeList') then
        begin
          lKeyValue := GetKeyValue('ChangeListID', AKeyValues);
          if (lKeyValue <> '') then
          begin
            lItemID := StrToInt(lKeyValue);
            lChangeList := ChangeListWithID(lItemID);
            if (lChangeList <> nil) then
              Result := lChangeList.ChangeListName;
          end;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TChangeManager.ParameterChangeAdded(AParamField, AKeyValues,AFieldIndex: WideString;AParameterChange : TParameterChange);
const OPNAME = 'TChangeManager.ParameterChangeAdded';
var
  LData        : TStringList;
  LKey         : string;
begin
  try
    LData := TStringList.Create;
    try
      LData.Add(AParamField);
      LData.Add(AKeyValues);
      LData.Add(AFieldIndex);
      LKey := UpperCase(LData.CommaText);
      FAllParamChanges.AddObject(LKey,AParameterChange);
    finally
      LData.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TChangeManager.ParameterChangeDeleted(AParamField, AKeyValues,AFieldIndex: WideString;AParameterChange : TParameterChange);
const OPNAME = 'TChangeManager.ParameterChangeDeleted';
var
  LData        : TStringList;
  LKey         : string;
  LIndex       : integer;
begin
  try
    LData := TStringList.Create;
    try
      LData.Add(AParamField);
      LData.Add(AKeyValues);
      LData.Add(AFieldIndex);
      LKey := UpperCase(LData.CommaText);
      LIndex := FAllParamChanges.IndexOf(LKey);
      if(LIndex >= 0) then
        FAllParamChanges.Delete(LIndex);
    finally
      LData.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TChangeManager.DoExportChangeList(AChangeListID: integer): WordBool;
const OPNAME = 'TChangeManager.DoExportChangeList';
var
  LSelectedChangeList: string;
begin
  Result := False;
  try
    frmExportChangeListForm := TfrmExportChangeListForm.Create(nil);
    try
      frmExportChangeListForm.Populate(FChangeGroupsList,FChangeListsList);
      frmExportChangeListForm.btnSelectAll.Glyph.LoadFromResourceName(HImagesInstance, UpperCase('CEActivate'));
      frmExportChangeListForm.btnSelectAll.NumGlyphs :=
        frmExportChangeListForm.btnSelectAll.Glyph.Width div frmExportChangeListForm.btnSelectAll.Glyph.Height;
      frmExportChangeListForm.btnDeSelectAll.Glyph.LoadFromResourceName(HImagesInstance, UpperCase('CEDeactivate'));
      frmExportChangeListForm.btnDeSelectAll.NumGlyphs :=
        frmExportChangeListForm.btnDeSelectAll.Glyph.Width div frmExportChangeListForm.btnDeSelectAll.Glyph.Height;

      frmExportChangeListForm.btnSelectAll.Hint := FAppModules.Language.GetString('ButtonHint.RDSelectAll');
      frmExportChangeListForm.btnDeSelectAll.Hint := FAppModules.Language.GetString('ButtonHint.RDUnSelectAll');

      frmExportChangeListForm.ShowModal;
      if (frmExportChangeListForm.ModalResult = mrOk) then
      begin
        if frmExportChangeListForm.GetSelections(LSelectedChangeList) then
        begin
          if(LSelectedChangeList <> '') then
            FAppModules.StudyAreaManager.ExportChangeLists(LSelectedChangeList);
        end;
      end;
    finally
      FreeAndNil(frmExportChangeListForm);
    end;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TChangeManager.DoImportChangeList(AChangeListID: integer): WordBool;
const OPNAME = 'TChangeManager.DoImportChangeList';
begin
  Result := False;
  try
    FAppModules.StudyAreaManager.ImportChangeList;
    LoadChanges;
    if Assigned(FChangeListValidator) then
      FChangeListValidator.PopulateDataViewer;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TChangeManager.SetExportChangeList(AEnabled: boolean);
const OPNAME = 'TChangeManager.SetExportChangeList';
begin
  try
    if (Assigned(FChangeAdminTabSheet)) then
      FChangeAdminTabSheet.MenuItemManager.SetExportChangeList(AEnabled);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TChangeManager.SetImportChangeList(AEnabled: boolean);
const OPNAME = 'TChangeManager.SetImportChangeList';
begin
  try
    if (Assigned(FChangeAdminTabSheet)) then
      FChangeAdminTabSheet.MenuItemManager.SetImportChangeList(AEnabled);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TChangeManager.ModelSupportsChangeLists: boolean;
const OPNAME = 'TChangeManager.ModelSupportsChangeLists';
begin
  Result := False;
  try
    Result := (FAppModules.StudyArea.ModelCode = CYield) OR
           (FAppModules.StudyArea.ModelCode = CPlanning) OR
           (FAppModules.StudyArea.ModelCode = CRainfall);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TChangeManager.DoStationFilter(AChangeListID: integer): WordBool;
const OPNAME = 'TChangeManager.DoStationFilter';
begin
  Result := False;
  try
    ShowMessage(FAppModules.Language.GetString('Message.NotImplemented'));
    result := true;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TChangeManager.SetStationFilter(AEnabled: boolean);
const OPNAME = 'TChangeManager.SetStationFilter';
begin
  try
    if (Assigned(FChangeAdminTabSheet)) then
      FChangeAdminTabSheet.MenuItemManager.SetStationFilter(AEnabled);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

end.
