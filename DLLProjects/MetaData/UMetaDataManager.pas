{******************************************************************************}
{*  UNIT      : Contains the class TMetaDataManager                           *}
{*  AUTHOR    : Riana Steyn                                                   *}
{*  DATE      : 2005/03/09                                                    *}
{*  COPYRIGHT : Copyright © 2005 DWAF                                         *}
{******************************************************************************}

unit UMetaDataManager;

interface

uses
  Classes,
  Vcl.ComCtrls,
  UAbstractObject,
  UMenuItemManager,
  UMetaData,
  UMetaDataMenuItemManager,
  UMetaDataForm,
  UAbstractModelObjects,
  VoaimsCom_TLB;

type
  TMetaDataManager = class(TAbstractMetaDataManager)
  protected
    FMetaDataListID  : integer;
    FMetaDataList    : TList;
    FMenuItemManager : TMetaDataMenuItemManager;
    FSystemFlag      : boolean;
    FMetaDataForm    : TMetaDataForm;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    procedure ClearMetaDataList;
    function LoadMetaData : boolean;
    function GetMetaDataSQL: string;
    function DBCreateNewMetaData (AListID     : integer;
                                  AParamField : string;
                                  AKeyValues  : string;
                                  AFieldIndex : string;
                                  ADate       : TDateTime;
                                  APerson     : string;
                                  AComment    : string) : Boolean;
    function DBDeleteData (AListID     : integer;
                           AParamField : string;
                           AKeyValues  : string;
                           AFieldIndex : string) : Boolean;
    function FindMetaDataCast (AParamField : WideString;
                               AKeyValues  : WideString;
                               AFieldIndex : WideString) : TMetaData;
    function FindListID (AListKey : string) : integer;
    function CreateNewListID (AListKey : string) : integer;
  public
    function Initialise: boolean; override;
    function LanguageHasChanged: boolean; override;
    function StudyHasChanged: boolean; override;
    function StudyDataHasChanged(AContext: TChangeContext; AFieldName,AOldValue,ANewValue: string): boolean; override;
    function DataList : TList; override;
    procedure SetMetaDataMenuItem (AEnabled : boolean); override;
    function FindMetaData (AParamField : WideString;
                           AKeyValues  : WideString;
                           AFieldIndex : WideString) : IMetaData;  override;
    procedure ShowMetaData (AParamField     : string;
                            AKeyValues      : string;
                            AFieldIndex     : string); override;
    function CreateNewMetaData (AParamField : WideString;
                                AKeyValues  : WideString;
                                AFieldIndex : WideString) : IMetaData; override;
    function DeleteMetaData (AParamField : WideString;
                             AKeyValues  : WideString;
                             AFieldIndex : WideString) : WordBool; override;
  end;

implementation

uses
  System.Types,
  Vcl.Controls,
  SysUtils,
  UDataSetType,
  UConstants,
  UDBConstants,
  Vcl.Dialogs,
  UErrorHandlingOperations;

{ TMetaDataManager }

procedure TMetaDataManager.CreateMemberObjects;
const OPNAME = 'TMetaDataManager.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
    FSystemFlag      := FALSE;
    FMetaDataList    := TList.Create;
    FMetaDataForm    := nil;
    FMenuItemManager := nil;
    if(FAppModules.MainForm <> nil) and (FAppModules.MainForm.MainForm <> nil) then
      FMenuItemManager := TMetaDataMenuItemManager.Create(FAppModules);
//    FMenuItemManager.Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMetaDataManager.DestroyMemberObjects;
const OPNAME = 'TMetaDataManager.DestroyMemberObjects';
begin
  try
    ClearMetaDataList;
    FreeAndNil(FMetaDataList);
    if Assigned(FMenuItemManager) then
      FreeAndNil(FMenuItemManager);
    inherited DestroyMemberObjects;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMetaDataManager.Initialise: boolean;
const OPNAME = 'TMetaDataManager.Initialise';
begin
  Result := FALSE;
  try
    ClearMetaDataList;
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMetaDataManager.ClearMetaDataList;
const OPNAME = 'TMetaDataManager.ClearMetaDataList';
var
  lMetaData : TMetaData;
begin
  try
    while (FMetaDataList.Count > 0) do
    begin
      lMetaData := TMetaData(FMetaDataList.Items[0]);
      FMetaDataList.Delete(0);
      FreeAndNil(lMetaData);
    end;
    FMetaDataList.Clear;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TMetaDataManager.LoadMetaData : boolean;
const OPNAME = 'TMetaDataManager.LoadMetaData';
var
  lDataSet  : TAbstractModelDataset;
  lMetaData : TMetaData;
  lGroupKey : string;
begin
  Result := FALSE;
  try
    ClearMetaDataList;
    lGroupKey := Trim(FAppModules.Model.GetModelDataSetKey);
    FMetaDataListID := FindListID(lGroupKey);
    if (FMetaDataListID <> 0) then
    begin
      FAppModules.Database.CreateDataset(integer(dtExecSQL), lDataSet);
      try
        if (Assigned(lDataSet)) then
        begin
          lDataSet.SetSQL(GetMetaDataSQL);
          lDataset.DataSet.Open;
          Result := True;

          while (NOT lDataset.DataSet.EOF) do
          begin
            lMetaData := TMetaData.Create(FAppModules);
            if (lMetaData <> nil) then
            begin
              FMetaDataList.Add(lMetaData);
              lMetaData.Populate(FMetaDataListID,
                            lDataSet.DataSet.FieldByName('ParamField').AsString,
                            lDataSet.DataSet.FieldByName('KeyValues').AsString,
                            lDataSet.DataSet.FieldByName('FieldIndex').AsString,
                            lDataSet.DataSet.FieldByName('DateCreated').AsDateTime,
                            lDataSet.DataSet.FieldByName('CreatedBy').AsString,
                            lDataSet.DataSet.FieldByName('Comment').AsString
                              );
            end;
            lDataset.DataSet.Next;
          end;
          lDataset.DataSet.Close;
        end;
      finally
        lDataset.Free;
      end;
    end;
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TMetaDataManager.GetMetaDataSQL: string;
const OPNAME = 'TMetaDataManager.GetMetaDataSQL';
begin
  Result := '';
  try
    Result := 'SELECT * FROM MetaDataItem WHERE MetaDataListID = ' + IntToStr(FMetaDataListID);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMetaDataManager.LanguageHasChanged: boolean;
const OPNAME = 'TMetaDataManager.LanguageHasChanged';
begin
  Result := inherited LanguageHasChanged;
  try
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TMetaDataManager.StudyDataHasChanged (AContext : TChangeContext;
                                               AFieldName, AOldValue, ANewValue: string): boolean;
const OPNAME = 'TMetaDataManager.StudyDataHasChanged';
begin
  Result := inherited StudyDataHasChanged(AContext, AFieldName, AOldValue, ANewValue);
  try
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TMetaDataManager.StudyHasChanged: boolean;
const OPNAME = 'TMetaDataManager.StudyHasChanged';
begin
  Result := inherited StudyHasChanged;
  try
    LoadMetaData;
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TMetaDataManager.DataList : TList;
const OPNAME = 'TMetaDataManager.DataList';
begin
  Result := nil;
  try
    Result := FMetaDataList;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TMetaDataManager.SetMetaDataMenuItem(AEnabled: boolean);
const OPNAME = 'TMetaDataManager.SetMetaDataMenuItem';
begin
  try
    if(FMenuItemManager <> nil) then
      FMenuItemManager.SetParameterChanges(AEnabled);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TMetaDataManager.ShowMetaData (AParamField     : string;
                                         AKeyValues      : string;
                                         AFieldIndex     : string);
const OPNAME = 'TMetaDataManager.ShowMetaData';
begin
  try
    FMetaDataForm := TMetaDataForm.CreateWithoutDFM(nil, FAppModules);
    try
      FMetaDataForm.Initialise;
      FMetaDataForm.LanguageHasChanged;
      FMetaDataForm.SetData(AParamField, AKeyValues, AFieldIndex);
      FMetaDataForm.ShowModal;
    finally
      FreeAndNil(FMetaDataForm);
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TMetaDataManager.FindMetaDataCast (AParamField : WideString;
                                            AKeyValues  : WideString;
                                            AFieldIndex : WideString) : TMetaData;
const OPNAME = 'TMetaDataManager.FindMetaDataCast';
var
  lIndex    : integer;
  lMetaData : TMetaData;
begin
  Result := nil;
  try
    lIndex := 0;
    while ((Result = nil) AND (lIndex < FMetaDataList.Count)) do
    begin
      lMetaData := TMetaData(FMetaDataList.Items[lIndex]);
      if ((lMetaData.ParamField = AParamField) AND
          (lMetaData.KeyValues  = AKeyValues)  AND
          (lMetaData.FieldIndex = AFieldIndex))then
        Result := lMetaData
      else
        lIndex := lIndex + 1;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMetaDataManager.FindMetaData (AParamField : WideString;
                                        AKeyValues  : WideString;
                                        AFieldIndex : WideString) : IMetaData;
const OPNAME = 'TMetaDataManager.FindMetaData';
begin
  Result := nil;
  try
    Result := FindMetaDataCast(AParamField, AKeyValues, AFieldIndex);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMetaDataManager.CreateNewMetaData (AParamField : WideString;
                                             AKeyValues  : WideString;
                                             AFieldIndex : WideString): IMetaData;
const OPNAME = 'TMetaDataManager.CreateNewMetaData';
var
  lMetaData    : TMetaData;
  lDateCreated : TDateTime;
  lNoneStr     : string;
  lGroupKey    : string;
begin
  Result := nil;
  try
    if (FAppModules.StudyArea.ModelCode = CYield) OR
       (FAppModules.StudyArea.ModelCode = CRainfall) then
    begin
      lDateCreated := Date;
      if (FMetaDataListID = 0) then
      begin
        lGroupKey := Trim(FAppModules.Model.GetModelDataSetKey            );
        FMetaDataListID := CreateNewListID(lGroupKey);
      end;  
      lNoneStr := FAppModules.Language.GetString('ChangeLists.None');

      if (DBCreateNewMetaData(FMetaDataListID, AParamField, AKeyValues, AFieldIndex, lDateCreated, lNoneStr, lNoneStr)) then
      begin
        lMetaData := TMetaData.Create(FAppModules);
        FMetaDataList.Add(lMetaData);
        lMetaData.Populate(FMetaDataListID, AParamField, AKeyValues, AFieldIndex, lDateCreated, lNoneStr, lNoneStr);
        Result := lMetaData;
        StudyDataHasChanged(sdccAdd, 'AddMetaData', '', '');
      end;
    end
    else
      ShowMessage(Format(FAppModules.Language.GetString('ChangeLists.NotYetImplementedForModel'),
                         [FAppModules.StudyArea.ModelCode]));
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TMetaDataManager.FindListID (AListKey : string) : integer;
const OPNAME = 'TMetaDataManager.FindListID';
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
        lSQL := 'SELECT MetaDataListID FROM MetaDataList ' +
                'WHERE MetaDataListKey = ' + QuotedStr(AListKey);
        lDataSet.SetSQL(lSQL);
        lDataSet.Dataset.Open;
        if (NOT lDataSet.DataSet.Eof) then
          Result := LDataset.Dataset.FieldByName('MetaDataListID').AsInteger
      end
    finally
      LDataset.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMetaDataManager.CreateNewListID (AListKey : string) : integer;
const OPNAME = 'TMetaDataManager.CreateNewListID';
var
  lDataSet : TAbstractModelDataset;
  lSQL     : string;
  lLastID  : integer;
begin
  Result := 0;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), lDataSet);
    try
      if Assigned(lDataSet) then
      begin
        lDataset.DataSet.Close;
        lSQL := 'SELECT Max(MetaDataListID) AS LastID FROM MetaDataList';
        lDataSet.SetSQL(lSQL);
        lDataSet.Dataset.Open;
        if (NOT lDataSet.DataSet.Eof) then
          lLastID := LDataset.Dataset.FieldByName('LastID').AsInteger
        else
          lLastID := 0;

        lDataSet.DataSet.Close;
        lSQL := 'INSERT INTO MetaDataList '+
                '(MetaDataListKey, MetaDataListID) ' +
                'VALUES (' +
                QuotedStr(AListKey) + ','+
                IntToStr(lLastID + 1) + ')';
        LDataSet.SetSQL(lSQL);
        LDataset.ExecSQL;
        LDataset.DataSet.Close;
        Result := lLastID + 1;
      end;
    finally
      LDataset.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMetaDataManager.DBCreateNewMetaData (AListID     : integer;
                                               AParamField : string;
                                               AKeyValues  : string;
                                               AFieldIndex : string;
                                               ADate       : TDateTime;
                                               APerson     : string;
                                               AComment    : string) : Boolean;
const OPNAME = 'TMetaDataManager.DBCreateNewMetaData';
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
        lDataset.DataSet.Close;
        lSQL := 'INSERT INTO MetaDataItem '+
                '(MetaDataListID, ParamField, KeyValues, FieldIndex, DateCreated, CreatedBy, Comment) ' +
                'VALUES (' +
                IntToStr(AListID)          + ',' +
                QuotedStr(AParamField)      + ',' +
                QuotedStr(AKeyValues)       + ',' +
                QuotedStr(AFieldIndex)      + ',' +
                QuotedStr(DateToStr(ADate)) + ',' +
                QuotedStr(APerson)          + ',' +
                QuotedStr(AComment)         + ')';
        LDataSet.SetSQL(lSQL);
        LDataset.ExecSQL;
        LDataset.DataSet.Close;
        Result := TRUE;
      end;
    finally
      LDataset.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMetaDataManager.DeleteMetaData (AParamField : WideString;
                                          AKeyValues  : WideString;
                                          AFieldIndex : WideString): WordBool;
const OPNAME = 'TMetaDataManager.DeleteMetaData';
var
  lMetaData : TMetaData;
begin
  Result := FALSE;
  try
    if (FAppModules.StudyArea.ModelCode = CYield) OR
       (FAppModules.StudyArea.ModelCode = CRainfall) then
    begin
      lMetaData := FindMetaDataCast(AParamField, AKeyValues, AFieldIndex);
      if (lMetaData <> nil) then
      begin
        if (DBDeleteData(FMetaDataListID, AParamField, AKeyValues, AFieldIndex)) then
        begin
          FMetaDataList.Remove(lMetaData);
          FreeAndNil(lMetaData);
          StudyDataHasChanged(sdccDelete, 'DeleteMetaData', '', '');
        end;
      end
    end
    else
      ShowMessage(Format(FAppModules.Language.GetString('ChangeLists.NotYetImplementedForModel'),
                         [FAppModules.StudyArea.ModelCode]));
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TMetaDataManager.DBDeleteData (AListID     : integer;
                                        AParamField : string;
                                        AKeyValues  : string;
                                        AFieldIndex : string): boolean;
const OPNAME = 'TMetaDataManager.DBDeleteData';
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
        lDataset.DataSet.Close;
        lSQL := 'DELETE FROM MetaDataItem ' +
                'WHERE MetaDataListID = ' + IntToStr(AListID) +
                '  AND ParamField = ' + QuotedStr(AParamField) +
                '  AND KeyValues = '  + QuotedStr(AKeyValues);
        if (AFieldIndex <> '') then
          lSQL := lSQL + ' AND FieldIndex = ' + QuotedStr(AFieldIndex);
        LDataSet.SetSQL(lSQL);
        LDataset.ExecSQL;
        Result := TRUE;
      end;
    finally
      LDataset.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
