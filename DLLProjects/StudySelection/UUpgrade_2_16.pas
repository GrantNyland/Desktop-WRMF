{******************************************************************************}
{*  UNIT      : Contains function to upgrade database to WRMF 2.16.0         *}
{*  AUTHOR    : RH Steyn                                                      *}
{*  DATE      : 2006/05/02                                                    *}
{*  COPYRIGHT : Copyright © 2006 DWAF                                         *}
{******************************************************************************}

unit UUpgrade_2_16;

interface

uses
  Classes,
  SysUtils,
  UAbstractObject;

function UpgradeToVersion_2_16_0 (ADirectory             : string;
                                  AProgressUpdateFuntion : TProgressUpdateFuntion;
                                  AFileNames             : TStringList;
                                  var AVersion           : string) : boolean;
function Upgrade_ChangeList_2_16_0 (ADirectory : string;
                                    AFileName  : string) : boolean;
function Upgrade_MetaDataGroup_2_16_0 (ADirectory : string;
                                       AFileName  : string) : boolean;
function Upgrade_MetaData_2_16_0 (ADirectory : string;
                                  AFileName  : string) : boolean;


implementation

uses
  DBClient,
  DB,
  Vcl.Dialogs,
  UUtilities,
  UErrorHandlingOperations;

function UpgradeToVersion_2_16_0 (ADirectory             : string;
                                  AProgressUpdateFuntion : TProgressUpdateFuntion;
                                  AFileNames             : TStringList;
                                  var AVersion           : string) : boolean;
const OPNAME = 'UpgradeToVersion_2_16_0';
var
  lFileName : string;
  lStop     : boolean;
  lIndex    : integer;
begin
  Result := FALSE;
  try
    AProgressUpdateFuntion('Upgrading data to version 2.16.0.', ptNone, lStop);
    for lIndex := 1 to AFileNames.Count - 1 do
    begin
      lFileName := AFileNames[lIndex];
      if (lFileName = 'LD_ChangeList.xml') then
        Upgrade_ChangeList_2_16_0(ADirectory, lFileName);
      if (lFileName = 'LD_MetaDataGroup.xml') then
        Upgrade_MetaDataGroup_2_16_0(ADirectory, lFileName);
      if (lFileName = 'LD_MetaData.xml') then
        Upgrade_MetaData_2_16_0(ADirectory, lFileName);
    end;
    AFileNames[0] := 'TableVersion=2.16.0';
    AVersion := '2.16.0';
    Result   := TRUE;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function Upgrade_ChangeList_2_16_0 (ADirectory : string;
                                    AFileName  : string) : boolean;
const OPNAME = 'Upgrade_ChangeList_2_16_0';
var
  lOldDataSet  : TClientDataSet;
  lNewDataSet  : TClientDataSet;
  lOldFieldDef : TFieldDef;
  lFieldDef    : TFieldDef;
  lIndex       : integer;
  lListKey     : string;
  lModel       : string;
begin
  Result := FALSE;
  try
    lOldDataSet := TClientDataSet.Create(nil);
    lNewDataSet := TClientDataSet.Create(nil);
    try
      lOldDataSet.FileName := ADirectory + AFileName;
      lOldDataSet.Active   := TRUE;

      lNewDataSet.FieldDefs.Clear;
      lFieldDef := lNewDataSet.FieldDefs.AddFieldDef;
      with lFieldDef do
      begin
        Name     := 'ChangeListKey';
        DataType := ftString;
        Size     := 255;
        Required := TRUE;
      end;
      for lIndex := 0 to lOldDataSet.FieldDefs.Count - 1 do
      begin
        lOldFieldDef := lOldDataSet.FieldDefs[lIndex];
        if (Pos('Model', lOldFieldDef.Name) = 0) AND
           (Pos('StudyAreaName', lOldFieldDef.Name) = 0) AND
           (Pos('SubArea', lOldFieldDef.Name) = 0) AND
           (Pos('Scenario', lOldFieldDef.Name) = 0) AND
           (Pos('ListOrder', lOldFieldDef.Name) = 0) AND
           (Pos('ListActive', lOldFieldDef.Name) = 0) then
        begin
          lFieldDef := lNewDataSet.FieldDefs.AddFieldDef;
          lFieldDef.Name     := lOldFieldDef.Name;
          lFieldDef.DataType := lOldFieldDef.DataType;
          lFieldDef.Required := lOldFieldDef.Required;
          lFieldDef.Size     := lOldFieldDef.Size;
        end;
      end;
      lNewDataSet.CreateDataSet;
      lNewDataSet.Active := TRUE;

      lOldDataSet.First;
      while (NOT lOldDataSet.Eof) do
      begin
        lListKey := '';
        lModel   := QuotedStr(Trim(lOldDataSet.FieldByName('Model').AsString));
        if (Trim(lOldDataSet.FieldByName('Model').AsString) <> '') then
          lListKey := 'Model=' + QuotedStr(Trim(lOldDataSet.FieldByName('Model').AsString));
        if (Pos(CRainfall, lModel) = 0) then
        begin
          if (Trim(lOldDataSet.FieldByName('StudyAreaName').AsString) <> '') then
            lListKey := lListKey + ',StudyAreaName=' + QuotedStr(Trim(lOldDataSet.FieldByName('StudyAreaName').AsString));
          if (Trim(lOldDataSet.FieldByName('SubArea').AsString) <> '') then
            lListKey := lListKey + ',SubArea=' + QuotedStr(Trim(lOldDataSet.FieldByName('SubArea').AsString));
          if (Trim(lOldDataSet.FieldByName('Scenario').AsString) <> '') then
            lListKey := lListKey + ',Scenario=' + QuotedStr(Trim(lOldDataSet.FieldByName('Scenario').AsString));
        end;
        lNewDataSet.Insert;
        lNewDataSet.FieldByName('ChangeListKey').AsString := lListKey;
        lNewDataSet.FieldByName('ChangeListID').AsInteger := lOldDataSet.FieldByName('ChangeListID').AsInteger;
        lNewDataSet.FieldByName('ListName').AsString      := Trim(lOldDataSet.FieldByName('ListName').AsString);
        lNewDataSet.FieldByName('DateCreated').AsString   := Trim(lOldDataSet.FieldByName('DateCreated').AsString);
        lNewDataSet.FieldByName('CreatedBy').AsString     := Trim(lOldDataSet.FieldByName('CreatedBy').AsString);
        lNewDataSet.FieldByName('ListDescr').AsString     := Trim(lOldDataSet.FieldByName('ListDescr').AsString);
        lNewDataSet.Post;
        lOldDataSet.Next;
      end;
      lNewDataSet.SaveToFile(ADirectory + AFileName, dfXML);
      lNewDataSet.Close;
      Result := TRUE;
    finally
      lOldDataSet.Free;
      lNewDataSet.Free;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function Upgrade_MetaDataGroup_2_16_0 (ADirectory : string;
                                       AFileName  : string) : boolean;
const OPNAME = 'Upgrade_MetaDataGroup_2_16_0';
var
  lOldDataSet  : TClientDataSet;
  lNewDataSet  : TClientDataSet;
  lFieldDef    : TFieldDef;
  lListKey     : string;
  lModel       : string;
begin
  Result := FALSE;
  try
    lOldDataSet := TClientDataSet.Create(nil);
    lNewDataSet := TClientDataSet.Create(nil);
    try
      lOldDataSet.FileName := ADirectory + AFileName;
      lOldDataSet.Active   := TRUE;

      lNewDataSet.FieldDefs.Clear;
      lFieldDef := lNewDataSet.FieldDefs.AddFieldDef;
      with lFieldDef do
      begin
        Name     := 'MetaDataListKey';
        DataType := ftString;
        Size     := 255;
        Required := TRUE;
      end;
      lFieldDef := lNewDataSet.FieldDefs.AddFieldDef;
      with lFieldDef do
      begin
        Name     := 'MetaDataListID';
        DataType := ftInteger;
        Required := TRUE;
      end;
      lNewDataSet.CreateDataSet;
      lNewDataSet.Active := TRUE;

      lOldDataSet.First;
      while (NOT lOldDataSet.Eof) do
      begin
        lListKey := '';
        lModel   := QuotedStr(Trim(lOldDataSet.FieldByName('Model').AsString));
        if (Trim(lOldDataSet.FieldByName('Model').AsString) <> '') then
          lListKey := 'Model=' + QuotedStr(Trim(lOldDataSet.FieldByName('Model').AsString));
        if (Pos(CRainfall, lModel) = 0) then
        begin
          if (Trim(lOldDataSet.FieldByName('StudyAreaName').AsString) <> '') then
            lListKey := lListKey + ',StudyAreaName=' + QuotedStr(Trim(lOldDataSet.FieldByName('StudyAreaName').AsString));
          if (Trim(lOldDataSet.FieldByName('SubArea').AsString) <> '') then
            lListKey := lListKey + ',SubArea=' + QuotedStr(Trim(lOldDataSet.FieldByName('SubArea').AsString));
          if (Trim(lOldDataSet.FieldByName('Scenario').AsString) <> '') then
            lListKey := lListKey + ',Scenario=' + QuotedStr(Trim(lOldDataSet.FieldByName('Scenario').AsString));
        end;
        lNewDataSet.Insert;
        lNewDataSet.FieldByName('MetaDataListKey').AsString := lListKey;
        lNewDataSet.FieldByName('MetaDataListID').AsInteger := lOldDataSet.FieldByName('GroupID').AsInteger;
        lNewDataSet.Post;
        lOldDataSet.Next;
      end;
      lNewDataSet.SaveToFile(ADirectory + AFileName, dfXML);
      lNewDataSet.Close;
      Result := TRUE;
    finally
      lOldDataSet.Free;
      lNewDataSet.Free;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function Upgrade_MetaData_2_16_0 (ADirectory : string;
                                  AFileName  : string) : boolean;
const OPNAME = 'Upgrade_MetaData_2_16_0';
var
  lOldDataSet  : TClientDataSet;
  lNewDataSet  : TClientDataSet;
  lOldFieldDef : TFieldDef;
  lFieldDef    : TFieldDef;
  lIndex       : integer;
begin
  Result := FALSE;
  try
    lOldDataSet := TClientDataSet.Create(nil);
    lNewDataSet := TClientDataSet.Create(nil);
    try
      lOldDataSet.FileName := ADirectory + AFileName;
      lOldDataSet.Active   := TRUE;

      lNewDataSet.FieldDefs.Clear;
      lFieldDef := lNewDataSet.FieldDefs.AddFieldDef;
      with lFieldDef do
      begin
        Name     := 'MetaDataListID';
        DataType := ftInteger;
        Required := TRUE;
      end;
      for lIndex := 0 to lOldDataSet.FieldDefs.Count - 1 do
      begin
        lOldFieldDef := lOldDataSet.FieldDefs[lIndex];
        if (Pos('GroupID', lOldFieldDef.Name) = 0) then
        begin
          lFieldDef := lNewDataSet.FieldDefs.AddFieldDef;
          lFieldDef.Name     := lOldFieldDef.Name;
          lFieldDef.DataType := lOldFieldDef.DataType;
          lFieldDef.Required := lOldFieldDef.Required;
          lFieldDef.Size     := lOldFieldDef.Size;
        end;
      end;
      lNewDataSet.CreateDataSet;
      lNewDataSet.Active := TRUE;

      lOldDataSet.First;
      while (NOT lOldDataSet.Eof) do
      begin
        lNewDataSet.Insert;
        lNewDataSet.FieldByName('MetaDataListID').AsInteger := lOldDataSet.FieldByName('GroupID').AsInteger;
        lNewDataSet.FieldByName('ParamField').AsString      := Trim(lOldDataSet.FieldByName('ParamField').AsString);
        lNewDataSet.FieldByName('KeyValues').AsString       := Trim(lOldDataSet.FieldByName('KeyValues').AsString);
        lNewDataSet.FieldByName('FieldIndex').AsString      := Trim(lOldDataSet.FieldByName('FieldIndex').AsString);
        lNewDataSet.FieldByName('DateCreated').AsString     := Trim(lOldDataSet.FieldByName('DateCreated').AsString);
        lNewDataSet.FieldByName('CreatedBy').AsString       := Trim(lOldDataSet.FieldByName('CreatedBy').AsString);
        lNewDataSet.FieldByName('Comment').AsString         := Trim(lOldDataSet.FieldByName('Comment').AsString);
        lNewDataSet.Post;
        lOldDataSet.Next;
      end;
      lNewDataSet.SaveToFile(ADirectory + AFileName, dfXML);
      lNewDataSet.Close;
      Result := TRUE;
    finally
      lOldDataSet.Free;
      lNewDataSet.Free;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

end.
