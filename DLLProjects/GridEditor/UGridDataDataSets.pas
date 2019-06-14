unit UGridDataDataSets;

interface

uses
  DB,
  Classes,
  Contnrs,
  UDataSetType,
  UViewDataItem,
  UGridFieldData,
  UAbstractObject;

type
  TGridDataDataSets = class(TGridData)
  protected
    FDataSets: TStringList;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    function DataSetCount: integer;
    function IsViewDataSetEOF: boolean;
    procedure AllViewDataSetsFirst;
    procedure AllViewDataSetsNext;
    procedure ExtractDataSetData;
    function AddDataSet(AViewDataSet: TViewDataSet): boolean;
    procedure CreateContextData(var AStringList: TStringList; ADataSet: TDataSet; APivotIndex: integer);
    procedure InitialiseDataSet(ADataSet: TAbstractModelDataset);
    procedure RefreshDatasetLanguage(ADataSet: TAbstractModelDataset);
    function GetDataSet(ADataSetIndex: integer): TAbstractModelDataset;
    property DataSet[ADataSetIndex: integer]: TAbstractModelDataset read GetDataSet;
  public
    procedure Clear; override;
    function DataIDCommaText: string; override;
    function LanguageHasChanged: boolean; override;
    procedure ConstructDataSets(AViewDataNode: TViewDataNode);
  end;

implementation

uses
  Math,
  SysUtils,
  UDBConstants,
  UStringFieldOperations,
  UErrorHandlingOperations;

procedure TGridDataDataSets.CreateMemberObjects;
const OPNAME = 'TGridDataDataSets.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
    FDataSets := TStringList.Create;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TGridDataDataSets.DestroyMemberObjects;
const OPNAME = 'TGridDataDataSets.DestroyMemberObjects';
begin
  try
    inherited DestroyMemberObjects;
    FreeAndNil(FDataSets); // Call last because ancestor calls the clear method.
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TGridDataDataSets.Clear;
const OPNAME = 'TGridDataDataSets.Clear';
var LDatasetIndex: integer;
begin
  try
    inherited Clear;
    for LDatasetIndex := 0 to FDataSets.Count - 1 do
    begin
      DataSet[LDatasetIndex].DataSet.Close;
      FDataSets.Objects[LDatasetIndex].Free;
      FDataSets.Objects[LDatasetIndex] := nil;
    end;
    FDataSets.Clear;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TGridDataDataSets.DataSetCount: integer;
const OPNAME = 'TGridDataDataSets.DataSetCount';
begin
  Result := 0;
  try
    Result := FDataSets.Count;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TGridDataDataSets.DataIDCommaText: string;
const OPNAME = 'TGridDataDataSets.DataIDCommaText';
begin
  Result := '';
  try
    Result := FDataSets.CommaText;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TGridDataDataSets.LanguageHasChanged: boolean;
const OPNAME = 'TGridDataDataSets.LanguageHasChanged';
var LDataSetIndex: integer;
begin
  Result := True;
  try
    inherited LanguageHasChanged;
    for LDataSetIndex := 0 to FDataSets.Count - 1 do
      RefreshDatasetLanguage(DataSet[LDataSetIndex]);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TGridDataDataSets.ConstructDataSets(AViewDataNode: TViewDataNode);
const OPNAME = 'TGridDataDataSets.ConstructDataSets';
var
  LDatasetIndex: integer;
  LResult : boolean;
begin
  try
    FDataSets.Clear;
    LResult := True;
    for LDataSetIndex := 0 to AViewDataNode.ViewDataSetCount - 1 do
      LResult := LResult and AddDataSet(AViewDataNode.ViewDataSet[LDataSetIndex]);
    if LResult then
      ExtractDataSetData;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TGridDataDataSets.AddDataSet(AViewDataSet: TViewDataSet): boolean;
const OPNAME = 'TGridDataDataSets.AddDataSet';
var LDataSet: TAbstractModelDataset;
begin
  Result := False;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    if Assigned(LDataSet) then
    begin
      LDataSet.SetSQL(AViewDataSet.ViewSQL);
      FAppModules.StudyArea.SetDefaultParams(LDataSet);
      if (AViewDataSet.ParamCount > 0) then
        LDataset.SetParams(AViewDataSet.ParamNames, AViewDataSet.ParamValues);
      if LDataset.AreAllParamsBound then
      begin
        LDataset.DataSet.Open;
        InitialiseDataSet(LDataSet);
      end;
      FDataSets.AddObject(AViewDataSet.DatasetID, LDataSet);
      Result := True;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TGridDataDataSets.InitialiseDataSet(ADataSet: TAbstractModelDataset);
const OPNAME = 'TGridDataDataSets.InitialiseDataSet';
var
  LCount: integer;
  LFieldProperty: TAbstractFieldProperty;
  LField: TField;
begin
  try
    if Assigned(ADataSet) and Assigned(FAppModules.FieldProperties()) then
    begin
      for LCount := 0 to ADataSet.DataSet.FieldCount -1 do
      begin
        LField := ADataSet.DataSet.Fields[LCount];
        LField.Visible := False;
        LFieldProperty := FAppModules.FieldProperties.FieldProperty(LField.FieldName);
        if Assigned(LFieldProperty) then
          if (LFieldProperty.FieldType = FieldDisplayType) then
            LField.Visible := True;
      end;
      RefreshDatasetLanguage(ADataSet);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TGridDataDataSets.RefreshDatasetLanguage(ADataSet: TAbstractModelDataset);
const OPNAME = 'TGridDataDataSets.RefreshDatasetLanguage';
var
  LCount: integer;
  LFieldProperty: TAbstractFieldProperty;
  LField: TField;
  LLangFieldName: string;
begin
  try
    if Assigned(ADataSet) and Assigned(FAppModules.FieldProperties()) then
    begin
      for LCount := 0 to ADataSet.DataSet.FieldCount -1 do
      begin
        LField := ADataSet.DataSet.Fields[LCount];
        LFieldProperty := FAppModules.FieldProperties.FieldProperty(LField.FieldName);
        if Assigned(LFieldProperty) and (Trim(LFieldProperty.FieldLangDescr) <> '') then
        begin
          LLangFieldName := LFieldProperty.FieldLangDescr;
          LField.DisplayLabel := LLangFieldName;
          LField.DisplayWidth := Max(Length(LLangFieldName), LFieldProperty.FieldWidth);
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TGridDataDataSets.GetDataSet(ADataSetIndex: integer): TAbstractModelDataset;
const OPNAME = 'TGridDataDataSets.GetDataSet';
begin
  Result := nil;
  try
    if (FDataSets.Count > ADataSetIndex) then
      Result := TAbstractModelDataset(FDataSets.Objects[ADataSetIndex]);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TGridDataDataSets.AllViewDataSetsFirst;
const OPNAME = 'TGridDataDataSets.AllViewDataSetsFirst';
var LDataSetIndex: integer;
begin
  try
    for LDataSetIndex := 0 to FDataSets.Count - 1 do
    begin
      try
        if DataSet[LDataSetIndex].DataSet.Active then
          DataSet[LDataSetIndex].DataSet.First;
      except end; // Ignore errors here because the dataset may be empty.
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TGridDataDataSets.AllViewDataSetsNext;
const OPNAME = 'TGridDataDataSets.AllViewDataSetsNext';
var LDataSetIndex: integer;
begin
  try
    for LDataSetIndex := 0 to FDataSets.Count - 1 do
      DataSet[LDataSetIndex].DataSet.Next;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TGridDataDataSets.IsViewDataSetEOF: boolean;
const OPNAME = 'TGridDataDataSets.IsViewDataSetEOF';
var LDataSetIndex: integer;
begin
  Result := False;
  try
    for LDataSetIndex := 0 to FDataSets.Count - 1 do
    begin
      if (DataSet[LDataSetIndex].DataSet.EOF) then
      begin
        Result := True;
        break;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TGridDataDataSets.ExtractDataSetData;
const OPNAME = 'TGridDataDataSets.ExtractDataSetData';
var
  LVisibleFieldIndex, LFieldIndex, LSubFieldIndex, LDatasetIndex: integer;
  LFieldFields, LContextData: TStringList;
  LDataset: TDataset;
  LIsFirstRecord: boolean;
begin
  try

    // Position all datasets at the first record.
    LIsFirstRecord := True;
    AllViewDataSetsFirst;
    LFieldFields := TStringList.Create;
    try

      // Process every record of every data set.
      while (not IsViewDataSetEOF) do
      begin

        // Loop for all data sets to extract field property information.
        LVisibleFieldIndex := -1;
        for LDatasetIndex := 0 to DataSetCount - 1 do
        begin
          LDataset := DataSet[LDatasetIndex].DataSet;

          // Loop for all fields to create the grid field objects.
          for LFieldIndex := 0 to LDataset.FieldCount - 1 do
          begin
            if LDataSet.Fields[LFieldIndex].Visible then
            begin
              Inc(LVisibleFieldIndex);

              // Extract the field data into a string list.
              ExtractFields(Trim(LDataset.Fields[LFieldIndex].DisplayText), ',', LFieldFields);

              // Add grid field object.
              if LIsFirstRecord then
                AddGridField(LDataset.Fields[LFieldIndex].FieldName);

              // Put the data into the object for non pivoted fields.
              if (LFieldFields.Count <= 1) then
              begin
                CreateContextData(LContextData, LDataset, 0);
                if (LFieldFields.Count < 1) then
                begin
                  GridField[LVisibleFieldIndex].AddFieldData('', LContextData, -1);
                end else begin
                  GridField[LVisibleFieldIndex].AddFieldData(LFieldFields[0], LContextData, -1);
                end;
              end else begin

                // Put the data into the object for pivoted fields.
                for LSubFieldIndex := 0 to LFieldFields.Count - 1 do
                begin
                  CreateContextData(LContextData, LDataset, LSubFieldIndex);
                  GridField[LVisibleFieldIndex].AddFieldData(LFieldFields[LSubFieldIndex], LContextData, LSubFieldIndex);
                end;
              end;
            end;
          end;
        end;

        // Goto the next record on every data set.
        AllViewDataSetsNext;
        LIsFirstRecord := False;
      end;

    // Done.
    finally
     LFieldFields.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TGridDataDataSets.CreateContextData(
  var AStringList: TStringList; ADataSet: TDataSet; APivotIndex: integer);
const OPNAME = 'TGridDataDataSets.CreateContextData';
var
  LFieldName: string;
  LFieldIndex: integer;
  LFieldFields: TStringList;
begin
  AStringList := TStringList.Create;
  try
    LFieldFields := TStringList.Create;
    try

      // Only add invisible fields as context data.
      for LFieldIndex := 0 to ADataSet.FieldCount - 1 do
      begin
        if (not ADataSet.Fields[LFieldIndex].Visible) then
        begin

          // Extract the field data into a string list.
          ExtractFields(Trim(ADataset.Fields[LFieldIndex].DisplayText), ',', LFieldFields);

          // Add the field name value pair for the required pivot field.
      //    if (LFieldFields.Count = 1) then
       //   begin
            LFieldName := Trim(ADataSet.Fields[LFieldIndex].FullName);
        //  end else begin
          //  LFieldName := Trim(ADataSet.Fields[LFieldIndex].FullName) + Format('%2.2d', [APivotIndex]);
        //  end;

          // Make sure that there are no duplicate context fields.
          if (AStringList.IndexOfName(LFieldName) >= 0) then
          begin
            AStringList.Clear;
            raise Exception.CreateFmt('Duplicate context field [%s]', [LFieldName]);
          end;

          // Add the field name value pair for the required pivot field.
          if (LFieldFields.Count = 1) then
          begin
            AStringList.Add(LFieldName + '=' + Trim(LFieldFields[0]));
          end else begin
          if (LFieldFields.Count > 0) then
            AStringList.Add(LFieldName + '=' + Trim(LFieldFields[APivotIndex]));
          end;
        end;
      end;
    finally
      LFieldFields.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
