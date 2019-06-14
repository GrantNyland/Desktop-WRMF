//
//  UNIT      : Contains TFieldProperty Class
//  AUTHOR    : Grant Nyland (PDNA)
//  DATE      : 2002/03/16
//  COPYRIGHT : Copyright © 2002 DWAF
//
unit UFieldPropertyManager;

interface

uses
  Classes,
  Contnrs,
  UAbstractObject,
  UTablePropertyList,
  UFieldPropertyList;

type
  TFieldPropertyManager = class(TAbstractFieldsProperties)
  protected
    FTableProperties: TTablePropertyList;
    FFieldProperties: TFieldPropertyList;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    function UpdateMemoField(AFieldName,AFieldValue: string;AUpdateDataset: TAbstractModelDataset): boolean;
    function ConstructUpdateFieldName(
      var AUpdateFieldName: string; AContextData: TStrings): boolean;
    function PerformFieldValueUpdateStep(ANewValue: string;
      AFieldProperty: TAbstractFieldProperty;
      AFieldUpdateSQLStep: TAbstractFieldUpdateSQLStep;
      AContextData: TStrings): boolean;
    function ExecuteUpdateSQL(ASQL: string; APrimaryKeys, AContextData: TStrings): boolean;
    function ExecuteGetValueSQL(ASQL: string;
      APrimaryKeys, AContextData: TStrings; var ANewValue: string): boolean;
    function ExecuteUpdateValueSQL(ASQL: string;
      APrimaryKeys, AContextData: TStrings; AUpdateFieldName, ANewValue: string): boolean;
    function ConstructUpdateDataSet(ASQL: string; APrimaryKeys, AContextData: TStrings;
      var AUpdateDataset: TAbstractModelDataset): boolean;
    function PostFieldUpdate(AFieldInTable, ANewValue: string;
      AUpdateDataset: TAbstractModelDataset): boolean;
    function MaximumFieldLabel(AFieldProperty:TAbstractFieldProperty): string;
    function MinimumFieldLabel(AFieldProperty:TAbstractFieldProperty): string;
    function ValidateField(AFieldProperty:TAbstractFieldProperty;AFieldValue: string;ADimOneIndex: integer;ADimTwoIndex: integer; var AErrorMessage: string): boolean;
    function ValidateArrayField(AFieldProperty:TAbstractFieldProperty;ADimOneIndex: integer;ADimTwoIndex: integer; var AErrorMessage: string): boolean;
    function ValidateMinMaxField(AFieldProperty:TAbstractFieldProperty;AFieldValue: string; var AErrorMessage: string): boolean;
    function ValidateMinMaxString(AFieldProperty:TAbstractFieldProperty;AFieldValue: string; var AErrorMessage: string): boolean;
    function ValidateRangeField(AFieldProperty:TAbstractFieldProperty;AFieldValue: string; var AErrorMessage: string): boolean;
{    function BuildFieldName (AFieldName  : string;
                             AFieldIndex : string) : string;}
  public
    function Initialise: boolean; override;
    function LanguageHasChanged: boolean; override;
    function FieldProperty(AFieldName: string): TAbstractFieldProperty; override;
    function ValidateFieldProperty(AFieldName,AFieldValue: string; var AErrorMessage: string;
             ADimOneIndex: integer = -1; ADimTwoIndex: integer = -1): boolean; override;
    function UpdateFieldValue(
      AFieldName, ANewValue, AOldValue: string; AContextData: TStrings): boolean; override;
    function FieldPropertyFromFileReference(
      AFileName,ALineNo: string; ACharacterPosition: integer): TAbstractFieldProperty; override;
    function GetFieldPropertiesList(AFieldList: TObjectList;AFieldGroup : TFieldGroup = fgNone): boolean; override;
    function GetArrayFieldPropertiesList(AFieldList: TObjectList): boolean; override;
    function ModeIsEditable(AFieldName: string): boolean; override;
    function FieldAvailableInModel(AFieldName: string): boolean; override;
    function LoadFieldFileReferencesData(AModelName: string): boolean; override;

{    function GetBaseValue (AFieldPropName : string;
                           AKeyValues     : string;
                           AFieldIndex    : string) : string; override;}
  end;

implementation

uses
  DB,
  SysUtils,
  UConstants,
  UDataSetType,
  UFieldProperty,
  UDBConstants,
  UErrorHandlingOperations;

procedure TFieldPropertyManager.CreateMemberObjects;
const OPNAME = 'TFieldPropertyManager.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
    FTableProperties := TTablePropertyList.Create(FAppModules);
    FFieldProperties := TFieldPropertyList.Create(FAppModules);
    FFieldProperties.TablePropertyList := FTableProperties;
    FLastFieldToChange := TStudyDataHasChangedData.Create;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFieldPropertyManager.DestroyMemberObjects;
const OPNAME = 'TFieldPropertyManager.DestroyMemberObjects';
begin
  try
    FreeAndNil(FFieldProperties);
    FreeAndNil(FTableProperties);
    FreeAndNil(FLastFieldToChange);
    inherited DestroyMemberObjects;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFieldPropertyManager.FieldProperty(AFieldName: string): TAbstractFieldProperty;
const OPNAME = 'TFieldPropertyManager.FieldProperty';
begin
  Result := nil;
  try
    Result := FFieldProperties[AFieldName];
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFieldPropertyManager.FieldPropertyFromFileReference(
  AFileName, ALineNo: string; ACharacterPosition: integer): TAbstractFieldProperty;
const OPNAME = 'TFieldPropertyManager.FieldPropertyFromFileReference';
begin
  Result := nil;
  try
    Result := FFieldProperties.GetFieldPropertyFromFileReference(AFileName, ALineNo, ACharacterPosition);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFieldPropertyManager.Initialise: boolean;
const OPNAME = 'TFieldPropertyManager.Initialise';
begin
  Result := False;
  try
    FTableProperties.Initialise;
    FFieldProperties.Initialise;
    FFieldProperties.LanguageHasChanged;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFieldPropertyManager.LanguageHasChanged: boolean;
const OPNAME = 'TFieldPropertyManager.LanguageHasChanged';
begin
  Result := False;
  try
    Result := FFieldProperties.LanguageHasChanged;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFieldPropertyManager.ConstructUpdateFieldName(
  var AUpdateFieldName: string; AContextData: TStrings): boolean;
const OPNAME = 'TFieldPropertyManager.ConstructUpdateFieldName';
var
  LPosOfComma, LPosOfEqualSign, LSourceDataType: integer;
  LSourceFieldName, LFieldFormatString: string;
begin
  Result := False;
  try

    // Layout: FieldName,Type=FormatString
    LPosOfComma     := Pos(',', AUpdateFieldName);
    LPosOfEqualSign := Pos('=', AUpdateFieldName);
    if (LPosOfComma <= 0) or (LPosOfEqualSign <= 0) then
    begin
      Result := True;
    end else begin
      LSourceFieldName := Copy(AUpdateFieldName, 1, LPosOfComma - 1);
      LSourceDataType := StrToInt(Copy(AUpdateFieldName, LPosOfComma + 1,1));
      LFieldFormatString := Copy(AUpdateFieldName, LPosOfEqualSign + 1, Length(AUpdateFieldName));
      if (AContextData.IndexOfName(LSourceFieldName) < 0) then
      begin
        raise Exception.CreateFmt('Field name identifier is missing from context data [%s].', [LSourceFieldName]);
      end else begin
        case LSourceDataType of
          0 : AUpdateFieldName := Format(LFieldFormatString, [AContextData.Values[LSourceFieldName]]);
          1 : AUpdateFieldName := Format(LFieldFormatString, [StrToInt(AContextData.Values[LSourceFieldName])]);
          2 : AUpdateFieldName := Format(LFieldFormatString, [StrToFloat(AContextData.Values[LSourceFieldName])]);
        else
          raise Exception.CreateFmt('Unknown update field type [%d].', [LSourceDataType]);
        end;
        Result := True;
      end;
    end;
  except on E: Exception do HandleErrorFunction(E, OPNAME, Result) end;
end;

function TFieldPropertyManager.UpdateFieldValue(AFieldName, ANewValue, AOldValue: string; AContextData: TStrings): boolean;
const OPNAME = 'TFieldPropertyManager.UpdateFieldValue';
var
  LImportDate    : TDateTime;
  LChangedBy     : string;
  LFieldProperty : TAbstractFieldProperty;
  LSQLStatementIndex, LLastSuccessfulIndex: integer;
begin
  Result := False;
  try

    // Make sure that the current user can edit.
    if  ModeIsEditable(AFieldName) then
    begin

      // Reset in case of failure.
      LChangedBy := FLastFieldToChange.ChangedBy;
      FLastFieldToChange.Reset;

      // Get the field property object.
      LFieldProperty := FieldProperty(AFieldName);

      // Perform the update.
      if (not Assigned(LFieldProperty)) then
      begin
        raise Exception.CreateFmt('Could not find field properties for field [%s].', [AFieldName]);
      end else begin
        if (LFieldProperty.FieldUpdateSQLCount <= 0) then
        begin
          raise Exception.CreateFmt('Field [%s] has no update SQL.', [AFieldName]);
        end else begin

          // Loop for every SQL step in the list.
          LLastSuccessfulIndex := -1;
          for LSQLStatementIndex := 0 to LFieldProperty.FieldUpdateSQLCount - 1 do
          begin
            if PerformFieldValueUpdateStep(ANewValue, LFieldProperty,
                LFieldProperty.FieldUpdateSQL[LSQLStatementIndex], AContextData) then
            begin
              LLastSuccessfulIndex := LSQLStatementIndex;
            end else begin
              break;
            end;
          end;

          // Check if all statements executed.
          if (LLastSuccessfulIndex = (LFieldProperty.FieldUpdateSQLCount - 1)) then
          begin

            // Populate the LastFieldToChange object.
            FLastFieldToChange.FieldProperty := LFieldProperty;
            FLastFieldToChange.NewValue  := ANewValue;
            FLastFieldToChange.OldValue  := AOldValue;
            FLastFieldToChange.Action    := saUpdate;
            FLastFieldToChange.Severity  := scsMinor;
            FLastFieldToChange.ChangedBy := LChangedBy;

            if ANewValue <> AOldValue then
              FAppModules.StudyArea.LastUpdateDate := Now();

            LImportDate := FAppModules.StudyArea.GetStudyImportDate;
            if LImportDate = NullDateTime then
              FAppModules.StudyArea.SetStudyImportDate(FAppModules.StudyArea.LastUpdateDate);

            // Done.
            Result := True;
          end else begin

            // Undo in reverse order.
            for LSQLStatementIndex := LLastSuccessfulIndex downto 0 do
              PerformFieldValueUpdateStep(AOldValue, LFieldProperty,
                LFieldProperty.FieldUpdateSQL[LSQLStatementIndex], AContextData);
          end;
        end;
      end;
    end;

  // Handle exceptions.
  except on E: Exception do HandleErrorFunction(E, OPNAME, Result) end;
end;

function TFieldPropertyManager.PerformFieldValueUpdateStep(ANewValue: string;
  AFieldProperty: TAbstractFieldProperty;
  AFieldUpdateSQLStep: TAbstractFieldUpdateSQLStep;
  AContextData: TStrings): boolean;
const OPNAME = 'TFieldPropertyManager.PerformFieldValueUpdateStep';
var LUpdateFieldName: string;
begin
  Result := False;
  try

    // Check if this is an update SQL.
    if (AFieldUpdateSQLStep.FieldInTable = '') then
    begin
      Result := ExecuteUpdateSQL(AFieldUpdateSQLStep.UpdateSQL,
        AFieldUpdateSQLStep.TableProperty.PrimaryKeys, AContextData);
    end else begin

      // Check if a GetValueSQL is required to override the new value.
      if (AFieldUpdateSQLStep.GetValueSQL = '') or
         ExecuteGetValueSQL(AFieldUpdateSQLStep.GetValueSQL,
           AFieldUpdateSQLStep.TableProperty.PrimaryKeys, AContextData, ANewValue) then
      begin

        // Costruct the field name if required.
        LUpdateFieldName := AFieldUpdateSQLStep.FieldInTable;
        if ConstructUpdateFieldName(LUpdateFieldName, AContextData) then
        begin

          // Execute the field update SQL.
          Result := ExecuteUpdateValueSQL(AFieldUpdateSQLStep.UpdateSQL,
            AFieldUpdateSQLStep.TableProperty.PrimaryKeys, AContextData, LUpdateFieldName, ANewValue);
        end;
      end;
    end;

  // Handle exceptions.
  except on E: Exception do HandleErrorFunction(E, OPNAME, Result) end;
end;

function TFieldPropertyManager.ExecuteUpdateSQL(ASQL: string; APrimaryKeys, AContextData: TStrings): boolean;
const OPNAME = 'TFieldPropertyManager.ExecuteUpdateSQL';
var LUpdateDataset: TAbstractModelDataset;
begin
  Result := False;
  try
    if ConstructUpdateDataSet(ASQL, APrimaryKeys, AContextData, LUpdateDataset) then
    begin
      try
        LUpdateDataset.ExecSQL;
        Result := True;
      finally
        LUpdateDataset.Free;
      end;
    end;
  except on E: Exception do HandleErrorFunction(E, OPNAME, Result) end;
end;

function TFieldPropertyManager.ExecuteGetValueSQL(ASQL: string;
  APrimaryKeys, AContextData: TStrings; var ANewValue: string): boolean;
const OPNAME = 'TFieldPropertyManager.ExecuteGetValueSQL';
var LUpdateDataset: TAbstractModelDataset;
begin
  Result := False;
  ANewValue := '';
  try
    if ConstructUpdateDataSet(ASQL, APrimaryKeys, AContextData, LUpdateDataset) then
    begin
      try
        LUpdateDataset.DataSet.Open;
        try
          ANewValue := Trim(LUpdateDataset.DataSet.Fields[0].AsString);

          // These SQL's must always return a value.
          if (ANewValue = '') then
          begin
            ReportError('The GetValueSQL did not return a value [' + ASQL + '].', OPNAME);
          end else begin

            // Done.
            Result := True;
          end;
        finally
          LUpdateDataset.DataSet.Close;
        end;
      finally
        LUpdateDataset.Free;
      end;
    end;
  except on E: Exception do HandleErrorFunction(E, OPNAME, Result) end;
end;

function TFieldPropertyManager.ExecuteUpdateValueSQL(ASQL: string;
  APrimaryKeys, AContextData: TStrings; AUpdateFieldName, ANewValue: string): boolean;
const OPNAME = 'TFieldPropertyManager.ExecuteUpdateValueSQL';
var LUpdateDataset: TAbstractModelDataset;
begin
  Result := False;
  try
    if ConstructUpdateDataSet(ASQL, APrimaryKeys, AContextData, LUpdateDataset) then
    begin
      try
        Result := PostFieldUpdate(AUpdateFieldName, ANewValue, LUpdateDataset);
      finally
        LUpdateDataset.Free;
      end;
    end;
  except on E: Exception do HandleErrorFunction(E, OPNAME, Result) end;
end;

function TFieldPropertyManager.ConstructUpdateDataSet(ASQL: string; APrimaryKeys, AContextData: TStrings;
  var AUpdateDataset: TAbstractModelDataset): boolean;
const OPNAME = 'TFieldPropertyManager.ConstructUpdateDataSet';
var LSQLParamIndex: integer;
begin
  Result := False;
  try

    // Request an empty dataset from the database layer.
    if FAppModules.Database.CreateDataset(integer(dtExecSQL), AUpdateDataset) then
    begin

      // Set the SQL and set all parameters.
      AUpdateDataset.SetSQL(ASQL);
      FAppModules.StudyArea.SetDefaultParams(AUpdateDataset);
      if Assigned(APrimaryKeys) and Assigned(AContextData) then
      begin

        // Loop for every known primary key for the table in question.
        for LSQLParamIndex := 0 to AContextData.Count - 1 do
        begin
          AUpdateDataset.SetParams(
            ['A' + AContextData.Names[LSQLParamIndex]],
            [AContextData.ValueFromIndex[LSQLParamIndex]]);
        end;

        // Loop for every known primary key for the table in question.
       { for LSQLParamIndex := 0 to APrimaryKeys.Count - 1 do
        begin
          AUpdateDataset.SetParams(
            ['A' + APrimaryKeys[LSQLParamIndex]],
            [AContextData.Values[APrimaryKeys[LSQLParamIndex]]]);
        end;}
      end;

      // Done.
      Result := AUpdateDataset.AreAllParamsBound(True);
    end;

  // Handle exceptions.
  except on E: Exception do HandleErrorFunction(E, OPNAME, Result) end;
end;

function TFieldPropertyManager.PostFieldUpdate(AFieldInTable, ANewValue: string;
  AUpdateDataset: TAbstractModelDataset): boolean;
const OPNAME = 'TFieldPropertyManager.PostFieldUpdate';
var
  LField :TField;
begin
  Result := False;
  try
    LField := nil;

    // Open the dataset for editing.
    AUpdateDataset.DataSet.Open;
    AUpdateDataset.SetReadOnly(False);
    try

      // These update datasets must find at least one record.
      if (AUpdateDataset.DataSet.Eof and  AUpdateDataset.DataSet.Bof) then
      begin
        ReportError('Could not locate record to update [' + AFieldInTable + '].', OPNAME);
      end
      else
      begin

        // Loop for every record in the dataset.
        AUpdateDataset.DataSet.First;
        while (not AUpdateDataset.DataSet.Eof) do
        begin
          if (ANewValue <> '') then
          begin
            LField := AUpdateDataset.DataSet.FieldByName(AFieldInTable);
            if Assigned(LField) then
            begin
              case LField.DataType of
                ftSmallint, ftInteger, ftWord,ftAutoInc, ftBytes, ftVarBytes,ftLargeint:
                begin
                  if(IntToStr(NullInteger) = ANewValue) then
                    ANewValue := '';
                end;
                ftFloat, ftCurrency, ftBCD:
                begin
                  if(FloatToStr(NullFloat) = ANewValue) then
                    ANewValue := '';
                end;
                ftDate, ftTime, ftDateTime,ftTimeStamp:
                begin
                  if(FloatToStr(NullDateTime) = ANewValue) then
                    ANewValue := '';
                end;
                ftFixedChar:
                begin
                  if(NullChar = ANewValue) then
                    ANewValue := '';
                end;
              end
            end;
          end;

          AUpdateDataset.DataSet.Edit;


          // Apply or clear the value.
          if (ANewValue <> '') then
          begin
            if(LField <> nil) and (LField.DataType = ftMemo) and (Length(ANewValue) > 255) then
               UpdateMemoField(AFieldInTable, ANewValue,AUpdateDataset)
            else
              AUpdateDataset.DataSet.FieldByName(AFieldInTable).AsString := ANewValue;
          end
          else
          begin
            AUpdateDataset.DataSet.FieldByName(AFieldInTable).Clear;
          end;

          // Post the value change.
          AUpdateDataset.DataSet.Post;
          AUpdateDataset.DataSet.Next;
        end;

        // Done.
        Result := True;
      end;
    finally
      AUpdateDataset.DataSet.Close;
    end;

  // Handle exceptions.
  except on E: Exception do HandleErrorFunction(E, OPNAME, Result) end;
end;

function TFieldPropertyManager.GetFieldPropertiesList(AFieldList: TObjectList;AFieldGroup : TFieldGroup = fgNone): boolean;
const OPNAME = 'TFieldPropertyManager.GetFieldPropertiesList';
var
  LIndex: integer;
  LField: TFieldProperty;
begin
  Result := False;
  try
    if Assigned(AFieldList) then
    begin
      AFieldList.OwnsObjects := False;
      AFieldList.Clear;
      for LIndex := 0 to FFieldProperties.FieldCount -1 do
      begin
        LField := FFieldProperties.FieldByIndex(LIndex);
        if Assigned(LField) and
          ((AFieldGroup = fgNone) or (LField.FieldGroup = AFieldGroup)) then
           AFieldList.Add(LField);
      end;
      Result := True;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFieldPropertyManager.GetArrayFieldPropertiesList(AFieldList: TObjectList): boolean;
const OPNAME = 'TFieldPropertyManager.GetArrayFieldPropertiesList';
var
  LIndex: integer;
  LField: TFieldProperty;
begin
  Result := False;
  try
    if Assigned(AFieldList) then
    begin
      AFieldList.OwnsObjects := False;
      AFieldList.Clear;
      for LIndex := 0 to FFieldProperties.FieldCount -1 do
      begin
        LField := FFieldProperties.FieldByIndex(LIndex);
        if Assigned(LField) and
          ((LField.FieldGroup = fgMinMaxArray) or (LField.FieldGroup = fgValidationArray)) then
           AFieldList.Add(LField);
      end;
      Result := True;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFieldPropertyManager.ValidateFieldProperty(AFieldName,AFieldValue: string; var AErrorMessage: string;
             ADimOneIndex: integer = -1; ADimTwoIndex: integer = -1): boolean;
const OPNAME = 'TFieldPropertyManager.ValidateFieldProperty';
var
  LFieldProperty: TAbstractFieldProperty;
begin
  Result := False;
  AErrorMessage := '';
  try
    if(Trim(AFieldName) = '') then
      AErrorMessage := FAppModules.language.GetString('Message.EmptyField')
    else
    begin
      LFieldProperty := FieldProperty(AFieldName);
      if not Assigned(LFieldProperty) then
        begin
          AErrorMessage := FAppModules.language.GetString('Field.FieldPropertyNotImplemented');
          AErrorMessage := Format(AErrorMessage,[AFieldName])
        end
      else
        Result := ValidateField(LFieldProperty,AFieldValue,ADimOneIndex,ADimTwoIndex,AErrorMessage);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFieldPropertyManager.ValidateField(AFieldProperty: TAbstractFieldProperty;
         AFieldValue: string;ADimOneIndex: integer;ADimTwoIndex: integer; var AErrorMessage: string): boolean;
const OPNAME = 'TFieldPropertyManager.ValidateField';
begin
  Result := False;
  AErrorMessage := '';
  try
   if not Assigned(AFieldProperty) then
      AErrorMessage := FAppModules.language.GetString('Field.FieldParameterNotAssigned')
    else
    begin
{      if (AFieldProperty.FieldGroup in [fgArray, fgMinMaxArray,fgValidationArray,fgMinMaxValidationArray]) then
      begin
        Result :=  ValidateMinMaxField(AFieldProperty,AFieldValue,AErrorMessage) and ValidateArrayField(AFieldProperty,ADimOneIndex,ADimTwoIndex,AErrorMessage);
      end
      else
        case AFieldProperty.FieldGroup of
        fgNone                  : Result := True;
        fgMinMax,
        fgMinMaxArray           : Result := ValidateMinMaxField(AFieldProperty,AFieldValue,AErrorMessage);
        fgValidation,
        fgValidationArray       : Result := ValidateRangeField(AFieldProperty,AFieldValue,AErrorMessage);
        fgMinMaxString          : Result := ValidateMinMaxString(AFieldProperty,AFieldValue,AErrorMessage);
        fgMinMaxValidation,
        fgMinMaxValidationArray : Result := ValidateMinMaxField(AFieldProperty,AFieldValue,AErrorMessage) and
                                            ValidateRangeField(AFieldProperty,AFieldValue,AErrorMessage);
        else
          AErrorMessage := FAppModules.language.GetString('Field.FieldPropertyNotImplemented');
          AErrorMessage := '1 :' + Format(AErrorMessage,[AFieldProperty.FieldName]);
        end;//case
}

      case AFieldProperty.FieldGroup of
      fgNone                  :
        Result := True;
      fgMinMax                :
        Result := ValidateMinMaxField(AFieldProperty,AFieldValue,AErrorMessage);
      fgArray           :
        Result := ValidateArrayField(AFieldProperty,ADimOneIndex,ADimTwoIndex,AErrorMessage) AND
                  ValidateMinMaxField(AFieldProperty,AFieldValue,AErrorMessage);
      fgMinMaxArray           :
        Result := ValidateArrayField(AFieldProperty,ADimOneIndex,ADimTwoIndex,AErrorMessage) AND
                  ValidateMinMaxField(AFieldProperty,AFieldValue,AErrorMessage);
      fgValidation            :
        Result := ValidateRangeField(AFieldProperty,AFieldValue,AErrorMessage);
      fgValidationArray       :
        Result := ValidateArrayField(AFieldProperty,ADimOneIndex,ADimTwoIndex,AErrorMessage) AND
                  ValidateRangeField(AFieldProperty,AFieldValue,AErrorMessage);
      fgMinMaxString          :
        Result := ValidateMinMaxString(AFieldProperty,AFieldValue,AErrorMessage);
      fgMinMaxValidation      :
        Result := ValidateMinMaxField(AFieldProperty,AFieldValue,AErrorMessage) OR
                  ValidateRangeField(AFieldProperty,AFieldValue,AErrorMessage);
      fgMinMaxValidationArray :
        Result := ValidateArrayField(AFieldProperty,ADimOneIndex,ADimTwoIndex,AErrorMessage) AND
                  (ValidateMinMaxField(AFieldProperty,AFieldValue,AErrorMessage) OR
                   ValidateRangeField(AFieldProperty,AFieldValue,AErrorMessage));
      else
        AErrorMessage := FAppModules.language.GetString('Field.FieldPropertyNotImplemented');
        AErrorMessage := '1 :' + Format(AErrorMessage,[AFieldProperty.FieldName]);
      end;//case}
    end;

    Result := AErrorMessage = '';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFieldPropertyManager.ValidateMinMaxField(AFieldProperty: TAbstractFieldProperty;
         AFieldValue: string; var AErrorMessage: string): boolean;
const OPNAME = 'TFieldPropertyManager.ValidateMinMaxField';
var
   LFloat   : Extended;
   LInteger : Integer;
   LDTime   : TDateTime;
begin
  Result := False;
  AErrorMessage := '';
  try
    if not Assigned(AFieldProperty) then
      AErrorMessage := FAppModules.language.GetString('Field.FieldParameterNotAssigned')
    else
    begin
      case AFieldProperty.FieldDataType of
         FieldFloatType   :
           begin
             LFloat := 0;
             try
               LFloat := StrToFloat(AFieldValue);
             except
               AErrorMessage := FAppModules.language.GetString('Field.InvalidFloatvalue');
               AErrorMessage := Format(AErrorMessage,[AFieldProperty.FieldName]);
             end;

             if (LFloat <> NullFloat) then
             begin
               if (AFieldProperty.FieldMinimumValue <> NegativeInf) then
                 if(LFloat < StrToFloat(AFieldProperty.FieldMinimumValue)) then
                 begin
                    AErrorMessage := FAppModules.language.GetString('Field.FieldOutOfRange');
                    AErrorMessage := Format(AErrorMessage,[AFieldProperty.FieldName,
                        MinimumFieldLabel(AFieldProperty),MaximumFieldLabel(AFieldProperty)]);
                 end;

               if (AFieldProperty.FieldMaximumValue <> PositiveInf) then
                 if (LFloat > StrToFloat(AFieldProperty.FieldMaximumValue)) then
                 begin
                    AErrorMessage := FAppModules.language.GetString('Field.FieldOutOfRange');
                    AErrorMessage := Format(AErrorMessage,[AFieldProperty.FieldName,
                          MinimumFieldLabel(AFieldProperty),MaximumFieldLabel(AFieldProperty)]);
                 end;
             end;
           end;

         FieldIntegerType :
         begin
           LInteger := 0;
           try
             LInteger := StrToInt(AFieldValue);
           except
             AErrorMessage := FAppModules.language.GetString('Field.InvalidIntegerValue');
             AErrorMessage := Format(AErrorMessage,[AFieldProperty.FieldName]);
           end;

           if (LInteger <> NullInteger) then
           begin
             if (AFieldProperty.FieldMinimumValue <> NegativeInf) then
               if(LInteger < StrToInt(AFieldProperty.FieldMinimumValue)) then
               begin
                  AErrorMessage := FAppModules.language.GetString('Field.FieldOutOfRange');
                  AErrorMessage := Format(AErrorMessage,[AFieldProperty.FieldName,
                       MinimumFieldLabel(AFieldProperty),MaximumFieldLabel(AFieldProperty)])
               end;

             if (AFieldProperty.FieldMaximumValue <> PositiveInf) then
               if (LInteger > StrToInt(AFieldProperty.FieldMaximumValue)) then
               begin
                 AErrorMessage := FAppModules.language.GetString('Field.FieldOutOfRange');
                 AErrorMessage := Format(AErrorMessage,[AFieldProperty.FieldName,
                                  MinimumFieldLabel(AFieldProperty),MaximumFieldLabel(AFieldProperty)]);
               end;
           end;
         end;


         FieldDTimeType   :
         begin
           LDTime := 0.0;
           try
             LDTime := StrToDateTime(AFieldValue);
           except
             AErrorMessage := FAppModules.language.GetString('Field.InvalidDate/TimeValue');
             AErrorMessage := Format(AErrorMessage,[AFieldProperty.FieldName]);
           end;

           if(LDTime <> NullDateTime) then
           begin
             if (AFieldProperty.FieldMinimumValue <> NegativeInf) then
               if(LDTime < StrToDatetime(AFieldProperty.FieldMinimumValue)) then
               begin
                  AErrorMessage := FAppModules.language.GetString('Field.FieldOutOfRange');
                  AErrorMessage := Format(AErrorMessage,[AFieldProperty.FieldName,
                                  MinimumFieldLabel(AFieldProperty),MaximumFieldLabel(AFieldProperty)]);
               end;

             if (AFieldProperty.FieldMaximumValue <> PositiveInf) then
               if (LDTime > StrToDatetime(AFieldProperty.FieldMaximumValue)) then
               begin
                 AErrorMessage := FAppModules.language.GetString('Field.FieldOutOfRange');
                 AErrorMessage := Format(AErrorMessage,[AFieldProperty.FieldName,
                                  MinimumFieldLabel(AFieldProperty),MaximumFieldLabel(AFieldProperty)]);
               end;
           end;
         end;
      else
         AErrorMessage := FAppModules.language.GetString('Field.FieldPropertyNotImplemented');
         AErrorMessage := '2 :' + Format(AErrorMessage,[AFieldProperty.FieldName]);
      end;//Case
    end;
    Result := AErrorMessage = '';
  except on E: Exception do HandleError(E, OPNAME) end;
end;


function TFieldPropertyManager.ValidateArrayField(AFieldProperty: TAbstractFieldProperty;
         ADimOneIndex: integer;ADimTwoIndex: integer; var AErrorMessage: string): boolean;
const OPNAME = 'TFieldPropertyManager.ValidateArrayField';
var
  LArrayFiledName: string;
  LArrayFieldProperty: TAbstractFieldProperty;
begin
  Result := False;
  AErrorMessage := '';
  try
    if not Assigned(AFieldProperty) then
      AErrorMessage := FAppModules.language.GetString('Field.FieldParameterNotAssigned')
    else
    begin
      if(Trim(AFieldProperty.FieldName) = '') then
        AErrorMessage := FAppModules.language.GetString('Message.EmptyField')
      else
      begin
        LArrayFiledName := AFieldProperty.FieldName;
        LArrayFieldProperty := FieldProperty(LArrayFiledName);
        if not Assigned(LArrayFieldProperty) then
          AErrorMessage := FAppModules.language.GetString('Field.FieldParameterNotAssigned')
        else
        begin
          if (ADimOneIndex < AFieldProperty.ArrayLow) or
             (ADimOneIndex > AFieldProperty.ArrayHigh) then
             begin
               AErrorMessage := FAppModules.language.GetString('Field.IndexOutOfBounds');
               AErrorMessage := Format(AErrorMessage,[AFieldProperty.FieldName,IntTostr(ADimOneIndex),
                   IntToStr(AFieldProperty.ArrayLow),IntToStr(AFieldProperty.ArrayHigh)]);
             end;
          if(ADimTwoIndex >= 0) then
          begin
            if(ADimTwoIndex < AFieldProperty.ArrayLowDimTwo) or
              (ADimTwoIndex > AFieldProperty.ArrayHighDimTwo) then
            begin
                AErrorMessage := FAppModules.language.GetString('Field.IndexOutOfBounds');
                AErrorMessage := Format(AErrorMessage,[AFieldProperty.FieldName,IntTostr(ADimTwoIndex),
                     IntToStr(AFieldProperty.ArrayLowDimTwo),IntToStr(AFieldProperty.ArrayHighDimTwo)])
            end;
          end;
        end;
      end;
    end;
    Result := AErrorMessage = '';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFieldPropertyManager.ValidateRangeField(AFieldProperty: TAbstractFieldProperty;
         AFieldValue: string; var AErrorMessage: string): boolean;
const OPNAME = 'TFieldPropertyManager.ValidateRangeField';
var
   LContainer  :TStringList;
begin
  Result := False;
  AErrorMessage := '';
  try
    if not Assigned(AFieldProperty) then
      AErrorMessage := FAppModules.language.GetString('Field.FieldParameterNotAssigned')
    else
    if (NOT(AFieldProperty.FieldDataType in [FieldStringType]) AND (AFieldValue = '')) then
      begin
        AErrorMessage := FAppModules.language.GetString('Field.EmptyFieldValue');
        AErrorMessage := Format(AErrorMessage,[AFieldProperty.FieldName])
      end
    else
    begin
      LContainer := TStringList.Create;
      try
        LContainer.CommaText := AFieldProperty.FieldAcceptedValues;
        case AFieldProperty.FieldDataType of
          FieldStringType:
          begin
            Result := (AFieldValue = NullString);
            if not Result then
              Result := (LContainer.IndexOf(AFieldValue) >= 0);
          end;
          FieldFloatType:
          begin
            Result := (AFieldValue = FloatToStr(NullFloat));
            if not Result then
              Result := (LContainer.IndexOf(AFieldValue) >= 0);
          end;
          FieldIntegerType:
          begin
            Result := (AFieldValue = IntToStr(NullInteger));
            if not Result then
              Result := (LContainer.IndexOf(AFieldValue) >= 0);
          end;
          FieldDTimeType:
          begin
            Result := (AFieldValue = FloatToStr(NullDateTime));
            if not Result then
              Result := (LContainer.IndexOf(AFieldValue) >= 0);
          end;
          FieldCharType:
          begin
            Result := (AFieldValue = NullChar);
            if not Result then
              Result := (LContainer.IndexOf(AFieldValue) >= 0);
          end;
          FieldBoolType:
          begin
            Result := True;
          end;
        else
          AErrorMessage := FAppModules.language.GetString('Field.FieldPropertyNotImplemented');
          AErrorMessage := '3 :' + Format(AErrorMessage,[AFieldProperty.FieldName]);
        end;//Case

        if (AErrorMessage = '') and (not Result) then
           begin
             AErrorMessage := FAppModules.language.GetString('Field.ValuesNotAccepted');
             AErrorMessage := Format(AErrorMessage,[AFieldProperty.FieldName,
                              MinimumFieldLabel(AFieldProperty),MaximumFieldLabel(AFieldProperty)]);
           end;
      finally
        LContainer.Free;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFieldPropertyManager.ValidateMinMaxString(AFieldProperty: TAbstractFieldProperty;
         AFieldValue: string; var AErrorMessage: string): boolean;
const OPNAME = 'TFieldPropertyManager.ValidateMinMaxString';
var
   LString : String;
begin
  Result := False;
  AErrorMessage := '';
  try
    if not Assigned(AFieldProperty) then
      AErrorMessage := FAppModules.language.GetString('Field.FieldParameterNotAssigned')
    else
    begin
      LString := '';
      try
        LString := AFieldValue;
      except
        AErrorMessage := FAppModules.language.GetString('Field.EmptyFieldValue');
        AErrorMessage := Format(AErrorMessage,[AFieldProperty.FieldName])
      end;

      if (LString = '') and (StrToInt(AFieldProperty.FieldMinimumValue) > 0) then
        begin
          AErrorMessage := FAppModules.language.GetString('Field.EmptyFieldValue');
          AErrorMessage := Format(AErrorMessage,[AFieldProperty.FieldName])
        end
      else
        if (Length(LString) < StrToInt(AFieldProperty.FieldMinimumValue)) or
           (Length(LString) > StrToInt(AFieldProperty.FieldMaximumValue)) then
           begin
             AErrorMessage := FAppModules.language.GetString('Field.CharactersOutOfRange');
             AErrorMessage := Format(AErrorMessage,[AFieldProperty.FieldName,
                                    MinimumFieldLabel(AFieldProperty),MaximumFieldLabel(AFieldProperty)])
           end;
       end;
       Result := AErrorMessage = '';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFieldPropertyManager.MaximumFieldLabel(AFieldProperty: TAbstractFieldProperty): string;
const OPNAME = 'TFieldPropertyManager.MaximumFieldLabel';
begin
  Result := '';
  try
    if Assigned(AFieldProperty) then
    begin
      if(AFieldProperty.FieldMaximumValue = PositiveInf) then
        Result :=  '>0'
      else
       Result :=  AFieldProperty.FieldMaximumValue;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFieldPropertyManager.MinimumFieldLabel(AFieldProperty: TAbstractFieldProperty): string;
const OPNAME = 'TFieldPropertyManager.MinimumFieldLabel';
begin
  Result := '';
  try
    if Assigned(AFieldProperty) then
    begin
      if(AFieldProperty.FieldMinimumValue = NegativeInf) then
        Result :=  '<0'
      else
       Result :=  AFieldProperty.FieldMinimumValue;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFieldPropertyManager.ModeIsEditable(AFieldName: string): boolean;
const OPNAME = 'TFieldPropertyManager.ModeIsEditable';
var
  LValues: TStringList;
  LModel : string;
  LFieldProperty :TAbstractFieldProperty;
begin
  Result := False;
  try
    LFieldProperty := FieldProperty(AFieldName);
    if Assigned(LFieldProperty) then
    begin
      LValues := TStringList.Create;
      try
        LValues.CommaText := LFieldProperty.ModeIsEditable;
        LModel := FAppModules.studyarea.ModelSubCode;
        Result := (LValues.IndexOf(LModel) >= 0) and (FAppModules.User.UserRights in CUR_EditData);
        if Result then
        begin
          if(FAppModules.StudyArea <> nil) and (FAppModules.StudyArea.ScenarioLocked) then
            Result := False;
        end;
      finally
        LValues.Free;
      end;
    end;
   except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFieldPropertyManager.LoadFieldFileReferencesData(AModelName: string): boolean;
const OPNAME = 'TFieldPropertyManager.LoadFieldFileReferencesData';
begin
  Result := False;
  try
    Result := FFieldProperties.LoadFieldFileReferencesData(AModelName);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFieldPropertyManager.FieldAvailableInModel(AFieldName: string): boolean;
const OPNAME = 'TFieldPropertyManager.FieldAvailableInModel';
var
  LFieldProperty :TAbstractFieldProperty;
begin
  Result := False;
  try
    LFieldProperty := FieldProperty(AFieldName);
    if Assigned(LFieldProperty) then
    begin
      Result := FFieldProperties.FieldAvailableInModel(LFieldProperty);
    end;
   except on E: Exception do HandleError(E, OPNAME) end;
end;
(*
function TFieldPropertyManager.GetBaseValue (AFieldPropName : string;
                                             AKeyValues     : string;
                                             AFieldIndex    : string) : string;
const OPNAME = 'TFieldPropertyManager.GetBaseValue';
var
  lFieldProperty : TAbstractFieldProperty;
  lTableProperty : TAbstractTableProperty;
  lDataSet       : TAbstractModelDataset;
  lSQL           : string;
  lIndex         : integer;
  lCount         : integer;
  lIndexField    : string;
  lIndexVal      : string;
  lError         : Boolean;
  lFound         : Boolean;
  lFirst         : Boolean;
  lKeyValues     : TStringList;
  lFormatStr     : string;
  lBaseValue     : string;
  lFieldName     : string;
begin
  Result := '';
  try
    lFieldProperty := FieldProperty(AFieldPropName);
    if Assigned(lFieldProperty) then
    begin
      if ((lFieldProperty.DBTableName <> '') AND (lFieldProperty.DBFieldName <> '')) then
      begin
        lTableProperty := FTableProperties[lFieldProperty.DBTableName];
        lKeyValues     := TStringList.Create;
        try
          lKeyValues.CommaText := AKeyValues;
          lFieldName := BuildFieldName(lFieldProperty.DBFieldName, AFieldIndex);
          lSQL := 'SELECT ' + lFieldName + ' AS BaseValue ' +
                  ' FROM ' + lFieldProperty.DBTableName +
                  ' WHERE ';
          lError := FALSE;
          lFirst := TRUE;
          lIndex := 0;
          while ((NOT lError) AND (lIndex < lTableProperty.PrimaryKeys.Count)) do
          begin
            lIndexField := lTableProperty.PrimaryKeys.Strings[lIndex];
            lFound := FALSE;
            lCount := 0;
            while ((NOT lFound) AND (lCount < lKeyValues.Count)) do
            begin
              lIndexVal := lKeyValues.Strings[lCount];
              if (Pos(lIndexField, lIndexVal) > 0) then
                lFound := TRUE
              else
                lCount := lCount + 1;
            end;
            if (lFound) then
            begin
              if (lFirst) then
              begin
                lSQL := lSQL + lIndexVal;
                lFirst := FALSE;
              end
              else
                lSQL := lSQL + ' AND ' + lIndexVal;
              lIndex := lIndex + 1;
            end
            else
            begin
              lError := TRUE;
              ReportError(Format('Key field [%s] has not been set.', [lIndexField]), OPNAME);
            end;
          end;
          if (NOT lError) then
          begin
            FAppModules.Database.CreateDataset(integer(dtExecSQL), lDataSet);
            try
              if Assigned(lDataSet) then
              begin
                lDataset.DataSet.Close;
                lDataSet.SetSQL(lSQL);
                lDataSet.Dataset.Open;
                if (NOT lDataSet.DataSet.Eof) then
                begin
                  lBaseValue := Trim(lDataSet.DataSet.FieldByName('BaseValue').AsString);
                  lFormatStr := lFieldProperty.FormatStringGrid;
                  if (lFormatStr <> '') then
                  begin
                    case lFieldProperty.FieldDataType of
                      FieldStringType  : lBaseValue := Trim(Format(lFieldProperty.FormatStringGrid, [Trim(lBaseValue)]));
                      FieldFloatType   : try lBaseValue := Trim(Format(lFieldProperty.FormatStringGrid, [StrToFloat(Trim(lBaseValue))])); except end;
                      FieldIntegerType : try lBaseValue := Trim(Format(lFieldProperty.FormatStringGrid, [StrToInt(Trim(lBaseValue))])); except end;
                      FieldDTimeType   : ;
                      FieldCharType    : ;
                    else
                      raise Exception.CreateFmt('Unknown field data type [%d].', [lFieldProperty.FieldDataType]);
                    end;
                    Result := lBaseValue;
                  end
                  else
                    Result := lBaseValue;
                end;    
              end;
            finally
              FreeAndNil(lDataSet);
              FreeAndNil(lKeyValues);
            end;
          end;
        finally
          FreeAndNil(lKeyValues);
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFieldPropertyManager.BuildFieldName (AFieldName  : string;
                                               AFieldIndex : string) : string;
const OPNAME = 'TFieldPropertyManager.BuildFieldName';
var
  lFormatStr : string;
  lPos       : integer;
  lFieldIdx  : integer;
begin
  Result := '';
  try
    if (Pos('Index=', AFieldName) > 0) then
    begin
      lPos       := Pos('=', AFieldName);
      lFormatStr := Copy(AFieldName, lPos+1, Length(AFieldName)-lPos);
      lPos       := Pos(',', AFieldIndex);
      try
        if (lPos > 0) then
        begin
          lFieldIdx := StrToInt(Copy(AFieldIndex, lPos+1, Length(AFieldIndex)-lPos));
          Result    := Format(lFormatStr, [lFieldIdx]);
        end
        else
        begin
          lFieldIdx := StrToInt(AFieldIndex);
          Result    := Format(lFormatStr, [lFieldIdx]);
        end;
      except
      end;
    end
    else
      Result := AFieldName;
  except on E: Exception do HandleError(E, OPNAME) end;
end;
*)

function TFieldPropertyManager.UpdateMemoField(AFieldName,AFieldValue: string; AUpdateDataset: TAbstractModelDataset): boolean;
const OPNAME = 'TFieldPropertyManager.UpdateMemoField';
{var
  LBlobStream   : TStream;
  LMStream      : TMemoryStream;
  LDataContainer : TStringList;}
begin
  Result := False;
  try
    if(AUpdateDataset = nil) then Exit;
    if(AUpdateDataset.DataSet.State <> dsEdit) then Exit;
    if(AFieldName = '') or (AFieldValue = '') then Exit;
    AUpdateDataset.DataSet.FieldByName(AFieldName).AsWideString := AFieldValue;
    {LDataContainer := TStringList.Create;
    LMStream := TMemoryStream.Create;
    try
      LDataContainer.Add(AFieldValue);
      LMStream.Clear;
      LMStream.Position := 0;
      LDataContainer.SaveToStream ( LMStream );
      LBlobStream := AUpdateDataset.DataSet.CreateBlobStream(AUpdateDataset.DataSet.FieldByName(AFieldName),bmWrite);
      try
        LMStream.Position := 0;
        LBlobStream.CopyFrom(LMStream,LMStream.Size );
        Result := True;
      finally
        FreeAndNil ( LBlobStream );
      end;
    finally
      FreeAndNil ( LDataContainer );
      FreeAndNil ( LMStream );
    end;}
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
