//
//  UNIT      : Contains TFieldPropertyList Class
//  AUTHOR    : Grant Nyland (PDNA)
//  DATE      : 2002/03/16
//  COPYRIGHT : Copyright © 2002 DWAF
//
unit UFieldPropertyList;

interface

uses
  Classes,
  UFieldProperty,
  UTablePropertyList,
  UAbstractObject;

type
  TFieldPropertyList = class(TAbstractAppObject)
  protected
    FProperties: TStringList;
    FTableProperties: TTablePropertyList;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    procedure Clear;
    procedure AddFieldProperty (AFieldName           : string;
                                AFieldDescription    : string;
                                AModeIsEditable      : string;
                                AModelVersionNumber  : string;
                                AFieldDataType       : integer;
                                AFieldMinimumValue   : string;
                                AFieldMaximumValue   : string;
                                AFieldGroup          : TFieldGroup;
                                AFieldArrayLength    : string;
                                AFieldAcceptedValues : string;
                                AFieldType           : integer;
                                AFieldWidth          : integer;
                                ANumberOfDecimals    : integer;
                                ASmartFormat         : string;
                                AFormatStringGrid    : string;
                                AFormatStringGraph   : string;
                                AFieldUnits          : string;
                                AFileName            : string;
                                AFieldSource         : string;
                                AFileFieldName       : string);
    procedure AddFieldPropertyB (AFieldName      : string;
                                 ADBTableName    : string;
                                 ADBFieldName    : string;
                                 ADataClassName  : string;
                                 AInChangeList   : boolean);
    procedure AddUpdateSQLStep(
      AStepNo: integer; AFieldName, ATableName, AFieldInTable, AUpdateSQL, AGetValueSQL: string);
    procedure AddFieldFileReference(AFieldName, AFileName, ALineNo: string; AStartCharacter, ALength: integer);
    function GetFieldProperty(AFieldName: string): TFieldProperty;
  public
    function FieldCount: integer;
    function FieldByIndex(AIndex: integer):TFieldProperty;
    function Initialise: boolean; override;
    function LanguageHasChanged: boolean; override;
    function LoadFieldFileReferencesData(AModelName : string): boolean;
    function GetFieldPropertyFromFileReference(AFileName, ALineNo: string; ACharacterPosition: integer): TFieldProperty;
    function FieldAvailableInModel(AFieldProperty :TAbstractFieldProperty): boolean;
    property FieldProperty[AFieldName: string]: TFieldProperty read GetFieldProperty; default;
    property TablePropertyList: TTablePropertyList read FTableProperties write FTableProperties;
  end;

implementation

uses
  SysUtils,
  Contnrs,
  UTableProperty,
  UTableFieldData,
  UFieldUpdateSQL_NetworkVisualiser,
  UFieldUpdateSQL_ChangeLists,
  UFieldUpdateSQL_YieldModel_Channels,
  UFieldUpdateSQL_YieldModel_Configuration,
  UFieldUpdateSQL_YieldModel_Reservoirs,
  UFieldUpdateSQL_PlanningModel_FAM,
  UFieldUpdateSQL_YieldModel_Mining,
  UFieldUpdateSQL_YieldModel_GroundWater,
  UFieldFileReferenceData,
  UErrorHandlingOperations;

procedure TFieldPropertyList.CreateMemberObjects;
const OPNAME = 'TFieldPropertyList.CreateMemberObjects';
begin
  try
    FProperties := TStringList.Create;
    FProperties.Sorted := True;
    FProperties.Duplicates := dupAccept;
    FTableProperties := nil;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFieldPropertyList.DestroyMemberObjects;
const OPNAME = 'TFieldPropertyList.DestroyMemberObjects';
begin
  try
    Clear;
    FreeAndNil(FProperties);
    FTableProperties := nil;
    inherited DestroyMemberObjects;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFieldPropertyList.Clear;
const OPNAME = 'TFieldPropertyList.Clear';
var LIndex: integer;
begin
  try
    for LIndex := 0 to FProperties.Count - 1 do
    begin
      FProperties.Objects[LIndex].Free;
      FProperties.Objects[LIndex] := nil;
    end;
    FProperties.Clear;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFieldPropertyList.GetFieldProperty(AFieldName: string): TFieldProperty;
const OPNAME = 'TFieldPropertyList.GetFieldProperty';
var
  LCount : integer;
  LIndex: integer;
  LFieldName : string;
  LFieldProperty: TFieldProperty;
begin
  Result := nil;
  try
    // Attempt to find it with the full field name.
    if (Trim(AFieldName) <> '') then
    begin
      // Check for the generic version of the field name by stripping
      // off numeric characters at the end of the field name.
      LFieldName := Trim(AFieldName);
      LIndex := FProperties.IndexOf(AFieldName);
      if (LIndex < 0) then
        while (Length(LFieldName) > 0) and (CharInSet(AFieldName[length(LFieldName)], ['0'..'9'])) do
           LFieldName := Copy(LFieldName, 1, Length(LFieldName) -1);

      // Attempt to locate the object again.
      LIndex := FProperties.IndexOf(LFieldName);
      if (LIndex >= 0) then
      begin
        Result := TFieldProperty(FProperties.Objects[LIndex]);
        if not FieldAvailableInModel(Result) then
        begin
          For LCount := (LIndex+1) to FProperties.Count -1 do
          begin
            LFieldProperty := TFieldProperty(FProperties.Objects[LCount]);
            if (LFieldProperty.FieldName <> LFieldName) then
              Break
            else
            begin
               if FieldAvailableInModel(LFieldProperty) then 
               begin
                 Result :=  LFieldProperty;
                 Break;
               end;
            end;
          end;
        end;
      end;
    end;
  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFieldPropertyList.GetFieldPropertyFromFileReference(
  AFileName, ALineNo: string; ACharacterPosition: integer): TFieldProperty;
const OPNAME = 'TFieldPropertyList.GetFieldPropertyFromFileReference';
var LIndex: integer;
begin
  Result := nil;
  try
    for LIndex := 0 to FProperties.Count - 1 do
    begin
      if (TFieldProperty(FProperties.Objects[LIndex]).FilePositionHitTest(
            AFileName, ALineNo, ACharacterPosition)) then
      begin
        Result := TFieldProperty(FProperties.Objects[LIndex]);
        break;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFieldPropertyList.Initialise: boolean;
const OPNAME = 'TFieldPropertyList.Initialise';
begin
  Result := False;
  try

    // Make sure that the list is clear.
    Clear;

    // Create all the field objects.
    LoadFieldPropertyData(AddFieldProperty,  FAppModules.StudyArea.ModelVersion);
    LoadFieldPropertyDataD(AddFieldProperty,  FAppModules.StudyArea.ModelVersion);
    LoadFieldPropertyDataB(AddFieldPropertyB, FAppModules.StudyArea.ModelVersion);

    // Load the update SQL.
    UFieldUpdateSQL_NetworkVisualiser.LoadFieldPropertyUpdateSQLSteps(AddUpdateSQLStep);
    UFieldUpdateSQL_ChangeLists.LoadFieldPropertyUpdateSQLSteps(AddUpdateSQLStep);
    UFieldUpdateSQL_YieldModel_Channels.LoadFieldPropertyUpdateSQLSteps(AddUpdateSQLStep);
    UFieldUpdateSQL_PlanningModel_FAM.LoadFieldPropertyUpdateSQLSteps(AddUpdateSQLStep);
    UFieldUpdateSQL_YieldModel_Configuration.LoadFieldPropertyUpdateSQLSteps(AddUpdateSQLStep);
    UFieldUpdateSQL_YieldModel_Reservoirs.LoadFieldPropertyUpdateSQLSteps(AddUpdateSQLStep);
    UFieldUpdateSQL_YieldModel_Mining.LoadFieldPropertyUpdateSQLSteps(AddUpdateSQLStep);
    UFieldUpdateSQL_YieldModel_GroundWater.LoadFieldPropertyUpdateSQLSteps(AddUpdateSQLStep);

    // Load the field file references.
    //UFieldFileReferenceData.LoadFieldFileReferencesData(AddFieldFileReference);

    // Done.
    Result := True;

  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFieldPropertyList.LoadFieldFileReferencesData(AModelName : string): boolean;
const OPNAME = 'TFieldPropertyList.LoadFieldFileReferencesData';
begin
  Result := False;
  try
    Result := Initialise;
    if (AModelName = CPlanning) then
      UFieldFileReferenceData.LoadPlanningFieldFileReferencesData(AddFieldFileReference);

    if (AModelName = CYield) then
      UFieldFileReferenceData.LoadFieldFileReferencesData(AddFieldFileReference);
    
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFieldPropertyList.AddFieldProperty(AFieldName           : string;
                                              AFieldDescription    : string;
                                              AModeIsEditable      : string;
                                              AModelVersionNumber  : string;
                                              AFieldDataType       : integer;
                                              AFieldMinimumValue   : string;
                                              AFieldMaximumValue   : string;
                                              AFieldGroup          : TFieldGroup;
                                              AFieldArrayLength    : string;
                                              AFieldAcceptedValues : string;
                                              AFieldType           : integer;
                                              AFieldWidth          : integer;
                                              ANumberOfDecimals    : integer;
                                              ASmartFormat         : string;
                                              AFormatStringGrid    : string;
                                              AFormatStringGraph   : string;
                                              AFieldUnits          : string;
                                              AFileName            : string;
                                              AFieldSource         : string;
                                              AFileFieldName       : string);
const OPNAME = 'TFieldPropertyList.AddFieldProperty';
var LFieldProperty: TFieldProperty;
begin
  try
    LFieldProperty := TFieldProperty.Create(FAppModules);
    LFieldProperty.SetMemberVariables(AFieldName,
                                      AFieldDescription,
                                      AModeIsEditable,
                                      AModelVersionNumber,
                                      AFieldDataType,
                                      AFieldMinimumValue,
                                      AFieldMaximumValue,
                                      AFieldGroup,
                                      AFieldArrayLength,
                                      AFieldAcceptedValues,
                                      AFieldType,
                                      AFieldWidth,
                                      ANumberOfDecimals,
                                      ASmartFormat,
                                      AFormatStringGrid,
                                      AFormatStringGraph,
                                      AFieldUnits,
                                      AFileName,
                                      AFieldSource,
                                      AFileFieldName);
    FProperties.AddObject(AFieldName, LFieldProperty)
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFieldPropertyList.AddFieldPropertyB (AFieldName      : string;
                                                ADBTableName    : string;
                                                ADBFieldName    : string;
                                                ADataClassName  : string;
                                                AInChangeList   : boolean);
const OPNAME = 'TFieldPropertyList.AddFieldPropertyB';
var
  LFieldProperty : TFieldProperty;
begin
  try
    LFieldProperty := GetFieldProperty(AFieldName);
    if (LFieldProperty <> nil) then
    begin
      LFieldProperty.SetMemberVariablesB(ADBTableName, ADBFieldName, ADataClassName, AInChangeList);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFieldPropertyList.AddUpdateSQLStep(
  AStepNo: integer; AFieldName, ATableName, AFieldInTable, AUpdateSQL, AGetValueSQL: string);
const OPNAME = 'TFieldPropertyList.AddUpdateSQLStep';
var
  LTableProperty: TTableProperty;
  LFieldProperty: TFieldProperty;
begin
  try
    LTableProperty := FTableProperties[ATableName];
    if Assigned(LTableProperty) then
    begin
      LFieldProperty := FieldProperty[AFieldName];
      if Assigned(LFieldProperty) then
        LFieldProperty.AddUpdateSQLStep(LTableProperty, AStepNo, AFieldInTable, AUpdateSQL, AGetValueSQL);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFieldPropertyList.AddFieldFileReference(
  AFieldName, AFileName, ALineNo: string; AStartCharacter, ALength: integer);
const OPNAME = 'TFieldPropertyList.AddFieldFileReference';
var LFieldProperty: TFieldProperty;
begin
  try
    LFieldProperty := FieldProperty[AFieldName];
    if Assigned(LFieldProperty) then
      LFieldProperty.AddFileReference(AFileName, ALineNo, AStartCharacter, ALength);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFieldPropertyList.LanguageHasChanged: boolean;
const OPNAME = 'TFieldPropertyList.LanguageHasChanged';
var LIndex: integer;
begin
  Result := False;
  try
    for LIndex := 0 to FProperties.Count - 1 do
      TFieldProperty(FProperties.Objects[LIndex]).LanguageHasChanged;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFieldPropertyList.FieldByIndex(AIndex: integer): TFieldProperty;
const OPNAME = 'TFieldPropertyList.FieldByIndex';
begin
  Result := Nil;
  try
    if (AIndex >= 0) and (AIndex < FProperties.Count) then
      Result := TFieldProperty(FProperties.Objects[AIndex]);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFieldPropertyList.FieldCount: integer;
const OPNAME = 'TFieldPropertyList.FieldCount';
begin
  Result := 0;
  try
    Result := FProperties.Count;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFieldPropertyList.FieldAvailableInModel(AFieldProperty :TAbstractFieldProperty): boolean;
const OPNAME = 'TFieldPropertyList.FieldAvailableInModel';
var
  LValues: TStringList;
  LVersion : string;
begin
  Result := False;
  try
    if Assigned(AFieldProperty) then
    begin
      LValues := TStringList.Create;
      try
        LValues.CommaText := AFieldProperty.ModelVersionNumber;
        LVersion := FAppModules.StudyArea.ModelVersion;
        Result := (LValues.IndexOf(LVersion) >= 0);
      finally
        LValues.Free;
      end;
    end;
   except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
