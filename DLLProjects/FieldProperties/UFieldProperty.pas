//
//  UNIT      : Contains TFieldProperty Class
//  AUTHOR    : Grant Nyland (PDNA)
//  DATE      : 2002/03/16
//  COPYRIGHT : Copyright © 2002 DWAF
//
unit UFieldProperty;

interface

uses
  Classes,
  Contnrs,
  UFieldFileReference,
  UAbstractObject;

type
  TFieldProperty = class(TAbstractFieldProperty)
  protected
    FFieldUpdateSQLSteps: TObjectList;
    FFieldFileReferences: TObjectList;
    function GetFieldUpdateSQL(AStepIndex: integer): TAbstractFieldUpdateSQLStep; override;
    function GetArrayLow: integer; override;
    function GetArrayHigh: integer; override;
    function GetArrayLowDimTwo: integer; override;
    function GetArrayHighDimTwo: integer; override;
    function GetFieldMinimumValue: string; override;
    function GetFieldMaximumValue: string; override;
    procedure SemicolonText (AString     : string;
                             AStringList : TStringList);
  public
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    function ArrayLength(Aindex: integer=0): integer;override;
    procedure Reset;
    procedure SetMemberVariables (AFieldName           : string;
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
                                  ANumberOfDecimals    : Integer;
                                  ASmartFormat         : string;
                                  AFormatStringGrid    : string;
                                  AFormatStringGraph   : string;
                                  AFieldUnits          : string;
                                  AFileName            : string;
                                  AFieldSource         : string;
                                  AFileFieldName       : string);
    procedure SetMemberVariablesB (ADBTableName        : string;
                                   ADBFieldName        : string;
                                   ADataClassName      : string;
                                   AInChangeList       : boolean);

    procedure AddUpdateSQLStep(ATableProperty: TAbstractTableProperty;
      AStepNo: integer; AFieldInTable, AUpdateSQL, AGetValueSQL: string);
    procedure AddFileReference(AFileName, ALineNo: string; AStartCharacter, ALength: integer);
    function FieldUpdateSQLCount: integer; override;
    function LanguageHasChanged: boolean; override;
    function FilePositionHitTest(AFileName, ALineNo: string; ACharacterPosition: integer): boolean;
  end;

implementation

uses
  SysUtils,
  UFieldUpdateSQLStep,
  UErrorHandlingOperations;

procedure TFieldProperty.CreateMemberObjects;
const OPNAME = 'TFieldProperty.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
    FFieldUpdateSQLSteps := TObjectList.Create;
    FFieldFileReferences := TObjectList.Create;
    Reset;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFieldProperty.DestroyMemberObjects;
const OPNAME = 'TFieldProperty.DestroyMemberObjects';
begin
  try
    Reset;
    FreeAndNil(FFieldUpdateSQLSteps);
    FreeAndNil(FFieldFileReferences);
    inherited DestroyMemberObjects;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFieldProperty.FieldUpdateSQLCount: integer;
const OPNAME = 'TFieldProperty.FieldUpdateSQLCount';
begin
  Result := 0;
  try
    Result := FFieldUpdateSQLSteps.Count;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFieldProperty.GetFieldUpdateSQL(AStepIndex: integer): TAbstractFieldUpdateSQLStep;
const OPNAME = 'TFieldProperty.GetFieldUpdateSQL';
begin
  Result := nil;
  try
    Result := TAbstractFieldUpdateSQLStep(FFieldUpdateSQLSteps.Items[AStepIndex]);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFieldProperty.Reset;
const OPNAME = 'TFieldProperty.Reset';
begin
  try
    FFieldUpdateSQLSteps.Clear;
    FFieldFileReferences.Clear;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFieldProperty.SetMemberVariables(AFieldName           : string;
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
                                            ANumberOfDecimals    : Integer;
                                            ASmartFormat         : string;
                                            AFormatStringGrid    : string;
                                            AFormatStringGraph   : string;
                                            AFieldUnits          : string;
                                            AFileName            : string;
                                            AFieldSource         : string;
                                            AFileFieldName       : string);
const OPNAME = 'TFieldProperty.SetMemberVariables';
begin
  try
    FFieldName           := AFieldName;
    FFieldType           := AFieldType;
    FFieldDataType       := AFieldDataType;
    FFieldWidth          := AFieldWidth;
    FFormatStringGrid    := AFormatStringGrid;
    FFormatStringGraph   := AFormatStringGraph;
    FModeIsEditable      := AModeIsEditable;
    FFieldUnits          := AFieldUnits;
    FFieldDescription    := AFieldDescription;
    FFieldSource         := AFieldSource;
    FFieldMinimumValue   := AFieldMinimumValue;
    FFieldMaximumValue   := AFieldMaximumValue;
    FFileFieldName       := AFileFieldName;
    FFieldArrayLength    := AFieldArrayLength;
    FFieldAcceptedValues := AFieldAcceptedValues;
    FFieldGroup          := AFieldGroup;
    FModelVersionNumber  := AModelVersionNumber;
    FFileName            := AFileName;
    FNumberOfDecimals    := ANumberOfDecimals;
    FSmartFormat         := ASmartFormat;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFieldProperty.SetMemberVariablesB (ADBTableName        : string;
                                              ADBFieldName        : string;
                                              ADataClassName      : string;
                                              AInChangeList       : boolean);
const OPNAME = 'TFieldProperty.SetMemberVariablesB';
begin
  try
    FDBTableName   := ADBTableName;
    FDBFieldName   := ADBFieldName;
    FDataClassName := ADataClassName;
    FInChangeList  := AInChangeList;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFieldProperty.AddUpdateSQLStep(ATableProperty: TAbstractTableProperty;
  AStepNo: integer; AFieldInTable, AUpdateSQL, AGetValueSQL: string);
const OPNAME = 'TFieldProperty.AddUpdateSQLStep';
var LSQLStep: TFieldUpdateSQLStep;
begin
  try
    LSQLStep := TFieldUpdateSQLStep.Create;
    LSQLStep.PopulateMemberVariables(Self, ATableProperty, AStepNo, AFieldInTable, AUpdateSQL, AGetValueSQL);
    FFieldUpdateSQLSteps.Add(LSQLStep);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFieldProperty.AddFileReference(
  AFileName, ALineNo: string; AStartCharacter, ALength: integer);
const OPNAME = 'TFieldProperty.AddFileReference';
var LFileReference: TFieldFileReference;
begin
  try
    LFileReference := TFieldFileReference.Create;
    LFileReference.PopulateMemberVariables(Self, AFileName, ALineNo, AStartCharacter, ALength);
    FFieldFileReferences.Add(LFileReference);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFieldProperty.LanguageHasChanged: boolean;
const OPNAME = 'TFieldProperty.LanguageHasChanged';
var LLanguageKey: string;
begin
  Result := False;
  try
    if (Trim(FFieldName) <> '') then
    begin
      LLanguageKey := 'TField.' + FFieldName;
      FFieldLangDescr := FAppModules.Language.GetString(LLanguageKey);
    end;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFieldProperty.FilePositionHitTest(
  AFileName,ALineNo: string; ACharacterPosition: integer): boolean;
const OPNAME = 'TFieldProperty.FilePositionHitTest';
var LIndex: integer;
begin
  Result := False;
  try
    for LIndex := 0 to FFieldFileReferences.Count - 1 do
    begin
      if (TFieldFileReference(FFieldFileReferences[LIndex]).
            HitTest(AFileName, ALineNo, ACharacterPosition)) then
      begin
        Result := True;
        break;
      end;
    end;
  except on E: Exception do HandleErrorFunction(E, OPNAME, Result) end;
end;

procedure TFieldProperty.SemicolonText (AString     : string;
                                        AStringList : TStringList);
const OPNAME = 'TFieldProperty.SemicolonText';
var
  lPos     : integer;
  lTempStr : string;
begin
  try
    AStringList.Clear;
    while (AString <> '') do
    begin
      lPos := Pos(';', AString);
      if (lPos = 0) then
      begin
        lTempStr := AString;
        AString  := '';
      end
      else
      begin
        lTempStr := Copy(AString, 1, lPos-1);
        AString  := Copy(AString, lPos+1, Length(AString)-lPos);
      end;
      AStringList.Add(lTempStr);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFieldProperty.GetArrayLow: integer;
const OPNAME = 'TFieldProperty.GetArrayLow';
var
  LValues          : TStringList;
  LModelVersions   : TStringList;
  LArrayDimensions : TStringList;
  LStrModelVersion : string;
  LModelVersionPos : integer;
begin
  Result := 0;
  try
    if (Trim(FFieldArrayLength) <> '') then
    begin
      LValues          := TStringList.Create;
      LModelVersions   := TStringList.Create;
      LArrayDimensions := TStringList.Create;
      try
        SemicolonText(FFieldArrayLength, LArrayDimensions);
        LModelVersions.CommaText := FModelVersionNumber;
        LStrModelVersion := FAppModules.StudyArea.ModelVersion;
        LModelVersionPos := LModelVersions.IndexOf(LStrModelVersion);

        if (LModelVersionPos < 0) then
          raise Exception.CreateFmt('Field (%s) is not defined in the current model or model version.',[FFieldName]);

        if (LArrayDimensions.Count = 0) then
          raise Exception.CreateFmt('Field (%s) is not defined as an array field.',[FFieldName]);

        if (LModelVersionPos >= LArrayDimensions.Count) then
          LValues.CommaText := LArrayDimensions[LArrayDimensions.Count-1]
        else
          LValues.CommaText := LArrayDimensions[LModelVersionPos];

        if (LValues.Count = 0) then
          raise Exception.CreateFmt('Array field (%s) does not have a defined array lowest index.',[FFieldName]);

        Result := StrToInt(LValues[0]);

      finally
        LValues.Free;
        LModelVersions.Free;
        LArrayDimensions.Free
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFieldProperty.GetArrayHigh: integer;
const OPNAME = 'TFieldProperty.GetArrayHigh';
var
  LValues          : TStringList;
  LModelVersions   : TStringList;
  LArrayDimensions : TStringList;
  LStrModelVersion : string;
  LModelVersionPos : integer;
begin
  Result := 0;
  try
    if (Trim(FFieldArrayLength) <> '') then
    begin
      LValues          := TStringList.Create;
      LModelVersions   := TStringList.Create;
      LArrayDimensions := TStringList.Create;
      try
        SemicolonText(FFieldArrayLength, LArrayDimensions);
        LModelVersions.CommaText := FModelVersionNumber;
        LStrModelVersion := FAppModules.StudyArea.ModelVersion;
        LModelVersionPos := LModelVersions.IndexOf(LStrModelVersion);

        if (LModelVersionPos < 0) then
          raise Exception.CreateFmt('Field (%s) is not defined in the current model or model version.',[FFieldName]);

        if (LArrayDimensions.Count = 0) then
          raise Exception.CreateFmt('Field (%s) is not defined as an array field.',[FFieldName]);

        if (LModelVersionPos >= LArrayDimensions.Count) then
          LValues.CommaText := LArrayDimensions[LArrayDimensions.Count-1]
        else
          LValues.CommaText := LArrayDimensions[LModelVersionPos];

        if (LValues.Count <= 1) then
          raise Exception.CreateFmt('Array field (%s) does not have a defined array highest index.',[FFieldName]);

        Result := StrToInt(LValues[1]);

      finally
        LValues.Free;
        LModelVersions.Free;
        LArrayDimensions.Free
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFieldProperty.GetArrayLowDimTwo: integer;
const OPNAME = 'TFieldProperty.GetArrayLowDimTwo';
var
  LValues          : TStringList;
  LModelVersions   : TStringList;
  LArrayDimensions : TStringList;
  LStrModelVersion : string;
  LModelVersionPos : integer;
begin
  Result := 0;
  try
    if (Trim(FFieldArrayLength) <> '') then
    begin
      LValues          := TStringList.Create;
      LModelVersions   := TStringList.Create;
      LArrayDimensions := TStringList.Create;
      try
        SemicolonText(FFieldArrayLength, LArrayDimensions);
        LModelVersions.CommaText := FModelVersionNumber;
        LStrModelVersion         := FAppModules.StudyArea.ModelVersion;
        LModelVersionPos         := LModelVersions.IndexOf(LStrModelVersion);

        if (LModelVersionPos < 0) then
          raise Exception.CreateFmt('Field (%s) is not defined in the current model or model version.',[FFieldName]);

        if (LArrayDimensions.Count = 0) then
          raise Exception.CreateFmt('Field (%s) is not defined as an array field.',[FFieldName]);

        if (LModelVersionPos >= LArrayDimensions.Count) then
          LValues.CommaText := LArrayDimensions[LArrayDimensions.Count-1]
        else
          LValues.CommaText := LArrayDimensions[LModelVersionPos];

        if (LValues.Count <= 2) then
          raise Exception.CreateFmt('Array field (%s) does not have a defined array lowest second(2nd) index.',[FFieldName]);

        Result := StrToInt(LValues[2]);
      finally
        LValues.Free;
        LModelVersions.Free;
        LArrayDimensions.Free;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFieldProperty.GetArrayHighDimTwo: integer;
const OPNAME = 'TFieldProperty.GetArrayHighDimTwo';
var
  LValues          : TStringList;
  LModelVersions   : TStringList;
  LArrayDimensions : TStringList;
  LStrModelVersion : string;
  LModelVersionPos : integer;
begin
  Result := 0;
  try
    if (Trim(FFieldArrayLength) <> '') then
    begin
      LValues          := TStringList.Create;
      LModelVersions   := TStringList.Create;
      LArrayDimensions := TStringList.Create;
      try
        SemicolonText(FFieldArrayLength, LArrayDimensions);
        LModelVersions.CommaText := FModelVersionNumber;
        LStrModelVersion         := FAppModules.StudyArea.ModelVersion;
        LModelVersionPos         := LModelVersions.IndexOf(LStrModelVersion);

        if (LModelVersionPos < 0) then
          raise Exception.CreateFmt('Field (%s) is not defined in the current model or model version.',[FFieldName]);

        if (LArrayDimensions.Count = 0) then
          raise Exception.CreateFmt('Field (%s) is not defined as an array field.',[FFieldName]);

        if (LModelVersionPos >= LArrayDimensions.Count) then
          LValues.CommaText := LArrayDimensions[LArrayDimensions.Count-1]
        else
          LValues.CommaText := LArrayDimensions[LModelVersionPos];

        if (LValues.Count <= 3) then
          raise Exception.CreateFmt('Array field (%s) does not have a defined array highest second(2nd) index.',[FFieldName]);

        Result := StrToInt(LValues[3]);
      finally
        LValues.Free;
        LModelVersions.Free;
        LArrayDimensions.Free
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFieldProperty.ArrayLength(Aindex: integer): integer;
const OPNAME = 'TFieldProperty.ArrayLength';
begin
  Result := 0;
  try
    if (Trim(FFieldArrayLength) = '') then
      raise Exception.CreateFmt('Array field (%s) does not have a defined array length.',[FFieldName]);

    case Aindex of
      0: Result := GetArrayLow + GetArrayHigh;
      1: Result := GetArrayLowDimTwo + GetArrayHighDimTwo;
      else
       raise Exception.CreateFmt('Array field (%s) does not have a defined array length for index (%d).',[FFieldName,Aindex]);
    end;
   except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFieldProperty.GetFieldMaximumValue: string;
const OPNAME = 'TFieldProperty.GetFieldMaximumValue';
var
  LModelVersions,
  LMaxValues: TStringList;
  LStrModelVersion : String;
  LModelVersionPos: integer;
begin
  Result := '';
  try
    LModelVersions := TStringList.Create;
    LMaxValues     := TStringList.Create;
    try
      LModelVersions.CommaText := FModelVersionNumber;
      LMaxValues.CommaText     := FFieldMaximumValue;
      LStrModelVersion         := FAppModules.StudyArea.ModelVersion;
      LModelVersionPos         := LModelVersions.IndexOf(LStrModelVersion);

      if (LModelVersionPos < 0) then
        raise Exception.CreateFmt('Field (%s) is not defined in the current model or model version.',[FFieldName]);

      if (LMaxValues.Count = 0) then
        raise Exception.CreateFmt('Field (%s) does not have any maximum values defined.',[FFieldName]);

      if(LModelVersionPos >= LMaxValues.Count) then
        Result := LMaxValues[LMaxValues.Count-1]
      else
        Result := LMaxValues[LModelVersionPos];

    finally
      LModelVersions.Free;
      LMaxValues.Free;
    end;
   except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFieldProperty.GetFieldMinimumValue: string;
const OPNAME = 'TFieldProperty.GetFieldMinimumValue';
var
  LModelVersions,
  LMinValues: TStringList;
  LStrModelVersion : String;
  LModelVersionPos: integer;
begin
  Result := '';
  try
    LModelVersions := TStringList.Create;
    LMinValues     := TStringList.Create;
    try
      LModelVersions.CommaText := FModelVersionNumber;
      LMinValues.CommaText     := FFieldMinimumValue;
      LStrModelVersion         := FAppModules.StudyArea.ModelVersion;
      LModelVersionPos         := LModelVersions.IndexOf(LStrModelVersion);

      if (LModelVersionPos < 0) then
        raise Exception.CreateFmt('Field (%s) is not defined in the current model or model version.',[FFieldName]);

      if (LMinValues.Count = 0) then
        raise Exception.CreateFmt('Field (%s) does not have any minimum values defined.',[FFieldName]);


      if(LModelVersionPos >= LMinValues.Count) then
        Result := LMinValues[LMinValues.Count-1]
      else
        Result := LMinValues[LModelVersionPos];
    finally
      LModelVersions.Free;
      LMinValues.Free;
    end;
   except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
