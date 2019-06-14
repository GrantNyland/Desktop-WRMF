//
//
//  UNIT      : Contains TYieldModelDataValidator Class
//  AUTHOR    : Dziedzi Ramulondi(Aravia)
//  DATE      : 28/07/2003
//  COPYRIGHT : Copyright © 2003 DWAF
//
//
unit UYieldModelDataValidator;

interface

uses
  Classes,
  UAbstractObject;

type

  TYieldModelDataValidator = class(TAbstractModelDataValidator)
  protected
    // Helper validators
    function ValidateReservoirExists(AFieldName: string; AFieldValue: integer; var AMessage: string): boolean; virtual;

    // Generic validators
    function ValidateStringField(AFieldName: string; AFieldValue: string; var AMessage: string; AContextData: TObject): boolean; virtual;
    function ValidateCharField(AFieldName: string; AFieldValue: char; var AMessage: string; AContextData: TObject): boolean; virtual;
    function ValidateIntegerField(AFieldName: string; AFieldValue: integer; var AMessage: string; AContextData: TObject): boolean; virtual;
    function ValidateFloatField(AFieldName: string; AFieldValue: double; var AMessage: string; AContextData: TObject): boolean; virtual;
    function ValidateBooleanField(AFieldName: string; AFieldValue: boolean; var AMessage: string; AContextData: TObject): boolean; virtual;
    function ValidateDateTimeField(AFieldName: string; AFieldValue: TDateTime; var AMessage: string; AContextData: TObject): boolean; virtual;

    // Generic field validators
    function ValidateScaledField(AFieldName: string; AFieldValue: Double; var AMessage: string; AContextData: TObject): boolean; virtual;
    function ValidateIfReservoirLevelInRange(AFieldValue: Double; var AMessage: string; AContextData: TObject): boolean; virtual;

    // Support validation functions (to validate dependencies)
    function DoPenaltyStructuresSatisfyRuleCurve(AFieldValue: integer; var AMessage: string; AContextData: TObject): boolean; virtual;

    // Specific field validators
    function ValidateReservoirName(AFieldName: string; AFieldValue: string; var AMessage: string; AContextData: TObject): boolean; virtual;
    function ValidatePenaltyStructure(AFieldName: string; AFieldValue: integer; var AMessage: string; AContextData: TObject): boolean; virtual;
    function ValidateReservoirIncludeSummary(AFieldName: string; AFieldValue: string; var AMessage: string; AContextData: TObject): boolean; virtual;
    function ValidateReservoirStatusIndicator(AFieldName: string; AFieldValue: integer; var AMessage: string; AContextData: TObject): boolean; virtual;

    function ValidateRuleCurveDefinition(AFieldName: string; AFieldValue: integer; var AMessage: string; AContextData: TObject): boolean; virtual;

    function ValidateReservoirResInitialLevelsLev(AFieldName: string; AFieldValue: Double; var AMessage: string; AContextData: TObject): boolean; virtual;
    function ValidateReservoirAreaFull(AFieldName: string; AFieldValue: Double; var AMessage: string; AContextData: TObject): boolean; virtual;

    function ValidateReservoirFullStorageLevel(AFieldName: string; AFieldValue: Double; var AMessage: string; AContextData: TObject): boolean; virtual;
    function ValidateReservoirDeadStorageLevel(AFieldName: string; AFieldValue: Double; var AMessage: string; AContextData: TObject): boolean; virtual;
    function ValidateReservoirBottomOfReservoirLevel(AFieldName: string; AFieldValue: Double; var AMessage: string; AContextData: TObject): boolean; virtual;

  public
    function ValidateModelFieldData(AFiledName: string; AFieldValue: string; var AMessage: string; AContextData: TObject): boolean; virtual;


    function ValidateFieldInContext(AFieldName, AFieldValue: string; AObjectsArray: array of TObject;
             var AErrorMessage: string; AFieldIndex:integer = -1): boolean;  override;
    function ValidateBusinessRule(ABusinessRule: integer; AObjectsArray: array of TObject;
             AErrorMessages: TStrings): boolean; override;
  end;

implementation

uses
  SysUtils,

  Math,
  UDBConstants,
  UReservoirPenaltyStructureData,
  UYieldModelDataObject,
  UErrorHandlingOperations, UReservoirZoneElevationData;

{ TYieldModelDataValidator }

function TYieldModelDataValidator.ValidateModelFieldData(
         AFiledName,AFieldValue: string; var AMessage: string; AContextData: TObject): boolean;
const OPNAME = 'TYieldModelDataValidator.ValidateModelFieldData';
var
  LFieldProperty: TAbstractFieldProperty;
  LString   : string;
  LFloat    : double;
  LInteger  : integer;
  LDateTime : TDateTime;
  LChar     : char;
  LBoolean  : boolean;

begin
  Result := False;
  try
    AMessage  := '';
    LString   := '';
    LFloat    := -99999.99;
    LInteger  := -99999;
    LDateTime := 0.0;
    LChar     := ' ';
    LBoolean  := False;
    LFieldProperty := FAppModules.FieldProperties.FieldProperty(AFiledName);
    if not Assigned(LFieldProperty) then
    begin
      AMessage := FAppModules.Language.GetString('TFieldError.FieldUnkown');
      AMessage := Format(AMessage,[AFiledName,AFieldValue]);
      Exit;
    end
    else
    begin
      case LFieldProperty.FieldDataType of
        FieldStringType  : LString   := AFieldValue;
        FieldFloatType   : LFloat    := StrToFloat(AFieldValue);
        FieldIntegerType : LInteger  := StrToInt(AFieldValue);
        FieldDTimeType   : LDateTime := StrToDateTime(AFieldValue);
        FieldCharType    : LChar     := AFieldValue[1];
        FieldBoolType    : LBoolean  := UpperCase(AFieldValue) = 'Y';
      end;

      case LFieldProperty.FieldDataType of
        FieldStringType  : Result := ValidateStringField(AFiledName,LString,AMessage,AContextData);
        FieldFloatType   : Result := ValidateFloatField(AFiledName,LFloat,AMessage,AContextData);
        FieldIntegerType : Result := ValidateIntegerField(AFiledName,LInteger,AMessage,AContextData);
        FieldDTimeType   : Result := ValidateDateTimeField(AFiledName,LDateTime,AMessage,AContextData);
        FieldCharType    : Result := ValidateCharField(AFiledName,LChar,AMessage,AContextData);
        FieldBoolType    : Result := ValidateBooleanField(AFiledName,LBoolean,AMessage,AContextData);
      end;
    end;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldModelDataValidator.ValidateFloatField(
         AFieldName: string; AFieldValue: double; var AMessage: string; AContextData: TObject): boolean;
const OPNAME = 'TYieldModelDataValidator.ValidateFloatField';
begin
  Result := False;
  try
    Result := True;

    if CompareText(AFieldName, 'FilePropInc') = 0 then
      Result := ValidateScaledField(AFieldName, AFieldValue, AMessage, AContextData)
    else if CompareText(AFieldName, 'FilePropAff') = 0 then
      Result := ValidateScaledField(AFieldName, AFieldValue, AMessage, AContextData)
    else if CompareText(AFieldName, 'FilePropIrr') = 0 then
      Result := ValidateScaledField(AFieldName, AFieldValue, AMessage, AContextData)
    else if CompareText(AFieldName, 'FullSupplyLevel') = 0 then
      Result := ValidateReservoirFullStorageLevel(AFieldName, AFieldValue, AMessage, AContextData)
    else if CompareText(AFieldName, 'DeadStorageLevel') = 0 then
      Result := ValidateReservoirDeadStorageLevel(AFieldName, AFieldValue, AMessage, AContextData)
    else if CompareText(AFieldName, 'BottomOfReservoir') = 0 then
      Result := ValidateReservoirBottomOfReservoirLevel(AFieldName, AFieldValue, AMessage, AContextData)
    else if CompareText(AFieldName, 'ResInitialLevelsLev') = 0 then
      Result := ValidateReservoirResInitialLevelsLev(AFieldName, AFieldValue, AMessage, AContextData)
    else if CompareText(AFieldName, 'AreaFull') = 0 then
      Result := ValidateReservoirAreaFull(AFieldName, AFieldValue, AMessage, AContextData);

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldModelDataValidator.ValidateIntegerField(
         AFieldName: string;AFieldValue: integer; var AMessage: string; AContextData: TObject): boolean;
const OPNAME = 'TYieldModelDataValidator.ValidateIntegerField';
begin
  Result := False;
  try

    if(AFieldName = 'PenaltyStruct') then
      Result := ValidatePenaltyStructure(AFieldName, AFieldValue, AMessage, AContextData)
    else if (AFieldName = 'StatusIndicator') then
      Result := ValidateReservoirStatusIndicator(AFieldName,AFieldValue,AMessage,AContextData)
    else if (AFieldName = 'RuleCurve') then
      Result := ValidateRuleCurveDefinition(AFieldName,AFieldValue,AMessage,AContextData);

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldModelDataValidator.ValidateStringField(
         AFieldName,AFieldValue: string; var AMessage: string; AContextData: TObject): boolean;
const OPNAME = 'TYieldModelDataValidator.ValidateStringField';
begin
  Result := False;
  try
    if(AFieldName = 'ReservoirName') then
      Result := ValidateReservoirName(AFieldName,AFieldValue,AMessage,AContextData)
    else
    if(AFieldName = 'IncludeSummary') then
      Result := ValidateReservoirIncludeSummary(AFieldName,AFieldValue,AMessage,AContextData)
    else
      Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldModelDataValidator.ValidateCharField(AFieldName: string;AFieldValue: char; var AMessage: string; AContextData: TObject): boolean;
const OPNAME = 'TYieldModelDataValidator.ValidateCharField';
begin
  Result := False;
  try
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldModelDataValidator.ValidateBooleanField(
         AFieldName: string; AFieldValue: boolean; var AMessage: string; AContextData: TObject): boolean;
const OPNAME = 'TYieldModelDataValidator.ValidateBooleanField';
begin
  Result := False;
  try
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


function TYieldModelDataValidator.ValidateDateTimeField(
         AFieldName: string;AFieldValue: TDateTime; var AMessage: string;AContextData: TObject): boolean;
const OPNAME = 'TYieldModelDataValidator.ValidateDateTimeField';
begin
  Result := False;
  try
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldModelDataValidator.ValidateReservoirName(
         AFieldName: string; AFieldValue: string; var AMessage: string; AContextData: TObject): boolean;
const OPNAME = 'TYieldModelDataValidator.ValidateReservoirName';
var
  LReservoir:TReservoirData;
begin
  Result := False;
  try
    AMessage := '';
    if(Trim(AFieldValue) = '') then
    begin
      AMessage := FAppModules.Language.GetString('TFieldError.StringFieldEmpty');
      AMessage := Format(AMessage,[AFieldName]);
      Exit;
    end
    else
    begin
      LReservoir := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.CastReservoirList.CastReservoirByName[AFieldValue];
      if Assigned(LReservoir) then
      begin
        AMessage := FAppModules.Language.GetString('TFieldError.Duplicate');
        AMessage := Format(AMessage,[AFieldName,AFieldValue]);
        Exit;
      end;
    end;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldModelDataValidator.ValidatePenaltyStructure(
         AFieldName:string; AFieldValue: integer; var AMessage: string; AContextData: TObject): boolean;
const OPNAME = 'TYieldModelDataValidator.ValidatePenaltyStructure';
var
  LPenaltyStructureCount: integer;
begin
  Result := False;
  try
    AMessage := '';
    LPenaltyStructureCount := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.CastPenaltyStructureList.PenaltyStructureCount;
    if(AFieldValue < 1) or (AFieldValue > LPenaltyStructureCount) then
    begin
      AMessage := FAppModules.Language.GetString('TFieldError.IntegerOutsideRange');
      AMessage := Format(AMessage,[AFieldName,1,LPenaltyStructureCount]);
      Exit;
    end;

    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldModelDataValidator.ValidateReservoirExists(AFieldName: string; AFieldValue: integer; var AMessage: string): boolean;
const OPNAME = 'TYieldModelDataValidator.ValidateReservoirExists';
begin
  Result := False;
  try
    AMessage := '';
    if(AFieldValue < 0) or (AFieldValue > 1) then
    begin
      AMessage := FAppModules.Language.GetString('TFieldError.IntegerOutsideRange');
      AMessage := Format(AMessage,[AFieldName,0,1]);
      Exit;
    end;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldModelDataValidator.ValidateReservoirIncludeSummary(
         AFieldName, AFieldValue: string; var AMessage: string; AContextData: TObject): boolean;
const OPNAME = 'TYieldModelDataValidator.ValidateReservoirIncludeSummary';
begin
  Result := False;
  try
    AMessage := '';
    if(AFieldValue <> 'Y') and (AFieldValue <> 'N') then
    begin
      AMessage := FAppModules.Language.GetString('TFieldError.BooleanOutsideRange');
      AMessage := Format(AMessage,[AFieldName]);
      Exit;
    end;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldModelDataValidator.ValidateReservoirStatusIndicator(
         AFieldName: string; AFieldValue: integer; var AMessage: string; AContextData: TObject): boolean;
const OPNAME = 'TYieldModelDataValidator.ValidateReservoirStatusIndicator';
begin
  Result := False;
  try
    AMessage := '';
    if(AFieldValue < 0) or (AFieldValue > 1) then
    begin
      AMessage := FAppModules.Language.GetString('TFieldError.IntegerOutsideRange');
       AMessage := Format(AMessage,[AFieldName,0,1]);
      Exit;
    end;

    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldModelDataValidator.ValidateScaledField(AFieldName : string;
  AFieldValue: double; var AMessage: string;
  AContextData: TObject): boolean;
const OPNAME = 'TYieldModelDataValidator.ValidateScaledField';
begin
  Result := False;
  try

    AMessage := '';

    if(AFieldValue < 0) or (AFieldValue > 1) then
    begin
      AMessage := FAppModules.Language.GetString('TFieldError.IntegerOutsideRange');
      AMessage := Format(AMessage,[AFieldName,0,1]);
      Exit;
    end;

    Result := True;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldModelDataValidator.ValidateReservoirFullStorageLevel(
  AFieldName: string; AFieldValue: Double; var AMessage: string;
  AContextData: TObject): boolean;
const OPNAME = 'TYieldModelDataValidator.ValidateReservoirFullStorageLevel';
var
  LReservoirObject : TReservoirData;
  LDRWDWN1 : Double;
  LIndex : integer;
begin
  Result := false;
  try
    LReservoirObject := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.CastReservoirList.CastReservoirByIdentifier[Integer(AContextData)];
    if Assigned(LReservoirObject) then
    begin
      Result := ValidateIfReservoirLevelInRange(AFieldValue, AMessage, AContextData);
      if Result then
      begin

        LDRWDWN1 := 0;
        for LIndex := 1 to 12 do
          LDRWDWN1 := LDRWDWN1 + LReservoirObject.ReservoirZoneElevationsData.DrawDownLevelByIndex[0].MonthlyElevation[LIndex] / 12;

        Result := Result and (AFieldValue >= LDRWDWN1);

        if not Result then
        begin
          AMessage := FAppModules.Language.GetString('TFieldError.FloatOutsideRange');
          AMessage := Format(AMessage,[AFieldName , LDRWDWN1, LReservoirObject.ReservoirElevationsData.ReservoirElevations[1]]);
        end;

      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldModelDataValidator.ValidateIfReservoirLevelInRange(
  AFieldValue: Double; var AMessage: string; AContextData: TObject): boolean;
const OPNAME = 'TYieldModelDataValidator.ValidateIfReservoirLevelInRange';
var
  LReservoirObject : TReservoirData;
  LMaxValue        : Double;
  LMinValue        : Double;
  LIndex           : integer;
begin
  Result := False;
  try
    LReservoirObject := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.CastReservoirList.CastReservoirByIdentifier[Integer(AContextData)];

    if Assigned(LReservoirObject) then
    begin
      LMinValue := LReservoirObject.ReservoirElevationsData.ReservoirElevations[1];
      LMaxValue := LReservoirObject.ReservoirElevationsData.ReservoirElevations[1];

      for LIndex := 1 to LReservoirObject.ReservoirConfigurationData.PointsCount do
      begin
        LMaxValue := Max(LMaxValue, LReservoirObject.ReservoirElevationsData.ReservoirElevations[LIndex]);
        LMinValue := Min(LMinValue, LReservoirObject.ReservoirElevationsData.ReservoirElevations[LIndex]);
      end;

      Result := (AFieldValue >= LMinValue) and (AFieldValue <= LMaxValue);

      if not Result then
      begin
        AMessage := FAppModules.Language.GetString('TFieldError.FloatOutsideRange');
        AMessage := Format(AMessage,['Reservoir Level' , LMinValue,LMaxValue]);
      end;

    end;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldModelDataValidator.ValidateReservoirDeadStorageLevel(
  AFieldName: string; AFieldValue: Double; var AMessage: string;
  AContextData: TObject): boolean;
const OPNAME = 'TYieldModelDataValidator.ValidateReservoirDeadStorageLevel';
var
  LReservoirObject : TReservoirData;
  LDRWDWNx : Double;
  LIndex : integer;
begin
  Result := false;
  try
    LReservoirObject := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.CastReservoirList.CastReservoirByIdentifier[Integer(AContextData)];
    if Assigned(LReservoirObject) then
      if ValidateIfReservoirLevelInRange(AFieldValue, AMessage, AContextData) then
      begin
        LDRWDWNx := 0;

        for LIndex := 1 to 12 do
          LDRWDWNx := LDRWDWNx + LReservoirObject.ReservoirZoneElevationsData.DrawDownLevelByIndex[
            LReservoirObject.ReservoirZoneElevationsData.ReservoirDrawDownLevelsCount - 1].MonthlyElevation[LIndex] / 12;

        Result := (AFieldValue <= LDRWDWNx) and (AFieldValue >= LReservoirObject.ReservoirZoneElevationsData.BottomOfReservoir.Elevation);

        if not Result then
        begin
          AMessage := FAppModules.Language.GetString('TFieldError.FloatOutsideRange');
          AMessage := Format(AMessage,[AFieldName , LReservoirObject.ReservoirZoneElevationsData.BottomOfReservoir.Elevation, LDRWDWNx]);
        end;

      end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldModelDataValidator.ValidateReservoirBottomOfReservoirLevel(
  AFieldName: string; AFieldValue: Double; var AMessage: string;
  AContextData: TObject): boolean;
const OPNAME = 'TYieldModelDataValidator.ValidateReservoirBottomOfReservoirLevel';
var
  LReservoirObject : TReservoirData;
begin
  Result := false;
  try
    LReservoirObject := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.CastReservoirList.
      CastReservoirByIdentifier[Integer(AContextData)];
    if Assigned(LReservoirObject) then
      if ValidateIfReservoirLevelInRange(AFieldValue, AMessage, AContextData) then
        Result := AFieldValue <= LReservoirObject.ReservoirZoneElevationsData.DeadStorageLevel.Elevation;

    if not Result then
    begin
      AMessage := FAppModules.Language.GetString('TFieldError.FloatOutsideRange');
      AMessage := Format(AMessage,[AFieldName, LReservoirObject.ReservoirElevationsData.ReservoirElevations[
        LReservoirObject.ReservoirConfigurationData.PointsCount],
        LReservoirObject.ReservoirZoneElevationsData.DeadStorageLevel.Elevation]);
    end;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldModelDataValidator.ValidateRuleCurveDefinition(
  AFieldName: string; AFieldValue: integer; var AMessage: string;
  AContextData: TObject): boolean;
const
  OPNAME = 'TYieldModelDataValidator.ValidateRuleCurveDefinition';
var
  LPenaltyStructureList : TReservoirPenaltyStructureList;
begin
  Result := False;
  try
    LPenaltyStructureList := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.CastPenaltyStructureList;
    if Assigned(LPenaltyStructureList) then
    begin
      Result := (AFieldValue >= 1) and (AFieldValue <= LPenaltyStructureList.PenaltyZoneCount);
      if Result then
        Result := DoPenaltyStructuresSatisfyRuleCurve(AFieldValue, AMessage, AContextData)
      else
      begin
        AMessage := FAppModules.Language.GetString('TFieldError.IntegerOutsideRange');
        AMessage := Format(AMessage,[AFieldName, 1, LPenaltyStructureList.PenaltyZoneCount]);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldModelDataValidator.ValidateReservoirAreaFull(
  AFieldName: string; AFieldValue: Double; var AMessage: string;
  AContextData: TObject): boolean;
const
  OPNAME = 'TYieldModelDataValidator.ValidateReservoirAreaFull';
var
  LReservoirObject : TReservoirData;
  LMinValue : Double;
  LMaxValue : Double;
  LIndex : integer;
begin
  Result := false;
  try
    LReservoirObject := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.CastReservoirList.
      CastReservoirByIdentifier[Integer(AContextData)];
    if Assigned(LReservoirObject) then
    begin
      LMinValue := LReservoirObject.ReservoirAreasData.ReservoirAreas[1];
      LMaxValue := LReservoirObject.ReservoirAreasData.ReservoirAreas[1];

      for LIndex := 1 to LReservoirObject.ReservoirConfigurationData.PointsCount do
      begin
        LMaxValue := Max(LMaxValue, LReservoirObject.ReservoirAreasData.ReservoirAreas[LIndex]);
        LMinValue := Min(LMinValue, LReservoirObject.ReservoirAreasData.ReservoirAreas[LIndex]);
      end;

      Result := (AFieldValue >= LMinValue) and (AFieldValue <= LMaxValue);

      if not Result then
      begin
        AMessage := FAppModules.Language.GetString('TFieldError.FloatOutsideRange');
        AMessage := Format(AMessage,[AFieldName, LMinValue, LMaxValue]);
      end;

    end;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldModelDataValidator.ValidateReservoirResInitialLevelsLev(
  AFieldName: string; AFieldValue: Double; var AMessage: string;
  AContextData: TObject): boolean;
const OPNAME = 'TYieldModelDataValidator.ValidateReservoirResInitialLevelsLev';
var
  LReservoirObject : TReservoirData;
  LMinValue : Double;
  LMaxValue : Double;
  LIndex : integer;
begin
  Result := false;
  try
    LReservoirObject := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.CastReservoirList.
      CastReservoirByIdentifier[Integer(AContextData)];
    if Assigned(LReservoirObject) then
    begin
      LMinValue := LReservoirObject.ReservoirElevationsData.ReservoirElevations[1];
      LMaxValue := LReservoirObject.ReservoirElevationsData.ReservoirElevations[1];

      for LIndex := 1 to LReservoirObject.ReservoirConfigurationData.PointsCount do
      begin
        LMaxValue := Max(LMaxValue, LReservoirObject.ReservoirElevationsData.ReservoirElevations[LIndex]);
        LMinValue := Min(LMinValue, LReservoirObject.ReservoirElevationsData.ReservoirElevations[LIndex]);
      end;

      Result := (AFieldValue >= LMinValue) and (AFieldValue <= LMaxValue);

      if not Result then
      begin
        AMessage := FAppModules.Language.GetString('TFieldError.FloatOutsideRange');
        AMessage := Format(AMessage,[AFieldName, LMinValue, LMaxValue]);
      end;

    end;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldModelDataValidator.DoPenaltyStructuresSatisfyRuleCurve(
  AFieldValue: integer; var AMessage: string;
  AContextData: TObject): boolean;
const OPNAME = 'TYieldModelDataValidator.DoPenaltyStructuresSatisfyRuleCurve';
var
   LPenaltyObject : TReservoirPenaltyStructureList;
   LIndex : integer;
   LDelta : integer;
begin
  Result := True;
  try
    LPenaltyObject := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.CastPenaltyStructureList;

    if Assigned(LPenaltyObject) then
    begin

       for LIndex := 1 to LPenaltyObject.PenaltyStructureCount do
       begin

         for LDelta := 1 to LPenaltyObject.PenaltyZoneCount - 2 do
         begin

           if LDelta < AFieldValue then
             if (LPenaltyObject.ReservoirPenaltyStructureByIdentifier[LIndex].ReservoirPenaltyValueByIndex[LDelta - 1].PenaltyValue) <
               (LPenaltyObject.ReservoirPenaltyStructureByIdentifier[LIndex].ReservoirPenaltyValueByIndex[LDelta].PenaltyValue) then
             begin
               Result := False;
               Break;
             end;

           if LDelta > AFieldValue then
             if (LPenaltyObject.ReservoirPenaltyStructureByIdentifier[LIndex].ReservoirPenaltyValueByIndex[LDelta - 1].PenaltyValue) >
               (LPenaltyObject.ReservoirPenaltyStructureByIdentifier[LIndex].ReservoirPenaltyValueByIndex[LDelta].PenaltyValue) then
             begin
               Result := False;
               Break;
             end;

         end;

         if not Result then
         begin
           AMessage := FAppModules.Language.GetString('TFieldError.PenaltyRuleCurveError');
           AMessage := Format(AMessage,[AFieldValue]);
           Break;
         end;

       end;

    end;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldModelDataValidator.ValidateBusinessRule(ABusinessRule: integer;
         AObjectsArray: array of TObject; AErrorMessages: TStrings): boolean;
const OPNAME = 'TYieldModelDataValidator.ValidateBusinessRule';
begin
  Result := False;
  try

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldModelDataValidator.ValidateFieldInContext(AFieldName, AFieldValue: string;
         AObjectsArray: array of TObject; var AErrorMessage: string;
         AFieldIndex: integer): boolean;
const OPNAME = 'TYieldModelDataValidator.ValidateFieldInContext';
begin
  Result := False;
  try
    Result := FAppModules.FieldProperties.ValidateFieldProperty(AFieldName,AFieldValue,AErrorMessage,AFieldIndex)
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
