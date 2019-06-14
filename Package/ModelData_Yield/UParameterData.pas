//
//
//  UNIT      : Contains TParamSetup Class
//  AUTHOR    : Dziedzi  Ramulondi (Arivia)
//  DATE      : 2003/07/15
//  COPYRIGHT : Copyright © 2003 DWAF
//
//
unit UParameterData;

interface

uses
  Contnrs,
  Classes,
  VoaimsCom_TLB,
  UAbstractObject;

type

  TMatrixArray = array of array of double;
  TParamReference = class(TAbstractAppObject,IParamReference)
  protected
    FCatchReference : Integer;
    FGaugeName      : WideString;
    FNumberOfYears  : Integer;
    FStartYear      : Integer;
    FResidual1      : Double;
    FResidual2      : Double;
    FVariate1       : Double;
    FVariate2       : Double;
    FTransformType  : Integer;
    FTransformGamma : Double;
    FTransformDelta : Double;
    FTransformXlam  : Double;
    FTransformXi    : Double;
    FResidualMean   : Double;
    FResidualStdDev : Double;
    FArmaPhi1       : Double;
    FArmaPhi2       : Double;
    FArmaTheta1     : Double;
    FArmaTheta2     : Double;
    FPhiZero        : Double;
    FZTVariates     : Integer;
    FParamXa        : Double;
    FParamXSD       : Double;
    FParamAIC       : Double;
    FParamANC       : Double;
    FCatchmentArea  : Double;

    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    function ValidateArmaPhi1(AErrorMessages: TStrings) : Boolean;
    function ValidateArmaPhi2(AErrorMessages: TStrings) : Boolean;
    function ValidateArmaTheta1(AErrorMessages: TStrings) : Boolean;
    function ValidateArmaTheta2(AErrorMessages: TStrings) : Boolean;
    function ValidateCatchmentAreaParam(AErrorMessages: TStrings) : Boolean;
    function ValidateGaugeName(AErrorMessages: TStrings) : Boolean;
    function ValidateNumberOfYearsParam(AErrorMessages: TStrings) : Boolean;
    function ValidateParamAIC(AErrorMessages: TStrings) : Boolean;
    function ValidateParamANC(AErrorMessages: TStrings) : Boolean;
    function ValidateParamXa(AErrorMessages: TStrings)  : Boolean;
    function ValidateParamXSD(AErrorMessages: TStrings) : Boolean;
    function ValidatePhiZero(AErrorMessages: TStrings)  : Boolean;
    function ValidateResidual1(AErrorMessages: TStrings) : Boolean;
    function ValidateResidual2(AErrorMessages: TStrings) : Boolean;
    function ValidateResidualMean(AErrorMessages: TStrings) : Boolean;
    function ValidateResidualStdDev(AErrorMessages: TStrings) : Boolean;
    function ValidateStartYearParam(AErrorMessages: TStrings) : Boolean;
    function ValidateTransformDelta(AErrorMessages: TStrings) : Boolean;
    function ValidateTransformGamma(AErrorMessages: TStrings) : Boolean;
    function ValidateTransformType(AErrorMessages: TStrings) : Boolean;
    function ValidateTransformXi(AErrorMessages: TStrings) : Boolean;
    function ValidateTransformXlam(AErrorMessages: TStrings) : Boolean;
    function ValidateVariate1(AErrorMessages: TStrings) : Boolean;
    function ValidateVariate2(AErrorMessages: TStrings) : Boolean;
    function ValidateZTVariates(AErrorMessages: TStrings): Boolean;
  public
    function Initialise: boolean; override;
    function InitialiseToUnAssigned: boolean; virtual;
    function PopulateFromDataset(ADataSet: TAbstractModelDataset): boolean;
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; safecall;
    function Get_CatchReference: Integer; safecall;
    function Get_NumberOfYears: Integer; safecall;
    procedure Set_NumberOfYears(Value: Integer); safecall;
    function Get_StartYear: Integer; safecall;
    procedure Set_StartYear(Value: Integer); safecall;
    function Get_Residual1: Double; safecall;
    procedure Set_Residual1(Value: Double); safecall;
    function Get_Residual2: Double; safecall;
    procedure Set_Residual2(Value: Double); safecall;
    function Get_Variate1: Double; safecall;
    procedure Set_Variate1(Value: Double); safecall;
    function Get_Variate2: Double; safecall;
    procedure Set_Variate2(Value: Double); safecall;
    function Get_TransformType: Integer; safecall;
    procedure Set_TransformType(Value: Integer); safecall;
    function Get_TransformGamma: Double; safecall;
    procedure Set_TransformGamma(Value: Double); safecall;
    function Get_TransformDelta: Double; safecall;
    procedure Set_TransformDelta(Value: Double); safecall;
    function Get_TransformXlam: Double; safecall;
    procedure Set_TransformXlam(Value: Double); safecall;
    function Get_TransformXi: Double; safecall;
    procedure Set_TransformXi(Value: Double); safecall;
    function Get_ResidualMean: Double; safecall;
    procedure Set_ResidualMean(Value: Double); safecall;
    function Get_ResidualStdDev: Double; safecall;
    procedure Set_ResidualStdDev(Value: Double); safecall;
    function Get_ArmaPhi1: Double; safecall;
    procedure Set_ArmaPhi1(Value: Double); safecall;
    function Get_ArmaPhi2: Double; safecall;
    procedure Set_ArmaPhi2(Value: Double); safecall;
    function Get_ArmaTheta1: Double; safecall;
    procedure Set_ArmaTheta1(Value: Double); safecall;
    function Get_ArmaTheta2: Double; safecall;
    procedure Set_ArmaTheta2(Value: Double); safecall;
    function Get_PhiZero: Double; safecall;
    procedure Set_PhiZero(Value: Double); safecall;
    function Get_ZTVariates: Integer; safecall;
    procedure Set_ZTVariates(Value: Integer); safecall;
    function Get_ParamXa: Double; safecall;
    procedure Set_ParamXa(Value: Double); safecall;
    function Get_ParamXSD: Double; safecall;
    procedure Set_ParamXSD(Value: Double); safecall;
    function Get_ParamAIC: Double; safecall;
    procedure Set_ParamAIC(Value: Double); safecall;
    function Get_ParamANC: Double; safecall;
    procedure Set_ParamANC(Value: Double); safecall;
    function Get_CatchmentArea: Double; safecall;
    procedure Set_CatchmentArea(Value: Double); safecall;
    function Get_FileReference: WideString; safecall;
    function Get_GaugeName: WideString; safecall;
    procedure Set_GaugeName(const Value: WideString); safecall;

    property CatchReference: Integer read Get_CatchReference;
    property NumberOfYears: Integer read Get_NumberOfYears write Set_NumberOfYears;
    property StartYear: Integer read Get_StartYear write Set_StartYear;
    property Residual1: Double read Get_Residual1 write Set_Residual1;
    property Residual2: Double read Get_Residual2 write Set_Residual2;
    property Variate1: Double read Get_Variate1 write Set_Variate1;
    property Variate2: Double read Get_Variate2 write Set_Variate2;
    property TransformType: Integer read Get_TransformType write Set_TransformType;
    property TransformGamma: Double read Get_TransformGamma write Set_TransformGamma;
    property TransformDelta: Double read Get_TransformDelta write Set_TransformDelta;
    property TransformXlam: Double read Get_TransformXlam write Set_TransformXlam;
    property TransformXi: Double read Get_TransformXi write Set_TransformXi;
    property ResidualMean: Double read Get_ResidualMean write Set_ResidualMean;
    property ResidualStdDev: Double read Get_ResidualStdDev write Set_ResidualStdDev;
    property ArmaPhi1: Double read Get_ArmaPhi1 write Set_ArmaPhi1;
    property ArmaPhi2: Double read Get_ArmaPhi2 write Set_ArmaPhi2;
    property ArmaTheta1: Double read Get_ArmaTheta1 write Set_ArmaTheta1;
    property ArmaTheta2: Double read Get_ArmaTheta2 write Set_ArmaTheta2;
    property PhiZero: Double read Get_PhiZero write Set_PhiZero;
    property ZTVariates: Integer read Get_ZTVariates write Set_ZTVariates;
    property ParamXa: Double read Get_ParamXa write Set_ParamXa;
    property ParamXSD: Double read Get_ParamXSD write Set_ParamXSD;
    property ParamAIC: Double read Get_ParamAIC write Set_ParamAIC;
    property ParamANC: Double read Get_ParamANC write Set_ParamANC;
    property FileReference: WideString read Get_FileReference;
    property CatchmentArea: Double read Get_CatchmentArea write Set_CatchmentArea;
    property GaugeName: WideString read Get_GaugeName write Set_GaugeName;
  end;


  TParamSetup = class(TAbstractAppObject,IParamSetup)
  protected
    FReferenceData   : TObjectList;
    FValidReferences : TStringList;
    FMatrixB         : TMatrixArray;
    FMatrixB0        : TMatrixArray;
    FMatrixB1        : TMatrixArray;
    FMatrixA         : TMatrixArray;
    FMatrixC         : TMatrixArray;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;

    function GetAllReferenceData:TObjectList;
    function GetValidReferences : TStringList;
    function GetReferenceDataByCatchNumberCast(ACatchReferenceNumber:integer):TParamReference;
    function GetReferenceDataByIndexCast(AIndex:integer):TParamReference;

  public
    function Initialise: boolean; override;
    function LoadReferenceDataFromDataset(ADataSet: TAbstractModelDataset): boolean;
    function LoadKeyGaugeDataFromDataset (ADataSet : TAbstractModelDataset): boolean;

    function LoadMatrixBDataFromDataset (ADataSet : TAbstractModelDataset): boolean;
    function LoadMatrixB0DataFromDataset (ADataSet : TAbstractModelDataset): boolean;
    function LoadMatrixB1DataFromDataset (ADataSet : TAbstractModelDataset): boolean;
    function LoadMatrixADataFromDataset (ADataSet : TAbstractModelDataset): boolean;
    function LoadMatrixCDataFromDataset (ADataSet : TAbstractModelDataset): boolean;

    property CastReferenceDataByIndex[AIndex: Integer]: TParamReference read GetReferenceDataByIndexCast;
    property CastReferenceDataByCatchNumber[ARefNumber: Integer]: TParamReference read GetReferenceDataByCatchNumberCast;

    property AllReferenceData: TObjectList read GetAllReferenceData;
    property ValidReferences : TStringList read GetValidReferences;

    function ReferenceNumberValid(ANumber: Integer): WordBool; safecall;
    function Get_ReferenceCount: Integer; safecall;
    function Get_ReferenceDataByIndex(AIndex: Integer): IParamReference; safecall;
    function Get_ReferenceDataByCatchNumber(ARefNumber: Integer): IParamReference; safecall;
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; safecall;
    function Get_MatrixB(ARow: Integer; ACol: Integer): Double; safecall;
    function Get_MatrixB0(ARow: Integer; ACol: Integer): Double; safecall;
    function Get_MatrixB1(ARow: Integer; ACol: Integer): Double; safecall;
    function Get_MatrixA(ARow: Integer; ACol: Integer): Double; safecall;
    function Get_MatrixC(ARow: Integer; ACol: Integer): Double; safecall;
    function Get_KeyGaugeCount: Integer; safecall;
    function Get_KeyGaugeNoByIndex(AIndex: Integer): Integer; safecall;
    property ReferenceCount: Integer read Get_ReferenceCount;
    property ReferenceDataByIndex[AIndex: Integer]: IParamReference read Get_ReferenceDataByIndex;
    property ReferenceDataByCatchNumber[ARefNumber: Integer]: IParamReference read Get_ReferenceDataByCatchNumber;
    property MatrixB[ARow: Integer; ACol: Integer]: Double read Get_MatrixB;
    property MatrixB0[ARow: Integer; ACol: Integer]: Double read Get_MatrixB0;
    property MatrixB1[ARow: Integer; ACol: Integer]: Double read Get_MatrixB1;
    property MatrixA[ARow: Integer; ACol: Integer]: Double read Get_MatrixA;
    property MatrixC[ARow: Integer; ACol: Integer]: Double read Get_MatrixC;
    property KeyGaugeCount: Integer read Get_KeyGaugeCount;
    property KeyGaugeNoByIndex[AIndex: Integer]: Integer read Get_KeyGaugeNoByIndex;


  end;

implementation

uses
  SysUtils,
  UConstants,
  UReservoirData,
  UIrrigationBlock,
  UStreamFlowReduction,
  UYieldModelDataObject,
  UErrorHandlingOperations,
  UParameterDataSQLAgent;


{ TParamReference }

function TParamReference._AddRef: Integer;
const OPNAME = 'TParamReference._AddRef';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TParamReference._Release: Integer;
const OPNAME = 'TParamReference._Release';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TParamReference.InitialiseToUnAssigned: boolean;
const OPNAME = 'TParamReference.InitialiseToUnAssigned';
begin
  Result := False;
  try
    FCatchReference := 0;
    FGaugeName      := 'UnAssigned';
    FNumberOfYears  := 0;
    FStartYear      := 0;
    FResidual1      := 0;
    FResidual2      := 0;
    FVariate1       := 0;
    FVariate2       := 0;
    FTransformType  := 0;
    FTransformGamma := 0;
    FTransformDelta := 0;
    FTransformXlam  := 0;
    FTransformXi    := 0;
    FResidualMean   := 0;
    FResidualStdDev := 0;
    FArmaPhi1       := 0;
    FArmaPhi2       := 0;
    FArmaTheta1     := 0;
    FArmaTheta2     := 0;
    FPhiZero        := 0;
    FZTVariates     := 0;
    FParamXa        := 0;
    FParamXSD       := 0;
    FParamAIC       := 0;
    FParamANC       := 0;
    FCatchmentArea  := 0;
    Result          := True;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TParamReference.Initialise: boolean;
const OPNAME = 'TParamReference.Initialise';
begin
  Result := inherited Initialise;
  try
    FCatchReference := NullInteger;
    FGaugeName      := '';
    FNumberOfYears  := NullInteger;
    FStartYear      := NullInteger;
    FResidual1      := NullFloat;
    FResidual2      := NullFloat;
    FVariate1       := NullFloat;
    FVariate2       := NullFloat;
    FTransformType  := NullInteger;
    FTransformGamma := NullFloat;
    FTransformDelta := NullFloat;
    FTransformXlam  := NullFloat;
    FTransformXi    := NullFloat;
    FResidualMean   := NullFloat;
    FResidualStdDev := NullFloat;
    FArmaPhi1       := NullFloat;
    FArmaPhi2       := NullFloat;
    FArmaTheta1     := NullFloat;
    FArmaTheta2     := NullFloat;
    FPhiZero        := NullFloat;
    FZTVariates     := NullInteger;
    FParamXa        := NullFloat;
    FParamXSD       := NullFloat;
    FParamAIC       := NullFloat;
    FParamANC       := NullFloat;
    FCatchmentArea  := NullFloat;
    Result          := True;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TParamReference.PopulateFromDataset(ADataSet: TAbstractModelDataset): boolean;
const OPNAME = 'TParamReference.PopulateFromDataset';
begin
  Result := False;
  try
    FCatchReference := ADataset.DataSet.FieldByName('Identifier').AsInteger;
    FGaugeName      := Trim(ADataset.DataSet.FieldByName('GaugePathName').AsString);
    FNumberOfYears  := ADataset.DataSet.FieldByName('YearsNumber').AsInteger;
    FStartYear      := ADataset.DataSet.FieldByName('YearStart').AsInteger;
    FResidual1      := ADataset.DataSet.FieldByName('Residual1').AsFloat;
    FResidual2      := ADataset.DataSet.FieldByName('Residual2').AsFloat;
    FVariate1       := ADataset.DataSet.FieldByName('Variate1').AsFloat;
    FVariate2       := ADataset.DataSet.FieldByName('Variate2').AsFloat;
    FTransformType  := ADataset.DataSet.FieldByName('TransformType').AsInteger;
    FTransformGamma := ADataset.DataSet.FieldByName('TransformGamma').AsFloat;
    FTransformDelta := ADataset.DataSet.FieldByName('TransformDelta').AsFloat;
    FTransformXlam  := ADataset.DataSet.FieldByName('TransformXlam').AsFloat;
    FTransformXi    := ADataset.DataSet.FieldByName('TransformXi').AsFloat;
    FResidualMean   := ADataset.DataSet.FieldByName('ResidualMean').AsFloat;
    FResidualStdDev := ADataset.DataSet.FieldByName('ResidualStdDev').AsFloat;
    FArmaPhi1       := ADataset.DataSet.FieldByName('ArmaPhi1').AsFloat;
    FArmaPhi2       := ADataset.DataSet.FieldByName('ArmaPhi2').AsFloat;
    FArmaTheta1     := ADataset.DataSet.FieldByName('ArmaTheta1').AsFloat;
    FArmaTheta2     := ADataset.DataSet.FieldByName('ArmaTheta2').AsFloat;
    FPhiZero        := ADataset.DataSet.FieldByName('PhiZero').AsFloat;
    FZTVariates     := ADataset.DataSet.FieldByName('ZTVariates').AsInteger;
    FParamXA        := ADataset.DataSet.FieldByName('ParamXA').AsFloat;
    FParamXSD       := ADataset.DataSet.FieldByName('ParamXSD').AsFloat;
    FParamAIC       := ADataset.DataSet.FieldByName('ParamAIC').AsFloat;
    FParamANC       := ADataset.DataSet.FieldByName('ParamANC').AsFloat;
    FCatchmentArea  := ADataset.DataSet.FieldByName('CatchmentArea').AsFloat;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TParamReference.Validate(var AErrors: WideString;  const AContext: WideString): WordBool;
const OPNAME = 'TParamReference.Validate';
var
  LErrorMessage : TStringList;
  LErrorCols    : TStringList;
  lStopOnFirstError : Boolean;
begin
  Result := False;
  try
    LErrorMessage := TStringList.Create;
    LErrorCols    := TStringList.Create;
    try
      if (AContext = 'ArmaPhi1') then
        Result := ValidateArmaPhi1(lErrorMessage)
      else
      if (AContext = 'ArmaPhi2') then
        Result := ValidateArmaPhi2(lErrorMessage)
      else
      if (AContext = 'ArmaTheta1') then
        Result := ValidateArmaTheta1(lErrorMessage)
      else
      if (AContext = 'ArmaTheta2') then
        Result := ValidateArmaTheta2(lErrorMessage)
      else
      if (AContext = 'CatchmentAreaParam') then
        Result := ValidateCatchmentAreaParam(lErrorMessage)
      else
      if (AContext = 'GaugeName') then
        Result := ValidateGaugeName(lErrorMessage)
      else
      if (AContext = 'NumberOfYearsParam') then
        Result := ValidateNumberOfYearsParam(lErrorMessage)
      else
      if (AContext = 'ParamAIC') then
        Result := ValidateParamAIC(lErrorMessage)
      else
      if (AContext = 'ParamANC') then
        Result := ValidateParamANC(lErrorMessage)
      else
      if (AContext = 'ParamXa') then
        Result := ValidateParamXa(lErrorMessage)
      else
      if (AContext = 'ParamXSD') then
        Result := ValidateParamXSD(lErrorMessage)
      else
      if (AContext = 'PhiZero') then
        Result := ValidatePhiZero(lErrorMessage)
      else
      if (AContext = 'Residual1') then
        Result := ValidateResidual1(lErrorMessage)
      else
      if (AContext = 'Residual2') then
        Result := ValidateResidual2(lErrorMessage)
      else
      if (AContext = 'ResidualMean') then
        Result := ValidateResidualMean(lErrorMessage)
      else
      if (AContext = 'ResidualStdDev') then
        Result := ValidateResidualStdDev(lErrorMessage)
      else
      if (AContext = 'StartYearParam') then
        Result := ValidateStartYearParam(lErrorMessage)
      else
      if (AContext = 'TransformDelta') then
        Result := ValidateTransformDelta(lErrorMessage)
      else
      if (AContext = 'TransformGamma') then
        Result := ValidateTransformGamma(lErrorMessage)
      else
      if (AContext = 'TransformType') then
        Result := ValidateTransformType(lErrorMessage)
      else
      if (AContext = 'TransformXi') then
        Result := ValidateTransformXi(lErrorMessage)
      else
      if (AContext = 'TransformXlam') then
        Result := ValidateTransformXlam(lErrorMessage)
      else
      if (AContext = 'Variate1') then
        Result := ValidateVariate1(lErrorMessage)
      else
      if (AContext = 'Variate2') then
        Result := ValidateVariate2(lErrorMessage)
      else
      if (AContext = 'ZTVariates') then
        Result := ValidateZTVariates(lErrorMessage)
      else
      begin
        Result := True;
        lStopOnFirstError := FAppModules.GlobalData.StopOnFirstErr;
        if (not ValidateArmaPhi1(lErrorMessage))           then Result := False;
        if (Result OR (NOT lStopOnFirstError)) then
        begin
          if (not ValidateArmaPhi2(lErrorMessage))   then Result := False;
        end;
        if (Result OR (NOT lStopOnFirstError)) then
        begin
          if (not ValidateArmaTheta1(lErrorMessage))    then Result := False;
        end;
        if (Result OR (NOT lStopOnFirstError)) then
        begin
          if (not ValidateArmaTheta2(lErrorMessage))       then Result := False;
        end;
        if (Result OR (NOT lStopOnFirstError)) then
        begin
          if (not ValidateCatchmentAreaParam(lErrorMessage))   then Result := False;
        end;
        if (Result OR (NOT lStopOnFirstError)) then
        begin
          if (not ValidateGaugeName(lErrorMessage))   then Result := False;
        end;
        if (Result OR (NOT lStopOnFirstError)) then
        begin
          if (not ValidateNumberOfYearsParam(lErrorMessage))  then Result := False;
        end;
        if (Result OR (NOT lStopOnFirstError)) then
        begin
          if (not ValidateParamAIC(lErrorMessage))  then Result := False;
        end;
        if (Result OR (NOT lStopOnFirstError)) then
        begin
          if (not ValidateParamANC(lErrorMessage))  then Result := False;
        end;
        if (Result OR (NOT lStopOnFirstError)) then
        begin
          if (not ValidateParamXa(lErrorMessage))  then Result := False;
        end;
        if (Result OR (NOT lStopOnFirstError)) then
        begin
          if (not ValidateParamXSD(lErrorMessage))  then Result := False;
        end;
        if (Result OR (NOT lStopOnFirstError)) then
        begin
          if (not ValidatePhiZero(lErrorMessage))  then Result := False;
        end;
        if (Result OR (NOT lStopOnFirstError)) then
        begin
          if (not ValidateResidual1(lErrorMessage))  then Result := False;
        end;
        if (Result OR (NOT lStopOnFirstError)) then
        begin
          if (not ValidateResidual2(lErrorMessage))  then Result := False;
        end;
        if (Result OR (NOT lStopOnFirstError)) then
        begin
          if (not ValidateResidualMean(lErrorMessage))  then Result := False;
        end;
        if (Result OR (NOT lStopOnFirstError)) then
        begin
          if (not ValidateResidualStdDev(lErrorMessage))  then Result := False;
        end;
        if (Result OR (NOT lStopOnFirstError)) then
        begin
          if (not ValidateStartYearParam(lErrorMessage))  then Result := False;
        end;
        if (Result OR (NOT lStopOnFirstError)) then
        begin
          if (not ValidateTransformDelta(lErrorMessage))  then Result := False;
        end;
        if (Result OR (NOT lStopOnFirstError)) then
        begin
          if (not ValidateTransformGamma(lErrorMessage))  then Result := False;
        end;
        if (Result OR (NOT lStopOnFirstError)) then
        begin
          if (not ValidateTransformType(lErrorMessage))  then Result := False;
        end;
        if (Result OR (NOT lStopOnFirstError)) then
        begin
          if (not ValidateTransformXi(lErrorMessage))  then Result := False;
        end;
        if (Result OR (NOT lStopOnFirstError)) then
        begin
          if (not ValidateTransformXlam(lErrorMessage))  then Result := False;
        end;
        if (Result OR (NOT lStopOnFirstError)) then
        begin
          if (not ValidateVariate1(lErrorMessage))  then Result := False;
        end;
        if (Result OR (NOT lStopOnFirstError)) then
        begin
          if (not ValidateVariate2(lErrorMessage))  then Result := False;
        end;
        if (Result OR (NOT lStopOnFirstError)) then
        begin
          if (not ValidateZTVariates(lErrorMessage))  then Result := False;
        end;
      end;
      AErrors := AErrors + LErrorMessage.Text;
    finally
      LErrorMessage.Free;
      LErrorCols.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TParamReference.Get_FileReference: WideString;
const OPNAME = 'TParamReference.Get_FileReference';
begin
  Result := '';
  try
    Result := FGaugeName;
    if(TYieldModelDataObject(FAppModules.Model.ModelData).FileNamesObject  <> nil) then
      Result := TYieldModelDataObject(FAppModules.Model.ModelData).FileNamesObject.HydrologyFilesPath + FGaugeName;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TParamReference.Get_GaugeName: WideString;
const OPNAME = 'TParamReference.Get_GaugeName';
begin
  Result := '';
  try
    Result := FGaugeName;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TParamReference.Get_ArmaPhi1: Double;
const OPNAME = 'TParamReference.Get_ArmaPhi1';
begin
  Result := NullFloat;
  try
    Result := FArmaPhi1;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TParamReference.Get_ArmaPhi2: Double;
const OPNAME = 'TParamReference.Get_ArmaPhi2';
begin
  Result := NullFloat;
  try
    Result := FArmaPhi2;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TParamReference.Get_ArmaTheta1: Double;
const OPNAME = 'TParamReference.Get_ArmaTheta1';
begin
  Result := NullFloat;
  try
    Result := FArmaTheta1;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TParamReference.Get_ArmaTheta2: Double;
const OPNAME = 'TParamReference.Get_ArmaTheta2';
begin
  Result := NullFloat;
  try
    Result := FArmaTheta2;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TParamReference.Get_CatchmentArea: double;
const OPNAME = 'TParamReference.Get_CatchmentArea';
begin
  Result := NullFloat;
  try
    Result := FCatchmentArea;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TParamReference.Get_CatchReference: integer;
const OPNAME = 'TParamReference.Get_CatchReference';
begin
  Result := NullInteger;
  try
    Result := FCatchReference;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TParamReference.Get_NumberOfYears: Integer;
const OPNAME = 'TParamReference.Get_NumberOfYears';
var
  LNumberOfYears : Integer;
begin
  Result := NullInteger;
  try
    LNumberOfYears := TYieldModelDataObject(FAppModules.Model.ModelData).RunConfigurationData.YearsInAnalysis;
    if(LNumberOfYears < FNumberOfYears) then
      Result :=  LNumberOfYears
    else
      Result := FNumberOfYears;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TParamReference.Get_ParamAIC: Double;
const OPNAME = 'TParamReference.Get_ParamAIC';
begin
  Result := NullFloat;
  try
    Result := FParamAIC;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TParamReference.Get_ParamANC: Double;
const OPNAME = 'TParamReference.Get_ParamANC';
begin
  Result := NullFloat;
  try
    Result := FParamANC;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TParamReference.Get_ParamXa: Double;
const OPNAME = 'TParamReference.Get_ParamXa';
begin
  Result := NullFloat;
  try
    Result := FParamXa;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TParamReference.Get_ParamXSD: Double;
const OPNAME = 'TParamReference.Get_ParamXSD';
begin
  Result := NullFloat;
  try
    Result := FParamXSD;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TParamReference.Get_PhiZero: Double;
const OPNAME = 'TParamReference.Get_PhiZero';
begin
  Result := NullFloat;
  try
    Result := FPhiZero;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TParamReference.Get_Residual1: Double;
const OPNAME = 'TParamReference.Get_Residual1';
begin
  Result := NullFloat;
  try
    Result := FResidual1;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TParamReference.Get_Residual2: Double;
const OPNAME = 'TParamReference.Get_Residual2';
begin
  Result := NullFloat;
  try
    Result := FResidual2;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TParamReference.Get_ResidualMean: Double;
const OPNAME = 'TParamReference.Get_ResidualMean';
begin
  Result := NullFloat;
  try
    Result := FResidualMean;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TParamReference.Get_ResidualStdDev: Double;
const OPNAME = 'TParamReference.Get_ResidualStdDev';
begin
  Result := NullFloat;
  try
    Result := FResidualStdDev;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TParamReference.Get_StartYear: Integer;
const OPNAME = 'TParamReference.Get_StartYear';
var
  LStartYearOther : Integer;
begin
  Result := NullInteger;
  try
    LStartYearOther := TYieldModelDataObject(FAppModules.Model.ModelData).RunConfigurationData.StartYearOther;
    if(LStartYearOther > FStartYear) then
      Result :=  LStartYearOther
    else
      Result := FStartYear;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TParamReference.Get_TransformDelta: Double;
const OPNAME = 'TParamReference.Get_TransformDelta';
begin
  Result := NullFloat;
  try
    Result := FTransformDelta;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TParamReference.Get_TransformGamma: Double;
const OPNAME = 'TParamReference.Get_TransformGamma';
begin
  Result := NullFloat;
  try
    Result := FTransformGamma;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TParamReference.Get_TransformType: Integer;
const OPNAME = 'TParamReference.Get_TransformType';
begin
  Result := NullInteger;
  try
    Result := FTransformType;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TParamReference.Get_TransformXi: Double;
const OPNAME = 'TParamReference.Get_TransformXi';
begin
  Result := NullFloat;
  try
    Result := FTransformXi;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TParamReference.Get_TransformXlam: Double;
const OPNAME = 'TParamReference.Get_TransformXlam';
begin
  Result := NullFloat;
  try
    Result := FTransformXlam;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TParamReference.Get_Variate1: Double;
const OPNAME = 'TParamReference.Get_Variate1';
begin
  Result := NullFloat;
  try
    Result := FVariate1;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TParamReference.Get_Variate2: Double;
const OPNAME = 'TParamReference.Get_Variate2';
begin
  Result := NullFloat;
  try
    Result := FVariate2;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TParamReference.Get_ZTVariates: Integer;
const OPNAME = 'TParamReference.Get_ZTVariates';
begin
  Result := NullInteger;
  try
    Result := FZTVariates;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TParamReference.Set_ArmaPhi1(Value: Double);
const OPNAME = 'TParamReference.Set_ArmaPhi1';
var
  LLoadAgent    : TParameterDataSQLAgent;
  LContextData  : TStringList;
  LOldValue     : string;
begin
  try
    if FCatchmentArea <> Value then
    begin
      LContextData  := TStringList.Create;
      LLoadAgent    := TParameterDataSQLAgent.Create(FAppModules);
      try
        LOldValue := FloatToStr(FArmaPhi1);
        LLoadAgent.LoadContextData_FeatureID(LContextData, IntToStr(FCatchReference));
        if FAppModules.FieldProperties.UpdateFieldValue('ArmaPhi1', FloatToStr(Value), LOldValue, LContextData ) then
        begin
          FArmaPhi1 := Value;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'ArmaPhi1',LOldValue,FloatToStr(Value));
        end;
      finally
        FreeAndNil(LLoadAgent);
        FreeAndNil(LContextData);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TParamReference.Set_ArmaPhi2(Value: Double);
const OPNAME = 'TParamReference.Set_ArmaPhi2';
var
  LLoadAgent    : TParameterDataSQLAgent;
  LContextData  : TStringList;
  LOldValue     : string;
begin
  try
    if FCatchmentArea <> Value then
    begin
      LContextData  := TStringList.Create;
      LLoadAgent    := TParameterDataSQLAgent.Create(FAppModules);
      try
        LOldValue := FloatToStr(FArmaPhi2);
        LLoadAgent.LoadContextData_FeatureID(LContextData, IntToStr(FCatchReference));
        if FAppModules.FieldProperties.UpdateFieldValue('ArmaPhi2', FloatToStr(Value), LOldValue, LContextData ) then
        begin
          FArmaPhi2 := Value;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'ArmaPhi2',LOldValue,FloatToStr(Value));
        end;
      finally
        FreeAndNil(LLoadAgent);
        FreeAndNil(LContextData);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TParamReference.Set_ArmaTheta1(Value: Double);
const OPNAME = 'TParamReference.Set_ArmaTheta1';
var
  LLoadAgent    : TParameterDataSQLAgent;
  LContextData  : TStringList;
  LOldValue     : string;
begin
  try
    if FCatchmentArea <> Value then
    begin
      LContextData  := TStringList.Create;
      LLoadAgent    := TParameterDataSQLAgent.Create(FAppModules);
      try
        LOldValue := FloatToStr(FArmaTheta1);
        LLoadAgent.LoadContextData_FeatureID(LContextData, IntToStr(FCatchReference));
        if FAppModules.FieldProperties.UpdateFieldValue('ArmaTheta1', FloatToStr(Value), LOldValue, LContextData ) then
        begin
          FArmaTheta1 := Value;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'ArmaTheta1',LOldValue,FloatToStr(Value));
        end;
      finally
        FreeAndNil(LLoadAgent);
        FreeAndNil(LContextData);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TParamReference.Set_ArmaTheta2(Value: Double);
const OPNAME = 'TParamReference.Set_ArmaTheta2';
var
  LLoadAgent    : TParameterDataSQLAgent;
  LContextData  : TStringList;
  LOldValue     : string;
begin
  try
    if FCatchmentArea <> Value then
    begin
      LContextData  := TStringList.Create;
      LLoadAgent    := TParameterDataSQLAgent.Create(FAppModules);
      try
        LOldValue := FloatToStr(FArmaTheta2);
        LLoadAgent.LoadContextData_FeatureID(LContextData, IntToStr(FCatchReference));
        if FAppModules.FieldProperties.UpdateFieldValue('ArmaTheta2', FloatToStr(Value), LOldValue, LContextData ) then
        begin
          FArmaTheta2 := Value;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'ArmaTheta2',LOldValue,FloatToStr(Value));
        end;
      finally
        FreeAndNil(LLoadAgent);
        FreeAndNil(LContextData);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TParamReference.Set_CatchmentArea(Value: Double);
const OPNAME = 'TParamReference.Set_CatchmentArea';
var
  LLoadAgent    : TParameterDataSQLAgent;
  LContextData  : TStringList;
  LOldValue     : string;
begin
  try
    if FCatchmentArea <> Value then
    begin
      LContextData  := TStringList.Create;
      LLoadAgent    := TParameterDataSQLAgent.Create(FAppModules);
      try
        LOldValue := FloatToStr(FCatchmentArea);
        LLoadAgent.LoadContextData_FeatureID(LContextData, IntToStr(FCatchReference));
        if FAppModules.FieldProperties.UpdateFieldValue('CatchmentAreaParam', FloatToStr(Value), LOldValue, LContextData ) then
        begin
          FCatchmentArea := Value;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'CatchmentAreaParam',LOldValue,FloatToStr(Value));
        end;
      finally
        FreeAndNil(LLoadAgent);
        FreeAndNil(LContextData);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TParamReference.Set_NumberOfYears(Value: Integer);
const OPNAME = 'TParamReference.Set_NumberOfYears';
var
  LLoadAgent    : TParameterDataSQLAgent;
  LContextData  : TStringList;
  LOldValue     : string;
begin
  try
    if FCatchmentArea <> Value then
    begin
      LContextData  := TStringList.Create;
      LLoadAgent    := TParameterDataSQLAgent.Create(FAppModules);
      try
        LOldValue := IntToStr(FNumberOfYears);
        LLoadAgent.LoadContextData_FeatureID(LContextData, IntToStr(FCatchReference));
        if FAppModules.FieldProperties.UpdateFieldValue('NumberOfYearsParam', FloatToStr(Value), LOldValue, LContextData ) then
        begin
          FNumberOfYears := Value;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'NumberOfYearsParam',LOldValue,FloatToStr(Value));
        end;
      finally
        FreeAndNil(LLoadAgent);
        FreeAndNil(LContextData);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TParamReference.Set_ParamAIC(Value: Double);
const OPNAME = 'TParamReference.Set_ParamAIC';
var
  LLoadAgent    : TParameterDataSQLAgent;
  LContextData  : TStringList;
  LOldValue     : string;
begin
  try
    if FCatchmentArea <> Value then
    begin
      LContextData  := TStringList.Create;
      LLoadAgent    := TParameterDataSQLAgent.Create(FAppModules);
      try
        LOldValue := FloatToStr(FParamAIC);
        LLoadAgent.LoadContextData_FeatureID(LContextData, IntToStr(FCatchReference));
        if FAppModules.FieldProperties.UpdateFieldValue('ParamAIC', FloatToStr(Value), LOldValue, LContextData ) then
        begin
          FParamAIC := Value;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'ParamAIC',LOldValue,FloatToStr(Value));
        end;
      finally
        FreeAndNil(LLoadAgent);
        FreeAndNil(LContextData);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TParamReference.Set_ParamANC(Value: Double);
const OPNAME = 'TParamReference.Set_ParamANC';
var
  LLoadAgent    : TParameterDataSQLAgent;
  LContextData  : TStringList;
  LOldValue     : string;
begin
  try
    if FCatchmentArea <> Value then
    begin
      LContextData  := TStringList.Create;
      LLoadAgent    := TParameterDataSQLAgent.Create(FAppModules);
      try
        LOldValue := FloatToStr(FParamANC);
        LLoadAgent.LoadContextData_FeatureID(LContextData, IntToStr(FCatchReference));
        if FAppModules.FieldProperties.UpdateFieldValue('ParamANC', FloatToStr(Value), LOldValue, LContextData ) then
        begin
          FParamANC := Value;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'ParamANC',LOldValue,FloatToStr(Value));
        end;
      finally
        FreeAndNil(LLoadAgent);
        FreeAndNil(LContextData);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TParamReference.Set_ParamXa(Value: Double);
const OPNAME = 'TParamReference.Set_ParamXa';
var
  LLoadAgent    : TParameterDataSQLAgent;
  LContextData  : TStringList;
  LOldValue     : string;
begin
  try
    if FCatchmentArea <> Value then
    begin
      LContextData  := TStringList.Create;
      LLoadAgent    := TParameterDataSQLAgent.Create(FAppModules);
      try
        LOldValue := FloatToStr(FParamXa);
        LLoadAgent.LoadContextData_FeatureID(LContextData, IntToStr(FCatchReference));
        if FAppModules.FieldProperties.UpdateFieldValue('ParamXa', FloatToStr(Value), LOldValue, LContextData ) then
        begin
          FParamXa := Value;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'ParamXa',LOldValue,FloatToStr(Value));
        end;
      finally
        FreeAndNil(LLoadAgent);
        FreeAndNil(LContextData);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TParamReference.Set_ParamXSD(Value: Double);
const OPNAME = 'TParamReference.Set_ParamXSD';
var
  LLoadAgent    : TParameterDataSQLAgent;
  LContextData  : TStringList;
  LOldValue     : string;
begin
  try
    if FCatchmentArea <> Value then
    begin
      LContextData  := TStringList.Create;
      LLoadAgent    := TParameterDataSQLAgent.Create(FAppModules);
      try
        LOldValue := FloatToStr(FParamXSD);
        LLoadAgent.LoadContextData_FeatureID(LContextData, IntToStr(FCatchReference));
        if FAppModules.FieldProperties.UpdateFieldValue('ParamXSD', FloatToStr(Value), LOldValue, LContextData ) then
        begin
          FParamXSD := Value;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'ParamXSD',LOldValue,FloatToStr(Value));
        end;
      finally
        FreeAndNil(LLoadAgent);
        FreeAndNil(LContextData);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TParamReference.Set_PhiZero(Value: Double);
const OPNAME = 'TParamReference.Set_PhiZero';
var
  LLoadAgent    : TParameterDataSQLAgent;
  LContextData  : TStringList;
  LOldValue     : string;
begin
  try
    if FCatchmentArea <> Value then
    begin
      LContextData  := TStringList.Create;
      LLoadAgent    := TParameterDataSQLAgent.Create(FAppModules);
      try
        LOldValue := FloatToStr(FPhiZero);
        LLoadAgent.LoadContextData_FeatureID(LContextData, IntToStr(FCatchReference));
        if FAppModules.FieldProperties.UpdateFieldValue('PhiZero', FloatToStr(Value), LOldValue, LContextData ) then
        begin
          FPhiZero := Value;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'PhiZero',LOldValue,FloatToStr(Value));
        end;
      finally
        FreeAndNil(LLoadAgent);
        FreeAndNil(LContextData);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TParamReference.Set_Residual1(Value: Double);
const OPNAME = 'TParamReference.Set_Residual1';
var
  LLoadAgent    : TParameterDataSQLAgent;
  LContextData  : TStringList;
  LOldValue     : string;
begin
  try
    if FCatchmentArea <> Value then
    begin
      LContextData  := TStringList.Create;
      LLoadAgent    := TParameterDataSQLAgent.Create(FAppModules);
      try
        LOldValue := FloatToStr(FResidual1);
        LLoadAgent.LoadContextData_FeatureID(LContextData, IntToStr(FCatchReference));
        if FAppModules.FieldProperties.UpdateFieldValue('Residual1', FloatToStr(Value), LOldValue, LContextData ) then
        begin
          FResidual1 := Value;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'Residual1',LOldValue,FloatToStr(Value));
        end;
      finally
        FreeAndNil(LLoadAgent);
        FreeAndNil(LContextData);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TParamReference.Set_Residual2(Value: Double);
const OPNAME = 'TParamReference.Set_Residual2';
var
  LLoadAgent    : TParameterDataSQLAgent;
  LContextData  : TStringList;
  LOldValue     : string;
begin
  try
    if FCatchmentArea <> Value then
    begin
      LContextData  := TStringList.Create;
      LLoadAgent    := TParameterDataSQLAgent.Create(FAppModules);
      try
        LOldValue := FloatToStr(FResidual2);
        LLoadAgent.LoadContextData_FeatureID(LContextData, IntToStr(FCatchReference));
        if FAppModules.FieldProperties.UpdateFieldValue('Residual2', FloatToStr(Value), LOldValue, LContextData ) then
        begin
          FResidual2 := Value;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'Residual2',LOldValue,FloatToStr(Value));
        end;
      finally
        FreeAndNil(LLoadAgent);
        FreeAndNil(LContextData);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TParamReference.Set_ResidualMean(Value: Double);
const OPNAME = 'TParamReference.Set_ResidualMean';
var
  LLoadAgent    : TParameterDataSQLAgent;
  LContextData  : TStringList;
  LOldValue     : string;
begin
  try
    if FCatchmentArea <> Value then
    begin
      LContextData  := TStringList.Create;
      LLoadAgent    := TParameterDataSQLAgent.Create(FAppModules);
      try
        LOldValue := FloatToStr(FResidualMean);
        LLoadAgent.LoadContextData_FeatureID(LContextData, IntToStr(FCatchReference));
        if FAppModules.FieldProperties.UpdateFieldValue('ResidualMean', FloatToStr(Value), LOldValue, LContextData ) then
        begin
          FResidualMean := Value;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'ResidualMean',LOldValue,FloatToStr(Value));
        end;
      finally
        FreeAndNil(LLoadAgent);
        FreeAndNil(LContextData);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TParamReference.Set_ResidualStdDev(Value: Double);
const OPNAME = 'TParamReference.Set_ResidualStdDev';
var
  LLoadAgent    : TParameterDataSQLAgent;
  LContextData  : TStringList;
  LOldValue     : string;
begin
  try
    if FCatchmentArea <> Value then
    begin
      LContextData  := TStringList.Create;
      LLoadAgent    := TParameterDataSQLAgent.Create(FAppModules);
      try
        LOldValue := FloatToStr(FResidualStdDev);
        LLoadAgent.LoadContextData_FeatureID(LContextData, IntToStr(FCatchReference));
        if FAppModules.FieldProperties.UpdateFieldValue('ResidualStdDev', FloatToStr(Value), LOldValue, LContextData ) then
        begin
          FResidualStdDev := Value;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'ResidualStdDev',LOldValue,FloatToStr(Value));
        end;
      finally
        FreeAndNil(LLoadAgent);
        FreeAndNil(LContextData);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TParamReference.Set_StartYear(Value: Integer);
const OPNAME = 'TParamReference.Set_StartYear';
var
  LLoadAgent    : TParameterDataSQLAgent;
  LContextData  : TStringList;
  LOldValue     : string;
begin
  try
    if FCatchmentArea <> Value then
    begin
      LContextData  := TStringList.Create;
      LLoadAgent    := TParameterDataSQLAgent.Create(FAppModules);
      try
        LOldValue := IntToStr(FStartYear);
        LLoadAgent.LoadContextData_FeatureID(LContextData, IntToStr(FCatchReference));
        if FAppModules.FieldProperties.UpdateFieldValue('StartYearParam', FloatToStr(Value), LOldValue, LContextData ) then
        begin
          FStartYear := Value;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'StartYearParam',LOldValue,FloatToStr(Value));
        end;
      finally
        FreeAndNil(LLoadAgent);
        FreeAndNil(LContextData);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TParamReference.Set_TransformDelta(Value: Double);
const OPNAME = 'TParamReference.Set_TransformDelta';
var
  LLoadAgent    : TParameterDataSQLAgent;
  LContextData  : TStringList;
  LOldValue     : string;
begin
  try
    if FCatchmentArea <> Value then
    begin
      LContextData  := TStringList.Create;
      LLoadAgent    := TParameterDataSQLAgent.Create(FAppModules);
      try
        LOldValue := FloatToStr(FTransformDelta);
        LLoadAgent.LoadContextData_FeatureID(LContextData, IntToStr(FCatchReference));
        if FAppModules.FieldProperties.UpdateFieldValue('TransformDelta', FloatToStr(Value), LOldValue, LContextData ) then
        begin
          FTransformDelta := Value;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'TransformDelta',LOldValue,FloatToStr(Value));
        end;
      finally
        FreeAndNil(LLoadAgent);
        FreeAndNil(LContextData);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TParamReference.Set_TransformGamma(Value: Double);
const OPNAME = 'TParamReference.Set_TransformGamma';
var
  LLoadAgent    : TParameterDataSQLAgent;
  LContextData  : TStringList;
  LOldValue     : string;
begin
  try
    if FCatchmentArea <> Value then
    begin
      LContextData  := TStringList.Create;
      LLoadAgent    := TParameterDataSQLAgent.Create(FAppModules);
      try
        LOldValue := FloatToStr(FTransformGamma);
        LLoadAgent.LoadContextData_FeatureID(LContextData, IntToStr(FCatchReference));
        if FAppModules.FieldProperties.UpdateFieldValue('TransformGamma', FloatToStr(Value), LOldValue, LContextData ) then
        begin
          FTransformGamma := Value;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'TransformGamma',LOldValue,FloatToStr(Value));
        end;
      finally
        FreeAndNil(LLoadAgent);
        FreeAndNil(LContextData);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TParamReference.Set_TransformType(Value: Integer);
const OPNAME = 'TParamReference.Set_TransformType';
var
  LLoadAgent    : TParameterDataSQLAgent;
  LContextData  : TStringList;
  LOldValue     : string;
begin
  try
    if FCatchmentArea <> Value then
    begin
      LContextData  := TStringList.Create;
      LLoadAgent    := TParameterDataSQLAgent.Create(FAppModules);
      try
        LOldValue := IntToStr(FTransformType);
        LLoadAgent.LoadContextData_FeatureID(LContextData, IntToStr(FCatchReference));
        if FAppModules.FieldProperties.UpdateFieldValue('TransformType', FloatToStr(Value), LOldValue, LContextData ) then
        begin
          FTransformType := Value;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'TransformType',LOldValue,FloatToStr(Value));
        end;
      finally
        FreeAndNil(LLoadAgent);
        FreeAndNil(LContextData);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TParamReference.Set_TransformXi(Value: Double);
const OPNAME = 'TParamReference.Set_TransformXi';
var
  LLoadAgent    : TParameterDataSQLAgent;
  LContextData  : TStringList;
  LOldValue     : string;
begin
  try
    if FCatchmentArea <> Value then
    begin
      LContextData  := TStringList.Create;
      LLoadAgent    := TParameterDataSQLAgent.Create(FAppModules);
      try
        LOldValue := FloatToStr(FTransformXi);
        LLoadAgent.LoadContextData_FeatureID(LContextData, IntToStr(FCatchReference));
        if FAppModules.FieldProperties.UpdateFieldValue('TransformXi', FloatToStr(Value), LOldValue, LContextData ) then
        begin
          FTransformXi := Value;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'TransformXi',LOldValue,FloatToStr(Value));
        end;
      finally
        FreeAndNil(LLoadAgent);
        FreeAndNil(LContextData);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TParamReference.Set_TransformXlam(Value: Double);
const OPNAME = 'TParamReference.Set_TransformXlam';
var
  LLoadAgent    : TParameterDataSQLAgent;
  LContextData  : TStringList;
  LOldValue     : string;
begin
  try
    if FCatchmentArea <> Value then
    begin
      LContextData  := TStringList.Create;
      LLoadAgent    := TParameterDataSQLAgent.Create(FAppModules);
      try
        LOldValue := FloatToStr(FTransformXlam);
        LLoadAgent.LoadContextData_FeatureID(LContextData, IntToStr(FCatchReference));
        if FAppModules.FieldProperties.UpdateFieldValue('TransformXlam', FloatToStr(Value), LOldValue, LContextData ) then
        begin
          FTransformXlam := Value;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'TransformXlam',LOldValue,FloatToStr(Value));
        end;
      finally
        FreeAndNil(LLoadAgent);
        FreeAndNil(LContextData);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TParamReference.Set_Variate1(Value: Double);
const OPNAME = 'TParamReference.Set_Variate1';
var
  LLoadAgent    : TParameterDataSQLAgent;
  LContextData  : TStringList;
  LOldValue     : string;
begin
  try
    if FCatchmentArea <> Value then
    begin
      LContextData  := TStringList.Create;
      LLoadAgent    := TParameterDataSQLAgent.Create(FAppModules);
      try
        LOldValue := FloatToStr(FVariate1);
        LLoadAgent.LoadContextData_FeatureID(LContextData, IntToStr(FCatchReference));
        if FAppModules.FieldProperties.UpdateFieldValue('Variate1', FloatToStr(Value), LOldValue, LContextData ) then
        begin
          FVariate1 := Value;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'Variate1',LOldValue,FloatToStr(Value));
        end;
      finally
        FreeAndNil(LLoadAgent);
        FreeAndNil(LContextData);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TParamReference.Set_Variate2(Value: Double);
const OPNAME = 'TParamReference.Set_Variate2';
var
  LLoadAgent    : TParameterDataSQLAgent;
  LContextData  : TStringList;
  LOldValue     : string;
begin
  try
    if FCatchmentArea <> Value then
    begin
      LContextData  := TStringList.Create;
      LLoadAgent    := TParameterDataSQLAgent.Create(FAppModules);
      try
        LOldValue := FloatToStr(FVariate2);
        LLoadAgent.LoadContextData_FeatureID(LContextData, IntToStr(FCatchReference));
        if FAppModules.FieldProperties.UpdateFieldValue('Variate2', FloatToStr(Value), LOldValue, LContextData ) then
        begin
          FVariate2 := Value;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'Variate2',LOldValue,FloatToStr(Value));
        end;
      finally
        FreeAndNil(LLoadAgent);
        FreeAndNil(LContextData);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TParamReference.Set_ZTVariates(Value: Integer);
const OPNAME = 'TParamReference.Set_ZTVariates';
var
  LLoadAgent    : TParameterDataSQLAgent;
  LContextData  : TStringList;
  LOldValue     : string;
begin
  try
    if FZTVariates <> Value then
    begin
      LContextData  := TStringList.Create;
      LLoadAgent    := TParameterDataSQLAgent.Create(FAppModules);
      try
        LOldValue := IntToStr(FZTVariates);
        LLoadAgent.LoadContextData_FeatureID(LContextData, IntToStr(FCatchReference));
        if FAppModules.FieldProperties.UpdateFieldValue('ZTVariates', FloatToStr(Value), LOldValue, LContextData ) then
        begin
          FZTVariates := Value;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'ZTVariates',LOldValue,FloatToStr(Value));
        end;
      finally
        FreeAndNil(LLoadAgent);
        FreeAndNil(LContextData);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TParamReference.Set_GaugeName(const Value: WideString);
const OPNAME = 'TParamReference.Set_GaugeName';
var
  LLoadAgent    : TParameterDataSQLAgent;
  LContextData  : TStringList;
  LOldValue     : string;
begin
  try
    if FGaugeName <> Value then
    begin
      LContextData  := TStringList.Create;
      LLoadAgent    := TParameterDataSQLAgent.Create(FAppModules);
      try
        LOldValue := FGaugeName;
        LLoadAgent.LoadContextData_FeatureID(LContextData, IntToStr(FCatchReference));
        if FAppModules.FieldProperties.UpdateFieldValue('GaugeName', Value, LOldValue, LContextData ) then
        begin
          FGaugeName := Value;
          FAppModules.Model.StudyDataHasChanged(sdccEdit,'GaugeName',LOldValue,Value);
        end;
      finally
        FreeAndNil(LLoadAgent);
        FreeAndNil(LContextData);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TParamReference.ValidateArmaPhi1(AErrorMessages: TStrings): Boolean;
const OPNAME = 'TParamReference.ValidateArmaPhi1';
var
  lMessage : string;
begin
  Result := False;
  try
    lMessage := '';
    if (not FAppModules.FieldProperties.ValidateFieldProperty('ArmaPhi1', FloatToStr(FArmaPhi1), lMessage)) then
      AErrorMessages.Add(FloatToStr(FArmaPhi1)+ ':'+lMessage)
    else
      Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TParamReference.ValidateArmaPhi2(AErrorMessages: TStrings): Boolean;
const OPNAME = 'TParamReference.ValidateArmaPhi2';
var
  lMessage : string;
begin
  Result := False;
  try
    lMessage := '';
    if (not FAppModules.FieldProperties.ValidateFieldProperty('ArmaPhi2', FloatToStr(FArmaPhi2), lMessage)) then
      AErrorMessages.Add(FloatToStr(FArmaPhi2)+ ':'+lMessage)
    else
      Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TParamReference.ValidateArmaTheta1(AErrorMessages: TStrings): Boolean;
const OPNAME = 'TParamReference.ValidateArmaTheta1';
var
  lMessage : string;
begin
  Result := False;
  try
    lMessage := '';
    if (not FAppModules.FieldProperties.ValidateFieldProperty('ArmaTheta1', FloatToStr(FArmaTheta1), lMessage)) then
      AErrorMessages.Add(FloatToStr(FArmaTheta1)+ ':'+lMessage)
    else
      Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TParamReference.ValidateArmaTheta2(AErrorMessages: TStrings): Boolean;
const OPNAME = 'TParamReference.ValidateArmaTheta2';
var
  lMessage : string;
begin
  Result := False;
  try
    lMessage := '';
    if (not FAppModules.FieldProperties.ValidateFieldProperty('ArmaTheta2', FloatToStr(FArmaTheta2), lMessage)) then
      AErrorMessages.Add(FloatToStr(FArmaTheta2)+ ':'+lMessage)
    else
      Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TParamReference.ValidateCatchmentAreaParam(AErrorMessages: TStrings): Boolean;
const OPNAME = 'TParamReference.ValidateCatchmentAreaParam';
var
  lMessage                : string;
  LNodeIndex              : integer;
  LSFRIndex               : integer;
  LIrrigationBlockIndex   : integer;
  LNodesList              : TObjectList;
  LSFRList                : TObjectList;
  LIrrigationBlockList    : TObjectList;
  LNode                   : TReservoirData;
  LIrrigationBlock        : TIrrigationBlock;
  LSFR                    : TStreamFlowReduction;
  LTotalSFRArea           : double;
  LTotalIrrigationArea    : double;
  LIdentifier             : integer;
begin
  Result := False;
  try
    if (FAppModules.StudyArea.ModelVersion = '7') then
    begin
      lMessage := '';
      if (not FAppModules.FieldProperties.ValidateFieldProperty('CatchmentAreaParam', FloatToStr(FCatchmentArea), lMessage)) then
        AErrorMessages.Add('ERROR:' +FloatToStr(FCatchmentArea)+ ':'+lMessage)
      else
      begin
        if(TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.IrrigationBlockList.IrrigationBlockCount > 0) or
          (TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.StreamFlowReductionList.StreamFlowReductionCount > 0) then
        begin
          LTotalSFRArea        := 0.0;
          LTotalIrrigationArea := 0.0;
          LIdentifier          := 0;
          LSFRList := TObjectList.Create(False);
          LNodesList := TObjectList.Create(False);
          LIrrigationBlockList := TObjectList.Create(False);
          try
            if TYieldModelDataObject(FAppModules.Model.ModelData).CastNetworkElementData.CastReservoirList.
               GetReservoirsAndNodesPerCatchmentRef(FCatchReference,LNodesList) then
            begin
              for LNodeIndex := 0 to LNodesList.Count-1 do
              begin
                LNode := TReservoirData(LNodesList[LNodeIndex]);
                LIdentifier := LNode.ReservoirConfigurationData.ReservoirIdentifier;

                if TYieldModelDataObject(FAppModules.Model.ModelData).CastNetworkFeaturesData.CastIrrigationBlockList.
                   GetIrrigationBlocksPerHydrologyNodeNumber(LNode.ReservoirConfigurationData.ReservoirIdentifier,LIrrigationBlockList) then
                begin
                  for LIrrigationBlockIndex := 0 to LIrrigationBlockList.Count-1 do
                  begin
                    LIrrigationBlock     := TIrrigationBlock(LIrrigationBlockList[LIrrigationBlockIndex]);
                    LTotalIrrigationArea := LTotalIrrigationArea + LIrrigationBlock.AllocatedIrrigationArea;
                  end;
                end;

                if TYieldModelDataObject(FAppModules.Model.ModelData).CastNetworkFeaturesData.CastStreamFlowReductionList.
                   GetStreamFlowReductionsPerInflowNodeNumber(LNode.ReservoirConfigurationData.ReservoirIdentifier,LSFRList) then
                begin
                  for LSFRIndex := 0 to LSFRList.Count-1 do
                  begin
                    LSFR     := TStreamFlowReduction(LSFRList[LSFRIndex]);
                    LTotalSFRArea := LTotalSFRArea + LSFR.CoveredArea;
                  end;
                end;
              end;
            end;
          finally
            LSFRList.Free;
            LNodesList.Free;
            LIrrigationBlockList.Free;
          end;
          if ((LTotalSFRArea + LTotalIrrigationArea) > FCatchmentArea) then
          begin
            lMessage := FAppModules.language.GetString('ContextValidation.TotalCatchmentAreaError');
            AErrorMessages.Add('ERROR:' +Format(lMessage, [FloatToStr(FCatchmentArea), IntToStr(LIdentifier) , FloatToStr(LTotalSFRArea + LTotalIrrigationArea)]));
          end;
        end;
        Result := (lMessage = '');
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TParamReference.ValidateGaugeName(AErrorMessages: TStrings): Boolean;
const OPNAME = 'TParamReference.ValidateGaugeName';
var
  lMessage : string;
begin
  Result := False;
  try
    lMessage := '';
    if (not FAppModules.FieldProperties.ValidateFieldProperty('GaugeName', FGaugeName, lMessage)) then
      AErrorMessages.Add(FGaugeName+ ':'+lMessage)
    else
      Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TParamReference.ValidateNumberOfYearsParam(AErrorMessages: TStrings): Boolean;
const OPNAME = 'TParamReference.ValidateNumberOfYearsParam';
var
  lMessage : string;
begin
  Result := False;
  try
    lMessage := '';
    if (not FAppModules.FieldProperties.ValidateFieldProperty('NumberOfYearsParam', IntToStr(FNumberOfYears), lMessage)) then
      AErrorMessages.Add(FloatToStr(FNumberOfYears)+ ':'+lMessage)
    else
      Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TParamReference.ValidateParamAIC(AErrorMessages: TStrings): Boolean;
const OPNAME = 'TParamReference.ValidateParamAIC';
var
  lMessage : string;
begin
  Result := False;
  try
    lMessage := '';
    if (not FAppModules.FieldProperties.ValidateFieldProperty('ParamAIC', FloatToStr(FParamAIC), lMessage)) then
      AErrorMessages.Add(FloatToStr(FParamAIC)+ ':'+lMessage)
    else
      Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TParamReference.ValidateParamANC(AErrorMessages: TStrings): Boolean;
const OPNAME = 'TParamReference.ValidateParamANC';
var
  lMessage : string;
begin
  Result := False;
  try
    lMessage := '';
    if (not FAppModules.FieldProperties.ValidateFieldProperty('ParamANC', FloatToStr(FParamANC), lMessage)) then
      AErrorMessages.Add(FloatToStr(FParamANC)+ ':'+lMessage)
    else
      Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TParamReference.ValidateParamXa(AErrorMessages: TStrings): Boolean;
const OPNAME = 'TParamReference.ValidateParamXa';
var
  lMessage : string;
begin
  Result := False;
  try
    lMessage := '';
    if (not FAppModules.FieldProperties.ValidateFieldProperty('ParamXa', FloatToStr(FParamXa), lMessage)) then
      AErrorMessages.Add(FloatToStr(FParamXa)+ ':'+lMessage)
    else
      Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TParamReference.ValidateParamXSD(AErrorMessages: TStrings): Boolean;
const OPNAME = 'TParamReference.ValidateParamXSD';
var
  lMessage : string;
begin
  Result := False;
  try
    lMessage := '';
    if (not FAppModules.FieldProperties.ValidateFieldProperty('ParamXSD', FloatToStr(FParamXSD), lMessage)) then
      AErrorMessages.Add(FloatToStr(FParamXSD)+ ':'+lMessage)
    else
      Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TParamReference.ValidatePhiZero(AErrorMessages: TStrings): Boolean;
const OPNAME = 'TParamReference.ValidatePhiZero';
var
  lMessage : string;
begin
  Result := False;
  try
    lMessage := '';
    if (not FAppModules.FieldProperties.ValidateFieldProperty('PhiZero', FloatToStr(FPhiZero), lMessage)) then
      AErrorMessages.Add(FloatToStr(FPhiZero)+ ':'+lMessage)
    else
      Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TParamReference.ValidateResidual1(AErrorMessages: TStrings): Boolean;
const OPNAME = 'TParamReference.ValidateResidual1';
var
  lMessage : string;
begin
  Result := False;
  try
    lMessage := '';
    if (not FAppModules.FieldProperties.ValidateFieldProperty('Residual1', FloatToStr(FResidual1), lMessage)) then
      AErrorMessages.Add(FloatToStr(FResidual1)+ ':'+lMessage)
    else
      Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TParamReference.ValidateResidual2(AErrorMessages: TStrings): Boolean;
const OPNAME = 'TParamReference.ValidateResidual2';
var
  lMessage : string;
begin
  Result := False;
  try
    lMessage := '';
    if (not FAppModules.FieldProperties.ValidateFieldProperty('Residual2', FloatToStr(FResidual2), lMessage)) then
      AErrorMessages.Add(FloatToStr(FResidual2)+ ':'+lMessage)
    else
      Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TParamReference.ValidateResidualMean(AErrorMessages: TStrings): Boolean;
const OPNAME = 'TParamReference.ValidateResidualMean';
var
  lMessage : string;
begin
  Result := False;
  try
    lMessage := '';
    if (not FAppModules.FieldProperties.ValidateFieldProperty('ResidualMean', FloatToStr(FResidualMean), lMessage)) then
      AErrorMessages.Add(FloatToStr(FResidualMean)+ ':'+lMessage)
    else
      Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TParamReference.ValidateResidualStdDev(AErrorMessages: TStrings): Boolean;
const OPNAME = 'TParamReference.ValidateResidualStdDev';
var
  lMessage : string;
begin
  Result := False;
  try
    lMessage := '';
    if (not FAppModules.FieldProperties.ValidateFieldProperty('ResidualStdDev', FloatToStr(FResidualStdDev), lMessage)) then
      AErrorMessages.Add(FloatToStr(FResidualStdDev)+ ':'+lMessage)
    else
      Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TParamReference.ValidateStartYearParam(AErrorMessages: TStrings): Boolean;
const OPNAME = 'TParamReference.ValidateStartYearParam';
var
  lMessage : string;
begin
  Result := False;
  try
    lMessage := '';
    if (not FAppModules.FieldProperties.ValidateFieldProperty('StartYearParam', FloatToStr(FStartYear), lMessage)) then
      AErrorMessages.Add(FloatToStr(FStartYear)+ ':'+lMessage)
    else
      Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TParamReference.ValidateTransformDelta(AErrorMessages: TStrings): Boolean;
const OPNAME = 'TParamReference.ValidateTransformDelta';
var
  lMessage : string;
begin
  Result := False;
  try
    lMessage := '';
    if (not FAppModules.FieldProperties.ValidateFieldProperty('TransformDelta', FloatToStr(FTransformDelta), lMessage)) then
      AErrorMessages.Add(FloatToStr(FTransformDelta)+ ':'+lMessage)
    else
      Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TParamReference.ValidateTransformGamma(AErrorMessages: TStrings): Boolean;
const OPNAME = 'TParamReference.ValidateTransformGamma';
var
  lMessage : string;
begin
  Result := False;
  try
    lMessage := '';
    if (not FAppModules.FieldProperties.ValidateFieldProperty('TransformGamma', FloatToStr(FTransformGamma), lMessage)) then
      AErrorMessages.Add(FloatToStr(FTransformGamma)+ ':'+lMessage)
    else
      Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TParamReference.ValidateTransformType(AErrorMessages: TStrings): Boolean;
const OPNAME = 'TParamReference.ValidateTransformType';
var
  lMessage : string;
begin
  Result := False;
  try
    lMessage := '';
    if (not FAppModules.FieldProperties.ValidateFieldProperty('StartYearParam', IntToStr(FTransformType), lMessage)) then
      AErrorMessages.Add(IntToStr(FTransformType)+ ':'+lMessage)
    else
      Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TParamReference.ValidateTransformXi(AErrorMessages: TStrings): Boolean;
const OPNAME = 'TParamReference.ValidateTransformXi';
var
  lMessage : string;
begin
  Result := False;
  try
    lMessage := '';
    if (not FAppModules.FieldProperties.ValidateFieldProperty('TransformXi', FloatToStr(FTransformXi), lMessage)) then
      AErrorMessages.Add(FloatToStr(FTransformXi)+ ':'+lMessage)
    else
      Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TParamReference.ValidateTransformXlam(AErrorMessages: TStrings): Boolean;
const OPNAME = 'TParamReference.ValidateTransformXlam';
var
  lMessage : string;
begin
  Result := False;
  try
    lMessage := '';
    if (not FAppModules.FieldProperties.ValidateFieldProperty('TransformXlam', FloatToStr(FTransformXlam), lMessage)) then
      AErrorMessages.Add(FloatToStr(FTransformXlam)+ ':'+lMessage)
    else
      Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TParamReference.ValidateVariate1(AErrorMessages: TStrings): Boolean;
const OPNAME = 'TParamReference.ValidateVariate1';
var
  lMessage : string;
begin
  Result := False;
  try
    lMessage := '';
    if (not FAppModules.FieldProperties.ValidateFieldProperty('Variate1', FloatToStr(FVariate1), lMessage)) then
      AErrorMessages.Add(FloatToStr(FVariate1)+ ':'+lMessage)
    else
      Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TParamReference.ValidateVariate2(AErrorMessages: TStrings): Boolean;
const OPNAME = 'TParamReference.ValidateVariate2';
var
  lMessage : string;
begin
  Result := False;
  try
    lMessage := '';
    if (not FAppModules.FieldProperties.ValidateFieldProperty('Variate2', FloatToStr(FVariate2), lMessage)) then
      AErrorMessages.Add(FloatToStr(FVariate2)+ ':'+lMessage)
    else
      Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TParamReference.ValidateZTVariates(AErrorMessages: TStrings): Boolean;
const OPNAME = 'TParamReference.ValidateZTVariates';
var
  lMessage : string;
begin
  Result := False;
  try
    lMessage := '';
    if (not FAppModules.FieldProperties.ValidateFieldProperty('ZTVariates', FloatToStr(FZTVariates), lMessage)) then
      AErrorMessages.Add(FloatToStr(FZTVariates)+ ':'+lMessage)
    else
      Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

{ TParamSetup }

procedure TParamSetup.CreateMemberObjects;
const OPNAME = 'TParamSetup.CreateMemberObjects';
begin
  inherited;
  try
    FReferenceData := TObjectList.Create(True);
    FValidReferences := TStringList.Create;
    FMatrixB   := nil;
    FMatrixB0  := nil;
    FMatrixB1  := nil;
    FMatrixA   := nil;
    FMatrixC   := nil;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TParamSetup.DestroyMemberObjects;
const OPNAME = 'TParamSetup.DestroyMemberObjects';
begin
  inherited;
  try
    FreeAndNil(FReferenceData);
    FreeAndNil(FValidReferences);
    Finalize(FMatrixB);
    Finalize(FMatrixB0);
    Finalize(FMatrixB1);
    Finalize(FMatrixA);
    Finalize(FMatrixC);
    FMatrixB   := nil;
    FMatrixB0  := nil;
    FMatrixB1  := nil;
    FMatrixA   := nil;
    FMatrixC   := nil;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TParamSetup.Initialise: boolean;
const OPNAME = 'TParamSetup.Initialise';
//var
//  LParamReference: TParamReference;
begin
  Result := inherited Initialise;
  try
    Finalize(FMatrixB);
    Finalize(FMatrixB0);
    Finalize(FMatrixB1);
    Finalize(FMatrixA);
    Finalize(FMatrixC);
    FMatrixB   := nil;
    FMatrixB0  := nil;
    FMatrixB1  := nil;
    FMatrixA   := nil;
    FMatrixC   := nil;
    FReferenceData.Clear;
    FValidReferences.Clear;
    {LParamReference := TParamReference.Create(FAppModules);
    LParamReference.Initialise;
    FReferenceData.Add(LParamReference);
    LParamReference.FCatchReference := 0;
    LParamReference.FGaugeName  := 'UnAssigned';
    LParamReference.FCatchmentArea  := 0.0;}
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TParamSetup.GetAllReferenceData: TObjectList;
const OPNAME = 'TParamSetup.GetAllReferenceData';
begin
  Result := nil;
  try
    Result := FReferenceData;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TParamSetup.GetValidReferences : TStringList;
const OPNAME = 'TParamSetup.GetValidReferences';
begin
  Result := nil;
  try
    Result := FValidReferences;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TParamSetup.ReferenceNumberValid (ANumber : integer) : wordbool;
const OPNAME = 'TParamSetup.ReferenceNumberValid';
begin
  Result := FALSE;
  try
    Result := (FValidReferences.IndexOf(IntToStr(ANumber)) >= 0);
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TParamSetup.LoadReferenceDataFromDataset(ADataSet: TAbstractModelDataset): boolean;
const OPNAME = 'TParamSetup.LoadReferenceDataFromDataset';
var
  LParamReference: TParamReference;
begin
  Result := False;
  try
    FReferenceData.Clear;
    while not ADataSet.DataSet.Eof do
    begin
      LParamReference := TParamReference.Create(FAppModules);
      LParamReference.Initialise;
      LParamReference.PopulateFromDataset(ADataSet);
      FReferenceData.Add(LParamReference);
      ADataSet.DataSet.Next;
    end;
    Result := True;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TParamSetup.LoadKeyGaugeDataFromDataset (ADataSet : TAbstractModelDataset): boolean;
const OPNAME = 'TParamSetup.LoadKeyGaugeDataFromDataset';
begin
  Result := False;
  try
    FValidReferences.Clear;
    if (not ADataSet.DataSet.Eof) then
    begin
      FValidReferences.Clear;
      FValidReferences.CommaText := Trim(ADataSet.DataSet.FieldByName('KeyGauges').AsString);
    end;
    Result := True;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TParamSetup.Get_ReferenceDataByCatchNumber(ARefNumber: Integer): IParamReference;
const OPNAME = 'TParamSetup.Get_ReferenceDataByCatchNumber';
var
  LIndex: integer;
begin
  Result := nil;
  try
    if(ARefNumber > 0) then
    begin
      for LIndex := 0 to FReferenceData.Count -1 do
      begin
        if(TParamReference(FReferenceData.Items[LIndex]).CatchReference = ARefNumber) then
        begin
          Result := TParamReference(FReferenceData.Items[LIndex]);
          Break;
        end;
      end;
    end;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TParamSetup.Get_ReferenceCount: integer;
const OPNAME = 'TParamSetup.Get_ReferenceCount';
begin
  Result := 0;
  try
    Result := FReferenceData.Count;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TParamSetup.Get_ReferenceDataByIndex(AIndex: integer): IParamReference;
const OPNAME = 'TParamSetup.Get_ReferenceDataByIndex';
begin
  Result := nil;
  try
    if(AIndex >= 0) and (AIndex < FReferenceData.Count) then
      Result := TParamReference(FReferenceData.Items[AIndex]);
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TParamSetup.Validate(var AErrors: WideString; const AContext: WideString): WordBool;
const OPNAME = 'TParamSetup.Validate';
var
  LIndex: integer;
begin
  Result := True;
  try
    for LIndex := 0 to FReferenceData.Count -1 do
    begin
      if not (TParamReference(FReferenceData.Items[LIndex]).Validate(AErrors,AContext)) then
        Result := False;
    end;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TParamSetup.GetReferenceDataByCatchNumberCast(ACatchReferenceNumber: integer): TParamReference;
const OPNAME = 'TParamSetup.GetReferenceDataByCatchNumberCast';
var
  LIndex: integer;
begin
  Result := nil;
  try
    if(ACatchReferenceNumber > 0) then
    begin
      for LIndex := 0 to FReferenceData.Count -1 do
      begin
        if(TParamReference(FReferenceData.Items[LIndex]).CatchReference = ACatchReferenceNumber) then
        begin
          Result := TParamReference(FReferenceData.Items[LIndex]);
          Break;
        end;
      end;
    end;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TParamSetup.GetReferenceDataByIndexCast(AIndex: integer): TParamReference;
const OPNAME = 'TParamSetup.GetReferenceDataByIndexCast';
begin
  Result := nil;
  try
    if(AIndex >= 0) and (AIndex < FReferenceData.Count) then
      Result := TParamReference(FReferenceData.Items[AIndex]);
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TParamSetup._AddRef: Integer;
const OPNAME = 'TParamSetup._AddRef';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TParamSetup._Release: Integer;
const OPNAME = 'TParamSetup._Release';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TParamSetup.Get_KeyGaugeCount: Integer;
const OPNAME = 'TParamSetup.Get_KeyGaugeCount';
begin
  Result := 0;
  try
    Result := FValidReferences.Count;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TParamSetup.Get_KeyGaugeNoByIndex(AIndex: Integer): Integer;
const OPNAME = 'TParamSetup.Get_KeyGaugeNoByIndex';
begin
  Result := 0;
  try
    if (AIndex >= 0) AND (AIndex < FValidReferences.Count) then
      Result := StrToInt(FValidReferences.Strings[AIndex]);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TParamSetup.Get_MatrixA(ARow, ACol: Integer): Double;
const OPNAME = 'TParamSetup.Get_MatrixA';
begin
  Result := NullFloat;
  try
    if(ACol >= 1) and (ACol <= 10) and (ARow >= 1) and (ARow <= High(FMatrixA[0])) then
      Result := FMatrixA[ARow,ACol];
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TParamSetup.Get_MatrixB(ARow, ACol: Integer): Double;
const OPNAME = 'TParamSetup.Get_MatrixB';
begin
  Result := NullFloat;
  try
    if(ACol >= 1) and (ACol <= 10) and (ARow >= 1) and (ARow <= High(FMatrixB[0])) then
      Result := FMatrixB[ARow,ACol];
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TParamSetup.Get_MatrixB0(ARow, ACol: Integer): Double;
const OPNAME = 'TParamSetup.Get_MatrixB0';
begin
  Result := NullFloat;
  try
    if(ACol >= 1) and (ACol <= 10) and (ARow >= 1) and (ARow <= High(FMatrixB0[0])) then
      Result := FMatrixB0[ARow,ACol];
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TParamSetup.Get_MatrixB1(ARow, ACol: Integer): Double;
const OPNAME = 'TParamSetup.Get_MatrixB1';
begin
  Result := NullFloat;
  try
    if(ACol >= 1) and (ACol <= 10) and (ARow >= 1) and (ARow <= High(FMatrixB1[0])) then
      Result := FMatrixB1[ARow,ACol];
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TParamSetup.Get_MatrixC(ARow, ACol: Integer): Double;
const OPNAME = 'TParamSetup.Get_MatrixC';
begin
  Result := NullFloat;
  try
    if(ACol >= 1) and (ACol <= 10) and (ARow >= 1) and (ARow <= High(FMatrixC[0])) then
      Result := FMatrixC[ARow,ACol];
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TParamSetup.LoadMatrixADataFromDataset(ADataSet: TAbstractModelDataset): boolean;
const OPNAME = 'TParamSetup.LoadMatrixADataFromDataset';
var
  LCol,
  LRow: integer;
  LFieldName  : string;
begin
  Result := False;
  try
    Finalize(FMatrixA);
    if(ADataSet.DataSet.RecordCount > 0) then
    begin
      SetLength(FMatrixA,ADataSet.DataSet.RecordCount+1,11);
      ADataSet.DataSet.First;
      for LRow := 1 to ADataSet.DataSet.RecordCount do
      begin
        for LCol := 1 to 10 do
        begin
          LFieldName := Format('Matrix%2.2d', [LCol]);
          FMatrixA[LRow,LCol] := ADataSet.DataSet.FieldByName(LFieldName).AsFloat;
        end;
        ADataSet.DataSet.Next;
      end;
    end;
    Result := True;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TParamSetup.LoadMatrixB0DataFromDataset(ADataSet: TAbstractModelDataset): boolean;
const OPNAME = 'TParamSetup.LoadMatrixB0DataFromDataset';
var
  LCol,
  LRow: integer;
  LFieldName  : string;
begin
  Result := False;
  try
    Finalize(FMatrixB0);
    if(ADataSet.DataSet.RecordCount > 0) then
    begin
      SetLength(FMatrixB0,ADataSet.DataSet.RecordCount+1,11);
      ADataSet.DataSet.First;
      for LRow := 1 to ADataSet.DataSet.RecordCount do
      begin
        for LCol := 1 to 10 do
        begin
          LFieldName := Format('Matrix%2.2d', [LCol]);
          FMatrixB0[LRow,LCol] := ADataSet.DataSet.FieldByName(LFieldName).AsFloat;
        end;
        ADataSet.DataSet.Next;
      end;
    end;
    Result := True;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TParamSetup.LoadMatrixB1DataFromDataset(ADataSet: TAbstractModelDataset): boolean;
const OPNAME = 'TParamSetup.LoadMatrixB1DataFromDataset';
var
  LCol,
  LRow: integer;
  LFieldName  : string;
begin
  Result := False;
  try
    Finalize(FMatrixB1);
    if(ADataSet.DataSet.RecordCount > 0) then
    begin
      SetLength(FMatrixB1,ADataSet.DataSet.RecordCount+1,11);
      ADataSet.DataSet.First;
      for LRow := 1 to ADataSet.DataSet.RecordCount do
      begin
        for LCol := 1 to 10 do
        begin
          LFieldName := Format('Matrix%2.2d', [LCol]);
          FMatrixB1[LRow,LCol] := ADataSet.DataSet.FieldByName(LFieldName).AsFloat;
        end;
        ADataSet.DataSet.Next;
      end;
    end;
    Result := True;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TParamSetup.LoadMatrixBDataFromDataset(ADataSet: TAbstractModelDataset): boolean;
const OPNAME = 'TParamSetup.LoadMatrixBDataFromDataset';
var
  LCol,
  LRow: integer;
  LFieldName  : string;
begin
  Result := False;
  try
    Finalize(FMatrixB);
    if(ADataSet.DataSet.RecordCount > 0) then
    begin
      SetLength(FMatrixB,ADataSet.DataSet.RecordCount+1,11);
      ADataSet.DataSet.First;
      for LRow := 1 to ADataSet.DataSet.RecordCount do
      begin
        for LCol := 1 to 10 do
        begin
          LFieldName := Format('Matrix%2.2d', [LCol]);
          FMatrixB[LRow,LCol] := ADataSet.DataSet.FieldByName(LFieldName).AsFloat;
        end;
        ADataSet.DataSet.Next;
      end;
    end;
    Result := True;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TParamSetup.LoadMatrixCDataFromDataset(ADataSet: TAbstractModelDataset): boolean;
const OPNAME = 'TParamSetup.LoadMatrixCDataFromDataset';
var
  LCol,
  LRow: integer;
  LFieldName  : string;
begin
  Result := False;
  try
    Finalize(FMatrixC);
    if(ADataSet.DataSet.RecordCount > 0) then
    begin
      SetLength(FMatrixC,ADataSet.DataSet.RecordCount+1,11);
      ADataSet.DataSet.First;
      for LRow := 1 to ADataSet.DataSet.RecordCount do
      begin
        for LCol := 1 to 10 do
        begin
          LFieldName := Format('Matrix%2.2d', [LCol]);
          FMatrixC[LRow,LCol] := ADataSet.DataSet.FieldByName(LFieldName).AsFloat;
        end;
        ADataSet.DataSet.Next;
      end;
    end;
    Result := True;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

end.
