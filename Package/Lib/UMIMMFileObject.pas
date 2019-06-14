//
//
//  UNIT      : Contains TMinesFileObject Class
//  AUTHOR    : Sam Dlamini (BCX)
//  DATE      : 19/10/2016
//  COPYRIGHT : Copyright © 2017 DWS
//
//
unit UMIMMFileObject;

interface

uses
  Classes, sysutils,contnrs,

  //  DWAF VCL
  UMineFileObject,
  UAbstractDataObject,
  UBasicObjects,
  UConstants;

type
  TGrowthFactors = class;

  TLoadGeneration = class(TAbstractDataObject)
  protected
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
  public
    FStdDeviation          : TDouble;
    FLoadGenType           : TInteger;
    FFlow                  : array[1..10] of TDouble;
    FMeanOfSalt            : array[1..10] of TDouble;
    procedure Reset;override;
    function Initialise: boolean;override;
  end;

  TMIMMOpenCastFileObject = class(TMineOpenCastFileObject)
  protected

    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
  public
    FAbstractionOpt                      : TChar;
    FWorkingCommYear                     : TInteger;
    FWorkingCommMonth                    : TInteger;
    FWorkingDecommYear                   : TInteger;
    FWorkingDecommMonth                  : TInteger;
    FRunoffSaltsWashoffEfficiencyFactor  : TDouble;
    FIniSaltStore                        : TDouble;
    FReChargeRate                        : TDouble;
    //Absraction values
    FAbstractToEvap                      : TDouble;
    FAbstrctToRiver                      : TDouble;
    FAbstrctToCPD                        : TDouble;
    FAbstrctMonthTimeSeriesFile          : TString;
    FIniConcentration                    : TDouble;
    FGrowthFactors : TObjectList;
    FLoadGeneration : TObjectList;

    procedure Reset;override;
    function Initialise: boolean;override;
    function AddGrowthFactors : TGrowthFactors;
    function AddLoadGeneration : TLoadGeneration;
  end;

  TMIMMUndegroundFileObject = class(TMineUndegroundFileObject)
  protected

    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
  public
    FGrowthFactors : TObjectList;
    FLoadGeneration : TObjectList;
    procedure Reset;override;
    function Initialise: boolean;override;
    function AddGrowthFactors : TGrowthFactors;
    function AddLoadGeneration : TLoadGeneration;
  end;

  TMIMMSlurryDumpFileObject = class(TMineSlurryDumpFileObject)
  protected

    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
  public
    FSaltConcentration        : TDouble;
    FGrowthFactors    : TObjectList;
    FLoadGeneration   : TObjectList;
    procedure Reset;override;
    function Initialise: boolean;override;
    function AddGrowthFactors : TGrowthFactors;
    function AddLoadGeneration : TLoadGeneration;
  end;

  TMIMMSubCatchmentFileObject = class(TMineSubCatchmentFileObject)
  protected

    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
  public
    procedure Reset;override;
    function Initialise: boolean;override;


  end;

  TMIMMSubCatchmentListFileObject  = class(TMineSubCatchmentListFileObject)
  protected

    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;

  public
    procedure Reset;override;
    function MineSubCatchmentObjectCount: integer;override;
    function Initialise: boolean;override;
    function AddMineSubCatchmentFileObject:TMIMMSubCatchmentFileObject;


  end;

  TGrowthFactors = class(TAbstractDataObject)
  protected
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
  public
    FGrowthCode            : TInteger;
    FNoOfPoints            : TInteger;
    FInterpolationMethod   : TInteger;
    FYearDatapoints        : TString;
    FGrowthFactors         : TString;
    FOwner                 : TString;
    FMineType              : TString;
    procedure Reset;override;
    function Initialise: boolean;override;
  end;

  TMIMMFileObject = class(TMineFileObject)
  protected
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
  public

    FAssSaltsWashoffNo : TInteger;
    FRainfallFileName  : TString;
    FRainfallMAP       : TDouble;
    FSaltsBuildUpRate  : TDouble;
    FSaltsWashoffEfficiencyFactor : TDouble;
    FIniSaltStore                 : TDouble;
    FMIMMGrowthFactorList         : TObjectList;
    procedure Reset;override;
    function Initialise: boolean;override;
    function OpenCastCount: integer; override;
    function AddOpenCast:TMIMMOpenCastFileObject;
    function UndegroundCount: integer;override;
    function AddUndeground:TMIMMUndegroundFileObject;
    function SlurryDumpCount: integer;override;
    function AddSlurryDump:TMIMMSlurryDumpFileObject;
    function AddMIMMGrowthFactors : TGrowthFactors;
  end;

  TMIMMListFileObject  = class(TMineListFileObject)
  protected
    FMIMMFileName : TString;
    FFileNumber   : integer;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
  public
    constructor Create(AFileNumber: integer); reintroduce;
    procedure Reset;override;
    function MineFileObjectCount: integer; override;
    function Initialise: boolean;override;
    function AddMineFileObject:TMIMMFileObject;

    property FileNumber   : integer   read FFileNumber;
    property MIMMFileName : TString   read FMIMMFileName;


  end;

implementation


uses UErrorHandlingOperations;

{ TMIMMOpenCastFileObject }

function TMIMMOpenCastFileObject.AddGrowthFactors: TGrowthFactors;
const OPNAME = 'TMIMMOpenCastFileObject.AddGrowthFactors';
begin
  Result := nil;
  try
    Result := TGrowthFactors.Create;
    FGrowthFactors.Add(Result);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMIMMOpenCastFileObject.AddLoadGeneration: TLoadGeneration;
const OPNAME = 'TMIMMOpenCastFileObject.AddLoadGeneration';
begin
  Result := nil;
  try
    Result := TLoadGeneration.Create;
    FLoadGeneration.Add(Result);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMIMMOpenCastFileObject.CreateMemberObjects;
const OPNAME = 'TMIMMOpenCastFileObject.CreateMemberObjects';
begin

  try
    FAbstractionOpt                      := TChar.Create;
    FWorkingCommYear                     := TInteger.Create;
    FWorkingCommMonth                    := TInteger.Create;
    FWorkingDecommYear                   := TInteger.Create;
    FWorkingDecommMonth                  := TInteger.Create;
    FRunoffSaltsWashoffEfficiencyFactor  := TDouble.Create;
    FIniSaltStore                        := TDouble.Create;
    FReChargeRate                        := TDouble.Create;

    FAbstractToEvap                      := TDouble.Create;
    FAbstrctToRiver                      := TDouble.Create;
    FAbstrctToCPD                        := TDouble.Create;
    FAbstrctMonthTimeSeriesFile          := TString.Create;

    FIniConcentration                    := TDouble.Create;

    FGrowthFactors := TObjectList.Create;
    FLoadGeneration := TObjectList.Create;


    inherited CreateMemberObjects;
    Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMIMMOpenCastFileObject.DestroyMemberObjects;
const OPNAME = 'TMIMMOpenCastFileObject.DestroyMemberObjects';
begin
  inherited DestroyMemberObjects;
  try
    FAbstractionOpt.Free;
    FWorkingCommYear.Free;
    FWorkingCommMonth.Free;
    FWorkingDecommYear.Free;
    FWorkingDecommMonth.Free;
    FRunoffSaltsWashoffEfficiencyFactor.Free;
    FIniSaltStore.Free;
    FReChargeRate.Free;

    FAbstractToEvap.Free;
    FAbstrctToRiver.Free;
    FAbstrctToCPD.Free;
    FAbstrctMonthTimeSeriesFile.Free;
    FIniConcentration.Free;
    FGrowthFactors.Clear;
    FGrowthFactors.Free;
    FLoadGeneration.Clear;
    FLoadGeneration.Free

  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMIMMOpenCastFileObject.Reset;
const OPNAME = 'TMIMMOpenCastFileObject.Reset';
begin
  try
     Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMIMMOpenCastFileObject.Initialise: boolean;
const OPNAME = 'TMIMMOpenCastFileObject.Initialise';
begin
  Result := False;
  try
    FAbstractionOpt.FData := ' ';
    FAbstractionOpt.FInitalised := False;
    FAbstractionOpt.FLength := 4;
    FAbstractionOpt.FDecimal := 0;
    FAbstractionOpt.FDefaultPadding := True;


    FWorkingCommYear.FData := 0;
    FWorkingCommYear.FInitalised := False;
    FWorkingCommYear.FLength := 4;
    FWorkingCommYear.FDecimal := 0;
    FWorkingCommYear.FDefaultPadding := True;

    FWorkingCommMonth.FData := 0;
    FWorkingCommMonth.FInitalised := False;
    FWorkingCommMonth.FLength := 4;
    FWorkingCommMonth.FDecimal := 0;
    FWorkingCommMonth.FDefaultPadding := True;

    FWorkingDecommYear.FData := 0;
    FWorkingDecommYear.FInitalised := False;
    FWorkingDecommYear.FLength := 4;
    FWorkingDecommYear.FDecimal := 0;
    FWorkingDecommYear.FDefaultPadding := True;

    FWorkingDecommMonth.FData := 0;
    FWorkingDecommMonth.FInitalised := False;
    FWorkingDecommMonth.FLength := 4;
    FWorkingDecommMonth.FDecimal := 0;
    FWorkingDecommMonth.FDefaultPadding := True;

    FRunoffSaltsWashoffEfficiencyFactor.FData := 0.000;
    FRunoffSaltsWashoffEfficiencyFactor.FInitalised := False;
    FRunoffSaltsWashoffEfficiencyFactor.FLength := 10;
    FRunoffSaltsWashoffEfficiencyFactor.FDecimal := 3;
    FRunoffSaltsWashoffEfficiencyFactor.FDefaultPadding := True;
    FRunoffSaltsWashoffEfficiencyFactor.ShowDecimalPoint := True;

    FIniSaltStore.FData := 0.00;
    FIniSaltStore.FInitalised := False;
    FIniSaltStore.FLength := 10;
    FIniSaltStore.FDecimal := 2;
    FIniSaltStore.FDefaultPadding := True;
    FIniSaltStore.ShowDecimalPoint := True;

    FReChargeRate.FData := 0.000;
    FReChargeRate.FInitalised := False;
    FReChargeRate.FLength := 10;
    FReChargeRate.FDecimal := 3;
    FReChargeRate.FDefaultPadding := True;
    FReChargeRate.ShowDecimalPoint := True;

    FAbstractToEvap.FData := 0.00;
    FAbstractToEvap.FInitalised := False;
    FAbstractToEvap.FLength := 10;
    FAbstractToEvap.FDecimal := 2;
    FAbstractToEvap.FDefaultPadding := True;
    FAbstractToEvap.ShowDecimalPoint := True;

    FAbstrctToRiver.FData := 0.00;
    FAbstrctToRiver.FInitalised := False;
    FAbstrctToRiver.FLength := 10;
    FAbstrctToRiver.FDecimal := 2;
    FAbstrctToRiver.FDefaultPadding := True;
    FAbstrctToRiver.ShowDecimalPoint := True;

    FAbstrctToCPD.FData := 0.00;
    FAbstrctToCPD.FInitalised := False;
    FAbstrctToCPD.FLength := 10;
    FAbstrctToCPD.FDecimal := 2;
    FAbstrctToCPD.FDefaultPadding := True;
    FAbstrctToCPD.ShowDecimalPoint := True;

    FAbstrctMonthTimeSeriesFile.FData := '';
    FAbstrctMonthTimeSeriesFile.FInitalised := False;
    FAbstrctMonthTimeSeriesFile.FLength := 255;
    FAbstrctMonthTimeSeriesFile.FDecimal := 0;
    FAbstrctMonthTimeSeriesFile.FDefaultPadding := True;

    FIniConcentration.FData := 0.00;
    FIniConcentration.FInitalised := False;
    FIniConcentration.FLength := 10;
    FIniConcentration.FDecimal := 2;
    FIniConcentration.FDefaultPadding := True;
    FIniConcentration.ShowDecimalPoint := True;

    FGrowthFactors.Clear;
    FLoadGeneration.Clear;

    Result := inherited Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{ TMIMMUndegroundFileObject }

function TMIMMUndegroundFileObject.AddGrowthFactors: TGrowthFactors;
const OPNAME = 'TMIMMUndegroundFileObject.AddGrowthFactors';
begin
  Result := nil;
  try
    Result := TGrowthFactors.Create;
    FGrowthFactors.Add(Result);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMIMMUndegroundFileObject.AddLoadGeneration: TLoadGeneration;
const OPNAME = 'TMIMMUndegroundFileObject.AddLoadGeneration';
begin
  Result := nil;
  try
    Result := TLoadGeneration.Create;
    FLoadGeneration.Add(Result);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMIMMUndegroundFileObject.CreateMemberObjects;
const OPNAME = 'TMIMMUndegroundFileObject.CreateMemberObjects';
begin
  inherited CreateMemberObjects;
  try
    FGrowthFactors := TObjectList.Create;
    FLoadGeneration := TObjectList.Create;
    Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMIMMUndegroundFileObject.DestroyMemberObjects;
const OPNAME = 'TMIMMUndegroundFileObject.DestroyMemberObjects';
begin
  inherited DestroyMemberObjects;
  try


  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMIMMUndegroundFileObject.Reset;
const OPNAME = 'TMIMMUndegroundFileObject.Reset';
begin
  try
     Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMIMMUndegroundFileObject.Initialise: boolean;
const OPNAME = 'TMIMMUndegroundFileObject.Initialise';
begin
  Result := False;
  try

    Result := inherited Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{ TMIMMSlurryDumpFileObject }

function TMIMMSlurryDumpFileObject.AddGrowthFactors: TGrowthFactors;
const OPNAME = 'TMIMMSlurryDumpFileObject.AddGrowthFactors';
begin
  Result := nil;
  try
    Result := TGrowthFactors.Create;
    FGrowthFactors.Add(Result);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMIMMSlurryDumpFileObject.AddLoadGeneration: TLoadGeneration;
const OPNAME = 'TMIMMUndegroundFileObject.AddLoadGeneration';
begin
  Result := nil;
  try
    Result := TLoadGeneration.Create;
    FLoadGeneration.Add(Result);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMIMMSlurryDumpFileObject.CreateMemberObjects;
const OPNAME = 'TMIMMSlurryDumpFileObject.CreateMemberObjects';
begin
  try

    FSaltConcentration        := TDouble.Create;
    FGrowthFactors            := TObjectList.Create;
    FLoadGeneration           := TObjectList.Create;

    inherited CreateMemberObjects;
    Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMIMMSlurryDumpFileObject.DestroyMemberObjects;
const OPNAME = 'TMIMMSlurryDumpFileObject.DestroyMemberObjects';
begin
  inherited DestroyMemberObjects;
  try
    FSaltConcentration.Free;
    FGrowthFactors.Clear;
    FGrowthFactors.Free;
    FLoadGeneration.Clear;
    FLoadGeneration.Free;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMIMMSlurryDumpFileObject.Reset;
const OPNAME = 'TMIMMSlurryDumpFileObject.Reset';
begin
  try
     Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMIMMSlurryDumpFileObject.Initialise: boolean;
const OPNAME = 'TMIMMSlurryDumpFileObject.Initialise';
begin
  Result := False;
  try
    FSaltConcentration.FData := 0.0;
    FSaltConcentration.FInitalised := False;
    FSaltConcentration.FLength := 10;
    FSaltConcentration.FDecimal := 2;
    FSaltConcentration.FDefaultPadding := True;
    FSaltConcentration.ShowDecimalPoint := True;

    FGrowthFactors.Clear;
    FLoadGeneration.Clear;
    Result := inherited Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{TMIMMFileObject}

procedure TMIMMFileObject.CreateMemberObjects;
const OPNAME = 'TMIMMFileObject.CreateMemberObjects';
Begin
  try

    FAssSaltsWashoffNo := TInteger.Create;
    FRainfallFileName  := TString.Create;
    FRainfallMAP       := TDouble.Create;
    FSaltsBuildUpRate  := TDouble.Create;
    FSaltsWashoffEfficiencyFactor := TDouble.Create;
    FIniSaltStore                 := TDouble.Create;
    FMIMMGrowthFactorList         := TObjectList.Create;
    inherited CreateMemberObjects;
    Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMIMMFileObject.DestroyMemberObjects;
const OPNAME = 'TMIMMFileObject.DestroyMemberObjects';
begin
  try
    FRainfallFileName.Free;
    FAssSaltsWashoffNo.Free;
    FRainfallMAP.Free;
    FSaltsBuildUpRate.Free;
    FSaltsWashoffEfficiencyFactor.Free;
    FIniSaltStore.Free;
    FMIMMGrowthFactorList.Free;
    inherited DestroyMemberObjects;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMIMMFileObject.Reset;
const OPNAME = 'TMIMMFileObject.Reset';
begin
  try
     Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMIMMFileObject.Initialise: boolean;
const OPNAME = 'TMIMMFileObject.Initialise';
begin
  Result := False;
  try
    FAssSaltsWashoffNo.FData := 0;
    FAssSaltsWashoffNo.FInitalised := False;
    FAssSaltsWashoffNo.FLength := 4;
    FAssSaltsWashoffNo.FDecimal := 0;
    FAssSaltsWashoffNo.FDefaultPadding := True;

    FRainfallFileName.FData := '';
    FRainfallFileName.FInitalised := False;
    FRainfallFileName.FLength := 255;
    FRainfallFileName.FDecimal := 0;
    FRainfallFileName.FDefaultPadding := True;

    FRainfallMAP.FData := 0.00;
    FRainfallMAP.FInitalised := False;
    FRainfallMAP.FLength := 10;
    FRainfallMAP.FDecimal := 2;
    FRainfallMAP.FDefaultPadding := True;
    FRainfallMAP.ShowDecimalPoint := True;

    FSaltsBuildUpRate.FData := 0.00;
    FSaltsBuildUpRate.FInitalised := False;
    FSaltsBuildUpRate.FLength := 10;
    FSaltsBuildUpRate.FDecimal := 2;
    FSaltsBuildUpRate.FDefaultPadding := True;
    FSaltsBuildUpRate.ShowDecimalPoint := True;


    FSaltsWashoffEfficiencyFactor.FData := 0.00;
    FSaltsWashoffEfficiencyFactor.FInitalised := False;
    FSaltsWashoffEfficiencyFactor.FLength := 10;
    FSaltsWashoffEfficiencyFactor.FDecimal := 2;
    FSaltsWashoffEfficiencyFactor.FDefaultPadding := True;
    FSaltsWashoffEfficiencyFactor.ShowDecimalPoint := True;

    FIniSaltStore.FData := 0.00;
    FIniSaltStore.FInitalised := False;
    FIniSaltStore.FLength := 10;
    FIniSaltStore.FDecimal := 2;
    FIniSaltStore.FDefaultPadding := True;
    FIniSaltStore.ShowDecimalPoint := True;

    Result := inherited Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMIMMFileObject.AddMIMMGrowthFactors: TGrowthFactors;
const OPNAME = 'TMIMMFileObject.AddOpenCast';
begin
  Result := nil;
  try
    Result := TGrowthFactors.Create;
    FMIMMGrowthFactorList.Add(Result);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMIMMFileObject.AddOpenCast: TMIMMOpenCastFileObject;
const OPNAME = 'TMIMMFileObject.AddOpenCast';
begin
  Result := nil;
  try
    Result := TMIMMOpenCastFileObject.Create;
    FOpenCastObjectContainer.Add(Result);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMIMMFileObject.AddUndeground: TMIMMUndegroundFileObject;
const OPNAME = 'TMIMMFileObject.AddUndeground';
begin
  Result := nil;
  try
    Result := TMIMMUndegroundFileObject.Create;
    FUnderGroundObjectContainer.Add(Result);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMIMMFileObject.AddSlurryDump: TMIMMSlurryDumpFileObject;
const OPNAME = 'TMIMMFileObject.AddSlurryDump';
begin
  Result := nil;
  try
    Result := TMIMMSlurryDumpFileObject.Create;
    FSlurryDumpObjectContainer.Add(Result);
  except on E: Exception do HandleError(E, OPNAME) end;
end;


function TMIMMFileObject.OpenCastCount: integer;
const OPNAME = 'TMIMMFileObject.OpenCastCount';
begin
  Result := 0;
  try
    Result := FOpenCastObjectContainer.Count;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMIMMFileObject.UndegroundCount: integer;
const OPNAME = 'TMIMMFileObject.UndegroundCount';
begin
  Result := 0;
  try
    Result := FUnderGroundObjectContainer.Count;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMIMMFileObject.SlurryDumpCount: integer;
const OPNAME = 'TMIMMFileObject.SlurryDumpCount';
begin
  Result := 0;
  try
    Result := FSlurryDumpObjectContainer.Count;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{ TMIMMListFileObject }


constructor TMIMMListFileObject.Create(AFileNumber: integer);
const OPNAME = 'TMIMMListFileObject.Create';
begin
  try
    FFileNumber := AFileNumber;
    inherited Create;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMIMMListFileObject.CreateMemberObjects;
const OPNAME = 'TMIMMListFileObject.CreateMemberObjects';
begin
  try
    FMIMMFileName := TString.Create;

    inherited CreateMemberObjects;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMIMMListFileObject.DestroyMemberObjects;
const OPNAME = 'TMIMMListFileObject.DestroyMemberObjects';
begin
  try
    FMIMMFileName.Free;
    inherited DestroyMemberObjects;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMIMMListFileObject.AddMineFileObject: TMIMMFileObject;
const OPNAME = 'TMIMMListFileObject.AddMineFileObject';
begin
  Result := nil;
  try
    Result := TMIMMFileObject.Create;
    FMineFileObjectContainer.Add(Result);
  except on E: Exception do HandleError(E, OPNAME) end;
end;


function TMIMMListFileObject.Initialise: boolean;
const OPNAME = 'TMIMMListFileObject.Initialise';
begin
  Result := False;
  try
    FMIMMFileName.FData := '';
    FMIMMFileName.FInitalised := False;
    FMIMMFileName.FLength := 255;
    FMIMMFileName.FDecimal := 0;
    FMIMMFileName.FDefaultPadding := True;

    Result := inherited Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMIMMListFileObject.Reset;
const OPNAME = 'TMIMMListFileObject.Reset';
begin
  try
    Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMIMMListFileObject.MineFileObjectCount: integer;
const OPNAME = 'TMIMMListFileObject.MineFileObjectCount';
begin
  Result := 0;
  try
    Result := FMineFileObjectContainer.Count;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{ TMIMMSubCatchmentFileObject }

procedure TMIMMSubCatchmentFileObject.CreateMemberObjects;
const OPNAME = 'TMIMMSubCatchmentFileObject.CreateMemberObjects';
Begin
  try
    inherited CreateMemberObjects;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMIMMSubCatchmentFileObject.DestroyMemberObjects;
const OPNAME = 'TMIMMSubCatchmentFileObject.DestroyMemberObjects';
begin
  try
    inherited DestroyMemberObjects;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMIMMSubCatchmentFileObject.Initialise: boolean;
const OPNAME = 'TMIMMSubCatchmentFileObject.Initialise';
begin
  Result := False;
  try

    Result := Inherited Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMIMMSubCatchmentFileObject.Reset;
const OPNAME = 'TMIMMSubCatchmentFileObject.Reset';
begin
  try
     Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{ TMIMMSubCatchmentListFileObject }

procedure TMIMMSubCatchmentListFileObject.CreateMemberObjects;
const OPNAME = 'TMIMMSubCatchmentListFileObject.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMIMMSubCatchmentListFileObject.DestroyMemberObjects;
const OPNAME = 'TMIMMSubCatchmentListFileObject.DestroyMemberObjects';
begin
  try
    inherited DestroyMemberObjects;
  
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMIMMSubCatchmentListFileObject.AddMineSubCatchmentFileObject: TMIMMSubCatchmentFileObject;
const OPNAME = 'TMIMMSubCatchmentListFileObject.AddMineSubCatchmentFileObject';
begin
  Result := nil;
  try
    Result := TMIMMSubCatchmentFileObject.Create;
    FMineSubCatchmentFileContainer.Add(Result);
  except on E: Exception do HandleError(E, OPNAME) end;
end; 


function TMIMMSubCatchmentListFileObject.Initialise: boolean;
const OPNAME = 'TMIMMSubCatchmentListFileObject.Initialise';
begin
  Result := False;
  try

    Result := inherited Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMIMMSubCatchmentListFileObject.MineSubCatchmentObjectCount: integer;
const OPNAME = 'TMIMMSubCatchmentListFileObject.MineSubCatchmentObjectCount';
begin
  Result := 0;
  try
    Result := FMineSubCatchmentFileContainer.Count;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMIMMSubCatchmentListFileObject.Reset;
const OPNAME = 'TMIMMSubCatchmentListFileObject.Reset';
begin
  try
    Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{ TGrowthFactors }

procedure TGrowthFactors.CreateMemberObjects;
const OPNAME = 'TMIMMFileObject.CreateMemberObjects';
Begin
  try
    FGrowthCode            := TInteger.Create;
    FNoOfPoints            := TInteger.Create;
    FInterpolationMethod   := TInteger.Create;
    FYearDatapoints        := TString.Create;
    FGrowthFactors         := TString.Create;
    FOwner                 := TString.Create;
    FMineType              := TString.Create;
    inherited CreateMemberObjects;
    Initialise;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TGrowthFactors.DestroyMemberObjects;
const OPNAME = 'TGrowthFactors.DestroyMemberObjects';
begin
  try
    FGrowthCode.Free;
    FNoOfPoints.Free;
    FInterpolationMethod.Free;
    FYearDatapoints.Free;
    FGrowthFactors.Free;
    FOwner.Free;
    FMineType.Free;

    inherited DestroyMemberObjects;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TGrowthFactors.Initialise: boolean;
const OPNAME = 'TGrowthFactors.Initialise';
begin
  Result := False;
  try
    FGrowthCode.FData := 0;
    FGrowthCode.FInitalised := False;
    FGrowthCode.FLength := 4;
    FGrowthCode.FDecimal := 0;
    FGrowthCode.FDefaultPadding := True;

    FNoOfPoints.FData := 0;
    FNoOfPoints.FInitalised := False;
    FNoOfPoints.FLength := 4;
    FNoOfPoints.FDecimal := 0;
    FNoOfPoints.FDefaultPadding := True;

    FInterpolationMethod.FData := 0;
    FInterpolationMethod.FInitalised := False;
    FInterpolationMethod.FLength := 4;
    FInterpolationMethod.FDecimal := 0;
    FInterpolationMethod.FDefaultPadding := True;

    FYearDatapoints.FData := '';
    FYearDatapoints.FInitalised := False;
    FYearDatapoints.FLength := 255;
    FYearDatapoints.FDecimal := 0;
    FYearDatapoints.FDefaultPadding := True;

    FGrowthFactors.FData := '';
    FGrowthFactors.FInitalised := False;
    FGrowthFactors.FLength := 255;
    FGrowthFactors.FDecimal := 0;
    FGrowthFactors.FDefaultPadding := True;

    FOwner.FData := '';
    FOwner.FInitalised := False;
    FOwner.FLength := 255;
    FOwner.FDecimal := 0;
    FOwner.FDefaultPadding := True;

    FMineType.FData := '';
    FMineType.FInitalised := False;
    FMineType.FLength := 255;
    FMineType.FDecimal := 0;
    FMineType.FDefaultPadding := True;


    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TGrowthFactors.Reset;
const OPNAME = 'TGrowthFactors.Reset';
begin
  try
     Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{ TLoadGeneration }

procedure TLoadGeneration.CreateMemberObjects;
const OPNAME = 'TMIMMOpenCastFileObject.CreateMemberObjects';
var
  LIndex : integer;
begin
  try
    FStdDeviation          := TDouble.Create;
    FLoadGenType           := TInteger.Create;
    for LIndex := 1 to 10 do
    begin
      FFlow[LIndex]       := TDouble.Create;
      FMeanOfSalt[LIndex] := TDouble.Create;
    end;
    inherited CreateMemberObjects;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TLoadGeneration.DestroyMemberObjects;
const OPNAME = 'TLoadGeneration.Initialise';
var
  LIndex : integer;
begin
  try
    FStdDeviation.Free;
    FLoadGenType.Free;
    for LIndex := 1 to 10 do
    begin
      FFlow[LIndex].Free;
      FMeanOfSalt[LIndex].Free;
    end;
    inherited DestroyMemberObjects;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TLoadGeneration.Initialise: boolean;
const OPNAME = 'TLoadGeneration.Initialise';
var
  LIndex : integer;
begin
  Result := False;
  try
    FStdDeviation.FData := 0.00;
    FStdDeviation.FInitalised := False;
    FStdDeviation.FLength := 10;
    FStdDeviation.FDecimal := 2;
    FStdDeviation.FDefaultPadding := True;
    FStdDeviation.ShowDecimalPoint := True;

    FLoadGenType.FData := 0;
    FLoadGenType.FInitalised := False;
    FLoadGenType.FLength := 4;
    FLoadGenType.FDecimal := 0;
    FLoadGenType.FDefaultPadding := True;


     for LIndex := 1 to 10 do
    begin
      FFlow[LIndex].FData := 0.00;
      FFlow[LIndex].FInitalised := False;
      FFlow[LIndex].FLength := 10;
      FFlow[LIndex].FDecimal := 2;
      FFlow[LIndex].FDefaultPadding := True;
      FFlow[LIndex].ShowDecimalPoint := True;

      FMeanOfSalt[LIndex].FData:= 0.00;
      FMeanOfSalt[LIndex].FInitalised := False;
      FMeanOfSalt[LIndex].FLength := 10;
      FMeanOfSalt[LIndex].FDecimal := 2;
      FMeanOfSalt[LIndex].FDefaultPadding := True;
      FMeanOfSalt[LIndex].ShowDecimalPoint := True;

    end;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TLoadGeneration.Reset;
const OPNAME = 'TLoadGeneration.Reset';
begin
  try
    Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
