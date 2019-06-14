//
//
//  UNIT      : Contains TParamHeader Class
//  AUTHOR    : Dziedzi Ramulondi(PDNA)
//  DATE      : 05/08/2002
//  COPYRIGHT : Copyright © 2002 DWAF
//
//
unit UParamObject;

interface

uses
  Classes, sysutils,contnrs,

  //  DWAF VCL
  UAbstractDataObject,
  UBasicObjects,
  UConstants;

type

  TMatrixType = (mtUnspecified,mtMatrixB,mtMatrixB0,mtMatrixB1,mtMatrixA,mtMatrixC);
  TKeyGuageArray = array[MinKeyGauge..MaxKeyGauge] of TInteger;
  TParamHeader = class(TAbstractDataObject)
  protected
    // Line 1
    FGaugeCount: TInteger;
    FGaugeComment: TString;
    //Line 3
    FKeyGaugeCount: TInteger;
    FKeyGauges: TKeyGuageArray;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
  public
    procedure Reset;override;
    function Initialise: boolean;override;
    property GaugeCount: TInteger        read FGaugeCount     write FGaugeCount;
    property GaugeComment: TString       read FGaugeComment   write FGaugeComment;
    property KeyGaugeCount: TInteger     read FKeyGaugeCount  write FKeyGaugeCount;
    property KeyGauges: TKeyGuageArray   read FKeyGauges;
  end;

  TGaugeStochastics  = class(TAbstractDataObject)
  protected
    //Line 5
    FGaugePathName : TString;
    //Line 6
    FYearsNumber   : TInteger;
    FYearStart     : TInteger;
    FResidual1     : TDouble;
    FResidual2     : TDouble;
    FVariate1      : TDouble;
    FVariate2      : TDouble;
    //Line 7
    FTransformType  : TInteger;
    FTransformGamma : TDouble;
    FTransformDelta : TDouble;
    FTransformXlam  : TDouble;
    FTransformXi    : TDouble;
    //Line 8
    FResidualMean   : TDouble;
    FResidualStdDev : TDouble;
    FArmaPhi1       : TDouble;
    FArmaPhi2       : TDouble;
    FArmaTheta1     : TDouble;
    FArmaTheta2     : TDouble;
    FPhiZero        : TDouble;
    FZTVariates     : TInteger;
    //Line 9
    FParamXA        : TDouble;
    FParamXSD       : TDouble;
    FParamAIC       : TDouble;
    FParamANC       : TDouble;
    FCatchmentArea  : TDouble;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
  public
    procedure Reset;override;
    function Initialise: boolean;override;

    property GaugePathName  : TString  read FGaugePathName;
    property YearsNumber    : TInteger read FYearsNumber;
    property YearStart      : TInteger read FYearStart;
    property Residual1      : TDouble  read FResidual1;
    property Residual2      : TDouble  read FResidual2;
    property Variate1       : TDouble  read FVariate1;
    property Variate2       : TDouble  read FVariate2;
    property TransformType  : TInteger read FTransformType;
    property TransformGamma : TDouble  read FTransformGamma;
    property TransformDelta : TDouble  read FTransformDelta;
    property TransformXlam  : TDouble  read FTransformXlam;
    property TransformXi    : TDouble  read FTransformXi;
    property ResidualMean   : TDouble  read FResidualMean;
    property ResidualStdDev : TDouble  read FResidualStdDev;
    property ArmaPhi1       : TDouble  read FArmaPhi1;
    property ArmaPhi2       : TDouble  read FArmaPhi2;
    property ArmaTheta1     : TDouble  read FArmaTheta1;
    property ArmaTheta2     : TDouble  read FArmaTheta2;
    property PhiZero        : TDouble  read FPhiZero;
    property ZTVariates     : TInteger read FZTVariates;
    property ParamXA        : TDouble  read FParamXA;
    property ParamXSD       : TDouble  read FParamXSD;
    property ParamAIC       : TDouble  read FParamAIC;
    property ParamANC       : TDouble  read FParamANC;
    property CatchmentArea  : TDouble  read FCatchmentArea;
  end;

  TGaugeStochasticsContainer  = class(TAbstractDataObject)
  protected
    FGaugeStochasticsContainer : TObjectList;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    function GetGaugeStochasticsByIndex(AIndex: integer): TGaugeStochastics;
  public
    procedure Reset;override;
    function ItemsCount: integer;
    function Initialise: boolean;override;
    function AddGaugeStochastics:TGaugeStochastics;
    property GaugeStochasticsByIndex[AIndex: integer]: TGaugeStochastics read GetGaugeStochasticsByIndex;
  end;

  TMatrixComments  = class(TAbstractDataObject)
  protected
    FMatrixBComment    : TString;
    FMatrixB0Comment   : TString;
    FMatrixB1Comment   : TString;
    FMatrixAComment    : TString;
    FMatrixCComment    : TString;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
  public
    procedure Reset;override;
    function Initialise: boolean;override;
    property MatrixBComment    : TString read FMatrixBComment;
    property MatrixB0Comment   : TString read FMatrixB0Comment;
    property MatrixB1Comment   : TString read FMatrixB1Comment;
    property MatrixAComment    : TString read FMatrixAComment;
    property MatrixCComment    : TString read FMatrixCComment;
  end;

  TMatrixArray = array[MinMatrix..MaxMatrix] of TDouble;
  TMatrixLine  = class(TAbstractDataObject)
  protected
    FMatrixLine : TMatrixArray;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
  public
    procedure Reset;override;
    function Initialise: boolean;override;
    property MatrixLine : TMatrixArray read FMatrixLine;
  end;

  TMatrixContainer  = class(TAbstractDataObject)
  protected
    FMatrixLineContainer : TObjectList;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    function GetMatrixLineByIndex(AIndex: integer):TMatrixLine;
  public
    procedure Reset;override;
    function ItemsCount: integer;
    function Initialise: boolean;override;
    function AddMatrixLine:TMatrixLine;
    property MatrixLineByIndex[AIndex: integer] :TMatrixLine read GetMatrixLineByIndex;
  end;

  TParamObject = class(TAbstractDataObject)
  protected
    FParamHeaderObject : TParamHeader;
    FGaugeStochasticsContainer  : TGaugeStochasticsContainer;
    FMatrixComments: TMatrixComments;
    FMatrixB    : TMatrixContainer;
    FMatrixB0   : TMatrixContainer;
    FMatrixB1   : TMatrixContainer;
    FMatrixA    : TMatrixContainer;
    FMatrixC    : TMatrixContainer;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
  public
    procedure Reset;override;
    function Initialise: boolean;override;

    property ParamHeaderObject : TParamHeader read FParamHeaderObject;
    property GaugeStochasticsContainer  : TGaugeStochasticsContainer read FGaugeStochasticsContainer;
    property MatrixComments: TMatrixComments read FMatrixComments;
    property MatrixB    : TMatrixContainer read FMatrixB;
    property MatrixB0   : TMatrixContainer read FMatrixB0;
    property MatrixB1   : TMatrixContainer read FMatrixB1;
    property MatrixA    : TMatrixContainer read FMatrixA;
    property MatrixC    : TMatrixContainer read FMatrixC;
  end;


implementation


uses UErrorHandlingOperations;

{TParamHeader}

procedure TParamHeader.CreateMemberObjects;
const OPNAME = 'TParamHeader.CreateMemberObjects';
var
  LCount: integer;
Begin
  try
    FGaugeCount    := TInteger.Create;
    FGaugeComment  := TString.Create;
    FKeyGaugeCount := TInteger.Create;
    for LCount  := MinKeyGauge to MaxKeyGauge do
    begin
      FKeyGauges[LCount] := TInteger.Create;
    end;
    Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TParamHeader.DestroyMemberObjects;
const OPNAME = 'TParamHeader.DestroyMemberObjects';
var
  LCount: integer;
Begin
  try
    FreeAndNil(FGaugeCount);
    FreeAndNil(FGaugeComment);
    FreeAndNil(FKeyGaugeCount);
    for LCount  := MinKeyGauge to MaxKeyGauge do
    begin
      FreeAndNil(FKeyGauges[LCount]);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TParamHeader.Reset;
const OPNAME = 'TParamHeader.Reset';
begin
  try
     Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TParamHeader.Initialise: boolean;
const OPNAME = 'TParamHeader.Initialise';
var
  LCount: integer;
Begin
  Result := False;
  try
    //Data and initialised.
    FGaugeCount.FData := 0;
    FGaugeCount.FInitalised := False;
    FGaugeCount.FLength := 12;
    FGaugeCount.FDecimal := 0;
    FGaugeCount.FDefaultPadding := True;

    FGaugeComment.FData := '';
    FGaugeComment.FInitalised := False;
    FGaugeComment.FLength := 0;
    FGaugeComment.FDecimal := 0;
    FGaugeComment.FDefaultPadding := True;

    FKeyGaugeCount.FData := 0;
    FKeyGaugeCount.FInitalised := False;
    FKeyGaugeCount.FLength := 3;
    FKeyGaugeCount.FDecimal := 0;
    FKeyGaugeCount.FDefaultPadding := True;

    for LCount  := MinKeyGauge to MaxKeyGauge do
    begin
      FKeyGauges[LCount].FData := 0;
      FKeyGauges[LCount].FInitalised := False;
      FKeyGauges[LCount].FLength := 3;
      FKeyGauges[LCount].FDecimal := 0;
      FKeyGauges[LCount].FDefaultPadding := True;
    end;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{ TGaugeStochastics }

procedure TGaugeStochastics.CreateMemberObjects;
const OPNAME = 'TGaugeStochastics.CreateMemberObjects';
begin
  inherited;
  try
    FGaugePathName := TString.Create;
    //Line 6
    FYearsNumber   := TInteger.Create;
    FYearStart     := TInteger.Create;
    FResidual1     := TDouble.Create;
    FResidual2     := TDouble.Create;
    FVariate1      := TDouble.Create;
    FVariate2      := TDouble.Create;
    //Line 7
    FTransformType  := TInteger.Create;
    FTransformGamma := TDouble.Create;
    FTransformDelta := TDouble.Create;
    FTransformXlam  := TDouble.Create;
    FTransformXi    := TDouble.Create;
    //Line 8
    FResidualMean   := TDouble.Create;
    FResidualStdDev := TDouble.Create;
    FArmaPhi1       := TDouble.Create;
    FArmaPhi2       := TDouble.Create;
    FArmaTheta1     := TDouble.Create;
    FArmaTheta2     := TDouble.Create;
    FPhiZero        := TDouble.Create;
    FZTVariates     := TInteger.Create;
    //Line 9
    FParamXA        := TDouble.Create;
    FParamXSD       := TDouble.Create;
    FParamAIC       := TDouble.Create;
    FParamANC       := TDouble.Create;
    FCatchmentArea  := TDouble.Create;
    Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TGaugeStochastics.DestroyMemberObjects;
const OPNAME = 'TGaugeStochastics.DestroyMemberObjects';
begin
  inherited;
  try
    FreeAndNil(FGaugePathName);
    //Line 6
    FreeAndNil(FYearsNumber);
    FreeAndNil(FYearStart);
    FreeAndNil(FResidual1);
    FreeAndNil(FResidual2);
    FreeAndNil(FVariate1);
    FreeAndNil(FVariate2);
    //Line 7
    FreeAndNil(FTransformType);
    FreeAndNil(FTransformGamma);
    FreeAndNil(FTransformDelta);
    FreeAndNil(FTransformXlam);
    FreeAndNil(FTransformXi);
    //Line 8
    FreeAndNil(FResidualMean);
    FreeAndNil(FResidualStdDev);
    FreeAndNil(FArmaPhi1);
    FreeAndNil(FArmaPhi2);
    FreeAndNil(FArmaTheta1);
    FreeAndNil(FArmaTheta2);
    FreeAndNil(FPhiZero);
    FreeAndNil(FZTVariates);
    //Line 9
    FreeAndNil(FParamXA);
    FreeAndNil(FParamXSD);
    FreeAndNil(FParamAIC);
    FreeAndNil(FParamANC);
    FreeAndNil(FCatchmentArea);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TGaugeStochastics.Initialise: boolean;
const OPNAME = 'TGaugeStochastics.Initialise';
begin
  Result := inherited Initialise;
  try
    FGaugePathName.FData := '';
    FGaugePathName.FInitalised := False;
    FGaugePathName.FLength := 0;
    FGaugePathName.FDecimal := 0;
    FGaugePathName.FDefaultPadding := True;

    //Line 6
    FYearsNumber.FData := 0;
    FYearsNumber.FInitalised := False;
    FYearsNumber.FLength := 12;
    FYearsNumber.FDecimal := 0;
    FYearsNumber.FDefaultPadding := True;

    FYearStart.FData := 0;
    FYearStart.FInitalised := False;
    FYearStart.FLength := 12;
    FYearStart.FDecimal := 0;
    FYearStart.FDefaultPadding := True;

    FResidual1.FData := 0;
    FResidual1.FInitalised := False;
    FResidual1.FLength := 12;
    FResidual1.FDecimal := 4;
    FResidual1.FDefaultPadding := True;

    FResidual2.FData := 0;
    FResidual2.FInitalised := False;
    FResidual2.FLength := 12;
    FResidual2.FDecimal := 4;
    FResidual2.FDefaultPadding := True;

    FVariate1.FData := 0;
    FVariate1.FInitalised := False;
    FVariate1.FLength := 12;
    FVariate1.FDecimal := 4;
    FVariate1.FDefaultPadding := True;

    FVariate2.FData := 0;
    FVariate2.FInitalised := False;
    FVariate2.FLength := 12;
    FVariate2.FDecimal := 4;
    FVariate2.FDefaultPadding := True;

    //Line 7
    FTransformType.FData := 0;
    FTransformType.FInitalised := False;
    FTransformType.FLength := 2;
    FTransformType.FDecimal := 0;
    FTransformType.FDefaultPadding := True;

    FTransformGamma.FData := 0;
    FTransformGamma.FInitalised := False;
    FTransformGamma.FLength := 20;
    FTransformGamma.FDecimal := 7;
    FTransformGamma.FDefaultPadding := True;

    FTransformDelta.FData := 0;
    FTransformDelta.FInitalised := False;
    FTransformDelta.FLength := 20;
    FTransformDelta.FDecimal := 7;
    FTransformDelta.FDefaultPadding := True;

    FTransformXlam.FData := 0;
    FTransformXlam.FInitalised := False;
    FTransformXlam.FLength := 20;
    FTransformXlam.FDecimal := 7;
    FTransformXlam.FDefaultPadding := True;

    FTransformXi.FData := 0;
    FTransformXi.FInitalised := False;
    FTransformXi.FLength := 20;
    FTransformXi.FDecimal := 7;
    FTransformXi.FDefaultPadding := True;

    //Line 8
    FResidualMean.FData := 0;
    FResidualMean.FInitalised := False;
    FResidualMean.FLength := 12;
    FResidualMean.FDecimal := 5;
    FResidualMean.FDefaultPadding := True;

    FResidualStdDev.FData := 0;
    FResidualStdDev.FInitalised := False;
    FResidualStdDev.FLength := 12;
    FResidualStdDev.FDecimal := 5;
    FResidualStdDev.FDefaultPadding := True;

    FArmaPhi1.FData := 0;
    FArmaPhi1.FInitalised := False;
    FArmaPhi1.FLength := 12;
    FArmaPhi1.FDecimal := 5;
    FArmaPhi1.FDefaultPadding := True;

    FArmaPhi2.FData := 0;
    FArmaPhi2.FInitalised := False;
    FArmaPhi2.FLength := 12;
    FArmaPhi2.FDecimal := 5;
    FArmaPhi2.FDefaultPadding := True;

    FArmaTheta1.FData := 0;
    FArmaTheta1.FInitalised := False;
    FArmaTheta1.FLength := 12;
    FArmaTheta1.FDecimal := 5;
    FArmaTheta1.FDefaultPadding := True;

    FArmaTheta2.FData := 0;
    FArmaTheta2.FInitalised := False;
    FArmaTheta2.FLength := 12;
    FArmaTheta2.FDecimal := 5;
    FArmaTheta2.FDefaultPadding := True;

    FPhiZero.FData := 0;
    FPhiZero.FInitalised := False;
    FPhiZero.FLength := 12;
    FPhiZero.FDecimal := 5;
    FPhiZero.FDefaultPadding := True;

    FZTVariates.FData := 0;
    FZTVariates.FInitalised := False;
    FZTVariates.FLength := 4;
    FZTVariates.FDecimal := 0;
    FZTVariates.FDefaultPadding := True;

    //Line 9
    FParamXA.FData := 0;
    FParamXA.FInitalised := False;
    FParamXA.FLength := 12;
    FParamXA.FDecimal := 5;
    FParamXA.FDefaultPadding := True;

    FParamXSD.FData := 0;
    FParamXSD.FInitalised := False;
    FParamXSD.FLength := 12;
    FParamXSD.FDecimal := 5;
    FParamXSD.FDefaultPadding := True;

    FParamAIC.FData := 0;
    FParamAIC.FInitalised := False;
    FParamAIC.FLength := 12;
    FParamAIC.FDecimal := 5;
    FParamAIC.FDefaultPadding := True;

    FParamANC.FData := 0;
    FParamANC.FInitalised := False;
    FParamANC.FLength := 12;
    FParamANC.FDecimal := 5;
    FParamANC.FDefaultPadding := True;

    FCatchmentArea.FData := 0;
    FCatchmentArea.FInitalised := False;
    FCatchmentArea.FLength := 7;
    FCatchmentArea.FDecimal := 3;
    FCatchmentArea.FDefaultPadding := True;

    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TGaugeStochastics.Reset;
const OPNAME = 'TGaugeStochastics.Reset';
begin
  inherited;
  try
     Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{ TGaugeStochasticsContainer }

procedure TGaugeStochasticsContainer.CreateMemberObjects;
const OPNAME = 'TGaugeStochasticsContainer.CreateMemberObjects';
begin
  inherited CreateMemberObjects;
  try
    FGaugeStochasticsContainer := TObjectList.Create(True);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TGaugeStochasticsContainer.DestroyMemberObjects;
const OPNAME = 'TGaugeStochasticsContainer.DestroyMemberObjects';
begin
  inherited DestroyMemberObjects;
  try
    FreeAndNil(FGaugeStochasticsContainer);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TGaugeStochasticsContainer.GetGaugeStochasticsByIndex(AIndex: integer): TGaugeStochastics;
const OPNAME = 'TGaugeStochasticsContainer.GetGaugeStochasticsByIndex';
begin
  Result := nil;
  try
    if (AIndex < FGaugeStochasticsContainer.Count) and Assigned(FGaugeStochasticsContainer.Items[AIndex]) then
      Result := TGaugeStochastics(FGaugeStochasticsContainer.Items[AIndex]);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TGaugeStochasticsContainer.Initialise: boolean;
const OPNAME = 'TGaugeStochasticsContainer.Initialise';
begin
  Result := False;
  try
    FGaugeStochasticsContainer.Clear;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TGaugeStochasticsContainer.Reset;
const OPNAME = 'TGaugeStochasticsContainer.Reset';
begin
  inherited;
  try
     Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TGaugeStochasticsContainer.AddGaugeStochastics: TGaugeStochastics;
const OPNAME = 'TGaugeStochasticsContainer.AddGaugeStochastics';
begin
  Result := nil;
  try
    Result := TGaugeStochastics.Create;
    FGaugeStochasticsContainer.Add(Result);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TGaugeStochasticsContainer.ItemsCount: integer;
const OPNAME = 'TGaugeStochasticsContainer.ItemsCount';
begin
  Result := 0;
  try
    Result := FGaugeStochasticsContainer.Count;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{ TMatrixComments }

procedure TMatrixComments.CreateMemberObjects;
const OPNAME = 'TMatrixComments.CreateMemberObjects';
begin
  inherited CreateMemberObjects;
  try
    FMatrixBComment    := TString.Create;
    FMatrixB0Comment   := TString.Create;
    FMatrixB1Comment   := TString.Create;
    FMatrixAComment    := TString.Create;
    FMatrixCComment    := TString.Create;
    Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMatrixComments.DestroyMemberObjects;
const OPNAME = 'TMatrixComments.DestroyMemberObjects';
begin
  try
    FreeAndNil(FMatrixBComment);
    FreeAndNil(FMatrixB0Comment);
    FreeAndNil(FMatrixB1Comment);
    FreeAndNil(FMatrixAComment);
    FreeAndNil(FMatrixCComment);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMatrixComments.Initialise: boolean;
const OPNAME = 'TMatrixComments.Initialise';
begin
  Result := False;
  try
    FMatrixBComment.FData := '';
    FMatrixBComment.FInitalised := False;
    FMatrixBComment.FLength := 0;
    FMatrixBComment.FDecimal := 0;
    FMatrixBComment.FDefaultPadding := True;

    FMatrixB0Comment.FData := '';
    FMatrixB0Comment.FInitalised := False;
    FMatrixB0Comment.FLength := 0;
    FMatrixB0Comment.FDecimal := 0;
    FMatrixB0Comment.FDefaultPadding := True;

    FMatrixB1Comment.FData := '';
    FMatrixB1Comment.FInitalised := False;
    FMatrixB1Comment.FLength := 0;
    FMatrixB1Comment.FDecimal := 0;
    FMatrixB1Comment.FDefaultPadding := True;

    FMatrixAComment.FData := '';
    FMatrixAComment.FInitalised := False;
    FMatrixAComment.FLength := 0;
    FMatrixAComment.FDecimal := 0;
    FMatrixAComment.FDefaultPadding := True;

    FMatrixCComment.FData := '';
    FMatrixCComment.FInitalised := False;
    FMatrixCComment.FLength := 0;
    FMatrixCComment.FDecimal := 0;
    FMatrixCComment.FDefaultPadding := True;

    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMatrixComments.Reset;
const OPNAME = 'TMatrixComments.Reset';
begin
  inherited;
  try
     Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{ TMatrixLine }

procedure TMatrixLine.CreateMemberObjects;
const OPNAME = 'TMatrixLine.CreateMemberObjects';
var
  LIndex: integer;
begin
  inherited CreateMemberObjects;
  try
    for LIndex := MinMatrix to MaxMatrix do
    begin
      FMatrixLine[LIndex] := TDouble.Create;
    end;
    Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMatrixLine.DestroyMemberObjects;
const OPNAME = 'TMatrixLine.DestroyMemberObjects';
var
  LIndex: integer;
begin
  inherited DestroyMemberObjects;
  try
    for LIndex := MinMatrix to MaxMatrix do
    begin
      FreeAndNil(FMatrixLine[LIndex]);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMatrixLine.Initialise: boolean;
const OPNAME = 'TMatrixLine.Initialise';
var
  LIndex: integer;
begin
  Result := False;
  try
    for LIndex := MinMatrix to MaxMatrix do
    begin
      FMatrixLine[LIndex].FData := 0;
      FMatrixLine[LIndex].FInitalised := False;
      FMatrixLine[LIndex].FLength := 12;
      FMatrixLine[LIndex].FDecimal := 8;
      FMatrixLine[LIndex].FDefaultPadding := True;
    end;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMatrixLine.Reset;
const OPNAME = 'TMatrixLine.Reset';
begin
  try
     Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{ TMatrixContainer }

procedure TMatrixContainer.CreateMemberObjects;
const OPNAME = 'TMatrixContainer.CreateMemberObjects';
begin
  inherited CreateMemberObjects;
  try
    FMatrixLineContainer := TObjectList.Create(True);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMatrixContainer.DestroyMemberObjects;
const OPNAME = 'TMatrixContainer.DestroyMemberObjects';
begin
  inherited DestroyMemberObjects;
  try
    FreeAndNil(FMatrixLineContainer);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMatrixContainer.GetMatrixLineByIndex(AIndex: integer): TMatrixLine;
const OPNAME = 'TMatrixContainer.GetMatrixLineByIndex';
begin
  Result := nil;
  try
    if (AIndex < FMatrixLineContainer.Count) and Assigned(FMatrixLineContainer.Items[AIndex]) then
      Result := TMatrixLine(FMatrixLineContainer.Items[AIndex]);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMatrixContainer.Initialise: boolean;
const OPNAME = 'TMatrixContainer.Initialise';
begin
  Result := False;
  try
    FMatrixLineContainer.Clear;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMatrixContainer.Reset;
const OPNAME = 'TMatrixContainer.Reset';
begin
  inherited;
  try
     Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMatrixContainer.AddMatrixLine: TMatrixLine;
const OPNAME = 'TMatrixContainer.AddMatrixLine';
begin
  Result := nil;
  try
    Result := TMatrixLine.Create;
    FMatrixLineContainer.Add(Result);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMatrixContainer.ItemsCount: integer;
const OPNAME = 'TMatrixContainer.ItemsCount';
begin
  Result := 0;
  try
    Result := FMatrixLineContainer.Count;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{ TParamObject }

procedure TParamObject.CreateMemberObjects;
const OPNAME = 'TParamObject.CreateMemberObjects';
begin
  inherited CreateMemberObjects;
  try
    FParamHeaderObject          := TParamHeader.Create;
    FGaugeStochasticsContainer  := TGaugeStochasticsContainer.Create;
    FMatrixComments             := TMatrixComments.Create;
    FMatrixB                    := TMatrixContainer.Create;
    FMatrixB0                   := TMatrixContainer.Create;
    FMatrixB1                   := TMatrixContainer.Create;
    FMatrixA                    := TMatrixContainer.Create;
    FMatrixC                    := TMatrixContainer.Create;
    Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TParamObject.DestroyMemberObjects;
const OPNAME = 'TParamObject.DestroyMemberObjects';
begin
  inherited DestroyMemberObjects;
  try
    FreeAndNil(FParamHeaderObject);
    FreeAndNil(FGaugeStochasticsContainer);
    FreeAndNil(FMatrixComments);
    FreeAndNil(FMatrixB);
    FreeAndNil(FMatrixB0);
    FreeAndNil(FMatrixB1);
    FreeAndNil(FMatrixA);
    FreeAndNil(FMatrixC);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TParamObject.Initialise: boolean;
const OPNAME = 'TParamObject.Initialise';
begin
  Result := False;
  try
    FParamHeaderObject.Initialise;
    FGaugeStochasticsContainer.Initialise;
    FMatrixComments.Initialise;
    FMatrixB.Initialise;
    FMatrixB0.Initialise;
    FMatrixB1.Initialise;
    FMatrixA.Initialise;
    FMatrixC.Initialise;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TParamObject.Reset;
const OPNAME = 'TParamObject.Reset';
begin
  try
    Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
