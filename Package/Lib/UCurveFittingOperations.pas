//
//
//  UNIT      : Contains curve fitting utility operations.
//  AUTHOR    : Grant Nyland (PDNA)
//  DATE      : 2002/09/12
//  COPYRIGHT : Copyright © 2002 DWAF
//
//
unit UCurveFittingOperations;

interface

uses
  UCurveFittingConstants;

function CalculateCurveCoefficientsDelphi (ACurveType             : TCurveType;
                                           var ACurveCoefficients : array of double;
                                           var AYXPairs           : array of double;
                                           AForce                 : Boolean=FALSE): boolean;

implementation

uses
  SysUtils,
  UFletcherPowellSearch_YRC,
  UErrorHandlingOperations;

function DoHorizontalDeterministic(var ACurveCoefficients: array of double; AYXPairs: array of double): boolean;
const OPNAME = 'DoHorizontalDeterministic';
begin
  Result := False;
  try

    // Calculate the coefficients.
    ACurveCoefficients[0] := AYXPairs[0];

    // Done.
    Result := True;

  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function DoHorizontalAverage(var ACurveCoefficients: array of double; AYXPairs: array of double): boolean;
const OPNAME = 'DoHorizontalAverage';
var LIndex: integer;
begin
  Result := False;
  try

    // Calculate the coefficients.
    for LIndex := 0 to (Length(AYXPairs) div 2) - 1 do
      ACurveCoefficients[0] := ACurveCoefficients[0] + AYXPairs[LIndex * 2];
    ACurveCoefficients[0] := ACurveCoefficients[0] / (Length(AYXPairs) div 2);

    // Done.
    Result := True;

  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function DoLinearDeterministic(var ACurveCoefficients: array of double; AYXPairs: array of double): boolean;
const OPNAME = 'DoLinearDeterministic';
begin
  Result := False;
  try

    // Calculate the coefficients.
    ACurveCoefficients[1] := (AYXPairs[2] - AYXPairs[0]) / (AYXPairs[3] - AYXPairs[1]);
    ACurveCoefficients[0] := AYXPairs[0] - ACurveCoefficients[1] * AYXPairs[1];

    // Done.
    Result := True;

  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function DoLinearRegression(var ACurveCoefficients: array of double; AYXPairs: array of double): boolean;
const OPNAME = 'DoLinearRegression';
var
  LIndex, N: integer;
  SigY, SigX, SigXY, SigX2: double;
  denom: double;
begin
  Result := False;
  try

    // Calculate the coefficients.
    N := Length(AYXPairs) div 2;
    SigY  := 0.0;
    SigX  := 0.0;
    SigXY := 0.0;
    SigX2 := 0.0;
    for LIndex := 0 to N - 1 do
    begin
      SigY  := SigY  + AYXPairs[LIndex * 2];
      SigX  := SigX  + AYXPairs[LIndex * 2 + 1];
      SigXY := SigXY + AYXPairs[LIndex * 2] * AYXPairs[LIndex * 2 + 1];
      SigX2 := SigX2 + AYXPairs[LIndex * 2 + 1] * AYXPairs[LIndex * 2 + 1];
    end;

    // Regression formula.
    denom := N * SigX2 - SigX * SigX;
    ACurveCoefficients[0] := (SigY * SigX2 - SigX * SigXY) / denom;
    ACurveCoefficients[1] := (N * SigXY - SigX * SigY) / denom;

    // Done.
    Result := True;

  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function DoQuadraticDeterministic(var ACurveCoefficients: array of double; AYXPairs: array of double): boolean;
const OPNAME = 'DoQuadraticDeterministic';
var K1, K2, K3, K4: double;
begin
  Result := False;
  try

    // Calculate quadratic sub constants.
    K1 := (AYXPairs[2] - AYXPairs[0]) / (AYXPairs[3] - AYXPairs[1]);
    K2 := (AYXPairs[4] - AYXPairs[0]) / (AYXPairs[5] - AYXPairs[1]);
    K3 := (AYXPairs[3] * AYXPairs[3] - AYXPairs[1] * AYXPairs[1]) / (AYXPairs[3] - AYXPairs[1]);
    K4 := (AYXPairs[5] * AYXPairs[5] - AYXPairs[1] * AYXPairs[1]) / (AYXPairs[5] - AYXPairs[1]);

    // Calculate the coefficients.
    ACurveCoefficients[2] := (K2 - K1) / (K4 - K3);
    ACurveCoefficients[1] := K1 - K3 * ACurveCoefficients[2];
    ACurveCoefficients[0] := AYXPairs[0] - ACurveCoefficients[1] * AYXPairs[1] - ACurveCoefficients[2] * AYXPairs[1] * AYXPairs[1];

    // Done.
    Result := True;

  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function DoTrinomialDeterministic(var ACurveCoefficients: array of double; AYXPairs: array of double): boolean;
const OPNAME = 'DoTrinomialDeterministic';
var K1, K2, K3, K4, K5, K6, K7, K8, K9, K10, K11, K12, K13: double;
begin
  Result := False;
  try

    // Calculate trinomial sub constants.
    K1 := (AYXPairs[2] - AYXPairs[0]) / (AYXPairs[3] - AYXPairs[1]);
    K2 := (AYXPairs[4] - AYXPairs[0]) / (AYXPairs[5] - AYXPairs[1]);
    K3 := (AYXPairs[6] - AYXPairs[0]) / (AYXPairs[7] - AYXPairs[1]);
    K4 := (AYXPairs[3] * AYXPairs[3] - AYXPairs[1] * AYXPairs[1]) / (AYXPairs[3] - AYXPairs[1]);
    K5 := (AYXPairs[5] * AYXPairs[5] - AYXPairs[1] * AYXPairs[1]) / (AYXPairs[5] - AYXPairs[1]);
    K6 := (AYXPairs[7] * AYXPairs[7] - AYXPairs[1] * AYXPairs[1]) / (AYXPairs[7] - AYXPairs[1]);
    K7 := (AYXPairs[3] * AYXPairs[3] * AYXPairs[3] - AYXPairs[1] * AYXPairs[1] * AYXPairs[1]) / (AYXPairs[3] - AYXPairs[1]);
    K8 := (AYXPairs[5] * AYXPairs[5] * AYXPairs[5] - AYXPairs[1] * AYXPairs[1] * AYXPairs[1]) / (AYXPairs[5] - AYXPairs[1]);
    K9 := (AYXPairs[7] * AYXPairs[7] * AYXPairs[7] - AYXPairs[1] * AYXPairs[1] * AYXPairs[1]) / (AYXPairs[7] - AYXPairs[1]);
    K10 := (K2 - K1) / (K5 - K4);
    K11 := (K8 - K7) / (K5 - K4);
    K12 := (K3 - K1) / (K6 - K4);
    K13 := (K9 - K7) / (K6 - K4);

    // Calculate the coefficients.
    ACurveCoefficients[3] := (K12 - K10) / (K13 - K11);
    ACurveCoefficients[2] := K10 - K11 * ACurveCoefficients[3];
    ACurveCoefficients[1] := K1 - K4 * ACurveCoefficients[2] - K7 * ACurveCoefficients[3];
    ACurveCoefficients[0] := AYXPairs[0] -
                             ACurveCoefficients[1] * AYXPairs[1] -
                             ACurveCoefficients[2] * AYXPairs[1] * AYXPairs[1] -
                             ACurveCoefficients[3] * AYXPairs[1] * AYXPairs[1] * AYXPairs[1];

    // Done.
    Result := True;

  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function CalculateCurveCoefficientsDelphi (ACurveType             : TCurveType;
                                           var ACurveCoefficients : array of double;
                                           var AYXPairs           : array of double;
                                           AForce                 : Boolean=FALSE): boolean;
const OPNAME = 'CalculateCurveCoefficientsDelphi';
var LIndex: integer;
begin
  Result := False;
  try

    // Set all the coefficients to zero.
    for LIndex := 0 to Length(ACurveCoefficients) - 1 do
      ACurveCoefficients[LIndex] := 0.0;

    // Branch on  the type of curve.
    case ACurveType of
      ctNull : ;
      ctHorizontalDeterministic :
        begin
          if (Length(AYXPairs) < 1) then
            raise Exception.CreateFmt('Insufficient points to calculate a horizontal line [%d].', [Length(AYXPairs) div 2]);
          DoHorizontalDeterministic(ACurveCoefficients, AYXPairs);
        end;
      ctHorizontalAverage :
        begin
          if (Length(AYXPairs) < 1) then
            raise Exception.CreateFmt('Insufficient points to calculate a horizontal line [%d].', [Length(AYXPairs) div 2]);
          DoHorizontalAverage(ACurveCoefficients, AYXPairs);
        end;
      ctLinearDeterministic :
        begin
          if (Length(AYXPairs) < 2) then
            raise Exception.CreateFmt('Insufficient points to calculate a line [%d].', [Length(AYXPairs) div 2]);
          DoLinearDeterministic(ACurveCoefficients, AYXPairs);
        end;
      ctLinearRegression :
        begin
          if (Length(AYXPairs) < 2) then
            raise Exception.CreateFmt('Insufficient points to do a linear regression [%d].', [Length(AYXPairs) div 2]);
          DoLinearRegression(ACurveCoefficients, AYXPairs);
        end;
      ctQuadraticDeterministic :
        begin
          if (Length(AYXPairs) < 3) then
            raise Exception.CreateFmt('Insufficient points to calculate a quadratic [%d].', [Length(AYXPairs) div 2]);
          DoQuadraticDeterministic(ACurveCoefficients, AYXPairs);
        end;
      ctQuadraticRegression :
        begin
          DoFletcherPowell(ctQuadraticRegression, ACurveCoefficients, AYXPairs);
        end;
      ctTrinomialDeterministic :
        begin
          if (Length(AYXPairs) < 4) then
            raise Exception.CreateFmt('Insufficient points to calculate a trinomial [%d].', [Length(AYXPairs) div 2]);
          DoTrinomialDeterministic(ACurveCoefficients, AYXPairs);
        end;
      ctTrinomialRegression :
        begin
          DoFletcherPowell(ctTrinomialRegression, ACurveCoefficients, AYXPairs, AForce);
        end;
    else
      raise Exception.CreateFmt('Unkown curve type [%d].', [integer(ACurveType)]);
    end;


    // Done.
    Result := True;

  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
