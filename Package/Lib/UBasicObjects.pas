//
//
//  UNIT      : Contains basic data types objects Classes
//  AUTHOR    : Titi Ngubane(PDNA)
//  DATE      : 29/01/2002
//  COPYRIGHT : Copyright © 2002 DWAF
//
//
unit UBasicObjects;

interface

uses
  //  Delphi VCL
  Classes, sysutils,inifiles,contnrs;
  //UConstants;

type

  TInteger = class(Tobject)
  public
    FData: Integer;
    FInitalised: boolean;
    FLength: integer;
    FDecimal: integer;
    FDefaultPadding: Boolean;
  end;

  TChar = class(Tobject)
  public
    FData: Char;
    FInitalised: boolean;
    FLength: integer;
    FDecimal: integer;
    FDefaultPadding: Boolean;
  end;

{  TSingle = class(Tobject)
  public
    FData: Single;
    FInitalised: boolean;
    FLength: integer;
    FDecimal: integer;
    FDefaultPadding: Boolean;
    ShowDecimalPoint: Boolean;
  end;
  }
  TDouble = class(Tobject)
  public
    FData: Double;
    FInitalised: boolean;
    FLength: integer;
    FDecimal: integer;
    FDefaultPadding: Boolean;
    ShowDecimalPoint: Boolean;
  end;

  TString = class(Tobject)
  public
    FData: String;
    FInitalised: boolean;
    FLength: integer;
    FDecimal: integer;
    FDefaultPadding: Boolean;
  end;

  TBoolean = class(Tobject)
  public
    FData: Boolean;
    FInitalised: boolean;
    FLength: integer;
    FDecimal: integer;
    FDefaultPadding: Boolean;
  end;

implementation

 end.




