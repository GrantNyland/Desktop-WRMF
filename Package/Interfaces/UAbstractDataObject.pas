//
//
//  UNIT      : Contains TAbstractDataObject Class
//  AUTHOR    : Dziedzi Ramulondi(PDNA)
//  DATE      : 28/01/2002
//  COPYRIGHT : Copyright © 2002 DWAF
//
//
unit UAbstractDataObject;

interface

uses
  Classes, sysutils,contnrs,

  //  DWAF VCL
  UAbstractObject;

type

  TAbstractDataObject = class(TAbstractObject)
  public
    procedure Reset;virtual; abstract;
  end;

  TAbstractAppDataObject = class(TAbstractAppObject)
  public
    procedure Reset;virtual; abstract;
  end;

implementation

end.
