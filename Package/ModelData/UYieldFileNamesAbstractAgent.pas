//
//
//  UNIT      : Contains TYieldFileNamesAbstractAgent Class
//  AUTHOR    : Dziedzi Ramulondi (PDNA)
//  DATE      : 15/01/2003
//  COPYRIGHT : Copyright © 2003 DWAF
//
//
unit UYieldFileNamesAbstractAgent;

interface

uses
  Classes,Contnrs, sysutils,

  //  DWAF VCL
  UConstants,
  //UDatabaseUtilities,
  UAbstractObject,
  UFileNames,
  UDataFileObjects;

type

  TYieldFileNamesAbstractAgent = class(TAbstractAppObject)
  public
   { function ReadDirectoryFileName(ADataFileObjects: TDataFileObjects;AFileNamesObject: TModelFileNames): boolean; virtual; abstract;
    function ReadConfigFileNames(ADataFileObjects: TDataFileObjects;AFileNamesObject: TModelFileNames): boolean; virtual; abstract;
    function ReadParamFileName(ADataFileObjects: TDataFileObjects;AFileNamesObject: TModelFileNames): boolean; virtual; abstract;
    function ReadDemandFileNames(ADataFileObjects: TDataFileObjects;AFileNamesObject: TModelFileNames): boolean; virtual; abstract;
    function ReadHydrologyFileNames(ADataFileObjects: TDataFileObjects;AFileNamesObject: TModelFileNames): boolean; virtual; abstract;
    function ReadOutputFileNames(ADataFileObjects: TDataFileObjects;AFileNamesObject: TModelFileNames): boolean; virtual; abstract;
   } 
  end;


implementation

end.
