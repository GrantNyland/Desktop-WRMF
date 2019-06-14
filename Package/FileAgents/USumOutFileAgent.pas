//
//
//  UNIT      : Contains TSumOutFileAgent Class
//  AUTHOR    : Dziedzi Ramulondi(PDNA)
//  DATE      : 21/01/2003
//  COPYRIGHT : Copyright © 2001 DWAF
//
//
unit USumOutFileAgent;

interface

uses
  Classes, sysutils,

  //  DWAF VCL
  UConstants,
  UFileNames,
  UAbstractFileAgent,
  UAbstractObject,
  UDataFileObjects,
  UAbstractFileNamesObject,
  UYieldModelDataObject;

type

  TSumOutFileAgent = class(TAbstractFileAgent)
  public
    { Public declarations }
    //FInteger :TInteger;
    function ReadModelDataFromFile(AFileName:TAbstractModelFileName;  ADataObject: TDataFileObjects;
             AProgressFunction: TProgressUpdateFuntion): boolean; override;
    function WriteModelDataToFile(AFileName:TAbstractModelFileName;  ADataObject: TDataFileObjects;
             AProgressFunction: TProgressUpdateFuntion): boolean; override;
  end;


implementation

uses UUtilities,
     UErrorHandlingOperations;

{ TSumOutFileAgent }

function TSumOutFileAgent.ReadModelDataFromFile(AFileName: TAbstractModelFileName; ADataObject: TDataFileObjects;
         AProgressFunction: TProgressUpdateFuntion): boolean;
const OPNAME = 'TSumOutFileAgent.ReadModelDataFromFile';
begin
  Result := False;
  try
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TSumOutFileAgent.WriteModelDataToFile(AFileName: TAbstractModelFileName;ADataObject: TDataFileObjects;
         AProgressFunction: TProgressUpdateFuntion): boolean;
const OPNAME = 'TSumOutFileAgent.WriteModelDataToFile';
begin
  Result := False;
  try
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
