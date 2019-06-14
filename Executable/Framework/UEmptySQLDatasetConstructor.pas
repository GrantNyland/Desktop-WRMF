//
//
//  UNIT      : Contains TEmptySQLDatasetConstructor Class
//  AUTHOR    : Grant Nyland (PDNA)
//  DATE      : 2002/03/20
//  COPYRIGHT : Copyright © 2002 DWAF
//
//
unit UEmptySQLDatasetConstructor;

interface

uses
  USQLDatabaseLayer,
  UAbstractObject;

type
  TEmptySQLDatasetConstructor = class(TAbstractDatasetConstructor)
  public
    function CreateDataset(ADataSetType: integer; var ADataSet: TAbstractModelDataset): boolean; override;
  end;

implementation

uses
  SysUtils,
  UDataSetType,
  UEmptySQLDataSet,
  UErrorHandlingOperations;

function TEmptySQLDatasetConstructor.CreateDataSet(ADataSetType: integer; var ADataSet: TAbstractModelDataset): boolean;
const
  OPNAME = 'TEmptySQLDatasetConstructor.CreateDataSet';
begin
  Result := True;
  ADataSet := nil;
  try

    // Create the data set.
    case TDataSetType(ADataSetType) of
      dtExecSQL : ADataSet := TEmptySQLDataset.Create(FAppModules);
    else
      Result := False;
    end;

  // Handle all exceptions.
  except on E: Exception do HandleError(E, OPNAME); end;
end;


end.
