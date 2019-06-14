//
//
//  UNIT      : Contains THydrologyFileTypeDataSetConstructor Class
//  AUTHOR    : Dziedzi Ramulondi (PDNA)
//  DATE      : 2002/05/10
//  COPYRIGHT : Copyright © 2002 DWAF
//
//
unit UHydrologyFileTypeDataSetConstructor;

interface

uses
  USQLDatabaseLayer,
  UAbstractObject;

type
  THydrologyFileTypeDataSetConstructor = class(TAbstractDatasetConstructor)
  public
    function CreateDataset(ADataSetType: integer; var ADataSet: TAbstractModelDataset): boolean; override;
  end;

implementation

uses
  SysUtils,
  UDataSetType,
  UHydrologyFileTypeDataset,
  UErrorHandlingOperations;

function THydrologyFileTypeDataSetConstructor.CreateDataSet(ADataSetType: integer; var ADataSet: TAbstractModelDataset): boolean;
const
  OPNAME = 'THydrologyFileTypeDataSetConstructor.CreateDataSet';
begin
  Result := True;
  ADataSet := nil;
  try

    // Create the data set.
    case TDataSetType(ADataSetType) of
      dtHydrologyFileType   : ADataSet := THydrologyFileTypeDataset.Create(FAppModules);
    else
      Result := False;
    end;

  // Handle all exceptions.
  except on E: Exception do HandleError(E, OPNAME); end;
end;


end.
