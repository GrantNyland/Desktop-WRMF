//
//
//  UNIT      : Contains TYRCModelDatasetConstructor Class
//  AUTHOR    : Dziedzi Ramulondi (PDNA)
//  DATE      : 2004/07/12
//  COPYRIGHT : Copyright © 2004 DWAF
//
//
unit UYRCModelDatasetConstructor;

interface

uses
  USQLDatabaseLayer,
  UAbstractObject;

type
  TYRCModelDatasetConstructor = class(TAbstractDatasetConstructor)
  public
    function CreateDataset(ADataSetType: integer; var ADataSet: TAbstractModelDataset): boolean; override;
  end;

implementation

uses
  SysUtils,
  UDataSetType,
//  UViewYieldDataDataset,
  UModelCalendarDataset,
  UErrorHandlingOperations;

function TYRCModelDatasetConstructor.CreateDataSet(ADataSetType: integer; var ADataSet: TAbstractModelDataset): boolean;
const
  OPNAME = 'TYRCModelDatasetConstructor.CreateDataSet';
begin
  Result := True;
  ADataSet := nil;
  try

    // Create the data set.
    case TDataSetType(ADataSetType) of
//      dtSelectViewData : ADataSet := TViewYieldDataDataset.Create(FAppModules);
      dtModelCalendarSelect : ADataSet := TModelCalendarDataset.Create(FAppModules);
    else
      Result := False;
    end;

  // Handle all exceptions.
  except on E: Exception do HandleError(E, OPNAME); end;
end;


end.
