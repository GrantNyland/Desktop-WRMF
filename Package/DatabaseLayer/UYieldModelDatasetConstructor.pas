//
//
//  UNIT      : Contains TYieldModelDatasetConstructor Class
//  AUTHOR    : Grant Nyland (PDNA)
//  DATE      : 2002/03/20
//  COPYRIGHT : Copyright © 2002 DWAF
//
//
unit UYieldModelDatasetConstructor;

interface

uses
  USQLDatabaseLayer,
  UAbstractObject;

type
  TYieldModelDatasetConstructor = class(TAbstractDatasetConstructor)
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

function TYieldModelDatasetConstructor.CreateDataSet(ADataSetType: integer; var ADataSet: TAbstractModelDataset): boolean;
const
  OPNAME = 'TYieldModelDatasetConstructor.CreateDataSet';
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
