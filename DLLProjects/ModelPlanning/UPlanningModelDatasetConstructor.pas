//
//
//  UNIT      : Contains TPlanningModelDatasetConstructor Class
//  AUTHOR    : Grant Nyland (PDNA)
//  DATE      : 2002/03/20
//  COPYRIGHT : Copyright © 2002 DWAF
//
//
unit UPlanningModelDatasetConstructor;

interface

uses
  USQLDatabaseLayer,
  UAbstractObject;

type
  TPlanningModelDatasetConstructor = class(TAbstractDatasetConstructor)
  public
    function CreateDataset(ADataSetType: integer; var ADataSet: TAbstractModelDataset): boolean; override;
  end;

implementation

uses
  SysUtils,
  UDataSetType,
//  UViewPlanningDataDataset,
  UModelCalendarDataset,
  UErrorHandlingOperations;

function TPlanningModelDatasetConstructor.CreateDataSet(ADataSetType: integer; var ADataSet: TAbstractModelDataset): boolean;
const
  OPNAME = 'TPlanningModelDatasetConstructor.CreateDataSet';
begin
  Result := True;
  ADataSet := nil;
  try

    // Create the data set.
    case TDataSetType(ADataSetType) of
//      dtSelectViewData : ADataSet := TViewPlanningDataDataset.Create(FAppModules);
      dtModelCalendarSelect : ADataSet := TModelCalendarDataset.Create(FAppModules);
    else
      Result := False;
    end;

  // Handle all exceptions.
  except on E: Exception do HandleError(E, OPNAME); end;
end;


end.
