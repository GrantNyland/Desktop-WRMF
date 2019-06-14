//
//
//  UNIT      : Contains TViewDataDatasetConstructor Class
//  AUTHOR    : Grant Nyland (PDNA)
//  DATE      : 2002/03/20
//  COPYRIGHT : Copyright © 2002 DWAF
//
//
unit UViewDataDatasetConstructor;

interface

uses
  USQLDatabaseLayer,
  UAbstractObject;

type
  TViewDataDatasetConstructor = class(TAbstractDatasetConstructor)
  public
    function CreateDataset(ADataSetType: integer; var ADataSet: TAbstractModelDataset): boolean; override;
  end;

implementation

uses
  SysUtils,
  UDataSetType,
  UViewDataDataset,
  UErrorHandlingOperations;

function TViewDataDatasetConstructor.CreateDataSet(ADataSetType: integer; var ADataSet: TAbstractModelDataset): boolean;
const
  OPNAME = 'TViewDataDatasetConstructor.CreateDataSet';
begin
  Result := True;
  ADataSet := nil;
  try

    // Create the data set.
    case TDataSetType(ADataSetType) of
      dtViewDataSets  : ADataSet := TViewDataSetsDataset.Create(FAppModules);
      dtViewDataJumps : ADataSet := TViewDataJumpsDataset.Create(FAppModules);
      dtViewDataNodes : ADataSet := TViewDataNodesDataset.Create(FAppModules);
    else
      Result := False;
    end;

  // Handle all exceptions.
  except on E: Exception do HandleError(E, OPNAME); end;
end;


end.
