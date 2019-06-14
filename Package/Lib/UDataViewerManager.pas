//
//
//  UNIT      : Contains TDataViewerManager Class
//  AUTHOR    : Grant Nyland (PDNA)
//  DATE      : 2002/02/18
//  COPYRIGHT : Copyright © 2002 DWAF
//
//
unit UDataViewerManager;

interface

uses
  UAbstractObject,
  UTabSheetManager;

type
  TDataViewerManager = class(TTabSheetManager)
  public
    function Initialise: boolean; override;
  end;

  TAbstractFileEditManager = class(TTabSheetManager)
  public
    procedure SetOnFileSave(AOnFileSave:TOnFileSave); virtual; abstract;
  end;

implementation

uses
  SysUtils,
  UDataSetType,
  UDataViewerSheet,
  UGridActionObject,
  UErrorHandlingOperations;

function TDataViewerManager.Initialise: boolean;
const OPNAME = 'TDataViewerManager.Initialise';
var LDataSet: TAbstractModelDataset;
begin
  Result := False;
  try
    try
      if Assigned(FTabSheet) then
      begin
        FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
        if Assigned(LDataSet) and Assigned(LDataSet.DataSet()) then
          TDataViewerSheet(FTabSheet).CurrentDataset := LDataSet;
      end;
    finally
      inherited Initialise;
    end;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

end.
