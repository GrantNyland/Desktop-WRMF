//
//
//  UNIT      : Contains TParameterDataLoadAgent Class
//  AUTHOR    : Dziedzi Ramulondi (Aravia)
//  DATE      : 2003/07/15
//  COPYRIGHT : Copyright © 2003 DWAF
//
//
unit UParameterDataLoadAgent;

interface

uses
  Classes,
  UReservoirData,
  UAbstractObject,
  UParameterData,
  UParameterDataSQLAgent;

type
  TParameterDataLoadAgent = class(TAbstractAppObject)
  protected
    FSQLAgent: TParameterDataSQLAgent;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
  public
    function ConstructData(AParamSetup: TParamSetup): boolean;
  end;

implementation

uses
  SysUtils,
  UDataSetType,
  UErrorHandlingOperations;

procedure TParameterDataLoadAgent.CreateMemberObjects;
const OPNAME = 'TParameterDataLoadAgent.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
    FSQLAgent := TParameterDataSQLAgent.Create(FAppModules);
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TParameterDataLoadAgent.DestroyMemberObjects;
const OPNAME = 'TParameterDataLoadAgent.DestroyMemberObjects';
begin
  try
    FreeAndNil(FSQLAgent);
    inherited DestroyMemberObjects;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TParameterDataLoadAgent.ConstructData(AParamSetup: TParamSetup): boolean;
const OPNAME = 'TParameterDataLoadAgent.ConstructData';
var
   LDataSet: TAbstractModelDataset;
begin
  Result := False;
  try
    AParamSetup.Initialise;
    // Loop for all the records in the table.
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      if Assigned(LDataSet) then
      begin
        LDataSet.SetSQL(FSQLAgent.GetParameterSQL);
        LDataSet.DataSet.Open;
        AParamSetup.LoadReferenceDataFromDataset(LDataSet);
        LDataSet.DataSet.Close;

        LDataSet.SetSQL(FSQLAgent.GetParamHeaderSQL);
        LDataSet.DataSet.Open;
        AParamSetup.LoadKeyGaugeDataFromDataset(LDataSet);
        LDataSet.DataSet.Close;

        LDataSet.SetSQL(FSQLAgent.GetParamMatrixSQL(1));
        LDataSet.DataSet.Open;
        AParamSetup.LoadMatrixBDataFromDataset(LDataSet);
        LDataSet.DataSet.Close;

        LDataSet.SetSQL(FSQLAgent.GetParamMatrixSQL(2));
        LDataSet.DataSet.Open;
        AParamSetup.LoadMatrixB0DataFromDataset(LDataSet);
        LDataSet.DataSet.Close;

        LDataSet.SetSQL(FSQLAgent.GetParamMatrixSQL(3));
        LDataSet.DataSet.Open;
        AParamSetup.LoadMatrixB1DataFromDataset(LDataSet);
        LDataSet.DataSet.Close;

        LDataSet.SetSQL(FSQLAgent.GetParamMatrixSQL(4));
        LDataSet.DataSet.Open;
        AParamSetup.LoadMatrixADataFromDataset(LDataSet);
        LDataSet.DataSet.Close;

        LDataSet.SetSQL(FSQLAgent.GetParamMatrixSQL(5));
        LDataSet.DataSet.Open;
        AParamSetup.LoadMatrixCDataFromDataset(LDataSet);
        LDataSet.DataSet.Close;
        Result := True;
      end;
    finally
      LDataSet.Free;
    end;

  // Handle exceptions.
  except on E: Exception do HandleErrorFunction(E, OPNAME, Result) end;
end;

end.
