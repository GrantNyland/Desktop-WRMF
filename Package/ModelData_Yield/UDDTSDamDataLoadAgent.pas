//
//
//  UNIT      : Contains TDDTSDamDataLoadAgent Class
//  AUTHOR    : Sam Dhlamini (bcx)
//  DATE      : 07/04/2014
//  COPYRIGHT : Copyright © 2004 DWAF
//
//
unit UDDTSDamDataLoadAgent;

interface

uses
  Classes,
  Contnrs,
  VoaimsCom_TLB,

  UFilesActionAbstractManager,
  UFileNames,
  UFileNameConstants,

  UFilePathsDatabaseAgent,
  UDDTSData,
  UDDTSDamDataSQLAgent,
  UUtilities,
  UNetworkFeaturesSQLAgent,
//  UNetworkFeaturesLoadAgent,
  UDataFileObjects,
  UReservoirDataSQLAgent,
  UReservoirData,
  UAbstractObject;

type
  TDDTSDamDataLoadAgent = class(TAbstractAppObject)
  protected
    FSQLAgent: TDDTSDamDataSQLAgent;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    function DamDailyDataExist : boolean;

  public
    function CreateReservoirData(ADDTSDamDataList : TDDTSDamDataList) : TReservoirData;
    function LoadDDTSDamDataLoadAgent(ADDTSDamDataList : TDDTSDamDataList) : boolean;
    function CreateDamDailyData(AIDentifier : integer) : boolean;
    function DeleteDamDailyData(AIDentifier : integer) : boolean;
  end;
implementation
uses
  VCL.Controls,
  SysUtils,
  VCL.Dialogs,
  UDataSetType,
  UConstants,
  UReservoirDataLoadAgent,
  UErrorHandlingOperations, DB, UAbstractComponent;



procedure TDDTSDamDataLoadAgent.CreateMemberObjects;
const OPNAME = 'TDDTSDamDataLoadAgent.CreateMemberObjects';
begin
  inherited CreateMemberObjects;
  try
     FSQLAgent := TDDTSDamDataSQLAgent.Create(FAppModules);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDDTSDamDataLoadAgent.CreateReservoirData(ADDTSDamDataList: TDDTSDamDataList): TReservoirData;
const OPNAME = 'TDDTSDamDataLoadAgent.CreateReservoirData';
var
  LReservoirData : IReservoirData;
begin
  Result := nil;
  try
    LReservoirData := nil;
    if (ADDTSDamDataList <> nil) and (ADDTSDamDataList.CastReservoirList <> nil)then
      LReservoirData := ADDTSDamDataList.CastReservoirList.CreateReservoir(ntReservoir);
    Result := TReservoirData(LReservoirData);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDDTSDamDataLoadAgent.CreateDamDailyData(AIDentifier : integer): boolean;
const OPNAME = 'TDDTSDamDataLoadAgent.CreateDamDailyData';
var
  LDataSet : TAbstractModelDataset;
  LSQL : string;
begin
  Result := False;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      if Assigned(LDataSet) then
      begin
        LDataset.DataSet.Close;
        LSQL := FSQLAgent.InsertDamDataSQL(AIDentifier);
        LDataSet.SetSQL(LSQL);
        LDataset.ExecSQL;

        LDataset.DataSet.Close;
        LSQL := FSQLAgent.InsertInputMinMaxSQL(AIDentifier);
        LDataSet.SetSQL(LSQL);
        LDataset.ExecSQL;

      end;
    finally
        LDataset.Free;
    end;
    Result := True;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDDTSDamDataLoadAgent.DeleteDamDailyData(AIDentifier: integer): boolean;
const OPNAME = 'TDDTSDamDataLoadAgent.CreateDamDailyData';
var
  LDataSet : TAbstractModelDataset;
  LSQL : string;
begin
  Result := False;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      if Assigned(LDataSet) then
      begin
        LDataset.DataSet.Close;
        LSQL := FSQLAgent.DeleteDamDataSQL(AIDentifier);
        LDataSet.SetSQL(LSQL);
        LDataset.ExecSQL;

        LDataset.DataSet.Close;
        LSQL := FSQLAgent.DeleteInputMinMaxSQL(AIDentifier);
        LDataSet.SetSQL(LSQL);
        LDataset.ExecSQL;

      end;
      Result := True;
    finally
      LDataset.Free;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDDTSDamDataLoadAgent.DamDailyDataExist: boolean;
const OPNAME = 'TDDTSDamDataLoadAgent.DamDailyDataExist';
var
  LDataSet : TAbstractModelDataset;
begin
  Result := False;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      if Assigned(LDataSet) then
      begin
        LDataSet.SetSQL(FSQLAgent.GetDDTSDamDataSQL(1));
        LDataset.DataSet.Open;
        LDataset.DataSet.Last;
        LDataset.DataSet.First;



        Result :=  (LDataset.DataSet.RecordCount>0);
      end;
    finally
        LDataset.Free;
    end;

  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TDDTSDamDataLoadAgent.DestroyMemberObjects;
const OPNAME = 'TDDTSDamDataLoadAgent.DestroyMemberObjects';
begin
  inherited DestroyMemberObjects;
  try
    FreeAndNil(FSQLAgent);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDDTSDamDataLoadAgent.LoadDDTSDamDataLoadAgent(ADDTSDamDataList : TDDTSDamDataList) : boolean;
const OPNAME = 'TDDTSDamDataLoadAgent.LoadDDTSDamDataLoadAgent';
var
  LDDTSDetailData : TDDTSDetailData;
  LDataSet : TAbstractModelDataset;
  LReservoirDataList : TReservoirDataList;
  LReservoirData : IReservoirData;
  LLoadAgent   : TReservoirDataLoadAgent;
  LDDTSInputData : TDDTSInputData;
begin
  Result := False;
  try
    if ADDTSDamDataList <> nil then
    begin
      FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
      LLoadAgent := TReservoirDataLoadAgent.Create(FAppModules);

      //ADDTSDamDataList.CastReservoirList.

      if (not DamDailyDataExist) then
        CreateDamDailyData(1);

      LReservoirData := nil;
      ADDTSDamDataList.Initialise;
      if LLoadAgent.ConstructData(ADDTSDamDataList.CastReservoirList,nil) then
      begin

        try
          LReservoirDataList := ADDTSDamDataList.CastReservoirList;
          if LReservoirDataList <> nil then
          begin
            LReservoirData :=  LReservoirDataList.Get_ReservoirByIdentifier(1);
            if LReservoirData = nil then
              LReservoirData := CreateReservoirData(ADDTSDamDataList);

            if LReservoirData <> nil then
            begin

              if Assigned(LDataSet) then
              begin
                LDataSet.SetSQL(FSQLAgent.GetDDTSDamDataSQL(LReservoirData.ReservoirConfigurationData.RecordIdentifier));
                LDataset.DataSet.Open;
                LDDTSDetailData := ADDTSDamDataList.AddDDTSDetailData;
                LDDTSDetailData.populate(LReservoirData.ReservoirConfigurationData.ReservoirName,
                                           LDataset.DataSet.FieldByName('Identifier').AsInteger,
                                           LDataset.DataSet.FieldByName('RunoffScaleFactor').AsFloat,
                                           LDataset.DataSet.FieldByName('OtherInflowScaleFactor').AsFloat,
                                           LDataset.DataSet.FieldByName('EWRScaleFactor').AsFloat,
                                           LDataset.DataSet.FieldByName('TargetDraft').AsFloat,
                                           LDataset.DataSet.FieldByName('DSRequirment').AsFloat,
                                           LDataset.DataSet.FieldByName('DSPercRelease').AsFloat,
                                           LDataset.DataSet.FieldByName('SpillPercRelease').AsFloat,
                                           LDataset.DataSet.FieldByName('EWRPercRelease').AsFloat,
                                           LDataset.DataSet.FieldByName('ImportHeadlines').AsString);

                LDataset.DataSet.Close;
                LDataSet.SetSQL(FSQLAgent.GetDDTSInputMinMaxSQL(LReservoirData.ReservoirConfigurationData.RecordIdentifier));
                LDataset.DataSet.Open;
                LDDTSDetailData.populateMinMax(
                                           LDataset.DataSet.FieldByName('MinRunoff').AsFloat,
                                           LDataset.DataSet.FieldByName('MaxRunoff').AsFloat,
                                           LDataset.DataSet.FieldByName('MinOtherInflow').AsFloat,
                                           LDataset.DataSet.FieldByName('MaxOtherInflow').AsFloat,
                                           LDataset.DataSet.FieldByName('MinRainfall').AsFloat,
                                           LDataset.DataSet.FieldByName('MaxRainfall').AsFloat,
                                           LDataset.DataSet.FieldByName('MinEvaporation').AsFloat,
                                           LDataset.DataSet.FieldByName('MaxEvaporation').AsFloat,
                                           LDataset.DataSet.FieldByName('MinIncreamentalRunoff').AsFloat,
                                           LDataset.DataSet.FieldByName('MaxIncreamentalRunoff').AsFloat,
                                           LDataset.DataSet.FieldByName('MinEWR').AsFloat,
                                           LDataset.DataSet.FieldByName('MaxEWR').AsFloat);


                LDataset.DataSet.Close;
                LDataSet.SetSQL(FSQLAgent.GetDDTSInputData);
                LDataset.DataSet.Open;
                while (not LDataset.DataSet.EOF) do
                begin
                  LDDTSInputData := LDDTSDetailData.AddDDTSInputData;
                  LDDTSInputData.Populate(LDataset.DataSet.FieldByName('RowID').AsInteger,
                                          LDataset.DataSet.FieldByName('DataDate').AsDateTime,
                                          LDataset.DataSet.FieldByName('Runoff').AsFloat,
                                          LDataset.DataSet.FieldByName('OtherInflow').AsFloat,
                                          LDataset.DataSet.FieldByName('Rainfall').AsFloat,
                                          LDataset.DataSet.FieldByName('Evaporation').AsFloat,
                                          LDataset.DataSet.FieldByName('IncreamentalRunoff').AsFloat,
                                          LDataset.DataSet.FieldByName('EWR').AsFloat);
                  LDataset.DataSet.Next;
                end;


              end;
            end;
          end;
        finally
          LDataset.Free;
          FreeAndNil(LLoadAgent);
        end;
      end;
      Result := True;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

end.

