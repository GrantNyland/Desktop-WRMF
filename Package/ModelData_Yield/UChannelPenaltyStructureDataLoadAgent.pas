//
//
//  UNIT      : Contains TChannelPenaltyStructureDataLoadAgent Class
//  AUTHOR    : Titi Ngubane (Arivia)
//  DATE      : 2003/09/08
//  COPYRIGHT : Copyright © 2003 DWAF
//
//
unit UChannelPenaltyStructureDataLoadAgent;

interface

uses
  Classes,
  UAbstractObject,
  UChannelDataSQLAgent,
  VoaimsCom_TLB,
  UChannelData;

type
  TChannelPenaltyStructureDataLoadAgent = class(TAbstractAppObject)
  protected
    FSQLAgent: TChannelDataSQLAgent;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    function LoadChannelPenaltyStructureData (AChannelPenaltyList : TChannelPenaltyList): boolean;
    function LoadInflowPenaltyNo : integer;
  public
    procedure LoadChannelPenaltyValueContextData(AContextData: TStringList;
              APenaltyNumber,AFieldNameIdentifier: string);
    procedure LoadChannelPenaltyContextData (AContextData : TStringList;
                                             APenaltyID   : string);
    function PopulateChannelPenaltyStructures (AChannelList : TChannelList;
                                               APenaltyList : TChannelPenaltyList): boolean;
    function ConstructData (AChannelPenaltyList : TChannelPenaltyList) : boolean;
  end;

implementation

uses
  SysUtils,
  UDataSetType,
  UConstants,
  UErrorHandlingOperations;

procedure TChannelPenaltyStructureDataLoadAgent.CreateMemberObjects;
const OPNAME = 'TChannelPenaltyStructureDataLoadAgent.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
    FSQLAgent := TChannelDataSQLAgent.Create(FAppModules);
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TChannelPenaltyStructureDataLoadAgent.DestroyMemberObjects;
const OPNAME = 'TChannelPenaltyStructureDataLoadAgent.DestroyMemberObjects';
begin
  try
    FreeAndNil(FSQLAgent);
    inherited DestroyMemberObjects;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TChannelPenaltyStructureDataLoadAgent.ConstructData (AChannelPenaltyList : TChannelPenaltyList): boolean;
const OPNAME = 'TChannelPenaltyStructureDataLoadAgent.ConstructData';
begin
  Result := True;
  try
    LoadChannelPenaltyStructureData(AChannelPenaltyList);
  except on E : Exception do HandleErrorFunction(E, OPNAME, Result); end;
end;

function TChannelPenaltyStructureDataLoadAgent.LoadChannelPenaltyStructureData
                                  (AChannelPenaltyList : TChannelPenaltyList) : boolean;
const OPNAME = 'TChannelPenaltyStructureDataLoadAgent.LoadChannelPenaltyStructureData';
var
  LDataSet            : TAbstractModelDataset;
  LChannelPenalty     : TChannelPenalty;
  lIndex              : integer;
  lPenaltyValues      : TChannelPenaltyValuesArray;
  lPenaltyID          : integer;
  lPenaltyName        : string;
  LPenaltyValuesField : TAbstractFieldProperty;
begin
  Result := False;
  try
    LPenaltyValuesField := FAppModules.FieldProperties.FieldProperty('Penalty');
    if not Assigned(LPenaltyValuesField) then
      raise Exception.Create('Field (Penalty) not found in field properties');
      SetLength(lPenaltyValues, LPenaltyValuesField.ArrayLength);
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      if Assigned(LDataSet) then
      begin
        LDataSet.SetSQL(FSQLAgent.GetChannelPenaltyStructureDataSQL);
        LDataset.DataSet.Open;
        Result := True;
        while (not LDataset.DataSet.Eof) do
        begin
          LChannelPenalty := AChannelPenaltyList.NewChannelPenalty;
          lPenaltyID   := LDataSet.DataSet.FieldByName('PenaltyNumber').AsInteger;
          lPenaltyName := Trim(LDataSet.DataSet.FieldByName('PenaltyName').AsString);
          for LIndex := LPenaltyValuesField.ArrayLow to LPenaltyValuesField.ArrayHigh do
          begin
            if (LDataset.DataSet.FieldByName(Format('Penalty%2.2d',[LIndex])).IsNull) then
              lPenaltyValues[lIndex] := NullFloat
            else
              lPenaltyValues[lIndex] := LDataSet.DataSet.FieldByName(Format('Penalty%2.2d',[LIndex])).AsFloat;
          end;
          if (NOT LChannelPenalty.Populate(lPenaltyID, lPenaltyName, lPenaltyValues)) then
            AChannelPenaltyList.DeleteChannelPenaltyWithID(lPenaltyID);
          LDataset.DataSet.Next;
        end;
        LDataset.DataSet.Close;
        Result := True;
      end;
    finally
      LDataset.Free;
    end;
  except on E: Exception do HandleErrorFunction(E, OPNAME, Result) end;
end;

function TChannelPenaltyStructureDataLoadAgent.LoadInflowPenaltyNo : integer;
const OPNAME = 'TChannelPenaltyStructureDataLoadAgent.LoadInflowPenaltyNo';
var
  LDataSet : TAbstractModelDataset;
begin
  Result := 0;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      if Assigned(LDataSet) then
      begin

        LDataSet.SetSQL(FSQLAgent.GetChannelInflowPenaltyNoSQL);
        LDataset.DataSet.Open;
        if LDataSet.DataSet.FieldByName('InflowPenaltyNo').IsNull then
        begin
          LDataset.DataSet.Close;
          LDataSet.SetSQL(FSQLAgent.InsertChannelInflowPenaltyNoRecordSQL);
          LDataSet.ClearQueryParams();
          LDataSet.SetParams(['AModel'], [FAppModules.StudyArea.ModelCode]);
          LDataSet.SetParams(['AStudyAreaName'], [FAppModules.StudyArea.StudyAreaCode]);
          LDataSet.SetParams(['ASubArea'], [FAppModules.StudyArea.SubAreaCode]);
          LDataSet.SetParams(['AScenario'], [FAppModules.StudyArea.ScenarioCode]);
          LDataSet.SetParams(['InflowPenaltyNo'], [IntToStr(0)]);
          LDataSet.ExecSQL;
        end
        else
          Result := LDataSet.DataSet.FieldByName('InflowPenaltyNo').AsInteger;
      end;
    finally
      FreeAndNil(LDataset)
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TChannelPenaltyStructureDataLoadAgent.PopulateChannelPenaltyStructures
                                              (AChannelList : TChannelList;
                                               APenaltyList : TChannelPenaltyList): boolean;
const OPNAME = 'TChannelPenaltyStructureDataLoadAgent.PopulateChannelPenaltyStructures';
var
  LCount       : integer;
  LChannel     : TGeneralFlowChannel;
  LPenalty     : TChannelPenalty;
begin
  Result := False;
  try
    if Assigned(AChannelList) and Assigned(APenaltyList) then
    begin
      APenaltyList.InflowPenaltyNo := LoadInflowPenaltyNo;
      for LCount := 0 to AChannelList.ChannelCount -1 do
      begin
        LChannel := AChannelList.CastChannelByIndex[LCount];
        if Assigned(LChannel) then
        begin
          LPenalty := APenaltyList.CastChannelPenaltyByIdentifier[LChannel.ChannelPenaltyNumber];
          if Assigned(LPenalty) then
            LChannel.PopulateChannelPenalty(LPenalty);
        end;
      end;
      Result := True;
    end;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TChannelPenaltyStructureDataLoadAgent.LoadChannelPenaltyValueContextData(AContextData: TStringList;
              APenaltyNumber, AFieldNameIdentifier: string);
const OPNAME = 'TChannelPenaltyStructureDataLoadAgent.LoadChannelPenaltyValueContextData';
begin
  try
    FSQLAgent.LoadChannelPenaltyValueContextData(AContextData, APenaltyNumber, AFieldNameIdentifier);
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TChannelPenaltyStructureDataLoadAgent.LoadChannelPenaltyContextData
                                                     (AContextData : TStringList;
                                                     APenaltyID   : string);
const OPNAME = 'TChannelPenaltyStructureDataLoadAgent.LoadChannelPenaltyContextData';
begin
  try
    FSQLAgent.LoadChannelPenaltyContextData(AContextData, APenaltyID);
  except on E : Exception do HandleError(E,OPNAME); end;
end;

end.
