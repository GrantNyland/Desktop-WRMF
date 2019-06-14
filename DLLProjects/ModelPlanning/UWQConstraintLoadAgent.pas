//
//
//  UNIT      : Contains TWQConstraintLoadAgent Class
//  AUTHOR    : Sam Dhlamini(Cornastone)
//  DATE      : 19/06/2004
//  COPYRIGHT : Copyright © 2004 DWAF
//
//

unit UWQConstraintLoadAgent;

interface
uses
  Classes,
  UAbstractObject,
  UWQConstraintSQLAgent,
  UWQConstraintData,
  VoaimsCom_TLB,
  UChannelData;
type
  TWQConstraintLoadAgent = class(TAbstractAppObject)
  protected
    FSQLAgent: TWQConstraintSQLAgent;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
  public
    function LoadWQConstriantData(AWQConstriantDataObject:TWQConstriantData) : boolean;
  end;

implementation
uses
  SysUtils,
  UDataSetType,
  UConstants,
  UErrorHandlingOperations, DB;

{ TWQConstraintLoadAgent }

procedure TWQConstraintLoadAgent.CreateMemberObjects;
const OPNAME = 'TWQConstraintLoadAgent.CreateMemberObjects';
begin
  inherited CreateMemberObjects;
  try
    FSQLAgent := TWQConstraintSQLAgent.Create(FAppModules);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TWQConstraintLoadAgent.DestroyMemberObjects;
const OPNAME = 'TWQConstraintLoadAgent.DestroyMemberObjects';
begin
  try
    FreeAndNil(FSQLAgent);
    inherited DestroyMemberObjects;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;


function TWQConstraintLoadAgent.LoadWQConstriantData(AWQConstriantDataObject:TWQConstriantData) : boolean;
const OPNAME = 'TWQConstraintLoadAgent.LoadWQConstriantData';
var

  LWQConstriantsChannel : TWQConstriantsChannel;
  LMinMaxUpperBoundChannel : TMinMaxUpperBoundChannel;
  LDataSet : TAbstractModelDataset;
  LSQL : string;
  LIdentifier : integer;
  LChannel : integer;
  LWQTarget : double;
 // LBlendingRefChannelCount : integer;
  LReservoirRef : integer;
  LWQConType : integer;
  LReferenceChannels : string;
  LReferenceChannelFactors : string;
  LSlopeLimit : integer;
  LEstimatedRelease  : string;
  LConcentration  : string;
 // LReferenceChannelCount : integer;
  LRefChannels : string;
begin
  Result := False;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      if Assigned(LDataSet) then
      begin
        LSQL := FSQLAgent.GetBoundChannelSQL;
        LDataSet.SetSQL(LSQL);
        LDataset.DataSet.Open;
        while (not LDataset.DataSet.EOF) do
        begin
          LIdentifier := LDataset.DataSet.FieldByName('Identifier').AsInteger;
          LChannel := LDataset.DataSet.FieldByName('ChannelNumber').AsInteger;
         // LReferenceChannelCount := LDataset.DataSet.FieldByName('ReferenceChannelCount').AsInteger;
          LRefChannels := LDataset.DataSet.FieldByName('ReferenceChannels').AsString;
          LMinMaxUpperBoundChannel := AWQConstriantDataObject.CreateMinMaxUpperBoundChannel;
          LMinMaxUpperBoundChannel.Populate(LIdentifier, LChannel, LRefChannels);
          LDataset.DataSet.Next;
        end;
        LDataset.DataSet.Close;

        LSQL := FSQLAgent.GetMinMaxWQConstrainSQL;
        LDataSet.SetSQL(LSQL);
        LDataset.DataSet.Open;
        while (not LDataset.DataSet.EOF) do
        begin
          LIdentifier := LDataset.DataSet.FieldByName('Identifier').AsInteger;
          LChannel := LDataset.DataSet.FieldByName('ChannelNumber').AsInteger;
          LWQTarget := LDataset.DataSet.FieldByName('WQTarget').AsFloat;
          //LBlendingRefChannelCount := LDataset.DataSet.FieldByName('BlendingRefChannelCount').AsInteger;
          LReservoirRef := LDataset.DataSet.FieldByName('ReservoirRef').AsInteger;
          LWQConType := LDataset.DataSet.FieldByName('WQConType').AsInteger;
          LReferenceChannels := LDataset.DataSet.FieldByName('ReferenceChannels').AsString;
          LReferenceChannelFactors := LDataset.DataSet.FieldByName('ReferenceChannelFactors').AsString;
          LSlopeLimit := LDataset.DataSet.FieldByName('SlopeLimit').AsInteger;
          LEstimatedRelease := LDataset.DataSet.FieldByName('EstimatedRelease').AsString;
          LConcentration := LDataset.DataSet.FieldByName('Concentration').AsString;
          LWQConstriantsChannel := AWQConstriantDataObject.CreateWQConstraintsChannels;
          LWQConstriantsChannel.Populate(LIdentifier,LChannel,LWQTarget,LReservoirRef,
                                         LWQConType,LReferenceChannels,LReferenceChannelFactors,LSlopeLimit,
                                         LEstimatedRelease,LConcentration);
          LDataset.DataSet.Next;
        end;
        Result := True;
      end;
    finally
      FreeandNil(LDataSet);
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

end.
