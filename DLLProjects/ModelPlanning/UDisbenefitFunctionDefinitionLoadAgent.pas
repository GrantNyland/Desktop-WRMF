//
//
//  UNIT      : Contains TDisbenefitFunctionDefinitionLoadAgent Class
//  AUTHOR    : Presley Mudau
//  DATE      : 2006/06/08
//  COPYRIGHT : Copyright © 2006 DWAF
//
//
unit UDisbenefitFunctionDefinitionLoadAgent;

interface

uses
  Classes,
  UAbstractObject,
  UChannelData,
  UDisbenefitFunctionData,
  UDisbenefitFunctionDefinitionSQLAgent;

type
  TDisbenefitFunctionDefinitionLoadAgent = class(TAbstractAppObject)
  private
  protected
    FSQLAgent: TDisbenefitFunctionDefinitionSQLAgent;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
  public
    function ConstructData(AChannelList: TChannelList): boolean;
    function LoadDisbenefitFunction(AChannelList: TChannelList): boolean;
  end;

implementation

uses
  SysUtils,
  VoaimsCom_TLB,
  UDataSetType,
  UConstants,
  UErrorHandlingOperations,
  DB;

procedure TDisbenefitFunctionDefinitionLoadAgent.CreateMemberObjects;
const OPNAME = 'TDisbenefitFunctionDefinitionLoadAgent.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
    FSQLAgent := TDisbenefitFunctionDefinitionSQLAgent.Create(FAppModules);
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TDisbenefitFunctionDefinitionLoadAgent.DestroyMemberObjects;
const OPNAME = 'TDisbenefitFunctionDefinitionLoadAgent.DestroyMemberObjects';
begin
  try
    FreeAndNil(FSQLAgent);
    inherited DestroyMemberObjects;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TDisbenefitFunctionDefinitionLoadAgent.ConstructData(AChannelList : TChannelList): boolean;
const OPNAME = 'TDisbenefitFunctionDefinitionLoadAgent.ConstructData';
begin
  Result := True;
  try
    AChannelList.Initialise;
    LoadDisbenefitFunction(AChannelList);
  except on E : Exception do HandleErrorFunction(E, OPNAME, Result); end;
end;

function TDisbenefitFunctionDefinitionLoadAgent.LoadDisbenefitFunction(AChannelList: TChannelList): boolean;
const OPNAME = 'TDisbenefitFunctionDefinitionLoadAgent.LoadDisbenefitFunction';
var
  LDataSet                     : TAbstractModelDataset;
  lChannel                     : TGeneralFlowChannel;
  LDisbenefitFunctionData      : TDisbenefitFunctionData;
  //LIdentifier                  : integer;
  LEquationDisbenefitX         : double;
  LEquationDisbenefitY         : double;
  LEquationDisbenefitNonSupply : double;
  LEquationDisbenefitCost      : double;
  LFieldName,
  LEscalationRate              : string;
  LYearActive,
  LMonthActive,
  LYearObsolete,
  LMonthObsolete,
  LWQConstraint,
  LIndex,
  LChannelNumber               : integer;
  LTDSConcentrationFactors     : array[1..4] of double;
begin
  Result := False;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      if Assigned(LDataSet) then
      begin
        LDataSet.SetSQL(FSQLAgent.GetDisbenefitDefinitionDataSQL);
        LDataSet.DataSet.Open;
        Result := TRUE;
        while not LDataSet.DataSet.Eof do
        begin
          LChannelNumber               := LDataSet.DataSet.FieldbyName('ChannelNumber').AsInteger;
          //LIdentifier                  := LDataset.DataSet.FieldByName('Identifier').AsInteger;
          LEquationDisbenefitX         := LDataset.DataSet.FieldByName('EquationDisbenefitX').AsFloat;
          LEquationDisbenefitY         := LDataset.DataSet.FieldByName('EquationDisbenefitY').AsFloat;
          LEquationDisbenefitCost      := LDataset.DataSet.FieldByName('EquationDisbenefitCost').AsFloat;
          LEquationDisbenefitNonSupply := LDataSet.DataSet.FieldByName('EquationDisbenefitNonSupply').AsFloat;
          LEscalationRate              := Trim(LDataset.DataSet.FieldByName('EscalationRate').AsString);

          LYearActive               := LDataset.DataSet.FieldByName('YearActive').AsInteger;
          LMonthActive              := LDataset.DataSet.FieldByName('MonthActive').AsInteger;
          LYearObsolete             := LDataset.DataSet.FieldByName('YearObsolete').AsInteger;
          LMonthObsolete            := LDataset.DataSet.FieldByName('MonthObsolete').AsInteger;
          LWQConstraint             := LDataset.DataSet.FieldByName('WQConstraint').AsInteger;

          for LIndex := Low(LTDSConcentrationFactors) to High(LTDSConcentrationFactors) do
          begin
            LFieldName := Format('%s%2.2d',['TDSConcentration',LIndex]);
            LTDSConcentrationFactors[LIndex] := LDataset.DataSet.FieldByName(LFieldName).AsInteger
          end;

          lChannel                     := AChannelList.CastChannelByChannelNumber[LChannelNumber];

          if Assigned(lChannel) then
          begin
            LDisbenefitFunctionData := lChannel.CreateDisbenefitFunction;
            if Assigned(LDisbenefitFunctionData) then
            begin
              LDisbenefitFunctionData.Populate(LChannelNumber,LEquationDisbenefitX,
                                               LEquationDisbenefitY, LEquationDisbenefitNonSupply,
                                               LEquationDisbenefitCost, LEscalationRate, LYearActive,
                                               LMonthActive,LYearObsolete,LMonthObsolete,LWQConstraint,LTDSConcentrationFactors) ;
            end;
          end;
          LDataSet.DataSet.Next;
        end;
        Result := TRUE;
        LDataSet.DataSet.Close;
      end;
    finally
      LDataSet.Free;
    end;
  except on E: Exception do HandleErrorFunction(E, OPNAME,Result) end;
end;

end.

