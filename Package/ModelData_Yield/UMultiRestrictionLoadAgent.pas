unit UMultiRestrictionLoadAgent;

interface

uses
  Classes,
  UAbstractObject,
  UMultiResChannelCurtailmentData,
  UMultiRestrictionSQLAgent;

type
  TMultiRestrictionLoadAgent = class(TAbstractAppObject)
  private
  protected
    FSQLAgent: TMultiRestrictionSQLAgent;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
  public
    function ConstructData(AMultiRestriction : TMultiResChannelCurtailmentList): boolean;
    function LoadMultiRestriction(AMultiRestriction : TMultiResChannelCurtailmentList): boolean;
    function GetLastMultiRestrictionID : integer;
    function InsertMultiRestriction(ACurtail : TMultiResChannelCurtail) : boolean;
    function DeleteMultiRestriction(AIdentifier, AChannelNo, AReservoirNo: integer): boolean;


  end;

implementation

uses
  SysUtils,
  VoaimsCom_TLB,
  UDataSetType,
  UConstants,
  UErrorHandlingOperations,
  DB;

procedure TMultiRestrictionLoadAgent.CreateMemberObjects;
const OPNAME = 'TMultiRestrictionLoadAgent.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;

    FSQLAgent := TMultiRestrictionSQLAgent.Create(FAppModules);

  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TMultiRestrictionLoadAgent.DestroyMemberObjects;
const OPNAME = 'TMultiRestrictionLoadAgent.DestroyMemberObjects';
begin
  try
    FreeAndNil(FSQLAgent);
    inherited DestroyMemberObjects;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TMultiRestrictionLoadAgent.GetLastMultiRestrictionID: integer;
const OPNAME = 'TMultiRestrictionLoadAgent.DestroyMemberObjects';
var
  LDataSet : TAbstractModelDataset;
  LSQL : string;
begin
  Result := 0;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      if Assigned(LDataSet) then
      begin
        LSQL := ' SELECT MAX(Identifier) AS LastID FROM MultiResChannelCurtail ' +
                ' WHERE Model=' +QuotedStr(FAppModules.StudyArea.ModelCode) +
                ' AND StudyAreaName=' +QuotedStr(FAppModules.StudyArea.StudyAreaCode)+
                ' AND SubArea=' +QuotedStr(FAppModules.StudyArea.SubAreaCode)+
                ' AND Scenario= ' +QuotedStr(FAppModules.StudyArea.ScenarioCode);

        LDataSet.SetSQL(LSQL);
        LDataSet.DataSet.Open;
        result := LDataSet.DataSet.FieldByName('LastID').AsInteger;
      end;

    finally
      LDataSet.Free;
    end;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TMultiRestrictionLoadAgent.InsertMultiRestriction(ACurtail: TMultiResChannelCurtail): boolean;
const OPNAME = 'TMultiRestrictionLoadAgent.ConstructData';
var
  LDataSet : TAbstractModelDataset;
  LSQL     : string;
  LCommon     : string;
begin
  Result := False;
  try
    try
      FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    if Assigned(LDataSet) then
    begin
      LCommon := QuotedStr(FAppModules.StudyArea.ModelCode) + ',' +
             QuotedStr(FAppModules.StudyArea.StudyAreaCode) + ',' +
             QuotedStr(FAppModules.StudyArea.SubAreaCode) + ',' +
             QuotedStr(FAppModules.StudyArea.ScenarioCode) + ',' +
             IntToStr(ACurtail.Identifier) + ','+
             IntToStr(ACurtail.ChannelNo) + ','+
             IntToStr(ACurtail.ReservoirNo) + ',';

      LSQL := 'INSERT INTO MultiResChannelCurtail(Model,StudyAreaName, SubArea, Scenario, Identifier, ChannelNo,ReservoirNo,DecisionMonth, StartMonth)' +
             'Values(' + LCommon  +
             IntToStr(ACurtail.DecisionMonth) + ',' +
             IntToStr(ACurtail.StartMonth) +             ')';
      LDataSet.SetSQL(LSQL);
      LDataSet.ExecSQL();

      LSQL := 'INSERT INTO MultiResChannelElevation(Model, StudyAreaName, SubArea, Scenario,Identifier,ChannelNo,ReservoirNo,' +
                              'Elevation01, Elevation02, Elevation03, Elevation04, Elevation05,' +
                              'Elevation06, Elevation07, Elevation08, Elevation09,Elevation10)' +
             'Values('+ LCommon + FormatFloat('##000.00',ACurtail.ElevationByIndex[0]) + ','
             + FormatFloat('##000.00',ACurtail.ElevationByIndex[1]) + ','
             + FormatFloat('##000.00',ACurtail.ElevationByIndex[2]) + ','
             + FormatFloat('##000.00',ACurtail.ElevationByIndex[3]) + ','
             + FormatFloat('##000.00',ACurtail.ElevationByIndex[4]) + ','
             + FormatFloat('##000.00',ACurtail.ElevationByIndex[5]) + ','
             + FormatFloat('##000.00',ACurtail.ElevationByIndex[6]) + ','
             + FormatFloat('##000.00',ACurtail.ElevationByIndex[7]) + ','
             + FormatFloat('##000.00',ACurtail.ElevationByIndex[8]) + ','
             + FormatFloat('##000.00',ACurtail.ElevationByIndex[9]) +
             ')';
      LDataSet.SetSQL(LSQL);
      LDataSet.ExecSQL();

      LSQL := 'INSERT INTO MultiResChannelFactor(Model, StudyAreaName, SubArea, Scenario,Identifier,ChannelNo,ReservoirNo,' +
                              'Factor01, Factor02, Factor03, Factor04, Factor05,' +
                              'Factor06, Factor07, Factor08, Factor09,Factor10)' +
             'Values(' + LCommon + FormatFloat('##000.00',ACurtail.FactorByIndex[0]) + ','
             + FormatFloat('##000.00',ACurtail.FactorByIndex[1]) + ','
             + FormatFloat('##000.00',ACurtail.FactorByIndex[2]) + ','
             + FormatFloat('##000.00',ACurtail.FactorByIndex[3]) + ','
             + FormatFloat('##000.00',ACurtail.FactorByIndex[4]) + ','
             + FormatFloat('##000.00',ACurtail.FactorByIndex[5]) + ','
             + FormatFloat('##000.00',ACurtail.FactorByIndex[6]) + ','
             + FormatFloat('##000.00',ACurtail.FactorByIndex[7]) + ','
             + FormatFloat('##000.00',ACurtail.FactorByIndex[8]) + ','
             + FormatFloat('##000.00',ACurtail.FactorByIndex[9]) +
             ')';

      LDataSet.SetSQL(LSQL);
      LDataSet.ExecSQL();
      //LDataSet.DataSet.Close;

    end;
    finally
    LDataSet.Free
    end;
    Result := True;
  except on E : Exception do HandleErrorFunction(E, OPNAME, Result); end;
end;

function TMultiRestrictionLoadAgent.ConstructData(AMultiRestriction : TMultiResChannelCurtailmentList): boolean;
const OPNAME = 'TMultiRestrictionLoadAgent.ConstructData';
begin
  Result := True;
  try
    AMultiRestriction.Initialise;
    LoadMultiRestriction(AMultiRestriction);
  except on E : Exception do HandleErrorFunction(E, OPNAME, Result); end;
end;

function TMultiRestrictionLoadAgent.LoadMultiRestriction(AMultiRestriction : TMultiResChannelCurtailmentList): boolean;
const OPNAME = 'TMultiRestrictionLoadAgent.LoadMultiRestriction';
var
  LDataSet : TAbstractModelDataset;
  LIdentifier : integer;
  LDecisionMonth : integer;
  LStartMonth : integer;
  LChannelNo : integer;
  LReservoirNo : integer;
  LMultiResChannelCurtail : TMultiResChannelCurtail;
  LElevation : TElevationsArray;
  LFactor : TElevationsArray;
  LIndex : integer;
  LFieldName : string;
begin
  Result := False;
  try
    if AMultiRestriction <> nil then
    begin
      FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
      try
        if Assigned(LDataSet) then
        begin
          LDataSet.SetSQL(FSQLAgent.GetMultiResChannelDataSQL);
          LDataSet.DataSet.Open;
          SetLength(LElevation,10);
          SetLength(LFactor,10);
          while not LDataSet.DataSet.Eof do
          begin
            LIdentifier := LDataSet.DataSet.FieldByName('Identifier').AsInteger;
            LDecisionMonth := LDataSet.DataSet.FieldByName('DecisionMonth').AsInteger;
            LStartMonth := LDataSet.DataSet.FieldByName('StartMonth').AsInteger;
            LChannelNo := LDataSet.DataSet.FieldByName('ChannelNo').AsInteger;
            LReservoirNo := LDataSet.DataSet.FieldByName('ReservoirNo').AsInteger;
            for LIndex := Low(LElevation) to High(LElevation) do
            begin
              LFieldName := Format('%s%2.2d',['Elevation',LIndex+1]);
              LElevation[LIndex] := LDataSet.DataSet.FieldByName(LFieldName).AsFloat;
            end;

            for LIndex := Low(LFactor) to High(LFactor) do
            begin
              LFieldName := Format('%s%2.2d',['Factor',LIndex +1]);
              LFactor[LIndex] := LDataSet.DataSet.FieldByName(LFieldName).AsFloat;
            end;

            LMultiResChannelCurtail := AMultiRestriction.AddRestriction;
            LMultiResChannelCurtail.populate(LIdentifier,LChannelNo,LReservoirNo,LStartMonth,LDecisionMonth,
                                                LElevation,LFactor);

            LDataSet.DataSet.Next;

          end;

          LDataSet.DataSet.Close;
          Finalize(LElevation);
          Finalize(LFactor);
        end;
      finally
        LDataSet.Free;
       // LDetalDataSet.Free;
      end;
    end;
    Result := TRUE;
  except on E: Exception do HandleErrorFunction(E, OPNAME,Result) end;
end;

function TMultiRestrictionLoadAgent.DeleteMultiRestriction(AIdentifier, AChannelNo: Integer; AReservoirNo: Integer): boolean;
const OPNAME = 'TMultiRestrictionLoadAgent.DeleteMultiRestriction';
var
  LDataSet    : TAbstractModelDataset;
  LImportDate : TDateTime;
  //LCount : Integer;
  LSQL : string;
begin
  Result := FALSE;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      if Assigned(LDataSet) then
      begin
        LSQL := FSQLAgent.GetDeleteMultiRestrictionSQL(AIdentifier,AChannelNo,AReservoirNo);

        LDataSet.SetSQL(LSQL);
        LDataset.ExecSQL;

        LSQL := FSQLAgent.GetDeleteMultiRestrictionElevationSQL(AIdentifier,AChannelNo,AReservoirNo);

        LDataSet.SetSQL(LSQL);
        LDataset.ExecSQL;

        LSQL := FSQLAgent.GetDeleteMultiRestrictionFactorSQL(AIdentifier,AChannelNo,AReservoirNo);

        LDataSet.SetSQL(LSQL);
        LDataset.ExecSQL;

        LDataset.DataSet.Close;
        FAppModules.StudyArea.LastUpdateDate := Now();
        LImportDate := FAppModules.StudyArea.GetStudyImportDate;

        if LImportDate = NullDateTime then
          FAppModules.StudyArea.SetStudyImportDate(FAppModules.StudyArea.LastUpdateDate);
        Result := True;
     end;
   finally
     LDataSet.Free;
   end;
 except on E: Exception do HandleError(E, OPNAME) end;
end;


end.

