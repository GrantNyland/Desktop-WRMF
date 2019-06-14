//
//
//  UNIT      : Contains TYieldModelDataSQLAgent Class
//  AUTHOR    : Titi Ngubane (Arivia)
//  DATE      : 2003/07/10
//  COPYRIGHT : Copyright © 2003 DWAF
//
//
unit UYieldModelDataSQLAgent;

interface

uses
  Classes,
  UAbstractObject;

type
  TYieldModelDataSQLAgent = class(TAbstractSQLAgent)
  protected
    function GetScenarioWhereClause: string;
  public
    function StudyDataIsEmpty: boolean;
    function ResetYieldModelData: boolean;
  end;

implementation

uses
  SysUtils,
  UDataSetType,
  UErrorHandlingOperations;

function TYieldModelDataSQLAgent.GetScenarioWhereClause: string;
const OPNAME = 'TYieldModelDataSQLAgent.GetScenarioWhereClause';
begin
  Result := '';
  try
    Result :=
      ' (Model         = ' + QuotedStr(FAppModules.StudyArea.ModelCode)     + ') AND ' +
      ' (StudyAreaName = ' + QuotedStr(FAppModules.StudyArea.StudyAreaCode) + ') AND ' +
      ' (SubArea       = ' + QuotedStr(FAppModules.StudyArea.SubAreaCode)   + ') AND ' +
      ' (Scenario      = ' + QuotedStr(FAppModules.StudyArea.ScenarioCode)  + ') ';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldModelDataSQLAgent.StudyDataIsEmpty: boolean;
const OPNAME = 'TYieldModelDataSQLAgent.StudyDataIsEmpty';
var
  LDataSet: TAbstractModelDataset;
  LSQL: string;
  LCount: integer;
begin
  Result := False;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      if Assigned(LDataSet) then
      begin
        LCount := 0;

        LSQL := 'SELECT COUNT(*) AS RecordsCount FROM RunTitle WHERE ' + GetScenarioWhereClause;
        LDataSet.DataSet.Close;
        LDataSet.SetSQL(LSQL);
        LDataset.SetReadOnly(True);
        LDataset.DataSet.Open;
        LCount := LCount + LDataset.Dataset.FieldByName('RecordsCount').AsInteger;
        if(LCount > 0) then Exit;

        LSQL := 'SELECT COUNT(*) AS RecordsCount FROM AnlySequences WHERE ' + GetScenarioWhereClause;
        LDataSet.DataSet.Close;
        LDataSet.SetSQL(LSQL);
        LDataset.SetReadOnly(True);
        LDataset.DataSet.Open;
        LCount := LCount + LDataset.Dataset.FieldByName('RecordsCount').AsInteger;
        if(LCount > 0) then Exit;

        LSQL := 'SELECT COUNT(*) AS RecordsCount FROM DaysPerMonth WHERE ' + GetScenarioWhereClause;
        LDataSet.DataSet.Close;
        LDataSet.SetSQL(LSQL);
        LDataset.SetReadOnly(True);
        LDataset.DataSet.Open;
        LCount := LCount + LDataset.Dataset.FieldByName('RecordsCount').AsInteger;
        if(LCount > 0) then Exit;

        LSQL := 'SELECT COUNT(*) AS RecordsCount FROM MaxYield WHERE ' + GetScenarioWhereClause;
        LDataSet.DataSet.Close;
        LDataSet.SetSQL(LSQL);
        LDataset.SetReadOnly(True);
        LDataset.DataSet.Open;
        LCount := LCount + LDataset.Dataset.FieldByName('RecordsCount').AsInteger;
        if(LCount > 0) then Exit;

        LSQL := 'SELECT COUNT(*) AS RecordsCount FROM MonthNames WHERE ' + GetScenarioWhereClause;
        LDataSet.DataSet.Close;
        LDataSet.SetSQL(LSQL);
        LDataset.SetReadOnly(True);
        LDataset.DataSet.Open;
        LCount := LCount + LDataset.Dataset.FieldByName('RecordsCount').AsInteger;
        if(LCount > 0) then Exit;

        LSQL := 'SELECT COUNT(*) AS RecordsCount FROM RunParameters WHERE ' + GetScenarioWhereClause;
        LDataSet.DataSet.Close;
        LDataSet.SetSQL(LSQL);
        LDataset.SetReadOnly(True);
        LDataset.DataSet.Open;
        LCount := LCount + LDataset.Dataset.FieldByName('RecordsCount').AsInteger;
        if(LCount > 0) then Exit;

        LSQL := 'SELECT COUNT(*) AS RecordsCount FROM TargetPower WHERE ' + GetScenarioWhereClause;
        LDataSet.DataSet.Close;
        LDataSet.SetSQL(LSQL);
        LDataset.SetReadOnly(True);
        LDataset.DataSet.Open;
        LCount := LCount + LDataset.Dataset.FieldByName('RecordsCount').AsInteger;
        if(LCount > 0) then Exit;

        LSQL := 'SELECT COUNT(*) AS RecordsCount FROM TargetYield WHERE ' + GetScenarioWhereClause;
        LDataSet.DataSet.Close;
        LDataset.SetReadOnly(True);
        LDataSet.SetSQL(LSQL);
        LDataset.DataSet.Open;
        LCount := LCount + LDataset.Dataset.FieldByName('RecordsCount').AsInteger;


        Result := (LCount = 0);
      end;
    finally
      LDataset.Free;
    end;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldModelDataSQLAgent.ResetYieldModelData: boolean;
const OPNAME = 'TYieldModelDataSQLAgent.ResetYieldModelData';
      RunTitleFields      = 'Model,StudyAreaName,SubArea,Scenario,'+
                            'Title1,Title2,Title3';
      RunParametersFields = 'Model,StudyAreaName,SubArea,Scenario,'+
                            'NumPeriods,StartYearG,StartYearO,DebugInit,DebugFinal,'+
                            'DebugLevel,SummaryLevel,SummaryOut,StoreYield,RandomOpt,'+
                            'PlotOpt,LimitOpt,MultPeriodOpt,CalcHistoryOpt,'+
                            'ReduceSeqOpt,YearsCount,HydroSeqCount,LoadCasesCount,'+
                            'StartMonthNo,RunType,StartType,ParamFile';
      MonthNamesFields    = 'Model,StudyAreaName,SubArea,Scenario,'+
                            'Month1,Month2,Month3,Month4,Month5,Month6,Month7,'+
                            'Month8,Month9,Month10,Month11,Month12';
      DaysPerMonthFields  = 'Model,StudyAreaName,SubArea,Scenario,'+
                            'Days1,Days2,Days3,Days4,Days5,Days6,Days7,Days8,'+
                            'Days9,Days10,Days11,Days12';
      AnlySequencesFields = 'Model,StudyAreaName,SubArea,Scenario,'+
                            'Seq1,Seq2,Seq3,Seq4,Seq5,Seq6,Seq7,Seq8,Seq9,Seq10';
      TargetYieldFields   = 'Model,StudyAreaName,SubArea,Scenario,'+
                            'TYield1,TYield2,TYield3,TYield4,TYield5,TYield6,'+
                            'TYield7,TYield8,TYield9,TYield10';
      MaxYieldFields      = 'Model,StudyAreaName,SubArea,Scenario,'+
                            'MYield1,MYield2,MYield3,MYield4,MYield5,MYield6,'+
                            'MYield7,MYield8,MYield9,MYield10';
      TargetPowerFields   = 'Model,StudyAreaName,SubArea,Scenario,'+
                            'TPower1,TPower2,TPower3,TPower4,TPower5,TPower6,'+
                            'TPower7,TPower8,TPower9,TPower10';

var
  LDataSet: TAbstractModelDataset;
  LModel,
  LStudyAreaName,
  LSubArea,
  LScenario,
  LSQL: string;
begin
  Result := False;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      if Assigned(LDataSet) then
      begin
        LModel         := FAppModules.StudyArea.ModelCode;
        LStudyAreaName := FAppModules.StudyArea.StudyAreaCode;
        LSubArea       := FAppModules.StudyArea.SubAreaCode;
        LScenario      := FAppModules.StudyArea.ScenarioCode;

        FAppModules.Database.StartTransaction;
        try

          //-----------------------------RunTitle-------------------------------
          LSQL := 'SELECT  '+ RunTitleFields + ' FROM RunTitle WHERE ' + GetScenarioWhereClause;
          LDataSet.DataSet.Close;
          LDataSet.SetSQL(LSQL);
          LDataset.SetReadOnly(False);
          LDataset.DataSet.Open;
          if LDataset.IsReadOnly  then
          begin
            LDataset.DataSet.Close;
            raise Exception.Create('Query to table RunTitle cannot be set to updateble.');
          end
          else
          begin
            if(LDataset.DataSet.RecordCount = 0) then
            begin
              LDataset.DataSet.Append;
              LDataset.DataSet.FieldByName('Model').AsString := LModel;
              LDataset.DataSet.FieldByName('StudyAreaName').AsString := LStudyAreaName;
              LDataset.DataSet.FieldByName('SubArea').AsString := LSubArea;
              LDataset.DataSet.FieldByName('Scenario').AsString := LScenario;

              LDataset.DataSet.FieldByName('Title1').AsString := 'Title1';
              LDataset.DataSet.FieldByName('Title2').AsString := 'Title2';
              LDataset.DataSet.FieldByName('Title3').AsString := 'Title3';
              LDataset.DataSet.Post;
            end;
          end;

          //-----------------------------RunParameters-------------------------------
          LSQL := 'SELECT  '+ RunParametersFields + ' FROM RunParameters WHERE ' + GetScenarioWhereClause;
          LDataSet.DataSet.Close;
          LDataSet.SetSQL(LSQL);
          LDataset.SetReadOnly(False);
          LDataset.DataSet.Open;
          if LDataset.IsReadOnly  then
          begin
            LDataset.DataSet.Close;
            raise Exception.Create('Query to table RunParameters cannot be set to updateble.');
          end
          else
          begin
            if(LDataset.DataSet.RecordCount = 0) then
            begin
              LDataset.DataSet.Append;
              LDataset.DataSet.FieldByName('Model').AsString := LModel;
              LDataset.DataSet.FieldByName('StudyAreaName').AsString := LStudyAreaName;
              LDataset.DataSet.FieldByName('SubArea').AsString := LSubArea;
              LDataset.DataSet.FieldByName('Scenario').AsString := LScenario;
            end
            else
              LDataset.DataSet.Edit;

            LDataset.DataSet.FieldByName('NumPeriods').AsInteger := 840;
            LDataset.DataSet.FieldByName('StartYearG').AsInteger := 1;
            LDataset.DataSet.FieldByName('StartYearO').AsInteger := 1920;
            LDataset.DataSet.FieldByName('DebugInit').AsInteger := 1;
            LDataset.DataSet.FieldByName('DebugFinal').AsInteger := 840;
            LDataset.DataSet.FieldByName('DebugLevel').AsInteger := -3;
            LDataset.DataSet.FieldByName('SummaryLevel').AsInteger := 0;
            LDataset.DataSet.FieldByName('SummaryOut').AsString := '0';
            LDataset.DataSet.FieldByName('StoreYield').AsString := '0';
            LDataset.DataSet.FieldByName('RandomOpt').AsString := '0';
            LDataset.DataSet.FieldByName('PlotOpt').AsString := 'N';
            LDataset.DataSet.FieldByName('LimitOpt').AsString := '0';
            LDataset.DataSet.FieldByName('MultPeriodOpt').AsInteger := 0;
            LDataset.DataSet.FieldByName('CalcHistoryOpt').AsInteger := 0;
            LDataset.DataSet.FieldByName('ReduceSeqOpt').AsInteger := 0;
            LDataset.DataSet.FieldByName('YearsCount').AsInteger := 70;
            LDataset.DataSet.FieldByName('HydroSeqCount').AsInteger := 1;
            LDataset.DataSet.FieldByName('LoadCasesCount').AsInteger := 1;
            LDataset.DataSet.FieldByName('StartMonthNo').AsInteger := 1;
            LDataset.DataSet.FieldByName('RunType').AsString := 'H';
            LDataset.DataSet.FieldByName('StartType').AsInteger := 1;
            LDataset.DataSet.FieldByName('ParamFile').AsString := '';
            LDataset.DataSet.Post;
          end;

          //-----------------------------MonthNames-------------------------------
          LSQL := 'SELECT  '+ MonthNamesFields + ' FROM MonthNames WHERE ' + GetScenarioWhereClause;
          LDataSet.DataSet.Close;
          LDataSet.SetSQL(LSQL);
          LDataset.SetReadOnly(False);
          LDataset.DataSet.Open;
          if LDataset.IsReadOnly  then
          begin
            LDataset.DataSet.Close;
            raise Exception.Create('Query to table MonthNames cannot be set to updateble.');
          end
          else
          begin
            if(LDataset.DataSet.RecordCount = 0) then
            begin
              LDataset.DataSet.Append;
              LDataset.DataSet.FieldByName('Model').AsString := LModel;
              LDataset.DataSet.FieldByName('StudyAreaName').AsString := LStudyAreaName;
              LDataset.DataSet.FieldByName('SubArea').AsString := LSubArea;
              LDataset.DataSet.FieldByName('Scenario').AsString := LScenario;
            end
            else
              LDataset.DataSet.Edit;

            LDataset.DataSet.FieldByName('Month1').AsString  := 'OCT';
            LDataset.DataSet.FieldByName('Month2').AsString  := 'NOV';
            LDataset.DataSet.FieldByName('Month3').AsString  := 'DEC';
            LDataset.DataSet.FieldByName('Month4').AsString  := 'JAN';
            LDataset.DataSet.FieldByName('Month5').AsString  := 'FEB';
            LDataset.DataSet.FieldByName('Month6').AsString  := 'MAR';
            LDataset.DataSet.FieldByName('Month7').AsString  := 'APR';
            LDataset.DataSet.FieldByName('Month8').AsString  := 'MAY';
            LDataset.DataSet.FieldByName('Month9').AsString  := 'JUN';
            LDataset.DataSet.FieldByName('Month10').AsString := 'JUL';
            LDataset.DataSet.FieldByName('Month11').AsString := 'AUG';
            LDataset.DataSet.FieldByName('Month12').AsString := 'SEP';
            LDataset.DataSet.Post;
          end;

          //-----------------------------DaysPerMonth-------------------------------
          LSQL := 'SELECT  '+ DaysPerMonthFields + ' FROM DaysPerMonth WHERE ' + GetScenarioWhereClause;
          LDataSet.DataSet.Close;
          LDataSet.SetSQL(LSQL);
          LDataset.SetReadOnly(False);
          LDataset.DataSet.Open;
          if LDataset.IsReadOnly  then
          begin
            LDataset.DataSet.Close;
            raise Exception.Create('Query to table DaysPerMonth cannot be set to updateble.');
          end
          else
          begin
            if(LDataset.DataSet.RecordCount = 0) then
            begin
              LDataset.DataSet.Append;
              LDataset.DataSet.FieldByName('Model').AsString := LModel;
              LDataset.DataSet.FieldByName('StudyAreaName').AsString := LStudyAreaName;
              LDataset.DataSet.FieldByName('SubArea').AsString := LSubArea;
              LDataset.DataSet.FieldByName('Scenario').AsString := LScenario;
            end
            else
              LDataset.DataSet.Edit;

            LDataset.DataSet.FieldByName('Days1').AsFloat  := 31.00;
            LDataset.DataSet.FieldByName('Days2').AsFloat  := 30.00;
            LDataset.DataSet.FieldByName('Days3').AsFloat  := 31.00;
            LDataset.DataSet.FieldByName('Days4').AsFloat  := 31.00;
            LDataset.DataSet.FieldByName('Days5').AsFloat  := 28.25;
            LDataset.DataSet.FieldByName('Days6').AsFloat  := 31.00;
            LDataset.DataSet.FieldByName('Days7').AsFloat  := 30.00;
            LDataset.DataSet.FieldByName('Days8').AsFloat  := 31.00;
            LDataset.DataSet.FieldByName('Days9').AsFloat  := 30.00;
            LDataset.DataSet.FieldByName('Days10').AsFloat := 31.00;
            LDataset.DataSet.FieldByName('Days11').AsFloat := 31.00;
            LDataset.DataSet.FieldByName('Days12').AsFloat := 30.00;
            LDataset.DataSet.Post;
          end;

          //-----------------------------AnlySequences-------------------------------
          LSQL := 'SELECT  '+ AnlySequencesFields + ' FROM AnlySequences WHERE ' + GetScenarioWhereClause;
          LDataSet.DataSet.Close;
          LDataSet.SetSQL(LSQL);
          LDataset.SetReadOnly(False);
          LDataset.DataSet.Open;
          if LDataset.IsReadOnly  then
          begin
            LDataset.DataSet.Close;
            raise Exception.Create('Query to table AnlySequences cannot be set to updateble.');
          end
          else
          begin
            if(LDataset.DataSet.RecordCount = 0) then
            begin
              LDataset.DataSet.Append;
              LDataset.DataSet.FieldByName('Model').AsString := LModel;
              LDataset.DataSet.FieldByName('StudyAreaName').AsString := LStudyAreaName;
              LDataset.DataSet.FieldByName('SubArea').AsString := LSubArea;
              LDataset.DataSet.FieldByName('Scenario').AsString := LScenario;
            end
            else
              LDataset.DataSet.Edit;

            LDataset.DataSet.FieldByName('Seq1').AsFloat  := 1.0;
            LDataset.DataSet.FieldByName('Seq2').Clear;
            LDataset.DataSet.FieldByName('Seq3').Clear;
            LDataset.DataSet.FieldByName('Seq4').Clear;
            LDataset.DataSet.FieldByName('Seq5').Clear;
            LDataset.DataSet.FieldByName('Seq6').Clear;
            LDataset.DataSet.FieldByName('Seq7').Clear;
            LDataset.DataSet.FieldByName('Seq8').Clear;
            LDataset.DataSet.FieldByName('Seq9').Clear;
            LDataset.DataSet.FieldByName('Seq10').Clear;
            LDataset.DataSet.Post;
          end;

          //-----------------------------TargetYield-------------------------------
          LSQL := 'SELECT  '+ TargetYieldFields + ' FROM TargetYield WHERE ' + GetScenarioWhereClause;
          LDataSet.DataSet.Close;
          LDataSet.SetSQL(LSQL);
          LDataset.SetReadOnly(False);
          LDataset.DataSet.Open;
          if LDataset.IsReadOnly  then
          begin
            LDataset.DataSet.Close;
            raise Exception.Create('Query to table TargetYield cannot be set to updateble.');
          end
          else
          begin
            if(LDataset.DataSet.RecordCount = 0) then
            begin
              LDataset.DataSet.Append;
              LDataset.DataSet.FieldByName('Model').AsString := LModel;
              LDataset.DataSet.FieldByName('StudyAreaName').AsString := LStudyAreaName;
              LDataset.DataSet.FieldByName('SubArea').AsString := LSubArea;
              LDataset.DataSet.FieldByName('Scenario').AsString := LScenario;
            end
            else
              LDataset.DataSet.Edit;

            LDataset.DataSet.FieldByName('TYield1').AsFloat  := 10.0;
            LDataset.DataSet.FieldByName('TYield2').Clear;
            LDataset.DataSet.FieldByName('TYield3').Clear;
            LDataset.DataSet.FieldByName('TYield4').Clear;
            LDataset.DataSet.FieldByName('TYield5').Clear;
            LDataset.DataSet.FieldByName('TYield6').Clear;
            LDataset.DataSet.FieldByName('TYield7').Clear;
            LDataset.DataSet.FieldByName('TYield8').Clear;
            LDataset.DataSet.FieldByName('TYield9').Clear;
            LDataset.DataSet.FieldByName('TYield10').Clear;
            LDataset.DataSet.Post;
          end;

          //-----------------------------MaxYield-------------------------------
          LSQL := 'SELECT  '+ MaxYieldFields + ' FROM MaxYield WHERE ' + GetScenarioWhereClause;
          LDataSet.DataSet.Close;
          LDataSet.SetSQL(LSQL);
          LDataset.SetReadOnly(False);
          LDataset.DataSet.Open;
          if LDataset.IsReadOnly  then
          begin
            LDataset.DataSet.Close;
            raise Exception.Create('Query to table MaxYield cannot be set to updateble.');
          end
          else
          begin
            if(LDataset.DataSet.RecordCount = 0) then
            begin
              LDataset.DataSet.Append;
              LDataset.DataSet.FieldByName('Model').AsString := LModel;
              LDataset.DataSet.FieldByName('StudyAreaName').AsString := LStudyAreaName;
              LDataset.DataSet.FieldByName('SubArea').AsString := LSubArea;
              LDataset.DataSet.FieldByName('Scenario').AsString := LScenario;
            end
            else
              LDataset.DataSet.Edit;

            LDataset.DataSet.FieldByName('MYield1').AsFloat  := 10.0;
            LDataset.DataSet.FieldByName('MYield2').Clear;
            LDataset.DataSet.FieldByName('MYield3').Clear;
            LDataset.DataSet.FieldByName('MYield4').Clear;
            LDataset.DataSet.FieldByName('MYield5').Clear;
            LDataset.DataSet.FieldByName('MYield6').Clear;
            LDataset.DataSet.FieldByName('MYield7').Clear;
            LDataset.DataSet.FieldByName('MYield8').Clear;
            LDataset.DataSet.FieldByName('MYield9').Clear;
            LDataset.DataSet.FieldByName('MYield10').Clear;
            LDataset.DataSet.Post;
          end;

          //-----------------------------TargetPower-------------------------------
          LSQL := 'SELECT  '+ TargetPowerFields + ' FROM TargetPower WHERE '+ GetScenarioWhereClause;
          LDataSet.DataSet.Close;
          LDataSet.SetSQL(LSQL);
          LDataset.SetReadOnly(False);
          LDataset.DataSet.Open;
          if LDataset.IsReadOnly  then
          begin
            LDataset.DataSet.Close;
            raise Exception.Create('Query to table TargetPower cannot be set to updateble.');
          end
          else
          begin
            if(LDataset.DataSet.RecordCount = 0) then
            begin
              LDataset.DataSet.Append;
              LDataset.DataSet.FieldByName('Model').AsString := LModel;
              LDataset.DataSet.FieldByName('StudyAreaName').AsString := LStudyAreaName;
              LDataset.DataSet.FieldByName('SubArea').AsString := LSubArea;
              LDataset.DataSet.FieldByName('Scenario').AsString := LScenario;
            end
            else
              LDataset.DataSet.Edit;

            LDataset.DataSet.FieldByName('TPower1').AsFloat  := 0.0;
            LDataset.DataSet.FieldByName('TPower2').Clear;
            LDataset.DataSet.FieldByName('TPower3').Clear;
            LDataset.DataSet.FieldByName('TPower4').Clear;
            LDataset.DataSet.FieldByName('TPower5').Clear;
            LDataset.DataSet.FieldByName('TPower6').Clear;
            LDataset.DataSet.FieldByName('TPower7').Clear;
            LDataset.DataSet.FieldByName('TPower8').Clear;
            LDataset.DataSet.FieldByName('TPower9').Clear;
            LDataset.DataSet.FieldByName('TPower10').Clear;
            LDataset.DataSet.Post;
          end;

          FAppModules.Database.Commit;
          Result := True;
        except
          FAppModules.Database.Rollback;
          raise;
        end;
      end;
    finally
      LDataset.Free;
    end;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.


