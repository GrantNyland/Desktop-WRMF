//
//
//  UNIT      : Contains TScenarioLockManager Class
//  AUTHOR    : Dziedzi Ramulondi (ARIVIA)
//  DATE      : 2005/05/03
//  COPYRIGHT : Copyright © 2005 DWAF
//
//
unit UScenarioLockManager;

interface

uses
  Classes, sysutils,VCL.Dialogs,

  //  DWAF VCL
  UConstants,
  UAbstractObject;

type

  TScenarioLockManager = class(TAbstractScenarioLockManager)
  protected
    FInstanceID: string;
    procedure CreateMemberObjects; override;
  public
    { Public declarations }
    function RequestLock: boolean;override;
    function ReleaseLock: boolean;override;
    function RequestLockStatus(AModelCode,AStudyAreaCode,ASubAreaCode,AScenarioCode : string) : boolean; override;
    function RequestScenarioUnlock(AModelCode,AStudyAreaCode,ASubAreaCode,AScenarioCode : string) : boolean; override;
  end;


implementation

uses UUtilities,
     UDataSetType,
     UErrorHandlingOperations;


{ TScenarioLockManager }

procedure TScenarioLockManager.CreateMemberObjects;
const OPNAME = 'TScenarioLockManager.CreateMemberObjects';
var
  LGuid: TGUID;
begin
  inherited;
  try
    CreateGUID(LGuid);
    FInstanceID := GUIDToString(LGuid);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TScenarioLockManager.ReleaseLock:boolean;
const OPNAME = 'TScenarioLockManager.ReleaseLock';
{var
  LDataSet     : TAbstractModelDataset;
  LSQL : string;}
begin
  Result := False;
  try
    Result := True;
    {FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      if Assigned(LDataSet) then
      begin
        LSQL :=
          'DELETE FROM StudyScenarioLock WHERE'+
          ' (Model         = ' + QuotedStr(FAppModules.StudyArea.ModelCode)           + ') AND ' +
          ' (StudyAreaName = ' + QuotedStr(FAppModules.StudyArea.StudyAreaCode)       + ') AND ' +
          ' (SubArea       = ' + QuotedStr(FAppModules.StudyArea.SubAreaCode)         + ') AND ' +
          ' (Scenario      = ' + QuotedStr(FAppModules.StudyArea.ScenarioCode)        + ') AND ' +
          ' (Usename       = ' + QuotedStr(FAppModules.User.UserId)+ ') AND ' +
          ' (InstanceID    = ' + QuotedStr(FInstanceID)+ ')';
        LDataSet.SetSQL(LSQL);
        LDataset.ExecSQL;
        Result := True;
      end;
    finally
      LDataset.DataSet.Close;
      LDataset.Free;
    end;}
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TScenarioLockManager.RequestLock: boolean;
const OPNAME = 'TScenarioLockManager.RequestLock';
{var
  LDataSet     : TAbstractModelDataset;
  LSQL : string;}
begin
  Result := False;
  try
    Result := True;
    {FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      if Assigned(LDataSet) then
      begin
        LSQL :=
          'SELECT * FROM StudyScenarioLock WHERE '+
          ' (Model         = ' + QuotedStr(FAppModules.StudyArea.ModelCode)           + ') AND ' +
          ' (StudyAreaName = ' + QuotedStr(FAppModules.StudyArea.StudyAreaCode)       + ') AND ' +
          ' (SubArea       = ' + QuotedStr(FAppModules.StudyArea.SubAreaCode)         + ') AND ' +
          ' (Scenario      = ' + QuotedStr(FAppModules.StudyArea.ScenarioCode)        + ')';
        //LDataSet.SetReadOnly(False);
        LDataSet.SetSQL(LSQL);
        LDataset.DataSet.Open;
        if(LDataset.DataSet.RecordCount > 0) then
        begin
          if(LDataset.DataSet.FieldByName('Usename').AsString = FAppModules.User.UserId) and
            (LDataset.DataSet.FieldByName('InstanceID').AsString = FInstanceID) then
          begin
            Result := True;
          end
          else
          begin
            // Broad Scale Pre-Processor and PesIMS data is not scenario specific
            // so "scenario locking" is irrelevant here for those models and
            // message is surpressed.
            // VGN 2007/01/11
            if FAppModules.Model.ModelName <> CPreProcessor then
            begin
              ShowMessage('The selected scenario is already locked by another user and will be read only.');
            end;
          end;
          LDataset.DataSet.Close;
        end
        else
        begin
          if (FAppModules.StudyArea.ModelCode <> '') and
             (FAppModules.StudyArea.StudyAreaCode <> '') and
             (FAppModules.StudyArea.SubAreaCode <> '') and
             (FAppModules.StudyArea.ScenarioCode <> '') then
          begin
            LSQL :=
              'INSERT INTO StudyScenarioLock'+
              '(Model, StudyAreaName, SubArea, Scenario, Usename, LockDate, InstanceID) '+
              'VALUES '+
              '(:AModel, :AStudyAreaName, :ASubArea, :AScenario, :AUsename, :ALockDate, :AInstanceID)';
            LDataset.DataSet.Close;
            LDataSet.SetSQL(LSQL);
            LDataSet.SetParams(['AModel'], [FAppModules.StudyArea.ModelCode]);
            LDataSet.SetParams(['AStudyAreaName'], [FAppModules.StudyArea.StudyAreaCode]);
            LDataSet.SetParams(['ASubArea'], [FAppModules.StudyArea.SubAreaCode]);
            LDataSet.SetParams(['AScenario'], [FAppModules.StudyArea.ScenarioCode]);
            LDataSet.SetParams(['AUsename'], [FAppModules.User.UserId]);
            LDataSet.SetParams(['ALockDate'], [DateTimeToStr(Now)]);
            LDataSet.SetParams(['AInstanceID'], [FInstanceID]);
            LDataset.ExecSQL;
            Result := True;
          end;
        end;
      end;
    finally
      LDataset.DataSet.Close;
      LDataset.Free;
    end;}
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TScenarioLockManager.RequestLockStatus(AModelCode,AStudyAreaCode,ASubAreaCode,AScenarioCode : string) : boolean;
const OPNAME = 'TScenarioLockManager.RequestLockStatus';
{var
  LDataSet     : TAbstractModelDataset;
  LSQL : string;}
begin
  Result := False;
  try
    {FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      if Assigned(LDataSet) then
      begin
        LSQL :=
          'SELECT * FROM StudyScenarioLock WHERE '         +
          ' (Model         = ' + QuotedStr(AModelCode)     + ') AND ' +
          ' (StudyAreaName = ' + QuotedStr(AStudyAreaCode) + ') AND ' +
          ' (SubArea       = ' + QuotedStr(ASubAreaCode)   + ') AND ' +
          ' (Scenario      = ' + QuotedStr(AScenarioCode)  + ')';
        LDataSet.SetReadOnly(False);
        LDataSet.SetSQL(LSQL);
        LDataset.DataSet.Open;

        if(LDataset.DataSet.RecordCount > 0) then
          Result := True;
        LDataset.DataSet.Close;;
      end;
    finally
      LDataset.DataSet.Close;
      LDataset.Free;
    end;}
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TScenarioLockManager.RequestScenarioUnlock(AModelCode,AStudyAreaCode,ASubAreaCode,AScenarioCode : string) : boolean;
const OPNAME = 'TScenarioLockManager.RequestScenarioUnlock';
{var
  LDataSet     : TAbstractModelDataset;
  LSQL : string;}
begin
  Result := False;
  try
    Result := True;
    {FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      if Assigned(LDataSet) then
      begin
        LSQL :=
          'DELETE FROM StudyScenarioLock WHERE' +
          ' (Model         =  ' + QuotedStr(AModelCode)     + ') AND ' +
          ' (StudyAreaName =  ' + QuotedStr(AStudyAreaCode) + ') AND ' +
          ' (SubArea       =  ' + QuotedStr(ASubAreaCode)   + ') AND ' +
          ' (Scenario      =  ' + QuotedStr(AScenarioCode)  + ')';
        LDataSet.SetSQL(LSQL);
        LDataset.ExecSQL;
        Result := True;
      end;
    finally
      LDataset.DataSet.Close;
      LDataset.Free;
    end;}
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
