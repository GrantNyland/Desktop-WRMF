//
//
//  UNIT      : Contains TSumOutCriticalPeriodsBlockAgent Class
//  AUTHOR    : Dziedzi Ramulondi(PDNA)
//  DATE      : 02/07/2002
//  COPYRIGHT : Copyright © 2002 DWAF
//
//
unit USumOutCriticalPeriodsBlockAgent;

interface

uses
  Classes, sysutils,contnrs,DB,

  //  DWAF VCL
  UConstants,
  UAbstractObject,
  UBasicObjects,
  UAbstractFileNamesObject,
  USumOutDataObjects;

type

  TSumOutCriticalPeriodsBlockAgent = class(TAbstractAppObject)
  protected
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
  public
    function Initialise: boolean;override;
    function Validate(ABLockData: TDataBlock; AProgressFunction: TProgressUpdateFuntion): boolean;
    function ImportFileBlock(ABLockData: TDataBlock;AProgressFunction: TProgressUpdateFuntion): boolean;
    function ExportFileBlock(ADataset: TDataset; AOutputStream:TFileStream; AProgressFunction: TProgressUpdateFuntion): boolean;
  end;

implementation


uses UUtilities,
     UErrorHandlingOperations;

{ TSumOutCriticalPeriodsBlockAgent }

procedure TSumOutCriticalPeriodsBlockAgent.CreateMemberObjects;
const OPNAME = 'TSumOutCriticalPeriodsBlockAgent.CreateMemberObjects';
begin
  inherited CreateMemberObjects;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TSumOutCriticalPeriodsBlockAgent.DestroyMemberObjects;
const OPNAME = 'TSumOutCriticalPeriodsBlockAgent.DestroyMemberObjects';
begin
  inherited DestroyMemberObjects;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TSumOutCriticalPeriodsBlockAgent.Initialise: boolean;
const OPNAME = 'TSumOutCriticalPeriodsBlockAgent.Initialise';
begin
  Result := inherited Initialise;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TSumOutCriticalPeriodsBlockAgent.Validate(ABLockData: TDataBlock; AProgressFunction: TProgressUpdateFuntion): boolean;
const OPNAME = 'TSumOutCriticalPeriodsBlockAgent.Validate';
var
  LSumOutCriticalPeriodsBlockHeading : TSumOutCriticalPeriodsBlockHeading;
  LSumOutCriticalPeriodsBlockValues  : TSumOutCriticalPeriodsBlockValues;
begin
  Result := False;
  try
    if not Assigned(ABLockData) then
      raise Exception.Create('Block data parameter is not yet assigned.');

    if not Assigned(AProgressFunction) then
      AProgressFunction := DummyShowProgress;

    LSumOutCriticalPeriodsBlockHeading := TSumOutCriticalPeriodsBlockHeading.Create(FAppModules);
    LSumOutCriticalPeriodsBlockValues  := TSumOutCriticalPeriodsBlockValues.Create(FAppModules);
    try
      LSumOutCriticalPeriodsBlockHeading.Initialise;
      LSumOutCriticalPeriodsBlockValues.Initialise;

      LSumOutCriticalPeriodsBlockHeading.BlockNumber       := ABLockData.BlockNumber;
      LSumOutCriticalPeriodsBlockHeading.LoadCaseNumber    := ABLockData.LoadCaseNumber;
      LSumOutCriticalPeriodsBlockHeading.SequenceNumber    := ABLockData.SequenceNumber;
      LSumOutCriticalPeriodsBlockHeading.ElementID         := ABLockData.ElementID;
      LSumOutCriticalPeriodsBlockHeading.AnnualWaterDemand := ABLockData.AnnualWaterDemand;
      LSumOutCriticalPeriodsBlockHeading.AnnualPowerDemand := ABLockData.AnnualPowerDemand;

      LSumOutCriticalPeriodsBlockValues.BlockNumber       := ABLockData.BlockNumber;
      LSumOutCriticalPeriodsBlockValues.LoadCaseNumber    := ABLockData.LoadCaseNumber;
      LSumOutCriticalPeriodsBlockValues.SequenceNumber    := ABLockData.SequenceNumber;

      Result :=
        LSumOutCriticalPeriodsBlockHeading.ReadFromBlockData(ABLockData) and
        LSumOutCriticalPeriodsBlockValues.ReadFromBlockData(ABLockData);

    finally
      FreeAndNil(LSumOutCriticalPeriodsBlockHeading);
      FreeAndNil(LSumOutCriticalPeriodsBlockValues);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TSumOutCriticalPeriodsBlockAgent.ImportFileBlock(ABLockData: TDataBlock; AProgressFunction: TProgressUpdateFuntion): boolean;
const OPNAME = 'TSumOutCriticalPeriodsBlockAgent.ImportFileBlock';
var
  LDatabaseName,
  LModel,
  LStudy,
  LSubArea,
  LScenario: string;

  LSumOutCriticalPeriodsBlockHeading : TSumOutCriticalPeriodsBlockHeading;
  LSumOutCriticalPeriodsBlockValues  : TSumOutCriticalPeriodsBlockValues;
begin
  Result := False;
  try
    if not Assigned(ABLockData) then
      raise Exception.Create('Block data parameter is not yet assigned.');

    if not Assigned(AProgressFunction) then
      AProgressFunction := DummyShowProgress;

    LSumOutCriticalPeriodsBlockHeading := TSumOutCriticalPeriodsBlockHeading.Create(FAppModules);
    LSumOutCriticalPeriodsBlockValues  := TSumOutCriticalPeriodsBlockValues.Create(FAppModules);
    try
      LSumOutCriticalPeriodsBlockHeading.Initialise;
      LSumOutCriticalPeriodsBlockValues.Initialise;

      LSumOutCriticalPeriodsBlockHeading.BlockNumber       := ABLockData.BlockNumber;
      LSumOutCriticalPeriodsBlockHeading.LoadCaseNumber    := ABLockData.LoadCaseNumber;
      LSumOutCriticalPeriodsBlockHeading.SequenceNumber    := ABLockData.SequenceNumber;
      LSumOutCriticalPeriodsBlockHeading.ElementID         := ABLockData.ElementID;
      LSumOutCriticalPeriodsBlockHeading.AnnualWaterDemand := ABLockData.AnnualWaterDemand;
      LSumOutCriticalPeriodsBlockHeading.AnnualPowerDemand := ABLockData.AnnualPowerDemand;

      LSumOutCriticalPeriodsBlockValues.BlockNumber       := ABLockData.BlockNumber;
      LSumOutCriticalPeriodsBlockValues.LoadCaseNumber    := ABLockData.LoadCaseNumber;
      LSumOutCriticalPeriodsBlockValues.SequenceNumber    := ABLockData.SequenceNumber;

      Result :=
        LSumOutCriticalPeriodsBlockHeading.ReadFromBlockData(ABLockData) and
        LSumOutCriticalPeriodsBlockValues.ReadFromBlockData(ABLockData);

      if Result then
      begin
        LDatabaseName := FAppModules.Database.DatabaseName;
        LModel        := FAppModules.StudyArea.ModelCode;
        LStudy        := FAppModules.StudyArea.StudyAreaCode;
        LSubArea      := FAppModules.StudyArea.SubAreaCode;
        LScenario     := FAppModules.StudyArea.ScenarioCode;

        Result := LSumOutCriticalPeriodsBlockHeading.SaveToDatabase(LDatabaseName,LModel,LStudy,LSubArea,LScenario) and
                  LSumOutCriticalPeriodsBlockValues.SaveToDatabase(LDatabaseName,LModel,LStudy,LSubArea,LScenario);
      end;

    finally
      FreeAndNil(LSumOutCriticalPeriodsBlockHeading);
      FreeAndNil(LSumOutCriticalPeriodsBlockValues);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TSumOutCriticalPeriodsBlockAgent.ExportFileBlock(ADataset: TDataset; AOutputStream:TFileStream;
         AProgressFunction: TProgressUpdateFuntion): boolean;
const OPNAME = 'TSumOutCriticalPeriodsBlockAgent.ExportFileBlock';
var
  LSumOutCriticalPeriodsBlockHeading : TSumOutCriticalPeriodsBlockHeading;
  LSumOutCriticalPeriodsBlockValues  : TSumOutCriticalPeriodsBlockValues;
begin
  Result := False;
  try
    if not Assigned(ADataset) then
      raise Exception.Create('Dataset parameter is not yet assigned.');

    if not Assigned(AProgressFunction) then
      AProgressFunction := DummyShowProgress;

    LSumOutCriticalPeriodsBlockHeading := TSumOutCriticalPeriodsBlockHeading.Create(FAppModules);
    LSumOutCriticalPeriodsBlockValues  := TSumOutCriticalPeriodsBlockValues.Create(FAppModules);
    try
      LSumOutCriticalPeriodsBlockHeading.Initialise;
      LSumOutCriticalPeriodsBlockValues.Initialise;

      Result :=
        LSumOutCriticalPeriodsBlockHeading.ReadFromDatabase(FAppModules.Database.DatabaseName,ADataset)  and
        LSumOutCriticalPeriodsBlockValues.ReadFromDatabase(FAppModules.Database.DatabaseName,ADataset);

      if Result then
      begin
        LSumOutCriticalPeriodsBlockHeading.SaveToStream(AOutputStream);
        LSumOutCriticalPeriodsBlockValues.SaveToStream(AOutputStream);
      end;

    finally
      FreeAndNil(LSumOutCriticalPeriodsBlockHeading);
      FreeAndNil(LSumOutCriticalPeriodsBlockValues);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
