//
//
//  UNIT      : Contains TSumOutMonthlyBlockAgent Class
//  AUTHOR    : Dziedzi Ramulondi(PDNA)
//  DATE      : 02/07/2002
//  COPYRIGHT : Copyright © 2002 DWAF
//
//
unit USumOutMonthlyBlockAgent;

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

  TSumOutMonthlyBlockAgent = class(TAbstractAppObject)
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

{ TSumOutMonthlyBlockAgent }

procedure TSumOutMonthlyBlockAgent.CreateMemberObjects;
const OPNAME = 'TSumOutMonthlyBlockAgent.CreateMemberObjects';
begin
  inherited CreateMemberObjects;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TSumOutMonthlyBlockAgent.DestroyMemberObjects;
const OPNAME = 'TSumOutMonthlyBlockAgent.DestroyMemberObjects';
begin
  inherited DestroyMemberObjects;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TSumOutMonthlyBlockAgent.Initialise: boolean;
const OPNAME = 'TSumOutMonthlyBlockAgent.Initialise';
begin
  Result := inherited Initialise;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TSumOutMonthlyBlockAgent.Validate(ABLockData: TDataBlock; AProgressFunction: TProgressUpdateFuntion): boolean;
const OPNAME = 'TSumOutMonthlyBlockAgent.Validate';
var
  LSumOutMonthlyBlockHeading : TSumOutMonthlyBlockHeading;
  LSumOutMonthlyBlockValues  : TSumOutMonthlyBlockValues;
  LSumOutMonthlyBlockAvarage : TSumOutMonthlyBlockAvarage;
begin
  Result := False;
  try
    if not Assigned(ABLockData) then
      raise Exception.Create('Block data parameter is not yet assigned.');

    if not Assigned(AProgressFunction) then
      AProgressFunction := DummyShowProgress;

    LSumOutMonthlyBlockHeading := TSumOutMonthlyBlockHeading.Create(FAppModules);
    LSumOutMonthlyBlockValues  := TSumOutMonthlyBlockValues.Create(FAppModules);
    LSumOutMonthlyBlockAvarage := TSumOutMonthlyBlockAvarage.Create(FAppModules);
    try
      LSumOutMonthlyBlockHeading.Initialise;
      LSumOutMonthlyBlockValues.Initialise;
      LSumOutMonthlyBlockAvarage.Initialise;

      LSumOutMonthlyBlockHeading.BlockNumber       := ABLockData.BlockNumber;
      LSumOutMonthlyBlockHeading.LoadCaseNumber    := ABLockData.LoadCaseNumber;
      LSumOutMonthlyBlockHeading.SequenceNumber    := ABLockData.SequenceNumber;
      LSumOutMonthlyBlockHeading.ElementID         := ABLockData.ElementID;
      LSumOutMonthlyBlockHeading.AnnualWaterDemand := ABLockData.AnnualWaterDemand;
      LSumOutMonthlyBlockHeading.AnnualPowerDemand := ABLockData.AnnualPowerDemand;

      LSumOutMonthlyBlockValues.BlockNumber       := ABLockData.BlockNumber;
      LSumOutMonthlyBlockValues.LoadCaseNumber    := ABLockData.LoadCaseNumber;
      LSumOutMonthlyBlockValues.SequenceNumber    := ABLockData.SequenceNumber;

      LSumOutMonthlyBlockAvarage.BlockNumber       := ABLockData.BlockNumber;
      LSumOutMonthlyBlockAvarage.LoadCaseNumber    := ABLockData.LoadCaseNumber;
      LSumOutMonthlyBlockAvarage.SequenceNumber    := ABLockData.SequenceNumber;

      Result :=
        LSumOutMonthlyBlockHeading.ReadFromBlockData(ABLockData) and
        LSumOutMonthlyBlockValues.ReadFromBlockData(ABLockData) and
        LSumOutMonthlyBlockAvarage.ReadFromBlockData(ABLockData);

    finally
      FreeAndNil(LSumOutMonthlyBlockHeading);
      FreeAndNil(LSumOutMonthlyBlockValues);
      FreeAndNil(LSumOutMonthlyBlockAvarage);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TSumOutMonthlyBlockAgent.ImportFileBlock(ABLockData: TDataBlock; AProgressFunction: TProgressUpdateFuntion): boolean;
const OPNAME = 'TSumOutMonthlyBlockAgent.ImportFileBlock';
var
  LDatabaseName,
  LModel,
  LStudy,
  LSubArea,
  LScenario: string;

  LSumOutMonthlyBlockHeading : TSumOutMonthlyBlockHeading;
  LSumOutMonthlyBlockValues  : TSumOutMonthlyBlockValues;
  LSumOutMonthlyBlockAvarage : TSumOutMonthlyBlockAvarage;
begin
  Result := False;
  try
    if not Assigned(ABLockData) then
      raise Exception.Create('Block data parameter is not yet assigned.');

    if not Assigned(AProgressFunction) then
      AProgressFunction := DummyShowProgress;

    LSumOutMonthlyBlockHeading := TSumOutMonthlyBlockHeading.Create(FAppModules);
    LSumOutMonthlyBlockValues  := TSumOutMonthlyBlockValues.Create(FAppModules);
    LSumOutMonthlyBlockAvarage := TSumOutMonthlyBlockAvarage.Create(FAppModules);
    try
      LSumOutMonthlyBlockHeading.Initialise;
      LSumOutMonthlyBlockValues.Initialise;
      LSumOutMonthlyBlockAvarage.Initialise;

      LSumOutMonthlyBlockHeading.BlockNumber       := ABLockData.BlockNumber;
      LSumOutMonthlyBlockHeading.LoadCaseNumber    := ABLockData.LoadCaseNumber;
      LSumOutMonthlyBlockHeading.SequenceNumber    := ABLockData.SequenceNumber;
      LSumOutMonthlyBlockHeading.ElementID         := ABLockData.ElementID;
      LSumOutMonthlyBlockHeading.AnnualWaterDemand := ABLockData.AnnualWaterDemand;
      LSumOutMonthlyBlockHeading.AnnualPowerDemand := ABLockData.AnnualPowerDemand;

      LSumOutMonthlyBlockValues.BlockNumber       := ABLockData.BlockNumber;
      LSumOutMonthlyBlockValues.LoadCaseNumber    := ABLockData.LoadCaseNumber;
      LSumOutMonthlyBlockValues.SequenceNumber    := ABLockData.SequenceNumber;

      LSumOutMonthlyBlockAvarage.BlockNumber       := ABLockData.BlockNumber;
      LSumOutMonthlyBlockAvarage.LoadCaseNumber    := ABLockData.LoadCaseNumber;
      LSumOutMonthlyBlockAvarage.SequenceNumber    := ABLockData.SequenceNumber;

      Result :=
        LSumOutMonthlyBlockHeading.ReadFromBlockData(ABLockData) and
        LSumOutMonthlyBlockValues.ReadFromBlockData(ABLockData) and
        LSumOutMonthlyBlockAvarage.ReadFromBlockData(ABLockData);

      if Result then
      begin
        LDatabaseName := FAppModules.Database.DatabaseName;
        LModel        := FAppModules.StudyArea.ModelCode;
        LStudy        := FAppModules.StudyArea.StudyAreaCode;
        LSubArea      := FAppModules.StudyArea.SubAreaCode;
        LScenario     := FAppModules.StudyArea.ScenarioCode;

        Result := LSumOutMonthlyBlockHeading.SaveToDatabase(LDatabaseName,LModel,LStudy,LSubArea,LScenario) and
                  LSumOutMonthlyBlockValues.SaveToDatabase(LDatabaseName,LModel,LStudy,LSubArea,LScenario) and
                  LSumOutMonthlyBlockAvarage.SaveToDatabase(LDatabaseName,LModel,LStudy,LSubArea,LScenario);
      end;


    finally
      FreeAndNil(LSumOutMonthlyBlockHeading);
      FreeAndNil(LSumOutMonthlyBlockValues);
      FreeAndNil(LSumOutMonthlyBlockAvarage);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TSumOutMonthlyBlockAgent.ExportFileBlock(ADataset: TDataset; AOutputStream:TFileStream;
         AProgressFunction: TProgressUpdateFuntion): boolean;
const OPNAME = 'TSumOutMonthlyBlockAgent.ExportFileBlock';
var
  LSumOutMonthlyBlockHeading : TSumOutMonthlyBlockHeading;
  LSumOutMonthlyBlockValues  : TSumOutMonthlyBlockValues;
  LSumOutMonthlyBlockAvarage : TSumOutMonthlyBlockAvarage;
begin
  Result := False;
  try
    if not Assigned(ADataset) then
      raise Exception.Create('Dataset parameter is not yet assigned.');

    if not Assigned(AProgressFunction) then
      AProgressFunction := DummyShowProgress;

    LSumOutMonthlyBlockHeading := TSumOutMonthlyBlockHeading.Create(FAppModules);
    LSumOutMonthlyBlockValues  := TSumOutMonthlyBlockValues.Create(FAppModules);
    LSumOutMonthlyBlockAvarage := TSumOutMonthlyBlockAvarage.Create(FAppModules);
    try
      LSumOutMonthlyBlockHeading.Initialise;
      LSumOutMonthlyBlockValues.Initialise;
      LSumOutMonthlyBlockAvarage.Initialise;

      Result :=
        LSumOutMonthlyBlockHeading.ReadFromDatabase(FAppModules.Database.DatabaseName,ADataset) and
        LSumOutMonthlyBlockValues.ReadFromDatabase(FAppModules.Database.DatabaseName,ADataset) and
        LSumOutMonthlyBlockAvarage.ReadFromDatabase(FAppModules.Database.DatabaseName,ADataset);

      if Result then
      begin
        LSumOutMonthlyBlockHeading.SaveToStream(AOutputStream);
        LSumOutMonthlyBlockValues.SaveToStream(AOutputStream);
        LSumOutMonthlyBlockAvarage.SaveToStream(AOutputStream);
      end;


    finally
      FreeAndNil(LSumOutMonthlyBlockHeading);
      FreeAndNil(LSumOutMonthlyBlockValues);
      FreeAndNil(LSumOutMonthlyBlockAvarage);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
