//
//
//  UNIT      : Contains TSumOutYieldFailureBlockAgent Class
//  AUTHOR    : Dziedzi Ramulondi(PDNA)
//  DATE      : 02/07/2002
//  COPYRIGHT : Copyright © 2002 DWAF
//
//
unit USumOutYieldFailureBlockAgent;

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

  TSumOutYieldFailureBlockAgent = class(TAbstractAppObject)
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

{ TSumOutYieldFailureBlockAgent }

procedure TSumOutYieldFailureBlockAgent.CreateMemberObjects;
const OPNAME = 'TSumOutYieldFailureBlockAgent.CreateMemberObjects';
begin
  inherited CreateMemberObjects;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TSumOutYieldFailureBlockAgent.DestroyMemberObjects;
const OPNAME = 'TSumOutYieldFailureBlockAgent.DestroyMemberObjects';
begin
  inherited DestroyMemberObjects;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TSumOutYieldFailureBlockAgent.Initialise: boolean;
const OPNAME = 'TSumOutYieldFailureBlockAgent.Initialise';
begin
  Result := inherited Initialise;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TSumOutYieldFailureBlockAgent.Validate(ABLockData: TDataBlock; AProgressFunction: TProgressUpdateFuntion): boolean;
const OPNAME = 'TSumOutYieldFailureBlockAgent.Validate';
var
  LSumOutYieldFailureBlockHeading : TSumOutYieldFailureBlockHeading;
  LSumOutYieldFailureBlockValues  : TSumOutYieldFailureBlockValues;
begin
  Result := False;
  try
    if not Assigned(ABLockData) then
      raise Exception.Create('Block data parameter is not yet assigned.');

    if not Assigned(AProgressFunction) then
      AProgressFunction := DummyShowProgress;

    LSumOutYieldFailureBlockHeading := TSumOutYieldFailureBlockHeading.Create(FAppModules);
    LSumOutYieldFailureBlockValues  := TSumOutYieldFailureBlockValues.Create(FAppModules);

    try
      LSumOutYieldFailureBlockHeading.Initialise;
      LSumOutYieldFailureBlockValues.Initialise;

      LSumOutYieldFailureBlockHeading.BlockNumber       := ABLockData.BlockNumber;
      LSumOutYieldFailureBlockHeading.LoadCaseNumber    := ABLockData.LoadCaseNumber;
      LSumOutYieldFailureBlockHeading.SequenceNumber    := ABLockData.SequenceNumber;
      LSumOutYieldFailureBlockHeading.ElementID         := ABLockData.ElementID;
      LSumOutYieldFailureBlockHeading.AnnualWaterDemand := ABLockData.AnnualWaterDemand;
      LSumOutYieldFailureBlockHeading.AnnualPowerDemand := ABLockData.AnnualPowerDemand;

      LSumOutYieldFailureBlockValues.BlockNumber       := ABLockData.BlockNumber;
      LSumOutYieldFailureBlockValues.LoadCaseNumber    := ABLockData.LoadCaseNumber;
      LSumOutYieldFailureBlockValues.SequenceNumber    := ABLockData.SequenceNumber;

      Result :=
        LSumOutYieldFailureBlockHeading.ReadFromBlockData(ABLockData) and
        LSumOutYieldFailureBlockValues.ReadFromBlockData(ABLockData);

    finally
      FreeAndNil(LSumOutYieldFailureBlockHeading);
      FreeAndNil(LSumOutYieldFailureBlockValues);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TSumOutYieldFailureBlockAgent.ImportFileBlock(ABLockData: TDataBlock; AProgressFunction: TProgressUpdateFuntion): boolean;
const OPNAME = 'TSumOutYieldFailureBlockAgent.ImportFileBlock';
var
  LDatabaseName,
  LModel,
  LStudy,
  LSubArea,
  LScenario: string;

  LSumOutYieldFailureBlockHeading : TSumOutYieldFailureBlockHeading;
  LSumOutYieldFailureBlockValues  : TSumOutYieldFailureBlockValues;
begin
  Result := False;
  try
    if not Assigned(ABLockData) then
      raise Exception.Create('Block data parameter is not yet assigned.');

    if not Assigned(AProgressFunction) then
      AProgressFunction := DummyShowProgress;

    LSumOutYieldFailureBlockHeading := TSumOutYieldFailureBlockHeading.Create(FAppModules);
    LSumOutYieldFailureBlockValues  := TSumOutYieldFailureBlockValues.Create(FAppModules);

    try
      LSumOutYieldFailureBlockHeading.Initialise;
      LSumOutYieldFailureBlockValues.Initialise;

      LSumOutYieldFailureBlockHeading.BlockNumber       := ABLockData.BlockNumber;
      LSumOutYieldFailureBlockHeading.LoadCaseNumber    := ABLockData.LoadCaseNumber;
      LSumOutYieldFailureBlockHeading.SequenceNumber    := ABLockData.SequenceNumber;
      LSumOutYieldFailureBlockHeading.ElementID         := ABLockData.ElementID;
      LSumOutYieldFailureBlockHeading.AnnualWaterDemand := ABLockData.AnnualWaterDemand;
      LSumOutYieldFailureBlockHeading.AnnualPowerDemand := ABLockData.AnnualPowerDemand;

      LSumOutYieldFailureBlockValues.BlockNumber       := ABLockData.BlockNumber;
      LSumOutYieldFailureBlockValues.LoadCaseNumber    := ABLockData.LoadCaseNumber;
      LSumOutYieldFailureBlockValues.SequenceNumber    := ABLockData.SequenceNumber;

      Result :=
        LSumOutYieldFailureBlockHeading.ReadFromBlockData(ABLockData) and
        LSumOutYieldFailureBlockValues.ReadFromBlockData(ABLockData);

      if Result then
      begin
        LDatabaseName := FAppModules.Database.DatabaseName;
        LModel        := FAppModules.StudyArea.ModelCode;
        LStudy        := FAppModules.StudyArea.StudyAreaCode;
        LSubArea      := FAppModules.StudyArea.SubAreaCode;
        LScenario     := FAppModules.StudyArea.ScenarioCode;

        Result := LSumOutYieldFailureBlockHeading.SaveToDatabase(LDatabaseName,LModel,LStudy,LSubArea,LScenario) and
                  LSumOutYieldFailureBlockValues.SaveToDatabase(LDatabaseName,LModel,LStudy,LSubArea,LScenario);
      end;
    finally
      FreeAndNil(LSumOutYieldFailureBlockHeading);
      FreeAndNil(LSumOutYieldFailureBlockValues);
    end;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TSumOutYieldFailureBlockAgent.ExportFileBlock(ADataset: TDataset; AOutputStream:TFileStream;
         AProgressFunction: TProgressUpdateFuntion): boolean;
const OPNAME = 'TSumOutYieldFailureBlockAgent.ExportFileBlock';
var
  LSumOutYieldFailureBlockHeading : TSumOutYieldFailureBlockHeading;
  LSumOutYieldFailureBlockValues  : TSumOutYieldFailureBlockValues;
begin
  Result := False;
  try
    if not Assigned(ADataset) then
      raise Exception.Create('Dataset parameter is not yet assigned.');

    if not Assigned(AProgressFunction) then
      AProgressFunction := DummyShowProgress;

    LSumOutYieldFailureBlockHeading := TSumOutYieldFailureBlockHeading.Create(FAppModules);
    LSumOutYieldFailureBlockValues  := TSumOutYieldFailureBlockValues.Create(FAppModules);

    try
      LSumOutYieldFailureBlockHeading.Initialise;
      LSumOutYieldFailureBlockValues.Initialise;

      Result :=
        LSumOutYieldFailureBlockHeading.ReadFromDatabase(FAppModules.Database.DatabaseName,ADataset) and
        LSumOutYieldFailureBlockValues.ReadFromDatabase(FAppModules.Database.DatabaseName,ADataset);

      if Result then
      begin
        LSumOutYieldFailureBlockHeading.SaveToStream(AOutputStream);
        LSumOutYieldFailureBlockValues.SaveToStream(AOutputStream);
      end;
    finally
      FreeAndNil(LSumOutYieldFailureBlockHeading);
      FreeAndNil(LSumOutYieldFailureBlockValues);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
