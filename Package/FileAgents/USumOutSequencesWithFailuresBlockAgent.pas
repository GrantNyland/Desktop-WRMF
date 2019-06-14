//
//
//  UNIT      : Contains TSumOutSequencesWithFailuresBlockAgent Class
//  AUTHOR    : Dziedzi Ramulondi(PDNA)
//  DATE      : 02/07/2002
//  COPYRIGHT : Copyright © 2002 DWAF
//
//
unit USumOutSequencesWithFailuresBlockAgent;

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

  TSumOutSequencesWithFailuresBlockAgent = class(TAbstractAppObject)
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

{ TSumOutSequencesWithFailuresBlockAgent }

procedure TSumOutSequencesWithFailuresBlockAgent.CreateMemberObjects;
const OPNAME = 'TSumOutSequencesWithFailuresBlockAgent.CreateMemberObjects';
begin
  inherited CreateMemberObjects;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TSumOutSequencesWithFailuresBlockAgent.DestroyMemberObjects;
const OPNAME = 'TSumOutSequencesWithFailuresBlockAgent.DestroyMemberObjects';
begin
  inherited DestroyMemberObjects;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TSumOutSequencesWithFailuresBlockAgent.Initialise: boolean;
const OPNAME = 'TSumOutSequencesWithFailuresBlockAgent.Initialise';
begin
  Result := inherited Initialise;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TSumOutSequencesWithFailuresBlockAgent.Validate(ABLockData: TDataBlock;
         AProgressFunction: TProgressUpdateFuntion): boolean;
const OPNAME = 'TSumOutSequencesWithFailuresBlockAgent.Validate';
var
  LSumOutSequencesWithFailuresBlockHeading : TSumOutSequencesWithFailuresBlockHeading;
  LSumOutSequencesWithFailuresBlockValues  : TSumOutSequencesWithFailuresBlockValues;
  LSumOutSequencesWithFailuresBlockTotal : TSumOutSequencesWithFailuresBlockTotal;
begin
  Result := False;
  try
    if not Assigned(ABLockData) then
      raise Exception.Create('Block data parameter is not yet assigned.');

    if not Assigned(AProgressFunction) then
      AProgressFunction := DummyShowProgress;

    LSumOutSequencesWithFailuresBlockHeading := TSumOutSequencesWithFailuresBlockHeading.Create(FAppModules);
    LSumOutSequencesWithFailuresBlockValues  := TSumOutSequencesWithFailuresBlockValues.Create(FAppModules);
    LSumOutSequencesWithFailuresBlockTotal := TSumOutSequencesWithFailuresBlockTotal.Create(FAppModules);
    try
      LSumOutSequencesWithFailuresBlockHeading.Initialise;
      LSumOutSequencesWithFailuresBlockValues.Initialise;
      LSumOutSequencesWithFailuresBlockTotal.Initialise;

      LSumOutSequencesWithFailuresBlockHeading.BlockNumber       := ABLockData.BlockNumber;
      LSumOutSequencesWithFailuresBlockHeading.LoadCaseNumber    := ABLockData.LoadCaseNumber;
      LSumOutSequencesWithFailuresBlockHeading.SequenceNumber    := ABLockData.SequenceNumber;
      LSumOutSequencesWithFailuresBlockHeading.ElementID         := ABLockData.ElementID;
      LSumOutSequencesWithFailuresBlockHeading.AnnualWaterDemand := ABLockData.AnnualWaterDemand;
      LSumOutSequencesWithFailuresBlockHeading.AnnualPowerDemand := ABLockData.AnnualPowerDemand;

      LSumOutSequencesWithFailuresBlockValues.BlockNumber       := ABLockData.BlockNumber;
      LSumOutSequencesWithFailuresBlockValues.LoadCaseNumber    := ABLockData.LoadCaseNumber;
      LSumOutSequencesWithFailuresBlockValues.SequenceNumber    := ABLockData.SequenceNumber;

      LSumOutSequencesWithFailuresBlockTotal.BlockNumber       := ABLockData.BlockNumber;
      LSumOutSequencesWithFailuresBlockTotal.LoadCaseNumber    := ABLockData.LoadCaseNumber;
      LSumOutSequencesWithFailuresBlockTotal.SequenceNumber    := ABLockData.SequenceNumber;

      Result :=
        LSumOutSequencesWithFailuresBlockHeading.ReadFromBlockData(ABLockData) and
        LSumOutSequencesWithFailuresBlockValues.ReadFromBlockData(ABLockData) and
        LSumOutSequencesWithFailuresBlockTotal.ReadFromBlockData(ABLockData);

    finally
      FreeAndNil(LSumOutSequencesWithFailuresBlockHeading);
      FreeAndNil(LSumOutSequencesWithFailuresBlockValues);
      FreeAndNil(LSumOutSequencesWithFailuresBlockTotal);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TSumOutSequencesWithFailuresBlockAgent.ImportFileBlock(ABLockData: TDataBlock;
         AProgressFunction: TProgressUpdateFuntion): boolean;
const OPNAME = 'TSumOutSequencesWithFailuresBlockAgent.ImportFileBlock';
var
  LDatabaseName,
  LModel,
  LStudy,
  LSubArea,
  LScenario: string;

  LSumOutSequencesWithFailuresBlockHeading : TSumOutSequencesWithFailuresBlockHeading;
  LSumOutSequencesWithFailuresBlockValues  : TSumOutSequencesWithFailuresBlockValues;
  LSumOutSequencesWithFailuresBlockTotal : TSumOutSequencesWithFailuresBlockTotal;
begin
  Result := False;
  try
    if not Assigned(ABLockData) then
      raise Exception.Create('Block data parameter is not yet assigned.');

    if not Assigned(AProgressFunction) then
      AProgressFunction := DummyShowProgress;

    LSumOutSequencesWithFailuresBlockHeading := TSumOutSequencesWithFailuresBlockHeading.Create(FAppModules);
    LSumOutSequencesWithFailuresBlockValues  := TSumOutSequencesWithFailuresBlockValues.Create(FAppModules);
    LSumOutSequencesWithFailuresBlockTotal   := TSumOutSequencesWithFailuresBlockTotal.Create(FAppModules);
    try
      LSumOutSequencesWithFailuresBlockHeading.Initialise;
      LSumOutSequencesWithFailuresBlockValues.Initialise;
      LSumOutSequencesWithFailuresBlockTotal.Initialise;

      LSumOutSequencesWithFailuresBlockHeading.BlockNumber       := ABLockData.BlockNumber;
      LSumOutSequencesWithFailuresBlockHeading.LoadCaseNumber    := ABLockData.LoadCaseNumber;
      LSumOutSequencesWithFailuresBlockHeading.SequenceNumber    := ABLockData.SequenceNumber;
      LSumOutSequencesWithFailuresBlockHeading.ElementID         := ABLockData.ElementID;
      LSumOutSequencesWithFailuresBlockHeading.AnnualWaterDemand := ABLockData.AnnualWaterDemand;
      LSumOutSequencesWithFailuresBlockHeading.AnnualPowerDemand := ABLockData.AnnualPowerDemand;

      LSumOutSequencesWithFailuresBlockValues.BlockNumber       := ABLockData.BlockNumber;
      LSumOutSequencesWithFailuresBlockValues.LoadCaseNumber    := ABLockData.LoadCaseNumber;
      LSumOutSequencesWithFailuresBlockValues.SequenceNumber    := ABLockData.SequenceNumber;

      LSumOutSequencesWithFailuresBlockTotal.BlockNumber       := ABLockData.BlockNumber;
      LSumOutSequencesWithFailuresBlockTotal.LoadCaseNumber    := ABLockData.LoadCaseNumber;
      LSumOutSequencesWithFailuresBlockTotal.SequenceNumber    := ABLockData.SequenceNumber;

      Result :=
        LSumOutSequencesWithFailuresBlockHeading.ReadFromBlockData(ABLockData) and
        LSumOutSequencesWithFailuresBlockValues.ReadFromBlockData(ABLockData) and
        LSumOutSequencesWithFailuresBlockTotal.ReadFromBlockData(ABLockData);

      if Result then
      begin
        LDatabaseName := FAppModules.Database.DatabaseName;
        LModel        := FAppModules.StudyArea.ModelCode;
        LStudy        := FAppModules.StudyArea.StudyAreaCode;
        LSubArea      := FAppModules.StudyArea.SubAreaCode;
        LScenario     := FAppModules.StudyArea.ScenarioCode;

        Result := LSumOutSequencesWithFailuresBlockHeading.SaveToDatabase(LDatabaseName,LModel,LStudy,LSubArea,LScenario) and
                  LSumOutSequencesWithFailuresBlockValues.SaveToDatabase(LDatabaseName,LModel,LStudy,LSubArea,LScenario) and
                  LSumOutSequencesWithFailuresBlockTotal.SaveToDatabase(LDatabaseName,LModel,LStudy,LSubArea,LScenario);
      end;

    finally
      FreeAndNil(LSumOutSequencesWithFailuresBlockHeading);
      FreeAndNil(LSumOutSequencesWithFailuresBlockValues);
      FreeAndNil(LSumOutSequencesWithFailuresBlockTotal);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TSumOutSequencesWithFailuresBlockAgent.ExportFileBlock(ADataset: TDataset; AOutputStream:TFileStream;
         AProgressFunction: TProgressUpdateFuntion): boolean;
const OPNAME = 'TSumOutSequencesWithFailuresBlockAgent.ExportFileBlock';
var
  LSumOutSequencesWithFailuresBlockHeading : TSumOutSequencesWithFailuresBlockHeading;
  LSumOutSequencesWithFailuresBlockValues  : TSumOutSequencesWithFailuresBlockValues;
  LSumOutSequencesWithFailuresBlockTotal : TSumOutSequencesWithFailuresBlockTotal;
begin
  Result := False;
  try
    if not Assigned(ADataset) then
      raise Exception.Create('Dataset parameter is not yet assigned.');

    if not Assigned(AProgressFunction) then
      AProgressFunction := DummyShowProgress;

    LSumOutSequencesWithFailuresBlockHeading := TSumOutSequencesWithFailuresBlockHeading.Create(FAppModules);
    LSumOutSequencesWithFailuresBlockValues  := TSumOutSequencesWithFailuresBlockValues.Create(FAppModules);
    LSumOutSequencesWithFailuresBlockTotal   := TSumOutSequencesWithFailuresBlockTotal.Create(FAppModules);
    try

      LSumOutSequencesWithFailuresBlockHeading.Initialise;
      LSumOutSequencesWithFailuresBlockValues.Initialise;
      LSumOutSequencesWithFailuresBlockTotal.Initialise;

      Result :=
        LSumOutSequencesWithFailuresBlockHeading.ReadFromDatabase(FAppModules.Database.DatabaseName,ADataset) and
        LSumOutSequencesWithFailuresBlockValues.ReadFromDatabase(FAppModules.Database.DatabaseName,ADataset) and
        LSumOutSequencesWithFailuresBlockTotal.ReadFromDatabase(FAppModules.Database.DatabaseName,ADataset);

      if Result then
      begin
        LSumOutSequencesWithFailuresBlockHeading.SaveToStream(AOutputStream);
        LSumOutSequencesWithFailuresBlockValues.SaveToStream(AOutputStream);
        LSumOutSequencesWithFailuresBlockTotal.SaveToStream(AOutputStream);
      end;


    finally
      FreeAndNil(LSumOutSequencesWithFailuresBlockHeading);
      FreeAndNil(LSumOutSequencesWithFailuresBlockValues);
      FreeAndNil(LSumOutSequencesWithFailuresBlockTotal);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
