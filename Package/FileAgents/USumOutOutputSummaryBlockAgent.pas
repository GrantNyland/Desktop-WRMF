//
//
//  UNIT      : Contains TSumOutOutputSummaryBlockAgent Class
//  AUTHOR    : Dziedzi Ramulondi(PDNA)
//  DATE      : 02/07/2002
//  COPYRIGHT : Copyright © 2002 DWAF
//
//
unit USumOutOutputSummaryBlockAgent;

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

  TSumOutOutputSummaryBlockAgent = class(TAbstractAppObject)
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

{ TSumOutOutputSummaryBlockAgent }

procedure TSumOutOutputSummaryBlockAgent.CreateMemberObjects;
const OPNAME = 'TSumOutOutputSummaryBlockAgent.CreateMemberObjects';
begin
  inherited CreateMemberObjects;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TSumOutOutputSummaryBlockAgent.DestroyMemberObjects;
const OPNAME = 'TSumOutOutputSummaryBlockAgent.DestroyMemberObjects';
begin
  inherited DestroyMemberObjects;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TSumOutOutputSummaryBlockAgent.Initialise: boolean;
const OPNAME = 'TSumOutOutputSummaryBlockAgent.Initialise';
begin
  Result := inherited Initialise;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TSumOutOutputSummaryBlockAgent.Validate(ABLockData: TDataBlock; AProgressFunction: TProgressUpdateFuntion): boolean;
const OPNAME = 'TSumOutOutputSummaryBlockAgent.Validate';
var
  LSumOutOutputSummaryBlockHeading : TSumOutOutputSummaryBlockHeading;
  LSumOutOutputSummaryBlockValues  : TSumOutOutputSummaryBlockValues;
  LSumOutOutputSummaryBlockTotal : TSumOutOutputSummaryBlockTotal;
begin
  Result := False;
  try
    if not Assigned(ABLockData) then
      raise Exception.Create('Block data parameter is not yet assigned.');

    if not Assigned(AProgressFunction) then
      AProgressFunction := DummyShowProgress;

    LSumOutOutputSummaryBlockHeading := TSumOutOutputSummaryBlockHeading.Create(FAppModules);
    LSumOutOutputSummaryBlockValues  := TSumOutOutputSummaryBlockValues.Create(FAppModules);
    LSumOutOutputSummaryBlockTotal := TSumOutOutputSummaryBlockTotal.Create(FAppModules);
    try
      LSumOutOutputSummaryBlockHeading.Initialise;
      LSumOutOutputSummaryBlockValues.Initialise;
      LSumOutOutputSummaryBlockTotal.Initialise;

      LSumOutOutputSummaryBlockHeading.BlockNumber       := ABLockData.BlockNumber;
      LSumOutOutputSummaryBlockHeading.LoadCaseNumber    := ABLockData.LoadCaseNumber;
      LSumOutOutputSummaryBlockHeading.SequenceNumber    := ABLockData.SequenceNumber;
      LSumOutOutputSummaryBlockHeading.ElementID         := ABLockData.ElementID;
      LSumOutOutputSummaryBlockHeading.AnnualWaterDemand := ABLockData.AnnualWaterDemand;
      LSumOutOutputSummaryBlockHeading.AnnualPowerDemand := ABLockData.AnnualPowerDemand;

      LSumOutOutputSummaryBlockValues.BlockNumber       := ABLockData.BlockNumber;
      LSumOutOutputSummaryBlockValues.LoadCaseNumber    := ABLockData.LoadCaseNumber;
      LSumOutOutputSummaryBlockValues.SequenceNumber    := ABLockData.SequenceNumber;

      LSumOutOutputSummaryBlockTotal.BlockNumber       := ABLockData.BlockNumber;
      LSumOutOutputSummaryBlockTotal.LoadCaseNumber    := ABLockData.LoadCaseNumber;
      LSumOutOutputSummaryBlockTotal.SequenceNumber    := ABLockData.SequenceNumber;

      Result :=
        LSumOutOutputSummaryBlockHeading.ReadFromBlockData(ABLockData) and
        LSumOutOutputSummaryBlockValues.ReadFromBlockData(ABLockData) and
        LSumOutOutputSummaryBlockTotal.ReadFromBlockData(ABLockData);

    finally
      FreeAndNil(LSumOutOutputSummaryBlockHeading);
      FreeAndNil(LSumOutOutputSummaryBlockValues);
      FreeAndNil(LSumOutOutputSummaryBlockTotal);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TSumOutOutputSummaryBlockAgent.ImportFileBlock(ABLockData: TDataBlock; AProgressFunction: TProgressUpdateFuntion): boolean;
const OPNAME = 'TSumOutOutputSummaryBlockAgent.ImportFileBlock';
var
  LDatabaseName,
  LModel,
  LStudy,
  LSubArea,
  LScenario: string;

  LSumOutOutputSummaryBlockHeading : TSumOutOutputSummaryBlockHeading;
  LSumOutOutputSummaryBlockValues  : TSumOutOutputSummaryBlockValues;
  LSumOutOutputSummaryBlockTotal : TSumOutOutputSummaryBlockTotal;
begin
  Result := False;
  try
    if not Assigned(ABLockData) then
      raise Exception.Create('Block data parameter is not yet assigned.');

    if not Assigned(AProgressFunction) then
      AProgressFunction := DummyShowProgress;

    LSumOutOutputSummaryBlockHeading := TSumOutOutputSummaryBlockHeading.Create(FAppModules);
    LSumOutOutputSummaryBlockValues  := TSumOutOutputSummaryBlockValues.Create(FAppModules);
    LSumOutOutputSummaryBlockTotal := TSumOutOutputSummaryBlockTotal.Create(FAppModules);
    try
      LSumOutOutputSummaryBlockHeading.Initialise;
      LSumOutOutputSummaryBlockValues.Initialise;
      LSumOutOutputSummaryBlockTotal.Initialise;

      LSumOutOutputSummaryBlockHeading.BlockNumber       := ABLockData.BlockNumber;
      LSumOutOutputSummaryBlockHeading.LoadCaseNumber    := ABLockData.LoadCaseNumber;
      LSumOutOutputSummaryBlockHeading.SequenceNumber    := ABLockData.SequenceNumber;
      LSumOutOutputSummaryBlockHeading.ElementID         := ABLockData.ElementID;
      LSumOutOutputSummaryBlockHeading.AnnualWaterDemand := ABLockData.AnnualWaterDemand;
      LSumOutOutputSummaryBlockHeading.AnnualPowerDemand := ABLockData.AnnualPowerDemand;

      LSumOutOutputSummaryBlockValues.BlockNumber       := ABLockData.BlockNumber;
      LSumOutOutputSummaryBlockValues.LoadCaseNumber    := ABLockData.LoadCaseNumber;
      LSumOutOutputSummaryBlockValues.SequenceNumber    := ABLockData.SequenceNumber;

      LSumOutOutputSummaryBlockTotal.BlockNumber       := ABLockData.BlockNumber;
      LSumOutOutputSummaryBlockTotal.LoadCaseNumber    := ABLockData.LoadCaseNumber;
      LSumOutOutputSummaryBlockTotal.SequenceNumber    := ABLockData.SequenceNumber;

      Result :=
        LSumOutOutputSummaryBlockHeading.ReadFromBlockData(ABLockData) and
        LSumOutOutputSummaryBlockValues.ReadFromBlockData(ABLockData) and
        LSumOutOutputSummaryBlockTotal.ReadFromBlockData(ABLockData);

      if Result then
      begin
        LDatabaseName := FAppModules.Database.DatabaseName;
        LModel        := FAppModules.StudyArea.ModelCode;
        LStudy        := FAppModules.StudyArea.StudyAreaCode;
        LSubArea      := FAppModules.StudyArea.SubAreaCode;
        LScenario     := FAppModules.StudyArea.ScenarioCode;

        Result := LSumOutOutputSummaryBlockHeading.SaveToDatabase(LDatabaseName,LModel,LStudy,LSubArea,LScenario) and
                  LSumOutOutputSummaryBlockValues.SaveToDatabase(LDatabaseName,LModel,LStudy,LSubArea,LScenario) and
                  LSumOutOutputSummaryBlockTotal.SaveToDatabase(LDatabaseName,LModel,LStudy,LSubArea,LScenario);
      end;


    finally
      FreeAndNil(LSumOutOutputSummaryBlockHeading);
      FreeAndNil(LSumOutOutputSummaryBlockValues);
      FreeAndNil(LSumOutOutputSummaryBlockTotal);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TSumOutOutputSummaryBlockAgent.ExportFileBlock(ADataset: TDataset; AOutputStream:TFileStream;
         AProgressFunction: TProgressUpdateFuntion): boolean;
const OPNAME = 'TSumOutOutputSummaryBlockAgent.ExportFileBlock';
var
  LSumOutOutputSummaryBlockHeading : TSumOutOutputSummaryBlockHeading;
  LSumOutOutputSummaryBlockValues  : TSumOutOutputSummaryBlockValues;
  LSumOutOutputSummaryBlockTotal : TSumOutOutputSummaryBlockTotal;
begin
  Result := False;
  try
    if not Assigned(ADataset) then
      raise Exception.Create('Dataset parameter is not yet assigned.');

    if not Assigned(AProgressFunction) then
      AProgressFunction := DummyShowProgress;

    LSumOutOutputSummaryBlockHeading := TSumOutOutputSummaryBlockHeading.Create(FAppModules);
    LSumOutOutputSummaryBlockValues  := TSumOutOutputSummaryBlockValues.Create(FAppModules);
    LSumOutOutputSummaryBlockTotal := TSumOutOutputSummaryBlockTotal.Create(FAppModules);
    try
      LSumOutOutputSummaryBlockHeading.Initialise;
      LSumOutOutputSummaryBlockValues.Initialise;
      LSumOutOutputSummaryBlockTotal.Initialise;

      Result :=
        LSumOutOutputSummaryBlockHeading.ReadFromDatabase(FAppModules.Database.DatabaseName,ADataset) and
        LSumOutOutputSummaryBlockValues.ReadFromDatabase(FAppModules.Database.DatabaseName,ADataset) and
        LSumOutOutputSummaryBlockTotal.ReadFromDatabase(FAppModules.Database.DatabaseName,ADataset);
  
      if Result then
      begin
        LSumOutOutputSummaryBlockHeading.SaveToStream(AOutputStream);
        LSumOutOutputSummaryBlockValues.SaveToStream(AOutputStream);
        LSumOutOutputSummaryBlockTotal.SaveToStream(AOutputStream);
      end;


    finally
      FreeAndNil(LSumOutOutputSummaryBlockHeading);
      FreeAndNil(LSumOutOutputSummaryBlockValues);
      FreeAndNil(LSumOutOutputSummaryBlockTotal);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
