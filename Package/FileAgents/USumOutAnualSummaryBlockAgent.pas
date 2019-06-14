//
//
//  UNIT      : Contains TSumOutAnualSummaryBlockAgent Class
//  AUTHOR    : Dziedzi Ramulondi(PDNA)
//  DATE      : 02/07/2002
//  COPYRIGHT : Copyright © 2002 DWAF
//
//
unit USumOutAnualSummaryBlockAgent;

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

  TSumOutAnualSummaryBlockAgent = class(TAbstractAppObject)
  protected
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
  public
    function Initialise: boolean;override;
    function Validate(ABLockData: TDataBlock; AProgressFunction: TProgressUpdateFuntion): boolean;
    function ImportFileBlock(ABLockData: TDataBlock;AProgressFunction: TProgressUpdateFuntion): boolean;
    function ExportFileBlock(ADataset: TDataset; AOutputStream:TFileStream; AProgressFunction: TProgressUpdateFuntion): boolean;
    function GetDeficitsBlockFromFile(ABLockData: TDataBlock; ADeficitsBlocks : TObjectList): boolean;
    function GetDeficitsBlockFromDB(ADataset: TDataset; ADeficitsBlocks : TObjectList): boolean;
  end;

implementation


uses UUtilities,
     UErrorHandlingOperations;

{ TSumOutAnualSummaryBlockAgent }

procedure TSumOutAnualSummaryBlockAgent.CreateMemberObjects;
const OPNAME = 'TSumOutAnualSummaryBlockAgent.CreateMemberObjects';
begin
  inherited CreateMemberObjects;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TSumOutAnualSummaryBlockAgent.DestroyMemberObjects;
const OPNAME = 'TSumOutAnualSummaryBlockAgent.DestroyMemberObjects';
begin
  inherited DestroyMemberObjects;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TSumOutAnualSummaryBlockAgent.Initialise: boolean;
const OPNAME = 'TSumOutAnualSummaryBlockAgent.Initialise';
begin
  Result := inherited Initialise;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TSumOutAnualSummaryBlockAgent.Validate(ABLockData: TDataBlock; AProgressFunction: TProgressUpdateFuntion): boolean;
const OPNAME = 'TSumOutAnualSummaryBlockAgent.Validate';
var
  LSumOutAnualSummaryBlockHeading : TSumOutAnualSummaryBlockHeading;
  LSumOutAnualSummaryBlockValues  : TSumOutAnualSummaryBlockValues;
begin
  Result := False;
  try
    if not Assigned(ABLockData) then
      raise Exception.Create('Block data parameter is not yet assigned.');

    if not Assigned(AProgressFunction) then
      AProgressFunction := DummyShowProgress;

    LSumOutAnualSummaryBlockHeading := TSumOutAnualSummaryBlockHeading.Create(FAppModules);
    LSumOutAnualSummaryBlockValues  := TSumOutAnualSummaryBlockValues.Create(FAppModules);
    try
      LSumOutAnualSummaryBlockHeading.Initialise;
      LSumOutAnualSummaryBlockValues.Initialise;

      LSumOutAnualSummaryBlockHeading.BlockNumber       := ABLockData.BlockNumber;
      LSumOutAnualSummaryBlockHeading.LoadCaseNumber    := ABLockData.LoadCaseNumber;
      LSumOutAnualSummaryBlockHeading.SequenceNumber    := ABLockData.SequenceNumber;
      LSumOutAnualSummaryBlockHeading.ElementID         := ABLockData.ElementID;
      LSumOutAnualSummaryBlockHeading.AnnualWaterDemand := ABLockData.AnnualWaterDemand;
      LSumOutAnualSummaryBlockHeading.AnnualPowerDemand := ABLockData.AnnualPowerDemand;

      LSumOutAnualSummaryBlockValues.BlockNumber       := ABLockData.BlockNumber;
      LSumOutAnualSummaryBlockValues.LoadCaseNumber    := ABLockData.LoadCaseNumber;
      LSumOutAnualSummaryBlockValues.SequenceNumber    := ABLockData.SequenceNumber;

      Result :=
        LSumOutAnualSummaryBlockHeading.ReadFromBlockData(ABLockData) and
        LSumOutAnualSummaryBlockValues.ReadFromBlockData(ABLockData);

    finally
      FreeAndNil(LSumOutAnualSummaryBlockHeading);
      FreeAndNil(LSumOutAnualSummaryBlockValues);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TSumOutAnualSummaryBlockAgent.ImportFileBlock(ABLockData: TDataBlock; AProgressFunction: TProgressUpdateFuntion): boolean;
const OPNAME = 'TSumOutAnualSummaryBlockAgent.ImportFileBlock';
var
  LDatabaseName,
  LModel,
  LStudy,
  LSubArea,
  LScenario: string;

  LSumOutAnualSummaryBlockHeading : TSumOutAnualSummaryBlockHeading;
  LSumOutAnualSummaryBlockValues  : TSumOutAnualSummaryBlockValues;
begin
  Result := False;
  try
    if not Assigned(ABLockData) then
      raise Exception.Create('Block data parameter is not yet assigned.');

    if not Assigned(AProgressFunction) then
      AProgressFunction := DummyShowProgress;

    LSumOutAnualSummaryBlockHeading := TSumOutAnualSummaryBlockHeading.Create(FAppModules);
    LSumOutAnualSummaryBlockValues  := TSumOutAnualSummaryBlockValues.Create(FAppModules);
    try
      LSumOutAnualSummaryBlockHeading.Initialise;
      LSumOutAnualSummaryBlockValues.Initialise;

      LSumOutAnualSummaryBlockHeading.BlockNumber       := ABLockData.BlockNumber;
      LSumOutAnualSummaryBlockHeading.LoadCaseNumber    := ABLockData.LoadCaseNumber;
      LSumOutAnualSummaryBlockHeading.SequenceNumber    := ABLockData.SequenceNumber;
      LSumOutAnualSummaryBlockHeading.ElementID         := ABLockData.ElementID;
      LSumOutAnualSummaryBlockHeading.AnnualWaterDemand := ABLockData.AnnualWaterDemand;
      LSumOutAnualSummaryBlockHeading.AnnualPowerDemand := ABLockData.AnnualPowerDemand;

      LSumOutAnualSummaryBlockValues.BlockNumber       := ABLockData.BlockNumber;
      LSumOutAnualSummaryBlockValues.LoadCaseNumber    := ABLockData.LoadCaseNumber;
      LSumOutAnualSummaryBlockValues.SequenceNumber    := ABLockData.SequenceNumber;

      Result :=
        LSumOutAnualSummaryBlockHeading.ReadFromBlockData(ABLockData) and
        LSumOutAnualSummaryBlockValues.ReadFromBlockData(ABLockData);

      if Result then
      begin
        LDatabaseName := FAppModules.Database.DatabaseName;
        LModel        := FAppModules.StudyArea.ModelCode;
        LStudy        := FAppModules.StudyArea.StudyAreaCode;
        LSubArea      := FAppModules.StudyArea.SubAreaCode;
        LScenario     := FAppModules.StudyArea.ScenarioCode;

        Result := LSumOutAnualSummaryBlockHeading.SaveToDatabase(LDatabaseName,LModel,LStudy,LSubArea,LScenario) and
                  LSumOutAnualSummaryBlockValues.SaveToDatabase(LDatabaseName,LModel,LStudy,LSubArea,LScenario);
      end;
    finally
      FreeAndNil(LSumOutAnualSummaryBlockHeading);
      FreeAndNil(LSumOutAnualSummaryBlockValues);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TSumOutAnualSummaryBlockAgent.ExportFileBlock(ADataset: TDataset; AOutputStream:TFileStream;
         AProgressFunction: TProgressUpdateFuntion): boolean;
const OPNAME = 'TSumOutAnualSummaryBlockAgent.ExportFileBlock';
var
  LSumOutAnualSummaryBlockHeading : TSumOutAnualSummaryBlockHeading;
  LSumOutAnualSummaryBlockValues  : TSumOutAnualSummaryBlockValues;
begin
  Result := False;
  try
    if not Assigned(ADataset) then
      raise Exception.Create('Dataset parameter is not yet assigned.');

    if not Assigned(AProgressFunction) then
      AProgressFunction := DummyShowProgress;

    LSumOutAnualSummaryBlockHeading := TSumOutAnualSummaryBlockHeading.Create(FAppModules);
    LSumOutAnualSummaryBlockValues  := TSumOutAnualSummaryBlockValues.Create(FAppModules);
    try

      LSumOutAnualSummaryBlockHeading.Initialise;
      LSumOutAnualSummaryBlockValues.Initialise;

      Result :=
        LSumOutAnualSummaryBlockHeading.ReadFromDatabase(FAppModules.Database.DatabaseName,ADataset) and
        LSumOutAnualSummaryBlockValues.ReadFromDatabase(FAppModules.Database.DatabaseName,ADataset);

      if Result then
      begin
        LSumOutAnualSummaryBlockHeading.SaveToStream(AOutputStream);
        LSumOutAnualSummaryBlockValues.SaveToStream(AOutputStream);
      end;
    finally
      FreeAndNil(LSumOutAnualSummaryBlockHeading);
      FreeAndNil(LSumOutAnualSummaryBlockValues);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TSumOutAnualSummaryBlockAgent.GetDeficitsBlockFromFile(ABLockData: TDataBlock;ADeficitsBlocks: TObjectList): boolean;
const OPNAME = 'TSumOutAnualSummaryBlockAgent.GetDeficitsBlockFromFile';
var
  LSumOutAnualSummaryBlockHeading : TSumOutAnualSummaryBlockHeading;
  LSumOutAnualSummaryBlockValues  : TSumOutAnualSummaryBlockValues;
begin
  Result := False;
  try
    if not Assigned(ABLockData) then
      raise Exception.Create('Block data parameter is not yet assigned.');

    if not Assigned(ADeficitsBlocks) then
      raise Exception.Create('Deficits blocks container parameter is not yet assigned.');

    LSumOutAnualSummaryBlockHeading := TSumOutAnualSummaryBlockHeading.Create(FAppModules);
    LSumOutAnualSummaryBlockValues  := TSumOutAnualSummaryBlockValues.Create(FAppModules);

    try
      LSumOutAnualSummaryBlockHeading.Initialise;
      LSumOutAnualSummaryBlockValues.Initialise;

      LSumOutAnualSummaryBlockHeading.BlockNumber       := ABLockData.BlockNumber;
      LSumOutAnualSummaryBlockHeading.LoadCaseNumber    := ABLockData.LoadCaseNumber;
      LSumOutAnualSummaryBlockHeading.SequenceNumber    := ABLockData.SequenceNumber;
      LSumOutAnualSummaryBlockHeading.ElementID         := ABLockData.ElementID;
      LSumOutAnualSummaryBlockHeading.AnnualWaterDemand := ABLockData.AnnualWaterDemand;
      LSumOutAnualSummaryBlockHeading.AnnualPowerDemand := ABLockData.AnnualPowerDemand;

      LSumOutAnualSummaryBlockValues.BlockNumber       := ABLockData.BlockNumber;
      LSumOutAnualSummaryBlockValues.LoadCaseNumber    := ABLockData.LoadCaseNumber;
      LSumOutAnualSummaryBlockValues.SequenceNumber    := ABLockData.SequenceNumber;

      Result :=
        LSumOutAnualSummaryBlockHeading.ReadFromBlockData(ABLockData) and
        LSumOutAnualSummaryBlockValues.ReadFromBlockData(ABLockData);

      if Result then
         ADeficitsBlocks.Add(LSumOutAnualSummaryBlockValues)
      else
        FreeAndNil(LSumOutAnualSummaryBlockValues);

    finally
      FreeAndNil(LSumOutAnualSummaryBlockHeading);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TSumOutAnualSummaryBlockAgent.GetDeficitsBlockFromDB( ADataset: TDataset; ADeficitsBlocks: TObjectList): boolean;
const OPNAME = 'TSumOutAnualSummaryBlockAgent.GetDeficitsBlockFromDB';
var
  LSumOutAnualSummaryBlockValues  : TSumOutAnualSummaryBlockValues;
begin
  Result := False;
  try
    if not Assigned(ADataset) then
      raise Exception.Create('Dataset parameter is not yet assigned.');

    if not Assigned(ADeficitsBlocks) then
      raise Exception.Create('Deficits blocks container parameter is not yet assigned.');

    LSumOutAnualSummaryBlockValues  := TSumOutAnualSummaryBlockValues.Create(FAppModules);

      LSumOutAnualSummaryBlockValues.Initialise;

      Result :=
        LSumOutAnualSummaryBlockValues.ReadFromDatabase(FAppModules.Database.DatabaseName,ADataset);

      if Result then
        ADeficitsBlocks.Add(LSumOutAnualSummaryBlockValues)
      else
        FreeAndNil(LSumOutAnualSummaryBlockValues);

  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
