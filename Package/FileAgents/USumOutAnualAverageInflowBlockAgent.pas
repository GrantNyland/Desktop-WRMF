//
//
//  UNIT      : Contains TSumOutAnualAverageInflowBlockAgent Class
//  AUTHOR    : Dziedzi Ramulondi(PDNA)
//  DATE      : 02/07/2002
//  COPYRIGHT : Copyright © 2002 DWAF
//
//
unit USumOutAnualAverageInflowBlockAgent;

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

  TSumOutAnualAverageInflowBlockAgent = class(TAbstractAppObject)
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

{ TSumOutAnualAverageInflowBlockAgent }

procedure TSumOutAnualAverageInflowBlockAgent.CreateMemberObjects;
const OPNAME = 'TSumOutAnualAverageInflowBlockAgent.CreateMemberObjects';
begin
  inherited CreateMemberObjects;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TSumOutAnualAverageInflowBlockAgent.DestroyMemberObjects;
const OPNAME = 'TSumOutAnualAverageInflowBlockAgent.DestroyMemberObjects';
begin
  inherited DestroyMemberObjects;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TSumOutAnualAverageInflowBlockAgent.Initialise: boolean;
const OPNAME = 'TSumOutAnualAverageInflowBlockAgent.Initialise';
begin
  Result := inherited Initialise;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TSumOutAnualAverageInflowBlockAgent.Validate(ABLockData: TDataBlock; AProgressFunction: TProgressUpdateFuntion): boolean;
const OPNAME = 'TSumOutAnualAverageInflowBlockAgent.Validate';
var
  LSumOutAnualAverageInflowBlockHeading : TSumOutAnualAverageInflowBlockHeading;
  LSumOutAnualAverageInflowBlockValues  : TSumOutAnualAverageInflowBlockValues;
begin
  Result := False;
  try
    if not Assigned(ABLockData) then
      raise Exception.Create('Block data parameter is not yet assigned.');

    if not Assigned(AProgressFunction) then
      AProgressFunction := DummyShowProgress;

    LSumOutAnualAverageInflowBlockHeading := TSumOutAnualAverageInflowBlockHeading.Create(FAppModules);
    LSumOutAnualAverageInflowBlockValues  := TSumOutAnualAverageInflowBlockValues.Create(FAppModules);
    try
      LSumOutAnualAverageInflowBlockHeading.Initialise;
      LSumOutAnualAverageInflowBlockValues.Initialise;

      LSumOutAnualAverageInflowBlockHeading.BlockNumber       := ABLockData.BlockNumber;
      LSumOutAnualAverageInflowBlockHeading.LoadCaseNumber    := ABLockData.LoadCaseNumber;
      LSumOutAnualAverageInflowBlockHeading.SequenceNumber    := ABLockData.SequenceNumber;
      LSumOutAnualAverageInflowBlockHeading.ElementID         := ABLockData.ElementID;
      LSumOutAnualAverageInflowBlockHeading.AnnualWaterDemand := ABLockData.AnnualWaterDemand;
      LSumOutAnualAverageInflowBlockHeading.AnnualPowerDemand := ABLockData.AnnualPowerDemand;

      LSumOutAnualAverageInflowBlockValues.BlockNumber       := ABLockData.BlockNumber;
      LSumOutAnualAverageInflowBlockValues.LoadCaseNumber    := ABLockData.LoadCaseNumber;
      LSumOutAnualAverageInflowBlockValues.SequenceNumber    := ABLockData.SequenceNumber;

      Result :=
        LSumOutAnualAverageInflowBlockHeading.ReadFromBlockData(ABLockData) and
        LSumOutAnualAverageInflowBlockValues.ReadFromBlockData(ABLockData);

    finally
      FreeAndNil(LSumOutAnualAverageInflowBlockHeading);
      FreeAndNil(LSumOutAnualAverageInflowBlockValues);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TSumOutAnualAverageInflowBlockAgent.ImportFileBlock(ABLockData: TDataBlock; AProgressFunction: TProgressUpdateFuntion): boolean;
const OPNAME = 'TSumOutAnualAverageInflowBlockAgent.ImportFileBlock';
var
  LDatabaseName,
  LModel,
  LStudy,
  LSubArea,
  LScenario: string;

  LSumOutAnualAverageInflowBlockHeading : TSumOutAnualAverageInflowBlockHeading;
  LSumOutAnualAverageInflowBlockValues  : TSumOutAnualAverageInflowBlockValues;
begin
  Result := False;
  try
    if not Assigned(ABLockData) then
      raise Exception.Create('Block data parameter is not yet assigned.');

    if not Assigned(AProgressFunction) then
      AProgressFunction := DummyShowProgress;

    LSumOutAnualAverageInflowBlockHeading := TSumOutAnualAverageInflowBlockHeading.Create(FAppModules);
    LSumOutAnualAverageInflowBlockValues  := TSumOutAnualAverageInflowBlockValues.Create(FAppModules);
    try
      LSumOutAnualAverageInflowBlockHeading.Initialise;
      LSumOutAnualAverageInflowBlockValues.Initialise;

      LSumOutAnualAverageInflowBlockHeading.BlockNumber       := ABLockData.BlockNumber;
      LSumOutAnualAverageInflowBlockHeading.LoadCaseNumber    := ABLockData.LoadCaseNumber;
      LSumOutAnualAverageInflowBlockHeading.SequenceNumber    := ABLockData.SequenceNumber;
      LSumOutAnualAverageInflowBlockHeading.ElementID         := ABLockData.ElementID;
      LSumOutAnualAverageInflowBlockHeading.AnnualWaterDemand := ABLockData.AnnualWaterDemand;
      LSumOutAnualAverageInflowBlockHeading.AnnualPowerDemand := ABLockData.AnnualPowerDemand;

      LSumOutAnualAverageInflowBlockValues.BlockNumber       := ABLockData.BlockNumber;
      LSumOutAnualAverageInflowBlockValues.LoadCaseNumber    := ABLockData.LoadCaseNumber;
      LSumOutAnualAverageInflowBlockValues.SequenceNumber    := ABLockData.SequenceNumber;

      Result :=
        LSumOutAnualAverageInflowBlockHeading.ReadFromBlockData(ABLockData) and
        LSumOutAnualAverageInflowBlockValues.ReadFromBlockData(ABLockData);

      if Result then
      begin
        LDatabaseName := FAppModules.Database.DatabaseName;
        LModel        := FAppModules.StudyArea.ModelCode;
        LStudy        := FAppModules.StudyArea.StudyAreaCode;
        LSubArea      := FAppModules.StudyArea.SubAreaCode;
        LScenario     := FAppModules.StudyArea.ScenarioCode;

        Result := LSumOutAnualAverageInflowBlockHeading.SaveToDatabase(LDatabaseName,LModel,LStudy,LSubArea,LScenario) and
                  LSumOutAnualAverageInflowBlockValues.SaveToDatabase(LDatabaseName,LModel,LStudy,LSubArea,LScenario);
      end;


    finally
      FreeAndNil(LSumOutAnualAverageInflowBlockHeading);
      FreeAndNil(LSumOutAnualAverageInflowBlockValues);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TSumOutAnualAverageInflowBlockAgent.ExportFileBlock(ADataset: TDataset; AOutputStream:TFileStream;
         AProgressFunction: TProgressUpdateFuntion): boolean;
const OPNAME = 'TSumOutAnualAverageInflowBlockAgent.ExportFileBlock';
var
  LSumOutAnualAverageInflowBlockHeading : TSumOutAnualAverageInflowBlockHeading;
  LSumOutAnualAverageInflowBlockValues  : TSumOutAnualAverageInflowBlockValues;
begin
  Result := False;
  try
    if not Assigned(ADataset) then
      raise Exception.Create('Dataset parameter is not yet assigned.');

    if not Assigned(AProgressFunction) then
      AProgressFunction := DummyShowProgress;

    LSumOutAnualAverageInflowBlockHeading := TSumOutAnualAverageInflowBlockHeading.Create(FAppModules);
    LSumOutAnualAverageInflowBlockValues  := TSumOutAnualAverageInflowBlockValues.Create(FAppModules);
    try
      LSumOutAnualAverageInflowBlockHeading.Initialise;
      LSumOutAnualAverageInflowBlockValues.Initialise;

      Result :=
        LSumOutAnualAverageInflowBlockHeading.ReadFromDatabase(FAppModules.Database.DatabaseName,ADataset)  and
        LSumOutAnualAverageInflowBlockValues.ReadFromDatabase(FAppModules.Database.DatabaseName,ADataset) ;
     
      if Result then
      begin
        LSumOutAnualAverageInflowBlockHeading.SaveToStream(AOutputStream);
        LSumOutAnualAverageInflowBlockValues.SaveToStream(AOutputStream);
      end;


    finally
      FreeAndNil(LSumOutAnualAverageInflowBlockHeading);
      FreeAndNil(LSumOutAnualAverageInflowBlockValues);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
