//
//
//  UNIT      : Contains TSumOutRecurrenceIntervalBlockAgent Class
//  AUTHOR    : Sam Dhlamini(Cornastone)
//  DATE      : 08/03/2005
//  COPYRIGHT : Copyright © 2004 DWAF
//
//

unit USumOutRecurrenceIntervalBlockAgent;

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

  TSumOutRecurrenceIntervalBlockAgent = class ( TAbstractAppObject )
  protected
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
  public
    function Initialise: boolean;override;
    function Validate (ABLockData : TDataBlock;AProgressFunction : TProgressUpdateFuntion ): boolean;
    function ImportFileBlock (ABLockData : TDataBlock; AProgressFunction : TProgressUpdateFuntion ) : boolean;
    function ExportFileBlock ( ADataset : TDataset; AOutputStream : TFileStream;AProgressFunction : TProgressUpdateFuntion ) : boolean;
    function GetRecurrenceIntervalBlockFromFile (ABLockData: TDataBlock;ARecurrenceIntervalBlocks : TObjectList ) : boolean;
    function GetNumberOfFailureSeqBlockFromFile (ABLockData: TDataBlock;ANumberOfFailureSeqBlocks : TObjectList ) : boolean;
end;

implementation
uses UUtilities,
     UErrorHandlingOperations;
{ TSumOutRecurrenceIntervalBlockAgent }

procedure TSumOutRecurrenceIntervalBlockAgent.CreateMemberObjects;
const OPNAME = 'TSumOutRecurrenceIntervalBlockAgent.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;

  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TSumOutRecurrenceIntervalBlockAgent.DestroyMemberObjects;
const OPNAME = 'TSumOutRecurrenceIntervalBlockAgent.DestroyMemberObjects';
begin
  try
    inherited DestroyMemberObjects;

  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TSumOutRecurrenceIntervalBlockAgent.ExportFileBlock ( ADataset : TDataset; AOutputStream: TFileStream;
                                                               AProgressFunction : TProgressUpdateFuntion ) : boolean;
const OPNAME = 'TSumOutRecurrenceIntervalBlockAgent.ExportFileBlock';
begin
  Result := False;
  try

  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TSumOutRecurrenceIntervalBlockAgent.GetNumberOfFailureSeqBlockFromFile(ABLockData : TDataBlock;
         ANumberOfFailureSeqBlocks : TObjectList ): boolean;
const OPNAME = 'TSumOutRecurrenceIntervalBlockAgent.GetNumberOfFailureSeqBlockFromFile';
var
  lSumOutNumberOfFailureSeqBlockHeading : TSumOutRecurrenceIntervalBlockHeading;
  lSumOutNumberOfFailureSeqBlockValues  : TSumOutRecurrenceIntervalBlockValues;
begin
  Result := False;
  try
    if not Assigned(ABLockData) then
      raise Exception.Create('Block data parameter is not yet assigned.');

    if not Assigned(ANumberOfFailureSeqBlocks) then
      raise Exception.Create('Number of Failure Seq. Blocks container parameter is not yet assigned.');

    lSumOutNumberOfFailureSeqBlockHeading := TSumOutRecurrenceIntervalBlockHeading.Create(FAppModules);
    lSumOutNumberOfFailureSeqBlockValues  := TSumOutRecurrenceIntervalBlockValues.Create(FAppModules);

    try
      lSumOutNumberOfFailureSeqBlockHeading.Initialise;
      lSumOutNumberOfFailureSeqBlockValues.Initialise;

      lSumOutNumberOfFailureSeqBlockHeading.BlockNumber       := ABLockData.BlockNumber;
      lSumOutNumberOfFailureSeqBlockHeading.LoadCaseNumber    := ABLockData.LoadCaseNumber;
      lSumOutNumberOfFailureSeqBlockHeading.SequenceNumber    := ABLockData.SequenceNumber;
      lSumOutNumberOfFailureSeqBlockHeading.AnnualWaterDemand := ABLockData.AnnualWaterDemand;
      lSumOutNumberOfFailureSeqBlockHeading.AnnualPowerDemand := ABLockData.AnnualPowerDemand;

      lSumOutNumberOfFailureSeqBlockValues.BlockNumber       := ABLockData.BlockNumber;
      lSumOutNumberOfFailureSeqBlockValues.LoadCaseNumber    := ABLockData.LoadCaseNumber;
      lSumOutNumberOfFailureSeqBlockValues.SequenceNumber    := ABLockData.SequenceNumber;

      Result :=
        lSumOutNumberOfFailureSeqBlockHeading.ReadFromBlockData ( ABLockData ) and
        lSumOutNumberOfFailureSeqBlockValues.ReadFromBlockData ( ABLockData );

      if Result then
         ANumberOfFailureSeqBlocks.Add ( lSumOutNumberOfFailureSeqBlockValues )
      else
        FreeAndNil ( lSumOutNumberOfFailureSeqBlockValues );

    finally
      FreeAndNil ( lSumOutNumberOfFailureSeqBlockHeading );
    end;

  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TSumOutRecurrenceIntervalBlockAgent.GetRecurrenceIntervalBlockFromFile(ABLockData : TDataBlock;
         ARecurrenceIntervalBlocks : TObjectList ): boolean;
const OPNAME = 'TSumOutRecurrenceIntervalBlockAgent.GetRecurrenceIntervalBlockFromFile';
var
  lSumOutRecurrenceIntervalBlockHeading : TSumOutRecurrenceIntervalBlockHeading;
  lSumOutRecurrenceIntervalBlockValues  : TSumOutRecurrenceIntervalBlockValues;
begin
  Result := False;
  try
    if not Assigned(ABLockData) then
      raise Exception.Create('Block data parameter is not yet assigned.');

    if not Assigned(ARecurrenceIntervalBlocks) then
      raise Exception.Create('Number of Failure Seq. Blocks container parameter is not yet assigned.');

    lSumOutRecurrenceIntervalBlockHeading := TSumOutRecurrenceIntervalBlockHeading.Create(FAppModules);
    lSumOutRecurrenceIntervalBlockValues  := TSumOutRecurrenceIntervalBlockValues.Create(FAppModules);

    try
      lSumOutRecurrenceIntervalBlockHeading.Initialise;
      lSumOutRecurrenceIntervalBlockValues.Initialise;

      lSumOutRecurrenceIntervalBlockHeading.BlockNumber       := ABLockData.BlockNumber;
      lSumOutRecurrenceIntervalBlockHeading.LoadCaseNumber    := ABLockData.LoadCaseNumber;
      lSumOutRecurrenceIntervalBlockHeading.SequenceNumber    := ABLockData.SequenceNumber;
      lSumOutRecurrenceIntervalBlockHeading.AnnualWaterDemand := ABLockData.AnnualWaterDemand;
      lSumOutRecurrenceIntervalBlockHeading.AnnualPowerDemand := ABLockData.AnnualPowerDemand;

      lSumOutRecurrenceIntervalBlockValues.BlockNumber       := ABLockData.BlockNumber;
      lSumOutRecurrenceIntervalBlockValues.LoadCaseNumber    := ABLockData.LoadCaseNumber;
      lSumOutRecurrenceIntervalBlockValues.SequenceNumber    := ABLockData.SequenceNumber;

      Result :=
        lSumOutRecurrenceIntervalBlockHeading.ReadFromBlockData ( ABLockData ) and
        lSumOutRecurrenceIntervalBlockValues.ReadFromBlockData ( ABLockData );

      if Result then
         ARecurrenceIntervalBlocks.Add ( lSumOutRecurrenceIntervalBlockValues )
      else
        FreeAndNil ( lSumOutRecurrenceIntervalBlockValues );
    finally
      FreeAndNil ( lSumOutRecurrenceIntervalBlockHeading );
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TSumOutRecurrenceIntervalBlockAgent.ImportFileBlock (ABLockData : TDataBlock;AProgressFunction : TProgressUpdateFuntion ) : boolean;
const OPNAME = 'TSumOutRecurrenceIntervalBlockAgent.ImportFileBlock';
var
  LDatabaseName,
  LModel,
  LStudy,
  LSubArea,
  LScenario: string;

  LSumOutRecurrenceIntervalBlockHeading : TSumOutRecurrenceIntervalBlockHeading;
  LSumOutRecurrenceIntervalBlockValues  : TSumOutRecurrenceIntervalBlockValues;
begin
  Result := False;
  try
    if not Assigned ( ABLockData ) then
      raise Exception.Create ( 'Block data parameter is not yet assigned.' );

    if not Assigned(AProgressFunction) then
      AProgressFunction := DummyShowProgress;

    LSumOutRecurrenceIntervalBlockHeading := TSumOutRecurrenceIntervalBlockHeading.Create ( FAppModules );
    LSumOutRecurrenceIntervalBlockValues  := TSumOutRecurrenceIntervalBlockValues.Create ( FAppModules );
    try
      LSumOutRecurrenceIntervalBlockHeading.Initialise;
      LSumOutRecurrenceIntervalBlockValues.Initialise;

//      LSumOutRecurrenceIntervalBlockHeading.FReserviorIDsAgent := ReserviorIDsAgent;
      LSumOutRecurrenceIntervalBlockHeading.BlockNumber       := ABLockData.BlockNumber;
      LSumOutRecurrenceIntervalBlockHeading.LoadCaseNumber    := ABLockData.LoadCaseNumber;
      LSumOutRecurrenceIntervalBlockHeading.SequenceNumber    := ABLockData.SequenceNumber;
      LSumOutRecurrenceIntervalBlockHeading.AnnualWaterDemand := ABLockData.AnnualWaterDemand;
      LSumOutRecurrenceIntervalBlockHeading.AnnualPowerDemand := ABLockData.AnnualPowerDemand;

      LSumOutRecurrenceIntervalBlockValues.BlockNumber       := ABLockData.BlockNumber;
      LSumOutRecurrenceIntervalBlockValues.LoadCaseNumber    := ABLockData.LoadCaseNumber;
      LSumOutRecurrenceIntervalBlockValues.SequenceNumber    := ABLockData.SequenceNumber;

      Result :=
        LSumOutRecurrenceIntervalBlockHeading.ReadFromBlockData(ABLockData) and
        LSumOutRecurrenceIntervalBlockValues.ReadFromBlockData(ABLockData);

      if Result then
      begin
        LDatabaseName := FAppModules.Database.DatabaseName;
        LModel        := FAppModules.StudyArea.ModelCode;
        LStudy        := FAppModules.StudyArea.StudyAreaCode;
        LSubArea      := FAppModules.StudyArea.SubAreaCode;
        LScenario     := FAppModules.StudyArea.ScenarioCode;

        Result := LSumOutRecurrenceIntervalBlockHeading.SaveToDatabase(LDatabaseName,LModel,LStudy,LSubArea,LScenario) and
                  LSumOutRecurrenceIntervalBlockValues.SaveToDatabase ( LDatabaseName, LModel, LStudy, LSubArea, LScenario );
      end;


    finally
      FreeAndNil ( LSumOutRecurrenceIntervalBlockHeading );
      FreeAndNil ( LSumOutRecurrenceIntervalBlockValues );
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TSumOutRecurrenceIntervalBlockAgent.Initialise: boolean;
const OPNAME = 'TSumOutRecurrenceIntervalBlockAgent.Initialise';
begin
  Result := inherited Initialise;
  try

  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TSumOutRecurrenceIntervalBlockAgent.Validate (ABLockData : TDataBlock;AProgressFunction : TProgressUpdateFuntion ) : boolean;
const OPNAME = 'TSumOutRecurrenceIntervalBlockAgent.Validate';
begin
  Result := false;
  try

  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

end.
