//
//
//  UNIT      : Contains TSumOutFileManager Class
//  AUTHOR    : Dziedzi Ramulondi(PDNA)
//  DATE      : 02/07/2002
//  COPYRIGHT : Copyright © 2002 DWAF
//
//
unit USumOutFileManager;

interface

uses
  Classes, sysutils,contnrs,DB,VCL.Dialogs,

  //  DWAF VCL
  UFileNames,
  UOutputData,
  UBasicObjects,
  Uconstants,
  UAbstractObject,
  UAbstractFileNamesObject,
  USumOutDataObjects,
  USumOutMonthlyBlockAgent,
  USumOutYieldFailureBlockAgent,
  USumOutOutputSummaryBlockAgent,
  USumOutAnualSummaryBlockAgent,
  USumOutAnualAverageInflowBlockAgent,
  USumOutSequencesWithFailuresBlockAgent,
  USumOutCriticalPeriodsBlockAgent,
  USumOutRecurrenceIntervalBlockAgent,
  USumOutDatabaseAgent,
  USumOutFileAgent,
  VoaimsCom_TLB,
  UYieldModelDataObject;
const
  ENDOFRUNLABEL1 = 'AVERAGE DRAWDOWN PERIOD';
  ENDOFRUNLABEL2 = 'FIRM YIELD RECURRENCE INTERVALS';
type
  //TFileProgressFunction = function (AResult: boolean): boolean of object;
  TSumOutFileManager = class(TAbstractAppObject)
  protected
    FSumOutMonthlyBlockAgent: TSumOutMonthlyBlockAgent;
    FSumOutYieldFailureBlockAgent: TSumOutYieldFailureBlockAgent;
    FSumOutOutputSummaryBlockAgent: TSumOutOutputSummaryBlockAgent;
    FSumOutAnualSummaryBlockAgent: TSumOutAnualSummaryBlockAgent;
    FSumOutAnualAverageInflowBlockAgent: TSumOutAnualAverageInflowBlockAgent;
    FSumOutSequencesWithFailuresBlockAgent:TSumOutSequencesWithFailuresBlockAgent;
    FSumOutCriticalPeriodsBlockAgent:TSumOutCriticalPeriodsBlockAgent;
    FSumOutRecurrenceIntervalBlockAgent : TSumOutRecurrenceIntervalBlockAgent;
    FSumOutDatabaseAgent: TSumOutDatabaseAgent;
    FSumOutFileAgent: TSumOutFileAgent;


    FSumOutMonthlyBlockHeading               : TSumOutMonthlyBlockHeading;
    FSumOutMonthlyBlockValues                : TSumOutMonthlyBlockValues;
    FSumOutMonthlyBlockAvarage               : TSumOutMonthlyBlockAvarage;

    FSumOutYieldFailureBlockHeading          : TSumOutYieldFailureBlockHeading;
    FSumOutYieldFailureBlockValues           : TSumOutYieldFailureBlockValues;

    FSumOutOutputSummaryBlockHeading         : TSumOutOutputSummaryBlockHeading;
    FSumOutOutputSummaryBlockValues          : TSumOutOutputSummaryBlockValues;
    FSumOutOutputSummaryBlockTotal           : TSumOutOutputSummaryBlockTotal;

    FSumOutAnualSummaryBlockHeading          : TSumOutAnualSummaryBlockHeading;
    FSumOutAnualSummaryBlockValues           : TSumOutAnualSummaryBlockValues;

    FSumOutAnualAverageInflowBlockHeading    : TSumOutAnualAverageInflowBlockHeading;
    FSumOutAnualAverageInflowBlockValues     : TSumOutAnualAverageInflowBlockValues;

    FSumOutSequencesWithFailuresBlockHeading : TSumOutSequencesWithFailuresBlockHeading;
    FSumOutSequencesWithFailuresBlockValues  : TSumOutSequencesWithFailuresBlockValues;
    FSumOutSequencesWithFailuresBlockTotal   : TSumOutSequencesWithFailuresBlockTotal;

    FSumOutCriticalPeriodsBlockHeading       : TSumOutCriticalPeriodsBlockHeading;
    FSumOutCriticalPeriodsBlockValues        : TSumOutCriticalPeriodsBlockValues;

    FSumOutRecurrenceIntervalBlockHeading    : TSumOutRecurrenceIntervalBlockHeading;
    FSumOutRecurrenceIntervalBlockValues     : TSumOutRecurrenceIntervalBlockValues;
    FRunType   : TRunType;
    FModelType : TModelType;
    MAXBLOCKCOUNT : string;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    function Get_MasterControlChannelNumber(AYieldModelData: TYieldModelDataObject;ABlockTitle:string) : integer;
  public
    function Initialise: boolean;override;
    function ValidateFile(AFileName: TAbstractModelFileName; AProgressFunction: TProgressUpdateFuntion): boolean;
    function ImportFile(AFileName: TAbstractModelFileName; AProgressFunction: TProgressUpdateFuntion): boolean;
    function ExportFile(AFileName: TAbstractModelFileName; AProgressFunction: TProgressUpdateFuntion): boolean;
    function GetDeficitsBlocksFromFile(AFileName: TAbstractModelFileName; ADeficitsBlocks: TObjectList): boolean;
    function GetNoOfFailureSeqBlocksFromFile(AFileName : TAbstractModelFileName; ANoOfFailureSeqBlocks : TObjectList): boolean;
    function GetRecurrenceIntervalBlocksFromFile(AFileName : TAbstractModelFileName; ARecurrenceIntervalBlocks : TObjectList): boolean;
    function GetDeficitsBlocksFromDB(ADeficitsBlocks: TObjectList): boolean;
    function ClearModelDataInDatabase(AFileName:TAbstractModelFileName;AProgressFunction: TProgressUpdateFuntion;
             AQuetly: boolean = False): boolean;
    function GetYieldChannelYield(var AValue: double;AFileNameObject: TAbstractModelFileName): boolean; virtual;
    function PopulateSumOutData(AFileName: TAbstractModelFileName; var AOutputData: TOutputData;
                                                         AYieldModelData: TYieldModelDataObject): boolean;
    function PopulateSumOutComparisonData(AFileName : string;AOutputData: TOutputData;AYieldModelData: TYieldModelDataObject): boolean;
    property RunType   : TRunType    read FRunType;
    property ModelType : TModelType  read FModelType;
  end;

implementation


{ TSumOutFileManager }
uses UDataSetType,
     UErrorHandlingOperations;


procedure TSumOutFileManager.CreateMemberObjects;
const OPNAME = 'TSumOutFileManager.CreateMemberObjects';
begin
  try
    FRunType   := rtUnKnown;
    FModelType := mtUnKnown;
    FSumOutMonthlyBlockAgent               := TSumOutMonthlyBlockAgent.Create(FAppModules);
    FSumOutYieldFailureBlockAgent          := TSumOutYieldFailureBlockAgent.Create(FAppModules);
    FSumOutOutputSummaryBlockAgent         := TSumOutOutputSummaryBlockAgent.Create(FAppModules);
    FSumOutAnualSummaryBlockAgent          := TSumOutAnualSummaryBlockAgent.Create(FAppModules);
    FSumOutAnualAverageInflowBlockAgent    := TSumOutAnualAverageInflowBlockAgent.Create(FAppModules);
    FSumOutSequencesWithFailuresBlockAgent := TSumOutSequencesWithFailuresBlockAgent.Create(FAppModules);
    FSumOutCriticalPeriodsBlockAgent       := TSumOutCriticalPeriodsBlockAgent.Create(FAppModules);
    FSumOutRecurrenceIntervalBlockAgent    := TSumOutRecurrenceIntervalBlockAgent.Create(FAppModules);
    FSumOutDatabaseAgent                   := TSumOutDatabaseAgent.Create(FAppModules);
    FSumOutFileAgent                       := TSumOutFileAgent.Create(FAppModules);

    FSumOutMonthlyBlockHeading               := TSumOutMonthlyBlockHeading.Create(FAppModules);
    FSumOutMonthlyBlockValues                := TSumOutMonthlyBlockValues.Create(FAppModules);
    FSumOutMonthlyBlockAvarage               := TSumOutMonthlyBlockAvarage.Create(FAppModules);

    FSumOutYieldFailureBlockHeading          := TSumOutYieldFailureBlockHeading.Create(FAppModules);
    FSumOutYieldFailureBlockValues           := TSumOutYieldFailureBlockValues.Create(FAppModules);

    FSumOutOutputSummaryBlockHeading         := TSumOutOutputSummaryBlockHeading.Create(FAppModules);
    FSumOutOutputSummaryBlockValues          := TSumOutOutputSummaryBlockValues.Create(FAppModules);
    FSumOutOutputSummaryBlockTotal           := TSumOutOutputSummaryBlockTotal.Create(FAppModules);

    FSumOutAnualSummaryBlockHeading          := TSumOutAnualSummaryBlockHeading.Create(FAppModules);
    FSumOutAnualSummaryBlockValues           := TSumOutAnualSummaryBlockValues.Create(FAppModules);

    FSumOutAnualAverageInflowBlockHeading    := TSumOutAnualAverageInflowBlockHeading.Create(FAppModules);
    FSumOutAnualAverageInflowBlockValues     := TSumOutAnualAverageInflowBlockValues.Create(FAppModules);

    FSumOutSequencesWithFailuresBlockHeading := TSumOutSequencesWithFailuresBlockHeading.Create(FAppModules);
    FSumOutSequencesWithFailuresBlockValues  := TSumOutSequencesWithFailuresBlockValues.Create(FAppModules);
    FSumOutSequencesWithFailuresBlockTotal   := TSumOutSequencesWithFailuresBlockTotal.Create(FAppModules);

    FSumOutCriticalPeriodsBlockHeading       := TSumOutCriticalPeriodsBlockHeading.Create(FAppModules);
    FSumOutCriticalPeriodsBlockValues        := TSumOutCriticalPeriodsBlockValues.Create(FAppModules);

    FSumOutRecurrenceIntervalBlockHeading    := TSumOutRecurrenceIntervalBlockHeading.Create(FAppModules);
    FSumOutRecurrenceIntervalBlockValues     := TSumOutRecurrenceIntervalBlockValues.Create(FAppModules);

  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TSumOutFileManager.DestroyMemberObjects;
const OPNAME = 'TSumOutFileManager.DestroyMemberObjects';
begin
  try
    FreeAndNil(FSumOutMonthlyBlockAgent);
    FreeAndNil(FSumOutYieldFailureBlockAgent);
    FreeAndNil(FSumOutOutputSummaryBlockAgent);
    FreeAndNil(FSumOutAnualSummaryBlockAgent);
    FreeAndNil(FSumOutAnualAverageInflowBlockAgent);
    FreeAndNil(FSumOutSequencesWithFailuresBlockAgent);
    FreeAndNil(FSumOutCriticalPeriodsBlockAgent);
    FreeAndNil(FSumOutRecurrenceIntervalBlockAgent);
    FreeAndNil(FSumOutDatabaseAgent);
    FreeAndNil(FSumOutFileAgent);

    FreeAndNil(FSumOutMonthlyBlockHeading);
    FreeAndNil(FSumOutMonthlyBlockValues);
    FreeAndNil(FSumOutMonthlyBlockAvarage);

    FreeAndNil(FSumOutYieldFailureBlockHeading);
    FreeAndNil(FSumOutYieldFailureBlockValues);

    FreeAndNil(FSumOutOutputSummaryBlockHeading);
    FreeAndNil(FSumOutOutputSummaryBlockValues);
    FreeAndNil(FSumOutOutputSummaryBlockTotal);

    FreeAndNil(FSumOutAnualSummaryBlockHeading);
    FreeAndNil(FSumOutAnualSummaryBlockValues);

    FreeAndNil(FSumOutAnualAverageInflowBlockHeading);
    FreeAndNil(FSumOutAnualAverageInflowBlockValues);

    FreeAndNil(FSumOutSequencesWithFailuresBlockHeading);
    FreeAndNil(FSumOutSequencesWithFailuresBlockValues);
    FreeAndNil(FSumOutSequencesWithFailuresBlockTotal);

    FreeAndNil(FSumOutCriticalPeriodsBlockHeading);
    FreeAndNil(FSumOutCriticalPeriodsBlockValues);

    FreeAndNil(FSumOutRecurrenceIntervalBlockHeading);
    FreeAndNil(FSumOutRecurrenceIntervalBlockValues);

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TSumOutFileManager.Initialise: boolean;
const OPNAME = 'TSumOutFileManager.Initialise';
begin
  Result := False;
  try

    MAXBLOCKCOUNT := FAppModules.ViewIni.ReadString('[TSumOutFileManager]','MAXBLOCKCOUNT','500');

    Result := //FReserviorIDsAgent.Initialise and
              FSumOutMonthlyBlockAgent.Initialise and
              FSumOutYieldFailureBlockAgent.Initialise and
              FSumOutOutputSummaryBlockAgent.Initialise and
              FSumOutAnualSummaryBlockAgent.Initialise and
              FSumOutAnualAverageInflowBlockAgent.Initialise and
              FSumOutSequencesWithFailuresBlockAgent.Initialise and
              FSumOutCriticalPeriodsBlockAgent.Initialise and
              FSumOutRecurrenceIntervalBlockAgent.Initialise;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TSumOutFileManager.ValidateFile(AFileName: TAbstractModelFileName;AProgressFunction: TProgressUpdateFuntion): boolean;
const OPNAME = 'TSumOutFileManager.ValidateFile';
var
  LMessage      : string;
  LInputFile    : TSumOutFile;
  LBLockData    : TDataBlock;
  LBlockNumber  : integer;
  LBlockSkipped : boolean;
  LStop         : boolean;
begin
  Result := False;
  LInputFile := nil;
  LBLockData   := nil;
  try
    //Check if file exists.
    If not FileExists(AFilename.FileName) then
    begin
      Result := True;
      Exit;
    end;
    MAXBLOCKCOUNT := FAppModules.ViewIni.ReadString('[TSumOutFileManager]','MAXBLOCKCOUNT','500');
    LMessage := FAppModules.Language.GetString('TSumOutFileManager.strValidationStarted');
    AProgressFunction(LMessage,ptNone,LStop);

    if not Assigned(AFileName) then
      raise Exception.Create('Output file names parameter is not yet assigned.');

    if not Assigned(AProgressFunction) then
      AProgressFunction := DummyShowProgress;

    Result := Initialise;
    if not Result then
     Exit;

    //Check if file exists.
    If not FileExists(AFilename.FileName) then
    begin
      LMessage := FAppModules.Language.GetString('TSumOutFileManager.strFileNoExist');
      LMessage := Format(LMessage,[ExtractFileName(AFileName.FileName)]);
      AProgressFunction(LMessage,ptError,LStop);
      Exit;
    end;


    LInputFile  := TSumOutFile.Create(FAppModules);
    LBLockData  := TDataBlock.Create(FAppModules);
    LBLockData.RunType := rtUnKnown;
    try

      LMessage := FAppModules.Language.GetString('TSumOutFileManager.strFileValidationStarted');
      LMessage := Format(LMessage,[ExtractFileName(AFilename.FileName)]);
      AProgressFunction(LMessage,ptNone,LStop);

      if Assigned(LInputFile) and Assigned(LBLockData) then
      begin
        if LInputFile.OpenTheFile(AFileName.FileName,ofaRead) then
        begin
          //Skip the top empty line
          LInputFile.GetCurrentLine;

          LBlockNumber := 0;

          //Process the rest of the file
          while not LInputFile.EOF do
          begin
            AProgressFunction('',ptNone,LStop);
            if LStop then Exit;
            if(LBlockNumber > StrToInt(MAXBLOCKCOUNT)) then
            begin
              ShowMessage('You have too much output in your SUM.OUT and only the first ('+ MAXBLOCKCOUNT+') have been read. Fix your debug setting to reduce the size of this file');
              Result := True;
              Exit;
            end;
            //Read a block of data.
            if not LBLockData.ReadBlock(LInputFile,LBlockSkipped) then
               Break;
            if LBlockSkipped then
              Continue;

            //Update counts.
            LBlockNumber := LBlockNumber + 1;
            LBLockData.BlockNumber := LBlockNumber;

            //if(Pos(ENDOFRUNLABEL1,LBLockData.Text) > 0) or (Pos(ENDOFRUNLABEL2,LBLockData.Text) > 0) then

            //Process the block of data.
            Result := True;
            case LBLockData.BlockType of
              btMonthEndReservoirVolume,
              btMonthEndReservoirElevation,
              btNetBasinRunoffIntoResArea,
              btRainfallOnReservoirSurface,
              btGrossEvaporationLossFromReservoir,
              btMonthlyAveragePowerFlow,
              btMonthlyAverageSpillFlow,
              btMonthlyAverageStackedCapacity,
              btMonthlyAverageStackedEnergy,
              btMonthlyAverageIrrigationDeficits,
              btMonthlyAverageChannelFlow,
              btMonthlyPumpingEnergy:
                begin
                Result := FSumOutMonthlyBlockAgent.Validate(LBLockData,AProgressFunction);
                end;
              btYieldFailurePerYearPerSequence:
                begin
                  FSumOutYieldFailureBlockAgent.Validate(LBLockData, AProgressFunction);
                end;
              btOutputSummary:
                begin
                  FSumOutOutputSummaryBlockAgent.Validate(LBLockData, AProgressFunction);
                end;
              btAnualFirmYieldDemands,
              btAnualFirmEnergyDemands,
              btAnualFirmSelectedYieldDemands,
              btAnualNonFirmYieldDemands,
              btAnualSecondaryYieldDemands,
              btAnualTotalSystemPumpingEnergy,
              btAnualFullSystemSupplyVolume:
              begin
                FSumOutAnualSummaryBlockAgent.Validate(LBLockData, AProgressFunction);
              end;
              btAnualAverageInflow:
              begin
                FSumOutAnualAverageInflowBlockAgent.Validate(LBLockData, AProgressFunction);
              end;

              btSequencesWithFailures:
              begin
                FSumOutSequencesWithFailuresBlockAgent.Validate(LBLockData, AProgressFunction);
              end;
              btCriticalPeriodsNumber,
              btCriticalPeriodsLength,
              btCriticalPeriodsDeficit,
              btCriticalPeriodsAvarage:
              begin
                FSumOutCriticalPeriodsBlockAgent.Validate(LBLockData, AProgressFunction);
              end;
              btFirmYieldRecurrenceInterval,
              btNumberOfFailureSequence:
              begin
                FSumOutRecurrenceIntervalBlockAgent.Validate ( LBLockData, AProgressFunction );
              end;
            end;//Case

            if not Result then
              Break;
          end;
        end;
      end;

      LMessage := FAppModules.Language.GetString('TSumOutFileManager.strFileValidationCompleted');
      LMessage := Format(LMessage,[ExtractFileName(AFileName.FileName)]);
      AProgressFunction(LMessage,ptNone,LStop);

    finally
      FreeAndNil(LInputFile);
      FreeAndNil(LBLockData);
    end;

    LMessage := FAppModules.Language.GetString('TSumOutFileManager.strValidationCompleted');
    AProgressFunction(LMessage,ptNone,LStop);

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TSumOutFileManager.ImportFile(AFileName: TAbstractModelFileName;AProgressFunction: TProgressUpdateFuntion): boolean;
const OPNAME = 'TSumOutFileManager.ImportFile';
var
  LMessage      : string;
  LInputFile    : TSumOutFile;
  LBLockData    : TDataBlock;
  LBlockNumber  : integer;
  LBlockSkipped : boolean;
  LStop         : boolean;
begin
  Result := False;
  LInputFile := nil;
  LBLockData   := nil;
  try

    //Check if file exists.
    If not FileExists(AFilename.FileName) then
    begin
      Result := True;
      Exit;
    end;
    MAXBLOCKCOUNT := FAppModules.ViewIni.ReadString('[TSumOutFileManager]','MAXBLOCKCOUNT','500');
    LMessage := FAppModules.Language.GetString('TSumOutFileManager.strImportStarted');
    AProgressFunction(LMessage,ptNone,LStop);

    if not Assigned(AFileName) then
      raise Exception.Create('Output file names parameter is not yet assigned.');

    if not Assigned(AProgressFunction) then
      AProgressFunction := DummyShowProgress;

    //Check if file exists.
    If not FileExists(AFileName.FileName) then
    begin
      LMessage := FAppModules.Language.GetString('TSumOutFileManager.strFileNoExist');
      LMessage := Format(LMessage,[ExtractFileName(AFileName.FileName)]);
      AProgressFunction(LMessage,ptError,LStop);
      Exit;
    end;

    Result := Initialise;
    if not Result then
     Exit;

    Result := ClearModelDataInDatabase(AFileName,AProgressFunction,True);
    if not Result then
      Exit;

    AProgressFunction('',ptNone,LStop);
    if LStop then Exit;

    LInputFile  := TSumOutFile.Create(FAppModules);
    LBLockData  := TDataBlock.Create(FAppModules);
    LBLockData.RunType := rtUnKnown;
    try

      LMessage := FAppModules.Language.GetString('TSumOutFileManager.strFileImportStarted');
      LMessage := Format(LMessage,[ExtractFileName(AFileName.FileName)]);
      AProgressFunction(LMessage,ptNone,LStop);

      if Assigned(LInputFile) and Assigned(LBLockData) then
      begin
        if LInputFile.OpenTheFile(AFileName.FileName,ofaRead) then
        begin
          //Skip the top empty line
          LInputFile.GetCurrentLine;

          LBlockNumber := 0;

          //Process the rest of the file
          while not LInputFile.EOF do
          begin

            AProgressFunction('',ptNone,LStop);
            if LStop then Exit;
            if(LBlockNumber > StrToInt(MAXBLOCKCOUNT)) then
            begin
              ShowMessage('You have too much output in your SUM.OUT and only the first ('+ MAXBLOCKCOUNT+') have been read. Fix your debug setting to reduce the size of this file');
              Result := True;
              Exit;
            end;

            //Read a block of data.
            if not LBLockData.ReadBlock(LInputFile,LBlockSkipped) then
               Break;
            if LBlockSkipped then
              Continue;

            //Update counts.
            LBlockNumber := LBlockNumber + 1;
            LBLockData.BlockNumber := LBlockNumber;
            //if(Pos(ENDOFRUNLABEL1,LBLockData.Text) > 0) or (Pos(ENDOFRUNLABEL2,LBLockData.Text) > 0) then

            //Process the block of data.
            Result := True;
            case LBLockData.BlockType of
              btMonthEndReservoirVolume,
              btMonthEndReservoirElevation,
              btNetBasinRunoffIntoResArea,
              btRainfallOnReservoirSurface,
              btGrossEvaporationLossFromReservoir,
              btMonthlyAveragePowerFlow,
              btMonthlyAverageSpillFlow,
              btMonthlyAverageStackedCapacity,
              btMonthlyAverageStackedEnergy,
              btMonthlyAverageIrrigationDeficits,
              btMonthlyAverageChannelFlow,
              btMonthlyPumpingEnergy:
                begin
                Result := FSumOutMonthlyBlockAgent.ImportFileBlock(LBLockData, AProgressFunction);
                end;
              btYieldFailurePerYearPerSequence:
                begin
                  FSumOutYieldFailureBlockAgent.ImportFileBlock(LBLockData, AProgressFunction);
                end;
              btOutputSummary:
                begin
                  FSumOutOutputSummaryBlockAgent.ImportFileBlock(LBLockData, AProgressFunction);
                end;
              btAnualFirmYieldDemands,
              btAnualFirmEnergyDemands,
              btAnualFirmSelectedYieldDemands,
              btAnualNonFirmYieldDemands,
              btAnualSecondaryYieldDemands,
              btAnualTotalSystemPumpingEnergy,
              btAnualFullSystemSupplyVolume:
              begin
                FSumOutAnualSummaryBlockAgent.ImportFileBlock(LBLockData, AProgressFunction);
              end;
              btAnualAverageInflow:
              begin
                FSumOutAnualAverageInflowBlockAgent.ImportFileBlock(LBLockData, AProgressFunction);
              end;

              btSequencesWithFailures:
              begin
                FSumOutSequencesWithFailuresBlockAgent.ImportFileBlock(LBLockData, AProgressFunction);
              end;
              btCriticalPeriodsNumber,
              btCriticalPeriodsLength,
              btCriticalPeriodsDeficit,
              btCriticalPeriodsAvarage:
              begin
                FSumOutCriticalPeriodsBlockAgent.ImportFileBlock(LBLockData, AProgressFunction);
              end;
              btFirmYieldRecurrenceInterval,
              btNumberOfFailureSequence:
              begin
                FSumOutRecurrenceIntervalBlockAgent.ImportFileBlock ( LBLockData, AProgressFunction );
              end;

            end;//Case
          end;

          Result := FSumOutDatabaseAgent.WriteModelDataToDatabase(TFileNameObject(AFileName),nil,AProgressFunction);
        end;
      end;

      if Result then
      begin
        LMessage := FAppModules.Language.GetString('TSumOutFileManager.strFileImportCompleted');
        LMessage := Format(LMessage,[ExtractFileName(AFileName.FileName)]);
        AProgressFunction(LMessage,ptNone,LStop);
      end;
    finally
      FreeAndNil(LInputFile);
      FreeAndNil(LBLockData);
    end;

    //LMessage := FAppModules.Language.GetString('TSumOutFileManager.strImportCompleted');
    //AProgressFunction(LMessage,ptNone,LStop);

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TSumOutFileManager.ExportFile(AFileName: TAbstractModelFileName;AProgressFunction: TProgressUpdateFuntion): boolean;
const OPNAME = 'TSumOutFileManager.ExportFile';
var
  LEmptyLine: string;
  LMessage : string;
  LOutputFile: TSumOutFile;
  LDataSet : TAbstractModelDataset;
  LBlockType: TOutputDataType;
  LBlockIntType: integer;
  LStop: boolean;
begin
  Result := False;
  LOutputFile := nil;
  try

    LMessage := FAppModules.Language.GetString('TSumOutFileManager.strExportStarted');
    AProgressFunction(LMessage,ptNone,LStop);

    if not Assigned(AFileName) then
      raise Exception.Create('Output file names parameter is not yet assigned.');

    if not Assigned(AProgressFunction) then
      AProgressFunction := DummyShowProgress;

    Result := Initialise;
    if not Result then
     Exit;

    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      LDataSet.SetSQL(
        'SELECT Model, StudyAreaName, SubArea, Scenario, BlockNumber, LoadCaseNumber,SequenceNumber,BlockType,ElementID'+
        ' FROM suBlockDescription'+
        ' WHERE Model = '+ QuotedStr(FAppModules.StudyArea.ModelCode)+
        ' AND StudyAreaName = '+ QuotedStr(FAppModules.StudyArea.StudyAreaCode)+
        ' AND SubArea = '+ QuotedStr(FAppModules.StudyArea.SubAreaCode)+
        ' AND Scenario = '+ QuotedStr(FAppModules.StudyArea.ScenarioCode)+
        ' ORDER BY Model, StudyAreaName, SubArea, Scenario, BlockNumber, LoadCaseNumber,SequenceNumber');

      LDataSet.DataSet.Open;

      if (LDataSet.DataSet.RecordCount <> 0) then
      begin

        //Check if file exists.
        If FileExists(AFileName.FileName) then
          DeleteFile(AFileName.FileName);

        LOutputFile := TSumOutFile.Create(FAppModules);
        try

          LMessage := FAppModules.Language.GetString('TSumOutFileManager.strFileExportStarted');
          LMessage := Format(LMessage,[ExtractFileName(AFileName.FileName)]);
          AProgressFunction(LMessage,ptNone,LStop);

          if Assigned(LOutputFile) then
          begin
            if LOutputFile.OpenTheFile(AFileName.FileName,ofaWrite) then
            begin
          //----------------------------------------------------------------------------------------
              //Write the top empty line
              LEmptyLine := #13#10;
              LOutputFile.FileStream.Write(PChar(LEmptyLine)^, Length(LEmptyLine));

              //Process the rest of the file
              while not LDataSet.DataSet.EOF do
              begin
                AProgressFunction('',ptNone,LStop);
                if LStop then Exit;

                //Read a block of data.
                LBlockIntType :=  LDataSet.DataSet.FieldByName('BlockType').AsInteger;
                LBlockType    := TOutputDataType(LBlockIntType);

                //if(Pos(ENDOFRUNLABEL1,LBLockData.Text) > 0) or (Pos(ENDOFRUNLABEL2,LBLockData.Text) > 0) then

                //Process the block of data.
                Result := True;
                case LBlockType of
                  btMonthEndReservoirVolume,
                  btMonthEndReservoirElevation,
                  btNetBasinRunoffIntoResArea,
                  btRainfallOnReservoirSurface,
                  btGrossEvaporationLossFromReservoir,
                  btMonthlyAveragePowerFlow,
                  btMonthlyAverageSpillFlow,
                  btMonthlyAverageStackedCapacity,
                  btMonthlyAverageStackedEnergy,
                  btMonthlyAverageIrrigationDeficits,
                  btMonthlyAverageChannelFlow,
                  btMonthlyPumpingEnergy:
                    begin
                    Result := FSumOutMonthlyBlockAgent.ExportFileBlock(LDataSet.DataSet,LOutputFile.FileStream,AProgressFunction);
                    end;
                  btYieldFailurePerYearPerSequence:
                    begin
                      FSumOutYieldFailureBlockAgent.ExportFileBlock(LDataSet.DataSet,LOutputFile.FileStream,AProgressFunction);
                    end;
                  btOutputSummary:
                    begin
                      FSumOutOutputSummaryBlockAgent.ExportFileBlock(LDataSet.DataSet,LOutputFile.FileStream,AProgressFunction);
                    end;
                  btAnualFirmYieldDemands,
                  btAnualFirmEnergyDemands,
                  btAnualFirmSelectedYieldDemands,
                  btAnualNonFirmYieldDemands,
                  btAnualSecondaryYieldDemands,
                  btAnualTotalSystemPumpingEnergy,
                  btAnualFullSystemSupplyVolume:
                  begin
                    FSumOutAnualSummaryBlockAgent.ExportFileBlock(LDataSet.DataSet,LOutputFile.FileStream,AProgressFunction);
                  end;
                  btAnualAverageInflow:
                  begin
                    FSumOutAnualAverageInflowBlockAgent.ExportFileBlock(LDataSet.DataSet,LOutputFile.FileStream,AProgressFunction);
                  end;

                  btSequencesWithFailures:
                  begin
                    FSumOutSequencesWithFailuresBlockAgent.ExportFileBlock(LDataSet.DataSet,LOutputFile.FileStream,AProgressFunction);
                  end;
                  btCriticalPeriodsNumber,
                  btCriticalPeriodsLength,
                  btCriticalPeriodsDeficit,
                  btCriticalPeriodsAvarage:
                  begin
                    FSumOutCriticalPeriodsBlockAgent.ExportFileBlock(LDataSet.DataSet,LOutputFile.FileStream,AProgressFunction);
                  end;
                  btFirmYieldRecurrenceInterval,
                  btNumberOfFailureSequence:
                  begin
                    FSumOutRecurrenceIntervalBlockAgent.ExportFileBlock ( LDataSet.DataSet,LOutputFile.FileStream,AProgressFunction );
                  end;
                end;//Case
                if not Result then
                  Break;
                LDataSet.DataSet.Next;
              end;
              Result := True;
            end
            else
              Result := True;
          end;
        finally
          FreeAndNil(LOutputFile);
        end;
      end;
    finally
      LDataSet.Free;
    end;

    if Result and FileExists(AFileName.FileName)then
    begin
      FSumOutFileAgent.SetFileDate(AFileName);
      LMessage := FAppModules.Language.GetString('TSumOutFileManager.strFileExportCompleted');
      LMessage := Format(LMessage,[ExtractFileName(AFileName.FileName)]);
      AProgressFunction(LMessage,ptNone,LStop);
    end;

    //LMessage := FAppModules.Language.GetString('TSumOutFileManager.strExportCompleted');
    //AProgressFunction(LMessage,ptNone,LStop);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TSumOutFileManager.GetDeficitsBlocksFromFile(AFileName: TAbstractModelFileName; ADeficitsBlocks: TObjectList): boolean;
const OPNAME = 'TSumOutFileManager.GetDeficitsBlocksFromFile';
var
  //LMessage : string;
  LInputFile    : TSumOutFile;
  LBLockData    : TDataBlock;
  LBlockNumber  : integer;
  LBlockSkipped : boolean;
begin
  Result := False;
  LInputFile := nil;
  LBLockData   := nil;
  try

    if not Assigned(AFileName) then
      raise Exception.Create('Output file names parameter is not yet assigned.');

    if not Assigned(ADeficitsBlocks) then
      raise Exception.Create('Deficits blocks container parameter is not yet assigned.');

    ADeficitsBlocks.Clear;
    MAXBLOCKCOUNT := FAppModules.ViewIni.ReadString('[TSumOutFileManager]','MAXBLOCKCOUNT','500');

    //Check if file exists.
    If not FileExists(AFileName.FileName) then
    begin
      //LMessage := FAppModules.Language.GetString('TSumOutFileManager.strFileNoExist');
      //LMessage := Format(LMessage,[ExtractFileName(AFileName)]);
      //MessageDlg(LMessage,mtWarning,[mbOK],0);
      Exit;
    end;

    LInputFile  := TSumOutFile.Create(FAppModules);
    LBLockData  := TDataBlock.Create(FAppModules);
    LBLockData.RunType := rtUnKnown;
    try
      if Assigned(LInputFile) and Assigned(LBLockData) then
      begin
        if LInputFile.OpenTheFile(AFileName.FileName,ofaRead) then
        begin
          //Skip the top empty line
          LInputFile.GetCurrentLine;

          LBlockNumber := 0;

          //Process the rest of the file
          while not LInputFile.EOF do
          begin

            //AProgressFunction('',ptNone,LStop);
            //if LStop then Exit;
            {if(LBlockNumber > StrToInt(MAXBLOCKCOUNT)) then
            begin
              ShowMessage('You have too much output in your SUM.OUT and only the first ('+ MAXBLOCKCOUNT+') have been read. Fix your debug setting to reduce the size of this file');
              Result := True;
              Exit;
            end;
             }
            //Read a block of data.
            if not LBLockData.ReadBlock(LInputFile,LBlockSkipped) then
               Break;
            if LBlockSkipped then
              Continue;

            if(FRunType = rtUnKnown) then
              FRunType := LBLockData.RunType;

            //Update counts.
            LBlockNumber := LBlockNumber + 1;
            LBLockData.BlockNumber := LBlockNumber;
            //if(Pos(ENDOFRUNLABEL1,LBLockData.Text) > 0) or (Pos(ENDOFRUNLABEL2,LBLockData.Text) > 0) then

            //Process the block of data.
            Result := True;
            if ( LBLockData.BlockType = btAnualFirmYieldDemands ) then
            begin
              if(pos('DEFICITS  (PROPORTION)',LBLockData.Lines.Text) > 0) then
              begin
                Result := Result and  FSumOutAnualSummaryBlockAgent.GetDeficitsBlockFromFile(LBLockData,ADeficitsBlocks );
                If not Result then
                  Exit;

              end;
            end;
          end;
        end;
      end;
    finally
      FreeAndNil(LInputFile);
      FreeAndNil(LBLockData);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TSumOutFileManager.GetDeficitsBlocksFromDB(ADeficitsBlocks: TObjectList): boolean;
const OPNAME = 'TSumOutFileManager.GetDeficitsBlocksFromDB';
var
  LDataSet : TAbstractModelDataset;
begin
  Result := False;
  try
    if not Assigned(ADeficitsBlocks) then
      raise Exception.Create('Deficits blocks container parameter is not yet assigned.');

    ADeficitsBlocks.Clear;

    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      LDataSet.SetSQL(
        'SELECT Model, StudyAreaName, SubArea, Scenario, BlockNumber, LoadCaseNumber,SequenceNumber,BlockType,ElementID'+
        ' FROM suBlockDescription'+
        ' WHERE Model = '+ QuotedStr(FAppModules.StudyArea.ModelCode)+
        ' AND StudyAreaName = '+ QuotedStr(FAppModules.StudyArea.StudyAreaCode)+
        ' AND SubArea = '+ QuotedStr(FAppModules.StudyArea.SubAreaCode)+
        ' AND Scenario = '+ QuotedStr(FAppModules.StudyArea.ScenarioCode)+
        ' AND BlockType = 15'+
        ' ORDER BY Model, StudyAreaName, SubArea, Scenario, BlockNumber');

      LDataSet.DataSet.Open;

      Result := True;
      While not LDataSet.DataSet.Eof do
      begin
        //AProgressFunction('',ptNone,LStop);
        //if LStop then Exit;
        Result := Result and  FSumOutAnualSummaryBlockAgent.GetDeficitsBlockFromDB(LDataSet.DataSet,ADeficitsBlocks);
        If not Result then
        Exit;
        LDataSet.DataSet.Next;
      end;
    finally
      LDataSet.Free
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TSumOutFileManager.ClearModelDataInDatabase(AFileName: TAbstractModelFileName;
         AProgressFunction: TProgressUpdateFuntion;AQuetly: boolean = False): boolean;
const OPNAME = 'TSumOutFileManager.ClearModelDataInDatabase';
begin
  Result := False;
  try
    Result := FSumOutDatabaseAgent.ClearModelDataInDatabase(TFileNameObject(AFileName),AProgressFunction,AQuetly);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TSumOutFileManager.GetYieldChannelYield(var AValue: double; AFileNameObject: TAbstractModelFileName): boolean;
const OPNAME = 'TSumOutFileManager.GetYieldChannelYield';
var
  LFile: TextFile;
  LLineData: string;
  LValue: string;
begin
  Result := False;
  AValue := 0.00;
  try
    if Assigned(AFileNameObject) then
    begin
      if FileExists(AFileNameObject.FileName) then
      begin
        AssignFile(LFile, AFileNameObject.FileName);
        Reset(LFile);
        try
          while not Eof(LFile) do
          begin
            Readln(LFile,LLineData);
            if(Pos('DEFICITS  (PROPORTION)',LLineData) > 0) then
            begin
              if not Eof(LFile) then
              begin
                 Readln(LFile,LLineData);
                 LValue := Copy(LLineData,21,10);
                 LValue := Trim(LValue);
                 if(LValue <> '') then
                 begin
                   AValue := StrToFloat(LValue);
                   Result := True;
                 end;
                 Break;
              end;
            end;
          end;
        finally
          CloseFile(LFile);
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TSumOutFileManager.GetNoOfFailureSeqBlocksFromFile ( AFileName : TAbstractModelFileName; ANoOfFailureSeqBlocks : TObjectList): boolean;
const OPNAME = 'TSumOutFileManager.GetNoOfFailureSeqBlocksFromFile';
var
  LInputFile      : TSumOutFile;
  LBLockData      : TDataBlock;
  LBlockNumber    : integer;
  LBlockSkipped   : boolean;
begin
  Result := False;
  LInputFile := nil;
  LBLockData   := nil;
  try
    if not Assigned(AFileName) then
      raise Exception.Create('Output file names parameter is not yet assigned.');

    if not Assigned(ANoOfFailureSeqBlocks) then
      raise Exception.Create('Deficits blocks container parameter is not yet assigned.');

    ANoOfFailureSeqBlocks.Clear;
    MAXBLOCKCOUNT := FAppModules.ViewIni.ReadString('[TSumOutFileManager]','MAXBLOCKCOUNT','500');

    //Check if file exists.
    if not FileExists(AFileName.FileName) then
      Exit;

    LInputFile  := TSumOutFile.Create(FAppModules);
    LBLockData  := TDataBlock.Create(FAppModules);
    LBLockData.RunType := rtUnKnown;
    try
      if Assigned(LInputFile) and Assigned(LBLockData) then
      begin
        if LInputFile.OpenTheFile(AFileName.FileName,ofaRead) then
        begin
          //Skip the top empty line
          LInputFile.GetCurrentLine;

          LBlockNumber := 0;

          //Process the rest of the file
          while not LInputFile.EOF do
          begin
            {if(LBlockNumber > StrToInt(MAXBLOCKCOUNT)) then
            begin
              ShowMessage('You have too much output in your SUM.OUT and only the first ('+ MAXBLOCKCOUNT+') have been read. Fix your debug setting to reduce the size of this file');
              Result := True;
              Exit;
            end;
            }
            //Read a block of data.
            if not LBLockData.ReadBlock(LInputFile,LBlockSkipped) then
               Break;
            if LBlockSkipped then
              Continue;

            if(FRunType = rtUnKnown) then
              FRunType := LBLockData.RunType;
            //Update counts.
            LBlockNumber := LBlockNumber + 1;
            LBLockData.BlockNumber := LBlockNumber;

            //Process the block of data.
            Result := True;
            if (LBLockData.BlockType = btNumberOfFailureSequence) then
            begin
              if(pos('NUMBER OF FAILURE SEQUENCES',LBLockData.Lines.Text) > 0) then
              begin
                Result := Result and  FSumOutRecurrenceIntervalBlockAgent.GetNumberOfFailureSeqBlockFromFile(LBLockData,ANoOfFailureSeqBlocks);
                If not Result then
                  Exit;

              end;
            end;
          end;
        end;
      end;
    finally
      FreeAndNil(LInputFile);
      FreeAndNil(LBLockData);
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TSumOutFileManager.GetRecurrenceIntervalBlocksFromFile ( AFileName: TAbstractModelFileName;
                                                                  ARecurrenceIntervalBlocks: TObjectList): boolean;
const OPNAME = 'TSumOutFileManager.GetRecurrenceIntervalBlocksFromFile';
var
  LInputFile     : TSumOutFile;
  LBLockData     : TDataBlock;
  LBlockNumber   : integer;
  LBlockSkipped  : boolean;
begin
  Result := False;
  LInputFile := nil;
  LBLockData   := nil;
  try
    if not Assigned(AFileName) then
      raise Exception.Create('Output file names parameter is not yet assigned.');

    if not Assigned(ARecurrenceIntervalBlocks) then
      raise Exception.Create('Deficits blocks container parameter is not yet assigned.');
//    ARecurrenceIntervalBlocks.Clear;
    //Check if file exists.
    if not FileExists(AFileName.FileName) then
      Exit;
    MAXBLOCKCOUNT := FAppModules.ViewIni.ReadString('[TSumOutFileManager]','MAXBLOCKCOUNT','500');
    LInputFile  := TSumOutFile.Create(FAppModules);
    LBLockData  := TDataBlock.Create(FAppModules);
    LBLockData.RunType := rtUnKnown;
    try
      if Assigned(LInputFile) and Assigned(LBLockData) then
      begin
        if LInputFile.OpenTheFile(AFileName.FileName,ofaRead) then
        begin
          //Skip the top empty line
          LInputFile.GetCurrentLine;
          LBlockNumber := 0;
          //Process the rest of the file
          while not LInputFile.EOF do
          begin
            {if(LBlockNumber > StrToInt(MAXBLOCKCOUNT)) then
            begin
              ShowMessage('You have too much output in your SUM.OUT and only the first ('+ MAXBLOCKCOUNT+') have been read. Fix your debug setting to reduce the size of this file');
              Result := True;
              Exit;
            end;
            }
            //Read a block of data.
            if not LBLockData.ReadBlock(LInputFile,LBlockSkipped) then
               Break;
            if LBlockSkipped then
              Continue;

            if(FRunType = rtUnKnown) then
              FRunType := LBLockData.RunType;
            //Update counts.
            LBlockNumber := LBlockNumber + 1;
            LBLockData.BlockNumber := LBlockNumber;
            //Process the block of data.
            Result := True;
            if (LBLockData.BlockType = btFirmYieldRecurrenceInterval) then
            begin
              if (pos('FIRM YIELD RECURRENCE INTERVALS',LBLockData.Lines.Text) > 0) then
              begin
                Result := Result and  FSumOutRecurrenceIntervalBlockAgent.GetRecurrenceIntervalBlockFromFile(LBLockData,ARecurrenceIntervalBlocks);
                If not Result then
                  Exit;
              end;
            end;
          end;
        end;
      end;
    finally
      FreeAndNil(LInputFile);
      FreeAndNil(LBLockData);
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TSumOutFileManager.PopulateSumOutComparisonData(AFileName : string;AOutputData: TOutputData;AYieldModelData: TYieldModelDataObject): boolean;
const OPNAME = 'TSumOutFileManager.PopulateSumOutComparisonData';
var
  LElementID         : integer;
  LBlockType         : integer;
  LBlockNumber       : integer;
  LLoadCaseNumber    : integer;
  LSequenceNumber    : integer;
  LAnnualWaterDemand : double;
  LAnnualPowerDemand : double;
  LBlockHeading      : string;
  LBlockTitle        : string;
  LInputFile         : TSumOutFile;
  LBLockData         : TDataBlock;
  LValuesLines       : TObjectlist;
  LBlockSkipped      : boolean;
begin
  Result       := False;
  LInputFile   := nil;
  LBLockData   := nil;
  LValuesLines := TObjectList.Create;
  LValuesLines.Clear;

  try
    LInputFile  := TSumOutFile.Create(FAppModules);
    LBLockData  := TDataBlock.Create(FAppModules);
    LBLockData.RunType := rtUnKnown;
    MAXBLOCKCOUNT := FAppModules.ViewIni.ReadString('[TSumOutFileManager]','MAXBLOCKCOUNT','500');
    try
      if Assigned(LInputFile) and Assigned(LBLockData) then
      begin
        if LInputFile.OpenTheFile(AFileName,ofaRead) then
        begin
          //Skip the top empty line
          LInputFile.GetCurrentLine;

          LBlockNumber := 0;

          //Process the rest of the file
          while not LInputFile.EOF do
          begin
            if(LBlockNumber > StrToInt(MAXBLOCKCOUNT)) then
            begin
              ShowMessage('You have too much output in your SUM.OUT and only the first ('+ MAXBLOCKCOUNT+') have been read. Fix your debug setting to reduce the size of this file');
              Result := True;
              Exit;
            end;
            //Read a block of data.
            LBlockSkipped := False;
            if not LBLockData.ReadBlock(LInputFile,LBlockSkipped) then
               Break;
            if LBlockSkipped then
              Continue;

            if(FRunType = rtUnKnown) then
              FRunType := LBLockData.RunType;
            //Update counts.
            LBlockNumber := LBlockNumber + 1;
            LBLockData.BlockNumber := LBlockNumber;

            LBlockType         := LBLockData.BlockType;
            LBlockNumber       := LBLockData.BlockNumber;
            LLoadCaseNumber    := LBLockData.LoadCaseNumber;
            LSequenceNumber    := LBLockData.SequenceNumber;
            LElementID         := LBLockData.ElementID;
            LAnnualWaterDemand := LBLockData.AnnualWaterDemand;
            LAnnualPowerDemand := LBLockData.AnnualPowerDemand;

            //Process the block of data.
            Result := True;
            case LBLockData.BlockType of
              btMonthEndReservoirVolume,
              btMonthEndReservoirElevation,
              btNetBasinRunoffIntoResArea,
              btRainfallOnReservoirSurface,
              btGrossEvaporationLossFromReservoir,
              btMonthlyAveragePowerFlow,
              btMonthlyAverageSpillFlow,
              btMonthlyAverageStackedCapacity,
              btMonthlyAverageStackedEnergy,
              btMonthlyAverageIrrigationDeficits,
              btMonthlyAverageChannelFlow,
              btMonthlyPumpingEnergy:
                begin
                  FSumOutMonthlyBlockHeading.Initialise;
                  FSumOutMonthlyBlockValues.Initialise;
                  FSumOutMonthlyBlockAvarage.Initialise;

                  Result := FSumOutMonthlyBlockHeading.ReadFromBlockData(LBLockData) and
                            FSumOutMonthlyBlockValues.ReadFromBlockData(LBLockData) and
                            FSumOutMonthlyBlockAvarage.ReadFromBlockData(LBLockData);

                  LBlockHeading  := FSumOutMonthlyBlockHeading.FHeading.FData;
                  LBlockTitle    := FSumOutMonthlyBlockHeading.FTitle.FData;
                  LValuesLines   := FSumOutMonthlyBlockValues.FValuesLines;

                  if(LElementID = NullInteger) then
                  begin
                    LElementID := Get_MasterControlChannelNumber(AYieldModelData,LBlockTitle);
                  end;

                  if(LElementID <> NullInteger) then
                  begin
                    AOutputData.CastSummaryOutputData.PopulateElement(TOutputDataType(LBlockType),LElementID,LBlockNumber,
                    LLoadCaseNumber,LSequenceNumber,LAnnualWaterDemand,LAnnualPowerDemand,LBlockHeading,LBlockTitle, LValuesLines);
                  end;
                end;
              btYieldFailurePerYearPerSequence:
                begin
                  FSumOutYieldFailureBlockHeading.Initialise;
                  FSumOutYieldFailureBlockValues.Initialise;

                  Result :=
                    FSumOutYieldFailureBlockHeading.ReadFromBlockData(LBLockData) and
                    FSumOutYieldFailureBlockValues.ReadFromBlockData(LBLockData);

                  LBlockHeading  := FSumOutYieldFailureBlockHeading.FHeading.FData;
                  LBlockTitle    := FSumOutYieldFailureBlockHeading.FTitle.FData;
                  LValuesLines   := FSumOutYieldFailureBlockValues.FValuesLines;

                  if(LElementID = NullInteger) then
                  begin
                    LElementID := Get_MasterControlChannelNumber(AYieldModelData,LBlockTitle);
                  end;

                  if(LElementID <> NullInteger) then
                  begin
                    AOutputData.CastSummaryOutputData.PopulateElement(TOutputDataType(LBlockType),LElementID,LBlockNumber,
                    LLoadCaseNumber,LSequenceNumber,LAnnualWaterDemand,LAnnualPowerDemand,LBlockHeading,LBlockTitle, LValuesLines);
                  end;
                end;
              btOutputSummary:
                begin
                  FSumOutOutputSummaryBlockHeading.Initialise;
                  FSumOutOutputSummaryBlockValues.Initialise;
                  FSumOutOutputSummaryBlockTotal.Initialise;

                  Result :=
                    FSumOutOutputSummaryBlockHeading.ReadFromBlockData(LBLockData) and
                    FSumOutOutputSummaryBlockValues.ReadFromBlockData(LBLockData) and
                    FSumOutOutputSummaryBlockTotal.ReadFromBlockData(LBLockData);

                  LBlockHeading  := FSumOutOutputSummaryBlockHeading.FHeading.FData;
                  LBlockTitle    := FSumOutOutputSummaryBlockHeading.FTitle.FData;
                  LValuesLines   := FSumOutOutputSummaryBlockValues.FValuesLines;

                  if(LElementID = NullInteger) then
                  begin
                    LElementID := Get_MasterControlChannelNumber(AYieldModelData,LBlockTitle);
                  end;

                  if(LElementID <> NullInteger) then
                  begin
                    AOutputData.CastSummaryOutputData.PopulateElement(TOutputDataType(LBlockType),LElementID,LBlockNumber,
                    LLoadCaseNumber,LSequenceNumber,LAnnualWaterDemand,LAnnualPowerDemand,LBlockHeading,LBlockTitle, LValuesLines);
                  end;
                end;
              btAnualFirmYieldDemands,
              btAnualFirmEnergyDemands,
              btAnualFirmSelectedYieldDemands,
              btAnualNonFirmYieldDemands,
              btAnualSecondaryYieldDemands,
              btAnualTotalSystemPumpingEnergy,
              btAnualFullSystemSupplyVolume:
              begin
                FSumOutAnualSummaryBlockHeading.Initialise;
                FSumOutAnualSummaryBlockValues.Initialise;

                Result :=
                  FSumOutAnualSummaryBlockHeading.ReadFromBlockData(LBLockData) and
                  FSumOutAnualSummaryBlockValues.ReadFromBlockData(LBLockData);

                LBlockHeading  := FSumOutAnualSummaryBlockHeading.FHeading.FData;
                LBlockTitle    := FSumOutAnualSummaryBlockHeading.FTitle.FData;
                LValuesLines   := FSumOutAnualSummaryBlockValues.FAnualValues;

                if(LElementID = NullInteger) then
                begin
                  LElementID := Get_MasterControlChannelNumber(AYieldModelData,LBlockTitle);
                end;

                if(LElementID <> NullInteger) then
                begin
                  AOutputData.CastSummaryOutputData.PopulateElement(TOutputDataType(LBlockType),LElementID,LBlockNumber,
                  LLoadCaseNumber,LSequenceNumber,LAnnualWaterDemand,LAnnualPowerDemand,LBlockHeading,LBlockTitle, LValuesLines);
                end;
              end;
              btAnualAverageInflow:
              begin
                FSumOutAnualAverageInflowBlockHeading.Initialise;
                FSumOutAnualAverageInflowBlockValues.Initialise;

                Result :=
                  FSumOutAnualAverageInflowBlockHeading.ReadFromBlockData(LBLockData) and
                  FSumOutAnualAverageInflowBlockValues.ReadFromBlockData(LBLockData);

                LBlockHeading  := FSumOutAnualAverageInflowBlockHeading.FHeading.FData;
                LBlockTitle    := FSumOutAnualAverageInflowBlockHeading.FTitle.FData;
                LValuesLines   := FSumOutAnualAverageInflowBlockValues.FValuesLines;

                if(LElementID = NullInteger) then
                begin
                  LElementID := Get_MasterControlChannelNumber(AYieldModelData,LBlockTitle);
                end;

                if(LElementID <> NullInteger) then
                begin
                  AOutputData.CastSummaryOutputData.PopulateElement(TOutputDataType(LBlockType),LElementID,LBlockNumber,
                  LLoadCaseNumber,LSequenceNumber,LAnnualWaterDemand,LAnnualPowerDemand,LBlockHeading,LBlockTitle, LValuesLines);
                end;
              end;

              btSequencesWithFailures:
              begin
                FSumOutSequencesWithFailuresBlockHeading.Initialise;
                FSumOutSequencesWithFailuresBlockValues.Initialise;
                FSumOutSequencesWithFailuresBlockTotal.Initialise;

                Result :=
                  FSumOutSequencesWithFailuresBlockHeading.ReadFromBlockData(LBLockData) and
                  FSumOutSequencesWithFailuresBlockValues.ReadFromBlockData(LBLockData) and
                  FSumOutSequencesWithFailuresBlockTotal.ReadFromBlockData(LBLockData);

                LBlockHeading  := FSumOutSequencesWithFailuresBlockHeading.FHeading.FData;
                LBlockTitle    := FSumOutSequencesWithFailuresBlockHeading.FTitle.FData;
                LValuesLines   := FSumOutSequencesWithFailuresBlockValues.FValuesLines;

                if(LElementID = NullInteger) then
                begin
                  LElementID := Get_MasterControlChannelNumber(AYieldModelData,LBlockTitle);
                end;

                if(LElementID <> NullInteger) then
                begin
                  AOutputData.CastSummaryOutputData.PopulateElement(TOutputDataType(LBlockType),LElementID,LBlockNumber,
                  LLoadCaseNumber,LSequenceNumber,LAnnualWaterDemand,LAnnualPowerDemand,LBlockHeading,LBlockTitle, LValuesLines);
                end;
              end;
              btCriticalPeriodsNumber,
              btCriticalPeriodsLength,
              btCriticalPeriodsDeficit,
              btCriticalPeriodsAvarage:
              begin
                FSumOutCriticalPeriodsBlockHeading.Initialise;
                FSumOutCriticalPeriodsBlockValues.Initialise;

                Result :=
                  FSumOutCriticalPeriodsBlockHeading.ReadFromBlockData(LBLockData) and
                  FSumOutCriticalPeriodsBlockValues.ReadFromBlockData(LBLockData);

                LBlockHeading  := FSumOutCriticalPeriodsBlockHeading.FHeading.FData;
                LBlockTitle    := FSumOutCriticalPeriodsBlockHeading.FTitle.FData;
              end;
              btFirmYieldRecurrenceInterval,
              btNumberOfFailureSequence:
              begin
                FSumOutRecurrenceIntervalBlockHeading.Initialise;
                FSumOutRecurrenceIntervalBlockValues.Initialise;

                Result :=
                  FSumOutRecurrenceIntervalBlockHeading.ReadFromBlockData(LBLockData) and
                  FSumOutRecurrenceIntervalBlockValues.ReadFromBlockData(LBLockData);

                LBlockHeading  := FSumOutRecurrenceIntervalBlockHeading.FHeading.FData;
                LBlockTitle    := FSumOutRecurrenceIntervalBlockHeading.FTitle.FData;
                LValuesLines   := FSumOutRecurrenceIntervalBlockValues.FValuesLines;

                if(LElementID = NullInteger) then
                begin
                  LElementID := Get_MasterControlChannelNumber(AYieldModelData,LBlockTitle);
                end;

                if(LElementID <> NullInteger) then
                begin
                  AOutputData.CastSummaryOutputData.PopulateElement(TOutputDataType(LBlockType),LElementID,LBlockNumber,
                  LLoadCaseNumber,LSequenceNumber,LAnnualWaterDemand,LAnnualPowerDemand,LBlockHeading,LBlockTitle, LValuesLines);
                end;
              end;
            end;//Case
          end;
        end;
      end;
    finally
      FreeAndNil(LInputFile);
      FreeAndNil(LBLockData);
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TSumOutFileManager.PopulateSumOutData(AFileName: TAbstractModelFileName; var AOutputData: TOutputData;
                                                 AYieldModelData: TYieldModelDataObject): boolean;
const OPNAME = 'TSumOutFileManager.PopulateSumOutData';
var
  LIndex             : integer;
  LElementID         : integer;
  LBlockType         : integer;
  LBlockNumber       : integer;
  LLoadCaseNumber    : integer;
  LSequenceNumber    : integer;
  LAnnualWaterDemand : double;
  LAnnualPowerDemand : double;
  LBlockHeading      : string;
  LBlockTitle        : string;
  LInputFile         : TSumOutFile;
  LBLockData         : TDataBlock;
  LValuesLines       : TObjectlist;
  lChannel           : IGeneralFlowChannel;
  lReservoir         : IReservoirData;
  LPowerPlant        : IPowerPlant;
  LElementName       : string;
  LBlockSkipped      : boolean;
begin
  Result       := False;
  LInputFile   := nil;
  LBLockData   := nil;
  LValuesLines := TObjectList.Create;
  LValuesLines.Clear;
  MAXBLOCKCOUNT := FAppModules.ViewIni.ReadString('TSumOutFileManager','MAXBLOCKCOUNT','500');
  try
    LInputFile  := TSumOutFile.Create(FAppModules);
    LBLockData  := TDataBlock.Create(FAppModules);
    LBLockData.RunType := rtUnKnown;
    try
      if Assigned(LInputFile) and Assigned(LBLockData) then
      begin
        if LInputFile.OpenTheFile(AFileName.FileName,ofaRead) then
        begin
          //Skip the top empty line
          LInputFile.GetCurrentLine;

          LBlockNumber := 0;

          //Process the rest of the file
          while not LInputFile.EOF do
          begin
            if(LBlockNumber > StrToInt(MAXBLOCKCOUNT)) then
            begin
              ShowMessage('You have too much output in your SUM.OUT and only the first ('+ MAXBLOCKCOUNT+') have been read. Fix your debug setting to reduce the size of this file');
              Result := True;
              Exit;
            end;
            //Read a block of data.
            if not LBLockData.ReadBlock(LInputFile,LBlockSkipped) then
               Break;
            if LBlockSkipped then
              Continue;

            if(FRunType = rtUnKnown) then
              FRunType := LBLockData.RunType;
            //Update counts.
            LBlockNumber := LBlockNumber + 1;
            LBLockData.BlockNumber := LBlockNumber;

            LBlockType         := LBLockData.BlockType;
            LBlockNumber       := LBLockData.BlockNumber;
            LLoadCaseNumber    := LBLockData.LoadCaseNumber;
            LSequenceNumber    := LBLockData.SequenceNumber;
            LAnnualWaterDemand := LBLockData.AnnualWaterDemand;
            LAnnualPowerDemand := LBLockData.AnnualPowerDemand;
            LElementName       := LBLockData.ElementName;
            LElementID         := NullInteger;
            if(LBLockData.ModelType <> mtPlanning) then
            begin
              LElementID         := LBLockData.ElementID;
            end
            else
            begin
              case LBLockData.BlockType of
                btMonthEndReservoirVolume,
                btMonthEndReservoirElevation,
                btNetBasinRunoffIntoResArea,
                btRainfallOnReservoirSurface,
                btGrossEvaporationLossFromReservoir:
                  begin
                    lReservoir := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.
                                  ReservoirList.ReservoirOrNodeByName[LElementName];;
                    if(lReservoir <> nil) then
                      LElementID := lReservoir.ReservoirConfigurationData.ReservoirIdentifier;

                  end;
                btMonthlyAverageChannelFlow:
                  begin
                    lChannel := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.
                                ChannelList.ChannelByName[LElementName];
                    if(lChannel <> nil) then
                      LElementID := lChannel.ChannelNumber;
                  end;
              end;
            end;


            //Process the block of data.
            Result := True;
            case LBLockData.BlockType of
              btMonthEndReservoirVolume,
              btMonthEndReservoirElevation,
              btNetBasinRunoffIntoResArea,
              btRainfallOnReservoirSurface,
              btGrossEvaporationLossFromReservoir,
              btMonthlyAveragePowerFlow,
              btMonthlyAverageSpillFlow,
              btMonthlyAverageStackedCapacity,
              btMonthlyAverageStackedEnergy,
              btMonthlyAverageIrrigationDeficits,
              btMonthlyAverageChannelFlow,
              btMonthlyPumpingEnergy:
              begin
                  FSumOutMonthlyBlockHeading.Initialise;
                  FSumOutMonthlyBlockValues.Initialise;
                  FSumOutMonthlyBlockAvarage.Initialise;

                  Result := FSumOutMonthlyBlockHeading.ReadFromBlockData(LBLockData) and
                            FSumOutMonthlyBlockValues.ReadFromBlockData(LBLockData) and
                            FSumOutMonthlyBlockAvarage.ReadFromBlockData(LBLockData);

                  LBlockHeading  := FSumOutMonthlyBlockHeading.FHeading.FData;
                  LBlockTitle    := FSumOutMonthlyBlockHeading.FTitle.FData;
                  LValuesLines   := FSumOutMonthlyBlockValues.FValuesLines;

                  if(LElementID = NullInteger) then
                  begin
                    if LBLockData.BlockType in [btMonthlyAverageStackedCapacity,btMonthlyAverageStackedEnergy] then
                    begin
                      for LIndex := 0 to TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.PowerPlantList.PowerPlantCount-1 do
                      begin
                        LPowerPlant := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.PowerPlantList.PowerPlantByIndex[LIndex];
                        if(LPowerPlant <> nil) and (Trim(LPowerPlant.FeatureName) = Trim(LElementName)) then
                        begin
                          LElementID := LPowerPlant.FeatureID;
                          Break;
                        end;
                      end;
                    end
                  end;

                  if(LElementID = NullInteger) then
                  begin
                    LElementID := Get_MasterControlChannelNumber(AYieldModelData,LBlockTitle);
                  end;

                  if(LElementID <> NullInteger) then
                  begin
                    AOutputData.CastSummaryOutputData.PopulateElement(TOutputDataType(LBlockType),LElementID,LBlockNumber,
                    LLoadCaseNumber,LSequenceNumber,LAnnualWaterDemand,LAnnualPowerDemand,LBlockHeading,LBlockTitle, LValuesLines);
                  end;
                end;
              btYieldFailurePerYearPerSequence:
                begin
                  FSumOutYieldFailureBlockHeading.Initialise;
                  FSumOutYieldFailureBlockValues.Initialise;

                  Result :=
                    FSumOutYieldFailureBlockHeading.ReadFromBlockData(LBLockData) and
                    FSumOutYieldFailureBlockValues.ReadFromBlockData(LBLockData);

                  LBlockHeading  := FSumOutYieldFailureBlockHeading.FHeading.FData;
                  LBlockTitle    := FSumOutYieldFailureBlockHeading.FTitle.FData;
                  LValuesLines   := FSumOutYieldFailureBlockValues.FValuesLines;

                  if(LElementID = NullInteger) then
                  begin
                    LElementID := Get_MasterControlChannelNumber(AYieldModelData,LBlockTitle);
                  end;

                  if(LElementID <> NullInteger) then
                  begin
                    AOutputData.CastSummaryOutputData.PopulateElement(TOutputDataType(LBlockType),LElementID,LBlockNumber,
                    LLoadCaseNumber,LSequenceNumber,LAnnualWaterDemand,LAnnualPowerDemand,LBlockHeading,LBlockTitle, LValuesLines);
                  end;
                end;
              btOutputSummary:
                begin
                  FSumOutOutputSummaryBlockHeading.Initialise;
                  FSumOutOutputSummaryBlockValues.Initialise;
                  FSumOutOutputSummaryBlockTotal.Initialise;

                  Result :=
                    FSumOutOutputSummaryBlockHeading.ReadFromBlockData(LBLockData) and
                    FSumOutOutputSummaryBlockValues.ReadFromBlockData(LBLockData) and
                    FSumOutOutputSummaryBlockTotal.ReadFromBlockData(LBLockData);

                  LBlockHeading  := FSumOutOutputSummaryBlockHeading.FHeading.FData;
                  LBlockTitle    := FSumOutOutputSummaryBlockHeading.FTitle.FData;
                  LValuesLines   := FSumOutOutputSummaryBlockValues.FValuesLines;

                  if(LElementID = NullInteger) then
                  begin
                    LElementID := Get_MasterControlChannelNumber(AYieldModelData,LBlockTitle);
                  end;

                  if(LElementID <> NullInteger) then
                  begin
                    AOutputData.CastSummaryOutputData.PopulateElement(TOutputDataType(LBlockType),LElementID,LBlockNumber,
                    LLoadCaseNumber,LSequenceNumber,LAnnualWaterDemand,LAnnualPowerDemand,LBlockHeading,LBlockTitle, LValuesLines);
                  end;
                end;
              btAnualFirmYieldDemands,
              btAnualFirmEnergyDemands,
              btAnualFirmSelectedYieldDemands,
              btAnualNonFirmYieldDemands,
              btAnualSecondaryYieldDemands,
              btAnualTotalSystemPumpingEnergy,
              btAnualFullSystemSupplyVolume:
              begin
                FSumOutAnualSummaryBlockHeading.Initialise;
                FSumOutAnualSummaryBlockValues.Initialise;

                Result :=
                  FSumOutAnualSummaryBlockHeading.ReadFromBlockData(LBLockData) and
                  FSumOutAnualSummaryBlockValues.ReadFromBlockData(LBLockData);

                LBlockHeading  := FSumOutAnualSummaryBlockHeading.FHeading.FData;
                LBlockTitle    := FSumOutAnualSummaryBlockHeading.FTitle.FData;
                LValuesLines   := FSumOutAnualSummaryBlockValues.FAnualValues;

                if(LElementID = NullInteger) then
                begin
                  LElementID := Get_MasterControlChannelNumber(AYieldModelData,LBlockTitle);
                end;

                if(LElementID <> NullInteger) then
                begin
                  AOutputData.CastSummaryOutputData.PopulateElement(TOutputDataType(LBlockType),LElementID,LBlockNumber,
                  LLoadCaseNumber,LSequenceNumber,LAnnualWaterDemand,LAnnualPowerDemand,LBlockHeading,LBlockTitle, LValuesLines);
                end;
              end;
              btAnualAverageInflow:
              begin
                FSumOutAnualAverageInflowBlockHeading.Initialise;
                FSumOutAnualAverageInflowBlockValues.Initialise;

                Result :=
                  FSumOutAnualAverageInflowBlockHeading.ReadFromBlockData(LBLockData) and
                  FSumOutAnualAverageInflowBlockValues.ReadFromBlockData(LBLockData);

                LBlockHeading  := FSumOutAnualAverageInflowBlockHeading.FHeading.FData;
                LBlockTitle    := FSumOutAnualAverageInflowBlockHeading.FTitle.FData;
                LValuesLines   := FSumOutAnualAverageInflowBlockValues.FValuesLines;

                if(LElementID = NullInteger) then
                begin
                  LElementID := Get_MasterControlChannelNumber(AYieldModelData,LBlockTitle);
                end;

                if(LElementID <> NullInteger) then
                begin
                  AOutputData.CastSummaryOutputData.PopulateElement(TOutputDataType(LBlockType),LElementID,LBlockNumber,
                  LLoadCaseNumber,LSequenceNumber,LAnnualWaterDemand,LAnnualPowerDemand,LBlockHeading,LBlockTitle, LValuesLines);
                end;
              end;

              btSequencesWithFailures:
              begin
                FSumOutSequencesWithFailuresBlockHeading.Initialise;
                FSumOutSequencesWithFailuresBlockValues.Initialise;
                FSumOutSequencesWithFailuresBlockTotal.Initialise;

                Result :=
                  FSumOutSequencesWithFailuresBlockHeading.ReadFromBlockData(LBLockData) and
                  FSumOutSequencesWithFailuresBlockValues.ReadFromBlockData(LBLockData) and
                  FSumOutSequencesWithFailuresBlockTotal.ReadFromBlockData(LBLockData);

                LBlockHeading  := FSumOutSequencesWithFailuresBlockHeading.FHeading.FData;
                LBlockTitle    := FSumOutSequencesWithFailuresBlockHeading.FTitle.FData;
                LValuesLines   := FSumOutSequencesWithFailuresBlockValues.FValuesLines;

                if(LElementID = NullInteger) then
                begin
                  LElementID := Get_MasterControlChannelNumber(AYieldModelData,LBlockTitle);
                end;

                if(LElementID <> NullInteger) then
                begin
                  AOutputData.CastSummaryOutputData.PopulateElement(TOutputDataType(LBlockType),LElementID,LBlockNumber,
                  LLoadCaseNumber,LSequenceNumber,LAnnualWaterDemand,LAnnualPowerDemand,LBlockHeading,LBlockTitle, LValuesLines);
                end;
              end;
              btCriticalPeriodsNumber,
              btCriticalPeriodsLength,
              btCriticalPeriodsDeficit,
              btCriticalPeriodsAvarage:
              begin
                FSumOutCriticalPeriodsBlockHeading.Initialise;
                FSumOutCriticalPeriodsBlockValues.Initialise;

                Result :=
                  FSumOutCriticalPeriodsBlockHeading.ReadFromBlockData(LBLockData) and
                  FSumOutCriticalPeriodsBlockValues.ReadFromBlockData(LBLockData);

                LBlockHeading  := FSumOutCriticalPeriodsBlockHeading.FHeading.FData;
                LBlockTitle    := FSumOutCriticalPeriodsBlockHeading.FTitle.FData;
              end;
              btFirmYieldRecurrenceInterval,
              btNumberOfFailureSequence:
              begin
                FSumOutRecurrenceIntervalBlockHeading.Initialise;
                FSumOutRecurrenceIntervalBlockValues.Initialise;

                Result :=
                  FSumOutRecurrenceIntervalBlockHeading.ReadFromBlockData(LBLockData) and
                  FSumOutRecurrenceIntervalBlockValues.ReadFromBlockData(LBLockData);

                LBlockHeading  := FSumOutRecurrenceIntervalBlockHeading.FHeading.FData;
                LBlockTitle    := FSumOutRecurrenceIntervalBlockHeading.FTitle.FData;
                LValuesLines   := FSumOutRecurrenceIntervalBlockValues.FValuesLines;

                if(LElementID = NullInteger) then
                begin
                  LElementID := Get_MasterControlChannelNumber(AYieldModelData,LBlockTitle);
                end;

                if(LElementID <> NullInteger) then
                begin
                  AOutputData.CastSummaryOutputData.PopulateElement(TOutputDataType(LBlockType),LElementID,LBlockNumber,
                  LLoadCaseNumber,LSequenceNumber,LAnnualWaterDemand,LAnnualPowerDemand,LBlockHeading,LBlockTitle, LValuesLines);
                end;
              end;
            end;//Case
          end;
        end;
      end;

      if(FAppModules.Model.ModelName = CPlanning) then
        AOutputData.CastSummaryOutputData.ReCalculateElementSequenceNumbers;
    finally
      FreeAndNil(LInputFile);
      FreeAndNil(LBLockData);
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TSumOutFileManager.Get_MasterControlChannelNumber(AYieldModelData: TYieldModelDataObject;ABlockTitle: string): integer;
const OPNAME = 'TSumOutFileManager.Get_MasterControlChannelNumber';
var
  lFeature : IMasterControlFeature;
  LIndex   : integer;
begin
  Result := NullInteger;
  try
    ABlockTitle := Trim(UpperCase(ABlockTitle));
    if(ABlockTitle = '') then Exit;

    for LIndex := 0 to AYieldModelData.NetworkFeaturesData.MasterControlFeatureList.MasterControlFeatureCount-1 do
    begin
      lFeature := AYieldModelData.NetworkFeaturesData.MasterControlFeatureList.MasterControlFeatureByIndex[LIndex];
      if(lFeature.Channel <> nil) then
      begin
        if(lFeature.MasterControlType = 'W') and (Pos('WATER YIELD SUMMARY',ABlockTitle) = 1) then
        begin
          Result := lFeature.Channel.ChannelNumber;
          Break;
        end;
        if(lFeature.MasterControlType = 'P') and (Pos('TOTAL INTEGRATED POWER SYSTEM',ABlockTitle) = 1) then
        begin
          Result := lFeature.Channel.ChannelNumber;
          Break;
        end;
      end;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

end.
