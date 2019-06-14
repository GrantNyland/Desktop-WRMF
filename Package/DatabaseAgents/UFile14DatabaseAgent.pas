//
//
//  UNIT      : Contains TFile14DatabaseAgent Class
//  AUTHOR    : Titi Ngubane(Arivia)
//  DATE      : 15/04/2003
//  COPYRIGHT : Copyright © 2003 DWAF
//
//
unit UFile14DatabaseAgent;

interface

uses
  Classes, sysutils,Db,

  //  DWAF VCL
  UFileNames,
  UConstants,
  UDatabaseUtilities,
  UAbstractObject,
  UDataFileObjects,
  UAbstractFileNamesObject,
  UReleaseStructureObject,
  UAbstractDatabaseAgent,
  UYieldModelDataObject;

type

  TFile14DatabaseAgent = class(TAbstractDatabaseAgent)
  protected
    function ReadF14UnknownDataSQL: string;
    function ReadIFRFeaturesSQL: string;
    function ReadIFRFeaturesDetailsSQL: string;
    function ReadIFRReferenceSQL: string;

    function WriteIFRFeaturesSQL: string;
    function WriteIFRFeaturesDetailsSQL: string;
    function WriteIFRReferenceSQL: string;
    function WriteUnknownDataSQL: string;

  public
    { Public declarations }
    function ReadModelDataFromDatabase(AFileName:TFileNameObject; ADataObject: TDataFileObjects;
             AProgressFunction: TProgressUpdateFuntion): boolean; override;
    function WriteModelDataToDatabase(AFileName:TFileNameObject;ADataObject: TDataFileObjects;
             AProgressFunction: TProgressUpdateFuntion): boolean; override;
    function ClearModelDataInDatabase(AFileName:TFileNameObject;AProgressFunction: TProgressUpdateFuntion;
             AQuetly: boolean = False): boolean; override;
  end;


implementation

uses
  System.Contnrs,
  UUtilities,
  UDataSetType,
  UErrorHandlingOperations;

function TFile14DatabaseAgent.ReadF14UnknownDataSQL: string;
const OPNAME = 'TFile14DatabaseAgent.ReadF14UnknownDataSQL';
begin

  Result := '';
  try
    Result := 'SELECT Model,StudyAreaName,SubArea,Scenario,LineNumber,LineData '+
              ' FROM WRYMFileLines'+
              ' WHERE Model       =  '+QuotedStr(FAppModules.StudyArea.ModelCode)+
              ' AND StudyAreaName =  '+QuotedStr(FAppModules.StudyArea.StudyAreaCode)+
              ' AND SubArea       =  '+QuotedStr(FAppModules.StudyArea.SubAreaCode)+
              ' AND Scenario      =  '+QuotedStr(FAppModules.StudyArea.ScenarioCode)+
              ' AND FileGroup     = :FileGroup' +
              ' AND FileType      = :FileType'+
              ' ORDER BY Model,StudyAreaName,SubArea,Scenario,LineNumber,LineSection';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFile14DatabaseAgent.ReadIFRFeaturesSQL: string;
const OPNAME = 'TFile14DatabaseAgent.ReadIFRFeaturesSQL';
begin

  Result := '';
  try
    Result :=
      'SELECT ' +
      'Model, StudyAreaName, SubArea, Scenario, Identifier, ' +
      'IFRChannelNumber, FeatureName, ReferenceNodeCount, LagInMonthsCount, ' +
      'PointsCount, RefNodeNumbers,CalculationOption,ReferenceFlowType, IFRStatusIndicator, IFRLoss, IFRUnknown, MonthlyIFRLoss ' +
      'FROM IFRFeatures ' +
      'WHERE Model        = ' + QuotedStr(FAppModules.StudyArea.ModelCode) +
      ' AND StudyAreaName = ' + QuotedStr(FAppModules.StudyArea.StudyAreaCode) +
      ' AND SubArea       = ' + QuotedStr(FAppModules.StudyArea.SubAreaCode) +
      ' AND Scenario      = ' + QuotedStr(FAppModules.StudyArea.ScenarioCode) +
      ' ORDER BY Model, StudyAreaName, SubArea, Scenario, Identifier';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFile14DatabaseAgent.ReadIFRFeaturesDetailsSQL: string;
const OPNAME = 'TFile14DatabaseAgent.ReadIFRFeaturesDetailsSQL';
begin

  Result := '';
  try
    Result := 'SELECT'+
      '  Model,StudyAreaName,SubArea,Scenario,Identifier,LineNumber,AnnualInflow,InflowVar01,InflowVar02,InflowVar03'+
      ' ,InflowVar04,InflowVar05,InflowVar06,InflowVar07,InflowVar08,InflowVar09,InflowVar10,InflowVar11,InflowVar12'+
      ' ,ReleaseVar01,ReleaseVar02,ReleaseVar03,ReleaseVar04,ReleaseVar05,ReleaseVar06,ReleaseVar07,ReleaseVar08'+
      ' ,ReleaseVar09,ReleaseVar10,ReleaseVar11,ReleaseVar12'+
      ' FROM IFRFeaturesDetails'+
      ' WHERE  (Model            ='+ QuotedStr(FAppModules.StudyArea.ModelCode)    +') AND'+
      '        (StudyAreaName    ='+QuotedStr(FAppModules.StudyArea.StudyAreaCode) +') AND'+
      '        (SubArea          ='+QuotedStr(FAppModules.StudyArea.SubAreaCode)   +') AND'+
      '        (Scenario         ='+QuotedStr(FAppModules.StudyArea.ScenarioCode)  +') AND'+
      '        Identifier       = :Identifier' +
      ' ORDER BY Model,StudyAreaName,SubArea,Scenario,Identifier,LineNumber';

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFile14DatabaseAgent.ReadIFRReferenceSQL: string;
const OPNAME = 'TFile14DatabaseAgent.ReadIFRReferenceSQL';
begin

  Result := '';
  try
    Result := 'SELECT'+
      '  Model,StudyAreaName,SubArea,Scenario,InflowOption'+
      ' FROM IFRReference'+
      ' WHERE  Model            ='+ QuotedStr(FAppModules.StudyArea.ModelCode)    +' AND'+
      '        StudyAreaName    ='+QuotedStr(FAppModules.StudyArea.StudyAreaCode) +' AND'+
      '        SubArea          ='+QuotedStr(FAppModules.StudyArea.SubAreaCode)   +' AND'+
      '        Scenario         ='+QuotedStr(FAppModules.StudyArea.ScenarioCode);

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFile14DatabaseAgent.WriteIFRFeaturesSQL: string;
const OPNAME = 'TFile14DatabaseAgent.WriteIFRFeaturesSQL';
begin
  Result := '';
  try
    Result :=
      'INSERT INTO IFRFeatures ' +
      '(Model, StudyAreaName, SubArea, Scenario, Identifier, ' +
      'IFRChannelNumber, FeatureName, ReferenceNodeCount, LagInMonthsCount, PointsCount, ' +
      'RefNodeNumbers, CalculationOption, ReferenceFlowType, IFRStatusIndicator, IFRLoss, IFRUnknown, MonthlyIFRLoss) ' +
      'Values ' +
      '(:Model, :StudyAreaName, :SubArea, :Scenario, :Identifier, ' +
      ':IFRChannelNumber, :FeatureName, :ReferenceNodeCount, :LagInMonthsCount, :PointsCount, ' +
      ':RefNodeNumbers, :CalculationOption, :ReferenceFlowType, :IFRStatusIndicator, :IFRLoss, :IFRUnknown, :MonthlyIFRLoss)';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFile14DatabaseAgent.WriteIFRFeaturesDetailsSQL: string;
const OPNAME = 'TFile14DatabaseAgent.WriteIFRFeaturesDetailsSQL';
begin
  Result := '';
  try
    Result := 'INSERT INTO IFRFeaturesDetails'+
              ' (Model,StudyAreaName,SubArea,Scenario,Identifier,LineNumber,AnnualInflow'+
              ' ,InflowVar01,InflowVar02,InflowVar03,InflowVar04,InflowVar05,InflowVar06,InflowVar07,InflowVar08,InflowVar09,InflowVar10'+
              ' ,InflowVar11,InflowVar12,ReleaseVar01,ReleaseVar02,ReleaseVar03,ReleaseVar04,ReleaseVar05,ReleaseVar06,ReleaseVar07,ReleaseVar08'+
              ' ,ReleaseVar09,ReleaseVar10,ReleaseVar11,ReleaseVar12)'+
              ' Values'+
              '(:Model,:StudyAreaName,:SubArea,:Scenario,:Identifier,:LineNumber,:AnnualInflow'+
              ' ,:InflowVar01,:InflowVar02,:InflowVar03,:InflowVar04,:InflowVar05,:InflowVar06,:InflowVar07,:InflowVar08,:InflowVar09,:InflowVar10'+
              ',:InflowVar11,:InflowVar12,:ReleaseVar01,:ReleaseVar02,:ReleaseVar03,:ReleaseVar04,:ReleaseVar05,:ReleaseVar06,:ReleaseVar07,:ReleaseVar08'+
              ',:ReleaseVar09,:ReleaseVar10,:ReleaseVar11,:ReleaseVar12)';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFile14DatabaseAgent.WriteIFRReferenceSQL: string;
const OPNAME = 'TFile14DatabaseAgent.WriteIFRReferenceSQL';
begin
  Result := '';
  try
    Result := 'INSERT INTO IFRReference'+
              ' (Model,StudyAreaName,SubArea,Scenario,InflowOption)'+
              ' Values'+
              '(:Model,:StudyAreaName,:SubArea,:Scenario,:InflowOption)';

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFile14DatabaseAgent.WriteUnknownDataSQL: string;
const OPNAME = 'TFile14DatabaseAgent.WriteUnknownDataSQL';
begin
  Result := '';
  try
    Result := 'INSERT INTO WRYMFileLines'+
              ' (Model,StudyAreaName,SubArea,Scenario,FileType,FileGroup,LineNumber,LineSection,LineData)'+
              ' Values(:Model,:StudyAreaName,:SubArea,:Scenario,:FileType,:FileGroup,:LineNumber,:LineSection,:LineData)';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFile14DatabaseAgent.ReadModelDataFromDatabase(AFileName:TFileNameObject; ADataObject: TDataFileObjects;
         AProgressFunction: TProgressUpdateFuntion): boolean;
const OPNAME = 'TFile14DatabaseAgent.ReadModelDataFromDatabase';
var
  LFieldName,
  LMessage: string;
  LDataSet : TAbstractModelDataset;
  LSubDataSet : TAbstractModelDataset;
  LCount: Integer;
  LReleaseControlStructureObject: TReleaseControlStructureObject;
  LControlStructureDetails:TReleaseControlStructureDetails;
  LInflowVariableLine :TInflowVariableLine;
  LStop: boolean;
  lRefNodesStr : string;
  LReference: TReference;
  LReferenceRelease: TReferenceRelease;
begin
  Result := False;
  try
    if not Assigned(AProgressFunction) then
      AProgressFunction := DummyShowProgress;

    LMessage := FAppModules.Language.GetString('TFile14DatabaseAgent.strReadStarted');
    AProgressFunction(LMessage,ptNone,LStop);

    if not Assigned(ADataObject) then
      raise Exception.Create('File object parameter is not yet assigned.');

    LReleaseControlStructureObject := ADataObject.FReleaseControlStructureObject;

    if not LReleaseControlStructureObject.Initialise then
    Exit;

    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LSubDataSet);
    try
      if FAppModules.FieldProperties.FieldAvailableInModel('IFRInflowOption') or (FAppModules.Model.ModelName = CPlanning) then
      begin
        LDataSet.SetSQL(ReadIFRReferenceSQL);
        LDataSet.DataSet.Open;

        //Check if there is any data.
        if (LDataSet.DataSet.RecordCount > 0) then
        begin
          LReleaseControlStructureObject.FInflowOption.FData := LDataSet.DataSet.FieldByName('InflowOption').AsInteger;
          LReleaseControlStructureObject.FInflowOption.FInitalised := True;
        end;
      end;

      LDataSet.DataSet.Close;
      LDataSet.SetSQL(ReadIFRFeaturesSQL);
      LDataSet.DataSet.Open;

      //Check if there is any data.
      if (LDataSet.DataSet.Eof and  LDataSet.DataSet.Bof) then
      begin
        if LReleaseControlStructureObject.FInflowOption.FInitalised then
        begin
          LReleaseControlStructureObject.FControlStructCount.FData := 0;
          LReleaseControlStructureObject.FControlStructCount.FInitalised := True;
        end;
        LMessage := FAppModules.Language.GetString('TFile14DatabaseAgent.strNoDataReturned');
        AProgressFunction(LMessage,ptWarning,LStop);
        Result := True;
      end
      else
      begin
        while not LDataSet.DataSet.EOF do
        begin
          LControlStructureDetails := nil;
          LReference               := nil;

          if (LDataSet.DataSet.FieldByName('ReferenceFlowType').AsInteger <> 1) then
          begin
            LControlStructureDetails := LReleaseControlStructureObject.AddReleaseControlStructureDetails;
            if not Assigned(LControlStructureDetails) then
              Break;

            //Line2 +++++++++++++++++++++++++++++++++

            if not LDataSet.DataSet.FieldByName('IFRChannelNumber').IsNull then
            begin
              LControlStructureDetails.FControlStructureData.FIFRChannelNumber.FData :=LDataSet.DataSet.FieldByName('IFRChannelNumber').AsInteger;
              LControlStructureDetails.FControlStructureData.FIFRChannelNumber.FInitalised := True;
            end;

            if not LDataSet.DataSet.FieldByName('ReferenceNodeCount').IsNull then
            begin
              LControlStructureDetails.FControlStructureData.FReferenceNodeCount.FData :=LDataSet.DataSet.FieldByName('ReferenceNodeCount').AsInteger;
              LControlStructureDetails.FControlStructureData.FReferenceNodeCount.FInitalised := True;
            end;

            if not LDataSet.DataSet.FieldByName('LagInMonthsCount').IsNull then
            begin
              LControlStructureDetails.FControlStructureData.FLagInMonthsCount.FData :=LDataSet.DataSet.FieldByName('LagInMonthsCount').AsInteger;
              LControlStructureDetails.FControlStructureData.FLagInMonthsCount.FInitalised := True;
            end;

            if not LDataSet.DataSet.FieldByName('PointsCount').IsNull then
            begin
              LControlStructureDetails.FControlStructureData.FPointsCount.FData :=LDataSet.DataSet.FieldByName('PointsCount').AsInteger;
              LControlStructureDetails.FControlStructureData.FPointsCount.FInitalised := True;
            end;

            if not (LDataSet.DataSet.FieldByName('IFRLoss').IsNull) then
            begin
              if LDataSet.DataSet.FieldByName('IFRLoss').AsInteger>0 then
              begin
                LControlStructureDetails.FControlStructureData.FIFRLoss.FData :=LDataSet.DataSet.FieldByName('IFRLoss').AsInteger;
                LControlStructureDetails.FControlStructureData.FIFRLoss.FInitalised := True;
              end;
            end;

            if not LDataSet.DataSet.FieldByName('IFRUnknown').IsNull then
            begin
              LControlStructureDetails.FControlStructureData.FIFRUnknown.FData :=LDataSet.DataSet.FieldByName('IFRUnknown').AsInteger;
              LControlStructureDetails.FControlStructureData.FIFRUnknown.FInitalised := True;
            end;

            if not LDataSet.DataSet.FieldByName('MonthlyIFRLoss').IsNull then
            begin
              if LDataSet.DataSet.FieldByName('IFRLoss').AsInteger>0 then
                LControlStructureDetails.FControlStructureData.FMonthlyIFRLoss.CommaText :=LDataSet.DataSet.FieldByName('MonthlyIFRLoss').AsString;
            end;

            //Line3 +++++++++++++++++++++++++++++++++
            if not LDataSet.DataSet.FieldByName('RefNodeNumbers').IsNull then
            begin
              lRefNodesStr := Trim(LDataSet.DataSet.FieldByName('RefNodeNumbers').AsString);
              LControlStructureDetails.FControlStructureData.FRefNodeNumber.CommaText := lRefNodesStr;
            end;
          end
          else
          begin
            LReference := LReleaseControlStructureObject.AddReference;
            if not Assigned(LReference) then
              Break;

            //Line6 +++++++++++++++++++++++++++++++++

            if not LDataSet.DataSet.FieldByName('IFRChannelNumber').IsNull then
            begin
              LReference.FReferenceChannel.FChannelNumber.FData :=LDataSet.DataSet.FieldByName('IFRChannelNumber').AsInteger;
              LReference.FReferenceChannel.FChannelNumber.FInitalised := True;
            end;

            if not LDataSet.DataSet.FieldByName('ReferenceNodeCount').IsNull then
            begin
              LReference.FReferenceChannel.FNodeCount.FData :=LDataSet.DataSet.FieldByName('ReferenceNodeCount').AsInteger;
              LReference.FReferenceChannel.FNodeCount.FInitalised := True;
            end;

            if not LDataSet.DataSet.FieldByName('CalculationOption').IsNull then
            begin
              LReference.FReferenceChannel.FCalculateOption.FData :=LDataSet.DataSet.FieldByName('CalculationOption').AsFloat;
              LReference.FReferenceChannel.FCalculateOption.FInitalised := True;
            end;

            if not LDataSet.DataSet.FieldByName('PointsCount').IsNull then
            begin
              LReference.FReferenceChannel.FClassCount.FData :=LDataSet.DataSet.FieldByName('PointsCount').AsInteger;
              LReference.FReferenceChannel.FClassCount.FInitalised := True;
            end;

            //Line7 +++++++++++++++++++++++++++++++++
            if not LDataSet.DataSet.FieldByName('RefNodeNumbers').IsNull then
            begin
              lRefNodesStr := Trim(LDataSet.DataSet.FieldByName('RefNodeNumbers').AsString);
              LReference.FReferenceNodeNumbers.CommaText := lRefNodesStr;
            end;
          end;

          //Line4 +++++++++++++++++++++++++++++++++
          LSubDataSet.DataSet.Close;
          LSubDataSet.SetSQL(ReadIFRFeaturesDetailsSQL);
          LSubDataSet.SetParams(['Identifier'], [IntToStr(LDataSet.DataSet.FieldByName('Identifier').AsInteger)]);
          LSubDataSet.DataSet.Open;

          while not LSubDataSet.DataSet.EOF do
          begin
            if (LDataSet.DataSet.FieldByName('ReferenceFlowType').AsInteger <> 1) then
            begin
              LInflowVariableLine := LControlStructureDetails.AddInflowVariableLine;
              if not Assigned(LInflowVariableLine) then
                Break;

              for LCount := MinReleaseStructure to MaxReleaseStructure do
              begin
                LFieldName := Format('%s%2.2d',['InflowVar',LCount]);
                if not LSubDataSet.DataSet.FieldByName(LFieldName).IsNull then
                begin
                  LInflowVariableLine.FInflowVariable[LCount].FData :=LSubDataSet.DataSet.FieldByName(LFieldName).AsFloat;
                  LInflowVariableLine.FInflowVariable[LCount].FInitalised := True;
                end;
                LFieldName := Format('%s%2.2d',['ReleaseVar',LCount]);
                if not LSubDataSet.DataSet.FieldByName(LFieldName).IsNull then
                begin
                  LInflowVariableLine.FReleaseVariable[LCount].FData :=LSubDataSet.DataSet.FieldByName(LFieldName).AsFloat;
                  LInflowVariableLine.FReleaseVariable[LCount].FInitalised := True;
                end;
              end;
            end
            else
            begin
              LReferenceRelease := LReference.AddReferenceRelease;
              if not Assigned(LReferenceRelease) then
                Break;

              if not LSubDataSet.DataSet.FieldByName('AnnualInflow').IsNull then
              begin
                LReferenceRelease.FAnualInflow.FData :=LSubDataSet.DataSet.FieldByName('AnnualInflow').AsFloat;
                LReferenceRelease.FAnualInflow.FInitalised := True;
              end;

              for LCount := MinReleaseStructure to MaxReleaseStructure do
              begin
                LFieldName := Format('%s%2.2d',['ReleaseVar',LCount]);
                if not LSubDataSet.DataSet.FieldByName(LFieldName).IsNull then
                begin
                  LReferenceRelease.FMonthlyRelease[LCount].FData :=LSubDataSet.DataSet.FieldByName(LFieldName).AsFloat;
                  LReferenceRelease.FMonthlyRelease[LCount].FInitalised := True;
                end;
              end;
            end;
            LSubDataSet.DataSet.Next;
          end;

          LDataSet.DataSet.Next;
        end;

        //Line1+++++++++++++++++++++++++++++++++
        //if(LReleaseControlStructureObject.FReleaseControlStructureDetails.Count > 0) then
        //begin
          LReleaseControlStructureObject.FControlStructCount.FData := LReleaseControlStructureObject.FReleaseControlStructureDetails.Count;
          LReleaseControlStructureObject.FControlStructCount.FInitalised := True;
        //end;

        //Line5+++++++++++++++++++++++++++++++++
        //if(LReleaseControlStructureObject.FReferenceDetails.Count > 0) then
        //begin
          LReleaseControlStructureObject.FReferenceChannelsCount.FData := LReleaseControlStructureObject.FReferenceDetails.Count;
          LReleaseControlStructureObject.FReferenceChannelsCount.FInitalised := True;
        //end;
      end;

      LDataSet.DataSet.Close;
      LDataSet.SetSQL(ReadF14UnknownDataSQL);
      LDataSet.SetParams(['FileType'], [IntToStr(AFileName.FileNumber)]);
      LDataSet.SetParams(['FileGroup'], [IntToStr(AFileName.FileGroup)]);
      LDataSet.DataSet.Open;

      while not LDataSet.DataSet.EOF do
      begin
        LReleaseControlStructureObject.FF14ExtraLines.Add(TrimRight(LDataSet.DataSet.FieldByName('LineData').AsString));
        LDataSet.DataSet.Next;
      end;
      LDataSet.DataSet.Close;

      LMessage := FAppModules.Language.GetString('TFile14DatabaseAgent.strReadEnded');
      AProgressFunction(LMessage,ptNone,LStop);
      Result :=  True;
    finally
      LDataSet.Free;
      LSubDataSet.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFile14DatabaseAgent.WriteModelDataToDatabase(AFileName:TFileNameObject;ADataObject: TDataFileObjects;
         AProgressFunction: TProgressUpdateFuntion): boolean;
const OPNAME = 'TFile14DatabaseAgent.WriteModelDataToDatabase';
var
  LWhereClause : string;
  LRefNodeNumbers : string;
  LMessage,
  LFieldName:string;
  LIdentifier,
  LCounter,
  LCount,
  LLineCount,
  LReleaseStructCount : integer;
  LDataSet : TAbstractModelDataset;
  LSubDataSet : TAbstractModelDataset;
  LReleaseControlStructureObject: TReleaseControlStructureObject;
  LControlStructureDetails:TReleaseControlStructureDetails;
  LInflowVariableLine :TInflowVariableLine;
  LStop: boolean;
  LReference: TReference;
  LReferenceRelease: TReferenceRelease;
begin
  Result := False;
  try
    if not Assigned(AProgressFunction) then
      AProgressFunction := DummyShowProgress;

    LMessage := FAppModules.Language.GetString('TFile14DatabaseAgent.strWriteStarted');
    AProgressFunction(LMessage,ptNone,LStop);

    if not Assigned(ADataObject) then
      raise Exception.Create('File object parameter is not yet assigned.');

    if not  ClearModelDataInDatabase(AFileName,AProgressFunction,True) then
      Exit;

    LReleaseControlStructureObject := ADataObject.FReleaseControlStructureObject;
    if not Assigned(LReleaseControlStructureObject) then
    begin
      Result := True;
      Exit;
    end;

    if(LReleaseControlStructureObject.FReleaseControlStructureDetails.Count = 0) and
      (LReleaseControlStructureObject.FReferenceDetails.Count = 0) and
      (LReleaseControlStructureObject.FF14ExtraLines.Count = 0) then
    begin
      LMessage := FAppModules.Language.GetString('TFile14DatabaseAgent.strWriteEnded');
      AProgressFunction(LMessage,ptNone,LStop);
      Result := True;
      Exit;
    end;

    //Line 1
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LSubDataSet);
    try

      //Line 1++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      if  LReleaseControlStructureObject.FInflowOption.FInitalised and
          FAppModules.FieldProperties.FieldAvailableInModel('IFRInflowOption') or (FAppModules.Model.ModelName = CPlanning) then
      begin
        LDataSet.DataSet.Close;
        LDataSet.SetSQL(WriteIFRReferenceSQL);
        LDataSet.ClearQueryParams();
        LDataSet.SetParams(['Model'], [FAppModules.StudyArea.ModelCode]);
        LDataSet.SetParams(['StudyAreaName'], [FAppModules.StudyArea.StudyAreaCode]);
        LDataSet.SetParams(['SubArea'], [FAppModules.StudyArea.SubAreaCode]);
        LDataSet.SetParams(['Scenario'], [FAppModules.StudyArea.ScenarioCode]);
        LDataSet.SetParams(['InflowOption'], [IntToStr(LReleaseControlStructureObject.FInflowOption.FData)]);
        LDataSet.ExecSQL;
      end;

      //Line 2++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      LIdentifier := 0;
      for LReleaseStructCount := 0 to  LReleaseControlStructureObject.FReleaseControlStructureDetails.Count -1 do
      begin
        LIdentifier := LIdentifier + 1;
        LDataSet.DataSet.Close;
        LDataSet.SetSQL(WriteIFRFeaturesSQL);
        LDataSet.ClearQueryParams();
        LDataSet.SetParams(['Model'], [FAppModules.StudyArea.ModelCode]);
        LDataSet.SetParams(['StudyAreaName'], [FAppModules.StudyArea.StudyAreaCode]);
        LDataSet.SetParams(['SubArea'], [FAppModules.StudyArea.SubAreaCode]);
        LDataSet.SetParams(['Scenario'], [FAppModules.StudyArea.ScenarioCode]);
        LDataSet.SetParams(['Identifier'], [IntToStr(LIdentifier)]);
        LControlStructureDetails := TReleaseControlStructureDetails(LReleaseControlStructureObject.FReleaseControlStructureDetails[LReleaseStructCount]);
        if Assigned(LControlStructureDetails) then
        begin
          if LControlStructureDetails.FControlStructureData.FIFRChannelNumber.FInitalised then
          begin
           LDataSet.SetParams(['IFRChannelNumber'], [IntToStr( LControlStructureDetails.FControlStructureData.FIFRChannelNumber.FData)]);
           LDataSet.SetParams(['FeatureName'], [UpperCase(FAppModules.Language.GetString('NetworkFeatures.IFRFeature')) + ' ' + IntToStr(LIdentifier)]);

         end;

          if LControlStructureDetails.FControlStructureData.FReferenceNodeCount.FInitalised then
           LDataSet.SetParams(['ReferenceNodeCount'], [IntToStr( LControlStructureDetails.FControlStructureData.FReferenceNodeCount.FData)]);

          if LControlStructureDetails.FControlStructureData.FLagInMonthsCount.FInitalised then
           LDataSet.SetParams(['LagInMonthsCount'], [IntToStr( LControlStructureDetails.FControlStructureData.FLagInMonthsCount.FData)]);

          if LControlStructureDetails.FControlStructureData.FPointsCount.FInitalised then
           LDataSet.SetParams(['PointsCount'], [IntToStr( LControlStructureDetails.FControlStructureData.FPointsCount.FData)]);

          if LControlStructureDetails.FControlStructureData.FIFRLoss.FInitalised then
           LDataSet.SetParams(['IFRLoss'], [IntToStr( LControlStructureDetails.FControlStructureData.FIFRLoss.FData)]);

          if LControlStructureDetails.FControlStructureData.FIFRUnknown.FInitalised then
           LDataSet.SetParams(['IFRUnknown'], [IntToStr( LControlStructureDetails.FControlStructureData.FIFRUnknown.FData)]);

          LDataSet.SetParams(['MonthlyIFRLoss'], [LControlStructureDetails.FControlStructureData.FMonthlyIFRLoss.CommaText]);

          LDataSet.SetParams(['ReferenceFlowType'], ['0']);
          LDataSet.SetParams(['IFRStatusIndicator'], ['0']);
          LDataSet.ExecSQL;


          //Line 3++++++++++++++++++++++++++++++++++++++++++++++++++++++++
          LRefNodeNumbers := LControlStructureDetails.FControlStructureData.FRefNodeNumber.CommaText;
          LWhereClause := ' WHERE Model         =  '+QuotedStr(FAppModules.StudyArea.ModelCode)+
                          ' AND StudyAreaName   =  '+QuotedStr(FAppModules.StudyArea.StudyAreaCode)+
                          ' AND SubArea         =  '+QuotedStr(FAppModules.StudyArea.SubAreaCode)+
                          ' AND Scenario        =  '+QuotedStr(FAppModules.StudyArea.ScenarioCode)+
                          ' AND Identifier      = ' + IntToStr(LIdentifier);
          FAppModules.Database.UpdateMemoField('IFRFeatures','RefNodeNumbers',LWhereClause, LRefNodeNumbers);


          //SubQuery
          for LLineCount := 0 to LControlStructureDetails.FInflowVariableLine.Count-1 do
          begin
            LSubDataSet.DataSet.Close;
            LSubDataSet.SetSQL(WriteIFRFeaturesDetailsSQL);
            LSubDataSet.ClearQueryParams(prFloat);
            LSubDataSet.SetParams(['Model'], [FAppModules.StudyArea.ModelCode]);
            LSubDataSet.SetParams(['StudyAreaName'], [FAppModules.StudyArea.StudyAreaCode]);
            LSubDataSet.SetParams(['SubArea'], [FAppModules.StudyArea.SubAreaCode]);
            LSubDataSet.SetParams(['Scenario'], [FAppModules.StudyArea.ScenarioCode]);
            LSubDataSet.SetParams(['Identifier'], [IntToStr(LIdentifier)]);
            LSubDataSet.SetParams(['LineNumber'], [IntToStr(LLineCount + 1)]);

            //Line 4++++++++++++++++++++++++++++++++++++++++++++++++++++++++
            LInflowVariableLine := TInflowVariableLine(LControlStructureDetails.FInflowVariableLine[LLineCount]);
            for LCount :=MinReleaseStructure to MaxReleaseStructure do
             begin
               LFieldName := Format('%s%2.2d',['InflowVar',LCount]);
               if LInflowVariableLine.FInflowVariable[LCount].FInitalised  then
                LSubDataSet.SetParams([LFieldName], [FloatToStr(LInflowVariableLine.FInflowVariable[LCount].FData)]);

               LFieldName := Format('%s%2.2d',['ReleaseVar',LCount]);
               if LInflowVariableLine.FReleaseVariable[LCount].FInitalised  then
                LSubDataSet.SetParams([LFieldName], [FloatToStr(LInflowVariableLine.FReleaseVariable[LCount].FData)]);
             end;
            LSubDataSet.ExecSQL;
          end;
        end;
      end;

      //Line 6++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      for LReleaseStructCount := 0 to  LReleaseControlStructureObject.FReferenceDetails.Count -1 do
      begin
        LIdentifier := LIdentifier + 1;
        LDataSet.DataSet.Close;
        LDataSet.SetSQL(WriteIFRFeaturesSQL);
        LDataSet.ClearQueryParams();
        LDataSet.SetParams(['Model'], [FAppModules.StudyArea.ModelCode]);
        LDataSet.SetParams(['StudyAreaName'], [FAppModules.StudyArea.StudyAreaCode]);
        LDataSet.SetParams(['SubArea'], [FAppModules.StudyArea.SubAreaCode]);
        LDataSet.SetParams(['Scenario'], [FAppModules.StudyArea.ScenarioCode]);
        LDataSet.SetParams(['Identifier'], [IntToStr(LIdentifier)]);
        LReference := LReleaseControlStructureObject.ReferenceByIndex(LReleaseStructCount);
        if Assigned(LReference) then
        begin
          if LReference.FReferenceChannel.FChannelNumber.FInitalised then
          begin
           LDataSet.SetParams(['IFRChannelNumber'], [IntToStr( LReference.FReferenceChannel.FChannelNumber.FData)]);
           LDataSet.SetParams(['FeatureName'], [UpperCase(FAppModules.Language.GetString('NetworkFeatures.IFRFeature')) + ' ' + IntToStr(LIdentifier)]);
         end;

          if LReference.FReferenceChannel.FNodeCount.FInitalised then
           LDataSet.SetParams(['ReferenceNodeCount'], [IntToStr( LReference.FReferenceChannel.FNodeCount.FData)]);

          if LReference.FReferenceChannel.FCalculateOption.FInitalised then
           LDataSet.SetParams(['CalculationOption'], [FloatToStr( LReference.FReferenceChannel.FCalculateOption.FData)]);

          if LReference.FReferenceChannel.FClassCount.FInitalised then
           LDataSet.SetParams(['PointsCount'], [FloatToStr( LReference.FReferenceChannel.FClassCount.FData)]);

          //Line 7++++++++++++++++++++++++++++++++++++++++++++++++++++++++
          LDataSet.SetParams(['RefNodeNumbers'], [LReference.FReferenceNodeNumbers.CommaText]);
          LDataSet.SetParams(['ReferenceFlowType'], ['1']);
          LDataSet.SetParams(['IFRStatusIndicator'], ['1']);
          LDataSet.ExecSQL;

          //SubQuery
          for LLineCount := 0 to LReference.FReferenceReleaseList.Count-1 do
          begin
            LSubDataSet.DataSet.Close;
            LSubDataSet.SetSQL(WriteIFRFeaturesDetailsSQL);
            LSubDataSet.ClearQueryParams(prFloat);
            LSubDataSet.SetParams(['Model'], [FAppModules.StudyArea.ModelCode]);
            LSubDataSet.SetParams(['StudyAreaName'], [FAppModules.StudyArea.StudyAreaCode]);
            LSubDataSet.SetParams(['SubArea'], [FAppModules.StudyArea.SubAreaCode]);
            LSubDataSet.SetParams(['Scenario'], [FAppModules.StudyArea.ScenarioCode]);
            LSubDataSet.SetParams(['Identifier'], [IntToStr(LIdentifier)]);
            LSubDataSet.SetParams(['LineNumber'], [IntToStr(LLineCount + 1)]);


            //Line 8++++++++++++++++++++++++++++++++++++++++++++++++++++++++
            LReferenceRelease   := LReference.ReferenceReleaseByIndex(LLineCount);

            if LReferenceRelease.FAnualInflow.FInitalised  then
             LSubDataSet.SetParams(['AnnualInflow'], [FloatToStr(LReferenceRelease.FAnualInflow.FData)]);

            for LCount :=MinReleaseStructure to MaxReleaseStructure do
             begin

               LFieldName := Format('%s%2.2d',['ReleaseVar',LCount]);
               if LReferenceRelease.FMonthlyRelease[LCount].FInitalised  then
                LSubDataSet.SetParams([LFieldName], [FloatToStr(LReferenceRelease.FMonthlyRelease[LCount].FData)]);
             end;
            LSubDataSet.ExecSQL;
          end;
        end;
      end;


      for LCounter := 0 to LReleaseControlStructureObject.FF14ExtraLines.Count - 1 do
      begin
        LDataSet.DataSet.Close;
        LDataSet.SetSQL(WriteUnknownDataSQL);
        LDataSet.ClearQueryParams();
        LDataSet.SetParams(['Model'], [FAppModules.StudyArea.ModelCode]);
        LDataSet.SetParams(['StudyAreaName'], [FAppModules.StudyArea.StudyAreaCode]);
        LDataSet.SetParams(['SubArea'], [FAppModules.StudyArea.SubAreaCode]);
        LDataSet.SetParams(['Scenario'], [FAppModules.StudyArea.ScenarioCode]);
        LDataSet.SetParams(['FileType'], [IntToStr(AFileName.FileNumber)]);
        LDataSet.SetParams(['FileGroup'], [IntToStr(AFileName.FileGroup)]);
        LDataSet.SetParams(['LineNumber'], [IntToStr(LCounter+1)]);
        LDataSet.SetParams(['LineSection'], [IntToStr(0)]);
        LDataSet.SetParams(['LineData'], [LReleaseControlStructureObject.FF14ExtraLines[LCounter]]);

        LDataSet.ExecSQL;
      end;
      LDataSet.DataSet.Close;

      Result := InsertFileName(AFileName);
      if Result then
      begin
        LMessage := FAppModules.Language.GetString('TFile14DatabaseAgent.strWriteEnded');
        AProgressFunction(LMessage,ptNone,LStop);
      end;
    finally
      LDataSet.Free;
    end;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFile14DatabaseAgent.ClearModelDataInDatabase(AFileName: TFileNameObject; AProgressFunction: TProgressUpdateFuntion;
         AQuetly: boolean = False): boolean;
const OPNAME = 'TFile14DatabaseAgent.ClearModelDataInDatabase';
var
  LTableNames,
  LMessage: string;
  LStop: boolean;
begin
  Result := False;
  try
    if not Assigned(AProgressFunction) then
      AProgressFunction := DummyShowProgress;

    if not AQuetly then
    begin
      LMessage := FAppModules.Language.GetString('TAbstractDatabaseAgent.strClearModelDataInDatabaseStarted');
      AProgressFunction(LMessage,ptNone,LStop);
    end;

    if not Assigned(AFileName) then
      raise Exception.Create('File name object parameter is not yet assigned.');

    LTableNames := 'IFRFeatures,IFRFeaturesDetails,IFRReference';
    Result := DeleteModelData(LTableNames,'',AProgressFunction,AQuetly);
    Result := Result and DeleteUnknownModelData(AFileName,AProgressFunction,AQuetly);
    Result := Result and DeleteFileName(AFileName);

    if Result and (not AQuetly)then
    begin
      LMessage := FAppModules.Language.GetString('TAbstractDatabaseAgent.strClearModelDataInDatabaseEnded');
      AProgressFunction(LMessage,ptNone,LStop);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
