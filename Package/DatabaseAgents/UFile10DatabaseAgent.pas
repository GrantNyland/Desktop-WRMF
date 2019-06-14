
//
//
//  UNIT      : Contains TFile10DatabaseAgent Class
//  AUTHOR    : Titi Ngubane(Arivia)
//  DATE      : 10/04/2003
//  COPYRIGHT : Copyright © 2003 DWAF
//
//
unit UFile10DatabaseAgent;

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
  UDivChannelDemandObject,
  UAbstractDatabaseAgent,
  UYieldModelDataObject;

type

  TFile10DatabaseAgent = class(TAbstractDatabaseAgent)
  protected
    function ReadF10UnknownDataSQL: string;
    function ReadDiversionFeaturesType1n2SQL: string;
    function ReadDiversionFeaturesSQL: string;
    function ReadDiversionFeaturesType3SQL: string;
    function ReadDiversionFeaturesType3ProportionsSQL: string;

    function WriteDiversionFeaturesType1n2SQL: string;
    function WriteDiversionChannelDemandDataSQL: string;
    function WriteDiversionFeaturesType3DataSQL: string;
    function WriteDiversionFeaturesType3ProportionsSQL: string;

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

uses UUtilities,
     UDataSetType,
     UChannelDescriptionObject,
     UErrorHandlingOperations;

function TFile10DatabaseAgent.ReadF10UnknownDataSQL: string;
const OPNAME = 'TFile10DatabaseAgent.ReadF10UnknownDataSQL';
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

function TFile10DatabaseAgent.ReadDiversionFeaturesSQL: string;
const OPNAME = 'TFile10DatabaseAgent.ReadDiversionFeaturesSQL';
begin

  Result := '';
  try
    Result := 'SELECT'+
      '  Dd.Model,Dd.StudyAreaName,Dd.SubArea,Dd.Scenario,Dd.Identifier'+
      ' ,Dd.DivChannelName,Dd.DivChannelNumber,Dd.DivChannelType'+
      ' FROM DiversionFeatures Dd'+
      ' WHERE  (Dd.Model            ='+ QuotedStr(FAppModules.StudyArea.ModelCode)    +') AND'+
      '        (Dd.StudyAreaName    ='+QuotedStr(FAppModules.StudyArea.StudyAreaCode) +') AND'+
      '        (Dd.SubArea          ='+QuotedStr(FAppModules.StudyArea.SubAreaCode)   +') AND'+
      '        (Dd.Scenario         ='+QuotedStr(FAppModules.StudyArea.ScenarioCode)  +')'+
      ' ORDER BY Dd.Model,Dd.StudyAreaName,Dd.SubArea,Dd.Scenario,Dd.Identifier';

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFile10DatabaseAgent.ReadDiversionFeaturesType1n2SQL: string;
const OPNAME = 'TFile10DatabaseAgent.ReadDiversionFeaturesType1n2SQL';
begin

  Result := '';
  try
    Result := 'SELECT'+
      '  Model,StudyAreaName,SubArea,Scenario,Identifier,DiversionCode'+
      ' ,DivFactor01,DivFactor02,DivFactor03,DivFactor04,DivFactor05,DivFactor06,DivFactor07,DivFactor08,DivFactor09,DivFactor10,DivFactor11,DivFactor12'+
      ' FROM DiversionFeaturesType1n2'+
      ' WHERE  (Model            ='+ QuotedStr(FAppModules.StudyArea.ModelCode)    +') AND'+
      '        (StudyAreaName    ='+QuotedStr(FAppModules.StudyArea.StudyAreaCode) +') AND'+
      '        (SubArea          ='+QuotedStr(FAppModules.StudyArea.SubAreaCode)   +') AND'+
      '        (Scenario         ='+QuotedStr(FAppModules.StudyArea.ScenarioCode)  +') AND'+
      '        Identifier       = :Identifier' +
      ' ORDER BY Model,StudyAreaName,SubArea,Scenario,Identifier,DiversionCode';

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFile10DatabaseAgent.ReadDiversionFeaturesType3SQL: string;
const OPNAME = 'TFile10DatabaseAgent.ReadDiversionFeaturesType3SQL';
begin

  Result := '';
  try
    Result :=
      'SELECT ' +
      ' Model, StudyAreaName, SubArea, Scenario, Identifier, ' +
      ' NodeNumber, ResLevelsCount, RefFlowsCount, ' +
      ' DivLevel01, DivLevel02, DivLevel03, DivLevel04, DivLevel05, DivLevel06, ' +
      ' DivLevel07, DivLevel08, DivLevel09, DivLevel10, DivLevel11, DivLevel12 ' +
      ' FROM DiversionFeaturesType3 ' +
      ' WHERE (Model         = ' + QuotedStr(FAppModules.StudyArea.ModelCode)     + ') AND ' +
      '       (StudyAreaName = ' + QuotedStr(FAppModules.StudyArea.StudyAreaCode) + ') AND ' +
      '       (SubArea       = ' + QuotedStr(FAppModules.StudyArea.SubAreaCode)   + ') AND ' +
      '       (Scenario      = ' + QuotedStr(FAppModules.StudyArea.ScenarioCode)  + ') AND ' +
      '       Identifier     = :Identifier ' +
      ' ORDER BY Identifier';

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFile10DatabaseAgent.ReadDiversionFeaturesType3ProportionsSQL: string;
const OPNAME = 'TFile10DatabaseAgent.ReadDiversionFeaturesType3ProportionsSQL';
begin

  Result := '';
  try
    Result := 'SELECT'+
      '  Model,StudyAreaName,SubArea,Scenario,Identifier,DiversionIndex,FlowValue'+
      ' ,DivProp01,DivProp02,DivProp03,DivProp04,DivProp05,DivProp06,DivProp07,DivProp08,DivProp09,DivProp10,DivProp11,DivProp12'+
      ' FROM DiversionFeaturesType3Proportions'+
      ' WHERE  (Model            ='+ QuotedStr(FAppModules.StudyArea.ModelCode)    +') AND'+
      '        (StudyAreaName    ='+QuotedStr(FAppModules.StudyArea.StudyAreaCode) +') AND'+
      '        (SubArea          ='+QuotedStr(FAppModules.StudyArea.SubAreaCode)   +') AND'+
      '        (Scenario         ='+QuotedStr(FAppModules.StudyArea.ScenarioCode)  +') AND'+
      '        Identifier       = :Identifier' +
      ' ORDER BY Model,StudyAreaName,SubArea,Scenario,Identifier,DiversionIndex';

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFile10DatabaseAgent.WriteDiversionChannelDemandDataSQL: string;
const OPNAME = 'TFile10DatabaseAgent.WriteDiversionChannelDemandDataSQL';
begin
  Result := '';
  try
    Result := 'INSERT INTO DiversionFeatures'+
              ' (Model,StudyAreaName,SubArea,Scenario,Identifier,DivChannelName,DivChannelNumber'+
              ' ,DivChannelType)'+
              ' Values'+
              '(:Model,:StudyAreaName,:SubArea,:Scenario,:Identifier,:DivChannelName,:DivChannelNumber'+
              ' ,:DivChannelType)';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFile10DatabaseAgent.WriteDiversionFeaturesType1n2SQL: string;
const OPNAME = 'TFile10DatabaseAgent.WriteDiversionFeaturesType1n2SQL';
begin
  Result := '';
  try
    Result := 'INSERT INTO DiversionFeaturesType1n2'+
              ' (Model,StudyAreaName,SubArea,Scenario,Identifier,DiversionCode'+
              ' ,DivFactor01,DivFactor02,DivFactor03,DivFactor04,DivFactor05,DivFactor06,DivFactor07,DivFactor08,DivFactor09,DivFactor10,DivFactor11,DivFactor12)'+
              ' Values'+
              '(:Model,:StudyAreaName,:SubArea,:Scenario,:Identifier,:DiversionCode'+
              ' ,:DivFactor01,:DivFactor02,:DivFactor03,:DivFactor04,:DivFactor05,:DivFactor06,:DivFactor07,:DivFactor08,:DivFactor09,:DivFactor10,:DivFactor11,:DivFactor12)';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFile10DatabaseAgent.WriteDiversionFeaturesType3DataSQL: string;
const OPNAME = 'TFile10DatabaseAgent.WriteDiversionFeaturesType3DataSQL';
begin
  Result := '';
  try
    Result :=
      'INSERT INTO DiversionFeaturesType3 ' +
      ' (Model, StudyAreaName, SubArea, Scenario, Identifier, ' +
      ' NodeNumber, ResLevelsCount, RefFlowsCount, ' +
      ' DivLevel01, DivLevel02, DivLevel03, DivLevel04, DivLevel05, DivLevel06, ' +
      ' DivLevel07, DivLevel08, DivLevel09, DivLevel10, DivLevel11, DivLevel12) ' +
      ' VALUES'+
      '(:Model, :StudyAreaName, :SubArea, :Scenario, :Identifier, ' +
      ' :NodeNumber, :ResLevelsCount, :RefFlowsCount, ' +
      ' :DivLevel01, :DivLevel02, :DivLevel03, :DivLevel04, :DivLevel05, :DivLevel06, ' +
      ' :DivLevel07, :DivLevel08, :DivLevel09, :DivLevel10, :DivLevel11, :DivLevel12)';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFile10DatabaseAgent.WriteDiversionFeaturesType3ProportionsSQL: string;
const OPNAME = 'TFile10DatabaseAgent.WriteDiversionFeaturesType3ProportionsSQL';
begin
  Result := '';
  try
    Result := 'INSERT INTO DiversionFeaturesType3Proportions'+
              ' (Model,StudyAreaName,SubArea,Scenario,Identifier,DiversionIndex,FlowValue'+
              ' ,DivProp01,DivProp02,DivProp03,DivProp04,DivProp05,DivProp06,DivProp07,DivProp08,DivProp09,DivProp10,DivProp11,DivProp12)'+
              ' Values'+
              '(:Model,:StudyAreaName,:SubArea,:Scenario,:Identifier,:DiversionIndex,:FlowValue'+
              ' ,:DivProp01,:DivProp02,:DivProp03,:DivProp04,:DivProp05,:DivProp06,:DivProp07,:DivProp08,:DivProp09,:DivProp10,:DivProp11,:DivProp12)';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFile10DatabaseAgent.WriteUnknownDataSQL: string;
const OPNAME = 'TFile10DatabaseAgent.WriteUnknownDataSQL';
begin
  Result := '';
  try
    Result := 'INSERT INTO WRYMFileLines'+
              ' (Model,StudyAreaName,SubArea,Scenario,FileType,FileGroup,LineNumber,LineSection,LineData)'+
              ' Values(:Model,:StudyAreaName,:SubArea,:Scenario,:FileType,:FileGroup,:LineNumber,:LineSection,:LineData)';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFile10DatabaseAgent.ReadModelDataFromDatabase(AFileName:TFileNameObject; ADataObject: TDataFileObjects;
         AProgressFunction: TProgressUpdateFuntion): boolean;
const OPNAME = 'TFile10DatabaseAgent.ReadModelDataFromDatabase';
var
  LFieldName,
  LMessage: string;
  LDataSet : TAbstractModelDataset;
  LSubDataSet : TAbstractModelDataset;
  LCount : Integer;
  LFlowLine:TFlowLine;
  LDiversionChannelData12:TDiversionChannelData12;
  LDiversionChannelData3 : TDiversionChannelData3;
  LDivChannelDemandObject: TDivChannelDemandObject;
  LStop: boolean;
begin
  Result := False;
  try
    if not Assigned(AProgressFunction) then
      AProgressFunction := DummyShowProgress;

    LMessage := FAppModules.Language.GetString('TFile10DatabaseAgent.strReadStarted');
    AProgressFunction(LMessage,ptNone,LStop);

    if not Assigned(ADataObject) then
      raise Exception.Create('File object parameter is not yet assigned.');

    LDivChannelDemandObject := ADataObject.FDivChannelDemandObject;

    if not LDivChannelDemandObject.Initialise then
    Exit;

    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LSubDataSet);
    try

      LDataSet.SetSQL(ReadDiversionFeaturesSQL);
      LDataSet.DataSet.Open;

      //Check if there is any data.
      if (LDataSet.DataSet.RecordCount = 0) then
      begin
        LMessage := FAppModules.Language.GetString('TFile10DatabaseAgent.strNoDataReturned');
        AProgressFunction(LMessage,ptWarning,LStop);
      end
      else
      begin
        while not LDataSet.DataSet.EOF do
        begin
          if LDataSet.DataSet.FieldByName('DivChannelType').AsInteger < 3 then
          begin
            LDiversionChannelData12 := LDivChannelDemandObject.AddDiversionChannelData12;
            if not LDataSet.DataSet.FieldByName('DivChannelType').IsNull then
            begin
              LDiversionChannelData12.FHeading.FChannelType.FData :=LDataSet.DataSet.FieldByName('DivChannelType').AsInteger;
              LDiversionChannelData12.FHeading.FChannelType.FInitalised := True;
            end;

            if not LDataSet.DataSet.FieldByName('DivChannelName').IsNull then
            begin
              LDiversionChannelData12.FHeading.FDivChannelName.FData := Trim(LDataSet.DataSet.FieldByName('DivChannelName').AsString);
              LDiversionChannelData12.FHeading.FDivChannelName.FInitalised := True;
            end;

            if not LDataSet.DataSet.FieldByName('DivChannelNumber').IsNull then
            begin
              LDiversionChannelData12.FHeading.FDivChannelNumber.FData :=LDataSet.DataSet.FieldByName('DivChannelNumber').AsInteger;
              LDiversionChannelData12.FHeading.FDivChannelNumber.FInitalised := True;
            end;

            LSubDataSet.DataSet.Close;
            LSubDataSet.SetSQL(ReadDiversionFeaturesType1n2SQL);
            LSubDataSet.SetParams(['Identifier'], [IntToStr(LDataSet.DataSet.FieldByName('Identifier').AsInteger)]);
            LSubDataSet.DataSet.Open;

            //Line 2 and 3 +++++++++++++++++++++++++++++++++
            while not LSubDataSet.DataSet.EOF do
            begin
              for LCount := MinDiversionDemand to MaxDiversionDemand do
              begin

                LFieldName := Format('%s%2.2d',['DivFactor',LCount]);
                //Line 2
                Case LSubDataSet.DataSet.FieldByName('DiversionCode').AsInteger of
                1:begin
                    if not LSubDataSet.DataSet.FieldByName(LFieldName).IsNull then
                    begin
                      LDiversionChannelData12.FLine1Array.FLineValues[LCount].FData :=LSubDataSet.DataSet.FieldByName(LFieldName).AsFloat;
                      LDiversionChannelData12.FLine1Array.FLineValues[LCount].FInitalised := True;
                    end;
                  end;
                //Line 3
                2:begin
                    if not LSubDataSet.DataSet.FieldByName(LFieldName).IsNull then
                    begin
                      LDiversionChannelData12.FLine2Array.FLineValues[LCount].FData :=LSubDataSet.DataSet.FieldByName(LFieldName).AsFloat;
                      LDiversionChannelData12.FLine2Array.FLineValues[LCount].FInitalised := True;
                    end;
                  end;
                end;//case
              end;
              LSubDataSet.DataSet.Next;
            end;
            LSubDataSet.DataSet.Close;
          end
          else
          begin
            if LDataSet.DataSet.FieldByName('DivChannelType').AsInteger = 3 then
            //Line 1 +++++++++++++++++++++++++++++
            begin
              LDiversionChannelData3 := LDivChannelDemandObject.AddDiversionChannelData3;
              if not LDataSet.DataSet.FieldByName('DivChannelType').IsNull then
              begin
                LDiversionChannelData3.FHeading.FChannelType.FData :=LDataSet.DataSet.FieldByName('DivChannelType').AsInteger;
                LDiversionChannelData3.FHeading.FChannelType.FInitalised := True;
              end;

              if not LDataSet.DataSet.FieldByName('DivChannelName').IsNull then
              begin
                LDiversionChannelData3.FHeading.FDivChannelName.FData := Trim(LDataSet.DataSet.FieldByName('DivChannelName').AsString);
                LDiversionChannelData3.FHeading.FDivChannelName.FInitalised := True;
              end;

              if not LDataSet.DataSet.FieldByName('DivChannelNumber').IsNull then
              begin
                LDiversionChannelData3.FHeading.FDivChannelNumber.FData :=LDataSet.DataSet.FieldByName('DivChannelNumber').AsInteger;
                LDiversionChannelData3.FHeading.FDivChannelNumber.FInitalised := True;
              end;

              LSubDataSet.DataSet.Close;
              LSubDataSet.SetSQL(ReadDiversionFeaturesType3SQL);
              LSubDataSet.SetParams(['Identifier'], [IntToStr(LDataSet.DataSet.FieldByName('Identifier').AsInteger)]);
              LSubDataSet.DataSet.Open;

              //Line 5 +++++++++++++++++++++++++++++++++
              while not LSubDataSet.DataSet.EOF do
              begin

                if not LSubDataSet.DataSet.FieldByName('NodeNumber').IsNull then
                begin
                  LDiversionChannelData3.FProportionCounts.FNodeNumber.FData :=LSubDataSet.DataSet.FieldByName('NodeNumber').AsInteger;
                  LDiversionChannelData3.FProportionCounts.FNodeNumber.FInitalised := True;
                end;

                if not LSubDataSet.DataSet.FieldByName('ResLevelsCount').IsNull then
                begin
                  LDiversionChannelData3.FProportionCounts.FResLevelsCount.FData :=LSubDataSet.DataSet.FieldByName('ResLevelsCount').AsInteger;
                  LDiversionChannelData3.FProportionCounts.FResLevelsCount.FInitalised := True;
                end;

                if not LSubDataSet.DataSet.FieldByName('RefFlowsCount').IsNull then
                begin
                  LDiversionChannelData3.FProportionCounts.FRefFlowsCount.FData :=LSubDataSet.DataSet.FieldByName('RefFlowsCount').AsInteger;
                  LDiversionChannelData3.FProportionCounts.FRefFlowsCount.FInitalised := True;
                end;

                for LCount := MinDiversionDemand to LDiversionChannelData3.FProportionCounts.FResLevelsCount.FData do
                begin
                  LFieldName := Format('%s%2.2d',['DivLevel',LCount]);
                  //Line 5
                  if not LSubDataSet.DataSet.FieldByName(LFieldName).IsNull then
                  begin
                    LDiversionChannelData3.FLineArray.FLineValues[LCount].FData :=LSubDataSet.DataSet.FieldByName(LFieldName).AsFloat;
                    LDiversionChannelData3.FLineArray.FLineValues[LCount].FInitalised := True;
                  end;
                end;
                LSubDataSet.DataSet.Next;
              end;

              //Line 6 +++++++++++++++++++++++++++++++++
              LSubDataSet.DataSet.Close;
              LSubDataSet.SetSQL(ReadDiversionFeaturesType3ProportionsSQL);
              LSubDataSet.SetParams(['Identifier'], [IntToStr(LDataSet.DataSet.FieldByName('Identifier').AsInteger)]);
              LSubDataSet.DataSet.Open;

              while not LSubDataSet.DataSet.EOF do
              begin

                LFlowLine := LDiversionChannelData3.FChannelProportion.AddProportionFlowArray;
                if not LSubDataSet.DataSet.FieldByName('FlowValue').IsNull then
                begin
                  LFlowLine.FFlowValue.FData :=LSubDataSet.DataSet.FieldByName('FlowValue').AsFloat;
                  LFlowLine.FFlowValue.FInitalised := True;
                end;

                for LCount := MinDiversionDemand to LDiversionChannelData3.FProportionCounts.FResLevelsCount.FData do
                begin
                  LFieldName := Format('%s%2.2d',['DivProp',LCount]);
                  //Line 6
                  if not LSubDataSet.DataSet.FieldByName(LFieldName).IsNull then
                  begin
                    LFlowLine.FLineValues[LCount].FData :=LSubDataSet.DataSet.FieldByName(LFieldName).AsFloat;
                    LFlowLine.FLineValues[LCount].FInitalised := True;
                  end;
                end;
                LSubDataSet.DataSet.Next;
              end;
            end;
          end;
          LDataSet.DataSet.Next;
        end;
      end;
      LDataSet.DataSet.Close;
      LDataSet.SetSQL(ReadF10UnknownDataSQL);
      LDataSet.SetParams(['FileType'], [IntToStr(AFileName.FileNumber)]);
      LDataSet.SetParams(['FileGroup'], [IntToStr(AFileName.FileGroup)]);
      LDataSet.DataSet.Open;

      while not LDataSet.DataSet.EOF do
      begin
        LDivChannelDemandObject.FF10ExtraLines.Add(TrimRight(LDataSet.DataSet.FieldByName('LineData').AsString));
        LDataSet.DataSet.Next;
      end;
      LDataSet.DataSet.Close;

      LMessage := FAppModules.Language.GetString('TFile10DatabaseAgent.strReadEnded');
      AProgressFunction(LMessage,ptNone,LStop);
      Result :=  True;
    finally
      LDataSet.Free;
      LSubDataSet.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFile10DatabaseAgent.WriteModelDataToDatabase(AFileName:TFileNameObject;ADataObject: TDataFileObjects;
         AProgressFunction: TProgressUpdateFuntion): boolean;
const OPNAME = 'TFile10DatabaseAgent.WriteModelDataToDatabase';
var
  LMessage,LFieldName:string;
  LLinesCount,
  LDiversionCode,
  LIdentifier,
  LCounter,LIndex,
  LCount   : integer;
  LDataSet : TAbstractModelDataset;
  LSubDataSet : TAbstractModelDataset;
  LFlowLine:TFlowLine;
  LDiversionChannelData12:TDiversionChannelData12;
  LDiversionChannelData3 : TDiversionChannelData3;
  LDivChannelDemandObject: TDivChannelDemandObject;
  LStop: boolean;
begin
  Result := False;
  try
    if not Assigned(AProgressFunction) then
      AProgressFunction := DummyShowProgress;

    LMessage := FAppModules.Language.GetString('TFile10DatabaseAgent.strWriteStarted');
    AProgressFunction(LMessage,ptNone,LStop);

    if not Assigned(ADataObject) then
      raise Exception.Create('File object parameter is not yet assigned.');

    if not ClearModelDataInDatabase(AFileName,AProgressFunction,True) then
      Exit;

    LDivChannelDemandObject := ADataObject.FDivChannelDemandObject;
    if not Assigned(LDivChannelDemandObject) then
      Exit;

    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LSubDataSet);
    try

      LIdentifier := 0;
      for LLinesCount := 0 to LDivChannelDemandObject.DiversionChannelCount - 1 do
      begin
        LIdentifier := LIdentifier + 1;
        LDataSet.DataSet.Close;
        LDataSet.SetSQL(WriteDiversionChannelDemandDataSQL);
        LDataSet.ClearQueryParams();
        LDataSet.SetParams(['Model'], [FAppModules.StudyArea.ModelCode]);
        LDataSet.SetParams(['StudyAreaName'], [FAppModules.StudyArea.StudyAreaCode]);
        LDataSet.SetParams(['SubArea'], [FAppModules.StudyArea.SubAreaCode]);
        LDataSet.SetParams(['Scenario'], [FAppModules.StudyArea.ScenarioCode]);
        LDataSet.SetParams(['Identifier'], [IntToStr(LIdentifier)]);

        if LDivChannelDemandObject.IsDiversionChannelData12(LLinesCount) then
        begin
          LDiversionChannelData12 := LDivChannelDemandObject.DiversionChannelData12[LLinesCount];

          if LDiversionChannelData12.FHeading.FChannelType.FInitalised then
           LDataSet.SetParams(['DivChannelType'], [IntToStr(LDiversionChannelData12.FHeading.FChannelType.FData)]);

          if LDiversionChannelData12.FHeading.FDivChannelName.FInitalised then
           LDataSet.SetParams(['DivChannelName'], [LDiversionChannelData12.FHeading.FDivChannelName.FData]);

          if LDiversionChannelData12.FHeading.FDivChannelNumber.FInitalised then
           LDataSet.SetParams(['DivChannelNumber'], [IntToStr(LDiversionChannelData12.FHeading.FDivChannelNumber.FData)]);

          LDataSet.ExecSQL;

          //SubQuery
          for LDiversionCode := 1 to 2 do
          begin
            LSubDataSet.DataSet.Close;
            LSubDataSet.SetSQL(WriteDiversionFeaturesType1n2SQL);
            LSubDataSet.ClearQueryParams(prFloat);
            LSubDataSet.SetParams(['Model'], [FAppModules.StudyArea.ModelCode]);
            LSubDataSet.SetParams(['StudyAreaName'], [FAppModules.StudyArea.StudyAreaCode]);
            LSubDataSet.SetParams(['SubArea'], [FAppModules.StudyArea.SubAreaCode]);
            LSubDataSet.SetParams(['Scenario'], [FAppModules.StudyArea.ScenarioCode]);
            LSubDataSet.SetParams(['Identifier'], [IntToStr(LIdentifier)]);
            LSubDataSet.SetParams(['DiversionCode'], [IntToStr(LDiversionCode)]);

              //Line 2 and 3
            for LCount := MinDiversionDemand to MaxDiversionDemand do
            begin
              LFieldName := Format('%s%2.2d',['DivFactor',LCount]);
              Case LDiversionCode of
              1:begin
                  if LDiversionChannelData12.FLine1Array.FLineValues[LCount].FInitalised  then
                  LSubDataSet.SetParams([LFieldName], [FloatToStr(LDiversionChannelData12.FLine1Array.FLineValues[LCount].FData)]);
                end;
              2:begin
                  if LDiversionChannelData12.FLine2Array.FLineValues[LCount].FInitalised then
                  LSubDataSet.SetParams([LFieldName], [FloatToStr(LDiversionChannelData12.FLine2Array.FLineValues[LCount].FData)]);
                end;
              end;//case
            end;
            LSubDataSet.ExecSQL;
          end;
        end
        else
        if LDivChannelDemandObject.IsDiversionChannelData3(LLinesCount) then
        begin
          LDiversionChannelData3 := TDiversionChannelData3(LDivChannelDemandObject.DiversionChannelData3[LLinesCount]);
          if LDiversionChannelData3.FHeading.FChannelType.FInitalised then
           LDataSet.SetParams(['DivChannelType'], [IntToStr(LDiversionChannelData3.FHeading.FChannelType.FData)]);

          if LDiversionChannelData3.FHeading.FDivChannelName.FInitalised then
           LDataSet.SetParams(['DivChannelName'], [LDiversionChannelData3.FHeading.FDivChannelName.FData]);

          if LDiversionChannelData3.FHeading.FDivChannelNumber.FInitalised then
           LDataSet.SetParams(['DivChannelNumber'], [IntToStr( LDiversionChannelData3.FHeading.FDivChannelNumber.FData)]);

          LDataSet.ExecSQL;

          //Line 4 & 5
          //SubQuery
          LSubDataSet.DataSet.Close;
          LSubDataSet.SetSQL(WriteDiversionFeaturesType3DataSQL);
          LSubDataSet.ClearQueryParams(prFloat);
          LSubDataSet.SetParams(['Model'], [FAppModules.StudyArea.ModelCode]);
          LSubDataSet.SetParams(['StudyAreaName'], [FAppModules.StudyArea.StudyAreaCode]);
          LSubDataSet.SetParams(['SubArea'], [FAppModules.StudyArea.SubAreaCode]);
          LSubDataSet.SetParams(['Scenario'], [FAppModules.StudyArea.ScenarioCode]);
          LSubDataSet.SetParams(['Identifier'], [IntToStr(LIdentifier)]);

          if LDiversionChannelData3.FProportionCounts.FNodeNumber.FInitalised then
           LSubDataSet.SetParams(['NodeNumber'], [IntToStr(LDiversionChannelData3.FProportionCounts.FNodeNumber.FData)]);

          if LDiversionChannelData3.FProportionCounts.FResLevelsCount.FInitalised then
           LSubDataSet.SetParams(['ResLevelsCount'], [IntToStr( LDiversionChannelData3.FProportionCounts.FResLevelsCount.FData)]);

          if LDiversionChannelData3.FProportionCounts.FRefFlowsCount.FInitalised then
           LSubDataSet.SetParams(['RefFlowsCount'], [IntToStr( LDiversionChannelData3.FProportionCounts.FRefFlowsCount.FData)]);

          for LCount := MinDiversionDemand to LDiversionChannelData3.FProportionCounts.FResLevelsCount.FData do
          begin
            LFieldName := Format('%s%2.2d',['DivLevel',LCount]);
            if LDiversionChannelData3.FLineArray.FLineValues[LCount].FInitalised  then
            LSubDataSet.SetParams([LFieldName], [FloatToStr(LDiversionChannelData3.FLineArray.FLineValues[LCount].FData)]);
          end;
          LSubDataSet.ExecSQL;

          //Line 6
          //SubQuery
          for LIndex := 0 to LDiversionChannelData3.FChannelProportion.ProportionCount - 1 do
          begin
            LFlowLine := TFlowLine(LDiversionChannelData3.FChannelProportion.FlowProportion[LIndex]);
            LSubDataSet.DataSet.Close;
            LSubDataSet.SetSQL(WriteDiversionFeaturesType3ProportionsSQL);
            LSubDataSet.ClearQueryParams(prFloat);
            LSubDataSet.SetParams(['Model'], [FAppModules.StudyArea.ModelCode]);
            LSubDataSet.SetParams(['StudyAreaName'], [FAppModules.StudyArea.StudyAreaCode]);
            LSubDataSet.SetParams(['SubArea'], [FAppModules.StudyArea.SubAreaCode]);
            LSubDataSet.SetParams(['Scenario'], [FAppModules.StudyArea.ScenarioCode]);
            LSubDataSet.SetParams(['Identifier'], [IntToStr(LIdentifier)]);
            LSubDataSet.SetParams(['DiversionIndex'], [IntToStr(LIndex + 1)]);

            if LFlowLine.FFlowValue.FInitalised then
             LSubDataSet.SetParams(['FlowValue'], [FloatToStr( LFlowLine.FFlowValue.FData)]);
           for LCount := MinDiversionDemand to LDiversionChannelData3.FProportionCounts.FResLevelsCount.FData do
           begin
             LFieldName := Format('%s%2.2d',['DivProp',LCount]);
             if LFlowLine.FLineValues[LCount].FInitalised  then
             LSubDataSet.SetParams([LFieldName], [FloatToStr(LFlowLine.FLineValues[LCount].FData)]);
           end;
           LSubDataSet.ExecSQL;
          end;
        end;
      end; //

      for LCounter := 0 to LDivChannelDemandObject.FF10ExtraLines.Count - 1 do
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
        LDataSet.SetParams(['LineData'], [LDivChannelDemandObject.FF10ExtraLines[LCounter]]);

        LDataSet.ExecSQL;
      end;
      LDataSet.DataSet.Close;

      Result := InsertFileName(AFileName);
      if Result then
      begin
        LMessage := FAppModules.Language.GetString('TFile10DatabaseAgent.strWriteEnded');
        AProgressFunction(LMessage,ptNone,LStop);
      end;
    finally
      LDataSet.Free;
      LSubDataSet.Free;
    end;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFile10DatabaseAgent.ClearModelDataInDatabase(AFileName: TFileNameObject; AProgressFunction: TProgressUpdateFuntion;
         AQuetly: boolean = False): boolean;
const OPNAME = 'TFile10DatabaseAgent.ClearModelDataInDatabase';
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

    LTableNames := 'DiversionFeatures,DiversionFeaturesType1n2,DiversionFeaturesType3,DiversionFeaturesType3Proportions';
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
