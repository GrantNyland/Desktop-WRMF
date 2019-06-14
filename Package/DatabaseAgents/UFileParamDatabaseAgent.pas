//
//
//  UNIT      : Contains TFileParamDatabaseAgent Class
//  AUTHOR    : Titi Ngubane(Arivia)
//  DATE      : 04/04/2003
//  COPYRIGHT : Copyright © 2003 DWAF
//
//
unit UFileParamDatabaseAgent;

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
  UParamObject,
  UAbstractDatabaseAgent,
  UYieldModelDataObject;

type

  TFileParamDatabaseAgent = class(TAbstractDatabaseAgent)
  protected
    FReadFileNamesOnly: boolean;
    function ReadParamFileNameSQL: string;
    function ReadParamHeaderDataSQL: string;
    function ReadParamStochasticsDataSQL: string;
    function ReadParamMatrixCommDataSQL: string;
    function ReadParamMatrixDataSQL: string;

    function WriteParamHeaderDataSQL: string;
    function WriteParamStochasticsDataSQL: string;
    function WriteParamMatrixDataCommSQL: string;
    function WriteParamMatrixDataSQL: string;

    procedure CreateMemberObjects; override;
  public
    { Public declarations }
    function ReadModelDataFromDatabase(AFileName:TFileNameObject; ADataObject: TDataFileObjects;
             AProgressFunction: TProgressUpdateFuntion): boolean; override;
    function WriteModelDataToDatabase(AFileName:TFileNameObject;ADataObject: TDataFileObjects;
             AProgressFunction: TProgressUpdateFuntion): boolean; override;
    function ClearModelDataInDatabase(AFileName:TFileNameObject;AProgressFunction: TProgressUpdateFuntion;
             AQuetly: boolean = False): boolean; override;
    property ReadFileNamesOnly: boolean read FReadFileNamesOnly write FReadFileNamesOnly;
  end;


implementation

uses UUtilities,
     UDataSetType,
     UErrorHandlingOperations, Math;

procedure TFileParamDatabaseAgent.CreateMemberObjects;
const OPNAME = 'TFileParamDatabaseAgent.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
    FReadFileNamesOnly := False;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFileParamDatabaseAgent.ReadParamFileNameSQL: string;
const OPNAME = 'TFileParamDatabaseAgent.ReadParamFileNameSQL';
begin
  Result := '';
  try
    Result := 'SELECT *'+
              ' FROM RunParameters'+
              ' WHERE Model       =  '+QuotedStr(FAppModules.StudyArea.ModelCode)+
              ' AND StudyAreaName =  '+QuotedStr(FAppModules.StudyArea.StudyAreaCode)+
              ' AND SubArea       =  '+QuotedStr(FAppModules.StudyArea.SubAreaCode)+
              ' AND Scenario      =  '+QuotedStr(FAppModules.StudyArea.ScenarioCode);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFileParamDatabaseAgent.ReadParamHeaderDataSQL: string;
const OPNAME = 'TFileParamDatabaseAgent.ReadParamHeaderDataSQL';
begin
  Result := '';
  try
    Result := 'SELECT *'+
              ' FROM ParamHeader'+
              ' WHERE Model       =  '+QuotedStr(FAppModules.StudyArea.ModelCode)+
              ' AND StudyAreaName =  '+QuotedStr(FAppModules.StudyArea.StudyAreaCode)+
              ' AND SubArea       =  '+QuotedStr(FAppModules.StudyArea.SubAreaCode)+
              ' AND Scenario      =  '+QuotedStr(FAppModules.StudyArea.ScenarioCode);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFileParamDatabaseAgent.ReadParamStochasticsDataSQL: string;
const OPNAME = 'TFileParamDatabaseAgent.ReadParamStochasticsDataSQL';
begin

  Result := '';
  try
    Result := 'SELECT'+
      '  Model,StudyAreaName,SubArea,Scenario,Identifier,GaugePathName,YearsNumber,YearStart'+
      ' ,Residual1,Residual2,Variate1,Variate2,TransformType,TransformGamma,TransformDelta,TransformXlam'+
      ' ,TransformXi,ResidualMean,ResidualStdDev,ArmaPhi1,ArmaPhi2,ArmaTheta1,ArmaTheta2,PhiZero,ZTVariates'+
      ' ,ParamXA,ParamXSD,ParamAIC,ParamANC,CatchmentArea'+
      ' FROM ParamStochastics'+
      ' WHERE  (Model            ='+ QuotedStr(FAppModules.StudyArea.ModelCode)    +') AND'+
      '        (StudyAreaName    ='+QuotedStr(FAppModules.StudyArea.StudyAreaCode) +') AND'+
      '        (SubArea          ='+QuotedStr(FAppModules.StudyArea.SubAreaCode)   +') AND'+
      '        (Scenario         ='+QuotedStr(FAppModules.StudyArea.ScenarioCode)  +')'+
      ' ORDER BY Model,StudyAreaName,SubArea,Scenario,Identifier';

  except on E: Exception do HandleError(E, OPNAME) end;
end;


function TFileParamDatabaseAgent.ReadParamMatrixCommDataSQL: string;
const OPNAME = 'TFileParamDatabaseAgent.ReadParamMatrixCommDataSQL';
begin

  Result := '';
  try
    Result := 'SELECT Model,StudyAreaName,SubArea,Scenario'+
              ' ,MatrixBComment,MatrixB0Comment,MatrixB1Comment,MatrixAComment,MatrixCComment'+
              ' FROM ParamMatrixComm'+
              ' WHERE Model       =  '+QuotedStr(FAppModules.StudyArea.ModelCode)+
              ' AND StudyAreaName =  '+QuotedStr(FAppModules.StudyArea.StudyAreaCode)+
              ' AND SubArea       =  '+QuotedStr(FAppModules.StudyArea.SubAreaCode)+
              ' AND Scenario      =  '+QuotedStr(FAppModules.StudyArea.ScenarioCode);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFileParamDatabaseAgent.ReadParamMatrixDataSQL: string;
const OPNAME = 'TFileParamDatabaseAgent.ReadParamMatrixDataSQL';
begin

  Result := '';
  try
    Result := 'SELECT'+
      '  Model,StudyAreaName,SubArea,Scenario,Identifier,MatrixType'+
      ' ,Matrix01,Matrix02,Matrix03,Matrix04,Matrix05,Matrix06,Matrix07,Matrix08,Matrix09,Matrix10'+
      ' FROM ParamMatrix'+
      ' WHERE  (Model            ='+ QuotedStr(FAppModules.StudyArea.ModelCode)    +') AND'+
      '        (StudyAreaName    ='+QuotedStr(FAppModules.StudyArea.StudyAreaCode) +') AND'+
      '        (SubArea          ='+QuotedStr(FAppModules.StudyArea.SubAreaCode)   +') AND'+
      '        (Scenario         ='+QuotedStr(FAppModules.StudyArea.ScenarioCode)  +')'+
      ' ORDER BY Model,StudyAreaName,SubArea,Scenario,Identifier,MatrixType';

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFileParamDatabaseAgent.WriteParamHeaderDataSQL: string;
const OPNAME = 'TFileParamDatabaseAgent.WriteParamHeaderDataSQL';
begin
  Result := '';
  try
    Result := 'INSERT INTO ParamHeader'+
              ' (Model,StudyAreaName,SubArea,Scenario,GaugeCount,GaugeComment,KeyGaugeCount'+
              ' ,KeyGauges)'+
              ' Values'+
              '(:Model,:StudyAreaName,:SubArea,:Scenario,:GaugeCount,:GaugeComment,:KeyGaugeCount'+
              ' ,:KeyGauges)';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFileParamDatabaseAgent.WriteParamStochasticsDataSQL: string;
const OPNAME = 'TFileParamDatabaseAgent.WriteParamStochasticsDataSQL';
begin
  Result := '';
  try
    Result := 'INSERT INTO ParamStochastics'+
              ' (Model,StudyAreaName,SubArea,Scenario,Identifier,GaugePathName,YearsNumber,YearStart'+
              ' ,Residual1,Residual2,Variate1,Variate2,TransformType,TransformGamma,TransformDelta,TransformXlam'+
              ' ,TransformXi,ResidualMean,ResidualStdDev,ArmaPhi1,ArmaPhi2,ArmaTheta1,ArmaTheta2,PhiZero,ZTVariates'+
              ' ,ParamXA,ParamXSD,ParamAIC,ParamANC,CatchmentArea)'+
              ' Values'+
              ' (:Model,:StudyAreaName,:SubArea,:Scenario,:Identifier,:GaugePathName,:YearsNumber,:YearStart'+
              ' ,:Residual1,:Residual2,:Variate1,:Variate2,:TransformType,:TransformGamma,:TransformDelta,:TransformXlam'+
              ' ,:TransformXi,:ResidualMean,:ResidualStdDev,:ArmaPhi1,:ArmaPhi2,:ArmaTheta1,:ArmaTheta2,:PhiZero,:ZTVariates'+
              ' ,:ParamXA,:ParamXSD,:ParamAIC,:ParamANC,:CatchmentArea)';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFileParamDatabaseAgent.WriteParamMatrixDataCommSQL: string;
const OPNAME = 'TFileParamDatabaseAgent.WriteParamMatrixDataCommSQL';
begin
  Result := '';
  try
    Result := 'INSERT INTO ParamMatrixComm'+
              ' (Model,StudyAreaName,SubArea,Scenario'+
              ' ,MatrixBComment,MatrixB0Comment,MatrixB1Comment,MatrixAComment,MatrixCComment)'+
              ' Values'+
              '(:Model,:StudyAreaName,:SubArea,:Scenario'+
              ' ,:MatrixBComment,:MatrixB0Comment,:MatrixB1Comment,:MatrixAComment,:MatrixCComment)'
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFileParamDatabaseAgent.WriteParamMatrixDataSQL: string;
const OPNAME = 'TFileParamDatabaseAgent.WriteParamMatrixDataSQL';
begin
  Result := '';
  try
    Result := 'INSERT INTO ParamMatrix'+
              ' (Model,StudyAreaName,SubArea,Scenario,Identifier,MatrixType'+
              ' ,Matrix01,Matrix02,Matrix03,Matrix04,Matrix05,Matrix06,Matrix07,Matrix08,Matrix09,Matrix10)'+
              ' Values'+
              '(:Model,:StudyAreaName,:SubArea,:Scenario,:Identifier,:MatrixType'+
              ' ,:Matrix01,:Matrix02,:Matrix03,:Matrix04,:Matrix05,:Matrix06,:Matrix07,:Matrix08,:Matrix09,:Matrix10)';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFileParamDatabaseAgent.ReadModelDataFromDatabase(AFileName:TFileNameObject; ADataObject: TDataFileObjects;
         AProgressFunction: TProgressUpdateFuntion): boolean;
const OPNAME = 'TFileParamDatabaseAgent.ReadModelDataFromDatabase';
var
  LFieldName,
  LMessage: string;
  LDataSet : TAbstractModelDataset;
  LCount : Integer;
  LParamObject :TParamObject;
  LParamHeaderObject : TParamHeader;
  LGaugeStochastics: TGaugeStochastics;
  LMatrixLine: TMatrixLine;
  LMatrixType: integer;
  LLinesCount : integer;
  LStop: boolean;
  lKeyGaugeStr : string;
  lStrValue    : string;
  lPos         : integer;
begin
  Result := False;
  try
    if not Assigned(AProgressFunction) then
      AProgressFunction := DummyShowProgress;

    LMessage := FAppModules.Language.GetString('TFileParamDatabaseAgent.strReadStarted');
    AProgressFunction(LMessage,ptNone,LStop);

    if not Assigned(ADataObject) then
      raise Exception.Create('File object parameter is not yet assigned.');

    LParamObject := ADataObject.FParamObject;

    if not LParamObject.Initialise then
    Exit;

    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      LDataSet.SetSQL(ReadParamHeaderDataSQL);
      LDataSet.DataSet.Open;

      //Check if there is any data.
      if (LDataSet.DataSet.RecordCount = 0) then
      begin
        LMessage := FAppModules.Language.GetString('TFileParamDatabaseAgent.strNoDataReturned');
        AProgressFunction(LMessage,ptWarning,LStop);
      end
      else
      begin
        LParamHeaderObject := LParamObject.ParamHeaderObject;

        if not LDataSet.DataSet.FieldByName('GaugeCount').IsNull then
        begin
          LParamHeaderObject.GaugeCount.FData :=LDataSet.DataSet.FieldByName('GaugeCount').AsInteger;
          LParamHeaderObject.GaugeCount.FInitalised := True;
        end;

        if not LDataSet.DataSet.FieldByName('GaugeComment').IsNull then
        begin
          LParamHeaderObject.GaugeComment.FData := TrimRight(LDataSet.DataSet.FieldByName('GaugeComment').AsString);
          LParamHeaderObject.GaugeComment.FInitalised := True;
          LParamHeaderObject.GaugeComment.FLength := Length(LParamHeaderObject.GaugeComment.FData);
        end;

        if not LDataSet.DataSet.FieldByName('KeyGaugeCount').IsNull then
        begin
          LParamHeaderObject.KeyGaugeCount.FData :=LDataSet.DataSet.FieldByName('KeyGaugeCount').AsInteger;
          LParamHeaderObject.KeyGaugeCount.FInitalised := True;
        end;

        lKeyGaugeStr := Trim(LDataSet.DataSet.FieldByName('KeyGauges').AsString);
        lCount       := MinKeyGauge;
        while ((LCount <= MaxKeyGauge) AND (lKeyGaugeStr <> '')) do
        begin
          lPos := Pos(',',lKeyGaugeStr);
          if (lPos = 0) then
          begin
            lStrValue    := lKeyGaugeStr;
            lKeyGaugeStr := '';
          end
          else
          begin
            lStrValue    := Copy(lKeyGaugeStr, 1, lPos-1);
            lKeyGaugeStr := Copy(lKeyGaugeStr, lPos+1, Length(lKeyGaugeStr) - lPos);
          end;
          if (lStrValue <> '') then
          begin
            LParamHeaderObject.KeyGauges[LCount].FData := StrToInt(lStrValue);
            LParamHeaderObject.KeyGauges[LCount].FInitalised := True;
          end;
          lCount := LCount + 1;
        end;

        LDataSet.DataSet.Close;
        LDataSet.SetSQL(ReadParamStochasticsDataSQL);
        LDataSet.DataSet.Open;

        LLinesCount := 0;
        while not LDataSet.DataSet.EOF do
        begin
          LGaugeStochastics := LParamObject.GaugeStochasticsContainer.AddGaugeStochastics;
          if not Assigned(LGaugeStochastics) then
            Exit;
          if((LLinesCount mod 50) = 0) then
          begin
            AProgressFunction('',ptNone,LStop);
            if LStop then Exit;
          end;
          LLinesCount := LLinesCount + 1;

          if not LDataSet.DataSet.FieldByName('GaugePathName').IsNull then
          begin
            LGaugeStochastics.GaugePathName.FData := IncludeTrailingPathDelimiter(ADataObject.FPathsObject.HydrologyPath.FData) +
                                                     Trim(LDataSet.DataSet.FieldByName('GaugePathName').AsString);
            LGaugeStochastics.GaugePathName.FInitalised := True;
            LGaugeStochastics.GaugePathName.FLength := Length(LGaugeStochastics.GaugePathName.FData);
          end;

          if not LDataSet.DataSet.FieldByName('YearsNumber').IsNull then
          begin
            LGaugeStochastics.YearsNumber.FData :=LDataSet.DataSet.FieldByName('YearsNumber').AsInteger;
            LGaugeStochastics.YearsNumber.FInitalised := True;
          end;

          if not LDataSet.DataSet.FieldByName('YearStart').IsNull then
          begin
            LGaugeStochastics.YearStart.FData :=LDataSet.DataSet.FieldByName('YearStart').AsInteger;
            LGaugeStochastics.YearStart.FInitalised := True;
          end;

          if not LDataSet.DataSet.FieldByName('Residual1').IsNull then
          begin
            LGaugeStochastics.Residual1.FData :=LDataSet.DataSet.FieldByName('Residual1').AsFloat;
            LGaugeStochastics.Residual1.FInitalised := True;
          end;

          if not LDataSet.DataSet.FieldByName('Residual2').IsNull then
          begin
            LGaugeStochastics.Residual2.FData :=LDataSet.DataSet.FieldByName('Residual2').AsFloat;
            LGaugeStochastics.Residual2.FInitalised := True;
          end;

          if not LDataSet.DataSet.FieldByName('Variate1').IsNull then
          begin
            LGaugeStochastics.Variate1.FData :=LDataSet.DataSet.FieldByName('Variate1').AsFloat;
            LGaugeStochastics.Variate1.FInitalised := True;
          end;

          if not LDataSet.DataSet.FieldByName('Variate2').IsNull then
          begin
            LGaugeStochastics.Variate2.FData :=LDataSet.DataSet.FieldByName('Variate2').AsFloat;
            LGaugeStochastics.Variate2.FInitalised := True;
          end;

          if not LDataSet.DataSet.FieldByName('TransformType').IsNull then
          begin
            LGaugeStochastics.TransformType.FData :=LDataSet.DataSet.FieldByName('TransformType').AsInteger;
            LGaugeStochastics.TransformType.FInitalised := True;
          end;

          if not LDataSet.DataSet.FieldByName('TransformGamma').IsNull then
          begin
            LGaugeStochastics.TransformGamma.FData :=LDataSet.DataSet.FieldByName('TransformGamma').AsFloat;
            LGaugeStochastics.TransformGamma.FInitalised := True;
          end;

          if not LDataSet.DataSet.FieldByName('TransformDelta').IsNull then
          begin
            LGaugeStochastics.TransformDelta.FData :=LDataSet.DataSet.FieldByName('TransformDelta').AsFloat;
            LGaugeStochastics.TransformDelta.FInitalised := True;
          end;

          if not LDataSet.DataSet.FieldByName('TransformXlam').IsNull then
          begin
            LGaugeStochastics.TransformXlam.FData :=LDataSet.DataSet.FieldByName('TransformXlam').AsFloat;
            LGaugeStochastics.TransformXlam.FInitalised := True;
          end;

          if not LDataSet.DataSet.FieldByName('TransformXi').IsNull then
          begin
            LGaugeStochastics.TransformXi.FData :=LDataSet.DataSet.FieldByName('TransformXi').AsFloat;
            LGaugeStochastics.TransformXi.FInitalised := True;
          end;

          if not LDataSet.DataSet.FieldByName('ResidualMean').IsNull then
          begin
            LGaugeStochastics.ResidualMean.FData :=LDataSet.DataSet.FieldByName('ResidualMean').AsFloat;
            LGaugeStochastics.ResidualMean.FInitalised := True;
          end;

          if not LDataSet.DataSet.FieldByName('ResidualStdDev').IsNull then
          begin
            LGaugeStochastics.ResidualStdDev.FData :=LDataSet.DataSet.FieldByName('ResidualStdDev').AsFloat;
            LGaugeStochastics.ResidualStdDev.FInitalised := True;
          end;

          if not LDataSet.DataSet.FieldByName('ArmaPhi1').IsNull then
          begin
            LGaugeStochastics.ArmaPhi1.FData :=LDataSet.DataSet.FieldByName('ArmaPhi1').AsFloat;
            LGaugeStochastics.ArmaPhi1.FInitalised := True;
          end;

          if not LDataSet.DataSet.FieldByName('ArmaPhi2').IsNull then
          begin
            LGaugeStochastics.ArmaPhi2.FData :=LDataSet.DataSet.FieldByName('ArmaPhi2').AsFloat;
            LGaugeStochastics.ArmaPhi2.FInitalised := True;
          end;

          if not LDataSet.DataSet.FieldByName('ArmaTheta1').IsNull then
          begin
            LGaugeStochastics.ArmaTheta1.FData :=LDataSet.DataSet.FieldByName('ArmaTheta1').AsFloat;
            LGaugeStochastics.ArmaTheta1.FInitalised := True;
          end;

          if not LDataSet.DataSet.FieldByName('ArmaTheta2').IsNull then
          begin
            LGaugeStochastics.ArmaTheta2.FData :=LDataSet.DataSet.FieldByName('ArmaTheta2').AsFloat;
            LGaugeStochastics.ArmaTheta2.FInitalised := True;
          end;

          if not LDataSet.DataSet.FieldByName('PhiZero').IsNull then
          begin
            LGaugeStochastics.PhiZero.FData :=LDataSet.DataSet.FieldByName('PhiZero').AsFloat;
            LGaugeStochastics.PhiZero.FInitalised := True;
          end;

          if not LDataSet.DataSet.FieldByName('ZTVariates').IsNull then
          begin
            LGaugeStochastics.ZTVariates.FData :=LDataSet.DataSet.FieldByName('ZTVariates').AsInteger;
            LGaugeStochastics.ZTVariates.FInitalised := True;
          end;

          if not LDataSet.DataSet.FieldByName('ParamXA').IsNull then
          begin
            LGaugeStochastics.ParamXA.FData :=LDataSet.DataSet.FieldByName('ParamXA').AsFloat;
            LGaugeStochastics.ParamXA.FInitalised := True;
          end;

          if not LDataSet.DataSet.FieldByName('ParamXSD').IsNull then
          begin
            LGaugeStochastics.ParamXSD.FData :=LDataSet.DataSet.FieldByName('ParamXSD').AsFloat;
            LGaugeStochastics.ParamXSD.FInitalised := True;
          end;

          if not LDataSet.DataSet.FieldByName('ParamAIC').IsNull then
          begin
            LGaugeStochastics.ParamAIC.FData :=LDataSet.DataSet.FieldByName('ParamAIC').AsFloat;
            LGaugeStochastics.ParamAIC.FInitalised := True;
          end;

          if not LDataSet.DataSet.FieldByName('ParamANC').IsNull then
          begin
            LGaugeStochastics.ParamANC.FData :=LDataSet.DataSet.FieldByName('ParamANC').AsFloat;
            LGaugeStochastics.ParamANC.FInitalised := True;
          end;

          if not LDataSet.DataSet.FieldByName('CatchmentArea').IsNull then
          begin
            LGaugeStochastics.CatchmentArea.FData :=LDataSet.DataSet.FieldByName('CatchmentArea').AsFloat;
            LGaugeStochastics.CatchmentArea.FInitalised := True;
          end;

          LDataSet.DataSet.Next;
        end;

        if not FReadFileNamesOnly  then
        begin
          LDataSet.DataSet.Close;
          LDataSet.SetSQL(ReadParamMatrixCommDataSQL);
          LDataSet.DataSet.Open;

          if not (LDataSet.DataSet.RecordCount = 0) then
          begin

            if not LDataSet.DataSet.FieldByName('MatrixBComment').IsNull then
            begin
              LParamObject.MatrixComments.MatrixBComment.FData := TrimRight(LDataSet.DataSet.FieldByName('MatrixBComment').AsString);
              LParamObject.MatrixComments.MatrixBComment.FInitalised := True;
              LParamObject.MatrixComments.MatrixBComment.FLength := Length(LParamObject.MatrixComments.MatrixBComment.FData);
            end;

            if not LDataSet.DataSet.FieldByName('MatrixB0Comment').IsNull then
            begin
              LParamObject.MatrixComments.MatrixB0Comment.FData := TrimRight(LDataSet.DataSet.FieldByName('MatrixB0Comment').AsString);
              LParamObject.MatrixComments.MatrixB0Comment.FInitalised := True;
              LParamObject.MatrixComments.MatrixB0Comment.FLength := Length(LParamObject.MatrixComments.MatrixB0Comment.FData);
            end;

            if not LDataSet.DataSet.FieldByName('MatrixB1Comment').IsNull then
            begin
              LParamObject.MatrixComments.MatrixB1Comment.FData := TrimRight(LDataSet.DataSet.FieldByName('MatrixB1Comment').AsString);
              LParamObject.MatrixComments.MatrixB1Comment.FInitalised := True;
              LParamObject.MatrixComments.MatrixB1Comment.FLength := Length(LParamObject.MatrixComments.MatrixB1Comment.FData);
            end;

            if not LDataSet.DataSet.FieldByName('MatrixAComment').IsNull then
            begin
              LParamObject.MatrixComments.MatrixAComment.FData := TrimRight(LDataSet.DataSet.FieldByName('MatrixAComment').AsString);
              LParamObject.MatrixComments.MatrixAComment.FInitalised := True;
              LParamObject.MatrixComments.MatrixAComment.FLength := Length(LParamObject.MatrixComments.MatrixAComment.FData);
            end;

            if not LDataSet.DataSet.FieldByName('MatrixCComment').IsNull then
            begin
              LParamObject.MatrixComments.MatrixCComment.FData := TrimRight(LDataSet.DataSet.FieldByName('MatrixCComment').AsString);
              LParamObject.MatrixComments.MatrixCComment.FInitalised := True;
              LParamObject.MatrixComments.MatrixCComment.FLength := Length(LParamObject.MatrixComments.MatrixCComment.FData);
            end;
          end;


          LDataSet.DataSet.Close;
          LDataSet.SetSQL(ReadParamMatrixDataSQL);
          LDataSet.DataSet.Open;

          LLinesCount := 0;
          while not LDataSet.DataSet.EOF do
          begin
            if((LLinesCount mod 50) = 0) then
            begin
              AProgressFunction('',ptNone,LStop);
              if LStop then Exit;
            end;

            LLinesCount := LLinesCount + 1;
            LMatrixType := LDataSet.DataSet.FieldByName('MatrixType').AsInteger;
            case TMatrixType(LMatrixType) of
              mtMatrixB:
              begin
                LMatrixLine := LParamObject.MatrixB.AddMatrixLine;
                if not Assigned(LMatrixLine) then
                  Exit;

                for LCount := MinMatrix to MaxMatrix do
                begin
                  LFieldName := Format('%s%2.2d',['Matrix',LCount]);
                  if not LDataSet.DataSet.FieldByName(LFieldName).IsNull then
                  begin
                    LMatrixLine.MatrixLine[LCount].FData :=LDataSet.DataSet.FieldByName(LFieldName).AsFloat;
                    LMatrixLine.MatrixLine[LCount].FInitalised := True;
                  end;
                end;
              end;
              mtMatrixB0:
              begin
                LMatrixLine := LParamObject.MatrixB0.AddMatrixLine;
                if not Assigned(LMatrixLine) then
                  Exit;

                for LCount := MinMatrix to MaxMatrix do
                begin
                  LFieldName := Format('%s%2.2d',['Matrix',LCount]);
                  if not LDataSet.DataSet.FieldByName(LFieldName).IsNull then
                  begin
                    LMatrixLine.MatrixLine[LCount].FData :=LDataSet.DataSet.FieldByName(LFieldName).AsFloat;
                    LMatrixLine.MatrixLine[LCount].FInitalised := True;
                  end;
                end;
              end;
              mtMatrixB1:
              begin
                LMatrixLine := LParamObject.MatrixB1.AddMatrixLine;
                if not Assigned(LMatrixLine) then
                  Exit;

                for LCount := MinMatrix to MaxMatrix do
                begin
                  LFieldName := Format('%s%2.2d',['Matrix',LCount]);
                  if not LDataSet.DataSet.FieldByName(LFieldName).IsNull then
                  begin
                    LMatrixLine.MatrixLine[LCount].FData :=LDataSet.DataSet.FieldByName(LFieldName).AsFloat;
                    LMatrixLine.MatrixLine[LCount].FInitalised := True;
                  end;
                end;
              end;
              mtMatrixA:
              begin
                LMatrixLine := LParamObject.MatrixA.AddMatrixLine;
                if not Assigned(LMatrixLine) then
                  Exit;

                for LCount := MinMatrix to MaxMatrix do
                begin
                  LFieldName := Format('%s%2.2d',['Matrix',LCount]);
                  if not LDataSet.DataSet.FieldByName(LFieldName).IsNull then
                  begin
                    LMatrixLine.MatrixLine[LCount].FData :=LDataSet.DataSet.FieldByName(LFieldName).AsFloat;
                    LMatrixLine.MatrixLine[LCount].FInitalised := True;
                  end;
                end;
              end;
              mtMatrixC:
              begin
                LMatrixLine := LParamObject.MatrixC.AddMatrixLine;
                if not Assigned(LMatrixLine) then
                  Exit;

                for LCount := MinMatrix to MaxMatrix do
                begin
                  LFieldName := Format('%s%2.2d',['Matrix',LCount]);
                  if not LDataSet.DataSet.FieldByName(LFieldName).IsNull then
                  begin
                    LMatrixLine.MatrixLine[LCount].FData :=LDataSet.DataSet.FieldByName(LFieldName).AsFloat;
                    LMatrixLine.MatrixLine[LCount].FInitalised := True;
                  end;
                end;
              end;
              else
              begin
                LMessage := FAppModules.Language.GetString('TFileParamDatabaseAgent.strInvalidMatrixType');
                LMessage := Format(LMessage,[LMatrixType]);
                AProgressFunction(LMessage,ptError,LStop);
                Exit;
              end;
            end;//case
            LDataSet.DataSet.Next;
          end;
        end;
      end;

      LDataSet.DataSet.Close;

      LMessage := FAppModules.Language.GetString('TFileParamDatabaseAgent.strReadEnded');
      AProgressFunction(LMessage,ptNone,LStop);
      Result :=  True;
    finally
      LDataSet.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFileParamDatabaseAgent.WriteModelDataToDatabase(AFileName:TFileNameObject;ADataObject: TDataFileObjects;
         AProgressFunction: TProgressUpdateFuntion): boolean;
const OPNAME = 'TFileParamDatabaseAgent.WriteModelDataToDatabase';
var
  LMessage:string;
  LFieldName:string;
  LLinesCount,
  LIdentifier,
  LCount  : integer;
  LDataSet : TAbstractModelDataset;
  LParamObject :TParamObject;
  LParamHeaderObject : TParamHeader;
  LGaugeStochastics: TGaugeStochastics;
  LMatrixLine: TMatrixLine;
  LStop: boolean;
  lKeyGaugeStr : string;
  LBlobStream   : TStream;
  LMStream      : TMemoryStream;
  LKeyGaugeList : TStringList;
begin
  Result := False;
  try
    if not Assigned(AProgressFunction) then
      AProgressFunction := DummyShowProgress;

    LMessage := FAppModules.Language.GetString('TFileParamDatabaseAgent.strWriteStarted');
    AProgressFunction(LMessage,ptNone,LStop);

    if not Assigned(ADataObject) then
      raise Exception.Create('File object parameter is not yet assigned.');

    if not  ClearModelDataInDatabase(AFileName,AProgressFunction,True) then
      Exit;

    LParamObject := ADataObject.FParamObject;
    if not Assigned(LParamObject) then
      Exit;

    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try

      LParamHeaderObject := LParamObject.ParamHeaderObject;
      if not Assigned(LParamObject) then
        Exit;

      LDataSet.DataSet.Close;
      LDataSet.SetSQL(WriteParamHeaderDataSQL);
      LDataSet.ClearQueryParams();
      LDataSet.SetParams(['Model'], [FAppModules.StudyArea.ModelCode]);
      LDataSet.SetParams(['StudyAreaName'], [FAppModules.StudyArea.StudyAreaCode]);
      LDataSet.SetParams(['SubArea'], [FAppModules.StudyArea.SubAreaCode]);
      LDataSet.SetParams(['Scenario'], [FAppModules.StudyArea.ScenarioCode]);

      if LParamHeaderObject.GaugeCount.FInitalised then
       LDataSet.SetParams(['GaugeCount'], [IntToStr(LParamHeaderObject.GaugeCount.FData)]);

      if LParamHeaderObject.KeyGaugeCount.FInitalised then
       LDataSet.SetParams(['KeyGaugeCount'], [IntToStr(LParamHeaderObject.KeyGaugeCount.FData)]);

      if LParamHeaderObject.GaugeComment.FInitalised then
       LDataSet.SetParams(['GaugeComment'], [LParamHeaderObject.GaugeComment.FData]);

      lKeyGaugeStr := '';
      for LCount := MinKeyGauge to MaxKeyGauge do
      begin
        if LParamHeaderObject.KeyGauges[LCount].FInitalised then
        begin
          if (lKeyGaugeStr <> '') then
            lKeyGaugeStr := lKeyGaugeStr + ',';
          lKeyGaugeStr := lKeyGaugeStr + IntToStr(LParamHeaderObject.KeyGauges[LCount].FData);
        end;
      end;
      LDataSet.SetParamValue ( 'KeyGauges', Copy ( lKeyGaugeStr, 1, 255 ), ftString );
      LDataSet.ExecSQL;

      if ( Length ( lKeyGaugeStr ) > 255 ) then
      begin
        LDataSet.DataSet.Close;
        LDataSet.SetSQL ( ReadParamHeaderDataSQL );
        LDataSet.SetReadOnly ( False );
        LDataSet.DataSet.Open;
        LDataSet.DataSet.Edit;
        if not LDataset.DataSet.Eof then
        begin
          try
            LMStream := TMemoryStream.Create;
            try
              LKeyGaugeList := TStringList.Create;
              LKeyGaugeList.Add ( lKeyGaugeStr );
              LMStream.Clear;
              LMStream.Position := 0;
              LKeyGaugeList.SaveToStream ( LMStream );
            finally
              FreeAndNil ( LKeyGaugeList );
            end;
            LBlobStream := LDataSet.DataSet.CreateBlobStream ( LDataSet.DataSet.FieldByName ( 'KeyGauges' ),bmWrite );
            LMStream.Position := 0;
            LBlobStream.CopyFrom ( LMStream, LMStream.Size );
            LDataSet.DataSet.Post;
          finally
            FreeAndNil ( LMStream );
            FreeAndNil ( LBlobStream );
          end;
        end;
      end;
      
      LIdentifier := 0;
      for LLinesCount := 0 to LParamObject.GaugeStochasticsContainer.ItemsCount-1 do
      begin
        LGaugeStochastics := LParamObject.GaugeStochasticsContainer.GaugeStochasticsByIndex[LLinesCount];
        if not Assigned(LGaugeStochastics) then
          Exit;


        if((LIdentifier mod 50) = 0) then
        begin
          AProgressFunction('',ptNone,LStop);
          if LStop then Exit;
        end;

        LIdentifier := LIdentifier + 1;
        LDataSet.DataSet.Close;
        LDataSet.SetSQL(WriteParamStochasticsDataSQL);
        LDataSet.ClearQueryParams();
        LDataSet.SetParams(['Model'], [FAppModules.StudyArea.ModelCode]);
        LDataSet.SetParams(['StudyAreaName'], [FAppModules.StudyArea.StudyAreaCode]);
        LDataSet.SetParams(['SubArea'], [FAppModules.StudyArea.SubAreaCode]);
        LDataSet.SetParams(['Scenario'], [FAppModules.StudyArea.ScenarioCode]);
        LDataSet.SetParams(['Identifier'], [IntToStr(LIdentifier)]);

        if LGaugeStochastics.GaugePathName.FInitalised then
         LDataSet.SetParams(['GaugePathName'], [ExtractFileName(LGaugeStochastics.GaugePathName.FData)]);

        if LGaugeStochastics.YearsNumber.FInitalised then
         LDataSet.SetParams(['YearsNumber'], [IntToStr( LGaugeStochastics.YearsNumber.FData)]);

        if LGaugeStochastics.YearStart.FInitalised then
         LDataSet.SetParams(['YearStart'], [IntToStr( LGaugeStochastics.YearStart.FData)]);

        if LGaugeStochastics.Residual1.FInitalised then
         LDataSet.SetParams(['Residual1'], [FloatToStr(LGaugeStochastics.Residual1.FData)]);

        if LGaugeStochastics.Residual2.FInitalised then
         LDataSet.SetParams(['Residual2'], [FloatToStr(LGaugeStochastics.Residual2.FData)]);

        if LGaugeStochastics.Variate1.FInitalised then
         LDataSet.SetParams(['Variate1'], [FloatToStr(LGaugeStochastics.Variate1.FData)]);

        if LGaugeStochastics.Variate2.FInitalised then
         LDataSet.SetParams(['Variate2'], [FloatToStr(LGaugeStochastics.Variate2.FData)]);

        if LGaugeStochastics.TransformType.FInitalised then
         LDataSet.SetParams(['TransformType'], [IntToStr( LGaugeStochastics.TransformType.FData)]);

        if LGaugeStochastics.TransformGamma.FInitalised then
         LDataSet.SetParams(['TransformGamma'], [FloatToStr(LGaugeStochastics.TransformGamma.FData)]);

        if LGaugeStochastics.TransformDelta.FInitalised then
         LDataSet.SetParams(['TransformDelta'], [FloatToStr(LGaugeStochastics.TransformDelta.FData)]);

        if LGaugeStochastics.TransformXlam.FInitalised then
         LDataSet.SetParams(['TransformXlam'], [FloatToStr(LGaugeStochastics.TransformXlam.FData)]);

        if LGaugeStochastics.TransformXi.FInitalised then
         LDataSet.SetParams(['TransformXi'], [FloatToStr(LGaugeStochastics.TransformXi.FData)]);

        if LGaugeStochastics.ResidualMean.FInitalised then
         LDataSet.SetParams(['ResidualMean'], [FloatToStr(LGaugeStochastics.ResidualMean.FData)]);

        if LGaugeStochastics.ResidualStdDev.FInitalised then
         LDataSet.SetParams(['ResidualStdDev'], [FloatToStr(LGaugeStochastics.ResidualStdDev.FData)]);

        if LGaugeStochastics.ArmaPhi1.FInitalised then
         LDataSet.SetParams(['ArmaPhi1'], [FloatToStr(LGaugeStochastics.ArmaPhi1.FData)]);

        if LGaugeStochastics.ArmaPhi2.FInitalised then
         LDataSet.SetParams(['ArmaPhi2'], [FloatToStr(LGaugeStochastics.ArmaPhi2.FData)]);

        if LGaugeStochastics.ArmaTheta1.FInitalised then
         LDataSet.SetParams(['ArmaTheta1'], [FloatToStr(LGaugeStochastics.ArmaTheta1.FData)]);

        if LGaugeStochastics.ArmaTheta2.FInitalised then
         LDataSet.SetParams(['ArmaTheta2'], [FloatToStr(LGaugeStochastics.ArmaTheta2.FData)]);

        if LGaugeStochastics.PhiZero.FInitalised then
         LDataSet.SetParams(['PhiZero'], [FloatToStr(LGaugeStochastics.PhiZero.FData)]);

        if LGaugeStochastics.ZTVariates.FInitalised then
         LDataSet.SetParams(['ZTVariates'], [IntToStr( LGaugeStochastics.ZTVariates.FData)]);

        if LGaugeStochastics.ParamXA.FInitalised then
         LDataSet.SetParams(['ParamXA'], [FloatToStr(LGaugeStochastics.ParamXA.FData)]);

        if LGaugeStochastics.ParamXSD.FInitalised then
         LDataSet.SetParams(['ParamXSD'], [FloatToStr(LGaugeStochastics.ParamXSD.FData)]);

        if LGaugeStochastics.ParamAIC.FInitalised then
         LDataSet.SetParams(['ParamAIC'], [FloatToStr(LGaugeStochastics.ParamAIC.FData)]);

        if LGaugeStochastics.ParamANC.FInitalised then
         LDataSet.SetParams(['ParamANC'], [FloatToStr(LGaugeStochastics.ParamANC.FData)]);

        if LGaugeStochastics.CatchmentArea.FInitalised then
         LDataSet.SetParams(['CatchmentArea'], [FloatToStr(LGaugeStochastics.CatchmentArea.FData)]);

        LDataSet.ExecSQL;
      end;

      LDataSet.DataSet.Close;
      LDataSet.SetSQL(WriteParamMatrixDataCommSQL);
      LDataSet.ClearQueryParams(prFloat);
      LDataSet.SetParams(['Model'], [FAppModules.StudyArea.ModelCode]);
      LDataSet.SetParams(['StudyAreaName'], [FAppModules.StudyArea.StudyAreaCode]);
      LDataSet.SetParams(['SubArea'], [FAppModules.StudyArea.SubAreaCode]);
      LDataSet.SetParams(['Scenario'], [FAppModules.StudyArea.ScenarioCode]);

      if LParamObject.MatrixComments.MatrixBComment.FInitalised then
       LDataSet.SetParams(['MatrixBComment'], [LParamObject.MatrixComments.MatrixBComment.FData]);

      if LParamObject.MatrixComments.MatrixB0Comment.FInitalised then
       LDataSet.SetParams(['MatrixB0Comment'], [LParamObject.MatrixComments.MatrixB0Comment.FData]);

      if LParamObject.MatrixComments.MatrixB1Comment.FInitalised then
       LDataSet.SetParams(['MatrixB1Comment'], [LParamObject.MatrixComments.MatrixB1Comment.FData]);

      if LParamObject.MatrixComments.MatrixAComment.FInitalised then
       LDataSet.SetParams(['MatrixAComment'], [LParamObject.MatrixComments.MatrixAComment.FData]);

      if LParamObject.MatrixComments.MatrixCComment.FInitalised then
       LDataSet.SetParams(['MatrixCComment'], [LParamObject.MatrixComments.MatrixCComment.FData]);

      LDataSet.ExecSQL;

      LIdentifier := 0;
      for LLinesCount := 0 to LParamObject.MatrixB.ItemsCount-1 do
      begin
        LMatrixLine := LParamObject.MatrixB.MatrixLineByIndex[LLinesCount];
        if not Assigned(LMatrixLine) then
          Exit;

        if((LIdentifier mod 50) = 0) then
        begin
          AProgressFunction('',ptNone,LStop);
          if LStop then Exit;
        end;

        LIdentifier := LIdentifier + 1;
        LDataSet.DataSet.Close;
        LDataSet.SetSQL(WriteParamMatrixDataSQL);
        LDataSet.ClearQueryParams();
        LDataSet.SetParams(['Model'], [FAppModules.StudyArea.ModelCode]);
        LDataSet.SetParams(['StudyAreaName'], [FAppModules.StudyArea.StudyAreaCode]);
        LDataSet.SetParams(['SubArea'], [FAppModules.StudyArea.SubAreaCode]);
        LDataSet.SetParams(['Scenario'], [FAppModules.StudyArea.ScenarioCode]);
        LDataSet.SetParams(['Identifier'], [IntToStr(LIdentifier)]);
        LDataSet.SetParams(['MatrixType'], [IntToStr(Ord(mtMatrixB))]);

        for LCount := MinMatrix to MaxMatrix do
        begin
          LFieldName := Format('%s%2.2d',['Matrix',LCount]);

          if LMatrixLine.MatrixLine[LCount].FInitalised then
           LDataSet.SetParams([LFieldName], [FloatToStr(LMatrixLine.MatrixLine[LCount].FData)]);
        end;
        LDataSet.ExecSQL;
      end;

      LIdentifier := 0;
      for LLinesCount := 0 to LParamObject.MatrixB0.ItemsCount-1 do
      begin
        LMatrixLine := LParamObject.MatrixB0.MatrixLineByIndex[LLinesCount];
        if not Assigned(LMatrixLine) then
          Exit;

        if((LIdentifier mod 50) = 0) then
        begin
          AProgressFunction('',ptNone,LStop);
          if LStop then Exit;
        end;

        LIdentifier := LIdentifier + 1;
        LDataSet.DataSet.Close;
        LDataSet.SetSQL(WriteParamMatrixDataSQL);
        LDataSet.ClearQueryParams();
        LDataSet.SetParams(['Model'], [FAppModules.StudyArea.ModelCode]);
        LDataSet.SetParams(['StudyAreaName'], [FAppModules.StudyArea.StudyAreaCode]);
        LDataSet.SetParams(['SubArea'], [FAppModules.StudyArea.SubAreaCode]);
        LDataSet.SetParams(['Scenario'], [FAppModules.StudyArea.ScenarioCode]);
        LDataSet.SetParams(['Identifier'], [IntToStr(LIdentifier)]);
        LDataSet.SetParams(['MatrixType'], [IntToStr(Ord(mtMatrixB0))]);

        for LCount := MinMatrix to MaxMatrix do
        begin
          LFieldName := Format('%s%2.2d',['Matrix',LCount]);

          if LMatrixLine.MatrixLine[LCount].FInitalised then
           LDataSet.SetParams([LFieldName], [FloatToStr(LMatrixLine.MatrixLine[LCount].FData)]);
        end;
        LDataSet.ExecSQL;
      end;

      LIdentifier := 0;
      for LLinesCount := 0 to LParamObject.MatrixB1.ItemsCount-1 do
      begin
        LMatrixLine := LParamObject.MatrixB1.MatrixLineByIndex[LLinesCount];
        if not Assigned(LMatrixLine) then
          Exit;

        if((LIdentifier mod 50) = 0) then
        begin
          AProgressFunction('',ptNone,LStop);
          if LStop then Exit;
        end;

        LIdentifier := LIdentifier + 1;
        LDataSet.DataSet.Close;
        LDataSet.SetSQL(WriteParamMatrixDataSQL);
        LDataSet.ClearQueryParams();
        LDataSet.SetParams(['Model'], [FAppModules.StudyArea.ModelCode]);
        LDataSet.SetParams(['StudyAreaName'], [FAppModules.StudyArea.StudyAreaCode]);
        LDataSet.SetParams(['SubArea'], [FAppModules.StudyArea.SubAreaCode]);
        LDataSet.SetParams(['Scenario'], [FAppModules.StudyArea.ScenarioCode]);
        LDataSet.SetParams(['Identifier'], [IntToStr(LIdentifier)]);
        LDataSet.SetParams(['MatrixType'], [IntToStr(Ord(mtMatrixB1))]);

        for LCount := MinMatrix to MaxMatrix do
        begin
          LFieldName := Format('%s%2.2d',['Matrix',LCount]);

          if LMatrixLine.MatrixLine[LCount].FInitalised then
           LDataSet.SetParams([LFieldName], [FloatToStr(LMatrixLine.MatrixLine[LCount].FData)]);
        end;
        LDataSet.ExecSQL;
      end;

      LIdentifier := 0;
      for LLinesCount := 0 to LParamObject.MatrixA.ItemsCount-1 do
      begin
        LMatrixLine := LParamObject.MatrixA.MatrixLineByIndex[LLinesCount];
        if not Assigned(LMatrixLine) then
          Exit;

        if((LIdentifier mod 50) = 0) then
        begin
          AProgressFunction('',ptNone,LStop);
          if LStop then Exit;
        end;

        LIdentifier := LIdentifier + 1;
        LDataSet.DataSet.Close;
        LDataSet.SetSQL(WriteParamMatrixDataSQL);
        LDataSet.ClearQueryParams();
        LDataSet.SetParams(['Model'], [FAppModules.StudyArea.ModelCode]);
        LDataSet.SetParams(['StudyAreaName'], [FAppModules.StudyArea.StudyAreaCode]);
        LDataSet.SetParams(['SubArea'], [FAppModules.StudyArea.SubAreaCode]);
        LDataSet.SetParams(['Scenario'], [FAppModules.StudyArea.ScenarioCode]);
        LDataSet.SetParams(['Identifier'], [IntToStr(LIdentifier)]);
        LDataSet.SetParams(['MatrixType'], [IntToStr(Ord(mtMatrixA))]);

        for LCount := MinMatrix to MaxMatrix do
        begin
          LFieldName := Format('%s%2.2d',['Matrix',LCount]);

          if LMatrixLine.MatrixLine[LCount].FInitalised then
           LDataSet.SetParams([LFieldName], [FloatToStr(LMatrixLine.MatrixLine[LCount].FData)]);
        end;
        LDataSet.ExecSQL;
      end;

      LIdentifier := 0;
      for LLinesCount := 0 to LParamObject.MatrixC.ItemsCount-1 do
      begin
        LMatrixLine := LParamObject.MatrixC.MatrixLineByIndex[LLinesCount];
        if not Assigned(LMatrixLine) then
          Exit;

        if((LIdentifier mod 50) = 0) then
        begin
          AProgressFunction('',ptNone,LStop);
          if LStop then Exit;
        end;

        LIdentifier := LIdentifier + 1;
        LDataSet.DataSet.Close;
        LDataSet.SetSQL(WriteParamMatrixDataSQL);
        LDataSet.ClearQueryParams();
        LDataSet.SetParams(['Model'], [FAppModules.StudyArea.ModelCode]);
        LDataSet.SetParams(['StudyAreaName'], [FAppModules.StudyArea.StudyAreaCode]);
        LDataSet.SetParams(['SubArea'], [FAppModules.StudyArea.SubAreaCode]);
        LDataSet.SetParams(['Scenario'], [FAppModules.StudyArea.ScenarioCode]);
        LDataSet.SetParams(['Identifier'], [IntToStr(LIdentifier)]);
        LDataSet.SetParams(['MatrixType'], [IntToStr(Ord(mtMatrixC))]);

        for LCount := MinMatrix to MaxMatrix do
        begin
          LFieldName := Format('%s%2.2d',['Matrix',LCount]);

          if LMatrixLine.MatrixLine[LCount].FInitalised then
           LDataSet.SetParams([LFieldName], [FloatToStr(LMatrixLine.MatrixLine[LCount].FData)]);
        end;
        LDataSet.ExecSQL;
      end;
      LDataSet.DataSet.Close;

      Result := InsertFileName(AFileName);

      if Result then
      begin
        LDataSet.DataSet.Close;
        LDataSet.SetSQL(ReadParamFileNameSQL);
        LDataSet.SetReadOnly(False);
        LDataSet.DataSet.Open;
        while not LDataSet.DataSet.Eof do
        begin
          LDataSet.DataSet.Edit;
          LDataSet.DataSet.FieldByName('ParamFile').AsString := ExtractFileName(AFileName.FileName);
          LDataSet.DataSet.Post;
          LDataSet.DataSet.Next;
        end;
      end;

      if Result then
      begin
        LMessage := FAppModules.Language.GetString('TFileParamDatabaseAgent.strWriteEnded');
        AProgressFunction(LMessage,ptNone,LStop);
      end;
    finally
      LDataSet.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFileParamDatabaseAgent.ClearModelDataInDatabase(AFileName: TFileNameObject; AProgressFunction: TProgressUpdateFuntion;
         AQuetly: boolean = False): boolean;
const OPNAME = 'TFileParamDatabaseAgent.ClearModelDataInDatabase';
var
  LTableNames,
  LMessage: string;
  LStop: boolean;
  LDataSet : TAbstractModelDataset;
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

    LTableNames := 'ParamHeader,ParamMatrix,ParamMatrixComm,ParamStochastics';
    Result := DeleteModelData(LTableNames,'',AProgressFunction,AQuetly);
    Result := Result and DeleteFileName(AFileName);

    if Result then
    begin
      FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
      try
        LDataSet.SetSQL(ReadParamFileNameSQL);
        LDataSet.SetReadOnly(False);
        LDataSet.DataSet.Open;
        while not LDataSet.DataSet.Eof do
        begin
          LDataSet.DataSet.Edit;
          LDataSet.DataSet.FieldByName('ParamFile').Clear;
          LDataSet.DataSet.Post;
          LDataSet.DataSet.Next;
        end;
      finally
        LDataSet.Free;
      end;
    end;

    if Result and (not AQuetly)then
    begin
      LMessage := FAppModules.Language.GetString('TAbstractDatabaseAgent.strClearModelDataInDatabaseEnded');
      AProgressFunction(LMessage,ptNone,LStop);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
