//
//
//  UNIT      : Contains TFile15DatabaseAgent Class
//  AUTHOR    : Presley Mudau(Cornastone)
//  DATE      : 17/07/2007
//  COPYRIGHT : Copyright © 2007 DWAF
//
//
unit UFile15DatabaseAgent;

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
  UCurtailmentAndDroughtFileObject,
  UAbstractDatabaseAgent,
  UYieldModelDataObject;

type

  TFile15DatabaseAgent = class(TAbstractDatabaseAgent)
  private
  protected
    function ReadCurtailmentDataSQL: string;
    function ReadCurtailmentChannelDataSQL: string;
    function ReadDroughtRestrictionDataSQL: string;
    function ReadDroughtRestrictionFactorsSQL(ADroughtIdentifier: integer): string;
    function ReadDroughtRestrictionStorageVolumesSQL(ADroughtIdentifier: integer): string;
    function ReadF15UnkownDataSQL: string;

    function WriteCurtailmentDataSQL: string;
    function WriteCurtailmentChannelDataSQL: string;
    function WriteDroughtRestrictionDataSQL: string;
    function WriteDroughtRestrictionFactorsSQL: string;
    function WriteDroughtRestrictionStorageVolumesSQL: string;
    function WriteF15UnkownDataSQL: string;
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
     UErrorHandlingOperations;

function TFile15DatabaseAgent.ReadCurtailmentDataSQL: string;
const OPNAME = 'TFile15DatabaseAgent.ReadCurtailmentDataSQL';
begin
  Result := '';
  try
    Result := 'SELECT Model,StudyAreaName,SubArea,Scenario,CurtailmentPeriodCount'+
              ' ,Month01,Month02,Month03,Month04,Month05'+
              ' ,Month06,Month07,Month08,Month09,Month10,InUse'+
             '  FROM Curtailment WHERE'+
              ' (Model            =' + QuotedStr(FAppModules.StudyArea.ModelCode)    +') AND'+
              ' (StudyAreaName    =' + QuotedStr(FAppModules.StudyArea.StudyAreaCode) +') AND'+
              ' (SubArea          =' + QuotedStr(FAppModules.StudyArea.SubAreaCode)   +') AND'+
              ' (Scenario         =' + QuotedStr(FAppModules.StudyArea.ScenarioCode)  +')';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFile15DatabaseAgent.ReadCurtailmentChannelDataSQL: string;
const OPNAME = 'TFile15DatabaseAgent.ReadCurtailmentChannelDataSQL';
begin
  Result := '';
  try
    Result := 'SELECT Model,StudyAreaName,SubArea,Scenario,Identifier,ChannelNumber'+
              ' ,Factor01,Factor02,Factor03,Factor04,Factor05'+
              ' ,Factor06,Factor07,Factor08,Factor09,Factor10'+
              '  FROM CurtailmentChannel WHERE'+
              ' (Model            =' + QuotedStr(FAppModules.StudyArea.ModelCode)    +') AND'+
              ' (StudyAreaName    =' + QuotedStr(FAppModules.StudyArea.StudyAreaCode) +') AND'+
              ' (SubArea          =' + QuotedStr(FAppModules.StudyArea.SubAreaCode)   +') AND'+
              ' (Scenario         =' + QuotedStr(FAppModules.StudyArea.ScenarioCode)  +')';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFile15DatabaseAgent.ReadDroughtRestrictionDataSQL: string;
const OPNAME = 'TFile15DatabaseAgent.ReadDroughtRestrictionDataSQL';
begin
  Result := '';
  try
    Result := 'SELECT Model,StudyAreaName,SubArea,Scenario,Identifier,Name'+
              ' ,ReservoirNumbers,ChannelNumbers'+
              '  FROM DroughtRestriction WHERE'+
              ' (Model            =' + QuotedStr(FAppModules.StudyArea.ModelCode)    +') AND'+
              ' (StudyAreaName    =' + QuotedStr(FAppModules.StudyArea.StudyAreaCode) +') AND'+
              ' (SubArea          =' + QuotedStr(FAppModules.StudyArea.SubAreaCode)   +') AND'+
              ' (Scenario         =' + QuotedStr(FAppModules.StudyArea.ScenarioCode)  +')';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFile15DatabaseAgent.ReadDroughtRestrictionFactorsSQL(ADroughtIdentifier: integer): string;
const OPNAME = 'TFile15DatabaseAgent.ReadDroughtRestrictionFactorsSQL';
begin
  Result := '';
  try
    Result := 'SELECT Model,StudyAreaName,SubArea,Scenario,Identifier'+
              ' ,Factor01,Factor02,Factor03,Factor04,Factor05'+
              ' ,Factor06,Factor07,Factor08,Factor09,Factor10'+
              ' FROM DroughtRestrictionFactors WHERE'+
              ' (Model            =' + QuotedStr(FAppModules.StudyArea.ModelCode)    +')  AND'+
              ' (StudyAreaName    =' + QuotedStr(FAppModules.StudyArea.StudyAreaCode) +') AND'+
              ' (SubArea          =' + QuotedStr(FAppModules.StudyArea.SubAreaCode)   +') AND'+
              ' (Scenario         =' + QuotedStr(FAppModules.StudyArea.ScenarioCode)  +') AND'+
              ' (Identifier       =' + IntToStr(ADroughtIdentifier) + ')';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFile15DatabaseAgent.ReadDroughtRestrictionStorageVolumesSQL(ADroughtIdentifier: integer): string;
const OPNAME = 'TFile15DatabaseAgent.ReadDroughtRestrictionStorageVolumesSQL';
begin
  Result := '';
  try
    Result := 'SELECT Model,StudyAreaName,SubArea,Scenario,Identifier'+
              ' ,Volume01,Volume02,Volume03,Volume04,Volume05'+
              ' ,Volume06,Volume07,Volume08,Volume09,Volume10'+
              ' FROM DroughtRestrictionStorageVolumes WHERE'+
              ' (Model            =' + QuotedStr(FAppModules.StudyArea.ModelCode)    +')  AND'+
              ' (StudyAreaName    =' + QuotedStr(FAppModules.StudyArea.StudyAreaCode) +') AND'+
              ' (SubArea          =' + QuotedStr(FAppModules.StudyArea.SubAreaCode)   +') AND'+
              ' (Scenario         =' + QuotedStr(FAppModules.StudyArea.ScenarioCode)  +') AND'+
              ' (Identifier       =' + IntToStr(ADroughtIdentifier) + ')';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFile15DatabaseAgent.ReadF15UnkownDataSQL: string;
const OPNAME = 'TFile15DatabaseAgent.ReadF15UnkownDataSQL';
begin
  Result := '';
  try
    Result := 'SELECT Model,StudyAreaName,SubArea,Scenario,LineNumber,LineData,FileType'+
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

function TFile15DatabaseAgent.WriteCurtailmentDataSQL: string;
const OPNAME = 'TFile15DatabaseAgent.WriteCurtailmentDataSQL';
begin
  Result := '';
  try
    Result := 'INSERT INTO Curtailment'+
              ' (Model,StudyAreaName,SubArea,Scenario,CurtailmentPeriodCount'+
              ' ,Month01,Month02,Month03,Month04,Month05'+
              ' ,Month06,Month07,Month08,Month09,Month10,InUse)'+
              ' Values'+
              ' (:Model,:StudyAreaName,:SubArea,:Scenario,:CurtailmentPeriodCount'+
              ' ,:Month01,:Month02,:Month03,:Month04,:Month05'+
              ' ,:Month06,:Month07,:Month08,:Month09,:Month10,:InUse)';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFile15DatabaseAgent.WriteCurtailmentChannelDataSQL: string;
const OPNAME = 'TFile15DatabaseAgent.WriteCurtailmentChannelDataSQL';
begin
  Result := '';
  try
    Result := 'INSERT INTO CurtailmentChannel'+
              ' (Model,StudyAreaName,SubArea,Scenario,Identifier,ChannelNumber'+
              ' ,Factor01,Factor02,Factor03,Factor04,Factor05'+
              ' ,Factor06,Factor07,Factor08,Factor09,Factor10)'+
              ' Values'+
              ' (:Model,:StudyAreaName,:SubArea,:Scenario,:Identifier,:ChannelNumber'+
              ' ,:Factor01,:Factor02,:Factor03,:Factor04,:Factor05'+
              ' ,:Factor06,:Factor07,:Factor08,:Factor09,:Factor10)';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFile15DatabaseAgent.WriteDroughtRestrictionDataSQL: string;
const OPNAME = 'TFile15DatabaseAgent.WriteDroughtRestrictionDataSQL';
begin
  Result := '';
  try
    Result := 'INSERT INTO DroughtRestriction'+
              ' (Model,StudyAreaName,SubArea,Scenario,Identifier,Name,ReservoirNumbers,ChannelNumbers)'+
              ' Values'+
              ' (:Model,:StudyAreaName,:SubArea,:Scenario,:Identifier,:Name,:ReservoirNumbers,:ChannelNumbers)';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFile15DatabaseAgent.WriteDroughtRestrictionFactorsSQL: string;
const OPNAME = 'TFile15DatabaseAgent.WriteDroughtRestrictionFactorsSQL';
begin
  Result := '';
  try
    Result := 'INSERT INTO DroughtRestrictionFactors'+
              ' (Model,StudyAreaName,SubArea,Scenario,Identifier'+
              ' ,Factor01,Factor02,Factor03,Factor04,Factor05'+
              ' ,Factor06,Factor07,Factor08,Factor09,Factor10)'+
              ' Values'+
              ' (:Model,:StudyAreaName,:SubArea,:Scenario,:Identifier'+
              ' ,:Factor01,:Factor02,:Factor03,:Factor04,:Factor05'+
              ' ,:Factor06,:Factor07,:Factor08,:Factor09,:Factor10)';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFile15DatabaseAgent.WriteDroughtRestrictionStorageVolumesSQL: string;
const OPNAME = 'TFile15DatabaseAgent.WriteDroughtRestrictionStorageVolumesSQL';
begin
  Result := '';
  try
    Result := 'INSERT INTO DroughtRestrictionStorageVolumes'+
              ' (Model,StudyAreaName,SubArea,Scenario,Identifier'+
              ' ,Volume01,Volume02,Volume03,Volume04,Volume05'+
              ' ,Volume06,Volume07,Volume08,Volume09,Volume10)'+
              ' Values'+
              ' (:Model,:StudyAreaName,:SubArea,:Scenario,:Identifier'+
              ' ,:Volume01,:Volume02,:Volume03,:Volume04,:Volume05'+
              ' ,:Volume06,:Volume07,:Volume08,:Volume09,:Volume10)';
  except on E: Exception do HandleError(E, OPNAME) end;
end;



function TFile15DatabaseAgent.ClearModelDataInDatabase(AFileName: TFileNameObject;
                                                       AProgressFunction: TProgressUpdateFuntion;
                                                       AQuetly: boolean): boolean;
const OPNAME = 'TFile15DatabaseAgent.ClearModelDataInDatabase';
var
  LTableNames : string;
  LMessage    : string;
  LStop       : boolean;
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

    LTableNames := 'Curtailment,CurtailmentChannel,DroughtRestriction,DroughtRestrictionFactors,DroughtRestrictionStorageVolumes';
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

function TFile15DatabaseAgent.ReadModelDataFromDatabase(AFileName: TFileNameObject;
                                                        ADataObject: TDataFileObjects;
                                                        AProgressFunction: TProgressUpdateFuntion): boolean;
const OPNAME = 'TFile15DatabaseAgent.ReadModelDataFromDatabase';
var
  LFieldName             : string;
  LMessage               : string;
  LDataSet               : TAbstractModelDataset;
  LDroughtDataSet        : TAbstractModelDataset;
  LFactorsDataSet        : TAbstractModelDataset;
  LCurtailment           : TCurtailmentFileObject;
  LCurtailmentChannel    : TCurtailedChannelFileObject;
  LDroughtRestriction    : TDroughtRestrictionFileObject;
  LCurtailmentAndDrought : TCurtailmentAndDroughtFileObject;
  LIndex                 : integer;
  LCurtailmentPeriodCount: integer;
  LStop                  : boolean;
  LDroughtIdentifier     : integer;
begin
  Result := False;
  try
    if not Assigned(AProgressFunction) then
      AProgressFunction := DummyShowProgress;

    LMessage := FAppModules.Language.GetString('TFile15DatabaseAgent.strReadStarted');
    AProgressFunction(LMessage,ptNone,LStop);

    if not Assigned(ADataObject) then
      raise Exception.Create('File object parameter is not yet assigned.');

    LCurtailmentAndDrought := ADataObject.FCurtailmentAndDroughtFileObject;
    if not LCurtailmentAndDrought.Initialise then
      Exit;

    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDroughtDataSet);
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LFactorsDataSet);
    try
      LDataSet.SetSQL(ReadCurtailmentDataSQL);
      LDataSet.DataSet.Open;

      //Check if there is any data.
      LCurtailmentPeriodCount := 0;
      if (LDataSet.DataSet.RecordCount = 0) then
      begin
        LMessage := FAppModules.Language.GetString('TFile15DatabaseAgent.strNoDataReturned');
        AProgressFunction(LMessage,ptWarning,LStop);
      end
      else
      begin
        LCurtailment := LCurtailmentAndDrought.AddCurtailment;

        //Read Line 1
        LCurtailmentPeriodCount := LDataSet.DataSet.FieldByName('CurtailmentPeriodCount').AsInteger;
        begin
          LCurtailment.FCurtailmentPeriodCount.FData := LCurtailmentPeriodCount;
          LCurtailment.FCurtailmentPeriodCount.FInitalised := True;
        end;

        //Read Line 2
        if (LCurtailmentPeriodCount > 0) then
        begin
          for LIndex := 1 to LCurtailmentPeriodCount do
          begin
            LFieldName := Format('%s%2.2d',['Month',LIndex]);
            if not LDataSet.DataSet.FieldByName(LFieldName).IsNull then
            begin
              LCurtailment.FStartMonth[LIndex].FData := LDataSet.DataSet.FieldByName(LFieldName).AsInteger;
              LCurtailment.FStartMonth[LIndex].FInitalised := True;
            end;
          end;
        end;
      end;
      LDroughtDataSet.DataSet.Close;

      if (LCurtailmentPeriodCount > 0) then
      begin
        LDroughtDataSet.DataSet.Close;
        LDroughtDataSet.SetSQL(ReadCurtailmentChannelDataSQL);
        LDroughtDataSet.DataSet.Open;
        while not LDroughtDataSet.DataSet.Eof do
        begin
          LCurtailmentChannel := LCurtailmentAndDrought.AddCurtailedChannel;

          if not LDroughtDataSet.DataSet.FieldByName('Identifier').IsNull then
          begin
            LCurtailmentChannel.FIdentifier.FData := LDroughtDataSet.DataSet.FieldByName('Identifier').AsInteger;
            LCurtailmentChannel.FIdentifier.FInitalised := True;
          end;

         //Line 3
          if not LDroughtDataSet.DataSet.FieldByName('ChannelNumber').IsNull then
          begin
            LCurtailmentChannel.FChannelNumber.FData := LDroughtDataSet.DataSet.FieldByName('ChannelNumber').AsInteger;
            LCurtailmentChannel.FChannelNumber.FInitalised := True;
          end;

          if (LCurtailmentPeriodCount > 0) then
          begin
            for LIndex := 1 to LCurtailmentPeriodCount do
            begin
              LFieldName := Format('%s%2.2d',['Factor',LIndex]);
              if not LDroughtDataSet.DataSet.FieldByName(LFieldName).IsNull then
              begin
                LCurtailmentChannel.FAllocationFactors[LIndex].FData := LDroughtDataSet.DataSet.FieldByName(LFieldName).AsFloat;
                LCurtailmentChannel.FAllocationFactors[LIndex].FInitalised := True;
              end;
            end;
          end;
          LDroughtDataSet.DataSet.Next;
        end;
      end;

      LDroughtDataSet.DataSet.Close;
      LDroughtDataSet.SetSQL(ReadDroughtRestrictionDataSQL);
      LDroughtDataSet.DataSet.Open;
      while not LDroughtDataSet.DataSet.Eof do
      begin
        LDroughtRestriction := LCurtailmentAndDrought.AddDroughtRestriction;

        LDroughtIdentifier := LDroughtDataSet.DataSet.FieldByName('Identifier').AsInteger;

        if (LDroughtIdentifier > 0) then
        begin
          LDroughtRestriction.FIdentifier.FData := LDroughtIdentifier;
          LDroughtRestriction.FIdentifier.FInitalised := True;
        end;

        if not LDroughtDataSet.DataSet.FieldByName('Name').IsNull then
        begin
          LDroughtRestriction.FName.FData := Trim(LDroughtDataSet.DataSet.FieldByName('Name').AsString);
          LDroughtRestriction.FName.FInitalised := True;
        end;

        if not LDroughtDataSet.DataSet.FieldByName('ChannelNumbers').IsNull then
        begin
          LDroughtRestriction.FChannelNumbers.FData := Trim(LDroughtDataSet.DataSet.FieldByName('ChannelNumbers').AsString);
          LDroughtRestriction.FChannelNumbers.FInitalised := True;
        end;

        if not LDroughtDataSet.DataSet.FieldByName('ReservoirNumbers').IsNull then
        begin
          LDroughtRestriction.FReservoirNumbers.FData := Trim(LDroughtDataSet.DataSet.FieldByName('ReservoirNumbers').AsString);
          LDroughtRestriction.FReservoirNumbers.FInitalised := True;
        end;

        LFactorsDataSet.DataSet.Close;
        LFactorsDataSet.SetSQL(ReadDroughtRestrictionFactorsSQL(LDroughtIdentifier));
        LFactorsDataSet.DataSet.Open;
        if not (LFactorsDataSet.DataSet.Eof and LFactorsDataSet.DataSet.Bof) then
        begin
          for LIndex := 1 to 10 do
          begin
            LFieldName := Format('%s%2.2d',['Factor',LIndex]);
            if not LFactorsDataSet.DataSet.FieldByName(LFieldName).IsNull then
            begin
              LDroughtRestriction.FAllocationFactors[LIndex].FData := LFactorsDataSet.DataSet.FieldByName(LFieldName).AsFloat;
              LDroughtRestriction.FAllocationFactors[LIndex].FInitalised := True;
            end;
          end;
        end;

        LFactorsDataSet.DataSet.Close;
        LFactorsDataSet.SetSQL(ReadDroughtRestrictionStorageVolumesSQL(LDroughtIdentifier));
        LFactorsDataSet.DataSet.Open;
        if not (LFactorsDataSet.DataSet.Eof and LFactorsDataSet.DataSet.Bof) then
        begin
          for LIndex := 1 to 10 do
          begin
            LFieldName := Format('%s%2.2d',['Volume',LIndex]);
            if not LFactorsDataSet.DataSet.FieldByName(LFieldName).IsNull then
            begin
              LDroughtRestriction.FStorageVolumes[LIndex].FData := LFactorsDataSet.DataSet.FieldByName(LFieldName).AsFloat;
              LDroughtRestriction.FStorageVolumes[LIndex].FInitalised := True;
            end;
          end;
        end;
      LDroughtDataSet.DataSet.Next;
      end;

      //Line 5 on wards+++++++++++++++++++++++++++
      LDataSet.DataSet.Close;
      LDataSet.SetSQL(ReadF15UnkownDataSQL);
      LDataSet.SetParams(['FileType'], [IntToStr(AFileName.FileNumber)]);
      LDataSet.SetParams(['FileGroup'], [IntToStr(AFileName.FileGroup)]);
      LDataSet.DataSet.Open;

      while not LDataSet.DataSet.Eof do
      begin
        LCurtailmentAndDrought.Comment.Add(TrimRight(LDataSet.DataSet.FieldByName('LineData').AsString));
        LDataSet.DataSet.Next;
      end;
      LDataSet.DataSet.Close;

      Result :=  True;
      LMessage := FAppModules.Language.GetString('TFile15DatabaseAgent.strReadEnded');
      AProgressFunction(LMessage,ptNone,LStop);
    finally
      LDataSet.Free;
      LDroughtDataSet.Free;
      LFactorsDataSet.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFile15DatabaseAgent.WriteF15UnkownDataSQL: string;
const OPNAME = 'TFile15DatabaseAgent.WriteF15UnkownDataSQL';
begin
  Result := '';
  try
    Result := 'INSERT INTO WRYMFileLines'+
              ' (Model,StudyAreaName,SubArea,Scenario,FileType,FileGroup,LineNumber,LineSection,LineData)'+
              ' Values(:Model,:StudyAreaName,:SubArea,:Scenario,:FileType,:FileGroup,:LineNumber,:LineSection,:LineData)';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFile15DatabaseAgent.WriteModelDataToDatabase(AFileName: TFileNameObject;
                                                       ADataObject: TDataFileObjects;
                                                       AProgressFunction: TProgressUpdateFuntion): boolean;
const OPNAME = 'TFile15DatabaseAgent.WriteModelDataToDatabase';
var
  LFieldName             : string;
  LMessage               : string;
  LStop                  : boolean;
  LDataSet               : TAbstractModelDataset;
  LDroughtDataSet        : TAbstractModelDataset;
  LFactorsDataSet        : TAbstractModelDataset;
  LIndex                 : integer;
  LCurtailmentIndex      : integer;
  LChannelIndex          : integer;
  LDroughtIndex          : integer;
  LDroughtIdentifier     : integer;
  LChannelIdentifier     : integer;
  LDroughtName           : string;
  LDroughtRestriction    : TDroughtRestrictionFileObject;
  LCurtailment           : TCurtailmentFileObject;
  LCurtailedChannel      : TCurtailedChannelFileObject;
  LCurtailmentAndDrought : TCurtailmentAndDroughtFileObject;
begin
  Result := False;
  try
    if not Assigned(AProgressFunction) then
      AProgressFunction := DummyShowProgress;

    LMessage := FAppModules.Language.GetString('TFile15DatabaseAgent.strWriteStarted');
    AProgressFunction(LMessage,ptNone,LStop);

    if not Assigned(ADataObject) then
      raise Exception.Create('File object parameter is not yet assigned.');

    if not  ClearModelDataInDatabase(AFileName,AProgressFunction,True) then
      Exit;

    LCurtailmentAndDrought := ADataObject.FCurtailmentAndDroughtFileObject;
    if not Assigned(LCurtailmentAndDrought) then
      Exit;

    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDroughtDataSet);
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LFactorsDataSet);
    try
      for LCurtailmentIndex := 0 to LCurtailmentAndDrought.CurtailmentCount-1 do
      begin
        LCurtailment := LCurtailmentAndDrought.CurtailmentByIndex[LCurtailmentIndex];

        LDataSet.DataSet.Close;
        LDataSet.SetSQL(WriteCurtailmentDataSQL);
        LDataSet.ClearQueryParams();
        LDataSet.SetParams(['Model'], [FAppModules.StudyArea.ModelCode]);
        LDataSet.SetParams(['StudyAreaName'], [FAppModules.StudyArea.StudyAreaCode]);
        LDataSet.SetParams(['SubArea'], [FAppModules.StudyArea.SubAreaCode]);
        LDataSet.SetParams(['Scenario'], [FAppModules.StudyArea.ScenarioCode]);
        LDataSet.SetParams(['InUse'], ['1']);

        if LCurtailment.FCurtailmentPeriodCount.FInitalised  then
          LDataSet.SetParams(['CurtailmentPeriodCount'], [IntToStr(LCurtailment.FCurtailmentPeriodCount.FData)]);

        for LIndex := 1 to 10 do
        begin
          if LCurtailment.FStartMonth[LIndex].FInitalised  then
          begin
            LFieldName := Format('%s%2.2d',['Month',LIndex]);
            LDataSet.SetParams([LFieldName], [IntToStr(LCurtailment.FStartMonth[LIndex].FData)]);
          end;
        end;
        LDataSet.ExecSQL;
      end;

      for LChannelIndex := 0 to LCurtailmentAndDrought.CurtailedChannelCount-1 do
      begin
        LCurtailedChannel := LCurtailmentAndDrought.CurtailedChannelByIndex[LChannelIndex];
        LChannelIdentifier := LChannelIndex + 1;

        LDroughtDataSet.DataSet.Close;
        LDroughtDataSet.SetSQL(WriteCurtailmentChannelDataSQL);
        LDroughtDataSet.ClearQueryParams();
        LDroughtDataSet.SetParams(['Model'], [FAppModules.StudyArea.ModelCode]);
        LDroughtDataSet.SetParams(['StudyAreaName'], [FAppModules.StudyArea.StudyAreaCode]);
        LDroughtDataSet.SetParams(['SubArea'], [FAppModules.StudyArea.SubAreaCode]);
        LDroughtDataSet.SetParams(['Scenario'], [FAppModules.StudyArea.ScenarioCode]);
        LDroughtDataSet.SetParams(['Identifier'], [IntToStr(LChannelIdentifier)]);

        if LCurtailedChannel.FChannelNumber.FInitalised then
          LDroughtDataSet.SetParams(['ChannelNumber'], [IntToStr(LCurtailedChannel.FChannelNumber.FData)]);

        for LIndex := 1 to 10 do
        begin
          if LCurtailedChannel.FAllocationFactors[LIndex].FInitalised  then
          begin
            LFieldName := Format('%s%2.2d',['Factor',LIndex]);
            LDroughtDataSet.SetParams([LFieldName], [FloatToStr( LCurtailedChannel.FAllocationFactors[LIndex].FData)]);
          end;
        end;
        LDroughtDataSet.ExecSQL;
      end;


      for LDroughtIndex := 0 to LCurtailmentAndDrought.DroughtRestrictionCount-1 do
      begin
        LDroughtRestriction := LCurtailmentAndDrought.DroughtRestrictionByIndex[LDroughtIndex];
        LDroughtIdentifier := LDroughtIndex + 1;

        LDroughtDataSet.DataSet.Close;
        LDroughtDataSet.SetSQL(WriteDroughtRestrictionDataSQL);
        LDroughtDataSet.ClearQueryParams();
        LDroughtDataSet.SetParams(['Model'], [FAppModules.StudyArea.ModelCode]);
        LDroughtDataSet.SetParams(['StudyAreaName'], [FAppModules.StudyArea.StudyAreaCode]);
        LDroughtDataSet.SetParams(['SubArea'], [FAppModules.StudyArea.SubAreaCode]);
        LDroughtDataSet.SetParams(['Scenario'], [FAppModules.StudyArea.ScenarioCode]);
        LDroughtDataSet.SetParams(['Identifier'], [IntToStr(LDroughtIdentifier)]);

        LDroughtName := 'DROUGHT NAME '+ IntToStr(LDroughtIdentifier);
        LDroughtDataSet.SetParams(['Name'], [LDroughtName]);


        if LDroughtRestriction.FChannelNumbers.FInitalised then
          LDroughtDataSet.SetParams(['ChannelNumbers'], [LDroughtRestriction.FChannelNumbers.FData]);

        if LDroughtRestriction.FReservoirNumbers.FInitalised then
          LDroughtDataSet.SetParams(['ReservoirNumbers'], [LDroughtRestriction.FReservoirNumbers.FData]);

        LDroughtDataSet.ExecSQL;

        LFactorsDataSet.DataSet.Close;
        LFactorsDataSet.SetSQL(WriteDroughtRestrictionFactorsSQL);
        LFactorsDataSet.ClearQueryParams();
        LFactorsDataSet.SetParams(['Model'], [FAppModules.StudyArea.ModelCode]);
        LFactorsDataSet.SetParams(['StudyAreaName'], [FAppModules.StudyArea.StudyAreaCode]);
        LFactorsDataSet.SetParams(['SubArea'], [FAppModules.StudyArea.SubAreaCode]);
        LFactorsDataSet.SetParams(['Scenario'], [FAppModules.StudyArea.ScenarioCode]);
        LFactorsDataSet.SetParams(['Identifier'], [IntToStr(LDroughtIdentifier)]);

        for LIndex := 1 to 10 do
        begin
          if LDroughtRestriction.FAllocationFactors[LIndex].FInitalised  then
          begin
            LFieldName := Format('%s%2.2d',['Factor',LIndex]);
            LFactorsDataSet.SetParams([LFieldName], [FloatToStr( LDroughtRestriction.FAllocationFactors[LIndex].FData)]);
          end;
        end;
        LFactorsDataSet.ExecSQL;

        LFactorsDataSet.DataSet.Close;
        LFactorsDataSet.SetSQL(WriteDroughtRestrictionStorageVolumesSQL);
        LFactorsDataSet.ClearQueryParams();
        LFactorsDataSet.SetParams(['Model'], [FAppModules.StudyArea.ModelCode]);
        LFactorsDataSet.SetParams(['StudyAreaName'], [FAppModules.StudyArea.StudyAreaCode]);
        LFactorsDataSet.SetParams(['SubArea'], [FAppModules.StudyArea.SubAreaCode]);
        LFactorsDataSet.SetParams(['Scenario'], [FAppModules.StudyArea.ScenarioCode]);
        LFactorsDataSet.SetParams(['Identifier'], [IntToStr(LDroughtIdentifier)]);

        for LIndex := 1 to 10 do
        begin
          if LDroughtRestriction.FStorageVolumes[LIndex].FInitalised  then
          begin
            LFieldName := Format('%s%2.2d',['Volume',LIndex]);
            LFactorsDataSet.SetParams([LFieldName], [FloatToStr( LDroughtRestriction.FStorageVolumes[LIndex].FData)]);
          end;
        end;
        LFactorsDataSet.ExecSQL;
      end;

      for LIndex := 0 to LCurtailmentAndDrought.Comment.Count - 1 do
      begin
        LDataSet.DataSet.Close;
        LDataSet.SetSQL(WriteF15UnkownDataSQL);
        LDataSet.ClearQueryParams();
        LDataSet.SetParams(['Model'], [FAppModules.StudyArea.ModelCode]);
        LDataSet.SetParams(['StudyAreaName'], [FAppModules.StudyArea.StudyAreaCode]);
        LDataSet.SetParams(['SubArea'], [FAppModules.StudyArea.SubAreaCode]);
        LDataSet.SetParams(['Scenario'], [FAppModules.StudyArea.ScenarioCode]);
        LDataSet.SetParams(['FileType'], [IntToStr(AFileName.FileNumber)]);
        LDataSet.SetParams(['FileGroup'], [IntToStr(AFileName.FileGroup)]);
        LDataSet.SetParams(['LineNumber'], [IntToStr(1+ LIndex)]);
        LDataSet.SetParams(['LineSection'], [IntToStr(0)]);
        LDataSet.SetParams(['LineData'], [LCurtailmentAndDrought.Comment[LIndex]]);

        LDataSet.ExecSQL;
      end;
      LDataSet.DataSet.Close;

      Result := InsertFileName(AFileName);
      if Result then
      begin
        LMessage := FAppModules.Language.GetString('TFile15DatabaseAgent.strWriteEnded');
        AProgressFunction(LMessage,ptNone,LStop);
      end;
    finally
      LDataSet.Free;
      LDroughtDataSet.Free;
      LFactorsDataSet.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
