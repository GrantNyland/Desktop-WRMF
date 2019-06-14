//
//
//  UNIT      : Contains TPlanningFileNamesDatabaseAgent Class
//  AUTHOR    : Dziedzi Ramulondi (PDNA)
//  DATE      : 16/05/2002
//  COPYRIGHT : Copyright © 2002 DWAF
//
//
unit UPlanningFileNamesDatabaseAgent;

interface

uses
  Classes,Contnrs, sysutils,Db,

  //  DWAF VCL
  UConstants,
  UDatabaseUtilities,
  UAbstractObject,
  UFileNames,
  UDataFileObjects,
  UYieldFileNamesDatabaseAgent,
  UYieldModelDataObject;

type

  TPlanningFileNamesDatabaseAgent = class(TYieldFileNamesDatabaseAgent)
  protected
    function ReadFileNamesSQL(AFileGroup: integer): string;
  public
    { Public declarations }
    function ReadAllocationDefinitionFileNames(ADataFileObjects: TDataFileObjects;AFileNamesObject: TModelFileNames): boolean;
    function ReadReservoirImplementationFileName(ADataFileObjects: TDataFileObjects;AFileNamesObject: TModelFileNames): boolean;
    function ReadDisbenefitDefinitionFileName(ADataFileObjects: TDataFileObjects;AFileNamesObject: TModelFileNames): boolean;
    function ReadGrowthFactorsFileName(ADataFileObjects: TDataFileObjects;AFileNamesObject: TModelFileNames): boolean;
    function ReadMonthlyWaterRequirementFileName(ADataFileObjects: TDataFileObjects;AFileNamesObject: TModelFileNames): boolean;
    function ReadHydropowerAllocationFileName(ADataFileObjects: TDataFileObjects;AFileNamesObject: TModelFileNames): boolean;
    function ReadPumpingChannelControlFileName(ADataFileObjects: TDataFileObjects;AFileNamesObject: TModelFileNames): boolean;
    function ReadGeneralChannelControlFileName(ADataFileObjects: TDataFileObjects;AFileNamesObject: TModelFileNames): boolean;
    function ReadReclamationPlantControlFileName(ADataFileObjects: TDataFileObjects;AFileNamesObject: TModelFileNames): boolean;
    function ReadReturnFlowChannelFileName(ADataFileObjects: TDataFileObjects;AFileNamesObject: TModelFileNames): boolean;
    function ReadChannelSwitchControlFileNames(ADataFileObjects: TDataFileObjects;AFileNamesObject: TModelFileNames): boolean;
    function ReadTariffCalculationFileName(ADataFileObjects: TDataFileObjects;AFileNamesObject: TModelFileNames): boolean;
    function ReadAllocationChannelFileName(ADataFileObjects: TDataFileObjects;AFileNamesObject: TModelFileNames): boolean;
    function ReadReleaseStructureFileName(ADataFileObjects: TDataFileObjects;AFileNamesObject: TModelFileNames): boolean;
    function ReadCurtailFileName(ADataFileObjects: TDataFileObjects;AFileNamesObject: TModelFileNames): boolean;

    function ReadSaltsWashoffFileNames(ADataFileObjects: TDataFileObjects;AFileNamesObject: TModelFileNames): boolean;
    function ReadMineFileNames(ADataFileObjects: TDataFileObjects;AFileNamesObject: TModelFileNames): boolean;
    function ReadMineRainfallFileNames(ADataFileObjects: TDataFileObjects;AFileNamesObject: TModelFileNames): boolean;

  end;


implementation

uses UUtilities,
     UFileNameConstants,
     UFilePathsDatabaseAgent,
     UStringDateTimeOperations,
     UDataSetType,
     UErrorHandlingOperations;


function TPlanningFileNamesDatabaseAgent.ReadFileNamesSQL(AFileGroup: integer): string;
const OPNAME = 'TPlanningFileNamesDatabaseAgent.ReadFileNamesSQL';
begin
  Result := '';
  try
    Result := 'SELECT '+
              ' Model,StudyAreaName,SubArea,Scenario,Identifier,FileName,FileGroup,ImportDate,FileDate'+
              ' FROM  FileNames WHERE'+
              ' StudyAreaName =  '+QuotedStr(FAppModules.StudyArea.StudyAreaCode)+
              ' AND SubArea       =  '+QuotedStr(FAppModules.StudyArea.SubAreaCode)+
              ' AND Scenario      =  '+QuotedStr(FAppModules.StudyArea.ScenarioCode)+
              ' AND FileGroup     =  '+ IntToStr(AFileGroup)+
              ' ORDER BY Model,StudyAreaName,SubArea,Scenario,FileGroup,Identifier';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TPlanningFileNamesDatabaseAgent.ReadAllocationDefinitionFileNames(ADataFileObjects: TDataFileObjects;
  AFileNamesObject: TModelFileNames): boolean;
const OPNAME = 'TPlanningFileNamesDatabaseAgent.ReadAllocationDefinitionFileNames';
var
  LDataSet : TAbstractModelDataset;
  LNewFileIndex: integer;
begin
  Result := False;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      LDataSet.SetSQL(ReadFileNamesSQL(fgAllocationDefinition));
      LDataSet.DataSet.Open;
      if not (LDataSet.DataSet.Eof and LDataSet.DataSet.Bof) then
      begin
        while not LDataSet.DataSet.Eof do
        begin
          if (not LDataSet.DataSet.FieldByName('FileName').IsNull) and
             (not LDataSet.DataSet.FieldByName('FileGroup').IsNull) then
          begin
            LNewFileIndex := LDataSet.DataSet.FieldByName('Identifier').AsInteger;
            AFileNamesObject.AddAllocationDefinitionFileName(LNewFileIndex,Trim(LDataSet.DataSet.FieldByName('FileName').AsString),True,
              DateTimeFromStamp(Trim(LDataSet.DataSet.FieldByName('ImportDate').AsString)),
              DateTimeFromStamp(Trim(LDataSet.DataSet.FieldByName('FileDate').AsString)));
          end;
          LDataSet.DataSet.Next;
        end;
      end;
      Result :=  True;
    finally
      LDataSet.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


function TPlanningFileNamesDatabaseAgent.ReadChannelSwitchControlFileNames(ADataFileObjects: TDataFileObjects;
  AFileNamesObject: TModelFileNames): boolean;
const OPNAME = 'TPlanningFileNamesDatabaseAgent.ReadChannelSwitchControlFileNames';
var
  LDataSet : TAbstractModelDataset;
  LNewFileIndex: integer;
begin
  Result := False;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      LDataSet.SetSQL(ReadFileNamesSQL(fgChannelSwitchControl));
      LDataSet.DataSet.Open;
      if not (LDataSet.DataSet.Eof and LDataSet.DataSet.Bof) then
      begin
        while not LDataSet.DataSet.Eof do
        begin
          if (not LDataSet.DataSet.FieldByName('FileName').IsNull) and
             (not LDataSet.DataSet.FieldByName('FileGroup').IsNull) then
          begin
            LNewFileIndex := LDataSet.DataSet.FieldByName('Identifier').AsInteger;
            AFileNamesObject.AddChannelSwitchControlFileName(LNewFileIndex,Trim(LDataSet.DataSet.FieldByName('FileName').AsString),True,
              DateTimeFromStamp(Trim(LDataSet.DataSet.FieldByName('ImportDate').AsString)),
              DateTimeFromStamp(Trim(LDataSet.DataSet.FieldByName('FileDate').AsString)));
          end;
          LDataSet.DataSet.Next;
        end;
      end;
      Result :=  True;
    finally
      LDataSet.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TPlanningFileNamesDatabaseAgent.ReadSaltsWashoffFileNames(ADataFileObjects: TDataFileObjects;
  AFileNamesObject: TModelFileNames): boolean;
const OPNAME = 'TPlanningFileNamesDatabaseAgent.ReadChannelSwitchControlFileNames';
var
  LDataSet : TAbstractModelDataset;
  LNewFileIndex: integer;
begin
  Result := False;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      LDataSet.SetSQL(ReadFileNamesSQL(fgSaltsWashoff));
      LDataSet.DataSet.Open;
      if not (LDataSet.DataSet.Eof and LDataSet.DataSet.Bof) then
      begin
        while not LDataSet.DataSet.Eof do
        begin
          if (not LDataSet.DataSet.FieldByName('FileName').IsNull) and
             (not LDataSet.DataSet.FieldByName('FileGroup').IsNull) then
          begin
            LNewFileIndex := LDataSet.DataSet.FieldByName('Identifier').AsInteger;
            AFileNamesObject.AddSaltsWashoffFileNames(LNewFileIndex,Trim(LDataSet.DataSet.FieldByName('FileName').AsString),True,
              DateTimeFromStamp(Trim(LDataSet.DataSet.FieldByName('ImportDate').AsString)),
              DateTimeFromStamp(Trim(LDataSet.DataSet.FieldByName('FileDate').AsString)));
          end;
          LDataSet.DataSet.Next;
        end;
      end;
      Result :=  (LDataSet.DataSet.RecordCount>0);
    finally
      LDataSet.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TPlanningFileNamesDatabaseAgent.ReadMineRainfallFileNames(ADataFileObjects: TDataFileObjects;
  AFileNamesObject: TModelFileNames): boolean;
const OPNAME = 'TPlanningFileNamesDatabaseAgent.ReadMineRainfallFileNames';
var
  LDataSet : TAbstractModelDataset;
  LNewFileIndex: integer;
begin
  Result := False;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      LDataSet.SetSQL(ReadFileNamesSQL(fgRAN));
      LDataSet.DataSet.Open;
      if not (LDataSet.DataSet.Eof and LDataSet.DataSet.Bof) then
      begin
        while not LDataSet.DataSet.Eof do
        begin
          if (not LDataSet.DataSet.FieldByName('FileName').IsNull) and
             (not LDataSet.DataSet.FieldByName('FileGroup').IsNull) then
          begin
            LNewFileIndex := LDataSet.DataSet.FieldByName('Identifier').AsInteger;
            AFileNamesObject.AddMineRainfallFileNames(LNewFileIndex,Trim(LDataSet.DataSet.FieldByName('FileName').AsString),True,
              DateTimeFromStamp(Trim(LDataSet.DataSet.FieldByName('ImportDate').AsString)),
              DateTimeFromStamp(Trim(LDataSet.DataSet.FieldByName('FileDate').AsString)));
          end;
          LDataSet.DataSet.Next;
        end;
      end;
      Result :=  (LDataSet.DataSet.RecordCount>0);
    finally
      LDataSet.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TPlanningFileNamesDatabaseAgent.ReadMineFileNames(ADataFileObjects: TDataFileObjects;
  AFileNamesObject: TModelFileNames): boolean;
const OPNAME = 'TPlanningFileNamesDatabaseAgent.ReadMineFileNames';
var
  LDataSet : TAbstractModelDataset;
  LNewFileIndex: integer;
begin
  Result := False;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      LDataSet.SetSQL(ReadFileNamesSQL(fgMIMM));
      LDataSet.DataSet.Open;
      if not (LDataSet.DataSet.Eof and LDataSet.DataSet.Bof) then
      begin
        while not LDataSet.DataSet.Eof do
        begin
          if (not LDataSet.DataSet.FieldByName('FileName').IsNull) and
             (not LDataSet.DataSet.FieldByName('FileGroup').IsNull) then
          begin
            LNewFileIndex := LDataSet.DataSet.FieldByName('Identifier').AsInteger;
            AFileNamesObject.AddMineFileNames(LNewFileIndex,Trim(LDataSet.DataSet.FieldByName('FileName').AsString),True,
              DateTimeFromStamp(Trim(LDataSet.DataSet.FieldByName('ImportDate').AsString)),
              DateTimeFromStamp(Trim(LDataSet.DataSet.FieldByName('FileDate').AsString)));
          end;
          LDataSet.DataSet.Next;
        end;
      end;
      Result :=  (LDataSet.DataSet.RecordCount>0);
    finally
      LDataSet.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TPlanningFileNamesDatabaseAgent.ReadGeneralChannelControlFileName(ADataFileObjects: TDataFileObjects;
         AFileNamesObject: TModelFileNames): boolean;
const OPNAME = 'TPlanningFileNamesDatabaseAgent.ReadGeneralChannelControlFileName';
var
  LDataSet : TAbstractModelDataset;
begin
  Result := False;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      LDataSet.SetSQL(ReadFileNamesSQL(fgGeneralChannelControl));
      LDataSet.DataSet.Open;
      if not (LDataSet.DataSet.Eof and LDataSet.DataSet.Bof) then
      begin
        while not LDataSet.DataSet.Eof do
        begin
          if (not LDataSet.DataSet.FieldByName('FileName').IsNull) and
             (not LDataSet.DataSet.FieldByName('FileGroup').IsNull) then
          begin
            AFileNamesObject.UpdateGeneralChannelControlFileName(0,Trim(LDataSet.DataSet.FieldByName('FileName').AsString),True,
              DateTimeFromStamp(Trim(LDataSet.DataSet.FieldByName('ImportDate').AsString)),
              DateTimeFromStamp(Trim(LDataSet.DataSet.FieldByName('FileDate').AsString)));
          end;
          LDataSet.DataSet.Next;
        end;
      end;
      Result :=  True;
    finally
      LDataSet.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TPlanningFileNamesDatabaseAgent.ReadPumpingChannelControlFileName(ADataFileObjects: TDataFileObjects;
         AFileNamesObject: TModelFileNames): boolean;
const OPNAME = 'TPlanningFileNamesDatabaseAgent.ReadPumpingChannelControlFileName';
var
  LDataSet : TAbstractModelDataset;
begin
  Result := False;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      LDataSet.SetSQL(ReadFileNamesSQL(fgPumpingChannelControl));
      LDataSet.DataSet.Open;
      if not (LDataSet.DataSet.Eof and LDataSet.DataSet.Bof) then
      begin
        while not LDataSet.DataSet.Eof do
        begin
          if (not LDataSet.DataSet.FieldByName('FileName').IsNull) and
             (not LDataSet.DataSet.FieldByName('FileGroup').IsNull) then
          begin
            AFileNamesObject.UpdatePumpingChannelControlFileName(0,Trim(LDataSet.DataSet.FieldByName('FileName').AsString),True,
              DateTimeFromStamp(Trim(LDataSet.DataSet.FieldByName('ImportDate').AsString)),
              DateTimeFromStamp(Trim(LDataSet.DataSet.FieldByName('FileDate').AsString)));
          end;
          LDataSet.DataSet.Next;
        end;
      end;
      Result :=  True;
    finally
      LDataSet.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TPlanningFileNamesDatabaseAgent.ReadReclamationPlantControlFileName(ADataFileObjects: TDataFileObjects;
         AFileNamesObject: TModelFileNames): boolean;
const OPNAME = 'TPlanningFileNamesDatabaseAgent.ReadReclamationPlantControlFileName';
var
  LDataSet : TAbstractModelDataset;
begin
  Result := False;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      LDataSet.SetSQL(ReadFileNamesSQL(fgReclamationPlantControl));
      LDataSet.DataSet.Open;
      if not (LDataSet.DataSet.Eof and LDataSet.DataSet.Bof) then
      begin
        while not LDataSet.DataSet.Eof do
        begin
          if (not LDataSet.DataSet.FieldByName('FileName').IsNull) and
             (not LDataSet.DataSet.FieldByName('FileGroup').IsNull) then
          begin
            AFileNamesObject.UpdateReclamationPlantControlFileName(0,Trim(LDataSet.DataSet.FieldByName('FileName').AsString),True,
              DateTimeFromStamp(Trim(LDataSet.DataSet.FieldByName('ImportDate').AsString)),
              DateTimeFromStamp(Trim(LDataSet.DataSet.FieldByName('FileDate').AsString)));
          end;
          LDataSet.DataSet.Next;
        end;
      end;
      Result :=  True;
    finally
      LDataSet.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TPlanningFileNamesDatabaseAgent.ReadReservoirImplementationFileName(ADataFileObjects: TDataFileObjects;
         AFileNamesObject: TModelFileNames): boolean;
const OPNAME = 'TPlanningFileNamesDatabaseAgent.ReadReservoirImplementationFileName';
var
  LDataSet : TAbstractModelDataset;
begin
  Result := False;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      LDataSet.SetSQL(ReadFileNamesSQL(fgReservoirImplementation));
      LDataSet.DataSet.Open;
      if not (LDataSet.DataSet.Eof and LDataSet.DataSet.Bof) then
      begin
        while not LDataSet.DataSet.Eof do
        begin
          if (not LDataSet.DataSet.FieldByName('FileName').IsNull) and
             (not LDataSet.DataSet.FieldByName('FileGroup').IsNull) then
          begin
            AFileNamesObject.UpdateReservoirImplementationFileName(0,Trim(LDataSet.DataSet.FieldByName('FileName').AsString),True,
              DateTimeFromStamp(Trim(LDataSet.DataSet.FieldByName('ImportDate').AsString)),
              DateTimeFromStamp(Trim(LDataSet.DataSet.FieldByName('FileDate').AsString)));
          end;
          LDataSet.DataSet.Next;
        end;
      end;
      Result :=  True;
    finally
      LDataSet.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TPlanningFileNamesDatabaseAgent.ReadGrowthFactorsFileName(ADataFileObjects: TDataFileObjects;
         AFileNamesObject: TModelFileNames): boolean;
const OPNAME = 'TPlanningFileNamesDatabaseAgent.ReadGrowthFactorsFileName';
var
  LDataSet : TAbstractModelDataset;
begin
  Result := False;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      LDataSet.SetSQL(ReadFileNamesSQL(fgGrowthFactors));
      LDataSet.DataSet.Open;
      if not (LDataSet.DataSet.Eof and LDataSet.DataSet.Bof) then
      begin
        while not LDataSet.DataSet.Eof do
        begin
          if (not LDataSet.DataSet.FieldByName('FileName').IsNull) and
             (not LDataSet.DataSet.FieldByName('FileGroup').IsNull) then
          begin
            AFileNamesObject.UpdateGrowthFactorsFileName(0,Trim(LDataSet.DataSet.FieldByName('FileName').AsString),True,
              DateTimeFromStamp(Trim(LDataSet.DataSet.FieldByName('ImportDate').AsString)),
              DateTimeFromStamp(Trim(LDataSet.DataSet.FieldByName('FileDate').AsString)));
          end;
          LDataSet.DataSet.Next;
        end;
      end;
      Result :=  True;
    finally
      LDataSet.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;
//__________________________________________________________________________________________________________________
function TPlanningFileNamesDatabaseAgent.ReadAllocationChannelFileName(ADataFileObjects: TDataFileObjects;
         AFileNamesObject: TModelFileNames): boolean;
const OPNAME = 'TPlanningFileNamesDatabaseAgent.ReadAllocationChannelFileName';
var
  LDataSet : TAbstractModelDataset;
begin
  Result := False;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      LDataSet.SetSQL(ReadFileNamesSQL(fgAllocationChannel));
      LDataSet.DataSet.Open;
      if not (LDataSet.DataSet.Eof and LDataSet.DataSet.Bof) then
      begin
        while not LDataSet.DataSet.Eof do
        begin
          if (not LDataSet.DataSet.FieldByName('FileName').IsNull) and
             (not LDataSet.DataSet.FieldByName('FileGroup').IsNull) then
          begin
            AFileNamesObject.UpdateAllocationChannelFileName(0,Trim(LDataSet.DataSet.FieldByName('FileName').AsString),True,
              DateTimeFromStamp(Trim(LDataSet.DataSet.FieldByName('ImportDate').AsString)),
              DateTimeFromStamp(Trim(LDataSet.DataSet.FieldByName('FileDate').AsString)));
          end;
          LDataSet.DataSet.Next;
        end;
      end;
      Result :=  True;
    finally
      LDataSet.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TPlanningFileNamesDatabaseAgent.ReadDisbenefitDefinitionFileName(ADataFileObjects: TDataFileObjects;
         AFileNamesObject: TModelFileNames): boolean;
const OPNAME = 'TPlanningFileNamesDatabaseAgent.ReadDisbenefitDefinitionFileName';
var
  LDataSet : TAbstractModelDataset;
begin
  Result := False;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      LDataSet.SetSQL(ReadFileNamesSQL(fgDisbenefitDefinition));
      LDataSet.DataSet.Open;
      if not (LDataSet.DataSet.Eof and LDataSet.DataSet.Bof) then
      begin
        while not LDataSet.DataSet.Eof do
        begin
          if (not LDataSet.DataSet.FieldByName('FileName').IsNull) and
             (not LDataSet.DataSet.FieldByName('FileGroup').IsNull) then
          begin
            AFileNamesObject.UpdateDisbenefitDefinitionFileName(0,Trim(LDataSet.DataSet.FieldByName('FileName').AsString),True,
              DateTimeFromStamp(Trim(LDataSet.DataSet.FieldByName('ImportDate').AsString)),
              DateTimeFromStamp(Trim(LDataSet.DataSet.FieldByName('FileDate').AsString)));
          end;
          LDataSet.DataSet.Next;
        end;
      end;
      Result :=  True;
    finally
      LDataSet.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TPlanningFileNamesDatabaseAgent.ReadHydropowerAllocationFileName(ADataFileObjects: TDataFileObjects;
         AFileNamesObject: TModelFileNames): boolean;
const OPNAME = 'TPlanningFileNamesDatabaseAgent.ReadHydropowerAllocationFileName';
var
  LDataSet : TAbstractModelDataset;
begin
  Result := False;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      LDataSet.SetSQL(ReadFileNamesSQL(fgHydropowerAllocation));
      LDataSet.DataSet.Open;
      if not (LDataSet.DataSet.Eof and LDataSet.DataSet.Bof) then
      begin
        while not LDataSet.DataSet.Eof do
        begin
          if (not LDataSet.DataSet.FieldByName('FileName').IsNull) and
             (not LDataSet.DataSet.FieldByName('FileGroup').IsNull) then
          begin
            AFileNamesObject.UpdateHydropowerAllocationFileName(0,Trim(LDataSet.DataSet.FieldByName('FileName').AsString),True,
              DateTimeFromStamp(Trim(LDataSet.DataSet.FieldByName('ImportDate').AsString)),
              DateTimeFromStamp(Trim(LDataSet.DataSet.FieldByName('FileDate').AsString)));
          end;
          LDataSet.DataSet.Next;
        end;
      end;
      Result :=  True;
    finally
      LDataSet.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TPlanningFileNamesDatabaseAgent.ReadMonthlyWaterRequirementFileName(ADataFileObjects: TDataFileObjects;
         AFileNamesObject: TModelFileNames): boolean;
const OPNAME = 'TPlanningFileNamesDatabaseAgent.ReadMonthlyWaterRequirementFileName';
var
  LDataSet : TAbstractModelDataset;
begin
  Result := False;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      LDataSet.SetSQL(ReadFileNamesSQL(fgMonthlyWaterRequirement));
      LDataSet.DataSet.Open;
      if not (LDataSet.DataSet.Eof and LDataSet.DataSet.Bof) then
      begin
        while not LDataSet.DataSet.Eof do
        begin
          if (not LDataSet.DataSet.FieldByName('FileName').IsNull) and
             (not LDataSet.DataSet.FieldByName('FileGroup').IsNull) then
          begin
            AFileNamesObject.UpdateMonthlyWaterRequirementFileName(0,Trim(LDataSet.DataSet.FieldByName('FileName').AsString),True,
              DateTimeFromStamp(Trim(LDataSet.DataSet.FieldByName('ImportDate').AsString)),
              DateTimeFromStamp(Trim(LDataSet.DataSet.FieldByName('FileDate').AsString)));
          end;
          LDataSet.DataSet.Next;
        end;
      end;
      Result :=  True;
    finally
      LDataSet.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TPlanningFileNamesDatabaseAgent.ReadReleaseStructureFileName(ADataFileObjects: TDataFileObjects;
         AFileNamesObject: TModelFileNames): boolean;
const OPNAME = 'TPlanningFileNamesDatabaseAgent.ReadReleaseStructureFileName';
var
  LDataSet : TAbstractModelDataset;
begin
  Result := False;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      LDataSet.SetSQL(ReadFileNamesSQL(fgReleaseStructure));
      LDataSet.DataSet.Open;
      if not (LDataSet.DataSet.Eof and LDataSet.DataSet.Bof) then
      begin
        while not LDataSet.DataSet.Eof do
        begin
          if (not LDataSet.DataSet.FieldByName('FileName').IsNull) and
             (not LDataSet.DataSet.FieldByName('FileGroup').IsNull) then
          begin
            AFileNamesObject.UpdateReleaseStructureFileName(0,Trim(LDataSet.DataSet.FieldByName('FileName').AsString),True,
              DateTimeFromStamp(Trim(LDataSet.DataSet.FieldByName('ImportDate').AsString)),
              DateTimeFromStamp(Trim(LDataSet.DataSet.FieldByName('FileDate').AsString)));
          end;
          LDataSet.DataSet.Next;
        end;
      end;
      Result :=  True;
    finally
      LDataSet.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


function TPlanningFileNamesDatabaseAgent.ReadCurtailFileName(ADataFileObjects: TDataFileObjects;
         AFileNamesObject: TModelFileNames): boolean;
const OPNAME = 'TPlanningFileNamesDatabaseAgent.ReadCurtailFileName';
var
  LDataSet : TAbstractModelDataset;
begin
  Result := False;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      LDataSet.SetSQL(ReadFileNamesSQL(fgCurtail));
      LDataSet.DataSet.Open;
      if not (LDataSet.DataSet.Eof and LDataSet.DataSet.Bof) then
      begin
        while not LDataSet.DataSet.Eof do
        begin
          if (not LDataSet.DataSet.FieldByName('FileName').IsNull) and
             (not LDataSet.DataSet.FieldByName('FileGroup').IsNull) then
          begin
            AFileNamesObject.UpdateCurtailFileName(0,Trim(LDataSet.DataSet.FieldByName('FileName').AsString),True,
              DateTimeFromStamp(Trim(LDataSet.DataSet.FieldByName('ImportDate').AsString)),
              DateTimeFromStamp(Trim(LDataSet.DataSet.FieldByName('FileDate').AsString)));
          end;
          LDataSet.DataSet.Next;
        end;
      end;
      Result :=  True;
    finally
      LDataSet.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TPlanningFileNamesDatabaseAgent.ReadReturnFlowChannelFileName(ADataFileObjects: TDataFileObjects;
         AFileNamesObject: TModelFileNames): boolean;
const OPNAME = 'TPlanningFileNamesDatabaseAgent.ReadReturnFlowChannelFileName';
var
  LDataSet : TAbstractModelDataset;
begin
  Result := False;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      LDataSet.SetSQL(ReadFileNamesSQL(fgReturnFlowChannel));
      LDataSet.DataSet.Open;
      if not (LDataSet.DataSet.Eof and LDataSet.DataSet.Bof) then
      begin
        while not LDataSet.DataSet.Eof do
        begin
          if (not LDataSet.DataSet.FieldByName('FileName').IsNull) and
             (not LDataSet.DataSet.FieldByName('FileGroup').IsNull) then
          begin
            AFileNamesObject.UpdateReturnFlowChannelFileName(0,Trim(LDataSet.DataSet.FieldByName('FileName').AsString),True,
              DateTimeFromStamp(Trim(LDataSet.DataSet.FieldByName('ImportDate').AsString)),
              DateTimeFromStamp(Trim(LDataSet.DataSet.FieldByName('FileDate').AsString)));
          end;
          LDataSet.DataSet.Next;
        end;
      end;
      Result :=  True;
    finally
      LDataSet.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TPlanningFileNamesDatabaseAgent.ReadTariffCalculationFileName(ADataFileObjects: TDataFileObjects;
         AFileNamesObject: TModelFileNames): boolean;
const OPNAME = 'TPlanningFileNamesDatabaseAgent.ReadTariffCalculationFileName';
var
  LDataSet : TAbstractModelDataset;
begin
  Result := False;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      LDataSet.SetSQL(ReadFileNamesSQL(fgTariffCalculation));
      LDataSet.DataSet.Open;
      if not (LDataSet.DataSet.Eof and LDataSet.DataSet.Bof) then
      begin
        while not LDataSet.DataSet.Eof do
        begin
          if (not LDataSet.DataSet.FieldByName('FileName').IsNull) and
             (not LDataSet.DataSet.FieldByName('FileGroup').IsNull) then
          begin
            AFileNamesObject.UpdateTariffCalculationFileName(0,Trim(LDataSet.DataSet.FieldByName('FileName').AsString),True,
              DateTimeFromStamp(Trim(LDataSet.DataSet.FieldByName('ImportDate').AsString)),
              DateTimeFromStamp(Trim(LDataSet.DataSet.FieldByName('FileDate').AsString)));
          end;
          LDataSet.DataSet.Next;
        end;
      end;
      Result :=  True;
    finally
      LDataSet.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
