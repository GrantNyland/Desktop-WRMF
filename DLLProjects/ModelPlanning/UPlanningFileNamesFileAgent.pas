//
//
//  UNIT      : Contains TPlanningFileNamesFileAgent Class
//  AUTHOR    : Dziedzi Ramulondi (PDNA)
//  DATE      : 16/05/2002
//  COPYRIGHT : Copyright © 2002 DWAF
//
//
unit UPlanningFileNamesFileAgent;

interface

uses
  Classes,Contnrs, sysutils,

  //  DWAF VCL
  UConstants,
  UAbstractObject,
  UYieldFileNamesFileAgent,
  UFileNames,
  UDataFileObjects;

type

  TPlanningFileNamesFileAgent = class(TYieldFileNamesFileAgent)
  protected
    function FindAllocationDefinitionFileNames(ADataFileObjects:TDataFileObjects;AFileNamesContainer: TStrings): boolean;
    function FindChannelSwitchControlFileNames(ADataFileObjects:TDataFileObjects;AFileNamesContainer: TStrings): boolean;
    function FindSaltWashoffFileNames(ADataFileObjects:TDataFileObjects;AFileNamesContainer: TStrings): boolean;
    function FindMineRainfallFileNames(ADataFileObjects:TDataFileObjects;AFileNamesContainer: TStrings): boolean;
    function FindMineFileNames(ADataFileObjects:TDataFileObjects;AFileNamesContainer: TStrings): boolean;

  public
    { Public declarations }
    function ReadOutputFileNames(ADataFileObjects: TDataFileObjects;AFileNamesObject: TModelFileNames;
             AProgressFunction: TProgressUpdateFuntion): boolean; override;

    function ReadAllocationDefinitionFileNames(ADataFileObjects: TDataFileObjects;AFileNamesObject: TModelFileNames;
             AProgressFunction: TProgressUpdateFuntion): boolean; virtual;

    function ReadReservoirImplementationFileName(ADataFileObjects: TDataFileObjects;AFileNamesObject: TModelFileNames;
             AProgressFunction: TProgressUpdateFuntion): boolean; virtual;

    function ReadDisbenefitDefinitionFileName(ADataFileObjects: TDataFileObjects;AFileNamesObject: TModelFileNames;
             AProgressFunction: TProgressUpdateFuntion): boolean; virtual;

    function ReadGrowthFactorsFileName(ADataFileObjects: TDataFileObjects;AFileNamesObject: TModelFileNames;
             AProgressFunction: TProgressUpdateFuntion): boolean; virtual;

     function ReadMonthlyWaterRequirementFileName(ADataFileObjects: TDataFileObjects;AFileNamesObject: TModelFileNames;
             AProgressFunction: TProgressUpdateFuntion): boolean; virtual;

    function ReadHydropowerAllocationFileName(ADataFileObjects: TDataFileObjects;AFileNamesObject: TModelFileNames;
             AProgressFunction: TProgressUpdateFuntion): boolean; virtual;

    function ReadPumpingChannelControlFileName(ADataFileObjects: TDataFileObjects;AFileNamesObject: TModelFileNames;
             AProgressFunction: TProgressUpdateFuntion): boolean; virtual;

    function ReadGeneralChannelControlFileName(ADataFileObjects: TDataFileObjects;AFileNamesObject: TModelFileNames;
             AProgressFunction: TProgressUpdateFuntion): boolean; virtual;

    function ReadReclamationPlantControlFileName(ADataFileObjects: TDataFileObjects;AFileNamesObject: TModelFileNames;
             AProgressFunction: TProgressUpdateFuntion): boolean; virtual;

    function ReadReturnFlowChannelFileName(ADataFileObjects: TDataFileObjects;AFileNamesObject: TModelFileNames;
             AProgressFunction: TProgressUpdateFuntion): boolean; virtual;

    function ReadChannelSwitchControlFileNames(ADataFileObjects: TDataFileObjects;AFileNamesObject: TModelFileNames;
             AProgressFunction: TProgressUpdateFuntion): boolean; virtual;

    function ReadTariffCalculationFileName(ADataFileObjects: TDataFileObjects;AFileNamesObject: TModelFileNames;
             AProgressFunction: TProgressUpdateFuntion): boolean; virtual;

    function ReadAllocationChannelFileName(ADataFileObjects: TDataFileObjects;AFileNamesObject: TModelFileNames;
             AProgressFunction: TProgressUpdateFuntion): boolean; virtual;

    function ReadReleaseStructureFileName(ADataFileObjects: TDataFileObjects;AFileNamesObject: TModelFileNames;
             AProgressFunction: TProgressUpdateFuntion): boolean; virtual;

    function ReadCurtailFileName(ADataFileObjects: TDataFileObjects;AFileNamesObject: TModelFileNames;
             AProgressFunction: TProgressUpdateFuntion): boolean; virtual;


   function ReadMineFileNames(ADataFileObjects: TDataFileObjects;AFileNamesObject: TModelFileNames;
             AProgressFunction: TProgressUpdateFuntion): boolean; virtual;

   function ReadSaltsWashoffFileNames(ADataFileObjects: TDataFileObjects;AFileNamesObject: TModelFileNames;
            AProgressFunction: TProgressUpdateFuntion): boolean; virtual;

   function ReadMineRainfallFileNames(ADataFileObjects: TDataFileObjects;AFileNamesObject: TModelFileNames;
             AProgressFunction: TProgressUpdateFuntion): boolean; virtual;

  end;


implementation

uses UUtilities,
     UFile01Agent,
     UFile02Agent,
     UFile03Agent,
     UFilePathsAgent,
     UFileParamAgent,
     UParamObject,
     UYieldModelDataObject,
     URunParametersObject,
     UChannelDescriptionObject,
     UAbstractFileNamesObject,
     UPlanningFileDataObjects,
     UErrorHandlingOperations;


{ TPlanningFileNamesFileAgent }

function TPlanningFileNamesFileAgent.FindAllocationDefinitionFileNames(ADataFileObjects: TDataFileObjects;AFileNamesContainer: TStrings): boolean;
const OPNAME = 'TPlanningFileNamesFileAgent.FindAllocationDefinitionFileNames';
var
  LFileName: string;
begin
  Result := False;
  try
    if Assigned(ADataFileObjects)  and Assigned(AFileNamesContainer)then
    begin
      AFileNamesContainer.Clear;
      if Assigned(ADataFileObjects.FPathsObject) and
         DirectoryExists(Trim(ADataFileObjects.FPathsObject.InputFilesPath.FData)) then
      begin
          LFileName := IncludeTrailingPathDelimiter(Trim(ADataFileObjects.FPathsObject.InputFilesPath.FData)) +
                       Trim(ADataFileObjects.FPathsObject.FileNamePrefix.FData) + 'FM*.dat';
          AFileNamesContainer.Clear;
          UUtilities.SearchFiles(LFileName,AFileNamesContainer);
      end;
      Result := True;
    end;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TPlanningFileNamesFileAgent.FindSaltWashoffFileNames(ADataFileObjects: TDataFileObjects;
         AFileNamesContainer: TStrings): boolean;
const OPNAME = 'TPlanningFileNamesFileAgent.FindChannelSwitchControlFileNames';
var
  LFileName: string;
begin
  Result := False;
  try
    if Assigned(ADataFileObjects)  and Assigned(AFileNamesContainer)then
    begin
      AFileNamesContainer.Clear;
      if Assigned(ADataFileObjects.FPathsObject) and
         DirectoryExists(Trim(ADataFileObjects.FPathsObject.InputFilesPath.FData)) then
      begin
          LFileName := IncludeTrailingPathDelimiter(Trim(ADataFileObjects.FPathsObject.InputFilesPath.FData+'\WQT\')) +
                        'MISW*.dat';
          AFileNamesContainer.Clear;
          UUtilities.SearchFiles(LFileName,AFileNamesContainer);
      end;
      Result := True;
    end;

  except on E: Exception do HandleError(E, OPNAME) end;
end;


function TPlanningFileNamesFileAgent.FindMineFileNames(ADataFileObjects: TDataFileObjects;
         AFileNamesContainer: TStrings): boolean;
const OPNAME = 'TPlanningFileNamesFileAgent.FindChannelSwitchControlFileNames';
var
  LFileName: string;
begin
  Result := False;
  try
    if Assigned(ADataFileObjects)  and Assigned(AFileNamesContainer)then
    begin
      AFileNamesContainer.Clear;
      if Assigned(ADataFileObjects.FPathsObject) and
         DirectoryExists(Trim(ADataFileObjects.FPathsObject.InputFilesPath.FData+'\WQT\')) then
      begin
          LFileName := IncludeTrailingPathDelimiter(Trim(ADataFileObjects.FPathsObject.InputFilesPath.FData+'\WQT\')) +
                        'MIMM*.dat';
          AFileNamesContainer.Clear;
          UUtilities.SearchFiles(LFileName,AFileNamesContainer);
      end;
      Result := True;
    end;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TPlanningFileNamesFileAgent.FindMineRainfallFileNames(ADataFileObjects: TDataFileObjects;
         AFileNamesContainer: TStrings): boolean;
const OPNAME = 'TPlanningFileNamesFileAgent.FindChannelSwitchControlFileNames';
var
  LFileName: string;
begin
  Result := False;
  try
    if Assigned(ADataFileObjects)  and Assigned(AFileNamesContainer)then
    begin
      AFileNamesContainer.Clear;
      if Assigned(ADataFileObjects.FPathsObject) and
         DirectoryExists(Trim(ADataFileObjects.FPathsObject.InputFilesPath.FData)) then
      begin
          LFileName := IncludeTrailingPathDelimiter(Trim(ADataFileObjects.FPathsObject.InputFilesPath.FData+'\WQT\')) +
                        '*.RAN';
          AFileNamesContainer.Clear;
          UUtilities.SearchFiles(LFileName,AFileNamesContainer);
      end;
      Result := True;
    end;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TPlanningFileNamesFileAgent.FindChannelSwitchControlFileNames(ADataFileObjects: TDataFileObjects;
         AFileNamesContainer: TStrings): boolean;
const OPNAME = 'TPlanningFileNamesFileAgent.FindChannelSwitchControlFileNames';
var
  LFileName: string;
begin
  Result := False;
  try
    if Assigned(ADataFileObjects)  and Assigned(AFileNamesContainer)then
    begin
      AFileNamesContainer.Clear;
      if Assigned(ADataFileObjects.FPathsObject) and
         DirectoryExists(Trim(ADataFileObjects.FPathsObject.InputFilesPath.FData)) then
      begin
          LFileName := IncludeTrailingPathDelimiter(Trim(ADataFileObjects.FPathsObject.InputFilesPath.FData)) +
                       Trim(ADataFileObjects.FPathsObject.FileNamePrefix.FData) + 'SW*.dat';
          AFileNamesContainer.Clear;
          UUtilities.SearchFiles(LFileName,AFileNamesContainer);
      end;
      Result := True;
    end;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TPlanningFileNamesFileAgent.ReadAllocationDefinitionFileNames(ADataFileObjects: TDataFileObjects;
         AFileNamesObject: TModelFileNames;AProgressFunction: TProgressUpdateFuntion): boolean;
const OPNAME = 'TPlanningFileNamesFileAgent.ReadAllocationDefinitionFileNames';
var
  LFileList: TStringList;
  LCount,
  LNewFileIndex : integer;
begin
  Result := False;
  try
    if not Assigned(ADataFileObjects) then
      raise Exception.Create('File data object parameter is not yet assigned.');

    if not Assigned(AFileNamesObject) then
      raise Exception.Create('File object parameter is not yet assigned.');

    LFileList := TStringList.Create;
    try
      if FindAllocationDefinitionFileNames(ADataFileObjects,LFileList) then
      begin
        for LCount := 0 to LFileList.Count -1 do
        begin
          LNewFileIndex := AFileNamesObject.CastAllocationDefinitionFileNames.HighestFileNumber + 1;
          AFileNamesObject.AddAllocationDefinitionFileName(LNewFileIndex,LFileList[LCount],False,0.0,
          FileLastWriteDate(LFileList[LCount]));
        end;
      end;
    finally
      LFileList.Free;
    end;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TPlanningFileNamesFileAgent.ReadChannelSwitchControlFileNames(ADataFileObjects: TDataFileObjects; AFileNamesObject: TModelFileNames;
         AProgressFunction: TProgressUpdateFuntion): boolean;
const OPNAME = 'TPlanningFileNamesFileAgent.ReadChannelSwitchControlFileNames';
var
  LFileList: TStringList;
  LCount,
  LNewFileIndex : integer;
begin
  Result := False;
  try
    if not Assigned(ADataFileObjects) then
      raise Exception.Create('File data object parameter is not yet assigned.');

    if not Assigned(AFileNamesObject) then
      raise Exception.Create('File object parameter is not yet assigned.');

    LFileList := TStringList.Create;
    try
      if FindChannelSwitchControlFileNames(ADataFileObjects,LFileList) then
      begin
        for LCount := 0 to LFileList.Count -1 do
        begin
          LNewFileIndex := AFileNamesObject.CastChannelSwitchControlFileNames.HighestFileNumber + 1;
          AFileNamesObject.AddChannelSwitchControlFileName(LNewFileIndex,LFileList[LCount],False,0.0,
          FileLastWriteDate(LFileList[LCount]));
        end;
      end;
    finally
      LFileList.Free;
    end;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TPlanningFileNamesFileAgent.ReadSaltsWashoffFileNames(ADataFileObjects: TDataFileObjects; AFileNamesObject: TModelFileNames;
         AProgressFunction: TProgressUpdateFuntion): boolean;
const OPNAME = 'TPlanningFileNamesFileAgent.ReadSaltsWashoffFileNames';
var
  LFileList: TStringList;
  LCount,
  LNewFileIndex : integer;
begin
  Result := False;
  try
    if not Assigned(ADataFileObjects) then
      raise Exception.Create('File data object parameter is not yet assigned.');

    if not Assigned(AFileNamesObject) then
      raise Exception.Create('File object parameter is not yet assigned.');

    LFileList := TStringList.Create;
    try
      if FindSaltWashoffFileNames(ADataFileObjects,LFileList) then
      begin
        for LCount := 0 to LFileList.Count -1 do
        begin
          LNewFileIndex := AFileNamesObject.CastSaltsWashoffFileNames.HighestFileNumber + 1;
          AFileNamesObject.AddSaltsWashoffFileNames(LNewFileIndex,LFileList[LCount],False,0.0,
          FileLastWriteDate(LFileList[LCount]));
        end;
      end;
    finally
      LFileList.Free;
    end;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TPlanningFileNamesFileAgent.ReadMineRainfallFileNames(ADataFileObjects: TDataFileObjects; AFileNamesObject: TModelFileNames;
         AProgressFunction: TProgressUpdateFuntion): boolean;
const OPNAME = 'TPlanningFileNamesFileAgent.ReadSaltsWashoffFileNames';
var
  LFileList: TStringList;
  LCount,
  LNewFileIndex : integer;
begin
  Result := False;
  try
    if not Assigned(ADataFileObjects) then
      raise Exception.Create('File data object parameter is not yet assigned.');

    if not Assigned(AFileNamesObject) then
      raise Exception.Create('File object parameter is not yet assigned.');

    LFileList := TStringList.Create;
    try
      if FindMineRainfallFileNames(ADataFileObjects,LFileList) then
      begin
        for LCount := 0 to LFileList.Count -1 do
        begin
          LNewFileIndex := AFileNamesObject.CastMineRainfallFileNames.HighestFileNumber + 1;
          AFileNamesObject.AddMineRainfallFileNames(LNewFileIndex,LFileList[LCount],False,0.0,
          FileLastWriteDate(LFileList[LCount]));
        end;
      end;
    finally
      LFileList.Free;
    end;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TPlanningFileNamesFileAgent.ReadMineFileNames(ADataFileObjects: TDataFileObjects; AFileNamesObject: TModelFileNames;
         AProgressFunction: TProgressUpdateFuntion): boolean;
const OPNAME = 'TPlanningFileNamesFileAgent.ReadMineFileNames';
var
  LFileList: TStringList;
  LCount,
  LNewFileIndex : integer;

begin
  Result := False;
  try
    if not Assigned(ADataFileObjects) then
      raise Exception.Create('File data object parameter is not yet assigned.');

    if not Assigned(AFileNamesObject) then
      raise Exception.Create('File object parameter is not yet assigned.');

    LFileList := TStringList.Create;
    try
      if FindMineFileNames(ADataFileObjects,LFileList) then
      begin
        for LCount := 0 to LFileList.Count -1 do
        begin
          LNewFileIndex := AFileNamesObject.CastMineFileNames.HighestFileNumber + 1;
          AFileNamesObject.AddMineFileNames(LNewFileIndex,LFileList[LCount],False,0.0,
          FileLastWriteDate(LFileList[LCount]));
        end;
      end;
    finally
      LFileList.Free;
    end;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


function TPlanningFileNamesFileAgent.ReadGeneralChannelControlFileName(ADataFileObjects: TDataFileObjects;
         AFileNamesObject: TModelFileNames;AProgressFunction: TProgressUpdateFuntion): boolean;
const OPNAME = 'TPlanningFileNamesFileAgent.ReadGeneralChannelControlFileName';
var
  LFilename: string;
begin
  Result := False;
  try
    if not Assigned(ADataFileObjects) then
      raise Exception.Create('File data object parameter is not yet assigned.');

    if not Assigned(AFileNamesObject) then
      raise Exception.Create('File object parameter is not yet assigned.');

    LFilename := AFileNamesObject.GeneralChannelControlFileNames.FileNameObject[0].FileName;
    if FileExists(LFilename) then
      AFileNamesObject.UpdateGeneralChannelControlFileName(0,LFilename,False,0.0,FileLastWriteDate(LFilename))
    else
      AFileNamesObject.UpdateGeneralChannelControlFileName(0,LFilename,False,0.0,0.0);

    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TPlanningFileNamesFileAgent.ReadPumpingChannelControlFileName(ADataFileObjects: TDataFileObjects;
         AFileNamesObject: TModelFileNames;AProgressFunction: TProgressUpdateFuntion): boolean;
const OPNAME = 'TPlanningFileNamesFileAgent.ReadPumpingChannelControlFileName';
var
  LFilename: string;
begin
  Result := False;
  try
    if not Assigned(ADataFileObjects) then
      raise Exception.Create('File data object parameter is not yet assigned.');

    if not Assigned(AFileNamesObject) then
      raise Exception.Create('File object parameter is not yet assigned.');

    LFilename := AFileNamesObject.PumpingChannelControlFileNames.FileNameObject[0].FileName;
    if FileExists(LFilename) then
      AFileNamesObject.UpdatePumpingChannelControlFileName(0,LFilename,False,0.0,FileLastWriteDate(LFilename))
    else
      AFileNamesObject.UpdatePumpingChannelControlFileName(0,LFilename,False,0.0,0.0);

    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TPlanningFileNamesFileAgent.ReadReclamationPlantControlFileName(ADataFileObjects: TDataFileObjects;
         AFileNamesObject: TModelFileNames;AProgressFunction: TProgressUpdateFuntion): boolean;
const OPNAME = 'TPlanningFileNamesFileAgent.ReadReclamationPlantControlFileName';
var
  LFilename: string;
begin
  Result := False;
  try
    if not Assigned(ADataFileObjects) then
      raise Exception.Create('File data object parameter is not yet assigned.');

    if not Assigned(AFileNamesObject) then
      raise Exception.Create('File object parameter is not yet assigned.');

    LFilename := AFileNamesObject.ReclamationPlantControlFileNames.FileNameObject[0].FileName;
    if FileExists(LFilename) then
      AFileNamesObject.UpdateReclamationPlantControlFileName(0,LFilename,False,0.0,FileLastWriteDate(LFilename))
    else
      AFileNamesObject.UpdateReclamationPlantControlFileName(0,LFilename,False,0.0,0.0);

    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TPlanningFileNamesFileAgent.ReadReservoirImplementationFileName(ADataFileObjects: TDataFileObjects;
         AFileNamesObject: TModelFileNames;AProgressFunction: TProgressUpdateFuntion): boolean;
const OPNAME = 'TPlanningFileNamesFileAgent.ReadReservoirImplementationFileName';
var
  LFilename: string;
begin
  Result := False;
  try
    if not Assigned(ADataFileObjects) then
      raise Exception.Create('File data object parameter is not yet assigned.');

    if not Assigned(AFileNamesObject) then
      raise Exception.Create('File object parameter is not yet assigned.');

    LFilename := AFileNamesObject.ReservoirImplementationFileNames.FileNameObject[0].FileName;
    if FileExists(LFilename) then
      AFileNamesObject.UpdateReservoirImplementationFileName(0,LFilename,False,0.0,FileLastWriteDate(LFilename))
    else
      AFileNamesObject.UpdateReservoirImplementationFileName(0,LFilename,False,0.0,0.0);

    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TPlanningFileNamesFileAgent.ReadGrowthFactorsFileName(ADataFileObjects: TDataFileObjects;
         AFileNamesObject: TModelFileNames; AProgressFunction: TProgressUpdateFuntion): boolean;
const OPNAME = 'TPlanningFileNamesFileAgent.ReadGrowthFactorsFileName';
var
  LFilename: string;
begin
  Result := False;
  try
    if not Assigned(ADataFileObjects) then
      raise Exception.Create('File data object parameter is not yet assigned.');

    if not Assigned(AFileNamesObject) then
      raise Exception.Create('File object parameter is not yet assigned.');

    LFilename := AFileNamesObject.GrowthFactorsFileNames.FileNameObject[0].FileName;
    if FileExists(LFilename) then
      AFileNamesObject.UpdateGrowthFactorsFileName(0,LFilename,False,0.0,FileLastWriteDate(LFilename))
    else
      AFileNamesObject.UpdateGrowthFactorsFileName(0,LFilename,False,0.0,0.0);

    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


function TPlanningFileNamesFileAgent.ReadAllocationChannelFileName(
  ADataFileObjects: TDataFileObjects; AFileNamesObject: TModelFileNames;
  AProgressFunction: TProgressUpdateFuntion): boolean;
const OPNAME = 'TPlanningFileNamesFileAgent.ReadAllocationChannelFileName';
var
  LFilename: string;
begin
  Result := False;
  try
    if not Assigned(ADataFileObjects) then
      raise Exception.Create('File data object parameter is not yet assigned.');

    if not Assigned(AFileNamesObject) then
      raise Exception.Create('File object parameter is not yet assigned.');

    LFilename := AFileNamesObject.AllocationChannelFileNames.FileNameObject[0].FileName;
    if FileExists(LFilename) then
      AFileNamesObject.UpdateAllocationChannelFileName(0,LFilename,False,0.0,FileLastWriteDate(LFilename))
    else
      AFileNamesObject.UpdateAllocationChannelFileName(0,LFilename,False,0.0,0.0);

    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TPlanningFileNamesFileAgent.ReadDisbenefitDefinitionFileName(
  ADataFileObjects: TDataFileObjects; AFileNamesObject: TModelFileNames;
  AProgressFunction: TProgressUpdateFuntion): boolean;
const OPNAME = 'TPlanningFileNamesFileAgent.ReadDisbenefitDefinitionFileName';
var
  LFilename: string;
begin
  Result := False;
  try
    if not Assigned(ADataFileObjects) then
      raise Exception.Create('File data object parameter is not yet assigned.');

    if not Assigned(AFileNamesObject) then
      raise Exception.Create('File object parameter is not yet assigned.');

    LFilename := AFileNamesObject.DisbenefitDefinitionFileNames.FileNameObject[0].FileName;
    if FileExists(LFilename) then
      AFileNamesObject.UpdateDisbenefitDefinitionFileName(0,LFilename,False,0.0,FileLastWriteDate(LFilename))
    else
      AFileNamesObject.UpdateDisbenefitDefinitionFileName(0,LFilename,False,0.0,0.0);

    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TPlanningFileNamesFileAgent.ReadHydropowerAllocationFileName(
  ADataFileObjects: TDataFileObjects; AFileNamesObject: TModelFileNames;
  AProgressFunction: TProgressUpdateFuntion): boolean;
const OPNAME = 'TPlanningFileNamesFileAgent.ReadHydropowerAllocationFileName';
var
  LFilename: string;
begin
  Result := False;
  try
    if not Assigned(ADataFileObjects) then
      raise Exception.Create('File data object parameter is not yet assigned.');

    if not Assigned(AFileNamesObject) then
      raise Exception.Create('File object parameter is not yet assigned.');

    LFilename := AFileNamesObject.HydropowerAllocationFileNames.FileNameObject[0].FileName;
    if FileExists(LFilename) then
      AFileNamesObject.UpdateHydropowerAllocationFileName(0,LFilename,False,0.0,FileLastWriteDate(LFilename))
    else
      AFileNamesObject.UpdateHydropowerAllocationFileName(0,LFilename,False,0.0,0.0);

    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TPlanningFileNamesFileAgent.ReadMonthlyWaterRequirementFileName(
  ADataFileObjects: TDataFileObjects; AFileNamesObject: TModelFileNames;
  AProgressFunction: TProgressUpdateFuntion): boolean;
const OPNAME = 'TPlanningFileNamesFileAgent.ReadMonthlyWaterRequirementFileName';
var
  LFilename: string;
begin
  Result := False;
  try
    if not Assigned(ADataFileObjects) then
      raise Exception.Create('File data object parameter is not yet assigned.');

    if not Assigned(AFileNamesObject) then
      raise Exception.Create('File object parameter is not yet assigned.');

    LFilename := AFileNamesObject.MonthlyWaterRequirementFileNames.FileNameObject[0].FileName;
    if FileExists(LFilename) then
      AFileNamesObject.UpdateMonthlyWaterRequirementFileName(0,LFilename,False,0.0,FileLastWriteDate(LFilename))
    else
      AFileNamesObject.UpdateMonthlyWaterRequirementFileName(0,LFilename,False,0.0,0.0);

    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TPlanningFileNamesFileAgent.ReadReleaseStructureFileName(ADataFileObjects: TDataFileObjects; AFileNamesObject: TModelFileNames;
         AProgressFunction: TProgressUpdateFuntion): boolean;
const OPNAME = 'TPlanningFileNamesFileAgent.ReadReleaseStructureFileName';
var
  LFilename: string;
begin
  Result := False;
  try
    if not Assigned(ADataFileObjects) then
      raise Exception.Create('File data object parameter is not yet assigned.');

    if not Assigned(AFileNamesObject) then
      raise Exception.Create('File object parameter is not yet assigned.');

    LFilename := AFileNamesObject.ReleaseStructureFileNames.FileNameObject[0].FileName;
    if FileExists(LFilename) then
      AFileNamesObject.UpdateReleaseStructureFileName(0,LFilename,False,0.0,FileLastWriteDate(LFilename))
    else
      AFileNamesObject.UpdateReleaseStructureFileName(0,LFilename,False,0.0,0.0);

    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;

end;


function TPlanningFileNamesFileAgent.ReadCurtailFileName(ADataFileObjects: TDataFileObjects;AFileNamesObject: TModelFileNames;
             AProgressFunction: TProgressUpdateFuntion): boolean;
const OPNAME = 'TPlanningFileNamesFileAgent.ReadCurtailFileName';
var
  LFilename: string;
begin
  Result := False;
  try
    if not Assigned(ADataFileObjects) then
      raise Exception.Create('File data object parameter is not yet assigned.');

    if not Assigned(AFileNamesObject) then
      raise Exception.Create('File object parameter is not yet assigned.');

    LFilename := AFileNamesObject.CurtailFileNames.FileNameObject[0].FileName;
    if FileExists(LFilename) then
      AFileNamesObject.UpdateCurtailFileName(0,LFilename,False,0.0,FileLastWriteDate(LFilename))
    else
      AFileNamesObject.UpdateCurtailFileName(0,LFilename,False,0.0,0.0);

    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;

end;


function TPlanningFileNamesFileAgent.ReadReturnFlowChannelFileName(ADataFileObjects: TDataFileObjects; AFileNamesObject: TModelFileNames;
         AProgressFunction: TProgressUpdateFuntion): boolean;
const OPNAME = 'TPlanningFileNamesFileAgent.ReadReturnFlowChannelFileName';
var
  LFilename: string;
begin
  Result := False;
  try
    if not Assigned(ADataFileObjects) then
      raise Exception.Create('File data object parameter is not yet assigned.');

    if not Assigned(AFileNamesObject) then
      raise Exception.Create('File object parameter is not yet assigned.');

    LFilename := AFileNamesObject.ReturnFlowChannelFileNames.FileNameObject[0].FileName;
    if FileExists(LFilename) then
      AFileNamesObject.UpdateReturnFlowChannelFileName(0,LFilename,False,0.0,FileLastWriteDate(LFilename))
    else
      AFileNamesObject.UpdateReturnFlowChannelFileName(0,LFilename,False,0.0,0.0);

    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;

end;

function TPlanningFileNamesFileAgent.ReadTariffCalculationFileName(ADataFileObjects: TDataFileObjects; AFileNamesObject: TModelFileNames;
         AProgressFunction: TProgressUpdateFuntion): boolean;
const OPNAME = 'TPlanningFileNamesFileAgent.ReadTariffCalculationFileName';
var
  LFilename: string;
begin
  Result := False;
  try
    if not Assigned(ADataFileObjects) then
      raise Exception.Create('File data object parameter is not yet assigned.');

    if not Assigned(AFileNamesObject) then
      raise Exception.Create('File object parameter is not yet assigned.');

    LFilename := AFileNamesObject.TariffCalculationFileNames.FileNameObject[0].FileName;
    if FileExists(LFilename) then
      AFileNamesObject.UpdateTariffCalculationFileName(0,LFilename,False,0.0,FileLastWriteDate(LFilename))
    else
      AFileNamesObject.UpdateTariffCalculationFileName(0,LFilename,False,0.0,0.0);

    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TPlanningFileNamesFileAgent.ReadOutputFileNames(ADataFileObjects: TDataFileObjects;
         AFileNamesObject: TModelFileNames;AProgressFunction: TProgressUpdateFuntion): boolean;
const OPNAME = 'TPlanningFileNamesFileAgent.ReadOutputFileNames';
var
  LFileList       : TStringList;
  LPath           : string;
  LPrefix         : string;
  LFileName       : string;
  LSearchFile     : string;
  LCount          : integer;
begin
  Result := False;
  try
    if not Assigned(ADataFileObjects) then
      raise Exception.Create('File data object parameter is not yet assigned.');

    if not Assigned(AFileNamesObject) then
      raise Exception.Create('File object parameter is not yet assigned.');

    if not ADataFileObjects.FPathsObject.Populated then
      ReadDirectoryFileContents(ADataFileObjects,AFileNamesObject,AProgressFunction);

    AFileNamesObject.CastOutputFileNames.Clear;
    if ADataFileObjects.FPathsObject.Populated then
    begin
      LPath   := Trim(ADataFileObjects.FPathsObject.OutputFilesPath.FData);
      LPath   := IncludeTrailingPathDelimiter(LPath);
      LPrefix := Trim(ADataFileObjects.FPathsObject.FileNamePrefix.FData);
      LFileList := TStringList.Create;
      try
        //INPUT DATA VERIFICATION OUTPUT FILE (DAT.OUT) ...............
        LFileName := LPath + LPrefix + 'DAT.OUT';
        if FileExists(LFilename) then
          AFileNamesObject.AddOutputFileName(LFileName,False,pufgDAT,1,0.0,FileLastWriteDate(LFilename))
        else
          AFileNamesObject.AddOutputFileName(LFileName,False,pufgDAT,1,0.0,0.0);

        //MODEL DEBUG OUTPUT FILE (DBG.OUT) ...........................
        LFileName := LPath + LPrefix + 'DBG.OUT';
        if FileExists(LFilename) then
          AFileNamesObject.AddOutputFileName(LFileName,False,pufgDBG,1,0.0,FileLastWriteDate(LFilename))
        else
          AFileNamesObject.AddOutputFileName(LFileName,False,pufgDBG,1,0.0,0.0);

        //DETAIL OR SUMMARY OUTPUT FILE (SUM.OUT) .....................
        LFileName := LPath + LPrefix + 'SUM.OUT';
        if FileExists(LFilename) then
          AFileNamesObject.AddOutputFileName(LFileName,False,pufgSUM,1,0.0,FileLastWriteDate(LFilename))
        else
          AFileNamesObject.AddOutputFileName(LFileName,False,pufgSUM,1,0.0,0.0);

        //ALLOCATION DECISION SUMMARY OUTPUT FILES (SYS.OUT) .
        LSearchFile := LPath + LPrefix+'SYS*.OUT';
        if(UUtilities.SearchFiles(LSearchFile,LFileList)) then
        begin
          for LCount := 0 to LFileList.Count - 1 do
            AFileNamesObject.AddOutputFileName(LFileList[LCount],False,pufgSYS,LCount+1,0.0,FileLastWriteDate(LFileList[LCount]))
        end;

        //ALLOCATION DECISION SUMMARY OUTPUT FILES (RES.OUT) .
        LSearchFile := LPath + LPrefix+'RES*.OUT';
        if(UUtilities.SearchFiles(LSearchFile,LFileList)) then
        begin
          for LCount := 0 to LFileList.Count - 1 do
            AFileNamesObject.AddOutputFileName(LFileList[LCount],False,pufgRES,LCount+1,0.0,FileLastWriteDate(LFileList[LCount]))
        end;

        //ALLOCATION DECISION DETAIL OUTPUT FILE(PLN.OUT) .............
        LFileName := LPath + LPrefix + 'PLN.OUT';
        if FileExists(LFilename) then
          AFileNamesObject.AddOutputFileName(LFileName,False,pufgPLN,1,0.0,FileLastWriteDate(LFilename))
        else
          AFileNamesObject.AddOutputFileName(LFileName,False,pufgPLN,1,0.0,0.0);

        //ALLOCATION DECISION RELAXATION OUTPUT FILE(RLX.OUT) .........
        LSearchFile := LPath + LPrefix+'RLX*.OUT';
        if(UUtilities.SearchFiles(LSearchFile,LFileList)) then
        begin
          for LCount := 0 to LFileList.Count - 1 do
            AFileNamesObject.AddOutputFileName(LFileList[LCount],False,pufgRLX,LCount+1,0.0,FileLastWriteDate(LFileList[LCount]))
        end;

        //GENERAL MOHTLY OUTPUT FILE (PLT.OUT) ........................
        LFileName := LPath + LPrefix + 'PLT.OUT';
        if FileExists(LFilename) then
          AFileNamesObject.AddOutputFileName(LFileName,False,pufgPLT,1,0.0,FileLastWriteDate(LFilename))
        else
          AFileNamesObject.AddOutputFileName(LFileName,False,pufgPLT,1,0.0,0.0);

        //PUMP CHANNELMONTHLY OUTPUT FILE (PMP.OUT) ...................
        LFileName := LPath + LPrefix + 'PMP.OUT';
        if FileExists(LFilename) then
          AFileNamesObject.AddOutputFileName(LFileName,False,pufgPMP,1,0.0,FileLastWriteDate(LFilename))
        else
          AFileNamesObject.AddOutputFileName(LFileName,False,pufgPMP,1,0.0,0.0);

      finally
        LFileList.Free;
      end;
    end;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
