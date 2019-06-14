//
//
//  UNIT      : Contains TOutputComparisonLoadAgent Class
//  AUTHOR    :  (Aravia)
//  DATE      : 
//  COPYRIGHT : Copyright © 2005 DWAF
//
//
unit UOutputComparisonLoadAgent;

interface

uses
  Classes,
  UOutputData,
  UOutputComparisonData,
  UAbstractObject,
  UYieldModelDataObject;
type
  TOutputComparisonLoadAgent = class(TAbstractAppObject)
  protected
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    function ConstructSummaryOutputData(AOutputData: TOutputData;AFileName : string;AYieldModelData : TYieldModelDataObject): boolean;
  public
    function ConstructData(AYieldModelData:TYieldModelDataObject): boolean;
  end;

implementation

uses
  SysUtils,
  UFileNames,
  Uconstants,
  UPlotFileManager,
  UDataSetType,
  UUtilities,
  VoaimsCom_TLB,
  USumOutFileManager,
  USumOutDataObjects,
  UAbstractFileNamesObject,
  UErrorHandlingOperations, DB;

procedure TOutputComparisonLoadAgent.CreateMemberObjects;
const OPNAME = 'TOutputComparisonLoadAgent.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TOutputComparisonLoadAgent.DestroyMemberObjects;
const OPNAME = 'TOutputComparisonLoadAgent.DestroyMemberObjects';
begin
  try
    inherited DestroyMemberObjects;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TOutputComparisonLoadAgent.ConstructData(AYieldModelData:TYieldModelDataObject): boolean;
const OPNAME = 'TOutputComparisonLoadAgent.ConstructData';
var
  LIndex : Integer;
  LOutputComparisonData : TOutputComparisonData;
  LSelectedFiles : TStringList;
  LSelectedFileStr,
  LModelCode, LStudyAreaCode,
  LSubAreaCode,LScenarioCode : string;
begin
  Result := False;
  try
    if AYieldModelData <> nil then
    begin
      if AYieldModelData.OutputComparisonData = nil then
        Exit;
      LSelectedFiles := TStringList.Create;
      try
        AYieldModelData.OutputComparisonData.Initialise;
        LSelectedFileStr := FAppModules.ViewIni.ReadString('TOutputComparisonFileSelectionValidator','SelectedFirstFile'+FAppModules.StudyArea.ScenarioCode,'');
        if Trim(LSelectedFileStr) <> '' then
          LSelectedFiles.Add(LSelectedFileStr);
        LSelectedFileStr := FAppModules.ViewIni.ReadString('TOutputComparisonFileSelectionValidator','SelectedSecondFile'+FAppModules.StudyArea.ScenarioCode,'');
        if Trim(LSelectedFileStr) <> '' then
          LSelectedFiles.Add(LSelectedFileStr);
        for LIndex := 0 to LSelectedFiles.Count-1 do
        begin
          LModelCode := FAppModules.ViewIni.ReadString('TOutputComparisonFileSelectionValidator','Model'+
                        FAppModules.StudyArea.ModelCode,'');
          LStudyAreaCode := FAppModules.ViewIni.ReadString('TOutputComparisonFileSelectionValidator','StudyAreaName'+
                            FAppModules.StudyArea.StudyAreaCode,'');
          LSubAreaCode := FAppModules.ViewIni.ReadString('TOutputComparisonFileSelectionValidator','SubArea'+
                          FAppModules.StudyArea.SubAreaCode,'');
          LScenarioCode := FAppModules.ViewIni.ReadString('TOutputComparisonFileSelectionValidator','Scenario'+
                           FAppModules.StudyArea.ScenarioCode,'');
          if (LModelCode = FAppModules.StudyArea.ModelCode) and
            (LStudyAreaCode = FAppModules.StudyArea.StudyAreaCode) and
            (LSubAreaCode = FAppModules.StudyArea.SubAreaCode) and
            (LScenarioCode = FAppModules.StudyArea.ScenarioCode) and
            (SysUtils.FileExists(LSelectedFiles[LIndex])) then
          begin
            LOutputComparisonData := AYieldModelData.OutputComparisonData.AddOutputComparisonDataFromFile(LSelectedFiles[LIndex]);
            if LOutputComparisonData <> nil then
            begin
              LOutputComparisonData.Initialise;
              ConstructSummaryOutputData(LOutputComparisonData.OutputData ,LOutputComparisonData.OutputFileName,AYieldModelData);
              if LIndex = 0 then
              begin
                LOutputComparisonData.ReservoirList := FAppModules.ViewIni.ReadString('TOutputComparisonFileSelectionValidator','SelectedReservoirs1'+FAppModules.StudyArea.ScenarioCode,'');
                LOutputComparisonData.ChannelList := FAppModules.ViewIni.ReadString('TOutputComparisonFileSelectionValidator','SelectedChannel1'+FAppModules.StudyArea.ScenarioCode,'');
              end
              else
              if LIndex > 0 then
              begin
                LOutputComparisonData.ReservoirList := FAppModules.ViewIni.ReadString('TOutputComparisonFileSelectionValidator','SelectedReservoirs2'+FAppModules.StudyArea.ScenarioCode,'');
                LOutputComparisonData.ChannelList := FAppModules.ViewIni.ReadString('TOutputComparisonFileSelectionValidator','SelectedChannel2'+FAppModules.StudyArea.ScenarioCode,'');
              end
            end;
          end;
        end;
      finally
        FreeAndNil(LSelectedFiles);
      end;
      AYieldModelData.OutputComparisonData.GenerateReservoirGrandAverage;
      AYieldModelData.OutputComparisonData.GenerateChannelGrandAverage;
      Result := True;
    end;
  except on E: Exception do HandleErrorFunction(E, OPNAME, Result) end;
end;

function TOutputComparisonLoadAgent.ConstructSummaryOutputData(AOutputData: TOutputData;AFileName : string;AYieldModelData : TYieldModelDataObject): boolean;
const OPNAME = 'TOutputComparisonLoadAgent.ConstructSummaryOutputData';
var
  LSumOutFileManager : TSumOutFileManager;
begin
  Result := False;
  try
    if (FileExists(AFileName)) then
    begin
      LSumOutFileManager := TSumOutFileManager.Create(FAppModules);
      try
        LSumOutFileManager.PopulateSumOutComparisonData(AFileName,AOutputData,AYieldModelData);
        Result := True;
      finally
        LSumOutFileManager.Free;
      end;
    end;
  except on E: Exception do HandleErrorFunction(E, OPNAME, Result) end;
end;

end.
