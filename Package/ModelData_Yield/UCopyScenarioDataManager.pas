{******************************************************************************}
{*  UNIT      : Contains the class TCopyScenarioDataManager.                  *}
{*  AUTHOR    : Dziedzi Ramulondi                                             *}
{*  DATE      : 2007/08/06                                                    *}
{*  COPYRIGHT : Copyright © 2005 DWAF                                         *}
{******************************************************************************}

unit UCopyScenarioDataManager;

interface

uses
  Classes,
  Contnrs,
  UAbstractObject,
  UProgressDialog,
  UCopyScenarioDataDialog;

type
  TCopyScenarioDataManager = class(TAbstractAppObject)
  protected
    FStudy          : string;
    FSubArea        : string;
    FScenario       : string;
    FSelectedDataContainer : TStringList;
    FExistingDataContainer : TStringList;
    FProgressDialog : TProgressDialog;
    FCopyDataDialog : TCopyScenarioDataDialog;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    function ExecCopyReservoirDataFromScenario(AProgressUpdateFuntion:TProgressUpdateFuntion):boolean;
    function ExecCopyChannelDataFromScenario(AProgressUpdateFuntion:TProgressUpdateFuntion):boolean;

    function ExecCopyIrrigationAreaFromScenario(AProgressUpdateFuntion:TProgressUpdateFuntion):boolean;
    function ExecCopyPowerPlantFromScenario(AProgressUpdateFuntion:TProgressUpdateFuntion):boolean;
    function ExecCopyIrrigationBlockFromScenario(AProgressUpdateFuntion:TProgressUpdateFuntion):boolean;
    function ExecCopyWetlandFromScenario(AProgressUpdateFuntion:TProgressUpdateFuntion):boolean;
    function ExecYMDemandCentreFromScenario(AProgressUpdateFuntion:TProgressUpdateFuntion):boolean;
    function ExecSFRFromScenario(AProgressUpdateFuntion:TProgressUpdateFuntion):boolean;
    function ExecMineFromScenario(AProgressUpdateFuntion:TProgressUpdateFuntion):boolean;
    function ExecGroundWaterFromScenario(AProgressUpdateFuntion: TProgressUpdateFuntion): boolean;
  public
    function Initialise: boolean; override;
    function CopyReservoirFromScenario: boolean;
    function CopyChannelFromScenario: boolean;
    function CopyIrrigationAreaFromScenario: boolean;
    function CopyPowerPlantFromScenario: boolean;
    function CopyIrrigationBlockFromScenario: boolean;
    function CopyWetlandFromScenario: boolean;
    function CopyYMDemandCentreFromScenario:boolean;
    function CopySFRFromScenario:boolean;
    function CopyMineFromScenario:boolean;
    function CopyGroundWaterFromScenario:boolean;

  end;


implementation

uses
  SysUtils,
  UConstants,
  VoaimsCom_TLB,
  UYieldModelDataObject,
  UReservoirDataSQLAgent,
  UChannelDataSQLAgent,
  UNetworkFeaturesSQLAgent,
  UIrrigationBlockSQLAgent,
  UWetlandSQLAgent,
  UYMDemandCentreSQLAgent,
  UStreamFlowReductionSQLAgent,
  UMineSQLAgent,
  UGroundWaterSQLAgent,
  UErrorHandlingOperations;

{ TCopyScenarioDataManager }

procedure TCopyScenarioDataManager.CreateMemberObjects;
const OPNAME = 'TCopyScenarioDataManager.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
    FSelectedDataContainer   := TStringList.Create;
    FExistingDataContainer   := TStringList.Create;
    FProgressDialog          := TProgressDialog.Create(nil,FAppModules);
    FCopyDataDialog          := TCopyScenarioDataDialog.CreateWithoutDFM(nil,FAppModules);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TCopyScenarioDataManager.DestroyMemberObjects;
const OPNAME = 'TCopyScenarioDataManager.DestroyMemberObjects';
begin
  try
    FreeAndNil(FSelectedDataContainer);
    FreeAndNil(FExistingDataContainer);
    FreeAndNil(FProgressDialog);
    FreeAndNil(FCopyDataDialog);
    inherited DestroyMemberObjects;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TCopyScenarioDataManager.Initialise: boolean;
const OPNAME = 'TCopyScenarioDataManager.Initialise';
begin
  Result := inherited Initialise;
  try
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TCopyScenarioDataManager.CopyReservoirFromScenario: boolean;
const OPNAME = 'TCopyScenarioDataManager.CopyReservoirFromScenario';
var
  LIndex : integer;
  LTableName,
  LIndexField,
  LDisplayFieldName,
  LWhereClause: string;
  LReservoirName : string;
  LReservoirNumber : integer;
  LReservoirDataList : IReservoirDataList;
begin
  Result := False;
  try
    FStudy    := '';
    FSubArea  := '';
    FScenario := '';
    FSelectedDataContainer.Clear;
    FExistingDataContainer.Clear;
    LReservoirDataList := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.ReservoirList;
    for LIndex := 0 to LReservoirDataList.ReservoirAndNodesCount-1 do
    begin
      LReservoirName    := LReservoirDataList.ReservoirOrNodeByIndex[LIndex].ReservoirConfigurationData.ReservoirName;
      LReservoirNumber  := LReservoirDataList.ReservoirOrNodeByIndex[LIndex].ReservoirConfigurationData.ReservoirIdentifier;
      FExistingDataContainer.AddObject(LReservoirName,TObject(LReservoirNumber));
    end;

    LTableName          := 'ReservoirDetails';
    LIndexField         := 'NodeCount';
    LDisplayFieldName   := 'ReservoirName';
    LWhereClause        := 'AND (NodeType = 1)';
    FCopyDataDialog.CaptionStr := FAppModules.Language.GetString('TCopyScenarioDataManager.strCopyReservoir');

    Result := FCopyDataDialog.CopyDataFromScenario(LTableName,LIndexField,LDisplayFieldName,LWhereClause,
              FSelectedDataContainer,FExistingDataContainer);
    if Result then
    begin
      FStudy     := FCopyDataDialog.Study;
      FSubArea   := FCopyDataDialog.SubArea;
      FScenario  := FCopyDataDialog.Scenario;
      FProgressDialog.Initialise;
      FProgressDialog.LanguageHasChanged;
      FProgressDialog.ActionProgressBar.Min := 0;
      FProgressDialog.ActionProgressBar.Max := FSelectedDataContainer.Count;
      FProgressDialog.AddExecutionFunctions(ExecCopyReservoirDataFromScenario);
      FProgressDialog.Caption := FAppModules.Language.GetString('TCopyScenarioDataManager.strCopyReservoir');
      FProgressDialog.Succsessful := False;
      FProgressDialog.ShowModal;
      Result := FProgressDialog.Succsessful;
      FProgressDialog.Hide;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TCopyScenarioDataManager.CopyChannelFromScenario: boolean;
const OPNAME = 'TCopyScenarioDataManager.CopyChannelFromScenario';
var
  LIndex            : integer;
  LTableName        : string;
  LIndexField       : string;
  LDisplayFieldName : string;
  LWhereClause      : string;
  LChannelName      : string;
  LChannelNumber    : integer;
  LChannelDataList  : IChannelList;
begin
  Result := False;
  try
    FStudy    := '';
    FSubArea  := '';
    FScenario := '';
    FSelectedDataContainer.Clear;
    FExistingDataContainer.Clear;
    LChannelDataList := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.ChannelList;
    for LIndex := 0 to LChannelDataList.ChannelCount-1 do
    begin
      LChannelName   := LChannelDataList.ChannelByIndex[LIndex].ChannelName;
      LChannelNumber := LChannelDataList.ChannelByIndex[LIndex].ChannelNumber;
      FExistingDataContainer.AddObject(LChannelName,TObject(LChannelNumber));
    end;

    LTableName          := 'ChannelDetails';
    LIndexField         := 'ChannelNumber';
    LDisplayFieldName   := 'ChannelName';
    LWhereClause        := '';
    FCopyDataDialog.CaptionStr := FAppModules.Language.GetString('TCopyScenarioDataManager.strCopyChannel');

    Result := FCopyDataDialog.CopyDataFromScenario(LTableName,LIndexField,LDisplayFieldName,LWhereClause,
              FSelectedDataContainer,FExistingDataContainer);
    if Result then
    begin
      FStudy     := FCopyDataDialog.Study;
      FSubArea   := FCopyDataDialog.SubArea;
      FScenario  := FCopyDataDialog.Scenario;
      FProgressDialog.Initialise;
      FProgressDialog.LanguageHasChanged;
      FProgressDialog.ActionProgressBar.Min := 0;
      FProgressDialog.ActionProgressBar.Max := FSelectedDataContainer.Count;
      FProgressDialog.AddExecutionFunctions(ExecCopyChannelDataFromScenario);
      FProgressDialog.Caption := FAppModules.Language.GetString('TCopyScenarioDataManager.strCopyChannel');
      FProgressDialog.Succsessful := False;
      FProgressDialog.ShowModal;
      Result := FProgressDialog.Succsessful;
      FProgressDialog.Hide;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TCopyScenarioDataManager.CopyIrrigationAreaFromScenario: boolean;
const OPNAME = 'TCopyScenarioDataManager.CopyIrrigationAreaFromScenario';
var
  LIrrigationAreaDataList    : IIrrigationAreaList;
  LIndex                     : integer;
  LTableName                 : string;
  LIndexField                : string;
  LDisplayFieldName          : string;
  LWhereClause               : string;
  LIrrigationAreaName        : string;
  LIrrigationNodeNumber      : integer;
begin
  Result := False;
  try
    FStudy    := '';
    FSubArea  := '';
    FScenario := '';
    FSelectedDataContainer.Clear;
    FExistingDataContainer.Clear;

    LIrrigationAreaDataList := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.IrrigationAreaList;
    for LIndex := 0 to LIrrigationAreaDataList.IrrigationAreaCount-1 do
    begin
      LIrrigationAreaName   := LIrrigationAreaDataList.IrrigationAreaByIndex[LIndex].FeatureName;
      LIrrigationNodeNumber := LIrrigationAreaDataList.IrrigationAreaByIndex[LIndex].IrrigationNodeNumber;
      FExistingDataContainer.AddObject(LIrrigationAreaName,TObject(LIrrigationNodeNumber));
    end;

    LTableName          := 'IrrigationAreas';
    LIndexField         := 'IrrigationNodeNumber';
    LDisplayFieldName   := 'AreaName';
    LWhereClause        := '';
    FCopyDataDialog.CaptionStr := FAppModules.Language.GetString('TCopyScenarioDataManager.strIrrigationAreas');

    Result := FCopyDataDialog.CopyDataFromScenario(LTableName,LIndexField,LDisplayFieldName,LWhereClause,
              FSelectedDataContainer,FExistingDataContainer);
    if Result then
    begin
      FStudy     := FCopyDataDialog.Study;
      FSubArea   := FCopyDataDialog.SubArea;
      FScenario  := FCopyDataDialog.Scenario;
      FProgressDialog.Initialise;
      FProgressDialog.LanguageHasChanged;
      FProgressDialog.ActionProgressBar.Min := 0;
      FProgressDialog.ActionProgressBar.Max := FSelectedDataContainer.Count;
      FProgressDialog.AddExecutionFunctions(ExecCopyIrrigationAreaFromScenario);
      FProgressDialog.Caption := FAppModules.Language.GetString('TCopyScenarioDataManager.strIrrigationAreas');
      FProgressDialog.Succsessful := False;
      FProgressDialog.ShowModal;
      Result := FProgressDialog.Succsessful;
      FProgressDialog.Hide;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TCopyScenarioDataManager.CopyIrrigationBlockFromScenario: boolean;
const OPNAME = 'TCopyScenarioDataManager.CopyIrrigationBlockFromScenario';
var
  LIrrigationBlockDataList   : IIrrigationBlockList;
  LIndex                     : integer;
  LTableName                 : string;
  LIndexField                : string;
  LDisplayFieldName          : string;
  LWhereClause               : string;
  LIrrigationBlockName       : string;
  LIrrigationNodeNumber      : integer;
begin
  Result := False;
  try
    FStudy    := '';
    FSubArea  := '';
    FScenario := '';
    FSelectedDataContainer.Clear;
    FExistingDataContainer.Clear;

    LIrrigationBlockDataList := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.IrrigationBlockList;
    for LIndex := 0 to LIrrigationBlockDataList.IrrigationBlockCount-1 do
    begin
      LIrrigationBlockName   := LIrrigationBlockDataList.IrrigationBlockByIndex[LIndex].BlockName;
      LIrrigationNodeNumber  := LIrrigationBlockDataList.IrrigationBlockByIndex[LIndex].BlockNodeNumber;
      FExistingDataContainer.AddObject(LIrrigationBlockName,TObject(LIrrigationNodeNumber));
    end;

    LTableName          := 'IrrigationBlock';
    LIndexField         := 'BlockNumber';
    LDisplayFieldName   := 'BlockName';
    LWhereClause        := '';
    FCopyDataDialog.CaptionStr := FAppModules.Language.GetString('TCopyScenarioDataManager.strIrrigationBlock');

    Result := FCopyDataDialog.CopyDataFromScenario(LTableName,LIndexField,LDisplayFieldName,LWhereClause,
              FSelectedDataContainer,FExistingDataContainer);
    if Result then
    begin
      FStudy     := FCopyDataDialog.Study;
      FSubArea   := FCopyDataDialog.SubArea;
      FScenario  := FCopyDataDialog.Scenario;
      FProgressDialog.Initialise;
      FProgressDialog.LanguageHasChanged;
      FProgressDialog.ActionProgressBar.Min := 0;
      FProgressDialog.ActionProgressBar.Max := FSelectedDataContainer.Count;
      FProgressDialog.AddExecutionFunctions(ExecCopyIrrigationBlockFromScenario);
      FProgressDialog.Caption := FAppModules.Language.GetString('TCopyScenarioDataManager.strIrrigationBlock');
      FProgressDialog.Succsessful := False;
      FProgressDialog.ShowModal;
      Result := FProgressDialog.Succsessful;
      FProgressDialog.Hide;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TCopyScenarioDataManager.CopyWetlandFromScenario: boolean;
const OPNAME = 'TCopyScenarioDataManager.CopyWetlandFromScenario';
var
  LWetlandDataList           : IWetlandList;
  LIndex                     : integer;
  LTableName                 : string;
  LIndexField                : string;
  LDisplayFieldName          : string;
  LWhereClause               : string;
  LWetlandName               : string;
  LNodeNumber                : integer;
begin
  Result := False;
  try
    FStudy    := '';
    FSubArea  := '';
    FScenario := '';
    FSelectedDataContainer.Clear;
    FExistingDataContainer.Clear;

    LWetlandDataList := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.WetlandList;
    for LIndex := 0 to LWetlandDataList.WetLandCount-1 do
    begin
      LWetlandName   := LWetlandDataList.WetlandByIndex[LIndex].Name;
      LNodeNumber  := LWetlandDataList.WetlandByIndex[LIndex].NodeNumber;
      FExistingDataContainer.AddObject(LWetlandName,TObject(LNodeNumber));
    end;

    LTableName          := 'Wetland';
    LIndexField         := 'NodeNumber';
    LDisplayFieldName   := 'WetlandName';
    LWhereClause        := '';
    FCopyDataDialog.CaptionStr := FAppModules.Language.GetString('TCopyScenarioDataManager.strWetland');

    Result := FCopyDataDialog.CopyDataFromScenario(LTableName,LIndexField,LDisplayFieldName,LWhereClause,
              FSelectedDataContainer,FExistingDataContainer);
    if Result then
    begin
      FStudy     := FCopyDataDialog.Study;
      FSubArea   := FCopyDataDialog.SubArea;
      FScenario  := FCopyDataDialog.Scenario;
      FProgressDialog.Initialise;
      FProgressDialog.LanguageHasChanged;
      FProgressDialog.ActionProgressBar.Min := 0;
      FProgressDialog.ActionProgressBar.Max := FSelectedDataContainer.Count;
      FProgressDialog.AddExecutionFunctions(ExecCopyWetlandFromScenario);
      FProgressDialog.Caption := FAppModules.Language.GetString('TCopyScenarioDataManager.strWetland');
      FProgressDialog.Succsessful := False;
      FProgressDialog.ShowModal;
      Result := FProgressDialog.Succsessful;
      FProgressDialog.Hide;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TCopyScenarioDataManager.CopyPowerPlantFromScenario: boolean;
const OPNAME = 'TCopyScenarioDataManager.CopyPowerPlantFromScenario';
var
  LPowerPlantDataList        : IPowerPlantList;
  LIndex                     : integer;
  LTableName                 : string;
  LIndexField                : string;
  LDisplayFieldName          : string;
  LWhereClause               : string;
  LPowerPlantName            : string;
  LPowerPlantIdentifier      : integer;
begin
  Result := False;
  try
    FStudy    := '';
    FSubArea  := '';
    FScenario := '';
    FSelectedDataContainer.Clear;
    FExistingDataContainer.Clear;

    LPowerPlantDataList := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.PowerPlantList;
    for LIndex := 0 to LPowerPlantDataList.PowerPlantCount-1 do
    begin
      LPowerPlantName   := LPowerPlantDataList.PowerPlantByIndex[LIndex].FeatureName;
      LPowerPlantIdentifier := LPowerPlantDataList.PowerPlantByIndex[LIndex].FeatureID;
      FExistingDataContainer.AddObject(LPowerPlantName,TObject(LPowerPlantIdentifier));
    end;

    LTableName          := 'PowerPlants';
    LIndexField         := 'Identifier';
    LDisplayFieldName   := 'PowerPlantName';
    LWhereClause        := '';
    FCopyDataDialog.CaptionStr := FAppModules.Language.GetString('TCopyScenarioDataManager.strPowerPlant');

    Result := FCopyDataDialog.CopyDataFromScenario(LTableName,LIndexField,LDisplayFieldName,LWhereClause,
              FSelectedDataContainer,FExistingDataContainer);
    if Result then
    begin
      FStudy     := FCopyDataDialog.Study;
      FSubArea   := FCopyDataDialog.SubArea;
      FScenario  := FCopyDataDialog.Scenario;
      FProgressDialog.Initialise;
      FProgressDialog.LanguageHasChanged;
      FProgressDialog.ActionProgressBar.Min := 0;
      FProgressDialog.ActionProgressBar.Max := FSelectedDataContainer.Count;
      FProgressDialog.AddExecutionFunctions(ExecCopyPowerPlantFromScenario);
      FProgressDialog.Caption := FAppModules.Language.GetString('TCopyScenarioDataManager.strPowerPlant');
      FProgressDialog.Succsessful := False;
      FProgressDialog.ShowModal;
      Result := FProgressDialog.Succsessful;
      FProgressDialog.Hide;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TCopyScenarioDataManager.CopyYMDemandCentreFromScenario:boolean;
const OPNAME = 'TCopyScenarioDataManager.CopyYMDemandCentreFromScenario';
var
  LYMDemandCentreDataList    : IYMDemandCentreList;
  LIndex                     : integer;
  LTableName                 : string;
  LIndexField                : string;
  LDisplayFieldName          : string;
  LWhereClause               : string;
  LCentreName                : string;
  LNodeNumber                : integer;
begin
  Result := False;
  try
    FStudy    := '';
    FSubArea  := '';
    FScenario := '';
    FSelectedDataContainer.Clear;
    FExistingDataContainer.Clear;

    LYMDemandCentreDataList := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.YMDemandCentreList;
    for LIndex := 0 to LYMDemandCentreDataList.YMDemandCentreCount-1 do
    begin
      LCentreName   := LYMDemandCentreDataList.YMDemandCentreByIndex[LIndex].Name;
      LNodeNumber := LYMDemandCentreDataList.YMDemandCentreByIndex[LIndex].NodeNumber;
      FExistingDataContainer.AddObject(LCentreName,TObject(LNodeNumber));
    end;

    LTableName          := 'YMDemandCentre';
    LIndexField         := 'NodeNumber';
    LDisplayFieldName   := 'CentreName';
    LWhereClause        := '';
    FCopyDataDialog.CaptionStr := FAppModules.Language.GetString('TCopyScenarioDataManager.strYMDemandCentre');

    Result := FCopyDataDialog.CopyDataFromScenario(LTableName,LIndexField,LDisplayFieldName,LWhereClause,
              FSelectedDataContainer,FExistingDataContainer);
    if Result then
    begin
      FStudy     := FCopyDataDialog.Study;
      FSubArea   := FCopyDataDialog.SubArea;
      FScenario  := FCopyDataDialog.Scenario;
      FProgressDialog.Initialise;
      FProgressDialog.LanguageHasChanged;
      FProgressDialog.ActionProgressBar.Min := 0;
      FProgressDialog.ActionProgressBar.Max := FSelectedDataContainer.Count;
      FProgressDialog.AddExecutionFunctions(ExecYMDemandCentreFromScenario);
      FProgressDialog.Caption := FAppModules.Language.GetString('TCopyScenarioDataManager.strYMDemandCentre');
      FProgressDialog.Succsessful := False;
      FProgressDialog.ShowModal;
      Result := FProgressDialog.Succsessful;
      FProgressDialog.Hide;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;


function TCopyScenarioDataManager.CopySFRFromScenario:boolean;
const OPNAME = 'TCopyScenarioDataManager.CopySFRFromScenario';
var
  LSFRDataList               : IStreamFlowReductionList;
  LIndex                     : integer;
  LTableName                 : string;
  LIndexField                : string;
  LDisplayFieldName          : string;
  LWhereClause               : string;
  LSFRName                : string;
  LIdentifier                : integer;
begin
  Result := False;
  try
    FStudy    := '';
    FSubArea  := '';
    FScenario := '';
    FSelectedDataContainer.Clear;
    FExistingDataContainer.Clear;

    LSFRDataList := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.StreamFlowReductionList;
    for LIndex := 0 to LSFRDataList.StreamFlowReductionCount-1 do
    begin
      LSFRName   := LSFRDataList.StreamFlowReductionByIndex[LIndex].SFRName;
      LIdentifier := LSFRDataList.StreamFlowReductionByIndex[LIndex].Identifier;
      FExistingDataContainer.AddObject(LSFRName,TObject(LIdentifier));
    end;

    LTableName          := 'SFRSubCatchment';
    LIndexField         := 'Identifier';
    LDisplayFieldName   := 'SFRName';
    LWhereClause        := '';
    FCopyDataDialog.CaptionStr := FAppModules.Language.GetString('TCopyScenarioDataManager.strSFR');

    Result := FCopyDataDialog.CopyDataFromScenario(LTableName,LIndexField,LDisplayFieldName,LWhereClause,
              FSelectedDataContainer,FExistingDataContainer);
    if Result then
    begin
      FStudy     := FCopyDataDialog.Study;
      FSubArea   := FCopyDataDialog.SubArea;
      FScenario  := FCopyDataDialog.Scenario;
      FProgressDialog.Initialise;
      FProgressDialog.LanguageHasChanged;
      FProgressDialog.ActionProgressBar.Min := 0;
      FProgressDialog.ActionProgressBar.Max := FSelectedDataContainer.Count;
      FProgressDialog.AddExecutionFunctions(ExecSFRFromScenario);
      FProgressDialog.Caption := FAppModules.Language.GetString('TCopyScenarioDataManager.strSFR');
      FProgressDialog.Succsessful := False;
      FProgressDialog.ShowModal;
      Result := FProgressDialog.Succsessful;
      FProgressDialog.Hide;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TCopyScenarioDataManager.CopyMineFromScenario:boolean;
const OPNAME = 'TCopyScenarioDataManager.CopyMineFromScenario';
var
  LMineDataList              : IMineList;
  LIndex                     : integer;
  LTableName                 : string;
  LIndexField                : string;
  LDisplayFieldName          : string;
  LWhereClause               : string;
  LMineName                  : string;
  LNodeNumber                : integer;
begin
  Result := False;
  try
    FStudy    := '';
    FSubArea  := '';
    FScenario := '';
    FSelectedDataContainer.Clear;
    FExistingDataContainer.Clear;

    LMineDataList := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.MineList;
    for LIndex := 0 to LMineDataList.MineCount-1 do
    begin
      LMineName   := LMineDataList.MineByIndex[LIndex].MineName;
      LNodeNumber := LMineDataList.MineByIndex[LIndex].NodeNumber;
      FExistingDataContainer.AddObject(LMineName,TObject(LNodeNumber));
    end;

    LTableName          := 'Mine';
    LIndexField         := 'NodeNumber';
    LDisplayFieldName   := 'MineName';
    LWhereClause        := '';
    FCopyDataDialog.CaptionStr := FAppModules.Language.GetString('TCopyScenarioDataManager.strMine');

    Result := FCopyDataDialog.CopyDataFromScenario(LTableName,LIndexField,LDisplayFieldName,LWhereClause,
              FSelectedDataContainer,FExistingDataContainer);
    if Result then
    begin
      FStudy     := FCopyDataDialog.Study;
      FSubArea   := FCopyDataDialog.SubArea;
      FScenario  := FCopyDataDialog.Scenario;
      FProgressDialog.Initialise;
      FProgressDialog.LanguageHasChanged;
      FProgressDialog.ActionProgressBar.Min := 0;
      FProgressDialog.ActionProgressBar.Max := FSelectedDataContainer.Count;
      FProgressDialog.AddExecutionFunctions(ExecMineFromScenario);
      FProgressDialog.Caption := FAppModules.Language.GetString('TCopyScenarioDataManager.strMine');
      FProgressDialog.Succsessful := False;
      FProgressDialog.ShowModal;
      Result := FProgressDialog.Succsessful;
      FProgressDialog.Hide;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TCopyScenarioDataManager.ExecCopyReservoirDataFromScenario(AProgressUpdateFuntion: TProgressUpdateFuntion): boolean;
const OPNAME = 'TCopyScenarioDataManager.ExecCopyReservoirDataFromScenario';
var
  LSQLAgent : TReservoirDataSQLAgent;
begin
  Result := False;
  try
    FProgressDialog.KeepStartBtnDisabled := True;
    LSQLAgent := TReservoirDataSQLAgent.Create(FAppModules);
    try
      Result := LSQLAgent.CopyReservoirsFromScenario(FStudy,FSubArea,FScenario,FSelectedDataContainer,AProgressUpdateFuntion);
    finally
      LSQLAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TCopyScenarioDataManager.ExecCopyChannelDataFromScenario(AProgressUpdateFuntion: TProgressUpdateFuntion): boolean;
const OPNAME = 'TCopyScenarioDataManager.ExecCopyChannelDataFromScenario';
var
  LSQLAgent : TChannelDataSQLAgent;
begin
  Result := False;
  try
    FProgressDialog.KeepStartBtnDisabled := True;
    LSQLAgent := TChannelDataSQLAgent.Create(FAppModules);
    try
      Result := LSQLAgent.CopyChannelFromScenario(FStudy,FSubArea,FScenario,FSelectedDataContainer,AProgressUpdateFuntion);
    finally
      LSQLAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TCopyScenarioDataManager.ExecCopyIrrigationAreaFromScenario(AProgressUpdateFuntion:TProgressUpdateFuntion):boolean;
const OPNAME = 'TCopyScenarioDataManager.ExecCopyIrrigationAreaFromScenario';
var
  LSQLAgent : TNetworkFeaturesSQLAgent;
begin
  Result := False;
  try
    FProgressDialog.KeepStartBtnDisabled := True;
    LSQLAgent := TNetworkFeaturesSQLAgent.Create(FAppModules);
    try
      Result := LSQLAgent.CopyIrrigationAreaFromScenario(FStudy,FSubArea,FScenario,FSelectedDataContainer,AProgressUpdateFuntion);
    finally
      LSQLAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TCopyScenarioDataManager.ExecCopyPowerPlantFromScenario(AProgressUpdateFuntion:TProgressUpdateFuntion):boolean;
const OPNAME = 'TCopyScenarioDataManager.ExecCopyPowerPlantFromScenario';
var
  LSQLAgent : TNetworkFeaturesSQLAgent;
begin
  Result := False;
  try
    FProgressDialog.KeepStartBtnDisabled := True;
    LSQLAgent := TNetworkFeaturesSQLAgent.Create(FAppModules);
    try
      Result := LSQLAgent.CopyPowerPlantsFromScenario(FStudy,FSubArea,FScenario,FSelectedDataContainer,AProgressUpdateFuntion);
    finally
      LSQLAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TCopyScenarioDataManager.ExecCopyIrrigationBlockFromScenario(AProgressUpdateFuntion:TProgressUpdateFuntion):boolean;
const OPNAME = 'TCopyScenarioDataManager.ExecCopyIrrigationBlockFromScenario';
var
  LSQLAgent : TIrrigationBlockSQLAgent;
begin
  Result := False;
  try
    FProgressDialog.KeepStartBtnDisabled := True;
    LSQLAgent := TIrrigationBlockSQLAgent.Create(FAppModules);
    try
      Result := LSQLAgent.CopyIrrigationBlockFromScenario(FStudy,FSubArea,FScenario,FSelectedDataContainer,AProgressUpdateFuntion);
    finally
      LSQLAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TCopyScenarioDataManager.ExecCopyWetlandFromScenario(AProgressUpdateFuntion:TProgressUpdateFuntion):boolean;
const OPNAME = 'TCopyScenarioDataManager.ExecCopyWetlandFromScenario';
var
  LSQLAgent : TWetlandSQLAgent;
begin
  Result := False;
  try
    FProgressDialog.KeepStartBtnDisabled := True;
    LSQLAgent := TWetlandSQLAgent.Create(FAppModules);
    try
      Result := LSQLAgent.CopyWetlandFromScenario(FStudy,FSubArea,FScenario,FSelectedDataContainer,AProgressUpdateFuntion);
    finally
      LSQLAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TCopyScenarioDataManager.ExecYMDemandCentreFromScenario(AProgressUpdateFuntion:TProgressUpdateFuntion):boolean;
const OPNAME = 'TCopyScenarioDataManager.ExecYMDemandCentreFromScenario';
var
  LSQLAgent : TYMDemandCentreSQLAgent;
begin
  Result := False;
  try
    FProgressDialog.KeepStartBtnDisabled := True;
    LSQLAgent := TYMDemandCentreSQLAgent.Create(FAppModules);
    try
      Result := LSQLAgent.CopyYMDemandCentreFromScenario(FStudy,FSubArea,FScenario,FSelectedDataContainer,AProgressUpdateFuntion);
    finally
      LSQLAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TCopyScenarioDataManager.ExecSFRFromScenario(AProgressUpdateFuntion:TProgressUpdateFuntion):boolean;
const OPNAME = 'TCopyScenarioDataManager.ExecSFRFromScenario';
var
  LSQLAgent : TStreamFlowReductionSQLAgent;
begin
  Result := False;
  try
    FProgressDialog.KeepStartBtnDisabled := True;
    LSQLAgent := TStreamFlowReductionSQLAgent.Create(FAppModules);
    try
      Result := LSQLAgent.CopySFRFromScenario(FStudy,FSubArea,FScenario,FSelectedDataContainer,AProgressUpdateFuntion);
    finally
      LSQLAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TCopyScenarioDataManager.ExecMineFromScenario(AProgressUpdateFuntion:TProgressUpdateFuntion):boolean;
const OPNAME = 'TCopyScenarioDataManager.ExecMineFromScenario';
var
  LSQLAgent : TMineSQLAgent;
begin
  Result := False;
  try
    FProgressDialog.KeepStartBtnDisabled := True;
    LSQLAgent := TMineSQLAgent.Create(FAppModules);
    try
      Result := LSQLAgent.CopyMineFromScenario(FStudy,FSubArea,FScenario,FSelectedDataContainer,AProgressUpdateFuntion);
    finally
      LSQLAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TCopyScenarioDataManager.ExecGroundWaterFromScenario(AProgressUpdateFuntion: TProgressUpdateFuntion): boolean;
const OPNAME = 'TCopyScenarioDataManager.ExecGroundWaterFromScenario';
var
  LSQLAgent : TGroundWaterSQLAgent;
begin
  Result := False;
  try
    FProgressDialog.KeepStartBtnDisabled := True;
    LSQLAgent := TGroundWaterSQLAgent.Create(FAppModules);
    try
      Result := LSQLAgent.CopyGroundWaterFromScenario(FStudy,FSubArea,FScenario,FSelectedDataContainer,AProgressUpdateFuntion);
    finally
      LSQLAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;


function TCopyScenarioDataManager.CopyGroundWaterFromScenario: boolean;
const OPNAME = 'TCopyScenarioDataManager.CopyGroundWaterFromScenario';
var
  LGroundWaterDataList       : IGroundWaterList;
  LIndex                     : integer;
  LTableName                 : string;
  LIndexField                : string;
  LDisplayFieldName          : string;
  LWhereClause               : string;
  LGroundWaterName           : string;
  LGroundWaterIdentifier     : integer;
begin
  Result := False;
  try
    FStudy    := '';
    FSubArea  := '';
    FScenario := '';
    FSelectedDataContainer.Clear;
    FExistingDataContainer.Clear;

    LGroundWaterDataList := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.GroundWaterList;
    for LIndex := 0 to LGroundWaterDataList.GroundWaterCount - 1 do
    begin
      LGroundWaterName       := LGroundWaterDataList.GroundWaterByIndex[LIndex].Name;
      LGroundWaterIdentifier := LGroundWaterDataList.GroundWaterByIndex[LIndex].Identifier;
      FExistingDataContainer.AddObject(LGroundWaterName,TObject(LGroundWaterIdentifier));
    end;
    LTableName          := 'GroundWater';
    LIndexField         := 'Identifier';
    LDisplayFieldName   := 'GroundWaterName';
    LWhereClause        := '';
    FCopyDataDialog.CaptionStr := FAppModules.Language.GetString('TCopyScenarioDataManager.strGroundWater');
    Result := FCopyDataDialog.CopyDataFromScenario(LTableName,LIndexField,LDisplayFieldName,LWhereClause,
              FSelectedDataContainer,FExistingDataContainer);
    if Result then
    begin
      FStudy     := FCopyDataDialog.Study;
      FSubArea   := FCopyDataDialog.SubArea;
      FScenario  := FCopyDataDialog.Scenario;
      FProgressDialog.Initialise;
      FProgressDialog.LanguageHasChanged;
      FProgressDialog.ActionProgressBar.Min := 0;
      FProgressDialog.ActionProgressBar.Max := FSelectedDataContainer.Count;
      FProgressDialog.AddExecutionFunctions(ExecGroundWaterFromScenario);
      FProgressDialog.Caption := FAppModules.Language.GetString('TCopyScenarioDataManager.strGroundWater');
      FProgressDialog.Succsessful := False;
      FProgressDialog.ShowModal;
      Result := FProgressDialog.Succsessful;
      FProgressDialog.Hide;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;



end.
