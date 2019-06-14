//
//  UNIT      : Contains TPlanningModelManager Class
//  AUTHOR    : Dziedzi Ramulondi(PDNA)
//  DATE      : 04/01/2006
//  COPYRIGHT : Copyright © 2006 DWAF
//
unit UPlanningModelManager;

interface

uses
  Classes,
  VCL.ComCtrls,
  VoaimsCom_TLB,
  UPlanningModelDataObject,
  UPlanningModelDataGUIManager,
  UAbstractObject,
  UPlanningMineData,
  UYieldModelManager;

type
  TPlanningModelManager = class(TYieldModelManager)
  protected

    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;

    procedure CreateModelData; override;
    procedure CreateFileActionManager; override;
    procedure CreateFileSelectionManager; override;
    procedure CreateModelGUIManager; override;
    procedure CreateTimeSeriesComparitor;override;
    procedure CreateYieldReliabilityCurveManager; override;
    procedure CreateOutputComparison; override;

    function LoadModelData: boolean; override;
    function LoadMiningData: boolean; override;
    function LoadAllocationDefinitionData: boolean; virtual;
    function LoadSwitchDefinitionData: boolean; virtual;
    function LoadReservoirTimeControlData: boolean; virtual;
    function LoadGrowthFactorData: boolean; virtual;
    function LoadDisbenefitFunctionData : boolean; virtual;
    function LoadReturnFlowChannelData : boolean; virtual;
    function LoadTariffCalculationData: Boolean;virtual;
    function LoadWQConstriantData: Boolean;virtual;
    function LoadMultiRestrictionData: Boolean;virtual;

    function LoadFieldFileReferencesData: Boolean;override;

    function GetPlanningModelData : TPlanningModelDataObject;
    property PlanningModelDataObject : TPlanningModelDataObject read GetPlanningModelData;
  public
    function ModelName: string; override;
    function ProcessEvent (AEventType : integer;
                           AData      : TObject): boolean; override;
    //function DoStationFilter(AChangeListID : integer) : WordBool;override;
    function StudyDataHasChanged(AContext: TChangeContext; AFieldName,AOldValue,ANewValue: string): boolean; override;
    function DoCreateAllocDef : IAllocationDefinition;
    function DoDeleteAllocDef (AAllocDefID : integer) : WordBool;
    function DoCreateSwitchDef : ISwitchDefinition;
    function DoDeleteSwitchDef (ASwitchDefID : integer) : WordBool;
    function DoCreateResTimeControl (AReservoirNr : integer): IReservoirTimeControl;
    function DoDeleteResTimeControl (AReservoirNr : integer): WordBool;
    function DoCreateResReplacement (AReservoirNr : integer): IReservoirData;
    function DoCreateChannelTimeControl (AChannelNr : integer): IChannelTimeControl;
    function DoDeleteChannelTimeControl (AChannelNr : integer): WordBool;
    function DoCreateChannelSwitchControl (AChannelNr : integer): IChannelSwitchControl;
    function DoDeleteChannelSwitchControl (AChannelNr   : integer;
                                           ASwitchDefID : integer): WordBool;
    function DoCreateDisbenefitFunction (AChannelNr : integer): IDisbenefitFunctionDefinition;
    function DoDeleteDisbenefitFunction (AChannelNr : integer): WordBool;
    function DoCreateReturnFlowChannel (AChannelNr : integer): IReturnFlowChannel;
    function DoDeleteReturnFlowChannel (AChannelNr : integer): WordBool;

    function DoCreateChannelTariff(AChannelNr : integer): WordBool; override;
    function DoDeleteChannelTariff(AChannelNr : integer): WordBool; override;

    function DoCreateMultiResChannelCurtail(AChannelNr: integer): IMultiResMultiChannelCurtail;
    function DoDeleteMultiResChannelCurtail(AChannelNr: integer): WordBool;

    function DoDeleteChannel(AChannelNumber: Integer): WordBool;
    function DoCreateMine: IPlanningMine; reintroduce;
    function DoDeleteMine(AMineNumber: Integer): WordBool; reintroduce;
    function DoCreateOpenCast(AMineNumber: integer) : IPlanningOpenCast; safecall;
    function DoDeleteOpenCast(AMineNumber,AOpenCastIdentifier: integer) : WordBool; safecall;
    function DoCreateUnderGround(AMineNumber: integer) : IUnderground; safecall;
    function DoDeleteUnderGround(AMineNumber,AUnderGroundIdentifier: integer) : WordBool; safecall;
    function DoCreateSlurryDump(AMineNumber: integer)  : IPlanningSlurryDump; reintroduce;
    function DoDeleteSlurryDump(AMineNumber,ASlurryDumpIdentifier: integer) : WordBool; safecall;

    function ModelDataLoaded: Boolean; override;
    function PopulateDataList(AModelDataChangeType: TModelDataChangeType;
      AList: TList): Boolean; override;
    function GetFileLineType(AFileObject: TObject; ALineNumber: Integer): string; override;




  end;

implementation

uses
  System.UITypes,
  windows,
  SysUtils,
  VCL.Dialogs,
  VCL.Controls,
  VCL.Forms,
  VarUtils,
  UConstants,
  UFileNames,
  UFilesActionPlanningManager,
  UPlanningFileSelectionManager,
  UMainMenuEventType,
  UViewDataItem,
  UTreeViewTabSheet,
  UDataViewerSheet,
  UAllocationDefLoadAgent,
  UReservoirTimeControlLoadAgent,
  UDisbenefitFunctionDefinitionLoadAgent,
  USwitchDefinitionLoadAgent,
  UGrowthFactorLoadAgent,
  UReturnFlowChannelLoadAgent,
  UChannelPlanningLoadAgent,
  UTariffCalculationLoadAgent,
  UWQConstraintLoadAgent,
  UMultiRestrictionLoadAgent,
  USystemModelManager,
  UYieldModelDataObject,
  UMultiResChannelCurtailmentData,
  UPlanningMineLoadAgent,
  UUtilities,
  UErrorHandlingOperations;

{ TPlanningModelManager }

procedure TPlanningModelManager.CreateMemberObjects;
const OPNAME = 'TPlanningModelManager.CreateMemberObjects';
begin
  inherited CreateMemberObjects;
  try

  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TPlanningModelManager.DestroyMemberObjects;
const OPNAME = 'TPlanningModelManager.DestroyMemberObjects';
begin
  try
    try
    finally
      inherited DestroyMemberObjects;

    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TPlanningModelManager._AddRef: Integer;
const OPNAME = 'TPlanningModelManager._AddRef';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TPlanningModelManager._Release: Integer;
const OPNAME = 'TPlanningModelManager._Release';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TPlanningModelManager.ModelName: string;
const OPNAME = 'TPlanningModelManager.ModelName';
begin
  Result := '';
  try
    Result := CPlanning;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TPlanningModelManager.GetPlanningModelData : TPlanningModelDataObject;
const OPNAME = 'TPlanningModelManager.GetPlanningModelData';
begin
  Result := nil;
  try
    Result := TPlanningModelDataObject(FModelData);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TPlanningModelManager.CreateFileActionManager;
const OPNAME = 'TPlanningModelManager.CreateFileActionManager';
begin
  try
    FModelFilesActionManager := TFilesActionPlanningManager.Create(FAppModules);
    if Assigned(FModelFilesActionManager) then
    begin
      FOwnedAppObjects.Add(FModelFilesActionManager);
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TPlanningModelManager.CreateFileSelectionManager;
const OPNAME = 'TPlanningModelManager.CreateFileSelectionManager';
begin
  try
    FFileSelectionManager := TPlanningFileSelectionManager.Create(FAppModules);
    if Assigned(FFileSelectionManager) then
    begin
      FOwnedAppObjects.Add(FFileSelectionManager);
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TPlanningModelManager.CreateOutputComparison;
const OPNAME = 'TPlanningModelManager.CreateOutputComparison';
begin
  try
   //Do not create this tabsheet for the planning model.
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TPlanningModelManager.CreateTimeSeriesComparitor;
const OPNAME = 'TPlanningModelManager.CreateTimeSeriesComparitor';
begin
  try
   //Do not create this tabsheet for the planning model.
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TPlanningModelManager.CreateYieldReliabilityCurveManager;
const OPNAME = 'TPlanningModelManager.CreateYieldReliabilityCurveManager';
begin
  try
   //Do not create this tabsheet for the planning model.
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TPlanningModelManager.CreateModelGUIManager;
const OPNAME = 'TPlanningModelManager.CreateModelGUIManager';
begin
  try
    FModelDataGUIManager := TPlanningModelDataGUIManager.Create(FAppModules);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TPlanningModelManager.CreateModelData;
const OPNAME = 'TPlanningModelManager.CreateModelData';
begin
  try
    FModelData := TPlanningModelDataObject.Create(FAppModules);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TPlanningModelManager.LoadModelData : Boolean;
const OPNAME = 'TPlanningModelManager.LoadModelData';
begin
  Result := inherited LoadModelData;
  try
    FLoadingData := True;
    try
      Result := Result and LoadAllocationDefinitionData;
      Result := Result and LoadSwitchDefinitionData;
      Result := Result AND LoadReservoirTimeControlData;
      Result := Result AND LoadGrowthFactorData;
      Result := Result AND LoadDisbenefitFunctionData;
      Result := Result AND LoadReturnFlowChannelData;
      Result := Result AND LoadTariffCalculationData;
      Result := Result AND LoadWQConstriantData;
      Result := Result AND LoadFieldFileReferencesData;
      Result := Result AND LoadMultiRestrictionData;
      Result := Result AND LoadMiningData;

    finally
      FLoadingData := False;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TPlanningModelManager.LoadMultiRestrictionData: Boolean;
const OPNAME = 'TPlanningModelManager.LoadMultiRestrictionData';
var
  LLoadAgent : TMultiRestrictionLoadAgent;
begin
  Result := False;
  try
    if (PlanningModelDataObject.CastMultiRestrictionData <> nil) then
    begin
      LLoadAgent := TMultiRestrictionLoadAgent.Create(FAppModules);
      try
        if (LLoadAgent.LoadMultiRestriction(PlanningModelDataObject.CastMultiRestrictionData)) then
          Result := True;
      finally
        lLoadAgent.Free;
      end;
    end;
  except on E: Exception do HandleErrorFunction(E, OPNAME, Result); end;
end;

function TPlanningModelManager.ModelDataLoaded: Boolean;
const OPNAME = 'TPlanningModelManager.ModelDataLoaded';
begin
  Result := inherited ModelDataLoaded;
  try

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TPlanningModelManager.GetFileLineType(AFileObject: TObject; ALineNumber: Integer): string;
const OPNAME = 'TPlanningModelManager.GetFileLineType';
begin
  Result := '';
  try
    if Assigned(AFileObject) and Assigned(ModelData()) and Assigned(PlanningModelDataObject.CastFilesLineTypes) then
      Result := PlanningModelDataObject.CastFilesLineTypes.GetFileLineType(TFileNameObject(AFileObject),ALineNumber);
  except on E: Exception do HandleError(E, OPNAME) end;
end;


function TPlanningModelManager.PopulateDataList(AModelDataChangeType: TModelDataChangeType;
      AList: TList): Boolean;
const OPNAME = 'TPlanningModelManager.PopulateDataList';
begin
  Result := inherited PopulateDataList(AModelDataChangeType,AList);
  try

  except on E: Exception do HandleError(E, OPNAME) end;
end;


function TPlanningModelManager.LoadFieldFileReferencesData: boolean;
const OPNAME = 'TPlanningModelManager.LoadFieldFileReferencesData';
begin
  Result := False;
  try
    Result := FAppModules.FieldProperties.LoadFieldFileReferencesData(ModelName);
  except on E: Exception do HandleError(E, OPNAME) end;
end;


function TPlanningModelManager.LoadAllocationDefinitionData: boolean;
const OPNAME = 'TPlanningModelManager.LoadAllocationDefinitionData';
var
  lLoadAgent : TAllocationDefLoadAgent;
begin
  Result := FALSE;
  try
    lLoadAgent := TAllocationDefLoadAgent.Create(FAppModules);
    try
      if (lLoadAgent.LoadAllocationDefinitions(PlanningModelDataObject.CastAllocationDefinitionsList)) then
        Result := TRUE;
    finally
      lLoadAgent.Free;
    end;
  except on E: Exception do HandleErrorFunction(E, OPNAME, Result); end;
end;

function TPlanningModelManager.LoadSwitchDefinitionData: boolean;
const OPNAME = 'TPlanningModelManager.LoadSwitchDefinitionData';
var
  lLoadAgent : TSwitchDefinitionLoadAgent;
begin
  Result := FALSE;
  try
    lLoadAgent := TSwitchDefinitionLoadAgent.Create(FAppModules);
    try
      if (lLoadAgent.LoadSwitchDefinitions(PlanningModelDataObject.CastSwitchDefinitionsList)) then
        Result := TRUE;
    finally
      lLoadAgent.Free;
    end;
  except on E: Exception do HandleErrorFunction(E, OPNAME, Result); end;
end;


function TPlanningModelManager.ProcessEvent (AEventType : integer;
                                             AData      : TObject): boolean;
const OPNAME = 'TPlanningModelManager.ProcessEvent';
begin
  Result := True;
  try
    case AEventType of
      CmeCreateMine              : DoCreateMine;
      CmeCreateAllocDef           : DoCreateAllocDef;
      CmeDeleteAllocDef           : DoDeleteAllocDef(-1);
      CmeCreateSwitchDef          : DoCreateSwitchDef;
      CmeDeleteSwitchDef          : DoDeleteSwitchDef(-1);
      CmeCreateResTimeCntrl       : DoCreateResTimeControl(-1);
      CmeDeleteResTimeCntrl       : DoDeleteResTimeControl(-1);
      CmeCreateResReplacement     : DoCreateResReplacement(-1);
      CmeCreateChannelTimeCntrl   : DoCreateChannelTimeControl(-1);
      CmeDeleteChannelTimeCntrl   : DoDeleteChannelTimeControl(-1);
      CmeCreateChannelSwitchCntrl : DoCreateChannelSwitchControl(-1);
      CmeDeleteChannelSwitchCntrl : DoDeleteChannelSwitchControl(-1, -1);
      CmeCreateDisbenefitFunction : DoCreateDisbenefitFunction(-1);
      CmeDeleteDisbenefitFunction : DoDeleteDisbenefitFunction(-1);

      CmeCreateReturnFlowChannel  : DoCreateReturnFlowChannel(-1);
      CmeDeleteReturnFlowChannel  : DoDeleteReturnFlowChannel(-1);

      CmeCreateMultiChannelCurtailmentRule : DoCreateMultiResChannelCurtail(-1);
      CmeDeleteMultiChannelCurtailmentRule : DoDeleteMultiResChannelCurtail(-1);
    else
      Result := inherited ProcessEvent(AEventType, AData);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TPlanningModelManager.DoCreateAllocDef : IAllocationDefinition;
const OPNAME = 'TPlanningModelManager.DoCreateAllocDef';
begin
  Result := nil;
  try
    Result :=  PlanningModelDataObject.AllocationDefinitionsList.NewAllocationDefinition;
    if (Result <> nil) then
    begin
      CreateTabSheetTreeViewElement('AllocationDefinition',
                                    'AllocationDefinition',
                                    Result.AllocationDefinitionID,
                                    tvnidAllocationDefinition,
                                    Result.Name,
                                    'ALLOCDEF',
                                    'ALLOCATIONDEFINITION',
                                    True,tvsnAll);
      StudyDataHasChanged(sdccAdd, 'AllocDefName', '0',
        IntToStr(Result.AllocationDefinitionID));
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TPlanningModelManager.DoDeleteAllocDef (AAllocDefID : integer): WordBool;
const OPNAME = 'TPlanningModelManager.DoDeleteAllocDef';
var
  lNode         : TTreeNode;
  lDataObject   : TViewDataTreeNodeData;
begin
  Result := FALSE;
  try
    if (AAllocDefID > 0) then
       Result := PlanningModelDataObject.AllocationDefinitionsList.RemoveAllocationDefinitionWithID(AAllocDefID)
    else
    begin
      if (Assigned(FDataTabSheetManager) AND
          Assigned(FDataTabSheetManager.GridEditorManager)) then
      begin
        LNode := TTreeViewTabSheet(FDataTabSheetManager.GridEditorManager.TabSheet).TreeView.Selected;
        if (Assigned(LNode) AND Assigned(LNode.Data)) then
        begin
          LDataObject := TViewDataTreeNodeData((LNode.Data));
          AAllocDefID := LDataObject.ViewDataNode.Weighting;
          Result := PlanningModelDataObject.AllocationDefinitionsList.RemoveAllocationDefinitionWithID(AAllocDefID);
          if Result then
            DeleteTabSheetTreeNode(lNode);
          SetSystemMenuState;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TPlanningModelManager.DoCreateSlurryDump(
  AMineNumber: integer): IPlanningSlurryDump;
const OPNAME = 'TPlanningModelManager.DoCreateSlurryDump';
var
  LMine        : TPlanningMine;
begin
  Result := nil;
  try
    if (FAppModules.StudyArea.ModelVersion <> '7')  then Exit;
    if(AMineNumber > 0) then
    begin
      LMine := TPlanningMine(PlanningModelDataObject.CastNetworkFeaturesData.CastMineList.CastMineByNodeNumber[AMineNumber]);
      if(LMine <> nil) then
      begin
        Result := LMine.CreateSlurryDump;
        if (Result <> nil) then
          StudyDataHasChanged(sdccAdd, 'Slurry', '0',IntToStr(Result.Identifier));
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TPlanningModelManager.DoCreateSwitchDef : ISwitchDefinition;
const OPNAME = 'TPlanningModelManager.DoCreateSwitchDef';
begin
  Result := nil;
  try
    Result :=  PlanningModelDataObject.SwitchDefinitionsList.NewSwitchDefinition;
    if (Result <> nil) then
    begin
      CreateTabSheetTreeViewElement('SwitchDefinition',
                                    'SwitchDefinition',
                                    Result.SwitchDefID,
                                    tvnidSwitchDefinition,
                                    Result.SwitchDefFileName,
                                    'SWITCHDEF',
                                    'SWITCHDEFINITION',
                                    True,tvsnAll);
      StudyDataHasChanged(sdccAdd, 'SwitchDefFileName', '0',
        IntToStr(Result.SwitchDefID));
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TPlanningModelManager.DoCreateUnderGround(
  AMineNumber: integer): IUnderground;
const OPNAME = 'TPlanningModelManager.DoCreateUnderGround';
var
  LMine : TPlanningMine;
begin
  try
    if (FAppModules.StudyArea.ModelVersion <> '7')  then Exit;
    if(AMineNumber > 0) then
    begin
      LMine := TPlanningMine(PlanningModelDataObject.CastNetworkFeaturesData.CastMineList.CastMineByNodeNumber[AMineNumber]);
      if(LMine <> nil) then
      begin
        Result := LMine.CreateUnderGround;
        if(Result <> nil) then
        begin
          if (Assigned(FDataTabSheetManager) AND Assigned(FDataTabSheetManager.GridEditorManager)) then
          begin
            if (Result.UndergroundDam <> nil) then
              CreateTabSheetTreeViewElement('UndegroundDam',
                                         'FeaturesHeading,Mine,Mine',
                                         Result.UndergroundDam.ReservoirConfigurationData.ReservoirIdentifier,
                                         AMineNumber,
                                         Result.UndergroundDam.ReservoirConfigurationData.ReservoirName,
                                         'RESERVOIR',
                                         'UNDEGROUNDDAM',
                                         False,tvsnAll);
          end;
          StudyDataHasChanged(sdccAdd, 'UndegroundDam', '0',IntToStr(Result.Identifier));
        end;
      end;
    end;
  except on E:Exception do HandleError(E,OPNAME); end;
end;

function TPlanningModelManager.DoDeleteSlurryDump(AMineNumber,
  ASlurryDumpIdentifier: integer): WordBool;
begin

end;

function TPlanningModelManager.DoDeleteSwitchDef (ASwitchDefID : integer): WordBool;
const OPNAME = 'TPlanningModelManager.DoDeleteSwitchDef';
var
  lNode         : TTreeNode;
  lDataObject   : TViewDataTreeNodeData;
begin
  Result := FALSE;
  try
    if (ASwitchDefID > 0) then
       Result := PlanningModelDataObject.SwitchDefinitionsList.RemoveSwitchDefinitionWithID(ASwitchDefID)
    else
    begin
      if (Assigned(FDataTabSheetManager) AND
          Assigned(FDataTabSheetManager.GridEditorManager)) then
      begin
        LNode := TTreeViewTabSheet(FDataTabSheetManager.GridEditorManager.TabSheet).TreeView.Selected;
        if (Assigned(LNode) AND Assigned(LNode.Data)) then
        begin
          LDataObject  := TViewDataTreeNodeData((LNode.Data));
          ASwitchDefID := LDataObject.ViewDataNode.Weighting;
          Result := PlanningModelDataObject.SwitchDefinitionsList.RemoveSwitchDefinitionWithID(ASwitchDefID);
          if Result then
            DeleteTabSheetTreeNode(lNode);
          SetSystemMenuState;
        end;
      end;
    end;
    StudyDataHasChanged(sdccDelete, 'SwitchDefFileName', '0',IntToStr(ASwitchDefID));
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TPlanningModelManager.DoDeleteUnderGround(AMineNumber,
  AUnderGroundIdentifier: integer): WordBool;
begin

end;

function TPlanningModelManager.LoadReservoirTimeControlData: boolean;
const OPNAME = 'TPlanningModelManager.LoadReservoirTimeControlData';
var
  lLoadAgent : TReservoirTimeControlLoadAgent;
begin
  Result := FALSE;
  try
    lLoadAgent := TReservoirTimeControlLoadAgent.Create(FAppModules);
    try
      if (lLoadAgent.LoadReservoirTimeControl(PlanningModelDataObject.CastNetworkElementData.CastReservoirList)) then
        Result := TRUE;
    finally
      lLoadAgent.Free;
    end;
  except on E: Exception do HandleErrorFunction(E, OPNAME, Result); end;
end;

function TPlanningModelManager.LoadGrowthFactorData: boolean;
const OPNAME = 'TPlanningModelManager.LoadGrowthFactorData';
var
  LLoadAgent : TGrowthFactorDataLoadAgent;
begin
  Result := FALSE;
  try
    LLoadAgent := TGrowthFactorDataLoadAgent.Create(FAppModules);
    try
      if (LLoadAgent.LoadGrowthFactors(PlanningModelDataObject.CastGrowthFactors)) then
        Result := TRUE;
    finally
      lLoadAgent.Free;
    end;
  except on E: Exception do HandleErrorFunction(E, OPNAME, Result); end;
end;

function TPlanningModelManager.LoadDisbenefitFunctionData: boolean;
const OPNAME = 'TPlanningModelManager.LoadDisbenefitFunctionData';
var
  LLoadAgent : TDisbenefitFunctionDefinitionLoadAgent;
begin
  Result := FALSE;
  try
    LLoadAgent := TDisbenefitFunctionDefinitionLoadAgent.Create(FAppModules);
    try
      if (LLoadAgent.LoadDisbenefitFunction(PlanningModelDataObject.CastNetworkElementData.CastChannelList)) then
        Result := TRUE;
    finally
      lLoadAgent.Free;
    end;
  except on E: Exception do HandleErrorFunction(E, OPNAME, Result); end;
end;

function TPlanningModelManager.LoadReturnFlowChannelData : boolean;
const OPNAME = 'TPlanningModelManager.LoadReturnFlowChannelData';
var
  LLoadAgent : TReturnFlowChannelDataLoadAgent;
begin
  Result := False;
  try
    LLoadAgent := TReturnFlowChannelDataLoadAgent.Create(FAppModules);
    try
      if (LLoadAgent.LoadReturnFlowChannelData(PlanningModelDataObject.CastReturnFlowChannel)) then
        Result := True;
    finally
      lLoadAgent.Free;
    end;
  except on E: Exception do HandleErrorFunction(E, OPNAME, Result); end;
end;

function TPlanningModelManager.LoadTariffCalculationData: Boolean;
const OPNAME = 'TPlanningModelManager.LoadTariffCalculationData';
var
  LLoadAgent : TTariffCalculationLoadAgent;
begin
  Result := False;
  try
    LLoadAgent := TTariffCalculationLoadAgent.Create(FAppModules);
    try
      if (LLoadAgent.LoadTariffCalculationData(PlanningModelDataObject.CastTariffCalculationData)) then
        Result := True;
    finally
      lLoadAgent.Free;
    end;
  except on E: Exception do HandleErrorFunction(E, OPNAME, Result); end;
end;

function TPlanningModelManager.LoadWQConstriantData: Boolean;
const OPNAME = 'TPlanningModelManager.LoadWQConstriantData';
var
  LLoadAgent : TWQConstraintLoadAgent;
begin
  Result := False;
  try
    LLoadAgent := TWQConstraintLoadAgent.Create(FAppModules);
    try
      if (LLoadAgent.LoadWQConstriantData(PlanningModelDataObject.CastWQConstriantData)) then
        Result := True;
    finally
      lLoadAgent.Free;
    end;
  except on E: Exception do HandleErrorFunction(E, OPNAME, Result); end;
end;


function TPlanningModelManager.DoCreateResTimeControl (AReservoirNr : integer) : IReservoirTimeControl;
const OPNAME = 'TPlanningModelManager.DoCreateResTimeControl';
var
  lNode          : TTreeNode;
  lDataObject    : TViewDataTreeNodeData;
  lReservoirList : IReservoirDataList;
  lReservoir     : IReservoirData;
  lTimeControl   : IReservoirTimeControl;
  lEndYear       : integer;
  lEndMonth      : integer;
  lStartMonth    : integer;
  lCalStartMonth : integer;
begin
  Result := nil;
  try
    lNode := nil;
    if (AReservoirNr <= 0) then
    begin
      if (Assigned(FDataTabSheetManager) AND
          Assigned(FDataTabSheetManager.GridEditorManager)) then
      begin
        lNode := TTreeViewTabSheet(FDataTabSheetManager.GridEditorManager.TabSheet).TreeView.Selected;
        if (Assigned(lNode) AND Assigned(lNode.Data)) then
        begin
          lDataObject    := TViewDataTreeNodeData((lNode.Data));
          AReservoirNr   := lDataObject.ViewDataNode.Weighting;
        end;
      end;
    end;
    if (AReservoirNr > 0) then
    begin
      lReservoirList := YieldModelDataObject.
                          NetworkElementData.ReservoirList;
      lReservoir     := lReservoirList.ReservoirByIdentifier[AReservoirNr];
      if (lReservoir <> nil) then
      begin
        lTimeControl   := lReservoir.NewTimeControl;
        lCalStartMonth := FAppModules.StudyArea.CalendarStartMonth;
        lStartMonth    := YieldModelDataObject.CastRunConfigurationData.StartMonthNumber;
        lStartMonth    := (lCalStartMonth - 1 + lStartMonth) mod 12;
        lTimeControl.StartYear  := YieldModelDataObject.CastRunConfigurationData.StartYearOther;
        lTimeControl.StartMonth := lStartMonth;
        lEndYear := 0;
        lEndMonth := 0;
        if (YieldModelDataObject.CastRunConfigurationData.GetEndYearAndMonth(lEndYear, lEndMonth)) then
        begin
          lEndMonth := (lCalStartMonth - 1 + lEndMonth) mod 12;
          lTimeControl.EndYear := lEndYear;
          lTimeControl.EndMonth := lEndMonth;
        end;
        Result := lTimeControl;
      end;
      if (lNode <> nil) then
        TTreeViewTabSheet(FDataTabSheetManager.GridEditorManager.TabSheet).ReselectTreeNode(lNode);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TPlanningModelManager.DoDeleteResTimeControl (AReservoirNr : Integer): WordBool;
const OPNAME = 'TPlanningModelManager.DoDeleteResTimeControl';
var
  lNode          : TTreeNode;
  lDataObject    : TViewDataTreeNodeData;
  lReservoirList : IReservoirDataList;
  lReservoir     : IReservoirData;
begin
  Result := FALSE;
  try
    if (AReservoirNr > 0) then
    begin
      lReservoirList := YieldModelDataObject.
                          NetworkElementData.ReservoirList;
      lReservoir     := lReservoirList.ReservoirByIdentifier[AReservoirNr];
      if (lReservoir <> nil) then
        Result := lReservoir.RemoveTimeControl;
    end
    else
    begin
      if (Assigned(FDataTabSheetManager) AND
          Assigned(FDataTabSheetManager.GridEditorManager)) then
      begin
        lNode := TTreeViewTabSheet(FDataTabSheetManager.GridEditorManager.TabSheet).TreeView.Selected;
        if (Assigned(lNode) AND Assigned(lNode.Data)) then
        begin
          lDataObject    := TViewDataTreeNodeData((lNode.Data));
          AReservoirNr   := lDataObject.ViewDataNode.Weighting;
          lReservoirList := YieldModelDataObject.
                              NetworkElementData.ReservoirList;
          lReservoir     := lReservoirList.ReservoirByIdentifier[AReservoirNr];
          if (lReservoir <> nil) then
            Result := lReservoir.RemoveTimeControl;
          TTreeViewTabSheet(FDataTabSheetManager.GridEditorManager.TabSheet).ReselectTreeNode(lNode);
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TPlanningModelManager.DoCreateResReplacement (AReservoirNr : integer) : IReservoirData;
const OPNAME = 'TPlanningModelManager.DoCreateResReplacement';
var
  lNode          : TTreeNode;
  lDataObject    : TViewDataTreeNodeData;
  lReservoirList : IReservoirDataList;
  lReservoir     : IReservoirData;
  lClone         : IReservoirData;
  lTimeControl   : IReservoirTimeControl;
begin
  Result := nil;
  try
    if (AReservoirNr <= 0) then
    begin
      if (Assigned(FDataTabSheetManager) AND
          Assigned(FDataTabSheetManager.GridEditorManager)) then
      begin
        lNode := TTreeViewTabSheet(FDataTabSheetManager.GridEditorManager.TabSheet).TreeView.Selected;
        if (Assigned(lNode) AND Assigned(lNode.Data)) then
        begin
          lDataObject    := TViewDataTreeNodeData((lNode.Data));
          AReservoirNr   := lDataObject.ViewDataNode.Weighting;
        end;
      end;
    end;
    if (AReservoirNr > 0) then
    begin
      lReservoirList := YieldModelDataObject.
                          NetworkElementData.ReservoirList;
      lReservoir     := lReservoirList.ReservoirByIdentifier[AReservoirNr];
      if (lReservoir <> nil) then
      begin
        lClone       := lReservoir.Clone;
        lTimeControl := lClone.NewTimeControl;
        lTimeControl.BaseNodeNumber := lReservoir.ReservoirConfigurationData.ReservoirIdentifier;
        lReservoir.TimeControl.AddReplacement(lClone.ReservoirConfigurationData.ReservoirIdentifier);
      end;
      CreateTabSheetTreeViewElement('AReservoir',
                                    'AReservoir',
                                    lClone.ReservoirConfigurationData.ReservoirIdentifier,
                                    tvnidAReservoir,
                                    lClone.ReservoirConfigurationData.ReservoirName,
                                    'RESERVOIR',
                                    'RESERVOIR',
                                    True,tvsnAll);
      Result := lClone;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TPlanningModelManager.DoCreateChannelTariff(AChannelNr: integer): WordBool;
const OPNAME = 'TPlanningModelManager.DoCreateChannelTariff';
begin
  Result := False;
  try
    Result := PlanningModelDataObject.CastTariffCalculationData.NewChannelTariff(AChannelNr);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TPlanningModelManager.DoDeleteChannelTariff(AChannelNr: integer): WordBool;
const OPNAME = 'TPlanningModelManager.DoDeleteChannelTariff';
begin
  Result := False;
  try
    Result := PlanningModelDataObject.CastTariffCalculationData.RemoveChannelTariffByChannelNumber(AChannelNr);
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TPlanningModelManager.DoCreateChannelTimeControl (AChannelNr : integer) : IChannelTimeControl;
const OPNAME = 'TPlanningModelManager.DoCreateChannelTimeControl';
var
  lNode          : TTreeNode;
  lDataObject    : TViewDataTreeNodeData;
  lChannelList   : IChannelList;
  lChannel       : IGeneralFlowChannel;
  lTimeControl   : IChannelTimeControl;
  lEndYear       : integer;
  lEndMonth      : integer;
  lStartMonth    : integer;
  lCalStartMonth : integer;
begin
  Result := nil;
  try
    lNode := nil;
    if (AChannelNr <= 0) then
    begin
      if (Assigned(FDataTabSheetManager) AND
          Assigned(FDataTabSheetManager.GridEditorManager)) then
      begin
        lNode := TTreeViewTabSheet(FDataTabSheetManager.GridEditorManager.TabSheet).TreeView.Selected;
        if (Assigned(lNode) AND Assigned(lNode.Data)) then
        begin
          lDataObject  := TViewDataTreeNodeData((lNode.Data));
          AChannelNr   := lDataObject.ViewDataNode.Weighting;
        end;
      end;
    end;
    if (AChannelNr > 0) then
    begin
      lChannelList := YieldModelDataObject.
                          NetworkElementData.ChannelList;
      lChannel     := lChannelList.ChannelByChannelNumber[AChannelNr];
      if (lChannel <> nil) then
      begin
        lTimeControl   := lChannel.NewTimeControl;
        lCalStartMonth := FAppModules.StudyArea.CalendarStartMonth;
        lStartMonth    := YieldModelDataObject.CastRunConfigurationData.StartMonthNumber;
        lStartMonth    := (lCalStartMonth - 1 + lStartMonth) mod 12;
        lTimeControl.StartYear  := YieldModelDataObject.CastRunConfigurationData.StartYearOther;
        lTimeControl.StartMonth := lStartMonth;
        lEndYear := 0;
        lEndMonth := 0;
        if (YieldModelDataObject.CastRunConfigurationData.GetEndYearAndMonth(lEndYear, lEndMonth)) then
        begin
          lEndMonth := (lCalStartMonth - 1 + lEndMonth) mod 12;
          lTimeControl.EndYear := lEndYear;
          lTimeControl.EndMonth := lEndMonth;
        end;
        Result := lTimeControl;
      end;
      if (lNode <> nil) then
        TTreeViewTabSheet(FDataTabSheetManager.GridEditorManager.TabSheet).ReselectTreeNode(lNode);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TPlanningModelManager.DoDeleteChannelTimeControl (AChannelNr : Integer): WordBool;
const OPNAME = 'TPlanningModelManager.DoDeleteChannelTimeControl';
var
  lNode          : TTreeNode;
  lDataObject    : TViewDataTreeNodeData;
  lChannelList   : IChannelList;
  lChannel       : IGeneralFlowChannel;
begin
  Result := FALSE;
  try
    if (AChannelNr > 0) then
    begin
      lChannelList := YieldModelDataObject.
                          NetworkElementData.ChannelList;
      lChannel     := lChannelList.ChannelByChannelNumber[AChannelNr];
      if (lChannel <> nil) then
        Result := lChannel.RemoveTimeControl;
    end
    else
    begin
      if (Assigned(FDataTabSheetManager) AND
          Assigned(FDataTabSheetManager.GridEditorManager)) then
      begin
        lNode := TTreeViewTabSheet(FDataTabSheetManager.GridEditorManager.TabSheet).TreeView.Selected;
        if (Assigned(lNode) AND Assigned(lNode.Data)) then
        begin
          lDataObject  := TViewDataTreeNodeData((lNode.Data));
          AChannelNr   := lDataObject.ViewDataNode.Weighting;
          lChannelList := YieldModelDataObject.
                              NetworkElementData.ChannelList;
          lChannel     := lChannelList.ChannelByChannelNumber[AChannelNr];
          if (lChannel <> nil) and (lChannel.TimeControl <> nil) then
            Result := lChannel.RemoveTimeControl;
          TTreeViewTabSheet(FDataTabSheetManager.GridEditorManager.TabSheet).ReselectTreeNode(lNode);
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TPlanningModelManager.DoCreateChannelSwitchControl (AChannelNr : integer) : IChannelSwitchControl;
const OPNAME = 'TPlanningModelManager.DoCreateChannelSwitchControl';
var
  lNode          : TTreeNode;
  lDataObject    : TViewDataTreeNodeData;
  lChannelList   : IChannelList;
  lChannel       : IGeneralFlowChannel;
  lSwitchControl : IChannelSwitchControl;
begin
  Result := nil;
  try
    lNode := nil;
    if (AChannelNr <= 0) then
    begin
      if (Assigned(FDataTabSheetManager) AND
          Assigned(FDataTabSheetManager.GridEditorManager)) then
      begin
        lNode := TTreeViewTabSheet(FDataTabSheetManager.GridEditorManager.TabSheet).TreeView.Selected;
        if (Assigned(lNode) AND Assigned(lNode.Data)) then
        begin
          lDataObject  := TViewDataTreeNodeData((lNode.Data));
          AChannelNr   := lDataObject.ViewDataNode.Weighting;
        end;
      end;
    end;
    if (AChannelNr > 0) then
    begin
      lChannelList := YieldModelDataObject.
                          NetworkElementData.ChannelList;
      lChannel     := lChannelList.ChannelByIdentifier[AChannelNr];
      if (lChannel <> nil) then
      begin
        lSwitchControl := lChannel.NewSwitchControl;
        Result := lSwitchControl;
      end;
      if (lNode <> nil) then
        TTreeViewTabSheet(FDataTabSheetManager.GridEditorManager.TabSheet).ReselectTreeNode(lNode);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TPlanningModelManager.DoDeleteChannel(
  AChannelNumber: Integer): WordBool;
const OPNAME = 'TPlanningModelManager.DoDeleteChannel';
begin
  Result  := False;
  try
     Result := DoDeleteMultiResChannelCurtail(-1) and inherited DoDeleteChannel(-1);
  except on E: Exception do HandleError(E,OPNAME) end;
end;

function TPlanningModelManager.DoDeleteChannelSwitchControl (AChannelNr   : Integer;
                                                             ASwitchDefID : integer): WordBool;
const OPNAME = 'TPlanningModelManager.DoDeleteChannelSwitchControl';
var
  lNode          : TTreeNode;
  lDataObject    : TViewDataTreeNodeData;
  lChannelList   : IChannelList;
  lChannel       : IGeneralFlowChannel;
begin
  Result := FALSE;
  try
    if (AChannelNr > 0) AND (ASwitchDefID > 0) then
    begin
      lChannelList := YieldModelDataObject.
                          NetworkElementData.ChannelList;
      lChannel     := lChannelList.ChannelByIdentifier[AChannelNr];
      if (lChannel <> nil) then
        Result := lChannel.RemoveSwitchControl(ASwitchDefID);
    end
    else
    begin
      if (Assigned(FDataTabSheetManager) AND
          Assigned(FDataTabSheetManager.GridEditorManager)) then
      begin
        lNode := TTreeViewTabSheet(FDataTabSheetManager.GridEditorManager.TabSheet).TreeView.Selected;
        if (Assigned(lNode) AND Assigned(lNode.Data)) then
        begin
          lDataObject  := TViewDataTreeNodeData((lNode.Data));
          AChannelNr   := lDataObject.ViewDataNode.Weighting;
          lChannelList := YieldModelDataObject.NetworkElementData.ChannelList;
          lChannel     := lChannelList.ChannelByIdentifier[AChannelNr];
          if (lChannel <> nil) then
          begin
            ASwitchDefID := lChannel.SelectedSwitchControlID;
            if (ASwitchDefID <> 0) then
              Result := lChannel.RemoveSwitchControl(ASwitchDefID);
          end;
          TTreeViewTabSheet(FDataTabSheetManager.GridEditorManager.TabSheet).ReselectTreeNode(lNode);
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TPlanningModelManager.StudyDataHasChanged(AContext: TChangeContext; AFieldName, AOldValue, ANewValue: string): boolean;
const OPNAME = 'TPlanningModelManager.StudyDataHasChanged';
var
  LTabSheet: TDataViewerSheet;
  //LNode    : TTReeNode;
begin
  Result := False;
  try
    if(AContext = sdccEdit) and ((AFieldName = 'AllocDefStartYear') or (AFieldName = 'AllocDefStartMonth')) then
    begin
      Result := True;
      if Assigned(FDataTabSheetManager) and
         Assigned(FDataTabSheetManager.GridEditorManager) and
         Assigned(FDataTabSheetManager.GridEditorManager.TabSheet) then
      begin
        FViewDataManager.StudyHasChanged;
        LTabSheet := TDataViewerSheet(FDataTabSheetManager.GridEditorManager.TabSheet);
        LTabSheet.PopulateTreeView;
        LTabSheet.LanguageHasChanged;
        {LNode := LTabSheet.MainNodeByName['ALLOCATIONDEFINITION'];
        if Assigned(LNode) then
           LTabSheet.TreeView.Selected := LNode
        else
           LTabSheet.TreeView.Selected := nil;}
      end;
    end
    else
      Result := inherited StudyDataHasChanged(AContext,AFieldName, AOldValue, ANewValue);


     if (FFileSelectionManager <> nil) and  (FFileEditManager <> nil) and (FFileEditManager.TabSheet <> nil) then
     begin
        Result := Result and FFileSelectionManager.StudyDataHasChanged(AContext,AFieldName,AOldValue,ANewValue);
        Result := FFileSelectionManager.PopulateTreeView(TTreeViewTabSheet(FFileEditManager.TabSheet).TreeView,
                                                    YieldModelDataObject.CastFileNamesObject);
         FFileEditManager.TabSheet.LanguageHasChanged;
     end;

  except on E: Exception do HandleError(E, OPNAME) end;
end;


function TPlanningModelManager.DoCreateDisbenefitFunction(AChannelNr: integer): IDisbenefitFunctionDefinition;
const OPNAME = 'TPlanningModelManager.DoCreateDisbenefitFunction';
var
  lNode               : TTreeNode;
  lDataObject         : TViewDataTreeNodeData;
  lChannelList        : IChannelList;
  lChannel            : IGeneralFlowChannel;
  lDisbenefitFunction : IDisbenefitFunctionDefinition;
begin
  Result := nil;
  try
    lNode := nil;
    if (AChannelNr <= 0) then
    begin
      if (Assigned(FDataTabSheetManager) AND
          Assigned(FDataTabSheetManager.GridEditorManager)) then
      begin
        lNode := TTreeViewTabSheet(FDataTabSheetManager.GridEditorManager.TabSheet).TreeView.Selected;
        if (Assigned(lNode) AND Assigned(lNode.Data)) then
        begin
          lDataObject := TViewDataTreeNodeData((lNode.Data));
          AChannelNr  := lDataObject.ViewDataNode.Weighting;
        end;
      end;
    end;
    if (AChannelNr > 0) then
    begin
      lChannelList := YieldModelDataObject.
                          NetworkElementData.ChannelList;
      lChannel     := lChannelList.ChannelByChannelNumber[AChannelNr];
      if (lChannel <> nil) then
      begin
        lDisbenefitFunction := lChannel.NewDisbenefitFunction;
        Result := lDisbenefitFunction;
      end;
      if (lNode <> nil) then
        TTreeViewTabSheet(FDataTabSheetManager.GridEditorManager.TabSheet).ReselectTreeNode(lNode);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TPlanningModelManager.DoDeleteDisbenefitFunction(AChannelNr: integer): WordBool;
const OPNAME = 'TPlanningModelManager.DoDeleteDisbenefitFunction';
var
  lNode          : TTreeNode;
  lDataObject    : TViewDataTreeNodeData;
  lChannelList   : IChannelList;
  lChannel       : IGeneralFlowChannel;
begin
  Result := FALSE;
  try
    if (AChannelNr > 0) then
    begin
      lChannelList := YieldModelDataObject.
                          NetworkElementData.ChannelList;
      lChannel     := lChannelList.ChannelByIdentifier[AChannelNr];
      if (lChannel <> nil) then
        Result := lChannel.RemoveDisbenefitFunction;
    end
    else
    begin
      if (Assigned(FDataTabSheetManager) AND
          Assigned(FDataTabSheetManager.GridEditorManager)) then
      begin
        lNode := TTreeViewTabSheet(FDataTabSheetManager.GridEditorManager.TabSheet).TreeView.Selected;
        if (Assigned(lNode) AND Assigned(lNode.Data)) then
        begin
          lDataObject  := TViewDataTreeNodeData((lNode.Data));
          AChannelNr   := lDataObject.ViewDataNode.Weighting;
          lChannelList := YieldModelDataObject.
                              NetworkElementData.ChannelList;
          lChannel     := lChannelList.ChannelByChannelNumber[AChannelNr];
          if (lChannel <> nil) then
            Result := lChannel.RemoveDisbenefitFunction;
          TTreeViewTabSheet(FDataTabSheetManager.GridEditorManager.TabSheet).ReselectTreeNode(lNode);
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TPlanningModelManager.DoCreateReturnFlowChannel(AChannelNr : integer): IReturnFlowChannel;
const OPNAME = 'TPlanningModelManager.DoCreateReturnFlowChannel';
var
  lNode               : TTreeNode;
  lDataObject         : TViewDataTreeNodeData;
  lChannelList        : IChannelList;
  lChannel            : IGeneralFlowChannel;
  LReturnFlowChannel  : IReturnFlowChannel;
  LReturnFlowChannelList : IReturnFlowChannelData;
begin
  Result := nil;
  try
    lNode := nil;
    if (AChannelNr <= 0) then
    begin
      if (Assigned(FDataTabSheetManager) AND
          Assigned(FDataTabSheetManager.GridEditorManager)) then
      begin
        lNode := TTreeViewTabSheet(FDataTabSheetManager.GridEditorManager.TabSheet).TreeView.Selected;
        if (Assigned(lNode) AND Assigned(lNode.Data)) then
        begin
          lDataObject := TViewDataTreeNodeData((lNode.Data));
          AChannelNr  := lDataObject.ViewDataNode.Weighting;
        end;
      end;
    end;
    if (AChannelNr > 0) then
    begin
      lChannelList := YieldModelDataObject.
                          NetworkElementData.ChannelList;
      lChannel     := lChannelList.ChannelByChannelNumber[AChannelNr];
      if (lChannel <> nil) and (lChannel.ReturnFlowChannel = nil) then
      begin
        LReturnFlowChannelList := PlanningModelDataObject.CastReturnFlowChannel;
        if (LReturnFlowChannelList.ReturnFlowChannelCount < 11) then
        begin
          LReturnFlowChannel := LChannel.NewReturnFlowChannel;
          Result := LReturnFlowChannel;
        end
        else
          MessageDlg(FAppModules.Language.GetString('Message.TotalNumberOfReturnFlow'), mtInformation, [mbOk], 0);
      end
      else
      if (lChannel <> nil) and (lChannel.ReturnFlowChannel <> nil) then
        MessageDlg(FAppModules.Language.GetString('Message.FlowChannelDataExist'), mtInformation, [mbOk], 0);

      if (lNode <> nil) then
        TTreeViewTabSheet(FDataTabSheetManager.GridEditorManager.TabSheet).ReselectTreeNode(lNode);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


function TPlanningModelManager.DoDeleteReturnFlowChannel (AChannelNr : integer): WordBool;
const OPNAME = 'TPlanningModelManager.DoDeleteReturnFlowChannel';
var
  lNode          : TTreeNode;
  lDataObject    : TViewDataTreeNodeData;
  lChannelList   : IChannelList;
  lChannel       : IGeneralFlowChannel;
begin
  Result := FALSE;
  try
    if (AChannelNr > 0) then
    begin
      lChannelList := YieldModelDataObject.
                          NetworkElementData.ChannelList;
      lChannel     := lChannelList.ChannelByIdentifier[AChannelNr];
      if (lChannel <> nil) and (lChannel.ReturnFlowChannel <> nil)then
        Result := lChannel.RemoveReturnFlowChannel;
    end
    else
    begin
      if (Assigned(FDataTabSheetManager) AND
          Assigned(FDataTabSheetManager.GridEditorManager)) then
      begin
        lNode := TTreeViewTabSheet(FDataTabSheetManager.GridEditorManager.TabSheet).TreeView.Selected;
        if (Assigned(lNode) AND Assigned(lNode.Data)) then
        begin
          lDataObject  := TViewDataTreeNodeData((lNode.Data));
          AChannelNr   := lDataObject.ViewDataNode.Weighting;
          lChannelList := YieldModelDataObject.
                              NetworkElementData.ChannelList;
          lChannel     := lChannelList.ChannelByChannelNumber[AChannelNr];
          if (lChannel <> nil) and (lChannel.ReturnFlowChannel <> nil)then
            Result := lChannel.RemoveReturnFlowChannel;
          TTreeViewTabSheet(FDataTabSheetManager.GridEditorManager.TabSheet).ReselectTreeNode(lNode);
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{function TPlanningModelManager.DoStationFilter(AChangeListID: integer): WordBool;
const OPNAME = 'TPlanningModelManager.DoStationFilter';
begin
  Result := True;
  try //Do not insert code here
    Result := False;
  except on E: Exception do HandleError(E, OPNAME) end;
end;}

function TPlanningModelManager.DoCreateMine: IPlanningMine;
const OPNAME = 'TPlanningModelManager.DoCreateMine';
begin
  begin
  Result := nil;
  try
    FMineCreated := True;
    if (FAppModules.StudyArea.ModelVersion <> '7')  then Exit;
    Result := IPlanningMine(PlanningModelDataObject.CastNetworkFeaturesData.CastMineList.CreateMine);
    if (Result <> nil) then
    begin
      CreateTabSheetTreeViewElement('Mine',
                                 'FeaturesHeading,Mine',
                                 (Result as IPlanningMine).NodeNumber,tvnidMine,
                                 (Result as IPlanningMine).MineName,
                                 'Mine',
                                 'Mine',True,tvsnAll);
      AddMineNodes(Result);
      StudyDataHasChanged(sdccAdd, 'Mine', '0',IntToStr((Result as IMine).NodeNumber));
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end;

function TPlanningModelManager.DoCreateMultiResChannelCurtail(AChannelNr: Integer): IMultiResMultiChannelCurtail;
const OPNAME = 'TPlanningModelManager.DoCreateMultiResChannelCurtail';
var
  lNode               : TTreeNode;
  lDataObject         : TViewDataTreeNodeData;
  lChannelList        : IChannelList;
  lChannel            : IGeneralFlowChannel;
  LMultiChannelCurtailList  : IMultiResMultiChannelCurtailList;
  LMultiChannelCurtail : IMultiResMultiChannelCurtail;
begin
  Result := nil;
  try
    lNode := nil;
    if (AChannelNr <= 0) then
    begin
      if (Assigned(FDataTabSheetManager) AND
          Assigned(FDataTabSheetManager.GridEditorManager)) then
      begin
        lNode := TTreeViewTabSheet(FDataTabSheetManager.GridEditorManager.TabSheet).TreeView.Selected;
        if (Assigned(lNode) AND Assigned(lNode.Data)) then
        begin
          lDataObject := TViewDataTreeNodeData((lNode.Data));
          AChannelNr  := lDataObject.ViewDataNode.Weighting;
        end;
      end;
    end;
    if (AChannelNr > 0) then
    begin
      lChannelList := YieldModelDataObject.
                          NetworkElementData.ChannelList;
      lChannel     := lChannelList.ChannelByChannelNumber[AChannelNr];
      if (lChannel <> nil) and (lChannel.MultiResChannelCurtailByChannelNo[AChannelNr] = nil) then
      begin
        LMultiChannelCurtailList := TPlanningModelDataObject(FAppModules.Model.ModelData).CastMultiRestrictionData;
        LMultiChannelCurtail := LMultiChannelCurtailList.NewRestriction(AChannelNr);
      end
      else
      if (lChannel <> nil) and (lChannel.MultiResChannelCurtailByChannelNo[AChannelNr] <> nil) then
        MessageDlg(FAppModules.Language.GetString('Message.MultiChannelRestrictionDataExist'), mtInformation, [mbOk], 0);

      if (lNode <> nil) then
        TTreeViewTabSheet(FDataTabSheetManager.GridEditorManager.TabSheet).ReselectTreeNode(lNode);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TPlanningModelManager.DoCreateOpenCast(
  AMineNumber: integer): IPlanningOpenCast;
const OPNAME = ' TPlanningModelManager.DoCreateOpenCast';
var
  LMine        : TPlanningMine;
begin
  Result := nil;
  try
    if (FAppModules.StudyArea.ModelVersion <> '7')  then Exit;
    if(AMineNumber > 0) then
    begin
      LMine := TPlanningMine(PlanningModelDataObject.CastNetworkFeaturesData.CastMineList.CastMinebyNodeNumber[AMineNumber]);
      if(LMine <> nil) then
      begin
        Result := LMine.CreateOpenCast;
        if (Result <> nil) then
          StudyDataHasChanged(sdccAdd, 'OpenCast', '0',IntToStr(Result.Identifier));
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TPlanningModelManager.DoDeleteMine(AMineNumber: Integer): WordBool;
const OPNAME = 'TPlanningModelManager.DoDeleteMine';
var
  lNode        : TTreeNode;
  lDataObject  : TViewDataTreeNodeData;
begin
  Result  := FALSE;
  try
    if (FAppModules.StudyArea.ModelVersion <> '7')  then Exit;
    if (AMineNumber > 0) then
    begin
      Result := PlanningModelDataObject.CastNetworkFeaturesData.CastMineList.RemoveMine(AMineNumber);
    end
    else
    begin
      if (Assigned(FDataTabSheetManager) AND Assigned(FDataTabSheetManager.GridEditorManager)) then
      begin
        LNode := TTreeViewTabSheet(FDataTabSheetManager.GridEditorManager.TabSheet).TreeView.Selected;
        if (Assigned(LNode) AND Assigned(LNode.Data)) then
        begin
          LDataObject     := TViewDataTreeNodeData((LNode.Data));
          AMineNumber := LDataObject.ViewDataNode.Weighting;
          Result := YieldModelDataObject.CastNetworkFeaturesData.CastMineList.RemoveMine(AMineNumber);
          if (Result) then
            DeleteTabSheetTreeNode(lNode);
          SetSystemMenuState;
        end;
      end;
    end;
    if Result then
      StudyDataHasChanged(sdccDelete, 'Mine', '0',IntToStr(AMineNumber));

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TPlanningModelManager.DoDeleteMultiResChannelCurtail(AChannelNr: Integer): wordBool;
const OPNAME = 'TPlanningModelManager.DoDeleteReturnFlowChannel';
var
  lNode          : TTreeNode;
  lDataObject    : TViewDataTreeNodeData;
  lChannelList   : IChannelList;
  lChannel       : IGeneralFlowChannel;
 // LMultiChannelCurtailList  : TMultiResChannelCurtailmentList;
  LMultiChannelCurtail  : IMultiResMultiChannelCurtail;
begin
  Result := FALSE;
  try

    if (AChannelNr > 0) then
    begin
      LChannelList := YieldModelDataObject.
                          NetworkElementData.ChannelList;
      LChannel     := lChannelList.ChannelByIdentifier[AChannelNr];
      if (LChannel <> nil) and (LChannel.MultiResChannelCurtailByChannelNo[AChannelNr] <> nil)then
      begin
       // LMultiChannelCurtailList := TPlanningModelDataObject(FAppModules.Model.ModelData).CastMultiRestrictionData;
        Result := TPlanningModelDataObject(FAppModules.Model.ModelData).CastMultiRestrictionData.RemoveRestriction(AChannelNr);
      end;

    end

    else
    begin
      if (Assigned(FDataTabSheetManager) AND
          Assigned(FDataTabSheetManager.GridEditorManager)) then
      begin
        lNode := TTreeViewTabSheet(FDataTabSheetManager.GridEditorManager.TabSheet).TreeView.Selected;
        if (Assigned(lNode) AND Assigned(lNode.Data)) then
        begin
          lDataObject  := TViewDataTreeNodeData((lNode.Data));
          AChannelNr   := lDataObject.ViewDataNode.Weighting;
          lChannelList := YieldModelDataObject.
                              NetworkElementData.ChannelList;

          lChannel     := lChannelList.ChannelByChannelNumber[AChannelNr];

         // LMultiChannelCurtailList := TPlanningModelDataObject(FAppModules.Model.ModelData).CastMultiRestrictionData;
          LMultiChannelCurtail :=  TPlanningModelDataObject(FAppModules.Model.ModelData).CastMultiRestrictionData.
                                   RestrictionByChannelNo[lChannel.ChannelNumber];
         // if (lChannel <> nil) and (LChannel.MultiResChannelCurtailByChannelNo[AChannelNr] <> nil)then
          if LMultiChannelCurtail <> nil then
          begin
           // LMultiChannelCurtailList := TPlanningModelDataObject(FAppModules.Model.ModelData).CastMultiRestrictionData;
            Result := TPlanningModelDataObject(FAppModules.Model.ModelData).CastMultiRestrictionData.RemoveRestriction(AChannelNr);
          end;

          if lNode <> nil then
            TTreeViewTabSheet(FDataTabSheetManager.GridEditorManager.TabSheet).ReselectTreeNode(lNode);

        end;
      end;
    end;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TPlanningModelManager.DoDeleteOpenCast(AMineNumber,
  AOpenCastIdentifier: integer): WordBool;
begin

end;

function TPlanningModelManager.LoadMiningData: boolean;
const OPNAME ='TPlanningModelManager.LoadMiningData';
var LLoadAgent: TPlanningMineLoadAgent;
begin
  Result := false;
  try
    LLoadAgent := TPlanningMineLoadAgent.Create(FAppModules);
    try
      Result := LLoadAgent.ConstructData(PlanningModelDataObject.CastNetworkFeaturesData.CastMineList,
                             PlanningModelDataObject.CastNetworkFeaturesData.CastMineSubCatchmentList);
    finally
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleError(E,OPNAME)  end;
end;
end.
