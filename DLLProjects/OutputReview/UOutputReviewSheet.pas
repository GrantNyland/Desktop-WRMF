//
//
//  UNIT      : Contains TOutputReviewSheet Class
//  AUTHOR    : Dziedzi Ramulondi (ARIVIA)
//  DATE      : 2005/05/03
//  COPYRIGHT : Copyright © 2005 DWAF
//
//
unit UOutputReviewSheet;

interface

uses
  // Delphi
  Classes,
  VCL.Controls,
  VCL.ComCtrls,
  // DWAF
  UAbstractObject,
  UViewDataItem,
  UDataViewerSheet,
  UDynamicTreeViewTabSheet,
  UAbstractGridData,
  UGridActionObject,
  UAbstractComponent,
  UHelpContexts;

type
  TOutputReviewSheet = class(TDynamicTreeViewTabSheet)
  protected
    FViewersCreated: boolean;
    procedure CreateMemberObjects; override;
    procedure DoTreeNodeAboutToChange(ASender: TObject; ANode: TTreeNode; var AAllowChange: Boolean); override;
    procedure DoTreeNodeHasChanged(ASender: TObject; ANode: TTreeNode); override;
    procedure PopulateDataViewer(ADataObject: TViewDataTreeNodeData); override;
    procedure AssignHelpContext; override;
    function GetToolBar: TAbstractToolBar; override;
  public
    procedure DoOnHint; override;
    procedure DoCopyToCLipboard; override;
    procedure DoExport(AFileName: string = ''); override;
    procedure DoPrint; override;
    function CanCopyToCLipboard: boolean; override;
    function CanExport: boolean; override;
    function CanPrint: boolean; override;

    function StudyHasChanged: boolean; override;
    function LanguageHasChanged: boolean; override;
    function StudyDataHasChanged(AContext: TChangeContext; AFieldName,AOldValue,ANewValue: string): boolean; override;
    function Initialise: boolean; override;
  end;

implementation

uses
  // Delphi
  SysUtils,
  Variants,

  // DWAF
  UUtilities,
  UConstants,
  VoaimsCom_TLB,
  UErrorHandlingOperations,
  UTreeViewTabSheet;

procedure TOutputReviewSheet.CreateMemberObjects;
const OPNAME = 'TOutputReviewSheet.CreateMemberObjects';
begin
  inherited CreateMemberObjects;
  try
    FViewersCreated := False;

    // Set ancestor properties.
    ShowHint := False;
    ParentShowHint := False;

    // Set defaults.
    FTabCaptionKey    := 'OutputReviewSheet';
    FViewTypeConstant := 'Review';
    FModelTabSheetName := mtsnOutput;
  // Handle exceptions.
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TOutputReviewSheet.LanguageHasChanged: boolean;
const OPNAME = 'TOutputReviewSheet.LanguageHasChanged';
begin
  Result := inherited LanguageHasChanged;
  try
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputReviewSheet.AssignHelpContext;
const OPNAME = 'TOutputReviewSheet.AssignHelpContext';
begin
  try
    SetControlHelpContext(Self,HC_GridView);
    SetControlHelpContext(FTreeView,HC_GridViewTreeView);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TOutputReviewSheet.Initialise: boolean;
const OPNAME = 'TOutputReviewSheet.Initialise';
begin
  Result := inherited Initialise;
  try
    Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TOutputReviewSheet.DoTreeNodeAboutToChange(ASender: TObject; ANode: TTreeNode; var AAllowChange: Boolean);
const OPNAME = 'TOutputReviewSheet.DoTreeNodeAboutToChange';
begin
  inherited DoTreeNodeAboutToChange(ASender, ANode, AAllowChange);
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputReviewSheet.DoOnHint;
const OPNAME = 'TOutputReviewSheet.DoOnHint';
begin
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOutputReviewSheet.StudyHasChanged: boolean;
const OPNAME = 'TOutputReviewSheet.StudyHasChanged';
begin
  Result := False;
  try
    Result := inherited StudyHasChanged;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOutputReviewSheet.CanCopyToClipboard: boolean;
const OPNAME = 'TOutputReviewSheet.CanCopyToClipboard';
begin
  Result := False;
  try
    Result := FAppModules.Model.CanCopyToCLipboard;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOutputReviewSheet.CanExport: boolean;
const OPNAME = 'TOutputReviewSheet.CanExport';
begin
  Result := False;
  try
    Result := FAppModules.Model.CanExport;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOutputReviewSheet.CanPrint: boolean;
const OPNAME = 'TOutputReviewSheet.CanPrint';
begin
  Result := False;
  try
    Result := FAppModules.Model.CanPrint;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputReviewSheet.DoCopyToClipboard;
const OPNAME = 'TOutputReviewSheet.DoCopyToClipboard';
begin
  try
    FAppModules.Model.DoCopyToClipboard;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputReviewSheet.DoExport(AFileName: string = '');
const OPNAME = 'TOutputReviewSheet.DoExport';
begin
  try
    FAppModules.Model.DoExport(AFileName);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TOutputReviewSheet.DoPrint;
const OPNAME = 'TOutputReviewSheet.DoPrint';
begin
  try
    FAppModules.Model.DoPrint;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TOutputReviewSheet.StudyDataHasChanged(AContext: TChangeContext;
  AFieldName, AOldValue, ANewValue: string): boolean;
const OPNAME = 'TOutputReviewSheet.StudyDataHasChanged';
begin
  Result := inherited StudyDataHasChanged(AContext,AFieldName,AOldValue,ANewValue);
  try
    Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TOutputReviewSheet.DoTreeNodeHasChanged(ASender: TObject;ANode: TTreeNode);
const OPNAME = 'TOutputReviewSheet.DoTreeNodeHasChanged';
begin
  inherited DoTreeNodeHasChanged(ASender,ANode);
  try
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TOutputReviewSheet.PopulateDataViewer(ADataObject: TViewDataTreeNodeData);
const OPNAME = 'TOutputReviewSheet.PopulateDataViewer';
var
  LContextData : string;
  LModelElementID : integer;
  LModelElementName : string;
  LNodeIndex: Integer;
begin
  inherited PopulateDataViewer(ADataObject);
  try
    LModelElementID   := ADataObject.ViewDataNode.Weighting;
    LModelElementName := ADataObject.ViewDataNode.OverrideCaption;
    LNodeIndex        := FTreeView.Selected.Index;
    if (ADataObject.ViewDataNode.ViewID = 'ReviewSumOutDataSource') and (FTreeView.Selected.Level = 1) then
    begin
      LContextData := ViewModelDataContextDataCommaText(mdvnSumOutDataSource,LModelElementID,LModelElementName,LNodeIndex);
      FAppModules.Model.ViewOutputDialog(self, LContextData, nil);
    end
    else if(ADataObject.ViewDataNode.ViewID = 'ReviewCollateOutputFiles') and (FTreeView.Selected.Level = 1) then
    begin
      LContextData := ViewModelDataContextDataCommaText(mdvnReviewCollateOutputFiles,LModelElementID,LModelElementName,LNodeIndex);
      FAppModules.Model.ViewOutputDialog(self, LContextData, nil);
    end
    else if(ADataObject.ViewDataNode.ViewID = 'ReviewSystemYield') and (FTreeView.Selected.Level = 1) then
    begin
      LContextData := ViewModelDataContextDataCommaText(mdvnSystemYield,LModelElementID,LModelElementName,LNodeIndex);
      FAppModules.Model.ViewOutputDialog(self, LContextData, nil);
    end
    else if(ADataObject.ViewDataNode.ViewID = 'ReviewChannelArea') and (FTreeView.Selected.Level = 2) then
    begin
      LContextData := ViewModelDataContextDataCommaText(mdvnChannelArea,LModelElementID,LModelElementName,LNodeIndex);
      FAppModules.Model.ViewOutputDialog(self, LContextData, nil);
    end
    else if(ADataObject.ViewDataNode.ViewID = 'ReviewReservoirArea') and (FTreeView.Selected.Level = 2) then
    begin
      LContextData := ViewModelDataContextDataCommaText(mdvnReservoirAreaGroup,LModelElementID,LModelElementName,LNodeIndex);
      FAppModules.Model.ViewOutputDialog(self, LContextData, nil);
    end
    else if(ADataObject.ViewDataNode.ViewID = 'ReviewAReservoir') and (FTreeView.Selected.Level = 1) then
    begin
      LContextData := ViewModelDataContextDataCommaText(mdvnReservoir,LModelElementID,LModelElementName,LNodeIndex);
      FAppModules.Model.ViewOutputDialog(self, LContextData, nil);
    end
    else if(ADataObject.ViewDataNode.ViewID = 'ReviewNodesWithInflow') and (FTreeView.Selected.Level = 2) then
    begin
      LContextData := ViewModelDataContextDataCommaText(mdvnNodesWithInflow,LModelElementID,LModelElementName,LNodeIndex);
      FAppModules.Model.ViewOutputDialog(self, LContextData, nil);
    end
    else if(ADataObject.ViewDataNode.ViewID = 'ReviewNodesWithoutInFlow') and (FTreeView.Selected.Level = 2) then
    begin
      LContextData := ViewModelDataContextDataCommaText(mdvnNodesWithoutInFlow,LModelElementID,LModelElementName,LNodeIndex);
      FAppModules.Model.ViewOutputDialog(self, LContextData, nil);
    end
    else if((FTreeView.Selected.Level = 2)  or
          (FTreeView.Selected.Level = 3)  or
          (FTreeView.Selected.Level = 4)) and
          ((ADataObject.ViewDataNode.ViewID   = 'ReviewChannelDetails2') or
          (ADataObject.ViewDataNode.ViewID    = 'ReviewChannelDetails5') or
          (ADataObject.ViewDataNode.ViewID    = 'ReviewChannelDetails6') or
          (ADataObject.ViewDataNode.ViewID    = 'ReviewChannelDetails7') or
          (ADataObject.ViewDataNode.ViewID    = 'ReviewChannelDetails8') or
          (ADataObject.ViewDataNode.ViewID    = 'ReviewChannelDetails9') or
          (ADataObject.ViewDataNode.ViewID    = 'ReviewChannelDetails10') or
          (ADataObject.ViewDataNode.ViewID    = 'ReviewChannelDetails11') or
          (ADataObject.ViewDataNode.ViewID    = 'ReviewChannelDetails12') or
          (ADataObject.ViewDataNode.ViewID    = 'ReviewChannelDetails18') or
          (ADataObject.ViewDataNode.ViewID    = 'ReviewChannelDetails19')) then
    begin
      LContextData := ViewModelDataContextDataCommaText(mdvnChannel,LModelElementID,LModelElementName,LNodeIndex);
      FAppModules.Model.ViewOutputDialog(self, LContextData, nil);
    end
    else if(ADataObject.ViewDataNode.ViewID = 'ReviewWetland') and (FTreeView.Selected.Level = 2) then
    begin
      LContextData := ViewModelDataContextDataCommaText(mdvnWetland,LModelElementID,LModelElementName,LNodeIndex);
      FAppModules.Model.ViewOutputDialog(self, LContextData, nil);
    end
    else if(ADataObject.ViewDataNode.ViewID = 'ReviewIrrigationAreas') and (FTreeView.Selected.Level = 2) then
    begin
      LContextData := ViewModelDataContextDataCommaText(mdvnIrrigationArea,LModelElementID,LModelElementName,LNodeIndex);
      FAppModules.Model.ViewOutputDialog(self, LContextData, nil);
    end
    else if(ADataObject.ViewDataNode.ViewID = 'ReviewIrrigationBlock') and (FTreeView.Selected.Level = 3) then
    begin
      LContextData := ViewModelDataContextDataCommaText(mdvnChannel,LModelElementID,LModelElementName,LNodeIndex);
      FAppModules.Model.ViewOutputDialog(self, LContextData, nil);
    end
    else if(ADataObject.ViewDataNode.ViewID = 'ReviewPCDDam') and (FTreeView.Selected.Level = 3) then
    begin
      LContextData := ViewModelDataContextDataCommaText(mdvnReservoir,LModelElementID,LModelElementName,LNodeIndex);
      FAppModules.Model.ViewOutputDialog(self, LContextData, nil);
    end
    else if(ADataObject.ViewDataNode.ViewID = 'ReviewUndegroundDam') and (FTreeView.Selected.Level = 3) then
    begin
      LContextData := ViewModelDataContextDataCommaText(mdvnReservoir,LModelElementID,LModelElementName,LNodeIndex);
      FAppModules.Model.ViewOutputDialog(self, LContextData, nil);
    end
    else if(ADataObject.ViewDataNode.ViewID = 'ReviewDemands') and (FTreeView.Selected.Level = 3) and
           (FAppModules.StudyArea.ModelCode = CPlanning) then
    begin
      LContextData := ViewModelDataContextDataCommaText(mdvnReviewDemands,LModelElementID,LModelElementName,LNodeIndex);
      FAppModules.Model.ViewOutputDialog(self, LContextData, nil);
    end
    else if(ADataObject.ViewDataNode.ViewID = 'ReviewSubSystems') and (FTreeView.Selected.Level = 3) and
           (FAppModules.StudyArea.ModelCode = CPlanning) then
    begin
      LContextData := ViewModelDataContextDataCommaText(mdvnReviewSubSystems,LModelElementID,LModelElementName,LNodeIndex);
      FAppModules.Model.ViewOutputDialog(self, LContextData, nil);
    end
    else if(FTreeView.Selected.Level = 3) and
          ((ADataObject.ViewDataNode.ViewID   = 'ReviewWRPMChannelDetails2') or
          (ADataObject.ViewDataNode.ViewID    = 'ReviewWRPMChannelDetails5') or
          (ADataObject.ViewDataNode.ViewID    = 'ReviewWRPMChannelDetails6') or
          (ADataObject.ViewDataNode.ViewID    = 'ReviewWRPMChannelDetails7') or
          (ADataObject.ViewDataNode.ViewID    = 'ReviewWRPMChannelDetails8') or
          (ADataObject.ViewDataNode.ViewID    = 'ReviewWRPMChannelDetails9') or
          (ADataObject.ViewDataNode.ViewID    = 'ReviewWRPMChannelDetails10') or
          (ADataObject.ViewDataNode.ViewID    = 'ReviewWRPMChannelDetails11') or
          (ADataObject.ViewDataNode.ViewID    = 'ReviewWRPMChannelDetails12') or
          (ADataObject.ViewDataNode.ViewID    = 'ReviewWRPMChannelDetails18') or
          (ADataObject.ViewDataNode.ViewID    = 'ReviewWRPMChannelDetails19')) and
          (FAppModules.StudyArea.ModelCode = CPlanning) then
    begin
      LContextData := ViewModelDataContextDataCommaText(mdvnReviewMonthlyChannelResult,LModelElementID,LModelElementName,LNodeIndex);
      FAppModules.Model.ViewOutputDialog(self, LContextData, nil);
    end
    else if(ADataObject.ViewDataNode.ViewID = 'ReviewPowerPlants') and (FTreeView.Selected.Level = 2) then
    begin
      LContextData := ViewModelDataContextDataCommaText(mdvnPowerPlant,LModelElementID,LModelElementName,LNodeIndex);
      FAppModules.Model.ViewOutputDialog(self, LContextData, nil);
    end
    else if(ADataObject.ViewDataNode.ViewID = 'ReviewPowerPlants') and (FTreeView.Selected.Level = 3) then
    begin
      LContextData := ViewModelDataContextDataCommaText(mdvnChannel,LModelElementID,LModelElementName,LNodeIndex);
      FAppModules.Model.ViewOutputDialog(self, LContextData, nil);
    end
    else if(ADataObject.ViewDataNode.ViewID = 'ReviewYMDemandCentre') and (FTreeView.Selected.Level = 3) then
    begin
      LContextData := ViewModelDataContextDataCommaText(mdvnChannel,LModelElementID,LModelElementName,LNodeIndex);
      FAppModules.Model.ViewOutputDialog(self, LContextData, nil);
    end
    else if(ADataObject.ViewDataNode.ViewID = 'ReviewDemandChannelsGridSummary') and (FTreeView.Selected.Level = 2) then
    begin
      LContextData := ViewModelDataContextDataCommaText(mdvnReviewDemandChannelsGridSummary,LModelElementID,LModelElementName,LNodeIndex);
      FAppModules.Model.ViewOutputDialog(self, LContextData, nil);
    end
    else if(ADataObject.ViewDataNode.ViewID = 'ReviewDemandChannelsGraphSummary') and (FTreeView.Selected.Level = 2) then
    begin
      LContextData := ViewModelDataContextDataCommaText(mdvnReviewDemandChannelsGraphSummary,LModelElementID,LModelElementName,LNodeIndex);
      FAppModules.Model.ViewOutputDialog(self, LContextData, nil);
    end

    else if(ADataObject.ViewDataNode.ViewID = 'ReviewCollateOutputFiles') and (FTreeView.Selected.Level = 1) and
           (FAppModules.StudyArea.ModelCode = CPlanning) then
    begin
      LContextData := ViewModelDataContextDataCommaText(mdvnReviewCollateOutputFiles,LModelElementID,LModelElementName,LNodeIndex);
      FAppModules.Model.ViewOutputDialog(self, LContextData, nil);
    end
    else if(ADataObject.ViewDataNode.ViewID = 'ReviewMasterControlConfiguration') and (FTreeView.Selected.Level = 2) then
    begin
      LContextData := ViewModelDataContextDataCommaText(mdvnMasterControlConfiguration,LModelElementID,LModelElementName,LNodeIndex);
      FAppModules.Model.ViewOutputDialog(self, LContextData, nil);
    end
    else if(ADataObject.ViewDataNode.ViewID = 'ReviewSubSystemStorage') and (FTreeView.Selected.Level = 2) and
           (FAppModules.StudyArea.ModelCode = CPlanning) then
    begin
      LContextData := ViewModelDataContextDataCommaText(mdvnReviewSubSystemStorage,LModelElementID,LModelElementName,LNodeIndex);
      FAppModules.Model.ViewOutputDialog(self, LContextData, nil);
    end
    else if(ADataObject.ViewDataNode.ViewID = 'ReviewTotalSystemStorage') and (FTreeView.Selected.Level = 2) and
           (FAppModules.StudyArea.ModelCode = CPlanning) then
    begin
      LContextData := ViewModelDataContextDataCommaText(mdvnReviewTotalSystemStorage,LModelElementID,LModelElementName,LNodeIndex);
      FAppModules.Model.ViewOutputDialog(self, LContextData, nil);
    end
    else if(ADataObject.ViewDataNode.ViewID = 'ReviewSubSystemCurtailment') and (FTreeView.Selected.Level = 2) and
           (FAppModules.StudyArea.ModelCode = CPlanning) then
    begin
      LContextData := ViewModelDataContextDataCommaText(mdvnReviewSubSystemCurtailment,LModelElementID,LModelElementName,LNodeIndex);
      FAppModules.Model.ViewOutputDialog(self, LContextData, nil);
    end
    else if(ADataObject.ViewDataNode.ViewID = 'ReviewTotalSystemCurtailment') and (FTreeView.Selected.Level = 2) and
           (FAppModules.StudyArea.ModelCode = CPlanning) then
    begin
      LContextData := ViewModelDataContextDataCommaText(mdvnReviewTotalSystemCurtailment,LModelElementID,LModelElementName,LNodeIndex);
      FAppModules.Model.ViewOutputDialog(self, LContextData, nil);
    end
    else if(ADataObject.ViewDataNode.ViewID = 'ReviewDemandSupply') and (FTreeView.Selected.Level = 2) and
           (FAppModules.StudyArea.ModelCode = CPlanning) then
    begin
      LContextData := ViewModelDataContextDataCommaText(mdvnReviewDemandSupply,LModelElementID,LModelElementName,LNodeIndex);
      FAppModules.Model.ViewOutputDialog(self, LContextData, nil);
    end
    else if(ADataObject.ViewDataNode.ViewID = 'ReviewInterBasinSupport') and (FTreeView.Selected.Level = 2) and
           (FAppModules.StudyArea.ModelCode = CPlanning) then
    begin
      LContextData := ViewModelDataContextDataCommaText(mdvnReviewInterBasinSupport,LModelElementID,LModelElementName,LNodeIndex);
      FAppModules.Model.ViewOutputDialog(self, LContextData, nil);
    end
    else if(ADataObject.ViewDataNode.ViewID = 'ReviewDamStorage') and (FTreeView.Selected.Level = 3) and
           (FAppModules.StudyArea.ModelCode = CPlanning) then
    begin
      LContextData := ViewModelDataContextDataCommaText(mdvnReviewDamStorage,LModelElementID,LModelElementName,LNodeIndex);
      FAppModules.Model.ViewOutputDialog(self, LContextData, nil);
    end
    else
    begin
      ShowNoDataMessage;
      //LContextData := ViewModelDataContextDataCommaText(mdvnOutput,NullInteger);
      //FAppModules.Model.ViewOutputDialog(self, LContextData, nil);
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TOutputReviewSheet.GetToolBar: TAbstractToolBar;
const OPNAME = 'TOutputReviewSheet.GetToolBar';
begin
  Result := nil;
  try
  except on E: Exception do HandleError(E, OPNAME); end;
end;

end.
