//
//  UNIT      : Contains TYieldModelDataGUIManager Class
//  AUTHOR    : Valentino Naicker(ARIVIA)
//  DATE      : 07/04/2003
//  COPYRIGHT : Copyright © 2003 DWAF
//
//  GUI's are managed on 2 levels  Owned or referenced
//  When referenced the Owner is the Application MainForm as per the Design
//

unit UYieldModelDataGUIManager;

interface

uses

  // Delphi
  classes,
  contnrs,
  VCL.controls,
  VCL.dialogs,
  VCL.Grids,
  Vcl.Forms,
  // DWAF
  UAbstractObject,
  UAbstractComponent,
  UDataComponent,
  UYieldModelDataGUIForm,
  UPopupDialogForm,
  UReservoirAreaGroup,
  VoaimsCom_TLB;


Type LastSelectedStringGridStore = class(TObject)
   Protected
    GridName : array of String;
    GridSelection : array of TGridRect;
    TopRow : Integer;
   end;

type
  TYieldModelDataGUIManager = class(TAbstractAppObject)
  protected
    FInputPageControl        : TAbstractDataPageControl;
    FOutputPageControl       : TAbstractDataPageControl;
    FInputPopupPageControl   : TAbstractDataPageControl;
    FOutputPopupPageControl  : TAbstractDataPageControl;
    FPopupForm               : TYieldModelDataGUIForm;
    FPopupFrameForm          : TfrmPopupDialog;
    LastSelectedStringGrid   : LastSelectedStringGridStore;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    procedure OnPageControlTabHasChanged(Sender: TObject);
    procedure OnPageControlChanging(Sender: TObject; var AllowChange: Boolean);
    function ViewInputData(ACommaTextContextData : String; APageControl   : TAbstractDataPageControl): boolean; overload; virtual;
    function ViewInputData(ACommaTextContextData : String): boolean; overload; virtual;
    function ViewOutputData(ACommaTextContextData : String; APageControl   : TAbstractDataPageControl): boolean; overload; virtual;
    function ViewOutputData(ACommaTextContextData : String): boolean;overload; virtual;
    function ViewOutputComparisonData(ACommaTextContextData : String; APageControl   : TAbstractDataPageControl): boolean;virtual;
  public
    function Initialise: boolean; override;
    function LanguageHasChanged: boolean; override;
    function StudyHasChanged: boolean; override;
    function SaveState: boolean; override;
    function SaveLastSelectedCell(APageCtrl : TAbstractDataPageControl) : boolean;
    function LoadLastSelectedCell(APageCtrl : TAbstractDataPageControl) : boolean;
    function StudyDataHasChanged(AContext: TChangeContext; AFieldName,AOldValue,ANewValue: string): boolean; override;
    function ProcessParameterChangeEvent : boolean;
    function ProcessMetaDataEvent : boolean;

    function ViewInputDialog(AParent : TWincontrol; ACommaTextContextData : String; AOwner : TWincontrol = nil): boolean; virtual;
    function ViewInputPopupDialog(AParent : TWincontrol; ACommaTextContextData : String;  AOwner : TWincontrol = nil): boolean;virtual;
    function ViewOutputDialog(AParent : TWincontrol; ACommaTextContextData : String; AOwner : TWincontrol = nil): boolean;virtual;
    function ViewOutputPopupDialog(AParent : TWincontrol; ACommaTextContextData : String;  AOwner : TWincontrol = nil): boolean;virtual;
    function ViewOutputComparisonDialog(AParent: TWincontrol;ACommaTextContextData: String;AOwner: TWincontrol): boolean;virtual;

    function CanCopyToCLipboard: boolean; virtual;
    function CanExport: boolean; virtual;
    function CanPrint: boolean; virtual;
    procedure DoCopyToCLipboard; virtual;
    procedure DoExport(AFileName: string = ''); virtual;
    procedure DoPrint; virtual;
    procedure ExitCurrentEditControl;
    function CurrentTabSheet: TModelTabSheetName;
    function PopupDialogShowing:boolean;
  end;


implementation

uses

  // Delphi
  Sysutils,
  Windows,

  // DWAF
  UConstants,
  UYieldModelDataObject,

  UInputGridValidator,
  UInputGraphValidator,

  UYieldMetaDataValidator,
  UNodePropertiesValidator,
  UReservoirPenaltyValidator,
  UReservoirPropertiesValidator,
  UReservoirEvaporationValidator,
  UReservoirZoneElevationsValidator,
  UReservoirCatchmentProportionsValidator,
  UReservoirPhysicalCharacteristicsValidator,
  UReservoirTimeControlValidator,
  UChannelTimeControlValidator,
  UChannelSwitchControlValidator,

  UGeneralFlowChannelValidator,
  UMinimumFlowChannelValidator,
  ULossChannelValidator,
  UMinMaxChannelValidator,
  UPumpingFeatureValidator,
  //UYMDemandCentreReturnFlowFeatureValidator,
  USpecifiedDemandChannelValidator,
  UChannelPenaltyValidator,
  UDiversionFeatureValidator,
  UPowerPlantValidator,
  UPowerPlantFactorsValidator,
  UPowerPlantTailwaterValidator,
  UPowerPlantDemandsValidator,
  UIrrigationAreaValidator,
  USpecifiedInflowDataValidator,
  UPhysicalFlowConstraintValidator,
  UIFRFeaturesPropertyValidator,
  UIFRFeatureValidator,
  UWaterDemandFeatureValidator,
  UWaterUseProportioningValidator,
  UReconciliationAnalysisValidator,
//  UChannelAreaDialog,
  UChannelAreaValidator,
  UWaterUseScenarioValidator,

  UOutputConfigurationValidator,
  URunConfigurationValidator,
  UMasterControlChannelValidator,
  UHydrologyAlocationValidator,
  UYieldRunTitleValidator,
  UReservoirAndChannelOutputValidator,

  UOutputGridValidator,
  UOutputGraphValidator,
  UOutputDistributionCurveValidator,
  USystemYieldValidator,
  USystemYieldHistoricValidator,
  USystemYieldStochasticValidator,
  UOutputWaterBalanceValidator,
  UOutputComplianceGridValidator,
  UOutputComplianceGraphValidator,
  UOutputBoxPlotGraphValidator,
  UOutputReviewDataSourceValidator,
  UOutputDeficitDurationValidator,
  UOutputMonthlyDeficitValidator,
  UChannelDemandsGridValidator,
//  UOutputWaterUseComplianceGraphValidator,

  {UFMAllocationDefinitionValidator,
  USwitchDefinitionValidator,
  UFMUserPriorityClassificationValidator,
  UFMYieldCharacteristicsValidator,
  UFMSupportStrategyValidator,
  UFMSubSystemsValidator,
  UFMDemandDefinitionValidator,
  UFMSupportChannelsValidator,

  UPlanningRunConfigurationValidator,
  UGrowthProjectionsValidator,
  UGrowthFactorsValidator,
  UDisbenefitFunctionDefinitionDataValidator,
  UReturnFlowChannelValidator,}

  UIrrBlockPropertiesValidator,
  UIrrBlockIrrigatedAreaValidator,
  UIrrBlockCropRequirementValidator,
  UIrrBlockClimateValidator,
  UIrrBlockGrowthValidator,
  UIrrBlockGrowthType4Validator,


  UIrrigationBlockValidator,
  UIrrigationBlockReturnFlowChannelValidator,
  UEvaporationValidator,
  UCropWaterValidator,
  USoilPropertiesValidator,

  UWetlandValidator,
  UYMDemandCentreValidator,
  UYMDemandCentreEvaporationValidator,
  UYMDemandCentreReturnFlowChannelValidator,
  UYMDemandCentreSupplyChannelValidator,
  UConfigurationFilesValidator,
  UStreamFlowReductionValidator,

  UMiningValidator,
  UMiningSlurryDumpValidator,
  UMiningOpenCastPitValidator,
  UMiningUnderGroundSectionValidator,
  UMineToRiverChannelValidator,
  UMineToPCDChannelValidator,
  UMineToUnderGroundDamChannelValidator,
  UAnnualIFRFeatureValidator,
  UCurtailmentStructureValidator,
  UDroughtRestrictionStructureValidator,

  //  UPlanningModelDataObject,
//  UGrowthFactorData,
//  UChannelDistributionValidator,
  UOutputComparisonFileSelectionValidator,
  UOutputComparisonReservoirValidator,
  UOutputComparisonChannelValidator,
  UEstimatedHistoricValidator,
  UGroundWaterValidator,
  UGroundWaterPitmanValidator,
  UReservoirAreaGroupValidator,
  UMineSubcatchmentValidator,

  UIrrBlockWaterAllocationValidator,
  UIrrBlockSupplyCapacityValidator,
  //UIrrBlockEfficienciesValidator,


  UErrorHandlingOperations,
  UNetworkFeaturesData,
  UChannelAreas,
  UChannelData,
  UNetworkElementData,
  UMiningData,
  UGroundWater,
  UOutputLongtermSupplyValidator,
  UOutputDemandChannelsGridSummaryValidator,
  UOutputDemandChannelsGraphSummaryValidator;

const
  CCopyToClipboard     : array[0..1] of string = ('Edit','CopyToClipboard');
  CExportToFile        : array[0..1] of string = ('Edit','ExportToFile');
  CPrint               : array[0..1] of string = ('File','Print');

{ TYieldModelDataGUIManager }

procedure TYieldModelDataGUIManager.CreateMemberObjects;
const OPNAME = 'TYieldModelDataGUIManager.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
    FInputPageControl           := TAbstractDataPageControl.Create(nil,FAppModules);
    FOutputPageControl          := TAbstractDataPageControl.Create(nil,FAppModules);
    FInputPopupPageControl      := TAbstractDataPageControl.Create(nil,FAppModules);
    FOutputPopupPageControl     := TAbstractDataPageControl.Create(nil,FAppModules);

    FInputPageControl.Align          := alClient;
    FOutputPageControl.Align         := alClient;
    FInputPopupPageControl.Align     := alClient;
    FOutputPopupPageControl.Align    := alClient;

    FInputPageControl.OnChange       := OnPageControlTabHasChanged;
    FOutputPageControl.OnChange      := OnPageControlTabHasChanged;
    FPopupForm                  := nil;
    FPopupFrameForm             := nil;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYieldModelDataGUIManager.DestroyMemberObjects;
const OPNAME = 'TYieldModelDataGUIManager.DestroyMemberObjects';
begin
  try
    LastSelectedStringGrid.Free;
    //FInputPageControl.Parent  := nil;
    //FOutputPageControl.Parent := nil;
    //FreeAndNil(FInputPageControl);
    //FreeAndNil(FOutputPageControl);
    inherited DestroyMemberObjects;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldModelDataGUIManager.CurrentTabSheet: TModelTabSheetName;
const OPNAME = 'TYieldModelDataGUIManager.CurrentTabSheet';
begin
  Result := mtsnNone;
  try
    if(FAppModules.MainForm <> nil) then
    begin
       Result := TAbstractTabSheet(FAppModules.MainForm.PageControl.ActivePage).ModelTabSheetName;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldModelDataGUIManager.Initialise: boolean;
const OPNAME = 'TYieldModelDataGUIManager.Initialise';
begin
  Result := inherited Initialise;
  try
    FInputPageControl.Initialise;
    FOutputPageControl.Initialise;
    FInputPopupPageControl.Initialise;
    FOutputPopupPageControl.Initialise;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldModelDataGUIManager.LanguageHasChanged: boolean;
const OPNAME = 'TYieldModelDataGUIManager.LanguageHasChanged';
begin
  Result := inherited LanguageHasChanged;
  try
    FInputPageControl.LanguageHasChanged;
    FOutputPageControl.LanguageHasChanged;
    FInputPopupPageControl.LanguageHasChanged;
    FOutputPopupPageControl.LanguageHasChanged;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldModelDataGUIManager.StudyHasChanged: boolean;
const OPNAME = 'TYieldModelDataGUIManager.StudyHasChanged';
begin
  Result := inherited StudyHasChanged;
  try
    FInputPageControl.DeleteAllTabSheets;
    FOutputPageControl.DeleteAllTabSheets;
    FInputPopupPageControl.DeleteAllTabSheets;
    FOutputPopupPageControl.DeleteAllTabSheets;
    FInputPageControl.StudyHasChanged;
    FOutputPageControl.StudyHasChanged;
    FInputPopupPageControl.StudyHasChanged;
    FOutputPopupPageControl.StudyHasChanged;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldModelDataGUIManager.SaveState: boolean;
const OPNAME = 'TYieldModelDataGUIManager.SaveState';
begin
  Result := inherited SaveState;
  try
    FInputPageControl.SaveState;
    FOutputPageControl.SaveState;
    FInputPopupPageControl.SaveState;
    FOutputPopupPageControl.SaveState;

    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldModelDataGUIManager.ProcessParameterChangeEvent : boolean;
const OPNAME = 'TYieldModelDataGUIManager.ProcessParameterChangeEvent';
begin
  Result := FALSE;
  try
    Result := FInputPageControl.ProcessParameterChangeEvent;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldModelDataGUIManager.ProcessMetaDataEvent : boolean;
const OPNAME = 'TYieldModelDataGUIManager.ProcessMetaDataEvent';
begin
  Result := FALSE;
  try
    Result := FInputPageControl.ProcessMetaDataEvent;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldModelDataGUIManager.StudyDataHasChanged(AContext: TChangeContext; AFieldName,AOldValue,ANewValue: string): boolean;
const OPNAME = 'TYieldModelDataGUIManager.StudyDataHasChanged';
begin
  Result := inherited StudyDataHasChanged(AContext,AFieldName,AOldValue,ANewValue);
  try
    FInputPageControl.StudyDataHasChanged(AContext,AFieldName,AOldValue,ANewValue);
    FOutputPageControl.StudyDataHasChanged(AContext,AFieldName,AOldValue,ANewValue);
    FInputPopupPageControl.StudyDataHasChanged(AContext,AFieldName,AOldValue,ANewValue);
    FOutputPopupPageControl.StudyDataHasChanged(AContext,AFieldName,AOldValue,ANewValue);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYieldModelDataGUIManager.OnPageControlChanging(Sender: TObject; var AllowChange: Boolean);
const OPNAME = 'TYieldModelDataGUIManager.OnPageControlChanging';

var
  pnt: TPoint;
  NewTabIndex: integer;
  LValidator: TAbstractDataDialogValidator;
begin
  try
    if not GetCursorPos(pnt) then Exit;
    pnt := FInputPopupPageControl.ScreenToClient(pnt);
    NewTabIndex := FInputPopupPageControl.IndexOfTabAt(pnt.X, pnt.Y);
    FInputPopupPageControl.ActivePageIndex :=  NewTabIndex;
    LValidator := FInputPopupPageControl.GetValidatorByIndex(NewTabIndex);
    if LValidator <> nil then
      LValidator.Panel.Visible := True;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYieldModelDataGUIManager.OnPageControlTabHasChanged(Sender: TObject);
const OPNAME = 'TYieldModelDataGUIManager.OnPageControlTabHasChanged';
begin
  try
    if FAppModules.MainForm <> nil then
    begin
      if CanCopyToCLipboard then
        FAppModules.SetMenuItem(CCopyToClipboard, msEnable)
      else
        FAppModules.SetMenuItem(CCopyToClipboard, msDisable);
      if CanExport then
        FAppModules.SetMenuItem(CExportToFile, msEnable)
      else
        FAppModules.SetMenuItem(CExportToFile, msDisable);
      if CanPrint then
        FAppModules.SetMenuItem(CPrint, msEnable)
      else
        FAppModules.SetMenuItem(CPrint, msDisable);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldModelDataGUIManager.CanCopyToCLipboard: boolean;
const OPNAME = 'TYieldModelDataGUIManager.CanCopyToCLipboard';
begin
  Result := False;
  try
    case CurrentTabSheet of
      mtsnInput : Result := FInputPageControl.CanCopyToCLipboard;
      mtsnOutput: Result := FOutputPageControl.CanCopyToCLipboard;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldModelDataGUIManager.CanExport: boolean;
const OPNAME = 'TYieldModelDataGUIManager.CanExport';
begin
  Result := False;
  try
    case CurrentTabSheet of
      mtsnInput : Result := FInputPageControl.CanExport;
      mtsnOutput: Result := FOutputPageControl.CanExport;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldModelDataGUIManager.CanPrint: boolean;
const OPNAME = 'TYieldModelDataGUIManager.CanPrint';
begin
  Result := False;
  try
    case CurrentTabSheet of
      mtsnInput : Result := FInputPageControl.CanPrint;
      mtsnOutput: Result := FOutputPageControl.CanPrint;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYieldModelDataGUIManager.DoCopyToCLipboard;
const OPNAME = 'TYieldModelDataGUIManager.DoCopyToCLipboard';
begin
  try
    case CurrentTabSheet of
      mtsnInput : FInputPageControl.DoCopyToCLipboard;
      mtsnOutput: FOutputPageControl.DoCopyToCLipboard;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYieldModelDataGUIManager.DoExport(AFileName: string = '');
const OPNAME = 'TYieldModelDataGUIManager.DoExport';
begin
  try
    case CurrentTabSheet of
      mtsnInput : FInputPageControl.DoExport(AFileName);
      mtsnOutput: FOutputPageControl.DoExport(AFileName);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYieldModelDataGUIManager.DoPrint;
const OPNAME = 'TYieldModelDataGUIManager.DoPrint';
begin
  try
    case CurrentTabSheet of
      mtsnInput : FInputPageControl.DoPrint;
      mtsnOutput: FOutputPageControl.DoPrint;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYieldModelDataGUIManager.ExitCurrentEditControl;
const OPNAME = 'TYieldModelDataGUIManager.ExitCurrentEditControl';
begin
  try
    case CurrentTabSheet of
      mtsnInput : FInputPageControl.ExitCurrentEditControl;
      mtsnOutput: FOutputPageControl.ExitCurrentEditControl;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldModelDataGUIManager.PopupDialogShowing: boolean;
const OPNAME = 'TYieldModelDataGUIManager.PopupDialogShowing';
begin
  Result := False;
  try
    Result := (FPopupForm <> nil);
  except on E: Exception do HandleError(E, OPNAME) end;

end;

function TYieldModelDataGUIManager.ViewInputDialog(AParent : TWincontrol; ACommaTextContextData : String;  AOwner : TWincontrol = nil): boolean;
const OPNAME = 'TYieldModelDataGUIManager.ViewInputDialog';
begin
  Result := False;
  try
    FInputPageControl.DeleteAllTabSheets;
    FInputPageControl.Parent := AParent;
    FInputPageControl.Visible := False;

    LockWindowUpdate(AParent.Handle);
    try
      Result := ViewInputData(ACommaTextContextData,FInputPageControl);

      Result := Result and (FInputPageControl.ValidatorCount > 0);
      if Result then
      begin
        FInputPageControl.Visible := True;
        FInputPageControl.LanguageHasChanged;
        FInputPageControl.SelectLastActiveTabsheet;
      end
      else
        FInputPageControl.Parent := nil;
    finally
      LockWindowUpdate(0);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldModelDataGUIManager.ViewInputPopupDialog(AParent: TWincontrol; ACommaTextContextData: String;
         AOwner: TWincontrol): boolean;
const OPNAME = 'TYieldModelDataGUIManager.ViewInputPopupDialog';
begin
  Result := False;
  try
    FPopupFrameForm  := TfrmPopupDialog.Create(Application);
    try
      FPopupFrameForm.Initialise;
      Result := ViewInputData(ACommaTextContextData);
      Result := Result and (FPopupFrameForm.CanShowValidators);
      if Result then
      begin
        repeat
          FPopupFrameForm.ShowModal;
        until (FPopupFrameForm.ModalResult = mrOk) or
              (FPopupFrameForm.CanCloseForm)
      end;
    finally
      FreeAndNil(FPopupFrameForm);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldModelDataGUIManager.ViewOutputDialog(AParent: TWincontrol;ACommaTextContextData: String;
         AOwner: TWincontrol): boolean;
const OPNAME = 'TYieldModelDataGUIManager.ViewOutputDialog';
begin
  Result := False;
  try
    SaveLastSelectedCell(FOutputPageControl);
    FOutputPageControl.DeleteAllTabSheets;
    FOutputPageControl.Parent := AParent;
    FOutputPageControl.Visible := False;

    LockWindowUpdate(AParent.Handle);
    try
      Result := ViewOutputData(ACommaTextContextData,FOutputPageControl);

      Result := Result and (FOutputPageControl.ValidatorCount > 0);
      if Result then
      begin
        FOutputPageControl.Visible := True;
        FOutputPageControl.LanguageHasChanged;
        FOutputPageControl.SelectLastActiveTabsheet;
        LoadLastSelectedCell(FOutputPageControl);
      end
      else
        FOutputPageControl.Parent := nil;
    finally
      LockWindowUpdate(0);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;
function TYieldModelDataGUIManager.ViewOutputPopupDialog(AParent: TWincontrol; ACommaTextContextData: String;
         AOwner: TWincontrol): boolean;
const OPNAME = 'TYieldModelDataGUIManager.ViewOutputPopupDialog';
begin
  Result := False;
  try
    FPopupFrameForm  := TfrmPopupDialog.Create(Application);
    try
      FPopupFrameForm.Initialise;
      Result := ViewOutputData(ACommaTextContextData);
      Result := Result and (FPopupFrameForm.CanShowValidators);
      if Result then
      begin
        repeat
          FPopupFrameForm.ShowModal;
          if Application.Terminated then
            FPopupFrameForm.ModalResult := mrCancel
        until (FPopupFrameForm.ModalResult = mrOk) or
              (FPopupFrameForm.CanCloseForm)
      end;
    finally
      FreeAndNil(FPopupFrameForm);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


(*
function TYieldModelDataGUIManager.ViewOutputPopupDialog(AParent: TWincontrol; ACommaTextContextData: String;
         AOwner: TWincontrol): boolean;
const OPNAME = 'TYieldModelDataGUIManager.ViewOutputPopupDialog';
begin
  Result := False;
  try
    FPopupForm   := TYieldModelDataGUIForm.CreateWithoutDFM(nil,FAppModules);
    try
      FOutputPopupPageControl.DeleteAllTabSheets;
      FOutputPopupPageControl.Visible := False;
      FOutputPopupPageControl.Parent  := FPopupForm;

      Result := ViewOutputData(ACommaTextContextData,FOutputPopupPageControl);
      Result := Result and (FOutputPopupPageControl.ValidatorCount > 0);
      if Result then
      begin
        FPopupForm.Initialise;
        FPopupForm.LanguageHasChanged;
        FOutputPopupPageControl.Align   := alClient;
        FOutputPopupPageControl.Visible := True;
        FOutputPopupPageControl.LanguageHasChanged;
        FOutputPopupPageControl.SelectLastActiveTabsheet;
        FPopupForm.ShowModal;
        FOutputPopupPageControl.DeleteAllTabSheets;
        FOutputPopupPageControl.Parent := nil;
      end;
    finally
      FreeAndNil(FPopupForm);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;
*)

function TYieldModelDataGUIManager.ViewOutputComparisonDialog(AParent: TWincontrol;ACommaTextContextData: String;
         AOwner: TWincontrol): boolean;
const OPNAME = 'TYieldModelDataGUIManager.ViewOutputComparisonDialog';
begin
  Result := False;
  try
    FOutputPageControl.DeleteAllTabSheets;
    FOutputPageControl.Parent := AParent;
    FOutputPageControl.Visible := False;

    LockWindowUpdate(AParent.Handle);
    try
      Result := ViewOutputComparisonData(ACommaTextContextData,FOutputPageControl);

      Result := Result and (FOutputPageControl.ValidatorCount > 0);
      if Result then
      begin
        FOutputPageControl.Visible := True;
        FOutputPageControl.LanguageHasChanged;
        FOutputPageControl.SelectLastActiveTabsheet;
      end
      else
        FOutputPageControl.Parent := nil;
    finally
      LockWindowUpdate(0);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldModelDataGUIManager.ViewInputData(ACommaTextContextData: String; APageControl   : TAbstractDataPageControl): boolean;
const OPNAME = 'TYieldModelDataGUIManager.ViewInputData';
var
  LElementType                : string;
  LViewName                   : string;
  LIdentifier                 : integer;
  lFeatureID                  : integer;
  //LGeneralNodeID              : Integer;
  LChannelType                : integer;
  LContextDataList            : TStringList;
  LDialogValidator            : TAbstractDataDialogValidator;
  LChannel                    : IGeneralFlowChannel;
  lRunConfigData              : IRunConfigurationData;
  lPowerPlant                 : IPowerPlant;
  lIrrigationArea             : IIrrigationArea;
  lMinimumConstraint          : IMinimumFlowConstraint;
  lLossFeature                : ILossFeature;
  lMinMaxConstraint           : IMinMaxFlowConstraint;
  lPumpingFeature             : IPumpingFeature;
  lDemandFeature              : ISpecifiedDemandFeature;
  lDivFeature                 : IDiversionFeature;
  lPhysConstraint             : IPhysicalFlowConstraint;
  lIFRFeature                 : IIFRFeature;
  LWaterDemandFeature         : IWaterDemandFeature;
  lMasterControlFeature       : IMasterControlFeature;
  lSpecInflow                 : ISpecifiedInflowFeature;
  lReservoir                  : IReservoirData;
  lIrrigationBlock            : IIrrigationBlock;
  LWetland                    : IWetland;
  LMine                       : IMine;
  LYMDemandCentre             : IYMDemandCentre;
  LDroughtRestriction         : IDroughtRestriction;
  LGroundWater                : IGroundWater;
//  LMinMaxChannelGrowthFactors : IMinMaxChannelGrowthFactors;
//  LGrowthFactors              : IGrowthFactors;
begin
  Result := False;
  try

    if (Trim(ACommaTextContextData) = '') then Exit;

    LContextDataList := TStringList.Create;
    try
      LContextDataList.CommaText := ACommaTextContextData;
      LViewName := UpperCase(Trim(LContextDataList.Values['VIEWNAME']));
      if(LViewName = '') then Exit;

      LIdentifier   := StrToInt(LContextDataList.Values['MODELELEMENTID']);
      LElementType  := UpperCase(Trim(LContextDataList.Values['MODELELEMENTTYPE']));
    finally
      FreeAndNil(LContextDataList);
    end;

    if (LViewName = mdvnInputViewDataGrid) then
    begin
      LDialogValidator := TInputGridValidator.Create(FPopupForm,FAppModules);
      APageControl.AddValidator(LDialogValidator);
      LDialogValidator.Initialise;
      TInputGridValidator(LDialogValidator).Identifier := LIdentifier;
      TInputGridValidator(LDialogValidator).ElementType := LElementType;
      LDialogValidator.PopulateDataViewer;
      LDialogValidator.LanguageHasChanged;
    end;

    if (LViewName = mdvnInputViewDataGraph) then
    begin
      LDialogValidator := TInputGraphValidator.Create(FPopupForm,FAppModules);
      APageControl.AddValidator(LDialogValidator);
      LDialogValidator.Initialise;
      TInputGraphValidator(LDialogValidator).Identifier := LIdentifier;
      TInputGraphValidator(LDialogValidator).ElementType := LElementType;
      LDialogValidator.PopulateDataViewer;
      LDialogValidator.LanguageHasChanged;
    end;

    // Group dialogs
    if (LViewName = mdvnIFRFeaturesProperty) then
    begin
      LDialogValidator := TIFRFeaturesPropertyValidator.Create(nil,FAppModules);
      APageControl.AddValidator(LDialogValidator);
      LDialogValidator.Initialise;
      LDialogValidator.PopulateDataViewer;
      LDialogValidator.LanguageHasChanged;
    end;

    if (LViewName = mdvnRunConfiguration) then
    begin
      //Run Configuration
      lRunConfigData := TYieldModelDataObject(FAppModules.Model.ModelData).RunConfigurationData;
      if (lRunConfigData.HasBeenPopulated) then
      begin
        LDialogValidator := TRunConfigurationValidator.Create(nil,FAppModules);
        APageControl.AddValidator(LDialogValidator);
        LDialogValidator.Initialise;
        LDialogValidator.PopulateDataViewer;
        LDialogValidator.LanguageHasChanged;

        LDialogValidator := TConfigurationFilesValidator.Create(nil,FAppModules);
        APageControl.AddValidator(LDialogValidator);
        LDialogValidator.Initialise;
        LDialogValidator.PopulateDataViewer;
        LDialogValidator.LanguageHasChanged;
      end
    end;

    if (LViewName = mdvnHydrologyAllocation) then
    begin
      //Output Configuration
      if (TYieldModelDataObject(FAppModules.Model.ModelData).RunConfigurationData.OutputDataHasBeenPopulated) then
      begin
        LDialogValidator := THydrologyAlocationValidator.Create(nil,FAppModules);
        APageControl.AddValidator(LDialogValidator);
        LDialogValidator.Initialise;
        LDialogValidator.PopulateDataViewer;
        LDialogValidator.LanguageHasChanged;
      end;
    end
    else
    if (LViewName = mdvnOutputConfiguration) then
    begin
      //Output Configuration
      if (TYieldModelDataObject(FAppModules.Model.ModelData).RunConfigurationData.OutputDataHasBeenPopulated) then
      begin
        LDialogValidator := TOutputConfigurationValidator.Create(nil,FAppModules);
        APageControl.AddValidator(LDialogValidator);
        LDialogValidator.Initialise;
        LDialogValidator.PopulateDataViewer;
        LDialogValidator.LanguageHasChanged;

        LDialogValidator := TReservoirAndChannelOutputValidator.Create(nil,FAppModules);
        APageControl.AddValidator(LDialogValidator);
        LDialogValidator.Initialise;
        LDialogValidator.PopulateDataViewer;
        LDialogValidator.LanguageHasChanged;
      end;
    end
    else
    if (LViewName = mdvnReconciliationAnalysis) then
    begin
      //Output Configuration
      if (TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.WaterDemandConfiguration <> nil) then
      begin
        LDialogValidator := TReconciliationAnalysisValidator.Create(nil,FAppModules);
        APageControl.AddValidator(LDialogValidator);
        LDialogValidator.Initialise;
        LDialogValidator.PopulateDataViewer;
        LDialogValidator.LanguageHasChanged;

        //Water use proportioning
        LDialogValidator := TWaterUseProportioningValidator.Create(nil,FAppModules);
        APageControl.AddValidator(LDialogValidator);
        LDialogValidator.Initialise;
        LDialogValidator.PopulateDataViewer;
        LDialogValidator.LanguageHasChanged;

        //Water use scenario
        LDialogValidator := TWaterUseScenarioValidator.Create ( nil, FAppModules );
        APageControl.AddValidator ( LDialogValidator );
        LDialogValidator.Initialise;
        LDialogValidator.PopulateDataViewer;
        LDialogValidator.LanguageHasChanged;
      end;
    end
    else
    if (LViewName = mdvnReservoirPenalty) then
    begin
      //Reservoir Penalty
      LDialogValidator := TReservoirPenaltyValidator.Create(nil,FAppModules);
      APageControl.AddValidator(LDialogValidator);
      LDialogValidator.Initialise;
      TReservoirPenaltyValidator(LDialogValidator).PenaltyStructureNumber := -1;
      TReservoirPenaltyValidator(LDialogValidator).ViewMode := vmEditable;
      LDialogValidator.PopulateDataViewer;
      LDialogValidator.LanguageHasChanged;
    end
    else
    if (LViewName = mdvnMetaData) then
    begin
      //Reservoir Penalty
      LDialogValidator := TYieldMetaDataValidator.Create(nil,FAppModules);
      APageControl.AddValidator(LDialogValidator);
      LDialogValidator.Initialise;
      LDialogValidator.PopulateDataViewer;
      LDialogValidator.LanguageHasChanged;
    end
    else
    if (LViewName = mdvnCatchmentProportions) then
    begin
      //Reservoir Catchment Proportions
      LDialogValidator := TReservoirCatchmentProportionsValidator.Create(nil,FAppModules);
      APageControl.AddValidator(LDialogValidator);
      LDialogValidator.Initialise;
      TReservoirCatchmentProportionsValidator(LDialogValidator).ViewMode :=  vmEditable;
      LDialogValidator.PopulateDataViewer;
      LDialogValidator.LanguageHasChanged;
    end
    else
    if (LViewName = mdvnChannelArea) then
    begin
      //Channel Area
      LDialogValidator := TChannelAreaValidator.Create(nil,FAppModules);
      APageControl.AddValidator(LDialogValidator);
      LDialogValidator.Initialise;
      LDialogValidator.PopulateDataViewer;
      LDialogValidator.LanguageHasChanged;
    end
    else
    if (LViewName = mdvnReservoirAreaGroup) then
    begin
      //Channel Area
      LDialogValidator := TReservoirAreaGroupValidator.Create(nil,FAppModules);
      APageControl.AddValidator(LDialogValidator);
      LDialogValidator.Initialise;
      LDialogValidator.PopulateDataViewer;
      LDialogValidator.LanguageHasChanged;
    end
    else
    if (LViewName = mdvnCurtailments) then
    begin
      //Curtailment Structure
      LDialogValidator := TCurtailmentStructureValidator.Create(nil,FAppModules);
      APageControl.AddValidator(LDialogValidator);
      LDialogValidator.Initialise;
      LDialogValidator.PopulateDataViewer;
      LDialogValidator.LanguageHasChanged;
    end
    else
    if (LViewName = mdvnRestrictions) then
    begin
      //Drought Restriction Structure
      LDroughtRestriction := TYieldModelDataObject(FAppModules.Model.ModelData)
                     .NetworkFeaturesData.CurtailmentAndDrought.DroughtRestrictionByID[LIdentifier];
      if (LDroughtRestriction <> nil) then
      begin
        LDialogValidator := TDroughtRestrictionStructureValidator.Create(nil,FAppModules);
        APageControl.AddValidator(LDialogValidator);
        LDialogValidator.Initialise;
        TDroughtRestrictionStructureValidator(LDialogValidator).Identifier := LIdentifier;
        LDialogValidator.PopulateDataViewer;
        LDialogValidator.LanguageHasChanged;
      end;
    end
    else
    if (LViewName = mdvnRunTitle) then
    begin
      LDialogValidator := TYieldRunTitleValidator.Create(nil,FAppModules);
      APageControl.AddValidator(LDialogValidator);
      LDialogValidator.Initialise;
      LDialogValidator.PopulateDataViewer;
      LDialogValidator.LanguageHasChanged;
    end
    else
    if (LViewName = mdvnReservoir) or (LViewName = mdvnPCDDam) or (LViewName = mdvnUndegroundDam) then
    begin
      lReservoir := TYieldModelDataObject(FAppModules.Model.ModelData).
                      NetworkElementData.ReservoirList.ReservoirOrNodeByIdentifier[LIdentifier];

      if(lReservoir <> nil) and (LViewName = mdvnPCDDam) and
        (lReservoir.ReservoirConfigurationData.NodeType in [ntNodeWithInflow,ntNodeWithoutInflow]) then
      begin
        //Node Properties
        LDialogValidator := TNodePropertiesValidator.Create(nil,FAppModules);
        APageControl.AddValidator(LDialogValidator);
        LDialogValidator.Initialise;
        TNodePropertiesValidator(LDialogValidator).Identifier := LIdentifier;
        LDialogValidator.PopulateDataViewer;
        LDialogValidator.LanguageHasChanged;

        LMine := TYieldModelDataObject(FAppModules.Model.ModelData).CastNetworkFeaturesData.
                 CastMineList.MineByPCDNumber[LIdentifier];

        LDialogValidator := TGeneralFlowChannelValidator.Create(nil,FAppModules);
        APageControl.AddValidator(LDialogValidator);
        LDialogValidator.Initialise;

        if (LMine <> nil) AND (LMine.PCDChannel <> nil) then
          TGeneralFlowChannelValidator(LDialogValidator).Identifier         := LMine.PCDChannel.ChannelNumber;
        TGeneralFlowChannelValidator(LDialogValidator).Heading              := 'NetworkFeatures.MineToPCDChannelProperties';
        TGeneralFlowChannelValidator(LDialogValidator).UpStreamEnabled      := False;
        TGeneralFlowChannelValidator(LDialogValidator).DownStreamEnabled    := False;
        TGeneralFlowChannelValidator(LDialogValidator).SelectChannelEnabled := False;
        TGeneralFlowChannelValidator(LDialogValidator).Mode                 := gfcvmMineToPCD;
        LDialogValidator.PopulateDataViewer;
        LDialogValidator.LanguageHasChanged;

        //Reservoir Catchment Proportions
        if(lReservoir.ReservoirConfigurationData.NodeType = ntNodeWithInflow) then
        begin
          LDialogValidator := TReservoirCatchmentProportionsValidator.Create(nil,FAppModules);
          APageControl.AddValidator(LDialogValidator);
          LDialogValidator.Initialise;
          TReservoirCatchmentProportionsValidator(LDialogValidator).Identifier := LIdentifier;
          TReservoirCatchmentProportionsValidator(LDialogValidator).ViewMode :=  vmEditableSelect;
          LDialogValidator.PopulateDataViewer;
          LDialogValidator.LanguageHasChanged;
        end;
      end
      else
      begin

        //Reservoir Properties
        LDialogValidator := TReservoirPropertiesValidator.Create(nil,FAppModules);
        APageControl.AddValidator(LDialogValidator);
        LDialogValidator.Initialise;
        TReservoirPropertiesValidator(LDialogValidator).Identifier := LIdentifier;
        LDialogValidator.PopulateDataViewer;
        LDialogValidator.LanguageHasChanged;

        //Reservoir Physical Characteristics
        LDialogValidator := TReservoirPhysicalCharacteristicsValidator.Create(nil,FAppModules);
        APageControl.AddValidator(LDialogValidator);
        LDialogValidator.Initialise;
        TReservoirPhysicalCharacteristicsValidator(LDialogValidator).Identifier := LIdentifier;
        LDialogValidator.PopulateDataViewer;
        LDialogValidator.LanguageHasChanged;


        if (LViewName = mdvnPCDDam) then
        begin
          LMine := TYieldModelDataObject(FAppModules.Model.ModelData).CastNetworkFeaturesData.
                   CastMineList.MineByPCDNumber[LIdentifier];

          LDialogValidator := TGeneralFlowChannelValidator.Create(nil,FAppModules);
          APageControl.AddValidator(LDialogValidator);
          LDialogValidator.Initialise;

          if (LMine <> nil) AND (LMine.PCDChannel <> nil) then
            TGeneralFlowChannelValidator(LDialogValidator).Identifier         := LMine.PCDChannel.ChannelNumber;
          TGeneralFlowChannelValidator(LDialogValidator).Heading              := 'NetworkFeatures.MineToPCDChannelProperties';
          TGeneralFlowChannelValidator(LDialogValidator).UpStreamEnabled      := False;
          TGeneralFlowChannelValidator(LDialogValidator).DownStreamEnabled    := False;
          TGeneralFlowChannelValidator(LDialogValidator).SelectChannelEnabled := False;
          TGeneralFlowChannelValidator(LDialogValidator).Mode                 := gfcvmMineToPCD;
          LDialogValidator.PopulateDataViewer;
          LDialogValidator.LanguageHasChanged;
        end;

        //Reservoir Evaporation
        if (LViewName <> mdvnUndegroundDam) then
        begin
          LDialogValidator := TReservoirEvaporationValidator.Create(nil,FAppModules);
          APageControl.AddValidator(LDialogValidator);
          LDialogValidator.Initialise;
          TReservoirEvaporationValidator(LDialogValidator).Identifier := LIdentifier;
          LDialogValidator.PopulateDataViewer;
          LDialogValidator.LanguageHasChanged;
        end;

        if (LViewName = mdvnUndegroundDam) then
        begin

          LChannel        := TYieldModelDataObject(FAppModules.Model.ModelData).CastNetworkFeaturesData.
                             CastMineList.ChannelToUnderGroundDamByDamNumber[LIdentifier];

          LDialogValidator := TGeneralFlowChannelValidator.Create(nil,FAppModules);
          APageControl.AddValidator(LDialogValidator);
          LDialogValidator.Initialise;
          if (LChannel <> nil) then
            TGeneralFlowChannelValidator(LDialogValidator).Identifier         := LChannel.ChannelNumber;
          TGeneralFlowChannelValidator(LDialogValidator).Heading              := 'NetworkFeatures.MineToUndergroundChannelProperties';
          TGeneralFlowChannelValidator(LDialogValidator).UpStreamEnabled      := False;
          TGeneralFlowChannelValidator(LDialogValidator).DownStreamEnabled    := False;
          TGeneralFlowChannelValidator(LDialogValidator).SelectChannelEnabled := False;
          TGeneralFlowChannelValidator(LDialogValidator).Mode                 := gfcvmMineToUndeground;
          LDialogValidator.PopulateDataViewer;
          LDialogValidator.LanguageHasChanged;
        end;



        //Reservoir Catchment Proportions
        LDialogValidator := TReservoirCatchmentProportionsValidator.Create(nil,FAppModules);
        APageControl.AddValidator(LDialogValidator);
        LDialogValidator.Initialise;
        TReservoirCatchmentProportionsValidator(LDialogValidator).Identifier := LIdentifier;
        TReservoirCatchmentProportionsValidator(LDialogValidator).ViewMode :=  vmEditableSelect;
        LDialogValidator.PopulateDataViewer;
        LDialogValidator.LanguageHasChanged;

        //Reservoir Penalty
        LDialogValidator := TReservoirPenaltyValidator.Create(nil,FAppModules);
        APageControl.AddValidator(LDialogValidator);
        LDialogValidator.Initialise;
        TReservoirPenaltyValidator(LDialogValidator).ReservoirNumber := LIdentifier;
        LDialogValidator.PopulateDataViewer;
        LDialogValidator.LanguageHasChanged;
        TReservoirPenaltyValidator(LDialogValidator).ViewMode := vmEditableSelect;

        //Reservoir Zone Elevations
        LDialogValidator := TReservoirZoneElevationsValidator.Create(nil,FAppModules);
        APageControl.AddValidator(LDialogValidator);
        LDialogValidator.Initialise;
        TReservoirZoneElevationsValidator(LDialogValidator).Identifier := LIdentifier;
        LDialogValidator.PopulateDataViewer;
        LDialogValidator.LanguageHasChanged;

        //Reservoir Time Control
        if Assigned(lReservoir) and (lReservoir.TimeControl <> nil) then
        begin
          LDialogValidator := TReservoirTimeControlValidator.Create(nil,FAppModules);
          APageControl.AddValidator(LDialogValidator);
          LDialogValidator.Initialise;
          TReservoirTimeControlValidator(LDialogValidator).Identifier := LIdentifier;
          LDialogValidator.PopulateDataViewer;
          LDialogValidator.LanguageHasChanged;
        end;

        LChannel := TYieldModelDataObject(FAppModules.Model.ModelData).
                    NetworkElementData.ChannelList.ChannelByChannelNumber[lReservoir.ReservoirConfigurationData.NaturalInflowChannel];
        if (LChannel <> nil) then
        begin
          LDialogValidator := TGeneralFlowChannelValidator.Create(nil,FAppModules);
          APageControl.AddValidator(LDialogValidator);
          LDialogValidator.Initialise;
          TGeneralFlowChannelValidator(LDialogValidator).Identifier := LChannel.ChannelNumber;
          TGeneralFlowChannelValidator(LDialogValidator).Heading       := 'NetworkFeatures.NaturalInflowChannel';
          TGeneralFlowChannelValidator(LDialogValidator).UpStreamEnabled := True;
          TGeneralFlowChannelValidator(LDialogValidator).DownStreamEnabled := FALSE;
          TGeneralFlowChannelValidator(LDialogValidator).SelectChannelEnabled := FALSE;
          LDialogValidator.PopulateDataViewer;
          LDialogValidator.LanguageHasChanged;
        end;

      end;
    end;

    if (LViewName = mdvnPowerPlant) then
    begin
      lPowerPlant := TYieldModelDataObject(FAppModules.Model.ModelData).
                       NetworkFeaturesData.PowerPlantList.PowerPlantByID[LIdentifier];
      // Power Plant
      LDialogValidator := TPowerPlantValidator.Create(nil,FAppModules);
      APageControl.AddValidator(LDialogValidator);
      LDialogValidator.Initialise;
      TPowerPlantValidator(LDialogValidator).Identifier := LIdentifier;
      TPowerPlantValidator(LDialogValidator).FeatureID := LIdentifier;
      LDialogValidator.PopulateDataViewer;
      LDialogValidator.LanguageHasChanged;

      // Power Channel
      LDialogValidator := TGeneralFlowChannelValidator.Create(nil,FAppModules);
      APageControl.AddValidator(LDialogValidator);
      LDialogValidator.Initialise;
      TGeneralFlowChannelValidator(LDialogValidator).Identifier := lPowerPlant.PowerChannel.ChannelNumber;
      TGeneralFlowChannelValidator(LDialogValidator).Heading       := 'NetworkFeatures.PowerChannelProperties';
      TGeneralFlowChannelValidator(LDialogValidator).UpStreamEnabled := FALSE;
      TGeneralFlowChannelValidator(LDialogValidator).DownStreamEnabled := FALSE;
      TGeneralFlowChannelValidator(LDialogValidator).SelectChannelEnabled := FALSE;
      LDialogValidator.PopulateDataViewer;
      LDialogValidator.LanguageHasChanged;

      // Spill Channel
      LDialogValidator := TGeneralFlowChannelValidator.Create(nil,FAppModules);
      APageControl.AddValidator(LDialogValidator);
      LDialogValidator.Initialise;
      TGeneralFlowChannelValidator(LDialogValidator).Identifier := lPowerPlant.SpillChannel.ChannelNumber;
      TGeneralFlowChannelValidator(LDialogValidator).Heading       := 'NetworkFeatures.SpillChannelProperties';
      TGeneralFlowChannelValidator(LDialogValidator).UpStreamEnabled := FALSE;
      TGeneralFlowChannelValidator(LDialogValidator).DownStreamEnabled := FALSE;
      TGeneralFlowChannelValidator(LDialogValidator).SelectChannelEnabled := FALSE;
      LDialogValidator.PopulateDataViewer;
      LDialogValidator.LanguageHasChanged;

      // Power Plant Factors
      LDialogValidator := TPowerPlantFactorsValidator.Create(nil,FAppModules);
      APageControl.AddValidator(LDialogValidator);
      LDialogValidator.Initialise;
      TPowerPlantFactorsValidator(LDialogValidator).FeatureID := LIdentifier;
      LDialogValidator.PopulateDataViewer;
      LDialogValidator.LanguageHasChanged;

      // Power Plant Tailwater
      LDialogValidator := TPowerPlantTailwaterValidator.Create(nil,FAppModules);
      APageControl.AddValidator(LDialogValidator);
      LDialogValidator.Initialise;
      TPowerPlantTailwaterValidator(LDialogValidator).FeatureID := LIdentifier;
      LDialogValidator.PopulateDataViewer;
      LDialogValidator.LanguageHasChanged;

      // Power Plant Demands
      LDialogValidator := TPowerPlantDemandsValidator.Create(nil,FAppModules);
      APageControl.AddValidator(LDialogValidator);
      LDialogValidator.Initialise;
      TPowerPlantDemandsValidator(LDialogValidator).FeatureID := LIdentifier;
      LDialogValidator.PopulateDataViewer;
      LDialogValidator.LanguageHasChanged;
    end;

    if (LViewName = mdvnIrrigationArea) then
    begin
      lIrrigationArea := TYieldModelDataObject(FAppModules.Model.ModelData).
                           NetworkFeaturesData.IrrigationAreaList.IrrigationAreaByNodeNumber[LIdentifier];
      // Irrigation area
      LDialogValidator := TIrrigationAreaValidator.Create(nil,FAppModules);
      APageControl.AddValidator(LDialogValidator);
      LDialogValidator.Initialise;
      TIrrigationAreaValidator(LDialogValidator).Identifier := LIdentifier;
      LDialogValidator.PopulateDataViewer;
      LDialogValidator.LanguageHasChanged;

      // Diversion Channel
      LDialogValidator := TGeneralFlowChannelValidator.Create(nil,FAppModules);
      APageControl.AddValidator(LDialogValidator);
      LDialogValidator.Initialise;
      TGeneralFlowChannelValidator(LDialogValidator).Identifier     := lIrrigationArea.DiversionChannel.ChannelNumber;
      TGeneralFlowChannelValidator(LDialogValidator).Heading           := 'NetworkFeatures.DiversionChannelProperties';
      TGeneralFlowChannelValidator(LDialogValidator).UpStreamEnabled   := FALSE;
      TGeneralFlowChannelValidator(LDialogValidator).DownStreamEnabled := FALSE;
      TGeneralFlowChannelValidator(LDialogValidator).SelectChannelEnabled := FALSE;
      LDialogValidator.PopulateDataViewer;
      LDialogValidator.LanguageHasChanged;

      // Consumptive Channel
      LDialogValidator := TGeneralFlowChannelValidator.Create(nil,FAppModules);
      APageControl.AddValidator(LDialogValidator);
      LDialogValidator.Initialise;
      TGeneralFlowChannelValidator(LDialogValidator).Identifier        := lIrrigationArea.ConsumptiveChannel.ChannelNumber;
      TGeneralFlowChannelValidator(LDialogValidator).Heading           := 'NetworkFeatures.ConsumptiveChannelProperties';
      TGeneralFlowChannelValidator(LDialogValidator).UpStreamEnabled   := FALSE;
      TGeneralFlowChannelValidator(LDialogValidator).DownStreamEnabled := FALSE;
      TGeneralFlowChannelValidator(LDialogValidator).SelectChannelEnabled := FALSE;
      LDialogValidator.PopulateDataViewer;
      LDialogValidator.LanguageHasChanged;

      // Return Flow Channel
      LDialogValidator := TGeneralFlowChannelValidator.Create(nil,FAppModules);
      APageControl.AddValidator(LDialogValidator);
      LDialogValidator.Initialise;
      TGeneralFlowChannelValidator(LDialogValidator).Identifier        := lIrrigationArea.ReturnFlowChannel.ChannelNumber;
      TGeneralFlowChannelValidator(LDialogValidator).Heading           := 'NetworkFeatures.ReturnFlowChannelProperties';
      TGeneralFlowChannelValidator(LDialogValidator).UpStreamEnabled   := FALSE;
      TGeneralFlowChannelValidator(LDialogValidator).DownStreamEnabled := FALSE;
      TGeneralFlowChannelValidator(LDialogValidator).SelectChannelEnabled := FALSE;
      LDialogValidator.PopulateDataViewer;
      LDialogValidator.LanguageHasChanged;
    end;

  (*if (LViewName = mdvnIrrigationBlock) then
    begin
      //Irrigation Block
      LDialogValidator := TIrrBlockPropertiesValidator.Create(nil,FAppModules);
      APageControl.AddValidator(LDialogValidator);
      LDialogValidator.Initialise;
      TIrrBlockPropertiesValidator(LDialogValidator).Identifier := LIdentifier;
      LDialogValidator.PopulateDataViewer;
      LDialogValidator.LanguageHasChanged;

      // Diversion Channel
      LDialogValidator := TGeneralFlowChannelValidator.Create(nil,FAppModules);
      APageControl.AddValidator(LDialogValidator);
      LDialogValidator.Initialise;
      lIrrigationBlock  := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.IrrigationBlockList.IrrigationBlockByBlockNodeNumber[LIdentifier];
      if(lIrrigationBlock <> nil) and (lIrrigationBlock.DiversionChannel <> nil) then
        TGeneralFlowChannelValidator(LDialogValidator).Identifier         := lIrrigationBlock.DiversionChannel.ChannelNumber;
      TGeneralFlowChannelValidator(LDialogValidator).Heading              := 'NetworkFeatures.DiversionChannelProperties';
      TGeneralFlowChannelValidator(LDialogValidator).UpStreamEnabled      := FALSE;
      TGeneralFlowChannelValidator(LDialogValidator).DownStreamEnabled    := FALSE;
      TGeneralFlowChannelValidator(LDialogValidator).SelectChannelEnabled := FALSE;
      TGeneralFlowChannelValidator(LDialogValidator).Mode                 :=  gfcvmIrrBlockUpstream;
      LDialogValidator.PopulateDataViewer;
      LDialogValidator.LanguageHasChanged;

      // Return Flow Channel
            LDialogValidator := TGeneralFlowChannelValidator.Create(nil,FAppModules);
      APageControl.AddValidator(LDialogValidator);
      LDialogValidator.Initialise;
      lIrrigationBlock  := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.IrrigationBlockList.IrrigationBlockByBlockNodeNumber[LIdentifier];
      if(lIrrigationBlock <> nil) and (lIrrigationBlock.ReturnFlowChannel <> nil) then
        TGeneralFlowChannelValidator(LDialogValidator).Identifier         := lIrrigationBlock.ReturnFlowChannel.ChannelNumber;
      TGeneralFlowChannelValidator(LDialogValidator).Heading              := 'NetworkFeatures.ReturnFlowChannelProperties';
      TGeneralFlowChannelValidator(LDialogValidator).UpStreamEnabled      := FALSE;
      TGeneralFlowChannelValidator(LDialogValidator).DownStreamEnabled    := FALSE;
      TGeneralFlowChannelValidator(LDialogValidator).SelectChannelEnabled := FALSE;
      TGeneralFlowChannelValidator(LDialogValidator).Mode                 :=  gfcvmIrrBlockDownstream;
      LDialogValidator.PopulateDataViewer;
      LDialogValidator.LanguageHasChanged;

      //Irrigated Area
      LDialogValidator := TIrrBlockIrrigatedAreaValidator.Create(nil,FAppModules);
      APageControl.AddValidator(LDialogValidator);
      LDialogValidator.Initialise;
      TIrrBlockIrrigatedAreaValidator(LDialogValidator).Identifier := LIdentifier;
      LDialogValidator.PopulateDataViewer;
      LDialogValidator.LanguageHasChanged;

      //Evaporation
      LDialogValidator := TIrrBlockClimateValidator.Create(nil,FAppModules);
      APageControl.AddValidator(LDialogValidator);
      LDialogValidator.Initialise;
      TIrrBlockClimateValidator(LDialogValidator).Identifier := LIdentifier;
      LDialogValidator.PopulateDataViewer;
      LDialogValidator.LanguageHasChanged;


      //Crop Water
      LDialogValidator := TIrrBlockCropRequirementValidator.Create(nil,FAppModules);
      APageControl.AddValidator(LDialogValidator);
      LDialogValidator.Initialise;
      TIrrBlockCropRequirementValidator(LDialogValidator).Identifier := LIdentifier;
      LDialogValidator.PopulateDataViewer;
      LDialogValidator.LanguageHasChanged;

      LDialogValidator := TIrrBlockGrowthValidator.Create(nil,FAppModules);
      APageControl.AddValidator(LDialogValidator);
      LDialogValidator.Initialise;
      TIrrBlockGrowthValidator(LDialogValidator).Identifier := LIdentifier;
      LDialogValidator.PopulateDataViewer;
      LDialogValidator.LanguageHasChanged;


      if(lIrrigationBlock <> nil) and (lIrrigationBlock.IrrigationBlockType = 4) then
      begin
        LDialogValidator := TIrrBlockGrowthType4Validator.Create(nil,FAppModules);
        APageControl.AddValidator(LDialogValidator);
        LDialogValidator.Initialise;
        TIrrBlockGrowthType4Validator(LDialogValidator).Identifier := LIdentifier;
        LDialogValidator.PopulateDataViewer;
        LDialogValidator.LanguageHasChanged;
      end;
    end;
    *)


    (*if (LViewName = mdvnIrrigationBlock) then
    begin
      //Irrigation Block
      LDialogValidator := TIrrigationBlockValidator.Create(nil,FAppModules);
      APageControl.AddValidator(LDialogValidator);
      LDialogValidator.Initialise;
      TIrrigationBlockValidator(LDialogValidator).Identifier := LIdentifier;
      LDialogValidator.PopulateDataViewer;
      LDialogValidator.LanguageHasChanged;

      // Diversion Channel
      LDialogValidator := TGeneralFlowChannelValidator.Create(nil,FAppModules);
      APageControl.AddValidator(LDialogValidator);
      LDialogValidator.Initialise;
      lIrrigationBlock  := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.IrrigationBlockList.IrrigationBlockByBlockNodeNumber[LIdentifier];
      if(lIrrigationBlock <> nil) and (lIrrigationBlock.DiversionChannel <> nil) then
        TGeneralFlowChannelValidator(LDialogValidator).Identifier      := lIrrigationBlock.DiversionChannel.ChannelNumber;
      TGeneralFlowChannelValidator(LDialogValidator).Heading           := 'NetworkFeatures.DiversionChannelProperties';
      TGeneralFlowChannelValidator(LDialogValidator).UpStreamEnabled   := FALSE;
      TGeneralFlowChannelValidator(LDialogValidator).DownStreamEnabled := FALSE;
      TGeneralFlowChannelValidator(LDialogValidator).SelectChannelEnabled := FALSE;
      TGeneralFlowChannelValidator(LDialogValidator).Mode               :=  gfcvmIrrBlockUpstream;
      LDialogValidator.PopulateDataViewer;
      LDialogValidator.LanguageHasChanged;

      // Return Flow Channel
      LDialogValidator := TIrrigationBlockReturnFlowChannelValidator.Create(nil,FAppModules);
      APageControl.AddValidator(LDialogValidator);
      LDialogValidator.Initialise;
      TIrrigationBlockReturnFlowChannelValidator(LDialogValidator).Identifier := LIdentifier;
      LDialogValidator.PopulateDataViewer;
      LDialogValidator.LanguageHasChanged;

      //Evaporation
      LDialogValidator := TEvaporationValidator.Create(nil,FAppModules);
      APageControl.AddValidator(LDialogValidator);
      LDialogValidator.Initialise;
      TEvaporationValidator(LDialogValidator).Identifier := LIdentifier;
      LDialogValidator.PopulateDataViewer;
      LDialogValidator.LanguageHasChanged;

      //Crop Water
      LDialogValidator := TCropWaterValidator.Create(nil,FAppModules);
      APageControl.AddValidator(LDialogValidator);
      LDialogValidator.Initialise;
      TCropWaterValidator(LDialogValidator).Identifier := LIdentifier;
      LDialogValidator.PopulateDataViewer;
      LDialogValidator.LanguageHasChanged;

      //Soil Properties
      LDialogValidator := TSoilPropertiesValidator.Create(nil,FAppModules);
      APageControl.AddValidator(LDialogValidator);
      LDialogValidator.Initialise;
      TSoilPropertiesValidator(LDialogValidator).Identifier := LIdentifier;
      LDialogValidator.PopulateDataViewer;
      LDialogValidator.LanguageHasChanged;

      //new.......................

        //Irrigated Area
      (*LDialogValidator := TIrrBlockIrrigatedAreaValidator.Create(nil,FAppModules);
      APageControl.AddValidator(LDialogValidator);
      LDialogValidator.Initialise;
      TIrrBlockIrrigatedAreaValidator(LDialogValidator).Identifier := LIdentifier;
      LDialogValidator.PopulateDataViewer;
      LDialogValidator.LanguageHasChanged;

      //Evaporation
     (* LDialogValidator := TIrrBlockClimateValidator.Create(nil,FAppModules);
      APageControl.AddValidator(LDialogValidator);
      LDialogValidator.Initialise;
      TIrrBlockClimateValidator(LDialogValidator).Identifier := LIdentifier;
      LDialogValidator.PopulateDataViewer;
      LDialogValidator.LanguageHasChanged;


      //Crop Water
     { LDialogValidator := TIrrBlockCropRequirementValidator.Create(nil,FAppModules);
      APageControl.AddValidator(LDialogValidator);
      LDialogValidator.Initialise;
      TIrrBlockCropRequirementValidator(LDialogValidator).Identifier := LIdentifier;
      LDialogValidator.PopulateDataViewer;
      LDialogValidator.LanguageHasChanged;
                }
      LDialogValidator := TIrrBlockGrowthValidator.Create(nil,FAppModules);
      APageControl.AddValidator(LDialogValidator);
      LDialogValidator.Initialise;
      TIrrBlockGrowthValidator(LDialogValidator).Identifier := LIdentifier;
      LDialogValidator.PopulateDataViewer;
      LDialogValidator.LanguageHasChanged;

      if(lIrrigationBlock <> nil) and (lIrrigationBlock.IrrigationBlockType = 4) then
      begin
        LDialogValidator := TIrrBlockGrowthType4Validator.Create(nil,FAppModules);
        APageControl.AddValidator(LDialogValidator);
        LDialogValidator.Initialise;
        TIrrBlockGrowthType4Validator(LDialogValidator).Identifier := LIdentifier;
        LDialogValidator.PopulateDataViewer;
        LDialogValidator.LanguageHasChanged;
      end;

    end;
    *)
    if (LViewName = mdvnIrrigationBlock) then
    begin

          //Irrigation Block
      LDialogValidator := TIrrigationBlockValidator.Create(nil,FAppModules);
      APageControl.AddValidator(LDialogValidator);
      LDialogValidator.Initialise;
      TIrrigationBlockValidator(LDialogValidator).Identifier := LIdentifier;
      LDialogValidator.PopulateDataViewer;
      LDialogValidator.LanguageHasChanged;

      // Diversion Channel
      LDialogValidator := TGeneralFlowChannelValidator.Create(nil,FAppModules);
      APageControl.AddValidator(LDialogValidator);
      LDialogValidator.Initialise;
      lIrrigationBlock  := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.IrrigationBlockList.IrrigationBlockByBlockNodeNumber[LIdentifier];
      if(lIrrigationBlock <> nil) and (lIrrigationBlock.DiversionChannel <> nil) then
        TGeneralFlowChannelValidator(LDialogValidator).Identifier      := lIrrigationBlock.DiversionChannel.ChannelNumber;
      TGeneralFlowChannelValidator(LDialogValidator).Heading           := 'NetworkFeatures.DiversionChannelProperties';
      TGeneralFlowChannelValidator(LDialogValidator).UpStreamEnabled   := FALSE;
      TGeneralFlowChannelValidator(LDialogValidator).DownStreamEnabled := FALSE;
      TGeneralFlowChannelValidator(LDialogValidator).SelectChannelEnabled := FALSE;
      TGeneralFlowChannelValidator(LDialogValidator).Mode               :=  gfcvmIrrBlockUpstream;
      LDialogValidator.PopulateDataViewer;
      LDialogValidator.LanguageHasChanged;

      // Return Flow Channel
      LDialogValidator := TIrrigationBlockReturnFlowChannelValidator.Create(nil,FAppModules);
      APageControl.AddValidator(LDialogValidator);
      LDialogValidator.Initialise;
      TIrrigationBlockReturnFlowChannelValidator(LDialogValidator).Identifier := LIdentifier;
      LDialogValidator.PopulateDataViewer;
      LDialogValidator.LanguageHasChanged;

      //Evaporation
      LDialogValidator := TEvaporationValidator.Create(nil,FAppModules);
      APageControl.AddValidator(LDialogValidator);
      LDialogValidator.Initialise;
      TEvaporationValidator(LDialogValidator).Identifier := LIdentifier;
      LDialogValidator.PopulateDataViewer;
      LDialogValidator.LanguageHasChanged;

      //Crop Water
      LDialogValidator := TCropWaterValidator.Create(nil,FAppModules);
      APageControl.AddValidator(LDialogValidator);
      LDialogValidator.Initialise;
      TCropWaterValidator(LDialogValidator).Identifier := LIdentifier;
      LDialogValidator.PopulateDataViewer;
      LDialogValidator.LanguageHasChanged;

      //Soil Properties
      LDialogValidator := TSoilPropertiesValidator.Create(nil,FAppModules);
      APageControl.AddValidator(LDialogValidator);
      LDialogValidator.Initialise;
      TSoilPropertiesValidator(LDialogValidator).Identifier := LIdentifier;
      LDialogValidator.PopulateDataViewer;
      LDialogValidator.LanguageHasChanged;
    end;


    if (LViewName = mdvnNodesWithInflow) then
    begin
      //Node Properties
      LDialogValidator := TNodePropertiesValidator.Create(nil,FAppModules);
      APageControl.AddValidator(LDialogValidator);
      LDialogValidator.Initialise;
      TNodePropertiesValidator(LDialogValidator).Identifier := LIdentifier;
      LDialogValidator.PopulateDataViewer;
      LDialogValidator.LanguageHasChanged;

      //Reservoir Catchment Proportions
      LDialogValidator := TReservoirCatchmentProportionsValidator.Create(nil,FAppModules);
      APageControl.AddValidator(LDialogValidator);
      LDialogValidator.Initialise;
      TReservoirCatchmentProportionsValidator(LDialogValidator).Identifier := LIdentifier;
      TReservoirCatchmentProportionsValidator(LDialogValidator).ViewMode :=  vmEditableSelect;
      LDialogValidator.PopulateDataViewer;
      LDialogValidator.LanguageHasChanged;

      if (FAppModules.StudyArea.ModelCode = CPlanning) then
      begin
        lReservoir :=   TYieldModelDataObject(FAppModules.Model.ModelData).
                        NetworkElementData.ReservoirList.ReservoirOrNodeByIdentifier[LIdentifier];
        if lReservoir <> nil then
          LChannel := TYieldModelDataObject(FAppModules.Model.ModelData).
                    NetworkElementData.ChannelList.ChannelByChannelNumber
                             [lReservoir.ReservoirConfigurationData.NaturalInflowChannel];

        if (LChannel <> nil) then
        begin
          LDialogValidator := TGeneralFlowChannelValidator.Create(nil,FAppModules);
          APageControl.AddValidator(LDialogValidator);
          LDialogValidator.Initialise;
          TGeneralFlowChannelValidator(LDialogValidator).Identifier := LChannel.ChannelNumber;
          TGeneralFlowChannelValidator(LDialogValidator).Heading       := 'NetworkFeatures.NaturalInflowChannel';
          TGeneralFlowChannelValidator(LDialogValidator).UpStreamEnabled := FALSE;
          TGeneralFlowChannelValidator(LDialogValidator).DownStreamEnabled := FALSE;
          TGeneralFlowChannelValidator(LDialogValidator).SelectChannelEnabled := FALSE;
          LDialogValidator.PopulateDataViewer;
          LDialogValidator.LanguageHasChanged;
        end;

      end;
    end;

    if (LViewName = mdvnNodesWithoutInFlow) then
    begin
      //Node Properties
      LDialogValidator := TNodePropertiesValidator.Create(nil,FAppModules);
      APageControl.AddValidator(LDialogValidator);
      LDialogValidator.Initialise;
      TNodePropertiesValidator(LDialogValidator).Identifier := LIdentifier;
      LDialogValidator.PopulateDataViewer;
      LDialogValidator.LanguageHasChanged;
    end;

    if ((LViewName = mdvnChannel) OR (LViewName = mdvnMasterControlConfiguration)) then
    begin
      LChannel := TYieldModelDataObject(FAppModules.Model.ModelData).
                    NetworkElementData.ChannelList.ChannelByChannelNumber[LIdentifier];
      if Assigned(LChannel) then
      begin
        LChannelType := LChannel.ChannelType;
        if (LChannelType in [1,13]) then Exit;

        lMasterControlFeature := LChannel.MasterControlFeature;
        lMinimumConstraint    := lChannel.MinimumFlowConstraint;
        lLossFeature          := LChannel.LossFeature;
        lMinMaxConstraint     := LChannel.MinMaxFlowConstraint;
        lPumpingFeature       := LChannel.PumpingFeature;
        lDemandFeature        := LChannel.SpecifiedDemandFeature;
        lDivFeature           := lChannel.DiversionFeature;
        lPhysConstraint       := lChannel.PhysicalFlowConstraint;
        lIFRFeature           := lChannel.IFRFeature;
        lSpecInflow           := lChannel.SpecifiedInflowFeature;
        LWaterDemandFeature   := lChannel.WaterDemandFeature;


        LDialogValidator := TGeneralFlowChannelValidator.Create(nil,FAppModules);
        APageControl.AddValidator(LDialogValidator);
        LDialogValidator.Initialise;
        TGeneralFlowChannelValidator(LDialogValidator).Identifier := LIdentifier;
        if (LChannelType in [12]) then
          TGeneralFlowChannelValidator(LDialogValidator).SelectChannelEnabled := FALSE;

        if (lChannel.ChannelType = ctIrrigationBlockInflowChannel) then
        begin
          TGeneralFlowChannelValidator(LDialogValidator).Heading           := 'NetworkFeatures.DiversionChannelProperties';
          TGeneralFlowChannelValidator(LDialogValidator).UpStreamEnabled   := FALSE;
          TGeneralFlowChannelValidator(LDialogValidator).DownStreamEnabled := FALSE;
          TGeneralFlowChannelValidator(LDialogValidator).SelectChannelEnabled := FALSE;
          TGeneralFlowChannelValidator(LDialogValidator).Mode := gfcvmIrrBlockUpstream;
        end;

        if (lChannel.ChannelType = ctIrrigationBlockReturnFlowChannel) then
          TGeneralFlowChannelValidator(LDialogValidator).Mode := gfcvmIrrBlockDownstream;

        if (lChannel.ChannelType = ctWetlandInflowChannel) then
        begin
          TGeneralFlowChannelValidator(LDialogValidator).Heading              := 'NetworkFeatures.InflowChannelProperties';
          TGeneralFlowChannelValidator(LDialogValidator).UpStreamEnabled      := FALSE;
          TGeneralFlowChannelValidator(LDialogValidator).DownStreamEnabled    := FALSE;
          TGeneralFlowChannelValidator(LDialogValidator).SelectChannelEnabled := FALSE;
          TGeneralFlowChannelValidator(LDialogValidator).Mode                 := gfcvmWetlandUpstream;
        end;

        if (lChannel.ChannelType = ctWetlandOutflowChannel) then
        begin
          TGeneralFlowChannelValidator(LDialogValidator).Heading              := 'NetworkFeatures.OutflowChannelProperties';
          TGeneralFlowChannelValidator(LDialogValidator).UpStreamEnabled      := FALSE;
          TGeneralFlowChannelValidator(LDialogValidator).DownStreamEnabled    := FALSE;
          TGeneralFlowChannelValidator(LDialogValidator).SelectChannelEnabled := FALSE;
          TGeneralFlowChannelValidator(LDialogValidator).Mode                 := gfcvmWetlandDownstream;
        end;

        if (lChannel.ChannelType = ctDemandCentreReturnFlowChannel) then
        begin
          TGeneralFlowChannelValidator(LDialogValidator).Heading              := 'NetworkFeatures.ReturnFlowChannelProperties';
          TGeneralFlowChannelValidator(LDialogValidator).UpStreamEnabled      := False;
          TGeneralFlowChannelValidator(LDialogValidator).DownStreamEnabled    := True;
          TGeneralFlowChannelValidator(LDialogValidator).SelectChannelEnabled := False;
          TGeneralFlowChannelValidator(LDialogValidator).Mode                 := gfcvmDemandCentreConsumptiveUpstream;
        end;

        if (lChannel.ChannelType = ctReclaimationPlantLossChannel) then
        begin
          TGeneralFlowChannelValidator(LDialogValidator).Heading              := 'NetworkFeatures.DemandCentreReclaimationChannelProperties';
          TGeneralFlowChannelValidator(LDialogValidator).UpStreamEnabled      := False;
          TGeneralFlowChannelValidator(LDialogValidator).DownStreamEnabled    := True;
          TGeneralFlowChannelValidator(LDialogValidator).SelectChannelEnabled := False;
          TGeneralFlowChannelValidator(LDialogValidator).Mode                 := gfcvmDemandCentreReclaimationUpstream;
        end;

        if (lChannel.ChannelType = ctMineToRiverDChannel) then
        begin
          TGeneralFlowChannelValidator(LDialogValidator).Heading              := 'NetworkFeatures.MineToRiverChannelProperties';
          TGeneralFlowChannelValidator(LDialogValidator).UpStreamEnabled      := False;
          TGeneralFlowChannelValidator(LDialogValidator).DownStreamEnabled    := True;
          TGeneralFlowChannelValidator(LDialogValidator).SelectChannelEnabled := False;
          TGeneralFlowChannelValidator(LDialogValidator).Mode                 := gfcvmMineToRiver;
        end;

        if (lChannel.ChannelType = ctMineToPCDChannel) then
        begin
          TGeneralFlowChannelValidator(LDialogValidator).Heading              := 'NetworkFeatures.MineToPCDChannelProperties';
          TGeneralFlowChannelValidator(LDialogValidator).UpStreamEnabled      := False;
          TGeneralFlowChannelValidator(LDialogValidator).DownStreamEnabled    := False;
          TGeneralFlowChannelValidator(LDialogValidator).SelectChannelEnabled := False;
          TGeneralFlowChannelValidator(LDialogValidator).Mode                 := gfcvmMineToPCD;
        end;

        if (lChannel.ChannelType = ctMineToUndergroundChannel) then
        begin
          TGeneralFlowChannelValidator(LDialogValidator).Heading              := 'NetworkFeatures.MineToUndergroundChannelProperties';
          TGeneralFlowChannelValidator(LDialogValidator).UpStreamEnabled      := False;
          TGeneralFlowChannelValidator(LDialogValidator).DownStreamEnabled    := False;
          TGeneralFlowChannelValidator(LDialogValidator).SelectChannelEnabled := False;
          TGeneralFlowChannelValidator(LDialogValidator).Mode                 := gfcvmMineToUndeground;
        end;

        if (lChannel.ChannelType = ctAquiferInflowChannel) then
        begin
          TGeneralFlowChannelValidator(LDialogValidator).Heading              := 'NetworkFeatures.GroundWaterAquiferInflowChannel';
          TGeneralFlowChannelValidator(LDialogValidator).UpStreamEnabled      := False;
          TGeneralFlowChannelValidator(LDialogValidator).DownStreamEnabled    := False;
          TGeneralFlowChannelValidator(LDialogValidator).SelectChannelEnabled := False;
          TGeneralFlowChannelValidator(LDialogValidator).Mode                 := gfcvmGroundWaterAquiferInflowChannel;
        end;

        if (lChannel.ChannelType = ctInflowFromUpstreamAquiferChannel) then
        begin
          TGeneralFlowChannelValidator(LDialogValidator).Heading              := 'NetworkFeatures.InflowFromUpstreamAquifer';
          TGeneralFlowChannelValidator(LDialogValidator).UpStreamEnabled      := True;
          TGeneralFlowChannelValidator(LDialogValidator).DownStreamEnabled    := False;
          TGeneralFlowChannelValidator(LDialogValidator).SelectChannelEnabled := False;
          TGeneralFlowChannelValidator(LDialogValidator).Mode                 := gfcvmInflowFromUpstreamAquiferChannel;
        end;

        if (lChannel.ChannelType = ctOutFlowToDownstreamAquiferChannel) then
        begin
          TGeneralFlowChannelValidator(LDialogValidator).Heading              := 'NetworkFeatures.OutflowToDownStreamAquifer';
          TGeneralFlowChannelValidator(LDialogValidator).UpStreamEnabled      := False;
          TGeneralFlowChannelValidator(LDialogValidator).DownStreamEnabled    := True;
          TGeneralFlowChannelValidator(LDialogValidator).SelectChannelEnabled := False;
          TGeneralFlowChannelValidator(LDialogValidator).Mode                 := gfcvmOutflowToDownstreamAquiferChannel;
        end;

        if (lChannel.ChannelType = ctGroundWaterBaseflowChannel) then
        begin
          TGeneralFlowChannelValidator(LDialogValidator).Heading              := 'NetworkFeatures.GroundWaterBaseFlow';
          TGeneralFlowChannelValidator(LDialogValidator).UpStreamEnabled      := False;
          TGeneralFlowChannelValidator(LDialogValidator).DownStreamEnabled    := False;
          TGeneralFlowChannelValidator(LDialogValidator).SelectChannelEnabled := False;
          TGeneralFlowChannelValidator(LDialogValidator).Mode                 := gfcvmGroundWaterBaseflowChannel;
        end;

        if (lChannel.ChannelType = ctAbstractionFromAquiferChannel) then
        begin
          TGeneralFlowChannelValidator(LDialogValidator).Heading              := 'NetworkFeatures.AbtstractionFromAquifer';
          TGeneralFlowChannelValidator(LDialogValidator).UpStreamEnabled      := False;
          TGeneralFlowChannelValidator(LDialogValidator).DownStreamEnabled    := False;
          TGeneralFlowChannelValidator(LDialogValidator).SelectChannelEnabled := False;
          TGeneralFlowChannelValidator(LDialogValidator).Mode                 := gfcvmAbtstractionFromAquiferChannel;
        end;

        if (lChannel.ChannelType = ctAbstractionFromBaseFlowChannel) then
        begin
          TGeneralFlowChannelValidator(LDialogValidator).Heading              := 'NetworkFeatures.AbstractionFromGroundWater';
          TGeneralFlowChannelValidator(LDialogValidator).UpStreamEnabled      := False;
          TGeneralFlowChannelValidator(LDialogValidator).DownStreamEnabled    := False;
          TGeneralFlowChannelValidator(LDialogValidator).SelectChannelEnabled := False;
          TGeneralFlowChannelValidator(LDialogValidator).Mode                 := gfcvmAbtstractionGroundWaterBaseflowChannel;
        end;

        if (lChannel.ChannelType = ctAquiferExcessInterflowChannel) then
        begin
          TGeneralFlowChannelValidator(LDialogValidator).Heading              := 'NetworkFeatures.ExcessInterflow';
          TGeneralFlowChannelValidator(LDialogValidator).UpStreamEnabled      := False;
          TGeneralFlowChannelValidator(LDialogValidator).DownStreamEnabled    := False;
          TGeneralFlowChannelValidator(LDialogValidator).SelectChannelEnabled := False;
          TGeneralFlowChannelValidator(LDialogValidator).Mode                 := gfcvmExcessInterflowChannel;
        end;

        if (lChannel.ChannelType = ctSurfaceRunoffChannel) then
        begin
          TGeneralFlowChannelValidator(LDialogValidator).Heading              := 'NetworkFeatures.SurfaceRunOffChannel';
          TGeneralFlowChannelValidator(LDialogValidator).UpStreamEnabled      := False;
          TGeneralFlowChannelValidator(LDialogValidator).DownStreamEnabled    := False;
          TGeneralFlowChannelValidator(LDialogValidator).SelectChannelEnabled := False;
          TGeneralFlowChannelValidator(LDialogValidator).Mode                 := gfcvmSurfaceRunoffChannel;
        end;

        if (lChannel.ChannelType = ctGroundWaterBaseFlowRemainderChannel) then
        begin
          TGeneralFlowChannelValidator(LDialogValidator).Heading              := 'NetworkFeatures.GroundWaterBaseFlowRemainder';
          TGeneralFlowChannelValidator(LDialogValidator).UpStreamEnabled      := False;
          TGeneralFlowChannelValidator(LDialogValidator).DownStreamEnabled    := False;
          TGeneralFlowChannelValidator(LDialogValidator).SelectChannelEnabled := False;
          TGeneralFlowChannelValidator(LDialogValidator).Mode                 := gfcvmGroundWaterBaseFlowRemainderChannel;
        end;

        if (lChannel.ChannelType = ctGroundWaterAbstractionChannel) then
        begin
          TGeneralFlowChannelValidator(LDialogValidator).Heading              := 'NetworkFeatures.GroundWaterAbstraction';
          TGeneralFlowChannelValidator(LDialogValidator).UpStreamEnabled      := False;
          TGeneralFlowChannelValidator(LDialogValidator).DownStreamEnabled    := True;
          TGeneralFlowChannelValidator(LDialogValidator).SelectChannelEnabled := False;
          TGeneralFlowChannelValidator(LDialogValidator).Mode                 := gfcvmGroundWaterAbstractionChannel;
        end;

        if (lChannel.ChannelType = ctOutflowToNetworkChannel) then
        begin
          TGeneralFlowChannelValidator(LDialogValidator).Heading              := 'NetworkFeatures.OutflowToNetwork';
          TGeneralFlowChannelValidator(LDialogValidator).UpStreamEnabled      := False;
          TGeneralFlowChannelValidator(LDialogValidator).DownStreamEnabled    := True;
          TGeneralFlowChannelValidator(LDialogValidator).SelectChannelEnabled := False;
          TGeneralFlowChannelValidator(LDialogValidator).Mode                 := gfcvmOutflowToNetworkChannel;
        end;

        LDialogValidator.PopulateDataViewer;
        LDialogValidator.LanguageHasChanged;


        // Channel Penalty
        LDialogValidator := TChannelPenaltyValidator.Create(nil,FAppModules);
        APageControl.AddValidator(LDialogValidator);
        LDialogValidator.Initialise;
        TChannelPenaltyValidator(LDialogValidator).ChannelNumber := LChannel.ChannelNumber;
        TChannelPenaltyValidator(lDialogValidator).ViewMode := vmEditableSelect;
        LDialogValidator.PopulateDataViewer;
        LDialogValidator.LanguageHasChanged;

        //Master Control Feature
        if(lMasterControlFeature <> nil) then
        begin
          LDialogValidator := TMasterControlChannelValidator.Create(nil,FAppModules);
          APageControl.AddValidator(LDialogValidator);
          LDialogValidator.Initialise;
          TMasterControlChannelValidator(LDialogValidator).FeatureID := lMasterControlFeature.FeatureID;
          LDialogValidator.PopulateDataViewer;
          LDialogValidator.LanguageHasChanged;
        end;

        // Minimum Flow Constraint
        if (lMinimumConstraint <> nil) then
        begin
          LDialogValidator := TMinimumFlowChannelValidator.Create(nil,FAppModules);
          APageControl.AddValidator(LDialogValidator);
          LDialogValidator.Initialise;
          TMinimumFlowChannelValidator(LDialogValidator).FeatureID := lMinimumConstraint.FeatureID;
          LDialogValidator.PopulateDataViewer;
          LDialogValidator.LanguageHasChanged;
        end;

        // Loss Feature
        if (lLossFeature <> nil) AND (lLossFeature.FeatureSubType <> 1) then
        begin
          LDialogValidator := TLossChannelValidator.Create(nil,FAppModules);
          APageControl.AddValidator(LDialogValidator);
          LDialogValidator.Initialise;
          TLossChannelValidator(LDialogValidator).Identifier := LIdentifier;
          TLossChannelValidator(LDialogValidator).FeatureID := lLossFeature.FeatureID;
          LDialogValidator.PopulateDataViewer;
          LDialogValidator.LanguageHasChanged;
        end;

        // Min-Max Feature
        if (lMinMaxConstraint <> nil) then
        begin
          LDialogValidator := TMinMaxChannelValidator.Create(nil,FAppModules);
          APageControl.AddValidator(LDialogValidator);
          LDialogValidator.Initialise;
          TMinMaxChannelValidator(LDialogValidator).Identifier := LIdentifier;
          TMinMaxChannelValidator(LDialogValidator).FeatureID := lMinMaxConstraint.FeatureID;
          LDialogValidator.PopulateDataViewer;
          LDialogValidator.LanguageHasChanged;
        end;

{          if (LMinMaxChannelGrowthFactors <> nil) and (LMinMaxConstraint <> nil) then
        begin
          LDialogValidator := TChannelDistributionValidator.Create(nil,FAppModules);
          APageControl.AddValidator(LDialogValidator);
          LDialogValidator.Initialise;
          TChannelDistributionValidator(LDialogValidator).FeatureID := LMinMaxConstraint.FeatureID;
          LDialogValidator.PopulateDataViewer;
          LDialogValidator.LanguageHasChanged;
        end;
}
        // Pumping Feature
        if (lPumpingFeature <> nil) then
        begin
          LDialogValidator := TPumpingFeatureValidator.Create(nil,FAppModules);
          APageControl.AddValidator(LDialogValidator);
          LDialogValidator.Initialise;
          TPumpingFeatureValidator(LDialogValidator).FeatureID := lPumpingFeature.FeatureID;
          LDialogValidator.PopulateDataViewer;
          LDialogValidator.LanguageHasChanged;
        end;

        // YM Demand Centre Return Flow
        {Showmessage('ask about populating this data');
        if LChannelType = 20 then
        //if (lYMDemandCentreReturnFlowFeature <> nil) then
        begin
          LDialogValidator := TYMDemandCentreReturnFlowFeatureValidator.Create(nil,FAppModules);
          APageControl.AddValidator(LDialogValidator);
          LDialogValidator.Initialise;
          //TYMDemandCentreReturnFlowFeatureValidator(LDialogValidator).FeatureID := lYMDemandCentreReturnFlowFeature.FeatureID;
          TYMDemandCentreReturnFlowFeatureValidator(LDialogValidator).FeatureID := LChannel.ChannelID;  //LIdentifier;
          LDialogValidator.PopulateDataViewer;
          LDialogValidator.LanguageHasChanged;
        end;}

        // Specified Demand Channel
        if (lDemandFeature <> nil) then
        begin
          LDialogValidator := TSpecifiedDemandChannelValidator.Create(nil,FAppModules);
          APageControl.AddValidator(LDialogValidator);
          LDialogValidator.Initialise;
          TSpecifiedDemandChannelValidator(LDialogValidator).FeatureID := lDemandFeature.FeatureID;
          LDialogValidator.PopulateDataViewer;
          LDialogValidator.LanguageHasChanged;
        end;

        // Diversion Feature
        if (lDivFeature <> nil) then
        begin
          LDialogValidator := TDiversionFeatureValidator.Create(nil,FAppModules);
          APageControl.AddValidator(LDialogValidator);
          LDialogValidator.Initialise;
          TDiversionFeatureValidator(LDialogValidator).FeatureID := lDivFeature.FeatureID;
          LDialogValidator.PopulateDataViewer;
          LDialogValidator.LanguageHasChanged;
        end;

        // Physical Flow Constraint
        if (lPhysConstraint <> nil) then
        begin
          LDialogValidator := TPhysicalFlowConstraintValidator.Create(nil,FAppModules);
          APageControl.AddValidator(LDialogValidator);
          LDialogValidator.Initialise;
          TPhysicalFlowConstraintValidator(LDialogValidator).FeatureID := lPhysConstraint.FeatureID;
          LDialogValidator.PopulateDataViewer;
          LDialogValidator.LanguageHasChanged;
        end;

        // IFR Feature
        if (lIFRFeature <> nil) then
        begin
          if (lIFRFeature.Get_ReferenceFlowType = ifrtMonthly) then
          begin
            LDialogValidator := TIFRFeatureValidator.Create(nil,FAppModules);
            APageControl.AddValidator(LDialogValidator);
            LDialogValidator.Initialise;
            TIFRFeatureValidator(LDialogValidator).FeatureID := lIFRFeature.FeatureID;
            LDialogValidator.PopulateDataViewer;
            LDialogValidator.LanguageHasChanged;
          end;
          if (lIFRFeature.Get_ReferenceFlowType = ifrrftAnnual) then
          begin
            LDialogValidator := TAnnualIFRFeatureValidator.Create(nil,FAppModules);
            APageControl.AddValidator(LDialogValidator);
            LDialogValidator.Initialise;
            TAnnualIFRFeatureValidator(LDialogValidator).FeatureID := lIFRFeature.FeatureID;
            LDialogValidator.PopulateDataViewer;
            LDialogValidator.LanguageHasChanged;
          end;
        end;
        // Specified Inflow Data
        if (lSpecInflow <> nil) then
        begin
          LDialogValidator := TSpecifiedInflowDataValidator.Create(nil,FAppModules);
          APageControl.AddValidator(LDialogValidator);
          LDialogValidator.Initialise;
          TSpecifiedInflowDataValidator(LDialogValidator).FeatureID := lSpecInflow.FeatureID;
          LDialogValidator.PopulateDataViewer;
          LDialogValidator.LanguageHasChanged;
        end;

        // Water demand feature
        if (LWaterDemandFeature <> nil) then
        begin
          LDialogValidator := TWaterDemandFeatureValidator.Create(nil,FAppModules);
          APageControl.AddValidator(LDialogValidator);
          LDialogValidator.Initialise;
          TSpecifiedInflowDataValidator(LDialogValidator).FeatureID := LWaterDemandFeature.FeatureID;
          LDialogValidator.PopulateDataViewer;
          LDialogValidator.LanguageHasChanged;
        end;

        //Channel Time Control
        if (lChannel.TimeControl <> nil) then
        begin
          LDialogValidator := TChannelTimeControlValidator.Create(nil,FAppModules);
          APageControl.AddValidator(LDialogValidator);
          LDialogValidator.Initialise;
          TChannelTimeControlValidator(LDialogValidator).Identifier := LIdentifier;
          LDialogValidator.PopulateDataViewer;
          LDialogValidator.LanguageHasChanged;
        end;


        //Channel Switch Control
        if (FAppModules.Model.ModelName = CYield) then
        begin
          if (lChannel.SwitchControlCount > 0) then
          begin
            LDialogValidator := TChannelSwitchControlValidator.Create(nil,FAppModules);
            APageControl.AddValidator(LDialogValidator);
            LDialogValidator.Initialise;
            TChannelSwitchControlValidator(LDialogValidator).Identifier := lChannel.ChannelID;//LIdentifier;
            LDialogValidator.PopulateDataViewer;
            LDialogValidator.LanguageHasChanged;
          end;
        end
        else
        begin
          if (lChannel.ChannelType <> 2) then
          begin
            LDialogValidator := TChannelSwitchControlValidator.Create(nil,FAppModules);
            APageControl.AddValidator(LDialogValidator);
            LDialogValidator.Initialise;
            TChannelSwitchControlValidator(LDialogValidator).Identifier := lChannel.ChannelID;//LIdentifier;
            LDialogValidator.PopulateDataViewer;
            LDialogValidator.LanguageHasChanged;
          end;
        end;
      end;
    end
    else
    if (LViewName = mdvnChannelPenalties) then
    begin
      //CHANNEL PENALTIES
      LDialogValidator := TChannelPenaltyValidator.Create(nil,FAppModules);
      APageControl.AddValidator(LDialogValidator);
      LDialogValidator.Initialise;
      TChannelPenaltyValidator(lDialogValidator).ViewMode := vmEditable;
      LDialogValidator.PopulateDataViewer;
      LDialogValidator.LanguageHasChanged;
    end;

    if (LViewName = mdvnWetland) then
    begin
      LWetland        := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.WetlandList.WetlandByNodeNumber[LIdentifier];
      lFeatureID      := LWetland.Identifier;

      //Wetland Properties
      LDialogValidator := TWetlandValidator.Create(nil,FAppModules);
      APageControl.AddValidator(LDialogValidator);
      LDialogValidator.Initialise;
      TWetlandValidator(LDialogValidator).Identifier := lFeatureID;
      LDialogValidator.PopulateDataViewer;
      LDialogValidator.LanguageHasChanged;

      //Inflow Channel
      LDialogValidator := TGeneralFlowChannelValidator.Create(nil,FAppModules);
      APageControl.AddValidator(LDialogValidator);
      LDialogValidator.Initialise;
      TGeneralFlowChannelValidator(LDialogValidator).Identifier     := lFeatureID;

      if(lWetland <> nil) and (lWetland.InflowChannel <> nil) then
        TGeneralFlowChannelValidator(LDialogValidator).Identifier         := lWetland.InflowChannel.ChannelNumber;
      TGeneralFlowChannelValidator(LDialogValidator).Heading              := 'NetworkFeatures.InflowChannelProperties';
      TGeneralFlowChannelValidator(LDialogValidator).UpStreamEnabled      := FALSE;
      TGeneralFlowChannelValidator(LDialogValidator).DownStreamEnabled    := FALSE;
      TGeneralFlowChannelValidator(LDialogValidator).SelectChannelEnabled := FALSE;
      TGeneralFlowChannelValidator(LDialogValidator).Mode                 := gfcvmWetlandUpstream;
      LDialogValidator.PopulateDataViewer;
      LDialogValidator.LanguageHasChanged;

      //Outflow Channel
      LDialogValidator := TGeneralFlowChannelValidator.Create(nil,FAppModules);
      APageControl.AddValidator(LDialogValidator);
      LDialogValidator.Initialise;
      TGeneralFlowChannelValidator(LDialogValidator).Identifier           := lFeatureID;
      if(LWetland <> nil) and (LWetland.OutflowChannel <> nil) then
        TGeneralFlowChannelValidator(LDialogValidator).Identifier         := LWetland.OutflowChannel.ChannelNumber;
      TGeneralFlowChannelValidator(LDialogValidator).Heading              := 'NetworkFeatures.OutflowChannelProperties';
      TGeneralFlowChannelValidator(LDialogValidator).UpStreamEnabled      := FALSE;
      TGeneralFlowChannelValidator(LDialogValidator).DownStreamEnabled    := FALSE;
      TGeneralFlowChannelValidator(LDialogValidator).SelectChannelEnabled := FALSE;
      TGeneralFlowChannelValidator(LDialogValidator).Mode                 := gfcvmWetlandDownstream;
      LDialogValidator.PopulateDataViewer;
      LDialogValidator.LanguageHasChanged;

      //Reservoir Evaporation
      LDialogValidator := TReservoirEvaporationValidator.Create(nil,FAppModules);
      APageControl.AddValidator(LDialogValidator);
      LDialogValidator.Initialise;
      TReservoirEvaporationValidator(LDialogValidator).Identifier := LWetland.NodeNumber;
      LDialogValidator.PopulateDataViewer;
      LDialogValidator.LanguageHasChanged;

      //Reservoir Physical Characteristics
      LDialogValidator := TReservoirPhysicalCharacteristicsValidator.Create(nil,FAppModules);
      APageControl.AddValidator(LDialogValidator);
      LDialogValidator.Initialise;
      TReservoirPhysicalCharacteristicsValidator(LDialogValidator).Identifier := LWetland.NodeNumber;
      LDialogValidator.PopulateDataViewer;
      LDialogValidator.LanguageHasChanged;

      //Reservoir Catchment Proportions
      LDialogValidator := TReservoirCatchmentProportionsValidator.Create(nil,FAppModules);
      APageControl.AddValidator(LDialogValidator);
      LDialogValidator.Initialise;
      TReservoirCatchmentProportionsValidator(LDialogValidator).Identifier := LWetland.NodeNumber;
      TReservoirCatchmentProportionsValidator(LDialogValidator).ViewMode :=  vmEditableSelect;
      LDialogValidator.PopulateDataViewer;
      LDialogValidator.LanguageHasChanged;

      //Reservoir Penalty
      LDialogValidator := TReservoirPenaltyValidator.Create(nil,FAppModules);
      APageControl.AddValidator(LDialogValidator);
      LDialogValidator.Initialise;
      TReservoirPenaltyValidator(LDialogValidator).ReservoirNumber := LWetland.NodeNumber;
      LDialogValidator.PopulateDataViewer;
      LDialogValidator.LanguageHasChanged;
      TReservoirPenaltyValidator(LDialogValidator).ViewMode := vmEditableSelect;

      //Reservoir Zone Elevations
      LDialogValidator := TReservoirZoneElevationsValidator.Create(nil,FAppModules);
      APageControl.AddValidator(LDialogValidator);
      LDialogValidator.Initialise;
      TReservoirZoneElevationsValidator(LDialogValidator).Identifier := LWetland.NodeNumber;
      LDialogValidator.PopulateDataViewer;
      LDialogValidator.LanguageHasChanged;
    end;

    if (LViewName = mdvnYMDemandCentre) then
    begin
      LYMDemandCentre := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.YMDemandCentreList.YMDemandCentreByNodeNumber[LIdentifier];
      lFeatureID      := LYMDemandCentre.Identifier;

      //Demand Centre Properties
      LDialogValidator := TYMDemandCentreValidator.Create(nil,FAppModules);
      APageControl.AddValidator(LDialogValidator);
      LDialogValidator.Initialise;
      TYMDemandCentreValidator(LDialogValidator).Identifier := lFeatureID;
      LDialogValidator.PopulateDataViewer;
      LDialogValidator.LanguageHasChanged;

      //Demand Centre Supply channel Properties
      LDialogValidator := TYMDemandCentreSupplyChannelValidator.Create(nil,FAppModules);
      APageControl.AddValidator(LDialogValidator);
      LDialogValidator.Initialise;
      TYMDemandCentreSupplyChannelValidator(LDialogValidator).Identifier := lFeatureID;
      LDialogValidator.PopulateDataViewer;
      LDialogValidator.LanguageHasChanged;

      //Consumptive Water Use
      {LDialogValidator := TGeneralFlowChannelValidator.Create(nil,FAppModules);
      APageControl.AddValidator(LDialogValidator);
      LDialogValidator.Initialise;
      if(LYMDemandCentre <> nil) and (LYMDemandCentre.ConsumptiveUseChannel <> nil) then
        TGeneralFlowChannelValidator(LDialogValidator).Identifier      := LYMDemandCentre.ConsumptiveUseChannel.ChannelNumber;
      TGeneralFlowChannelValidator(LDialogValidator).Heading           := 'NetworkFeatures.DemandCentreConsumptiveChannelProperties';
      TGeneralFlowChannelValidator(LDialogValidator).UpStreamEnabled   := False;
      TGeneralFlowChannelValidator(LDialogValidator).DownStreamEnabled := True;
      TGeneralFlowChannelValidator(LDialogValidator).SelectChannelEnabled := False;
      TGeneralFlowChannelValidator(LDialogValidator).Mode               := gfcvmDemandCentreConsumptiveUpstream;
      LDialogValidator.PopulateDataViewer;
      LDialogValidator.LanguageHasChanged; }

      //Demand Centre Return flow channel Properties
      LDialogValidator := TYMDemandCentreReturnFlowChannelValidator.Create(nil,FAppModules);
      APageControl.AddValidator(LDialogValidator);
      LDialogValidator.Initialise;
      TYMDemandCentreReturnFlowChannelValidator(LDialogValidator).Identifier := lFeatureID;
      LDialogValidator.PopulateDataViewer;
      LDialogValidator.LanguageHasChanged;

      //Reclaimation Plant Loss
      LDialogValidator := TGeneralFlowChannelValidator.Create(nil,FAppModules);
      APageControl.AddValidator(LDialogValidator);
      LDialogValidator.Initialise;

      if(LYMDemandCentre <> nil) and (LYMDemandCentre.ReclaimationChannel <> nil)
         and (LYMDemandCentre.ReclaimationPlantExists) then
        TGeneralFlowChannelValidator(LDialogValidator).Identifier         := LYMDemandCentre.ReclaimationChannel.ChannelNumber;

      TGeneralFlowChannelValidator(LDialogValidator).Heading              := 'NetworkFeatures.DemandCentreReclaimationChannelProperties';
      TGeneralFlowChannelValidator(LDialogValidator).UpStreamEnabled      := False;
      TGeneralFlowChannelValidator(LDialogValidator).DownStreamEnabled    := True;
      TGeneralFlowChannelValidator(LDialogValidator).SelectChannelEnabled := False;
      TGeneralFlowChannelValidator(LDialogValidator).Mode                 := gfcvmDemandCentreReclaimationUpstream;
      LDialogValidator.PopulateDataViewer;
      LDialogValidator.LanguageHasChanged;

      LDialogValidator := TYMDemandCentreEvaporationValidator.Create(nil,FAppModules);
      APageControl.AddValidator(LDialogValidator);
      LDialogValidator.Initialise;
      TYMDemandCentreEvaporationValidator(LDialogValidator).Identifier := lFeatureID;
      LDialogValidator.PopulateDataViewer;
      LDialogValidator.LanguageHasChanged;

    end;

    if (LViewName = mdvnStreamFlowReduction) then
    begin
      // Switch Definition
      LDialogValidator := TStreamFlowReductionValidator.Create(nil,FAppModules);
      APageControl.AddValidator(LDialogValidator);
      LDialogValidator.Initialise;
      TStreamFlowReductionValidator(LDialogValidator).Identifier := LIdentifier;
      LDialogValidator.PopulateDataViewer;
      LDialogValidator.LanguageHasChanged;
    end;

    if (LViewName = mdvnMine) then
    begin
      LMine := TYieldModelDataObject(FAppModules.Model.ModelData).CastNetworkFeaturesData.CastMineList.MineByNodeNumber[LIdentifier];

      LDialogValidator := TMiningValidator.Create(nil,FAppModules);
      APageControl.AddValidator(LDialogValidator);
      LDialogValidator.Initialise;
      TMiningValidator(LDialogValidator).Identifier := LIdentifier;
      LDialogValidator.PopulateDataViewer;
      LDialogValidator.LanguageHasChanged;

      LDialogValidator := TGeneralFlowChannelValidator.Create(nil,FAppModules);
      APageControl.AddValidator(LDialogValidator);
      LDialogValidator.Initialise;
      if (LMine <> nil) AND (LMine.RiverChannel <> nil) then
        TGeneralFlowChannelValidator(LDialogValidator).Identifier         := LMine.RiverChannel.ChannelNumber;
      TGeneralFlowChannelValidator(LDialogValidator).Heading              := 'NetworkFeatures.MineToRiverChannelProperties';
      TGeneralFlowChannelValidator(LDialogValidator).UpStreamEnabled      := False;
      TGeneralFlowChannelValidator(LDialogValidator).DownStreamEnabled    := True;
      TGeneralFlowChannelValidator(LDialogValidator).SelectChannelEnabled := False;
      TGeneralFlowChannelValidator(LDialogValidator).Mode                 := gfcvmMineToRiver;
      LDialogValidator.PopulateDataViewer;
      LDialogValidator.LanguageHasChanged;

      LDialogValidator := TMiningOpenCastPitValidator.Create(nil,FAppModules);
      APageControl.AddValidator(LDialogValidator);
      LDialogValidator.Initialise;
      TMiningOpenCastPitValidator(LDialogValidator).Identifier := LIdentifier;
      LDialogValidator.PopulateDataViewer;
      LDialogValidator.LanguageHasChanged;

      LDialogValidator := TMiningUnderGroundSectionValidator.Create(nil,FAppModules);
      APageControl.AddValidator(LDialogValidator);
      LDialogValidator.Initialise;
      TMiningUnderGroundSectionValidator(LDialogValidator).Identifier := LIdentifier;
      LDialogValidator.PopulateDataViewer;
      LDialogValidator.LanguageHasChanged;

      LDialogValidator := TMiningSlurryDumpValidator.Create(nil,FAppModules);
      APageControl.AddValidator(LDialogValidator);
      LDialogValidator.Initialise;
      TMiningSlurryDumpValidator(LDialogValidator).Identifier := LIdentifier;
      LDialogValidator.PopulateDataViewer;
      LDialogValidator.LanguageHasChanged;
    end;

    if (LViewName = mdvnGroundwaterSubcatchment) then
    begin
      LDialogValidator := TMineSubcatchmentValidator.Create(nil,FAppModules);
      APageControl.AddValidator(LDialogValidator);
      LDialogValidator.Initialise;
      LDialogValidator.PopulateDataViewer;
      LDialogValidator.LanguageHasChanged;
    end;

    if (LViewName = mdvnGroundWater) then
    begin
      LGroundWater := TYieldModelDataObject(FAppModules.Model.ModelData).CastNetworkFeaturesData.
                      CastGroundWaterList.GroundWaterByID[LIdentifier];

      LDialogValidator := TGroundWaterValidator.Create(nil,FAppModules);
      APageControl.AddValidator(LDialogValidator);
      LDialogValidator.Initialise;
      TGroundWaterValidator(LDialogValidator).Identifier := LIdentifier;
      LDialogValidator.PopulateDataViewer;
      LDialogValidator.LanguageHasChanged;

      LDialogValidator := TGroundWaterPitmanValidator.Create(nil,FAppModules);
      APageControl.AddValidator(LDialogValidator);
      LDialogValidator.Initialise;
      TGroundWaterPitmanValidator(LDialogValidator).Identifier := LIdentifier;
      LDialogValidator.PopulateDataViewer;
      LDialogValidator.LanguageHasChanged;
    end;

    if (LViewName = mdvnAquiferNode) then
    begin
      LGroundWater := TYieldModelDataObject(FAppModules.Model.ModelData).CastNetworkFeaturesData.
                      CastGroundWaterList.GroundWaterByNodeNumber[LIdentifier];
      if LGroundWater <> nil then
      begin
        LDialogValidator := TReservoirPropertiesValidator.Create(nil,FAppModules);
        APageControl.AddValidator(LDialogValidator);
        LDialogValidator.Initialise;
        TReservoirPropertiesValidator(LDialogValidator).Identifier := LIdentifier;
        LDialogValidator.PopulateDataViewer;
        LDialogValidator.LanguageHasChanged;

        LDialogValidator := TGeneralFlowChannelValidator.Create(nil,FAppModules);
        APageControl.AddValidator(LDialogValidator);
        LDialogValidator.Initialise;
        if (LGroundWater.AquiferInflowChannel <> nil) then
          TGeneralFlowChannelValidator(LDialogValidator).Identifier         := LGroundWater.AquiferInflowChannelNr;
        TGeneralFlowChannelValidator(LDialogValidator).Heading              := 'NetworkFeatures.GroundWaterAquiferInflowChannel';
        TGeneralFlowChannelValidator(LDialogValidator).UpStreamEnabled      := False;
        TGeneralFlowChannelValidator(LDialogValidator).DownStreamEnabled    := False;
        TGeneralFlowChannelValidator(LDialogValidator).SelectChannelEnabled := False;
        TGeneralFlowChannelValidator(LDialogValidator).Mode                 := gfcvmGroundWaterAquiferInflowChannel;
        LDialogValidator.PopulateDataViewer;
        LDialogValidator.LanguageHasChanged;

        LDialogValidator := TGeneralFlowChannelValidator.Create(nil,FAppModules);
        APageControl.AddValidator(LDialogValidator);
        LDialogValidator.Initialise;
        if(LGroundWater.InflowFromUpstreamAquiferChannel <> nil) then
          TGeneralFlowChannelValidator(LDialogValidator).Identifier         := LGroundWater.InflowFromUpstreamAquiferChannelNr;
        TGeneralFlowChannelValidator(LDialogValidator).Heading              := 'NetworkFeatures.InflowFromUpstreamAquifer';
        TGeneralFlowChannelValidator(LDialogValidator).UpStreamEnabled      := True;
        TGeneralFlowChannelValidator(LDialogValidator).DownStreamEnabled    := False;
        TGeneralFlowChannelValidator(LDialogValidator).SelectChannelEnabled := False;
        TGeneralFlowChannelValidator(LDialogValidator).Mode                 := gfcvmInflowFromUpstreamAquiferChannel;
        LDialogValidator.PopulateDataViewer;
        LDialogValidator.LanguageHasChanged;

        LDialogValidator := TGeneralFlowChannelValidator.Create(nil,FAppModules);
        APageControl.AddValidator(LDialogValidator);
        LDialogValidator.Initialise;
        if (LGroundWater.OutflowToDownstreamAquiferChannel <> nil) then
          TGeneralFlowChannelValidator(LDialogValidator).Identifier         := LGroundWater.OutflowToDownstreamAquiferChannelNr;
        TGeneralFlowChannelValidator(LDialogValidator).Heading              := 'NetworkFeatures.OutflowToDownStreamAquifer';
        TGeneralFlowChannelValidator(LDialogValidator).UpStreamEnabled      := False;
        TGeneralFlowChannelValidator(LDialogValidator).DownStreamEnabled    := True;
        TGeneralFlowChannelValidator(LDialogValidator).SelectChannelEnabled := False;
        TGeneralFlowChannelValidator(LDialogValidator).Mode                 := gfcvmOutflowToDownstreamAquiferChannel;
        LDialogValidator.PopulateDataViewer;
        LDialogValidator.LanguageHasChanged;

        //Reservoir Physical Characteristics
        LDialogValidator := TReservoirPhysicalCharacteristicsValidator.Create(nil,FAppModules);
        APageControl.AddValidator(LDialogValidator);
        LDialogValidator.Initialise;
        TReservoirPhysicalCharacteristicsValidator(LDialogValidator).Identifier := LIdentifier;
        LDialogValidator.PopulateDataViewer;
        LDialogValidator.LanguageHasChanged;

        //Reservoir Catchment Proportions
        LDialogValidator := TReservoirCatchmentProportionsValidator.Create(nil,FAppModules);
        APageControl.AddValidator(LDialogValidator);
        LDialogValidator.Initialise;
        TReservoirCatchmentProportionsValidator(LDialogValidator).Identifier := LIdentifier;
        TReservoirCatchmentProportionsValidator(LDialogValidator).ViewMode :=  vmEditableSelect;
        LDialogValidator.PopulateDataViewer;
        LDialogValidator.LanguageHasChanged;

        //Reservoir Penalty
        LDialogValidator := TReservoirPenaltyValidator.Create(nil,FAppModules);
        APageControl.AddValidator(LDialogValidator);
        LDialogValidator.Initialise;
        TReservoirPenaltyValidator(LDialogValidator).ReservoirNumber := LIdentifier;
        LDialogValidator.PopulateDataViewer;
        LDialogValidator.LanguageHasChanged;
        TReservoirPenaltyValidator(LDialogValidator).ViewMode := vmEditableSelect;

        //Reservoir Zone Elevations
        LDialogValidator := TReservoirZoneElevationsValidator.Create(nil,FAppModules);
        APageControl.AddValidator(LDialogValidator);
        LDialogValidator.Initialise;
        TReservoirZoneElevationsValidator(LDialogValidator).Identifier := LIdentifier;
        LDialogValidator.PopulateDataViewer;
        LDialogValidator.LanguageHasChanged;
      end;
    end;

    if (LViewName = mdvnBaseFlowNode) then
    begin
      LGroundWater := TYieldModelDataObject(FAppModules.Model.ModelData).CastNetworkFeaturesData.
                      CastGroundWaterList.GroundWaterByBaseFlowNumber[LIdentifier];
      if (LGroundWater <> nil) then
      begin

        LDialogValidator := TNodePropertiesValidator.Create(nil,FAppModules);
        APageControl.AddValidator(LDialogValidator);
        LDialogValidator.Initialise;

        if (LGroundWater.BaseFlowNode <> nil) then
          TNodePropertiesValidator(LDialogValidator).Identifier := LGroundWater.BaseFlowNodeNr;
        LDialogValidator.PopulateDataViewer;
        LDialogValidator.LanguageHasChanged;

        LDialogValidator := TGeneralFlowChannelValidator.Create(nil,FAppModules);
        APageControl.AddValidator(LDialogValidator);
        LDialogValidator.Initialise;
        if (LGroundWater.GroundWaterBaseflowChannel <> nil) then
          TGeneralFlowChannelValidator(LDialogValidator).Identifier         := LGroundWater.GroundWaterBaseflowChannelNr;
        TGeneralFlowChannelValidator(LDialogValidator).Heading              := 'NetworkFeatures.GroundWaterBaseFlow';
        TGeneralFlowChannelValidator(LDialogValidator).UpStreamEnabled      := False;
        TGeneralFlowChannelValidator(LDialogValidator).DownStreamEnabled    := False;
        TGeneralFlowChannelValidator(LDialogValidator).SelectChannelEnabled := False;
        TGeneralFlowChannelValidator(LDialogValidator).Mode                 := gfcvmGroundWaterBaseflowChannel;
        LDialogValidator.PopulateDataViewer;
        LDialogValidator.LanguageHasChanged;
      end;
    end;

    if (LViewName = mdvnAbstractionNode) then
    begin
      LGroundWater := TYieldModelDataObject(FAppModules.Model.ModelData).CastNetworkFeaturesData.
                        CastGroundWaterList.GroundWaterByAbstractionNodeNumber[LIdentifier];
      if (LGroundWater <> nil) then
      begin
        LDialogValidator := TNodePropertiesValidator.Create(nil,FAppModules);
        APageControl.AddValidator(LDialogValidator);
        LDialogValidator.Initialise;

        if (LGroundWater <> nil) AND (LGroundWater.BaseFlowNode <> nil) then
          TNodePropertiesValidator(LDialogValidator).Identifier         := LGroundWater.AbstractionNodeNr;
        LDialogValidator.PopulateDataViewer;
        LDialogValidator.LanguageHasChanged;

        LDialogValidator := TGeneralFlowChannelValidator.Create(nil,FAppModules);
        APageControl.AddValidator(LDialogValidator);
        LDialogValidator.Initialise;
        if (LGroundWater.AbstractionFromAquiferChannel <> nil) then
          TGeneralFlowChannelValidator(LDialogValidator).Identifier         := LGroundWater.AbstractionFromAquiferChannelNr;
        TGeneralFlowChannelValidator(LDialogValidator).Heading              := 'NetworkFeatures.AbtstractionFromAquifer';
        TGeneralFlowChannelValidator(LDialogValidator).UpStreamEnabled      := False;
        TGeneralFlowChannelValidator(LDialogValidator).DownStreamEnabled    := False;
        TGeneralFlowChannelValidator(LDialogValidator).SelectChannelEnabled := False;
        TGeneralFlowChannelValidator(LDialogValidator).Mode                 := gfcvmAbtstractionFromAquiferChannel;
        LDialogValidator.PopulateDataViewer;
        LDialogValidator.LanguageHasChanged;

        LDialogValidator := TGeneralFlowChannelValidator.Create(nil,FAppModules);
        APageControl.AddValidator(LDialogValidator);
        LDialogValidator.Initialise;
        if (LGroundWater.AbstractionFromBaseFlowChannel <> nil) then
          TGeneralFlowChannelValidator(LDialogValidator).Identifier         := LGroundWater.AbstractionFromBaseflowChannelNr;
        TGeneralFlowChannelValidator(LDialogValidator).Heading              := 'NetworkFeatures.AbstractionFromGroundWater';
        TGeneralFlowChannelValidator(LDialogValidator).UpStreamEnabled      := False;
        TGeneralFlowChannelValidator(LDialogValidator).DownStreamEnabled    := False;
        TGeneralFlowChannelValidator(LDialogValidator).SelectChannelEnabled := False;
        TGeneralFlowChannelValidator(LDialogValidator).Mode                 := gfcvmAbtstractionGroundWaterBaseflowChannel;
        LDialogValidator.PopulateDataViewer;
        LDialogValidator.LanguageHasChanged;

        LDialogValidator := TGeneralFlowChannelValidator.Create(nil,FAppModules);
        APageControl.AddValidator(LDialogValidator);
        LDialogValidator.Initialise;
        if (LGroundWater.GroundWaterAbstractionChannel <> nil) then
          TGeneralFlowChannelValidator(LDialogValidator).Identifier         := LGroundWater.GroundWaterAbstractionChannelNr;
        TGeneralFlowChannelValidator(LDialogValidator).Heading              := 'NetworkFeatures.GroundWaterAbstraction';
        TGeneralFlowChannelValidator(LDialogValidator).UpStreamEnabled      := False;
        TGeneralFlowChannelValidator(LDialogValidator).DownStreamEnabled    := True;
        TGeneralFlowChannelValidator(LDialogValidator).SelectChannelEnabled := False;
        TGeneralFlowChannelValidator(LDialogValidator).Mode                 := gfcvmGroundWaterAbstractionChannel;
        LDialogValidator.PopulateDataViewer;
        LDialogValidator.LanguageHasChanged;
      end;
    end;

    if (LViewName = mdvnCollectionNode) then
    begin
      LGroundWater := TYieldModelDataObject(FAppModules.Model.ModelData).CastNetworkFeaturesData.
                      CastGroundWaterList.GroundWaterByCollectionNodeNumber[LIdentifier];
      if (LGroundWater <> nil) then
      begin

        LDialogValidator := TNodePropertiesValidator.Create(nil,FAppModules);
        APageControl.AddValidator(LDialogValidator);
        LDialogValidator.Initialise;

        if (LGroundWater <> nil) AND (LGroundWater.CollectionNode <> nil) then
          TNodePropertiesValidator(LDialogValidator).Identifier         := LGroundWater.CollectionNodeNr;
        LDialogValidator.PopulateDataViewer;
        LDialogValidator.LanguageHasChanged;

        LDialogValidator := TGeneralFlowChannelValidator.Create(nil,FAppModules);
        APageControl.AddValidator(LDialogValidator);
        LDialogValidator.Initialise;
        if (LGroundWater.SurfaceRunoffAndSoilInterflowChannel <> nil) then
          TGeneralFlowChannelValidator(LDialogValidator).Identifier         := LGroundWater.SurfaceRunoffAndSoilInterflowChannelNr;
        TGeneralFlowChannelValidator(LDialogValidator).Heading              := 'NetworkFeatures.SurfaceRunOffChannel';
        TGeneralFlowChannelValidator(LDialogValidator).UpStreamEnabled      := False;
        TGeneralFlowChannelValidator(LDialogValidator).DownStreamEnabled    := False;
        TGeneralFlowChannelValidator(LDialogValidator).SelectChannelEnabled := False;
        TGeneralFlowChannelValidator(LDialogValidator).Mode                 := gfcvmSurfaceRunoffChannel;
        LDialogValidator.PopulateDataViewer;
        LDialogValidator.LanguageHasChanged;

        LDialogValidator := TGeneralFlowChannelValidator.Create(nil,FAppModules);
        APageControl.AddValidator(LDialogValidator);
        LDialogValidator.Initialise;
        if (LGroundWater.AquiferExcessInterflowChannel <> nil) then
          TGeneralFlowChannelValidator(LDialogValidator).Identifier         := LGroundWater.AquiferExcessInterflowChannelNr;
        TGeneralFlowChannelValidator(LDialogValidator).Heading              := 'NetworkFeatures.ExcessInterflow';
        TGeneralFlowChannelValidator(LDialogValidator).UpStreamEnabled      := False;
        TGeneralFlowChannelValidator(LDialogValidator).DownStreamEnabled    := False;
        TGeneralFlowChannelValidator(LDialogValidator).SelectChannelEnabled := False;
        TGeneralFlowChannelValidator(LDialogValidator).Mode                 := gfcvmExcessInterflowChannel;
        LDialogValidator.PopulateDataViewer;
        LDialogValidator.LanguageHasChanged;

        LDialogValidator := TGeneralFlowChannelValidator.Create(nil,FAppModules);
        APageControl.AddValidator(LDialogValidator);
        LDialogValidator.Initialise;
        if (LGroundWater.GroundWaterBaseFlowRemainderChannel <> nil) then
          TGeneralFlowChannelValidator(LDialogValidator).Identifier         := LGroundWater.GroundWaterBaseFlowRemainderChannelNr;
        TGeneralFlowChannelValidator(LDialogValidator).Heading              := 'NetworkFeatures.GroundWaterBaseFlowRemainder';
        TGeneralFlowChannelValidator(LDialogValidator).UpStreamEnabled      := False;
        TGeneralFlowChannelValidator(LDialogValidator).DownStreamEnabled    := False;
        TGeneralFlowChannelValidator(LDialogValidator).SelectChannelEnabled := False;
        TGeneralFlowChannelValidator(LDialogValidator).Mode                 := gfcvmGroundWaterBaseFlowRemainderChannel;
        LDialogValidator.PopulateDataViewer;
        LDialogValidator.LanguageHasChanged;

        LDialogValidator := TGeneralFlowChannelValidator.Create(nil,FAppModules);
        APageControl.AddValidator(LDialogValidator);
        LDialogValidator.Initialise;
        if (LGroundWater.OutflowToNetworkChannel <> nil) then
          TGeneralFlowChannelValidator(LDialogValidator).Identifier         := LGroundWater.OutflowToNetworkChannelNr;
        TGeneralFlowChannelValidator(LDialogValidator).Heading              := 'NetworkFeatures.OutflowToNetwork';
        TGeneralFlowChannelValidator(LDialogValidator).UpStreamEnabled      := False;
        TGeneralFlowChannelValidator(LDialogValidator).DownStreamEnabled    := True;
        TGeneralFlowChannelValidator(LDialogValidator).SelectChannelEnabled := False;
        TGeneralFlowChannelValidator(LDialogValidator).Mode                 := gfcvmOutflowToNetworkChannel;
        LDialogValidator.PopulateDataViewer;
        LDialogValidator.LanguageHasChanged;

      end;
    end;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldModelDataGUIManager.ViewInputData(ACommaTextContextData: String): boolean;
const OPNAME = 'TYieldModelDataGUIManager.ViewInputData';
var
  LElementType                : string;
  LViewName                   : string;
  LIdentifier                 : integer;
  lFeatureID                  : integer;
  //LGeneralNodeID              : Integer;
  LChannelType                : integer;
  LContextDataList            : TStringList;
  LDialogValidator            : TAbstractDataDialogValidator;
  LChannel                    : IGeneralFlowChannel;
  lRunConfigData              : IRunConfigurationData;
  lPowerPlant                 : IPowerPlant;
  lIrrigationArea             : IIrrigationArea;
  lMinimumConstraint          : IMinimumFlowConstraint;
  lLossFeature                : ILossFeature;
  lMinMaxConstraint           : IMinMaxFlowConstraint;
  lPumpingFeature             : IPumpingFeature;
  lDemandFeature              : ISpecifiedDemandFeature;
  lDivFeature                 : IDiversionFeature;
  lPhysConstraint             : IPhysicalFlowConstraint;
  lIFRFeature                 : IIFRFeature;
  LWaterDemandFeature         : IWaterDemandFeature;
  lMasterControlFeature       : IMasterControlFeature;
  lSpecInflow                 : ISpecifiedInflowFeature;
  lReservoir                  : IReservoirData;
  lIrrigationBlock            : IIrrigationBlock;
  LWetland                    : IWetland;
  LMine                       : IMine;
  LYMDemandCentre             : IYMDemandCentre;
  LDroughtRestriction         : IDroughtRestriction;
  LGroundWater                : IGroundWater;
//  LMinMaxChannelGrowthFactors : IMinMaxChannelGrowthFactors;
//  LGrowthFactors              : IGrowthFactors;
begin
  Result := False;
  try

    if (Trim(ACommaTextContextData) = '') then Exit;

    LContextDataList := TStringList.Create;
    try
      LContextDataList.CommaText := ACommaTextContextData;
      LViewName := UpperCase(Trim(LContextDataList.Values['VIEWNAME']));
      if(LViewName = '') then Exit;

      LIdentifier   := StrToInt(LContextDataList.Values['MODELELEMENTID']);
      LElementType  := UpperCase(Trim(LContextDataList.Values['MODELELEMENTTYPE']));
    finally
      FreeAndNil(LContextDataList);
    end;

    if (LViewName = mdvnInputViewDataGrid) then
    begin
      LDialogValidator := TInputGridValidator.Create(FPopupForm,FAppModules);
      FPopupFrameForm.AddValidator(LDialogValidator);
      //APageControl.AddValidator(LDialogValidator);


      LDialogValidator.Initialise;
      TInputGridValidator(LDialogValidator).Identifier := LIdentifier;
      TInputGridValidator(LDialogValidator).ElementType := LElementType;
      LDialogValidator.PopulateDataViewer;
      LDialogValidator.LanguageHasChanged;
    end;

    if (LViewName = mdvnInputViewDataGraph) then
    begin
      LDialogValidator := TInputGraphValidator.Create(FPopupForm,FAppModules);
      FPopupFrameForm.AddValidator(LDialogValidator);
      //APageControl.AddValidator(LDialogValidator);

      LDialogValidator.Initialise;
      TInputGraphValidator(LDialogValidator).Identifier := LIdentifier;
      TInputGraphValidator(LDialogValidator).ElementType := LElementType;
      LDialogValidator.PopulateDataViewer;
      LDialogValidator.LanguageHasChanged;
    end;

    // Group dialogs
    if (LViewName = mdvnIFRFeaturesProperty) then
    begin
      LDialogValidator := TIFRFeaturesPropertyValidator.Create(nil,FAppModules);
      //APageControl.AddValidator(LDialogValidator);
      FPopupFrameForm.AddValidator(LDialogValidator);
      LDialogValidator.Initialise;
      LDialogValidator.PopulateDataViewer;
      LDialogValidator.LanguageHasChanged;
    end;

    if (LViewName = mdvnRunConfiguration) then
    begin
      //Run Configuration
      lRunConfigData := TYieldModelDataObject(FAppModules.Model.ModelData).RunConfigurationData;
      if (lRunConfigData.HasBeenPopulated) then
      begin
        LDialogValidator := TRunConfigurationValidator.Create(nil,FAppModules);
        //APageControl.AddValidator(LDialogValidator);
        FPopupFrameForm.AddValidator(LDialogValidator);
        LDialogValidator.Initialise;
        LDialogValidator.PopulateDataViewer;
        LDialogValidator.LanguageHasChanged;

        LDialogValidator := TConfigurationFilesValidator.Create(nil,FAppModules);
        //APageControl.AddValidator(LDialogValidator);
        FPopupFrameForm.AddValidator(LDialogValidator);
        LDialogValidator.Initialise;
        LDialogValidator.PopulateDataViewer;
        LDialogValidator.LanguageHasChanged;
      end
    end;

    if (LViewName = mdvnHydrologyAllocation) then
    begin
      //Output Configuration
      if (TYieldModelDataObject(FAppModules.Model.ModelData).RunConfigurationData.OutputDataHasBeenPopulated) then
      begin
        LDialogValidator := THydrologyAlocationValidator.Create(nil,FAppModules);
        //APageControl.AddValidator(LDialogValidator);
        FPopupFrameForm.AddValidator(LDialogValidator);
        LDialogValidator.Initialise;
        LDialogValidator.PopulateDataViewer;
        LDialogValidator.LanguageHasChanged;
      end;
    end
    else
    if (LViewName = mdvnOutputConfiguration) then
    begin
      //Output Configuration
      if (TYieldModelDataObject(FAppModules.Model.ModelData).RunConfigurationData.OutputDataHasBeenPopulated) then
      begin
        LDialogValidator := TOutputConfigurationValidator.Create(nil,FAppModules);
        FPopupFrameForm.AddValidator(LDialogValidator);//(LDialogValidator);
        LDialogValidator.Initialise;
        LDialogValidator.PopulateDataViewer;
        LDialogValidator.LanguageHasChanged;

        LDialogValidator := TReservoirAndChannelOutputValidator.Create(nil,FAppModules);
        FPopupFrameForm.AddValidator(LDialogValidator);
        LDialogValidator.Initialise;
        LDialogValidator.PopulateDataViewer;
        LDialogValidator.LanguageHasChanged;
      end;
    end
    else
    if (LViewName = mdvnReconciliationAnalysis) then
    begin
      //Output Configuration
      if (TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.WaterDemandConfiguration <> nil) then
      begin
        LDialogValidator := TReconciliationAnalysisValidator.Create(nil,FAppModules);
        FPopupFrameForm.AddValidator(LDialogValidator);
        LDialogValidator.Initialise;
        LDialogValidator.PopulateDataViewer;
        LDialogValidator.LanguageHasChanged;

        //Water use proportioning
        LDialogValidator := TWaterUseProportioningValidator.Create(nil,FAppModules);
        FPopupFrameForm.AddValidator(LDialogValidator);
        LDialogValidator.Initialise;
        LDialogValidator.PopulateDataViewer;
        LDialogValidator.LanguageHasChanged;

        //Water use scenario
        LDialogValidator := TWaterUseScenarioValidator.Create ( nil, FAppModules );
        FPopupFrameForm.AddValidator ( LDialogValidator );
        LDialogValidator.Initialise;
        LDialogValidator.PopulateDataViewer;
        LDialogValidator.LanguageHasChanged;
      end;
    end
    else
    if (LViewName = mdvnReservoirPenalty) then
    begin
      //Reservoir Penalty
      LDialogValidator := TReservoirPenaltyValidator.Create(nil,FAppModules);
      FPopupFrameForm.AddValidator(LDialogValidator);
      LDialogValidator.Initialise;
      TReservoirPenaltyValidator(LDialogValidator).PenaltyStructureNumber := -1;
      TReservoirPenaltyValidator(LDialogValidator).ViewMode := vmEditable;
      LDialogValidator.PopulateDataViewer;
      LDialogValidator.LanguageHasChanged;
    end
    else
    if (LViewName = mdvnMetaData) then
    begin
      //Reservoir Penalty
      LDialogValidator := TYieldMetaDataValidator.Create(nil,FAppModules);
      FPopupFrameForm.AddValidator(LDialogValidator);
      LDialogValidator.Initialise;
      LDialogValidator.PopulateDataViewer;
      LDialogValidator.LanguageHasChanged;
    end
    else
    if (LViewName = mdvnCatchmentProportions) then
    begin
      //Reservoir Catchment Proportions
      LDialogValidator := TReservoirCatchmentProportionsValidator.Create(nil,FAppModules);
      FPopupFrameForm.AddValidator(LDialogValidator);
      LDialogValidator.Initialise;
      TReservoirCatchmentProportionsValidator(LDialogValidator).ViewMode :=  vmEditable;
      LDialogValidator.PopulateDataViewer;
      LDialogValidator.LanguageHasChanged;
    end
    else
    if (LViewName = mdvnChannelArea) then
    begin
      //Channel Area
      LDialogValidator := TChannelAreaValidator.Create(nil,FAppModules);
      FPopupFrameForm.AddValidator(LDialogValidator);
      LDialogValidator.Initialise;
      LDialogValidator.PopulateDataViewer;
      LDialogValidator.LanguageHasChanged;
    end
    else
    if (LViewName = mdvnReservoirAreaGroup) then
    begin
      //Channel Area
      LDialogValidator := TReservoirAreaGroupValidator.Create(nil,FAppModules);
      FPopupFrameForm.AddValidator(LDialogValidator);
      LDialogValidator.Initialise;
      LDialogValidator.PopulateDataViewer;
      LDialogValidator.LanguageHasChanged;
    end
    else
    if (LViewName = mdvnCurtailments) then
    begin
      //Curtailment Structure
      LDialogValidator := TCurtailmentStructureValidator.Create(nil,FAppModules);
      FPopupFrameForm.AddValidator(LDialogValidator);
      LDialogValidator.Initialise;
      LDialogValidator.PopulateDataViewer;
      LDialogValidator.LanguageHasChanged;
    end
    else
    if (LViewName = mdvnRestrictions) then
    begin
      //Drought Restriction Structure
      LDroughtRestriction := TYieldModelDataObject(FAppModules.Model.ModelData)
                     .NetworkFeaturesData.CurtailmentAndDrought.DroughtRestrictionByID[LIdentifier];
      if (LDroughtRestriction <> nil) then
      begin
        LDialogValidator := TDroughtRestrictionStructureValidator.Create(nil,FAppModules);
        FPopupFrameForm.AddValidator(LDialogValidator);
        LDialogValidator.Initialise;
        TDroughtRestrictionStructureValidator(LDialogValidator).Identifier := LIdentifier;
        LDialogValidator.PopulateDataViewer;
        LDialogValidator.LanguageHasChanged;
      end;
    end
    else
    if (LViewName = mdvnRunTitle) then
    begin
      LDialogValidator := TYieldRunTitleValidator.Create(nil,FAppModules);
      FPopupFrameForm.AddValidator(LDialogValidator);
      LDialogValidator.Initialise;
      LDialogValidator.PopulateDataViewer;
      LDialogValidator.LanguageHasChanged;
    end
    else


    if (LViewName = mdvnReservoir) or (LViewName = mdvnPCDDam) or (LViewName = mdvnUndegroundDam) then
    begin
      lReservoir := TYieldModelDataObject(FAppModules.Model.ModelData).
                      NetworkElementData.ReservoirList.ReservoirOrNodeByIdentifier[LIdentifier];

      if(lReservoir <> nil) and (LViewName = mdvnPCDDam) and
        (lReservoir.ReservoirConfigurationData.NodeType in [ntNodeWithInflow,ntNodeWithoutInflow]) then
      begin
        //Node Properties
        LDialogValidator := TNodePropertiesValidator.Create(nil,FAppModules);
        FPopupFrameForm.AddValidator(LDialogValidator);
        //FPopupFrameForm.AddValidator(LDialogValidator);
        LDialogValidator.Initialise;
        TNodePropertiesValidator(LDialogValidator).Identifier := LIdentifier;
        LDialogValidator.PopulateDataViewer;
        LDialogValidator.LanguageHasChanged;

        LMine := TYieldModelDataObject(FAppModules.Model.ModelData).CastNetworkFeaturesData.
                 CastMineList.MineByPCDNumber[LIdentifier];

        LDialogValidator := TGeneralFlowChannelValidator.Create(nil,FAppModules);
        //FPopupFrameForm.AddValidator(LDialogValidator);
        FPopupFrameForm.AddValidator(LDialogValidator);
        LDialogValidator.Initialise;

        if (LMine <> nil) AND (LMine.PCDChannel <> nil) then
          TGeneralFlowChannelValidator(LDialogValidator).Identifier         := LMine.PCDChannel.ChannelNumber;
        TGeneralFlowChannelValidator(LDialogValidator).Heading              := 'NetworkFeatures.MineToPCDChannelProperties';
        TGeneralFlowChannelValidator(LDialogValidator).UpStreamEnabled      := False;
        TGeneralFlowChannelValidator(LDialogValidator).DownStreamEnabled    := False;
        TGeneralFlowChannelValidator(LDialogValidator).SelectChannelEnabled := False;
        TGeneralFlowChannelValidator(LDialogValidator).Mode                 := gfcvmMineToPCD;
        LDialogValidator.PopulateDataViewer;
        LDialogValidator.LanguageHasChanged;

        //Reservoir Catchment Proportions
        if(lReservoir.ReservoirConfigurationData.NodeType = ntNodeWithInflow) then
        begin
          LDialogValidator := TReservoirCatchmentProportionsValidator.Create(nil,FAppModules);
          //FPopupFrameForm.AddValidator(LDialogValidator);
          FPopupFrameForm.AddValidator(LDialogValidator);
          LDialogValidator.Initialise;
          TReservoirCatchmentProportionsValidator(LDialogValidator).Identifier := LIdentifier;
          TReservoirCatchmentProportionsValidator(LDialogValidator).ViewMode :=  vmEditableSelect;
          LDialogValidator.PopulateDataViewer;
          LDialogValidator.LanguageHasChanged;
        end;
      end
      else
      begin

        //Reservoir Properties
        LDialogValidator := TReservoirPropertiesValidator.Create(nil,FAppModules);
        //FPopupFrameForm.AddValidator(LDialogValidator);
        FPopupFrameForm.AddValidator(LDialogValidator);
        LDialogValidator.Initialise;
        TReservoirPropertiesValidator(LDialogValidator).Identifier := LIdentifier;
        LDialogValidator.PopulateDataViewer;
        LDialogValidator.LanguageHasChanged;

        //Reservoir Physical Characteristics
        LDialogValidator := TReservoirPhysicalCharacteristicsValidator.Create(nil,FAppModules);
        //FPopupFrameForm.AddValidator(LDialogValidator);
        FPopupFrameForm.AddValidator(LDialogValidator);
        LDialogValidator.Initialise;
        TReservoirPhysicalCharacteristicsValidator(LDialogValidator).Identifier := LIdentifier;
        LDialogValidator.PopulateDataViewer;
        LDialogValidator.LanguageHasChanged;


        if (LViewName = mdvnPCDDam) then
        begin
          LMine := TYieldModelDataObject(FAppModules.Model.ModelData).CastNetworkFeaturesData.
                   CastMineList.MineByPCDNumber[LIdentifier];

          LDialogValidator := TGeneralFlowChannelValidator.Create(nil,FAppModules);
          //FPopupFrameForm.AddValidator(LDialogValidator);
          FPopupFrameForm.AddValidator(LDialogValidator);
          LDialogValidator.Initialise;

          if (LMine <> nil) AND (LMine.PCDChannel <> nil) then
            TGeneralFlowChannelValidator(LDialogValidator).Identifier         := LMine.PCDChannel.ChannelNumber;
          TGeneralFlowChannelValidator(LDialogValidator).Heading              := 'NetworkFeatures.MineToPCDChannelProperties';
          TGeneralFlowChannelValidator(LDialogValidator).UpStreamEnabled      := False;
          TGeneralFlowChannelValidator(LDialogValidator).DownStreamEnabled    := False;
          TGeneralFlowChannelValidator(LDialogValidator).SelectChannelEnabled := False;
          TGeneralFlowChannelValidator(LDialogValidator).Mode                 := gfcvmMineToPCD;
          LDialogValidator.PopulateDataViewer;
          LDialogValidator.LanguageHasChanged;
        end;

        //Reservoir Evaporation
        if (LViewName <> mdvnUndegroundDam) then
        begin
          LDialogValidator := TReservoirEvaporationValidator.Create(nil,FAppModules);
          //FPopupFrameForm.AddValidator(LDialogValidator);
          FPopupFrameForm.AddValidator(LDialogValidator);
          LDialogValidator.Initialise;
          TReservoirEvaporationValidator(LDialogValidator).Identifier := LIdentifier;
          LDialogValidator.PopulateDataViewer;
          LDialogValidator.LanguageHasChanged;
        end;

        if (LViewName = mdvnUndegroundDam) then
        begin

          LChannel        := TYieldModelDataObject(FAppModules.Model.ModelData).CastNetworkFeaturesData.
                             CastMineList.ChannelToUnderGroundDamByDamNumber[LIdentifier];

          LDialogValidator := TGeneralFlowChannelValidator.Create(nil,FAppModules);
          //APageControl.AddValidator(LDialogValidator);
          FPopupFrameForm.AddValidator(LDialogValidator);
          LDialogValidator.Initialise;
          if (LChannel <> nil) then
            TGeneralFlowChannelValidator(LDialogValidator).Identifier         := LChannel.ChannelNumber;
          TGeneralFlowChannelValidator(LDialogValidator).Heading              := 'NetworkFeatures.MineToUndergroundChannelProperties';
          TGeneralFlowChannelValidator(LDialogValidator).UpStreamEnabled      := False;
          TGeneralFlowChannelValidator(LDialogValidator).DownStreamEnabled    := False;
          TGeneralFlowChannelValidator(LDialogValidator).SelectChannelEnabled := False;
          TGeneralFlowChannelValidator(LDialogValidator).Mode                 := gfcvmMineToUndeground;
          LDialogValidator.PopulateDataViewer;
          LDialogValidator.LanguageHasChanged;
        end;



        //Reservoir Catchment Proportions
        LDialogValidator := TReservoirCatchmentProportionsValidator.Create(nil,FAppModules);
        //APageControl.AddValidator(LDialogValidator);
        FPopupFrameForm.AddValidator(LDialogValidator);
        LDialogValidator.Initialise;
        TReservoirCatchmentProportionsValidator(LDialogValidator).Identifier := LIdentifier;
        TReservoirCatchmentProportionsValidator(LDialogValidator).ViewMode :=  vmEditableSelect;
        LDialogValidator.PopulateDataViewer;
        LDialogValidator.LanguageHasChanged;

        //Reservoir Penalty
        LDialogValidator := TReservoirPenaltyValidator.Create(nil,FAppModules);
        //APageControl.AddValidator(LDialogValidator);
        FPopupFrameForm.AddValidator(LDialogValidator);
        LDialogValidator.Initialise;
        TReservoirPenaltyValidator(LDialogValidator).ReservoirNumber := LIdentifier;
        LDialogValidator.PopulateDataViewer;
        LDialogValidator.LanguageHasChanged;
        TReservoirPenaltyValidator(LDialogValidator).ViewMode := vmEditableSelect;

        //Reservoir Zone Elevations
        LDialogValidator := TReservoirZoneElevationsValidator.Create(nil,FAppModules);
        //APageControl.AddValidator(LDialogValidator);
        FPopupFrameForm.AddValidator(LDialogValidator);
        LDialogValidator.Initialise;
        TReservoirZoneElevationsValidator(LDialogValidator).Identifier := LIdentifier;
        LDialogValidator.PopulateDataViewer;
        LDialogValidator.LanguageHasChanged;

        //Reservoir Time Control
        if Assigned(lReservoir) and (lReservoir.TimeControl <> nil) then
        begin
          LDialogValidator := TReservoirTimeControlValidator.Create(nil,FAppModules);
          //APageControl.AddValidator(LDialogValidator);
          FPopupFrameForm.AddValidator(LDialogValidator);
          LDialogValidator.Initialise;
          TReservoirTimeControlValidator(LDialogValidator).Identifier := LIdentifier;
          LDialogValidator.PopulateDataViewer;
          LDialogValidator.LanguageHasChanged;
        end;

        LChannel := TYieldModelDataObject(FAppModules.Model.ModelData).
                    NetworkElementData.ChannelList.ChannelByChannelNumber[lReservoir.ReservoirConfigurationData.NaturalInflowChannel];
        if (LChannel <> nil) then
        begin
          LDialogValidator := TGeneralFlowChannelValidator.Create(nil,FAppModules);
          //APageControl.AddValidator(LDialogValidator);
          FPopupFrameForm.AddValidator(LDialogValidator);
          LDialogValidator.Initialise;
          TGeneralFlowChannelValidator(LDialogValidator).Identifier := LChannel.ChannelNumber;
          TGeneralFlowChannelValidator(LDialogValidator).Heading       := 'NetworkFeatures.NaturalInflowChannel';
          TGeneralFlowChannelValidator(LDialogValidator).UpStreamEnabled := True;
          TGeneralFlowChannelValidator(LDialogValidator).DownStreamEnabled := FALSE;
          TGeneralFlowChannelValidator(LDialogValidator).SelectChannelEnabled := FALSE;
          LDialogValidator.PopulateDataViewer;
          LDialogValidator.LanguageHasChanged;
        end;

      end;
    end;


    if (LViewName = mdvnPowerPlant) then
    begin
      lPowerPlant := TYieldModelDataObject(FAppModules.Model.ModelData).
                       NetworkFeaturesData.PowerPlantList.PowerPlantByID[LIdentifier];
      // Power Plant
      LDialogValidator := TPowerPlantValidator.Create(nil,FAppModules);
      FPopupFrameForm.AddValidator(LDialogValidator);
      LDialogValidator.Initialise;
      TPowerPlantValidator(LDialogValidator).Identifier := LIdentifier;
      TPowerPlantValidator(LDialogValidator).FeatureID := LIdentifier;
      LDialogValidator.PopulateDataViewer;
      LDialogValidator.LanguageHasChanged;

      // Power Channel
      LDialogValidator := TGeneralFlowChannelValidator.Create(nil,FAppModules);
      FPopupFrameForm.AddValidator(LDialogValidator);
      LDialogValidator.Initialise;
      TGeneralFlowChannelValidator(LDialogValidator).Identifier := lPowerPlant.PowerChannel.ChannelNumber;
      TGeneralFlowChannelValidator(LDialogValidator).Heading       := 'NetworkFeatures.PowerChannelProperties';
      TGeneralFlowChannelValidator(LDialogValidator).UpStreamEnabled := FALSE;
      TGeneralFlowChannelValidator(LDialogValidator).DownStreamEnabled := FALSE;
      TGeneralFlowChannelValidator(LDialogValidator).SelectChannelEnabled := FALSE;
      LDialogValidator.PopulateDataViewer;
      LDialogValidator.LanguageHasChanged;

      // Spill Channel
      LDialogValidator := TGeneralFlowChannelValidator.Create(nil,FAppModules);
      FPopupFrameForm.AddValidator(LDialogValidator);
      LDialogValidator.Initialise;
      TGeneralFlowChannelValidator(LDialogValidator).Identifier := lPowerPlant.SpillChannel.ChannelNumber;
      TGeneralFlowChannelValidator(LDialogValidator).Heading       := 'NetworkFeatures.SpillChannelProperties';
      TGeneralFlowChannelValidator(LDialogValidator).UpStreamEnabled := FALSE;
      TGeneralFlowChannelValidator(LDialogValidator).DownStreamEnabled := FALSE;
      TGeneralFlowChannelValidator(LDialogValidator).SelectChannelEnabled := FALSE;
      LDialogValidator.PopulateDataViewer;
      LDialogValidator.LanguageHasChanged;

      // Power Plant Factors
      LDialogValidator := TPowerPlantFactorsValidator.Create(nil,FAppModules);
      FPopupFrameForm.AddValidator(LDialogValidator);
      LDialogValidator.Initialise;
      TPowerPlantFactorsValidator(LDialogValidator).FeatureID := LIdentifier;
      LDialogValidator.PopulateDataViewer;
      LDialogValidator.LanguageHasChanged;

      // Power Plant Tailwater
      LDialogValidator := TPowerPlantTailwaterValidator.Create(nil,FAppModules);
      FPopupFrameForm.AddValidator(LDialogValidator);
      LDialogValidator.Initialise;
      TPowerPlantTailwaterValidator(LDialogValidator).FeatureID := LIdentifier;
      LDialogValidator.PopulateDataViewer;
      LDialogValidator.LanguageHasChanged;

      // Power Plant Demands
      LDialogValidator := TPowerPlantDemandsValidator.Create(nil,FAppModules);
      FPopupFrameForm.AddValidator(LDialogValidator);
      LDialogValidator.Initialise;
      TPowerPlantDemandsValidator(LDialogValidator).FeatureID := LIdentifier;
      LDialogValidator.PopulateDataViewer;
      LDialogValidator.LanguageHasChanged;
    end;

    if (LViewName = mdvnIrrigationArea) then
    begin
      lIrrigationArea := TYieldModelDataObject(FAppModules.Model.ModelData).
                           NetworkFeaturesData.IrrigationAreaList.IrrigationAreaByNodeNumber[LIdentifier];
      // Irrigation area
      LDialogValidator := TIrrigationAreaValidator.Create(nil,FAppModules);
      FPopupFrameForm.AddValidator(LDialogValidator);
      LDialogValidator.Initialise;
      TIrrigationAreaValidator(LDialogValidator).Identifier := LIdentifier;
      LDialogValidator.PopulateDataViewer;
      LDialogValidator.LanguageHasChanged;

      // Diversion Channel
      LDialogValidator := TGeneralFlowChannelValidator.Create(nil,FAppModules);
      FPopupFrameForm.AddValidator(LDialogValidator);
      LDialogValidator.Initialise;
      TGeneralFlowChannelValidator(LDialogValidator).Identifier     := lIrrigationArea.DiversionChannel.ChannelNumber;
      TGeneralFlowChannelValidator(LDialogValidator).Heading           := 'NetworkFeatures.DiversionChannelProperties';
      TGeneralFlowChannelValidator(LDialogValidator).UpStreamEnabled   := FALSE;
      TGeneralFlowChannelValidator(LDialogValidator).DownStreamEnabled := FALSE;
      TGeneralFlowChannelValidator(LDialogValidator).SelectChannelEnabled := FALSE;
      LDialogValidator.PopulateDataViewer;
      LDialogValidator.LanguageHasChanged;

      // Consumptive Channel
      LDialogValidator := TGeneralFlowChannelValidator.Create(nil,FAppModules);
      FPopupFrameForm.AddValidator(LDialogValidator);
      LDialogValidator.Initialise;
      TGeneralFlowChannelValidator(LDialogValidator).Identifier        := lIrrigationArea.ConsumptiveChannel.ChannelNumber;
      TGeneralFlowChannelValidator(LDialogValidator).Heading           := 'NetworkFeatures.ConsumptiveChannelProperties';
      TGeneralFlowChannelValidator(LDialogValidator).UpStreamEnabled   := FALSE;
      TGeneralFlowChannelValidator(LDialogValidator).DownStreamEnabled := FALSE;
      TGeneralFlowChannelValidator(LDialogValidator).SelectChannelEnabled := FALSE;
      LDialogValidator.PopulateDataViewer;
      LDialogValidator.LanguageHasChanged;

      // Return Flow Channel
      LDialogValidator := TGeneralFlowChannelValidator.Create(nil,FAppModules);
      FPopupFrameForm.AddValidator(LDialogValidator);
      LDialogValidator.Initialise;
      TGeneralFlowChannelValidator(LDialogValidator).Identifier        := lIrrigationArea.ReturnFlowChannel.ChannelNumber;
      TGeneralFlowChannelValidator(LDialogValidator).Heading           := 'NetworkFeatures.ReturnFlowChannelProperties';
      TGeneralFlowChannelValidator(LDialogValidator).UpStreamEnabled   := FALSE;
      TGeneralFlowChannelValidator(LDialogValidator).DownStreamEnabled := FALSE;
      TGeneralFlowChannelValidator(LDialogValidator).SelectChannelEnabled := FALSE;
      LDialogValidator.PopulateDataViewer;
      LDialogValidator.LanguageHasChanged;
    end;

  (*if (LViewName = mdvnIrrigationBlock) then
    begin
      //Irrigation Block
      LDialogValidator := TIrrBlockPropertiesValidator.Create(nil,FAppModules);
      APageControl.AddValidator(LDialogValidator);
      LDialogValidator.Initialise;
      TIrrBlockPropertiesValidator(LDialogValidator).Identifier := LIdentifier;
      LDialogValidator.PopulateDataViewer;
      LDialogValidator.LanguageHasChanged;

      // Diversion Channel
      LDialogValidator := TGeneralFlowChannelValidator.Create(nil,FAppModules);
      APageControl.AddValidator(LDialogValidator);
      LDialogValidator.Initialise;
      lIrrigationBlock  := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.IrrigationBlockList.IrrigationBlockByBlockNodeNumber[LIdentifier];
      if(lIrrigationBlock <> nil) and (lIrrigationBlock.DiversionChannel <> nil) then
        TGeneralFlowChannelValidator(LDialogValidator).Identifier         := lIrrigationBlock.DiversionChannel.ChannelNumber;
      TGeneralFlowChannelValidator(LDialogValidator).Heading              := 'NetworkFeatures.DiversionChannelProperties';
      TGeneralFlowChannelValidator(LDialogValidator).UpStreamEnabled      := FALSE;
      TGeneralFlowChannelValidator(LDialogValidator).DownStreamEnabled    := FALSE;
      TGeneralFlowChannelValidator(LDialogValidator).SelectChannelEnabled := FALSE;
      TGeneralFlowChannelValidator(LDialogValidator).Mode                 :=  gfcvmIrrBlockUpstream;
      LDialogValidator.PopulateDataViewer;
      LDialogValidator.LanguageHasChanged;

      // Return Flow Channel
            LDialogValidator := TGeneralFlowChannelValidator.Create(nil,FAppModules);
      APageControl.AddValidator(LDialogValidator);
      LDialogValidator.Initialise;
      lIrrigationBlock  := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.IrrigationBlockList.IrrigationBlockByBlockNodeNumber[LIdentifier];
      if(lIrrigationBlock <> nil) and (lIrrigationBlock.ReturnFlowChannel <> nil) then
        TGeneralFlowChannelValidator(LDialogValidator).Identifier         := lIrrigationBlock.ReturnFlowChannel.ChannelNumber;
      TGeneralFlowChannelValidator(LDialogValidator).Heading              := 'NetworkFeatures.ReturnFlowChannelProperties';
      TGeneralFlowChannelValidator(LDialogValidator).UpStreamEnabled      := FALSE;
      TGeneralFlowChannelValidator(LDialogValidator).DownStreamEnabled    := FALSE;
      TGeneralFlowChannelValidator(LDialogValidator).SelectChannelEnabled := FALSE;
      TGeneralFlowChannelValidator(LDialogValidator).Mode                 :=  gfcvmIrrBlockDownstream;
      LDialogValidator.PopulateDataViewer;
      LDialogValidator.LanguageHasChanged;

      //Irrigated Area
      LDialogValidator := TIrrBlockIrrigatedAreaValidator.Create(nil,FAppModules);
      APageControl.AddValidator(LDialogValidator);
      LDialogValidator.Initialise;
      TIrrBlockIrrigatedAreaValidator(LDialogValidator).Identifier := LIdentifier;
      LDialogValidator.PopulateDataViewer;
      LDialogValidator.LanguageHasChanged;

      //Evaporation
      LDialogValidator := TIrrBlockClimateValidator.Create(nil,FAppModules);
      APageControl.AddValidator(LDialogValidator);
      LDialogValidator.Initialise;
      TIrrBlockClimateValidator(LDialogValidator).Identifier := LIdentifier;
      LDialogValidator.PopulateDataViewer;
      LDialogValidator.LanguageHasChanged;


      //Crop Water
      LDialogValidator := TIrrBlockCropRequirementValidator.Create(nil,FAppModules);
      APageControl.AddValidator(LDialogValidator);
      LDialogValidator.Initialise;
      TIrrBlockCropRequirementValidator(LDialogValidator).Identifier := LIdentifier;
      LDialogValidator.PopulateDataViewer;
      LDialogValidator.LanguageHasChanged;

      LDialogValidator := TIrrBlockGrowthValidator.Create(nil,FAppModules);
      APageControl.AddValidator(LDialogValidator);
      LDialogValidator.Initialise;
      TIrrBlockGrowthValidator(LDialogValidator).Identifier := LIdentifier;
      LDialogValidator.PopulateDataViewer;
      LDialogValidator.LanguageHasChanged;


      if(lIrrigationBlock <> nil) and (lIrrigationBlock.IrrigationBlockType = 4) then
      begin
        LDialogValidator := TIrrBlockGrowthType4Validator.Create(nil,FAppModules);
        APageControl.AddValidator(LDialogValidator);
        LDialogValidator.Initialise;
        TIrrBlockGrowthType4Validator(LDialogValidator).Identifier := LIdentifier;
        LDialogValidator.PopulateDataViewer;
        LDialogValidator.LanguageHasChanged;
      end;
    end;
    *)


    if (LViewName = mdvnIrrigationBlock) then
    begin
      //Irrigation Block
      LDialogValidator := TIrrigationBlockValidator.Create(nil,FAppModules);
      //APageControl.AddValidator(LDialogValidator);
      FPopupFrameForm.AddValidator(LDialogValidator);
      LDialogValidator.Initialise;
      TIrrigationBlockValidator(LDialogValidator).Identifier := LIdentifier;
      LDialogValidator.PopulateDataViewer;
      LDialogValidator.LanguageHasChanged;

      // Diversion Channel
      LDialogValidator := TGeneralFlowChannelValidator.Create(nil,FAppModules);
      FPopupFrameForm.AddValidator(LDialogValidator);
      LDialogValidator.Initialise;
      lIrrigationBlock  := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.IrrigationBlockList.IrrigationBlockByBlockNodeNumber[LIdentifier];
      if(lIrrigationBlock <> nil) and (lIrrigationBlock.DiversionChannel <> nil) then
        TGeneralFlowChannelValidator(LDialogValidator).Identifier      := lIrrigationBlock.DiversionChannel.ChannelNumber;
      TGeneralFlowChannelValidator(LDialogValidator).Heading           := 'NetworkFeatures.DiversionChannelProperties';
      TGeneralFlowChannelValidator(LDialogValidator).UpStreamEnabled   := FALSE;
      TGeneralFlowChannelValidator(LDialogValidator).DownStreamEnabled := FALSE;
      TGeneralFlowChannelValidator(LDialogValidator).SelectChannelEnabled := FALSE;
      TGeneralFlowChannelValidator(LDialogValidator).Mode               :=  gfcvmIrrBlockUpstream;
      LDialogValidator.PopulateDataViewer;
      LDialogValidator.LanguageHasChanged;

      // Return Flow Channel
      LDialogValidator := TIrrigationBlockReturnFlowChannelValidator.Create(nil,FAppModules);
      FPopupFrameForm.AddValidator(LDialogValidator);
      LDialogValidator.Initialise;
      TIrrigationBlockReturnFlowChannelValidator(LDialogValidator).Identifier := LIdentifier;
      LDialogValidator.PopulateDataViewer;
      LDialogValidator.LanguageHasChanged;

      //Evaporation
      LDialogValidator := TEvaporationValidator.Create(nil,FAppModules);
      FPopupFrameForm.AddValidator(LDialogValidator);
      LDialogValidator.Initialise;
      TEvaporationValidator(LDialogValidator).Identifier := LIdentifier;
      LDialogValidator.PopulateDataViewer;
      LDialogValidator.LanguageHasChanged;

      //Crop Water
      LDialogValidator := TCropWaterValidator.Create(nil,FAppModules);
      FPopupFrameForm.AddValidator(LDialogValidator);
      LDialogValidator.Initialise;
      TCropWaterValidator(LDialogValidator).Identifier := LIdentifier;
      LDialogValidator.PopulateDataViewer;
      LDialogValidator.LanguageHasChanged;

      //Soil Properties
      LDialogValidator := TSoilPropertiesValidator.Create(nil,FAppModules);
      FPopupFrameForm.AddValidator(LDialogValidator);
      LDialogValidator.Initialise;
      TSoilPropertiesValidator(LDialogValidator).Identifier := LIdentifier;
      LDialogValidator.PopulateDataViewer;
      LDialogValidator.LanguageHasChanged;

      //new.......................
      (*
        //Irrigated Area
      LDialogValidator := TIrrBlockIrrigatedAreaValidator.Create(nil,FAppModules);
      FPopupFrameForm.AddValidator(LDialogValidator);
      LDialogValidator.Initialise;
      TIrrBlockIrrigatedAreaValidator(LDialogValidator).Identifier := LIdentifier;
      LDialogValidator.PopulateDataViewer;
      LDialogValidator.LanguageHasChanged;

      //Evaporation
     (* LDialogValidator := TIrrBlockClimateValidator.Create(nil,FAppModules);
      FPopupFrameForm.AddValidator(LDialogValidator);
      LDialogValidator.Initialise;
      TIrrBlockClimateValidator(LDialogValidator).Identifier := LIdentifier;
      LDialogValidator.PopulateDataViewer;
      LDialogValidator.LanguageHasChanged;
       *)

      //Crop Water
     { LDialogValidator := TIrrBlockCropRequirementValidator.Create(nil,FAppModules);
      APageControl.AddValidator(LDialogValidator);
      LDialogValidator.Initialise;
      TIrrBlockCropRequirementValidator(LDialogValidator).Identifier := LIdentifier;
      LDialogValidator.PopulateDataViewer;
      LDialogValidator.LanguageHasChanged;
                }
                (*
      LDialogValidator := TIrrBlockGrowthValidator.Create(nil,FAppModules);
      //APageControl.AddValidator(LDialogValidator);
      FPopupFrameForm.AddValidator(LDialogValidator);
      LDialogValidator.Initialise;
      TIrrBlockGrowthValidator(LDialogValidator).Identifier := LIdentifier;
      LDialogValidator.PopulateDataViewer;
      LDialogValidator.LanguageHasChanged;

      if(lIrrigationBlock <> nil) and (lIrrigationBlock.IrrigationBlockType = 4) then
      begin
        LDialogValidator := TIrrBlockGrowthType4Validator.Create(nil,FAppModules);
        FPopupFrameForm.AddValidator(LDialogValidator);
        LDialogValidator.Initialise;
        TIrrBlockGrowthType4Validator(LDialogValidator).Identifier := LIdentifier;
        LDialogValidator.PopulateDataViewer;
        LDialogValidator.LanguageHasChanged;
      end;
                  *)
    end;

    if (LViewName = mdvnNodesWithInflow) then
    begin
      //Node Properties
      LDialogValidator := TNodePropertiesValidator.Create(nil,FAppModules);
      FPopupFrameForm.AddValidator(LDialogValidator);
      LDialogValidator.Initialise;
      TNodePropertiesValidator(LDialogValidator).Identifier := LIdentifier;
      LDialogValidator.PopulateDataViewer;
      LDialogValidator.LanguageHasChanged;

      //Reservoir Catchment Proportions
      LDialogValidator := TReservoirCatchmentProportionsValidator.Create(nil,FAppModules);
      FPopupFrameForm.AddValidator(LDialogValidator);
      LDialogValidator.Initialise;
      TReservoirCatchmentProportionsValidator(LDialogValidator).Identifier := LIdentifier;
      TReservoirCatchmentProportionsValidator(LDialogValidator).ViewMode :=  vmEditableSelect;
      LDialogValidator.PopulateDataViewer;
      LDialogValidator.LanguageHasChanged;

      if (FAppModules.StudyArea.ModelCode = CPlanning) then
      begin
        lReservoir :=   TYieldModelDataObject(FAppModules.Model.ModelData).
                        NetworkElementData.ReservoirList.ReservoirOrNodeByIdentifier[LIdentifier];
        if lReservoir <> nil then
          LChannel := TYieldModelDataObject(FAppModules.Model.ModelData).
                    NetworkElementData.ChannelList.ChannelByChannelNumber
                             [lReservoir.ReservoirConfigurationData.NaturalInflowChannel];

        if (LChannel <> nil) then
        begin
          LDialogValidator := TGeneralFlowChannelValidator.Create(nil,FAppModules);
          FPopupFrameForm.AddValidator(LDialogValidator);
          LDialogValidator.Initialise;
          TGeneralFlowChannelValidator(LDialogValidator).Identifier := LChannel.ChannelNumber;
          TGeneralFlowChannelValidator(LDialogValidator).Heading       := 'NetworkFeatures.NaturalInflowChannel';
          TGeneralFlowChannelValidator(LDialogValidator).UpStreamEnabled := FALSE;
          TGeneralFlowChannelValidator(LDialogValidator).DownStreamEnabled := FALSE;
          TGeneralFlowChannelValidator(LDialogValidator).SelectChannelEnabled := FALSE;
          LDialogValidator.PopulateDataViewer;
          LDialogValidator.LanguageHasChanged;
        end;

      end;
    end;

    if (LViewName = mdvnNodesWithoutInFlow) then
    begin
      //Node Properties
      LDialogValidator := TNodePropertiesValidator.Create(nil,FAppModules);
      FPopupFrameForm.AddValidator(LDialogValidator);
      LDialogValidator.Initialise;
      TNodePropertiesValidator(LDialogValidator).Identifier := LIdentifier;
      LDialogValidator.PopulateDataViewer;
      LDialogValidator.LanguageHasChanged;
    end;

    if ((LViewName = mdvnChannel) OR (LViewName = mdvnMasterControlConfiguration)) then
    begin
      LChannel := TYieldModelDataObject(FAppModules.Model.ModelData).
                    NetworkElementData.ChannelList.ChannelByChannelNumber[LIdentifier];
      if Assigned(LChannel) then
      begin
        LChannelType := LChannel.ChannelType;
        if (LChannelType in [1,13]) then Exit;

        lMasterControlFeature := LChannel.MasterControlFeature;
        lMinimumConstraint    := lChannel.MinimumFlowConstraint;
        lLossFeature          := LChannel.LossFeature;
        lMinMaxConstraint     := LChannel.MinMaxFlowConstraint;
        lPumpingFeature       := LChannel.PumpingFeature;
        lDemandFeature        := LChannel.SpecifiedDemandFeature;
        lDivFeature           := lChannel.DiversionFeature;
        lPhysConstraint       := lChannel.PhysicalFlowConstraint;
        lIFRFeature           := lChannel.IFRFeature;
        lSpecInflow           := lChannel.SpecifiedInflowFeature;
        LWaterDemandFeature   := lChannel.WaterDemandFeature;


        LDialogValidator := TGeneralFlowChannelValidator.Create(nil,FAppModules);
        FPopupFrameForm.AddValidator(LDialogValidator);
        LDialogValidator.Initialise;
        TGeneralFlowChannelValidator(LDialogValidator).Identifier := LIdentifier;
        if (LChannelType in [12]) then
          TGeneralFlowChannelValidator(LDialogValidator).SelectChannelEnabled := FALSE;

        if (lChannel.ChannelType = ctIrrigationBlockInflowChannel) then
        begin
          TGeneralFlowChannelValidator(LDialogValidator).Heading           := 'NetworkFeatures.DiversionChannelProperties';
          TGeneralFlowChannelValidator(LDialogValidator).UpStreamEnabled   := FALSE;
          TGeneralFlowChannelValidator(LDialogValidator).DownStreamEnabled := FALSE;
          TGeneralFlowChannelValidator(LDialogValidator).SelectChannelEnabled := FALSE;
          TGeneralFlowChannelValidator(LDialogValidator).Mode := gfcvmIrrBlockUpstream;
        end;

        if (lChannel.ChannelType = ctIrrigationBlockReturnFlowChannel) then
          TGeneralFlowChannelValidator(LDialogValidator).Mode := gfcvmIrrBlockDownstream;

        if (lChannel.ChannelType = ctWetlandInflowChannel) then
        begin
          TGeneralFlowChannelValidator(LDialogValidator).Heading              := 'NetworkFeatures.InflowChannelProperties';
          TGeneralFlowChannelValidator(LDialogValidator).UpStreamEnabled      := FALSE;
          TGeneralFlowChannelValidator(LDialogValidator).DownStreamEnabled    := FALSE;
          TGeneralFlowChannelValidator(LDialogValidator).SelectChannelEnabled := FALSE;
          TGeneralFlowChannelValidator(LDialogValidator).Mode                 := gfcvmWetlandUpstream;
        end;

        if (lChannel.ChannelType = ctWetlandOutflowChannel) then
        begin
          TGeneralFlowChannelValidator(LDialogValidator).Heading              := 'NetworkFeatures.OutflowChannelProperties';
          TGeneralFlowChannelValidator(LDialogValidator).UpStreamEnabled      := FALSE;
          TGeneralFlowChannelValidator(LDialogValidator).DownStreamEnabled    := FALSE;
          TGeneralFlowChannelValidator(LDialogValidator).SelectChannelEnabled := FALSE;
          TGeneralFlowChannelValidator(LDialogValidator).Mode                 := gfcvmWetlandDownstream;
        end;

        if (lChannel.ChannelType = ctDemandCentreReturnFlowChannel) then
        begin
          TGeneralFlowChannelValidator(LDialogValidator).Heading              := 'NetworkFeatures.ReturnFlowChannelProperties';
          TGeneralFlowChannelValidator(LDialogValidator).UpStreamEnabled      := False;
          TGeneralFlowChannelValidator(LDialogValidator).DownStreamEnabled    := True;
          TGeneralFlowChannelValidator(LDialogValidator).SelectChannelEnabled := False;
          TGeneralFlowChannelValidator(LDialogValidator).Mode                 := gfcvmDemandCentreConsumptiveUpstream;
        end;

        if (lChannel.ChannelType = ctReclaimationPlantLossChannel) then
        begin
          TGeneralFlowChannelValidator(LDialogValidator).Heading              := 'NetworkFeatures.DemandCentreReclaimationChannelProperties';
          TGeneralFlowChannelValidator(LDialogValidator).UpStreamEnabled      := False;
          TGeneralFlowChannelValidator(LDialogValidator).DownStreamEnabled    := True;
          TGeneralFlowChannelValidator(LDialogValidator).SelectChannelEnabled := False;
          TGeneralFlowChannelValidator(LDialogValidator).Mode                 := gfcvmDemandCentreReclaimationUpstream;
        end;

        if (lChannel.ChannelType = ctMineToRiverDChannel) then
        begin
          TGeneralFlowChannelValidator(LDialogValidator).Heading              := 'NetworkFeatures.MineToRiverChannelProperties';
          TGeneralFlowChannelValidator(LDialogValidator).UpStreamEnabled      := False;
          TGeneralFlowChannelValidator(LDialogValidator).DownStreamEnabled    := True;
          TGeneralFlowChannelValidator(LDialogValidator).SelectChannelEnabled := False;
          TGeneralFlowChannelValidator(LDialogValidator).Mode                 := gfcvmMineToRiver;
        end;

        if (lChannel.ChannelType = ctMineToPCDChannel) then
        begin
          TGeneralFlowChannelValidator(LDialogValidator).Heading              := 'NetworkFeatures.MineToPCDChannelProperties';
          TGeneralFlowChannelValidator(LDialogValidator).UpStreamEnabled      := False;
          TGeneralFlowChannelValidator(LDialogValidator).DownStreamEnabled    := False;
          TGeneralFlowChannelValidator(LDialogValidator).SelectChannelEnabled := False;
          TGeneralFlowChannelValidator(LDialogValidator).Mode                 := gfcvmMineToPCD;
        end;

        if (lChannel.ChannelType = ctMineToUndergroundChannel) then
        begin
          TGeneralFlowChannelValidator(LDialogValidator).Heading              := 'NetworkFeatures.MineToUndergroundChannelProperties';
          TGeneralFlowChannelValidator(LDialogValidator).UpStreamEnabled      := False;
          TGeneralFlowChannelValidator(LDialogValidator).DownStreamEnabled    := False;
          TGeneralFlowChannelValidator(LDialogValidator).SelectChannelEnabled := False;
          TGeneralFlowChannelValidator(LDialogValidator).Mode                 := gfcvmMineToUndeground;
        end;

        if (lChannel.ChannelType = ctAquiferInflowChannel) then
        begin
          TGeneralFlowChannelValidator(LDialogValidator).Heading              := 'NetworkFeatures.GroundWaterAquiferInflowChannel';
          TGeneralFlowChannelValidator(LDialogValidator).UpStreamEnabled      := False;
          TGeneralFlowChannelValidator(LDialogValidator).DownStreamEnabled    := False;
          TGeneralFlowChannelValidator(LDialogValidator).SelectChannelEnabled := False;
          TGeneralFlowChannelValidator(LDialogValidator).Mode                 := gfcvmGroundWaterAquiferInflowChannel;
        end;

        if (lChannel.ChannelType = ctInflowFromUpstreamAquiferChannel) then
        begin
          TGeneralFlowChannelValidator(LDialogValidator).Heading              := 'NetworkFeatures.InflowFromUpstreamAquifer';
          TGeneralFlowChannelValidator(LDialogValidator).UpStreamEnabled      := True;
          TGeneralFlowChannelValidator(LDialogValidator).DownStreamEnabled    := False;
          TGeneralFlowChannelValidator(LDialogValidator).SelectChannelEnabled := False;
          TGeneralFlowChannelValidator(LDialogValidator).Mode                 := gfcvmInflowFromUpstreamAquiferChannel;
        end;

        if (lChannel.ChannelType = ctOutFlowToDownstreamAquiferChannel) then
        begin
          TGeneralFlowChannelValidator(LDialogValidator).Heading              := 'NetworkFeatures.OutflowToDownStreamAquifer';
          TGeneralFlowChannelValidator(LDialogValidator).UpStreamEnabled      := False;
          TGeneralFlowChannelValidator(LDialogValidator).DownStreamEnabled    := True;
          TGeneralFlowChannelValidator(LDialogValidator).SelectChannelEnabled := False;
          TGeneralFlowChannelValidator(LDialogValidator).Mode                 := gfcvmOutflowToDownstreamAquiferChannel;
        end;

        if (lChannel.ChannelType = ctGroundWaterBaseflowChannel) then
        begin
          TGeneralFlowChannelValidator(LDialogValidator).Heading              := 'NetworkFeatures.GroundWaterBaseFlow';
          TGeneralFlowChannelValidator(LDialogValidator).UpStreamEnabled      := False;
          TGeneralFlowChannelValidator(LDialogValidator).DownStreamEnabled    := False;
          TGeneralFlowChannelValidator(LDialogValidator).SelectChannelEnabled := False;
          TGeneralFlowChannelValidator(LDialogValidator).Mode                 := gfcvmGroundWaterBaseflowChannel;
        end;

        if (lChannel.ChannelType = ctAbstractionFromAquiferChannel) then
        begin
          TGeneralFlowChannelValidator(LDialogValidator).Heading              := 'NetworkFeatures.AbtstractionFromAquifer';
          TGeneralFlowChannelValidator(LDialogValidator).UpStreamEnabled      := False;
          TGeneralFlowChannelValidator(LDialogValidator).DownStreamEnabled    := False;
          TGeneralFlowChannelValidator(LDialogValidator).SelectChannelEnabled := False;
          TGeneralFlowChannelValidator(LDialogValidator).Mode                 := gfcvmAbtstractionFromAquiferChannel;
        end;

        if (lChannel.ChannelType = ctAbstractionFromBaseFlowChannel) then
        begin
          TGeneralFlowChannelValidator(LDialogValidator).Heading              := 'NetworkFeatures.AbstractionFromGroundWater';
          TGeneralFlowChannelValidator(LDialogValidator).UpStreamEnabled      := False;
          TGeneralFlowChannelValidator(LDialogValidator).DownStreamEnabled    := False;
          TGeneralFlowChannelValidator(LDialogValidator).SelectChannelEnabled := False;
          TGeneralFlowChannelValidator(LDialogValidator).Mode                 := gfcvmAbtstractionGroundWaterBaseflowChannel;
        end;

        if (lChannel.ChannelType = ctAquiferExcessInterflowChannel) then
        begin
          TGeneralFlowChannelValidator(LDialogValidator).Heading              := 'NetworkFeatures.ExcessInterflow';
          TGeneralFlowChannelValidator(LDialogValidator).UpStreamEnabled      := False;
          TGeneralFlowChannelValidator(LDialogValidator).DownStreamEnabled    := False;
          TGeneralFlowChannelValidator(LDialogValidator).SelectChannelEnabled := False;
          TGeneralFlowChannelValidator(LDialogValidator).Mode                 := gfcvmExcessInterflowChannel;
        end;

        if (lChannel.ChannelType = ctSurfaceRunoffChannel) then
        begin
          TGeneralFlowChannelValidator(LDialogValidator).Heading              := 'NetworkFeatures.SurfaceRunOffChannel';
          TGeneralFlowChannelValidator(LDialogValidator).UpStreamEnabled      := False;
          TGeneralFlowChannelValidator(LDialogValidator).DownStreamEnabled    := False;
          TGeneralFlowChannelValidator(LDialogValidator).SelectChannelEnabled := False;
          TGeneralFlowChannelValidator(LDialogValidator).Mode                 := gfcvmSurfaceRunoffChannel;
        end;

        if (lChannel.ChannelType = ctGroundWaterBaseFlowRemainderChannel) then
        begin
          TGeneralFlowChannelValidator(LDialogValidator).Heading              := 'NetworkFeatures.GroundWaterBaseFlowRemainder';
          TGeneralFlowChannelValidator(LDialogValidator).UpStreamEnabled      := False;
          TGeneralFlowChannelValidator(LDialogValidator).DownStreamEnabled    := False;
          TGeneralFlowChannelValidator(LDialogValidator).SelectChannelEnabled := False;
          TGeneralFlowChannelValidator(LDialogValidator).Mode                 := gfcvmGroundWaterBaseFlowRemainderChannel;
        end;

        if (lChannel.ChannelType = ctGroundWaterAbstractionChannel) then
        begin
          TGeneralFlowChannelValidator(LDialogValidator).Heading              := 'NetworkFeatures.GroundWaterAbstraction';
          TGeneralFlowChannelValidator(LDialogValidator).UpStreamEnabled      := False;
          TGeneralFlowChannelValidator(LDialogValidator).DownStreamEnabled    := True;
          TGeneralFlowChannelValidator(LDialogValidator).SelectChannelEnabled := False;
          TGeneralFlowChannelValidator(LDialogValidator).Mode                 := gfcvmGroundWaterAbstractionChannel;
        end;

        if (lChannel.ChannelType = ctOutflowToNetworkChannel) then
        begin
          TGeneralFlowChannelValidator(LDialogValidator).Heading              := 'NetworkFeatures.OutflowToNetwork';
          TGeneralFlowChannelValidator(LDialogValidator).UpStreamEnabled      := False;
          TGeneralFlowChannelValidator(LDialogValidator).DownStreamEnabled    := True;
          TGeneralFlowChannelValidator(LDialogValidator).SelectChannelEnabled := False;
          TGeneralFlowChannelValidator(LDialogValidator).Mode                 := gfcvmOutflowToNetworkChannel;
        end;

        LDialogValidator.PopulateDataViewer;
        LDialogValidator.LanguageHasChanged;


        // Channel Penalty
        LDialogValidator := TChannelPenaltyValidator.Create(nil,FAppModules);
        FPopupFrameForm.AddValidator(LDialogValidator);
        LDialogValidator.Initialise;
        TChannelPenaltyValidator(LDialogValidator).ChannelNumber := LChannel.ChannelNumber;
        TChannelPenaltyValidator(lDialogValidator).ViewMode := vmEditableSelect;
        LDialogValidator.PopulateDataViewer;
        LDialogValidator.LanguageHasChanged;

        //Master Control Feature
        if(lMasterControlFeature <> nil) then
        begin
          LDialogValidator := TMasterControlChannelValidator.Create(nil,FAppModules);
          FPopupFrameForm.AddValidator(LDialogValidator);
          LDialogValidator.Initialise;
          TMasterControlChannelValidator(LDialogValidator).FeatureID := lMasterControlFeature.FeatureID;
          LDialogValidator.PopulateDataViewer;
          LDialogValidator.LanguageHasChanged;
        end;

        // Minimum Flow Constraint
        if (lMinimumConstraint <> nil) then
        begin
          LDialogValidator := TMinimumFlowChannelValidator.Create(nil,FAppModules);
          FPopupFrameForm.AddValidator(LDialogValidator);
          LDialogValidator.Initialise;
          TMinimumFlowChannelValidator(LDialogValidator).FeatureID := lMinimumConstraint.FeatureID;
          LDialogValidator.PopulateDataViewer;
          LDialogValidator.LanguageHasChanged;
        end;

        // Loss Feature
        if (lLossFeature <> nil) AND (lLossFeature.FeatureSubType <> 1) then
        begin
          LDialogValidator := TLossChannelValidator.Create(nil,FAppModules);
          FPopupFrameForm.AddValidator(LDialogValidator);
          LDialogValidator.Initialise;
          TLossChannelValidator(LDialogValidator).Identifier := LIdentifier;
          TLossChannelValidator(LDialogValidator).FeatureID := lLossFeature.FeatureID;
          LDialogValidator.PopulateDataViewer;
          LDialogValidator.LanguageHasChanged;
        end;

        // Min-Max Feature
        if (lMinMaxConstraint <> nil) then
        begin
          LDialogValidator := TMinMaxChannelValidator.Create(nil,FAppModules);
          FPopupFrameForm.AddValidator(LDialogValidator);
          LDialogValidator.Initialise;
          TMinMaxChannelValidator(LDialogValidator).Identifier := LIdentifier;
          TMinMaxChannelValidator(LDialogValidator).FeatureID := lMinMaxConstraint.FeatureID;
          LDialogValidator.PopulateDataViewer;
          LDialogValidator.LanguageHasChanged;
        end;

{          if (LMinMaxChannelGrowthFactors <> nil) and (LMinMaxConstraint <> nil) then
        begin
          LDialogValidator := TChannelDistributionValidator.Create(nil,FAppModules);
          FPopupFrameForm.AddValidator(LDialogValidator);
          LDialogValidator.Initialise;
          TChannelDistributionValidator(LDialogValidator).FeatureID := LMinMaxConstraint.FeatureID;
          LDialogValidator.PopulateDataViewer;
          LDialogValidator.LanguageHasChanged;
        end;
}
        // Pumping Feature
        if (lPumpingFeature <> nil) then
        begin
          LDialogValidator := TPumpingFeatureValidator.Create(nil,FAppModules);
          FPopupFrameForm.AddValidator(LDialogValidator);
          LDialogValidator.Initialise;
          TPumpingFeatureValidator(LDialogValidator).FeatureID := lPumpingFeature.FeatureID;
          LDialogValidator.PopulateDataViewer;
          LDialogValidator.LanguageHasChanged;
        end;

        // YM Demand Centre Return Flow
        {Showmessage('ask about populating this data');
        if LChannelType = 20 then
        //if (lYMDemandCentreReturnFlowFeature <> nil) then
        begin
          LDialogValidator := TYMDemandCentreReturnFlowFeatureValidator.Create(nil,FAppModules);
          APageControl.AddValidator(LDialogValidator);
          LDialogValidator.Initialise;
          //TYMDemandCentreReturnFlowFeatureValidator(LDialogValidator).FeatureID := lYMDemandCentreReturnFlowFeature.FeatureID;
          TYMDemandCentreReturnFlowFeatureValidator(LDialogValidator).FeatureID := LChannel.ChannelID;  //LIdentifier;
          LDialogValidator.PopulateDataViewer;
          LDialogValidator.LanguageHasChanged;
        end;}

        // Specified Demand Channel
        if (lDemandFeature <> nil) then
        begin
          LDialogValidator := TSpecifiedDemandChannelValidator.Create(nil,FAppModules);
          FPopupFrameForm.AddValidator(LDialogValidator);
          LDialogValidator.Initialise;
          TSpecifiedDemandChannelValidator(LDialogValidator).FeatureID := lDemandFeature.FeatureID;
          LDialogValidator.PopulateDataViewer;
          LDialogValidator.LanguageHasChanged;
        end;

        // Diversion Feature
        if (lDivFeature <> nil) then
        begin
          LDialogValidator := TDiversionFeatureValidator.Create(nil,FAppModules);
          FPopupFrameForm.AddValidator(LDialogValidator);
          LDialogValidator.Initialise;
          TDiversionFeatureValidator(LDialogValidator).FeatureID := lDivFeature.FeatureID;
          LDialogValidator.PopulateDataViewer;
          LDialogValidator.LanguageHasChanged;
        end;

        // Physical Flow Constraint
        if (lPhysConstraint <> nil) then
        begin
          LDialogValidator := TPhysicalFlowConstraintValidator.Create(nil,FAppModules);
          FPopupFrameForm.AddValidator(LDialogValidator);
          LDialogValidator.Initialise;
          TPhysicalFlowConstraintValidator(LDialogValidator).FeatureID := lPhysConstraint.FeatureID;
          LDialogValidator.PopulateDataViewer;
          LDialogValidator.LanguageHasChanged;
        end;

        // IFR Feature
        if (lIFRFeature <> nil) then
        begin
          if (lIFRFeature.Get_ReferenceFlowType = ifrtMonthly) then
          begin
            LDialogValidator := TIFRFeatureValidator.Create(nil,FAppModules);
            FPopupFrameForm.AddValidator(LDialogValidator);
            LDialogValidator.Initialise;
            TIFRFeatureValidator(LDialogValidator).FeatureID := lIFRFeature.FeatureID;
            LDialogValidator.PopulateDataViewer;
            LDialogValidator.LanguageHasChanged;
          end;
          if (lIFRFeature.Get_ReferenceFlowType = ifrrftAnnual) then
          begin
            LDialogValidator := TAnnualIFRFeatureValidator.Create(nil,FAppModules);
            FPopupFrameForm.AddValidator(LDialogValidator);
            LDialogValidator.Initialise;
            TAnnualIFRFeatureValidator(LDialogValidator).FeatureID := lIFRFeature.FeatureID;
            LDialogValidator.PopulateDataViewer;
            LDialogValidator.LanguageHasChanged;
          end;
        end;
        // Specified Inflow Data
        if (lSpecInflow <> nil) then
        begin
          LDialogValidator := TSpecifiedInflowDataValidator.Create(nil,FAppModules);
          FPopupFrameForm.AddValidator(LDialogValidator);
          LDialogValidator.Initialise;
          TSpecifiedInflowDataValidator(LDialogValidator).FeatureID := lSpecInflow.FeatureID;
          LDialogValidator.PopulateDataViewer;
          LDialogValidator.LanguageHasChanged;
        end;

        // Water demand feature
        if (LWaterDemandFeature <> nil) then
        begin
          LDialogValidator := TWaterDemandFeatureValidator.Create(nil,FAppModules);
          FPopupFrameForm.AddValidator(LDialogValidator);
          LDialogValidator.Initialise;
          TSpecifiedInflowDataValidator(LDialogValidator).FeatureID := LWaterDemandFeature.FeatureID;
          LDialogValidator.PopulateDataViewer;
          LDialogValidator.LanguageHasChanged;
        end;

        //Channel Time Control
        if (lChannel.TimeControl <> nil) then
        begin
          LDialogValidator := TChannelTimeControlValidator.Create(nil,FAppModules);
          FPopupFrameForm.AddValidator(LDialogValidator);
          LDialogValidator.Initialise;
          TChannelTimeControlValidator(LDialogValidator).Identifier := LIdentifier;
          LDialogValidator.PopulateDataViewer;
          LDialogValidator.LanguageHasChanged;
        end;


        //Channel Switch Control
        if (FAppModules.Model.ModelName = CYield) then
        begin
          if (lChannel.SwitchControlCount > 0) then
          begin
            LDialogValidator := TChannelSwitchControlValidator.Create(nil,FAppModules);
            FPopupFrameForm.AddValidator(LDialogValidator);
            LDialogValidator.Initialise;
            TChannelSwitchControlValidator(LDialogValidator).Identifier := lChannel.ChannelID;//LIdentifier;
            LDialogValidator.PopulateDataViewer;
            LDialogValidator.LanguageHasChanged;
          end;
        end
        else
        begin
          if (lChannel.ChannelType <> 2) then
          begin
            LDialogValidator := TChannelSwitchControlValidator.Create(nil,FAppModules);
            FPopupFrameForm.AddValidator(LDialogValidator);
            LDialogValidator.Initialise;
            TChannelSwitchControlValidator(LDialogValidator).Identifier := lChannel.ChannelID;//LIdentifier;
            LDialogValidator.PopulateDataViewer;
            LDialogValidator.LanguageHasChanged;
          end;
        end;
      end;
    end
    else
    if (LViewName = mdvnChannelPenalties) then
    begin
      //CHANNEL PENALTIES
      LDialogValidator := TChannelPenaltyValidator.Create(nil,FAppModules);
      FPopupFrameForm.AddValidator(LDialogValidator);
      LDialogValidator.Initialise;
      TChannelPenaltyValidator(lDialogValidator).ViewMode := vmEditable;
      LDialogValidator.PopulateDataViewer;
      LDialogValidator.LanguageHasChanged;
    end;

    if (LViewName = mdvnWetland) then
    begin
      LWetland        := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.WetlandList.WetlandByNodeNumber[LIdentifier];
      lFeatureID      := LWetland.Identifier;

      //Wetland Properties
      LDialogValidator := TWetlandValidator.Create(nil,FAppModules);
      FPopupFrameForm.AddValidator(LDialogValidator);
      LDialogValidator.Initialise;
      TWetlandValidator(LDialogValidator).Identifier := lFeatureID;
      LDialogValidator.PopulateDataViewer;
      LDialogValidator.LanguageHasChanged;

      //Inflow Channel
      LDialogValidator := TGeneralFlowChannelValidator.Create(nil,FAppModules);
      FPopupFrameForm.AddValidator(LDialogValidator);
      LDialogValidator.Initialise;
      TGeneralFlowChannelValidator(LDialogValidator).Identifier     := lFeatureID;

      if(lWetland <> nil) and (lWetland.InflowChannel <> nil) then
        TGeneralFlowChannelValidator(LDialogValidator).Identifier         := lWetland.InflowChannel.ChannelNumber;
      TGeneralFlowChannelValidator(LDialogValidator).Heading              := 'NetworkFeatures.InflowChannelProperties';
      TGeneralFlowChannelValidator(LDialogValidator).UpStreamEnabled      := FALSE;
      TGeneralFlowChannelValidator(LDialogValidator).DownStreamEnabled    := FALSE;
      TGeneralFlowChannelValidator(LDialogValidator).SelectChannelEnabled := FALSE;
      TGeneralFlowChannelValidator(LDialogValidator).Mode                 := gfcvmWetlandUpstream;
      LDialogValidator.PopulateDataViewer;
      LDialogValidator.LanguageHasChanged;

      //Outflow Channel
      LDialogValidator := TGeneralFlowChannelValidator.Create(nil,FAppModules);
      FPopupFrameForm.AddValidator(LDialogValidator);
      LDialogValidator.Initialise;
      TGeneralFlowChannelValidator(LDialogValidator).Identifier           := lFeatureID;
      if(LWetland <> nil) and (LWetland.OutflowChannel <> nil) then
        TGeneralFlowChannelValidator(LDialogValidator).Identifier         := LWetland.OutflowChannel.ChannelNumber;
      TGeneralFlowChannelValidator(LDialogValidator).Heading              := 'NetworkFeatures.OutflowChannelProperties';
      TGeneralFlowChannelValidator(LDialogValidator).UpStreamEnabled      := FALSE;
      TGeneralFlowChannelValidator(LDialogValidator).DownStreamEnabled    := FALSE;
      TGeneralFlowChannelValidator(LDialogValidator).SelectChannelEnabled := FALSE;
      TGeneralFlowChannelValidator(LDialogValidator).Mode                 := gfcvmWetlandDownstream;
      LDialogValidator.PopulateDataViewer;
      LDialogValidator.LanguageHasChanged;

      //Reservoir Evaporation
      LDialogValidator := TReservoirEvaporationValidator.Create(nil,FAppModules);
      FPopupFrameForm.AddValidator(LDialogValidator);
      LDialogValidator.Initialise;
      TReservoirEvaporationValidator(LDialogValidator).Identifier := LWetland.NodeNumber;
      LDialogValidator.PopulateDataViewer;
      LDialogValidator.LanguageHasChanged;

      //Reservoir Physical Characteristics
      LDialogValidator := TReservoirPhysicalCharacteristicsValidator.Create(nil,FAppModules);
      FPopupFrameForm.AddValidator(LDialogValidator);
      LDialogValidator.Initialise;
      TReservoirPhysicalCharacteristicsValidator(LDialogValidator).Identifier := LWetland.NodeNumber;
      LDialogValidator.PopulateDataViewer;
      LDialogValidator.LanguageHasChanged;

      //Reservoir Catchment Proportions
      LDialogValidator := TReservoirCatchmentProportionsValidator.Create(nil,FAppModules);
      FPopupFrameForm.AddValidator(LDialogValidator);
      LDialogValidator.Initialise;
      TReservoirCatchmentProportionsValidator(LDialogValidator).Identifier := LWetland.NodeNumber;
      TReservoirCatchmentProportionsValidator(LDialogValidator).ViewMode :=  vmEditableSelect;
      LDialogValidator.PopulateDataViewer;
      LDialogValidator.LanguageHasChanged;

      //Reservoir Penalty
      LDialogValidator := TReservoirPenaltyValidator.Create(nil,FAppModules);
      FPopupFrameForm.AddValidator(LDialogValidator);
      LDialogValidator.Initialise;
      TReservoirPenaltyValidator(LDialogValidator).ReservoirNumber := LWetland.NodeNumber;
      LDialogValidator.PopulateDataViewer;
      LDialogValidator.LanguageHasChanged;
      TReservoirPenaltyValidator(LDialogValidator).ViewMode := vmEditableSelect;

      //Reservoir Zone Elevations
      LDialogValidator := TReservoirZoneElevationsValidator.Create(nil,FAppModules);
      FPopupFrameForm.AddValidator(LDialogValidator);
      LDialogValidator.Initialise;
      TReservoirZoneElevationsValidator(LDialogValidator).Identifier := LWetland.NodeNumber;
      LDialogValidator.PopulateDataViewer;
      LDialogValidator.LanguageHasChanged;
    end;

    if (LViewName = mdvnYMDemandCentre) then
    begin
      LYMDemandCentre := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.YMDemandCentreList.YMDemandCentreByNodeNumber[LIdentifier];
      lFeatureID      := LYMDemandCentre.Identifier;

      //Demand Centre Properties
      LDialogValidator := TYMDemandCentreValidator.Create(nil,FAppModules);
      FPopupFrameForm.AddValidator(LDialogValidator);
      LDialogValidator.Initialise;
      TYMDemandCentreValidator(LDialogValidator).Identifier := lFeatureID;
      LDialogValidator.PopulateDataViewer;
      LDialogValidator.LanguageHasChanged;

      //Demand Centre Supply channel Properties
      LDialogValidator := TYMDemandCentreSupplyChannelValidator.Create(nil,FAppModules);
      FPopupFrameForm.AddValidator(LDialogValidator);
      LDialogValidator.Initialise;
      TYMDemandCentreSupplyChannelValidator(LDialogValidator).Identifier := lFeatureID;
      LDialogValidator.PopulateDataViewer;
      LDialogValidator.LanguageHasChanged;

      //Consumptive Water Use
      {LDialogValidator := TGeneralFlowChannelValidator.Create(nil,FAppModules);
      APageControl.AddValidator(LDialogValidator);
      LDialogValidator.Initialise;
      if(LYMDemandCentre <> nil) and (LYMDemandCentre.ConsumptiveUseChannel <> nil) then
        TGeneralFlowChannelValidator(LDialogValidator).Identifier      := LYMDemandCentre.ConsumptiveUseChannel.ChannelNumber;
      TGeneralFlowChannelValidator(LDialogValidator).Heading           := 'NetworkFeatures.DemandCentreConsumptiveChannelProperties';
      TGeneralFlowChannelValidator(LDialogValidator).UpStreamEnabled   := False;
      TGeneralFlowChannelValidator(LDialogValidator).DownStreamEnabled := True;
      TGeneralFlowChannelValidator(LDialogValidator).SelectChannelEnabled := False;
      TGeneralFlowChannelValidator(LDialogValidator).Mode               := gfcvmDemandCentreConsumptiveUpstream;
      LDialogValidator.PopulateDataViewer;
      LDialogValidator.LanguageHasChanged; }

      //Demand Centre Return flow channel Properties
      LDialogValidator := TYMDemandCentreReturnFlowChannelValidator.Create(nil,FAppModules);
      FPopupFrameForm.AddValidator(LDialogValidator);
      LDialogValidator.Initialise;
      TYMDemandCentreReturnFlowChannelValidator(LDialogValidator).Identifier := lFeatureID;
      LDialogValidator.PopulateDataViewer;
      LDialogValidator.LanguageHasChanged;

      //Reclaimation Plant Loss
      LDialogValidator := TGeneralFlowChannelValidator.Create(nil,FAppModules);
      FPopupFrameForm.AddValidator(LDialogValidator);
      LDialogValidator.Initialise;

      if(LYMDemandCentre <> nil) and (LYMDemandCentre.ReclaimationChannel <> nil)
         and (LYMDemandCentre.ReclaimationPlantExists) then
        TGeneralFlowChannelValidator(LDialogValidator).Identifier         := LYMDemandCentre.ReclaimationChannel.ChannelNumber;

      TGeneralFlowChannelValidator(LDialogValidator).Heading              := 'NetworkFeatures.DemandCentreReclaimationChannelProperties';
      TGeneralFlowChannelValidator(LDialogValidator).UpStreamEnabled      := False;
      TGeneralFlowChannelValidator(LDialogValidator).DownStreamEnabled    := True;
      TGeneralFlowChannelValidator(LDialogValidator).SelectChannelEnabled := False;
      TGeneralFlowChannelValidator(LDialogValidator).Mode                 := gfcvmDemandCentreReclaimationUpstream;
      LDialogValidator.PopulateDataViewer;
      LDialogValidator.LanguageHasChanged;

      LDialogValidator := TYMDemandCentreEvaporationValidator.Create(nil,FAppModules);
      FPopupFrameForm.AddValidator(LDialogValidator);
      LDialogValidator.Initialise;
      TYMDemandCentreEvaporationValidator(LDialogValidator).Identifier := lFeatureID;
      LDialogValidator.PopulateDataViewer;
      LDialogValidator.LanguageHasChanged;

    end;

    if (LViewName = mdvnStreamFlowReduction) then
    begin
      // Switch Definition
      LDialogValidator := TStreamFlowReductionValidator.Create(nil,FAppModules);
      FPopupFrameForm.AddValidator(LDialogValidator);
      LDialogValidator.Initialise;
      TStreamFlowReductionValidator(LDialogValidator).Identifier := LIdentifier;
      LDialogValidator.PopulateDataViewer;
      LDialogValidator.LanguageHasChanged;
    end;

    if (LViewName = mdvnMine) then
    begin
      LMine := TYieldModelDataObject(FAppModules.Model.ModelData).CastNetworkFeaturesData.CastMineList.MineByNodeNumber[LIdentifier];

      LDialogValidator := TMiningValidator.Create(nil,FAppModules);
      FPopupFrameForm.AddValidator(LDialogValidator);
      LDialogValidator.Initialise;
      TMiningValidator(LDialogValidator).Identifier := LIdentifier;
      LDialogValidator.PopulateDataViewer;
      LDialogValidator.LanguageHasChanged;

      LDialogValidator := TGeneralFlowChannelValidator.Create(nil,FAppModules);
      FPopupFrameForm.AddValidator(LDialogValidator);
      LDialogValidator.Initialise;
      if (LMine <> nil) AND (LMine.RiverChannel <> nil) then
        TGeneralFlowChannelValidator(LDialogValidator).Identifier         := LMine.RiverChannel.ChannelNumber;
      TGeneralFlowChannelValidator(LDialogValidator).Heading              := 'NetworkFeatures.MineToRiverChannelProperties';
      TGeneralFlowChannelValidator(LDialogValidator).UpStreamEnabled      := False;
      TGeneralFlowChannelValidator(LDialogValidator).DownStreamEnabled    := True;
      TGeneralFlowChannelValidator(LDialogValidator).SelectChannelEnabled := False;
      TGeneralFlowChannelValidator(LDialogValidator).Mode                 := gfcvmMineToRiver;
      LDialogValidator.PopulateDataViewer;
      LDialogValidator.LanguageHasChanged;

      LDialogValidator := TMiningOpenCastPitValidator.Create(nil,FAppModules);
      FPopupFrameForm.AddValidator(LDialogValidator);
      LDialogValidator.Initialise;
      TMiningOpenCastPitValidator(LDialogValidator).Identifier := LIdentifier;
      LDialogValidator.PopulateDataViewer;
      LDialogValidator.LanguageHasChanged;

      LDialogValidator := TMiningUnderGroundSectionValidator.Create(nil,FAppModules);
      FPopupFrameForm.AddValidator(LDialogValidator);
      LDialogValidator.Initialise;
      TMiningUnderGroundSectionValidator(LDialogValidator).Identifier := LIdentifier;
      LDialogValidator.PopulateDataViewer;
      LDialogValidator.LanguageHasChanged;

      LDialogValidator := TMiningSlurryDumpValidator.Create(nil,FAppModules);
      FPopupFrameForm.AddValidator(LDialogValidator);
      LDialogValidator.Initialise;
      TMiningSlurryDumpValidator(LDialogValidator).Identifier := LIdentifier;
      LDialogValidator.PopulateDataViewer;
      LDialogValidator.LanguageHasChanged;
    end;

    if (LViewName = mdvnGroundwaterSubcatchment) then
    begin
      LDialogValidator := TMineSubcatchmentValidator.Create(nil,FAppModules);
      FPopupFrameForm.AddValidator(LDialogValidator);
      LDialogValidator.Initialise;
      LDialogValidator.PopulateDataViewer;
      LDialogValidator.LanguageHasChanged;
    end;

    if (LViewName = mdvnGroundWater) then
    begin
      LGroundWater := TYieldModelDataObject(FAppModules.Model.ModelData).CastNetworkFeaturesData.
                      CastGroundWaterList.GroundWaterByID[LIdentifier];

      LDialogValidator := TGroundWaterValidator.Create(nil,FAppModules);
      FPopupFrameForm.AddValidator(LDialogValidator);
      LDialogValidator.Initialise;
      TGroundWaterValidator(LDialogValidator).Identifier := LIdentifier;
      LDialogValidator.PopulateDataViewer;
      LDialogValidator.LanguageHasChanged;

      LDialogValidator := TGroundWaterPitmanValidator.Create(nil,FAppModules);
      FPopupFrameForm.AddValidator(LDialogValidator);
      LDialogValidator.Initialise;
      TGroundWaterPitmanValidator(LDialogValidator).Identifier := LIdentifier;
      LDialogValidator.PopulateDataViewer;
      LDialogValidator.LanguageHasChanged;
    end;

    if (LViewName = mdvnAquiferNode) then
    begin
      LGroundWater := TYieldModelDataObject(FAppModules.Model.ModelData).CastNetworkFeaturesData.
                      CastGroundWaterList.GroundWaterByNodeNumber[LIdentifier];
      if LGroundWater <> nil then
      begin
        LDialogValidator := TReservoirPropertiesValidator.Create(nil,FAppModules);
        FPopupFrameForm.AddValidator(LDialogValidator);
        LDialogValidator.Initialise;
        TReservoirPropertiesValidator(LDialogValidator).Identifier := LIdentifier;
        LDialogValidator.PopulateDataViewer;
        LDialogValidator.LanguageHasChanged;

        LDialogValidator := TGeneralFlowChannelValidator.Create(nil,FAppModules);
        FPopupFrameForm.AddValidator(LDialogValidator);
        LDialogValidator.Initialise;
        if (LGroundWater.AquiferInflowChannel <> nil) then
          TGeneralFlowChannelValidator(LDialogValidator).Identifier         := LGroundWater.AquiferInflowChannelNr;
        TGeneralFlowChannelValidator(LDialogValidator).Heading              := 'NetworkFeatures.GroundWaterAquiferInflowChannel';
        TGeneralFlowChannelValidator(LDialogValidator).UpStreamEnabled      := False;
        TGeneralFlowChannelValidator(LDialogValidator).DownStreamEnabled    := False;
        TGeneralFlowChannelValidator(LDialogValidator).SelectChannelEnabled := False;
        TGeneralFlowChannelValidator(LDialogValidator).Mode                 := gfcvmGroundWaterAquiferInflowChannel;
        LDialogValidator.PopulateDataViewer;
        LDialogValidator.LanguageHasChanged;

        LDialogValidator := TGeneralFlowChannelValidator.Create(nil,FAppModules);
        FPopupFrameForm.AddValidator(LDialogValidator);
        LDialogValidator.Initialise;
        if(LGroundWater.InflowFromUpstreamAquiferChannel <> nil) then
          TGeneralFlowChannelValidator(LDialogValidator).Identifier         := LGroundWater.InflowFromUpstreamAquiferChannelNr;
        TGeneralFlowChannelValidator(LDialogValidator).Heading              := 'NetworkFeatures.InflowFromUpstreamAquifer';
        TGeneralFlowChannelValidator(LDialogValidator).UpStreamEnabled      := True;
        TGeneralFlowChannelValidator(LDialogValidator).DownStreamEnabled    := False;
        TGeneralFlowChannelValidator(LDialogValidator).SelectChannelEnabled := False;
        TGeneralFlowChannelValidator(LDialogValidator).Mode                 := gfcvmInflowFromUpstreamAquiferChannel;
        LDialogValidator.PopulateDataViewer;
        LDialogValidator.LanguageHasChanged;

        LDialogValidator := TGeneralFlowChannelValidator.Create(nil,FAppModules);
        FPopupFrameForm.AddValidator(LDialogValidator);
        LDialogValidator.Initialise;
        if (LGroundWater.OutflowToDownstreamAquiferChannel <> nil) then
          TGeneralFlowChannelValidator(LDialogValidator).Identifier         := LGroundWater.OutflowToDownstreamAquiferChannelNr;
        TGeneralFlowChannelValidator(LDialogValidator).Heading              := 'NetworkFeatures.OutflowToDownStreamAquifer';
        TGeneralFlowChannelValidator(LDialogValidator).UpStreamEnabled      := False;
        TGeneralFlowChannelValidator(LDialogValidator).DownStreamEnabled    := True;
        TGeneralFlowChannelValidator(LDialogValidator).SelectChannelEnabled := False;
        TGeneralFlowChannelValidator(LDialogValidator).Mode                 := gfcvmOutflowToDownstreamAquiferChannel;
        LDialogValidator.PopulateDataViewer;
        LDialogValidator.LanguageHasChanged;

        //Reservoir Physical Characteristics
        LDialogValidator := TReservoirPhysicalCharacteristicsValidator.Create(nil,FAppModules);
        FPopupFrameForm.AddValidator(LDialogValidator);
        LDialogValidator.Initialise;
        TReservoirPhysicalCharacteristicsValidator(LDialogValidator).Identifier := LIdentifier;
        LDialogValidator.PopulateDataViewer;
        LDialogValidator.LanguageHasChanged;

        //Reservoir Catchment Proportions
        LDialogValidator := TReservoirCatchmentProportionsValidator.Create(nil,FAppModules);
        FPopupFrameForm.AddValidator(LDialogValidator);
        LDialogValidator.Initialise;
        TReservoirCatchmentProportionsValidator(LDialogValidator).Identifier := LIdentifier;
        TReservoirCatchmentProportionsValidator(LDialogValidator).ViewMode :=  vmEditableSelect;
        LDialogValidator.PopulateDataViewer;
        LDialogValidator.LanguageHasChanged;

        //Reservoir Penalty
        LDialogValidator := TReservoirPenaltyValidator.Create(nil,FAppModules);
        FPopupFrameForm.AddValidator(LDialogValidator);
        LDialogValidator.Initialise;
        TReservoirPenaltyValidator(LDialogValidator).ReservoirNumber := LIdentifier;
        LDialogValidator.PopulateDataViewer;
        LDialogValidator.LanguageHasChanged;
        TReservoirPenaltyValidator(LDialogValidator).ViewMode := vmEditableSelect;

        //Reservoir Zone Elevations
        LDialogValidator := TReservoirZoneElevationsValidator.Create(nil,FAppModules);
        FPopupFrameForm.AddValidator(LDialogValidator);
        LDialogValidator.Initialise;
        TReservoirZoneElevationsValidator(LDialogValidator).Identifier := LIdentifier;
        LDialogValidator.PopulateDataViewer;
        LDialogValidator.LanguageHasChanged;
      end;
    end;

    if (LViewName = mdvnBaseFlowNode) then
    begin
      LGroundWater := TYieldModelDataObject(FAppModules.Model.ModelData).CastNetworkFeaturesData.
                      CastGroundWaterList.GroundWaterByBaseFlowNumber[LIdentifier];
      if (LGroundWater <> nil) then
      begin

        LDialogValidator := TNodePropertiesValidator.Create(nil,FAppModules);
        FPopupFrameForm.AddValidator(LDialogValidator);
        LDialogValidator.Initialise;

        if (LGroundWater.BaseFlowNode <> nil) then
          TNodePropertiesValidator(LDialogValidator).Identifier := LGroundWater.BaseFlowNodeNr;
        LDialogValidator.PopulateDataViewer;
        LDialogValidator.LanguageHasChanged;

        LDialogValidator := TGeneralFlowChannelValidator.Create(nil,FAppModules);
        FPopupFrameForm.AddValidator(LDialogValidator);
        LDialogValidator.Initialise;
        if (LGroundWater.GroundWaterBaseflowChannel <> nil) then
          TGeneralFlowChannelValidator(LDialogValidator).Identifier         := LGroundWater.GroundWaterBaseflowChannelNr;
        TGeneralFlowChannelValidator(LDialogValidator).Heading              := 'NetworkFeatures.GroundWaterBaseFlow';
        TGeneralFlowChannelValidator(LDialogValidator).UpStreamEnabled      := False;
        TGeneralFlowChannelValidator(LDialogValidator).DownStreamEnabled    := False;
        TGeneralFlowChannelValidator(LDialogValidator).SelectChannelEnabled := False;
        TGeneralFlowChannelValidator(LDialogValidator).Mode                 := gfcvmGroundWaterBaseflowChannel;
        LDialogValidator.PopulateDataViewer;
        LDialogValidator.LanguageHasChanged;
      end;
    end;

    if (LViewName = mdvnAbstractionNode) then
    begin
      LGroundWater := TYieldModelDataObject(FAppModules.Model.ModelData).CastNetworkFeaturesData.
                        CastGroundWaterList.GroundWaterByAbstractionNodeNumber[LIdentifier];
      if (LGroundWater <> nil) then
      begin
        LDialogValidator := TNodePropertiesValidator.Create(nil,FAppModules);
        FPopupFrameForm.AddValidator(LDialogValidator);
        LDialogValidator.Initialise;

        if (LGroundWater <> nil) AND (LGroundWater.BaseFlowNode <> nil) then
          TNodePropertiesValidator(LDialogValidator).Identifier         := LGroundWater.AbstractionNodeNr;
        LDialogValidator.PopulateDataViewer;
        LDialogValidator.LanguageHasChanged;

        LDialogValidator := TGeneralFlowChannelValidator.Create(nil,FAppModules);
        FPopupFrameForm.AddValidator(LDialogValidator);
        LDialogValidator.Initialise;
        if (LGroundWater.AbstractionFromAquiferChannel <> nil) then
          TGeneralFlowChannelValidator(LDialogValidator).Identifier         := LGroundWater.AbstractionFromAquiferChannelNr;
        TGeneralFlowChannelValidator(LDialogValidator).Heading              := 'NetworkFeatures.AbtstractionFromAquifer';
        TGeneralFlowChannelValidator(LDialogValidator).UpStreamEnabled      := False;
        TGeneralFlowChannelValidator(LDialogValidator).DownStreamEnabled    := False;
        TGeneralFlowChannelValidator(LDialogValidator).SelectChannelEnabled := False;
        TGeneralFlowChannelValidator(LDialogValidator).Mode                 := gfcvmAbtstractionFromAquiferChannel;
        LDialogValidator.PopulateDataViewer;
        LDialogValidator.LanguageHasChanged;

        LDialogValidator := TGeneralFlowChannelValidator.Create(nil,FAppModules);
        FPopupFrameForm.AddValidator(LDialogValidator);
        LDialogValidator.Initialise;
        if (LGroundWater.AbstractionFromBaseFlowChannel <> nil) then
          TGeneralFlowChannelValidator(LDialogValidator).Identifier         := LGroundWater.AbstractionFromBaseflowChannelNr;
        TGeneralFlowChannelValidator(LDialogValidator).Heading              := 'NetworkFeatures.AbstractionFromGroundWater';
        TGeneralFlowChannelValidator(LDialogValidator).UpStreamEnabled      := False;
        TGeneralFlowChannelValidator(LDialogValidator).DownStreamEnabled    := False;
        TGeneralFlowChannelValidator(LDialogValidator).SelectChannelEnabled := False;
        TGeneralFlowChannelValidator(LDialogValidator).Mode                 := gfcvmAbtstractionGroundWaterBaseflowChannel;
        LDialogValidator.PopulateDataViewer;
        LDialogValidator.LanguageHasChanged;

        LDialogValidator := TGeneralFlowChannelValidator.Create(nil,FAppModules);
        FPopupFrameForm.AddValidator(LDialogValidator);
        LDialogValidator.Initialise;
        if (LGroundWater.GroundWaterAbstractionChannel <> nil) then
          TGeneralFlowChannelValidator(LDialogValidator).Identifier         := LGroundWater.GroundWaterAbstractionChannelNr;
        TGeneralFlowChannelValidator(LDialogValidator).Heading              := 'NetworkFeatures.GroundWaterAbstraction';
        TGeneralFlowChannelValidator(LDialogValidator).UpStreamEnabled      := False;
        TGeneralFlowChannelValidator(LDialogValidator).DownStreamEnabled    := True;
        TGeneralFlowChannelValidator(LDialogValidator).SelectChannelEnabled := False;
        TGeneralFlowChannelValidator(LDialogValidator).Mode                 := gfcvmGroundWaterAbstractionChannel;
        LDialogValidator.PopulateDataViewer;
        LDialogValidator.LanguageHasChanged;
      end;
    end;

    if (LViewName = mdvnCollectionNode) then
    begin
      LGroundWater := TYieldModelDataObject(FAppModules.Model.ModelData).CastNetworkFeaturesData.
                      CastGroundWaterList.GroundWaterByCollectionNodeNumber[LIdentifier];
      if (LGroundWater <> nil) then
      begin

        LDialogValidator := TNodePropertiesValidator.Create(nil,FAppModules);
        FPopupFrameForm.AddValidator(LDialogValidator);
        LDialogValidator.Initialise;

        if (LGroundWater <> nil) AND (LGroundWater.CollectionNode <> nil) then
          TNodePropertiesValidator(LDialogValidator).Identifier         := LGroundWater.CollectionNodeNr;
        LDialogValidator.PopulateDataViewer;
        LDialogValidator.LanguageHasChanged;

        LDialogValidator := TGeneralFlowChannelValidator.Create(nil,FAppModules);
        FPopupFrameForm.AddValidator(LDialogValidator);
        LDialogValidator.Initialise;
        if (LGroundWater.SurfaceRunoffAndSoilInterflowChannel <> nil) then
          TGeneralFlowChannelValidator(LDialogValidator).Identifier         := LGroundWater.SurfaceRunoffAndSoilInterflowChannelNr;
        TGeneralFlowChannelValidator(LDialogValidator).Heading              := 'NetworkFeatures.SurfaceRunOffChannel';
        TGeneralFlowChannelValidator(LDialogValidator).UpStreamEnabled      := False;
        TGeneralFlowChannelValidator(LDialogValidator).DownStreamEnabled    := False;
        TGeneralFlowChannelValidator(LDialogValidator).SelectChannelEnabled := False;
        TGeneralFlowChannelValidator(LDialogValidator).Mode                 := gfcvmSurfaceRunoffChannel;
        LDialogValidator.PopulateDataViewer;
        LDialogValidator.LanguageHasChanged;

        LDialogValidator := TGeneralFlowChannelValidator.Create(nil,FAppModules);
        FPopupFrameForm.AddValidator(LDialogValidator);
        LDialogValidator.Initialise;
        if (LGroundWater.AquiferExcessInterflowChannel <> nil) then
          TGeneralFlowChannelValidator(LDialogValidator).Identifier         := LGroundWater.AquiferExcessInterflowChannelNr;
        TGeneralFlowChannelValidator(LDialogValidator).Heading              := 'NetworkFeatures.ExcessInterflow';
        TGeneralFlowChannelValidator(LDialogValidator).UpStreamEnabled      := False;
        TGeneralFlowChannelValidator(LDialogValidator).DownStreamEnabled    := False;
        TGeneralFlowChannelValidator(LDialogValidator).SelectChannelEnabled := False;
        TGeneralFlowChannelValidator(LDialogValidator).Mode                 := gfcvmExcessInterflowChannel;
        LDialogValidator.PopulateDataViewer;
        LDialogValidator.LanguageHasChanged;

        LDialogValidator := TGeneralFlowChannelValidator.Create(nil,FAppModules);
        FPopupFrameForm.AddValidator(LDialogValidator);
        LDialogValidator.Initialise;
        if (LGroundWater.GroundWaterBaseFlowRemainderChannel <> nil) then
          TGeneralFlowChannelValidator(LDialogValidator).Identifier         := LGroundWater.GroundWaterBaseFlowRemainderChannelNr;
        TGeneralFlowChannelValidator(LDialogValidator).Heading              := 'NetworkFeatures.GroundWaterBaseFlowRemainder';
        TGeneralFlowChannelValidator(LDialogValidator).UpStreamEnabled      := False;
        TGeneralFlowChannelValidator(LDialogValidator).DownStreamEnabled    := False;
        TGeneralFlowChannelValidator(LDialogValidator).SelectChannelEnabled := False;
        TGeneralFlowChannelValidator(LDialogValidator).Mode                 := gfcvmGroundWaterBaseFlowRemainderChannel;
        LDialogValidator.PopulateDataViewer;
        LDialogValidator.LanguageHasChanged;

        LDialogValidator := TGeneralFlowChannelValidator.Create(nil,FAppModules);
        FPopupFrameForm.AddValidator(LDialogValidator);
        LDialogValidator.Initialise;
        if (LGroundWater.OutflowToNetworkChannel <> nil) then
          TGeneralFlowChannelValidator(LDialogValidator).Identifier         := LGroundWater.OutflowToNetworkChannelNr;
        TGeneralFlowChannelValidator(LDialogValidator).Heading              := 'NetworkFeatures.OutflowToNetwork';
        TGeneralFlowChannelValidator(LDialogValidator).UpStreamEnabled      := False;
        TGeneralFlowChannelValidator(LDialogValidator).DownStreamEnabled    := True;
        TGeneralFlowChannelValidator(LDialogValidator).SelectChannelEnabled := False;
        TGeneralFlowChannelValidator(LDialogValidator).Mode                 := gfcvmOutflowToNetworkChannel;
        LDialogValidator.PopulateDataViewer;
        LDialogValidator.LanguageHasChanged;

      end;
    end;

    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldModelDataGUIManager.ViewOutputComparisonData(ACommaTextContextData : String; APageControl   : TAbstractDataPageControl): boolean;
const OPNAME = 'TYieldModelDataGUIManager.ViewOutputComparisonData';
var
  LViewName           : string;
  LContextDataList    : TStringList;
  LDialogValidator    : TAbstractDataDialogValidator;
  LIdentifier         : integer;
begin
  Result := False;
  try
    if (Trim(ACommaTextContextData) = '') then Exit;

    LContextDataList := TStringList.Create;
    try
      LContextDataList.CommaText := ACommaTextContextData;
      LViewName := UpperCase(Trim(LContextDataList.Values['VIEWNAME']));
      if(LViewName = '') then Exit;

      LIdentifier   := StrToInt(LContextDataList.Values['MODELELEMENTID']);
      if(LIdentifier = NullInteger) then Exit;
    finally
      FreeAndNil(LContextDataList);
    end;
    if (LViewName = mdvnOutputComparisonFileSelection) then
    begin
      LDialogValidator := TOutputComparisonFileSelectionValidator.Create(APageControl,FAppModules);
      APageControl.AddValidator(LDialogValidator);
      LDialogValidator.Initialise;
      TOutputComparisonFileSelectionValidator(LDialogValidator).Identifier :=  LIdentifier;
      LDialogValidator.PopulateDataViewer;
      LDialogValidator.LanguageHasChanged;
    end
    else
    if (LViewName = mdvnOutputReservoirComparison) then
    begin
      LDialogValidator := TOutputComparisonReservoirValidator.Create(APageControl,FAppModules);
      APageControl.AddValidator(LDialogValidator);
      LDialogValidator.Initialise;
      TOutputComparisonReservoirValidator(LDialogValidator).Identifier :=  LIdentifier;
      LDialogValidator.PopulateDataViewer;
      LDialogValidator.LanguageHasChanged;
    end
    else
    if (LViewName = mdvnOutputChannelComparison) then
    begin
      LDialogValidator := TOutputComparisonChannelValidator.Create(APageControl,FAppModules);
      APageControl.AddValidator(LDialogValidator);
      LDialogValidator.Initialise;
      TOutputComparisonChannelValidator(LDialogValidator).Identifier :=  LIdentifier;
      LDialogValidator.PopulateDataViewer;
      LDialogValidator.LanguageHasChanged;
    end;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


function TYieldModelDataGUIManager.ViewOutputData(ACommaTextContextData: String; APageControl   : TAbstractDataPageControl): boolean;
const OPNAME = 'TYieldModelDataGUIManager.ViewOutputData';
var
  LViewName           : string;
  LContextDataList    : TStringList;
  LDialogValidator    : TAbstractDataDialogValidator;
  LIdentifier         : integer;
  LNetworkElementType : TNetworkElementType;
  lChannel            : IGeneralFlowChannel;
  LChannelArea        : IChannelArea;
  LReservoirAreaGroup : IReservoirAreaGroup;
begin
  Result := False;
  try

    if (Trim(ACommaTextContextData) = '') then Exit;

    LContextDataList := TStringList.Create;
    try
      LContextDataList.CommaText := ACommaTextContextData;
      LViewName := UpperCase(Trim(LContextDataList.Values['VIEWNAME']));
      if(LViewName = '') then Exit;

      LIdentifier   := StrToInt(LContextDataList.Values['MODELELEMENTID']);
      if(LIdentifier = NullInteger) then Exit;
    finally
      FreeAndNil(LContextDataList);
    end;

    if (LViewName = mdvnSystemYield) then
    begin
      //if (TYieldModelDataObject(FAppModules.Model.ModelData).RunConfigurationData.RunSequenceType = 'H') and
      //   (TYieldModelDataObject(FAppModules.Model.ModelData).RunConfigurationData.CalculateHistoricFirmYield = 0)then
      //begin
        // System yield (Historic)
        LDialogValidator := TSystemYieldHistoricValidator.Create(nil,FAppModules);
        APageControl.AddValidator(LDialogValidator);
        LDialogValidator.Initialise;
        LDialogValidator.PopulateDataViewer;
        LDialogValidator.LanguageHasChanged;

      //end;
      if (TYieldModelDataObject(FAppModules.Model.ModelData).RunConfigurationData.RunSequenceType = 'S') then
      begin
        // System yield (Stochastic)
        LDialogValidator := TSystemYieldStochasticValidator.Create(nil,FAppModules);
        APageControl.AddValidator(LDialogValidator);
        LDialogValidator.Initialise;
        LDialogValidator.PopulateDataViewer;
        LDialogValidator.LanguageHasChanged;
      end;
      if (TYieldModelDataObject(FAppModules.Model.ModelData).RunConfigurationData.RunSequenceType = 'H') and
         ((TYieldModelDataObject(FAppModules.Model.ModelData).RunConfigurationData.CalculateHistoricFirmYield = 1) or
         (TYieldModelDataObject(FAppModules.Model.ModelData).RunConfigurationData.CalculateHistoricFirmYield = 2))then
      begin
        // System yield (EstimatedStochastic)
        LDialogValidator := TEstimatedHistoricValidator.Create(nil,FAppModules);
        APageControl.AddValidator(LDialogValidator);
        LDialogValidator.Initialise;
        LDialogValidator.PopulateDataViewer;
        LDialogValidator.LanguageHasChanged;
      end;
      Result := True;
      Exit;
    end
    else
    if (LViewName = mdvnSumOutDataSource) then
    begin
      // SUM.out data source
      LDialogValidator := TOutputReviewDataSourceValidator.Create(nil,FAppModules);
      APageControl.AddValidator(LDialogValidator);
      LDialogValidator.Initialise;
      LDialogValidator.PopulateDataViewer;
      LDialogValidator.LanguageHasChanged;
      Result := True;
      Exit;
    end
    else
    if (LViewName = mdvnReviewDemandChannelsGridSummary) then
    begin

      LDialogValidator := TOutputDemandChannelsGridSummaryValidator.Create(nil,FAppModules);
      APageControl.AddValidator(LDialogValidator);
      LDialogValidator.Initialise;
      LDialogValidator.PopulateDataViewer;
      LDialogValidator.LanguageHasChanged;
      Result := True;
      Exit;

    end
    else
    if (LViewName = mdvnReviewDemandChannelsGraphSummary) then
    begin
      LDialogValidator := TOutputDemandChannelsGraphSummaryValidator.Create(nil,FAppModules);
      APageControl.AddValidator(LDialogValidator);
      LDialogValidator.Initialise;
      LDialogValidator.PopulateDataViewer;
      LDialogValidator.LanguageHasChanged;
      Result := True;
      Exit;

    end;

    LNetworkElementType := votNone;
    if(LViewName = mdvnMasterControlConfiguration) then LNetworkElementType := votMasterControl;
    if(LViewName = mdvnReservoir)                  then LNetworkElementType := votReservoir;
    if(LViewName = mdvnPCDDam)                     then LNetworkElementType := votReservoir;
    if(LViewName = mdvnUndegroundDam)              then LNetworkElementType := votReservoir;
    if(LViewName = mdvnNodesWithInflow)            then LNetworkElementType := votNodeWithInflow;
    if(LViewName = mdvnNodesWithoutInFlow)         then LNetworkElementType := votNodeWithoutInflow;
    if(LViewName = mdvnChannel)                    then LNetworkElementType := votChannel;
    if(LViewName = mdvnIrrigationArea)             then LNetworkElementType := votIrrigationArea;
    if(LViewName = mdvnPowerPlant)                 then LNetworkElementType := votPowerPlant;
    if(LViewName = mdvnWetland)                    then LNetworkElementType := votWetland;
    if(LViewName = mdvnChannelArea)                then LNetworkElementType := votChannelArea;
    if(LViewName = mdvnIrrigationBlock)            then LNetworkElementType := votIrrigationBlock;
    if(LViewName = mdvnReservoirAreaGroup)         then LNetworkElementType := votReservoirAreaGroup;

    if(LNetworkElementType = votNone) then Exit;

    if(LNetworkElementType in [votMasterControl,votChannel]) then
    begin
      lChannel := TYieldModelDataObject(FAppModules.Model.ModelData).CastNetworkElementData.
                    CastChannelList.CastChannelByChannelNumber[LIdentifier];
      if(lChannel <> nil) then
      begin
        // Create the grid
        LDialogValidator := TOutputGridValidator.Create(nil,FAppModules);
        APageControl.AddValidator(LDialogValidator);
        LDialogValidator.Initialise;
        TOutputGridValidator(LDialogValidator).NetworkElementType   :=  LNetworkElementType;
        TOutputGridValidator(LDialogValidator).Identifier :=  LIdentifier;
        LDialogValidator.PopulateDataViewer;
        LDialogValidator.LanguageHasChanged;




        if(lChannel.ChannelType in [ctMasterControlChannel,ctMinMaxChannel,ctDemandChannel,ctIrrigationBlockInflowChannel,ctIFRChannel]) then
        begin
          LDialogValidator := TChannelDemandsGridValidator.Create(nil,FAppModules);
          APageControl.AddValidator(LDialogValidator);
          LDialogValidator.Initialise;
          TChannelDemandsGridValidator(LDialogValidator).NetworkElementType :=  LNetworkElementType;
          TChannelDemandsGridValidator(LDialogValidator).Identifier         :=  LIdentifier;
          LDialogValidator.PopulateDataViewer;
          LDialogValidator.LanguageHasChanged;

          LDialogValidator := TOutputComplianceGridValidator.Create(nil,FAppModules);
          APageControl.AddValidator(LDialogValidator);
          LDialogValidator.Initialise;
          TOutputComplianceGridValidator(LDialogValidator).NetworkElementType :=  LNetworkElementType;
          TOutputComplianceGridValidator(LDialogValidator).Identifier :=  LIdentifier;
          LDialogValidator.PopulateDataViewer;
          LDialogValidator.LanguageHasChanged;
        end;

        // Create the graph
        LDialogValidator := TOutputGraphValidator.Create(nil,FAppModules);
        APageControl.AddValidator(LDialogValidator);
        LDialogValidator.Initialise;
        TOutputGraphValidator(LDialogValidator).NetworkElementType   :=  LNetworkElementType;
        TOutputGraphValidator(LDialogValidator).Identifier :=  LIdentifier;
        LDialogValidator.PopulateDataViewer;
        LDialogValidator.LanguageHasChanged;

        LDialogValidator := TOutputDistributionCurveValidator.Create(nil,FAppModules);
        APageControl.AddValidator(LDialogValidator);
        LDialogValidator.Initialise;
        TOutputDistributionCurveValidator(LDialogValidator).NetworkElementType   :=  votChannel;
        TOutputDistributionCurveValidator(LDialogValidator).Identifier :=  LIdentifier;
        LDialogValidator.PopulateDataViewer;
        LDialogValidator.LanguageHasChanged;

        LDialogValidator := TOutputBoxPlotGraphValidator.Create(APageControl,FAppModules);
        APageControl.AddValidator(LDialogValidator);
        LDialogValidator.Initialise;
        TOutputBoxPlotGraphValidator(LDialogValidator).NetworkElementType   :=  votChannel;
        TOutputBoxPlotGraphValidator(LDialogValidator).Identifier :=  LIdentifier;
        LDialogValidator.PopulateDataViewer;
        LDialogValidator.LanguageHasChanged;


        if(lChannel.ChannelType in [ctMasterControlChannel,ctMinMaxChannel,ctDemandChannel,ctIrrigationBlockInflowChannel,ctIFRChannel]) then
        begin
          LDialogValidator := TOutputDeficitDurationValidator.Create(nil,FAppModules);
          APageControl.AddValidator(LDialogValidator);
          LDialogValidator.Initialise;
          TOutputDeficitDurationValidator(LDialogValidator).NetworkElementType :=  LNetworkElementType;
          TOutputDeficitDurationValidator(LDialogValidator).Identifier         :=  LIdentifier;
          LDialogValidator.PopulateDataViewer;
          LDialogValidator.LanguageHasChanged;

          LDialogValidator := TOutputMonthlyDeficitValidator.Create(nil,FAppModules);
          APageControl.AddValidator(LDialogValidator);
          LDialogValidator.Initialise;
          TOutputMonthlyDeficitValidator(LDialogValidator).NetworkElementType :=  LNetworkElementType;
          TOutputMonthlyDeficitValidator(LDialogValidator).Identifier         :=  LIdentifier;
          LDialogValidator.PopulateDataViewer;
          LDialogValidator.LanguageHasChanged;

          LDialogValidator := TOutputComplianceGraphValidator.Create(nil,FAppModules);
          APageControl.AddValidator(LDialogValidator);
          LDialogValidator.Initialise;
          TOutputComplianceGraphValidator(LDialogValidator).NetworkElementType :=  LNetworkElementType;
          TOutputComplianceGraphValidator(LDialogValidator).Identifier :=  LIdentifier;
          LDialogValidator.PopulateDataViewer;
          LDialogValidator.LanguageHasChanged;

        end;

        if(lChannel.ChannelType in [ctMasterControlChannel,ctMinMaxChannel,ctDemandChannel,ctIrrigationBlockInflowChannel,ctIFRChannel,ctDemandCentreReturnFlowChannel]) or
          (LChannel.MinimumFlowConstraint <> nil) then
        begin
          LDialogValidator := TOutputLongtermSupplyValidator.Create(nil,FAppModules);
          APageControl.AddValidator(LDialogValidator);
          LDialogValidator.Initialise;
          TOutputLongtermSupplyValidator(LDialogValidator).NetworkElementType :=  LNetworkElementType;
          TOutputLongtermSupplyValidator(LDialogValidator).Identifier :=  LIdentifier;
          LDialogValidator.PopulateDataViewer;
          LDialogValidator.LanguageHasChanged;

          {LDialogValidator := TOutputVarLongtermSupplyValidator.Create(nil,FAppModules);
          APageControl.AddValidator(LDialogValidator);
          LDialogValidator.Initialise;
          TOutputVarLongtermSupplyValidator(LDialogValidator).NetworkElementType :=  LNetworkElementType;
          TOutputVarLongtermSupplyValidator(LDialogValidator).Identifier :=  LIdentifier;
          LDialogValidator.PopulateDataViewer;
          LDialogValidator.LanguageHasChanged;
          }

        end;

        {if (lChannel.ChannelType = 8) and (lChannel.IFRFeature <> nil) then
        begin
          LDialogValidator := TOutputComplianceGraphValidator.Create(nil,FAppModules);
          APageControl.AddValidator(LDialogValidator);
          LDialogValidator.Initialise;
          TOutputComplianceGraphValidator(LDialogValidator).NetworkElementType :=  LNetworkElementType;
          TOutputComplianceGraphValidator(LDialogValidator).Identifier :=  LIdentifier;
          LDialogValidator.PopulateDataViewer;
          LDialogValidator.LanguageHasChanged;
        end;

        LDialogValidator := TOutputWaterBalanceValidator.Create(nil,FAppModules);
        APageControl.AddValidator(LDialogValidator);
        LDialogValidator.Initialise;
        TOutputWaterBalanceValidator(LDialogValidator).NetworkElementType   :=  votChannel;
        TOutputWaterBalanceValidator(LDialogValidator).Identifier :=  LIdentifier;
        LDialogValidator.PopulateDataViewer;
        LDialogValidator.LanguageHasChanged;
        }
      end;
    end
    else
    begin
      // Create the grid
      LDialogValidator := TOutputGridValidator.Create(nil,FAppModules);
      APageControl.AddValidator(LDialogValidator);
      LDialogValidator.Initialise;
      TOutputGridValidator(LDialogValidator).NetworkElementType   :=  LNetworkElementType;
      TOutputGridValidator(LDialogValidator).Identifier :=  LIdentifier;
      LDialogValidator.PopulateDataViewer;
      LDialogValidator.LanguageHasChanged;


      // Create the graph
      LDialogValidator := TOutputGraphValidator.Create(nil,FAppModules);
      APageControl.AddValidator(LDialogValidator);
      LDialogValidator.Initialise;
      TOutputGraphValidator(LDialogValidator).NetworkElementType   :=  LNetworkElementType;
      TOutputGraphValidator(LDialogValidator).Identifier :=  LIdentifier;
      LDialogValidator.PopulateDataViewer;
      LDialogValidator.LanguageHasChanged;

      if(LViewName = mdvnReservoir)  or
        (LViewName = mdvnNodesWithInflow)  or
        (LViewName = mdvnPCDDam)  or
        (LViewName = mdvnUndegroundDam) then
      begin

        LDialogValidator := TOutputWaterBalanceValidator.Create(APageControl,FAppModules);
        APageControl.AddValidator(LDialogValidator);
        LDialogValidator.Initialise;
        TOutputWaterBalanceValidator(LDialogValidator).NetworkElementType   :=  votReservoir;
        TOutputWaterBalanceValidator(LDialogValidator).Identifier :=  LIdentifier;
        LDialogValidator.PopulateDataViewer;
        LDialogValidator.LanguageHasChanged;
      end;

      if(LViewName = mdvnReservoir)  or
        (LViewName = mdvnPCDDam)  or
        (LViewName = mdvnUndegroundDam) or
        (LViewName = mdvnReservoirAreaGroup)  then
      begin
        LDialogValidator := TOutputComplianceGraphValidator.Create(APageControl,FAppModules);
        APageControl.AddValidator(LDialogValidator);
        LDialogValidator.Initialise;
        TOutputComplianceGraphValidator(LDialogValidator).NetworkElementType   :=  LNetworkElementType;
        TOutputComplianceGraphValidator(LDialogValidator).Identifier :=  LIdentifier;
        LDialogValidator.PopulateDataViewer;
        LDialogValidator.LanguageHasChanged;

        LDialogValidator := TOutputBoxPlotGraphValidator.Create(APageControl,FAppModules);
        APageControl.AddValidator(LDialogValidator);
        LDialogValidator.Initialise;
        TOutputBoxPlotGraphValidator(LDialogValidator).NetworkElementType   :=  LNetworkElementType;
        TOutputBoxPlotGraphValidator(LDialogValidator).Identifier :=  LIdentifier;
        LDialogValidator.PopulateDataViewer;
        LDialogValidator.LanguageHasChanged;
      end;

      if (LViewName = mdvnWetland) then
      begin
        LDialogValidator := TOutputWaterBalanceValidator.Create(APageControl,FAppModules);
        APageControl.AddValidator(LDialogValidator);
        LDialogValidator.Initialise;
        TOutputWaterBalanceValidator(LDialogValidator).NetworkElementType   := votWetland;
        TOutputWaterBalanceValidator(LDialogValidator).Identifier :=  LIdentifier;
        LDialogValidator.PopulateDataViewer;
        LDialogValidator.LanguageHasChanged;

        LDialogValidator := TOutputComplianceGraphValidator.Create(APageControl,FAppModules);
        APageControl.AddValidator(LDialogValidator);
        LDialogValidator.Initialise;
        TOutputComplianceGraphValidator(LDialogValidator).NetworkElementType   :=  LNetworkElementType;
        TOutputComplianceGraphValidator(LDialogValidator).Identifier :=  LIdentifier;
        LDialogValidator.PopulateDataViewer;
        LDialogValidator.LanguageHasChanged;

        LDialogValidator := TOutputBoxPlotGraphValidator.Create(APageControl,FAppModules);
        APageControl.AddValidator(LDialogValidator);
        LDialogValidator.Initialise;
        TOutputBoxPlotGraphValidator(LDialogValidator).NetworkElementType   :=  LNetworkElementType;
        TOutputBoxPlotGraphValidator(LDialogValidator).Identifier :=  LIdentifier;
        LDialogValidator.PopulateDataViewer;
        LDialogValidator.LanguageHasChanged;
      end;

      if (LViewName = mdvnChannelArea) then
      begin
        LChannelArea := TYieldModelDataObject(FAppModules.Model.ModelData).CastNetworkFeaturesData.
                        CastChannelAreaList.ChannelAreaByID(LIdentifier);
        if LChannelArea <> nil then
        begin
          {LDialogValidator := TOutputWaterBalanceValidator.Create(APageControl,FAppModules);
          APageControl.AddValidator(LDialogValidator);
          LDialogValidator.Initialise;
          TOutputWaterBalanceValidator(LDialogValidator).NetworkElementType   := votChannelArea;
          TOutputWaterBalanceValidator(LDialogValidator).Identifier :=  LIdentifier;
          LDialogValidator.PopulateDataViewer;
          LDialogValidator.LanguageHasChanged;}

          LDialogValidator := TOutputDistributionCurveValidator.Create(nil,FAppModules);
          APageControl.AddValidator(LDialogValidator);
          LDialogValidator.Initialise;
          TOutputDistributionCurveValidator(LDialogValidator).NetworkElementType   :=  votChannelArea;
          TOutputDistributionCurveValidator(LDialogValidator).Identifier := LIdentifier;
          LDialogValidator.PopulateDataViewer;
          LDialogValidator.LanguageHasChanged;

          LDialogValidator := TOutputBoxPlotGraphValidator.Create(APageControl,FAppModules);
          APageControl.AddValidator(LDialogValidator);
          LDialogValidator.Initialise;
          TOutputBoxPlotGraphValidator(LDialogValidator).NetworkElementType   :=  LNetworkElementType;
          TOutputBoxPlotGraphValidator(LDialogValidator).Identifier :=  LIdentifier;
          LDialogValidator.PopulateDataViewer;
          LDialogValidator.LanguageHasChanged;



          {LDialogValidator := TOutputComplianceGridValidator.Create(nil,FAppModules);
          APageControl.AddValidator(LDialogValidator);
          LDialogValidator.Initialise;
          TOutputComplianceGridValidator(LDialogValidator).NetworkElementType :=  LNetworkElementType;
          TOutputComplianceGridValidator(LDialogValidator).Identifier :=  LIdentifier;
          LDialogValidator.PopulateDataViewer;
          LDialogValidator.LanguageHasChanged;}
        end;
      end;

      if (LViewName = mdvnIrrigationBlock) then
      begin

        LDialogValidator := TOutputWaterBalanceValidator.Create(APageControl,FAppModules);
        APageControl.AddValidator(LDialogValidator);
        LDialogValidator.Initialise;
        TOutputWaterBalanceValidator(LDialogValidator).NetworkElementType := votIrrigationBlock;
        TOutputWaterBalanceValidator(LDialogValidator).Identifier := LIdentifier;
        LDialogValidator.PopulateDataViewer;
        LDialogValidator.LanguageHasChanged;

        LDialogValidator := TOutputDistributionCurveValidator.Create(nil,FAppModules);
        APageControl.AddValidator(LDialogValidator);
        LDialogValidator.Initialise;
        TOutputDistributionCurveValidator(LDialogValidator).NetworkElementType :=  votIrrigationBlock;
        TOutputDistributionCurveValidator(LDialogValidator).Identifier := LIdentifier;
        LDialogValidator.PopulateDataViewer;
        LDialogValidator.LanguageHasChanged;

        LDialogValidator := TOutputBoxPlotGraphValidator.Create(APageControl,FAppModules);
        APageControl.AddValidator(LDialogValidator);
        LDialogValidator.Initialise;
        TOutputBoxPlotGraphValidator(LDialogValidator).NetworkElementType := LNetworkElementType;
        TOutputBoxPlotGraphValidator(LDialogValidator).Identifier := LIdentifier;
        LDialogValidator.PopulateDataViewer;
        LDialogValidator.LanguageHasChanged;
      end;

      if (LViewName = mdvnReservoirAreaGroup) then
      begin
        LReservoirAreaGroup := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.
                        ReservoirAreaGroupList.ReservoirAreaGroupByID(LIdentifier);
        if LReservoirAreaGroup <> nil then
        begin
          LDialogValidator := TOutputBoxPlotGraphValidator.Create(APageControl,FAppModules);
          APageControl.AddValidator(LDialogValidator);
          LDialogValidator.Initialise;
          TOutputBoxPlotGraphValidator(LDialogValidator).NetworkElementType   :=  LNetworkElementType;
          TOutputBoxPlotGraphValidator(LDialogValidator).Identifier :=  LIdentifier;
          LDialogValidator.PopulateDataViewer;
          LDialogValidator.LanguageHasChanged;
        end;
      end;



    end;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYieldModelDataGUIManager.ViewOutputData(ACommaTextContextData: String): boolean;
const OPNAME = 'TYieldModelDataGUIManager.ViewOutputData';
var
  LViewName           : string;
  LContextDataList    : TStringList;
  LDialogValidator    : TAbstractDataDialogValidator;
  LIdentifier         : integer;
  LNetworkElementType : TNetworkElementType;
  lChannel            : IGeneralFlowChannel;
  LChannelArea        : IChannelArea;
  LReservoirAreaGroup : IReservoirAreaGroup;
begin
  Result := False;
  try

    if (Trim(ACommaTextContextData) = '') then Exit;

    LContextDataList := TStringList.Create;
    try
      LContextDataList.CommaText := ACommaTextContextData;
      LViewName := UpperCase(Trim(LContextDataList.Values['VIEWNAME']));
      if(LViewName = '') then Exit;

      LIdentifier   := StrToInt(LContextDataList.Values['MODELELEMENTID']);
      if(LIdentifier = NullInteger) then Exit;
    finally
      FreeAndNil(LContextDataList);
    end;

    if (LViewName = mdvnSystemYield) then
    begin
      //if (TYieldModelDataObject(FAppModules.Model.ModelData).RunConfigurationData.RunSequenceType = 'H') and
      //   (TYieldModelDataObject(FAppModules.Model.ModelData).RunConfigurationData.CalculateHistoricFirmYield = 0)then
      //begin
        // System yield (Historic)
        LDialogValidator := TSystemYieldHistoricValidator.Create(nil,FAppModules);
        FPopupFrameForm.AddValidator(LDialogValidator);
      //APageControl.AddValidator(LDialogValidator);
        LDialogValidator.Initialise;
        LDialogValidator.PopulateDataViewer;
        LDialogValidator.LanguageHasChanged;

      //end;
      if (TYieldModelDataObject(FAppModules.Model.ModelData).RunConfigurationData.RunSequenceType = 'S') then
      begin
        // System yield (Stochastic)
        LDialogValidator := TSystemYieldStochasticValidator.Create(nil,FAppModules);
        FPopupFrameForm.AddValidator(LDialogValidator);
      //APageControl.AddValidator(LDialogValidator);
        LDialogValidator.Initialise;
        LDialogValidator.PopulateDataViewer;
        LDialogValidator.LanguageHasChanged;
      end;
      if (TYieldModelDataObject(FAppModules.Model.ModelData).RunConfigurationData.RunSequenceType = 'H') and
         ((TYieldModelDataObject(FAppModules.Model.ModelData).RunConfigurationData.CalculateHistoricFirmYield = 1) or
         (TYieldModelDataObject(FAppModules.Model.ModelData).RunConfigurationData.CalculateHistoricFirmYield = 2))then
      begin
        // System yield (EstimatedStochastic)
        LDialogValidator := TEstimatedHistoricValidator.Create(nil,FAppModules);
        FPopupFrameForm.AddValidator(LDialogValidator);
      //APageControl.AddValidator(LDialogValidator);
        LDialogValidator.Initialise;
        LDialogValidator.PopulateDataViewer;
        LDialogValidator.LanguageHasChanged;
      end;
      Result := True;
      Exit;
    end
    else
    if (LViewName = mdvnSumOutDataSource) then
    begin
      // SUM.out data source
      LDialogValidator := TOutputReviewDataSourceValidator.Create(nil,FAppModules);
      FPopupFrameForm.AddValidator(LDialogValidator);
      // APageControl.AddValidator(LDialogValidator);
      LDialogValidator.Initialise;
      LDialogValidator.PopulateDataViewer;
      LDialogValidator.LanguageHasChanged;
      Result := True;
      Exit;
    end
    else
    if (LViewName = mdvnReviewDemandChannelsGridSummary) then
    begin

      LDialogValidator := TOutputDemandChannelsGridSummaryValidator.Create(nil,FAppModules);
      FPopupFrameForm.AddValidator(LDialogValidator);
      // APageControl.AddValidator(LDialogValidator);
      LDialogValidator.Initialise;
      LDialogValidator.PopulateDataViewer;
      LDialogValidator.LanguageHasChanged;
      Result := True;
      Exit;

    end
    else
    if (LViewName = mdvnReviewDemandChannelsGraphSummary) then
    begin
      LDialogValidator := TOutputDemandChannelsGraphSummaryValidator.Create(nil,FAppModules);
      FPopupFrameForm.AddValidator(LDialogValidator);
      // APageControl.AddValidator(LDialogValidator);
      LDialogValidator.Initialise;
      LDialogValidator.PopulateDataViewer;
      LDialogValidator.LanguageHasChanged;
      Result := True;
      Exit;

    end;

    LNetworkElementType := votNone;
    if(LViewName = mdvnMasterControlConfiguration) then LNetworkElementType := votMasterControl;
    if(LViewName = mdvnReservoir)                  then LNetworkElementType := votReservoir;
    if(LViewName = mdvnPCDDam)                     then LNetworkElementType := votReservoir;
    if(LViewName = mdvnUndegroundDam)              then LNetworkElementType := votReservoir;
    if(LViewName = mdvnNodesWithInflow)            then LNetworkElementType := votNodeWithInflow;
    if(LViewName = mdvnNodesWithoutInFlow)         then LNetworkElementType := votNodeWithoutInflow;
    if(LViewName = mdvnChannel)                    then LNetworkElementType := votChannel;
    if(LViewName = mdvnIrrigationArea)             then LNetworkElementType := votIrrigationArea;
    if(LViewName = mdvnPowerPlant)                 then LNetworkElementType := votPowerPlant;
    if(LViewName = mdvnWetland)                    then LNetworkElementType := votWetland;
    if(LViewName = mdvnChannelArea)                then LNetworkElementType := votChannelArea;
    if(LViewName = mdvnIrrigationBlock)            then LNetworkElementType := votIrrigationBlock;
    if(LViewName = mdvnReservoirAreaGroup)         then LNetworkElementType := votReservoirAreaGroup;

    if(LNetworkElementType = votNone) then Exit;

    if(LNetworkElementType in [votMasterControl,votChannel]) then
    begin
      lChannel := TYieldModelDataObject(FAppModules.Model.ModelData).CastNetworkElementData.
                    CastChannelList.CastChannelByChannelNumber[LIdentifier];
      if(lChannel <> nil) then
      begin
        // Create the grid
        LDialogValidator := TOutputGridValidator.Create(nil,FAppModules);
        FPopupFrameForm.AddValidator(LDialogValidator);
      // APageControl.AddValidator(LDialogValidator);
        LDialogValidator.Initialise;
        TOutputGridValidator(LDialogValidator).NetworkElementType   :=  LNetworkElementType;
        TOutputGridValidator(LDialogValidator).Identifier :=  LIdentifier;
        LDialogValidator.PopulateDataViewer;
        LDialogValidator.LanguageHasChanged;




        if(lChannel.ChannelType in [ctMasterControlChannel,ctMinMaxChannel,ctDemandChannel,ctIrrigationBlockInflowChannel,ctIFRChannel]) then
        begin
          LDialogValidator := TChannelDemandsGridValidator.Create(nil,FAppModules);
          FPopupFrameForm.AddValidator(LDialogValidator);
      // APageControl.AddValidator(LDialogValidator);
          LDialogValidator.Initialise;
          TChannelDemandsGridValidator(LDialogValidator).NetworkElementType :=  LNetworkElementType;
          TChannelDemandsGridValidator(LDialogValidator).Identifier         :=  LIdentifier;
          LDialogValidator.PopulateDataViewer;
          LDialogValidator.LanguageHasChanged;

          LDialogValidator := TOutputComplianceGridValidator.Create(nil,FAppModules);
          FPopupFrameForm.AddValidator(LDialogValidator);
      // APageControl.AddValidator(LDialogValidator);
          LDialogValidator.Initialise;
          TOutputComplianceGridValidator(LDialogValidator).NetworkElementType :=  LNetworkElementType;
          TOutputComplianceGridValidator(LDialogValidator).Identifier :=  LIdentifier;
          LDialogValidator.PopulateDataViewer;
          LDialogValidator.LanguageHasChanged;
        end;

        // Create the graph
        LDialogValidator := TOutputGraphValidator.Create(nil,FAppModules);
        FPopupFrameForm.AddValidator(LDialogValidator);
      // APageControl.AddValidator(LDialogValidator);
        LDialogValidator.Initialise;
        TOutputGraphValidator(LDialogValidator).NetworkElementType   :=  LNetworkElementType;
        TOutputGraphValidator(LDialogValidator).Identifier :=  LIdentifier;
        LDialogValidator.PopulateDataViewer;
        LDialogValidator.LanguageHasChanged;

        LDialogValidator := TOutputDistributionCurveValidator.Create(nil,FAppModules);
        FPopupFrameForm.AddValidator(LDialogValidator);
      // APageControl.AddValidator(LDialogValidator);
        LDialogValidator.Initialise;
        TOutputDistributionCurveValidator(LDialogValidator).NetworkElementType   :=  votChannel;
        TOutputDistributionCurveValidator(LDialogValidator).Identifier :=  LIdentifier;
        LDialogValidator.PopulateDataViewer;
        LDialogValidator.LanguageHasChanged;

        LDialogValidator := TOutputBoxPlotGraphValidator.Create(nil,FAppModules);
        FPopupFrameForm.AddValidator(LDialogValidator);
      // APageControl.AddValidator(LDialogValidator);
        LDialogValidator.Initialise;
        TOutputBoxPlotGraphValidator(LDialogValidator).NetworkElementType   :=  votChannel;
        TOutputBoxPlotGraphValidator(LDialogValidator).Identifier :=  LIdentifier;
        LDialogValidator.PopulateDataViewer;
        LDialogValidator.LanguageHasChanged;


        if(lChannel.ChannelType in [ctMasterControlChannel,ctMinMaxChannel,ctDemandChannel,ctIrrigationBlockInflowChannel,ctIFRChannel]) then
        begin
          LDialogValidator := TOutputDeficitDurationValidator.Create(nil,FAppModules);
          FPopupFrameForm.AddValidator(LDialogValidator);
      //  APageControl.AddValidator(LDialogValidator);
          LDialogValidator.Initialise;
          TOutputDeficitDurationValidator(LDialogValidator).NetworkElementType :=  LNetworkElementType;
          TOutputDeficitDurationValidator(LDialogValidator).Identifier         :=  LIdentifier;
          LDialogValidator.PopulateDataViewer;
          LDialogValidator.LanguageHasChanged;

          LDialogValidator := TOutputMonthlyDeficitValidator.Create(nil,FAppModules);
          FPopupFrameForm.AddValidator(LDialogValidator);
      //  APageControl.AddValidator(LDialogValidator);
          LDialogValidator.Initialise;
          TOutputMonthlyDeficitValidator(LDialogValidator).NetworkElementType :=  LNetworkElementType;
          TOutputMonthlyDeficitValidator(LDialogValidator).Identifier         :=  LIdentifier;
          LDialogValidator.PopulateDataViewer;
          LDialogValidator.LanguageHasChanged;

          LDialogValidator := TOutputComplianceGraphValidator.Create(nil,FAppModules);
          FPopupFrameForm.AddValidator(LDialogValidator);
      // APageControl.AddValidator(LDialogValidator);
          LDialogValidator.Initialise;
          TOutputComplianceGraphValidator(LDialogValidator).NetworkElementType :=  LNetworkElementType;
          TOutputComplianceGraphValidator(LDialogValidator).Identifier :=  LIdentifier;
          LDialogValidator.PopulateDataViewer;
          LDialogValidator.LanguageHasChanged;

        end;

        if(lChannel.ChannelType in [ctMasterControlChannel,ctMinMaxChannel,ctDemandChannel,ctIrrigationBlockInflowChannel,ctIFRChannel,ctDemandCentreReturnFlowChannel]) or
          (LChannel.MinimumFlowConstraint <> nil) then
        begin
          LDialogValidator := TOutputLongtermSupplyValidator.Create(nil,FAppModules);
          FPopupFrameForm.AddValidator(LDialogValidator);
      // APageControl.AddValidator(LDialogValidator);
          LDialogValidator.Initialise;
          TOutputLongtermSupplyValidator(LDialogValidator).NetworkElementType :=  LNetworkElementType;
          TOutputLongtermSupplyValidator(LDialogValidator).Identifier :=  LIdentifier;
          LDialogValidator.PopulateDataViewer;
          LDialogValidator.LanguageHasChanged;

          {LDialogValidator := TOutputVarLongtermSupplyValidator.Create(nil,FAppModules);
          APageControl.AddValidator(LDialogValidator);
          LDialogValidator.Initialise;
          TOutputVarLongtermSupplyValidator(LDialogValidator).NetworkElementType :=  LNetworkElementType;
          TOutputVarLongtermSupplyValidator(LDialogValidator).Identifier :=  LIdentifier;
          LDialogValidator.PopulateDataViewer;
          LDialogValidator.LanguageHasChanged;
          }

        end;

        {if (lChannel.ChannelType = 8) and (lChannel.IFRFeature <> nil) then
        begin
          LDialogValidator := TOutputComplianceGraphValidator.Create(nil,FAppModules);
          APageControl.AddValidator(LDialogValidator);
          LDialogValidator.Initialise;
          TOutputComplianceGraphValidator(LDialogValidator).NetworkElementType :=  LNetworkElementType;
          TOutputComplianceGraphValidator(LDialogValidator).Identifier :=  LIdentifier;
          LDialogValidator.PopulateDataViewer;
          LDialogValidator.LanguageHasChanged;
        end;

        LDialogValidator := TOutputWaterBalanceValidator.Create(nil,FAppModules);
        APageControl.AddValidator(LDialogValidator);
        LDialogValidator.Initialise;
        TOutputWaterBalanceValidator(LDialogValidator).NetworkElementType   :=  votChannel;
        TOutputWaterBalanceValidator(LDialogValidator).Identifier :=  LIdentifier;
        LDialogValidator.PopulateDataViewer;
        LDialogValidator.LanguageHasChanged;
        }
      end;
    end
    else
    begin
      // Create the grid
      LDialogValidator := TOutputGridValidator.Create(nil,FAppModules);
      FPopupFrameForm.AddValidator(LDialogValidator);
      // APageControl.AddValidator(LDialogValidator);
      LDialogValidator.Initialise;
      TOutputGridValidator(LDialogValidator).NetworkElementType   :=  LNetworkElementType;
      TOutputGridValidator(LDialogValidator).Identifier :=  LIdentifier;
      LDialogValidator.PopulateDataViewer;
      LDialogValidator.LanguageHasChanged;


      // Create the graph
      LDialogValidator := TOutputGraphValidator.Create(nil,FAppModules);
      FPopupFrameForm.AddValidator(LDialogValidator);
      // APageControl.AddValidator(LDialogValidator);
      LDialogValidator.Initialise;
      TOutputGraphValidator(LDialogValidator).NetworkElementType   :=  LNetworkElementType;
      TOutputGraphValidator(LDialogValidator).Identifier :=  LIdentifier;
      LDialogValidator.PopulateDataViewer;
      LDialogValidator.LanguageHasChanged;

      if(LViewName = mdvnReservoir)  or
        (LViewName = mdvnNodesWithInflow)  or
        (LViewName = mdvnPCDDam)  or
        (LViewName = mdvnUndegroundDam) then
      begin

        LDialogValidator := TOutputWaterBalanceValidator.Create(nil,FAppModules);
        FPopupFrameForm.AddValidator(LDialogValidator);
      // APageControl.AddValidator(LDialogValidator);
        LDialogValidator.Initialise;
        TOutputWaterBalanceValidator(LDialogValidator).NetworkElementType   :=  votReservoir;
        TOutputWaterBalanceValidator(LDialogValidator).Identifier :=  LIdentifier;
        LDialogValidator.PopulateDataViewer;
        LDialogValidator.LanguageHasChanged;
      end;

      if(LViewName = mdvnReservoir)  or
        (LViewName = mdvnPCDDam)  or
        (LViewName = mdvnUndegroundDam) or
        (LViewName = mdvnReservoirAreaGroup)  then
      begin
        LDialogValidator := TOutputComplianceGraphValidator.Create(nil,FAppModules);
        FPopupFrameForm.AddValidator(LDialogValidator);
      // APageControl.AddValidator(LDialogValidator);
        LDialogValidator.Initialise;
        TOutputComplianceGraphValidator(LDialogValidator).NetworkElementType   :=  LNetworkElementType;
        TOutputComplianceGraphValidator(LDialogValidator).Identifier :=  LIdentifier;
        LDialogValidator.PopulateDataViewer;
        LDialogValidator.LanguageHasChanged;

        LDialogValidator := TOutputBoxPlotGraphValidator.Create(nil,FAppModules);
        FPopupFrameForm.AddValidator(LDialogValidator);
      // APageControl.AddValidator(LDialogValidator);
        LDialogValidator.Initialise;
        TOutputBoxPlotGraphValidator(LDialogValidator).NetworkElementType   :=  LNetworkElementType;
        TOutputBoxPlotGraphValidator(LDialogValidator).Identifier :=  LIdentifier;
        LDialogValidator.PopulateDataViewer;
        LDialogValidator.LanguageHasChanged;
      end;

      if (LViewName = mdvnWetland) then
      begin
        LDialogValidator := TOutputWaterBalanceValidator.Create(nil,FAppModules);
        FPopupFrameForm.AddValidator(LDialogValidator);
      // APageControl.AddValidator(LDialogValidator);
        LDialogValidator.Initialise;
        TOutputWaterBalanceValidator(LDialogValidator).NetworkElementType   := votWetland;
        TOutputWaterBalanceValidator(LDialogValidator).Identifier :=  LIdentifier;
        LDialogValidator.PopulateDataViewer;
        LDialogValidator.LanguageHasChanged;

        LDialogValidator := TOutputComplianceGraphValidator.Create(nil,FAppModules);
        FPopupFrameForm.AddValidator(LDialogValidator);
      // APageControl.AddValidator(LDialogValidator);
        LDialogValidator.Initialise;
        TOutputComplianceGraphValidator(LDialogValidator).NetworkElementType   :=  LNetworkElementType;
        TOutputComplianceGraphValidator(LDialogValidator).Identifier :=  LIdentifier;
        LDialogValidator.PopulateDataViewer;
        LDialogValidator.LanguageHasChanged;

        LDialogValidator := TOutputBoxPlotGraphValidator.Create(nil,FAppModules);
        FPopupFrameForm.AddValidator(LDialogValidator);
      // APageControl.AddValidator(LDialogValidator);
        LDialogValidator.Initialise;
        TOutputBoxPlotGraphValidator(LDialogValidator).NetworkElementType   :=  LNetworkElementType;
        TOutputBoxPlotGraphValidator(LDialogValidator).Identifier :=  LIdentifier;
        LDialogValidator.PopulateDataViewer;
        LDialogValidator.LanguageHasChanged;
      end;

      if (LViewName = mdvnChannelArea) then
      begin
        LChannelArea := TYieldModelDataObject(FAppModules.Model.ModelData).CastNetworkFeaturesData.
                        CastChannelAreaList.ChannelAreaByID(LIdentifier);
        if LChannelArea <> nil then
        begin
          {LDialogValidator := TOutputWaterBalanceValidator.Create(APageControl,FAppModules);
          APageControl.AddValidator(LDialogValidator);
          LDialogValidator.Initialise;
          TOutputWaterBalanceValidator(LDialogValidator).NetworkElementType   := votChannelArea;
          TOutputWaterBalanceValidator(LDialogValidator).Identifier :=  LIdentifier;
          LDialogValidator.PopulateDataViewer;
          LDialogValidator.LanguageHasChanged;}

          LDialogValidator := TOutputDistributionCurveValidator.Create(nil,FAppModules);
          FPopupFrameForm.AddValidator(LDialogValidator);
      // APageControl.AddValidator(LDialogValidator);
          LDialogValidator.Initialise;
          TOutputDistributionCurveValidator(LDialogValidator).NetworkElementType   :=  votChannelArea;
          TOutputDistributionCurveValidator(LDialogValidator).Identifier := LIdentifier;
          LDialogValidator.PopulateDataViewer;
          LDialogValidator.LanguageHasChanged;

          LDialogValidator := TOutputBoxPlotGraphValidator.Create(nil,FAppModules);
          FPopupFrameForm.AddValidator(LDialogValidator);
      // APageControl.AddValidator(LDialogValidator);
          LDialogValidator.Initialise;
          TOutputBoxPlotGraphValidator(LDialogValidator).NetworkElementType   :=  LNetworkElementType;
          TOutputBoxPlotGraphValidator(LDialogValidator).Identifier :=  LIdentifier;
          LDialogValidator.PopulateDataViewer;
          LDialogValidator.LanguageHasChanged;



          {LDialogValidator := TOutputComplianceGridValidator.Create(nil,FAppModules);
          APageControl.AddValidator(LDialogValidator);
          LDialogValidator.Initialise;
          TOutputComplianceGridValidator(LDialogValidator).NetworkElementType :=  LNetworkElementType;
          TOutputComplianceGridValidator(LDialogValidator).Identifier :=  LIdentifier;
          LDialogValidator.PopulateDataViewer;
          LDialogValidator.LanguageHasChanged;}
        end;
      end;

      if (LViewName = mdvnIrrigationBlock) then
      begin

        LDialogValidator := TOutputWaterBalanceValidator.Create(nil,FAppModules);
        FPopupFrameForm.AddValidator(LDialogValidator);
      //  APageControl.AddValidator(LDialogValidator);
        LDialogValidator.Initialise;
        TOutputWaterBalanceValidator(LDialogValidator).NetworkElementType := votIrrigationBlock;
        TOutputWaterBalanceValidator(LDialogValidator).Identifier := LIdentifier;
        LDialogValidator.PopulateDataViewer;
        LDialogValidator.LanguageHasChanged;

        LDialogValidator := TOutputDistributionCurveValidator.Create(nil,FAppModules);
        FPopupFrameForm.AddValidator(LDialogValidator);
      // APageControl.AddValidator(LDialogValidator);
        LDialogValidator.Initialise;
        TOutputDistributionCurveValidator(LDialogValidator).NetworkElementType :=  votIrrigationBlock;
        TOutputDistributionCurveValidator(LDialogValidator).Identifier := LIdentifier;
        LDialogValidator.PopulateDataViewer;
        LDialogValidator.LanguageHasChanged;

        LDialogValidator := TOutputBoxPlotGraphValidator.Create(nil,FAppModules);
        FPopupFrameForm.AddValidator(LDialogValidator);
      // APageControl.AddValidator(LDialogValidator);
        LDialogValidator.Initialise;
        TOutputBoxPlotGraphValidator(LDialogValidator).NetworkElementType := LNetworkElementType;
        TOutputBoxPlotGraphValidator(LDialogValidator).Identifier := LIdentifier;
        LDialogValidator.PopulateDataViewer;
        LDialogValidator.LanguageHasChanged;
      end;

      if (LViewName = mdvnReservoirAreaGroup) then
      begin
        LReservoirAreaGroup := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.
                        ReservoirAreaGroupList.ReservoirAreaGroupByID(LIdentifier);
        if LReservoirAreaGroup <> nil then
        begin
          LDialogValidator := TOutputBoxPlotGraphValidator.Create(nil,FAppModules);
          FPopupFrameForm.AddValidator(LDialogValidator);
      // APageControl.AddValidator(LDialogValidator);
          LDialogValidator.Initialise;
          TOutputBoxPlotGraphValidator(LDialogValidator).NetworkElementType   :=  LNetworkElementType;
          TOutputBoxPlotGraphValidator(LDialogValidator).Identifier :=  LIdentifier;
          LDialogValidator.PopulateDataViewer;
          LDialogValidator.LanguageHasChanged;
        end;
      end;



    end;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


function TYieldModelDataGUIManager.SaveLastSelectedCell(
  APageCtrl: TAbstractDataPageControl): boolean;
const OPNAME = 'TYieldModelDataGUIManager.SaveLastSelectedCell';
var i,j,k,l : integer;
    TmpGrid : TStringGrid;
    Found : Boolean;
begin
 Result := false;
 found := false;
try
 if ApageCtrl.PageCount > 0 then
  begin
   for i:=0 to APageCtrl.Pages[0].ControlCount -1 do
    begin
     for j:=0 to APageCtrl.Pages[0].Controls[i].ComponentCount -1 do
      begin
       for k:=0 to APageCtrl.Pages[0].Controls[i].Components[j].ComponentCount -1 do
        begin
          if APageCtrl.Pages[0].Controls[i].Components[j].Components[k].ClassType = TAbstractStringGrid then
           begin
            {Lets see if we have previous values to use}
            if not assigned(LastSelectedStringGrid) then
            LastSelectedStringGrid := LastSelectedStringGridStore.Create;

            TmpGrid := APageCtrl.Pages[0].Controls[i].Components[j].Components[k] as TAbstractStringGrid;
            {Search through LastSelectedStringGrid to see if previous entry exists to edit}
            for l:=0 to length(LastSelectedStringGrid.GridName)- 1 do
             begin
              if LastSelectedStringGrid.GridName[l] = TmpGrid.Name then
               begin
                {Previous entry found so change the recorded selection}
                LastSelectedStringGrid.GridSelection[l] := TmpGrid.Selection;
                LastSelectedStringGrid.TopRow := TmpGrid.TopRow;
                found := true;
                result := true;
               end;
             end;
            {No previous entry found so we have to create one}
            if not found then
             begin
              setlength(LastSelectedStringGrid.GridName,length(LastSelectedStringGrid.GridName)+1);
              setlength(LastSelectedStringGrid.GridSelection,length(LastSelectedStringGrid.GridSelection)+1);
              LastSelectedStringGrid.GridName[length(LastSelectedStringGrid.GridName)-1] := tmpGrid.Name;
              LastSelectedStringGrid.GridSelection[length(LastSelectedStringGrid.GridSelection)-1] := tmpGrid.selection;
              result := true;
             end;
            found := false;
           end;
        end;
      end;
    end;
  end;
except on E: Exception do HandleErrorFunction(E, OPNAME, Result); end;
end;

function TYieldModelDataGUIManager.LoadLastSelectedCell(
  APageCtrl: TAbstractDataPageControl): boolean;
const OPNAME = 'TYieldModelDataGUIManager.LoadLastSelectedCell';
var i,j,k,l : integer;
    TmpGrid : TAbstractStringGrid;
begin
Result := false;
try

 if ApageCtrl.PageCount > 0 then
  begin
   for i:=0 to APageCtrl.Pages[0].ControlCount -1 do
    begin
     for j:=0 to APageCtrl.Pages[0].Controls[i].ComponentCount -1 do
      begin
       for k:=0 to APageCtrl.Pages[0].Controls[i].Components[j].ComponentCount -1 do
        begin
          if APageCtrl.Pages[0].Controls[i].Components[j].Components[k].ClassType = TAbstractStringGrid then
           begin
            {Lets see if we have previous values to use}
            if not assigned(LastSelectedStringGrid) then exit;

            TmpGrid := TAbstractStringGrid(APageCtrl.Pages[0].Controls[i].Components[j].Components[k]);

            {Search through LastSelectedStringGrid to see if previous entry exists to load}
            for l:=0 to length(LastSelectedStringGrid.GridName)- 1 do
             begin
              if LastSelectedStringGrid.GridName[l] = TmpGrid.Name then
               begin
                {Previous entry found so load the Grid Selection}
                TmpGrid.Selection := LastSelectedStringGrid.GridSelection[l];
                TmpGrid.TopRow := LastSelectedStringGrid.TopRow;

                Result := true;
               end;
             end;
           end;
        end;
      end;
    end;
  end;
except on E: Exception do HandleErrorFunction(E, OPNAME, Result); end;
end;



end.

