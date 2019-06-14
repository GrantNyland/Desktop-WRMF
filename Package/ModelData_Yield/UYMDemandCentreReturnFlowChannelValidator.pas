{******************************************************************************}
{*  UNIT      : Contains the class TYMDemandCentreReturnFlowChannelValidator  *}
{*  AUTHOR    : Maurice Marinus                                               *}
{*  DATE      : 2006/06/27                                                    *}
{*  COPYRIGHT : Copyright © 2006 DWAF                                         *}
{******************************************************************************}

unit UYMDemandCentreReturnFlowChannelValidator;

interface

uses
  Classes,
  VCL.Controls,
  VCL.ComCtrls,
  VCL.StdCtrls,
  VCL.ExtCtrls,
  UAbstractObject,
  UAbstractComponent,
  UDataComponent,
  UDataEditComponent,
  VoaimsCom_TLB,
  UYMDemandCentreReturnFlowChannelDialog,
  UAbstractYieldDataDialogValidator,
  UYieldContextValidationType,
  UIrrigationBlock,
  UChannelPenaltyValidator,
  UChannelPenaltyDialog,
  VCL.Dialogs;

type
  TYMDemandCentreReturnFlowChannelValidator = class(TAbstractYieldDataDialogValidator)
  protected
    FSelectedReturnFlowID,
    FSelectedCol,
    FSelectedRow          : Integer;
    FChannelPenaltyValidator : TChannelPenaltyValidator;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;

    procedure OnEditControlEnter(Sender: TObject); override;
    procedure OnEditControltExit(Sender: TObject); override;
    procedure OnGridSelectCell(ASender: TObject;ACol, ARow: Longint; var CanSelect: Boolean);
    procedure OnSummaryOutputClick(Sender: TObject);
    procedure OnFirmYieldAnalysisClick(Sender: TObject);
    procedure OnSelectChannelClick(Sender: TObject);
    procedure OnSelectPenaltyClick(Sender: TObject);
    procedure OnInsertReturnFlow(Sender: TObject);
    procedure OnDeleteReturnFlow(Sender: TObject);

    procedure ShowChannelPenaltyDialog (ASender : TObject);

    procedure RePopulateNodes;
    procedure RePopulateDataViewer;
    procedure PopulateFields;

    procedure UpdateTotalReturnFlow;
    procedure UpdateFlowDiversion;
    procedure UpdateChannelPenaltyStructure; virtual;
    procedure UpdateChannelDownstreamNode;
    procedure UpdateChannelIncludeSummary;
    procedure UpdateChannelFirmYieldAnalysis;

    procedure ValidateTotalReturnFlow;
    procedure ValidateFlowDiversion;
    procedure ValidateChannelPenalty;
    procedure ValidateDownstreamNodes;
  public
    function Initialise: boolean; override;
    function SaveState: boolean; override;
    function LanguageHasChanged: boolean; override;
    function StudyHasChanged: boolean; override;
    function StudyDataHasChanged(AContext: TChangeContext; AFieldName,AOldValue,ANewValue: string): boolean; override;
    procedure ClearDataViewer; override;
    procedure DoContextValidation (AValidationType : TDialogValidationType); override;
    procedure PopulateDataViewer; override;
    function YMDemandCentreReturnFlowChannelDialog: TYMDemandCentreReturnFlowChannelDialog;
  end;

implementation

uses
  VCL.Grids,
  SysUtils,
  VCL.Graphics,
  UConstants,
  UUtilities,
  UYieldModelDataGUIForm,
  UYieldModelDataObject,
  UErrorHandlingOperations,
  USelectChannelDialog,
  Math,
  UNetworkFeaturesData;

{******************************************************************************}
{* TYMDemandCentreReturnFlowChannelValidator                                  *}
{******************************************************************************}

procedure TYMDemandCentreReturnFlowChannelValidator.CreateMemberObjects;
const OPNAME = 'TYMDemandCentreReturnFlowChannelValidator.CreateMemberObjects';
var
  lIndex : Integer;
begin
  try
    inherited CreateMemberObjects;
    FPanel := TYMDemandCentreReturnFlowChannelDialog.Create(FPanelOwner,FAppModules);

    with YMDemandCentreReturnFlowChannelDialog do
    begin
      for lIndex := 0 to ControlsParent.ComponentCount - 1 do
      begin
        if (ControlsParent.Components[lIndex].ClassNameIs('TFieldEdit')) then
        begin
          TFieldEdit(ControlsParent.Components[lIndex]).OnEnter  := OnEditControlEnter;
          TFieldEdit(ControlsParent.Components[lIndex]).OnExit   := OnEditControltExit;
        end;

        if (ControlsParent.Components[lIndex].ClassNameIs('TFieldStringGrid')) then
        begin
          TFieldStringGrid(ControlsParent.Components[lIndex]).OnSelectCell         := OnGridSelectCell;
          TFieldStringGrid(ControlsParent.Components[lIndex]).OnColEnter           := OnStringGridColEnter;
          TFieldStringGrid(ControlsParent.Components[lIndex]).OnExit               := OnEditControltExit;
          TFieldStringGrid(ControlsParent.Components[lIndex]).OnEnter              := OnEditControlEnter;
        end;
      end;

      FChannelPenaltyValidator := nil;
            
      DownStreamNodeCbx.FieldProperty         := FAppModules.FieldProperties.FieldProperty('DownNodeNumber');
      DownStreamNodeCbx.OnEnter               := OnEditControlEnter;
      DownStreamNodeCbx.OnExit                := OnEditControltExit;

      SummaryOutputChkBox.FieldProperty       := FAppModules.FieldProperties.FieldProperty('SummaryOutput');
      SummaryOutputChkBox.OnEnter             := OnEditControlEnter;
      SummaryOutputChkBox.OnClick             := OnSummaryOutputClick;

      FirmYieldAnalysisChkBox.FieldProperty   := FAppModules.FieldProperties.FieldProperty('FirmYieldCalc');
      FirmYieldAnalysisChkBox.OnEnter         := OnEditControlEnter;
      FirmYieldAnalysisChkBox.OnClick         := OnFirmYieldAnalysisClick;

      PenaltyStructureEdit.FieldProperty      := FAppModules.FieldProperties.FieldProperty('PenaltyNumber');
      PenaltyStructureEdit.ReadOnly           := TRUE;
      PenaltyStructureEdit.Color              := clWindow;
      PenaltyStructureEdit.OnEnter            := OnEditControlEnter;
      PenaltyStructureEdit.OnExit             := OnEditControltExit;

      SelectPenaltyStructureBtn.FieldProperty := FAppModules.FieldProperties.FieldProperty('PenaltyNumber');
      SelectPenaltyStructureBtn.OnEnter       := OnEditControlEnter;
      SelectPenaltyStructureBtn.OnExit        := OnEditControltExit;
      SelectPenaltyStructureBtn.OnClick       := OnSelectPenaltyClick;

      TotalReturnFlowEdit.FieldProperty       := FAppModules.FieldProperties.FieldProperty('TotalReturnFlow');
      FlowDiversionEdit.FieldProperty         := FAppModules.FieldProperties.FieldProperty('FlowDiversion');

      BtnInsertRow.OnClick                    := OnInsertReturnFlow;
      BtnDeleteRow.OnClick                    := OnDeleteReturnFlow;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYMDemandCentreReturnFlowChannelValidator.DestroyMemberObjects;
const OPNAME = 'TYMDemandCentreReturnFlowChannelValidator.DestroyMemberObjects';
begin
  try
    inherited DestroyMemberObjects;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYMDemandCentreReturnFlowChannelValidator.Initialise: boolean;
const OPNAME = 'TYMDemandCentreReturnFlowChannelValidator.Initialise';
begin
  Result := inherited Initialise;
  try
    with YMDemandCentreReturnFlowChannelDialog.ReturnFlowChannelsGrid do
    begin
      Cells[0,0] := FAppModules.Language.GetString('GridHeading.Number');
      Cells[1,0] := FAppModules.Language.GetString('GridHeading.ChannelNr');
      Cells[2,0] := FAppModules.Language.GetString('GridHeading.ChannelName');
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYMDemandCentreReturnFlowChannelValidator.LanguageHasChanged: boolean;
const OPNAME = 'TYMDemandCentreReturnFlowChannelValidator.LanguageHasChanged';
begin
  Result := False;
  try
    TabShetCaption := FAppModules.Language.GetString('ViewData.YMDemandCentreReturnFlowChannel');
    Result := inherited LanguageHasChanged;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYMDemandCentreReturnFlowChannelValidator.ClearDataViewer;
const OPNAME = 'TYMDemandCentreReturnFlowChannelValidator.ClearDataViewer';
var
  lIndex     : Integer;
  lComponent : TComponent;
  lFieldEdit : TFieldEdit;
  lFieldCbx  : TFieldComboBox;
  lChkBox    : TFieldChkBox;
  lRadioGrp  : TFieldRadioGroup;
  lDatePick  : TFieldDateTimePicker;
begin
  inherited ClearDataViewer;
  try
    with YMDemandCentreReturnFlowChannelDialog do
    begin
      for lIndex := 1 to 3 do
        ReturnFlowChannelsGrid.Cells[lIndex, 1] := '';
      for lIndex := 0 to ControlsParent.ComponentCount - 1 do
      begin
        lComponent := ControlsParent.Components[lIndex];
        if (lComponent.ClassNameIs('TFieldEdit')) then
        begin
          lFieldEdit := TFieldEdit(lComponent);
          if (lFieldEdit.FieldProperty <> nil) then
          begin
            case lFieldEdit.FieldProperty.FieldDataType of
            1 : lFieldEdit.SetFieldValue('');   //String
            2 : lFieldEdit.SetFieldValue('-1'); //Float
            3 : lFieldEdit.SetFieldValue('-1'); //Integer
            else
            end;
          end
        end
        else if (lComponent.ClassNameIs('TFieldChkBox')) then
        begin
          lChkBox := TFieldChkBox(lComponent);
          lChkBox.Checked := FALSE;
        end
        else if (lComponent.ClassNameIs('TFieldComboBox')) then
        begin
          lFieldCbx := TFieldComboBox(lComponent);
          lFieldCbx.ItemIndex := -1;
        end
        else if (lComponent.ClassNameIs('TFieldRadioGroup')) then
        begin
          lRadioGrp := TFieldRadioGroup(lComponent);
          lRadioGrp.ItemIndex := -1;
        end
        else if (lComponent.ClassNameIs('TFieldDateTimePicker')) then
        begin
          lDatePick := TFieldDateTimePicker(lComponent);
          lDatePick.Date := 0;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYMDemandCentreReturnFlowChannelValidator.PopulateDataViewer;
const OPNAME = 'TYMDemandCentreReturnFlowChannelValidator.PopulateDataViewer';
begin
  inherited PopulateDataViewer;
  try
    ClearDataViewer;
    RePopulateDataViewer;
    DoContextValidation(dvtYMDCReturnFlowFeatureAll);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYMDemandCentreReturnFlowChannelValidator.StudyDataHasChanged(AContext: TChangeContext; AFieldName,AOldValue,ANewValue: string): boolean;
const OPNAME = 'TYMDemandCentreReturnFlowChannelValidator.StudyDataHasChanged';
begin
  Result := inherited StudyDataHasChanged(AContext,AFieldName,AOldValue,ANewValue);
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYMDemandCentreReturnFlowChannelValidator.StudyHasChanged: boolean;
const OPNAME = 'TYMDemandCentreReturnFlowChannelValidator.StudyHasChanged';
begin
  Result := inherited StudyHasChanged;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYMDemandCentreReturnFlowChannelValidator.OnEditControlEnter(Sender: TObject);
const OPNAME = 'TYMDemandCentreReturnFlowChannelValidator.OnEditControlEnter';
begin
  inherited OnEditControlEnter(Sender);
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYMDemandCentreReturnFlowChannelValidator.OnEditControltExit(Sender: TObject);
const OPNAME = 'TYMDemandCentreReturnFlowChannelValidator.OnEditControltExit';
begin
  inherited OnEditControltExit(Sender);
  try
    with YMDemandCentreReturnFlowChannelDialog do
    begin
      if ((Sender = TotalReturnFlowEdit) AND (NOT TotalReturnFlowEdit.HasChanges) AND (TotalReturnFlowEdit.HasValueChanged)) then
        UpdateTotalReturnFlow
      else
      if ((Sender = FlowDiversionEdit) AND (NOT FlowDiversionEdit.HasChanges) AND (FlowDiversionEdit.HasValueChanged)) then
        UpdateFlowDiversion
      else
      if ((Sender = PenaltyStructureEdit) AND (PenaltyStructureEdit.HasValueChanged)) then
        UpdateChannelPenaltyStructure
      else
      if ((Sender = DownstreamNodeCbx) AND (DownstreamNodeCbx.HasValueChanged)) then
        UpdateChannelDownstreamNode;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYMDemandCentreReturnFlowChannelValidator.UpdateChannelIncludeSummary;
const OPNAME = 'TYMDemandCentreReturnFlowChannelValidator.UpdateChannelIncludeSummary';
var
  lChannel    : IGeneralFlowChannel;
  lOldInclude : string;
  lNewInclude : string;
  lMessage    : string;
  lYMDemandCentreReturnFlowFeature  : IYMDemandCentreReturnFlowFeature;
  lDemandCentre                     : IYMDemandCentre;
  lChannelList   : IChannelList;
begin
  try
    lDemandCentre := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.YMDemandCentreList.YMDemandCentreByID[FIdentifier];
    if lDemandCentre = nil then
      Exit;
    lYMDemandCentreReturnFlowFeature := lDemandCentre.ReturnFlowFeatureList.ReturnFlowFeatureByID[FSelectedReturnFlowID];

    lChannelList := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.ChannelList;
    lChannel     := lChannelList.ChannelByChannelNumber[lYMDemandCentreReturnFlowFeature.ChannelNr];
    if (lChannel <> nil) then
    begin
      with YMDemandCentreReturnFlowChannelDialog do
      begin
        lOldInclude := UpperCase(Trim(lChannel.SummaryOutputRequired));
        if SummaryOutputChkBox.Checked then
          lNewInclude := 'Y'
        else
          lNewInclude := 'N';
        if (lOldInclude <> lNewInclude) then
        begin
          if (FAppModules.FieldProperties.ValidateFieldProperty(
              SummaryOutputChkBox.FieldProperty.FieldName,
              lNewInclude,lMessage)) then
          begin
            lChannel.SummaryOutputRequired := lNewInclude;
            SummaryOutputChkBox.Checked := (lChannel.SummaryOutputRequired = 'Y');
            if not SummaryOutputChkBox.Checked and (lChannel.RequiresFirmYieldAnalysis = 'Y') then
               lChannel.RequiresFirmYieldAnalysis := 'N';
            PopulateFields;
          end
          else
            SummaryOutputChkBox.ValidationError := lMessage;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYMDemandCentreReturnFlowChannelValidator.RePopulateDataViewer;
const OPNAME = 'TYMDemandCentreReturnFlowChannelValidator.RePopulateDataViewer';
var
  lCnt          : Integer;
  lDemandCentre : IYMDemandCentre;
  LCanSelect    : Boolean;
begin
  try
    lDemandCentre := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.YMDemandCentreList.YMDemandCentreByID[FIdentifier];
    if lDemandCentre <> nil then
    begin
      with YMDemandCentreReturnFlowChannelDialog do
      begin
        UpstreamNodeEdit.Text           := lDemandCentre.Name;
        ReturnFlowChannelsGrid.RowCount := lDemandCentre.ReturnFlowFeatureList.ReturnFlowFeatureCount + 1;

        for lCnt := 0 to lDemandCentre.ReturnFlowFeatureList.ReturnFlowFeatureCount - 1 do
        begin
          ReturnFlowChannelsGrid.Rows[LCnt+1].Objects[0] := TObject(lDemandCentre.ReturnFlowFeatureList.ReturnFlowFeatureByIndex[lCnt].FeatureID);
          ReturnFlowChannelsGrid.Cells[0 ,LCnt+1] := IntToStr(LCnt + 1);
          ReturnFlowChannelsGrid.Cells[1 ,LCnt+1] := IntToStr(lDemandCentre.ReturnFlowFeatureList.ReturnFlowFeatureByIndex[lCnt].ChannelNr);
           ReturnFlowChannelsGrid.Cells[2 ,LCnt+1] := lDemandCentre.ReturnFlowFeatureList.ReturnFlowFeatureByIndex[lCnt].Channel.ChannelName;
        end;
        //channel stuff
        RePopulateNodes;
        LCanSelect := True;
        if lDemandCentre.ReturnFlowFeatureList.ReturnFlowFeatureCount > 0 then
          OnGridSelectCell(ReturnFlowChannelsGrid,1,1,LCanSelect);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYMDemandCentreReturnFlowChannelValidator.SaveState: boolean;
const OPNAME = 'TYMDemandCentreReturnFlowChannelValidator.SaveState';
begin
  Result := inherited SaveState;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYMDemandCentreReturnFlowChannelValidator.YMDemandCentreReturnFlowChannelDialog: TYMDemandCentreReturnFlowChannelDialog;
const OPNAME = 'TYMDemandCentreReturnFlowChannelValidator.YMDemandCentreReturnFlowChannelDialog';
begin
  Result := Nil;
  try
    if (FPanel <> nil) then
      Result := TYMDemandCentreReturnFlowChannelDialog(FPanel);
  except on E: Exception do HandleError(E, OPNAME) end;
end;


procedure TYMDemandCentreReturnFlowChannelValidator.DoContextValidation(AValidationType: TDialogValidationType);
const OPNAME = 'TYMDemandCentreReturnFlowChannelValidator.DoContextValidation';
var
//  LRow : Integer;
  lYMDemandCentreReturnFlowFeature : IYMDemandCentreReturnFlowFeature;
  lDemandCentre : IYMDemandCentre;
begin
  try
    lDemandCentre := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.YMDemandCentreList.YMDemandCentreByID[FIdentifier];
    if lDemandCentre = nil then
      Exit;
    lYMDemandCentreReturnFlowFeature := lDemandCentre.ReturnFlowFeatureList.ReturnFlowFeatureByID[FSelectedReturnFlowID];

    //LRow              := FSelectedRow;
    if (lDemandCentre <> nil) then
    begin
      case AValidationType of
        dvtYMDCReturnFlowFeatureAll
          : begin
              ValidateTotalReturnFlow;
              ValidateFlowDiversion;
              ValidateChannelPenalty;
              ValidateDownstreamNodes;
            end;
        dvtYMDCReturnFlowFeatureTotalReturnFlow : ValidateTotalReturnFlow;
        dvtYMDCReturnFlowFeatureFlowDiversion   : ValidateFlowDiversion;
        dvtChanPropPenalty                      : ValidateChannelPenalty;
        dvtChanPropDownstreamNode               : ValidateDownstreamNodes;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYMDemandCentreReturnFlowChannelValidator.OnGridSelectCell(ASender: TObject; ACol, ARow: Integer; var CanSelect: Boolean);
const OPNAME = 'TYMDemandCentreReturnFlowChannelValidator.OnGridSelectCell';
begin
  try
    FSelectedReturnFlowID    := -1;
    FSelectedCol          := -1;
    FSelectedRow          := -1;
{    if ACol = 1 then
      FLastCropNameSelected := CropWaterDialog.IrrigationBlockMonthlyWaterUsageGrid.Cells[ACol, ARow];}

    if ASender = YMDemandCentreReturnFlowChannelDialog.ReturnFlowChannelsGrid then
    begin
      FSelectedCol  := ACol;
      FSelectedRow  := ARow;
      if YMDemandCentreReturnFlowChannelDialog.ReturnFlowChannelsGrid.Rows[ARow].Objects[0] <> nil then
        FSelectedReturnFlowID := Integer(YMDemandCentreReturnFlowChannelDialog.ReturnFlowChannelsGrid.Rows[ARow].Objects[0]);
    end;

    CanSelect := True;
    YMDemandCentreReturnFlowChannelDialog.DownStreamNodeCbx.Clear;
    RePopulateNodes;
    PopulateFields;
    YMDemandCentreReturnFlowChannelDialog.ResetButtonState;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYMDemandCentreReturnFlowChannelValidator.RePopulateNodes;
const OPNAME = 'TYMDemandCentreReturnFlowChannelValidator.RePopulateNodes';
var
  lChannel       : IGeneralFlowChannel;
  lReservoirList : IReservoirDataList;
  lIndexA        : integer;
  lReservoir     : IReservoirConfigurationData;
  lYMDemandCentreReturnFlowFeature  : IYMDemandCentreReturnFlowFeature;
  lDemandCentre                     : IYMDemandCentre;
begin
  try
    YMDemandCentreReturnFlowChannelDialog.DownStreamNodeCbx.Items.Clear;
      
    lDemandCentre := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.YMDemandCentreList.YMDemandCentreByID[FIdentifier];
    if lDemandCentre = nil then
      Exit;

    lYMDemandCentreReturnFlowFeature := lDemandCentre.ReturnFlowFeatureList.ReturnFlowFeatureByID[FSelectedReturnFlowID];
    if lYMDemandCentreReturnFlowFeature = nil then
      Exit;

    lChannel  := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.
                    ChannelList.ChannelByChannelNumber[lYMDemandCentreReturnFlowFeature.ChannelNr];

    if (FIdentifier >= 0) then
    begin
      if (lChannel <> nil) then
      begin
        with YMDemandCentreReturnFlowChannelDialog do
        begin
          lReservoirList := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.ReservoirList;
          if (lReservoirList <> nil) then
          begin
            for lIndexA := 0 to lReservoirList.ReservoirAndNodesCount - 1 do
            begin
              lReservoir := lReservoirList.ReservoirOrNodeByIndex[lIndexA].
                              ReservoirConfigurationData;
              if (lReservoir.ReservoirIdentifier = 0) then
              begin
                if (lChannel.ChannelType <> 10) then // SpecifiedInflow
                  DownStreamNodeCbx.Items.AddObject('(0) ' + UpperCase(lChannel.SinkName),
                   TObject(lReservoir.ReservoirIdentifier));
              end
              else
              begin
                case lReservoir.NodeType of
                  ntReservoir,
                  ntNodeWithInflow :
                    DownStreamNodeCbx.Items.AddObject
                      ('(' + IntToStr(lReservoir.ReservoirIdentifier) + ') ' + lReservoir.ReservoirName,
                       TObject(lReservoir.ReservoirIdentifier));
                  ntNodeWithoutInflow :
                    if (lChannel.ChannelType <> 10) then
                      DownStreamNodeCbx.Items.AddObject
                        ('(' + IntToStr(lReservoir.ReservoirIdentifier) + ') ' + lReservoir.ReservoirName,
                         TObject(lReservoir.ReservoirIdentifier));
                  ntIrrigationNode :
                    if ((lChannel.ChannelType = 4) AND (lChannel.ChannelSubType = 1)) then
                      DownStreamNodeCbx.Items.AddObject
                        ('(' + IntToStr(lReservoir.ReservoirIdentifier) + ') ' + lReservoir.ReservoirName,
                         TObject(lReservoir.ReservoirIdentifier));
                  ntWetlandNode :
                    if ((lChannel.ChannelType = 4) AND (lChannel.ChannelSubType = 1)) then
                      DownStreamNodeCbx.Items.AddObject
                        ('(' + IntToStr(lReservoir.ReservoirIdentifier) + ') ' + lReservoir.ReservoirName,
                         TObject(lReservoir.ReservoirIdentifier));
                else
                end;   
              end;
            end;
          end;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYMDemandCentreReturnFlowChannelValidator.UpdateFlowDiversion;
const OPNAME = 'TYMDemandCentreReturnFlowChannelValidator.UpdateFlowDiversion';
var
  lMessage        : string;
  lYMDemandCentreReturnFlowFeature : IYMDemandCentreReturnFlowFeature;
  lDemandCentre : IYMDemandCentre;
begin
  try
    lDemandCentre := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.YMDemandCentreList.YMDemandCentreByID[FIdentifier];
    if lDemandCentre = nil then
      Exit;
    lYMDemandCentreReturnFlowFeature := lDemandCentre.ReturnFlowFeatureList.ReturnFlowFeatureByID[FSelectedReturnFlowID];

    with YMDemandCentreReturnFlowChannelDialog do
    begin
      UpstreamNodeEdit.Text           := lDemandCentre.Name;
      ReturnFlowChannelsGrid.RowCount := lDemandCentre.ReturnFlowFeatureList.ReturnFlowFeatureCount + 1;
    end;
    
    if (lYMDemandCentreReturnFlowFeature <> nil) then
    begin
      with YMDemandCentreReturnFlowChannelDialog do
      begin
        if (FAppModules.FieldProperties.ValidateFieldProperty(FlowDiversionEdit.FieldProperty.FieldName,
          TotalReturnFlowEdit.Text,lMessage)) then
        begin
          FlowDiversionEdit.FieldValidationError := lMessage;
          lYMDemandCentreReturnFlowFeature.FlowDiversion := StrToFloat(FlowDiversionEdit.Text);
          FlowDiversionEdit.SetFieldValue(lYMDemandCentreReturnFlowFeature.FlowDiversion);
          DoContextValidation(dvtYMDCReturnFlowFeatureFlowDiversion);
        end
        else
          FlowDiversionEdit.FieldValidationError := lMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;

end;

procedure TYMDemandCentreReturnFlowChannelValidator.UpdateTotalReturnFlow;
const OPNAME = 'TYMDemandCentreReturnFlowChannelValidator.UpdateTotalReturnFlow';
var
  lMessage        : string;
  lYMDemandCentreReturnFlowFeature : IYMDemandCentreReturnFlowFeature;
  lDemandCentre : IYMDemandCentre;
begin
  try
    lDemandCentre := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.YMDemandCentreList.YMDemandCentreByID[FIdentifier];
    if lDemandCentre = nil then
      Exit;
    lYMDemandCentreReturnFlowFeature := lDemandCentre.ReturnFlowFeatureList.ReturnFlowFeatureByID[FSelectedReturnFlowID];
    if (lYMDemandCentreReturnFlowFeature <> nil) then
    begin
      with YMDemandCentreReturnFlowChannelDialog do
      begin
        if (FAppModules.FieldProperties.ValidateFieldProperty(TotalReturnFlowEdit.FieldProperty.FieldName,
          TotalReturnFlowEdit.Text,lMessage)) then
        begin
          TotalReturnFlowEdit.FieldValidationError := lMessage;
          lYMDemandCentreReturnFlowFeature.TotalReturnFlow := StrToFloat(TotalReturnFlowEdit.Text);
          TotalReturnFlowEdit.SetFieldValue(lYMDemandCentreReturnFlowFeature.TotalReturnFlow);
          DoContextValidation(dvtYMDCReturnFlowFeatureTotalReturnFlow);
        end
        else
          TotalReturnFlowEdit.FieldValidationError := lMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYMDemandCentreReturnFlowChannelValidator.ValidateFlowDiversion;
const OPNAME = 'TYMDemandCentreReturnFlowChannelValidator.ValidateFlowDiversion';
var
  lYMDemandCentreReturnFlowFeature : IYMDemandCentreReturnFlowFeature;
  lDemandCentre : IYMDemandCentre;
begin
  try
    lDemandCentre := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.YMDemandCentreList.YMDemandCentreByID[FIdentifier];
    if lDemandCentre = nil then
      Exit;
    lYMDemandCentreReturnFlowFeature := lDemandCentre.ReturnFlowFeatureList.ReturnFlowFeatureByID[FSelectedReturnFlowID];

    if lYMDemandCentreReturnFlowFeature <> nil then
    begin
      with YMDemandCentreReturnFlowChannelDialog do
      begin
        FErrorMessage := '';
        if (NOT lYMDemandCentreReturnFlowFeature.Validate(FErrorMessage, 'FlowDiversion')) then
          FAllErrorMessages.Add(Trim(FErrorMessage));
        FlowDiversionEdit.ContextValidationError := FErrorMessage;
      end;
    end;  
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYMDemandCentreReturnFlowChannelValidator.ValidateTotalReturnFlow;
const OPNAME = 'TYMDemandCentreReturnFlowChannelValidator.ValidateTotalReturnFlow';
var
  lYMDemandCentreReturnFlowFeature : IYMDemandCentreReturnFlowFeature;
  lDemandCentre : IYMDemandCentre;
begin
  try
    lDemandCentre := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.YMDemandCentreList.YMDemandCentreByID[FIdentifier];
    if lDemandCentre = nil then
      Exit;
    lYMDemandCentreReturnFlowFeature := lDemandCentre.ReturnFlowFeatureList.ReturnFlowFeatureByID[FSelectedReturnFlowID];

    if lYMDemandCentreReturnFlowFeature <> nil then
    begin
      with YMDemandCentreReturnFlowChannelDialog do
      begin
        FErrorMessage := '';
        if (not lYMDemandCentreReturnFlowFeature.Validate(FErrorMessage, 'TotalReturnFlow')) then
          FAllErrorMessages.Add(Trim(FErrorMessage));
        TotalReturnFlowEdit.ContextValidationError := FErrorMessage;
      end;
    end;  
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYMDemandCentreReturnFlowChannelValidator.PopulateFields;
const OPNAME = 'TYMDemandCentreReturnFlowChannelValidator.PopulateFields';
var
  lYMDemandCentreReturnFlowFeature  : IYMDemandCentreReturnFlowFeature;
  lDemandCentre                     : IYMDemandCentre;
  lReservoirData : IReservoirData;
  lReservoir     : IReservoirConfigurationData;
  LPenalty       : IChannelPenalty;
begin
  try
    lDemandCentre := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.YMDemandCentreList.YMDemandCentreByID[FIdentifier];
    if lDemandCentre = nil then
      Exit;
    lYMDemandCentreReturnFlowFeature := lDemandCentre.ReturnFlowFeatureList.ReturnFlowFeatureByID[FSelectedReturnFlowID];
    if (lYMDemandCentreReturnFlowFeature <> nil) then
    begin
      with YMDemandCentreReturnFlowChannelDialog do
      begin
        if lYMDemandCentreReturnFlowFeature.Channel <> nil then
        begin
          lReservoirData := TYieldModelDataObject(FAppModules.Model.ModelData).CastNetworkElementData.
                            ReservoirList.ReservoirOrNodeByIdentifier[lYMDemandCentreReturnFlowFeature.Channel.DownStreamNodeNumber];
          if (lReservoirData <> nil) then
          begin
            lReservoir := lReservoirData.ReservoirConfigurationData;
            DownstreamNodeCbx.SetFieldIndex(DownstreamNodeCbx.Items.IndexOfObject(TObject(lReservoir.ReservoirIdentifier)));
          end;
        end;

        lPenalty := lYMDemandCentreReturnFlowFeature.Channel.ChannelPenalty;
        if (lPenalty <> nil) then
          PenaltyStructureEdit.SetFieldValue(lPenalty.ChannelPenaltyID)
        else
          PenaltyStructureEdit.SetFieldValue(0);
        SummaryOutputChkBox.Checked := (lYMDemandCentreReturnFlowFeature.Channel.SummaryOutputRequired = 'Y');
        TotalReturnFlowEdit.SetFieldValue(lYMDemandCentreReturnFlowFeature.TotalReturnFlow);
        FlowDiversionEdit.SetFieldValue(lYMDemandCentreReturnFlowFeature.FlowDiversion);

        FirmYieldAnalysisChkBox.Checked := (lYMDemandCentreReturnFlowFeature.Channel.RequiresFirmYieldAnalysis = 'Y');
        FirmYieldAnalysisChkBox.Enabled := (lYMDemandCentreReturnFlowFeature.Channel.SummaryOutputRequired = 'Y');
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYMDemandCentreReturnFlowChannelValidator.OnSelectChannelClick(Sender: TObject);
const OPNAME = 'TYMDemandCentreReturnFlowChannelValidator.OnSelectChannelClick';
begin
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYMDemandCentreReturnFlowChannelValidator.OnSummaryOutputClick(Sender: TObject);
const OPNAME = 'TYMDemandCentreReturnFlowChannelValidator.OnSummaryOutputClick';
begin
  try
    if(YMDemandCentreReturnFlowChannelDialog.SummaryOutputChkBox.HasValueChanged) then
      UpdateChannelIncludeSummary;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYMDemandCentreReturnFlowChannelValidator.OnSelectPenaltyClick(Sender: TObject);
const OPNAME = 'TYMDemandCentreReturnFlowChannelValidator.OnSelectPenaltyClick';
var
  LForm            : TYieldModelDataGUIForm;
  LDialogValidator : TChannelPenaltyValidator;
  LSelectedPenalty : integer;
  lChannel         : IGeneralFlowChannel;
  lChannelList     : IChannelList;
  lYMDemandCentreReturnFlowFeature  : IYMDemandCentreReturnFlowFeature;
  lDemandCentre                     : IYMDemandCentre;
begin
  try
    lDemandCentre := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.YMDemandCentreList.YMDemandCentreByID[FIdentifier];
    if lDemandCentre = nil then
      Exit;
    lYMDemandCentreReturnFlowFeature := lDemandCentre.ReturnFlowFeatureList.ReturnFlowFeatureByID[FSelectedReturnFlowID];

    lChannelList := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.ChannelList;
    lChannel     := lChannelList.ChannelByChannelNumber[lYMDemandCentreReturnFlowFeature.ChannelNr];
    if (lChannel <> nil) then
    begin
      LForm   := TYieldModelDataGUIForm.CreateWithoutDFM(nil,FAppModules);
      try
        LForm.Initialise;
        LForm.LanguageHasChanged;

        LDialogValidator          := TChannelPenaltyValidator.Create(LForm,FAppModules);
        FChannelPenaltyValidator  := LDialogValidator;
        try
          LForm.AddModelDataPanel(LDialogValidator.Panel);
          LDialogValidator.Initialise;
          LDialogValidator.ViewMode       := vmSelect;
          LDialogValidator.ChannelNumber  := lChannel.ChannelNumber;
          LDialogValidator.ArcCountSet    := [2];

          LDialogValidator.PopulateDataViewer;
          LDialogValidator.LanguageHasChanged;
          lForm.OnShow := ShowChannelPenaltyDialog;

          LForm.ShowModal;
          if (LForm.ModalResult = mrOk) and (TChannelPenaltyDialog(LDialogValidator.Panel).ActivePanel <> nil)then
          begin
            LSelectedPenalty := StrToInt(TChannelPenaltyDialog(LDialogValidator.Panel).ActivePanel.IDLabel.Caption);
            if(YMDemandCentreReturnFlowChannelDialog.PenaltyStructureEdit.Text <> IntToStr(LSelectedPenalty)) then
            begin
              YMDemandCentreReturnFlowChannelDialog.PenaltyStructureEdit.Text := IntToStr(LSelectedPenalty);
              YMDemandCentreReturnFlowChannelDialog.PenaltyStructureEdit.OnExit(YMDemandCentreReturnFlowChannelDialog.PenaltyStructureEdit);
            end;
          end;
        finally
          FChannelPenaltyValidator := nil;
          LDialogValidator.Free;
        end;
      finally
        LForm.Free;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYMDemandCentreReturnFlowChannelValidator.ShowChannelPenaltyDialog(ASender: TObject);
const OPNAME = 'TYMDemandCentreReturnFlowChannelValidator.ShowChannelPenaltyDialog';
begin
  try
    if (FChannelPenaltyValidator <> nil) then
      FChannelPenaltyValidator.ChannelPenaltyDialog.Resize;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYMDemandCentreReturnFlowChannelValidator.UpdateChannelDownstreamNode;
const OPNAME = 'TYMDemandCentreReturnFlowChannelValidator.UpdateChannelDownstreamNode';
var
  lReservoir     : IReservoirData;
  lReservoirNr   : integer;
  lChannelList   : IChannelList;
  lChannel       : IGeneralFlowChannel;
  lMessage       : string;
  lReservoirList : IReservoirDataList;
  lYMDemandCentreReturnFlowFeature  : IYMDemandCentreReturnFlowFeature;
  lDemandCentre                     : IYMDemandCentre;
begin
  try
    lDemandCentre := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.YMDemandCentreList.YMDemandCentreByID[FIdentifier];
    if lDemandCentre = nil then
      Exit;
    lYMDemandCentreReturnFlowFeature := lDemandCentre.ReturnFlowFeatureList.ReturnFlowFeatureByID[FSelectedReturnFlowID];

    lChannelList := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.ChannelList;
    lChannel     := lChannelList.ChannelByChannelNumber[lYMDemandCentreReturnFlowFeature.ChannelNr];
    if (lChannel <> nil) then
    begin
      with YMDemandCentreReturnFlowChannelDialog do
      begin
        lReservoirList  := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.ReservoirList;
        lReservoir      := nil;
        lReservoirNr    := -1;
        if (DownstreamNodeCbx.ItemIndex >= 0) then
        begin
          lReservoirNr  := Integer(DownstreamNodeCbx.Items.Objects[DownstreamNodeCbx.ItemIndex]);
          lReservoir    := lReservoirList.ReservoirOrNodeByIdentifier[lReservoirNr];
        end;
        if (FAppModules.FieldProperties.ValidateFieldProperty(
            DownstreamNodeCbx.FieldProperty.FieldName,
            IntToStr(lReservoirNr), lMessage)) then
        begin
          lChannel.DownstreamNodeNumber := lReservoirNr;
          lReservoir                    := lChannel.DownStreamNode;
          DownstreamNodeCbx.SetFieldIndex
            (DownstreamNodeCbx.Items.IndexOfObject
              (TObject(lReservoir.ReservoirConfigurationData.ReservoirIdentifier)));
          DoContextValidation(dvtChanPropDownstreamNode);
        end
        else
          DownstreamNodeCbx.ValidationError := lMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYMDemandCentreReturnFlowChannelValidator.UpdateChannelPenaltyStructure;
const OPNAME = 'TYMDemandCentreReturnFlowChannelValidator.UpdateChannelPenaltyStructure';
var
  lPenaltyTypeList  : IChannelPenaltyList;
  lPenaltyStructure : IChannelPenalty;
  lPenaltyID        : integer;
  lChannel          : IGeneralFlowChannel;
  lMessage          : string;
  lYMDemandCentreReturnFlowFeature  : IYMDemandCentreReturnFlowFeature;
  lDemandCentre                     : IYMDemandCentre;
  lChannelList   : IChannelList;
begin
  try
    lDemandCentre := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.YMDemandCentreList.YMDemandCentreByID[FIdentifier];
    if lDemandCentre = nil then
      Exit;
    lYMDemandCentreReturnFlowFeature := lDemandCentre.ReturnFlowFeatureList.ReturnFlowFeatureByID[FSelectedReturnFlowID];
    if lYMDemandCentreReturnFlowFeature = nil then
      Exit;

    lChannelList := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.ChannelList;
    lChannel     := lChannelList.ChannelByChannelNumber[lYMDemandCentreReturnFlowFeature.ChannelNr];
    if (lChannel <> nil) then
    begin
      with YMDemandCentreReturnFlowChannelDialog do
      begin
        if (FAppModules.FieldProperties.ValidateFieldProperty(
            PenaltyStructureEdit.FieldProperty.FieldName,
            PenaltyStructureEdit.Text,lMessage)) then
        begin
          PenaltyStructureEdit.FieldValidationError := lMessage;
          lPenaltyID       := StrToInt(Trim(PenaltyStructureEdit.Text));
          lPenaltyTypeList := TYieldModelDataObject(FAppModules.Model.ModelData).
                                NetworkElementData.ChannelPenaltyList;
          if (lPenaltyTypeList <> nil) then
          begin
            lPenaltyStructure := lPenaltyTypeList.ChannelPenaltyByIdentifier[lPenaltyID];
            lChannel.ChannelPenalty := lPenaltyStructure;
            PenaltyStructureEdit.SetFieldValue(IntToStr(lChannel.ChannelPenalty.ChannelPenaltyID));
            DoContextValidation(dvtChanPropPenalty);
          end;
        end
        else
          PenaltyStructureEdit.FieldValidationError := lMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYMDemandCentreReturnFlowChannelValidator.ValidateChannelPenalty;
const OPNAME = 'TYMDemandCentreReturnFlowChannelValidator.ValidateChannelPenalty';
var
  lYMDemandCentreReturnFlowFeature : IYMDemandCentreReturnFlowFeature;
  lDemandCentre : IYMDemandCentre;
begin
  try
    lDemandCentre := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.YMDemandCentreList.YMDemandCentreByID[FIdentifier];
    if lDemandCentre = nil then
      Exit;
    lYMDemandCentreReturnFlowFeature := lDemandCentre.ReturnFlowFeatureList.ReturnFlowFeatureByID[FSelectedReturnFlowID];

    if lYMDemandCentreReturnFlowFeature <> nil then
    begin
      with YMDemandCentreReturnFlowChannelDialog do
      begin
        FErrorMessage := '';
        if lYMDemandCentreReturnFlowFeature.Channel <> nil then
        begin
          if (not lYMDemandCentreReturnFlowFeature.Channel.Validate(FErrorMessage, 'ChannelPenalty')) then
            FAllErrorMessages.Add(Trim(FErrorMessage));
          PenaltyStructureEdit.ContextValidationError := FErrorMessage;
        end;  
      end;
    end;  
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYMDemandCentreReturnFlowChannelValidator.ValidateDownstreamNodes;
const OPNAME = 'TYMDemandCentreReturnFlowChannelValidator.ValidateDownstreamNodes';
var
  lYMDemandCentreReturnFlowFeature  : IYMDemandCentreReturnFlowFeature;
  lDemandCentre                     : IYMDemandCentre;
  lChannelList                      : IChannelList;
  lChannel                          : IGeneralFlowChannel;
begin
  try
    lDemandCentre := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.YMDemandCentreList.YMDemandCentreByID[FIdentifier];
    if lDemandCentre = nil then
      Exit;
    lYMDemandCentreReturnFlowFeature := lDemandCentre.ReturnFlowFeatureList.ReturnFlowFeatureByID[FSelectedReturnFlowID];

    if lYMDemandCentreReturnFlowFeature <> nil then
    begin
      lChannelList := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.ChannelList;
      lChannel     := lChannelList.ChannelByChannelNumber[lYMDemandCentreReturnFlowFeature.ChannelNr];

      with YMDemandCentreReturnFlowChannelDialog do
      begin
        FErrorMessage := '';
        if LChannel <> nil then
        begin
          if (LChannel.Validate(FErrorMessage, 'UpDownNotSameNode')) then
          begin
            FErrorMessage := '';
            if (lChannel.Validate(FErrorMessage, 'DownNodeNumber')) then
            begin
              if (lChannel.DownStreamNodeNumber <> 0) then
              begin
                DownstreamNodeCbx.InValidationError := FALSE;
                DownstreamNodeCbx.ShowErrorState(FALSE);
              end
              else
              begin
                if (lChannelList.Validate(FErrorMessage, 'MaximumZeroDownstreamNode')) then
                begin
                  DownstreamNodeCbx.InValidationError := FALSE;
                  DownstreamNodeCbx.ShowErrorState(FALSE);
                end
                else
                begin
                  DownstreamNodeCbx.InValidationError := TRUE;
                  DownstreamNodeCbx.ValidationError := FErrorMessage;
                  DownstreamNodeCbx.ShowErrorState(TRUE);
                  FAllErrorMessages.Add(Trim(FErrorMessage));
                end;
              end
            end
            else
            begin
              DownstreamNodeCbx.InValidationError := TRUE;
              DownstreamNodeCbx.ValidationError := FErrorMessage;
              DownstreamNodeCbx.ShowErrorState(TRUE);
              FAllErrorMessages.Add(Trim(FErrorMessage));
            end;
          end
          else
          begin
            DownstreamNodeCbx.InValidationError := TRUE;
            DownstreamNodeCbx.ValidationError   := FErrorMessage;
            DownstreamNodeCbx.ShowErrorState(TRUE);
            FAllErrorMessages.Add(Trim(FErrorMessage));
          end;
        end;
      end;
    end;  
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYMDemandCentreReturnFlowChannelValidator.OnDeleteReturnFlow(Sender: TObject);
const OPNAME = 'TYMDemandCentreReturnFlowChannelValidator.OnDeleteReturnFlow';
var
  lDemandCentre : IYMDemandCentre;
  lChannelNr    : Integer;
  lReturnFlowChannel : IYMDemandCentreReturnFlowFeature;
begin
  try
    lDemandCentre := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.YMDemandCentreList.YMDemandCentreByID[FIdentifier];
    if lDemandCentre = nil then
      Exit;

    lReturnFlowChannel := lDemandCentre.ReturnFlowFeatureList.ReturnFlowFeatureByID[FSelectedReturnFlowID];
    if lReturnFlowChannel <> nil then
      lChannelNr  := lReturnFlowChannel.Channel.ChannelNumber
    else
      lChannelNr  := 0;

    lDemandCentre.ReturnFlowFeatureList.RemoveReturnFlowFeatureWithNr(lDemandCentre.Identifier, lChannelNr);

    //FSelectedReturnFlowID := -1;
    RePopulateDataViewer;
    DoContextValidation(dvtYMDCReturnFlowFeatureAll);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYMDemandCentreReturnFlowChannelValidator.OnInsertReturnFlow(Sender: TObject);
const OPNAME = 'TYMDemandCentreReturnFlowChannelValidator.OnInsertReturnFlow';
var
  lYMDemandCentreReturnFlowFeature  : IYMDemandCentreReturnFlowFeature;
  lDemandCentre                     : IYMDemandCentre;
begin
  try
    lDemandCentre := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.YMDemandCentreList.YMDemandCentreByID[FIdentifier];
    if lDemandCentre = nil then
      Exit;

    lYMDemandCentreReturnFlowFeature := lDemandCentre.
                        ReturnFlowFeatureList.CreateReturnFlowFeature(lDemandCentre.Identifier);
    RePopulateDataViewer;
    DoContextValidation(dvtYMDCReturnFlowFeatureAll);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYMDemandCentreReturnFlowChannelValidator.OnFirmYieldAnalysisClick(Sender: TObject);
const OPNAME = 'TYMDemandCentreReturnFlowChannelValidator.OnFirmYieldAnalysisClick';
begin
  try
    if(YMDemandCentreReturnFlowChannelDialog.FirmYieldAnalysisChkBox.HasValueChanged) then
      UpdateChannelFirmYieldAnalysis;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYMDemandCentreReturnFlowChannelValidator.UpdateChannelFirmYieldAnalysis;
const OPNAME = 'TYMDemandCentreReturnFlowChannelValidator.UpdateChannelFirmYieldAnalysis';
var
  lChannel    : IGeneralFlowChannel;
  lOldInclude : string;
  lNewInclude : string;
  lMessage    : string;
  lYMDemandCentreReturnFlowFeature  : IYMDemandCentreReturnFlowFeature;
  lDemandCentre                     : IYMDemandCentre;
  lChannelList   : IChannelList;
begin
  try
    lDemandCentre := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.YMDemandCentreList.YMDemandCentreByID[FIdentifier];
    if lDemandCentre = nil then
      Exit;
    lYMDemandCentreReturnFlowFeature := lDemandCentre.ReturnFlowFeatureList.ReturnFlowFeatureByID[FSelectedReturnFlowID];

    lChannelList := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.ChannelList;
    lChannel     := lChannelList.ChannelByChannelNumber[lYMDemandCentreReturnFlowFeature.ChannelNr];
    if (lChannel <> nil) then
    begin
      with YMDemandCentreReturnFlowChannelDialog do
      begin
        lOldInclude := UpperCase(Trim(lChannel.RequiresFirmYieldAnalysis));
        if FirmYieldAnalysisChkBox.Checked then
          lNewInclude := 'Y'
        else
          lNewInclude := 'N';
        if (lOldInclude <> lNewInclude) then
        begin
          if (FAppModules.FieldProperties.ValidateFieldProperty(
              FirmYieldAnalysisChkBox.FieldProperty.FieldName,
              lNewInclude,lMessage)) then
          begin
            lChannel.RequiresFirmYieldAnalysis := lNewInclude;
            FirmYieldAnalysisChkBox.Checked := (lChannel.RequiresFirmYieldAnalysis = 'Y');
          end
          else
            FirmYieldAnalysisChkBox.ValidationError := lMessage;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
