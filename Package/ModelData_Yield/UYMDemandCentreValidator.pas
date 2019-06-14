{******************************************************************************}
{*  UNIT      : Contains the class TYMDemandCentreValidator                   *}
{*  AUTHOR    : Maurice Marinus                                               *}
{*  DATE      : 2006/11/10                                                    *}
{*  COPYRIGHT : Copyright © 2006 DWAF                                         *}
{******************************************************************************}

unit UYMDemandCentreValidator;

interface

uses
  Classes,
  VCL.Controls,
  VCL.ComCtrls,
  VCL.StdCtrls,
  VCL.ExtCtrls,
  VCLTee.Series,
  VCLTee.Chart,
  VCLTee.TeEngine,
  UAbstractObject,
  UAbstractComponent,
  UDataComponent,
  UDataEditComponent,
  VoaimsCom_TLB,
  UAbstractYieldDataDialogValidator,
  UYieldContextValidationType,
  UYMDemandCentreDialog,
  VCL.Dialogs,
  UFileNames;

type
  TYMDemandCentreValidator = class(TAbstractYieldDataDialogValidator)
  protected
    FBusyPopulating : Boolean;
    procedure CreateMemberObjects;                  override;
    procedure DestroyMemberObjects;                 override;
    procedure OnEditControltExit(Sender: TObject);  override;
    procedure ReclaimationChkBoxOnClick(Sender : TObject);
    procedure RePopulateDataViewer;
    procedure RePopulateNodes;

    procedure UpdateDemandCentreName;
    procedure UpdateDescription;
    procedure UpdateNodeNumber;
    procedure UpdateAveReturnFlowFactor;
    procedure UpdateAveMonthlyEvaporation;
    procedure UpdateRoutingConstant;
    procedure UpdateRainScalingFactor;
    procedure UpdateTotalFlowLost;
    procedure UpdateStdDeviationFactor;
    procedure UpdateNodeNumberRefCbx;
    procedure UpdateConsumptiveChannel;
    procedure UpdateXCoord;
    procedure UpdateYCoord;

    procedure ValidateName;
    procedure ValidateDescription;
    procedure ValidateNodeNumber;
    procedure ValidateAveReturnFlowFactor;
    procedure ValidateAveMonthlyEvaporation;
    procedure ValidateRoutingConstant;
    procedure ValidateRainScalingFactor;
    procedure ValidateTotalFlowLost;
    procedure ValidateStdDeviationFactor;
    procedure ValidateNodeNumberRefCbx;
    procedure ValidateConsumptiveChannel;
    procedure ValidateXCoord(AReservoirData: IReservoirData);
    procedure ValidateYCoord(AReservoirData: IReservoirData);
  public
    function Initialise:          Boolean; override;
    function LanguageHasChanged:  Boolean; override;
    function StudyHasChanged:     Boolean; override;
    function YMDemandCentreDialog : TYMDemandCentreDialog;

    procedure ClearDataViewer;    override;
    procedure DoContextValidation (AValidationType : TDialogValidationType); override;
    procedure PopulateDataViewer; override;
  end;

implementation

uses
  VCL.Grids,
  VCLTee.TeeProcs,
  VCL.Graphics,
  windows,
  SysUtils,
  UConstants,
  UUtilities,
  UYieldModelDataGUIForm,
  UYieldModelDataObject,
  UErrorHandlingOperations,
  USelectChannelDialog,
  Math,
  Contnrs,
  UParameterData,
  UNetworkFeaturesData,
  UAbstractFileNamesObject;

{******************************************************************************}
{* TYMDemandCentreValidator                                                   *}
{******************************************************************************}

procedure TYMDemandCentreValidator.CreateMemberObjects;
const OPNAME = 'TYMDemandCentreValidator.CreateMemberObjects';
begin
  inherited CreateMemberObjects;
  try
    FBusyPopulating := False;
    FPanel := TYMDemandCentreDialog.Create(FPanelOwner,FAppModules);

    with TYMDemandCentreDialog(FPanel) do
    begin
      DemandCentreNameEdit.OnEnter                   := OnEditControlEnter;
      DemandCentreNameEdit.OnExit                    := OnEditControltExit;
      DemandCentreNameEdit.FieldProperty             := FAppModules.FieldProperties.FieldProperty('YMDemandCentreName');

      DescriptionEdit.OnEnter            := OnEditControlEnter;
      DescriptionEdit.OnExit             := OnEditControltExit;
      DescriptionEdit.FieldProperty      := FAppModules.FieldProperties.FieldProperty('YMDemandCentreDescription');

      NodeNumberEdit.OnEnter            := OnEditControlEnter;
      NodeNumberEdit.OnExit             := OnEditControltExit;
      NodeNumberEdit.FieldProperty      := FAppModules.FieldProperties.FieldProperty('YMDemandCentreNodeNumber');

      AveReturnFlowFactorEdit.OnEnter            := OnEditControlEnter;
      AveReturnFlowFactorEdit.OnExit             := OnEditControltExit;
      AveReturnFlowFactorEdit.FieldProperty      := FAppModules.FieldProperties.FieldProperty('AveReturnFlowFactor');

      AveMonthlyEvaporationEdit.OnEnter          := OnEditControlEnter;
      AveMonthlyEvaporationEdit.OnExit           := OnEditControltExit;
      AveMonthlyEvaporationEdit.FieldProperty    := FAppModules.FieldProperties.FieldProperty('AveEvaporation');

      RoutingConstantEdit.OnEnter                := OnEditControlEnter;
      RoutingConstantEdit.OnExit                 := OnEditControltExit;
      RoutingConstantEdit.FieldProperty          := FAppModules.FieldProperties.FieldProperty('YMDemandCentreRoutingConstant');

      RainScalingFactorEdit.OnEnter              := OnEditControlEnter;
      RainScalingFactorEdit.OnExit               := OnEditControltExit;
      RainScalingFactorEdit.FieldProperty        := FAppModules.FieldProperties.FieldProperty('RainfallScalingFactor');

      TotalFlowLostEdit.OnEnter                  := OnEditControlEnter;
      TotalFlowLostEdit.OnExit                   := OnEditControltExit;
      TotalFlowLostEdit.FieldProperty            := FAppModules.FieldProperties.FieldProperty('TotalFlowLost');

      StdDeviationFactorEdit.OnEnter             := OnEditControlEnter;
      StdDeviationFactorEdit.OnExit              := OnEditControltExit;
      StdDeviationFactorEdit.FieldProperty       := FAppModules.FieldProperties.FieldProperty('StdDeviationFactor');

      NodeNumberRefCbx.OnEnter                  := OnEditControlEnter;
      NodeNumberRefCbx.OnExit                   := OnEditControltExit;
      NodeNumberRefCbx.FieldProperty            := FAppModules.FieldProperties.FieldProperty('NodeRefNr');

      DemandCentreXCoordEdit.FieldProperty := FAppModules.FieldProperties.FieldProperty('XCoord');
      DemandCentreXCoordEdit.OnEnter       := OnEditControlEnter;
      DemandCentreXCoordEdit.OnExit        := OnEditControltExit;

      DemandCentreYCoordEdit.FieldProperty := FAppModules.FieldProperties.FieldProperty('YCoord');
      DemandCentreYCoordEdit.OnEnter       := OnEditControlEnter;
      DemandCentreYCoordEdit.OnExit        := OnEditControltExit;


      ConsumptiveChannelCbx.OnEnter             := OnEditControlEnter;
      ConsumptiveChannelCbx.OnExit              := OnEditControltExit;
      ConsumptiveChannelCbx.FieldProperty       := FAppModules.FieldProperties.FieldProperty('DemandCentreConsumptiveChannelNumber');

      {ReclaimationChannel.OnEnter            := OnEditControlEnter;
      ReclaimationChannel.OnExit             := OnEditControltExit;}
      ReclaimationChannelCbx.FieldProperty      := FAppModules.FieldProperties.FieldProperty('DemandCentreReclaimationChannelNumber');
      ReclaimationChannelCbx.OnClick            := ReclaimationChkBoxOnClick;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYMDemandCentreValidator.DestroyMemberObjects;
const OPNAME = 'TYMDemandCentreValidator.DestroyMemberObjects';
begin
  try
    inherited DestroyMemberObjects;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYMDemandCentreValidator.Initialise: Boolean;
const OPNAME = 'TYMDemandCentreValidator.Initialise';
begin
  Result := inherited Initialise;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYMDemandCentreValidator.LanguageHasChanged: Boolean;
const OPNAME = 'TYMDemandCentreValidator.LanguageHasChanged';
begin
  Result := False;
  try
    TabShetCaption := FAppModules.Language.GetString('ViewData.YMDemandCentre');
    Result  := inherited LanguageHasChanged;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYMDemandCentreValidator.ClearDataViewer;
const OPNAME = 'TYMDemandCentreValidator.ClearDataViewer';
var
  lIndex     : integer;
  lComponent : TComponent;
  lFieldEdit : TFieldEdit;
  lFieldCbx  : TFieldComboBox;
  lChkBox    : TFieldChkBox;
  lRadioGrp  : TFieldRadioGroup;
  lDatePick  : TFieldDateTimePicker;
begin
  inherited ClearDataViewer;
  try
    with YMDemandCentreDialog do
    begin
      for lIndex := 0 to ControlsParent.ComponentCount - 1 do
      begin
        lComponent := ControlsParent.Components[lIndex];
        if (lComponent.ClassNameIs('TFieldEdit')) then
        begin
          lFieldEdit := TFieldEdit(lComponent);
          if (lFieldEdit.FieldProperty <> nil) then
          begin
            case lFieldEdit.FieldProperty.FieldDataType of
            1 : lFieldEdit.SetFieldValue(''); //String
            2 : lFieldEdit.SetFieldValue('-1'); //Float
            3 : lFieldEdit.SetFieldValue('-1'); //Integer
            else
            end;
          end
        end
        else if (lComponent.ClassNameIs('TFieldChkBox')) then
        begin
          lChkBox := TFieldChkBox(lComponent);
          lChkBox.Checked := False;
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

procedure TYMDemandCentreValidator.DoContextValidation(AValidationType: TDialogValidationType);
const OPNAME = 'TYMDemandCentreValidator.DoContextValidation';
var
  LYMDemandCentre  : IYMDemandCentre; 
  LReservoir       : IReservoirData;
begin
  try
    LYMDemandCentre := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.YMDemandCentreList.YMDemandCentreByID[FIdentifier];
    if (LYMDemandCentre <> nil) then
    begin
      case AValidationType of
        dvtYMDemandCentreAll:
          begin
            ValidateName;
            ValidateDescription;
            ValidateNodeNumber;
            ValidateAveReturnFlowFactor;
            ValidateAveMonthlyEvaporation;
            ValidateRoutingConstant;
            ValidateRainScalingFactor;
            ValidateTotalFlowLost;
            ValidateStdDeviationFactor;
            ValidateNodeNumberRefCbx;
            ValidateConsumptiveChannel;
          end;
        dvtYMDemandCentreName                 :  ValidateName;
        dvtYMDemandCentreDescription          :  ValidateDescription;
        dvtYMDemandCentreNodeNumber           :  ValidateNodeNumber;
        dvtYMDemandCentreAveReturnFlowFactor  :  ValidateAveReturnFlowFactor;
        dvtYMDemandCentreAveMonthlyEvaporation:  ValidateAveMonthlyEvaporation;
        dvtYMDemandCentreRoutingConstant      :  ValidateRoutingConstant;
        dvtYMDemandCentreRainScalingFactor    :  ValidateRainScalingFactor;
        dvtYMDemandCentreTotalFlowLost        :  ValidateTotalFlowLost;
        dvtYMDemandCentreStdDeviationFactor   :  ValidateStdDeviationFactor;
        dvtYMDemandCentreNodeNumberRef        :  ValidateNodeNumberRefCbx;
        dvtYMDemandCentreConsumptive          :  ValidateConsumptiveChannel;
      end;
      LReservoir  := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData
                    .ReservoirList.ReservoirOrNodeByIdentifier[LYMDemandCentre.NodeNumber];
      if (LReservoir <> nil) then
      begin
        if (AValidationType = dvtYMDemandCentreAll) then
        begin
          ValidateXCoord(LReservoir);
          ValidateYCoord(LReservoir);
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYMDemandCentreValidator.OnEditControltExit(Sender: TObject);
const OPNAME = 'TYMDemandCentreValidator.OnEditControltExit';
begin
  inherited OnEditControltExit(Sender);
  try
    with YMDemandCentreDialog do
    begin
      if((Sender = DemandCentreNameEdit) and (DemandCentreNameEdit.HasValueChanged ))then
        UpdateDemandCentreName;
      if((Sender = DescriptionEdit) and (DescriptionEdit.HasValueChanged ))then
        UpdateDescription;
      if((Sender = NodeNumberEdit) and (NodeNumberEdit.HasValueChanged ))then
        UpdateNodeNumber;
      if((Sender = AveReturnFlowFactorEdit) and (AveReturnFlowFactorEdit.HasValueChanged ))then
        UpdateAveReturnFlowFactor;
      if((Sender = AveMonthlyEvaporationEdit) and (AveMonthlyEvaporationEdit.HasValueChanged ))then
        UpdateAveMonthlyEvaporation;
      if((Sender = RoutingConstantEdit) and (RoutingConstantEdit.HasValueChanged ))then
        UpdateRoutingConstant;
      if((Sender = RainScalingFactorEdit) and (RainScalingFactorEdit.HasValueChanged ))then
        UpdateRainScalingFactor;
      if((Sender = TotalFlowLostEdit) and (TotalFlowLostEdit.HasValueChanged ))then
        UpdateTotalFlowLost;
      if((Sender = StdDeviationFactorEdit) and (StdDeviationFactorEdit.HasValueChanged ))then
        UpdateStdDeviationFactor;
      if((Sender = NodeNumberRefCbx) and (NodeNumberRefCbx.HasValueChanged ))then
        UpdateNodeNumberRefCbx;
      if((Sender = ConsumptiveChannelCbx) and (ConsumptiveChannelCbx.HasValueChanged ))then
        UpdateConsumptiveChannel;
      if ((sender = DemandCentreXCoordEdit) AND (DemandCentreXCoordEdit.HasValueChanged)) then
       UpdateXCoord;
      if ((sender = DemandCentreYCoordEdit) AND (DemandCentreYCoordEdit.HasValueChanged)) then
       UpdateYCoord;

    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYMDemandCentreValidator.PopulateDataViewer;
const OPNAME = 'TYMDemandCentreValidator.PopulateDataViewer';
begin
  inherited PopulateDataViewer;
  try
    ClearDataViewer;
    RePopulateDataViewer;
    DoContextValidation(dvtYMDemandCentreAll);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYMDemandCentreValidator.ReclaimationChkBoxOnClick(Sender: TObject);
const OPNAME = 'TYMDemandCentreValidator.ReclaimationChkBoxOnClick';
var
  lYMDemandCentre   : IYMDemandCentre;
begin
  try
    if not FBusyPopulating then
    begin
      lYMDemandCentre := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.YMDemandCentreList.YMDemandCentreByID[FIdentifier];
      if (lYMDemandCentre <> nil) then
        lYMDemandCentre.ReclaimationPlantExists := YMDemandCentreDialog.ReclaimationChannelCbx.Checked;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYMDemandCentreValidator.RePopulateDataViewer;
const OPNAME = 'TYMDemandCentreValidator.RePopulateDataViewer';
var
  lYMDemandCentre   : IYMDemandCentre;
  lIndexA           : integer;
  lNodesList        : IReservoirDataList;
  lFound            : boolean;
  lGeneralChannel   : IGeneralFlowChannel;
  LReservoirData    : IReservoirData;
begin
  try
    lYMDemandCentre := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.YMDemandCentreList.YMDemandCentreByID[FIdentifier];
    if (lYMDemandCentre <> nil) then
    begin
      FBusyPopulating := True;
      try
        with YMDemandCentreDialog do
        begin

          DemandCentreNameEdit.SetFieldValue(lYMDemandCentre.Name);
          DescriptionEdit.SetFieldValue(lYMDemandCentre.Description);
          NodeNumberEdit.SetFieldValue(lYMDemandCentre.NodeNumber);
          AveReturnFlowFactorEdit.SetFieldValue(lYMDemandCentre.AveReturnFlowFactor);
          AveMonthlyEvaporationEdit.SetFieldValue(lYMDemandCentre.AveEvaporation);
          RoutingConstantEdit.SetFieldValue(lYMDemandCentre.RoutingConstant);
          RainScalingFactorEdit.SetFieldValue(lYMDemandCentre.RainfallScalingFactor);
          TotalFlowLostEdit.SetFieldValue(lYMDemandCentre.TotalFlowLost);
          StdDeviationFactorEdit.SetFieldValue(lYMDemandCentre.StdDeviationFactor);
          ReclaimationChannelCbx.Checked := lYMDemandCentre.ReclaimationPlantExists;

          NodeNumberRefCbx.Items.Clear;

          lNodesList := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.ReservoirList;
          if (lNodesList.ReservoirAndNodesCount > 0) then
          begin
            for lIndexA := 0 to lNodesList.ReservoirAndNodesCount - 1 do
            begin
              LReservoirData  := lNodesList.ReservoirOrNodeByIndex[lIndexA];
              if(LReservoirData.ReservoirConfigurationData.CatchmentRef > 0) then
              begin
                NodeNumberRefCbx.Items.AddObject
                  (LReservoirData.ReservoirConfigurationData.ReservoirName, TObject(LReservoirData.ReservoirConfigurationData.ReservoirIdentifier));
              end;
            end;

            lFound := False;
            lIndexA := 0;
            while ((not lFound) and (lIndexA < NodeNumberRefCbx.Items.Count)) do
            begin
              if (Integer(NodeNumberRefCbx.Items.Objects[lIndexA]) = lYMDemandCentre.NodeRefNr) then
                lFound := True
              else
                lIndexA := lIndexA + 1;
            end;
            if (lFound) then
              NodeNumberRefCbx.ItemIndex := lIndexA
            else
              NodeNumberRefCbx.ItemIndex := -1;
          end;

          RePopulateNodes;

          LReservoirData := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData
                            .ReservoirList.ReservoirOrNodeByIdentifier[lYMDemandCentre.NodeNumber];
          if LReservoirData <> nil then
          begin
            DemandCentreXCoordEdit.SetFieldValue(LReservoirData.ReservoirConfigurationData.XCoord);
            DemandCentreYCoordEdit.SetFieldValue(LReservoirData.ReservoirConfigurationData.YCoord);
          end;

          if (lYMDemandCentre.ConsumptiveUseChannelNr <> 0) then
          begin
            lGeneralChannel :=  TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.
                                 ChannelList.ChannelByChannelNumber[lYMDemandCentre.ConsumptiveUseChannelNr];
            if (lGeneralChannel <> nil) then
            begin
              ConsumptiveChannelCbx.SetFieldIndex
                (ConsumptiveChannelCbx.Items.IndexOfObject(TObject(lGeneralChannel.ChannelNumber)));
            end;
          end;
        end;
      finally
        FBusyPopulating := False;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYMDemandCentreValidator.RePopulateNodes;
const OPNAME = 'TYMDemandCentreValidator.RePopulateNodes';
var
  lIndexA           : integer;
  lYMDemandCentre   : IYMDemandCentre;
  lChannelList      : IChannelList;
  lGeneralChannel   : IGeneralFlowChannel;
begin
  try
    if (FIdentifier >= 0) then
    begin
      lYMDemandCentre := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.YMDemandCentreList.YMDemandCentreByID[FIdentifier];
      if (lYMDemandCentre <> nil) then
      begin
        with YMDemandCentreDialog do
        begin
          lChannelList := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.ChannelList;
          if (lChannelList <> nil) then
          begin
            for lIndexA := 0 to lChannelList.ChannelCount - 1do
            begin
              lGeneralChannel := lChannelList.ChannelByIndex[lIndexA];
              if((lGeneralChannel.ChannelPenalty <> nil) and
                 (lGeneralChannel.ChannelPenalty.ChannelPenaltyArcCount = 2))  and
                (lGeneralChannel.ChannelType in [2, 8, 11])  then
              begin
                ConsumptiveChannelCbx.Items.AddObject
                  ('(' + IntToStr(lGeneralChannel.ChannelID) + ') ' + lGeneralChannel.ChannelName,
                   TObject(lGeneralChannel.ChannelNumber));
              end;
            end;
          end;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYMDemandCentreValidator.StudyHasChanged: Boolean;
const OPNAME = 'TYMDemandCentreValidator.StudyHasChanged';
begin
  Result := inherited StudyHasChanged;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYMDemandCentreValidator.UpdateAveMonthlyEvaporation;
const OPNAME = 'TYMDemandCentreValidator.UpdateAveMonthlyEvaporation';
var
  LYMDemandCentre : IYMDemandCentre;
  LMessage        : string;
begin
  try
    LYMDemandCentre := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.YMDemandCentreList.YMDemandCentreByID[FIdentifier];
    if (LYMDemandCentre <> nil) then
    begin
      with YMDemandCentreDialog do
      begin
        AveMonthlyEvaporationEdit.FieldValidationError := '';
        if FAppModules.FieldProperties.ValidateFieldProperty(
                        AveMonthlyEvaporationEdit.FieldProperty.FieldName,
                        AveMonthlyEvaporationEdit.Text,
                        LMessage) then
        begin
          LYMDemandCentre.AveEvaporation := StrToFloat(AveMonthlyEvaporationEdit.Text);
          RePopulateDataViewer;
          DoContextValidation (dvtYMDemandCentreAveMonthlyEvaporation);
        end
        else
          AveMonthlyEvaporationEdit.FieldValidationError := LMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYMDemandCentreValidator.UpdateAveReturnFlowFactor;
const OPNAME = 'TYMDemandCentreValidator.UpdateAveReturnFlowFactor';
var
  LYMDemandCentre : IYMDemandCentre;
  LMessage        : string;
begin
  try
    LYMDemandCentre := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.YMDemandCentreList.YMDemandCentreByID[FIdentifier];
    if (LYMDemandCentre <> nil) then
    begin
      with YMDemandCentreDialog do
      begin
        AveReturnFlowFactorEdit.FieldValidationError := '';
        if FAppModules.FieldProperties.ValidateFieldProperty(
                        AveReturnFlowFactorEdit.FieldProperty.FieldName,
                        AveReturnFlowFactorEdit.Text,
                        LMessage) then
        begin
          LYMDemandCentre.AveReturnFlowFactor := StrToFloat(AveReturnFlowFactorEdit.Text);
          RePopulateDataViewer;
          DoContextValidation (dvtYMDemandCentreAveReturnFlowFactor);
        end
        else
          AveReturnFlowFactorEdit.FieldValidationError := LMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYMDemandCentreValidator.UpdateConsumptiveChannel;
const OPNAME = 'TYMDemandCentreValidator.UpdateConsumptiveChannel';
var
  lGeneralFlowChannel : IGeneralFlowChannel;
  lChannelNr          : integer;
  lYMDemandCentre     : IYMDemandCentre;
  lMessage            : string;
begin
  try
    lYMDemandCentre := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.YMDemandCentreList.YMDemandCentreByID[FIdentifier];
    if (lYMDemandCentre <> nil) then
    begin
      with YMDemandCentreDialog do
      begin
        lGeneralFlowChannel   := nil;
        lChannelNr := -1;

        if (ConsumptiveChannelCbx.ItemIndex >= 0) then
        begin
          lChannelNr := Integer(ConsumptiveChannelCbx.Items.Objects[ConsumptiveChannelCbx.ItemIndex]);
          lGeneralFlowChannel   := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.
                            ChannelList.ChannelByChannelNumber[lChannelNr];
        end;
        if (lGeneralFlowChannel <> nil) then
        begin
          if (FAppModules.FieldProperties.ValidateFieldProperty('DemandCentreConsumptiveChannelNumber',IntToStr(lChannelNr), lMessage)) then
          begin
            lGeneralFlowChannel.UpStreamNodeNumber  := lYMDemandCentre.NodeNumber;
            lYMDemandCentre.ConsumptiveUseChannelNr := lChannelNr;
            ConsumptiveChannelCbx.SetFieldIndex(ConsumptiveChannelCbx.Items.IndexOfObject(TObject(lChannelNr)));
            DoContextValidation(dvtYMDemandCentreConsumptive);
          end
          else
            ConsumptiveChannelCbx.ValidationError := lMessage;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYMDemandCentreValidator.UpdateDescription;
const OPNAME = 'TYMDemandCentreValidator.UpdateDescription';
var
  LYMDemandCentre : IYMDemandCentre;
  LMessage        : string;
begin
  try
    LYMDemandCentre := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.YMDemandCentreList.YMDemandCentreByID[FIdentifier];
    if (LYMDemandCentre <> nil) then
    begin
      with YMDemandCentreDialog do
      begin
        DescriptionEdit.FieldValidationError := '';
        if FAppModules.FieldProperties.ValidateFieldProperty(
                        DescriptionEdit.FieldProperty.FieldName,
                        DescriptionEdit.Text,
                        LMessage) then
        begin
          LYMDemandCentre.Description := DescriptionEdit.Text;
          RePopulateDataViewer;
          DoContextValidation (dvtYMDemandCentreDescription);
        end
        else
          DescriptionEdit.FieldValidationError := LMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYMDemandCentreValidator.UpdateDemandCentreName;
const OPNAME = 'TYMDemandCentreValidator.UpdateDemandCentreName';
var
  LYMDemandCentre : IYMDemandCentre;
  LMessage        : string;
begin
  try
    LYMDemandCentre := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.YMDemandCentreList.YMDemandCentreByID[FIdentifier];
    if (LYMDemandCentre <> nil) then
    begin
      with YMDemandCentreDialog do
      begin
        DemandCentreNameEdit.FieldValidationError := '';
        if FAppModules.FieldProperties.ValidateFieldProperty(
                        DemandCentreNameEdit.FieldProperty.FieldName,
                        DemandCentreNameEdit.Text,
                        LMessage) then
        begin
          LYMDemandCentre.Name := DemandCentreNameEdit.Text;
          RePopulateDataViewer;
          DoContextValidation (dvtYMDemandCentreName);
        end
        else
          DemandCentreNameEdit.FieldValidationError := LMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYMDemandCentreValidator.UpdateNodeNumber;
const OPNAME = 'TYMDemandCentreValidator.UpdateNodeNumber';
var
  LYMDemandCentre : IYMDemandCentre;
  LMessage        : string;
begin
  try
    LYMDemandCentre := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.YMDemandCentreList.YMDemandCentreByID[FIdentifier];
    if (LYMDemandCentre <> nil) then
    begin
      with YMDemandCentreDialog do
      begin
        NodeNumberEdit.FieldValidationError := '';
        if FAppModules.FieldProperties.ValidateFieldProperty(
                        NodeNumberEdit.FieldProperty.FieldName,
                        NodeNumberEdit.Text,
                        LMessage) then
        begin
          LYMDemandCentre.NodeNumber := StrToInt(NodeNumberEdit.Text);
          RePopulateDataViewer;
          DoContextValidation (dvtYMDemandCentreNodeNumber);
        end
        else
          NodeNumberEdit.FieldValidationError := LMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYMDemandCentreValidator.UpdateNodeNumberRefCbx;
const OPNAME = 'TYMDemandCentreValidator.UpdateNodeNumberRefCbx';
var
  LYMDemandCentre : IYMDemandCentre;
  LMessage        : string;
begin
  try
    LYMDemandCentre := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.YMDemandCentreList.YMDemandCentreByID[FIdentifier];
    if (LYMDemandCentre <> nil) then
    begin
      with YMDemandCentreDialog do
      begin
        NodeNumberRefCbx.ValidationError := '';
        if FAppModules.FieldProperties.ValidateFieldProperty(
                        NodeNumberRefCbx.FieldProperty.FieldName,
                        IntToStr(Integer(NodeNumberRefCbx.Items.Objects[NodeNumberRefCbx.ItemIndex])),
                        LMessage) then
        begin
          if NodeNumberRefCbx.ItemIndex <> -1 then
            LYMDemandCentre.NodeRefNr := Integer(NodeNumberRefCbx.Items.Objects[NodeNumberRefCbx.ItemIndex]);
          RePopulateDataViewer;
          DoContextValidation (dvtYMDemandCentreNodeNumberRef);
        end
        else
          NodeNumberRefCbx.ValidationError := LMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYMDemandCentreValidator.UpdateRainScalingFactor;
const OPNAME = 'TYMDemandCentreValidator.UpdateRainScalingFactor';
var
  LYMDemandCentre : IYMDemandCentre;
  LMessage        : string;
begin
  try
    LYMDemandCentre := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.YMDemandCentreList.YMDemandCentreByID[FIdentifier];
    if (LYMDemandCentre <> nil) then
    begin
      with YMDemandCentreDialog do
      begin
        RainScalingFactorEdit.FieldValidationError := '';
        if FAppModules.FieldProperties.ValidateFieldProperty(
                        RainScalingFactorEdit.FieldProperty.FieldName,
                        RainScalingFactorEdit.Text,
                        LMessage) then
        begin
          LYMDemandCentre.RainfallScalingFactor := StrToFloat(RainScalingFactorEdit.Text);
          RePopulateDataViewer;
          DoContextValidation (dvtYMDemandCentreRainScalingFactor);
        end
        else
          RainScalingFactorEdit.FieldValidationError := LMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYMDemandCentreValidator.UpdateRoutingConstant;
const OPNAME = 'TYMDemandCentreValidator.UpdateRoutingConstant';
var
  LYMDemandCentre : IYMDemandCentre;
  LMessage        : string;
begin
  try
    LYMDemandCentre := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.YMDemandCentreList.YMDemandCentreByID[FIdentifier];
    if (LYMDemandCentre <> nil) then
    begin
      with YMDemandCentreDialog do
      begin
        RoutingConstantEdit.FieldValidationError := '';
        if FAppModules.FieldProperties.ValidateFieldProperty(
                        RoutingConstantEdit.FieldProperty.FieldName,
                        RoutingConstantEdit.Text,
                        LMessage) then
        begin
          LYMDemandCentre.RoutingConstant := StrToFloat(RoutingConstantEdit.Text);
          RePopulateDataViewer;
          DoContextValidation (dvtYMDemandCentreRoutingConstant);
        end
        else
          RoutingConstantEdit.FieldValidationError := LMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYMDemandCentreValidator.UpdateStdDeviationFactor;
const OPNAME = 'TYMDemandCentreValidator.UpdateStdDeviationFactor';
var
  LYMDemandCentre : IYMDemandCentre;
  LMessage        : string;
begin
  try
    LYMDemandCentre := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.YMDemandCentreList.YMDemandCentreByID[FIdentifier];
    if (LYMDemandCentre <> nil) then
    begin
      with YMDemandCentreDialog do
      begin
        StdDeviationFactorEdit.FieldValidationError := '';
        if FAppModules.FieldProperties.ValidateFieldProperty(
                        StdDeviationFactorEdit.FieldProperty.FieldName,
                        StdDeviationFactorEdit.Text,
                        LMessage) then
        begin
          LYMDemandCentre.StdDeviationFactor := StrToFloat(StdDeviationFactorEdit.Text);
          RePopulateDataViewer;
          DoContextValidation (dvtYMDemandCentreStdDeviationFactor);
        end
        else
          StdDeviationFactorEdit.FieldValidationError := LMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYMDemandCentreValidator.UpdateTotalFlowLost;
const OPNAME = 'TYMDemandCentreValidator.UpdateTotalFlowLost';
var
  LYMDemandCentre : IYMDemandCentre;
  LMessage        : string;
begin
  try
    LYMDemandCentre := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.YMDemandCentreList.YMDemandCentreByID[FIdentifier];
    if (LYMDemandCentre <> nil) then
    begin
      with YMDemandCentreDialog do
      begin
        TotalFlowLostEdit.FieldValidationError := '';
        if FAppModules.FieldProperties.ValidateFieldProperty(
                        TotalFlowLostEdit.FieldProperty.FieldName,
                        TotalFlowLostEdit.Text,
                        LMessage) then
        begin
          LYMDemandCentre.TotalFlowLost := StrToFloat(TotalFlowLostEdit.Text);
          RePopulateDataViewer;
          DoContextValidation (dvtYMDemandCentreTotalFlowLost);
        end
        else
          TotalFlowLostEdit.FieldValidationError := LMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYMDemandCentreValidator.ValidateAveMonthlyEvaporation;
const OPNAME ='TYMDemandCentreValidator.ValidateAveMonthlyEvaporation';
var
  LYMDemandCentre : IYMDemandCentre;
  LMessage : WideString;
begin
  try
    LYMDemandCentre := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.YMDemandCentreList.YMDemandCentreByID[FIdentifier];
    if (LYMDemandCentre <> nil) then
    begin
      with YMDemandCentreDialog do
      begin
        AveMonthlyEvaporationEdit.ContextValidationError := '';
        if (not LYMDemandCentre.Validate(LMessage, AveMonthlyEvaporationEdit.FieldProperty.FieldName)) then
          AveMonthlyEvaporationEdit.ContextValidationError := LMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYMDemandCentreValidator.ValidateAveReturnFlowFactor;
const OPNAME = 'TYMDemandCentreValidator.ValidateAveReturnFlowFactor';
var
  LYMDemandCentre : IYMDemandCentre;
  LMessage : WideString;
begin
  try
    LYMDemandCentre := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.YMDemandCentreList.YMDemandCentreByID[FIdentifier];
    if (LYMDemandCentre <> nil) then
    begin
      with YMDemandCentreDialog do
      begin
        AveReturnFlowFactorEdit.ContextValidationError := '';
        if (not LYMDemandCentre.Validate(LMessage, AveReturnFlowFactorEdit.FieldProperty.FieldName)) then
          AveReturnFlowFactorEdit.ContextValidationError := LMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYMDemandCentreValidator.ValidateConsumptiveChannel;
const OPNAME = 'TYMDemandCentreValidator.ValidateConsumptiveChannel';
var
  lYMDemandCentre  : IYMDemandCentre;
begin
  try
    lYMDemandCentre := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.YMDemandCentreList.YMDemandCentreByID[FIdentifier];
    if (lYMDemandCentre <> nil) then
    begin
      with YMDemandCentreDialog do
      begin
        FErrorMessage := '';
        if (lYMDemandCentre.Validate(FErrorMessage, 'DemandCentreConsumptiveChannelNumber')) then
        begin
          ConsumptiveChannelCbx.InValidationError := FALSE;
          ConsumptiveChannelCbx.ShowErrorState(FALSE);
        end
        else
        begin
          ConsumptiveChannelCbx.InValidationError := TRUE;
          ConsumptiveChannelCbx.ValidationError := FErrorMessage;
          ConsumptiveChannelCbx.ShowErrorState(TRUE);
          FAllErrorMessages.Add(Trim(FErrorMessage));
        end;
      end;
    end;  
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYMDemandCentreValidator.ValidateDescription;
const OPNAME = 'TYMDemandCentreValidator.ValidateDescription';
var
  LYMDemandCentre : IYMDemandCentre;
  LMessage : WideString;
begin
  try
    LYMDemandCentre := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.YMDemandCentreList.YMDemandCentreByID[FIdentifier];
    if (LYMDemandCentre <> nil) then
    begin
      with YMDemandCentreDialog do
      begin
        DescriptionEdit.ContextValidationError := '';
        if (not LYMDemandCentre.Validate(LMessage, DescriptionEdit.FieldProperty.FieldName)) then
          DescriptionEdit.ContextValidationError := LMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYMDemandCentreValidator.ValidateName;
const OPNAME = 'TYMDemandCentreValidator.ValidateName';
var
  LYMDemandCentre : IYMDemandCentre;
  LMessage : WideString;
begin
  try
    LYMDemandCentre := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.YMDemandCentreList.YMDemandCentreByID[FIdentifier];
    if (LYMDemandCentre <> nil) then
    begin
      with YMDemandCentreDialog do
      begin
        DemandCentreNameEdit.ContextValidationError := '';
        if (not LYMDemandCentre.Validate(LMessage, DemandCentreNameEdit.FieldProperty.FieldName)) then
          DemandCentreNameEdit.ContextValidationError := LMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYMDemandCentreValidator.ValidateNodeNumber;
const OPNAME = 'TYMDemandCentreValidator.ValidateNodeNumber';
var
  LYMDemandCentre : IYMDemandCentre;
  LMessage : WideString;
begin
  try
    LYMDemandCentre := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.YMDemandCentreList.YMDemandCentreByID[FIdentifier];
    if (LYMDemandCentre <> nil) then
    begin
      with YMDemandCentreDialog do
      begin
        NodeNumberEdit.ContextValidationError := '';
        if (not LYMDemandCentre.Validate(LMessage, NodeNumberEdit.FieldProperty.FieldName)) then
          NodeNumberEdit.ContextValidationError := LMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYMDemandCentreValidator.ValidateNodeNumberRefCbx;
const OPNAME = 'TYMDemandCentreValidator.ValidateNodeNumberRefCbx';
var
  LYMDemandCentre   : IYMDemandCentre; 
  LMessage          : WideString;
  lReservoir        : IReservoirConfigurationData;
  lReservoirList    : IReservoirDataList;
  lIndexA           : integer;
  lCurrentValue     : Integer;
  lItemFound        : Boolean;
begin
  lItemFound := False;
  try
    LYMDemandCentre  := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.YMDemandCentreList.YMDemandCentreByID[FIdentifier];
    if (LYMDemandCentre <> nil) then
    begin
      with YMDemandCentreDialog do
      begin
        FErrorMessage := '';
        if NodeNumberRefCbx.ItemIndex <> -1 then
        begin
          NodeNumberRefCbx.ValidationError := '';
          if not LYMDemandCentre.Validate(LMessage,NodeNumberRefCbx.FieldProperty.FieldName) then
            NodeNumberRefCbx.ValidationError := LMessage;

          lCurrentValue := Integer(NodeNumberRefCbx.Items.Objects[NodeNumberRefCbx.ItemIndex]);
          lItemFound    := False;
          if lCurrentValue <> -1 then
          begin
            lReservoirList := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.ReservoirList;
            if (lReservoirList <> nil) then
            begin
              for lIndexA := 0 to lReservoirList.ReservoirAndNodesCount - 1 do
              begin
                lReservoir := lReservoirList.ReservoirOrNodeByIndex[lIndexA].ReservoirConfigurationData;
                if (lReservoir.ReservoirIdentifier <> 0) and (lReservoir.CatchmentRef <> 0) then
                begin
                  if lCurrentValue = lReservoir.ReservoirIdentifier then
                  begin
                    lItemFound := True;
                    Break;
                  end;
                end;
              end;
            end;
          end;  
        end;

        if not lItemFound then
          NodeNumberRefCbx.ValidationError := 'Demand centre node not found';
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYMDemandCentreValidator.ValidateRainScalingFactor;
const OPNAME = 'TYMDemandCentreValidator.ValidateRainScalingFactor';
var
  LYMDemandCentre : IYMDemandCentre;
  LMessage : WideString;
begin
  try
    LYMDemandCentre := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.YMDemandCentreList.YMDemandCentreByID[FIdentifier];
    if (LYMDemandCentre <> nil) then
    begin
      with YMDemandCentreDialog do
      begin
        RainScalingFactorEdit.ContextValidationError := '';
        if (not LYMDemandCentre.Validate(LMessage, RainScalingFactorEdit.FieldProperty.FieldName)) then
          RainScalingFactorEdit.ContextValidationError := LMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYMDemandCentreValidator.ValidateRoutingConstant;
const OPNAME = 'TYMDemandCentreValidator.ValidateRoutingConstant';
var
  LYMDemandCentre : IYMDemandCentre;
  LMessage : WideString;
begin
  try
    LYMDemandCentre := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.YMDemandCentreList.YMDemandCentreByID[FIdentifier];
    if (LYMDemandCentre <> nil) then
    begin
      with YMDemandCentreDialog do
      begin
        RoutingConstantEdit.ContextValidationError := '';
        if (not LYMDemandCentre.Validate(LMessage, RoutingConstantEdit.FieldProperty.FieldName)) then
          RoutingConstantEdit.ContextValidationError := LMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYMDemandCentreValidator.ValidateStdDeviationFactor;
const OPNAME = 'TYMDemandCentreValidator.ValidateStdDeviationFactor';
var
  LYMDemandCentre : IYMDemandCentre;
  LMessage : WideString;
begin
  try
    LYMDemandCentre := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.YMDemandCentreList.YMDemandCentreByID[FIdentifier];
    if (LYMDemandCentre <> nil) then
    begin
      with YMDemandCentreDialog do
      begin
        StdDeviationFactorEdit.ContextValidationError := '';
        if (not LYMDemandCentre.Validate(LMessage, StdDeviationFactorEdit.FieldProperty.FieldName)) then
          StdDeviationFactorEdit.ContextValidationError := LMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYMDemandCentreValidator.ValidateTotalFlowLost;
const OPNAME = 'TYMDemandCentreValidator.ValidateTotalFlowLost';
var
  LYMDemandCentre : IYMDemandCentre;
  LMessage : WideString;
begin
  try
    LYMDemandCentre := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.YMDemandCentreList.YMDemandCentreByID[FIdentifier];
    if (LYMDemandCentre <> nil) then
    begin
      with YMDemandCentreDialog do
      begin
        TotalFlowLostEdit.ContextValidationError := '';
        if (not LYMDemandCentre.Validate(LMessage, TotalFlowLostEdit.FieldProperty.FieldName)) then
          TotalFlowLostEdit.ContextValidationError := LMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYMDemandCentreValidator.YMDemandCentreDialog: TYMDemandCentreDialog;
const OPNAME = 'TYMDemandCentreValidator.YMDemandCentreDialog';
begin
  Result := Nil;
  try
    if (FPanel <> nil) then
      Result := TYMDemandCentreDialog(FPanel);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYMDemandCentreValidator.UpdateXCoord;
const OPNAME = 'TYMDemandCentreValidator.UpdateXCoord';
var
  lErrorMessage    : string;
  LReservoirObject : IReservoirData;
  LYMDemandCentre  : IYMDemandCentre;
begin
  try
    if YMDemandCentreDialog.DemandCentreXCoordEdit.HasValueChanged then
    begin
      LYMDemandCentre := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.
                         YMDemandCentreList.YMDemandCentreByID[FIdentifier];
      if (LYMDemandCentre <> nil) then
      begin
        LReservoirObject := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.ReservoirList.ReservoirOrNodeByIdentifier[LYMDemandCentre.NodeNumber];
        if (LReservoirObject <> nil) then
        begin
          with YMDemandCentreDialog do
          begin
            if FAppModules.FieldProperties.ValidateFieldProperty('XCoord',DemandCentreXCoordEdit.Text, lErrorMessage) then
            begin
              DemandCentreXCoordEdit.FieldValidationError := lErrorMessage;
              LReservoirObject.ReservoirConfigurationData.XCoord := StrToFloat(DemandCentreXCoordEdit.Text);
              DemandCentreXCoordEdit.SetFieldValue(LReservoirObject.ReservoirConfigurationData.XCoord);
            end
            else
              DemandCentreXCoordEdit.FieldValidationError := lErrorMessage;
          end;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYMDemandCentreValidator.UpdateYCoord;
const OPNAME = 'TYMDemandCentreValidator.UpdateYCoord';
var
  lErrorMessage    : string;
  LReservoirObject : IReservoirData;
  LYMDemandCentre  : IYMDemandCentre;
begin
  try
    if YMDemandCentreDialog.DemandCentreYCoordEdit.HasValueChanged then
    begin
      LYMDemandCentre := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.
                         YMDemandCentreList.YMDemandCentreByID[FIdentifier];
      if (LYMDemandCentre <> nil) then
      begin
        LReservoirObject := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.ReservoirList.
                            ReservoirOrNodeByIdentifier[LYMDemandCentre.NodeNumber];
        if (LReservoirObject <> nil) then
        begin
          with YMDemandCentreDialog do
          begin
            if FAppModules.FieldProperties.ValidateFieldProperty('YCoord',DemandCentreYCoordEdit.Text, lErrorMessage) then
            begin
              DemandCentreYCoordEdit.FieldValidationError := lErrorMessage;
              LReservoirObject.ReservoirConfigurationData.YCoord := StrToFloat(DemandCentreYCoordEdit.Text);
              DemandCentreYCoordEdit.SetFieldValue(LReservoirObject.ReservoirConfigurationData.YCoord);
            end
            else
              DemandCentreYCoordEdit.FieldValidationError := lErrorMessage;
          end;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYMDemandCentreValidator.ValidateXCoord(AReservoirData: IReservoirData);
const OPNAME = 'TYMDemandCentreValidator.ValidateXCoord';
begin
  try
    with YMDemandCentreDialog do
    begin
      FErrorMessage := '';
      DemandCentreXCoordEdit.ContextValidationError := FErrorMessage;
      if (NOT AReservoirData.ReservoirConfigurationData.Validate(FErrorMessage, 'XCoord')) then
        FAllErrorMessages.Add(FErrorMessage);
      DemandCentreXCoordEdit.ContextValidationError := FErrorMessage;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYMDemandCentreValidator.ValidateYCoord(AReservoirData: IReservoirData);
const OPNAME = 'TYMDemandCentreValidator.ValidateYCoord';
begin
  try
    with YMDemandCentreDialog do
    begin
      FErrorMessage := '';
      DemandCentreYCoordEdit.ContextValidationError := FErrorMessage;
      if (NOT AReservoirData.ReservoirConfigurationData.Validate(FErrorMessage, 'YCoord')) then
        FAllErrorMessages.Add(FErrorMessage);
      DemandCentreYCoordEdit.ContextValidationError := FErrorMessage;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.

