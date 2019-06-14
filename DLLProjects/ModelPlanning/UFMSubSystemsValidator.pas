{******************************************************************************}
{*  UNIT      : Contains the class TFMSubSystemsValidator.                    *}
{*  AUTHOR    : Riana Steyn                                                   *}
{*  DATE      : 2006/01/11                                                    *}
{*  COPYRIGHT : Copyright © 2006 DWAF                                         *}
{******************************************************************************}

unit UFMSubSystemsValidator;

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
  UFMSubSystemsDialog,
  UAbstractYieldDataDialogValidator,
  UYieldContextValidationType;

type

  TFMSubSystemsValidator = class(TAbstractYieldDataDialogValidator)
  protected
    FHeading     : string;
    FSubSystemID : integer;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    procedure CreateDialog; virtual;
    procedure OnEditControlEnter(Sender: TObject); override;
    procedure OnEditControltExit(Sender: TObject); override;
    procedure OnRgpFirmYieldClick(Sender: TObject);
    procedure OnClbReservoirsInSubSystemClick(Sender: TObject);
    procedure OnRgpSupportCalcTypeClick(Sender: TObject);
    procedure OnStringGridCellDataHasChanged (ASender: TObject; ACol, ARow: integer); override;
    procedure OnTrvSubSystemsChange (Sender : TObject;
                                     ANode  : TTreeNode);
    procedure OnStartPercSelectionChange (Sender        : TObject;
                                          ACol, ARow    : Integer;
                                          var CanSelect : Boolean);
    procedure OnGrdRoutingChannelsSelectCell (Sender        : TObject;
                                              ACol, ARow    : Integer;
                                              var CanSelect : Boolean);
    procedure RePopulateDataViewer;
    procedure DoAddSubSystem (Sender: TObject);
    procedure DoDeleteSubSystem (Sender: TObject);
    procedure PopulateSubSystemComboBoxes (AAllocDef    : IAllocationDefinition;
                                           ASubSystemID : integer);
    procedure PopulateSubSystem (AAllocDef : IAllocationDefinition);
    procedure PopulateCoefficients (AAllocDef  : IAllocationDefinition;
                                    ASubSystem : ISubSystem;
                                    APercCol   : integer;
                                    ACurveCol  : integer);
    procedure UpdateSubSystemName;
    procedure UpdateSubSystemStartYear;
    procedure UpdateSubSystemStartMonth;
    procedure UpdateSubSystemEndYear;
    procedure UpdateSubSystemEndMonth;
    procedure UpdateSubtractedSubSystem;
    procedure UpdateSupportingSubSystem;
    procedure UpdateSupportChannel;
    procedure UpdateShortTermYield;
    procedure UpdateLongTermYield;
    procedure UpdateLowestStreamFlow;
    procedure UpdateFirmYield;
    procedure UpdateReservoirsInSubSystem;
    procedure UpdateCoefficients (ACol   : integer;
                                  ARow   : integer;
                                  AValue : string);
    procedure UpdateRoutingChannels;
    procedure UpdateSupportCalcType;
    procedure ValidateSubSystemName (ASubSystem : ISubSystem);
    procedure ValidateSubSystemStartYear (ASubSystem : ISubSystem);
    procedure ValidateSubSystemStartMonth (ASubSystem : ISubSystem);
    procedure ValidateSubSystemEndYear (ASubSystem : ISubSystem);
    procedure ValidateSubSystemEndMonth (ASubSystem : ISubSystem);
    procedure ValidateSubtractedSubSystem (ASubSystem : ISubSystem);
    procedure ValidateSupportingSubSystem (ASubSystem : ISubSystem);
    procedure ValidateShortTermYield (ASubSystem : ISubSystem);
    procedure ValidateLongTermYield (ASubSystem : ISubSystem);
    procedure ValidateLowestStreamFlow (ASubSystem : ISubSystem);
    procedure ValidateFirmYield (ASubSystem : ISubSystem);
    procedure ValidateReservoirsInSubSystem (ASubSystem : ISubSystem);
    procedure ValidateCoefficients (AAllocDef  : IAllocationDefinition;
                                    ASubSystem : ISubSystem);
    procedure ValidateRoutingChannels (ASubSystem : ISubSystem);
    procedure ValidateSupportCalcType (ASubSystem : ISubSystem);
  public
    function Initialise: boolean; override;
    function SaveState: boolean; override;
    function LanguageHasChanged: boolean; override;
    function StudyHasChanged: boolean; override;
    function StudyDataHasChanged(AContext: TChangeContext; AFieldName,AOldValue,ANewValue: string): boolean; override;
    procedure ClearDataViewer; override;
    procedure PopulateDataViewer; override;
    procedure DoContextValidation (AValidationType : TDialogValidationType); override;
    function FMSubSystemsDialog : TFMSubSystemsDialog;
  end;

implementation

uses
  SysUtils,
  VCL.Graphics,
  UUtilities,
  UYieldModelDataGUIForm,
  UErrorHandlingOperations, Math, Variants;

{******************************************************************************}
{* TFMSubSystemsValidator                                                     *}
{******************************************************************************}

procedure TFMSubSystemsValidator.CreateMemberObjects;
const OPNAME = 'TFMSubSystemsValidator.CreateMemberObjects';
var
  lpPanel     : TFMSubSystemsDialog;
begin
  try
    inherited CreateMemberObjects;
    FIdentifier  := 0;
    FSubSystemID := 0;
    FHeading     := 'TabCaption.SubSystems';

    CreateDialog;
    lpPanel := FMSubSystemsDialog;
    with lpPanel do
    begin
      BtnAddSubSystem.OnClick    := DoAddSubSystem;
      BtnDeleteSubSystem.OnClick := DoDeleteSubSystem;
      TrvSubSystems.OnChange     := OnTrvSubSystemsChange;
      GrdStartStoragePerc.OnSelectCell := OnStartPercSelectionChange;
      GrdStartStoragePerc.OnColEnter   := OnStringGridColEnter;

      GrdDecisionCurveSet.OnSelectCell := OnStartPercSelectionChange;
      GrdDecisionCurveSet.OnColEnter   := OnStringGridColEnter;

      EdtSubSystemName.FieldProperty  := FAppModules.FieldProperties.FieldProperty('SubSystemName');
      EdtSubSystemName.OnEnter        := OnEditControlEnter;
      EdtSubSystemName.OnExit         := OnEditControltExit;

      CbxStartYear.FieldProperty      := FAppModules.FieldProperties.FieldProperty('SubSystemStartYear');
      CbxStartYear.OnEnter            := OnEditControlEnter;
      CbxStartYear.OnExit             := OnEditControltExit;

      CbxStartMonth.FieldProperty     := FAppModules.FieldProperties.FieldProperty('SubSystemStartMonth');
      CbxStartMonth.OnEnter           := OnEditControlEnter;
      CbxStartMonth.OnExit            := OnEditControltExit;

      CbxEndYear.FieldProperty        := FAppModules.FieldProperties.FieldProperty('SubSystemEndYear');
      CbxEndYear.OnEnter              := OnEditControlEnter;
      CbxEndYear.OnExit               := OnEditControltExit;

      CbxEndMonth.FieldProperty       := FAppModules.FieldProperties.FieldProperty('SubSystemEndMonth');
      CbxEndMonth.OnEnter             := OnEditControlEnter;
      CbxEndMonth.OnExit              := OnEditControltExit;

      CbxSubtracted.FieldProperty     := FAppModules.FieldProperties.FieldProperty('SubtractedSubSystemID');
      CbxSubtracted.OnEnter           := OnEditControlEnter;
      CbxSubtracted.OnExit            := OnEditControltExit;

      CbxSupporting.FieldProperty     := FAppModules.FieldProperties.FieldProperty('SupportingSubSystemID');
      CbxSupporting.OnEnter           := OnEditControlEnter;
      CbxSupporting.OnExit            := OnEditControltExit;

      CbxSupportChannel.FieldProperty := FAppModules.FieldProperties.FieldProperty('SupportingChannelNr');
      CbxSupportChannel.OnEnter       := OnEditControlEnter;
      CbxSupportChannel.OnExit        := OnEditControltExit;

      EdtYieldShort.FieldProperty     := FAppModules.FieldProperties.FieldProperty('ShortTermYield');
      EdtYieldShort.OnEnter           := OnEditControlEnter;
      EdtYieldShort.OnExit            := OnEditControltExit;

      EdtLowestStreamFlow.FieldProperty := FAppModules.FieldProperties.FieldProperty('LowestStreamFlow');
      EdtLowestStreamFlow.OnEnter       := OnEditControlEnter;
      EdtLowestStreamFlow.OnExit        := OnEditControltExit;

      EdtYieldLong.FieldProperty      := FAppModules.FieldProperties.FieldProperty('LongTermYield');
      EdtYieldLong.OnEnter            := OnEditControlEnter;
      EdtYieldLong.OnExit             := OnEditControltExit;

      RgpFirmYield.FieldProperty      := FAppModules.FieldProperties.FieldProperty('FirmYield');
      RgpFirmYield.OnEnter            := OnEditControlEnter;
      RgpFirmYield.OnClick            := OnRgpFirmYieldClick;

      ClbReservoirsInSubSystem.FieldProperty := FAppModules.FieldProperties.FieldProperty('SubSystemReservoirNrs');
      ClbReservoirsInSubSystem.OnEnter       := OnEditControlEnter;
      ClbReservoirsInSubSystem.OnClick       := OnClbReservoirsInSubSystemClick;

      GrdRoutingChannels.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('RoutingChannelNr'));
      GrdRoutingChannels.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('RoutingChannelNr'));
      GrdRoutingChannels.OnSelectCell := OnGrdRoutingChannelsSelectCell;

      CbxChannel.FieldProperty     := FAppModules.FieldProperties.FieldProperty('RoutingChannelNr');
      CbxChannel.OnEnter           := OnEditControlEnter;
      CbxChannel.OnChange          := OnEditControltExit;

      RgpSupportCalcType.FieldProperty := FAppModules.FieldProperties.FieldProperty('SupportCalcType');
      RgpSupportCalcType.OnEnter       := OnEditControlEnter;
      RgpSupportCalcType.OnClick       := OnRgpSupportCalcTypeClick;

      GrdCoefficients.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('LoadCase'));
      GrdCoefficients.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('CoefficientA'));
      GrdCoefficients.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('CoefficientB'));
      GrdCoefficients.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('CoefficientC'));
      GrdCoefficients.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('CoefficientD'));
      GrdCoefficients.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('RiskProportion'));
      GrdCoefficients.OnBeforeCellChange := OnStringGridCellDataHasChanged;
      GrdCoefficients.OnColEnter         := OnStringGridColEnter;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFMSubSystemsValidator.CreateDialog;
const OPNAME = 'TFMSubSystemsValidator.CreateDialog';
begin
  try
    FPanel  := TFMSubSystemsDialog.Create(FPanelOwner,FAppModules);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFMSubSystemsValidator.DestroyMemberObjects;
const OPNAME = 'TFMSubSystemsValidator.DestroyMemberObjects';
begin
  try
    inherited DestroyMemberObjects;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFMSubSystemsValidator.Initialise: boolean;
const OPNAME = 'TFMSubSystemsValidator.Initialise';
var
  lIndex          : integer;
  lYieldModelData : IYieldModelData;
  lChannelList    : IChannelList;
  lReservoirList  : IReservoirDataList;
  lChannel        : IGeneralFlowChannel;
  lReservoir      : IReservoirData;
  lMonth          : integer;
begin
  Result := inherited Initialise;
  try
    lYieldModelData := (FAppModules.Model.ModelData as IYieldModelData);
    lChannelList    := lYieldModelData.NetworkElementData.ChannelList;
    lReservoirList  := lYieldModelData.NetworkElementData.ReservoirList;
    with FMSubSystemsDialog do
    begin
      GrdRoutingChannels.ColWidths[0] := 80;
      CbxStartYear.Clear;
      CbxStartMonth.Clear;
      CbxEndYear.Clear;
      CbxEndMonth.Clear;
      for lIndex := 1900 to 3000 do
      begin
        CbxStartYear.Items.Add(IntToStr(lIndex));
        CbxEndYear.Items.Add(IntToStr(lIndex));
      end;
      for lIndex := 1 to 12 do
      begin
        CbxStartMonth.Items.Add(UpperCase(FormatSettings.ShortMonthNames[lIndex]));
        CbxEndMonth.Items.Add(UpperCase(FormatSettings.ShortMonthNames[lIndex]));
      end;
      with RgpFirmYield do
      begin
        Items.Clear;
        Items.Add(FAppModules.Language.GetString('PlanningGUI.NONFIRM'));
        Items.Add(FAppModules.Language.GetString('PlanningGUI.FIRM'));
      end;
      with RgpSupportCalcType do
      begin
        Items.Clear;
        Items.Add(FAppModules.Language.GetString('PlanningGUI.SupportMaxCapacity'));
        Items.Add(FAppModules.Language.GetString('PlanningGUI.SupportPriorityClass'));
      end;

      CbxChannel.Clear;
      CbxChannel.Items.AddObject('0 - None', TObject(0));
      CbxSupportChannel.Clear;
      CbxSupportChannel.Items.AddObject('0 - None', TObject(0));
      for lIndex := 0 to lChannelList.ChannelCount - 1 do
      begin
        lChannel := lChannelList.ChannelByIndex[lIndex];
        if (lChannel.ChannelType in [8,9]) then {Min-Max}
        begin
          CbxChannel.Items.AddObject(lChannel.ChannelName, TObject(lChannel.ChannelNumber));
          CbxSupportChannel.Items.AddObject(lChannel.ChannelName, TObject(lChannel.ChannelNumber));
        end;
      end;
      CbxChannel.Sorted := TRUE;
      CbxSupportChannel.Sorted := TRUE;

      ClbReservoirsInSubSystem.Clear;
      for lIndex := 0 to lReservoirList.ReservoirCount - 1 do
      begin
        lReservoir := lReservoirList.ReservoirByIndex[lIndex];
        ClbReservoirsInSubSystem.Items.AddObject(lReservoir.ReservoirConfigurationData.ReservoirName,
                                      TObject(lReservoir.ReservoirConfigurationData.ReservoirIdentifier));
      end;

      GrdDecisionCurveSet.ColWidths[0] := 55;
      GrdDecisionCurveSet.Cells[0, 0] := FAppModules.Language.GetString('PlanningGUI.Month');
      GrdDecisionCurveSet.Cells[0, 1] := FAppModules.Language.GetString('PlanningGUI.CurveSet');
      for lMonth := 1 to 12 do
      begin
        lIndex := (lMonth + 9) mod 12;
        if (lIndex = 0) then
          lIndex := 12;
        GrdDecisionCurveSet.Cells[lMonth, 0] := FormatSettings.ShortMonthNames[lIndex];
      end;

      GrdCoefficients.Cells[0, 0] := FAppModules.Language.GetString('PlanningGUI.LoadCase');
      GrdCoefficients.Cells[1, 0] := 'A';
      GrdCoefficients.Cells[2, 0] := 'B';
      GrdCoefficients.Cells[3, 0] := 'C';
      GrdCoefficients.Cells[4, 0] := 'D';
      GrdCoefficients.Cells[5, 0] := 'Y';

    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFMSubSystemsValidator.LanguageHasChanged: boolean;
const OPNAME = 'TFMSubSystemsValidator.LanguageHasChanged';
begin
  Result := False;
  try
    TabShetCaption := FAppModules.Language.GetString(FHeading);
    Result := inherited LanguageHasChanged;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFMSubSystemsValidator.ClearDataViewer;
const OPNAME = 'TFMSubSystemsValidator.ClearDataViewer';
var
  lIndex : integer;
begin
  inherited ClearDataViewer;
  try
    with FMSubSystemsDialog do
    begin
      EdtSubSystemName.Text        := '';
      CbxStartYear.ItemIndex       := -1;
      CbxStartMonth.ItemIndex      := -1;
      CbxEndYear.ItemIndex         := -1;
      CbxEndMonth.ItemIndex        := -1;
      CbxSubtracted.ItemIndex      := -1;
      CbxSupporting.ItemIndex      := -1;
      CbxSupportChannel.ItemIndex  := -1;
      EdtYieldShort.Text           := '';
      EdtLowestStreamFlow.Text     := '';
      EdtYieldLong.Text            := '';
      RgpFirmYield.ItemIndex       := -1;
      RgpSupportCalcType.ItemIndex := -1;
      for lIndex := 0 to ClbReservoirsInSubSystem.Items.Count - 1 do
        ClbReservoirsInSubSystem.Checked[lIndex] := FALSE;
      for lIndex := 1 to GrdRoutingChannels.RowCount - 1 do
        GrdRoutingChannels.Rows[lIndex].Clear;
      for lIndex := 1 to GrdCoefficients.RowCount - 1 do
        GrdCoefficients.Rows[lIndex].Clear;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFMSubSystemsValidator.PopulateDataViewer;
const OPNAME = 'TFMSubSystemsValidator.PopulateDataViewer';
begin
  inherited PopulateDataViewer;
  try
    ClearDataViewer;
    RePopulateDataViewer;
    DoContextValidation(dvtSubSystemPropAll);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFMSubSystemsValidator.RePopulateDataViewer;
const OPNAME = 'TFMSubSystemsValidator.RePopulateDataViewer';
var
  lAllocDef           : IAllocationDefinition;
  lNrOfSubSystems     : integer;
  lSubSystem          : ISubSystem;
  lIndex              : integer;
  lWidth              : integer;
  lIntVal             : integer;
  lMonth              : integer;
  lSelectedIndex      : integer;
  LFieldProperty      : TAbstractFieldProperty;
begin
  try
    if (FIdentifier > 0) then
    begin
      lAllocDef := (FAppModules.Model.ModelData as IPlanningModelData).
                     AllocationDefinitionsList.AllocationDefinitionByID[FIdentifier];
      if (lAllocDef <> nil) then
      begin
        with FMSubSystemsDialog do
        begin
          lNrOfSubSystems := lAllocDef.NrOfSubSystems;
          TrvSubSystems.Items.Clear;
          lSelectedIndex := 0;
          for lIndex := 0 to lNrOfSubSystems - 1 do
          begin
            lSubSystem := lAllocDef.SubSystemByIndex[lIndex];
            if (FSubSystemID = 0) then
              FSubSystemID := lSubSystem.SubSystemID;
            TrvSubSystems.Items.AddChildObject(nil, lSubSystem.Name, TObject(lSubSystem.SubSystemID));
            if (lSubSystem.SubSystemID = FSubSystemID) then
              lSelectedIndex := lIndex;
          end;
          LFieldProperty    := FAppModules.FieldProperties.FieldProperty('FlowConstraints');
          GrdStartStoragePerc.ColCount := lAllocDef.NrOfStartingPercentages;
          lWidth := 3;
          for lIndex := 0 to GrdStartStoragePerc.ColCount - 1 do
            lWidth := lWidth + GrdStartStoragePerc.ColWidths[lIndex] + 1;
          GrdStartStoragePerc.Width := lWidth;
          for lIndex := 1 to lAllocDef.NrOfStartingPercentages do
            GrdStartStoragePerc.Cells[lIndex-1, 0] := Format(LFieldProperty.FormatStringGrid{'%6.2f'}, [lAllocDef.StartingPercentageByIndex[lIndex]]);

          for lMonth := 1 to 12 do
          begin
            lIntVal := lAllocDef.DecisionCurveSetByMonth[lMonth];
            if (lIntVal > 0) then
              GrdDecisionCurveSet.Cells[lMonth, 1] := IntToStr(lIntVal)
            else
              GrdDecisionCurveSet.Cells[lMonth, 1] := '';
          end;

          GrdCoefficients.RowCount := lAllocDef.NrOfLoadCases + 1;
          if (GrdCoefficients.RowCount > 1) then
            GrdCoefficients.FixedRows := 1;
          GrdCoefficients.Height   := 3 + (GrdCoefficients.DefaultRowHeight + 1) * (GrdCoefficients.RowCount);

          GrdStartStoragePerc.Col := 0;
          GrdDecisionCurveSet.Col := 1;
          ClearDataViewer;
          if (lNrOfSubSystems > 0) then
          begin
            PgcSubSystems.Enabled := TRUE;
            if (TrvSubSystems.Selected.Index <> lSelectedIndex) then
              TrvSubSystems.Items[lSelectedIndex].Selected := TRUE
            else
              PopulateSubSystem(lAllocDef);
          end
          else
            PgcSubSystems.Enabled := FALSE;
        end;
      end
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFMSubSystemsValidator.OnTrvSubSystemsChange (Sender : TObject;
                                                        ANode  : TTreeNode);
const OPNAME = 'TFMSubSystemsValidator.OnTrvSubSystemsChange';
var
  lAllocDef : IAllocationDefinition;
begin
  try
    if (FIdentifier > 0) then
    begin
      lAllocDef := (FAppModules.Model.ModelData as IPlanningModelData).
                     AllocationDefinitionsList.AllocationDefinitionByID[FIdentifier];
      if (lAllocDef <> nil) then
      begin
        FSubSystemID := Integer(ANode.Data);
        PopulateSubSystem(lAllocDef);
        DoContextValidation(dvtSubSystemPropAll);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFMSubSystemsValidator.PopulateSubSystem (AAllocDef : IAllocationDefinition);
const OPNAME = 'TFMSubSystemsValidator.PopulateSubSystem';
var
  lOtherSystem    : ISubSystem;
  lIndex          : integer;
  lElementNrs     : TStringList;
  lEntityNr       : integer;
  lSubSystem      : ISubSystem;
  lChannelList    : IChannelList;
  lChannel        : IGeneralFlowChannel;
  LFieldProperty  : TAbstractFieldProperty;

begin
  try
    with FMSubSystemsDialog do
    begin
      ClearDataViewer;
      CbxChannel.Visible := FALSE;
      if (FSubSystemID > 0) then
        lSubSystem := AAllocDef.SubSystemByID[FSubSystemID]
      else
        lSubSystem := nil;
      if (lSubSystem <> nil) then
      begin
        EdtSubSystemName.Text := lSubSystem.Name;
        if (lSubSystem.StartYear <> 0) then
          CbxStartYear.SetFieldIndex(CbxStartYear.Items.IndexOf(IntToStr(lSubSystem.StartYear)));
        if (lSubSystem.StartMonth <> 0) then
          CbxStartMonth.SetFieldIndex(lSubSystem.StartMonth - 1);
        if (lSubSystem.EndYear <> 0) then
          CbxEndYear.SetFieldIndex(CbxEndYear.Items.IndexOf(IntToStr(lSubSystem.EndYear)));
        if (lSubSystem.EndMonth <> 0) then
          CbxEndMonth.SetFieldIndex(lSubSystem.EndMonth - 1);

        PopulateSubSystemComboBoxes(AAllocDef, lSubSystem.SubSystemID);
        lOtherSystem := AAllocDef.SubSystemByID[lSubSystem.SubtractID];
        if (lOtherSystem <> nil) then
          CbxSubtracted.ItemIndex := CbxSubtracted.Items.IndexOf(lOtherSystem.Name)
        else
          CbxSubtracted.ItemIndex := 0;
        lOtherSystem := AAllocDef.SubSystemByID[lSubSystem.SupportID];
        if (lOtherSystem <> nil) then
          CbxSupporting.ItemIndex := CbxSupporting.Items.IndexOf(lOtherSystem.Name)
        else
          CbxSupporting.ItemIndex := 0;

        lChannelList := (FAppModules.Model.ModelData as IYieldModelData).NetworkElementData.ChannelList;
        lChannel := lChannelList.ChannelByChannelNumber[lSubSystem.SupportChannelNr];
        if (lChannel <> nil) then
          CbxSupportChannel.ItemIndex := CbxSupportChannel.Items.IndexOf(lChannel.ChannelName)
        else
          CbxSupportChannel.ItemIndex := 0;

        LFieldProperty           := FAppModules.FieldProperties.FieldProperty('FlowConstraints');
        EdtYieldShort.Text       := Format(LFieldProperty.FormatStringGrid {'%6.2f'},
                                           [lSubSystem.ShortTermYield]);
        EdtLowestStreamFlow.Text := Format(LFieldProperty.FormatStringGrid{'%6.2f'},
                                           [lSubSystem.LowestStreamFlow]);
        EdtYieldLong.Text        := Format(LFieldProperty.FormatStringGrid{'%6.2f'},
                                           [lSubSystem.LongTermYield]);
        if (lSubSystem.FirmYield) then
          RgpFirmYield.ItemIndex  := 1
        else
          RgpFirmYield.ItemIndex  := 0;
        lElementNrs := TStringList.Create;
        try
          lElementNrs.CommaText := lSubSystem.ReservoirNrs;
          for lIndex := 0 to ClbReservoirsInSubSystem.Items.Count - 1 do
          begin
            lEntityNr := Integer(ClbReservoirsInSubSystem.Items.Objects[lIndex]);
            ClbReservoirsInSubSystem.Checked[lIndex] := lElementNrs.IndexOf(IntToStr(lEntityNr)) >= 0;
          end;

          if (lSubSystem.SupportCalcType < RgpSupportCalcType.Items.Count) then
            RgpSupportCalcType.ItemIndex := lSubSystem.SupportCalcType;
          GrdRoutingChannels.Cells[1,0] := FAppModules.Language.GetString('PlanningGUI.RoutingChannels');
          if (lSubSystem.SupportCalcType = 0) OR (AAllocDef.NrOfReliabilityClasses > 5) then
            GrdRoutingChannels.RowCount := 5
          else
            GrdRoutingChannels.RowCount:= 1 + AAllocDef.NrOfReliabilityClasses;
          for lIndex := 1 to GrdRoutingChannels.RowCount do
          begin
            if (lSubSystem.SupportCalcType = 0) then
              GrdRoutingChannels.Cells[0, lIndex] := IntToStr(lIndex)
            else
              GrdRoutingChannels.Cells[0, lIndex] := '1:' + IntToStr(AAllocDef.RecurrenceIntervalByIndex[lIndex]);
            lEntityNr := lSubSystem.RoutingChannelNrByIndex[lIndex];
            if (lEntityNr = 0) then
              GrdRoutingChannels.Cells[1, lIndex] := '0 - None'
            else
            begin
              lChannel := lChannelList.ChannelByChannelNumber[lEntityNr];
              if (lChannel = nil) then
                GrdRoutingChannels.Cells[1, lIndex] := 'Invalid'
              else
                GrdRoutingChannels.Cells[1, lIndex] := lChannel.ChannelName;
            end;
          end;
        finally
          FreeAndNil(lElementNrs);
        end;
        PopulateCoefficients(AAllocDef, lSubSystem, -1, -1);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFMSubSystemsValidator.OnStartPercSelectionChange (Sender        : TObject;
                                                             ACol, ARow    : Integer;
                                                             var CanSelect : Boolean);
const OPNAME = 'TFMSubSystemsValidator.OnStartPercSelectionChange';
var
  lAllocDef  : IAllocationDefinition;
  lSubSystem : ISubSystem;
begin
  try
    if (FIdentifier > 0) then
    begin
      lAllocDef := (FAppModules.Model.ModelData as IPlanningModelData).
                     AllocationDefinitionsList.AllocationDefinitionByID[FIdentifier];
      if (lAllocDef <> nil) AND (FSubSystemID > 0) then
      begin
        lSubSystem := lAllocDef.SubSystemByID[FSubSystemID];
        if (lSubSystem <> nil) then
        begin
          if (Sender = FMSubSystemsDialog.GrdStartStoragePerc) then
            PopulateCoefficients(lAllocDef, lSubSystem, ACol, -1)
          else
          if (Sender = FMSubSystemsDialog.GrdDecisionCurveSet) then
            PopulateCoefficients(lAllocDef, lSubSystem, -1, ACol);
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFMSubSystemsValidator.PopulateCoefficients (AAllocDef  : IAllocationDefinition;
                                                       ASubSystem : ISubSystem;
                                                       APercCol   : integer;
                                                       ACurveCol  : integer);
const OPNAME = 'TFMSubSystemsValidator.PopulateCoefficients';
var
  lCurveIdx      : integer;
  lPercIdx       : integer;
  lCaseIdx       : integer;
  lCoeffData     : ICoefficient;
  LFieldProperty : TAbstractFieldProperty;
begin
  try
    with FMSubSystemsDialog do
    begin
      if (AAllocDef.NrOfStartingPercentages > 0) AND (AAllocDef.NrOfCurveSets > 0) then
      begin
        if (APercCol < 0) then
          APercCol := GrdStartStoragePerc.Col;
        if (ACurveCol < 0) then
          ACurveCol := GrdDecisionCurveSet.Col;
        lPercIdx   := APercCol + 1;
        lCurveIdx  := StrToInt(GrdDecisionCurveSet.Cells[ACurveCol, 1]);
        LFieldProperty := FAppModules.FieldProperties.FieldProperty('CoefficientA');
        for lCaseIdx := 1 to AAllocDef.NrOfLoadCases do
        begin
          GrdCoefficients.Rows[lCaseIdx].Clear;
          lCoeffData := ASubSystem.CoefficientByPercCurveCase[lPercIdx, lCurveIdx, lCaseIdx];
          GrdCoefficients.Cells[0, lCaseIdx] := Format(LFieldProperty.FormatStringGrid {'%12.6f'},
                                                        [lCoeffData.TargetDraft]);
          GrdCoefficients.Cells[1, lCaseIdx] := Format(LFieldProperty.FormatStringGrid {'%12.6f'},
                                                        [lCoeffData.CoefficientA]);
          GrdCoefficients.Cells[2, lCaseIdx] := Format(LFieldProperty.FormatStringGrid {'%12.6f'},
                                                        [lCoeffData.CoefficientB]);
          GrdCoefficients.Cells[3, lCaseIdx] := Format(LFieldProperty.FormatStringGrid {'%12.6f'},
                                                        [lCoeffData.CoefficientC]);
          GrdCoefficients.Cells[4, lCaseIdx] := Format(LFieldProperty.FormatStringGrid {'%12.6f'},
                                                        [lCoeffData.CoefficientD]);
          GrdCoefficients.Cells[5, lCaseIdx] := Format(LFieldProperty.FormatStringGrid {'%12.6f'},
                                                        [lCoeffData.Risk]);
        end;
      end;
    end;  
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFMSubSystemsValidator.PopulateSubSystemComboBoxes (AAllocDef    : IAllocationDefinition;
                                                              ASubSystemID : integer);
const OPNAME = 'TFMSubSystemsValidator.PopulateSubSystemComboBoxes';
var
  lIndex     : integer;
  lSubSystem : ISubSystem;
begin
  try
    with FMSubSystemsDialog do
    begin
      CbxSubtracted.Clear;
      CbxSupporting.Clear;
      CbxSubtracted.Items.AddObject(FAppModules.Language.GetString('PlanningGUI.None'), TObject(0));
      CbxSupporting.Items.AddObject(FAppModules.Language.GetString('PlanningGUI.None'), TObject(0));
      for lIndex := 0 to AAllocDef.NrOfSubSystems - 1 do
      begin
        lSubSystem := AAllocDef.SubSystemByIndex[lIndex];
        if (lSubSystem.SubSystemID <> ASubSystemID) then
        begin
          CbxSubtracted.Items.AddObject(lSubSystem.Name, TObject(lSubSystem.SubSystemID));
          CbxSupporting.Items.AddObject(lSubSystem.Name, TObject(lSubSystem.SubSystemID));
        end;  
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFMSubSystemsValidator.SaveState: boolean;
const OPNAME = 'TFMSubSystemsValidator.SaveState';
begin
  Result := inherited SaveState;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFMSubSystemsValidator.FMSubSystemsDialog:TFMSubSystemsDialog;
const OPNAME = 'TFMSubSystemsValidator.FMSubSystemsDialog';
begin
  Result := Nil;
  try
    if (FPanel <> nil) then
      Result := TFMSubSystemsDialog(FPanel);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFMSubSystemsValidator.StudyDataHasChanged(AContext: TChangeContext; AFieldName,AOldValue,ANewValue: string): boolean;
const OPNAME = 'TFMSubSystemsValidator.StudyDataHasChanged';
begin
  Result := inherited StudyDataHasChanged(AContext,AFieldName,AOldValue,ANewValue);
  try
    if (AFieldName = 'NrOfStartStoragePercs') OR
       (AFieldName = 'NrOfCurveSets') OR
       (AFieldName = 'NrOfLoadCases') OR
       (AFieldName = 'MonthCurveSet') then
      RePopulateDataViewer;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFMSubSystemsValidator.StudyHasChanged: boolean;
const OPNAME = 'TFMSubSystemsValidator.StudyHasChanged';
begin
  Result := inherited StudyHasChanged;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFMSubSystemsValidator.DoAddSubSystem (Sender: TObject);
const OPNAME = 'TFMSubSystemsValidator.DoAddSubSystem';
var
  lAllocDef  : IAllocationDefinition;
  lSubSystem : ISubSystem;
  lCount     : integer;
begin
  try
    lAllocDef := (FAppModules.Model.ModelData as IPlanningModelData).
                   AllocationDefinitionsList.AllocationDefinitionByID[FIdentifier];
    if (lAllocDef <> nil) then
    begin
      lCount       := lAllocDef.NrOfSubSystems;
      lSubSystem   := lAllocDef.NewSubSystem;
      FSubSystemID := lSubSystem.SubSystemID;
      RePopulateDataViewer;
      DoContextValidation(dvtSubSystemPropAll);
      FAppModules.Model.StudyDataHasChanged
        (sdccEdit, 'NrOfSubSystems', IntToStr(lCount), IntToStr(lAllocDef.NrOfSubSystems));
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFMSubSystemsValidator.DoDeleteSubSystem(Sender: TObject);
const OPNAME = 'TFMSubSystemsValidator.DoDeleteSubSystem';
var
  lAllocDef  : IAllocationDefinition;
  lCount     : integer;
begin
  try
    lAllocDef := (FAppModules.Model.ModelData as IPlanningModelData).
                   AllocationDefinitionsList.AllocationDefinitionByID[FIdentifier];
    if (lAllocDef <> nil) AND (FSubSystemID > 0) then
    begin
      lCount := lAllocDef.NrOfSubSystems;
      if (lAllocDef.RemoveSubSystem(FSubSystemID)) then
      begin
        FSubSystemID := 0;
        RePopulateDataViewer;
        DoContextValidation(dvtSubSystemPropAll);
        FAppModules.Model.StudyDataHasChanged
          (sdccEdit, 'NrOfSubSystems', IntToStr(lCount), IntToStr(lAllocDef.NrOfSubSystems));
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFMSubSystemsValidator.OnEditControlEnter(Sender: TObject);
const OPNAME = 'TFMSubSystemsValidator.OnEditControlEnter';
begin
  inherited OnEditControlEnter(Sender);
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFMSubSystemsValidator.OnEditControltExit (Sender : TObject);
const OPNAME = 'TFMSubSystemsValidator.OnEditControltExit';
begin
  inherited OnEditControltExit(Sender);
  try
    with FMSubSystemsDialog do
    begin
      if ((Sender = EdtSubSystemName) AND
          (EdtSubSystemName.HasValueChanged)) then
        UpdateSubSystemName
      else
      if ((Sender = CbxStartYear) AND
          (CbxStartYear.HasValueChanged)) then
        UpdateSubSystemStartYear
      else
      if ((Sender = CbxStartMonth) AND
          (CbxStartMonth.HasValueChanged)) then
        UpdateSubSystemStartMonth
      else
      if ((Sender = CbxEndYear) AND
          (CbxEndYear.HasValueChanged)) then
        UpdateSubSystemEndYear
      else
      if ((Sender = CbxEndMonth) AND
          (CbxEndMonth.HasValueChanged)) then
        UpdateSubSystemEndMonth
      else
      if ((Sender = CbxSubtracted) AND
          (CbxSubtracted.HasValueChanged)) then
        UpdateSubtractedSubSystem
      else
      if ((Sender = CbxSupporting) AND
          (CbxSupporting.HasValueChanged)) then
        UpdateSupportingSubSystem
      else
      if ((Sender = CbxSupportChannel) AND
          (CbxSupportChannel.HasValueChanged)) then
        UpdateSupportChannel
      else
      if ((Sender = EdtYieldShort) AND
          (EdtYieldShort.HasValueChanged)) then
        UpdateShortTermYield
      else
      if ((Sender = EdtYieldLong) AND
          (EdtYieldLong.HasValueChanged)) then
        UpdateLongTermYield
      else
      if ((Sender = EdtLowestStreamFlow) AND
          (EdtLowestStreamFlow.HasValueChanged)) then
        UpdateLowestStreamFlow
      else
      if (Sender = CbxChannel)  then
        UpdateRoutingChannels;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFMSubSystemsValidator.OnRgpFirmYieldClick(Sender: TObject);
const OPNAME = 'TFMSubSystemsValidator.OnRgpFirmYieldClick';
begin
  try
    if(FMSubSystemsDialog.RgpFirmYield.HasValueChanged) then
      UpdateFirmYield;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFMSubSystemsValidator.OnClbReservoirsInSubSystemClick(Sender: TObject);
const OPNAME = 'TFMSubSystemsValidator.OnClbReservoirsInSubSystemClick';
begin
  try
    if(FMSubSystemsDialog.ClbReservoirsInSubSystem.HasValueChanged) then
      UpdateReservoirsInSubSystem;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFMSubSystemsValidator.OnRgpSupportCalcTypeClick(Sender: TObject);
const OPNAME = 'TFMSubSystemsValidator.OnRgpSupportCalcTypeClick';
begin
  try
    if(FMSubSystemsDialog.RgpSupportCalcType.HasValueChanged) then
      UpdateSupportCalcType;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFMSubSystemsValidator.OnStringGridCellDataHasChanged
                                                           (ASender    : TObject;
                                                            ACol, ARow : integer);
const OPNAME = 'TFMSubSystemsValidator.OnStringGridCellDataHasChanged';
begin
  inherited OnStringGridCellDataHasChanged(ASender, ACol, ARow);
  try
    with FMSubSystemsDialog do
    begin
      if (GrdCoefficients = ASender) then
        UpdateCoefficients(ACol, ARow, Trim(GrdCoefficients.Cells[ACol, ARow]));
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFMSubSystemsValidator.UpdateSubSystemName;
const OPNAME = 'TFMSubSystemsValidator.UpdateSubSystemName';
var
  lAllocDef   : IAllocationDefinition;
  lSubSystem  : ISubSystem;
  lMessage    : string;
begin
  try
    lAllocDef := (FAppModules.Model.ModelData as IPlanningModelData).
                   AllocationDefinitionsList.AllocationDefinitionByID[FIdentifier];
    if (lAllocDef <> nil) AND (FSubSystemID > 0) then
    begin
      lSubSystem := lAllocDef.SubSystemByID[FSubSystemID];
      if (lSubSystem <> nil) then
      begin
        with FMSubSystemsDialog do
        begin
          if (FAppModules.FieldProperties.ValidateFieldProperty(
              EdtSubSystemName.FieldProperty.FieldName, Trim(EdtSubSystemName.Text), lMessage)) then
          begin
            EdtSubSystemName.FieldValidationError := lMessage;
            lSubSystem.Name := Trim(EdtSubSystemName.Text);
            EdtSubSystemName.SetFieldValue(lSubSystem.Name);
            RePopulateDataViewer;
            DoContextValidation(dvtSubSystemName);
          end
          else
            EdtSubSystemName.FieldValidationError := lMessage;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFMSubSystemsValidator.UpdateSubSystemStartYear;
const OPNAME = 'TFMSubSystemsValidator.UpdateSubSystemStartYear';
var
  lAllocDef  : IAllocationDefinition;
  lMessage   : string;
  lValue     : integer;
  lSubSystem : ISubSystem;
begin
  try
    lAllocDef := (FAppModules.Model.ModelData as IPlanningModelData).
                   AllocationDefinitionsList.AllocationDefinitionByID[FIdentifier];
    if (lAllocDef <> nil) AND (FSubSystemID > 0) then
    begin
      lSubSystem := lAllocDef.SubSystemByID[FSubSystemID];
      if (lSubSystem <> nil) then
      begin
        with FMSubSystemsDialog do
        begin
          if (CbxStartYear.ItemIndex >= 0) then
          begin
            lValue := StrToInt(CbxStartYear.Items[CbxStartYear.ItemIndex]);
            if (FAppModules.FieldProperties.ValidateFieldProperty(
                CbxStartYear.FieldProperty.FieldName,
                IntToStr(lValue), lMessage)) then
            begin
              lSubSystem.StartYear := lValue;
              CbxStartYear.SetFieldIndex
                (CbxStartYear.Items.IndexOf(IntToStr(lSubSystem.StartYear)));
              DoContextValidation(dvtSubSystemStartYear);
            end
            else
              CbxStartYear.ValidationError := lMessage;
          end;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFMSubSystemsValidator.UpdateSubSystemStartMonth;
const OPNAME = 'TFMSubSystemsValidator.UpdateSubSystemStartMonth';
var
  lAllocDef  : IAllocationDefinition;
  lMessage   : string;
  lValue     : integer;
  lSubSystem : ISubSystem;
begin
  try
    lAllocDef := (FAppModules.Model.ModelData as IPlanningModelData).
                   AllocationDefinitionsList.AllocationDefinitionByID[FIdentifier];
    if (lAllocDef <> nil) AND (FSubSystemID > 0) then
    begin
      lSubSystem := lAllocDef.SubSystemByID[FSubSystemID];
      if (lSubSystem <> nil) then
      begin
        with FMSubSystemsDialog do
        begin
          if (CbxStartMonth.ItemIndex >= 0) then
          begin
            lValue := CbxStartMonth.ItemIndex + 1;
            if (FAppModules.FieldProperties.ValidateFieldProperty(
                CbxStartMonth.FieldProperty.FieldName,
                IntToStr(lValue), lMessage)) then
            begin
              lSubSystem.StartMonth := lValue;
              CbxStartMonth.SetFieldIndex(lSubSystem.StartMonth - 1);
              DoContextValidation(dvtSubSystemStartMonth);
            end
            else
              CbxStartMonth.ValidationError := lMessage;
          end;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFMSubSystemsValidator.UpdateSubSystemEndYear;
const OPNAME = 'TFMSubSystemsValidator.UpdateSubSystemEndYear';
var
  lAllocDef  : IAllocationDefinition;
  lMessage   : string;
  lValue     : integer;
  lSubSystem : ISubSystem;
begin
  try
    lAllocDef := (FAppModules.Model.ModelData as IPlanningModelData).
                   AllocationDefinitionsList.AllocationDefinitionByID[FIdentifier];
    if (lAllocDef <> nil) AND (FSubSystemID > 0) then
    begin
      lSubSystem := lAllocDef.SubSystemByID[FSubSystemID];
      if (lSubSystem <> nil) then
      begin
        with FMSubSystemsDialog do
        begin
          if (CbxEndYear.ItemIndex >= 0) then
          begin
            lValue := StrToInt(CbxEndYear.Items[CbxEndYear.ItemIndex]);
            if (FAppModules.FieldProperties.ValidateFieldProperty(
                CbxEndYear.FieldProperty.FieldName,
                IntToStr(lValue), lMessage)) then
            begin
              lSubSystem.EndYear := lValue;
              CbxEndYear.SetFieldIndex
                (CbxEndYear.Items.IndexOf(IntToStr(lSubSystem.EndYear)));
              DoContextValidation(dvtSubSystemEndYear);
            end
            else
              CbxEndYear.ValidationError := lMessage;
          end;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFMSubSystemsValidator.UpdateSubSystemEndMonth;
const OPNAME = 'TFMSubSystemsValidator.UpdateSubSystemEndMonth';
var
  lAllocDef  : IAllocationDefinition;
  lMessage   : string;
  lValue     : integer;
  lSubSystem : ISubSystem;
begin
  try
    lAllocDef := (FAppModules.Model.ModelData as IPlanningModelData).
                   AllocationDefinitionsList.AllocationDefinitionByID[FIdentifier];
    if (lAllocDef <> nil) AND (FSubSystemID > 0) then
    begin
      lSubSystem := lAllocDef.SubSystemByID[FSubSystemID];
      if (lSubSystem <> nil) then
      begin
        with FMSubSystemsDialog do
        begin
          if (CbxEndMonth.ItemIndex >= 0) then
          begin
            lValue := CbxEndMonth.ItemIndex + 1;
            if (FAppModules.FieldProperties.ValidateFieldProperty(
                CbxEndMonth.FieldProperty.FieldName,
                IntToStr(lValue), lMessage)) then
            begin
              lSubSystem.EndMonth := lValue;
              CbxEndMonth.SetFieldIndex(lSubSystem.EndMonth - 1);
              DoContextValidation(dvtSubSystemEndMonth);
            end
            else
              CbxEndMonth.ValidationError := lMessage;
          end;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFMSubSystemsValidator.UpdateSubtractedSubSystem;
const OPNAME = 'TFMSubSystemsValidator.UpdateSubtractedSubSystem';
var
  lAllocDef    : IAllocationDefinition;
  lMessage     : string;
  lValue       : integer;
  lSubSystem   : ISubSystem;
  lOtherSystem : ISubSystem;
begin
  try
    lAllocDef := (FAppModules.Model.ModelData as IPlanningModelData).
                   AllocationDefinitionsList.AllocationDefinitionByID[FIdentifier];
    if (lAllocDef <> nil) AND (FSubSystemID > 0) then
    begin
      lSubSystem := lAllocDef.SubSystemByID[FSubSystemID];
      if (lSubSystem <> nil) then
      begin
        with FMSubSystemsDialog do
        begin
          if (CbxSubtracted.ItemIndex >= 0) then
          begin
            lValue := Integer(CbxSubtracted.Items.Objects[CbxSubtracted.ItemIndex]);
            if (FAppModules.FieldProperties.ValidateFieldProperty(
                CbxSubtracted.FieldProperty.FieldName,
                IntToStr(lValue), lMessage)) then
            begin
              lSubSystem.SubtractID := lValue;

              lOtherSystem := lAllocDef.SubSystemByID[lSubSystem.SubtractID];
              if (lOtherSystem <> nil) then
                CbxSubtracted.ItemIndex := CbxSubtracted.Items.IndexOf(lOtherSystem.Name)
              else
                CbxSubtracted.ItemIndex := 0;
              DoContextValidation(dvtSubtractedSubSystemID);
            end
            else
              CbxSubtracted.ValidationError := lMessage;
          end;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFMSubSystemsValidator.UpdateSupportingSubSystem;
const OPNAME = 'TFMSubSystemsValidator.UpdateSupportingSubSystem';
var
  lAllocDef    : IAllocationDefinition;
  lMessage     : string;
  lValue       : integer;
  lSubSystem   : ISubSystem;
  lOtherSystem : ISubSystem;
begin
  try
    lAllocDef := (FAppModules.Model.ModelData as IPlanningModelData).
                   AllocationDefinitionsList.AllocationDefinitionByID[FIdentifier];
    if (lAllocDef <> nil) AND (FSubSystemID > 0) then
    begin
      lSubSystem := lAllocDef.SubSystemByID[FSubSystemID];
      if (lSubSystem <> nil) then
      begin
        with FMSubSystemsDialog do
        begin
          if (CbxSupporting.ItemIndex >= 0) then
          begin
            lValue := Integer(CbxSupporting.Items.Objects[CbxSupporting.ItemIndex]);
            if (FAppModules.FieldProperties.ValidateFieldProperty(
                CbxSupporting.FieldProperty.FieldName,
                IntToStr(lValue), lMessage)) then
            begin
              lSubSystem.SupportID := lValue;

              lOtherSystem := lAllocDef.SubSystemByID[lSubSystem.SupportID];
              if (lOtherSystem <> nil) then
                CbxSupporting.ItemIndex := CbxSupporting.Items.IndexOf(lOtherSystem.Name)
              else
                CbxSupporting.ItemIndex := 0;
              DoContextValidation(dvtSupportingSubSystemID);
            end
            else
              CbxSupporting.ValidationError := lMessage;
          end;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFMSubSystemsValidator.UpdateSupportChannel;
const OPNAME = 'TFMSubSystemsValidator.UpdateSupportChannel';
var
  lAllocDef    : IAllocationDefinition;
  lMessage     : string;
  lValue       : integer;
  lSubSystem   : ISubSystem;
  lChannelList : IChannelList;
  lChannel     : IGeneralFlowChannel;
begin
  try
    lAllocDef := (FAppModules.Model.ModelData as IPlanningModelData).
                   AllocationDefinitionsList.AllocationDefinitionByID[FIdentifier];
    if (lAllocDef <> nil) AND (FSubSystemID > 0) then
    begin
      lSubSystem := lAllocDef.SubSystemByID[FSubSystemID];
      if (lSubSystem <> nil) then
      begin
        with FMSubSystemsDialog do
        begin
          if (CbxSupportChannel.ItemIndex >= 0) then
          begin
            lValue := Integer(CbxSupportChannel.Items.Objects[CbxSupportChannel.ItemIndex]);
            if (FAppModules.FieldProperties.ValidateFieldProperty(
                CbxSupportChannel.FieldProperty.FieldName,
                IntToStr(lValue), lMessage)) then
            begin
              lSubSystem.SupportChannelNr := lValue;
              lChannelList := (FAppModules.Model.ModelData as IYieldModelData).NetworkElementData.ChannelList;
              lChannel := lChannelList.ChannelByChannelNumber[lSubSystem.SupportChannelNr];
              if (lChannel <> nil) then
                CbxSupportChannel.ItemIndex := CbxSupportChannel.Items.IndexOf(lChannel.ChannelName)
              else
                CbxSupportChannel.ItemIndex := 0;
              DoContextValidation(dvtSupportingSubSystemID);
            end
            else
              CbxSupportChannel.ValidationError := lMessage;
          end;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFMSubSystemsValidator.UpdateShortTermYield;
const OPNAME = 'TFMSubSystemsValidator.UpdateShortTermYield';
var
  lAllocDef  : IAllocationDefinition;
  lSubSystem : ISubSystem;
  lMessage   : string;
begin
  try
    lAllocDef := (FAppModules.Model.ModelData as IPlanningModelData).
                   AllocationDefinitionsList.AllocationDefinitionByID[FIdentifier];
    if (lAllocDef <> nil) AND (FSubSystemID > 0) then
    begin
      lSubSystem := lAllocDef.SubSystemByID[FSubSystemID];
      if (lSubSystem <> nil) then
      begin
        with FMSubSystemsDialog do
        begin
          if (FAppModules.FieldProperties.ValidateFieldProperty(
              EdtYieldShort.FieldProperty.FieldName, Trim(EdtYieldShort.Text), lMessage)) then
          begin
            EdtYieldShort.FieldValidationError := lMessage;
            lSubSystem.ShortTermYield := StrToFloat(Trim(EdtYieldShort.Text));
            EdtYieldShort.SetFieldValue(lSubSystem.ShortTermYield);
            DoContextValidation(dvtShortTermYield);
          end
          else
            EdtYieldShort.FieldValidationError := lMessage;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFMSubSystemsValidator.UpdateLongTermYield;
const OPNAME = 'TFMSubSystemsValidator.UpdateLongTermYield';
var
  lAllocDef  : IAllocationDefinition;
  lSubSystem : ISubSystem;
  lMessage   : string;
begin
  try
    lAllocDef := (FAppModules.Model.ModelData as IPlanningModelData).
                   AllocationDefinitionsList.AllocationDefinitionByID[FIdentifier];
    if (lAllocDef <> nil) AND (FSubSystemID > 0) then
    begin
      lSubSystem := lAllocDef.SubSystemByID[FSubSystemID];
      if (lSubSystem <> nil) then
      begin
        with FMSubSystemsDialog do
        begin
          if (FAppModules.FieldProperties.ValidateFieldProperty(
              EdtYieldLong.FieldProperty.FieldName, Trim(EdtYieldLong.Text), lMessage)) then
          begin
            EdtYieldLong.FieldValidationError := lMessage;
            lSubSystem.LongTermYield := StrToFloat(Trim(EdtYieldLong.Text));
            EdtYieldLong.SetFieldValue(lSubSystem.LongTermYield);
            DoContextValidation(dvtLongTermYield);
          end
          else
            EdtYieldLong.FieldValidationError := lMessage;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFMSubSystemsValidator.UpdateLowestStreamFlow;
const OPNAME = 'TFMSubSystemsValidator.UpdateLowestStreamFlow';
var
  lAllocDef  : IAllocationDefinition;
  lSubSystem : ISubSystem;
  lMessage   : string;
begin
  try
    lAllocDef := (FAppModules.Model.ModelData as IPlanningModelData).
                   AllocationDefinitionsList.AllocationDefinitionByID[FIdentifier];
    if (lAllocDef <> nil) AND (FSubSystemID > 0) then
    begin
      lSubSystem := lAllocDef.SubSystemByID[FSubSystemID];
      if (lSubSystem <> nil) then
      begin
        with FMSubSystemsDialog do
        begin
          if (FAppModules.FieldProperties.ValidateFieldProperty(
              EdtLowestStreamFlow.FieldProperty.FieldName, Trim(EdtLowestStreamFlow.Text), lMessage)) then
          begin
            EdtLowestStreamFlow.FieldValidationError := lMessage;
            lSubSystem.LowestStreamFlow := StrToFloat(Trim(EdtLowestStreamFlow.Text));
            EdtLowestStreamFlow.SetFieldValue(lSubSystem.LowestStreamFlow);
            DoContextValidation(dvtLowestStreamFlow);
          end
          else
            EdtLowestStreamFlow.FieldValidationError := lMessage;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFMSubSystemsValidator.UpdateFirmYield;
const OPNAME = 'TFMSubSystemsValidator.UpdateFirmYield';
var
  lAllocDef  : IAllocationDefinition;
  lSubSystem : ISubSystem;
  lMessage   : string;
  lValue     : string;
begin
  try
    lAllocDef := (FAppModules.Model.ModelData as IPlanningModelData).
                   AllocationDefinitionsList.AllocationDefinitionByID[FIdentifier];
    if (lAllocDef <> nil) AND (FSubSystemID > 0) then
    begin
      lSubSystem := lAllocDef.SubSystemByID[FSubSystemID];
      if (lSubSystem <> nil) then
      begin
        with FMSubSystemsDialog do
        begin
          if (lSubSystem.FirmYield AND (RgpFirmYield.ItemIndex <> 1)) OR
             ((NOT lSubSystem.FirmYield) AND (RgpFirmYield.ItemIndex <> 0)) then
          begin
            if (RgpFirmYield.ItemIndex = 0) then
              lValue := 'N'
            else
              lValue := 'Y';
            if (FAppModules.FieldProperties.ValidateFieldProperty(
                RgpFirmYield.FieldProperty.FieldName, lValue, lMessage)) then
            begin
              RgpFirmYield.ValidationError := lMessage;
              lSubSystem.FirmYield := (lValue = 'Y');
              if (lSubSystem.FirmYield) then
                RgpFirmYield.ItemIndex  := 1
              else
                RgpFirmYield.ItemIndex  := 0;
              DoContextValidation(dvtFirmYield);
            end
            else
              RgpFirmYield.ValidationError := lMessage;
          end;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFMSubSystemsValidator.UpdateReservoirsInSubSystem;
const OPNAME = 'TFMSubSystemsValidator.UpdateReservoirsInSubSystem';
var
  lAllocDef   : IAllocationDefinition;
  lSubSystem  : ISubSystem;
  lElementNrs : TStringList;
  lIndex      : integer;
  lEntityNr   : integer;
begin
  try
    lAllocDef := (FAppModules.Model.ModelData as IPlanningModelData).
                   AllocationDefinitionsList.AllocationDefinitionByID[FIdentifier];
    if (lAllocDef <> nil) AND (FSubSystemID > 0) then
    begin
      lSubSystem := lAllocDef.SubSystemByID[FSubSystemID];
      if (lSubSystem <> nil) then
      begin
        with FMSubSystemsDialog do
        begin
          lElementNrs := TStringList.Create;
          try
            for lIndex := 0 to ClbReservoirsInSubSystem.Items.Count - 1 do
            begin
              if (ClbReservoirsInSubSystem.Checked[lIndex]) then
                lElementNrs.Add(IntToStr(Integer(ClbReservoirsInSubSystem.Items.Objects[lIndex])));
            end;
            if (lSubSystem.ReservoirNrs <> lElementNrs.CommaText) then
            begin
              lSubSystem.ReservoirNrs := lElementNrs.CommaText;
              for lIndex := 0 to ClbReservoirsInSubSystem.Items.Count - 1 do
              begin
                lEntityNr := Integer(ClbReservoirsInSubSystem.Items.Objects[lIndex]);
                ClbReservoirsInSubSystem.Checked[lIndex] := lElementNrs.IndexOf(IntToStr(lEntityNr)) >= 0;
              end;
              DoContextValidation(dvtSubSystemReservoirNrs);
            end;
          finally
            FreeAndNil(lElementNrs);
          end;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFMSubSystemsValidator.UpdateCoefficients (ACol   : integer;
                                                     ARow   : integer;
                                                     AValue : string);
const OPNAME = 'TFMSubSystemsValidator.UpdateCoefficients';
var
  lAllocDef    : IAllocationDefinition;
  lSubSystem   : ISubSystem;
  lMessage     : string;
  lValue       : double;
  lCurveIdx    : integer;
  lPercIdx     : integer;
  lCoefficient : ICoefficient;
begin
  try
    lAllocDef := (FAppModules.Model.ModelData as IPlanningModelData).
                   AllocationDefinitionsList.AllocationDefinitionByID[FIdentifier];
    if (lAllocDef <> nil) AND (FSubSystemID > 0) then
    begin
      lSubSystem := lAllocDef.SubSystemByID[FSubSystemID];
      if (lSubSystem <> nil) then
      begin
        with FMSubSystemsDialog do
        begin
          lCurveIdx  := StrToInt(GrdDecisionCurveSet.Cells[GrdDecisionCurveSet.Col, 1]);
          lPercIdx   := GrdStartStoragePerc.Col + 1;
          lCoefficient := lSubSystem.CoefficientByPercCurveCase[lPercIdx, lCurveIdx, ARow];

          GrdCoefficients.ValidationError[ACol, ARow, gveCellContext] :='';
          if (Trim(AValue) = '') then
            AValue := '0.0';
          if (FAppModules.FieldProperties.ValidateFieldProperty(
              GrdCoefficients.FieldProperty(ACol).FieldName, AValue, lMessage)) then
          begin
            lValue := StrToFloat(AValue);
            case ACol of
            0 : lCoefficient.TargetDraft  := lValue;
            1 : lCoefficient.CoefficientA := lValue;
            2 : lCoefficient.CoefficientB := lValue;
            3 : lCoefficient.CoefficientC := lValue;
            4 : lCoefficient.CoefficientD := lValue;
            5 : lCoefficient.Risk         := lValue;
            else
            end;
            PopulateCoefficients(lAllocDef, lSubSystem, -1, -1);
            DoContextValidation(dvtCoefficients);
          end
          else
            GrdCoefficients.ValidationError[ACol, ARow, gveCellContext] := lMessage;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFMSubSystemsValidator.UpdateRoutingChannels;
const OPNAME = 'TFMSubSystemsValidator.UpdateRoutingChannels';
var
  lAllocDef   : IAllocationDefinition;
  lSubSystem  : ISubSystem;
  lIndex      : integer;
  lRow        : integer;
  lChannelNr  : integer;
  lMessage    : string;
begin
  try
    lAllocDef := (FAppModules.Model.ModelData as IPlanningModelData).
                   AllocationDefinitionsList.AllocationDefinitionByID[FIdentifier];
    if (lAllocDef <> nil) AND (FSubSystemID > 0) then
    begin
      lSubSystem := lAllocDef.SubSystemByID[FSubSystemID];
      if (lSubSystem <> nil) then
      begin
        with FMSubSystemsDialog do
        begin
          lIndex := GrdRoutingChannels.Row;
          lChannelNr := 0;
          if (CbxChannel.ItemIndex >= 0) then
            lChannelNr := Integer(CbxChannel.Items.Objects[CbxChannel.ItemIndex]);
          if (lChannelNr = 0) then
          begin
            for lRow := lIndex to 3 do
              lSubSystem.RoutingChannelNrByIndex[lRow] :=
                 lSubSystem.RoutingChannelNrByIndex[lRow+1];
            lSubSystem.RoutingChannelNrByIndex[4] := 0;
            PopulateSubSystem(lAllocDef);
            DoContextValidation(dvtRoutingChannelNrs);
          end
          else
          begin
            CbxChannel.ValidationError := '';
            GrdRoutingChannels.ValidationError[0, lIndex, gveCellContext] := '';
            if (FAppModules.FieldProperties.ValidateFieldProperty(
                CbxChannel.FieldProperty.FieldName, IntToStr(lChannelNr), lMessage, lIndex)) then
            begin
              GrdRoutingChannels.ValidationError[0, lIndex, gveCellContext] := '';
              lSubSystem.RoutingChannelNrByIndex[lIndex] := lChannelNr;
              PopulateSubSystem(lAllocDef);
              DoContextValidation(dvtRoutingChannelNrs);
            end
            else
            begin
              CbxChannel.ValidationError := lMessage;
              GrdRoutingChannels.ValidationError[0, lIndex, gveCellContext] := lMessage;
            end;
          end;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFMSubSystemsValidator.UpdateSupportCalcType;
const OPNAME = 'TFMSubSystemsValidator.UpdateSupportCalcType';
var
  lAllocDef  : IAllocationDefinition;
  lSubSystem : ISubSystem;
  lMessage   : string;
  lValue     : string;
begin
  try
    lAllocDef := (FAppModules.Model.ModelData as IPlanningModelData).
                   AllocationDefinitionsList.AllocationDefinitionByID[FIdentifier];
    if (lAllocDef <> nil) AND (FSubSystemID > 0) then
    begin
      lSubSystem := lAllocDef.SubSystemByID[FSubSystemID];
      if (lSubSystem <> nil) then
      begin
        with FMSubSystemsDialog do
        begin
          if (lSubSystem.SupportCalcType  <> RgpSupportCalcType.ItemIndex) then
          begin
            lValue := IntToStr(RgpSupportCalcType.ItemIndex);
            if (FAppModules.FieldProperties.ValidateFieldProperty(
                RgpSupportCalcType.FieldProperty.FieldName, lValue, lMessage)) then
            begin
              RgpSupportCalcType.ValidationError := lMessage;
              lSubSystem.SupportCalcType := RgpSupportCalcType.ItemIndex;
              RgpSupportCalcType.ItemIndex := lSubSystem.SupportCalcType;
              PopulateSubSystem(lAllocDef);
              DoContextValidation(dvtSupportCalcType);
            end
            else
              RgpSupportCalcType.ValidationError := lMessage;
          end;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFMSubSystemsValidator.DoContextValidation (AValidationType : TDialogValidationType);
const OPNAME = 'TFMSubSystemsValidator.DoContextValidation';
var
  lAllocDef  : IAllocationDefinition;
  lSubSystem : ISubSystem;
begin
  try
    FAllErrorMessages.Clear;
    lAllocDef := nil;
    lSubSystem := nil;
    if (FIdentifier > 0) then
    begin
      lAllocDef := (FAppModules.Model.ModelData as IPlanningModelData).
                     AllocationDefinitionsList.AllocationDefinitionByID[FIdentifier];
      if (lAllocDef <> nil) AND (FSubSystemID > 0) then
      begin
        lSubSystem := lAllocDef.SubSystemByID[FSubSystemID];
        if (lSubSystem <> nil) then
        begin
          if (AValidationType in [dvtSubSystemPropAll, dvtSubSystemName]) then
            ValidateSubSystemName(lSubSystem);
          if (AValidationType in [dvtSubSystemPropAll, dvtSubSystemStartYear, dvtSubSystemStartMonth,
                                  dvtSubSystemEndYear, dvtSubSystemEndMonth]) then
          begin
            ValidateSubSystemStartYear(lSubSystem);
            ValidateSubSystemStartMonth(lSubSystem);
            ValidateSubSystemEndYear(lSubSystem);
            ValidateSubSystemEndMonth(lSubSystem);
          end;
          if (AValidationType in [dvtSubSystemPropAll, dvtSubtractedSubSystemID]) then
            ValidateSubtractedSubSystem(lSubSystem);
          if (AValidationType in [dvtSubSystemPropAll, dvtSupportingSubSystemID]) then
            ValidateSupportingSubSystem(lSubSystem);
          if (AValidationType in [dvtSubSystemPropAll, dvtShortTermYield]) then
            ValidateShortTermYield(lSubSystem);
          if (AValidationType in [dvtSubSystemPropAll, dvtLowestStreamFlow]) then
            ValidateLowestStreamFlow(lSubSystem);
          if (AValidationType in [dvtSubSystemPropAll, dvtLongTermYield]) then
            ValidateLongTermYield(lSubSystem);
          if (AValidationType in [dvtSubSystemPropAll, dvtFirmYield]) then
            ValidateFirmYield(lSubSystem);
          if (AValidationType in [dvtSubSystemPropAll, dvtSubSystemReservoirNrs]) then
            ValidateReservoirsInSubSystem(lSubSystem);
          if (AValidationType in [dvtSubSystemPropAll, dvtRoutingChannelNrs]) then
            ValidateRoutingChannels(lSubSystem);
          if (AValidationType in [dvtSubSystemPropAll, dvtSupportCalcType]) then
            ValidateSupportCalcType(lSubSystem);
          if (AValidationType in [dvtSubSystemPropAll, dvtCoefficients]) then
            ValidateCoefficients(lAllocDef, lSubSystem);
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFMSubSystemsValidator.ValidateSubSystemName (ASubSystem : ISubSystem);
const OPNAME = 'TFMSubSystemsValidator.ValidateSubSystemName';
begin
  try
    with FMSubSystemsDialog do
    begin
      FErrorMessage := '';
      if (NOT ASubSystem.Validate(FErrorMessage, 'SubSystemName')) then
        FAllErrorMessages.Add(Trim(FErrorMessage));
      EdtSubSystemName.ContextValidationError := FErrorMessage;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFMSubSystemsValidator.ValidateSubSystemStartYear (ASubSystem : ISubSystem);
const OPNAME = 'TFMSubSystemsValidator.ValidateSubSystemStartYear';
begin
  try
    with FMSubSystemsDialog do
    begin
      FErrorMessage := '';
      if (NOT ASubSystem.Validate(FErrorMessage, 'SubSystemStartYear')) then
      begin
        CbxStartYear.InValidationError := TRUE;
        CbxStartYear.ValidationError := FErrorMessage;
        CbxStartYear.ShowErrorState(TRUE);
        FAllErrorMessages.Add(Trim(FErrorMessage));
      end
      else
      begin
        CbxStartYear.InValidationError := FALSE;
        CbxStartYear.ShowErrorState(FALSE);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFMSubSystemsValidator.ValidateSubSystemStartMonth (ASubSystem : ISubSystem);
const OPNAME = 'TFMSubSystemsValidator.ValidateSubSystemStartMonth';
begin
  try
    with FMSubSystemsDialog do
    begin
      FErrorMessage := '';
      if (NOT ASubSystem.Validate(FErrorMessage, 'SubSystemStartMonth')) then
      begin
        CbxStartMonth.InValidationError := TRUE;
        CbxStartMonth.ValidationError := FErrorMessage;
        CbxStartMonth.ShowErrorState(TRUE);
        FAllErrorMessages.Add(Trim(FErrorMessage));
      end
      else
      begin
        CbxStartMonth.InValidationError := FALSE;
        CbxStartMonth.ShowErrorState(FALSE);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFMSubSystemsValidator.ValidateSubSystemEndYear (ASubSystem : ISubSystem);
const OPNAME = 'TFMSubSystemsValidator.ValidateSubSystemEndYear';
begin
  try
    with FMSubSystemsDialog do
    begin
      FErrorMessage := '';
      if (NOT ASubSystem.Validate(FErrorMessage, 'SubSystemEndYear')) then
      begin
        CbxEndYear.InValidationError := TRUE;
        CbxEndYear.ValidationError := FErrorMessage;
        CbxEndYear.ShowErrorState(TRUE);
        FAllErrorMessages.Add(Trim(FErrorMessage));
      end
      else
      begin
        CbxEndYear.InValidationError := FALSE;
        CbxEndYear.ShowErrorState(FALSE);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFMSubSystemsValidator.ValidateSubSystemEndMonth (ASubSystem : ISubSystem);
const OPNAME = 'TFMSubSystemsValidator.ValidateSubSystemEndMonth';
begin
  try
    with FMSubSystemsDialog do
    begin
      FErrorMessage := '';
      if (NOT ASubSystem.Validate(FErrorMessage, 'SubSystemEndMonth')) then
      begin
        CbxEndMonth.InValidationError := TRUE;
        CbxEndMonth.ValidationError := FErrorMessage;
        CbxEndMonth.ShowErrorState(TRUE);
        FAllErrorMessages.Add(Trim(FErrorMessage));
      end
      else
      begin
        CbxEndMonth.InValidationError := FALSE;
        CbxEndMonth.ShowErrorState(FALSE);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFMSubSystemsValidator.ValidateSubtractedSubSystem (ASubSystem : ISubSystem);
const OPNAME = 'TFMSubSystemsValidator.ValidateSubtractedSubSystem';
begin
  try
    with FMSubSystemsDialog do
    begin
      FErrorMessage := '';
      if (NOT ASubSystem.Validate(FErrorMessage, 'SubtractedSubSystemID')) then
      begin
        CbxSubtracted.InValidationError := TRUE;
        CbxSubtracted.ValidationError := FErrorMessage;
        CbxSubtracted.ShowErrorState(TRUE);
        FAllErrorMessages.Add(Trim(FErrorMessage));
      end
      else
      begin
        CbxSubtracted.InValidationError := FALSE;
        CbxSubtracted.ShowErrorState(FALSE);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFMSubSystemsValidator.ValidateSupportingSubSystem (ASubSystem : ISubSystem);
const OPNAME = 'TFMSubSystemsValidator.ValidateSupportingSubSystem';
begin
  try
    with FMSubSystemsDialog do
    begin
      FErrorMessage := '';
      if (NOT ASubSystem.Validate(FErrorMessage, 'SupportingSubSystemID')) then
      begin
        CbxSupporting.InValidationError := TRUE;
        CbxSupporting.ValidationError := FErrorMessage;
        CbxSupporting.ShowErrorState(TRUE);
        FAllErrorMessages.Add(Trim(FErrorMessage));
      end
      else
      begin
        CbxSupporting.InValidationError := FALSE;
        CbxSupporting.ShowErrorState(FALSE);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFMSubSystemsValidator.ValidateShortTermYield (ASubSystem : ISubSystem);
const OPNAME = 'TFMSubSystemsValidator.ValidateShortTermYield';
begin
  try
    with FMSubSystemsDialog do
    begin
      FErrorMessage := '';
      if (NOT ASubSystem.Validate(FErrorMessage, 'ShortTermYield')) then
        FAllErrorMessages.Add(Trim(FErrorMessage));
      EdtYieldShort.ContextValidationError := FErrorMessage;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFMSubSystemsValidator.ValidateLongTermYield (ASubSystem : ISubSystem);
const OPNAME = 'TFMSubSystemsValidator.ValidateLongTermYield';
begin
  try
    with FMSubSystemsDialog do
    begin
      FErrorMessage := '';
      if (NOT ASubSystem.Validate(FErrorMessage, 'LongTermYield')) then
        FAllErrorMessages.Add(Trim(FErrorMessage));
      EdtYieldLong.ContextValidationError := FErrorMessage;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFMSubSystemsValidator.ValidateLowestStreamFlow (ASubSystem : ISubSystem);
const OPNAME = 'TFMSubSystemsValidator.ValidateLowestStreamFlow';
begin
  try
    with FMSubSystemsDialog do
    begin
      FErrorMessage := '';
      if (NOT ASubSystem.Validate(FErrorMessage, 'LowestStreamFlow')) then
        FAllErrorMessages.Add(Trim(FErrorMessage));
      EdtLowestStreamFlow.ContextValidationError := FErrorMessage;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFMSubSystemsValidator.ValidateFirmYield (ASubSystem : ISubSystem);
const OPNAME = 'TFMSubSystemsValidator.ValidateFirmYield';
begin
  try
    with FMSubSystemsDialog do
    begin
      FErrorMessage := '';
      if (NOT ASubSystem.Validate(FErrorMessage, 'FirmYield')) then
      begin
        FAllErrorMessages.Add(Trim(FErrorMessage));
        RgpFirmYield.ValidationError := FErrorMessage;
        RgpFirmYield.InValidationError := TRUE;
        RgpFirmYield.ShowErrorState(TRUE);
      end
      else
      begin
        RgpFirmYield.ValidationError := '';
        RgpFirmYield.InValidationError := FALSE;
        RgpFirmYield.ShowErrorState(FALSE);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFMSubSystemsValidator.ValidateReservoirsInSubSystem (ASubSystem : ISubSystem);
const OPNAME = 'TFMSubSystemsValidator.ValidateReservoirsInSubSystem';
begin
  try
    with FMSubSystemsDialog do
    begin
      FErrorMessage := '';
      if (NOT ASubSystem.Validate(FErrorMessage, 'SubSystemReservoirNrs')) then
      begin
        FAllErrorMessages.Add(Trim(FErrorMessage));
        ClbReservoirsInSubSystem.ValidationError := FErrorMessage;
        ClbReservoirsInSubSystem.InValidationError := TRUE;
        ClbReservoirsInSubSystem.ShowErrorState(TRUE);
      end
      else
      begin
        ClbReservoirsInSubSystem.ValidationError := '';
        ClbReservoirsInSubSystem.InValidationError := FALSE;
        ClbReservoirsInSubSystem.ShowErrorState(FALSE);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFMSubSystemsValidator.ValidateRoutingChannels (ASubSystem : ISubSystem);
const OPNAME = 'TFMSubSystemsValidator.ValidateRoutingChannels';
var
  lErrorIndexes  : TStringList;
  lRow           : integer;
  lCount         : integer;
  lErrorMessages : TStringList;
begin
  try
    with FMSubSystemsDialog do
    begin
      FErrorMessage := '';

      for lRow := 1 to GrdRoutingChannels.RowCount - 1 do
         GrdRoutingChannels.ValidationError[0, lRow, gveCellContext] := '';
      if (NOT ASubSystem.Validate(FErrorMessage, 'RoutingChannelNr')) then
      begin
        lErrorIndexes  := TStringList.Create;
        lErrorMessages := TStringList.Create;
        try
          ExtractErrorsAndColumns(FErrorMessage, lErrorMessages, lErrorIndexes);
          for lCount := 0 to lErrorIndexes.Count - 1 do
          begin
            lRow := StrToInt(lErrorIndexes.Strings[lCount]);
            GrdRoutingChannels.ValidationError[0, lRow, gveCellContext] := lErrorMessages.Strings[lCount];
          end;
          FAllErrorMessages.AddStrings(lErrorMessages);
        finally
          FreeAndNil(lErrorIndexes);
          FreeAndNil(lErrorMessages);
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFMSubSystemsValidator.ValidateSupportCalcType (ASubSystem : ISubSystem);
const OPNAME = 'TFMSubSystemsValidator.ValidateSupportCalcType';
begin
  try
    with FMSubSystemsDialog do
    begin
      FErrorMessage := '';
      if (NOT ASubSystem.Validate(FErrorMessage, 'SupportCalcType')) then
      begin
        FAllErrorMessages.Add(Trim(FErrorMessage));
        RgpSupportCalcType.ValidationError := FErrorMessage;
        RgpSupportCalcType.InValidationError := TRUE;
        RgpSupportCalcType.ShowErrorState(TRUE);
      end
      else
      begin
        RgpSupportCalcType.ValidationError := '';
        RgpSupportCalcType.InValidationError := FALSE;
        RgpSupportCalcType.ShowErrorState(FALSE);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFMSubSystemsValidator.ValidateCoefficients (AAllocDef  : IAllocationDefinition;
                                                       ASubSystem : ISubSystem);
const OPNAME = 'TFMSubSystemsValidator.ValidateCoefficients';
var
  lErrorIndexes     : TStringList;
  lSelectedPercIdx  : integer;
  lSelectedCurveIdx : integer;
  lPercIdx          : integer;
  lCurveIdx         : integer;
  lRow              : integer;
  lCol              : integer;
  lCount            : integer;
  lIndexStr         : string;
  lErrorMessages    : TStringList;
  lPos              : integer;
begin
  try
    with FMSubSystemsDialog do
    begin
      FErrorMessage := '';

      if (AAllocDef.NrOfLoadCases > 0) AND (AAllocDef.NrOfStartingPercentages > 0) AND
         (AAllocDef.NrOfCurveSets > 0) then
      begin
        for lRow := 1 to AAllocDef.NrOfLoadCases do
          for lCol := 1 to 6 do
            GrdCoefficients.ValidationError[lCol, lRow, gveCellContext] := '';

        lSelectedPercIdx  := GrdStartStoragePerc.Col + 1;
        lSelectedCurveIdx := StrToInt(GrdDecisionCurveSet.Cells[GrdDecisionCurveSet.Col, 1]);

        lErrorIndexes  := TStringList.Create;
        lErrorMessages := TStringList.Create;
        try
          if (NOT AAllocDef.Validate(FErrorMessage, 'Distribution')) then
          begin
            ExtractErrorsAndColumns(FErrorMessage, lErrorMessages, lErrorIndexes);
            for lCount := 0 to lErrorIndexes.Count - 1 do
            begin
              lIndexStr := lErrorIndexes.Strings[lCount];
              lPos      := Pos(',', lIndexStr);
              lPercIdx  := StrToInt(Copy(lIndexStr, 1, lPos-1));
              lIndexStr := Copy(lIndexStr, lPos+1, Length(lIndexStr)-lPos);
              lPos      := Pos(',', lIndexStr);
              lCurveIdx := StrToInt(Copy(lIndexStr, 1, lPos-1));
              if (lPercIdx = lSelectedPercIdx) AND (lCurveIdx = lSelectedCurveIdx) then
              begin
                lIndexStr := Copy(lIndexStr, lPos+1, Length(lIndexStr)-lPos);
                lPos      := Pos(',', lIndexStr);
                lRow      := StrToInt(Copy(lIndexStr, 1, lPos-1));
                lCol      := StrToInt(Copy(lIndexStr, lPos+1, Length(lIndexStr)-lPos));
                GrdCoefficients.ValidationError[lCol-1, lRow, gveCellContext] := lErrorMessages.Text;
              end
            end;
            FAllErrorMessages.AddStrings(lErrorMessages);
          end;
        finally
          FreeAndNil(lErrorIndexes);
          FreeAndNil(lErrorMessages);
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFMSubSystemsValidator.OnGrdRoutingChannelsSelectCell (Sender        : TObject;
                                                                 ACol, ARow    : Integer;
                                                                 var CanSelect : Boolean);
const OPNAME = 'TFMSubSystemsValidator.OnGrdRoutingChannelsSelectCell';
var
  lName      : string;
  lRow       : integer;
  lSubSystem : ISubSystem;
  lAllocDef  : IAllocationDefinition;
begin
  try
    if (FIdentifier > 0) then
    begin
      lAllocDef := (FAppModules.Model.ModelData as IPlanningModelData).
                     AllocationDefinitionsList.AllocationDefinitionByID[FIdentifier];
      if (lAllocDef <> nil) AND (FSubSystemID > 0) then
        lSubSystem := lAllocDef.SubSystemByID[FSubSystemID]
      else
        lSubSystem := nil;
      if (lSubSystem <> nil) then
      begin
        with FMSubSystemsDialog do
        begin
          if (ARow > 0) then
          begin
            lRow  := ARow;
            while (Trim(GrdRoutingChannels.Cells[0, lRow-1]) = '') AND (lRow > 1) do
              lRow  := lRow - 1;
            if (ARow = lRow) then
            begin
              CbxChannel.Top  := 2 + GrdRoutingChannels.Top +
                                 ((1 + GrdRoutingChannels.DefaultRowHeight) *
                                  (ARow - GrdRoutingChannels.TopRow + 1));
{              CbxChannel.Left := 2 + GrdRoutingChannels.Left +
                                 ((1 + GrdRoutingChannels.DefaultColWidth) *
                                  (ACol - GrdRoutingChannels.LeftCol));}
              lName := Trim(GrdRoutingChannels.Cells[ACol, ARow]);
              CbxChannel.ItemIndex := CbxChannel.Items.IndexOf(lName);
              CbxChannel.Visible := TRUE;
              if (GrdRoutingChannels.ValidationError[ACol, ARow, gveCellContext] <> '') then
              begin
                CbxChannel.ValidationError   := GrdRoutingChannels.ValidationError[ACol, ARow, gveCellContext];
                CbxChannel.InValidationError := TRUE;
                CbxChannel.ShowErrorState(TRUE);
              end
              else
              begin
                CbxChannel.ValidationError   := '';
                CbxChannel.InValidationError := FALSE;
                CbxChannel.ShowErrorState(FALSE);
              end;
            end
            else
              CanSelect := FALSE;
          end;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.

