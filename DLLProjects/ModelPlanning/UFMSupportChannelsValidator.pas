{******************************************************************************}
{*  UNIT      : Contains the class TFMSupportChannelsValidator.               *}
{*  AUTHOR    : Riana Steyn                                                   *}
{*  DATE      : 2006/01/23                                                    *}
{*  COPYRIGHT : Copyright © 2006 DWAF                                         *}
{******************************************************************************}

unit UFMSupportChannelsValidator;

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
  UFMSupportChannelsDialog,
  UAbstractYieldDataDialogValidator,
  UYieldContextValidationType;

type

  TFMSupportChannelsValidator = class(TAbstractYieldDataDialogValidator)
  protected
    FHeading          : string;
    FSupportChannelID : integer;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    procedure CreateDialog; virtual;
    procedure OnEditControlEnter(Sender: TObject); override;
    procedure OnEditControltExit(Sender: TObject); override;
    procedure OnStringGridCellDataHasChanged (ASender: TObject; ACol, ARow: integer); override;
    procedure OnGrdSupportChannelsSelectCell (Sender        : TObject;
                                              ACol, ARow    : Integer;
                                              var CanSelect : Boolean);
    procedure OnGrdCntrlSubSystemsSelectCell (Sender        : TObject;
                                              ACol, ARow    : Integer;
                                              var CanSelect : Boolean);
    procedure OnGrdCntrlSubSystemsTopLeftChanged (Sender : TObject);
    procedure RePopulateDataViewer;
    procedure DoAddSupportChannel (Sender: TObject);
    procedure DoDeleteSupportChannel (Sender: TObject);
    procedure DoAddCntrlSubSystem (Sender: TObject);
    procedure DoDeleteCntrlSubSystem (Sender: TObject);
    procedure PopulateSubSystemComboBox;
    procedure PopulateSupportChannel (AAllocDef : IAllocationDefinition;
                                      ARow      : integer);
    procedure UpdateSupportChannelNumber;
    procedure UpdateCntrlSubSystemID;
    procedure UpdateCntrlSubSystemFactor (ARow   : integer;
                                          AValue : string);
    procedure ValidateSupportChannel (ASupportChannel : ISupportChannel);
    procedure ValidateCntrlSubSystems (ASupportChannel : ISupportChannel);
  public
    function Initialise: boolean; override;
    function SaveState: boolean; override;
    function LanguageHasChanged: boolean; override;
    function StudyHasChanged: boolean; override;
    function StudyDataHasChanged(AContext: TChangeContext; AFieldName,AOldValue,ANewValue: string): boolean; override;
    procedure ClearDataViewer; override;
    procedure PopulateDataViewer; override;
    procedure DoContextValidation (AValidationType : TDialogValidationType); override;
    function FMSupportChannelsDialog : TFMSupportChannelsDialog;
  end;

implementation

uses
  SysUtils,
  VCL.Graphics,
  VCL.Grids,
  UUtilities,
  UYieldModelDataGUIForm,
  UErrorHandlingOperations, Math, Variants;

{******************************************************************************}
{* TFMSupportChannelsValidator                                                *}
{******************************************************************************}

procedure TFMSupportChannelsValidator.CreateMemberObjects;
const OPNAME = 'TFMSupportChannelsValidator.CreateMemberObjects';
var
  lpPanel     : TFMSupportChannelsDialog;
begin
  try
    inherited CreateMemberObjects;
    FIdentifier       := 0;
    FSupportChannelID := 0;
    FHeading          := 'TabCaption.SupportChannels';

    CreateDialog;
    lpPanel := FMSupportChannelsDialog;
    with lpPanel do
    begin
      BtnAddSupportChannel.OnClick    := DoAddSupportChannel;
      BtnDeleteSupportChannel.OnClick := DoDeleteSupportChannel;
      BtnAddCntrlSubSystem.OnClick    := DoAddCntrlSubSystem;
      BtnDeleteCntrlSubSystem.OnClick := DoDeleteCntrlSubSystem;

      GrdSupportChannels.OnSelectCell := OnGrdSupportChannelsSelectCell;
      GrdSupportChannels.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('SupportChannelNr'));

      CbxSupportChannel.FieldProperty := FAppModules.FieldProperties.FieldProperty('SupportChannelNr');
      CbxSupportChannel.OnEnter       := OnEditControlEnter;
      CbxSupportChannel.OnExit        := OnEditControltExit;

      GrdCntrlSubSystems.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('CntrlSubSystemID'));
      GrdCntrlSubSystems.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('CntrlSubSystemID'));
      GrdCntrlSubSystems.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('CntrlFactor'));
      GrdCntrlSubSystems.OnBeforeCellChange := OnStringGridCellDataHasChanged;
      GrdCntrlSubSystems.OnSelectCell       := OnGrdCntrlSubSystemsSelectCell;
      GrdCntrlSubSystems.OnTopLeftChanged   := OnGrdCntrlSubSystemsTopLeftChanged;

      CbxSubSystem.FieldProperty     := FAppModules.FieldProperties.FieldProperty('CntrlSubSystemID');
      CbxSubSystem.OnEnter           := OnEditControlEnter;
      CbxSubSystem.OnChange          := OnEditControltExit;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFMSupportChannelsValidator.CreateDialog;
const OPNAME = 'TFMSupportChannelsValidator.CreateDialog';
begin
  try
    FPanel  := TFMSupportChannelsDialog.Create(FPanelOwner,FAppModules);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFMSupportChannelsValidator.DestroyMemberObjects;
const OPNAME = 'TFMSupportChannelsValidator.DestroyMemberObjects';
begin
  try
    inherited DestroyMemberObjects;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFMSupportChannelsValidator.Initialise: boolean;
const OPNAME = 'TFMSupportChannelsValidator.Initialise';
var
  lIndex          : integer;
  lYieldModelData : IYieldModelData;
  lChannelList    : IChannelList;
  lChannel        : IGeneralFlowChannel;
begin
  Result := inherited Initialise;
  try
    lYieldModelData := (FAppModules.Model.ModelData as IYieldModelData);
    lChannelList    := lYieldModelData.NetworkElementData.ChannelList;
    with FMSupportChannelsDialog do
    begin
      CbxSupportChannel.Clear;
      for lIndex := 0 to lChannelList.ChannelCount - 1 do
      begin
        lChannel := lChannelList.ChannelByIndex[lIndex];
        if (lChannel.ChannelType in [8,9]) then {Min-Max}
          CbxSupportChannel.Items.AddObject(lChannel.ChannelName, TObject(lChannel.ChannelNumber));
      end;

      GrdCntrlSubSystems.ColWidths[0] := 30;
      GrdCntrlSubSystems.ColWidths[1] := 170;
      GrdCntrlSubSystems.Cells[0, 0]  := 'Nr.';
      GrdCntrlSubSystems.Cells[1, 0]  := FAppModules.Language.GetString('PlanningGUI.Subsystem');
      GrdCntrlSubSystems.Cells[2, 0]  := FAppModules.Language.GetString('PlanningGUI.Factor');

    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFMSupportChannelsValidator.LanguageHasChanged: boolean;
const OPNAME = 'TFMSupportChannelsValidator.LanguageHasChanged';
begin
  Result := False;
  try
    TabShetCaption := FAppModules.Language.GetString(FHeading);
    Result := inherited LanguageHasChanged;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFMSupportChannelsValidator.ClearDataViewer;
const OPNAME = 'TFMSupportChannelsValidator.ClearDataViewer';
var
  lIndex : integer;
begin
  inherited ClearDataViewer;
  try
    with FMSupportChannelsDialog do
    begin
      CbxSupportChannel.ItemIndex := -1;
      for lIndex := 1 to GrdCntrlSubSystems.RowCount do
        GrdCntrlSubSystems.Rows[lIndex].Clear;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFMSupportChannelsValidator.PopulateDataViewer;
const OPNAME = 'TFMSupportChannelsValidator.PopulateDataViewer';
begin
  inherited PopulateDataViewer;
  try
    ClearDataViewer;
    PopulateSubSystemComboBox;
    RePopulateDataViewer;
    DoContextValidation(dvtSupportChannelsAll);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFMSupportChannelsValidator.RePopulateDataViewer;
const OPNAME = 'TFMSupportChannelsValidator.RePopulateDataViewer';
var
  lAllocDef            : IAllocationDefinition;
  lNrOfSupportChannels : integer;
  lSupportChannel      : ISupportChannel;
  lChannel             : IGeneralFlowChannel;
  lIndex               : integer;
  lSelectedIndex       : integer;
begin
  try
    with FMSupportChannelsDialog do
    begin
      BtnDeleteSupportChannel.Enabled := FALSE;
      BtnAddCntrlSubSystem.Enabled    := FALSE;
      BtnDeleteCntrlSubSystem.Enabled := FALSE;
      if (FIdentifier > 0) then
      begin
        lAllocDef := (FAppModules.Model.ModelData as IPlanningModelData).
                       AllocationDefinitionsList.AllocationDefinitionByID[FIdentifier];
        if (lAllocDef <> nil) then
        begin
          CbxSupportChannel.Visible := FALSE;
          lNrOfSupportChannels := lAllocDef.NrOfSupportChannels;
          BtnDeleteSupportChannel.Enabled := lNrOfSupportChannels > 0;

          GrdSupportChannels.Cells[0, 0] := FAppModules.Language.GetString('PlanningGUI.SupportChannel');
          GrdSupportChannels.RowCount := lNrOfSupportChannels + 1;
          if (GrdSupportChannels.RowCount > 1) then
            GrdSupportChannels.FixedRows := 1;
          lSelectedIndex := 0;
          for lIndex := 1 to lNrOfSupportChannels do
          begin
            lSupportChannel := lAllocDef.SupportChannelByIndex[lIndex - 1];
            if (FSupportChannelID = 0) then
              FSupportChannelID := lSupportChannel.SupportChannelID;
            GrdSupportChannels.Objects[0, lIndex] := TObject(lSupportChannel.SupportChannelID);
            lChannel := (FAppModules.Model.ModelData as IYieldModelData).
                          NetworkElementData.ChannelList.ChannelByChannelNumber[lSupportChannel.ChannelNumber];
            if (lChannel <> nil) then
              GrdSupportChannels.Cells[0, lIndex] := lChannel.ChannelName
            else
              GrdSupportChannels.Cells[0, lIndex] := FAppModules.Language.GetString('PlanningGUI.Undefined');
            if (lSupportChannel.SupportChannelID = FSupportChannelID) then
              lSelectedIndex := lIndex;
          end;
          ClearDataViewer;
          if (lNrOfSupportChannels > 0) then
          begin
            PnlSupportChannel.Enabled := TRUE;
            if (GrdSupportChannels.Row <> lSelectedIndex) then
              GrdSupportChannels.Row := lSelectedIndex
            else
              PopulateSupportChannel(lAllocDef, 0);
          end
          else
            PnlSupportChannel.Enabled := FALSE;
        end;
      end
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFMSupportChannelsValidator.OnGrdSupportChannelsSelectCell (Sender        : TObject;
                                                                      ACol, ARow    : Integer;
                                                                      var CanSelect : Boolean);
const OPNAME = 'TFMSupportChannelsValidator.OnGrdSupportChannelsSelectCell';
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
        FSupportChannelID := Integer(FMSupportChannelsDialog.GrdSupportChannels.Objects[0, ARow]);
        PopulateSupportChannel(lAllocDef, ARow);
        DoContextValidation(dvtSupportChannelsAll);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFMSupportChannelsValidator.PopulateSupportChannel (AAllocDef : IAllocationDefinition;
                                                              ARow      : integer);
const OPNAME = 'TFMSupportChannelsValidator.PopulateSupportChannel';
var
  lSupportChannel : ISupportChannel;
  lChannel        : IGeneralFlowChannel;
  lIndex          : integer;
  lCntrlSubSysID  : integer;
  lCntrlFactor    : double;
  lSubSystem      : ISubSystem;
  lName           : string;
  LFieldProperty  : TAbstractFieldProperty;
begin
  try
    ClearDataViewer;
    with FMSupportChannelsDialog do
    begin
      if (ARow = 0) then
        ARow := GrdSupportChannels.Row;
      CbxSupportChannel.ItemIndex := -1;
      CbxSupportChannel.Top       := GrdSupportChannels.Top +  2 +
                                     ((1 + GrdSupportChannels.DefaultRowHeight) *
                                      (ARow - GrdSupportChannels.TopRow + 1));

      lName := Trim(GrdSupportChannels.Cells[1, ARow]);
      CbxSupportChannel.ItemIndex := CbxSupportChannel.Items.IndexOf(lName);
      CbxSupportChannel.Visible := TRUE;
      if (GrdSupportChannels.InValidationError[1, ARow, gveCellContext]) then
      begin
        CbxSupportChannel.ValidationError   := GrdSupportChannels.ValidationError[0, ARow, gveCellContext];
        CbxSupportChannel.InValidationError := TRUE;
        CbxSupportChannel.ShowErrorState(TRUE);
      end
      else
      begin
        CbxSupportChannel.ValidationError   := '';
        CbxSupportChannel.InValidationError := FALSE;
        CbxSupportChannel.ShowErrorState(FALSE);
      end;

      CbxSubSystem.Visible := FALSE;
      for lIndex := 1 to GrdCntrlSubSystems.RowCount - 1 do
        GrdCntrlSubSystems.Rows[lIndex].Clear;
      if (FSupportChannelID > 0) then
        lSupportChannel := AAllocDef.SupportChannelByID[FSupportChannelID]
      else
        lSupportChannel := nil;
      if (lSupportChannel <> nil) then
      begin
        lChannel := (FAppModules.Model.ModelData as IYieldModelData).NetworkElementData.
                      ChannelList.ChannelByChannelNumber[lSupportChannel.ChannelNumber];
        if (lChannel <> nil) then
          CbxSupportChannel.ItemIndex := CbxSupportChannel.Items.IndexOf(lChannel.ChannelName);

        BtnAddCntrlSubSystem.Enabled    := AAllocDef.NrOfSubSystems > 0;
        BtnDeleteCntrlSubSystem.Enabled := lSupportChannel.NrOfCntrlSubSystems > 0;
        GrdCntrlSubSystems.RowCount := lSupportChannel.NrOfCntrlSubSystems + 1;
        if (GrdCntrlSubSystems.RowCount > 1) then
          GrdCntrlSubSystems.FixedRows := 1;
        for lIndex := 1 to lSupportChannel.NrOfCntrlSubSystems  do
        begin
          lCntrlSubSysID := lSupportChannel.SubSystemIDByIndex[lIndex];
          lCntrlFactor   := lSupportChannel.SubSystemFactorByIndex[lIndex];
          lSubSystem     := AAllocDef.SubSystemByID[lCntrlSubSysID];
          GrdCntrlSubSystems.Cells[0, lIndex] := IntToStr(lIndex);
          if (lSubSystem <> nil) then
            GrdCntrlSubSystems.Cells[1, lIndex] := lSubSystem.Name
          else
            GrdCntrlSubSystems.Cells[1, lIndex] := FAppModules.Language.GetString('PlanningGUI.Undefined');
          LFieldProperty := FAppModules.FieldProperties.FieldProperty('FlowConstraints');
          GrdCntrlSubSystems.Cells[2, lIndex] := Format(LFieldProperty.FormatStringGrid {'%6.2f'}, [lCntrlFactor]);
        end;
      end
      else
      begin
        GrdCntrlSubSystems.RowCount := 1;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFMSupportChannelsValidator.PopulateSubSystemComboBox;
const OPNAME = 'TFMSupportChannelsValidator.PopulateSubSystemComboBox';
var
  lIndex     : integer;
  lSubSystem : ISubSystem;
  lAllocDef  : IAllocationDefinition;
begin
  try
    with FMSupportChannelsDialog do
    begin
      CbxSubSystem.Clear;
      if (FIdentifier > 0) then
      begin
        lAllocDef := (FAppModules.Model.ModelData as IPlanningModelData).
                       AllocationDefinitionsList.AllocationDefinitionByID[FIdentifier];
        if (lAllocDef <> nil) then
        begin
          for lIndex := 0 to lAllocDef.NrOfSubSystems - 1 do
          begin
            lSubSystem := lAllocDef.SubSystemByIndex[lIndex];
            CbxSubSystem.Items.AddObject(lSubSystem.Name, TObject(lSubSystem.SubSystemID));
          end;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFMSupportChannelsValidator.SaveState: boolean;
const OPNAME = 'TFMSupportChannelsValidator.SaveState';
begin
  Result := inherited SaveState;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFMSupportChannelsValidator.FMSupportChannelsDialog:TFMSupportChannelsDialog;
const OPNAME = 'TFMSupportChannelsValidator.FMSupportChannelsDialog';
begin
  Result := Nil;
  try
    if (FPanel <> nil) then
      Result := TFMSupportChannelsDialog(FPanel);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFMSupportChannelsValidator.StudyDataHasChanged(AContext: TChangeContext; AFieldName,AOldValue,ANewValue: string): boolean;
const OPNAME = 'TFMSupportChannelsValidator.StudyDataHasChanged';
begin
  Result := inherited StudyDataHasChanged(AContext,AFieldName,AOldValue,ANewValue);
  try
    if (AFieldName = 'NrOfSubSystems') then
      PopulateDataViewer;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFMSupportChannelsValidator.StudyHasChanged: boolean;
const OPNAME = 'TFMSupportChannelsValidator.StudyHasChanged';
begin
  Result := inherited StudyHasChanged;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFMSupportChannelsValidator.DoAddSupportChannel (Sender: TObject);
const OPNAME = 'TFMSupportChannelsValidator.DoAddSupportChannel';
var
  lAllocDef       : IAllocationDefinition;
  lSupportChannel : ISupportChannel;
begin
  try
    lAllocDef := (FAppModules.Model.ModelData as IPlanningModelData).
                   AllocationDefinitionsList.AllocationDefinitionByID[FIdentifier];
    if (lAllocDef <> nil) then
    begin
      lSupportChannel   := lAllocDef.NewSupportChannel;
      FSupportChannelID := lSupportChannel.SupportChannelID;
      RePopulateDataViewer;
      DoContextValidation(dvtSupportChannelsAll);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFMSupportChannelsValidator.DoDeleteSupportChannel (Sender: TObject);
const OPNAME = 'TFMSupportChannelsValidator.DoDeleteSupportChannel';
var
  lAllocDef     : IAllocationDefinition;
begin
  try
    lAllocDef := (FAppModules.Model.ModelData as IPlanningModelData).
                   AllocationDefinitionsList.AllocationDefinitionByID[FIdentifier];
    if (lAllocDef <> nil) AND (FSupportChannelID > 0) then
    begin
      if (lAllocDef.RemoveSupportChannel(FSupportChannelID)) then
      begin
        FSupportChannelID := 0;
        RePopulateDataViewer;
        DoContextValidation(dvtSupportChannelsAll);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFMSupportChannelsValidator.DoAddCntrlSubSystem (Sender: TObject);
const OPNAME = 'TFMSupportChannelsValidator.DoAddCntrlSubSystem';
var
  lAllocDef       : IAllocationDefinition;
  lSupportChannel : ISupportChannel;
begin
  try
    lAllocDef := (FAppModules.Model.ModelData as IPlanningModelData).
                   AllocationDefinitionsList.AllocationDefinitionByID[FIdentifier];
    if (lAllocDef <> nil) AND (FSupportChannelID > 0) then
    begin
      lSupportChannel := lAllocDef.SupportChannelByID[FSupportChannelID];
      if (lSupportChannel <> nil) then
      begin
        lSupportChannel.NewControllingSubSystem;
        PopulateSupportChannel(lAllocDef, 0);
        DoContextValidation(dvtSupportChannelsAll);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFMSupportChannelsValidator.DoDeleteCntrlSubSystem (Sender: TObject);
const OPNAME = 'TFMSupportChannelsValidator.DoDeleteCntrlSubSystem';
var
  lAllocDef       : IAllocationDefinition;
  lIndex          : integer;
  lSupportChannel : ISupportChannel;
begin
  try
    lIndex := FMSupportChannelsDialog.GrdCntrlSubSystems.Row;
    lAllocDef := (FAppModules.Model.ModelData as IPlanningModelData).
                   AllocationDefinitionsList.AllocationDefinitionByID[FIdentifier];
    if (lAllocDef <> nil) AND (lIndex >= 0) AND (FSupportChannelID > 0) then
    begin
      lSupportChannel := lAllocDef.SupportChannelByID[FSupportChannelID];
      if (lSupportChannel <> nil) then
      begin
        lSupportChannel.RemoveControllingSubSystem(lIndex);
        PopulateSupportChannel(lAllocDef, 0);
        DoContextValidation(dvtSupportChannelsAll);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFMSupportChannelsValidator.OnGrdCntrlSubSystemsSelectCell (Sender        : TObject;
                                                                      ACol, ARow    : Integer;
                                                                      var CanSelect : Boolean);
const OPNAME = 'TFMSupportChannelsValidator.OnGrdCntrlSubSystemsSelectCell';
var
  lAllocDef : IAllocationDefinition;
  lName     : string;
begin
  try
    if (FIdentifier > 0) then
    begin
      lAllocDef := (FAppModules.Model.ModelData as IPlanningModelData).
                     AllocationDefinitionsList.AllocationDefinitionByID[FIdentifier];
      if (lAllocDef <> nil) then
      begin
        with FMSupportChannelsDialog do
        begin
          if (ARow > 0) then
          begin
            CbxSubSystem.Left := 53;
            CbxSubSystem.Top  := 3 + GrdCntrlSubSystems.Top +
                                 ((1 + GrdCntrlSubSystems.DefaultRowHeight) *
                                  (ARow - GrdCntrlSubSystems.TopRow + 1));
            lName := Trim(GrdCntrlSubSystems.Cells[1, ARow]);
            CbxSubSystem.ItemIndex := CbxSubSystem.Items.IndexOf(lName);
            CbxSubSystem.Visible := TRUE;
            if (GrdCntrlSubSystems.InValidationError[1, ARow, gveCellContext]) then
            begin
              CbxSubSystem.ValidationError   := GrdCntrlSubSystems.ValidationError[1, ARow, gveCellContext];
              CbxSubSystem.InValidationError := TRUE;
              CbxSubSystem.ShowErrorState(TRUE);
            end
            else
            begin
              CbxSubSystem.ValidationError   := '';
              CbxSubSystem.InValidationError := FALSE;
              CbxSubSystem.ShowErrorState(FALSE);
            end;
          end;
          if (ACol = 1) then
            GrdCntrlSubSystems.Options := GrdCntrlSubSystems.Options - [goEditing]
          else
            GrdCntrlSubSystems.Options := GrdCntrlSubSystems.Options + [goEditing];
          OnEditControlEnter(Sender);
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFMSupportChannelsValidator.OnGrdCntrlSubSystemsTopLeftChanged(Sender: TObject);
const OPNAME = 'TFMSupportChannelsValidator.OnGrdCntrlSubSystemsTopLeftChanged';
begin
  try
    FMSupportChannelsDialog.CbxSubSystem.Visible := FALSE;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFMSupportChannelsValidator.OnEditControlEnter(Sender: TObject);
const OPNAME = 'TFMSupportChannelsValidator.OnEditControlEnter';
begin
  inherited OnEditControlEnter(Sender);
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFMSupportChannelsValidator.OnEditControltExit (Sender : TObject);
const OPNAME = 'TFMSupportChannelsValidator.OnEditControltExit';
begin
  inherited OnEditControltExit(Sender);
  try
    with FMSupportChannelsDialog do
    begin
      if ((Sender = CbxSupportChannel) AND
          (CbxSupportChannel.HasValueChanged)) then
        UpdateSupportChannelNumber
      else
      if ((Sender = CbxSubSystem) AND
          (CbxSubSystem.HasValueChanged)) then
        UpdateCntrlSubSystemID;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFMSupportChannelsValidator.OnStringGridCellDataHasChanged
                                                           (ASender    : TObject;
                                                            ACol, ARow : integer);
const OPNAME = 'TFMSupportChannelsValidator.OnStringGridCellDataHasChanged';
begin
  inherited OnStringGridCellDataHasChanged(ASender, ACol, ARow);
  try
    with FMSupportChannelsDialog do
    begin
      if (GrdCntrlSubSystems = ASender) AND (ACol = 2) then
        UpdateCntrlSubSystemFactor(ARow, Trim(GrdCntrlSubSystems.Cells[ACol, ARow]));
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFMSupportChannelsValidator.UpdateSupportChannelNumber;
const OPNAME = 'TFMSupportChannelsValidator.UpdateSupportChannelNumber';
var
  lAllocDef       : IAllocationDefinition;
  lSupportChannel : ISupportChannel;
  lChannel        : IGeneralFlowChannel;
  lMessage        : string;
  lStrValue       : string;
  lChannelNr      : integer;
begin
  try
    lAllocDef := (FAppModules.Model.ModelData as IPlanningModelData).
                   AllocationDefinitionsList.AllocationDefinitionByID[FIdentifier];
    if (lAllocDef <> nil) AND (FSupportChannelID > 0) then
    begin
      lSupportChannel := lAllocDef.SupportChannelByID[FSupportChannelID];
      if (lSupportChannel <> nil) then
      begin
        with FMSupportChannelsDialog do
        begin
          lChannelNr := 0;
          if (CbxSupportChannel.ItemIndex >= 0) then
            lChannelNr := Integer(CbxSupportChannel.Items.Objects[CbxSupportChannel.ItemIndex]);
          lStrValue := IntToStr(lChannelNr);
          if (FAppModules.FieldProperties.ValidateFieldProperty(
              CbxSupportChannel.FieldProperty.FieldName, lStrValue, lMessage)) then
          begin
            CbxSupportChannel.ValidationError := lMessage;
            lSupportChannel.ChannelNumber := lChannelNr;
            lChannel := (FAppModules.Model.ModelData as IYieldModelData).NetworkElementData.
                          ChannelList.ChannelByChannelNumber[lChannelNr];
            if (lChannel <> nil) then
              CbxSupportChannel.ItemIndex := CbxSupportChannel.Items.IndexOf(lChannel.ChannelName)
            else
              CbxSupportChannel.ItemIndex := -1;
            RePopulateDataViewer;
            DoContextValidation(dvtSupportChannels);
          end
          else
            CbxSupportChannel.ValidationError := lMessage;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFMSupportChannelsValidator.UpdateCntrlSubSystemID;
const OPNAME = 'TFMSupportChannelsValidator.UpdateCntrlSubSystemID';
var
  lAllocDef       : IAllocationDefinition;
  lSupportChannel : ISupportChannel;
  lMessage        : string;
  lStrValue       : string;
  lSubSystemID    : integer;
  lRow            : integer;
begin
  try
    lAllocDef := (FAppModules.Model.ModelData as IPlanningModelData).
                   AllocationDefinitionsList.AllocationDefinitionByID[FIdentifier];
    if (lAllocDef <> nil) AND (FSupportChannelID > 0) then
    begin
      lSupportChannel := lAllocDef.SupportChannelByID[FSupportChannelID];
      if (lSupportChannel <> nil) then
      begin
        with FMSupportChannelsDialog do
        begin
          lSubSystemID := 0;
          lRow := GrdCntrlSubSystems.Row;
          if (CbxSubSystem.ItemIndex >= 0) then
            lSubSystemID := Integer(CbxSubSystem.Items.Objects[CbxSubSystem.ItemIndex]);
          lStrValue := IntToStr(lSubSystemID);
          CbxSubSystem.ValidationError := '';
          GrdCntrlSubSystems.ValidationError[1, lRow, gveCellContext] := '';
          if (FAppModules.FieldProperties.ValidateFieldProperty(
              CbxSubSystem.FieldProperty.FieldName, lStrValue, lMessage, lRow)) then
          begin
            GrdCntrlSubSystems.ValidationError[1, lRow, gveCellContext] := lMessage;
            lSupportChannel.SubSystemIDByIndex[lRow] := lSubSystemID;
            PopulateSupportChannel(lAllocDef, 0);
            DoContextValidation(dvtCntrlSubSystems);
          end
          else
          begin
            CbxSubSystem.ValidationError := lMessage;
            GrdCntrlSubSystems.ValidationError[1, lRow, gveCellContext] := lMessage;
          end;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFMSupportChannelsValidator.UpdateCntrlSubSystemFactor (ARow   : integer;
                                                                  AValue : string);
const OPNAME = 'TFMSupportChannelsValidator.UpdateCntrlSubSystemFactor';
var
  lAllocDef       : IAllocationDefinition;
  lSupportChannel : ISupportChannel;
  lMessage        : string;
begin
  try
    lAllocDef := (FAppModules.Model.ModelData as IPlanningModelData).
                   AllocationDefinitionsList.AllocationDefinitionByID[FIdentifier];
    if (lAllocDef <> nil) AND (FSupportChannelID > 0) then
    begin
      lSupportChannel := lAllocDef.SupportChannelByID[FSupportChannelID];
      if (lSupportChannel <> nil) then
      begin
        with FMSupportChannelsDialog do
        begin
          GrdCntrlSubSystems.ValidationError[2, ARow, gveCellContext] :='';
          if (Trim(AValue) = '') then
            AValue := '0.0';
          if (FAppModules.FieldProperties.ValidateFieldProperty(
              GrdCntrlSubSystems.FieldProperty(2).FieldName, AValue, lMessage, ARow)) then
          begin
            GrdCntrlSubSystems.ValidationError[2, ARow, gveCellContext] := lMessage;
            lSupportChannel.SubSystemFactorByIndex[ARow] := StrToFloat(AValue);
            PopulateSupportChannel(lAllocDef, 0);
            DoContextValidation(dvtCntrlSubSystems);
          end
          else
            GrdCntrlSubSystems.ValidationError[2, ARow, gveCellContext] := lMessage;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFMSupportChannelsValidator.DoContextValidation(AValidationType : TDialogValidationType);
const OPNAME = 'TFMSupportChannelsValidator.DoContextValidation';
var
  lAllocDef       : IAllocationDefinition;
  lSupportChannel : ISupportChannel;
begin
  try
    FAllErrorMessages.Clear;
    lAllocDef := nil;
    if (FIdentifier > 0) then
    begin
      lAllocDef := (FAppModules.Model.ModelData as IPlanningModelData).
                     AllocationDefinitionsList.AllocationDefinitionByID[FIdentifier];
      if (lAllocDef <> nil) AND (FSupportChannelID > 0) then
      begin
        lSupportChannel := lAllocDef.SupportChannelByID[FSupportChannelID];
        if (lSupportChannel <> nil) then
        begin
          if (AValidationType in [dvtSupportChannelsAll, dvtSupportChannels]) then
            ValidateSupportChannel(lSupportChannel);
          if (AValidationType in [dvtSupportChannelsAll, dvtCntrlSubSystems]) then
            ValidateCntrlSubSystems(lSupportChannel);
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFMSupportChannelsValidator.ValidateCntrlSubSystems (ASupportChannel : ISupportChannel);
const OPNAME = 'TFMSupportChannelsValidator.ValidateCntrlSubSystems';
var
  lErrorIndexes  : TStringList;
  lRow           : integer;
  lCol           : integer;
  lCount         : integer;
  lIndexStr      : string;
  lErrorMessages : TStringList;
  lPos           : integer;
begin
  try
    with FMSupportChannelsDialog do
    begin
      FErrorMessage := '';

      for lRow := 1 to ASupportChannel.NrOfCntrlSubSystems do
        for lCol := 1 to GrdCntrlSubSystems.ColCount - 1 do
          GrdCntrlSubSystems.ValidationError[lCol, lRow, gveCellContext] := '';
      if (NOT ASupportChannel.Validate(FErrorMessage, 'CntrlSubSystems')) then
      begin
        lErrorIndexes  := TStringList.Create;
        lErrorMessages := TStringList.Create;
        try
          ExtractErrorsAndColumns(FErrorMessage, lErrorMessages, lErrorIndexes);
          for lCount := 0 to lErrorIndexes.Count - 1 do
          begin
            lIndexStr := lErrorIndexes.Strings[lCount];
            lPos      := Pos(',', lIndexStr);
            lRow      := StrToInt(Copy(lIndexStr, 1, lPos-1));
            lCol      := StrToInt(Copy(lIndexStr, lPos+1, Length(lIndexStr)-lPos));
            GrdCntrlSubSystems.ValidationError[lCol, lRow, gveCellContext] := lErrorMessages.Strings[lCount];
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

procedure TFMSupportChannelsValidator.ValidateSupportChannel (ASupportChannel : ISupportChannel);
const OPNAME = 'TFMSupportChannelsValidator.ValidateSupportChannel';
begin
  try
    with FMSupportChannelsDialog do
    begin
      FErrorMessage := '';
      if (NOT ASupportChannel.Validate(FErrorMessage, 'SupportChannelNr')) then
      begin
        CbxSupportChannel.InValidationError := TRUE;
        CbxSupportChannel.ValidationError := FErrorMessage;
        CbxSupportChannel.ShowErrorState(TRUE);
        FAllErrorMessages.Add(Trim(FErrorMessage));
      end
      else
      begin
        CbxSupportChannel.InValidationError := FALSE;
        CbxSupportChannel.ShowErrorState(FALSE);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.

