{******************************************************************************}
{*  UNIT      : Contains the class TChannelSwitchControlValidator.            *}
{*  AUTHOR    : Riana Steyn                                                   *}
{*  DATE      : 2006/03/09                                                    *}
{*  COPYRIGHT : Copyright © 2006 DWAF                                         *}
{******************************************************************************}

unit UChannelSwitchControlValidator;

interface

uses
  Classes,
  VCL.Controls,
  VCL.ComCtrls,
  VCL.StdCtrls,
  VCL.ExtCtrls,
  VCL.Dialogs,

  VoaimsCom_TLB,
  UAbstractObject,
  UAbstractComponent,
  UDataComponent,
  UDataEditComponent,
  UChannelSwitchControlDialog,
  UAbstractYieldDataDialogValidator,
  UYieldContextValidationType;
type
  TChannelSwitchControlValidator = class(TAbstractYieldDataDialogValidator)
  protected
    FChannelSwitchID : integer;
    FRelationType : integer;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    procedure CreateDialog;
    procedure OnEditControlEnter(Sender: TObject); override;
    procedure OnEditControltExit(Sender: TObject); override;
    procedure OnRgpSwitchTypeClick(Sender: TObject);
    procedure OnRgpInitialStatusClick(Sender: TObject);
    procedure OnGrdSwitchControlSelectCell (Sender        : TObject;
                                            ACol, ARow    : Integer;
                                            var CanSelect : Boolean);
    procedure DoAddSwitchControl (Sender: TObject);
    procedure DoDeleteSwitchControl (Sender: TObject);
    procedure UpdateSwitchDefinition;
    procedure UpdateAssociatedNode;
    procedure UpdateWaterLevel;
    procedure UpdateSwitchType;
    procedure UpdateInitialStatus;

    procedure UpdateYearActive;
    procedure UpdateMonthActive;
    procedure UpdateYearAbsolete;
    procedure UpdateMonthAbsolete;

    procedure RePopulateDataViewer;
    procedure PopulateDateComboBoxes;
    procedure PopulateDateValues(ATimeControl   : IChannelTimeControl);
    procedure PopulateSwitchControl (AChannel : IGeneralFlowChannel);
    function Get_SwitchDefinition(ASwitchDef : ISwitchDefinition): string;

    procedure ValidateSwitchDefinition (ASwitchControl : IChannelSwitchControl);
    procedure ValidateSwitchAssociatedNodeNr (ASwitchControl : IChannelSwitchControl);
    procedure ValidateSwitchWaterLevel (ASwitchControl : IChannelSwitchControl);
    procedure ValidateSwitchType (ASwitchControl : IChannelSwitchControl);
    procedure ValidateSwitchInitialStatus (ASwitchControl : IChannelSwitchControl);
  public

    function Initialise: boolean; override;
    function SaveState: boolean; override;
    function LanguageHasChanged: boolean; override;
    function StudyHasChanged: boolean; override;
    function StudyDataHasChanged (AContext   : TChangeContext;
                                  AFieldName : string;
                                  AOldValue  : string;
                                  ANewValue  : string): boolean; override;
    procedure ClearDataViewer; override;
    procedure PopulateDataViewer; override;
    function ChannelSwitchControlDialog : TChannelSwitchControlDialog;
    procedure DoContextValidation (AValidationType : TDialogValidationType); override;

  end;

implementation

uses
  SysUtils,
  VCL.Graphics,
  UUtilities,
  UFileNames,
  UConstants,
  USwitchDialog,
  UAbstractFileNamesObject,
  UMonthlyDamLevelsObject,
  UReservoirPenaltyValidator,
  UYieldModelDataGUIForm,
  UReservoirPenaltyDialog,
  UReservoirPenaltyStructureData,
  UYieldModelDataObject,
  UMainMenuEventType,
  UErrorHandlingOperations, Math;

{******************************************************************************}
{* TChannelSwitchControlValidator                                             *}
{******************************************************************************}

procedure TChannelSwitchControlValidator.CreateMemberObjects;
const OPNAME = 'TChannelSwitchControlValidator.CreateMemberObjects';
var
  lPanel : TChannelSwitchControlDialog;
begin
  try
    inherited CreateMemberObjects;
    FChannelSwitchID := 0;
    FIdentifier := 0;
    CreateDialog;
    lPanel := ChannelSwitchControlDialog;
    with lPanel do
    begin
      BtnAddSwitchControl.OnClick    := DoAddSwitchControl;
      BtnDeleteSwitchControl.OnClick := DoDeleteSwitchControl;
      
      CbxSwitchDefinition.FieldProperty := FAppModules.FieldProperties.FieldProperty('SwitchDefinitionID');
      CbxSwitchDefinition.OnEnter       := OnEditControlEnter;
      CbxSwitchDefinition.OnChange      := OnEditControltExit;

      CbxAssociatedNode.FieldProperty := FAppModules.FieldProperties.FieldProperty('SwitchAssociatedNodeNr');
      CbxAssociatedNode.OnEnter       := OnEditControlEnter;
      CbxAssociatedNode.OnChange      := OnEditControltExit;

      EdtWaterLevel.FieldProperty := FAppModules.FieldProperties.FieldProperty('SwitchWaterlevel');
      EdtWaterLevel.OnEnter       := OnEditControlEnter;
      EdtWaterLevel.OnExit        := OnEditControltExit;

      RgpSwitchType.FieldProperty := FAppModules.FieldProperties.FieldProperty('SwitchType');
      RgpSwitchType.OnEnter       := OnEditControlEnter;
      RgpSwitchType.OnClick       := OnRgpSwitchTypeClick;

      RgpInitialStatus.FieldProperty := FAppModules.FieldProperties.FieldProperty('SwitchInitialStatus');
      RgpInitialStatus.OnEnter       := OnEditControlEnter;
      RgpInitialStatus.OnClick       := OnRgpInitialStatusClick;


      CbxYearActive.FieldProperty  := FAppModules.FieldProperties.FieldProperty('ChannelStartYear');
      CbxYearActive.OnEnter        := OnEditControlEnter;
      CbxYearActive.OnChange         := OnEditControltExit;

      CbxMonthActive.FieldProperty  := FAppModules.FieldProperties.FieldProperty('ChannelStartMonth');
      CbxMonthActive.OnEnter        := OnEditControlEnter;
      CbxMonthActive.OnChange         := OnEditControltExit;

      CbxYearObsolete.FieldProperty  := FAppModules.FieldProperties.FieldProperty('ChannelEndYear');
      CbxYearObsolete.OnEnter        := OnEditControlEnter;
      CbxYearObsolete.OnChange         := OnEditControltExit;

      CbxMonthObsolete.FieldProperty  := FAppModules.FieldProperties.FieldProperty('ChannelEndMonth');
      CbxMonthObsolete.OnEnter        := OnEditControlEnter;
      CbxMonthObsolete.OnChange         := OnEditControltExit;


      GrdSwitchControls.OnSelectCell := OnGrdSwitchControlSelectCell;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TChannelSwitchControlValidator.DestroyMemberObjects;
const OPNAME = 'TChannelSwitchControlValidator.DestroyMemberObjects';
begin
  try
    inherited DestroyMemberObjects;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TChannelSwitchControlValidator.CreateDialog;
const OPNAME = 'TChannelSwitchControlValidator.CreateDialog';
begin
  try
    FPanel  := TChannelSwitchControlDialog.Create(FPanelOwner,FAppModules);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TChannelSwitchControlValidator.Initialise: boolean;
const OPNAME = 'TChannelSwitchControlValidator.Initialise';
begin
  Result := inherited Initialise;
  try
    FRelationType := -1;
    ChannelSwitchControlDialog.TimeRelatedGroup.visible := False;
    ChannelSwitchControlDialog.LevelRelatedGroup.visible := False;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TChannelSwitchControlValidator.LanguageHasChanged: boolean;
const OPNAME = 'TChannelSwitchControlValidator.LanguageHasChanged';
begin
  Result := False;
  try
    TabShetCaption := 'Switch control(s)';
    Result := inherited LanguageHasChanged;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TChannelSwitchControlValidator.ClearDataViewer;
const OPNAME = 'TChannelSwitchControlValidator.ClearDataViewer';
//var
//  lIndex : integer;
begin
  inherited ClearDataViewer;
  try
    with ChannelSwitchControlDialog do
    begin
      CbxSwitchDefinition.ItemIndex := -1;
      CbxAssociatedNode.ItemIndex   := -1;
      EdtWaterLevel.SetFieldValue(0.0);
      RgpSwitchType.ItemIndex       := -1;
      RgpInitialStatus.ItemIndex    := -1;
//      for lIndex := 1 to GrdSwitchControls.RowCount - 1 do
//        GrdSwitchControls.Rows[lIndex].Clear;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TChannelSwitchControlValidator.PopulateDataViewer;
const OPNAME = 'TChannelSwitchControlValidator.PopulateDataViewer';
begin
  inherited PopulateDataViewer;
  try
    ClearDataViewer;
    PopulateDateComboBoxes;
    RePopulateDataViewer;
    DoContextValidation(dvtChannelSwitchControlAll);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TChannelSwitchControlValidator.PopulateDateComboBoxes;
const OPNAME = 'TChannelSwitchControlValidator.PopulateDateComboBoxes';
var
  lIndex : integer;
  lYear  : word;
  lMonth : word;
  lDay   : word;
begin
  try
    with ChannelSwitchControlDialog do
    begin
      CbxYearActive.Clear;
      CbxMonthActive.Clear;
      CbxYearObsolete.Clear;
      CbxMonthObsolete.Clear;
      DecodeDate(Now, lYear, lMonth, lDay);
      for lIndex := 1900 to lYear + 1000 do
      begin
        CbxYearActive.Items.Add(IntToStr(lIndex));
        CbxYearObsolete.Items.Add(IntToStr(lIndex));
      end;
      for lIndex := 1 to 12 do
      begin
        CbxMonthActive.Items.Add(UpperCase(FormatSettings.ShortMonthNames[lIndex]));
        CbxMonthObsolete.Items.Add(UpperCase(FormatSettings.ShortMonthNames[lIndex]));
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TChannelSwitchControlValidator.PopulateDateValues(ATimeControl: IChannelTimeControl);
const OPNAME = 'TChannelSwitchControlValidator.PopulateDateValues';
var
  LCalendarStartMonth,
  LStartYear,
  LStartMonth,
  LEndYear,
  LEndMonth : integer;
begin
  try
    with ChannelSwitchControlDialog do
    begin
      LCalendarStartMonth := FAppModules.StudyArea.CalendarStartMonth;
      LStartYear          := ConvertHydrologicalYearToCalendarYear(LCalendarStartMonth,ATimeControl.StartYear,ATimeControl.StartMonth);
      LStartMonth         := ConvertHydrologicalMonthToCalendarMonth(LCalendarStartMonth,ATimeControl.StartYear,ATimeControl.StartMonth);
      LEndYear            := ConvertHydrologicalYearToCalendarYear(LCalendarStartMonth,ATimeControl.EndYear,ATimeControl.EndMonth);
      LEndMonth           := ConvertHydrologicalMonthToCalendarMonth(LCalendarStartMonth,ATimeControl.EndYear,ATimeControl.EndMonth);

      CbxYearActive.SetFieldIndex(CbxYearActive.Items.IndexOf(IntToStr(LStartYear)));
      CbxMonthActive.SetFieldIndex(LStartMonth-1);
      CbxYearObsolete.SetFieldIndex(CbxYearObsolete.Items.IndexOf(IntToStr(LEndYear)));
      CbxMonthObsolete.SetFieldIndex(LEndMonth-1);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TChannelSwitchControlValidator.RePopulateDataViewer;
const OPNAME = 'TChannelSwitchControlValidator.RePopulateDataViewer';
var
  lChannel       : IGeneralFlowChannel;
  lSwitchControl : IChannelSwitchControl;
  lIndex         : integer;
  lSwitchDefID   : integer;
  lSwitchDef     : ISwitchDefinition;
  lSwitchDefList : ISwitchDefinitionsList;
  lSwitchDefStr  : string;
  lReservoirList : IReservoirDataList;
  lReservoir     : IReservoirData;
  lSelectedIndex : integer;
  lChannelSwitchID : integer;
begin
  try
    if (FIdentifier >= 0) then
    begin
      lChannel := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.
                    ChannelList.ChannelByIdentifier[FIdentifier];
      if (lChannel <> nil) then
      begin
        with ChannelSwitchControlDialog do
        begin
          lSwitchDefList := (FAppModules.Model.ModelData as IPlanningModelData).SwitchDefinitionsList;
          CbxSwitchDefinition.Clear;
          CbxSwitchDefinition.AddItem(Get_SwitchDefinition(nil), TObject(0));
          for lIndex := 0 to lSwitchDefList.SwitchDefinitionCount - 1 do
          begin
            lSwitchDef    := lSwitchDefList.SwitchDefinitionByIndex[lIndex];
            lSwitchDefStr := Get_SwitchDefinition(lSwitchDef);
            CbxSwitchDefinition.AddItem(lSwitchDefStr, TObject(lSwitchDef.SwitchDefID));
          end;

          lReservoirList := TYieldModelDataObject(FAppModules.Model.ModelData).
                              NetworkElementData.ReservoirList;
          CbxAssociatedNode.Clear;

          for lIndex := 0 to lReservoirList.ReservoirCount - 1 do
          begin
            lReservoir := lReservoirList.ReservoirByIndex[lIndex];
            CbxAssociatedNode.AddItem(lReservoir.ReservoirConfigurationData.ReservoirName,
                                      TObject(lReservoir.ReservoirConfigurationData.ReservoirIdentifier));
          end;

          GrdSwitchControls.RowCount := lChannel.SwitchControlCount + 1;
          if (GrdSwitchControls.RowCount > 1) then
            GrdSwitchControls.FixedRows := 1;

          GrdSwitchControls.Cells[0, 0] := 'Switch Controls';
          lSelectedIndex := 0;
          TimeRelatedGroup.Visible := (lChannel.TimeControl <> nil);
          LevelRelatedGroup.Visible := (lChannel.SwitchControlCount >0);
          if LevelRelatedGroup.Visible then
          begin
            for lIndex := 0 to lChannel.SwitchControlCount - 1 do
            begin

              lSwitchControl   := lChannel.SwitchControlByIndex[lIndex];
              lChannelSwitchID := lSwitchControl.ChannelSwitchID;
              lSwitchDefID     := lSwitchControl.SwitchDefinitionID;
              GrdSwitchControls.Objects[0, lIndex+1] := TObject(lChannelSwitchID);

              if (FChannelSwitchID = 0) then
              begin
                FChannelSwitchID := lChannelSwitchID;
                lChannel.SelectedSwitchControlID := FChannelSwitchID;
              end;
              if (lChannelSwitchID = FChannelSwitchID) then
                lSelectedIndex := lIndex + 1;

              lSwitchDef     := nil;
              if (lSwitchDefID <> 0) then
                lSwitchDef   := (FAppModules.Model.ModelData as IPlanningModelData).
                                   SwitchDefinitionsList.SwitchDefinitionByID[lSwitchDefID];
              if (lSwitchDef <> nil) then
                GrdSwitchControls.Cells[0, lIndex+1] := Get_SwitchDefinition(lSwitchDef)
              else
                GrdSwitchControls.Cells[0, lIndex+1] := Get_SwitchDefinition(nil);
            end;
          end
          else
          begin
            if (lChannel.TimeControl <> nil) then
            begin
              PopulateDateValues(lChannel.TimeControl);
              //CbxYearActive.SetFieldValue(IntToStr(lChannel.TimeControl.StartYear));
              //CbxMonthActive.SetFieldValue(IntToStr(lChannel.TimeControl.StartMonth));
              //CbxYearObsolete.SetFieldValue(IntToStr(lChannel.TimeControl.EndYear));
              //CbxMonthObsolete.SetFieldValue(IntToStr(lChannel.TimeControl.EndMonth));
            end;
          end;
          ClearDataViewer;
          if (lChannel.SwitchControlCount > 0) then
          begin
            PnlClient.Visible := TRUE;
            if (GrdSwitchControls.Row <> lSelectedIndex) then
              GrdSwitchControls.Row := lSelectedIndex
            else
              PopulateSwitchControl(lChannel);
          end
          else
            PnlClient.Visible := FALSE;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TChannelSwitchControlValidator.UpdateYearActive;
const OPNAME = 'TDisbenefitFunctionDefinitionDataValidator.UpdateYearActive';
var
  lChannelList        : IChannelList;
  lChannel            : IGeneralFlowChannel;
  lMessage            : string;

  LCalendarStartMonth,
  LHydroYear,
  LHydroMonth,
  LCalendarYear,
  LCalendarMonth : integer;
begin
  try
    lChannelList := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData
                    .ChannelList;
    with ChannelSwitchControlDialog do
    begin
      lChannel   := lChannelList.ChannelByIdentifier[Identifier];
      if (lChannel <> nil) AND (lChannel.TimeControl <> nil) then
      begin
        if (FAppModules.FieldProperties.ValidateFieldProperty(
            CbxYearActive.FieldProperty.FieldName, CbxYearActive.Text, lMessage)) then
        begin
          CbxYearActive.ValidationError := lMessage;

          LCalendarStartMonth := FAppModules.StudyArea.CalendarStartMonth;
          LCalendarYear          := StrToInt(CbxYearActive.Text);
          LCalendarMonth         := CbxMonthActive.ItemIndex+1;
          LHydroMonth            :=  ConvertCalendarMonthToHydrologicalMonth(LCalendarStartMonth,LCalendarYear,LCalendarMonth);
          LHydroYear             :=  ConvertCalendarYearToHydrologicalYear(LCalendarStartMonth,LCalendarYear,LCalendarMonth);

          lChannel.TimeControl.StartYear := LHydroYear;
          if(lChannel.TimeControl.StartMonth <> LHydroMonth) then
            lChannel.TimeControl.StartMonth := LHydroMonth;

          DoContextValidation(dvtYearActive);
          PopulateDateValues(lChannel.TimeControl);
        end
        else
          CbxYearActive.ValidationError := lMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;
procedure TChannelSwitchControlValidator.UpdateMonthActive;
const OPNAME = 'TDisbenefitFunctionDefinitionDataValidator.UpdateMonthActive';
var
  lChannelList        : IChannelList;
  lChannel            : IGeneralFlowChannel;
  lMessage            : string;

  LCalendarStartMonth,
  LHydroYear,
  LHydroMonth,
  LCalendarYear,
  LCalendarMonth : integer;
begin
  try
    lChannelList := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData
                    .ChannelList;
    with ChannelSwitchControlDialog do
    begin
      lChannel   := lChannelList.ChannelByIdentifier[Identifier];
      if (lChannel <> nil) AND (lChannel.TimeControl <> nil) then
      begin
        if (FAppModules.FieldProperties.ValidateFieldProperty(
            CbxMonthActive.FieldProperty.FieldName, CbxMonthActive.Text, lMessage)) then
        begin
          CbxMonthActive.ValidationError := lMessage;

          LCalendarStartMonth := FAppModules.StudyArea.CalendarStartMonth;
          LCalendarYear          := StrToInt(CbxYearActive.Text);
          LCalendarMonth         := CbxMonthActive.ItemIndex+1;
          LHydroMonth            :=  ConvertCalendarMonthToHydrologicalMonth(LCalendarStartMonth,LCalendarYear,LCalendarMonth);
          LHydroYear             :=  ConvertCalendarYearToHydrologicalYear(LCalendarStartMonth,LCalendarYear,LCalendarMonth);

          lChannel.TimeControl.StartMonth := LHydroMonth;
          if(lChannel.TimeControl.StartYear <> LHydroYear) then
            lChannel.TimeControl.StartYear := LHydroYear;

          DoContextValidation(dvtMonthActive);
          PopulateDateValues(lChannel.TimeControl);
        end
        else
          CbxMonthActive.ValidationError := lMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TChannelSwitchControlValidator.UpdateYearAbsolete;
const OPNAME = 'TDisbenefitFunctionDefinitionDataValidator.UpdateYearAbsolete';
var
  lChannelList        : IChannelList;
  lChannel            : IGeneralFlowChannel;
  lMessage            : string;

  LCalendarStartMonth,
  LHydroYear,
  LHydroMonth,
  LCalendarYear,
  LCalendarMonth : integer;
begin
  try
    lChannelList := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData
                    .ChannelList;
    with ChannelSwitchControlDialog do
    begin
      lChannel   := lChannelList.ChannelByIdentifier[Identifier];
      if (lChannel <> nil) AND (lChannel.TimeControl <> nil) then
      begin
        if (FAppModules.FieldProperties.ValidateFieldProperty(
            CbxYearObsolete.FieldProperty.FieldName, CbxYearObsolete.Text, lMessage)) then
        begin
          CbxYearObsolete.ValidationError := lMessage;

          LCalendarStartMonth := FAppModules.StudyArea.CalendarStartMonth;
          LCalendarYear          := StrToInt(CbxYearObsolete.Text);
          LCalendarMonth         := CbxMonthObsolete.ItemIndex+1;
          LHydroMonth            :=  ConvertCalendarMonthToHydrologicalMonth(LCalendarStartMonth,LCalendarYear,LCalendarMonth);
          LHydroYear             :=  ConvertCalendarYearToHydrologicalYear(LCalendarStartMonth,LCalendarYear,LCalendarMonth);

          lChannel.TimeControl.EndYear := LHydroYear;
          if(lChannel.TimeControl.EndMonth <> LHydroMonth) then
            lChannel.TimeControl.EndMonth := LHydroMonth;

          DoContextValidation(dvtYearObsolete);
          PopulateDateValues(lChannel.TimeControl);
        end
        else
          CbxYearObsolete.ValidationError := lMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TChannelSwitchControlValidator.UpdateMonthAbsolete;
const OPNAME = 'TChannelSwitchControlValidator.UpdateMonthAbsolete';
var
  lChannelList        : IChannelList;
  lChannel            : IGeneralFlowChannel;
  lMessage            : string;

  LCalendarStartMonth,
  LHydroYear,
  LHydroMonth,
  LCalendarYear,
  LCalendarMonth : integer;
begin
  try
    lChannelList := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData
                    .ChannelList;
    with ChannelSwitchControlDialog do
    begin
      lChannel   := lChannelList.ChannelByIdentifier[Identifier];
      if (lChannel <> nil) AND (lChannel.TimeControl <> nil) then
      begin
        if (FAppModules.FieldProperties.ValidateFieldProperty(
            CbxMonthObsolete.FieldProperty.FieldName, CbxMonthObsolete.Text, lMessage)) then
        begin
          CbxMonthObsolete.ValidationError := lMessage;

          LCalendarStartMonth := FAppModules.StudyArea.CalendarStartMonth;
          LCalendarYear          := StrToInt(CbxYearObsolete.Text);
          LCalendarMonth         := CbxMonthObsolete.ItemIndex+1;
          LHydroMonth            :=  ConvertCalendarMonthToHydrologicalMonth(LCalendarStartMonth,LCalendarYear,LCalendarMonth);
          LHydroYear             :=  ConvertCalendarYearToHydrologicalYear(LCalendarStartMonth,LCalendarYear,LCalendarMonth);

          lChannel.TimeControl.EndYear := LHydroYear;
          if(lChannel.TimeControl.EndMonth <> LHydroMonth) then
            lChannel.TimeControl.EndMonth := LHydroMonth;

          DoContextValidation(dvtMonthObsolete);
          PopulateDateValues(lChannel.TimeControl);
        end
        else
          CbxMonthObsolete.ValidationError := lMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TChannelSwitchControlValidator.OnGrdSwitchControlSelectCell (Sender        : TObject;
                                                                       ACol, ARow    : Integer;
                                                                       var CanSelect : Boolean);
const OPNAME = 'TChannelSwitchControlValidator.OnGrdSwitchControlSelectCell';
var
  lChannel : IGeneralFlowChannel;
begin
  try
    if (FIdentifier >= 0) then
    begin
      lChannel := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.
                    ChannelList.ChannelByIdentifier[FIdentifier];
      if (lChannel <> nil) AND (lChannel.SwitchControlCount > 0) then
      begin
        FChannelSwitchID := Integer(ChannelSwitchControlDialog.GrdSwitchControls.Objects[0, ARow]);
        lChannel.SelectedSwitchControlID := FChannelSwitchID;
        PopulateSwitchControl(lChannel);
        DoContextValidation(dvtChannelSwitchControlAll);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TChannelSwitchControlValidator.Get_SwitchDefinition(ASwitchDef : ISwitchDefinition): string;
const OPNAME = 'TChannelSwitchControlValidator.Get_SwitchDefinition';
var
  LCalendarStartMonth,
  LStartYear,
  LStartMonth : integer;
begin
  Result := 'Undefined';
  try
    if(ASwitchDef <> nil) then
    begin
      if(ASwitchDef.SwitchDefStartYear > 0) and ((ASwitchDef.SwitchDefStartMonth >= 1) and (ASwitchDef.SwitchDefStartMonth <= 12)) then
      begin
        LCalendarStartMonth := FAppModules.StudyArea.CalendarStartMonth;
        LStartYear          := ConvertHydrologicalYearToCalendarYear(LCalendarStartMonth,ASwitchDef.SwitchDefStartYear,ASwitchDef.SwitchDefStartMonth);
        LStartMonth         := ConvertHydrologicalMonthToCalendarMonth(LCalendarStartMonth,ASwitchDef.SwitchDefStartYear,ASwitchDef.SwitchDefStartMonth);
        Result              := IntToStr(LStartYear) + '/' + FormatSettings.ShortMonthNames[LStartMonth];
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TChannelSwitchControlValidator.PopulateSwitchControl (AChannel : IGeneralFlowChannel);
const OPNAME = 'TChannelSwitchControlValidator.PopulateSwitchControl';
var
  lSwitchControl : IChannelSwitchControl;
  lReservoir     : IReservoirData;
  lSwitchDefStr  : string;
  lSwitchDef     : ISwitchDefinition;
begin
  try
    with ChannelSwitchControlDialog do
    begin
      if (FChannelSwitchID > 0) then
        lSwitchControl := AChannel.SwitchControlByID[FChannelSwitchID]
      else
        lSwitchControl := nil;

      if (lSwitchControl <> nil) then
      begin
        CbxSwitchDefinition.ItemIndex := -1;
        if (lSwitchControl.SwitchDefinitionID = 0) then
          CbxSwitchDefinition.ItemIndex := 0
        else
        if (lSwitchControl.SwitchDefinitionID > 0) then
        begin
          lSwitchDef := (FAppModules.Model.ModelData as IPlanningModelData).
                          SwitchDefinitionsList.SwitchDefinitionByID[lSwitchControl.SwitchDefinitionID];
          if (lSwitchDef <> nil) then
          begin
            lSwitchDefStr := Get_SwitchDefinition(lSwitchDef);
            CbxSwitchDefinition.SetFieldIndex(CbxSwitchDefinition.Items.IndexOf(lSwitchDefStr));
          end;
        end;

        CbxAssociatedNode.ItemIndex := -1;
        if (lSwitchControl.AssociatedNodeNr <> 0) then
        begin
          lReservoir := TYieldModelDataObject(FAppModules.Model.ModelData).
                          NetworkElementData.ReservoirList.ReservoirByIdentifier[lSwitchControl.AssociatedNodeNr];
          if (lReservoir <> nil) then
            CbxAssociatedNode.SetFieldIndex(CbxAssociatedNode.Items.IndexOf(lReservoir.ReservoirConfigurationData.ReservoirName));
        end;

        EdtWaterLevel.SetFieldValue(lSwitchControl.WaterLevel);
        RgpSwitchType.ItemIndex := lSwitchControl.SwitchType - 1;
        if (lSwitchControl.SwitchType = 3) then
        begin
          RgpInitialStatus.Enabled   := TRUE;
          RgpInitialStatus.ItemIndex := lSwitchControl.InitialStatus;
        end
        else
        begin
          RgpInitialStatus.Enabled   := FALSE;
          RgpInitialStatus.ItemIndex := -1;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TChannelSwitchControlValidator.SaveState: boolean;
const OPNAME = 'TChannelSwitchControlValidator.SaveState';
begin
  Result := inherited SaveState;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TChannelSwitchControlValidator.OnEditControlEnter(Sender: TObject);
const OPNAME = 'TChannelSwitchControlValidator.OnEditControlEnter';
begin
  inherited OnEditControlEnter(Sender);
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TChannelSwitchControlValidator.OnEditControltExit(Sender: TObject);
const OPNAME = 'TChannelSwitchControlValidator.OnEditControltExit';
begin
  inherited OnEditControltExit(Sender);
  try
    with ChannelSwitchControlDialog do
    begin
      if (Sender = CbxSwitchDefinition) AND (CbxSwitchDefinition.HasValueChanged) then
        UpdateSwitchDefinition
      else
      if (Sender = CbxAssociatedNode) AND (CbxAssociatedNode.HasValueChanged) then
        UpdateAssociatedNode
      else
      if (Sender = EdtWaterLevel) AND (EdtWaterLevel.HasValueChanged) then
        UpdateWaterLevel
       else if ((Sender = CbxYearActive) AND (CbxYearActive.HasValueChanged)) then
        UpdateYearActive
      else if ((Sender = CbxMonthActive) AND (CbxMonthActive.HasValueChanged)) then
        UpdateMonthActive
      else if ((Sender = CbxYearObsolete) AND (CbxYearObsolete.HasValueChanged)) then
        UpdateYearAbsolete
      else if ((Sender = CbxMonthObsolete) AND (CbxMonthObsolete.HasValueChanged)) then
        UpdateMonthAbsolete;


    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TChannelSwitchControlValidator.OnRgpSwitchTypeClick(Sender: TObject);
const OPNAME = 'TChannelSwitchControlValidator.OnRgpSwitchTypeClick';
begin
  try
    if(ChannelSwitchControlDialog.RgpSwitchType.HasValueChanged) then
      UpdateSwitchType;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TChannelSwitchControlValidator.OnRgpInitialStatusClick(Sender: TObject);
const OPNAME = 'TChannelSwitchControlValidator.OnRgpInitialStatusClick';
begin
  try
    if(ChannelSwitchControlDialog.RgpInitialStatus.HasValueChanged) then
      UpdateInitialStatus;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TChannelSwitchControlValidator.ChannelSwitchControlDialog : TChannelSwitchControlDialog;
const OPNAME = 'TChannelSwitchControlValidator.ChannelSwitchControlDialog';
begin
  Result := Nil;
  try
    if Assigned(FPanel) then
      Result := TChannelSwitchControlDialog(FPanel);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TChannelSwitchControlValidator.StudyDataHasChanged (AContext   : TChangeContext;
                                                           AFieldName : string;
                                                           AOldValue  : string;
                                                           ANewValue  : string): boolean;
const OPNAME = 'TChannelSwitchControlValidator.StudyDataHasChanged';
begin
  Result := inherited StudyDataHasChanged(AContext,AFieldName,AOldValue,ANewValue);
  try
    if (AFieldName = 'ChannelYearsToConstruct') then
      RePopulateDataViewer;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TChannelSwitchControlValidator.StudyHasChanged: boolean;
const OPNAME = 'TChannelSwitchControlValidator.StudyHasChanged';
begin
  Result := inherited StudyHasChanged;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;
(*
procedure TChannelSwitchControlValidator.DoAddSwitchControl (Sender: TObject);
const OPNAME = 'TChannelSwitchControlValidator.DoAddSwitchControl';
begin
  try
    FAppModules.Model.ProcessEvent(CmeCreateChannelSwitchCntrl, nil);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TChannelSwitchControlValidator.DoDeleteSwitchControl(Sender: TObject);
const OPNAME = 'TChannelSwitchControlValidator.DoDeleteSwitchControl';
begin
  try
    FAppModules.Model.ProcessEvent(CmeDeleteChannelSwitchCntrl, nil);
  except on E: Exception do HandleError(E, OPNAME) end;
end;
*)

procedure TChannelSwitchControlValidator.DoAddSwitchControl (Sender: TObject);
const OPNAME = 'TChannelSwitchControlValidator.DoAddSwitchControl';
var
  lChannelList   : IChannelList;
  lChannel       : IGeneralFlowChannel;
  lSwitchControl : IChannelSwitchControl;
  LChannelTimeControl : IChannelTimeControl;
  LSwitchDialog  : TSwitchDialog;
begin
  try
    lChannelList := TYieldModelDataObject(FAppModules.Model.ModelData).
                        NetworkElementData.ChannelList;
    lChannel     := lChannelList.ChannelByIdentifier[FIdentifier];
    if (lChannel <> nil) then
    begin
      try
        LSwitchDialog  := TSwitchDialog.CreateWithoutDFM(nil,AppModules);
        LSwitchDialog.Initialise;
        LSwitchDialog.LanguageHasChanged;
        LSwitchDialog.ShowModal;
         if (LSwitchDialog.ModalResult = mrOK) then
         begin
           FRelationType := LSwitchDialog.SelectedRelationType;
           if (FRelationType = 1) then
           begin
             if (lChannel.SwitchControlByChannelNumber[lChannel.ChannelNumber] = nil) and (lChannel.TimeControl = nil) then
             begin
               lSwitchControl   := lChannel.NewSwitchControl;
               FChannelSwitchID := lSwitchControl.ChannelSwitchID;
             end;
           end
           else
           if (FRelationType = 0) then
           begin
             if (lChannel.TimeControl = nil) and (lChannel.SwitchControlCount<=0) then
               LChannelTimeControl := lChannel.NewTimeControl;
           end;
         end;
      finally
        FreeAndNil(LSwitchDialog);
      end;
      RePopulateDataViewer;
      DoContextValidation(dvtChannelSwitchControlAll);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TChannelSwitchControlValidator.DoDeleteSwitchControl(Sender: TObject);
const OPNAME = 'TChannelSwitchControlValidator.DoDeleteSwitchControl';
var
  lChannelList   : IChannelList;
  lChannel       : IGeneralFlowChannel;
  lSwitchControl : IChannelSwitchControl;
begin
  try
    lChannelList := TYieldModelDataObject(FAppModules.Model.ModelData).
                        NetworkElementData.ChannelList;
    lChannel     := lChannelList.ChannelByIdentifier[FIdentifier];
    lSwitchControl :=  lChannel.SwitchControlByChannelNumber[lChannel.ChannelNumber];
    if (lChannel <> nil) AND (lSwitchControl <> nil) then
    begin
      if (lChannel.RemoveSwitchControl(lSwitchControl.ChannelSwitchID)) then
      begin
        FChannelSwitchID := 0;
      end;
    end
    else
    if (lChannel <> nil) AND (lChannel.TimeControl <> nil) then
    begin
      lChannel.RemoveTimeControl;
    end;
      RePopulateDataViewer;
      DoContextValidation(dvtChannelSwitchControlAll);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TChannelSwitchControlValidator.UpdateSwitchDefinition;
const OPNAME = 'TChannelSwitchControlValidator.UpdateSwitchDefinition';
var
  lChannelList   : IChannelList;
  lChannel       : IGeneralFlowChannel;
  lSwitchControl : IChannelSwitchControl;
  lMessage       : string;
  lValStr        : string;
begin
  try
    lChannelList := TYieldModelDataObject(FAppModules.Model.ModelData).
                        NetworkElementData.ChannelList;
    lChannel     := lChannelList.ChannelByIdentifier[FIdentifier];
    if (lChannel <> nil) AND (FChannelSwitchID <> 0) then
    begin
      lSwitchControl := lChannel.SwitchControlByID[FChannelSwitchID];
      with ChannelSwitchControlDialog do
      begin
        CbxSwitchDefinition.ValidationError := '';
        if (CbxSwitchDefinition.ItemIndex >= 0) then
          lValStr := IntToStr(Integer(CbxSwitchDefinition.Items.Objects[CbxSwitchDefinition.ItemIndex]))
        else
          lValStr := '0';
        if (FAppModules.FieldProperties.ValidateFieldProperty(
            CbxSwitchDefinition.FieldProperty.FieldName, lValStr, lMessage)) then
        begin
          CbxSwitchDefinition.ValidationError := lMessage;
          lSwitchControl.SwitchDefinitionID := StrToInt(lValStr);
          RePopulateDataViewer;
          DoContextValidation(dvtSwitchDefinitionID);
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TChannelSwitchControlValidator.UpdateAssociatedNode;
const OPNAME = 'TChannelSwitchControlValidator.UpdateAssociatedNode';
var
  lChannelList   : IChannelList;
  lChannel       : IGeneralFlowChannel;
  lSwitchControl : IChannelSwitchControl;
  lMessage       : string;
  lValStr        : string;
begin
  try
    lChannelList := TYieldModelDataObject(FAppModules.Model.ModelData).
                        NetworkElementData.ChannelList;
    lChannel     := lChannelList.ChannelByIdentifier[FIdentifier];
    if (lChannel <> nil) AND (FChannelSwitchID <> 0) then
    begin
      lSwitchControl := lChannel.SwitchControlByID[FChannelSwitchID];
      with ChannelSwitchControlDialog do
      begin
        CbxAssociatedNode.ValidationError := '';
        if (CbxAssociatedNode.ItemIndex >= 0) then
          lValStr := IntToStr(Integer(CbxAssociatedNode.Items.Objects[CbxAssociatedNode.ItemIndex]))
        else
          lValStr := '0';
        if (FAppModules.FieldProperties.ValidateFieldProperty(
            CbxAssociatedNode.FieldProperty.FieldName, lValStr, lMessage)) then
        begin
          CbxAssociatedNode.ValidationError := lMessage;
          lSwitchControl.AssociatedNodeNr := StrToInt(lValStr);
          DoContextValidation(dvtSwitchAssociatedNodeNr);
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TChannelSwitchControlValidator.UpdateWaterLevel;
const OPNAME = 'TChannelSwitchControlValidator.UpdateWaterLevel';
var
  lChannelList   : IChannelList;
  lChannel       : IGeneralFlowChannel;
  lSwitchControl : IChannelSwitchControl;
  lMessage       : string;
begin
  try
    lChannelList := TYieldModelDataObject(FAppModules.Model.ModelData).
                        NetworkElementData.ChannelList;
    lChannel     := lChannelList.ChannelByIdentifier[FIdentifier];
    if (lChannel <> nil) AND (FChannelSwitchID <> 0) then
    begin
      lSwitchControl := lChannel.SwitchControlByID[FChannelSwitchID];
      with ChannelSwitchControlDialog do
      begin
        if (FAppModules.FieldProperties.ValidateFieldProperty(
            EdtWaterLevel.FieldProperty.FieldName, Trim(EdtWaterLevel.Text), lMessage)) then
        begin
          EdtWaterLevel.FieldValidationError := lMessage;
          lSwitchControl.WaterLevel := StrToFloat(Trim(EdtWaterLevel.Text));
          EdtWaterLevel.SetFieldValue(lSwitchControl.WaterLevel);
          DoContextValidation(dvtSwitchWaterlevel);
        end
        else
          EdtWaterLevel.FieldValidationError := lMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TChannelSwitchControlValidator.UpdateSwitchType;
const OPNAME = 'TChannelSwitchControlValidator.UpdateSwitchType';
var
  lChannelList   : IChannelList;
  lChannel       : IGeneralFlowChannel;
  lSwitchControl : IChannelSwitchControl;
  lMessage       : string;
begin
  try
    lChannelList := TYieldModelDataObject(FAppModules.Model.ModelData).
                        NetworkElementData.ChannelList;
    lChannel     := lChannelList.ChannelByIdentifier[FIdentifier];
    if (lChannel <> nil) AND (FChannelSwitchID <> 0) then
    begin
      lSwitchControl := lChannel.SwitchControlByID[FChannelSwitchID];
      with ChannelSwitchControlDialog do
      begin
        if (RgpSwitchType.ItemIndex <> lSwitchControl.SwitchType - 1) then
        begin
          if (FAppModules.FieldProperties.ValidateFieldProperty(
              RgpSwitchType.FieldProperty.FieldName, IntToStr(RgpSwitchType.ItemIndex + 1), lMessage)) then
          begin
            RgpSwitchType.ValidationError := lMessage;
            lSwitchControl.SwitchType     := RgpSwitchType.ItemIndex + 1;
            RgpSwitchType.ItemIndex       := lSwitchControl.SwitchType - 1;
            PopulateSwitchControl(lChannel);
            DoContextValidation(dvtSwitchType);
          end
          else
            RgpSwitchType.ValidationError := lMessage;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TChannelSwitchControlValidator.UpdateInitialStatus;
const OPNAME = 'TChannelSwitchControlValidator.UpdateInitialStatus';
var
  lChannelList   : IChannelList;
  lChannel       : IGeneralFlowChannel;
  lSwitchControl : IChannelSwitchControl;
  lMessage       : string;
begin
  try
    lChannelList := TYieldModelDataObject(FAppModules.Model.ModelData).
                        NetworkElementData.ChannelList;
    lChannel     := lChannelList.ChannelByIdentifier[FIdentifier];
    if (lChannel <> nil) AND (FChannelSwitchID <> 0) then
    begin
      lSwitchControl := lChannel.SwitchControlByID[FChannelSwitchID];
      with ChannelSwitchControlDialog do
      begin
        if (FAppModules.FieldProperties.ValidateFieldProperty(
            RgpInitialStatus.FieldProperty.FieldName, IntToStr(RgpInitialStatus.ItemIndex), lMessage)) then
        begin
          RgpInitialStatus.ValidationError := lMessage;
          lSwitchControl.InitialStatus     := RgpInitialStatus.ItemIndex;
          RgpInitialStatus.ItemIndex       := lSwitchControl.InitialStatus;
          DoContextValidation(dvtSwitchInitialStatus);
        end
        else
          RgpSwitchType.ValidationError := lMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TChannelSwitchControlValidator.DoContextValidation (AValidationType : TDialogValidationType);
const OPNAME = 'TChannelSwitchControlValidator.DoContextValidation';
var
  lChannel       : IGeneralFlowChannel;
  lChannelList   : IChannelList;
  lSwitchControl : IChannelSwitchControl;
begin
  try
    FAllErrorMessages.Clear;
    if (FIdentifier >= 0) then
    begin
      try
        lChannelList := TYieldModelDataObject(FAppModules.Model.ModelData).
                          NetworkElementData.ChannelList;
        lChannel     := lChannelList.ChannelByIdentifier[FIdentifier];
        if (lChannel <> nil) AND (FChannelSwitchID <> 0) then
        begin
          lSwitchControl := lChannel.SwitchControlByID[FChannelSwitchID];
          if (lSwitchControl <> nil) then
          begin
            if (AValidationType = dvtChannelSwitchControlAll) OR
               (AValidationType = dvtSwitchDefinitionID) then
              ValidateSwitchDefinition(lSwitchControl);
            if (AValidationType = dvtChannelSwitchControlAll) OR
               (AValidationType = dvtSwitchAssociatedNodeNr) then
              ValidateSwitchAssociatedNodeNr(lSwitchControl);
            if (AValidationType = dvtChannelSwitchControlAll) OR
               (AValidationType = dvtSwitchWaterlevel) then
              ValidateSwitchWaterLevel(lSwitchControl);
            if (AValidationType = dvtChannelSwitchControlAll) OR
               (AValidationType = dvtSwitchType) then
              ValidateSwitchType(lSwitchControl);
            if (AValidationType = dvtChannelSwitchControlAll) OR
               (AValidationType = dvtSwitchInitialStatus) then
              ValidateSwitchInitialStatus(lSwitchControl);
          end;
        end;
      finally
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TChannelSwitchControlValidator.ValidateSwitchDefinition (ASwitchControl : IChannelSwitchControl);
const OPNAME = 'TChannelSwitchControlValidator.ValidateSwitchDefinition';
begin
  try
    with ChannelSwitchControlDialog do
    begin
      FErrorMessage := '';
      if (NOT ASwitchControl.Validate(FErrorMessage, 'SwitchDefinitionID')) then
      begin
        CbxSwitchDefinition.InValidationError := TRUE;
        CbxSwitchDefinition.ValidationError := FErrorMessage;
        CbxSwitchDefinition.ShowErrorState(TRUE);
        FAllErrorMessages.Add(Trim(FErrorMessage));
      end
      else
      begin
        CbxSwitchDefinition.InValidationError := FALSE;
        CbxSwitchDefinition.ShowErrorState(FALSE);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TChannelSwitchControlValidator.ValidateSwitchAssociatedNodeNr (ASwitchControl : IChannelSwitchControl);
const OPNAME = 'TChannelSwitchControlValidator.ValidateSwitchAssociatedNodeNr';
begin
  try
    with ChannelSwitchControlDialog do
    begin
      FErrorMessage := '';
      if (NOT ASwitchControl.Validate(FErrorMessage, 'SwitchAssociatedNodeNr')) then
      begin
        CbxAssociatedNode.InValidationError := TRUE;
        CbxAssociatedNode.ValidationError := FErrorMessage;
        CbxAssociatedNode.ShowErrorState(TRUE);
        FAllErrorMessages.Add(Trim(FErrorMessage));
      end
      else
      begin
        CbxAssociatedNode.InValidationError := FALSE;
        CbxAssociatedNode.ShowErrorState(FALSE);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TChannelSwitchControlValidator.ValidateSwitchWaterLevel (ASwitchControl : IChannelSwitchControl);
const OPNAME = 'TChannelSwitchControlValidator.ValidateSwitchWaterLevel';
begin
  try
    with ChannelSwitchControlDialog do
    begin
      FErrorMessage := '';
      if (NOT ASwitchControl.Validate(FErrorMessage, 'SwitchWaterlevel')) then
        FAllErrorMessages.Add(Trim(FErrorMessage));
      EdtWaterLevel.ContextValidationError := FErrorMessage;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TChannelSwitchControlValidator.ValidateSwitchType (ASwitchControl : IChannelSwitchControl);
const OPNAME = 'TChannelSwitchControlValidator.ValidateSwitchType';
begin
  try
    with ChannelSwitchControlDialog do
    begin
      FErrorMessage := '';
      if (NOT ASwitchControl.Validate(FErrorMessage, 'SwitchType')) then
      begin
        FAllErrorMessages.Add(Trim(FErrorMessage));
        RgpSwitchType.ValidationError := FErrorMessage;
        RgpSwitchType.InValidationError := TRUE;
        RgpSwitchType.ShowErrorState(TRUE);
      end
      else
      begin
        RgpSwitchType.ValidationError := '';
        RgpSwitchType.InValidationError := FALSE;
        RgpSwitchType.ShowErrorState(FALSE);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TChannelSwitchControlValidator.ValidateSwitchInitialStatus (ASwitchControl : IChannelSwitchControl);
const OPNAME = 'TChannelSwitchControlValidator.ValidateSwitchInitialStatus';
begin
  try
    with ChannelSwitchControlDialog do
    begin
      FErrorMessage := '';
      if (NOT ASwitchControl.Validate(FErrorMessage, 'SwitchInitialStatus')) then
      begin
        FAllErrorMessages.Add(Trim(FErrorMessage));
        RgpInitialStatus.ValidationError := FErrorMessage;
        RgpInitialStatus.InValidationError := TRUE;
        RgpInitialStatus.ShowErrorState(TRUE);
      end
      else
      begin
        RgpInitialStatus.ValidationError := '';
        RgpInitialStatus.InValidationError := FALSE;
        RgpInitialStatus.ShowErrorState(FALSE);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.


