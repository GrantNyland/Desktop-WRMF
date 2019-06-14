{******************************************************************************}
{*  UNIT      : Contains the class TChannelPenaltyvalidator.                  *}
{*  AUTHOR    : Riana Steyn                                                   *}
{*  DATE      : 2003/09/05                                                    *}
{*  COPYRIGHT : Copyright © 2003 DWAF                                         *}
{******************************************************************************}

unit UChannelPenaltyValidator;

interface

uses

  Classes,
  VCL.Controls,
  VCL.ComCtrls,
  VCL.StdCtrls,
  VCL.ExtCtrls,

  UWRMFThemes,
  UAbstractObject,
  UAbstractComponent,
  UDataComponent,
  UDataEditComponent,
  VoaimsCom_TLB,
  UChannelPenaltyDialog,
  UAbstractYieldDataDialogValidator,
  UYieldContextValidationType;

type
  TChannelPenaltyValidator = class(TAbstractYieldDataDialogValidator)
  protected
    {protected}
    FChannelNumber : integer;
    FArcCountSet   : TArcCountSet;
    FBusyUpdating : boolean;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;

    procedure OnEditControlEnter(ASender: TObject); override;
    procedure OnStringGridColEnter(Sender: TObject;ACol, ARow: integer); override;
    procedure OnEditControltExit(ASender: TObject); override;
    procedure OnStringGridCellDataHasChanged(ASender: TObject; ACol, ARow: integer);  override;
    procedure OnChannelPenaltyDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure OnChannelPenaltyDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
    procedure OnChannelDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);

    procedure RePopulateDataViewer;
    procedure RePopulatePenaltyStructureGrid (APenaltyPanel : TChannelPenaltyPanel);
    procedure UpdatePenaltyValue(APanel : TChannelPenaltyPanel;
                                 AIndex : integer;
                                 AValue : string);
    procedure UpdatePenaltyName (APanel : TChannelPenaltyPanel;
                                 AValue : string);
    procedure UpdateInflowPenaltyNocmbox(AValue : integer);
    procedure SelectPenaltyPanel(APanel : TChannelPenaltyPanel);
    procedure SelectNewPanel(Sender: TObject);
    procedure RepopulateChannelListBox;
    procedure ResetChannelsLabel(APenaltyNr : integer);
    procedure OnNewPenaltyClick(Sender: TObject);
    procedure OnDeletePenaltyClick(Sender: TObject);
    procedure ValidatePenalty;
    procedure ValidateAllPenalties;
    procedure SetViewMode(AViewMode: TViewMode);override;
  public
    function Initialise: boolean; override;
    function SaveState: boolean; override;
    function LanguageHasChanged: boolean; override;
    function StudyHasChanged: boolean; override;
    function StudyDataHasChanged(AContext: TChangeContext; AFieldName,AOldValue,ANewValue: string): boolean; override;
    function ProcessParameterChangeEvent : boolean; override;
    function ProcessMetaDataEvent : boolean; override;

    procedure ClearDataViewer; override;
    procedure PopulateDataViewer; override;
    procedure DoContextValidation (AValidationType : TDialogValidationType); override;

    function ChannelPenaltyDialog : TChannelPenaltyDialog;
    property ChannelNumber: integer read FChannelNumber write FChannelNumber;
    property ArcCountSet   : TArcCountSet  read FArcCountSet write FArcCountSet;

  end;

implementation

uses
  UConstants,
  Contnrs,
  SysUtils,
  VCL.Graphics,
  UYieldModelDataObject,
  UErrorHandlingOperations,
  UNetworkElementData;

{******************************************************************************}
{* TChannelPenaltyValidator                                                   *}
{******************************************************************************}

procedure TChannelPenaltyValidator.CreateMemberObjects;
const OPNAME = 'TChannelPenaltyValidator.CreateMemberObjects';
var
  lPanel : TChannelPenaltyDialog;
begin
  try
    inherited CreateMemberObjects;
    FChannelNumber := NullInteger;
    FArcCountSet   := [];
    FBusyUpdating := False;
    FPanel         := TChannelPenaltyDialog.Create(FPanelOwner,FAppModules);
    lPanel         := ChannelPenaltyDialog;
    with lPanel do
    begin
      NewPenaltyButton.OnClick    := OnNewPenaltyClick;
      DeletePenaltyButton.OnClick := OnDeletePenaltyClick;
      ChannelsListBox.OnDragOver  := OnChannelDragOver;
      NewPenaltyButton.FieldProperty := FAppModules.FieldProperties.FieldProperty('Penalty');
      DeletePenaltyButton.FieldProperty := FAppModules.FieldProperties.FieldProperty('Penalty');
      if InflowPenaltyNocmbox <> nil then
      begin
        InflowPenaltyNocmbox.FieldProperty := FAppModules.FieldProperties.FieldProperty('InflowPenaltyNo');
        InflowPenaltyNocmbox.OnEnter            := OnEditControlEnter;
        InflowPenaltyNocmbox.OnExit             := OnEditControltExit;

      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TChannelPenaltyValidator.DestroyMemberObjects;
const OPNAME = 'TChannelPenaltyValidator.DestroyMemberObjects';
begin
  try
  except on E: Exception do HandleError(E, OPNAME) end;
  inherited DestroyMemberObjects;
end;

function TChannelPenaltyValidator.Initialise: boolean;
const OPNAME = 'TChannelPenaltyValidator.Initialise';
begin
  Result := inherited Initialise;
  try
    ChannelPenaltyDialog.HighlightSelectedPanel := False;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TChannelPenaltyValidator.LanguageHasChanged: boolean;
const OPNAME = 'TChannelPenaltyValidator.LanguageHasChanged';
begin
  Result := False;
  try
    TabShetCaption := FAppModules.Language.GetString('TabCaption.Penalty');;
    Result := inherited LanguageHasChanged;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TChannelPenaltyValidator.ClearDataViewer;
const OPNAME = 'TChannelPenaltyValidator.ClearDataViewer';
begin
  inherited ClearDataViewer;
  try
    ChannelPenaltyDialog.ActivePanel := nil;
    ChannelPenaltyDialog.PenaltyPanels.Clear;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TChannelPenaltyValidator.PopulateDataViewer;
const OPNAME = 'TChannelPenaltyValidator.PopulateDataViewer';
begin
  inherited PopulateDataViewer;
  try
    ClearDataViewer;
    RePopulateDataViewer;
    DoContextValidation(dvtChanPenaltyAll);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TChannelPenaltyValidator.RePopulateDataViewer;
const OPNAME = 'TChannelPenaltyValidator.RePopulateDataViewer';
var
  lPenaltyList      : IChannelPenaltyList;
  lIndexA           : integer;
  lIndexB           : integer;
  lPenaltyPanel     : TChannelPenaltyPanel;
  lChannelPenalty   : IChannelPenalty;
  lChannel          : IGeneralFlowChannel;
  lPenaltyNr        : integer;
  lArcCounts        : TStringList;
  lArcs             : string;
  lCheckIFR         : Boolean;
  lFieldProperty    : TAbstractFieldProperty;
  lKeyValues        : string;
  LInflowPenaltyNo  : integer;
begin
  try
    lArcCounts := TStringList.Create;
    try
      lPenaltyList := TYieldModelDataObject(FAppModules.Model.ModelData).
                        NetworkElementData.ChannelPenaltyList;
      lChannel     := TYieldModelDataObject(FAppModules.Model.ModelData).
                        NetworkElementData.ChannelList.ChannelByChannelNumber[FChannelNumber];
      if (lChannel <> nil) then
      begin
        if (lChannel.ChannelPenalty <> nil) then
          lPenaltyNr := lChannel.ChannelPenalty.ChannelPenaltyID
        else
          lPenaltyNr := NullInteger;
        lArcCounts.CommaText := lChannel.ValidArcCounts;
        lCheckIFR  := lChannel.IFRFeature <> nil;
        with ChannelPenaltyDialog do
        begin
          lArcs := '[';
          for lIndexA := 1 to 5 do
            if (lArcCounts.IndexOf(IntToStr(lIndexA)) >= 0) then
            begin
              if (Length(lArcs) > 1) then
                lArcs := lArcs + ', ';
              lArcs := lArcs + IntToStr(lIndexA)
            end;
          HeadingPenaltiesLabel.Caption := lArcs + '] '+ FAppModules.Language.GetString('Channel.NArcPenaltyStructures');
        end;
      end
      else
      begin
        lPenaltyNr := NullInteger;
        lCheckIFR  := FALSE;
        ChannelPenaltyDialog.HeadingPenaltiesLabel.Caption := FAppModules.Language.GetString('Channel.PenaltyStructures');
      end;

      ChannelPenaltyDialog.DeletePenaltyButton.Enabled := FALSE;

      SelectPenaltyPanel(nil);
      ChannelPenaltyDialog.ActivePanel := nil;
      ChannelPenaltyDialog.PenaltyPanels.Clear;
      ChannelPenaltyDialog.InflowPenaltyNocmbox.Items.Clear;
      for lIndexA := 0 to lPenaltyList.ChannelPenaltyCount - 1 do
      begin
        lChannelPenalty := lPenaltyList.ChannelPenaltyByIndex[lIndexA];
        if (ChannelPenaltyDialog.InflowPenaltyNocmbox <> nil) and (lChannelPenalty.ChannelPenaltyArcCount=1) then
        begin
          LInflowPenaltyNo := lChannelPenalty.ChannelPenaltyID;
          ChannelPenaltyDialog.InflowPenaltyNocmbox.Items.AddObject(lChannelPenalty.ChannelPenaltyName, TObject(LInflowPenaltyNo));

        end;


        if ((lChannel = nil) OR
            ((lChannel <> nil) AND (lChannel.ChannelPenalty <> nil) AND (lChannelPenalty = lChannel.ChannelPenalty)) OR
            ((lArcCounts.IndexOf(IntToStr(lChannelPenalty.ChannelPenaltyArcCount)) >= 0) AND
             ((NOT lCheckIFR) OR
              (lChannelPenalty.ChannelPenaltyValueByIndex[1] <
               lChannelPenalty.ChannelPenaltyValueByIndex[2])))) then
        begin
          if(FArcCountSet <> []) and (not (lChannelPenalty.ChannelPenaltyArcCount in FArcCountSet))then
            Continue;
          lPenaltyPanel := ChannelPenaltyDialog.AddChannelPenaltyPanel;
          lPenaltyPanel.OnClick := SelectNewPanel;
          lPenaltyPanel.SetEventHandlerOnDragOver(OnChannelPenaltyDragOver);
          lPenaltyPanel.SetEventHandlerOnDragDrop(OnChannelPenaltyDragDrop);
          with lPenaltyPanel do
          begin
            PenaltyNumber := lChannelPenalty.ChannelPenaltyID;
            IDLabel.Caption := IntToStr(lChannelPenalty.ChannelPenaltyID);
            DescriptionEdit.FieldProperty := FAppModules.FieldProperties.FieldProperty('ChannelPenaltyName');
            DescriptionEdit.OnEnter       := OnEditControlEnter;
            DescriptionEdit.OnExit        := OnEditControltExit;

            lFieldProperty := DescriptionEdit.FieldProperty;
            lKeyValues     := lChannelPenalty.GetKeyValues(lFieldProperty.FieldName, '');
            DescriptionEdit.HasMetaData :=
              FAppModules.MetaData.FindMetaData(lFieldProperty.FieldName, lKeyValues, '') <> nil;

            DescriptionEdit.SetFieldValue(lChannelPenalty.ChannelPenaltyName);
            if ((FViewMode = vmEditable) OR
                (FViewMode = vmEditableSelect)) then
              DescriptionEdit.IsEnabled := TRUE
            else
              DescriptionEdit.IsEnabled := FALSE;

            for lIndexB := 1 to 5 do
              ArcPenaltiesGrid.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('Penalty'));
            ArcPenaltiesGrid.OnBeforeCellChange    := OnStringGridCellDataHasChanged;
            ArcPenaltiesGrid.OnColEnter            := OnStringGridColEnter;
            ArcPenaltiesGrid.OnEnter               := OnEditControlEnter;

            RePopulatePenaltyStructureGrid(lPenaltyPanel);
          end;
        end;
      end;
      if lPenaltyList <> nil then
        if ChannelPenaltyDialog.InflowPenaltyNocmbox <> nil then
        begin
          ChannelPenaltyDialog.InflowPenaltyNocmbox.ItemIndex :=
          ChannelPenaltyDialog.InflowPenaltyNocmbox.Items.IndexOfObject(TObject(lPenaltyList.InflowPenaltyNo));
          ChannelPenaltyDialog.InflowPenaltyNocmbox.Enabled := ChannelPenaltyDialog.NewPenaltyButton.Enabled;
          ChannelPenaltyDialog.InflowPenaltyNolbl.Enabled := ChannelPenaltyDialog.InflowPenaltyNocmbox.Enabled;
        end;
      if (lPenaltyNr <> NullInteger) then
      begin
        with ChannelPenaltyDialog do
        begin
          for lIndexA := 0 to PenaltyPanels.Count-1 do
          begin
            lPenaltyPanel := TChannelPenaltyPanel(PenaltyPanels.Items[lIndexA]);
            if (lPenaltyPanel.PenaltyNumber = lPenaltyNr) then
              SelectPenaltyPanel(lPenaltyPanel);
          end;
        end;
      end;

    finally
      FreeAndNil(lArcCounts);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TChannelPenaltyValidator.RePopulatePenaltyStructureGrid (APenaltyPanel : TChannelPenaltyPanel);
const OPNAME = 'TChannelPenaltyValidator.RePopulatePenaltyStructureGrid';
var
  lPenaltyList    : IChannelPenaltyList;
  lChannelPenalty : IChannelPenalty;
  lIndexA         : integer;
  lFieldProperty : TAbstractFieldProperty;
  lKeyValues     : string;
  lFieldIndex    : string;
begin
  try
    lPenaltyList := TYieldModelDataObject(FAppModules.Model.ModelData).
                      NetworkElementData.ChannelPenaltyList;
    lChannelPenalty := lPenaltyList.ChannelPenaltyByIdentifier[APenaltyPanel.PenaltyNumber];
    if (lChannelPenalty <> nil) then
    begin
      lFieldProperty := APenaltyPanel.ArcPenaltiesGrid.FieldProperty(1);
      for lIndexA := 1 to APenaltyPanel.ArcPenaltiesGrid.ColCount do
      begin
        if (lIndexA <= lChannelPenalty.ChannelPenaltyArcCount) then
        begin
          lFieldIndex := IntToStr(lIndexA);
          lKeyValues  := lChannelPenalty.GetKeyValues(lFieldProperty.FieldName, lFieldIndex);
          APenaltyPanel.ArcPenaltiesGrid.HasChanges[lIndexA-1, 0] :=
            FAppModules.Changes.HasParamChange(lFieldProperty.FieldName, lKeyValues, lFieldIndex);
          APenaltyPanel.ArcPenaltiesGrid.HasMetaData[lIndexA-1, 0] :=
            FAppModules.MetaData.FindMetaData(lFieldProperty.FieldName, lKeyValues, lFieldIndex) <> nil;

          APenaltyPanel.ArcPenaltiesGrid.Cells[lIndexA-1, 0] := Format(lFieldProperty.FormatStringGrid {'%6.2f'},
            [lChannelPenalty.ChannelPenaltyValueByIndex[lIndexA]]);
        end
        else
          APenaltyPanel.ArcPenaltiesGrid.Cells[lIndexA-1, 0] := '';
        if ((FViewMode = vmEditable) OR
            ((FViewMode = vmEditableSelect) AND
             (lIndexA <= lChannelPenalty.ChannelPenaltyArcCount))) then
          APenaltyPanel.ArcPenaltiesGrid.IsColumnEnabled[lIndexA-1] := TRUE
        else
          APenaltyPanel.ArcPenaltiesGrid.IsColumnEnabled[lIndexA-1] := FALSE;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TChannelPenaltyValidator.SelectPenaltyPanel(APanel : TChannelPenaltyPanel);
const OPNAME = 'TChannelPenaltyValidator.SelectPenaltyPanel';
begin
  try
    if (ChannelPenaltyDialog.ActivePanel <> APanel) then
    begin
      begin
        ChannelPenaltyDialog.ActivePanel := APanel;
        RepopulateChannelListBox;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TChannelPenaltyValidator.OnEditControlEnter(ASender: TObject);
const OPNAME = 'TChannelPenaltyValidator.OnEditControlEnter';
begin
  inherited OnEditControlEnter(ASender);
  try
     SelectNewPanel(ASender);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TChannelPenaltyValidator.OnStringGridColEnter(Sender: TObject;ACol, ARow: integer);
const OPNAME = 'TChannelPenaltyValidator.OnStringGridColEnter';
begin
  inherited OnStringGridColEnter(Sender, ACol, ARow);
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TChannelPenaltyValidator.ResetChannelsLabel(APenaltyNr : integer);
const OPNAME = 'TChannelPenaltyValidator.ResetChannelsLabel';
begin
  try
    with ChannelPenaltyDialog do
    begin
      if (APenaltyNr <> NullInteger) then
        HeadingChannelsLabel.Caption  := FAppModules.Language.GetString('Channel.ChannelsUsingPenaltyStructureM') +
                                         ' ' + IntToStr(APenaltyNr)
      else
        HeadingChannelsLabel.Caption  := ' ';
      Resize;  
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TChannelPenaltyValidator.RepopulateChannelListBox;
const OPNAME = 'TChannelPenaltyValidator.RepopulateChannelListBox';
var
  lPenaltyNr   : integer;
  lChannelList : IChannelList;
  lIndex       : integer;
  lChannel     : IGeneralFlowChannel;
begin
  try
    with ChannelPenaltyDialog do
    begin
      ChannelsListBox.Items.Clear;
      if (ActivePanel <> nil) then
        lPenaltyNr := ActivePanel.PenaltyNumber
      else
        lPenaltyNr := NullInteger;
      ResetChannelsLabel(lPenaltyNr);
      if (lPenaltyNr <> NullInteger) then
      begin
        lChannelList := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.ChannelList;
        for lIndex := 0 to lChannelList.ChannelCount - 1 do
        begin
          lChannel := lChannelList.ChannelByIndex[lIndex];
          if ((lChannel.ChannelPenalty <> nil) AND
              (lPenaltyNr = lChannel.ChannelPenalty.ChannelPenaltyID)) then
            ChannelsListBox.Items.AddObject('(' + IntToStr(lChannel.ChannelNumber) + ') ' +
                                      lChannel.ChannelName,TObject(lChannel.ChannelNumber));
        end;
        DeletePenaltyButton.Enabled := (FViewMode in [vmEditable,vmEditableSelect]) AND
                                       (ChannelsListBox.Items.Count = 0);
      end
      else
        DeletePenaltyButton.Enabled := FALSE;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TChannelPenaltyValidator.ChannelPenaltyDialog : TChannelPenaltyDialog;
const OPNAME = 'TChannelPenaltyValidator.ChannelPenaltyDialog';
begin
  Result := Nil;
  try
    if (FPanel <> nil) then
      Result := TChannelPenaltyDialog(FPanel);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TChannelPenaltyValidator.StudyDataHasChanged(AContext: TChangeContext; AFieldName,AOldValue,ANewValue: string): boolean;
const OPNAME = 'TChannelPenaltyValidator.StudyDataHasChanged';
begin
  Result := inherited StudyDataHasChanged(AContext,AFieldName,AOldValue,ANewValue);
  try
    if FBusyUpdating then Exit;
    if (UpperCase(AFieldName) = 'CHANGELISTAPPLY') OR
       ((UpperCase(AFieldName) = 'CHANGELIST') AND (AContext = sdccDelete)) OR
       (UpperCase(AFieldName) = 'CHANGELISTCOPY')  OR
       (UpperCase(AFieldName) = 'ELEMENTACTIVE') OR
       (UpperCase(AFieldName) = 'ELEMENTORDER') then
      PopulateDataViewer
    else
    if (AContext = sdccEdit) then
    begin
      if(AFieldName = 'ChannelPenaltyName') or
         (AFieldName ='Penalty') then
        RePopulateDataViewer
      else if (AFieldName ='PenaltyNumber') then
      begin
        PopulateDataViewer;
        LanguageHasChanged;
      end;
    end
    else
    begin
      if (AFieldName = 'ChannelPenalty') then
      begin
        PopulateDataViewer;
        LanguageHasChanged;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TChannelPenaltyValidator.StudyHasChanged: boolean;
const OPNAME = 'TChannelPenaltyValidator.StudyHasChanged';
begin
  Result := inherited StudyHasChanged;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TChannelPenaltyValidator.SaveState: boolean;
const OPNAME = 'TChannelPenaltyValidator.SaveState';
begin
  Result := inherited SaveState;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TChannelPenaltyValidator.OnEditControltExit(ASender: TObject);
const OPNAME = 'TChannelPenaltyValidator.OnEditControltExit';
var
  lPenaltyPanel : TChannelPenaltyPanel;
  lPanelList    : TList;
  lIndexA       : integer;
  lFound        : Boolean;
begin
  inherited OnEditControltExit(ASender);
  try
    with ChannelPenaltyDialog do
    begin
      lPanelList := ChannelPenaltyDialog.PenaltyPanels;
      lFound  := FALSE;
      lIndexA := 0;
      while ((NOT lFound) AND (lIndexA < lPanelList.Count)) do
      begin
        lPenaltyPanel := lPanelList.Items[lIndexA];
        if (ASender = lPenaltyPanel.DescriptionEdit) then
        begin
          lFound := TRUE;
          if (lPenaltyPanel.DescriptionEdit.HasValueChanged) then
            UpdatePenaltyName(lPenaltyPanel, Trim(lPenaltyPanel.DescriptionEdit.Text));

        end
        else
          lIndexA := lIndexA + 1;
      end;

      if (ChannelPenaltyDialog.InflowPenaltyNocmbox.HasValueChanged) then
            UpdateInflowPenaltyNocmbox(integer(ChannelPenaltyDialog.InflowPenaltyNocmbox.Items.Objects[ChannelPenaltyDialog.InflowPenaltyNocmbox.ItemIndex]));
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TChannelPenaltyValidator.OnStringGridCellDataHasChanged(
  ASender: TObject; ACol, ARow: integer);
const OPNAME = 'TChannelPenaltyValidator.OnStringGridCellDataHasChanged';
var
  lPenaltyPanel : TChannelPenaltyPanel;
  lPanelList    : TList;
  lIndexA       : integer;
  lFound        : Boolean;
begin
  inherited OnStringGridCellDataHasChanged(ASender, ACol, ARow);
  try
    with ChannelPenaltyDialog do
    begin
      lPanelList := ChannelPenaltyDialog.PenaltyPanels;
      lFound  := FALSE;
      lIndexA := 0;
      while ((NOT lFound) AND (lIndexA < lPanelList.Count)) do
      begin
        lPenaltyPanel := lPanelList.Items[lIndexA];
        if (ASender = lPenaltyPanel.ArcPenaltiesGrid) AND
           (NOT lPenaltyPanel.ArcPenaltiesGrid.HasChanges[lPenaltyPanel.ArcPenaltiesGrid.Col,0]) then
        begin
          lFound := TRUE;
          UpdatePenaltyValue(lPenaltyPanel, ACol+1, Trim(lPenaltyPanel.ArcPenaltiesGrid.Cells[ACol, ARow]));
        end
        else
          lIndexA := lIndexA + 1;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TChannelPenaltyValidator.UpdatePenaltyName (APanel : TChannelPenaltyPanel;
                                                      AValue : string);
const OPNAME = 'TChannelPenaltyValidator.UpdatePenaltyName';
var
  lChannelPenalty : IChannelPenalty;
  lChannelList    : IChannelList;
  lPenaltyList    : IChannelPenaltyList;
  lMessage        : string;
begin
  try
    FBusyUpdating := True;
    try
      with ChannelPenaltyDialog do
      begin
        lPenaltyList := TYieldModelDataObject(FAppModules.Model.ModelData).
                          NetworkElementData.ChannelPenaltyList;
        lChannelPenalty := lPenaltyList.ChannelPenaltyByIdentifier[APanel.PenaltyNumber];
        lChannelList := TYieldModelDataObject(FAppModules.Model.ModelData).
                          NetworkElementData.ChannelList;
        if ((lChannelPenalty <> nil) AND ((FViewMode = vmEditable) or (FViewMode = vmEditableSelect))) then
        begin
          if (FAppModules.FieldProperties.ValidateFieldProperty(
              'ChannelPenaltyName', AValue, lMessage)) then
          begin
            APanel.DescriptionEdit.FieldValidationError := lMessage;
            lChannelPenalty.ChannelPenaltyName := AValue;
            APanel.DescriptionEdit.SetFieldValue(lChannelPenalty.ChannelPenaltyName);
          end
          else
            APanel.DescriptionEdit.FieldValidationError := lMessage;
        end;
      end;
    finally
      FBusyUpdating := False;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TChannelPenaltyValidator.UpdateInflowPenaltyNocmbox(AValue : integer);
const OPNAME = 'TChannelPenaltyValidator.UpdatePenaltyName';
var
  lPenaltyList    : IChannelPenaltyList;
  lMessage        : string;
begin
  try
    FBusyUpdating := True;
    try
      with ChannelPenaltyDialog do
      begin
        lPenaltyList := TYieldModelDataObject(FAppModules.Model.ModelData).
                          NetworkElementData.ChannelPenaltyList;
        if (FAppModules.FieldProperties.ValidateFieldProperty(
            'InflowPenaltyNo', IntToStr(AValue), lMessage)) then
        begin
          lPenaltyList.InflowPenaltyNo := AValue;
          RePopulateDataViewer;
        end
      end;
    finally
      FBusyUpdating := False;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TChannelPenaltyValidator.UpdatePenaltyValue(APanel : TChannelPenaltyPanel;
                                                      AIndex : integer;
                                                      AValue : string);
const OPNAME = 'TChannelPenaltyValidator.UpdatePenaltyValue';
var
  lChannelPenalty : IChannelPenalty;
  lChannelList    : IChannelList;
  lPenaltyList    : IChannelPenaltyList;
  lNewValue       : double;
  lMessage        : string;
begin
  try
    FBusyUpdating := True;
    try
      with ChannelPenaltyDialog do
      begin
        lPenaltyList := TYieldModelDataObject(FAppModules.Model.ModelData).
                          NetworkElementData.ChannelPenaltyList;
        lChannelPenalty := lPenaltyList.ChannelPenaltyByIdentifier[APanel.PenaltyNumber];
        lChannelList := TYieldModelDataObject(FAppModules.Model.ModelData).
                          NetworkElementData.ChannelList;
        if ((lChannelPenalty <> nil) AND ((FViewMode = vmEditable) or (FViewMode = vmEditableSelect))) then
        begin
          if (Trim(AValue) = '') then
          begin
            if ((FViewMode = vmEditable) OR
                ((FViewMode = vmEditableSelect) AND
                 (lChannelList.MayChangePenaltyArcCount(lChannelPenalty.ChannelPenaltyID, lChannelPenalty.ChannelPenaltyArcCount-1)))) then
              AValue := FloatToStr(NullFloat)
            else
              AValue := '0.0';
          end;
          if (FAppModules.FieldProperties.ValidateFieldProperty(
              'Penalty', AValue, lMessage, AIndex)) then
          begin
            APanel.ArcPenaltiesGrid.ValidationError[AIndex-1, 0, gveCellField] := '';
            lNewValue := StrToFloat(Trim(AValue));
            if (lChannelPenalty.ChannelPenaltyValueByIndex[AIndex] <> lNewValue) then
            begin
              lChannelPenalty.ChannelPenaltyValueByIndex[AIndex] := lNewValue;
              RepopulatePenaltyStructureGrid(APanel);
              DoContextValidation(dvtChanPenalty);
            end;
          end
          else
            APanel.ArcPenaltiesGrid.ValidationError[AIndex-1, 0, gveCellField] := lMessage;

        end;
      end;
    finally
      FBusyUpdating := False;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TChannelPenaltyValidator.OnNewPenaltyClick (Sender: TObject);
const OPNAME = 'TChannelPenaltyValidator.OnNewPenaltyClick';
var
  lPanel       : TChannelPenaltyPanel;
  lPenalty     : IChannelPenalty;
  lChannel     : IGeneralFlowChannel;
  lArcCounts   : TStringList;
  LIndex,
  LMinArcCount : integer;
begin
  try
    lPenalty := TYieldModelDataObject(FAppModules.Model.ModelData).
                  NetworkElementData.ChannelPenaltyList.CreateChannelPenalty;
    if (lPenalty <> nil) then
    begin
      lChannel     := TYieldModelDataObject(FAppModules.Model.ModelData).
                        NetworkElementData.ChannelList.ChannelByChannelNumber[FChannelNumber];
      if(lChannel <> nil) then
      begin
        lArcCounts := TStringList.Create;
        try
          lArcCounts.CommaText :=  lChannel.ValidArcCounts;
          if(lArcCounts.Count > 0) then
          begin
            LMinArcCount := 5;
            for LIndex := 0 to lArcCounts.Count -1 do
            begin
              if(StrToInt(lArcCounts[LIndex]) < LMinArcCount) then
                LMinArcCount := StrToInt(lArcCounts[LIndex]);
            end;
            for LIndex := 1 to LMinArcCount do
              lPenalty.ChannelPenaltyValueByIndex[LIndex] := 0.0;
          end;
        finally
          FreeAndNil(lArcCounts);
        end;
      end;
      PopulateDataViewer;
      with ChannelPenaltyDialog do
      begin
        lPanel := TChannelPenaltyPanel(PenaltyPanels.Items[PenaltyPanels.Count - 1]);
        SelectPenaltyPanel(lPanel);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TChannelPenaltyValidator.OnDeletePenaltyClick (Sender: TObject);
const OPNAME = 'TChannelPenaltyValidator.OnDeletePenaltyClick';
var
  lPenaltyList    : IChannelPenaltyList;
begin
  try
    if (ChannelPenaltyDialog.ActivePanel <> nil) then
    begin
      lPenaltyList := TYieldModelDataObject(FAppModules.Model.ModelData).
                        NetworkElementData.ChannelPenaltyList;
      if (TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.
            ChannelPenaltyList.RemoveChannelPenaltyWithID(ChannelPenaltyDialog.ActivePanel.PenaltyNumber)) then
        PopulateDataViewer;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TChannelPenaltyValidator.DoContextValidation(AValidationType : TDialogValidationType);
const OPNAME = 'TChannelPenaltyValidator.DoContextValidation';
begin
  try
    if (AValidationType in [dvtChanPenaltyAll]) then
      ValidateAllPenalties;
    if (AValidationType in [dvtChanPenalty]) then
      ValidatePenalty;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TChannelPenaltyValidator.ValidateAllPenalties;
const OPNAME = 'TChannelPenaltyValidator.ValidateAllPenalties';
var
  lIndex          : integer;
  lPanel          : TChannelPenaltyPanel;
  lCount          : integer;
  lChannelPenalty : IChannelPenalty;
  lPenaltyList    : IChannelPenaltyList;
begin
  try
    with ChannelPenaltyDialog do
    begin
      lPenaltyList := TYieldModelDataObject(FAppModules.Model.ModelData).
                        NetworkElementData.ChannelPenaltyList;
      for lIndex := 0 to PenaltyPanels.Count - 1 do
      begin
        lPanel := TChannelPenaltyPanel(PenaltyPanels.Items[lIndex]);
        FErrorMessage := '';
        lChannelPenalty := lPenaltyList.ChannelPenaltyByIdentifier[lPanel.PenaltyNumber];
        if (lChannelPenalty.Validate(FErrorMessage, 'ForChannels')) then
        begin
          for lCount := 0 to 4 do
            lPanel.ArcPenaltiesGrid.ValidationError[lCount, 0, gveColContext] := '';
        end
        else
        begin
          for lCount := 0 to 4 do
            lPanel.ArcPenaltiesGrid.ValidationError[lCount, 0, gveColContext] := FErrorMessage;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TChannelPenaltyValidator.ValidatePenalty;
const OPNAME = 'TChannelPenaltyValidator.ValidatePenalty';
var
  lPenaltyList : IChannelPenaltyList;
  lPenalty     : IChannelPenalty;
  lIndex       : integer;
begin
  try
    with ChannelPenaltyDialog do
    begin
      if (ActivePanel <> nil) then
      begin
        lPenaltyList := TYieldModelDataObject(FAppModules.Model.ModelData).
                          NetworkElementData.ChannelPenaltyList;
        lPenalty     := lPenaltyList.ChannelPenaltyByIdentifier[ActivePanel.PenaltyNumber];
        FErrorMessage := '';
        if (lPenalty.Validate(FErrorMessage, 'ForChannels')) then
        begin
          for lIndex := 0 to 4 do
            ActivePanel.ArcPenaltiesGrid.ValidationError[lIndex, 0, gveColContext] := '';
        end
        else
        begin
          for lIndex := 0 to 4 do
            ActivePanel.ArcPenaltiesGrid.ValidationError[lIndex, 0, gveColContext] := FErrorMessage;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TChannelPenaltyValidator.SetViewMode(AViewMode: TViewMode);
const OPNAME = 'TChannelPenaltyValidator.SetViewMode';
begin
  inherited;
  try
    with ChannelPenaltyDialog do
    begin
      NewPenaltyButton.IsEnabled    := (AViewMode in [vmEditable,vmEditableSelect]);
      DeletePenaltyButton.IsEnabled := (AViewMode in [vmEditable,vmEditableSelect]);
      HighlightSelectedPanel        := (AViewMode in [vmSelect,vmEditableSelect]);
      if(AViewMode in [vmEditable,vmSelect,vmEditableSelect]) then
        ChannelsListBox.DragMode := dmAutomatic
      else
        ChannelsListBox.DragMode := dmManual;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TChannelPenaltyValidator.SelectNewPanel(Sender: TObject);
const OPNAME = 'TChannelPenaltyValidator.SelectNewPanel';
begin
  try
    if Sender.ClassNameIs('TChannelPenaltyPanel') then
      SelectPenaltyPanel(TChannelPenaltyPanel(Sender))
    else
    if Sender.ClassNameIs('TFieldEdit') and
      (TFieldEdit(Sender).Parent.ClassNameIs('TChannelPenaltyPanel')) then
      SelectPenaltyPanel(TChannelPenaltyPanel(TFieldEdit(Sender).Parent))
    else
    if Sender.ClassNameIs('TFieldStringGrid') and
      (TFieldStringGrid(Sender).Parent.ClassNameIs('TChannelPenaltyPanel')) then
      SelectPenaltyPanel(TChannelPenaltyPanel(TFieldStringGrid(Sender).Parent));

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TChannelPenaltyValidator.ProcessParameterChangeEvent : boolean;
const OPNAME = 'TChannelPenaltyValidator.ProcessParameterChangeEvent';
var
  lFieldProperty : TAbstractFieldProperty;
  lKeyValues     : string;
  lFieldIndex    : string;
  lPenalty       : IChannelPenalty;
  lArc           : integer;
begin
  Result := FALSE;
  try
    if (FPanel.Visible AND (FActiveControl <> nil) AND (ChannelPenaltyDialog.ActivePanel <> nil)) then
    begin
      lPenalty := TYieldModelDataObject(FAppModules.Model.ModelData).
                    NetworkElementData.ChannelPenaltyList.
                      ChannelPenaltyByIdentifier[ChannelPenaltyDialog.ActivePanel.PenaltyNumber];
      if (lPenalty <> nil) then
      begin
        with ChannelPenaltyDialog.ActivePanel do
        begin
          if (FActiveControl = ArcPenaltiesGrid) then
          begin
            lArc := ArcPenaltiesGrid.Col + 1;
            if (lArc <= lPenalty.ChannelPenaltyArcCount) then
            begin
              lFieldIndex    := IntToStr(lArc);
              lFieldProperty := ArcPenaltiesGrid.FieldProperty(1);
              if (lFieldProperty <> nil) then
              begin
                lKeyValues := lPenalty.GetKeyValues(lFieldProperty.FieldName, lFieldIndex);
                FAppModules.Changes.ShowParameterChanges
                  (lFieldProperty.FieldName, lKeyValues, lFieldIndex);
                Result := TRUE;
                RePopulateDataViewer;
                FAppModules.Changes.SetParameterChanges(TRUE);
                Result := TRUE;
              end;
            end;  
          end;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TChannelPenaltyValidator.ProcessMetaDataEvent : boolean;
const OPNAME = 'TChannelPenaltyValidator.ProcessMetaDataEvent';
var
  lFieldProperty : TAbstractFieldProperty;
  lKeyValues     : string;
  lFieldIndex    : string;
  lPenalty       : IChannelPenalty;
  lArc           : integer;
begin
  Result := FALSE;
  try
    if (FPanel.Visible AND (FActiveControl <> nil) AND (ChannelPenaltyDialog.ActivePanel <> nil)) then
    begin
      lPenalty := TYieldModelDataObject(FAppModules.Model.ModelData).
                    NetworkElementData.ChannelPenaltyList.
                      ChannelPenaltyByIdentifier[ChannelPenaltyDialog.ActivePanel.PenaltyNumber];
      if (lPenalty <> nil) then
      begin
        with ChannelPenaltyDialog.ActivePanel do
        begin
          lFieldIndex    := '';
          lFieldProperty := nil;
          if (FActiveControl = ArcPenaltiesGrid) then
          begin
            lArc := ArcPenaltiesGrid.Col + 1;
            if (lArc <= lPenalty.ChannelPenaltyArcCount) then
            begin
              lFieldIndex    := IntToStr(lArc);
              lFieldProperty := ArcPenaltiesGrid.FieldProperty(1);
            end;
          end
          else
          if (FActiveControl = DescriptionEdit) then
            lFieldProperty := DescriptionEdit.FieldProperty;
          if (lFieldProperty <> nil) then
          begin
            lKeyValues := lPenalty.GetKeyValues(lFieldProperty.FieldName, lFieldIndex);
            FAppModules.MetaData.ShowMetaData
              (lFieldProperty.FieldName, lKeyValues, lFieldIndex);
            RePopulateDataViewer;
            Result := TRUE;
          end;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TChannelPenaltyValidator.OnChannelPenaltyDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
const OPNAME = 'TChannelPenaltyValidator.OnChannelPenaltyDragOver';
var
  LPenaltyNumber : integer;
  LChannelNumber : integer;
  lPenalty       : IChannelPenalty;
  lChannel       : IGeneralFlowChannel;
  LPanel         : TChannelPenaltyPanel;
  lArcCounts     : TStringList;
  lArcs          : string;
begin
  try
    Accept := False;
    if(Source = ChannelPenaltyDialog.ChannelsListBox) then
    begin
      LPanel := nil;
      if (Sender.ClassName = 'TChannelPenaltyPanel') then
       LPanel := TChannelPenaltyPanel(Sender)
      else
      if (TControl(Sender).Parent.ClassName = 'TChannelPenaltyPanel') then
       LPanel := TChannelPenaltyPanel(TControl(Sender).Parent);

      if(LPanel <> nil) then
      begin
        LPenaltyNumber  := LPanel.PenaltyNumber;
        lPenalty        := TYieldModelDataObject(FAppModules.Model.ModelData).
                            NetworkElementData.ChannelPenaltyList.ChannelPenaltyByIdentifier[LPenaltyNumber];
        if (lPenalty <> nil) then
        begin
          LChannelNumber  := Integer(ChannelPenaltyDialog.ChannelsListBox.Items.Objects[ChannelPenaltyDialog.ChannelsListBox.ItemIndex]);
          lChannel        := TYieldModelDataObject(FAppModules.Model.ModelData).
                             NetworkElementData.ChannelList.ChannelByChannelNumber[LChannelNumber];
          if(lChannel <> nil) then
          begin
            lArcCounts := TStringList.Create;
            try
              lArcCounts.CommaText := lChannel.ValidArcCounts;
              lArcs                := IntToStr(lPenalty.ChannelPenaltyArcCount);
              Accept               := (lArcCounts.IndexOf(lArcs) >= 0);
            finally
              lArcCounts.Free;
            end;
          end;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TChannelPenaltyValidator.OnChannelPenaltyDragDrop(Sender, Source: TObject; X, Y: Integer);
const OPNAME = 'TChannelPenaltyValidator.OnChannelPenaltyDragDrop';
var
  lIndexA        : integer;
  LPenaltyNumber : integer;
  LChannelNumber : integer;
  lPenaltyPanel  : TChannelPenaltyPanel;
  lPenalty     : IChannelPenalty;
  lChannel     : IGeneralFlowChannel;
begin
  try
    if(Source = ChannelPenaltyDialog.ChannelsListBox) then
    begin
      if Sender.ClassNameIs('TChannelPenaltyPanel') then
        LPenaltyNumber  := TChannelPenaltyPanel(Sender).PenaltyNumber
      else
        LPenaltyNumber  :=TChannelPenaltyPanel(TControl(Sender).Parent).PenaltyNumber;
      LChannelNumber  := Integer(ChannelPenaltyDialog.ChannelsListBox.Items.Objects[ChannelPenaltyDialog.ChannelsListBox.ItemIndex]);

      lPenalty := TYieldModelDataObject(FAppModules.Model.ModelData).
                    NetworkElementData.ChannelPenaltyList.ChannelPenaltyByIdentifier[LPenaltyNumber];
      if (lPenalty <> nil) then
      begin
        lChannel     := TYieldModelDataObject(FAppModules.Model.ModelData).
                          NetworkElementData.ChannelList.ChannelByChannelNumber[LChannelNumber];
        if(lChannel <> nil) then
        begin
          lChannel.ChannelPenalty := lPenalty;
          RePopulateDataViewer;
          with ChannelPenaltyDialog do
          begin
            for lIndexA := 0 to PenaltyPanels.Count-1 do
            begin
              lPenaltyPanel := TChannelPenaltyPanel(PenaltyPanels.Items[lIndexA]);
              if (lPenaltyPanel.PenaltyNumber = LPenaltyNumber) then
              begin
                SelectPenaltyPanel(lPenaltyPanel);
                Break;
              end;
            end;
          end;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TChannelPenaltyValidator.OnChannelDragOver(Sender,Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
const OPNAME = 'TChannelPenaltyValidator.OnChannelDragOver ';
var
  LChannelNumber : integer;
begin
  Accept := False;
  try
    if(FViewMode = vmNone) then
    begin
      ChannelPenaltyDialog.ChannelsListBox.EndDrag(False);
      Exit;
    end;

    if(FViewMode <> vmEditable) then
    begin
      if(FChannelNumber >= 0) then
      begin
        LChannelNumber  := Integer(ChannelPenaltyDialog.ChannelsListBox.Items.Objects[ChannelPenaltyDialog.ChannelsListBox.ItemIndex]);
        if(LChannelNumber <> FChannelNumber) then
          ChannelPenaltyDialog.ChannelsListBox.EndDrag(False);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.

