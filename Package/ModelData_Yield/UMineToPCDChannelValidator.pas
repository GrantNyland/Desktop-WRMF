{******************************************************************************}
{*  UNIT      : Contains the class TMineToPCDChannelValidator.                *}
{*  AUTHOR    : Presley Mudau                                                 *}
{*  DATE      : 2007/03/15                                                    *}
{*  COPYRIGHT : Copyright � 2007 DWAF                                         *}
{******************************************************************************}

unit UMineToPCDChannelValidator;

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
  UChannelPenaltyValidator,
  USelectChannelValidator,
  UMineChannelPropertiesDialog,
  UAbstractYieldDataDialogValidator,
  UYieldContextValidationType;

type
  TMineToPCDChannelValidator = class(TAbstractYieldDataDialogValidator)
  protected
    FIdentifier              : integer;
    FChannelPenaltyValidator : TChannelPenaltyValidator;
    FSelectChannelValidator  : TSelectChannelValidator;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    procedure CreateDialog; virtual;
    procedure OnEditControlEnter(Sender: TObject); override;
    procedure OnEditControltExit(Sender: TObject); override;
    procedure OnSelectPenaltyClick(Sender: TObject);
    procedure UpdateChannelName;
    procedure UpdateChannelPenaltyStructure; virtual;
    procedure UpdateChannelArea;
    procedure ValidateChannelName (AChannel : IGeneralFlowChannel);
    procedure ValidateChannelNumber (AChannel : IGeneralFlowChannel);
    procedure ValidateChannelPenalty (AChannel : IGeneralFlowChannel);
    procedure RePopulateDataViewer;
    procedure RepopulateChannelAreas;
    procedure RePopulateNodes;
    procedure ShowChannelPenaltyDialog (ASender : TObject);
  public
    function Initialise: boolean; override;
    function SaveState: boolean; override;
    function LanguageHasChanged: boolean; override;
    function StudyHasChanged: boolean; override;
    function StudyDataHasChanged(AContext: TChangeContext; AFieldName,AOldValue,ANewValue: string): boolean; override;
//    function ProcessMetaDataEvent : boolean; override;
    procedure ClearDataViewer; override;
    procedure PopulateDataViewer; override;
    procedure DoContextValidation (AValidationType : TDialogValidationType); override;
//    function DetermineWizardStatus (ASequence : integer = 0) : integer; override;
    function MineChannelPropertiesDialog : TMineChannelPropertiesDialog;
    property Identifier : integer read FIdentifier write FIdentifier;
  end;

implementation

uses
  SysUtils,
  VCL.Graphics,
  VCL.Dialogs,
  UIrrigationBlock,
  UWetland,
  UYieldModelDataGUIForm,
  UChannelPenaltyDialog,
  UYieldModelDataObject,
  UErrorHandlingOperations,
  UPlanningModelDataObject,
  UGrowthFactorData,
  USelectChannelDialog, UNetworkElementData, UNetworkFeaturesData;

{******************************************************************************}
{* TMineToPCDChannelValidator                                               *}
{******************************************************************************}

procedure TMineToPCDChannelValidator.CreateMemberObjects;
const OPNAME = 'TMineToPCDChannelValidator.CreateMemberObjects';
var
  lpPanel     : TMineChannelPropertiesDialog;
begin
  try
    inherited CreateMemberObjects;
    CreateDialog;
    lpPanel := MineChannelPropertiesDialog;
    with lpPanel do
    begin
      ChannelNumberEdit.FieldProperty     := FAppModules.FieldProperties.FieldProperty('ChannelNumber');
      ChannelNumberEdit.ReadOnly          := TRUE;
      ChannelNumberEdit.OnEnter           := OnEditControlEnter;
      ChannelNumberEdit.OnExit            := OnEditControltExit;

      ChannelNameEdit.FieldProperty       := FAppModules.FieldProperties.FieldProperty('ChannelName');
      ChannelNameEdit.OnEnter             := OnEditControlEnter;
      ChannelNameEdit.OnExit              := OnEditControltExit;

      UpStreamNodeEdit.FieldProperty      := FAppModules.FieldProperties.FieldProperty('UpNodeNumber');
      UpStreamNodeEdit.IsEnabled          := FALSE;
      UpStreamNodeEdit.OnEnter            := OnEditControlEnter;
      UpStreamNodeEdit.OnExit             := OnEditControltExit;

      DownStreamNodeCbx.FieldProperty    := FAppModules.FieldProperties.FieldProperty('DownNodeNumber');
      DownStreamNodeCbx.OnEnter          := OnEditControlEnter;
      DownStreamNodeCbx.IsEnabled        := TRUE;
      DownStreamNodeCbx.OnExit           := OnEditControltExit;

      ChannelAreaCbx.FieldProperty       := FAppModules.FieldProperties.FieldProperty('ChannelAreaName');
      ChannelAreaCbx.OnEnter             := OnEditControlEnter;
      ChannelAreaCbx.OnExit              := OnEditControltExit;

      PenaltyStructureEdit.FieldProperty := FAppModules.FieldProperties.FieldProperty('PenaltyNumber');
      PenaltyStructureEdit.ReadOnly      := TRUE;
      PenaltyStructureEdit.Color         := clWindow;
      PenaltyStructureEdit.OnEnter       := OnEditControlEnter;
      PenaltyStructureEdit.OnExit        := OnEditControltExit;

      SelectPenaltyStructureBtn.FieldProperty := FAppModules.FieldProperties.FieldProperty('PenaltyNumber');
      SelectPenaltyStructureBtn.OnEnter  := OnEditControlEnter;
      SelectPenaltyStructureBtn.OnExit   := OnEditControltExit;
      SelectPenaltyStructureBtn.OnClick  := OnSelectPenaltyClick;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMineToPCDChannelValidator.CreateDialog;
const OPNAME = 'TMineToPCDChannelValidator.CreateDialog';
begin
  try
    FPanel  := TMineChannelPropertiesDialog.Create(FPanelOwner,FAppModules);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMineToPCDChannelValidator.DestroyMemberObjects;
const OPNAME = 'TMineToPCDChannelValidator.DestroyMemberObjects';
begin
  try
    inherited DestroyMemberObjects;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMineToPCDChannelValidator.Initialise: boolean;
const OPNAME = 'TMineToPCDChannelValidator.Initialise';
begin
  Result := inherited Initialise;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMineToPCDChannelValidator.LanguageHasChanged: boolean;
const OPNAME = 'TMineToPCDChannelValidator.LanguageHasChanged';
begin
  Result := False;
  try
    TabShetCaption := FAppModules.Language.GetString('TabCaption.MineToPCDTabSheet');
    Result := inherited LanguageHasChanged;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMineToPCDChannelValidator.ClearDataViewer;
const OPNAME = 'TMineToPCDChannelValidator.ClearDataViewer';
var
  lpPanel     : TMineChannelPropertiesDialog;
begin
  inherited ClearDataViewer;
  try
    lpPanel := MineChannelPropertiesDialog;
    with lpPanel do
    begin
      ChannelNameEdit.Text         := '';
      ChannelNumberEdit.Text       := '-1';
      UpStreamNodeEdit.Text        := '-1';
      DownStreamNodeCbx.ItemIndex  := -1;
      PenaltyStructureEdit.Text    := '-1';
      ChannelAreaCbx.ItemIndex     := -1;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMineToPCDChannelValidator.PopulateDataViewer;
const OPNAME = 'TMineToPCDChannelValidator.PopulateDataViewer';
begin
  inherited PopulateDataViewer;
  try
    ClearDataViewer;
    RePopulateDataViewer;
    DoContextValidation(dvtChanPropAll);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMineToPCDChannelValidator.RePopulateDataViewer;
const OPNAME = 'TMineToPCDChannelValidator.RePopulateDataViewer';
var
  LMine            : IMine;
  LChannelNr       : integer;
  lFieldProperty   : TAbstractFieldProperty;
  lKeyValues       : string;
  lPenalty         : IChannelPenalty;
  lChannel         : IGeneralFlowChannel;
begin
  try
    if (FIdentifier >= 0) then
    begin
      LMine := TYieldModelDataObject(FAppModules.Model.ModelData).CastNetworkFeaturesData
               .CastMineList.MineByID[FIdentifier];
      if (LMine <> nil) then
      begin
        LChannelNr := LMine.PCDChannelNumber;
        lChannel := TYieldModelDataObject(FAppModules.Model.ModelData).
                      NetworkElementData.ChannelList.ChannelByChannelNumber[LChannelNr];
        if (lChannel <> nil) then
        begin
          with MineChannelPropertiesDialog do
          begin
             ChannelNumberEdit.SetFieldValue(lChannel.ChannelNumber);
             ChannelNameEdit.SetFieldValue(lChannel.ChannelName);
             UpStreamNodeEdit.SetFieldValue(lChannel.UpStreamNodeNumber);
             RepopulateChannelAreas;
             lFieldProperty := PenaltyStructureEdit.FieldProperty;
             PenaltyStructureEdit.HasMetaData := FAppModules.MetaData.FindMetaData(lFieldProperty.FieldName, lKeyValues, '') <> nil;
             
             lPenalty := lChannel.ChannelPenalty;
             if (lPenalty <> nil) then
              PenaltyStructureEdit.SetFieldValue(lPenalty.ChannelPenaltyID)
            else
              PenaltyStructureEdit.SetFieldValue(0);
          end;
          RePopulateNodes;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMineToPCDChannelValidator.SaveState: boolean;
const OPNAME = 'TMineToPCDChannelValidator.SaveState';
begin
  Result := inherited SaveState;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMineToPCDChannelValidator.MineChannelPropertiesDialog:TMineChannelPropertiesDialog;
const OPNAME = 'TMineToPCDChannelValidator.MineChannelPropertiesDialog';
begin
  Result := Nil;
  try
    if (FPanel <> nil) then
      Result := TMineChannelPropertiesDialog(FPanel);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMineToPCDChannelValidator.StudyDataHasChanged(AContext: TChangeContext; AFieldName,AOldValue,ANewValue: string): boolean;
const OPNAME = 'TMineToPCDChannelValidator.StudyDataHasChanged';
begin
  Result := inherited StudyDataHasChanged(AContext,AFieldName,AOldValue,ANewValue);
  try

{    if (AFieldName = 'DemandCentreConsumptiveChannelNumber') then
    begin
      if (AContext = sdccEdit) and ((AOldValue = IntToStr(FIdentifier)) or (FIdentifier = -1)) then
      begin
        if (FMode = gfcvmDemandCentreConsumptiveUpstream) then
        begin
          FIdentifier := StrToInt(ANewValue);
          RePopulateDataViewer;
        end;
      end;
    end else
    if (AFieldName = 'DemandCentreReclaimationChannelNumber') then
    begin
      if (AContext = sdccEdit) and ((AOldValue = IntToStr(FIdentifier)) or (FIdentifier = -1)) then
      begin
        if (FMode = gfcvmDemandCentreReclaimationUpstream) then
        begin
          FIdentifier := StrToInt(ANewValue);
          ClearDataViewer;
          RePopulateDataViewer;
        end;
      end;
    end else
    if (AFieldName = 'Penalty') then
      DoContextValidation(dvtChanPropPenalty)
    else if (AFieldName = 'ChannelName') then
      RePopulateDataViewer
    else if (AFieldName = 'PenaltyNumber') then
      RePopulateDataViewer
    else if (AFieldName = 'DownNodeNumber') then
      RePopulateDataViewer
    else if (AFieldName = 'UpNodeNumber') then
      RePopulateDataViewer
    else if (AFieldName = 'SummaryOutput') then
      RePopulateDataViewer;        }

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMineToPCDChannelValidator.StudyHasChanged: boolean;
const OPNAME = 'TMineToPCDChannelValidator.StudyHasChanged';
begin
  Result := inherited StudyHasChanged;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMineToPCDChannelValidator.OnSelectPenaltyClick(Sender: TObject);
const OPNAME = 'TMineToPCDChannelValidator.OnSelectPenaltyClick';
var
  LForm            : TYieldModelDataGUIForm;
  LDialogValidator : TChannelPenaltyValidator;
  LSelectedPenalty : integer;
begin
  try
    LForm   := TYieldModelDataGUIForm.CreateWithoutDFM(nil,FAppModules);
    try
      LForm.Initialise;
      LForm.LanguageHasChanged;

      LDialogValidator := TChannelPenaltyValidator.Create(LForm,FAppModules);
      FChannelPenaltyValidator := LDialogValidator;
      try
        LForm.AddModelDataPanel(LDialogValidator.Panel);
        LDialogValidator.Initialise;
        LDialogValidator.ViewMode := vmSelect;
        LDialogValidator.ChannelNumber := FIdentifier;
        LDialogValidator.PopulateDataViewer;
        LDialogValidator.LanguageHasChanged;
        lForm.OnShow := ShowChannelPenaltyDialog;

        LForm.ShowModal;
        if (LForm.ModalResult = mrOk) and (TChannelPenaltyDialog(LDialogValidator.Panel).ActivePanel <> nil)then
        begin
          LSelectedPenalty := StrToInt(TChannelPenaltyDialog(LDialogValidator.Panel).ActivePanel.IDLabel.Caption);
          if(MineChannelPropertiesDialog.PenaltyStructureEdit.Text <> IntToStr(LSelectedPenalty)) then
          begin
            MineChannelPropertiesDialog.PenaltyStructureEdit.Text := IntToStr(LSelectedPenalty);
            MineChannelPropertiesDialog.PenaltyStructureEdit.OnExit(MineChannelPropertiesDialog.PenaltyStructureEdit);
          end;
        end;
      finally
        FChannelPenaltyValidator := nil;
        LDialogValidator.Free;
      end;
    finally
      LForm.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMineToPCDChannelValidator.ShowChannelPenaltyDialog(ASender: TObject);
const OPNAME = 'TMineToPCDChannelValidator.ShowChannelPenaltyDialog';
begin
  try
    if (FChannelPenaltyValidator <> nil) then
      FChannelPenaltyValidator.ChannelPenaltyDialog.Resize;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMineToPCDChannelValidator.OnEditControlEnter(Sender: TObject);
const OPNAME = 'TMineToPCDChannelValidator.OnEditControlEnter';
begin
  inherited OnEditControlEnter(Sender);
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMineToPCDChannelValidator.OnEditControltExit(Sender: TObject);
const OPNAME = 'TMineToPCDChannelValidator.OnEditControltExit';
begin
  inherited OnEditControltExit(Sender);
  try
    with MineChannelPropertiesDialog do
    begin
      if ((Sender = ChannelNameEdit) AND
          (ChannelNameEdit.HasValueChanged)) then
        UpdateChannelName
      else
      if ((Sender = PenaltyStructureEdit) AND
          (PenaltyStructureEdit.HasValueChanged)) then
        UpdateChannelPenaltyStructure
      else
      if ((Sender = UpStreamNodeEdit) AND
          (UpStreamNodeEdit.HasValueChanged)) then
        //UpdateChannelUpstreamNode
      else
      if ((Sender = DownStreamNodeCbx) AND
          (DownStreamNodeCbx.HasValueChanged)) then
       // UpdateChannelDownstreamNode
      else
      if ((Sender = ChannelAreaCbx) AND (ChannelAreaCbx.HasValueChanged)) then
        UpdateChannelArea;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMineToPCDChannelValidator.UpdateChannelName;
const OPNAME = 'TMineToPCDChannelValidator.UpdateChannelName';
var
  LChannel   : IGeneralFlowChannel;
  LMessage   : string;
  LMine      : IMine;
  LChannelNr : integer;
begin
  try
    LMine := TYieldModelDataObject(FAppModules.Model.ModelData).
             CastNetworkFeaturesData.CastMineList.MineByID[FIdentifier];
    if LMine <> nil then
    begin
      LChannelNr := LMine.PCDChannelNumber;
      LChannel := TYieldModelDataObject(FAppModules.Model.ModelData).
                    NetworkElementData.ChannelList.ChannelByChannelNumber[LChannelNr];
      if (LChannel <> nil) then
      begin
        with MineChannelPropertiesDialog do
        begin
          if (FAppModules.FieldProperties.ValidateFieldProperty(
              ChannelNameEdit.FieldProperty.FieldName,
              ChannelNameEdit.Text,LMessage)) then
          begin
            ChannelNameEdit.FieldValidationError := LMessage;
            lChannel.ChannelName := Trim(ChannelNameEdit.Text);
            ChannelNameEdit.SetFieldValue(lChannel.ChannelName);
            DoContextValidation(dvtChanPropName);
          end
          else
            ChannelNameEdit.FieldValidationError := LMessage;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


procedure TMineToPCDChannelValidator.UpdateChannelPenaltyStructure;
const OPNAME = 'TMineToPCDChannelValidator.UpdateChannelPenaltyStructure';
var
  lPenaltyTypeList  : IChannelPenaltyList;
  lPenaltyStructure : IChannelPenalty;
  lPenaltyID        : integer;
  lChannel          : IGeneralFlowChannel;
  lMessage          : string;
  LMine             : IMine;
  LChannelNr        : integer;
begin
  try
    LMine := TYieldModelDataObject(FAppModules.Model.ModelData).
             CastNetworkFeaturesData.CastMineList.MineByID[FIdentifier];
    if LMine <> nil then
    begin
      LChannelNr := LMine.PCDChannelNumber;
      LChannel := TYieldModelDataObject(FAppModules.Model.ModelData).
                    NetworkElementData.ChannelList.ChannelByChannelNumber[LChannelNr];
      if (lChannel <> nil) then
      begin
        with MineChannelPropertiesDialog do
        begin
          if (FAppModules.FieldProperties.ValidateFieldProperty(
              PenaltyStructureEdit.FieldProperty.FieldName,
              PenaltyStructureEdit.Text,LMessage)) then
          begin
            PenaltyStructureEdit.FieldValidationError := LMessage;
            LPenaltyID       := StrToInt(Trim(PenaltyStructureEdit.Text));
            LPenaltyTypeList := TYieldModelDataObject(FAppModules.Model.ModelData).
                                  NetworkElementData.ChannelPenaltyList;
            if (LPenaltyTypeList <> nil) then
            begin
              LPenaltyStructure := lPenaltyTypeList.ChannelPenaltyByIdentifier[LPenaltyID];
              LChannel.ChannelPenalty := lPenaltyStructure;
              PenaltyStructureEdit.SetFieldValue(IntToStr(lChannel.ChannelPenalty.ChannelPenaltyID));
              DoContextValidation(dvtChanPropPenalty);
            end;
          end
          else
            PenaltyStructureEdit.FieldValidationError := lMessage;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMineToPCDChannelValidator.DoContextValidation(AValidationType : TDialogValidationType);
const OPNAME = 'TMineToPCDChannelValidator.DoContextValidation';
var
  lChannel     : IGeneralFlowChannel;
  lChannelList : IChannelList;
begin
  try
    FAllErrorMessages.Clear;
    if (FIdentifier >= 0) then
    begin
      lChannelList := TYieldModelDataObject(FAppModules.Model.ModelData).
                        NetworkElementData.ChannelList;
      lChannel     := lChannelList.ChannelByChannelNumber[FIdentifier];
      if (lChannel <> nil) then
      begin
        if (AValidationType in [dvtChanPropAll, dvtChanPropWizardStep2, dvtChanPropName]) then
          ValidateChannelName(lChannel);
        if (AValidationType in [dvtChanPropAll, dvtChanPropWizardStep2, dvtChanPropPenalty]) then
          ValidateChannelPenalty(lChannel);
        if (AValidationType in [dvtChanPropAll, dvtChanPropWizardStep2, dvtChanPropNumber]) then
          ValidateChannelNumber(lChannel);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{function TMineToPCDChannelValidator.DetermineWizardStatus (ASequence : integer = 0) : integer;
const OPNAME = 'TMineToPCDChannelValidator.DetermineWizardStatus';
var
  lChannel     : IGeneralFlowChannel;
  lChannelList : IChannelList;
begin
  Result := inherited DetermineWizardStatus(ASequence);
  try
    if (FIdentifier >= 0) then
    begin
      lChannelList := TYieldModelDataObject(FAppModules.Model.ModelData).
                        NetworkElementData.ChannelList;
      lChannel     := lChannelList.ChannelByChannelNumber[FIdentifier];
      if (lChannel <> nil) then
      begin
        case ASequence of
          1 :
            begin
              DoContextValidation(dvtChanPropWizardStep1);
              if ((lChannel.UpStreamNodeNumber <> 0) OR (lChannel.DownStreamNodeNumber <> 0)) then
              begin
                Result := 1;
                if (FAllErrorMessages.Count = 0) then
                  Result := 2;
              end;
            end;
          2 :
            begin
              DoContextValidation(dvtChanPropWizardStep2);
              if (lChannel.ChannelPenaltyNumber <> 0) then
              begin
                Result := 1;
                if (FAllErrorMessages.Count = 0) then
                  Result := 2;
              end;
            end;
        else
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end; }

procedure TMineToPCDChannelValidator.ValidateChannelNumber(AChannel: IGeneralFlowChannel);
const OPNAME = 'TMineToPCDChannelValidator.ValidateChannelNumber';
begin
  try
    with MineChannelPropertiesDialog do
    begin
      FErrorMessage := '';
      if (NOT AChannel.Validate(FErrorMessage, 'ChannelNumber')) then
        FAllErrorMessages.Add(Trim(FErrorMessage));
      ChannelNumberEdit.ContextValidationError := FErrorMessage;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMineToPCDChannelValidator.ValidateChannelName (AChannel : IGeneralFlowChannel);
const OPNAME = 'TMineToPCDChannelValidator.ValidateChannelName';
begin
  try
    with MineChannelPropertiesDialog do
    begin
      FErrorMessage := '';
      if (NOT AChannel.Validate(FErrorMessage, 'ChannelName')) then
        FAllErrorMessages.Add(Trim(FErrorMessage));
      ChannelNameEdit.ContextValidationError := FErrorMessage;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMineToPCDChannelValidator.ValidateChannelPenalty (AChannel : IGeneralFlowChannel);
const OPNAME = 'TMineToPCDChannelValidator.ValidateChannelPenalty';
begin
  try
    with MineChannelPropertiesDialog do
    begin
      FErrorMessage := '';
      if (NOT AChannel.Validate(FErrorMessage, 'ChannelPenalty')) then
        FAllErrorMessages.Add(Trim(FErrorMessage));
      PenaltyStructureEdit.ContextValidationError := FErrorMessage;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{procedure TMineToPCDChannelValidator.ValidateUpAndDownstreamNodes
                                        (AChannelList : IChannelList;
                                         AChannel     : IGeneralFlowChannel);
const OPNAME = 'TMineToPCDChannelValidator.ValidateUpAndDownstreamNodes';
begin
  try
    with MineChannelPropertiesDialog do
    begin
      FErrorMessage := '';
      if (AChannel.Validate(FErrorMessage, 'UpDownNotSameNode')) then
      begin
        FErrorMessage := '';
        if (AChannel.Validate(FErrorMessage, 'UpNodeNumber')) then
        begin
          if (AChannel.UpStreamNodeNumber <> 0) then
          begin
            UpstreamNodeCbx.InValidationError := FALSE;
            UpstreamNodeCbx.ShowErrorState(FALSE);
          end
          else
          begin
            if (AChannelList.Validate(FErrorMessage, 'MaximumZeroUpstreamNode')) then
            begin
              UpstreamNodeCbx.InValidationError := FALSE;
              UpstreamNodeCbx.ShowErrorState(FALSE);
            end
            else
            begin
              UpstreamNodeCbx.InValidationError := TRUE;
              UpstreamNodeCbx.ValidationError := FErrorMessage;
              UpstreamNodeCbx.ShowErrorState(TRUE);
              FAllErrorMessages.Add(Trim(FErrorMessage));
            end;
          end
        end
        else
        begin
          UpstreamNodeCbx.InValidationError := TRUE;
          UpstreamNodeCbx.ValidationError := FErrorMessage;
          UpstreamNodeCbx.ShowErrorState(TRUE);
          FAllErrorMessages.Add(Trim(FErrorMessage));
        end;

        FErrorMessage := '';
        if (AChannel.Validate(FErrorMessage, 'DownNodeNumber')) then
        begin
          if (AChannel.DownStreamNodeNumber <> 0) then
          begin
            DownstreamNodeCbx.InValidationError := FALSE;
            DownstreamNodeCbx.ShowErrorState(FALSE);
          end
          else
          begin
            if (AChannelList.Validate(FErrorMessage, 'MaximumZeroDownstreamNode')) then
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
        UpstreamNodeCbx.InValidationError   := TRUE;
        UpstreamNodeCbx.ValidationError     := FErrorMessage;
        DownstreamNodeCbx.InValidationError := TRUE;
        DownstreamNodeCbx.ValidationError   := FErrorMessage;
        UpstreamNodeCbx.ShowErrorState(TRUE);
        DownstreamNodeCbx.ShowErrorState(TRUE);
        FAllErrorMessages.Add(Trim(FErrorMessage));
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMineToPCDChannelValidator.ProcessMetaDataEvent : boolean;
const OPNAME = 'TMineToPCDChannelValidator.ProcessMetaDataEvent';
var
  lFieldProperty : TAbstractFieldProperty;
  lKeyValues     : string;
  lFieldIndex    : string;
  lChannel       : IGeneralFlowChannel;
begin
  Result := FALSE;
  try
    if (FPanel.Visible AND (FActiveControl <> nil) AND (FIdentifier <> 0)) then
    begin
      lChannel := TYieldModelDataObject(FAppModules.Model.ModelData).
                    NetworkElementData.ChannelList.ChannelByChannelNumber[FIdentifier];
      if (lChannel <> nil) then
      begin
        with MineChannelPropertiesDialog do
        begin
          lFieldIndex    := '';
          lFieldProperty := nil;
          if (FActiveControl = ChannelNameEdit) then
            lFieldProperty := ChannelNameEdit.FieldProperty
          else
          if (FActiveControl = ChannelNumberEdit) then
            lFieldProperty := ChannelNumberEdit.FieldProperty
          else
          if (FActiveControl = UpStreamNodeCbx) then
            lFieldProperty := UpStreamNodeCbx.FieldProperty
          else
          if (FActiveControl = DownStreamNodeCbx) then
            lFieldProperty := DownStreamNodeCbx.FieldProperty
          else
          if (FActiveControl = PenaltyStructureEdit) then
            lFieldProperty := PenaltyStructureEdit.FieldProperty
          else
          if (FActiveControl = SummaryOutputChkBox) then
            lFieldProperty := SummaryOutputChkBox.FieldProperty
          else
          if (FActiveControl = ChannelAreaCbx) then
            lFieldProperty := ChannelAreaCbx.FieldProperty;
          if (lFieldProperty <> nil) then
          begin
            lKeyValues := lChannel.GetKeyValues(lFieldProperty.FieldName, lFieldIndex);
            FAppModules.MetaData.ShowMetaData
              (lFieldProperty.FieldName, lKeyValues, lFieldIndex);
            RePopulateDataViewer;
            Result := TRUE;
          end;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end; }

procedure TMineToPCDChannelValidator.UpdateChannelArea;
const OPNAME = 'TMineToPCDChannelValidator.UpdateChannelArea';
var
  lChannel         : IGeneralFlowChannel;
  lChannelArea     : IChannelArea;
  lChannelAreaList : IChannelAreaList;
  lMessage         : string;
  LMine            : IMine;
  LChannelNr       : integer;
begin
  try
    LMine := TYieldModelDataObject(FAppModules.Model.ModelData).
             CastNetworkFeaturesData.CastMineList.MineByID[FIdentifier];
    if LMine <> nil then
    begin
      LChannelNr := LMine.PCDChannelNumber;
      lChannel := TYieldModelDataObject(FAppModules.Model.ModelData).
                      NetworkElementData.ChannelList.ChannelByChannelNumber[LChannelNr];
      if (lChannel <> nil) then
      begin
        with MineChannelPropertiesDialog do
        begin
          lChannelAreaList := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData
                              .ChannelAreaList;
          if (lChannelAreaList <> nil) then
          begin
            lChannelArea := lChannelAreaList.ChannelAreaByName(ChannelAreaCbx.Text);
            if(lChannelArea <> nil) AND
            (lChannelArea.AreaID <> lChannel.ChannelArea) then
            begin
              if (FAppModules.FieldProperties.ValidateFieldProperty(
                 ChannelAreaCbx.FieldProperty.FieldName,
                 IntToStr(lChannelArea.AreaID),lMessage)) then
              begin
                ChannelAreaCbx.ValidationError := lMessage;
                lChannel.ChannelArea := lChannelArea.AreaID;
                RePopulateDataViewer;
              end
              else
                ChannelAreaCbx.ValidationError := lMessage;
            end;
          end;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMineToPCDChannelValidator.RepopulateChannelAreas;
const OPNAME = 'TMineToPCDChannelValidator.RepopulateChannelAreas';
var
  LAreas           : TStringList;
  LIndex           : integer;
  lChannelAreaName : string;
  lChannelAreaList : IChannelAreaList;
  lChannelAreaData : IChannelArea;
  lChannel         : IGeneralFlowChannel;
  lFieldIndex      : string;
  lFieldProperty   : TAbstractFieldProperty;
  lKeyValues       : string;
  LMine            : IMine;
  LChannelNr       : integer;
begin
  try
    LMine :=  TYieldModelDataObject(FAppModules.Model.ModelData).
              CastNetworkFeaturesData.CastMineList.MineByID[FIdentifier];
    if LMine <> nil then
    begin
      LChannelNr := LMine.PCDChannelNumber;
      lChannel := TYieldModelDataObject(FAppModules.Model.ModelData).
                  NetworkElementData.ChannelList.ChannelByChannelNumber[LChannelNr];
      if (lChannel <> nil) then
      begin
        LAreas := TStringList.Create;
        try
          LAreas.Sorted := True;
          LAreas.Duplicates := dupIgnore;
          for LIndex := 0 to TYieldModelDataObject(FAppModules.Model.ModelData).
                             NetworkFeaturesData.ChannelAreaList.AreaCount -1 do
          begin
            LAreas.Add(TYieldModelDataObject(FAppModules.Model.ModelData).
                       NetworkFeaturesData.ChannelAreaList.ChannelAreaByIndex(LIndex).AreaName)
          end;

          MineChannelPropertiesDialog.ChannelAreaCbx.Items.Assign(LAreas);
          if (lChannel.ChannelArea <> 0) then
          begin
            lChannelAreaName := '';
            lChannelAreaList := TYieldModelDataObject(FAppModules.Model.ModelData).
                                NetworkFeaturesData.ChannelAreaList;
            lChannelAreaData := lChannelAreaList.ChannelAreaByID(lChannel.ChannelArea);

            lFieldIndex    := '';
            lFieldProperty := MineChannelPropertiesDialog.ChannelAreaCbx.FieldProperty;
            lKeyValues     := lChannel.GetKeyValues(lFieldProperty.FieldName, lFieldIndex);
            MineChannelPropertiesDialog.ChannelAreaCbx.HasMetaData :=
              FAppModules.MetaData.FindMetaData(lFieldProperty.FieldName, lKeyValues, lFieldIndex) <> nil;

            if (lChannelAreaData <> nil) then
            begin
              lChannelAreaName := lChannelAreaData.AreaName;
              MineChannelPropertiesDialog.ChannelAreaCbx.ItemIndex :=
              MineChannelPropertiesDialog.ChannelAreaCbx.Items.IndexOf(lChannelAreaName);
            end;
          end;
        finally
          LAreas.Free;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


procedure TMineToPCDChannelValidator.RePopulateNodes;
const OPNAME = 'TMineToPCDChannelValidator.RePopulateNodes';
var
  lReservoirList : IReservoirDataList;
  lIndexA        : integer;
  lReservoir     : IReservoirConfigurationData;
  lMine          : IMine;
begin
  try
    if (FIdentifier >= 0) then
    begin
      lMine := TYieldModelDataObject(FAppModules.Model.ModelData).CastNetworkFeaturesData
      .CastMineList.MineByID[FIdentifier];
      if (lMine <> nil) then
      begin
        with MineChannelPropertiesDialog do
        begin
          lReservoirList := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.ReservoirList;
          if (lReservoirList <> nil) then
          begin
            for lIndexA := 0 to lReservoirList.ReservoirAndNodesCount - 1 do
            begin
              lReservoir := lReservoirList.ReservoirOrNodeByIndex[lIndexA].ReservoirConfigurationData;
              if (lReservoir.NodeType <> ntMinePolutionControlDam) then
              begin
                DownStreamNodeCbx.Items.AddObject
                    ('(' + IntToStr(lReservoir.ReservoirIdentifier) + ') ' + lReservoir.ReservoirName,
                     TObject(lReservoir.ReservoirIdentifier));
              end;
            end;
          end;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


end.

