{******************************************************************************}
{*  UNIT      : Contains the class TIrrigationBlockReturnFlowChannelValidator *}
{*  AUTHOR    : Maurice Marinus                                               *}
{*  DATE      : 2006/07/31                                                    *}
{*  COPYRIGHT : Copyright © 2006 DWAF                                         *}
{******************************************************************************}

unit UIrrigationBlockReturnFlowChannelValidator;

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
  VCL.dialogs,
  UChannelPenaltyValidator,
  USelectChannelValidator,
  UIrrigationBlockReturnFlowChannelDialog,
  UAbstractYieldDataDialogValidator,
  UYieldContextValidationType;

type

  TIrrigationBlockReturnFlowChannelValidator = class(TAbstractYieldDataDialogValidator)
  protected
    FHeading                 : string;
    FUpStreamEnabled         : Boolean;
    FDownStreamEnabled       : Boolean;
    FSelectChannelEnabled    : Boolean;
    FChannelPenaltyValidator : TChannelPenaltyValidator;
    FSelectChannelValidator  : TSelectChannelValidator;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    procedure CreateDialog; virtual;
    procedure OnEditControlEnter(Sender: TObject); override;
    procedure OnEditControltExit(Sender: TObject); override;
    procedure OnSummaryOutputClick(Sender: TObject);
    procedure OnFirmYieldAnalysisClick(Sender: TObject);
    procedure UpdateChannelName;
    procedure UpdateChannelUpstreamNode;
    procedure UpdateChannelDownstreamNode;
    procedure UpdateChannelArea;
    procedure UpdateChannelPenaltyStructure; virtual;
    procedure UpdateChannelIncludeSummary; virtual;
    procedure UpdateIrrigationBlockReturnFlowLoss;
    procedure UpdateIrrigationBlockReturnFlowFactor;
    procedure OnSelectPenaltyClick(Sender: TObject);
    procedure OnSelectChannelClick(Sender: TObject);
    procedure UpdateChannelFirmYieldAnalysis;
    procedure DisableModelControls;
    procedure RePopulateDataViewer;
    procedure RePopulateNodes;
    procedure RepopulateChannelAreas;
    procedure ShowChannelPenaltyDialog (ASender : TObject);
    procedure ShowSelectChannelDialog (ASender : TObject);
    procedure ValidateIrrigationBlockReturnFlowLoss;
    procedure ValidateIrrigationBlockReturnFlowFactor;
    procedure ValidateChannelNumber (AChannel : IGeneralFlowChannel);
    procedure ValidateChannelName (AChannel : IGeneralFlowChannel);
    procedure ValidateChannelPenalty (AChannel : IGeneralFlowChannel);
    procedure ValidateUpAndDownstreamNodes (AChannelList : IChannelList;
                                            AChannel     : IGeneralFlowChannel);
  public
    function Initialise: boolean; override;
    function SaveState: boolean; override;
    function LanguageHasChanged: boolean; override;
    function StudyHasChanged: boolean; override;
    function StudyDataHasChanged(AContext: TChangeContext; AFieldName,AOldValue,ANewValue: string): boolean; override;
    function ProcessMetaDataEvent : boolean; override;
    procedure ClearDataViewer; override;
    procedure PopulateDataViewer; override;
    procedure DoContextValidation (AValidationType : TDialogValidationType); override;
    function DetermineWizardStatus (ASequence : integer = 0) : integer; override;
    function GeneralFlowChannelDialog: TIrrigationBlockReturnFlowChannelDialog;
    property Heading           : string  read FHeading           write FHeading;
  end;

implementation

uses
  SysUtils,
  VCL.Graphics,
  UYieldModelDataGUIForm,
  UChannelPenaltyDialog,
  UYieldModelDataObject,
  UErrorHandlingOperations,
  USelectChannelDialog, UNetworkElementData;

{******************************************************************************}
{* TIrrigationBlockReturnFlowChannelValidator                                 *}
{******************************************************************************}

procedure TIrrigationBlockReturnFlowChannelValidator.CreateMemberObjects;
const OPNAME = 'TIrrigationBlockReturnFlowChannelValidator.CreateMemberObjects';
var
  lpPanel     : TIrrigationBlockReturnFlowChannelDialog;
begin
  try
    inherited CreateMemberObjects;
    FHeading                 := 'NetworkFeatures.ReturnFlowChannelProperties';
    FUpStreamEnabled         := False;
    FDownStreamEnabled       := False;
    FSelectChannelEnabled    := False;

    FChannelPenaltyValidator := nil;
    FSelectChannelValidator  := nil;
    CreateDialog;
    lpPanel := GeneralFlowChannelDialog;
    with lpPanel do
    begin
      ChannelNumberEdit.FieldProperty    := FAppModules.FieldProperties.FieldProperty('ChannelNumber');
      ChannelNumberEdit.ReadOnly         := TRUE;
      ChannelNumberEdit.OnEnter          := OnEditControlEnter;
      ChannelNumberEdit.OnExit           := OnEditControltExit;

      ChannelNameEdit.FieldProperty      := FAppModules.FieldProperties.FieldProperty('ChannelName');
      ChannelNameEdit.OnEnter            := OnEditControlEnter;
      ChannelNameEdit.OnExit             := OnEditControltExit;

      SummaryOutputChkBox.FieldProperty  := FAppModules.FieldProperties.FieldProperty('SummaryOutput');
      SummaryOutputChkBox.OnEnter        := OnEditControlEnter;
      SummaryOutputChkBox.OnClick        := OnSummaryOutputClick;

      UpstreamNodeCbx.FieldProperty      := FAppModules.FieldProperties.FieldProperty('UpNodeNumber');
      UpstreamNodeCbx.OnEnter            := OnEditControlEnter;
      UpstreamNodeCbx.OnExit             := OnEditControltExit;

      DownStreamNodeCbx.FieldProperty    := FAppModules.FieldProperties.FieldProperty('DownNodeNumber');
      DownStreamNodeCbx.OnEnter          := OnEditControlEnter;
      DownStreamNodeCbx.OnExit           := OnEditControltExit;

      PenaltyStructureEdit.FieldProperty := FAppModules.FieldProperties.FieldProperty('PenaltyNumber');
      PenaltyStructureEdit.ReadOnly      := TRUE;
      PenaltyStructureEdit.Color         := clWindow;
      PenaltyStructureEdit.OnEnter       := OnEditControlEnter;
      PenaltyStructureEdit.OnExit        := OnEditControltExit;

      SelectPenaltyStructureBtn.FieldProperty := FAppModules.FieldProperties.FieldProperty('PenaltyNumber');
      SelectPenaltyStructureBtn.OnEnter  := OnEditControlEnter;
      SelectPenaltyStructureBtn.OnExit   := OnEditControltExit;
      SelectPenaltyStructureBtn.OnClick  := OnSelectPenaltyClick;

      SelectChannelBtn.OnClick           := OnSelectChannelClick;

      ChannelAreaCbx.FieldProperty       := FAppModules.FieldProperties.FieldProperty('ChannelAreaID');
      ChannelAreaCbx.OnEnter             := OnEditControlEnter;
      ChannelAreaCbx.OnExit              := OnEditControltExit;

      FirmYieldAnalysisChkBox.FieldProperty := FAppModules.FieldProperties.FieldProperty('FirmYieldCalc');
      FirmYieldAnalysisChkBox.OnEnter       := OnEditControlEnter;
      FirmYieldAnalysisChkBox.OnClick       := OnFirmYieldAnalysisClick;

      ReturnFlowLossEdit.OnEnter           := OnEditControlEnter;
      ReturnFlowLossEdit.OnExit            := OnEditControltExit;
      ReturnFlowLossEdit.FieldProperty     := FAppModules.FieldProperties.FieldProperty('IrrigationBlockReturnFlowLoss');

      ReturnFlowFactorEdit.OnEnter         := OnEditControlEnter;
      ReturnFlowFactorEdit.OnExit          := OnEditControltExit;
      ReturnFlowFactorEdit.FieldProperty   := FAppModules.FieldProperties.FieldProperty('IrrigationBlockReturnFlowFactor');
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrigationBlockReturnFlowChannelValidator.CreateDialog;
const OPNAME = 'TIrrigationBlockReturnFlowChannelValidator.CreateDialog';
begin
  try
    FPanel  := TIrrigationBlockReturnFlowChannelDialog.Create(FPanelOwner,FAppModules);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrigationBlockReturnFlowChannelValidator.DestroyMemberObjects;
const OPNAME = 'TIrrigationBlockReturnFlowChannelValidator.DestroyMemberObjects';
begin
  try
    inherited DestroyMemberObjects;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TIrrigationBlockReturnFlowChannelValidator.Initialise: boolean;
const OPNAME = 'TIrrigationBlockReturnFlowChannelValidator.Initialise';
begin
  Result := inherited Initialise;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TIrrigationBlockReturnFlowChannelValidator.LanguageHasChanged: boolean;
const OPNAME = 'TIrrigationBlockReturnFlowChannelValidator.LanguageHasChanged';
begin
  Result := False;
  try
    TabShetCaption := FAppModules.Language.GetString(FHeading);
    Result := inherited LanguageHasChanged;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrigationBlockReturnFlowChannelValidator.ClearDataViewer;
const OPNAME = 'TIrrigationBlockReturnFlowChannelValidator.ClearDataViewer';
var
  lpPanel     : TIrrigationBlockReturnFlowChannelDialog;
begin
  inherited ClearDataViewer;
  try
    lpPanel := GeneralFlowChannelDialog;
    with lpPanel do
    begin
      ChannelNameEdit.Text        := '';
      ChannelNumberEdit.Text      := '-1';
      UpStreamNodeCbx.ItemIndex   := -1;
      DownStreamNodeCbx.ItemIndex := -1;
      UpStreamNodeEdit.Text       := '-1';
      DownStreamNodeEdit.Text     := '-1';
      PenaltyStructureEdit.Text   := '-1';
      SummaryOutputChkBox.Checked := FALSE;
      ReturnFlowLossEdit.Text     := '';
      ReturnFlowFactorEdit.Text   := '';
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrigationBlockReturnFlowChannelValidator.PopulateDataViewer;
const OPNAME = 'TIrrigationBlockReturnFlowChannelValidator.PopulateDataViewer';
begin
  inherited PopulateDataViewer;
  try
    ClearDataViewer;
    RePopulateDataViewer;
    DoContextValidation(dvtChanPropAll);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrigationBlockReturnFlowChannelValidator.RePopulateNodes;
const OPNAME = 'TIrrigationBlockReturnFlowChannelValidator.RePopulateNodes';
var
  lChannel       : IGeneralFlowChannel;
  lReservoirList : IReservoirDataList;
  lIndexA        : integer;
  lReservoir     : IReservoirConfigurationData;
  lIrrigationBlock : IIrrigationBlock;
begin
  try
    GeneralFlowChannelDialog.UpStreamNodeCbx.Items.Clear;
    GeneralFlowChannelDialog.DownStreamNodeCbx.Items.Clear;
    if (FIdentifier >= 0) then
    begin
      lChannel := nil;
      lIrrigationBlock := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.IrrigationBlockList.IrrigationBlockByBlockNodeNumber[FIdentifier];
      If (lIrrigationBlock <> nil) then
        lChannel := lIrrigationBlock.ReturnFlowChannel;
      if (lChannel <> nil) then
      begin
        with GeneralFlowChannelDialog do
        begin
          lReservoirList := TYieldModelDataObject(FAppModules.Model.ModelData).
                              NetworkElementData.ReservoirList;
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
                UpStreamNodeCbx.Items.AddObject('(0) ' + UpperCase(lChannel.SourceName),
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
                else
                end;
                case lReservoir.NodeType of
                  ntReservoir,
                  ntNodeWithInflow,
                  ntNodeWithoutInflow :
                    UpStreamNodeCbx.Items.AddObject
                      ('(' + IntToStr(lReservoir.ReservoirIdentifier) + ') ' + lReservoir.ReservoirName,
                       TObject(lReservoir.ReservoirIdentifier));
                  ntIrrigationNode :
                    if ((lChannel.ChannelType = 4) AND
                        ((lChannel.ChannelSubType = 2) OR (lChannel.ChannelSubType = 3))) then
                      UpStreamNodeCbx.Items.AddObject
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

procedure TIrrigationBlockReturnFlowChannelValidator.RePopulateDataViewer;
const OPNAME = 'TIrrigationBlockReturnFlowChannelValidator.RePopulateDataViewer';
var
  lReservoirData   : IReservoirData;
  lReservoir       : IReservoirConfigurationData;
  lPenalty         : IChannelPenalty;
  lFieldProperty   : TAbstractFieldProperty;
  lKeyValues       : string;
  lChannel         : IGeneralFlowChannel;
  lIrrigationBlock : IIrrigationBlock;
begin
  try
    if (FIdentifier >= 0) then
    begin
      lChannel := nil;
      lIrrigationBlock := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.IrrigationBlockList.IrrigationBlockByBlockNodeNumber[FIdentifier];
      If (lIrrigationBlock <> nil) then
        lChannel := lIrrigationBlock.ReturnFlowChannel;
      if (lChannel <> nil) then
      begin
        with GeneralFlowChannelDialog do
        begin
          lFieldProperty := ChannelNumberEdit.FieldProperty;
          lKeyValues := lChannel.GetKeyValues(lFieldProperty.FieldName, '');
          ChannelNumberEdit.HasMetaData := FAppModules.MetaData.FindMetaData(lFieldProperty.FieldName, lKeyValues, '') <> nil;
          ChannelNumberEdit.SetFieldValue(IntToStr(lChannel.ChannelNumber));

          lFieldProperty := ChannelNameEdit.FieldProperty;
          ChannelNameEdit.HasMetaData := FAppModules.MetaData.FindMetaData(lFieldProperty.FieldName, lKeyValues, '') <> nil;
          ChannelNameEdit.SetFieldValue(lChannel.ChannelName);

          RePopulateNodes;

          lReservoirData := TYieldModelDataObject(FAppModules.Model.ModelData).CastNetworkElementData.
                              ReservoirList.ReservoirOrNodeByIdentifier[lChannel.UpStreamNodeNumber];
          if (lReservoirData <> nil) then
          begin
            lFieldProperty := UpStreamNodeCbx.FieldProperty;
            UpStreamNodeCbx.HasMetaData := FAppModules.MetaData.FindMetaData(lFieldProperty.FieldName, lKeyValues, '') <> nil;
            lReservoir := lReservoirData.ReservoirConfigurationData;
            UpstreamNodeCbx.SetFieldIndex
              (UpstreamNodeCbx.Items.IndexOfObject(TObject(lReservoir.ReservoirIdentifier)));
          end;
          lReservoirData := TYieldModelDataObject(FAppModules.Model.ModelData).CastNetworkElementData.
                              ReservoirList.ReservoirOrNodeByIdentifier[lChannel.DownStreamNodeNumber];
          if (lReservoirData <> nil) then
          begin
            lFieldProperty := DownStreamNodeCbx.FieldProperty;
            DownStreamNodeCbx.HasMetaData := FAppModules.MetaData.FindMetaData(lFieldProperty.FieldName, lKeyValues, '') <> nil;
            lReservoir := lReservoirData.ReservoirConfigurationData;
            DownstreamNodeCbx.SetFieldIndex
              (DownstreamNodeCbx.Items.IndexOfObject(TObject(lReservoir.ReservoirIdentifier)));
          end;

          UpStreamNodeCbx.Visible    := FUpStreamEnabled;
          UpStreamNodeEdit.Visible   := NOT FUpStreamEnabled;
          UpStreamNodeEdit.Text      := UpStreamNodeCbx.Text;
          DownStreamNodeCbx.Visible  := FDownStreamEnabled;
          DownStreamNodeEdit.Visible := NOT FDownStreamEnabled;
          DownStreamNodeEdit.Text    := DownStreamNodeCbx.Text;
          SelectChannelBtn.Visible   := FSelectChannelEnabled;

          RepopulateChannelAreas;

          lFieldProperty := PenaltyStructureEdit.FieldProperty;
          PenaltyStructureEdit.HasMetaData := FAppModules.MetaData.FindMetaData(lFieldProperty.FieldName, lKeyValues, '') <> nil;
          lPenalty := lChannel.ChannelPenalty;
          if (lPenalty <> nil) then
            PenaltyStructureEdit.SetFieldValue(lPenalty.ChannelPenaltyID)
          else
            PenaltyStructureEdit.SetFieldValue(0);

          lFieldProperty := SummaryOutputChkBox.FieldProperty;
          SummaryOutputChkBox.HasMetaData := FAppModules.MetaData.FindMetaData(lFieldProperty.FieldName, lKeyValues, '') <> nil;
          SummaryOutputChkBox.Checked := (lChannel.SummaryOutputRequired = 'Y');

           LIrrigationBlock  := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.IrrigationBlockList.IrrigationBlockByBlockNodeNumber[FIdentifier];
           if(LIrrigationBlock <> nil) then
           begin
             UpStreamNodeEdit.Text := LIrrigationBlock.BlockName;
           end;
          DisableModelControls;
        end;
      end;
    end;
    with GeneralFlowChannelDialog do
    begin
      ReturnFlowFactorEdit.SetFieldValue(lIrrigationBlock.ReturnFlowFactor);
      ReturnFlowLossEdit.SetFieldValue(lIrrigationBlock.ReturnFlowLoss);
    end;  
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TIrrigationBlockReturnFlowChannelValidator.SaveState: boolean;
const OPNAME = 'TIrrigationBlockReturnFlowChannelValidator.SaveState';
begin
  Result := inherited SaveState;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TIrrigationBlockReturnFlowChannelValidator.GeneralFlowChannelDialog:TIrrigationBlockReturnFlowChannelDialog;
const OPNAME = 'TIrrigationBlockReturnFlowChannelValidator.GeneralFlowChannelDialog';
begin
  Result := Nil;
  try
    if (FPanel <> nil) then
      Result := TIrrigationBlockReturnFlowChannelDialog(FPanel);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TIrrigationBlockReturnFlowChannelValidator.StudyDataHasChanged(AContext: TChangeContext; AFieldName,AOldValue,ANewValue: string): boolean;
const OPNAME = 'TIrrigationBlockReturnFlowChannelValidator.StudyDataHasChanged';
begin
  Result := inherited StudyDataHasChanged(AContext,AFieldName,AOldValue,ANewValue);
  try
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
      RePopulateDataViewer;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TIrrigationBlockReturnFlowChannelValidator.StudyHasChanged: boolean;
const OPNAME = 'TIrrigationBlockReturnFlowChannelValidator.StudyHasChanged';
begin
  Result := inherited StudyHasChanged;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrigationBlockReturnFlowChannelValidator.OnSelectPenaltyClick(Sender: TObject);
const OPNAME = 'TIrrigationBlockReturnFlowChannelValidator.OnSelectPenaltyClick';
var
  LForm            : TYieldModelDataGUIForm;
  LDialogValidator : TChannelPenaltyValidator;
  LSelectedPenalty : integer;
  lChannel         : IGeneralFlowChannel;
  lIrrigationBlock : IIrrigationBlock;
begin
  try
    lChannel := nil;
    lIrrigationBlock := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.IrrigationBlockList.IrrigationBlockByBlockNodeNumber[FIdentifier];
    If (lIrrigationBlock <> nil) then
      lChannel := lIrrigationBlock.ReturnFlowChannel;
    if (lChannel <> nil) then
    begin
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
          LDialogValidator.ChannelNumber := lChannel.ChannelNumber;
          LDialogValidator.PopulateDataViewer;
          LDialogValidator.LanguageHasChanged;
          lForm.OnShow := ShowChannelPenaltyDialog;

          LForm.ShowModal;
          if (LForm.ModalResult = mrOk) and (TChannelPenaltyDialog(LDialogValidator.Panel).ActivePanel <> nil)then
          begin
            LSelectedPenalty := StrToInt(TChannelPenaltyDialog(LDialogValidator.Panel).ActivePanel.IDLabel.Caption);
            if(GeneralFlowChannelDialog.PenaltyStructureEdit.Text <> IntToStr(LSelectedPenalty)) then
            begin
              GeneralFlowChannelDialog.PenaltyStructureEdit.Text := IntToStr(LSelectedPenalty);
              GeneralFlowChannelDialog.PenaltyStructureEdit.OnExit(GeneralFlowChannelDialog.PenaltyStructureEdit);
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

procedure TIrrigationBlockReturnFlowChannelValidator.ShowChannelPenaltyDialog(ASender: TObject);
const OPNAME = 'TIrrigationBlockReturnFlowChannelValidator.ShowChannelPenaltyDialog';
begin
  try
    if (FChannelPenaltyValidator <> nil) then
      FChannelPenaltyValidator.ChannelPenaltyDialog.Resize;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrigationBlockReturnFlowChannelValidator.OnSelectChannelClick(Sender: TObject);
const OPNAME = 'TIrrigationBlockReturnFlowChannelValidator.OnSelectChannelClick';
{
var
  LForm            : TYieldModelDataGUIForm;
  LDialogValidator : TSelectChannelValidator;
  lSelectedNumber  : integer;
  lOldChannel      : TGeneralFlowChannel;
  lNewChannel      : TGeneralFlowChannel;
  lIndex           : integer;}
begin
  try
{    LForm   := TYieldModelDataGUIForm.CreateWithoutDFM(nil,FAppModules);
    try
      LForm.Initialise;
      LForm.LanguageHasChanged;

      LDialogValidator := TSelectChannelValidator.Create(LForm,FAppModules);
      FSelectChannelValidator := LDialogValidator;
      try
        LForm.AddModelDataPanel(LDialogValidator.Panel);
        LDialogValidator.Initialise;
        LDialogValidator.ChannelNumber := FIdentifier;
        LDialogValidator.PopulateDataViewer;
        LDialogValidator.LanguageHasChanged;
        lForm.OnShow := ShowSelectChannelDialog;

        LForm.ShowModal;
        if (LForm.ModalResult = mrOk) then
        begin
          with LDialogValidator.SelectChannelDialog do
          begin
            lIndex := ChannelsListBox.ItemIndex;
            if (lIndex >= 0) then
            begin
              LSelectedNumber := Integer(ChannelsListBox.Items.Objects[lIndex]);
              if (FIdentifier <> LSelectedNumber) then
              begin
                lOldChannel := TYieldModelDataObject(FAppModules.Model.ModelData).
                                 CastNetworkElementData.CastChannelList.CastChannelByChannelNumber[FIdentifier];
                lNewChannel := TYieldModelDataObject(FAppModules.Model.ModelData).
                                 CastNetworkElementData.CastChannelList.CastChannelByChannelNumber[LSelectedNumber];
                if (lOldChannel <> nil) then
                begin
                  if (lOldChannel.DiversionFeature <> nil) then
                  begin

                  end;
                  FIdentifier := LSelectedNumber;
                  RePopulateDataViewer;
                end;
              end;
            end;
          end;
        end;
      finally
        FSelectChannelValidator := nil;
        LDialogValidator.Free;
      end;
    finally
      LForm.Free;
    end;}
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrigationBlockReturnFlowChannelValidator.UpdateChannelFirmYieldAnalysis;
const OPNAME = 'TIrrigationBlockReturnFlowChannelValidator.UpdateChannelFirmYieldAnalysis';
var
  lOldValue : string;
  lNewValue : string;
  lMessage  : string;
  lChannel  : IGeneralFlowChannel;
  lIrrigationBlock : IIrrigationBlock;
begin
  try
    lChannel := nil;
    lIrrigationBlock := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.IrrigationBlockList.IrrigationBlockByBlockNodeNumber[FIdentifier];
    If (lIrrigationBlock <> nil) then
      lChannel := lIrrigationBlock.ReturnFlowChannel;
    if (lChannel <> nil ) then
    begin
      with GeneralFlowChannelDialog do
      begin
        lOldValue := UpperCase(Trim(lChannel.RequiresFirmYieldAnalysis));
        if FirmYieldAnalysisChkBox.Checked then
          lNewValue := 'Y'
        else
          lNewValue := 'N';
        if (lOldValue <> lNewvalue) then
        begin
          if (FAppModules.FieldProperties.ValidateFieldProperty(
              FirmYieldAnalysisChkBox.FieldProperty.FieldName,
              lNewValue,lMessage)) then
          begin
            lChannel.RequiresFirmYieldAnalysis := lNewValue;
            FirmYieldAnalysisChkBox.Checked := (lChannel.RequiresFirmYieldAnalysis = 'Y');
          end
          else
            FirmYieldAnalysisChkBox.ValidationError := lMessage;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrigationBlockReturnFlowChannelValidator.DisableModelControls;
const OPNAME = 'TIrrigationBlockReturnFlowChannelValidator.DisableModelControls';
var
  lFirmYield  : boolean;
  lChannel    : IGeneralFlowChannel;
  lConfigData : IRunConfigurationData;
  lIrrigationBlock : IIrrigationBlock;
begin
  try
    lChannel := nil;
    lIrrigationBlock := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.IrrigationBlockList.IrrigationBlockByBlockNodeNumber[FIdentifier];
    If (lIrrigationBlock <> nil) then
      lChannel := lIrrigationBlock.ReturnFlowChannel;
    if (lChannel <> nil) then
    begin
      GeneralFlowChannelDialog.FirmYieldAnalysisChkBox.Visible := False;
      if (FAppModules.Model.ModelName = CPlanning) then
      begin
        lConfigData := TYieldModelDataObject(FAppModules.Model.ModelData).RunConfigurationData;
        if (lConfigData <> nil) then
        begin
          lFirmYield := lConfigData.CreatePlotFile;
          GeneralFlowChannelDialog.FirmYieldAnalysisChkBox.Visible := lFirmYield;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrigationBlockReturnFlowChannelValidator.ShowSelectChannelDialog(ASender: TObject);
const OPNAME = 'TIrrigationBlockReturnFlowChannelValidator.ShowSelectChannelDialog';
begin
  try
    if (FSelectChannelValidator <> nil) then
      FSelectChannelValidator.SelectChannelDialog.Resize;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrigationBlockReturnFlowChannelValidator.OnEditControlEnter(Sender: TObject);
const OPNAME = 'TIrrigationBlockReturnFlowChannelValidator.OnEditControlEnter';
begin
  inherited OnEditControlEnter(Sender);
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrigationBlockReturnFlowChannelValidator.OnEditControltExit(Sender: TObject);
const OPNAME = 'TIrrigationBlockReturnFlowChannelValidator.OnEditControltExit';
begin
  inherited OnEditControltExit(Sender);
  try
    with GeneralFlowChannelDialog do
    begin
      if ((Sender = ChannelNameEdit) AND
          (ChannelNameEdit.HasValueChanged)) then
        UpdateChannelName
      else
      if ((Sender = PenaltyStructureEdit) AND
          (PenaltyStructureEdit.HasValueChanged)) then
        UpdateChannelPenaltyStructure
      else
      if ((Sender = UpstreamNodeCbx) AND
          (UpstreamNodeCbx.HasValueChanged)) then
        UpdateChannelUpstreamNode
      else
      if ((Sender = DownstreamNodeCbx) AND
          (DownstreamNodeCbx.HasValueChanged)) then
        UpdateChannelDownstreamNode
      else
      if ((Sender = ChannelAreaCbx) AND (ChannelAreaCbx.HasValueChanged)) then
        UpdateChannelArea
      else
      if (Sender=ReturnFlowLossEdit) and (ReturnFlowLossEdit.HasValueChanged) then
        UpdateIrrigationBlockReturnFlowLoss
      else
      if (Sender=ReturnFlowFactorEdit) and (ReturnFlowFactorEdit.HasValueChanged) then
        UpdateIrrigationBlockReturnFlowFactor;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrigationBlockReturnFlowChannelValidator.OnSummaryOutputClick(Sender: TObject);
const OPNAME = 'TIrrigationBlockReturnFlowChannelValidator.OnSummaryOutputClick';
begin
  try
    if(GeneralFlowChannelDialog.SummaryOutputChkBox.HasValueChanged) then
      UpdateChannelIncludeSummary;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrigationBlockReturnFlowChannelValidator.OnFirmYieldAnalysisClick(Sender: TObject);
const OPNAME = 'TIrrigationBlockReturnFlowChannelValidator.OnFirmYieldAnalysisClick';
begin
  try
    if(GeneralFlowChannelDialog.FirmYieldAnalysisChkBox.HasValueChanged) then
      UpdateChannelFirmYieldAnalysis;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrigationBlockReturnFlowChannelValidator.UpdateChannelName;
const OPNAME = 'TIrrigationBlockReturnFlowChannelValidator.UpdateChannelName';
var
  lChannel : IGeneralFlowChannel;
  lMessage : string;
  lIrrigationBlock : IIrrigationBlock;
begin
  try
    lChannel := nil;
    lIrrigationBlock := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.IrrigationBlockList.IrrigationBlockByBlockNodeNumber[FIdentifier];
    If (lIrrigationBlock <> nil) then
      lChannel := lIrrigationBlock.ReturnFlowChannel;
    if (lChannel <> nil) then
    begin
      with GeneralFlowChannelDialog do
      begin
        if (FAppModules.FieldProperties.ValidateFieldProperty(
            ChannelNameEdit.FieldProperty.FieldName,
            ChannelNameEdit.Text,lMessage)) then
        begin
          ChannelNameEdit.FieldValidationError := lMessage;
          lChannel.ChannelName := Trim(ChannelNameEdit.Text);
          ChannelNameEdit.SetFieldValue(lChannel.ChannelName);
          DoContextValidation(dvtChanPropName);
        end
        else
          ChannelNameEdit.FieldValidationError := lMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrigationBlockReturnFlowChannelValidator.UpdateChannelPenaltyStructure;
const OPNAME = 'TIrrigationBlockReturnFlowChannelValidator.UpdateChannelPenaltyStructure';
var
  lPenaltyTypeList  : IChannelPenaltyList;
  lPenaltyStructure : IChannelPenalty;
  lPenaltyID        : integer;
  lChannel          : IGeneralFlowChannel;
  lMessage          : string;
  lIrrigationBlock : IIrrigationBlock;
begin
  try
    lChannel := nil;
    lIrrigationBlock := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.IrrigationBlockList.IrrigationBlockByBlockNodeNumber[FIdentifier];
    If (lIrrigationBlock <> nil) then
      lChannel := lIrrigationBlock.ReturnFlowChannel;
    if (lChannel <> nil) then
    begin
      with GeneralFlowChannelDialog do
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

procedure TIrrigationBlockReturnFlowChannelValidator.UpdateChannelUpstreamNode;
const OPNAME = 'TIrrigationBlockReturnFlowChannelValidator.UpdateChannelUpstreamNode';
var
  lReservoir     : IReservoirData;
  lReservoirNr   : integer;
  lChannel       : IGeneralFlowChannel;
  lMessage       : string;
  lReservoirList : IReservoirDataList;
  lIrrigationBlock : IIrrigationBlock;
begin
  try
    lChannel := nil;
    lIrrigationBlock := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.IrrigationBlockList.IrrigationBlockByBlockNodeNumber[FIdentifier];
    If (lIrrigationBlock <> nil) then
      lChannel := lIrrigationBlock.ReturnFlowChannel;
    if (lChannel <> nil) then
    begin
      with GeneralFlowChannelDialog do
      begin
        lReservoirList := TYieldModelDataObject(FAppModules.Model.ModelData).
                            NetworkElementData.ReservoirList;
        lReservoir   := nil;
        lReservoirNr := -1;
        if (UpstreamNodeCbx.ItemIndex >= 0) then
        begin
          lReservoirNr := Integer(UpstreamNodeCbx.Items.Objects[UpstreamNodeCbx.ItemIndex]);
          lReservoir := lReservoirList.ReservoirOrNodeByIdentifier[lReservoirNr];
        end;
        if (FAppModules.FieldProperties.ValidateFieldProperty(
            UpstreamNodeCbx.FieldProperty.FieldName,
            IntToStr(lReservoirNr),lMessage)) then
        begin
          lChannel.UpStreamNodeNumber := lReservoirNr;
          lReservoir := lChannel.UpStreamNode;
          UpstreamNodeCbx.SetFieldIndex
            (UpstreamNodeCbx.Items.IndexOfObject
              (TObject(lReservoir.ReservoirConfigurationData.ReservoirIdentifier)));
          DoContextValidation(dvtChanPropUpstreamNode);
        end
        else
          UpstreamNodeCbx.ValidationError := lMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrigationBlockReturnFlowChannelValidator.UpdateChannelDownstreamNode;
const OPNAME = 'TIrrigationBlockReturnFlowChannelValidator.UpdateChannelDownstreamNode';
var
  lReservoir     : IReservoirData;
  lReservoirNr   : integer;
  lChannel       : IGeneralFlowChannel;
  lMessage       : string;
  lReservoirList : IReservoirDataList;
  lIrrigationBlock : IIrrigationBlock;
begin
  try
    lChannel := nil;
    lIrrigationBlock := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.IrrigationBlockList.IrrigationBlockByBlockNodeNumber[FIdentifier];
    If (lIrrigationBlock <> nil) then
      lChannel := lIrrigationBlock.ReturnFlowChannel;
    if (lChannel <> nil) then
    begin
      with GeneralFlowChannelDialog do
      begin
        lReservoirList := TYieldModelDataObject(FAppModules.Model.ModelData).
                            NetworkElementData.ReservoirList;
        lReservoir   := nil;
        lReservoirNr := -1;
        if (DownstreamNodeCbx.ItemIndex >= 0) then
        begin
          lReservoirNr := Integer(DownstreamNodeCbx.Items.Objects[DownstreamNodeCbx.ItemIndex]);
          lReservoir := lReservoirList.ReservoirOrNodeByIdentifier[lReservoirNr];
        end;
        if (FAppModules.FieldProperties.ValidateFieldProperty(
            DownstreamNodeCbx.FieldProperty.FieldName,
            IntToStr(lReservoirNr), lMessage)) then
        begin
          lChannel.DownstreamNodeNumber := lReservoirNr;
          lReservoir := lChannel.DownStreamNode;
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

procedure TIrrigationBlockReturnFlowChannelValidator.UpdateChannelIncludeSummary;
const OPNAME = 'TIrrigationBlockReturnFlowChannelValidator.UpdateChannelIncludeSummary';
var
  lChannel    : IGeneralFlowChannel;
  lOldInclude : string;
  lNewInclude : string;
  lMessage    : string;
  lIrrigationBlock : IIrrigationBlock;
begin
  try
    lChannel := nil;
    lIrrigationBlock := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.IrrigationBlockList.IrrigationBlockByBlockNodeNumber[FIdentifier];
    If (lIrrigationBlock <> nil) then
      lChannel := lIrrigationBlock.ReturnFlowChannel;
    if (lChannel <> nil) then
    begin
      with GeneralFlowChannelDialog do
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
          end
          else
            SummaryOutputChkBox.ValidationError := lMessage;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrigationBlockReturnFlowChannelValidator.DoContextValidation(AValidationType : TDialogValidationType);
const OPNAME = 'TIrrigationBlockReturnFlowChannelValidator.DoContextValidation';
var
  lChannel     : IGeneralFlowChannel;
  lChannelList : IChannelList;
  lIrrigationBlock : IIrrigationBlock;
begin
  try
    FAllErrorMessages.Clear;
    if (FIdentifier >= 0) then
    begin
      lChannel := nil;
      lChannelList := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.ChannelList;
      lIrrigationBlock := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.IrrigationBlockList.IrrigationBlockByBlockNodeNumber[FIdentifier];
      If (lIrrigationBlock <> nil) then
        lChannel := lIrrigationBlock.ReturnFlowChannel;
      if (lChannel <> nil) then
      begin
        if (AValidationType in [dvtChanPropAll, dvtChanPropWizardStep2, dvtChanPropName]) then
          ValidateChannelName(lChannel);
        if (AValidationType in [dvtChanPropAll, dvtChanPropWizardStep2, dvtChanPropPenalty]) then
          ValidateChannelPenalty(lChannel);
        if (AValidationType in [dvtChanPropAll, dvtChanPropWizardStep1, dvtChanPropUpstreamNode,
                                dvtChanPropDownstreamNode]) then
          ValidateUpAndDownstreamNodes(lChannelList, lChannel);
        if (AValidationType in [dvtChanPropAll, dvtChanPropWizardStep2, dvtChanPropNumber]) then
          ValidateChannelNumber(lChannel);
      end;
    end;
    if (AValidationType = dvtChanPropAll) or (AValidationType=dvtReturnFlowLoss) then
      ValidateIrrigationBlockReturnFlowLoss;
    if (AValidationType = dvtChanPropAll) or (AValidationType=dvtIrrigationBlockReturnFlowFactor) then
      ValidateIrrigationBlockReturnFlowFactor;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TIrrigationBlockReturnFlowChannelValidator.DetermineWizardStatus (ASequence : integer = 0) : integer;
const OPNAME = 'TIrrigationBlockReturnFlowChannelValidator.DetermineWizardStatus';
var
  lChannel     : IGeneralFlowChannel;
  lIrrigationBlock : IIrrigationBlock;
 begin
  Result := inherited DetermineWizardStatus(ASequence);
  try
    if (FIdentifier >= 0) then
    begin
      lChannel := nil;
      lIrrigationBlock := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.IrrigationBlockList.IrrigationBlockByBlockNodeNumber[FIdentifier];
      If (lIrrigationBlock <> nil) then
        lChannel := lIrrigationBlock.ReturnFlowChannel;
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
end;

procedure TIrrigationBlockReturnFlowChannelValidator.ValidateChannelNumber(AChannel: IGeneralFlowChannel);
const OPNAME = 'TIrrigationBlockReturnFlowChannelValidator.ValidateChannelNumber';
begin
  try
    with GeneralFlowChannelDialog do
    begin
      FErrorMessage := '';
      if (NOT AChannel.Validate(FErrorMessage, 'ChannelNumber')) then
        FAllErrorMessages.Add(Trim(FErrorMessage));
      ChannelNumberEdit.ContextValidationError := FErrorMessage;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrigationBlockReturnFlowChannelValidator.ValidateChannelName (AChannel : IGeneralFlowChannel);
const OPNAME = 'TIrrigationBlockReturnFlowChannelValidator.ValidateChannelName';
begin
  try
    with GeneralFlowChannelDialog do
    begin
      FErrorMessage := '';
      if (NOT AChannel.Validate(FErrorMessage, 'ChannelName')) then
        FAllErrorMessages.Add(Trim(FErrorMessage));
      ChannelNameEdit.ContextValidationError := FErrorMessage;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrigationBlockReturnFlowChannelValidator.ValidateChannelPenalty (AChannel : IGeneralFlowChannel);
const OPNAME = 'TIrrigationBlockReturnFlowChannelValidator.ValidateChannelPenalty';
begin
  try
    with GeneralFlowChannelDialog do
    begin
      FErrorMessage := '';
      if (NOT AChannel.Validate(FErrorMessage, 'ChannelPenalty')) then
        FAllErrorMessages.Add(Trim(FErrorMessage));
      PenaltyStructureEdit.ContextValidationError := FErrorMessage;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrigationBlockReturnFlowChannelValidator.ValidateUpAndDownstreamNodes
                                        (AChannelList : IChannelList;
                                         AChannel     : IGeneralFlowChannel);
const OPNAME = 'TIrrigationBlockReturnFlowChannelValidator.ValidateUpAndDownstreamNodes';
begin
  try
    with GeneralFlowChannelDialog do
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

function TIrrigationBlockReturnFlowChannelValidator.ProcessMetaDataEvent : boolean;
const OPNAME = 'TIrrigationBlockReturnFlowChannelValidator.ProcessMetaDataEvent';
var
  lFieldProperty : TAbstractFieldProperty;
  lKeyValues     : string;
  lFieldIndex    : string;
  lChannel       : IGeneralFlowChannel;
  lIrrigationBlock : IIrrigationBlock;
begin
  Result := FALSE;
  try
    if (FPanel.Visible AND (FActiveControl <> nil) AND (FIdentifier <> 0)) then
    begin
      lChannel := nil;
      lIrrigationBlock := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.IrrigationBlockList.IrrigationBlockByBlockNodeNumber[FIdentifier];
      If (lIrrigationBlock <> nil) then
        lChannel := lIrrigationBlock.ReturnFlowChannel;
      if (lChannel <> nil) then
      begin
        with GeneralFlowChannelDialog do
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
end;

procedure TIrrigationBlockReturnFlowChannelValidator.RepopulateChannelAreas;
const OPNAME = 'TIrrigationBlockReturnFlowChannelValidator.RepopulateChannelAreas';
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
  lIrrigationBlock : IIrrigationBlock;
begin
  try
    lChannel := nil;
    lIrrigationBlock := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.IrrigationBlockList.IrrigationBlockByBlockNodeNumber[FIdentifier];
    If (lIrrigationBlock <> nil) then
      lChannel := lIrrigationBlock.ReturnFlowChannel;
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

        GeneralFlowChannelDialog.ChannelAreaCbx.Items.Assign(LAreas);
        if (lChannel.ChannelArea <> 0) then
        begin
          lChannelAreaName := '';
          lChannelAreaList := TYieldModelDataObject(FAppModules.Model.ModelData).
                              NetworkFeaturesData.ChannelAreaList;
          lChannelAreaData := lChannelAreaList.ChannelAreaByID(lChannel.ChannelArea);

          lFieldIndex    := '';
          lFieldProperty := GeneralFlowChannelDialog.ChannelAreaCbx.FieldProperty;
          lKeyValues     := lChannel.GetKeyValues(lFieldProperty.FieldName, lFieldIndex);
          GeneralFlowChannelDialog.ChannelAreaCbx.HasMetaData :=
            FAppModules.MetaData.FindMetaData(lFieldProperty.FieldName, lKeyValues, lFieldIndex) <> nil;

          if (lChannelAreaData <> nil) then
          begin
            lChannelAreaName := lChannelAreaData.AreaName;
            GeneralFlowChannelDialog.ChannelAreaCbx.ItemIndex :=
            GeneralFlowChannelDialog.ChannelAreaCbx.Items.IndexOf(lChannelAreaName);
          end;
        end;
      finally
        LAreas.Free;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrigationBlockReturnFlowChannelValidator.UpdateChannelArea;
const OPNAME = 'TIrrigationBlockReturnFlowChannelValidator.UpdateChannelArea';
var
  lChannel         : IGeneralFlowChannel;
  lChannelArea     : IChannelArea;
  lChannelAreaList : IChannelAreaList;
  lMessage         : string;
  lIrrigationBlock : IIrrigationBlock;
begin
  try
    lChannel := nil;
    lIrrigationBlock := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.IrrigationBlockList.IrrigationBlockByBlockNodeNumber[FIdentifier];
    If (lIrrigationBlock <> nil) then
      lChannel := lIrrigationBlock.ReturnFlowChannel;
    if (lChannel <> nil) then
    begin
      with GeneralFlowChannelDialog do
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
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrigationBlockReturnFlowChannelValidator.UpdateIrrigationBlockReturnFlowFactor;
const OPNAME = 'TIrrigationBlockReturnFlowChannelValidator.UpdateIrrigationBlockReturnFlowFactor';
var
  lIrrigationBlock  : IIrrigationBlock;
  LMessage          : string;
begin
  try
    lIrrigationBlock := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.
                                                IrrigationBlockList.IrrigationBlockByBlockNodeNumber[FIdentifier];
    if (lIrrigationBlock <> nil) then
    begin
      with GeneralFlowChannelDialog do
      begin
        ReturnFlowFactorEdit.FieldValidationError := '';
        if FAppModules.FieldProperties.ValidateFieldProperty(
                ReturnFlowFactorEdit.FieldProperty.FieldName,
                ReturnFlowFactorEdit.Text,
                LMessage) then
        begin
          lIrrigationBlock.ReturnFlowFactor := StrToFloat(ReturnFlowFactorEdit.Text);
          RePopulateDataViewer;
          DoContextValidation (dvtIrrigationBlockReturnFlowFactor);
        end
        else
          ReturnFlowFactorEdit.FieldValidationError := LMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrigationBlockReturnFlowChannelValidator.UpdateIrrigationBlockReturnFlowLoss;
const OPNAME = 'TIrrigationBlockReturnFlowChannelValidator.UpdateIrrigationBlockReturnFlowLoss';
var
  lIrrigationBlock : IIrrigationBlock;
  LMessage          : string;
begin
  try
    lIrrigationBlock := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.
                                        IrrigationBlockList.IrrigationBlockByBlockNodeNumber[FIdentifier];
    if (lIrrigationBlock <> nil) then
    begin
      with GeneralFlowChannelDialog do
      begin
        ReturnFlowLossEdit.FieldValidationError := '';
        if FAppModules.FieldProperties.ValidateFieldProperty(
                ReturnFlowLossEdit.FieldProperty.FieldName,
                ReturnFlowLossEdit.Text,
                LMessage) then
        begin
          lIrrigationBlock.ReturnFlowLoss := StrToFloat(ReturnFlowLossEdit.Text);
          RePopulateDataViewer;
          DoContextValidation (dvtReturnFlowLoss);
        end
        else
          ReturnFlowLossEdit.FieldValidationError := LMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrigationBlockReturnFlowChannelValidator.ValidateIrrigationBlockReturnFlowFactor;
const OPNAME = 'TIrrigationBlockReturnFlowChannelValidator.ValidateIrrigationBlockReturnFlowFactor';
var
  lIrrigationBlock  : IIrrigationBlock;
  LMessage          : WideString;
begin
  try
    lIrrigationBlock := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.
                                    IrrigationBlockList.IrrigationBlockByBlockNodeNumber[FIdentifier];
    if (lIrrigationBlock <> nil) then
    begin
      with GeneralFlowChannelDialog do
      begin
        ReturnFlowFactorEdit.ContextValidationError := '';
        if not lIrrigationBlock.Validate(LMessage,ReturnFlowFactorEdit.FieldProperty.FieldName) then
          ReturnFlowFactorEdit.ContextValidationError := LMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrigationBlockReturnFlowChannelValidator.ValidateIrrigationBlockReturnFlowLoss;
const OPNAME = 'TIrrigationBlockReturnFlowChannelValidator.ValidateIrrigationBlockReturnFlowLoss';
var
  lIrrigationBlock : IIrrigationBlock;
  LMessage          : WideString;
begin
  try
    lIrrigationBlock := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.
                                      IrrigationBlockList.IrrigationBlockByBlockNodeNumber[FIdentifier];
    if (lIrrigationBlock <> nil) then
    begin
      with GeneralFlowChannelDialog do
      begin
        ReturnFlowLossEdit.ContextValidationError := '';
        if (not lIrrigationBlock.Validate(LMessage, ReturnFlowLossEdit.FieldProperty.FieldName)) then
          ReturnFlowLossEdit.ContextValidationError := LMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.

