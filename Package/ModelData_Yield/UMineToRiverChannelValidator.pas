{******************************************************************************}
{*  UNIT      : Contains the class TMineToRiverChannelValidator.              *}
{*  AUTHOR    : Presley Mudau                                                 *}
{*  DATE      : 2007/03/14                                                    *}
{*  COPYRIGHT : Copyright © 2007 DWAF                                         *}
{******************************************************************************}

unit UMineToRiverChannelValidator;

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
  TMineToRiverChannelValidator = class(TAbstractYieldDataDialogValidator)
  private
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
    procedure RepopulateChannelAreas;
    procedure RePopulateNodes;
    procedure UpdateChannelName;
    procedure UpdateChannelPenaltyStructure; virtual;
    procedure UpdateChannelArea;
    procedure ValidateChannelName (AChannel : IGeneralFlowChannel);
    procedure ValidateChannelNumber (AChannel : IGeneralFlowChannel);
    procedure ValidateChannelPenalty (AChannel : IGeneralFlowChannel);
    procedure RePopulateDataViewer;
    procedure ShowChannelPenaltyDialog (ASender : TObject);
  public
    function Initialise: boolean; override;
    function SaveState: boolean; override;
    function LanguageHasChanged: boolean; override;
    function StudyHasChanged: boolean; override;
    function StudyDataHasChanged(AContext: TChangeContext; AFieldName,AOldValue,ANewValue: string): boolean; override;
    procedure ClearDataViewer; override;
    procedure PopulateDataViewer; override;
    procedure DoContextValidation (AValidationType : TDialogValidationType); override;
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
  UMiningData,
  USelectChannelDialog, UNetworkElementData, UNetworkFeaturesData;

{******************************************************************************}
{* TMineToRiverChannelValidator                                               *}
{******************************************************************************}

procedure TMineToRiverChannelValidator.CreateMemberObjects;
const OPNAME = 'TMineToRiverChannelValidator.CreateMemberObjects';
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
      DownStreamNodeCbx.OnExit           := OnEditControltExit;

      ChannelAreaCbx.FieldProperty        := FAppModules.FieldProperties.FieldProperty('ChannelAreaName');
      ChannelAreaCbx.OnEnter              := OnEditControlEnter;
      ChannelAreaCbx.OnExit               := OnEditControltExit;

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

procedure TMineToRiverChannelValidator.CreateDialog;
const OPNAME = 'TMineToRiverChannelValidator.CreateDialog';
begin
  try
    FPanel  := TMineChannelPropertiesDialog.Create(FPanelOwner,FAppModules);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMineToRiverChannelValidator.DestroyMemberObjects;
const OPNAME = 'TMineToRiverChannelValidator.DestroyMemberObjects';
begin
  try
    inherited DestroyMemberObjects;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMineToRiverChannelValidator.Initialise: boolean;
const OPNAME = 'TMineToRiverChannelValidator.Initialise';
begin
  Result := inherited Initialise;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMineToRiverChannelValidator.LanguageHasChanged: boolean;
const OPNAME = 'TMineToRiverChannelValidator.LanguageHasChanged';
begin
  Result := False;
  try
    TabShetCaption := FAppModules.Language.GetString('TabCaption.MineToRiverTabSheet');
    Result := inherited LanguageHasChanged;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMineToRiverChannelValidator.ClearDataViewer;
const OPNAME = 'TMineToRiverChannelValidator.ClearDataViewer';
var
  lpPanel     : TMineChannelPropertiesDialog;
begin
  inherited ClearDataViewer;
  try
    lpPanel := MineChannelPropertiesDialog;
    with lpPanel do
    begin
      ChannelNameEdit.Text        := '';
      ChannelNumberEdit.Text      := '-1';
      UpStreamNodeEdit.Text       := '-1';
      DownStreamNodeCbx.ItemIndex := -1;
      PenaltyStructureEdit.Text   := '-1';
      ChannelAreaCbx.ItemIndex    := -1;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMineToRiverChannelValidator.PopulateDataViewer;
const OPNAME = 'TMineToRiverChannelValidator.PopulateDataViewer';
begin
  inherited PopulateDataViewer;
  try
    ClearDataViewer;
    RePopulateDataViewer;
    DoContextValidation(dvtChanPropAll);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMineToRiverChannelValidator.RePopulateDataViewer;
const OPNAME = 'TMineToRiverChannelValidator.RePopulateDataViewer';
var
  lRiverChannelNr : integer;
  lMine           : IMine;
  lPenalty        : IChannelPenalty;
  lChannel        : IGeneralFlowChannel;
begin
  try
    if (FIdentifier >= 0) then
    begin
      lMine := TYieldModelDataObject(FAppModules.Model.ModelData).
                    CastNetworkFeaturesData.CastMineList.MineByID[FIdentifier];
      if (lMine <> nil) then
      begin
        with MineChannelPropertiesDialog do
        begin
          lRiverChannelNr :=  lMine.RiverChannelNumber;
          lChannel := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData
                      .ChannelList.ChannelByChannelNumber[lRiverChannelNr];
          if lChannel <> nil then
          begin
            ChannelNumberEdit.SetFieldValue(lChannel.ChannelNumber);
            ChannelNameEdit.SetFieldValue(lChannel.ChannelName);
            UpStreamNodeEdit.SetFieldValue(lChannel.UpStreamNodeNumber);

            lPenalty := lChannel.ChannelPenalty;
            if (lPenalty <> nil) then
              PenaltyStructureEdit.SetFieldValue(lPenalty.ChannelPenaltyID)
            else
              PenaltyStructureEdit.SetFieldValue(0);
          end;
          RepopulateChannelAreas;
          RePopulateNodes;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMineToRiverChannelValidator.SaveState: boolean;
const OPNAME = 'TMineToRiverChannelValidator.SaveState';
begin
  Result := inherited SaveState;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMineToRiverChannelValidator.MineChannelPropertiesDialog:TMineChannelPropertiesDialog;
const OPNAME = 'TMineToRiverChannelValidator.MineChannelPropertiesDialog';
begin
  Result := Nil;
  try
    if (FPanel <> nil) then
      Result := TMineChannelPropertiesDialog(FPanel);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMineToRiverChannelValidator.StudyDataHasChanged(AContext: TChangeContext; AFieldName,AOldValue,ANewValue: string): boolean;
const OPNAME = 'TMineToRiverChannelValidator.StudyDataHasChanged';
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

function TMineToRiverChannelValidator.StudyHasChanged: boolean;
const OPNAME = 'TMineToRiverChannelValidator.StudyHasChanged';
begin
  Result := inherited StudyHasChanged;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMineToRiverChannelValidator.OnSelectPenaltyClick(Sender: TObject);
const OPNAME = 'TMineToRiverChannelValidator.OnSelectPenaltyClick';
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

procedure TMineToRiverChannelValidator.ShowChannelPenaltyDialog(ASender: TObject);
const OPNAME = 'TMineToRiverChannelValidator.ShowChannelPenaltyDialog';
begin
  try
    if (FChannelPenaltyValidator <> nil) then
      FChannelPenaltyValidator.ChannelPenaltyDialog.Resize;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMineToRiverChannelValidator.OnEditControlEnter(Sender: TObject);
const OPNAME = 'TMineToRiverChannelValidator.OnEditControlEnter';
begin
  inherited OnEditControlEnter(Sender);
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMineToRiverChannelValidator.OnEditControltExit(Sender: TObject);
const OPNAME = 'TMineToRiverChannelValidator.OnEditControltExit';
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
      if ((Sender = ChannelAreaCbx) AND (ChannelAreaCbx.HasValueChanged)) then
        UpdateChannelArea;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{procedure TMineToRiverChannelValidator.UpdateChannelName;
const OPNAME = 'TMineToRiverChannelValidator.UpdateChannelName';
{var
  LMine    :   IMine;
  lMessage : string;
begin
  try
    LMine := TYieldModelDataObject(FAppModules.Model.ModelData).
                  CastNetworkFeaturesData.CastMineList.MineByID[FIdentifier];
    if (LMine <> nil) then
    begin
      with MineChannelPropertiesDialog do
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
end; }


procedure TMineToRiverChannelValidator.UpdateChannelPenaltyStructure;
const OPNAME = 'TMineToRiverChannelValidator.UpdateChannelPenaltyStructure';
var
  lMessage          : string;
  lPenaltyID        : integer;
  LChannelNr        : integer;
  LMine             : IMine;
  lPenaltyTypeList  : IChannelPenaltyList;
  lPenaltyStructure : IChannelPenalty;
  lChannel          : IGeneralFlowChannel;
begin
  try
    LMine := TYieldModelDataObject(FAppModules.Model.ModelData).CastNetworkFeaturesData
             .CastMineList.MineByID[FIdentifier];
    if (LMine <> nil) then
    begin
      LChannelNr := LMine.RiverChannelNumber;
      lChannel := TYieldModelDataObject(FAppModules.Model.ModelData).
                    NetworkElementData.ChannelList.ChannelByChannelNumber[LChannelNr];
      if (lChannel <> nil) then
      begin
        with MineChannelPropertiesDialog do
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
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMineToRiverChannelValidator.UpdateChannelArea;
const OPNAME = 'TMineToRiverChannelValidator.UpdateChannelArea';
var
  LMine            : IMine;
  LChannelNr       : integer;
  lChannel         : IGeneralFlowChannel;
  lChannelArea     : IChannelArea;
  lChannelAreaList : IChannelAreaList;
  lMessage         : string;
begin
  try
    LMine := TYieldModelDataObject(FAppModules.Model.ModelData).CastNetworkFeaturesData
             .CastMineList.MineByID[FIdentifier];
    if (LMine <> nil) then
    begin
      LChannelNr := LMine.RiverChannelNumber;
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

procedure TMineToRiverChannelValidator.DoContextValidation(AValidationType : TDialogValidationType);
const OPNAME = 'TMineToRiverChannelValidator.DoContextValidation';
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


{procedure TMineToRiverChannelValidator.RepopulateChannelAreas;
const OPNAME = 'TMineToRiverChannelValidator.RepopulateChannelAreas';
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
begin
  try
    lChannel := TYieldModelDataObject(FAppModules.Model.ModelData).
                NetworkElementData.ChannelList.ChannelByChannelNumber[FIdentifier];
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
  except on E: Exception do HandleError(E, OPNAME) end;
end;
}


procedure TMineToRiverChannelValidator.ValidateChannelName(AChannel: IGeneralFlowChannel);
const OPNAME = 'TMineToRiverChannelValidator.ValidateChannelName';
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

procedure TMineToRiverChannelValidator.ValidateChannelPenalty(AChannel: IGeneralFlowChannel);
const OPNAME = 'TMineToRiverChannelValidator.ValidateChannelPenalty';
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

procedure TMineToRiverChannelValidator.ValidateChannelNumber(AChannel: IGeneralFlowChannel);
const OPNAME = 'TMineToRiverChannelValidator.ValidateChannelNumber';
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

procedure TMineToRiverChannelValidator.RepopulateChannelAreas;
const OPNAME = 'TMineToRiverChannelValidator.RepopulateChannelAreas';
var
  LAreas           : TStringList;
  LIndex           : integer;
  lChannelAreaName : string;
  LMine            : IMine;
  lChannelNr       : integer;
  lChannelAreaList : IChannelAreaList;
  lChannelAreaData : IChannelArea;
  lChannel         : IGeneralFlowChannel;
begin
  try
    LMine := TYieldModelDataObject(FAppModules.Model.ModelData).CastNetworkFeaturesData
             .CastMineList.MineByID[FIdentifier];
    lChannelNr := LMine.RiverChannelNumber;

    lChannel := TYieldModelDataObject(FAppModules.Model.ModelData).
                NetworkElementData.ChannelList.ChannelByChannelNumber[lChannelNr];
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
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMineToRiverChannelValidator.RePopulateNodes;
const OPNAME = 'TMineToRiverChannelValidator.RePopulateNodes';
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

procedure TMineToRiverChannelValidator.UpdateChannelName;
const OPNAME = 'TMineToRiverChannelValidator.UpdateChannelName';
var
  LMine      : IMine;
  LChannelNr : integer;
  lChannel   : IGeneralFlowChannel;
  lMessage   : string;
begin
  try
    LMine := TYieldModelDataObject(FAppModules.Model.ModelData).CastNetworkFeaturesData
             .CastMineList.MineByID[FIdentifier];
    if (LMine <> nil) then
    begin
      LChannelNr := LMine.RiverChannelNumber;
      lChannel := TYieldModelDataObject(FAppModules.Model.ModelData).
                    NetworkElementData.ChannelList.ChannelByChannelNumber[LChannelNr];

      if (lChannel <> nil) then
      begin
        with MineChannelPropertiesDialog do
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
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;



end.

