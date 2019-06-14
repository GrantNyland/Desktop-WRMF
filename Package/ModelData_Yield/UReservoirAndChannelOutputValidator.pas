{******************************************************************************}
{*  UNIT      : Contains the class TReservoirAndChannelsOutputValidator       *}
{*  AUTHOR    : Presley Mudau                                                 *}
{*  DATE      : 2004/12/23                                                    *}
{*  COPYRIGHT : Copyright © 2003 DWAF                                         *}
{******************************************************************************}

unit UReservoirAndChannelOutputValidator;

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
  UAbstractYieldDataDialogValidator,
  UReservoirAndChannelOutputDialog;

type
  TReservoirAndChannelOutputValidator = class(TAbstractYieldDataDialogValidator)
  private
    FReservoirSummaryCount : integer;
    FChannelSummaryCount   : integer;
    FActiveReservoirCount  : integer;
    FChannelAnalysisCount  : integer;
  protected
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    procedure OnEditControlClick(Sender: TObject);
    procedure OnEditControlEnter(Sender: TObject); override;
    procedure OnEditControltExit(Sender: TObject); override;
    procedure ChannelsTreeviewMouseDown(Sender : TObject;
                                        Button : TMouseButton;
                                        Shift  : TShiftState;
                                        X,  Y  : Integer);
    procedure ReservoirsTreeviewMouseDown(Sender : TObject;
                                          Button : TMouseButton;
                                          Shift  : TShiftState;
                                          X,  Y  : Integer);
    procedure RePopulateDataViewer;
    procedure DisplayChannelSummaryRequired (ANode    : TTreeNode;
                                             AChannel : IGeneralFlowChannel);
    procedure DisplayChannelAnalysisRequired (ANode   : TTreeNode;
                                             AChannel : IGeneralFlowChannel);
    procedure DisplayReservoirSummaryRequired (ANode : TTreeNode);
    procedure DisplayReservoirActive (ANode : TTreeNode);
    procedure UpdateChannelSummaryRequired (ANode     : TTreeNode;
                                            ARequired : Boolean);
    procedure UpdateChannelAnalysisRequired (ANode     : TTreeNode;
                                             ARequired : Boolean);
    procedure UpdateReservoirSummaryRequired (ANode      : TTreeNode;
                                              ARequired  : Boolean);
    procedure UpdateReservoirActive (ANode   : TTreeNode;
                                     AActive : Boolean);
  public
    function Initialise: boolean; override;
    function SaveState: boolean; override;
    function LanguageHasChanged: boolean; override;
    function StudyHasChanged: boolean; override;
    function StudyDataHasChanged(AContext: TChangeContext; AFieldName,AOldValue,ANewValue: string): boolean; override;
    procedure ClearDataViewer; override;
    procedure PopulateDataViewer; override;
    function ReservoirAndChannelOutputDialog : TReservoirAndChannelOutputDialog;
  end;

implementation

uses
  SysUtils,
  VCL.Graphics,
  URunConfigurationData,
  UYieldModelDataGUIForm,
  UYieldModelDataObject,
  UErrorHandlingOperations;

{******************************************************************************}
{* TReservoirAndChannelOutputValidator                                              *}
{******************************************************************************}

procedure TReservoirAndChannelOutputValidator.CreateMemberObjects;
const OPNAME = 'TReservoirAndChannelOutputValidator.CreateMemberObjects';
var
  lPanel     : TReservoirAndChannelOutputDialog;
  lIndex     : integer;
  lComponent : TComponent;
  lFieldEdit : TFieldEdit;
  lFieldCbx  : TFieldComboBox;
  lRadioGrp  : TFieldRadioGroup;
  lParent    : TControl;
  lChkBox    : TFieldChkBox;
begin
  try
    inherited CreateMemberObjects;
    FPanel  := TReservoirAndChannelOutputDialog.Create(FPanelOwner,FAppModules);
    lPanel := ReservoirAndChannelOutputDialog;
    with lPanel do
    begin
      ChannelsTreeview.OnMouseDown         := ChannelsTreeviewMouseDown;
      ReservoirsTreeview.OnMouseDown       := ReservoirsTreeviewMouseDown;
    end;
    lParent := lPanel.ControlsParent;
    for lIndex := 0 to lParent.ComponentCount - 1 do
    begin
      lComponent := lParent.Components[lIndex];
      if (lComponent.ClassNameIs('TFieldEdit')) then
      begin
        lFieldEdit := TFieldEdit(lComponent);
        lFieldEdit.OnEnter := OnEditControlEnter;
        lFieldEdit.OnExit  := OnEditControltExit;
      end
      else if (lComponent.ClassNameIs('TFieldComboBox')) then
      begin
        lFieldCbx := TFieldComboBox(lComponent);
        lFieldCbx.OnEnter := OnEditControlEnter;
        lFieldCbx.OnExit  := OnEditControltExit;
      end
      else if (lComponent.ClassNameIs('TFieldChkBox')) then
      begin
        lChkBox := TFieldChkBox(lComponent);
        lChkBox.OnEnter := OnEditControlEnter;
        lChkBox.OnExit  := OnEditControltExit;
      end
      else if (lComponent.ClassNameIs('TFieldRadioGroup')) then
      begin
        lRadioGrp := TFieldRadioGroup(lComponent);
        lRadioGrp.OnEnter := OnEditControlEnter;
        lRadioGrp.OnExit  := OnEditControltExit;
      end;
    end;

    ReservoirAndChannelOutputDialog.NrOfResInSummaryEdit.FieldProperty := FAppModules.FieldProperties.FieldProperty('NrOfResInSummary');

    ReservoirAndChannelOutputDialog.NrOfActiveReservoirsEdit.FieldProperty := FAppModules.FieldProperties.FieldProperty('NrOfActiveReservoirs');
    ReservoirAndChannelOutputDialog.NrOfActiveReservoirsEdit.Color := clAqua;

    ReservoirAndChannelOutputDialog.NrOfChanInSummaryEdit.FieldProperty := FAppModules.FieldProperties.FieldProperty('NrOfChanInSummary');

    ReservoirAndChannelOutputDialog.NrOfChanInFirmYieldEdit.FieldProperty := FAppModules.FieldProperties.FieldProperty('NrOfChanInFirmYield');
    ReservoirAndChannelOutputDialog.NrOfChanInFirmYieldEdit.Color  := clYellow;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirAndChannelOutputValidator.DestroyMemberObjects;
const OPNAME = 'TReservoirAndChannelOutputValidator.DestroyMemberObjects';
begin
  try
    inherited DestroyMemberObjects;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirAndChannelOutputValidator.Initialise: boolean;
const OPNAME = 'TReservoirAndChannelOutputValidator.Initialise';
begin
  Result := inherited Initialise;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirAndChannelOutputValidator.LanguageHasChanged: boolean;
const OPNAME = 'TReservoirAndChannelOutputValidator.LanguageHasChanged';
begin
  Result := False;
  try
    TabShetCaption := FAppModules.Language.GetString('ViewData.ReservoirAndChannel');
    Result := inherited LanguageHasChanged;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirAndChannelOutputValidator.ClearDataViewer;
const OPNAME = 'TReservoirAndChannelOutputValidator.ClearDataViewer';
var
  lIndex     : integer;
  lComponent : TComponent;
  lFieldEdit : TFieldEdit;
  lFieldCbx  : TFieldComboBox;
  lChkBox    : TFieldChkBox;
  lRadioGrp  : TFieldRadioGroup;
begin
  inherited ClearDataViewer;
  try
    with ReservoirAndChannelOutputDialog do
    begin
      ChannelsTreeview.Items.Clear;
      ReservoirsTreeview.Items.Clear;
      FReservoirSummaryCount := 0;
      FChannelSummaryCount   := 0;
      FActiveReservoirCount  := 0;
      FChannelAnalysisCount  := 0;
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
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirAndChannelOutputValidator.PopulateDataViewer;
const OPNAME = 'TReservoirAndChannelOutputValidator.PopulateDataViewer';
begin
  inherited PopulateDataViewer;
  try
    ClearDataViewer;
    RePopulateDataViewer;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirAndChannelOutputValidator.RePopulateDataViewer;
const OPNAME = 'TReservoirAndChannelOutputValidator.RePopulateDataViewer';
var
  lReservoirList : IReservoirDataList;
  lChannelList   : IChannelList;
  lChannel       : IGeneralFlowChannel;
  lReservoir     : IReservoirConfigurationData;
  lIndexA        : integer;
  lRootNode      : TTreeNode;
  lNode          : TTreeNode;
  lsName         : string;
begin
  try
    lReservoirList := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.ReservoirList;
    if (lReservoirList <> nil) then
    begin
      with ReservoirAndChannelOutputDialog do
      begin
        ReservoirsTreeview.Indent := 22;
        lRootNode := ReservoirsTreeview.Items.Add(nil, FAppModules.Language.GetString('ViewData.ReservoirHeading'));
        lRootNode.ImageIndex    := 4;
        lRootNode.SelectedIndex := 4;
        lRootNode.StateIndex    := -1;
        for lIndexA := 0 to lReservoirList.ReservoirCount - 1 do
        begin
          lReservoir := lReservoirList.ReservoirByIndex[lIndexA].ReservoirConfigurationData;
          lsName := '(' + IntToStr(lReservoir.ReservoirIdentifier) + ') ' + lReservoir.ReservoirName;
          lNode := ReservoirsTreeview.Items.AddChildObject(lRootNode, lsName, TObject(lReservoir.ReservoirIdentifier));

          lNode.StateIndex    := -1;
          lNode.ImageIndex    := -1;
          lNode.SelectedIndex := -1;
          DisplayReservoirSummaryRequired(lNode);
          DisplayReservoirActive(lNode);
        end;
        for lIndexA := 0 to lReservoirList.NodesWithInflowCount - 1 do
        begin
          lReservoir := lReservoirList.NodeWithInflowByIndex[lIndexA].ReservoirConfigurationData;
          lsName := '(' + IntToStr(lReservoir.ReservoirIdentifier) + ') ' + lReservoir.ReservoirName;
          lNode := ReservoirsTreeview.Items.AddChildObject(lRootNode, lsName, TObject(lReservoir.ReservoirIdentifier));
          lNode.StateIndex    := -1;
          lNode.ImageIndex    := 5;
          lNode.SelectedIndex := 5;
          DisplayReservoirSummaryRequired(lNode);
          DisplayReservoirActive(lNode);
        end;
        ReservoirsTreeview.Items[0].Expand(TRUE);
        NrOfResInSummaryEdit.SetFieldValue(FReservoirSummaryCount);
        NrOfActiveReservoirsEdit.SetFieldValue(FActiveReservoirCount);
      end;
    end;

    lChannelList := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.ChannelList;

    if (lChannelList <> nil) then
    begin
      with ReservoirAndChannelOutputDialog do
      begin
        ChannelsTreeview.Indent := 22;
        lRootNode := ChannelsTreeview.Items.Add(nil, FAppModules.Language.GetString('ViewData.ChannelHeading'));
        lRootNode.ImageIndex    := 4;
        lRootNode.SelectedIndex := 4;
        lRootNode.StateIndex    := -1;
        for lIndexA := 0 to lChannelList.ChannelCount - 1 do
        begin
          lChannel := lChannelList.ChannelByIndex[lIndexA];
          lsName := '(' + IntToStr(lChannel.ChannelNumber) + ') ' + lChannel.ChannelName;
          lNode  := ChannelsTreeview.Items.AddChildObject(lRootNode, lsName, TObject(lChannel.ChannelNumber));

          if(lChannel.ChannelType in [ctMinimumFlowChannel,ctMinMaxChannel,ctDemandChannel,ctIrrigationBlockInflowChannel,ctIFRChannel,ctDemandCentreReturnFlowChannel]) then
             lNode.StateIndex := -1;

          lNode.StateIndex    := -1;
          lNode.ImageIndex    := -1;
          lNode.SelectedIndex := -1;
          DisplayChannelSummaryRequired(lNode, lChannel);
          DisplayChannelAnalysisRequired(lNode, lChannel);
        end;
        ChannelsTreeview.Items[0].Expand(TRUE);
        NrOfChanInSummaryEdit.SetFieldValue(FChannelSummaryCount);
        NrOfChanInFirmYieldEdit.SetFieldValue(FChannelAnalysisCount);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirAndChannelOutputValidator.SaveState: boolean;
const OPNAME = 'TReservoirAndChannelOutputValidator.SaveState';
begin
  Result := inherited SaveState;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirAndChannelOutputValidator.ReservoirAndChannelOutputDialog : TReservoirAndChannelOutputDialog;
const OPNAME = 'TReservoirAndChannelOutputValidator.ReservoirAndChannelOutputDialog';
begin
  Result := Nil;
  try
    if (FPanel <> nil) then
      Result := TReservoirAndChannelOutputDialog(FPanel);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirAndChannelOutputValidator.StudyDataHasChanged(AContext: TChangeContext; AFieldName,AOldValue,ANewValue: string): boolean;
const OPNAME = 'TReservoirAndChannelOutputValidator.StudyDataHasChanged';
begin
  Result := inherited StudyDataHasChanged(AContext,AFieldName,AOldValue,ANewValue);
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirAndChannelOutputValidator.StudyHasChanged: boolean;
const OPNAME = 'TReservoirAndChannelOutputValidator.StudyHasChanged';
begin
  Result := inherited StudyHasChanged;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirAndChannelOutputValidator.OnEditControlEnter(Sender: TObject);
const OPNAME = 'TReservoirAndChannelOutputValidator.OnEditControlEnter';
begin
  inherited OnEditControlEnter(Sender);
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirAndChannelOutputValidator.OnEditControltExit(Sender: TObject);
const OPNAME = 'TReservoirAndChannelOutputValidator.OnEditControltExit';
begin
  inherited OnEditControltExit(Sender);
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirAndChannelOutputValidator.ChannelsTreeviewMouseDown(Sender : TObject;
                                                                  Button : TMouseButton;
                                                                  Shift  : TShiftState;
                                                                  X,  Y  : Integer);
const OPNAME = 'TReservoirAndChannelOutputValidator.ChannelsTreeviewMouseDown';
var
  lNode        : TTreeNode;
  lHit         : THitTests;
  lChannelList : IChannelList;
  lChannelNr   : integer;
  lChannel     : IGeneralFlowChannel;
begin
  try
    lChannelList := TYieldModelDataObject(FAppModules.Model.ModelData).
                      NetworkElementData.ChannelList;
    with ReservoirAndChannelOutputDialog do
    begin
      lNode := ChannelsTreeview.GetNodeAt(X,Y);
      if (lNode <> nil) then
      begin
        lChannelNr := Integer(lNode.Data);
        lChannel := lChannelList.ChannelByChannelNumber[lChannelNr];
        if (lNode.Level = 1) then
        begin
          lHit  := ChannelsTreeview.GetHitTestInfoAt(X, Y);
          if htOnIcon in lHit then
          begin
            case lNode.ImageIndex of
              0 : UpdateChannelAnalysisRequired(lNode, TRUE);  {unchecked -> checked}
              1 : UpdateChannelAnalysisRequired(lNode, FALSE); {checked -> unchecked}
            end;
            DisplayChannelAnalysisRequired(lNode, lChannel);
            NrOfChanInFirmYieldEdit.SetFieldValue(FChannelAnalysisCount);
          end
          else if htOnStateIcon in lHit then
          begin
            case lNode.StateIndex of
              2 : UpdateChannelSummaryRequired(lNode, TRUE);  {unchecked -> checked}
              3 : UpdateChannelSummaryRequired(lNode, FALSE); {checked -> unchecked}
            end;
            DisplayChannelSummaryRequired(lNode, lChannel);
            if(FAppModules.Model.ModelName = CYield) then
              DisplayChannelAnalysisRequired(lNode, lChannel);
            NrOfChanInSummaryEdit.SetFieldValue(FChannelSummaryCount);
            NrOfChanInFirmYieldEdit.SetFieldValue(FChannelAnalysisCount);
          end;
          ChannelsTreeview.Repaint;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirAndChannelOutputValidator.ReservoirsTreeviewMouseDown(Sender : TObject;
                                                                    Button : TMouseButton;
                                                                    Shift  : TShiftState;
                                                                    X,  Y  : Integer);
const OPNAME = 'TReservoirAndChannelOutputValidator.ReservoirsTreeviewMouseDown';
var
  lNode : TTreeNode;
  lHit  : THitTests;
begin
  try
    with ReservoirAndChannelOutputDialog do
    begin
      lNode := ReservoirsTreeview.GetNodeAt(X,Y);
      if (lNode <> nil) then
      begin
        if (lNode.Level = 1) then
        begin
          lHit  := ReservoirsTreeview.GetHitTestInfoAt(X, Y);
          if htOnIcon in lHit then
          begin
            case lNode.ImageIndex of
              0 : UpdateReservoirActive(lNode, TRUE);  {unchecked -> checked}
              1 : UpdateReservoirActive(lNode, FALSE); {checked -> unchecked}
            end;
            DisplayReservoirActive(lNode);
            NrOfActiveReservoirsEdit.SetFieldValue(FActiveReservoirCount);
          end
          else if htOnStateIcon in lHit then
          begin
            case lNode.StateIndex of
              2 : UpdateReservoirSummaryRequired(lNode, TRUE);  {unchecked -> checked}
              3 : UpdateReservoirSummaryRequired(lNode, FALSE); {checked -> unchecked}
            end;
            DisplayReservoirSummaryRequired(lNode);
            NrOfResInSummaryEdit.SetFieldValue(FReservoirSummaryCount);
          end;
          ReservoirsTreeview.Repaint;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirAndChannelOutputValidator.DisplayChannelSummaryRequired (ANode    : TTreeNode;
                                                                       AChannel : IGeneralFlowChannel);
const OPNAME = 'TReservoirAndChannelOutputValidator.DisplayChannelSummaryRequired';
begin
  try
    if (AChannel.ChannelType = ctMasterControlChannel) then
      // Master control channel always included in firm yield analysis
    begin
      if (ANode.StateIndex <> 3) then
        FChannelSummaryCount := FChannelSummaryCount + 1;
      ANode.StateIndex := 3;
    end
    else
    begin
      if (AChannel.SummaryOutputRequired = 'Y') then
      begin
        if (ANode.StateIndex <> 3) then
          FChannelSummaryCount := FChannelSummaryCount + 1;
        ANode.StateIndex := 3;
      end
      else
      begin
        if (ANode.StateIndex = 3) then
          FChannelSummaryCount := FChannelSummaryCount - 1;
        ANode.StateIndex := 2;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirAndChannelOutputValidator.DisplayChannelAnalysisRequired (ANode    : TTreeNode;
                                                                        AChannel : IGeneralFlowChannel);
const OPNAME = 'TReservoirAndChannelOutputValidator.DisplayChannelAnalysisRequired';
begin
  try
    case AChannel.ChannelType of
    ctMasterControlChannel : // Master control channel always included in firm yield analysis
      begin
        if (ANode.ImageIndex <> 1) then
          FChannelAnalysisCount := FChannelAnalysisCount + 1;
        ANode.ImageIndex    := 1;
        ANode.SelectedIndex := 1;
      end;
    ctMinimumFlowChannel,ctMinMaxChannel,ctDemandChannel,ctIrrigationBlockInflowChannel,ctIFRChannel,ctDemandCentreReturnFlowChannel : // Minimum flow and Min-max channels may be included in firm yield analysis
      begin
        if (FAppModules.Model.ModelName = CYield) then
        begin
          if (AChannel.SummaryOutputRequired = 'Y') then
          begin
            if (AChannel.RequiresFirmYieldAnalysis = 'Y') then
            begin
              if (ANode.ImageIndex <> 1) then
                FChannelAnalysisCount := FChannelAnalysisCount + 1;
              ANode.ImageIndex    := 1;
              ANode.SelectedIndex := 1;
            end
            else
            begin
              if (ANode.ImageIndex = 1) then
                FChannelAnalysisCount := FChannelAnalysisCount - 1;
              ANode.ImageIndex    := 0;
              ANode.SelectedIndex := 0;
            end;
          end
          else
          begin
            if (ANode.ImageIndex = 1) then
              FChannelAnalysisCount := FChannelAnalysisCount - 1;
            ANode.ImageIndex    := 5;
            ANode.SelectedIndex := 5;
          end;
        end
        else
        begin
          if (AChannel.RequiresFirmYieldAnalysis = 'Y') then
          begin
            if (ANode.ImageIndex <> 1) then
              FChannelAnalysisCount := FChannelAnalysisCount + 1;
            ANode.ImageIndex    := 1;
            ANode.SelectedIndex := 1;
          end
          else
          begin
            if (ANode.ImageIndex = 1) then
              FChannelAnalysisCount := FChannelAnalysisCount - 1;
            ANode.ImageIndex    := 0;
            ANode.SelectedIndex := 0;
          end;
        end;
      end;
    else // All other channels are never included in firm yield analysis
      begin
        ANode.ImageIndex      := 5;
        ANode.SelectedIndex   := 5;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirAndChannelOutputValidator.DisplayReservoirSummaryRequired (ANode : TTreeNode);
const OPNAME = 'TReservoirAndChannelOutputValidator.DisplayReservoirSummaryRequired';
var
  lReservoirNr : integer;
  lReservoir   : IReservoirConfigurationData;
begin
  try
    lReservoirNr := Integer(ANode.Data);
    lReservoir   := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.
                      ReservoirList.ReservoirOrNodeByIdentifier[lReservoirNr].ReservoirConfigurationData;
    if (lReservoir.IncludeSummary = 'Y') then
    begin
      if (ANode.StateIndex <> 3) then
        FReservoirSummaryCount := FReservoirSummaryCount + 1;
      ANode.StateIndex := 3;
    end
    else
    begin
      if (ANode.StateIndex = 3) then
        FReservoirSummaryCount := FReservoirSummaryCount - 1;
      ANode.StateIndex := 2;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirAndChannelOutputValidator.DisplayReservoirActive (ANode : TTreeNode);
const OPNAME = 'TReservoirAndChannelOutputValidator.DisplayReservoirActive';
var
  lReservoirNr : integer;
  lReservoir   : IReservoirConfigurationData;
begin
  try
    lReservoirNr := Integer(ANode.Data);
    lReservoir   := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.
                      ReservoirList.ReservoirOrNodeByIdentifier[lReservoirNr].ReservoirConfigurationData;
    if (ANode.ImageIndex <> 5) then
    begin
      if (lReservoir.StatusIndicator = 1) then
      begin
        if (ANode.ImageIndex <> 1) then
          FActiveReservoirCount := FActiveReservoirCount + 1;
        ANode.ImageIndex    := 1;
        ANode.SelectedIndex := 1;
      end
      else
      begin
        if (ANode.ImageIndex = 1) then
          FActiveReservoirCount := FActiveReservoirCount - 1;
        ANode.ImageIndex    := 0;
        ANode.SelectedIndex := 0;
      end;
    end;  
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirAndChannelOutputValidator.UpdateChannelSummaryRequired
                                          (ANode     : TTreeNode;
                                           ARequired : Boolean);
const OPNAME = 'TReservoirAndChannelOutputValidator.UpdateChannelSummaryRequired';
var
  lChannel     : IGeneralFlowChannel;
  lChannelList : IChannelList;
  lChannelNr   : integer;
begin
  try
    lChannelList := TYieldModelDataObject(FAppModules.Model.ModelData).
                      NetworkElementData.ChannelList;
    lChannelNr   := Integer(ANode.Data);
    lChannel     := lChannelList.ChannelByChannelNumber[lChannelNr];
    if (ARequired) then
      lChannel.SummaryOutputRequired := 'Y' {RHS comeback}
    else
      lChannel.SummaryOutputRequired := 'N';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirAndChannelOutputValidator.UpdateChannelAnalysisRequired
                                          (ANode     : TTreeNode;
                                           ARequired : Boolean);
const OPNAME = 'TReservoirAndChannelOutputValidator.UpdateChannelAnalysisRequired';
var
  lChannel     : IGeneralFlowChannel;
  lChannelList : IChannelList;
  lChannelNr   : integer;
begin
  try
    lChannelList := TYieldModelDataObject(FAppModules.Model.ModelData).
                      NetworkElementData.ChannelList;
    lChannelNr   := Integer(ANode.Data);
    lChannel     := lChannelList.ChannelByChannelNumber[lChannelNr];
    if (ARequired) then
      lChannel.RequiresFirmYieldAnalysis := 'Y'
    else
      lChannel.RequiresFirmYieldAnalysis := 'N';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirAndChannelOutputValidator.UpdateReservoirSummaryRequired
                                          (ANode      : TTreeNode;
                                           ARequired  : Boolean);
const OPNAME = 'TReservoirAndChannelOutputValidator.UpdateReservoirSummaryRequired';
var
  lReservoirNr : integer;
  lReservoir   : IReservoirConfigurationData;
begin
  try
    lReservoirNr := Integer(ANode.Data);
    lReservoir   := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.
                      ReservoirList.ReservoirOrNodeByIdentifier[lReservoirNr].ReservoirConfigurationData;
    if (ARequired) then
      lReservoir.IncludeSummary := 'Y'
    else
      lReservoir.IncludeSummary := 'N';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirAndChannelOutputValidator.UpdateReservoirActive
                                          (ANode   : TTreeNode;
                                           AActive : Boolean);
const OPNAME = 'TReservoirAndChannelOutputValidator.UpdateReservoirActive';
var
  lReservoirNr : integer;
  lReservoir   : IReservoirConfigurationData;
begin
  try
    lReservoirNr := Integer(ANode.Data);
    lReservoir   := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.
                      ReservoirList.ReservoirOrNodeByIdentifier[lReservoirNr].ReservoirConfigurationData;
    if (AActive) then
      lReservoir.StatusIndicator := 1
    else
      lReservoir.StatusIndicator := 0;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirAndChannelOutputValidator.OnEditControlClick(Sender: TObject);
const OPNAME = 'TReservoirAndChannelOutputValidator.OnEditControlClick';
begin
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.

