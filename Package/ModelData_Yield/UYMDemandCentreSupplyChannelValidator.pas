{******************************************************************************}
{*  UNIT      : Contains the class TYMDemandCentreSupplyChannelValidator      *}
{*  AUTHOR    : Maurice Marinus                                               *}
{*  DATE      : 2006/12/12                                                    *}
{*  COPYRIGHT : Copyright © 2006 DWAF                                         *}
{******************************************************************************}

unit UYMDemandCentreSupplyChannelValidator;

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
  UYMDemandCentreSupplyChannelDialog,
  UAbstractYieldDataDialogValidator,
  UYieldContextValidationType,
  UIrrigationBlock,
  UChannelPenaltyValidator,
  UChannelPenaltyDialog,
  VCL.Dialogs;

type
  TYMDemandCentreSupplyChannelValidator = class(TAbstractYieldDataDialogValidator)
  protected
    FSelectedSupplyID,
    FSelectedCol,
    FSelectedRow              : Integer;
    FChannelPenaltyValidator  : TChannelPenaltyValidator;
    procedure CreateMemberObjects;  override;
    procedure DestroyMemberObjects; override;

    procedure OnEditControlEnter(Sender: TObject); override;
    procedure OnEditControltExit(Sender: TObject); override;
    procedure OnGridSelectCell(ASender: TObject;ACol, ARow: Longint; var CanSelect: Boolean);
    procedure OnInsertSupply(Sender: TObject);
    procedure OnDeleteSupply(Sender: TObject);

    procedure RePopulateNodes;
    procedure RePopulateDataViewer;
  public
    function Initialise: boolean;         override;
    function SaveState: boolean;          override;
    function LanguageHasChanged: boolean; override;
    function StudyHasChanged: boolean;    override;
    function StudyDataHasChanged(AContext: TChangeContext; AFieldName,AOldValue,ANewValue: string): boolean; override;
    procedure ClearDataViewer;            override;
    procedure PopulateDataViewer;         override;
    function YMDemandCentreSupplyChannelDialog: TYMDemandCentreSupplyChannelDialog;
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
{* TYMDemandCentreSupplyChannelValidator                                      *}
{******************************************************************************}

procedure TYMDemandCentreSupplyChannelValidator.CreateMemberObjects;
const OPNAME = 'TYMDemandCentreSupplyChannelValidator.CreateMemberObjects';
var
  lIndex : Integer;
begin
  try
    inherited CreateMemberObjects;
    FPanel := TYMDemandCentreSupplyChannelDialog.Create(FPanelOwner,FAppModules);

    with YMDemandCentreSupplyChannelDialog do
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
            
      GeneralChannelsCbx.FieldProperty        := FAppModules.FieldProperties.FieldProperty('UpNodeNumber');
      GeneralChannelsCbx.OnEnter              := OnEditControlEnter;
      GeneralChannelsCbx.OnExit               := OnEditControltExit;

      BtnInsertRow.OnClick                    := OnInsertSupply;
      BtnDeleteRow.OnClick                    := OnDeleteSupply;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYMDemandCentreSupplyChannelValidator.DestroyMemberObjects;
const OPNAME = 'TYMDemandCentreSupplyChannelValidator.DestroyMemberObjects';
begin
  try
    inherited DestroyMemberObjects;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYMDemandCentreSupplyChannelValidator.Initialise: boolean;
const OPNAME = 'TYMDemandCentreSupplyChannelValidator.Initialise';
begin
  Result := inherited Initialise;
  try
    with YMDemandCentreSupplyChannelDialog.SupplyChannelsGrid do
    begin
      Cells[0,0] := FAppModules.Language.GetString('GridHeading.Number');
      Cells[1,0] := FAppModules.Language.GetString('GridHeading.ChannelNr');
      Cells[2,0] := FAppModules.Language.GetString('GridHeading.ChannelName');
      Cells[3,0] := FAppModules.Language.GetString('GridHeading.UpstreamNode');
      Cells[4,0] := FAppModules.Language.GetString('GridHeading.PenaltyStructure');
      Cells[5,0] := FAppModules.Language.GetString('GridHeading.PrintOutput');
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYMDemandCentreSupplyChannelValidator.LanguageHasChanged: boolean;
const OPNAME = 'TYMDemandCentreSupplyChannelValidator.LanguageHasChanged';
begin
  Result := False;
  try
    TabShetCaption := FAppModules.Language.GetString('ViewData.YMDemandCentreSupplyChannel');
    Result := inherited LanguageHasChanged;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYMDemandCentreSupplyChannelValidator.ClearDataViewer;
const OPNAME = 'TYMDemandCentreSupplyChannelValidator.ClearDataViewer';
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
    with YMDemandCentreSupplyChannelDialog do
    begin
      for lIndex := 1 to 6 do
        SupplyChannelsGrid.Cells[lIndex, 1] := '';
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

procedure TYMDemandCentreSupplyChannelValidator.PopulateDataViewer;
const OPNAME = 'TYMDemandCentreSupplyChannelValidator.PopulateDataViewer';
begin
  inherited PopulateDataViewer;
  try
    ClearDataViewer;
    RePopulateDataViewer;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYMDemandCentreSupplyChannelValidator.StudyDataHasChanged(AContext: TChangeContext; AFieldName,AOldValue,ANewValue: string): boolean;
const OPNAME = 'TYMDemandCentreSupplyChannelValidator.StudyDataHasChanged';
begin
  Result := inherited StudyDataHasChanged(AContext,AFieldName,AOldValue,ANewValue);
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYMDemandCentreSupplyChannelValidator.StudyHasChanged: boolean;
const OPNAME = 'TYMDemandCentreSupplyChannelValidator.StudyHasChanged';
begin
  Result := inherited StudyHasChanged;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYMDemandCentreSupplyChannelValidator.OnEditControlEnter(Sender: TObject);
const OPNAME = 'TYMDemandCentreSupplyChannelValidator.OnEditControlEnter';
begin
  inherited OnEditControlEnter(Sender);
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYMDemandCentreSupplyChannelValidator.OnEditControltExit(Sender: TObject);
const OPNAME = 'TYMDemandCentreSupplyChannelValidator.OnEditControltExit';
begin
  inherited OnEditControltExit(Sender);
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYMDemandCentreSupplyChannelValidator.RePopulateDataViewer;
const OPNAME = 'TYMDemandCentreSupplyChannelValidator.RePopulateDataViewer';
var
  lCount              : Integer;
  LChannel            : IGeneralFlowChannel;
  LDemandCentreList   : IYMDemandCentreList;
  LDemandCentreNodeNr : Integer;
  LRowCount           : Integer;
begin
  try
    LDemandCentreList   := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.YMDemandCentreList;
    LDemandCentreNodeNr := LDemandCentreList.YMDemandCentreByID[FIdentifier].NodeNumber;

    YMDemandCentreSupplyChannelDialog.DownStreamNodeEdit.Text     := LDemandCentreList.YMDemandCentreByID[FIdentifier].Name;
    YMDemandCentreSupplyChannelDialog.SupplyChannelsGrid.RowCount := 1;

    LRowCount := 0;
    for LCount := 0 to TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.ChannelList.ChannelCount -1 do
    begin
      LChannel  := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.ChannelList.ChannelByIndex[LCount];
      if (LChannel.DownStreamNodeNumber = LDemandCentreNodeNr) and (lChannel.ChannelType = 12) then
        Inc(LRowCount);
    end;

    YMDemandCentreSupplyChannelDialog.SupplyChannelsGrid.RowCount := LRowCount + 1;
    LRowCount := 0;
    for LCount := 0 to TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.ChannelList.ChannelCount-1 do
    begin
      LChannel  := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.ChannelList.ChannelByIndex[LCount];
      if (LChannel.DownStreamNodeNumber = LDemandCentreNodeNr) and (lChannel.ChannelType = 12) then
      begin
        Inc(LRowCount);
        with YMDemandCentreSupplyChannelDialog do
        begin
          SupplyChannelsGrid.Rows[LRowCount].Objects[0] := TObject(LChannel.ChannelID);
          SupplyChannelsGrid.Cells[0,LRowCount]         := IntToStr(LRowCount);
          SupplyChannelsGrid.Cells[1,LRowCount]         := IntToStr(LChannel.ChannelNumber);
          SupplyChannelsGrid.Cells[2,LRowCount]         := LChannel.ChannelName;
          if LChannel.UpStreamNodeNumber <> 0 then
            SupplyChannelsGrid.Cells[3,LRowCount]       := LChannel.UpStreamNode.ReservoirConfigurationData.ReservoirName
          else
            SupplyChannelsGrid.Cells[3,LRowCount]       := LChannel.SourceName;
          SupplyChannelsGrid.Cells[4,LRowCount]         := IntToStr(lChannel.ChannelPenaltyNumber);
          if Trim(lChannel.SummaryOutputRequired) = '' then
            SupplyChannelsGrid.Cells[5,LRowCount]         := 'N'
          else
            SupplyChannelsGrid.Cells[5,LRowCount]         := lChannel.SummaryOutputRequired;

          if SupplyChannelsGrid.RowCount > 1 then
            SupplyChannelsGrid.FixedRows := 1;
        end;
      end;
    end;
    RePopulateNodes;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYMDemandCentreSupplyChannelValidator.SaveState: boolean;
const OPNAME = 'TYMDemandCentreSupplyChannelValidator.SaveState';
begin
  Result := inherited SaveState;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYMDemandCentreSupplyChannelValidator.YMDemandCentreSupplyChannelDialog: TYMDemandCentreSupplyChannelDialog;
const OPNAME = 'TYMDemandCentreSupplyChannelValidator.YMDemandCentreSupplyChannelDialog';
begin
  Result := Nil;
  try
    if (FPanel <> nil) then
      Result := TYMDemandCentreSupplyChannelDialog(FPanel);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYMDemandCentreSupplyChannelValidator.OnGridSelectCell(ASender: TObject; ACol, ARow: Integer; var CanSelect: Boolean);
const OPNAME = 'TYMDemandCentreSupplyChannelValidator.OnGridSelectCell';
begin
  try
    FSelectedSupplyID    := -1;
    FSelectedCol          := -1;
    FSelectedRow          := -1;

    if ASender = YMDemandCentreSupplyChannelDialog.SupplyChannelsGrid then
    begin
      FSelectedCol  := ACol;
      FSelectedRow  := ARow;
      if YMDemandCentreSupplyChannelDialog.SupplyChannelsGrid.Rows[ARow].Objects[0] <> nil then
        FSelectedSupplyID := Integer(YMDemandCentreSupplyChannelDialog.SupplyChannelsGrid.Rows[ARow].Objects[0]);
    end;

    CanSelect := True;
    YMDemandCentreSupplyChannelDialog.GeneralChannelsCbx.Clear;
    RePopulateNodes;
    YMDemandCentreSupplyChannelDialog.ResetButtonState;    
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYMDemandCentreSupplyChannelValidator.RePopulateNodes;
const OPNAME = 'TYMDemandCentreSupplyChannelValidator.RePopulateNodes';
var
  lChannel  : IGeneralFlowChannel;
  LCount,
  LCount1   : Integer;
  LAddChannel : Boolean;
  LYMDemandCentre : IYMDemandCentre;
begin
  try
    YMDemandCentreSupplyChannelDialog.GeneralChannelsCbx.Items.Clear;
    if (FIdentifier >= 0) then
    begin
      for LCount := 0 to TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.
                                                                                    ChannelList.ChannelCount - 1 do
      begin
        lChannel := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.ChannelList.ChannelByIndex[LCount];
        if (lChannel <> nil) and (lChannel.ChannelType = 12) then
        begin
          LAddChannel := True;
          for LCount1 := 0 to TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.
                                                                      YMDemandCentreList.YMDemandCentreCount - 1 do
          begin
            LYMDemandCentre := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.
                                                            YMDemandCentreList.YMDemandCentreByIndex[LCount1];
            if LYMDemandCentre.NodeNumber = lChannel.DownStreamNodeNumber then
            begin
              LAddChannel := False;
              Break;
            end;
          end;
          if LAddChannel then
            YMDemandCentreSupplyChannelDialog.GeneralChannelsCbx.Items.
                    AddObject('('+IntToStr(lChannel.ChannelNumber)+')'+lChannel.ChannelName,TObject(lChannel.ChannelID));
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYMDemandCentreSupplyChannelValidator.OnDeleteSupply(Sender: TObject);
const OPNAME = 'TYMDemandCentreSupplyChannelValidator.OnDeleteSupply';
var
  LChannelID  : Integer;
  LChannel    : IGeneralFlowChannel;
begin
  try
    LChannelID  := FSelectedSupplyID;
    LChannel    := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.
                            ChannelList.ChannelByIdentifier[LChannelID];
    if LChannel <> nil then
    begin
      LChannel.DownStreamNodeNumber := 0;
      RePopulateDataViewer;
    end;  
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYMDemandCentreSupplyChannelValidator.OnInsertSupply(Sender: TObject);
const OPNAME = 'TYMDemandCentreSupplyChannelValidator.OnInsertSupply';
var
  LChannelID        : Integer;
  LChannel          : IGeneralFlowChannel;
  LCbxSelectedIndex : Integer;
  LDemandCentreList : IYMDemandCentreList;
  LDemandCentreNodeNr : Integer;
begin
  try
    LCbxSelectedIndex := YMDemandCentreSupplyChannelDialog.GeneralChannelsCbx.ItemIndex;
    if LCbxSelectedIndex <> - 1 then
    begin
      LChannelID        := Integer(YMDemandCentreSupplyChannelDialog.GeneralChannelsCbx.Items.Objects[LCbxSelectedIndex]);
      LChannel          := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.
                              ChannelList.ChannelByIdentifier[LChannelID];
      LDemandCentreList   := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.YMDemandCentreList;
      LDemandCentreNodeNr := LDemandCentreList.YMDemandCentreByID[FIdentifier].NodeNumber;
      
      LChannel.DownStreamNodeNumber := LDemandCentreNodeNr;
      RePopulateDataViewer;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
