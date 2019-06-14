{******************************************************************************}
{*  UNIT      : Contains the class TCurtailmentStructureValidator.            *}
{*  AUTHOR    : Presley Mudau                                                 *}
{*  DATE      : 2007/06/04                                                    *}
{*  COPYRIGHT : Copyright © 2007 DWAF                                         *}
{******************************************************************************}

unit UCurtailmentStructureValidator;

interface

uses
  Classes,
  VCL.Controls,
  VCL.ComCtrls,
  VCL.StdCtrls,
  VCL.ExtCtrls,
  VCL.Grids,
  UAbstractObject,
  UAbstractComponent,
  VoaimsCom_TLB,
  UDataComponent,
  UDataEditComponent,
  UAbstractModelData,
  UFilesActionAbstractManager,
  UFileNames,
  UAbstractFileNamesObject,
  UFilesActionYieldManager,
  UDataFileObjects,
  UYieldContextValidationType,
  UAbstractYieldDataDialogValidator,
  UCurtailmentStructureDialog;

type
  TCurtailmentStructureValidator = class(TAbstractYieldDataDialogValidator)
  private
  protected
    FSelectedID         : integer;
    FCurtailmentPeriod  : integer;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    procedure OnEditControlEnter(Sender: TObject); override;
    procedure OnEditControltExit(Sender: TObject); override;
    procedure OnCurtailmentCheckClick(Sender: TObject);
    procedure OnInsertCurtailedChannelClick(Sender : TObject);
    procedure OnDeleteCurtailedChannelClick(Sender: TObject);
    procedure OnGridSelectCell(ASender: TObject;ACol, ARow: Longint; var CanSelect: Boolean);
    procedure OnStringGridCellDataHasChanged(ASender: TObject; ACol, ARow: integer); override;
    procedure RePopulateDataViewer;
    procedure RePopulateMonthNumberGrid;
    procedure RePopulateCurtailedChannelGrids;
    procedure RePopulateChannelNodes;
    procedure UpdateNrOfCurtailmentPeriod;
    procedure UpdateCurtailmentStartMonthsGrid(AIndex: integer;AValue : string);
    procedure UpdateAllocationFactorsGrid(ACol, ARow: Integer; AValue : double);
    procedure UpdateImplementCurtailmentFile;
    procedure ValidateNrOfCurtailmentPeriod(ACurtailmentAndDrought : ICurtailmentAndDrought);
    procedure ValidateCurtailedChannelCount (ACurtailmentAndDrought : ICurtailmentAndDrought);
    procedure ValidateStartMonths (ACurtailmentAndDrought : ICurtailmentAndDrought);
    procedure ValidateAllocationFactors (ACurtailmentAndDrought : ICurtailmentAndDrought);

  public
    function Initialise: boolean; override;
    function SaveState: boolean; override;
    function LanguageHasChanged: boolean; override;
    function StudyHasChanged: boolean; override;
    function StudyDataHasChanged(AContext: TChangeContext; AFieldName,AOldValue,ANewValue: string): boolean; override;
    procedure ClearDataViewer; override;
    procedure PopulateDataViewer; override;
    function CurtailmentStructureDialog: TCurtailmentStructureDialog;
    procedure DoContextValidation (AValidationType : TDialogValidationType); override;
  end;

implementation

uses
  SysUtils,
  VCL.Graphics,
  UConstants,
  UUtilities,
  Math,
  UYieldModelDataGUIForm,
  UYieldModelDataObject,
  UErrorHandlingOperations, UNetworkFeaturesData;

{******************************************************************************}
{* TCurtailmentStructureValidator                                                 *}
{******************************************************************************}

procedure TCurtailmentStructureValidator.CreateMemberObjects;
const OPNAME = 'TCurtailmentStructureValidator.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
    FPanel := TCurtailmentStructureDialog.Create(FPanelOwner,FAppModules);
    with CurtailmentStructureDialog do
    begin
      NrOfCurtailmentPeriodEdit.FieldProperty   := FAppModules.FieldProperties.FieldProperty('CurtailmentPeriodCount');
      NrOfCurtailmentPeriodEdit.OnEnter         := OnEditControlEnter;
      NrOfCurtailmentPeriodEdit.OnExit          := OnEditControltExit;

      NrOfCurtailmentChannelEdit.FieldProperty  := FAppModules.FieldProperties.FieldProperty('CurtailmentChannelCount');
      NrOfCurtailmentChannelEdit.OnEnter        := OnEditControlEnter;
      NrOfCurtailmentChannelEdit.OnExit         := OnEditControltExit;

      CurtailmentChannelNumberCbx.FieldProperty   := FAppModules.FieldProperties.FieldProperty('CurtailmentChannelNumber');
      CurtailmentChannelNumberCbx.OnEnter         := OnEditControlEnter;
      CurtailmentChannelNumberCbx.OnExit          := OnEditControltExit;

      BtnAddChannel.OnClick                       := OnInsertCurtailedChannelClick;
      BtnDeleteChannel.OnClick                    := OnDeleteCurtailedChannelClick;

      CurtailmentMonthNumberGrid.OnBeforeCellChange   := OnStringGridCellDataHasChanged;
      CurtailmentMonthNumberGrid.OnSelectCell         := OnGridSelectCell;
      CurtailmentMonthNumberGrid.OnColEnter           := OnStringGridColEnter;
      CurtailmentMonthNumberGrid.OnExit               := OnEditControltExit;
      CurtailmentMonthNumberGrid.OnEnter              := OnEditControlEnter;

      CurtailmentFactorsGrid.OnBeforeCellChange   := OnStringGridCellDataHasChanged;
      CurtailmentFactorsGrid.OnSelectCell         := OnGridSelectCell;
      CurtailmentFactorsGrid.OnColEnter           := OnStringGridColEnter;
      CurtailmentFactorsGrid.OnExit               := OnEditControltExit;
      CurtailmentFactorsGrid.OnEnter              := OnEditControlEnter;

      CurtailmentCheck.FieldProperty := FAppModules.FieldProperties.FieldProperty('CurtailmentSummaryOut');
      CurtailmentCheck.OnEnter       := OnEditControlEnter;
      CurtailmentCheck.OnClick       := OnCurtailmentCheckClick;

    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TCurtailmentStructureValidator.DestroyMemberObjects;
const OPNAME = 'TCurtailmentStructureValidator.DestroyMemberObjects';
begin
  try
    inherited DestroyMemberObjects;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TCurtailmentStructureValidator.Initialise: boolean;
const OPNAME = 'TCurtailmentStructureValidator.Initialise';
begin
  Result := inherited Initialise;
  try
    FCurtailmentPeriod := 0;
    FSelectedID := -1;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TCurtailmentStructureValidator.CurtailmentStructureDialog : TCurtailmentStructureDialog;
const OPNAME = 'TCurtailmentStructureValidator.CurtailmentStructureDialog';
begin
  Result := Nil;
  try
    if (FPanel <> nil) then
      Result := TCurtailmentStructureDialog(FPanel);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TCurtailmentStructureValidator.StudyDataHasChanged(AContext: TChangeContext; AFieldName,AOldValue,ANewValue: string): boolean;
const OPNAME = 'TCurtailmentStructureValidator.StudyDataHasChanged';
begin
  Result := inherited StudyDataHasChanged(AContext,AFieldName,AOldValue,ANewValue);
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TCurtailmentStructureValidator.StudyHasChanged: boolean;
const OPNAME = 'TCurtailmentStructureValidator.StudyHasChanged';
begin
  Result := inherited StudyHasChanged;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TCurtailmentStructureValidator.LanguageHasChanged: boolean;
const OPNAME = 'TCurtailmentStructureValidator.LanguageHasChanged';
begin
  Result := False;
  try
    TabShetCaption := FAppModules.Language.GetString('TabCaption.CurtailmentStructure');
    Result := inherited LanguageHasChanged;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TCurtailmentStructureValidator.SaveState: boolean;
const OPNAME = 'TCurtailmentStructureValidator.SaveState';
begin
  Result := inherited SaveState;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TCurtailmentStructureValidator.ClearDataViewer;
const OPNAME = 'TCurtailmentStructureValidator.ClearDataViewer';
var
  LRow   : integer;
  LCol   : integer;
  lPanel : TCurtailmentStructureDialog;
begin
  inherited ClearDataViewer;
  try
    lPanel := CurtailmentStructureDialog;
    with lPanel do
    begin
      NrOfCurtailmentPeriodEdit.SetFieldValue('');
      for LRow := 0 to CurtailmentFactorsGrid.RowCount do
      begin
         for LCol := 0 to CurtailmentFactorsGrid.ColCount do
           CurtailmentFactorsGrid.Cells[LCol,LRow] := ''
      end;
        CurtailmentCheck.Checked := False;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TCurtailmentStructureValidator.PopulateDataViewer;
const OPNAME = 'TCurtailmentStructureValidator.PopulateDataViewer';
begin
  inherited PopulateDataViewer;
  try
    ClearDataViewer;
    RePopulateDataViewer;
    DoContextValidation(dvtCurtailmentAll);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TCurtailmentStructureValidator.RePopulateDataViewer;
const OPNAME = 'TCurtailmentStructureValidator.RePopulateDataViewer';
var
  LCurtailmentAndDrought  : ICurtailmentAndDrought;
begin
  try
    LCurtailmentAndDrought := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.
                     CurtailmentAndDrought;
    if (LCurtailmentAndDrought <> nil) then
    begin
      with CurtailmentStructureDialog do
      begin
        FCurtailmentPeriod := LCurtailmentAndDrought.CurtailmentPeriodCount;
        NrOfCurtailmentPeriodEdit.SetFieldValue(IntToStr(FCurtailmentPeriod));
        NrOfCurtailmentChannelEdit.SetFieldValue(LCurtailmentAndDrought.CurtailedChannelCount);

        if LCurtailmentAndDrought.ImplementCurtailmentFile then
          CurtailmentCheck.Checked := True
        else
          CurtailmentCheck.Checked := False;

        CurtailmentCheck.Enabled := FCurtailmentPeriod > 0;

        RePopulateMonthNumberGrid;
        RePopulateChannelNodes;
        RePopulateCurtailedChannelGrids;

        BtnDeleteChannel.Enabled := LCurtailmentAndDrought.CurtailedChannelCount > 0;
        BtnAddChannel.Enabled    := (FCurtailmentPeriod > 0) AND
                                    (FCurtailmentPeriod <= 50);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TCurtailmentStructureValidator.OnEditControlEnter(Sender: TObject);
const OPNAME = 'TCurtailmentStructureValidator.OnEditControlEnter';
begin
  inherited OnEditControlEnter(Sender);
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TCurtailmentStructureValidator.OnEditControltExit(Sender: TObject);
const OPNAME = 'TCurtailmentStructureValidator.OnEditControltExit';
begin
  inherited OnEditControltExit(Sender);
  try
   with CurtailmentStructureDialog do
    begin
      if ((Sender = NrOfCurtailmentPeriodEdit) AND
          (NrOfCurtailmentPeriodEdit.HasValueChanged)) then
        UpdateNrOfCurtailmentPeriod;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


procedure TCurtailmentStructureValidator.RePopulateMonthNumberGrid;
const OPNAME = 'TCurtailmentStructureValidator.RePopulateMonthNumberGrid';
var
  LCurtailmentAndDrought : ICurtailmentAndDrought;
  LIndex                 : integer;
  LFieldProperty         : TAbstractFieldProperty;
begin
  try
    lCurtailmentAndDrought := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.
                     CurtailmentAndDrought;
    if (lCurtailmentAndDrought <> nil) then
    begin
      with CurtailmentStructureDialog do
      begin
        CurtailmentMonthNumberGrid.ColCount := FCurtailmentPeriod;
        CurtailmentMonthNumberGrid.Width    := CurtailmentMonthNumberGrid.ColCount * (CurtailmentMonthNumberGrid.DefaultColWidth + 1) + 3;
        CurtailmentMonthNumberGrid.ClearFieldProperties;

        lFieldProperty := FAppModules.FieldProperties.FieldProperty('CurtailmentStartMonthNumber');
        if FCurtailmentPeriod > 0 then
        begin
          for lIndex := 1 to FCurtailmentPeriod do
          begin
            CurtailmentMonthNumberGrid.AddFieldProperty(lFieldProperty);
            if (LCurtailmentAndDrought.StartMonthsByIndex[lIndex] = NullInteger) then
              CurtailmentMonthNumberGrid.Cells[lIndex-1, 0] := ''
            else
              CurtailmentMonthNumberGrid.Cells[lIndex-1, 0] := IntToStr(LCurtailmentAndDrought.StartMonthsByIndex[lIndex]);
          end;
        end
        else
          CurtailmentMonthNumberGrid.Cells[0, 0] := ''
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TCurtailmentStructureValidator.UpdateCurtailmentStartMonthsGrid(AIndex: integer;AValue : string);
const OPNAME = 'TCurtailmentStructureValidator.UpdateCurtailmentStartMonthsGrid';
var
  LCurtailmentAndDrought : ICurtailmentAndDrought;
  LMessage               : string;
begin
  try
    LCurtailmentAndDrought := TYieldModelDataObject(FAppModules.Model.ModelData).
                     NetworkFeaturesData.CurtailmentAndDrought;
    if (LCurtailmentAndDrought <> nil) then
    begin
      with CurtailmentStructureDialog do
      begin
        if (FAppModules.FieldProperties.ValidateFieldProperty('CurtailmentStartMonthValue', AValue,
              lMessage, AIndex)) then
        begin
          CurtailmentMonthNumberGrid.ValidationError[AIndex, 0, gveCellField] := '';
          LCurtailmentAndDrought.StartMonthsByIndex[Aindex] := StrToInt(AValue);
          RePopulateDataViewer;
        end
        else
          CurtailmentMonthNumberGrid.ValidationError[AIndex, 0, gveCellField] := lMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TCurtailmentStructureValidator.RePopulateChannelNodes;
const OPNAME = 'TCurtailmentStructureValidator.RePopulateChannelNodes';
var
  lChannel          : IGeneralFlowChannel;
  LCount,
  LCount1           : Integer;
  LAddChannel       : Boolean;
  LCurtailedChannel : ICurtailedChannel;
begin
  try
    with CurtailmentStructureDialog do
    begin
      CurtailmentChannelNumberCbx.Items.Clear;
      CurtailmentChannelNumberCbx.Enabled := FCurtailmentPeriod > 0;
      for LCount := 0 to TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.
                         ChannelList.ChannelCount - 1 do
      begin
        lChannel := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.
                    ChannelList.ChannelByIndex[LCount];
        if (lChannel <> nil) then
        begin
          LAddChannel := True;
          for LCount1 := 0 to TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.
                               CurtailmentAndDrought.CurtailedChannelCount - 1 do
          begin
            LCurtailedChannel := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.
                                 CurtailmentAndDrought.CurtailedChannelByIndex[LCount1];
            if LCurtailedChannel.ChannelNumber = lChannel.ChannelNumber then
            begin
              LAddChannel := False;
              Break;
            end;
          end;
          if LAddChannel then
            CurtailmentChannelNumberCbx.Items.
                    AddObject('('+IntToStr(lChannel.ChannelNumber)+')' + lChannel.ChannelName,TObject(lChannel.ChannelID));
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TCurtailmentStructureValidator.RePopulateCurtailedChannelGrids;
const OPNAME = 'TCurtailmentStructureValidator.RePopulateCurtailedChannelGrids';
var
  LRow              : integer;
  LCount            : integer;
  LFieldProperty    : TAbstractFieldProperty;
  LChannel          : IGeneralFlowChannel;
  LCurtailedChannel : ICurtailedChannel;
  LCurtailmentAndDrought  : ICurtailmentAndDrought;
begin
  try
    LCurtailmentAndDrought := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.
                    CurtailmentAndDrought;
    if (LCurtailmentAndDrought <> nil) then
    begin
      with CurtailmentStructureDialog do
      begin
        CurtailmentFactorsGrid.ClearFieldProperties;
        CurtailmentFactorsGrid.Cells[0,0]   := 'Number';
        CurtailmentFactorsGrid.Cells[1,0]   := FAppModules.Language.GetString('GridHeading.ChannelNr');
        CurtailmentFactorsGrid.Cells[2,0]   := FAppModules.Language.GetString('GridHeading.ChannelName');
        if FCurtailmentPeriod = 0 then
          CurtailmentFactorsGrid.ColCount := ( 3 + FCurtailmentPeriod +1)
        else
          CurtailmentFactorsGrid.ColCount := ( 3 + FCurtailmentPeriod);

        CurtailmentFactorsGrid.RowCount := Max((CurtailmentFactorsGrid.FixedRows + 1),(1 + LCurtailmentAndDrought.CurtailedChannelCount));


        if (LCurtailmentAndDrought.CurtailedChannelCount > 0) then
        begin
          for lRow := 1 to LCurtailmentAndDrought.CurtailedChannelCount do
          begin
            LCurtailedChannel := LCurtailmentAndDrought.CurtailedChannelByIndex[lRow-1];
            if (LCurtailedChannel <> nil) then
            begin
              LChannel := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData
                          .ChannelList.ChannelByChannelNumber[LCurtailedChannel.ChannelNumber];

              CurtailmentFactorsGrid.Cells[0, lRow] := IntToStr(LRow);
              CurtailmentFactorsGrid.Cells[1, lRow] := IntToStr(LCurtailedChannel.ChannelNumber);
              if (LChannel <> nil) then
              begin
                CurtailmentFactorsGrid.Cells[2, lRow] := LChannel.ChannelName;
                CurtailmentFactorsGrid.IsColumnEnabled [2] := False;
              end;

              lFieldProperty := FAppModules.FieldProperties.FieldProperty('CurtailmentFactors');
              for LCount := 0 to FCurtailmentPeriod-1 do
              begin
                CurtailmentFactorsGrid.AddFieldProperty(lFieldProperty);
                CurtailmentFactorsGrid.AddFieldProperty(lFieldProperty);
                CurtailmentFactorsGrid.AddFieldProperty(lFieldProperty);
                CurtailmentFactorsGrid.AddFieldProperty(lFieldProperty);
                CurtailmentFactorsGrid.Cells[LCount+3, lRow] := FloatToStr(LCurtailedChannel.AllocationFactors[LCount+1]);
                CurtailmentFactorsGrid.Options := CurtailmentFactorsGrid.Options + [goEditing];
                CurtailmentFactorsGrid.Cells[LCount+3,0] := FloatToStr(LCount+1);
              end;
            end;
          end;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TCurtailmentStructureValidator.OnInsertCurtailedChannelClick(Sender: TObject);
const OPNAME = 'TCurtailmentStructureValidator.OnInsertCurtailedChannelClick';
var
  LChannelID             : Integer;
  LChannel               : IGeneralFlowChannel;
  LCbxSelectedIndex      : Integer;
  LCurtailmentAndDrought : ICurtailmentAndDrought;
  LChannelNumber         : Integer;
begin
  try
    with CurtailmentStructureDialog do
    begin
      LCbxSelectedIndex := CurtailmentChannelNumberCbx.ItemIndex;
      if LCbxSelectedIndex <> - 1 then
      begin
        LChannelID:= Integer(CurtailmentChannelNumberCbx.Items.Objects[LCbxSelectedIndex]);
        LChannel  := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.
                                ChannelList.ChannelByIdentifier[LChannelID];
        if (LChannel <> nil) then
        begin
          LCurtailmentAndDrought := TYieldModelDataObject(FAppModules.Model.ModelData).
                                    NetworkFeaturesData.CurtailmentAndDrought;
          LChannelNumber := LChannel.ChannelNumber;
          LCurtailmentAndDrought.CreateChannelCurtailment(LChannelNumber);
          RePopulateDataViewer;
        end;
      end;
      BtnAddChannel.Enabled := ((FCurtailmentPeriod > 0) and (FCurtailmentPeriod <= 50))
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TCurtailmentStructureValidator.OnDeleteCurtailedChannelClick(Sender: TObject);
const OPNAME = 'TCurtailmentStructureValidator.OnDeleteCurtailedChannelClick';
var
  LChannelID             : Integer;
  LCurtailmentAndDrought : ICurtailmentAndDrought;
  LChannel               : IGeneralFlowChannel;
begin
  try
    LChannelID  := FSelectedID;
    LChannel    := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.
                            ChannelList.ChannelByChannelNumber[LChannelID];
    if LChannel <> nil then
    begin
      LCurtailmentAndDrought := TYieldModelDataObject(FAppModules.Model.ModelData).
                                NetworkFeaturesData.CurtailmentAndDrought;
      LCurtailmentAndDrought.RemoveChannelCurtailment(LChannelID);
      PopulateDataViewer;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TCurtailmentStructureValidator.OnGridSelectCell(ASender: TObject; ACol, ARow: Integer; var CanSelect: Boolean);
const OPNAME = 'TCurtailmentStructureValidator.OnGridSelectCell';
var
  LCurtailmentAndDrought : ICurtailmentAndDrought;
  LChannelNumber         : string;
begin
  try
    begin
      LCurtailmentAndDrought := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData
                            .CurtailmentAndDrought;
      if ((LCurtailmentAndDrought <> nil) AND (LCurtailmentAndDrought.CurtailedChannelCount > 0)) then
      begin
        FSelectedID    := -1;
        if (ASender =  CurtailmentStructureDialog.CurtailmentFactorsGrid) then
        begin
          if ((LCurtailmentAndDrought.CurtailedChannelCount > 0) AND (ACol >= 3) AND (ARow >= 1)) then
          begin
            LChannelNumber := CurtailmentStructureDialog.CurtailmentFactorsGrid.Cells[1,ARow];
            if LChannelNumber <> '' then
              FSelectedID := StrToInt(LChannelNumber);
          end;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TCurtailmentStructureValidator.UpdateNrOfCurtailmentPeriod;
const OPNAME = 'TCurtailmentStructureValidator.UpdateNrOfCurtailmentPeriod';
var
  LCurtailmentAndDrought : ICurtailmentAndDrought;
  LMessage               : string;
begin
  try
    LCurtailmentAndDrought := TYieldModelDataObject(FAppModules.Model.ModelData).
                     CastNetworkFeaturesData.CastCurtailmentAndDrought;
    if (LCurtailmentAndDrought <> nil) then
    begin
      with CurtailmentStructureDialog do
      begin
        if (FAppModules.FieldProperties.ValidateFieldProperty('CurtailmentPeriodCount',
              NrOfCurtailmentPeriodEdit.Text, lMessage)) then
        begin
          NrOfCurtailmentPeriodEdit.FieldValidationError := LMessage;
          LCurtailmentAndDrought.CurtailmentPeriodCount  := StrToInt(Trim(NrOfCurtailmentPeriodEdit.Text));
          NrOfCurtailmentPeriodEdit.FieldValidationError := LMessage;
          RePopulateDataViewer;
          DoContextValidation(dvtNrOfCurtailmentPeriod);
        end
        else
          NrOfCurtailmentPeriodEdit.FieldValidationError := LMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TCurtailmentStructureValidator.OnStringGridCellDataHasChanged(ASender: TObject;
                                                                        ACol,
                                                                        ARow: integer);
const OPNAME = 'TCurtailmentStructureValidator.OnStringGridCellDataHasChanged';
begin
  inherited OnStringGridCellDataHasChanged(ASender,ACol,ARow);
    try
    with CurtailmentStructureDialog do
    begin
      if (ASender = CurtailmentMonthNumberGrid) then
        UpdateCurtailmentStartMonthsGrid(ACol+1, Trim(CurtailmentMonthNumberGrid.Cells[ACol, ARow]))
      else
      if (ASender = CurtailmentFactorsGrid) then
         UpdateAllocationFactorsGrid(ACol, ARow, StrToFloat(Trim(CurtailmentFactorsGrid.Cells[ACol, ARow])));
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TCurtailmentStructureValidator.UpdateAllocationFactorsGrid(ACol, ARow: Integer; AValue: double);
const OPNAME = 'TCurtailmentStructureValidator.UpdateAllocationFactorsGrid';
var
  LMessage               : string;
  LCurtailmentAndDrought : ICurtailmentAndDrought;
  LCurtailedChannel      : ICurtailedChannel;
begin
  try
    LCurtailmentAndDrought := TYieldModelDataObject(FAppModules.Model.ModelData).
                             NetworkFeaturesData.CurtailmentAndDrought;
    if (LCurtailmentAndDrought <> nil) then
    begin
      with CurtailmentStructureDialog do
      begin
        CurtailmentFactorsGrid.ValidationError[ACol, ARow, gveCellContext] := '';
        if (FAppModules.FieldProperties.ValidateFieldProperty('CurtailmentFactors',
            FloatToStr(AValue), LMessage, ACol, ARow)) then
        begin
          LCurtailedChannel := LCurtailmentAndDrought.CurtailedChannelByChannelNumber[strToInt(CurtailmentFactorsGrid.Cells[1,ARow])];
          if(LCurtailedChannel <> nil) then
          begin
            LCurtailedChannel.AllocationFactors[ACol-2] := AValue;
            RePopulateDataViewer;
            DoContextValidation (dvtIrrigationBlockWaterUseMonthly);
          end;
        end
        else
          CurtailmentFactorsGrid.ValidationError[ACol, ARow, gveCellContext] := LMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TCurtailmentStructureValidator.DoContextValidation(AValidationType: TDialogValidationType);
const OPNAME = 'TCurtailmentStructureValidator.DoContextValidation';
var
  lCurtailmentAndDrought : ICurtailmentAndDrought;
begin
  try
    FAllErrorMessages.Clear;
    lCurtailmentAndDrought := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.
                     CurtailmentAndDrought;
    if (lCurtailmentAndDrought <> nil) then
    begin
      case AValidationType of
        dvtCurtailmentAll :
        begin
          ValidateNrOfCurtailmentPeriod(lCurtailmentAndDrought);
          ValidateCurtailedChannelCount(lCurtailmentAndDrought);
          ValidateStartMonths(lCurtailmentAndDrought);
          ValidateAllocationFactors(lCurtailmentAndDrought);
        end;
        dvtNrOfCurtailmentPeriod : ValidateNrOfCurtailmentPeriod(lCurtailmentAndDrought);
        dvtCurtailmentStartMonth : ValidateStartMonths(lCurtailmentAndDrought);
        dvtAllocationFactors     : ValidateAllocationFactors(lCurtailmentAndDrought);
        dvtNrOfCurtailedChannel  : ValidateCurtailedChannelCount(lCurtailmentAndDrought);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TCurtailmentStructureValidator.UpdateImplementCurtailmentFile;
const OPNAME = 'TCurtailmentStructureValidator.UpdateImplementCurtailmentFile';
var
  LCurtailmentAndDrought    : ICurtailmentAndDrought;
begin
  try
    LCurtailmentAndDrought := TYieldModelDataObject(FAppModules.Model.ModelData).
                              NetworkFeaturesData.CurtailmentAndDrought;
    if (LCurtailmentAndDrought <> nil) then
    begin
      with CurtailmentStructureDialog do
      begin
        if CurtailmentCheck.Checked then
          LCurtailmentAndDrought.ImplementCurtailmentFile := TRUE
        else
          LCurtailmentAndDrought.ImplementCurtailmentFile := FALSE
        end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TCurtailmentStructureValidator.ValidateAllocationFactors
                                  (ACurtailmentAndDrought : ICurtailmentAndDrought);
const OPNAME = 'TCurtailmentStructureValidator.ValidateAllocationFactors';
var
  lErrorCols        : TStringList;
  lErrorMsgs        : TStringList;
  lCol              : integer;
  lIndex            : integer;
  LChannelCount     : integer;
  LCurtailedChannel : ICurtailedChannel;
begin
  try
    if (ACurtailmentAndDrought <> nil) then
    begin
      with CurtailmentStructureDialog do
      begin
        lErrorCols := TStringList.Create;
        lErrorMsgs := TStringList.Create;
        try
          lErrorCols.Clear;
          FErrorMessage := '';
          for LChannelCount := 0 to ACurtailmentAndDrought.CurtailedChannelCount-1 do
          begin
            LCurtailedChannel := ACurtailmentAndDrought.CurtailedChannelByIndex[LChannelCount];
            if LCurtailedChannel <> nil then
            begin
              if (LCurtailedChannel.Validate(FErrorMessage, 'CurtailmentFactors')) then
              begin
                for lCol := 1 to FCurtailmentPeriod do
                   CurtailmentFactorsGrid.ValidationError[lCol+1, LChannelCount+1 , gveCellContext] := ''
              end
              else
              begin
                ExtractErrorsAndColumns(FErrorMessage, lErrorMsgs, lErrorCols);
                for lCol := 1 to FCurtailmentPeriod do
                begin
                  lIndex := lErrorCols.IndexOf(IntToStr(lCol));
                  if (lIndex >= 0) then
                    CurtailmentFactorsGrid.ValidationError[lCol+1, LChannelCount+1, gveCellContext] := lErrorMsgs.Strings[lIndex]
                  else
                    CurtailmentFactorsGrid.ValidationError[lCol+1, LChannelCount+1, gveCellContext] := ''
                end;
                FAllErrorMessages.AddStrings(lErrorMsgs);
              end;
            end;
          end;
        finally
          FreeAndNil(lErrorCols);
          FreeAndNil(lErrorMsgs);
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TCurtailmentStructureValidator.ValidateStartMonths
                                (ACurtailmentAndDrought : ICurtailmentAndDrought);
const OPNAME = 'TCurtailmentStructureValidator.ValidateStartMonths';
var
  lErrorCols : TStringList;
  lErrorMsgs : TStringList;
  lIndex     : integer;
  lCount     : integer;
begin
  try
    with CurtailmentStructureDialog do
    begin
      lErrorCols := TStringList.Create;
      lErrorMsgs := TStringList.Create;
      try
        lErrorCols.Clear;
        FErrorMessage := '';
        if (ACurtailmentAndDrought.Validate(FErrorMessage, 'CurtailmentStartMonthValue')) then
        begin
          for lCount := 1 to FCurtailmentPeriod do
            CurtailmentMonthNumberGrid.ValidationError[lCount-1, 0, gveColContext] := ''
        end
        else
        begin
          ExtractErrorsAndColumns(FErrorMessage, lErrorMsgs, lErrorCols);
          for lCount := 1 to FCurtailmentPeriod do
          begin
            lIndex := lErrorCols.IndexOf(IntToStr(lCount));
            if (lIndex >= 0) then
              CurtailmentMonthNumberGrid.ValidationError[lCount-1, 0, gveColContext] := lErrorMsgs.Strings[lIndex]
            else
              CurtailmentMonthNumberGrid.ValidationError[lCount-1, 0, gveColContext] := '';
          end;
          FAllErrorMessages.AddStrings(lErrorMsgs);
        end;
      finally
        FreeAndNil(lErrorCols);
        FreeAndNil(lErrorMsgs);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


procedure TCurtailmentStructureValidator.ValidateCurtailedChannelCount
                                  (ACurtailmentAndDrought : ICurtailmentAndDrought);
const OPNAME = 'TCurtailmentStructureValidator.ValidateCurtailedChannelCount';
begin
  try
    with CurtailmentStructureDialog do
    begin
      FErrorMessage := '';
      if (NOT ACurtailmentAndDrought.Validate(FErrorMessage, 'CurtailmentChannelCount')) then
        FAllErrorMessages.Add(Trim(FErrorMessage));
      NrOfCurtailmentChannelEdit.ContextValidationError := FErrorMessage;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TCurtailmentStructureValidator.ValidateNrOfCurtailmentPeriod
                                 (ACurtailmentAndDrought : ICurtailmentAndDrought);
const OPNAME = 'TCurtailmentStructureValidator.ValidateNrOfCurtailmentPeriod';
begin
  try
    with CurtailmentStructureDialog do
    begin
      FErrorMessage := '';
      if (NOT ACurtailmentAndDrought.Validate(FErrorMessage, 'CurtailmentPeriodCount')) then
        FAllErrorMessages.Add(Trim(FErrorMessage));
      NrOfCurtailmentPeriodEdit.ContextValidationError := FErrorMessage;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


procedure TCurtailmentStructureValidator.OnCurtailmentCheckClick(Sender: TObject);
const OPNAME = 'TCurtailmentStructureValidator.OnCurtailmentCheckClick';
begin
  try
    if(CurtailmentStructureDialog.CurtailmentCheck.HasValueChanged) then
     UpdateImplementCurtailmentFile;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.

