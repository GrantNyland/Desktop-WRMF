//
//
//  UNIT      : Contains the class TChannelDemandsGridValidator.
//  AUTHOR    : Dziedzi Ramulondi (ARIVIA)
//  DATE      : 2005/05/03
//  COPYRIGHT : Copyright © 2005 DWAF
//
//
unit UChannelDemandsGridValidator;

interface

uses
  Classes,
  VCL.Controls,
  VCL.ComCtrls,
  VCL.StdCtrls,
  VCL.ExtCtrls,
  VCL.Dialogs,
  Types,
  VCL.Grids,
  VCL.Graphics,
  UAbstractObject,
  UAbstractComponent,
  UDataComponent,
  VoaimsCom_TLB,
  UDataEditComponent,
  UYieldContextValidationType,
  UChannelDemandsGridDialog;

type
  TChannelDemandsGridValidator = class(TAbstractOutputDialogValidator)
  protected
    FLoadCase            : integer;
    FSequence            : integer;
    FMonth               : integer;
    FUnits               : TOutputUnits;
    FValueType           : TOutputValueType;
    FDefValueType        : TOutputValueType;
    FTimeStep            : TOutputTimeStep;
    FHighLight           : WordBool;
    FDisplayMonth        : integer;
    FPopulating          : boolean;
    procedure CreateMemberObjects; override;

    procedure OnBtnDataSelectionClick(Sender: TObject);
    procedure OnUseMaxValuesClick(Sender: TObject);
    procedure PopulateDemandeGrid(AChannel : IGeneralFlowChannel);
    procedure PopulateLimiteGrid(AChannel : IGeneralFlowChannel);
    procedure RePopulateDataViewer;
    procedure GetSelectionData;
    procedure ClearGrid;
    procedure OnStringGridCellDataHasChanged(ASender: TObject; ACol, ARow: integer);  override;
    procedure UpdateMaxDemand(AMonthNumber: integer; ANewValue: string);
  public
    function Initialise: boolean; override;
    function SaveState: boolean; override;
    function LanguageHasChanged: boolean; override;
    function StudyHasChanged: boolean; override;
    function StudyDataHasChanged(AContext: TChangeContext; AFieldName,AOldValue,ANewValue: string): boolean; override;
    function CanExport : boolean; override;
    function CanPrint  : boolean; override;
    function ViewDialog: TChannelDemandsGridDialog;
    procedure DoExport(AFileName: string = ''); override;
    procedure DoPrint; override;
    procedure ClearDataViewer; override;
    procedure PopulateDataViewer; override;
  end;

implementation

uses
  Math,
  VCLTee.Series,
  Windows,
  SysUtils,
  Variants,
  UUtilities,
  UOutputData,
  UConstants,
  UDataSetType,
  UIrrigationBlock,
  UYieldModelDataObject,
  UErrorHandlingOperations,
  UNetworkElementData,
  UChannelData,
  UReservoirData;

{ TChannelDemandsGridValidator }

procedure TChannelDemandsGridValidator.CreateMemberObjects;
const OPNAME = 'TChannelDemandsGridValidator.CreateMemberObjects';
var
  LIndex : integer;
begin
  try
    inherited CreateMemberObjects;
    FPanel := TChannelDemandsGridDialog.Create(FPanelOwner,FAppModules);
    ViewDialog.BtnDataSelection.OnClick := OnBtnDataSelectionClick;
    ViewDialog.LimitsGrid.OnBeforeCellChange := OnStringGridCellDataHasChanged;
    ViewDialog.chkboxLimits.OnClick := OnUseMaxValuesClick;
    for LIndex := 1 to 13 do
    begin
     ViewDialog.LimitsGrid.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('IrrigationBlockAllocatedIrrigationArea'));
   end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TChannelDemandsGridValidator.Initialise: boolean;
const OPNAME = 'TChannelDemandsGridValidator.Initialise';
begin
  Result := inherited Initialise;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TChannelDemandsGridValidator.StudyDataHasChanged(AContext: TChangeContext; AFieldName,AOldValue,ANewValue: string): boolean;
const OPNAME = 'TChannelDemandsGridValidator.StudyDataHasChanged';
begin
  Result := inherited StudyDataHasChanged(AContext,AFieldName,AOldValue,ANewValue);
  try
    if(UpperCase(AFieldName) = 'LOADCASESCOUNT') or
      (UpperCase(AFieldName) = 'HYDROSEQCOUNT')  or
      (UpperCase(AFieldName) = 'OUTPUTDATASELECTION')then
      PopulateDataViewer;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TChannelDemandsGridValidator.StudyHasChanged: boolean;
const OPNAME = 'TChannelDemandsGridValidator.StudyHasChanged';
begin
  Result := inherited StudyHasChanged;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TChannelDemandsGridValidator.LanguageHasChanged: boolean;
const OPNAME = 'TChannelDemandsGridValidator.LanguageHasChanged';
begin
  Result := False;
  try
    TabShetCaption := FAppModules.Language.GetString('TabCaption.ChannelDemands');
    Result := inherited LanguageHasChanged;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TChannelDemandsGridValidator.SaveState: boolean;
const OPNAME = 'TChannelDemandsGridValidator.SaveState';
begin
  Result := inherited SaveState;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TChannelDemandsGridValidator.ViewDialog : TChannelDemandsGridDialog;
const OPNAME = 'TChannelDemandsGridValidator.ViewDialog';
begin
  Result := nil;
  try
    if Assigned(FPanel) then
      Result := TChannelDemandsGridDialog(FPanel);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TChannelDemandsGridValidator.ClearDataViewer;
const OPNAME = 'TChannelDemandsGridValidator.ClearDataViewer';
begin
  inherited ClearDataViewer;
  try
    ClearGrid;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TChannelDemandsGridValidator.PopulateDataViewer;
const OPNAME = 'TChannelDemandsGridValidator.PopulateDataViewer';
begin
  inherited PopulateDataViewer;
  try
    ClearDataViewer;
    RePopulateDataViewer;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TChannelDemandsGridValidator.RePopulateDataViewer;
const OPNAME = 'TChannelDemandsGridValidator.RePopulateDataViewer';
var
  LIndex          : integer;
  LChannel        : IGeneralFlowChannel;
  LStartMonth     : integer;
  LYieldModelData : IYieldModelData;
begin
  FPopulating := True;
  try
    GetSelectionData;
    ClearGrid;
    LYieldModelData := (FAppModules.Model.ModelData as IYieldModelData);
    LChannel := LYieldModelData.NetworkElementData.ChannelList.ChannelByChannelNumber[FIdentifier];
    if (lChannel <> nil) then
    begin
      ViewDialog.DemandsGrid.Cells[0,0] := FAppModules.Language.GetString('MonthGrid.Year');
      ViewDialog.DemandsGrid.Cells[13,0] := FAppModules.Language.GetString('MonthGrid.Average');
      ViewDialog.LimitsGrid.Cells[0,0] := FAppModules.Language.GetString('NetworkFeatures.Month');
      ViewDialog.LimitsGrid.Cells[0,1] := FAppModules.Language.GetString('TabCaption.ChannelDemands');

      LStartMonth := LYieldModelData.RunConfigurationData.StartMonthNumber - 1; // + FAppModules.StudyArea.CalendarStartMonth + 1;

      for LIndex := 1 to 12 do
      begin
        LStartMonth := LStartMonth + 1;
        if(LStartMonth > 12) then
          LStartMonth := 1;
        ViewDialog.DemandsGrid.Cells[LIndex,0] :=
          lYieldModelData.RunConfigurationData.MonthNameByIndex[LStartMonth];
        ViewDialog.LimitsGrid.Cells[LIndex,0] :=
          lYieldModelData.RunConfigurationData.MonthNameByIndex[LStartMonth];
      end;
      PopulateDemandeGrid(lChannel);
      if(lChannel.ChannelType = ctIrrigationBlockInflowChannel) then
      begin
        PopulateLimiteGrid(lChannel);
        ViewDialog.gboxLimits.Visible := True;
      end;
    end;
    if (FUnits = ouMcmPerMonthOrYear) then
       CalculateMCMPerMonthGridValues(ViewDialog.DemandsGrid);
  except on E: Exception do HandleError(E, OPNAME) end;
  FPopulating := False;
end;

procedure TChannelDemandsGridValidator.PopulateDemandeGrid(AChannel: IGeneralFlowChannel);
const OPNAME = 'TChannelDemandsGridValidator.PopulateDemandeGrid';
var
  LDataContainer : TStringList;
  LOutputData    : TOutputData;
  LErrors        : string;
  LIndex         : integer;
  LCol           : integer;

  LMonthlyDataContainer : TStringList;
  LYear          : string;
  LValues        : array[1..13] of double;
  LDaysPerMonth  : array[1..13] of double;
  //LMonthlyTotals : array[1..13] of double;
  LMonthValue    : double;
  //LTotal         : double;
  //LGrandTotal    : double;
begin
  try
    LDataContainer := TStringList.Create;
    LMonthlyDataContainer := TStringList.Create;
    try
      LOutputData := TYieldModelDataObject(FAppModules.Model.ModelData).OutputData;
      LOutputData.CastDemandOutputData.GetChannelDemandValues(LDataContainer,AChannel.ChannelNumber,LErrors);
      if (LDataContainer.Count > 0) then
        ViewDialog.DemandsGrid.RowCount := ViewDialog.DemandsGrid.RowCount + LDataContainer.Count-1;

      for LCol := 1 to 12 do
        LDaysPerMonth[LCol] :=  TYieldModelDataObject(FAppModules.Model.ModelData).RunConfigurationData.MonthDaysByIndex[LCol];
      LDaysPerMonth[13] := TYieldModelDataObject(FAppModules.Model.ModelData).CastRunConfigurationData.TotalDaysInAYear;

      for LIndex := 0 to LDataContainer.Count-1 do
      begin
        if FUnits in [ouMcmPerMonthOrYear,ouMegaLitersPerDay] then
        begin
          LMonthlyDataContainer.CommaText := LDataContainer[LIndex];
          if(LMonthlyDataContainer.Count >= 13) then
          begin
            LYear  := LMonthlyDataContainer[0];
            for LCol := 1 to 13 do
            begin
              LMonthValue := StrToFloatDef(LMonthlyDataContainer[LCol],0.0);
              if(FUnits = ouMcmPerMonthOrYear) then
                LMonthValue := (LMonthValue * 60 * 60 * 24 * LDaysPerMonth[LCol])/1000000.0;
              if(FUnits = ouMegaLitersPerDay) then
                LMonthValue := (LMonthValue * 60 * 60 * 24) /1000.0;
              LValues[LCol] := LMonthValue;
            end;
            LDataContainer[LIndex] := LYear +','+ DoubleArrayToCommaText(LValues,3);
          end;
        end;
        ViewDialog.DemandsGrid.Rows[LIndex+1].CommaText := LDataContainer[LIndex];
      end;
    finally
      LDataContainer.Free;
      LMonthlyDataContainer.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TChannelDemandsGridValidator.PopulateLimiteGrid(AChannel: IGeneralFlowChannel);
const OPNAME = 'TChannelDemandsGridValidator.GetSelectionData';
var
  LIndex              : integer;
  LNodeNumber         : integer;
  LIrrigationBlock    : TIrrigationBlock;
  LMaxDemand          : TDiversionChannelMaxDemand;
begin
  try
    LNodeNumber       := AChannel.DownStreamNodeNumber;
    LIrrigationBlock  := TYieldModelDataObject(FAppModules.Model.ModelData).CastNetworkFeaturesData.
                         CastIrrigationBlockList.CastIrrigationBlockByBlockNodeNumber(LNodeNumber);
    if(LIrrigationBlock <> nil) then
    begin
      LMaxDemand := LIrrigationBlock.DiversionChannelMaxDemand;
       if LMaxDemand.Populated then
      begin
        for LIndex := 1 to 12 do
        begin
          if(LMaxDemand.MaxDemandByIndex[LIndex] = NullFloat) then
            ViewDialog.LimitsGrid.Cells[LIndex,1] := ''
          else
            ViewDialog.LimitsGrid.Cells[LIndex,1] := FormatFloat('##0.000',LMaxDemand.MaxDemandByIndex[LIndex]);
        end;
      end;
      ViewDialog.chkboxLimits.Checked := LMaxDemand.InUse;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TChannelDemandsGridValidator.GetSelectionData;
const OPNAME = 'TChannelDemandsGridValidator.GetSelectionData';
var
  LDataSelection : IOutputDataSelection;
begin
  try
   LDataSelection := TYieldModelDataObject(FAppModules.Model.ModelData).OutputData.CastDataSelection;
   if (LDataSelection <> nil) then
   begin
     FLoadCase     := LDataSelection.LoadCase;
     FSequence     := LDataSelection.Sequence;
     FMonth        := LDataSelection.Month;
     FUnits        := LDataSelection.Units;
     FValueType    := LDataSelection.ValueType;
     FDefValueType := FValueType;
     FTimeStep     := LDataSelection.TimeStep;
     FHighLight    := LDataSelection.Highlight;
     FDisplayMonth := LDataSelection.DisplayMonth;
   end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TChannelDemandsGridValidator.ClearGrid;
const OPNAME = 'TChannelDemandsGridValidator.ClearGrid';
var
  LIndex: integer;
begin
  try
    ViewDialog.gboxLimits.Visible := False;
    for LIndex := 0 to ViewDialog.LimitsGrid.ColCount -1 do
      ViewDialog.LimitsGrid.Cols[LIndex].Clear;
    for LIndex := 0 to ViewDialog.DemandsGrid.ColCount -1 do
      ViewDialog.DemandsGrid.Cols[LIndex].Clear;
    ViewDialog.LimitsGrid.ColCount := 13;
    ViewDialog.LimitsGrid.RowCount := 2;
    ViewDialog.DemandsGrid.ColCount := 14;
    ViewDialog.DemandsGrid.RowCount := 2;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TChannelDemandsGridValidator.CanExport: boolean;
const OPNAME = 'TChannelDemandsGridValidator.CanExport';
begin
  Result := False;
  try
    if (ViewDialog <> nil) then
      Result := ViewDialog.CanExport;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TChannelDemandsGridValidator.CanPrint: boolean;
const OPNAME = 'TChannelDemandsGridValidator.CanPrint';
begin
  Result := False;
  try
    if (ViewDialog <> nil) then
      Result := ViewDialog.CanPrint;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TChannelDemandsGridValidator.DoExport(AFileName: string = '');
const OPNAME = 'TChannelDemandsGridValidator.DoExport';
begin
  try
    ViewDialog.DoExport(AFileName);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TChannelDemandsGridValidator.DoPrint;
const OPNAME = 'TChannelDemandsGridValidator.DoPrint';
begin
  try
    ViewDialog.DoPrint;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TChannelDemandsGridValidator.OnBtnDataSelectionClick(Sender: TObject);
const OPNAME = 'TChannelDemandsGridValidator.OnBtnDataSelectionClick';
begin
  try
    TYieldModelDataObject(FAppModules.Model.ModelData).OutputData.ShowDataSelectionDialog(FIdentifier,FNetworkElementType,osdChannelDemands,btMonthlyAverageChannelFlow,FValueType);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TChannelDemandsGridValidator.OnStringGridCellDataHasChanged(ASender: TObject; ACol, ARow: integer);
const OPNAME = 'TReservoirEvaporationValidator.OnStringGridCellDataHasChanged';
begin
  inherited OnStringGridCellDataHasChanged(ASender,ACol, ARow);
  try
    if FPopulating then Exit;
    if((ASender =  ViewDialog.LimitsGrid) and (ARow < 13)) then
       UpdateMaxDemand(ACol,ViewDialog.LimitsGrid.Cells[ACol, ARow]);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TChannelDemandsGridValidator.UpdateMaxDemand(AMonthNumber: integer; ANewValue: string);
const OPNAME = 'TChannelDemandsGridValidator.UpdateMaxDemand';
var
  LNodeNumber         : integer;
  LIrrigationBlock    : TIrrigationBlock;
  LMaxDemand          : TDiversionChannelMaxDemand;
  LChannel            : IGeneralFlowChannel;
  LYieldModelData     : IYieldModelData;
  LValue              : double;
begin
  try
    LYieldModelData := (FAppModules.Model.ModelData as IYieldModelData);
    LChannel := LYieldModelData.NetworkElementData.ChannelList.ChannelByChannelNumber[FIdentifier];
    if (lChannel <> nil) and (lChannel.ChannelType = ctIrrigationBlockInflowChannel) then
    begin
      LNodeNumber       := lChannel.DownStreamNodeNumber;
      LIrrigationBlock  := TYieldModelDataObject(FAppModules.Model.ModelData).CastNetworkFeaturesData.
                           CastIrrigationBlockList.CastIrrigationBlockByBlockNodeNumber(LNodeNumber);
      if(LIrrigationBlock <> nil) then
      begin
        LMaxDemand := LIrrigationBlock.DiversionChannelMaxDemand;
        LValue := StrToFloatDef(ANewValue,0.0);
        LMaxDemand.MaxDemandByIndex[AMonthNumber] := LValue;
        RePopulateDataViewer;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TChannelDemandsGridValidator.OnUseMaxValuesClick(Sender: TObject);
const OPNAME = 'TChannelDemandsGridValidator.UpdateMaxDemand';
var
  LNodeNumber         : integer;
  LIrrigationBlock    : TIrrigationBlock;
  LMaxDemand          : TDiversionChannelMaxDemand;
  LChannel            : IGeneralFlowChannel;
  LYieldModelData     : IYieldModelData;
begin
  try
    if FPopulating then Exit;
    LYieldModelData := (FAppModules.Model.ModelData as IYieldModelData);
    LChannel := LYieldModelData.NetworkElementData.ChannelList.ChannelByChannelNumber[FIdentifier];
    if (lChannel <> nil) and (lChannel.ChannelType = ctIrrigationBlockInflowChannel) then
    begin
      LNodeNumber       := lChannel.DownStreamNodeNumber;
      LIrrigationBlock  := TYieldModelDataObject(FAppModules.Model.ModelData).CastNetworkFeaturesData.
                           CastIrrigationBlockList.CastIrrigationBlockByBlockNodeNumber(LNodeNumber);
      if(LIrrigationBlock <> nil) then
      begin
        LMaxDemand := LIrrigationBlock.DiversionChannelMaxDemand;
        LMaxDemand.InUse := ViewDialog.chkboxLimits.Checked;
        RePopulateDataViewer;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.

