//
//
//  UNIT      : Contains the class TReservoirEvaporationValidator.
//  AUTHOR    : Presley Mudau
//  DATE      : 2007/11/09
//  COPYRIGHT : Copyright © 2007 DWAF
//
//
unit UMineSubCatchmentFlowFactorsValidator;

interface

uses
  Classes,
  VCL.Controls,
  VCL.ComCtrls,
  VCL.StdCtrls,
  VCL.ExtCtrls,
  VCLTee.Series,
  VCLTee.Chart,
  VCLTee.TeEngine,

  // arivia.kom
  VoaimsCom_TLB,
  UAbstractObject,
  UAbstractComponent,
  UDataComponent,
  UDataEditComponent,
  UYieldContextValidationType,
  UAbstractYieldDataDialogValidator,
  UMineSubCatchmentFlowFactorsDialog;

type
  TMineSubCatchmentFlowFactorsValidator = class(TAbstractYieldDataDialogValidator)
  protected
    FInDragMode        : boolean;
    FSeriesPointIndex  : Longint;
    FSeries            : TBarSeries;
    FStartYValue       : double;
    FStartYPosition    : double;
    FIdentifier        : integer;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    procedure OnEditControlEnter(Sender: TObject); override;
    procedure OnEditControltExit(Sender: TObject); override;
    procedure OnStringGridCellDataHasChanged(ASender: TObject; ACol, ARow: integer);  override;
    procedure OnAfterPasteColumnData(Sender: TObject);

    procedure UpdateMineSubCatchmentFlowFactors(AMonthNumber: integer; ANewValue: string);
    function PopulateMineSubCatchmentFlowFactors (AMineSubCatchment : IMineSubCatchment) : boolean;
    procedure OnChartClickSeries(Sender: TCustomChart; Series: TChartSeries; ValueIndex: LongInt;
              Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure OnMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: integer);
    procedure OnMouseMove(Sender: TObject; Shift: TShiftState; X, Y: integer);
    procedure OnKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure CancelDragMode;
    procedure ValidateEvaporation(AMineSubCatchment: IMineSubCatchment);
  public

    function Initialise: boolean; override;
    function SaveState: boolean; override;
    function LanguageHasChanged: boolean; override;
    function StudyHasChanged: boolean; override;
    function StudyDataHasChanged(AContext: TChangeContext; AFieldName,AOldValue,ANewValue: string): boolean; override;

    procedure ClearDataViewer; override;
    procedure PopulateDataViewer; override;

    function MineSubCatchmentFlowFactorsDialog: TMineSubCatchmentFlowFactorsDialog;
    procedure DoContextValidation(AValidationtype: TDialogValidationType); override;

    property Identifier : integer read FIdentifier write FIdentifier;
  end;

implementation

uses

  // Delphi VCL, RTL, etc

  SysUtils,
  VCLTee.TeeProcs,
  VCL.Graphics,
  windows,
  UUtilities,
  // arivia.kom
  UConstants,
  UYieldModelDataObject,
  URunConfigurationData,
  UErrorHandlingOperations;

{ TMineSubCatchmentFlowFactorsValidator }

procedure TMineSubCatchmentFlowFactorsValidator.CreateMemberObjects;
const OPNAME = 'TMineSubCatchmentFlowFactorsValidator.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
    CancelDragMode;

    FPanel := TMineSubCatchmentFlowFactorsDialog.Create(FPanelOwner,FAppModules);

    MineSubCatchmentFlowFactorsDialog.GrdEvaporation.AddFieldProperty(fappModules.FieldProperties.FieldProperty('Evaporation'));
    MineSubCatchmentFlowFactorsDialog.GrdEvaporation.OnEnter    := OnEditControlEnter;
    MineSubCatchmentFlowFactorsDialog.GrdEvaporation.OnExit     := OnEditControltExit;
    MineSubCatchmentFlowFactorsDialog.GrdEvaporation.OnColEnter := OnStringGridColEnter;
    MineSubCatchmentFlowFactorsDialog.GrdEvaporation.OnBeforeCellChange := OnStringGridCellDataHasChanged;
    MineSubCatchmentFlowFactorsDialog.GrdEvaporation.OnKeyDown          := Self.OnKeyDown;
    MineSubCatchmentFlowFactorsDialog.GrdEvaporation.ShowGridPopupMenu   := True;
    MineSubCatchmentFlowFactorsDialog.GrdEvaporation.AllowPasteFromExcel := True;
    MineSubCatchmentFlowFactorsDialog.GrdEvaporation.OnPasteFromExcel    := Self.OnAfterPasteColumnData;
    MineSubCatchmentFlowFactorsDialog.GrdEvaporation.OnAfterPasteColumnData := Self.OnAfterPasteColumnData;

    if(FAppModules.User.UserRights in CUR_EditData) and
      (not FAppModules.StudyArea.ScenarioLocked) then
    begin
      MineSubCatchmentFlowFactorsDialog.ChtEvaporation.OnClickSeries := Self.OnChartClickSeries;
      MineSubCatchmentFlowFactorsDialog.ChtEvaporation.OnMouseMove   := Self.OnMouseMove;
      MineSubCatchmentFlowFactorsDialog.ChtEvaporation.OnMouseUp     := Self.OnMouseUp;
    end;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMineSubCatchmentFlowFactorsValidator.DestroyMemberObjects;
const OPNAME = 'TMineSubCatchmentFlowFactorsValidator.DestroyMemberObjects';
begin
  try
    inherited DestroyMemberObjects;

  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMineSubCatchmentFlowFactorsValidator.Initialise: boolean;
const OPNAME = 'TMineSubCatchmentFlowFactorsValidator.Initialise';
begin
  Result := inherited Initialise;
  try
    CancelDragMode;
    MineSubCatchmentFlowFactorsDialog.GrdEvaporation.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('Month'));
    MineSubCatchmentFlowFactorsDialog.GrdEvaporation.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('Evaporation'));
    MineSubCatchmentFlowFactorsDialog.GrdEvaporation.OnColEnter(MineSubCatchmentFlowFactorsDialog.GrdEvaporation,1,1);
  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMineSubCatchmentFlowFactorsValidator.LanguageHasChanged: boolean;
const OPNAME = 'TMineSubCatchmentFlowFactorsValidator.LanguageHasChanged';
begin
  Result := False;
  try
    TabShetCaption := 'Evaporation';
    Result := inherited LanguageHasChanged;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMineSubCatchmentFlowFactorsValidator.ClearDataViewer;
const OPNAME = 'TMineSubCatchmentFlowFactorsValidator.ClearDataViewer';
var
  LIndex : integer;
begin
  inherited ClearDataViewer;
  try
    for LIndex := 1 to 13 do
      MineSubCatchmentFlowFactorsDialog.GrdEvaporation.Cells[1,LIndex] := '';
    MineSubCatchmentFlowFactorsDialog.ChtEvaporation.SeriesList.Clear;
    MineSubCatchmentFlowFactorsDialog.ChtEvaporation.UndoZoom;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMineSubCatchmentFlowFactorsValidator.PopulateDataViewer;
const OPNAME = 'TMineSubCatchmentFlowFactorsValidator.PopulateDataViewer';
var
  LCount                 : integer;
  lTotal                 : double;
  lValue                 : double;
  lFieldProperty         : TAbstractFieldProperty;
  LRunConfigurationData  : IRunConfigurationData;
  LMineSubCatchment  :    IMineSubCatchment;
begin
  inherited PopulateDataViewer;
  try
    ClearDataViewer;
    DoContextValidation(dvtMineSubCatchmentAll);
    LMineSubCatchment := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.
                         MineSubCatchmentList.MineSubCatchmentByIdentifier[FIdentifier];
    if (LMineSubCatchment <> nil) then
    begin
      LRunConfigurationData := TYieldModelDataObject(FAppModules.Model.ModelData).RunConfigurationData;
      if Assigned(LRunConfigurationData) then
      begin
        with MineSubCatchmentFlowFactorsDialog do
        begin
          lTotal := 0.0;
          for LCount := 1 to 12 do
          begin
            MineSubCatchmentFlowFactorsDialog.GrdEvaporation.Cells[0,LCount] := LRunConfigurationData.MonthNameByIndex[LCount];
            if(LMineSubCatchment.MinimunGroundwaterFlowVolume[LCount] = NullFloat) then
                MineSubCatchmentFlowFactorsDialog.GrdEvaporation.Cells[1,LCount] := ''
            else
            begin
              lFieldProperty := FAppModules.FieldProperties.FieldProperty('Evaporation');
              MineSubCatchmentFlowFactorsDialog.GrdEvaporation.AddFieldProperty(lFieldProperty);
              MineSubCatchmentFlowFactorsDialog.GrdEvaporation.Cells[1,LCount] :=
                 Format(lFieldProperty.FormatStringGrid, [LMineSubCatchment.MinimunGroundwaterFlowVolume[LCount]]);
              lValue := LMineSubCatchment.MinimunGroundwaterFlowVolume[LCount];
              lTotal := lTotal + lValue;
              MineSubCatchmentFlowFactorsDialog.GrdEvaporation.Cells[1,13] :=Format(lFieldProperty.FormatStringGrid, [lTotal]);
            end;
          end;
        end;

        MineSubCatchmentFlowFactorsDialog.GrdEvaporation.IsRowEnabled[13] := False;

        if PopulateMineSubCatchmentFlowFactors(lMineSubCatchment) then
          MineSubCatchmentFlowFactorsDialog.Populated := True;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMineSubCatchmentFlowFactorsValidator.SaveState: boolean;
const OPNAME = 'TMineSubCatchmentFlowFactorsValidator.SaveState';
begin
  Result := inherited SaveState;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMineSubCatchmentFlowFactorsValidator.OnEditControlEnter(Sender: TObject);
const OPNAME = 'TMineSubCatchmentFlowFactorsValidator.OnEditControlEnter';
begin
  inherited OnEditControlEnter(Sender);
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMineSubCatchmentFlowFactorsValidator.OnEditControltExit(Sender: TObject);
const OPNAME = 'TMineSubCatchmentFlowFactorsValidator.OnEditControltExit';
begin
  inherited OnEditControltExit(Sender);
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMineSubCatchmentFlowFactorsValidator.OnStringGridCellDataHasChanged(ASender: TObject; ACol, ARow: integer);
const OPNAME = 'TMineSubCatchmentFlowFactorsValidator.OnStringGridCellDataHasChanged';
begin
  inherited OnStringGridCellDataHasChanged(ASender,ACol, ARow);
  try
    if((ASender =  MineSubCatchmentFlowFactorsDialog.GrdEvaporation) and (ARow < 13)) then
       UpdateMineSubCatchmentFlowFactors(ARow,MineSubCatchmentFlowFactorsDialog.GrdEvaporation.Cells[ACol, ARow]);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMineSubCatchmentFlowFactorsValidator.MineSubCatchmentFlowFactorsDialog: TMineSubCatchmentFlowFactorsDialog;
const OPNAME = 'TMineSubCatchmentFlowFactorsValidator.MineSubCatchmentFlowFactorsDialog';
begin
  Result := Nil;
  try
    if Assigned(FPanel) then
      Result := TMineSubCatchmentFlowFactorsDialog(FPanel);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMineSubCatchmentFlowFactorsValidator.StudyDataHasChanged(AContext: TChangeContext; AFieldName,AOldValue,ANewValue: string): boolean;
const OPNAME = 'TMineSubCatchmentFlowFactorsValidator.StudyDataHasChanged';
begin
  Result := inherited StudyDataHasChanged(AContext,AFieldName,AOldValue,ANewValue);
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMineSubCatchmentFlowFactorsValidator.StudyHasChanged: boolean;
const OPNAME = 'TMineSubCatchmentFlowFactorsValidator.StudyHasChanged';
begin
  Result := inherited StudyHasChanged;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMineSubCatchmentFlowFactorsValidator.PopulateMineSubCatchmentFlowFactors(AMineSubCatchment : IMineSubCatchment) : boolean;
const OPNAME = 'TMineSubCatchmentFlowFactorsValidator.PopulateMineSubCatchmentFlowFactors';
var
  LIndex                 : integer;
  LSeries                : TCustomBarSeries;
  LRunConfigurationData  : IRunConfigurationData;
begin
  Result := False;
  try
    LRunConfigurationData := TYieldModelDataObject(FAppModules.Model.ModelData).RunConfigurationData;

    MineSubCatchmentFlowFactorsDialog.ChtEvaporation.AllowPanning := pmBoth;  // possibly disable to allow dragging of bars
    MineSubCatchmentFlowFactorsDialog.ChtEvaporation.AllowZoom := True;        // possibly disable to allow dragging of bars
    MineSubCatchmentFlowFactorsDialog.ChtEvaporation.Title.Text.Text := '';
    MineSubCatchmentFlowFactorsDialog.ChtEvaporation.AxisVisible := True;
    MineSubCatchmentFlowFactorsDialog.ChtEvaporation.ClipPoints := True;
    MineSubCatchmentFlowFactorsDialog.ChtEvaporation.Frame.Visible := True;
    MineSubCatchmentFlowFactorsDialog.ChtEvaporation.Legend.Visible := False;
    MineSubCatchmentFlowFactorsDialog.ChtEvaporation.View3D := False;
    MineSubCatchmentFlowFactorsDialog.ChtEvaporation.RemoveAllSeries;
    MineSubCatchmentFlowFactorsDialog.ChtEvaporation.SeriesList.Clear;

    MineSubCatchmentFlowFactorsDialog.ChtEvaporation.LeftAxis.Title.Caption := FAppModules.Language.GetString('TField.ReservoirEvap');
    MineSubCatchmentFlowFactorsDialog.ChtEvaporation.LeftAxis.Title.Angle := 90;
    MineSubCatchmentFlowFactorsDialog.ChtEvaporation.BottomAxis.Title.Caption := FAppModules.Language.GetString('TField.Month');

    // Series
    LSeries := TBarSeries.Create(MineSubCatchmentFlowFactorsDialog.ChtEvaporation);
    LSeries.ParentChart := MineSubCatchmentFlowFactorsDialog.ChtEvaporation;
    LSeries.Marks.Visible := False;
    LSeries.SeriesColor := clRed;
    LSeries.Title := FAppModules.Language.GetString('ReservoirSeries.EvaporationSeries');
    LSeries.BarWidthPercent := 80;
    LSeries.XValues.Order := loAscending;
    LSeries.YValues.Order := loNone;

   for LIndex := 1 to 12 do
    begin
      if(AMineSubCatchment.MinimunGroundwaterFlowVolume[LIndex] = NullFloat) then
        LSeries.Add(0.0, LRunConfigurationData.MonthNameByIndex[LIndex])
      else
      LSeries.Add(AMineSubCatchment.MinimunGroundwaterFlowVolume[LIndex], LRunConfigurationData.MonthNameByIndex[LIndex]);
    end;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMineSubCatchmentFlowFactorsValidator.UpdateMineSubCatchmentFlowFactors(AMonthNumber: integer; ANewValue: string);
const OPNAME = 'TMineSubCatchmentFlowFactorsValidator.UpdateMineSubCatchmentFlowFactors';
var
  LMineSubCatchment : IMineSubCatchment;
  LEvapValue        : double;
begin
  try
    LEvapValue := StrToFloat(ANewValue);
    LMineSubCatchment := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.
                         MineSubCatchmentList.MineSubCatchmentByIdentifier[FIdentifier];
    if (LMineSubCatchment <> nil) then
    begin
      LMineSubCatchment.MinimunGroundwaterFlowVolume[AMonthNumber] := LEvapValue;
      MineSubCatchmentFlowFactorsDialog.GrdEvaporation.Cells[1,AMonthNumber] :=
        FloatToStr(LMineSubCatchment.MinimunGroundwaterFlowVolume[AMonthNumber]);
      PopulateMineSubCatchmentFlowFactors(LMineSubCatchment);
      PopulateDataViewer;
      DoContextValidation(dvtMineSubCatchmentAll);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMineSubCatchmentFlowFactorsValidator.CancelDragMode;
const OPNAME = 'TMineSubCatchmentFlowFactorsValidator.CancelDragMode';
begin
  try
    FInDragMode  := False;
    FSeriesPointIndex := -1;
    FSeries      := nil;
    FStartYPosition  := 0.0;
    FStartYValue     := 0.0;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMineSubCatchmentFlowFactorsValidator.OnChartClickSeries(Sender: TCustomChart; Series: TChartSeries;
          ValueIndex: Integer; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
const OPNAME = 'TMineSubCatchmentFlowFactorsValidator.OnChartClickSeries';
begin
  try
    if(Button = mbLeft) then
    begin
      FSeries      := TBarSeries(Series);
      FSeriesPointIndex := FSeries.Clicked(X, Y);
      if(FSeriesPointIndex >= 0) and ((FSeriesPointIndex <= 11)) then
      begin
        FInDragMode     := True;
        FStartYValue    := FSeries.YValues.Value[FSeriesPointIndex];
        FStartYPosition := FSeries.YScreenToValue(Y);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMineSubCatchmentFlowFactorsValidator.OnMouseMove(Sender: TObject; Shift: TShiftState; X, Y: integer);
const OPNAME = 'TMineSubCatchmentFlowFactorsValidator.OnMouseMove';
var
  LYChange,
  LYValue: Double;
  lFieldProperty    : TAbstractFieldProperty;
begin
  try
    if (ssLeft in Shift) and FInDragMode then
    begin
      LYValue  := FSeries.YScreenToValue(Trunc(Y));
      if(LYValue >= 0.0) then
      begin
        LYChange := LYValue - FStartYPosition;
        FSeries.YValues.Value[FSeriesPointIndex] := FStartYValue + LYChange;
        FSeries.Repaint;
        lFieldProperty := FAppModules.FieldProperties.FieldProperty('Evaporation');
        MineSubCatchmentFlowFactorsDialog.GrdEvaporation.AddFieldProperty(lFieldProperty);
        MineSubCatchmentFlowFactorsDialog.GrdEvaporation.Cells[1,FSeriesPointIndex+1] :=Format(lFieldProperty.FormatStringGrid, [FSeries.YValues.Value[FSeriesPointIndex]]);
          //FormatFloat('####0.0###', FSeries.YValues.Value[FSeriesPointIndex]);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMineSubCatchmentFlowFactorsValidator.OnMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: integer);
const OPNAME = 'TMineSubCatchmentFlowFactorsValidator.OnMouseUp';
var
  LYChange,
  LYValue: Double;
begin
  try
    if FInDragMode then
    begin
      LYValue  := FSeries.YScreenToValue(Trunc(Y));
      LYChange := LYValue - FStartYPosition;
      if(LYChange > 0.001) or (LYChange < -0.001)then
      UpdateMineSubCatchmentFlowFactors(FSeriesPointIndex + 1,MineSubCatchmentFlowFactorsDialog.GrdEvaporation.Cells[1,FSeriesPointIndex+1]);
      CancelDragMode;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


procedure TMineSubCatchmentFlowFactorsValidator.OnKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
const OPNAME = 'TMineSubCatchmentFlowFactorsValidator.OnKeyDown';
begin
  try
    if (Key = VK_ESCAPE) then
    begin
      if FInDragMode then
      begin
        CancelDragMode;
        PopulateDataViewer;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMineSubCatchmentFlowFactorsValidator.ValidateEvaporation(AMineSubCatchment: IMineSubCatchment);
const OPNAME = 'TMineSubCatchmentFlowFactorsValidator.ValidateEvaporation';
var
  lCol           : integer;
  lIndex         : integer;
  lErrorCols     : TStringList;
  lErrorMessages : TStringList;
begin
  try
    if (AMineSubCatchment <> nil) then
    begin
      with MineSubCatchmentFlowFactorsDialog do
      begin
        lErrorCols     := TStringList.Create;
        lErrorMessages := TStringList.Create;
        try
          FErrorMessage := '';
          lErrorCols.Clear;
          if (AMineSubCatchment.Validate(FErrorMessage,'Evaporation')) then
          begin
            for lCol := 1 to 12 do
              GrdEvaporation.ValidationError[1, lCol, gveCellContext] := ''
          end
          else
          begin
            ExtractErrorsAndColumns(FErrorMessage, lErrorMessages, lErrorCols);
            for lCol := 1 to 12 do
            begin
              lIndex := lErrorCols.IndexOf(IntToStr(lCol));
              if (lIndex >= 0) then
                GrdEvaporation.ValidationError[1, lCol, gveCellContext] := lErrorMessages.Strings[lIndex]
              else
                GrdEvaporation.ValidationError[1, lCol, gveCellContext] := '';
            end;
              FAllErrorMessages.Add(FErrorMessage);
          end;
        finally
          FreeAndNil(lErrorCols);
          FreeAndNil(lErrorMessages);
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMineSubCatchmentFlowFactorsValidator.DoContextValidation(AValidationtype: TDialogValidationType);
const OPNAME = 'TMineSubCatchmentFlowFactorsValidator.DoContextValidation';
var
  LMineSubCatchment    : IMineSubCatchment;
  LMineSubCatchmentList: IMineSubCatchmentList;
begin
  try
    FAllErrorMessages.Clear;
    if (FIdentifier >= 0) then
    begin
      LMineSubCatchmentList := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.
                         MineSubCatchmentList;
      LMineSubCatchment := LMineSubCatchmentList.MineSubCatchmentByIdentifier[FIdentifier];
      if (LMineSubCatchment <> nil) then
      begin
        case AValidationtype of
          dvtMineSubCatchmentAll : ValidateEvaporation(LMineSubCatchment);
          dvtMinimunGroundwaterFlowVolume : ValidateEvaporation(LMineSubCatchment);
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMineSubCatchmentFlowFactorsValidator.OnAfterPasteColumnData(Sender: TObject);
const OPNAME = 'TMineSubCatchmentFlowFactorsValidator.OnAfterPasteColumnData';
var
  LMineSubCatchment : IMineSubCatchment;
  LEvapValue        : double;
  LMonth            : integer;
begin
  try
    LMineSubCatchment := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData
                         .MineSubCatchmentList.MineSubCatchmentByIdentifier[FIdentifier];
    if(LMineSubCatchment <> nil) then
    begin
      for LMonth := 1 to 12 do
      begin
        LEvapValue := StrToFloat(MineSubCatchmentFlowFactorsDialog.GrdEvaporation.Cells[1,LMonth]);
        LMineSubCatchment.MinimunGroundwaterFlowVolume[LMonth] := LEvapValue;
      end;
      PopulateDataViewer;
      DoContextValidation(dvtMineSubCatchmentAll);
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

end.

