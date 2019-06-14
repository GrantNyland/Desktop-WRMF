//
//
//  UNIT      : Contains the class TReservoirEvaporationValidator.
//  AUTHOR    : Dziedzi Ramulondi
//  DATE      : 2003/07/02
//  COPYRIGHT : Copyright © 2003 DWAF
//
//
unit UReservoirEvaporationValidator;

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
  VCLTee.TeeProcs,

  // arivia.kom
  VoaimsCom_TLB,
  UAbstractObject,
  UAbstractComponent,
  UDataComponent,
  UDataEditComponent,
  UYieldContextValidationType,
  UAbstractYieldDataDialogValidator,
  UReservoirEvaporationDialog;

type
  TReservoirEvaporationValidator = class(TAbstractYieldDataDialogValidator)
  protected
    FInDragMode: boolean;
    FSeriesPointIndex: Longint;
    FSeries: TBarSeries;
    FStartYValue,
    FStartYPosition: double;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    procedure OnEditControlEnter(Sender: TObject); override;
    procedure OnEditControltExit(Sender: TObject); override;
    procedure OnStringGridCellDataHasChanged(ASender: TObject; ACol, ARow: integer);  override;
    procedure OnAfterPasteColumnData(Sender: TObject); 

    procedure UpdateEvaporation(AMonthNumber: integer; ANewValue: string);
    function PopulateReservoirEvaporations (AReservoirData : IReservoirData) : boolean;
    procedure OnChartClickSeries(Sender: TCustomChart; Series: TChartSeries; ValueIndex: LongInt;
              Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure OnMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: integer);
    procedure OnMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure OnKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure CancelDragMode;
    procedure ValidateEvaporation(AReservoir: IReservoirData);
  public

    function Initialise: boolean; override;
    function SaveState: boolean; override;
    function LanguageHasChanged: boolean; override;
    function StudyHasChanged: boolean; override;
    function StudyDataHasChanged(AContext: TChangeContext; AFieldName,AOldValue,ANewValue: string): boolean; override;
    function ProcessMetaDataEvent : boolean; override;

    procedure ClearDataViewer; override;
    procedure PopulateDataViewer; override;

    function ReservoirEvaporationDialog: TReservoirEvaporationDialog;
    procedure DoContextValidation(AValidationtype: TDialogValidationType); override;
    function DetermineWizardStatus (ASequence : integer = 0) : integer; override;
  end;

implementation

uses

  // Delphi VCL, RTL, etc

  SysUtils,
  VCL.Graphics,
  windows,
  UUtilities,
  // arivia.kom
  UConstants,
  UYieldModelDataObject,
  URunConfigurationData,
  UErrorHandlingOperations;

{ TReservoirEvaporationValidator }

procedure TReservoirEvaporationValidator.CreateMemberObjects;
const OPNAME = 'TReservoirEvaporationValidator.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
    CancelDragMode;

    FPanel := TReservoirEvaporationDialog.Create(FPanelOwner,FAppModules);

    ReservoirEvaporationDialog.GrdEvaporation.AddFieldProperty(fappModules.FieldProperties.FieldProperty('Evaporation'));
    ReservoirEvaporationDialog.GrdEvaporation.OnEnter    := OnEditControlEnter;
    ReservoirEvaporationDialog.GrdEvaporation.OnExit     := OnEditControltExit;
    ReservoirEvaporationDialog.GrdEvaporation.OnColEnter := OnStringGridColEnter;
    ReservoirEvaporationDialog.GrdEvaporation.OnBeforeCellChange := OnStringGridCellDataHasChanged;
    ReservoirEvaporationDialog.GrdEvaporation.OnKeyDown          := Self.OnKeyDown;
    ReservoirEvaporationDialog.GrdEvaporation.ShowGridPopupMenu   := True;
    ReservoirEvaporationDialog.GrdEvaporation.AllowPasteFromExcel := True;
    ReservoirEvaporationDialog.GrdEvaporation.OnPasteFromExcel    := Self.OnAfterPasteColumnData;
    ReservoirEvaporationDialog.GrdEvaporation.OnAfterPasteColumnData := Self.OnAfterPasteColumnData;

    if(FAppModules.User.UserRights in CUR_EditData) and
      (not FAppModules.StudyArea.ScenarioLocked) then
    begin
      ReservoirEvaporationDialog.ChtEvaporation.OnClickSeries := Self.OnChartClickSeries;
      ReservoirEvaporationDialog.ChtEvaporation.OnMouseMove   := Self.OnMouseMove;
      ReservoirEvaporationDialog.ChtEvaporation.OnMouseUp     := Self.OnMouseUp;
    end;

    //ReservoirEvaporationDialog.GrdEvaporation.OnAfterCellChange
  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirEvaporationValidator.DestroyMemberObjects;
const OPNAME = 'TReservoirEvaporationValidator.DestroyMemberObjects';
begin
  try
    inherited DestroyMemberObjects;

  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirEvaporationValidator.Initialise: boolean;
const OPNAME = 'TReservoirEvaporationValidator.Initialise';
begin
  Result := inherited Initialise;
  try
    CancelDragMode;
    ReservoirEvaporationDialog.GrdEvaporation.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('Month'));
    ReservoirEvaporationDialog.GrdEvaporation.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('Evaporation'));
    ReservoirEvaporationDialog.GrdEvaporation.OnColEnter(ReservoirEvaporationDialog.GrdEvaporation,1,1);
  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirEvaporationValidator.LanguageHasChanged: boolean;
const OPNAME = 'TReservoirEvaporationValidator.LanguageHasChanged';
begin
  Result := False;
  try
    TabShetCaption := 'Evaporation';
    Result := inherited LanguageHasChanged;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirEvaporationValidator.ClearDataViewer;
const OPNAME = 'TReservoirEvaporationValidator.ClearDataViewer';
var
  LIndex : integer;
begin
  inherited ClearDataViewer;
  try
    for LIndex := 1 to 13 do
      ReservoirEvaporationDialog.GrdEvaporation.Cells[1,LIndex] := '';
    ReservoirEvaporationDialog.ChtEvaporation.SeriesList.Clear;
    ReservoirEvaporationDialog.ChtEvaporation.UndoZoom;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirEvaporationValidator.PopulateDataViewer;
const OPNAME = 'TReservoirEvaporationValidator.PopulateDataViewer';
var
  lFieldIndex            : string;
  lKeyValues             : string;
  LCount                 : integer;
  lTotal                 : double;
  lValue                 : double;
  LReservoirData         : IReservoirData;
  lFieldProperty         : TAbstractFieldProperty;
  LRunConfigurationData  : IRunConfigurationData;
  LMonthsEvaporationData : IReservoirEvaporationsData;
begin
  inherited PopulateDataViewer;
  try
    ClearDataViewer;
    DoContextValidation(dvtReservoirEvaporationAll);
    LReservoirData := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.ReservoirList.ReservoirByIdentifier[FIdentifier];
    if (LReservoirData <> nil) then
    begin
      LMonthsEvaporationData := LReservoirData.ReservoirEvaporationsData;
      if (LMonthsEvaporationData <> nil) then
      begin
        LRunConfigurationData := TYieldModelDataObject(FAppModules.Model.ModelData).RunConfigurationData;
        if Assigned(LRunConfigurationData) then
        begin
          with ReservoirEvaporationDialog do
          begin
            //lValue := 0.0;
            lTotal := 0.0;
            for LCount := 1 to 12 do
            begin
              lFieldIndex := IntToStr(LCount);
              lFieldProperty := GrdEvaporation.FieldProperty(1);
              lKeyValues := LMonthsEvaporationData.GetKeyValues(lFieldProperty.FieldName,lFieldIndex);
              GrdEvaporation.HasMetaData[1, LCount] :=
                FAppModules.MetaData.FindMetaData(lFieldProperty.FieldName,lKeyValues,lFieldIndex) <> nil;

              ReservoirEvaporationDialog.GrdEvaporation.Cells[0,LCount] := LRunConfigurationData.MonthNameByIndex[LCount];
              if(LMonthsEvaporationData.MonthlyEvaporationsByIndex[LCount] = NullFloat) then
                ReservoirEvaporationDialog.GrdEvaporation.Cells[1,LCount] := ''
              else
              begin
                lFieldProperty := FAppModules.FieldProperties.FieldProperty('Evaporation');
                ReservoirEvaporationDialog.GrdEvaporation.AddFieldProperty(lFieldProperty);
                ReservoirEvaporationDialog.GrdEvaporation.Cells[1,LCount] := Format(lFieldProperty.FormatStringGrid, [LMonthsEvaporationData.MonthlyEvaporationsByIndex[LCount]]);
                lValue := LMonthsEvaporationData.MonthlyEvaporationsByIndex[LCount];
                lTotal := lTotal + lValue;
                ReservoirEvaporationDialog.GrdEvaporation.Cells[1,13] :=Format(lFieldProperty.FormatStringGrid, [lTotal]);
              end;
            end;
          end;

          ReservoirEvaporationDialog.GrdEvaporation.IsRowEnabled[13] := False;

          if PopulateReservoirEvaporations(LReservoirData) then
            ReservoirEvaporationDialog.Populated := True;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirEvaporationValidator.SaveState: boolean;
const OPNAME = 'TReservoirEvaporationValidator.SaveState';
begin
  Result := inherited SaveState;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirEvaporationValidator.OnEditControlEnter(Sender: TObject);
const OPNAME = 'TReservoirEvaporationValidator.OnEditControlEnter';
begin
  inherited OnEditControlEnter(Sender);
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirEvaporationValidator.OnEditControltExit(Sender: TObject);
const OPNAME = 'TReservoirEvaporationValidator.OnEditControltExit';
begin
  inherited OnEditControltExit(Sender);
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirEvaporationValidator.OnStringGridCellDataHasChanged(ASender: TObject; ACol, ARow: integer);
const OPNAME = 'TReservoirEvaporationValidator.OnStringGridCellDataHasChanged';
begin
  inherited OnStringGridCellDataHasChanged(ASender,ACol, ARow);
  try
    if((ASender =  ReservoirEvaporationDialog.GrdEvaporation) and (ARow < 13)) then
       UpdateEvaporation(ARow,ReservoirEvaporationDialog.GrdEvaporation.Cells[ACol, ARow]);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirEvaporationValidator.ReservoirEvaporationDialog: TReservoirEvaporationDialog;
const OPNAME = 'TReservoirEvaporationValidator.ReservoirEvaporationDialog';
begin
  Result := Nil;
  try
    if Assigned(FPanel) then
      Result := TReservoirEvaporationDialog(FPanel);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirEvaporationValidator.StudyDataHasChanged(AContext: TChangeContext; AFieldName,AOldValue,ANewValue: string): boolean;
const OPNAME = 'TReservoirEvaporationValidator.StudyDataHasChanged';
begin
  Result := inherited StudyDataHasChanged(AContext,AFieldName,AOldValue,ANewValue);
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirEvaporationValidator.StudyHasChanged: boolean;
const OPNAME = 'TReservoirEvaporationValidator.StudyHasChanged';
begin
  Result := inherited StudyHasChanged;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirEvaporationValidator.PopulateReservoirEvaporations(AReservoirData : IReservoirData) : boolean;
const OPNAME = 'TReservoirEvaporationValidator.PopulateReservoirEvaporations';
var
  LIndex                 : integer;
  LSeries                : TCustomBarSeries;
  LRunConfigurationData  : IRunConfigurationData;
  LMonthsEvaporationData : IReservoirEvaporationsData;
begin
  Result := False;
  try
    LMonthsEvaporationData := AReservoirData.ReservoirEvaporationsData;
    LRunConfigurationData := TYieldModelDataObject(FAppModules.Model.ModelData).RunConfigurationData;

    ReservoirEvaporationDialog.ChtEvaporation.AllowPanning := pmBoth;  // possibly disable to allow dragging of bars
    ReservoirEvaporationDialog.ChtEvaporation.AllowZoom := True;        // possibly disable to allow dragging of bars
    //ReservoirEvaporationDialog.ChtEvaporation.BackWall.Brush.Color := clWhite;
    //ReservoirEvaporationDialog.ChtEvaporation.BackWall.Brush.Style := bsClear;
    //ReservoirEvaporationDialog.ChtEvaporation.BackWall.Pen.Visible := False;
    ReservoirEvaporationDialog.ChtEvaporation.Title.Text.Text := '';
    ReservoirEvaporationDialog.ChtEvaporation.AxisVisible := True;
    ReservoirEvaporationDialog.ChtEvaporation.ClipPoints := True;
    ReservoirEvaporationDialog.ChtEvaporation.Frame.Visible := True;
    ReservoirEvaporationDialog.ChtEvaporation.Legend.Visible := False;
    ReservoirEvaporationDialog.ChtEvaporation.View3D := False;
    //ReservoirEvaporationDialog.ChtEvaporation.View3DWalls := False;
    ReservoirEvaporationDialog.ChtEvaporation.RemoveAllSeries;
    ReservoirEvaporationDialog.ChtEvaporation.SeriesList.Clear;

    ReservoirEvaporationDialog.ChtEvaporation.LeftAxis.Title.Caption := FAppModules.Language.GetString('TField.ReservoirEvap');
    ReservoirEvaporationDialog.ChtEvaporation.LeftAxis.Title.Angle := 90;
    ReservoirEvaporationDialog.ChtEvaporation.BottomAxis.Title.Caption := FAppModules.Language.GetString('TField.Month');

    // Series
    LSeries := TBarSeries.Create(ReservoirEvaporationDialog.ChtEvaporation);
    LSeries.ParentChart := ReservoirEvaporationDialog.ChtEvaporation;
    LSeries.Marks.Visible := False;
    LSeries.SeriesColor := clRed;
    LSeries.Title := FAppModules.Language.GetString('ReservoirSeries.EvaporationSeries');
    LSeries.BarWidthPercent := 80;
    LSeries.XValues.Order := loAscending;
    LSeries.YValues.Order := loNone;

   for LIndex := 1 to 12 do
    begin
      if(LMonthsEvaporationData.MonthlyEvaporationsByIndex[LIndex] = NullFloat) then
        LSeries.Add(0.0, LRunConfigurationData.MonthNameByIndex[LIndex])
      else
      LSeries.Add(LMonthsEvaporationData.MonthlyEvaporationsByIndex[LIndex], LRunConfigurationData.MonthNameByIndex[LIndex]);
    end;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirEvaporationValidator.UpdateEvaporation(AMonthNumber: integer; ANewValue: string);
const OPNAME = 'TReservoirEvaporationValidator.UpdateEvaporation';
var
  LMonthsEvaporationData: IReservoirEvaporationsData;
  LReservoirData :IReservoirData;
  LEvapValue: double;
begin
  try
    LEvapValue := StrToFloat(ANewValue);
    LReservoirData := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.ReservoirList.ReservoirByIdentifier[FIdentifier];
    if (LReservoirData <> nil) then
    begin
      LMonthsEvaporationData := LReservoirData.ReservoirEvaporationsData;
      if (LMonthsEvaporationData <> nil) then
      begin
        LMonthsEvaporationData.MonthlyEvaporationsByIndex[AMonthNumber] := LEvapValue;
        ReservoirEvaporationDialog.GrdEvaporation.Cells[1,AMonthNumber] := FloatToStr(LMonthsEvaporationData.MonthlyEvaporationsByIndex[AMonthNumber]);
        PopulateReservoirEvaporations(LReservoirData);
        PopulateDataViewer;
        DoContextValidation(dvtReservoirEvaporation);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirEvaporationValidator.CancelDragMode;
const OPNAME = 'TReservoirEvaporationValidator.CancelDragMode';
begin
  try
    FInDragMode  := False;
    FSeriesPointIndex := -1;
    FSeries      := nil;
    FStartYPosition  := 0.0;
    FStartYValue     := 0.0;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirEvaporationValidator.OnChartClickSeries(Sender: TCustomChart; Series: TChartSeries;
          ValueIndex: Integer; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
const OPNAME = 'TReservoirEvaporationValidator.OnChartClickSeries';
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

procedure TReservoirEvaporationValidator.OnMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
const OPNAME = 'TReservoirEvaporationValidator.OnMouseMove';
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
        ReservoirEvaporationDialog.GrdEvaporation.AddFieldProperty(lFieldProperty);
        ReservoirEvaporationDialog.GrdEvaporation.Cells[1,FSeriesPointIndex+1] :=Format(lFieldProperty.FormatStringGrid, [FSeries.YValues.Value[FSeriesPointIndex]]);
          //FormatFloat('####0.0###', FSeries.YValues.Value[FSeriesPointIndex]);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirEvaporationValidator.OnMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: integer);
const OPNAME = 'TReservoirEvaporationValidator.OnMouseUp';
var
  LYChange,
  LYValue: Double;
begin
  try
    if FInDragMode then
    begin
      LYValue  := FSeries.YScreenToValue(Y);
      LYChange := LYValue - FStartYPosition;
      if(LYChange > 0.001) or (LYChange < -0.001)then
      UpdateEvaporation(FSeriesPointIndex + 1,ReservoirEvaporationDialog.GrdEvaporation.Cells[1,FSeriesPointIndex+1]);
      CancelDragMode;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


procedure TReservoirEvaporationValidator.OnKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
const OPNAME = 'TReservoirEvaporationValidator.OnKeyDown';
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

procedure TReservoirEvaporationValidator.ValidateEvaporation(AReservoir: IReservoirData);
const OPNAME = 'TReservoirEvaporationValidator.ValidateEvaporation';
var
  lCol           : integer;
  lIndex         : integer;
  lErrorCols     : TStringList;
  lErrorMessages : TStringList;
begin
  try
    if (AReservoir <> nil) then
    begin
      with ReservoirEvaporationDialog do
      begin
        lErrorCols     := TStringList.Create;
        lErrorMessages := TStringList.Create;
        try
          FErrorMessage := '';
          lErrorCols.Clear;
          if (AReservoir.Validate(FErrorMessage,'Evaporation')) then
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

procedure TReservoirEvaporationValidator.DoContextValidation(AValidationtype: TDialogValidationType);
const OPNAME = 'TReservoirEvaporationValidator.DoContextValidation';
var
  lReservoir    : IReservoirData;
  lReservoirList: IReservoirDataList;
begin
  try
    FAllErrorMessages.Clear;
    if (FIdentifier >= 0) then
    begin
      lReservoirList := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.
                        ReservoirList;
      lReservoir := lReservoirList.ReservoirByIdentifier[FIdentifier];
      if (lReservoir <> nil) then
      begin
        if (AValidationType in [dvtReservoirEvaporationAll, dvtReservoirEvaporation]) then
          ValidateEvaporation(lReservoir);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirEvaporationValidator.DetermineWizardStatus (ASequence : integer = 0) : integer;
const OPNAME = 'TReservoirEvaporationValidator.DetermineWizardStatus';
var
  lReservoir     : IReservoirData;
  lReservoirList : IReservoirDataList;
  lMonth         : integer;
  lCheck         : Boolean;
  lProperty      : TAbstractFieldProperty;
begin
  Result := inherited DetermineWizardStatus(ASequence);
  try
    if (FIdentifier >= 0) then
    begin
      try
        lReservoirList := TYieldModelDataObject(FAppModules.Model.ModelData).
                            NetworkElementData.ReservoirList;
        lReservoir     := lReservoirList.ReservoirOrNodeByIdentifier[FIdentifier];
        if (lReservoir <> nil) then
        begin
          DoContextValidation(dvtReservoirEvaporationAll);
          lProperty := FAppModules.FieldProperties.FieldProperty('Evaporation');
          lCheck    := FALSE;
          lMonth    := lProperty.ArrayLow;
          while ((NOT lCheck) AND (lMonth <= lProperty.ArrayHigh)) do
          begin
            if (lReservoir.ReservoirEvaporationsData.MonthlyEvaporationsByIndex[lMonth] <> NullFloat) then
              lCheck := TRUE
            else
              lMonth := lMonth + 1;
          end;
          if (lCheck) then
          begin
            Result := 1;
            if (FAllErrorMessages.Count = 0) then
              Result := 2;
          end;
        end;
      finally
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirEvaporationValidator.ProcessMetaDataEvent: boolean;
const OPNAME = 'TReservoirEvaporationValidator.ProcessMetaDataEvent';
var
  lFieldIndex    : string;
  lKeyValues     : string;
  lReservoir     : IReservoirData;
  lFieldProperty : TAbstractFieldProperty;
begin
  Result := FALSE;
  try
    if (FPanel.Visible AND (FActiveControl <> nil)) then
    begin
      lReservoir := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.ReservoirList.ReservoirByIdentifier[FIdentifier];
      if (lReservoir <> nil) then
      begin
        with ReservoirEvaporationDialog do
        begin
          lFieldIndex         := '';
          lFieldProperty      := nil;
          if (FActiveControl  = GrdEvaporation) then
          begin
            lFieldIndex := IntToStr(GrdEvaporation.Row);
            lFieldProperty := GrdEvaporation.FieldProperty(GrdEvaporation.Col);
          end;

          if (lFieldProperty <> nil) then
          begin
            lKeyValues := lReservoir.GetKeyValues(lFieldProperty.FieldName, lFieldIndex);
            FAppModules.MetaData.ShowMetaData(lFieldProperty.FieldName, lKeyValues, lFieldIndex);
            PopulateDataViewer;
            Result := TRUE;
          end;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TReservoirEvaporationValidator.OnAfterPasteColumnData(Sender: TObject);
const OPNAME = 'TReservoirEvaporationValidator.OnAfterPasteColumnData';
var
  LMonthsEvaporationData: IReservoirEvaporationsData;
  LReservoirData :IReservoirData;
  LEvapValue: double;
  LMonth    : integer;
begin
  try
    LReservoirData := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.ReservoirList.ReservoirByIdentifier[FIdentifier];
    if(LReservoirData <> nil) then
    begin
      LMonthsEvaporationData := LReservoirData.ReservoirEvaporationsData;
      if(LMonthsEvaporationData <> nil) then
      begin
        for LMonth := 1 to 12 do
        begin
          LEvapValue := StrToFloat(ReservoirEvaporationDialog.GrdEvaporation.Cells[1,LMonth]);
          LMonthsEvaporationData.MonthlyEvaporationsByIndex[LMonth] := LEvapValue;
        end;
        PopulateDataViewer;
        DoContextValidation(dvtReservoirEvaporation);
      end;
    end;
    PopulateDataViewer;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

end.

