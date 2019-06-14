//
//
//  UNIT      : Contains the class TYMDemandCentreEvaporationValidator.
//  AUTHOR    : Maurice Marinus
//  DATE      : 2006/11/16
//  COPYRIGHT : Copyright © 2006 DWAF
//
//
unit UYMDemandCentreEvaporationValidator;

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
  UYMDemandCentre,
  UYMDemandCentreEvaporationDialog;

type
  TYMDemandCentreEvaporationValidator = class(TAbstractYieldDataDialogValidator)
  protected
    FInDragMode       : boolean;
    FSeriesPointIndex : Longint;
    FSeries           : TBarSeries;
    FStartYValue,
    FStartYPosition   : double;
    procedure CreateMemberObjects;  override;
    procedure DestroyMemberObjects; override;
    procedure OnEditControlEnter(Sender: TObject); override;
    procedure OnEditControltExit(Sender: TObject); override;
    procedure OnStringGridCellDataHasChanged(ASender: TObject; ACol, ARow: integer);  override;
    procedure OnAfterPasteColumnData(Sender: TObject); 

    procedure UpdateEvaporation(AIndex: Integer; AValue: string);
    function PopulateEvaporations (AYMDemandCentre: IYMDemandCentre) : boolean;
    procedure OnChartClickSeries(Sender: TCustomChart; Series: TChartSeries; ValueIndex: LongInt;
              Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure OnMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: integer);
    procedure OnMouseMove(Sender: TObject; Shift: TShiftState; X, Y: integer);
    procedure OnKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure CancelDragMode;
    procedure ValidateEvaporation(AYMDemandCentre: IYMDemandCentre);
  public
    function Initialise           : boolean; override;
    function SaveState            : boolean; override;
    function LanguageHasChanged   : boolean; override;
    function StudyHasChanged      : boolean; override;
    function StudyDataHasChanged(AContext: TChangeContext; AFieldName,AOldValue,ANewValue: string): boolean; override;
    function ProcessMetaDataEvent : boolean; override;

    procedure ClearDataViewer;    override;
    procedure PopulateDataViewer; override;

    function YMDemandCentreEvaporationDialog: TYMDemandCentreEvaporationDialog;
    procedure DoContextValidation(AValidationtype: TDialogValidationType);  override;
 end;

implementation

uses

  // Delphi VCL, RTL, etc

  SysUtils,
  VCLTee.TeeProcs,
  VCL.Graphics,
  windows,
  UUtilities,

  UConstants,
  UYieldModelDataObject,
  URunConfigurationData,
  UErrorHandlingOperations;


{ TYMDemandCentreEvaporationValidator }

procedure TYMDemandCentreEvaporationValidator.CancelDragMode;
const OPNAME = 'TYMDemandCentreEvaporationValidator.CancelDragMode';
begin
  try
    FInDragMode  := False;
    FSeriesPointIndex := -1;
    FSeries      := nil;
    FStartYPosition  := 0.0;
    FStartYValue     := 0.0;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYMDemandCentreEvaporationValidator.ClearDataViewer;
const OPNAME = 'TYMDemandCentreEvaporationValidator.ClearDataViewer';
var
  LIndex : integer;
begin
  inherited ClearDataViewer;
  try
    for LIndex := 1 to 13 do
      YMDemandCentreEvaporationDialog.GrdEvaporation.Cells[1,LIndex] := '';
    YMDemandCentreEvaporationDialog.ChtEvaporation.SeriesList.Clear;
    YMDemandCentreEvaporationDialog.ChtEvaporation.UndoZoom;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYMDemandCentreEvaporationValidator.CreateMemberObjects;
const OPNAME = 'TYMDemandCentreEvaporationValidator.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
    CancelDragMode;

    FPanel := TYMDemandCentreEvaporationDialog.Create(FPanelOwner,FAppModules);

    with YMDemandCentreEvaporationDialog do
    begin
//      GrdEvaporation.AddFieldProperty(fappModules.FieldProperties.FieldProperty('EvapoTranspiration'));
      GrdEvaporation.OnEnter    := OnEditControlEnter;
      GrdEvaporation.OnExit     := OnEditControltExit;
      GrdEvaporation.OnColEnter := OnStringGridColEnter;
      GrdEvaporation.OnBeforeCellChange := OnStringGridCellDataHasChanged;
      GrdEvaporation.OnKeyDown := Self.OnKeyDown;
      GrdEvaporation.OnAfterPasteColumnData := Self.OnAfterPasteColumnData;
      GrdEvaporation.ShowGridPopupMenu      := True;
      GrdEvaporation.AllowPasteFromExcel    := True;
      GrdEvaporation.OnPasteFromExcel       := Self.OnAfterPasteColumnData;
    end;

    if(FAppModules.User.UserRights in CUR_EditData) and
      (not FAppModules.StudyArea.ScenarioLocked) then
    begin
      YMDemandCentreEvaporationDialog.ChtEvaporation.OnClickSeries := Self.OnChartClickSeries;
      YMDemandCentreEvaporationDialog.ChtEvaporation.OnMouseMove   := Self.OnMouseMove;
      YMDemandCentreEvaporationDialog.ChtEvaporation.OnMouseUp     := Self.OnMouseUp;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;    
end;

procedure TYMDemandCentreEvaporationValidator.DestroyMemberObjects;
const OPNAME = 'TYMDemandCentreEvaporationValidator.DestroyMemberObjects';
begin
  try
    inherited DestroyMemberObjects;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYMDemandCentreEvaporationValidator.DoContextValidation(AValidationtype: TDialogValidationType);
const OPNAME = 'TYMDemandCentreEvaporationValidator.DoContextValidation';
var
  LYMDemandCentre  : IYMDemandCentre;
begin
  try
    LYMDemandCentre := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.YMDemandCentreList.YMDemandCentreByID[FIdentifier];
    if (LYMDemandCentre <> nil) then
    begin
      case AValidationType of
        dvtIrrigationBlockAll:
          begin
            ValidateEvaporation(LYMDemandCentre);
          end;
        dvtYMDemandCentreEvapoTranspiration        :  ValidateEvaporation(LYMDemandCentre);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYMDemandCentreEvaporationValidator.Initialise: boolean;
const OPNAME = 'TYMDemandCentreEvaporationValidator.Initialise';
begin
  Result := inherited Initialise;
  try
    CancelDragMode;
    with YMDemandCentreEvaporationDialog do
    begin
{      GrdEvaporation.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('Month'));
      GrdEvaporation.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('EvapoTranspiration'));}
      GrdEvaporation.OnColEnter(YMDemandCentreEvaporationDialog.GrdEvaporation,1,1);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYMDemandCentreEvaporationValidator.LanguageHasChanged: boolean;
const OPNAME = 'TYMDemandCentreEvaporationValidator.LanguageHasChanged';
begin
  Result := False;
  try
    TabShetCaption := 'Evaporation';
    Result := inherited LanguageHasChanged;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYMDemandCentreEvaporationValidator.OnAfterPasteColumnData(Sender: TObject);
const OPNAME = 'TYMDemandCentreEvaporationValidator.OnAfterPasteColumnData';
var
  LYMDemandCentre : IYMDemandCentre;
  LMonth          : integer;
  LValue          : double;
begin
  try
    LYMDemandCentre := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.YMDemandCentreList.YMDemandCentreByID[FIdentifier];
    if(LYMDemandCentre <> nil) then
    begin
      for LMonth := 1 to 12 do
      begin
        LValue := StrToFloat(Trim(YMDemandCentreEvaporationDialog.GrdEvaporation.Cells[1,LMonth]));
        LYMDemandCentre.EvapoTranspiration[LMonth] := LValue;
      end;
      PopulateDataViewer;
      DoContextValidation (dvtYMDemandCentreEvapoTranspiration);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYMDemandCentreEvaporationValidator.OnChartClickSeries(
  Sender: TCustomChart; Series: TChartSeries; ValueIndex: Integer;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
const OPNAME = 'TYMDemandCentreEvaporationValidator.OnChartClickSeries';
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

procedure TYMDemandCentreEvaporationValidator.OnEditControlEnter(Sender: TObject);
const OPNAME = 'TYMDemandCentreEvaporationValidator.OnEditControlEnter';
begin
  inherited OnEditControlEnter(Sender);
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYMDemandCentreEvaporationValidator.OnEditControltExit(Sender: TObject);
const OPNAME = 'TYMDemandCentreEvaporationValidator.OnEditControltExit';
begin
  inherited OnEditControltExit(Sender);
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYMDemandCentreEvaporationValidator.OnKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
const OPNAME = 'TYMDemandCentreEvaporationValidator.OnKeyDown';
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

procedure TYMDemandCentreEvaporationValidator.OnMouseMove(Sender: TObject; Shift: TShiftState; X, Y: integer);
const OPNAME = 'TYMDemandCentreEvaporationValidator.OnMouseMove';
var
  LYChange,
  LYValue: Double;
begin
  try
    if (ssLeft in Shift) and FInDragMode then
    begin
      LYValue  := FSeries.YScreenToValue(Y);
      if(LYValue >= 0.0) then
      begin
        LYChange := LYValue - FStartYPosition;
        FSeries.YValues.Value[FSeriesPointIndex] := FStartYValue + LYChange;
        FSeries.Repaint;
        YMDemandCentreEvaporationDialog.GrdEvaporation.Cells[1,FSeriesPointIndex+1] :=
          FormatFloat('####0.0#', FSeries.YValues.Value[FSeriesPointIndex]);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYMDemandCentreEvaporationValidator.OnMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: integer);
const OPNAME = 'TYMDemandCentreEvaporationValidator.OnMouseUp';
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
        UpdateEvaporation(FSeriesPointIndex + 1, YMDemandCentreEvaporationDialog.GrdEvaporation.Cells[1,FSeriesPointIndex+1]);
      CancelDragMode;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYMDemandCentreEvaporationValidator.OnStringGridCellDataHasChanged(ASender: TObject; ACol, ARow: integer);
const OPNAME = 'TYMDemandCentreEvaporationValidator.OnStringGridCellDataHasChanged';
begin
  inherited OnStringGridCellDataHasChanged(ASender,ACol, ARow);
  try
    if((ASender =  YMDemandCentreEvaporationDialog.GrdEvaporation) and (ARow < 13)) then
       UpdateEvaporation(ARow, YMDemandCentreEvaporationDialog.GrdEvaporation.Cells[ACol, ARow]);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYMDemandCentreEvaporationValidator.PopulateDataViewer;
const OPNAME = 'TYMDemandCentreEvaporationValidator.PopulateDataViewer';
var
  lFieldIndex            : string;
  LCount                 : integer;
  lTotal                 : double;
  lValue                 : double;
  lFieldProperty         : TAbstractFieldProperty;
  LRunConfigurationData  : IRunConfigurationData;
{  LMonthsEvaporationData : IReservoirEvaporationsData;
  LReservoirData         : IReservoirData;}
  LYMDemandCentre        : IYMDemandCentre;
begin
  inherited PopulateDataViewer;
  try
    ClearDataViewer;
    DoContextValidation(dvtYMDemandCentreEvapoTranspiration);

    LYMDemandCentre := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.YMDemandCentreList.YMDemandCentreByID[FIdentifier];
    if (LYMDemandCentre <> nil) then
    begin
      LRunConfigurationData := TYieldModelDataObject(FAppModules.Model.ModelData).RunConfigurationData;
      if Assigned(LRunConfigurationData) then
      begin
        with YMDemandCentreEvaporationDialog do
        begin
          //lValue := 0.0;
          lTotal := 0.0;
          for LCount := 1 to 12 do
          begin
            lFieldIndex     := IntToStr(LCount);

            LFieldProperty  := FAppModules.FieldProperties.FieldProperty('EvapoTranspiration');
            GrdEvaporation.AddFieldProperty(lFieldProperty);
            GrdEvaporation.Cells[0,LCount] := LRunConfigurationData.MonthNameByIndex[LCount];
            if(LYMDemandCentre.EvapoTranspiration[LCount] = NullFloat) then
              GrdEvaporation.Cells[1,LCount]  := ''
            else
            begin
              GrdEvaporation.Cells[1,LCount]  := FormatFloat('####0.0###', LYMDemandCentre.EvapoTranspiration[LCount]);
              lValue                          := LYMDemandCentre.EvapoTranspiration[LCount];
              lTotal                          := lTotal + lValue;
              GrdEvaporation.Cells[1,13]      := Format('%6.2f', [lTotal]);
            end;
          end;
        end;

        YMDemandCentreEvaporationDialog.GrdEvaporation.IsRowEnabled[13] := False;

        if PopulateEvaporations(LYMDemandCentre) then
          YMDemandCentreEvaporationDialog.Populated := True;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYMDemandCentreEvaporationValidator.PopulateEvaporations(AYMDemandCentre: IYMDemandCentre): boolean;
const OPNAME = 'TYMDemandCentreEvaporationValidator.PopulateEvaporations';
var
  LIndex                 : integer;
  LSeries                : TCustomBarSeries;
  LRunConfigurationData  : IRunConfigurationData;
begin
  Result := False;
  try
    LRunConfigurationData   := TYieldModelDataObject(FAppModules.Model.ModelData).RunConfigurationData;
    with YMDemandCentreEvaporationDialog do
    begin
      ChtEvaporation.AllowPanning     := pmBoth;  // possibly disable to allow dragging of bars
      ChtEvaporation.AllowZoom        := True;    // possibly disable to allow dragging of bars
      ChtEvaporation.Title.Text.Text  := '';
      ChtEvaporation.AxisVisible      := True;
      ChtEvaporation.ClipPoints       := True;
      ChtEvaporation.Frame.Visible    := True;
      ChtEvaporation.Legend.Visible   := False;
      ChtEvaporation.View3D           := False;
      ChtEvaporation.RemoveAllSeries;
      ChtEvaporation.SeriesList.Clear;

      ChtEvaporation.LeftAxis.Title.Caption   := FAppModules.Language.GetString('TField.ReservoirEvap');
      ChtEvaporation.LeftAxis.Title.Angle     := 90;
      ChtEvaporation.BottomAxis.Title.Caption := FAppModules.Language.GetString('TField.Month');
    end;
    // Series
    LSeries                 := TBarSeries.Create(YMDemandCentreEvaporationDialog.ChtEvaporation);
    LSeries.ParentChart     := YMDemandCentreEvaporationDialog.ChtEvaporation;
    LSeries.Marks.Visible   := False;
    LSeries.SeriesColor     := clRed;
    LSeries.Title           := FAppModules.Language.GetString('ReservoirSeries.EvaporationSeries');
    LSeries.BarWidthPercent := 80;
    LSeries.XValues.Order   := loAscending;
    LSeries.YValues.Order   := loNone;

   for LIndex := 1 to 12 do
    begin
      if(AYMDemandCentre.EvapoTranspiration[LIndex] = NullFloat) then
        LSeries.Add(0.0, LRunConfigurationData.MonthNameByIndex[LIndex])
      else
      LSeries.Add(AYMDemandCentre.EvapoTranspiration[LIndex], LRunConfigurationData.MonthNameByIndex[LIndex]);
    end;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYMDemandCentreEvaporationValidator.ProcessMetaDataEvent: boolean;
const OPNAME = 'TYMDemandCentreEvaporationValidator.ProcessMetaDataEvent';
{var
  lFieldIndex    : string;
  lKeyValues     : string;
  lReservoir     : IReservoirData;
  lFieldProperty : TAbstractFieldProperty;}
begin
  Result := FALSE;
  {try
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
  except on E: Exception do HandleError(E, OPNAME); end;}
end;

function TYMDemandCentreEvaporationValidator.SaveState: boolean;
const OPNAME = 'TYMDemandCentreEvaporationValidator.SaveState';
begin
  Result := inherited SaveState;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYMDemandCentreEvaporationValidator.StudyDataHasChanged(
  AContext: TChangeContext; AFieldName, AOldValue,
  ANewValue: string): boolean;
const OPNAME = 'TYMDemandCentreEvaporationValidator.StudyDataHasChanged';
begin
  Result := inherited StudyDataHasChanged(AContext,AFieldName,AOldValue,ANewValue);
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYMDemandCentreEvaporationValidator.StudyHasChanged: boolean;
const OPNAME = 'TYMDemandCentreEvaporationValidator.StudyHasChanged';
begin
  Result := inherited StudyHasChanged;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYMDemandCentreEvaporationValidator.UpdateEvaporation(AIndex: Integer; AValue: string);
const OPNAME = 'TYMDemandCentreEvaporationValidator.UpdateEvaporation';
var
  LYMDemandCentre : IYMDemandCentre; 
  LMessage        : string;
begin
  try
    LYMDemandCentre := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.YMDemandCentreList.YMDemandCentreByID[FIdentifier];
    if (LYMDemandCentre <> nil) then
    begin
      with YMDemandCentreEvaporationDialog do
      begin
        GrdEvaporation.ValidationError[1,AIndex, gveCellContext] :='';
        if (FAppModules.FieldProperties.ValidateFieldProperty(
            TAbstractFieldProperty(GrdEvaporation.FieldProperty(AIndex-1)).FieldName,
            AValue,
            LMessage,
            AIndex)) then
        begin
          LYMDemandCentre.EvapoTranspiration[AIndex] := StrToFloat(AValue);
          PopulateDataViewer;
          DoContextValidation (dvtYMDemandCentreEvapoTranspiration);
        end else
          GrdEvaporation.ValidationError[1,AIndex, gveCellContext] := LMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYMDemandCentreEvaporationValidator.ValidateEvaporation(AYMDemandCentre: IYMDemandCentre);
const OPNAME = 'TYMDemandCentreEvaporationValidator.ValidateEvaporation';
var
  lCol           : integer;
  lIndex         : integer;
  lErrorCols     : TStringList;
  lErrorMessages : TStringList;
begin
  try
    if (AYMDemandCentre <> nil) then
    begin
      with YMDemandCentreEvaporationDialog do
      begin
        lErrorCols     := TStringList.Create;
        lErrorMessages := TStringList.Create;
        try
          FErrorMessage := '';
          lErrorCols.Clear;
          if (AYMDemandCentre.Validate(FErrorMessage,'EvapoTranspiration')) then
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

function TYMDemandCentreEvaporationValidator.YMDemandCentreEvaporationDialog: TYMDemandCentreEvaporationDialog;
const OPNAME = 'TYMDemandCentreEvaporationValidator.YMDemandCentreEvaporationDialog';
begin
  Result := Nil;
  try
    if Assigned(FPanel) then
      Result := TYMDemandCentreEvaporationDialog(FPanel);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
