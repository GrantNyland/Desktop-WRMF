//
//
//  UNIT      : Contains the class TSystemYieldDialog.
//  AUTHOR    : Sam Dhlamini(RIVIA)
//  DATE      : 2005/02/15
//  COPYRIGHT : Copyright © 2003 DWAF
//
//
unit USystemYieldValidator;

interface

uses
  Classes,
  VCL.Controls,
  VCL.ComCtrls,
  VCL.StdCtrls,
  VCL.ExtCtrls,
  Types,
  VCL.Grids,
  UAbstractObject,
  UAbstractComponent,
  UDataComponent,
  VoaimsCom_TLB,
  UDataEditComponent,
  UYieldContextValidationType,
  USystemYieldDialog;

type
  TRequiredMode = (rmNone,rmHistoric, rmStochastic);
  TSystemYieldValidator = class(TAbstractDataDialogValidator)
  protected
    FRequiredMode : TRequiredMode;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    procedure OnEditControlEnter(Sender: TObject); override;
    procedure OnEditControltExit(Sender: TObject); override;
    procedure OnStringGridCellDataHasChanged ( ASender: TObject; ACol, ARow: integer ); override;
    procedure OnStringGridSelectCell(Sender: TObject; ACol, ARow: Longint; var CanSelect: Boolean);
    procedure RePopulateDataViewer;
    procedure PopulateHistoricGrid;
    procedure PopulateStochasticGrds;
    procedure SetGridFields ( aFieldStringGrid : TFieldStringGrid );
    procedure DoOnDrawCell ( Sender: TObject; ACol, ARow : Longint; Rect : TRect; State : TGridDrawState );
  public
    function Initialise: boolean; override;
    function SaveState: boolean; override;
    function LanguageHasChanged: boolean; override;
    function StudyHasChanged: boolean; override;
    function StudyDataHasChanged(AContext: TChangeContext; AFieldName,AOldValue,ANewValue: string): boolean; override;
    procedure ClearDataViewer; override;
    procedure PopulateDataViewer; override;
    function SystemYieldDialog: TSystemYieldDialog;
    property RequiredMode: TRequiredMode read FRequiredMode write FRequiredMode;
    
    function CanExport : Boolean; override;
    procedure DoExport(AFileName: string = ''); override;
  end;

implementation

uses
  System.UITypes,
  SysUtils,
  UConstants,
  VCL.Graphics,
  UYieldModelDataObject,
  URunConfigurationData,
  USystemYieldObject,
  UErrorHandlingOperations;

{ TSystemYieldValidator }

procedure TSystemYieldValidator.CreateMemberObjects;
const OPNAME = 'TSystemYieldValidator.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
    FRequiredMode := rmNone;
    FPanel := TSystemYieldDialog.Create(FPanelOwner,FAppModules);

    SystemYieldDialog.HistoricGrid.AddFieldProperty ( FAppModules.FieldProperties.FieldProperty ( 'SumOutTargetDraft' ) );
    SystemYieldDialog.HistoricGrid.AddFieldProperty ( FAppModules.FieldProperties.FieldProperty ( 'SumOutDeficitPropotion' ) );
    SystemYieldDialog.HistoricGrid.AddFieldProperty ( FAppModules.FieldProperties.FieldProperty ( 'SumOutYield' ) );

    SystemYieldDialog.HistoricGrid.Cells[0, 0] := FAppModules.Language.GetString('TField.SumOutTargetDraft');
    SystemYieldDialog.HistoricGrid.Cells[1, 0] := FAppModules.Language.GetString('TField.SumOutDeficitPropotion');
    SystemYieldDialog.HistoricGrid.Cells[2, 0] := FAppModules.Language.GetString('TField.SumOutYield');

    SystemYieldDialog.HistoricGrid.OnBeforeCellChange    := OnStringGridCellDataHasChanged;
    SystemYieldDialog.HistoricGrid.OnSelectCell          := OnStringGridSelectCell;
    SystemYieldDialog.HistoricGrid.OnColEnter            := OnStringGridColEnter;

    SystemYieldDialog.StochasticLongTermGrid.AddFieldProperty ( FAppModules.FieldProperties.FieldProperty ( 'SumOutTargetDraft' ) );
    SystemYieldDialog.StochasticLongTermGrid.AddFieldProperty ( FAppModules.FieldProperties.FieldProperty ( 'SumOutNoOfFailureSequences' ) );
    SystemYieldDialog.StochasticLongTermGrid.AddFieldProperty ( FAppModules.FieldProperties.FieldProperty ( 'SumOutReliabilityOfSupply' ) );
    SystemYieldDialog.StochasticLongTermGrid.AddFieldProperty ( FAppModules.FieldProperties.FieldProperty ( 'SumOutRecurrenceIntervalOfFailure' ) );

    SystemYieldDialog.StochasticLongTermGrid.Cells[0, 0] := FAppModules.Language.GetString('TField.SumOutTargetDraft');
    SystemYieldDialog.StochasticLongTermGrid.Cells[1, 0] := FAppModules.Language.GetString('TField.SumOutNoOfFailureSequences');
    SystemYieldDialog.StochasticLongTermGrid.Cells[2, 0] := FAppModules.Language.GetString('TField.SumOutReliabilityOfSupply');
    SystemYieldDialog.StochasticLongTermGrid.Cells[3, 0] := FAppModules.Language.GetString('TField.SumOutRecurrenceIntervalOfFailure');

    SystemYieldDialog.StochasticLongTermGrid.OnBeforeCellChange    := OnStringGridCellDataHasChanged;
    SystemYieldDialog.StochasticLongTermGrid.OnSelectCell          := OnStringGridSelectCell;
    SystemYieldDialog.StochasticLongTermGrid.OnColEnter            := OnStringGridColEnter;
    SystemYieldDialog.StochasticLongTermGrid.OnDrawCell            := DoOnDrawCell;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TSystemYieldValidator.DestroyMemberObjects;
const OPNAME = 'TSystemYieldValidator.DestroyMemberObjects';
begin
  try
    inherited DestroyMemberObjects;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TSystemYieldValidator.Initialise: boolean;
const OPNAME = 'TSystemYieldValidator.Initialise';
begin
  Result := inherited Initialise;
  try
    ClearDataViewer;
    RePopulateDataViewer
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TSystemYieldValidator.LanguageHasChanged: boolean;
const OPNAME = 'TSystemYieldValidator.LanguageHasChanged';
begin
  Result := False;
  try
    TabShetCaption := FAppModules.Language.GetString('TabCaption.SystemYield');
    Result := inherited LanguageHasChanged;
    ClearDataViewer;
    RePopulateDataViewer;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TSystemYieldValidator.ClearDataViewer;
const OPNAME = 'TSystemYieldValidator.ClearDataViewer';
begin
  inherited ClearDataViewer;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TSystemYieldValidator.PopulateDataViewer;
const OPNAME = 'TSystemYieldValidator.PopulateDataViewer';
begin
  inherited PopulateDataViewer;
  try
    ClearDataViewer;
    RePopulateDataViewer;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TSystemYieldValidator.RePopulateDataViewer;
const OPNAME = 'TSystemYieldValidator.RePopulateDataViewer';
begin
  try
    case FRequiredMode of
      rmHistoric   : PopulateHistoricGrid;
      rmStochastic : PopulateStochasticGrds;
    end;//case
    SystemYieldDialog.Resize;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TSystemYieldValidator.PopulateHistoricGrid;
const OPNAME = 'TSystemYieldValidator.PopulateHistoricGrid';
var
  lIndex            : integer;
  lHistoricSysYield : TSystemYieldObject;
  LFieldProperty    : TAbstractFieldProperty;
begin
  try
    SystemYieldDialog.HistoricGrid.Visible := True;
    try
      lHistoricSysYield := TSystemYieldObject.Create ( FAppModules );
      lHistoricSysYield.SystemYieldType := ytHistoric;
      lHistoricSysYield.Initialise;
      if ( lHistoricSysYield <> nil ) then
      begin
        if ( lHistoricSysYield.ErrorMsg <> '' ) then
        begin
          SystemYieldDialog.GroupBox.Visible := False;
          SystemYieldDialog.ShowError ( FAppModules.Language.GetString ( 'TYRCSelectorsPanel.'+ lHistoricSysYield.ErrorMsg ) );
          FreeAndNil ( lHistoricSysYield );
          Exit;
        end;
        SystemYieldDialog.HistoricGrid.RowCount := lHistoricSysYield.HistoricSystemYieldCounter + 1;
        for lIndex := 0 to SystemYieldDialog.HistoricGrid.RowCount do
        begin
          if ( lHistoricSysYield.GetTargetYieldByIndex ( lIndex ) = NullFloat ) or
             ( lHistoricSysYield.GetTargetYieldByIndex ( lIndex ) = 0 ) then
            break;
          LFieldProperty    := FAppModules.FieldProperties.FieldProperty('FlowConstraints');
          SystemYieldDialog.HistoricGrid.Cells [ 0, lIndex + 1 ] := Format(LFieldProperty.FormatStringGrid {'%6.2f'}, [ lHistoricSysYield.GetTargetYieldByIndex ( lIndex ) ] );
          SystemYieldDialog.HistoricGrid.Cells [ 1, lIndex + 1 ] := Format(LFieldProperty.FormatStringGrid{'%6.2f'}, [ lHistoricSysYield.GetDeficitPropotionByIndex ( lIndex ) ] );
          SystemYieldDialog.HistoricGrid.Cells [ 2, lIndex + 1 ] := Format(LFieldProperty.FormatStringGrid{'%6.2f'}, [ lHistoricSysYield.GetSystemYieldByIndex ( lIndex ) ] );
        end;
      end;
    finally
      FreeAndNil ( lHistoricSysYield );
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TSystemYieldValidator.PopulateStochasticGrds;
const OPNAME = 'TSystemYieldValidator.PopulateStochasticGrds';
var
  lSystemYieldObject : TSystemYieldObject;
  lRowCount,
  lGridIndex,
  lIndex2,
  lIndex             : integer;
  lPeriodLength      : TPeriodLength;
  LFieldProperty     : TAbstractFieldProperty;
begin
  try
    try
      lSystemYieldObject := TSystemYieldObject.Create(FAppModules);
      lSystemYieldObject.SystemYieldType := ytStochastic;
      lSystemYieldObject.Initialise;
      lGridIndex := 0;
      for lIndex := 0 to 9 do
      begin
        try
          lPeriodLength := lSystemYieldObject.PriodLengthData[lIndex];
          if (lPeriodLength <> nil) then
          begin
            if lPeriodLength.PeriodCount = 0 then
              break;
            SystemYieldDialog.StochasticLongTermGrid.Visible := True;
            SystemYieldDialog.lblStochastic.Visible := True;
            SystemYieldDialog.lblNumberOfSeqAnalysed.Visible := True;
            if ( lIndex > 0 ) then
              SystemYieldDialog.lblStochastic.Caption := FAppModules.Language.GetString('LabelCaption.MultipleStochasticYieldResult')
            else
            if ( lIndex = 0 ) then
              SystemYieldDialog.lblStochastic.Caption := FAppModules.Language.GetString('LabelCaption.StochasticYieldResult');
            SystemYieldDialog.lblNumberOfSeqAnalysed.Caption := FAppModules.Language.GetString('LabelCaption.NrOfSequencesAnalysed')+
                                                                IntToStr ( lSystemYieldObject.NumberOfSeqAnalysed );
            if ( lIndex > 0 ) then
              lGridIndex := lGridIndex + ( lPeriodLength.RecordCounter + 1 )
            else
              lGridIndex := 2;
            lRowCount := lGridIndex + lPeriodLength.RecordCounter;
            SystemYieldDialog.StochasticLongTermGrid.RowCount := lRowCount;
            for lIndex2 := 0 to lPeriodLength.RecordCounter do
            begin
              if lPeriodLength.TargetDraft [ lIndex2 ] = 0 then
                break;
              if lIndex2 = 0 then
                SystemYieldDialog.StochasticLongTermGrid.Cells [ 0, ( lIndex2 + lGridIndex ) - 1 ] := 'Period Length (years) = ' + IntToStr ( lPeriodLength.FPeriod );
              LFieldProperty    := FAppModules.FieldProperties.FieldProperty('FlowConstraints');
              SystemYieldDialog.StochasticLongTermGrid.Cells [ 0, lIndex2 + lGridIndex ] := Format(LFieldProperty.FormatStringGrid {'%6.2f'}, [ lPeriodLength.TargetDraft [ lIndex2 ] ] );
              SystemYieldDialog.StochasticLongTermGrid.Cells [ 1, lIndex2 + lGridIndex ] := Format('%d', [ Trunc ( lPeriodLength.NoOfFaiureSeq [ lIndex2 ] ) ] );
              SystemYieldDialog.StochasticLongTermGrid.Cells [ 2, lIndex2 + lGridIndex ] := Format('%6f', [ lPeriodLength.ReliabilityOfSupply [ lIndex2  ] ] );
              SystemYieldDialog.StochasticLongTermGrid.Cells [ 3, lIndex2 + lGridIndex ] := Format('%6f', [ lPeriodLength.RecurrenceIntaval [ lIndex2 ] ] );
            end;
         end;
         if ( lPeriodLength.PeriodCount = 0 ) and not ( lIndex >= 0 ) then
         begin
           SystemYieldDialog.lblStochastic.Visible := False;
           SystemYieldDialog.lblNumberOfSeqAnalysed.Visible := False;
           SystemYieldDialog.StochasticLongTermGrid.Visible := False;
         end;
       finally
         if Assigned ( lPeriodLength ) then
            FreeAndNil ( lPeriodLength );
       end;
      end
    finally
      FreeAndNil ( lSystemYieldObject );
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TSystemYieldValidator.SaveState: boolean;
const OPNAME = 'TSystemYieldValidator.SaveState';
begin
  Result := inherited SaveState;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TSystemYieldValidator.StudyDataHasChanged(AContext: TChangeContext; AFieldName,AOldValue,ANewValue: string): boolean;
const OPNAME = 'TSystemYieldValidator.StudyDataHasChanged';
begin
  Result := inherited StudyDataHasChanged(AContext,AFieldName,AOldValue,ANewValue);
  try
    if(UpperCase(AFieldName) = 'LOADCASESCOUNT') or (UpperCase(AFieldName) = 'HYDROSEQCOUNT') then
      PopulateDataViewer;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TSystemYieldValidator.StudyHasChanged: boolean;
const OPNAME = 'TSystemYieldValidator.StudyHasChanged';
begin
  Result := inherited StudyHasChanged;
  try
    ClearDataViewer;
    RePopulateDataViewer;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TSystemYieldValidator.SystemYieldDialog: TSystemYieldDialog;
const OPNAME = 'TSystemYieldValidator.SystemYieldDialog';
begin
  Result := nil;
  try
    if Assigned(FPanel) then
      Result := TSystemYieldDialog(FPanel);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TSystemYieldValidator.OnEditControlEnter(Sender: TObject);
const OPNAME = 'TSystemYieldValidator.OnEditControlEnter';
begin
  inherited OnEditControlEnter ( Sender );
  try

  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TSystemYieldValidator.OnEditControltExit ( Sender : TObject );
const OPNAME = 'TSystemYieldValidator.OnEditControltExit';
begin
  inherited OnEditControltExit ( Sender );
  try

  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TSystemYieldValidator.OnStringGridSelectCell ( Sender: TObject; ACol, ARow: Integer; var CanSelect: Boolean);
const OPNAME = 'TSystemYieldValidator.OnStringGridSelectCell';
begin
  try

  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TSystemYieldValidator.OnStringGridCellDataHasChanged ( ASender: TObject; ACol, ARow: integer );
const OPNAME = 'TSystemYieldValidator.OnStringGridCellDataHasChanged';
begin
  inherited;
  try

  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TSystemYieldValidator.SetGridFields ( aFieldStringGrid : TFieldStringGrid );
const OPNAME = 'TSystemYieldValidator.SetGridFields';
begin
    aFieldStringGrid.AddFieldProperty ( FAppModules.FieldProperties.FieldProperty ( 'SumOutTargetDraft' ) );
    aFieldStringGrid.AddFieldProperty ( FAppModules.FieldProperties.FieldProperty ( 'SumOutNoOfFailureSequences' ) );
    aFieldStringGrid.AddFieldProperty ( FAppModules.FieldProperties.FieldProperty ( 'SumOutReliabilityOfSupply' ) );
    aFieldStringGrid.AddFieldProperty ( FAppModules.FieldProperties.FieldProperty ( 'SumOutRecurrenceIntervalOfFailure' ) );

    aFieldStringGrid.Cells[0, 0] := FAppModules.Language.GetString('TField.SumOutTargetDraft');
    aFieldStringGrid.Cells[1, 0] := FAppModules.Language.GetString('TField.SumOutNoOfFailureSequences');
    aFieldStringGrid.Cells[2, 0] := FAppModules.Language.GetString('TField.SumOutReliabilityOfSupply');
    aFieldStringGrid.Cells[3, 0] := FAppModules.Language.GetString('TField.SumOutRecurrenceIntervalOfFailure');

    aFieldStringGrid.OnBeforeCellChange    := OnStringGridCellDataHasChanged;
    aFieldStringGrid.OnSelectCell          := OnStringGridSelectCell;
    aFieldStringGrid.OnColEnter            := OnStringGridColEnter;

end;

procedure TSystemYieldValidator.DoOnDrawCell ( Sender: TObject; ACol, ARow: Integer; Rect: TRect; State: TGridDrawState );
const OPNAME = 'TSystemYieldValidator.DoOnDrawCell';
var
  lRect : TRect;
  lOldColour : TColor;
  lOldBrushStyle : TBrushStyle;
begin
  try
    if ( ARow = 1 ) and ( not ACol = 0 ) or ( ( Sender as TFieldStringGrid ).Cells [ ACol, ARow ] = '' )  then
    begin
      LOldColour := SystemYieldDialog.StochasticLongTermGrid.Canvas.Brush.Color;
      LOldBrushStyle := SystemYieldDialog.StochasticLongTermGrid.Canvas.Brush.Style;
      try
        LRect.Left := Rect.Left + 0;
        LRect.Top := Rect.Top + 0;
        LRect.Right := Rect.Right - 0;
        LRect.Bottom := Rect.Bottom - 0;

        SystemYieldDialog.StochasticLongTermGrid.Canvas.Brush.Color := clBtnFace;
        SystemYieldDialog.StochasticLongTermGrid.Canvas.Brush.Style := bsSolid;
        SystemYieldDialog.StochasticLongTermGrid.Canvas.FillRect(LRect);
      finally
        SystemYieldDialog.StochasticLongTermGrid.Canvas.Brush.Style := LOldBrushStyle;
        SystemYieldDialog.StochasticLongTermGrid.Canvas.Brush.Color := LOldColour;
      end;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TSystemYieldValidator.CanExport: Boolean;
const OPNAME = 'TSystemYieldValidator.CanExport';
begin
   Result := False;
  try
    if FRequiredMode = rmHistoric then
    begin
      if Assigned(SystemYieldDialog.HistoricGrid) then
        Result := True
    end else
    if FRequiredMode = rmStochastic then
    begin
      if Assigned(SystemYieldDialog.StochasticGrid) and
         Assigned(SystemYieldDialog.StochasticLongTermGrid) then
        Result := True;
    end else
      Result := FAppModules.Model.CanExport;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TSystemYieldValidator.DoExport(AFileName: string = '');
const OPNAME = 'TSystemYieldValidator.DoExport';
begin
  try
    if FRequiredMode = rmHistoric then
    begin
      if Assigned(SystemYieldDialog.HistoricGrid) then
        SystemYieldDialog.HistoricGrid.DoExport(AFileName);
    end else        
    if FRequiredMode = rmStochastic then
    begin
      if //Assigned(SystemYieldDialog.StochasticGrid) and
         Assigned(SystemYieldDialog.StochasticLongTermGrid) then
        SystemYieldDialog.StochasticLongTermGrid.DoExport(AFileName);
    end else
      FAppModules.Model.DoExport(AFileName);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.

