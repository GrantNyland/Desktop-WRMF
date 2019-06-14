//
//
//  UNIT      : Contains TFlowDiversionRelationshipValidator Class
//  AUTHOR    : Sam Dhlamini(Cornastone)
//  DATE      : 20/08/2006
//  COPYRIGHT : Copyright © 2004 DWAF
//
//

unit UFlowDiversionRelationshipValidator;

interface
uses
  Classes,
  VCL.stdctrls,
  VCL.ComCtrls,
  VCL.Controls,
  VCL.extctrls,
  VCL.CheckLst,
  VCLTee.Chart,
  VCLTee.Series,
  VCL.Grids,
  VCL.Graphics,
  VCL.Dialogs,
  //TeeProcs,
  VCLTee.TeEngine,
  Math,
  VCL.Forms,
  Windows,
  Contnrs,

  UHelpContexts,
  UAbstractObject,
  UAbstractComponent,
  UMenuItemManager,
  UDataComponent,
  UDailyDiversionDataObject,
  UDailyDiversionGaugeData,
  UFlowDiversionRelationshipDialog,
  UDailyDiversionContextValidationType,
  UGenericModelLinkClasses;
type
  TFlowDiversionRelationshipValidator = class (TAbstractDataDialogValidator)
  protected
    FFlowDiversionRelationshipDialog : TFlowDiversionRelationshipDialog;
    procedure CreateMemberObjects; override;
    procedure OnEditControltExit(Sender: TObject); override;
    procedure OnStringGridCellDataHasChanged(ASender: TObject;ACol: Integer; ARow: Integer); override;
    procedure OnstrgrdDailyInstreamFlowSelectCell(Sender: TObject; ACol, ARow: Integer; var CanSelect: Boolean);
    procedure OnstrgrdDailyInstreamFlowTopLeftChanged(Sender: TObject);
    procedure RepopulateDataViewer;
    function GetFlowDiversionRelationshipDialog : TFlowDiversionRelationshipDialog;
    procedure UpdateUnRankedRelationship;
    procedure UpdateGeneratedWRYMData;
    procedure DoDrawstrgrdWRYMData(Sender: TObject;ACol, ARow : integer; Rect : TRect;State : TGridDrawState);

  public
    function Initialise: boolean; override;
    function SaveState: boolean; override;
    function LanguageHasChanged: boolean; override;
    function StudyHasChanged: boolean; override;
    function StudyDataHasChanged(AContext: TChangeContext; AFieldName,AOldValue,ANewValue: string): boolean; override;

    procedure ClearDataViewer; override;
    procedure PopulateDataViewer; override;
    procedure DoContextValidation(AValidationType: TDialogValidationType);

    procedure DoExport(AFileName: string = ''); override;
    procedure DoPrint; override;
    function CanExport : boolean; override;
    function CanPrint  : boolean; override;

    property FlowDiversionRelationshipDialog : TFlowDiversionRelationshipDialog read GetFlowDiversionRelationshipDialog;
end;
implementation
uses
  System.UITypes,
  SysUtils,
  UConstants,
  UDataEditComponent,
  UErrorHandlingOperations;
{ DailyDiversionValidator }

procedure TFlowDiversionRelationshipValidator.ClearDataViewer;
const OPNAME = 'TFlowDiversionRelationshipValidator.ClearDataViewer';
begin
  inherited ClearDataViewer;
  try
    FlowDiversionRelationshipDialog.strgrdUnRankedRelationship.ColCount     := 4;
    FlowDiversionRelationshipDialog.strgrdUnRankedRelationship.RowCount     := 2;
    FlowDiversionRelationshipDialog.strgrdUnRankedRelationship.ColWidths[0] := 45;
    FlowDiversionRelationshipDialog.strgrdUnRankedRelationship.ColWidths[1] := 75;
    FlowDiversionRelationshipDialog.strgrdUnRankedRelationship.ColWidths[2] := 75;
    FlowDiversionRelationshipDialog.strgrdUnRankedRelationship.ColWidths[3] := 75;

    FlowDiversionRelationshipDialog.strgrdRankedRelationship.ColWidths[0] := 35;
    FlowDiversionRelationshipDialog.strgrdRankedRelationship.ColWidths[1] := 75;
    FlowDiversionRelationshipDialog.strgrdRankedRelationship.ColWidths[2] := 75;
    FlowDiversionRelationshipDialog.strgrdRankedRelationship.ColWidths[3] := 75;

    FlowDiversionRelationshipDialog.strgrdWRYMData.ColWidths[0] := 35;
    FlowDiversionRelationshipDialog.strgrdWRYMData.ColWidths[1] := 75;
    FlowDiversionRelationshipDialog.strgrdWRYMData.ColWidths[2] := 75;
    FlowDiversionRelationshipDialog.strgrdWRYMData.ColWidths[3] := 75;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFlowDiversionRelationshipValidator.GetFlowDiversionRelationshipDialog : TFlowDiversionRelationshipDialog;
const OPNAME = 'TFlowDiversionRelationshipValidator.GetFlowDiversionRelationshipDialog';
begin
  Result := nil;
  try
    if (FPanel <> nil) then
      Result := TFlowDiversionRelationshipDialog(FPanel);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TFlowDiversionRelationshipValidator.CreateMemberObjects;
const OPNAME = 'TFlowDiversionRelationshipValidator.CreateMemberObjects';
begin
  inherited CreateMemberObjects;
  try
    FPanel := TFlowDiversionRelationshipDialog.Create(nil, FAppModules);
    FlowDiversionRelationshipDialog.strgrdUnRankedRelationship.OnBeforeCellChange := OnStringGridCellDataHasChanged;
    FlowDiversionRelationshipDialog.strgrdWRYMData.OnBeforeCellChange := OnStringGridCellDataHasChanged;
    FlowDiversionRelationshipDialog.strgrdWRYMData.OnDrawCell := DoDrawstrgrdWRYMData;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TFlowDiversionRelationshipValidator.OnstrgrdDailyInstreamFlowSelectCell(Sender: TObject; ACol, ARow: Integer; var CanSelect: Boolean);
const OPNAME = 'TFlowDiversionRelationshipValidator.OnstrgrdDailyInstreamFlowSelectCell';
begin
  try
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TFlowDiversionRelationshipValidator.OnstrgrdDailyInstreamFlowTopLeftChanged(Sender: TObject);
const OPNAME = 'TFlowDiversionRelationshipValidator.OnstrgrdDailyInstreamFlowTopLeftChanged';
begin
  try
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TFlowDiversionRelationshipValidator.DoContextValidation(AValidationType: TDialogValidationType);
const OPNAME = 'TFlowDiversionRelationshipValidator.DoContextValidation';
begin
  inherited;
  try
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TFlowDiversionRelationshipValidator.Initialise: boolean;
const OPNAME = 'TFlowDiversionRelationshipValidator.Initialise';
begin
  Result := inherited Initialise;
  try
    ClearDataViewer;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFlowDiversionRelationshipValidator.LanguageHasChanged: boolean;
const OPNAME = 'TFlowDiversionRelationshipValidator.LanguageHasChanged';
begin
  Result := False;
  try
    TabShetCaption :=  'Flow Diversion Relationship'; //FAppModules.Language.GetString('TabCaption.MonthlyDiversionFlow');
    Result := inherited LanguageHasChanged;
    Result := True;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TFlowDiversionRelationshipValidator.OnEditControltExit(Sender: TObject);
const OPNAME = 'TFlowDiversionRelationshipValidator.OnEditControltExit';
begin
  inherited OnEditControltExit(Sender);
  try
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TFlowDiversionRelationshipValidator.PopulateDataViewer;
const OPNAME = 'TFlowDiversionRelationshipValidator.PopulateDataViewer';
begin
  inherited PopulateDataViewer;
  try
    ClearDataViewer;
    RepopulateDataViewer;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFlowDiversionRelationshipValidator.RepopulateDataViewer;
const OPNAME = 'TFlowDiversionRelationshipValidator.RepopulateDataViewer';
var
  LDiversionGauge : TDiversionGauge;
  LIndex : integer;
  LWRYMData : TWRYMChannelData;
  LRankedFlowDiversionRelationship,
  LUnRankedFlowDiversionRelationship : TFlowDiversionRelationship;
begin
  try
    LDiversionGauge := TDailyDiversionDataObject(FAppModules.Model.ModelData).
                        DailyDiversionGaugeDataList.DiversionGaugeByStationID[Identifier];
    if LDiversionGauge <> nil then
    begin
      FlowDiversionRelationshipDialog.strgrdUnRankedRelationship.RowCount := LDiversionGauge.FlowDiversionRelationshipCount + 1;
      for LIndex := 0  to LDiversionGauge.FlowDiversionRelationshipCount - 1 do
      begin
        LUnRankedFlowDiversionRelationship := LDiversionGauge.UnRankedFlowDiversionRelationshipByIndex[LIndex];
        FlowDiversionRelationshipDialog.strgrdUnRankedRelationship.IsColumnEnabled[0] := False;
        FlowDiversionRelationshipDialog.strgrdUnRankedRelationship.Cells[0, LIndex + 1] :=
        FormatDateTime('yyyy/mm',LUnRankedFlowDiversionRelationship.RelationDate);
        if LUnRankedFlowDiversionRelationship.ReferenceFlow = NullFloat then
          FlowDiversionRelationshipDialog.strgrdUnRankedRelationship.Cells[1, LIndex + 1] := ''
        else
        begin
          FlowDiversionRelationshipDialog.strgrdUnRankedRelationship.Cells[1, LIndex + 1] :=
          FormatFloat('####0.000',LUnRankedFlowDiversionRelationship.ReferenceFlow);
        end;

        if LUnRankedFlowDiversionRelationship.DiversionFlow = NullFloat then
          FlowDiversionRelationshipDialog.strgrdUnRankedRelationship.Cells[2, LIndex + 1] := ''
        else
        begin
          FlowDiversionRelationshipDialog.strgrdUnRankedRelationship.Cells[2, LIndex + 1] :=
          FormatFloat('####0.000',LUnRankedFlowDiversionRelationship.DiversionFlow);
        end;

        if LUnRankedFlowDiversionRelationship.NonDiversionFlow = NullFloat then
          FlowDiversionRelationshipDialog.strgrdUnRankedRelationship.Cells[3, LIndex + 1] := ''
        else
        begin
          FlowDiversionRelationshipDialog.strgrdUnRankedRelationship.Cells[3, LIndex + 1] :=
          FormatFloat('####0.000',LUnRankedFlowDiversionRelationship.NonDiversionFlow);
        end;
      end;

      FlowDiversionRelationshipDialog.FlowDiversionRelationshipPointSeries.Clear;
      FlowDiversionRelationshipDialog.strgrdRankedRelationship.RowCount := LDiversionGauge.RankedFlowDiversionRelationshipCount + 1;
      for LIndex := 0  to LDiversionGauge.RankedFlowDiversionRelationshipCount - 1 do
      begin
        LRankedFlowDiversionRelationship := LDiversionGauge.RankedFlowDiversionRelationshipByIndex[LIndex];
        FlowDiversionRelationshipDialog.strgrdRankedRelationship.Cells[0, LIndex + 1] := IntToStr(LIndex+1);
        FlowDiversionRelationshipDialog.strgrdRankedRelationship.Cells[1, LIndex + 1] :=
        FormatFloat('####0.000',LRankedFlowDiversionRelationship.ReferenceFlow);

        FlowDiversionRelationshipDialog.strgrdRankedRelationship.Cells[2, LIndex + 1] :=
        FormatFloat('####0.000',LRankedFlowDiversionRelationship.DiversionFlow);

        FlowDiversionRelationshipDialog.strgrdRankedRelationship.Cells[3, LIndex + 1] :=
        FormatFloat('####0.000',LRankedFlowDiversionRelationship.NonDiversionFlow);

        FlowDiversionRelationshipDialog.FlowDiversionRelationshipPointSeries.AddXY(
        LRankedFlowDiversionRelationship.ReferenceFlow,LRankedFlowDiversionRelationship.DiversionFlow,'',clBlack);

        FlowDiversionRelationshipDialog.FlowNonDiversionRelationshipPointSeries.AddXY(
        LRankedFlowDiversionRelationship.ReferenceFlow,LRankedFlowDiversionRelationship.NonDiversionFlow,'',clBlack);

      end;
      FlowDiversionRelationshipDialog.FlowDiversionRelationshipLineSeries.Clear;

      FlowDiversionRelationshipDialog.strgrdWRYMData.RowCount := LDiversionGauge.WRYMDataCount + 1;
      for LIndex := 0  to LDiversionGauge.WRYMDataCount - 1 do
      begin
        LWRYMData := LDiversionGauge.WRYMDataByIndex[LIndex];
        FlowDiversionRelationshipDialog.strgrdWRYMData.Cells[0, LIndex + 1] :=IntToStr(LIndex+1);
        if LWRYMData.ReferenceFlow <> NullFloat then
        begin
          FlowDiversionRelationshipDialog.strgrdWRYMData.AddFieldProperty(
          FAppModules.FieldProperties.FieldProperty('WRYMReferenceFlow'));
          FlowDiversionRelationshipDialog.strgrdWRYMData.Cells[1, LIndex + 1] :=
          FormatFloat('####0.000',LWRYMData.ReferenceFlow);
        end
        else
          FlowDiversionRelationshipDialog.strgrdWRYMData.Cells[1, LIndex + 1] := '';
        if LWRYMData.DiversionFlow <> NullFloat then
        begin
          FlowDiversionRelationshipDialog.strgrdWRYMData.AddFieldProperty(
          FAppModules.FieldProperties.FieldProperty('WRYMDiversionFlow'));
          FlowDiversionRelationshipDialog.strgrdWRYMData.Cells[2, LIndex + 1] :=
          FormatFloat('####0.000',LWRYMData.DiversionFlow);
        end
        else
          FlowDiversionRelationshipDialog.strgrdWRYMData.Cells[2, LIndex + 1] := '';

        if LWRYMData.NonDiversionFlow <> NullFloat then
        begin
          FlowDiversionRelationshipDialog.strgrdWRYMData.AddFieldProperty(
          FAppModules.FieldProperties.FieldProperty('WRYMNonDiversionFlow'));
          FlowDiversionRelationshipDialog.strgrdWRYMData.Cells[3, LIndex + 1] :=
          FormatFloat('####0.000',LWRYMData.NonDiversionFlow);
        end
        else
          FlowDiversionRelationshipDialog.strgrdWRYMData.Cells[3, LIndex + 1] := '';

        if (LWRYMData.ReferenceFlow <> NullFloat) and (LWRYMData.DiversionFlow <> NullFloat) then
          FlowDiversionRelationshipDialog.FlowDiversionRelationshipLineSeries.AddXY(
          LWRYMData.ReferenceFlow,LWRYMData.DiversionFlow,'',clTeeColor);

        if (LWRYMData.ReferenceFlow <> NullFloat) and (LWRYMData.NonDiversionFlow <> NullFloat) then
          FlowDiversionRelationshipDialog.FlowNonDiversionRelationshipLineSeries.AddXY(
          LWRYMData.ReferenceFlow,LWRYMData.NonDiversionFlow,'',clTeeColor);
      end;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TFlowDiversionRelationshipValidator.SaveState: boolean;
const OPNAME = 'TFlowDiversionRelationshipValidator.SaveState';
begin
  Result := False;
  try
    Result := True;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TFlowDiversionRelationshipValidator.StudyDataHasChanged(AContext: TChangeContext; AFieldName, AOldValue,
                                                      ANewValue: string): boolean;
const OPNAME = 'TFlowDiversionRelationshipValidator.StudyDataHasChanged';
var
  LDiversionGauge : TDiversionGauge;
begin
  Result := inherited StudyDataHasChanged(AContext,AFieldName,AOldValue,ANewValue);
  try
    if (AFieldName = 'CapacityOfDiversion') or (AFieldName ='InstreamScaleFactor') or (AFieldName ='CompensationValue')then
    begin
      LDiversionGauge := TDailyDiversionDataObject(FAppModules.Model.ModelData).DailyDiversionGaugeDataList.
                         DiversionGaugeByStationID[Identifier];
      if LDiversionGauge <> nil then
      begin
        TDailyDiversionDataObject(FAppModules.Model.ModelData).
        GenerateFlowDiversionRelation(LDiversionGauge.StationID);
        PopulateDataViewer;
      end;
    end
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFlowDiversionRelationshipValidator.StudyHasChanged: boolean;
const OPNAME = 'TFlowDiversionRelationshipValidator.StudyHasChanged';
begin
  Result := inherited StudyHasChanged;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFlowDiversionRelationshipValidator.OnStringGridCellDataHasChanged(ASender: TObject; ACol, ARow: Integer);
const OPNAME = 'TFlowDiversionRelationshipValidator.OnStringGridCellDataHasChanged';
begin
  inherited OnStringGridCellDataHasChanged(ASender, ACol, ARow);
  try
    if ASender = FlowDiversionRelationshipDialog.strgrdWRYMData then
      UpdateGeneratedWRYMData;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFlowDiversionRelationshipValidator.UpdateGeneratedWRYMData;
const OPNAME = 'TFlowDiversionRelationshipValidator.UpdateGeneratedWRYMData';
var
  LMessage : string;
  LDiversionGauge : TDiversionGauge;
  LWRYMData : TWRYMChannelData;
  LValue : string;
  LRow : integer;
begin
  try
    LDiversionGauge := TDailyDiversionDataObject(FAppModules.Model.ModelData).
                        DailyDiversionGaugeDataList.DiversionGaugeByStationID[Identifier];
    if LDiversionGauge <> nil then
    begin
      LValue := FlowDiversionRelationshipDialog.strgrdWRYMData.Cells[
                FlowDiversionRelationshipDialog.strgrdWRYMData.col,
                FlowDiversionRelationshipDialog.strgrdWRYMData.Row ];
      LRow := FlowDiversionRelationshipDialog.strgrdWRYMData.Row;
      LWRYMData := LDiversionGauge.WRYMDataByIndex[LRow-1];
      if LWRYMData <> nil then
      begin
        if (FlowDiversionRelationshipDialog.strgrdWRYMData.col = 1) and
           (FlowDiversionRelationshipDialog.strgrdWRYMData.Row > 0) then
        begin
          if (FAppModules.FieldProperties.ValidateFieldProperty(
            'WRYMReferenceFlow', LValue,LMessage)) then
          begin
            LWRYMData.ReferenceFlow := StrToFloat(LValue);
            LWRYMData.RefFlowValueEdited := True;
            PopulateDataViewer;
            DoContextValidation(dvtReferenceFlow);
          end;
        end;
        if (FlowDiversionRelationshipDialog.strgrdWRYMData.col = 2) and
           (FlowDiversionRelationshipDialog.strgrdWRYMData.Row > 0) then
        begin
          if (FAppModules.FieldProperties.ValidateFieldProperty(
            'WRYMDiversionFlow', LValue,LMessage)) then
          begin
            LWRYMData.DiversionFlow := StrToFloat(LValue);
            LWRYMData.DivFlowValueEdited := True;
            PopulateDataViewer;
            DoContextValidation(dvtDiversionFlow);
          end;
        end;
        if (FlowDiversionRelationshipDialog.strgrdWRYMData.col = 3) and
           (FlowDiversionRelationshipDialog.strgrdWRYMData.Row > 0) then
        begin
          if (FAppModules.FieldProperties.ValidateFieldProperty(
            'WRYMNonDiversionFlow', LValue,LMessage)) then
          begin
            LWRYMData.NonDiversionFlow := StrToFloat(LValue);
            LWRYMData.NonDivFlowValueEdited := True;
            PopulateDataViewer;
            DoContextValidation(dvtDiversionFlow);
          end;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFlowDiversionRelationshipValidator.DoDrawstrgrdWRYMData(Sender: TObject;ACol, ARow : integer; Rect : TRect;State : TGridDrawState);
const OPNAME = 'TFlowDiversionRelationshipValidator.DoDrawstrgrdWRYMData';
var
  LDiversionGauge : TDiversionGauge;
  LWRYMData : TWRYMChannelData;
begin
  try
    if (ARow > 0) and (ACol > 0) then
    begin
      LDiversionGauge := TDailyDiversionDataObject(FAppModules.Model.ModelData).
                          DailyDiversionGaugeDataList.DiversionGaugeByStationID[Identifier];
      if LDiversionGauge <> nil then
      begin
        LWRYMData := LDiversionGauge.WRYMDataByIndex[ARow-1];
        if not (LWRYMData.RefFlowValueEdited) and not (LWRYMData.DivFlowValueEdited)
          and not (LWRYMData.NonDivFlowValueEdited) then
          Exit;
        if (((LWRYMData.RefFlowValueEdited)and (ACol = 1)) or ((LWRYMData.DivFlowValueEdited)and (ACol = 2)))
          or ((LWRYMData.NonDivFlowValueEdited)and (ACol = 3)) then
        begin
          (Sender as TFieldStringGrid).Canvas.Font.Color := clRed;
          if ((Sender as TFieldStringGrid).Canvas.Brush.Color = clHighlight) then
            (Sender as TFieldStringGrid).Canvas.Font.Color := clBlack;
          (Sender as TFieldStringGrid).Canvas.Brush.Color := clYellow;
          (Sender as TFieldStringGrid).Canvas.TextRect(Rect, Rect.Left, Rect.Top, (Sender as TFieldStringGrid).Cells[ACol, ARow]);
        end;
      end;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TFlowDiversionRelationshipValidator.UpdateUnRankedRelationship;
const OPNAME = 'TFlowDiversionRelationshipValidator.UpdateUnRankedRelationship';
var
  LMessage : string;
  LDiversionGauge : TDiversionGauge;
  LUnRankedFlowDiversionRelationship : TFlowDiversionRelationship;
  LValue : string;
  LRow : integer;
begin
  try
    LDiversionGauge := TDailyDiversionDataObject(FAppModules.Model.ModelData).
                        DailyDiversionGaugeDataList.DiversionGaugeByStationID[Identifier];
    if LDiversionGauge <> nil then
    begin
      LValue := FlowDiversionRelationshipDialog.strgrdUnRankedRelationship.Cells[
                FlowDiversionRelationshipDialog.strgrdUnRankedRelationship.col,
                FlowDiversionRelationshipDialog.strgrdUnRankedRelationship.Row ];
      LRow := FlowDiversionRelationshipDialog.strgrdUnRankedRelationship.Row;
      LUnRankedFlowDiversionRelationship := LDiversionGauge.UnRankedFlowDiversionRelationshipByIndex[LRow-1];
      if LUnRankedFlowDiversionRelationship <> nil then
      begin
        if (FlowDiversionRelationshipDialog.strgrdUnRankedRelationship.col = 1) and
           (FlowDiversionRelationshipDialog.strgrdUnRankedRelationship.Row > 0) then
        begin
          if (FAppModules.FieldProperties.ValidateFieldProperty(
            'ReferenceFlow', LValue,LMessage)) then
          begin
            LUnRankedFlowDiversionRelationship.ReferenceFlow := StrToFloat(LValue);
            TDailyDiversionDataObject(FAppModules.Model.ModelData).RefreshFlowDiversionRelationship(LDiversionGauge,LDiversionGauge.StartDate,LDiversionGauge.EndDate);
            PopulateDataViewer;
            DoContextValidation(dvtReferenceFlow);
          end;
        end;
        if (FlowDiversionRelationshipDialog.strgrdUnRankedRelationship.col = 2) and
           (FlowDiversionRelationshipDialog.strgrdUnRankedRelationship.Row > 0) then
        begin
          if (FAppModules.FieldProperties.ValidateFieldProperty(
            'RelationDiversionFlow', LValue,LMessage)) then
          begin
            LUnRankedFlowDiversionRelationship.DiversionFlow := StrToFloat(LValue);
            TDailyDiversionDataObject(FAppModules.Model.ModelData).RefreshFlowDiversionRelationship(LDiversionGauge,LDiversionGauge.StartDate,LDiversionGauge.EndDate);
            PopulateDataViewer;
            DoContextValidation(dvtDiversionFlow);
          end;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFlowDiversionRelationshipValidator.CanExport: boolean;
const OPNAME = 'TFlowDiversionRelationshipValidator.CanExport';
begin
  Result := False;
  try
    if(FlowDiversionRelationshipDialog <> nil) then
      Result := FlowDiversionRelationshipDialog.CanExport;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFlowDiversionRelationshipValidator.CanPrint: boolean;
const OPNAME = 'TOutputDistributionCurveValidator.CanExport';
begin
  Result := False;
  try
    if(FlowDiversionRelationshipDialog <> nil) then
      Result := FlowDiversionRelationshipDialog.CanExport;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFlowDiversionRelationshipValidator.DoExport(AFileName: string);
const OPNAME = 'TFlowDiversionRelationshipValidator.DoExport';
begin
  try
    if(FlowDiversionRelationshipDialog <> nil) then
      FlowDiversionRelationshipDialog.DoExport(AFileName);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFlowDiversionRelationshipValidator.DoPrint;
const OPNAME = 'TFlowDiversionRelationshipValidator.DoPrint';
begin
  try
    if(FlowDiversionRelationshipDialog <> nil) then
      FlowDiversionRelationshipDialog.DoPrint;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
