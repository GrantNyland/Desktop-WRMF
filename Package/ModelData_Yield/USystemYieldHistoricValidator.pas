//
//
//  UNIT      : Contains the class TSystemYieldHistoricValidator.
//  AUTHOR    : Sam Dhlamini(ARIVIA)
//  DATE      : 2005/02/15
//  COPYRIGHT : Copyright © 2003 DWAF
//
//
unit USystemYieldHistoricValidator;

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
  USystemYieldHistoricDialog;

type
  TSystemYieldHistoricValidator = class(TAbstractDataDialogValidator)
  protected
    procedure CreateMemberObjects; override;
    procedure RePopulateDataViewer;
    procedure PopulateHistoricGraph(ATargetDraftData,ASysYieldData : TStrings);
    procedure DisplayHistoricGraphLegend;
  public
    function Initialise: boolean; override;
    function LanguageHasChanged: boolean; override;
    function StudyHasChanged: boolean; override;
    function StudyDataHasChanged(AContext: TChangeContext; AFieldName,AOldValue,ANewValue: string): boolean; override;
    procedure ClearDataViewer; override;
    procedure PopulateDataViewer; override;
    function SystemYieldHistoricDialog: TSystemYieldHistoricDialog;
    function CanExport : Boolean; override;
    procedure DoExport(AFileName: string = ''); override;
  end;

implementation

uses
  VCLTee.TeEngine,
  VCLTee.Chart,
  SysUtils,
  UConstants,
  VCL.Graphics,
  VCLTee.ArrowCha,
  UYieldModelDataObject,
  URunConfigurationData,
  USystemYieldObject,
  UErrorHandlingOperations, VCLTee.Series;

{ TSystemYieldHistoricValidator }

procedure TSystemYieldHistoricValidator.CreateMemberObjects;
const OPNAME = 'TSystemYieldHistoricValidator.CreateMemberObjects';
begin
  inherited CreateMemberObjects;
  try
    FPanel := TSystemYieldHistoricDialog.Create(FPanelOwner,FAppModules);
    SystemYieldHistoricDialog.HistoricGrid.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('SumOutTargetDraft'));
    SystemYieldHistoricDialog.HistoricGrid.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('SumOutDeficitPropotion'));
    SystemYieldHistoricDialog.HistoricGrid.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('SumOutYield'));
    SystemYieldHistoricDialog.HistoricGrid.OnColEnter            := OnStringGridColEnter;
    TabShetCaption := FAppModules.Language.GetString('ViewData.Historic');
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TSystemYieldHistoricValidator.Initialise: boolean;
const OPNAME = 'TSystemYieldHistoricValidator.Initialise';
begin
  Result := inherited Initialise;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TSystemYieldHistoricValidator.LanguageHasChanged: boolean;
const OPNAME = 'TSystemYieldHistoricValidator.LanguageHasChanged';
begin
  Result := False;
  try
    Result := inherited LanguageHasChanged;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TSystemYieldHistoricValidator.ClearDataViewer;
const OPNAME = 'TSystemYieldHistoricValidator.ClearDataViewer';
begin
  inherited ClearDataViewer;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TSystemYieldHistoricValidator.PopulateDataViewer;
const OPNAME = 'TSystemYieldHistoricValidator.PopulateDataViewer';
begin
  inherited PopulateDataViewer;
  try
    ClearDataViewer;
    RePopulateDataViewer;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TSystemYieldHistoricValidator.RePopulateDataViewer;
const OPNAME = 'TSystemYieldHistoricValidator.RePopulateDataViewer';
var
  lIndex            : integer;
  lHistoricSysYield : TSystemYieldObject;
  LFieldProperty    : TAbstractFieldProperty;
  LTargetDraftData  : TStringList;
  LSysYieldData     : TStringList;
begin
  try
    try
      lHistoricSysYield := TSystemYieldObject.Create(FAppModules);
      lHistoricSysYield.SystemYieldType := ytHistoric;
      lHistoricSysYield.Initialise;
      if (lHistoricSysYield <> nil) then
      begin
        if (lHistoricSysYield.ErrorMsg <> '') then
        begin
          SystemYieldHistoricDialog.ShowError(FAppModules.Language.GetString('TYRCSelectorsPanel.'+ lHistoricSysYield.ErrorMsg));
          if lHistoricSysYield <> nil then
            lHistoricSysYield := nil;
          Exit;
        end;
        LTargetDraftData  := TStringList.Create;
        LSysYieldData     := TStringList.Create;
        try
          SystemYieldHistoricDialog.HistoricGrid.RowCount := lHistoricSysYield.HistoricSystemYieldCounter + 1;
          for lIndex := 0 to SystemYieldHistoricDialog.HistoricGrid.RowCount do
          begin
            if (lHistoricSysYield.GetTargetYieldByIndex(lIndex) = NullFloat) or
               (lHistoricSysYield.GetTargetYieldByIndex(lIndex) = 0) then
              break;
            LFieldProperty    := FAppModules.FieldProperties.FieldProperty('FlowConstraints');
            SystemYieldHistoricDialog.HistoricGrid.Cells[0,lIndex + 1] := Format(LFieldProperty.FormatStringGrid,[lHistoricSysYield.GetTargetYieldByIndex(lIndex)]);
            SystemYieldHistoricDialog.HistoricGrid.Cells[1,lIndex + 1] := Format(LFieldProperty.FormatStringGrid,[lHistoricSysYield.GetDeficitPropotionByIndex(lIndex)]);
            SystemYieldHistoricDialog.HistoricGrid.Cells[2,lIndex + 1] := Format(LFieldProperty.FormatStringGrid,[lHistoricSysYield.GetSystemYieldByIndex(lIndex)]);
          end;
          for lIndex := 0 to lHistoricSysYield.HistoricSystemYieldCounter-1 do
          begin
            if (lHistoricSysYield.GetTargetYieldByIndex(lIndex) = NullFloat) or
               (lHistoricSysYield.GetTargetYieldByIndex(lIndex) = 0) then
              break;
            LFieldProperty    := FAppModules.FieldProperties.FieldProperty('FlowConstraints');
            LSysYieldData.Add(Format(LFieldProperty.FormatStringGrid,[lHistoricSysYield.GetSystemYieldByIndex(lIndex)]));
            LTargetDraftData.Add(Format(LFieldProperty.FormatStringGrid,[lHistoricSysYield.GetTargetYieldByIndex(lIndex)]));
          end;
          if (LSysYieldData.Count > 0) and (LTargetDraftData.Count > 0) then
          begin
            PopulateHistoricGraph(LTargetDraftData,LSysYieldData);
            DisplayHistoricGraphLegend;
          end;
        finally
          FreeAndNil(LTargetDraftData);
          FreeAndNil(LSysYieldData);
        end;
      end;
    finally
      FreeAndNil(lHistoricSysYield);
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TSystemYieldHistoricValidator.DisplayHistoricGraphLegend;
const OPNAME = 'TSystemYieldHistoricValidator.DisplayHistoricGraphLegend';
var
  LIndex : integer;
begin
  try
    SystemYieldHistoricDialog.HistoricGraph.Legend.Alignment := laBottom;
    for LIndex := 0 to SystemYieldHistoricDialog.HistoricGraph.SeriesCount-1 do
    begin
      if (SystemYieldHistoricDialog.HistoricGraph.Series[LIndex] is TLineSeries) then
        SystemYieldHistoricDialog.HistoricGraph.Series[LIndex].ShowInLegend := True
      else
        SystemYieldHistoricDialog.HistoricGraph.Series[LIndex].ShowInLegend := False;
    end;
  except on E: Exception do HandleError(E,OPNAME) end;
end;

procedure TSystemYieldHistoricValidator.PopulateHistoricGraph(ATargetDraftData,ASysYieldData : TStrings);
const OPNAME = 'TSystemYieldHistoricValidator.PopulateHistoricGraph';
var
  LIndex : integer;
  LNonFirmYield : TStringList;
  LNonFirmTD : TStringList;
begin
  try
    if Assigned(ATargetDraftData) and Assigned(ASysYieldData) then
    begin
      SystemYieldHistoricDialog.ClearChart;
      SystemYieldHistoricDialog.TDElements := 1;
      SystemYieldHistoricDialog.PrepareChart;
      SystemYieldHistoricDialog.HistoricGraph.BottomAxis.Title.Caption := FAppModules.Language.GetString('GraphHeading.TargetDraft');
      SystemYieldHistoricDialog.HistoricGraph.LeftAxis.Title.Caption := FAppModules.Language.GetString('GraphHeading.SysYield');
      SystemYieldHistoricDialog.HistoricLineSeries[0].Color := clGray;
      SystemYieldHistoricDialog.HistoricLineSeries[0].Title := '45 Degree Line(Y=X)';
      SystemYieldHistoricDialog.HistoricLineSeries[0].LinePen.Style    := psDot;
      SystemYieldHistoricDialog.HistoricLineSeries[1].Color := clBlue;
      SystemYieldHistoricDialog.HistoricLineSeries[1].Title := 'Draft-Yield Line';
      SystemYieldHistoricDialog.HistoricLineSeries[0].AddXY(0,0,'',clGray);
      SystemYieldHistoricDialog.HistoricPointSeries[0].Pointer.Visible := True;
      SystemYieldHistoricDialog.HistoricLineSeries[2].Color := clRed;
      SystemYieldHistoricDialog.HistoricLineSeries[2].Title := 'Non-Firm Yield Line';
      for LIndex := 0 to ATargetDraftData.Count-1 do
      begin
        SystemYieldHistoricDialog.HistoricLineSeries[0].AddXY(StrToFloat(ATargetDraftData[LIndex]),StrToFloat(ATargetDraftData[LIndex]),'',clGray);
        SystemYieldHistoricDialog.HistoricPointSeries[0].AddXY(StrToFloat(ATargetDraftData[LIndex]),StrToFloat(ASysYieldData[LIndex]));
      end;
      for LIndex := 0 to ATargetDraftData.Count-1 do
        SystemYieldHistoricDialog.HistoricLineSeries[1].AddXY(StrToFloat(ATargetDraftData[LIndex]),StrToFloat(ASysYieldData[LIndex]));
      LNonFirmYield := TStringList.Create;
      LNonFirmTD := TStringList.Create;
      try
        for LIndex := 0 to ATargetDraftData.Count-1 do
        begin
          if (ASysYieldData[LIndex] <> ATargetDraftData[LIndex]) then
          begin
            LNonFirmYield.Add(ASysYieldData[LIndex]);
            LNonFirmTD.Add(ATargetDraftData[LIndex]);
          end;
          if (ASysYieldData[LIndex] = ATargetDraftData[LIndex]) then
          begin
            LNonFirmYield.Add(ASysYieldData[LIndex]);
            LNonFirmTD.Add(ATargetDraftData[LIndex]);
            Break;
          end;
        end;
        for  LIndex := 0 to LNonFirmYield.Count-1 do
          SystemYieldHistoricDialog.HistoricLineSeries[2].AddXY(StrToFloat(LNonFirmTD[LIndex]),StrToFloat(LNonFirmYield[LIndex]));
      finally
        FreeAndNil(LNonFirmYield);
        FreeandNil(LNonFirmTD);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TSystemYieldHistoricValidator.StudyDataHasChanged(AContext: TChangeContext; AFieldName,AOldValue,ANewValue: string): boolean;
const OPNAME = 'TSystemYieldHistoricValidator.StudyDataHasChanged';
begin
  Result := inherited StudyDataHasChanged(AContext,AFieldName,AOldValue,ANewValue);
  try
    if(UpperCase(AFieldName) = 'LOADCASESCOUNT') or (UpperCase(AFieldName) = 'HYDROSEQCOUNT') then
      PopulateDataViewer;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TSystemYieldHistoricValidator.StudyHasChanged: boolean;
const OPNAME = 'TSystemYieldHistoricValidator.StudyHasChanged';
begin
  Result := inherited StudyHasChanged;
  try
    ClearDataViewer;
    RePopulateDataViewer;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TSystemYieldHistoricValidator.SystemYieldHistoricDialog: TSystemYieldHistoricDialog;
const OPNAME = 'TSystemYieldHistoricValidator.SystemYieldHistoricDialog';
begin
  Result := nil;
  try
    if Assigned(FPanel) then
      Result := TSystemYieldHistoricDialog(FPanel);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TSystemYieldHistoricValidator.CanExport: Boolean;
const OPNAME = 'TSystemYieldHistoricValidator.CanExport';
begin
   Result := False;
  try
    if (SystemYieldHistoricDialog.HistoricGrid.Cells[0,1] <> '') then
      Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TSystemYieldHistoricValidator.DoExport(AFileName: string = '');
const OPNAME = 'TSystemYieldHistoricValidator.DoExport';
begin
  try
    if (SystemYieldHistoricDialog.HistoricGrid.Cells[0,1] <> '') then
      SystemYieldHistoricDialog.HistoricGrid.DoExport(AFileName);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.

