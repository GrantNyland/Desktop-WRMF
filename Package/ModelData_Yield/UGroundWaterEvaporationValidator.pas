unit UGroundWaterEvaporationValidator;

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
  UGroundWaterEvaporationDialog;

type
  TGroundWaterEvaporationValidator = class(TAbstractYieldDataDialogValidator)
  protected
    FIdentifier          : integer;
    FRiverineMonthlyType : string;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    procedure OnEditControlEnter(Sender: TObject); override;
    procedure OnEditControltExit(Sender: TObject); override;
    procedure OnStringGridCellDataHasChanged(ASender: TObject; ACol, ARow: integer);  override;
    procedure OnAfterPasteColumnData(Sender: TObject); 
    procedure RePopulateDataViewer;
    procedure UpdateGroundWaterEvaporation(AMonthNumber: integer; ANewValue: string);
    procedure PopulateEvaporationGrid;
    procedure PopulateEvaporationGraph;
  public

    function Initialise: boolean; override;
    function SaveState: boolean; override;
    function LanguageHasChanged: boolean; override;
    function StudyHasChanged: boolean; override;
    function StudyDataHasChanged(AContext: TChangeContext; AFieldName,AOldValue,ANewValue: string): boolean; override;

    procedure ClearDataViewer; override;
    procedure PopulateDataViewer; override;
    function GroundWaterEvaporationDialog: TGroundWaterEvaporationDialog;
    property Identifier           : integer read  FIdentifier          write FIdentifier;
    property RiverineMonthlyType  : string  read  FRiverineMonthlyType write FRiverineMonthlyType;
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
  UErrorHandlingOperations, UNetworkFeaturesData, UMiningData,
  UGroundWater;

{ TMiningEvaporationValidator }

procedure TGroundWaterEvaporationValidator.CreateMemberObjects;
const OPNAME = 'TGroundWaterEvaporationValidator.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
    FPanel := TGroundWaterEvaporationDialog.Create(FPanelOwner,FAppModules);
    with GroundWaterEvaporationDialog do
    begin
      GrdMonthlyData.AddFieldProperty(FappModules.FieldProperties.FieldProperty('Evaporation'));
      GrdMonthlyData.OnEnter                := OnEditControlEnter;
      GrdMonthlyData.OnExit                 := OnEditControltExit;
      GrdMonthlyData.OnColEnter             := OnStringGridColEnter;
      GrdMonthlyData.OnBeforeCellChange     := OnStringGridCellDataHasChanged;
      GrdMonthlyData.ShowGridPopupMenu      := True;
      GrdMonthlyData.AllowPasteFromExcel    := True;
      GrdMonthlyData.OnPasteFromExcel       := Self.OnAfterPasteColumnData;
      GrdMonthlyData.OnAfterPasteColumnData := Self.OnAfterPasteColumnData;
    end;
  // Handle exceptions.   }
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TGroundWaterEvaporationValidator.DestroyMemberObjects;
const OPNAME = 'TGroundWaterEvaporationValidator.DestroyMemberObjects';
begin
  try
    inherited DestroyMemberObjects;
  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TGroundWaterEvaporationValidator.Initialise: boolean;
const OPNAME = 'TGroundWaterEvaporationValidator.Initialise';
begin
  Result := inherited Initialise;
  try
    GroundWaterEvaporationDialog.GrdMonthlyData.Cells[0,0] := FAppModules.Language.GetString('GridHeading.Month');
    GroundWaterEvaporationDialog.ChtMonthlyData.BottomAxis.Title.Caption := FAppModules.Language.GetString('ChartCaption.Month');
    GroundWaterEvaporationDialog.GrdMonthlyData.Cells[1,0] := 'GroundWater Evaporation';
    GroundWaterEvaporationDialog.ChtMonthlyData.LeftAxis.Title.Caption := 'GroundWater Evaporation';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TGroundWaterEvaporationValidator.LanguageHasChanged: boolean;
const OPNAME = 'TGroundWaterEvaporationValidator.LanguageHasChanged';
begin
  Result := False;
  try
    Result := inherited LanguageHasChanged;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TGroundWaterEvaporationValidator.ClearDataViewer;
const OPNAME = 'TGroundWaterEvaporationValidator.ClearDataViewer';
var
  LIndex : integer;
begin
  inherited ClearDataViewer;
  try
    for LIndex := 1 to 12 do
      GroundWaterEvaporationDialog.GrdMonthlyData.Cells[1,LIndex] := '';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TGroundWaterEvaporationValidator.PopulateDataViewer;
const OPNAME = 'TGroundWaterEvaporationValidator.PopulateDataViewer';
begin
  inherited PopulateDataViewer;
  try
    ClearDataViewer;
    RePopulateDataViewer;
    DoContextValidation(dvtConfigurationAll);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TGroundWaterEvaporationValidator.SaveState: boolean;
const OPNAME = 'TGroundWaterEvaporationValidator.SaveState';
begin
  Result := inherited SaveState;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TGroundWaterEvaporationValidator.OnEditControlEnter(Sender: TObject);
const OPNAME = 'TGroundWaterEvaporationValidator.OnEditControlEnter';
begin
  inherited OnEditControlEnter(Sender);
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TGroundWaterEvaporationValidator.OnEditControltExit(Sender: TObject);
const OPNAME = 'TGroundWaterEvaporationValidator.OnEditControltExit';
begin
  inherited OnEditControltExit(Sender);
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TGroundWaterEvaporationValidator.OnStringGridCellDataHasChanged(ASender: TObject; ACol, ARow: integer);
const OPNAME = 'TGroundWaterEvaporationValidator.OnStringGridCellDataHasChanged';
begin
  inherited OnStringGridCellDataHasChanged(ASender,ACol, ARow);
  try
    if(ASender =  GroundWaterEvaporationDialog.GrdMonthlyData) then
       UpdateGroundWaterEvaporation(ARow,GroundWaterEvaporationDialog.GrdMonthlyData.Cells[ACol, ARow]);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TGroundWaterEvaporationValidator.GroundWaterEvaporationDialog: TGroundWaterEvaporationDialog;
const OPNAME = 'TGroundWaterEvaporationValidator.GroundWaterEvaporationDialog';
begin
  Result := Nil;
  try
    if Assigned(FPanel) then
      Result := TGroundWaterEvaporationDialog(FPanel);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TGroundWaterEvaporationValidator.StudyDataHasChanged(AContext: TChangeContext; AFieldName,AOldValue,ANewValue: string): boolean;
const OPNAME = 'TGroundWaterEvaporationValidator.StudyDataHasChanged';
begin
  Result := inherited StudyDataHasChanged(AContext,AFieldName,AOldValue,ANewValue);
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TGroundWaterEvaporationValidator.StudyHasChanged: boolean;
const OPNAME = 'TGroundWaterEvaporationValidator.StudyHasChanged';
begin
  Result := inherited StudyHasChanged;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TGroundWaterEvaporationValidator.UpdateGroundWaterEvaporation(AMonthNumber: integer; ANewValue: string);
const OPNAME = 'TGroundWaterEvaporationValidator.UpdateGroundWaterEvaporation';
var
  LGroundWater : IGroundWater;
begin
  try
    LGroundWater := TYieldModelDataObject(FAppModules.Model.ModelData).CastNetworkFeaturesData
                 .CastGroundWaterList.GroundWaterByID[FIdentifier];
    if (LGroundWater <> nil) then
    begin
      if (RiverineMonthlyType = 'Evaporation') then
      begin
        LGroundWater.MonthlyWaterEvaporation[AMonthNumber] := StrToFloat(ANewValue);
        GroundWaterEvaporationDialog.GrdMonthlyData.Cells[1,AMonthNumber] := FloatToStr(LGroundWater.MonthlyWaterEvaporation[AMonthNumber]);
      end
      else
      if (RiverineMonthlyType = 'Factors') then
      begin
        LGroundWater.MonthlyWaterUsageFactors[AMonthNumber] := StrToFloat(ANewValue);
        GroundWaterEvaporationDialog.GrdMonthlyData.Cells[1,AMonthNumber] := FloatToStr(LGroundWater.MonthlyWaterUsageFactors[AMonthNumber]);
      end;
      PopulateDataViewer;
      DoContextValidation(dvtGroundWaterEvaporation);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;



procedure TGroundWaterEvaporationValidator.RePopulateDataViewer;
const OPNAME = 'TGroundWaterEvaporationValidator.RePopulateDataViewer';
begin
  try
    if (FIdentifier > 0) then
    begin
      PopulateEvaporationGrid;
      PopulateEvaporationGraph;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TGroundWaterEvaporationValidator.PopulateEvaporationGrid;
const OPNAME = 'TGroundWaterEvaporationValidator.PopulateEvaporationGrid';
var
  LCol           : integer;
  LIndex         : integer;
  LGroundWater   : IGroundWater;
  LMonthData     : IRunConfigurationData;
  LHeading       : string;
begin
  try
    LMonthData := TYieldModelDataObject(FAppModules.Model.ModelData).RunConfigurationData;
    LGroundWater := TYieldModelDataObject(FAppModules.Model.ModelData).CastNetworkFeaturesData
                 .CastGroundWaterList.GroundWaterByID[FIdentifier];
    if (LGroundWater <> nil) then
    begin
      with GroundWaterEvaporationDialog do
      begin
        if (RiverineMonthlyType = 'Evaporation') then
          LHeading := FAppModules.Language.GetString('GridCaption.MonthlyReference') + RiverineMonthlyType
        else
          LHeading := FAppModules.Language.GetString('ChartCaption.MonthlyWaterUsage') + RiverineMonthlyType;
          
        GroundWaterEvaporationDialog.GrdMonthlyData.Cells[0,0] := FAppModules.Language.GetString('GridHeading.Month');
        GroundWaterEvaporationDialog.ChtMonthlyData.BottomAxis.Title.Caption := FAppModules.Language.GetString('ChartCaption.Month');

        GroundWaterEvaporationDialog.GrdMonthlyData.Cells[1,0]   := LHeading;
        GroundWaterEvaporationDialog.ChtMonthlyData.LeftAxis.Title.Caption := LHeading;
        for LCol := 1 to GrdMonthlyData.ColCount-1 do
        begin
          for LIndex := 1 to 12 do
          begin
            if (RiverineMonthlyType = 'Evaporation') then
            begin
              GrdMonthlyData.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('MonthlyWaterEvaporation'));
              if (LMonthData <> nil)then
              begin
                GrdMonthlyData.Cells[0, LIndex] := LMonthData.MonthNameByIndex[LIndex];
                GrdMonthlyData.Cells[1, LIndex] := FloatToStr(LGroundWater.MonthlyWaterEvaporation[LIndex]);
              end;
            end
            else
            if (RiverineMonthlyType = 'Factors') then
            begin
              GrdMonthlyData.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('MonthlyWaterUsageFactors'));
              if (LMonthData <> nil)then
              begin
                GrdMonthlyData.Cells[0, LIndex] := LMonthData.MonthNameByIndex[LIndex];
                GrdMonthlyData.Cells[1, LIndex] := FloatToStr(LGroundWater.MonthlyWaterUsageFactors[LIndex]);
              end;
            end;
          end;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TGroundWaterEvaporationValidator.PopulateEvaporationGraph;
const OPNAME = 'TGroundWaterEvaporationValidator.PopulateEvaporationGraph';
var
  LSeries                : TBarSeries;
  LRunConfigurationData  : IRunConfigurationData;
  Lindex                 : integer;
  LChartHeading          : string;
begin
  try
    LRunConfigurationData := TYieldModelDataObject(FAppModules.Model.ModelData).RunConfigurationData;

    if (RiverineMonthlyType = 'Evaporation') then
      LChartHeading := FAppModules.Language.GetString('ChartCaption.RiverineMonthlyReference') + RiverineMonthlyType
    else
      LChartHeading := FAppModules.Language.GetString('ChartCaption.RiverineMonthlyWaterUsage') + RiverineMonthlyType;

    GroundWaterEvaporationDialog.ChtMonthlyData.Title.Text.Text := LChartHeading;
    GroundWaterEvaporationDialog.ChtMonthlyData.AxisVisible := True;
    GroundWaterEvaporationDialog.ChtMonthlyData.ClipPoints := True;
    GroundWaterEvaporationDialog.ChtMonthlyData.Frame.Visible := True;
    GroundWaterEvaporationDialog.ChtMonthlyData.Legend.Visible := False;
    GroundWaterEvaporationDialog.ChtMonthlyData.View3D := False;
    GroundWaterEvaporationDialog.ChtMonthlyData.RemoveAllSeries;
    GroundWaterEvaporationDialog.ChtMonthlyData.SeriesList.Clear;

    GroundWaterEvaporationDialog.ChtMonthlyData.LeftAxis.Title.Caption    := LChartHeading;
    GroundWaterEvaporationDialog.ChtMonthlyData.LeftAxis.AxisValuesFormat := '######0.0';
    GroundWaterEvaporationDialog.ChtMonthlyData.LeftAxis.Title.Angle      := 90;
    GroundWaterEvaporationDialog.ChtMonthlyData.BottomAxis.Title.Caption  := FAppModules.Language.GetString('ChartCaption.Month');

    LSeries := TBarSeries.Create(GroundWaterEvaporationDialog.ChtMonthlyData);
    LSeries.ParentChart     := GroundWaterEvaporationDialog.ChtMonthlyData;
    LSeries.Marks.Visible   := False;
    LSeries.SeriesColor     := clRed;
    LSeries.BarWidthPercent := 60;
    LSeries.XValues.Order   := loAscending;
    LSeries.YValues.Order   := loNone;

    for LIndex := 1 to 12 do
    begin
      if StrToFloat(GroundWaterEvaporationDialog.GrdMonthlyData.Cells[1,LIndex]) = NullFloat then
        LSeries.Add(0.0, LRunConfigurationData.MonthNameByIndex[LIndex])
      else
      LSeries.Add(StrToFloat(GroundWaterEvaporationDialog.GrdMonthlyData.Cells[1,LIndex]), LRunConfigurationData.MonthNameByIndex[LIndex]);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TGroundWaterEvaporationValidator.OnAfterPasteColumnData(Sender: TObject);
const OPNAME = 'TGroundWaterEvaporationValidator.OnAfterPasteColumnData';
var
  LGroundWater : IGroundWater;
  LValue       : double;
  LMonth       : integer;
begin
  try
    LGroundWater := TYieldModelDataObject(FAppModules.Model.ModelData).CastNetworkFeaturesData
                 .CastGroundWaterList.GroundWaterByID[FIdentifier];
    if(LGroundWater <> nil) then
    begin
      if(RiverineMonthlyType = 'Evaporation') then
      begin
        for LMonth := 1 to 12 do
        begin
          LValue := StrToFloat(Trim(GroundWaterEvaporationDialog.GrdMonthlyData.Cells[1,LMonth]));
          LGroundWater.MonthlyWaterEvaporation[LMonth] := LValue;
        end;
      end
      else
      if(RiverineMonthlyType = 'Factors') then
      begin
        for LMonth := 1 to 12 do
        begin
          LValue := StrToFloat(Trim(GroundWaterEvaporationDialog.GrdMonthlyData.Cells[1,LMonth]));
          LGroundWater.MonthlyWaterUsageFactors[LMonth] := LValue;
        end;
      end;
      PopulateDataViewer;
      DoContextValidation(dvtReservoirEvaporation);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.

