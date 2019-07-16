//
//
//  UNIT      : Contains the class TMiningEvaporationValidator.
//  AUTHOR    : Presley Mudau
//  DATE      : 2007/03/14
//  COPYRIGHT : Copyright © 2007 DWAF
//
//
unit UMiningEvaporationValidator;

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
  UMineMonthlyDataDialog;

type
  TMiningEvaporationValidator = class(TAbstractYieldDataDialogValidator)
  protected
    FIdentifier      : integer;
    FEvaporationType : string;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    procedure OnEditControlEnter(Sender: TObject); override;
    procedure OnEditControltExit(Sender: TObject); override;
    procedure OnStringGridCellDataHasChanged(ASender: TObject; ACol, ARow: integer);  override;
    procedure OnAfterPasteColumnData(Sender: TObject);
    procedure RePopulateDataViewer;
    procedure UpdateEvaporation(AMonthNumber: integer; ANewValue: string);
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
    function MineMonthlyDataDialog: TMineMonthlyDataDialog;
    property Identifier : integer read  FIdentifier  write FIdentifier;
    property EvaporationType  : string  read  FEvaporationType write FEvaporationType;
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
  UErrorHandlingOperations, UNetworkFeaturesData, UMiningData;

{ TMiningEvaporationValidator }

procedure TMiningEvaporationValidator.CreateMemberObjects;
const OPNAME = 'TMiningEvaporationValidator.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
    FPanel := TMineMonthlyDataDialog.Create(FPanelOwner,FAppModules);
    with MineMonthlyDataDialog do
    begin
      GrdMothlyData.OnEnter                := OnEditControlEnter;
      GrdMothlyData.OnExit                 := OnEditControltExit;
      GrdMothlyData.OnColEnter             := OnStringGridColEnter;
      GrdMothlyData.OnBeforeCellChange     := OnStringGridCellDataHasChanged;
      GrdMothlyData.ShowGridPopupMenu      := True;
      GrdMothlyData.AllowPasteFromExcel    := True;
      GrdMothlyData.OnPasteFromExcel       := Self.OnAfterPasteColumnData;
      GrdMothlyData.OnAfterPasteColumnData := Self.OnAfterPasteColumnData;
    end;
  // Handle exceptions.   }
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMiningEvaporationValidator.DestroyMemberObjects;
const OPNAME = 'TMiningEvaporationValidator.DestroyMemberObjects';
begin
  try
    inherited DestroyMemberObjects;

  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMiningEvaporationValidator.Initialise: boolean;
const OPNAME = 'TMiningEvaporationValidator.Initialise';
begin
  Result := inherited Initialise;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMiningEvaporationValidator.LanguageHasChanged: boolean;
const OPNAME = 'TMiningEvaporationValidator.LanguageHasChanged';
begin
  Result := False;
  try
    Result := inherited LanguageHasChanged;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMiningEvaporationValidator.ClearDataViewer;
const OPNAME = 'TMiningEvaporationValidator.ClearDataViewer';
var
  LIndex : integer;
begin
  inherited ClearDataViewer;
  try
    for LIndex := 1 to 12 do
      MineMonthlyDataDialog.GrdMothlyData.Cells[1,LIndex] := '';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMiningEvaporationValidator.PopulateDataViewer;
const OPNAME = 'TMiningEvaporationValidator.PopulateDataViewer';
begin
  inherited PopulateDataViewer;
  try
    ClearDataViewer;
    RePopulateDataViewer;
    DoContextValidation(dvtConfigurationAll);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMiningEvaporationValidator.SaveState: boolean;
const OPNAME = 'TMiningEvaporationValidator.SaveState';
begin
  Result := inherited SaveState;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMiningEvaporationValidator.OnEditControlEnter(Sender: TObject);
const OPNAME = 'TMiningEvaporationValidator.OnEditControlEnter';
begin
  inherited OnEditControlEnter(Sender);
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMiningEvaporationValidator.OnEditControltExit(Sender: TObject);
const OPNAME = 'TMiningEvaporationValidator.OnEditControltExit';
begin
  inherited OnEditControltExit(Sender);
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMiningEvaporationValidator.OnStringGridCellDataHasChanged(ASender: TObject; ACol, ARow: integer);
const OPNAME = 'TMiningEvaporationValidator.OnStringGridCellDataHasChanged';
begin
  inherited OnStringGridCellDataHasChanged(ASender,ACol, ARow);
  try
    if(ASender =  MineMonthlyDataDialog.GrdMothlyData) then
       UpdateEvaporation(ARow,MineMonthlyDataDialog.GrdMothlyData.Cells[ACol, ARow]);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMiningEvaporationValidator.MineMonthlyDataDialog: TMineMonthlyDataDialog;
const OPNAME = 'TMiningEvaporationValidator.MineMonthlyDataDialog';
begin
  Result := Nil;
  try
    if Assigned(FPanel) then
      Result := TMineMonthlyDataDialog(FPanel);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMiningEvaporationValidator.StudyDataHasChanged(AContext: TChangeContext; AFieldName,AOldValue,ANewValue: string): boolean;
const OPNAME = 'TMiningEvaporationValidator.StudyDataHasChanged';
begin
  Result := inherited StudyDataHasChanged(AContext,AFieldName,AOldValue,ANewValue);
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMiningEvaporationValidator.StudyHasChanged: boolean;
const OPNAME = 'TMiningEvaporationValidator.StudyHasChanged';
begin
  Result := inherited StudyHasChanged;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMiningEvaporationValidator.UpdateEvaporation(AMonthNumber: integer; ANewValue: string);
const OPNAME = 'TMiningEvaporationValidator.UpdateEvaporation';
var
  LMine  : IMine;
  LValue : double;
begin
  try
    LValue := StrToFloat(ANewValue);
    LMine := TYieldModelDataObject(FAppModules.Model.ModelData).CastNetworkFeaturesData
             .CastMineList.MineByNodeNumber[FIdentifier];
    if (LMine <> nil) then
    begin
      if (EvaporationType = 'Pan') then
      begin
        LMine.PanEvaporation[AMonthNumber] := LValue;
        MineMonthlyDataDialog.GrdMothlyData.SetFieldValue(1,AMonthNumber,LMine.PanEvaporation[AMonthNumber]);
      end
      else
      if (EvaporationType = 'Lake') then
      begin
        LMine.LakeEvaporation[AMonthNumber] := LValue;
        MineMonthlyDataDialog.GrdMothlyData.SetFieldValue(1,AMonthNumber,LMine.LakeEvaporation[AMonthNumber]);
      end;
      PopulateDataViewer;
      DoContextValidation(dvtReservoirEvaporation);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;



procedure TMiningEvaporationValidator.RePopulateDataViewer;
const OPNAME = 'TMiningEvaporationValidator.RePopulateDataViewer';
begin
  try
    if (FIdentifier > 0) then
    begin
      PopulateEvaporationGrid;
      PopulateEvaporationGraph;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMiningEvaporationValidator.PopulateEvaporationGrid;
const OPNAME = 'TMiningEvaporationValidator.PopulateEvaporationGrid';
var
  LCol           : integer;
  LIndex         : integer;
  LMine          : IMine;
  LMonthData     : IRunConfigurationData;
begin
  try
    LMonthData := TYieldModelDataObject(FAppModules.Model.ModelData).RunConfigurationData;
    LMine := TYieldModelDataObject(FAppModules.Model.ModelData).CastNetworkFeaturesData.CastMineList.MineByNodeNumber[FIdentifier];
    if (LMine <> nil) then
    begin
      with MineMonthlyDataDialog do
      begin
        MineMonthlyDataDialog.GrdMothlyData.ClearFieldProperties;
        GrdMothlyData.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('Month'));
        if (EvaporationType = 'Pan') then
          GrdMothlyData.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('MinePanEvaporationFactors'))
        else
          GrdMothlyData.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('MineLakeEvaporationFactors'));

        MineMonthlyDataDialog.GrdMothlyData.Cells[0,0] := FAppModules.Language.GetString('GridHeading.Month');
        MineMonthlyDataDialog.GrdMothlyData.Cells[1,0]   := {'Mine' + ' ' + }EvaporationType + ' ' + 'Evaporation Conversion Factors';

        for LCol := 1 to GrdMothlyData.ColCount-1 do
        begin
          for LIndex := 1 to 12 do
          begin
            if (EvaporationType = 'Pan') then
            begin
              if (LMonthData <> nil)then
                GrdMothlyData.Cells[0, LIndex] := LMonthData.MonthNameByIndex[LIndex];
              GrdMothlyData.SetFieldValue(1, LIndex,LMine.PanEvaporation[LIndex]);
            end
            else if (EvaporationType = 'Lake') then
            begin
              if (LMonthData <> nil)then
                 GrdMothlyData.Cells[0, LIndex] := LMonthData.MonthNameByIndex[LIndex];
              GrdMothlyData.SetFieldValue(1, LIndex,LMine.LakeEvaporation[LIndex]);
            end;
          end;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMiningEvaporationValidator.PopulateEvaporationGraph;
const OPNAME = 'TMiningEvaporationValidator.PopulateEvaporationGraph';
var
  LSeries                : TBarSeries;
  LRunConfigurationData  : IRunConfigurationData;
  Lindex                 : integer;
begin
  try
    LRunConfigurationData := TYieldModelDataObject(FAppModules.Model.ModelData).RunConfigurationData;
    MineMonthlyDataDialog.ChtMonthlyData.Title.Text.Text := {'Mine '+ }FEvaporationType + ' Evaporation Conversion Factors';
    MineMonthlyDataDialog.ChtMonthlyData.AxisVisible := True;
    MineMonthlyDataDialog.ChtMonthlyData.ClipPoints := True;
    MineMonthlyDataDialog.ChtMonthlyData.Frame.Visible := True;
    MineMonthlyDataDialog.ChtMonthlyData.Legend.Visible := False;
    MineMonthlyDataDialog.ChtMonthlyData.View3D := False;
    MineMonthlyDataDialog.ChtMonthlyData.RemoveAllSeries;
    MineMonthlyDataDialog.ChtMonthlyData.SeriesList.Clear;

    MineMonthlyDataDialog.ChtMonthlyData.LeftAxis.Title.Caption := 'Mine '+ FEvaporationType + ' Evaporation (mm)';
    MineMonthlyDataDialog.ChtMonthlyData.LeftAxis.Title.Angle := 90;
    MineMonthlyDataDialog.ChtMonthlyData.BottomAxis.Title.Caption := FAppModules.Language.GetString('ChartCaption.Month');

    LSeries := TBarSeries.Create(MineMonthlyDataDialog.ChtMonthlyData);
    LSeries.ParentChart     := MineMonthlyDataDialog.ChtMonthlyData;
    LSeries.Marks.Visible   := False;
    LSeries.SeriesColor     := clRed;
    LSeries.BarWidthPercent := 60;
    LSeries.XValues.Order   := loAscending;
    LSeries.YValues.Order   := loNone;

    for LIndex := 1 to 12 do
    begin
      if StrToFloat(MineMonthlyDataDialog.GrdMothlyData.Cells[1,LIndex]) = NullFloat then
        LSeries.Add(0.0, LRunConfigurationData.MonthNameByIndex[LIndex])
      else
      LSeries.Add(StrToFloat(MineMonthlyDataDialog.GrdMothlyData.Cells[1,LIndex]), LRunConfigurationData.MonthNameByIndex[LIndex]);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMiningEvaporationValidator.OnAfterPasteColumnData(Sender: TObject);
const OPNAME = 'TMiningEvaporationValidator.OnAfterPasteColumnData';
var
  LMine  : IMine;
  LValue : double;
  LMonth : integer;
begin
  try
    LMine := TYieldModelDataObject(FAppModules.Model.ModelData).CastNetworkFeaturesData.CastMineList.MineByNodeNumber[FIdentifier];
    if(LMine <> nil) then
    begin
      if(EvaporationType = 'Pan') then
      begin
        for LMonth := 1 to 12 do
        begin
          LValue := StrToFloat(Trim(MineMonthlyDataDialog.GrdMothlyData.Cells[1,LMonth]));
          LMine.PanEvaporation[LMonth] := LValue;
        end;
      end
      else
      if(EvaporationType = 'Lake') then
      begin
        for LMonth := 1 to 12 do
        begin
          LValue := StrToFloat(Trim(MineMonthlyDataDialog.GrdMothlyData.Cells[1,LMonth]));
          LMine.LakeEvaporation[LMonth] := LValue;
        end;
      end;
      PopulateDataViewer;
      DoContextValidation(dvtReservoirEvaporation);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.

