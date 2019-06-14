//
//
//  UNIT      : Contains the class TMineMonthlyDataValidator.
//  AUTHOR    : Presley Mudau
//  DATE      : 2007/03/12
//  COPYRIGHT : Copyright © 2007 DWAF
//
//
unit UMineMonthlyDataValidator;

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
  TRechargeFactorType = (rftNone,rftOpenCast_DisturbedRechargeFactor, rftOpenCast_WorkingAreaRechargeFactor,
                        rftUnderGround_UpstreamRunoffPortion,rftUnderGround_BoardAndPilarRechargeFactor,
                        rftUnderGround_HighExtractionRechargeFactor,rftSlurryDump_RechargeFactor);
  TMineMonthlyDataValidator = class(TAbstractYieldDataDialogValidator)
  protected
    FIdentifier           : integer;
    FParentIdentifier     : integer;
    FRechargeFactorType   : TRechargeFactorType;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    procedure OnEditControlEnter(Sender: TObject); override;
    procedure OnEditControltExit(Sender: TObject); override;
    procedure OnStringGridCellDataHasChanged(ASender: TObject; ACol, ARow: integer);  override;
    procedure OnAfterPasteColumnData(Sender: TObject);
    procedure UpdateOpenCastRechargeFactors(AMonthNumber: integer; ANewValue: string);
    procedure UpdateUndergroundRechargeFactors(AMonthNumber: integer; ANewValue: string);
    procedure UpdateSlurryRechargeFactors(AMonthNumber: integer; ANewValue: string);

    procedure PopulateGrids;
    procedure PopulateMonthlyChart;
    procedure PopulateOpenCastRechargeFactorsGrid;
    procedure PopulateUndergroundRechargeFactorsGrid;
    procedure PopulateSlurryRechargeFactorsGrid;
    procedure ValidateEvaporation(AReservoir: IReservoirData);
    function OpenCastFactors: boolean;
    function UnderGroundFactors: boolean;
    function SlurryDumpFactors: boolean;
   public

    function Initialise: boolean; override;
    function SaveState: boolean; override;
    function LanguageHasChanged: boolean; override;
    function StudyHasChanged: boolean; override;
    function StudyDataHasChanged(AContext: TChangeContext; AFieldName,AOldValue,ANewValue: string): boolean; override;

    procedure ClearDataViewer; override;
    procedure PopulateDataViewer; override;
    procedure DoContextValidation(AValidationtype: TDialogValidationType); override;
    function MineMonthlyDataDialog: TMineMonthlyDataDialog;

    property Identifier               : integer read FIdentifier               write FIdentifier;
    property ParentIdentifier         : integer read FParentIdentifier         write FParentIdentifier;
    property RechargeFactorType       : TRechargeFactorType read FRechargeFactorType       write FRechargeFactorType;

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
  UMiningData,
  UErrorHandlingOperations, UNetworkFeaturesData;

{ TMineMonthlyDataValidator }

procedure TMineMonthlyDataValidator.CreateMemberObjects;
const OPNAME = 'TMineMonthlyDataValidator.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
    FPanel := TMineMonthlyDataDialog.Create(FPanelOwner,FAppModules);
    with MineMonthlyDataDialog do
    begin
      GrdMothlyData.OnEnter    := OnEditControlEnter;
      GrdMothlyData.OnExit     := OnEditControltExit;
      GrdMothlyData.OnColEnter := OnStringGridColEnter;
      GrdMothlyData.OnBeforeCellChange := OnStringGridCellDataHasChanged;
      GrdMothlyData.OnAfterPasteColumnData := Self.OnAfterPasteColumnData;
      GrdMothlyData.ShowGridPopupMenu    := True;
      GrdMothlyData.AllowPasteFromExcel  := True;
      GrdMothlyData.OnPasteFromExcel := Self.OnAfterPasteColumnData;
    end;
  // Handle exceptions.   }
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMineMonthlyDataValidator.DestroyMemberObjects;
const OPNAME = 'TMineMonthlyDataValidator.DestroyMemberObjects';
begin
  try
    inherited DestroyMemberObjects;

  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMineMonthlyDataValidator.Initialise: boolean;
const OPNAME = 'TMineMonthlyDataValidator.Initialise';
begin
  Result := inherited Initialise;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMineMonthlyDataValidator.LanguageHasChanged: boolean;
const OPNAME = 'TMineMonthlyDataValidator.LanguageHasChanged';
begin
  Result := False;
  try
    Result := inherited LanguageHasChanged;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMineMonthlyDataValidator.ClearDataViewer;
const OPNAME = 'TMineMonthlyDataValidator.ClearDataViewer';
var
  LIndex : integer;
begin
  inherited ClearDataViewer;
  try
    for LIndex := 1 to 12 do
      MineMonthlyDataDialog.GrdMothlyData.Cells[1,LIndex] := '';
    MineMonthlyDataDialog.ChtMonthlyData.SeriesList.Clear;
    MineMonthlyDataDialog.ChtMonthlyData.UndoZoom;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMineMonthlyDataValidator.PopulateDataViewer;
const OPNAME = 'TMineMonthlyDataValidator.PopulateDataViewer';
begin
  inherited PopulateDataViewer;
  try
   ClearDataViewer;
   PopulateGrids;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMineMonthlyDataValidator.SaveState: boolean;
const OPNAME = 'TMineMonthlyDataValidator.SaveState';
begin
  Result := inherited SaveState;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMineMonthlyDataValidator.OnEditControlEnter(Sender: TObject);
const OPNAME = 'TMineMonthlyDataValidator.OnEditControlEnter';
begin
  inherited OnEditControlEnter(Sender);
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMineMonthlyDataValidator.OnEditControltExit(Sender: TObject);
const OPNAME = 'TMineMonthlyDataValidator.OnEditControltExit';
begin
  inherited OnEditControltExit(Sender);
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMineMonthlyDataValidator.OnStringGridCellDataHasChanged(ASender: TObject; ACol, ARow: integer);
const OPNAME = 'TMineMonthlyDataValidator.OnStringGridCellDataHasChanged';
begin
  inherited OnStringGridCellDataHasChanged(ASender,ACol, ARow);
  try
    if((ASender = MineMonthlyDataDialog.GrdMothlyData) and OpenCastFactors) then
       UpdateOpenCastRechargeFactors(ARow,MineMonthlyDataDialog.GrdMothlyData.Cells[ACol, ARow]);
    if((ASender = MineMonthlyDataDialog.GrdMothlyData) and UnderGroundFactors) then
       UpdateUndergroundRechargeFactors(ARow,MineMonthlyDataDialog.GrdMothlyData.Cells[ACol, ARow]);
    if((ASender = MineMonthlyDataDialog.GrdMothlyData) and SlurryDumpFactors) then
       UpdateSlurryRechargeFactors(ARow,MineMonthlyDataDialog.GrdMothlyData.Cells[ACol, ARow]);

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMineMonthlyDataValidator.MineMonthlyDataDialog: TMineMonthlyDataDialog;
const OPNAME = 'TMineMonthlyDataValidator.MineMonthlyDataDialog';
begin
  Result := Nil;
  try
    if Assigned(FPanel) then
      Result := TMineMonthlyDataDialog(FPanel);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMineMonthlyDataValidator.StudyDataHasChanged(AContext: TChangeContext; AFieldName,AOldValue,ANewValue: string): boolean;
const OPNAME = 'TMineMonthlyDataValidator.StudyDataHasChanged';
begin
  Result := inherited StudyDataHasChanged(AContext,AFieldName,AOldValue,ANewValue);
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMineMonthlyDataValidator.StudyHasChanged: boolean;
const OPNAME = 'TMineMonthlyDataValidator.StudyHasChanged';
begin
  Result := inherited StudyHasChanged;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMineMonthlyDataValidator.PopulateGrids;
const OPNAME = 'TMineMonthlyDataValidator.PopulateGrids';
begin
  try
    if OpenCastFactors then PopulateOpenCastRechargeFactorsGrid;
    if UnderGroundFactors then PopulateUndergroundRechargeFactorsGrid;
    if SlurryDumpFactors then PopulateSlurryRechargeFactorsGrid;
    PopulateMonthlyChart;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMineMonthlyDataValidator.PopulateOpenCastRechargeFactorsGrid;
const OPNAME = 'TMineMonthlyDataValidator.PopulateOpenCastRechargeFactorsGrid';
var
  Lindex         : integer;
  LOpenCastData  : IOpenCast;
  LMine          : IMine;
  LMonthData     : IRunConfigurationData;
begin
  try
    with MineMonthlyDataDialog do
    begin
      LMine := TYieldModelDataObject(FAppModules.Model.ModelData).CastNetworkFeaturesData
                 .CastMineList.MineByNodeNumber[FIdentifier];
      LMonthData := TYieldModelDataObject(FAppModules.Model.ModelData).RunConfigurationData;
      if (LMine <> nil) then
      begin
        LOpenCastData :=  LMine.OpenCastByIdentifier[FParentIdentifier];
        if (LOpenCastData <> nil) then
        begin
          if (RechargeFactorType = rftOpenCast_DisturbedRechargeFactor) then
          begin
            GrdMothlyData.ClearFieldProperties;
            GrdMothlyData.Cells[0,0]   := FAppModules.Language.GetString('GridHeading.Month');
            GrdMothlyData.Cells[1,0]   := FAppModules.Language.GetString('GridHeading.DisturbedMonthlyRecharge');;
            GrdMothlyData.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('Month'));
            GrdMothlyData.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('OpenCastDisturbedAreaRechargeFactors'));
            for Lindex := 1 to 12 do
            begin
              GrdMothlyData.SetFieldValue(0,Lindex,LMonthData.MonthNameByIndex[Lindex]);
              GrdMothlyData.SetFieldValue(1, LIndex,LOpenCastData.DisturbedRechargeFactor[LIndex]);
            end;
          end
          else
          if (RechargeFactorType = rftOpenCast_WorkingAreaRechargeFactor) then
          begin
            GrdMothlyData.ClearFieldProperties;
            GrdMothlyData.Cells[0,0]   := FAppModules.Language.GetString('GridHeading.Month');
            GrdMothlyData.Cells[1,0]   := FAppModules.Language.GetString('GridHeading.DisturbedWorkingsMonthlyRecharge');;
            GrdMothlyData.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('Month'));
            GrdMothlyData.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('OpenCastDisturbedWorkingsRechargeFactors'));
            for Lindex := 1 to 12 do
            begin
              GrdMothlyData.SetFieldValue(0,Lindex,LMonthData.MonthNameByIndex[Lindex]);
              GrdMothlyData.SetFieldValue(1, LIndex,LOpenCastData.WorkingAreaRechargeFactor[LIndex]);
            end;
          end
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMineMonthlyDataValidator.PopulateUndergroundRechargeFactorsGrid;
const OPNAME = 'TMineMonthlyDataValidator.PopulateUndergroundRechargeFactorsGrid';
var
  Lindex           : integer;
  LUndergroundData : IUnderground;
  LMine            : IMine;
  LMonthData       : IRunConfigurationData;
begin
  try
    with MineMonthlyDataDialog do
    begin
      LMine := TYieldModelDataObject(FAppModules.Model.ModelData).CastNetworkFeaturesData
                 .CastMineList.MineByNodeNumber[FIdentifier];
      LMonthData := TYieldModelDataObject(FAppModules.Model.ModelData).RunConfigurationData;
      if (LMine <> nil) then
      begin
        LUndergroundData :=  LMine.UnderGroundByIdentifier[ParentIdentifier];
        if (LUndergroundData <> nil) then
        begin
          if (RechargeFactorType = rftUnderGround_UpstreamRunoffPortion) then
          begin
            GrdMothlyData.ClearFieldProperties;
            GrdMothlyData.Cells[0,0]   := FAppModules.Language.GetString('GridHeading.Month');
            GrdMothlyData.Cells[1,0]   := FAppModules.Language.GetString('GridHeading.MineUGUpstreamRunoff');;
            GrdMothlyData.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('Month'));
            GrdMothlyData.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('MineUGUpstreamRunoff'));
            for Lindex := 1 to 12 do
            begin
              GrdMothlyData.SetFieldValue(0,Lindex,LMonthData.MonthNameByIndex[Lindex]);
              GrdMothlyData.SetFieldValue(1, LIndex,LUndergroundData.UpstreamRunoffPortion[LIndex]);
            end;
          end
          else
          if (RechargeFactorType = rftUnderGround_BoardAndPilarRechargeFactor) then
          begin
            GrdMothlyData.ClearFieldProperties;
            GrdMothlyData.Cells[0,0]   := FAppModules.Language.GetString('GridHeading.Month');
            GrdMothlyData.Cells[1,0]   := FAppModules.Language.GetString('GridHeading.UGBoardPillarRechargeFactors');;
            GrdMothlyData.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('Month'));
            GrdMothlyData.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('UGBoardPillarRechargeFactors'));
            for Lindex := 1 to 12 do
            begin
              GrdMothlyData.SetFieldValue(0,Lindex,LMonthData.MonthNameByIndex[Lindex]);
              GrdMothlyData.SetFieldValue(1, LIndex,LUndergroundData.BoardAndPilarRechargeFactor[LIndex]);
            end;
          end
          else
          if (RechargeFactorType = rftUnderGround_HighExtractionRechargeFactor) then
          begin
            GrdMothlyData.ClearFieldProperties;
            GrdMothlyData.Cells[0,0]   := FAppModules.Language.GetString('GridHeading.Month');
            GrdMothlyData.Cells[1,0]   := FAppModules.Language.GetString('GridHeading.UGHighExtractionRechargeFactors');;
            GrdMothlyData.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('Month'));
            GrdMothlyData.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('UGHighExtractionRechargeFactors'));
            for Lindex := 1 to 12 do
            begin
              GrdMothlyData.SetFieldValue(0,Lindex, LMonthData.MonthNameByIndex[Lindex]);
              GrdMothlyData.SetFieldValue(1, LIndex,LUndergroundData.HighExtractionRechargeFactor[LIndex]);
            end;
          end
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMineMonthlyDataValidator.PopulateSlurryRechargeFactorsGrid;
const OPNAME = 'TMineMonthlyDataValidator.PopulateSlurryRechargeFactorsGrid';
var
  Lindex         : integer;
  LSlurryData    : ISlurryDump;
  LMine          : IMine;
  LMonthData     : IRunConfigurationData;
begin
  try
    with MineMonthlyDataDialog do
    begin
      GrdMothlyData.ClearFieldProperties;
      LMine := TYieldModelDataObject(FAppModules.Model.ModelData).CastNetworkFeaturesData
                 .CastMineList.MineByNodeNumber[FIdentifier];
      LMonthData := TYieldModelDataObject(FAppModules.Model.ModelData).RunConfigurationData;
      if (LMine <> nil) then
      begin
        LSlurryData :=  LMine.SlurryDumpByIdentifier[ParentIdentifier];
        if (LSlurryData <> nil) then
        begin
            GrdMothlyData.ClearFieldProperties;
            GrdMothlyData.Cells[0,0]   := FAppModules.Language.GetString('GridHeading.Month');
            GrdMothlyData.Cells[1,0]   := FAppModules.Language.GetString('TField.DumpRechargeFactorsDescr');;
            GrdMothlyData.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('Month'));
            GrdMothlyData.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('DumpRechargeFactors'));
          GrdMothlyData.AddFieldProperty(FAppModules.FieldProperties.FieldProperty(''));
          for Lindex := 1 to 12 do
          begin
            GrdMothlyData.SetFieldValue(0,Lindex,LMonthData.MonthNameByIndex[Lindex]);
            GrdMothlyData.SetFieldValue(1, LIndex,LSlurryData.RechargeFactor[LIndex]);
          end;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMineMonthlyDataValidator.UpdateOpenCastRechargeFactors(AMonthNumber: integer; ANewValue: string);
const OPNAME = 'TMineMonthlyDataValidator.UpdateOpenCastRechargeFactors';
var
  Lindex    : integer;
  LMine     : IMine;
  LOpenCast : IOpenCast;
  LValue    : double;
begin
  try
    LValue := StrToFloat(ANewValue);
    LMine := TYieldModelDataObject(FAppModules.Model.ModelData).CastNetworkFeaturesData
             .CastMineList.MineByNodeNumber[FIdentifier];
    if (LMine <> nil) then
    begin
      LOpenCast := LMine.OpenCastByIdentifier[FParentIdentifier];
      if (LOpenCast <> nil) then
      begin
        if (RechargeFactorType = rftOpenCast_DisturbedRechargeFactor) then
        begin
          for Lindex := 1 to 12 do
          begin
            LOpenCast.DisturbedRechargeFactor[AMonthNumber] := LValue;
            MineMonthlyDataDialog.GrdMothlyData.SetFieldValue(1,AMonthNumber,LOpenCast.DisturbedRechargeFactor[AMonthNumber]);
          end;
        end;

        if (RechargeFactorType = rftOpenCast_WorkingAreaRechargeFactor) then
        begin
          for Lindex := 1 to 12 do
          begin
            LOpenCast.WorkingAreaRechargeFactor[AMonthNumber] := LValue;
            MineMonthlyDataDialog.GrdMothlyData.SetFieldValue(1,AMonthNumber,LOpenCast.WorkingAreaRechargeFactor[AMonthNumber]);
          end;
        end;

      end;
      PopulateDataViewer;
      DoContextValidation(dvtReservoirEvaporation);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMineMonthlyDataValidator.UpdateSlurryRechargeFactors(AMonthNumber: integer; ANewValue: string);
const OPNAME = 'TMineMonthlyDataValidator.UpdateSlurryRechargeFactors';
var
  Lindex       : integer;
  LMine        : IMine;
  LSlurryData  : ISlurryDump;
  LValue       : double;
begin
  try
    LValue := StrToFloat(ANewValue);
    LMine := TYieldModelDataObject(FAppModules.Model.ModelData).CastNetworkFeaturesData
             .CastMineList.MineByNodeNumber[FIdentifier];
    if (LMine <> nil) then
    begin
      LSlurryData := LMine.SlurryDumpByIdentifier[FParentIdentifier];
      if (LSlurryData <> nil) then
      begin
        for Lindex := 1 to 12 do
        begin
          LSlurryData.RechargeFactor[AMonthNumber] := LValue;
          MineMonthlyDataDialog.GrdMothlyData.SetFieldValue(1,AMonthNumber,LSlurryData.RechargeFactor[AMonthNumber]);
        end;
      end;
      PopulateDataViewer;
      DoContextValidation(dvtReservoirEvaporation);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMineMonthlyDataValidator.UpdateUndergroundRechargeFactors(AMonthNumber: integer; ANewValue: string);
const OPNAME = 'TMineMonthlyDataValidator.UpdateUndergroundRechargeFactors';
var
  Lindex       : integer;
  LMine        : IMine;
  LUnderGround : IUnderground;
  LValue       : double;
begin
  try
    LValue := StrToFloat(ANewValue);
    LMine := TYieldModelDataObject(FAppModules.Model.ModelData).CastNetworkFeaturesData
             .CastMineList.MineByNodeNumber[FIdentifier];
    if (LMine <> nil) then
    begin
      LUnderGround := LMine.UnderGroundByIdentifier[FParentIdentifier];
      if (LUnderGround <> nil) then
      begin
        if (RechargeFactorType = rftUnderGround_UpstreamRunoffPortion) then
        begin
          for Lindex := 1 to 12 do
          begin
            LUnderGround.UpstreamRunoffPortion[AMonthNumber] := LValue;
            MineMonthlyDataDialog.GrdMothlyData.SetFieldValue(1,AMonthNumber,LUnderGround.UpstreamRunoffPortion[AMonthNumber]);
          end;
        end;

        if (RechargeFactorType = rftUnderGround_BoardAndPilarRechargeFactor) then
        begin
          for Lindex := 1 to 12 do
          begin
            LUnderGround.BoardAndPilarRechargeFactor[AMonthNumber] := LValue;
            MineMonthlyDataDialog.GrdMothlyData.SetFieldValue(1,AMonthNumber,LUnderGround.BoardAndPilarRechargeFactor[AMonthNumber]);
          end;
        end;

        if (RechargeFactorType = rftUnderGround_HighExtractionRechargeFactor) then
        begin
          for Lindex := 1 to 12 do
          begin
            LUnderGround.HighExtractionRechargeFactor[AMonthNumber] := LValue;
            MineMonthlyDataDialog.GrdMothlyData.SetFieldValue(1,AMonthNumber,LUnderGround.HighExtractionRechargeFactor[AMonthNumber]);
          end;
        end;
      end;
      PopulateDataViewer;
      DoContextValidation(dvtReservoirEvaporation);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMineMonthlyDataValidator.PopulateMonthlyChart;
const OPNAME = 'TMineMonthlyDataValidator.PopulateMonthlyChart';
var
  LIndex                 : integer;
  LSeries                : TCustomBarSeries;
  LRunConfigurationData  : IRunConfigurationData;
begin
  try
    LRunConfigurationData := TYieldModelDataObject(FAppModules.Model.ModelData).RunConfigurationData;

    MineMonthlyDataDialog.ChtMonthlyData.AllowPanning := pmBoth;  // possibly disable to allow dragging of bars
    MineMonthlyDataDialog.ChtMonthlyData.AllowZoom := True;        // possibly disable to allow dragging of bars
    //MineMonthlyDataDialog.ChtMonthlyData.BackWall.Brush.Color := clWhite;
    //MineMonthlyDataDialog.ChtMonthlyData.BackWall.Brush.Style := bsClear;
    //MineMonthlyDataDialog.ChtMonthlyData.BackWall.Pen.Visible := False;
    MineMonthlyDataDialog.ChtMonthlyData.Title.Text.Text := '';
    MineMonthlyDataDialog.ChtMonthlyData.AxisVisible := True;
    MineMonthlyDataDialog.ChtMonthlyData.ClipPoints := True;
    MineMonthlyDataDialog.ChtMonthlyData.Frame.Visible := True;
    MineMonthlyDataDialog.ChtMonthlyData.Legend.Visible := False;
    MineMonthlyDataDialog.ChtMonthlyData.View3D := False;
    //MineMonthlyDataDialog.ChtMonthlyData.View3DWalls := False;
    MineMonthlyDataDialog.ChtMonthlyData.RemoveAllSeries;
    MineMonthlyDataDialog.ChtMonthlyData.SeriesList.Clear;
    MineMonthlyDataDialog.ChtMonthlyData.LeftAxis.Title.Angle := 90;
    MineMonthlyDataDialog.ChtMonthlyData.BottomAxis.Title.Caption := FAppModules.Language.GetString('TField.Month');

    // Series
    LSeries := TBarSeries.Create(MineMonthlyDataDialog.ChtMonthlyData);
    LSeries.ParentChart := MineMonthlyDataDialog.ChtMonthlyData;
    LSeries.Marks.Visible := False;
    LSeries.SeriesColor := clRed;


    LSeries.BarWidthPercent := 80;
    LSeries.XValues.Order := loAscending;
    LSeries.YValues.Order := loNone;

    if (FRechargeFactorType = rftOpenCast_DisturbedRechargeFactor) then
    begin
      MineMonthlyDataDialog.ChtMonthlyData.LeftAxis.Title.Caption := FAppModules.Language.GetString('GridHeading.DisturbedMonthlyRecharge');
      LSeries.Title := FAppModules.Language.GetString('ReservoirSeries.EvaporationSeries');
    end
    else if (FRechargeFactorType = rftOpenCast_WorkingAreaRechargeFactor) then
    begin
      MineMonthlyDataDialog.ChtMonthlyData.LeftAxis.Title.Caption := FAppModules.Language.GetString('GridHeading.DisturbedWorkingsMonthlyRecharge');
      LSeries.Title := FAppModules.Language.GetString('ReservoirSeries.EvaporationSeries');
    end
    else if (FRechargeFactorType = rftUnderGround_UpstreamRunoffPortion) then
    begin
      MineMonthlyDataDialog.ChtMonthlyData.LeftAxis.Title.Caption := FAppModules.Language.GetString('GridHeading.MineUGUpstreamRunoff');
      LSeries.Title := FAppModules.Language.GetString('ReservoirSeries.EvaporationSeries');
    end
    else if (FRechargeFactorType = rftUnderGround_BoardAndPilarRechargeFactor) then
    begin
      MineMonthlyDataDialog.ChtMonthlyData.LeftAxis.Title.Caption := FAppModules.Language.GetString('GridHeading.UGBoardPillarRechargeFactors');
      LSeries.Title := FAppModules.Language.GetString('ReservoirSeries.EvaporationSeries');
    end
    else if (FRechargeFactorType = rftUnderGround_HighExtractionRechargeFactor) then
    begin
      MineMonthlyDataDialog.ChtMonthlyData.LeftAxis.Title.Caption := FAppModules.Language.GetString('GridHeading.UGHighExtractionRechargeFactors');
      LSeries.Title := FAppModules.Language.GetString('ReservoirSeries.EvaporationSeries');
    end
    else if (FRechargeFactorType = rftSlurryDump_RechargeFactor) then
    begin
      MineMonthlyDataDialog.ChtMonthlyData.LeftAxis.Title.Caption := FAppModules.Language.GetString('GridHeading.DumpRechargeFactorsDescr');
      LSeries.Title := FAppModules.Language.GetString('TField.DumpRechargeFactorsDescr');
    end;

    for LIndex := 1 to 12 do
    begin
      if(Trim(MineMonthlyDataDialog.GrdMothlyData.Cells[1, LIndex]) = '') or
        (StrToFloat(MineMonthlyDataDialog.GrdMothlyData.Cells[1, LIndex]) = NullFloat) then
        LSeries.Add(0.0, LRunConfigurationData.MonthNameByIndex[LIndex])
      else
      LSeries.Add( StrToFloat(MineMonthlyDataDialog.GrdMothlyData.Cells[1, LIndex]), LRunConfigurationData.MonthNameByIndex[LIndex]);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMineMonthlyDataValidator.OnAfterPasteColumnData(Sender: TObject);
const OPNAME = 'TMineMonthlyDataValidator.OnAfterPasteColumnData';
var
  LIndex       : integer;
  LMine        : IMine;
  LValue       : double;
  LOpenCast    : IOpenCast;
  LSlurryData  : ISlurryDump;
  LUnderGround : IUnderground;
begin
  try
    LMine := TYieldModelDataObject(FAppModules.Model.ModelData).CastNetworkFeaturesData.CastMineList.MineByNodeNumber[FIdentifier];
    if(LMine <> nil) then
    begin
      if((Sender = MineMonthlyDataDialog.GrdMothlyData) and OpenCastFactors) then
      begin
        LOpenCast := LMine.OpenCastByIdentifier[FParentIdentifier];
        if(LOpenCast <> nil) then
        begin
           if(RechargeFactorType = rftOpenCast_DisturbedRechargeFactor) then
          begin
            for LIndex := 1 to 12 do
            begin
              LValue := StrToFloat(Trim(MineMonthlyDataDialog.GrdMothlyData.Cells[1,LIndex]));
              LOpenCast.DisturbedRechargeFactor[LIndex] := LValue;
            end;
          end;

          if(RechargeFactorType = rftOpenCast_WorkingAreaRechargeFactor) then
          begin
            for LIndex := 1 to 12 do
            begin
              LValue := StrToFloat(Trim(MineMonthlyDataDialog.GrdMothlyData.Cells[1,LIndex]));
              LOpenCast.WorkingAreaRechargeFactor[LIndex] := LValue;
            end;
          end;
        end;
      end;

      if((Sender = MineMonthlyDataDialog.GrdMothlyData) and UnderGroundFactors) then
      begin
        LUnderGround := LMine.UnderGroundByIdentifier[FParentIdentifier];
        if(LUnderGround <> nil) then
        begin
          if(RechargeFactorType = rftUnderGround_UpstreamRunoffPortion) then
          begin
            for LIndex := 1 to 12 do
            begin
              LValue := StrToFloat(Trim(MineMonthlyDataDialog.GrdMothlyData.Cells[1,LIndex])); 
              LUnderGround.UpstreamRunoffPortion[LIndex] := LValue;
            end;
          end;

          if(RechargeFactorType = rftUnderGround_BoardAndPilarRechargeFactor) then
          begin
            for LIndex := 1 to 12 do
            begin
              LValue := StrToFloat(Trim(MineMonthlyDataDialog.GrdMothlyData.Cells[1,LIndex])); 
              LUnderGround.BoardAndPilarRechargeFactor[LIndex] := LValue;
            end;
          end;

          if(RechargeFactorType = rftUnderGround_HighExtractionRechargeFactor) then
          begin
            for Lindex := 1 to 12 do
            begin
              LValue := StrToFloat(Trim(MineMonthlyDataDialog.GrdMothlyData.Cells[1,LIndex])); 
              LUnderGround.HighExtractionRechargeFactor[LIndex] := LValue;
            end;
          end;
        end;
      end;

      if((Sender = MineMonthlyDataDialog.GrdMothlyData) and SlurryDumpFactors) then
      begin
        LSlurryData := LMine.SlurryDumpByIdentifier[FParentIdentifier];
        if(LSlurryData <> nil) then
        begin
          for LIndex := 1 to 12 do
          begin
            LValue := StrToFloat(Trim(MineMonthlyDataDialog.GrdMothlyData.Cells[1,LIndex]));
            LSlurryData.RechargeFactor[LIndex] := LValue;
          end;
        end;
      end;
      PopulateDataViewer;
      DoContextValidation(dvtReservoirEvaporation);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMineMonthlyDataValidator.DoContextValidation(AValidationtype: TDialogValidationType);
const OPNAME = 'TMineMonthlyDataValidator.DoContextValidation';
var
  LReservoir    : IReservoirData;
  LReservoirList: IReservoirDataList;
begin
  try
    FAllErrorMessages.Clear;
    if(FIdentifier >= 0) then
    begin
      LReservoirList := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.
                        ReservoirList;
      LReservoir := LReservoirList.ReservoirByIdentifier[FIdentifier];
      if(LReservoir <> nil) then
      begin
        if(AValidationType in [dvtReservoirEvaporationAll, dvtReservoirEvaporation]) then
          ValidateEvaporation(LReservoir);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMineMonthlyDataValidator.ValidateEvaporation(AReservoir: IReservoirData);
const OPNAME = 'TMineMonthlyDataValidator.ValidateEvaporation';
var
  LCol           : integer;
  LIndex         : integer;
  LErrorCols     : TStringList;
  LErrorMessages : TStringList;
begin
  try
    if(AReservoir <> nil) then
    begin
      with MineMonthlyDataDialog do
      begin
        LErrorCols     := TStringList.Create;
        LErrorMessages := TStringList.Create;
        try
          FErrorMessage := '';
          LErrorCols.Clear;
          if(AReservoir.Validate(FErrorMessage,'Evaporation')) then
          begin
            for LCol := 1 to 12 do
              GrdMothlyData.ValidationError[1, LCol, gveCellContext] := ''
          end
          else
          begin
            ExtractErrorsAndColumns(FErrorMessage, LErrorMessages, LErrorCols);
            for LCol := 1 to 12 do
            begin
              LIndex := LErrorCols.IndexOf(IntToStr(LCol));
              if(LIndex >= 0) then
                GrdMothlyData.ValidationError[1, LCol, gveCellContext] := LErrorMessages.Strings[LIndex]
              else
                GrdMothlyData.ValidationError[1, LCol, gveCellContext] := '';
            end;
              FAllErrorMessages.Add(FErrorMessage);
          end;
        finally
          FreeAndNil(LErrorCols);
          FreeAndNil(LErrorMessages);
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMineMonthlyDataValidator.OpenCastFactors: boolean;
const OPNAME = 'TMineMonthlyDataValidator.OpenCastFactors';
begin
  Result := False;
  try
    Result := FRechargeFactorType in [rftOpenCast_DisturbedRechargeFactor, rftOpenCast_WorkingAreaRechargeFactor];
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMineMonthlyDataValidator.SlurryDumpFactors: boolean;
const OPNAME = 'TMineMonthlyDataValidator.SlurryDumpFactors';
begin
  Result := False;
  try
    Result := FRechargeFactorType in [rftSlurryDump_RechargeFactor];
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMineMonthlyDataValidator.UnderGroundFactors: boolean;
const OPNAME = 'TMineMonthlyDataValidator.UnderGroundFactors';
begin
  Result := False;
  try
    Result := FRechargeFactorType in [rftUnderGround_UpstreamRunoffPortion,rftUnderGround_BoardAndPilarRechargeFactor,
                        rftUnderGround_HighExtractionRechargeFactor];
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.

