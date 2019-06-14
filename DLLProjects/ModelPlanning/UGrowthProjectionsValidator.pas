//
//
//  UNIT      : Contains the class TGrowthProjectionsValidator.
//  AUTHOR    : Dziedzi Ramulondi (Cornastone)
//  DATE      : 2006/05/11
//  COPYRIGHT : Copyright © 2006 DWAF
//
//
unit UGrowthProjectionsValidator;

interface

uses
  Classes,
  VCL.Controls,
  VCL.ComCtrls,
  VCL.StdCtrls,
  VoaimsCom_TLB,
  VCL.ExtCtrls,
  Types,
  VCL.Grids,
  UAbstractObject,
  UAbstractComponent,
  UDataComponent,
  UDataEditComponent,
  UYieldModelDataObject,
  UYieldContextValidationType,
  UAbstractYieldDataDialogValidator,
  UGrowthFactorsExcelData,
  UGrowthFactorData,
  UGrowthProjectionsDialog;

  type
  TGrowthProjectionsValidator = class(TAbstractYieldDataDialogValidator)
  protected

    FBaseYearIndex,
    FStartYearIndex : Integer;
    procedure CreateMemberObjects; override;
    procedure OnEditControltExit(Sender: TObject); override;
    procedure OnPasteFromExcelClick(Sender: TObject);
    procedure OnGenerateFactorsClick(Sender: TObject);
    //procedure OnGenerateMinMaxFactorsClick(Sender: TObject);
    procedure OnValidateProjectionsClick(Sender: TObject);
    procedure OnViewItemHasChanged(Sender: TObject);
    procedure OnTabChanged(Sender: Tobject);
   // procedure OnBaseYearChange(ASender : TObject);
    procedure RepopulateDataViewer;
    procedure PopulateGrowthFactorsFromExcel;
    procedure RemoveEmptyColumn(AGrid: TFieldStringGrid);
    procedure OnStringGridCellDataHasChanged(ASender: TObject; ACol, ARow: integer);  override;
    function ExecValidateGrowthFiles(AProgressUpdateFuntion:TProgressUpdateFuntion):boolean; 

    procedure DoDrawGrid (Sender: TObject; ACol, ARow: Longint; Rect: TRect; State: TGridDrawState);

    procedure UpdateNumYears(AGrowthFactors: TExelGrowthFactors);
    procedure UpdateStartYear(AGrowthFactors: TExelGrowthFactors);
   // procedure UpdateBaseYear(AGrowthFactors: TExelGrowthFactors);
    procedure UpdateInstitution(ACol, ARow: integer);
    procedure UpdateWaterUser(ACol, ARow: integer);
    procedure UpdateChannelNumber(ACol, ARow: integer);

    procedure ValidateNumYears(AGrowthFactors: TExelGrowthFactors);
    procedure ValidateStartYear(AGrowthFactors: TExelGrowthFactors);
//    procedure ValidateBaseYear(AGrowthFactors: TExelGrowthFactors);

    procedure UpdateExcelDemandGrowthFactors(ACol, ARow:integer);
    procedure UpdateMinMaxChannelGrowthFactors(ACol, ARow:integer);
    procedure UpdateHydrologyGrowthFactors(ACol, ARow:integer);

    function ValidateDataFromExcel: boolean;
    function ValidateChannelNumbersFromExcel: boolean;
    function ValidateDemandCentreGrowthFactorsFromExcel: boolean;
    function ValidateMinMaxChannelGrowthFactorsFromExcel: boolean;
    function ValidateHydrologyGrowthFactorsFromExcel: boolean;

    procedure PopulateYears(AGrowthFactors : TExelGrowthFactors;AGrid: TFieldStringGrid);
    procedure PopulateDemandCentreGrowthFactorsGrid(AGrowthFactors : TExelGrowthFactors);
    procedure PopulateMinMaxChannelGrowthFactorsGrid(AGrowthFactors : TExelGrowthFactors);
    procedure RePopulateMinMaxChannelGrowthFactorsGrid(AGrid: TFieldStringGrid); //AGrowthFactors: TExelGrowthFactors);
    procedure PopulateHydrologyGrowthFactorsGrid(AGrowthFactors : TExelGrowthFactors);
    procedure RePopulateHydrologyGrowthFactorsGrid(AGrid: TFieldStringGrid );//; AGrowthFactors : TExelGrowthFactors);

    function PopulateYearsObject(AFactors : TExelGrowthFactors):boolean;
    function PopulateDemandChannelGrowthFactorsObject(AFactors : TExelGrowthFactors):boolean;
    function PopulateMinMaxChannelGrowthFactorsObject(AFactors : TExelGrowthFactors):boolean;
    function PopulateHydrologyGrowthFactorsObject(AFactors     : TExelGrowthFactors):boolean;
    function GetCurrentGrid : TFieldStringGrid;
  public
    function Initialise: boolean; override;
    function LanguageHasChanged: boolean; override;
    function StudyHasChanged: boolean; override;
    function StudyDataHasChanged(AContext: TChangeContext; AFieldName,AOldValue,ANewValue: string): boolean; override;
    function SearchGaugeNumber(AGaugeNo: string; AGaugerList: TStringList): boolean;
    procedure ClearDataViewer; override;
    procedure PopulateDataViewer; override;

    function GrowthProjectionsDialog: TGrowthProjectionsDialog;
    procedure DoContextValidation(AValidationType: TDialogValidationType);override;
    property CurrentGrid : TFieldStringGrid read GetCurrentGrid;
  end;

implementation

uses
  System.UITypes,
  SysUtils,
  VCL.Dialogs,
  UConstants,
  VCL.Graphics,
  UUtilities,
  UProgressDialog,
  UPlanningModelDataObject,
  UErrorHandlingOperations, ConvUtils;

{ TGrowthProjectionsValidator }

procedure TGrowthProjectionsValidator.CreateMemberObjects;
const OPNAME = 'TGrowthProjectionsValidator.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
    FPanel := TGrowthProjectionsDialog.Create(FPanelOwner,FAppModules);

    with GrowthProjectionsDialog do
    begin

     
      //edtNumYears.FieldProperty := FAppModules.FieldProperties.FieldProperty('GrowthProjectionYearsCount');
      edtNumYears.OnEnter       := OnEditControlEnter;
      edtNumYears.OnExit        := OnEditControltExit;
      edtNumYears.IsEnabled     := True;

      //edtStartYear.FieldProperty := FAppModules.FieldProperties.FieldProperty('GrowthProjectionStartYear');
      edtStartYear.Color         := clBtnShadow;
      edtStartYear.OnEnter       := OnEditControlEnter;
      edtStartYear.OnExit        := OnEditControltExit;
      edtStartYear.IsEnabled     := True;

      edtBaseYear.Color         := clBtnShadow;
      edtBaseYear.IsEnabled     := False;
      edtBaseYear.OnEnter       := OnEditControlEnter;
      edtBaseYear.OnExit        := OnEditControltExit;

      //BaseYearCbx.FieldProperty := FAppModules.FieldProperties.FieldProperty('GrowthProjectionBaseYear');
      //BaseYearCbx.OnEnter       := OnEditControlEnter;
      //BaseYearCbx.OnChange      := OnBaseYearChange;
      //BaseYearCbx.IsEnabled     := True;
      
      pgcProjections.OnChange   := OnTabChanged;

      btnGenerateFactors.OnClick             := OnGenerateFactorsClick;
      btnValidateProjections.OnClick         := OnValidateProjectionsClick;
     // btnGenMinMaxFactors.OnClick            := OnGenerateMinMaxFactorsClick;

      strgrdDemandFactors.AllowPasteFromExcel := True;
      strgrdDemandFactors.OnPasteFromExcel    := OnPasteFromExcelClick;
      strgrdDemandFactors.OnBeforeCellChange  := OnStringGridCellDataHasChanged;
      strgrdDemandFactors.OnDrawCell          := DoDrawGrid;

      strgrdMinMaxFactors.AllowPasteFromExcel := True;
      strgrdMinMaxFactors.OnPasteFromExcel    := OnPasteFromExcelClick;
      strgrdMinMaxFactors.OnBeforeCellChange  := OnStringGridCellDataHasChanged;
      strgrdMinMaxFactors.OnDrawCell          := DoDrawGrid;

      strgrdHydrologyFactors.AllowPasteFromExcel := True;
      strgrdHydrologyFactors.OnPasteFromExcel    := OnPasteFromExcelClick;
      strgrdHydrologyFactors.OnBeforeCellChange  := OnStringGridCellDataHasChanged;
      strgrdHydrologyFactors.OnDrawCell          := DoDrawGrid;

                                                      
{      edtNumYears.FieldProperty         := FAppModules.FieldProperties.FieldProperty('ArcNumber');
      edtStartYear.FieldProperty        := FAppModules.FieldProperties.FieldProperty('ArcNumber');
      edtBaseYear.FieldProperty          := FAppModules.FieldProperties.FieldProperty('ArcNumber');}
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TGrowthProjectionsValidator.Initialise: boolean;
const OPNAME = 'TGrowthProjectionsValidator.Initialise';
var
  LGrowthFactors: TExelGrowthFactors;
begin
  Result := inherited Initialise;
  try
    ClearDataViewer;
    FStartYearIndex := -1;
    FBaseYearIndex  := -1;
    LGrowthFactors := TPlanningModelDataObject(FAppModules.Model.ModelData).CastExelGrowthFactors;
    if not LGrowthFactors.Populated  then
      LGrowthFactors.LoadFromDB;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TGrowthProjectionsValidator.LanguageHasChanged: boolean;
const OPNAME = 'TGrowthProjectionsValidator.LanguageHasChanged';
begin
  Result := False;
  try
    TabShetCaption := FAppModules.Language.GetString('TabCaption.GrowthProjections');
    Result := inherited LanguageHasChanged;

    GrowthProjectionsDialog.strgrdDemandFactors.Cells[0,0] := FAppModules.language.GetString('GridHeading.Institutions');
    GrowthProjectionsDialog.strgrdDemandFactors.Cells[1,0] := FAppModules.language.GetString('GridHeading.Water Users');
    GrowthProjectionsDialog.strgrdDemandFactors.Cells[2,0] := FAppModules.language.GetString('TField.MinMaxChannelNumber');;
    GrowthProjectionsDialog.strgrdDemandFactors.Cells[3,0] := 'Base Demand'; //FAppModules.language.GetString('GridHeading.BaseYearDemand');

    GrowthProjectionsDialog.strgrdMinMaxFactors.Cells[0,0] := FAppModules.language.GetString('GridHeading.Institutions');
    GrowthProjectionsDialog.strgrdMinMaxFactors.Cells[1,0] := FAppModules.language.GetString('GridHeading.Water Users');
    GrowthProjectionsDialog.strgrdMinMaxFactors.Cells[2,0] := FAppModules.language.GetString('TField.MinMaxChannelNumber');
    GrowthProjectionsDialog.strgrdMinMaxFactors.Cells[3,0] := FAppModules.language.GetString('GridHeading.ArcNumber');
    GrowthProjectionsDialog.strgrdMinMaxFactors.Cells[4,0] := 'Base Demand';

    GrowthProjectionsDialog.strgrdHydrologyFactors.Cells[0,0] := FAppModules.language.GetString('GridHeading.Institutions');
    GrowthProjectionsDialog.strgrdHydrologyFactors.Cells[1,0] := FAppModules.language.GetString('GridHeading.Water Users');
    GrowthProjectionsDialog.strgrdHydrologyFactors.Cells[2,0] := FAppModules.language.GetString('GridHeading.GaugeNumber');
    GrowthProjectionsDialog.strgrdHydrologyFactors.Cells[3,0] := FAppModules.language.GetString('GridHeading.FactorType');
//    GrowthProjectionsDialog.strgrdHydrologyFactors.Cells[4,0] := FAppModules.language.GetString('GridHeading.BaseYearDemand');

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TGrowthProjectionsValidator.StudyDataHasChanged(AContext: TChangeContext; AFieldName,AOldValue,ANewValue: string): boolean;
const OPNAME = 'TGrowthProjectionsValidator.StudyDataHasChanged';
begin
  Result := inherited StudyDataHasChanged(AContext,AFieldName,AOldValue,ANewValue);
  try
  if( AFieldName = 'GrowthFactors') and (AContext = sdccAdd)then
     PopulateDataViewer;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TGrowthProjectionsValidator.StudyHasChanged: boolean;
const OPNAME = 'TGrowthProjectionsValidator.StudyHasChanged';
begin
  Result := inherited StudyHasChanged;
  try

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TGrowthProjectionsValidator.GrowthProjectionsDialog : TGrowthProjectionsDialog;
const OPNAME = 'TGrowthProjectionsValidator.GrowthProjectionsDialog';
begin
  Result := nil;
  try
    if Assigned(FPanel) then
      Result := TGrowthProjectionsDialog(FPanel);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TGrowthProjectionsValidator.ClearDataViewer;
const OPNAME = 'TGrowthProjectionsValidator.ClearDataViewer';
var
  LCol,
  LRow : integer;
begin
  inherited ClearDataViewer;
  try
    for LCol := 0 to GrowthProjectionsDialog.strgrdDemandFactors.ColCount -1 do
    begin
      for LRow := 1 to GrowthProjectionsDialog.strgrdDemandFactors.RowCount - 1 do
      begin
        GrowthProjectionsDialog.strgrdDemandFactors.Cells[LCol,LRow] := '';
      end;
    end;
    GrowthProjectionsDialog.strgrdDemandFactors.Reset;
    GrowthProjectionsDialog.strgrdDemandFactors.RowCount  := 2;
    GrowthProjectionsDialog.strgrdDemandFactors.ColCount  := 4;
    GrowthProjectionsDialog.strgrdDemandFactors.FixedRows := 1;
    GrowthProjectionsDialog.strgrdDemandFactors.Enabled   := True;
    for LCol := 0 to GrowthProjectionsDialog.strgrdMinMaxFactors.ColCount -1 do
    begin
      for LRow := 1 to GrowthProjectionsDialog.strgrdMinMaxFactors.RowCount - 1 do
      begin
        GrowthProjectionsDialog.strgrdMinMaxFactors.Cells[LCol,LRow] := '';
      end;
    end;
    GrowthProjectionsDialog.strgrdMinMaxFactors.Reset;
    GrowthProjectionsDialog.strgrdMinMaxFactors.RowCount  := 2;
    GrowthProjectionsDialog.strgrdMinMaxFactors.ColCount  := 5;
    GrowthProjectionsDialog.strgrdMinMaxFactors.FixedRows := 1;
    GrowthProjectionsDialog.strgrdMinMaxFactors.Enabled := True;
    GrowthProjectionsDialog.strgrdMinMaxFactors.Rows[1].Clear;

    for LCol := 0 to GrowthProjectionsDialog.strgrdHydrologyFactors.ColCount -1 do
    begin
      for LRow := 1 to GrowthProjectionsDialog.strgrdHydrologyFactors.RowCount - 1 do
      begin
        GrowthProjectionsDialog.strgrdHydrologyFactors.Cells[LCol,LRow] := '';
      end;
    end;
    GrowthProjectionsDialog.strgrdHydrologyFactors.Reset;
    GrowthProjectionsDialog.strgrdHydrologyFactors.RowCount  := 2;
    GrowthProjectionsDialog.strgrdHydrologyFactors.ColCount  := 4;
    GrowthProjectionsDialog.strgrdHydrologyFactors.FixedRows := 1;
    GrowthProjectionsDialog.strgrdHydrologyFactors.Enabled := True;
    GrowthProjectionsDialog.strgrdHydrologyFactors.Rows[1].Clear;


    GrowthProjectionsDialog.edtNumYears.Text            := '';
    //GrowthProjectionsDialog.BaseYearCbx.ItemIndex       := -1;
//    GrowthProjectionsDialog.edtBaseYear.Text            := '';
    GrowthProjectionsDialog.edtStartYear.Text           := '';
    GrowthProjectionsDialog.btnGenerateFactors.Enabled  := False;
    GrowthProjectionsDialog.btnValidateProjections.Enabled  := False;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TGrowthProjectionsValidator.PopulateDataViewer;
const OPNAME = 'TGrowthProjectionsValidator.PopulateDataViewer';
begin
  inherited PopulateDataViewer;
  try
    ClearDataViewer;
    RepopulateDataViewer;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TGrowthProjectionsValidator.RepopulateDataViewer;
const OPNAME = 'TGrowthProjectionsValidator.RepopulateDataViewer';
var
  LGrowth       : TGrowthFactors;
  LGrowthFactors: TExelGrowthFactors;
  LRunConfig    : IRunConfigurationData;
  LStartYear    : integer;
  //LIndex        : integer;
  LNrOfYears    : integer;
begin
  try
    GrowthProjectionsDialog.edtNumYears.Text      := '';
    //GrowthProjectionsDialog.BaseYearCbx.ItemIndex := -1;
    GrowthProjectionsDialog.edtStartYear.Text     := '';
  //  GrowthProjectionsDialog.btnGenMinMaxFactors.Visible := True;
  //  GrowthProjectionsDialog.btnGenMinMaxFactors.Enabled := True;
    LRunConfig                := TYieldModelDataObject(FAppModules.Model.ModelData).RunConfigurationData;
    LGrowthFactors            := TPlanningModelDataObject(FAppModules.Model.ModelData).CastExelGrowthFactors;
    LGrowth                   := TPlanningModelDataObject(FAppModules.Model.ModelData).CastGrowthFactors;
    LGrowthFactors.StartYear  := LRunConfig.HistoricSequenceStartYear;
    LStartYear                := LRunConfig.StartYearOther;
    LNrOfYears                := 0;
        if LGrowth <> nil then
      LNrOfYears := LGrowth.NumberOfYears;

    //for LIndex := 0 to LNrOfYears do
    //  GrowthProjectionsDialog.BaseYearCbx.Items.Add(IntToStr(LStartYear + LIndex));

    if not LGrowthFactors.Populated  then
    begin
      //GrowthProjectionsDialog.strgrdProjections.Enabled := False;
    end
    else
    //if not LGrowthFactors.Populated  then
    begin
      GrowthProjectionsDialog.edtNumYears.Text := IntToStr(LGrowthFactors.YearsCount);
     // LIndex := GrowthProjectionsDialog.BaseYearCbx.Items.IndexOf(IntToStr(LGrowthFactors.BaseYear));
     // if LIndex < 0 then
     //   GrowthProjectionsDialog.BaseYearCbx.Items.Insert(0,IntToStr(LGrowthFactors.BaseYear));
     // GrowthProjectionsDialog.BaseYearCbx.ItemIndex := LIndex;
      GrowthProjectionsDialog.edtBaseYear.Text := IntToStr(LStartYear);
      GrowthProjectionsDialog.edtStartYear.Text := IntToStr(LGrowthFactors.StartYear+LNrOfYears-1);
      case LGrowthFactors.ProjectionType of
        ptDemand: PopulateDemandCentreGrowthFactorsGrid(LGrowthFactors);
        ptMinMax: PopulateMinMaxChannelGrowthFactorsGrid(LGrowthFactors);
        ptHydrology: PopulateHydrologyGrowthFactorsGrid(LGrowthFactors);
      end;

      GrowthProjectionsDialog.btnGenerateFactors.Enabled := LGrowthFactors.Populated;
      GrowthProjectionsDialog.btnValidateProjections.Enabled := LGrowthFactors.Populated;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TGrowthProjectionsValidator.PopulateYears(AGrowthFactors : TExelGrowthFactors;AGrid: TFieldStringGrid);
const OPNAME = 'TGrowthProjectionsValidator.PopulateYears';
var
  LStart,
  LCurrentYear,
  LCol              : integer;
  LRunConfig        : IRunConfigurationData;

begin
  try
    LRunConfig          := TYieldModelDataObject(FAppModules.Model.ModelData).RunConfigurationData;
    LanguageHasChanged;
    FStartYearIndex := -1;
    FBaseYearIndex  := -1;
    //if(AGrowthFactors.ProjectionType = ptHydrology) or (AGrowthFactors.ProjectionType = ptMinMax) then
    if (AGrowthFactors.ProjectionType = ptMinMax) then
    LStart := 5
    else
      LStart := 4;
   
    GrowthProjectionsDialog.edtStartYear.SetFieldValue('');
    //GrowthProjectionsDialog.BaseYearCbx.ItemIndex := -1;
    GrowthProjectionsDialog.edtNumYears.SetFieldValue('');

    if( AGrid.RowCount >= 2)then
    begin
      if(AGrowthFactors.StartYear <> NullInteger) then
        GrowthProjectionsDialog.edtStartYear.SetFieldValue(IntToStr(AGrowthFactors.StartYear));
   //   if(AGrowthFactors.BaseYear <> NullInteger) then
  //      GrowthProjectionsDialog.BaseYearCbx.ItemIndex :=
   //       GrowthProjectionsDialog.BaseYearCbx.Items.IndexOf(IntToStr(AGrowthFactors.BaseYear));
//      if(AGrowthFactors.YearsCount <> NullInteger) then
        GrowthProjectionsDialog.edtNumYears.SetFieldValue(LRunConfig.YearsInAnalysis);

      AGrid.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('ReservoirName'));

      LCurrentYear := AGrowthFactors.DataStartYear;//LRunConfig.StartYearOther; //
      for LCol := LStart to AGrid.ColCount-1 do
      begin
        AGrid.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('ReservoirName'));
        AGrid.Cells[LCol,0] := IntToStr(LCurrentYear);

        if(AGrowthFactors.BaseYear <> NullInteger) and (AGrowthFactors.BaseYear = LCurrentYear) then
        begin
          //AGrid.Cells[LCol,0] := 'Base Year: '+ IntToStr(AGrowthFactors.BaseYear);
          FBaseYearIndex := LCol;
          AGrowthFactors.BaseYearIndex := LCol;
        end;

        if(AGrowthFactors.StartYear <> NullInteger) and (AGrowthFactors.StartYear = LCurrentYear) then
        begin
          AGrid.Cells[LCol,0]   := 'Start Year: '+ IntToStr(AGrowthFactors.StartYear);
          FStartYearIndex := LCol;
          AGrowthFactors.StartYearIndex := LCol;
        end;

        LCurrentYear := LCurrentYear + 1;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TGrowthProjectionsValidator.PopulateDemandCentreGrowthFactorsGrid(AGrowthFactors: TExelGrowthFactors);
const OPNAME = 'TGrowthProjectionsValidator.PopulateDemandCentreGrowthFactorsGrid';
var
  LRow,
  LCol: integer;
  LGrowthFactors:TExelDemandChannelGrowthFactors;
  LChannel : IGeneralFlowChannel;
  LFeature : IMasterControlFeature;

begin
  try
    if (AGrowthFactors.DemandChannelGrowthFactorsCount = 0) then Exit;

    GrowthProjectionsDialog.strgrdDemandFactors.Reset;
    GrowthProjectionsDialog.strgrdDemandFactors.RowCount  := 2;
    GrowthProjectionsDialog.strgrdDemandFactors.ColCount  := 4;
    GrowthProjectionsDialog.strgrdDemandFactors.FixedRows := 1;
    GrowthProjectionsDialog.strgrdDemandFactors.ClearFieldProperties;

    GrowthProjectionsDialog.strgrdDemandFactors.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('ReservoirName'));
    GrowthProjectionsDialog.strgrdDemandFactors.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('ReservoirName'));
    GrowthProjectionsDialog.strgrdDemandFactors.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('ChannelNumber'));
    GrowthProjectionsDialog.strgrdDemandFactors.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('ChannelNumber'));

    GrowthProjectionsDialog.strgrdDemandFactors.RowCount := AGrowthFactors.DemandChannelGrowthFactorsCount+1;

    for LRow := 0 to AGrowthFactors.DemandChannelGrowthFactorsCount-1 do
    begin
      GrowthProjectionsDialog.strgrdDemandFactors.Rows[LRow+1].Clear;
      LGrowthFactors := AGrowthFactors.DemandChannelGrowthFactorsByIndex[LRow];
      if(LGrowthFactors.GrowthFactorsCount > (GrowthProjectionsDialog.strgrdDemandFactors.ColCount - 4)) then
        GrowthProjectionsDialog.strgrdDemandFactors.ColCount := LGrowthFactors.GrowthFactorsCount + 4;

      GrowthProjectionsDialog.strgrdDemandFactors.Cells[0,LRow+1] := LGrowthFactors.Institution;
      GrowthProjectionsDialog.strgrdDemandFactors.Cells[1,LRow+1] := LGrowthFactors.WaterUser;
      GrowthProjectionsDialog.strgrdDemandFactors.Cells[2,LRow+1] := IntToStr(LGrowthFactors.ChannelNumber);
      GrowthProjectionsDialog.strgrdDemandFactors.Objects[2,LRow+1] := TObject(LGrowthFactors.ChannelNumber);

      //FloatToStr(LGrowthFactors.BaseYearDemand)
      LChannel := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.ChannelList.ChannelByChannelNumber[LGrowthFactors.ChannelNumber];
      if LChannel <> nil then
        LFeature := TYieldModelDataObject(FAppModules.Model.ModelData).
                    NetworkFeaturesData.MasterControlFeatureList.MasterControlFeatureByID[LChannel.MasterControlFeature.FeatureID];
      if LFeature <> nil then
        GrowthProjectionsDialog.strgrdDemandFactors.Cells[3,LRow+1] := FloatToStr(LFeature.AnnualDemand);
      LFeature := nil;

      for LCol := 0 to LGrowthFactors.GrowthFactorsCount -1 do
      begin
        GrowthProjectionsDialog.strgrdDemandFactors.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('ExcelDemandGrowthFactors'));
        GrowthProjectionsDialog.strgrdDemandFactors.Cells[LCol+4,LRow+1] := LGrowthFactors.GrowthFactorsList[LCol];
      end;
    end;
    PopulateYears(AGrowthFactors,GrowthProjectionsDialog.strgrdDemandFactors);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TGrowthProjectionsValidator.PopulateMinMaxChannelGrowthFactorsGrid(AGrowthFactors: TExelGrowthFactors);
const OPNAME = 'TGrowthProjectionsValidator.PopulateMinMaxChannelGrowthFactorsGrid';
var
  LRow,
  LCol: integer;
  LGrowthFactors:TExelMinMaxChannelGrowthFactors;
begin
  try
    GrowthProjectionsDialog.strgrdMinMaxFactors.Cells[0,0] := FAppModules.language.GetString('GridHeading.Institutions');
    GrowthProjectionsDialog.strgrdMinMaxFactors.Cells[1,0] := FAppModules.language.GetString('GridHeading.Water Users');
    GrowthProjectionsDialog.strgrdMinMaxFactors.Cells[2,0] := FAppModules.language.GetString('TField.MinMaxChannelNumber');
    GrowthProjectionsDialog.strgrdMinMaxFactors.Cells[3,0] := FAppModules.language.GetString('GridHeading.ArcNumber');
    GrowthProjectionsDialog.strgrdMinMaxFactors.Cells[4,0] := 'Base Demand';

    GrowthProjectionsDialog.strgrdMinMaxFactors.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('ReservoirName'));
    GrowthProjectionsDialog.strgrdMinMaxFactors.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('ReservoirName'));
    GrowthProjectionsDialog.strgrdMinMaxFactors.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('ArcNumber'));
    GrowthProjectionsDialog.strgrdMinMaxFactors.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('ArcNumber'));
    GrowthProjectionsDialog.strgrdMinMaxFactors.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('ArcNumber'));

    if (AGrowthFactors.MinMaxChannelGrowthFactorsCount = 0) then   Exit;
   
    GrowthProjectionsDialog.strgrdMinMaxFactors.RowCount := AGrowthFactors.MinMaxChannelGrowthFactorsCount+1;
    for LRow := 0 to AGrowthFactors.MinMaxChannelGrowthFactorsCount-1 do
    begin
      GrowthProjectionsDialog.strgrdMinMaxFactors.Rows[LRow+1].Clear;
      LGrowthFactors := AGrowthFactors.MinMaxChannelGrowthFactorsByIndex[LRow];
      if(LGrowthFactors.MinMaxGrowthFactorsCount > (GrowthProjectionsDialog.strgrdMinMaxFactors.ColCount - 5)) then
        GrowthProjectionsDialog.strgrdMinMaxFactors.ColCount := LGrowthFactors.MinMaxGrowthFactorsCount + 5;
      GrowthProjectionsDialog.strgrdMinMaxFactors.Cells[0,LRow+1] := LGrowthFactors.Institution;
      GrowthProjectionsDialog.strgrdMinMaxFactors.Cells[1,LRow+1] := LGrowthFactors.WaterUser;
      GrowthProjectionsDialog.strgrdMinMaxFactors.Cells[2,LRow+1] := IntToStr(LGrowthFactors.ChannelNumber);
      GrowthProjectionsDialog.strgrdMinMaxFactors.Cells[3,LRow+1] := IntToStr(LGrowthFactors.ArcNumber);

      GrowthProjectionsDialog.strgrdMinMaxFactors.Cells[4,LRow+1] := FloatToStr(LGrowthFactors.BaseYearDemand);

     for LCol := 0 to LGrowthFactors.MinMaxGrowthFactorsCount -1 do
     begin
        GrowthProjectionsDialog.strgrdMinMaxFactors.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('ExcelMinMaxGrowthFactors'));
        GrowthProjectionsDialog.strgrdMinMaxFactors.Cells[LCol+5,LRow+1] := LGrowthFactors.GrowthFactorsList[LCol];
     end;

    end;
   PopulateYears(AGrowthFactors,GrowthProjectionsDialog.strgrdMinMaxFactors);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TGrowthProjectionsValidator.PopulateHydrologyGrowthFactorsGrid( AGrowthFactors: TExelGrowthFactors);
const OPNAME = 'TGrowthProjectionsValidator.PopulateHydrologyGrowthFactorsGrid';
var
  LColCount,
  LCount,
  LRow,
  LCol: integer;
  LGrowthFactors:TExelHydrologyGrowthFactors;
begin
  try
    if (AGrowthFactors.HydrologyGrowthFactorsCount = 0) then Exit;
    GrowthProjectionsDialog.strgrdHydrologyFactors.ClearFieldProperties;
    GrowthProjectionsDialog.strgrdHydrologyFactors.Cells[0,0] := FAppModules.language.GetString('GridHeading.Institutions');
    GrowthProjectionsDialog.strgrdHydrologyFactors.Cells[1,0] := FAppModules.language.GetString('GridHeading.Water Users');
    GrowthProjectionsDialog.strgrdHydrologyFactors.Cells[2,0] := FAppModules.language.GetString('GridHeading.GaugeNumber');
    GrowthProjectionsDialog.strgrdHydrologyFactors.Cells[3,0] := FAppModules.language.GetString('GridHeading.FactorType');

    GrowthProjectionsDialog.strgrdHydrologyFactors.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('ReservoirName'));
    GrowthProjectionsDialog.strgrdHydrologyFactors.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('ReservoirName'));
    GrowthProjectionsDialog.strgrdHydrologyFactors.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('ArcNumber'));
    GrowthProjectionsDialog.strgrdHydrologyFactors.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('ArcNumber'));

    GrowthProjectionsDialog.strgrdHydrologyFactors.RowCount := (AGrowthFactors.HydrologyGrowthFactorsCount*3)+1;
    LRow := 0;
    for LCount := 0  to AGrowthFactors.HydrologyGrowthFactorsCount-1 do
    begin
      LGrowthFactors := AGrowthFactors.HydrologyGrowthFactorsByIndex[LCount];

      LColCount := LGrowthFactors.AFFGrowthFactorsCount;
      if(LGrowthFactors.IRRGrowthFactorsCount > LColCount) then
        LColCount := LGrowthFactors.IRRGrowthFactorsCount;
      if(LGrowthFactors.URBGrowthFactorsCount > LColCount) then
        LColCount := LGrowthFactors.URBGrowthFactorsCount;
      LColCount :=  LColCount + 4;
      if(LColCount > (GrowthProjectionsDialog.strgrdHydrologyFactors.ColCount)) then
        GrowthProjectionsDialog.strgrdHydrologyFactors.ColCount := LColCount;

      LRow := LRow + 1;
      GrowthProjectionsDialog.strgrdHydrologyFactors.Rows[LRow].Clear;
      GrowthProjectionsDialog.strgrdHydrologyFactors.Cells[0,LRow] := LGrowthFactors.Institution;
      GrowthProjectionsDialog.strgrdHydrologyFactors.Cells[1,LRow] := LGrowthFactors.WaterUser;
      GrowthProjectionsDialog.strgrdHydrologyFactors.Cells[2,LRow] := IntToStr(LGrowthFactors.GaugeNumber);
      GrowthProjectionsDialog.strgrdHydrologyFactors.Cells[3,LRow] := 'AFF';

      for LCol := 0 to LGrowthFactors.AFFGrowthFactorsCount -1 do
      begin
        GrowthProjectionsDialog.strgrdHydrologyFactors.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('ExcelAFFGrowthFactors'));
        GrowthProjectionsDialog.strgrdHydrologyFactors.Cells[LCol+4,LRow] := LGrowthFactors.AFFGrowthFactorsList[LCol];
      end;

      LRow := LRow + 1;
      GrowthProjectionsDialog.strgrdHydrologyFactors.Rows[LRow].Clear;
      GrowthProjectionsDialog.strgrdHydrologyFactors.Cells[0,LRow] := LGrowthFactors.Institution1;
      GrowthProjectionsDialog.strgrdHydrologyFactors.Cells[1,LRow] := LGrowthFactors.WaterUser1;
      GrowthProjectionsDialog.strgrdHydrologyFactors.Cells[2,LRow] := IntToStr(LGrowthFactors.GaugeNumber);
      GrowthProjectionsDialog.strgrdHydrologyFactors.Cells[3,LRow] := 'IRR';
      for LCol := 0 to LGrowthFactors.IRRGrowthFactorsCount -1 do
      begin
         GrowthProjectionsDialog.strgrdHydrologyFactors.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('ExcelIRRGrowthFactors'));
         GrowthProjectionsDialog.strgrdHydrologyFactors.Cells[LCol+4,LRow] := LGrowthFactors.IRRGrowthFactorsList[LCol];
      end;

      LRow := LRow + 1;
      GrowthProjectionsDialog.strgrdHydrologyFactors.Rows[LRow].Clear;
      GrowthProjectionsDialog.strgrdHydrologyFactors.Cells[0,LRow] := LGrowthFactors.Institution2;
      GrowthProjectionsDialog.strgrdHydrologyFactors.Cells[1,LRow] := LGrowthFactors.WaterUser2;
      GrowthProjectionsDialog.strgrdHydrologyFactors.Cells[2,LRow] := IntToStr(LGrowthFactors.GaugeNumber);
      GrowthProjectionsDialog.strgrdHydrologyFactors.Cells[3,LRow] := 'URB';
      for LCol := 0 to LGrowthFactors.URBGrowthFactorsCount -1 do
      begin
        GrowthProjectionsDialog.strgrdHydrologyFactors.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('ExcelURBGrowthFactors'));
        GrowthProjectionsDialog.strgrdHydrologyFactors.Cells[LCol+4,LRow] := LGrowthFactors.URBGrowthFactorsList[LCol];
      end;
    end;

    PopulateYears(AGrowthFactors,GrowthProjectionsDialog.strgrdHydrologyFactors);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TGrowthProjectionsValidator.OnEditControltExit(Sender: TObject);
const OPNAME = 'TGrowthProjectionsValidator.OnEditControltExit';
var
  LGrowthFactors: TExelGrowthFactors;
begin
  inherited OnEditControltExit(Sender);
  try
    if Sender.ClassNameIs('TFieldEdit') then
    begin
      if TFieldEdit(Sender).HasValueChanged then
      begin
        LGrowthFactors := TPlanningModelDataObject(FAppModules.Model.ModelData).CastExelGrowthFactors;
        if(Sender = GrowthProjectionsDialog.edtNumYears) AND
          (GrowthProjectionsDialog.edtNumYears.HasValueChanged) then
            UpdateNumYears(LGrowthFactors);
        {
        if(Sender = GrowthProjectionsDialog.edtStartYear) AND
          (GrowthProjectionsDialog.edtStartYear.HasValueChanged) then
            UpdateStartYear(LGrowthFactors); }
        RepopulateDataViewer;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TGrowthProjectionsValidator.OnTabChanged(Sender: Tobject);
const OPNAME = 'TGrowthProjectionsValidator.OnTabChanged';
var
  LGrowthFactors     :TExelGrowthFactors;
begin
  try
    LGrowthFactors := TPlanningModelDataObject(FAppModules.Model.ModelData).CastExelGrowthFactors;

    if GrowthProjectionsDialog.pgcProjections.ActivePage =
      GrowthProjectionsDialog.tbsDemandFactors then
     LGrowthFactors.ProjectionType := ptDemand;
    if GrowthProjectionsDialog.pgcProjections.ActivePage =
      GrowthProjectionsDialog.tbsMinMaxFactors then
      LGrowthFactors.ProjectionType := ptMinMax;
    if GrowthProjectionsDialog.pgcProjections.ActivePage =
      GrowthProjectionsDialog.tbsHydrologyFactors then
      LGrowthFactors.ProjectionType := ptHydrology;
    RepopulateDataViewer;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TGrowthProjectionsValidator.OnViewItemHasChanged(Sender: TObject);
const OPNAME = 'TGrowthProjectionsValidator.OnViewItemHasChanged';
begin
  try
    RepopulateDataViewer;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

(*
procedure TGrowthProjectionsValidator.OnGenerateMinMaxFactorsClick(Sender: TObject);
const OPNAME = 'TGrowthProjectionsValidator.OnGenerateMinMaxFactorsClick';
var
  LChannel : IGeneralFlowChannel;
  LGrowthFactors : TGrowthFactors;
  LGrowthProjections : TExelGrowthFactors;
  LMinMaxChannelGrowthFactors : IMinMaxChannelGrowthFactors;
  LMinMaxGrowth : TMinMaxChannelGrowthFactors;
  LRunConfig : IRunConfigurationData;
  LExelMinMaxChannelProjections   : TExelMinMaxChannelGrowthFactors;
  LIndex : integer;
  LTempStr        : TStringList;
  LNumberOfYears : integer;
  LCount : integer;
  LArc : integer;
  LProjection  : string;
begin
  try
    LGrowthFactors           := TPlanningModelDataObject(FAppModules.Model.ModelData).CastGrowthFactors;
    LRunConfig               := TYieldModelDataObject(FAppModules.Model.ModelData).RunConfigurationData;


    if (LGrowthFactors <> nil) and (LRunConfig <> nil) then
    begin
      if LGrowthFactors.MinMaxChannelGrowthFactorsCount = 0 then
      begin
        for LIndex := 0 to TYieldModelDataObject(FAppModules.Model.ModelData).
                    NetworkElementData.ChannelList.ChannelCount-1 do
        begin
          LChannel :=  TYieldModelDataObject(FAppModules.Model.ModelData).
                    NetworkElementData.ChannelList.ChannelByIdentifier[LIndex];
          if LChannel <> nil then
          begin
            if (LChannel.MinMaxFlowConstraint <> nil) then
            begin
              for LArc := 1 to 2 do
              begin
                LMinMaxChannelGrowthFactors :=  LGrowthFactors.AddMinMaxChannelGrowthFactor(LChannel.ChannelNumber);
                if LMinMaxChannelGrowthFactors <> nil then
                begin
                  LTempStr := TStringList.Create;
                  try
                    LNumberOfYears := LGrowthFactors.NumberOfYears;
                    for LCount := 0 to LNumberOfYears do
                      LTempStr.Add('0.0000');
                    LMinMaxChannelGrowthFactors.GrowthFactors := LTempStr.CommaText;
                    LMinMaxChannelGrowthFactors.ArcNumber := LArc;
                  finally
                      LTempStr.Free;
                  end;
                end;
              end;

            end;
          end;
        end;
       // LGrowthFactors.GenerateGrowthProjections;
        LGrowthProjections  := TPlanningModelDataObject(FAppModules.Model.ModelData).CastExelGrowthFactors;
        if (LGrowthProjections <> nil) then
        begin
          for LIndex := 0 to LGrowthFactors.MinMaxChannelGrowthFactorsCount -1 do
          begin
            LMinMaxGrowth                               := LGrowthFactors.MinMaxChannelGrowthFactorsByIndex[LIndex];
            LExelMinMaxChannelProjections               := LGrowthProjections.CreateMinMaxChannelGrowthProjection;
            LExelMinMaxChannelProjections.ChannelNumber := LMinMaxGrowth.MinMaxChannel;
            LExelMinMaxChannelProjections.ArcNumber     := LMinMaxGrowth.ArcNumber;
            LProjection := LMinMaxGrowth.GenerateMinMaxChannelGrowthProjections(
                           LExelMinMaxChannelProjections.BaseYearDemand,LRunConfig.HistoricSequenceStartYear,LGrowthFactors.NumberOfYears);
            LExelMinMaxChannelProjections.Populate(LIndex+1,LMinMaxGrowth.MinMaxChannel,LGrowthFactors.NumberOfYears,LProjection);
          end;
        end;
      end
      else
      begin
        for LIndex := 0 to TYieldModelDataObject(FAppModules.Model.ModelData).
                    NetworkElementData.ChannelList.ChannelCount-1 do
        begin
          LChannel :=  TYieldModelDataObject(FAppModules.Model.ModelData).
                    NetworkElementData.ChannelList.ChannelByIdentifier[LIndex];
          if LChannel <> nil then
          begin
            LMinMaxChannelGrowthFactors :=   LGrowthFactors.MinMaxChannelGrowthFactorsByMinMaxChannel[LChannel.ChannelNumber];
            if LMinMaxChannelGrowthFactors <> nil then
               LGrowthFactors.RemoveMinMaxChannelGrowthFactor(LChannel.ChannelNumber);
          end;
        end;
      end;
    end;
    PopulateDataViewer;
  except on E: Exception do HandleError(E, OPNAME) end;
end;
*)


procedure TGrowthProjectionsValidator.OnGenerateFactorsClick(Sender: TObject);
const OPNAME = 'TGrowthProjectionsValidator.OnGenerateFactorsClick';
{      MSG1   = 'There is no data to generate growth factors from.';
      MSG2   = 'There are no Demand Channel Growth Projection. Do you want to continue without them';
      MSG3   = 'There are no MinMax Channel Growth Projection. Do you want to continue without them';
      MSG4   = 'There are no Hydrology Growth Projection. Do you want to continue without them';
      MSG5   = 'The Start Date for one or more of the projections is not on the Available range '+
                   'to  Generate Growth Factors. You won''t be able to Generate them.'+
                   ' Do you want to continue without them';
      MSG6   = 'There isn''t enough Demand Channel Growth Projection  . Do you want to continue with '+
                   ' the available ones';
      MSG7   = 'There isn''t enough Min Max Channel Growth Projection  . Do you want to continue with '+
                   ' the available ones';
      MSG8   = 'There isn''t enough Hydrology Channel Growth Projection  . Do you want to continue with '+
                   ' the available ones';}
var
  LGrowthFactors               : TExelGrowthFactors;
  LRunConfig                   : IRunConfigurationData;
begin
  try
    LGrowthFactors           := TPlanningModelDataObject(FAppModules.Model.ModelData).CastExelGrowthFactors;
    LRunConfig               := TYieldModelDataObject(FAppModules.Model.ModelData).RunConfigurationData;

    if not LGrowthFactors.Populated then
    begin
      ShowMessage(FAppModules.Language.GetString('Message.GrowthProjectionsValidatorMSG1'));
      Exit;
    end;
    if(GrowthProjectionsDialog.strgrdDemandFactors.ColCount < LGrowthFactors.StartYearIndex) or
       (GrowthProjectionsDialog.strgrdMinMaxFactors.ColCount < LGrowthFactors.StartYearIndex) or
        (GrowthProjectionsDialog.strgrdHydrologyFactors.ColCount < LGrowthFactors.StartYearIndex) then
         if (MessageDlg(FAppModules.Language.GetString('Message.GrowthProjectionsValidatorMSG5'),mtConfirmation,mbYesNoCancel,0) <> mrYes) then Exit;

    if(LGrowthFactors.DemandChannelGrowthFactorsCount = 0) then
      if (MessageDlg(FAppModules.Language.GetString('Message.GrowthProjectionsValidatorMSG2'),mtConfirmation,mbYesNoCancel,0) <> mrYes) then Exit;

     if (LGrowthFactors.DemandChannelGrowthFactorsCount > 0) then
       if ((GrowthProjectionsDialog.strgrdDemandFactors.ColCount  - (FStartYearIndex)) < LRunConfig.YearsInAnalysis) then
         if (MessageDlg(FAppModules.Language.GetString('Message.GrowthProjectionsValidatorMSG6'),mtConfirmation,mbYesNoCancel,0) <> mrYes) then Exit;

    if(LGrowthFactors.MinMaxChannelGrowthFactorsCount = 0) then
      if (MessageDlg(FAppModules.Language.GetString('Message.GrowthProjectionsValidatorMSG3'),mtConfirmation,mbYesNoCancel,0) <> mrYes) then Exit;

    if (LGrowthFactors.MinMaxChannelGrowthFactorsCount > 0) then
      if ((GrowthProjectionsDialog.strgrdMinMaxFactors.ColCount  - (FStartYearIndex)) < LRunConfig.YearsInAnalysis) then
        if (MessageDlg(FAppModules.Language.GetString('Message.GrowthProjectionsValidatorMSG7'),mtConfirmation,mbYesNoCancel,0) <> mrYes) then Exit;

    if(LGrowthFactors.HydrologyGrowthFactorsCount = 0) then
      if (MessageDlg(FAppModules.Language.GetString('Message.GrowthProjectionsValidatorMSG4'),mtConfirmation,mbYesNoCancel,0) <> mrYes) then Exit;

     if (LGrowthFactors.HydrologyGrowthFactorsCount > 0) then
       if ((GrowthProjectionsDialog.strgrdHydrologyFactors.ColCount  - (FStartYearIndex)) < LRunConfig.YearsInAnalysis) then
         if (MessageDlg(FAppModules.Language.GetString('Message.GrowthProjectionsValidatorMSG8'),mtConfirmation,mbYesNoCancel,0) <> mrYes) then Exit;


    LGrowthFactors.GenerateGrowthFactors ;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TGrowthProjectionsValidator.OnPasteFromExcelClick(Sender: TObject);
const OPNAME = 'TGrowthProjectionsValidator.OnPasteFromExcelClick';
var
  LGrowthFactors  : TExelGrowthFactors;
  LGrowth  : TGrowthFactors;
  LNumOfYears : integer;
begin
  try
    LGrowthFactors := TPlanningModelDataObject(FAppModules.Model.ModelData).CastExelGrowthFactors;
    LGrowth := TPlanningModelDataObject(FAppModules.Model.ModelData).CastGrowthFactors;
    if LGrowth <> nil then
    begin
      LNumOfYears := LGrowth.NumberOfYears;
      GrowthProjectionsDialog.strgrdDemandFactors.ColCount  := LNumOfYears;
      GrowthProjectionsDialog.strgrdHydrologyFactors.ColCount := LNumOfYears;
    end;

    case LGrowthFactors.ProjectionType of
      ptDemand : RemoveEmptyColumn(GrowthProjectionsDialog.strgrdDemandFactors);
      ptHydrology: RemoveEmptyColumn(GrowthProjectionsDialog.strgrdHydrologyFactors);
    end;
    if ValidateDataFromExcel then
    begin
      PopulateGrowthFactorsFromExcel;
      RepopulateDataViewer;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TGrowthProjectionsValidator.RemoveEmptyColumn(AGrid: TFieldStringGrid );
const OPNAME = 'TGrowthProjectionsValidator.RemoveEmptyColumn';
var
  LRow,LCol: integer;
begin
  try
    if(AGrid.ColCount >= 4)then
    begin
      for LRow := 1 to AGrid.RowCount-1 do
      begin
        for LCol := 4 to AGrid.ColCount-1 do
        begin
          AGrid.Cells[LCol,LRow] := AGrid.Cells[LCol,LRow];
        end;
      end;
      AGrid.ColCount := AGrid.ColCount-1;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TGrowthProjectionsValidator.PopulateGrowthFactorsFromExcel;
const OPNAME = 'TGrowthProjectionsValidator.PopulateGrowthFactorsFromExcel';
var
  LGrowthFactors: TExelGrowthFactors;
begin
  try
    LGrowthFactors := TPlanningModelDataObject(FAppModules.Model.ModelData).CastExelGrowthFactors;
    if(LGrowthFactors <> nil) then
    begin
      if PopulateYearsObject(LGrowthFactors) then
      begin
        case LGrowthFactors.ProjectionType of
          ptDemand: PopulateDemandChannelGrowthFactorsObject(LGrowthFactors);
          ptMinMax: PopulateMinMaxChannelGrowthFactorsObject(LGrowthFactors);
          ptHydrology: PopulateHydrologyGrowthFactorsObject(LGrowthFactors);
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TGrowthProjectionsValidator.PopulateYearsObject(AFactors: TExelGrowthFactors): boolean;
const OPNAME = 'TGrowthProjectionsValidator.PopulateYearsObject';
var
  LStartYear,
  LBaseYear,
  LYearsCount,
  LDataStartYear,
  LRow,LCol   : integer;
  LRunConfig  : IRunConfigurationData;
begin
  Result := False;
  try
    if(CurrentGrid.ColCount >= 3) then
    begin
      //LBaseYear   := NullInteger;
      //0;
     // LDataStartYear := 0;
      LRunConfig  := TYieldModelDataObject(FAppModules.Model.ModelData).RunConfigurationData;
      LStartYear  := LRunConfig.HistoricSequenceStartYear;
      LBaseYear   := LStartYear;
      LDataStartYear := LStartYear;
      LYearsCount :=  AFactors.YearsCount;
      case AFactors.ProjectionType of
        ptDemand:
          begin
           // LBaseYear   :=  StrToIntDef(CurrentGrid.Cells[3,1],-1);
            //LDataStartYear := StrToIntDef(CurrentGrid.Cells[4,1],-1);
        //    LYearsCount := GrowthProjectionsDialog.strgrdDemandFactors.ColCount;
          end;
        ptMinMax:
          begin
      //      LBaseYear   := StrToIntDef(CurrentGrid.Cells[4,1],-1);
          //  LDataStartYear := StrToIntDef(CurrentGrid.Cells[5,1],-1);
          //  LYearsCount := GrowthProjectionsDialog.strgrdMinMaxFactors.ColCount - 5;
          end;
        ptHydrology:
          begin
          //  LBaseYear   := StrToIntDef(CurrentGrid.Cells[3,1],-1);
          //  LYearsCount := GrowthProjectionsDialog.strgrdHydrologyFactors.ColCount - 5;
          end;
      end;

      if(LStartYear >= 0) {and (LBaseYear >= 0)} and (LYearsCount >= 0)  then
      begin
        //LDataStartYear      := LBaseYear;

        AFactors.Populate(LBaseYear,FBaseYearIndex,LStartYear,FStartYearIndex,LYearsCount,LDataStartYear);
        //Delete row 2
        for LRow := 1 to CurrentGrid.RowCount -1 do
        begin
          for LCol := 0 to CurrentGrid.ColCount -1 do
          begin
            CurrentGrid.Cells[LCol,LRow] := CurrentGrid.Cells[LCol,LRow];
          end;
        end;
        //CurrentGrid.RowCount := CurrentGrid.RowCount-1;
        Result := True;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TGrowthProjectionsValidator.PopulateDemandChannelGrowthFactorsObject(AFactors: TExelGrowthFactors): boolean;
const OPNAME = 'TGrowthProjectionsValidator.PopulateDemandChannelGrowthFactorsObject';
var
  LRow,
  LCount,
  LChannelNumber: integer;
  LData: TStringList;
  LInstitution,
  LWaterUser : string;
  LGrowthFactors:TExelDemandChannelGrowthFactors;
begin
  Result := False;
  try
    AFactors.ClearDemandChannelGrowthFactors;
    //check the channel numbers are all integers
    if(GetCurrentGrid.ColCount >= 4) then
    begin
      LData := TStringList.Create;
      try
        for LRow := 1 to GetCurrentGrid.RowCount -1 do
        begin
          LData.Assign(GetCurrentGrid.Rows[LRow]);
          if(Trim(LData.Text) <> '') then
          begin
            LInstitution   := LData[0];
            LWaterUser     := LData[1];
             LChannelNumber  := StrToIntDef(LData[2],-1);
            for LCount := 0 to 3 do
              LData.Delete(0);
            LGrowthFactors := AFactors.AddDemandChannelGrowthFactors;
            if not LGrowthFactors.PopulateDemandChannelGrowthFactors(LRow,LChannelNumber,LInstitution,LWaterUser,LData.CommaText) then
              Exit;
          end;
        end;
      finally
        LData.Free;
      end;
    end;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TGrowthProjectionsValidator.PopulateMinMaxChannelGrowthFactorsObject(AFactors: TExelGrowthFactors): boolean;
const OPNAME = 'TGrowthProjectionsValidator.PopulateMinMaxChannelGrowthFactorsObject';
var
  LRow,
  LCount,
  LArcNumber,
  LChannelNumber: integer;
  LData: TStringList;
  LInstitution,
  LWaterUser : string;
  LGrowthFactors:TExelMinMaxChannelGrowthFactors;
begin
  Result := False;
  try

    RePopulateMinMaxChannelGrowthFactorsGrid(GrowthProjectionsDialog.strgrdMinMaxFactors);
    AFactors.ClearMinMaxChannelGrowthFactors;
    //check the channel numbers are all integers

    if(GrowthProjectionsDialog.strgrdMinMaxFactors.ColCount >= 5) then
    begin
      LData := TStringList.Create;
      try
        for LRow := 1 to GrowthProjectionsDialog.strgrdMinMaxFactors.RowCount -1 do
        begin
          LData.Assign(GrowthProjectionsDialog.strgrdMinMaxFactors.Rows[LRow]);
          if(Trim(LData.Text) <> '') then
          begin
            LInstitution   := LData[0];
            LWaterUser     := LData[1];
            LChannelNumber := StrToInt(LData[2]);
            LArcNumber     := StrToInt(LData[3]);
            //LBaseDemand    := StrToFloat(LData[4]);
            for LCount := 0 to 4 do
              LData.Delete(0);
            LGrowthFactors := AFactors.AddMinMaxChannelGrowthFactors;
            if not LGrowthFactors.PopulateMinMaxChannelGrowthFactors(LRow,LChannelNumber,LArcNumber,LInstitution,LWaterUser,LData.CommaText) then
              Exit;
          end;
        end;
      finally
        LData.Free;
      end;
    end;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TGrowthProjectionsValidator.PopulateHydrologyGrowthFactorsObject(AFactors: TExelGrowthFactors): boolean;
const OPNAME = 'TGrowthProjectionsValidator.PopulateHydrologyGrowthFactorsObject';
var
  LRow,
  LCount,
  LIdentifier,
  LChannelNumber     : integer;
  LData              : TStringList;
  LAFFData,
  LIRRData,
  LURBData,
  LInstitution,
  LInstitution1,
  LInstitution2,
  LWaterUser,
  LWaterUser1,
  LWaterUser2        : string;
  LGrowthFactors     : TExelHydrologyGrowthFactors;
begin
  Result := False;
  try

    RePopulateHydrologyGrowthFactorsGrid(GrowthProjectionsDialog.strgrdHydrologyFactors);
    AFactors.ClearHydrologyGrowthFactors;

    //check the channel numbers are all integers
    if(GrowthProjectionsDialog.strgrdHydrologyFactors.ColCount >= 3) then
    begin
      LData := TStringList.Create;
      try
        LRow := 1;
        LIdentifier := 0;
        while (LRow < GrowthProjectionsDialog.strgrdHydrologyFactors.RowCount) do
        begin
          LData.Assign(GrowthProjectionsDialog.strgrdHydrologyFactors.Rows[LRow]);
          if(Trim(LData.Text) <> '') then
          begin
            LInstitution   := LData[0];
            LWaterUser     := LData[1];
            LChannelNumber := StrToInt(LData[2]);
            for LCount := 0 to 2 do
              LData.Delete(0);
            LAFFData := LData.CommaText;
            LIRRData := '';
            LURBData := '';

            LRow     := LRow + 1;
            if(LRow < GrowthProjectionsDialog.strgrdHydrologyFactors.RowCount) then
            begin
              LData.Assign(GrowthProjectionsDialog.strgrdHydrologyFactors.Rows[LRow]);
              LInstitution1 := LData[0];
              LWaterUser1   := LData[1];
              for LCount := 0 to 2 do
                LData.Delete(0);
              LIRRData := LData.CommaText;
            end;

            LRow     := LRow + 1;
            if(LRow < GrowthProjectionsDialog.strgrdHydrologyFactors.RowCount) then
            begin
              LData.Assign(GrowthProjectionsDialog.strgrdHydrologyFactors.Rows[LRow]);
              LInstitution2 := LData[0];
              LWaterUser2   := LData[1];
              for LCount := 0 to 2 do
                LData.Delete(0);
              LURBData := LData.CommaText;
            end;

            LIdentifier := LIdentifier + 1;
            LGrowthFactors := AFactors.AddHydrologyGrowthFactors;
            if not LGrowthFactors.PopulateHydrologyGrowthFactors(LIdentifier,LChannelNumber,LInstitution,LWaterUser,LAFFData,LIRRData,LURBData) then
              Exit;
            LGrowthFactors.Institution1 := LInstitution1;
            LGrowthFactors.Institution2 := LInstitution2;
            LGrowthFactors.WaterUser1   := LWaterUser1;
            LGrowthFactors.WaterUser2   := LWaterUser2;
            LRow     := LRow + 1;
          end;
        end;
      finally
        LData.Free;
      end;
    end;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TGrowthProjectionsValidator.ValidateDataFromExcel: boolean;
const OPNAME = 'TGrowthProjectionsValidator.ValidateDataFromExcel';
var
  LGrowthFactors   :   TExelGrowthFactors;
begin
  Result := ValidateChannelNumbersFromExcel;
  try
    LGrowthFactors            := TPlanningModelDataObject(FAppModules.Model.ModelData).CastExelGrowthFactors;
    if Result then
    begin
      case LGrowthFactors.ProjectionType of
        ptDemand: Result := ValidateDemandCentreGrowthFactorsFromExcel;
        ptMinMax: Result := ValidateMinMaxChannelGrowthFactorsFromExcel;
        ptHydrology: Result := ValidateHydrologyGrowthFactorsFromExcel;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TGrowthProjectionsValidator.ValidateChannelNumbersFromExcel: boolean;
const OPNAME = 'TGrowthProjectionsValidator.ValidateChannelNumbersFromExcel';
var
  LInteger,
  LIndex: integer;
  LColData: TStrings;
begin
  Result := False;
  try
    //check the channel numbers are all integers
    if(GetCurrentGrid.ColCount >= 3) then
    begin
      LColData := GetCurrentGrid.Cols[2];
      for LIndex := 1 to LColData.Count -1 do
      begin
        LInteger := StrToIntDef(LColData[LIndex],-1);
        if(LInteger = -1) then
        begin
          GetCurrentGrid.ValidationError[3, LIndex,gveCellContext] :=
                  FAppModules.Language.GetString('ErrorString.ChannelNoTypeError');
          Exit;
        end;
      end;
      Result := True;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TGrowthProjectionsValidator.ValidateDemandCentreGrowthFactorsFromExcel: boolean;
const OPNAME = 'TGrowthProjectionsValidator.ValidateDemandCentreGrowthFactorsFromExcel';
var
  LRow,
  LCol: integer;
  LDouble: Double;
begin
  Result := False;
  try
    //check the growth projections are all float
    if(GrowthProjectionsDialog.strgrdDemandFactors.ColCount >= 3)then
    begin
      for LRow := 2 to GrowthProjectionsDialog.strgrdDemandFactors.RowCount-1 do
      begin
        for LCol := 4 to GrowthProjectionsDialog.strgrdDemandFactors.ColCount-1 do
        begin
          if(Trim(GrowthProjectionsDialog.strgrdDemandFactors.Cells[LCol,LRow]) <> '') then
          begin
            LDouble := StrToFloatDef(GrowthProjectionsDialog.strgrdDemandFactors.Cells[LCol,LRow],NullFloat);
            if(LDouble = NullFloat) then
            begin
              GrowthProjectionsDialog.strgrdDemandFactors.ValidationError[LCol, LRow,gveCellContext] :=
                FAppModules.Language.GetString('ErrorString.GrowthProjectionTypeError');
              Exit;
            end;
          end;
        end;
      end;
      Result := True;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TGrowthProjectionsValidator.ValidateMinMaxChannelGrowthFactorsFromExcel: boolean;
const OPNAME = 'TGrowthProjectionsValidator.ValidateMinMaxChannelGrowthFactorsFromExcel';
var
  LInteger,
  LRow,
  LCol: integer;
  LDouble: Double;
begin
  Result := False;
  try
    //check the growth projections are all float
    if(GrowthProjectionsDialog.strgrdMinMaxFactors.ColCount >= 5)then
    begin
      for LRow := 2 to GrowthProjectionsDialog.strgrdMinMaxFactors.RowCount-1 do
      begin
        for LCol := 3 to GrowthProjectionsDialog.strgrdMinMaxFactors.ColCount-1 do
        begin
          if(LCol = 3) then
          begin
            if(Trim(GrowthProjectionsDialog.strgrdMinMaxFactors.Cells[LCol,LRow]) <> '') then
            begin
              LInteger := StrToIntDef(GrowthProjectionsDialog.strgrdMinMaxFactors.Cells[LCol,LRow],-1);
              if(LInteger = -1) then
              begin
                GrowthProjectionsDialog.strgrdMinMaxFactors.ValidationError[LCol, LRow,gveCellContext] :=
                  FAppModules.Language.GetString('ErrorString.ArcNumberTypeError');
                Exit;
              end;
            end;
          end
          else
          begin
            if(Trim(GrowthProjectionsDialog.strgrdMinMaxFactors.Cells[LCol,LRow]) <> '') then
            begin
              LDouble := StrToFloatDef(GrowthProjectionsDialog.strgrdMinMaxFactors.Cells[LCol,LRow],NullFloat);
              if(LDouble = NullFloat) then
              begin
                GrowthProjectionsDialog.strgrdMinMaxFactors.ValidationError[LCol, LRow,gveCellContext] :=
                  FAppModules.Language.GetString('ErrorString.GrowthProjectionTypeError');
                Exit;
              end;
            end;
          end;
        end;
      end;
      Result := True;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TGrowthProjectionsValidator.ValidateHydrologyGrowthFactorsFromExcel: boolean;
const OPNAME = 'TGrowthProjectionsValidator.ValidateHydrologyGrowthFactorsFromExcel';
var
  LRow,
  LCol: integer;
  LDouble: Double;
begin
  Result := False;
  try
    //check the growth projections are all float
    if(GrowthProjectionsDialog.strgrdHydrologyFactors.ColCount >= 3)then
    begin
      for LRow := 2 to GrowthProjectionsDialog.strgrdHydrologyFactors.RowCount-1 do
      begin
        for LCol := 3 to GrowthProjectionsDialog.strgrdHydrologyFactors.ColCount-1 do
        begin
          if(Trim(GrowthProjectionsDialog.strgrdHydrologyFactors.Cells[LCol,LRow]) <> '') then
          begin
            LDouble := StrToFloatDef(GrowthProjectionsDialog.strgrdHydrologyFactors.Cells[LCol,LRow],NullFloat);
            if(LDouble = NullFloat) then
            begin
              GrowthProjectionsDialog.strgrdHydrologyFactors.ValidationError[LCol, LRow,gveCellContext] :=
                  FAppModules.Language.GetString('ErrorString.GrowthProjectionTypeError');
              Exit;
            end;
          end;
        end;
      end;
      Result := True;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TGrowthProjectionsValidator.UpdateNumYears(AGrowthFactors: TExelGrowthFactors);
const OPNAME = 'TGrowthProjectionsValidator.UpdateNumYears';
var
  LMessage    : string;
  LFieldValue : string;
  LValue      : integer;
begin
  try
    if (AGrowthFactors <> nil) then
    begin
      with GrowthProjectionsDialog do
      begin
        edtNumYears.FieldValidationError :='';
        LFieldValue := edtNumYears.Text;
        if (FAppModules.FieldProperties.ValidateFieldProperty('GrowthProjectionYearsCount',
            LFieldValue, LMessage)) then
        begin
          if (Trim(LFieldValue) = '') then
            LFieldValue := '0';
          LValue := StrToInt(LFieldValue);
          AGrowthFactors.YearsCount := LValue;
          RepopulateDataViewer;
          DoContextValidation(dvtNumberOfYears);
        end
        else
          edtNumYears.FieldValidationError := LMessage;
      end;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TGrowthProjectionsValidator.UpdateStartYear(AGrowthFactors: TExelGrowthFactors);
const OPNAME = 'TGrowthProjectionsValidator.UpdateStartYear';
var
  LMessage,
  LFieldValue : string;
  LValue : integer;
begin
  try
//    LRunConfig  := TYieldModelDataObject(FAppModules.Model.ModelData).RunConfigurationData;
 //   LValue  := LRunConfig.HistoricSequenceStartYear;

    if (AGrowthFactors <> nil) then
    begin
      with GrowthProjectionsDialog do
      begin
        edtStartYear.FieldValidationError :='';
        LFieldValue := edtStartYear.Text;
        if (FAppModules.FieldProperties.ValidateFieldProperty('GrowthProjectionStartYear',LFieldValue, LMessage)) then
        begin
          if (Trim(LFieldValue) = '') then
            LFieldValue := '0';  

          LValue := StrToInt(LFieldValue);
          AGrowthFactors.StartYear :=  LValue;
          RepopulateDataViewer;
          DoContextValidation(dvtStartYear);
        end
        else
          edtStartYear.FieldValidationError := LMessage;
      end;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;
 {
procedure TGrowthProjectionsValidator.UpdateBaseYear(AGrowthFactors: TExelGrowthFactors);
const OPNAME = 'TGrowthProjectionsValidator.UpdateBaseYear';
var
  LMessage,
  LFieldValue : string;
  LValue : integer;
begin
  try
    if (AGrowthFactors <> nil) then
    begin
      with GrowthProjectionsDialog do
      begin
        //edtBaseYear.FieldValidationError :='';
        //BaseYearCbx.ValidationError := '';
        //LFieldValue := edtBaseYear.Text;
      //  LFieldValue := BaseYearCbx.Items[BaseYearCbx.ItemIndex];
        if (FAppModules.FieldProperties.ValidateFieldProperty('GrowthProjectionBaseYear', LFieldValue, LMessage)) then
        begin
          if (Trim(LFieldValue) = '') then
            LFieldValue := '0';
          LValue := StrToInt(LFieldValue);
          AGrowthFactors.BaseYear := LValue;
          RepopulateDataViewer;
          DoContextValidation(dvtBaseYear);
        end
        else
          BaseYearCbx.ValidationError := LMessage;
          //edtBaseYear.FieldValidationError := LMessage;
      end;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;
 }
procedure TGrowthProjectionsValidator.ValidateNumYears(AGrowthFactors: TExelGrowthFactors);
const OPNAME = 'TGrowthProjectionsValidator.ValidateNumYears';
begin
  try
    if (AGrowthFactors <> nil) then
    begin
      with GrowthProjectionsDialog do
      begin
        FErrorMessage := '';
        if (AGrowthFactors.Validate(FErrorMessage, 'NumberYears')) then
          edtNumYears.ContextValidationError := ''
        else
          edtNumYears.ContextValidationError := FErrorMessage;
      end;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;
{
procedure TGrowthProjectionsValidator.ValidateBaseYear(AGrowthFactors: TExelGrowthFactors);
const OPNAME = 'TGrowthProjectionsValidator.ValidateBaseYear';
begin
  try
    if (AGrowthFactors <> nil) then
    begin
      with GrowthProjectionsDialog do
      begin
        FErrorMessage := '';
        if (AGrowthFactors.Validate(FErrorMessage, 'BaseYear')) then
        begin
          //BaseYearCbx.InValidationError := False;
         // BaseYearCbx.ShowErrorState(False);
//          edtBaseYear.ContextValidationError := ''
        end
        else
        begin
          BaseYearCbx.InValidationError := True;
          BaseYearCbx.ValidationError := FErrorMessage;
          BaseYearCbx.ShowErrorState(True);
          FAllErrorMessages.Add(Trim(FErrorMessage));
//          edtBaseYear.ContextValidationError := FErrorMessage;
        end;
      end;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;
}

procedure TGrowthProjectionsValidator.ValidateStartYear(AGrowthFactors: TExelGrowthFactors);
const OPNAME = 'TGrowthProjectionsValidator.ValidateStartYear';
begin
  try
    if (AGrowthFactors <> nil) then
    begin
      with GrowthProjectionsDialog do
      begin
        FErrorMessage := '';
        if (AGrowthFactors.Validate(FErrorMessage, 'StartYear')) then
          edtStartYear.ContextValidationError := ''
        else
          edtStartYear.ContextValidationError := FErrorMessage;
      end;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TGrowthProjectionsValidator.DoContextValidation(AValidationType: TDialogValidationType);
const OPNAME = 'TGrowthProjectionsValidator.DoContextValidation';
var
  LGrowthFactors: TExelGrowthFactors;
begin
  try
    LGrowthFactors := TPlanningModelDataObject(FAppModules.Model.ModelData).CastExelGrowthFactors;
    if (LGrowthFactors <> nil) then
    begin
      if (AValidationType = dvtNumberOfYears) then
          ValidateNumYears(LGrowthFactors);
      if (AValidationType = dvtStartYear) then
          ValidateStartYear(LGrowthFactors);
      //if (AValidationType = dvtBaseYear) then
      //    ValidateBaseYear(LGrowthFactors);
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TGrowthProjectionsValidator.OnStringGridCellDataHasChanged(ASender: TObject; ACol, ARow: integer);
const OPNAME = 'TGrowthProjectionsValidator.OnStringGridCellDataHasChanged';
var
  LGrowthFactors: TExelGrowthFactors;
begin
  try
    LGrowthFactors            := TPlanningModelDataObject(FAppModules.Model.ModelData).CastExelGrowthFactors;
    if(ACol = 0) then
      UpdateInstitution(ACol,ARow)
    else
    if(ACol = 1) then
      UpdateWaterUser(ACol,ARow)
    else
    if(ACol = 2) then
      UpdateChannelNumber(ACol,ARow)
    else
    if(ACol >= 3) then
    begin
      case LGrowthFactors.ProjectionType of
        ptDemand: UpdateExcelDemandGrowthFactors(ACol, ARow);
        ptMinMax: UpdateMinMaxChannelGrowthFactors(ACol, ARow);
        ptHydrology: UpdateHydrologyGrowthFactors(ACol, ARow);
      end;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TGrowthProjectionsValidator.UpdateInstitution(ACol, ARow: integer);
const OPNAME = 'TGrowthProjectionsValidator.UpdateInstitution';
var
  LGrowthFactors: TExelGrowthFactors;
  LDemandChannelFactors : TExelDemandChannelGrowthFactors;
  LMinMaxChannelFactors : TExelMinMaxChannelGrowthFactors;
  LHydrologyFactors     : TExelHydrologyGrowthFactors;
begin
  try
    if (ARow <= 0) or (ARow >= CurrentGrid.RowCount)then Exit;
    LGrowthFactors := TPlanningModelDataObject(FAppModules.Model.ModelData).CastExelGrowthFactors;
    case LGrowthFactors.ProjectionType of
      ptDemand:
        begin
          LDemandChannelFactors := LGrowthFactors.DemandChannelGrowthFactorsByIdentifier[ARow];
          if Assigned(LDemandChannelFactors) then
            LDemandChannelFactors.Institution := GrowthProjectionsDialog.strgrdDemandFactors.Cells[ACol, ARow];
        end;
      ptMinMax:
        begin
          LMinMaxChannelFactors := LGrowthFactors.MinMaxChannelGrowthFactorsByIdentifier[ARow];
          if Assigned(LMinMaxChannelFactors) then
            LMinMaxChannelFactors.Institution := GrowthProjectionsDialog.strgrdMinMaxFactors.Cells[ACol, ARow];
        end;
      ptHydrology:
        begin
          LHydrologyFactors := LGrowthFactors.HydrologyGrowthFactorsByIdentifier[ARow];
          if Assigned(LHydrologyFactors) then
            LHydrologyFactors.Institution := GrowthProjectionsDialog.strgrdHydrologyFactors.Cells[ACol, ARow];
        end;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TGrowthProjectionsValidator.UpdateWaterUser(ACol, ARow: integer);
const OPNAME = 'TGrowthProjectionsValidator.UpdateWaterUser';
var
  LGrowthFactors        : TExelGrowthFactors;
  LDemandChannelFactors : TExelDemandChannelGrowthFactors;
  LMinMaxChannelFactors : TExelMinMaxChannelGrowthFactors;
  LHydrologyFactors     : TExelHydrologyGrowthFactors;
begin
  try
    if (ARow <= 0) or (ARow >= CurrentGrid.RowCount)then Exit;
    LGrowthFactors := TPlanningModelDataObject(FAppModules.Model.ModelData).CastExelGrowthFactors;
    case LGrowthFactors.ProjectionType of
      ptDemand:
        begin
          LDemandChannelFactors := LGrowthFactors.DemandChannelGrowthFactorsByIdentifier[ARow];
          if Assigned(LDemandChannelFactors) then
            LDemandChannelFactors.WaterUser := GrowthProjectionsDialog.strgrdDemandFactors.Cells[ACol, ARow];
        end;
      ptMinMax:
        begin
          LMinMaxChannelFactors := LGrowthFactors.MinMaxChannelGrowthFactorsByIdentifier[ARow];
          if Assigned(LMinMaxChannelFactors) then
            LMinMaxChannelFactors.WaterUser := GrowthProjectionsDialog.strgrdMinMaxFactors.Cells[ACol, ARow];
        end;
      ptHydrology:
        begin
          LHydrologyFactors := LGrowthFactors.HydrologyGrowthFactorsByIdentifier[ARow];
          if Assigned(LHydrologyFactors) then
            LHydrologyFactors.WaterUser := GrowthProjectionsDialog.strgrdHydrologyFactors.Cells[ACol, ARow];
        end;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TGrowthProjectionsValidator.UpdateChannelNumber(ACol, ARow: integer);
const OPNAME = 'TGrowthProjectionsValidator.UpdateChannelNumber';
var
  LGrowthFactors: TExelGrowthFactors;
  LDemandChannelFactors : TExelDemandChannelGrowthFactors;
  LMinMaxChannelFactors : TExelMinMaxChannelGrowthFactors;
  LHydrologyFactors     : TExelHydrologyGrowthFactors;
begin
  try
    if (ARow <= 0) or (ARow >= CurrentGrid.RowCount)then Exit;
    LGrowthFactors := TPlanningModelDataObject(FAppModules.Model.ModelData).CastExelGrowthFactors;
    case LGrowthFactors.ProjectionType of
      ptDemand:
        begin
          LDemandChannelFactors := LGrowthFactors.DemandChannelGrowthFactorsByIdentifier[ARow];
          if Assigned(LDemandChannelFactors) then
            LDemandChannelFactors.ChannelNumber := StrToInt(GrowthProjectionsDialog.strgrdDemandFactors.Cells[ACol, ARow]);
        end;
      ptMinMax:
        begin
          LMinMaxChannelFactors := LGrowthFactors.MinMaxChannelGrowthFactorsByIdentifier[ARow];
          if Assigned(LMinMaxChannelFactors) then
            LMinMaxChannelFactors.ChannelNumber := StrToInt(GrowthProjectionsDialog.strgrdMinMaxFactors.Cells[ACol, ARow]);
        end;
      ptHydrology:
        begin
          LHydrologyFactors := LGrowthFactors.HydrologyGrowthFactorsByIdentifier[ARow];
          if Assigned(LHydrologyFactors) then
            LHydrologyFactors.GaugeNumber := StrToInt(GrowthProjectionsDialog.strgrdHydrologyFactors.Cells[ACol, ARow]);
        end;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TGrowthProjectionsValidator.UpdateExcelDemandGrowthFactors(ACol, ARow: integer);
const OPNAME = 'TGrowthProjectionsValidator.UpdateExcelDemandGrowthFactors';
var
  LIndex : integer;
  LRowData: TStringList;
  LGrowthFactors: TExelGrowthFactors;
  LDemandChannelFactors : TExelDemandChannelGrowthFactors;
  LChannelNumber : integer;
begin
  try
    if (ARow <= 0) or (ARow >= GrowthProjectionsDialog.strgrdDemandFactors.RowCount)then Exit;
    LGrowthFactors := TPlanningModelDataObject(FAppModules.Model.ModelData).CastExelGrowthFactors;
    //LDemandChannelFactors := LGrowthFactors.DemandChannelGrowthFactorsByIdentifier[ARow];
    LChannelNumber := Integer(GrowthProjectionsDialog.strgrdDemandFactors.Objects[2,ARow]);
    LDemandChannelFactors := LGrowthFactors.GetDemandChannelGrowthFactorsByChannel(LChannelNumber);
    if Assigned(LDemandChannelFactors) then
    begin
      LRowData := TStringList.Create;
      try
        LRowData.Assign(GrowthProjectionsDialog.strgrdDemandFactors.Rows[ARow]);
        for LIndex := 0 to 3 do
          LRowData.Delete(0);
        LDemandChannelFactors.GrowthFactors := LRowData.CommaText;

      finally
        LRowData.Free;
      end;
    end;
  except on E: Exception do HandleError( E, OPNAME ) end;
end;

procedure TGrowthProjectionsValidator.UpdateMinMaxChannelGrowthFactors(ACol, ARow: integer);
const OPNAME = 'TGrowthProjectionsValidator.UpdateMinMaxChannelGrowthFactors';
var
  LIndex : integer;
  LRowData: TStringList;
  LGrowthFactors: TExelGrowthFactors;
  LMinMaxChannelFactors : TExelMinMaxChannelGrowthFactors;
  LArc : integer;
  LChannelNumber : integer;
begin
  try
    if (ARow <= 0) or (ARow >= GrowthProjectionsDialog.strgrdMinMaxFactors.RowCount)then Exit;
    LGrowthFactors := TPlanningModelDataObject(FAppModules.Model.ModelData).CastExelGrowthFactors;
    //LMinMaxChannelFactors := LGrowthFactors.MinMaxChannelGrowthFactorsByIdentifier[ARow];
    LChannelNumber :=  StrToInt(GrowthProjectionsDialog.strgrdMinMaxFactors.Cells[2,ARow]);
    LArc := StrToInt(GrowthProjectionsDialog.strgrdMinMaxFactors.Cells[3,ARow]);
    LMinMaxChannelFactors := LGrowthFactors.GetExelMinMaxChannelGrowthFactorsByCannelArc(LArc,LChannelNumber);
    if Assigned(LMinMaxChannelFactors) then
    begin
      LRowData := TStringList.Create;
      try
        LRowData.Assign(GrowthProjectionsDialog.strgrdMinMaxFactors.Rows[ARow]);
        for LIndex := 0 to 4 do
          LRowData.Delete(0);
        LMinMaxChannelFactors.GrowthFactors := LRowData.CommaText;
      finally
        LRowData.Free;
      end;
    end;
  except on E: Exception do HandleError( E, OPNAME ) end;
end;

procedure TGrowthProjectionsValidator.UpdateHydrologyGrowthFactors(ACol,ARow: integer);
const OPNAME = 'TGrowthProjectionsValidator.UpdateHydrologyGrowthFactors';
var
  LIdentifier,
  LFactorType,
  LIndex : integer;
  LRowData: TStringList;
  LGrowthFactors: TExelGrowthFactors;
  LHydrologyFactors     : TExelHydrologyGrowthFactors;
begin
  try
    if (ARow <= 0) or (ARow >= GrowthProjectionsDialog.strgrdHydrologyFactors.RowCount)then Exit;
    LGrowthFactors := TPlanningModelDataObject(FAppModules.Model.ModelData).CastExelGrowthFactors;
    if((ARow mod 3) = 0) then
      LIdentifier := (ARow div 3)
    else
      LIdentifier := (ARow div 3) + 1;
    LFactorType := ARow mod 3;
    if(LFactorType = 0) then LFactorType := 3;

    LHydrologyFactors := LGrowthFactors.HydrologyGrowthFactorsByIdentifier[LIdentifier];
    if Assigned(LHydrologyFactors) then
    begin
      LRowData := TStringList.Create;
      try
        LRowData.Assign(GrowthProjectionsDialog.strgrdHydrologyFactors.Rows[ARow]);
        for LIndex := 0 to 3 do
          LRowData.Delete(0);
        case LFactorType of
          1: LHydrologyFactors.AFFGrowthFactors := LRowData.CommaText;
          2: LHydrologyFactors.IRRGrowthFactors := LRowData.CommaText;
          3: LHydrologyFactors.URBGrowthFactors := LRowData.CommaText;
        end;
      finally
        LRowData.Free;
      end;
    end;
  except on E: Exception do HandleError( E, OPNAME ) end;
end;

function TGrowthProjectionsValidator.GetCurrentGrid: TFieldStringGrid;
const OPNAME = 'TGrowthProjectionsValidator.GetCurrentGrid';
var
  LGrowthFactors: TExelGrowthFactors;
begin
  Result := nil;
  try
    LGrowthFactors            := TPlanningModelDataObject(FAppModules.Model.ModelData).CastExelGrowthFactors;
    case LGrowthFactors.ProjectionType of
      ptDemand: Result := GrowthProjectionsDialog.strgrdDemandFactors;
      ptMinMax: Result := GrowthProjectionsDialog.strgrdMinMaxFactors;
      ptHydrology: Result := GrowthProjectionsDialog.strgrdHydrologyFactors;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TGrowthProjectionsValidator.DoDrawGrid(Sender: TObject; ACol,ARow: Integer; Rect: TRect; State: TGridDrawState);
const OPNAME = 'TGrowthProjectionsValidator.DoDrawGrid';
var
  LGrowthFactors: TExelGrowthFactors;
begin
  try
    LGrowthFactors            := TPlanningModelDataObject(FAppModules.Model.ModelData).CastExelGrowthFactors;
    if(LGrowthFactors.ProjectionType = ptMinMax) or (LGrowthFactors.ProjectionType = ptHydrology) then
    begin
      if (LGrowthFactors.ProjectionType = ptHydrology) then
      begin
        if (ACol = FBaseYearIndex) and (ARow > 0) then
        begin
          (Sender as TFieldStringGrid).Canvas.Font.Color := clWhite;
          if ((Sender as TFieldStringGrid).Canvas.Brush.Color = clHighlight) then
            (Sender as TFieldStringGrid).Canvas.Font.Color := clWhite;
          if (FBaseYearIndex = -1 ) then
            (Sender as TFieldStringGrid).Canvas.Brush.Color := clWhite
          else
            (Sender as TFieldStringGrid).Canvas.Brush.Color := clGreen;
          (Sender as TFieldStringGrid).Canvas.TextRect(Rect, Rect.Left, Rect.Top, (Sender as TFieldStringGrid).Cells[ACol, ARow]);
        end;
      end;
      
      if (ACol = FStartYearIndex) and (ARow > 0) then
      begin
        (Sender as TFieldStringGrid).Canvas.Font.Color := clRed;
        if ((Sender as TFieldStringGrid).Canvas.Brush.Color = clHighlight) then
          (Sender as TFieldStringGrid).Canvas.Font.Color := clBlack;
        if (FStartYearIndex = -1 ) then
          (Sender as TFieldStringGrid).Canvas.Brush.Color := clWhite
        else
          (Sender as TFieldStringGrid).Canvas.Brush.Color := clYellow;
        (Sender as TFieldStringGrid).Canvas.TextRect(Rect, Rect.Left, Rect.Top, (Sender as TFieldStringGrid).Cells[ACol, ARow]);
      end;
    end;

    if(LGrowthFactors.ProjectionType = ptDemand) then
    begin
      {if (ACol = FBaseYearIndex) and (ARow > 0) then
      begin
        (Sender as TFieldStringGrid).Canvas.Font.Color := clWhite;
        if ((Sender as TFieldStringGrid).Canvas.Brush.Color = clHighlight) then
          (Sender as TFieldStringGrid).Canvas.Font.Color := clWhite;
        if (FBaseYearIndex = -1 ) then
          (Sender as TFieldStringGrid).Canvas.Brush.Color := clWhite
        else
          (Sender as TFieldStringGrid).Canvas.Brush.Color := clGreen;
        (Sender as TFieldStringGrid).Canvas.TextRect(Rect, Rect.Left, Rect.Top, (Sender as TFieldStringGrid).Cells[ACol, ARow]);
      end;
       }
      if (ACol = FStartYearIndex) and (ARow > 0) then
      begin
        (Sender as TFieldStringGrid).Canvas.Font.Color := clRed;
        if ((Sender as TFieldStringGrid).Canvas.Brush.Color = clHighlight) then
          (Sender as TFieldStringGrid).Canvas.Font.Color := clBlack;

        if (FStartYearIndex = -1 ) then
          (Sender as TFieldStringGrid).Canvas.Brush.Color := clWhite
        else
          (Sender as TFieldStringGrid).Canvas.Brush.Color := clYellow;
        (Sender as TFieldStringGrid).Canvas.TextRect(Rect, Rect.Left, Rect.Top, (Sender as TFieldStringGrid).Cells[ACol, ARow]);
      end;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TGrowthProjectionsValidator.RePopulateMinMaxChannelGrowthFactorsGrid(AGrid: TFieldStringGrid);
const OPNAME = 'TGrowthProjectionsValidator.RePopulateMinMaxChannelGrowthFactorsGrid';
var
  LRowIndex,
  LColIndex,
  LInsertedRowIndex,
  LNumberOfArcs         : Integer;
  LPenaltyStructure     : IChannelPenalty;
  LChannel              : IGeneralFlowChannel;
begin
  LRowIndex :=1;
  try
    AGrid.RowCount := AGrid.RowCount + 1;
    while LRowIndex < AGrid.RowCount do
    begin
      LChannel          := TYieldModelDataObject(FAppModules.Model.ModelData).
                                NetworkElementData.ChannelList.ChannelByChannelNumber[StrToInt(AGrid.Cells[2, LRowIndex])];
      if (LChannel <> nil) then
      begin
        LPenaltyStructure  := LChannel.ChannelPenalty;
        LNumberOfArcs      := lPenaltyStructure.ChannelPenaltyArcCount;

        if (LNumberOfArcs = 2)and (AGrid.Cells[2,LRowIndex] <> AGrid.Cells[2,LRowIndex + 1])and
                   (AGrid.Cells[3,LRowIndex] <>IntToStr(LNumberOfArcs))  then
        begin
          LInsertedRowIndex :=  AGrid.InsertRow(LRowIndex);
          for LColIndex := 0 to AGrid.ColCount -1 do
            AGrid.Cells[LColIndex,LInsertedRowIndex] := AGrid.Cells[LColIndex,LRowIndex+1 ];

          AGrid.Cells[3,LInsertedRowIndex + 1] := '2';
        end
        else
          if(AGrid.Cells[2,LRowIndex + 1] = '') then
            break;
      end;
      Inc(LRowIndex);
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TGrowthProjectionsValidator.RePopulateHydrologyGrowthFactorsGrid(AGrid: TFieldStringGrid);// ;AGrowthFactors: TExelGrowthFactors);
const OPNAME = 'TGrowthProjectionsValidator.RePopulateHydrologyGrowthFactorsGrid';
  var
  LCount,
  LIndex,
  LRowIndex,
  LColIndex,
//  LGuageIndex,
  LHydroGridIndex,
  LReferenceCount    : Integer;
  LParamReference    : IParamReference;
  LGuageNumbers      : TStringList;
begin
  try
    LRowIndex := 1;
    LReferenceCount := TYieldModelDataObject(FAppModules.Model.ModelData).ParamSetup.ReferenceCount;
    LGuageNumbers   := TStringList.Create;
    try

      for LIndex := 1 to  LReferenceCount do
      begin
        LParamReference := TYieldModelDataObject(FAppModules.Model.ModelData).ParamSetup.
                              ReferenceDataByCatchNumber[LIndex];
        LCount := 0;
        if(LParamReference <> nil) then
        begin
          for LHydroGridIndex := LRowIndex to AGrid.RowCount -1  do
          begin
            if (IntToStr(LIndex) <> AGrid.Cells[2,LHydroGridIndex])and
                 (LIndex < StrToInt(AGrid.Cells[2,LHydroGridIndex])) then // and
              //  not (SearchGaugeNumber(IntToStr(LIndex),LGuageNumbers)) then
            begin
              AGrid.InsertRow(LHydroGridIndex);
              AGrid.Cells[1,LHydroGridIndex] := 'Urban Runoff';
              AGrid.Cells[2,LHydroGridIndex] := IntToStr(Lindex);
              for LColIndex := 3 to AGrid.ColCount do
                AGrid.Cells[LColIndex,LHydroGridIndex] := '0.0';

              AGrid.InsertRow(LHydroGridIndex);
              AGrid.Cells[0,LHydroGridIndex] :=  '(' + ExtractFileName(LParamReference.FileReference) +')';
              AGrid.Cells[1,LHydroGridIndex] := 'Diffuse Irrigation';
              AGrid.Cells[2,LHydroGridIndex] := IntToStr(Lindex);
              for LColIndex := 3 to AGrid.ColCount do
                AGrid.Cells[LColIndex,LHydroGridIndex] := '0.0';

              AGrid.InsertRow(LHydroGridIndex);
              AGrid.Cells[1,LHydroGridIndex] := 'Afforestation';
              AGrid.Cells[2,LHydroGridIndex] := IntToStr(Lindex);
              for LColIndex := 3 to AGrid.ColCount do
                AGrid.Cells[LColIndex,LHydroGridIndex] := '0.0';
             break;
            end
            else
            begin
              Inc(LCount);
              for LColIndex := 0 to AGrid.ColCount do
                AGrid.Cells[LColIndex,LHydroGridIndex] := AGrid.Cells[LColIndex,LHydroGridIndex];
              if LCount = 3 then
                break;
            end;
          end;
          LRowIndex := LRowIndex + 3;
        end;
      end;
    finally
      LGuageNumbers.Free;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TGrowthProjectionsValidator.SearchGaugeNumber(AGaugeNo: string; AGaugerList: TStringList): boolean;
const OPNAME = 'TGrowthProjectionsValidator.SearchGaugeNumber';
var
  LIndex,
  LGaugeIndex : Integer;
begin
  Result       := true;
  LGaugeIndex  := 0;
  try
    for LIndex := 0 to AGaugerList.Count do
      if AGaugerList.Find(AGaugeNo,LGaugeIndex) then
        Exit;
       Inc(LGaugeIndex);
    Result := false;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TGrowthProjectionsValidator.OnValidateProjectionsClick( Sender: TObject);
const OPNAME = 'TGrowthProjectionsValidator.OnValidateProjectionsClick';
var
  LDialog : TProgressDialog;
begin
  try
    LDialog := TProgressDialog.Create(nil,FAppModules);
    try
      LDialog.Caption := FAppModules.Language.GetString('DialogCaption.TProgressDialog');
    
      LDialog.AddExecutionFunctions(ExecValidateGrowthFiles);
      LDialog.ProgressRichEdit.Clear;
      LDialog.clbOptions.Items.Clear;
      LDialog.ShowModal;
    finally
      LDialog.Free;
    end;

  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TGrowthProjectionsValidator.ExecValidateGrowthFiles(AProgressUpdateFuntion: TProgressUpdateFuntion): boolean;
const OPNAME = 'TGrowthProjectionsValidator.ExecValidateGrowthFiles';
  function ErrorSeverity(var AErrorMsg: string): integer;
  const OPNAME = 'UGrowthProjectionsValidator.ErrorSeverity';
  begin
    Result := 0;
    try
      if(Pos('WARNING:',AErrorMsg) = 1) then
      begin
        AErrorMsg := Copy(AErrorMsg,9,Length(AErrorMsg));
        Result    := 1;
      end
      else
      if(Pos('ERROR:',AErrorMsg) = 1) then
      begin
        AErrorMsg := Copy(AErrorMsg,7,Length(AErrorMsg));
        Result    := 2;
      end;
    except on E: Exception do HandleError(E, OPNAME) end;
  end;

var
  LColumns,
  LValidationErrors: TStringList;
  LErrors: WideString;
  LCount: integer;
  LStop: boolean;
  LErrorMsg: string;
begin
  Result := False;
  try
    if not Assigned(AProgressUpdateFuntion) then
      AProgressUpdateFuntion := DummyShowProgress;
    LColumns := TStringList.Create;
    LValidationErrors := TStringList.Create;
    try
      AProgressUpdateFuntion('Started validating model data',ptNone,LStop);
      Result := TPlanningModelDataObject(FAppModules.Model.ModelData).CastExelGrowthFactors.Validate(LErrors,'');
      while (LErrors <> '') do
      begin
        LErrors := CutErrorsAndColumns(LErrors,LValidationErrors,LColumns);
        for LCount := 0 to LValidationErrors.Count -1 do
        begin
          LErrorMsg := LValidationErrors[LCount];
          if(ErrorSeverity(LErrorMsg)= 1) then
            AProgressUpdateFuntion(LErrorMsg,ptWarning,LStop)
          else
            AProgressUpdateFuntion(LErrorMsg,ptError,LStop);
        end;
      end;
      AProgressUpdateFuntion('Completed validating model data',ptNone,LStop);
    finally
      LColumns.Free;
      LValidationErrors.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;
{
procedure TGrowthProjectionsValidator.OnBaseYearChange(ASender: TObject);
const OPNAME = 'TGrowthProjectionsValidator.OnBaseYearChange';
var
  LGrowthFactors: TExelGrowthFactors;
begin
  try
    if((ASender = GrowthProjectionsDialog.BaseYearCbx) AND
      (GrowthProjectionsDialog.BaseYearCbx.HasValueChanged)) then
    begin
      LGrowthFactors := TPlanningModelDataObject(FAppModules.Model.ModelData).CastExelGrowthFactors;
      if(LGrowthFactors.BaseYear <> StrToInt(GrowthProjectionsDialog.BaseYearCbx.Items[GrowthProjectionsDialog.BaseYearCbx.ItemIndex])) then
      begin
        UpdateBaseYear(LGrowthFactors);
        MessageDlg('Growth factors need to be re-generated',mtWarning,[mbOK], 0);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;
  }
end.


