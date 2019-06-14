//
//
//  UNIT      : Contains the class TGrowthFactorsValidator.
//  AUTHOR    : Dziedzi Ramulondi (Cornastone)
//  DATE      : 2006/05/11
//  COPYRIGHT : Copyright © 2006 DWAF
//
//
unit UGrowthFactorsValidator;

interface

uses
  Classes,
  VCL.Controls,
  VCL.ComCtrls,
  VCL.StdCtrls,
  VCL.ExtCtrls,
  Vcl.Grids,
  VoaimsCom_TLB,
  UAbstractObject,
  UAbstractComponent,
  UDataComponent,
  UGrowthFactorData,
  UDataEditComponent,
  UYieldContextValidationType,
  UAbstractYieldDataDialogValidator,
  UGrowthFactorsDialog;

type
  TGrowthFactorsValidator = class(TAbstractYieldDataDialogValidator)
  protected
    FChannelNumber : integer;
    FMinMaxChannel : integer;
    FGaugeNumber   : integer;
    FGrowthFactorsIndex : integer;
    FArcNumberIndex : integer;
    FSelectedCol : TGridCoord;
    procedure CreateMemberObjects; override;
    procedure OnEditControltExit(Sender: TObject); override;
    procedure OnStringGridCellDataHasChanged(ASender: TObject; ACol, ARow: integer);override;

    procedure OnViewDataHasChanged(Sender: TObject);
    procedure OnGenerateProjectionsClick(Sender: TObject);
    procedure OnPasteFromExcelClick(Sender: TObject);
    procedure OnTabChanged(Sender: Tobject);


    procedure RepopulateDataViewer;
    procedure PopulateDemandCentreGrowthFactors(AGrowthFactors : TGrowthFactors);
    procedure PopulateMinMaxChannelGrowthFactors(AGrowthFactors : TGrowthFactors);
    procedure PopulateHydrologyGrowthFactors(AGrowthFactors : TGrowthFactors);

    procedure UpdateNumYears(AGrowthFactors : TGrowthFactors);
    procedure UpdateDemandCentreGrowthFactors(AGrowthFactors : TGrowthFactors;AChannelNumber, ACol, ARow:integer);

    procedure UpdateArcNumber(AGrowthFactors : TGrowthFactors;AMinMaxChannel,ACol, ARow:integer);
    procedure UpdateMinMaxChannelGrowthFactors(AGrowthFactors : TGrowthFactors;AMinMaxChannel,ACol, ARow:integer);

    procedure UpdateAFFGrowthFactors(AGrowthFactors : TGrowthFactors;AGaugeNumber,ACol, ARow:integer);
    procedure UpdateIRRGrowthFactors(AGrowthFactors : TGrowthFactors;AGaugeNumber,ACol, ARow:integer);
    procedure UpdateURBGrowthFactors(AGrowthFactors : TGrowthFactors;AGaugeNumber,ACol, ARow:integer);

    procedure ValidateChannelNumber(ADemadGrowthFactors : IDemandCentreGrowthFactors);
    procedure ValidateDemandGrowthFactors(ADemadGrowthFactors : IDemandCentreGrowthFactors);
    procedure ValidateNumberOfYears(AGrowthFactors : TGrowthFactors);

    procedure ValidateArcNumber(AMinMaxGrowthFactors : IMinMaxChannelGrowthFactors);
    procedure ValidateMinMaxChannel(AMinMaxGrowthFactors : IMinMaxChannelGrowthFactors);
    procedure ValidateMinMaxGowthFactors(AMinMaxGrowthFactors : IMinMaxChannelGrowthFactors);

    procedure ValidateAFFGrowthFactors(AHydrologyGrowthFactors:IHydrologyGrowthFactors);
    procedure ValidateIRRGrowthFactors(AHydrologyGrowthFactors:IHydrologyGrowthFactors);
    procedure ValidateURBGrowthFactors(AHydrologyGrowthFactors:IHydrologyGrowthFactors);
    function ValidateDemandCentreGrowthFactorsFromExcel: boolean;
    function ValidateDataFromExcel: boolean;

    function PopulateDemandChannelGrowthFactorsObject(AFactors: TGrowthFactors): boolean;
    function PopulateMinMaxChannelGrowthFactorsObject(AFactors: TGrowthFactors): boolean;
    function PopulateHydrologyGrowthFactorsObject(AFactors: TGrowthFactors): boolean;


    procedure PopulateGrowthFactorsFromExcel;
    function PopulateYearsObject(AFactors: TGrowthFactors): boolean;
    procedure RemoveEmptyColumn(AGrid: TFieldStringGrid);

  public
    function Initialise: boolean; override;
    function SaveState: boolean; override;
    function LanguageHasChanged: boolean; override;
    function StudyHasChanged: boolean; override;
    function StudyDataHasChanged(AContext: TChangeContext; AFieldName,AOldValue,ANewValue: string): boolean; override;

    procedure ClearDataViewer; override;
    procedure PopulateDataViewer; override;
    procedure DoContextValidation(AValidationType: TDialogValidationType);override;
    function GrowthFactorsDialog: TGrowthFactorsDialog;

  end;

implementation

uses
  System.UITypes,
  SysUtils,
  VCL.Dialogs,
  Vcl.Graphics,
  UUtilities,
  UConstants,
  UYieldModelDataObject,
  UPlanningModelDataObject,
  UErrorHandlingOperations;

{ TGrowthFactorsValidator }

procedure TGrowthFactorsValidator.CreateMemberObjects;
const OPNAME = 'TGrowthFactorsValidator.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
    FPanel := TGrowthFactorsDialog.Create(FPanelOwner,FAppModules);

    with GrowthFactorsDialog do
    begin
      strgrdDemandFactors.OnBeforeCellChange    := OnStringGridCellDataHasChanged;
      strgrdDemandFactors.OnColEnter            := OnStringGridColEnter;
      strgrdDemandFactors.OnPasteFromExcel      := OnPasteFromExcelClick;
      strgrdDemandFactors.AllowPasteFromExcel   := True;

      strgrdMinMaxFactors.OnBeforeCellChange    := OnStringGridCellDataHasChanged;
      strgrdMinMaxFactors.OnColEnter            := OnStringGridColEnter;
      strgrdMinMaxFactors.OnPasteFromExcel    := OnPasteFromExcelClick;
      strgrdMinMaxFactors.AllowPasteFromExcel   := True;

      strgrdHydrologyFactors.OnBeforeCellChange := OnStringGridCellDataHasChanged;
      strgrdHydrologyFactors.OnColEnter         := OnStringGridColEnter;
      strgrdHydrologyFactors.OnPasteFromExcel    := OnPasteFromExcelClick;
      strgrdHydrologyFactors.AllowPasteFromExcel   := True;


      pgcProjections.OnChange   := OnTabChanged;

      //edtNumYears.FieldProperty := FAppModules.FieldProperties.FieldProperty('GrowthFactorsYearCount');
      edtNumYears.Color         := clBtnShadow;
      edtNumYears.OnExit        := OnEditControltExit;
      edtNumYears.OnEnter       := OnEditControlEnter;
      edtNumYears.IsEnabled     := False;

      //edtStartYear.FieldProperty := FAppModules.FieldProperties.FieldProperty('GrowthFactorStartYear');
      edtStartYear.Color         := clBtnShadow;
      edtStartYear.OnExit        := OnEditControltExit;
      edtStartYear.OnEnter       := OnEditControlEnter;
      edtStartYear.IsEnabled     := False;

      //edtEndYear.FieldProperty := FAppModules.FieldProperties.FieldProperty('GrowthFactorEndYear');
      edtEndYear.Color         := clBtnShadow;
      edtEndYear.OnExit        := OnEditControltExit;
      edtEndYear.OnEnter       := OnEditControlEnter;
      edtEndYear.IsEnabled     := False;

      btnGenerateProjections.OnClick    := OnGenerateProjectionsClick;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TGrowthFactorsValidator.Initialise: boolean;
const OPNAME = 'TGrowthFactorsValidator.Initialise';
begin
  Result := inherited Initialise;
  try
    ClearDataViewer;
    FGrowthFactorsIndex := 0;
    FArcNumberIndex := 0;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TGrowthFactorsValidator.LanguageHasChanged: boolean;
const OPNAME = 'TGrowthFactorsValidator.LanguageHasChanged';
begin
  Result := False;
  try
    TabShetCaption := FAppModules.Language.GetString('TabCaption.GrowthFactors');
    Result := inherited LanguageHasChanged;
    RepopulateDataViewer;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TGrowthFactorsValidator.ClearDataViewer;
const OPNAME = 'TGrowthFactorsValidator.ClearDataViewer';
begin
  inherited ClearDataViewer;
  try
    FChannelNumber := 0;
    FMinMaxChannel := 0;
    FGaugeNumber   := 0;
    GrowthFactorsDialog.btnGenerateProjections.Enabled := false;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TGrowthFactorsValidator.PopulateDataViewer;
const OPNAME = 'TGrowthFactorsValidator.PopulateDataViewer';
begin
  inherited PopulateDataViewer;
  try
    ClearDataViewer;
    RepopulateDataViewer;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

//=============================================================================================


procedure TGrowthFactorsValidator.PopulateGrowthFactorsFromExcel;
const OPNAME = 'TGrowthFactorsValidator.PopulateGrowthFactorsFromExcel';
var
  LGrowthFactors: TGrowthFactors;
begin
  try
    LGrowthFactors := TPlanningModelDataObject(FAppModules.Model.ModelData).CastGrowthFactors;
    if(LGrowthFactors <> nil) then
    begin
      if PopulateYearsObject(LGrowthFactors) then
      begin
        case LGrowthFactors.GrowthType of
          gtDemand: PopulateDemandChannelGrowthFactorsObject(LGrowthFactors);
          gtMinMax: PopulateMinMaxChannelGrowthFactorsObject(LGrowthFactors);
          gtHydrology: PopulateHydrologyGrowthFactorsObject(LGrowthFactors);
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TGrowthFactorsValidator.PopulateDemandChannelGrowthFactorsObject(AFactors: TGrowthFactors): boolean;
const OPNAME = 'TGrowthFactorsValidator.PopulateDemandChannelGrowthFactorsObject';
var
  LRow, LCol,
  LCount: integer;
  LData: TStringList;
  LFieldValue : string;
  LGrowthFactors: TDemandCentreGrowthFactors;
  LValue : double;
begin
  Result := False;
  try
    if (GrowthFactorsDialog.strgrdDemandFactors.ColCount >= 2) then
    begin
      LData := TStringList.Create;
      try
        FSelectedCol := GrowthFactorsDialog.strgrdDemandFactors.CurrenCell;
        for LRow := 0 to GrowthFactorsDialog.strgrdDemandFactors.RowCount-1 do
        begin
          if (LRow >= AFactors.DemandCentresGrowthFactorsCount) then Break;
          LData.Assign(GrowthFactorsDialog.strgrdDemandFactors.Rows[LRow]);
          LCol := FSelectedCol.Y;
          LCol := LCol-1;
          if (LCol <0) then
            LCol := 0;
          if(Trim(LData.Text) <> '') then
          begin
            LGrowthFactors := AFactors.CastDemandGrowthFactorByIndex(LRow);
            if LGrowthFactors <> nil then
            begin
              for LCount := 0 to LData.Count-1 do
              begin
                LFieldValue := LData[LCount];
                if (Trim(LFieldValue) = '') then
                  LFieldValue := '0';
                LValue := StrToFLoat(LFieldValue);
                if (LCol <= AFactors.NumberOfYears-1) then
                begin
                  LGrowthFactors.GrowthFactorsValueByIndex[LCol] := LValue;
                  LCol := LCol+1;
                end;
              end;
            end;
          end;
        end;
      finally
        LData.Free;
      end;
    end;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TGrowthFactorsValidator.PopulateMinMaxChannelGrowthFactorsObject(AFactors: TGrowthFactors): boolean;
const OPNAME = 'TGrowthFactorsValidator.PopulateMinMaxChannelGrowthFactorsObject';
begin
  Result := False;
  try
  
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TGrowthFactorsValidator.PopulateHydrologyGrowthFactorsObject(AFactors: TGrowthFactors): boolean;
const OPNAME = 'TGrowthFactorsValidator.PopulateHydrologyGrowthFactorsObject';
begin
  Result := False;
  try
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


function TGrowthFactorsValidator.PopulateYearsObject(AFactors: TGrowthFactors): boolean;
const OPNAME = 'TGrowthFactorsValidator.PopulateYearsObject';
var
  LStartYear,
 // LYearsCount,
  //LDataStartYear : integer;
  LRow,LCol   : integer;
  LRunConfig  : IRunConfigurationData;
begin
  Result := False;
  try
    if(GrowthFactorsDialog.strgrdDemandFactors.ColCount >= 2) then
    begin
      LRunConfig  := TYieldModelDataObject(FAppModules.Model.ModelData).RunConfigurationData;
      LStartYear  := LRunConfig.HistoricSequenceStartYear;
      case AFactors.GrowthType of
        gtDemand:
          begin
          //  LDataStartYear := StrToIntDef(GrowthFactorsDialog.strgrdDemandFactors.Cells[1,1],-1);
          end;
        gtMinMax: ;
        gtHydrology: ;
      end;

      if(LStartYear >= 0) then
      begin
        for LRow := 1 to GrowthFactorsDialog.strgrdDemandFactors.RowCount -1 do
        begin
          for LCol := 0 to GrowthFactorsDialog.strgrdDemandFactors.ColCount -1 do
          begin
            GrowthFactorsDialog.strgrdDemandFactors.Cells[LCol,LRow-1] := GrowthFactorsDialog.strgrdDemandFactors.Cells[LCol,LRow];
          end;
        end;
        GrowthFactorsDialog.strgrdDemandFactors.RowCount := GrowthFactorsDialog.strgrdDemandFactors.RowCount-1;
        Result := True;
      end;
       Result := True;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TGrowthFactorsValidator.OnPasteFromExcelClick(Sender: TObject);
const OPNAME = 'TGrowthFactorsValidator.OnPasteFromExcelClick';
var
  LGrowth  : TGrowthFactors;
  LNumOfYears : integer;
begin
  try
    LGrowth := TPlanningModelDataObject(FAppModules.Model.ModelData).CastGrowthFactors;
    if LGrowth <> nil then
    begin
      LNumOfYears := LGrowth.NumberOfYears;
      GrowthFactorsDialog.strgrdDemandFactors.ColCount  := LNumOfYears;
      GrowthFactorsDialog.strgrdHydrologyFactors.ColCount := LNumOfYears;
    end;

    case LGrowth.GrowthType of
      gtDemand : RemoveEmptyColumn(GrowthFactorsDialog.strgrdDemandFactors);
      gtHydrology: RemoveEmptyColumn(GrowthFactorsDialog.strgrdHydrologyFactors);
    end;
    if ValidateDataFromExcel then
    begin
      PopulateGrowthFactorsFromExcel;
      RepopulateDataViewer;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


function TGrowthFactorsValidator.ValidateDataFromExcel: boolean;
const OPNAME = 'TGrowthFactorsValidator.ValidateDataFromExcel';
var
  LGrowthFactors   :   TGrowthFactors;
begin
  Result := False;
  try
    LGrowthFactors := TPlanningModelDataObject(FAppModules.Model.ModelData).CastGrowthFactors;
    if MessageDlg('Does your channels from the excel match the one in the Framework?', mtConfirmation, [mbYes,mbNo], 0) = mrNo then Exit;
    case LGrowthFactors.GrowthType of
        gtDemand: Result := ValidateDemandCentreGrowthFactorsFromExcel;
        gtMinMax: Result := False; //ValidateMinMaxChannelGrowthFactorsFromExcel;
        gtHydrology: Result := False; //ValidateHydrologyGrowthFactorsFromExcel;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


function TGrowthFactorsValidator.ValidateDemandCentreGrowthFactorsFromExcel: boolean;
const OPNAME = 'TGrowthFactorsValidator.ValidateDemandCentreGrowthFactorsFromExcel';
var
  LRow,
  LCol: integer;
  LDouble: Double;
begin
  Result := False;
  try
    if(GrowthFactorsDialog.strgrdDemandFactors.ColCount >= 3)then
    begin
      for LRow := 2 to GrowthFactorsDialog.strgrdDemandFactors.RowCount-1 do
      begin
        for LCol := 1 to GrowthFactorsDialog.strgrdDemandFactors.ColCount-1 do
        begin
          if(Trim(GrowthFactorsDialog.strgrdDemandFactors.Cells[LCol,LRow]) <> '') then
          begin
            LDouble := StrToFloatDef(GrowthFactorsDialog.strgrdDemandFactors.Cells[LCol,LRow],NullFloat);
            if(LDouble = NullFloat) then
            begin
              GrowthFactorsDialog.strgrdDemandFactors.ValidationError[LCol, LRow,gveCellContext] :=
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

procedure TGrowthFactorsValidator.OnTabChanged(Sender: Tobject);
const OPNAME = 'TGrowthFactorsValidator.OnTabChanged';
var
  LGrowthFactors :TGrowthFactors;
begin
  try
    LGrowthFactors := TPlanningModelDataObject(FAppModules.Model.ModelData).CastGrowthFactors;

    if GrowthFactorsDialog.pgcProjections.ActivePage =
      GrowthFactorsDialog.tbsDemandFactors then
     LGrowthFactors.GrowthType := gtDemand;
    if GrowthFactorsDialog.pgcProjections.ActivePage =
      GrowthFactorsDialog.tbsMinMaxFactors then
      LGrowthFactors.GrowthType := gtMinMax;
    if GrowthFactorsDialog.pgcProjections.ActivePage =
      GrowthFactorsDialog.tbsHydrologyFactors then
      LGrowthFactors.GrowthType := gtHydrology;
    RepopulateDataViewer;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


procedure TGrowthFactorsValidator.RemoveEmptyColumn(AGrid: TFieldStringGrid );
const OPNAME = 'TGrowthFactorsValidator.RemoveEmptyColumn';
var
  LRow,LCol: integer;
begin
  try
    if(AGrid.ColCount >= 2)then
    begin
      for LRow := 1 to AGrid.RowCount-1 do
      begin
        for LCol := 3 to AGrid.ColCount-1 do
        begin
          AGrid.Cells[LCol,LRow] := AGrid.Cells[LCol,LRow];
        end;
      end;
      AGrid.ColCount := AGrid.ColCount-1;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

//===============================================================================================//

procedure TGrowthFactorsValidator.RepopulateDataViewer;
const OPNAME = 'TGrowthFactorsValidator.RepopulateDataViewer';
var
  LGrowthFactors: TGrowthFactors;
  LRunConfig : IRunConfigurationData;
  LStartYear : integer;
  LEndYear : integer;
begin
  try
    LGrowthFactors := TPlanningModelDataObject(FAppModules.Model.ModelData).CastGrowthFactors;
    LRunConfig := TYieldModelDataObject(FAppModules.Model.ModelData).RunConfigurationData;
    if (LRunConfig <> nil) and (LGrowthFactors <> nil) then
    begin
      LStartYear := LRunConfig.HistoricSequenceStartYear;
      LEndYear := LStartYear + LGrowthFactors.NumberOfYears-1;
      if  (LGrowthFactors.NumberOfYears >= 0)  then
      begin
      
        GrowthFactorsDialog.strgrdDemandFactors.RowCount := 2;
        GrowthFactorsDialog.strgrdMinMaxFactors.RowCount := 2;
        GrowthFactorsDialog.strgrdHydrologyFactors.RowCount := 2;

        GrowthFactorsDialog.edtNumYears.Text := IntToStr(LGrowthFactors.NumberOfYears);
        GrowthFactorsDialog.edtStartYear.Text := IntToStr(LStartYear);
        GrowthFactorsDialog.edtEndYear.Text := IntToStr(LEndYear);

        PopulateDemandCentreGrowthFactors(LGrowthFactors);
        PopulateMinMaxChannelGrowthFactors(LGrowthFactors);
        PopulateHydrologyGrowthFactors(LGrowthFactors);

       GrowthFactorsDialog.btnGenerateProjections.Enabled := LGrowthFactors.Populated;

      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TGrowthFactorsValidator.PopulateDemandCentreGrowthFactors(AGrowthFactors : TGrowthFactors);
const OPNAME = 'TGrowthFactorsValidator.PopulateDemandCentreGrowthFactors';
var
  LRow,LCol: integer;
  LIndex: integer;
  LDemandCentreGrowthFactors  : TDemandCentreGrowthFactors;
  LFactors                    : TStringList;
  LFieldProperty              : TAbstractFieldProperty;
begin
  try
    GrowthFactorsDialog.strgrdDemandFactors.Rows[1].Clear;
    if Assigned(AGrowthFactors) then
    begin
      if(GrowthFactorsDialog.strgrdDemandFactors.RowCount = 2) then
       LRow := 1
      else
       LRow := GrowthFactorsDialog.strgrdDemandFactors.RowCount;

      GrowthFactorsDialog.strgrdDemandFactors.ColCount := AGrowthFactors.NumberOfYears+1;
      GrowthFactorsDialog.strgrdDemandFactors.Cells[0,0] := FAppModules.language.GetString('TField.ChannelNumber');;
      GrowthFactorsDialog.strgrdDemandFactors.ClearFieldProperties;
      GrowthFactorsDialog.strgrdDemandFactors.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('ChannelNumber'));

      for LIndex := 0 to GrowthFactorsDialog.strgrdDemandFactors.ColCount-1 do
      begin
        GrowthFactorsDialog.strgrdDemandFactors.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('DemandGrowthFactors'));
        GrowthFactorsDialog.strgrdDemandFactors.Cells[LIndex+1,0] := IntToStr(StrToInt(GrowthFactorsDialog.edtStartYear.Text) + (LIndex));
      end;

      GrowthFactorsDialog.strgrdDemandFactors.RowCount := GrowthFactorsDialog.strgrdDemandFactors.RowCount +
                                                          AGrowthFactors.DemandCentresGrowthFactorsCount;
      LFactors := TStringList.Create;
      try
        for LIndex := 0 to AGrowthFactors.DemandCentresGrowthFactorsCount -1 do
        begin
          GrowthFactorsDialog.strgrdDemandFactors.Rows[LRow].Clear;
          LDemandCentreGrowthFactors := AGrowthFactors.CastDemandGrowthFactorByIndex(LIndex);
          GrowthFactorsDialog.strgrdDemandFactors.Cells[0,LRow] := IntToStr(LDemandCentreGrowthFactors.ChannelNumber);
          FChannelNumber := LDemandCentreGrowthFactors.ChannelNumber;
          FGrowthFactorsIndex := LRow;
          LFactors.CommaText :=  LDemandCentreGrowthFactors.GrowthFactors;
          LFieldProperty    := FAppModules.FieldProperties.FieldProperty('MonthlyAvrgFactor');
          for LCol := 0 to LFactors.Count-1 do
            GrowthFactorsDialog.strgrdDemandFactors.Cells[LCol+1,LRow] := Format(LFieldProperty.FormatStringGrid{'%2.6f'},[StrToFloat(LFactors.Strings[LCol])]);
          LRow := LRow + 1;
          //DoContextValidation(dvtDemandGrowthFactors);
          FGrowthFactorsIndex := 0;
        end;
      finally
        LFactors.Free;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TGrowthFactorsValidator.PopulateMinMaxChannelGrowthFactors(AGrowthFactors : TGrowthFactors);
const OPNAME = 'TGrowthFactorsValidator.PopulateMinMaxChannelGrowthFactors';
var
  LRow,LCol: integer;
  LIndex: integer;
  LMinMaxChannelGrowthFactors : TMinMaxChannelGrowthFactors;
  LFactors                    : TStringList;
  LFieldProperty              : TAbstractFieldProperty;
begin
  try
    GrowthFactorsDialog.strgrdMinMaxFactors.Rows[1].Clear;
    if Assigned(AGrowthFactors) then
    begin
      if (GrowthFactorsDialog.strgrdMinMaxFactors.RowCount = 2) then
        LRow := 1
      else
        LRow := GrowthFactorsDialog.strgrdMinMaxFactors.RowCount;
      GrowthFactorsDialog.strgrdMinMaxFactors.ColCount := AGrowthFactors.NumberOfYears+3;
      GrowthFactorsDialog.strgrdMinMaxFactors.Cells[0,0] := FAppModules.language.GetString('TField.MinMaxChannelNumber');
      GrowthFactorsDialog.strgrdMinMaxFactors.Cells[1,0] := FAppModules.language.GetString('GridHeading.ArcNumber');
      GrowthFactorsDialog.strgrdMinMaxFactors.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('ChannelNumber'));
      GrowthFactorsDialog.strgrdMinMaxFactors.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('ArcNumber'));

     for LIndex := 2 to GrowthFactorsDialog.strgrdMinMaxFactors.ColCount do
      begin
        GrowthFactorsDialog.strgrdMinMaxFactors.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('MinMaxGrowthFactors'));
        GrowthFactorsDialog.strgrdMinMaxFactors.Cells[LIndex,0] := IntToStr(StrToInt(GrowthFactorsDialog.edtStartYear.Text) + (LIndex-2));

      end;
      GrowthFactorsDialog.strgrdMinMaxFactors.RowCount := GrowthFactorsDialog.strgrdMinMaxFactors.RowCount +
                                                        AGrowthFactors.MinMaxChannelGrowthFactorsCount;
      LFactors := TStringList.Create;
      try
        for LIndex := 0 to AGrowthFactors.MinMaxChannelGrowthFactorsCount -1 do
        begin
          GrowthFactorsDialog.strgrdMinMaxFactors.Rows[LRow].Clear;
          LMinMaxChannelGrowthFactors := AGrowthFactors.CastMinMaxChannelGrowthFactorByIndex(LIndex);
          GrowthFactorsDialog.strgrdMinMaxFactors.Cells[0,LRow] := IntToStr(LMinMaxChannelGrowthFactors.MinMaxChannel);
          GrowthFactorsDialog.strgrdMinMaxFactors.Cells[1,LRow] := IntToStr(LMinMaxChannelGrowthFactors.ArcNumber);
          FMinMaxChannel := LMinMaxChannelGrowthFactors.MinMaxChannel;
          FGrowthFactorsIndex := LRow;
          LFieldProperty    := FAppModules.FieldProperties.FieldProperty('MonthlyAvrgFactor');
          LFactors.CommaText :=  LMinMaxChannelGrowthFactors.GrowthFactors;
          for LCol := 0 to LFactors.Count -1 do
            GrowthFactorsDialog.strgrdMinMaxFactors.Cells[LCol+2,LRow] := Format(LFieldProperty.FormatStringGrid {'%2.6f'},[StrToFloat(LFactors.Strings[LCol])]);
          LRow := LRow + 1;
          //DoContextValidation(dvtMinMaxGrowthFactors);
          FGrowthFactorsIndex := 0;
        end;
      finally
        LFactors.Free;
      end;
 //     RePopulateMinMaxChannelGrowthFactorsGrid(GrowthFactorsDialog.strgrdMinMaxFactors);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TGrowthFactorsValidator.PopulateHydrologyGrowthFactors(AGrowthFactors : TGrowthFactors);
const OPNAME = 'TGrowthFactorsValidator.PopulateHydrologyGrowthFactors';
var
  LRow,LCol: integer;
  LIndex: integer;
  LHydrologyGrowthFactors     : THydrologyGrowthFactors;
  LFactors                    : TStringList;
  LFieldProperty              : TAbstractFieldProperty;
begin
  try
    GrowthFactorsDialog.strgrdHydrologyFactors.Rows[1].Clear;
    LFieldProperty    := FAppModules.FieldProperties.FieldProperty('MonthlyAvrgFactor');
    if Assigned(AGrowthFactors) then
    begin
      if(GrowthFactorsDialog.strgrdHydrologyFactors.RowCount = 2) then
       LRow := 1
      else
       LRow := GrowthFactorsDialog.strgrdHydrologyFactors.RowCount;

      GrowthFactorsDialog.strgrdHydrologyFactors.ColCount := AGrowthFactors.NumberOfYears+3;
      GrowthFactorsDialog.strgrdHydrologyFactors.Cells[0,0] := FAppModules.language.GetString('GridHeading.GaugeNumber');
      GrowthFactorsDialog.strgrdHydrologyFactors.Cells[1,0] := FAppModules.language.GetString('GridHeading.FactorType');
      GrowthFactorsDialog.strgrdHydrologyFactors.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('ChannelNumber'));
      GrowthFactorsDialog.strgrdHydrologyFactors.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('ChannelNumber'));
      for LIndex := 2 to GrowthFactorsDialog.strgrdHydrologyFactors.ColCount do
      begin
        GrowthFactorsDialog.strgrdHydrologyFactors.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('AFFGrowthFactors'));
        GrowthFactorsDialog.strgrdHydrologyFactors.Cells[LIndex,0] := IntToStr(StrToInt(GrowthFactorsDialog.edtStartYear.Text) + (LIndex-2));
      end;
      GrowthFactorsDialog.strgrdHydrologyFactors.RowCount := GrowthFactorsDialog.strgrdHydrologyFactors.RowCount +
                                                             (3 * AGrowthFactors.HydrologyGrowthFactorsCount);
      LFactors := TStringList.Create;
      try
        for LIndex := 0 to AGrowthFactors.HydrologyGrowthFactorsCount -1 do
        begin
          LHydrologyGrowthFactors := AGrowthFactors.CastHydrologyGrowthFactorByIndex(LIndex);

          GrowthFactorsDialog.strgrdHydrologyFactors.Rows[LRow].Clear;
          GrowthFactorsDialog.strgrdHydrologyFactors.Cells[0,LRow] := IntToStr(LHydrologyGrowthFactors.GaugeNumber);
          GrowthFactorsDialog.strgrdHydrologyFactors.Cells[1,LRow] := 'AFF';
          LFactors.CommaText :=  LHydrologyGrowthFactors.AFFGrowthFactors;
          for LCol := 0 to LFactors.Count -1 do
          begin
            GrowthFactorsDialog.strgrdHydrologyFactors.Cells[LCol+2,LRow] := Format(LFieldProperty.FormatStringGrid {'%2.6f'},
                                                                                     [StrToFloatDef(LFactors.Strings[LCol],0.00)]);
          end;
          LRow := LRow + 1;
          
          GrowthFactorsDialog.strgrdHydrologyFactors.Rows[LRow].Clear;
          GrowthFactorsDialog.strgrdHydrologyFactors.Cells[0,LRow] := IntToStr(LHydrologyGrowthFactors.GaugeNumber);
          GrowthFactorsDialog.strgrdHydrologyFactors.Cells[1,LRow] := 'IRR';
          LFactors.CommaText :=  LHydrologyGrowthFactors.IRRGrowthFactors;
          for LCol := 0 to LFactors.Count -1 do
            GrowthFactorsDialog.strgrdHydrologyFactors.Cells[LCol+2,LRow] := Format(LFieldProperty.FormatStringGrid {'%2.6f'},
                                                                                     [StrToFloatDef(LFactors.Strings[LCol],0.00)]);
          LRow := LRow + 1;

          GrowthFactorsDialog.strgrdHydrologyFactors.Rows[LRow].Clear;
          GrowthFactorsDialog.strgrdHydrologyFactors.Cells[0,LRow] := IntToStr(LHydrologyGrowthFactors.GaugeNumber);
          GrowthFactorsDialog.strgrdHydrologyFactors.Cells[1,LRow] := 'URB';
          LFactors.CommaText :=  LHydrologyGrowthFactors.URBGrowthFactors;
         for LCol := 0 to LFactors.Count -1 do
            GrowthFactorsDialog.strgrdHydrologyFactors.Cells[LCol+2,LRow] := Format(LFieldProperty.FormatStringGrid {'%2.6f'},
                                                                                      [StrToFloatDef(LFactors.Strings[LCol],0.00)]);
          LRow := LRow + 1;
        end;
      finally
        LFactors.Free;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end; 

function TGrowthFactorsValidator.SaveState: boolean;
const OPNAME = 'TGrowthFactorsValidator.SaveState';
begin
  Result := inherited SaveState;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TGrowthFactorsValidator.GrowthFactorsDialog : TGrowthFactorsDialog;
const OPNAME = 'TGrowthFactorsValidator.GrowthFactorsDialog';
begin
  Result := nil;
  try
    if Assigned(FPanel) then
      Result := TGrowthFactorsDialog(FPanel);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TGrowthFactorsValidator.StudyDataHasChanged(AContext: TChangeContext; AFieldName,AOldValue,ANewValue: string): boolean;
const OPNAME = 'TGrowthFactorsValidator.StudyDataHasChanged';
begin
  Result := inherited StudyDataHasChanged(AContext,AFieldName,AOldValue,ANewValue);
  try
    if(AContext = sdccAdd) and (AFieldName = 'GrowthFactors') or (AFieldName = 'ExcelMinMaxGrowthFactors')
      or (AFieldName = 'ValidFactors') or (AFieldName = 'ExcelDemandGrowthFactors') then
      PopulateDataViewer;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TGrowthFactorsValidator.StudyHasChanged: boolean;
const OPNAME = 'TGrowthFactorsValidator.StudyHasChanged';
begin
  Result := inherited StudyHasChanged;
  try

  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TGrowthFactorsValidator.OnEditControltExit(Sender: TObject);
const OPNAME = 'TGrowthFactorsValidator.OnEditControltExit';
var
  LGrowthFactors : TGrowthFactors;
begin

  inherited OnEditControltExit(Sender);
  try
    LGrowthFactors := TPlanningModelDataObject(FAppModules.Model.ModelData).CastGrowthFactors;
    if LGrowthFactors <> nil then
    begin
    
      if (Sender = GrowthFactorsDialog.edtNumYears) and (GrowthFactorsDialog.edtNumYears.HasValueChanged) then
        UpdateNumYears(LGrowthFactors);

    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TGrowthFactorsValidator.OnViewDataHasChanged(Sender: TObject);
const OPNAME = 'TGrowthFactorsValidator.OnViewDataHasChanged';
begin
  try
    RepopulateDataViewer;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TGrowthFactorsValidator.OnStringGridCellDataHasChanged(ASender: TObject; ACol, ARow: integer);
const OPNAME = 'TGrowthFactorsValidator.OnStringGridCellDataHasChanged';
var
  LGrowthFactors : TGrowthFactors;
begin
  try
    if (ARow > 0) and (((ACol > 0) and (ACol < GrowthFactorsDialog.strgrdDemandFactors.ColCount - 1)) or
                       ((ACol > 1) and (ACol < GrowthFactorsDialog.strgrdMinMaxFactors.ColCount - 1)) or
                       ((ACol > 1) and (ACol < GrowthFactorsDialog.strgrdHydrologyFactors.ColCount - 1))) then
    begin
      LGrowthFactors := TPlanningModelDataObject(FAppModules.Model.ModelData).CastGrowthFactors;
      if (LGrowthFactors <> nil) then
      begin
        if ASender = GrowthFactorsDialog.strgrdDemandFactors then
        begin
          FChannelNumber := StrToInt(GrowthFactorsDialog.strgrdDemandFactors.Cells[0,ARow]);
          UpdateDemandCentreGrowthFactors(LGrowthFactors,FChannelNumber,ACol-1, ARow);
        end;
        if ASender = GrowthFactorsDialog.strgrdMinMaxFactors then
        begin
          FMinMaxChannel := StrToInt(GrowthFactorsDialog.strgrdMinMaxFactors.Cells[0,ARow]);
          if (ARow > 0) and (ACol > 1) then
             UpdateMinMaxChannelGrowthFactors(LGrowthFactors,FMinMaxChannel,ACol-2, ARow);
        end;
        if ASender = GrowthFactorsDialog.strgrdHydrologyFactors then
        begin
          FGaugeNumber := StrToInt(GrowthFactorsDialog.strgrdHydrologyFactors.Cells[0,ARow]);
          if GrowthFactorsDialog.strgrdHydrologyFactors.Cells[1,ARow] = 'AFF' then
            UpdateAFFGrowthFactors(LGrowthFactors,FGaugeNumber,ACol-2, ARow)
          else
          if GrowthFactorsDialog.strgrdHydrologyFactors.Cells[1,ARow] = 'IRR' then
            UpdateIRRGrowthFactors(LGrowthFactors,FGaugeNumber,ACol-2, ARow)
          else
          if GrowthFactorsDialog.strgrdHydrologyFactors.Cells[1,ARow] = 'URB' then
            UpdateURBGrowthFactors(LGrowthFactors,FGaugeNumber,ACol-2, ARow);

        end;
      end;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TGrowthFactorsValidator.UpdateDemandCentreGrowthFactors(AGrowthFactors: TGrowthFactors;AChannelNumber,
                                                                  ACol, ARow: integer);
const OPNAME = 'TGrowthFactorsValidator.UpdateDemandCentreGrowthFactors';
var
  LDemadGrowthFactors : IDemandCentreGrowthFactors;
  LMessage,
  LFieldValue : string;
  LValue : double;
begin
  try
    LDemadGrowthFactors := AGrowthFactors.DemandGrowthFactorsByChannel[AChannelNumber];
    if (LDemadGrowthFactors <> nil) then
    begin
      GrowthFactorsDialog.strgrdDemandFactors.ValidationError[ACol+1, ARow, gveCellContext] :='';
      LFieldValue := GrowthFactorsDialog.strgrdDemandFactors.Cells[ACol+1,ARow];
      if (FAppModules.FieldProperties.ValidateFieldProperty(
        'DemandGrowthFactors', LFieldValue, LMessage)) then
      begin
        if (Trim(LFieldValue) = '') then
          LFieldValue := '0';
        LValue := StrToFLoat(LFieldValue);
        LDemadGrowthFactors.GrowthFactorsValueByIndex[ACol] := LValue;
        RepopulateDataViewer;
        FGrowthFactorsIndex := ARow;
        //DoContextValidation(dvtDemandGrowthFactors);
        FGrowthFactorsIndex := 0;
      end
      else
        GrowthFactorsDialog.strgrdDemandFactors.ValidationError[ACol+1, ARow, gveCellContext] := LMessage;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TGrowthFactorsValidator.UpdateNumYears(AGrowthFactors : TGrowthFactors);
const OPNAME = 'TGrowthFactorsValidator.UpdateNumYears';
var
  LMessage,
  LFieldValue : string;
  LValue : integer;
begin
  try
    if (AGrowthFactors <> nil) then
    begin
      GrowthFactorsDialog.edtNumYears.FieldValidationError :='';
      LFieldValue := GrowthFactorsDialog.edtNumYears.Text;
      if (FAppModules.FieldProperties.ValidateFieldProperty(
        'GrowthFactorsYearCount', LFieldValue, LMessage)) then
      begin
        if (Trim(LFieldValue) = '') then
          LFieldValue := '0';
        LValue := StrToInt(LFieldValue);
        AGrowthFactors.NumberOfYears := LValue;
        RepopulateDataViewer;
        DoContextValidation(dvtNumberOfYears);
      end
      else
        GrowthFactorsDialog.edtNumYears.FieldValidationError := LMessage;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TGrowthFactorsValidator.DoContextValidation(AValidationType: TDialogValidationType);
const OPNAME = 'TGrowthFactorsValidator.DoContextValidation';
var
  LGrowthFactors : TGrowthFactors;
  LDemadGrowthFactors : IDemandCentreGrowthFactors;
  LMinMaxGrowthFactors : IMinMaxChannelGrowthFactors;
  LHydrologyGrowthFactors : IHydrologyGrowthFactors;
begin
  try
    LGrowthFactors := TPlanningModelDataObject(FAppModules.Model.ModelData).CastGrowthFactors;
    if (LGrowthFactors <> nil) then
    begin
      if (AValidationType = dvtDemandGrowthFactors) or (AValidationType = dvtAllGrowthFactors) then
      begin
        LDemadGrowthFactors := LGrowthFactors.DemandGrowthFactorsByChannel[FChannelNumber];
        if LDemadGrowthFactors <> nil then
        begin
          ValidateDemandGrowthFactors(LDemadGrowthFactors);
          ValidateChannelNumber(LDemadGrowthFactors);
        end;
      end;
      if (AValidationType =  dvtMinMaxGrowthFactors) or
         (AValidationType = dvtArcNumber) or
         (AValidationType = dvtAllGrowthFactors) then
      begin
        LMinMaxGrowthFactors := LGrowthFactors.MinMaxChannelGrowthFactorsByMinMaxChannel[FMinMaxChannel];
        if (LMinMaxGrowthFactors <> nil) then
        begin
          case AValidationType of
          dvtArcNumber : ValidateArcNumber(LMinMaxGrowthFactors);
          dvtMinMaxGrowthFactors,dvtAllGrowthFactors :
          begin
            ValidateMinMaxChannel(LMinMaxGrowthFactors);
            ValidateMinMaxGowthFactors(LMinMaxGrowthFactors);
          end;
          end;
        end;
      end;
      if (AValidationType = dvtNumberOfYears) or (AValidationType = dvtAllGrowthFactors) then
      begin
        ValidateNumberOfYears(LGrowthFactors);
      end;
      if (AValidationType = dvtAFFGrowthFactors) or
         (AValidationType = dvtIRRGrowthFactors) or
         (AValidationType = dvtURBGrowthFactors) or
         (AValidationType = dvtAllGrowthFactors) then
      begin
        LHydrologyGrowthFactors := LGrowthFactors.HydrologyGrowthFactorsByGaugeNumber[FGaugeNumber];
        if LHydrologyGrowthFactors <> nil then
        begin
          case AValidationType of
            dvtAFFGrowthFactors : ValidateAFFGrowthFactors(LHydrologyGrowthFactors);
            dvtIRRGrowthFactors : ValidateIRRGrowthFactors(LHydrologyGrowthFactors);
            dvtURBGrowthFactors : ValidateURBGrowthFactors(LHydrologyGrowthFactors);
          end;
        end;
      end;
    end
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TGrowthFactorsValidator.ValidateDemandGrowthFactors (ADemadGrowthFactors : IDemandCentreGrowthFactors);
const OPNAME = 'TGrowthFactorsValidator.ValidateDemandGrowthFactors';
var
  LCol           : integer;
  LIndex         : integer;
  LErrorMessages : TStringList;
  LErrorCols     : TStringList;
begin
  try
    if (ADemadGrowthFactors <> nil) then
    begin
      LErrorCols     := TStringList.Create;
      LErrorMessages := TStringList.Create;
      try
        FErrorMessage := '';
        lErrorCols.Clear;
        if (ADemadGrowthFactors.Validate(FErrorMessage,'DemandGrowthFactors')) then
        begin
          for LCol := 0 to ADemadGrowthFactors.GrowthFactorsCount -1 do
            GrowthFactorsDialog.strgrdDemandFactors.ValidationError[LCol+1, FGrowthFactorsIndex, gveColContext] := '';
        end
        else
        begin
          ExtractErrorsAndColumns(FErrorMessage, lErrorMessages, lErrorCols);
          for LCol := 0 to ADemadGrowthFactors.GrowthFactorsCount -1 do
          begin
            LIndex := LErrorCols.IndexOf(IntToStr(LCol));
            if (LIndex >= 0) then
               GrowthFactorsDialog.strgrdDemandFactors.ValidationError[LCol+1, FGrowthFactorsIndex, gveCellContext] := lErrorMessages.Strings[LIndex]
            else
               GrowthFactorsDialog.strgrdDemandFactors.ValidationError[LCol+1, FGrowthFactorsIndex, gveCellContext] := '';

          end;

          {for LCol := 3 to ADemadGrowthFactors.GrowthFactorsCount-1 do
          begin
            LIndex := lErrorCols.IndexOf(IntToStr(LCol));
            if (lIndex >= 0) then
               GrowthFactorsDialog.strgrdDemandFactors.ValidationError[LCol+1, FGrowthFactorsIndex, gveCellContext] := lErrorMessages.Strings[lIndex]
            else
               GrowthFactorsDialog.strgrdDemandFactors.ValidationError[LCol+1, FGrowthFactorsIndex, gveCellContext] := '';

          end;
          }
            FAllErrorMessages.AddStrings(lErrorMessages);
        end;
      finally
        FreeAndNil(lErrorCols);
        FreeAndNil(lErrorMessages);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TGrowthFactorsValidator.ValidateChannelNumber(ADemadGrowthFactors : IDemandCentreGrowthFactors);
const OPNAME = 'TGrowthFactorsValidator.ValidateChannelNumber';
begin
  try
    FErrorMessage := '';
    GrowthFactorsDialog.strgrdDemandFactors.ValidationError[0,FArcNumberIndex , gveColContext] := '';
    if (not ADemadGrowthFactors.Validate(FErrorMessage, 'ChannelNumber')) then
      GrowthFactorsDialog.strgrdDemandFactors.ValidationError[0,FArcNumberIndex, gveColContext] := FErrorMessage;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TGrowthFactorsValidator.UpdateAFFGrowthFactors(AGrowthFactors: TGrowthFactors;AGaugeNumber, ACol, ARow: integer);
const OPNAME = 'TGrowthFactorsValidator.UpdateAFFGrowthFactors';
var
  LAFFGrowthFactors : IHydrologyGrowthFactors;
  LMessage,
  LFieldValue : string;
  LValue : double;
begin
  try
    LAFFGrowthFactors := AGrowthFactors.HydrologyGrowthFactorsByGaugeNumber[AGaugeNumber];
    if (LAFFGrowthFactors <> nil) then
    begin
      GrowthFactorsDialog.strgrdHydrologyFactors.ValidationError[ACol+2, ARow, gveCellContext] :='';
      LFieldValue := GrowthFactorsDialog.strgrdHydrologyFactors.Cells[ACol+2,ARow];
      if (FAppModules.FieldProperties.ValidateFieldProperty(
        'AFFGrowthFactors', LFieldValue, LMessage)) then
      begin
        if (Trim(LFieldValue) = '') then
          LFieldValue := '0';
        LValue := StrToFLoat(LFieldValue);
        LAFFGrowthFactors.AFFGrowthFactorsValueByIndex[ACol] := LValue;
        FGrowthFactorsIndex := ARow;
        RepopulateDataViewer;
        DoContextValidation(dvtAFFGrowthFactors);
        FGrowthFactorsIndex := 0;
      end
      else
        GrowthFactorsDialog.strgrdHydrologyFactors.ValidationError[ACol+2, ARow, gveCellContext]  := LMessage;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TGrowthFactorsValidator.UpdateIRRGrowthFactors(AGrowthFactors : TGrowthFactors;AGaugeNumber,ACol, ARow:integer);
const OPNAME = 'TGrowthFactorsValidator.UpdateIRRGrowthFactors';
var
  LIRRGrowthFactors : IHydrologyGrowthFactors;
  LMessage,
  LFieldValue : string;
  LValue : double;
begin
  try
    LIRRGrowthFactors := AGrowthFactors.HydrologyGrowthFactorsByGaugeNumber[AGaugeNumber];
    if (LIRRGrowthFactors <> nil) then
    begin
      GrowthFactorsDialog.strgrdHydrologyFactors.ValidationError[ACol+2, ARow, gveCellContext] :='';
      LFieldValue := GrowthFactorsDialog.strgrdHydrologyFactors.Cells[ACol+2,ARow];
      if (FAppModules.FieldProperties.ValidateFieldProperty(
        'IRRGrowthFactors', LFieldValue, LMessage)) then
      begin
        if (Trim(LFieldValue) = '') then
          LFieldValue := '0';
        LValue := StrToFLoat(LFieldValue);
        LIRRGrowthFactors.IRRGrowthFactorsValueByIndex[ACol] := LValue;
        RepopulateDataViewer;
        FGrowthFactorsIndex := ARow;
        DoContextValidation(dvtIRRGrowthFactors);
        FGrowthFactorsIndex := 0;
      end
      else
        GrowthFactorsDialog.strgrdHydrologyFactors.ValidationError[ACol+2,ARow, gveCellContext] := LMessage;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TGrowthFactorsValidator.UpdateURBGrowthFactors(AGrowthFactors : TGrowthFactors;AGaugeNumber,ACol, ARow:integer);
const OPNAME = 'TGrowthFactorsValidator.UpdateURBGrowthFactors';
var
  LURBGrowthFactors : IHydrologyGrowthFactors;
  LMessage,
  LFieldValue : string;
  LValue : double;
begin
  try
    LURBGrowthFactors := AGrowthFactors.HydrologyGrowthFactorsByGaugeNumber[AGaugeNumber];
    if (LURBGrowthFactors <> nil) then
    begin
      GrowthFactorsDialog.strgrdHydrologyFactors.ValidationError[ACol+2,ARow, gveCellContext] :='';
      LFieldValue := GrowthFactorsDialog.strgrdHydrologyFactors.Cells[ACol+2,ARow];
      if (FAppModules.FieldProperties.ValidateFieldProperty(
        'URBGrowthFactors', LFieldValue, LMessage)) then
      begin
        if (Trim(LFieldValue) = '') then
          LFieldValue := '0';
        LValue := StrToFLoat(LFieldValue);
        LURBGrowthFactors.URBGrowthFactorsValueByIndex[ACol] := LValue;
        RepopulateDataViewer;
        FGrowthFactorsIndex := ARow;
        DoContextValidation(dvtURBGrowthFactors);
        FGrowthFactorsIndex := 0;
      end
      else
        GrowthFactorsDialog.strgrdHydrologyFactors.ValidationError[ACol+2,ARow, gveCellContext] := LMessage;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TGrowthFactorsValidator.UpdateMinMaxChannelGrowthFactors(AGrowthFactors: TGrowthFactors;AMinMaxChannel, ACol, ARow: integer);
const OPNAME = 'TGrowthFactorsValidator.UpdateMinMaxChannelGrowthFactors';
var
  LMinMaxGrowthFactors : IMinMaxChannelGrowthFactors;
  LMessage,
  LFieldValue : string;
  LValue : double;
begin
  try
    LMinMaxGrowthFactors := AGrowthFactors.MinMaxChannelGrowthFactorByIndex[ARow-1];
    if (LMinMaxGrowthFactors <> nil) then
    begin
      GrowthFactorsDialog.strgrdMinMaxFactors.ValidationError[ACol+2,ARow, gveCellContext] :='';
      LFieldValue := GrowthFactorsDialog.strgrdMinMaxFactors.Cells[ACol+2,ARow];
      if (FAppModules.FieldProperties.ValidateFieldProperty(
        'MinMaxGrowthFactors', LFieldValue, LMessage, ACol)) then
      begin
        if (Trim(LFieldValue) = '') then
          LFieldValue := '0';
        LValue := StrToFLoat(LFieldValue);

        LMinMaxGrowthFactors.GrowthFactorsValueByIndex[ACol] := LValue;
        RepopulateDataViewer;
        FGrowthFactorsIndex := ARow;
        DoContextValidation(dvtMinMaxGrowthFactors);
        FGrowthFactorsIndex := 0;
      end
      else
        GrowthFactorsDialog.strgrdMinMaxFactors.ValidationError[ACol+2,ARow, gveCellContext] := LMessage;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TGrowthFactorsValidator.ValidateNumberOfYears(AGrowthFactors: TGrowthFactors);
const OPNAME = 'TGrowthFactorsValidator.ValidateNumberOfYears';
begin
  try
    if (AGrowthFactors <> nil) then
    begin
      FErrorMessage := '';
      if (AGrowthFactors.Validate(FErrorMessage, 'GrowthFactorsYearCount')) then
        GrowthFactorsDialog.edtNumYears.ContextValidationError := ''
      else
        GrowthFactorsDialog.edtNumYears.ContextValidationError := FErrorMessage;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TGrowthFactorsValidator.ValidateArcNumber(AMinMaxGrowthFactors: IMinMaxChannelGrowthFactors);
const OPNAME = 'TGrowthFactorsValidator.ValidateArcNumber';
begin
  try
    FErrorMessage := '';
    GrowthFactorsDialog.strgrdMinMaxFactors.ValidationError[1,FArcNumberIndex, gveColContext] := '';
    if (not AMinMaxGrowthFactors.Validate(FErrorMessage, 'ArcNumber')) then
      GrowthFactorsDialog.strgrdMinMaxFactors.ValidationError[1,FArcNumberIndex, gveColContext] := FErrorMessage;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TGrowthFactorsValidator.ValidateMinMaxChannel(AMinMaxGrowthFactors: IMinMaxChannelGrowthFactors);
const OPNAME = 'TGrowthFactorsValidator.ValidateMinMaxChannel';
begin
  try
    FErrorMessage := '';
    GrowthFactorsDialog.strgrdMinMaxFactors.ValidationError[0,FArcNumberIndex , gveColContext] := '';
    if (not AMinMaxGrowthFactors.Validate(FErrorMessage, 'ChannelNumber')) then
      GrowthFactorsDialog.strgrdMinMaxFactors.ValidationError[0,FArcNumberIndex, gveColContext] := FErrorMessage;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TGrowthFactorsValidator.ValidateMinMaxGowthFactors(AMinMaxGrowthFactors: IMinMaxChannelGrowthFactors);
const OPNAME = 'TGrowthFactorsValidator.ValidateMinMaxGowthFactors';
var
  LCol           : integer;
  LIndex         : integer;
  LErrorMessages : TStringList;
  LErrorCols     : TStringList;
begin
  try
    if (AMinMaxGrowthFactors <> nil) then
    begin
      LErrorCols     := TStringList.Create;
      LErrorMessages := TStringList.Create;
      try
        FErrorMessage := '';
        lErrorCols.Clear;

        if (AMinMaxGrowthFactors.Validate(FErrorMessage,'MinMaxGrowthFactors')) then
        begin
          for LCol := 0 to AMinMaxGrowthFactors.GrowthFactorsCount -1 do
            GrowthFactorsDialog.strgrdMinMaxFactors.ValidationError[LCol +1, FGrowthFactorsIndex, gveColContext] := '';
        end
        else
        begin
          ExtractErrorsAndColumns(FErrorMessage, lErrorMessages, lErrorCols);
          for LCol := 0 to AMinMaxGrowthFactors.GrowthFactorsCount-1 do
          begin
            LIndex := lErrorCols.IndexOf(IntToStr(LCol));
            if (lIndex >= 0) then
               GrowthFactorsDialog.strgrdMinMaxFactors.ValidationError[LCol+1, FGrowthFactorsIndex, gveCellContext] := lErrorMessages.Strings[lIndex]
            else
               GrowthFactorsDialog.strgrdMinMaxFactors.ValidationError[LCol+1, FGrowthFactorsIndex, gveCellContext] := '';
          end;
            FAllErrorMessages.AddStrings(lErrorMessages);
        end;
      finally
        FreeAndNil(lErrorCols);
        FreeAndNil(lErrorMessages);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TGrowthFactorsValidator.UpdateArcNumber(AGrowthFactors: TGrowthFactors; AMinMaxChannel, ACol, ARow: integer);
const OPNAME = 'TGrowthFactorsValidator.UpdateArcNumber';
var
  LMinMaxGrowthFactors : IMinMaxChannelGrowthFactors;
  LMessage,
  LFieldValue : string;
  LValue : integer;
begin
  try
    LMinMaxGrowthFactors := AGrowthFactors.MinMaxChannelGrowthFactorsByMinMaxChannel[AMinMaxChannel];
    if (LMinMaxGrowthFactors <> nil) then
    begin
      GrowthFactorsDialog.strgrdMinMaxFactors.ValidationError[1, ACol, gveCellContext] :='';
      LFieldValue := GrowthFactorsDialog.strgrdMinMaxFactors.Cells[ACol,ARow];
      if (FAppModules.FieldProperties.ValidateFieldProperty(
        'ArcNumber', LFieldValue, LMessage)) then
      begin
        if (Trim(LFieldValue) = '') then
          LFieldValue := '0';
        LValue := StrToInt(LFieldValue);
        FArcNumberIndex :=  ARow;
        LMinMaxGrowthFactors.ArcNumber := LValue;
        RepopulateDataViewer;
        DoContextValidation(dvtArcNumber);
        FArcNumberIndex := 0;
      end
      else
        GrowthFactorsDialog.strgrdMinMaxFactors.ValidationError[1, ACol, gveCellContext] := LMessage;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TGrowthFactorsValidator.ValidateAFFGrowthFactors(AHydrologyGrowthFactors: IHydrologyGrowthFactors);
const OPNAME = 'TGrowthFactorsValidator.ValidateAFFGrowthFactors';
var
  LCol           : integer;
  LIndex         : integer;
  LErrorMessages : TStringList;
  LErrorCols     : TStringList;
begin
  try
    if (AHydrologyGrowthFactors <> nil) then
    begin
      LErrorCols     := TStringList.Create;
      LErrorMessages := TStringList.Create;
      try
        FErrorMessage := '';
        lErrorCols.Clear;
        if (AHydrologyGrowthFactors.Validate(FErrorMessage,'AFFGrowthFactors')) then
        begin
          for LCol := 0 to AHydrologyGrowthFactors.AFFGrowthFactorsCount -1 do
            GrowthFactorsDialog.strgrdHydrologyFactors.ValidationError[ LCol+1,FGrowthFactorsIndex, gveColContext] := '';
        end
        else
        begin
          ExtractErrorsAndColumns(FErrorMessage, lErrorMessages, lErrorCols);
          for LCol := 0 to AHydrologyGrowthFactors.AFFGrowthFactorsCount-1 do
          begin
            LIndex := lErrorCols.IndexOf(IntToStr(LCol));
            if (lIndex >= 0) then
               GrowthFactorsDialog.strgrdHydrologyFactors.ValidationError[ LCol+1,FGrowthFactorsIndex, gveColContext] := lErrorMessages.Strings[lIndex]
            else
               GrowthFactorsDialog.strgrdHydrologyFactors.ValidationError[ LCol+1,FGrowthFactorsIndex, gveColContext] := '';
          end;
            FAllErrorMessages.AddStrings(lErrorMessages);
        end;
      finally
        FreeAndNil(lErrorCols);
        FreeAndNil(lErrorMessages);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TGrowthFactorsValidator.ValidateIRRGrowthFactors(AHydrologyGrowthFactors: IHydrologyGrowthFactors);
const OPNAME = 'TGrowthFactorsValidator.ValidateIRRGrowthFactors';
var
  LCol           : integer;
  LIndex         : integer;
  LErrorMessages : TStringList;
  LErrorCols     : TStringList;
begin
  try
    if (AHydrologyGrowthFactors <> nil) then
    begin
      LErrorCols     := TStringList.Create;
      LErrorMessages := TStringList.Create;
      try
        FErrorMessage := '';
        lErrorCols.Clear;
        if (AHydrologyGrowthFactors.Validate(FErrorMessage,'IRRGrowthFactors')) then
        begin
          for LCol := 0 to AHydrologyGrowthFactors.IRRGrowthFactorsCount -1 do
            GrowthFactorsDialog.strgrdHydrologyFactors.ValidationError[LCol+1,FGrowthFactorsIndex, gveColContext] := '';
        end
        else
        begin
          ExtractErrorsAndColumns(FErrorMessage, lErrorMessages, lErrorCols);
          for LCol := 0 to AHydrologyGrowthFactors.IRRGrowthFactorsCount-1 do
          begin
            LIndex := lErrorCols.IndexOf(IntToStr(LCol));
            if (lIndex >= 0) then
               GrowthFactorsDialog.strgrdHydrologyFactors.ValidationError[LCol+1, FGrowthFactorsIndex, gveCellContext] := LErrorMessages.Strings[LIndex]
            else
               GrowthFactorsDialog.strgrdHydrologyFactors.ValidationError[LCol+1, FGrowthFactorsIndex, gveCellContext] := '';
          end;
            FAllErrorMessages.AddStrings(lErrorMessages);
        end;
      finally
        FreeAndNil(lErrorCols);
        FreeAndNil(lErrorMessages);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TGrowthFactorsValidator.ValidateURBGrowthFactors(AHydrologyGrowthFactors: IHydrologyGrowthFactors);
const OPNAME = 'TGrowthFactorsValidator.ValidateURBGrowthFactors';
var
  LCol           : integer;
  LIndex         : integer;
  LErrorMessages : TStringList;
  LErrorCols     : TStringList;
begin
  try
    if (AHydrologyGrowthFactors <> nil) then
    begin
      LErrorCols     := TStringList.Create;
      LErrorMessages := TStringList.Create;
      try
        FErrorMessage := '';
        lErrorCols.Clear;
        if (AHydrologyGrowthFactors.Validate(FErrorMessage,'IRRGrowthFactors')) then
        begin
          for LCol := 0 to AHydrologyGrowthFactors.IRRGrowthFactorsCount -1 do
            GrowthFactorsDialog.strgrdHydrologyFactors.ValidationError[LCol+1, FGrowthFactorsIndex, gveColContext] := '';
        end
        else
        begin
          ExtractErrorsAndColumns(FErrorMessage, lErrorMessages, lErrorCols);
          for LCol := 0 to AHydrologyGrowthFactors.IRRGrowthFactorsCount-1 do
          begin
            LIndex := lErrorCols.IndexOf(IntToStr(LCol));
            if (lIndex >= 0) then
               GrowthFactorsDialog.strgrdHydrologyFactors.ValidationError[LCol+1, FGrowthFactorsIndex, gveCellContext] := LErrorMessages.Strings[LIndex]
            else
               GrowthFactorsDialog.strgrdHydrologyFactors.ValidationError[LCol+1, FGrowthFactorsIndex, gveCellContext] := '';
          end;
            FAllErrorMessages.AddStrings(lErrorMessages);
        end;
      finally
        FreeAndNil(lErrorCols);
        FreeAndNil(lErrorMessages);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TGrowthFactorsValidator.OnGenerateProjectionsClick(Sender: TObject);
const OPNAME = 'TGrowthFactorsValidator.OnGenerateProjectionsClick';
{      MSG1   = 'There is no data to generate growth factors from.';
      MSG2   = 'There are no Demand Channel Growth Factors. Do you want to continue without them';
      MSG3   = 'There are no MinMax Channel Growth Factors. Do you want to continue without them';
      MSG4   = 'There are no Hydrology Growth Factors. Do you want to continue without them';
      MSG5   = 'There is already Growth Projections. Do you want to continue and overwrite them';}
var
  LGrowthFactors: TGrowthFactors;

begin
  try
    LGrowthFactors := TPlanningModelDataObject(FAppModules.Model.ModelData).CastGrowthFactors;
    if not LGrowthFactors.Populated then
    begin
      ShowMessage(FAppModules.Language.GetString('Message.GrowthFactorsValidatorMSG1'));
      Exit;
    end
    else
    begin
      if (MessageDlg(FAppModules.Language.GetString('Message.GrowthFactorsValidatorMSG5'),mtConfirmation,mbOKCancel,0) <> mrOk) then Exit;
      
      if(LGrowthFactors.DemandCentresGrowthFactorsCount = 0) then
        if (MessageDlg(FAppModules.Language.GetString('Message.GrowthFactorsValidatorMSG2'),mtConfirmation,mbOKCancel,0) <> mrOk) then Exit;

      if(LGrowthFactors.MinMaxChannelGrowthFactorsCount = 0) then
        if (MessageDlg(FAppModules.Language.GetString('Message.GrowthFactorsValidatorMSG3'),mtConfirmation,mbOKCancel,0) <> mrOk) then Exit;

      if(LGrowthFactors.HydrologyGrowthFactorsCount = 0) then
        if (MessageDlg(FAppModules.Language.GetString('Message.GrowthFactorsValidatorMSG4'),mtConfirmation,mbOKCancel,0) <> mrOk) then Exit;

    LGrowthFactors.GenerateGrowthProjections;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.

