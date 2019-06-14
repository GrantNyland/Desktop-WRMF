unit UPlanningMineOpenCastValidator;

interface
uses
  VCL.Grids,
  System.Types,
  UDataEditComponent,
  UAbstractObject,
  UPlanningMineOpenCastDialog,
  UPlanningOpenCast,
  UPlanningMineGrowthFactor,
  ULoadGeneration,
  UFileNames,
  UMiningOpenCastPitValidator;
type
  TPlanningMineOpenCastValidator = class(TMiningOpenCastPitValidator)
    protected
      procedure CreateMemberObjects; override;
      procedure DestroyMemberObjects; override;
      procedure OnEditControlEnter(Sender: TObject); override;
      procedure OnEditControltExit(Sender: TObject); override;
      procedure OnStringGridCellDataHasChanged (ASender: TObject; ACol, ARow: integer); override;
      procedure OnGridSelectCell(ASender: TObject; ACol, ARow: Integer; var CanSelect: Boolean);
      procedure OnGrowthGridSelectCell(ASender: TObject; ACol, ARow: Integer; var CanSelect: Boolean);
      procedure OnGrowthGridChanged(Sender: TObject);
      procedure getFileName(ASender: TObject; ACol, ARow: Longint); overload;
      procedure OnDrawAbstractionGridCell(Sender: TObject; ACol, ARow: Integer;
              Rect: TRect; State: TGridDrawState);

      procedure OnAbstractionCheckBoxChange(Sender: TObject);
      procedure OnInsertOpenCast(Sender: TObject);
      procedure OnDeleteOpenCast(Sender: TObject);
      procedure refreshGrowthControl(Sender: TObject);
      procedure refreshLoadGenerationControl(Sender: TObject);
      procedure RePopulateDataViewer;
      procedure PopulateGrowthFactorControl(AOpenCast: TPlanningOpenCast; AType: integer = 2);
      procedure PopulateLoadGenerationControl(AOpenCast: TPlanningOpenCast; AType: integer= 1);
      procedure PopulateAbstractionControl(AOpenCast: TPlanningOpenCast);
      procedure PopulateYearAndFactorGrid(AGrowthFactor : TPlanningMineGrowthFactor);
      procedure ClearGrowthFactorGrid;
      procedure ClearFactorAndMeanGrid;
      procedure ClearAbstractionGrid;


      procedure UpdateAbstraction;
      procedure ValidateAbstraction(AOpenCast: TPlanningOpenCast);
      procedure UpdateAbstractionToEvap;
      procedure ValidateAbstractionToEvap(AOpenCast: TPlanningOpenCast);
      procedure UpdateAbstractionToRiver;
      procedure ValidateAbstractionToRiver(AOpenCast: TPlanningOpenCast);
      procedure UpdateAbstractionToCPD;
      procedure ValidateAbstractionToCPD(AOpenCast: TPlanningOpenCast);
      procedure UpdateAbstractMonthTimeSeriesFile;
      procedure ValidateAbstractMonthTimeSeriesFile(AOpenCast: TPlanningOpenCast);

      procedure AddGrowthFactor(ASender: TObject);
      procedure DeleteGrowthFactor(ASender: TObject);
      procedure AddLoadGeneration(ASender: TObject);
      procedure DeleteLoadGeneration(ASender: TObject);

      procedure UpdatePCDIniConcentration;
      procedure ValidatePCDIniConcentration(AOpenCast: TPlanningOpenCast);
      procedure UpdateWorkingCommYear;
      procedure ValidateWorkingCommYear(AOpenCast: TPlanningOpenCast);
      procedure UpdateWorkingCommMonth;
      procedure ValidateWorkingCommMonth(AOpenCast: TPlanningOpenCast);
      procedure UpdateWorkingDecommYear;
      procedure ValidateWorkingDecommYear(AOpenCast: TPlanningOpenCast);
      procedure UpdateWorkingDecommMonth;
      procedure ValidateWorkingDecommMonth(AOpenCast: TPlanningOpenCast);
      procedure UpdateRunoffSaltWashOffEfficiencyFactor;
      procedure ValidateRunoffSaltWashOffEfficiencyFactor(AOpenCast: TPlanningOpenCast);
      procedure UpdateOpenCastIniSaltStore;
      procedure ValidateOpenCastIniSaltStore(AOpenCast: TPlanningOpenCast);
      procedure UpdateRechargeRate;
      procedure ValidateRechargeRate(AOpenCast: TPlanningOpenCast);

      procedure UpdateIntepolationMethod;
      procedure UpdateYearDataPoints;
      procedure UpDateNoOfYears(ACol, ARow: integer);
      procedure UpDateGrowthFactor(ACol, ARow: integer);

      procedure ValidateNoOfPoints(AGrowthFactor : TPlanningMineGrowthFactor);
      procedure ValidateInterpolationMedthod(AGrowthFactor : TPlanningMineGrowthFactor);
      procedure ValidateNoOfYears(AGrowthFactor : TPlanningMineGrowthFactor);
      procedure ValidateGrowthFactor(AGrowthFactor : TPlanningMineGrowthFactor);

      procedure UpdateStandardDeviation;
      procedure UpDateFlow;
      procedure UpDateMeanOfSalt;
      procedure ValidateStandardDeviation(ALoadGeneration : TLoadGeneration);
      procedure ValidateFlow(ALoadGeneration : TLoadGeneration);
      procedure ValidateMeanOfSalt(ALoadGeneration : TLoadGeneration);

    public
      procedure ClearDataViewer; override;
      procedure PopulateDataViewer; override;

      function PMiningOpenCastPitDialog: TPlanningMineOpenCastDialog;
      function Initialise: Boolean; override;
      procedure DoContextValidation(AValidationType: Integer); override;

  end;

implementation
uses
  Windows, Messages, SysUtils, Classes, StrUtils, VCL.Graphics, VCL.Controls, VCL.Forms,
  VCL.Dialogs,
  VCL.ComCtrls, VCL.StdCtrls,
  UPlanningMineData,
  UYieldContextValidationType,
  UPlanningModelDataObject,
  UPlanningMineSQLAgent,
  UUtilities,
  UPlanningModelManager,
  System.Math,
  VoaimsCom_TLB,
  UErrorHandlingOperations;

procedure TPlanningMineOpenCastValidator.ClearFactorAndMeanGrid;
const OPNAME = 'TPlanningMineOpenCastValidator.ClearFactorAndMeanGrid';
var
  LRows, LCols : integer;
   LFieldProperty      : TAbstractFieldProperty;
begin
  try
  with PMiningOpenCastPitDialog do
  begin
      FactorAndMeanGrid.ClearErrors;
      FactorAndMeanGrid.ColCount := 2;
      FactorAndMeanGrid.WrapHeaderText := True;
      FactorAndMeanGrid.FixedRows := 1;
      FactorAndMeanGrid.RowHeights[0] := FactorAndMeanGrid.DefaultRowHeight * 2;
      LFieldProperty := FAppModules.FieldProperties.FieldProperty('Flow');
      for LRows := 1 to LFieldProperty.ArrayLength do
        for LCols := 0 to 1 do
          FactorAndMeanGrid.Cells[LCols,LRows] := '';


  end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TPlanningMineOpenCastValidator.ClearGrowthFactorGrid;
const OPNAME = 'TPlanningMineOpenCastValidator.ClearGrowthFactorGrid';
var
  LCol, LRow: integer;
begin
  try
  with PMiningOpenCastPitDialog do
  begin
      //GrowthFactorGrid.ClearFieldProperties;
      //GrowthFactorGrid.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('InterpolationMethod'));
      GrowthFactorGrid.ColCount := 3;
      GrowthFactorGrid.WrapHeaderText := True;
      GrowthFactorGrid.Cells[0,0] := 'Factor Type';
      GrowthFactorGrid.Cells[1,0] := FAppModules.Language.GetString('TPlanningMineGrowthFactorControl.InterpolationMethod');
      GrowthFactorGrid.Cells[2,0] := FAppModules.Language.GetString('TPlanningMineGrowthFactorControl.NoOfPoints');

      GrowthFactorGrid.FixedCols := 1;
      GrowthFactorGrid.FixedRows := 1;
      GrowthFactorGrid.ColWidths[0] := GrowthFactorGrid.DefaultColWidth*4;
      for LRow := 0 to YearAndFactorGrid.RowCount -1 do
      begin
        for LCol := 1 to YearAndFactorGrid.ColCount -1 do
          YearAndFactorGrid.Cells[LCol,LRow] := '';
      end;

  end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TPlanningMineOpenCastValidator.CreateMemberObjects;
const OPNAME = 'TPlanningMineOpenCastValidator.CreateMemberObjects';
begin
  inherited;
  try
    FIdentifier := -1;
    //FreeAndNil(FPanel);
    FPanel := TPlanningMineOpenCastDialog.Create(FPanelOwner,FAppModules);

    with PMiningOpenCastPitDialog do
    begin
      NumberOfOpenCastPitsEdt.FieldProperty    := FAppModules.FieldProperties.FieldProperty('NrOfMineCastPits');
      NumberOfOpenCastPitsEdt.OnEnter          := OnEditControlEnter;
      NumberOfOpenCastPitsEdt.OnExit           := OnEditControltExit;


      OpenCastPitsGrid.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('NrOfMineCastPits'));
      OpenCastPitsGrid.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('PitName'));
      OpenCastPitsGrid.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('CoalReserveArea'));
      OpenCastPitsGrid.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('WorkingsArea'));
      OpenCastPitsGrid.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('DisturbedWorkingsArea'));
      OpenCastPitsGrid.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('DisturbedArea'));
      OpenCastPitsGrid.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('WaterSurfaceEvapArea'));
      OpenCastPitsGrid.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('DisturbedAreaRunOff'));
      OpenCastPitsGrid.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('DisturbedWorkingsAreaRunOff'));
      OpenCastPitsGrid.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('DecantVolume'));
      OpenCastPitsGrid.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('SeepageVolume'));
      OpenCastPitsGrid.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('AnalysisStartVolume'));
      OpenCastPitsGrid.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('MaximumSeepageRate'));
      OpenCastPitsGrid.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('SeepageExponent'));
      OpenCastPitsGrid.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('OpenCastPCDSurfaceArea'));
      OpenCastPitsGrid.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('OpenCastPCDStorageCapacity'));
      OpenCastPitsGrid.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('OpenCastPCDAnalysisStartVolume'));
      //Added just to enable the last 3 colums
      OpenCastPitsGrid.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('PCDIniConcentration'));
      OpenCastPitsGrid.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('PCDIniConcentration'));
      //
      OpenCastPitsGrid.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('PCDIniConcentration'));
      OpenCastPitsGrid.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('WorkingCommYear'));
      OpenCastPitsGrid.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('WorkingCommMonth'));
      OpenCastPitsGrid.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('WorkingDecommYear'));
      OpenCastPitsGrid.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('WorkingDecommMonth'));
      OpenCastPitsGrid.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('RunoffSaltWashOffEfficiencyFactor'));
      OpenCastPitsGrid.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('OpenCastIniSaltStore'));
      OpenCastPitsGrid.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('RechargeRate'));

      GrowthFactorGrid.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('InterpolationMethod'));
      GrowthFactorGrid.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('InterpolationMethod'));
      GrowthFactorGrid.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('NoOfPoints'));

      InterpolationCmBox.FieldProperty := FAppModules.FieldProperties.FieldProperty('InterpolationMethod');
      InterpolationCmBox.OnEnter       := OnEditControlEnter;
      InterpolationCmBox.OnChange      := OnEditControltExit;


      OpenCastPitsGrid.OnBeforeCellChange   := OnStringGridCellDataHasChanged;
      OpenCastPitsGrid.OnSelectCell         := OnGridSelectCell;
      OpenCastPitsGrid.OnColEnter           := OnStringGridColEnter;
      OpenCastPitsGrid.OnEnter              := OnEditControlEnter;

      InsertOpenCastBtn.OnClick                := OnInsertOpenCast;
      DeleteOpenCastBtn.OnClick                := OnDeleteOpenCast;

      OpenCastPitsGrid.ButtonColumn[17] := True;
      OpenCastPitsGrid.ButtonColumnCaption[17] := FAppModules.Language.GetString('LabelText.ThreeDots');
      OpenCastPitsGrid.ButtonColumnOnClick[17] := OnMonthlyRechargeFactorsClick;

      OpenCastPitsGrid.ButtonColumn[18] := True;
      OpenCastPitsGrid.ButtonColumnCaption[18] := FAppModules.Language.GetString('LabelText.ThreeDots');
      OpenCastPitsGrid.ButtonColumnOnClick[18] := OnMonthlyRechargeFactorsClick;



      GrowthFactorGrid.OnSelectCell        := OnGrowthGridSelectCell;
      GrowthFactorGrid.OnTopLeftChanged    := OnGrowthGridChanged;
      GrowthFactorGrid.OnBeforeCellChange  := OnStringGridCellDataHasChanged;
      GrowthFactorGrid.OnEnter             := OnEditControlEnter;
      GrowthFactorGrid.OnColEnter          := OnStringGridColEnter;




   //   GrowthFactorGrid.OnBeforeCellChange   := OnStringGridCellDataHasChanged;
     // GrowthFactorGrid.OnSelectCell         := OnGridSelectCell;
    //  GrowthFactorGrid.OnColEnter           := OnStringGridColEnter;
   //

      InterpolationCmBox.FieldProperty := FAppModules.FieldProperties.FieldProperty('InterpolationMethod');

      YearAndFactorGrid.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('NYR'));
      //YearAndFactorGrid.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('GrowthFactors'));

      YearAndFactorGrid.OnBeforeCellChange := OnStringGridCellDataHasChanged;
      YearAndFactorGrid.onEnter := OnEditControlEnter;
      YearAndFactorGrid.OnColEnter := OnStringGridColEnter;

      //FactorTypeCombo.OnSelect := refreshGrowthControl;
      StdDeviationEdit.FieldProperty := FAppModules.FieldProperties.FieldProperty('StdDeviation');
      StdDeviationEdit.OnEnter := OnEditControlEnter;
      StdDeviationEdit.OnExit := OnEditControltExit;
      FactorAndMeanGrid.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('Flow'));
      FactorAndMeanGrid.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('MeanOfSalt'));
      FactorAndMeanGrid.OnBeforeCellChange := OnStringGridCellDataHasChanged;
      FactorAndMeanGrid.OnColEnter :=  OnStringGridColEnter;
      LoadGenerationCombo.OnSelect := refreshLoadGenerationControl;

      AbstractionGrid.OnBeforeCellChange   := OnStringGridCellDataHasChanged;
      //AbstractionGrid.OnSelectCell         := OnGridSelectCell;
      AbstractionGrid.OnColEnter           := getFileName;
      AbstractionGrid.OnDrawCell           := OnDrawAbstractionGridCell;
      AbstractionGrid.OnEnter              := OnEditControlEnter;

      AbstractionCheckBox.FieldProperty := FAppModules.FieldProperties.FieldProperty('AbstractionIndicator');
      AbstractionCheckBox.OnClick :=     OnAbstractionCheckBoxChange;
      AbstractionGrid.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('AbstractToEvap'));
      AbstractionGrid.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('AbstractToRiver'));
      AbstractionGrid.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('AbstractToCPD'));
      AbstractionGrid.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('AbstractTotal'));
      AbstractionGrid.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('AbstractMonthTimeSeriesFile'));

      BtnAddGrowthFactors.OnClick := AddGrowthFactor;
      BtnDeleteGrowthFactors.OnClick := DeleteGrowthFactor;
      BtnAddLoadGeneration.OnClick := AddLoadGeneration;
      BtnDeleteLoadGeneration.OnClick := DeleteLoadGeneration;

    end;
  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TPlanningMineOpenCastValidator.DeleteGrowthFactor(ASender: TObject);
const OPNAME = 'TPlanningMineOpenCastValidator.DeleteGrowthFactor';
var
  LMine : TPlanningMine;
  LOpenCast : TPlanningOpenCast;
begin
  try
    with PMiningOpenCastPitDialog do
    begin
      if (ASender = BtnDeleteGrowthFactors) then
      begin
        LMine := TPlanningMine(TPlanningModelDataObject(FAppModules.Model.ModelData).CastNetworkFeaturesData.CastMineList.CastMineByNodeNumber[FIdentifier]);
        if LMine <> nil then
        begin
          if FSelectOpenCastID <> -1  then
          begin
            LOpenCast:=  LMine.CastOpenCastByID[FSelectOpenCastID];
            if LOpenCast <> nil then
            begin
              LOpenCast.RemoveGrowthFactorByIndex(GrowthFactorGrid.Row - 1);
              PopulateGrowthFactorControl(LOpenCast);
            end;
          end;
        end;
      end;
    end;

  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TPlanningMineOpenCastValidator.DeleteLoadGeneration(ASender: TObject);
const OPNAME = 'TPlanningMineOpenCastValidator.DeleteLoadGeneration';
var
  LMine : TPlanningMine;
  LOpenCast : TPlanningOpenCast;
begin
  try
    with PMiningOpenCastPitDialog do
    begin
      if (ASender = BtnDeleteLoadGeneration) then
      begin
        LMine := TPlanningMine(TPlanningModelDataObject(FAppModules.Model.ModelData).CastNetworkFeaturesData.CastMineList.CastMineByNodeNumber[FIdentifier]);
        if LMine <> nil then
        begin
          if FSelectOpenCastID <> -1  then
          begin
            LOpenCast:=  LMine.CastOpenCastByID[FSelectOpenCastID];
            if LOpenCast <> nil then
            begin
              LOpenCast.RemoveLoadGenerationByType(Integer(LoadGenerationCombo.Items.Objects[LoadGenerationCombo.ItemIndex]));
              PopulateLoadGenerationControl(LOpenCast);
            end;
          end;

        end;
      end;
    end;

  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TPlanningMineOpenCastValidator.DestroyMemberObjects;
const OPNAME = 'TPlanningMineOpenCastValidator.DestroyMemberObjects';
begin
  try
     inherited;
  except on E: Exception do HandleError(e,opname); end;
end;


procedure TPlanningMineOpenCastValidator.DoContextValidation(AValidationType: Integer);
const OPNAME = 'TPlanningMineOpenCastValidator.DoContextValidation';
var
  LMine : TPlanningMine;
  LOpenCast : TPlanningOpenCast;
  LGrowth : TPlanningMineGrowthFactor;
  LLoadGeneration : TLoadGeneration;
begin
  try
    LLoadGeneration := nil;
    inherited;
    with PMiningOpenCastPitDialog do
    begin
      LMine :=  TPlanningMine(TPlanningModelDataObject(FAppModules.Model.ModelData).CastNetworkFeaturesData.CastMineList.CastMinebyNodeNumber[FIdentifier]);
      if LMine <> nil then
      begin
        LOpenCast := LMine.CastOpenCastByID[FSelectOpenCastID];
        if LOpenCast <> nil then
        begin
          if LOpenCast.GrowthFactorCount < GrowthFactorGrid.Row then
            LGrowth := LOpenCast.GrowthFactorByIndex(LOpenCast.GrowthFactorCount)
          else
            LGrowth := LOpenCast.GrowthFactorByIndex(GrowthFactorGrid.Row-1);
          if LoadGenerationCombo.Items.Count > 0 then
              LLoadGeneration := LOpenCast.LoadGenerationByType(Integer(LoadGenerationCombo.Items.Objects[LoadGenerationCombo.ItemIndex]));
          if ((LGrowth <> nil) or (LLoadGeneration <> nil)) then
          begin

            case AValidationType of
              dvtNoOfPoints               : ValidateNoOfPoints(LGrowth);
              dvtInterpolationMethod      : ValidateInterpolationMedthod(LGrowth);
              dvtNYR                      : ValidateNoOfYears(LGrowth);
              dvtGrowthFactor             : ValidateGrowthFactor(LGrowth);
              dvtAbstraction              : ValidateAbstraction(LOpenCast);
              dvtAbstractToEvap           : ValidateAbstractionToEvap(LOpenCast);
              dvtAbstractToCPD            : ValidateAbstractionToCPD(LOpenCast);
              dvtAbstractToRiver          : ValidateAbstractionToRiver(LOpenCast);
              dvtAbstractionTimeSeries    : ValidateAbstractMonthTimeSeriesFile(LOpenCast);

              dvtCommYear                 : ValidateWorkingCommYear(LOpenCast);
              dvtCommMonth                : ValidateWorkingCommMonth(LOpenCast);
              dvtDecommYear               : ValidateWorkingDecommYear(LOpenCast);
              dvtDecommMonth              : ValidateWorkingDecommMonth(LOpenCast);
              dvtRunOffSaltWashEfficieny  : ValidateRunoffSaltWashOffEfficiencyFactor(LOpenCast);
              dvtIniSaltStore             : ValidateOpenCastIniSaltStore(LOpenCast);
              dvtReChargeRate             : ValidateRechargeRate(LOpenCast);
              dvtPCDIniConcentration      : ValidatePCDIniConcentration(LOpenCast);

              dvtStandardDeviation        : ValidateStandardDeviation(LLoadGeneration);
              dvtFlow                     : ValidateFlow(LLoadGeneration);
              dvtMeanOfSalt               : ValidateMeanOfSalt(LLoadGeneration);
              dvtResPropAll:
              begin
                ValidateAbstraction(LOpenCast);
                ValidateAbstractionToEvap(LOpenCast);
                ValidateAbstractionToCPD(LOpenCast);
                ValidateAbstractionToRiver(LOpenCast);
                ValidateAbstractMonthTimeSeriesFile(LOpenCast);
                ValidateAbstraction(LOpenCast);
                ValidatePCDIniConcentration(LOpenCast);
                ValidateWorkingCommYear(LOpenCast);
                ValidateWorkingCommMonth(LOpenCast);
                ValidateWorkingDecommYear(LOpenCast);
                ValidateWorkingDecommMonth(LOpenCast);
                ValidateRunoffSaltWashOffEfficiencyFactor(LOpenCast);
                ValidateOpenCastIniSaltStore(LOpenCast);
                ValidateRechargeRate(LOpenCast);
                ValidateNoOfPoints(LGrowth);
                ValidateInterpolationMedthod(LGrowth);
                ValidateNoOfYears(LGrowth);
                ValidateGrowthFactor(LGrowth);
              end;
            end;
          end;
        end;
      end;
    end;
  except on E: Exception do HandleError(e,opname); end;
end;

procedure TPlanningMineOpenCastValidator.getFileName(ASender: TObject; ACol, ARow: Longint);
const OPNAME = 'TPlanningMineOpenCastValidator.getFileName';
var
  LOpenDialog : TOpenDialog;    // Open dialog variable
  LFileNamesObject: TModelFileNames;
  LPath : string;
  LFileName : string;
begin
  OnStringGridColEnter(ASender,ACol,ARow);
  try
    LOpenDialog := nil;
    with PMiningOpenCastPitDialog do
    begin
      try
        if ASender = AbstractionGrid then
          if (ACol = 4) and (ARow =  1) then
          begin
            LOpenDialog := TOpenDialog.Create(nil);
            LFileNamesObject :=  TPlanningModelDataObject(FAppModules.Model.ModelData).CastFileNamesObject;
            if LFileNamesObject <> nil then
              LPath := LFileNamesObject.InputFilesPath + CWQT;
            LOpenDialog.InitialDir := LPath;
            LOpenDialog.Options := [ofFileMustExist];
            LOpenDialog.Filter := 'Time Series File|*.q';
            LOpenDialog.FilterIndex := 1;
            if LOpenDialog.Execute then
            begin
              LFileName := StringReplace(LOpenDialog.FileName,LPath + '\','',[rfReplaceAll,rfIgnoreCase]);
              AbstractionGrid.Col := ACol;
              AbstractionGrid.Row := ARow;
              AbstractionGrid.Cells[ACol,ARow] := LFileName;
              OnStringGridCellDataHasChanged(AbstractionGrid,ACol,ARow);
              AbstractionGrid.SetFocus;
            end;
         end;
      finally
        if (Assigned(LOpenDialog)) then
          LOpenDialog.Free;
      end;
    end;
  except on E: Exception do HandleError(E,OPNAME) end;
end;

function TPlanningMineOpenCastValidator.Initialise: Boolean;
const OPNAME = 'TChannelGrowthValidator.OnEditControlEnter';
begin
  Result := False;
  try
    with PMiningOpenCastPitDialog do
    begin
      InterpolationCmBox.Clear;
      InterpolationCmBox.Items.AddObject('1 - Linear Interpolation', TObject(1));
      InterpolationCmBox.Items.AddObject('2 - Exponential Interpolation', TObject(2));

      YearAndFactorGrid.FixedRows := 0;
      YearAndFactorGrid.FixedCols := 1;
      YearAndFactorGrid.ColCount := 2;
      YearAndFactorGrid.RowCount := 2;

      YearAndFactorGrid.ColWidths[0] := YearAndFactorGrid.DefaultColWidth*5;
      YearAndFactorGrid.Cells[0,0] := FAppModules.Language.GetString('FYearAndFactorGrid.YearCol');
      //YearAndFactorGrid.Cells[0,1] := FAppModules.Language.GetString('FYearAndFactorGrid.GrowthFactorMine');
    end;
    Result := FPanel.Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TPlanningMineOpenCastValidator.OnEditControlEnter(Sender: TObject);
const OPNAME = 'TChannelGrowthValidator.OnEditControlEnter';
begin
  inherited;
  try
  except on E: Exception do HandleError(E,OPNAME) end;
end;

procedure TPlanningMineOpenCastValidator.OnEditControltExit(Sender: TObject);
const OPNAME = 'TChannelGrowthValidator.OnEditControltExit';
begin
  inherited OnEditControltExit(Sender);
  try
    with PMiningOpenCastPitDialog do
    begin
      if Sender = InterpolationCmBox then
        UpdateIntepolationMethod
      else if Sender = StdDeviationEdit then
        UpdateStandardDeviation
    end;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TPlanningMineOpenCastValidator.OnStringGridCellDataHasChanged;
const OPNAME = 'TChannelGrowthValidator.OnStringGridCellDataHasChanged';
begin
  inherited OnStringGridCellDataHasChanged(ASender, ACol, ARow);
  try
    with PMiningOpenCastPitDialog do
    begin
      if (ASender = OpenCastPitsGrid) then
      begin
        case OpenCastPitsGrid.Col of
          19: UpdatePCDIniConcentration;
          20: UpdateWorkingCommYear;
          21: UpdateWorkingCommMonth;
          22: UpdateWorkingDecommYear;
          23: UpdateWorkingDecommMonth;
          24: UpdateRunoffSaltWashOffEfficiencyFactor;
          25: UpdateOpenCastIniSaltStore;
          26: UpdateRechargeRate;
        end;
      end
      else if ((ASender = GrowthFactorGrid) and (GrowthFactorGrid.Col = 2)) then UpdateYearDataPoints
      else if ((ASender = YearAndFactorGrid) and (YearAndFactorGrid.Col > 0) and (YearAndFactorGrid.Row = 0)) then UpDateNoOfYears(ACol,ARow)
      else if ((ASender = YearAndFactorGrid) and (YearAndFactorGrid.Col > 0) and (YearAndFactorGrid.Row = 1)) then UpDateGrowthFactor(ACol,ARow)
      else if (ASender = AbstractionGrid) then
            begin
              case AbstractionGrid.Col of
                0: UpdateAbstractionToEvap;
                1: UpdateAbstractionToRiver;
                2: UpdateAbstractionToCPD;
                4: UpdateAbstractMonthTimeSeriesFile;

              end;
            end
      else if (ASender = FactorAndMeanGrid) then
           begin
             case FactorAndMeanGrid.Col of
              0: UpDateFlow;
              1: UpDateMeanOfSalt;
             end;
           end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TPlanningMineOpenCastValidator.OnAbstractionCheckBoxChange(
  Sender: TObject);
const OPNAME = 'TPlanningMineOpenCastValidator.OnAbstractionCheckBoxChange';
begin
  try
    UpdateAbstraction;
  except on E:Exception do HandleError(E, OPNAME); end;
end;

procedure TPlanningMineOpenCastValidator.OnDeleteOpenCast(Sender: TObject);
const OPNAME = 'TPlanningMineOpenCastValidator.OnDeleteOpenCast';
var
LMine : TPlanningMine;
//LOpenCast : TPlanningOpenCast;
begin
  try
    LMine :=   TPlanningMine(TPlanningModelDataObject(FAppModules.Model.ModelData).CastNetworkFeaturesData.CastMineList.CastMinebyNodeNumber[FIdentifier]);
    if LMine.RemoveOpenCast(FSelectOpenCastID) then
    begin
      PopulateDataViewer;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TPlanningMineOpenCastValidator.OnDrawAbstractionGridCell(
  Sender: TObject; ACol, ARow: Integer; Rect: TRect; State: TGridDrawState);
const OPNAME = 'TPlanningMineOpenCastValidator.OnDrawAbstractionGridCell';
begin
  try
    with PMiningOpenCastPitDialog do
    begin
      if Sender = AbstractionGrid then
      begin
        if (ACol = 4) And (ARow = 1) then
        begin
          if (AbstractionGrid.Cells[ACol,ARow] = '') or
          (AbstractionGrid.Cells[ACol,ARow] = FAppModules.Language.GetString('NoTimeSeriesFilePrompt')) then
          begin
            DrawFrameControl(AbstractionGrid.Canvas.handle, Rect, DFC_BUTTON, DFCS_BUTTONPUSH or
              DFCS_ADJUSTRECT or DFCS_PUSHED * Ord(False));
            AbstractionGrid.Canvas.Brush.Style := bsClear;
            AbstractionGrid.Canvas.Font.Color := clBlack;
            AbstractionGrid.Canvas.FillRect(Rect);
            AbstractionGrid.Canvas.TextRect(Rect, Rect.Left + 2, Rect.Top + 2, FAppModules.Language.GetString('LabelText.ThreeDots'));
            AbstractionGrid.Canvas.Brush := AbstractionGrid.Brush;

          end;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TPlanningMineOpenCastValidator.OnGridSelectCell(ASender: TObject; ACol: Integer; ARow: Integer; var CanSelect: Boolean);
const OPNAME = 'TPlanningMineOpenCastValidator.OnGridSelectCell';
var
  LMine : TPlanningMine;
  LOpenCast : TPlanningOpenCast;
  LGrowth : TPlanningMineGrowthFactor;
begin
  inherited;
  try
    LMine :=  TPlanningMine(TPlanningModelDataObject(FAppModules.Model.ModelData).CastNetworkFeaturesData.CastMineList.CastMinebyNodeNumber[FIdentifier]);
    if LMine <> nil then
    begin
      LOpenCast := LMine.CastOpenCastByID[FSelectOpenCastID];
      if LOpenCast <> nil then
      begin
       if ASender = PMiningOpenCastPitDialog.OpenCastPitsGrid then
       begin
         if ((CanSelect)  AND (ARow > 0)) AND (ASender = MiningOpenCastPitDialog.OpenCastPitsGrid)  then
         begin
             PopulateGrowthFactorControl(LOpenCast);
             PMiningOpenCastPitDialog.GrowthFactorControl.Visible :=  CanSelect;
             PopulateLoadGenerationControl(LOpenCast);
             PMiningOpenCastPitDialog.LoadGenerationControl.Visible := CanSelect;
             PopulateAbstractionControl(LOpenCast);
             PMiningOpenCastPitDialog.AbstractionControl.Visible := CanSelect;
         end;
       end
       else
       if (ASender = PMiningOpenCastPitDialog.GrowthFactorGrid) and (ARow > 0)  then
        begin
          LGrowth :=  LOpenCast.GrowthFactorByIndex(ARow-1);
          PopulateYearAndFactorGrid(LGrowth);
        end
        else
        begin
          PMiningOpenCastPitDialog.GrowthFactorControl.Visible := CanSelect;
          PMiningOpenCastPitDialog.LoadGenerationControl.Visible := CanSelect;
        end;
      end
      else
      begin
         ClearDataViewer;
      end;
    end;
  except on E: Exception do HandleError(E,OPNAME); end;
end;

procedure TPlanningMineOpenCastValidator.OnGrowthGridSelectCell(
  ASender: TObject; ACol, ARow: Integer; var CanSelect: Boolean);
const OPNAME = 'TReturnFlowChannelValidator.OnGrdCorrespondingChannelSelectCell';
var
  LInterpolation : string;
  LMine : TPlanningMine;
  LOpenCast : TPlanningOpenCast;
  LGrowth : TPlanningMineGrowthFactor;
begin
  try
    with PMiningOpenCastPitDialog do
    begin
      if ACol = 2 then
      begin
        LMine :=  TPlanningMine(TPlanningModelDataObject(FAppModules.Model.ModelData).CastNetworkFeaturesData.CastMineList.CastMinebyNodeNumber[FIdentifier]);
        if LMine <> nil then
        begin
          LOpenCast := LMine.CastOpenCastByID[FSelectOpenCastID];
          if LOpenCast <> nil then
          begin

            if (ASender = PMiningOpenCastPitDialog.GrowthFactorGrid) and (ARow > 0)  then
            begin
              //UpdateYearDataPoints;
              LGrowth :=  LOpenCast.GrowthFactorByIndex(ARow-1);
              PopulateYearAndFactorGrid(LGrowth);
            end;

          end;
        end;
      end;

      if ACol = 1 then
      begin
        InterpolationCmBox.Top :=  2 + GrowthFactorGrid.Top + ((1 + GrowthFactorGrid.DefaultRowHeight) *
                                    (ARow - GrowthFactorGrid.TopRow + 1));
        InterpolationCmBox.Left := GrowthFactorGrid.ColWidths[0] + 10;
        LInterpolation := Trim(GrowthFactorGrid.Cells[1, ARow]);
        InterpolationCmBox.ItemIndex := InterpolationCmBox.Items.IndexOf(LInterpolation);
        InterpolationCmBox.Visible := True;
        if (GrowthFactorGrid.ValidationError[ACol, ARow, gveCellContext] <> '') then
        begin
          InterpolationCmBox.ValidationError   := GrowthFactorGrid.ValidationError[ACol, ARow, gveCellContext];
          InterpolationCmBox.InValidationError := True;
          InterpolationCmBox.ShowErrorState(True);
        end
        else
        begin
          InterpolationCmBox.ValidationError   := '';
          InterpolationCmBox.InValidationError := False;
          InterpolationCmBox.ShowErrorState(False);
        end;
      end
      else
        InterpolationCmBox.Visible := False;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TPlanningMineOpenCastValidator.OnGrowthGridChanged(Sender: TObject);
const OPNAME = 'TPlanningMineOpenCastValidator.OnGrowthGridChanged';
begin
  try
    PMiningOpenCastPitDialog.InterpolationCmBox.Visible := False;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TPlanningMineOpenCastValidator.OnInsertOpenCast(Sender: TObject);
const OPNAME = 'TPlanningMineOpenCastValidator.OnInsertOpenCast';
var
  LNewOpenCast : IPlanningOpenCast;
begin
  try
    LNewOpenCast := TPlanningModelManager(FAppModules.Model).DoCreateOpenCast(FIdentifier);
    if( LNewOpenCast <> nil)then
    begin
      RePopulateDataViewer;
      //DoContextValidation(dvtYMDCReturnFlowFeatureAll);
    end;
  except on E: Exception do HandleError(E,OPNAME); end;
end;

function TPlanningMineOpenCastValidator.PMiningOpenCastPitDialog: TPlanningMineOpenCastDialog;
const OPNAME = 'TPlanningMineOpenCastValidator.PMiningOpenCastPitDialog';
begin
  Result := nil;
  try
     if Assigned(FPanel) then
      Result := TPlanningMineOpenCastDialog(FPanel);
  except on E: Exception do HandleError(E,OPNAME); end;
end;

procedure TPlanningMineOpenCastValidator.RePopulateDataViewer;
const OPNAME = 'TPlanningMineOpenCastValidator.RePopulateDataViewer';
var
  LMine : TPlanningMine;
  LRow : integer;
  LOpenCast :  TPlanningOpenCast;
  LCummulativeRowSize : integer;
  LOpenCastsExist : boolean;
//  LCell : TGridCoord;
begin
  inherited;
  try
    ClearDataViewer;
    LMine := TPlanningMine(TPlanningModelDataObject(FAppModules.Model.ModelData).CastNetworkFeaturesData.
                CastMineList.CastMinebyNodeNumber[FIdentifier]);
    LCummulativeRowSize := 0;
    if (LMine <> nil) then
    begin
      with PMiningOpenCastPitDialog do
      begin
        OpenCastPitsGrid.ColCount := 27;
        OpenCastPitsGrid.RowCount := Max((OpenCastPitsGrid.FixedRows + 1),(1 + LMine.OpenCastCount));
        OpenCastPitsGrid.Width := 3 + (1 + OpenCastPitsGrid.DefaultColWidth) * OpenCastPitsGrid.ColCount;
        //OpenCastPitsGrid.ClearErrors;
        LOpenCastsExist := LMine.OpenCastCount > 0;
        if (LOpenCastsExist) then
        begin
          for LRow := 1 to LMine.OpenCastCount do
          begin
            LOpenCast := LMine.CastOpenCastByIndex[LRow-1];

            OpenCastPitsGrid.Cells[19, LRow] :=  FloatToStr(LOpenCast.PCDIniConcentration);
            OpenCastPitsGrid.Cells[20, LRow] :=  IntToStr(LOpenCast.WorkingCommYear);
            OpenCastPitsGrid.Cells[21, LRow] :=  IntToStr(LOpenCast.WorkingCommMonth);
            OpenCastPitsGrid.Cells[22, LRow] :=  IntToStr(LOpenCast.WorkingDecommYear);
            OpenCastPitsGrid.Cells[23, LRow] :=  IntToStr(LOpenCast.WorkingDecommMonth);
            OpenCastPitsGrid.Cells[24, LRow] :=  FloatToStr(LOpenCast.RunOffSaltWashOffEfficiencyFactor);
            OpenCastPitsGrid.Cells[25, LRow] :=  FloatToStr(LOpenCast.IniSaltStore);
            OpenCastPitsGrid.Cells[26, LRow] :=  FloatToStr(LOpenCast.ReChargeRate);
            PopulateGrowthFactorControl(LOpenCast);
            PopulateLoadGenerationControl(LOpenCast);
            PopulateAbstractionControl(LOpenCast);
          end;

          for LRow := 0 to OpenCastPitsGrid.RowCount -1 do
          begin
            LCummulativeRowSize := LCummulativeRowSize + OpenCastPitsGrid.RowHeights[LRow];
          end;

          OpenCastPitsGrid.Height := LCummulativeRowSize + 30;
          OpenCastPitsGrid.Repaint;

          AbstractionControl.Top          :=  OpenCastPitsGrid.Top +  OpenCastPitsGrid.Height + 10;
          GrowthFactorControl.Top         := AbstractionControl.Top;
          LoadGenerationControl.Top       := AbstractionControl.Top +  AbstractionControl.Height + 10;
          YearAndFactorGrid.Top           := GrowthFactorGrid.Top + GrowthFactorGrid.Height + 10;
          //YearAndFactorGrid.Left          := GrowthFactorGrid.Width + 15;
         // LCell := OpenCastPitsGrid.CurrenCell;
        //  OnGridSelectCell(TObject(OpenCastPitsGrid),LCell.X,LCell.Y,LOpenCastsExist);
        //  OnGridSelectCell(TObject(AbstractionGrid),0,1,LOpenCastsExist);
        //  OnGridSelectCell(TObject( GrowthFactorGrid),1,1,LOpenCastsExist);
         // OnGridSelectCell(TObject( FactorAndMeanGrid),0,1,LOpenCastsExist);


        end
        else
        AbstractionGrid.Enabled := False;
      end;
    end;
  except on E: Exception do HandleError(E,OPNAME); end;
end;

procedure TPlanningMineOpenCastValidator.UpdateIntepolationMethod;
const OPNAME = 'TPlanningMineOpenCastValidator.UpdateIntepolationMethod';
var
  LMine : TPlanningMine;
  LOpenCast : TPlanningOpenCast;
  LGrowth : TPlanningMineGrowthFactor;
  LMessage: string;
  LValue : integer;
begin
  try
    LMine := TPlanningMine(TPlanningModelDataObject(FAppModules.Model.ModelData).CastNetworkFeaturesData.
                CastMineList.CastMinebyNodeNumber[FIdentifier]);
    if LMine <> nil then
    begin
      with PMiningOpenCastPitDialog do
      begin
        LOpenCast := LMine.CastOpenCastByID[FSelectOpenCastID];
        if LOpenCast <> nil then
        begin

          LValue := integer(PMiningOpenCastPitDialog.InterpolationCmBox.Items.Objects[PMiningOpenCastPitDialog.InterpolationCmBox.ItemIndex]);
          LGrowth := LOpenCast.GrowthFactorByIndex(PMiningOpenCastPitDialog.GrowthFactorGrid.Row-1);
          if LGrowth <> nil then
          begin
            if (FAppModules.FieldProperties.ValidateFieldProperty(
                  'InterpolationMethod', IntToStr(LValue), LMessage)) then
            begin
              LGrowth.InterpolationMethod := LValue;
              RePopulateDataViewer;
              DoContextValidation(dvtInterpolationMethod);
            end
              else
                GrowthFactorGrid.ValidationError[GrowthFactorGrid.Col, GrowthFactorGrid.Row,gveCellField ] := LMessage;


          end;
        end;
      end;
    end;
  except on E: Exception do HandleError(E,OPNAME); end;
end;

procedure TPlanningMineOpenCastValidator.UpdateOpenCastIniSaltStore;
const OPNAME = 'TPlanningMineOpenCastValidator.UpdateOpenCastIniSaltStore';
var
  LMine : TPlanningMine;
  LOpenCast : TPlanningOpenCast;
  LMessage : string;
  LValue : double;
begin
  try
    with PMiningOpenCastPitDialog do
    begin
      LMine :=  TPlanningMine(TPlanningModelDataObject(FAppModules.Model.ModelData).CastNetworkFeaturesData.CastMineList.CastMinebyNodeNumber[FIdentifier]);
      if LMine <> nil then
      begin
        LOpenCast := LMine.CastOpenCastByID[FSelectOpenCastID];
        if LOpenCast <> nil then
        begin
          LValue := StrToFloat(OpenCastPitsGrid.Cells[OpenCastPitsGrid.Col, OpenCastPitsGrid.Row]);
          if (FAppModules.FieldProperties.ValidateFieldProperty(
              'OpenCastIniSaltStore', FloatToStr(LValue), LMessage)) then
          begin
            LOpenCast.IniSaltStore := LValue;
            RePopulateDataViewer;
            //ValidateOpenCastIniSaltStore(LOpenCast);
            DoContextValidation(dvtIniSaltStore);
          end
          else
            OpenCastPitsGrid.ValidationError[OpenCastPitsGrid.Col, OpenCastPitsGrid.Row,gveCellField ] := LMessage;
        end;
      end;
    end;

  except on E: Exception do HandleError(E,OPNAME); end;
end;

procedure TPlanningMineOpenCastValidator.UpdatePCDIniConcentration;
const OPNAME = 'TPlanningMineOpenCastValidator.UpdatePCDIniConcentration';
var
  LMine : TPlanningMine;
  LOpenCast : TPlanningOpenCast;
  LMessage : string;
  LValue : double;
begin
  try
    with PMiningOpenCastPitDialog do
    begin
      LMine :=  TPlanningMine(TPlanningModelDataObject(FAppModules.Model.ModelData).CastNetworkFeaturesData.CastMineList.CastMinebyNodeNumber[FIdentifier]);
      if LMine <> nil then
      begin
        LOpenCast := LMine.CastOpenCastByID[FSelectOpenCastID];
        if LOpenCast <> nil then
        begin
          LValue := StrToFloat(OpenCastPitsGrid.Cells[OpenCastPitsGrid.Col, OpenCastPitsGrid.Row]);
          if (FAppModules.FieldProperties.ValidateFieldProperty(
              'PCDIniConcentration', FloatToStr(LValue), LMessage)) then
          begin
            LOpenCast.PCDIniConcentration := LValue;
            RePopulateDataViewer;
            DoContextValidation(dvtPCDIniConcentration);
            //ValidatePCDIniConcentration(LOpenCast);
          end
          else
            OpenCastPitsGrid.ValidationError[OpenCastPitsGrid.Col, OpenCastPitsGrid.Row,gveCellField ] := LMessage;
        end;
      end;
    end;

  except on E: Exception do HandleError(E,OPNAME); end;
end;


procedure TPlanningMineOpenCastValidator.UpdateRechargeRate;
const OPNAME = 'TPlanningMineOpenCastValidator.UpdateRechargeRate';
var
  LMine : TPlanningMine;
  LOpenCast : TPlanningOpenCast;
  LMessage : string;
  LValue : double;
begin
  try
    with PMiningOpenCastPitDialog do
    begin
      LMine :=  TPlanningMine(TPlanningModelDataObject(FAppModules.Model.ModelData).CastNetworkFeaturesData.CastMineList.CastMinebyNodeNumber[FIdentifier]);
      if LMine <> nil then
      begin
        LOpenCast := LMine.CastOpenCastByID[FSelectOpenCastID];
        if LOpenCast <> nil then
        begin
          LValue := StrToFloat(OpenCastPitsGrid.Cells[OpenCastPitsGrid.Col, OpenCastPitsGrid.Row]);
          if (FAppModules.FieldProperties.ValidateFieldProperty(
              'RechargeRate', FloatToStr(LValue), LMessage)) then
          begin
            LOpenCast.ReChargeRate := LValue;
            RePopulateDataViewer;
            //ValidateRechargeRate(LOpenCast);
            DoContextValidation(dvtReChargeRate);
          end
          else
            OpenCastPitsGrid.ValidationError[OpenCastPitsGrid.Col, OpenCastPitsGrid.Row,gveCellField ] := LMessage;
        end;
      end;
    end;

  except on E: Exception do HandleError(E,OPNAME); end;
end;

procedure TPlanningMineOpenCastValidator.UpdateRunoffSaltWashOffEfficiencyFactor;
const OPNAME = 'TPlanningMineOpenCastValidator.UpdateRunoffSaltWashOffEfficiencyFactor';
var
  LMine : TPlanningMine;
  LOpenCast : TPlanningOpenCast;
  LMessage : string;
  LValue : double;
begin
  try
    with PMiningOpenCastPitDialog do
    begin
      LMine :=  TPlanningMine(TPlanningModelDataObject(FAppModules.Model.ModelData).CastNetworkFeaturesData.CastMineList.CastMinebyNodeNumber[FIdentifier]);
      if LMine <> nil then
      begin
        LOpenCast := LMine.CastOpenCastByID[FSelectOpenCastID];
        if LOpenCast <> nil then
        begin
          LValue := StrToFloat(OpenCastPitsGrid.Cells[OpenCastPitsGrid.Col, OpenCastPitsGrid.Row]);
          if (FAppModules.FieldProperties.ValidateFieldProperty(
              'RunoffSaltWashOffEfficiencyFactor', FloatToStr(LValue), LMessage)) then
          begin
            LOpenCast.RunOffSaltWashOffEfficiencyFactor := LValue;
            RePopulateDataViewer;
            //ValidateRunoffSaltWashOffEfficiencyFactor(LOpenCast);
            DoContextValidation(dvtRunOffSaltWashEfficieny);
          end
          else
            OpenCastPitsGrid.ValidationError[OpenCastPitsGrid.Col, OpenCastPitsGrid.Row,gveCellField ] := LMessage;
        end;
      end;
    end;

  except on E: Exception do HandleError(E,OPNAME); end;
end;

procedure TPlanningMineOpenCastValidator.UpdateWorkingCommMonth;
const OPNAME = 'TPlanningMineOpenCastValidator.UpdateWorkingCommMonth';
var
  LMine : TPlanningMine;
  LOpenCast : TPlanningOpenCast;
  LMessage : string;
  LValue : integer;
begin
  try
    with PMiningOpenCastPitDialog do
    begin
      LMine :=  TPlanningMine(TPlanningModelDataObject(FAppModules.Model.ModelData).CastNetworkFeaturesData.CastMineList.CastMinebyNodeNumber[FIdentifier]);
      if LMine <> nil then
      begin
        LOpenCast := LMine.CastOpenCastByID[FSelectOpenCastID];
        if LOpenCast <> nil then
        begin
          LValue := StrToInt(OpenCastPitsGrid.Cells[OpenCastPitsGrid.Col, OpenCastPitsGrid.Row]);
          if (FAppModules.FieldProperties.ValidateFieldProperty(
              'WorkingCommMonth', IntToStr(LValue), LMessage)) then
          begin
            LOpenCast.WorkingCommMonth := LValue;
            RePopulateDataViewer;
            //ValidateWorkingCommMonth(LOpenCast);
            DoContextValidation(dvtCommMonth);
          end
          else
            OpenCastPitsGrid.ValidationError[OpenCastPitsGrid.Col, OpenCastPitsGrid.Row,gveCellField ] := LMessage;
        end;
      end;
    end;

  except on E: Exception do HandleError(E,OPNAME); end;
end;

procedure TPlanningMineOpenCastValidator.UpdateWorkingCommYear;
const OPNAME = 'TPlanningMineOpenCastValidator.UpdateWorkingCommYear';
var
  LMine : TPlanningMine;
  LOpenCast : TPlanningOpenCast;
  LMessage : string;
  LValue : integer;
begin
  try
    with PMiningOpenCastPitDialog do
    begin
      LMine :=  TPlanningMine(TPlanningModelDataObject(FAppModules.Model.ModelData).CastNetworkFeaturesData.CastMineList.CastMinebyNodeNumber[FIdentifier]);
      if LMine <> nil then
      begin
        LOpenCast := LMine.CastOpenCastByID[FSelectOpenCastID];
        if LOpenCast <> nil then
        begin
          LValue := StrToInt(OpenCastPitsGrid.Cells[OpenCastPitsGrid.Col, OpenCastPitsGrid.Row]);
          if (FAppModules.FieldProperties.ValidateFieldProperty(
              'WorkingCommYear', IntToStr(LValue), LMessage)) then
          begin
            LOpenCast.WorkingCommYear := LValue;
            RePopulateDataViewer;
            //ValidateWorkingCommYear(LOpenCast);
            DoContextValidation(dvtCommYear);
          end
          else
            OpenCastPitsGrid.ValidationError[OpenCastPitsGrid.Col, OpenCastPitsGrid.Row,gveCellField ] := LMessage;
        end;
      end;
    end;

  except on E: Exception do HandleError(E,OPNAME); end;
end;


procedure TPlanningMineOpenCastValidator.UpdateWorkingDecommMonth;
const OPNAME = 'TPlanningMineOpenCastValidator.UpdateWorkingDecommMonth';
var
  LMine : TPlanningMine;
  LOpenCast : TPlanningOpenCast;
  LMessage : string;
  LValue : integer;
begin
  try
    with PMiningOpenCastPitDialog do
    begin
      LMine :=  TPlanningMine(TPlanningModelDataObject(FAppModules.Model.ModelData).CastNetworkFeaturesData.CastMineList.CastMinebyNodeNumber[FIdentifier]);
      if LMine <> nil then
      begin
        LOpenCast := LMine.CastOpenCastByID[FSelectOpenCastID];
        if LOpenCast <> nil then
        begin
          LValue := StrToInt(OpenCastPitsGrid.Cells[OpenCastPitsGrid.Col, OpenCastPitsGrid.Row]);
          if (FAppModules.FieldProperties.ValidateFieldProperty(
              'WorkingDecommMonth', IntToStr(LValue), LMessage)) then
          begin
            LOpenCast.WorkingDecommMonth := LValue;
            RePopulateDataViewer;
            //ValidateWorkingDecommMonth(LOpenCast);
            DoContextValidation(dvtDecommMonth);
          end
          else
            OpenCastPitsGrid.ValidationError[OpenCastPitsGrid.Col, OpenCastPitsGrid.Row,gveCellField ] := LMessage;
        end;
      end;
    end;

  except on E: Exception do HandleError(E,OPNAME); end;
end;

procedure TPlanningMineOpenCastValidator.UpdateWorkingDecommYear;
const OPNAME = 'TPlanningMineOpenCastValidator.UpdateWorkingDecommYear';
var
  LMine : TPlanningMine;
  LOpenCast : TPlanningOpenCast;
  LMessage : string;
  LValue : integer;
begin
  try
    with PMiningOpenCastPitDialog do
    begin
      LMine :=  TPlanningMine(TPlanningModelDataObject(FAppModules.Model.ModelData).CastNetworkFeaturesData.CastMineList.CastMinebyNodeNumber[FIdentifier]);
      if LMine <> nil then
      begin
        LOpenCast := LMine.CastOpenCastByID[FSelectOpenCastID];
        if LOpenCast <> nil then
        begin
          LValue := StrToInt(OpenCastPitsGrid.Cells[OpenCastPitsGrid.Col, OpenCastPitsGrid.Row]);
          if (FAppModules.FieldProperties.ValidateFieldProperty(
              'WorkingDecommYear', IntToStr(LValue), LMessage)) then
          begin
            LOpenCast.WorkingDecommYear := LValue;
            RePopulateDataViewer;
            //ValidateWorkingDecommYear(LOpenCast);
            DoContextValidation(dvtDecommYear);
          end
          else
            OpenCastPitsGrid.ValidationError[OpenCastPitsGrid.Col, OpenCastPitsGrid.Row,gveCellField ] := LMessage;
        end;
      end;
    end;

  except on E: Exception do HandleError(E,OPNAME); end;
end;


procedure TPlanningMineOpenCastValidator.UPdateYearDataPoints;
const OPNAME = 'TPlanningMineOpenCastValidator.UPdateYearDataPoints';
var
  LMine : TPlanningMine;
  LOpenCast : TPlanningOpenCast;
  LGrowth : TPlanningMineGrowthFactor;
  LMessage : string;
  LValue : integer;
begin
  try
    with PMiningOpenCastPitDialog do
    begin
      LMine :=  TPlanningMine(TPlanningModelDataObject(FAppModules.Model.ModelData).CastNetworkFeaturesData.CastMineList.CastMinebyNodeNumber[FIdentifier]);
      if LMine <> nil then
      begin
        LOpenCast := LMine.CastOpenCastByID[FSelectOpenCastID];
        if LOpenCast <> nil then
        begin
          LGrowth := LOpenCast.GrowthFactorByIndex(GrowthFactorGrid.Row-1);
          if LGrowth <> nil then
          begin
            LValue := StrToInt(GrowthFactorGrid.Cells[GrowthFactorGrid.Col, GrowthFactorGrid.Row]);
            if (FAppModules.FieldProperties.ValidateFieldProperty(
                'NoOfPoints', IntToStr(LValue), LMessage)) then
            begin
              LGrowth.NoOfPoints := LValue;
              PopulateGrowthFactorControl(LOpenCast,GrowthFactorGrid.Row +1);
              DoContextValidation(dvtNoOfPoints);
            end
            else
              GrowthFactorGrid.ValidationError[GrowthFactorGrid.Col, GrowthFactorGrid.Row,gveCellField ] := LMessage;
          end;
        end;
      end;
    end;

  except on E: Exception do HandleError(E,OPNAME); end;
end;

procedure TPlanningMineOpenCastValidator.UpdateAbstraction;
const OPNAME = 'TPlanningMineOpenCastValidator.UpdateAbstraction';
var
  LMine : TPlanningMine;
  LOpenCast : TPlanningOpenCast;
  LMessage : string;
begin
  try
    with PMiningOpenCastPitDialog do
    begin
      LMine :=  TPlanningMine(TPlanningModelDataObject(FAppModules.Model.ModelData).CastNetworkFeaturesData.CastMineList.CastMinebyNodeNumber[FIdentifier]);
      if LMine <> nil then
      begin
        LOpenCast := LMine.CastOpenCastByID[FSelectOpenCastID];
        if LOpenCast <> nil then
        begin
          if (FAppModules.FieldProperties.ValidateFieldProperty(
              'AbstractionIndicator', LOpenCast.BoolToStr(AbstractionCheckBox.Checked), LMessage)) then
          begin
            LOpenCast.Abstraction := AbstractionCheckBox.Checked;
            //ClearDataViewer;
            //RePopulateDataViewer;
            PopulateAbstractionControl(LOpenCast);
            //ValidateAbstraction(LOpenCast);
          end
          else
            AbstractionCheckBox.ValidationError := LMessage;
        end;
      end;
    end;
  except on E: Exception do HandleError(E,OPNAME); end;
end;

procedure TPlanningMineOpenCastValidator.UpdateAbstractionToCPD;
const OPNAME = 'TPlanningMineOpenCastValidator.UpdateAbstractionToEvap';
var
  LMine : TPlanningMine;
  LOpenCast : TPlanningOpenCast;
  //LGrowthFactor : TPlanningMineGrowthFactor;
  LMessage : string;
  LCellVal : string;
begin
  try
    with PMiningOpenCastPitDialog do
    begin
      LMine :=  TPlanningMine(TPlanningModelDataObject(FAppModules.Model.ModelData).CastNetworkFeaturesData.CastMineList.CastMinebyNodeNumber[FIdentifier]);
      if LMine <> nil then
      begin
        LOpenCast := LMine.CastOpenCastByID[FSelectOpenCastID];
        if LOpenCast <> nil then
        begin
          with PMiningOpenCastPitDialog do
          begin
            LCellVal := AbstractionGrid.Cells[AbstractionGrid.Col,AbstractionGrid.Row];
            if (FAppModules.FieldProperties.ValidateFieldProperty(
                'AbstractToCPD',LCellVal, LMessage)) then
            begin

                LOpenCast.AbstractToCPD  := StrToFloat(Trim(LCellVal));
                //ClearDataViewer;
                //RePopulateDataViewer;
                PopulateAbstractionControl(LOpenCast);
                //ValidateAbstractionToCPD(LOpenCast);
            end
            else
              AbstractionGrid.ValidationError[AbstractionGrid.Col, AbstractionGrid.Row,gveCellField ] := LMessage;
          end;
        end;
      end;
    end;
  except on E: Exception do HandleError(E,OPNAME); end;
end;

procedure TPlanningMineOpenCastValidator.UpdateAbstractionToEvap;
const OPNAME = 'TPlanningMineOpenCastValidator.UpdateAbstractionToEvap';
var
  LMine : TPlanningMine;
  LOpenCast : TPlanningOpenCast;
  //LGrowthFactor : TPlanningMineGrowthFactor;
  LMessage : string;
  LCellVal : string;
begin
  try
    with PMiningOpenCastPitDialog do
    begin
      LMine :=  TPlanningMine(TPlanningModelDataObject(FAppModules.Model.ModelData).CastNetworkFeaturesData.CastMineList.CastMinebyNodeNumber[FIdentifier]);
      if LMine <> nil then
      begin
        LOpenCast := LMine.CastOpenCastByID[FSelectOpenCastID];
        if LOpenCast <> nil then
        begin
          with PMiningOpenCastPitDialog do
          begin
            LCellVal := AbstractionGrid.Cells[AbstractionGrid.Col,AbstractionGrid.Row];
            if Trim(LCellVal) <> '' then
            begin
              if (FAppModules.FieldProperties.ValidateFieldProperty(
                  'AbstractToEvap',LCellVal, LMessage)) then
              begin

                  LOpenCast.AbstractToEvap  := StrToFloat(Trim(LCellVal));
                  //ClearDataViewer;
                  //RePopulateDataViewer;
                  PopulateAbstractionControl(LOpenCast);
                  //ValidateAbstractionToEvap(LOpenCast);
              end

              else
                AbstractionGrid.ValidationError[AbstractionGrid.Col, AbstractionGrid.Row,gveCellField ] := LMessage;
            end;

          end;
        end;
      end;
    end;
  except on E: Exception do HandleError(E,OPNAME); end;
end;

procedure TPlanningMineOpenCastValidator.UpdateAbstractionToRiver;
const OPNAME = 'TPlanningMineOpenCastValidator.UpdateAbstractionToRiver';
var
  LMine : TPlanningMine;
  LOpenCast : TPlanningOpenCast;
  //LGrowthFactor : TPlanningMineGrowthFactor;
  LMessage : string;
  LCellVal : string;
begin
  try
    with PMiningOpenCastPitDialog do
    begin
      LMine :=  TPlanningMine(TPlanningModelDataObject(FAppModules.Model.ModelData).CastNetworkFeaturesData.CastMineList.CastMinebyNodeNumber[FIdentifier]);
      if LMine <> nil then
      begin
        LOpenCast := LMine.CastOpenCastByID[FSelectOpenCastID];
        if LOpenCast <> nil then
        begin
          with PMiningOpenCastPitDialog do
          begin
            LCellVal := AbstractionGrid.Cells[AbstractionGrid.Col,AbstractionGrid.Row];
            if (FAppModules.FieldProperties.ValidateFieldProperty(
                'AbstractToRiver',LCellVal, LMessage)) then
            begin
              LOpenCast.AbstractToRiver  := StrToFloat(Trim(LCellVal));
              PopulateAbstractionControl(LOpenCast);

            end
            else
              AbstractionGrid.ValidationError[AbstractionGrid.Col, AbstractionGrid.Row,gveCellField ] := LMessage;
          end;
        end;
      end;
    end;
  except on E: Exception do HandleError(E,OPNAME); end;
end;

procedure TPlanningMineOpenCastValidator.UpdateAbstractMonthTimeSeriesFile;
const OPNAME = 'TPlanningMineOpenCastValidator.UpdateAbstractionToRiver';
var
  LMine : TPlanningMine;
  LOpenCast : TPlanningOpenCast;
  //LGrowthFactor : TPlanningMineGrowthFactor;
  LMessage : string;
  LCellVal : string;
begin
  try
    with PMiningOpenCastPitDialog do
    begin
      LMine :=  TPlanningMine(TPlanningModelDataObject(FAppModules.Model.ModelData).CastNetworkFeaturesData.CastMineList.CastMinebyNodeNumber[FIdentifier]);
      if LMine <> nil then
      begin
        LOpenCast := LMine.CastOpenCastByID[FSelectOpenCastID];
        if LOpenCast <> nil then
        begin
          with PMiningOpenCastPitDialog do
          begin
            LCellVal := AbstractionGrid.Cells[AbstractionGrid.Col,AbstractionGrid.Row];
            if (FAppModules.FieldProperties.ValidateFieldProperty(
                'AbstractMonthTimeSeriesFile',LCellVal, LMessage)) then
            begin

                LOpenCast.AbstractMonthTimeSeriesFile  := Trim(LCellVal);
                //ClearDataViewer;
                //RePopulateDataViewer;
                PopulateAbstractionControl(LOpenCast);
                //ValidateAbstractMonthTimeSeriesFile(LOpenCast);
            end
            else
              AbstractionGrid.ValidationError[AbstractionGrid.Col, AbstractionGrid.Row,gveCellField ] := LMessage;
          end;
        end;
      end;
    end;
  except on E: Exception do HandleError(E,OPNAME); end;
end;

procedure TPlanningMineOpenCastValidator.UpDateGrowthFactor(ACol, ARow: integer);
const OPNAME = 'TPlanningMineOpenCastValidator.UpDateGrowthFactor';
var
  LMine : TPlanningMine;
  LOpenCast : TPlanningOpenCast;
  LGrowthFactor : TPlanningMineGrowthFactor;
  LMessage : string;
  LCellVal : string;
begin
  try
    with PMiningOpenCastPitDialog do
    begin
      LMine :=  TPlanningMine(TPlanningModelDataObject(FAppModules.Model.ModelData).CastNetworkFeaturesData.CastMineList.CastMinebyNodeNumber[FIdentifier]);
      if LMine <> nil then
      begin
        LOpenCast := LMine.CastOpenCastByID[FSelectOpenCastID];
        if LOpenCast <> nil then
        begin
          with PMiningOpenCastPitDialog do
          begin
            LCellVal := YearAndFactorGrid.Cells[ACol,ARow];
            if (FAppModules.FieldProperties.ValidateFieldProperty(
                'GrowthFactors',LCellVal, LMessage)) then
            begin
              LGrowthFactor := LOpenCast.GrowthFactorByIndex(GrowthFactorGrid.Row - 1);
              if LGrowthFactor <> nil then
              begin
                LGrowthFactor.GrowthFactorByIndex[ACol -1]  := StrToFloat(Trim(YearAndFactorGrid.Cells[ACol,ARow]));
                //ClearDataViewer;
                //RePopulateDataViewer;
                PopulateYearAndFactorGrid(LGrowthFactor);
                //ValidateGrowthFactor(LGrowthFactor);
              end;
            end
            else
              YearAndFactorGrid.ValidationError[YearAndFactorGrid.Col, YearAndFactorGrid.Row,gveCellField ] := LMessage;
          end;
        end;
      end;
    end;
  except on E: Exception do HandleError(E,OPNAME); end;
end;

procedure TPlanningMineOpenCastValidator.UpdateStandardDeviation;
const OPNAME = 'TPlanningMineOpenCastValidator.UpdateStandardDeviation';
var
  LMine : TPlanningMine;
  LOpenCast : TPlanningOpenCast;
  LLoadGeneration : TLoadGeneration;
  LMessage : string;
  LCellVal : string;
begin
  try
    with PMiningOpenCastPitDialog do
    begin
      LMine :=  TPlanningMine(TPlanningModelDataObject(FAppModules.Model.ModelData).CastNetworkFeaturesData.CastMineList.CastMinebyNodeNumber[FIdentifier]);
      if LMine <> nil then
      begin
        LOpenCast := LMine.CastOpenCastByID[FSelectOpenCastID];
        if LOpenCast <> nil then
        begin
          with PMiningOpenCastPitDialog do
          begin
            LCellVal := StdDeviationEdit.Text;
            if (FAppModules.FieldProperties.ValidateFieldProperty(
                'StdDeviation',LCellVal, LMessage)) then
            begin
              LLoadGeneration := LOpenCast.LoadGenerationByType(LoadGenerationCombo.ItemIndex + 1);
              if LLoadGeneration <> nil then
              begin
                LLoadGeneration.StandardDeviation  := StrToFloat(StdDeviationEdit.Text);
                //ClearDataViewer;
                //RePopulateDataViewer;
                PopulateLoadGenerationControl(LOpenCast,LLoadGeneration.type_);
                //PopulateYearAndFactorGrid(LGrowthFactor);
                //ValidateNoOfYears(LGrowthFactor);
              end;
            end
            else
              StdDeviationEdit.FieldValidationError := LMessage;
          end;
        end;
      end;
    end;
  except on E: Exception do HandleError(E,OPNAME); end;
end;

procedure TPlanningMineOpenCastValidator.UpDateFlow;
const OPNAME = 'TPlanningMineOpenCastValidator.UpDateFlow';
var
  LMine : TPlanningMine;
  LOpenCast : TPlanningOpenCast;
  LLoadGeneration : TLoadGeneration;
  LMessage : string;
  LCellVal : string;
begin
  try
    with PMiningOpenCastPitDialog do
    begin
      LMine :=  TPlanningMine(TPlanningModelDataObject(FAppModules.Model.ModelData).CastNetworkFeaturesData.CastMineList.CastMinebyNodeNumber[FIdentifier]);
      if LMine <> nil then
      begin
        LOpenCast := LMine.CastOpenCastByID[FSelectOpenCastID];
        if LOpenCast <> nil then
        begin
          with PMiningOpenCastPitDialog do
          begin
            LCellVal := FactorAndMeanGrid.Cells[FactorAndMeanGrid.Col, FactorAndMeanGrid.Row];
            if trim(LCellVal) <> '' then
            begin
              if (FAppModules.FieldProperties.ValidateFieldProperty(
                  'Flow',LCellVal, LMessage,FactorAndMeanGrid.Row)) then
              begin
                LLoadGeneration := LOpenCast.LoadGenerationByType(LoadGenerationCombo.ItemIndex + 1);
                if LLoadGeneration <> nil then
                begin
                  LLoadGeneration.FlowByIndex[FactorAndMeanGrid.Row - 1]  := StrToFloat(FactorAndMeanGrid.Cells[FactorAndMeanGrid.Col,FactorAndMeanGrid.Row]);
                  //ClearDataViewer;
                  //RePopulateDataViewer;
                  PopulateLoadGenerationControl(LOpenCast,LLoadGeneration.type_);
                  //PopulateYearAndFactorGrid(LGrowthFactor);
                  //ValidateNoOfYears(LGrowthFactor);
                end;
              end
              else
                FactorAndMeanGrid.ValidationError[FactorAndMeanGrid.Col, FactorAndMeanGrid.Row,gveCellField] := LMessage;
            end;
          end;
        end;
      end;
    end;
  except on E: Exception do HandleError(E,OPNAME); end;
end;

procedure TPlanningMineOpenCastValidator.UpDateMeanOfSalt;
const OPNAME = 'TPlanningMineOpenCastValidator.UpDateMeanOfSalt';
var
  LMine : TPlanningMine;
  LOpenCast : TPlanningOpenCast;
  LLoadGeneration : TLoadGeneration;
  LMessage : string;
  LCellVal : string;
begin
  try
    with PMiningOpenCastPitDialog do
    begin
      LMine :=  TPlanningMine(TPlanningModelDataObject(FAppModules.Model.ModelData).CastNetworkFeaturesData.CastMineList.CastMinebyNodeNumber[FIdentifier]);
      if LMine <> nil then
      begin
        LOpenCast := LMine.CastOpenCastByID[FSelectOpenCastID];
        if LOpenCast <> nil then
        begin
          with PMiningOpenCastPitDialog do
          begin
            LCellVal := FactorAndMeanGrid.Cells[FactorAndMeanGrid.Col, FactorAndMeanGrid.Row];
            if trim(LCellVal) <> '' then
            begin
              if (FAppModules.FieldProperties.ValidateFieldProperty(
                  'MeanOfSalt',LCellVal, LMessage,FactorAndMeanGrid.Row)) then
              begin
                LLoadGeneration := LOpenCast.LoadGenerationByType(Integer(LoadGenerationCombo.Items.Objects[LoadGenerationCombo.ItemIndex]));
                if LLoadGeneration <> nil then
                begin
                  LLoadGeneration.MeanOfSaltByIndex[FactorAndMeanGrid.Row - 1]  := StrToFloat(FactorAndMeanGrid.Cells[FactorAndMeanGrid.Col,FactorAndMeanGrid.Row]);
                  //ClearDataViewer;
                  //RePopulateDataViewer;
                  PopulateLoadGenerationControl(LOpenCast,LLoadGeneration.type_);
                  //PopulateYearAndFactorGrid(LGrowthFactor);
                  //ValidateNoOfYears(LGrowthFactor);
                end;
              end
              else
                FactorAndMeanGrid.ValidationError[FactorAndMeanGrid.Col, FactorAndMeanGrid.Row,gveCellField] := LMessage;
            end;
          end;
        end;
      end;
    end;
  except on E: Exception do HandleError(E,OPNAME); end;
end;

procedure TPlanningMineOpenCastValidator.UpDateNoOfYears(ACol, ARow: integer);
const OPNAME = 'TPlanningMineOpenCastValidator.UpDateNoOfYears';
var
  LMine : TPlanningMine;
  LOpenCast : TPlanningOpenCast;
  LGrowthFactor : TPlanningMineGrowthFactor;
  LMessage : string;
  LCellVal : string;
begin
  try
    with PMiningOpenCastPitDialog do
    begin
      LMine :=  TPlanningMine(TPlanningModelDataObject(FAppModules.Model.ModelData).CastNetworkFeaturesData.CastMineList.CastMinebyNodeNumber[FIdentifier]);
      if LMine <> nil then
      begin
        LOpenCast := LMine.CastOpenCastByID[FSelectOpenCastID];
        if LOpenCast <> nil then
        begin
          with PMiningOpenCastPitDialog do
          begin
            LCellVal := YearAndFactorGrid.Cells[ACol,ARow];
            if (FAppModules.FieldProperties.ValidateFieldProperty(
                'NYR',LCellVal, LMessage)) then
            begin
              LGrowthFactor := LOpenCast.GrowthFactorByIndex(GrowthFactorGrid.Row - 1);
              if LGrowthFactor <> nil then
              begin
                LGrowthFactor.NoOfYearsByIndex[ACol -1]  := StrToInt(YearAndFactorGrid.Cells[ACol,ARow]);
                //ClearDataViewer;
                //RePopulateDataViewer;
                PopulateYearAndFactorGrid(LGrowthFactor);
                //ValidateNoOfYears(LGrowthFactor);
              end;
            end
            else
              YearAndFactorGrid.ValidationError[YearAndFactorGrid.Col, YearAndFactorGrid.Row,gveCellField ] := LMessage;
          end;
        end;
      end;
    end;
  except on E: Exception do HandleError(E,OPNAME); end;
end;

procedure TPlanningMineOpenCastValidator.ValidateNoOfYears(AGrowthFactor : TPlanningMineGrowthFactor);
const OPNAME = 'TPlanningMineOpenCastValidator.ValidateNoOfYears';
var
  LErrorCols  : TStringList;
  LErrorMsgs  : TStringList;
  //LFieldProperties: TAbstractFieldProperty;
  LIndex,
  LCol : integer;
begin
  try
    LErrorCols  := TStringList.Create;
    LErrorMsgs  := TStringList.Create;
    //LFieldProperties := FAppModules.FieldProperties.FieldProperty('NYR');
    try
      if AGrowthFactor <> nil then
      begin
        if AGrowthFactor.Validate(FErrorMessage,'NYR') then
          for LCol := 1 to AGrowthFactor.NoOfPoints do
            PMiningOpenCastPitDialog.YearAndFactorGrid.ValidationError[LCol, 0, gveCellContext] := ''
        else
        begin
          ExtractErrorsAndColumns(FErrorMessage, LErrorMsgs, LErrorCols);
          for LCol := 1 to AGrowthFactor.NoOfPoints do
          begin
            LIndex := LErrorCols.IndexOf(IntToStr(LCol));
            if (LIndex >= 0) then
              PMiningOpenCastPitDialog.YearAndFactorGrid.ValidationError[LCol, 0, gveCellContext] := LErrorMsgs.Strings[lIndex]
            else
              PMiningOpenCastPitDialog.YearAndFactorGrid.ValidationError[LCol, 0, gveCellContext] := ''
          end;
        end;
      end;
   finally
     FreeAndNil(LErrorCols);
     FreeAndNil(LErrorMsgs);
   end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TPlanningMineOpenCastValidator.ValidateOpenCastIniSaltStore(
  AOpenCast: TPlanningOpenCast);
const OPNAME = 'TPlanningMineOpenCastValidator.ValidateOpenCastIniSaltStore';
var
  LRow  : integer;
begin
  try
    with MiningOpenCastPitDialog do
    begin
      LRow := OpenCastPitsGrid.Row;
      FErrorMessage := '';
      if(AOpenCast.Validate(FErrorMessage,'OpenCastIniSaltStore')) then
      begin
        OpenCastPitsGrid.ValidationError[25,LRow,gveCellContext] := '';
      end
      else
        OpenCastPitsGrid.ValidationError[25,LRow,gveCellContext] := FErrorMessage;
    end;
  except on E : Exception do HandleError(E, OPNAME); end;
end;

procedure TPlanningMineOpenCastValidator.ValidatePCDIniConcentration(AOpenCast: TPlanningOpenCast);
const OPNAME = 'TPlanningMineOpenCastValidator.ValidatePCDIniConcentration';
var
  LRow  : integer;
begin
  try
    with MiningOpenCastPitDialog do
    begin
      LRow := OpenCastPitsGrid.Row;
      FErrorMessage := '';
      if(AOpenCast.Validate(FErrorMessage,'PCDIniConcentration')) then
      begin
        OpenCastPitsGrid.ValidationError[19,LRow,gveCellContext] := '';
      end
      else
        OpenCastPitsGrid.ValidationError[19,LRow,gveCellContext] := FErrorMessage;
    end;
  except on E : Exception do HandleError(E, OPNAME); end;
end;

procedure TPlanningMineOpenCastValidator.ValidateRechargeRate(
  AOpenCast: TPlanningOpenCast);
const OPNAME = 'TPlanningMineOpenCastValidator.ValidateRechargeRate';
var
  LRow  : integer;
begin
  try
    with MiningOpenCastPitDialog do
    begin
      LRow := OpenCastPitsGrid.Row;
      FErrorMessage := '';
      if(AOpenCast.Validate(FErrorMessage,'RechargeRate')) then
      begin
        OpenCastPitsGrid.ValidationError[26,LRow,gveCellContext] := '';
      end
      else
        OpenCastPitsGrid.ValidationError[26,LRow,gveCellContext] := FErrorMessage;
    end;
  except on E : Exception do HandleError(E, OPNAME); end;
end;

procedure TPlanningMineOpenCastValidator.ValidateRunoffSaltWashOffEfficiencyFactor(
  AOpenCast: TPlanningOpenCast);
const OPNAME = 'TPlanningMineOpenCastValidator.ValidateRunoffSaltWashOffEfficiencyFactor';
var
  LRow  : integer;
begin
  try
    with MiningOpenCastPitDialog do
    begin
      LRow := OpenCastPitsGrid.Row;
      FErrorMessage := '';
      if(AOpenCast.Validate(FErrorMessage,'RunoffSaltWashOffEfficiencyFactor')) then
      begin
        OpenCastPitsGrid.ValidationError[24,LRow,gveCellContext] := '';
      end
      else
        OpenCastPitsGrid.ValidationError[24,LRow,gveCellContext] := FErrorMessage;
    end;
  except on E : Exception do HandleError(E, OPNAME); end;
end;

procedure TPlanningMineOpenCastValidator.ValidateWorkingCommMonth(
  AOpenCast: TPlanningOpenCast);
const OPNAME = 'TPlanningMineOpenCastValidator.ValidateWorkingCommMonth';
var
  LRow  : integer;
begin
  try
    with MiningOpenCastPitDialog do
    begin
      LRow := OpenCastPitsGrid.Row;
      FErrorMessage := '';
      if(AOpenCast.Validate(FErrorMessage,'WorkingCommMonth')) then
      begin
        OpenCastPitsGrid.ValidationError[21,LRow,gveCellContext] := '';
      end
      else
        OpenCastPitsGrid.ValidationError[21,LRow,gveCellContext] := FErrorMessage;
    end;
  except on E : Exception do HandleError(E, OPNAME); end;
end;


procedure TPlanningMineOpenCastValidator.ValidateWorkingCommYear(
  AOpenCast: TPlanningOpenCast);
const OPNAME = 'TPlanningMineOpenCastValidator.ValidateWorkingCommYear';
var
  LRow  : integer;
begin
  try
    with MiningOpenCastPitDialog do
    begin
      LRow := OpenCastPitsGrid.Row;
      FErrorMessage := '';
      if(AOpenCast.Validate(FErrorMessage,'WorkingCommYear')) then
      begin
        OpenCastPitsGrid.ValidationError[20,LRow,gveCellContext] := '';
      end
      else
        OpenCastPitsGrid.ValidationError[20,LRow,gveCellContext] := FErrorMessage;
    end;
  except on E : Exception do HandleError(E, OPNAME); end;
end;

procedure TPlanningMineOpenCastValidator.ValidateWorkingDecommMonth(
  AOpenCast: TPlanningOpenCast);
const OPNAME = 'TPlanningMineOpenCastValidator.ValidateWorkingDecommMonth';
var
  LRow  : integer;
begin
  try
    with MiningOpenCastPitDialog do
    begin
      LRow := OpenCastPitsGrid.Row;
      FErrorMessage := '';
      if(AOpenCast.Validate(FErrorMessage,'WorkingDecommMonth')) then
      begin
        OpenCastPitsGrid.ValidationError[23,LRow,gveCellContext] := '';
      end
      else
        OpenCastPitsGrid.ValidationError[23,LRow,gveCellContext] := FErrorMessage;
    end;
  except on E : Exception do HandleError(E, OPNAME); end;
end;

procedure TPlanningMineOpenCastValidator.ValidateWorkingDecommYear(
  AOpenCast: TPlanningOpenCast);
const OPNAME = 'TPlanningMineOpenCastValidator.ValidateWorkingDecommYear';
var
  LRow  : integer;
begin
  try
    with MiningOpenCastPitDialog do
    begin
      LRow := OpenCastPitsGrid.Row;
      FErrorMessage := '';
      if(AOpenCast.Validate(FErrorMessage,'WorkingDecommYear')) then
      begin
        OpenCastPitsGrid.ValidationError[22,LRow,gveCellContext] := '';
      end
      else
        OpenCastPitsGrid.ValidationError[22,LRow,gveCellContext] := FErrorMessage;
    end;
  except on E : Exception do HandleError(E, OPNAME); end;
end;
procedure TPlanningMineOpenCastValidator.ValidateNoOfPoints(AGrowthFactor : TPlanningMineGrowthFactor);
const OPNAME = 'TPlanningMineOpenCastValidator.ValidateInterpolationMedthod';
begin
  try
  with PMiningOpenCastPitDialog do
  begin
    if AGrowthFactor <> nil then
    begin
      FErrorMessage := '';
      AGrowthFactor.Validate(FErrorMessage,'NoOfPoints');
      GrowthFactorGrid.ValidationError[GrowthFactorGrid.Col, GrowthFactorGrid.Row,gveCellField] := FErrorMessage;
    end;
  end;
  except on E: Exception do HandleError(E,OPNAME); end;
end;

procedure TPlanningMineOpenCastValidator.ValidateAbstraction(
  AOpenCast: TPlanningOpenCast);
const OPNAME = 'TPlanningMineOpenCastValidator.ValidateAbstraction';
begin
  try
    with PMiningOpenCastPitDialog do
    begin
    if AOpenCast <> nil then
    begin
      FErrorMessage := '';
      AOpenCast.Validate(FErrorMessage,'AbstractionIndicator');
      AbstractionCheckBox.ValidationError := FErrorMessage;
    end;
  end;
  except on E: Exception do HandleError(E,OPNAME); end;
end;


procedure TPlanningMineOpenCastValidator.ValidateAbstractionToCPD(
  AOpenCast: TPlanningOpenCast);
const OPNAME = 'TPlanningMineOpenCastValidator.ValidateAbstractionToCPD';
begin
  try
    with PMiningOpenCastPitDialog do
    begin
      FErrorMessage := '';
      if(AOpenCast.Validate(FErrorMessage,'AbstractToCPD')) then
      begin
        AbstractionGrid.ValidationError[AbstractionGrid.Col,1,gveCellContext] := '';
      end
      else
        AbstractionGrid.ValidationError[AbstractionGrid.Col,1,gveCellContext] := FErrorMessage;
    end;
  except on E : Exception do HandleError(E, OPNAME); end;
end;

procedure TPlanningMineOpenCastValidator.ValidateAbstractionToEvap(
  AOpenCast: TPlanningOpenCast);
const OPNAME = 'TPlanningMineOpenCastValidator.ValidateAbstractionToEvap';
begin
  try
    with PMiningOpenCastPitDialog do
    begin
      FErrorMessage := '';
      if(AOpenCast.Validate(FErrorMessage,'AbstractToEvap')) then
      begin
        AbstractionGrid.ValidationError[AbstractionGrid.Col,1,gveCellContext] := '';
      end
      else
        AbstractionGrid.ValidationError[AbstractionGrid.Col,1,gveCellContext] := FErrorMessage;
    end;
  except on E : Exception do HandleError(E, OPNAME); end;
end;

procedure TPlanningMineOpenCastValidator.ValidateAbstractionToRiver(
  AOpenCast: TPlanningOpenCast);
const OPNAME = 'TPlanningMineOpenCastValidator.ValidateAbstractionToRiver';
begin
  try
    with PMiningOpenCastPitDialog do
    begin
      FErrorMessage := '';
      if(AOpenCast.Validate(FErrorMessage,'AbstractToRiver')) then
      begin
        AbstractionGrid.ValidationError[AbstractionGrid.Col,1,gveCellContext] := '';
      end
      else
        AbstractionGrid.ValidationError[AbstractionGrid.Col,1,gveCellContext] := FErrorMessage;
    end;
  except on E : Exception do HandleError(E, OPNAME); end;
end;

procedure TPlanningMineOpenCastValidator.ValidateAbstractMonthTimeSeriesFile(
  AOpenCast: TPlanningOpenCast);
const OPNAME = 'TPlanningMineOpenCastValidator.ValidateAbstractMonthTimeSeriesFile';
begin
  try
    with PMiningOpenCastPitDialog do
    begin
      FErrorMessage := '';
      if(AOpenCast.Validate(FErrorMessage,'AbstractMonthTimeSeriesFile')) then
      begin
        AbstractionGrid.ValidationError[AbstractionGrid.Col,1,gveCellContext] := '';
      end
      else
        AbstractionGrid.ValidationError[AbstractionGrid.Col,1,gveCellContext] := FErrorMessage;
    end;
  except on E : Exception do HandleError(E, OPNAME); end;
end;

procedure TPlanningMineOpenCastValidator.ValidateGrowthFactor(
  AGrowthFactor: TPlanningMineGrowthFactor);
const OPNAME = 'TPlanningMineOpenCastValidator.ValidateNoOfYears';
var
  LErrorCols  : TStringList;
  LErrorMsgs  : TStringList;
  //LFieldProperties: TAbstractFieldProperty;
  LIndex,
  LCol : integer;
begin
  try
    LErrorCols  := TStringList.Create;
    LErrorMsgs  := TStringList.Create;
    //LFieldProperties := FAppModules.FieldProperties.FieldProperty('NYR');
    try
      if AGrowthFactor <> nil then
      begin
        if AGrowthFactor.Validate(FErrorMessage,'GrowthFactors') then
          for LCol := 1 to AGrowthFactor.NoOfPoints do
            PMiningOpenCastPitDialog.YearAndFactorGrid.ValidationError[LCol, 1, gveCellContext] := ''
        else
        begin
          ExtractErrorsAndColumns(FErrorMessage, LErrorMsgs, LErrorCols);
          for LCol := 1 to AGrowthFactor.NoOfPoints do
          begin
            LIndex := LErrorCols.IndexOf(IntToStr(LCol));
            if (LIndex >= 0) then
              PMiningOpenCastPitDialog.YearAndFactorGrid.ValidationError[LCol, 1, gveCellContext] := LErrorMsgs.Strings[lIndex]
            else
              PMiningOpenCastPitDialog.YearAndFactorGrid.ValidationError[LCol, 1, gveCellContext] := '';
          end;
        end;
      end;
   finally
     FreeAndNil(LErrorCols);
     FreeAndNil(LErrorMsgs);
   end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


procedure TPlanningMineOpenCastValidator.ValidateInterpolationMedthod(AGrowthFactor : TPlanningMineGrowthFactor);
const OPNAME = 'TPlanningMineOpenCastValidator.ValidateInterpolationMedthod';
begin
  try
    with PMiningOpenCastPitDialog do
    begin
    if AGrowthFactor <> nil then
    begin
      FErrorMessage := '';
      AGrowthFactor.Validate(FErrorMessage,'InterpolationMethod');
      GrowthFactorGrid.ValidationError[GrowthFactorGrid.Col, GrowthFactorGrid.Row,gveCellField] := FErrorMessage;
    end;
  end;
  except on E: Exception do HandleError(E,OPNAME); end;
end;

procedure TPlanningMineOpenCastValidator.AddGrowthFactor(ASender: TObject);
const OPNAME = 'TPlanningMineOpenCastValidator.AddGrowthFactor';
var
  LMine : TPlanningMine;
  LOpenCast : TPlanningOpenCast;
  LType, LCount : integer;
begin
  try
  with PMiningOpenCastPitDialog do
  begin
    if (ASender = BtnAddGrowthFactors ) then
    begin
      LMine := TPlanningMine(TPlanningModelDataObject(FAppModules.Model.ModelData).CastNetworkFeaturesData.CastMineList.MineByNodeNumber[FIdentifier]);
      if LMine <> nil then
      begin
        if FSelectOpenCastID <> -1 then
        begin
          LOpenCast := LMine.CastOpenCastByID[FSelectOpenCastID];
          if LOpenCast <> nil then
          begin
            for LCount := 0 to LOpenCast.AcceptedGrowthTypes.Count -1 do
            begin
              LType := LOpenCast.AcceptedGrowthTypes[LCount];
              if LOpenCast.GrowthFactorByType(LType) = nil then
              begin
                if (LOpenCast.CreateGrowthFactor(LType) <> nil) then
                begin
                  PopulateGrowthFactorControl(LOpenCast);
                  Exit;
                end;
              end;
            end;
          end;
        end;
      end;
    end;
  end;

  except on E: Exception do HandleError(E, OPNAME); end;

end;

procedure TPlanningMineOpenCastValidator.AddLoadGeneration(ASender: TObject);
const OPNAME = 'TPlanningMineOpenCastValidator.AddGrowthFactor';
var
  LMine : TPlanningMine;
  LOpenCast : TPlanningOpenCast;
  LType, LCount : integer;
begin
  try
  with PMiningOpenCastPitDialog do
  begin
    if (ASender = BtnAddLoadGeneration ) then
    begin
      LMine := TPlanningMine(TPlanningModelDataObject(FAppModules.Model.ModelData).CastNetworkFeaturesData.CastMineList.MineByNodeNumber[FIdentifier]);
      if LMine <> nil then
      begin
        if FSelectOpenCastID <> -1 then
        begin
          LOpenCast := LMine.CastOpenCastByID[FSelectOpenCastID];
          if LOpenCast <> nil then
          begin
            for LCount := 0 to LOpenCast.AcceptedLoadGenTypes.Count-1 do
            begin
              LType := LOpenCast.AcceptedLoadGenTypes[LCount];
              if LOpenCast.LoadGenerationByType(LType) = nil then
              begin
                if (LOpenCast.CreateLoadGeneration(LType) <> nil) then
                begin
                  PopulateLoadGenerationControl(LOpenCast);
                  Exit;
                end;
              end;
            end;
          end;
        end;
      end;
    end;
  end;

  except on E: Exception do HandleError(E, OPNAME); end;

end;

procedure TPlanningMineOpenCastValidator.ClearAbstractionGrid;
const OPNAME='TPlanningMineOpenCastValidator.ClearAbstractionGrid';

begin
  try
    with PMiningOpenCastPitDialog do
    begin
       AbstractionGrid.Cells[0,1] := '';
       AbstractionGrid.Cells[1,1] := '';
       AbstractionGrid.Cells[2,1] := '';
       AbstractionGrid.Cells[3,1] := '';
       AbstractionGrid.Cells[4,1] := '';
    end;
  except on E: Exception do HandleError(E,OPNAME) end;
end;

procedure TPlanningMineOpenCastValidator.ClearDataViewer;
const OPNAME = 'TPlanningMineOpenCastValidator.ClearDataViewer';
begin
  try
    with PMiningOpenCastPitDialog do
    begin
      ClearGrowthFactorGrid;
      ClearAbstractionGrid;
      ClearFactorAndMeanGrid;
      FactorAndMeanGrid.FixedCols := 0;
      FactorAndMeanGrid.FixedRows := 1;
      FactorAndMeanGrid.ColCount := 2;
      FactorAndMeanGrid.RowCount := 11;
      FactorAndMeanGrid.Height := (FactorAndMeanGrid.DefaultRowHeight*FactorAndMeanGrid.RowCount) + Trunc(FactorAndMeanGrid.RowHeights[0]/2)+30;
      FactorAndMeanGrid.Width := 10+(1+FactorAndMeanGrid.DefaultColWidth*FactorAndMeanGrid.ColCount);//-FactorAndMeanGrid.ColWidths[0];
      FactorAndMeanGrid.Cells[0,0] := FAppModules.Language.GetString('FactorsAndMeanGrid.OpenCastTypeFlow1');
      FactorAndMeanGrid.Cells[1,0] := FAppModules.Language.GetString('FactorsAndMeanGrid.OpenCastTypeMean1');
    end;
  except on E: Exception do HandleError(E,OPNAME); end;
end;

procedure TPlanningMineOpenCastValidator.PopulateAbstractionControl(
  AOpenCast: TPlanningOpenCast);
const OPNAME = 'TPlanningMineOpenCastValidator.PopulateAbstractionControl';
var
  LVar: double;
  LFieldProperty      : TAbstractFieldProperty;
begin
  try
    if AOpenCast <> nil then
    begin
      with PMiningOpenCastPitDialog do
      begin
        AbstractionGrid.ClearErrors;
        AbstractionGrid.ClearFieldProperties;
        ClearAbstractionGrid;
        AbstractionCheckBox.Checked := AOpenCast.Abstraction;
        if AOpenCast.Abstraction then
        begin
          AbstractionGrid.FixedRows := 1;
          AbstractionGrid.FixedCols := 0;
          AbstractionGrid.RowCount := 2;
          AbstractionGrid.ColCount := 5;


          LFieldProperty := FAppModules.FieldProperties.FieldProperty('AbstractToEvap');
          AbstractionGrid.AddFieldProperty(LFieldProperty);
          AbstractionGrid.Cells[0,1] := Format(LFieldProperty.FormatStringGrid,[AOpenCast.AbstractToEvap]);
          LFieldProperty := FAppModules.FieldProperties.FieldProperty('AbstractToRiver');
          AbstractionGrid.AddFieldProperty(LFieldProperty);
          AbstractionGrid.Cells[1,1] := Format(LFieldProperty.FormatStringGrid,[AOpenCast.AbstractToRiver]);
          LFieldProperty := FAppModules.FieldProperties.FieldProperty('AbstractToCPD');
          AbstractionGrid.AddFieldProperty(LFieldProperty);
          AbstractionGrid.Cells[2,1] := Format(LFieldProperty.FormatStringGrid,[AOpenCast.AbstractToCPD]);

          LVar := AOpenCast.AbstractToEvap + AOpenCast.AbstractToRiver + AOpenCast.AbstractToCPD;
           LFieldProperty := FAppModules.FieldProperties.FieldProperty('AbstractTotal');
          AbstractionGrid.AddFieldProperty(LFieldProperty);
          AbstractionGrid.Cells[3,1] := Format(LFieldProperty.FormatStringGrid,[LVar]);
          //ValidateAbstraction(AOpenCast);
          DoContextValidation(dvtAbstraction);
          //ValidateAbstractionToEvap(AOpenCast);
          DoContextValidation(dvtAbstractToEvap);
          //ValidateAbstractionToRiver(AOpenCast);
          //ValidateAbstractionToCPD(AOpenCast);
          DoContextValidation(dvtAbstractToCPD);
          //ValidateAbstractMonthTimeSeriesFile(AOpenCast);
          DoContextValidation(dvtAbstractionTimeSeries);
          if LVar <> 1 then
          begin
            AbstractionGrid.ValidationError[3,1,gveCellField] :=FAppModules.Language.GetString('AbstractionTotalErrorString');
            AbstractionGrid.ValidationError[0,1,gveCellField] :=FAppModules.Language.GetString('AbstractionTotalErrorString');
            AbstractionGrid.ValidationError[1,1,gveCellField] :=FAppModules.Language.GetString('AbstractionTotalErrorString');
            AbstractionGrid.ValidationError[2,1,gveCellField] :=FAppModules.Language.GetString('AbstractionTotalErrorString');
          end
          else
          begin
            AbstractionGrid.ValidationError[3,1,gveCellField] := '';
            AbstractionGrid.ValidationError[0,1,gveCellField] := '';
            AbstractionGrid.ValidationError[1,1,gveCellField] := '';
            AbstractionGrid.ValidationError[2,1,gveCellField] := '';
          end;

          LFieldProperty := FAppModules.FieldProperties.FieldProperty('AbstractMonthTimeSeriesFile');
          AbstractionGrid.AddFieldProperty(LFieldProperty);
          AbstractionGrid.Cells[4,1] := AOpenCast.AbstractMonthTimeSeriesFile;

          AbstractionGrid.Enabled := AOpenCast.Abstraction;

        end
        else
        AbstractionGrid.Enabled := AOpenCast.Abstraction;

      end;
    end
    else
      PMiningOpenCastPitDialog.AbstractionGrid.Enabled := false;
  except on E: Exception do HandleError(E,OPNAME); end;
end;

procedure TPlanningMineOpenCastValidator.PopulateDataViewer;
const OPNAME = 'TPlanningMineOpenCastValidator.PopulateDataViewer';
begin
  inherited;
  try
    ClearDataViewer;
    RePopulateDataViewer;
    DoContextValidation(dvtResPropAll);
  except on E: Exception do HandleError(E,OPNAME) end;

end;

procedure TPlanningMineOpenCastValidator.PopulateGrowthFactorControl(AOpenCast: TPlanningOpenCast; AType: integer);
const OPNAME = 'TPlanningMineOpenCastValidator.PopulateGrowthFactorControl';
var
  LIndex: integer;
  LCount : integer;
  LGrowthFactor : TPlanningMineGrowthFactor;
  LFieldProperty      : TAbstractFieldProperty;
begin
  try
    if AOpenCast <> nil then
    begin
      with PMiningOpenCastPitDialog do
      begin
        ClearGrowthFactorGrid;
        GrowthFactorGrid.ClearErrors;
        GrowthFactorGrid.RowCount := Max(GrowthFactorGrid.FixedRows +1 ,AOpenCast.GrowthFactorCount+1);
        for LIndex := 0 to GrowthFactorGrid.ColCount - 1 do
        begin
          for LCount := 1 to GrowthFactorGrid.RowCount -1 do
          begin
            GrowthFactorGrid.Cells[LIndex,LCount] := '';
          end;
        end;


        //GrowthFactorGrid.RowCount := AOpenCast.GrowthFactorCount + 1;
        GrowthFactorGrid.Width := (3+(1+GrowthFactorGrid.DefaultColWidth)*GrowthFactorGrid.ColCount-1) + GrowthFactorGrid.ColWidths[0];
        GrowthFactorGrid.Height := 3 + (1 +  GrowthFactorGrid.DefaultRowHeight) *GrowthFactorGrid.RowCount;
        InterpolationCmBox.Visible := False;
        if AOpenCast.GrowthFactorCount >= 6 then BtnAddGrowthFactors.Enabled := False else BtnAddGrowthFactors.Enabled := true;
        if AOpenCast.GrowthFactorCount = 0 then BtnDeleteLoadGeneration.Enabled := False else BtnDeleteGrowthFactors.Enabled := true;


        for LIndex := 0 to AOpenCast.GrowthFactorCount-1 do
        begin
          LGrowthFactor := AOpenCast.GrowthFactorByIndex(LIndex);
          if LGrowthFactor <> nil then
          begin
            GrowthFactorControl.Visible    := True;

            GrowthFactorGrid.Cells[0,LIndex+1] := LGrowthFactor.Description;
            LFieldProperty := FAppModules.FieldProperties.FieldProperty('InterpolationMethod');
            GrowthFactorGrid.AddFieldProperty(LFieldProperty);
            GrowthFactorGrid.Cells[1,LIndex+1] := IntToStr(LGrowthFactor.InterpolationMethod);

            LFieldProperty := FAppModules.FieldProperties.FieldProperty('NoOfPoints');
            GrowthFactorGrid.AddFieldProperty(LFieldProperty);
            GrowthFactorGrid.Cells[2,LIndex+1] := IntToStr(LGrowthFactor.NoOfPoints);

            GrowthFactorGrid.Options := GrowthFactorGrid.Options + [goEditing];
            GrowthFactorGrid.ClearErrors;
            ValidateNoOfPoints(LGrowthFactor);
            ValidateInterpolationMedthod(LGrowthFactor);

            PopulateYearAndFactorGrid(LGrowthFactor);

          end;
        end;
      end;
    end;
  except on E: Exception do HandleError(E,OPNAME); end;
end;


procedure TPlanningMineOpenCastValidator.PopulateYearAndFactorGrid(AGrowthFactor : TPlanningMineGrowthFactor);
const OPNAME = 'TPlanningMineOpenCastValidator.PopulateYearAndFactorGrid';
var
  LIndex : integer;
  LControlCount,LMaxLeftPos: integer;
  LControl : TControl;
begin
  try
    if Assigned(AGrowthFactor) then
    begin
      with PMiningOpenCastPitDialog do
      begin
        YearAndFactorGrid.FixedCols := 1;
        YearAndFactorGrid.ColCount := AGrowthFactor.NoOfPoints + 1;
        YearAndFactorGrid.Width := (10 + (1 + YearAndFactorGrid.DefaultColWidth) * (YearAndFactorGrid.ColCount-1)) + YearAndFactorGrid.ColWidths[0];
        YearAndFactorGrid.Top := GrowthFactorGrid.Top + GrowthFactorGrid.Height + 10;
        YearAndFactorGrid.ClearErrors;
        for LIndex := 1 to AGrowthFactor.NoOfPoints do
        begin
          YearAndFactorGrid.Cells[0,1] := AGrowthFactor.Description;
          //YearAndFactorGrid.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('NYR'));
          YearAndFactorGrid.Cells[LIndex,0] := IntToStr(AGrowthFactor.NoOfYearsByIndex[LIndex - 1]);
          YearAndFactorGrid.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('GrowthFactors'));
          YearAndFactorGrid.Cells[LIndex,1] := Format(FAppModules.FieldProperties.FieldProperty('GrowthFactors').FormatStringGrid,[AGrowthFactor.GrowthFactorByIndex[LIndex-1]]);
          YearAndFactorGrid.Options := YearAndFactorGrid.Options + [goEditing];
        end;
        LMaxLeftPos := 0;
        for LControlCount := 0 to GrowthFactorControl.ControlCount -1 do
        begin
          LControl := GrowthFactorControl.Controls[LControlCount];
          LMaxLeftPos := Max(LMaxLeftPos,LControl.Left + LControl.Width);
        end;
        GrowthFactorControl.Width := LMaxLeftPos + 10;
        GrowthFactorControl.Height     := 290;
                //ValidateNoOfYears(AGrowthFactor);
        DoContextValidation(dvtNYR);
        //ValidateGrowthFactor(AGrowthFactor);
        DoContextValidation(dvtGrowthFactor);
      end;
    end
    else
    begin
      PMiningOpenCastPitDialog.YearAndFactorGrid.ColCount := 2;
      PMiningOpenCastPitDialog.YearAndFactorGrid.Cells[1,0] := '';
      PMiningOpenCastPitDialog.YearAndFactorGrid.Cells[1,1] := '';
    end;
  except on E: Exception do HandleError(E,OPNAME); end;
end;

procedure TPlanningMineOpenCastValidator.PopulateLoadGenerationControl(AOpenCast: TPlanningOpenCast; AType: integer);
const OPNAME = 'TPlanningMineOpenCastValidator.PopulateLoadGenerationControl';
var
  LLoadGeneration :  TLoadGeneration;
  LCount : integer;
begin
  try
    if AOpenCast <> nil then
    begin
      with PMiningOpenCastPitDialog do
      begin
        LoadGenerationCombo.Clear;
        ClearFactorAndMeanGrid;
         StdDeviationEdit.Text := '';
        if AOpenCast.LoadGenerationCount >= 2 then
        begin
          BtnAddLoadGeneration.Enabled := False
        end
        else
          BtnAddLoadGeneration.Enabled := True;

        if AOpenCast.LoadGenerationCount > 0 then
          BtnDeleteLoadGeneration.Enabled := True
        else
        BtnDeleteLoadGeneration.Enabled := False;
        for  LCount := 1 to AOpenCast.LoadGenerationCount + 1 do
        begin
          LLoadGeneration := AOpenCast.LoadGenerationByType(LCount);
          if LLoadGeneration <> nil then
          begin
            LoadGenerationCombo.AddItem(LLoadGeneration.Description,TObject(LLoadGeneration.type_));
          end;
        end;

        if AType > 2 then Exit;

        LLoadGeneration := AOpenCast.LoadGenerationByType(AType);
        if LLoadGeneration <> nil then
        begin
          LoadGenerationCombo.SetFieldIndex(LoadGenerationCombo.Items.IndexOfObject(TObject(LLoadGeneration.type_)));
          StdDeviationEdit.Text :=  FloatToStr(LLoadGeneration.StandardDeviation); //FloatToStrF(LLoadGeneration.StandardDeviation,ffNumber,5,2);

          case AType of
          1:
            begin
              FactorAndMeanGrid.Cells[0,0] := FAppModules.Language.GetString('FactorsAndMeanGrid.OpenCastTypeFlow1');
              FactorAndMeanGrid.Cells[1,0] := FAppModules.Language.GetString('FactorsAndMeanGrid.OpenCastTypeMean1');
            end;
          2:
            begin
              FactorAndMeanGrid.Cells[0,0] := FAppModules.Language.GetString('FactorsAndMeanGrid.OpenCastTypeFlow2');
              FactorAndMeanGrid.Cells[1,0] := FAppModules.Language.GetString('FactorsAndMeanGrid.OpenCastTypeMean2');
            end;
          end;
          for LCount := 0 to 9 do
          begin
            FactorAndMeanGrid.Cells[0,LCount+1] := Format(FAppModules.FieldProperties.FieldProperty('Flow').FormatStringGrid,[LLoadGeneration.FlowByIndex[LCount]]);//FloatToStrF(LLoadGeneration.FlowByIndex[LCount],ffNumber,5,2);
            FactorAndMeanGrid.Cells[1,LCount+1] := Format(FAppModules.FieldProperties.FieldProperty('MeanOfSalt').FormatStringGrid,[LLoadGeneration.MeanOfSaltByIndex[LCount]]);//loatToStrF(LLoadGeneration.MeanOfSaltByIndex[LCount],ffNumber,5,2);
          end;
          LoadGenerationControl.Visible := True;
          ValidateStandardDeviation(AOpenCast.LoadGenerationByType(AType));
          ValidateFlow(AOpenCast.LoadGenerationByType(AType));
          ValidateMeanOfSalt(AOpenCast.LoadGenerationByType(AType));
          end
        else
          PopulateLoadGenerationControl(AOpenCast,AType +1);
      end;

    end;
  except on E: Exception do HandleError(E,OPNAME); end;

end;

procedure TPlanningMineOpenCastValidator.refreshGrowthControl(Sender: TObject);
const OPNAME = 'TPlanningMineOpenCastValidator.refreshGrowthControl';
var
  LMine : TPlanningMine;
  LOpenCast : TPlanningOpenCast;
 // LSelectedType : integer;
begin
  try
    LMine := TPlanningMine(TPlanningModelDataObject(FAppModules.Model.ModelData).CastNetworkFeaturesData.CastMineList.CastMinebyNodeNumber[FIdentifier]);
    if LMine <> nil then
    begin
      LOpenCast := LMine.CastOpenCastByID[FSelectOpenCastID];
      if LOpenCast <> nil then
      begin

        //LSelectedType := MiningOpenCastPitDialog.GrowthFactorControl.FactorTypeCombo.ItemIndex;
        //LSelectedType := Integer(MiningOpenCastPitDialog.GrowthFactorControl.FactorTypeCombo.Items.Objects[LSelectedType]);
        //LSelectedTypeString := MiningOpenCastPitDialog.GrowthFactorControl.FactorTypeCombo.Items[LSelectedType];
        //LSelectedType := MiningOpenCastPitDialog.GrowthFactorControl.FactorTypeCombo.Items.

        PopulateGrowthFactorControl(LOpenCast);
      end;
    end;
  except on E: Exception do HandleError(E,OPNAME); end;
end;

procedure TPlanningMineOpenCastValidator.refreshLoadGenerationControl(
  Sender: TObject);
const OPNAME = 'TPlanningMineOpenCastValidator.refreshLoadGenerationControl';
var
  LMine : TPlanningMine;
  LOpenCast : TPlanningOpenCast;
  LSelectedType : integer;
begin
  try
    LMine := TPlanningMine(TPlanningModelDataObject(FAppModules.Model.ModelData).CastNetworkFeaturesData.CastMineList.
    CastMinebyNodeNumber[FIdentifier]);
    LOpenCast := LMine.CastOpenCastByID[FSelectOpenCastID];
    if LOpenCast <> nil then
    begin
      LSelectedType := Integer(PMiningOpenCastPitDialog.LoadGenerationCombo.Items.Objects[PMiningOpenCastPitDialog.LoadGenerationCombo.ItemIndex]);
      PopulateLoadGenerationControl(LOpenCast,LSelectedType);
    end;
  except on E: Exception do HandleError(E,OPNAME); end;

end;

procedure TPlanningMineOpenCastValidator.ValidateStandardDeviation(ALoadGeneration : TLoadGeneration);
const OPNAME = 'TPlanningMineOpenCastValidator.ValidateStandardDeviation';
begin
  try
    with PMiningOpenCastPitDialog do
    begin
    if ALoadGeneration <> nil then
    begin
      FErrorMessage := '';
      ALoadGeneration.Validate(FErrorMessage,'StdDeviation');
      //GrowthFactorGrid.ValidationError[GrowthFactorGrid.Col, GrowthFactorGrid.Row,gveCellField] := FErrorMessage;
      StdDeviationEdit.FieldValidationError := FErrorMessage;
    end;
  end;
  except on E: Exception do HandleError(E,OPNAME); end;
end;


procedure TPlanningMineOpenCastValidator.ValidateFlow(ALoadGeneration : TLoadGeneration);
const OPNAME = 'TPlanningMineOpenCastValidator.ValidateFlow';
var
  LErrorCols  : TStringList;
  LErrorMsgs  : TStringList;
  //LFieldProperties: TAbstractFieldProperty;
  LIndex,
  LRow : integer;
begin
  try
    LErrorCols  := TStringList.Create;
    LErrorMsgs  := TStringList.Create;
    //LFieldProperties := FAppModules.FieldProperties.FieldProperty('NYR');
    try
      if ALoadGeneration <> nil then
      begin
        if ALoadGeneration.Validate(FErrorMessage,'Flow') then
          for LRow := 1 to 10 do
            PMiningOpenCastPitDialog.FactorAndMeanGrid.ValidationError[0, LRow, gveCellContext] := ''
        else
        begin
          ExtractErrorsAndColumns(FErrorMessage, LErrorMsgs, LErrorCols);
          for LRow := 1 to 10 do
          begin
            LIndex := LErrorCols.IndexOf(IntToStr(LRow));
            if (LIndex >= 0) then
              PMiningOpenCastPitDialog.FactorAndMeanGrid.ValidationError[0, LRow, gveCellContext] := LErrorMsgs.Strings[lIndex]
            else
              PMiningOpenCastPitDialog.FactorAndMeanGrid.ValidationError[0, LRow, gveCellContext] := '';
          end;
        end;
      end;
   finally
     FreeAndNil(LErrorCols);
     FreeAndNil(LErrorMsgs);
   end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TPlanningMineOpenCastValidator.ValidateMeanOfSalt(ALoadGeneration : TLoadGeneration);
const OPNAME = 'TPlanningMineOpenCastValidator.ValidateMeanOfSalt';
var
  LErrorCols  : TStringList;
  LErrorMsgs  : TStringList;
  //LFieldProperties: TAbstractFieldProperty;
  LIndex,
  LRow : integer;
begin
  try
    LErrorCols  := TStringList.Create;
    LErrorMsgs  := TStringList.Create;
    //LFieldProperties := FAppModules.FieldProperties.FieldProperty('NYR');
    try
      if ALoadGeneration <> nil then
      begin
        if ALoadGeneration.Validate(FErrorMessage,'Flow') then
          for LRow := 1 to 10 do
            PMiningOpenCastPitDialog.FactorAndMeanGrid.ValidationError[0, LRow, gveCellContext] := ''
        else
        begin
          ExtractErrorsAndColumns(FErrorMessage, LErrorMsgs, LErrorCols);
          for LRow := 1 to 10 do
          begin
            LIndex := LErrorCols.IndexOf(IntToStr(LRow));
            if (LIndex >= 0) then
              PMiningOpenCastPitDialog.FactorAndMeanGrid.ValidationError[0, LRow, gveCellContext] := LErrorMsgs.Strings[lIndex]
            else
              PMiningOpenCastPitDialog.FactorAndMeanGrid.ValidationError[0, LRow, gveCellContext] := '';
          end;
        end;
      end;
   finally
     FreeAndNil(LErrorCols);
     FreeAndNil(LErrorMsgs);
   end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


end.
