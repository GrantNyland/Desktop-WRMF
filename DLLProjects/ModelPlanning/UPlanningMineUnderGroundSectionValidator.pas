unit UPlanningMineUnderGroundSectionValidator;

interface

uses
  Classes,
  UMiningUnderGroundSectionValidator,
  UPlanningUnderGroundMine,
  UPlanningMineUnderGroundSectionDialog,
  UPlanningMineGrowthFactor,
  UAbstractObject,
  ULoadGeneration;

type
  TPlanningMineUnderGroundSectionValidator = class
    (TMiningUnderGroundSectionValidator)
  protected
    procedure CreateMemberObjects; override;
    procedure OnStringGridCellDataHasChanged(ASender: TObject;
      ACol, ARow: integer); override;
    procedure OnGridSelectCell(ASender: TObject; ACol, ARow: integer;
      var CanSelect: Boolean);
    procedure OnEditControlEnter(Sender: TObject); override;
    procedure OnEditControltExit(Sender: TObject); override;
    procedure OnInsertUndergroundSection(Sender: TObject);
    procedure OnDeleteUndergroundSection(Sender: TObject);
    procedure refreshGrowthControl(Sender: TObject);
    procedure refreshLoadGenerationControl(Sender: TObject);
    procedure PopulateGrowthFactorControl(AUnderGroundSection
      : TPlanningUnderGroundMine; AType: integer = 8);
    procedure PopulateLoadGenerationControl(AUnderGroundSection
      : TPlanningUnderGroundMine);
    procedure RePopulateDataViewer;
    procedure ClearGrowthControl;
    procedure ClearLoadGenerationControl;

    procedure UpdateIntepolationMethod;
    procedure UpdateYearDataPoints;
    procedure UpDateGrowthFactor(ACol, ARow: integer);
    procedure UpDateNoOfYears(ACol, ARow: integer);

    procedure ValidateNoOfPoints(AGrowthFactor: TPlanningMineGrowthFactor);
    procedure ValidateInterpolationMedthod(AGrowthFactor
      : TPlanningMineGrowthFactor);
    procedure ValidateGrowthFactor(AGrowthFactor: TPlanningMineGrowthFactor);
    procedure ValidateNoOfYears(AGrowthFactor: TPlanningMineGrowthFactor);

    procedure UpdateStandardDeviation;
    procedure ValidateStandardDeviation(ALoadGeneration: TLoadGeneration);
    procedure UpdateFlow(ACol, ARow: integer);
    procedure ValidateFlow(ALoadGeneration: TLoadGeneration);
    procedure UpDateMeanOfSalt(ACol, ARow: integer);
    procedure ValidateMeanOfSalt(ALoadGeneration : TLoadGeneration);

    procedure AddLoadGeneration(ASender: TObject);
    procedure DeleteLoadGeneration(ASender: TObject);
    procedure AddGrowthFactor(ASender: TObject);
    procedure DeleteGrowthFactor(ASender: TObject);

  public
    function MiningUnderGroundSectionDialog
      : TPlanningMineUnderGroundSectionDialog;
    procedure ClearDataViewer; override;
    procedure PopulateDataViewer; override;
    procedure DoContextValidation(AValidationType: integer); override;
    function Initialise: Boolean; override;

  end;

implementation

uses
  SysUtils,
  VCL.Grids,
  Math,
  UPlanningModelManager,
  UPlanningModelDataObject,
  UPlanningMineData,
  UYieldContextValidationType,
  UDataEditComponent,
  UUtilities,
  // UPlanningMineGrowthFactor,

  VoaimsCom_TLB,
  UErrorHandlingOperations;

{ TPlanningMineUnderGroundSectionValidator }

procedure TPlanningMineUnderGroundSectionValidator.AddGrowthFactor
  (ASender: TObject);
const
  OPNAME = 'TPlanningMineUnderGroundSectionValidator.AddGrowthFactor';
var
  LMine: TPlanningMine;
  LUnderGroundMine: TPlanningUnderGroundMine;
  LCount: integer;
begin
  try
    with MiningUnderGroundSectionDialog do
    begin
      if (ASender = BtnAddGrowthFactors) then
      begin
        LMine := TPlanningMine
          (TPlanningModelDataObject(FAppModules.Model.ModelData)
          .CastNetworkFeaturesData.CastMineList.MineByNodeNumber[FIdentifier]);
        if LMine <> nil then
        begin
          if FSelectUGIdentifier <> -1 then
          begin
            LUnderGroundMine := LMine.CastUnderGroundByID[FSelectUGIdentifier];
            if LUnderGroundMine <> nil then
            begin
              for LCount := 0 to LUnderGroundMine.AcceptedGrowthTypes.
                Count - 1 do
              begin

                if LUnderGroundMine.GrowthFactorByType
                  (LUnderGroundMine.AcceptedGrowthTypes[LCount]) = nil then
                begin
                  LUnderGroundMine.CreateGrowthFactor
                    (LUnderGroundMine.AcceptedGrowthTypes[LCount]);
                  PopulateGrowthFactorControl(LUnderGroundMine);
                  break;
                end;
              end;
            end;
          end;
        end;
      end;
    end;
  except
    on E: Exception do
      HandleError(E, OPNAME);
  end;

end;

procedure TPlanningMineUnderGroundSectionValidator.AddLoadGeneration
  (ASender: TObject);
const
  OPNAME = 'TPlanningMineOpenCastValidator.AddGrowthFactor';
var
  LMine: TPlanningMine;
  LUnderGroundMine: TPlanningUnderGroundMine;
begin
  try
    with MiningUnderGroundSectionDialog do
    begin
      if (ASender = BtnAddLoadGeneration) then
      begin
        LMine := TPlanningMine
          (TPlanningModelDataObject(FAppModules.Model.ModelData)
          .CastNetworkFeaturesData.CastMineList.MineByNodeNumber[FIdentifier]);
        if LMine <> nil then
        begin
          if FSelectUGIdentifier <> -1 then
          begin
            LUnderGroundMine := LMine.CastUnderGroundByID[FSelectUGIdentifier];
            if LUnderGroundMine <> nil then
            begin
              LUnderGroundMine.CreateLoadGeneration;
              PopulateLoadGenerationControl(LUnderGroundMine);
            end;
          end;
        end;
      end;
    end;

  except
    on E: Exception do
      HandleError(E, OPNAME);
  end;

end;

procedure TPlanningMineUnderGroundSectionValidator.ClearDataViewer;
const
  OPNAME = 'TPlanningMineUnderGroundSectionValidator.ClearDataViewer';
begin
  try
    with MiningUnderGroundSectionDialog do
    begin
      FactorTypeCombo.Clear;
      YearAndFactorGrid.ColWidths[0] :=
        MiningUnderGroundSectionDialog.YearAndFactorGrid.DefaultColWidth * 5;
      InterpolationCheckBox.Items.Clear;
      InterpolationCheckBox.Items.AddObject('1 - Linear Interpolation',
        TObject(1));
      InterpolationCheckBox.Items.AddObject('2 - Exponential interpolation',
        TObject(2));
      YearAndFactorGrid.FixedRows := 0;

      FactorAndMeanGrid.Cells[0, 0] := FAppModules.Language.GetString
        ('UndergroundFlow');
      FactorAndMeanGrid.Cells[1, 0] := FAppModules.Language.GetString
        ('UndergroundMean');

      FactorAndMeanGrid.FixedCols := 0;
      FactorAndMeanGrid.FixedRows := 1;

      FactorAndMeanGrid.ColCount := 2;
      FactorAndMeanGrid.RowCount := 11;

      FactorAndMeanGrid.Height :=
        (FactorAndMeanGrid.DefaultRowHeight * FactorAndMeanGrid.RowCount) + 30;
      FactorAndMeanGrid.Width := 10 + (1 + FactorAndMeanGrid.DefaultColWidth *
        FactorAndMeanGrid.ColCount);
      YearAndFactorGrid.ColWidths[0] := YearAndFactorGrid.DefaultColWidth * 4;
      YearAndFactorGrid.FixedCols := 0;
      YearAndFactorGrid.FixedRows := 1;
      YearAndFactorGrid.ColCount := 2;

      YearAndFactorGrid.Width := (10 + (1 + YearAndFactorGrid.DefaultColWidth) *
        (YearAndFactorGrid.ColCount - 1)) + YearAndFactorGrid.ColWidths[0];
      YearAndFactorGrid.Cells[0, 0] := FAppModules.Language.GetString
        ('FYearAndFactorGrid.YearCol');
    end;
  except
    on E: Exception do
      HandleError(E, OPNAME);
  end;
end;

procedure TPlanningMineUnderGroundSectionValidator.ClearGrowthControl;
const
  OPNAME = 'TPlanningMineUnderGroundSectionValidator.ClearGrowthControl';
var
  LCount: integer;
begin
  try
    with MiningUnderGroundSectionDialog do
    begin
      FactorTypeCombo.Clear;
      NoOfPointsEdit.Text := '';
      NoOfPointsEdit.FieldValidationError := '';
      YearAndFactorGrid.ClearErrors;
      for LCount := 1 to YearAndFactorGrid.ColCount - 1 do
      begin
        YearAndFactorGrid.Cells[LCount, 0] := '';
        YearAndFactorGrid.Cells[LCount, 1] := '';
      end;

    end;
  except
    on E: Exception do
      HandleError(E, OPNAME);
  end;
end;

procedure TPlanningMineUnderGroundSectionValidator.ClearLoadGenerationControl;
const
  OPNAME = 'TPlanningMineUnderGroundSectionValidator.ClearLoadGenerationControl';
var
  LCount : integer;
  lFieldProperty : TAbstractFieldProperty;
begin
  try
    with MiningUnderGroundSectionDialog do
    begin
      LoadGenerationCombo.Clear;
      StdDeviationEdit.Text := '';
      StdDeviationEdit.FieldValidationError := '';
      lFieldProperty := FAppModules.FieldProperties.FieldProperty('MeanOfSalt');
      for LCount := lFieldProperty.ArrayLow to lFieldProperty.ArrayHigh do
      begin
        FactorAndMeanGrid.Cells[0,LCount] := '';
        FactorAndMeanGrid.ValidationError[0,LCount,gveCellField] := '';
        FactorAndMeanGrid.Cells[1,LCount] := '';
        FactorAndMeanGrid.ValidationError[1,LCount,gveCellField] := '';
      end;
    end;
  except
    on E: Exception do
      HandleError(E, OPNAME);
  end;
end;

procedure TPlanningMineUnderGroundSectionValidator.CreateMemberObjects;
const
  OPNAME = 'TPlanningMineUnderGroundSectionValidator.CreateMemberObjects';
begin
  inherited;
  try
    FIdentifier := -1;
    FPanel := TPlanningMineUnderGroundSectionDialog.Create(FPanelOwner,
      FAppModules);
    with MiningUnderGroundSectionDialog do
    begin
      NrOfUnderGroundMiningEdt.FieldProperty :=
        FAppModules.FieldProperties.FieldProperty('NrOfUnderGroundMining');
      NrOfUnderGroundMiningEdt.OnEnter := OnEditControlEnter;
      NrOfUnderGroundMiningEdt.OnExit := OnEditControltExit;

      InsertUndergroundBtn.OnClick := OnInsertUndergroundSection;
      DeleteUndergroundSlurryBtn.OnClick := OnDeleteUndergroundSection;

      UnderGroundMiningGrid.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('NrOfUnderGroundMining'));
      UnderGroundMiningGrid.AddFieldProperty
        (FAppModules.FieldProperties.FieldProperty('ChannelNumberToUGDam'));
      UnderGroundMiningGrid.AddFieldProperty
        (FAppModules.FieldProperties.FieldProperty('UndergroundSectionName'));
      UnderGroundMiningGrid.AddFieldProperty
        (FAppModules.FieldProperties.FieldProperty('UpstreamCatchmentArea'));
      UnderGroundMiningGrid.AddFieldProperty
        (FAppModules.FieldProperties.FieldProperty('BoardPillarCatchmentArea'));
      UnderGroundMiningGrid.AddFieldProperty
        (FAppModules.FieldProperties.FieldProperty
        ('HighExtractionCatchmentArea'));
      UnderGroundMiningGrid.AddFieldProperty
        (FAppModules.FieldProperties.FieldProperty
        ('HighExtractionAreaRunoffFactor'));
      UnderGroundMiningGrid.OnBeforeCellChange :=
        OnStringGridCellDataHasChanged;
      UnderGroundMiningGrid.OnSelectCell := OnGridSelectCell;
      UnderGroundMiningGrid.OnColEnter := OnStringGridColEnter;
      UnderGroundMiningGrid.OnExit := OnEditControltExit;
      UnderGroundMiningGrid.OnEnter := OnEditControlEnter;

      UnderGroundMiningGrid.ButtonColumn[7] := True;
      UnderGroundMiningGrid.ButtonColumnCaption[7] :=
        FAppModules.Language.GetString('LabelText.ThreeDots');
      UnderGroundMiningGrid.ButtonColumnOnClick[7] :=
        OnMonthlyRechargeFactorsClick;

      UnderGroundMiningGrid.ButtonColumn[8] := True;
      UnderGroundMiningGrid.ButtonColumnCaption[8] :=
        FAppModules.Language.GetString('LabelText.ThreeDots');
      UnderGroundMiningGrid.ButtonColumnOnClick[8] :=
        OnMonthlyRechargeFactorsClick;

      UnderGroundMiningGrid.ButtonColumn[9] := True;
      UnderGroundMiningGrid.ButtonColumnCaption[9] :=
        FAppModules.Language.GetString('LabelText.ThreeDots');
      UnderGroundMiningGrid.ButtonColumnOnClick[9] :=
        OnMonthlyRechargeFactorsClick;

      BtnAddGrowthFactors.OnClick := AddGrowthFactor;
      BtnDeleteGrowthFactors.OnClick := DeleteGrowthFactor;

      FactorTypeCombo.OnSelect := refreshGrowthControl;
      NoOfPointsEdit.FieldProperty := FAppModules.FieldProperties.FieldProperty
        ('NoOfPoints');
      NoOfPointsEdit.OnEnter := OnEditControlEnter;
      NoOfPointsEdit.OnExit := OnEditControltExit;
      InterpolationCheckBox.FieldProperty :=  FAppModules.FieldProperties.FieldProperty
        ('InterpolationMethod');
      InterpolationCheckBox.OnEnter := OnEditControlEnter;


      InterpolationCheckBox.OnSelect := OnEditControltExit;
      YearAndFactorGrid.OnEnter := OnEditControlEnter;
      YearAndFactorGrid.OnBeforeCellChange := OnStringGridCellDataHasChanged;

      BtnAddLoadGeneration.OnClick := AddLoadGeneration;
      BtnDeleteLoadGeneration.OnClick := DeleteLoadGeneration;

      StdDeviationEdit.FieldProperty :=
        FAppModules.FieldProperties.FieldProperty('StdDeviation');
      StdDeviationEdit.OnEnter := OnEditControlEnter;
      StdDeviationEdit.OnExit := OnEditControltExit;
      FactorAndMeanGrid.OnBeforeCellChange := OnStringGridCellDataHasChanged;
      FactorAndMeanGrid.AddFieldProperty
        (FAppModules.FieldProperties.FieldProperty('Flow'));
      FactorAndMeanGrid.AddFieldProperty
        (FAppModules.FieldProperties.FieldProperty('MeanOfSalt'));
       FactorAndMeanGrid.OnEnter  := OnEditControlEnter;
       FactorAndMeanGrid.OnColEnter := OnStringGridColEnter;
      InterpolationCheckBox.OnEnter := OnEditControlEnter;


      YearAndFactorGrid.AddFieldProperty
        (FAppModules.FieldProperties.FieldProperty('NYR'));

      LoadGenerationCombo.OnSelect := refreshLoadGenerationControl;

    end;
  except
    on E: Exception do
      HandleError(E, OPNAME);
  end;

end;

procedure TPlanningMineUnderGroundSectionValidator.DeleteGrowthFactor
  (ASender: TObject);
const
  OPNAME = 'TPlanningMineUnderGroundSectionValidator.DeleteGrowthFactor';
var
  LMine: TPlanningMine;
  LUnderGroundMine: TPlanningUnderGroundMine;
begin
  try
    with MiningUnderGroundSectionDialog do
    begin
      if (ASender = BtnDeleteGrowthFactors) then
      begin
        LMine := TPlanningMine
          (TPlanningModelDataObject(FAppModules.Model.ModelData)
          .CastNetworkFeaturesData.CastMineList.CastMineByNodeNumber
          [FIdentifier]);
        if LMine <> nil then
        begin
          if FSelectUGIdentifier <> -1 then
          begin
            LUnderGroundMine := LMine.CastUnderGroundByID[FSelectUGIdentifier];
            if LUnderGroundMine <> nil then
            begin
              LUnderGroundMine.DeleteGrowthFactorByType
                (integer(FactorTypeCombo.Items.Objects
                [FactorTypeCombo.ItemIndex]));
              PopulateGrowthFactorControl(LUnderGroundMine);
            end;
          end;
        end;
      end;
    end;

  except
    on E: Exception do
      HandleError(E, OPNAME);
  end;
end;

procedure TPlanningMineUnderGroundSectionValidator.DeleteLoadGeneration
  (ASender: TObject);
const
  OPNAME = 'TPlanningMineOpenCastValidator.DeleteLoadGeneration';
var
  LMine: TPlanningMine;
  LUnderGroundMine: TPlanningUnderGroundMine;
begin
  try
    with MiningUnderGroundSectionDialog do
    begin
      if (ASender = BtnDeleteLoadGeneration) then
      begin
        LMine := TPlanningMine
          (TPlanningModelDataObject(FAppModules.Model.ModelData)
          .CastNetworkFeaturesData.CastMineList.CastMineByNodeNumber
          [FIdentifier]);
        if LMine <> nil then
        begin
          if FSelectUGIdentifier <> -1 then
          begin
            LUnderGroundMine := LMine.CastUnderGroundByID[FSelectUGIdentifier];
            if LUnderGroundMine <> nil then
            begin
              LUnderGroundMine.DeleteLoadGeneration;
              PopulateLoadGenerationControl(LUnderGroundMine);
            end;
          end;

        end;
      end;
    end;

  except
    on E: Exception do
      HandleError(E, OPNAME);
  end;
end;

procedure TPlanningMineUnderGroundSectionValidator.DoContextValidation
  (AValidationType: integer);
const
  OPNAME = 'TPlanningMineUnderGroundSectionValidator.DoContextValidation';
var
  LMine: TPlanningMine;
  LUnderGroundMine: TPlanningUnderGroundMine;
  LGrowth: TPlanningMineGrowthFactor;
  LLoadGeneration: TLoadGeneration;
begin
  try
    inherited;
    with MiningUnderGroundSectionDialog do
    begin
      LMine := TPlanningMine
        (TPlanningModelDataObject(FAppModules.Model.ModelData)
        .CastNetworkFeaturesData.CastMineList.CastMineByNodeNumber
        [FIdentifier]);
      if LMine <> nil then
      begin
        LUnderGroundMine := LMine.CastUnderGroundByID[FSelectUGIdentifier];
        if LUnderGroundMine <> nil then
        begin
          if FactorTypeCombo.Items.Count > 0 then
            LGrowth := LUnderGroundMine.GrowthFactorByType
              (integer(FactorTypeCombo.Items.Objects
              [FactorTypeCombo.ItemIndex]))
          else
            LGrowth := nil;
          LLoadGeneration := LUnderGroundMine.LoadGeneration;
          if ((LGrowth <> nil) or (LLoadGeneration <> nil)) then
          begin

            case AValidationType of

              dvtNoOfPoints:
                ValidateNoOfPoints(LGrowth);
              dvtInterpolationMethod:
                ValidateInterpolationMedthod(LGrowth);
              dvtNYR:
                ValidateNoOfYears(LGrowth);
              dvtGrowthFactor:
                ValidateGrowthFactor(LGrowth);
              dvtStandardDeviation:
                ValidateStandardDeviation(LLoadGeneration);
              dvtFlow: ValidateFlow(LLoadGeneration);
              dvtMeanOfSalt : ValidateMeanOfSalt(LLoadGeneration);

              dvtResPropAll:
                begin
                  ValidateNoOfPoints(LGrowth);
                  ValidateInterpolationMedthod(LGrowth);
                  ValidateGrowthFactor(LGrowth);
                  ValidateNoOfYears(LGrowth);
                end;
            end;
          end;
        end;
      end;
    end;
  except
    on E: Exception do
      HandleError(E, OPNAME);
  end;
end;

function TPlanningMineUnderGroundSectionValidator.Initialise: Boolean;
const
  OPNAME = 'TChannelGrowthValidator.Initialise';
begin
  Result := False;
  try
    with MiningUnderGroundSectionDialog do
    begin
      InterpolationCheckBox.Clear;
      InterpolationCheckBox.Items.AddObject('1 - Linear Interpolation',
        TObject(1));
      InterpolationCheckBox.Items.AddObject('2 - Exponential Interpolation',
        TObject(2));

      YearAndFactorGrid.FixedRows := 0;
      YearAndFactorGrid.FixedCols := 1;
      YearAndFactorGrid.ColCount := 2;
      YearAndFactorGrid.RowCount := 2;

      YearAndFactorGrid.ColWidths[0] := YearAndFactorGrid.DefaultColWidth * 5;
      YearAndFactorGrid.Cells[0, 0] := FAppModules.Language.GetString
        ('FYearAndFactorGrid.YearCol');
      // YearAndFactorGrid.Cells[0,1] := FAppModules.Language.GetString('FYearAndFactorGrid.GrowthFactorMine');
    end;
    Result := FPanel.Initialise;
  except
    on E: Exception do
      HandleError(E, OPNAME)
  end;
end;

function TPlanningMineUnderGroundSectionValidator.MiningUnderGroundSectionDialog
  : TPlanningMineUnderGroundSectionDialog;
const
  OPNAME = 'TPlanningMineUnderGroundSectionValidator.MiningUnderGroundSectionDialog';
begin
  Result := nil;
  try
    if Assigned(FPanel) then
      Result := TPlanningMineUnderGroundSectionDialog(FPanel);
  except
    on E: Exception do
      HandleError(E, OPNAME)
  end;
end;

procedure TPlanningMineUnderGroundSectionValidator.OnDeleteUndergroundSection
  (Sender: TObject);
begin
  inherited;

end;

procedure TPlanningMineUnderGroundSectionValidator.OnEditControlEnter
  (Sender: TObject);
const
  OPNAME = 'TPlanningMineUnderGroundSectionValidator.OnEditControltExit';
begin
  try
    inherited OnEditControlEnter(Sender);
  except
    on E: Exception do
      HandleError(E, OPNAME);
  end;
end;

procedure TPlanningMineUnderGroundSectionValidator.OnEditControltExit
  (Sender: TObject);
const
  OPNAME = 'TPlanningMineUnderGroundSectionValidator.OnEditControltExit';
begin
  inherited;
  try
    if (Sender = MiningUnderGroundSectionDialog.InterpolationCheckBox) then
      UpdateIntepolationMethod
    else if (Sender = MiningUnderGroundSectionDialog.NoOfPointsEdit) then
      UpdateYearDataPoints
    else if (Sender = MiningUnderGroundSectionDialog.StdDeviationEdit) then
      UpdateStandardDeviation;

  except
    on E: Exception do
      HandleError(E, OPNAME);
  end;
end;

procedure TPlanningMineUnderGroundSectionValidator.OnGridSelectCell
  (ASender: TObject; ACol, ARow: integer; var CanSelect: Boolean);
const
  OPNAME = 'TPlanningMineUnderGroundSectionValidator.OnGridSelectCell';
var
  LMine: TPlanningMine;
  LUnderGroundMine: TPlanningUnderGroundMine;
  // LInterpolation :String;
  // LGrowth : TPlanningMineGrowthFactor;
begin
  inherited;
  try
    with MiningUnderGroundSectionDialog do
    begin
      if (CanSelect) and (ARow > 0) then
      begin
        LMine := TPlanningMine
          (TPlanningModelDataObject(FAppModules.Model.ModelData)
          .CastNetworkFeaturesData.CastMineList.MineByNodeNumber[FIdentifier]);
        if LMine <> nil then
        begin
          if FSelectUGIdentifier <> -1 then
          begin
            LUnderGroundMine := LMine.CastUnderGroundByID[FSelectUGIdentifier];
            if LUnderGroundMine <> nil then
            begin
              PopulateGrowthFactorControl(LUnderGroundMine);
              PopulateLoadGenerationControl(LUnderGroundMine);
            end;
          end;
        end;
      end;

    end;
  except
    on E: Exception do
      HandleError(E, OPNAME);
  end;

end;

procedure TPlanningMineUnderGroundSectionValidator.OnInsertUndergroundSection
  (Sender: TObject);
const
  OPNAME = 'TPlanningMineUnderGroundSectionValidator.OnInsertUndergroundSection';
begin
  try
    if TPlanningModelManager(FAppModules.Model).DoCreateUnderGround(FIdentifier)
      <> nil then
    begin
      RePopulateDataViewer;
    end;

  except
    on E: Exception do
      HandleError(E, OPNAME);
  end;
end;

procedure TPlanningMineUnderGroundSectionValidator.
  OnStringGridCellDataHasChanged(ASender: TObject; ACol, ARow: integer);
const
  OPNAME = 'TPlanningMineUnderGroundSectionValidator.OnStringGridCellDataHasChanged';
begin
  inherited;
  try
    with MiningUnderGroundSectionDialog do
    begin
      if (ASender = YearAndFactorGrid) then
      begin
        if (ACol > 0) and (ARow = 1) then
          UpDateGrowthFactor(ACol, ARow)
        else if (ACol > 0) and (ARow = 0) then
          UpDateNoOfYears(ACol, ARow);
      end
      else if ASender = FactorAndMeanGrid then
           begin
             if (ACol = 0) then UpdateFlow(ACol,ARow)
             else if (ACol = 1) then UpDateMeanOfSalt(ACol,ARow);
           end;
    end;
  except
    on E: Exception do
      HandleError(E, OPNAME);
  end;
end;

procedure TPlanningMineUnderGroundSectionValidator.PopulateDataViewer;
const
  OPNAME = 'TPlanningMineUnderGroundSectionValidator.PopulateDataViewer';
begin
  inherited;
  try
    ClearDataViewer;
    RePopulateDataViewer;
    DoContextValidation(dvtResPropAll);
  except
    on E: Exception do
      HandleError(E, OPNAME);
  end;
end;

procedure TPlanningMineUnderGroundSectionValidator.PopulateGrowthFactorControl
  (AUnderGroundSection: TPlanningUnderGroundMine; AType: integer);
const
  OPNAME = 'TPlanningMineUnderGroundSectionValidator.PopulateGrowthFactorControl';
var
  LGrowthFactor: TPlanningMineGrowthFactor;
  LIndex: integer;
begin
  try
    if AUnderGroundSection <> nil then
    begin
      with MiningUnderGroundSectionDialog do
      begin
        if AUnderGroundSection.GrowthFactorCount = 2 then
        begin
          BtnAddGrowthFactors.Enabled := False;
          BtnDeleteGrowthFactors.Enabled := True;
        end
        else if AUnderGroundSection.GrowthFactorCount = 1 then
        begin
          BtnAddGrowthFactors.Enabled := True;
          BtnDeleteGrowthFactors.Enabled := True;
        end
        else if AUnderGroundSection.GrowthFactorCount = 0 then
        begin
          BtnAddGrowthFactors.Enabled := True;
          BtnDeleteGrowthFactors.Enabled := False;
        end;
        FactorTypeCombo.Clear;
        ClearGrowthControl;
        for LIndex := 0 to AUnderGroundSection.GrowthFactorCount - 1 do
        begin
          LGrowthFactor := AUnderGroundSection.GrowthFactorByIndex(LIndex);
          if LGrowthFactor <> nil then
          begin
            FactorTypeCombo.AddItem(LGrowthFactor.Description,
              TObject(LGrowthFactor.FactorType));
          end;
        end;

        if AType > 9 then
          Exit;

        LGrowthFactor := AUnderGroundSection.GrowthFactorByType(AType);
        if LGrowthFactor <> nil then
        begin

          FactorTypeCombo.ItemIndex := FactorTypeCombo.Items.IndexOfObject
            (TObject(LGrowthFactor.FactorType));
          NoOfPointsEdit.Text := IntToStr(LGrowthFactor.NoOfPoints);
          InterpolationCheckBox.SetFieldIndex
            (InterpolationCheckBox.Items.IndexOfObject
            (TObject(LGrowthFactor.InterpolationMethod)));
          YearAndFactorGrid.FixedCols := 1;
          YearAndFactorGrid.FixedRows := 0;
          YearAndFactorGrid.ColCount := LGrowthFactor.NoOfPoints + 1;

          YearAndFactorGrid.Width :=
            (10 + (1 + YearAndFactorGrid.DefaultColWidth) *
            (YearAndFactorGrid.ColCount - 1)) + YearAndFactorGrid.ColWidths[0];
          YearAndFactorGrid.Cells[0, 0] := FAppModules.Language.GetString
            ('FYearAndFactorGrid.YearCol');
          case AType of
            8:
              YearAndFactorGrid.Cells[0, 1] := FAppModules.Language.GetString
                ('FYearAndFactorGrid.GrowthFactorUnderGroundType1');
            9:
              YearAndFactorGrid.Cells[0, 1] := FAppModules.Language.GetString
                ('FYearAndFactorGrid.GrowthFactorUnderGroundType2');
          end;

          for LIndex := 1 to LGrowthFactor.NoOfPoints do
          begin
            GrowthFactorControl.Visible := True;
             //YearAndFactorGrid.AddFieldProperty
              //(FAppModules.FieldProperties.FieldProperty('NYR'));

            YearAndFactorGrid.Cells[LIndex, 0] :=
              IntToStr(LGrowthFactor.NoOfYearsByIndex[LIndex - 1]);

            YearAndFactorGrid.AddFieldProperty
              (FAppModules.FieldProperties.FieldProperty('GrowthFactors'));
            YearAndFactorGrid.Cells[LIndex, 1] :=
              Format(FAppModules.FieldProperties.FieldProperty('GrowthFactors')
              .FormatStringGrid,
              [LGrowthFactor.GrowthFactorByIndex[LIndex - 1]]);
          end;
          Resize;
          ValidateNoOfPoints(LGrowthFactor);
          ValidateInterpolationMedthod(LGrowthFactor);
          ValidateGrowthFactor(LGrowthFactor);
          ValidateNoOfYears(LGrowthFactor);
        end
        else
          PopulateGrowthFactorControl(AUnderGroundSection, AType + 1);

      end;
    end;
  except
    on E: Exception do
      HandleError(E, OPNAME);
  end;

end;

procedure TPlanningMineUnderGroundSectionValidator.PopulateLoadGenerationControl
  (AUnderGroundSection: TPlanningUnderGroundMine);
const
  OPNAME = 'TPlanningMineUnderGroundSectionValidator.PopulateLoadGenerationControl';
var
  LLoadGeneration: TLoadGeneration;
  LCount: integer;
begin
  try
    ClearLoadGenerationControl;
    if AUnderGroundSection <> nil then
    begin
      LLoadGeneration := AUnderGroundSection.LoadGeneration;

      if LLoadGeneration = nil then
      begin
        MiningUnderGroundSectionDialog.BtnAddLoadGeneration.Enabled := True;
        MiningUnderGroundSectionDialog.BtnDeleteLoadGeneration.Enabled := False;
        Exit;
      end;

      with MiningUnderGroundSectionDialog do
      begin
        LoadGenerationCombo.Clear;
        BtnAddLoadGeneration.Enabled := False;
        BtnDeleteLoadGeneration.Enabled := True;
        LoadGenerationCombo.AddItem('Recharge to underground',
          TObject(LLoadGeneration.type_));
        LoadGenerationCombo.ItemIndex := LoadGenerationCombo.Items.IndexOfObject
          (TObject(LLoadGeneration.type_));
        StdDeviationEdit.Text := FloatToStr(LLoadGeneration.StandardDeviation);
        // FloatToStrF(LLoadGeneration.StandardDeviation,ffNumber,5,2);
        FactorAndMeanGrid.Cells[0, 0] := FAppModules.Language.GetString
          ('FactorsAndMeanGrid.UndergroundFlow');
        FactorAndMeanGrid.Cells[1, 0] := FAppModules.Language.GetString
          ('FactorsAndMeanGrid.UndergroundMean');

        FactorAndMeanGrid.FixedCols := 0;
        FactorAndMeanGrid.FixedRows := 1;
        FactorAndMeanGrid.ColCount := 2;
        FactorAndMeanGrid.RowCount := 11;
        FactorAndMeanGrid.Height :=
          (FactorAndMeanGrid.DefaultRowHeight *
          FactorAndMeanGrid.RowCount) + 30;
        FactorAndMeanGrid.Width := 10 + (1 + FactorAndMeanGrid.DefaultColWidth *
          FactorAndMeanGrid.ColCount);
        for LCount := 0 to 9 do
        begin
          FactorAndMeanGrid.Cells[0, LCount + 1] :=
            Format(FAppModules.FieldProperties.FieldProperty('Flow')
            .FormatStringGrid, [LLoadGeneration.FlowByIndex[LCount]]);
          FactorAndMeanGrid.Cells[1, LCount + 1] :=
            Format(FAppModules.FieldProperties.FieldProperty('MeanOfSalt')
            .FormatStringGrid, [LLoadGeneration.MeanOfSaltByIndex[LCount]])
          // FloatToStrF(LLoadGeneration.FlowByIndex[LCount],ffNumber,5,2);

        end;
      end;
    end;

  except
    on E: Exception do
      HandleError(E, OPNAME);
  end;
end;

procedure TPlanningMineUnderGroundSectionValidator.refreshGrowthControl
  (Sender: TObject);
const
  OPNAME = 'TPlanningMineUnderGroundSectionValidator.refreshGrowthControl';
var
  LPlanningMine: TPlanningMine;
  LUnderGroundSection: TPlanningUnderGroundMine;
  LType: integer;
begin
  try
    LPlanningMine := TPlanningMine
      (TPlanningModelDataObject(FAppModules.Model.ModelData)
      .CastNetworkFeaturesData.CastMineList.CastMineByNodeNumber[FIdentifier]);
    LUnderGroundSection := LPlanningMine.CastUnderGroundByID
      [FSelectUGIdentifier];
    if LUnderGroundSection <> nil then
    begin
      with MiningUnderGroundSectionDialog do
      begin
        LType := integer(FactorTypeCombo.Items.Objects
          [FactorTypeCombo.ItemIndex]);
        PopulateGrowthFactorControl(LUnderGroundSection, LType);
      end;
    end;
  except
    on E: Exception do
      HandleError(E, OPNAME);
  end;
end;

procedure TPlanningMineUnderGroundSectionValidator.refreshLoadGenerationControl
  (Sender: TObject);
const
  OPNAME = 'TPlanningMineUnderGroundSectionValidator.refreshLoadGenerationControl';
begin
  try

  except
    on E: Exception do
      HandleError(E, OPNAME);
  end;
end;

procedure TPlanningMineUnderGroundSectionValidator.RePopulateDataViewer;
const
  OPNAME = 'TPlanningMineUnderGroundSectionValidator.RePopulateDataViewer';
var
  LMine: TPlanningMine;
  LUnderGroundMine: TPlanningUnderGroundMine;
begin
  inherited;
  try
    LMine := TPlanningMine(TPlanningModelDataObject(FAppModules.Model.ModelData)
      .CastNetworkFeaturesData.CastMineList.CastMineByNodeNumber[FIdentifier]);
    if LMine <> nil then
    begin
      with MiningUnderGroundSectionDialog do
      begin
        LUnderGroundMine := LMine.CastUnderGroundByID[FSelectUGIdentifier];
        if LUnderGroundMine <> nil then
        begin
          if LUnderGroundMine.GrowthFactorCount = 2 then
          begin
            BtnAddGrowthFactors.Enabled := False;
            BtnDeleteGrowthFactors.Enabled := True;
          end
          else if LUnderGroundMine.GrowthFactorCount = 1 then
          begin
            BtnAddGrowthFactors.Enabled := True;
            BtnDeleteGrowthFactors.Enabled := True;
          end
          else if LUnderGroundMine.GrowthFactorCount = 0 then
          begin
            BtnAddGrowthFactors.Enabled := False;
            BtnDeleteGrowthFactors.Enabled := True;
          end;
        end
        else
        begin
          BtnAddGrowthFactors.Enabled := False;
          BtnDeleteGrowthFactors.Enabled := False;
        end;

        MiningUnderGroundSectionDialog.GrowthFactorControl.Visible := True;
        PopulateLoadGenerationControl(LUnderGroundMine);
        MiningUnderGroundSectionDialog.LoadGenerationControl.Visible := True;
        PopulateGrowthFactorControl(LUnderGroundMine);
      end;
    end;
  except
    on E: Exception do
      HandleError(E, OPNAME);
  end;
end;

procedure TPlanningMineUnderGroundSectionValidator.UpdateFlow(ACol, ARow: integer);
const OPNAME = 'TPlanningMineUnderGroundSectionValidator.UpdateFlow';
var
  LMine : TPlanningMine;
  LUnderGroundMine : TPlanningUnderGroundMine;
  LLoadGeneration : TLoadGeneration;
  LMessage : string;
  LCellVal : string;
begin
  try
    with MiningUnderGroundSectionDialog do
    begin
      LMine :=  TPlanningMine(TPlanningModelDataObject(FAppModules.Model.ModelData).CastNetworkFeaturesData.CastMineList.CastMinebyNodeNumber[FIdentifier]);
      if LMine <> nil then
      begin
        LUnderGroundMine := LMine.CastUnderGroundByID[FSelectUGIdentifier];
        if LUnderGroundMine <> nil then
        begin

            LCellVal := FactorAndMeanGrid.Cells[FactorAndMeanGrid.Col, FactorAndMeanGrid.Row];
            if (FAppModules.FieldProperties.ValidateFieldProperty(
                'Flow',LCellVal, LMessage,ARow)) then
            begin
              LLoadGeneration := LUnderGroundMine.LoadGeneration;
              if LLoadGeneration <> nil then
              begin
                LLoadGeneration.FlowByIndex[ARow - 1]  := StrToFloat(FactorAndMeanGrid.Cells[ACol,ARow]);
                //ClearDataViewer;
                //RePopulateDataViewer;
                PopulateLoadGenerationControl(LUnderGroundMine);
                DoContextValidation(dvtFlow);
              end;
            end
            else
              FactorAndMeanGrid.ValidationError[ACol, ARow,gveCellField] := LMessage;
          end;
        end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TPlanningMineUnderGroundSectionValidator.UpDateGrowthFactor(ACol,
  ARow: integer);
const
  OPNAME = 'TPlanningMineUnderGroundSectionValidator.UpDateGrowthFactor';
var
  LMine: TPlanningMine;
  LUnderGroundMine: TPlanningUnderGroundMine;
  LGrowthFactor: TPlanningMineGrowthFactor;
  LMessage: string;
  LCellVal: string;
begin
  try
    with MiningUnderGroundSectionDialog do
    begin
      LMine := TPlanningMine
        (TPlanningModelDataObject(FAppModules.Model.ModelData)
        .CastNetworkFeaturesData.CastMineList.CastMineByNodeNumber
        [FIdentifier]);
      if LMine <> nil then
      begin
        LUnderGroundMine := LMine.CastUnderGroundByID[FSelectUGIdentifier];
        if LUnderGroundMine <> nil then
        begin
          with MiningUnderGroundSectionDialog do
          begin
            LCellVal := YearAndFactorGrid.Cells[ACol, ARow];
            if (FAppModules.FieldProperties.ValidateFieldProperty
              ('GrowthFactors', LCellVal, LMessage)) then
            begin
              LGrowthFactor := LUnderGroundMine.GrowthFactorByType
                (integer(FactorTypeCombo.Items.Objects
                [FactorTypeCombo.ItemIndex]));
              if LGrowthFactor <> nil then
              begin
                LGrowthFactor.GrowthFactorByIndex[ACol - 1] :=
                  StrToFloat(Trim(YearAndFactorGrid.Cells[ACol, ARow]));
                PopulateGrowthFactorControl(LUnderGroundMine,
                  LGrowthFactor.FactorType);
                DoContextValidation(dvtGrowthFactor);
              end;
            end
            else
              YearAndFactorGrid.ValidationError[YearAndFactorGrid.Col,
                YearAndFactorGrid.Row, gveCellField] := LMessage;
          end;
        end;
      end;
    end;
  except
    on E: Exception do
      HandleError(E, OPNAME);
  end;
end;

procedure TPlanningMineUnderGroundSectionValidator.UpdateIntepolationMethod;
const
  OPNAME = 'TPlanningMineUnderGroundSectionValidator.UpdateIntepolationMethod';
var
  LMine: TPlanningMine;
  LUnderGroundMine: TPlanningUnderGroundMine;
  LGrowth: TPlanningMineGrowthFactor;
  LMessage: string;
  LValue: integer;
  LType: integer;
begin
  try
    LMine := TPlanningMine(TPlanningModelDataObject(FAppModules.Model.ModelData)
      .CastNetworkFeaturesData.CastMineList.CastMineByNodeNumber[FIdentifier]);
    if LMine <> nil then
    begin
      with MiningUnderGroundSectionDialog do
      begin
        LUnderGroundMine := LMine.CastUnderGroundByID[FSelectUGIdentifier];
        if LUnderGroundMine <> nil then
        begin

          LValue := integer
            (MiningUnderGroundSectionDialog.InterpolationCheckBox.Items.Objects
            [MiningUnderGroundSectionDialog.InterpolationCheckBox.ItemIndex]);
          LType := integer(FactorTypeCombo.Items.Objects
            [FactorTypeCombo.ItemIndex]);
          LGrowth := LUnderGroundMine.GrowthFactorByType(LType);
          if LGrowth <> nil then
          begin
            if (FAppModules.FieldProperties.ValidateFieldProperty
              ('InterpolationMethod', IntToStr(LValue), LMessage)) then
            begin
              LGrowth.InterpolationMethod := LValue;
              PopulateGrowthFactorControl(LUnderGroundMine, LType);
              DoContextValidation(dvtInterpolationMethod);
            end
            else
              InterpolationCheckBox.ValidationError := LMessage;
          end;
        end;
      end;
    end;
  except
    on E: Exception do
      HandleError(E, OPNAME);
  end;
end;

procedure TPlanningMineUnderGroundSectionValidator.UpDateMeanOfSalt(ACol,
  ARow: integer);
const OPNAME = 'TPlanningMineOpenCastValidator.UpDateMeanOfSalt';
var
  LMine : TPlanningMine;
  LUnderGroundMine : TPlanningUnderGroundMine;
  LLoadGeneration : TLoadGeneration;
  LMessage : string;
  LCellVal : string;
begin
  try
    with MiningUnderGroundSectionDialog do
    begin
      LMine :=  TPlanningMine(TPlanningModelDataObject(FAppModules.Model.ModelData).CastNetworkFeaturesData.CastMineList.CastMinebyNodeNumber[FIdentifier]);
      if LMine <> nil then
      begin
        LUnderGroundMine := LMine.CastUnderGroundByID[FSelectUGIdentifier];
        if LUnderGroundMine <> nil then
        begin

            LCellVal := FactorAndMeanGrid.Cells[FactorAndMeanGrid.Col, FactorAndMeanGrid.Row];
            if (FAppModules.FieldProperties.ValidateFieldProperty(
                'MeanOfSalt',LCellVal, LMessage,FactorAndMeanGrid.Row)) then
            begin
              LLoadGeneration := LUnderGroundMine.LoadGeneration;
              if LLoadGeneration <> nil then
              begin
                LLoadGeneration.MeanOfSaltByIndex[ARow - 1]  := StrToFloat(FactorAndMeanGrid.Cells[ACol,ARow]);
                //ClearDataViewer;
                //RePopulateDataViewer;
                PopulateLoadGenerationControl(LUnderGroundMine);
                DoContextValidation(dvtMeanOfSalt);
                //PopulateYearAndFactorGrid(LGrowthFactor);
                //ValidateNoOfYears(LGrowthFactor);
              end;
            end
            else
              FactorAndMeanGrid.ValidationError[FactorAndMeanGrid.Col, FactorAndMeanGrid.Row,gveCellField] := LMessage;
        end;
      end;
    end;
  except on E: Exception do HandleError(E,OPNAME); end;
end;


procedure TPlanningMineUnderGroundSectionValidator.UpDateNoOfYears(ACol,
  ARow: integer);
const
  OPNAME = 'TPlanningMineUnderGroundSectionValidator.UpDateNoOfYears';
var
  LMine: TPlanningMine;
  LUnderGroundMine: TPlanningUnderGroundMine;
  LGrowthFactor: TPlanningMineGrowthFactor;
  LMessage: string;
  LCellVal: string;
begin
  try
    with MiningUnderGroundSectionDialog do
    begin
      LMine := TPlanningMine
        (TPlanningModelDataObject(FAppModules.Model.ModelData)
        .CastNetworkFeaturesData.CastMineList.CastMineByNodeNumber
        [FIdentifier]);
      if LMine <> nil then
      begin
        LUnderGroundMine := LMine.CastUnderGroundByID[FSelectUGIdentifier];
        if LUnderGroundMine <> nil then
        begin
          LGrowthFactor := LUnderGroundMine.GrowthFactorByType
            (integer(FactorTypeCombo.Items.Objects[FactorTypeCombo.ItemIndex]));
          if LGrowthFactor <> nil then
          begin
            LCellVal := YearAndFactorGrid.Cells[ACol, ARow];
            if FAppModules.FieldProperties.ValidateFieldProperty('NYR',
              LCellVal, LMessage) then
            begin
              LGrowthFactor.NoOfYearsByIndex[YearAndFactorGrid.Col - 1] :=
                StrToInt(YearAndFactorGrid.Cells[ACol, ARow]);
              PopulateGrowthFactorControl(LUnderGroundMine,
                LGrowthFactor.FactorType);
              DoContextValidation(dvtNYR);
            end;
          end;
        end;
      end;
    end;
  except
    on E: Exception do
      HandleError(E, OPNAME);
  end;
end;

procedure TPlanningMineUnderGroundSectionValidator.UpdateStandardDeviation;
const
  OPNAME = 'TPlanningMineUnderGroundSectionValidator.UpdateStandardDeviation';
var
  LMine: TPlanningMine;
  LUnderGroundMine: TPlanningUnderGroundMine;
  LLoadGeneration: TLoadGeneration;
  LMessage: string;
  LCellVal: string;
begin
  try
    with MiningUnderGroundSectionDialog do
    begin
      LMine := TPlanningMine
        (TPlanningModelDataObject(FAppModules.Model.ModelData)
        .CastNetworkFeaturesData.CastMineList.CastMineByNodeNumber
        [FIdentifier]);
      if LMine <> nil then
      begin
        LUnderGroundMine := LMine.CastUnderGroundByID[FSelectUGIdentifier];
        if LUnderGroundMine <> nil then
        begin
          with MiningUnderGroundSectionDialog do
          begin
            LCellVal := StdDeviationEdit.Text;
            if (FAppModules.FieldProperties.ValidateFieldProperty
              ('StdDeviation', LCellVal, LMessage)) then
            begin
              LLoadGeneration := LUnderGroundMine.LoadGeneration;
              if LLoadGeneration <> nil then
              begin
                LLoadGeneration.StandardDeviation :=
                  StrToFloat(StdDeviationEdit.Text);
                PopulateLoadGenerationControl(LUnderGroundMine);
                DoContextValidation(dvtStandardDeviation);
              end;
            end
            else
              StdDeviationEdit.FieldValidationError := LMessage;
          end;
        end;
      end;
    end;
  except
    on E: Exception do
      HandleError(E, OPNAME);
  end;
end;

procedure TPlanningMineUnderGroundSectionValidator.UpdateYearDataPoints;
const
  OPNAME = 'TPlanningMineUnderGroundSectionValidator.UPdateYearDataPoints';
var
  LMine: TPlanningMine;
  LUnderGroundMine: TPlanningUnderGroundMine;
  LGrowth: TPlanningMineGrowthFactor;
  LMessage: string;
  LValue: integer;
  LType: integer;
begin
  try
    with MiningUnderGroundSectionDialog do
    begin
      LMine := TPlanningMine
        (TPlanningModelDataObject(FAppModules.Model.ModelData)
        .CastNetworkFeaturesData.CastMineList.CastMineByNodeNumber
        [FIdentifier]);
      if LMine <> nil then
      begin
        LUnderGroundMine := LMine.CastUnderGroundByID[FSelectUGIdentifier];
        if LUnderGroundMine <> nil then
        begin
          LType := integer(FactorTypeCombo.Items.Objects
            [FactorTypeCombo.ItemIndex]);
          LGrowth := LUnderGroundMine.GrowthFactorByType(LType);
          if LGrowth <> nil then
          begin
            LValue := StrToInt(NoOfPointsEdit.Text);
            if (FAppModules.FieldProperties.ValidateFieldProperty('NoOfPoints',
              IntToStr(LValue), LMessage)) then
            begin
              LGrowth.NoOfPoints := LValue;
              PopulateGrowthFactorControl(LUnderGroundMine, LType);
              DoContextValidation(dvtNoOfPoints);
            end
            else
              NoOfPointsEdit.FieldValidationError := LMessage;
          end;
        end;
      end;
    end;

  except
    on E: Exception do
      HandleError(E, OPNAME);
  end;
end;

procedure TPlanningMineUnderGroundSectionValidator.ValidateFlow(
  ALoadGeneration: TLoadGeneration);
const OPNAME = 'TPlanningMineUnderGroundSectionValidator.ValidateFlow';
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
            MiningUnderGroundSectionDialog.FactorAndMeanGrid.ValidationError[0, LRow, gveCellContext] := ''
        else
        begin
          ExtractErrorsAndColumns(FErrorMessage, LErrorMsgs, LErrorCols);
          for LRow := 1 to 10 do
          begin
            LIndex := LErrorCols.IndexOf(IntToStr(LRow));
            if (LIndex >= 0) then
              MiningUnderGroundSectionDialog.FactorAndMeanGrid.ValidationError[0, LRow, gveCellContext] := LErrorMsgs.Strings[lIndex]
            else
              MiningUnderGroundSectionDialog.FactorAndMeanGrid.ValidationError[0, LRow, gveCellContext] := '';
          end;
        end;
      end;
   finally
     FreeAndNil(LErrorCols);
     FreeAndNil(LErrorMsgs);
   end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


procedure TPlanningMineUnderGroundSectionValidator.ValidateGrowthFactor
  (AGrowthFactor: TPlanningMineGrowthFactor);
const
  OPNAME = 'TPlanningMineOpenCastValidator.ValidateNoOfYears';
var
  LErrorCols: TStringList;
  LErrorMsgs: TStringList;
  LIndex, LCol: integer;
begin
  try
    LErrorCols := TStringList.Create;
    LErrorMsgs := TStringList.Create;
    try
      if AGrowthFactor <> nil then
      begin
        if AGrowthFactor.Validate(FErrorMessage, 'GrowthFactors') then
          for LCol := 1 to AGrowthFactor.NoOfPoints do
            MiningUnderGroundSectionDialog.YearAndFactorGrid.ValidationError
              [LCol, 1, gveCellContext] := ''
        else
        begin
          for LCol := 1 to AGrowthFactor.NoOfPoints do
          begin
            LIndex := LErrorCols.IndexOf(IntToStr(LCol));
            if (LIndex >= 0) then
              MiningUnderGroundSectionDialog.YearAndFactorGrid.ValidationError
                [LCol, 1, gveCellContext] := LErrorMsgs.Strings[LIndex]
            else
              MiningUnderGroundSectionDialog.YearAndFactorGrid.ValidationError
                [LCol, 1, gveCellContext] := '';
          end;
        end;
      end;
    finally
      FreeAndNil(LErrorCols);
      FreeAndNil(LErrorMsgs);
    end;
  except
    on E: Exception do
      HandleError(E, OPNAME)
  end;
end;

procedure TPlanningMineUnderGroundSectionValidator.ValidateInterpolationMedthod
  (AGrowthFactor: TPlanningMineGrowthFactor);
const
  OPNAME = 'TPlanningMineOpenCastValidator.ValidateInterpolationMedthod';
begin
  try
    with MiningUnderGroundSectionDialog do
    begin
      if AGrowthFactor <> nil then
      begin
        FErrorMessage := '';
        AGrowthFactor.Validate(FErrorMessage, 'InterpolationMethod');
        InterpolationCheckBox.ValidationError := FErrorMessage;
      end;
    end;
  except
    on E: Exception do
      HandleError(E, OPNAME);
  end;
end;

procedure TPlanningMineUnderGroundSectionValidator.ValidateMeanOfSalt(
  ALoadGeneration: TLoadGeneration);
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
            MiningUnderGroundSectionDialog.FactorAndMeanGrid.ValidationError[0, LRow, gveCellContext] := ''
        else
        begin
          ExtractErrorsAndColumns(FErrorMessage, LErrorMsgs, LErrorCols);
          for LRow := 1 to 10 do
          begin
            LIndex := LErrorCols.IndexOf(IntToStr(LRow));
            if (LIndex >= 0) then
              MiningUnderGroundSectionDialog.FactorAndMeanGrid.ValidationError[0, LRow, gveCellContext] := LErrorMsgs.Strings[lIndex]
            else
              MiningUnderGroundSectionDialog.FactorAndMeanGrid.ValidationError[0, LRow, gveCellContext] := '';
          end;
        end;
      end;
   finally
     FreeAndNil(LErrorCols);
     FreeAndNil(LErrorMsgs);
   end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


procedure TPlanningMineUnderGroundSectionValidator.ValidateNoOfPoints
  (AGrowthFactor: TPlanningMineGrowthFactor);
const
  OPNAME = 'TPlanningMineOpenCastValidator.ValidateInterpolationMedthod';
begin
  try
    with MiningUnderGroundSectionDialog do
    begin
      if AGrowthFactor <> nil then
      begin
        FErrorMessage := '';
        AGrowthFactor.Validate(FErrorMessage, 'NoOfPoints');
        NoOfPointsEdit.FieldValidationError := FErrorMessage;
      end;
    end;
  except
    on E: Exception do
      HandleError(E, OPNAME);
  end;
end;

procedure TPlanningMineUnderGroundSectionValidator.ValidateNoOfYears
  (AGrowthFactor: TPlanningMineGrowthFactor);
const
  OPNAME = 'TPlanningMineUnderGroundSectionValidator.ValidateNoOfYears';
begin
  try
    with MiningUnderGroundSectionDialog do
    begin
      if AGrowthFactor <> nil then
      begin
        FErrorMessage := '';
        AGrowthFactor.Validate(FErrorMessage, 'NYR');
        YearAndFactorGrid.ValidationError[YearAndFactorGrid.Col,
          YearAndFactorGrid.Row, gveCellField] := FErrorMessage;
      end;
    end;
  except
    on E: Exception do
      HandleError(E, OPNAME);
  end;
end;

procedure TPlanningMineUnderGroundSectionValidator.ValidateStandardDeviation
  (ALoadGeneration: TLoadGeneration);
const
  OPNAME = 'TPlanningMineOpenCastValidator.ValidateStandardDeviation';
begin
  try
    with MiningUnderGroundSectionDialog do
    begin
      if ALoadGeneration <> nil then
      begin
        FErrorMessage := '';
        ALoadGeneration.Validate(FErrorMessage, 'StdDeviation');
        StdDeviationEdit.FieldValidationError := FErrorMessage;
      end;
    end;
  except
    on E: Exception do
      HandleError(E, OPNAME);
  end;
end;

end.
