unit UPlanningSlurryDumpValidator;

interface
uses
  UDataEditComponent,
  UMiningSlurryDumpValidator,
  UPlanningSlurryDump,
  UPlanningSlurryDumpDialog,
  UAbstractYieldDataDialogValidator,
  UPlanningMineGrowthFactor,
  ULoadGeneration,
  UYieldContextValidationType,
  UAbstractObject,
  UAbstractComponent,
  UDataComponent,
  VoaimsCom_TLB;

type
  TPlanningSlurryDumpValidator = class(TMiningSlurryDumpValidator)
  private
    protected
      procedure CreateMemberObjects; override;
      procedure DestroyMemberObjects; override;
      procedure OnEditControlEnter(Sender: TObject); override;
      procedure OnEditControltExit(Sender: TObject); override;
      procedure OnGridSelectCell(ASender: TObject; ACol, ARow: Integer; var CanSelect: Boolean);
      procedure OnInsertSlurryDump(Sender: TObject);
      procedure OnDeleteSlurryDump(Sender: TObject);
      procedure PopulateSlurryDumpGrid;
      procedure RePopulateDataViewer;
      procedure ClearGrowthFactorControl;
      procedure ClearLoadGenerationControl;

      procedure AddGrowthFactor(ASender: TObject);
      procedure DeleteGrowthFactor(ASender: TObject);
      procedure AddLoadGeneration(ASender: TObject);
      procedure DeleteLoadGeneration(ASender: TObject);

      //procedure refreshGrowthControl(Sender: TObject);
      //procedure refreshLoadGenerationControl(Sender: TObject);
      procedure PopulateGrowthFactorControl(ASlurryDump: TPlanningSlurryDump; AType : Integer = 10);
      procedure PopulateLoadGenerationControl(ASlurryDump:  TPlanningSlurryDump);
      procedure OnStringGridCellDataHasChanged(ASender: TObject; ACol, ARow: integer); override;
      procedure UpdateNoOfDataPoints(ACol : Integer; ARow: integer);
      procedure UpdateGrowthFactor(ACol : Integer; ARow: integer);
      procedure UpdateFlow(ACol, ARow: integer);
      procedure UpDateMeanOfSalt(ACol, ARow: integer);
      procedure UpDateSaltC(ACol, ARow: integer);
      procedure UpdateNoOfPoints;
      procedure UpdateInterpolationMethodCombo;
      procedure UpdateStandardDev;
      procedure ValidateFlow(ALoadGeneration: TLoadGeneration);
      procedure ValidateMeanOfSalt(ALoadGeneration : TLoadGeneration);
      procedure ValidateNoOfDataPoint(AGrowthFactor : TPlanningMineGrowthFactor);
      procedure ValidateGrowthFactor(AGrowthFactor : TPlanningMineGrowthFactor);
      procedure ValidateNoOfPoint(AGrowthFactor: TPlanningMineGrowthFactor);
      procedure ValidateInterpolationMethodCombo(AGrowthFactor: TPlanningMineGrowthFactor);
      procedure ValidateStandardDev(ALoadGeneration : TLoadGeneration);

    public
      procedure ClearDataViewer; override;
      procedure PopulateDataViewer; override;
      function MiningSlurryDumpDialog: TPlanningMineSlurryDumpDialog;
      procedure DoContextValidation(AValidationType: Integer); override;

  end;


implementation
uses
  SysUtils,
  VCL.Grids,
  Math,
  UPlanningMineData,
  UPlanningModelDataObject,
  UErrorHandlingOperations;

{ TPlanningSlurryDumpValidator }

procedure TPlanningSlurryDumpValidator.ClearDataViewer;
const OPNAME = 'TPlanningSlurryDumpValidator.ClearDataViewer';
var
  LCols , LRows : integer;
  LFieldProperty      : TAbstractFieldProperty;
begin
 try
    MiningSlurryDumpDialog.YearAndFactorGrid.ColWidths[0] := MiningSlurryDumpDialog.YearAndFactorGrid.DefaultColWidth*5;
    MiningSlurryDumpDialog.InterpolationCheckBox.Items.Clear;
    MiningSlurryDumpDialog.InterpolationCheckBox.Items.AddObject('1 - Linear Interpolation', TObject(1));
    MiningSlurryDumpDialog.InterpolationCheckBox.Items.AddObject('2 - Exponential interpolation', TObject(2));
    MiningSlurryDumpDialog.YearAndFactorGrid.FixedRows := 0;
    MiningSlurryDumpDialog.YearAndFactorGrid.FixedCols := 1;
    MiningSlurryDumpDialog.YearAndFactorGrid.FixedRows := 0;
    MiningSlurryDumpDialog.YearAndFactorGrid.ColCount := 2;
    MiningSlurryDumpDialog.YearAndFactorGrid.RowCount := 2;

    MiningSlurryDumpDialog.FactorAndMeanGrid.FixedCols := 0;
    MiningSlurryDumpDialog.FactorAndMeanGrid.FixedRows := 1;
    MiningSlurryDumpDialog.FactorAndMeanGrid.ColCount := 2;
    MiningSlurryDumpDialog.FactorAndMeanGrid.RowCount := 11;
    MiningSlurryDumpDialog.FactorAndMeanGrid.Height := (MiningSlurryDumpDialog.FactorAndMeanGrid.DefaultRowHeight*MiningSlurryDumpDialog.FactorAndMeanGrid.RowCount)+30;
    MiningSlurryDumpDialog.FactorAndMeanGrid.Width := 10+(1+MiningSlurryDumpDialog.FactorAndMeanGrid.DefaultColWidth*MiningSlurryDumpDialog.FactorAndMeanGrid.ColCount);

    MiningSlurryDumpDialog.FactorAndMeanGrid.ClearErrors;
    MiningSlurryDumpDialog.FactorAndMeanGrid.ColCount := 2;
    MiningSlurryDumpDialog.FactorAndMeanGrid.WrapHeaderText := True;
    MiningSlurryDumpDialog.FactorAndMeanGrid.FixedRows := 1;
    LFieldProperty := FAppModules.FieldProperties.FieldProperty('Flow');
    MiningSlurryDumpDialog.FactorAndMeanGrid.Height := (MiningSlurryDumpDialog.FactorAndMeanGrid.DefaultRowHeight*MiningSlurryDumpDialog.FactorAndMeanGrid.RowCount) +
                                                        Trunc(MiningSlurryDumpDialog.FactorAndMeanGrid.RowHeights[0]/2)+30;
      for LRows := 1 to LFieldProperty.ArrayLength do
        for LCols := 0 to 1 do
          MiningSlurryDumpDialog.FactorAndMeanGrid.Cells[LCols,LRows] := '';
    ClearGrowthFactorControl;
    ClearLoadGenerationControl;
  except on E: Exception do HandleError(E,OPNAME); end;
end;

procedure TPlanningSlurryDumpValidator.ClearGrowthFactorControl;
const OPNAME = 'TPlanningSlurryDumpValidator.ClearGrowthFactorControl';
var
  LRow, LCol: integer;
begin
  try
    with MiningSlurryDumpDialog do
    begin
      FactorTypeCombo.Clear;
      NoOfPointsEdit.Text := '';
      NoOfPointsEdit.FieldValidationError := '';
      YearAndFactorGrid.ClearErrors;
      for  LRow:= 0 to YearAndFactorGrid.RowCount -1 do
        for LCol := 1 to YearAndFactorGrid.ColCount -1 do
          YearAndFactorGrid.Cells[LCol,LRow] := '';
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TPlanningSlurryDumpValidator.ClearLoadGenerationControl;
const OPNAME = 'TPlanningSlurryDumpValidator.ClearLoadGenerationControl';
var
  LCol, LRow : integer;
begin
  try
    with MiningSlurryDumpDialog do
    begin
      StdDeviationEdit.Text := '';
      StdDeviationEdit.FieldValidationError := '';
      LoadGenerationCombo.Clear;
      FactorAndMeanGrid.ClearErrors;
      FactorAndMeanGrid.RowHeights[0] := FactorAndMeanGrid.DefaultRowHeight * 2;
      for LRow := 1 to FactorAndMeanGrid.RowCount -1 do
        for LCol := 0 to FactorAndMeanGrid.ColCount - 1 do
          FactorAndMeanGrid.Cells[LCol,LRow] := '';
    end;
  except on E: Exception do HandleError(E,OPNAME); end;
end;

procedure TPlanningSlurryDumpValidator.CreateMemberObjects;
const OPNAME = 'TPlanningSlurryDumpValidator.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
    FIdentifier := -1;
    FPanel := TPlanningMineSlurryDumpDialog.Create(FPanelOwner,FAppModules);
    with MiningSlurryDumpDialog do
    begin
      NumberOfSlurryDumpEdt.FieldProperty  := FAppModules.FieldProperties.FieldProperty('NrOfSlurryDump');
      NumberOfSlurryDumpEdt.OnEnter        := OnEditControlEnter;
      NumberOfSlurryDumpEdt.OnExit         := OnEditControltExit;
      NumberOfSlurryDumpEdt.IsEnabled      := False;

      SlurryDumpGrid.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('NrOfSlurryDump'));
      SlurryDumpGrid.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('DumpName'));
      SlurryDumpGrid.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('DumpSurfaceArea'));
      SlurryDumpGrid.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('RunoffFactorToPCD'));
      SlurryDumpGrid.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('SeepageSplitFactor'));
      SlurryDumpGrid.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('DumpPCDStorageCapacity'));
      SlurryDumpGrid.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('DumpPCDSurfaceArea'));
      SlurryDumpGrid.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('DumpPCDAnalysisStartVolume'));
      SlurryDumpGrid.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('SaltConcentration'));
      SlurryDumpGrid.OnBeforeCellChange   := OnStringGridCellDataHasChanged;
      SlurryDumpGrid.OnSelectCell         := OnGridSelectCell;
      SlurryDumpGrid.OnColEnter           := OnStringGridColEnter;
      SlurryDumpGrid.OnExit               := OnEditControltExit;
      SlurryDumpGrid.OnEnter              := OnEditControlEnter;

      SlurryDumpGrid.ButtonColumn[8] := True;
      SlurryDumpGrid.ButtonColumnCaption[8] := FAppModules.Language.GetString('LabelText.ThreeDots');
      SlurryDumpGrid.ButtonColumnOnClick[8] := OnMonthlyRechargeFactorsClick;

      SlurryDumpGrid.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('SaltConcentration'));

      InsertSlurryBtn.OnClick              := OnInsertSlurryDump;
      DeleteSlurryBtn.OnClick              := OnDeleteSlurryDump;

      InterpolationCheckBox.OnSelect       := OnEditControltExit;
      NoOfPointsEdit.OnEnter               := OnEditControlEnter;
      StdDeviationEdit.OnEnter            := OnEditControlEnter;
      StdDeviationEdit.OnExit              := OnEditControltExit;


      NoOfPointsEdit.OnExit                := OnEditControltExit;

      YearAndFactorGrid.OnColEnter        := OnStringGridColEnter;
      YearAndFactorGrid.OnExit            := OnEditControltExit;

      FactorAndMeanGrid.OnColEnter        := OnStringGridColEnter;
      FactorAndMeanGrid.OnExit            := OnEditControltExit;

      BtnAddGrowthFactors.OnClick          := AddGrowthFactor;
      BtnDeleteGrowthFactors.OnClick       := DeleteGrowthFactor;
      BtnAddLoadGeneration.OnClick         := AddLoadGeneration;
      BtnDeleteLoadGeneration.OnClick      := DeleteLoadGeneration;

      NoOfPointsEdit.FieldProperty := FAppModules.FieldProperties.FieldProperty('NoOfPoints');
      StdDeviationEdit.FieldProperty := FAppModules.FieldProperties.FieldProperty('StdDeviation');

      YearAndFactorGrid.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('NYR'));
      YearAndFactorGrid.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('GrowthFactors'));
      YearAndFactorGrid.OnBeforeCellChange := OnStringGridCellDataHasChanged;

      FactorAndMeanGrid.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('Flow'));
      FactorAndMeanGrid.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('MeanOfSalt'));
      FactorAndMeanGrid.OnBeforeCellChange := OnStringGridCellDataHasChanged;

    end;
  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TPlanningSlurryDumpValidator.AddGrowthFactor(ASender: TObject);
const OPNAME = 'TPlanningSlurryDumpValidator.AddGrowthFactor';
var
  LMine : TPlanningMine;
  LSlurryDump : TPlanningSlurryDump;
begin
   try
    with MiningSlurryDumpDialog do
      begin
        if (ASender = BtnAddGrowthFactors ) then
        begin
        LMine := TPlanningMine(TPlanningModelDataObject(FAppModules.Model.ModelData).CastNetworkFeaturesData.CastMineList.MineByNodeNumber[FIdentifier]);
        LSlurryDump := LMine.CastSlurryDumpByID[FSelectSlurryID];
        LSlurryDump.CreateGrowthFactor;
        if LSlurryDump.GrowthFactor <> nil then
          PopulateGrowthFactorControl(LSlurryDump);
        end;
      end;
 except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TPlanningSlurryDumpValidator.AddLoadGeneration(ASender: TObject);
const OPNAME = 'TPlanningSlurryDumpValidator.AddLoadGeneration';
var
  LMine : TPlanningMine;
  LSlurryDump : TPlanningSlurryDump;
begin
   try
     with MiningSlurryDumpDialog do
         begin
            if (ASender =  BtnAddLoadGeneration) then
              begin
                LMine := TPlanningMine(TPlanningModelDataObject(FAppModules.Model.ModelData).CastNetworkFeaturesData.CastMineList.MineByNodeNumber[FIdentifier]);
                LSlurryDump := LMine.CastSlurryDumpByID[FSelectSlurryID];
                LSlurryDump.CreateLoadGeneration;
                if LSlurryDump.LoadGeneration <> nil then
                    PopulateLoadGenerationControl(LSlurryDump);
              end;
            end;
    except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TPlanningSlurryDumpValidator.DeleteGrowthFactor(ASender: TObject);
const OPNAME = 'TPlanningSlurryDumpValidator.DeleteGrowthFactor';
var
  LMine : TPlanningMine;
  LSlurryDump : TPlanningSlurryDump;
begin
  try
   with MiningSlurryDumpDialog do
       begin
          if (ASender = BtnDeleteGrowthFactors) then
          begin
            LMine := TPlanningMine(TPlanningModelDataObject(FAppModules.Model.ModelData).CastNetworkFeaturesData.CastMineList.CastMineByNodeNumber[FIdentifier]);
            LSlurryDump:=  LMine.CastSlurryDumpByID[FSelectSlurryID];
            if LMine <> nil then
            begin
              if LSlurryDump.GrowthFactor <> nil then
                 LSlurryDump.DeleteGrowthFactor;
                 PopulateGrowthFactorControl(LSlurryDump);
                end;
            end;
       end;
   except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TPlanningSlurryDumpValidator.DeleteLoadGeneration(ASender: TObject);
const OPNAME = 'TPlanningSlurryDumpValidator.DeleteLoadGeneration';
var
  LMine : TPlanningMine;
  LSlurryDump : TPlanningSlurryDump;
begin
  try
    with MiningSlurryDumpDialog do
    begin
      if (ASender = BtnDeleteLoadGeneration) then
      begin
        LMine := TPlanningMine(TPlanningModelDataObject(FAppModules.Model.ModelData).CastNetworkFeaturesData.CastMineList.CastMineByNodeNumber[FIdentifier]);
        LSlurryDump :=  LMine.CastSlurryDumpByID[FSelectSlurryID];
        if LMine <> nil then
        begin
          if LSlurryDump.LoadGeneration <> nil then
             LSlurryDump.DeleteLoadGeneration;
          PopulateLoadGenerationControl(LSlurryDump);
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TPlanningSlurryDumpValidator.DestroyMemberObjects;
const OPNAME = 'TPlanningSlurryDumpValidator.DestroyMemberObjects';
begin
 try
  inherited;
 except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TPlanningSlurryDumpValidator.DoContextValidation(AValidationType: Integer);
const OPNAME = 'TPlanningSlurryDumpValidator.DoContextValidation';
var

  LMine : TPlanningMine;
  LSlurryDumpData : TPlanningSlurryDump;
  LLoadGenData : TLoadGeneration;
  LGrowthFactor : TPlanningMineGrowthFactor;

begin
 try
   inherited;

   LMine := TPlanningMine(TPlanningModelDataObject(FAppModules.Model.ModelData).CastNetworkFeaturesData.CastMineList.CastMineByNodeNumber[FIdentifier]);
   LSlurryDumpData := LMine.CastSlurryDumpByID[FSelectSlurryID];

   if LSlurryDumpData <> nil then
   begin

    LLoadGenData := LSlurryDumpData.LoadGeneration;
    LGrowthFactor := LSlurryDumpData.GrowthFactor;

    case AValidationType of

    dvtNoOfPoints          : ValidateNoOfPoint(LGrowthFactor);
    dvtInterpolationMethod : ValidateInterpolationMethodCombo(LGrowthFactor);
    dvtStandardDeviation   : ValidateStandardDev(LLoadGenData);
    dvtNYR       : ValidateNoOfDataPoint(LGrowthFactor);
    dvtGrowthFactor       : ValidateGrowthFactor(LGrowthFactor);
    dvtFlow : ValidateFlow(LLoadGenData);
    dvtMeanOfSalt : ValidateMeanOfSalt(LLoadGenData);

   end;

   end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TPlanningSlurryDumpValidator.MiningSlurryDumpDialog: TPlanningMineSlurryDumpDialog;
const OPNAME = 'TPlanningSlurryDumpValidator.MiningSlurryDumpDialog';
begin
  Result := nil;
  try
    if Assigned(FPanel) then
      Result:= TPlanningMineSlurryDumpDialog(FPanel);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TPlanningSlurryDumpValidator.OnDeleteSlurryDump(Sender: TObject);
const OPNAME = 'TPlanningSlurryDumpValidator.OnInsertSlurryDump';
var
  LSlarry : TPlanningSlurryDump;
  LMine : TPlanningMine;
begin
  try
    LMine := TPlanningMine(TPlanningModelDataObject(FAppModules.Model.ModelData).CastNetworkFeaturesData.CastMineList.CastMineByNodeNumber[FIdentifier]);
    if LMine <> nil then
    begin
      LSlarry := LMine.CastSlurryDumpByID[FSelectSlurryID];
      if LSlarry <> nil then
      begin
        if LMine.RemoveSlurryDump(LSlarry.Identifier) then
        begin
          RePopulateDataViewer;
          DoContextValidation(dvtYMDCReturnFlowFeatureAll);
        end;
      end;
    end;

  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TPlanningSlurryDumpValidator.OnEditControlEnter(Sender: TObject);
const OPNAME = 'TPlanningSlurryDumpValidator.OnEditControlEnter';
begin
  inherited OnEditControlEnter(Sender);
  try
   except on E: Exception do HandleError(E,OPNAME); end;
end;

procedure TPlanningSlurryDumpValidator.OnEditControltExit(Sender: TObject);
const OPNAME = 'TPlanningSlurryDumpValidator.OnEditControltExit';
begin
  inherited OnEditControltExit(Sender);
   try
    with MiningSlurryDumpDialog do
     begin
     if (sender = NoOfPointsEdit) then
         UpdateNoOfPoints
     else
     if (sender = InterpolationCheckBox) then
         UpdateInterpolationMethodCombo
     else
     if (sender =  StdDeviationEdit) then
         UpdateStandardDev;
     end;
    except on E: Exception do HandleError(E,OPNAME); end;
end;

procedure TPlanningSlurryDumpValidator.OnGridSelectCell(ASender: TObject; ACol,
  ARow: Integer; var CanSelect: Boolean);
const OPNAME = 'TPlanningSlurryDumpValidator.OnGridSelectCell';
var
LMine : TPlanningMine;
LSlurryDump : TPlanningSlurryDump;

begin
  try
    if ((ARow > 0))  then
    begin
      LMine := TPlanningMine(TPlanningModelDataObject(FAppModules.Model.ModelData).CastNetworkFeaturesData.CastMineList.CastMinebyNodeNumber[FIdentifier]);
      FSelectSlurryID := -1;
      if LMine <> nil then
      begin
        FSelectSlurryID := StrToInt(MiningSlurryDumpDialog.SlurryDumpGrid.Cells[0,ARow]);
        if FSelectSlurryID <> -1 then
        begin
          LSlurryDump := LMine.CastSlurryDumpByID[FSelectSlurryID];
          PopulateGrowthFactorControl(LSlurryDump);
          MiningSlurryDumpDialog.GrowthFactorControl.Visible :=  CanSelect;
          PopulateLoadGenerationControl(LSlurryDump);
          MiningSlurryDumpDialog.LoadGenerationControl.Visible := CanSelect;
          MiningSlurryDumpDialog.YearAndFactorGrid.Visible := CanSelect;
        end;
      end;
    end
    else
    begin
      MiningSlurryDumpDialog.GrowthFactorControl.Visible := Canselect;
      MiningSlurryDumpDialog.LoadGenerationControl.Visible := Canselect;
    end;
  except on E: Exception do HandleError(E,OPNAME); end;
end;

procedure TPlanningSlurryDumpValidator.OnInsertSlurryDump(Sender: TObject);
const OPNAME = 'TPlanningSlurryDumpValidator.OnInsertSlurryDump';
begin
  try

    if (TPlanningMine(TPlanningModelDataObject(FAppModules.Model.ModelData).CastNetworkFeaturesData.CastMineList
             .CastMineByNodeNumber[FIdentifier]).CreateSlurryDump <> nil)then
    begin
      RePopulateDataViewer;
      DoContextValidation(dvtYMDCReturnFlowFeatureAll);
    end;

  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TPlanningSlurryDumpValidator.OnStringGridCellDataHasChanged(
  ASender: TObject; ACol, ARow: integer);
  const OPNAME = 'TPlanningSlurryDumpValidar.OnStringGridCellDataHasChanged';
begin
  inherited OnStringGridCellDataHasChanged(ASender, ACol, ARow);
  try
    with MiningSlurryDumpDialog do
    begin
      if (YearAndFactorGrid = ASender) then
      begin
        if ARow = 0 then
          UpdateNoOfDataPoints(ACol, ARow)
        else
          UpDateGrowthFactor(ACol, ARow);
      end;

      if (FactorAndMeanGrid = ASender) then
      begin
        if ACol =  0 then
        UpdateFlow(ACol, ARow);
      end;

      if (FactorAndMeanGrid = ASender) then
      begin
      if ACol <> 0 then

        UpDateMeanOfSalt(ACol, ARow);
      end;

      if (SlurryDumpGrid = ASender) then
      begin
      if ACol = 9 then

        UpDateSaltC(ACol, ARow);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TPlanningSlurryDumpValidator.PopulateDataViewer;
const OPNAME = 'TPlanningSlurryDumpValidator.PopulateDataViewer';
begin
  inherited;
  try
    ClearDataViewer;
    RePopulateDataViewer;
  except on E: Exception do HandleError(E, OPNAME); end;

end;

procedure TPlanningSlurryDumpValidator.PopulateGrowthFactorControl(ASlurryDump: TPlanningSlurryDump; AType: Integer);
const OPNAME = 'TPlanningSlurryDumpValidator.PopulateGrowthFactorControl';
var
 LGrowthFactor : TPlanningMineGrowthFactor;
 LIndex : Integer;
begin
  try
    ClearGrowthFactorControl;
   if ASlurryDump <> nil then
    begin
      with MiningSlurryDumpDialog do
      begin

        LGrowthFactor := ASlurryDump.GrowthFactor;

        if LGrowthFactor = nil then
        begin
          MiningSlurryDumpDialog.BtnAddGrowthFactors.Enabled := True;
          MiningSlurryDumpDialog.BtnDeleteGrowthFactors.Enabled := False;
          Exit;
        end
        else
        MiningSlurryDumpDialog.BtnAddGrowthFactors.Enabled := False;
        MiningSlurryDumpDialog.BtnDeleteGrowthFactors.Enabled := True;

        if LGrowthFactor <> nil then
        begin
          FactorTypeCombo.Clear;
          FactorTypeCombo.AddItem(LGrowthFactor.Description, TObject(LGrowthFactor.FactorType));
          FactorTypeCombo.SetFieldIndex(FactorTypeCombo.Items.IndexOfObject(TObject(LGrowthFactor.FactorType)));
        end;

        FactorTypeCombo.ItemIndex := FactorTypeCombo.Items.IndexOf(LGrowthFactor.Description);
        NoOfPointsEdit.SetFieldValue(LGrowthFactor.NoOfPoints);
        InterpolationCheckBox.SetFieldIndex(InterpolationCheckBox.Items.IndexOfObject(TObject(LGrowthFactor.InterpolationMethod)));

        YearAndFactorGrid.FixedCols := 1;
        YearAndFactorGrid.FixedRows := 0;
        YearAndFactorGrid.ColCount := LGrowthFactor.NoOfPoints + 1;
        YearAndFactorGrid.RowCount := 2; //Max(LGrowthFactor.NoOfPoints + 1,GrowthFactorControl.YearAndFactorGrid.FixedRows+1);
        YearAndFactorGrid.Cells[0,0] := FAppModules.Language.GetString('FYearAndFactorGrid.YearCol');
        YearAndFactorGrid.Cells[0,1] := FAppModules.Language.GetString('FYearAndFactorGrid.GrowthFactorSlurryDump');

        for LIndex := 1 to LGrowthFactor.NoOfPoints do
        begin
          //YearAndFactorGrid.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('NYR'));
          YearAndFactorGrid.Cells[LIndex,0 ] := IntToStr(LGrowthFactor.NoOfYearsByIndex[LIndex-1]);

          YearAndFactorGrid.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('GrowthFactors'));
          YearAndFactorGrid.Cells[LIndex,1 ]  := Format(FAppModules.FieldProperties.FieldProperty('GrowthFactors').FormatStringGrid,[LGrowthFactor.GrowthFactorByIndex[LIndex-1]]);
          //FloatToStr(LGrowthFactor.GrowthFactorByIndex[LIndex-1]);
        end;
        YearAndFactorGrid.Width := 10 + (YearAndFactorGrid.ColWidths[0] + YearAndFactorGrid.DefaultColWidth *LGrowthFactor.NoOfPoints + 10);

      end;
    end;
 except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TPlanningSlurryDumpValidator.PopulateLoadGenerationControl(
  ASlurryDump: TPlanningSlurryDump);
const OPNAME = 'TPlanningSlurryDumpValidator.PopulateLoadGenerationControl';
var
  LRow : integer;
  LLoadGeneration : TLoadGeneration;
begin
  try
    ClearLoadGenerationControl;
    if ASlurryDump <> nil then
    begin
     with MiningSlurryDumpDialog do
      begin

        LLoadGeneration := ASlurryDump.LoadGeneration;
        if LLoadGeneration = nil then
        begin
          MiningSlurryDumpDialog.BtnAddLoadGeneration.Enabled := True;
          MiningSlurryDumpDialog.BtnDeleteLoadGeneration.Enabled := False;
          Exit;
        end
        else
        MiningSlurryDumpDialog.BtnAddLoadGeneration.Enabled := False;
        MiningSlurryDumpDialog.BtnDeleteLoadGeneration.Enabled := True;

        if ASlurryDump.LoadGeneration <> nil then
        begin
          LoadGenerationCombo.Clear;
          LoadGenerationCombo.AddItem(ASlurryDump.LoadGeneration.Description,TObject(ASlurryDump.LoadGeneration.type_));
          LoadGenerationCombo.ItemIndex := LoadGenerationCombo.Items.IndexOf(ASlurryDump.LoadGeneration.Description);
          StdDeviationEdit.Text := FloatToStr(ASlurryDump.LoadGeneration.StandardDeviation); //FloatToStrF(ASlurryDump.LoadGeneration.StandardDeviation,ffNumber,5,2);
          FactorAndMeanGrid.Height := (FactorAndMeanGrid.DefaultRowHeight*FactorAndMeanGrid.RowCount) + Trunc(FactorAndMeanGrid.RowHeights[0]/2)+30;
          FactorAndMeanGrid.Cells[0,0] := FAppModules.Language.GetString('FactorsAndMeanGrid.SlurryDumpFlow');
          FactorAndMeanGrid.Cells[1,0] := FAppModules.Language.GetString('FactorsAndMeanGrid.SlurryDumpMean');

          FactorAndMeanGrid.FixedCols := 0;
          FactorAndMeanGrid.FixedRows := 1;
          FactorAndMeanGrid.ColCount := 2;
          FactorAndMeanGrid.RowCount := 11;
         // FactorAndMeanGrid.Height := (FactorAndMeanGrid.DefaultRowHeight*FactorAndMeanGrid.RowCount)+30;
          FactorAndMeanGrid.Width := 10+(1+FactorAndMeanGrid.DefaultColWidth*FactorAndMeanGrid.ColCount);

          for LRow := 0 to 9 do
          begin
            FactorAndMeanGrid.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('Flow'));
            FactorAndMeanGrid.Cells[0,LRow +1] := Format(FAppModules.FieldProperties.FieldProperty('Flow').FormatStringGrid,[ASlurryDump.LoadGeneration.FlowByIndex[LRow]]);
            FactorAndMeanGrid.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('MeanOfSalt'));
            FactorAndMeanGrid.Cells[1,LRow + 1] := Format(FAppModules.FieldProperties.FieldProperty('MeanOfSalt').FormatStringGrid,[ASlurryDump.LoadGeneration.MeanOfSaltByIndex[LRow]]);
          end;
          LoadGenerationCombo.ItemIndex := LoadGenerationCombo.Items.IndexOf(ASlurryDump.LoadGeneration.Description);

        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TPlanningSlurryDumpValidator.PopulateSlurryDumpGrid;
const OPNAME = 'TPlanningSlurryDumpValidator.PopulateSlurryDumpGrid';
var
  LIndex            : integer;
  LSlurryDump       : TPlanningSlurryDump;
  LMine             : TPlanningMine;
  LSlurryExist : boolean;
begin
  try
    LMine := TPlanningMine(TPlanningModelDataObject(FAppModules.Model.ModelData).CastNetworkFeaturesData.CastMineList
             .CastMineByNodeNumber[FIdentifier]);
    with MiningSlurryDumpDialog do
    begin

      SlurryDumpGrid.ColCount := 10;
      SlurryDumpGrid.RowCount := Max((SlurryDumpGrid.FixedRows + 1),(1 + LMine.SlurryDumpCount));
      LSlurryExist := LMine.SlurryDumpCount > 0;
      if (LSlurryExist) then
      begin
        for LIndex := 1 to LMine.SlurryDumpCount do
        begin
          LSlurryDump := LMine.CastSlurryDumpByIndex[LIndex-1];
          SlurryDumpGrid.SetFieldValue(0, LIndex,LSlurryDump.Identifier);
          SlurryDumpGrid.SetFieldValue(1, LIndex,LSlurryDump.DumpName);
          SlurryDumpGrid.SetFieldValue(2, LIndex,LSlurryDump.DumpSurfaceArea);
          SlurryDumpGrid.SetFieldValue(3, LIndex,LSlurryDump.RunoffFactorToPCD);
          SlurryDumpGrid.SetFieldValue(4, LIndex,LSlurryDump.SeepageSplitFactor);
          SlurryDumpGrid.SetFieldValue(5, LIndex,LSlurryDump.PCDStorageCapacity);
          SlurryDumpGrid.SetFieldValue(6, LIndex,LSlurryDump.PCDSurfaceArea);
          SlurryDumpGrid.SetFieldValue(7, LIndex,LSlurryDump.PCDAnalysisStartVolume);
          SlurryDumpGrid.SetFieldValue(9, LIndex,LSlurryDump.SaltConcentration);
          SlurryDumpGrid.Options := SlurryDumpGrid.Options + [goEditing];
          GrowthFactorControl.Visible := True;
          YearAndFactorGrid.Visible := True;
          PopulateGrowthFactorControl(LSlurryDump);
          LoadGenerationControl.Visible := True;
          PopulateLoadGenerationControl(LSlurryDump);

        end;
        OnGridSelectCell(SlurryDumpGrid, 0,1, LSlurryExist);
      end
      else
      begin
        SlurryDumpGrid.Cells[0, 1] := '';
        SlurryDumpGrid.Cells[1, 1] := '';
        SlurryDumpGrid.Cells[2, 1] := '';
        SlurryDumpGrid.Cells[3, 1] := '';
        SlurryDumpGrid.Cells[4, 1] := '';
        SlurryDumpGrid.Cells[5, 1] := '';
        SlurryDumpGrid.Cells[6, 1] := '';
        SlurryDumpGrid.Cells[7, 1] := '';
        SlurryDumpGrid.Cells[9, 1] := '';
        SlurryDumpGrid.Options := SlurryDumpGrid.Options - [goEditing];
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{*procedure TPlanningSlurryDumpValidator.PopulateYearAndFactorGrid(AGrowthFactor: TPlanningMineGrowthFactor);
const OPNAME = 'TPlanningSlurryDumpValidator.PopulateYearAndFactorGrid';
var
  LIndex : integer;
begin
   try
    if Assigned(AGrowthFactor) then
    begin
      with MiningSlurryDumpDialog do
      begin
        YearAndFactorGrid.ColCount := AGrowthFactor.NoOfPoints + 1;
        YearAndFactorGrid.Width := (10 + (1 + YearAndFactorGrid.DefaultColWidth) * (YearAndFactorGrid.ColCount-1)) + YearAndFactorGrid.ColWidths[0];
        YearAndFactorGrid.Top := GrowthFactorGrid.Top + GrowthFactorGrid.Height + 10;
        YearAndFactorGrid.ClearErrors;
        for LIndex := 1 to AGrowthFactor.NoOfPoints do
        begin
          YearAndFactorGrid.Cells[0,1] := AGrowthFactor.Description;
          YearAndFactorGrid.Cells[LIndex,0] := IntToStr(AGrowthFactor.NoOfYearsByIndex[LIndex - 1]);
          YearAndFactorGrid.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('GrowthFactors'));
          YearAndFactorGrid.Cells[LIndex,1] := Format(FAppModules.FieldProperties.FieldProperty('GrowthFactors').FormatStringGrid,[AGrowthFactor.GrowthFactorByIndex[LIndex-1]]);
          YearAndFactorGrid.Options := YearAndFactorGrid.Options + [goEditing];
        end;
        DoContextValidation(dvtNYR);
        DoContextValidation(dvtGrowthFactor);
      end;
    end
    else
    begin
      MiningSlurryDumpDialog.YearAndFactorGrid.ColCount := 2;
      MiningSlurryDumpDialog.YearAndFactorGrid.Cells[1,0] := '';
      MiningSlurryDumpDialog.YearAndFactorGrid.Cells[1,1] := '';
    end;
  except on E: Exception do HandleError(E,OPNAME); end;
end;*}

{*procedure TPlanningSlurryDumpValidator.refreshGrowthControl(Sender: TObject);
const OPNAME ='TPlanningSlurryDumpValidator.refreshGrowthControl';
var
  LMine : TPlanningMine;
  LSlurryDump : TPlanningSlurryDump;
begin
  try
    LMine := TPlanningMine(TPlanningModelDataObject(FAppModules.Model.ModelData).CastNetworkFeaturesData.CastMineList.CastMinebyNodeNumber[FIdentifier]);
     if LMine <> nil then
     begin
       LSlurryDump  := LMine.CastSlurryDumpByID[FSelectSlurryID];
       if LSlurryDump <> nil then
       begin
        PopulateGrowthFactorControl(LSlurryDump);
       end;
    end;
   except on E: Exception do HandleError(E, OPNAME) end;
end;*}

{*procedure TPlanningSlurryDumpValidator.refreshLoadGenerationControl(Sender: TObject);
const OPNAME = 'TPlanningSlurryDumpValidator.refreshLoadGenerationControl';
var
  LMine : TPlanningMine;
  LSlurryDump : TPlanningSlurryDump;
  LSelectedType : integer;
begin
  try
    LMine := TPlanningMine(TPlanningModelDataObject(FAppModules.Model.ModelData).CastNetworkFeaturesData.CastMineList.
    CastMinebyNodeNumber[FIdentifier]);
    LSlurryDump := LMine.CastSlurryDumpByID[FSelectSlurryID];
    if LSlurryDump <> nil then
    begin
      LSelectedType := Integer(MiningSlurryDumpDialog.LoadGenerationCombo.Items.Objects[MiningSlurryDumpDialog.LoadGenerationCombo.ItemIndex]);
      PopulateLoadGenerationControl(LSlurryDump);
    end;
  except on E: Exception do HandleError(E,OPNAME); end;
end;*}

procedure TPlanningSlurryDumpValidator.RePopulateDataViewer;
const OPNAME = 'TPlanningSlurryDumpValidator.RePopulateDataViewer';
var
  LMine  : TPlanningMine;
  LSlurryExist : boolean;
begin
  Inherited ;
  try
    ClearDataViewer;
    if(FIdentifier >= 0) then
    begin
      LMine := TPlanningMine(TPlanningModelDataObject(FAppModules.Model.ModelData).CastNetworkFeaturesData
                       .CastMineList.CastMineByNodeNumber[FIdentifier]);
      if LMine <> nil then
      begin
        LSlurryExist := LMine.SlurryDumpCount > 0;
        if  LSlurryExist then
        begin
          with MiningSlurryDumpDialog do
          begin
             NumberOfSlurryDumpEdt.SetFieldValue(LMine.SlurryDumpCount);
             PopulateSlurryDumpGrid;
             OnGridSelectCell(SlurryDumpGrid,1,1,LSlurryExist);
          end;
           MiningSlurryDumpDialog.ResetButtonState(LMine.SlurryDumpCount);
        end;
      end;

    end
    else
    begin
      MiningSlurryDumpDialog.BtnAddGrowthFactors.Enabled  := false;
      MiningSlurryDumpDialog.BtnDeleteGrowthFactors.Enabled := False;
      MiningSlurryDumpDialog.BtnAddLoadGeneration.Enabled := false;
      MiningSlurryDumpDialog.BtnDeleteLoadGeneration.Enabled := false;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TPlanningSlurryDumpValidator.UpdateInterpolationMethodCombo;
const OPNAME = 'TPlanningSlurryDumpValidator.UpdateInterpolationMethodCombo';
var

  LMessage : String;
  LMine    : TPlanningMine;
  LSlurryDumpData : TPlanningSlurryDump;
  LField : TPlanningMineGrowthFactor;
  LValue : Integer;

begin
   try
     with MiningSlurryDumpDialog do
      begin
       LMine := TPlanningMine(TPlanningModelDataObject(FAppModules.Model.ModelData).CastNetworkFeaturesData.CastMineList.MineByNodeNumber[FIdentifier]);
       LSlurryDumpData := LMine.CastSlurryDumpByID[FSelectSlurryID];
       if (LSlurryDumpData <> nil) then
        begin
        LField := LSlurryDumpData.GrowthFactor;
        if LField <> nil then
        begin
        LValue :=  Integer(InterpolationCheckBox.Items.Objects[InterpolationCheckBox.ItemIndex]);
          if (FAppModules.FieldProperties.ValidateFieldProperty('InterpolationMethod', IntToStr(LValue), LMessage)) then
             begin
             LField.InterpolationMethod := LValue;
             RePopulateDataViewer;
             DoContextValidation(dvtInterpolationMethod);
             end
             else
             InterpolationCheckBox.ValidationError := LMessage;

          end;
       end;
      end;
   except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TPlanningSlurryDumpValidator.UpDateMeanOfSalt(ACol, ARow: integer);
const OPNAME = 'TPlanningSlurryDumpValidator.UpDateMeanOfSalt';
var
  LMine : TPlanningMine;
  LSlurryDump : TPlanningSlurryDump;
  LLoadGeneration : TLoadGeneration;
  LMessage : string;
  LCellVal : string;
begin
  try
    with MiningSlurryDumpDialog do
    begin
      LMine :=  TPlanningMine(TPlanningModelDataObject(FAppModules.Model.ModelData).CastNetworkFeaturesData.CastMineList.CastMinebyNodeNumber[FIdentifier]);
      if LMine <> nil then
      begin
        LSlurryDump := LMine.CastSlurryDumpByID[FSelectSlurryID];
        if LSlurryDump <> nil then
        begin

            LCellVal := FactorAndMeanGrid.Cells[FactorAndMeanGrid.Col, FactorAndMeanGrid.Row];
            if (FAppModules.FieldProperties.ValidateFieldProperty('MeanOfSalt',LCellVal, LMessage,ARow)) then
            begin
              LLoadGeneration := LSlurryDump.LoadGeneration;
              if LLoadGeneration <> nil then
              begin
                LLoadGeneration.MeanOfSaltByIndex[ARow - 1]  := StrToFloat(FactorAndMeanGrid.Cells[ACol,ARow]);
                PopulateLoadGenerationControl(LSlurryDump);
                DoContextValidation(dvtMeanOfSalt);
              end;
            end
            else
              FactorAndMeanGrid.ValidationError[ACol, ARow,gveCellField] := LMessage;
          end;
        end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TPlanningSlurryDumpValidator.UpdateNoOfDataPoints(ACol : Integer; ARow: integer);
const OPNAME = 'TPlanningSlurryDumpValidator.UpdateNoOfDataPoints';
var

  LMine : TPlanningMine;
  LSlurryDumpData : TPlanningSlurryDump;
  LField : TPlanningMineGrowthFactor;
  LMessage : String;
  LValue : String;

begin
  try
    with MiningSlurryDumpDialog do
    begin
      LMine :=  TPlanningMine(TPlanningModelDataObject(FAppModules.Model.ModelData).CastNetworkFeaturesData.CastMineList.CastMinebyNodeNumber[FIdentifier]);
      if LMine <> nil then
      begin
        LSlurryDumpData := LMine.CastSlurryDumpByID[FSelectSlurryID];
        if LSlurryDumpData <> nil then
        begin
          with MiningSlurryDumpDialog do
          begin
            LValue := YearAndFactorGrid.Cells[ACol,ARow];
            if (FAppModules.FieldProperties.ValidateFieldProperty(
                'NYR',LValue, LMessage)) then
            begin
            LField := LSlurryDumpData.GrowthFactor;
              if LField <> nil then
              begin
                LField.NoOfYearsByIndex[ACol -1]  := StrToInt(YearAndFactorGrid.Cells[ACol,ARow]);
                RePopulateDataViewer;
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

procedure TPlanningSlurryDumpValidator.UpdateFlow(ACol, ARow: integer);
const OPNAME = 'TPlanningSlurryDumpValidator.UpdateFlow';
var
  LMine : TPlanningMine;
  LSlurryDump : TPlanningSlurryDump;
  LLoadGeneration : TLoadGeneration;
  LMessage : string;
  LCellVal : string;
begin
  try
    with MiningSlurryDumpDialog do
    begin
      LMine :=  TPlanningMine(TPlanningModelDataObject(FAppModules.Model.ModelData).CastNetworkFeaturesData.CastMineList.CastMinebyNodeNumber[FIdentifier]);
      if LMine <> nil then
      begin
        LSlurryDump := LMine.CastSlurryDumpByID[FSelectSlurryID];
        if LSlurryDump <> nil then
        begin
            LCellVal := FactorAndMeanGrid.Cells[FactorAndMeanGrid.Col, FactorAndMeanGrid.Row];
            if (FAppModules.FieldProperties.ValidateFieldProperty('Flow',LCellVal, LMessage,ARow)) then
            begin
              LLoadGeneration := LSlurryDump.LoadGeneration;
              if LLoadGeneration <> nil then
              begin
                LLoadGeneration.FlowByIndex[ARow - 1]  := StrToFloat(FactorAndMeanGrid.Cells[ACol,ARow]);
                PopulateLoadGenerationControl(LSlurryDump);
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

procedure TPlanningSlurryDumpValidator.UpdateGrowthFactor(ACol : Integer; ARow: integer);
const OPNAME = 'TPlanningSlurryDumpValidator.UpdateGrowthFactor';
var

 LMine : TPlanningMine;
 LSlurryDumpData : TPlanningSlurryDump;
 LField : TPlanningMineGrowthFactor;
 LMessage : String;
 LValue : String;

begin
  try
    with MiningSlurryDumpDialog do
    begin
      LMine :=  TPlanningMine(TPlanningModelDataObject(FAppModules.Model.ModelData).CastNetworkFeaturesData.CastMineList.CastMinebyNodeNumber[FIdentifier]);
      if LMine <> nil then
      begin
        LSlurryDumpData := LMine.CastSlurryDumpByID[FSelectSlurryID];
        if LSlurryDumpData <> nil then
        begin
          with MiningSlurryDumpDialog do
          begin
            LValue := YearAndFactorGrid.Cells[ACol,ARow];
            if (FAppModules.FieldProperties.ValidateFieldProperty(
                'GrowthFactors',LValue, LMessage)) then
            begin
            LField := LSlurryDumpData.GrowthFactor;
              if LField <> nil then
              begin
                LField.GrowthFactorByIndex[ACol -1]  := StrToFloat(Trim(YearAndFactorGrid.Cells[ACol,ARow]));
                RePopulateDataViewer;
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

procedure TPlanningSlurryDumpValidator.UpdateNoOfPoints;
const OPNAME = 'TPlanningSlurryDumpValidator.UpdateNoOfPoints';
var

  LMessage : String;
  LMine : TPlanningMine;
  LSlurryDumpData : TPlanningSlurryDump;
  LField : TPlanningMineGrowthFactor;
  LValue : Integer;
  LFieldEdit : TFieldEdit;

begin
  try
   with MiningSlurryDumpDialog do
   begin
     LMine:= TPlanningMine(TPlanningModelDataObject(FAppModules.Model.ModelData).CastNetworkFeaturesData.CastMineList.MineByNodeNumber[FIdentifier]);
     LSlurryDumpData := LMine.CastSlurryDumpByID[FSelectSlurryID];
     if (LSlurryDumpData <> nil) then
      begin
      LField := LSlurryDumpData.GrowthFactor;
      if LField <> nil then
      begin
      LFieldEdit := NoOfPointsEdit;
      LValue := StrToInt(LFieldEdit.Text);
        if (FAppModules.FieldProperties.ValidateFieldProperty('NoOfPoints', IntToStr(LValue), LMessage)) then
          begin
          LField.NoOfPoints := LValue;
          RePopulateDataViewer;
          DoContextValidation(dvtNoOfPoints);
          end
          else
          NoOfPointsEdit.FieldValidationError := LMessage;

      end;
     end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TPlanningSlurryDumpValidator.UpDateSaltC(ACol, ARow: integer);
const OPNAME = 'TPlanningSlurryDumpValidator.UpDateSaltC';
var
  LMine : TPlanningMine;
  LSlurryDump : TPlanningSlurryDump;
  LMessage : string;
  LValue : string;
begin
  try
    with MiningSlurryDumpDialog do
    begin
      LMine :=  TPlanningMine(TPlanningModelDataObject(FAppModules.Model.ModelData).CastNetworkFeaturesData.CastMineList.CastMinebyNodeNumber[FIdentifier]);
      if LMine <> nil then
      begin
        LSlurryDump := LMine.CastSlurryDumpByID[FSelectSlurryID];
        if LSlurryDump <> nil then
        begin
            LValue := SlurryDumpGrid.Cells[SlurryDumpGrid.Col, SlurryDumpGrid.Row];
            if (FAppModules.FieldProperties.ValidateFieldProperty(
                'SaltConcentration',LValue, LMessage,ARow)) then
              begin
                LSlurryDump.SaltConcentration := StrToFloat(SlurryDumpGrid.Cells[ACol,ARow]);
                PopulateSlurryDumpGrid;
              end;
            end
            else
            SlurryDumpGrid.ValidationError[ACol, ARow,gveCellField] := LMessage;
          end;
        end;
     except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TPlanningSlurryDumpValidator.UpdateStandardDev;
const OPNAME = 'TPlanningSlurryDumpValidator.UpdateStandardDev';
var

  LMessage : String;
  LMine : TPlanningMine;
  LSlurryDumpData : TPlanningSlurryDump;
  LField : TLoadGeneration;
  LValue : double;

begin
   try
    with MiningSlurryDumpDialog do
    begin
     LMine := TPlanningMine(TPlanningModelDataObject(FAppModules.Model.ModelData).CastNetworkFeaturesData.CastMineList.CastMineByNodeNumber[FIdentifier]);
     LSlurryDumpData := LMine.CastSlurryDumpByID[FSelectSlurryID];
     if (LSlurryDumpData <> nil) then
      begin
       LField := LSlurryDumpData.LoadGeneration;
          if LField <> nil then
          begin
            LValue := StrToFloat(StdDeviationEdit.Text);
            if (FAppModules.FieldProperties.ValidateFieldProperty('StdDeviation', FloatToStr(LValue) , LMessage)) then
            begin
            LField.StandardDeviation := LValue;
            RePopulateDataViewer ;
            DoContextValidation(dvtStandardDeviation);
            end
            else
            StdDeviationEdit.FieldValidationError := LMessage;

          end;
          end;
    end;
   except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TPlanningSlurryDumpValidator.ValidateNoOfDataPoint(AGrowthFactor : TPlanningMineGrowthFactor);
const OPNAME = 'TPlanningSlurryDumpValidator.ValidateNoOfDataPoint';
var

 LCol : Integer;

begin
   try
     if AGrowthFactor <> nil then
        begin
          with MiningSlurryDumpDialog do
          begin
          FErrorMessage := '';
          if (AGrowthFactor.Validate(FErrorMessage,'NYR')) then
              begin
              for LCol := 0 to 0 do

                 YearAndFactorGrid.ValidationError[1, lCol, gveCellContext] := ''
              end
              else
              FAllErrorMessages.Add(FErrorMessage);
          end;
        end;
   except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TPlanningSlurryDumpValidator.ValidateFlow(
  ALoadGeneration: TLoadGeneration);
  const OPNAME = 'TPlanningSlurryDumpValidator.ValidateFlow';
var

 LCol : Integer;

begin
   try
     if ALoadGeneration <> nil then
        begin
          with MiningSlurryDumpDialog do
          begin
          FErrorMessage := '';
          if (ALoadGeneration.Validate(FErrorMessage,'Flow')) then
              begin
              for LCol := 0 to 0 do

                 FactorAndMeanGrid.ValidationError[1, lCol, gveCellContext] := ''
              end
              else
              FAllErrorMessages.Add(FErrorMessage);
          end;
        end;
   except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TPlanningSlurryDumpValidator.ValidateGrowthFactor(AGrowthFactor : TPlanningMineGrowthFactor);
const OPNAME = 'TPlanningSlurryDumpValidator.ValidateGrowthFactor';
var

 LCol : Integer;

begin
   try
    if AGrowthFactor <> nil then
       begin
         with MiningSlurryDumpDialog do
         begin
         FErrorMessage := '';
         if (AGrowthFactor.Validate(FErrorMessage,'GrowthFactors')) then
              begin
              for LCol := 0 to 0 do

                 YearAndFactorGrid.ValidationError[1, lCol, gveCellContext] := ''
              end
              else
              FAllErrorMessages.Add(FErrorMessage);
         end;

       end;
   except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TPlanningSlurryDumpValidator.ValidateInterpolationMethodCombo(AGrowthFactor : TPlanningMineGrowthFactor);
const OPNAME = 'TPlanningSlurryDumpValidator.ValidateInterpolationMethodCombo';
begin
   try
     with MiningSlurryDumpDialog do
        begin
           if AGrowthFactor <> nil then
           begin
           FErrorMessage := '';
           AGrowthFactor.Validate(FErrorMessage,'InterpolationMethod');
           InterpolationCheckBox.ValidationError := FErrorMessage;
         end;
       end;
   except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TPlanningSlurryDumpValidator.ValidateMeanOfSalt(
  ALoadGeneration: TLoadGeneration);
  const OPNAME = 'TPlanningSlurryDumpValidator.ValidateMeanOfSalt';
var

 LCol : Integer;

begin
   try
     if ALoadGeneration <> nil then
        begin
          with MiningSlurryDumpDialog do
          begin
          FErrorMessage := '';
          if (ALoadGeneration.Validate(FErrorMessage,'MeanOfSalt')) then
              begin
              for LCol := 0 to 0 do

                 FactorAndMeanGrid.ValidationError[1, lCol, gveCellContext] := ''
              end
              else
              FAllErrorMessages.Add(FErrorMessage);
          end;
        end;
   except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TPlanningSlurryDumpValidator.ValidateNoOfPoint(AGrowthFactor : TPlanningMineGrowthFactor);
const OPNAME = 'TPlanningSlurryDumpValidator.ValidateNoOfPoint';
begin
  try
     with MiningSlurryDumpDialog do
       begin
         if AGrowthFactor <> nil then
         begin
         FErrorMessage := '';
         AGrowthFactor.Validate(FErrorMessage,'NoOfPoints');
         NoOfPointsEdit.ContextValidationError := FErrorMessage;
       end;
      end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TPlanningSlurryDumpValidator.ValidateStandardDev(ALoadGeneration : TLoadGeneration);
const OPNAME = 'TPlanningSlurryDumpValidator.ValidateStandardDev';
begin
  try
    with MiningSlurryDumpDialog do
      begin
         if ALoadGeneration <> nil then
         begin
         FErrorMessage := '';
         ALoadGeneration.Validate(FErrorMessage, 'StdDeviation');
         StdDeviationEdit.ContextValidationError := FErrorMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.



