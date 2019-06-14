unit UMultiResChannelValidator;

interface

uses
  Classes,
  VCL.Controls,
  VCL.ComCtrls,
  VCL.StdCtrls,
  VCL.ExtCtrls,
  Types,
  VCL.Grids,
  VCL.Dialogs,
  UAbstractObject,
  UAbstractComponent,
  UDataComponent,
  UDataEditComponent,
  UYieldContextValidationType,
  VoaimsCom_TLB,
  UAbstractYieldDataDialogValidator,
  UMultiResChannelDialog;

type
  TMultiResChannelValidator = class(TAbstractYieldDataDialogValidator)
  protected
    FFeatureID: integer;
    FChannelNr: integer;
    FReservoirNr: integer;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    procedure OnEditControlEnter(Sender: TObject); override;
    procedure OnEditControltExit(Sender: TObject); override;
    procedure OnStringGridCellDataHasChanged(ASender: TObject; ACol, ARow: integer); override;
    procedure ValidateDecisionMonth(AFeature: IMultiResMultiChannelCurtail);
    procedure ValidateStartMonth(AFeature: IMultiResMultiChannelCurtail);
    procedure ValidateFactorValue(AFeature: IMultiResMultiChannelCurtail);
    procedure ValidateReservoirNo(AFeature: IMultiResMultiChannelCurtail);
    procedure ValidateElevationValue(AFeature: IMultiResMultiChannelCurtail);

    procedure UpdateReservoirNoCombo;
    procedure UpdateDecisionMonth;
    procedure UpdateStartMonth;

    procedure UpdateElevation(ACol, ARow : integer);
    procedure UpdateFactor(ACol, ARow : integer);

  public
    function Initialise: boolean; override;
    function LanguageHasChanged: boolean; override;
    function StudyHasChanged: boolean; override;
    function SaveState: boolean; override;
    function StudyDataHasChanged(AContext: TChangeContext;
      AFieldName, AOldValue, ANewValue: string): boolean; override;
    function ProcessMetaDataEvent: boolean; override;
    procedure ClearDataViewer; override;
    procedure PopulateDataViewer; override;
    procedure RePopulateData;
    procedure DoContextValidation(AValidationType: Integer); override;


    function MultiResChannelDialog: TMultiResChannelDialog;
    property FeatureID: integer read FFeatureID write FFeatureID;
//    property ChannelNr: integer read FChannelNr write FChannelNr;
//    property ReservoirNr: integer read FReservoirNr write FReservoirNr;
  end;

implementation

uses
  // VCL.Dialogs,
  VCL.Forms,
  SysUtils,
  UConstants,
  UUtilities,
  // VCL.Grids,
  // UPlanningModelDataGUIForm,
  UReservoirData,
  UViewDataItem,
  UPlanningModelDataObject,
  UTreeViewTabSheet,
  UErrorHandlingOperations;

{ ****************************************************************************** }
{ * TDiversionFeatureValidator                                                 * }
{ ****************************************************************************** }

procedure TMultiResChannelValidator.ClearDataViewer;
const OPNAME = 'TMultiResChannelValidator.ClearDataViewer';
var
  LCol, LRow : integer;
begin
  inherited;
  try
      with MultiResChannelDialog do
      begin
        DecisionMonthField.Text := '';
        StartMonthField.Text := '';
        ReservoirNoCombo.Text := '';
        CurtailRestrictionGrid.FixedRows := 1;
        CurtailRestrictionGrid.FixedCols := 0;
        CurtailRestrictionGrid.ColCount := 2;
        CurtailRestrictionGrid.RowCount := 11;

        CurtailRestrictionGrid.cells[0,0] := FAppModules.Language.GetString('CurtailRestrictionGrid.ElevationHeading');
        CurtailRestrictionGrid.cells[1,0] := FAppModules.Language.GetString('CurtailRestrictionGrid.FactorHeading');

        for LCol := 0 to 1 do
          for lRow := 1 to 10 do
            CurtailRestrictionGrid.cells[LCol,LRow] := '';
         DecisionMonthField.Clear;
         for LCol := 1 to 12 do
           DecisionMonthField.Items.AddObject(IntToStr(LCol),TObject(LCol));

         StartMonthField.Clear;
         for LCol := 1 to 12 do
           StartMonthField.Items.AddObject(IntToStr(LCol),TObject(LCol));

      end;
  except
    on E: Exception do
      HandleError(E, OPNAME)
  end;
end;

procedure TMultiResChannelValidator.CreateMemberObjects;
const
  OPNAME = 'TMultiResChannelValidator.CreateMemberObjects';
var
  lPanel: TMultiResChannelDialog;
begin
  try
  inherited CreateMemberObjects;

  FPanel := TMultiResChannelDialog.Create(FPanelOwner, FAppModules);
  lPanel := MultiResChannelDialog;
  with lPanel do
  begin

    ReservoirNoCombo.FieldProperty := FAppModules.FieldProperties.FieldProperty
      ('MultiCurReservoir');
    ReservoirNoCombo.OnSelect:= OnEditControltExit;

    DecisionMonthField.FieldProperty :=
      FAppModules.FieldProperties.FieldProperty('MultiCurDecisionMonth');
    DecisionMonthField.OnEnter := OnEditControlEnter;
    DecisionMonthField.OnExit := OnEditControltExit;

    StartMonthField.FieldProperty := FAppModules.FieldProperties.FieldProperty
      ('MultiCurStartMonth');
    StartMonthField.OnExit := OnEditControltExit;
    StartMonthField.OnEnter := OnEditControlEnter;
    CurtailRestrictionGrid.AddFieldProperty
      (FAppModules.FieldProperties.FieldProperty('MultiCurElevation'));
    CurtailRestrictionGrid.AddFieldProperty
      (FAppModules.FieldProperties.FieldProperty('MultiCurFactor'));

    CurtailRestrictionGrid.OnBeforeCellChange := OnStringGridCellDataHasChanged;

  end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMultiResChannelValidator.DestroyMemberObjects;
const
  OPNAME = 'TDiversionFeatureValidator.DestroyMemberObjects';
begin
  try
    inherited DestroyMemberObjects;

  except
    on E: Exception do
      HandleError(E, OPNAME)
  end;
end;

procedure TMultiResChannelValidator.DoContextValidation(AValidationType: Integer);
const OPNAME = 'TDiversionFeatureValidator.DestroyMemberObjects';
var
   LCurtail: IMultiResMultiChannelCurtail;
begin
  try
    inherited;
    LCurtail := TPlanningModelDataObject(FAppModules.Model.ModelData).
                CastMultiRestrictionData.RestrictionByIndentifier[FFeatureID];
    case AValidationType of
      dvtMultiRestrictionDecisionMonth : ValidateDecisionMonth(LCurtail);
      dvtMultiRestrictionStartMonth : ValidateStartMonth(LCurtail);
      dvtMultiRestrictionRes : ValidateReservoirNo(LCurtail);
      dvtMultiRestrictionElev : ValidateElevationValue(LCurtail);
      dvtMultiRestrictionFactor : ValidateFactorValue(LCurtail);


    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMultiResChannelValidator.Initialise: boolean;
const
  OPNAME = 'TMultiResChannelValidator.Initialise';
begin
  Result := inherited Initialise;
  try



  except
    on E: Exception do
      HandleError(E, OPNAME)
  end;
end;

function TMultiResChannelValidator.LanguageHasChanged: boolean;
const
  OPNAME = 'TMultiResChannelValidator.LanguageHasChanged';
begin
  Result := False;
  try
    Result := inherited LanguageHasChanged;
    FTabShetCaption := FAppModules.Language.GetString
      ('TMultiResChannelValidator.Caption');

  except
    on E: Exception do
      HandleError(E, OPNAME)
  end;
end;

function TMultiResChannelValidator.MultiResChannelDialog
  : TMultiResChannelDialog;
const
  OPNAME = 'TMultiResChannelValidator.MultiResChannelDialog';
begin
  Result := Nil;
  try
    if (FPanel <> nil) then
      Result := TMultiResChannelDialog(FPanel);
  except
    on E: Exception do
      HandleError(E, OPNAME)
  end;
end;

procedure TMultiResChannelValidator.OnEditControlEnter(Sender: TObject);
const  OPNAME = 'TMultiResChannelValidator.OnEditControlEnter';
begin
  inherited OnEditControlEnter(Sender);
  try

  except
    on E: Exception do
      HandleError(E, OPNAME)
  end;
end;

procedure TMultiResChannelValidator.OnEditControltExit(Sender: TObject);
const
  OPNAME = 'TMultiResChannelValidator.OnEditControltExit';
begin
  try
    with MultiResChannelDialog do
    begin
      if (Sender = ReservoirNoCombo) then
        UpdateReservoirNoCombo
      else
      if (Sender = DecisionMonthField) then
        UpdateDecisionMonth
      else
      if (Sender = StartMonthField) then
        UpdateStartMonth;
    end;
  except
    on E: Exception do
      HandleError(E, OPNAME)
  end;
  inherited OnEditControltExit(Sender);
end;

procedure TMultiResChannelValidator.OnStringGridCellDataHasChanged
  (ASender: TObject; ACol, ARow: integer);
const
  OPNAME = 'TMultiResChannelValidator.OnStringGridCellDataHasChanged';
begin
  inherited OnStringGridCellDataHasChanged(ASender, ACol, ARow);
  try
    if (ACol = 0) then
      UpdateElevation(ACol, ARow)
    else
    if (ACol = 1) then
      UpdateFactor(ACol, ARow)
  except
    on E: Exception do
      HandleError(E, OPNAME)
  end;
end;

procedure TMultiResChannelValidator.PopulateDataViewer;
const OPNAME = 'TPhysicalFlowConstraintValidator.PopulateDataViewer';
begin
  try
    ClearDataViewer;
    RePopulateData;
    DoContextValidation(dvtMultiRestrictionAll);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMultiResChannelValidator.ProcessMetaDataEvent: boolean;
begin
  Result := True;
end;

procedure TMultiResChannelValidator.RePopulateData;
const
  OPNAME = 'TPhysicalFlowConstraintValidator.PopulateDataViewer';
var
  lRestriction: IMultiResMultiChannelCurtail;
  lCount: integer;
  lReservoir: IReservoirData;
  lReservoirCount: integer;
  LReservoirList : IReservoirDataList;
begin
  try
    if (FFeatureID >= 0) then
    begin
      lRestriction := TPlanningModelDataObject(FAppModules.Model.ModelData).CastMultiRestrictionData.RestrictionByIndentifier[FFeatureID];
      if (lRestriction <> nil) then
      begin
        with MultiResChannelDialog do
        begin

          CurtailRestrictionGrid.Width := 3 + (1 + CurtailRestrictionGrid.DefaultColWidth) * CurtailRestrictionGrid.ColCount;

          CurtailRestrictionGrid.Height := 3 + (1 + CurtailRestrictionGrid.DefaultRowHeight) * CurtailRestrictionGrid.RowCount;



          DecisionMonthField.ItemIndex := DecisionMonthField.Items.IndexOf(IntToStr(lRestriction.DecisionMonth));
          StartMonthField.ItemIndex := StartMonthField.Items.IndexOf(IntToStr(lRestriction.StartMonth));

          CurtailRestrictionGrid.Cells[0, 0] := FAppModules.Language.GetString('CurtailRestrictionGrid.ElevationHeading');
          CurtailRestrictionGrid.Cells[1, 0] := FAppModules.Language.GetString('CurtailRestrictionGrid.FactorHeading');

          for lCount := 1 to 10 do
          begin
            CurtailRestrictionGrid.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('MultiCurElevation'));
            CurtailRestrictionGrid.Cells[0, lCount] := Format(FAppModules.FieldProperties.FieldProperty('MultiCurElevation').FormatStringGrid,[lRestriction.ElevationByIndex[lCount-1]]);

            CurtailRestrictionGrid.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('MultiCurFactor'));
            CurtailRestrictionGrid.Cells[1, lCount] := Format(FAppModules.FieldProperties.FieldProperty('MultiCurFactor').FormatStringGrid,[lRestriction.FactorByIndex[lCount-1]]);
          end;
          LReservoirList := TPlanningModelDataObject(FAppModules.Model.ModelData).NetworkElementData.ReservoirList;
          LReservoirCount := LReservoirList.ReservoirCount;
          ReservoirNoCombo.Items.Clear;
          for lCount := 0 to lReservoirCount - 1 do
          begin
            if LReservoirList <> nil then
            begin
              lReservoir := LReservoirList.ReservoirByIndex[lCount];
              if lReservoir <> nil then
                if lReservoir.ReservoirConfigurationData.NodeType = ntReservoir then
                  ReservoirNoCombo.Items.AddObject(LReservoir.ReservoirConfigurationData.ReservoirName,TObject(LReservoir.ReservoirConfigurationData.ReservoirIdentifier));
            end;
          end;
           ReservoirNoCombo.SetFieldIndex(ReservoirNoCombo.Items.IndexOfObject(TObject(lRestriction.ReservoirNo)));

        end;
      end;
    end;
  except
    on E: Exception do
      HandleError(E, OPNAME)
  end;
end;

procedure TMultiResChannelValidator.ValidateDecisionMonth
  (AFeature: IMultiResMultiChannelCurtail);
const
  OPNAME = 'TMultiResChannelValidator.ValidateDecisionMonth';
begin
  try
    if AFeature <> nil then
    begin
      FErrorMessage := '';
      if (NOT AFeature.Validate(FErrorMessage, 'DecisionMonth')) then
        FAllErrorMessages.Add(Trim(FErrorMessage));

      MultiResChannelDialog.DecisionMonthField.ValidationError := FErrorMessage;

    end;
  except
    on E: Exception do
      HandleError(E, OPNAME)
  end;
end;

procedure TMultiResChannelValidator.ValidateStartMonth
  (AFeature: IMultiResMultiChannelCurtail);
const
  OPNAME = 'TMultiResChannelValidator.ValidateStartMonth';
begin
  try
    if AFeature <> nil then
    begin
      FErrorMessage := '';
      if (Not AFeature.Validate(FErrorMessage, 'StartMonth')) then
        FAllErrorMessages.Add(Trim(FErrorMessage));

      MultiResChannelDialog.StartMonthField.ValidationError :=
        FErrorMessage;
    end;
  except
    on E: Exception do
      HandleError(E, OPNAME)
  end;
end;

// Implement Study as changed
function TMultiResChannelValidator.SaveState: boolean;
begin
  Result := True;
end;

function TMultiResChannelValidator.StudyDataHasChanged(AContext: TChangeContext;
  AFieldName, AOldValue, ANewValue: string): boolean;
const
  OPNAME = 'TMultiResChannelValidator.StudyHasChanged';
begin
  Result := inherited StudyDataHasChanged(AContext, AFieldName, AOldValue,
    ANewValue);
  try
    Result := True;
  except
    on E: Exception do
      HandleError(E, OPNAME)
  end;
end;

function TMultiResChannelValidator.StudyHasChanged;
const
  OPNAME = 'TMultiResChannelValidator.StudyHasChanged';
begin
  Result := inherited StudyHasChanged;
  try

  except
    on E: Exception do
      HandleError(E, OPNAME)
  end;
end;

(*

      end;

      if (Sender = StartMonthField) then
      begin
        if (FAppModules.FieldProperties.ValidateFieldProperty
          ('MultiCurStartMonth', StartMonthField.Text, lstring)) then
        begin
          StartMonthField.FieldValidationError := lstring;
          lFeature.StartMonth := StrToInt(Trim(StartMonthField.Text));
          DecisionMonthField.SetFieldValue(lFeature.StartMonth);
          if (not(lFeature.Validate(FErrorMessage, 'StartMonth'))) then
            FAllErrorMessages.Add(Trim(FErrorMessage));
          StartMonthField.ContextValidationError := FErrorMessage;
        end;
      end;

    end;

*)


procedure TMultiResChannelValidator.UpdateDecisionMonth;
const OPNAME = 'TMultiResChannelValidator.UpdateDecisionMonth';
var
  LMessage: string;
  LCurtail: IMultiResMultiChannelCurtail;
begin
  try
    LCurtail := TPlanningModelDataObject(FAppModules.Model.ModelData).
                CastMultiRestrictionData.RestrictionByIndentifier[FFeatureID];
    with MultiResChannelDialog do
    begin
      if (FAppModules.FieldProperties.ValidateFieldProperty('MultiCurDecisionMonth', DecisionMonthField.Text, LMessage )) then
      begin
        LCurtail.DecisionMonth := StrToInt(Trim(DecisionMonthField.Text));
        RePopulateData;
        DoContextValidation(dvtMultiRestrictionDecisionMonth);
      end
      else
        DecisionMonthField.ValidationError := LMessage;
    end;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMultiResChannelValidator.UpdateElevation(ACol, ARow: integer);
const OPNAME = 'TMultiResChannelValidator.UpdateElevation';
var
  LStrValue,
  LMessage: string;
  LCurtail: IMultiResMultiChannelCurtail;
begin
  try
    LCurtail := TPlanningModelDataObject(FAppModules.Model.ModelData).
                CastMultiRestrictionData.RestrictionByIndentifier[FFeatureID];
    with MultiResChannelDialog do
    begin
       LStrValue := Trim(CurtailRestrictionGrid.Cells[ACol,ARow]);
       if (LStrValue = '') then
         LStrValue := '0.0';

       if (FAppModules.FieldProperties.ValidateFieldProperty(
              'MultiCurElevation', LStrValue,LMessage, ARow)) then
       begin
         LCurtail.ElevationByIndex[ARow-1] := StrToFloat(LStrValue);
         RePopulateData;
         DoContextValidation(dvtMultiRestrictionElev);
       end;

    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMultiResChannelValidator.UpdateFactor(ACol, ARow: integer);
const OPNAME = 'TMultiResChannelValidator.UpdateElevation';
var
  LStrValue,
  LMessage: string;
  LCurtail: IMultiResMultiChannelCurtail;
begin
  try
    LCurtail := TPlanningModelDataObject(FAppModules.Model.ModelData).
                CastMultiRestrictionData.RestrictionByIndentifier[FFeatureID];
    with MultiResChannelDialog do
    begin
       LStrValue := Trim(CurtailRestrictionGrid.Cells[ACol,ARow]);
       if (LStrValue = '') then
         LStrValue := '0.0';

       if (FAppModules.FieldProperties.ValidateFieldProperty(
              'MultiCurFactor', LStrValue,LMessage, ARow)) then
       begin
         LCurtail.FactorByIndex[ARow-1] := StrToFloat(LStrValue);
         RePopulateData;
         DoContextValidation(dvtMultiRestrictionFactor);
       end
       else
       begin
         FErrorMessage := 'ERROR:'+LMessage ;
         CurtailRestrictionGrid.ValidationError[1, ARow, gveCellContext] := FErrorMessage;
       end;

    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMultiResChannelValidator.UpdateReservoirNoCombo;
const
  OPNAME = 'TMultiResChannelValidator.UpdateReservoirNoCombo';
var
  LMessage: string;
  LCurtail: IMultiResMultiChannelCurtail;
  LIndex,
  LReservoirNo : integer;
begin
  try
    LCurtail := TPlanningModelDataObject(FAppModules.Model.ModelData).
                CastMultiRestrictionData.RestrictionByIndentifier[FFeatureID];
    with MultiResChannelDialog do
    begin
      LIndex := ReservoirNoCombo.ItemIndex;
      LReservoirNo := Integer(ReservoirNoCombo.Items.Objects[LIndex]);
      if (FAppModules.FieldProperties.ValidateFieldProperty('MultiCurReservoir',IntToStr(LReservoirNo) , LMessage )) then
      begin
        LCurtail.ReservoirNo := LReservoirNo;
        RePopulateData;
        DoContextValidation(dvtMultiRestrictionRes);
      end
      else
        ReservoirNoCombo.ValidationError := LMessage;
    end;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMultiResChannelValidator.UpdateStartMonth;
const OPNAME = 'TMultiResChannelValidator.UpdateStartMonth';
var
  LMessage: string;
  LCurtail: IMultiResMultiChannelCurtail;
begin
  try
    LCurtail := TPlanningModelDataObject(FAppModules.Model.ModelData).
                CastMultiRestrictionData.RestrictionByIndentifier[FFeatureID];
    with MultiResChannelDialog do
    begin
      if (FAppModules.FieldProperties.ValidateFieldProperty('MultiCurStartMonth', StartMonthField.Text, LMessage )) then
      begin
        LCurtail.StartMonth := StrToInt(Trim(StartMonthField.Text));
        RePopulateData;
        DoContextValidation(dvtMultiRestrictionStartMonth);
      end
      else
        StartMonthField.ValidationError := LMessage;
    end;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMultiResChannelValidator.ValidateFactorValue(AFeature: IMultiResMultiChannelCurtail);
const OPNAME = 'TMultiResChannelValidator.ValidateFactorValue';
var
  LErrorCols  : TStringList;
  LErrorMsgs  : TStringList;
  LFieldProperties: TAbstractFieldProperty;
  LIndex,
  LRow : integer;
begin
  try
    LErrorCols  := TStringList.Create;
    LErrorMsgs  := TStringList.Create;
    LFieldProperties := FAppModules.FieldProperties.FieldProperty('MultiCurFactor');
    try
      if AFeature <> nil then
      begin
        if AFeature.Validate(FErrorMessage,'Factor') then
          for LRow := LFieldProperties.ArrayLow to LFieldProperties.ArrayHigh do
            MultiResChannelDialog.CurtailRestrictionGrid.ValidationError[1, LRow, gveCellContext] := ''
        else
        begin
          ExtractErrorsAndColumns(FErrorMessage, LErrorMsgs, LErrorCols);
          for LRow := LFieldProperties.ArrayLow to LFieldProperties.ArrayHigh do
          begin
            LIndex := LErrorCols.IndexOf(IntToStr(LRow));
            if (LIndex >= 0) then
              MultiResChannelDialog.CurtailRestrictionGrid.ValidationError[1, LRow, gveCellContext] := LErrorMsgs.Strings[lIndex]
            else
              MultiResChannelDialog.CurtailRestrictionGrid.ValidationError[1, LRow, gveCellContext] := ''
          end;
        end;
      end;
   finally
     FreeAndNil(LErrorCols);
     FreeAndNil(LErrorMsgs);
   end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


procedure TMultiResChannelValidator.ValidateReservoirNo
  (AFeature: IMultiResMultiChannelCurtail);
const
  OPNAME = 'TMultiResChannelValidator.ValidateReservoirNo';
begin
  try
    if AFeature <> nil then
    begin
      FErrorMessage := '';
      if (Not AFeature.Validate(FErrorMessage, 'ReservoirNo')) then
        FAllErrorMessages.Add(Trim(FErrorMessage));

      MultiResChannelDialog.ReservoirNoCombo.ValidationError := FErrorMessage;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMultiResChannelValidator.ValidateElevationValue(AFeature: IMultiResMultiChannelCurtail);
const OPNAME = 'TMultiResChannelValidator.ValidateFactorValue';
var
  LErrorCols  : TStringList;
  LErrorMsgs  : TStringList;
  LFieldProperties: TAbstractFieldProperty;
  LIndex,
  LRow : integer;
begin
  try
    LErrorCols  := TStringList.Create;
    LErrorMsgs  := TStringList.Create;
    LFieldProperties := FAppModules.FieldProperties.FieldProperty('MultiCurElevation');
    try
      if AFeature <> nil then
      begin
        if AFeature.Validate(FErrorMessage,'Elevation') then
          for LRow := LFieldProperties.ArrayLow to LFieldProperties.ArrayHigh do
            MultiResChannelDialog.CurtailRestrictionGrid.ValidationError[0, LRow, gveCellContext] := ''
        else
        begin
          ExtractErrorsAndColumns(FErrorMessage, LErrorMsgs, LErrorCols);
          for LRow := LFieldProperties.ArrayLow to LFieldProperties.ArrayHigh do
          begin
            LIndex := LErrorCols.IndexOf(IntToStr(LRow));
            if (LIndex >= 0) then
              MultiResChannelDialog.CurtailRestrictionGrid.ValidationError[0, LRow, gveCellContext] := LErrorMsgs.Strings[lIndex]
            else
              MultiResChannelDialog.CurtailRestrictionGrid.ValidationError[0, LRow, gveCellContext] := ''
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
