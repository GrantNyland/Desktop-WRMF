{******************************************************************************}
{*  UNIT      : Contains the class TPowerPlantTailwaterValidator.             *}
{*  AUTHOR    : Riana Steyn                                                   *}
{*  DATE      : 2003/11/27                                                    *}
{*  COPYRIGHT : Copyright © 2003 DWAF                                         *}
{******************************************************************************}

unit UPowerPlantTailwaterValidator;

interface

uses
  Classes,
  Types,
  VCL.Grids,
  VCL.Controls,
  VCL.ComCtrls,
  VCL.StdCtrls,
  VCL.ExtCtrls,
  UAbstractObject,
  UAbstractComponent,
  UDataComponent,
  UDataEditComponent,
  UYieldContextValidationType,
  VoaimsCom_TLB,
  UAbstractYieldDataDialogValidator,
  UPowerPlantTailwaterDialog;

type
  TPowerPlantTailwaterValidator = class(TAbstractYieldDataDialogValidator)
  protected
    FFeatureID : integer;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    procedure OnEditControlEnter(Sender: TObject); override;
    procedure OnEditControltExit(Sender: TObject); override;
    procedure OnStringGridCellDataHasChanged (ASender: TObject; ACol, ARow: integer); override;
    procedure OnAfterPasteGridData(Sender: TObject);
    procedure OnAfterPasteColumnData(Sender: TObject);
    procedure RePopulateDataViewer;
    procedure RePopulateTailwaterGrid;
    procedure OnTailwaterTypeClick(Sender: TObject);
    procedure UpdateTailwaterType;
    procedure UpdateDischarge(AIndex : integer;
                              AValue : string);
    procedure UpdateTailwaterElevation(AIndex : integer;
                                       AValue : string);
    procedure ValidateTailwaterDischarge (AFeature : IPowerPlant);
    procedure ValidateTailwaterElevation (AFeature : IPowerPlant);
  public
    function Initialise: boolean; override;
    function SaveState: boolean; override;
    function LanguageHasChanged: boolean; override;
    function StudyHasChanged: boolean; override;
    function StudyDataHasChanged(AContext: TChangeContext; AFieldName,AOldValue,ANewValue: string): boolean; override;
    function ProcessMetaDataEvent : boolean; override;
    procedure ClearDataViewer; override;
    procedure PopulateDataViewer; override;
    procedure DoContextValidation (AValidationType : TDialogValidationType); override;
    function DetermineWizardStatus (ASequence : integer = 0) : integer; override;
    function PowerPlantTailwaterDialog : TPowerPlantTailwaterDialog;
    property FeatureID : integer read FFeatureID write FFeatureID;
  end;

implementation

uses
  SysUtils,
  VCL.Graphics,
  UConstants,
  UUtilities,
  UYieldModelDataGUIForm,
  UYieldModelDataObject,
  UErrorHandlingOperations;

{******************************************************************************}
{* TPowerPlantTailwaterValidator                                              *}
{******************************************************************************}

procedure TPowerPlantTailwaterValidator.CreateMemberObjects;
const OPNAME = 'TPowerPlantTailwaterValidator.CreateMemberObjects';
var
  lPanel : TPowerPlantTailwaterDialog;
begin
  try
    inherited CreateMemberObjects;
    FFeatureID := -1;
    FPanel := TPowerPlantTailwaterDialog.Create(FPanelOwner,FAppModules);
    lPanel := PowerPlantTailwaterDialog;
    with lPanel do
    begin
      TailwaterTypeRadioGroup.FieldProperty := FAppModules.FieldProperties.FieldProperty('TailWaterTypeCode');
      TailwaterTypeRadioGroup.OnEnter       := OnEditControlEnter;
      TailwaterTypeRadioGroup.OnClick       := OnTailwaterTypeClick;

      TailwaterGrid.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('Month'));
      TailwaterGrid.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('DownStreamLevel'));
      TailwaterGrid.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('TailWaterElevation'));
      TailwaterGrid.OnBeforeCellChange             := OnStringGridCellDataHasChanged;
      TailwaterGrid.OnColEnter                     := OnStringGridColEnter;
      TailwaterGrid.OnEnter                        := OnEditControlEnter;
      TailwaterGrid.ShowGridPopupMenu              := True;
      TailwaterGrid.AllowPasteFromExcel            := True;
      TailwaterGrid.OnAfterPasteColumnData         := Self.OnAfterPasteColumnData;
      TailwaterGrid.OnAfterPasteColumnsAndRowsData := Self.OnAfterPasteGridData;
      TailwaterGrid.OnPasteFromExcel               := Self.OnAfterPasteGridData;
  end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TPowerPlantTailwaterValidator.DestroyMemberObjects;
const OPNAME = 'TPowerPlantTailwaterValidator.DestroyMemberObjects';
begin
  try
    inherited DestroyMemberObjects;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TPowerPlantTailwaterValidator.Initialise: boolean;
const OPNAME = 'TPowerPlantTailwaterValidator.Initialise';
begin
  Result := inherited Initialise;
  try
    with PowerPlantTailwaterDialog.TailwaterTypeRadioGroup do
    begin
      Items.Clear;
      Items.Add(FAppModules.Language.GetString('NetworkFeatures.TailwaterType1'));
      Items.Add(FAppModules.Language.GetString('NetworkFeatures.TailwaterType2'));
      Items.Add(FAppModules.Language.GetString('NetworkFeatures.TailwaterType3'));
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TPowerPlantTailwaterValidator.LanguageHasChanged: boolean;
const OPNAME = 'TPowerPlantTailwaterValidator.LanguageHasChanged';
begin
  Result := False;
  try
    TabShetCaption := FAppModules.Language.GetString('NetworkFeatures.TailwaterFunction');
    Result := inherited LanguageHasChanged;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TPowerPlantTailwaterValidator.ClearDataViewer;
const OPNAME = 'TPowerPlantTailwaterValidator.ClearDataViewer';
var
  lPanel : TPowerPlantTailwaterDialog;
  lIndex : integer;
begin
  inherited ClearDataViewer;
  try
    lPanel := PowerPlantTailwaterDialog;
    with lPanel do
    begin
      TailwaterTypeRadioGroup.ItemIndex := -1;
      for lIndex := 1 to TailwaterGrid.RowCount - 1 do
      begin
        TailwaterGrid.Cells[0, lIndex] := '';
        TailwaterGrid.Cells[1, lIndex] := '-1.0';
        TailwaterGrid.Cells[2, lIndex] := '-1.0';
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TPowerPlantTailwaterValidator.PopulateDataViewer;
const OPNAME = 'TPowerPlantTailwaterValidator.PopulateDataViewer';
begin
  inherited PopulateDataViewer;
  try
    ClearDataViewer;
    RePopulateDataViewer;
    DoContextValidation(dvtPowerPlantTailwater);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TPowerPlantTailwaterValidator.RePopulateDataViewer;
const OPNAME = 'TPowerPlantTailwaterValidator.RePopulateDataViewer';
var
  lPowerPlant    : IPowerPlant;
  lFieldProperty : TAbstractFieldProperty;
  lKeyValues     : string;
  lFieldIndex    : string;
begin
  try
    if (FFeatureID >= 0) then
    begin
      lPowerPlant := TYieldModelDataObject(FAppModules.Model.ModelData).
                       NetworkFeaturesData.PowerPlantList.PowerPlantByID[FFeatureID];
      if (lPowerPlant <> nil) then
      begin
        with PowerPlantTailwaterDialog do
        begin
          FeatureNameLabel.Caption := lPowerPlant.FeatureName;

          lFieldIndex    := '';
          lFieldProperty := TailwaterTypeRadioGroup.FieldProperty;
          lKeyValues     := lPowerPlant.GetKeyValues(lFieldProperty.FieldName, lFieldIndex);
          TailwaterTypeRadioGroup.HasMetaData :=
            FAppModules.MetaData.FindMetaData(lFieldProperty.FieldName, lKeyValues, lFieldIndex) <> nil;
          TailwaterTypeRadioGroup.ItemIndex := lPowerPlant.TailWaterType - 1;

          RePopulateTailwaterGrid;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TPowerPlantTailwaterValidator.RePopulateTailwaterGrid;
const OPNAME = 'TPowerPlantTailwaterValidator.RePopulateTailwaterGrid';
var
  lPowerPlant : IPowerPlant;
  lIndex      : integer;
  lValue      : double;
  lFieldProp1 : TAbstractFieldProperty;
  lFieldProp2 : TAbstractFieldProperty;
  lKeyValues  : string;
  lFieldIndex : string;
begin
  try
    if (FFeatureID >= 0) then
    begin
      lPowerPlant := TYieldModelDataObject(FAppModules.Model.ModelData).
                       NetworkFeaturesData.PowerPlantList.PowerPlantByID[FFeatureID];
      if (lPowerPlant <> nil) then
      begin
        with PowerPlantTailwaterDialog do
        begin
          lFieldProp1 := TailwaterGrid.FieldProperty(1);
          lFieldProp2 := TailwaterGrid.FieldProperty(2);
          for lIndex := 1 to TailwaterGrid.RowCount - 1 do
          begin
            lFieldIndex := IntToStr(lIndex);
            lKeyValues  := lPowerPlant.GetKeyValues(lFieldProp1.FieldName, lFieldIndex);
            TailwaterGrid.HasMetaData[1, lIndex] :=
              FAppModules.MetaData.FindMetaData(lFieldProp1.FieldName, lKeyValues, lFieldIndex) <> nil;
            lKeyValues  := lPowerPlant.GetKeyValues(lFieldProp2.FieldName, lFieldIndex);
            TailwaterGrid.HasMetaData[2, lIndex] :=
              FAppModules.MetaData.FindMetaData(lFieldProp2.FieldName, lKeyValues, lFieldIndex) <> nil;

            TailwaterGrid.Cells[0, lIndex] := IntToStr(lIndex);
            lValue := lPowerPlant.DischargeByIndex[lIndex];
            if (lValue <> NullFloat) then
              TailwaterGrid.Cells[1, lIndex] := Format(lFieldProp1.FormatStringGrid{ '%9.3f'}, [lValue])
            else
              TailwaterGrid.Cells[1, lIndex] := '';
            lValue := lPowerPlant.TailwaterElevationByIndex[lIndex];
            if (lValue <> NullFloat) then
              TailwaterGrid.Cells[2, lIndex] := Format(lFieldProp1.FormatStringGrid{ '%9.3f'}, [lValue])
            else
              TailwaterGrid.Cells[2, lIndex] := '';
          end;
        end;
      end;
    end;    
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TPowerPlantTailwaterValidator.OnTailwaterTypeClick(Sender: TObject);
const OPNAME = 'TPowerPlantTailwaterValidator.OnTailwaterTypeClick';
begin
  try
    with PowerPlantTailwaterDialog do
    begin
      Tailwater1Label.Visible := False;
      Tailwater2Label.Visible := False;
      Tailwater3Label.Visible := False;
      if(TailwaterTypeRadioGroup.ItemIndex = 0) then
      begin
        Tailwater1Label.Visible := True;
        Tailwater1Label.BringToFront
      end
      else
      if(TailwaterTypeRadioGroup.ItemIndex = 1) then
      begin
        Tailwater2Label.Visible := True;
        Tailwater2Label.BringToFront
      end
      else
      begin
        Tailwater3Label.Visible := True;
        Tailwater3Label.BringToFront;
      end;

      if(PowerPlantTailwaterDialog.TailwaterTypeRadioGroup.HasValueChanged) then
        UpdateTailwaterType;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TPowerPlantTailwaterValidator.SaveState: boolean;
const OPNAME = 'TPowerPlantTailwaterValidator.SaveState';
begin
  Result := inherited SaveState;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TPowerPlantTailwaterValidator.PowerPlantTailwaterDialog : TPowerPlantTailwaterDialog;
const OPNAME = 'TPowerPlantTailwaterValidator.PowerPlantTailwaterDialog';
begin
  Result := Nil;
  try
    if (FPanel <> nil) then
      Result := TPowerPlantTailwaterDialog(FPanel);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TPowerPlantTailwaterValidator.StudyDataHasChanged(AContext: TChangeContext; AFieldName,AOldValue,ANewValue: string): boolean;
const OPNAME = 'TPowerPlantTailwaterValidator.StudyDataHasChanged';
begin
  Result := inherited StudyDataHasChanged(AContext,AFieldName,AOldValue,ANewValue);
  try
    if(AFieldName = 'PowerPlantName') then
    begin
      if(PowerPlantTailwaterDialog.FeatureNameLabel.Caption =  AOldValue) then
        PowerPlantTailwaterDialog.FeatureNameLabel.Caption := ANewValue;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TPowerPlantTailwaterValidator.StudyHasChanged: boolean;
const OPNAME = 'TPowerPlantTailwaterValidator.StudyHasChanged';
begin
  Result := inherited StudyHasChanged;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TPowerPlantTailwaterValidator.OnEditControlEnter(Sender: TObject);
const OPNAME = 'TPowerPlantTailwaterValidator.OnEditControlEnter';
begin
  inherited OnEditControlEnter(Sender);
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TPowerPlantTailwaterValidator.OnEditControltExit(Sender: TObject);
const OPNAME = 'TPowerPlantTailwaterValidator.OnEditControltExit';
begin
  inherited OnEditControltExit(Sender);
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TPowerPlantTailwaterValidator.UpdateTailwaterType;
const OPNAME = 'TPowerPlantTailwaterValidator.UpdateTailwaterType';
var
  lPowerPlant : IPowerPlant;
  lOldType    : integer;
  lNewType    : integer;
  lMessage    : string;
begin
  try
    lPowerPlant := TYieldModelDataObject(FAppModules.Model.ModelData).
                     NetworkFeaturesData.PowerPlantList.PowerPlantByID[FFeatureID];
    if (lPowerPlant <> nil) then
    begin
      with PowerPlantTailwaterDialog do
      begin
        lOldType := lPowerPlant.TailWaterType;
        lNewType := TailwaterTypeRadioGroup.ItemIndex + 1;
        if (lNewType <> lOldType) then
        begin
          if (FAppModules.FieldProperties.ValidateFieldProperty(
              TailwaterTypeRadioGroup.FieldProperty.FieldName,
              IntToStr(lNewType),lMessage)) then
          begin
            lPowerPlant.TailWaterType := lNewType;
            TailwaterTypeRadioGroup.ItemIndex := lPowerPlant.TailWaterType - 1;
          end
          else
            TailwaterTypeRadioGroup.ValidationError := lMessage;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TPowerPlantTailwaterValidator.OnStringGridCellDataHasChanged
                                         (ASender: TObject; ACol, ARow: integer);
const OPNAME = 'TPowerPlantTailwaterValidator.OnStringGridCellDataHasChanged';
begin
  inherited OnStringGridCellDataHasChanged(ASender, ACol, ARow);
  try
    with PowerPlantTailwaterDialog do
    begin
      if (TailwaterGrid = ASender) then
      begin
        if (ACol = 1) then
          UpdateDischarge(ARow, Trim(TailwaterGrid.Cells[ACol, ARow]))
        else
        if (ACol = 2) then
          UpdateTailwaterElevation(ARow, Trim(TailwaterGrid.Cells[ACol, ARow]));
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TPowerPlantTailwaterValidator.UpdateDischarge(AIndex : integer;
                                                        AValue : string);
const OPNAME = 'TPowerPlantTailwaterValidator.UpdateDischarge';
var
  lPowerPlant : IPowerPlant;
  lValue      : double;
  lMessage    : string;
  lCount      : integer;
begin
  try
    lPowerPlant := TYieldModelDataObject(FAppModules.Model.ModelData).
                     NetworkFeaturesData.PowerPlantList.PowerPlantByID[FFeatureID];
    if (lPowerPlant <> nil)then
    begin
      with PowerPlantTailwaterDialog do
      begin
        TailwaterGrid.ValidationError[1, AIndex, gveCellField] := '';
        if (Trim(AValue) = '') then
        begin
          AValue := FloatToStr(NullFloat);
          lValue := NullFloat;
        end
        else
          lValue := StrToFloat(AValue);
        if (FAppModules.FieldProperties.ValidateFieldProperty(
            'DownStreamLevel', AValue,lMessage, AIndex)) then
        begin
          lPowerPlant.DischargeByIndex[AIndex] := lValue;
          lCount := lPowerPlant.TailwaterElevationCount;
          lPowerPlant.TailwaterElevationCount := lCount;
          RePopulateDataViewer;
          DoContextValidation(dvtPowerPlantTailwaterDischarges);
        end
        else
          TailwaterGrid.ValidationError[1, AIndex, gveCellField] := lMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TPowerPlantTailwaterValidator.UpdateTailwaterElevation(AIndex : integer;
                                                                 AValue : string);
const OPNAME = 'TPowerPlantTailwaterValidator.UpdateTailwaterElevation';
var
  lPowerPlant : IPowerPlant;
  lValue      : double;
  lMessage    : string;
  lCount      : integer;
begin
  try
    lPowerPlant := TYieldModelDataObject(FAppModules.Model.ModelData).
                     NetworkFeaturesData.PowerPlantList.PowerPlantByID[FFeatureID];
    if (lPowerPlant <> nil)then
    begin
      with PowerPlantTailwaterDialog do
      begin
        TailwaterGrid.ValidationError[2, AIndex, gveCellField] := '';
        if (Trim(AValue) = '') then
        begin
          AValue := FloatToStr(NullFloat);
          lValue := NullFloat;
        end
        else
          lValue := StrToFloat(AValue);
        if (FAppModules.FieldProperties.ValidateFieldProperty(
            'TailWaterElevation', AValue, lMessage, AIndex)) then
        begin
          lPowerPlant.TailwaterElevationByIndex[AIndex] := lValue;
          lCount := lPowerPlant.TailwaterElevationCount;
          lPowerPlant.TailwaterElevationCount := lCount;
          RePopulateDataViewer;
          DoContextValidation(dvtPowerPlantTailwaterElevations);
        end
        else
          TailwaterGrid.ValidationError[2, AIndex, gveCellField] := lMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TPowerPlantTailwaterValidator.DoContextValidation(AValidationType: TDialogValidationType);
const OPNAME = 'TPowerPlantTailwaterValidator.DoContextValidation';
var
  lFeature     : IPowerPlant;
  lFeatureList : IPowerPlantList;
begin
  try
    FAllErrorMessages.Clear;
    if (FFeatureID >= 0) then
    begin
      lFeatureList := TYieldModelDataObject(FAppModules.Model.ModelData).
                        NetworkFeaturesData.PowerPlantList;
      lFeature     := lFeatureList.PowerPlantByID[FFeatureID];
      if (lFeature <> nil) then
      begin
        if (AValidationType in [dvtPowerPlantTailwater, dvtPowerPlantTailwaterElevations]) then
          ValidateTailwaterElevation(lFeature);
        if (AValidationType in [dvtPowerPlantTailwater, dvtPowerPlantTailwaterDischarges]) then
          ValidateTailwaterDischarge(lFeature);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TPowerPlantTailwaterValidator.DetermineWizardStatus (ASequence : integer = 0) : integer;
const OPNAME = 'TPowerPlantTailwaterValidator.DetermineWizardStatus';
var
  lFeature       : IPowerPlant;
  lFeatureList   : IPowerPlantList;
  lFieldProperty : TAbstractFieldProperty;
  lNotZero       : Boolean;
  lIndex         : integer;
begin
  Result := inherited DetermineWizardStatus(ASequence);
  try
    if (FFeatureID >= 0) then
    begin
      lFeatureList := TYieldModelDataObject(FAppModules.Model.ModelData).
                        NetworkFeaturesData.PowerPlantList;
      lFeature := lFeatureList.PowerPlantByID[FFeatureID];
      if (lFeature <> nil) then
      begin
        DoContextValidation(dvtPowerPlantTailwater);
        lFieldProperty := FAppModules.FieldProperties.FieldProperty('DownStreamLevel');
        lNotZero := FALSE;
        lIndex := lFieldProperty.ArrayLow;
        while ((NOT lNotZero) AND (lIndex <= lFeature.TailwaterElevationCount)) do
        begin
          if ((lFeature.DischargeByIndex[lIndex] > 0) OR
              (lFeature.TailwaterElevationByIndex[lIndex] > 0)) then
            lNotZero := TRUE
          else
            lIndex := lIndex + 1;
        end;
        if (lNotZero) then
        begin
          Result := 1;
          if (FAllErrorMessages.Count = 0) then
            Result := 2;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TPowerPlantTailwaterValidator.ValidateTailwaterElevation (AFeature : IPowerPlant);
const OPNAME = 'TPowerPlantTailwaterValidator.ValidateTailwaterElevation';
var
  lCol           : integer;
  lIndex         : integer;
  lErrorCols     : TStringList;
  lErrorMessages : TStringList;
begin
  try
    with PowerPlantTailwaterDialog do
    begin
      lErrorCols     := TStringList.Create;
      lErrorMessages := TStringList.Create;
      try
        FErrorMessage := '';
        lErrorCols.Clear;
        if (AFeature.Validate(FErrorMessage, 'TailWaterElevation')) then
        begin
          for lCol := 1 to 10 do
            TailwaterGrid.ValidationError[2, lCol, gveCellContext] := '';
        end
        else
        begin
          ExtractErrorsAndColumns(FErrorMessage, lErrorMessages, lErrorCols);
          for lCol := 1 to 10 do
          begin
            lIndex := lErrorCols.IndexOf(IntToStr(lCol));
            if (lIndex >= 0) then
              TailwaterGrid.ValidationError[2, lCol, gveCellContext] := lErrorMessages.Strings[lIndex]
            else
              TailwaterGrid.ValidationError[2, lCol, gveCellContext] := '';
          end;
        end;
        FAllErrorMessages.Add(Trim(FErrorMessage));
      finally
        FreeAndNil(lErrorCols);
        FreeAndNil(lErrorMessages);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TPowerPlantTailwaterValidator.ValidateTailwaterDischarge (AFeature : IPowerPlant);
const OPNAME = 'TPowerPlantTailwaterValidator.ValidateTailwaterDischarge';
var
  lCol           : integer;
  lIndex         : integer;
  lErrorMessages : TStringList;
  lErrorCols     : TStringList;
begin
  try
    FErrorMessage := '';
    with PowerPlantTailwaterDialog do
    begin
      lErrorCols     := TStringList.Create;
      lErrorMessages := TStringList.Create;
      try
        FErrorMessage := '';
        lErrorCols.Clear;
        if (AFeature.Validate(FErrorMessage, 'Discharge')) then
        begin
          for lCol := 1 to 10 do
            TailwaterGrid.ValidationError[1, lCol, gveCellContext] := '';
        end
        else
        begin
          ExtractErrorsAndColumns(FErrorMessage, lErrorMessages, lErrorCols);
          for lCol := 1 to 10 do
          begin
            lIndex := lErrorCols.IndexOf(IntToStr(lCol));
            if (lIndex >= 0 ) then
              TailwaterGrid.ValidationError[1, lCol, gveCellContext] := lErrorMessages.Strings[lIndex]
            else
              TailwaterGrid.ValidationError[1, lCol, gveCellContext] := '';
          end;
        end;
        FAllErrorMessages.Add(Trim(FErrorMessage));
      finally
        FreeAndNil(lErrorCols);
        FreeAndNil(lErrorMessages);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TPowerPlantTailwaterValidator.ProcessMetaDataEvent : boolean;
const OPNAME = 'TPowerPlantTailwaterValidator.ProcessMetaDataEvent';
var
  lFieldProperty : TAbstractFieldProperty;
  lKeyValues     : string;
  lFieldIndex    : string;
  lFeature       : IPowerPlant;
begin
  Result := FALSE;
  try
    if (FPanel.Visible AND (FActiveControl <> nil) AND (FFeatureID <> 0)) then
    begin
      lFeature := TYieldModelDataObject(FAppModules.Model.ModelData).
                    NetworkFeaturesData.PowerPlantList.PowerPlantByID[FFeatureID];
      if (lFeature <> nil) then
      begin
        with PowerPlantTailwaterDialog do
        begin
          lFieldIndex    := '';
          lFieldProperty := nil;
          if (FActiveControl = TailwaterTypeRadioGroup) then
            lFieldProperty := TailwaterTypeRadioGroup.FieldProperty
          else
          if (FActiveControl = TailwaterGrid) then
          begin
            lFieldProperty := TailwaterGrid.FieldProperty(TailwaterGrid.Col);
            lFieldIndex    := IntToStr(TailwaterGrid.Row);
          end;
          if (lFieldProperty <> nil) then
          begin
            lKeyValues := lFeature.GetKeyValues(lFieldProperty.FieldName, lFieldIndex);
            FAppModules.MetaData.ShowMetaData
              (lFieldProperty.FieldName, lKeyValues, lFieldIndex);
            RePopulateDataViewer;
            Result := TRUE;
          end;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TPowerPlantTailwaterValidator.OnAfterPasteColumnData(Sender: TObject);
const OPNAME = 'TPowerPlantTailwaterValidator.OnAfterPasteColumnData';
var
  LPowerPlant : IPowerPlant;
  LValue      : double;
  LCol,
  LRow        : integer;
begin
  try
    LPowerPlant := TYieldModelDataObject(FAppModules.Model.ModelData).
                     NetworkFeaturesData.PowerPlantList.PowerPlantByID[FFeatureID];
    if(LPowerPlant <> nil)then
    begin
      if(Sender = PowerPlantTailwaterDialog.TailwaterGrid) then
      begin
        LCol := PowerPlantTailwaterDialog.TailwaterGrid.Col;
        if(LCol = 1) then
        begin
          for LRow := TFieldStringGrid(Sender).FixedRows to TFieldStringGrid(Sender).RowCount - 1 do
          begin
            LValue := StrToFloat(Trim(PowerPlantTailwaterDialog.TailwaterGrid.Cells[LCol,LRow]));
            LPowerPlant.DischargeByIndex[LRow] := LValue;
          end;
        end
        else
        if(LCol = 2) then
        begin
          for LRow := TFieldStringGrid(Sender).FixedRows to TFieldStringGrid(Sender).RowCount - 1 do
          begin
            LValue := StrToFloat(Trim(PowerPlantTailwaterDialog.TailwaterGrid.Cells[LCol,LRow]));
            LPowerPlant.TailwaterElevationByIndex[LRow] := LValue;
          end;
        end;
        RePopulateDataViewer;
        DoContextValidation(dvtPowerPlantTailwaterDischarges);
        DoContextValidation(dvtPowerPlantTailwaterElevations);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TPowerPlantTailwaterValidator.OnAfterPasteGridData(Sender: TObject);
const OPNAME = 'TPowerPlantTailwaterValidator.OnAfterPasteGridData';
var
  LPowerPlant : IPowerPlant;
  LValue      : double;
  LRow        : integer;
begin
  try
    LPowerPlant := TYieldModelDataObject(FAppModules.Model.ModelData).
                     NetworkFeaturesData.PowerPlantList.PowerPlantByID[FFeatureID];
    if(LPowerPlant <> nil)then
    begin
      if(Sender = PowerPlantTailwaterDialog.TailwaterGrid) then
      begin
        for LRow := TFieldStringGrid(Sender).FixedRows to TFieldStringGrid(Sender).RowCount - 1 do
        begin
            LValue := StrToFloat(Trim(PowerPlantTailwaterDialog.TailwaterGrid.Cells[1,LRow]));
            LPowerPlant.DischargeByIndex[LRow] := LValue;
            LValue := StrToFloat(Trim(PowerPlantTailwaterDialog.TailwaterGrid.Cells[2,LRow]));
            LPowerPlant.TailwaterElevationByIndex[LRow] := LValue;
        end;
        RePopulateDataViewer;
        DoContextValidation(dvtPowerPlantTailwaterDischarges);
        DoContextValidation(dvtPowerPlantTailwaterElevations);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.

