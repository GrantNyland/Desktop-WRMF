{******************************************************************************}
{*  UNIT      : Contains the class TPowerPlantDemandsValidator.               *}
{*  AUTHOR    : Riana Steyn                                                   *}
{*  DATE      : 2003/11/27                                                    *}
{*  COPYRIGHT : Copyright © 2003 DWAF                                         *}
{******************************************************************************}

unit UPowerPlantDemandsValidator;

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
  VoaimsCom_TLB,
  UYieldContextValidationType,
  UAbstractYieldDataDialogValidator,
  UPowerPlantDemandsDialog;

type
  TPowerPlantDemandsValidator = class(TAbstractYieldDataDialogValidator)
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
    procedure RepopulateDemandsGrid;
    procedure UpdatePowerGeneration(AMonth : integer;
                                    AValue : string);
    procedure UpdatePowerRelease(AMonth : integer;
                                 AValue : string);
    procedure ValidatePowerGeneration (AFeature : IPowerPlant);
    procedure ValidatePowerRelease (AFeature : IPowerPlant);
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
    function PowerPlantDemandsDialog : TPowerPlantDemandsDialog;
    property FeatureID : integer read FFeatureID write FFeatureID;
  end;

implementation

uses
  SysUtils,
  VCL.Graphics,
  UUtilities,
  UYieldModelDataGUIForm,
  UYieldModelDataObject,
  UErrorHandlingOperations;

{******************************************************************************}
{* TPowerPlantDemandsValidator                                                *}
{******************************************************************************}

procedure TPowerPlantDemandsValidator.CreateMemberObjects;
const OPNAME = 'TPowerPlantDemandsValidator.CreateMemberObjects';
var
  lPanel : TPowerPlantDemandsDialog;
begin
  try
    inherited CreateMemberObjects;
    FFeatureID := -1;
    FPanel := TPowerPlantDemandsDialog.Create(FPanelOwner,FAppModules);
    lPanel := PowerPlantDemandsDialog;
    with lPanel do
    begin
      DemandsGrid.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('Month'));
      DemandsGrid.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('MinEnergyGenerated'));
      DemandsGrid.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('MinPowerChannelRelease'));
      DemandsGrid.OnBeforeCellChange             := OnStringGridCellDataHasChanged;
      DemandsGrid.OnColEnter                     := OnStringGridColEnter;
      DemandsGrid.OnEnter                        := OnEditControlEnter;
      DemandsGrid.ShowGridPopupMenu              := True;
      DemandsGrid.AllowPasteFromExcel            := True;
      DemandsGrid.OnAfterPasteColumnData         := Self.OnAfterPasteColumnData;
      DemandsGrid.OnAfterPasteColumnsAndRowsData := Self.OnAfterPasteGridData;
      DemandsGrid.OnPasteFromExcel               := Self.OnAfterPasteGridData;
  end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TPowerPlantDemandsValidator.DestroyMemberObjects;
const OPNAME = 'TPowerPlantDemandsValidator.DestroyMemberObjects';
begin
  try
    inherited DestroyMemberObjects;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TPowerPlantDemandsValidator.Initialise: boolean;
const OPNAME = 'TPowerPlantDemandsValidator.Initialise';
begin
  Result := inherited Initialise;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TPowerPlantDemandsValidator.LanguageHasChanged: boolean;
const OPNAME = 'TPowerPlantDemandsValidator.LanguageHasChanged';
begin
  Result := False;
  try
    TabShetCaption := FAppModules.Language.GetString('NetworkFeatures.PowerGenerationDemands');
    Result := inherited LanguageHasChanged;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TPowerPlantDemandsValidator.ClearDataViewer;
const OPNAME = 'TPowerPlantDemandsValidator.ClearDataViewer';
var
  lPanel : TPowerPlantDemandsDialog;
  lIndex : integer;
begin
  inherited ClearDataViewer;
  try
    lPanel := PowerPlantDemandsDialog;
    with lPanel do
    begin
      for lIndex := 1 to DemandsGrid.RowCount - 1 do
      begin
        DemandsGrid.Cells[0, lIndex] := '';
        DemandsGrid.Cells[1, lIndex] := '-1.0';
        DemandsGrid.Cells[2, lIndex] := '-1.0';
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TPowerPlantDemandsValidator.PopulateDataViewer;
const OPNAME = 'TPowerPlantDemandsValidator.PopulateDataViewer';
begin
  inherited PopulateDataViewer;
  try
    ClearDataViewer;
    RePopulateDataViewer;
    DoContextValidation(dvtPowerPlantDemands);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TPowerPlantDemandsValidator.RePopulateDataViewer;
const OPNAME = 'TPowerPlantDemandsValidator.RePopulateDataViewer';
var
  lPowerPlant : IPowerPlant;
begin
  try
    if (FFeatureID >= 0) then
    begin
      lPowerPlant := TYieldModelDataObject(FAppModules.Model.ModelData).
                       NetworkFeaturesData.PowerPlantList.PowerPlantByID[FFeatureID];
      if (lPowerPlant <> nil) then
      begin
        with PowerPlantDemandsDialog do
        begin
          FeatureNameLabel.Caption := lPowerPlant.FeatureName;
          RepopulateDemandsGrid;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TPowerPlantDemandsValidator.RepopulateDemandsGrid;
const OPNAME = 'TPowerPlantDemandsValidator.RepopulateDemandsGrid';
var
  lMonth      : integer;
  lMonths     : TMonthNamesArray;
  lPowerPlant : IPowerPlant;
  lFieldProp1 : TAbstractFieldProperty;
  lFieldProp2 : TAbstractFieldProperty;
  lKeyValues  : string;
  lFieldIndex : string;
begin
  lMonths := nil;
  try
    if (FFeatureID >= 0) then
    begin
      lMonths := TYieldModelDataObject(FAppModules.Model.ModelData).
                   CastRunConfigurationData.MonthNamesArray;
      lPowerPlant := TYieldModelDataObject(FAppModules.Model.ModelData).
                       NetworkFeaturesData.PowerPlantList.PowerPlantByID[FFeatureID];
      with PowerPlantDemandsDialog do
      begin
        lFieldProp1 := DemandsGrid.FieldProperty(1);
        lFieldProp2 := DemandsGrid.FieldProperty(2);
        for lMonth := 1 to 12 do
        begin
          lFieldIndex := IntToStr(lMonth);
          lKeyValues  := lPowerPlant.GetKeyValues(lFieldProp1.FieldName, lFieldIndex);
          DemandsGrid.HasMetaData[1, lMonth] :=
            FAppModules.MetaData.FindMetaData(lFieldProp1.FieldName, lKeyValues, lFieldIndex) <> nil;
          lKeyValues  := lPowerPlant.GetKeyValues(lFieldProp2.FieldName, lFieldIndex);
          DemandsGrid.HasMetaData[2, lMonth] :=
            FAppModules.MetaData.FindMetaData(lFieldProp2.FieldName, lKeyValues, lFieldIndex) <> nil;

          DemandsGrid.Cells[0, lMonth] := lMonths[lMonth];
          DemandsGrid.Cells[1, lMonth] := Format(lFieldProp1.FormatStringGrid{ '%6.3f'}, [lPowerPlant.MinimumPowerGenerationByMonth[lMonth]]);
          DemandsGrid.Cells[2, lMonth] := Format(lFieldProp1.FormatStringGrid{ '%6.3f'}, [lPowerPlant.MinimumPowerReleaseByMonth[lMonth]]);
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TPowerPlantDemandsValidator.SaveState: boolean;
const OPNAME = 'TPowerPlantDemandsValidator.SaveState';
begin
  Result := inherited SaveState;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TPowerPlantDemandsValidator.PowerPlantDemandsDialog : TPowerPlantDemandsDialog;
const OPNAME = 'TPowerPlantDemandsValidator.PowerPlantDemandsDialog';
begin
  Result := Nil;
  try
    if (FPanel <> nil) then
      Result := TPowerPlantDemandsDialog(FPanel);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TPowerPlantDemandsValidator.StudyDataHasChanged(AContext: TChangeContext; AFieldName,AOldValue,ANewValue: string): boolean;
const OPNAME = 'TPowerPlantDemandsValidator.StudyDataHasChanged';
begin
  Result := inherited StudyDataHasChanged(AContext,AFieldName,AOldValue,ANewValue);
  try
    if(AFieldName = 'PowerPlantName') then
    begin
      if(PowerPlantDemandsDialog.FeatureNameLabel.Caption =  AOldValue) then
        PowerPlantDemandsDialog.FeatureNameLabel.Caption := ANewValue;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TPowerPlantDemandsValidator.StudyHasChanged: boolean;
const OPNAME = 'TPowerPlantDemandsValidator.StudyHasChanged';
begin
  Result := inherited StudyHasChanged;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TPowerPlantDemandsValidator.OnEditControlEnter(Sender: TObject);
const OPNAME = 'TPowerPlantDemandsValidator.OnEditControlEnter';
begin
  inherited OnEditControlEnter(Sender);
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TPowerPlantDemandsValidator.OnEditControltExit(Sender: TObject);
const OPNAME = 'TPowerPlantDemandsValidator.OnEditControltExit';
begin
  inherited OnEditControltExit(Sender);
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TPowerPlantDemandsValidator.OnStringGridCellDataHasChanged
                                         (ASender: TObject; ACol, ARow: integer);
const OPNAME = 'TPowerPlantDemandsValidator.OnStringGridCellDataHasChanged';
begin
  inherited OnStringGridCellDataHasChanged(ASender, ACol, ARow);
  try
    with PowerPlantDemandsDialog do
    begin
      if (DemandsGrid = ASender) then
      begin
        if (ACol = 1) then
          UpdatePowerGeneration(ARow, Trim(DemandsGrid.Cells[ACol, ARow]))
        else
        if (ACol = 2) then
          UpdatePowerRelease(ARow, Trim(DemandsGrid.Cells[ACol, ARow]));
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TPowerPlantDemandsValidator.UpdatePowerGeneration(AMonth : integer;
                                                            AValue : string);
const OPNAME = 'TPowerPlantDemandsValidator.UpdatePowerGeneration';
var
  lPowerPlant : IPowerPlant;
  lValue      : double;
  lMessage    : string;
begin
  try
    lPowerPlant := TYieldModelDataObject(FAppModules.Model.ModelData).
                     NetworkFeaturesData.PowerPlantList.PowerPlantByID[FFeatureID];
    if (lPowerPlant <> nil) then
    begin
      with PowerPlantDemandsDialog do
      begin
        DemandsGrid.ValidationError[1, AMonth, gveCellField] := '';
        if (Trim(AValue) = '') then
          AValue := '0.0';
        if (FAppModules.FieldProperties.ValidateFieldProperty(
            DemandsGrid.FieldProperty(1).FieldName,
            AValue,lMessage, AMonth)) then
        begin
          lValue := StrToFloat(AValue);
          lPowerPlant.MinimumPowerGenerationByMonth[AMonth] := LValue;
          RepopulateDemandsGrid;
          DoContextValidation(dvtPowerPlantGenerations);
        end
        else
          DemandsGrid.ValidationError[1, AMonth, gveCellField] := lMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TPowerPlantDemandsValidator.UpdatePowerRelease(AMonth : integer;
                                                         AValue : string);
const OPNAME = 'TPowerPlantDemandsValidator.UpdatePowerRelease';
var
  lPowerPlant : IPowerPlant;
  lValue      : double;
  lMessage    : string;
begin
  try
    lPowerPlant := TYieldModelDataObject(FAppModules.Model.ModelData).
                     NetworkFeaturesData.PowerPlantList.PowerPlantByID[FFeatureID];
    if (lPowerPlant <> nil) then
    begin
      with PowerPlantDemandsDialog do
      begin
        if (Trim(AValue) = '') then
          AValue := '0.0';
        DemandsGrid.ValidationError[2, AMonth, gveCellField] := '';
        if (FAppModules.FieldProperties.ValidateFieldProperty(
            DemandsGrid.FieldProperty(2).FieldName,
            AValue, lMessage, AMonth)) then
        begin
          lValue := StrToFloat(AValue);
          lPowerPlant.MinimumPowerReleaseByMonth[AMonth] := LValue;
          RepopulateDemandsGrid;
          DoContextValidation(dvtPowerPlantReleases);
        end
        else
          DemandsGrid.ValidationError[2, AMonth, gveCellField] := lMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TPowerPlantDemandsValidator.DoContextValidation(AValidationType: TDialogValidationType);
const OPNAME = 'TPowerPlantDemandsValidator.DoContextValidation';
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
        if (AValidationType in [dvtPowerPlantDemands, dvtPowerPlantGenerations]) then
          ValidatePowerGeneration(lFeature);
        if (AValidationType in [dvtPowerPlantDemands, dvtPowerPlantReleases]) then
          ValidatePowerRelease(lFeature);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TPowerPlantDemandsValidator.DetermineWizardStatus (ASequence : integer = 0) : integer;
const OPNAME = 'TPowerPlantDemandsValidator.DetermineWizardStatus';
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
        DoContextValidation(dvtPowerPlantDemands);
        lFieldProperty := FAppModules.FieldProperties.FieldProperty('MinEnergyGenerated');
        lNotZero := FALSE;
        lIndex := lFieldProperty.ArrayLow;
        while ((NOT lNotZero) AND (lIndex <= lFieldProperty.ArrayHigh)) do
        begin
          if ((lFeature.MinimumPowerGenerationByMonth[lIndex] > 0) OR
              (lFeature.MinimumPowerReleaseByMonth[lIndex] > 0)) then
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

procedure TPowerPlantDemandsValidator.ValidatePowerGeneration (AFeature : IPowerPlant);
const OPNAME = 'TPowerPlantDemandsValidator.ValidatePowerGeneration';
var
  lCol           : integer;
  lIndex         : integer;
  lErrorcols     : TStringList;
  lErrorMessages : TStringList;
begin
  try
    with PowerPlantDemandsDialog do
    begin
      lErrorcols     := TStringList.Create;
      lErrorMessages := TStringList.Create;
      try
        FErrorMessage := '';
        lErrorcols.Clear;
        if (AFeature.Validate(FErrorMessage, 'PowerGenerations')) then
        begin
           for lCol := 1 to 12 do
             DemandsGrid.ValidationError[1, lCol, gveCellContext] := '';
        end
        else
        begin
          ExtractErrorsAndColumns(FErrorMessage, lErrorMessages, lErrorcols);
          for lCol := 1 to 12 do
          begin
            lIndex := lErrorcols.IndexOf(IntToStr(lCol));
            if (lindex >= 0) then
              DemandsGrid.ValidationError[1, lCol, gveCellContext] := lErrorMessages.Strings[lIndex]
            else
              DemandsGrid.ValidationError[1, lCol, gveCellContext] := '';
          end;
        end;
        FAllErrorMessages.Add(Trim(FErrorMessage));
      finally
        FreeAndNil(lErrorcols);
        FreeAndNil(lErrorMessages);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TPowerPlantDemandsValidator.ValidatePowerRelease (AFeature : IPowerPlant);
const OPNAME = 'TPowerPlantDemandsValidator.ValidatePowerRelease';
var
  lCol           : integer;
  lIndex         : integer;
  lErrorCols     : TStringList;
  lErrorMessages : TStringList;
begin
  try
    with PowerPlantDemandsDialog do
    begin
      lErrorCols     := TStringList.Create;
      lErrorMessages := TStringList.Create;
      try
        FErrorMessage := '';
        lErrorCols.Clear;
        if (AFeature.Validate(FErrorMessage, 'PowerRelease')) then
        begin
          for lCol := 1 to 12 do
            DemandsGrid.ValidationError[2, lCol, gveCellContext] := '';
        end
        else
        begin
          ExtractErrorsAndColumns(FErrorMessage, lErrorMessages, lErrorCols);
          for lCol := 1 to 12 do
          begin
            lIndex := lErrorCols.IndexOf(IntToStr(lCol));
            if ( lIndex >= 0 ) then
              DemandsGrid.ValidationError[2, lCol, gveCellContext] := lErrorMessages.Strings[lIndex]
            else
              DemandsGrid.ValidationError[2, lCol, gveCellContext] := '';
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

function TPowerPlantDemandsValidator.ProcessMetaDataEvent : boolean;
const OPNAME = 'TPowerPlantDemandsValidator.ProcessMetaDataEvent';
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
        with PowerPlantDemandsDialog do
        begin
          lFieldIndex    := '';
          lFieldProperty := nil;
          if (FActiveControl = DemandsGrid) then
          begin
            lFieldProperty := DemandsGrid.FieldProperty(DemandsGrid.Col);
            lFieldIndex    := IntToStr(DemandsGrid.Row);
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

procedure TPowerPlantDemandsValidator.OnAfterPasteColumnData(Sender: TObject);
const OPNAME = 'TPowerPlantDemandsValidator.OnAfterPasteColumnData';
var
  LPowerPlant : IPowerPlant;
  LValue      : double;
  LRow,
  LCol        : integer;
begin
  try
    LPowerPlant := TYieldModelDataObject(FAppModules.Model.ModelData).
                     NetworkFeaturesData.PowerPlantList.PowerPlantByID[FFeatureID];
    if(LPowerPlant <> nil) then
    begin
      if(Sender = PowerPlantDemandsDialog.DemandsGrid) then
      begin
        LCol := PowerPlantDemandsDialog.DemandsGrid.Col;
        if(LCol = 1) then
        begin
          for LRow := TFieldStringGrid(Sender).FixedRows to TFieldStringGrid(Sender).RowCount - 1 do
          begin
            LValue := StrToFloat(Trim(PowerPlantDemandsDialog.DemandsGrid.Cells[LCol,LRow]));
            LPowerPlant.MinimumPowerGenerationByMonth[LRow] := LValue;
          end;
        end
        else
        if(LCol = 2) then
        begin
          for LRow := TFieldStringGrid(Sender).FixedRows to TFieldStringGrid(Sender).RowCount - 1 do
          begin
            LValue := StrToFloat(Trim(PowerPlantDemandsDialog.DemandsGrid.Cells[LCol,LRow]));
            LPowerPlant.MinimumPowerReleaseByMonth[LRow] := LValue;
          end;
        end;
        RepopulateDemandsGrid;
        DoContextValidation(dvtPowerPlantGenerations);
        DoContextValidation(dvtPowerPlantReleases);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TPowerPlantDemandsValidator.OnAfterPasteGridData(Sender: TObject);
const OPNAME = 'TPowerPlantDemandsValidator.OnAfterPasteGridData';
var
  LPowerPlant : IPowerPlant;
  LValue      : double;
  LRow        : integer;
begin
  try
    LPowerPlant := TYieldModelDataObject(FAppModules.Model.ModelData).
                     NetworkFeaturesData.PowerPlantList.PowerPlantByID[FFeatureID];
    if(LPowerPlant <> nil) then
    begin
      if(Sender = PowerPlantDemandsDialog.DemandsGrid) then
      begin
        for LRow := TFieldStringGrid(Sender).FixedRows to TFieldStringGrid(Sender).RowCount - 1 do
        begin
          LValue := StrToFloat(Trim(PowerPlantDemandsDialog.DemandsGrid.Cells[1,LRow]));
          LPowerPlant.MinimumPowerGenerationByMonth[LRow] := LValue;
          LValue := StrToFloat(Trim(PowerPlantDemandsDialog.DemandsGrid.Cells[2,LRow]));
          LPowerPlant.MinimumPowerReleaseByMonth[LRow] := LValue;
        end;
        RepopulateDemandsGrid;
        DoContextValidation(dvtPowerPlantGenerations);
        DoContextValidation(dvtPowerPlantReleases);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.

