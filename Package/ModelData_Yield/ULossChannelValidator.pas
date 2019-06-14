{******************************************************************************}
{*  UNIT      : Contains the class TLossChannelValidator.                     *}
{*  AUTHOR    : Riana Steyn                                                   *}
{*  DATE      : 2003/09/12                                                    *}
{*  COPYRIGHT : Copyright © 2003 DWAF                                         *}
{******************************************************************************}

unit ULossChannelValidator;

interface

uses
  Classes,
  VCL.Controls,
  VCL.ComCtrls,
  VCL.StdCtrls,
  VCL.ExtCtrls,
  UAbstractObject,
  VoaimsCom_TLB,
  UAbstractComponent,
  UDataComponent,
  UDataEditComponent,
  UYieldContextValidationType,
  UAbstractYieldDataDialogValidator,
  ULossChannelDialog;

type
  TLossChannelValidator = class(TAbstractYieldDataDialogValidator)
  protected
    FFeatureID : integer;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    procedure OnEditControlEnter(Sender: TObject); override;
    procedure OnEditControltExit(Sender: TObject); override;
    procedure OnStringGridCellDataHasChanged (ASender: TObject; ACol, ARow: integer); override;
    procedure OnAfterPasteColumnData(Sender: TObject);
    procedure RePopulateDataViewer;
    procedure RePopulateLossProportions;
    procedure UpdateFeatureName;
    procedure UpdateChannelReferenceNode;
    procedure UpdateLossProportions(AMonth : integer;
                                    AValue : string);
    procedure ValidateLossChannelName(AFeature : ILossFeature);
    procedure ValidateWaterLoss (AFeature : ILossFeature);
    procedure ValidateReferenceNodes (AFeature : ILossFeature);
  public
    function Initialise: boolean; override;
    function SaveState: boolean; override;
    function LanguageHasChanged: boolean; override;
    function StudyHasChanged: boolean; override;
    function StudyDataHasChanged(AContext: TChangeContext; AFieldName,AOldValue,ANewValue: string): boolean; override;
    function ProcessParameterChangeEvent : boolean; override;
    function ProcessMetaDataEvent : boolean; override;
    procedure ClearDataViewer; override;
    procedure PopulateDataViewer; override;
    function LossChannelDialog : TLossChannelDialog;
    procedure DoContextValidation(AValidationType: TDialogValidationType); override;
    function DetermineWizardStatus (ASequence : integer = 0) : integer; override;
    property FeatureID : integer read FFeatureID write FFeatureID;
  end;

implementation

uses
  SysUtils,
  VCL.Graphics,
  UYieldModelDataGUIForm,
  UYieldModelDataObject,
  UErrorHandlingOperations,
  UNetworkFeaturesData,
  UNetworkElementData;

{******************************************************************************}
{* TLossChannelValidator                                                      *}
{******************************************************************************}

procedure TLossChannelValidator.CreateMemberObjects;
const OPNAME = 'TLossChannelValidator.CreateMemberObjects';
var
  lpPanel : TLossChannelDialog;
begin
  try
    inherited CreateMemberObjects;
    FFeatureID := -1;
    FPanel  := TLossChannelDialog.Create(FPanelOwner,FAppModules);
    lpPanel := LossChannelDialog;
    with lpPanel do
    begin
      FeatureNameEdit.FieldProperty  := FAppModules.FieldProperties.FieldProperty('LossFeatureName');
      FeatureNameEdit.OnEnter        := OnEditControlEnter;
      FeatureNameEdit.OnExit         := OnEditControltExit;

      ReferenceNodeCbx.FieldProperty := FAppModules.FieldProperties.FieldProperty('Reference');
      ReferenceNodeCbx.OnEnter       := OnEditControlEnter;
      ReferenceNodeCbx.OnExit        := OnEditControltExit;

      LossProportionGrid.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('Month'));
      LossProportionGrid.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('WaterLoss'));
      LossProportionGrid.OnEnter             := OnEditControlEnter;
      LossProportionGrid.OnBeforeCellChange  := OnStringGridCellDataHasChanged;
      LossProportionGrid.OnColEnter          := OnStringGridColEnter;
      LossProportionGrid.ShowGridPopupMenu   := True;
      LossProportionGrid.AllowPasteFromExcel := True;
      LossProportionGrid.OnPasteFromExcel    := Self.OnAfterPasteColumnData;
      LossProportionGrid.OnAfterPasteColumnData := Self.OnAfterPasteColumnData;
  end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TLossChannelValidator.DestroyMemberObjects;
const OPNAME = 'TLossChannelValidator.DestroyMemberObjects';
begin
  try
    inherited DestroyMemberObjects;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TLossChannelValidator.Initialise: boolean;
const OPNAME = 'TLossChannelValidator.Initialise';
begin
  Result := inherited Initialise;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TLossChannelValidator.LanguageHasChanged: boolean;
const OPNAME = 'TLossChannelValidator.LanguageHasChanged';
begin
  Result := False;
  try
    TabShetCaption := FAppModules.Language.GetString('NetworkFeatures.LossFeature');
    Result := inherited LanguageHasChanged;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TLossChannelValidator.ClearDataViewer;
const OPNAME = 'TLossChannelValidator.ClearDataViewer';
var
  lPanel : TLossChannelDialog;
  lMonth : integer;
begin
  inherited ClearDataViewer;
  try
    lPanel := LossChannelDialog;
    with lPanel do
    begin
      FeatureNameEdit.Text := '';
      ReferenceNodeCbx.ItemIndex := -1;
      for lMonth := 0 to 11 do
        LossProportionGrid.Cells[0, lMonth] := '-1';
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TLossChannelValidator.PopulateDataViewer;
const OPNAME = 'TLossChannelValidator.PopulateDataViewer';
begin
  inherited PopulateDataViewer;
  try
    RePopulateDataViewer;
    DoContextValidation(dvtLossFeature);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TLossChannelValidator.RePopulateDataViewer;
const OPNAME = 'TLossChannelValidator.RePopulateDataViewer';
var
  lFeature       : ILossFeature;
  lMonths        : TMonthNamesArray;
  lReservoirList : IReservoirDataList;
  lIndexA        : integer;
  lReservoir     : IReservoirData;
  lReservoirNr   : integer;
  lFieldProperty : TAbstractFieldProperty;
  lKeyValues     : string;
  lFieldIndex    : string;
begin
   lMonths := nil;
  try
    lReservoirList := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.
                        ReservoirList;
    if (lReservoirList <> nil) then
    begin
      with LossChannelDialog do
      begin
        ReferenceNodeCbx.Items.AddObject
          (FAppModules.Language.GetString('NetworkFeatures.UseChannelUpstreamNode'), nil);
        for lIndexA := 0 to lReservoirList.ReservoirAndNodesCount - 1 do
        begin
          lReservoir := lReservoirList.ReservoirOrNodeByIndex[lIndexA];
          if (lReservoir.ReservoirConfigurationData.ReservoirIdentifier <> 0) then
            ReferenceNodeCbx.Items.AddObject
              ('(' + IntToStr(lReservoir.ReservoirConfigurationData.ReservoirIdentifier) + ') ' +
               lReservoir.ReservoirConfigurationData.ReservoirName,
               TObject(lReservoir.ReservoirConfigurationData.ReservoirIdentifier));
        end;
      end;
    end;
    if (FFeatureID >= 0) then
    begin
      lFeature := TYieldModelDataObject(FAppModules.Model.ModelData).
                    NetworkFeaturesData.LossFeatureList.LossFeatureByID[FFeatureID];
      lMonths := TYieldModelDataObject(FAppModules.Model.ModelData).
                    CastRunConfigurationData.MonthNamesArray;
      if (lFeature <> nil) then
      begin
        with LossChannelDialog do
        begin
          lFieldIndex    := '';
          lFieldProperty := ReferenceNodeCbx.FieldProperty;
          lKeyValues     := lFeature.GetKeyValues(lFieldProperty.FieldName, lFieldIndex);
          ReferenceNodeCbx.HasMetaData :=
            FAppModules.MetaData.FindMetaData(lFieldProperty.FieldName, lKeyValues, lFieldIndex) <> nil;

          lFieldProperty := FeatureNameEdit.FieldProperty;
          lKeyValues     := lFeature.GetKeyValues(lFieldProperty.FieldName, lFieldIndex);
          FeatureNameEdit.HasMetaData :=
            FAppModules.MetaData.FindMetaData(lFieldProperty.FieldName, lKeyValues, lFieldIndex) <> nil;

          lReservoirNr := lFeature.ReferenceNode;
          if (lReservoirNr = 0) then
            ReferenceNodeCbx.SetFieldIndex(ReferenceNodeCbx.Items.IndexOfObject(nil))
          else
            ReferenceNodeCbx.SetFieldIndex(ReferenceNodeCbx.Items.IndexOfObject(TObject(lReservoirNr)));
          FeatureNameEdit.SetFieldValue(lFeature.FeatureName);
        end;
      end;
    end;
    RePopulateLossProportions;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TLossChannelValidator.RePopulateLossProportions;
const OPNAME = 'TLossChannelValidator.RePopulateLossProportions';
var
  lFeature         : ILossFeature;
  lRow             : integer;
  lMonths          : TMonthNamesArray;
  lFieldProperty   : TAbstractFieldProperty;
  lKeyValues       : string;
  lFieldIndex      : string;
begin
  lMonths := nil;
  try
    if (FFeatureID >= 0) then
    begin
      lFeature := TYieldModelDataObject(FAppModules.Model.ModelData).
                    NetworkFeaturesData.LossFeatureList.LossFeatureByID[FFeatureID];
      lMonths := TYieldModelDataObject(FAppModules.Model.ModelData).
                   CastRunConfigurationData.MonthNamesArray;
      if (lFeature <> nil) then
      begin
        with LossChannelDialog do
        begin
          lFieldProperty := LossProportionGrid.FieldProperty(1);
          for lRow := 0 to 11 do
          begin
            lFieldIndex := IntToStr(lRow+1);
            lKeyValues  := lFeature.GetKeyValues(lFieldProperty.FieldName, lFieldIndex);
            LossProportionGrid.HasChanges[1, lRow] :=
              FAppModules.Changes.HasParamChange(lFieldProperty.FieldName, lKeyValues, lFieldIndex);
            LossProportionGrid.HasMetaData[1, lRow] :=
              FAppModules.MetaData.FindMetaData(lFieldProperty.FieldName, lKeyValues, lFieldIndex) <> nil;

            LossProportionGrid.Cells[0, lRow] := lMonths[lRow+1];
            LossProportionGrid.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('WaterLoss'));
            LossProportionGrid.Cells[1, lRow] :=Format(FAppModules.FieldProperties.FieldProperty('WaterLoss').
                                                    FormatStringGrid, [lFeature.WaterLossByMonth[lRow+1]]);// Format('%6.2f', [lFeature.WaterLossByMonth[lRow+1]]);
          end;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TLossChannelValidator.SaveState: boolean;
const OPNAME = 'TLossChannelValidator.SaveState';
begin
  Result := inherited SaveState;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TLossChannelValidator.LossChannelDialog : TLossChannelDialog;
const OPNAME = 'TLossChannelValidator.LossChannelDialog';
begin
  Result := Nil;
  try
    if (FPanel <> nil) then
      Result := TLossChannelDialog(FPanel);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TLossChannelValidator.StudyDataHasChanged(AContext: TChangeContext; AFieldName,AOldValue,ANewValue: string): boolean;
const OPNAME = 'TLossChannelValidator.StudyDataHasChanged';
begin
  Result := inherited StudyDataHasChanged(AContext,AFieldName,AOldValue,ANewValue);
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TLossChannelValidator.StudyHasChanged: boolean;
const OPNAME = 'TLossChannelValidator.StudyHasChanged';
begin
  Result := inherited StudyHasChanged;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TLossChannelValidator.OnEditControlEnter(Sender: TObject);
const OPNAME = 'TLossChannelValidator.OnEditControlEnter';
begin
  inherited OnEditControlEnter(Sender);
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TLossChannelValidator.OnEditControltExit(Sender: TObject);
const OPNAME = 'TLossChannelValidator.OnEditControltExit';
begin
  inherited OnEditControltExit(Sender);
  try
    with LossChannelDialog do
    begin
      if ((Sender = ReferenceNodeCbx) AND
          (ReferenceNodeCbx.HasValueChanged)) then
        UpdateChannelReferenceNode
      else
      if ((Sender = FeatureNameEdit) AND
           (FeatureNameEdit.HasValueChanged)) then
        UpdateFeatureName;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TLossChannelValidator.UpdateFeatureName;
const OPNAME = 'TLossChannelValidator.UpdateFeatureName';
var
  lLossFeature : ILossFeature;
  lMessage     : string;
begin
  try
    lLossFeature := TYieldModelDataObject(FAppModules.Model.ModelData).
                      NetworkFeaturesData.LossFeatureList.LossFeatureByID[FFeatureID];
    if (lLossFeature <> nil) then
    begin
      with LossChannelDialog do
      begin
        if (FAppModules.FieldProperties.ValidateFieldProperty(
            FeatureNameEdit.FieldProperty.FieldName,
            FeatureNameEdit.Text,lMessage)) then
        begin
          FeatureNameEdit.FieldValidationError := lMessage;
          lLossFeature.FeatureName := Trim(FeatureNameEdit.Text);
          FeatureNameEdit.SetFieldValue(lLossFeature.FeatureName);
          DoContextValidation(dvtLossFeatureName);
        end
        else
          FeatureNameEdit.FieldValidationError := lMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TLossChannelValidator.UpdateChannelReferenceNode;
const OPNAME = 'TLossChannelValidator.UpdateChannelReferenceNode';
var
  lReservoirList : IReservoirDataList;
  lReservoir     : IReservoirData;
  lReservoirNr   : integer;
  lFeature       : ILossFeature;
  lMessage       : string;
begin
  try
    lFeature := TYieldModelDataObject(FAppModules.Model.ModelData).
                  NetworkFeaturesData.LossFeatureList.LossFeatureByID[FFeatureID];
    if (lFeature <> nil)then
    begin
      with LossChannelDialog do
      begin
        lReservoirList := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.
                            ReservoirList;
        lReservoir     := nil;
        lReservoirNr   := 0;
        if (ReferenceNodeCbx.ItemIndex >= 0) then
        begin
          lReservoirNr := Integer(ReferenceNodeCbx.Items.Objects[ReferenceNodeCbx.ItemIndex]);
          lReservoir   := lReservoirList.ReservoirByIdentifier[lReservoirNr];
        end;
        if (FAppModules.FieldProperties.ValidateFieldProperty(
            ReferenceNodeCbx.FieldProperty.FieldName,
            IntToStr(lReservoirNr),lMessage)) then
        begin
          lFeature.ReferenceNode := lReservoirNr;
          if (lReservoirList <> nil) then
          begin
            lReservoirNr := lFeature.ReferenceNode;
            if (lReservoirNr = 0) then
              ReferenceNodeCbx.SetFieldIndex(ReferenceNodeCbx.Items.IndexOfObject(nil))
            else
              ReferenceNodeCbx.SetFieldIndex(ReferenceNodeCbx.Items.IndexOfObject(TObject(lReservoirNr)));
          end;
        end
        else
          ReferenceNodeCbx.ValidationError := lMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TLossChannelValidator.OnStringGridCellDataHasChanged
                                         (ASender: TObject; ACol, ARow: integer);
const OPNAME = 'TLossChannelValidator.OnStringGridCellDataHasChanged';
begin
  inherited OnStringGridCellDataHasChanged(ASender, ACol, ARow);
  try
    with LossChannelDialog do
    begin
      if ((LossProportionGrid = ASender) AND (ACol = 1) AND
          (NOT LossProportionGrid.HasChanges[ACol,ARow])) then
      begin
        UpdateLossProportions(ARow+1, Trim(LossProportionGrid.Cells[ACol, ARow]));
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TLossChannelValidator.UpdateLossProportions(AMonth : integer;
                                                      AValue : string);
const OPNAME = 'TLossChannelValidator.UpdateLossProportions';
var
  lFeature  : ILossFeature;
  lValue    : double;
  lMessage  : string;
begin
  try
    lFeature := TYieldModelDataObject(FAppModules.Model.ModelData).
                  NetworkFeaturesData.LossFeatureList.LossFeatureByID[FFeatureID];
    if (lFeature <> nil) then
    begin
      with LossChannelDialog do
      begin
        LossProportionGrid.ValidationError[1, AMonth-1, gveCellField] := '';
        if (Trim(AValue) = '') then
          AValue := '0.0';
        if (FAppModules.FieldProperties.ValidateFieldProperty(
            LossProportionGrid.FieldProperty(1).FieldName,
            AValue,lMessage, AMonth)) then
        begin
          lValue := StrToFloat(AValue);
          lFeature.WaterLossByMonth[AMonth] := LValue;
          RePopulateLossProportions;
          DoContextValidation(dvtLossFeatureWaterLoss);
        end
        else
          LossProportionGrid.ValidationError[1, AMonth-1, gveCellField] := lMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TLossChannelValidator.DoContextValidation(AValidationType: TDialogValidationType);
const OPNAME = 'TLossChannelValidator.DoContextValidation';
var
  lFeature     : ILossFeature;
  lFeatureList : ILossFeatureList;
begin
  try
    FAllErrorMessages.Clear;
    if (FFeatureID > 0) then
    begin
      lFeatureList := TYieldModelDataObject(FAppModules.Model.ModelData).
                        NetworkFeaturesData.LossFeatureList;
      lFeature := lFeatureList.LossFeatureByID[FFeatureID];
      if (lFeature <> nil) then
      begin
        if (AValidationType in [dvtLossFeature, dvtLossFeatureName]) then
          ValidateLossChannelName(lFeature);
        if (AValidationType in [dvtLossFeature, dvtLossFeatureWaterLoss]) then
           ValidateWaterLoss(lFeature);
        if (AValidationType in [dvtLossFeature, dvtLossFeatureWaterLoss]) then
           ValidateReferenceNodes(lFeature);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TLossChannelValidator.DetermineWizardStatus (ASequence : integer = 0) : integer;
const OPNAME = 'TLossChannelValidator.DetermineWizardStatus';
var
  lFeature       : ILossFeature;
  lFeatureList   : ILossFeatureList;
  lWaterLoss     : TAbstractFieldProperty;
  lNotZero       : Boolean;
  lIndex         : integer;
begin
  Result := inherited DetermineWizardStatus(ASequence);
  try
    if (FFeatureID >= 0) then
    begin
      lFeatureList := TYieldModelDataObject(FAppModules.Model.ModelData).
                        NetworkFeaturesData.LossFeatureList;
      lFeature := lFeatureList.LossFeatureByID[FFeatureID];
      if (lFeature <> nil) then
      begin
        DoContextValidation(dvtLossFeature);
        lWaterLoss := FAppModules.FieldProperties.FieldProperty('WaterLoss');
        lNotZero := FALSE;
        lIndex   := lWaterLoss.ArrayLow;
        while ((NOT lNotZero) AND (lIndex <= lWaterLoss.ArrayHigh)) do
        begin
          if (lFeature.WaterLossByMonth[lIndex] > 0) then
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

procedure TLossChannelValidator.ValidateLossChannelName(AFeature: ILossFeature);
const OPNAME = 'TLossChannelValidator.ValidateLossChannelName';
begin
  try
    with LossChannelDialog do
    begin
      FErrorMessage := '';
      if (NOT AFeature.Validate(FErrorMessage, 'MinFlowChannelName')) then
        FAllErrorMessages.Add(Trim(FErrorMessage));
      FeatureNameEdit.ContextValidationError := FErrorMessage;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TLossChannelValidator.ValidateWaterLoss(AFeature: ILossFeature);
const OPNAME = 'TLossChannelValidator.ValidateWaterLoss';
begin
  try
    if (AFeature <> nil) then
    begin
      with LossChannelDialog do
      begin
        FErrorMessage := '';
        if (AFeature.Validate(FErrorMessage, 'WaterLoss')) then
          LossProportionGrid.ValidationError[1, 0, gveColContext] := ''
        else
        begin
          LossProportionGrid.ValidationError[1, 0, gveColContext] := FErrorMessage;
          FAllErrorMessages.Add(Trim(FErrorMessage));
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TLossChannelValidator.ValidateReferenceNodes(AFeature: ILossFeature);
const OPNAME = 'TLossChannelValidator.ValidateReferenceNodes';
begin
  try
    with LossChannelDialog do
    begin
      FErrorMessage := '';
      if (AFeature.Validate(FErrorMessage, 'Reference')) then
      begin
        ReferenceNodeCbx.InValidationError := FALSE;
        ReferenceNodeCbx.ShowErrorState(FALSE);
      end
      else
      begin
        ReferenceNodeCbx.InValidationError := TRUE;
        ReferenceNodeCbx.ValidationError := FErrorMessage;
        ReferenceNodeCbx.ShowErrorState(TRUE);
        FAllErrorMessages.Add(Trim(FErrorMessage));
      end;
    end
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TLossChannelValidator.ProcessParameterChangeEvent : boolean;
const OPNAME = 'TLossChannelValidator.ProcessParameterChangeEvent';
var
  lFieldProperty : TAbstractFieldProperty;
  lKeyValues     : string;
  lRowIdx        : integer;
  lFeature       : ILossFeature;
begin
  Result := FALSE;
  try
    if (FPanel.Visible AND (FActiveControl <> nil) AND (FFeatureID <> 0)) then
    begin
      lFeature := TYieldModelDataObject(FAppModules.Model.ModelData).
                    NetworkFeaturesData.LossFeatureList.LossFeatureByID[FFeatureID];
      if (lFeature <> nil) then
      begin
        with LossChannelDialog do
        begin
          if (FActiveControl = LossProportionGrid) then
          begin
            lRowIdx := LossProportionGrid.Row+1;
            lFieldProperty := LossProportionGrid.FieldProperty(1);
            if (lFieldProperty <> nil) then
            begin
              lKeyValues := lFeature.GetKeyValues(lFieldProperty.FieldName, IntToStr(lRowIdx));
              FAppModules.Changes.ShowParameterChanges
                (lFieldProperty.FieldName, lKeyValues, IntToStr(lRowIdx));
              RePopulateDataViewer;
              FAppModules.Changes.SetParameterChanges(TRUE);
              Result := TRUE;
            end;
          end;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TLossChannelValidator.ProcessMetaDataEvent : boolean;
const OPNAME = 'TLossChannelValidator.ProcessMetaDataEvent';
var
  lFieldProperty : TAbstractFieldProperty;
  lKeyValues     : string;
//  lRowIdx        : integer;
  lFieldIndex    : string;
  lFeature       : ILossFeature;
begin
  Result := FALSE;
  try
    if (FPanel.Visible AND (FActiveControl <> nil) AND (FFeatureID <> 0)) then
    begin
      lFeature := TYieldModelDataObject(FAppModules.Model.ModelData).
                    NetworkFeaturesData.LossFeatureList.LossFeatureByID[FFeatureID];
      if (lFeature <> nil) then
      begin
        with LossChannelDialog do
        begin
          lFieldIndex    := '';
          lFieldProperty := nil;
          if (FActiveControl = FeatureNameEdit) then
            lFieldProperty := FeatureNameEdit.FieldProperty
          else
          if (FActiveControl = ReferenceNodeCbx) then
            lFieldProperty := ReferenceNodeCbx.FieldProperty
          else
          if (FActiveControl = LossProportionGrid) then
          begin
            lFieldIndex := IntToStr(LossProportionGrid.Row+1);
            lFieldProperty := LossProportionGrid.FieldProperty(LossProportionGrid.Col);
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

procedure TLossChannelValidator.OnAfterPasteColumnData(Sender: TObject);
const OPNAME = 'TLossChannelValidator.OnAfterPasteColumnData';
var
  LFeature : ILossFeature;
  LValue   : double;
  LRow     : integer;
begin
  try
    LFeature := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.LossFeatureList.LossFeatureByID[FFeatureID];
    if(LFeature <> nil) then
    begin
      for LRow := LossChannelDialog.LossProportionGrid.FixedRows to LossChannelDialog.LossProportionGrid.RowCount - 1 do
      begin
        LValue := StrToFloat(Trim(LossChannelDialog.LossProportionGrid.Cells[1,LRow]));
        LFeature.WaterLossByMonth[LRow + 1] := LValue;
      end; 
      RePopulateLossProportions;
      DoContextValidation(dvtLossFeatureWaterLoss);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.

