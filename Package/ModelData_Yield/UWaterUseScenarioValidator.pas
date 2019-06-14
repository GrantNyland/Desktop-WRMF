{******************************************************************************}
{*  UNIT      : Contains the class TWaterUseScenarioValidator.                *}
{*  AUTHOR    : Dziedzi Ramulondi                                             *}
{*  DATE      : 2004/10/20                                                    *}
{*  COPYRIGHT : Copyright © 2004 DWAF                                         *}
{******************************************************************************}

unit UWaterUseScenarioValidator;

interface

uses
  Classes,
  VCL.Controls,
  VCL.ComCtrls,
  VCL.StdCtrls,
  VCL.ExtCtrls,
  UAbstractObject,
  UAbstractComponent,
  VoaimsCom_TLB,
  UDataComponent,
  UDataEditComponent,
  UYieldContextValidationType,
  UAbstractYieldDataDialogValidator,
  UWaterUseScenarioDialog;

type
  TWaterUseScenarioValidator = class(TAbstractYieldDataDialogValidator)
  protected
    FValidateCategoryIndex : integer;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    procedure OnEditControlEnter(Sender: TObject); override;
    procedure OnEditControltExit(Sender: TObject); override;
    procedure OnStringGridCellDataHasChanged (ASender: TObject; ACol, ARow: integer); override;
    procedure RePopulateDataViewer;
    procedure UpdateScenarionCount;
    procedure UpdateProportionWaterUse (ARow   : integer;
                                        ACol   : integer;
                                        AValue : string);
    procedure ValidateWaterDemands(AConfigData : IWaterDemandFeatureList);
    procedure ValidateWaterDemandScenarioCount(AFeature: IWaterDemandFeatureList);
    procedure ValidateScenarioPortion(AFeature: IWaterDemandFeature);
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
    function WaterUseScenarioFeatureDialog: TWaterUseScenarioDialog;
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
{* TWaterUseScenarioValidator                                                 *}
{******************************************************************************}

procedure TWaterUseScenarioValidator.CreateMemberObjects;
const OPNAME = 'TWaterUseScenarioValidator.CreateMemberObjects';
var
  lPanel : TWaterUseScenarioDialog;
  LIndex : integer;
begin
  try
    inherited CreateMemberObjects;
    FValidateCategoryIndex := 0;
    FPanel := TWaterUseScenarioDialog.Create(FPanelOwner,FAppModules);
    lPanel := WaterUseScenarioFeatureDialog;
    with lPanel do
    begin
      ScenarioCountEdit.FieldProperty   := FAppModules.FieldProperties.FieldProperty('WaterDemandScenarioCount');
      ScenarioCountEdit.OnEnter         := OnEditControlEnter;
      ScenarioCountEdit.OnExit          := OnEditControltExit;
      for LIndex := 1 to 13 do
        ProportionWaterUseGrid.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('ScenarioPortion'));
      ProportionWaterUseGrid.OnBeforeCellChange       := OnStringGridCellDataHasChanged;
      ProportionWaterUseGrid.OnColEnter               := OnStringGridColEnter;
      ProportionWaterUseGrid.OnExit                   := OnEditControltExit;
      ProportionWaterUseGrid.OnEnter                  := OnEditControlEnter;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TWaterUseScenarioValidator.DestroyMemberObjects;
const OPNAME = 'TWaterUseScenarioValidator.DestroyMemberObjects';
begin
  try
    inherited DestroyMemberObjects;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TWaterUseScenarioValidator.Initialise: boolean;
const OPNAME = 'TWaterUseScenarioValidator.Initialise';
begin
  Result := inherited Initialise;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TWaterUseScenarioValidator.WaterUseScenarioFeatureDialog : TWaterUseScenarioDialog;
const OPNAME = 'TWaterUseScenarioValidator.WaterUseScenarioFeatureDialog';
begin
  Result := Nil;
  try
    if (FPanel <> nil) then
      Result := TWaterUseScenarioDialog(FPanel);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TWaterUseScenarioValidator.StudyDataHasChanged(AContext: TChangeContext; AFieldName,AOldValue,ANewValue: string): boolean;
const OPNAME = 'TWaterUseScenarioValidator.StudyDataHasChanged';
begin
  Result := inherited StudyDataHasChanged(AContext,AFieldName,AOldValue,ANewValue);
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TWaterUseScenarioValidator.StudyHasChanged: boolean;
const OPNAME = 'TWaterUseScenarioValidator.StudyHasChanged';
begin
  Result := inherited StudyHasChanged;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TWaterUseScenarioValidator.LanguageHasChanged: boolean;
const OPNAME = 'TWaterUseScenarioValidator.LanguageHasChanged';
begin
  Result := False;
  try
    TabShetCaption := FAppModules.Language.GetString('TabCaption.WaterUseScenario');
    Result := inherited LanguageHasChanged;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TWaterUseScenarioValidator.SaveState: boolean;
const OPNAME = 'TWaterUseScenarioValidator.SaveState';
begin
  Result := inherited SaveState;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TWaterUseScenarioValidator.ClearDataViewer;
const OPNAME = 'TWaterUseScenarioValidator.ClearDataViewer';
var
  lPanel : TWaterUseScenarioDialog;
begin
  inherited ClearDataViewer;
  try
    lPanel := WaterUseScenarioFeatureDialog;
    with lPanel do
    begin
      SetScenarioCount(0,0);
      ScenarioCountEdit.SetFieldValue('');
    end;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TWaterUseScenarioValidator.PopulateDataViewer;
const OPNAME = 'TWaterUseScenarioValidator.PopulateDataViewer';
begin
  inherited PopulateDataViewer;
  try
    ClearDataViewer;
    RePopulateDataViewer;
    DoContextValidation(dvtWaterDemandWaterUseProportioning);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TWaterUseScenarioValidator.RePopulateDataViewer;
const OPNAME = 'TWaterUseScenarioValidator.RePopulateDataViewer';
var
  LChannelCount        : integer;
  LSenarioCount        : integer;
  LCount               : integer;
  LIndex               : integer;
  lWaterDemandList     : IWaterDemandFeatureList;
  lWaterDemandFeature  : IWaterDemandFeature;
  LWaterDemandCategory : IWaterDemandCategory;
  LCategoryName        : string;
  LField               : TAbstractFieldProperty;
  lFieldProperty       : TAbstractFieldProperty;
  lKeyValues           : string;
  lFieldIndex          : string;
begin
  try
    lWaterDemandList := TYieldModelDataObject(FAppModules.Model.ModelData).
                          NetworkFeaturesData.WaterDemandFeatureList;
    LSenarioCount    := lWaterDemandList.ScenarioCount;
    LChannelCount    := lWaterDemandList.WaterDemandFeatureCount;
    WaterUseScenarioFeatureDialog.SetScenarioCount(LChannelCount,LSenarioCount);

    lFieldProperty := WaterUseScenarioFeatureDialog.ScenarioCountEdit.FieldProperty;
    lFieldIndex    := '';
    lKeyValues     := lWaterDemandList.GetKeyValues(lFieldProperty.FieldName, lFieldIndex);
    WaterUseScenarioFeatureDialog.ScenarioCountEdit.HasMetaData :=
      FAppModules.MetaData.FindMetaData(lFieldProperty.FieldName, lKeyValues, lFieldIndex) <> nil;
    WaterUseScenarioFeatureDialog.ScenarioCountEdit.SetFieldValue(LSenarioCount);

    LField := FAppModules.FieldProperties.FieldProperty('ScenarioPortion');
    for LIndex := 0 to LChannelCount -1 do
    begin
      lWaterDemandFeature := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.
                               WaterDemandFeatureList.WaterDemandFeatureByIndex[LIndex];
      if (lWaterDemandFeature <> nil) then
      begin
        LCategoryName := '';
        LWaterDemandCategory := TYieldModelDataObject(FAppModules.Model.ModelData).
                                NetworkFeaturesData.WaterDemandConfiguration.
                                DemandCategoryByID[lWaterDemandFeature.WaterDemandCategory];
        if (LWaterDemandCategory <> nil) then
          LCategoryName := LWaterDemandCategory.CategoryName;
        WaterUseScenarioFeatureDialog.ProportionWaterUseGrid.Cells[0,LIndex+1] := IntToStr(lWaterDemandFeature.Channel.ChannelNumber);
        WaterUseScenarioFeatureDialog.ProportionWaterUseGrid.Cells[1,LIndex+1] := lWaterDemandFeature.Channel.ChannelName;
        WaterUseScenarioFeatureDialog.ProportionWaterUseGrid.Cells[2,LIndex+1] := LCategoryName;

        for LCount := 1 to LSenarioCount do
        begin
          lFieldIndex := IntToStr(LCount);
          lKeyValues  := lWaterDemandFeature.GetKeyValues(LField.FieldName, lFieldIndex);
          WaterUseScenarioFeatureDialog.ProportionWaterUseGrid.HasMetaData[lCount+2, LIndex+1] :=
            FAppModules.MetaData.FindMetaData(LField.FieldName, lKeyValues, lFieldIndex) <> nil;
          if (lWaterDemandFeature.ScenarioPortionByIndex[LCount] = NullFloat) then
            WaterUseScenarioFeatureDialog.ProportionWaterUseGrid.Cells[LCount+2,LIndex+1] :=
            Trim(Format(LField.FormatStringGrid,[0.0]))
          else
            WaterUseScenarioFeatureDialog.ProportionWaterUseGrid.Cells[LCount+2,LIndex+1] :=
            Trim(Format(LField.FormatStringGrid,[lWaterDemandFeature.ScenarioPortionByIndex[LCount]]));
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TWaterUseScenarioValidator.OnEditControlEnter(Sender: TObject);
const OPNAME = 'TWaterUseScenarioValidator.OnEditControlEnter';
begin
  inherited OnEditControlEnter(Sender);
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TWaterUseScenarioValidator.OnEditControltExit(Sender: TObject);
const OPNAME = 'TWaterUseScenarioValidator.OnEditControltExit';
begin
  inherited OnEditControltExit(Sender);
  try
   with WaterUseScenarioFeatureDialog do
    begin
      if ((Sender = ScenarioCountEdit) AND
          (ScenarioCountEdit.HasValueChanged)) then
        UpdateScenarionCount
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TWaterUseScenarioValidator.UpdateScenarionCount;
const OPNAME = 'TWaterUseScenarioValidator.UpdateScenarionCount';
var
  LCount: integer;
begin
  try
   LCount := StrToInt(WaterUseScenarioFeatureDialog.ScenarioCountEdit.Text);
   if (LCount <> TYieldModelDataObject(FAppModules.Model.ModelData).
             NetworkFeaturesData.WaterDemandFeatureList.ScenarioCount) then
   begin
     TYieldModelDataObject(FAppModules.Model.ModelData).
     NetworkFeaturesData.WaterDemandFeatureList.ScenarioCount := LCount;
     DoContextValidation(dvtWaterDemandScenarioCount);
     PopulateDataViewer;
     WaterUseScenarioFeatureDialog.Resize;
   end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TWaterUseScenarioValidator.UpdateProportionWaterUse (ARow   : integer;
                                                                    ACol   : integer;
                                                                    AValue : string);
const OPNAME = 'TWaterUseScenarioValidator.UpdateProportionWaterUse';
var
  LWaterDemandFeature : IWaterDemandFeature;
  LChannel            : IGeneralFlowChannel;
  LChannelNumber      : integer;
  lValue              : double;
  lMessage            : string;
begin
  try
    LChannelNumber := StrToInt(WaterUseScenarioFeatureDialog.ProportionWaterUseGrid.Cells[0,ARow]);
    LChannel := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.
                  ChannelList.ChannelByChannelNumber[LChannelNumber];
    if (LChannel <> nil) and (LChannel.WaterDemandFeature <> nil) then
    begin
      LWaterDemandFeature := LChannel.WaterDemandFeature;
      with WaterUseScenarioFeatureDialog do
      begin
        if (Trim(AValue) = '') then
          AValue := '0.0';
        ProportionWaterUseGrid.ValidationError[ACol+2, ARow, gveCellField] := '';
        if (FAppModules.FieldProperties.ValidateFieldProperty(
              ProportionWaterUseGrid.FieldProperty(ACol).FieldName,
              AValue, lMessage, ACol)) then
        begin
          lValue := StrToFloat(AValue);
          LWaterDemandFeature.ScenarioPortionByIndex[ACol] := LValue;
          FValidateCategoryIndex := ARow;
          DoContextValidation(dvtWaterDemandScenarioPortion);
          FValidateCategoryIndex := 0;
        end;
      end;    
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TWaterUseScenarioValidator.OnStringGridCellDataHasChanged
                                         (ASender: TObject; ACol, ARow: integer);
const OPNAME = 'TWaterUseScenarioValidator.OnStringGridCellDataHasChanged';
begin
  inherited OnStringGridCellDataHasChanged(ASender, ACol, ARow);
  try
    with WaterUseScenarioFeatureDialog do
    begin
      if ((ProportionWaterUseGrid = ASender) AND (ACol > 2)) then
        UpdateProportionWaterUse(ARow, ACol - 2, Trim(ProportionWaterUseGrid.Cells[ACol, ARow]))
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TWaterUseScenarioValidator.DoContextValidation(AValidationType: TDialogValidationType);
const OPNAME = 'TWaterUseScenarioValidator.DoContextValidation';
var
  lFeatureList        : IWaterDemandFeatureList;
  lWaterDemandFeature : IWaterDemandFeature;
begin
  try
    FAllErrorMessages.Clear;
    lFeatureList  := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.WaterDemandFeatureList;

    if (lFeatureList <> nil) then
    begin
      if (AValidationType in [dvtWaterDemandWaterUseProportioning,
                              dvtWaterDemandScenarioCount]) then
        ValidateWaterDemandScenarioCount(lFeatureList);
    end;

    if (FValidateCategoryIndex >= 0) then
      lWaterDemandFeature := TYieldModelDataObject(FAppModules.Model.ModelData).
                             NetworkFeaturesData.WaterDemandFeatureList.WaterDemandFeatureByIndex[FValidateCategoryIndex]
    else
      lWaterDemandFeature := nil;
    if (lWaterDemandFeature <> nil) then
    begin
      if (AValidationType in [dvtWaterDemandWaterUseProportioning,
                              dvtWaterDemandScenarioPortion]) then
      ValidateWaterDemands(lFeatureList);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TWaterUseScenarioValidator.ValidateWaterDemandScenarioCount(AFeature: IWaterDemandFeatureList);
const OPNAME = 'TWaterUseScenarioValidator.ValidateWaterDemandScenarioCount';
begin
  try
    with WaterUseScenarioFeatureDialog do
    begin
      FErrorMessage := '';
      AFeature.Validate(FErrorMessage, 'WaterDemandScenarioCount');
      ScenarioCountEdit.ContextValidationError := FErrorMessage;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TWaterUseScenarioValidator.ValidateScenarioPortion(AFeature: IWaterDemandFeature);
const OPNAME = 'TWaterUseScenarioValidator.ValidateScenarioPortion';
var
  LErrorCols  : TStringList;
  lErrorMsgs  : TStringList;
  lCol        : integer;
  lIndex      : integer;
  lConfigData : IWaterDemandFeatureList;
begin
  try
    if (AFeature <> nil) then
    begin
      lConfigData := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.
                       WaterDemandFeatureList;
      with WaterUseScenarioFeatureDialog do
      begin
        lErrorCols := TStringList.Create;
        lErrorMsgs := TStringList.Create;
        try
          lErrorCols.Clear;
          FErrorMessage := '';
          if (AFeature.Validate(FErrorMessage, 'ScenarioPortion')) then
          begin
            for lCol := 1 to lConfigData.ScenarioCount do
              ProportionWaterUseGrid.ValidationError[lCol+1, FValidateCategoryIndex+1, gveCellContext] := ''
          end
          else
          begin
            ExtractErrorsAndColumns(FErrorMessage, lErrorMsgs, lErrorCols);
            for lCol := 1 to lConfigData.ScenarioCount do
            begin
              lIndex := lErrorCols.IndexOf(IntToStr(lCol));
              if (lIndex >= 0) then
                ProportionWaterUseGrid.ValidationError[lCol+2, FValidateCategoryIndex+1, gveCellContext] := lErrorMsgs.Strings[lIndex]
              else
                ProportionWaterUseGrid.ValidationError[lCol+2, FValidateCategoryIndex+1, gveCellContext] := ''
            end;
            FAllErrorMessages.AddStrings(lErrorMsgs);
          end;
        finally
          FreeAndNil(lErrorCols);
          FreeAndNil(lErrorMsgs);
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TWaterUseScenarioValidator.ValidateWaterDemands(AConfigData: IWaterDemandFeatureList);
const OPNAME = 'TWaterUseScenarioValidator.ValidateWaterDemands';
var
  lIndex        : integer;
  LChannelCount : integer;
  lCategory     : IWaterDemandFeature;
begin
  try
    with WaterUseScenarioFeatureDialog do
    begin
      FErrorMessage := '';
      LChannelCount := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.
                    WaterDemandFeatureList.WaterDemandFeatureCount;
      for LIndex := 0 to LChannelCount - 1 do
      begin
        lCategory := AConfigData.WaterDemandFeatureByIndex[lIndex];
        FValidateCategoryIndex := lIndex;
        ValidateScenarioPortion(lCategory);
        FValidateCategoryIndex := 0;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TWaterUseScenarioValidator.ProcessMetaDataEvent: boolean;
const OPNAME = 'TWaterUseScenarioValidator.ProcessMetaDataEvent';
var
  lFieldProperty   : TAbstractFieldProperty;
  lKeyValues       : string;
  lFieldIndex      : string;
  lFeature         : IWaterDemandFeature;
  lWaterDemandList : IWaterDemandFeatureList;
begin
  Result := FALSE;
  try
    lWaterDemandList := TYieldModelDataObject(FAppModules.Model.ModelData).
                          NetworkFeaturesData.WaterDemandFeatureList;
    if (FPanel.Visible AND (FActiveControl <> nil) AND (lWaterDemandList <> nil)) then
    begin
      with WaterUseScenarioFeatureDialog do
      begin
        lFieldProperty := nil;
        if (FActiveControl = ScenarioCountEdit) then
        begin
          lFieldProperty := ScenarioCountEdit.FieldProperty;
          lFieldIndex    := '';
          lKeyValues     := lWaterDemandList.GetKeyValues(lFieldProperty.FieldName, lFieldIndex);
        end
        else
        if (FActiveControl = ProportionWaterUseGrid) then
        begin
          lFieldProperty := ProportionWaterUseGrid.FieldProperty(1);
          lFeature       := lWaterDemandList.WaterDemandFeatureByIndex[ProportionWaterUseGrid.Row-1];
          lFieldIndex    := IntToStr(ProportionWaterUseGrid.Col - 2);
          lKeyValues     := lFeature.GetKeyValues(lFieldProperty.FieldName, lFieldIndex);
        end;
        if (lFieldProperty <> nil) then
        begin
          FAppModules.MetaData.ShowMetaData
            (lFieldProperty.FieldName, lKeyValues, lFieldIndex);
          RePopulateDataViewer;
          Result := TRUE;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.

