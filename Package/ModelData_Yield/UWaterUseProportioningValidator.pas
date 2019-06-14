//
//
//  UNIT      : Contains  TWaterUseProportioningValidator   Class
//  AUTHOR    : Sam Dhlamini(Cornastone)
//  DATE      : 22/01/2004
//  COPYRIGHT : Copyright © 2004 DWAF
//
//
unit UWaterUseProportioningValidator;

interface

uses
  Classes,
  VCL.Controls,
  VCL.ComCtrls,
  VCL.StdCtrls,
  VCL.Grids,
  VCL.ExtCtrls,
  UAbstractObject,
  UAbstractComponent,
  VoaimsCom_TLB,
  UDataComponent,
  UDataEditComponent,
  UYieldContextValidationType,
  UAbstractYieldDataDialogValidator,
  UWaterUseProportioningDialog;

type
  TWaterUseProportioningValidator = class(TAbstractYieldDataDialogValidator)
  protected
    FSelectedChannelIndex  : integer;
    FSelectedChannelNumber : integer;
    FValidateChannelIndex  : integer;
    FDemandCategoryCount   : integer;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    procedure OnReconciliationGridSelectCell (ASender : TObject; ACol, ARow: Longint; var CanSelect: Boolean);
    procedure OnEditControlEnter(Sender: TObject); override;
    procedure OnEditControltExit(Sender: TObject); override;
    procedure OnStringGridCellDataHasChanged (ASender: TObject; ACol, ARow: integer); override;
    procedure RePopulateDataViewer;
    procedure UpdateProportionWaterUse (ARow   : integer;
                                        ACol   : integer;
                                        AValue : string);
//    procedure AddPorportionTotal (AConfigData : IWaterUseOutputProportion);
    procedure ValidateWaterUseCategories;
    procedure ValidateWaterUsePortion (AWaterUseOutputProportion : IWaterUseOutputProportion);
    procedure ValidateWaterUsePortionTotal (AWaterUseOutputProportion : IWaterUseOutputProportion);
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
    function WaterUseProportioningDialog : TWaterUseProportioningDialog;

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


procedure TWaterUseProportioningValidator.CreateMemberObjects;
const OPNAME = 'TWaterUseProportioningValidator.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
    FValidateChannelIndex := 0;
    FPanel := TWaterUseProportioningDialog.Create(FPanelOwner,FAppModules);
    WaterUseProportioningDialog.ProportionWaterUseGrid.OnSelectCell             := OnReconciliationGridSelectCell;
    WaterUseProportioningDialog.ProportionWaterUseGrid.OnBeforeCellChange       := OnStringGridCellDataHasChanged;
    WaterUseProportioningDialog.ProportionWaterUseGrid.OnColEnter               := OnStringGridColEnter;
    WaterUseProportioningDialog.ProportionWaterUseGrid.OnExit                   := OnEditControltExit;
    WaterUseProportioningDialog.ProportionWaterUseGrid.OnEnter                  := OnEditControlEnter;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TWaterUseProportioningValidator.DestroyMemberObjects;
const OPNAME = 'TWaterUseProportioningValidator.DestroyMemberObjects';
begin
  try
    inherited DestroyMemberObjects;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TWaterUseProportioningValidator.Initialise: boolean;
const OPNAME = 'TWaterUseProportioningValidator.Initialise';
begin
  Result := inherited Initialise;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TWaterUseProportioningValidator.WaterUseProportioningDialog : TWaterUseProportioningDialog;
const OPNAME = 'TWaterUseProportioningValidator.WaterUseProportioningDialog';
begin
  Result := Nil;
  try
    if (FPanel <> nil) then
      Result := TWaterUseProportioningDialog(FPanel);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TWaterUseProportioningValidator.StudyDataHasChanged(AContext: TChangeContext; AFieldName,AOldValue,ANewValue: string): boolean;
const OPNAME = 'TWaterUseProportioningValidator.StudyDataHasChanged';
begin
  Result := inherited StudyDataHasChanged(AContext,AFieldName,AOldValue,ANewValue);
  try
    if ( AFieldName = 'WaterDemandCategoryCount' ) or
      ( AFieldName = 'WaterDemandCategoryName' ) then
      PopulateDataViewer;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TWaterUseProportioningValidator.StudyHasChanged: boolean;
const OPNAME = 'TWaterUseProportioningValidator.StudyHasChanged';
begin
  Result := inherited StudyHasChanged;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TWaterUseProportioningValidator.LanguageHasChanged: boolean;
const OPNAME = 'TWaterUseProportioningValidator.LanguageHasChanged';
begin
  Result := False;
  try
    TabShetCaption := FAppModules.Language.GetString('TabCaption.WaterUseOutputProportioning');
    Result := inherited LanguageHasChanged;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TWaterUseProportioningValidator.SaveState: boolean;
const OPNAME = 'TWaterUseProportioningValidator.SaveState';
begin
  Result := inherited SaveState;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TWaterUseProportioningValidator.ClearDataViewer;
const OPNAME = 'TWaterUseProportioningValidator.ClearDataViewer';
var
  LCol,
  LIndex : integer;
begin
  inherited ClearDataViewer;
  try
    WaterUseProportioningDialog.ProportionWaterUseGrid.ClearFieldProperties;
    for LIndex := 0 to WaterUseProportioningDialog.ProportionWaterUseGrid.RowCount - 1 do
    begin
      for LCol := 0 to WaterUseProportioningDialog.ProportionWaterUseGrid.ColCount - 1 do
        WaterUseProportioningDialog.ProportionWaterUseGrid.Cells [ LCol, LIndex ] := '0';
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TWaterUseProportioningValidator.PopulateDataViewer;
const OPNAME = 'TWaterUseProportioningValidator.PopulateDataViewer';
begin
  inherited PopulateDataViewer;
  try
    ClearDataViewer;
    RePopulateDataViewer;
    ValidateWaterUseCategories;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TWaterUseProportioningValidator.RePopulateDataViewer;
const OPNAME = 'TWaterUseProportioningValidator.RePopulateDataViewer';
var
  lRowIndex            : integer;
  lColIndex            : integer;
  lCategory            : IWaterDemandCategory;
  LCount               : integer;
  LProportion          : IWaterUseOutputProportion;
  LField               : TAbstractFieldProperty;
  LChannel             : IGeneralFlowChannel;
  lWaterDemandConfig   : IWaterDemandConfiguration;
  lGrid                : TFieldStringGrid;
begin
  try
    WaterUseProportioningDialog.ProportionWaterUseGrid.ClearFieldProperties;
    lWaterDemandConfig := (FAppModules.Model.ModelData as IYieldModelData).NetworkFeaturesData.WaterDemandConfiguration;
    LCount := lWaterDemandConfig.WaterUseOutputProportionCount;
    lGrid  := WaterUseProportioningDialog.ProportionWaterUseGrid;
    lGrid.ColCount := lWaterDemandConfig.DemandCategoryCount + 2;
    lGrid.RowCount := LCount + 1;
    lGrid.Enabled  := TRUE;
    lGrid.Options  := lGrid.Options + [goEditing];
    WaterUseProportioningDialog.Resize;

    LField := FAppModules.FieldProperties.FieldProperty('WaterUsePortionTotal');
    lGrid.AddFieldProperty(LField);
    lGrid.IsColumnEnabled[0] := FALSE;
    LField := FAppModules.FieldProperties.FieldProperty('WaterUsePortion');
    for lColIndex := 1 to lGrid.ColCount - 2 do
    begin
      lGrid.IsColumnEnabled[lColIndex] := TRUE;
      lGrid.AddFieldProperty(LField);
    end;
    LField := FAppModules.FieldProperties.FieldProperty('WaterUsePortionTotal');
    lGrid.AddFieldProperty(LField);

    lGrid.IsColumnEnabled[lGrid.ColCount - 1] := FALSE;

    lGrid.Cells[lGrid.ColCount - 1, 0] := FAppModules.Language.GetString('GridHeading.Total');
    lGrid.Cells[0,0] := FAppModules.Language.GetString('WaterUse.Channel');
    for lRowIndex := 0 to LCount -1 do
    begin
      LProportion := lWaterDemandConfig.WaterUseOutputProportionByIndex[lRowIndex];
      if (LProportion <> nil) then
      begin
        LChannel := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.
                      ChannelList.ChannelByChannelNumber[LProportion.ChannelNumber];
        if (LChannel <> nil) then
          lGrid.Cells[0, lRowIndex + 1] := LChannel.ChannelName
        else
          lGrid.Cells[0, lRowIndex + 1] := '';
        for lColIndex := LField.ArrayLow to LField.ArrayHigh do
        begin
          lCategory := lWaterDemandConfig.DemandCategoryByID[lColIndex];
          if (lCategory <> nil) then
          begin
            lGrid.Cells[lColIndex, 0] := lCategory.CategoryName;
            lGrid.Cells[lColIndex, lRowIndex + 1] := FloatToStr(LProportion.ProportionByIndex[lColIndex]);
          end;
        end;
        lGrid.Cells[lGrid.ColCount -1, lRowIndex + 1] := FloatToStr(LProportion.Total);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TWaterUseProportioningValidator.OnEditControlEnter(Sender: TObject);
const OPNAME = 'TWaterUseProportioningValidator.OnEditControlEnter';
begin
  inherited OnEditControlEnter(Sender);
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TWaterUseProportioningValidator.OnEditControltExit(Sender: TObject);
const OPNAME = 'TWaterUseProportioningValidator.OnEditControltExit';
begin
  inherited OnEditControltExit(Sender);
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;


procedure TWaterUseProportioningValidator.OnReconciliationGridSelectCell(ASender       : TObject;
                                                                         ACol, ARow    : Longint;
                                                                         var CanSelect : Boolean);
const OPNAME = 'TWaterUseProportioningValidator.OnReconciliationGridSelectCell';
var
  lConfigData : IWaterDemandConfiguration;
begin
  try
    CanSelect := True;
    lConfigData := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.
                     WaterDemandConfiguration;
    with WaterUseProportioningDialog do
    begin
      if ( ARow > 0 ) then
      begin
        FSelectedChannelIndex  := ARow - 1;
        FSelectedChannelNumber := lConfigData.WaterUseOutputProportionByIndex[FSelectedChannelIndex].ChannelNumber;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TWaterUseProportioningValidator.UpdateProportionWaterUse (ARow   : integer;
                                                                    ACol   : integer;
                                                                    AValue : string);
const OPNAME = 'TWaterUseProportioningValidator.UpdateProportionWaterUse';
var
  lProportion : IWaterUseOutputProportion;
  lValue      : double;
  lMessage    : string;
begin
  try
    lProportion := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.
                     WaterDemandConfiguration.WaterUseOutputProportionByIndex[FSelectedChannelIndex];
    if (lProportion <> nil) then
    begin
      with WaterUseProportioningDialog do
      begin
        if (Trim(AValue) = '') then
          AValue := '0.0';
        ProportionWaterUseGrid.ValidationError[ACol+1, ARow, gveCellField] := '';
        if (FAppModules.FieldProperties.ValidateFieldProperty(
              ProportionWaterUseGrid.FieldProperty ( ACol + 1 ).FieldName,
              AValue, lMessage, ACol + 1 )) then
        begin
          LValue := StrToFloat(AValue);
          lProportion.ProportionByIndex [ACol + 1] := LValue;
          FValidateChannelIndex := ARow;
          DoContextValidation ( dvtWaterUseCategoryPortion );
          FValidateChannelIndex := 0;
          PopulateDataViewer;
        end
        else
          ProportionWaterUseGrid.ValidationError[ACol+1, ARow, gveCellField] := lMessage
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TWaterUseProportioningValidator.OnStringGridCellDataHasChanged
                                         (ASender: TObject; ACol, ARow: integer);
const OPNAME = 'TWaterUseProportioningValidator.OnStringGridCellDataHasChanged';
begin
  inherited OnStringGridCellDataHasChanged(ASender, ACol, ARow);
  try
    with WaterUseProportioningDialog do
    begin
      if ((ProportionWaterUseGrid = ASender) and ( ACol >= 1 ) ) then
        UpdateProportionWaterUse ( ARow, ACol - 1, Trim ( ProportionWaterUseGrid.Cells [ ACol, ARow ] ) )
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TWaterUseProportioningValidator.DoContextValidation ( AValidationType : TDialogValidationType );
const OPNAME = 'TWaterUseProportioningValidator.DoContextValidation';
var
  LWaterDemandConfiguration : IWaterDemandConfiguration;
  LWaterUseOutputProportion : IWaterUseOutputProportion;
begin
  try
    FAllErrorMessages.Clear;
    LWaterDemandConfiguration := TYieldModelDataObject(FAppModules.Model.ModelData).
                                 NetworkFeaturesData.WaterDemandConfiguration;
    if LWaterDemandConfiguration <> nil then
    begin
      LWaterUseOutputProportion := LWaterDemandConfiguration.WaterUseOutputProportionByIndex [ FSelectedChannelIndex ];
      case AValidationType of
      dvtWaterUseCategoryPortion :
        begin
          ValidateWaterUsePortion ( LWaterUseOutputProportion );
          ValidateWaterUsePortionTotal ( LWaterUseOutputProportion );
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;

end;


function TWaterUseProportioningValidator.ProcessMetaDataEvent: boolean;
const OPNAME = 'TWaterUseProportioningValidator.ProcessMetaDataEvent';
{var
  lFieldProperty   : TAbstractFieldProperty;
  lKeyValues       : string;
  lFieldIndex      : string;
  lFeature         : IWaterDemandFeature;
  lWaterDemandList : IWaterDemandFeatureList;
  }
begin

  Result := FALSE;
  (*
  try
    lWaterDemandList := TYieldModelDataObject(FAppModules.Model.ModelData).
                          NetworkFeaturesData.WaterDemandFeatureList;
    if (FPanel.Visible AND (FActiveControl <> nil) AND (lWaterDemandList <> nil)) then
    begin
      with WaterUseProportioningDialog do
      begin
        lFieldProperty := nil;
        if (FActiveControl = ChannelCountEdit) then
        begin
          lFieldProperty := ChannelCountEdit.FieldProperty;
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
  *)
end;
{
procedure TWaterUseProportioningValidator.AddPorportionTotal ( AConfigData : IWaterUseCategory);
const OPNAME = 'TWaterUseProportioningValidator.AddPorportionTotal';
begin
  try
    if AConfigData <> nil then
      AConfigData.WaterUsePortionTotal [ AConfigData.ChannelID ];
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;
}
procedure TWaterUseProportioningValidator.ValidateWaterUsePortion (AWaterUseOutputProportion : IWaterUseOutputProportion);
const OPNAME = 'TWaterUseProportioningValidator.ValidateWaterUsePortion';
var
  LErrorCols  : TStringList;
  LErrorMsgs  : TStringList;
  LCol        : integer;
  LIndex      : integer;
  LDemandConf : IWaterDemandConfiguration;
begin
  try
    if ( AWaterUseOutputProportion <> nil ) then
    begin
      LDemandConf := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.WaterDemandConfiguration;
      lErrorCols := TStringList.Create;
      lErrorMsgs := TStringList.Create;
      try
        lErrorCols.Clear;
        FErrorMessage := '';
        if ( AWaterUseOutputProportion.Validate ( FErrorMessage, 'WaterUsePortion' ) ) then
        begin
          for lCol := 1 to LDemandConf.DemandCategoryCount  do
          WaterUseProportioningDialog.ProportionWaterUseGrid.ValidationError [ lCol, FValidateChannelIndex, gveCellContext] := ''
        end
        else
        begin
          ExtractErrorsAndColumns(FErrorMessage, lErrorMsgs, lErrorCols);
          for lCol := 1 to LDemandConf.DemandCategoryCount do
          begin
            lIndex := lErrorCols.IndexOf(IntToStr(lCol));
            if (lIndex >= 0) then
              WaterUseProportioningDialog.ProportionWaterUseGrid.ValidationError[lCol, FValidateChannelIndex + 1, gveCellContext] := lErrorMsgs.Strings[lIndex]
            else
              WaterUseProportioningDialog.ProportionWaterUseGrid.ValidationError[lCol, FValidateChannelIndex + 1, gveCellContext] := ''
          end;
          FAllErrorMessages.AddStrings(lErrorMsgs);
        end;
      finally
        FreeAndNil(lErrorCols);
        FreeAndNil(lErrorMsgs);
      end;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TWaterUseProportioningValidator.ValidateWaterUsePortionTotal ( AWaterUseOutputProportion : IWaterUseOutputProportion );
const OPNAME = 'TWaterUseProportioningValidator.ValidateWaterUsePortionTotal';
begin
  try
    if ( AWaterUseOutputProportion <> nil ) then
    begin
      FErrorMessage := '';
      if ( AWaterUseOutputProportion.Validate ( FErrorMessage, 'WaterUsePortionTotal' ) ) then
          WaterUseProportioningDialog.ProportionWaterUseGrid.ValidationError
          [ WaterUseProportioningDialog.ProportionWaterUseGrid.ColCount -1, FValidateChannelIndex + 1, gveCellContext] := ''
      else
          WaterUseProportioningDialog.ProportionWaterUseGrid.ValidationError
          [ WaterUseProportioningDialog.ProportionWaterUseGrid.ColCount -1, FValidateChannelIndex + 1, gveCellContext] := FErrorMessage;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TWaterUseProportioningValidator.ValidateWaterUseCategories;
const OPNAME = 'TWaterUseProportioningValidator.ValidateWaterUseCategories';
var
  LWaterDemandConfiguration : IWaterDemandConfiguration;
  LWaterUseOutputProportion : IWaterUseOutputProportion;
  LIndex                    : integer;
begin
  try
    FAllErrorMessages.Clear;
    LWaterDemandConfiguration := TYieldModelDataObject(FAppModules.Model.ModelData).
                                 NetworkFeaturesData.WaterDemandConfiguration;
    if LWaterDemandConfiguration <> nil then
    begin
      for LIndex := 0 to LWaterDemandConfiguration.WaterUseOutputProportionCount -1 do
      begin
        LWaterUseOutputProportion := LWaterDemandConfiguration.WaterUseOutputProportionByIndex [ LIndex ];
        FValidateChannelIndex := lIndex;
        ValidateWaterUsePortion ( LWaterUseOutputProportion );
        ValidateWaterUsePortionTotal ( LWaterUseOutputProportion );
        FValidateChannelIndex := 0;
      end;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

end.

