//
//
//  UNIT      : Contains the class TDDTSInputMinMaxValidator.
//  AUTHOR    : Sam Dhlamini
//  DATE      : 2014/07/17
//  COPYRIGHT : Copyright © 2003 DWAF
//
//
unit UDDTSInputMinMaxValidator;

interface

uses
  Classes,
  VCL.Controls,
  VCL.ComCtrls,
  VCL.StdCtrls,
  VCL.ExtCtrls,
  VCL.Dialogs,

  // arivia.kom
  VoaimsCom_TLB,
  UAbstractObject,
  UAbstractComponent,
  UDataComponent,
  UDataEditComponent,
  UDDTSInputMinMaxDialog,
  UDDTSData,
  UAbstractYieldDataDialogValidator,
  UYieldContextValidationType;
type
  TDDTSInputMinMaxValidator = class(TAbstractYieldDataDialogValidator)
  protected
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    procedure OnEditControlEnter(Sender: TObject); override;
    procedure OnEditControltExit(Sender: TObject); override;


   // procedure UpdateRunoffScaleFactor;
   // procedure UpdateOtherInflowScale;
  //  procedure UpdateEWRScale;
 //   procedure UpdateTargetDraft;
  //  procedure UpdateDSRequiments;
  //  procedure UpdateDSPercRelease;
 //   procedure UpdateSpillPercRelease;
 //   procedure UpdateEWRPercRelease;

    procedure UpdateMaxRunoff;
    procedure UpdateMinRunoff;
    procedure UpdateMinOtherInflow;
    procedure UpdateMaxOtherInflow;
    procedure UpdateMinEvaporation;
    procedure UpdateMaxEvaporation;
    procedure UpdateMinRainfall;
    procedure UpdateMaxRainfall;
    procedure UpdateMinIncreamentalRunoff;
    procedure UpdateMaxIncreamentalRunoff;
    procedure UpdateMinEWR;
    procedure UpdateMaxEWR;

    procedure RePopulateDataViewer;
    function  CurrentReservoir:IReservoirData;
  public
    function Initialise: boolean; override;
    function SaveState: boolean; override;
    function LanguageHasChanged: boolean; override;
    function StudyHasChanged: boolean; override;
    function StudyDataHasChanged(AContext: TChangeContext; AFieldName,AOldValue,ANewValue: string): boolean; override;
    procedure ClearDataViewer; override;
    procedure PopulateDataViewer; override;

    function DDTSInputMinMaxDialog: TDDTSInputMinMaxDialog;
    procedure DoContextValidation (AValidationType : TDialogValidationType); override;
  end;

implementation

uses
  SysUtils,
  VCL.Graphics,
  UFileNames,
  UConstants,
  UAbstractFileNamesObject,
  UDDTSDataObject,
  UErrorHandlingOperations, Math;

{ TDDTSInputMinMaxValidator }

procedure TDDTSInputMinMaxValidator.CreateMemberObjects;
const OPNAME = 'TDDTSInputMinMaxValidator.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
    FIdentifier := -1;
    FPanel := TDDTSInputMinMaxDialog.Create(FPanelOwner,FAppModules);
         {
    DDTSInputMinMaxDialog.RunoffScaleFactorEdit.FieldProperty := FAppModules.FieldProperties.FieldProperty('RunoffScaleFactor');
    DDTSInputMinMaxDialog.RunoffScaleFactorEdit.OnEnter       := OnEditControlEnter;
    DDTSInputMinMaxDialog.RunoffScaleFactorEdit.OnExit        := OnEditControltExit;

    DDTSInputMinMaxDialog.OtherInflowScaleFactorEdit.FieldProperty := FAppModules.FieldProperties.FieldProperty('OtherInflowScaleFactor');
    DDTSInputMinMaxDialog.OtherInflowScaleFactorEdit.OnEnter       := OnEditControlEnter;
    DDTSInputMinMaxDialog.OtherInflowScaleFactorEdit.OnExit        := OnEditControltExit;

    DDTSInputMinMaxDialog.EWRScaleFactorEdit.FieldProperty := FAppModules.FieldProperties.FieldProperty('EWRScaleFactor');
    DDTSInputMinMaxDialog.EWRScaleFactorEdit.OnEnter       := OnEditControlEnter;
    DDTSInputMinMaxDialog.EWRScaleFactorEdit.OnExit        := OnEditControltExit;

    DDTSInputMinMaxDialog.TargetDraftEdit.FieldProperty := FAppModules.FieldProperties.FieldProperty('TargetDraft');
    DDTSInputMinMaxDialog.TargetDraftEdit.OnEnter       := OnEditControlEnter;
    DDTSInputMinMaxDialog.TargetDraftEdit.OnExit        := OnEditControltExit;

    DDTSInputMinMaxDialog.DSRequirmentEdit.FieldProperty := FAppModules.FieldProperties.FieldProperty('DSRequirment');
    DDTSInputMinMaxDialog.DSRequirmentEdit.OnEnter       := OnEditControlEnter;
    DDTSInputMinMaxDialog.DSRequirmentEdit.OnExit        := OnEditControltExit;

    DDTSInputMinMaxDialog.DSPercReleaseEdit.FieldProperty := FAppModules.FieldProperties.FieldProperty('DSPercRelease');
    DDTSInputMinMaxDialog.DSPercReleaseEdit.OnEnter       := OnEditControlEnter;
    DDTSInputMinMaxDialog.DSPercReleaseEdit.OnExit        := OnEditControltExit;


    DDTSInputMinMaxDialog.SpillPercReleaseEdit.FieldProperty := FAppModules.FieldProperties.FieldProperty('SpillPercRelease');
    DDTSInputMinMaxDialog.SpillPercReleaseEdit.OnEnter       := OnEditControlEnter;
    DDTSInputMinMaxDialog.SpillPercReleaseEdit.OnExit        := OnEditControltExit;

    DDTSInputMinMaxDialog.EWRPercReleaseEdit.FieldProperty := FAppModules.FieldProperties.FieldProperty('EWRPercRelease');
    DDTSInputMinMaxDialog.EWRPercReleaseEdit.OnEnter       := OnEditControlEnter;
    DDTSInputMinMaxDialog.EWRPercReleaseEdit.OnExit        := OnEditControltExit;
         }
    DDTSInputMinMaxDialog.MaxRunoffEdit.FieldProperty := FAppModules.FieldProperties.FieldProperty('MaxRunoff');
    DDTSInputMinMaxDialog.MaxRunoffEdit.OnEnter       := OnEditControlEnter;
    DDTSInputMinMaxDialog.MaxRunoffEdit.OnExit        := OnEditControltExit;

    DDTSInputMinMaxDialog.MinRunoffEdit.FieldProperty := FAppModules.FieldProperties.FieldProperty('MinRunoff');
    DDTSInputMinMaxDialog.MinRunoffEdit.OnEnter       := OnEditControlEnter;
    DDTSInputMinMaxDialog.MinRunoffEdit.OnExit        := OnEditControltExit;

    DDTSInputMinMaxDialog.MinOtherInflowEdit.FieldProperty := FAppModules.FieldProperties.FieldProperty('MinOtherInflow');
    DDTSInputMinMaxDialog.MinOtherInflowEdit.OnEnter       := OnEditControlEnter;
    DDTSInputMinMaxDialog.MinOtherInflowEdit.OnExit        := OnEditControltExit;

    DDTSInputMinMaxDialog.MaxOtherInflowEdit.FieldProperty := FAppModules.FieldProperties.FieldProperty('MaxOtherInflow');
    DDTSInputMinMaxDialog.MaxOtherInflowEdit.OnEnter       := OnEditControlEnter;
    DDTSInputMinMaxDialog.MaxOtherInflowEdit.OnExit        := OnEditControltExit;

    DDTSInputMinMaxDialog.MinRainfallEdit.FieldProperty := FAppModules.FieldProperties.FieldProperty('MinRainfall');
    DDTSInputMinMaxDialog.MinRainfallEdit.OnEnter       := OnEditControlEnter;
    DDTSInputMinMaxDialog.MinRainfallEdit.OnExit        := OnEditControltExit;

    DDTSInputMinMaxDialog.MaxRainfallEdit.FieldProperty := FAppModules.FieldProperties.FieldProperty('MaxRainfall');
    DDTSInputMinMaxDialog.MaxRainfallEdit.OnEnter       := OnEditControlEnter;
    DDTSInputMinMaxDialog.MaxRainfallEdit.OnExit        := OnEditControltExit;

    DDTSInputMinMaxDialog.MaxEvaporationEdit.FieldProperty := FAppModules.FieldProperties.FieldProperty('MaxEvaporation');
    DDTSInputMinMaxDialog.MaxEvaporationEdit.OnEnter       := OnEditControlEnter;
    DDTSInputMinMaxDialog.MaxEvaporationEdit.OnExit        := OnEditControltExit;

    DDTSInputMinMaxDialog.MinEvaporationEdit.FieldProperty := FAppModules.FieldProperties.FieldProperty('MinEvaporation');
    DDTSInputMinMaxDialog.MinEvaporationEdit.OnEnter       := OnEditControlEnter;
    DDTSInputMinMaxDialog.MinEvaporationEdit.OnExit        := OnEditControltExit;

    DDTSInputMinMaxDialog.MaxIncreamentalRunoffEdit.FieldProperty := FAppModules.FieldProperties.FieldProperty('MaxIncreamentalRunoff');
    DDTSInputMinMaxDialog.MaxIncreamentalRunoffEdit.OnEnter       := OnEditControlEnter;
    DDTSInputMinMaxDialog.MaxIncreamentalRunoffEdit.OnExit        := OnEditControltExit;

    DDTSInputMinMaxDialog.MinIncreamentalRunoffEdit.FieldProperty := FAppModules.FieldProperties.FieldProperty('MinIncreamentalRunoff');
    DDTSInputMinMaxDialog.MinIncreamentalRunoffEdit.OnEnter       := OnEditControlEnter;
    DDTSInputMinMaxDialog.MinIncreamentalRunoffEdit.OnExit        := OnEditControltExit;

    DDTSInputMinMaxDialog.MaxEWREdit.FieldProperty := FAppModules.FieldProperties.FieldProperty('MaxEWR');
    DDTSInputMinMaxDialog.MaxEWREdit.OnEnter       := OnEditControlEnter;
    DDTSInputMinMaxDialog.MaxEWREdit.OnExit        := OnEditControltExit;

    DDTSInputMinMaxDialog.MinEWREdit.FieldProperty := FAppModules.FieldProperties.FieldProperty('MinEWR');
    DDTSInputMinMaxDialog.MinEWREdit.OnEnter       := OnEditControlEnter;
    DDTSInputMinMaxDialog.MinEWREdit.OnExit        := OnEditControltExit;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDDTSInputMinMaxValidator.DestroyMemberObjects;
const OPNAME = 'TDDTSInputMinMaxValidator.DestroyMemberObjects';
begin
  try
    inherited DestroyMemberObjects;

  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDDTSInputMinMaxValidator.Initialise: boolean;
const OPNAME = 'TDDTSInputMinMaxValidator.Initialise';
begin
  Result := inherited Initialise;
  try
  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDDTSInputMinMaxValidator.LanguageHasChanged: boolean;
const OPNAME = 'TDDTSInputMinMaxValidator.LanguageHasChanged';
begin
  Result := False;
  try
    TabShetCaption := 'Input Min-Max';
    Result := inherited LanguageHasChanged;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDDTSInputMinMaxValidator.ClearDataViewer;
const OPNAME = 'TDDTSInputMinMaxValidator.ClearDataViewer';
begin
  inherited ClearDataViewer;
  try

  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDDTSInputMinMaxValidator.RePopulateDataViewer;
const OPNAME = 'TDDTSInputMinMaxValidator.RePopulateDataViewer';
var
  LDDTSDetailData : TDDTSDetailData;
begin
  try
    if(FIdentifier >= 0) then
    begin
      LDDTSDetailData :=   TDDTSDataObject(FAppModules.Model.ModelData).DDTSDamDataList.GetDDTSDetailDataByIdentifier(FIdentifier);
      if (LDDTSDetailData <> nil) then
      begin
        with DDTSInputMinMaxDialog do
        begin
        {
          RunoffScaleFactorEdit.SetFieldValue(LDDTSDetailData.RunoffScale);
          OtherInflowScaleFactorEdit.SetFieldValue(LDDTSDetailData.OtherInflowScale);
          EWRScaleFactorEdit.SetFieldValue(LDDTSDetailData.EWRScale);
          TargetDraftEdit.SetFieldValue(LDDTSDetailData.TargetDraft);
          DSRequirmentEdit.SetFieldValue(LDDTSDetailData.DSRequiments);
          DSPercReleaseEdit.SetFieldValue(LDDTSDetailData.DSPercRelease);
          SpillPercReleaseEdit.SetFieldValue(LDDTSDetailData.SpillPercRelease);
          EWRPercReleaseEdit.SetFieldValue(LDDTSDetailData.EWRPercRelease);
                }
          MaxRunoffEdit.SetFieldValue(LDDTSDetailData.MaxRunoff);
          MinRunoffEdit.SetFieldValue(LDDTSDetailData.MinRunoff);
          MinOtherInflowEdit.SetFieldValue(LDDTSDetailData.MinOtherInflow);
          MaxOtherInflowEdit.SetFieldValue(LDDTSDetailData.MaxOtherInflow);
          MinEvaporationEdit.SetFieldValue(LDDTSDetailData.MinEvaporation);
          MaxEvaporationEdit.SetFieldValue(LDDTSDetailData.MaxEvaporation);
          MinRainfallEdit.SetFieldValue(LDDTSDetailData.MinRainfall);
          MaxRainfallEdit.SetFieldValue(LDDTSDetailData.MaxRainfall);
          MinIncreamentalRunoffEdit.SetFieldValue(LDDTSDetailData.MinIncreamentalRunoff);
          MaxIncreamentalRunoffEdit.SetFieldValue(LDDTSDetailData.MaxIncreamentalRunoff);
          MinEWREdit.SetFieldValue(LDDTSDetailData.MinEWR);
          MaxEWREdit.SetFieldValue(LDDTSDetailData.MaxEWR);


        end;
        DoContextValidation(dvtResPropAll);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDDTSInputMinMaxValidator.SaveState: boolean;
const OPNAME = 'TDDTSInputMinMaxValidator.SaveState';
begin
  Result := inherited SaveState;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDDTSInputMinMaxValidator.OnEditControlEnter(Sender: TObject);
const OPNAME = 'TDDTSInputMinMaxValidator.OnEditControlEnter';
begin
  inherited OnEditControlEnter(Sender);
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDDTSInputMinMaxValidator.OnEditControltExit(Sender: TObject);
const OPNAME = 'TDDTSInputMinMaxValidator.OnEditControltExit';
begin
  inherited OnEditControltExit(Sender);
  try
    {
    if ((sender = DDTSInputMinMaxDialog.RunoffScaleFactorEdit) AND
       (DDTSInputMinMaxDialog.RunoffScaleFactorEdit.HasValueChanged)) then
       UpdateRunoffScaleFactor

    else
    if ((sender = DDTSInputMinMaxDialog.OtherInflowScaleFactorEdit) AND
       (DDTSInputMinMaxDialog.OtherInflowScaleFactorEdit.HasValueChanged)) then
       UpdateOtherInflowScale
    else
    if ((sender = DDTSInputMinMaxDialog.EWRScaleFactorEdit) AND
       (DDTSInputMinMaxDialog.EWRScaleFactorEdit.HasValueChanged)) then
       UpdateEWRScale
    else
    if ((sender = DDTSInputMinMaxDialog.TargetDraftEdit) AND
       (DDTSInputMinMaxDialog.TargetDraftEdit.HasValueChanged)) then
       UpdateTargetDraft
    else
    if ((sender = DDTSInputMinMaxDialog.DSRequirmentEdit) AND
       (DDTSInputMinMaxDialog.DSRequirmentEdit.HasValueChanged)) then
       UpdateDSRequiments
    else}
    if ((sender = DDTSInputMinMaxDialog.MaxRunoffEdit) AND
       (DDTSInputMinMaxDialog.MaxRunoffEdit.HasValueChanged)) then
       UpdateMaxRunoff
    else
    if ((sender = DDTSInputMinMaxDialog.MinRunoffEdit) AND
       (DDTSInputMinMaxDialog.MinRunoffEdit.HasValueChanged)) then
       UpdateMinRunoff
    else
    if ((sender = DDTSInputMinMaxDialog.MinOtherInflowEdit) AND
       (DDTSInputMinMaxDialog.MinOtherInflowEdit.HasValueChanged)) then
        UpdateMinOtherInflow
    else
    if ((sender = DDTSInputMinMaxDialog.MaxOtherInflowEdit) AND
       (DDTSInputMinMaxDialog.MaxOtherInflowEdit.HasValueChanged)) then
      UpdateMaxOtherInflow

    else
    if ((sender = DDTSInputMinMaxDialog.MinEvaporationEdit) AND
       (DDTSInputMinMaxDialog.MinEvaporationEdit.HasValueChanged)) then
      UpdateMinEvaporation
    else
    if ((sender = DDTSInputMinMaxDialog.MaxEvaporationEdit) AND
       (DDTSInputMinMaxDialog.MaxEvaporationEdit.HasValueChanged)) then
      UpdateMaxEvaporation
    else
    if ((sender = DDTSInputMinMaxDialog.MinRainfallEdit) AND
       (DDTSInputMinMaxDialog.MinRainfallEdit.HasValueChanged)) then
      UpdateMinRainfall
    else
    if ((sender = DDTSInputMinMaxDialog.MaxRainfallEdit) AND
       (DDTSInputMinMaxDialog.MaxRainfallEdit.HasValueChanged)) then
      UpdateMaxRainfall
    else
    if ((sender = DDTSInputMinMaxDialog.MinIncreamentalRunoffEdit) AND
       (DDTSInputMinMaxDialog.MinIncreamentalRunoffEdit.HasValueChanged)) then
      UpdateMinIncreamentalRunoff
    else
    if ((sender = DDTSInputMinMaxDialog.MaxIncreamentalRunoffEdit) AND
       (DDTSInputMinMaxDialog.MaxIncreamentalRunoffEdit.HasValueChanged)) then
      UpdateMaxIncreamentalRunoff
    else
    if ((sender = DDTSInputMinMaxDialog.MinEWREdit) AND
       (DDTSInputMinMaxDialog.MinEWREdit.HasValueChanged)) then
      UpdateMinEWR
    else
    if ((sender = DDTSInputMinMaxDialog.MaxEWREdit) AND
       (DDTSInputMinMaxDialog.MaxEWREdit.HasValueChanged)) then
      UpdateMaxEWR;{
    else
    if ((sender = DDTSInputMinMaxDialog.DSPercReleaseEdit) AND
       (DDTSInputMinMaxDialog.DSPercReleaseEdit.HasValueChanged)) then
       UpdateDSPercRelease
    else
    if ((sender = DDTSInputMinMaxDialog.SpillPercReleaseEdit) AND
       (DDTSInputMinMaxDialog.SpillPercReleaseEdit.HasValueChanged)) then
       UpdateSpillPercRelease
    else
    if ((sender = DDTSInputMinMaxDialog.EWRPercReleaseEdit) AND
       (DDTSInputMinMaxDialog.EWRPercReleaseEdit.HasValueChanged)) then
       UpdateEWRPercRelease;
        }
      RePopulateDataViewer;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDDTSInputMinMaxValidator.DDTSInputMinMaxDialog:TDDTSInputMinMaxDialog;
const OPNAME = 'TDDTSInputMinMaxValidator.DDTSInputMinMaxDialog';
begin
  Result := Nil;
  try
    if Assigned(FPanel) then
      Result := TDDTSInputMinMaxDialog(FPanel);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDDTSInputMinMaxValidator.StudyDataHasChanged(AContext: TChangeContext; AFieldName,AOldValue,ANewValue: string): boolean;
const OPNAME = 'TDDTSInputMinMaxValidator.StudyDataHasChanged';
begin
  Result := inherited StudyDataHasChanged(AContext,AFieldName,AOldValue,ANewValue);
  try

      RePopulateDataViewer;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDDTSInputMinMaxValidator.StudyHasChanged: boolean;
const OPNAME = 'TDDTSInputMinMaxValidator.StudyHasChanged';
begin
  Result := inherited StudyHasChanged;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;
{
procedure TDDTSInputMinMaxValidator.UpdateDSPercRelease;
const OPNAME = 'TDDTSInputMinMaxValidator.UpdateDSPercRelease';
var
  LDDTSDetailData : TDDTSDetailData;
  LErrorMessage: string;
begin
  try
    if DDTSInputMinMaxDialog.DSPercReleaseEdit.HasValueChanged then
    begin
      LDDTSDetailData :=  TDDTSDataObject(FAppModules.Model.ModelData).DDTSDamDataList.GetDDTSDetailDataByIdentifier(FIdentifier);
      if (LDDTSDetailData <> nil) then
      begin
        with DDTSInputMinMaxDialog do
        begin
          if (FAppModules.FieldProperties.ValidateFieldProperty(
             DSPercReleaseEdit.FieldProperty.FieldName,
              DSPercReleaseEdit.Text,LErrorMessage)) then
          begin
            LDDTSDetailData.DSPercRelease := StrToFloat(DSPercReleaseEdit.Text);
            DSPercReleaseEdit.SetFieldValue(LDDTSDetailData.DSPercRelease);
            //DoContextValidation(dvtRunoffScale);
          end;
          DSPercReleaseEdit.ContextValidationError := LErrorMessage;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDDTSInputMinMaxValidator.UpdateDSRequiments;
const OPNAME = 'TDDTSInputMinMaxValidator.UpdateDSRequiments';
var
  LDDTSDetailData : TDDTSDetailData;
  LErrorMessage: string;
begin
  try
    if DDTSInputMinMaxDialog.DSRequirmentEdit.HasValueChanged then
    begin
      LDDTSDetailData :=  TDDTSDataObject(FAppModules.Model.ModelData).DDTSDamDataList.GetDDTSDetailDataByIdentifier(FIdentifier);
      if (LDDTSDetailData <> nil) then
      begin
        with DDTSInputMinMaxDialog do
        begin
          if (FAppModules.FieldProperties.ValidateFieldProperty(
             DSRequirmentEdit.FieldProperty.FieldName,
              DSRequirmentEdit.Text,LErrorMessage)) then
          begin
            LDDTSDetailData.DSRequiments := StrToFloat(DSRequirmentEdit.Text);
            DSRequirmentEdit.SetFieldValue(LDDTSDetailData.DSRequiments);
            //DoContextValidation(dvtRunoffScale);
          end;
          DSRequirmentEdit.ContextValidationError := LErrorMessage;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;
 }
procedure TDDTSInputMinMaxValidator.UpdateMaxEvaporation;
const OPNAME = 'TDDTSInputMinMaxValidator.UpdateMaxEvaporation';
var
  LDDTSDetailData : TDDTSDetailData;
  LErrorMessage: string;
begin
  try
    if DDTSInputMinMaxDialog.MaxEvaporationEdit.HasValueChanged then
    begin
      LDDTSDetailData :=  TDDTSDataObject(FAppModules.Model.ModelData).DDTSDamDataList.GetDDTSDetailDataByIdentifier(FIdentifier);
      if (LDDTSDetailData <> nil) then
      begin
        with DDTSInputMinMaxDialog do
        begin
          if (FAppModules.FieldProperties.ValidateFieldProperty(
             MaxEvaporationEdit.FieldProperty.FieldName,
              MaxEvaporationEdit.Text,LErrorMessage)) then
          begin
            LDDTSDetailData.MaxEvaporation := StrToFloat(MaxEvaporationEdit.Text);
            MaxEvaporationEdit.SetFieldValue(LDDTSDetailData.MaxEvaporation);
          end;
          MaxEvaporationEdit.ContextValidationError := LErrorMessage;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDDTSInputMinMaxValidator.UpdateMaxEWR;
const OPNAME = 'TDDTSInputMinMaxValidator.UpdateMaxEWR';
var
  LDDTSDetailData : TDDTSDetailData;
  LErrorMessage: string;
begin
  try
    if DDTSInputMinMaxDialog.MaxEWREdit.HasValueChanged then
    begin
      LDDTSDetailData :=  TDDTSDataObject(FAppModules.Model.ModelData).DDTSDamDataList.GetDDTSDetailDataByIdentifier(FIdentifier);
      if (LDDTSDetailData <> nil) then
      begin
        with DDTSInputMinMaxDialog do
        begin
          if (FAppModules.FieldProperties.ValidateFieldProperty(
             MaxEWREdit.FieldProperty.FieldName,
             MaxEWREdit.Text,LErrorMessage)) then
          begin
            LDDTSDetailData.MaxEWR := StrToFloat(MaxEWREdit.Text);
            MaxEWREdit.SetFieldValue(LDDTSDetailData.MaxEWR);
          end;
          MaxEWREdit.ContextValidationError := LErrorMessage;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


procedure TDDTSInputMinMaxValidator.UpdateMaxIncreamentalRunoff;
const OPNAME = 'TDDTSInputMinMaxValidator.UpdateMaxIncreamentalRunoff';
var
  LDDTSDetailData : TDDTSDetailData;
  LErrorMessage: string;
begin
  try
    if DDTSInputMinMaxDialog.MaxIncreamentalRunoffEdit.HasValueChanged then
    begin
      LDDTSDetailData :=  TDDTSDataObject(FAppModules.Model.ModelData).DDTSDamDataList.GetDDTSDetailDataByIdentifier(FIdentifier);
      if (LDDTSDetailData <> nil) then
      begin
        with DDTSInputMinMaxDialog do
        begin
          if (FAppModules.FieldProperties.ValidateFieldProperty(
             MaxIncreamentalRunoffEdit.FieldProperty.FieldName,
             MaxIncreamentalRunoffEdit.Text,LErrorMessage)) then
          begin
            LDDTSDetailData.MaxIncreamentalRunoff := StrToFloat(MaxIncreamentalRunoffEdit.Text);
            MaxIncreamentalRunoffEdit.SetFieldValue(LDDTSDetailData.MaxIncreamentalRunoff);
          end;
          MaxIncreamentalRunoffEdit.ContextValidationError := LErrorMessage;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;



procedure TDDTSInputMinMaxValidator.UpdateMaxOtherInflow;
const OPNAME = 'TDDTSInputMinMaxValidator.UpdateMaxOtherInflow';
var
  LDDTSDetailData : TDDTSDetailData;
  LErrorMessage: string;
begin
  try
    if DDTSInputMinMaxDialog.MaxOtherInflowEdit.HasValueChanged then
    begin
      LDDTSDetailData :=  TDDTSDataObject(FAppModules.Model.ModelData).DDTSDamDataList.GetDDTSDetailDataByIdentifier(FIdentifier);
      if (LDDTSDetailData <> nil) then
      begin
        with DDTSInputMinMaxDialog do
        begin
          if (FAppModules.FieldProperties.ValidateFieldProperty(
             MaxOtherInflowEdit.FieldProperty.FieldName,
              MaxOtherInflowEdit.Text,LErrorMessage)) then
          begin
            LDDTSDetailData.MaxOtherInflow := StrToFloat(MaxOtherInflowEdit.Text);
            MaxOtherInflowEdit.SetFieldValue(LDDTSDetailData.MaxOtherInflow);
          end;
          MaxOtherInflowEdit.ContextValidationError := LErrorMessage;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDDTSInputMinMaxValidator.UpdateMaxRainfall;
const OPNAME = 'TDDTSInputMinMaxValidator.UpdateMaxRainfall';
var
  LDDTSDetailData : TDDTSDetailData;
  LErrorMessage: string;
begin
  try
    if DDTSInputMinMaxDialog.MaxRainfallEdit.HasValueChanged then
    begin
      LDDTSDetailData :=  TDDTSDataObject(FAppModules.Model.ModelData).DDTSDamDataList.GetDDTSDetailDataByIdentifier(FIdentifier);
      if (LDDTSDetailData <> nil) then
      begin
        with DDTSInputMinMaxDialog do
        begin
          if (FAppModules.FieldProperties.ValidateFieldProperty(
             MaxRainfallEdit.FieldProperty.FieldName,
             MaxRainfallEdit.Text,LErrorMessage)) then
          begin
            LDDTSDetailData.MaxRainfall := StrToFloat(MaxRainfallEdit.Text);
            MaxRainfallEdit.SetFieldValue(LDDTSDetailData.MaxRainfall);
          end;
          MaxRainfallEdit.ContextValidationError := LErrorMessage;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


procedure TDDTSInputMinMaxValidator.UpdateMaxRunoff;
const OPNAME = 'TDDTSInputMinMaxValidator.UpdateMaxRunoff';
var
  LDDTSDetailData : TDDTSDetailData;
  LErrorMessage: string;
begin
  try
    if DDTSInputMinMaxDialog.MaxRunoffEdit.HasValueChanged then
    begin
      LDDTSDetailData :=  TDDTSDataObject(FAppModules.Model.ModelData).DDTSDamDataList.GetDDTSDetailDataByIdentifier(FIdentifier);
      if (LDDTSDetailData <> nil) then
      begin
        with DDTSInputMinMaxDialog do
        begin
          if (FAppModules.FieldProperties.ValidateFieldProperty(
             MaxRunoffEdit.FieldProperty.FieldName,
              MaxRunoffEdit.Text,LErrorMessage)) then
          begin
            LDDTSDetailData.MaxRunoff := StrToFloat(MaxRunoffEdit.Text);
            MaxRunoffEdit.SetFieldValue(LDDTSDetailData.MaxRunoff);
          end;
          MaxRunoffEdit.ContextValidationError := LErrorMessage;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


procedure TDDTSInputMinMaxValidator.UpdateMinRainfall;
const OPNAME = 'TDDTSInputMinMaxValidator.UpdateMinRainfall';
var
  LDDTSDetailData : TDDTSDetailData;
  LErrorMessage: string;
begin
  try
    if DDTSInputMinMaxDialog.MinRainfallEdit.HasValueChanged then
    begin
      LDDTSDetailData :=  TDDTSDataObject(FAppModules.Model.ModelData).DDTSDamDataList.GetDDTSDetailDataByIdentifier(FIdentifier);
      if (LDDTSDetailData <> nil) then
      begin
        with DDTSInputMinMaxDialog do
        begin
          if (FAppModules.FieldProperties.ValidateFieldProperty(
             MinRainfallEdit.FieldProperty.FieldName,
             MinRainfallEdit.Text,LErrorMessage)) then
          begin
            LDDTSDetailData.MinRainfall := StrToFloat(MinRainfallEdit.Text);
            MinRainfallEdit.SetFieldValue(LDDTSDetailData.MinRainfall);
          end;
          MinRainfallEdit.ContextValidationError := LErrorMessage;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


procedure TDDTSInputMinMaxValidator.UpdateMinRunoff;
const OPNAME = 'TDDTSInputMinMaxValidator.UpdateMinRunoff';
var
  LDDTSDetailData : TDDTSDetailData;
  LErrorMessage: string;
begin
  try
    if DDTSInputMinMaxDialog.MinRunoffEdit.HasValueChanged then
    begin
      LDDTSDetailData :=  TDDTSDataObject(FAppModules.Model.ModelData).DDTSDamDataList.GetDDTSDetailDataByIdentifier(FIdentifier);
      if (LDDTSDetailData <> nil) then
      begin
        with DDTSInputMinMaxDialog do
        begin
          if (FAppModules.FieldProperties.ValidateFieldProperty(
             MinRunoffEdit.FieldProperty.FieldName,
              MinRunoffEdit.Text,LErrorMessage)) then
          begin
            LDDTSDetailData.MinRunoff := StrToFloat(MinRunoffEdit.Text);
            MinRunoffEdit.SetFieldValue(LDDTSDetailData.MinRunoff);
            //DoContextValidation(dvtRunoffScale);
          end;
          MinRunoffEdit.ContextValidationError := LErrorMessage;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;



procedure TDDTSInputMinMaxValidator.UpdateMinEvaporation;
const OPNAME = 'TDDTSInputMinMaxValidator.UpdateMinEvaporation';
var
  LDDTSDetailData : TDDTSDetailData;
  LErrorMessage: string;
begin
  try
    if DDTSInputMinMaxDialog.MinEvaporationEdit.HasValueChanged then
    begin
      LDDTSDetailData :=  TDDTSDataObject(FAppModules.Model.ModelData).DDTSDamDataList.GetDDTSDetailDataByIdentifier(FIdentifier);
      if (LDDTSDetailData <> nil) then
      begin
        with DDTSInputMinMaxDialog do
        begin
          if (FAppModules.FieldProperties.ValidateFieldProperty(
             MinEvaporationEdit.FieldProperty.FieldName,
              MinEvaporationEdit.Text,LErrorMessage)) then
          begin
            LDDTSDetailData.MinEvaporation := StrToFloat(MinEvaporationEdit.Text);
            MinEvaporationEdit.SetFieldValue(LDDTSDetailData.MinEvaporation);
          end;
          MinEvaporationEdit.ContextValidationError := LErrorMessage;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDDTSInputMinMaxValidator.UpdateMinEWR;
const OPNAME = 'TDDTSInputMinMaxValidator.UpdateMinEWR';
var
  LDDTSDetailData : TDDTSDetailData;
  LErrorMessage: string;
begin
  try
    if DDTSInputMinMaxDialog.MinEWREdit.HasValueChanged then
    begin
      LDDTSDetailData :=  TDDTSDataObject(FAppModules.Model.ModelData).DDTSDamDataList.GetDDTSDetailDataByIdentifier(FIdentifier);
      if (LDDTSDetailData <> nil) then
      begin
        with DDTSInputMinMaxDialog do
        begin
          if (FAppModules.FieldProperties.ValidateFieldProperty(
             MinEWREdit.FieldProperty.FieldName,
             MinEWREdit.Text,LErrorMessage)) then
          begin
            LDDTSDetailData.MinEWR := StrToFloat(MinEWREdit.Text);
            MinEWREdit.SetFieldValue(LDDTSDetailData.MinEWR);
          end;
          MinEWREdit.ContextValidationError := LErrorMessage;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


procedure TDDTSInputMinMaxValidator.UpdateMinIncreamentalRunoff;
const OPNAME = 'TDDTSInputMinMaxValidator.UpdateMinIncreamentalRunoff';
var
  LDDTSDetailData : TDDTSDetailData;
  LErrorMessage: string;
begin
  try
    if DDTSInputMinMaxDialog.MinIncreamentalRunoffEdit.HasValueChanged then
    begin
      LDDTSDetailData :=  TDDTSDataObject(FAppModules.Model.ModelData).DDTSDamDataList.GetDDTSDetailDataByIdentifier(FIdentifier);
      if (LDDTSDetailData <> nil) then
      begin
        with DDTSInputMinMaxDialog do
        begin
          if (FAppModules.FieldProperties.ValidateFieldProperty(
             MinIncreamentalRunoffEdit.FieldProperty.FieldName,
             MinIncreamentalRunoffEdit.Text,LErrorMessage)) then
          begin
            LDDTSDetailData.MinIncreamentalRunoff := StrToFloat(MinIncreamentalRunoffEdit.Text);
            MinIncreamentalRunoffEdit.SetFieldValue(LDDTSDetailData.MinIncreamentalRunoff);
          end;
          MinIncreamentalRunoffEdit.ContextValidationError := LErrorMessage;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


procedure TDDTSInputMinMaxValidator.UpdateMinOtherInflow;
const OPNAME = 'TDDTSInputMinMaxValidator.UpdateMinOtherInflow';
var
  LDDTSDetailData : TDDTSDetailData;
  LErrorMessage: string;
begin
  try
    if DDTSInputMinMaxDialog.MinOtherInflowEdit.HasValueChanged then
    begin
      LDDTSDetailData :=  TDDTSDataObject(FAppModules.Model.ModelData).DDTSDamDataList.GetDDTSDetailDataByIdentifier(FIdentifier);
      if (LDDTSDetailData <> nil) then
      begin
        with DDTSInputMinMaxDialog do
        begin
          if (FAppModules.FieldProperties.ValidateFieldProperty(
             MinOtherInflowEdit.FieldProperty.FieldName,
             MinOtherInflowEdit.Text,LErrorMessage)) then
          begin
            LDDTSDetailData.MinOtherInflow := StrToFloat(MinOtherInflowEdit.Text);
            MinOtherInflowEdit.SetFieldValue(LDDTSDetailData.MinOtherInflow);
          end;
          MinOtherInflowEdit.ContextValidationError := LErrorMessage;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;
  (*
procedure TDDTSInputMinMaxValidator.UpdateEWRPercRelease;
const OPNAME = 'TDDTSInputMinMaxValidator.UpdateEWRPercRelease';
var
  LDDTSDetailData : TDDTSDetailData;
  LErrorMessage: string;
begin
  try
    if DDTSInputMinMaxDialog.EWRPercReleaseEdit.HasValueChanged then
    begin
      LDDTSDetailData :=  TDDTSDataObject(FAppModules.Model.ModelData).DDTSDamDataList.GetDDTSDetailDataByIdentifier(FIdentifier);
      if (LDDTSDetailData <> nil) then
      begin
        with DDTSInputMinMaxDialog do
        begin
          if (FAppModules.FieldProperties.ValidateFieldProperty(
             EWRPercReleaseEdit.FieldProperty.FieldName,
              EWRPercReleaseEdit.Text,LErrorMessage)) then
          begin
            LDDTSDetailData.EWRPercRelease := StrToFloat(EWRPercReleaseEdit.Text);
            EWRPercReleaseEdit.SetFieldValue(LDDTSDetailData.EWRPercRelease);
            //DoContextValidation(dvtRunoffScale);
          end;
          EWRPercReleaseEdit.ContextValidationError := LErrorMessage;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDDTSInputMinMaxValidator.UpdateEWRScale;
const OPNAME = 'TDDTSInputMinMaxValidator.UpdateEWRScale';
var
  LDDTSDetailData : TDDTSDetailData;
  LErrorMessage: string;
begin
  try
    if DDTSInputMinMaxDialog.EWRScaleFactorEdit.HasValueChanged then
    begin
      LDDTSDetailData :=  TDDTSDataObject(FAppModules.Model.ModelData).DDTSDamDataList.GetDDTSDetailDataByIdentifier(FIdentifier);
      if (LDDTSDetailData <> nil) then
      begin
        with DDTSInputMinMaxDialog do
        begin
          if (FAppModules.FieldProperties.ValidateFieldProperty(
              EWRScaleFactorEdit.FieldProperty.FieldName,
              EWRScaleFactorEdit.Text,LErrorMessage)) then
          begin
            LDDTSDetailData.EWRScale := StrToFloat(EWRScaleFactorEdit.Text);
            EWRScaleFactorEdit.SetFieldValue(LDDTSDetailData.EWRScale);
            //DoContextValidation(dvtRunoffScale);
          end;
          EWRScaleFactorEdit.ContextValidationError := LErrorMessage;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDDTSInputMinMaxValidator.UpdateOtherInflowScale;
const OPNAME = 'TDDTSInputMinMaxValidator.UpdateOtherInflowScale';
var
  LDDTSDetailData : TDDTSDetailData;
  LErrorMessage: string;
begin
  try
    if DDTSInputMinMaxDialog.OtherInflowScaleFactorEdit.HasValueChanged then
    begin
      LDDTSDetailData :=  TDDTSDataObject(FAppModules.Model.ModelData).DDTSDamDataList.GetDDTSDetailDataByIdentifier(FIdentifier);
      if (LDDTSDetailData <> nil) then
      begin
        with DDTSInputMinMaxDialog do
        begin
          if (FAppModules.FieldProperties.ValidateFieldProperty(
              OtherInflowScaleFactorEdit.FieldProperty.FieldName,
              OtherInflowScaleFactorEdit.Text,LErrorMessage)) then
          begin
            LDDTSDetailData.OtherInflowScale := StrToFloat(OtherInflowScaleFactorEdit.Text);
            OtherInflowScaleFactorEdit.SetFieldValue(LDDTSDetailData.OtherInflowScale);
            //DoContextValidation(dvtRunoffScale);
          end;
          OtherInflowScaleFactorEdit.ContextValidationError := LErrorMessage;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


procedure TDDTSInputMinMaxValidator.UpdateRunoffScaleFactor;
const OPNAME = 'TDDTSInputMinMaxValidator.UpdateRunoffScaleFactor';
var
  LDDTSDetailData : TDDTSDetailData;
  LErrorMessage: string;
begin
  try
    if DDTSInputMinMaxDialog.RunoffScaleFactorEdit.HasValueChanged then
    begin
      LDDTSDetailData :=  TDDTSDataObject(FAppModules.Model.ModelData).DDTSDamDataList.GetDDTSDetailDataByIdentifier(FIdentifier);
      if (LDDTSDetailData <> nil) then
      begin
        with DDTSInputMinMaxDialog do
        begin
          if (FAppModules.FieldProperties.ValidateFieldProperty(
              RunoffScaleFactorEdit.FieldProperty.FieldName,
              RunoffScaleFactorEdit.Text,LErrorMessage)) then
          begin
            LDDTSDetailData.RunoffScale := StrToFloat(RunoffScaleFactorEdit.Text);
            RunoffScaleFactorEdit.SetFieldValue(LDDTSDetailData.RunoffScale);
            //DoContextValidation(dvtRunoffScale);
          end;
          RunoffScaleFactorEdit.ContextValidationError := LErrorMessage;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDDTSInputMinMaxValidator.UpdateSpillPercRelease;
const OPNAME = 'TDDTSInputMinMaxValidator.UpdateSpillPercRelease';
var
  LDDTSDetailData : TDDTSDetailData;
  LErrorMessage: string;
begin
  try
    if DDTSInputMinMaxDialog.SpillPercReleaseEdit.HasValueChanged then
    begin
      LDDTSDetailData :=  TDDTSDataObject(FAppModules.Model.ModelData).DDTSDamDataList.GetDDTSDetailDataByIdentifier(FIdentifier);
      if (LDDTSDetailData <> nil) then
      begin
        with DDTSInputMinMaxDialog do
        begin
          if (FAppModules.FieldProperties.ValidateFieldProperty(
             SpillPercReleaseEdit.FieldProperty.FieldName,
              SpillPercReleaseEdit.Text,LErrorMessage)) then
          begin
            LDDTSDetailData.SpillPercRelease := StrToFloat(SpillPercReleaseEdit.Text);
            SpillPercReleaseEdit.SetFieldValue(LDDTSDetailData.SpillPercRelease);
            //DoContextValidation(dvtRunoffScale);
          end;
          SpillPercReleaseEdit.ContextValidationError := LErrorMessage;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDDTSInputMinMaxValidator.UpdateTargetDraft;
const OPNAME = 'TDDTSInputMinMaxValidator.UpdateTargetDraft';
var
  LDDTSDetailData : TDDTSDetailData;
  LErrorMessage: string;
begin
  try
    if DDTSInputMinMaxDialog.TargetDraftEdit.HasValueChanged then
    begin
      LDDTSDetailData :=  TDDTSDataObject(FAppModules.Model.ModelData).DDTSDamDataList.GetDDTSDetailDataByIdentifier(FIdentifier);
      if (LDDTSDetailData <> nil) then
      begin
        with DDTSInputMinMaxDialog do
        begin
          if (FAppModules.FieldProperties.ValidateFieldProperty(
             TargetDraftEdit.FieldProperty.FieldName,
              TargetDraftEdit.Text,LErrorMessage)) then
          begin
            LDDTSDetailData.TargetDraft := StrToFloat(TargetDraftEdit.Text);
            TargetDraftEdit.SetFieldValue(LDDTSDetailData.TargetDraft);
            //DoContextValidation(dvtRunoffScale);
          end;
          TargetDraftEdit.ContextValidationError := LErrorMessage;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;
*)

procedure TDDTSInputMinMaxValidator.PopulateDataViewer;
const OPNAME = 'TDDTSInputMinMaxValidator.PopulateDataViewer';
begin
  inherited PopulateDataViewer;
  try
    ClearDataViewer;
    RePopulateDataViewer;
    DoContextValidation(dvtResPropAll);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDDTSInputMinMaxValidator.CurrentReservoir: IReservoirData;
const OPNAME = 'TDDTSInputMinMaxValidator.CurrentReservoir';
begin
  Result := nil;
  try
    Result := TDDTSDataObject(FAppModules.Model.ModelData).DDTSDamDataList.CastReservoirList.ReservoirByIdentifier[FIdentifier];

  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDDTSInputMinMaxValidator.DoContextValidation(AValidationType: TDialogValidationType);
const OPNAME = 'TDDTSInputMinMaxValidator.DoContextValidation';
var
  LReservoir     : IReservoirData;
begin
  try
    FAllErrorMessages.Clear;
    if (FIdentifier >= 0) then
    begin
      try
        LReservoir := TDDTSDataObject(FAppModules.Model.ModelData).DDTSDamDataList.CastReservoirList.ReservoirByIdentifier[FIdentifier];

        if (lReservoir <> nil) then
        begin
           if (AValidationType in [dvtResPropAll, dvtResPropReservoirName]) then
             // ValidateReservoirName(LReservoir);

        end;
      finally
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;



end.


