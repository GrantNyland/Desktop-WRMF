//
//
//  UNIT      : Contains the class TDDTSDamConfigValidator.
//  AUTHOR    : Sam Dhlamini
//  DATE      : 2014/07/17
//  COPYRIGHT : Copyright © 2003 DWAF
//
//
unit UDDTSDamConfigValidator;

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
  UDDTSDamConfigDialog,
  UDDTSData,
  UAbstractYieldDataDialogValidator,
  UYieldContextValidationType;
type
  TDDTSDamConfigValidator = class(TAbstractYieldDataDialogValidator)
  protected
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    procedure OnEditControlEnter(Sender: TObject); override;
    procedure OnEditControltExit(Sender: TObject); override;
    procedure OnImportHeadingClick(Sender: TObject);



    procedure UpdateRunoffScaleFactor;
    procedure UpdateOtherInflowScale;
    procedure UpdateEWRScale;
    procedure UpdateTargetDraft;
    procedure UpdateDSRequiments;
    procedure UpdateDSPercRelease;
    procedure UpdateSpillPercRelease;
    procedure UpdateEWRPercRelease;
    procedure UpdateImportHeading;

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

    function DDTSDamConfigDialog: TDDTSDamConfigDialog;
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

{ TDDTSDamConfigValidator }

procedure TDDTSDamConfigValidator.CreateMemberObjects;
const OPNAME = 'TDDTSDamConfigValidator.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
    FIdentifier := -1;
    FPanel := TDDTSDamConfigDialog.Create(FPanelOwner,FAppModules);

    DDTSDamConfigDialog.RunoffScaleFactorEdit.FieldProperty := FAppModules.FieldProperties.FieldProperty('RunoffScaleFactor');
    DDTSDamConfigDialog.RunoffScaleFactorEdit.OnEnter       := OnEditControlEnter;
    DDTSDamConfigDialog.RunoffScaleFactorEdit.OnExit        := OnEditControltExit;

    DDTSDamConfigDialog.OtherInflowScaleFactorEdit.FieldProperty := FAppModules.FieldProperties.FieldProperty('OtherInflowScaleFactor');
    DDTSDamConfigDialog.OtherInflowScaleFactorEdit.OnEnter       := OnEditControlEnter;
    DDTSDamConfigDialog.OtherInflowScaleFactorEdit.OnExit        := OnEditControltExit;

    DDTSDamConfigDialog.EWRScaleFactorEdit.FieldProperty := FAppModules.FieldProperties.FieldProperty('EWRScaleFactor');
    DDTSDamConfigDialog.EWRScaleFactorEdit.OnEnter       := OnEditControlEnter;
    DDTSDamConfigDialog.EWRScaleFactorEdit.OnExit        := OnEditControltExit;

    DDTSDamConfigDialog.TargetDraftEdit.FieldProperty := FAppModules.FieldProperties.FieldProperty('TargetDraft');
    DDTSDamConfigDialog.TargetDraftEdit.OnEnter       := OnEditControlEnter;
    DDTSDamConfigDialog.TargetDraftEdit.OnExit        := OnEditControltExit;

    DDTSDamConfigDialog.DSRequirmentEdit.FieldProperty := FAppModules.FieldProperties.FieldProperty('DSRequirment');
    DDTSDamConfigDialog.DSRequirmentEdit.OnEnter       := OnEditControlEnter;
    DDTSDamConfigDialog.DSRequirmentEdit.OnExit        := OnEditControltExit;

    DDTSDamConfigDialog.DSPercReleaseEdit.FieldProperty := FAppModules.FieldProperties.FieldProperty('DSPercRelease');
    DDTSDamConfigDialog.DSPercReleaseEdit.OnEnter       := OnEditControlEnter;
    DDTSDamConfigDialog.DSPercReleaseEdit.OnExit        := OnEditControltExit;


    DDTSDamConfigDialog.SpillPercReleaseEdit.FieldProperty := FAppModules.FieldProperties.FieldProperty('SpillPercRelease');
    DDTSDamConfigDialog.SpillPercReleaseEdit.OnEnter       := OnEditControlEnter;
    DDTSDamConfigDialog.SpillPercReleaseEdit.OnExit        := OnEditControltExit;

    DDTSDamConfigDialog.EWRPercReleaseEdit.FieldProperty := FAppModules.FieldProperties.FieldProperty('EWRPercRelease');
    DDTSDamConfigDialog.EWRPercReleaseEdit.OnEnter       := OnEditControlEnter;
    DDTSDamConfigDialog.EWRPercReleaseEdit.OnExit        := OnEditControltExit;

    DDTSDamConfigDialog.ImportHeadingChkBox.FieldProperty := FAppModules.FieldProperties.FieldProperty('ImportHeadlines');
    DDTSDamConfigDialog.ImportHeadingChkBox.OnEnter       := OnEditControlEnter;
    DDTSDamConfigDialog.ImportHeadingChkBox.OnClick       := OnImportHeadingClick;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDDTSDamConfigValidator.DestroyMemberObjects;
const OPNAME = 'TDDTSDamConfigValidator.DestroyMemberObjects';
begin
  try
    inherited DestroyMemberObjects;

  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDDTSDamConfigValidator.Initialise: boolean;
const OPNAME = 'TDDTSDamConfigValidator.Initialise';
begin
  Result := inherited Initialise;
  try
  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDDTSDamConfigValidator.LanguageHasChanged: boolean;
const OPNAME = 'TDDTSDamConfigValidator.LanguageHasChanged';
begin
  Result := False;
  try
    TabShetCaption := 'Run Configuration';
    Result := inherited LanguageHasChanged;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDDTSDamConfigValidator.ClearDataViewer;
const OPNAME = 'TDDTSDamConfigValidator.ClearDataViewer';
begin
  inherited ClearDataViewer;
  try

  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDDTSDamConfigValidator.RePopulateDataViewer;
const OPNAME = 'TDDTSDamConfigValidator.RePopulateDataViewer';
var
  LDDTSDetailData : TDDTSDetailData;
begin
  try
    if(FIdentifier >= 0) then
    begin
      LDDTSDetailData :=   TDDTSDataObject(FAppModules.Model.ModelData).DDTSDamDataList.GetDDTSDetailDataByIdentifier(FIdentifier);
      if (LDDTSDetailData <> nil) then
      begin
        with DDTSDamConfigDialog do
        begin
          RunoffScaleFactorEdit.SetFieldValue(LDDTSDetailData.RunoffScale);
          OtherInflowScaleFactorEdit.SetFieldValue(LDDTSDetailData.OtherInflowScale);
          EWRScaleFactorEdit.SetFieldValue(LDDTSDetailData.EWRScale);
          TargetDraftEdit.SetFieldValue(LDDTSDetailData.TargetDraft);
          DSRequirmentEdit.SetFieldValue(LDDTSDetailData.DSRequiments);
          DSPercReleaseEdit.SetFieldValue(LDDTSDetailData.DSPercRelease);
          SpillPercReleaseEdit.SetFieldValue(LDDTSDetailData.SpillPercRelease);
          EWRPercReleaseEdit.SetFieldValue(LDDTSDetailData.EWRPercRelease);
          ImportHeadingChkBox.Checked := (LDDTSDetailData.ImportHeading = 'Y');

        end;
        DoContextValidation(dvtResPropAll);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDDTSDamConfigValidator.SaveState: boolean;
const OPNAME = 'TDDTSDamConfigValidator.SaveState';
begin
  Result := inherited SaveState;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDDTSDamConfigValidator.OnEditControlEnter(Sender: TObject);
const OPNAME = 'TDDTSDamConfigValidator.OnEditControlEnter';
begin
  inherited OnEditControlEnter(Sender);
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDDTSDamConfigValidator.OnEditControltExit(Sender: TObject);
const OPNAME = 'TDDTSDamConfigValidator.OnEditControltExit';
begin
  inherited OnEditControltExit(Sender);
  try
    if ((sender = DDTSDamConfigDialog.RunoffScaleFactorEdit) AND
       (DDTSDamConfigDialog.RunoffScaleFactorEdit.HasValueChanged)) then
       UpdateRunoffScaleFactor

    else
    if ((sender = DDTSDamConfigDialog.OtherInflowScaleFactorEdit) AND
       (DDTSDamConfigDialog.OtherInflowScaleFactorEdit.HasValueChanged)) then
       UpdateOtherInflowScale
    else
    if ((sender = DDTSDamConfigDialog.EWRScaleFactorEdit) AND
       (DDTSDamConfigDialog.EWRScaleFactorEdit.HasValueChanged)) then
       UpdateEWRScale
    else
    if ((sender = DDTSDamConfigDialog.TargetDraftEdit) AND
       (DDTSDamConfigDialog.TargetDraftEdit.HasValueChanged)) then
       UpdateTargetDraft
    else
    if ((sender = DDTSDamConfigDialog.DSRequirmentEdit) AND
       (DDTSDamConfigDialog.DSRequirmentEdit.HasValueChanged)) then
       UpdateDSRequiments
    else
    if ((sender = DDTSDamConfigDialog.DSPercReleaseEdit) AND
       (DDTSDamConfigDialog.DSPercReleaseEdit.HasValueChanged)) then
       UpdateDSPercRelease
    else
    if ((sender = DDTSDamConfigDialog.SpillPercReleaseEdit) AND
       (DDTSDamConfigDialog.SpillPercReleaseEdit.HasValueChanged)) then
       UpdateSpillPercRelease
    else
    if ((sender = DDTSDamConfigDialog.EWRPercReleaseEdit) AND
       (DDTSDamConfigDialog.EWRPercReleaseEdit.HasValueChanged)) then
       UpdateEWRPercRelease;

      RePopulateDataViewer;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDDTSDamConfigValidator.OnImportHeadingClick(Sender: TObject);
const OPNAME = 'TDDTSDamConfigValidator.OnImportHeadingClick';
begin
  try
    if(DDTSDamConfigDialog.ImportHeadingChkBox.HasValueChanged) then
      UpdateImportHeading;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDDTSDamConfigValidator.DDTSDamConfigDialog:TDDTSDamConfigDialog;
const OPNAME = 'TDDTSDamConfigValidator.DDTSDamConfigDialog';
begin
  Result := Nil;
  try
    if Assigned(FPanel) then
      Result := TDDTSDamConfigDialog(FPanel);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDDTSDamConfigValidator.StudyDataHasChanged(AContext: TChangeContext; AFieldName,AOldValue,ANewValue: string): boolean;
const OPNAME = 'TDDTSDamConfigValidator.StudyDataHasChanged';
begin
  Result := inherited StudyDataHasChanged(AContext,AFieldName,AOldValue,ANewValue);
  try

      RePopulateDataViewer;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDDTSDamConfigValidator.StudyHasChanged: boolean;
const OPNAME = 'TDDTSDamConfigValidator.StudyHasChanged';
begin
  Result := inherited StudyHasChanged;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDDTSDamConfigValidator.UpdateDSPercRelease;
const OPNAME = 'TDDTSDamConfigValidator.UpdateDSPercRelease';
var
  LDDTSDetailData : TDDTSDetailData;
  LErrorMessage: string;
begin
  try
    if DDTSDamConfigDialog.DSPercReleaseEdit.HasValueChanged then
    begin
      LDDTSDetailData :=  TDDTSDataObject(FAppModules.Model.ModelData).DDTSDamDataList.GetDDTSDetailDataByIdentifier(FIdentifier);
      if (LDDTSDetailData <> nil) then
      begin
        with DDTSDamConfigDialog do
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

procedure TDDTSDamConfigValidator.UpdateDSRequiments;
const OPNAME = 'TDDTSDamConfigValidator.UpdateDSRequiments';
var
  LDDTSDetailData : TDDTSDetailData;
  LErrorMessage: string;
begin
  try
    if DDTSDamConfigDialog.DSRequirmentEdit.HasValueChanged then
    begin
      LDDTSDetailData :=  TDDTSDataObject(FAppModules.Model.ModelData).DDTSDamDataList.GetDDTSDetailDataByIdentifier(FIdentifier);
      if (LDDTSDetailData <> nil) then
      begin
        with DDTSDamConfigDialog do
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

procedure TDDTSDamConfigValidator.UpdateEWRPercRelease;
const OPNAME = 'TDDTSDamConfigValidator.UpdateEWRPercRelease';
var
  LDDTSDetailData : TDDTSDetailData;
  LErrorMessage: string;
begin
  try
    if DDTSDamConfigDialog.EWRPercReleaseEdit.HasValueChanged then
    begin
      LDDTSDetailData :=  TDDTSDataObject(FAppModules.Model.ModelData).DDTSDamDataList.GetDDTSDetailDataByIdentifier(FIdentifier);
      if (LDDTSDetailData <> nil) then
      begin
        with DDTSDamConfigDialog do
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

procedure TDDTSDamConfigValidator.UpdateEWRScale;
const OPNAME = 'TDDTSDamConfigValidator.UpdateEWRScale';
var
  LDDTSDetailData : TDDTSDetailData;
  LErrorMessage: string;
begin
  try
    if DDTSDamConfigDialog.EWRScaleFactorEdit.HasValueChanged then
    begin
      LDDTSDetailData :=  TDDTSDataObject(FAppModules.Model.ModelData).DDTSDamDataList.GetDDTSDetailDataByIdentifier(FIdentifier);
      if (LDDTSDetailData <> nil) then
      begin
        with DDTSDamConfigDialog do
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

procedure TDDTSDamConfigValidator.UpdateImportHeading;
const OPNAME = 'TDDTSDamConfigValidator.UpdateOtherInflowScale';
var
  LDDTSDetailData : TDDTSDetailData;
  LErrorMessage: string;
  LOldValue : string;
  LNewValue : string;
begin
  try
    if DDTSDamConfigDialog.ImportHeadingChkBox.HasValueChanged then
    begin
      LDDTSDetailData :=  TDDTSDataObject(FAppModules.Model.ModelData).DDTSDamDataList.GetDDTSDetailDataByIdentifier(FIdentifier);
      if (LDDTSDetailData <> nil) then
      begin
        with DDTSDamConfigDialog do
        begin
          LOldValue := UpperCase(Trim(LDDTSDetailData.ImportHeading));
          if ImportHeadingChkBox.Checked then
            LNewValue := 'Y'
          else
            LNewValue := 'N';
          if (LOldValue <> LNewvalue) then
          begin
            if (FAppModules.FieldProperties.ValidateFieldProperty(
                ImportHeadingChkBox.FieldProperty.FieldName,
                LNewValue,LErrorMessage)) then
            begin
              LDDTSDetailData.ImportHeading := LNewValue;
              ImportHeadingChkBox.Checked  := (LDDTSDetailData.ImportHeading = 'Y');
            end;
            ImportHeadingChkBox.ValidationError := LErrorMessage;
          end;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


procedure TDDTSDamConfigValidator.UpdateOtherInflowScale;
const OPNAME = 'TDDTSDamConfigValidator.UpdateOtherInflowScale';
var
  LDDTSDetailData : TDDTSDetailData;
  LErrorMessage: string;
begin
  try
    if DDTSDamConfigDialog.OtherInflowScaleFactorEdit.HasValueChanged then
    begin
      LDDTSDetailData :=  TDDTSDataObject(FAppModules.Model.ModelData).DDTSDamDataList.GetDDTSDetailDataByIdentifier(FIdentifier);
      if (LDDTSDetailData <> nil) then
      begin
        with DDTSDamConfigDialog do
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


procedure TDDTSDamConfigValidator.UpdateRunoffScaleFactor;
const OPNAME = 'TDDTSDamConfigValidator.UpdateRunoffScaleFactor';
var
  LDDTSDetailData : TDDTSDetailData;
  LErrorMessage: string;
begin
  try
    if DDTSDamConfigDialog.RunoffScaleFactorEdit.HasValueChanged then
    begin
      LDDTSDetailData :=  TDDTSDataObject(FAppModules.Model.ModelData).DDTSDamDataList.GetDDTSDetailDataByIdentifier(FIdentifier);
      if (LDDTSDetailData <> nil) then
      begin
        with DDTSDamConfigDialog do
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

procedure TDDTSDamConfigValidator.UpdateSpillPercRelease;
const OPNAME = 'TDDTSDamConfigValidator.UpdateSpillPercRelease';
var
  LDDTSDetailData : TDDTSDetailData;
  LErrorMessage: string;
begin
  try
    if DDTSDamConfigDialog.SpillPercReleaseEdit.HasValueChanged then
    begin
      LDDTSDetailData :=  TDDTSDataObject(FAppModules.Model.ModelData).DDTSDamDataList.GetDDTSDetailDataByIdentifier(FIdentifier);
      if (LDDTSDetailData <> nil) then
      begin
        with DDTSDamConfigDialog do
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

procedure TDDTSDamConfigValidator.UpdateTargetDraft;
const OPNAME = 'TDDTSDamConfigValidator.UpdateTargetDraft';
var
  LDDTSDetailData : TDDTSDetailData;
  LErrorMessage: string;
begin
  try
    if DDTSDamConfigDialog.TargetDraftEdit.HasValueChanged then
    begin
      LDDTSDetailData :=  TDDTSDataObject(FAppModules.Model.ModelData).DDTSDamDataList.GetDDTSDetailDataByIdentifier(FIdentifier);
      if (LDDTSDetailData <> nil) then
      begin
        with DDTSDamConfigDialog do
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


procedure TDDTSDamConfigValidator.PopulateDataViewer;
const OPNAME = 'TDDTSDamConfigValidator.PopulateDataViewer';
begin
  inherited PopulateDataViewer;
  try
    ClearDataViewer;
    RePopulateDataViewer;
    DoContextValidation(dvtResPropAll);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDDTSDamConfigValidator.CurrentReservoir: IReservoirData;
const OPNAME = 'TDDTSDamConfigValidator.CurrentReservoir';
begin
  Result := nil;
  try
    Result := TDDTSDataObject(FAppModules.Model.ModelData).DDTSDamDataList.CastReservoirList.ReservoirByIdentifier[FIdentifier];

  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDDTSDamConfigValidator.DoContextValidation(AValidationType: TDialogValidationType);
const OPNAME = 'TDDTSDamConfigValidator.DoContextValidation';
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


