//
//
//  UNIT      : Contains the class TDDTSDamPropertiesValidator.
//  AUTHOR    : Sam Dhlamini
//  DATE      : 2014/07/17
//  COPYRIGHT : Copyright © 2003 DWAF
//
//
unit UDDTSDamPropertiesValidator;

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
  UDDTSDamPropertiesDialog,
  UDDTSData,
  UAbstractYieldDataDialogValidator,
  UYieldContextValidationType;
type
  TDDTSDamPropertiesValidator = class(TAbstractYieldDataDialogValidator)
  protected
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    procedure OnEditControlEnter(Sender: TObject); override;
    procedure OnEditControltExit(Sender: TObject); override;
    procedure UpdateResevoirName;
    procedure UpdateXCoord;
    procedure UpdateYCoord;

    procedure RePopulateDataViewer;
    function  CurrentReservoir:IReservoirData;
    procedure ValidateReservoirName(AReservoir : IReservoirData);
  public
    function Initialise: boolean; override;
    function SaveState: boolean; override;
    function LanguageHasChanged: boolean; override;
    function StudyHasChanged: boolean; override;
    function StudyDataHasChanged(AContext: TChangeContext; AFieldName,AOldValue,ANewValue: string): boolean; override;
    procedure ClearDataViewer; override;
    procedure PopulateDataViewer; override;

    function DDTSDamPropertiesDialog: TDDTSDamPropertiesDialog;
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

{ TDDTSDamPropertiesValidator }

procedure TDDTSDamPropertiesValidator.CreateMemberObjects;
const OPNAME = 'TDDTSDamPropertiesValidator.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
    FIdentifier := -1;
    FPanel := TDDTSDamPropertiesDialog.Create(FPanelOwner,FAppModules);

    DDTSDamPropertiesDialog.ReservoirNameEdit.EditIdentifier   := giReservoirName;
    DDTSDamPropertiesDialog.ReservoirNameEdit.FieldProperty    := FAppModules.FieldProperties.FieldProperty('ReservoirName');
    DDTSDamPropertiesDialog.ReservoirNameEdit.OnEnter          := OnEditControlEnter;
    DDTSDamPropertiesDialog.ReservoirNameEdit.OnExit           := OnEditControltExit;

    DDTSDamPropertiesDialog.ReservoirXCoordEdit.FieldProperty := FAppModules.FieldProperties.FieldProperty('XCoord');
    DDTSDamPropertiesDialog.ReservoirXCoordEdit.OnEnter       := OnEditControlEnter;
    DDTSDamPropertiesDialog.ReservoirXCoordEdit.OnExit        := OnEditControltExit;

    DDTSDamPropertiesDialog.ReservoirYCoordEdit.FieldProperty := FAppModules.FieldProperties.FieldProperty('YCoord');
    DDTSDamPropertiesDialog.ReservoirYCoordEdit.OnEnter       := OnEditControlEnter;
    DDTSDamPropertiesDialog.ReservoirYCoordEdit.OnExit        := OnEditControltExit;




  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDDTSDamPropertiesValidator.DestroyMemberObjects;
const OPNAME = 'TDDTSDamPropertiesValidator.DestroyMemberObjects';
begin
  try
    inherited DestroyMemberObjects;

  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDDTSDamPropertiesValidator.Initialise: boolean;
const OPNAME = 'TDDTSDamPropertiesValidator.Initialise';
begin
  Result := inherited Initialise;
  try
  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDDTSDamPropertiesValidator.LanguageHasChanged: boolean;
const OPNAME = 'TDDTSDamPropertiesValidator.LanguageHasChanged';
begin
  Result := False;
  try
    TabShetCaption := 'Properties';
    Result := inherited LanguageHasChanged;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDDTSDamPropertiesValidator.ClearDataViewer;
const OPNAME = 'TDDTSDamPropertiesValidator.ClearDataViewer';
begin
  inherited ClearDataViewer;
  try
    DDTSDamPropertiesDialog.ReservoirNameEdit.SetFieldValue('');

  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDDTSDamPropertiesValidator.RePopulateDataViewer;
const OPNAME = 'TDDTSDamPropertiesValidator.RePopulateDataViewer';
var
  LReservoirObject : IReservoirData;
begin
  try
    if(FIdentifier >= 0) then
    begin
      LReservoirObject :=  TDDTSDataObject(FAppModules.Model.ModelData).DDTSDamDataList.CastReservoirList.ReservoirByIdentifier[FIdentifier];
      if (LReservoirObject <> nil) then
      begin
        with DDTSDamPropertiesDialog do
        begin
          ReservoirNameEdit.SetFieldValue(LReservoirObject.ReservoirConfigurationData.ReservoirName);
          ReservoirXCoordEdit.SetFieldValue(LReservoirObject.ReservoirConfigurationData.XCoord);
          ReservoirYCoordEdit.SetFieldValue(LReservoirObject.ReservoirConfigurationData.YCoord);

        end;
        DoContextValidation(dvtResPropAll);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDDTSDamPropertiesValidator.SaveState: boolean;
const OPNAME = 'TDDTSDamPropertiesValidator.SaveState';
begin
  Result := inherited SaveState;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDDTSDamPropertiesValidator.OnEditControlEnter(Sender: TObject);
const OPNAME = 'TDDTSDamPropertiesValidator.OnEditControlEnter';
begin
  inherited OnEditControlEnter(Sender);
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDDTSDamPropertiesValidator.OnEditControltExit(Sender: TObject);
const OPNAME = 'TDDTSDamPropertiesValidator.OnEditControltExit';
begin
  inherited OnEditControltExit(Sender);
  try
    if ((Sender =  DDTSDamPropertiesDialog.ReservoirNameEdit) AND
       (DDTSDamPropertiesDialog.ReservoirNameEdit.HasValueChanged)) then
       UpdateResevoirName
    else
    if ((sender = DDTSDamPropertiesDialog.ReservoirXCoordEdit) AND
       (DDTSDamPropertiesDialog.ReservoirXCoordEdit.HasValueChanged)) then
       UpdateXCoord
    else
    if ((sender = DDTSDamPropertiesDialog.ReservoirYCoordEdit) AND
       (DDTSDamPropertiesDialog.ReservoirYCoordEdit.HasValueChanged)) then
       UpdateYCoord;
      RePopulateDataViewer;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDDTSDamPropertiesValidator.DDTSDamPropertiesDialog:TDDTSDamPropertiesDialog;
const OPNAME = 'TDDTSDamPropertiesValidator.DDTSDamPropertiesDialog';
begin
  Result := Nil;
  try
    if Assigned(FPanel) then
      Result := TDDTSDamPropertiesDialog(FPanel);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDDTSDamPropertiesValidator.StudyDataHasChanged(AContext: TChangeContext; AFieldName,AOldValue,ANewValue: string): boolean;
const OPNAME = 'TDDTSDamPropertiesValidator.StudyDataHasChanged';
begin
  Result := inherited StudyDataHasChanged(AContext,AFieldName,AOldValue,ANewValue);
  try
   if (AFieldName = 'ReservoirName') and
      (AOldValue = DDTSDamPropertiesDialog.ReservoirNameEdit.Text) then
      RePopulateDataViewer;

      RePopulateDataViewer;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDDTSDamPropertiesValidator.StudyHasChanged: boolean;
const OPNAME = 'TDDTSDamPropertiesValidator.StudyHasChanged';
begin
  Result := inherited StudyHasChanged;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDDTSDamPropertiesValidator.UpdateResevoirName;
const OPNAME = 'TDDTSDamPropertiesValidator.UpdateResevoirName';
var
  LReservoirObject: IReservoirData;
  LErrorMessage: string;
begin
  try
    if DDTSDamPropertiesDialog.ReservoirNameEdit.HasValueChanged then
    begin
      LReservoirObject :=  TDDTSDataObject(FAppModules.Model.ModelData).DDTSDamDataList.CastReservoirList.ReservoirByIdentifier[FIdentifier];
      if (LReservoirObject <> nil) then
      begin
        with DDTSDamPropertiesDialog do
        begin
          if (FAppModules.FieldProperties.ValidateFieldProperty(
              ReservoirNameEdit.FieldProperty.FieldName,
              ReservoirNameEdit.Text,LErrorMessage)) then
          begin
            LReservoirObject.ReservoirConfigurationData.ReservoirName := Trim(ReservoirNameEdit.Text);
            ReservoirNameEdit.SetFieldValue(LReservoirObject.ReservoirConfigurationData.ReservoirName);
            DoContextValidation(dvtResPropReservoirName);
          end;
          ReservoirNameEdit.ContextValidationError := LErrorMessage;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


procedure TDDTSDamPropertiesValidator.UpdateXCoord;
const OPNAME = 'TDDTSDamPropertiesValidator.UpdateXCoord';
var
  lErrorMessage    : string;
  LReservoirObject : IReservoirData;
begin
  try
    if DDTSDamPropertiesDialog.ReservoirXCoordEdit.HasValueChanged then
    begin
      LReservoirObject :=  TDDTSDataObject(FAppModules.Model.ModelData).DDTSDamDataList.CastReservoirList.ReservoirByIdentifier[FIdentifier];
      if (LReservoirObject <> nil) then
      begin
        with DDTSDamPropertiesDialog do
        begin
          if FAppModules.FieldProperties.ValidateFieldProperty('XCoord',ReservoirXCoordEdit.Text, lErrorMessage) then
          begin
            ReservoirXCoordEdit.FieldValidationError := lErrorMessage;
            LReservoirObject.ReservoirConfigurationData.XCoord := StrToFloat(ReservoirXCoordEdit.Text);
            ReservoirXCoordEdit.SetFieldValue(LReservoirObject.ReservoirConfigurationData.XCoord);
          end
          else
            ReservoirXCoordEdit.FieldValidationError := lErrorMessage;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDDTSDamPropertiesValidator.UpdateYCoord;
const OPNAME = 'TDDTSDamPropertiesValidator.UpdateYCoord';
var
  lErrorMessage    : string;
  LReservoirObject : IReservoirData;
begin
  try
    if DDTSDamPropertiesDialog.ReservoirYCoordEdit.HasValueChanged then
    begin
      LReservoirObject :=  TDDTSDataObject(FAppModules.Model.ModelData).DDTSDamDataList.CastReservoirList.ReservoirByIdentifier[FIdentifier];
      if (LReservoirObject <> nil) then
      begin
        with DDTSDamPropertiesDialog do
        begin
          if FAppModules.FieldProperties.ValidateFieldProperty('YCoord',ReservoirYCoordEdit.Text, lErrorMessage) then
          begin
            ReservoirYCoordEdit.FieldValidationError := lErrorMessage;
            LReservoirObject.ReservoirConfigurationData.YCoord := StrToFloat(ReservoirYCoordEdit.Text);
            ReservoirYCoordEdit.SetFieldValue(LReservoirObject.ReservoirConfigurationData.YCoord);
          end
          else
            ReservoirYCoordEdit.FieldValidationError := lErrorMessage;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDDTSDamPropertiesValidator.PopulateDataViewer;
const OPNAME = 'TDDTSDamPropertiesValidator.PopulateDataViewer';
begin
  inherited PopulateDataViewer;
  try
    ClearDataViewer;
    RePopulateDataViewer;
    DoContextValidation(dvtResPropAll);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDDTSDamPropertiesValidator.CurrentReservoir: IReservoirData;
const OPNAME = 'TDDTSDamPropertiesValidator.CurrentReservoir';
begin
  Result := nil;
  try
    Result := TDDTSDataObject(FAppModules.Model.ModelData).DDTSDamDataList.CastReservoirList.ReservoirByIdentifier[FIdentifier];

  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDDTSDamPropertiesValidator.DoContextValidation(AValidationType: TDialogValidationType);
const OPNAME = 'TDDTSDamPropertiesValidator.DoContextValidation';
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
              ValidateReservoirName(LReservoir);

        end;
      finally
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDDTSDamPropertiesValidator.ValidateReservoirName(AReservoir: IReservoirData);
const OPNAME = 'TDDTSDamPropertiesValidator.ValidateReservoirName';
begin
  try
    with DDTSDamPropertiesDialog do
    begin
      FErrorMessage := '';
      ReservoirNameEdit.ContextValidationError := FErrorMessage;
      if (NOT AReservoir.ReservoirConfigurationData.Validate(FErrorMessage ,'ReservoirName')) then
        FAllErrorMessages.Add(FErrorMessage);
      ReservoirNameEdit.ContextValidationError := FErrorMessage;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


end.


