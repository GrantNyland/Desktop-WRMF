{******************************************************************************}
{*  UNIT      : Contains the class TSwitchDefinitionValidator.                *}
{*  AUTHOR    : Riana Steyn                                                   *}
{*  DATE      : 2006/02/27                                                    *}
{*  COPYRIGHT : Copyright © 2006 DWAF                                         *}
{******************************************************************************}

unit USwitchDefinitionValidator;

interface

uses
  Classes,
  VCL.Controls,
  VCL.ComCtrls,
  VCL.StdCtrls,
  VCL.ExtCtrls,
  UAbstractObject,
  UAbstractComponent,
  UDataComponent,
  UDataEditComponent,
  VoaimsCom_TLB,
  USwitchDefinitionDialog,
  UAbstractYieldDataDialogValidator,
  UYieldContextValidationType;

type

  TSwitchDefinitionValidator = class(TAbstractYieldDataDialogValidator)
  protected
    FHeading  : string;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    procedure CreateDialog; virtual;
    procedure OnEditControlEnter(Sender: TObject); override;
    procedure OnEditControltExit(Sender: TObject); override;
    procedure UpdateSwitchDefFileName;
    procedure UpdateAllocDefStartYear;
    procedure UpdateAllocDefStartMonth;

    procedure RePopulateDataViewer;
    procedure PopulateDateComboBoxes;
    procedure PopulateDateValues(ASwitchDef : ISwitchDefinition);

    procedure ValidateSwitchDefFileName (ASwitchDef : ISwitchDefinition);
    procedure ValidateSwitchDefStartYear (ASwitchDef : ISwitchDefinition);
    procedure ValidateSwitchDefStartMonth (ASwitchDef : ISwitchDefinition);
  public
    function Initialise: boolean; override;
    function SaveState: boolean; override;
    function LanguageHasChanged: boolean; override;
    function StudyHasChanged: boolean; override;
    function StudyDataHasChanged(AContext: TChangeContext; AFieldName,AOldValue,ANewValue: string): boolean; override;
    procedure ClearDataViewer; override;
    procedure PopulateDataViewer; override;
    procedure DoContextValidation (AValidationType : TDialogValidationType); override;
    function SwitchDefinitionDialog: TSwitchDefinitionDialog;
  end;

implementation

uses
  SysUtils,
  VCL.Graphics,
  UUtilities,
  USwitchDefinition,
  UPlanningModelDataObject,
  UYieldModelDataGUIForm,
  UYieldModelDataObject,
  UErrorHandlingOperations;

{******************************************************************************}
{* TSwitchDefinitionValidator                                                 *}
{******************************************************************************}

procedure TSwitchDefinitionValidator.CreateMemberObjects;
const OPNAME = 'TSwitchDefinitionValidator.CreateMemberObjects';
var
  lpPanel     : TSwitchDefinitionDialog;
begin
  try
    inherited CreateMemberObjects;
    FIdentifier := 0;
    FHeading    := 'TabCaption.SwitchDefinition';
    CreateDialog;
    lpPanel := SwitchDefinitionDialog;
    with lpPanel do
    begin
      EdtSwitchDefFileName.FieldProperty := FAppModules.FieldProperties.FieldProperty('SwitchDefFileName');
      EdtSwitchDefFileName.OnEnter       := OnEditControlEnter;
      EdtSwitchDefFileName.OnExit        := OnEditControltExit;

      CbxStartYear.FieldProperty  := FAppModules.FieldProperties.FieldProperty('AllocDefStartYear');
      CbxStartYear.OnEnter        := OnEditControlEnter;
      CbxStartYear.OnChange       := OnEditControltExit;

      CbxStartMonth.FieldProperty      := FAppModules.FieldProperties.FieldProperty('AllocDefStartMonth');
      CbxStartMonth.OnEnter            := OnEditControlEnter;
      CbxStartMonth.OnChange           := OnEditControltExit;
      
      lbxChannels.FieldProperty        := FAppModules.FieldProperties.FieldProperty('ChannelNumber');
      lbxChannels.IsEnabled            := False;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TSwitchDefinitionValidator.CreateDialog;
const OPNAME = 'TSwitchDefinitionValidator.CreateDialog';
begin
  try
    FPanel  := TSwitchDefinitionDialog.Create(FPanelOwner,FAppModules);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TSwitchDefinitionValidator.DestroyMemberObjects;
const OPNAME = 'TSwitchDefinitionValidator.DestroyMemberObjects';
begin
  try
    inherited DestroyMemberObjects;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TSwitchDefinitionValidator.Initialise: boolean;
const OPNAME = 'TSwitchDefinitionValidator.Initialise';
begin
  Result := inherited Initialise;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TSwitchDefinitionValidator.LanguageHasChanged: boolean;
const OPNAME = 'TSwitchDefinitionValidator.LanguageHasChanged';
begin
  Result := False;
  try
    TabShetCaption := FAppModules.Language.GetString(FHeading);
    Result := inherited LanguageHasChanged;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TSwitchDefinitionValidator.ClearDataViewer;
const OPNAME = 'TSwitchDefinitionValidator.ClearDataViewer';
var
  lpPanel     : TSwitchDefinitionDialog;
begin
  inherited ClearDataViewer;
  try
    lpPanel := SwitchDefinitionDialog;
    with lpPanel do
    begin
      EdtSwitchDefFileName.Text := '';
      CbxStartYear.ItemIndex    := -1;
      CbxStartMonth.ItemIndex   := -1;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TSwitchDefinitionValidator.PopulateDateComboBoxes;
const OPNAME = 'TSwitchDefinitionValidator.PopulateDateComboBoxes';
var
  lIndex : integer;
  lYear  : word;
  lMonth : word;
  lDay   : word;
begin
  try
    with SwitchDefinitionDialog do
    begin
      CbxStartYear.Clear;
      CbxStartMonth.Clear;
      DecodeDate(Now, lYear, lMonth, lDay);
      for lIndex := 1900 to lYear + 1000 do
      begin
        CbxStartYear.Items.Add(IntToStr(lIndex));
      end;
      for lIndex := 1 to 12 do
      begin
        CbxStartMonth.Items.Add(UpperCase(FormatSettings.ShortMonthNames[lIndex]));
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TSwitchDefinitionValidator.PopulateDataViewer;
const OPNAME = 'TSwitchDefinitionValidator.PopulateDataViewer';
begin
  inherited PopulateDataViewer;
  try
    ClearDataViewer;
    PopulateDateComboBoxes;
    RePopulateDataViewer;
    DoContextValidation(dvtSwitchDefPropAll);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TSwitchDefinitionValidator.PopulateDateValues(ASwitchDef : ISwitchDefinition);
const OPNAME = 'TSwitchDefinitionValidator.PopulateDateValues';
var
  LCalendarStartMonth,
  LStartYear,
  LStartMonth : integer;
begin
  try
    with SwitchDefinitionDialog do
    begin
      LCalendarStartMonth := FAppModules.StudyArea.CalendarStartMonth;
      LStartYear          := ConvertHydrologicalYearToCalendarYear(LCalendarStartMonth,ASwitchDef.SwitchDefStartYear,ASwitchDef.SwitchDefStartMonth);
      LStartMonth         := ConvertHydrologicalMonthToCalendarMonth(LCalendarStartMonth,ASwitchDef.SwitchDefStartYear,ASwitchDef.SwitchDefStartMonth);

      CbxStartYear.SetFieldIndex(CbxStartYear.Items.IndexOf(IntToStr(LStartYear)));
      CbxStartMonth.SetFieldIndex(LStartMonth-1);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TSwitchDefinitionValidator.RePopulateDataViewer;
const OPNAME = 'TSwitchDefinitionValidator.RePopulateDataViewer';
var
  lSwitchDef : ISwitchDefinition;
  LChannelList   : IChannelList;
  LChannel       : IGeneralFlowChannel;
  LSwitchControl : IChannelSwitchControl;
  LCount,
  LIndex : integer;
begin
  try
    if (FIdentifier > 0) then
    begin
     lSwitchDef := (FAppModules.Model.ModelData as IPlanningModelData).
                     SwitchDefinitionsList.SwitchDefinitionByID[FIdentifier];
      if (lSwitchDef <> nil) then
      begin
        with SwitchDefinitionDialog do
        begin
          EdtSwitchDefFileName.SetFieldValue(lSwitchDef.SwitchDefFileName);
          PopulateDateValues(lSwitchDef);
          LChannelList := TYieldModelDataObject(FAppModules.Model.ModelData).
                          NetworkElementData.ChannelList;
          for LIndex := 0 to LChannelList.ChannelCount -1 do
          begin
            LChannel := LChannelList.ChannelByIndex[LIndex];
            if LChannel <> nil then
            begin
              for LCount := 0 to LChannel.SwitchControlCount - 1 do
              begin
                LSwitchControl   := LChannel.SwitchControlByIndex[LCount];
                if (LSwitchControl <> nil) and (LSwitchControl.SwitchDefinitionID = LSwitchDef.SwitchDefID) then
                  lbxChannels.Items.Add(LChannel.ChannelName);
              end;
            end;
          end;

        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TSwitchDefinitionValidator.SaveState: boolean;
const OPNAME = 'TSwitchDefinitionValidator.SaveState';
begin
  Result := inherited SaveState;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TSwitchDefinitionValidator.SwitchDefinitionDialog:TSwitchDefinitionDialog;
const OPNAME = 'TSwitchDefinitionValidator.SwitchDefinitionDialog';
begin
  Result := Nil;
  try
    if (FPanel <> nil) then
      Result := TSwitchDefinitionDialog(FPanel);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TSwitchDefinitionValidator.StudyDataHasChanged(AContext: TChangeContext; AFieldName,AOldValue,ANewValue: string): boolean;
const OPNAME = 'TSwitchDefinitionValidator.StudyDataHasChanged';
begin
  Result := inherited StudyDataHasChanged(AContext,AFieldName,AOldValue,ANewValue);
  try
    if (AFieldName = 'SwitchDefFileName') then
      RePopulateDataViewer;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TSwitchDefinitionValidator.StudyHasChanged: boolean;
const OPNAME = 'TSwitchDefinitionValidator.StudyHasChanged';
begin
  Result := inherited StudyHasChanged;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TSwitchDefinitionValidator.OnEditControlEnter(Sender: TObject);
const OPNAME = 'TSwitchDefinitionValidator.OnEditControlEnter';
begin
  inherited OnEditControlEnter(Sender);
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TSwitchDefinitionValidator.OnEditControltExit(Sender: TObject);
const OPNAME = 'TSwitchDefinitionValidator.OnEditControltExit';
begin
  inherited OnEditControltExit(Sender);
  try
    with SwitchDefinitionDialog do
    begin
      if ((Sender = EdtSwitchDefFileName) AND
          (EdtSwitchDefFileName.HasValueChanged)) then
        UpdateSwitchDefFileName
      else
      if ((Sender = CbxStartYear) AND
          (CbxStartYear.HasValueChanged)) then
        UpdateAllocDefStartYear
      else
      if ((Sender = CbxStartMonth) AND
          (CbxStartMonth.HasValueChanged)) then
        UpdateAllocDefStartMonth;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TSwitchDefinitionValidator.UpdateSwitchDefFileName;
const OPNAME = 'TSwitchDefinitionValidator.UpdateSwitchDefFileName';
var
  lSwitchDef : ISwitchDefinition;
  lMessage  : string;
begin
  try
    lSwitchDef := (FAppModules.Model.ModelData as IPlanningModelData).
                     SwitchDefinitionsList.SwitchDefinitionByID[FIdentifier];
    if (lSwitchDef <> nil) then
    begin
      with SwitchDefinitionDialog do
      begin
        if (FAppModules.FieldProperties.ValidateFieldProperty(
            EdtSwitchDefFileName.FieldProperty.FieldName,
            EdtSwitchDefFileName.Text, lMessage)) then
        begin
          EdtSwitchDefFileName.FieldValidationError := lMessage;
          lSwitchDef.SwitchDefFileName := Trim(EdtSwitchDefFileName.Text);
          EdtSwitchDefFileName.SetFieldValue(lSwitchDef.SwitchDefFileName);
          DoContextValidation(dvtSwitchDefFileName);
        end
        else
          EdtSwitchDefFileName.FieldValidationError := lMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TSwitchDefinitionValidator.UpdateAllocDefStartYear;
const OPNAME = 'TSwitchDefinitionValidator.UpdateAllocDefStartYear';
var
  lSwitchDef : ISwitchDefinition;
  lMessage   : string;

  LCalendarStartMonth,
  LHydroYear,
  LHydroMonth,
  LCalendarYear,
  LCalendarMonth : integer;
begin
  try
    lSwitchDef := (FAppModules.Model.ModelData as IPlanningModelData).
                     SwitchDefinitionsList.SwitchDefinitionByID[FIdentifier];
    if (lSwitchDef <> nil) then
    begin
      with SwitchDefinitionDialog do
      begin
        if (CbxStartYear.ItemIndex >= 0) then
        begin
          if (FAppModules.FieldProperties.ValidateFieldProperty(CbxStartYear.FieldProperty.FieldName,CbxStartYear.Text, lMessage)) then
          begin
            LCalendarStartMonth := FAppModules.StudyArea.CalendarStartMonth;
            LCalendarYear          := StrToInt(CbxStartYear.Text);
            LCalendarMonth         := CbxStartMonth.ItemIndex+1;
            LHydroMonth            :=  ConvertCalendarMonthToHydrologicalMonth(LCalendarStartMonth,LCalendarYear,LCalendarMonth);
            LHydroYear             :=  ConvertCalendarYearToHydrologicalYear(LCalendarStartMonth,LCalendarYear,LCalendarMonth);

            lSwitchDef.SwitchDefStartYear := LHydroYear;
            if(lSwitchDef.SwitchDefStartMonth <> LHydroMonth) then
              lSwitchDef.SwitchDefStartMonth := LHydroMonth;

            PopulateDateValues(lSwitchDef);
            DoContextValidation(dvtSwitchDefStartYear);
          end
          else
            CbxStartYear.ValidationError := lMessage;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TSwitchDefinitionValidator.UpdateAllocDefStartMonth;
const OPNAME = 'TSwitchDefinitionValidator.UpdateAllocDefStartMonth';
var
  lSwitchDef : ISwitchDefinition;
  lMessage   : string;

  LCalendarStartMonth,
  LHydroYear,
  LHydroMonth,
  LCalendarYear,
  LCalendarMonth : integer;
begin
  try
    lSwitchDef := (FAppModules.Model.ModelData as IPlanningModelData).
                     SwitchDefinitionsList.SwitchDefinitionByID[FIdentifier];
    if (lSwitchDef <> nil) then
    begin
      with SwitchDefinitionDialog do
      begin
        if (CbxStartMonth.ItemIndex >= 0) then
        begin
          if (FAppModules.FieldProperties.ValidateFieldProperty(CbxStartMonth.FieldProperty.FieldName,IntToStr(CbxStartMonth.ItemIndex+1), lMessage)) then
          begin
            LCalendarStartMonth := FAppModules.StudyArea.CalendarStartMonth;
            LCalendarYear          := StrToInt(CbxStartYear.Text);
            LCalendarMonth         := CbxStartMonth.ItemIndex+1;
            LHydroMonth            :=  ConvertCalendarMonthToHydrologicalMonth(LCalendarStartMonth,LCalendarYear,LCalendarMonth);
            LHydroYear             :=  ConvertCalendarYearToHydrologicalYear(LCalendarStartMonth,LCalendarYear,LCalendarMonth);

            lSwitchDef.SwitchDefStartMonth := LHydroMonth;
            if(lSwitchDef.SwitchDefStartYear <> LHydroYear) then
              lSwitchDef.SwitchDefStartYear := LHydroYear;

            PopulateDateValues(lSwitchDef);
            DoContextValidation(dvtSwitchDefStartMonth);
          end
          else
            CbxStartMonth.ValidationError := lMessage;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TSwitchDefinitionValidator.DoContextValidation(AValidationType : TDialogValidationType);
const OPNAME = 'TSwitchDefinitionValidator.DoContextValidation';
var
  lSwitchDef : ISwitchDefinition;
begin
  try
    FAllErrorMessages.Clear;

    if (FIdentifier > 0) then
    begin
      lSwitchDef := (FAppModules.Model.ModelData as IPlanningModelData).
                     SwitchDefinitionsList.SwitchDefinitionByID[FIdentifier];
      if (lSwitchDef <> nil) then
      begin
        if (AValidationType = dvtSwitchDefPropAll) OR
           (AValidationType = dvtSwitchDefFileName) then
          ValidateSwitchDefFileName(lSwitchDef);
        if (AValidationType = dvtSwitchDefPropAll) OR
           (AValidationType = dvtSwitchDefStartYear) OR
           (AValidationType = dvtSwitchDefStartMonth) then
        begin
          ValidateSwitchDefStartYear(lSwitchDef);
          ValidateSwitchDefStartMonth(lSwitchDef);
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TSwitchDefinitionValidator.ValidateSwitchDefFileName (ASwitchDef : ISwitchDefinition);
const OPNAME = 'TSwitchDefinitionValidator.ValidateSwitchDefFileName';
begin
  try
    with SwitchDefinitionDialog do
    begin
      FErrorMessage := '';
      if (NOT ASwitchDef.Validate(FErrorMessage, 'SwitchDefFileName')) then
        FAllErrorMessages.Add(Trim(FErrorMessage));
      EdtSwitchDefFileName.ContextValidationError := FErrorMessage;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TSwitchDefinitionValidator.ValidateSwitchDefStartYear (ASwitchDef : ISwitchDefinition);
const OPNAME = 'TSwitchDefinitionValidator.ValidateSwitchDefStartYear';
begin
  try
    with SwitchDefinitionDialog do
    begin
      FErrorMessage := '';
      if (NOT ASwitchDef.Validate(FErrorMessage, 'SwitchDefStartYear')) then
      begin
        CbxStartYear.InValidationError := TRUE;
        CbxStartYear.ValidationError := FErrorMessage;
        CbxStartYear.ShowErrorState(TRUE);
        FAllErrorMessages.Add(Trim(FErrorMessage));
      end
      else
      begin
        CbxStartYear.InValidationError := FALSE;
        CbxStartYear.ShowErrorState(FALSE);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TSwitchDefinitionValidator.ValidateSwitchDefStartMonth (ASwitchDef : ISwitchDefinition);
const OPNAME = 'TSwitchDefinitionValidator.ValidateSwitchDefStartMonth';
begin
  try
    with SwitchDefinitionDialog do
    begin
      FErrorMessage := '';
      if (NOT ASwitchDef.Validate(FErrorMessage, 'SwitchDefStartMonth')) then
      begin
        CbxStartMonth.InValidationError := TRUE;
        CbxStartMonth.ValidationError := FErrorMessage;
        CbxStartMonth.ShowErrorState(TRUE);
        FAllErrorMessages.Add(Trim(FErrorMessage));
      end
      else
      begin
        CbxStartMonth.InValidationError := FALSE;
        CbxStartMonth.ShowErrorState(FALSE);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.

