{******************************************************************************}
{*  UNIT      : Contains the class TFMAllocationDefinitionValidator.          *}
{*  AUTHOR    : Riana Steyn                                                   *}
{*  DATE      : 2006/01/11                                                    *}
{*  COPYRIGHT : Copyright © 2006 DWAF                                         *}
{******************************************************************************}

unit UFMAllocationDefinitionValidator;

interface

uses
  Classes,
  VCL.Controls,
  VCL.ComCtrls,
  VCL.StdCtrls,
  VCL.Dialogs,
  VCL.ExtCtrls,
  UAbstractObject,
  UAbstractComponent,
  UDataComponent,
  UDataEditComponent,
  VoaimsCom_TLB,
  UFMAllocationDefinitionDialog,
  UAbstractYieldDataDialogValidator,
  UYieldContextValidationType;

type

  TFMAllocationDefinitionValidator = class(TAbstractYieldDataDialogValidator)
  protected
    FHeading  : string;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    procedure CreateDialog; virtual;
    procedure OnEditControlEnter(Sender: TObject); override;
    procedure OnEditControltExit(Sender: TObject); override;
    procedure OnFileSelectorClick(Sender: TObject);
    procedure UpdateAllocDefName;
    procedure UpdateAllocDefFileName(AFileName : string);
    procedure UpdateAllocDefStartYear;
    procedure UpdateAllocDefStartMonth;
    procedure PopulateAllocDefFileEndDate;
    procedure RePopulateDataViewer;
    procedure ValidateAllocDefName(AAllocDef : IAllocationDefinition);
    procedure ValidateAllocDefFileName(AAllocDef : IAllocationDefinition);
    procedure ValidateAllocDefStartYear(AAllocDef : IAllocationDefinition);
    procedure ValidateAllocDefStartMonth(AAllocDef : IAllocationDefinition);
  public
    function Initialise: boolean; override;
    function SaveState: boolean; override;
    function LanguageHasChanged: boolean; override;
    function StudyHasChanged: boolean; override;
    function StudyDataHasChanged(AContext: TChangeContext; AFieldName,AOldValue,ANewValue: string): boolean; override;
    procedure ClearDataViewer; override;
    procedure PopulateDataViewer; override;
    procedure DoContextValidation (AValidationType : TDialogValidationType); override;
    function FMAllocationDefinitionDialog: TFMAllocationDefinitionDialog;
  end;

implementation

uses
  SysUtils,
  VCL.Graphics,
  UUtilities,
  UYieldModelDataObject,
  UYieldModelDataGUIForm,
  UErrorHandlingOperations, Math;

{******************************************************************************}
{* TFMAllocationDefinitionValidator                                           *}
{******************************************************************************}

procedure TFMAllocationDefinitionValidator.CreateMemberObjects;
const OPNAME = 'TFMAllocationDefinitionValidator.CreateMemberObjects';
var
  lpPanel     : TFMAllocationDefinitionDialog;
begin
  try
    inherited CreateMemberObjects;
    FIdentifier := 0;
    FHeading    := 'TabCaption.AllocationDefinition';
    CreateDialog;
    lpPanel := FMAllocationDefinitionDialog;
    with lpPanel do
    begin
      EdtAllocDefName.FieldProperty      := FAppModules.FieldProperties.FieldProperty('AllocDefName');
      EdtAllocDefName.OnEnter            := OnEditControlEnter;
      EdtAllocDefName.OnExit             := OnEditControltExit;

      EdtAllocDefFileName.FieldProperty  := FAppModules.FieldProperties.FieldProperty('AllocDefFileName');
      EdtAllocDefFileName.OnEnter        := OnEditControlEnter;
      EdtAllocDefFileName.OnExit         := OnEditControltExit;

      CbxStartYear.FieldProperty         := FAppModules.FieldProperties.FieldProperty('AllocDefStartYear');
      CbxStartYear.OnEnter               := OnEditControlEnter;
      CbxStartYear.OnExit                := OnEditControltExit;

      CbxStartMonth.FieldProperty        := FAppModules.FieldProperties.FieldProperty('AllocDefStartMonth');
      CbxStartMonth.OnEnter              := OnEditControlEnter;
      CbxStartMonth.OnExit               := OnEditControltExit;

      BtnFileSelector.OnClick             := OnFileSelectorClick;

    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFMAllocationDefinitionValidator.CreateDialog;
const OPNAME = 'TFMAllocationDefinitionValidator.CreateDialog';
begin
  try
    FPanel  := TFMAllocationDefinitionDialog.Create(FPanelOwner,FAppModules);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFMAllocationDefinitionValidator.DestroyMemberObjects;
const OPNAME = 'TFMAllocationDefinitionValidator.DestroyMemberObjects';
begin
  try
    inherited DestroyMemberObjects;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFMAllocationDefinitionValidator.Initialise: boolean;
const OPNAME = 'TFMAllocationDefinitionValidator.Initialise';
var
  lIndex : integer;
  lYear  : word;
  lMonth : word;
  lDay   : word;
  lMonths : TMonthNamesArray;
begin
  Result := inherited Initialise;
  try
    with FMAllocationDefinitionDialog do
    begin
      lMonths := TYieldModelDataObject(FAppModules.Model.ModelData).
                   CastRunConfigurationData.MonthNamesArray;
      CbxStartYear.Clear;
      CbxStartMonth.Clear;
      DecodeDate(Now, lYear, lMonth, lDay);
      for lIndex := 1900 to lYear + 50 do
      begin
        CbxStartYear.Items.Add(IntToStr(lIndex));
      end;
      for lIndex := 1 to 12 do
      begin
        CbxStartMonth.Items.Add(UpperCase(lMonths[lIndex]));
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFMAllocationDefinitionValidator.LanguageHasChanged: boolean;
const OPNAME = 'TFMAllocationDefinitionValidator.LanguageHasChanged';
begin
  Result := False;
  try
    TabShetCaption := FAppModules.Language.GetString(FHeading);
    Result := inherited LanguageHasChanged;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFMAllocationDefinitionValidator.ClearDataViewer;
const OPNAME = 'TFMAllocationDefinitionValidator.ClearDataViewer';
var
  lpPanel     : TFMAllocationDefinitionDialog;
begin
  inherited ClearDataViewer;
  try
    lpPanel := FMAllocationDefinitionDialog;
    with lpPanel do
    begin
      EdtAllocDefName.Text     := '';
      CbxStartYear.ItemIndex   := -1;
      CbxStartMonth.ItemIndex  := -1;
      EdtAllocDefFileName.Text := '';
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFMAllocationDefinitionValidator.PopulateDataViewer;
const OPNAME = 'TFMAllocationDefinitionValidator.PopulateDataViewer';
begin
  inherited PopulateDataViewer;
  try
    ClearDataViewer;
    RePopulateDataViewer;
    DoContextValidation(dvtAllocDefPropAll);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFMAllocationDefinitionValidator.RePopulateDataViewer;
const OPNAME = 'TFMAllocationDefinitionValidator.RePopulateDataViewer';
var
  lAllocDef : IAllocationDefinition;
begin
  try
    if (FIdentifier > 0) then
    begin
      lAllocDef := (FAppModules.Model.ModelData as IPlanningModelData).
                     AllocationDefinitionsList.AllocationDefinitionByID[FIdentifier];
      if (lAllocDef <> nil) then
      begin
        with FMAllocationDefinitionDialog do
        begin
          EdtAllocDefName.SetFieldValue(lAllocDef.Name);
          CbxStartYear.SetFieldIndex(CbxStartYear.Items.IndexOf(IntToStr(lAllocDef.StartYear)));
          CbxStartMonth.SetFieldIndex(lAllocDef.StartMonth - 1);
          EdtAllocDefFileName.SetFieldValue(lAllocDef.AllocDefFileName);
          PopulateAllocDefFileEndDate;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFMAllocationDefinitionValidator.SaveState: boolean;
const OPNAME = 'TFMAllocationDefinitionValidator.SaveState';
begin
  Result := inherited SaveState;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFMAllocationDefinitionValidator.FMAllocationDefinitionDialog:TFMAllocationDefinitionDialog;
const OPNAME = 'TFMAllocationDefinitionValidator.FMAllocationDefinitionDialog';
begin
  Result := Nil;
  try
    if (FPanel <> nil) then
      Result := TFMAllocationDefinitionDialog(FPanel);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFMAllocationDefinitionValidator.StudyDataHasChanged(AContext: TChangeContext; AFieldName,AOldValue,ANewValue: string): boolean;
const OPNAME = 'TFMAllocationDefinitionValidator.StudyDataHasChanged';
begin
  Result := inherited StudyDataHasChanged(AContext,AFieldName,AOldValue,ANewValue);
  try
    if ((AFieldName = 'AllocDefName') OR
        (AFieldName = 'AllocDefFileName')) then
      RePopulateDataViewer;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFMAllocationDefinitionValidator.StudyHasChanged: boolean;
const OPNAME = 'TFMAllocationDefinitionValidator.StudyHasChanged';
begin
  Result := inherited StudyHasChanged;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFMAllocationDefinitionValidator.OnEditControlEnter(Sender: TObject);
const OPNAME = 'TFMAllocationDefinitionValidator.OnEditControlEnter';
begin
  inherited OnEditControlEnter(Sender);
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFMAllocationDefinitionValidator.OnEditControltExit(Sender: TObject);
const OPNAME = 'TFMAllocationDefinitionValidator.OnEditControltExit';
begin
  inherited OnEditControltExit(Sender);
  try
    with FMAllocationDefinitionDialog do
    begin
      if ((Sender = EdtAllocDefName) AND
          (EdtAllocDefName.HasValueChanged)) then
        UpdateAllocDefName
      else
      if ((Sender = CbxStartYear) AND
          (CbxStartYear.HasValueChanged)) then
        UpdateAllocDefStartYear
      else
      if ((Sender = CbxStartMonth) AND
          (CbxStartMonth.HasValueChanged)) then
        UpdateAllocDefStartMonth
      else
      if ((Sender = EdtAllocDefFileName) AND
           (EdtAllocDefFileName.HasValueChanged)) then
        UpdateAllocDefFileName(EdtAllocDefFileName.Text);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFMAllocationDefinitionValidator.OnFileSelectorClick(Sender: TObject);
const OPNAME = 'TFMAllocationDefinitionValidator.OnFileSelectorClick';
var
  LFileName : string;
begin
  try
    if PromptForFileName(LFileName, '*.dat','','select allocation definition file','',False) then
      UpdateAllocDefFileName(LFileName);;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


procedure TFMAllocationDefinitionValidator.UpdateAllocDefName;
const OPNAME = 'TFMAllocationDefinitionValidator.UpdateAllocDefName';
var
  lAllocDef : IAllocationDefinition;
  lMessage  : string;
begin
  try
      lAllocDef := (FAppModules.Model.ModelData as IPlanningModelData).
                     AllocationDefinitionsList.AllocationDefinitionByID[FIdentifier];
    if (lAllocDef <> nil) then
    begin
      with FMAllocationDefinitionDialog do
      begin
        if (FAppModules.FieldProperties.ValidateFieldProperty(
            EdtAllocDefName.FieldProperty.FieldName,
            EdtAllocDefName.Text, lMessage)) then
        begin
          EdtAllocDefName.FieldValidationError := lMessage;
          lAllocDef.Name := Trim(EdtAllocDefName.Text);
          EdtAllocDefName.SetFieldValue(lAllocDef.Name);
          DoContextValidation(dvtAllocDefName);
        end
        else
          EdtAllocDefName.FieldValidationError := lMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFMAllocationDefinitionValidator.UpdateAllocDefFileName(AFilename: string);
const OPNAME = 'TFMAllocationDefinitionValidator.UpdateAllocDefFileName';
var
  LMessage  : string;
  lAllocDef : IAllocationDefinition;
begin
  try
    lAllocDef := (FAppModules.Model.ModelData as IPlanningModelData).
                  AllocationDefinitionsList.AllocationDefinitionByID[FIdentifier];
    if (lAllocDef <> nil) then
    begin
      with FMAllocationDefinitionDialog do
      begin
        if (FAppModules.FieldProperties.ValidateFieldProperty('AllocDefFileName',
            EdtAllocDefFileName.Text, LMessage)) then
        begin
          EdtAllocDefFileName.FieldValidationError := LMessage;
          lAllocDef.AllocDefFileName := AFileName;
          RePopulateDataViewer;
          DoContextValidation(dvtAllocDefFileName);
        end
        else
          EdtAllocDefFileName.FieldValidationError := LMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFMAllocationDefinitionValidator.UpdateAllocDefStartYear;
const OPNAME = 'TFMAllocationDefinitionValidator.UpdateAllocDefStartYear';
var
  lAllocDef : IAllocationDefinition;
  lMessage  : string;
  lValue    : integer;
begin
  try
      lAllocDef := (FAppModules.Model.ModelData as IPlanningModelData).
                     AllocationDefinitionsList.AllocationDefinitionByID[FIdentifier];
    if (lAllocDef <> nil) then
    begin
      with FMAllocationDefinitionDialog do
      begin
        if (CbxStartYear.ItemIndex >= 0) then
        begin
          lValue := StrToInt(CbxStartYear.Items[CbxStartYear.ItemIndex]);
          if (FAppModules.FieldProperties.ValidateFieldProperty(
              CbxStartYear.FieldProperty.FieldName,
              IntToStr(lValue), lMessage)) then
          begin
            lAllocDef.StartYear := lValue;
            CbxStartYear.SetFieldIndex
              (CbxStartYear.Items.IndexOf(IntToStr(lAllocDef.StartYear)));
            DoContextValidation(dvtAllocDefStartYear);
          end
          else
            CbxStartYear.ValidationError := lMessage;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFMAllocationDefinitionValidator.UpdateAllocDefStartMonth;
const OPNAME = 'TFMAllocationDefinitionValidator.UpdateAllocDefStartMonth';
var
  lAllocDef : IAllocationDefinition;
  lMessage  : string;
  lValue    : integer;
begin
  try
      lAllocDef := (FAppModules.Model.ModelData as IPlanningModelData).
                     AllocationDefinitionsList.AllocationDefinitionByID[FIdentifier];
    if (lAllocDef <> nil) then
    begin
      with FMAllocationDefinitionDialog do
      begin
        if (CbxStartMonth.ItemIndex >= 0) then
        begin
          lValue := CbxStartMonth.ItemIndex + 1;
          if (FAppModules.FieldProperties.ValidateFieldProperty(
              CbxStartMonth.FieldProperty.FieldName,
              IntToStr(lValue), lMessage)) then
          begin
            lAllocDef.StartMonth := lValue;
            CbxStartMonth.SetFieldIndex(lAllocDef.StartMonth - 1);
            DoContextValidation(dvtAllocDefStartMonth);
          end
          else
            CbxStartMonth.ValidationError := lMessage;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFMAllocationDefinitionValidator.PopulateAllocDefFileEndDate;
const OPNAME = 'TFMAllocationDefinitionValidator.PopulateAllocDefFileEndDate';
var
  LIndex             : integer;
  LPreviousEndYear   : integer;
  LAllocDef          : IAllocationDefinition;
  LOthersAllocDef    : IAllocationDefinition;
  lAllocDefList      : IAllocationDefinitionsList;
begin
  try
    LAllocDef := (FAppModules.Model.ModelData as IPlanningModelData).
                     AllocationDefinitionsList.AllocationDefinitionByID[FIdentifier];
    if (LAllocDef <> nil) then
    begin
      lAllocDefList := (FAppModules.Model.ModelData as IPlanningModelData).
                        AllocationDefinitionsList;
      LPreviousEndYear := 0;
      for LIndex := 0 to lAllocDefList.AllocationDefinitionCount - 1 do
      begin
        LOthersAllocDef := lAllocDefList.AllocationDefinitionByIndex[Lindex];
        if (LAllocDef.AllocationDefinitionID <> LOthersAllocDef.AllocationDefinitionID) then
        begin
          if ((LAllocDef.StartYear < LOthersAllocDef.StartYear) OR
             ((LAllocDef.StartYear = LOthersAllocDef.StartYear) AND (LAllocDef.StartMonth <= LOthersAllocDef.StartMonth))) then
          begin
            if LPreviousEndYear = 0 then
              LPreviousEndYear := LOthersAllocDef.StartYear;
            if LPreviousEndYear <=  LOthersAllocDef.StartYear then
            begin
              FMAllocationDefinitionDialog.LblDisplayEndDate.Caption := IntToStr(LPreviousEndYear) + '/' + IntToStr(LOthersAllocDef.StartMonth);
              LPreviousEndYear := LOthersAllocDef.StartYear;
            end;
          end;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


procedure TFMAllocationDefinitionValidator.DoContextValidation(AValidationType : TDialogValidationType);
const OPNAME = 'TFMAllocationDefinitionValidator.DoContextValidation';
var
  lAllocDef : IAllocationDefinition;
begin
  try
    FAllErrorMessages.Clear;
    if (FIdentifier > 0) then
    begin
      lAllocDef := (FAppModules.Model.ModelData as IPlanningModelData).
                     AllocationDefinitionsList.AllocationDefinitionByID[FIdentifier];
      if (lAllocDef <> nil) then
      begin
        if (AValidationType in [dvtAllocDefPropAll, dvtAllocDefName]) then
          ValidateAllocDefName(lAllocDef);
        if (AValidationType in [dvtAllocDefPropAll, dvtAllocDefFileName]) then
          ValidateAllocDefFileName(lAllocDef);
        if (AValidationType in [dvtAllocDefPropAll, dvtAllocDefStartYear,
                                dvtAllocDefStartMonth]) then
        begin
          ValidateAllocDefStartYear(lAllocDef);
          ValidateAllocDefStartMonth(lAllocDef);
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFMAllocationDefinitionValidator.ValidateAllocDefName (AAllocDef : IAllocationDefinition);
const OPNAME = 'TFMAllocationDefinitionValidator.ValidateAllocDefName';
begin
  try
    with FMAllocationDefinitionDialog do
    begin
      FErrorMessage := '';
      if (NOT AAllocDef.Validate(FErrorMessage, 'AllocDefName')) then
        FAllErrorMessages.Add(Trim(FErrorMessage));
      EdtAllocDefName.ContextValidationError := FErrorMessage;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFMAllocationDefinitionValidator.ValidateAllocDefFileName(AAllocDef: IAllocationDefinition);
const OPNAME = 'TFMAllocationDefinitionValidator.ValidateAllocDefFileName';
begin
  try
    with FMAllocationDefinitionDialog do
    begin
      FErrorMessage := '';
      if (NOT AAllocDef.Validate(FErrorMessage, 'AllocDefFileName')) then
        FAllErrorMessages.Add(Trim(FErrorMessage));
      EdtAllocDefFileName.ContextValidationError := FErrorMessage;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


procedure TFMAllocationDefinitionValidator.ValidateAllocDefStartYear (AAllocDef : IAllocationDefinition);
const OPNAME = 'TFMAllocationDefinitionValidator.ValidateAllocDefStartYear';
begin
  try
    with FMAllocationDefinitionDialog do
    begin
      FErrorMessage := '';
      if (NOT AAllocDef.Validate(FErrorMessage, 'AllocDefStartYear')) then
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

procedure TFMAllocationDefinitionValidator.ValidateAllocDefStartMonth (AAllocDef : IAllocationDefinition);
const OPNAME = 'TFMAllocationDefinitionValidator.ValidateAllocDefStartMonth';
begin
  try
    with FMAllocationDefinitionDialog do
    begin
      FErrorMessage := '';
      if (NOT AAllocDef.Validate(FErrorMessage, 'AllocDefStartMonth')) then
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

