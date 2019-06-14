//
//
//  UNIT      : Contains the class TOutputReviewDataSourceValidator.
//  AUTHOR    : Dziedzi Ramulondi
//  DATE      : 2007/09/20
//  COPYRIGHT : Copyright © 2007 DWAF
//
//
unit UOutputReviewDataSourceValidator;

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
  VoaimsCom_TLB,
  UDataEditComponent,
  UYieldContextValidationType,
  UAbstractYieldDataDialogValidator,
  UOutputReviewDataSourceDialog;

type
  TOutputReviewDataSourceValidator = class(TAbstractYieldDataDialogValidator)
  protected
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    procedure OnEditControlEnter(Sender: TObject); override;
    procedure OnEditControltExit(Sender: TObject); override;

    procedure AvailableDataSourcesComboOnChange(Sender: TObject);
    procedure LoadFromSourceButtonOnClick(Sender: TObject);
    procedure RePopulateDataViewer;
    function OutputReviewDataSourceDialog: TOutputReviewDataSourceDialog;
  public
    function Initialise: boolean; override;
    function SaveState: boolean; override;
    function LanguageHasChanged: boolean; override;
    function StudyHasChanged: boolean; override;
    function StudyDataHasChanged(AContext: TChangeContext; AFieldName,AOldValue,ANewValue: string): boolean; override;

    procedure ClearDataViewer; override;
    procedure PopulateDataViewer; override;
    procedure DoContextValidation (AValidationType : TDialogValidationType); override;
    function DetermineWizardStatus (ASequence : integer = 0) : integer; override;
  end;

implementation

uses
  SysUtils,
  UOutputData,
  UOutputDataLoadAgent,
  UYieldModelDataObject,
  UErrorHandlingOperations;

{ TOutputReviewDataSourceValidator }

procedure TOutputReviewDataSourceValidator.CreateMemberObjects;
const OPNAME = 'TOutputReviewDataSourceValidator.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
    FIdentifier := -1;
    FPanel := TOutputReviewDataSourceDialog.Create(FPanelOwner,FAppModules);

    with OutputReviewDataSourceDialog do
    begin
      CurrentDataSourceEdit.FieldProperty := FAppModules.FieldProperties.FieldProperty('SumOutCurrentDataSource');
      CurrentDataSourceEdit.OnEnter       := OnEditControlEnter;
      CurrentDataSourceEdit.IsEnabled     := FALSE;

      AvailableDataSourcesCombo.FieldProperty := FAppModules.FieldProperties.FieldProperty('SumOutAvailableDataSource');
      AvailableDataSourcesCombo.OnEnter       := OnEditControlEnter;
      AvailableDataSourcesCombo.OnChange      := AvailableDataSourcesComboOnChange;

      LoadFromSourceButton.FieldProperty := FAppModules.FieldProperties.FieldProperty('SumOutLoadFromSource');
      LoadFromSourceButton.OnEnter       := OnEditControlEnter;
      LoadFromSourceButton.OnClick       := LoadFromSourceButtonOnClick;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputReviewDataSourceValidator.DestroyMemberObjects;
const OPNAME = 'TOutputReviewDataSourceValidator.DestroyMemberObjects';
begin
  try
    inherited DestroyMemberObjects;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOutputReviewDataSourceValidator.Initialise: boolean;
const OPNAME = 'TOutputReviewDataSourceValidator.Initialise';
begin
  Result := inherited Initialise;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOutputReviewDataSourceValidator.LanguageHasChanged: boolean;
const OPNAME = 'TOutputReviewDataSourceValidator.LanguageHasChanged';
begin
  Result := False;
  try
    TabShetCaption := FAppModules.Language.GetString('TabCaption.Properties');
    Result := inherited LanguageHasChanged;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputReviewDataSourceValidator.ClearDataViewer;
const OPNAME = 'TOutputReviewDataSourceValidator.ClearDataViewer';
begin
  inherited ClearDataViewer;
  try
    OutputReviewDataSourceDialog.CurrentDataSourceEdit.SetFieldValue('');
    OutputReviewDataSourceDialog.AvailableDataSourcesCombo.Items.Clear;
    OutputReviewDataSourceDialog.AvailableDataSourcesCombo.Text := '';
    OutputReviewDataSourceDialog.LoadFromSourceButton.IsEnabled := False;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputReviewDataSourceValidator.PopulateDataViewer;
const OPNAME = 'TOutputReviewDataSourceValidator.PopulateDataViewer';
begin
  inherited PopulateDataViewer;
  try
    ClearDataViewer;
    RePopulateDataViewer;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputReviewDataSourceValidator.RePopulateDataViewer;
const OPNAME = 'TOutputReviewDataSourceValidator.RePopulateDataViewer';
var
  LSummaryOutputData  : TSummaryOutputData;
  LSumOutFile         : string;
  LDatabase           : string;
  //LBlobFile           : string;
  //LBlobDatabase       : string;
  LPlotOutFile        : string;
begin
  try
    LSumOutFile    := FAppModules.Language.GetString('DataSources.DataSourceSumOutFile');
    LDatabase      := FAppModules.Language.GetString('DataSources.DataSourceDatabase');
    //LBlobFile      := FAppModules.Language.GetString('DataSources.DataSourceBlobFile');
    //LBlobDatabase  := FAppModules.Language.GetString('DataSources.DataSourceBlobDatabase');
    LPlotOutFile   := FAppModules.Language.GetString('DataSources.DataSourcePltOutFile');

    with OutputReviewDataSourceDialog do
    begin
      LSummaryOutputData := TYieldModelDataObject(FAppModules.Model.ModelData).OutputData.CastSummaryOutputData;
      case LSummaryOutputData.DataSources.CurrentSource of
        sodsNone         : CurrentDataSourceEdit.SetFieldValue('');
        sodsSumFile      : CurrentDataSourceEdit.SetFieldValue(LSumOutFile);
        //sodsDatabase     : CurrentDataSourceEdit.SetFieldValue(LDatabase);
        //sodsBlobFile     : CurrentDataSourceEdit.SetFieldValue(LBlobFile);
        //sodsBlobDatabase : CurrentDataSourceEdit.SetFieldValue(LBlobDatabase);
        sodsPltFile      : CurrentDataSourceEdit.SetFieldValue(LPlotOutFile);
      end;

      if LSummaryOutputData.DataSources.DataSourceAvailable[sodsSumFile] then
         OutputReviewDataSourceDialog.AvailableDataSourcesCombo.Items.AddObject(LSumOutFile,TObject(Ord(sodsSumFile)));
      {if LSummaryOutputData.DataSources.DataSourceAvailable[sodsDatabase] then
         OutputReviewDataSourceDialog.AvailableDataSourcesCombo.Items.AddObject(LDatabase,TObject(Ord(sodsDatabase)));
      if LSummaryOutputData.DataSources.DataSourceAvailable[sodsBlobFile] then
         OutputReviewDataSourceDialog.AvailableDataSourcesCombo.Items.AddObject(LBlobFile,TObject(Ord(sodsBlobFile)));
      if LSummaryOutputData.DataSources.DataSourceAvailable[sodsBlobDatabase] then
         OutputReviewDataSourceDialog.AvailableDataSourcesCombo.Items.AddObject(LBlobDatabase,TObject(Ord(sodsBlobDatabase)));}
      if LSummaryOutputData.DataSources.DataSourceAvailable[sodsPltFile] then
         OutputReviewDataSourceDialog.AvailableDataSourcesCombo.Items.AddObject(LPlotOutFile,TObject(Ord(sodsPltFile)));

      LoadFromSourceButton.IsEnabled     := (OutputReviewDataSourceDialog.AvailableDataSourcesCombo.Items.Count > 0);
      if LoadFromSourceButton.IsEnabled then
      begin
        OutputReviewDataSourceDialog.AvailableDataSourcesCombo.ItemIndex :=
          OutputReviewDataSourceDialog.AvailableDataSourcesCombo.Items.IndexOf(CurrentDataSourceEdit.Text);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOutputReviewDataSourceValidator.SaveState: boolean;
const OPNAME = 'TOutputReviewDataSourceValidator.SaveState';
begin
  Result := inherited SaveState;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOutputReviewDataSourceValidator.OutputReviewDataSourceDialog : TOutputReviewDataSourceDialog;
const OPNAME = 'TOutputReviewDataSourceValidator.OutputReviewDataSourceDialog';
begin
  Result := nil;
  try
    if Assigned(FPanel) then
      Result := TOutputReviewDataSourceDialog(FPanel);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOutputReviewDataSourceValidator.StudyDataHasChanged(AContext: TChangeContext; AFieldName,AOldValue,ANewValue: string): boolean;
const OPNAME = 'TOutputReviewDataSourceValidator.StudyDataHasChanged';
begin
  Result := inherited StudyDataHasChanged(AContext,AFieldName,AOldValue,ANewValue);
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOutputReviewDataSourceValidator.StudyHasChanged: boolean;
const OPNAME = 'TOutputReviewDataSourceValidator.StudyHasChanged';
begin
  Result := inherited StudyHasChanged;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputReviewDataSourceValidator.OnEditControlEnter(Sender: TObject);
const OPNAME = 'TOutputReviewDataSourceValidator.OnEditControlEnter';
begin
  inherited OnEditControlEnter(Sender);
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputReviewDataSourceValidator.OnEditControltExit(Sender: TObject);
const OPNAME = 'TOutputReviewDataSourceValidator.OnEditControltExit';
begin
  inherited OnEditControltExit(Sender);
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputReviewDataSourceValidator.DoContextValidation(AValidationType: TDialogValidationType);
const OPNAME = 'TOutputReviewDataSourceValidator.DoContextValidation';
begin
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOutputReviewDataSourceValidator.DetermineWizardStatus (ASequence : integer = 0) : integer;
const OPNAME = 'TOutputReviewDataSourceValidator.DetermineWizardStatus';
begin
  Result := inherited DetermineWizardStatus(ASequence);
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputReviewDataSourceValidator.AvailableDataSourcesComboOnChange(Sender: TObject);
const OPNAME = 'TOutputReviewDataSourceValidator.AvailableDataSourcesComboOnChange';
begin
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputReviewDataSourceValidator.LoadFromSourceButtonOnClick( Sender: TObject);
const OPNAME = 'TOutputReviewDataSourceValidator.LoadFromSourceButtonOnClick';
var
  LIndex             : integer;
  LLoadAgent         : TOutputDataLoadAgent;
  LDataSourceName    : TSummaryOutputDataSourceName;
begin
  try
    LIndex := OutputReviewDataSourceDialog.AvailableDataSourcesCombo.ItemIndex;
    LDataSourceName :=
      TSummaryOutputDataSourceName(Integer(OutputReviewDataSourceDialog.AvailableDataSourcesCombo.Items.Objects[LIndex]));

    LLoadAgent := TOutputDataLoadAgent.Create(FAppModules);
    try
      if LLoadAgent.ConstructSummaryOutputData(TYieldModelDataObject(FAppModules.Model.ModelData).OutputData,
         TYieldModelDataObject(FAppModules.Model.ModelData),LDataSourceName) then
      begin
         PopulateDataViewer;
      end;
    finally
      LLoadAgent.Free;
    end;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.

