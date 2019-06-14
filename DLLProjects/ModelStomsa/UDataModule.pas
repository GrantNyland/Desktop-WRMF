unit UDataModule;

interface

uses
  Windows, Messages, SysUtils, Classes, VCL.Graphics, VCL.Controls, VCL.Forms, VCL.Dialogs,
  UAbstractObject,UStomsaData;

type
  TfmData = class(TDataModule)
  protected
    FAppModules  : TAppModules;
    FDataStorage : TStomsaData;
    procedure DataStorageIncAdd(Sender: TObject; FileName, FileDirectory: String);
    procedure DataStorageIncRemove(Sender: TObject; FileName, FileDirectory: String);
    procedure DataStorageMarginalFitted(Sender: TObject; FileName, FileDirectory: String);
    procedure DataStorageMarginalUnfitted(Sender: TObject; FileName, FileDirectory: String);
    procedure DataStorageTimeSeriesFitted(Sender: TObject; FileName, FileDirectory: String);
    procedure DataStorageTimeSeriesUnFitted(Sender: TObject; FileName, FileDirectory: String);
    procedure DataStorageFileLoadError(Sender: TObject;  ErrorList: TStringList);
    procedure DataStoragePARAMFileCreated(Sender: TObject);
    procedure DataStoragePARAMFileDestroyed(Sender: TObject);
    procedure DataStorageKeyGaugeChange(Sender: TObject; FileName, FileDirectory: String);
    procedure DataStorageFlowsAndStatsGenerated(Sender: TObject);
    procedure IncFilesChanged(FileName, Directory : string);
    procedure KeyGaugesChanged(FileName, Directory : string);
    procedure MarginalValuesChanged(FileName, Directory : string);
    procedure TimeSeriesValuesChanged(FileName, Directory : string);
    procedure PARAMFilesChanged;
    procedure StatisticsChanged;
  public
    constructor Create(AOwner: TComponent; AAppModules: TAppModules); reintroduce; virtual;
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    function ResetState: boolean; virtual;
    function Initialise: boolean; virtual;
    function LanguageHasChanged: boolean; virtual;
    function StudyHasChanged: boolean; virtual;
    property DataStorage: TStomsaData read FDataStorage;
  end;

var
  fmData: TfmData;

implementation

uses
  UStomsaMainForm,
  UMarginalForm,
  UTimeSeriesForm,
  UErrorForm,
  UKeyGaugesForm,
  UErrorHandlingOperations;

{$R *.DFM}

constructor TfmData.Create(AOwner: TComponent; AAppModules: TAppModules);
const OPNAME = 'TfmData.Create';
begin
  inherited Create(AOwner);
  try
    FAppModules := AAppModules;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TfmData.AfterConstruction;
const OPNAME = 'TfmData.AfterConstruction';
begin
  inherited;
  try
    FDataStorage                          := TStomsaData.Create(FAppModules);
    FDataStorage.OnIncAdd                 := DataStorageIncAdd;
    FDataStorage.OnIncRemove              := DataStorageIncRemove;
    FDataStorage.OnKeyGaugeChange         := DataStorageKeyGaugeChange;
    FDataStorage.OnMarginalFitted         := DataStorageMarginalFitted;
    FDataStorage.OnMarginalUnfitted       := DataStorageMarginalUnfitted;
    FDataStorage.OnTimeSeriesFitted       := DataStorageTimeSeriesFitted;
    FDataStorage.OnTimeSeriesUnFitted     := DataStorageTimeSeriesUnFitted;
    FDataStorage.OnFileLoadError          := DataStorageFileLoadError;
    FDataStorage.OnPARAMFileCreated       := DataStoragePARAMFileCreated;
    FDataStorage.OnPARAMFileDestroyed     := DataStoragePARAMFileDestroyed;
    FDataStorage.OnFlowsAndStatsGenerated := DataStorageFlowsAndStatsGenerated;
    //FDataStorage.
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TfmData.BeforeDestruction;
const OPNAME = 'TfmData.BeforeDestruction';
begin
  inherited;
  try
    FreeAndNil(FDataStorage);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TfmData.IncFilesChanged(FileName, Directory : string);
const OPNAME = 'TfmData.IncFilesChanged';
begin
  try
    if DataStorage.IncFileCount > 0 then
    begin
      fmStomsaMainForm.btnMarginal.Enabled := true;
      fmStomsaMainForm.btnKeyGauges.Enabled := true;
      fmStomsaMainForm.btnRunAutomatic.Enabled := true;
      fmStomsaMainForm.imgSelectDone.Show;
    end
    else
    begin
      //No INC files available - close the marginal form
      fmStomsaMainForm.btnMarginal.Enabled := False;
      fmStomsaMainForm.btnKeyGauges.Enabled := False;
      fmStomsaMainForm.btnRunAutomatic.Enabled := false;
      fmKeyGauges.Free;
      fmKeyGauges := nil;
      fmMarginal.Free;
      fmMarginal := nil;
      fmStomsaMainForm.imgSelectDone.Hide;
    end;

    MarginalValuesChanged(FileName,Directory);
    KeyGaugesChanged(FileName,Directory);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TfmData.KeyGaugesChanged(FileName, Directory : string);
const OPNAME = 'TfmData.KeyGaugesChanged';
begin
  try
    DataStorage.KeyGaugesHaveChanged := true;
    if (DataStorage.IncFileCount > 0) and (DataStorage.KeyGaugeCount > 0) then
    begin
      fmStomsaMainForm.btnRunAutomatic.Enabled := true;
      if DataStorage.TimeSeriesFittedCount > 0 then
        fmStomsaMainForm.btnParamFiles.Enabled := true;
      fmStomsaMainForm.imgKeyGaugesDone.Show;
    end
    else
    begin
      fmStomsaMainForm.btnRunAutomatic.Enabled := false;
      fmStomsaMainForm.btnParamFiles.Enabled := False;
      fmStomsaMainForm.imgKeyGaugesDone.Hide;
    end;

    PARAMFilesChanged;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TfmData.MarginalValuesChanged(FileName, Directory : string);
const OPNAME = 'TfmData.MarginalValuesChanged';
begin
  try
    DataStorage.MarginalDataHasChanged := true;
    fmStomsaMainForm.btnOutputManager.Enabled := false;
    if DataStorage.MarginalFittedCount > 0 then
    begin
      fmStomsaMainForm.btnTimeSeries.Enabled := true;
      if DataStorage.MarginalFittedCount = DataStorage.IncFileCount then
      begin
        fmStomsaMainForm.imgMarginalFitted.Show;
        fmStomsaMainForm.btnOutputManager.Enabled := true;
      end;
    end
    else
    begin
      fmStomsaMainForm.btnTimeSeries.Enabled := false;
      fmTimeSeries.Free;
      fmTimeSeries := nil;
      fmStomsaMainForm.imgMarginalFitted.Hide;
    end;
    fmStomsaMainForm.pnlTaskBar.Refresh;

    TimeSeriesValuesChanged(FileName,Directory);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TfmData.TimeSeriesValuesChanged(FileName, Directory : string);
const OPNAME = 'TfmData.TimeSeriesValuesChanged';
begin
  try
    DataStorage.TimeSeriesDataHasChanged := true;
    if (DataStorage.IncFileCount <> 0) and
       (DataStorage.IncFileCount = DataStorage.MarginalFittedCount) and
       (DataStorage.MarginalFittedCount = DataStorage.TimeSeriesFittedCount) and
       (DataStorage.KeyGaugeCount > 0) then
    begin
      fmStomsaMainForm.btnParamFiles.Enabled := true;
      fmStomsaMainForm.imgTimeSeriesFitted.Show;
    end
    else
    begin
      fmStomsaMainForm.btnParamFiles.Enabled := false;
      fmStomsaMainForm.imgTimeSeriesFitted.Hide;
    end;
    if (DataStorage.IncFileCount <> 0) and
       (DataStorage.IncFileCount = DataStorage.MarginalFittedCount) and
       (DataStorage.MarginalFittedCount = DataStorage.TimeSeriesFittedCount) then
      fmStomsaMainForm.imgTimeSeriesFitted.Show;
    fmStomsaMainForm.pnlTaskBar.Refresh;

    PARAMFilesChanged;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TfmData.PARAMFilesChanged;
const OPNAME = 'TfmData.PARAMFilesChanged';
begin
  try
    DataStorage.ParamDataHasChanged := true;
    if DataStorage.PARAMFileCreated then
    begin
      fmStomsaMainForm.imgPARAMFileCreated.Show;
      fmStomsaMainForm.btnSavePARAMFile.Enabled := true;
      fmStomsaMainForm.btnGenerate.Enabled := true;
      fmStomsaMainForm.btnRunAutomatic.Enabled := false;
    end
    else
    begin
      fmStomsaMainForm.imgPARAMFileCreated.Hide;
      fmStomsaMainForm.imgPARAMFileSaved.Hide;
      fmStomsaMainForm.btnSavePARAMFile.Enabled := false;
      fmStomsaMainForm.btnGenerate.Enabled := false;
      if (DataStorage.IncFileCount > 0) and (DataStorage.KeyGaugeCount > 0) then
        fmStomsaMainForm.btnRunAutomatic.Enabled := true;
    end;
    fmStomsaMainForm.pnlTaskBar.Refresh;

    StatisticsChanged;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TfmData.StatisticsChanged;
const OPNAME = 'TfmData.StatisticsChanged';
begin
  try
    //set the flag to say that correlation data has changed !!!!!!
    DataStorage.StochasticDataHasChanged := true;
    if (DataStorage.IncFileCount = DataStorage.StatisticsCalculatedCount) and
       (DataStorage.PARAMFileCreated) then
    begin
      fmStomsaMainForm.imgGenerated.Show;
      fmStomsaMainForm.btnCorrelate.Enabled := true;
      fmStomsaMainForm.btnRunAutomatic.Enabled := false;
    end
    else
    begin
      fmStomsaMainForm.imgGenerated.Hide;
      fmStomsaMainForm.btnCorrelate.Enabled := false;
      if DataStorage.KeyGaugeCount > 0 then
        fmStomsaMainForm.btnRunAutomatic.Enabled := true
      else
        fmStomsaMainForm.btnRunAutomatic.Enabled := false;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TfmData.DataStorageIncAdd(Sender: TObject; FileName, FileDirectory: String);
const OPNAME = 'TfmData.DataStorageIncAdd';
begin
  try
    IncFilesChanged(FileName, FileDirectory);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TfmData.DataStorageIncRemove(Sender: TObject; FileName, FileDirectory: String);
const OPNAME = 'TfmData.DataStorageIncRemove';
begin
  try
    IncFilesChanged(FileName, FileDirectory);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TfmData.DataStorageMarginalFitted(Sender: TObject; FileName,FileDirectory: String);
const OPNAME = 'TfmData.DataStorageMarginalFitted';
begin
  try
    MarginalValuesChanged(FileName, FileDirectory);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TfmData.DataStorageMarginalUnfitted(Sender: TObject; FileName, FileDirectory: String);
const OPNAME = 'TfmData.DataStorageMarginalUnfitted';
begin
  try
    MarginalValuesChanged(FileName,FileDirectory);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TfmData.DataStorageTimeSeriesFitted(Sender: TObject; FileName, FileDirectory: String);
const OPNAME = 'TfmData.DataStorageTimeSeriesFitted';
begin
  try
    TimeSeriesValuesChanged(FileName,FileDirectory);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TfmData.DataStorageTimeSeriesUnFitted(Sender: TObject; FileName, FileDirectory: String);
const OPNAME = 'TfmData.DataStorageTimeSeriesUnFitted';
begin
  try
    TimeSeriesValuesChanged(FileName,FileDirectory);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TfmData.DataStorageFileLoadError(Sender: TObject; ErrorList: TStringList);
const OPNAME = 'TfmData.DataStorageFileLoadError';
begin
  try
    if fmErrorReporting = nil then
    begin
      Application.CreateForm(TfmErrorReporting, fmErrorReporting);
    end
    else
    begin
      fmErrorReporting.FormStyle := fsNormal;
      fmErrorReporting.BringToFront;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TfmData.DataStoragePARAMFileCreated(Sender: TObject);
const OPNAME = 'TfmData.DataStoragePARAMFileCreated';
begin
  try
    PARAMFilesChanged;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TfmData.DataStoragePARAMFileDestroyed(Sender: TObject);
const OPNAME = 'TfmData.DataStoragePARAMFileDestroyed';
begin
  try
    PARAMFilesChanged;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TfmData.DataStorageKeyGaugeChange(Sender: TObject; FileName, FileDirectory: String);
const OPNAME = 'TfmData.DataStorageKeyGaugeChange';
begin
  try
    KeyGaugesChanged(FileName,FileDirectory);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TfmData.DataStorageFlowsAndStatsGenerated(Sender: TObject);
const OPNAME = 'TfmData.DataStorageFlowsAndStatsGenerated';
begin
  try
    StatisticsChanged;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TfmData.Initialise: boolean;
const OPNAME = 'TfmData.Initialise';
begin
  Result := False;
  try
    FDataStorage.Initialise;
    Result := True;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TfmData.LanguageHasChanged: boolean;
const OPNAME = 'TfmData.LanguageHasChanged';
begin
  Result := False;
  try
    Result := True;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TfmData.ResetState: boolean;
const OPNAME = 'TfmData.ResetState';
begin
  Result := False;
  try
    Result := True;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TfmData.StudyHasChanged: boolean;
const OPNAME = 'TfmData.StudyHasChanged';
begin
  Result := False;
  try
    FDataStorage.Initialise;
    Result := True;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

end.
