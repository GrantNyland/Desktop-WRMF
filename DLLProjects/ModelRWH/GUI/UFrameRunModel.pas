unit UFrameRunModel;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Variants,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.ComCtrls,
  Vcl.Grids,
  Vcl.StdCtrls,
  Vcl.ExtCtrls,
  UColourButtons,
  UAbstractObject,
  URWHDataObject,
  //URRWHSolver;
  URRWHSolver2;

type
  TfrmRunModel = class(TFrame)
    ProgressRichEdit: TRichEdit;
    ActionProgressBar: TProgressBar;
    pnlButtons: TPanel;
    btnStart: TColourBitBtn;
    btnStop: TColourBitBtn;
    btnPrint: TColourBitBtn;
    btnSave: TColourBitBtn;
    OpenDialog1: TOpenDialog;
    lblSelect: TLabel;
    procedure btnStopClick(Sender: TObject);
    procedure btnStartClick(Sender: TObject);
    procedure btnPrintClick(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
  private
    { Private declarations }
    FStop            : boolean;
    FErrorCount      : integer;
    FWarningCount    : integer;
    //FRainFallFileAgent : TRainFallFileAgent;
    FAppModules          : TAppModules;
    procedure SetAppModules(AAppModules : TAppModules);
    procedure SetButtonState;
    //function Get_RunConfigurationDataPerRainfallStation(AStation:TRainfallStation; ANodeData : TNodeData):TRunConfig;
    //function ValidateRainfallStation(AProvinceName: string;AStation: TRainfallStation; ARunConfig: TRWHRunConfig): boolean;
    procedure OnShowProgress(AProgress: String; AProgressType: TProgressType; var AStop: boolean; AUpdateSteps: boolean);
  public
    { Public declarations }
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    function Initialise : boolean;
    function Finalise   : boolean;
    function LanguageHasChanged : boolean;
    function StudyHasChanged : boolean;
    procedure TabHasChanged;
    property AppModules : TAppModules read FAppModules write SetAppModules;
  end;

var
  frmRunModel : TfrmRunModel;

implementation

{$R *.dfm}

uses
  UConstants,
  {UCountry,
  UProvincePerCountry,
  UWMAPerProvince,
  URainfallDistrictPerWMA,
  UStationPerDistrict,
  UFrameWork,}
  UUtilities,
  UErrorHandlingOperations;
{ TfrmRunModel }

procedure TfrmRunModel.AfterConstruction;
const OPNAME = 'TfrmRunModel.BeforeDestruction';
begin
  inherited;
  try
    {FRainFallFileAgent := TRainFallFileAgent.Create;
  }except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TfrmRunModel.BeforeDestruction;
const OPNAME = 'TfrmRunModel.BeforeDestruction';
begin
  inherited;
  try
    {FreeAndNil(FRainFallFileAgent);
  }except on E: Exception do HandleError(E, OPNAME) end;
end;

function TfrmRunModel.Initialise: boolean;
const OPNAME = 'TfrmRunModel.Initialise';
begin
  Result := False;
  try
    SetButtonState;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TfrmRunModel.Finalise: boolean;
const OPNAME = 'TfrmRunModel.Finalise';
begin
  Result := False;
  try
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TfrmRunModel.SetAppModules(AAppModules: TAppModules);
const OPNAME = 'TfrmRunModel.SetAppModules';
begin
  try
    FAppModules := AAppModules;
    //if(AAppModules <> nil) then
     // FEditRunConfig  := TRWHRunConfig.Create(FAppModules);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TfrmRunModel.LanguageHasChanged: boolean;
const OPNAME = 'TfrmRunModel.LanguageHasChanged';
begin
  Result := False;
  try
    //FrameWork.LanguageManager.TranslateForm(Self);
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TfrmRunModel.StudyHasChanged: boolean;
const OPNAME = 'TfrmRunModel.StudyHasChanged';
begin
  Result := False;
  try
    //FrameWork.LanguageManager.TranslateForm(Self);
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TfrmRunModel.TabHasChanged;
const OPNAME = 'TfrmRunModel.TabHasChanged';
begin
  try
    if(RWHModelData.SelectedRunConfig <> nil) then
      lblSelect.Caption := 'Configuration Used in Run: '+RWHModelData.SelectedRunConfig.RunName
    else
      lblSelect.Caption := 'Configuration Used in Run: None';
    SetButtonState;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TfrmRunModel.btnStartClick(Sender: TObject);
const OPNAME = 'TfrmRunModel.btnStartClick';
var
  LStop           : boolean;
  LCursor         : TCursor;
  LIndex          : integer;
  LFileName       : string;
  LStation        : TRainfallStation;
  LRunConfig      : TRWHRunConfig;
  LSolver         : TSolver;
begin
  try
    SetButtonState;
    btnStart.Enabled           := False;
    btnStop.Enabled            := True;
    FStop                      := False;
    FErrorCount                := 0;
    FWarningCount              := 0;
    ActionProgressBar.Position := 0;
    ActionProgressBar.Min      := 0;
    ProgressRichEdit.Clear;
    LCursor                    := Screen.Cursor;
    Screen.Cursor              := crHourGlass;
    LSolver   := TSolver.Create(FAppModules);
    try
      RWHModelData.ClearInputFiles;
      RWHModelData.ClearOutputFiles;

      if(RWHModelData.SelectedRainfallStationList.Count = 0) then
      begin
        OnShowProgress('There are no selected Rainfall stations under the selected tree node.',ptWarning,LStop,False);
      end
      else
      begin
        ActionProgressBar.Max := RWHModelData.SelectedRainfallStationList.Count;
        ActionProgressBar.Position := 0;
        Application.ProcessMessages;
        for LIndex := 0 to RWHModelData.SelectedRainfallStationList.Count-1 do
        begin
          if FStop then Break;
          ActionProgressBar.Position := ActionProgressBar.Position + 1;
          LStation                   := RWHModelData.SelectedRainfallStationList.RainfallStationByIndex[LIndex];
          LRunConfig                 := RWHModelData.SelectedRunConfig;
          RWHModelData.CreateDailyDataInputFile(LStation.StationNumber);

          LFileName               := RWHModelData.GetRainfallStationInputFileName(LStation.StationNumber);
          if not FileExists(LFileName) then
            OnShowProgress('There in no rainfall file for station : '+LStation.StationNumber,ptWarning,LStop,False)
          else if(LRunConfig = nil) then
            OnShowProgress(': There is no run configuration data selected for the run.',ptWarning,LStop,False)
          else if LStation.ExcludeFromRun then
              OnShowProgress(LStation.StationNumber+': Has been excluded from the run by user.',ptWarning,LStop,False)
          else
          begin
            FErrorCount      := 0;
            FWarningCount    := 0;
            //LPath := ExtractFilePath(LStation.OutputDailyFileName[LWMAPerProvince.ProvinceName]);
            //if not DirectoryExists(LPath) then ForceDirectories(LPath);
            //ValidateRainfallStation(LWMAPerProvince.ProvinceName,LStation,LRunConfig);
            if(FErrorCount = 0) then
              LSolver.Run(LRunConfig,LStation,OnShowProgress);
          end;
        end;
      end;
    finally
      FreeAndNil(LSolver);
      btnStop.Enabled  := False;
      Screen.Cursor := LCursor;
    end;
    SetButtonState;
    ShowMessage('Completed.');
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{function TfrmRunModel.Get_RunConfigurationDataPerRainfallStation(AStation: TRainfallStation; ANodeData : TNodeData): TRWHRunConfig;
const OPNAME = 'TfrmRunModel.Get_RunConfigurationDataPerRainfallStation';
var
  LCountry                : TCountry;
  LNodeData               : TNodeData;
  LProvincePerCountry     : TProvincePerCountry;
  LWMAPerProvince         : TWMAPerProvince;
  LRainfallDistrictPerWMA : TRainfallDistrictPerWMA;
begin
  Result := nil;
  try
    Result := FrameWork.DataManager.RunConfigList.RunConfigByID[strStation,IntToStr(AStation.StationID),AStation.StationNumber];
    if(Result = nil) then
    begin
      LNodeData := TNodeData(ANodeData.TreeNode.Parent.Data);
      LRainfallDistrictPerWMA := TRainfallDistrictPerWMA(LNodeData.DataObject);
      Result := FrameWork.DataManager.RunConfigList.RunConfigByID[strRainfallDistrict,IntToStr(LRainfallDistrictPerWMA.DistrictID),LRainfallDistrictPerWMA.DistrictName];
    end;
    if(Result = nil) then
    begin
      LNodeData := TNodeData(ANodeData.TreeNode.Parent.Parent.Data);
      LWMAPerProvince := TWMAPerProvince(LNodeData.DataObject);
      Result := FrameWork.DataManager.RunConfigList.RunConfigByID[strWaterManagementArea,IntToStr(LWMAPerProvince.WMAID),LWMAPerProvince.AreaName];
    end;
    if(Result = nil) then
    begin
      LNodeData := TNodeData(ANodeData.TreeNode.Parent.Parent.Parent.Data);
      LProvincePerCountry := TProvincePerCountry(LNodeData.DataObject);
      Result := FrameWork.DataManager.RunConfigList.RunConfigByID[strProvince,IntToStr(LProvincePerCountry.ProvinceID),LProvincePerCountry.ProvinceName];
    end;
    if(Result = nil) then
    begin
      LNodeData := TNodeData(ANodeData.TreeNode.Parent.Parent.Parent.Parent.Data);
      LCountry := TCountry(LNodeData.DataObject);
      Result := FrameWork.DataManager.RunConfigList.RunConfigByID[strCountry,IntToStr(LCountry.CountryID),LCountry.CountryName];
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TfrmRunModel.ValidateRainfallStation(AProvinceName: string;AStation: TRainfallStation; ARunConfig: TRWHRunConfig): boolean;
const OPNAME = 'TfrmRunModel.ValidateRainfallStation';
      StartValidationMsg='Started validating configuration data for rainfall station:(%s) Named: %s';
      FinishValidationMsg='Finished validating configuration data for rainfall station:(%s) Named: %s';
      DateLessMsg='  Run configuration start date %s is less rainfall start date %s for rainfall station : %s';
      DateMoreMsg='  Run configuration end date %s is more rainfall end date %s for rainfall station : %s';
      NotEnoughDataMsg='  Rainfall data does not cover at least %s years for rainfall station : %s';
var
  LStop      : boolean;
  LFileName     : string;
  LDate : TDate;
begin
  Result := False;
  try
    {LStop := False;
    LFileName     := AStation.RainfallFileName[AProvinceName];
    OnShowProgress(Format(StartValidationMsg,[AStation.StationNumber,AStation.StationName]),ptNone,LStop,False);
    if not AStation.Validate(OnShowProgress) then Exit;
    if not ARunConfig.Validate(OnShowProgress) then Exit;
    if FileExists(LFileName) then
    begin
      FRainFallFileAgent.LoadFile(LFileName);
      LDate := FRainFallFileAgent.StartNoNullDate;
      if(ARunConfig.Period_StartDate <> NullDateTime) and (ARunConfig.Period_StartDate < LDate) then
        OnShowProgress(Format(DateLessMsg,[DateToStr(ARunConfig.Period_StartDate),DateToStr(LDate),AStation.StationName]),ptError,LStop,False);
      LDate := FRainFallFileAgent.EndNoNullDate;
      if(ARunConfig.Period_EndDate <> NullDateTime) and (ARunConfig.Period_EndDate > LDate) then
        OnShowProgress(Format(DateMoreMsg,[DateToStr(ARunConfig.Period_EndDate),DateToStr(LDate),AStation.StationName]),ptError,LStop,False);
      if not AStation.RainfallDataSpanPeriod(AProvinceName,EnoughDataPeriod) then
        OnShowProgress(Format(NotEnoughDataMsg,[IntToStr(EnoughDataPeriod),AStation.StationName]),ptError,LStop,False);
    end;
    OnShowProgress(Format(FinishValidationMsg,[AStation.StationNumber,AStation.StationName]),ptNone,LStop,False);
    Result := Not LStop;
  except on E: Exception do HandleError(E, OPNAME) end;
end;}

procedure TfrmRunModel.btnStopClick(Sender: TObject);
const OPNAME = 'TfrmRunModel.btnStopClick';
begin
  try
    FStop := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TfrmRunModel.btnPrintClick(Sender: TObject);
const OPNAME = 'TfrmRunModel.btnPrintClick';
begin
  try
    ProgressRichEdit.Print('RWH_DSS Run Messages');
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TfrmRunModel.btnSaveClick(Sender: TObject);
const OPNAME = 'TfrmRunModel.btnSaveClick';
begin
  try
    if OpenDialog1.Execute then
      ProgressRichEdit.Lines.SaveToFile(OpenDialog1.FileName);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TfrmRunModel.SetButtonState;
const OPNAME = 'TfrmRunModel.SetButtonState';
begin
  try
    btnStart.Enabled := (RWHModelData.SelectedRunConfig  <> nil) and (RWHModelData.SelectedRainfallStationList.Count > 0);
    btnStop.Enabled  := False;
    btnPrint.Enabled := (ProgressRichEdit.Lines.Count > 0);
    btnSave.Enabled := (ProgressRichEdit.Lines.Count > 0);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TfrmRunModel.OnShowProgress(AProgress: String; AProgressType: TProgressType; var AStop: boolean; AUpdateSteps: boolean);
const OPNAME = 'TfrmRunModel.OnShowProgress';
begin
  try
    case AProgressType of
      ptError: begin
                 ProgressRichEdit.SelAttributes.Color := clRed;
                 FErrorCount := FErrorCount + 1;
               end;
      ptWarning : ProgressRichEdit.SelAttributes.Color := clTeal;
    else
      ProgressRichEdit.SelAttributes.Color := clBlack;
    end;

    if(Trim(AProgress) <> '') then
      ProgressRichEdit.Lines.Add(AProgress);
    if AUpdateSteps and (ActionProgressBar.Position < ActionProgressBar.Max) then
      ActionProgressBar.Position := ActionProgressBar.Position + 1;

    Application.ProcessMessages;
    AStop := FStop;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
