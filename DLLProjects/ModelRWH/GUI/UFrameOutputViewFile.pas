unit UFrameOutputViewFile;

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
  URWHDataObject;

type
  TfrmOutputViewFile = class(TFrame)
    redtFile: TRichEdit;
    Panel1: TPanel;
    rgViewFileText: TRadioGroup;
    rgViewFileExcel: TRadioGroup;
    pnlFilename: TPanel;
    procedure rgViewFileTextClick(Sender: TObject);
    procedure rgViewFileExcelClick(Sender: TObject);
  private
    { Private declarations }
    procedure ClearDialog;
  public
    { Public declarations }
    function Initialise : boolean;
    function AfterInitialise : boolean;
    function Finalise   : boolean;
    function LanguageHasChanged : boolean;
  end;

var
  frmOutputViewFile : TfrmOutputViewFile;

implementation

{$R *.dfm}

uses
  ShellApi,
  UUtilities,
  UErrorHandlingOperations;

{ TfrmOutputDaily }

function TfrmOutputViewFile.AfterInitialise: boolean;
const OPNAME = 'TfrmOutputDaily.AfterInitialise';
begin
  Result := False;
  try
    ClearDialog;
    rgViewFileText.ItemIndex := -1;
    rgViewFileExcel.ItemIndex := -1;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TfrmOutputViewFile.Finalise: boolean;
const OPNAME = 'TfrmOutputDaily.Finalise';
begin
  Result := False;
  try
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TfrmOutputViewFile.Initialise: boolean;
const OPNAME = 'TfrmOutputDaily.Initialise';
begin
  Result := False;
  try
    ClearDialog;
    rgViewFileText.ItemIndex := -1;
    rgViewFileExcel.ItemIndex := -1;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TfrmOutputViewFile.LanguageHasChanged: boolean;
const OPNAME = 'TfrmOutputDaily.LanguageHasChanged';
begin
  Result := False;
  try
    //FrameWork.LanguageManager.TranslateForm(Self);
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TfrmOutputViewFile.ClearDialog;
const OPNAME = 'TfrmOutputDaily.ClearDialog';
begin
  try
    pnlFilename.Caption := '';
    redtFile.Lines.Clear;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TfrmOutputViewFile.rgViewFileTextClick(Sender: TObject);
const OPNAME = 'TfrmOutputDaily.rgViewFileTextClick';
var
  LStation      : TRainfallStation;
  LFileName : string;
begin
  try
    ClearDialog;
    LFileName := '';
    LStation := RWHModelData.SelectedRainfallStation;
    if(LStation <> nil) then
    begin
      Case rgViewFileText.ItemIndex of
        0: LFileName := RWHModelData.GetRainfallStationOutputDailyFileName(LStation.StationNumber);
        1: LFileName := RWHModelData.GetRainfallStationOutputMonthlyFileName(LStation.StationNumber);
        2: LFileName := RWHModelData.GetRainfallStationOutputAnnualFileName(LStation.StationNumber);
      end;
    end;

    if FileExists(LFileName) then
      redtFile.Lines.LoadFromFile(LFileName)
    else
      redtFile.Lines.Add('File does not exist: '+LFileName);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TfrmOutputViewFile.rgViewFileExcelClick(Sender: TObject);
const OPNAME = 'TfrmOutputDaily.rgViewFileTextClick';
var
  LStation  : TRainfallStation;
  LFileName : string;
begin
  try
    LFileName := '';
    LStation := RWHModelData.SelectedRainfallStation;
    if(LStation <> nil) then
    begin
      Case rgViewFileExcel.ItemIndex of
        0: LFileName := RWHModelData.GetRainfallStationOutputDailyFileName(LStation.StationNumber);
        1: LFileName := RWHModelData.GetRainfallStationOutputMonthlyFileName(LStation.StationNumber);
        2: LFileName := RWHModelData.GetRainfallStationOutputAnnualFileName(LStation.StationNumber);
      end;
    end;

    if FileExists(LFileName) then
      ShellExecute(Self.Handle,'open',PChar(LFileName),'','',SW_SHOWNORMAL)
    else
      ShowMessage('File does not exist: '+LFileName);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
