unit UFrameOutputSummary;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Variants,
  System.Classes,
  System.DateUtils,
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
  UOutputDailyFileAgent,
  URWHDataObject;

type
  TfrmOutputSummary = class(TFrame)
    gboxSupply: TGroupBox;
    strgrdSummary: TStringGrid;
    gbox: TGroupBox;
    lblCost: TLabel;
    edtCost: TEdit;
    btnCalculate: TButton;
    procedure btnCalculateClick(Sender: TObject);
  private
    { Private declarations }
    FOutputDailyFileAgent : TOutputDailyFileAgent;
    procedure PopulateDialog;
    procedure ClearDialog;
  public
    { Public declarations }
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    function Initialise : boolean;
    function AfterInitialise : boolean;
    function Finalise   : boolean;
    function LanguageHasChanged : boolean;
  end;

var
  frmOutputSummary : TfrmOutputSummary;

implementation

{$R *.dfm}

uses
  UUtilities,
  UConstants,
  UErrorHandlingOperations;

{ TfrmConfiguration }

procedure TfrmOutputSummary.AfterConstruction;
const OPNAME = 'TfrmOutputSummary.AfterConstruction';
begin
  inherited;
  try
    FOutputDailyFileAgent := TOutputDailyFileAgent.Create;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TfrmOutputSummary.BeforeDestruction;
const OPNAME = 'TfrmOutputSummary.BeforeDestruction';
begin
  inherited;
  try
    FreeAndNil(FOutputDailyFileAgent);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TfrmOutputSummary.Initialise: boolean;
const OPNAME = 'TfrmConfiguration.Initialise';
begin
  Result := False;
  try
    ClearDialog;
    FOutputDailyFileAgent.Initialise;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TfrmOutputSummary.Finalise: boolean;
const OPNAME = 'TfrmConfiguration.Finalise';
begin
  Result := False;
  try
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TfrmOutputSummary.AfterInitialise: boolean;
const OPNAME = 'TfrmOutputSummary.AfterInitialise';
begin
  Result := False;
  try
    PopulateDialog;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TfrmOutputSummary.ClearDialog;
const OPNAME = 'TfrmOutputSummary.AfterInitialise';
begin
  try
    strgrdSummary.RowCount := 2;
    strgrdSummary.Rows[1].Clear;
    edtCost.Text := '1.00';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TfrmOutputSummary.PopulateDialog;
const OPNAME = 'TfrmOutputSummary.PopulateDialog';
var
  LFileName     : string;
  LStation      : TRainfallStation;
  LIndex        : integer;
  LTankCount    : integer;
  LDaysTotal    : integer;
  LDaysSupplied : integer;
  LDaysDeficit  : integer;
  LReliability  : integer;
  LMultiplier   : integer;
begin
  try
    ClearDialog;
    LStation      :=  RWHModelData.SelectedRainfallStation;
    if(LStation = nil) then Exit;
    LFileName     := RWHModelData.GetRainfallStationOutputDailyFileName(LStation.StationNumber);
    if not FileExists(LFileName) then Exit;

    LMultiplier := 1000;

    FOutputDailyFileAgent.LoadFile(LFileName);
    LTankCount    := FOutputDailyFileAgent.OutputFileHeader.TankCount;
    if(LTankCount = 0) then Exit;
    strgrdSummary.RowCount := LTankCount+1;
    LDaysTotal := DaysBetween(FOutputDailyFileAgent.OutputFileHeader.FPeriod_EndDate, FOutputDailyFileAgent.OutputFileHeader.FPeriod_StartDate);
    for LIndex := 1 to LTankCount do
    begin
      LDaysSupplied := FOutputDailyFileAgent.OutputFileHeader.FDaysSupplied[LIndex];
      LDaysDeficit  := LDaysTotal - LDaysSupplied;
      LReliability   := Trunc((LDaysSupplied/LDaysTotal)*100);
      strgrdSummary.Cells[0,LIndex] := PadLeftString(FormatFloat('0.000',FOutputDailyFileAgent.OutputFileHeader.FTankSizes[LIndex]*LMultiplier),10);
      strgrdSummary.Cells[1,LIndex] := PadLeftString(IntToStr(LDaysTotal),10);
      strgrdSummary.Cells[2,LIndex] := PadLeftString(IntToStr(LDaysSupplied),10);
      strgrdSummary.Cells[3,LIndex] := PadLeftString(IntToStr(LDaysDeficit),10);
      strgrdSummary.Cells[4,LIndex] := PadLeftString(IntToStr(LReliability),10)+'%';
    end;
    btnCalculateClick(nil);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TfrmOutputSummary.LanguageHasChanged: boolean;
const OPNAME = 'TfrmConfiguration.LanguageHasChanged';
begin
  Result := False;
  try
    lblCost.Caption := lblCost.Caption  + ' ' + VolumeUnitName + ' : ' + SACurrencyUnit;
    strgrdSummary.Cells[0,0] := 'Tank Size('+VolumeUnitName+')';
    strgrdSummary.Cells[1,0] := 'Total Days';
    strgrdSummary.Cells[2,0] := 'Days Supplied';
    strgrdSummary.Cells[3,0] := 'Days not Supplied';
    strgrdSummary.Cells[4,0] := '% Days Supplied';
    strgrdSummary.Cells[5,0] := 'Total Savings';
    strgrdSummary.Cells[6,0] := 'Annual Savings';
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TfrmOutputSummary.btnCalculateClick(Sender: TObject);
const OPNAME = 'TfrmOutputSummary.btnCalculateClick';
var
  LIndex        : integer;
  LTankCount    : integer;
  LDaysSupplied : integer;
  LCost         : double;
  LCurr         : string;
  LPersonCount  : integer;
  LLitersPerPerson  : double;
  LSavings          : double;
  NumberOfYears     : integer;
  LMultiplier       : integer;
begin
  try
    for LIndex := 1 to strgrdSummary.RowCount-1 do
      strgrdSummary.Cells[5,LIndex] := '';

    //LCurr         := FrameWork.GlobalSettings.FormatSettings.CurrencyString + ' ';
    LCurr         := 'Rands' + ' ';
    LCost         := StrToFloatDef(edtCost.Text,0.0);
    LPersonCount  := FOutputDailyFileAgent.OutputFileHeader.FHouseHold_Members;
    LLitersPerPerson  := FOutputDailyFileAgent.OutputFileHeader.FHouseHold_DemandPP;

    LMultiplier := 1000;

    NumberOfYears := YearsBetween(FOutputDailyFileAgent.OutputFileHeader.FPeriod_EndDate,FOutputDailyFileAgent.OutputFileHeader.FPeriod_StartDate);

    LTankCount    := FOutputDailyFileAgent.OutputFileHeader.TankCount;
    if(LTankCount = 0) then Exit;
    for LIndex := 1 to LTankCount do
    begin
      LDaysSupplied := FOutputDailyFileAgent.OutputFileHeader.FDaysSupplied[LIndex];
      LSavings      := LDaysSupplied * LPersonCount * LLitersPerPerson * LMultiplier * LCost;
      strgrdSummary.Cells[5,LIndex] := LCurr + FormatFloat('0.00',LSavings);
      strgrdSummary.Cells[6,LIndex] := LCurr + FormatFloat('0.00',LSavings/NumberOfYears);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
