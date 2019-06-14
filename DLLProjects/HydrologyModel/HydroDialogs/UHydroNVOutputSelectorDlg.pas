unit UHydroNVOutputSelectorDlg;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, VCL.Graphics, VCL.Controls, VCL.Forms,
  VCL.Dialogs, VCL.StdCtrls, xmldom, XMLIntf, XMLDoc, VCL.Menus, VCL.ExtCtrls,
  VCL.Buttons;

type
  THydroNVOutputSelectorDlg = class(TForm)
    FScrollBox          : TScrollBox;
    PnlInterval         : TPanel;
    LblInterval         : TLabel;
    PnlIntervalBottom   : TPanel;
    BtnFirstInterval    : TSpeedButton;
    BtnMinus12Intervals : TSpeedButton;
    BtnPreviousInterval : TSpeedButton;
    EdtInterval         : TEdit;
    BtnNextInterval     : TSpeedButton;
    BtnPlus12Intervals  : TSpeedButton;
    BtnLastInterval     : TSpeedButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure BtnFirstIntervalClick(Sender: TObject);
    procedure BtnMinus12IntervalsClick(Sender: TObject);
    procedure BtnPreviousIntervalClick(Sender: TObject);
    procedure BtnNextIntervalClick(Sender: TObject);
    procedure BtnPlus12IntervalsClick(Sender: TObject);
    procedure BtnLastIntervalClick(Sender: TObject);
  protected
    FOnFirstInterval    : TNotifyEvent;
    FOnMinus12Intervals : TNotifyEvent;
    FOnPreviousInterval : TNotifyEvent;
    FOnNextInterval     : TNotifyEvent;
    FOnPlus12Intervals  : TNotifyEvent;
    FOnLastInterval     : TNotifyEvent;
    procedure SetOnFirstInterval (const Value: TNotifyEvent);
    procedure SetOnMinus12Intervals (const Value: TNotifyEvent);
    procedure SetOnPreviousInterval (const Value: TNotifyEvent);
    procedure SetOnNextInterval (const Value: TNotifyEvent);
    procedure SetOnPlus12Intervals (const Value: TNotifyEvent);
    procedure SetOnLastInterval (const Value: TNotifyEvent);
  private
    FInterval : Integer;
    procedure SetInterval (AInterval : Integer);
  public
    property Interval           : Integer      read FInterval           write SetInterval;
    property OnFirstInterval    : TNotifyEvent read FOnFirstInterval    write SetOnFirstInterval;
    property OnMinus12Intervals : TNotifyEvent read FOnMinus12Intervals write SetOnMinus12Intervals;
    property OnPreviousInterval : TNotifyEvent read FOnPreviousInterval write SetOnPreviousInterval;
    property OnNextInterval     : TNotifyEvent read FOnNextInterval     write SetOnNextInterval;
    property OnPlus12Intervals  : TNotifyEvent read FOnPlus12Intervals  write SetOnPlus12Intervals;
    property OnLastInterval     : TNotifyEvent read FOnLastInterval     write SetOnLastInterval;
  end;


implementation

uses

  UErrorHandlingOperations;

{$R *.dfm}

procedure THydroNVOutputSelectorDlg.FormCreate(Sender: TObject);
const OPNAME = 'THydroNVOutputSelectorDlg.FormCreate';
begin
  try
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure THydroNVOutputSelectorDlg.FormDestroy(Sender: TObject);
const OPNAME = 'THydroNVOutputSelectorDlg.FormDestroy';
begin
  try
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure THydroNVOutputSelectorDlg.SetInterval (AInterval : Integer);
const OPNAME = 'THydroNVOutputSelectorDlg.SetInterval';
begin
  try
    FInterval := AInterval;
    EdtInterval.Text := IntToStr(FInterval);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure THydroNVOutputSelectorDlg.SetOnFirstInterval (const Value: TNotifyEvent);
const OPNAME = 'THydroNVOutputSelectorDlg.SetOnFirstInterval';
begin
  try
    FOnFirstInterval := Value;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure THydroNVOutputSelectorDlg.SetOnMinus12Intervals (const Value: TNotifyEvent);
const OPNAME = 'THydroNVOutputSelectorDlg.SetOnFirstInterval';
begin
  try
    FOnMinus12Intervals := Value;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure THydroNVOutputSelectorDlg.SetOnPreviousInterval (const Value: TNotifyEvent);
const OPNAME = 'THydroNVOutputSelectorDlg.SetOnFirstInterval';
begin
  try
    FOnPreviousInterval := Value;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure THydroNVOutputSelectorDlg.SetOnNextInterval (const Value: TNotifyEvent);
const OPNAME = 'THydroNVOutputSelectorDlg.SetOnFirstInterval';
begin
  try
    FOnNextInterval := Value;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure THydroNVOutputSelectorDlg.SetOnPlus12Intervals (const Value: TNotifyEvent);
const OPNAME = 'THydroNVOutputSelectorDlg.SetOnFirstInterval';
begin
  try
    FOnPlus12Intervals := Value;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure THydroNVOutputSelectorDlg.SetOnLastInterval (const Value: TNotifyEvent);
const OPNAME = 'THydroNVOutputSelectorDlg.SetOnFirstInterval';
begin
  try
    FOnLastInterval := Value;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure THydroNVOutputSelectorDlg.BtnFirstIntervalClick(Sender: TObject);
const OPNAME = 'THydroNVOutputSelectorDlg.BtnFirstIntervalClick';
begin
  try
    if Assigned(FOnFirstInterval) then
      FOnFirstInterval(Sender);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure THydroNVOutputSelectorDlg.BtnMinus12IntervalsClick(Sender: TObject);
const OPNAME = 'THydroNVOutputSelectorDlg.BtnMinus12IntervalsClick';
begin
  try
    if Assigned(FOnMinus12Intervals) then
      FOnMinus12Intervals(Sender);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure THydroNVOutputSelectorDlg.BtnPreviousIntervalClick(Sender: TObject);
const OPNAME = 'THydroNVOutputSelectorDlg.BtnPreviousIntervalClick';
begin
  try
    if Assigned(FOnPreviousInterval) then
      FOnPreviousInterval(Sender);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure THydroNVOutputSelectorDlg.BtnNextIntervalClick(Sender: TObject);
const OPNAME = 'THydroNVOutputSelectorDlg.BtnNextIntervalClick';
begin
  try
    if Assigned(FOnNextInterval) then
      FOnNextInterval(Sender);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure THydroNVOutputSelectorDlg.BtnPlus12IntervalsClick(Sender: TObject);
const OPNAME = 'THydroNVOutputSelectorDlg.BtnPlus12IntervalsClick';
begin
  try
    if Assigned(FOnPlus12Intervals) then
      FOnPlus12Intervals(Sender);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure THydroNVOutputSelectorDlg.BtnLastIntervalClick(Sender: TObject);
const OPNAME = 'THydroNVOutputSelectorDlg.BtnLastIntervalClick';
begin
  try
    if Assigned(FOnLastInterval) then
      FOnLastInterval(Sender);
  except on E: Exception do HandleError(E, OPNAME) end;
end;


end.
