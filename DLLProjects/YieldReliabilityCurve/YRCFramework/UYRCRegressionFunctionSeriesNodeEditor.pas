unit UYRCRegressionFunctionSeriesNodeEditor;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls,

  UYRCDeterministicFunctionSeriesNodeEditor;

type
  TYRCRegressionFunctionSeriesNodeEditor = class(TYRCDeterministicFunctionSeriesNodeEditor)
    Label7: TLabel;
    mePointCount: TEdit;
    procedure mePointXExit(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  protected
    procedure DisplayError(AValueWithError: integer);  override;
    procedure SetPointAction(APointAction:TPointAction); override;
    function ObjectToScreen: boolean; override;
    function ScreenToObject: boolean; override;
    { Private declarations }
  public
    procedure SetDisableOptions(ADisabled: array of TDisableOption);  override;
    { Public declarations }
  end;

var
  YRCRegressionFunctionSeriesNodeEditor: TYRCRegressionFunctionSeriesNodeEditor;

implementation

uses
  UErrorHandlingOperations;

{$R *.dfm}

procedure TYRCRegressionFunctionSeriesNodeEditor.DisplayError(AValueWithError: integer);
const OPNAME = 'TYRCRegressionFunctionSeriesNodeEditor.DisplayError';
begin
  inherited DisplayError(AValueWithError);
  try
    if (AValueWithError = 3) then
      mePointCount.Color := clRed;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYRCRegressionFunctionSeriesNodeEditor.mePointXExit(Sender: TObject);
const OPNAME = 'TYRCRegressionFunctionSeriesNodeEditor.mePointXExit';
begin
  inherited;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYRCRegressionFunctionSeriesNodeEditor.ObjectToScreen: boolean;
const OPNAME = 'TYRCRegressionFunctionSeriesNodeEditor.ObjectToScreen';
begin
  Result := inherited ObjectToScreen;
  try
    if Result then
       mePointCount.Text  := FormatFloat('###0.000',FCurrentPoint.ZValue);

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYRCRegressionFunctionSeriesNodeEditor.ScreenToObject: boolean;
const OPNAME = 'TYRCRegressionFunctionSeriesNodeEditor.ScreenToObject';
begin
  Result := inherited ScreenToObject;
  try
    if Result then
      FCurrentPoint.ZValue := StrToFloat(mePointCount.Text);

  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYRCRegressionFunctionSeriesNodeEditor.SetDisableOptions(ADisabled: array of TDisableOption);
const OPNAME = 'TYRCRegressionFunctionSeriesNodeEditor.SetDisableOptions';
var
  LCount: integer;
begin
  inherited SetDisableOptions(ADisabled);
  try
    mePointCount.Enabled := True;
    for LCount := Low(ADisabled) to High(ADisabled) do
    begin
      if (ADisabled[LCount] = doCountValue) then
        mePointCount.Enabled := False;
    end;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYRCRegressionFunctionSeriesNodeEditor.SetPointAction(APointAction: TPointAction);
const OPNAME = 'TYRCRegressionFunctionSeriesNodeEditor.SetPointAction';
begin
  inherited SetPointAction(APointAction);
  try
    case APointAction of 
      actUpdate:
        begin
          mePointCount.Enabled := True;
        end;
      actDelete:
        begin
          mePointCount.Enabled := False;
        end;
      actCreate:
        begin
          mePointCount.Enabled := True;
        end;
    end;//case

  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYRCRegressionFunctionSeriesNodeEditor.FormClose(Sender: TObject; var Action: TCloseAction);
const OPNAME = 'TYRCRegressionFunctionSeriesNodeEditor.FormClose';
begin
  inherited;
  try
    mePointCount.Color := clWindow;

  except on E: Exception do HandleError(E, OPNAME) end;

end;

end.
