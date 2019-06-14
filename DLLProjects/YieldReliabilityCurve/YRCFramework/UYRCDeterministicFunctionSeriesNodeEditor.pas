unit UYRCDeterministicFunctionSeriesNodeEditor;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Mask,

  UYRCDataObject, ExtCtrls;

type
  TPointAction = (actUpdate,actDelete,actCreate);
  TDisableOption = (doNone,doXValue,doYValue,doCountValue);
  TValidateFunctionPoint = procedure(APointValue: TXYZValue; var AValueWithError: integer) of object;

  TYRCDeterministicFunctionSeriesNodeEditor= class(TForm)
    Panel1: TPanel;
    btnCancel: TButton;
    btnSave: TButton;
    Panel3: TPanel;
    Label5: TLabel;
    Label6: TLabel;
    mePointX: TEdit;
    mePointY: TEdit;
    procedure btnSaveClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure mePointXEnter(Sender: TObject);
    procedure mePointXExit(Sender: TObject);
    procedure mePointXKeyPress(Sender: TObject; var Key: Char);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  protected
    FPointAction:TPointAction;
    FCurrentPoint: TXYZValue;
    FValidateFunctionPoint:TValidateFunctionPoint;
    procedure DisplayError(AValueWithError: integer);  virtual;
    procedure SetPointAction(APointAction:TPointAction); virtual;
    function ValidateData: boolean;
    function ObjectToScreen: boolean; virtual;
    function ScreenToObject: boolean; virtual;
    { Private declarations }
  public
    { Public declarations }
    procedure SetDisableOptions(ADisabled: array of TDisableOption);  virtual;
    procedure SetPositionCloseToCursor(CursorX,CursorY,OwnerLeft,OwnerTop,OwnerHeight,OwnerWidth: integer);
    property ValidateFunctionPoint:TValidateFunctionPoint read FValidateFunctionPoint write FValidateFunctionPoint;
    property CurrentPoint : TXYZValue read FCurrentPoint;
    property PointAction : TPointAction Read FPointAction write SetPointAction;
  end;

implementation

uses
  UErrorHandlingOperations;

{$R *.dfm}

procedure TYRCDeterministicFunctionSeriesNodeEditor.FormCreate(Sender: TObject);
const OPNAME = 'TYRCDeterministicFunctionSeriesNodeEditor.FormCreate';
begin
  try
    FPointAction := actUpdate;
    FCurrentPoint:= TXYZValue.Create;
    FValidateFunctionPoint := nil;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYRCDeterministicFunctionSeriesNodeEditor.FormDestroy(Sender: TObject);
const OPNAME = 'TYRCDeterministicFunctionSeriesNodeEditor.FormDestroy';
begin
  try
    FCurrentPoint.Free;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYRCDeterministicFunctionSeriesNodeEditor.FormShow(Sender: TObject);
const OPNAME = 'TYRCDeterministicFunctionSeriesNodeEditor.FormShow';
begin
  try
    ObjectToScreen;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYRCDeterministicFunctionSeriesNodeEditor.btnSaveClick(Sender: TObject);
const OPNAME = 'TYRCDeterministicFunctionSeriesNodeEditor.btnSaveClick';

begin
  try
    if ValidateData then
    begin
      if ScreenToObject then
        ModalResult := mrOk;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYRCDeterministicFunctionSeriesNodeEditor.ValidateData: boolean;
const OPNAME = 'TYRCDeterministicFunctionSeriesNodeEditor.ValidateData';
var
  LValueWithError: integer;
begin
  Result := False;
  try
    if Assigned(FValidateFunctionPoint) then
    begin
      if ScreenToObject then
      begin
        FValidateFunctionPoint(FCurrentPoint,LValueWithError);
        if(LValueWithError = 0) then
          Result := True
        else
          DisplayError(LValueWithError);
      end;
    end
    else
      Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYRCDeterministicFunctionSeriesNodeEditor.DisplayError(AValueWithError: integer);
const OPNAME = 'TYRCDeterministicFunctionSeriesNodeEditor.DisplayError';
begin
  try
    if (AValueWithError = 1) then
      mePointX.Color := clRed;
    if (AValueWithError = 2) then
      mePointY.Color := clRed;
   Self.Invalidate;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYRCDeterministicFunctionSeriesNodeEditor.mePointXEnter(Sender: TObject);
const OPNAME = 'TYRCDeterministicFunctionSeriesNodeEditor.mePointXEnter';
begin
  try
    if(Sender is TEdit) then
    begin
      TEdit(Sender).Color := clWindow;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYRCDeterministicFunctionSeriesNodeEditor.mePointXExit(Sender: TObject);
const OPNAME = 'TYRCDeterministicFunctionSeriesNodeEditor.mePointXExit';
begin
  try
    if (Self.ActiveControl <> btnCancel) then
      ValidateData;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYRCDeterministicFunctionSeriesNodeEditor.SetPositionCloseToCursor(CursorX,CursorY,
          OwnerLeft,OwnerTop,OwnerHeight,OwnerWidth: integer);
const OPNAME = 'TYRCDeterministicFunctionSeriesNodeEditor.SetPositionCloseToCursor';
begin
  try
    Self.Top  := CursorY;
    Self.Left := CursorX;
    if (Self.Left + Self.Width) > (OwnerLeft + OwnerWidth) then
        Self.Left := Self.Left - ((Self.Left + Self.Width) - (OwnerLeft + OwnerWidth));
    if (Self.Top + Self.Height) > (OwnerTop + OwnerHeight) then
        Self.Top := Self.Top - ((Self.Top + Self.Height) - (OwnerTop + OwnerHeight));
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYRCDeterministicFunctionSeriesNodeEditor.mePointXKeyPress(Sender: TObject; var Key: Char);
const OPNAME = 'TYRCDeterministicFunctionSeriesNodeEditor.mePointXKeyPress';
begin
  try
   if not (Key in ['0'..'9','.',#8]) then
     Key := #0;
   if (Key = '.') and (pos('.',TEdit(Sender).Text) > 0) then
     Key := #0;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYRCDeterministicFunctionSeriesNodeEditor.SetPointAction(APointAction: TPointAction);
const OPNAME = 'TYRCDeterministicFunctionSeriesNodeEditor.SetPointAction';
begin
  try
    Case APointAction of
      actUpdate:
        begin
          mePointX.Enabled := True;
          mePointY.Enabled := True;

        end;
      actDelete:
        begin
          mePointX.Enabled := False;
          mePointY.Enabled := False;
        end;
      actCreate:
        begin
          mePointX.Enabled := True;
          mePointY.Enabled := True;
        end;
    end;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYRCDeterministicFunctionSeriesNodeEditor.ObjectToScreen: boolean;
const OPNAME = 'TYRCDeterministicFunctionSeriesNodeEditor.ObjectToScreen';
begin
  Result := False;
  try
    if Assigned(FCurrentPoint) then
    begin
      mePointX.Text      := FormatFloat('###0.000',FCurrentPoint.XValue);
      mePointY.Text      := FormatFloat('###0.000',FCurrentPoint.YValue);
      Result := True;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYRCDeterministicFunctionSeriesNodeEditor.ScreenToObject: boolean;
const OPNAME = 'TYRCDeterministicFunctionSeriesNodeEditor.ScreenToObject';
begin
  Result := False;
  try
    if Assigned(FCurrentPoint) then
    begin
      FCurrentPoint.XValue := StrToFloat(mePointX.Text);
      FCurrentPoint.YValue := StrToFloat(mePointY.Text);
      Result := True;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYRCDeterministicFunctionSeriesNodeEditor.SetDisableOptions(ADisabled: array of TDisableOption);
const OPNAME = 'TYRCDeterministicFunctionSeriesNodeEditor.SetDisableOptions';
var
  LCount: integer;
begin
  try
    mePointX.Enabled := True;
    mePointY.Enabled := True;
    for LCount := Low(ADisabled) to High(ADisabled) do
    begin
       case ADisabled[LCount] of
         doXValue    : mePointX.Enabled := False;
         doYValue    : mePointY.Enabled := False;
       end;
    end;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYRCDeterministicFunctionSeriesNodeEditor.FormClose(Sender: TObject; var Action: TCloseAction);
const OPNAME = 'TYRCDeterministicFunctionSeriesNodeEditor.FormClose';
begin
  try
    mePointX.Color := clWindow;
    mePointY.Color := clWindow;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
