{******************************************************************************}
{*  UNIT      : Contains the class TWeatherEventsDialog.                      *}
{*  AUTHOR    : Riana Steyn                                                   *}
{*  DATE      : 2005/04/20                                                    *}
{*  COPYRIGHT : Copyright © 2005 DWAF                                         *}
{******************************************************************************}

unit UWeatherEventsDialog;

interface

uses
  Classes,
  VCL.Controls,
  VCL.ComCtrls,
  VCL.ExtCtrls,
  VCL.StdCtrls,
  VCL.Buttons,
  VCL.Graphics,
  UAbstractObject,
  UAbstractComponent,
  UDataEditComponent,
  VCL.Grids,
  UDataComponent;

type

  TWeatherEventsDialog = class(TAbstractScrollablePanel)
  protected
    FPnlTop              : TAbstractPanel;
    FLblStartDate        : TLabel;
    FLblEndDate          : TLabel;
    FDtpStartDate        : TAbstractDateTimePicker;
    FDtpEndDate          : TAbstractDateTimePicker;
    FBtnRefresh          : TButton;
    FLblAreas            : TLabel;
    FCbxAreas            : TComboBox;
    FGrdWeatherEvents    : TFieldStringGrid;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    procedure AssignHelpContext; override;
  public
    procedure Resize; override;
    function LanguageHasChanged: boolean; override;
    function Initialise: boolean; override;
    property DtpStartDate     : TAbstractDateTimePicker read FDtpStartDate;
    property DtpEndDate       : TAbstractDateTimePicker read FDtpEndDate;
    property CbxAreas         : TComboBox               read FCbxAreas;
    property GrdWeatherEvents : TFieldStringGrid        read FGrdWeatherEvents;
    property BtnRefresh       : TButton                 read FBtnRefresh;

  end;

  implementation

uses
  SysUtils,
  Math,
  VCL.ImgList,
  UHelpContexts,
  UControlCreationUtilities,
  UErrorHandlingOperations;

{ TWeatherEventsDialog }

procedure TWeatherEventsDialog.CreateMemberObjects;
const OPNAME = 'TWeatherEventsDialog.CreateMemberObjects';
var
  lOwner  : TComponent;
begin
  inherited;
  try
    lOwner  := ControlsOwner;

    FPnlTop := TAbstractPanel.Create(lOwner, FAppModules);
    with FPnlTop do
    begin
      Parent     := Self;
      BevelOuter := bvNone;
      Height     := 40;
      TabOrder   := 0;
    end;
    FLblStartDate := TLabel.Create(lOwner);
    with FLblStartDate do
    begin
      Parent   := FPnlTop;
      Left     := 10;
      Top      := 14;
//      Width    := 52;
      Caption  := 'Start Date :';
    end;
    FDtpStartDate := TAbstractDateTimePicker.Create(lOwner, FAppModules);
    with FDtpStartDate do
    begin
      Parent   := FPnlTop;
      Left     := 70;
      Top      := 10;
      Width    := 90;
      Height   := 21;
      TabOrder := 0;
      MinDate  := 0;
      MaxDate  := Date;
    end;
    FLblEndDate := TLabel.Create(lOwner);
    with FLblEndDate do
    begin
      Parent   := FPnlTop;
      Left     := 170;
      Top      := 14;
      Width    := 49;
      Caption  := 'End Date :';
    end;
    FDtpEndDate := TAbstractDateTimePicker.Create(lOwner, FAppModules);
    with FDtpEndDate do
    begin
      Parent   := FPnlTop;
      Left     := 230;
      Top      := 10;
      Width    := 90;
      Height   := 21;
      TabOrder := 1;
      MinDate  := 0;
      MaxDate  := Date;
    end;
    FLblAreas := TLabel.Create(Self);
    with FLblAreas do
    begin
      Parent   := FPnlTop;
      Left     := 330;
      Top      := 14;
      Width    := 40;
      Caption  := 'Area :';
    end;
    FCbxAreas := TComboBox.Create(Self);
    with FCbxAreas do
    begin
      Parent   := FPnlTop;
      Left     := 370;
      Top      := 10;
      Width    := 180;
      TabOrder := 2;
      Style    := csDropDownList;
    end;
    FBtnRefresh := TButton.Create(Self);
    with FBtnRefresh do
    begin
      Parent   := FPnlTop;
      Left     := 570;
      Top      := 10;
      Width    := 80;
      Height   := 21;
      TabOrder := 3;
      Caption  := 'Refresh';
    end;
    FGrdWeatherEvents := TFieldStringGrid.Create(lOwner, FAppModules);
    with FGrdWeatherEvents do
    begin
      Parent           := Self;
      ColCount         := 6;
      RowCount         := 1;
      DefaultColWidth  := 60;
      DefaultRowHeight := 20;
      DefaultColWidth  := 50;
      DefaultRowHeight := 20;
      TabOrder         := 3;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TWeatherEventsDialog.DestroyMemberObjects;
const OPNAME = 'TWeatherEventsDialog.DestroyMemberObjects';
begin
  try
    inherited;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TWeatherEventsDialog.Initialise: boolean;
const OPNAME = 'TWeatherEventsDialog.Initialise';
begin
  Result := inherited Initialise;
  try
    FGrdWeatherEvents.ColWidths[0] := 30;
    FGrdWeatherEvents.ColWidths[1] := 80;
    FGrdWeatherEvents.ColWidths[2] := 80;
    FGrdWeatherEvents.ColWidths[3] := 80;
    FGrdWeatherEvents.ColWidths[4] := 80;
    FGrdWeatherEvents.ColWidths[5] := 300;

    FDtpStartDate.Date := Date;
    FDtpEndDate.Date   := Date;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TWeatherEventsDialog.Resize;
const OPNAME = 'TWeatherEventsDialog.Resize';
begin
  inherited Resize;
  try
    FPnlTop.Top   := 0;
    FPnlTop.Align := alTop;
    FGrdWeatherEvents.Align := alClient;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TWeatherEventsDialog.LanguageHasChanged: boolean;
const OPNAME = 'TWeatherEventsDialog.LanguageHasChanged';
begin
  Result := True;
  try
    FGrdWeatherEvents.Cells[0, 0] := FAppModules.Language.GetString('Weather.CellNr');
    FGrdWeatherEvents.Cells[1, 0] := FAppModules.Language.GetString('Weather.CellStartDate');
    FGrdWeatherEvents.Cells[2, 0] := FAppModules.Language.GetString('Weather.CellEndDate');
    FGrdWeatherEvents.Cells[3, 0] := FAppModules.Language.GetString('Weather.CellArea');
    FGrdWeatherEvents.Cells[5, 0] := FAppModules.Language.GetString('Weather.CellComment');
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TWeatherEventsDialog.AssignHelpContext;
const OPNAME = 'TWeatherEventsDialog.AssignHelpContext';
begin
  try
  except on E: Exception do HandleError(E, OPNAME); end;
end;

end.
