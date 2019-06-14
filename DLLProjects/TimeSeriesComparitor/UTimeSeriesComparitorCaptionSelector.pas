//
//
//  UNIT      : Contains TTimeSeriesComparitorCaptionSelector Classes
//  AUTHOR    : Dziedzi Ramulondi(PDNA)
//  DATE      : 13/03/2003
//  COPYRIGHT : Copyright © 2003 DWAF
//
//
unit UTimeSeriesComparitorCaptionSelector;

interface

uses
  VCL.controls,
  Classes,
  VCL.Forms,
  VCL.Grids,
  VCL.ComCtrls,
  VCL.ExtCtrls,
  VCL.Buttons,
  UAbstractComponent;

type

  TTimeSeriesComparitorCaptionSelector = class(TAbstractForm)
  protected
    FStrgrdCaptions : TStringGrid;
    FPnlButtons     : TPanel;
    FBtnOK          : TBitBtn;
    FBtnCancel      : TBitBtn;
    //FChartCount     : Integer;
    procedure CreateMemberObjects; override;
    procedure AssignHelpContext; override;
  public
    procedure Resize; override;
    function LanguageHasChanged: boolean; override;
    function StudyHasChanged: boolean; override;
    function Initialise: boolean; override;
    procedure SetChartCount(AChartCount : Integer);
    property CaptionsGrid : TStringGrid read FStrgrdCaptions;
  end;

implementation
uses
  SysUtils,
  UTimeSeriesComparitorLinkClasses,
  UErrorHandlingOperations;

{ TTimeSeriesComparitorCaptionSelector }

procedure TTimeSeriesComparitorCaptionSelector.AssignHelpContext;
const OPNAME = 'TTimeSeriesComparitorCaptionSelector.AssignHelpContext';
begin
  inherited;

end;

procedure TTimeSeriesComparitorCaptionSelector.CreateMemberObjects;
const OPNAME = 'TTimeSeriesComparitorCaptionSelector.CreateMemberObjects';
begin
  inherited CreateMemberObjects;
  try
    FPnlButtons := TPanel.Create(Self);
    FBtnOK      := TBitBtn.Create(Self);
    FBtnCancel  := TBitBtn.Create(Self);
    FStrgrdCaptions := TStringGrid.Create(Self);

    FPnlButtons.Parent     := Self;
    FBtnOK.Parent          := FPnlButtons;
    FBtnCancel.Parent      := FPnlButtons;
    FStrgrdCaptions.Parent := Self;

    FBtnOK.ModalResult     := mrOK;
    FBtnCancel.ModalResult := mrCancel;

  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TTimeSeriesComparitorCaptionSelector.Resize;
const OPNAME = 'TTimeSeriesComparitorCaptionSelector.Resize';
begin
  inherited Resize;
  try
    FBtnOK.Left     :=  btnSpace;
    FBtnCancel.Left := FBtnOK.Left + FBtnOK.Width + btnSpace;

    FBtnOK.Top      := (FBtnOK.Parent.Height - FBtnOK.Height) div 2;
    FBtnCancel.Top  := (FBtnCancel.Parent.Height - FBtnCancel.Height) div 2;

    FStrgrdCaptions.DefaultColWidth := (FStrgrdCaptions.Width div FStrgrdCaptions.ColCount)-2 ;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TTimeSeriesComparitorCaptionSelector.Initialise: boolean;
const OPNAME = 'TTimeSeriesComparitorCaptionSelector.Initialise';
begin
  Result := False;
  try
    FPnlButtons.Height := 40;
    FBtnOK.Width       := 80;
    FBtnOK.Height      := 30;
    FBtnCancel.Width   := 80;
    FBtnCancel.Height  := 30;

    FStrgrdCaptions.ColCount         := 3;
    FStrgrdCaptions.RowCount         := 2;
    FStrgrdCaptions.FixedCols        := 1;
    FStrgrdCaptions.FixedCols        := 2;

    FStrgrdCaptions.DefaultRowHeight := 17;
    FStrgrdCaptions.Options  := [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goRowSizing,
                                goColSizing, goEditing, goTabs, goAlwaysShowEditor];

    FPnlButtons.BevelInner := bvLowered;


    FPnlButtons.Align     := alBottom;
    FStrgrdCaptions.Align := alClient;
    Result := True;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TTimeSeriesComparitorCaptionSelector.LanguageHasChanged: boolean;
const OPNAME = 'TTimeSeriesComparitorCaptionSelector.LanguageHasChanged';
begin
  Result := False;
  try

    //Self.Caption := 'Edit Chart Captions (Use Ctrl + Enter to create multiple lines)';
    Self.Caption := FAppModules.Language.GetString('TSCCaptionSelector.Form_Caption');
    FBtnOK.Caption := FAppModules.Language.GetString('TSCCaptionSelector.BtnOK_Caption');
    FBtnCancel.Caption := FAppModules.Language.GetString('TSCCaptionSelector.BtnCancel_Caption');
    FPnlButtons.Caption := '';
    FStrgrdCaptions.Cols[0].Strings[0] := FAppModules.Language.GetString('TSCCaptionSelector.Grid_Caption1');
    FStrgrdCaptions.Cols[1].Strings[0] := FAppModules.Language.GetString('TSCCaptionSelector.Grid_Caption2');
    FStrgrdCaptions.Cols[2].Strings[0] := FAppModules.Language.GetString('TSCCaptionSelector.Grid_Caption3');
    Result := True;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TTimeSeriesComparitorCaptionSelector.SetChartCount(AChartCount: Integer);
const OPNAME = 'TTimeSeriesComparitorCaptionSelector.SetChartCount';
begin
  try
    FStrgrdCaptions.RowCount := AChartCount + 1;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TTimeSeriesComparitorCaptionSelector.StudyHasChanged: boolean;
const OPNAME = 'TTimeSeriesComparitorCaptionSelector.StudyHasChanged';
begin
  Result := False;
  try
    Result := True;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

end.
