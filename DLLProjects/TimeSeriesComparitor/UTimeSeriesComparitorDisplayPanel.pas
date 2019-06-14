//
//
//  UNIT      : Contains TTimeSeriesComparitorDisplayPanel Class
//  AUTHOR    : Dziedzi Ramulondi (PDNA)
//  DATE      : 2003/03/04
//  COPYRIGHT : Copyright © 2003 DWAF
//
//
unit UTimeSeriesComparitorDisplayPanel;

interface
uses
  Types,
  VCL.Grids,
  Classes,
  VCL.Controls,
  VCL.ComCtrls,
  VCL.StdCtrls,
  VCL.ExtCtrls,
  UAbstractComponent,
  UTimeSeriesComparitorLinkClasses,
  UTimeSeriesComparitorChartPanel,
  UTimeSeriesComparitorSelectorPanel;

type

  TTimeSeriesComparitorDisplayPanel = class(TAbstractPanel)
  protected
    FSelectorPanel: TTimeSeriesComparitorSelectorPanel;
    FChartPanel   : TAbstractChartPanel;
    procedure CreateMemberObjects; override;
  public
    function Initialise: boolean; override;
    function StudyHasChanged: boolean; override;
    function LanguageHasChanged: boolean; override;
    property SelectorsPanel : TTimeSeriesComparitorSelectorPanel read FSelectorPanel;
    property ChartPanel     : TAbstractChartPanel    read FChartPanel;
  end;

implementation
uses
  SysUtils,
  VCL.Graphics,
  //UTimeSeriesComparitorData,
  UErrorHandlingOperations;

{ TTimeSeriesComparitorDisplayPanel }

procedure TTimeSeriesComparitorDisplayPanel.CreateMemberObjects;
const OPNAME = 'TTimeSeriesComparitorDisplayPanel.CreateMemberObjects';
begin
  inherited CreateMemberObjects;
  try
    FSelectorPanel        := TTimeSeriesComparitorSelectorPanel.Create(Self,FAppModules);
    FChartPanel           := TTimeSeriesComparitorChartPanel.Create(Self,FAppModules);

    FSelectorPanel.Parent := Self;
    FChartPanel.Parent    := Self;

    FSelectorPanel.Align := alTop;
    FChartPanel.Align    := alClient;

    Self.BevelInner := PanelBevelInnerInActive;
    Self.BevelOuter := PanelBevelOuterInActive;
    Self.Caption := '';
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TTimeSeriesComparitorDisplayPanel.StudyHasChanged: boolean;
const OPNAME = 'TTimeSeriesComparitorDisplayPanel.StudyHasChanged';
begin
  Result := inherited StudyHasChanged;
  try
    FSelectorPanel.StudyHasChanged;
    FChartPanel.StudyHasChanged;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TTimeSeriesComparitorDisplayPanel.LanguageHasChanged: boolean;
const OPNAME = 'TTimeSeriesComparitorDisplayPanel.LanguageHasChanged';
begin
  Result := inherited LanguageHasChanged;
  try
    FSelectorPanel.LanguageHasChanged;
    FChartPanel.LanguageHasChanged;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TTimeSeriesComparitorDisplayPanel.Initialise: boolean;
const OPNAME = 'TTimeSeriesComparitorDisplayPanel.Initialise';
begin
  Result := inherited Initialise;
  try
    FSelectorPanel.Initialise;
    FChartPanel.Initialise;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

end.
