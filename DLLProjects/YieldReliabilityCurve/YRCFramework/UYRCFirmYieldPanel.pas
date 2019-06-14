//
//
//  UNIT      : Contains TYRCFirmYieldPanel Classes
//  AUTHOR    : Dziedzi Ramulondi(Aravia)
//  DATE      : 19/07/2006
//  COPYRIGHT : Copyright © 2006 DWAF
//
//
unit UYRCFirmYieldPanel;

interface
uses                                       
  Classes,
  VCL.Controls,
  VCL.Forms,
  VCL.Buttons,
  VCL.StdCtrls,
  VCL.ExtCtrls,
  VCL.ComCtrls,
  UAbstractComponent,
  UDataComponent;


type
  TYRCFirmYieldPanel = class(TAbstractPanel)
  protected
    FbtnSetYMax           : TButton;
    FbtnAssuranceInterval : TButton;
    FChartZoomLabel       : TLabel;
    FChartZoom            : TComboBox;
    procedure CreateMemberObjects; override;
    procedure AssignHelpContext; override;
  public
    function LanguageHasChanged: boolean; override;
    procedure Resize; override;
    property btnSetYMax                   : TButton                read FbtnSetYMax;
    property btnAssuranceInterval         : TButton                read FbtnAssuranceInterval;
    property ChartZoom                    : TComboBox              read FChartZoom;
    property ChartZoomLabel               : TLabel                 read FChartZoomLabel;
  end;

implementation

uses
  VCL.Graphics,
  SysUtils,
  UHelpContexts,
  UErrorHandlingOperations;

{ TYRCFirmYieldPanel }

procedure TYRCFirmYieldPanel.CreateMemberObjects;
const OPNAME = 'TYRCFirmYieldPanel.CreateMemberObjects';
begin
  inherited;
  try
    Self.BevelOuter              := bvRaised;
    Self.Height                  := 32;

    FbtnAssuranceInterval        := TButton.Create(Self);
    FbtnAssuranceInterval.Parent := Self;

    FbtnSetYMax                  := TButton.Create(Self);
    FbtnSetYMax.Parent           := Self;

    FChartZoomLabel              := TLabel.Create(Self);
    FChartZoomLabel.Parent       := Self;

    FChartZoom                   := TComboBox.Create(Self);
    FChartZoom.Parent            := Self;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYRCFirmYieldPanel.Resize;
const OPNAME = 'TYRCFirmYieldPanel.Resize';
begin
  inherited;
  try
    FbtnAssuranceInterval.Top    := 5;
    FbtnAssuranceInterval.Width  := (Self.ClientWidth div 10);
    FbtnAssuranceInterval.Left   := Self.Width - FbtnAssuranceInterval.Width -5;

    FbtnSetYMax.Top              := FbtnAssuranceInterval.Top;
    FbtnSetYMax.Width            := FbtnAssuranceInterval.Width;
    FbtnSetYMax.Left             := FbtnAssuranceInterval.Left - FbtnSetYMax.Width -5;

    FChartZoom.Left              := FbtnSetYMax.Left -  FChartZoom.Width - 5;
    FChartZoom.Top               := FbtnSetYMax.Top;

    FChartZoomLabel.Left         := FChartZoom.Left - FChartZoomLabel.Width - 5;
    FChartZoomLabel.Top          := FbtnSetYMax.Top + 5;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYRCFirmYieldPanel.LanguageHasChanged: boolean;
const OPNAME = 'TYRCFirmYieldPanel.LanguageHasChanged';
begin
  Result := inherited LanguageHasChanged;
  try
    FChartZoomLabel.Caption       := FAppModules.Language.GetString('TTargetDraftSelector.ZoomSelection');
    FChartZoom.Items.CommaText    := FAppModules.Language.GetString('TTargetDraftSelector.ViewToggle');
    FbtnSetYMax.Caption           := FAppModules.Language.GetString('ButtonCaption.Muximum');
    FbtnAssuranceInterval.Caption := FAppModules.Language.GetString('TTargetDraftSelector.Assurance');
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYRCFirmYieldPanel.AssignHelpContext;
const OPNAME = 'TYRCFirmYieldPanel.AssignHelpContext';
begin
  try
    SetControlHelpContext(Self,       HC_YieldReliabilityCurves);
    SetControlHelpContext(FChartZoom, HC_YieldReliabilityCurves);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
