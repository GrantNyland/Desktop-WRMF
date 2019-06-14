//
//
//  UNIT      : Contains the class TOutputComplianceGridDialog.
//  AUTHOR    : Dziedzi Ramulondi (ARIVIA)
//  DATE      : 2005/05/03
//  COPYRIGHT : Copyright © 2005 DWAF
//
//

unit UOutputComplianceGridDialog;

interface

uses
  Classes,
  Windows,
  VCL.Controls,
  VCL.ComCtrls,
  VCL.ExtCtrls,
  VCL.StdCtrls,
  VCL.Buttons,
  VCL.CheckLst,
  VCL.Forms,
  UAbstractObject,
  UAbstractComponent,
  UDataEditComponent,
  UStringGridWithCellChange,
  UDataComponent;

type

  TOutputComplianceGridDialog = class(TAbstractScrollablePanel)
  protected
    FSelectorPanel     : TPanel;
    FViewDataType      : TFieldComboBox;
    FViewDataLabel,
    FUnitsLabel        : TLabel;
    FBtnDataSelection  : TFieldButton;
    FMonthlyDeficitDataGrid : TMonthlyDeficitGrid;
    FSupplyComplianceDataGrid  : TSupplyComplianceGrid;
    FgrdIFRStatsComplianceData  : TIFRStatsComplianceGrid;
    FSupplyComplianceAggregateDataGrid  : TSupplyComplianceAggregateGrid;
    procedure CreateMemberObjects; override;
    procedure AssignHelpContext; override;
  public
    procedure Resize; override;
    procedure DoExport(AFileName: string = ''); override;
    procedure DoPrint; override;
    function CanExport : boolean; override;
    function CanPrint  : boolean; override;
    function StudyHasChanged: boolean; override;
    function LanguageHasChanged: boolean; override;
    function Initialise: boolean; override;

    property grdMonthlySupplyData : TMonthlyDeficitGrid   read FMonthlyDeficitDataGrid;
    property grdSupplyCompliance  : TSupplyComplianceGrid read FSupplyComplianceDataGrid;
    property grdIFRStatsComplianceData : TIFRStatsComplianceGrid read FgrdIFRStatsComplianceData;
    property cmbViewDataType      : TFieldComboBox        read FViewDataType;
    property BtnDataSelection     : TFieldButton          read FBtnDataSelection;
    property UnitsLabel           : TLabel                read FUnitsLabel;
    property grdSupplyComplianceAggregate  : TSupplyComplianceAggregateGrid read FSupplyComplianceAggregateDataGrid;
  end;

  implementation

uses
  SysUtils,
  VCL.Graphics,
  UHelpContexts,
  UControlCreationUtilities,
  UErrorHandlingOperations;



{ TOutputComplianceGridDialog }

procedure TOutputComplianceGridDialog.CreateMemberObjects;
const OPNAME = 'TOutputComplianceGridDialog.CreateMemberObjects';
begin
  inherited CreateMemberObjects;
  try
    FSelectorPanel              := TPanel.Create(ControlsOwner);
    FSelectorPanel.Parent       := ControlsParent;

    FViewDataLabel              := TLabel.Create(ControlsOwner);
    FViewDataLabel.Parent       := FSelectorPanel;
    FViewDataLabel.Alignment    := taCenter;
    FViewDataLabel.AutoSize     := False;

    FViewDataType               := TFieldComboBox.Create(ControlsOwner,FAppModules);
    FViewDataType.Parent        := FSelectorPanel;

    FUnitsLabel                 := TLabel.Create(ControlsOwner);
    FUnitsLabel.Parent          := FSelectorPanel;

    FBtnDataSelection           := TFieldButton.Create(ControlsOwner, FAppModules,'');
    FBtnDataSelection.Parent    := FSelectorPanel;

    FMonthlyDeficitDataGrid                      := TMonthlyDeficitGrid.Create(ControlsOwner, FAppModules);
    FMonthlyDeficitDataGrid.Parent               := ControlsParent;
    FMonthlyDeficitDataGrid.FixedCols            := 0;
    FMonthlyDeficitDataGrid.FixedRows            := 1;

    FSupplyComplianceDataGrid                    := TSupplyComplianceGrid.Create(ControlsOwner, FAppModules);
    FSupplyComplianceDataGrid.Parent             := ControlsParent;
    FSupplyComplianceDataGrid.FixedCols          := 0;
    FSupplyComplianceDataGrid.FixedRows          := 1;


    FgrdIFRStatsComplianceData                   := TIFRStatsComplianceGrid.Create(ControlsOwner, FAppModules);
    FgrdIFRStatsComplianceData.Parent            := ControlsParent;
    FgrdIFRStatsComplianceData.FixedCols         := 1;
    FgrdIFRStatsComplianceData.FixedRows         := 2;

    FSupplyComplianceAggregateDataGrid           := TSupplyComplianceAggregateGrid.Create(ControlsOwner, FAppModules);
    FSupplyComplianceAggregateDataGrid.Parent    := ControlsParent;
    FSupplyComplianceAggregateDataGrid.FixedCols := 0;
    FSupplyComplianceAggregateDataGrid.FixedRows := 1;

    //FMonthlyDeficitDataGrid.DisabledColor     := clBtnFace;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOutputComplianceGridDialog.Initialise: boolean;
const OPNAME = 'TOutputComplianceGridDialog.Initialise';
begin
  Result := inherited Initialise;
  try
    FViewDataType.Clear;
    FMonthlyDeficitDataGrid.Initialise;
    FSupplyComplianceDataGrid.Initialise;
    FgrdIFRStatsComplianceData.Initialise;
//    FgrdIFRStatsComplianceData.Visible := False;
    FSupplyComplianceAggregateDataGrid.Initialise;
    FSelectorPanel.BorderStyle := bsSingle;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOutputComplianceGridDialog.StudyHasChanged: boolean;
const OPNAME = 'TOutputComplianceGridDialog.StudyHasChanged';
begin
  Result := inherited StudyHasChanged;
  try
    FMonthlyDeficitDataGrid.StudyHasChanged;
    FSupplyComplianceDataGrid.StudyHasChanged;
    FgrdIFRStatsComplianceData.StudyHasChanged;
    FSupplyComplianceAggregateDataGrid.StudyHasChanged;
    FViewDataType.StudyHasChanged;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOutputComplianceGridDialog.LanguageHasChanged: boolean;
const OPNAME = 'TOutputComplianceGridDialog.LanguageHasChanged';
begin
  Result := inherited LanguageHasChanged;
  try
    FMonthlyDeficitDataGrid.LanguageHasChanged;
    FSupplyComplianceDataGrid.LanguageHasChanged;
    FgrdIFRStatsComplianceData.LanguageHasChanged;
    FSupplyComplianceAggregateDataGrid.LanguageHasChanged;
    FViewDataType.LanguageHasChanged;
    FViewDataLabel.Caption     := FAppModules.Language.GetString('LabelText.ViewData');
    FUnitsLabel.Caption        := FAppModules.Language.GetString('MasterControl.M3perSecond');
    FBtnDataSelection.Caption  := FAppModules.Language.GetString('LabelText.SelectData');
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputComplianceGridDialog.AssignHelpContext;
const OPNAME = 'TOutputComplianceGridDialog.AssignHelpContext';
begin
  try
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TOutputComplianceGridDialog.Resize;
const OPNAME = 'TOutputComplianceGridDialog.Resize';
begin
  try
    LockWindowUpdate(Self.Handle);
    try
      FMonthlyDeficitDataGrid.Align     := alNone;
      FSupplyComplianceDataGrid.Align   := alNone;
      FgrdIFRStatsComplianceData.Align  := alNone;
      FSupplyComplianceAggregateDataGrid.Align   := alNone;

      FSelectorPanel.Align        := alTop;
      FSelectorPanel.ClientHeight := 30;

      FViewDataType.Align         := alLeft;
      FViewDataType.Width         := 300;

      FViewDataLabel.Align        := alLeft;
      FViewDataLabel.Width        := 60;
      FViewDataLabel.Layout       := tlCenter;

      FUnitsLabel.Top             := 5;
      FUnitsLabel.Left            := FViewDataType.Left + FViewDataType.Width + 5;
      FUnitsLabel.Width           := 60;
      FUnitsLabel.Font.Style      := [fsBold];

      FBtnDataSelection.Align     := alRight;
      FBtnDataSelection.Width     := 80;

      FMonthlyDeficitDataGrid.Align     := alClient;
      FSupplyComplianceDataGrid.Align   := alClient;
      FgrdIFRStatsComplianceData.Align  := alClient;
      FSupplyComplianceAggregateDataGrid.Align   := alClient;
    finally
      LockWindowUpdate(0);
    end;
    inherited Resize;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TOutputComplianceGridDialog.DoExport(AFileName: string = '');
const OPNAME = 'TOutputComplianceGridDialog.DoExport';
begin
  try
    if FMonthlyDeficitDataGrid.Visible then FMonthlyDeficitDataGrid.DoExport(AFileName);
    if FSupplyComplianceDataGrid.Visible then FSupplyComplianceDataGrid.DoExport(AFileName);
    if FgrdIFRStatsComplianceData.Visible then FgrdIFRStatsComplianceData.DoExport(AFileName);
    if FSupplyComplianceAggregateDataGrid.Visible then FSupplyComplianceAggregateDataGrid.DoExport(AFileName);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TOutputComplianceGridDialog.DoPrint;
const OPNAME = 'TOutputComplianceGridDialog.DoPrint';
begin
  try
    if FMonthlyDeficitDataGrid.Visible then FMonthlyDeficitDataGrid.DoPrint('');
    if FSupplyComplianceDataGrid.Visible then FSupplyComplianceDataGrid.DoPrint('');
    if FgrdIFRStatsComplianceData.Visible then FgrdIFRStatsComplianceData.DoPrint('');
    if FSupplyComplianceAggregateDataGrid.Visible then FSupplyComplianceAggregateDataGrid.DoPrint('');
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TOutputComplianceGridDialog.CanExport: boolean;
const OPNAME = 'TOutputComplianceGridDialog.CanExport';
begin
  Result := False;
  try
    Result := (Assigned(FMonthlyDeficitDataGrid)            and (FMonthlyDeficitDataGrid.Visible)) or
              (Assigned(FSupplyComplianceDataGrid)          and (FSupplyComplianceDataGrid.Visible)) or
              (Assigned(FgrdIFRStatsComplianceData)         and (FgrdIFRStatsComplianceData.Visible)) or
              (Assigned(FSupplyComplianceAggregateDataGrid) and (FSupplyComplianceAggregateDataGrid.Visible));
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TOutputComplianceGridDialog.CanPrint: boolean;
const OPNAME = 'TOutputComplianceGridDialog.CanPrint';
begin
  Result := False;
  try
    Result := (Assigned(FMonthlyDeficitDataGrid)            and (FMonthlyDeficitDataGrid.Visible)) or
              (Assigned(FSupplyComplianceDataGrid)          and (FSupplyComplianceDataGrid.Visible)) or
              (Assigned(FgrdIFRStatsComplianceData)         and (FgrdIFRStatsComplianceData.Visible)) or
              (Assigned(FSupplyComplianceAggregateDataGrid) and (FSupplyComplianceAggregateDataGrid.Visible));
  except on E: Exception do HandleError(E, OPNAME); end;
end;

end.
