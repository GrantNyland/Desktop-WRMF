
unit UOutputDemandChannelsGridSummaryDialog;

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

  TOutputDemandChannelsGridSummaryDialog = class(TAbstractScrollablePanel)
  protected
    FSelectorPanel        : TPanel;

    FLeftPanel : TPanel;
    FRightpanel : TPanel;
    FSpliter : TSplitter;
    FHorizSpliter : TSplitter;

    FViewDataLabel        : TLabel;
    FViewDataType         : TFieldComboBox;

    FlsvDemandChannels : TFieldCheckListBox;
    FsgrdDemandChannels : TFieldStringGrid;
    FsgrdChannelSummary : TFieldStringGrid;

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

    property lsvDemandChannels : TFieldCheckListBox read FlsvDemandChannels;
    property sgrdDemandChannels : TFieldStringGrid read FsgrdDemandChannels;
    property sgrdChannelSummary : TFieldStringGrid read FsgrdChannelSummary;

    property ViewDataLabel : TLabel read FViewDataLabel;
    property ViewDataType : TFieldComboBox read FViewDataType;


  end;

  implementation

uses
  SysUtils,
  VCL.Graphics,
  UHelpContexts,
  UControlCreationUtilities,
  UErrorHandlingOperations, VCL.Grids;



{ TOutputDemandChannelsGridSummaryDialog }

procedure TOutputDemandChannelsGridSummaryDialog.CreateMemberObjects;
const OPNAME = 'TOutputDemandChannelsGridSummaryDialog.CreateMemberObjects';
begin
  inherited CreateMemberObjects;
  try
    FSelectorPanel                := TPanel.Create(ControlsOwner);
    FSelectorPanel.Parent         := ControlsParent;

    FViewDataLabel                := TLabel.Create(ControlsOwner);
    FViewDataLabel.Parent         := FSelectorPanel;
    FViewDataLabel.Alignment      := taCenter;
    FViewDataLabel.AutoSize       := False;

    FViewDataType                 := TFieldComboBox.Create(ControlsOwner,FAppModules);
    FViewDataType.Parent          := FSelectorPanel;

    FLeftPanel := TPanel.Create(ControlsOwner);
    FLeftPanel.Parent := ControlsParent;

    FRightpanel := TPanel.Create(ControlsOwner);
    FRightpanel.Parent := ControlsParent;

    FlsvDemandChannels := TFieldCheckListBox.Create(ControlsOwner,FAppModules);
    FlsvDemandChannels.Parent := FLeftPanel;

    FSpliter := TSplitter.Create(ControlsOwner);
    FSpliter.Parent := ControlsParent;

    FHorizSpliter := TSplitter.Create(ControlsOwner);
    FHorizSpliter.Parent := FRightpanel;

    FsgrdDemandChannels := TFieldStringGrid.Create(ControlsOwner,FAppModules);
    FsgrdDemandChannels.Parent := FRightpanel;

    FsgrdChannelSummary := TFieldStringGrid.Create(ControlsOwner,FAppModules);
    FsgrdChannelSummary.Parent := FRightpanel;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOutputDemandChannelsGridSummaryDialog.Initialise: boolean;
const OPNAME = 'TOutputDemandChannelsGridSummaryDialog.Initialise';
begin
  Result := inherited Initialise;
  try
    FsgrdDemandChannels.RowCount := 2;
    FsgrdDemandChannels.ColCount := 11;
    FsgrdDemandChannels.FixedCols := 0;
    FsgrdDemandChannels.FixedRows := 1;
    FsgrdDemandChannels.WrapHeaderText := True;

    FsgrdChannelSummary.RowCount := 2;
    FsgrdChannelSummary.ColCount := 7;
    FsgrdChannelSummary.FixedCols := 0;
    FsgrdChannelSummary.FixedRows := 1;
    FsgrdChannelSummary.WrapHeaderText := True;

    FSelectorPanel.BorderStyle := bsSingle;
    FSpliter.AutoSnap := True;
    FHorizSpliter.AutoSnap := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOutputDemandChannelsGridSummaryDialog.StudyHasChanged: boolean;
const OPNAME = 'TOutputDemandChannelsGridSummaryDialog.StudyHasChanged';
begin
  Result := inherited StudyHasChanged;
  try
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOutputDemandChannelsGridSummaryDialog.LanguageHasChanged: boolean;
const OPNAME = 'TOutputDemandChannelsGridSummaryDialog.LanguageHasChanged';
begin
  Result := inherited LanguageHasChanged;
  try
    FViewDataLabel.Caption    := FAppModules.Language.GetString('LabelText.ViewData');
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputDemandChannelsGridSummaryDialog.AssignHelpContext;
const OPNAME = 'TOutputDemandChannelsGridSummaryDialog.AssignHelpContext';
begin
  try

  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TOutputDemandChannelsGridSummaryDialog.Resize;
const OPNAME = 'TOutputDemandChannelsGridSummaryDialog.Resize';
begin
  try
    LockWindowUpdate(Self.Handle);
    try
      FSelectorPanel.Align          := alTop;
      FSelectorPanel.ClientHeight   := 30;

      FViewDataType.Align           := alLeft;
      FViewDataType.Width           := 160;

      FViewDataLabel.Align          := alLeft;
      FViewDataLabel.Width          := 60;
      FViewDataLabel.Layout         := tlCenter;

      FLeftPanel.Align := alLeft;
      FLeftPanel.Width := 150;

      FSpliter.Left := FLeftPanel.Width+5;

      FRightpanel.Left := FSpliter.Left+5;
      FRightpanel.Align := alClient;


      FsgrdDemandChannels.Align := alTop;
      FsgrdDemandChannels.Height := (ClientHeight div 2);
      FsgrdDemandChannels.Top := FSelectorPanel.Height+5;

      FHorizSpliter.Align   := alTop;
      FHorizSpliter.Top     := FsgrdChannelSummary.Top + FsgrdDemandChannels.Height;
      FHorizSpliter.Height  := 4;
      FHorizSpliter.Beveled := True;
      FsgrdChannelSummary.Top := FsgrdDemandChannels.Height + FHorizSpliter.Top+ FHorizSpliter.Height;
      FsgrdChannelSummary.Align := alClient;
      FlsvDemandChannels.Align := alClient;
    finally
      LockWindowUpdate(0);
    end;
    inherited Resize;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TOutputDemandChannelsGridSummaryDialog.DoExport(AFileName: string = '');
const OPNAME = 'TOutputDemandChannelsGridSummaryDialog.DoExport';
begin
  try
    FsgrdDemandChannels.DoExport;
    FsgrdChannelSummary.DoExport;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TOutputDemandChannelsGridSummaryDialog.DoPrint;
const OPNAME = 'TOutputDemandChannelsGridSummaryDialog.DoPrint';
begin
  try
    FsgrdDemandChannels.DoPrint('');
    FsgrdDemandChannels.DoPrint('');
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TOutputDemandChannelsGridSummaryDialog.CanExport: boolean;
const OPNAME = 'TOutputDemandChannelsGridSummaryDialog.CanExport';
begin
  Result := False;
  try
    Result := Assigned(FsgrdDemandChannels) and (FsgrdDemandChannels.Visible);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TOutputDemandChannelsGridSummaryDialog.CanPrint: boolean;
const OPNAME = 'TOutputDemandChannelsGridSummaryDialog.CanPrint';
begin
  Result := False;
  try
    Result := Assigned(FsgrdDemandChannels) and (FsgrdDemandChannels.Visible);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

end.
