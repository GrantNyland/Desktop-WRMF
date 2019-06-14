

unit UOutputComparisonFileSelectionDialog;

interface

uses
  Classes,
  VCL.Controls,
  VCL.ComCtrls,
  VCL.ExtCtrls,
  VCL.StdCtrls,
  VCL.Buttons,
  VCL.Graphics,
  VCL.Forms,
  UAbstractObject,
  UAbstractComponent,
  UDataEditComponent,
  UDataComponent;

type

  TOutputComparisonFileSelectionDialog = class(TAbstractScrollablePanel)
  protected
    FPnlBottom              : TPanel;
    FgbEdits                : TGroupBox;

    FlblFirstSumOut : TLabel;
    FlblFirstAverageVolume : TLabel;

    FedtFirstSumOut : TFieldEdit;
    FbtnSearchFirstSumOut : TButton;
    FbtnResetSumOut : TButton;

    FlblSecondSumOut : TLabel;
    FlblSecondAverageVolume : TLabel;

    FedtSecondSumOut : TFieldEdit;
    FbtnSearchSecondSumOut : TButton;
    FbtnRefreshSecondSumOut : TButton;

    FstrgFirstReservoirs : TFieldCheckListStringGrid;
    FstrgSecondReservoirs : TFieldCheckListStringGrid;

    FstrgFirstChannel : TFieldCheckListStringGrid;
    FstrgSecondChannel : TFieldCheckListStringGrid;
    procedure CreateMemberObjects; override;
    procedure AssignHelpContext; override;
  public
    procedure Resize; override;
    procedure RestoreColourState; override;
    function LanguageHasChanged: boolean; override;
    function Initialise: boolean; override;
    property edtFirstSumOut : TFieldEdit read FedtFirstSumOut;
    property btnSearchFirstSumOut : TButton read FbtnSearchFirstSumOut;
    property btnResetSumOut : TButton read FbtnResetSumOut;
    property edtSecondSumOut : TFieldEdit read FedtSecondSumOut;
    property btnSearchSecondSumOut : TButton read FbtnSearchSecondSumOut;
    property btnRefreshSecondSumOut : TButton read FbtnRefreshSecondSumOut;
    property strgFirstReservoirs : TFieldCheckListStringGrid read FstrgFirstReservoirs;
    property strgSecondReservoirs : TFieldCheckListStringGrid read FstrgSecondReservoirs;
    property strgFirstChannel : TFieldCheckListStringGrid read FstrgFirstChannel;
    property strgSecondChannel : TFieldCheckListStringGrid read FstrgSecondChannel;



   end;

implementation

uses
  VCL.Grids,
  SysUtils,
  UHelpContexts,
  UErrorHandlingOperations,
  UControlCreationUtilities;


procedure TOutputComparisonFileSelectionDialog.CreateMemberObjects;
const OPNAME = 'TOutputComparisonFileSelectionDialog.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
    FPnlBottom                       := TPanel.Create(ControlsOwner);
    FPnlBottom.Parent                := ControlsParent;

    FgbEdits                         := TGroupBox.Create(ControlsOwner);
    FgbEdits.Parent                  := ControlsParent;

    FlblFirstSumOut                  := TLabel.Create(ControlsOwner);
    FlblFirstSumOut.Parent           := FgbEdits;

    FedtFirstSumOut                  := TFieldEdit.Create(ControlsOwner,FAppModules);
    FedtFirstSumOut.Parent           := FgbEdits;

    FbtnSearchFirstSumOut            := TButton.Create(ControlsOwner);
    FbtnSearchFirstSumOut.Parent     := FgbEdits;


    FbtnResetSumOut                  := TButton.Create(ControlsOwner);
    FbtnResetSumOut.Parent           := FgbEdits;

    FlblSecondSumOut                 := TLabel.Create(ControlsOwner);
    FlblSecondSumOut.Parent          := FgbEdits;

    FedtSecondSumOut                 := TFieldEdit.Create(ControlsOwner,FAppModules);
    FedtSecondSumOut.Parent          := FgbEdits;


    FbtnSearchSecondSumOut           := TButton.Create(ControlsOwner);
    FbtnSearchSecondSumOut.Parent    := FgbEdits;

    FbtnRefreshSecondSumOut          := TButton.Create(ControlsOwner);
    FbtnRefreshSecondSumOut.Parent   := FgbEdits;

    FstrgFirstReservoirs             := TFieldCheckListStringGrid.Create(ControlsOwner,FAppModules);
    FstrgFirstReservoirs.Parent      := FPnlBottom;

    FstrgSecondReservoirs            := TFieldCheckListStringGrid.Create(ControlsOwner,FAppModules);
    FstrgSecondReservoirs.Parent     := FPnlBottom;

    FstrgFirstChannel                := TFieldCheckListStringGrid.Create(ControlsOwner,FAppModules);
    FstrgFirstChannel.Parent         := FPnlBottom;

    FstrgSecondChannel               := TFieldCheckListStringGrid.Create(ControlsOwner,FAppModules);
    FstrgSecondChannel.Parent        := FPnlBottom;



  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputComparisonFileSelectionDialog.Resize;
const OPNAME = 'TOutputComparisonFileSelectionDialog.Resize';
var
  LEditWidth,
  LLabelWidth,
  LUnitWidth : integer;
begin
  inherited Resize;
  try
    FgbEdits.Height                   := 40;
    FgbEdits.Align                    := alTop;
    FgbEdits.Width                    := Self.ClientWidth;
    LUnitWidth  := FgbEdits.Width div 3;
    LLabelWidth := LUnitWidth - (LUnitWidth div 4);
    LEditWidth  := (LUnitWidth div 2)+20;
    LLabelWidth := LLabelWidth - C_LabelOffset - C_LabelOffset;
    LEditWidth  := LEditWidth -1;

    FlblFirstSumOut.Top               := 15;
    FlblFirstSumOut.Left              := 10;
    FlblFirstSumOut.Width             := LLabelWidth;

    FedtFirstSumOut.Top               := 15;
    FedtFirstSumOut.Width             := LEditWidth;
    FedtFirstSumOut.Left              := FlblFirstSumOut.Left + FlblFirstSumOut.Width;

    FbtnSearchFirstSumOut.Top         := 11;
    FbtnSearchFirstSumOut.Width       := 25;
    FbtnSearchFirstSumOut.Left        := FedtFirstSumOut.Left + FedtFirstSumOut.Width + C_ControlOffset;

    FbtnResetSumOut.Top               := 11;
    FbtnResetSumOut.Width             := 50;

    FlblSecondSumOut.Top              := 15;
    FlblSecondSumOut.Left             := FbtnSearchFirstSumOut.Left + FbtnSearchFirstSumOut.Width + 40;

    FedtSecondSumOut.Top              := 15;
    FedtSecondSumOut.Left             := FlblSecondSumOut.Left + FlblSecondSumOut.Width + C_ControlOffset;
    FedtSecondSumOut.Width            := FedtFirstSumOut.Width;

    FbtnSearchSecondSumOut.Top        := 11;
    FbtnSearchSecondSumOut.Width      := 25;
    FbtnSearchSecondSumOut.Left       := FedtSecondSumOut.Left + FedtSecondSumOut.Width + C_ControlOffset;

    FbtnRefreshSecondSumOut.Top       := 11;
    FbtnRefreshSecondSumOut.Width     := 50;
    FbtnRefreshSecondSumOut.Left      := FgbEdits.Width - FbtnRefreshSecondSumOut.Width - C_ControlOffset; //FbtnSearchSecondSumOut.Left + FbtnSearchSecondSumOut.Width + C_ControlOffset;
    FbtnResetSumOut.Left              := FbtnRefreshSecondSumOut.Left - FbtnRefreshSecondSumOut.Width - C_ControlOffset;

    FPnlBottom.Width                := Self.ClientWidth;
    FPnlBottom.Height               := Self.ClientHeight - FPnlBottom.Height;
    LUnitWidth  := FPnlBottom.Width div 4;
    LLabelWidth := LUnitWidth - (LUnitWidth div 4);
    LLabelWidth := LLabelWidth - C_LabelOffset - C_LabelOffset;

    FstrgFirstReservoirs.Top          := 15;
    FstrgFirstReservoirs.Width        :=  FstrgFirstReservoirs.ColWidths[0] + (FstrgFirstReservoirs.ColWidths[1]+FstrgFirstReservoirs.ColWidths[2])+25;
    FstrgFirstReservoirs.Left         := 10;//LLabelWidth -(C_ControlOffset);
    FstrgFirstReservoirs.Height       := (FPnlBottom.Height div 2)-C_ControlOffset;

    FstrgSecondReservoirs.Top         := 15;
    FstrgSecondReservoirs.Width       := FstrgFirstReservoirs.Width;
    FstrgSecondReservoirs.Left        := FstrgFirstReservoirs.Left + FstrgFirstReservoirs.Width + LLabelWidth-(C_ControlOffset*5);
    FstrgSecondReservoirs.Height      := (FPnlBottom.Height div 2)-C_ControlOffset;


    FstrgFirstChannel.Top             := FstrgSecondReservoirs.Height + 15 + C_ControlOffset;
    FstrgFirstChannel.Left            := FstrgFirstReservoirs.Left;
    FstrgFirstChannel.Width           := FstrgSecondReservoirs.Width;
    FstrgFirstChannel.Height          := (FPnlBottom.Height div 2)-(C_ControlOffset*3);


    FstrgSecondChannel.Top             := FstrgSecondReservoirs.Height + 15 + C_ControlOffset;
    FstrgSecondChannel.Left            := FstrgSecondReservoirs.Left;
    FstrgSecondChannel.Width           := FstrgSecondReservoirs.Width;
    FstrgSecondChannel.Height          := (FPnlBottom.Height div 2)-(C_ControlOffset*3);

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOutputComparisonFileSelectionDialog.Initialise: boolean;
const OPNAME = 'TOutputComparisonFileSelectionDialog.Initialise';
begin
  Result := inherited Initialise;
  try
    FstrgFirstReservoirs.BorderStyle      := bsSingle;
    FstrgFirstReservoirs.ColCount         := 3;
    FstrgFirstReservoirs.RowCount         := 2;
    FstrgFirstReservoirs.FixedCols        := 1;
    FstrgFirstReservoirs.FixedRows        := 1;
    FstrgFirstReservoirs.DefaultRowHeight := 20;
    FstrgFirstReservoirs.RowHeights[0]    := 35;
    FstrgFirstReservoirs.ColWidths[0]     := 20;
    FstrgFirstReservoirs.ColWidths[1]     := 250;
    FstrgFirstReservoirs.ColWidths[2]     := 60;
    FstrgFirstReservoirs.WrapHeaderText   := True;
    FstrgFirstReservoirs.Options          := [goColSizing,goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goEditing];


    FstrgSecondReservoirs.BorderStyle      := bsSingle;
    FstrgSecondReservoirs.ColCount         := 3;
    FstrgSecondReservoirs.RowCount         := 2 ;
    FstrgSecondReservoirs.FixedCols        := 1;
    FstrgSecondReservoirs.FixedRows        := 1;
    FstrgSecondReservoirs.DefaultRowHeight := 20;
    FstrgSecondReservoirs.RowHeights[0]    := 35;
    FstrgSecondReservoirs.ColWidths[0]     := 20;
    FstrgSecondReservoirs.ColWidths[1]     := 250;
    FstrgSecondReservoirs.ColWidths[2]     := 60;
    FstrgSecondReservoirs.WrapHeaderText   := True;
    FstrgSecondReservoirs.Options          := [goColSizing,goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goEditing];

    FstrgFirstChannel.BorderStyle      := bsSingle;
    FstrgFirstChannel.ColCount         := 3;
    FstrgFirstChannel.FixedCols        := 1;
    FstrgFirstChannel.FixedRows        := 1;
    FstrgFirstChannel.DefaultRowHeight := 20;
    FstrgFirstChannel.RowHeights[0]    := 35;
    FstrgFirstChannel.ColWidths[0]     := 20;
    FstrgFirstChannel.ColWidths[1]     := 250;
    FstrgFirstChannel.ColWidths[2]     := 60;
    FstrgFirstChannel.WrapHeaderText   := True;
    FstrgFirstChannel.Options          := [goColSizing,goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goEditing];

    FstrgSecondChannel.BorderStyle      := bsSingle;
    FstrgSecondChannel.ColCount         := 3;
    FstrgSecondChannel.FixedCols        := 1;
    FstrgSecondChannel.FixedRows        := 1;
    FstrgSecondChannel.DefaultRowHeight := 20;
    FstrgSecondChannel.RowHeights[0]    := 35;
    FstrgSecondChannel.ColWidths[0]     := 20;
    FstrgSecondChannel.ColWidths[1]     := 250;
    FstrgSecondChannel.ColWidths[2]     := 60;
    FstrgSecondChannel.WrapHeaderText   := True;
    FstrgSecondChannel.Options          := [goColSizing,goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goEditing];

    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOutputComparisonFileSelectionDialog.LanguageHasChanged: boolean;
const OPNAME = 'TOutputComparisonFileSelectionDialog.LanguageHasChanged';
begin
  Result := inherited LanguageHasChanged;
  try
    FPnlBottom.Align                    := alClient;
    FPnlBottom.BevelOuter               := bvNone;

    FgbEdits.Caption                   := '';
    FlblFirstSumOut.Caption            := 'Output 1';
    FlblSecondSumOut.Caption           := 'Output 2';

    FstrgFirstReservoirs.Cells[1,0]    := FAppModules.Language.GetString('GridHeading.ReservoirsDescription1');
    FstrgFirstReservoirs.Cells[2,0]    := FAppModules.Language.GetString('GridHeading.AverageVolume');
    FstrgSecondReservoirs.Cells[1,0]   := FAppModules.Language.GetString('GridHeading.ReservoirsDescription2');
    FstrgSecondReservoirs.Cells[2,0]   := FAppModules.Language.GetString('GridHeading.AverageVolume');

    FstrgFirstChannel.Cells[1,0]    := FAppModules.Language.GetString('GridHeading.ChannelDescription1');
    FstrgFirstChannel.Cells[2,0]    := FAppModules.Language.GetString('GridHeading.AverageFlow');
    FstrgSecondChannel.Cells[1,0]   := FAppModules.Language.GetString('GridHeading.ChannelDescription2');
    FstrgSecondChannel.Cells[2,0]   := FAppModules.Language.GetString('GridHeading.AverageFlow');
    FbtnResetSumOut.Caption         := FAppModules.Language.GetString('LabelCaption.Reset');
    FbtnRefreshSecondSumOut.Caption := FAppModules.Language.GetString('LabelCaption.Refresh');
    FbtnSearchSecondSumOut.Caption  := '...';
    FbtnSearchFirstSumOut.Caption   := '...';
    
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputComparisonFileSelectionDialog.RestoreColourState;
const OPNAME = 'TOutputComparisonFileSelectionDialog.RestoreColourState';
var
  LIndex : integer;
begin
  inherited RestoreColourState;
  try
    for LIndex := 0 to ControlsOwner.ComponentCount - 1 do
      if ControlsOwner.Components[LIndex].ClassName = TFieldEdit.ClassName then
        if TFieldEdit(ControlsOwner.Components[LIndex]).Color = clRed then
          TFieldEdit(ControlsOwner.Components[LIndex]).Color := clWindow;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputComparisonFileSelectionDialog.AssignHelpContext;
const OPNAME = 'TOutputComparisonFileSelectionDialog.AssignHelpContext';
begin
  try
  
  except on E: Exception do HandleError(E, OPNAME); end;
end;

end.
