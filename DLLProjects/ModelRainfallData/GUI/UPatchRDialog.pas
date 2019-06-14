{******************************************************************************}
{*  UNIT      : Contains TPatchRDialog Class                                  *}
{*  AUTHOR    : RH Steyn(Cornastone)                                          *}
{*  DATE      : 18/05/2005                                                    *}
{*  COPYRIGHT : Copyright © 2005 DWAF                                         *}
{******************************************************************************}

unit UPatchRDialog;

interface

uses
  Classes,
  VCL.stdctrls,
  VCL.ComCtrls,
  VCL.extctrls,
  VCL.CheckLst,
  Math,
  VCL.Dialogs,
  VCL.Forms,
  SysUtils,
  Windows,
  VCL.Controls,
  VCL.Buttons,
  VCL.Samples.Spin,

  UDataComponent,
  UStringGridWithCellChange;

type
  TPatchRDialog = class(TAbstractScrollablePanel)
  protected
    FScrollBox                       : TScrollBox;
    FClassRGroupBox                  : TGroupBox;
    FShiftDataLabel                  : TLabel;
    FShiftDataRadioGroup             : TRadioGroup;
    FClassRInputFileLabel            : TLabel;
    FClassRInputFileEdit             : TEdit;
    FClassROutputFileLabel           : TLabel;
    FClassROutputFileEdit            : TEdit;
    FClassRViewInputFileButton       : TButton;
    FClassRViewOutputFileButton      : TButton;
    FClassRHeaderLabel               : TLabel;
    FClassRHeaderEdit                : TEdit;
    FClassRCurrentDatesLabel         : TLabel;
    FClassRChangeDatesLabel          : TLabel;
    FClassRChangeDatesRadioGroup     : TRadioGroup;
    FClassRFirstYearLabel            : TLabel;
    FClassRFirstYearComboBox         : TComboBox;
    FClassRLastYearLabel             : TLabel;
    FClassRLastYearComboBox          : TComboBox;
    FRunClassRHeaderLabel            : TLabel;
    FClassROption0Button             : TButton;
    FClassROption0Label              : TLabel;
    FClassROption1Button             : TButton;
    FClassROption1Label              : TLabel;
    FClassROption2ChkBox             : TCheckBox;
//    FClassROption2Label              : TLabel;
    FClassROption3Button             : TButton;
    FClassROption3Label              : TLabel;
    FClassROption4Button             : TButton;
    FClassROption4Label              : TLabel;
    FClassROption5Button             : TButton;
    FClassROption5Label              : TLabel;

    FPatchInfoGroupBox               : TGroupBox;
    FTargetStationLabel              : TLabel;
    FTargetStationValue              : TLabel;
    FPatchSourcesLabel               : TLabel;
    FPatchSourcesListBox             : TListBox;
    FPatchMultipleRadioGroup         : TRadioGroup;
    FNoSourcesLabel                  : TLabel;
    FClassRNotRunYetLabel            : TLabel;
    FPatchNotRunYetLabel             : TLabel;
    FRAWFilesDoesNotExistLabel       : TLabel;
    FPatchRInputFileDoesNotExistLabel: TLabel;
    FPatchChangedShape               : TShape;
    FPatchChangedLabel               : TLabel;
    FFileNotOnDiskShape              : TShape;
    FFileNotOnDiskLabel              : TLabel;
    FFileInvalidShape                : TShape;
    FFileInvalidLabel                : TLabel;
    FViewInfoLabel                   : TLabel;

    FPatchRGroupBox                  : TGroupBox;
    FPatchRInputFileLabel            : TLabel;
    FPatchRInputFileEdit             : TEdit;
    FNoOfSeasonsLabel                : TLabel;
    FNoOfSeasonsEdit                 : TSpinEdit;
    FSeasonsGrid                     : TStringGridWithCellChange;
    FPatchRCreateInputFileButton     : TButton;
    FPatchRViewInputFileButton       : TButton;

    FRunPatchRGroupBox               : TGroupBox;
    FPatchRPeriodLabel               : TLabel;
    FPatchRPeriodRadioGroup          : TRadioGroup;
    FPatchRFistYearLabel             : TLabel;
    FPatchRFirstYearComboBox         : TComboBox;
    FPatchRLastYearLabel             : TLabel;
    FPatchRLastYearComboBox          : TComboBox;
    FPatchRHeaderLabel               : TLabel;
    FPatchRHeaderEdit                : TEdit;
    FPatchROutputFileNameLabel       : TLabel;
    FPatchROutputFileNameComboBox    : TComboBox;
    FPatchRViewOutputFileButton      : TButton;
    FPatchRPrintFileNameLabel        : TLabel;
    FPatchRPrintFileNameEdit         : TEdit;
    FPatchRViewPrintFileButton       : TButton;
    FPatchRPlotFileNameLabel         : TLabel;
    FPatchRPlotFileNameEdit          : TEdit;
    FPatchRViewPlotFileButton        : TButton;
    FRunPatchROptionsLabel           : TLabel;
    FPatchRRunOptionsGrid            : TStringGridWithCellChange;
    FPatchRRunOption0Button          : TButton;
    FPatchRRunOption1Button          : TButton;
    FPatchRRunOption2Button          : TButton;
    FPatchRRunOption3Button          : TButton;
    FPatchRRunOption4Button          : TButton;

    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    procedure AssignHelpContext; override;
  public
    function Initialise : boolean; override;
    property ClassRInputFileEdit             : TEdit    read FClassRInputFileEdit;
    property ClassRViewInputFileButton       : TButton  read FClassRViewInputFileButton;
    property ClassROutputFileEdit            : TEdit    read FClassROutputFileEdit;
    property ClassRViewOutputFileButton      : TButton  read FClassRViewOutputFileButton;
    property ClassRHeaderEdit                : TEdit    read FClassRHeaderEdit;
    property ClassRChangeDatesRadioGroup     : TRadioGroup read FClassRChangeDatesRadioGroup ;
    property ClassROption0Button             : TButton     read FClassROption0Button;
    property ClassROption1Button             : TButton     read FClassROption1Button;
    property ClassROption2ChkBox             : TCheckBox   read FClassROption2ChkBox;
    property ClassROption3Button             : TButton     read FClassROption3Button;
    property ClassROption4Button             : TButton     read FClassROption4Button;
    property ClassROption5Button             : TButton     read FClassROption5Button;
    property NoOfSeasonsEdit                 : TSpinEdit   read FNoOfSeasonsEdit;
    property SeasonsGrid                     : TStringGridWithCellChange read FSeasonsGrid;
    property PatchRCreateInputFileButton     : TButton     read FPatchRCreateInputFileButton;
    property PatchRViewInputFileButton       : TButton     read FPatchRViewInputFileButton;
    property PatchMultipleRadioGroup         : TRadioGroup read FPatchMultipleRadioGroup;
    property PatchRPeriodRadioGroup          : TRadioGroup read FPatchRPeriodRadioGroup;
    property PatchROutputFileNameComboBox    : TComboBox   read FPatchROutputFileNameComboBox;
    property PatchRViewOutputFileButton      : TButton     read FPatchRViewOutputFileButton;
    property PatchRPrintFileNameEdit         : TEdit       read FPatchRPrintFileNameEdit;
    property PatchRViewPrintFileButton       : TButton     read FPatchRViewPrintFileButton;
    property PatchRPlotFileNameEdit          : TEdit       read FPatchRPlotFileNameEdit;
    property PatchRViewPlotFileButton        : TButton     read FPatchRViewPlotFileButton;
    property PatchRRunOption0Button          : TButton     read FPatchRRunOption0Button;
    property PatchRRunOption1Button          : TButton     read FPatchRRunOption1Button;
    property PatchRRunOption2Button          : TButton     read FPatchRRunOption2Button;
    property PatchRRunOption3Button          : TButton     read FPatchRRunOption3Button;
    property PatchRRunOption4Button          : TButton     read FPatchRRunOption4Button;
    property ClassRGroupBox                  : TGroupBox   read FClassRGroupBox;
    property PatchRGroupBox                  : TGroupBox   read FPatchRGroupBox;
    property RunPatchRGroupBox               : TGroupBox   read FRunPatchRGroupBox;
    property PatchInfoGroupBox               : TGroupBox   read FPatchInfoGroupBox;
    property PatchSourcesListBox             : TListBox    read FPatchSourcesListBox;
    property TargetStationValue              : TLabel      read FTargetStationValue;
    property PatchRInputFileEdit             : TEdit       read FPatchRInputFileEdit;
    property PatchRHeaderEdit                : TEdit       read FPatchRHeaderEdit;
    property ShiftDataRadioGroup             : TRadioGroup read FShiftDataRadioGroup;
    property ClassRFirstYearComboBox         : TComboBox   read FClassRFirstYearComboBox;
    property ClassRLastYearComboBox          : TComboBox   read FClassRLastYearComboBox;
    property PatchRFirstYearComboBox         : TComboBox   read FPatchRFirstYearComboBox;
    property PatchRLastYearComboBox          : TComboBox   read FPatchRLastYearComboBox;
    property ClassRFirstYearLabel            : TLabel      read FClassRFirstYearLabel;
    property ClassRLastYearLabel             : TLabel      read FClassRLastYearLabel;
    property ClassRCurrentDatesLabel         : TLabel      read FClassRCurrentDatesLabel;
    property RAWFilesDoesNotExistLabel       : TLabel      read FRAWFilesDoesNotExistLabel;
    property PatchRInputFileDoesNotExistLabel: TLabel      read FPatchRInputFileDoesNotExistLabel;
    property ClassRNotRunYetLabel            : TLabel      read FClassRNotRunYetLabel;
    property PatchNotRunYetLabel             : TLabel      read FPatchNotRunYetLabel;
    property NoSourcesLabel                  : TLabel      read FNoSourcesLabel;
    property PatchRFistYearLabel             : TLabel      read FPatchRFistYearLabel;
    property PatchRLastYearLabel             : TLabel      read FPatchRLastYearLabel;
end;

implementation

uses
  UErrorHandlingOperations,
  UHelpContexts,
  VCL.Graphics;

{ TPatchRDialog }

procedure TPatchRDialog.CreateMemberObjects;
const OPNAME = 'TPatchRDialog.CreateMemberObjects';
var
  LViewButton : String;
begin
  inherited CreateMemberObjects;
  try
    LViewButton := FAppModules.Language.GetString('ButtonCaption.View');
    FScrollBox := TScrollBox.Create(Self);
    with FScrollBox do
    begin
      Parent     := Self;
      Left       := 0;
      Top        := 80;
      Width      := 557;
      Height     := 336;
      Align      := alClient;
      BevelInner := bvNone;
      TabOrder   := 1;
    end;
    {* Patch Info *************************************************************}
    FPatchInfoGroupBox := TGroupBox.Create(Self);
    FPatchInfoGroupBox.Parent  := FScrollBox;
    FPatchInfoGroupBox.Left    := 10;
    FPatchInfoGroupBox.Top     := 5;
    FPatchInfoGroupBox.Width   := 880;
    FPatchInfoGroupBox.Height  := 135;
    FPatchInfoGroupBox.Caption := FAppModules.Language.GetString('GroupBoxCaption.PatchInformation');

    FTargetStationLabel := TLabel.Create(Self);
    FTargetStationLabel.Parent  := FPatchInfoGroupBox;
    FTargetStationLabel.Left    := 10;
    FTargetStationLabel.Top     := 20;
    FTargetStationLabel.Height  := 21;
    FTargetStationLabel.Caption := FAppModules.Language.GetString('LabelText.TargetGauge');

    FTargetStationValue := TLabel.Create(Self);
    FTargetStationValue.Parent  := FPatchInfoGroupBox;
    FTargetStationValue.Left    := 110;
    FTargetStationValue.Top     := 20;
    FTargetStationValue.Height  := 21;
    FTargetStationValue.Caption := FAppModules.Language.GetString('LabelText.TargetGaugeValue');

    FPatchSourcesLabel  := TLabel.Create(Self);
    FPatchSourcesLabel.Parent  := FPatchInfoGroupBox;
    FPatchSourcesLabel.Left    := 10;
    FPatchSourcesLabel.Top     := 40;
    FPatchSourcesLabel.Height  := 21;
    FPatchSourcesLabel.Caption := FAppModules.Language.GetString('LabelText.PatchSource');

    FPatchSourcesListBox := TListBox.Create(Self);
    FPatchSourcesListBox.Parent  := FPatchInfoGroupBox;
    FPatchSourcesListBox.Left    := 110;
    FPatchSourcesListBox.Top     := 40;
    FPatchSourcesListBox.Width   := 190;
    FPatchSourcesListBox.Height  := 90;
    FPatchSourcesListBox.Columns := 1;
    FPatchSourcesListBox.Color   := clBtnFace;

    FViewInfoLabel  := TLabel.Create(Self);
    FViewInfoLabel.Parent   := FPatchInfoGroupBox;
    FViewInfoLabel.Left     := 330;
    FViewInfoLabel.Top      := 10;
    FViewInfoLabel.AutoSize := FALSE;
    FViewInfoLabel.Width    := 500;
    FViewInfoLabel.Height   := 26;
    FViewInfoLabel.Layout   := tlCenter;
    FViewInfoLabel.WordWrap := TRUE;
    FViewInfoLabel.Caption  := FAppModules.Language.GetString('LabelText.DisplayTheResult');

    FPatchChangedShape := TShape.Create(Self);
    FPatchChangedShape.Parent := FPatchInfoGroupBox;
    FPatchChangedShape.Left   := 330;
    FPatchChangedShape.Top    := 35;
    FPatchChangedShape.Width  := 20;
    FPatchChangedShape.Height := 20;
    FPatchChangedShape.Brush.Color := clRed;

    FPatchChangedLabel  := TLabel.Create(Self);
    FPatchChangedLabel.Parent   := FPatchInfoGroupBox;
    FPatchChangedLabel.Left     := 360;
    FPatchChangedLabel.Top      := 33;
    FPatchChangedLabel.AutoSize := FALSE;
    FPatchChangedLabel.Width    := 500;
    FPatchChangedLabel.Height   := 26;
    FPatchChangedLabel.Layout   := tlCenter;
    FPatchChangedLabel.WordWrap := TRUE;
    FPatchChangedLabel.Caption := FAppModules.Language.GetString('LabelText.ResultsStoredMayBeInvalid');

    FFileNotOnDiskShape := TShape.Create(Self);
    FFileNotOnDiskShape.Parent := FPatchInfoGroupBox;
    FFileNotOnDiskShape.Left   := 330;
    FFileNotOnDiskShape.Top    := 60;
    FFileNotOnDiskShape.Width  := 20;
    FFileNotOnDiskShape.Height := 20;
    FFileNotOnDiskShape.Brush.Color := clAqua;

    FFileNotOnDiskLabel  := TLabel.Create(Self);
    FFileNotOnDiskLabel.Parent   := FPatchInfoGroupBox;
    FFileNotOnDiskLabel.Left     := 360;
    FFileNotOnDiskLabel.Top      := 58;
    FFileNotOnDiskLabel.AutoSize := FALSE;
    FFileNotOnDiskLabel.Width    := 500;
    FFileNotOnDiskLabel.Height   := 26;
    FFileNotOnDiskLabel.Layout   := tlCenter;
    FFileNotOnDiskLabel.WordWrap := TRUE;
    FFileNotOnDiskLabel.Caption := FAppModules.Language.GetString('LabelText.Indication');

    FFileInvalidShape := TShape.Create(Self);
    FFileInvalidShape.Parent := FPatchInfoGroupBox;
    FFileInvalidShape.Left   := 330;
    FFileInvalidShape.Top    := 85;
    FFileInvalidShape.Width  := 20;
    FFileInvalidShape.Height := 20;
    FFileInvalidShape.Brush.Color := clYellow;

    FFileInvalidLabel  := TLabel.Create(Self);
    FFileInvalidLabel.Parent   := FPatchInfoGroupBox;
    FFileInvalidLabel.Left     := 360;
    FFileInvalidLabel.Top      := 83;
    FFileInvalidLabel.AutoSize := FALSE;
    FFileInvalidLabel.Width    := 500;
    FFileInvalidLabel.Height   := 26;
    FFileInvalidLabel.Layout   := tlCenter;
    FFileInvalidLabel.WordWrap := TRUE;
    FFileInvalidLabel.Caption  := FAppModules.Language.GetString('LabelText.FileStoredOnDiskMayBeInvalid');

    FNoSourcesLabel            := TLabel.Create(Self);
    FNoSourcesLabel.Parent     := FPatchInfoGroupBox;
    FNoSourcesLabel.Left       := 330;
    FNoSourcesLabel.Top        := 110;
    FNoSourcesLabel.Layout     := tlCenter;
    FNoSourcesLabel.Caption    := FAppModules.Language.GetString('LabelText.NoSourcesClassAndPatchCannotRun');
    FNoSourcesLabel.Visible    := FALSE;
    FNoSourcesLabel.Font.Style := [fsBold];
    FNoSourcesLabel.Font.Color := clRed;

    FClassRNotRunYetLabel            := TLabel.Create(Self);
    FClassRNotRunYetLabel.Parent     := FPatchInfoGroupBox;
    FClassRNotRunYetLabel.Left       := 330;
    FClassRNotRunYetLabel.Top        := 110;
    FClassRNotRunYetLabel.Layout     := tlCenter;
    FClassRNotRunYetLabel.Caption    := FAppModules.Language.GetString('LabelText.ClassRNotYetRun');
    FClassRNotRunYetLabel.Visible    := FALSE;
    FClassRNotRunYetLabel.Font.Style := [fsBold];
    FClassRNotRunYetLabel.Font.Color := clRed;

    FPatchNotRunYetLabel          := TLabel.Create(Self);
    FPatchNotRunYetLabel.Parent   := FPatchInfoGroupBox;
    FPatchNotRunYetLabel.Left     := 330;
    FPatchNotRunYetLabel.Top      := 110;
    FPatchNotRunYetLabel.Layout   := tlCenter;
    FPatchNotRunYetLabel.Caption  := FAppModules.Language.GetString('LabelText.PatchRNotYetRun');
    FPatchNotRunYetLabel.Visible  := FALSE;
    FPatchNotRunYetLabel.Font.Style := [fsBold];
    FPatchNotRunYetLabel.Font.Color := clRed;

    FRAWFilesDoesNotExistLabel          := TLabel.Create(Self);
    FRAWFilesDoesNotExistLabel.Parent   := FPatchInfoGroupBox;
    FRAWFilesDoesNotExistLabel.Left     := 330;
    FRAWFilesDoesNotExistLabel.Top      := 110;
    FRAWFilesDoesNotExistLabel.Layout   := tlCenter;
    FRAWFilesDoesNotExistLabel.Caption  := FAppModules.Language.GetString('LabelText.PatchRCannotBeRun');
    FRAWFilesDoesNotExistLabel.Visible  := FALSE;
    FRAWFilesDoesNotExistLabel.Font.Style := [fsBold];
    FRAWFilesDoesNotExistLabel.Font.Color := clRed;

    FPatchRInputFileDoesNotExistLabel := TLabel.Create(Self);
    FPatchRInputFileDoesNotExistLabel.Parent   := FPatchInfoGroupBox;
    FPatchRInputFileDoesNotExistLabel.Left     := 330;
    FPatchRInputFileDoesNotExistLabel.Top      := 110;
    FPatchRInputFileDoesNotExistLabel.Layout   := tlCenter;
    FPatchRInputFileDoesNotExistLabel.Caption  := FAppModules.Language.GetString('LabelText.PatchRRun');
    FPatchRInputFileDoesNotExistLabel.Visible  := FALSE;
    FPatchRInputFileDoesNotExistLabel.Font.Style := [fsBold];
    FPatchRInputFileDoesNotExistLabel.Font.Color := clRed;

    {* Class R ****************************************************************}
    FClassRGroupBox := TGroupBox.Create(Self);
    FClassRGroupBox.Parent  := FScrollBox;
    FClassRGroupBox.Left    := 10;
    FClassRGroupBox.Top     := 142;
    FClassRGroupBox.Width   := 310;
    FClassRGroupBox.Height  := 398;
    FClassRGroupBox.Caption := FAppModules.Language.GetString('GroupBoxCaption.ClassR');

    FShiftDataLabel := TLabel.Create(Self);
    FShiftDataLabel.Parent  := FClassRGroupBox;
    FShiftDataLabel.Left    := 10;
    FShiftDataLabel.Top     := 20;
    FShiftDataLabel.Caption := FAppModules.Language.GetString('LabelText.ShiftDataAboutMean');

    FShiftDataRadioGroup := TRadioGroup.Create(Self);
    FShiftDataRadioGroup.Parent := FClassRGroupBox;
    FShiftDataRadioGroup.Left   := 150;
    FShiftDataRadioGroup.Height := 30;
    FShiftDataRadioGroup.Top    := 10;
    FShiftDataRadioGroup.Width  := 100;
    FShiftDataRadioGroup.Columns := 2;

    FClassRInputFileLabel := TLabel.Create(Self);
    FClassRInputFileLabel.Parent  := FClassRGroupBox;
    FClassRInputFileLabel.Left    := 10;
    FClassRInputFileLabel.Top     := 45;
    FClassRInputFileLabel.Caption := FAppModules.Language.GetString('LabelText.InputFileName');

    FClassRInputFileEdit := TEdit.Create(Self);
    FClassRInputFileEdit.Parent := FClassRGroupBox;
    FClassRInputFileEdit.Left   := 150;
    FClassRInputFileEdit.Top    := 45;
    FClassRInputFileEdit.Width  := 100;

    FClassRViewInputFileButton := TButton.Create(Self);
    FClassRViewInputFileButton.Parent  := FClassRGroupBox;
    FClassRViewInputFileButton.Left    := 260;
    FClassRViewInputFileButton.Top     := 45;
    FClassRViewInputFileButton.Width   := 40;
    FClassRViewInputFileButton.Height  := 21;
    FClassRViewInputFileButton.Caption := FAppModules.Language.GetString('ButtonCaption.View');
    FClassRViewInputFileButton.Enabled := FALSE;

    FClassROutputFileLabel := TLabel.Create(Self);
    FClassROutputFileLabel.Parent  := FClassRGroupBox;
    FClassROutputFileLabel.Left    := 10;
    FClassROutputFileLabel.Top     := 70;
    FClassROutputFileLabel.Caption := FAppModules.Language.GetString('LabelText.OutputFileName');

    FClassROutputFileEdit := TEdit.Create(Self);
    FClassROutputFileEdit.Parent := FClassRGroupBox;
    FClassROutputFileEdit.Left   := 150;
    FClassROutputFileEdit.Top    := 70;
    FClassROutputFileEdit.Width  := 100;

    FClassRViewOutputFileButton := TButton.Create(Self);
    FClassRViewOutputFileButton.Parent  := FClassRGroupBox;
    FClassRViewOutputFileButton.Left    := 260;
    FClassRViewOutputFileButton.Top     := 70;
    FClassRViewOutputFileButton.Width   := 40;
    FClassRViewOutputFileButton.Height  := 21;
    FClassRViewOutputFileButton.Caption := FAppModules.Language.GetString('ButtonCaption.View');
    FClassRViewOutputFileButton.Enabled := FALSE;

    FClassRHeaderLabel := TLabel.Create(Self);
    FClassRHeaderLabel.Parent := FClassRGroupBox;
    FClassRHeaderLabel.Left   := 10;
    FClassRHeaderLabel.Top    := 95;
    FClassRHeaderLabel.Caption := FAppModules.Language.GetString('LabelText.OutputFileHeader');

    FClassRHeaderEdit := TEdit.Create(Self);
    FClassRHeaderEdit.Parent  := FClassRGroupBox;
    FClassRHeaderEdit.Left    := 10;
    FClassRHeaderEdit.Top     := 110;
    FClassRHeaderEdit.Width   := 240;

    FClassRCurrentDatesLabel := TLabel.Create(Self);
    FClassRCurrentDatesLabel.Parent   := FClassRGroupBox;
    FClassRCurrentDatesLabel.Left     := 10;
    FClassRCurrentDatesLabel.Top      := 140;

    FClassRChangeDatesLabel := TLabel.Create(Self);
    FClassRChangeDatesLabel.Parent   := FClassRGroupBox;
    FClassRChangeDatesLabel.Left     := 10;
    FClassRChangeDatesLabel.Top      := 165;
    FClassRChangeDatesLabel.Caption  := FAppModules.Language.GetString('LabelText.ChangeTheDates');

    FClassRChangeDatesRadioGroup := TRadioGroup.Create(Self);
    FClassRChangeDatesRadioGroup.Parent := FClassRGroupBox;
    FClassRChangeDatesRadioGroup.Left   := 150;
    FClassRChangeDatesRadioGroup.Height := 30;
    FClassRChangeDatesRadioGroup.Top    := 155;
    FClassRChangeDatesRadioGroup.Width  := 100;
    FClassRChangeDatesRadioGroup.Columns := 2;

    FClassRFirstYearLabel := TLabel.Create(Self);
    FClassRFirstYearLabel.Parent   := FClassRGroupBox;
    FClassRFirstYearLabel.Left     := 10;
    FClassRFirstYearLabel.Top      := 190;
    FClassRFirstYearLabel.Caption  := FAppModules.Language.GetString('LabelText.StartYear');

    FClassRFirstYearComboBox := TComboBox.Create(Self);
    FClassRFirstYearComboBox.Parent := FClassRGroupBox;
    FClassRFirstYearComboBox.Left   := 150;
    FClassRFirstYearComboBox.Top    := 190;
    FClassRFirstYearComboBox.Width  := 100;
    FClassRFirstYearComboBox.Style  := csDropDown;
    FClassRFirstYearComboBox.AutoComplete := FALSE;

    FClassRLastYearLabel := TLabel.Create(Self);
    FClassRLastYearLabel.Parent   := FClassRGroupBox;
    FClassRLastYearLabel.Left     := 10;
    FClassRLastYearLabel.Top      := 215;
    FClassRLastYearLabel.Caption  := FAppModules.Language.GetString('LabelText.EndYear');

    FClassRLastYearComboBox := TComboBox.Create(Self);
    FClassRLastYearComboBox.Parent := FClassRGroupBox;
    FClassRLastYearComboBox.Left   := 150;
    FClassRLastYearComboBox.Top    := 215;
    FClassRLastYearComboBox.Width  := 100;
    FClassRLastYearComboBox.Style := csDropDown;
    FClassRLastYearComboBox.AutoComplete := FALSE;

    FRunClassRHeaderLabel := TLabel.Create(Self);
    FRunClassRHeaderLabel.Parent   := FClassRGroupBox;
    FRunClassRHeaderLabel.Left     := 10;
    FRunClassRHeaderLabel.Top      := 240;
    FRunClassRHeaderLabel.Caption  := FAppModules.Language.GetString('LabelText.ClassRRunOptions');

    FClassROption0Button := TButton.Create(Self);
    FClassROption0Button.Parent  := FClassRGroupBox;
    FClassROption0Button.Left    := 10;
    FClassROption0Button.Top     := 260;
    FClassROption0Button.Width   := 30;
    FClassROption0Button.Height  := 21;
    FClassROption0Button.Caption := '0';

    FClassROption0Label := TLabel.Create(Self);
    FClassROption0Label.Parent  := FClassRGroupBox;
    FClassROption0Label.Left    := 50;
    FClassROption0Label.Top     := 260 + 5;
    FClassROption0Label.Height  := 21;
    FClassROption0Label.Caption := FAppModules.Language.GetString('LabelText.RefreshData');

    FClassROption1Button := TButton.Create(Self);
    FClassROption1Button.Parent  := FClassRGroupBox;
    FClassROption1Button.Left    := 10;
    FClassROption1Button.Top     := 281;
    FClassROption1Button.Width   := 30;
    FClassROption1Button.Height  := 21;
    FClassROption1Button.Caption := '1';

    FClassROption1Label := TLabel.Create(Self);
    FClassROption1Label.Parent  := FClassRGroupBox;
    FClassROption1Label.Left    := 50;
    FClassROption1Label.Top     := 281 + 5;
    FClassROption1Label.Height  := 21;
    FClassROption1Label.Caption := FAppModules.Language.GetString('LabelText.DeterminePercentageIntactData');

    FClassROption3Button := TButton.Create(Self);
    FClassROption3Button.Parent  := FClassRGroupBox;
    FClassROption3Button.Left    := 10;
    FClassROption3Button.Top     := 302;
    FClassROption3Button.Width   := 30;
    FClassROption3Button.Height  := 21;
    FClassROption3Button.Caption := '3';

    FClassROption3Label := TLabel.Create(Self);
    FClassROption3Label.Parent  := FClassRGroupBox;
    FClassROption3Label.Left    := 50;
    FClassROption3Label.Top     := 302 + 5;
    FClassROption3Label.Height  := 21;
    FClassROption3Label.Caption := FAppModules.Language.GetString('LabelText.StationsVSMonths');

    FClassROption4Button := TButton.Create(Self);
    FClassROption4Button.Parent  := FClassRGroupBox;
    FClassROption4Button.Left    := 10;
    FClassROption4Button.Top     := 323;
    FClassROption4Button.Width   := 30;
    FClassROption4Button.Height  := 21;
    FClassROption4Button.Caption := '4';

    FClassROption4Label := TLabel.Create(Self);
    FClassROption4Label.Parent  := FClassRGroupBox;
    FClassROption4Label.Left    := 50;
    FClassROption4Label.Top     := 324 + 5;
    FClassROption4Label.Height  := 21;
    FClassROption4Label.Caption := FAppModules.Language.GetString('LabelText.StationsVSYears');

    FClassROption5Button := TButton.Create(Self);
    FClassROption5Button.Parent  := FClassRGroupBox;
    FClassROption5Button.Left    := 10;
    FClassROption5Button.Top     := 345;
    FClassROption5Button.Width   := 30;
    FClassROption5Button.Height  := 21;
    FClassROption5Button.Caption := '5';

    FClassROption5Label := TLabel.Create(Self);
    FClassROption5Label.Parent  := FClassRGroupBox;
    FClassROption5Label.Left    := 50;
    FClassROption5Label.Top     := 345 + 5;
    FClassROption5Label.Height  := 21;
    FClassROption5Label.Caption := FAppModules.Language.GetString('LabelText.Options3And4');

    FClassROption2ChkBox := TCheckBox.Create(Self);
    FClassROption2ChkBox.Parent  := FClassRGroupBox;
    FClassROption2ChkBox.Left    := 10;
    FClassROption2ChkBox.Top     := 366;
    FClassROption2ChkBox.Width   := 270;
    FClassROption2ChkBox.Height  := 21;
    FClassROption2ChkBox.Caption := FAppModules.Language.GetString('CheckBoxCaption.PerformRoughPatch');
{
    FClassROption2Label := TLabel.Create(Self);
    FClassROption2Label.Parent  := FClassRGroupBox;
    FClassROption2Label.Left    := 50;
    FClassROption2Label.Top     := 302 + 5;
    FClassROption2Label.Height  := 21;
    FClassROption2Label.Caption := 'Perform rough patch to enable classification';
}
    {* Patch R Input **********************************************************}
    FPatchRGroupBox := TGroupBox.Create(Self);
    FPatchRGroupBox.Parent  := FScrollBox;
    FPatchRGroupBox.Left    := 330;
    FPatchRGroupBox.Top     := 142;
    FPatchRGroupBox.Width   := 230;
    FPatchRGroupBox.Height  := 398;
    FPatchRGroupBox.Caption := FAppModules.Language.GetString('GroupBoxCaption.PatchRInput');

    FNoOfSeasonsLabel := TLabel.Create(Self);
    FNoOfSeasonsLabel.Parent := FPatchRGroupBox;
    FNoOfSeasonsLabel.Left   := 10;
    FNoOfSeasonsLabel.Top    := 20;
    FNoOfSeasonsLabel.Height := 21;
    FNoOfSeasonsLabel.Caption := FAppModules.Language.GetString('LabelText.NrOfSeasons');

    FNoOfSeasonsEdit := TSpinEdit.Create(Self);
    FNoOfSeasonsEdit.Parent := FPatchRGroupBox;
    FNoOfSeasonsEdit.Left   := 120;
    FNoOfSeasonsEdit.Top    := 20;
    FNoOfSeasonsEdit.Width  := 30;
    FNoOfSeasonsEdit.MinValue := 1;
    FNoOfSeasonsEdit.MaxValue := 4;
    FNoOfSeasonsEdit.EditorEnabled := FALSE;

    FSeasonsGrid := TStringGridWithCellChange.Create( Self,FAppModules );
    FSeasonsGrid.Parent    := FPatchRGroupBox;
    FSeasonsGrid.Left      := 10;
    FSeasonsGrid.Top       := 50;
    FSeasonsGrid.DefaultRowHeight := 20;
    FSeasonsGrid.DefaultColWidth  := 25;
    FSeasonsGrid.ColCount  := 3;
    FSeasonsGrid.RowCount  := 13;
    FSeasonsGrid.FixedCols := 1 ;
    FSeasonsGrid.Width     := 210;
    FSeasonsGrid.Height    := 277;

    FPatchRInputFileLabel := TLabel.Create(Self);
    FPatchRInputFileLabel.Parent  := FPatchRGroupBox;
    FPatchRInputFileLabel.Left    := 10;
    FPatchRInputFileLabel.Top     := 335;
    FPatchRInputFileLabel.Caption := FAppModules.Language.GetString('LabelText.InputFileName');

    FPatchRInputFileEdit := TEdit.Create(Self);
    FPatchRInputFileEdit.Parent := FPatchRGroupBox;
    FPatchRInputFileEdit.Left   := 90;
    FPatchRInputFileEdit.Top    := 335;
    FPatchRInputFileEdit.Width  := 80;

    FPatchRViewInputFileButton := TButton.Create(Self);
    FPatchRViewInputFileButton.Parent  := FPatchRGroupBox;
    FPatchRViewInputFileButton.Left    := 180;
    FPatchRViewInputFileButton.Top     := 335;
    FPatchRViewInputFileButton.Height  := 21;
    FPatchRViewInputFileButton.Width   := 40;
    FPatchRViewInputFileButton.Caption := LViewButton;

    FPatchRCreateInputFileButton := TButton.Create ( self );
    FPatchRCreateInputFileButton.Parent  := FPatchRGroupBox;
    FPatchRCreateInputFileButton.Left    := 10;
    FPatchRCreateInputFileButton.Top     := 365;
    FPatchRCreateInputFileButton.Width   := 210;
    FPatchRCreateInputFileButton.Height  := 21;
    FPatchRCreateInputFileButton.Caption := FAppModules.Language.GetString('ButtonCaption.PatchRInput');
    FPatchRCreateInputFileButton.Enabled := True;

    {* Run Patch R ************************************************************}
    FRunPatchRGroupBox := TGroupBox.Create(Self);
    FRunPatchRGroupBox.Parent  := FScrollBox;
    FRunPatchRGroupBox.Left    := 570;
    FRunPatchRGroupBox.Top     := 142;
    FRunPatchRGroupBox.Width   := 320;
    FRunPatchRGroupBox.Height  := 398;
    FRunPatchRGroupBox.Caption := FAppModules.Language.GetString('GroupBoxCaption.RunPatchR');

    FPatchMultipleRadioGroup := TRadioGroup.Create(Self);
    FPatchMultipleRadioGroup.Parent := FRunPatchRGroupBox;
    FPatchMultipleRadioGroup.Left   := 10;
    FPatchMultipleRadioGroup.Height := 48;
    FPatchMultipleRadioGroup.Top    := 12;
    FPatchMultipleRadioGroup.Width  := 240;
    FPatchMultipleRadioGroup.Columns := 1;

    FPatchRPeriodLabel := TLabel.Create(Self);
    FPatchRPeriodLabel.Parent   := FRunPatchRGroupBox;
    FPatchRPeriodLabel.Left     := 10;
    FPatchRPeriodLabel.Top      := 70;
    FPatchRPeriodLabel.Caption  := FAppModules.Language.GetString('LabelText.PatchPeriod');

    FPatchRPeriodRadioGroup := TRadioGroup.Create(Self);
    FPatchRPeriodRadioGroup.Parent := FRunPatchRGroupBox;
    FPatchRPeriodRadioGroup.Left   := 150;
    FPatchRPeriodRadioGroup.Height := 30;
    FPatchRPeriodRadioGroup.Top    := 60;
    FPatchRPeriodRadioGroup.Width  := 100;
    FPatchRPeriodRadioGroup.Columns := 2;

    FPatchRFistYearLabel := TLabel.Create(Self);
    FPatchRFistYearLabel.Parent   := FRunPatchRGroupBox;
    FPatchRFistYearLabel.Left     := 10;
    FPatchRFistYearLabel.Top      := 95;
    FPatchRFistYearLabel.Caption  := FAppModules.Language.GetString('LabelText.StartYear');

    FPatchRFirstYearComboBox := TComboBox.Create(Self);
    FPatchRFirstYearComboBox.Parent := FRunPatchRGroupBox;
    FPatchRFirstYearComboBox.Left   := 65;
    FPatchRFirstYearComboBox.Top    := 95;
    FPatchRFirstYearComboBox.Width  := 60;
    FPatchRFirstYearComboBox.Style := csDropDown;
    FPatchRFirstYearComboBox.AutoComplete := FALSE;

    FPatchRLastYearLabel := TLabel.Create(Self);
    FPatchRLastYearLabel.Parent   := FRunPatchRGroupBox;
    FPatchRLastYearLabel.Left     := 140;
    FPatchRLastYearLabel.Top      := 95;
    FPatchRLastYearLabel.Caption  := FAppModules.Language.GetString('LabelText.EndYear');

    FPatchRLastYearComboBox := TComboBox.Create(Self);
    FPatchRLastYearComboBox.Parent := FRunPatchRGroupBox;
    FPatchRLastYearComboBox.Left   := 190;
    FPatchRLastYearComboBox.Top    := 95;
    FPatchRLastYearComboBox.Width  := 60;
    FPatchRLastYearComboBox.Style := csDropDown;
    FPatchRLastYearComboBox.AutoComplete := FALSE;

    FPatchRHeaderLabel := TLabel.Create(Self);
    FPatchRHeaderLabel.Parent := FRunPatchRGroupBox;
    FPatchRHeaderLabel.Left   := 10;
    FPatchRHeaderLabel.Top    := 125;
    FPatchRHeaderLabel.Caption := FAppModules.Language.GetString('LabelText.OutputFileHeader');

    FPatchRHeaderEdit := TEdit.Create(Self);
    FPatchRHeaderEdit.Parent  := FRunPatchRGroupBox;
    FPatchRHeaderEdit.Left    := 10;
    FPatchRHeaderEdit.Top     := 140;
    FPatchRHeaderEdit.Width   := 240;

    FPatchROutputFileNameLabel := TLabel.Create(Self);
    FPatchROutputFileNameLabel.Parent  := FRunPatchRGroupBox;
    FPatchROutputFileNameLabel.Left    := 10;
    FPatchROutputFileNameLabel.Top     := 170;
    FPatchROutputFileNameLabel.Caption := FAppModules.Language.GetString('LabelText.PatchFileName');

    FPatchROutputFileNameComboBox := TComboBox.Create(Self);
    FPatchROutputFileNameComboBox.Parent   := FRunPatchRGroupBox;
    FPatchROutputFileNameComboBox.Left     := 150;
    FPatchROutputFileNameComboBox.Top      := 170;
    FPatchROutputFileNameComboBox.Width    := 100;
    FPatchROutputFileNameComboBox.Style    := csDropDownList;

    FPatchRViewOutputFileButton := TButton.Create(Self);
    FPatchRViewOutputFileButton.Parent  := FRunPatchRGroupBox;
    FPatchRViewOutputFileButton.Top     := 170;
    FPatchRViewOutputFileButton.Height  := 21;
    FPatchRViewOutputFileButton.Width   := 40;
    FPatchRViewOutputFileButton.Left    := 260;
    FPatchRViewOutputFileButton.Caption := FAppModules.Language.GetString('ButtonCaption.View');

    FPatchRPrintFileNameLabel := TLabel.Create(Self);
    FPatchRPrintFileNameLabel.Parent  := FRunPatchRGroupBox;
    FPatchRPrintFileNameLabel.Left    := 10;
    FPatchRPrintFileNameLabel.Top     := 195;
    FPatchRPrintFileNameLabel.Caption := FAppModules.Language.GetString('LabelText.PrintedFileName');

    FPatchRPrintFileNameEdit := TEdit.Create(Self);
    FPatchRPrintFileNameEdit.Parent := FRunPatchRGroupBox;
    FPatchRPrintFileNameEdit.Left   := 150;
    FPatchRPrintFileNameEdit.Top    := 195;
    FPatchRPrintFileNameEdit.Width  := 100;

    FPatchRViewPrintFileButton := TButton.Create(Self);
    FPatchRViewPrintFileButton.Parent  := FRunPatchRGroupBox;
    FPatchRViewPrintFileButton.Left    := 260;
    FPatchRViewPrintFileButton.Top     := 195;
    FPatchRViewPrintFileButton.Height  := 21;
    FPatchRViewPrintFileButton.Width   := 40;
    FPatchRViewPrintFileButton.Caption := LViewButton;

    FPatchRPlotFileNameLabel := TLabel.Create(Self);
    FPatchRPlotFileNameLabel.Parent  := FRunPatchRGroupBox;
    FPatchRPlotFileNameLabel.Left    := 10;
    FPatchRPlotFileNameLabel.Top     := 220;
    FPatchRPlotFileNameLabel.Caption := FAppModules.Language.GetString('LabelText.PlottedFileName');

    FPatchRPlotFileNameEdit := TEdit.Create(Self);
    FPatchRPlotFileNameEdit.Parent := FRunPatchRGroupBox;
    FPatchRPlotFileNameEdit.Left   := 150;
    FPatchRPlotFileNameEdit.Top    := 220;
    FPatchRPlotFileNameEdit.Width  := 100;

    FPatchRViewPlotFileButton := TButton.Create(Self);
    FPatchRViewPlotFileButton.Parent  := FRunPatchRGroupBox;
    FPatchRViewPlotFileButton.Left    := 260;
    FPatchRViewPlotFileButton.Top     := 220;
    FPatchRViewPlotFileButton.Height  := 21;
    FPatchRViewPlotFileButton.Width   := 40;
    FPatchRViewPlotFileButton.Caption := LViewButton;

    FRunPatchROptionsLabel := TLabel.Create(Self);
    FRunPatchROptionsLabel.Parent  := FRunPatchRGroupBox;
    FRunPatchROptionsLabel.Left    := 10;
    FRunPatchROptionsLabel.Top     := 245;
    FRunPatchROptionsLabel.Caption := FAppModules.Language.GetString('LabelText.PatchRRunOptions');

    FPatchRRunOptionsGrid := TStringGridWithCellChange.Create(Self, FAppModules);
    FPatchRRunOptionsGrid.Parent    := FRunPatchRGroupBox;
    FPatchRRunOptionsGrid.Left      := 10;
    FPatchRRunOptionsGrid.Top       := 260;
    FPatchRRunOptionsGrid.ColCount  := 3;
    FPatchRRunOptionsGrid.FixedCols := 0;
    FPatchRRunOptionsGrid.FixedRows := 1;
    FPatchRRunOptionsGrid.RowCount  := 6;
    FPatchRRunOptionsGrid.DefaultRowHeight := 20;
    FPatchRRunOptionsGrid.DefaultColWidth  := 70;
    FPatchRRunOptionsGrid.Width     := 217;
    FPatchRRunOptionsGrid.Height    := 130;
    FPatchRRunOptionsGrid.Color     := clBtnFace;

    FPatchRRunOption0Button := TButton.Create(Self);
    FPatchRRunOption0Button.Parent  := FRunPatchRGroupBox;
    FPatchRRunOption0Button.Left    := 235;
    FPatchRRunOption0Button.Top     := 283;
    FPatchRRunOption0Button.Caption := '0';
    FPatchRRunOption0Button.Height  := 21;
    FPatchRRunOption0Button.Width   := 30;

    FPatchRRunOption1Button := TButton.Create(Self);
    FPatchRRunOption1Button.Parent  := FRunPatchRGroupBox;
    FPatchRRunOption1Button.Left    := 235;
    FPatchRRunOption1Button.Top     := 304;
    FPatchRRunOption1Button.Caption := '1';
    FPatchRRunOption1Button.Height  := 21;
    FPatchRRunOption1Button.Width   := 30;

    FPatchRRunOption2Button := TButton.Create(Self);
    FPatchRRunOption2Button.Parent  := FRunPatchRGroupBox;
    FPatchRRunOption2Button.Left    := 235;
    FPatchRRunOption2Button.Top     := 325;
    FPatchRRunOption2Button.Caption := '2';
    FPatchRRunOption2Button.Height  := 21;
    FPatchRRunOption2Button.Width   := 30;

    FPatchRRunOption3Button := TButton.Create(Self);
    FPatchRRunOption3Button.Parent  := FRunPatchRGroupBox;
    FPatchRRunOption3Button.Left    := 235;
    FPatchRRunOption3Button.Top     := 346;
    FPatchRRunOption3Button.Caption := '3';
    FPatchRRunOption3Button.Height  := 21;
    FPatchRRunOption3Button.Width   := 30;

    FPatchRRunOption4Button := TButton.Create(Self);
    FPatchRRunOption4Button.Parent  := FRunPatchRGroupBox;
    FPatchRRunOption4Button.Left    := 235;
    FPatchRRunOption4Button.Top     := 367;
    FPatchRRunOption4Button.Caption := '4';
    FPatchRRunOption4Button.Height  := 21;
    FPatchRRunOption4Button.Width   := 30;

  except on E: Exception do HandleError (E, OPNAME) end;
end;

procedure TPatchRDialog.DestroyMemberObjects;
const OPNAME = 'TPatchRDialog.DestroyMemberObjects';
begin
  inherited DestroyMemberObjects;
  try
  except on E: Exception do HandleError (E, OPNAME) end;
end;

function TPatchRDialog.Initialise: boolean;
const OPNAME = 'TPatchRDialog.Initialise';
var
  lIndex   : integer;
  LNoText,
  LYesText,
  LSummaryText,
  LFullText,
  LLogOnlyText : String;

begin
  Result := False;
  try
    if (Parent <> nil) then
    begin
      LNoText := FAppModules.Language.GetString('OptionGrid.No');
      LYesText := FAppModules.Language.GetString('OptionGrid.Yes');
      LFullText := FAppModules.Language.GetString('OptionGrid.Full');
      LLogOnlyText := FAppModules.Language.GetString('OptionGrid.LogOnly');
      LSummaryText := FAppModules.Language.GetString('OptionGrid.Summary');

      FPatchInfoGroupBox.Caption := FAppModules.Language.GetString('GroupBoxCaption.PatchInformation');

      FPatchMultipleRadioGroup.Items.Clear;
      FPatchMultipleRadioGroup.Items.Add('Patch target gauge only');
      FPatchMultipleRadioGroup.Items.Add('Patch target and source gauges');
      FPatchMultipleRadioGroup.ItemIndex := 0;

      FShiftDataRadioGroup.Items.Clear;
      FShiftDataRadioGroup.Items.Add('No');
      FShiftDataRadioGroup.Items.Add('Yes');
      FShiftDataRadioGroup.ItemIndex := 0;

      FClassRChangeDatesRadioGroup.Items.Clear;
      FClassRChangeDatesRadioGroup.Items.Add('No');
      FClassRChangeDatesRadioGroup.Items.Add('Yes');
      FClassRChangeDatesRadioGroup.ItemIndex := 0;

      FSeasonsGrid.ColWidths[0] := 48;
      FSeasonsGrid.Cells[0, 0]  := FAppModules.Language.GetString('SeasonGrid.Seasons');
      FSeasonsGrid.Cells[0, 1]  := FAppModules.Language.GetString('SeasonGrid.Oct');
      FSeasonsGrid.Cells[0, 2]  := FAppModules.Language.GetString('SeasonGrid.Nov');
      FSeasonsGrid.Cells[0, 3]  := FAppModules.Language.GetString('SeasonGrid.Dec');
      FSeasonsGrid.Cells[0, 4]  := FAppModules.Language.GetString('SeasonGrid.Jan');
      FSeasonsGrid.Cells[0, 5]  := FAppModules.Language.GetString('SeasonGrid.Feb');
      FSeasonsGrid.Cells[0, 6]  := FAppModules.Language.GetString('SeasonGrid.Mar');
      FSeasonsGrid.Cells[0, 7]  := FAppModules.Language.GetString('SeasonGrid.Apr');
      FSeasonsGrid.Cells[0, 8]  := FAppModules.Language.GetString('SeasonGrid.May');
      FSeasonsGrid.Cells[0, 9]  := FAppModules.Language.GetString('SeasonGrid.Jun');
      FSeasonsGrid.Cells[0, 10] := FAppModules.Language.GetString('SeasonGrid.Jul');
      FSeasonsGrid.Cells[0, 11] := FAppModules.Language.GetString('SeasonGrid.Aug');
      FSeasonsGrid.Cells[0, 12] := FAppModules.Language.GetString('SeasonGrid.Sep');
      for lIndex := 1 to FSeasonsGrid.ColCount - 1 do
        FSeasonsGrid.Cells[lIndex, 0] := IntToStr(lIndex);

      FPatchRPeriodRadioGroup.Items.Clear;
      FPatchRPeriodRadioGroup.Items.Add('No');
      FPatchRPeriodRadioGroup.Items.Add('Yes');
      FPatchRPeriodRadioGroup.ItemIndex := 0;

      FPatchRRunOptionsGrid.Cells[0, 0] := FAppModules.Language.GetString('OptionGrid.Diagnostics');
      FPatchRRunOptionsGrid.Cells[1, 0] := FAppModules.Language.GetString('OptionGrid.Plotting');
      FPatchRRunOptionsGrid.Cells[2, 0] := FAppModules.Language.GetString('OptionGrid.UpdateFiles');
      FPatchRRunOptionsGrid.Cells[0, 1] := LNoText;
      FPatchRRunOptionsGrid.Cells[1, 1] := LNoText;
      FPatchRRunOptionsGrid.Cells[2, 1] := LYesText;
      FPatchRRunOptionsGrid.Cells[0, 2] := LSummaryText;
      FPatchRRunOptionsGrid.Cells[1, 2] := LNoText;
      FPatchRRunOptionsGrid.Cells[2, 2] := LYesText;
      FPatchRRunOptionsGrid.Cells[0, 3] := LSummaryText;
      FPatchRRunOptionsGrid.Cells[1, 3] := LLogOnlyText;
      FPatchRRunOptionsGrid.Cells[2, 3] := LYesText;
      FPatchRRunOptionsGrid.Cells[0, 4] := LFullText;
      FPatchRRunOptionsGrid.Cells[1, 4] := LLogOnlyText;
      FPatchRRunOptionsGrid.Cells[2, 4] := LNoText;
      FPatchRRunOptionsGrid.Cells[0, 5] := LFullText;
      FPatchRRunOptionsGrid.Cells[1, 5] := FAppModules.Language.GetString('OptionGrid.ScreenAndLog');
      FPatchRRunOptionsGrid.Cells[2, 5] := LNoText;
    end;
    Result := True;
  except on E: Exception do HandleError (E, OPNAME) end;
end;

procedure TPatchRDialog.AssignHelpContext;
const OPNAME = 'TPatchRDialog.AssignHelpContext';
begin
  inherited;
  try
    SetControlHelpContext(FPatchSourcesListBox, HC_PatchInformation);
    SetControlHelpContext(FShiftDataRadioGroup, HC_PatchInformation);
    SetControlHelpContext(FClassRInputFileEdit, HC_PatchInformation);
    SetControlHelpContext(FClassROutputFileEdit, HC_PatchInformation);
    SetControlHelpContext(FClassRHeaderEdit, HC_PatchInformation);
    SetControlHelpContext(FClassRHeaderEdit, HC_PatchInformation);
    SetControlHelpContext(FClassRChangeDatesRadioGroup, HC_PatchInformation);
    SetControlHelpContext(FClassROption0Button, HC_PatchInformation);
    SetControlHelpContext(FClassROption1Button, HC_PatchInformation);
    SetControlHelpContext(FClassROption2ChkBox, HC_PatchInformation);
    SetControlHelpContext(FClassROption3Button, HC_PatchInformation);
    SetControlHelpContext(FClassROption4Button, HC_PatchInformation);
    SetControlHelpContext(FClassROption5Button, HC_PatchInformation);
    SetControlHelpContext(FPatchRInputFileEdit, HC_PatchInformation);
    SetControlHelpContext(FNoOfSeasonsEdit, HC_PatchInformation);
    SetControlHelpContext(FPatchMultipleRadioGroup, HC_PatchInformation);
    SetControlHelpContext(FPatchRHeaderEdit, HC_PatchInformation);
    SetControlHelpContext(FPatchROutputFileNameComboBox, HC_PatchInformation);
    SetControlHelpContext(FPatchRPrintFileNameEdit, HC_PatchInformation);
    SetControlHelpContext(FPatchRPlotFileNameEdit, HC_PatchInformation);
    SetControlHelpContext(FPatchRRunOptionsGrid, HC_PatchInformation);


  except on E: Exception do HandleError (E, OPNAME) end;
end;


end.
