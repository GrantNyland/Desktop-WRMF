{******************************************************************************}
{*  UNIT      : Contains the class TReservoirAndChannelsOutputDialog          *}
{*  AUTHOR    : Presley Mudau                                                 *}
{*  DATE      : 2004/12/23                                                    *}
{*  COPYRIGHT : Copyright © 2003 DWAF                                         *}
{******************************************************************************}

unit UReservoirAndChannelOutputDialog;

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
  UDataComponent;

type

  TReservoirAndChannelOutputDialog = class(TAbstractScrollablePanel)
  private
  protected
    FNrOfChanInSummaryLabel    : TLabel;
    FNrOfChanInFirmYieldLabel  : TLabel;
    FNrOfResInSummaryLabel     : TLabel;
    FNrOfActiveReservoirsLabel : TLabel;
    FNrOfResInSummaryEdit      : TFieldEdit;
    FNrOfActiveReservoirsEdit  : TFieldEdit;
    FNrOfChanInSummaryEdit     : TFieldEdit;
    FNrOfChanInFirmYieldEdit   : TFieldEdit;
    FOutputImages              : TImageList;
    FAnalysisImages            : TImageList;
    FChannelsTreeview          : TFieldTreeView;
    FReservoirsTreeview        : TFieldTreeView;
    FChannelImageList          : TImageList;
    FReservoirImageList        : TImageList;

    FClickResActiveLabel       : TLabel;
    FClickResSummaryLabel      : TLabel;
    FClickChanSummaryLabel     : TLabel;
    FClickChanFirmYieldLabel   : TLabel;
    FClickResActiveImage       : TImage;
    FClickResSummaryImage      : TImage;
    FClickChanSummaryImage     : TImage;
    FClickChanFirmYieldImage   : TImage;

    FReservoirGroupBox         : TGroupBox;
    FChannelGroupBox           : TGroupBox;

    procedure CreateMemberObjects; override;
    procedure AssignHelpContext; override;
  public
    procedure Resize; override;
    procedure RestoreColourState; override;
    function LanguageHasChanged: boolean; override;
    function Initialise: boolean; override;

    property ChannelsTreeview          : TFieldTreeView    read FChannelsTreeview;
    property ReservoirsTreeview        : TFieldTreeView    read FReservoirsTreeview;
    property NrOfChanInSummaryLabel    : TLabel            read FNrOfChanInSummaryLabel;
    property NrOfChanInFirmYieldLabel  : TLabel            read FNrOfChanInFirmYieldLabel;
    property NrOfResInSummaryLabel     : TLabel            read FNrOfResInSummaryLabel;
    property NrOfActiveReservoirsLabel : TLabel            read FNrOfActiveReservoirsLabel;
    property NrOfResInSummaryEdit      : TFieldEdit        read FNrOfResInSummaryEdit;
    property NrOfActiveReservoirsEdit  : TFieldEdit        read FNrOfActiveReservoirsEdit ;
    property NrOfChanInSummaryEdit     : TFieldEdit        read FNrOfChanInSummaryEdit;
    property NrOfChanInFirmYieldEdit   : TFieldEdit        read FNrOfChanInFirmYieldEdit;
    property ReservoirGroupBox         : TGroupBox         read FReservoirGroupBox;
    property ChannelGroupBox           : TGroupBox         read FChannelGroupBox;

  end;

  implementation

uses
  SysUtils,
  UHelpContexts,
  UErrorHandlingOperations,
  UControlCreationUtilities,
  VCL.ImgList;

{******************************************************************************}
{* TReservoirAndChannelOutputDialog                                                 *}
{******************************************************************************}

procedure TReservoirAndChannelOutputDialog.CreateMemberObjects;
const OPNAME = 'TReservoirAndChannelOutputDialog.CreateMemberObjects';
var
  lOwner  : TComponent;
  lParent : TWinControl;
begin
  inherited;
  try
    lOwner  := ControlsOwner;
    lParent := ControlsParent;
    //                                                                 Left  Top  Width Height
    FReservoirGroupBox          := CreateFieldGroupBox(lOwner,lParent, 10, 10, 300, 350,0,FALSE);
    FChannelGroupBox            := CreateFieldGroupBox(lOwner,lParent, 330, 10, 330, 350,0,FALSE);
    FNrOfResInSummaryLabel      := CreateFieldLabel  (lOwner, FReservoirGroupBox,  10, 280, 197,  26);
    FNrOfActiveReservoirsLabel  := CreateFieldLabel  (lOwner, FReservoirGroupBox,  10, 310, 137,  26);
    FNrOfChanInSummaryLabel     := CreateFieldLabel  (lOwner, FChannelGroupBox, 10, 280, 197,  26);
    FNrOfChanInFirmYieldLabel   := CreateFieldLabel  (lOwner, FChannelGroupBox, 10, 310, 207,  26);

    FNrOfActiveReservoirsEdit   := CreateFieldEdit   (FAppModules, lOwner, FReservoirGroupBox, 220, 310,  40,  21, 0, FALSE);
    FNrOfActiveReservoirsEdit.Color    := clAqua;
    FNrOfActiveReservoirsEdit.ReadOnly := TRUE;
    FNrOfResInSummaryEdit       := CreateFieldEdit   (FAppModules, lOwner, FReservoirGroupBox, 220, 280,  40,  21, 0, FALSE);
    FNrOfResInSummaryEdit.Color        := clBtnFace;
    FNrOfResInSummaryEdit.ReadOnly     := TRUE;
    FNrOfChanInFirmYieldEdit    := CreateFieldEdit   (FAppModules, lOwner, FChannelGroupBox, 220, 310,  40,  21, 0, FALSE);
    FNrOfChanInFirmYieldEdit.Color     := clYellow;
    FNrOfChanInFirmYieldEdit.ReadOnly  := TRUE;
    FNrOfChanInSummaryEdit      := CreateFieldEdit   (FAppModules, lOwner, FChannelGroupBox, 220, 280,  40,  21, 0, FALSE);
    FNrOfChanInSummaryEdit.Color       := clBtnFace;
    FNrOfChanInSummaryEdit.ReadOnly    := TRUE;

    FNrOfActiveReservoirsLabel.WordWrap := TRUE;
    FNrOfResInSummaryLabel.WordWrap     := TRUE;
    FNrOfChanInSummaryLabel.WordWrap    := TRUE;
    FNrOfChanInFirmYieldLabel.WordWrap  := TRUE;

    FChannelImageList := TImageList.Create(lOwner);
    FChannelImageList.Height := 18;
    FChannelImageList.Width  := 18;
    FChannelImageList.GetInstRes(HImagesInstance, rtBitmap, 'UNCHECKYELLOW',   16, [], clWhite);
    FChannelImageList.GetInstRes(HImagesInstance, rtBitmap, 'CHECKYELLOW',     16, [], clWhite);
    FChannelImageList.GetInstRes(HImagesInstance, rtBitmap, 'DONTPRINTOUTPUT', 16, [], clWhite);
    FChannelImageList.GetInstRes(HImagesInstance, rtBitmap, 'PRINTOUTPUT',     16, [], clWhite);
    FChannelImageList.GetInstRes(HImagesInstance, rtBitmap, 'ROOTLINES',       16, [], clWhite);
    FChannelImageList.GetInstRes(HImagesInstance, rtBitmap, 'UNCHECKGRAY',     16, [], clWhite);

    FReservoirImageList := TImageList.Create(lOwner);
    FReservoirImageList.Height := 18;
    FReservoirImageList.Width  := 18;
    FReservoirImageList.GetInstRes(HImagesInstance, rtBitmap, 'UNCHECKCYAN',     16, [], clWhite);
    FReservoirImageList.GetInstRes(HImagesInstance, rtBitmap, 'CHECKCYAN',       16, [], clWhite);
    FReservoirImageList.GetInstRes(HImagesInstance, rtBitmap, 'DONTPRINTOUTPUT', 16, [], clWhite);
    FReservoirImageList.GetInstRes(HImagesInstance, rtBitmap, 'PRINTOUTPUT',     16, [], clWhite);
    FReservoirImageList.GetInstRes(HImagesInstance, rtBitmap, 'ROOTLINES',       16, [], clWhite);
    FReservoirImageList.GetInstRes(HImagesInstance, rtBitmap, 'UNCHECKGRAY',     16, [], clWhite);

    FReservoirsTreeview := TFieldTreeView.Create(lOwner, FAppModules);
    with FReservoirsTreeview do
    begin
      Parent        := FReservoirGroupBox;
      Left          := 10;
      Top           := 60;
      Width         := 270;
      Height        := 195;
      Images        := FReservoirImageList;
      StateImages   := FReservoirImageList;
      TabStop       := TRUE;
      TabOrder      := 4;
      ReadOnly      := TRUE;
    end;

    FChannelsTreeview := TFieldTreeView.Create(lOwner, FAppModules);
    with FChannelsTreeview do
    begin
      Parent        := FChannelGroupBox;
      Left          := 10;
      Top           := 60;
      Width         := 270;
      Height        := 195;
      Images        := FChannelImageList;
      StateImages   := FChannelImageList;
      TabStop       := TRUE;
      TabOrder      := 5;
      ReadOnly      := TRUE;
    end;

    FClickResSummaryLabel      := CreateFieldLabel  (lOwner, FReservoirGroupBox,  32, 10, 217,  13);
    FClickResSummaryImage      := TImage.Create(Self);
    with FClickResSummaryImage do
    begin
      Parent   := FReservoirGroupBox;
      Left     := 10;
      Top      := 10;
      Width    := 16;
      Height   := 16;
      AutoSize := True;
    end;
    FClickResSummaryImage.Picture.Bitmap.LoadFromResourceName(HImagesInstance, 'DONTPRINTOUTPUT2');

    FClickResActiveLabel       := CreateFieldLabel  (lOwner, FReservoirGroupBox,  32, 30, 176,  13);
    FClickResActiveImage       := TImage.Create(Self);
    with FClickResActiveImage do
    begin
      Parent   := FReservoirGroupBox;
      Left     := 10;
      Top      := 30;
      Width    := 16;
      Height   := 16;
      AutoSize := True;
    end;
    FClickResActiveImage.Picture.Bitmap.LoadFromResourceName(HImagesInstance, 'UNCHECKCYAN');

    FClickChanSummaryLabel     := CreateFieldLabel  (lOwner, FChannelGroupBox, 32, 10, 217,  13);
    FClickChanSummaryImage     := TImage.Create(Self);
    with FClickChanSummaryImage do
    begin
      Parent   := FChannelGroupBox;
      Left     := 10;
      Top      := 10;
      Width    := 16;
      Height   := 16;
      AutoSize := True;
    end;
    FClickChanSummaryImage.Picture.Bitmap.LoadFromResourceName(HImagesInstance, 'DONTPRINTOUTPUT2');

    FClickChanFirmYieldLabel   := CreateFieldLabel  (lOwner, FChannelGroupBox, 32, 30, 290,  13);
    FClickChanFirmYieldImage   := TImage.Create(Self);
    with FClickChanFirmYieldImage do
    begin
      Parent   := FChannelGroupBox;
      Left     := 10;
      Top      := 30;
      Width    := 16;
      Height   := 16;
      AutoSize := True;
    end;
    FClickChanFirmYieldImage.Picture.Bitmap.LoadFromResourceName(HImagesInstance, 'UNCHECKYELLOW');

  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirAndChannelOutputDialog.Resize;
const OPNAME = 'TReservoirAndChannelOutputDialog.Resize';
begin
  inherited Resize;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirAndChannelOutputDialog.Initialise: boolean;
const OPNAME = 'TReservoirAndChannelOutputDialog.Initialise';
begin
  Result := inherited Initialise;
  try
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirAndChannelOutputDialog.LanguageHasChanged: boolean;
const OPNAME = 'TReservoirAndChannelOutputDialog.LanguageHasChanged';
begin
  Result := inherited LanguageHasChanged;
  try
    FNrOfActiveReservoirsLabel.Caption := FAppModules.Language.GetString('RunParameters.NrOfActiveReservoirs') + ' :';
    FNrOfResInSummaryLabel.Caption     := FAppModules.Language.GetString('RunParameters.NrOfResInSummary') + ' :';
    FNrOfChanInSummaryLabel.Caption    := FAppModules.Language.GetString('RunParameters.NrOfChanInSummary') + ' :';
    if (FAppModules.Model.ModelName = CPlanning) then
      FNrOfChanInFirmYieldLabel.Caption  := FAppModules.Language.GetString('RunParameters.PlanningNrOfChanInPMP') + ' :'
    else
      FNrOfChanInFirmYieldLabel.Caption  := FAppModules.Language.GetString('RunParameters.NrOfChanInFirmYield') + ' :';

    FClickResActiveLabel.Caption       := FAppModules.Language.GetString('RunParameters.ClickActivateReservoir');
    FClickResSummaryLabel.Caption      := FAppModules.Language.GetString('RunParameters.ClickIncludeInSummary');
    FClickChanSummaryLabel.Caption     := FAppModules.Language.GetString('RunParameters.ClickIncludeInSummary');
    if (FAppModules.Model.ModelName = CPLanning) then
      FClickChanFirmYieldLabel.Caption   := FAppModules.Language.GetString('RunParameters.PlanningPMPChannelOutput')
    else
      FClickChanFirmYieldLabel.Caption   := FAppModules.Language.GetString('RunParameters.ClickIncludeInFirmYield');

    FNrOfResInSummaryEdit.Hint         := FAppModules.Language.GetString('TReservoirAndChannelOutputDialog.NrOfResInSummaryHint');
    FNrOfActiveReservoirsEdit.Hint     := FAppModules.Language.GetString('TReservoirAndChannelOutputDialog.NrOfActiveReservoirsHint');
    FNrOfChanInSummaryEdit.Hint        := FAppModules.Language.GetString('TReservoirAndChannelOutputDialog.NrOfChanInSummaryHint');
    FNrOfChanInFirmYieldEdit.Hint      := FAppModules.Language.GetString('TReservoirAndChannelOutputDialog.NrOfChanInAnalysesHint');

    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirAndChannelOutputDialog.RestoreColourState;
const OPNAME = 'TReservoirAndChannelOutputDialog.RestoreColourState';
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

procedure TReservoirAndChannelOutputDialog.AssignHelpContext;
const OPNAME = 'TReservoirAndChannelOutputDialog.AssignHelpContext';
begin
  try
    SetControlHelpContext(Self,                      HC_ChannelOutputOptions);
    SetControlHelpContext(FNrOfResInSummaryEdit,     HC_ReservoirOutputOptions);
    SetControlHelpContext(FNrOfActiveReservoirsEdit, HC_ReservoirOutputOptions);
    SetControlHelpContext(FNrOfChanInSummaryEdit,    HC_ChannelOutputOptions);
    SetControlHelpContext(FNrOfChanInFirmYieldEdit,  HC_ChannelOutputOptions);
    SetControlHelpContext(FChannelsTreeview,         HC_ChannelOutputOptions);
    SetControlHelpContext(FReservoirsTreeview,       HC_ReservoirOutputOptions);
    SetControlHelpContext(FChannelGroupBox,          HC_ChannelOutputOptions);
    SetControlHelpContext(FReservoirGroupBox,        HC_ReservoirOutputOptions);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

end.
