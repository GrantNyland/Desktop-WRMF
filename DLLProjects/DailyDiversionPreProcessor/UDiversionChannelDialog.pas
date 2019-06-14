unit UDiversionChannelDialog;

interface

uses
  VCL.controls,
  Classes,
  VCL.Forms,
  VCL.Grids,
  VCL.ComCtrls,
  VCL.ExtCtrls,
  VCL.Buttons,
  VCL.StdCtrls,
  VCL.Dialogs,
  Contnrs,
  UControlCreationUtilities,
  UHelpContexts,
  UAbstractObject,
  UAbstractComponent,
  UDataEditComponent,
  UDataComponent,
  UDailyDiversionGaugeSQLAgent;


type
  TDiversionChannelDialog = class(TAbstractForm)
  protected
    FPnlCheckBoxes  : TPanel;
    FPnlButtons     : TPanel;
    FBtnOK          : TBitBtn;
    FBtnCancel      : TBitBtn;
    FBtnReset       : TBitBtn;
    FChannelList    : TStringList;
    FCheckBoxList   : TObjectList;

    function GetSelectedChannelsCommaText: string;
    function GetIdentifierForSelectedChannelsCommaText: string;
    function GetChannelListCommaText: string;
    procedure ResetBtnOnClick(Sender : TObject);
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    procedure Resize; override;
  public
    property ChannelList: string read GetChannelListCommaText;
    property SelectedChannelsCommaText: string read GetSelectedChannelsCommaText;
    property IdentifierForSelectedChannelsCommaText: string read GetIdentifierForSelectedChannelsCommaText;
    function LanguageHasChanged: boolean; override;
    function Initialise: Boolean; override;
  end;


implementation

uses
  SysUtils,
  VCL.ImgList,
  UDatasetType,
  VCL.Printers,
  UConstants,
  VoaimsCom_TLB,
  UErrorHandlingOperations, Math;

{ TDiversionChannelDialog }

procedure TDiversionChannelDialog.CreateMemberObjects;
const OPNAME = 'TDiversionChannelDialog.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
    FCheckBoxList              := TObjectList.Create(False);
    FChannelList               := TStringList.Create;

    Self.BorderStyle           := bsDialog;
    Self.Position              := poScreenCenter;

    FPnlCheckBoxes             := TPanel.Create(Self);
    FPnlCheckBoxes.BevelOuter  := bvLowered;

    FPnlButtons                := TPanel.Create(Self);
    FPnlButtons.BevelOuter     := bvRaised;

    FBtnOK                     := TBitBtn.Create(Self);
    FBtnCancel                 := TBitBtn.Create(Self);
    FBtnReset                  := TBitBtn.Create(Self);

    FPnlCheckBoxes.Align       := alClient;
    FPnlButtons.Align          := alBottom;

    FBtnOK.ModalResult         := mrOK;
    FBtnCancel.ModalResult     := mrCancel;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDiversionChannelDialog.DestroyMemberObjects;
const OPNAME = 'TDiversionChannelDialog.DestroyMemberObjects';
begin
  try
    FreeAndNil(FCheckBoxList);
    FreeAndNil(FChannelList);
    inherited DestroyMemberObjects;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDiversionChannelDialog.LanguageHasChanged: boolean;
const OPNAME = 'TDiversionChannelDialog.LanguageHasChanged';
begin
  Result := inherited LanguageHasChanged;
  try
    FPnlCheckBoxes.Caption  := '';
    FPnlButtons.Caption     := '';
    Self.Caption            := FAppModules.Language.GetString('TDiversionChannelDialog.DiversionChannelDialog');
    FBtnOK.Caption          := FAppModules.Language.GetString('ButtonCaption.OK');
    FBtnCancel.Caption      := FAppModules.Language.GetString('ButtonCaption.Cancel');
    FBtnReset.Caption       := FAppModules.Language.GetString('ButtonCaption.Reset');
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDiversionChannelDialog.Initialise: Boolean;
const OPNAME = 'TDiversionChannelDialog.Initialise';
var
  LIndex,
  LHeightPos   : integer;
  LChkChanels  : TCheckBox;
begin
  Result := False;
  try
    FChannelList.CommaText := GetChannelListCommaText;
    LHeightPos             := 0;
    for LIndex := 0  to FChannelList.Count - 1 do
    begin
      LChkChanels              := TCheckBox.Create(Self);
      FCheckBoxList.Add(LChkChanels);
      LChkChanels.Parent       := FPnlCheckBoxes;
      LChkChanels.Width        := Self.Width;
      LChkChanels.Caption      := FChannelList[Lindex];
      LChkChanels.Left         := 5;
      LChkChanels.Top          := LHeightPos + LChkChanels.Height + 3;
      LHeightPos               := LHeightPos + LChkChanels.Height + 3;
    end;
    FPnlCheckBoxes.Parent  := Self;
    FPnlButtons.Parent     := Self;

    FBtnOK.Parent          := FPnlButtons;
    FBtnCancel.Parent      := FPnlButtons;
    FBtnReset.Parent       := FPnlButtons;
    FBtnReset.OnClick      := ResetBtnOnClick;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDiversionChannelDialog.GetChannelListCommaText: string;
const OPNAME = 'TDiversionChannelDialog.GetChannelListCommaText';
var
  LDailyDiversionGaugeSQLAgent  : TDailyDiversionGaugeSQLAgent;
  LChannelList                  : TStringList;
begin
  Result := '';
  try
    LChannelList                 := TStringList.Create;
    LDailyDiversionGaugeSQLAgent := TDailyDiversionGaugeSQLAgent.Create(FAppModules);
    try
      if LDailyDiversionGaugeSQLAgent.WRYMDataIsLoaded(LChannelList) then
      begin
        Result := LChannelList.CommaText;
      end;
    finally
      FreeAndNil(LDailyDiversionGaugeSQLAgent);
      FreeAndNil(LChannelList);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDiversionChannelDialog.GetIdentifierForSelectedChannelsCommaText: string;
const OPNAME = 'TDiversionChannelDialog.GetIdentifierForSelectedChannelsCommaText';
var
  LIdentifierList,
  LSelectedChannels     : TStringList;
  LChanelIndex          : Integer;
  LIdentifier,
  LChannelName          : string;
  LPos                  : integer;
begin
  Result := '';
  try
    LIdentifierList   := TStringList.Create;
    LSelectedChannels := TStringList.Create;
    try
      LSelectedChannels.CommaText := GetSelectedChannelsCommaText;
      for LChanelIndex := 0 to LSelectedChannels.Count - 1 do
      begin
        LChannelName := LSelectedChannels[LChanelIndex];
        LIdentifier  := '';
        LPos         := Pos(' ',LChannelName);
        if(LPos > 0) then
          LIdentifier  := Copy(LChannelName,1,LPos-1);
        if (LIdentifier <> '') then;
        LIdentifierList.Add(LIdentifier);
      end;
      Result := LIdentifierList.CommaText;
    finally
      LIdentifierList.Free;
      LSelectedChannels.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDiversionChannelDialog.GetSelectedChannelsCommaText: string;
const OPNAME = 'TDiversionChannelDialog.GetSelectedChannelsCommaText';
var
  LSelectedChannels : TStringList;
  LIndex            : Integer;
  LCheckBox         : TCheckBox;
begin
  Result := '';
  try
    LSelectedChannels := TStringList.Create;
    try
      for LIndex:=0 to FCheckBoxList.Count - 1 do
      begin
        LCheckBox := TCheckBox(FCheckBoxList.Items[LIndex]);
        if(LCheckBox.Checked) then
          LSelectedChannels.Add(LCheckBox.Caption);
      end;
      Result := LSelectedChannels.CommaText;
    finally
      LSelectedChannels.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDiversionChannelDialog.ResetBtnOnClick(Sender: TObject);
const OPNAME = 'TDiversionChannelDialog.ResetBtnOnClick';
var
  LIndex     : Integer;
  LCheckBox  : TCheckBox;
begin
  try
    for LIndex:=0 to FCheckBoxList.Count - 1 do
    begin
      LCheckBox := TCheckBox(FCheckBoxList.Items[LIndex]);
      if(LCheckBox.Checked) then
        LCheckBox.Checked := False;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDiversionChannelDialog.Resize;
const OPNAME = 'TDiversionChannelDialog.Resize';
var
  LComponentCount,
  LNoOfComponents,
  LDialogHeight : Integer;
begin
  inherited;
  try
    FPnlButtons.Height      := 30;
    FBtnOK.Left             := 5;
    FBtnOK.Width            := 80;
    FBtnOK.Height           := 30;

    FBtnCancel.Left         := FBtnOK.Left + FBtnOK.ClientWidth + 10;
    FBtnCancel.Width        := 80;
    FBtnCancel.Height       := 30;

    FBtnReset.Left          := FBtnCancel.Left + FBtnCancel.ClientWidth + 10;
    FBtnReset.Width         := 80;
    FBtnReset. Height       := 30;

    LNoOfComponents         := 0;

    for LComponentCount :=0 to Self.ComponentCount - 1 do
       Inc(LNoOfComponents);
    LDialogHeight := 0;
    if(FCheckBoxList.Count > 0) then
      LDialogHeight  := LNoOfComponents * TCheckBox(FCheckBoxList[0]).Height ;
    LDialogHeight  := LDialogHeight + FBtnOK.Height;
    Self.Height    := LDialogHeight;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
