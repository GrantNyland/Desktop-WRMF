//
//
//  UNIT      : Contains the class TToolBar.
//  AUTHOR    : Grant Nyland
//  DATE      : 2002/02/20
//  COPYRIGHT : Copyright © 2002 DWAF
//
//
unit UToolBar;

interface

uses
  Vcl.ExtCtrls,
  Classes,
  Vcl.Buttons,
  Vcl.Menus,
  UHelpContexts,
  UAbstractObject,
  UAbstractComponent;

type
  TVaalDBMSSpeedButton = class(TAbstractMainSpeedButton)
  end;
  TToolBar = class(TAbstractToolBar)
  protected
    FSystemChildToolBars: TList;

    // Speed buttons
    FClipboard    : TVaalDBMSSpeedButton;
    FExport       : TVaalDBMSSpeedButton;
    FWhatIsThis   : TVaalDBMSSpeedButton;
    //FHelp     : TVaalDBMSSpeedButton;
    FWhatIsThisDown : boolean;

    function CreateButton(AButtonKey: string; AMenuEvent: integer = 0): TVaalDBMSSpeedButton;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    procedure SetHorizontalPositions; override;
    procedure SetChildToolBarHorizontalPositions;
    procedure SetButtonHorizontalPosition(AButton: TSpeedButton; AVisible, AGap: boolean; var AButtonCount, AGaps: integer);
    procedure DoButtonClick(ASender: TObject);
    procedure DoWhatIsThisClick(ASender: TObject);
    procedure AssignHelpContext; override;
  public
    function LanguageHasChanged: boolean; override;
    procedure SetClipboardEnabled(AEnabled: boolean);
    procedure SetExportEnabled(AEnabled: boolean);
    procedure SetHelpEnabled(AEnabled: boolean);
    procedure SetWhatIsThisDown(ADown: boolean);
    procedure AddSystemChildToolBar(ASystemChildToolBar: TCustomPanel);
    procedure RemoveSystemChildToolBar(ASystemChildToolBar: TCustomPanel);
    function SaveState: boolean; override;
    function ResetState: boolean; override;
    procedure RefreshState; virtual;
    property IsButtonWhatIsThisDown: boolean read FWhatIsThisDown write FWhatIsThisDown;
    property BtnWhatIsThis : TVaalDBMSSpeedButton read FWhatIsThis;

  end;

implementation

uses
  SysUtils,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Printers,
  UMainMenuEventType,
  UErrorHandlingOperations;

const
  C_ButtonSize     = 28;
  C_ButtonBorder   = 3;
  C_ButtonGap      = 8;
  C_ChildTooBarGap = 4;

procedure TToolBar.CreateMemberObjects;
const OPNAME = 'TToolBar.CreateMemberObjects';
begin
  try
    FSystemChildToolBars := TList.Create;
    Height := C_ButtonSize + C_ButtonBorder * 2;
    FClipboard     := CreateButton('CopyToClipboard', CmeCopyToClipboard);
    FExport        := CreateButton('ExportToFile',    CmeExportToFile);
    FWhatIsThis    := CreateButton('HelpWhatIsThis',CmeHelpWhatIsThis);
    //FHelp          := CreateButton('HelpContents',    CmeHelpContents);
    FWhatIsThisDown := False;

    SetHorizontalPositions;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TToolBar.DestroyMemberObjects;
const OPNAME = 'TToolBar.DestroyMemberObjects';
var
  LCount: integer;
begin
  inherited DestroyMemberObjects;
  try
    for LCount := 0 to FSystemChildToolBars.Count - 1 do
       TCustomPanel(FSystemChildToolBars.Items[LCount]).Parent := nil;
    FreeAndNil(FSystemChildToolBars);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TToolBar.AssignHelpContext;
const OPNAME = 'TToolBar.AssignHelpContext';
begin
  try
    SetControlHelpContext(Self,           HC_Toolbar);
    SetControlHelpContext(FClipboard,     HC_EditCopyToClipboard);
    SetControlHelpContext(FExport,        HC_EditExportToFile);
    SetControlHelpContext(FWhatIsThis,    HC_EditExportToFile);
  except on E: Exception do HandleError(E, OPNAME); end;
end;


function TToolBar.CreateButton(AButtonKey: string; AMenuEvent: integer = 0): TVaalDBMSSpeedButton;
const OPNAME = 'TToolBar.CreateButton';
begin
  Result := nil;
  try
    Result := TVaalDBMSSpeedButton.Create(self, FAppModules, AButtonKey);
    Result.Parent    := self;
    Result.Top       := C_ButtonBorder;
    Result.Height    := C_ButtonSize;
    Result.Width     := C_ButtonSize;
    Result.Name      := AButtonKey;
    Result.MenuEvent := AMenuEvent;
    Result.OnClick   := DoButtonClick;
    Result.Glyph.LoadFromResourceName(HImagesInstance, UpperCase(AButtonKey));
    Result.NumGlyphs := Result.Glyph.Width div Result.Glyph.Height;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TToolBar.SetButtonHorizontalPosition(AButton: TSpeedButton; AVisible, AGap: boolean; var AButtonCount, AGaps: integer);
const OPNAME = 'TToolBar.SetButtonHorizontalPosition';
begin
  try
    if Assigned(AButton) and AVisible then
    begin
      Inc(AButtonCount);
      if AGap then
        Inc(AGaps, C_ButtonGap);
      AButton.Left := C_ButtonBorder + AGaps + C_ButtonSize * AButtonCount;
      AButton.Visible := True;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TToolBar.SetHorizontalPositions;
const
  OPNAME = 'TToolBar.SetHorizontalPositions';
var
  LButtonCount, LGaps: integer;
begin
  try
    inherited SetHorizontalPositions;
    LButtonCount := -1;
    LGaps :=  0;
    SetButtonHorizontalPosition(FClipboard,     True, False, LButtonCount, LGaps);
    SetButtonHorizontalPosition(FExport,        True, False, LButtonCount, LGaps);
    SetButtonHorizontalPosition(FWhatIsThis,    True, False, LButtonCount, LGaps);
    if Assigned(FChildToolBar) then
    begin
      SetChildToolBarHorizontalPositions;
      Inc(LGaps, FChildToolBar.Width + C_ButtonGap);
    end;
    //SetButtonHorizontalPosition(FHelp, True, True, LButtonCount, LGaps);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TToolBar.LanguageHasChanged: boolean;
const
  OPNAME = 'TToolBar.LanguageHasChanged';
begin
  Result := True;
  try
    inherited LanguageHasChanged;
    if Assigned(FClipboard)     then FClipboard.LanguageHasChanged;
    if Assigned(FExport)        then FExport.LanguageHasChanged;
    if Assigned(FWhatIsThis)    then FWhatIsThis.LanguageHasChanged;
    //if Assigned(FHelp)          then FHelp.LanguageHasChanged;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TToolBar.DoButtonClick(ASender: TObject);
const OPNAME = 'TToolBar.DoButtonClick';
begin
  try
    if (TVaalDBMSSpeedButton(ASender).MenuEvent <> CmeNull) then
      if Assigned(FAppModules) then
        FAppModules.ProcessEvent(TVaalDBMSSpeedButton(ASender).MenuEvent, nil);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TToolBar.SetClipboardEnabled(AEnabled: boolean);
const OPNAME = 'TToolBar.SetClipboardEnabled';
begin
  try
    SetButtonEnabled(FClipboard, AEnabled, 'ClipboardDisabled');
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TToolBar.SetExportEnabled(AEnabled: boolean);
const OPNAME = 'TToolBar.SetExportEnabled';
begin
  try
    SetButtonEnabled(FExport, AEnabled, 'ExportDisabled');
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TToolBar.SetHelpEnabled(AEnabled: boolean);
const OPNAME = 'TToolBar.SetHelpEnabled';
begin
  try
    //FHelp.Enabled := AEnabled;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TToolBar.AddSystemChildToolBar(ASystemChildToolBar: TCustomPanel);
const OPNAME = 'TToolBar.AddSystemChildToolBar';
var
  LPrevToolBar: TCustomPanel;
  LCount : integer;
begin
  try
    if Assigned(ASystemChildToolBar) then
    begin
      for LCount := 0 to  FSystemChildToolBars.Count-1 do
      begin
        LPrevToolBar := TAbstractToolBar(FSystemChildToolBars.Items[LCount]);
        if (LPrevToolBar.ClassName = ASystemChildToolBar.ClassName) then
          RemoveSystemChildToolBar(LPrevToolBar);
      end;

      ASystemChildToolBar.Parent := Self;
      ASystemChildToolBar.Top    := (Self.Height - ASystemChildToolBar.Height) DIV 2;
      Self.Width := Self.Width + ASystemChildToolBar.Width;
      if (FSystemChildToolBars.Count <= 0) then
        ASystemChildToolBar.Left :=  FWhatIsThis.Left + FWhatIsThis.Width + 2
      else
      begin
        LPrevToolBar := TAbstractToolBar(FSystemChildToolBars.Items[FSystemChildToolBars.Count-1]);
        ASystemChildToolBar.Left := LPrevToolBar.Left + LPrevToolBar.Width + 2;
      end;

      FSystemChildToolBars.Add(ASystemChildToolBar);
      SetChildToolBarHorizontalPositions;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TToolBar.RemoveSystemChildToolBar(ASystemChildToolBar: TCustomPanel);
const OPNAME = 'TToolBar.RemoveSystemChildToolBar';
var
  LIndex,
  LStartLeft,
  LFoundIndex: integer;
  LCurrentToolBar: TCustomPanel;
begin
  try
    if Assigned(ASystemChildToolBar) then
    begin
      LFoundIndex := -1;
      LStartLeft  := 0;
      for LIndex := 0 to FSystemChildToolBars.Count-1 do
      begin
        LCurrentToolBar := TCustomPanel(FSystemChildToolBars.Items[LIndex]);
        if(LCurrentToolBar = ASystemChildToolBar) then
        begin
          LFoundIndex := LIndex;
          LStartLeft  := ASystemChildToolBar.Left;
          ASystemChildToolBar.Parent := nil;
          Self.Width := Self.Width - ASystemChildToolBar.Width;
        end
        else
        begin
          if(LFoundIndex >= 0) then
          begin
            LCurrentToolBar := TCustomPanel(FSystemChildToolBars.Items[LIndex]);
            LCurrentToolBar.Left  := LStartLeft;
            LStartLeft  := LCurrentToolBar.Left + LCurrentToolBar.Width + 2;
          end;
        end;
      end;
      if(LFoundIndex >= 0) then
      begin
        FSystemChildToolBars.Delete(LFoundIndex);
        SetChildToolBarHorizontalPositions;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TToolBar.SetChildToolBarHorizontalPositions;
const OPNAME = 'TToolBar.SetChildToolBarHorizontalPositions';
var
  lLeft  : integer;
  lIndex : integer;
begin
  try
    if Assigned(FChildToolBar) then
    begin
      if (FSystemChildToolBars.Count > 0)   then
      begin
        lLeft  := 0;
        lIndex := FSystemChildToolBars.Count - 1;
        while (lLeft = 0) AND (lIndex >= 0) do
        begin
          if (TCustomPanel(FSystemChildToolBars.Items[lIndex]).Visible) then
            lLeft := TCustomPanel(FSystemChildToolBars.Items[lIndex]).Left +
                     TCustomPanel(FSystemChildToolBars.Items[lIndex]).Width + C_ChildTooBarGap
         else
           lIndex := lIndex - 1;
       end;
        FChildToolBar.Left :=  lLeft;
      end  
      else
        FChildToolBar.Left := FWhatIsThis.Left + FWhatIsThis.Width + C_ChildTooBarGap; // Fix  function if FWhatIsThis is no longer the last button
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TToolBar.ResetState: boolean;
const OPNAME = 'TToolBar.ResetState';
var
  LVisible: integer;
begin
  Result := False;
  try
    LVisible := FAppModules.ViewIni.ReadInteger(ClassName, 'Visible', 1);
    Self.Visible := LVisible <> 0;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TToolBar.SaveState: boolean;
const OPNAME = 'TToolBar.SaveState';
begin
  Result := False;
  try
    if(Self.Visible) then
      FAppModules.ViewIni.WriteInteger(ClassName, 'Visible', 1)
    else
      FAppModules.ViewIni.WriteInteger(ClassName, 'Visible', 0);
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TToolBar.RefreshState;
const OPNAME = 'TToolBar.RefreshState';
var
  LVisible: integer;
begin
  try
    LVisible := FAppModules.ViewIni.ReadInteger(ClassName, 'Visible', 1);
    Self.Visible := LVisible <> 0;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TToolBar.SetWhatIsThisDown(ADown: boolean);
const OPNAME = 'TToolBar.SetWhatIsThisDown';
begin
  try
    if Assigned(FWhatIsThis) and (FWhatIsThis.Enabled) and (FWhatIsThis.Down <>  ADown)then
    begin
      FWhatIsThis.Down :=  ADown;
      //DoButtonClick(FWhatIsThis);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TToolBar.DoWhatIsThisClick(ASender: TObject);
const OPNAME = 'TToolBar.DoWhatIsThisClick';
begin
  try
    FWhatIsThisDown := not FWhatIsThisDown;
    DoButtonClick(FWhatIsThis);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
