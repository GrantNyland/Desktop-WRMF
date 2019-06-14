//
//
//  UNIT      : Contains the class TMainForm.
//  AUTHOR    : Grant Nyland
//  DATE      : 2002/02/20
//  COPYRIGHT : Copyright © 2002 DWAF
//
//
unit UMainForm;

interface

uses
  System.Types,
  Vcl.Forms,
  Vcl.Controls,
  Vcl.ComCtrls,
  Vcl.StdCtrls,
  Vcl.ExtCtrls,
  Messages,
  Classes,
  Windows,
  UMainMenu,
  UAbstractObject,
  UStudyPanel,
  USystemModelLinkClasses,
  UMainFormMenuItemManager,
  UAbstractComponent;

type
  TMainForm = class(TAbstractMainForm)
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private
    FAppModules: TAppModules;
  protected
    FMainFormMenuItemManager: TMainFormMenuItemManager;
    FWhatIsThisOn: boolean;
    FOldCursor : TCursor;
    FMenu: TMainMenu;
    FPageControl: TPageControl;
    FBackground: TImage;
    FStudyPanel: TStudyPanel;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    procedure DoClose(var Action: TCloseAction); override;
    procedure MHLoadModel(var AModel: TLoadModelMessage); message UM_LoadModelManager;
    procedure MHStudyDetailsChanged(var AMsg: TMessage); message UM_NewStudyDetails;
    procedure AppMessage(var Msg: TMsg; var Handled: Boolean);
    function  HandleMouseMsg(CtlHandle: THandle; Button: TMouseButton; Shift: TShiftState; X, Y: Integer): boolean;
  public
    constructor Create(AOwner: TComponent); override;
    procedure ApplyPreviousTabIndex;
    procedure SaveCurrentTabIndex;
    procedure Resize; override;
    procedure OnHelpWhatIsThisClick(ADown:Boolean);
    function LanguageHasChanged: boolean; override;
    function StudyHasChanged: boolean; override;
    procedure RefreshState; virtual;
    function SaveState: boolean; override;
    function ResetState: boolean; override;
    property PageControl: TPageControl read FPageControl;
    property StudyPanel: TStudyPanel read FStudyPanel;
    property AppModules: TAppModules read FAppModules write FAppModules;
    property MainFormMenuItemManager: TMainFormMenuItemManager read FMainFormMenuItemManager write FMainFormMenuItemManager;

  end;

var
  GAppModules: TAppModules;

implementation

{$R *.DFM}

uses
  SysUtils,
  Vcl.Graphics,
  UHelpContexts,
  UUtilities,
  UMainMenuEventType,
  UErrorHandlingOperations;

constructor TMainForm.Create(AOwner: TComponent);
const OPNAME = 'TMainForm.Create';
begin
  try
    FAppModules := GAppModules;
    inherited Create(AOwner);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMainForm.CreateMemberObjects;
const
  OPNAME = 'TMainForm.CreateMemberObjects';
begin
  try
    FOldCursor := crDefault;
    FWhatIsThisOn := False;
    FClosing := False;
    Self.HelpContext :=  HC_Introduction;
    FMainFormMenuItemManager := nil;

    // Switch on hints for the form and application.
    Application.ShowHint := True;
    Application.OnMessage := AppMessage;

    ShowHint := True;

    // Create the menu.
    FMenu := TMainMenu.Create(self, FAppModules);
    Menu := FMenu;

    // Create the page control.
    FPageControl := TPageControl.Create(self);
    FPageControl.Parent := self;
    FPageControl.Align := alClient;
    FPageControl.Visible := False;

    // Create the background image.
    FBackground := TImage.Create(self);
    FBackground.Parent := self;
    FBackground.Transparent := True;
    FBackground.Align := alClient;
    FBackground.Picture.Bitmap.LoadFromResourceName(HImagesInstance, 'BACKGROUND');
    FBackground.Center := True;
    {FBackground.Left := 0;
    FBackground.Width := FBackground.Picture.Bitmap.Width;
    FBackground.Anchors := [akBottom];
    FBackground.Height := FBackground.Picture.Bitmap.Height;
    }

    // Create the information panel.
    FStudyPanel := TStudyPanel.Create(self, FAppModules);
    FStudyPanel.Parent := self;
    FStudyPanel.Align := alBottom;

    Self.Caption := GetGeneralFormCaption(FAppModules);
  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TMainForm.DestroyMemberObjects;
const OPNAME = 'TMainForm.DestroyMemberObjects';
var LIndex: integer;
begin
  try

    // Report any tab sheets that have not extracted themselves.
    for LIndex := FPageControl.PageCount - 1 downto 0 do
    begin
      ReportError('Tab sheet ' + FPageControl.Pages[LIndex].ClassName + ' was not extracted.', OPNAME);
      FPageControl.Pages[LIndex].PageControl := nil;
    end;

    // Call ancestor.
    inherited DestroyMemberObjects;

  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMainForm.Resize;
const OPNAME = 'TMainForm.Resize';
begin
  try
    inherited Resize;
    //FBackground.Top := ClientHeight - FBackground.Height - FStudyPanel.Height;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TMainForm.DoClose(var Action: TCloseAction);
const OPNAME = 'TMainForm.DoClose';
begin
  try
    FClosing := True;
    inherited DoClose(Action);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMainForm.MHLoadModel(var AModel: TLoadModelMessage);
const OPNAME = 'TMainForm.MHLoadModel';
begin
  try
    FAppModules.LoadModel(AModel.ModelManagerType);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TMainForm.ApplyPreviousTabIndex;
const OPNAME = 'TMainForm.ApplyPreviousTabIndex';
var LIndex: integer;
begin
  try
    if Assigned(FPageControl) then
    begin
      if Assigned(FAppModules) then
      begin
        if Assigned(FAppModules.ViewIni()) then
        begin
          LIndex := FAppModules.ViewIni.ReadInteger(ClassName, 'LastTabSheet', 0);
          if (LIndex >= FPageControl.PageCount) then
            LIndex := FPageControl.PageCount - 1;
          if (LIndex >= 0) and (FPageControl.Pages[LIndex].TabVisible) then
            FPageControl.ActivePageIndex := LIndex;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TMainForm.SaveCurrentTabIndex;
const OPNAME = 'TMainForm.SaveCurrentTabIndex';
begin
  try
    if Assigned(FPageControl) and (FPageControl.ActivePageIndex >= 0) then
      if Assigned(FAppModules) then
        if Assigned(FAppModules.ViewIni()) then
          FAppModules.ViewIni.WriteInteger(ClassName, 'LastTabSheet', FPageControl.ActivePageIndex);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TMainForm.LanguageHasChanged: boolean;
const
  OPNAME = 'TMainForm.LanguageHasChanged';
begin
  Result := True;
  try
    FMenu.LanguageHasChanged;
    FStudyPanel.LanguageHasChanged;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TMainForm.RefreshState;
const OPNAME = 'TMainForm.RefreshState';
begin
  try
    FStudyPanel.StudyHasChanged;
    FStudyPanel.UserHasChanged;
    FStudyPanel.RefreshState;
    FPageControl.Visible := (FPageControl.PageCount > 0);
    FBackground.Visible := (not FPageControl.Visible);
    Self.Caption := GetGeneralFormCaption(FAppModules);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMainForm.MHStudyDetailsChanged(var AMsg: TMessage);
const OPNAME = 'TMainForm.MHStudyDetailsChanged';
begin
  try
    RefreshState;
    if Assigned(FAppModules.Model()) then
      FAppModules.Model.StudyHasChanged;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMainForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
const OPNAME = 'TMainForm.FormCloseQuery';
begin
  CanClose := True;
  try
    CanClose :=  FAppmodules.CanApplicationClose;
    FClosing := CanClose;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMainForm.ResetState: boolean;
const OPNAME = 'TMainForm.ResetState';
begin
  Result := False;
  try
    Self.WindowState := wsNormal;
    Self.Left := 0;
    Self.Top  := 0;
    Self.Height := 480;
    Self.Width  := 640;

    FStudyPanel.ResetState;
    FMenu.ResetState;
    ApplyPreviousTabIndex;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMainForm.SaveState: boolean;
const OPNAME = 'TMainForm.SaveState';
begin
  Result := False;
  try
    FStudyPanel.SaveState;
    FMenu.SaveState;
    SaveCurrentTabIndex;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMainForm.OnHelpWhatIsThisClick(ADown:Boolean);
const OPNAME = 'TMainForm.OnHelpWhatIsThisClick';
begin
  try
    FWhatIsThisOn := ADown;
    if ADown then  //in help mode now
    begin
        FOldCursor := Screen.Cursor;
        Screen.Cursor := crHelp;
    end
    else Screen.Cursor := FOldCursor;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMainForm.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
const OPNAME = 'TMainForm.FormKeyDown';
begin
  try
   if (key = VK_ESCAPE) and FWhatIsThisOn then
   begin
     FAppModules.ProcessEvent(CmeSetHelpWhatIsThisOff,nil);
     Key := 0;
   end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMainForm.AppMessage(var Msg: TMsg; var Handled: Boolean);
const OPNAME = 'TMainForm.AppMessage';
begin
  Handled := False;
  try
    if not FWhatIsThisOn then Exit;

    if Screen.ActiveForm = Self then
    begin
      case Msg.message of
      WM_LBUTTONDOWN:
            Handled := HandleMouseMsg(Msg.hwnd, mbLeft, KeysToShiftState(Msg.wParam), Loword(Msg.lParam), Hiword(Msg.lParam));
      //WM_RBUTTONUP:
      //      Handled := HandleMouseMsg(Msg.hwnd, mbRight, KeysToShiftState(Msg.wParam), Loword(Msg.lParam), Hiword(Msg.lParam));
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMainForm.HandleMouseMsg(CtlHandle: THandle; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer): boolean;
const OPNAME = 'TMainForm.HandleMouseMsg';
var
  FocusCtl: TWinControl;
  ClickCtl: TControl;
  ContextID: integer;
  Pt: TSmallPoint;

  { The simple function FindControl(Handle: HWnd) fails on combos,
    because a TCustomCombobox has 3 window handles. }
  function FindFocusControl(Ctl: TWinControl): TWinControl;
  const OPNAME = 'UMainForm.FindFocusControl';
  var
    i: integer;
  begin
    Result := nil;
    try
      if Ctl.Handle = CtlHandle then
        Result := Ctl
      else
      if (Ctl is TCustomCombobox) and
         (ChildWindowfromPoint(Ctl.handle, point(x,y)) = CtlHandle) then
        Result := Ctl
      else
      begin
        for i := 0 to Ctl.ControlCount-1 do
        begin
          if (Ctl.Controls[i] is TWinControl) then
            result := FindFocusControl(TWinControl(Ctl.Controls[i]));
          if result <> nil then
            break;
        end;
      end;
    except on E: Exception do HandleError(E, OPNAME) end;
  end;

  function FindContextID(Ctl: TControl): integer;
  const OPNAME = 'UMainForm.FindContextID';
  begin
    result := 0;
    try
      { If our control is a TWinControl, it has a HelpContext property by sure, if not, we use
        the Tag property to retrieve a help context for TGraphicControls and TSpeedButtons. }

      if (Ctl is TWinControl) then
        Result := TWinControl(Ctl).HelpContext
      else if (Ctl is TGraphicControl) then
        Result := Ctl.tag;

      if (Ctl is TLabel) and (TLabel(Ctl).FocusControl <> nil) then
        Result := TLabel(Ctl).FocusControl.HelpContext;

      { If no help context number has been found, inherit the help context from the parent. }
      if (result = 0) and (Ctl.parent <> nil) then
        Result := FindContextID(Ctl.Parent);
    except on E: Exception do HandleError(E, OPNAME) end;
   end;

begin
  Result := False;
  try
    if not FWhatIsThisOn then Exit;
    FocusCtl := FindFocusControl(Self);
    if FocusCtl = nil then FocusCtl := self;
    ClickCtl := FocusCtl.ControlAtPos(point(x,y), true);
    if (ClickCtl = nil) then ClickCtl := FocusCtl;

    {ClickCtl is now definitely the control beneath the mouse cursor. This could be any
     control, even disabled controls are valid. FocusCtl, on the other hand, is that control
     which has received the mouse message. FocusCtl is a TWinControl and has a HelpContext property
     while ClickCtl may not have a HelpContext property - e.g. a TGraphicControl or TSpeedButton. }

     ContextID := FindContextID(ClickCtl);
     //if ContextID = 0 then ContextID := 1000;

     case Button of
     mbLeft:
       begin
         Pt := PointToSmallPoint(FocusCtl.Clienttoscreen( point(x,y) ));

        {When a help context number is a minus value, we display the help topic in the
         main window. If it is a plus value - the default -  we display it in a popup window.
         This is not a standard function of Winhelp but very useful. }

         if(ClickCtl = FMainFormMenuItemManager.ToolBar.BtnWhatIsThis) then
         begin
           FMainFormMenuItemManager.ToolBar.BtnWhatIsThis.Click;
         end
         else
         begin
           if ContextID < 0 then
             Application.HelpCommand(HELP_CONTEXT, abs(ContextID))
           else
           begin
                //Application.HelpCommand(HELP_SETPOPUP_POS, Longint(Pt));
                //Application.HelpCommand(HELP_CONTEXTPOPUP, ContextID);
             Application.HelpCommand(HELP_CONTEXT, ContextID)
           end;
         end;
         Result := True;
       end;
     {mbRight:
       begin
         result := (not (ClickCtl is TCustomEdit)) and (not (ClickCtl is TCustomComboBox));
         if result then
         begin
           PopupWhatsthis.tag := ContextID;
           Pt := PointToSmallPoint(FocusCtl.Clienttoscreen( point(x,y) ));
           if TLabel(ClickCtl).PopupMenu = nil then WhatsThisPopupmenu.popup(Pt.x, Pt.y)
           else TLabel(ClickCtl).PopupMenu.popup(Pt.x, Pt.y);
         end;
       end;
       }
     end;

  except on E: Exception do HandleError(E, OPNAME) end;
end;
function TMainForm.StudyHasChanged: boolean;
const  OPNAME = 'TMainForm.StudyHasChanged';
begin
  Result := inherited StudyHasChanged;
  try
    Self.Caption := GetGeneralFormCaption(FAppModules);
    Self.Invalidate;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

end.
