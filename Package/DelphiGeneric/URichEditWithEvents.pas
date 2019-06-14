//
//
//  UNIT      : Contains URichEditWithEvents utility.
//  AUTHOR    : Dziedzi Ramulondi
//  DATE      : 2003/09/16
//  COPYRIGHT : Copyright © 2003 Arivia
//
//
unit URichEditWithEvents;

interface

uses
  Classes,
  Windows,
  Messages,
  vcl.StdCtrls, vcl.Menus, vcl.ComCtrls,vcl.Dialogs,UAbstractComponent;

type

  TRichEditWithEvents = class(TAbstractRichEdit)
  protected
    FFindDialog: TFindDialog;
    FPopupMenu: TPopupMenu;
    FmnuCopy: TMenuItem;
    FmnuCut: TMenuItem;
    FmnuPaste: TMenuItem;
    FmnuFind: TMenuItem;
    FmnuSelectAll: TMenuItem;
    FmnuUndo: TMenuItem;

    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    procedure SelectionChange; override;
  public
    function StudyHasChanged: boolean; override;
    function LanguageHasChanged: boolean; override;
    function Initialise: boolean; override;
    function SaveState: boolean; override;

    procedure SetMenuState;

    procedure OnCopyClick(Sender: TObject);
    procedure OnPasteClick(Sender: TObject);
    procedure OnCutClick(Sender: TObject);
    procedure OnSelectAllClick(Sender: TObject);
    procedure OnFindClick(Sender: TObject);
    procedure OnUndoClick(Sender: TObject);
    procedure OnFindDialogFind(Sender: TObject);

    property mnuCopy      : TMenuItem read FmnuCopy;
    property mnuCut       : TMenuItem read FmnuCut;
    property mnuPaste     : TMenuItem read FmnuPaste;
    property mnuFind      : TMenuItem read FmnuFind;
    property mnuSelectAll : TMenuItem read FmnuSelectAll;
    property mnuUndo      : TMenuItem read FmnuUndo;
  end;

implementation

uses
  vcl.Clipbrd,
  SysUtils,
  UErrorHandlingOperations;

{ TRichEditWithEvents }

procedure TRichEditWithEvents.CreateMemberObjects;
const OPNAME = 'TRichEditWithEvents.CreateMemberObjects';
begin
  inherited CreateMemberObjects;
  try
    FPopupMenu := TPopupMenu.Create(Self);
    //FPopupMenu.AutoPopup := True;

    FmnuCopy          := TMenuItem.Create(FPopupMenu);
    FmnuCopy.Caption  := 'Copy';
    FmnuCopy.ShortCut := 16451;
    FmnuCopy.OnClick  := OnCopyClick;

    FmnuPaste          := TMenuItem.Create(FPopupMenu);
    FmnuPaste.Caption  := 'Paste';
    FmnuPaste.ShortCut := 16470;
    FmnuPaste.OnClick  := OnPasteClick;

    FmnuCut          := TMenuItem.Create(FPopupMenu);
    FmnuCut.Caption  := 'Cut';
    FmnuCut.ShortCut := 16472;
    FmnuCut.OnClick  := OnCutClick;

    FmnuUndo          := TMenuItem.Create(FPopupMenu);
    FmnuUndo.Caption  := 'Undo';
    FmnuUndo.ShortCut := 16474;
    FmnuUndo.OnClick  := OnUndoClick;

    FmnuSelectAll          := TMenuItem.Create(FPopupMenu);
    FmnuSelectAll.Caption  := 'Select All';
    FmnuSelectAll.ShortCut := 16449;
    FmnuSelectAll.OnClick  := OnSelectAllClick;

    FmnuFind          := TMenuItem.Create(FPopupMenu);
    FmnuFind.Caption  := 'Find';
    FmnuFind.ShortCut := 16454;
    FmnuFind.OnClick  := OnFindClick;

    FFindDialog          := TFindDialog.Create(Self);
    FFindDialog.Options  := [frDown, frFindNext, frHideUpDown];
    FFindDialog.OnFind   := OnFindDialogFind;

    FPopupMenu.Items.Add(FmnuCopy);
    FPopupMenu.Items.Add(FmnuCut);
    FPopupMenu.Items.Add(FmnuPaste);
    FPopupMenu.Items.Add(FmnuFind);
    FPopupMenu.Items.Add(FmnuSelectAll);
    FPopupMenu.Items.Add(FmnuUndo);

    Self.PopupMenu         := FPopupMenu;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRichEditWithEvents.DestroyMemberObjects;
const OPNAME = 'TRichEditWithEvents.DestroyMemberObjects';
begin
  inherited DestroyMemberObjects;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRichEditWithEvents.Initialise: boolean;
const OPNAME = 'TRichEditWithEvents.Initialise';
begin
  Result := inherited Initialise;
  try
    SetMenuState;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRichEditWithEvents.StudyHasChanged: boolean;
const OPNAME = 'TRichEditWithEvents.StudyHasChanged';
begin
  Result := inherited StudyHasChanged;
  try
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRichEditWithEvents.LanguageHasChanged: boolean;
const OPNAME = 'TRichEditWithEvents.LanguageHasChanged';
begin
  Result := inherited LanguageHasChanged;
  try                            
    FmnuCopy.Caption      := FAppModules.Language.GetString('MenuCaption.EditCopy');
    FmnuFind.Caption      := FAppModules.Language.GetString('MenuCaption.EditFind');
    FmnuPaste.Caption     := FAppModules.Language.GetString('MenuCaption.EditPaste');
    FmnuCut.Caption       := FAppModules.Language.GetString('MenuCaption.EditCut');
    FmnuUndo.Caption      := FAppModules.Language.GetString('MenuCaption.EditUndo');
    FmnuSelectAll.Caption := FAppModules.Language.GetString('MenuCaption.EditSelectAll');
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRichEditWithEvents.SaveState: boolean;
const OPNAME = 'TRichEditWithEvents.SaveState';
begin
  Result := inherited SaveState;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRichEditWithEvents.OnCopyClick(Sender: TObject);
const OPNAME = 'TRichEditWithEvents.OnCopyClick';
begin
  try
    Self.Perform(WM_COPY, 0, 0);
    SetMenuState;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRichEditWithEvents.OnCutClick(Sender: TObject);
const OPNAME = 'TRichEditWithEvents.OnCopyClick';
begin
  try
    Self.Perform( WM_Cut, 0, 0);
    SetMenuState;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRichEditWithEvents.OnPasteClick(Sender: TObject);
const OPNAME = 'TRichEditWithEvents.OnPasteClick';
begin
  try
    Self.Perform(WM_Paste, 0, 0);
    SetMenuState;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRichEditWithEvents.OnSelectAllClick(Sender: TObject);
const OPNAME = 'TRichEditWithEvents.OnSelectAllClick';
begin
  try
    Self.Perform(EM_SETSEL, 0, -1);
    SetMenuState;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRichEditWithEvents.OnUndoClick(Sender: TObject);
const OPNAME = 'TRichEditWithEvents.OnUndoClick';
begin
  try
    Self.Perform( WM_UNDO, 0, 0);
    SetMenuState;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRichEditWithEvents.OnFindClick(Sender: TObject);
const OPNAME = 'TRichEditWithEvents.OnFindClick';
begin
  try
    FFindDialog.Execute;
    SetMenuState;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRichEditWithEvents.OnFindDialogFind(Sender: TObject);
const OPNAME = 'TRichEditWithEvents.OnFindDialogFind';
var
  LFoundAt: LongInt;
  LStartPos, LEndPos: Integer;
  LSearchType: TSearchTypes;
begin
  try
    LFoundAt := Self.SelStart;
    with Self do
    begin
      if frWholeWord in FFindDialog.Options then
        LSearchType := LSearchType + [stWholeWord]
      else
        LSearchType := LSearchType + [];
      if frMatchCase in FFindDialog.Options then
        LSearchType := LSearchType + [stMatchCase]
      else
        LSearchType := LSearchType + [];
      if frDown in FFindDialog.Options then
      begin
        { begin the search after the current selection if there is one }
        { otherwise, begin at the start of the text }
        if SelLength <> 0 then
          LStartPos := SelStart + SelLength
        else
          LStartPos := SelStart;
        { LEndPos is the length from LStartPos to the end of the text in the rich edit control }
        LEndPos := Length(Text) - LStartPos;
        LFoundAt := FindText(FFindDialog.FindText, LStartPos, LEndPos, LSearchType);
      end
      else
      begin
        {Up-direction code goes here}
      end;
      if LFoundAt <> -1 then
      begin
        SelStart := LFoundAt;
        SelLength := Length(FFindDialog.FindText);
        Perform(EM_SCROLLCARET, 0, 0);
        //SetFocus;
      end;
    end;
    SetMenuState;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


procedure TRichEditWithEvents.SetMenuState;
const OPNAME = 'TRichEditWithEvents.OnFindClick';
var
  LHasSelection: Boolean;  { declare a temporary variable }
begin
  try
    FmnuPaste.Enabled := Clipboard.HasFormat(CF_TEXT);  {enable or disable the Paste                                                menu item}
    LHasSelection := Self.SelLength > 0;  { True if text is selected }
    FmnuCut.Enabled := LHasSelection;  { enable menu items if HasSelection is True }
    FmnuCopy.Enabled := LHasSelection;
    FmnuUndo.Enabled := Self.CanUndo;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRichEditWithEvents.SelectionChange;
const OPNAME = 'TRichEditWithEvents.OnSelectionChanged';
begin
  inherited SelectionChange;
  try
    SetMenuState;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
