//
//
//  UNIT      : Contains THintComboBox Class
//  AUTHOR    : Christopher Levendall
//  DATE      : 2004/11/15
//  COPYRIGHT : Copyright © 2003 DWAF
//
//
unit UHintComboBox;

interface

uses
  Windows,
  Messages,
  SysUtils,
  VCL.Graphics,
  Classes,
  VCL.Forms,
  VCL.Controls,
  VCL.StdCtrls,
  UDataEditComponent;
{$WARN SYMBOL_DEPRECATED OFF}
{Combo with Hints for each item in the list}
type
  THintComboBox = Class (TFieldComboBox)
  protected
    { Protected declarations }
    FListHandle: HWND;
    FNewWndProc: pointer;
    FOldWndProc: pointer;
    FHintWin : THintWindow;
    //FHintList : TStrings;
    procedure NewWndProc(var Message: TMessage);
    procedure WMLButtonUp(var Message: TMessage); message WM_LBUTTONUP;
  public
    { Public declarations }
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
//    constructor Create(AOwner: TComponent); override;
//    destructor Destroy; override;
    //procedure SetHintList(Value: TStrings);

  published
    { Published declarations }
    //property HintList: TStrings read FHintList write SetHintList;
  end;

implementation

procedure THintComboBox.CreateMemberObjects;
//constructor THintComboBox.Create(AOwner: TComponent);
const OPNAME = 'THintComboBox.CreateMemberObjects';
begin
  inherited;
  //FHintList := TStringList.Create; //Creates the list of hits for hints list
  FHintWin := THintWindow.Create(Self); // Creates a HintWindows object
  FNewWndProc:= MakeObjectInstance(NewWndProc);
end;

//destructor THintComboBox.Destroy;
procedure THintComboBox.DestroyMemberObjects;
const OPNAME = 'THintComboBox.DestroyMemberObjects';
begin
 If Assigned(FHintWin) Then FHintWin.ReleaseHandle;
  if FListHandle <> 0
    then SetWindowLong(FListHandle, GWL_WNDPROC, longint(FOldWndProc));
  if FNewWndProc <> nil
    then FreeObjectInstance(FNewWndProc);
  inherited;
end;

{procedure THintComboBox.SetHintList(Value: TStrings);
var
  i : Integer;
begin
  FHintList.Assign(Value);
  If FHintList.Count < DropDownCount then // To have the same number of hints than items
  begin
    for I:= FHintList.Count to DropDownCount do
    FHintList.Add ('');
  end;
end;}


procedure THintComboBox.NewWndProc(var Message: TMessage);
const OPNAME = 'THintComboBox.NewWndProc';
var
  pt: TPoint;
  wnd : HWND;
//  buf: array[0..128] of Char;
  i: Integer;
//  CurHint : String;
  CmbPoint : TPoint;
  CmbRect, HntRect : TRect;
  CmbHeight : Integer;
//  CmbDropHeight : Integer;
begin
  // To close the hint when the box is not displayed
  if Message.Msg = WM_SHOWWINDOW then
  begin
      If Assigned (FHintWin) then FHintWin.ReleaseHandle;
  end;
  if Message.Msg = WM_MOUSEMOVE then
  begin
    GetCursorPos(pt);
    wnd := WindowFromPoint( pt );
    If Wnd = FListHandle then
    Begin
      Windows.ScreenToClient(wnd, pt );
      i:= SendMessage(wnd, LB_ITEMFROMPOINT, 0, LongWord(@pt));   //PointToSmallpoint
      If (i >= 0) and (i< Items.count) Then
      Begin
        If not Assigned(FHintWin) then
          FHintWin := THintWindow.Create(Self);
        HntRect:= FHintWin.CalcHintRect(150, Items[i], nil);
        FHintWin.Color := clInfoBk;
        CmbHeight := HntRect.Bottom - HntRect.Top;
//        CmbDropHeight := CmbHeight * DropDownCount;
        cmbPoint := ClientOrigin;
        CmbRect.Left   := CmbPoint.x + Pt.x; //cmbPoint.x + (FCmbWidth div 2);
        CmbRect.Top    := cmbPoint.y + CmbHeight + Pt.Y + 25;
        CmbRect.Bottom := CmbRect.Top + CmbHeight + 2;
        CmbRect.Right  := CmbRect.Left + HntRect.right - HntRect.Left + 5;
        If length(Items[i]) > 0 then
          FHintWin.ActivateHint(CmbRect,Items [i])
        else
          FHintWin.ReleaseHandle; // It there is a null string
      end;
    end
    else
      FHintWin.ReleaseHandle; // we are out of the list window
  end;
  with Message do
    Result:= CallWindowProc(FOldWndProc, FListHandle, Msg, wParam,lParam);
end;

procedure THintComboBox.WMLButtonUp(var Message: TMessage);
const OPNAME = 'THintComboBox.WMLButtonUp';
var
  Handle: HWnd;
  WindowClass: string;
begin
  inherited;
  if (Style <> csSimple) and DroppedDown and (FListHandle = 0) then
  begin
    Handle:= GetWindow(GetParentForm(Self).Handle, GW_HWndFirst);
    while Handle <> 0 do
    begin
      SetLength(WindowClass, 255);
      SetLength(WindowClass, GetClassName(Handle, @WindowClass[1], 255));
      if WindowClass = 'ComboLBox' then
      begin
        if IsWindowVisible(Handle) and
           (GetWindowTask(Handle) = GetWindowTask(TForm(Owner).Handle)) then
        begin
          FListHandle:= Handle;
          FOldWndProc:= pointer(SetWindowLong(FListHandle,
                            GWL_WNDPROC, longint(FNewWndProc)));
          if FOldWndProc = nil then
          begin
            raise exception.create('Failed to subclass combobox');
            exit;
          end;
        end;
      end;
      Handle:= GetWindow(Handle, GW_HWndNext);
    end;
  end;
end;
{$WARN SYMBOL_DEPRECATED ON}
end.
