//
//
//  UNIT      : Contains TMenuItemManager Class
//  AUTHOR    : Grant Nyland (PDNA)
//  DATE      : 2002/03/04
//  COPYRIGHT : Copyright © 2002 DWAF
//
//
unit UMenuItemManager;

interface

uses
  Classes,
  Contnrs,
  UAbstractComponent,
  UAbstractObject;

type
  TMenuItemEntry = class(TObject)
  public
    MenuKeys: array of string;
  end;
  TMenuItemManager = class(TAbstractAppObject, IMenuItemManager)
  protected
    FItems: TObjectList;
    FMenuItemPtr : TList;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    function GetMenuKeysCommaText(AMenuKeys: array of string): string;
    procedure AddMenuItemEntry(AMenuKeys: array of string; ASortWeight: integer;
      AEvent: integer = CmeNull; AData: TObject = nil); virtual;
    procedure SetMenuItems(AnAction: TMenuSetAction; AStatusReason: string = ''); virtual;
  public
    procedure AddMenuItems; virtual;
    procedure DeleteMenuItems; virtual;
    procedure Show; virtual;
    procedure Hide; virtual;
    procedure Enable(AStatusReason: string = ''); virtual;
    procedure Disable(AStatusReason: string = ''); virtual;
  end;

implementation

uses
  SysUtils,
  UErrorHandlingOperations;

procedure TMenuItemManager.CreateMemberObjects;
const OPNAME = 'TMenuItemManager.CreateMemberObjects';
begin
  try
    FItems := TObjectList.Create;
    FMenuItemPtr := TList.Create;
    AddMenuItems;
   // Hide;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TMenuItemManager.DestroyMemberObjects;
const OPNAME = 'TMenuItemManager.DestroyMemberObjects';
begin
  try
    DeleteMenuItems;
    FreeAndNil(FItems);
    FreeAndNil(FMenuItemPtr);
    inherited DestroyMemberObjects;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMenuItemManager.GetMenuKeysCommaText(AMenuKeys: array of string): string;
const OPNAME = 'TMenuItemManager.GetMenuKeysCommaText';
var LIndex: integer;
begin
  Result := '';
  try
    for LIndex := 0 to Length(AMenuKeys) - 1 do
      Result := Result + ',' + AMenuKeys[LIndex];
    Delete(Result, 1, 1);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMenuItemManager.AddMenuItemEntry(AMenuKeys: array of string;
  ASortWeight: integer; AEvent: integer = CmeNull; AData: TObject = nil);
const OPNAME = 'TMenuItemManager.AddMenuItemEntry';
var
  LItem: TMenuItemEntry;
  LIndex: integer;
begin
  try
    LItem := TMenuItemEntry.Create;
    SetLength(LItem.MenuKeys, Length(AMenuKeys));
    for LIndex := 0 to Length(AMenuKeys) - 1 do
      LItem.MenuKeys[LIndex] := AMenuKeys[LIndex];
    FItems.Add(LItem);
    FAppModules.AddMenuItem(LItem.MenuKeys, ASortWeight, AEvent, AData);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMenuItemManager.AddMenuItems;
const OPNAME = 'TMenuItemManager.AddMenuItems';
begin
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMenuItemManager.SetMenuItems(AnAction: TMenuSetAction; AStatusReason: string = '');
const OPNAME = 'TMenuItemManager.SetMenuItems';
var LIndex: integer;
begin
  try

    // Run in reversed order because one of the actions can be a delete.
    for LIndex := FItems.Count - 1 downto 0 do
      FAppModules.SetMenuItem(TMenuItemEntry(FItems[LIndex]).MenuKeys, AnAction, AStatusReason);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMenuItemManager.DeleteMenuItems;
const OPNAME = 'TMenuItemManager.DeleteMenuItems';
begin
  try
    SetMenuItems(msDelete);
    FItems.Clear;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMenuItemManager.Show;
const OPNAME = 'TMenuItemManager.Show';
begin
  try
    SetMenuItems(msShow);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMenuItemManager.Hide;
const OPNAME = 'TMenuItemManager.Hide';
begin
  try
    SetMenuItems(msHide);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMenuItemManager.Enable(AStatusReason: string = '');
const OPNAME = 'TMenuItemManager.Enable';
begin
  try
    SetMenuItems(msEnable, AStatusReason);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMenuItemManager.Disable(AStatusReason: string = '');
const OPNAME = 'TMenuItemManager.Disable';
begin
  try
    SetMenuItems(msDisable, AStatusReason);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
