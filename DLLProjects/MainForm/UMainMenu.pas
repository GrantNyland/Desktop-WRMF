//
//
//  UNIT      : Contains the class TMainMenu.
//  AUTHOR    : Grant Nyland
//  DATE      : 2002/02/20
//  COPYRIGHT : Copyright © 2002 DWAF
//
//
unit UMainMenu;

interface

uses
  Classes,
  Vcl.Menus,
  UAbstractObject,
  UAbstractComponent;

type
  TMainMenu = class(TAbstractMainMenu)
  protected
    function GetMenuKeysCommaText(AMenuKeys: array of string): string;
    function FindMenuItem(AMenuKeys: array of string): TAbstractMenuItem;
    function FindMenuItemChild(AParentMenuItem: TMenuItem; AKey: string): TAbstractMenuItem;
    function FindMenuItemParent(AMainMenuKeys: array of string): TAbstractMenuItem;
    function CreateCustomMenuItem(AParent: TMenuItem; AMenuCaptionKey: string; ASortWeight: integer;
                            AMenuEvent: integer = 0; AData: TObject = nil): TAbstractMenuItem;
    procedure DoMenuClick(ASender: TObject);
  public
    function AddMenuItem(AMenuKeys: array of string;
      ASortWeight: integer; AEvent: integer; AData: TObject): TAbstractMenuItem; override;
    function SetMenuItem(AMenuKeys: array of string; AnAction: TMenuSetAction; AStatusReason: string): boolean; override;
    function SetMenuItemCaption(AMenuKeys: array of string; ACaption: string): boolean; override;
    function SetMenuItemHelpContext(AMenuKeys: array of string; AHelpContextID : THelpContext): boolean; override;
    function GetMenuItemProperties(AMenuKeys: array of string) : TSetOfMenuAction; override;
  end;

implementation

uses
  Windows,
  Vcl.Controls,
  SysUtils,
  UErrorHandlingOperations;

function TMainMenu.CreateCustomMenuItem(AParent: TMenuItem; AMenuCaptionKey: string; ASortWeight: integer;
                                      AMenuEvent: integer; AData: TObject): TAbstractMenuItem;
const OPNAME = 'TMainMenu.CreateMenuItem';
var LIndex, LPlace: integer;
begin
  Result := nil;
  try
    Result := TAbstractMenuItem.Create(self, FAppModules, AMenuCaptionKey);
    Result.SortWeight := ASortWeight;
    Result.MenuEvent := AMenuEvent;
    Result.Data := AData;
    Result.OnClick := DoMenuClick;
    if Assigned(AParent) then
    begin
      LPlace := -1;
      for LIndex := 0 to AParent.Count - 1 do
      begin
        if (ASortWeight < TAbstractMenuItem(AParent.Items[LIndex]).SortWeight) then
        begin
          LPlace := LIndex;
          break;
        end;
      end;
      if (LPlace >= 0) then
      begin
        AParent.Insert(LPlace, Result);
      end
      else
      begin
        AParent.Add(Result);
      end;
    end
    else
    begin
      Items.Add(Result);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMainMenu.GetMenuKeysCommaText(AMenuKeys: array of string): string;
const OPNAME = 'TMainMenu.GetMenuKeysCommaText';
var LIndex: integer;
begin
  Result := '';
  try
    for LIndex := 0 to Length(AMenuKeys) - 1 do
      Result := Result + ',' + AMenuKeys[LIndex];
    Delete(Result, 1, 1);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMainMenu.FindMenuItem(AMenuKeys: array of string): TAbstractMenuItem;
const OPNAME = 'TMainMenu.FindMenuItem';
var
  LIndex: integer;
  LMenuItem: TMenuItem;
begin
  Result := nil;
  try
    LMenuItem := Items;
    for LIndex := 0 to Length(AMenuKeys) - 1 do
    begin
      LMenuItem := FindMenuItemChild(LMenuItem, AMenuKeys[LIndex]);
      if (not Assigned(LMenuItem)) then
        break;
      if (LIndex = Length(AMenuKeys) - 1) and Assigned(LMenuItem) then
        Result := TAbstractMenuItem(LMenuItem);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMainMenu.FindMenuItemChild(AParentMenuItem: TMenuItem; AKey: string): TAbstractMenuItem;
const OPNAME = 'TMainMenu.FindMenuItemChild';
var LIndex: integer;
begin
  Result := nil;
  try
    for LIndex := 0 to AParentMenuItem.Count - 1 do
    begin
      if (TAbstractMenuItem(AParentMenuItem.Items[LIndex]).MenuKey = AKey) then
      begin
        Result := TAbstractMenuItem(AParentMenuItem.Items[LIndex]);
        break;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMainMenu.FindMenuItemParent(AMainMenuKeys: array of string): TAbstractMenuItem;
const OPNAME = 'TMainMenu.FindMenuItemParent';
var
  LIndex: integer;
  LMenuItem: TMenuItem;
begin
  Result := nil;
  try
    LMenuItem := Items;
    for LIndex := 0 to Length(AMainMenuKeys) - 2 do
    begin
      LMenuItem := FindMenuItemChild(LMenuItem, AMainMenuKeys[LIndex]);
      if (not Assigned(LMenuItem)) then
        break;
      if (LIndex = Length(AMainMenuKeys) - 2) and Assigned(LMenuItem) then
        Result := TAbstractMenuItem(LMenuItem);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMainMenu.AddMenuItem(AMenuKeys: array of string;
  ASortWeight: integer; AEvent: integer; AData: TObject): TAbstractMenuItem;
const OPNAME = 'TMainMenu.AddMenuItem';
var
  LMenuItem: TAbstractMenuItem;
begin
  Result := nil;
  try
    if Assigned(FindMenuItem(AMenuKeys)) then
    begin
      raise Exception.CreateFmt('Menu item already exists [%s].', [GetMenuKeysCommaText(AMenuKeys)]);
    end
    else
    begin
      if (Length(AMenuKeys) > 1) then
      begin
        LMenuItem := FindMenuItemParent(AMenuKeys);
        if Assigned(LMenuItem) then
        begin
          Result := CreateCustomMenuItem(LMenuItem, AMenuKeys[High(AMenuKeys)], ASortWeight, AEvent, AData);
          // Result := True;
        end
        else
        begin
          raise Exception.CreateFmt('Parent menu item not created [%s].', [GetMenuKeysCommaText(AMenuKeys)]);
        end;
      end
      else
      begin
        Result := CreateCustomMenuItem(Items, AMenuKeys[High(AMenuKeys)], ASortWeight, AEvent, AData);
        // Result := True;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMainMenu.SetMenuItem(AMenuKeys: array of string; AnAction: TMenuSetAction; AStatusReason: string): boolean;
const OPNAME = 'TMainMenu.SetMenuItem';
var
  LMenuItem: TAbstractMenuItem;
begin
  Result := False;
  try
    LMenuItem := FindMenuItem(AMenuKeys);
    if (not Assigned(LMenuItem)) then
    begin
       Exit;
      //This code should be uncommented when debugging so that non-existing menus can be identified.
      //raise Exception.CreateFmt('Menu item not found [%s].', [GetMenuKeysCommaText(AMenuKeys)]);
    end
    else
    begin
      LMenuItem.StatusReason := AStatusReason;
      LMenuItem.LanguageHasChanged;
      case AnAction of
        msShow :
          begin
            LMenuItem.Visible := True;
          end;
        msHide :
          begin
            LMenuItem.Visible := False;
          end;
        msEnable :
          begin
            if LMenuItem.Visible then
            LMenuItem.Enabled := True;
          end;
        msDisable :
          begin
            //if LMenuItem.Visible then
            LMenuItem.Enabled := False;
          end;
        msChecked :
          begin
            //if LMenuItem.Visible then
            LMenuItem.Checked := True;
          end;
        msUnChecked :
          begin
            //if LMenuItem.Visible then
            LMenuItem.Checked := False;
          end;
        msDelete :
          begin
            LMenuItem.Parent.Remove(LMenuItem);
            LMenuItem.Free;
          end;
      else
        raise Exception.CreateFmt('Unknown menu action [%d].', [integer(AnAction)]);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMainMenu.DoMenuClick(ASender: TObject);
const OPNAME = 'TMainMenu.DoMenuClick';
begin
  try
    if (TAbstractMenuItem(ASender).MenuEvent > 0) then
      if Assigned(FAppModules) then
        FAppModules.ProcessEvent(TAbstractMenuItem(ASender).MenuEvent, TAbstractMenuItem(ASender).Data);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMainMenu.SetMenuItemCaption(AMenuKeys: array of string; ACaption: string): boolean;
const OPNAME = 'TMainMenu.SetMenuItemCaption';
var LMenuItem: TAbstractMenuItem;
begin
  Result := False;
  try
    LMenuItem := FindMenuItem(AMenuKeys);
    if (not Assigned(LMenuItem)) then
    begin
      raise Exception.CreateFmt('Menu item not found [%s].', [GetMenuKeysCommaText(AMenuKeys)]);
    end else begin
      LMenuItem.LanguageSwitchable := False;
      LMenuItem.Caption := ACaption;
      Result := True;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMainMenu.SetMenuItemHelpContext(AMenuKeys: array of string;
  AHelpContextID : THelpContext): boolean;
const OPNAME = 'TMainMenu.SetMenuItemHelpContext';
var LMenuItem: TAbstractMenuItem;
begin
  Result := False;
  try
    LMenuItem := FindMenuItem(AMenuKeys);
    if (not Assigned(LMenuItem)) then
    begin
      raise Exception.CreateFmt('Menu item not found [%s].', [GetMenuKeysCommaText(AMenuKeys)]);
    end else begin
      LMenuItem.HelpContext := AHelpContextID;
      Result := True;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMainMenu.GetMenuItemProperties(
  AMenuKeys: array of string): TSetOfMenuAction;
const OPNAME = 'TMainMenu.GetMenuItemProperties';
var
  LMenuItem : TAbstractMenuItem;
begin
  Result := [];
  try
    LMenuItem := FindMenuItem(AMenuKeys);
    if (not Assigned(LMenuItem)) then
    begin
      raise Exception.CreateFmt('Menu item not found [%s].', [GetMenuKeysCommaText(AMenuKeys)]);
    end else begin
      if LMenuItem.Checked then
        Result := Result + [msChecked]
      else
        Result := Result + [msUnChecked];
      if LMenuItem.Enabled then
        Result := Result + [msEnable]
      else
        Result := Result + [msDisable];
      if LMenuItem.Visible then
        Result := Result + [msShow]
      else
        Result := Result + [msHide];
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
