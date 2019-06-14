//
//
//  UNIT      : Contains TDataModelMenuItemManager Class
//  AUTHOR    : Dziedzi Ramulondi(PDNA)
//  DATE      : 2002/04/24
//  COPYRIGHT : Copyright © 2002 DWAF
//
//
unit UDataModelMenuItemManager;

interface

uses
  UAbstractObject,
  UGenericModelLinkClasses,
  UMenuItemManager;

type
  TDataModelMenuItemManager = class(TMenuItemManager)
  protected
    FIsNetworkLoaded, FIsGridLoaded, FIsGraphLoaded, FIsFileSelectionLoaded: boolean;
    procedure CreateToolBar;virtual;
  public
    constructor Create(AAppModules: TAppModules;
      AIsNetworkLoaded, AIsGridLoaded, AIsGraphLoaded, AIsFileSelectionLoaded: boolean); reintroduce;
    procedure AddMenuItems; override;
  end;

implementation

uses
  SysUtils,
  UAbstractComponent,
  UMainMenuEventType,
  UErrorHandlingOperations;

const
  CViewNetworkVisualiser:  array[0..1] of string = ('View','ViewNetworkVisualiser');
  CViewEditGrid:           array[0..1] of string = ('View','ViewEditGrid');
  CViewFileSelection:      array[0..1] of string = ('View','ViewFileSelection');
  CViewGraph:              array[0..1] of string = ('View','ViewGraph');
  CViewTabSheetsSep:       array[0..1] of string = ('View','ViewTabSheetsSep');

constructor TDataModelMenuItemManager.Create(AAppModules: TAppModules;
  AIsNetworkLoaded, AIsGridLoaded, AIsGraphLoaded, AIsFileSelectionLoaded: boolean);
const OPNAME = 'TDataModelMenuItemManager.Create';
begin
  try
    FIsNetworkLoaded       := AIsNetworkLoaded;
    FIsGridLoaded          := AIsGridLoaded;
    FIsGraphLoaded         := AIsGraphLoaded;
    FIsFileSelectionLoaded := AIsFileSelectionLoaded;
    inherited Create(AAppModules);
    CreateToolBar;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDataModelMenuItemManager.AddMenuItems;
const OPNAME = 'TDataModelMenuItemManager.AddMenuItems';
begin
  try
    if FIsNetworkLoaded then
      AddMenuItemEntry(CViewNetworkVisualiser, 200, CmeViewNetworkVisualiser);
    if FIsGridLoaded then
      AddMenuItemEntry(CViewEditGrid,          210, CmeViewEditGrid);
    if FIsGraphLoaded then
      AddMenuItemEntry(CViewGraph,             220, CmeViewGraph);
    if FIsFileSelectionLoaded then
      AddMenuItemEntry(CViewFileSelection,     230, CmeViewFileEdit);
    if FIsNetworkLoaded or FIsGridLoaded or FIsGraphLoaded or FIsFileSelectionLoaded then
      AddMenuItemEntry(CViewTabSheetsSep,      250, CmeSeparator);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDataModelMenuItemManager.CreateToolBar;
const OPNAME = 'TDataModelMenuItemManager.CreateToolBar';
begin
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
