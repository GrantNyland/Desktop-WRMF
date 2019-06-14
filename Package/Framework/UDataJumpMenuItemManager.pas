//
//
//  UNIT      : Contains TDataJumpMenuItemManager Class
//  AUTHOR    : Grant Nyland (PDNA)
//  DATE      : 2002/03/04
//  COPYRIGHT : Copyright © 2002 DWAF
//
//
unit UDataJumpMenuItemManager;

interface

uses
  Classes,
  UViewDataItem,
  UMenuItemManager;

type
  TDataJumpMenuItemManager = class(TMenuItemManager)
  protected
    function IsJumpDestinationAvailable(AJumpNode: TViewDataNode; AGridLoaded, AGraphLoaded: boolean): boolean;
  public
    procedure RefreshContext(AJumpList: TStringList; AGridLoaded, AGraphLoaded: boolean;
      ACurrentNodeName: string = ''; ACurrentIDValues: string = '');
  end;

implementation

uses
  SysUtils,
  UAbstractComponent,
  UMainMenuEventType,
  UErrorHandlingOperations, UAbstractObject;

const

  // Help menu items.
  CViewGotoSep : array[0..1] of string = ('View', 'ViewGotoSep');
  CViewGoto    : array[0..1] of string = ('View', 'ViewGoto');
  CViewGotoJump: array[0..2] of string = ('View', 'ViewGoto', '_To_Add_');

procedure TDataJumpMenuItemManager.RefreshContext(AJumpList: TStringList; AGridLoaded, AGraphLoaded: boolean;
  ACurrentNodeName: string = ''; ACurrentIDValues: string = '');
const OPNAME = 'TDataJumpMenuItemManager.RefreshContext';
var
  LIndex: integer;
  LMainMenuItemAdded: boolean;
  LCurrentJumpNodeData, LNewJumpNodeData: TViewDataNode;
  LLoadedJumpMenus: TStringList;
  LMenuString: string;
  LTabsheetItems : TStringList;
  LTabsheetManagesJumps : boolean;
begin
  try

    // Clear the existing menu items.
    DeleteMenuItems;
    if Assigned(FAppModules.MainForm()) and Assigned(FAppModules.MainForm().PageControl.ActivePage) then
      LTabsheetManagesJumps := TAbstractTabSheet(FAppModules.MainForm().PageControl.ActivePage).DoesTabSheetOverrideJumpToHandling()
    else
      LTabsheetManagesJumps := false;

    // Make sure that the current node is assigned.
    if (ACurrentNodeName <> '') then
    begin

      // Create a sorted string list of menu identifiers.
      LLoadedJumpMenus := TStringList.Create;
      LTabsheetItems := TStringList.Create;
      try
        LLoadedJumpMenus.Sorted := True;
        LLoadedJumpMenus.CaseSensitive := False;

        // Loop for all jump items in the jump list.
        LMainMenuItemAdded := False;
        for LIndex := 0 to AJumpList.Count - 1 do
        begin

          // Get the current jump node data in the list.
          LCurrentJumpNodeData := TViewDataNode(AJumpList.Objects[LIndex]);

          // Make sure that the FROM and TO are correct.
          if (AJumpList[LIndex] = ACurrentNodeName) and
             IsJumpDestinationAvailable(LCurrentJumpNodeData, AGridLoaded, AGraphLoaded) then
          begin
            // Add the main menu items if not yet done.
            if (not LTabsheetManagesJumps) and (not LMainMenuItemAdded) then
            begin
              AddMenuItemEntry(CViewGotoSep, 240, CmeSeparator);
              FAppModules.SetMenuItemCaption(CViewGotoSep, '-');
              AddMenuItemEntry(CViewGoto, 241, CmeNull);
              FAppModules.SetMenuItemCaption(CViewGoto, FAppModules.Language.GetString('MenuItem.ViewGoto'));
              LMainMenuItemAdded := True;
            end;

            // Construct the menu identifier string and make sure it does not already exist.
            //CViewGotoJump[2] := LCurrentJumpNodeData.ViewID;
            LMenuString := CViewGotoJump[0] + ',' + CViewGotoJump[1] + ',' + CViewGotoJump[2];

            if (LLoadedJumpMenus.IndexOf(LMenuString) < 0) then
            begin

              // Create the jump node data object.
              LNewJumpNodeData := TViewDataNode.Create;
              LNewJumpNodeData.AssignFrom(LCurrentJumpNodeData);
              LNewJumpNodeData.IDValues := ACurrentIDValues;

              // Add the object to the list.
              LLoadedJumpMenus.Add(LMenuString);

              if (not LTabsheetManagesJumps) then
              begin
                AddMenuItemEntry(CViewGotoJump, LNewJumpNodeData.Weighting, CmeDataViewJump, LNewJumpNodeData);
                FAppModules.SetMenuItemCaption(CViewGotoJump,
                  FAppModules.Language.GetString('ViewData.' + CViewGotoJump[2]));
              end else begin
                LTabSheetItems.AddObject(FAppModules.Language.GetString('ViewData.' + LCurrentJumpNodeData.ViewID), LNewJumpNodeData);
              end;
            end;
          end;
        end;
        if LTabsheetManagesJumps then
        begin
          TAbstractTabSheet(FAppModules.MainForm().PageControl.ActivePage).DoTabSheetOverrideJumpToHandling(LTabSheetItems);
        end;
      finally
        for LIndex := 0 to LTabsheetItems.Count - 1 do
          if Assigned(LTabsheetItems.Objects[LIndex]) then
            LTabsheetItems.Objects[LIndex].Free;
        LTabsheetItems.Free;
        LLoadedJumpMenus.Free;
      end;
    end else begin
      if LTabsheetManagesJumps then
        TAbstractTabSheet(FAppModules.MainForm().PageControl.ActivePage).DoTabSheetOverrideJumpToHandling(nil);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDataJumpMenuItemManager.IsJumpDestinationAvailable(
  AJumpNode: TViewDataNode; AGridLoaded, AGraphLoaded: boolean): boolean;
const OPNAME = 'TDataJumpMenuItemManager.IsJumpDestinationAvailable';
begin
  Result := False;
  try

    // Check the grid.
    if (AJumpNode.TopParentID = 'Grid') then
    begin
      Result := AGridLoaded;
    end else begin

      // Check the graph.
      if (AJumpNode.TopParentID = 'Graph') then
      begin
        Result := AGraphLoaded;
      end else begin

        // Unknown jump destination.
        Result := False;
      end;
    end;
  except on E: Exception do HandleErrorFunction(E, OPNAME, Result) end;
end;

end.
