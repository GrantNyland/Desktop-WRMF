//
//
//  UNIT      : Contains TFileEditTabSheet Class
//  AUTHOR    : Dziedzi (PDNA)
//  DATE      : 2002/03/02
//  COPYRIGHT : Copyright © 2002 DWAF
//
//
unit UFileEditTabSheet;

interface

uses
  DB,
  vcl.Menus,
  Classes,
  vcl.stdctrls,
  vcl.ComCtrls,
  contnrs,
  vcl.extctrls,
  vcl.CheckLst,
  vcl.Dialogs,
  UHelpContexts,
  UAbstractObject,
  UAbstractComponent,
  UFileEditMenuItemManager,
  UGenericModelLinkClasses,
  UAbstractFileNamesObject,
  URichEditWithEvents,
  UTreeViewTabSheet;

type

  TFileEditTabSheet = class(TTreeViewTabSheet)
  protected
    // Visual  components.
    FEditPopupMenu: TPopupMenu;
    FRichEdit: TRichEditWithEvents;
    //FOpenDialog: TOpenDialog;
    FCheckListBox  : TCheckListBox;
    FClientPanel   : TPanel;
    FFieldDescription: TStatusBar;
    FOnFileSave: TOnFileSave;

    // Overriden from TAbstractTabSheet
    procedure CreateMemberObjects; override;
    function GetToolBar: TAbstractToolBar; override;
    procedure AssignHelpContext; override;

    // Introduced virtual methods that needs overriding per model.
    procedure DoTreeNodeAboutToChange(ASender: TObject; ANode: TTreeNode; var AAllowChange: Boolean); virtual;
    procedure DoTreeNodeHasChanged(ASender: TObject; ANode: TTreeNode); virtual;

    // Introduced in this class.
    procedure TogglePanel(AEnabled:Boolean);
    function GetMenuItemManager: TFileEditMenuItemManager;
    procedure EditSelectedFile(ADataObject: TAbstractModelFileName);
    procedure SetFileEditMenuState(ANode: TTreeNode);
    procedure SetSelectedState;
    function DoSaveFile: boolean;
    procedure OnFileEditChange(Sender: TObject);
    procedure DoSelectionChange(Sender: TObject);
    procedure UpdateHintText;
    procedure UpdateFileHints(AFileNameObject:TAbstractModelFileName);
    procedure DoHide; override;
  public

    function Initialise: boolean; override;
    function LanguageHasChanged: boolean; override;
    function StudyHasChanged: boolean; override;
    function StudyDataHasChanged(AContext: TChangeContext; AFieldName,AOldValue,ANewValue: string): boolean; override;
    procedure SetMenuSaveFile(AAction: TMenuSetAction);
    function CanCopyToCLipboard: boolean; override;
    function CanExport: boolean; override;
    function CanPrint: boolean; override;
    procedure DoCopyToCLipboard; override;
    procedure DoExport(AFileName: string = ''); override;
    procedure DoPrint; override;
    procedure TabHasChanged; override;
    procedure HideTabsheet;
    procedure ShowTabsheet;
    function RefreshSelectedFile: boolean;
    function DoCustomTabSheetEvent(ACustomModelEvent: TModelMenuData): boolean; override;
    property MenuItemManager  : TFileEditMenuItemManager read GetMenuItemManager;
    property OnFileSave       : TOnFileSave read FOnFileSave write FOnFileSave;
  end;

implementation

{$WARN UNIT_PLATFORM OFF}

uses
  windows,
  vcl.Forms,
  vcl.Graphics,
  SysUtils,
  vcl.Controls,
  vcl.FileCtrl,
  UUtilities,
  UConstants,
  UFileNameConstants,
  UMainMenuEventType,
  UErrorHandlingOperations;

{ TFileEditTabSheet }

procedure TFileEditTabSheet.CreateMemberObjects;
const OPNAME = 'TFileEditTabSheet.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
    ParentShowHint := False;

    FTabCaptionKey := 'FileEdit';

    FOnFileSave := nil;

    // Create the menu item manager.
    FMenuItemManager := TFileEditMenuItemManager.Create(FAppModules);

    FClientPanel            := TPanel.Create(Self);
    FClientPanel.Parent     := self;
    FClientPanel.Align      := alClient;
    FClientPanel.Color      := Self.Color;

    FCheckListBox            := TCheckListBox.Create(FClientPanel);
    FCheckListBox.Parent     := FClientPanel;
    FCheckListBox.Align      := alBottom;
    FCheckListBox.Height     := 17;
    FCheckListBox.ItemHeight := 13;
    FCheckListBox.Enabled    := False;
    FCheckListBox.Font.Name  := 'Courier';
    FCheckListBox.Visible    := False;

    // Create the memo.
    FRichEdit := TRichEditWithEvents.Create(FClientPanel,FAppModules);
    FRichEdit.Parent            := FClientPanel;
    FRichEdit.Visible           := False;
    FRichEdit.Align             := alClient;
    FRichEdit.ScrollBars        := ssBoth;
    FRichEdit.ReadOnly          := True;
    FRichEdit.Font.Name         := 'Courier';
    FRichEdit.WordWrap          := False;
    FRichEdit.WantTabs          := False;
    FRichEdit.WantReturns       := True;
    FRichEdit.PlainText         := True;
    FRichEdit.ShowHint          := False;
    FRichEdit.ParentShowHint    := False;
    FRichEdit.OnSelectionChange := DoSelectionChange;
    //FRichEdit.Enabled           := LEnabled;

    // Create the statust bar.
    FFieldDescription := TStatusBar.Create(FClientPanel);
    FFieldDescription.Parent := FClientPanel;
    FFieldDescription.SimplePanel := True;
    FFieldDescription.Visible := False;

    // Put these event handlers last.
    FTreeView.ReadOnly   := True;
    FTreeView.OnChanging := DoTreeNodeAboutToChange;
    FTreeView.OnChange   := DoTreeNodeHasChanged;

    FEditPopupMenu := TPopupMenu.Create(Self);

  // Handle exceptions.
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TFileEditTabSheet.AssignHelpContext;
const OPNAME = 'TFileEditTabSheet.AssignHelpContext';
begin
  try
    SetControlHelpContext(Self,HC_FileSelection);
    SetControlHelpContext(FTreeView,HC_FileSelectionTreeView);
    SetControlHelpContext(FRichEdit,HC_FileViewer);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TFileEditTabSheet.StudyHasChanged: boolean;
const OPNAME = 'TFileEditTabSheet.StudyHasChanged';
begin
  Result := inherited StudyHasChanged;
  try
    Result := Result and MenuItemManager.StudyHasChanged and
              FRichEdit.StudyHasChanged;
    TogglePanel(False);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFileEditTabSheet.GetMenuItemManager: TFileEditMenuItemManager;
const OPNAME = 'GetMenuItemManager: TFileEditMenuItemManager';
begin
  Result := nil;
  try
    Result := TFileEditMenuItemManager(FMenuItemManager);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFileEditTabSheet.LanguageHasChanged: boolean;
const OPNAME = 'TFileEditTabSheet.LanguageHasChanged';
var
  LCount: integer;
  LDataObject: TAbstractFileNamesList;
  LNode: TTreeNode;
begin
  Result := inherited LanguageHasChanged;;
  try
    Result :=  FMenuItemManager.LanguageHasChanged and
               FRichEdit.LanguageHasChanged;
    for LCount := 0 to  FTreeView.Items.Count -1 do
    begin
      LNode := FTreeView.Items[LCount];
      if (LNode.Level = 0)  and Assigned(LNode.Data) then
      begin
        LDataObject := TAbstractFileNamesList(LNode.Data);
        LNode.Text := FAppModules.Language.GetString(LDataObject.CaptionStr);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFileEditTabSheet.EditSelectedFile(ADataObject: TAbstractModelFileName);
const OPNAME = 'TFileEditTabSheet.EditSelectedFile';
var
  LCursor: TCursor;
  LReadOnly: boolean;
begin
  try
    if not ADataObject.FileFound then
    begin
      //FRichEdit.Lines.Text := FAppModules.Language.GetString('TFileSelectionDataSheet.FileNoExist');
      //FRichEdit.ReadOnly := True;
      //FRichEdit.Color := clInactiveBorder;
      TogglePanel(True);
      FRichEdit.Visible := False;
    end
    else
    begin
      LCursor := Screen.Cursor;
      try
        Screen.Cursor := crHourGlass;
        FRichEdit.Lines.LoadFromFile(ADataObject.FileName);
        LReadOnly := ADataObject.FileReadOnly;
        if not LReadOnly then
          LReadOnly := not (FAppModules.User.UserRights in CUR_EditData);;
        if (not LReadOnly) and (FAppModules.StudyArea <> nil) then
          LReadOnly    :=   FAppModules.StudyArea.ScenarioLocked;

        if LReadOnly then
        begin
          FRichEdit.ReadOnly := True;
          FRichEdit.Color := clInactiveBorder;
        end
        else
        begin
          FRichEdit.Color    := clWindow;
          FRichEdit.ReadOnly := False;
          FRichEdit.OnChange := OnFileEditChange;
        end;
        TogglePanel(True);
        UpdateFileHints(ADataObject);
      finally
        Screen.Cursor := LCursor;
      end;
    end;
    FRichEdit.SetMenuState;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


procedure TFileEditTabSheet.DoTreeNodeAboutToChange(ASender: TObject; ANode: TTreeNode; var AAllowChange: Boolean);
const OPNAME = 'TFileEditTabSheet.DoTreeNodeAboutToChange';
begin
  try
    if (csDestroying in Self.ComponentState) then Exit;
    FRichEdit.OnChange    := nil;
    TogglePanel(False);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFileEditTabSheet.DoTreeNodeHasChanged(ASender: TObject; ANode: TTreeNode);
const OPNAME = 'TFileEditTabSheet.DoTreeNodeHasChanged';
begin
  try
    if (csDestroying in Self.ComponentState) then Exit;
    FTreeView.Hint := '';
    if Assigned(FAppModules) then
    begin
      //LockWindowUpdate(Self.Handle);
      //try
        FRichEdit.OnChange    := nil;
        SetSelectedState;
        if(ANode.Level = 1) and Assigned(ANode.Data) then
        begin
          EditSelectedFile(TAbstractModelFileName(ANode.Data));
          FFieldDescription.SimpleText := TAbstractModelFileName(ANode.Data).FileName;
          FTreeView.Hint := TAbstractModelFileName(ANode.Data).FileName;
        end;

        FAppModules.ProcessEvent(CmeRefreshMenuItems,nil);
        SetFileEditMenuState(ANode);
      //finally
      //  LockWindowUpdate(0);
      //end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFileEditTabSheet.GetToolBar: TAbstractToolBar;
const OPNAME = 'TFileEditTabSheet.GetToolBar';
begin
  Result := nil;
  try
    Result := TFileEditMenuItemManager(FMenuItemManager).ToolBar;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFileEditTabSheet.Initialise: boolean;
const OPNAME = 'TFileEditTabSheet.Initialise';
var
  LOnTreeViewNodeChangedEvent  :TTVChangedEvent;
  LOnTreeViewNodeChangingEvent :TTVChangingEvent;
begin
  Result := False;
  try
    FTreeView.ShowHint := True;

    LOnTreeViewNodeChangedEvent  := FTreeView.OnChange;
    LOnTreeViewNodeChangingEvent := FTreeView.OnChanging;
    try
      FTreeView.OnChange   := nil;
      FTreeView.OnChanging := nil;
      FTreeView.Selected   := nil;

      FTreeView.Items.Clear;

    finally
      FTreeView.OnChange   := LOnTreeViewNodeChangedEvent;
      FTreeView.OnChanging := LOnTreeViewNodeChangingEvent;
    end;

    FCheckListBox.Columns := 3;
    FCheckListBox.Items.Clear;
    FCheckListBox.Items.Add(FAppModules.Language.GetString('FileStatus.strFileImported'));
    FCheckListBox.Items.Add(FAppModules.Language.GetString('FileStatus.strFileInDir'));
    FCheckListBox.Items.Add(FAppModules.Language.GetString('FileStatus.strFileReadOnly'));
    FRichEdit.Initialise;
    Result := FMenuItemManager.Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFileEditTabSheet.TogglePanel(AEnabled: Boolean);
const OPNAME = 'TFileEditTabSheet.TogglePanel';
begin
  try
    FCheckListBox.Visible := AEnabled;
    FRichEdit.Visible     := AEnabled;
    FFieldDescription.Visible := AEnabled;
    FFieldDescription.SimpleText := '';
    FRichEdit.Hint := '';

    if not AEnabled then
      FRichEdit.Clear
    else
    begin
      FRichEdit.Invalidate;
      FRichEdit.Refresh;
    end;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFileEditTabSheet.DoSaveFile: boolean;
const OPNAME = 'TFileEditTabSheet.DoSaveFile';
var
  LFileName: string;
  LFileNameObject : TAbstractModelFileName;
begin
  Result := False;
  try
    if Assigned(FTreeView.Selected) and Assigned(FTreeView.Selected.Data) then
    begin
      LFileNameObject := TAbstractModelFileName(FTreeView.Selected.Data);
      LFileName       := LFileNameObject.FileName;
      FRichEdit.Lines.SaveToFile(LFileName);
      SetMenuSaveFile(msDisable);
      Result := True;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFileEditTabSheet.SetMenuSaveFile(AAction: TMenuSetAction);
const OPNAME = 'TFileEditTabSheet.SetMenuSaveFile';
begin
  try
    TFileEditMenuItemManager(FMenuItemManager).SetMenuSaveFile(AAction);
    FRichEdit.SetMenuState;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFileEditTabSheet.CanCopyToClipboard: boolean;
const OPNAME = 'TFileEditTabSheet.CanCopyToClipboard';
begin
  Result := False;
  try
    Result := FRichEdit.Visible and (FRichEdit.Lines.Count > 0);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFileEditTabSheet.CanExport: boolean;
const OPNAME = 'TFileEditTabSheet.CanExport';
begin
  Result := False;
  try
    Result := FRichEdit.Visible and (FRichEdit.Lines.Count > 0);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFileEditTabSheet.CanPrint: boolean;
const OPNAME = 'TFileEditTabSheet.CanPrint';
begin
  Result := False;
  try
    Result := FRichEdit.Visible and (FRichEdit.Lines.Count > 0);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFileEditTabSheet.DoCopyToClipboard;
const OPNAME = 'TFileEditTabSheet.DoCopyToClipboard';
begin
  try
    if(FRichEdit.SelText = '') then
    begin
      FRichEdit.SelectAll;
      FRichEdit.CopyToClipboard;
      FRichEdit.SelLength := 0;
    end
    else
    FRichEdit.CopyToClipboard;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFileEditTabSheet.DoExport(AFileName: string = '');
const OPNAME = 'TFileEditTabSheet.DoExport';
var
  LNewFileName: string;
begin
  try
   LNewFileName := InputBox('Enter new file name and extension','Output File Name',AFileName);
   if(Trim(LNewFileName) <> '') then
   begin
     if (ExtractFilePath(LNewFileName) = '') then
       LNewFileName := TAbstractModelFileName(FTreeView.Selected.Data).FilePath + LNewFileName;
     FRichEdit.Lines.SaveToFile(LNewFileName);
   end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFileEditTabSheet.DoPrint;
const OPNAME = 'TFileEditTabSheet.DoPrint';
begin
  try
    FRichEdit.Print(FTreeView.Selected.Text);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFileEditTabSheet.DoCustomTabSheetEvent(ACustomModelEvent: TModelMenuData): boolean;
const OPNAME = 'TFileEditTabSheet.DoCustomTabSheetEvent';
begin
  Result := False;
  try
    if (ACustomModelEvent.Action = meSaveFile) then
    begin
      if DoSaveFile then
        if Assigned(FOnFileSave) then
           FOnFileSave;
      FRichEdit.SetMenuState;
      Result := True;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFileEditTabSheet.SetFileEditMenuState(ANode: TTreeNode);
const OPNAME = 'TFileEditTabSheet.SetFileEditMenuState';
var
  LDataObject: TAbstractModelFileName;
  LCount : integer;
begin
  try
    MenuItemManager.SetMenuImportFile(msDisable);
    MenuItemManager.SetMenuExportFile(msDisable);
    MenuItemManager.SetMenuSaveFile(msDisable);
    MenuItemManager.SetMenuValidateFile(msDisable);

    for LCount := 0 to  FCheckListBox.Items.Count - 1 do
      FCheckListBox.Checked[LCount] := False;

    if Assigned(ANode) and (ANode.Level = 1) and Assigned(ANode.Data) then
    begin
      LDataObject := TAbstractModelFileName(ANode.Data);

      if LDataObject.FileFound then
      begin
        FCheckListBox.Checked[1] := True;

        if LDataObject.FileReadOnly then
          FCheckListBox.Checked[2] := True;

        if LDataObject.Validatable then
          MenuItemManager.SetMenuValidateFile(msEnable)
        else
          MenuItemManager.SetMenuValidateFile(msDisable);

        if LDataObject.Importable then
          MenuItemManager.SetMenuImportFile(msEnable)
        else
         MenuItemManager.SetMenuImportFile(msDisable)

      end;

      if LDataObject.SavedInDB then
      begin
        FCheckListBox.Checked[0] := True;
        if LDataObject.Exportable then
          MenuItemManager.SetMenuExportFile(msEnable)
        else
          MenuItemManager.SetMenuExportFile(msDisable)
      end;
    end
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFileEditTabSheet.SetSelectedState;
const OPNAME = 'TFileEditTabSheet.SetSelectedState';
var
  LIndex: integer;
  LFile03Name: TAbstractModelFileName;
begin
  try
    LFile03Name := nil;
    for LIndex := 0 to FTreeView.Items.Count - 1 do
    begin
      if (FTreeView.Items[LIndex].Level = 1) then
      begin
        TAbstractModelFileName(FTreeView.Items[LIndex].Data).Selected := FTreeView.Items[LIndex].Selected;

        if(TAbstractModelFileName(FTreeView.Items[LIndex].Data).StandardFileName = sfnFile03) then
          LFile03Name := TAbstractModelFileName(FTreeView.Items[LIndex].Data);

        if(TAbstractModelFileName(FTreeView.Items[LIndex].Data).StandardFileName = sfnFile10) then
          if(TAbstractModelFileName(FTreeView.Items[LIndex].Data).Selected  and Assigned(LFile03Name)) then
            LFile03Name.Selected := True;

        if(TAbstractModelFileName(FTreeView.Items[LIndex].Data).StandardFileName = sfnFile11) then
          if(TAbstractModelFileName(FTreeView.Items[LIndex].Data).Selected  and Assigned(LFile03Name)) then
            LFile03Name.Selected := True;

        if(TAbstractModelFileName(FTreeView.Items[LIndex].Data).StandardFileName = sfnFile12) then
          if(TAbstractModelFileName(FTreeView.Items[LIndex].Data).Selected and Assigned(LFile03Name)) then
            LFile03Name.Selected := True;

        if(TAbstractModelFileName(FTreeView.Items[LIndex].Data).StandardFileName = sfnFile13) then
          if(TAbstractModelFileName(FTreeView.Items[LIndex].Data).Selected  and Assigned(LFile03Name)) then
            LFile03Name.Selected := True;

        if(TAbstractModelFileName(FTreeView.Items[LIndex].Data).StandardFileName = sfnFile19) then
          if(TAbstractModelFileName(FTreeView.Items[LIndex].Data).Selected  and Assigned(LFile03Name)) then
            LFile03Name.Selected := True
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFileEditTabSheet.OnFileEditChange(Sender: TObject);
const OPNAME = 'TFileEditTabSheet.OnFileEditChange';
begin
  try
    if FRichEdit.Modified then
      MenuItemManager.SetMenuSaveFile(msEnable)
    else
      MenuItemManager.SetMenuSaveFile(msDisable);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFileEditTabSheet.StudyDataHasChanged(AContext: TChangeContext; AFieldName,AOldValue,ANewValue: string): boolean;
const OPNAME = 'TFileEditTabSheet.StudyDataHasChanged';
{var
 LNode : TTreeNode;
 LIndex: integer;
 LDataObject:TAbstractModelFileName;
}
begin
  Result := inherited StudyDataHasChanged(AContext,AFieldName,AOldValue,ANewValue);
  try
    if(AFieldName = 'ParamFile') then
    begin
      {for LIndex := 0 to FTreeViewNodes.Count -1 do
      begin
        LNode := TTreeNode(FTreeViewNodes.Objects[LIndex]);
        if Assigned(LNode)  and Assigned(LNode.Data)then
        begin
          if(LNode.Level = 0) then Continue;
          LDataObject := TAbstractModelFileName(LNode.Data);
          if Assigned(LDataObject) then
          begin
            LNode.Text := LDataObject.ShortName;
          end;
        end;
      end;}
      RefreshSelectedFile;
    end
    else
    if(AContext in [sdccImport,sdccExport]) then
      RefreshSelectedFile;

    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFileEditTabSheet.DoSelectionChange(Sender: TObject);
const OPNAME = 'TFileEditTabSheet.DoSelectionChange';
begin
  try
    UpdateHintText;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFileEditTabSheet.UpdateHintText;
const OPNAME = 'TFileEditTabSheet.UpdateHintText';
var
  LGridCoords, LLineType: string;
  LDataObject: TAbstractModelFileName;
  LFieldProperties: TAbstractFieldProperty;
  LGridHintText: string;
  LLineNumber, LCharacterPosition: integer;
begin
  try
    LLineNumber := FRichEdit.CaretPos.Y + 1;
    LCharacterPosition := FRichEdit.CaretPos.X;
    LGridCoords := '(' + IntToStr(LLineNumber) + ',' + IntToStr(LCharacterPosition+1) + ')';
    LGridHintText := '';
    if Assigned(FTreeView.Selected) and
       Assigned(FTreeView.Selected.Data) and
       TObject(FTreeView.Selected.Data).ClassNameIs('TFileNameObject') then
    begin
      LDataObject := TAbstractModelFileName(FTreeView.Selected.Data);
      if LDataObject.HintsSet then
        LLineType := FAppModules.Model.GetFileLineType(LDataObject,LLineNumber)
      else
        LLineType := '';
      LFieldProperties := FAppModules.FieldProperties.FieldPropertyFromFileReference(
        LDataObject.StandardFileName, LLineType, LCharacterPosition);
      if Assigned(LFieldProperties) then
      begin
        LGridHintText := LFieldProperties.FieldDescription;
        LGridHintText := LFieldProperties.FieldName + ': ' + FAppModules.Language.GetString(LGridHintText);
      end;
    end;
    if (LGridHintText = '') then
    begin
      LGridHintText := LGridCoords;
    end else begin
      LGridHintText := LGridCoords + ' ' + LGridHintText;
    end;
    FRichEdit.Hint := LGridHintText;
    FFieldDescription.SimpleText := LGridHintText;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFileEditTabSheet.UpdateFileHints(AFileNameObject:TAbstractModelFileName);
const OPNAME = 'TFileEditTabSheet.UpdateFileHints';
begin
  try
    if Assigned(AFileNameObject) then
    begin
      if not AFileNameObject.HintsSet then
      begin
        FFieldDescription.SimpleText := FAppModules.Language.GetString('TFileEditTabSheet.strGettingHints');
        Application.ProcessMessages;
        FAppModules.ProcessEvent(CmeRefreshFileHints,nil);
        FFieldDescription.SimpleText := '';
      end;
    end;
  // Handle exceptions.
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TFileEditTabSheet.RefreshSelectedFile: boolean;
const OPNAME = 'TFileEditTabSheet.RefreshSelectedFile';
begin
  Result := False;
  try
    if(FTreeView.Selected <> nil) then
    DoTreeNodeHasChanged(Self,FTreeView.Selected);
    {SetSelectedState;
    FRichEdit.SetMenuState;
    FAppModules.ProcessEvent(CmeRefreshMenuItems,nil);
    SetFileEditMenuState(FTreeView.Selected); }
    Result := True;
  // Handle exceptions.
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TFileEditTabSheet.TabHasChanged;
const OPNAME = 'TFileEditTabSheet.TabHasChanged';
begin
  inherited;
  try
    RefreshSelectedFile;
  // Handle exceptions.
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TFileEditTabSheet.DoHide;
const OPNAME = 'TFileEditTabSheet.DoHide';
begin
  inherited;
//
end;

const LMenu: array[0..1] of WideString = ('View','ViewFileSelection');

procedure TFileEditTabSheet.HideTabsheet;
const OPNAME = 'TFileEditTabSheet.HideTabsheet';
begin
  try
    TFileEditMenuItemManager(FMenuItemManager).SetMenuSaveFile(msHide);
    TFileEditMenuItemManager(FMenuItemManager).SetMenuValidateFile(msHide);
    TFileEditMenuItemManager(FMenuItemManager).SetMenuImportFile(msHide);
    TFileEditMenuItemManager(FMenuItemManager).SetMenuExportFile(msHide);
    FAppModules.SetMenuItem(LMenu,msHide);
  // Handle exceptions.
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TFileEditTabSheet.ShowTabsheet;
const OPNAME = 'TFileEditTabSheet.ShowTabsheet';
begin
  try
    TFileEditMenuItemManager(FMenuItemManager).SetMenuSaveFile(msShow);
    TFileEditMenuItemManager(FMenuItemManager).SetMenuValidateFile(msShow);
    TFileEditMenuItemManager(FMenuItemManager).SetMenuImportFile(msShow);
    TFileEditMenuItemManager(FMenuItemManager).SetMenuExportFile(msShow);
    FAppModules.SetMenuItem(LMenu,msShow);
  // Handle exceptions.
  except on E : Exception do HandleError(E,OPNAME); end;
end;

end.

