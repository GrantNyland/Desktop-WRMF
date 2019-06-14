//
//
//  UNIT      : Contains TDataTabSheetManager Class
//  AUTHOR    : Dziedzi Ramulondi (Aravia)
//  DATE      : 2003/12/10
//  COPYRIGHT : Copyright © 2003 DWAF
//
//
unit UDataTabSheetManager;

interface

uses
  UAbstractObject,
  UGenericModelLinkClasses,
  UViewDataItem,
  UGridActionObject,
  UTabSheetManager,
  UDataViewerManager;

type
  TDataTabSheetManager = class(TTabSheetManager)
  protected
    FGridEditorManager: TDataViewerManager;
    FViewDataGraphManager: TDataViewerManager;
    procedure CreateMemberObjects; override;
    procedure CreateGridEditorManager; virtual;
    procedure CreateViewDataGraphManager; virtual;
  public
    function ResetState: boolean; override;
    function Initialise: Boolean; override;
    function LanguageHasChanged: boolean; override;
    function StudyHasChanged: boolean; override;
    function StudyDataHasChanged(AContext: TChangeContext; AFieldName,AOldValue,ANewValue: string): boolean; override;
    function DoJumpRequest(AViewDataNode: TViewDataNode): boolean; override;
    function DoGridAction(AGridActionObject: TObject): boolean; override;
    function DoCustomTabSheetEvent(ACustomModelEvent: TModelMenuData): boolean; override;
    class function CanDataTabBeCreated: boolean;

    function ShowGridEditor: boolean; virtual;
    function EditGridEditor(AData: TObject): boolean; virtual;
    function ShowGraph: boolean; virtual;

    property GridEditorManager: TDataViewerManager read FGridEditorManager;
    property GraphManager: TDataViewerManager read FViewDataGraphManager;
  end;

implementation

uses
  VCL.Forms,
  SysUtils,
  UDataSheet,
  UErrorHandlingOperations;

class function TDataTabSheetManager.CanDataTabBeCreated: boolean;
const OPNAME = 'TDataTabSheetManager.CanDataTabBeCreated';
begin
  Result := False;
  try
{$IFDEF MERGE_DLLS}
    Result := True;
{$ELSE}
    Result :=
      FileExists(ExtractFilePath(ApplicationExeName) + 'bin\GridEditor.dll') or
      FileExists(ExtractFilePath(ApplicationExeName) + 'bin\ViewDataGraph.dll');
{$ENDIF}
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TDataTabSheetManager.CreateMemberObjects;
const OPNAME = 'TDataTabSheetManager.CreateMemberObjects';
begin
  try

    inherited CreateMemberObjects;
    FTabSheet := TDataSheet.Create(nil, FAppModules);
    FGridEditorManager    := nil;
    FViewDataGraphManager := nil;
    CreateGridEditorManager;
    CreateViewDataGraphManager;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TDataTabSheetManager.CreateGridEditorManager;
const OPNAME = 'TDataTabSheetManager.CreateGridEditorManager';
begin
  try
{$IFNDEF MERGE_DLLS}
    if not FileExists(ExtractFilePath(ApplicationExeName) + 'bin\GridEditor.dll') then Exit;
{$ENDIF}
    CreateDLLObject(ExtractFilePath(ApplicationExeName) + 'bin\GridEditor.dll',
      TAbstractAppObject(FGridEditorManager), FAppModules, False, OPNAME);

    {if not Assigned(FAppModules.MainForm()) then Exit;
    if not FileExists(ExtractFilePath(ApplicationExeName) + 'bin\GridEditor.dll') then Exit;
    try
      CreateDLLObject(ExtractFilePath(ApplicationExeName) + 'bin\GridEditor.dll',
        TAbstractAppObject(FGridEditorManager), FAppModules, False, OPNAME);
      if Assigned(FGridEditorManager) then
      begin
        FOwnedAppObjects.Add(FGridEditorManager);
        FGridEditorManager.TabSheet.PageControl := FAppModules.MainForm.PageControl;
      end;
      }
      {if Assigned(FViewDataManager) then
        FViewDataManager.GridLoaded := Assigned(FGridEditorManager);
      }
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TDataTabSheetManager.CreateViewDataGraphManager;
const OPNAME = 'TDataTabSheetManager.CreateViewDataGraphManager';
begin
  try
{$IFNDEF MERGE_DLLS}
    if not FileExists(ExtractFilePath(ApplicationExeName) + 'bin\ViewDataGraph.dll') then Exit;
{$ENDIF}
    CreateDLLObject(ExtractFilePath(ApplicationExeName) + 'bin\ViewDataGraph.dll',
      TAbstractAppObject(FViewDataGraphManager), FAppModules, False, OPNAME);

  {if not FileExists(ExtractFilePath(ApplicationExeName) + 'bin\ViewDataGraph.dll') then Exit;
  try
    CreateDLLObject(ExtractFilePath(ApplicationExeName) + 'bin\ViewDataGraph.dll',
      TAbstractAppObject(FViewDataGraphManager), FAppModules, False, OPNAME);
    if Assigned(FViewDataGraphManager) then
    begin
      FOwnedAppObjects.Add(FViewDataGraphManager);
      FViewDataGraphManager.TabSheet.PageControl := FAppModules.MainForm.PageControl;
    end;
    }
    {if Assigned(FViewDataManager) then
      FViewDataManager.GraphLoaded := Assigned(FViewDataGraphManager);
    }
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TDataTabSheetManager.Initialise: Boolean;
const OPNAME = 'TDataTabSheetManager.Initialise';
begin
  Result := inherited Initialise;
  try
    if Assigned(FGridEditorManager) then
      FGridEditorManager.TabSheet.PageControl := TDataSheet(FTabSheet).PageControl;
    if Assigned(FViewDataGraphManager) then
      FViewDataGraphManager.TabSheet.PageControl := TDataSheet(FTabSheet).PageControl;
    Result := Result and FTabSheet.Initialise;
    if Assigned(FGridEditorManager) then
      Result := Result and FGridEditorManager.Initialise;
    if Assigned(FViewDataGraphManager) then
      Result := Result and FViewDataGraphManager.Initialise;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TDataTabSheetManager.LanguageHasChanged: boolean;
const OPNAME = 'TDataTabSheetManager.LanguageHasChanged';
begin
  Result := inherited LanguageHasChanged;
  try
    Result := Result and FTabSheet.LanguageHasChanged;
    if Assigned(FGridEditorManager) then
      Result := Result and FGridEditorManager.LanguageHasChanged;
    if Assigned(FViewDataGraphManager) then
      Result := Result and FViewDataGraphManager.LanguageHasChanged;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TDataTabSheetManager.StudyDataHasChanged(AContext: TChangeContext; AFieldName,AOldValue,ANewValue: string): boolean;
const OPNAME = 'TDataTabSheetManager.StudyDataHasChanged';
begin
  Result := inherited StudyDataHasChanged(AContext,AFieldName,AOldValue,ANewValue);
  try
    Result := Result and FTabSheet.StudyDataHasChanged(AContext,AFieldName,AOldValue,ANewValue);
    if Assigned(FGridEditorManager) then
      Result := Result and FGridEditorManager.StudyDataHasChanged(AContext,AFieldName,AOldValue,ANewValue);
    if Assigned(FViewDataGraphManager) then
      Result := Result and FViewDataGraphManager.StudyDataHasChanged(AContext,AFieldName,AOldValue,ANewValue);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TDataTabSheetManager.StudyHasChanged: boolean;
const OPNAME = 'TDataTabSheetManager.StudyHasChanged';
begin
  Result := inherited StudyHasChanged;
  try
    Result := Result and FTabSheet.StudyHasChanged;
    if Assigned(FGridEditorManager) then
      Result := Result and FGridEditorManager.StudyHasChanged;
    if Assigned(FViewDataGraphManager) then
      Result := Result and FViewDataGraphManager.StudyHasChanged;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TDataTabSheetManager.DoGridAction(AGridActionObject: TObject): boolean;
const OPNAME = 'TDataTabSheetManager.DoGridAction';
begin
  Result := False;
  try
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDataTabSheetManager.DoJumpRequest(AViewDataNode: TViewDataNode): boolean;
const OPNAME = 'TDataTabSheetManager.DoJumpRequest';
begin
  Result := False;
  try
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDataTabSheetManager.DoCustomTabSheetEvent(ACustomModelEvent: TModelMenuData): boolean;
const OPNAME = 'TDataTabSheetManager.DoCustomTabSheetEvent';
begin
  Result := False;
  try
    if (not Result) and Assigned(FGridEditorManager) then
      Result := FGridEditorManager.DoCustomTabSheetEvent(ACustomModelEvent);
    if (not Result) and Assigned(FViewDataGraphManager) then
      Result := FViewDataGraphManager.DoCustomTabSheetEvent(ACustomModelEvent);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDataTabSheetManager.ShowGraph: boolean;
const OPNAME = 'TDataTabSheetManager.ShowGraph';
begin
  Result := False;
  try
    if Assigned(FViewDataGraphManager) then
    begin
      TDataSheet(FTabSheet).PageControl.ActivePage := FViewDataGraphManager.TabSheet;
      Result := True;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDataTabSheetManager.ShowGridEditor: boolean;
const OPNAME = 'TDataTabSheetManager.ShowGridEditor';
begin
  Result := False;
  try
    if Assigned(FGridEditorManager) then
    begin
      TDataSheet(FTabSheet).PageControl.ActivePage := FGridEditorManager.TabSheet;
      Result := True;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDataTabSheetManager.EditGridEditor(AData: TObject): boolean;
const OPNAME = 'TDataTabSheetManager.EditGridEditor';
begin
  Result := False;
  try
    if Assigned(FGridEditorManager) then
    begin
      FGridEditorManager.DoGridAction(AData);
      Result := True;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDataTabSheetManager.ResetState: boolean;
const OPNAME = 'TDataTabSheetManager.ResetState';
begin
  Result := False;
  try
    if Assigned(FGridEditorManager) then
      FGridEditorManager.ResetState;
    if Assigned(FViewDataGraphManager) then
      FViewDataGraphManager.ResetState;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
