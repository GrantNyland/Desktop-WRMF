//
//
//  UNIT      : Contains TResultsTabSheetManager Class
//  AUTHOR    : Dziedzi Ramulondi (PDNA)
//  DATE      : 2002/08/22
//  COPYRIGHT : Copyright © 2002 DWAF
//
//
unit UResultsTabSheetManager;

interface

uses
  UAbstractObject,
  UGenericModelLinkClasses,
  UViewDataItem,
  UGridActionObject,
  UTabSheetManager,
  UDataViewerManager;

type
  TResultsTabSheetManager = class(TTabSheetManager)
  protected
    FViewOutputGraphManager: TTabSheetManager;
    FGridOutputEditorManager: TTabSheetManager;
    FTimeSeriesComparitorManager: TDataViewerManager;
    procedure CreateMemberObjects; override;
    procedure CreateViewOutputGraphManager; virtual;
    procedure CreateGridOutputEditorManager; virtual;
    procedure CreateTimeSeriesComparitorManager; virtual;
  public
    function ResetState: boolean; override;
    function Initialise: Boolean; override;
    function LanguageHasChanged: boolean; override;
    function StudyHasChanged: boolean; override;
    function StudyDataHasChanged(AContext: TChangeContext; AFieldName,AOldValue,ANewValue: string): boolean; override;
    function DoJumpRequest(AViewDataNode: TViewDataNode): boolean; override;
    function DoGridAction(AGridActionObject: TObject): boolean; override;
    function DoCustomTabSheetEvent(ACustomModelEvent: TModelMenuData): boolean; override;
    function ProcessMetaDataEvent : boolean; override;
    class function CanResultsTabBeCreated: boolean;
    property ViewOutputGraphManager: TTabSheetManager read FViewOutputGraphManager;
    property GridOutputEditorManager : TTabsheetManager read FGridOutputEditorManager;
    property TimeSeriesComparitorManager: TDataViewerManager read FTimeSeriesComparitorManager;
  end;

implementation

uses
  VCL.Forms,
  SysUtils,
  UResultsSheet,
  UErrorHandlingOperations;

class function TResultsTabSheetManager.CanResultsTabBeCreated: boolean;
const OPNAME = 'TResultsTabSheetManager.CanResultsTabBeCreated';
begin
  Result := False;
  try
    Result :=
      FileExists(ExtractFilePath(ApplicationExeName) + 'bin\ViewOutputGraph.dll') or
      FileExists(ExtractFilePath(ApplicationExeName) + 'bin\TimeSeriesComparitor.dll') or
      FileExists(ExtractFilePath(ApplicationExeName) + 'bin\YieldReliabilityCurve.dll') or
      FileExists(ExtractFilePath(ApplicationExeName) + 'bin\GridOutputEditor.dll');
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TResultsTabSheetManager.CreateMemberObjects;
const OPNAME = 'TResultsTabSheetManager.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
    FTabSheet := TResultsSheet.Create(nil, FAppModules);
    CreateViewOutputGraphManager;
    CreateGridOutputEditorManager;    
    CreateTimeSeriesComparitorManager;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TResultsTabSheetManager.CreateViewOutputGraphManager;
const OPNAME = 'TResultsTabSheetManager.CreateViewOutputGraphManager';
begin
  try
{$IFNDEF MERGE_DLLS}
    if not FileExists(ExtractFilePath(ApplicationExeName) + 'bin\ViewOutputGraph.dll') then Exit;
{$ENDIF}
    CreateDLLObject(ExtractFilePath(ApplicationExeName) + 'bin\ViewOutputGraph.dll',
      TAbstractAppObject(FViewOutputGraphManager), FAppModules, False, OPNAME);
      
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TResultsTabSheetManager.CreateGridOutputEditorManager;
const OPNAME = 'TResultsTabSheetManager.CreateGridOutputEditorManager';
begin
  try
{$IFNDEF MERGE_DLLS}
    if not FileExists(ExtractFilePath(ApplicationExeName) + 'bin\GridOutputEditor.dll') then Exit;
{$ENDIF}
    CreateDLLObject(ExtractFilePath(ApplicationExeName) + 'bin\GridOutputEditor.dll',
      TAbstractAppObject(FGridOutputEditorManager), FAppModules, False, OPNAME);

  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TResultsTabSheetManager.CreateTimeSeriesComparitorManager;
const OPNAME = 'TResultsTabSheetManager.CreateTimeSeriesComparitorManager';
begin
  try
{$IFNDEF MERGE_DLLS}
    if not FileExists(ExtractFilePath(ApplicationExeName) + 'bin\TimeSeriesComparitor.dll') then Exit;
{$ENDIF}
    CreateDLLObject(ExtractFilePath(ApplicationExeName) + 'bin\TimeSeriesComparitor.dll',
      TAbstractAppObject(FTimeSeriesComparitorManager), FAppModules, False, OPNAME);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TResultsTabSheetManager.Initialise: Boolean;
const OPNAME = 'TResultsTabSheetManager.Initialise';
begin
  Result := inherited Initialise;
  try

    if Assigned(FGridOutputEditorManager) then
      FGridOutputEditorManager.TabSheet.PageControl := TResultsSheet(FTabSheet).PageControl;
    if Assigned(FViewOutputGraphManager) then
      FViewOutputGraphManager.TabSheet.PageControl := TResultsSheet(FTabSheet).PageControl;
    if Assigned(FTimeSeriesComparitorManager) then
      FTimeSeriesComparitorManager.TabSheet.PageControl := TResultsSheet(FTabSheet).PageControl;
    Result := Result and FTabSheet.Initialise;

    if Assigned(FViewOutputGraphManager) then
      Result := Result and FViewOutputGraphManager.Initialise;
    if Assigned(FGridOutputEditorManager) then
      Result := Result and FGridOutputEditorManager.Initialise;
    if Assigned(FTimeSeriesComparitorManager) then
      Result := Result and FTimeSeriesComparitorManager.Initialise;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TResultsTabSheetManager.LanguageHasChanged: boolean;
const OPNAME = 'TResultsTabSheetManager.LanguageHasChanged';
begin
  Result := inherited LanguageHasChanged;
  try
    Result := Result and FTabSheet.LanguageHasChanged;

    if Assigned(FViewOutputGraphManager) then
      Result := Result and FViewOutputGraphManager.LanguageHasChanged;
    if Assigned(FGridOutputEditorManager) then
      Result := Result and FGridOutputEditorManager.LanguageHasChanged;
    if Assigned(FTimeSeriesComparitorManager) then
      Result := Result and FTimeSeriesComparitorManager.LanguageHasChanged;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TResultsTabSheetManager.StudyDataHasChanged(AContext: TChangeContext; AFieldName,AOldValue,ANewValue: string): boolean;
const OPNAME = 'TResultsTabSheetManager.StudyDataHasChanged';
begin
  Result := inherited StudyDataHasChanged(AContext,AFieldName,AOldValue,ANewValue);
  try

    Result := Result and FTabSheet.StudyDataHasChanged(AContext,AFieldName,AOldValue,ANewValue);
    if Assigned(FViewOutputGraphManager) then
      Result := Result and FViewOutputGraphManager.StudyDataHasChanged(AContext,AFieldName,AOldValue,ANewValue);
    if Assigned(FGridOutputEditorManager) then
      Result := Result and FGridOutputEditorManager.StudyDataHasChanged(AContext,AFieldName,AOldValue,ANewValue);
    if Assigned(FTimeSeriesComparitorManager) then
      Result := Result and FTimeSeriesComparitorManager.StudyDataHasChanged(AContext,AFieldName,AOldValue,ANewValue);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TResultsTabSheetManager.StudyHasChanged: boolean;
const OPNAME = 'TResultsTabSheetManager.StudyHasChanged';
begin
  Result := inherited StudyHasChanged;
  try
    Result := Result and FTabSheet.StudyHasChanged;
    
    if Assigned(FViewOutputGraphManager) then
      Result := Result and FViewOutputGraphManager.StudyHasChanged;

    if Assigned(FGridOutputEditorManager) then
      Result := Result and FGridOutputEditorManager.StudyHasChanged;

    if Assigned(FTimeSeriesComparitorManager) then
      Result := Result and FTimeSeriesComparitorManager.StudyHasChanged;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TResultsTabSheetManager.DoGridAction(AGridActionObject: TObject): boolean;
const OPNAME = 'TResultsTabSheetManager.DoGridAction';
begin
  Result := False;
  try
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TResultsTabSheetManager.DoJumpRequest(AViewDataNode: TViewDataNode): boolean;
const OPNAME = 'TResultsTabSheetManager.DoJumpRequest';
begin
  Result := False;
  try
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TResultsTabSheetManager.DoCustomTabSheetEvent(ACustomModelEvent: TModelMenuData): boolean;
const OPNAME = 'TResultsTabSheetManager.DoCustomTabSheetEvent';
begin
  Result := False;
  try
    if (not Result) and Assigned(FViewOutputGraphManager) then
      Result := FViewOutputGraphManager.DoCustomTabSheetEvent(ACustomModelEvent);
    if (not Result) and Assigned(FGridOutputEditorManager) then
      Result := FGridOutputEditorManager.DoCustomTabSheetEvent(ACustomModelEvent);
    if (not Result) and Assigned(FTimeSeriesComparitorManager) then
      Result := FTimeSeriesComparitorManager.DoCustomTabSheetEvent(ACustomModelEvent);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TResultsTabSheetManager.ProcessMetaDataEvent: boolean;
const OPNAME = 'TResultsTabSheetManager.ProcessMetaDataEvent';
begin
  Result := FALSE;
  try
    if Assigned(FTimeSeriesComparitorManager) then
    begin
      if (FAppModules.MainForm.PageControl.ActivePage = FTabSheet) then
      begin
        if (TResultsSheet(FTabSheet).PageControl.ActivePage = FTimeSeriesComparitorManager.TabSheet) then
          Result := FTimeSeriesComparitorManager.ProcessMetaDataEvent;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TResultsTabSheetManager.ResetState: boolean;
const OPNAME = 'TResultsTabSheetManager.ResetState';
begin
  Result := False;
  try

    if Assigned(FViewOutputGraphManager) then
      FViewOutputGraphManager.ResetState;
    if Assigned(FGridOutputEditorManager) then
      FGridOutputEditorManager.ResetState;
    if Assigned(FTimeSeriesComparitorManager) then
      FTimeSeriesComparitorManager.ResetState;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


end.
