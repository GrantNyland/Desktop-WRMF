//
//
//  UNIT      : Contains TTabSheetManager Class
//  AUTHOR    : Grant Nyland (PDNA)
//  DATE      : 2002/03/02
//  COPYRIGHT : Copyright © 2002 DWAF
//
//
unit UTabSheetManager;

interface

uses
  UViewDataItem,
  UGridActionObject,
  UAbstractComponent,
  UAbstractObject,
  UGenericModelLinkClasses;

type
  TTabSheetManager = class(TAbstractAppObject)
  protected
    FTabSheet: TAbstractTabSheet;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
  public
    function ResetState: boolean; override;
    function Initialise: Boolean; override;
    function LanguageHasChanged: boolean; override;
    function StudyHasChanged: boolean; override;
    function StudyDataHasChanged(AContext: TChangeContext; AFieldName,AOldValue,ANewValue: string): boolean; override;
    function DoJumpRequest(AViewDataNode: TViewDataNode): boolean; virtual;
    function DoGridAction(AGridActionObject: TObject): boolean; virtual;
    function DoCustomTabSheetEvent(ACustomModelEvent: TModelMenuData): boolean; virtual;
    function ProcessMetaDataEvent : boolean; virtual;
    function ProcessParameterChangeEvent : boolean; virtual;
    property TabSheet: TAbstractTabSheet read FTabSheet;
  end;

implementation

uses
  SysUtils,
  UDataViewerSheet,
  UErrorHandlingOperations;

procedure TTabSheetManager.CreateMemberObjects;
const OPNAME = 'TTabSheetManager.CreateMemberObjects';
begin
  try

    // Call the ancestor.
    inherited CreateMemberObjects;

  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TTabSheetManager.DestroyMemberObjects;
const OPNAME = 'TTabSheetManager.DestroyMemberObjects';
begin
  try
    FreeAndNil(FTabSheet);
    inherited DestroyMemberObjects;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TTabSheetManager.LanguageHasChanged;
const OPNAME = 'TTabSheetManager.LanguageHasChanged';
begin
  Result := False;
  try
    if Assigned(FTabSheet) then
      FTabSheet.LanguageHasChanged;
    inherited LanguageHasChanged;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TTabSheetManager.StudyDataHasChanged(AContext: TChangeContext; AFieldName,AOldValue,ANewValue: string): boolean;
const OPNAME = 'TTabSheetManager.StudyDataHasChanged';
begin
  Result := inherited StudyDataHasChanged(AContext,AFieldName,AOldValue,ANewValue);
  try
    if Assigned(FTabSheet) then
      Result := Result and FTabSheet.StudyDataHasChanged(AContext,AFieldName,AOldValue,ANewValue);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TTabSheetManager.StudyHasChanged: boolean;
const OPNAME = 'TTabSheetManager.StudyHasChanged';
begin
  Result := inherited StudyHasChanged;
  try
    if Assigned(FTabSheet) then
      Result := Result and FTabSheet.StudyHasChanged;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TTabSheetManager.DoGridAction(AGridActionObject: TObject): boolean;
const OPNAME = 'TTabSheetManager.DoGridAction';
begin
  Result := False;
  try
    if Assigned(FTabSheet) then
      Result := FTabSheet.DoGridAction(TGridActionObject(AGridActionObject));
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TTabSheetManager.DoJumpRequest(AViewDataNode: TViewDataNode): boolean;
const OPNAME = 'TTabSheetManager.DoJumpRequest';
begin
  Result := False;
  try
    if Assigned(FTabSheet) then
      Result := FTabSheet.DoJumpRequest(AViewDataNode);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TTabSheetManager.DoCustomTabSheetEvent(ACustomModelEvent: TModelMenuData): boolean;
const OPNAME = 'TTabSheetManager.DoCustomTabSheetEvent';
begin
  Result := False;
  try
    if Assigned(FTabSheet) then
      Result := FTabSheet.DoCustomTabSheetEvent(ACustomModelEvent);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TTabSheetManager.ProcessMetaDataEvent: boolean;
const OPNAME = 'TTabSheetManager.ProcessMetaDataEvent';
begin
  Result := False;
  try
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TTabSheetManager.ProcessParameterChangeEvent: boolean;
const OPNAME = 'TTabSheetManager.ProcessParameterChangeEvent';
begin
  Result := False;
  try
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TTabSheetManager.Initialise: Boolean;
const OPNAME = 'TTabSheetManager.Initialise';
begin
  Result := False;
  try
    if Assigned(FTabSheet) then
      Result := FTabSheet.Initialise;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TTabSheetManager.ResetState: boolean;
const OPNAME = 'TTabSheetManager.ResetState';
begin
  Result := inherited ResetState;
  try
    if Assigned(FTabSheet) then
      Result := Result and FTabSheet.ResetState;

  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME); end;
end;

end.
