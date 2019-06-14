//
//
//  UNIT      : Contains TStomsaTabSheetManager Class
//  AUTHOR    : Dziedzi Ramulondi (arivia.kom)
//  DATE      : 09/01/2007
//  COPYRIGHT : Copyright © 2007 DWAF
//
//
unit UStomsaTabSheetManager;

interface

uses
  UGenericModelLinkClasses,
  UDataViewerManager;

type
  TStomsaTabSheetManager = class(TDataViewerManager)
  protected
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
  public
    function ResetState: boolean; override;
    function Initialise: boolean; override;
    function LanguageHasChanged: boolean; override;
    function StudyHasChanged: boolean; override;
    function DoCustomTabSheetEvent(ACustomModelEvent: TModelMenuData): Boolean;override;
  end;

implementation

uses
  SysUtils,
  UStomsaTabSheet,
  UErrorHandlingOperations;

procedure TStomsaTabSheetManager.CreateMemberObjects;
const OPNAME = 'TStomsaTabSheetManager.CreateMemberObjects';
begin
  inherited CreateMemberObjects;
  try
    FTabSheet := TStomsaTabSheet.Create(nil, FAppModules);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TStomsaTabSheetManager.DestroyMemberObjects;
const OPNAME = 'TStomsaTabSheetManager.DestroyMemberObjects';
begin
  inherited DestroyMemberObjects;
  try
    FTabSheet.Free;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TStomsaTabSheetManager.DoCustomTabSheetEvent(ACustomModelEvent: TModelMenuData): Boolean;
const OPNAME = 'TStomsaTabSheetManager.DoCustomTabSheetEvent';
begin
  Result := False;
  try
    Result := FTabSheet.DoCustomTabSheetEvent(ACustomModelEvent);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TStomsaTabSheetManager.Initialise: Boolean;
const OPNAME = 'TStomsaTabSheetManager.Initialise';
begin
  Result := False;
  try
    Result := FTabSheet.Initialise;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TStomsaTabSheetManager.LanguageHasChanged: boolean;
const OPNAME = 'TStomsaTabSheetManager.LanguageHasChanged';
begin
  Result := False;
  try
    Result := FTabSheet.LanguageHasChanged;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TStomsaTabSheetManager.ResetState: boolean;
const OPNAME = 'TStomsaTabSheetManager.ResetState';
begin
  Result := False;
  try
    Result := FTabSheet.ResetState;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TStomsaTabSheetManager.StudyHasChanged: boolean;
const OPNAME = 'TStomsaTabSheetManager.StudyHasChanged';
begin
  Result := False;
  try
    Result := FTabSheet.StudyHasChanged;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

end.
