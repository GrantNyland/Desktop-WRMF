//
//
//  UNIT      : Contains TStomsaGUIManager Class
//  AUTHOR    : Dziedzi Ramulondi (arivia.kom)
//  DATE      : 09/01/2007
//  COPYRIGHT : Copyright © 2007 DWAF
//
//
unit UStomsaGUIManager;

interface

uses

  // DWAF
  UStomsaTabSheetManager,
  UAbstractObject;

type
  TStomsaGUIManager = class(TAbstractAppObject)
  protected
    FStomsaTabSheetManager: TStomsaTabSheetManager;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
  public
    function Initialise: boolean; override;
    function LanguageHasChanged: boolean; override;
    function StudyHasChanged: boolean; override;
    function CanCopyToCLipboard: boolean; virtual;
    function CanExport: boolean; virtual;
    function CanPrint: boolean; virtual;
    procedure DoCopyToCLipboard; virtual;
    procedure DoExport; virtual;
    procedure DoPrint; virtual;
  end;

implementation

uses

  // Delphi
  SysUtils,

  //DWAF
  UErrorHandlingOperations;

{ TStomsaGUIManager }

procedure TStomsaGUIManager.CreateMemberObjects;
const OPNAME = 'TStomsaGUIManager.CreateMemberObjects';
begin
  try
    FStomsaTabSheetManager                      := TStomsaTabSheetManager.Create(FAppModules);
    FStomsaTabSheetManager.TabSheet.PageControl := FAppModules.MainForm.PageControl;
    inherited CreateMemberObjects;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TStomsaGUIManager.DestroyMemberObjects;
const OPNAME = 'TStomsaGUIManager.DestroyMemberObjects';
begin
  try
    if Assigned(FStomsaTabSheetManager) then
      FreeAndNil(FStomsaTabSheetManager);
    inherited DestroyMemberObjects;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TStomsaGUIManager.Initialise: boolean;
const OPNAME = 'TStomsaGUIManager.Initialise';
begin
  Result := inherited Initialise;;
  try
    if Assigned(FStomsaTabSheetManager) then
      Result := Result and FStomsaTabSheetManager.Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TStomsaGUIManager.LanguageHasChanged: boolean;
const OPNAME = 'TStomsaGUIManager.LanguageHasChanged';
begin
  Result := inherited LanguageHasChanged;
  try
    if Assigned (FStomsaTabSheetManager) then
      Result := Result and FStomsaTabSheetManager.LanguageHasChanged;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TStomsaGUIManager.StudyHasChanged: boolean;
const OPNAME = 'TStomsaGUIManager.StudyHasChanged';
begin
  Result := inherited StudyHasChanged;
  try
    if Assigned(FStomsaTabSheetManager) then
      Result := Result and FStomsaTabSheetManager.StudyHasChanged;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


function TStomsaGUIManager.CanCopyToCLipboard: boolean;
const OPNAME = 'TStomsaGUIManager.CanCopyToCLipboard';
begin
  Result := False;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TStomsaGUIManager.CanExport: boolean;
const OPNAME = 'TStomsaGUIManager.CanExport';
begin
  Result := False;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TStomsaGUIManager.CanPrint: boolean;
const OPNAME = 'TStomsaGUIManager.CanPrint';
begin
  Result := False;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TStomsaGUIManager.DoCopyToCLipboard;
const OPNAME = 'TStomsaGUIManager.DoCopyToCLipboard';
begin
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TStomsaGUIManager.DoExport;
const OPNAME = 'TStomsaGUIManager.DoExport';
begin
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TStomsaGUIManager.DoPrint;
const OPNAME = 'TStomsaGUIManager.DoPrint';
begin
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
