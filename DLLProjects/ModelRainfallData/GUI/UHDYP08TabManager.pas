
//
//
//  UNIT      : Contains THDYP08TabManager Class
//  AUTHOR    : Sam Dhlamini(Cornastone)
//  DATE      : 27/05/2004
//  COPYRIGHT : Copyright © 2004 DWAF
//
//

unit UHDYP08TabManager;

interface
uses
  UTabsheetManager,
  UAbstractObject,
  UHDYP08TabSheet;

type
  THDYP08TabManager = class ( TTabSheetManager )
    protected
      procedure CreateMemberObjects; override;
      procedure DestroyMemberObjects; override;
    public
      function Initialise: Boolean;override;
      function StudyDataHasChanged ( AContext: TChangeContext;
        AFieldName: String; AOldValue: String; ANewValue: String ): Boolean; override;
      function StudyHasChanged: Boolean;override;
      function SaveState: Boolean;override;
      function LanguageHasChanged: Boolean;override;
end;

implementation
uses
  SysUtils,
    UErrorHandlingOperations;
{ THDYP08TabManager }

procedure THDYP08TabManager.CreateMemberObjects;
const OPNAME = 'THDYP08TabManager.CreateMemberObjects';
begin
  inherited CreateMemberObjects;
  try
   FTabSheet := THDYP08TabSheet.Create( nil ,fAppModules );
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure THDYP08TabManager.DestroyMemberObjects;
const OPNAME = 'THDYP08TabManager.DestroyMemberObjects';
begin
  inherited DestroyMemberObjects;
  try
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function THDYP08TabManager.Initialise: Boolean;
const OPNAME = 'THDYP08TabManager.Initialise';
begin
  Result := True;

  try
    if Assigned ( FTabSheet ) then
      FTabSheet.Initialise;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function THDYP08TabManager.LanguageHasChanged: Boolean;
const OPNAME = 'THDYP08TabManager.LanguageHasChanged';
begin
  Result := inherited LanguageHasChanged;
  try
     if Assigned ( FTabSheet ) then
      FTabSheet.LanguageHasChanged;

  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function THDYP08TabManager.SaveState: Boolean;
const OPNAME = 'THDYP08TabManager.SaveState';
begin
  Result := inherited SaveState;
  try
    Result := True;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function THDYP08TabManager.StudyDataHasChanged(AContext: TChangeContext;
  AFieldName, AOldValue, ANewValue: String): Boolean;
const OPNAME = 'THDYP08TabManager.StudyDataHasChanged';
begin
  Result := inherited StudyDataHasChanged ( AContext, AFieldName, AOldValue, ANewValue );
  try
    if Assigned ( FTabSheet ) then
      FTabSheet.StudyDataHasChanged ( AContext, AFieldName, AOldValue, ANewValue );
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function THDYP08TabManager.StudyHasChanged: Boolean;
const OPNAME = 'THDYP08TabManager.StudyHasChanged';
begin
  Result := inherited StudyHasChanged;
  try
    if Assigned ( FTabSheet ) then
      FTabSheet.StudyHasChanged;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

end.
