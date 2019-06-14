{******************************************************************************}
{*  UNIT      : Contains TPatchRTabSheet Class                                *}
{*  AUTHOR    : RH Steyn(Cornastone)                                          *}
{*  DATE      : 12/10/2004                                                    *}
{*  COPYRIGHT : Copyright © 2004 DWAF                                         *}
{******************************************************************************}

unit UPatchRTabSheet;

interface
                                                            
uses
  VCL.Controls,
  UAbstractComponent,
  UPatchRValidator,
  UAbstractObject;

type
  TPatchRTabSheet = class(TAbstractTabSheet)
  protected
    FPatchRValidator : TPatchRValidator;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    function GetToolBar : TAbstractToolBar; override;
  public
    function Initialise : boolean; override;
    function LanguageHasChanged : boolean; override;
    function StudyHasChanged : boolean; override;
    function StudyDataHasChanged (AContext   : TChangeContext;
                                  AFieldName : string;
                                  AOldValue  : string;
                                  ANewValue  : string): Boolean; override;
    procedure TabHasChanged; override;
end;

implementation

uses
  SysUtils,
  UErrorHandlingOperations;

{ TPatchRTabSheet }

procedure TPatchRTabSheet.CreateMemberObjects;
const OPNAME = 'TPatchRTabSheet.CreateMemberObjects';
begin
  inherited CreateMemberObjects;
  try
    FTabCaptionKey := 'PatchREditing';

    FPatchRValidator := TPatchRValidator.Create(Self, FAppModules);
    FPatchRValidator.Panel.Parent := Self;
    FPatchRValidator.Panel.Align  := alClient;
  except on E: Exception do HandleError (E, OPNAME) end;
end;

procedure TPatchRTabSheet.DestroyMemberObjects;
const OPNAME = 'TPatchRTabSheet.DestroyMemberObjects';
begin
  inherited DestroyMemberObjects;
  try
  except on E: Exception do HandleError (E, OPNAME) end;
end;

function TPatchRTabSheet.GetToolBar: TAbstractToolBar;
const OPNAME = 'TPatchRTabSheet.GetToolBar';
begin
  Result := nil;
  try
  except on E: Exception do HandleError (E, OPNAME) end;
end;

function TPatchRTabSheet.Initialise: boolean;
const OPNAME = 'TPatchRTabSheet.Initialise';
begin
  Result := inherited initialise;
  try
    FPatchRValidator.Initialise;
    Result := TRUE;
  except on E: Exception do HandleError (E, OPNAME) end;
end;

function TPatchRTabSheet.LanguageHasChanged: boolean;
const OPNAME = 'TPatchRTabSheet.LanguageHasChanged';
begin
  Result := inherited LanguageHasChanged;
  try
  except on E: Exception do HandleError (E, OPNAME) end;
end;

function TPatchRTabSheet.StudyHasChanged: boolean;
const OPNAME = 'TPatchRTabSheet.StudyHasChanged';
begin
  Result := inherited StudyHasChanged;
  try
    if (Self.PageControl.ActivePage = Self) then
      Result := FPatchRValidator.StudyHasChanged;
  except on E: Exception do HandleError (E, OPNAME) end;
end;

function TPatchRTabSheet.StudyDataHasChanged (AContext   : TChangeContext;
                                              AFieldName : string;
                                              AOldValue  : string;
                                              ANewValue  : string): boolean;
const OPNAME = 'TPatchRTabSheet.StudyDataHasChanged';
begin
  Result := inherited StudyDataHasChanged(AContext, AFieldName, AOldValue, ANewValue);
  try
    if (Self.PageControl.ActivePage = Self) then
      Result := FPatchRValidator.StudyDataHasChanged(AContext, AFieldName, AOldValue, ANewValue);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TPatchRTabSheet.TabHasChanged;
const OPNAME = 'TPatchRTabSheet.TabHasChanged';
begin
  inherited;
  try
    FPatchRValidator.PopulateDataViewer;
  except on E: Exception do HandleError (E, OPNAME) end;
end;

end.
