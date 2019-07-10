//
//
//  UNIT      : Contains TStomsaTabSheet Class
//  AUTHOR    : Dziedzi Ramulondi (arivia.kom)
//  DATE      : 09/01/2007
//  COPYRIGHT : Copyright © 2007 DWAF
//
//
unit UStomsaTabSheet;

interface

uses
  UGenericModelLinkClasses,
  UAbstractComponent;

type
  TStomsaTabSheet = class(TAbstractTabSheet)
  protected
    procedure CreateMemberObjects; override;
    function GetToolBar: TAbstractToolBar; override;
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
  VCL.Controls,
  UDataModule,
  UMessagesForm,
  UStomsaMainForm,
  UErrorHandlingOperations;

{ TStomsaTabSheet }

procedure TStomsaTabSheet.CreateMemberObjects;
const OPNAME = 'TStomsaTabSheet.CreateMemberObjects';
begin
  inherited CreateMemberObjects;
  try
    FTabCaptionKey           := 'StomsaTabSheet';
    fmStomsaMainForm         := TfmStomsaMainForm.Create(Self, FAppModules);
    fmStomsaMainForm.Parent  := Self;
    fmStomsaMainForm.Align   := alClient;
    fmMessages               := TfmMessages.Create(Self);
    fmData                   := TfmData.Create(Self, FAppModules);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TStomsaTabSheet.DoCustomTabSheetEvent(ACustomModelEvent: TModelMenuData): Boolean;
const OPNAME = 'TStomsaTabSheet.DoCustomTabSheetEvent';
begin
  Result := inherited DoCustomTabSheetEvent(ACustomModelEvent);
  try
    Result := fmStomsaMainForm.DoCustomTabSheetEvent(ACustomModelEvent);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TStomsaTabSheet.GetToolBar: TAbstractToolBar;
const OPNAME = 'TStomsaTabSheet.GetToolBar';
begin
  Result := nil;
  try
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TStomsaTabSheet.Initialise: boolean;
const OPNAME = 'TStomsaTabSheet.Initialise';
begin
  Result := inherited Initialise;
  try
    Result := fmStomsaMainForm.Initialise;
    Result := Result and fmMessages.Initialise;
    Result := Result and fmData.Initialise;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TStomsaTabSheet.LanguageHasChanged: boolean;
const OPNAME = 'TStomsaTabSheet.LanguageHasChanged';
begin
  Result := inherited LanguageHasChanged;
  try
    Result := fmStomsaMainForm.LanguageHasChanged;
    Result := Result and fmMessages.LanguageHasChanged;
    Result := Result and fmData.LanguageHasChanged;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TStomsaTabSheet.ResetState: boolean;
const OPNAME = 'TStomsaTabSheet.ResetState';
begin
  Result := inherited ResetState;
  try
    Result := fmStomsaMainForm.ResetState;
    Result := Result and fmMessages.ResetState;
    Result := Result and fmData.ResetState;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TStomsaTabSheet.StudyHasChanged: boolean;
const OPNAME = 'TStomsaTabSheet.StudyHasChanged';
begin
  Result := inherited StudyHasChanged;
  try
    Result := fmStomsaMainForm.StudyHasChanged;
    Result := Result and fmMessages.StudyHasChanged;
    Result := Result and fmData.StudyHasChanged;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

end.
