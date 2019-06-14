//
//
//  UNIT      : Contains TRunModelTabSheet Class
//  AUTHOR    : Dziedzi Ramulondi (arivia.kom)
//  DATE      : 25/02/2014
//  COPYRIGHT : Copyright © 2014 DWAF
//
//
unit URunModelTabSheet;

interface
uses
  System.UITypes,
  System.Classes,
  Vcl.Graphics,
  Vcl.ComCtrls,
  Vcl.Controls,
  UAbstractComponent,
  UFrameRunModel,
  URWHDataObject;

type
  TRunModelTabSheet = class(TAbstractTabSheet)
  protected
    procedure CreateMemberObjects; override;
    function GetToolBar: TAbstractToolBar; override;
   public
    //procedure PopulateTreeView;
    function StudyHasChanged: boolean; override;
    function LanguageHasChanged: boolean; override;
    function Initialise: boolean; override;
    procedure TabHasChanged; override;
  end;


implementation
uses
  SysUtils,
  UUtilities,
  UErrorHandlingOperations;


const
  FirstNodeCaption = 'RunModels List';

{ TRWHTabSheet }

procedure TRunModelTabSheet.CreateMemberObjects;
const OPNAME = 'TRunModelTabSheet.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
    FTabCaptionKey := 'RWHRunModel';
    frmRunModel := TfrmRunModel.Create(Self);
    frmRunModel.Parent := Self;
    frmRunModel.Align  := alClient;
    frmRunModel.AppModules := FAppModules;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TRunModelTabSheet.GetToolBar: TAbstractToolBar;
const OPNAME = 'TRunModelTabSheet.GetToolBar';
begin
  Result := nil;
  try

  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TRunModelTabSheet.Initialise: boolean;
const OPNAME = 'TRunModelTabSheet.Initialise';
begin
  Result := inherited Initialise;
  try
    Self.Font.Style := Self.Font.Style + [fsBold];
    frmRunModel.Initialise;
    Result := True;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TRunModelTabSheet.LanguageHasChanged: boolean;
const OPNAME = 'TRunModelTabSheet.LanguageHasChanged';
begin
  Result := inherited LanguageHasChanged;;
  try
    frmRunModel.LanguageHasChanged;
    Result := True;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TRunModelTabSheet.StudyHasChanged: boolean;
const OPNAME = 'TRunModelTabSheet.StudyHasChanged';
begin
  Result := inherited StudyHasChanged;
  try
    frmRunModel.StudyHasChanged;
    Result := True;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TRunModelTabSheet.TabHasChanged;
const OPNAME = 'TRunModelTabSheet.TabHasChanged';
begin
  try
    frmRunModel.TabHasChanged;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

end.
