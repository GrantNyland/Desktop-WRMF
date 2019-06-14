//
//
//  UNIT      : Contains TViewContoursTabSheet Class
//  AUTHOR    : Dziedzi Ramulondi (arivia.kom)
//  DATE      : 25/02/2014
//  COPYRIGHT : Copyright © 2014 DWAF
//
//
unit UViewContoursTabSheet;

interface
uses
  System.Classes,
  Vcl.Graphics,
  Vcl.ComCtrls,
  Vcl.Controls,
  UAbstractObject,
  UFrameViewContours,
  UAbstractComponent,
  UDataViewerSheet;
type

  TViewContoursTabSheet = class(TAbstractTabSheet)
  protected
    procedure CreateMemberObjects; override;
    function GetToolBar: TAbstractToolBar; override;

  public
    function Initialise: Boolean; override;

  end;


implementation
uses
  System.UITypes,
  SysUtils,
  UUtilities,
  UErrorHandlingOperations;

{ TRWHTabSheet }

procedure TViewContoursTabSheet.CreateMemberObjects;
const OPNAME = 'TViewContoursTabSheet.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
    FTabCaptionKey := 'RWHViewContours';
    //FViewTypeConstant := 'RWHViewContours';
    frmViewContours := TfrmViewContours.Create(Self);
    frmViewContours.Parent := Self;
    frmViewContours.Align  := alClient;
    frmViewContours.AppModules := FAppModules;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TViewContoursTabSheet.GetToolBar: TAbstractToolBar;
const OPNAME = 'TViewContoursTabSheet.GetToolBar';
begin
  Result := nil;
  try

  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TViewContoursTabSheet.Initialise: boolean;
const OPNAME = 'TViewContoursTabSheet.Initialise';
begin
  Result := inherited LanguageHasChanged;
  try
    Self.Font.Style := Self.Font.Style + [fsBold];
    frmViewContours.Initialise;
    Result := True;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

end.
