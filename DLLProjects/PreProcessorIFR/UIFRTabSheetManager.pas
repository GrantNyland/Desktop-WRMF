//
//
//  UNIT      : Contains TIFRTabSheetManager Class
//  AUTHOR    : Dziedzi Ramulondi (arivia.kom)
//  DATE      : 09/01/2007
//  COPYRIGHT : Copyright © 2007 DWAF
//
//
unit UIFRTabSheetManager;

interface

uses
  VCL.ComCtrls,
  VCL.Controls,
  VCL.ExtCtrls,
  UDataViewerManager,
  UTabSheetManager,
  UGenericModelLinkClasses,
  UAbstractModelObjects;

type
  TIFRTabSheetManager = class(TDataViewerManager)
  protected
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
  public
    function Initialise: Boolean; override;
    function DoCustomTabSheetEvent(ACustomModelEvent: TModelMenuData): Boolean;override;

  end;

implementation

uses
  SysUtils,
  UIFRTabSheet,
  UErrorHandlingOperations, UAbstractObject;

procedure TIFRTabSheetManager.CreateMemberObjects;
const OPNAME = 'TIFRTabSheetManager.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
    FTabSheet := TIFRTabSheet.Create(nil, AppModules);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TIFRTabSheetManager.DestroyMemberObjects;
const OPNAME = 'TIFRTabSheetManager.DestroyMemberObjects';
begin
  try
    inherited DestroyMemberObjects;

  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TIFRTabSheetManager.DoCustomTabSheetEvent(ACustomModelEvent: TModelMenuData): Boolean;
const OPNAME = 'TIFRTabSheetManager.DoCustomTabSheetEvent';
begin
  Result := False;
  try
    Result := True;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TIFRTabSheetManager.Initialise: Boolean;
const OPNAME = 'TIFRTabSheetManager.Initialise';
begin
  Result := False;
  try
    Result := True;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

end.
