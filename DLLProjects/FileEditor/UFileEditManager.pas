//
//
//  UNIT      : Contains TFileEditManager Class
//  AUTHOR    : Grant Nyland (PDNA)
//  DATE      : 2002/02/18
//  COPYRIGHT : Copyright © 2002 DWAF
//
//
unit UFileEditManager;

interface

uses
  contnrs,
//  UDataModelObjects,
  UAbstractObject,
  UGenericModelLinkClasses,
  UDataViewerManager;

type
  TFileEditManager = class(TAbstractFileEditManager)
  protected
    procedure CreateMemberObjects; override;
    procedure CreateFileEditTabSheet;virtual;
  public
    function Initialise: boolean; override;
    function DoCustomTabSheetEvent(ACustomModelEvent: TModelMenuData): boolean; override;
    procedure SetOnFileSave(AOnFileSave:TOnFileSave); override;
  end;

implementation

uses
  SysUtils,
  UFileEditTabSheet,
  UErrorHandlingOperations;


procedure TFileEditManager.CreateMemberObjects;
const OPNAME = 'TFileEditManager.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
    CreateFileEditTabSheet;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TFileEditManager.CreateFileEditTabSheet;
const OPNAME = 'TFileEditManager.CreateFileEditTabSheet';
begin
  try
    FTabSheet := TFileEditTabSheet.Create(nil, AppModules);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TFileEditManager.Initialise: boolean;
const OPNAME = 'TFileEditManager.Initialise';
begin
  Result := inherited Initialise;
  try
    Result := Result and Assigned(FTabSheet) and FTabSheet.Initialise;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TFileEditManager.SetOnFileSave(AOnFileSave: TOnFileSave);
const OPNAME = 'TFileEditManager.SetOnFileSave';
begin
  try
    if Assigned(FTabSheet) then
     TFileEditTabSheet(FTabSheet).OnFileSave := AOnFileSave;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TFileEditManager.DoCustomTabSheetEvent(ACustomModelEvent: TModelMenuData): boolean;
const OPNAME = 'TFileEditManager.DoCustomTabSheetEvent';
begin
  Result := False;
  try
    if Assigned(FTabSheet) then
    begin
      if Assigned(ACustomModelEvent) then
      begin
        Result := True;
        case ACustomModelEvent.Action of
          meHideFileEditTab: TFileEditTabSheet(FTabSheet).HideTabsheet;
          meShowFileEditTab: TFileEditTabSheet(FTabSheet).ShowTabsheet;
        else
          Result := TFileEditTabSheet(FTabSheet).DoCustomTabSheetEvent(ACustomModelEvent);
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

end.
