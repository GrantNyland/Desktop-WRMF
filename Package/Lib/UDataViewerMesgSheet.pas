//
//
//  UNIT      : Contains TDataViewerMesgSheet Class
//  AUTHOR    : Grant Nyland (PDNA)
//  DATE      : 2002/04/12
//  COPYRIGHT : Copyright © 2002 DWAF
//
//
unit UDataViewerMesgSheet;

interface

uses
  vcl.ExtCtrls,
  vcl.ComCtrls,
  UViewDataItem,
  UAbstractObject,
  UDataViewerSheet;

type
  TDataViewerMesgSheet = class(TDataViewerSheet)
  protected
    FMsgPanel: TPanel;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    procedure ClearDataViewer; override;
    procedure PopulateDataViewer(ADataObject: TViewDataTreeNodeData); override;
    procedure ShowNoDataMessage;
    procedure ClearMessage;
  end;

implementation

uses
  SysUtils,
  vcl.Controls,
  UErrorHandlingOperations;

{ TDataViewerMesgSheet }

procedure TDataViewerMesgSheet.CreateMemberObjects;
const OPNAME = 'TDataViewerMesgSheet.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
    FMsgPanel := TPanel.Create(nil);
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TDataViewerMesgSheet.DestroyMemberObjects;
const OPNAME = 'TDataViewerMesgSheet.DestroyMemberObjects';
begin
  try
    FreeAndNil(FMsgPanel);
    inherited DestroyMemberObjects;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TDataViewerMesgSheet.ClearDataViewer;
const OPNAME = 'TDataViewerMesgSheet.ClearDataViewer';
begin
  inherited ClearDataViewer;
  try
    ClearMessage;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TDataViewerMesgSheet.PopulateDataViewer(ADataObject: TViewDataTreeNodeData);
const OPNAME = 'TDataViewerMesgSheet.PopulateDataViewer';
var
  LViewDataSet: TViewDataSet;
begin
  inherited PopulateDataViewer(ADataObject);
  try
    FMsgPanel.Caption := FAppModules.Language.GetString('ViewData.strNoDataReturned');
    LViewDataSet := ADataObject.ViewDataNode.ViewDataSet[0];
    if Assigned(LViewDataSet) then
     FMsgPanel.Caption := FAppModules.Language.GetString('ViewData.'+LViewDataSet.NoDataMessage);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TDataViewerMesgSheet.ClearMessage;
const OPNAME = 'TDataViewerMesgSheet.ClearMessage';
begin
  try
    FMsgPanel.Caption := '';
    FMsgPanel.Visible := False;
    FMsgPanel.Parent  := nil;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TDataViewerMesgSheet.ShowNoDataMessage;
const OPNAME = 'TDataViewerMesgSheet.ShowNoDataMessage';
begin
  try
    FMsgPanel.Parent  := Self;
    FMsgPanel.Align   := alClient;
    FMsgPanel.Visible := True
  except on E: Exception do HandleError(E, OPNAME); end;
end;

end.

