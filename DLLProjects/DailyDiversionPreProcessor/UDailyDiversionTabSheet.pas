//
//
//  UNIT      : Contains TDailyDiversionTabSheet Class
//  AUTHOR    : Sam Dhlamini(Cornastone)
//  DATE      : 22/01/2004
//  COPYRIGHT : Copyright © 2004 DWAF
//
//

unit UDailyDiversionTabSheet;

interface
uses
  VCL.Menus,
  Classes,
  VCL.stdctrls,
  VCL.ComCtrls,
  contnrs,
  UAbstractObject,
  UAbstractComponent,
  UMenuItemManager,
  UViewDataItem,
  UTreeViewTabSheet,
  UDataViewerMesgSheet,

  UDailyDiversionGaugeData,
  UDailyDiversionDataObject,

  UGenericModelLinkClasses;
type
  TDailyDiversionTabSheet = class(TDataViewerMesgSheet)
  protected
    procedure PopulateDataViewer(ADataObject: TViewDataTreeNodeData);override;
    procedure CreateMemberObjects; override;
    procedure DoTreeNodeAboutToChange(ASender: TObject; ANode: TTreeNode; var AAllowChange: Boolean);override;
    procedure DoTreeNodeHasChanged(ASender: TObject; ANode: TTreeNode); override;
    function GetToolBar: TAbstractToolBar; override;
  public
    procedure AfterConstruction; override;
    function StudyHasChanged: Boolean; override;
    function StudyDataHasChanged(AContext: TChangeContext; AFieldName: String;
                                 AOldValue: String; ANewValue: String): Boolean;override;
    function Initialise: Boolean; override;
    function SaveState: Boolean; override;

    procedure DoCopyToCLipboard; override;
    procedure DoExport(AFileName: string = ''); override;
    procedure DoPrint; override;
    function CanCopyToCLipboard: boolean; override;
    function CanExport: boolean; override;
    function CanPrint: boolean; override;
end;
implementation
uses
  Math,
  UConstants,
  windows,
  VCL.Forms,
  VCL.Graphics,
  SysUtils,
  VCL.Controls,
  //FileCtrl,
  UUtilities,
  UFileNameConstants,
  UMainMenuEventType,
  UErrorHandlingOperations, UDataViewerSheet;

{ TDailyDiversionTabSheet }

procedure TDailyDiversionTabSheet.AfterConstruction;
const OPNAME = 'TDailyDiversionTabSheet.AfterConstruction';
begin
  try
    inherited AfterConstruction;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDailyDiversionTabSheet.CanCopyToClipboard: boolean;
const OPNAME = 'TDailyDiversionTabSheet.CanCopyToClipboard';
begin
  Result := False;
  try
    Result := FAppModules.Model.CanCopyToClipboard;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDailyDiversionTabSheet.CanExport: boolean;
const OPNAME = 'TDailyDiversionTabSheet.CanExport';
begin
  Result := False;
  try
    Result := FAppModules.Model.CanExport;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDailyDiversionTabSheet.CanPrint: boolean;
const OPNAME = 'TDailyDiversionTabSheet.CanPrint';
begin
  Result := False;
  try
    Result := FAppModules.Model.CanPrint;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDailyDiversionTabSheet.CreateMemberObjects;
const OPNAME = 'TDailyDiversionTabSheet.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
    FTabCaptionKey := 'DailyDiversion';
    FViewTypeConstant := 'DailyDiversion';
    FTreeView.ReadOnly   := True;
    FTreeView.OnChanging := DoTreeNodeAboutToChange;
    FTreeView.OnChange   := DoTreeNodeHasChanged;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TDailyDiversionTabSheet.DoCopyToClipboard;
const OPNAME = 'TDailyDiversionTabSheet.DoCopyToClipboard';
begin
  try
    FAppModules.Model.DoCopyToClipboard;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDailyDiversionTabSheet.DoExport(AFileName: string);
const OPNAME = 'TDailyDiversionTabSheet.DoExport';
begin
  try
    FAppModules.Model.DoExport(AFileName);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDailyDiversionTabSheet.DoPrint;
const OPNAME = 'TDailyDiversionTabSheet.DoPrint';
begin
  try
    FAppModules.Model.DoPrint;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDailyDiversionTabSheet.DoTreeNodeAboutToChange(ASender: TObject;ANode: TTreeNode; var AAllowChange: Boolean);
const OPNAME = 'TDailyDiversionTabSheet.DoTreeNodeAboutToChange';
begin
  try
    if (csDestroying in Self.ComponentState) then Exit;
    inherited DoTreeNodeAboutToChange(ASender, ANode, AAllowChange);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDailyDiversionTabSheet.DoTreeNodeHasChanged(ASender: TObject;ANode: TTreeNode);
const OPNAME = 'TDailyDiversionTabSheet.DoTreeNodeHasChanged';
begin
  if (csDestroying in Self.ComponentState) then Exit;
  try
    inherited DoTreeNodeHasChanged(ASender,ANode);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TDailyDiversionTabSheet.Initialise: Boolean;
const OPNAME = 'TDailyDiversionTabSheet.Initialise';
begin
  Result := False;
  try
    Result := True;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TDailyDiversionTabSheet.PopulateDataViewer(ADataObject: TViewDataTreeNodeData);
const OPNAME = 'TDailyDiversionTabSheet.PopulateDataViewer';
var
  LContextDataList : TStringList;
begin
  inherited PopulateDataViewer(ADataObject);
  try
    LContextDataList := TStringList.Create;
    try
      if (UpperCase(ADataObject.ViewDataNode.ViewID) = 'DAILYDIVERSIONSTATIONDATA') then
      begin
        LContextDataList.Add('VIEWNAME=DAILYDIVERSIONDATA');
        LContextDataList.Add(Format('MODELELEMENTID=%d',[ADataObject.ViewDataNode.Weighting]));
        FAppModules.Model.ViewInputDialog(self, LContextDataList.CommaText, nil);
      end;
    finally
      FreeAndNil(LContextDataList);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDailyDiversionTabSheet.SaveState: Boolean;
const OPNAME = 'TDailyDiversionTabSheet.SaveState';
begin
  Result := False;
  try
    Result := True;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDailyDiversionTabSheet.StudyDataHasChanged(
  AContext: TChangeContext; AFieldName, AOldValue,
  ANewValue: String): Boolean;
const OPNAME = 'TDailyDiversionTabSheet.StudyDataHasChanged';
var
  LIndex : integer;
  LNode : TTreeNode;
  LIdentifier : integer;
  LDataObject : TViewDataTreeNodeData;
  LDiversionGauge : TDiversionGauge;
begin
  Result := inherited StudyDataHasChanged(AContext,AFieldName,AOldValue,ANewValue);
  try
    for LIndex := 0 to FTreeView.Items.Count -1 do
    begin
      LNode := FTreeView.Items[LIndex];
      if Assigned(LNode) then
      begin
        if (LNode.Level = 0) then Continue;
        LDataObject := TViewDataTreeNodeData(LNode.Data);
        if Assigned(LDataObject) and Assigned(LDataObject.ViewDataNode) then
        begin
          LIdentifier := LDataObject.ViewDataNode.Weighting;
          if (AFieldName = 'OldStationNo') then
          begin
            LDiversionGauge := TDailyDiversionDataObject(FAppModules.Model.ModelData).
                               DailyDiversionGaugeDataList.DiversionGaugeByStationID[LIdentifier];
            if LDiversionGauge <> nil then
            begin
              LDataObject.ViewDataNode.OverrideCaption := LDiversionGauge.StationNo;
              LNode.Text := LDataObject.ViewDataNode.OverrideCaption;
            end;
          end;
        end;
      end;
    end;
    Result := True;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDailyDiversionTabSheet.StudyHasChanged: Boolean;
const OPNAME = 'TDailyDiversionTabSheet.StudyHasChanged';
begin
  Result := False;
  try
    Result := inherited StudyHasChanged;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDailyDiversionTabSheet.GetToolBar: TAbstractToolBar;
const OPNAME = 'TDailyDiversionTabSheet.GetToolBar';
begin
  Result := nil;
  try
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;


end.
