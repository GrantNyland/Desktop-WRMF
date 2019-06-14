//
//
//  UNIT      : Contains TDDTSTabSheet Class
//  AUTHOR    : Sam Dhlamini(Cornastone)
//  DATE      : 22/01/2004
//  COPYRIGHT : Copyright © 2004 DWAF
//
//

unit UDDTSTabSheet;

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

  //UDailyDiversionGaugeData,
  UDDTSDataObject,

  UGenericModelLinkClasses;
type
  TDDTSTabSheet = class(TDataViewerMesgSheet)
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

{ TDDTSTabSheet }

procedure TDDTSTabSheet.AfterConstruction;
const OPNAME = 'TDDTSTabSheet.AfterConstruction';
begin
  try
    inherited AfterConstruction;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDDTSTabSheet.CanCopyToClipboard: boolean;
const OPNAME = 'TDDTSTabSheet.CanCopyToClipboard';
begin
  Result := False;
  try
    Result := FAppModules.Model.CanCopyToClipboard;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDDTSTabSheet.CanExport: boolean;
const OPNAME = 'TDDTSTabSheet.CanExport';
begin
  Result := False;
  try
    Result := FAppModules.Model.CanExport;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDDTSTabSheet.CanPrint: boolean;
const OPNAME = 'TDDTSTabSheet.CanPrint';
begin
  Result := False;
  try
    Result := FAppModules.Model.CanPrint;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDDTSTabSheet.CreateMemberObjects;
const OPNAME = 'TDDTSTabSheet.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
    FTabCaptionKey := 'DDTS';
    FViewTypeConstant := 'DDTS';
    FTreeView.ReadOnly   := True;
    FTreeView.OnChanging := DoTreeNodeAboutToChange;
    FTreeView.OnChange   := DoTreeNodeHasChanged;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TDDTSTabSheet.DoCopyToClipboard;
const OPNAME = 'TDDTSTabSheet.DoCopyToClipboard';
begin
  try
    FAppModules.Model.DoCopyToClipboard;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDDTSTabSheet.DoExport(AFileName: string);
const OPNAME = 'TDDTSTabSheet.DoExport';
begin
  try
    FAppModules.Model.DoExport(AFileName);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDDTSTabSheet.DoPrint;
const OPNAME = 'TDDTSTabSheet.DoPrint';
begin
  try
    FAppModules.Model.DoPrint;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDDTSTabSheet.DoTreeNodeAboutToChange(ASender: TObject;ANode: TTreeNode; var AAllowChange: Boolean);
const OPNAME = 'TDDTSTabSheet.DoTreeNodeAboutToChange';
begin
  try
    if (csDestroying in Self.ComponentState) then Exit;
    inherited DoTreeNodeAboutToChange(ASender, ANode, AAllowChange);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDDTSTabSheet.DoTreeNodeHasChanged(ASender: TObject;ANode: TTreeNode);
const OPNAME = 'TDDTSTabSheet.DoTreeNodeHasChanged';
begin
  if (csDestroying in Self.ComponentState) then Exit;
  try
    inherited DoTreeNodeHasChanged(ASender,ANode);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TDDTSTabSheet.Initialise: Boolean;
const OPNAME = 'TDDTSTabSheet.Initialise';
begin
  Result := False;
  try
    Result := True;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TDDTSTabSheet.PopulateDataViewer(ADataObject: TViewDataTreeNodeData);
const OPNAME = 'TDDTSTabSheet.PopulateDataViewer';
var
  LContextDataList : TStringList;
begin
  inherited PopulateDataViewer(ADataObject);
  try
    LContextDataList := TStringList.Create;
    try
      if (UpperCase(ADataObject.ViewDataNode.ViewID) = 'DDTSDAMDATA') then
      begin
        LContextDataList.Add('VIEWNAME=DDTSDAMDATA');
        LContextDataList.Add(Format('MODELELEMENTID=%d',[ADataObject.ViewDataNode.Weighting]));
        FAppModules.Model.ViewInputDialog(self, LContextDataList.CommaText, nil);
      end;

      if (UpperCase(ADataObject.ViewDataNode.ViewID) = 'PARAMETERSHEADING') then
      begin
        LContextDataList.Add('VIEWNAME=PARAMETERSHEADING');
        LContextDataList.Add(Format('MODELELEMENTID=%d',[ADataObject.ViewDataNode.Weighting]));
        FAppModules.Model.ViewInputDialog(self, LContextDataList.CommaText, nil);
      end;
      if (UpperCase(ADataObject.ViewDataNode.ViewID) = 'RUNCONFIGURATION') then
      begin
        LContextDataList.Add('VIEWNAME=RUNCONFIGURATION');
        LContextDataList.Add(Format('MODELELEMENTID=%d',[ADataObject.ViewDataNode.Weighting]));
        FAppModules.Model.ViewInputDialog(self, LContextDataList.CommaText, nil);
      end;
    finally
      FreeAndNil(LContextDataList);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDDTSTabSheet.SaveState: Boolean;
const OPNAME = 'TDDTSTabSheet.SaveState';
begin
  Result := False;
  try
    Result := True;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDDTSTabSheet.StudyDataHasChanged(
  AContext: TChangeContext; AFieldName, AOldValue,
  ANewValue: String): Boolean;
const OPNAME = 'TDDTSTabSheet.StudyDataHasChanged';
var
  LIndex : integer;
  LNode : TTreeNode;
 // LIdentifier : integer;
  LDataObject : TViewDataTreeNodeData;
 // LDiversionGauge : TDiversionGauge;
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
        //  LIdentifier := LDataObject.ViewDataNode.Weighting;
          if (AFieldName = 'OldStationNo') then
          begin
            //LDiversionGauge := TDailyDiversionDataObject(FAppModules.Model.ModelData).
            //                   DailyDiversionGaugeDataList.DiversionGaugeByStationID[LIdentifier];
            //if LDiversionGauge <> nil then
          //  begin
          //    LDataObject.ViewDataNode.OverrideCaption := LDiversionGauge.StationNo;
           //   LNode.Text := LDataObject.ViewDataNode.OverrideCaption;
          //  end;
          end;
        end;
      end;
    end;
    Result := True;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDDTSTabSheet.StudyHasChanged: Boolean;
const OPNAME = 'TDDTSTabSheet.StudyHasChanged';
begin
  Result := False;
  try
    Result := inherited StudyHasChanged;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDDTSTabSheet.GetToolBar: TAbstractToolBar;
const OPNAME = 'TDDTSTabSheet.GetToolBar';
begin
  Result := nil;
  try
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;


end.
