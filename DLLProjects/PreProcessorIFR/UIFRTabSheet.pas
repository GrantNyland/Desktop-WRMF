//
//
//  UNIT      : Contains TIFRTabSheet Class
//  AUTHOR    : Dziedzi Ramulondi (arivia.kom)
//  DATE      : 09/01/2007
//  COPYRIGHT : Copyright © 2007 DWAF
//
//
unit UIFRTabSheet;

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
  UGenericModelLinkClasses;
type
  TIFRTabSheet = class(TDataViewerMesgSheet)
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
    procedure DobtnGenerateDailyIFRClick;


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
  UIFRDataObject,
  UErrorHandlingOperations, UDataViewerSheet;

{ TIFRTabSheet }

procedure TIFRTabSheet.AfterConstruction;
const OPNAME = 'TIFRTabSheet.AfterConstruction';
begin
  try
    inherited AfterConstruction;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TIFRTabSheet.CreateMemberObjects;
const OPNAME = 'TIFRTabSheet.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
    FTabCaptionKey := 'IFRTabSheetCaption';
    FViewTypeConstant := 'IFRSite';
    FTreeView.ReadOnly   := True;
    FTreeView.OnChanging := DoTreeNodeAboutToChange;
    FTreeView.OnChange   := DoTreeNodeHasChanged;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TIFRTabSheet.DobtnGenerateDailyIFRClick;
const OPNAME = 'TIFRTabSheet.DobtnGenerateDailyIFRClick';
begin
  try

  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TIFRTabSheet.DoTreeNodeAboutToChange(ASender: TObject;ANode: TTreeNode; var AAllowChange: Boolean);
const OPNAME = 'TIFRTabSheet.DoTreeNodeAboutToChange';
begin
  try
    if (csDestroying in Self.ComponentState) then Exit;
    inherited DoTreeNodeAboutToChange(ASender, ANode, AAllowChange);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIFRTabSheet.DoTreeNodeHasChanged(ASender: TObject;ANode: TTreeNode);
const OPNAME = 'TIFRTabSheet.DoTreeNodeHasChanged';
begin
  if (csDestroying in Self.ComponentState) then Exit;
  try
    inherited DoTreeNodeHasChanged(ASender,ANode);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIFRTabSheet.GetToolBar: TAbstractToolBar;
const OPNAME = 'TIFRTabSheet.GetToolBar';
begin
  Result := nil;
  try

  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TIFRTabSheet.Initialise: Boolean;
const OPNAME = 'TIFRTabSheet.Initialise';
begin
  Result := False;
  try
    Result := True;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TIFRTabSheet.PopulateDataViewer(ADataObject: TViewDataTreeNodeData);
const OPNAME = 'TIFRTabSheet.PopulateDataViewer';
var
  LContextDataList : TStringList;
begin
  inherited PopulateDataViewer(ADataObject);
  try
    LContextDataList := TStringList.Create;
    try
      if (UpperCase(ADataObject.ViewDataNode.ViewID) = 'IFRSITEDATA') then
      begin
        LContextDataList.Add('VIEWNAME=IFRSITEDATA');
        LContextDataList.Add(Format('MODELELEMENTID=%d',[ADataObject.ViewDataNode.Weighting]));
        FAppModules.Model.ViewInputDialog(self, LContextDataList.CommaText, nil);
      end;
    finally
      FreeAndNil(LContextDataList);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TIFRTabSheet.StudyDataHasChanged(AContext: TChangeContext; AFieldName, AOldValue,
  ANewValue: String): Boolean;
const OPNAME = 'TIFRTabSheet.StudyDataHasChanged';
var
  LIndex : integer;
  LNode : TTreeNode;
  LIdentifier : integer;
  LDataObject : TViewDataTreeNodeData;
  LIFRSiteData : TIFRSiteDataObject;
begin
  Result := inherited StudyDataHasChanged(AContext,AFieldName,AOldValue,ANewValue);
  try
    if AFieldName = 'IFRSiteName' then
    begin
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
            LIFRSiteData := TIFRModelData(FAppModules.Model.ModelData).
                            IFRSiteDataList.IFRSiteDataByIdentifier[LIdentifier];
            if LIFRSiteData <> nil then
            begin
              LDataObject.ViewDataNode.OverrideCaption := LIFRSiteData.SiteName;
              LNode.Text := LDataObject.ViewDataNode.OverrideCaption;
            end;
          end;
        end;
      end;
    end;
    Result := True;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TIFRTabSheet.StudyHasChanged: Boolean;
const OPNAME = 'TIFRTabSheet.StudyHasChanged';
begin
  Result := False;
  try
    Result := inherited StudyHasChanged;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

end.
