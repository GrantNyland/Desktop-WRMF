
unit USedimentationTabSheet;

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
  TSedimentationTabSheet = class(TDataViewerMesgSheet)
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
  UErrorHandlingOperations, UDataViewerSheet;

{ TSedimentationTabSheet }

procedure TSedimentationTabSheet.AfterConstruction;
const OPNAME = 'TSedimentationTabSheet.AfterConstruction';
begin
  try
    inherited AfterConstruction;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TSedimentationTabSheet.CreateMemberObjects;
const OPNAME = 'TSedimentationTabSheet.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
    FTabCaptionKey := 'DamSediTabSheetCaption';
    FViewTypeConstant := 'DamSedimentation';
    FTreeView.ReadOnly   := True;
    FTreeView.OnChanging := DoTreeNodeAboutToChange;
    FTreeView.OnChange   := DoTreeNodeHasChanged;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TSedimentationTabSheet.DobtnGenerateDailyIFRClick;
const OPNAME = 'TSedimentationTabSheet.DobtnGenerateDailyIFRClick';
begin
  try

  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TSedimentationTabSheet.DoTreeNodeAboutToChange(ASender: TObject;ANode: TTreeNode; var AAllowChange: Boolean);
const OPNAME = 'TSedimentationTabSheet.DoTreeNodeAboutToChange';
begin
  try
    if (csDestroying in Self.ComponentState) then Exit;
    inherited DoTreeNodeAboutToChange(ASender, ANode, AAllowChange);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TSedimentationTabSheet.DoTreeNodeHasChanged(ASender: TObject;ANode: TTreeNode);
const OPNAME = 'TSedimentationTabSheet.DoTreeNodeHasChanged';
begin
  if (csDestroying in Self.ComponentState) then Exit;
  try
    inherited DoTreeNodeHasChanged(ASender,ANode);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TSedimentationTabSheet.GetToolBar: TAbstractToolBar;
const OPNAME = 'TSedimentationTabSheet.GetToolBar';
begin
  Result := nil;
  try

  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TSedimentationTabSheet.Initialise: Boolean;
const OPNAME = 'TSedimentationTabSheet.Initialise';
begin
  Result := False;
  try
    Result := True;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TSedimentationTabSheet.PopulateDataViewer(ADataObject: TViewDataTreeNodeData);
const OPNAME = 'TSedimentationTabSheet.PopulateDataViewer';
var
  LContextDataList : TStringList;
begin
  inherited PopulateDataViewer(ADataObject);
  try
    LContextDataList := TStringList.Create;
    try
      if (UpperCase(ADataObject.ViewDataNode.ViewID) = 'DAMSEDIMENTATIONDATA') then
      begin
        LContextDataList.Add('VIEWNAME=DAMSEDIMENTATIONDATA');
        LContextDataList.Add(Format('MODELELEMENTID=%d',[ADataObject.ViewDataNode.Weighting]));
        FAppModules.Model.ViewInputDialog(self, LContextDataList.CommaText, nil);
      end;
    finally
      FreeAndNil(LContextDataList);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TSedimentationTabSheet.StudyDataHasChanged(AContext: TChangeContext; AFieldName, AOldValue,
  ANewValue: String): Boolean;
const OPNAME = 'TSedimentationTabSheet.StudyDataHasChanged';
begin
  Result := inherited StudyDataHasChanged(AContext,AFieldName,AOldValue,ANewValue);
  try
    Result := True;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TSedimentationTabSheet.StudyHasChanged: Boolean;
const OPNAME = 'TSedimentationTabSheet.StudyHasChanged';
begin
  Result := False;
  try
    Result := inherited StudyHasChanged;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

end.
