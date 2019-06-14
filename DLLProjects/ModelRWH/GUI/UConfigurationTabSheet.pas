//
//
//  UNIT      : Contains TRunModelTabSheet Class
//  AUTHOR    : Dziedzi Ramulondi (arivia.kom)
//  DATE      : 25/02/2014
//  COPYRIGHT : Copyright © 2014 DWAF
//
//
unit UConfigurationTabSheet;

interface
uses
  System.Classes,
  Vcl.Graphics,
  Vcl.ComCtrls,
  Vcl.Controls,
  UAbstractComponent,
  UFrameConfiguration,
  URWHDataObject,
  UDataViewerSheet;

type
  TConfigurationTabSheet = class(TDataViewerSheet)
  protected
    procedure CreateMemberObjects; override;
    function GetToolBar: TAbstractToolBar; override;
    procedure OnTreeViewItemSelected(Sender: TObject; Node: TTreeNode);
    procedure OnRunConfigHasBeenAdded(Sender: TObject);
    procedure OnRunConfigHasBeenDeleted(Sender: TObject);
   public
    procedure PopulateTreeView; override;
    function StudyHasChanged: boolean; override;
    function LanguageHasChanged: boolean; override;
    function Initialise: boolean; override;
  end;


implementation
uses
  SysUtils,
  UUtilities,
  UErrorHandlingOperations;


const
  FirstNodeCaption = 'Configurations List';

{ TRWHTabSheet }

procedure TConfigurationTabSheet.CreateMemberObjects;
const OPNAME = 'TConfigurationTabSheet.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
    FTabCaptionKey := 'RWHConfiguration';
    FViewTypeConstant := 'RWHConfiguration';
    frmConfiguration := TfrmConfiguration.Create(Self);
    frmConfiguration.Parent := Self;
    frmConfiguration.Align  := alClient;
    frmConfiguration.AppModules := FAppModules;

    FTreeView.OnChange                  := OnTreeViewItemSelected;
    frmConfiguration.OnRunConfigAdded   := OnRunConfigHasBeenAdded;
    frmConfiguration.OnRunConfigDeleted := OnRunConfigHasBeenDeleted;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TConfigurationTabSheet.GetToolBar: TAbstractToolBar;
const OPNAME = 'TConfigurationTabSheet.GetToolBar';
begin
  Result := nil;
  try

  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TConfigurationTabSheet.Initialise: boolean;
const OPNAME = 'TConfigurationTabSheet.Initialise';
begin
  Result := inherited LanguageHasChanged;
  try
    Self.Font.Style := Self.Font.Style + [fsBold];
    frmConfiguration.Initialise;
    FTreeView.AutoExpand := True;
    Result := True;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TConfigurationTabSheet.LanguageHasChanged: boolean;
const OPNAME = 'TConfigurationTabSheet.LanguageHasChanged';
begin
  Result := False;
  try
    frmConfiguration.LanguageHasChanged;
    Result := True;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TConfigurationTabSheet.OnRunConfigHasBeenAdded(Sender: TObject);
const OPNAME = 'TConfigurationTabSheet.Initialise';
var
  LIndex     : integer;
  LNode      : TTreeNode;
begin
  try
    if(Sender <> nil) and (Sender is TRWHRunConfig) then
    begin
      PopulateTreeView;
      for LIndex := 1 to FTreeView.Items.Count-1 do
      begin
        LNode := FTreeView.Items[LIndex];
        if(LNode.Data <> nil) and (LNode.Text = (TRWHRunConfig(Sender).RunName))then
        begin
          FTreeView.Selected := LNode;
          Break;
        end;
      end;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TConfigurationTabSheet.OnRunConfigHasBeenDeleted(Sender: TObject);
const OPNAME = 'TConfigurationTabSheet.OnRunConfigHasBeenDeleted';
var
  LIndex     : integer;
  LNode      : TTreeNode;
begin
  try
    if(Sender <> nil) and (Sender is TRWHRunConfig) then
    begin
      for LIndex := 1 to FTreeView.Items.Count-1 do
      begin
        LNode := FTreeView.Items[LIndex];
        if(LNode.Data <> nil) and (LNode.Text = (TRWHRunConfig(Sender).RunName))then
        begin
          FTreeView.Items.Delete(LNode);
          FTreeView.FullExpand;
          Break;
        end;
      end;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TConfigurationTabSheet.PopulateTreeView;
const OPNAME = 'TConfigurationTabSheet.PopulateTreeView';
var
  LIndex     : integer;
  LNode      : TTreeNode;
  LRunConfig : TRWHRunConfig;
begin
  try
    FTreeView.Items.Clear;
    LNode := FTreeView.Items.Add(Nil,FirstNodeCaption);
    if(RWHModelData.RunConfigList.Count > 0) then
    begin
      for LIndex := 0 to RWHModelData.RunConfigList.Count-1 do
      begin
        LRunConfig := RWHModelData.RunConfigList.RWHRunConfigByIndex[LIndex];
        FTreeView.Items.AddChildObject(LNode,LRunConfig.RunName,LRunConfig);
      end;
      FTreeView.FullExpand;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TConfigurationTabSheet.OnTreeViewItemSelected(Sender: TObject; Node: TTreeNode);
const OPNAME = 'TConfigurationTabSheet.OnTreeViewItemSelected';
begin
  try
    frmConfiguration.SelectRunConfig(TRWHRunConfig(Node.Data));
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TConfigurationTabSheet.StudyHasChanged: boolean;
const OPNAME = 'TConfigurationTabSheet.StudyHasChanged';
begin
  Result := inherited StudyHasChanged;
  try
    Result := True;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

end.
