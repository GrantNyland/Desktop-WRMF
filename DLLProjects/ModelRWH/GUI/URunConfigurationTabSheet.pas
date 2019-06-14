//
//
//  UNIT      : Contains TRunModelTabSheet Class
//  AUTHOR    : Dziedzi Ramulondi (arivia.kom)
//  DATE      : 25/02/2014
//  COPYRIGHT : Copyright © 2014 DWAF
//
//
unit URunConfigurationTabSheet;

interface
uses
  System.Classes,
  Vcl.Graphics,
  Vcl.ComCtrls,
  Vcl.Controls,
  UAbstractComponent,
  UFrameRunConfiguration,
  URWHDataObject,
  UDataViewerSheet;

type
  TRunConfigurationTabSheet = class(TDataViewerSheet)
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
  System.UITypes,
  SysUtils,
  UUtilities,
  UErrorHandlingOperations;

const
  FirstNodeCaption = 'RunConfigurations List';

{ TRWHTabSheet }

procedure TRunConfigurationTabSheet.CreateMemberObjects;
const OPNAME = 'TRunConfigurationTabSheet.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
    FTabCaptionKey                         := 'RWHRunConfiguration';
    FViewTypeConstant                      := 'RWHRunConfiguration';
    frmRunConfiguration                    := TfrmRunConfiguration.Create(Self);
    frmRunConfiguration.Parent             := Self;
    frmRunConfiguration.Align              := alClient;
    frmRunConfiguration.AppModules         := FAppModules;

    FTreeView.OnChange                     := OnTreeViewItemSelected;
    FTreeView.Font.Style                   := FTreeView.Font.Style + [fsBold];

    frmRunConfiguration.OnRunConfigAdded   := OnRunConfigHasBeenAdded;
    frmRunConfiguration.OnRunConfigDeleted := OnRunConfigHasBeenDeleted;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TRunConfigurationTabSheet.GetToolBar: TAbstractToolBar;
const OPNAME = 'TRunConfigurationTabSheet.GetToolBar';
begin
  Result := nil;
  try

  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TRunConfigurationTabSheet.Initialise: boolean;
const OPNAME = 'TRunConfigurationTabSheet.Initialise';
begin
  Result := inherited Initialise;
  try
    //Self.Font.Style := Self.Font.Style + [fsBold];
    frmRunConfiguration.Initialise;
    FTreeView.AutoExpand := True;
    Result := True;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TRunConfigurationTabSheet.LanguageHasChanged: boolean;
const OPNAME = 'TRunConfigurationTabSheet.LanguageHasChanged';
begin
  FTreeView.Items.Clear;
  Result := inherited LanguageHasChanged;
  try
    PopulateTreeView;
    frmRunConfiguration.LanguageHasChanged;
    Result := True;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TRunConfigurationTabSheet.OnRunConfigHasBeenAdded(Sender: TObject);
const OPNAME = 'TRunConfigurationTabSheet.Initialise';
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

procedure TRunConfigurationTabSheet.OnRunConfigHasBeenDeleted(Sender: TObject);
const OPNAME = 'TRunConfigurationTabSheet.OnRunConfigHasBeenDeleted';
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

procedure TRunConfigurationTabSheet.PopulateTreeView;
const OPNAME = 'TRunConfigurationTabSheet.PopulateTreeView';
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

procedure TRunConfigurationTabSheet.OnTreeViewItemSelected(Sender: TObject; Node: TTreeNode);
const OPNAME = 'TRunConfigurationTabSheet.OnTreeViewItemSelected';
begin
  try
    frmRunConfiguration.SelectRunConfig(TRWHRunConfig(Node.Data));
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TRunConfigurationTabSheet.StudyHasChanged: boolean;
const OPNAME = 'TRunConfigurationTabSheet.StudyHasChanged';
begin
  Result := inherited StudyHasChanged;
  try
    Result := True;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

end.
