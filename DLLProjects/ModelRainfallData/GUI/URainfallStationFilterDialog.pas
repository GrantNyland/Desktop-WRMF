unit URainfallStationFilterDialog;

interface

uses
  Classes,
  VCL.stdctrls,
  VCL.ComCtrls,
  VCL.Controls,
  VCL.extctrls,
  VCL.CheckLst,
  //Chart,
  //Series,
  VCL.Graphics,
  //TeeProcs,
  //TeEngine,
  Math,
  VCL.Dialogs,
  VCL.Forms,
  Windows,
  Contnrs,
  VCL.Buttons,
  VCL.Grids,
  Types,
  VCL.Menus,

  UConstants,
  UTabsheetManager,
  UAbstractObject,
  UAbstractComponent,
  RainfallCom_TLB,
  URainfallGraphMenuItemManager,
  UMenuItemManager,
  UDataComponent,
  UDataEditComponent,
  UGenericModelLinkClasses;

type

  TRainfallStationFilterDialog = class (TAbstractForm)
  protected
    FCurrentStationID        : integer;
    FCurrentSplitIndex       : integer;
    FCurrentPatchID          : integer;
    FChangeListID            : integer;
    FPanelBottom             : TPanel;
    FGraphTreeView           : TAbstractTreeView;
    FFilterButton            : TFieldButton;
    FCancelButton            : TFieldButton;
    FNode                    : TTreeNode;

    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    procedure AssignHelpContext; override;
    procedure PopulateTreeView;
    procedure PopulateDataViewer;
    procedure DoTreeViewChange (Sender : TObject; ANode : TTreeNode);
    procedure FormShow (Sender: TObject);
  public
    function Initialise: boolean; override;
    procedure Resize; override;
    function LanguageHasChanged: boolean; override;
    property GraphTreeView       : TAbstractTreeView   read FGraphTreeView;
    property PanelBottom         : TPanel              read FPanelBottom;
    property CurrentStationID    : integer             read FCurrentStationID;
    property CurrentPatchID      : integer             read FCurrentPatchID;
    property Node                : TTreeNode            read FNode;
  end;

implementation

uses
  SysUtils,
  VCL.Printers,
  UHelpContexts,
  UErrorHandlingOperations;

{ TRainfallGraphDialog }

procedure TRainfallStationFilterDialog.CreateMemberObjects;
const OPNAME = 'TRainfallStationFilterDialog.CreateMemberObjects';
begin
  inherited;
  try
    Self.OnShow                  := FormShow;
    self.BorderStyle             := bsDialog;
    FGraphTreeView               := TAbstractTreeView.Create(Self, FAppModules);
    FGraphTreeView.Parent        := Self;
    FGraphTreeView.Align         := alClient;
    FGraphTreeView.ReadOnly      := True;
    FGraphTreeView.HideSelection := False;
    FGraphTreeView.OnChange      := DoTreeViewChange;

    FPanelBottom                 := TPanel.Create(Self);
    FPanelBottom.Parent          := Self;
    FPanelBottom.Align           := alBottom;
    FPanelBottom.Top             := 0;
    FPanelBottom.Height          := 50;
    FPanelBottom.BorderStyle     := bsNone;
    FPanelBottom.BevelInner      := bvNone;
    FPanelBottom.BevelOuter      := bvNone;
    FPanelBottom.Width           := Self.Width;

    FFilterButton                := TFieldButton.Create(FPanelBottom,FAppModules,'Station Filter');
    FFilterButton.Parent         := FPanelBottom;
    FFilterButton.Top            := 15;
    FFilterButton.ModalResult    := mrYes;

    FCancelButton                := TFieldButton.Create(FPanelBottom,FAppModules,'Cancel');
    FCancelButton.Parent         := FPanelBottom;
    FCancelButton.Top            := 15;
    FCancelButton.ModalResult    := mrCancel;

  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TRainfallStationFilterDialog.DestroyMemberObjects;
const OPNAME = 'TRainfallStationFilterDialog.DestroyMemberObjects';
begin
  inherited DestroyMemberObjects;
  try
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TRainfallStationFilterDialog.Initialise: Boolean;
const OPNAME = 'TRainfallStationFilterDialog.Initialise';
begin
  Result := inherited initialise;
  try
    FCurrentPatchID      := 0;
    FCurrentStationID    := 0;
    FChangeListID        := 0;
    FCurrentSplitIndex   := -1;
    Result := True;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TRainfallStationFilterDialog.AssignHelpContext;
const OPNAME = 'TRainfallStationFilterDialog.AssignHelpContext';
begin
  inherited;
  try
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TRainfallStationFilterDialog.LanguageHasChanged: boolean;
const OPNAME = 'TRainfallStationFilterDialog.LanguageHasChanged';
begin
  Result := inherited LanguageHasChanged;
  try
    FFilterButton.Caption        := FAppModules.Language.GetString('ButtonCaption.Filter');
    FCancelButton.Caption        := FAppModules.Language.GetString('ButtonCaption.Cancel');
  {
    FStationIDLabel.Caption      := FAppModules.Language.GetString('LabelText.CurrentStationID');
    FStationIDName .Caption      := IntToStr(FCurrentStationID);

    FPatchIDLabel.Caption        := FAppModules.Language.GetString('LabelText.CurrentPatchID');
    FPatchIDName.Caption         := IntToStr(FCurrentPatchID);

    FHeadingLabel.Caption        := FAppModules.Language.GetString('LabelText.FilteredStations');
    FPanelClient.Caption         := FAppModules.Language.GetString('LabelText.PanelClient');   }

  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TRainfallStationFilterDialog.Resize;
const OPNAME = 'TRainfallStationFilterDialog.Resize';
begin
  inherited;
  try

    FCancelButton.Left           := (FPanelBottom.Width div 2) + 5;
    FFilterButton.Left           := (FPanelBottom.Width div 2) -( FFilterButton.ClientWidth + 5);
{    FStationIDLabel.Left         := 10;
    FStationIDName.Left          := FStationIDLabel.Left +  FStationIDLabel.Width + 5;
    FPatchIDLabel.Left           := 10;
    FPatchIDName.Left            := FPatchIDLabel.Left +  FPatchIDLabel.Width + 5;
    FHeadingLabel.Width          := FPanelClient.Width; }
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;


procedure TRainfallStationFilterDialog.PopulateTreeView;
const OPNAME = 'TRainfallStationFilterDialog.PopulateTreeView';
var
  LIndex         : Integer;
  LPatchIndex    : Integer;
  LCurrentNode   : TTreeNode;
  LStationID     : integer;
  lSelectedNode  : TTreeNode;
  lSplitNode     : TTreeNode;
  lPatchNode     : TTreeNode;
  lPatchID       : integer;
  lRainfallObj   : IRainfallModelData;
  lStation       : IStationData;
  lPatch         : IPatchData;
  lStationNumber : string;
  lSplitIndex    : integer;
  lSplit         : IRainfallDataSplit;
  lSplitName     : string;
  lSrcPatchID    : integer;
  lTarget        : WideString;
  lStartYear     : integer;
  lEndYear       : integer;
  lFoundSplit    : boolean;
begin
  try
    try
      GraphTreeView.Items.Clear;
      lRainfallObj := (FAppModules.Model.ModelData as IRainfallModelData);
      if (lRainfallObj.StationCount = 0) then
      begin
  //      HeadingLabel.Caption := '';
      end
      else
      begin
        lSelectedNode := nil;
        for LIndex := 0 to lRainfallObj.StationCount - 1 do
        begin
          lStation := lRainfallObj.GetStationDataByIndex(lIndex);
          if (lStation <> nil) then
          begin
            LStationID := lStation.RainfallData.StationID;
            lStationNumber := lStation.RainfallData.StationNumber;
            if (lStation.IsInWR90) then
              lStationNumber := lStationNumber + ' *';
            LCurrentNode := GraphTreeView.Items.AddObject(nil, lStationNumber, TObject(LStationID));
            if ((FCurrentStationID <> 0) AND (FCurrentStationID = LStationID)) then
              lSelectedNode := LCurrentNode;
            for lSplitIndex := 0 to lStation.SplitCount - 1 do
            begin
              lSplit     := lStation.GetSplitWithIndex(lSplitIndex);
              lSplitName := IntToStr(lSplit.HydroStartYear) + ' - ' + IntToStr(lSplit.HydroEndYear);
              lSplitNode := GraphTreeView.Items.AddChildObject(LCurrentNode, lSplitName, TObject(LStationID));
              if ((FCurrentStationID <> 0) AND (FCurrentStationID = LStationID) AND
                  (FCurrentSplitIndex <> -1) AND (FCurrentSplitIndex = lSplitIndex)) then
                lSelectedNode := lSplitNode;
            end;
            for LPatchIndex := 0 to lStation.PatchCount - 1 do
            begin
              lPatch      := lStation.GetPatchWithIndex(LPatchIndex);
              lPatchID    := lPatch.PatchID;
              lSplitNode  := nil;
              lFoundSplit := FALSE;
              if (lPatch.PatchTypeID = 1) then
              begin
                lSplitNode  := LCurrentNode.Item[0];
                lFoundSplit := TRUE;
              end
              else
              begin
                lPatch.GetSourceInfoByStationID(LStationID, lSrcPatchID, lTarget, lStartYear, lEndYear);
                if ((lStartYear = 0) AND (lEndYear = 0)) then
                begin
                  lSplitNode  := LCurrentNode.Item[0];
                  lFoundSplit := TRUE;
                end
                else
                begin
                  lSplitIndex := 0;
                  while ((lSplitIndex < lStation.SplitCount) AND (NOT lFoundSplit)) do
                  begin
                    lSplitNode := LCurrentNode.Item[lSplitIndex];
                    if (lSplitNode.Text = IntToStr(lStartYear) + ' - ' + IntToStr(lEndYear)) then
                      lFoundSplit := TRUE
                    else
                      lSplitIndex := lSplitIndex + 1;
                  end;
                end;
              end;
              if (lFoundSplit) then
              begin
                lPatchNode := GraphTreeView.Items.AddChildObject(lSplitNode, lPatch.PatchName, TObject(lPatchID));
                if ((FCurrentStationID <> 0) AND (FCurrentStationID = LStationID)  AND
                    (FCurrentSplitIndex <> -1) AND (FCurrentSplitIndex = lSplitNode.Index) AND
                    (FCurrentPatchID <> 0) AND (FCurrentPatchID = lPatchID)) then
                  lSelectedNode := lPatchNode;
              end;
            end;
          end;
        end;
        GraphTreeView.FullExpand;
        if (lSelectedNode = nil) AND (GraphTreeView.Items.Count > 0) then
          lSelectedNode := GraphTreeView.Items[0];
        if (lSelectedNode <> nil) then
        begin
          GraphTreeView.Selected := lSelectedNode;
        end;
      end;
    finally
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRainfallStationFilterDialog.PopulateDataViewer;
const OPNAME = 'TRainfallStationFilterDialog.PopulateDataViewer';
begin
  try
    FCurrentStationID  := (FAppModules.Model.ModelData as IRainfallModelData).CurrentStationID;
    FCurrentSplitIndex := (FAppModules.Model.ModelData as IRainfallModelData).CurrentSplitIndex;
    FCurrentPatchID    := (FAppModules.Model.ModelData as IRainfallModelData).CurrentPatchID;
    if (FAppModules.Changes.ChangeListWithID(FChangeListID) = nil) then
      FChangeListID    := 0;
    PopulateTreeView;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRainfallStationFilterDialog.FormShow(Sender: TObject);
const OPNAME = 'TRainfallStationFilterDialog.FormShow';
begin
  try
    PopulateDataViewer;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRainfallStationFilterDialog.DoTreeViewChange(Sender: TObject; ANode: TTreeNode);
const OPNAME = 'TRainfallStationFilterDialog.DoTreeViewChange';
var
  lRainfallModelData : IRainfallModelData;
begin
  try
    if (Assigned(FAppModules.Model())) AND (Assigned(FAppModules.Model.ModelData())) then
    begin
      lRainfallModelData := (FAppModules.Model.ModelData as IRainfallModelData);
      FNode := ANode;
      if (GraphTreeView.Selected <> nil) then
      begin
        if (GraphTreeView.Selected.Level = 0) then
        begin
          FCurrentStationID  := Integer(aNode.Data);
          FCurrentSplitIndex := -1;
          FCurrentPatchID    := 0;
        end
        else
        if (GraphTreeView.Selected.Level = 1) then
        begin
          FCurrentStationID  := Integer(ANode.Parent.Data);
          FCurrentSplitIndex := ANode.Index;
          FCurrentPatchID    := 0;
        end
        else
        if (GraphTreeView.Selected.Level > 1) then
        begin
          FCurrentStationID  := Integer(ANode.Parent.Data);
          FCurrentSplitIndex := ANode.Parent.Index;
          FCurrentPatchID    := Integer(ANode.Data);
        end;
      end;

      lRainfallModelData.CurrentPatchID    := FCurrentPatchID;
      lRainfallModelData.CurrentSplitIndex := FCurrentSplitIndex;
      lRainfallModelData.CurrentStationID  := FCurrentStationID;
      LanguageHasChanged;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

end.



