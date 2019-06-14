//
//
//  UNIT      : Contains TViewDataManager Class
//  AUTHOR    : Grant Nyland (PDNA)
//  DATE      : 2002/05/07
//  COPYRIGHT : Copyright © 2002 DWAF
//
//
unit UViewDataManager;

interface

uses
  Classes,
  UViewDataItem,
  UViewDataList,
  UDataJumpMenuItemManager,
  UAbstractObject;

type
  TViewDataManager = class(TAbstractAppObject)
  protected
    FViewDataList: TViewDataList;
    FJumpList: TStringList;
    FDataJumpMenuItemManager: TDataJumpMenuItemManager;
    FGridLoaded, FGraphLoaded: boolean;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    function PopulateViewDataList: boolean;
  public
    function StudyHasChanged: boolean; override;
    function StudyDataHasChanged(AContext: TChangeContext; AFieldName,AOldValue,ANewValue: string): boolean; override;
    procedure DoJumpRequest(AViewDataJump: TObject);
    procedure RefreshMenuItems;
    property ViewDataList: TViewDataList read FViewDataList;
    property DataJumpMenuItemManager: TDataJumpMenuItemManager read FDataJumpMenuItemManager;
    property GridLoaded: boolean  read FGridLoaded  write FGridLoaded;
    property GraphLoaded: boolean read FGraphLoaded write FGraphLoaded;
  end;

implementation

uses
  SysUtils,
  UDataSetType,
  UDataViewerSheet,
  USQLDatabaseLayer,
  UGridActionObject,
  UAbstractComponent,
  UMainMenuEventType,
  UViewDataDatasetConstructor,
  UErrorHandlingOperations;

procedure TViewDataManager.CreateMemberObjects;
const OPNAME = 'TViewDataManager.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
    FViewDataList := TViewDataList.Create(FAppModules);
    FJumpList := TStringList.Create;
    FDataJumpMenuItemManager := TDataJumpMenuItemManager.Create(FAppModules);
    FOwnedAppObjects.Add(FDataJumpMenuItemManager);
    TSQLDatabaseLayer(FAppModules.Database).AddDataSetConstructor(TViewDataDatasetConstructor.Create(FAppModules));
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TViewDataManager.DestroyMemberObjects;
const OPNAME = 'TViewDataManager.DestroyMemberObjects';
begin
  try
    TSQLDatabaseLayer(FAppModules.Database).DeleteDataSetConstructorsOfType(TViewDataDatasetConstructor);
    FreeAndNil(FJumpList);
    FreeAndNil(FViewDataList);
    inherited DestroyMemberObjects;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TViewDataManager.StudyHasChanged: boolean;
const OPNAME = 'TViewDataManager.StudyHasChanged';
begin
  Result := False;
  try
    FAppModules.Model.ViewData.CurrentTreeNodeData := nil;
    if Assigned(FAppModules.Database()) and (FAppModules.Database.Connected) then
    begin
      if PopulateViewDataList then
      begin
        FViewDataList.PopulateJumpList(FJumpList);
        Result := True;
      end;
    end else begin
      Result := True;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TViewDataManager.StudyDataHasChanged(
  AContext: TChangeContext; AFieldName,AOldValue,ANewValue: string): boolean;
const OPNAME = 'TViewDataManager.StudyDataHasChanged';
begin
  Result := False;
  try
    Result := ViewDataList.StudyDataHasChanged(AContext,AFieldName,AOldValue,ANewValue);
    {if Assigned(FAppModules.FieldProperties.LastFieldToChange) and
       Assigned(FAppModules.FieldProperties.LastFieldToChange.FieldProperty) then
    begin
      if (FAppModules.FieldProperties.LastFieldToChange.FieldProperty.FieldName = 'ReservoirName') or
         (FAppModules.FieldProperties.LastFieldToChange.FieldProperty.FieldName = 'ChannelName') or
         (FAppModules.FieldProperties.LastFieldToChange.FieldProperty.FieldName = 'IncludeSummary') then
      begin
        Result := StudyHasChanged;
      end;
    end;}
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TViewDataManager.DoJumpRequest(AViewDataJump: TObject);
const OPNAME = 'TViewDataManager.DoJumpRequest';
var
  LViewChangeResult: TViewChangeResult;
  LViewDataNode: TViewDataNode;
begin
  try

    // Create a copy because the original is destroyed half way through this function.
    LViewDataNode := TViewDataNode.Create;
    try
      LViewDataNode.AssignFrom(TViewDataNode(AViewDataJump));
      LViewChangeResult := TViewChangeResult.Create;
      try
        LViewChangeResult.Result := True;

        // Jump to the grid.
        if (LViewDataNode.TopParentID = 'Grid') then
        begin
          FAppModules.Model.ProcessEvent(CmeViewEditGrid, LViewChangeResult);
          if LViewChangeResult.Result then
          begin
            TDataViewerSheet(TAbstractMainFormManager(FAppModules.MainForm).ActivePage).DoJumpRequest(LViewDataNode);
          end;
        end else begin

          // Jump to the graph.
          if (LViewDataNode.TopParentID = 'Graph') then
          begin
            FAppModules.Model.ProcessEvent(CmeViewGraph, LViewChangeResult);
            if LViewChangeResult.Result then
            begin
              TDataViewerSheet(TAbstractMainFormManager(FAppModules.MainForm).ActivePage).DoJumpRequest(LViewDataNode);
            end;
          end else begin

            // Unknown tab sheet.
            raise Exception.Create('Unknown Tab Sheet: [' + LViewDataNode.TopParentID + '].');
          end;
        end;

      // Done.
      finally
        LViewChangeResult.Free;
      end;
    finally
      LViewDataNode.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TViewDataManager.RefreshMenuItems;
const OPNAME = 'TViewDataManager.RefreshMenuItems';
begin
  try
    if Assigned(FAppModules.Model()) and
       Assigned(FAppModules.Model.ViewData()) and
       Assigned(FAppModules.Model.ViewData.CurrentTreeNodeData) and
       (FAppModules.Model.ViewData.CurrentTreeNodeData is TViewDataTreeNodeData) and
       Assigned(TViewDataTreeNodeData(FAppModules.Model.ViewData.CurrentTreeNodeData).ViewDataNode) then
    begin
      if TViewDataTreeNodeData(FAppModules.Model.ViewData.CurrentTreeNodeData).ViewDataNode.ViewDataSetCount > 0 then
      begin
        FDataJumpMenuItemManager.RefreshContext(FJumpList, FGridLoaded, FGraphLoaded,
          TViewDataTreeNodeData(FAppModules.Model.ViewData.CurrentTreeNodeData).ViewDataNode.ViewID,
          TViewDataTreeNodeData(FAppModules.Model.ViewData.CurrentTreeNodeData).ViewDataNode.ViewDataSet[0].IDValues);
      end else begin
        FDataJumpMenuItemManager.RefreshContext(FJumpList, FGridLoaded, FGraphLoaded,
          TViewDataTreeNodeData(FAppModules.Model.ViewData.CurrentTreeNodeData).ViewDataNode.ViewID,
          TViewDataTreeNodeData(FAppModules.Model.ViewData.CurrentTreeNodeData).ViewDataNode.IDValues);
      end;
    end else begin
      FDataJumpMenuItemManager.RefreshContext(FJumpList, FGridLoaded, FGraphLoaded);
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TViewDataManager.PopulateViewDataList: boolean;
const OPNAME = 'TViewDataManager.PopulateViewDataList';
var
  LViewDataSetsDataSet : TAbstractModelDataset;
  LViewDataSubNodesDataSet: TAbstractModelDataset;
  LViewDataNodesDataSet: TAbstractModelDataset;
  LViewDataJumpsDataSet: TAbstractModelDataset;
begin
  Result := False;
  try
    LViewDataSetsDataSet     := nil;
    LViewDataSubNodesDataSet := nil;
    LViewDataNodesDataSet    := nil;
    LViewDataJumpsDataSet    := nil;
    FAppModules.Database.CreateDataset(integer(dtViewDataSets), LViewDataSetsDataSet);
    try
      if Assigned(LViewDataSetsDataSet) and
         Assigned(LViewDataSetsDataSet.DataSet()) then
      begin
        FAppModules.Database.CreateDataset(integer(dtExecSQL), LViewDataSubNodesDataSet);
        try
          if Assigned(LViewDataSubNodesDataSet) and
             Assigned(LViewDataSubNodesDataSet.DataSet()) then
          begin
            FAppModules.Database.CreateDataset(integer(dtViewDataNodes), LViewDataNodesDataSet);
            try
              if Assigned(LViewDataNodesDataSet) and
                 Assigned(LViewDataNodesDataSet.DataSet()) then
              begin
                FAppModules.Database.CreateDataset(integer(dtViewDataJumps), LViewDataJumpsDataSet);
                try
                  if Assigned(LViewDataJumpsDataSet) and
                     Assigned(LViewDataJumpsDataSet.DataSet()) then
                  begin
                    FAppModules.StudyArea.SetDefaultParams(LViewDataNodesDataSet);
                    FViewDataList.Populate(LViewDataSetsDataSet, LViewDataSubNodesDataSet, LViewDataNodesDataSet, LViewDataJumpsDataSet);
                    Result := True;
                  end;
                finally
                  LViewDataJumpsDataSet.Free;
                end;
              end;
            finally
              LViewDataNodesDataSet.Free;
            end;
          end;
        finally
          LViewDataSubNodesDataSet.Free;
        end;
      end;
    finally
      LViewDataSetsDataSet.Free;
    end;

  // Handle Exceptions.
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
