{******************************************************************************}
{*  UNIT      : Contains TRainfallZoneValidator Class                         *}
{*  AUTHOR    : RH Steyn(Cornastone)                                          *}
{*  DATE      : 04/05/2005                                                    *}
{*  COPYRIGHT : Copyright © 2005 DWAF                                         *}
{******************************************************************************}

unit URainfallZoneValidator;

interface

uses
  Classes,
  VCL.stdctrls,
  VCL.ComCtrls,
  VCL.extctrls,
  VCL.CheckLst,
  Math,
  VCL.Dialogs,
  VCL.Forms,
  VCL.Grids,
  Windows,
  VCL.Graphics,

  UHelpContexts,
  UAbstractObject,
  UDataComponent,
  UAbstractComponent,
  URainfallZoneMenuItemManager,
  UMenuItemManager,
  URainfallZoneDialog,
  UDataObjectRainfall,
  UCatchmentZone,
  UProgressDialog,
  UGenericModelLinkClasses;

type

  TRainfallZoneValidator = class(TAbstractDataDialogValidator)
  protected
    FShowTree        : boolean;
    FMenuItemManager : TRainfallZoneMenuItemManager;
    FSelectedCatchment : string;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    procedure ResetMenuItemManager;
    function RainfallZoneDialog : TRainfallZoneDialog;
    procedure DoGaugeTreeViewChanged(ASender : TObject; ANode: TTreeNode);
    procedure PopulateTreeView;
    procedure PopulateCatchmentSources;

    procedure PopulateCatchmentFileList;
    procedure CreateCatchmentFileList(ANewCatchment : string);
    function AddToCatchment(ACatchmentZone : TCatchmentZone;
                            ASourceStationID : Integer;
                            ASplitIndex      : integer;
                            ASourcePatchID   : Integer;
                            ALevel           : integer): Boolean;
    function DeleteFromZone(ASourcePatchID : Integer; AStationId: Integer): Boolean;
    function DeleteCatchmentFileList(ACatchment : string) : boolean;
    function GetSelectedCatchment : TCatchmentZone;
    procedure DoZoneListBoxClick(Sender : TObject);
    procedure DoDrawGrid (Sender : TObject;
                          ACol   : integer;
                          ARow   : Integer;
                          Rect   : TRect;
                          State  : TGridDrawState);
    procedure DoIncludeUnreliableCheckBoxClick ( Sender : TObject );
    procedure DoRunHDYP08(Sender : TObject);
    procedure DoPeriodRadioGroupClick(Sender : TObject);
  public
    function Initialise: boolean; override;
    function LanguageHasChanged: boolean; override;
    function StudyHasChanged: boolean; override;
    procedure PopulateDataViewer; override;
    procedure DoAddToZone(ASender : TObject);
    procedure DoCreateCatchmentFile(ASender : TObject);
    procedure DoDeleteFromZone(ASender : TObject);
    procedure DoDeleteCatchmentFile(ASender: TObject);
    procedure DoToggleTree(ASender : TObject);
    function CanCopyToCLipboard: boolean; override;
    function CanExport: boolean; override;
    function CanPrint: boolean; override;
    procedure DoCopyToCLipboard; override;
    procedure DoExport(AFileName: string = ''); override;
    procedure DoPrint; override;
    property MenuItemManager   : TRainfallZoneMenuItemManager read FMenuItemManager write FMenuItemManager;
  end;

implementation

uses
  System.UITypes,
  SysUtils,
  VCL.Controls,
  VCL.Printers,
  UConstants,
  RainfallCom_TLB,
  UErrorHandlingOperations;

{ TRainfallZoneValidator }

procedure TRainfallZoneValidator.CreateMemberObjects;
const OPNAME = 'TRainfallZoneValidator.CreateMemberObjects';
begin
  inherited CreateMemberObjects;
  try
    FShowTree := TRUE;
    FPanel    := TRainfallZoneDialog.Create(nil, FAppModules);
    with RainfallZoneDialog do
    begin
      GaugeGrid.OnDrawCell := DoDrawGrid;
      ZoneListBox.OnClick := DoZoneListBoxClick;
      btnRunHDYP08.OnClick := DoRunHDYP08;
      RainfallZoneDialog.PeriodRadioGroup.OnClick := DoPeriodRadioGroupClick;
      PanelHDYP08.Visible := False;
    end;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TRainfallZoneValidator.DestroyMemberObjects;
const OPNAME = 'TRainfallZoneValidator.DestroyMemberObjects';
begin
  inherited DestroyMemberObjects;
  try
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TRainfallZoneValidator.RainfallZoneDialog : TRainfallZoneDialog;
const OPNAME = 'TRainfallZoneValidator.RainfallZoneDialog';
begin
  Result := nil;
  try
    if (FPanel <> nil) then
      Result := TRainfallZoneDialog(FPanel);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TRainfallZoneValidator.ResetMenuItemManager;
const OPNAME = 'TRainfallZoneValidator.ResetMenuItemManager';
begin
  try
    if FMenuItemManager <> nil then
    begin
      if GetSelectedCatchment <> nil then
        FMenuItemManager.SetAddToZone((GetSelectedCatchment.CatchmentFileName <> ''));
      FMenuItemManager.SetCreateCatchmentZone(True);
      if GetSelectedCatchment <> nil then
        FMenuItemManager.SetDeleteCatchmentZone((GetSelectedCatchment.CatchmentFileName <> ''));
      if GetSelectedCatchment <> nil then
        FMenuItemManager.SetRemoveFromZone((GetSelectedCatchment.CatchmentFileName <> '') and
                                           (RainfallZoneDialog.GaugeGrid.RowCount > 3));
       RainfallZoneDialog.PanelHDYP08.Visible := (RainfallZoneDialog.GaugeGrid.RowCount > 3);
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TRainfallZoneValidator.Initialise: boolean;
const OPNAME = 'TRainfallZoneValidator.Initialise';
var
  LIndex    : integer;
  LRowIndex : integer;
begin
  Result := inherited Initialise;
  try
    with RainfallZoneDialog do
    begin
      GaugeGrid.RowCount := 3;
      GaugeGrid.RowCount := 4;
      GaugeGrid.RowHeights[2] := 8;
      GaugeGrid.Visible := False;
      PanelHDYP08.Visible := False;
      for LRowIndex := 1 to GaugeGrid.RowCount - 1 do
        for LIndex := 0 to GaugeGrid.ColCount - 1 do
          GaugeGrid.Cells[LIndex, LRowIndex] := '';

      GaugeTreeview.OnChange := DoGaugeTreeViewChanged;
      IncludeUnreliableChkBox.Visible := False;
    end;
    if (FMenuItemManager <> nil) then
      FMenuItemManager.Initialise;
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TRainfallZoneValidator.LanguageHasChanged: boolean;
const OPNAME = 'TRainfallZoneValidator.LanguageHasChanged';
begin
  Result := inherited LanguageHasChanged;;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRainfallZoneValidator.StudyHasChanged: boolean;
const OPNAME = 'TRainfallZoneValidator.StudyHasChanged';
begin
  Result := inherited StudyHasChanged;
  try
    PopulateDataViewer;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRainfallZoneValidator.PopulateDataViewer;
const OPNAME = 'TRainfallZoneValidator.PopulateDataViewer';
begin
  inherited PopulateDataViewer;
  try
    FSelectedCatchment := FAppModules.ViewIni.ReadString(ClassName,'SelectedCatchment','');
    RainfallZoneDialog.IncludeUnreliableChkBox.Checked :=
      (FAppModules.Model.ModelData as IRainfallModelData).IncludeUnreliableData;
    PopulateTreeView;
    PopulateCatchmentFileList;
    PopulateCatchmentSources;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRainfallZoneValidator.PopulateTreeView;
const OPNAME = 'TRainfallZoneValidator.PopulateTreeView';
var
  LIndex       : Integer;
  LPatchIndex  : Integer;
  lSplitIndex  : Integer;
  LCurrentNode : TTreeNode;
  lSplitNode   : TTreeNode;
  LStationID   : integer;
  lStation     : IStationData;
  lPatch       : IPatchData;
  lSplit       : IRainfallDataSplit;
  lRainfallObj : IRainfallModelData;
  lSplitName   : string;
  lFoundSplit  : boolean;
  lSrcPatchID  : integer;
  lTarget      : WideString;
  lStartYear   : integer;
  lEndYear     : integer;
begin
  try
    lRainfallObj := (FAppModules.Model.ModelData as IRainfallModelData);
    with RainfallZoneDialog do
    begin
      GaugeTreeview.Items.Clear;
      for LIndex := 0 to lRainfallObj.StationCount - 1 do
      begin
        lStation := lRainfallObj.GetStationDataByIndex(LIndex);
        if (lStation <> nil) then
        begin
          LStationID := lStation.RainfallData.StationID;
          LCurrentNode := GaugeTreeview.Items.AddObject(nil, lStation.RainfallData.StationNumber, TObject(LStationID));

          for lSplitIndex := 0 to lStation.SplitCount - 1 do
          begin
            lSplit     := lStation.GetSplitWithIndex(lSplitIndex);
            lSplitName := IntToStr(lSplit.HydroStartYear) + ' - ' + IntToStr(lSplit.HydroEndYear);
            GaugeTreeview.Items.AddChildObject(LCurrentNode, lSplitName, TObject(LStationID));
          end;

          for LPatchIndex := 0 to lStation.PatchCount - 1 do
          begin
            lPatch := lStation.GetPatchWithIndex(lPatchIndex);
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
              GaugeTreeview.Items.AddChildObject(lSplitNode, lPatch.PatchName, TObject(lPatch.PatchID));
          end;
        end;
      end;

      GaugeTreeview.FullExpand;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRainfallZoneValidator.DoAddToZone(ASender: TObject);
const OPNAME = 'TRainfallZoneValidator.DoAddToZone';
var
  LSourcePatchId : integer;
  LStationId     : integer;
  lSplitIndex    : integer;
  LCatchmentZone : TCatchmentZone;
begin
  try
    with RainfallZoneDialog do
    begin
      if (GaugeTreeview.Selected = nil) then
      begin
        ShowMessage(FAppModules.Language.GetString('Message.SelectRawOrPatch'));
      end
      else
      begin
        LStationId     := 0;
        lSplitIndex    := -1;
        LSourcePatchId := 0;
        if (GaugeTreeview.Selected.Level = 0) then
        begin
          LStationId     := Integer(GaugeTreeview.Selected.Data);
          lSplitIndex    := 0;
          LSourcePatchId := 0;
        end
        else
        if (GaugeTreeview.Selected.Level = 1) then
        begin
          LStationId     := Integer(GaugeTreeview.Selected.Parent.Data);
          lSplitIndex    := GaugeTreeview.Selected.Index;
          LSourcePatchId := 0;
        end
        else
        if (GaugeTreeview.Selected.Level = 2) then
        begin
          LStationId     := Integer(GaugeTreeview.Selected.Parent.Data);
          lSplitIndex    := GaugeTreeview.Selected.Parent.Index;
          LSourcePatchId := Integer(GaugeTreeview.Selected.Data);
        end;
        LCatchmentZone := GetSelectedCatchment;
        if LCatchmentZone <> nil then
          AddToCatchment(LCatchmentZone,LStationId, lSplitIndex, LSourcePatchId,GaugeTreeview.Selected.Level)
        else
          ShowMessage(FAppModules.Language.GetString('Rainfall.SelectCatchmentFileToAdd'));
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRainfallZoneValidator.DoDeleteFromZone(ASender: TObject);
const OPNAME = 'TRainfallZoneValidator.DoDeleteFromZone';
var
  LSourcePatchID : integer;
  LStationID     : integer;
begin
  try
    with RainfallZoneDialog do
    begin
      if (GaugeGrid.Row < 3) then
        ShowMessage(FAppModules.Language.GetString('Rainfall.SelectSourceStation'))
      else
      begin
        LSourcePatchID := Integer(GaugeGrid.Objects[1,GaugeGrid.Row]);
        LStationID := Integer(GaugeGrid.Objects[0,GaugeGrid.Row]);
        DeleteFromZone(LSourcePatchID, LStationID);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRainfallZoneValidator.DoToggleTree;
const OPNAME = 'TRainfallZoneValidator.DoToggleTree';
begin
  try
    FShowTree := NOT FShowTree;
    with RainfallZoneDialog do
    begin
      PanelTreeView.Visible := FShowTree;
      PanelLeft.Visible     := FShowTree;
      Splitter.Left         := PanelLeft.Left + (PanelLeft.Width + 1);
    end;
    if FShowTree then
      FAppModules.SetMenuItem(CToggleTree, msChecked)
    else
      FAppModules.SetMenuItem(CToggleTree, msUnChecked);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRainfallZoneValidator.DoGaugeTreeViewChanged (ASender : TObject;
                                                         ANode   : TTreeNode);
const OPNAME = 'TRainfallZoneValidator.DoGaugeTreeViewChanged';
begin
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRainfallZoneValidator.AddToCatchment(ACatchmentZone : TCatchmentZone;ASourceStationID : Integer;
                                               ASplitIndex : integer; ASourcePatchID : Integer; ALevel : integer): Boolean;
const OPNAME = 'TRainfallZoneValidator.AddToCatchment';
var
  LRainfallObj : TDataObjectRainfall;
  LStation : IStationData;
  LSplit : IRainfallDataSplit;
  LMessage : string;
  LStartYear,
  LEndYear : integer;
  LPatch : IPatchData;
begin
  Result := False;
  try
    if Assigned(FAppModules) and
       Assigned(FAppModules.Model()) and
       Assigned(FAppModules.Model.ModelData()) and
       (ACatchmentZone <> nil) then
    begin
      LRainfallObj := TDataObjectRainfall(FAppModules.Model.ModelData);
      if (ACatchmentZone.Validate(ASourceStationID,ASplitIndex,ASourcePatchID,ALevel,LMessage)) then
      begin
        if (Pos('WARNING', LMessage) > 0) or (Trim(LMessage) = '')then
        begin
          if (Pos('WARNING', LMessage) > 0) then
            MessageDlg(LMessage, mtWarning,[mbOk], 0);
          if (LRainfallObj <> nil) then
          begin
            LStartYear := 0;
            LEndYear := 0;
            LStation := LRainfallObj.GetStationDataByID(ASourceStationID);
            if LStation <> nil then
            begin
              LStartYear := LStation.RainfallData.HydroStartYear;
              LEndYear := LStation.RainfallData.HydroEndYear;
              LPatch := LStation.GetPatchWithID(ASourcePatchID);
              if LPatch <> nil then
              begin
                LStartYear := LPatch.PatchStartYear;
                LEndYear := LPatch.PatchEndYear;
              end;
              if ASplitIndex > 0 then
              begin
                LSplit := LStation.GetSplitWithIndex(ASplitIndex);
                LStartYear := LSplit.HydroStartYear;
                LEndYear := LSplit.HydroEndYear;
              end;
            end;
            LRainfallObj.AddToCatchmentZone(ACatchmentZone,ASourceStationID,ASplitIndex,ASourcePatchID, LStartYear, LEndYear);
            PopulateCatchmentSources;
            Result := True;
          end;
        end
        else
          MessageDlg(LMessage,mtInformation,[mbOk], 0);
      end
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TRainfallZoneValidator.DeleteFromZone(ASourcePatchID, AStationId: Integer): Boolean;
const OPNAME = 'TRainfallZoneValidator.DeleteFromZone';
var
  LRainfallObj : TDataObjectRainfall;
  LCatchmentZone : TCatchmentZone;
begin
  Result := False;
  try
    LRainfallObj := TDataObjectRainfall(FAppModules.Model.ModelData);
    if Assigned(FAppModules) and
       Assigned(FAppModules.Model()) and
       Assigned(FAppModules.Model.ModelData()) and
       (LRainfallObj <> nil) then
    begin
      if GetSelectedCatchment <> nil then
      begin
        LCatchmentZone := LRainfallObj.GetCatchmentZoneByName(GetSelectedCatchment.CatchmentFileName);
        Result := LRainfallObj.DeleteFromRainfallZone(AStationId, ASourcePatchID, LCatchmentZone);
        PopulateCatchmentSources;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRainfallZoneValidator.DeleteCatchmentFileList(ACatchment : string) : boolean;
const OPNAME = 'TRainfallZoneValidator.DeleteCatchmentFileList';
var
  LRainfallObj : TDataObjectRainfall;
begin
  Result := False;
  try
    LRainfallObj := TDataObjectRainfall(FAppModules.Model.ModelData);
    if Assigned(FAppModules) and
       Assigned(FAppModules.Model()) and
       Assigned(FAppModules.Model.ModelData()) and
       (LRainfallObj <> nil) then
    begin
      Result := LRainfallObj.DeleteCatchmentZoneByName(ACatchment);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


function TRainfallZoneValidator.GetSelectedCatchment : TCatchmentZone;
const OPNAME = 'TRainfallZoneValidator.GetSelectedCatchment';
var
  LIndex : integer;
  LRainfallObj : TDataObjectRainfall;
begin
  Result := nil;
  try
    LRainfallObj := TDataObjectRainfall(FAppModules.Model.ModelData);
    for LIndex := 0 to RainfallZoneDialog.ZoneListBox.Items.Count-1 do
    begin
      if RainfallZoneDialog.ZoneListBox.Selected[LIndex] then
      begin
        Result :=  LRainfallObj.GetCatchmentZoneByIndex(LIndex);
        FAppModules.ViewIni.WriteString(ClassName,'SelectedCatchment',Result.CatchmentFileName);
        Break;
      end;
    end;
    if (Result = nil) and (Trim(FSelectedCatchment) <> '') then
    begin
      LIndex := RainfallZoneDialog.ZoneListBox.Items.IndexOf(FSelectedCatchment);
      if (LIndex >= 0) then
      begin
        Result :=  LRainfallObj.GetCatchmentZoneByName(FSelectedCatchment);
        RainfallZoneDialog.ZoneListBox.Selected[LIndex] := True;
      end;
    end;  
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TRainfallZoneValidator.DoDrawGrid (Sender : TObject;
                                             ACol   : integer;
                                             ARow   : Integer;
                                             Rect   : TRect;
                                             State  : TGridDrawState);
const OPNAME = 'TRainfallZoneValidator.DoDrawGrid';
var
  LRect          : TRect;
  LOldColour     : TColor;
  LOldBrushStyle : TBrushStyle;
begin
  try
    with RainfallZoneDialog do
    begin
      if (ARow in [2]) AND (GaugeGrid.RowHeights[ARow] <= 8) then
      begin
        LOldColour := GaugeGrid.Canvas.Brush.Color;
        LOldBrushStyle := GaugeGrid.Canvas.Brush.Style;

        try
          LRect.Left := Rect.Left + 0;
          LRect.Top := Rect.Top + 0;
          LRect.Right := Rect.Right - 0;
          LRect.Bottom := Rect.Bottom - 0;
          GaugeGrid.Canvas.Brush.Color := clGray;
          GaugeGrid.Canvas.Brush.Style := bsSolid;
          GaugeGrid.Canvas.FillRect(LRect);
        finally
          GaugeGrid.Canvas.Brush.Style := LOldBrushStyle;
          GaugeGrid.Canvas.Brush.Color := LOldColour;
        end;
      end;
    end;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TRainfallZoneValidator.PopulateCatchmentSources;
const OPNAME = 'TRainfallZoneValidator.PopulateCatchmentSources';
var
  LIndex         : integer;
  LStationID     : integer;
  LSourcePatchID : integer;
  LStation       : IStationData;
  LSplit         : IRainfallDataSplit;
  LPatch         : IPatchData;
  LValue         : double;
  LLatStr        : WideString;
  LLonStr        : WideString;
  LStartYear     : integer;
  LEndYear       : integer;
  LCatchmentZone : TCatchmentZone;
  LCatchmentSource : TCatchmentSource;
  LRainfallObj : TDataObjectRainfall;
begin
  try
    LRainfallObj := TDataObjectRainfall(FAppModules.Model.ModelData);
   (FAppModules.Model.ModelData as IRainfallModelData).IncludeUnreliableData := True;
    with RainfallZoneDialog do
    begin
      LCatchmentZone := GetSelectedCatchment;
      GaugeGrid.Visible := False;
      PanelHDYP08.Visible := False;
      if LCatchmentZone = nil then
        Exit;
      GaugeGrid.Visible := True;
      GaugeGrid.RowCount := 3 + LCatchmentZone.CatchmentSourceCount;
      PanelHDYP08.Visible := GaugeGrid.RowCount > 3;
      for LIndex := 0 to LCatchmentZone.CatchmentSourceCount - 1 do
      begin
        LCatchmentSource := LCatchmentZone.CatchmentSourceByIndex[LIndex];
        LStationID       := LCatchmentSource.StationID;
        LSourcePatchID   := LCatchmentSource.PatchID;
        LStartYear       := LCatchmentSource.HydroStartYear;
        LEndYear         := LCatchmentSource.HydroEndYear;
        LStation       := LRainfallObj.GetStationDataByID(LStationID);
        if (lStation <> nil) then
        begin
          LStation.LatLong(LLatStr, lLonStr);
          LSplit := LStation.GetSplitForYears(LStartYear, lEndYear);
          if (LSplit <> nil) then
          begin
            if (LSourcePatchID = 0) then
            begin
              GaugeGrid.Cells[0, LIndex + 3]  := LStation.RainfallData.StationNumber;
              GaugeGrid.Cells[1, LIndex + 3]  := LStation.StationName;
              GaugeGrid.Cells[2, LIndex + 3]  := FAppModules.Language.GetString('Rainfall.RAW');
              GaugeGrid.Cells[3, LIndex + 3]  := LLatStr;
              GaugeGrid.Cells[4, LIndex + 3]  := LLonStr;
              GaugeGrid.Cells[5, LIndex + 3]  := IntToStr(LSplit.HydroStartYear);
              GaugeGrid.Cells[6, LIndex + 3]  := IntToStr(LSplit.HydroEndYear);
              if (RainfallZoneDialog.IncludeUnreliableChkBox.Checked) then
              begin
                GaugeGrid.Cells[7, LIndex + 3]  := Format('%6.1f', [LSplit.MAP]);
                GaugeGrid.Cells[8, LIndex + 3]  := Format('%6.1f', [LSplit.StdDeviation]);
                GaugeGrid.Cells[9, LIndex + 3]  := Format('%4.1f', [LSplit.CV]);
              end
              else
              begin
                GaugeGrid.Cells[7, LIndex + 3]  := Format('%6.1f', [LSplit.XMAP]);
                GaugeGrid.Cells[8, LIndex + 3]  := Format('%6.1f', [LSplit.XStdDeviation]);
                GaugeGrid.Cells[9, LIndex + 3]  := Format('%4.1f', [LSplit.XCV]);
              end;
              if ((LSplit.HydroStartYear = 0) AND (LSplit.HydroEndYear = 0)) then
                LValue := 0
              else
                LValue := (LSplit.HydroEndYear - LSplit.HydroStartYear + 1);
              GaugeGrid.Cells[11, LIndex + 3] := Format('%4d', [Trunc(LValue)]);
              if (LValue <> 0) then
                LValue := LSplit.NrOfMissingMonths * 100 / (lValue * 12);
              GaugeGrid.Cells[10, LIndex + 3] := Format('%6.2f', [LValue]);
              if (LStation.Height <> NullInteger) then
                GaugeGrid.Cells[12, LIndex + 3] := Format('%4d', [LStation.Height])
              else
                GaugeGrid.Cells[12, LIndex + 3] := '';
              GaugeGrid.Cells[13, LIndex + 3] := LStation.StationType;
            end;
          end;
          LPatch := LStation.GetPatchWithID(LSourcePatchID);
          if (LPatch <> nil) then
          begin
            LStation.LatLong(LLatStr, LLonStr);
            GaugeGrid.Cells[0, LIndex + 3]  := LStation.RainfallData.StationNumber;
            GaugeGrid.Cells[1, LIndex + 3]  := LStation.StationName;
            GaugeGrid.Cells[2, LIndex + 3]  := LPatch.PatchName;
            GaugeGrid.Cells[3, LIndex + 3]  := LLatStr;
            GaugeGrid.Cells[4, LIndex + 3]  := LLonStr;
            GaugeGrid.Cells[5, LIndex + 3]  := IntToStr(LPatch.RainfallData.StartYear);
            GaugeGrid.Cells[6, LIndex + 3]  := IntToStr(LPatch.RainfallData.EndYear);
            if (RainfallZoneDialog.IncludeUnreliableChkBox.Checked) then
            begin
              GaugeGrid.Cells[7, LIndex + 3]  := Format('%6.1f', [LPatch.RainfallData.MAP]);
              GaugeGrid.Cells[8, LIndex + 3]  := Format('%6.1f', [LPatch.RainfallData.StdDeviation]);
              GaugeGrid.Cells[9,LIndex + 3]  := Format('%4.1f', [LPatch.RainfallData.CV]);
            end
            else
            begin
              GaugeGrid.Cells[7, LIndex + 3]  := Format('%6.1f', [LPatch.RainfallData.XMAP]);
              GaugeGrid.Cells[8, LIndex + 3]  := Format('%6.1f', [LPatch.RainfallData.XStdDeviation]);
              GaugeGrid.Cells[9,LIndex + 3]  := Format('%4.1f', [LPatch.RainfallData.XCV]);
            end;
            if ((LPatch.RainfallData.EndYear = 0) and (LPatch.RainfallData.StartYear = 0)) then
              LValue := 0
            else
              LValue := (LPatch.RainfallData.EndYear - LPatch.RainfallData.StartYear + 1);
            GaugeGrid.Cells[11, LIndex + 3] := Format('%4d', [Trunc(LValue)]);
            if (LValue <> 0) then
              LValue := LPatch.RainfallData.NrOfMissingMonths * 100 / (lValue * 12);
            GaugeGrid.Cells[10, LIndex + 3] := Format('%6.2f', [LValue]);
            if (LStation.Height <> NullInteger) then
              GaugeGrid.Cells[12, LIndex + 3] := Format('%4d', [LStation.Height])
            else
              GaugeGrid.Cells[12, LIndex + 3] := '';
            GaugeGrid.Cells[13, LIndex + 3] := LStation.StationType;
          end;
          GaugeGrid.Objects[0 , LIndex + 3] := TObject(LStationID);
          GaugeGrid.Objects[1 , LIndex + 3] := TObject(LSourcePatchID);
        end;
      end;
    end;
    ResetMenuItemManager;
  except on E: Exception do HandleError(E, OPNAME) end;
end;



function TRainfallZoneValidator.CanPrint: Boolean;
const OPNAME = 'TRainfallZoneValidator.CanPrint';
begin
  Result := TRUE;
  try
  except on E:Exception do HandleError(E,OPNAME) end;
end;

function TRainfallZoneValidator.CanCopyToClipboard: Boolean;
const OPNAME = 'TRainfallZoneValidator.CanCopyToClipboard';
begin
  Result := TRUE;
  try
  except on E:Exception do HandleError(E,OPNAME) end;
end;

function TRainfallZoneValidator.CanExport: Boolean;
const OPNAME = 'TRainfallZoneValidator.CanExport';
begin
  Result := TRUE;
  try
  except on E:Exception do HandleError(E,OPNAME) end;
end;

procedure TRainfallZoneValidator.DoPrint;
const OPNAME = 'TRainfallZoneValidator.DoPrint';
var
  lPrinterOrientation : TPrinterOrientation;
begin
  try
    if ((FAppModules.Model.ModelData as IRainfallModelData).GetZoneCount > 0) then
    begin
      lPrinterOrientation := Printer.Orientation;
      try
        Printer.Orientation := poLandscape;
        RainfallZoneDialog.GaugeGrid.DoPrint('Rainfall Zone : ' + FAppModules.StudyArea.ScenarioLabel);
      finally
        Printer.Orientation := lPrinterOrientation;
      end;
    end
    else
      ShowMessage(FAppModules.Language.GetString('Rainfall.NoGaugesOnZone'));
  except on E:Exception do HandleError(E,OPNAME) end;
end;

procedure TRainfallZoneValidator.DoCopyToClipboard;
const OPNAME = 'TRainfallZoneValidator.DoCopyToClipboard';
begin
  try
    if ((FAppModules.Model.ModelData as IRainfallModelData).GetZoneCount > 0) then
      RainfallZoneDialog.GaugeGrid.DoCopyToClipboard
    else
      ShowMessage(FAppModules.Language.GetString('Rainfall.NoGaugesOnZone'));
  except on E:Exception do HandleError(E,OPNAME) end;
end;

procedure TRainfallZoneValidator.DoExport(AFileName: string = '');
const OPNAME = 'TRainfallZoneValidator.DoExport';
begin
  try
    if ((FAppModules.Model.ModelData as IRainfallModelData).GetZoneCount > 0) then
      RainfallZoneDialog.GaugeGrid.DoExport(AFileName)
    else
      ShowMessage(FAppModules.Language.GetString('Rainfall.NoGaugesOnZone'));
  except on E:Exception do HandleError(E,OPNAME) end;
end;

procedure TRainfallZoneValidator.DoIncludeUnreliableCheckBoxClick ( Sender : TObject );
const OPNAME = 'TRainfallZoneValidator.DoIncludeUnreliableCheckBoxClick';
var
  lRainfallObj : IRainfallModelData;
begin
  try
    lRainfallObj := (FAppModules.Model.ModelData as IRainfallModelData);
    lRainfallObj.IncludeUnreliableData := RainfallZoneDialog.IncludeUnreliableChkBox.Checked;
    PopulateCatchmentSources;    
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TRainfallZoneValidator.DoZoneListBoxClick(Sender : TObject);
const OPNAME = 'TRainfallZoneValidator.DoZoneListBoxClick';
begin
  try
    PopulateCatchmentSources;
    ResetMenuItemManager;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TRainfallZoneValidator.CreateCatchmentFileList(ANewCatchment : string);
const OPNAME = 'TRainfallZoneValidator.CreateCatchmentFileList';
var
  LRainfallObj : TDataObjectRainfall;
begin
  try
    LRainfallObj := TDataObjectRainfall(FAppModules.Model.ModelData);
    LRainfallObj.CreateCatchmentZone(ANewCatchment);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TRainfallZoneValidator.DoCreateCatchmentFile(ASender: TObject);
const OPNAME = 'TRainfallZoneValidator.DoCreateCatchmentFile';
var
  LNewCatchment : string;
begin
  try
    LNewCatchment := InputBox(FAppModules.Language.GetString('Rainfall.EnterRainfallFile'),FAppModules.Language.GetString('Rainfall.CatchmentRainfallFile'),'');
    if LNewCatchment <> '' then
      CreateCatchmentFileList(LNewCatchment);
   PopulateCatchmentFileList;
   PopulateCatchmentSources;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;


procedure TRainfallZoneValidator.DoDeleteCatchmentFile(ASender: TObject);
const OPNAME = 'TRainfallZoneValidator.DoDeleteCatchmentFile';
var
  LCatchment : string;
begin
  try
    if GetSelectedCatchment <> nil then
    begin
      LCatchment := GetSelectedCatchment.CatchmentFileName;
      if (MessageDlg(Format('Are you sure you want to delete %s',[LCatchment]),mtConfirmation,[mbYes,mbNo],0) = mrYes) then
        DeleteCatchmentFileList(LCatchment);
      PopulateCatchmentFileList;
      PopulateCatchmentSources;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TRainfallZoneValidator.PopulateCatchmentFileList;
const OPNAME = 'TRainfallZoneValidator.PopulateCatchmentFileList';
var
  LCatchmentZone : TCatchmentZone;
  LRainfallObj : TDataObjectRainfall;
  LIndex : integer;
begin
  try
    LRainfallObj := TDataObjectRainfall(FAppModules.Model.ModelData);
    for LIndex := 0 to RainfallZoneDialog.ZoneListBox.Count -1 do
      RainfallZoneDialog.ZoneListBox.Items.Delete(LIndex);
    RainfallZoneDialog.ZoneListBox.Clear;
    for LIndex := 0 to LRainfallObj.GetCatchmentZoneListCount -1 do
    begin
      LCatchmentZone := LRainfallObj.GetCatchmentZoneByIndex(LIndex);
      if LCatchmentZone <> nil then
        RainfallZoneDialog.ZoneListBox.Items.AddObject(LCatchmentZone.CatchmentFileName,LCatchmentZone);
    end;
    ResetMenuItemManager;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


procedure TRainfallZoneValidator.DoRunHDYP08(Sender: TObject);
const OPNAME = 'TRainfallZoneValidator.DoRunHDYP08';
var
  LProgressDialog : TProgressDialog;
  LRainfallObj : TDataObjectRainfall;
  LFirstYear : integer;
  LLastYear : integer;
begin
  try
    LProgressDialog := TProgressDialog.Create(nil,FAppModules);
    LRainfallObj := TDataObjectRainfall(FAppModules.Model.ModelData);
    try
      if LRainfallObj <> nil then
      begin
        LRainfallObj.SetSelectedCatchmentZone(GetSelectedCatchment.CatchmentFileName);
        if (RainfallZoneDialog.PeriodRadioGroup.ItemIndex = 1) then
        begin
          if (Trim(RainfallZoneDialog.CbxFirstYear.Text) <> '') and
             (Trim(RainfallZoneDialog.CbxFirstYear.Text) <> '') then
          begin
            LFirstYear := StrToInt(Trim(RainfallZoneDialog.CbxFirstYear.Text));
            LLastYear := StrToInt(Trim(RainfallZoneDialog.CbxLastYear.Text));
            LRainfallObj.SetCatchmentStartEndYear(LFirstYear,LLastYear);
          end;
        end
        else
          LRainfallObj.SetCatchmentStartEndYear(0,0);
        LProgressDialog.AddExecutionFunctions(LRainfallObj.ExecHDYP08);
        LProgressDialog.Caption := FAppModules.Language.GetString('Rainfall.RunHDYP08');
        LProgressDialog.Succsessful := False;
        LProgressDialog.ShowModal;
      end;
    finally
      FreeAndNil(LProgressDialog);
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TRainfallZoneValidator.DoPeriodRadioGroupClick(Sender: TObject);
const OPNAME = 'TRainfallZoneValidator.DoPeriodRadioGroupClick';
begin
  try
    case RainfallZoneDialog.PeriodRadioGroup.ItemIndex of
      0 :
      begin
        RainfallZoneDialog.CbxFirstYear.Enabled := False;
        RainfallZoneDialog.CbxFirstYear.Color := clBtnFace;
        RainfallZoneDialog.lblFirstYear.Enabled := False;
        RainfallZoneDialog.lblFirstYear.Color := clBtnFace;
        
        RainfallZoneDialog.CbxLastYear.Enabled := False;
        RainfallZoneDialog.CbxLastYear.Color := clBtnFace;
        RainfallZoneDialog.lblLastYear.Enabled := False;
        RainfallZoneDialog.lblLastYear.Color := clBtnFace;
      end;
      1 :
      begin
        RainfallZoneDialog.CbxFirstYear.Enabled := True;
        RainfallZoneDialog.CbxFirstYear.Color := clWindow;
        RainfallZoneDialog.lblFirstYear.Enabled := True;
        RainfallZoneDialog.CbxLastYear.Enabled := True;
        RainfallZoneDialog.CbxLastYear.Color := clWindow;
        RainfallZoneDialog.lblLastYear.Enabled := True;
      end;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

end.


