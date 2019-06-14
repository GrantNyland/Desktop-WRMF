//
//
//  UNIT      : Contains TRWHGaugeSelectionTabSheet Class
//  AUTHOR    : Sam Dhlamini(arivia.kom)
//  DATE      : 06/02/2004
//  COPYRIGHT : Copyright © 2004 DWAF
//
//

unit URWHGaugeSelectionTabSheet;


interface
uses
  System.Classes,
  Vcl.Graphics,
  Vcl.ComCtrls,
  Vcl.Controls,
  UAbstractObject,
  UAbstractComponent,
  UFrameGaugeSelection,
  URWHDataObject,
  UConstants,
  UDataViewerSheet;

type
  TRWHGaugeSelectionTabSheet = class(TDataViewerSheet)
  protected
    procedure CreateMemberObjects; override;
    function GetToolBar: TAbstractToolBar; override;
    procedure OnTreeViewItemSelected(Sender: TObject; Node: TTreeNode);
    procedure DoGISSelectionChanged(Sender: TObject);
    procedure DoClearSelection(Sender: TObject);
    procedure DoDeleteStationSelected(Sender: TObject);
    function RWHModelData : TRWHModelData;
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
  FirstNodeCaption = 'Selected Stations List';

{ TRWHGaugeSelectionTabSheet }

procedure TRWHGaugeSelectionTabSheet.CreateMemberObjects;
const OPNAME = 'TRWHGaugeSelectionTabSheet.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
    FTabCaptionKey := 'RWHGaugeSelection';
    FViewTypeConstant := 'RWHGaugeSelection';
    frmGaugeSelection := TfrmGaugeSelection.Create(Self);
    frmGaugeSelection.Parent := Self;
    frmGaugeSelection.Align  := alClient;
    frmGaugeSelection.AppModules := FAppModules;
    FTreeView.OnChange := OnTreeViewItemSelected;
    frmGaugeSelection.OnGISSelectionChanged := DoGISSelectionChanged;
    frmGaugeSelection.OnClearSelection := DoClearSelection;
    FTreeView.PopupMenu := frmGaugeSelection.pmGisSelection;
    frmGaugeSelection.OnDeleteSelectedStation := DoDeleteStationSelected;
    frmGaugeSelection.importRawDailyData.Visible := (FAppModules.User.UserRights = CUR_SysAdmin);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TRWHGaugeSelectionTabSheet.GetToolBar: TAbstractToolBar;
const OPNAME = 'TRWHGaugeSelectionTabSheet.GetToolBar';
begin
  Result := nil;
  try

  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TRWHGaugeSelectionTabSheet.Initialise: boolean;
const OPNAME = 'TRWHGaugeSelectionTabSheet.Initialise';
begin
  Result := inherited Initialise;
  try
    Self.Font.Style := Self.Font.Style + [fsBold];
    frmGaugeSelection.Initialise;
    frmGaugeSelection.rdgDailyDataSource.ItemIndex := Ord(RWHModelData.DailyDataSource);
    Result := True;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TRWHGaugeSelectionTabSheet.LanguageHasChanged: boolean;
const OPNAME = 'TRWHGaugeSelectionTabSheet.LanguageHasChanged';
begin
  FTreeView.Items.Clear;
  Result := inherited LanguageHasChanged;
  try
    PopulateTreeView;
    Result := True;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TRWHGaugeSelectionTabSheet.DoClearSelection(Sender: TObject);
const OPNAME = 'TRWHGaugeSelectionTabSheet.DoClearSelection';
begin
  try
    frmGaugeSelection.FSelectedList.Clear;
    RWHModelData.RemoveAllRainfallStationFromSelection;
    frmGaugeSelection.UpdateFromList;
    PopulateTreeView;
    FAppModules.Model.StudyDataHasChanged(sdccEdit,'Gauge','0', '0');
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;


procedure TRWHGaugeSelectionTabSheet.DoDeleteStationSelected(Sender: TObject);
const OPNAME = 'TRWHGaugeSelectionTabSheet.DoDeleteStationSelected';
var
  LStation : TRainfallStation;
  LNode : TTreeNode;
 // LIndex : integer;
begin
  try
    //LNode := nil;
    if (FTreeView.Items.Count > 0) then
    begin
      LNode := FTreeView.Selected;
      if LNode <> nil then
      begin
        LStation := RWHModelData.SelectedRainfallStationList.RainfallStationByStationNumber[LNode.Text];
        if (LStation <> nil) then
        begin
          RWHModelData.RemoveRainfallStationFromSelection(LStation.StationID);
         { if frmGaugeSelection.FSelectedList.Count > 0 then
          begin
            LIndex := frmGaugeSelection.FSelectedList.IndexOf(LNode.Text);
            if LIndex > -1 then
              frmGaugeSelection.FSelectedList.Delete(LIndex);
          end;}
        end;
      end;
    end;
    frmGaugeSelection.UpdateFromList;
    PopulateTreeView;
    FAppModules.Model.StudyDataHasChanged(sdccEdit,'Gauge','0', '0');
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TRWHGaugeSelectionTabSheet.DoGISSelectionChanged(Sender: TObject);
const OPNAME = 'TRWHGaugeSelectionTabSheet.DoGISSelectionChanged';
var
  LIndex : integer;
  LStationNum : string;
  LSelectedData : TStringList;
begin
  try
    if frmGaugeSelection.FSelectedList <> nil then
    begin
      LSelectedData := TStringList.Create;
      LSelectedData.Duplicates :=  dupAccept;
      LSelectedData.Sorted := False;
      LSelectedData.Delimiter := ',';
      LSelectedData.QuoteChar := '"';
      LSelectedData.StrictDelimiter := True;
      try
        for LIndex := 0 to frmGaugeSelection.FSelectedList.Count-1 do
        begin
          LStationNum := '';
          //if frmGaugeSelection.rgLayerSelection.ItemIndex = 3 then
          //begin
          LStationNum :=  frmGaugeSelection.FSelectedList[LIndex];
          {end
          else
          begin
             LSelectedData.CommaText := frmGaugeSelection.FSelectedList[LIndex];
             if LSelectedData.Count>1 then
               LStationNum := LSelectedData[1];
          end;
          if LStationNum <> '' then
          }
          RWHModelData.AddSelectedRainfallStationList(LStationNum);
        end;
      finally
        FreeAndNil(LSelectedData);
      end;
    end;
    PopulateTreeView;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;


function TRWHGaugeSelectionTabSheet.RWHModelData: TRWHModelData;
const OPNAME = 'TRWHGaugeSelectionTabSheet.RWHModelData';
begin
  Result := nil;
  try
    Result :=  TRWHModelData(FAppModules.Model.ModelData);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRWHGaugeSelectionTabSheet.PopulateTreeView;
const OPNAME = 'TRWHGaugeSelectionTabSheet.PopulateTreeView';
var
  LIndex     : integer;
  LNode      : TTreeNode;
  LSelectedStation : TRainfallStation;
begin
  try
    FTreeView.Items.Clear;
    if(RWHModelData.SelectedRainfallStationList.Count > 0) then
    begin
      LNode := FTreeView.Items.Add(Nil,FirstNodeCaption);
      for LIndex := 0 to RWHModelData.SelectedRainfallStationList.Count-1 do
      begin
        LSelectedStation := RWHModelData.SelectedRainfallStationList.RainfallStationByIndex[LIndex];
        FTreeView.Items.AddChildObject(LNode,LSelectedStation.StationNumber,LSelectedStation);
      end;
      LNode.Expand(True);
    end;

  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TRWHGaugeSelectionTabSheet.OnTreeViewItemSelected(Sender: TObject; Node: TTreeNode);
const OPNAME = 'TRWHGaugeSelectionTabSheet.OnTreeViewItemSelected';
var
  LSelectedStation : TRainfallStation;
begin
  try
    frmGaugeSelection.edtStationNumber.Text := '';
    frmGaugeSelection.edtName.Text := '';
    frmGaugeSelection.edtStationType.Text := '';
    frmGaugeSelection.edtType.Text := '';
    if(Node.Data <> nil) then
    begin
      frmGaugeSelection.FSelectedList.Clear;
      frmGaugeSelection.FSelectedList.Add(Node.Text);
      LSelectedStation := RWHModelData.SelectedRainfallStationList.RainfallStationByStationNumber[Node.Text];
      if LSelectedStation <> nil then
      begin
        frmGaugeSelection.edtStationNumber.Text := LSelectedStation.StationNumber;
        frmGaugeSelection.edtName.Text := LSelectedStation.StationName;
        frmGaugeSelection.edtStationType.Text := LSelectedStation.StationType;
        frmGaugeSelection.edtType.Text := 'Rainfall Stations';  //LSelectedStation.StationOwner;
      end;
      frmGaugeSelection.UpdateFromList;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TRWHGaugeSelectionTabSheet.StudyHasChanged: boolean;
const OPNAME = 'TRWHGaugeSelectionTabSheet.StudyHasChanged';
begin
  Result := inherited StudyHasChanged;
  try
    Result := True;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

end.
