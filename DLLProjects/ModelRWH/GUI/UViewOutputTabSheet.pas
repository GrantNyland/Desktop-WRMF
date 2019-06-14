//
//
//  UNIT      : Contains TViewOutputTabSheet Class
//  AUTHOR    : Dziedzi Ramulondi (arivia.kom)
//  DATE      : 25/02/2014
//  COPYRIGHT : Copyright © 2014 DWAF
//
//
unit UViewOutputTabSheet;

interface
uses
  System.Classes,
  Vcl.Graphics,
  Vcl.ComCtrls,
  Vcl.Controls,
  Vcl.Forms,
  UAbstractComponent,
  URWHDataObject,
  UDataViewerSheet;
type

  TViewOutputTabSheet = class(TDataViewerSheet)
  protected
    procedure CreateMemberObjects; override;
    function GetToolBar: TAbstractToolBar; override;
    procedure OnTreeViewItemSelected(Sender: TObject; Node: TTreeNode);
    procedure OnCustomDrawItemEvent(Sender: TCustomTreeView; Node: TTreeNode; State: TCustomDrawState; var DefaultDraw: Boolean);
  public
    procedure TabHasChanged; override;
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
  UFrameOutputResults,
  UErrorHandlingOperations;

const
  FirstNodeCaption = 'Selected Station List';

{ TRWHTabSheet }

procedure TViewOutputTabSheet.CreateMemberObjects;
const OPNAME = 'TViewOutputTabSheet.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
    FTabCaptionKey := 'RWHViewOutput';
    FViewTypeConstant := 'RWHRunConfiguration';

    FTreeView.OnChange := OnTreeViewItemSelected;
    FTreeView.OnCustomDrawItem := OnCustomDrawItemEvent;
    FTreeView.Font.Style := FTreeView.Font.Style + [fsBold];

    frmOutputResults := TfrmOutputResults.Create(Self);
    frmOutputResults.Parent := Self;
    frmOutputResults.Align  := alClient;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TViewOutputTabSheet.GetToolBar: TAbstractToolBar;
const OPNAME = 'TViewOutputTabSheet.GetToolBar';
begin
  Result := nil;
  try

  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TViewOutputTabSheet.Initialise: boolean;
const OPNAME = 'TViewOutputTabSheet.Initialise';
begin
  Result := inherited Initialise;
  try
    frmOutputResults.Initialise;
    Result := True;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TViewOutputTabSheet.LanguageHasChanged: boolean;
const OPNAME = 'TViewOutputTabSheet.LanguageHasChanged';
begin
  FTreeView.Items.Clear;
  Result := inherited LanguageHasChanged;
  try
    PopulateTreeView;
    frmOutputResults.LanguageHasChanged;
    Result := True;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TViewOutputTabSheet.OnCustomDrawItemEvent(Sender: TCustomTreeView; Node: TTreeNode; State: TCustomDrawState; var DefaultDraw: Boolean);
const OPNAME = 'TfrmRainfallStationSelection.tvRainfallStationCustomDrawItem';
var
  LStation  : TRainfallStation;
  LFileName : string;
begin
  try
    if TAbstractMainForm(Application.MainForm).Closing then Exit;

    if(State = []) and (Node.Level=1) and (Node.Data <> nil) then
    begin
      LStation  := TRainfallStation(Node.Data);
      LFileName := RWHModelData.GetRainfallStationOutputDailyFileName(LStation.StationNumber);
      if FileExists(LFileName) then
        Sender.Canvas.Font.Color := clBlue
        //Sender.Canvas.Font.Color := clBlue
      else
        Sender.Canvas.Font.Color := clBlack;
        //Sender.Canvas.Font.Color := clTeal;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TViewOutputTabSheet.OnTreeViewItemSelected(Sender: TObject; Node: TTreeNode);
const OPNAME = 'TViewOutputTabSheet.OnTreeViewItemSelected';
begin
  try
    RWHModelData.SelectedRainfallStation := nil;
    if(Node.Data <> nil) then
    begin
      RWHModelData.SelectedRainfallStation  := TRainfallStation(Node.Data);
    end;
    frmOutputResults.AfterInitialise;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TViewOutputTabSheet.StudyHasChanged: boolean;
const OPNAME = 'TViewOutputTabSheet.StudyHasChanged';
begin
  Result := inherited StudyHasChanged;
  try

    Result := True;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TViewOutputTabSheet.TabHasChanged;
const OPNAME = 'TViewOutputTabSheet.TabHasChanged';
begin
  try
    PopulateTreeView;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TViewOutputTabSheet.PopulateTreeView;
const OPNAME = 'TViewOutputTabSheet.PopulateTreeView';
var
  LIndex     : integer;
  LNode      : TTreeNode;
  LStation  : TRainfallStation;
begin
  try
    FTreeView.Items.Clear;
    LNode := FTreeView.Items.Add(Nil,FirstNodeCaption);
    if(RWHModelData.SelectedRainfallStationList.Count > 0) then
    begin
      for LIndex := 0 to RWHModelData.SelectedRainfallStationList.Count-1 do
      begin
        LStation := RWHModelData.SelectedRainfallStationList.RainfallStationByIndex[LIndex];
        FTreeView.Items.AddChildObject(LNode,LStation.StationNumber,LStation);
      end;
      FTreeView.AlphaSort;
      FTreeView.FullExpand;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

end.
