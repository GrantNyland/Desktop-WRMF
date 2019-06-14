
{******************************************************************************}
{*  UNIT      : Contains TfrmGaugeSelection Class                             *}
{*  AUTHOR    : Sam Dhlamini                                                  *}
{*  DATE      : 14/03/2014                                                    *}
{*  COPYRIGHT : Copyright © 2015 DWAF                                         *}
{******************************************************************************}

unit UFrameGaugeSelection;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  Winapi.ActiveX,
  System.SysUtils,
  System.Variants,
  System.Classes,
  System.Win.StdVCL,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.ComCtrls,
  Vcl.Grids,
  Vcl.StdCtrls,
  Vcl.ExtCtrls,
  Vcl.Buttons,
  Vcl.AxCtrls,
  Math,
  //UColourButtons,
  UOleVariantEnum,
  UAbstractObject,
  URWHDataObject,
//  GisViewerX41Control_TLB,
  Vcl.Menus,
  MapWinGIS_TLB;
//  MapWinGIS32_TLB;

type

  TfrmGaugeSelection = class(TFrame)
    gbSelection: TGroupBox;
    lblType: TLabel;
    lblName: TLabel;
    lblStationNumber: TLabel;
    lblStationType: TLabel;
    edtType: TEdit;
    edtName: TEdit;
    edtStationNumber: TEdit;
    edtStationType: TEdit;
    pnlLayerSection: TPanel;
    rgLayerSelection: TRadioGroup;
    pmGisSelection: TPopupMenu;
    ClearAll1: TMenuItem;
    DeleteStation1: TMenuItem;
    rdgDailyDataSource: TRadioGroup;
    importRawDailyData: TButton;

    procedure rgLayerSelectionClick(Sender: TObject);
    procedure ClearAll1Click(Sender: TObject);
    procedure DeleteStation1Click(Sender: TObject);
    procedure rdgDailyDataSourceClick(Sender: TObject);
    procedure importRawDailyDataClick(Sender: TObject);
    procedure DoOnSelectBoxFinal(ASender: TObject; left, right, bottom, top: Integer);
    procedure DoOnShapeIdentified(ASender: TObject; LayerHandle: Integer; ShapeIndex: Integer;
                                                    pointX: Double; pointY: Double);
    procedure DoOnShapesIdentified(ASender: TObject; const selectedShapes: ISelectionList;
                                                     ProjX: Double; ProjY: Double);

    procedure DoOnShapeHighlighted(ASender: TObject; LayerHandle: Integer; ShapeIndex: Integer);

    procedure DoSelectionChanged(Sender : TObject;LayerHandle: Integer);
    procedure DoOnAfterShapeEdit(ASender: TObject; Operation: tkUndoOperation; LayerHandle: Integer; ShapeIndex: Integer);

  protected
    { Private declarations }
    FAppModules            : TAppModules;
    FEditContext           : TChangeContext;
    FSelectedStations      : TRainfallStationList;
    FPopulating            : boolean;
    //FGISViewer             : TGisViewerX;
    FGISViewerImage        : TImage;
    FGeographicalExtent    : TRect;
    FLayerHandle            : Integer;
    FOnGISSelectionChanged : TNotifyEvent;
    FOnClearSelection      : TNotifyEvent;
    FOnDeleteSelectedStation : TNotifyEvent;
    FExternal              : Boolean;
    FFieldName,
    FCurrFeature           : string;
    FGISViewer             : TMap;

    FToolPanel             : TPanel;
    FbtnResetMap           : TSpeedButton;
    FbtnZoomIn             : TSpeedButton;
    FbtnZoomOut            : TSpeedButton;
    FbtnPan                : TSpeedButton;
    FbtnSelect             : TSpeedButton;


    //function GetGISViewer  : TGisViewerX;
    procedure ClearDialog;
    procedure PopulateObject;
    procedure PopulateDialog;
    procedure SetAppModules(AAppModules : TAppModules);
    function RWHModelData : TRWHModelData;

    procedure DobtnResetMapOnClick(Sender : TObject);
    procedure DoFbtnSelectOnClick(Sender : TObject);
    procedure DoFbtnPanOnClick(Sender : TObject);
    procedure DoFbtnZoomOutOnClick(Sender : TObject);
    procedure DoFbtnZoomInOnClick(Sender : TObject);

    {function Query_RainStation : boolean;
    function Query_Province : boolean;
    function Query_WMA : boolean;
    function Query_District : boolean;
    function Query_LocM : boolean;
    function Query_DistrictM : boolean;
    function Query_Quan : boolean;
    }
    procedure LoadMapWinGis;
    procedure Resize; override;

  public
    { Public declarations }
    FSelectedList : TStringList;
    FStationList: TStringList;
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    function Initialise : boolean;
    function Finalise   : boolean;

    //procedure SelectRunConfig(ARunConfig : TRWHRunConfig);
    function LanguageHasChanged : boolean;
    //procedure DoGISSelectionChanged(Sender : TObject; aGisEvent : SYSINT);
    procedure UpdateFromList;
    property AppModules : TAppModules read FAppModules write SetAppModules;
    property OnGISSelectionChanged : TNotifyEvent read FOnGISSelectionChanged  write FOnGISSelectionChanged;
    property OnClearSelection : TNotifyEvent   read FOnClearSelection write FOnClearSelection;
    property OnDeleteSelectedStation : TNotifyEvent   read FOnDeleteSelectedStation write FOnDeleteSelectedStation;

   // property OnRunConfigAdded    : TNotifyEvent read FOnRunConfigAdded write FOnRunConfigAdded;
  //  property OnRunConfigDeleted  : TNotifyEvent read FOnRunConfigDeleted write FOnRunConfigDeleted;
  end;

var
  frmGaugeSelection : TfrmGaugeSelection;

implementation

{$R *.dfm}

uses
  System.UITypes,
  UConstants,
  UUtilities,
  UImportHydsraData,
  UErrorHandlingOperations;

{ TfrmGaugeSelection }

procedure TfrmGaugeSelection.AfterConstruction;
const OPNAME = 'TfrmGaugeSelection.AfterConstruction';
begin
  inherited;
  try
    FPopulating       := False;
    FSelectedList := TStringList.Create;
    FSelectedList.Duplicates := dupIgnore;
    FSelectedList.Sorted := true;
    FStationList  := TStringList.Create;
    FStationList.Duplicates := dupIgnore;
    FStationList.Sorted := true;

    FGISViewer := TMap.Create(Self);
    FGISViewer.Parent := Self;
    FGISViewer.Align := alClient;

    FToolPanel             := TPanel.Create(Self);
    FToolPanel.Parent      := Self;

    FbtnResetMap             := TSpeedButton.Create(self);
    FbtnResetMap.Parent      := FToolPanel;
    FbtnResetMap.OnClick     := DobtnResetMapOnClick;

    FbtnZoomIn             := TSpeedButton.Create(self);
    FbtnZoomIn.Parent      := FToolPanel;
    FbtnZoomIn.OnClick     := DoFbtnZoomInOnClick;

    FbtnZoomOut            := TSpeedButton.Create(self);
    FbtnZoomOut.Parent     := FToolPanel;
    FbtnZoomOut.OnClick    := DoFbtnZoomOutOnClick;

    FbtnPan                := TSpeedButton.Create(self);
    FbtnPan.Parent         := FToolPanel;
    FbtnPan.OnClick        := DoFbtnPanOnClick;

    FbtnSelect             := TSpeedButton.Create(self);
    FbtnSelect.Parent      := FToolPanel;
    FbtnSelect.OnClick     := DoFbtnSelectOnClick;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TfrmGaugeSelection.BeforeDestruction;
const OPNAME = 'TfrmGaugeSelection.AfterConstruction';
begin
  inherited;
  try
  //  if(FEditRunConfig <> nil) then
  //    FreeAndNil(FEditRunConfig);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TfrmGaugeSelection.SetAppModules(AAppModules: TAppModules);
const OPNAME = 'TfrmGaugeSelection.SetAppModules';
begin
  try
    FAppModules := AAppModules;
    if(AAppModules <> nil) then
  //    FEditRunConfig  := TRWHRunConfig.Create(FAppModules);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

 {
function TfrmGaugeSelection.GetGISViewer : TGisViewerX;
const OPNAME = 'TfrmGaugeSelection.GetGISViewer';
begin
  Result := nil;
  try
    if Assigned(FGISViewer) then
      Result := FGISViewer
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;
  }


procedure TfrmGaugeSelection.DoFbtnPanOnClick(Sender: TObject);
const OPNAME = 'TfrmGaugeSelection.DoFbtnPanOnClick';
begin
  try
    //FGISViewer.InertiaOnPanning := t
    FGISViewer.CursorMode := cmPan;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TfrmGaugeSelection.DobtnResetMapOnClick(Sender : TObject);
const OPNAME = 'TfrmGaugeSelection.DoFbtnSelectOnClick';
var
  x,y, span : double;
  LExtends : TMWGExtents;
begin
  try
    x := 25.0;
    y := -30.0;
    span := 10;
    LExtends := TMWGExtents.Create(nil);
    try
      LExtends.SetBounds(x-span,y-span,0.0,x+span,y+span,0.0);
      FGISViewer.Extents := LExtends.DefaultInterface;
    finally
      FreeAndNil(LExtends);
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TfrmGaugeSelection.DoFbtnSelectOnClick(Sender: TObject);
const OPNAME = 'TfrmGaugeSelection.DoFbtnSelectOnClick';
begin
  try
    FGISViewer.CursorMode := cmSelection;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TfrmGaugeSelection.DoFbtnZoomInOnClick(Sender: TObject);
const OPNAME = 'TfrmGaugeSelection.DoFbtnZoomInOnClick';
begin
  try
    FGISViewer.CursorMode := cmZoomIn;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;
procedure TfrmGaugeSelection.DoFbtnZoomOutOnClick(Sender: TObject);
const OPNAME = 'TfrmGaugeSelection.DoFbtnZoomOutOnClick';
begin
  try
    FGISViewer.CursorMode := cmZoomOut;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TfrmGaugeSelection.importRawDailyDataClick(Sender: TObject);
const OPNAME = 'TfrmGaugeSelection.GetGISViewer';
begin
  try
    if not Assigned(frmImportRawDailyData) then
      frmImportRawDailyData := TfrmImportRawDailyData.Create(Self);

    frmImportRawDailyData.AppModules := FAppModules;
    frmImportRawDailyData.Show;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TfrmGaugeSelection.Initialise: boolean;
const OPNAME = 'TfrmGaugeSelection.Initialise';
//var
  //LFileName : string;
begin
  Result := False;
  try

    {if not Assigned(FGISViewer) then
    begin
      FGISViewer := nil;
      try
        FGISViewer := TGisViewerX.Create(Self);
        FGISViewer.Parent := self;
        FGISViewer.ControlInterface.Control_Initialize;
        FGISViewer.Visible := True;
      except
        if FGISViewer <> nil then
        begin
          FGISViewer.Parent := nil;
          FGISViewer.Visible := False;
        end;
        FGISViewer := nil;
      end;
      if Assigned(FGISViewer) and (FGISViewer.Parent <> nil)then
      begin
        FGISViewer.Caption := '';
        FGISViewer.Align := alClient;
      end
      else
        Caption := '';
    end;
    if Assigned(FGISViewer) and (FGISViewer.Parent <> nil) then
    begin
      FGISViewer.Display_Interface  := True;
      FGISViewer.Display_ToolBar    := True ;
      FGISViewer.Display_DataTab    := False;
      FGISViewer.Display_Legend     := False;
      FGISViewer.Display_ScrollBars := False;
      FGISViewer.Display_StatusBar  := False;
      FGISViewer.Chart_ClearAll;
      FGISViewer.Map_SetFilePathCovers(GISCoversDirectory);
      LFileName := GISCoversDirectory + 'RWH.glf';
      UpdateGISShapeFilePath(LFileName);
      FGISViewer.ControlInterface.Layers_LoadLayout(LFileName,False);
      FGISViewer.ControlInterface.Control_RefreshAll;
      FGISViewer.Toolbar_Property(toolBuildQuery,True,True);
      FGISViewer.Toolbar_Property(toolSelectionMode,True,True);
      FGISViewer.Toolbar_Property(toolRemoveLayers,False,False);
      FGISViewer.Toolbar_Property(toolRemoveLayers,False,False);
      FGISViewer.Toolbar_Property(toolRemoveLayers,False,False);
      FGISViewer.OnGisEvent := DoGISSelectionChanged;
      FGISViewer.Toolbar_Mode(getSelectionMode);
      FCurrFeature := '';
      FFieldName := '';
      Query_RainStation;
    end;
    }

    LoadMapWinGis;
    FLayerHandle := -1;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TfrmGaugeSelection.LoadMapWinGis;
const OPNAME = 'TfrmGaugeSelection.LoadMapWinGis';
var
  LHandle : integer;
  LShape,
  LShape1,
  LShape2,
  LShape3,
  LShape5,
  LShape6,LShape7,LShape8,LShape9,
  LShape10, LShape11,
  LShape4 : TMWGShapefile;
  LExtends : TMWGExtents;
  y, span,
  x : double;
  LPath,
  LNewPath,
  LAppDrive : string;
begin
  try
    LShape := TMWGShapefile.Create(nil);
    LShape1 := TMWGShapefile.Create(nil);
    LShape2 := TMWGShapefile.Create(nil);
    LShape3 := TMWGShapefile.Create(nil);
    LShape4 := TMWGShapefile.Create(nil);
    LShape5 := TMWGShapefile.Create(nil);
    LShape6 := TMWGShapefile.Create(nil);
    LShape7 := TMWGShapefile.Create(nil);
    LShape8 := TMWGShapefile.Create(nil);
    LShape9 := TMWGShapefile.Create(nil);
    LShape10 := TMWGShapefile.Create(nil);
    LShape11 := TMWGShapefile.Create(nil);
    LExtends := TMWGExtents.Create(nil);
    try

      FGISViewer.Clear;

      LPath := FAppModules.IniFile.ReadString('GISPath','CurrentPath','' );
      if Trim(LPath) = '' then
        LPath := ExtractFilePath(ApplicationExeName);

      LAppDrive  := ExtractFileDrive(LPath);
      LNewPath   := GISCoversDirectory;

      if(UpperCase(LAppDrive) <> UpperCase(ExtractFileDrive(LNewPath))) then
      begin
        LNewPath := StringReplace(LPath,LPath,LNewPath,[rfIgnoreCase]);
        FAppModules.IniFile.WriteString('GISPath','CurrentPath',LNewPath);
      end;



      LShape.Open(LNewPath + 'International boundaries\africa.shp', LShape.GlobalCallback);

      LHandle := FGISViewer.AddLayer(LShape.DefaultInterface , True);
      FGISViewer.LayerVisible[LHandle] := True;
      FGISViewer.ShapeLayerFillTransparency[LHandle] := 0;
      x := 25.0;
      y := -30.0;
      span := 10;
      LExtends.SetBounds(x-span,y-span,0.0,x+span,y+span,0.0);
      FGISViewer.Extents := LExtends.DefaultInterface;
      {
      LShape1.Open(LNewPath + 'Dams and lakes\dams and lakes.shp', LShape1.GlobalCallback);

      LHandle := FGISViewer.AddLayer(LShape1.DefaultInterface , True);
      FGISViewer.LayerVisible[LHandle] := True;
      FGISViewer.ShapeLayerFillColor[LHandle] := clBlue;

      LShape2.Open(LNewPath +'Primary rivers\prim_riv.shp', LShape2.GlobalCallback);

      LHandle := FGISViewer.AddLayer(LShape2.DefaultInterface , True);
      FGISViewer.LayerVisible[LHandle] := True;
      FGISViewer.ShapeLayerLineColor[LHandle] := clBlue;

      LShape3.Open(LNewPath + 'All towns and cities (poly)\all towns and cities_poly.shp', LShape3.GlobalCallback);

      LHandle := FGISViewer.AddLayer(LShape3.DefaultInterface , True);
      FGISViewer.LayerVisible[LHandle] := True;
      FGISViewer.ShapeLayerFillColor[LHandle] := clSilver;
        }
      LShape4.Open(LNewPath + 'Rainfall Stations\rainfall_stations.shp', LShape4.GlobalCallback);

      LHandle := FGISViewer.AddLayer(LShape4.DefaultInterface , True);

      FGISViewer.ShapeLayerFillColor[LHandle] := clMaroon;
      FGISViewer.ShapeLayerPointType[LHandle]  := ptTriangleUp;
      FGISViewer.LayerVisible[LHandle] := True;
      {
      LShape5.Open(LNewPath + '30mingrid\30mingrid.shp', LShape5.GlobalCallback);

      LHandle := FGISViewer.AddLayer(LShape5.DefaultInterface , True);
      FGISViewer.LayerVisible[LHandle] := True;
      FGISViewer.ShapeLayerFillTransparency[LHandle] := 0;

         }
      LShape6.Open(LNewPath + 'Water Management Areas\wmareas.shp', LShape6.GlobalCallback);

      LHandle := FGISViewer.AddLayer(LShape6.DefaultInterface , True);
      FGISViewer.LayerVisible[LHandle] := False;
      FGISViewer.ShapeLayerFillTransparency[LHandle] := 0;


      LShape8.Open(LNewPath + 'Rainfall Districts\districtpol.shp', LShape8.GlobalCallback);

      LHandle := FGISViewer.AddLayer(LShape8.DefaultInterface , True);
      FGISViewer.LayerVisible[LHandle] := False;
      FGISViewer.ShapeLayerFillTransparency[LHandle] := 0;


      LShape7.Open(LNewPath + 'Quatenary catchments\catchments.shp', LShape7.GlobalCallback);

      LHandle := FGISViewer.AddLayer(LShape7.DefaultInterface , True);
      FGISViewer.LayerVisible[LHandle] := False;
      FGISViewer.ShapeLayerFillTransparency[LHandle] := 0;
      //FGISViewer.ShapeLayerFillColor[LHandle] := 45232;



      LShape9.Open(LNewPath + 'Provinces\province.shp', LShape9.GlobalCallback);

      LHandle := FGISViewer.AddLayer(LShape9.DefaultInterface , True);
      FGISViewer.LayerVisible[LHandle] := False;
      FGISViewer.ShapeLayerFillTransparency[LHandle] := 0;
     // FGISViewer.ShapeLayerFillColor[LHandle] := clLime;


      LShape10.Open(LNewPath + 'District Municipalities\DistrictMunicipalities.shp', LShape10.GlobalCallback);
      LHandle := FGISViewer.AddLayer(LShape10.DefaultInterface , True);
      FGISViewer.LayerVisible[LHandle] := False;
      FGISViewer.ShapeLayerFillTransparency[LHandle] := 0;

      LShape11.Open(LNewPath + 'Local Municipalities\LocalMunicipalities.shp', LShape10.GlobalCallback);
      LHandle := FGISViewer.AddLayer(LShape11.DefaultInterface , True);
      FGISViewer.LayerVisible[LHandle] := False;
      FGISViewer.ShapeLayerFillTransparency[LHandle] := 0;


      

      

      FGISViewer.ShowZoomBar := True;
      FGISViewer.CursorMode := cmSelection;
      FGISViewer.SendSelectBoxFinal := true;
      FGISViewer.OnSelectBoxFinal := DoOnSelectBoxFinal;
      FGISViewer.LayerVisible[1] := True;
      //FGISViewer.OnSelectionChanged := DoSelectionChanged;


      //FGISViewer.CursorMode := cmZoomIn;
      //FGISViewer.UseSeamlessPan := True;
      FGISViewer.ScalebarVisible := False;







      //FShapesTrv.FullExpand;
    finally
      FreeAndNil(LShape);
      FreeAndNil(LShape1);
      FreeAndNil(LShape2);
      FreeAndNil(LShape3);
      FreeAndNil(LShape4);
      FreeAndNil(LShape5);
      FreeAndNil(LShape6);
      FreeAndNil(LShape7);
      FreeAndNil(LShape8);
      FreeAndNil(LShape9);
      FreeAndNil(LShape10);
      FreeAndNil(LShape11);
      FreeAndNil(LExtends);
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;


function TfrmGaugeSelection.Finalise: boolean;
const OPNAME = 'TfrmGaugeSelection.Finalise';
begin
  Result := False;
  try
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TfrmGaugeSelection.LanguageHasChanged: boolean;
const OPNAME = 'TfrmGaugeSelection.LanguageHasChanged';
begin
  Result := False;
  try
   Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


procedure TfrmGaugeSelection.ClearAll1Click(Sender: TObject);
const OPNAME = 'TfrmGaugeSelection.ClearAll1Click';
begin
  try
    if Assigned(OnClearSelection) then
        OnClearSelection(Sender);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TfrmGaugeSelection.ClearDialog;
const OPNAME = 'TfrmGaugeSelection.ClearDialog';
begin
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TfrmGaugeSelection.PopulateDialog;
const OPNAME = 'TfrmGaugeSelection.PopulateDialog';
begin
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TfrmGaugeSelection.PopulateObject;
const OPNAME = 'TfrmGaugeSelection.PopulateObject';
begin
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TfrmGaugeSelection.rdgDailyDataSourceClick(Sender: TObject);
const OPNAME = 'TfrmGaugeSelection.rdgDailyDataSourceClick';
begin
  try
    TRWHModelData(FAppModules.Model.ModelData).DailyDataSource :=
    TDailyDataSource(rdgDailyDataSource.ItemIndex);
    FAppModules.ViewIni.WriteInteger('TRWHModelData','TDailyDataSource',rdgDailyDataSource.ItemIndex);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TfrmGaugeSelection.Resize;
const OPNAME = 'TfrmGaugeSelection.rgLayerSelectionClick';
begin
  try
    FToolPanel.Top         := pnlLayerSection.Height+3;
    FToolPanel.Height      := 30;
    FToolPanel.Align       := alTop;
    FToolPanel.Width       := FGISViewer.Width;

    FbtnResetMap.Left         := 10;
    FbtnResetMap.Top          := 5;
    FbtnResetMap.AllowAllUp   := True;
    FbtnResetMap.GroupIndex   := 1;
    FbtnResetMap.Glyph.LoadFromResourceName(HImagesInstance, UpperCase('ZoomFullExtender'));
    FbtnResetMap.Hint         := 'Reset Map';


    FbtnZoomIn.Left         := 70;
    FbtnZoomIn.Top          := 5;
    FbtnZoomIn.AllowAllUp   := True;
    FbtnZoomIn.GroupIndex   := 1;
    FbtnZoomIn.Glyph.LoadFromResourceName(HImagesInstance, UpperCase('ZoomIn2x'));

    FbtnZoomIn.Hint         := 'Zoom In';

    FbtnZoomOut.Left        := FbtnZoomIn.Left + FbtnZoomIn.Width+1;
    FbtnZoomOut.Top         := 5;
    FbtnZoomOut.AllowAllUp  := True;
    FbtnZoomOut.GroupIndex  := 1;
    FbtnZoomOut.Glyph.LoadFromResourceName(HImagesInstance, UpperCase('ZoomOut2x'));
    FbtnZoomOut.Hint         := 'Zoom Out';

    FbtnPan.Left            := FbtnZoomOut.Left + FbtnZoomOut.Width+1;
    FbtnPan.Top             := 5;
    FbtnPan.AllowAllUp      := True;
    FbtnPan.GroupIndex      := 1;
    FbtnPan.Glyph.LoadFromResourceName(HImagesInstance, UpperCase('PanMode'));
    FbtnPan.Hint         := 'Pan';

    FbtnSelect.Left         := FbtnPan.Left + FbtnPan.Width+1;
    FbtnSelect.Top          := 5;
    FbtnSelect.AllowAllUp   := True;
    FbtnSelect.GroupIndex   := 1;
    FbtnSelect.Glyph.LoadFromResourceName(HImagesInstance, UpperCase('SelectionTool'));
    FbtnSelect.Hint         := 'Selection Tool';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TfrmGaugeSelection.rgLayerSelectionClick(Sender: TObject);
const OPNAME = 'TfrmGaugeSelection.rgLayerSelectionClick';
var
  LIndex : integer;

begin
  try
    for LIndex := 2 to FGISViewer.NumLayers-1 do
      if FGISViewer.LayerVisible[LIndex] = True then
        FGISViewer.LayerVisible[LIndex] := False;

    FGISViewer.LayerVisible[1] := True;
    case rgLayerSelection.ItemIndex of
      0 :
      begin
        FGISViewer.LayerVisible[5] := True;
        FLayerHandle := 5;

      end;
      1 :
      begin
        FGISViewer.LayerVisible[2] := True;
        FLayerHandle := 2;
      end;
      2 :
      begin
        FGISViewer.LayerVisible[3] := True;
        FLayerHandle := 3;
      end;
      3 :
      begin
        FGISViewer.LayerVisible[1] := True;
        FLayerHandle := 1;
      end;
      4 :
      begin
        FGISViewer.LayerVisible[4] := True;
        FLayerHandle := 4;
      end;
      5 :
      begin
        FGISViewer.LayerVisible[7] := True;
        FLayerHandle := 7;
      end;
      6 :
      begin
        FGISViewer.LayerVisible[6] := True;
        FLayerHandle := 6;

      end;

    end;

    FGISViewer.SendSelectBoxFinal := true;
    //FGISViewer.SendMouseMove := True;
    FGISViewer.CursorMode := cmIdentify;
    FGISViewer.OnShapeIdentified := DoOnShapeIdentified;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TfrmGaugeSelection.RWHModelData: TRWHModelData;
const OPNAME = 'TfrmGaugeSelection.RWHModelData';
begin
  Result := nil;
  try
    Result :=  TRWHModelData(FAppModules.Model.ModelData);
  except on E: Exception do HandleError(E, OPNAME) end;
end;
 {
function TfrmGaugeSelection.Query_Province : boolean;
const OPNAME = 'TfrmGaugeSelection.Query_Province';
var
  LSelectionList : IStrings;
  LSelectedList : TStringList;
begin
  Result := False;
  try
    LSelectedList := TStringList.Create;
    try
      FFieldName :=  'NAME';
      FCurrFeature := 'provinces';
      LSelectedList.Add(FCurrFeature);
      LSelectedList.Add('');
      LSelectedList.Add('clLime');
      LSelectedList.Add('2');
      LSelectedList.Add('4');
      GetOleStrings(LSelectedList, LSelectionList);
      FGISViewer.Query_Setup(LSelectionList);
      Result := True;
    finally
      FreeAndNil(LSelectedList);
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TfrmGaugeSelection.Query_WMA : boolean;
const OPNAME = 'TfrmGaugeSelection.Query_WMA';
var
  LSelectionList : IStrings;
  LSelectedList : TStringList;
begin
  Result := False;
  try
    LSelectedList := TStringList.Create;
    try
      FFieldName :=  'NAME';
      FCurrFeature := 'wmareas';
      LSelectedList.Add(FCurrFeature);
      LSelectedList.Add('');
      LSelectedList.Add('clLime');
      LSelectedList.Add('2');
      LSelectedList.Add('4');
      GetOleStrings(LSelectedList, LSelectionList);
      FGISViewer.Query_Setup(LSelectionList);
      Result := True;
    finally
      FreeAndNil(LSelectedList);
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TfrmGaugeSelection.Query_District : boolean;
const OPNAME = 'TfrmGaugeSelection.Query_District';
var
  LSelectionList : IStrings;
  LSelectedList : TStringList;
begin
  Result := False;
  try
    LSelectedList := TStringList.Create;
    try
      FFieldName :=  'ET_ID';
      FCurrFeature := 'districtpol';
      LSelectedList.Add(FCurrFeature);
      LSelectedList.Add('');
      LSelectedList.Add('clLime');
      LSelectedList.Add('2');
      LSelectedList.Add('4');
      GetOleStrings(LSelectedList, LSelectionList);
      FGISViewer.Query_Setup(LSelectionList);
      Result := True;
    finally
      FreeAndNil(LSelectedList);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TfrmGaugeSelection.Query_RainStation : boolean;
const OPNAME = 'TfrmGaugeSelection.Query_RainStation';
var
  LSelectionList : IStrings;
  LSelectedList : TStringList;
begin
  Result := False;
  try
    LSelectedList := TStringList.Create;
    try
      FFieldName :=  'STATIONNUM';
      FCurrFeature := 'rainfall_stations';
      LSelectedList.Add(FCurrFeature);
      LSelectedList.Add('');
      LSelectedList.Add('clLime');
      LSelectedList.Add('2');
      LSelectedList.Add('4');
      GetOleStrings(LSelectedList, LSelectionList);
      FGISViewer.Query_ClearSQL;
      FGISViewer.Query_Setup(LSelectionList);
      Result := True;
    finally
      FreeAndNil(LSelectedList);
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

      function TfrmGaugeSelection.Query_Quan : boolean;
const OPNAME = 'TfrmGaugeSelection.Query_Quan';
var
  LSelectionList : IStrings;
  LSelectedList : TStringList;
begin
  Result := False;
  try
    LSelectedList := TStringList.Create;
    try
      FFieldName :=  'CATCH_ID';
      FCurrFeature := 'catchments';
      LSelectedList.Add(FCurrFeature);
      LSelectedList.Add('');
      LSelectedList.Add('clLime');
      LSelectedList.Add('2');
      LSelectedList.Add('4');
      GetOleStrings(LSelectedList, LSelectionList);
      FGISViewer.Query_Setup(LSelectionList);
      Result := True;
    finally
      FreeAndNil(LSelectedList);
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

   function TfrmGaugeSelection.Query_LocM : boolean;
const OPNAME = 'TfrmGaugeSelection.Query_LocM';
var
  LSelectionList : IStrings;
  LSelectedList : TStringList;
begin
  Result := False;
  try
    LSelectedList := TStringList.Create;
    try
      FFieldName :=  'CAT_B';
      FCurrFeature := 'LocalMunicipalities';
      LSelectedList.Add(FCurrFeature);
      LSelectedList.Add('');
      LSelectedList.Add('clLime');
      LSelectedList.Add('2');
      LSelectedList.Add('4');
      GetOleStrings(LSelectedList, LSelectionList);
      FGISViewer.Query_Setup(LSelectionList);
      Result := True;
    finally
      FreeAndNil(LSelectedList);
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

      function TfrmGaugeSelection.Query_DistrictM : boolean;
const OPNAME = 'TfrmGaugeSelection.Query_DistrictM';
var
  LSelectionList : IStrings;
  LSelectedList : TStringList;
begin
  Result := False;
  try
    LSelectedList := TStringList.Create;
    try
      FFieldName :=  'DISTRICT';
      FCurrFeature := 'DistrictMunicipalities';
      LSelectedList.Add(FCurrFeature);
      LSelectedList.Add('');
      LSelectedList.Add('clLime');
      LSelectedList.Add('2');
      LSelectedList.Add('4');
      GetOleStrings(LSelectedList, LSelectionList);
      FGISViewer.Query_Setup(LSelectionList);
      Result := True;
    finally
      FreeAndNil(LSelectedList);
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;
  }
procedure TfrmGaugeSelection.DeleteStation1Click(Sender: TObject);
const OPNAME = 'TfrmGaugeSelection.DeleteStation1Click';
begin
  try
    if Assigned(OnDeleteSelectedStation) then
        OnDeleteSelectedStation(Sender);
  except on E: Exception do HandleError(E, OPNAME) end;
end;
{
procedure TfrmGaugeSelection.DoGISSelectionChanged(Sender: TObject; aGisEvent : SYSINT);
const OPNAME = 'TfrmGaugeSelection.DoGISSelectionChanged';
var
  LIndex : integer;
  LField, LValue, LCurrFeature : string;
  LSelectionList : IStrings;
  LSelectedList : TStringList;
begin
  try
    if Assigned(FGISViewer) then
    begin
      LSelectedList := TStringList.Create;
      try
        case rgLayerSelection.ItemIndex of
          0: Query_Province;
          1: Query_WMA;
          2: Query_District;
          3: Query_RainStation;
          4: Query_Quan;
          5: Query_LocM;
          6: Query_DistrictM;

        end;
          LSelectionList := FGISViewer.Query_GetResults(FCurrFeature, FFieldName);
          SetOleStrings(LSelectedList,LSelectionList);
          if (LSelectedList.Count > 1) and (rgLayerSelection.ItemIndex <> 3)then
          begin
            LField :=  LSelectedList[1];
            LValue := LSelectedList[2];
            LCurrFeature := FCurrFeature;
            Query_RainStation;
            if rgLayerSelection.ItemIndex = 2 then
              LField := 'ID';
            LSelectionList :=  FGISViewer.Query_GetFeatureResults(LCurrFeature,LField ,LValue,true);
            SetOleStrings(LSelectedList,LSelectionList);
          end;
          FSelectedList.Clear;
          for LIndex := 0 to LSelectedList.Count -1  do
            FSelectedList.Add(LSelectedList[LIndex ]);
      finally
        FreeAndNil(LSelectedList);
      end;
      if Assigned(OnGISSelectionChanged) then
        OnGISSelectionChanged(Sender);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;
  }
procedure TfrmGaugeSelection.DoOnAfterShapeEdit(ASender: TObject;
  Operation: tkUndoOperation; LayerHandle, ShapeIndex: Integer);
const OPNAME = 'TRaingaugeGISPanel.DoOnSelectBoxFinal';
begin
  try

  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TfrmGaugeSelection.DoOnSelectBoxFinal(ASender: TObject; left, right,
  bottom, top: Integer);
const OPNAME = 'TRaingaugeGISPanel.DoOnSelectBoxFinal';
var
  LLayerHandle : integer;
  LShape : IShapefile;
  LResult,
  LResults : OleVariant;
  LExtends : TMWGExtents;
  LDim,
  LIndex : integer;
  LLeft, LRight, LBottom,
  LTop: double;
  LMsg : string;
  LContinue : boolean;
begin
  try

    case rgLayerSelection.ItemIndex of
      0:; //Query_Province;
      1:;
       //Query_WMA;

      2:; //Query_District;
      3: begin
          LLayerHandle := FGISViewer.LayerHandle[1];
          LShape := FGISViewer.Shapefile[LLayerHandle];
          if (LShape <> nil) then
          begin
            LLeft := 0;
            LRight := 0;
            LBottom := 0;
            LTop := 0;
            FGISViewer.PixelToProj(left, top,LLeft,LTop );
            FGISViewer.PixelToProj(right, bottom,LRight,LBottom );
            LExtends := TMWGExtents.Create(nil);
            LExtends.SetBounds(LLeft, LBottom, 0.0, LRight, LTop, 0.0);
            if (LShape.SelectShapes(LExtends.DefaultInterface,0.0, INTERSECTION,  LResults)) then
            begin
              FSelectedList.Clear;
              for LResult in GetOleVariantArrEnum(LResults) do
              begin
                LShape.set_ShapeSelected(LResult, True);
                LDim := LResult;
                LShape.SelectionColor := clLime;

                 for LIndex := 0 to LShape.NumFields-1 do
                 begin

                   if (LShape.Field[LIndex].Name = 'STATIONNUM') THEN
                     FSelectedList.Add(VarToStr(LShape.CellValue[LIndex, LDim]));

                 end;

               end;
              LContinue := True;
              if (FSelectedList.Count > 100) then
              begin
                LMsg := FAppModules.Language.GetString('Message.100Gauges') + #13#10 +
                        FAppModules.Language.GetString('Message.TakeLongTime') + #13#10 +
                        FAppModules.Language.GetString('Message.WantToContinue');
                if (MessageDlg(LMsg, mtWarning, [mbYes, mbNo], 0) = mrNo) then
                  LContinue := False;
              end;
              if (LContinue) then
              begin
               if Assigned(OnGISSelectionChanged) then
                   OnGISSelectionChanged(ASender);
              end;
             end;
             FGISViewer.Redraw;

          end;

        end;

      4: ;//Query_Quan;
      5: ;//Query_LocM;
      6: ;//Query_DistrictM;

    end;


  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TfrmGaugeSelection.DoOnShapeHighlighted(ASender: TObject; LayerHandle,
  ShapeIndex: Integer);
const OPNAME = 'TfrmGaugeSelection.DoOnShapeHighlighted';
var
   LShape : IShapefile;
begin
  try
    LShape := FGISViewer.Shapefile[LayerHandle];
    if(LShape <> nil) then
    begin
       FGISViewer.LayerVisible[LayerHandle] := True;

    end;

  except on E: Exception do HandleError ( E, OPNAME ) end;
end;



procedure TfrmGaugeSelection.DoOnShapeIdentified(ASender: TObject; LayerHandle: Integer; ShapeIndex: Integer;
                                                    pointX: Double; pointY: Double);
const OPNAME = 'TfrmGaugeSelection.DoOnShapeIdentified';
var
  LShape : IShapefile;
  LShp,
  LSelectedShape : IShape;
  LShapeRainstations : IShapefile;
  LResult,
  LResults : OleVariant;
  LMsg : string;
  LDim,
  LPIndex,
  LIndex : integer;
  LPt : TMWGPoint;
  LContinue : boolean;
begin
  try
    LContinue := False;
    LShapeRainstations := FGISViewer.Shapefile[1];
    if(LShapeRainstations <> nil) then
    begin
      LShape := FGISViewer.Shapefile[FLayerHandle];
      if LShape <> nil then
      begin
        for LIndex := 0 to LShape.NumShapes-1 do
        begin
          LSelectedShape := nil;
          LShp := LShape.Shape[LIndex].Clone;
          LPt := TMWGPoint.Create(nil);
          LPt.x := pointX;
          LPt.y := pointY;
          if LShp.PointInThisPoly(LPt.DefaultInterface) then
            LSelectedShape := LShp;
          if LSelectedShape <> nil then
          begin
            if LShapeRainstations.SelectShapes(LSelectedShape.Extents, 0.0, INCLUSION,  LResults) then
            begin
              for LResult in GetOleVariantArrEnum(LResults) do
              begin
                LShapeRainstations.set_ShapeSelected(LResult, True);
                LShapeRainstations.SelectionColor := clLime;
                LDim := LResult;
                for LPIndex := 0 to LShapeRainstations.NumFields-1 do
                begin
                  LContinue := True;
                  if (LShapeRainstations.Field[LPIndex].Name = 'STATIONNUM') then
                    FSelectedList.Add(VarToStr(LShapeRainstations.CellValue[LPIndex, LDim]));
                end;
              end;
            end;
          end;

          if (LContinue) then
            Break
        end;
        if (FSelectedList.Count > 100) then
        begin
          LMsg := FAppModules.Language.GetString('Message.100Gauges') + #13#10 +
                  FAppModules.Language.GetString('Message.TakeLongTime') + #13#10 +
                  FAppModules.Language.GetString('Message.WantToContinue');
          if (MessageDlg(LMsg, mtWarning, [mbYes, mbNo], 0) = mrNo) then
            LContinue := False;
        end;
        if (LContinue) then
        begin
          if Assigned(OnGISSelectionChanged) then
          OnGISSelectionChanged(ASender);
        end;
      end;
    end;
    FGISViewer.Redraw;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TfrmGaugeSelection.DoOnShapesIdentified(ASender: TObject;
  const selectedShapes: ISelectionList; ProjX, ProjY: Double);
const OPNAME = 'TfrmGaugeSelection.DoOnShapesIdentified';
var
  LIndex : integer;
begin
  try
    if selectedShapes <> nil then
    begin
      for LIndex := 0 to selectedShapes.Count-1 do
        //selectedShapes.AddShape();
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TfrmGaugeSelection.DoSelectionChanged(Sender: TObject;LayerHandle: Integer);
const OPNAME = 'TfrmGaugeSelection.DoSelectionChanged';
var
   LShape : IShapefile;
begin
  try
    LShape := FGISViewer.Shapefile[LayerHandle];
    if(LShape <> nil) then
    begin
       FGISViewer.LayerVisible[LayerHandle] := True;

    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;
procedure TfrmGaugeSelection.UpdateFromList;
const OPNAME = 'TfrmGaugeSelection.UpdateFromList';
var
  LSelectionList : IStrings;
begin
  try
    LockWindowUpdate(Self.Handle);
    if Assigned(FGISViewer) then
    begin
      try
       { if (FSelectedList.Count > 0) then
        begin
          GetOleStrings(FSelectedList,LSelectionList);
          FStationList.CommaText := FSelectedList.CommaText;
          if (LSelectionList.Count > 0) then
          begin
            FGISViewer.Query_SelectMapFeatures('rainfall_stations', 'STATIONNUM', $00404080, LSelectionList, True);
            FGISViewer.Query_SelectMapFeatures('rainfall_stations', 'STATIONNUM', clLime, LSelectionList, True);
          end;
        end;
        if (FSelectedList.Count = 0) then
        begin
          if FStationList.Count > 0 then
          begin
            GetOleStrings(FStationList,LSelectionList);
            FGISViewer.Query_SelectMapFeatures('rainfall_stations', 'STATIONNUM', $00404080, LSelectionList, True);
          end;
        end;}
      finally
        LSelectionList := nil;
      end;
    end;
    LockWindowUpdate(0);
  except on E: Exception do HandleError(E, OPNAME) end;
end;




end.
