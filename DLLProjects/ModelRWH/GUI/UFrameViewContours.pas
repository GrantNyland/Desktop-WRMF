
{******************************************************************************}
{*  UNIT      : Contains TfrmViewContours Class                             *}
{*  AUTHOR    : Sam Dhlamini                                                  *}
{*  DATE      : 14/03/2014                                                    *}
{*  COPYRIGHT : Copyright © 2015 DWAF                                         *}
{******************************************************************************}

unit UFrameViewContours;

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
  MapWinGIS_TLB,
//  MapWinGIS32_TLB,
  Vcl.AxCtrls,
  UColourButtons,
  UAbstractObject,
  URWHDataObject,
//  GisViewerX41Control_TLB,
  Vcl.Menus;

type

  TfrmViewContours = class(TFrame)

    procedure rgLayerSelectionClick(Sender: TObject);
    procedure ClearAll1Click(Sender: TObject);
   // procedure DeleteStation1Click(Sender: TObject);
  protected
    { Private declarations }
    FAppModules            : TAppModules;
    FEditContext           : TChangeContext;
    FSelectedStations      : TRainfallStationList;
    FPopulating            : boolean;
    //FGISViewer             : TGisViewerX;
    FGISViewer             : TMap;
    FGISViewerImage        : TImage;
    FGeographicalExtent    : TRect;

    FOnGISSelectionChanged : TNotifyEvent;
    FOnClearSelection      : TNotifyEvent;
    FOnDeleteSelectedStation : TNotifyEvent;
    FExternal              : Boolean;
    FFieldName,
    FCurrFeature           : string;
    //function GetGISViewer  : TGisViewerX;
    procedure ClearDialog;
    procedure PopulateObject;
    procedure PopulateDialog;
    procedure SetAppModules(AAppModules : TAppModules);
    function RWHModelData : TRWHModelData;
    procedure LoadMapWinGis;
  public
    { Public declarations }

    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    function Initialise : boolean;
    function Finalise   : boolean;

    function LanguageHasChanged : boolean;

    property AppModules : TAppModules read FAppModules write SetAppModules;
    property OnGISSelectionChanged : TNotifyEvent read FOnGISSelectionChanged  write FOnGISSelectionChanged;
    property OnClearSelection : TNotifyEvent   read FOnClearSelection write FOnClearSelection;
    property OnDeleteSelectedStation : TNotifyEvent   read FOnDeleteSelectedStation write FOnDeleteSelectedStation;


  end;

var
  frmViewContours : TfrmViewContours;

implementation

{$R *.dfm}

uses

  UConstants,
  UUtilities,
  UErrorHandlingOperations;

{ TfrmViewContours }

procedure TfrmViewContours.AfterConstruction;
const OPNAME = 'TfrmViewContours.AfterConstruction';
begin
  inherited;
  try
    FPopulating       := False;
    FGISViewer := TMap.Create(Self);
    FGISViewer.Parent := Self;
    FGISViewer.Align := alClient;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TfrmViewContours.BeforeDestruction;
const OPNAME = 'TfrmViewContours.AfterConstruction';
begin
  inherited;
  try

  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TfrmViewContours.SetAppModules(AAppModules: TAppModules);
const OPNAME = 'TfrmViewContours.SetAppModules';
begin
  try
    FAppModules := AAppModules;
    if(AAppModules <> nil) then

  except on E: Exception do HandleError(E, OPNAME) end;
end;

{
function TfrmViewContours.GetGISViewer : TGisViewerX;
const OPNAME = 'TfrmViewContours.GetGISViewer';
begin
  Result := nil;
  try
    if Assigned(FGISViewer) then
      Result := FGISViewer
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;
   }
function TfrmViewContours.Initialise: boolean;
const OPNAME = 'TfrmViewContours.Initialise';
begin
  Result := False;
  try
  {
    if not Assigned(FGISViewer) then
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
      LFileName := GISCoversDirectory + 'Contours.glf';
      UpdateGISShapeFilePath(LFileName);
      FGISViewer.ControlInterface.Layers_LoadLayout(LFileName,False);
      FGISViewer.ControlInterface.Control_RefreshAll;
      FGISViewer.Toolbar_Property(toolBuildQuery,True,True);
      FGISViewer.Toolbar_Property(toolSelectionMode,True,True);
      FGISViewer.Toolbar_Property(toolRemoveLayers,False,False);
      FGISViewer.Toolbar_Property(toolRemoveLayers,False,False);
      FGISViewer.Toolbar_Property(toolRemoveLayers,False,False);
 //     FGISViewer.OnGisEvent := DoGISSelectionChanged;
      FGISViewer.Toolbar_Mode(getSelectionMode);
      FCurrFeature := '';
      FFieldName := '';
     // Query_RainStation;
    end;
    }


    LoadMapWinGis;

    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TfrmViewContours.LoadMapWinGis;
const OPNAME = 'TfrmViewContours.LoadMapWinGis';
var
  LHandle: integer;
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
      FGISViewer.LayerVisible[LHandle] := False;

      LShape5.Open(LNewPath + 'avgrainfall\avg_rainfall.shp', LShape5.GlobalCallback);
      //LShape5.Open(LNewPath + '30mingrid\30mingrid.shp', LShape5.GlobalCallback);

      LHandle := FGISViewer.AddLayer(LShape5.DefaultInterface , True);
      FGISViewer.LayerVisible[LHandle] := True;
      FGISViewer.ShapeLayerFillTransparency[LHandle] := 0;

      LShape3.Open(LNewPath + 'All towns and cities (poly)\all towns and cities_poly.shp', LShape3.GlobalCallback);

      LHandle := FGISViewer.AddLayer(LShape3.DefaultInterface , True);
      FGISViewer.LayerVisible[LHandle] := True;
      FGISViewer.ShapeLayerFillColor[LHandle] := clMaroon;


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
      //FGISViewer.CursorMode := cmSelection;
      //FGISViewer.SendSelectBoxFinal := true;
      //FGISViewer.OnSelectBoxFinal := DoOnSelectBoxFinal;
      //FGISViewer.LayerVisible[1] := True;
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


function TfrmViewContours.Finalise: boolean;
const OPNAME = 'TfrmViewContours.Finalise';
begin
  Result := False;
  try
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TfrmViewContours.LanguageHasChanged: boolean;
const OPNAME = 'TfrmViewContours.LanguageHasChanged';
begin
  Result := False;
  try
   Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


procedure TfrmViewContours.ClearAll1Click(Sender: TObject);
const OPNAME = 'TfrmViewContours.ClearAll1Click';
begin
  try
    if Assigned(OnClearSelection) then
        OnClearSelection(Sender);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TfrmViewContours.ClearDialog;
const OPNAME = 'TfrmViewContours.ClearDialog';
begin
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TfrmViewContours.PopulateDialog;
const OPNAME = 'TfrmViewContours.PopulateDialog';
begin
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TfrmViewContours.PopulateObject;
const OPNAME = 'TfrmViewContours.PopulateObject';
begin
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TfrmViewContours.rgLayerSelectionClick(Sender: TObject);
const OPNAME = 'TfrmViewContours.rgLayerSelectionClick';
begin
  try

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TfrmViewContours.RWHModelData: TRWHModelData;
const OPNAME = 'TfrmViewContours.RWHModelData';
begin
  Result := nil;
  try
    Result :=  TRWHModelData(FAppModules.Model.ModelData);
  except on E: Exception do HandleError(E, OPNAME) end;
end;
 


end.
