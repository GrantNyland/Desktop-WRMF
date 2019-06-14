//
//
//  UNIT      : Contains the class TNetworkVisualiserGISPanel.
//  AUTHOR    : Grant Nyland
//  DATE      : 2002/04/05
//  COPYRIGHT : Copyright © 2002 DWAF
//
//                                               
unit URaingaugeGISPanel;

interface

uses
  Windows,
  Classes,
  Vcl.Controls,
  VCL.ExtCtrls, Vcl.Buttons,
  Vcl.ComCtrls,
  StdVCL,
  ActiveX,
  System.Variants,
  UAbstractObject,
  URaingaugeGISLegend,
  UAbstractComponent,
  UOleVariantEnum,
  MapWinGIS_TLB;
//  MapWinGIS32_TLB;
  //GisViewerX41Control_TLB;


type
  TRaingaugeGISPanel = class(TAbstractPanel)
  private
    FSelectedList          : TStringList;
    //FGISViewer             : TGisViewerX;
    FGISViewer             : TMap;
    FGISViewerImage        : TImage;
    //FGeographicalExtent    : TRect;
    FOnGISSelectionChanged : TNotifyEvent;
    FExternal              : Boolean;
    FStationList           : TStringList;
    FToolPanel             : TPanel;

    FShapesTrv             : TTreeView;
    FShapesImgLst          : TImageList;

    FMapTrvSplt            : TSplitter;

    FbtnResetMap           : TSpeedButton;
    FbtnZoomIn             : TSpeedButton;
    FbtnZoomOut            : TSpeedButton;
    FbtnPan                : TSpeedButton;
    FbtnSelect             : TSpeedButton;

    FbtnLegend             : TSpeedButton;
    FSelectedNode          : TTreeNode;
    FSelectedNodeList      : TStringList;
    FNewGISPath            : string;
    //function GetGISViewer : TGisViewerX;
    function GetGISViewer : TMap;
    procedure DoShapesTrvChange(Sender: TObject; Node: TTreeNode);
  protected
    //procedure UpdateGeographicalExtent;
   // procedure SetGeographicalExtent(const Value: TRect);
    function GetGISViewerState : boolean;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    procedure DobtnResetMapOnClick(Sender : TObject);
    procedure DoFbtnSelectOnClick(Sender : TObject);
    procedure DoFbtnPanOnClick(Sender : TObject);
    procedure DoFbtnZoomOutOnClick(Sender : TObject);
    procedure DoFbtnZoomInOnClick(Sender : TObject);
    procedure DobtnLegendOnClick(Sender : TObject);
    function PopulateMapTreeView(ANode : TTreeNode;AShapeNames,ASubShapeNames,ATopoShapeNames : TStrings) : boolean;
    //procedure DoShapesTrvChange(Sender : TObject; Node : TTreeNode);
    procedure DoShapesTrvMouseDown(Sender: TObject; Button: TMouseButton;
                                   Shift: TShiftState; X, Y: Integer);
    procedure LoadShape(ANode : TTreeNode);
    procedure  UnLoadShape(ANode : TTreeNode);
    procedure LoadMapWinGis;
    procedure ShowLegend;
  public
    //procedure DoGISSelectionChanged(Sender : TObject; aGisEvent : SYSINT);
    procedure AddToSelection(AValue : string);
    procedure RemoveFromSelection(AValue : string);
    procedure ClearSelection;
    procedure UpdateFromList;
    procedure Resize; override;
    //procedure Paint; override;

    function CanCopyToClipboard: Boolean; override;
    function CanPrint: Boolean; override;
    function CanExport: Boolean; override;
    procedure DoCopyToClipboard; override;
    procedure DoPrint; override;
    procedure DoExport(AFileName: string = ''); override;
    procedure DoOnSelectBoxFinal(ASender: TObject; left, right, bottom, top: Integer);
    property OnGISSelectionChanged : TNotifyEvent
                                            read FOnGISSelectionChanged  write FOnGISSelectionChanged;
    property GISViewerImage : TImage        read FGISViewerImage;
    property IsGISViewerVisible : boolean   read GetGISViewerState;
    //property GeographicalExtent: TRect      read FGeographicalExtent     write SetGeographicalExtent;
    property SelectedList : TStringList     read FSelectedList           write FSelectedList;
    //property GISViewer: TGisViewerX         read GetGISViewer;
    property GISViewer: TMap         read GetGISViewer;
    function Initialise: Boolean; override;
    //function QueryBuilder : boolean;


  end;

implementation

uses
  System.UITypes,
  VCL.AxCtrls,
  SysUtils,
  VCL.Graphics,
  Vcl.ImgList,
  VCL.Forms,
  VCL.Dialogs,
  VCL.Clipbrd,
  VCL.Printers,
  UUtilities,
  UErrorHandlingOperations;

const
  CExtentConversionFactor = 10000.0;

procedure TRaingaugeGISPanel.Resize;
const OPNAME = 'TRaingaugeGISPanel.Resize';
begin
  try
    inherited;
    if Assigned(FGISViewerImage) then
    begin
      FGISViewerImage.Width  := ClientWidth  - 8;
      FGISViewerImage.Height := ClientHeight - 8;
    end;

    FToolPanel.Height      := 30;
    FToolPanel.Align       := alTop;

    FShapesTrv.Width       := (ClientWidth div 5);
    FShapesTrv.Align       := alLeft;

    FMapTrvSplt.Align      := alLeft;

    FbtnResetMap.Left         := FShapesTrv.Width + 10;
    FbtnResetMap.Top          := 5;
    FbtnResetMap.AllowAllUp   := True;
    FbtnResetMap.GroupIndex   := 1;
    FbtnResetMap.Glyph.LoadFromResourceName(HImagesInstance, UpperCase('ZoomFullExtender'));
    FbtnResetMap.Hint         := 'Reset Map';


    FbtnZoomIn.Left         := FShapesTrv.Width + 70;
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

    FbtnLegend.Left         := FbtnSelect.Left + FbtnSelect.Width+1;
    FbtnLegend.Top          := 5;
    FbtnLegend.AllowAllUp   := True;
    FbtnLegend.GroupIndex   := 1;
    FbtnLegend.Glyph.LoadFromResourceName(HImagesInstance, UpperCase('UnCheckYellow'));
    FbtnLegend.Hint         := 'Show Legend';

  except on E: Exception do HandleError(E, OPNAME) end;
end;
(*
procedure TRaingaugeGISPanel.Paint;
const OPNAME = 'TRaingaugeGISPanel.Paint';
begin
  try
    inherited;
    if Assigned(FGISViewerImage) then
    begin
      Canvas.Pen.Width := 1;
      Canvas.Pen.Color := clBlack;
      Canvas.Polyline([
        Point(FGISViewerImage.Left - 1,                                   FGISViewerImage.Top - 1),
        Point(FGISViewerImage.Left - 1 + FGISViewerImage.ClientWidth + 1, FGISViewerImage.Top - 1),
        Point(FGISViewerImage.Left - 1 + FGISViewerImage.ClientWidth + 1, FGISViewerImage.Top - 1 + FGISViewerImage.ClientHeight + 1),
        Point(FGISViewerImage.Left - 1,                                   FGISViewerImage.Top - 1 + FGISViewerImage.ClientHeight + 1),
        Point(FGISViewerImage.Left - 1,                                   FGISViewerImage.Top - 1)]);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


procedure TRaingaugeGISPanel.ToggleView;
const OPNAME = 'TRaingaugeGISPanel.ToggleView';
begin
  try
    if Assigned(FGISViewer) and Assigned(FGISViewerImage) then
    begin
      if (FGISViewer.Visible) then
      begin
        UpdateGeographicalExtent;
        FGISViewer.Output_ExportMap(3, '', 1);
        FGISViewer.Visible := False;
        FGISViewerImage.Picture.Bitmap.LoadFromClipboardFormat(cf_BitMap, ClipBoard.GetAsHandle(cf_Bitmap), 0);
        FGISViewerImage.Visible := True;
      end else begin
        FGISViewer.Visible := True;
        FGISViewerImage.Visible := False;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRaingaugeGISPanel.UpdateGeographicalExtent;
const OPNAME = 'TRaingaugeGISPanel.UpdateGeographicalExtent';
begin
  try
    if Assigned(FGISViewer) then
    begin
      FGeographicalExtent.Left   := Round(FGISViewer.Extent_Left   * CExtentConversionFactor);
      FGeographicalExtent.Top    := Round(FGISViewer.Extent_Top    * CExtentConversionFactor);
      FGeographicalExtent.Right  := Round(FGISViewer.Extent_Right  * CExtentConversionFactor);
      FGeographicalExtent.Bottom := Round(FGISViewer.Extent_Bottom * CExtentConversionFactor);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRaingaugeGISPanel.SetGeographicalExtent(const Value: TRect);
const OPNAME = 'TRaingaugeGISPanel.SetGeographicalExtent';
begin
  try
    if Assigned(FGISViewer) then
    begin
      if (not FGISViewer.Visible) then
      begin
        raise Exception.Create('The geographical extent may not be changed while the GIS Viewer is invisible.');
      end else begin
        // Ensure the extent is valid eg Left > Right and Left < Right, etc VGN - PDNA DWAF 05/2002
        if (Value.Left < Value.Right) and (Value.Top > Value.Bottom) then
        begin
          FGISViewer.Extent_Set(
            Value.Left   / CExtentConversionFactor,
            Value.Top    / CExtentConversionFactor,
            Value.Right  / CExtentConversionFactor,
            Value.Bottom / CExtentConversionFactor
          );
          UpdateGeographicalExtent;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;
  *)
function TRaingaugeGISPanel.GetGISViewerState: boolean;
const OPNAME = 'TRaingaugeGISPanel.GetGISViewerState';
begin
  Result := False;
  try
    if Assigned(FGISViewer) then
      Result := FGISViewer.Visible;
  except on E: Exception do HandleError(E, OPNAME) end;
end;
  (*
procedure TRaingaugeGISPanel.DoGISSelectionChanged(Sender: TObject; aGisEvent : SYSINT);
const OPNAME = 'TRaingaugeGISPanel.DoGISSelectionChanged';
var
  LIndex    : integer;
  LContinue : boolean;
  LMsg      : string;
  LSelectionList : IStrings;
  LSelectedList : TStringList;
begin
  try
    if Assigned(FGISViewer) then
    begin
      lContinue := TRUE;
      LSelectedList := TStringList.Create;
      try
        if QueryBuilder then
          LSelectionList := FGISViewer.Query_GetResults('rainfall_stations', 'STATIONNUM');
        if LSelectionList.Count > 0 then
          FGISViewer.Query_SelectMapFeatures('rainfall_stations','STATIONNUM',clLime, LSelectionList, True );
        SetOleStrings(LSelectedList,LSelectionList);
        if ((not FExternal) and (LSelectedList.Count > 100 ) ) then
        begin
          lMsg := FAppModules.Language.GetString('Message.100Gauges') + #13#10 +
                  FAppModules.Language.GetString('Message.TakeLongTime') + #13#10 +
                  FAppModules.Language.GetString('Message.WantToContinue');
          if (MessageDlg(lMsg, mtWarning, [mbYes, mbNo], 0) = mrNo) then
            lContinue := FALSE;
        end;
        if (lContinue) then
        begin
          FSelectedList.Clear;
          for LIndex := 0 to LSelectedList.Count -1  do
            FSelectedList.Add(LSelectedList[LIndex ]);
        end;
        if Assigned(OnGISSelectionChanged) then
          OnGISSelectionChanged(Sender);
      finally
        FreeAndNil(LSelectedList);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;
   *)

procedure TRaingaugeGISPanel.AddToSelection(AValue: string);
const OPNAME = 'TRaingaugeGISPanel.AddToSelection';
begin
  try
    if Assigned(FSelectedList) then
      FSelectedList.Add(AValue);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRaingaugeGISPanel.ClearSelection;
const OPNAME = 'TRaingaugeGISPanel.ClearSelection';
var
  LShape : IShapefile;
begin
  try
    if Assigned(FGISViewer) then
    begin
      LShape := FGISViewer.Shapefile[4];
      if (LShape <> nil) then
        LShape.SelectNone;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRaingaugeGISPanel.RemoveFromSelection(AValue: string);
const OPNAME = 'TRaingaugeGISPanel.RemoveFromSelection';
begin
  try
    if Assigned(FSelectedList) then
      if FSelectedList.IndexOf(AValue)>= 0 then
        FSelectedList.Delete(FSelectedList.IndexOf(AValue));
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRaingaugeGISPanel.UnLoadShape(ANode: TTreeNode);
const OPNAME = 'TRaingaugeGISPanel.UnLoadShape';
var
  LIndex : integer;
begin
  try
    if ANode <> nil then
    begin
      FGISViewer.LayerVisible[Integer(ANode.Data)] := False;
      //ANode.Parent.StateIndex := 2;
      //ANode.Parent.ImageIndex := 2;
      //ANode.StateIndex := -1;
      ANode.SelectedIndex := 0;
      ANode.ImageIndex := 0;
      if Integer(ANode.Data) = 10 then
      begin
        LIndex := 10;
        FSelectedNode := nil;
        FSelectedNodeList.Delete(FSelectedNodeList.IndexOf(IntToStr(Integer(ANode.Data))));
        While LIndex <= 18 do
        begin
          FGISViewer.LayerVisible[Integer(LIndex)] := False;
          LIndex := LIndex + 1;
        end;
      end;
      if Integer(ANode.Data) = 18 then
      begin
        FSelectedNode := nil;
        FSelectedNodeList.Delete(FSelectedNodeList.IndexOf(IntToStr(Integer(ANode.Data))));
        LIndex := 18;
        While LIndex <= 26 do
        begin
          FGISViewer.LayerVisible[Integer(LIndex)] := False;
          LIndex := LIndex + 1;
        end;
      end;

    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRaingaugeGISPanel.UpdateFromList;
const OPNAME = 'TRaingaugeGISPanel.UpdateFromList';
var
  LSelectionList : IStrings;
  LShape : IShapefile;
  LResult,
  LResults : OleVariant;
  LIndex,
  LLayerHandle : integer;
  LErrorMsg,
  LQuery : WideString;
  LList : TStringList;
begin
  try
    LockWindowUpdate(Self.Handle);
    FExternal := TRUE;
    if Assigned(FGISViewer) then
    begin
      try
        if (FSelectedList.Count > 0) then
        begin
          GetOleStrings(FSelectedList,LSelectionList);
          FStationList.CommaText := FSelectedList.CommaText;
          if (LSelectionList.Count > 0) then
          begin
            LLayerHandle := FGISViewer.LayerHandle[4];
            LShape := FGISViewer.Shapefile[LLayerHandle];

            if LShape <> nil then
            begin
              LShape.SelectNone;
              LList := TStringList.Create;
              try
                for LIndex := 0 to FStationList.count-1 do
                begin
                  LList.Clear;
                  LList.Add(FStationList[LIndex]);
                  LQuery := '[STATIONNUM] = '+ LList.CommaText; //QuotedStr(FStationList.CommaText);
                  if (LShape.Table.Query(LQuery, LResults, LErrorMsg)) then
                  begin
                    for LResult in GetOleVariantArrEnum(LResults) do
                    begin
                      LShape.set_ShapeSelected(LResult, True);
                      LShape.SelectionColor := clLime;


                    end;
                  end;
                end;
              finally
                FreeAndNil(LList);
              end;
            end;
          end;
        end;
        if (FSelectedList.Count = 0) then
        begin
          if FStationList.Count > 0 then
          begin
            GetOleStrings(FStationList,LSelectionList);
            LLayerHandle := FGISViewer.LayerHandle[4];
            LShape := FGISViewer.Shapefile[LLayerHandle];
            if LShape <> nil then
            begin
              LShape.SelectNone;
              LList := TStringList.Create;
              try
                for LIndex := 0 to FStationList.count-1 do
                begin
                  LList.Clear;
                  LList.Add(FStationList[LIndex]);
                  LQuery := '[STATIONNUM] = '+ LList.CommaText; //QuotedStr(FStationList.CommaText);
                  if (LShape.Table.Query(LQuery, LResults, LErrorMsg)) then
                  begin
                    for LResult in GetOleVariantArrEnum(LResults) do
                    begin
                      LShape.set_ShapeSelected(LResult, True);
                      LShape.SelectionColor := clLime;


                    end;
                  end;
                end;
              finally
                FreeAndNil(LList);
              end;
            end;

          end;
        end;
      finally
        LSelectionList := nil;
      end;
    end;
    FExternal := FALSE;
    LockWindowUpdate(0);
    FGISViewer.Redraw;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRaingaugeGISPanel.CreateMemberObjects;
const OPNAME = 'TRaingaugeGISPanel.CreateMemberObjects';
var

  LAppDrive, LNewPath,
  LPath : string;
begin
  inherited CreateMemberObjects;
  try
    FExternal := FALSE;
    FSelectedList := TStringList.Create;
    FSelectedList.Duplicates := dupIgnore;
    FSelectedList.Sorted := true;
    FStationList  := TStringList.Create;
    FStationList.Duplicates := dupIgnore;
    FStationList.Sorted := true;
    FSelectedNodeList      := TStringList.Create;
    FToolPanel             := TPanel.Create(Self);
    FToolPanel.Parent      := Self;

    FShapesTrv             := TTreeView.Create(Self);
    FShapesTrv.Parent      := Self;

    FShapesTrv.OnChange    := DoShapesTrvChange;
    FShapesTrv.OnMouseDown := DoShapesTrvMouseDown;


    FShapesImgLst          := TImageList.Create(Self);
    FShapesImgLst.Height   := 20;
    FShapesImgLst.Width    := 20;


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


      FShapesImgLst.GetInstRes(HImagesInstance, rtBitmap, 'UNCHECKWHITE',     16, [], clWhite);
      FShapesImgLst.GetInstRes(HImagesInstance, rtBitmap, 'CHECKWHITE',       16, [], clWhite);
      FShapesImgLst.GetInstRes(HImagesInstance, rtBitmap, 'SAICON',           16, [], clWhite);

    //FShapesImgLst.Height   := 70;
    //FShapesImgLst.Width    := 30;
    //FShapesImgLst.GetInstRes(HImagesInstance, rtBitmap, 'ISOHYETS',           16, [], clWhite);
    {
    FShapesImgLst.GetInstRes(HImagesInstance, rtBitmap, 'UnCheckOrange',           16, [], clWhite);
    FShapesImgLst.GetInstRes(HImagesInstance, rtBitmap, 'AVGMAP300',           16, [], clWhite);
    FShapesImgLst.GetInstRes(HImagesInstance, rtBitmap, 'UnCheckBrown',           16, [], clWhite);
    FShapesImgLst.GetInstRes(HImagesInstance, rtBitmap, 'UnCheckYellow',           16, [], clWhite);
    FShapesImgLst.GetInstRes(HImagesInstance, rtBitmap, 'AVGMAP600',           16, [], clWhite);
    FShapesImgLst.GetInstRes(HImagesInstance, rtBitmap, 'AVGMAP700',           16, [], clWhite);
    FShapesImgLst.GetInstRes(HImagesInstance, rtBitmap, 'AVGMAP800',           16, [], clWhite);
    FShapesImgLst.GetInstRes(HImagesInstance, rtBitmap, 'AVGMAP1000',          16, [], clWhite);
    FShapesImgLst.GetInstRes(HImagesInstance, rtBitmap, 'AVGMAP1500',          16, [], clWhite);

    FShapesImgLst.GetInstRes(HImagesInstance, rtBitmap, 'ELEV_GEO200',           16, [], clWhite);
    FShapesImgLst.GetInstRes(HImagesInstance, rtBitmap, 'ELEV_GEO400',           16, [], clWhite);
    FShapesImgLst.GetInstRes(HImagesInstance, rtBitmap, 'ELEV_GEO800',           16, [], clWhite);
    FShapesImgLst.GetInstRes(HImagesInstance, rtBitmap, 'ELEV_GEO1200',           16, [], clWhite);
    FShapesImgLst.GetInstRes(HImagesInstance, rtBitmap, 'UnCheckOrange',           16, [], clWhite);
    FShapesImgLst.GetInstRes(HImagesInstance, rtBitmap, 'ELEV_GEO2000',           16, [], clWhite);
    FShapesImgLst.GetInstRes(HImagesInstance, rtBitmap, 'ELEV_GEO2400',           16, [], clWhite);
    FShapesImgLst.GetInstRes(HImagesInstance, rtBitmap, 'ELEV_GEO2800',          16, [], clWhite);
          }



    FMapTrvSplt            := TSplitter.Create(Self);
    FMapTrvSplt.Parent     := Self;

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

    FbtnLegend             := TSpeedButton.Create(self);
    FbtnLegend.Parent      := FToolPanel;
    FbtnLegend.OnClick     := DobtnLegendOnClick;


  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TRaingaugeGISPanel.DestroyMemberObjects;
const OPNAME = 'TRaingaugeGISPanel.DestroyMemberObjects';
begin
  try
    FSelectedList.Free;
    FGISViewerImage.Free;
    FGISViewer.Free;
    FStationList.Free;
    FGISViewer := nil;
    FreeAndNil(FSelectedNodeList);
    inherited DestroyMemberObjects;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TRaingaugeGISPanel.CanCopyToClipboard : Boolean;
const OPNAME = 'TRaingaugeGISPanel.CanCopyToClipboard';
begin
  Result := False;
  try
    Result := True;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TRaingaugeGISPanel.CanExport : Boolean;
const OPNAME = 'TRaingaugeGISPanel.CanExport';
begin
  Result := False;
  try
    Result := True;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TRaingaugeGISPanel.CanPrint : Boolean;
const OPNAME = 'TRaingaugeGISPanel.CanPrint';
begin
  Result := False;
  try
    Result := True;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TRaingaugeGISPanel.DoCopyToClipboard;
const OPNAME = 'TRaingaugeGISPanel.DoCopyToClipboard';
begin
  try

  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TRaingaugeGISPanel.DoExport(AFileName: string = '');
const OPNAME = 'TRaingaugeGISPanel.DoExport';
begin
  try
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TRaingaugeGISPanel.DoFbtnPanOnClick(Sender: TObject);
const OPNAME = 'TRaingaugeGISPanel.DoFbtnPanOnClick';
begin
  try
    //FGISViewer.InertiaOnPanning := t
    FGISViewer.CursorMode := cmPan;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TRaingaugeGISPanel.DobtnLegendOnClick(Sender: TObject);
const OPNAME = 'TRaingaugeGISPanel.DobtnLegendOnClick';
begin
  try
    //FGISViewer.CursorMode := cmPan;
    if not Assigned(frmGISLegend) then
      frmGISLegend := TfrmGISLegend.Create(Self);
    frmGISLegend.Position := poDesktopCenter;
    frmGISLegend.BorderStyle := bsDialog;
    if FSelectedNode = nil then
      if FSelectedNodeList.Count>0 then
        FSelectedNode := TTreeNode(FSelectedNodeList[0]);

    if (FSelectedNode <> nil)  then
    begin

      if (integer(FSelectedNode) = 10) or (StrToInt(FSelectedNodeList[0]) = 10) then
      begin
        frmGISLegend.imgLoader.Picture.LoadFromFile(FNewGISPath+'avgrainfall\Isohyets2.bmp');
        frmGISLegend.Caption := 'AVGMAP';
      end;
      if (integer(FSelectedNode) = 18) or (StrToInt(FSelectedNodeList[0]) = 18)  then
      begin
        frmGISLegend.Caption := 'ELEV_GEO_I';
        frmGISLegend.imgLoader.Picture.LoadFromFile(FNewGISPath+'Topography\Topology.bmp');
      end;
      frmGISLegend.Show;
    end
    else
      ShowMessage('There is no legend for the current visible layer(s)');

  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TRaingaugeGISPanel.DobtnResetMapOnClick(Sender : TObject);
const OPNAME = 'TRaingaugeGISPanel.DoFbtnSelectOnClick';
var
  x,y, span : double;
  LExtends : TMWGExtents;
begin
  try
    x := 25.0;
    y := 3;
    span := 40;
    LExtends := TMWGExtents.Create(nil);
    try
      LExtends.SetBounds(x-span,y-span,0.0,x+span,y+span,0.0);
      FGISViewer.Extents := LExtends.DefaultInterface;
    finally
      FreeAndNil(LExtends);
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TRaingaugeGISPanel.DoFbtnSelectOnClick(Sender: TObject);
const OPNAME = 'TRaingaugeGISPanel.DoFbtnSelectOnClick';
begin
  try
    FGISViewer.CursorMode := cmSelection;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TRaingaugeGISPanel.DoFbtnZoomInOnClick(Sender: TObject);
const OPNAME = 'TRaingaugeGISPanel.DoFbtnZoomInOnClick';
begin
  try
    FGISViewer.CursorMode := cmZoomIn;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;
procedure TRaingaugeGISPanel.DoFbtnZoomOutOnClick(Sender: TObject);
const OPNAME = 'TRaingaugeGISPanel.DoFbtnZoomOutOnClick';
begin
  try
    FGISViewer.CursorMode := cmZoomOut;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TRaingaugeGISPanel.DoPrint;
const OPNAME = 'TRaingaugeGISPanel.DoPrint';
begin
  try
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TRaingaugeGISPanel.DoShapesTrvChange(Sender: TObject;
  Node: TTreeNode);
const OPNAME = 'TRaingaugeGISPanel.DoShapesTrvChange';
begin
  try
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TRaingaugeGISPanel.DoShapesTrvMouseDown(Sender: TObject; Button: TMouseButton;
                                                              Shift: TShiftState; X, Y: Integer);

const OPNAME = 'TRaingaugeGISPanel.DoShapesTrvMouseDown';
var
  LNode : TTreeNode;
  LHit : THitTests;
begin
  try

    LNode := FShapesTrv.GetNodeAt(X,Y);
    if (LNode <> nil) then
    begin

      LHit := FShapesTrv.GetHitTestInfoAt(X,Y);
      if (htOnIcon in LHit) then
      begin
        case LNode.ImageIndex of
          0 : LoadShape(LNode);
          1 : UnLoadShape(LNode);
        end;
      end;
      FShapesTrv.Repaint;
    end;

  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

(*
function TRaingaugeGISPanel.GetGISViewer : TGisViewerX;//TGISView3;
const OPNAME = 'TRaingaugeGISPanel.GetGISViewer';
begin
  Result := nil;
  try
    if Assigned ( FGISViewer ) then
      Result := FGISViewer
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;
  *)

function TRaingaugeGISPanel.GetGISViewer : TMap;//TGISView3;
const OPNAME = 'TRaingaugeGISPanel.GetGISViewer';
begin
  Result := nil;
  try
    if Assigned(FGISViewer) then
      Result := FGISViewer
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;



function TRaingaugeGISPanel.Initialise : Boolean;
const OPNAME = 'TRaingaugeGISPanel.Initialise';
begin
  Result := inherited Initialise;
  try
    FGISViewer := TMap.Create(Self);
    FGISViewer.Parent := Self;
    FGISViewer.Align := alClient;

    FShapesTrv.Images      := FShapesImgLst;
    FShapesTrv.StateImages := FShapesImgLst;


    LoadMapWinGis;
    FSelectedNode := nil;
    Result := True;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TRaingaugeGISPanel.LoadMapWinGis;
const OPNAME = 'TRaingaugeGISPanel.LoadMapWinGis';
var
  LHandle, LLayerHandle,
  LFieldIndex : integer;
  LShape,
  LShape1,
  LShape2,
  LShape3,
  LShape5,
  LShape6,LShape7,LShape8,LShape9,
  LShape10, LShape11,
  LShape4 : TMWGShapefile;
  LNewLShape11,
  LNewLShape10 : IShapefile;
  LExtends : TMWGExtents;
  y, span,
  x : double;
  LPath,
  LNewPath,
  LAppDrive : string;
  LNode : TTreeNode;
  LSubShapeNames, LTopoShapeNames,
  LShapeNames : TStringList;
  LShapeIndex,
  LCount,
  LIndex : integer;
  LShp : Shape;
  LText : string;
  LShpCategory : IShapefileCategory;
  
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
      FShapesTrv.Items.Clear;
      FGISViewer.Clear;
      LLayerHandle := 0;
      LNode := FShapesTrv.Items.Add(nil, 'Main Layers');
      LNode.ImageIndex := 2;

      LShapeNames := TStringList.Create;
      LSubShapeNames := TStringList.Create;
      LTopoShapeNames := TStringList.Create;

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

      FNewGISPath := LNewPath;

      LShape.Open(LNewPath + 'International boundaries\africa.shp', LShape.GlobalCallback);

      LHandle := FGISViewer.AddLayer(LShape.DefaultInterface , True);
      FGISViewer.LayerVisible[LHandle] := True;
      FGISViewer.ShapeLayerFillColor[LHandle] := 8454143;
      x := 25.0;
      y := 3;
      span := 40;
      LExtends.SetBounds(x-span,y-span,0.0,x+span,y+span,0.0);
      FGISViewer.Extents := LExtends.DefaultInterface;

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

      LShape4.Open(LNewPath + 'Rainfall Stations\rainfall_stations.shp', LShape4.GlobalCallback);

      LHandle := FGISViewer.AddLayer(LShape4.DefaultInterface , True);

      FGISViewer.ShapeLayerFillColor[LHandle] := clMaroon;
      FGISViewer.ShapeLayerPointType[LHandle]  := ptTriangleUp;
      FGISViewer.LayerVisible[LHandle] := True;

      LShape5.Open(LNewPath + '30mingrid\30mingrid.shp', LShape5.GlobalCallback);

      LHandle := FGISViewer.AddLayer(LShape5.DefaultInterface , True);
      FGISViewer.LayerVisible[LHandle] := True;
      FGISViewer.ShapeLayerFillTransparency[LHandle] := 0;


      LShape6.Open(LNewPath + 'Water Management Areas\wmareas.shp', LShape6.GlobalCallback);

      LHandle := FGISViewer.AddLayer(LShape6.DefaultInterface , True);
      FGISViewer.LayerVisible[LHandle] := False;
      FGISViewer.ShapeLayerFillTransparency[LHandle] := 0;
      FGISViewer.ShapeLayerFillColor[LHandle] := 25186;


      LShape7.Open(LNewPath + 'Quatenary catchments\catchments.shp', LShape7.GlobalCallback);

      LHandle := FGISViewer.AddLayer(LShape7.DefaultInterface , True);
      FGISViewer.LayerVisible[LHandle] := False;
      FGISViewer.ShapeLayerFillTransparency[LHandle] := 0;
      FGISViewer.ShapeLayerFillColor[LHandle] := 45232;

      LShape8.Open(LNewPath + 'National Main and Arterial Roads\national roads.shp', LShape8.GlobalCallback);

      LHandle := FGISViewer.AddLayer(LShape8.DefaultInterface , True);
      FGISViewer.LayerVisible[LHandle] := False;
      FGISViewer.ShapeLayerFillColor[LHandle] := 4227327;

      LShape9.Open(LNewPath + 'Provinces\province.shp', LShape9.GlobalCallback);

      LHandle := FGISViewer.AddLayer(LShape9.DefaultInterface , True);
      FGISViewer.LayerVisible[LHandle] := False;
      FGISViewer.ShapeLayerFillTransparency[LHandle] := 0;
      FGISViewer.ShapeLayerFillColor[LHandle] := clLime;

      LShape10.Open(LNewPath + 'avgrainfall\avg_rainfall.shp', LShape10.GlobalCallback);
      LFieldIndex := LShape10.Table.FieldIndexByName['AVGMAP'];
      LShape10.Categories.Generate(LFieldIndex, ctUniqueValues, 0);


      for LIndex := 0 to LShape10.Categories.Count-1 do
      begin
        LNewLShape10 := LShape10.Clone;
        for LShapeIndex := 0 to LShape10.NumShapes-1 do
        begin
           if (LShape10.ShapeCategory[LShapeIndex] = LIndex) then
           begin
             LShp := LShape10.Shape[LShapeIndex].Clone;
             LCount := LNewLShape10.NumShapes;
             LNewLShape10.EditInsertShape(LShp, LCount);
           end;
        end;
        LShpCategory := LShape10.Categories.Item[LIndex];
        LText := Copy(LShpCategory.Name,Pos('=',LShpCategory.Name)+1,Length(LShpCategory.Name));   //VarToStr(LShape10.CellValue[LFieldIndex, LIndex]);
         case StrToInt(LText) of
           100:
           begin
             LNewLShape10.DefaultDrawingOptions.FillColor := 33023;
             LLayerHandle := FGISViewer.AddLayer(LNewLShape10 , True);
             FGISViewer.LayerName[LLayerHandle] := LText;
             FGISViewer.LayerVisible[LLayerHandle] := False;
             LSubShapeNames.AddObject(LText, TObject(LLayerHandle));
           end;
          300:
            begin
              LNewLShape10.DefaultDrawingOptions.FillColor := 3434343;
              LLayerHandle := FGISViewer.AddLayer(LNewLShape10, True);
              FGISViewer.LayerName[LLayerHandle] := LText;
              FGISViewer.LayerVisible[LLayerHandle] := False;
              LSubShapeNames.AddObject(LText, TObject(LLayerHandle));
            end;
          400:
            begin
              LNewLShape10.DefaultDrawingOptions.FillColor := 5460902;
              LLayerHandle := FGISViewer.AddLayer(LNewLShape10, True);
              FGISViewer.LayerName[LLayerHandle] := LText;
              FGISViewer.LayerVisible[LLayerHandle] := False;
              LSubShapeNames.AddObject(LText, TObject(LLayerHandle));
            end;
          500:
            begin
              LNewLShape10.DefaultDrawingOptions.FillColor := 8454143;
              LLayerHandle := FGISViewer.AddLayer(LNewLShape10, True);
              FGISViewer.LayerName[LLayerHandle] := LText;
              FGISViewer.LayerVisible[LLayerHandle] := False;
              LSubShapeNames.AddObject(LText, TObject(LLayerHandle));
            end;
          600:
            begin
              LNewLShape10.DefaultDrawingOptions.FillColor := clGreen;
              LLayerHandle := FGISViewer.AddLayer(LNewLShape10, True);
              FGISViewer.LayerName[LLayerHandle] := LText;
              FGISViewer.LayerVisible[LLayerHandle] := False;
              LSubShapeNames.AddObject(LText, TObject(LLayerHandle));
            end;

          700:
            begin
              LNewLShape10.DefaultDrawingOptions.FillColor := 10921472;
              LLayerHandle := FGISViewer.AddLayer(LNewLShape10, True);
              FGISViewer.LayerName[LLayerHandle] := LText;
              FGISViewer.LayerVisible[LLayerHandle] := False;
              LSubShapeNames.AddObject(LText, TObject(LLayerHandle));
            end;
          800:
            begin
              LNewLShape10.DefaultDrawingOptions.FillColor := 16744576;
              LLayerHandle := FGISViewer.AddLayer(LNewLShape10, True);
              FGISViewer.LayerName[LLayerHandle] := LText;
              FGISViewer.LayerVisible[LLayerHandle] := False;
              LSubShapeNames.AddObject(LText, TObject(LLayerHandle));
            end;
          1000:
            begin
              LNewLShape10.DefaultDrawingOptions.FillColor := clBlue;
              LLayerHandle := FGISViewer.AddLayer(LNewLShape10, True);
              FGISViewer.LayerName[LLayerHandle] := LText;
              FGISViewer.LayerVisible[LLayerHandle] := False;
              LSubShapeNames.AddObject(LText, TObject(LLayerHandle));
            end;
          1500:
            begin
              LNewLShape10.DefaultDrawingOptions.FillColor := clMaroon;
              LLayerHandle := FGISViewer.AddLayer(LNewLShape10, True);
              FGISViewer.LayerName[LLayerHandle] := LText;
              FGISViewer.LayerVisible[LLayerHandle] := False;
              LSubShapeNames.AddObject(LText, TObject(LLayerHandle));
            end;

          end;

       end;

      LShapeNames.AddObject('SAWS Blocks', TObject(5));
      LShapeNames.AddObject('rainfall_stations', TObject(4));
      LShapeNames.AddObject('Major Towns and Cities', TObject(3));
      LShapeNames.AddObject('Dams and Lakes', TObject(1));
      LShapeNames.AddObject('Primary Rivers', TObject(2));

      LShapeNames.AddObject('Water Management Areas', TObject(6));
      LShapeNames.AddObject('Quatenary catchments', TObject(7));
      LShapeNames.AddObject('National Roads', TObject(8));
      LShapeNames.AddObject('Provinces', TObject(9));
      LShapeNames.AddObject('Isohyets', TObject(10));
      LShapeNames.AddObject('Topography', TObject(LLayerHandle));

      LShape11.Open(LNewPath + 'Topography\topo.shp', LShape11.GlobalCallback);
      LFieldIndex := LShape11.Table.FieldIndexByName['ELEV_GEO_I'];
      LShape11.Categories.Generate(LFieldIndex, ctUniqueValues, 0);
     
      for LIndex := 0 to LShape11.Categories.Count-1 do
      begin
        LNewLShape11 := LShape11.Clone;
        for LShapeIndex := 0 to LShape11.NumShapes-1 do
        begin
          if (LShape11.ShapeCategory[LShapeIndex] = LIndex) then
          begin
            LShp := LShape11.Shape[LShapeIndex].Clone;
            LCount := LNewLShape11.NumShapes;
            LNewLShape11.EditInsertShape(LShp, LCount);
          end;
        end;
        LShpCategory := LShape11.Categories.Item[LIndex];
        LText := Copy(LShpCategory.Name,Pos('=',LShpCategory.Name)+1,Length(LShpCategory.Name));
        case StrToInt(LText) of
        200:
         begin
           LNewLShape11.DefaultDrawingOptions.FillColor := clAqua;
           LLayerHandle := FGISViewer.AddLayer(LNewLShape11 , True);
           FGISViewer.LayerName[LLayerHandle] := LText;
           FGISViewer.LayerVisible[LLayerHandle] := False;
           LTopoShapeNames.AddObject(LText, TObject(LLayerHandle));
         end;
        400:
         begin
           LNewLShape11.DefaultDrawingOptions.FillColor := 4227072;
           LLayerHandle := FGISViewer.AddLayer(LNewLShape11 , True);
           FGISViewer.LayerName[LLayerHandle] := LText;
           FGISViewer.LayerVisible[LLayerHandle] := False;
           LTopoShapeNames.AddObject(LText, TObject(LLayerHandle));
         end;
        800:
          begin
            LNewLShape11.DefaultDrawingOptions.FillColor := 44544;
            LLayerHandle := FGISViewer.AddLayer(LNewLShape11, True);
            FGISViewer.LayerName[LLayerHandle] := LText;
            FGISViewer.LayerVisible[LLayerHandle] := False;
            LTopoShapeNames.AddObject(LText, TObject(LLayerHandle));
          end;
        1200:
          begin
            LNewLShape11.DefaultDrawingOptions.FillColor := 16384;
            LLayerHandle := FGISViewer.AddLayer(LNewLShape11, True);
            FGISViewer.LayerName[LLayerHandle] := LText;
            FGISViewer.LayerVisible[LLayerHandle] := False;
            LTopoShapeNames.AddObject(LText, TObject(LLayerHandle));
          end;
        1600:
          begin
            LNewLShape11.DefaultDrawingOptions.FillColor := 4227327;
            LLayerHandle := FGISViewer.AddLayer(LNewLShape11, True);
            FGISViewer.LayerName[LLayerHandle] := LText;
            FGISViewer.LayerVisible[LLayerHandle] := False;
            LTopoShapeNames.AddObject(LText, TObject(LLayerHandle));
          end;
        2000:
          begin
            LNewLShape11.DefaultDrawingOptions.FillColor := clRed;
            LLayerHandle := FGISViewer.AddLayer(LNewLShape11, True);
            FGISViewer.LayerName[LLayerHandle] := LText;
            FGISViewer.LayerVisible[LLayerHandle] := False;
            LTopoShapeNames.AddObject(LText, TObject(LLayerHandle));
          end;

        2400:
          begin
            LNewLShape11.DefaultDrawingOptions.FillColor := 64;
            LLayerHandle := FGISViewer.AddLayer(LNewLShape11, True);
            FGISViewer.LayerName[LLayerHandle] := LText;
            FGISViewer.LayerVisible[LLayerHandle] := False;
            LTopoShapeNames.AddObject(LText, TObject(LLayerHandle));
          end;
        2800:
          begin
            LNewLShape11.DefaultDrawingOptions.FillColor := clWhite;
            LLayerHandle := FGISViewer.AddLayer(LNewLShape11, True);
            FGISViewer.LayerName[LLayerHandle] := LText;
            FGISViewer.LayerVisible[LLayerHandle] := False;
            LTopoShapeNames.AddObject(LText, TObject(LLayerHandle));
          end;

        end;
      end;
      PopulateMapTreeView(LNode,LShapeNames, LSubShapeNames,LTopoShapeNames );
      FGISViewer.ShowZoomBar := True;
      FGISViewer.ScalebarVisible := False;
      FGISViewer.SendSelectBoxFinal := true;
      FGISViewer.OnSelectBoxFinal := DoOnSelectBoxFinal;
      FGISViewer.LayerVisible[4] := True;
      FShapesTrv.FullExpand;
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
      FreeAndNil(LShapeNames);
      FreeAndNil(LSubShapeNames);
      FreeAndNil(LTopoShapeNames);
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;


procedure TRaingaugeGISPanel.ShowLegend;
const OPNAME = 'TRaingaugeGISPanel.LoadShape';
begin
  try


  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TRaingaugeGISPanel.LoadShape(ANode: TTreeNode);
const OPNAME = 'TRaingaugeGISPanel.LoadShape';
var
  LIndex : integer;
begin
  try
    if ANode <> nil then
    begin
      FGISViewer.LayerVisible[Integer(ANode.Data)] := True;
      //ANode.Parent.StateIndex := 2;
      //ANode.Parent.SelectedIndex := 2;
//      ANode.Parent.ImageIndex := 2;
      //ANode.StateIndex := -1;
      ANode.SelectedIndex := 1;
      ANode.ImageIndex := 1;
      if Integer(ANode.Data) = 10 then
      begin
        LIndex := 10;
        FSelectedNode := ANode.Data;
        FSelectedNodeList.AddObject(IntToStr(Integer(ANode.Data)),TObject(ANode.Data));
        While LIndex <= 18 do
        begin
          FGISViewer.LayerVisible[Integer(LIndex)] := True;
          LIndex := LIndex + 1;
        end;
      end;
      if Integer(ANode.Data) = 18 then
      begin
        LIndex := 18;
        FSelectedNode := ANode.Data;
        FSelectedNodeList.AddObject(IntToStr(Integer(ANode.Data)),TObject(ANode.Data));
        While LIndex <= 26 do
        begin
          FGISViewer.LayerVisible[Integer(LIndex)] := True;
          LIndex := LIndex + 1;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRaingaugeGISPanel.PopulateMapTreeView(ANode: TTreeNode; AShapeNames,ASubShapeNames,ATopoShapeNames: TStrings): boolean;
const OPNAME = 'TRaingaugeGISPanel.PopulateMapTreeView';
var
  //LSubNode,
  LNode : TTreeNode;
  //LCount,
  LIndex : integer;
begin
  Result := False;
  try
    if (ANode <> nil) and (AShapeNames <> nil) then
    begin

      for LIndex := 0 to AShapeNames.Count-1 do
      begin
        LNode := FShapesTrv.Items.AddChildObject(ANode, AShapeNames[LIndex], AShapeNames.Objects[LIndex]);
        LNode.ImageIndex := 0;
        if LIndex < 5 then
          LNode.ImageIndex := 1;

        //if AShapeNames[LIndex] = 'Isohyets' then
       // begin

          //LSubNode := FShapesTrv.Items.AddChild(LNode,'AVGMAP');
          //LSubNode.ImageIndex := -1;
          //for LCount := 0 to ASubShapeNames.Count-1 do
         // begin
            //LNode := FShapesTrv.Items.AddChildObject(LSubNode, ASubShapeNames[LCount], ASubShapeNames.Objects[LCount]);
            //LNode.ImageIndex := LCount+3;

         // end;
        //end;

        //if AShapeNames[LIndex] = 'Topography' then
        //begin

          //LSubNode := FShapesTrv.Items.AddChild(LNode,'ELEV_GEO_I');
          //LSubNode.ImageIndex := -1;
          //for LCount := 0 to ATopoShapeNames.Count-1 do
          //begin
            //LNode := FShapesTrv.Items.AddChildObject(LSubNode, ATopoShapeNames[LCount], ATopoShapeNames.Objects[LCount]);
            //LNode.ImageIndex := LCount+12;
         // end;
        //end;

      end;


      Result := True;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TRaingaugeGISPanel.DoOnSelectBoxFinal(ASender: TObject; left, right, bottom,
  top: Integer);
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
  // Tatazi 0616949760.... Boy  : 0834937396
  try
    LLayerHandle := FGISViewer.LayerHandle[4];
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

      //LShape.SelectNone;
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
        if ((not FExternal) and (FSelectedList.Count > 100 ) ) then
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

  except on E: Exception do HandleError ( E, OPNAME ) end;
end;


{

function TRaingaugeGISPanel.QueryBuilder : boolean;
const OPNAME = 'TRaingaugeGISPanel.QueryBuilder';
var
  LSelectionList : IStrings;
  LSelectedList : TStringList;
begin
  Result := False;
  try
    LSelectedList := TStringList.Create;
    try
      LSelectedList.Add('rainfall_stations');
      LSelectedList.Add('');
      LSelectedList.Add('clLime');
      LSelectedList.Add('2');
      LSelectedList.Add('0');
      GetOleStrings(LSelectedList, LSelectionList);
      FGISViewer.Query_Setup(LSelectionList);
      Result := True;
    finally
      FreeAndNil(LSelectedList);
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;
 }

end.
