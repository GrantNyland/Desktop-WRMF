//
//
//  UNIT      : Contains the class TGISStudyAreaSelectorPanel.
//  AUTHOR    : Grant Nyland
//  DATE      : 2002/04/05
//  COPYRIGHT : Copyright © 2002 DWAF
//
//
unit UGISStudyAreaSelectorPanel;

interface

uses
  Classes,
  Vcl.StdCtrls,
  Vcl.ComCtrls,
  Vcl.ExtCtrls,
  StdVCL,
  UHelpContexts,
  UAbstractObject,
  UAbstractModelObjects,
  UOleVariantEnum,
  //GisViewerX41Control_TLB,
  MapWinGIS_TLB,
//  MapWinGIS32_TLB,
  //ArcGISViewer104_TLB,
  //UArcGISViewer104Control,
  UAbstractComponent;

const
  C_IdLastModel = 'LastModel';
  C_IdLastDate  = 'LastDate';
  C_StudyShapeFile = 'StudyPath';
  C_SubAreaShapeFile = 'SubAreaPath';

type
  TStudySelectionAction = (ssaNoStudy, ssaShowStudyDetails, ssaDoStudySelection);
  TStudyAreaSelectionEvent = procedure (AModelName, ASubAreaName: string; ADateLaterThan: TDateTime; AActionRequired: TStudySelectionAction) of object;
  TGISStudyAreaSelectorPanel = class(TAbstractPanel)
  protected
    FCriteriaPanel: TPanel;
    FModelLabel, FDateLabel: TLabel;
    FModelCombo: TComboBox;
    FDatePicker: TDateTimePicker;
    //FGISViewer: //TArcGISViewer104Control;   //TGisViewerX;

    FGISViewer:   TMap;
    FOnStudyAreaSelectionEvent: TStudyAreaSelectionEvent;
    FModelAutoLoaded,
    FBlockEvents: boolean;
    FPreviousLayer: string;
    FModel,
    FSubArea : string;
    FPreviousLayerLevel : TModelActionLevel;
    procedure CreateMemberObjects; override;
    function GetLastDate: TDateTime;
    procedure DoModelComboChange(ASender: TObject);
    procedure DoDatePickerChange(ASender: TObject);
    procedure DoStudyAreaSelectionEvent(AModelName, ASubAreaName: string; ADateLaterThan: TDateTime; AActionRequired: TStudySelectionAction);
    procedure AssignHelpContext; override;
    function GetGISViewer : TMap;

    //function GetGISViewer : TArcGISViewer104Control; // TGisViewerX;
  public
    function Initialise: Boolean; override;
    procedure Resize; override;
    procedure SetSelection ( AStudy, ASubArea, AStudyShapefile, aSubAreaShapeFile : string; aActionLevel : TModelActionLevel );overload;
    function LanguageHasChanged: boolean; override;
    property ModelCombo: TComboBox read FModelCombo;
    property OnStudyAreaSelectionEvent: TStudyAreaSelectionEvent
      read FOnStudyAreaSelectionEvent write FOnStudyAreaSelectionEvent;
    Property PreviousLayer : string read FPreviousLayer write FPreviousLayer;
    Property PreviousLayerLevel : TModelActionLevel read FPreviousLayerLevel write FPreviousLayerLevel;
    property BlockEvents: boolean read FBlockEvents write FBlockEvents;
    property ModelAutoLoaded : boolean read FModelAutoLoaded write FModelAutoLoaded;
    property GISViewer : TMap {TArcGISViewer104Control TGisViewerX} read GetGISViewer;


  end;

implementation

uses
  SysUtils,
  Vcl.Controls,
  Vcl.Forms,
  UUtilities,
  Vcl.AXCtrls,
  ActiveX,
  windows,
  Vcl.Graphics,
  UErrorHandlingOperations;

const
  C_ControlGap    = 4;
  C_LabelOffset   = 3;
  C_ControlHeight = 21;

procedure TGISStudyAreaSelectorPanel.CreateMemberObjects;
const OPNAME = 'TGISStudyAreaSelectorPanel.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;

    // Set default values.
    FOnStudyAreaSelectionEvent := nil;
    FBlockEvents := False;

    FModelAutoLoaded := (FAppModules.Model <> nil);

    // Create the criteria panel.
    FCriteriaPanel := TPanel.Create(self);
    FCriteriaPanel.Parent := self;
    FCriteriaPanel.Align  := alTop;
    FCriteriaPanel.Height := C_ControlHeight + C_ControlGap * 2 - 2;

    // Create
    FModelLabel := TLabel.Create(self);
    FModelLabel.Parent    := FCriteriaPanel;
    FModelLabel.AutoSize  := False;
    FModelLabel.Alignment := taCenter;
    FModelLabel.Top       := C_ControlGap + C_LabelOffset;
    FModelLabel.Height    := C_ControlHeight - C_LabelOffset - 2;
    FDateLabel  := TLabel.Create(self);
    FDateLabel.AutoSize  := False;
    FDateLabel.Alignment := taCenter;
    FDateLabel.Parent    := FCriteriaPanel;
    FDateLabel.Top       := C_ControlGap + C_LabelOffset;
    FDateLabel.Height    := C_ControlHeight - C_LabelOffset - 2;

    // Create the models combo box.
    FModelCombo := TComboBox.Create(self);
    FModelCombo.Parent   := self;
    FModelCombo.Style    := csDropDownList;
    FModelCombo.Top      := C_ControlGap;
    FModelCombo.Height   := C_ControlHeight;
    FModelCombo.OnChange := DoModelComboChange;

    // Create the date picker.
    FDatePicker := TDateTimePicker.Create(self);
    FDatePicker.Parent   := self;
    FDatePicker.Top      := C_ControlGap;
    FDatePicker.Height   := C_ControlHeight;
    FDatePicker.Date     := GetLastDate;
    FDatePicker.OnChange := DoDatePickerChange;


    FGISViewer := TMap.Create(Self);
    FGISViewer.Parent := Self;
    FGISViewer.Align := alClient;
    // Last of all call the assigners.
    AssignHelpContext;
    LanguageHasChanged;

  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TGISStudyAreaSelectorPanel.Initialise: Boolean;
const OPNAME = 'TGISStudyAreaSelectorPanel.Initialise';
var

  LHandle : integer;
  LShape,
  LShape1,
  LShape2,
  LShape3 : TMWGShapefile;
  LExtends : TMWGExtents;
  y, span,
  x : double;
  LPath,
  LNewPath,
  LAppDrive : string;
begin
  Result := inherited Initialise;
  try
    // Set default values.
    FOnStudyAreaSelectionEvent := nil;
    FBlockEvents := False;



    if Assigned(FGISViewer) then
    begin
      LShape := TMWGShapefile.Create(nil);
      LShape1 := TMWGShapefile.Create(nil);
      LShape2 := TMWGShapefile.Create(nil);
      LShape3 := TMWGShapefile.Create(nil);
      //LShape4 := TMWGShapefile.Create(nil);
      LExtends := TMWGExtents.Create(nil);
      try


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
        FGISViewer.ShapeLayerFillColor[LHandle] := 8454143;
        x := 25.0;
        y := -30.0;
        span := 10;
        LExtends.SetBounds(x-span,y-span,0.0,x+span,y+span,0.0);
        FGISViewer.Extents := LExtends.DefaultInterface;

        LShape1.Open(LNewPath + 'Dams and lakes\dams and lakes.shp', LShape1.GlobalCallback);

        LHandle := FGISViewer.AddLayer(LShape1.DefaultInterface , True);
        FGISViewer.LayerVisible[LHandle] := True;
        FGISViewer.ShapeLayerFillColor[LHandle] := clBlue;

        LShape2.Open(LNewPath + 'Primary rivers\prim_riv.shp', LShape2.GlobalCallback);

        LHandle := FGISViewer.AddLayer(LShape2.DefaultInterface , True);
        FGISViewer.LayerVisible[LHandle] := True;
        FGISViewer.ShapeLayerLineColor[LHandle] := clBlue;

        LShape3.Open(LNewPath + 'All towns and cities (poly)\all towns and cities_poly.shp', LShape3.GlobalCallback);

        LHandle := FGISViewer.AddLayer(LShape3.DefaultInterface , True);
        FGISViewer.LayerVisible[LHandle] := True;
        FGISViewer.ShapeLayerFillColor[LHandle] := clMaroon;

        FGISViewer.CursorMode := cmSelection;

      finally
        FreeAndNil(LShape);
        FreeAndNil(LShape1);
        FreeAndNil(LShape2);
        FreeAndNil(LShape3);
        //FreeAndNil(LShape4);
        FreeAndNil(LExtends);
      end;
    end;



        (*FGISViewer := TGisViewerX.Create(Self);
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
        // Ignore the error as it just means that the GIS Viewer is not installed.
      end;
      if Assigned(FGISViewer) and (FGISViewer.Parent <> nil)then
      begin

          // Set the ancestor properties.
        FGISViewer.Caption := '';

        // Set the ocx properties.
        FGISViewer.Align := alClient;
        //FGISViewer.AxBorderStyle := 0;

      // Show no GIS Viewer.
      end
      else
        Caption := FAppModules.Language.GetString('StudySelection.Caption1');

    end;

    if Assigned(FGISViewer) and (FGISViewer.Parent <> nil) then
    begin
      FGISViewer.Display_DataTab    := False;
      FGISViewer.Display_ToolBar    := False;
      FGISViewer.Display_Legend     := False;
      FGISViewer.Display_Interface  := False;
      FGISViewer.Display_ScrollBars := False;
      FGISViewer.Display_StatusBar  := False;

      //FGISViewer.Display_MsgShowAll(false);
      //FGISViewer.Display_MsgAssumeTrue(true);

      // Load the view settings for the study selection mode.
      FGISViewer.Chart_ClearAll;

      FGISViewer.Map_SetFilePathCovers(GISCoversDirectory);
      // The True parameter indicated that the local file path
      // should be used and not the path contained in the glf
      LFileName := GISCoversDirectory + 'StudySelectionView.glf';
      UpdateGISShapeFilePath(LFileName);
      FGISViewer.ControlInterface.Layers_LoadLayout(LFileName,False);
      FGISViewer.ControlInterface.Control_RefreshAll;
      FGISViewer.Toolbar_Mode(getSelectionMode);
    end;
    *)

    Result := True;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;




procedure TGISStudyAreaSelectorPanel.AssignHelpContext;
const OPNAME = 'TGISStudyAreaSelectorPanel.AssignHelpContext';
begin
  try
   SetControlHelpContext(Self,        HC_WaterResourcesYieldModel);
   SetControlHelpContext(FModelCombo, HC_WaterResourcesYieldModel);
   SetControlHelpContext(FDatePicker, HC_WaterResourcesYieldModel);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TGISStudyAreaSelectorPanel.LanguageHasChanged: boolean;
const OPNAME = 'TGISStudyAreaSelectorPanel.LanguageHasChanged';
begin
  Result := False;
  try
    FModelLabel.Caption := FAppModules.Language.GetString(ClassName + '.ModelLabel') + ' :';
    FDateLabel.Caption  := FAppModules.Language.GetString(ClassName + '.DateLabel')  + ' :';
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TGISStudyAreaSelectorPanel.DoStudyAreaSelectionEvent(
  AModelName, ASubAreaName: string; ADateLaterThan: TDateTime; AActionRequired: TStudySelectionAction);
const OPNAME = 'TGISStudyAreaSelectorPanel.DoStudyAreaSelectionEvent';
begin
  try
    if (not FBlockEvents) then
      if Assigned(FOnStudyAreaSelectionEvent) then
        FOnStudyAreaSelectionEvent(AModelName, ASubAreaName, ADateLaterThan, AActionRequired);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TGISStudyAreaSelectorPanel.GetLastDate: TDateTime;
const OPNAME = 'TGISStudyAreaSelectorPanel.GetLastDate';
var LDateStr: string;
begin
  Result := EncodeDate(1950, 1, 1);
  try
    if Assigned(FAppModules) and Assigned(FAppModules.ViewIni()) then
    begin
      LDateStr := FAppModules.ViewIni.ReadString(ClassName, C_IdLastDate, '');
      if (Length(LDateStr) >= 10) then
      begin
        try
          Result := EncodeDate(StrToInt(Copy(LDateStr, 1, 4)),
                               StrToInt(Copy(LDateStr, 6, 2)),
                               StrToInt(Copy(LDateStr, 9, 2)));
        except end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TGISStudyAreaSelectorPanel.Resize;
const OPNAME = 'TGISStudyAreaSelectorPanel.Resize';
var LWidth: integer;
begin
  try
    LWidth := (ClientWidth - C_ControlGap) div 4 - C_ControlGap;
    FModelLabel.Left  := C_ControlGap + (LWidth + C_ControlGap) * 0;
    FModelCombo.Left  := C_ControlGap + (LWidth + C_ControlGap) * 1;
    FDateLabel.Left   := C_ControlGap + (LWidth + C_ControlGap) * 2;
    FDatePicker.Left  := C_ControlGap + (LWidth + C_ControlGap) * 3;
    FModelLabel.Width := LWidth;
    FModelCombo.Width := LWidth;
    FDateLabel.Width  := LWidth;
    FDatePicker.Width := LWidth;
    inherited;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TGISStudyAreaSelectorPanel.DoModelComboChange(ASender: TObject);
const OPNAME = 'TGISStudyAreaSelectorPanel.DoModelComboChange';
begin
  try
    FAppModules.ViewIni.WriteString(ClassName, C_IdLastModel, FModelCombo.Text);
    //DoGISSelectionChanged(nil);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TGISStudyAreaSelectorPanel.DoDatePickerChange(ASender: TObject);
const OPNAME = 'TGISStudyAreaSelectorPanel.DoDatePickerChange';
begin
  try
//    FAppModules.ViewIni.WriteString(ClassName, C_IdLastDate, FormatDateTime('yyyy/mm/dd', FDatePicker.Date));
//    DoGISSelectionChanged(nil);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{
procedure TGISStudyAreaSelectorPanel.DoGISSelectionChanged(ASender: TObject);
const OPNAME = 'TGISStudyAreaSelectorPanel.DoGISSelectionChanged';
var LCurrentSelection: string;
begin
  try
    if Assigned(FGISViewer) then
    begin
      try // Temporary to block "library not registered error" pdna - vgn 20020710
        if (FGISViewer.Search_GetSelectionValues.Count <= 0) then
        begin
          DoStudyAreaSelectionEvent('', '', 0.0, ssaNoStudy);
        end else begin
          LCurrentSelection := '';
          if (FGISViewer.Search_GetSelectionValues.Count > 0) then
          begin
            LCurrentSelection := FGISViewer.Search_GetSelectionValues.Item[0];
            if (LCurrentSelection <> '') then
              DoStudyAreaSelectionEvent(
                FModelCombo.Text, LCurrentSelection, FDatePicker.Date, ssaShowStudyDetails);
          end;
        end;
      except // Temporary to block "library not registered error" pdna - vgn 20020710
      end; // Temporary to block "library not registered error" pdna - vgn 20020710
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;
 }

procedure TGISStudyAreaSelectorPanel.SetSelection(AStudy, ASubArea, AStudyShapefile,
                                                    aSubAreaShapeFile : string; aActionLevel: TModelActionLevel);
const OPNAME = 'TGISStudyAreaSelectorPanel.SetSelection';
var
  LHandle : integer;
  LShape : TMWGShapefile;
  LIndex : integer;

begin
  try
    LockWindowUpdate(Self.Handle);
    try
      try
        LShape := TMWGShapefile.Create(nil);
        try
          LShape.Open(AStudyShapefile + '.shp', LShape.GlobalCallback);
           for LIndex := FGISViewer.NumLayers-1 to FGISViewer.NumLayers-1 do
          begin
            if (FGISViewer.NumLayers-1 = 4) then
              FGISViewer.RemoveLayer(LIndex);

            LHandle := FGISViewer.AddLayer(LShape.DefaultInterface , True);
            FGISViewer.ShapeLayerFillColor[LHandle] := 40375925;
            FGISViewer.LayerVisible[LHandle] := True;
          end;
        finally
          FreeAndNil(LShape);
        end;
      except
        // No DIS viewer.
      end;
    finally
      LockWindowUpdate(0);
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TGISStudyAreaSelectorPanel.GetGISViewer :  TMap;  //TArcGISViewer104Control; //TGisViewerX;
const OPNAME = 'TGISStudyAreaSelectorPanel.GetGISViewer';
begin
  Result := nil;
  try
    Result := FGISViewer;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;


end.
