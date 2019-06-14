{******************************************************************************}
{*  UNIT      : Contains TRainfallGaugeSelectionValidator Class               *}
{*  AUTHOR    : RH Steyn(Cornastone)                                          *}
{*  DATE      : 04/05/2005                                                    *}
{*  COPYRIGHT : Copyright © 2005 DWAF                                         *}
{******************************************************************************}

unit URainfallGaugeSelectionValidator;

interface
{$WARN UNIT_PLATFORM OFF}
uses                                                    
  Classes,
  VCL.stdctrls,
  VCL.ComCtrls,
  VCL.extctrls,
  VCL.CheckLst,
  Math,
  VCL.Controls,
  VCL.Dialogs,
  VCL.Forms,
  Windows,

  UDataComponent,
  URainfallGaugeSelectionDialog,

  UHelpContexts,
  UAbstractObject,
  UAbstractComponent,
  URainfallGaugeSelectionMenuItemManager,
  RainfallCom_TLB,
  URaingaugeGISPanel,
  USearchDialog,
  UGenericModelLinkClasses,
  UShellExecuteObject;

type
  { Rain Tabsheet class }
  TRainfallGaugeSelectionValidator = class(TAbstractDataDialogValidator)
  protected
    FSearchDialog          : TRGSearchDialog;
    FReplaceSelection      : boolean;
    FUpdateGISLive         : boolean;
    FSystemChange          : Boolean;
    FShowTree              : boolean;
    FGaugeDeletable        : boolean;
    FCurrentStationID      : integer;
    FMenuItemManager       : TRainfallGaugeSelectionMenuItemManager;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    function RainfallGaugeSelectionDialog : TRainfallGaugeSelectionDialog;
    procedure TrvGaugeListChange(Sender: TObject; Node: TTreeNode);
    procedure GaugeCheckListBoxClick(Sender: TObject);
    procedure BtnUpdateGISViewerClick(Sender : TObject);
    procedure SaveProjectGaugesToDB;
    function RepopulateGaugeTreeview : boolean;
    procedure ReduceSecondAspectRatioToFirst ( AX1, AY1 : integer; var AX2,AY2 : integer );
    procedure UpdateGUI;
    procedure UpdateStatusPanel;
    procedure DoSelectedGaugesListBoxOnDrawItem ( Control: TWinControl; Index: Integer;
                                                  Rect:TRect;State: TOwnerDrawState );
  public
    function DoCustomEvent (ACustomModelEvent : TModelMenuData): Boolean;
    procedure DisplayGISViewerSelectionList;
    procedure DoSetAddToSelection(Sender : TObject);
    procedure DoUnSelectAll(Sender: TObject);
    procedure DoUnSelect(Sender: TObject);
    procedure DoSelectByStationNumber(Sender: TObject);
    procedure DoSelectByStationName(Sender: TObject);
    procedure DoSelectByRect(Sender: TObject);
    procedure DoSelectByDistance(Sender: TObject);
    procedure DoViewStatusbar(Sender : TObject);
    procedure DoToggleTree (Sender : TObject);
    procedure DoHelpContents;
    procedure DoHelpAbout;
    procedure DoClearUserData (Sender : TObject);
    procedure OnKeyDownListBox(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure DoUpdateGaugePanel(Sender: TObject);
    procedure SynchroniseCheckList;
    procedure SynchroniseGISViewer;
    procedure SynchroniseSelectionList;
    procedure DoGisSelectionHasChanged(Sender : TObject);
    procedure DoResizeRainfall(Sender : TObject);

    procedure DoExport(AFileName: string = ''); override;
    function CanExport: Boolean; override;
    function CanCopyToCLipboard: Boolean; override;
    procedure DoCopyToCLipboard; override;
    function CanPrint: Boolean; override;
    procedure DoPrint; override;

    function Initialise: boolean; override;
    procedure PopulateDataViewer; override;
    function LanguageHasChanged: boolean; override;
    function StudyDataHasChanged (AContext   : TChangeContext;
                                  AFieldName : string;
                                  AOldValue  : string;
                                  ANewValue  : string): Boolean; override;
    function StudyHasChanged: boolean; override;
    function SaveState: boolean; override;
    property MenuItemManager  : TRainfallGaugeSelectionMenuItemManager read FMenuItemManager write FMenuItemManager;

end;

implementation

uses
  System.UITypes,
  SysUtils,
  VCL.FileCtrl,
  VCL.Graphics,
  VCL.Clipbrd,
  UDataSetType,
  VCL.Printers,
  UErrorHandlingOperations;

{ TRainfallGaugeSelectionValidator }

procedure TRainfallGaugeSelectionValidator.CreateMemberObjects;
const OPNAME = 'TRainfallGaugeSelectionValidator.CreateMemberObjects';
const LPracticalHeight : integer = 300;
begin
  inherited CreateMemberObjects;
  try
    FPanel := TRainfallGaugeSelectionDialog.Create(nil, FAppModules);
    with RainfallGaugeSelectionDialog do
    begin
      GaugeListTreeview.OnChange      := TrvGaugeListChange;
      GaugeCheckListBox.OnClickCheck  := GaugeCheckListBoxClick;
      SelectedGaugesListBox.OnKeyDown := OnKeyDownListBox;
      SelectedGaugesListBox.OnClick   := DoUpdateGaugePanel;
      SelectedGaugesListBox.OnDrawItem := DoSelectedGaugesListBoxOnDrawItem;
      BtnUpdateGISViewer.OnClick      := BtnUpdateGISViewerClick;
    end;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TRainfallGaugeSelectionValidator.DestroyMemberObjects;
const OPNAME = 'TRainfallGaugeSelectionValidator.DestroyMemberObjects';
begin
  inherited DestroyMemberObjects;
  try

    SaveState;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TRainfallGaugeSelectionValidator.Initialise: boolean;
const OPNAME = 'TRainfallGaugeSelectionValidator.Initialise';
begin
  Result := False;
  try
    FSystemChange := FALSE;
    FShowTree     := TRUE;

    FReplaceSelection := (FAppModules.ViewIni.ReadInteger(ClassName,'ReplaceSelection',0 ) = 1 );
    FUpdateGISLive    := Boolean(FAppModules.ViewIni.ReadInteger(ClassName,'UpdateGISViewerLive',0));

    if (FMenuItemManager <> nil) then
    begin
      if (FReplaceSelection) then
        FMenuItemManager.SetMenuState(mcReplaceSelection)
      else
        FMenuItemManager.SetMenuState(mcUnReplaceSelection);
    end;

    Result := RainfallGaugeSelectionDialog.Initialise;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TRainfallGaugeSelectionValidator.PopulateDataViewer;
const OPNAME = 'TRainfallGaugeSelectionValidator.PopulateDataViewer';
var
  lIndex   : integer;
  lStation : IStationData;
begin
  try
    if Assigned(RainfallGaugeSelectionDialog.RaingaugeGISPanel) then
      RainfallGaugeSelectionDialog.RaingaugeGISPanel.OnGISSelectionChanged := nil;
    RepopulateGaugeTreeview;
    (FAppModules.Model.ModelData as IRainfallModelData).GaugeList.DeSelectAll;
    for LIndex := 0 to (FAppModules.Model.ModelData as IRainfallModelData).StationCount - 1 do
    begin
      lStation := (FAppModules.Model.ModelData as IRainfallModelData).GetStationDataByIndex(lIndex);
      (FAppModules.Model.ModelData as IRainfallModelData).GaugeList.
        SelectByStationID(lStation.RainfallData.StationID, False);
    end;
    SynchroniseSelectionList;
    with RainfallGaugeSelectionDialog do
    begin
      if Assigned(RaingaugeGISPanel) then
        RaingaugeGISPanel.OnGISSelectionChanged := DoGisSelectionHasChanged;
      GaugeCheckListBox.Items.Clear;
    end;
    UpdateStatusPanel;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRainfallGaugeSelectionValidator.RainfallGaugeSelectionDialog : TRainfallGaugeSelectionDialog;
const OPNAME = 'TRainfallGaugeSelectionValidator.RainfallGaugeSelectionDialog';
begin
  Result := nil;
  try
    if (FPanel <> nil) then
      Result := TRainfallGaugeSelectionDialog(FPanel);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRainfallGaugeSelectionValidator.LanguageHasChanged: boolean;
const OPNAME = 'TRainfallGaugeSelectionValidator.LanguageHasChanged';
begin
  Result := inherited LanguageHasChanged;;
  try
    result := true;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRainfallGaugeSelectionValidator.StudyHasChanged: boolean;
const OPNAME = 'TRainfallGaugeSelectionValidator.StudyHasChanged';
begin
  Result := inherited StudyHasChanged;
  try
    PopulateDataViewer;
    Result := true;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRainfallGaugeSelectionValidator.StudyDataHasChanged (AContext   : TChangeContext;
                                                               AFieldName : string;
                                                               AOldValue  : string;
                                                               ANewValue  : string): Boolean;
const OPNAME = 'TRainfallGaugeSelectionValidator.StudyDataHasChanged';
begin
  Result := inherited StudyDataHasChanged(AContext, AFieldName, AOldValue, ANewValue);
  try
    if ((AContext = sdccAdd) OR (AContext = sdccDelete)) AND (AFieldName = 'User Gauges') then
      PopulateDataViewer;
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TRainfallGaugeSelectionValidator.UpdateGUI;
const OPNAME = 'TRainfallGaugeSelectionValidator.UpdateGUI';
begin
  try
    UpdateStatusPanel;
    SynchroniseSelectionList;
    SynchroniseCheckList;
    DoUpdateGaugePanel(nil);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRainfallGaugeSelectionValidator.DoGisSelectionHasChanged(Sender: TObject);
const OPNAME = 'TRainfallGaugeSelectionValidator.DoGisSelectionHasChanged';
var
  LIndex           : integer;
  LGauge           : IRainGauge;
  lSelectionList   : TStringList;
  lGaugeNr         : string;
begin
  try
    if ((NOT FSystemChange) AND
        Assigned(FAppModules.Model()) AND
        Assigned(FAppModules.Model.ModelData()) AND
        Assigned((FAppModules.Model.ModelData as IRainfallModelData).GaugeList()) AND
        Assigned(RainfallGaugeSelectionDialog.RaingaugeGISPanel.SelectedList)) then
    begin
      lSelectionList := TStringList.Create;
      try
        lSelectionList.Sorted := TRUE;
        lSelectionList.Text := RainfallGaugeSelectionDialog.RaingaugeGISPanel.SelectedList.Text;
        if (FReplaceSelection) then
          (FAppModules.Model.ModelData as IRainfallModelData).GaugeList.DeSelectAll;
        for LIndex := 0 to lSelectionList.Count - 1 do
        begin
          lGaugeNr := lSelectionList.Strings[lIndex];
          LGauge := (FAppModules.Model.ModelData as IRainfallModelData).GaugeList.GetGaugeByNumber(lGaugeNr);

          if Assigned(LGauge) then
            LGauge.Selected := TRUE;
     
        end;
      finally
        FreeAndNil(lSelectionList);
      end;
      SaveProjectGaugesToDB;
      UpdateGUI;
      SynchroniseGISViewer;
    end;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TRainfallGaugeSelectionValidator.DoHelpAbout;
const OPNAME = 'TRainfallGaugeSelectionValidator.DoHelpAbout';
begin
  try
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TRainfallGaugeSelectionValidator.DoHelpContents;
const OPNAME = 'TRainfallGaugeSelectionValidator.DoHelpContents';
begin
  try
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TRainfallGaugeSelectionValidator.DoSelectByDistance(Sender: TObject);
const OPNAME = 'TRainfallGaugeSelectionValidator.DoSelectByDistance';
var
  lDistance  : double;
  lLatitude  : double;
  lLongitude : double;
begin
  try
    try
      FSearchDialog := TRGSearchDialog.CreateWithoutDFM(nil, FAppModules);
      FSearchDialog.SetSearchFormType(StDistance, Application.Title);
      FSearchDialog.ShowFormModal(StDistance);
      lDistance  := FSearchDialog.Distance;
      lLatitude  := FSearchDialog.Latitude;
      lLongitude := FSearchDialog.Longitude;
      if (FSearchDialog.ModalResult = mrOK) then
      begin
        (FAppModules.Model.ModelData as IRainfallModelData).
          GaugeList.SelectByDistance(lLatitude, lLongitude, lDistance, FReplaceSelection );
        SaveProjectGaugesToDB;
        UpdateGUI;
        SynchroniseGISViewer;
        if (FMenuItemManager <> nil) AND
           (RainfallGaugeSelectionDialog.SelectedGaugesListBox.Count > 0) then
          FMenuItemManager.SetMenuState ( mcSelectionIsAvailable );
      end;
    finally
      FSearchDialog.Free;
    end;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TRainfallGaugeSelectionValidator.DoSelectByRect;
const OPNAME = 'TRainfallGaugeSelectionValidator.DoSelectByRect';
var
  LRect : TRect;
begin
  try
    try
      FSearchDialog := TRGSearchDialog.CreateWithoutDFM ( nil, FAppModules );
      FSearchDialog.SetSearchFormType ( stRectangle, Application.Title );
      FSearchDialog.ShowFormModal ( stRectangle );
      try
        LRect.Top := FSearchDialog.TopValue;
        LRect.Left := FSearchDialog.LeftValue;
        LRect.Bottom := FSearchDialog.BottomValue;
        LRect.Right := FSearchDialog.RightValue;
        if (FSearchDialog.ModalResult = mrOK) then
        begin
          (FAppModules.Model.ModelData as IRainfallModelData).
            GaugeList.SelectByRectangle(LRect.Left, LRect.Top, LRect.Bottom, LRect.Right, FReplaceSelection);
          SaveProjectGaugesToDB;
          UpdateGUI;
          SynchroniseGISViewer;
          if (FMenuItemManager <> nil) AND
             (RainfallGaugeSelectionDialog.SelectedGaugesListBox.Count > 0) then
            FMenuItemManager.SetMenuState(mcSelectionIsAvailable);
        end;
      except
        ShowMessage(FAppModules.Language.GetString('Message.InvalidCoordinateEntered'));
      end;
    finally
      FSearchDialog.Free;
    end;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TRainfallGaugeSelectionValidator.DoSelectByStationName (Sender: TObject);
const OPNAME = 'TRainfallGaugeSelectionValidator.DoSelectByStationName';
begin
  try
    try
      FSearchDialog := TRGSearchDialog.CreateWithoutDFM ( nil, FAppModules );
      FSearchDialog.SetSearchFormType ( StStationName, Application.Title );
      FSearchDialog.ShowFormModal ( StStationName );
      if (FSearchDialog.StationName <> '') AND (FSearchDialog.ModalResult = mrOK) then
      begin
        (FAppModules.Model.ModelData as IRainfallModelData).GaugeList.
          SelectByName(UpperCase(FSearchDialog.StationName), FReplaceSelection);
        SaveProjectGaugesToDB;
        UpdateGUI;
        SynchroniseGISViewer;
        if (FMenuItemManager <> nil) AND
           (RainfallGaugeSelectionDialog.SelectedGaugesListBox.Count > 0) then
          FMenuItemManager.SetMenuState(mcSelectionIsAvailable);
      end;
    finally
      FSearchDialog.Free;
    end;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TRainfallGaugeSelectionValidator.DoSelectByStationNumber (Sender: TObject);
const OPNAME = 'TRainfallGaugeSelectionValidator.DoSelectByStationNumber';
begin
  try
    try
      FSearchDialog := TRGSearchDialog.CreateWithoutDFM( nil, FAppModules );
      FSearchDialog.SetSearchFormType ( StStationNumber, Application.Title );
      FSearchDialog.ShowFormModal(StStationNumber);

      if (FSearchDialog.GaugeNumber <> '') AND (FSearchDialog.ModalResult = mrOk) then
      begin
        (FAppModules.Model.ModelData as IRainfallModelData).
          GaugeList.SelectByNumber(FSearchDialog.GaugeNumber, FReplaceSelection);
        SaveProjectGaugesToDB;
        UpdateGUI;
        SynchroniseGISViewer;
        if (FMenuItemManager <> nil) AND
           (RainfallGaugeSelectionDialog.SelectedGaugesListBox.Count > 0) then
          FMenuItemManager.SetMenuState(mcSelectionIsAvailable);
      end;
    finally
      FSearchDialog.Free;
    end;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TRainfallGaugeSelectionValidator.DoSetAddToSelection(Sender: TObject);
const OPNAME = 'TRainfallGaugeSelectionValidator.DoSetAddToSelection';
var
  LChecked : boolean;
begin
  try
    if (FMenuItemManager <> nil) then
    begin
      if not (FMenuItemManager.GetCheckedState(MeOptionAddToSelection)) then
      begin
        FMenuItemManager.SetMenuState(mcReplaceSelection);
        LChecked := true;
      end
      else
      begin
        FMenuItemManager.SetMenuState(mcUnReplaceSelection);
        LChecked := false;
      end;
      FReplaceSelection := LChecked;
      if (RainfallGaugeSelectionDialog.SelectedGaugesListBox.Count > 0) then
        FMenuItemManager.SetMenuState(mcSelectionIsAvailable);
    end;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TRainfallGaugeSelectionValidator.DoUnSelectAll;
const OPNAME = 'TRainfallGaugeSelectionValidator.DoUnSelectAll';
var
  lKey : word;
  lIndex : integer;
begin
  try
    if Assigned((FAppModules.Model.ModelData as IRainfallModelData).GaugeList()) then
    begin
      if (FAppModules.Model.ModelData as IRainfallModelData).GaugeList.TotalCount = 0 then
        ShowMessage(FAppModules.Language.GetString('Message.GaugesNotFound'))
      else
      begin
        lKey := VK_DELETE;
        with RainfallGaugeSelectionDialog do
        begin
          for lIndex := 0 to SelectedGaugesListBox.Items.Count - 1 do
            SelectedGaugesListBox.Selected[lIndex] := TRUE;
        end;
        OnKeyDownListBox(nil, lKey, []);
      end;
    end;
    if (FMenuItemManager <> nil) AND
       (RainfallGaugeSelectionDialog.SelectedGaugesListBox.Count = 0) then
      FMenuItemManager.SetMenuState(mcSelectionIsNotAvailable);
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TRainfallGaugeSelectionValidator.DoUnSelect(Sender: TObject);
const OPNAME = 'TRainfallGaugeSelectionValidator.DoUnSelect';
var
  lKey : word;
begin
  try
    if Assigned((FAppModules.Model.ModelData as IRainfallModelData).GaugeList()) then
    begin
      if (FAppModules.Model.ModelData as IRainfallModelData).GaugeList.TotalCount = 0 then
        ShowMessage(FAppModules.Language.GetString('Message.GaugesNotFound'))
      else
      begin
        lKey := VK_DELETE;
        OnKeyDownListBox(nil, lKey, []);
      end;
    end;
    if (FMenuItemManager <> nil) AND
       (RainfallGaugeSelectionDialog.SelectedGaugesListBox.Count = 0) then
      FMenuItemManager.SetMenuState(mcSelectionIsNotAvailable);
  except on E : Exception do HandleError(E,OPNAME); end;
end;


procedure TRainfallGaugeSelectionValidator.DoViewStatusbar(Sender: TObject);
const OPNAME = 'TRainfallGaugeSelectionValidator.DoViewStatusbar';
var
  LStatusBarVisible : boolean;
begin
  try
    if (FMenuItemManager <> nil) then
    begin
      if not (FMenuItemManager.GetCheckedState(MeViewStatusBar)) then
      begin
        FMenuItemManager.SetMenuState(mcViewStatusBar);
        LStatusBarVisible := true;
      end
      else
      begin
        FMenuItemManager.SetMenuState(mcHideStatusBar);
        LStatusBarVisible := false;
      end;
      RainfallGaugeSelectionDialog.StatusBar.Visible := LStatusBarVisible;
    end;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TRainfallGaugeSelectionValidator.DoToggleTree;
const OPNAME = 'TRainfallGaugeSelectionValidator.DoToggleTree';
begin
  try
    FShowTree := NOT FShowTree;
    DisplayGISViewerSelectionList;
    if FShowTree then
      FAppModules.SetMenuItem(CToggleTree, msChecked)
    else
      FAppModules.SetMenuItem(CToggleTree, msUnChecked);
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TRainfallGaugeSelectionValidator.DisplayGISViewerSelectionList;
const OPNAME = 'TRainfallGaugeSelectionValidator.DisplayGISViewerSelectionList';
begin
  try
    with RainfallGaugeSelectionDialog do
    begin
      GaugeSelectionPanel.Visible := FShowTree;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TRainfallGaugeSelectionValidator.OnKeyDownListBox (Sender  : TObject;
                                                             var Key : Word;
                                                             Shift   : TShiftState);
const OPNAME = 'TRainfallGaugeSelectionValidator.OnKeyDownListBox';
var
  LIndex       : integer;
  lGaugeList   : IRainGaugeList;
  lGauge       : IRainGauge;
  lStation     : IStationData;
  lRainfallObj : IRainfallModelData;
  lMessage     : string;
  lGaugeNr     : string;
begin
  try
    if (Key = VK_DELETE) then
    begin
      lRainfallObj := (FAppModules.Model.ModelData as IRainfallModelData);
      lGaugeList   := lRainfallObj.GaugeList;
      if (lGaugeList <> nil) then
      begin
        for LIndex := 0 to RainfallGaugeSelectionDialog.SelectedGaugesListBox.Count - 1 do
        begin
          if (RainfallGaugeSelectionDialog.SelectedGaugesListBox.Selected[LIndex]) then
          begin
            lGaugeNr := RainfallGaugeSelectionDialog.SelectedGaugesListBox.Items[LIndex];
            if (Pos('*', lGaugeNr) > 0) then
              lGaugeNr := Copy(lGaugeNr, 1, Length(lGaugeNr) - 2);
            lGauge   := lGaugeList.GetGaugeByNumber(lGaugeNr);
            lStation := lRainfallObj.GetStationDataByNumber(lGauge.GaugeNumber);
            if Assigned(lStation) then
              FGaugeDeletable := lStation.MayBeDeleted;           
            if (lStation = nil) then
              lGauge.Selected := FALSE
            else
            if (lStation.IsPartOfZone) then
            begin
              lMessage := FAppModules.Language.GetString('Rainfall.Gauge') + lStation.RainfallData.StationNumber +
                          FAppModules.Language.GetString('Rainfall.MayNotBeDeleted') +
                          FAppModules.Language.GetString('Rainfall.BecauseUsedInZone');
              ShowMessage(lMessage);
            end
            else
            if (lStation.HasACreatedPatch) then
            begin
              lMessage := FAppModules.Language.GetString('Rainfall.Gauge') + lStation.RainfallData.StationNumber +
                          FAppModules.Language.GetString('Rainfall.MayNotBeDeleted') +
                          FAppModules.Language.GetString('Rainfall.BecausePatchIsCreated');
              ShowMessage(lMessage);
            end
            else
            if (lStation.IsAPatchSource) then
            begin
              lMessage := FAppModules.Language.GetString('Rainfall.Gauge') + lStation.RainfallData.StationNumber +
                          FAppModules.Language.GetString('Rainfall.MayNotBeDeleted') +
                          FAppModules.Language.GetString('Rainfall.BecauseIsSourceToPatch');
              ShowMessage(lMessage);
            end
            else
            begin
              lGauge.Selected := FALSE;
            end;
          end;
        end;
        if lGaugeList <> nil then
          lGaugeList   := nil;
        if lGauge <> nil then
          lGauge       := nil;
        if lStation <> nil then
          lStation     := nil;
        if lRainfallObj <> nil then
          lRainfallObj := nil;
        SaveProjectGaugesToDB;
        UpdateGUI;
        SynchroniseGISViewer;
      end;
    end
  except on E:Exception do HandleError(E,OPNAME) end;
end;

procedure TRainfallGaugeSelectionValidator.SynchroniseCheckList;
const OPNAME = 'TRainfallGaugeSelectionValidator.SynchroniseCheckList';
var
  LIndex   : integer;
  lGaugeNr : string;
begin
  try
    if Assigned((FAppModules.Model.ModelData as IRainfallModelData).GaugeList()) then
    begin
      with RainfallGaugeSelectionDialog do
      begin
        for LIndex := 0 to GaugeCheckListBox.Items.Count - 1 do
        begin
          lGaugeNr := GaugeCheckListBox.Items[LIndex];
          if (Pos('*', lGaugeNr) > 0) then
            lGaugeNr := Copy(lGaugeNr, 1, Length(lGaugeNr) - 2);
          GaugeCheckListBox.Checked[LIndex] := (FAppModules.Model.ModelData as IRainfallModelData).
                                                  GaugeList.GetGaugeByNumber(lGaugeNr).Selected;
        end;
      end;
    end;
  except on E:Exception do HandleError(E,OPNAME) end;
end;

procedure TRainfallGaugeSelectionValidator.SynchroniseGISViewer;
const OPNAME = 'TRainfallGaugeSelectionValidator.SynchroniseGISViewer';
var
  lIndex         : integer;
  lGaugeNr : string;
begin
  try
    FSystemChange := TRUE;
    if (FUpdateGISLive) and Assigned(RainfallGaugeSelectionDialog.RaingaugeGISPanel) then
    begin
      RainfallGaugeSelectionDialog.RaingaugeGISPanel.SelectedList.Clear;
      for LIndex := 0 to RainfallGaugeSelectionDialog.SelectedGaugesListBox.Count - 1 do
      begin
        //lGaugeNr := Copy(RainfallGaugeSelectionDialog.SelectedGaugesListBox.Items[LIndex], 1,
        //            Length ( RainfallGaugeSelectionDialog.SelectedGaugesListBox.Items[LIndex] ) - 2);
        lGaugeNr := RainfallGaugeSelectionDialog.SelectedGaugesListBox.Items[LIndex];
        if (Pos('*', lGaugeNr) > 0) then
          lGaugeNr := Copy(lGaugeNr, 1, Length(lGaugeNr) - 2);
        RainfallGaugeSelectionDialog.RaingaugeGISPanel.SelectedList.Add(lGaugeNr);
      end;
      RainfallGaugeSelectionDialog.RaingaugeGISPanel.UpdateFromList;
    end;
    FSystemChange := FALSE;
  except on E:Exception do HandleError(E,OPNAME) end;
end;

procedure TRainfallGaugeSelectionValidator.SynchroniseSelectionList;
const OPNAME = 'TRainfallGaugeSelectionValidator.SynchroniseSelectionList';
var
  LStringList : TStringList;
  LGauges     : string;
  LIndex      : integer;
  LGaugeNr    : string;
  lGauge      : IRainGauge;
begin
  try
    FCurrentStationID := (FAppModules.Model.ModelData as IRainfallModelData).CurrentStationID;
    if Assigned((FAppModules.Model.ModelData as IRainfallModelData).GaugeList()) then
    begin
      LStringList := TStringList.Create;
      try
        lGauges := (FAppModules.Model.ModelData as IRainfallModelData).GaugeList.GetSelectedGauges;
        LStringList.CommaText := lGauges;
        RainfallGaugeSelectionDialog.SelectedGaugesListBox.Items.Text := LStringList.GetText;
        for LIndex := 0 to RainfallGaugeSelectionDialog.SelectedGaugesListBox.Count - 1 do
        begin
          lGaugeNr := RainfallGaugeSelectionDialog.SelectedGaugesListBox.Items[LIndex];
          if (Pos('*', lGaugeNr) > 0) then
            lGaugeNr := Copy(lGaugeNr, 1, Length(lGaugeNr) - 2);
            lGauge := (FAppModules.Model.ModelData as IRainfallModelData).
                                                  GaugeList.GetGaugeByNumber(lGaugeNr);
            if ( lGauge <> nil ) and ( lGauge.GaugeID = FCurrentStationID ) then
              RainfallGaugeSelectionDialog.SelectedGaugesListBox.Selected [ LIndex ] := True;
        end;

      finally
        FreeAndNil(LStringList);
      end;
    end;
  except on E:Exception do HandleError(E,OPNAME) end;
end;

procedure TRainfallGaugeSelectionValidator.GaugeCheckListBoxClick(Sender: TObject);
const OPNAME = 'TRainfallGaugeSelectionValidator.GaugeCheckListBoxClick';
var
  LGauge       : IRainGauge;
  lGaugeNr     : string;
  lGaugeNrStar : string;
  lKey         : word;
begin
  try
    if Assigned((FAppModules.Model.ModelData as IRainfallModelData).GaugeList()) then
    begin
      with RainfallGaugeSelectionDialog do
      begin
        lGaugeNrStar := GaugeCheckListBox.Items[GaugeCheckListBox.ItemIndex];
        if (Pos('*', lGaugeNrStar) > 0) then
          lGaugeNr := Copy(lGaugeNrStar, 1, Length(lGaugeNrStar) - 2)
        else
          lGaugeNr := lGaugeNrStar;
        LGauge := (FAppModules.Model.ModelData as IRainfallModelData).GaugeList.GetGaugeByNumber(lGaugeNr);
        LGauge.Selected := GaugeCheckListBox.Checked[GaugeCheckListBox.ItemIndex];
        if (GaugeCheckListBox.Checked[GaugeCheckListBox.ItemIndex]) then
        begin
          RaingaugeGISPanel.AddToSelection(lGaugeNr);
          if (SelectedGaugesListBox.Items.IndexOf(lGaugeNrStar) < 0) then
            SelectedGaugesListBox.Items.Add(lGaugeNrStar);
          SaveProjectGaugesToDB;
          UpdateStatusPanel;
          DoUpdateGaugePanel(nil);
          SynchroniseGISViewer;
        end
        else
        begin
          RaingaugeGISPanel.RemoveFromSelection(lGaugeNr);
          if (SelectedGaugesListBox.Items.IndexOf(lGaugeNrStar) >= 0) then
          begin
            lKey := VK_DELETE;
            SelectedGaugesListBox.Selected[SelectedGaugesListBox.Items.IndexOf(lGaugeNrStar)] := TRUE;
            OnKeyDownListBox(Sender, lKey, []);
            if FGaugeDeletable then
              SelectedGaugesListBox.Items.Delete(SelectedGaugesListBox.Items.IndexOf(lGaugeNrStar));
          end;
        end;
        if LGauge <> nil then
          LGauge := nil;
        if (FMenuItemManager <> nil) then
        begin
          if (SelectedGaugesListBox.Items.Count > 0) then
            FMenuItemManager.SetMenuState(mcSelectionIsAvailable)
          else
            FMenuItemManager.SetMenuState(mcSelectionIsNotAvailable);
        end;
      end;
    end;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TRainfallGaugeSelectionValidator.TrvGaugeListChange (Sender : TObject;
                                                               Node   : TTreeNode);
const OPNAME = 'TRainfallGaugeSelectionValidator.TrvGaugeListChange';
var
  LIndex       : integer;
  LAddedIndex  : integer;
  LGauge       : IRainGauge;
  lGaugeNumber : string;
begin
  try
    if (NOT FSystemChange) then
    begin
      with RainfallGaugeSelectionDialog do
      begin
        GaugeCheckListBox.Items.Clear;
        if (Assigned(GaugeListTreeview.Selected)) then
        begin
          if (GaugeListTreeview.Selected.Level = 0 ) then
          begin
            for LIndex := 0 to (FAppModules.Model.ModelData as IRainfallModelData).GaugeList.TotalCount - 1 do
            begin
              LGauge := (FAppModules.Model.ModelData as IRainfallModelData).GaugeList.GetGaugeByIndex(LIndex);
              if (UpperCase(GaugeListTreeview.Selected.Text) = UpperCase(LGauge.Group)) then
              begin
                lGaugeNumber := LGauge.GaugeNumber;
                if (LGauge.IsInWR90) then
                  lGaugeNumber := lGaugeNumber + ' *';
                LAddedIndex := GaugeCheckListBox.Items.Add(lGaugeNumber);
                GaugeCheckListBox.Checked[LAddedIndex] :=  LGauge.Selected;
              end;
            end;
          end;
        end;
      end;
    end;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TRainfallGaugeSelectionValidator.UpdateStatusPanel;
const OPNAME = 'TRainfallGaugeSelectionValidator.UpdateStatusPanel';
var
  lTotal    : integer;
  lSelected : integer;
  lStatusTotal : String;
  lStatusSelected : String;
  lStatusUnSelected : String;
begin
  try
    lStatusTotal := FAppModules.Language.GetString('StatusBarText.Total');
    lStatusSelected := FAppModules.Language.GetString('StatusBarText.Selected');
    lStatusUnSelected := FAppModules.Language.GetString('StatusBarText.UnSelected');
    with RainfallGaugeSelectionDialog do
    begin
      if Assigned((FAppModules.Model.ModelData as IRainfallModelData).GaugeList()) then
      begin
        lTotal    := (FAppModules.Model.ModelData as IRainfallModelData).GaugeList.TotalCount;
        lSelected := (FAppModules.Model.ModelData as IRainfallModelData).GaugeList.SelectedCount;
        StatusBar.Panels[0].Text := Format(lStatusTotal,[lTotal]);
        StatusBar.Panels[1].Text := Format(lStatusSelected,[lSelected]);
        StatusBar.Panels[2].Text := Format(lStatusUnSelected,[lTotal - lSelected]);
        StatusBar.Panels[3].Text := '';
      end
      else
      begin
        StatusBar.Panels[0].Text := Format(lStatusTotal,[0]);
        StatusBar.Panels[1].Text := Format(lStatusSelected,[0]);
        StatusBar.Panels[2].Text := Format(lStatusUnSelected,[0]);
        StatusBar.Panels[3].Text := '';
      end;
      StatusBar.Update;
    end;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TRainfallGaugeSelectionValidator.SaveState: boolean;
const OPNAME = 'TRainfallGaugeSelectionValidator.SaveState';
begin
  Result := False; //inherited SaveState;
  try
    FAppModules.ViewIni.WriteInteger(ClassName,'PanelHeight', FPanel.Height);
    Result := true;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TRainfallGaugeSelectionValidator.BtnUpdateGISViewerClick(Sender: TObject);
const OPNAME = 'TRainfallGaugeSelectionValidator.BtnUpdateGISViewerClick';
var
  LSavedBoolean : boolean;
begin
  try
    LSavedBoolean := FUpdateGISLive;
    TButton(Sender).Enabled := FALSE;
    FUpdateGISLive := True;
    SynchroniseGISViewer;
    FUpdateGISLive := LSavedBoolean;
    TButton(Sender).Enabled := TRUE;
  except on E:Exception do HandleError(E,OPNAME) end;
end;

procedure TRainfallGaugeSelectionValidator.DoUpdateGaugePanel(Sender: TObject);
const OPNAME = 'TRainfallGaugeSelectionValidator.DoUpdateGaugePanel';
var
  LIndex   : integer;
  lGaugeNr : string;
  lStation : IStationData;
  lLat     : WideString;
  lLong    : WideString;
begin
  try

    with RainfallGaugeSelectionDialog do
    begin
      if (SelectedGaugesListBox.SelCount <> 1) then
      begin
        LblStationNumberValue.Caption := '';
        LblStationNameValue.Caption   := '';
        LblStationLongitude.Caption   := '';
        LblStationLatitude.Caption    := '';
      end
      else
      begin
        LIndex := SelectedGaugesListBox.ItemIndex;
        if ((LIndex >= 0) AND
            Assigned((FAppModules.Model.ModelData as IRainfallModelData).GaugeList())) then
        begin
          FCurrentStationID := (FAppModules.Model.ModelData as IRainfallModelData).CurrentStationID;
          lGaugeNr := SelectedGaugesListBox.Items[LIndex];
          if (Pos('*', lGaugeNr) > 0) then
            lGaugeNr := Copy(lGaugeNr, 1, Length(lGaugeNr) - 2);
          lStation := (FAppModules.Model.ModelData as IRainfallModelData).GetStationDataByNumber(lGaugeNr);
          if Assigned(lStation) then
          begin
            LblStationNumberValue.Caption := lStation.RainfallData.StationNumber;
            LblStationNameValue.Caption   := lStation.StationName;
            lStation.LatLong(lLat, lLong);
            LblStationLongitude.Caption   := FAppModules.Language.GetString('LabelText.StationLongitute') + lLong;
            LblStationLatitude.Caption    := FAppModules.Language.GetString('LabelText.StationLatitude') + lLat;
            if SelectedGaugesListBox.Selected [ LIndex ] then
              (FAppModules.Model.ModelData as IRainfallModelData).CurrentStationID := lStation.RainfallData.StationID;

          end;
        end;
      end;
    end;
  except on E:Exception do HandleError(E,OPNAME) end;
end;

procedure TRainfallGaugeSelectionValidator.DoResizeRainfall(Sender: TObject);
const OPNAME = 'TRainfallGaugeSelectionValidator.DoResizeRainfall';
var
  LMinHeight : integer;
begin
  try
    with RainfallGaugeSelectionDialog do
    begin
      GaugeInfoPanel.Height := 140;
      LMinHeight := SelectedGaugesListBox.ItemHeight * 6 + 2;

      if ((SelectedGaugesListBox.Height <= LMinHeight) and (FPanel.ClientHeight >= 200)) then
      begin
        SelectedGaugesListBox.Height := SelectedGaugesListBox.Height + GaugeInfoPanel.Height;
        GaugeInfoPanel.Height := GaugeInfoPanel.Height - GaugeInfoPanel.Height;
      end
      else
      begin
        SelectedGaugesListBox.Height := SelectedGaugesListBox.Height - GaugeInfoPanel.Height;
        GaugeInfoPanel.Height := 140;
      end;

      if (FPanel.ClientHeight < 200) then
      begin
        SelectedGaugesListBox.Hide;
        GaugeInfoPanel.Hide;
      end
      else begin
        GaugeInfoPanel.Show;
        SelectedGaugesListBox.Show;
      end;
    end;
  except on E:Exception do HandleError(E,OPNAME) end;
end;

function TRainfallGaugeSelectionValidator.RepopulateGaugeTreeview: boolean;
const OPNAME = 'TRainfallGaugeSelectionValidator.RepopulateGaugeTreeview';
var
  lIndex        : integer;
  lGroupIdx     : integer;
  lGauge        : IRainGauge;
  lPrevGroup    : string;
  lFound        : boolean;
begin
  Result := false;
  try
    if (Assigned(FAppModules.Model.ModelData()) AND
        Assigned((FAppModules.Model.ModelData as IRainfallModelData).GaugeList)) then
    begin
      with RainfallGaugeSelectionDialog do
      begin
        StatusBar.Panels[0].Text := '';
        StatusBar.Panels[1].Text := '';
        StatusBar.Panels[2].Text := '';
        StatusBar.Panels[3].Text := FAppModules.Language.GetString('StatusBarText.LoadNone');

        StatusBar.Panels[3].Text := FAppModules.Language.GetString('StatusBarText.ReadFile');
        lPrevGroup := '';

        FSystemChange := TRUE;
        GaugeListTreeview.Items.Clear;

        GaugeListTreeview.Items.BeginUpdate;
        try
          for lIndex := 0 to (FAppModules.Model.ModelData as IRainfallModelData).GaugeList.TotalCount - 1 do
          begin
            lGauge := (FAppModules.Model.ModelData as IRainfallModelData).GaugeList.GetGaugeByIndex(LIndex);
            if (lPrevGroup <> lGauge.Group) then
            begin
              lFound := FALSE;
              lGroupIdx := 0;
              while ((NOT lFound) AND (lGroupIdx < GaugeListTreeview.Items.Count)) do
              begin
                if (lGauge.Group = GaugeListTreeview.Items[lGroupIdx].Text) then
                  lFound := TRUE
                else
                  lGroupIdx := lGroupIdx + 1;
              end;
              if (NOT lFound) then
                GaugeListTreeview.Items.Add(nil, lGauge.Group);
              lPrevGroup := lGauge.Group;
            end;
          end;
          GaugeListTreeview.AlphaSort(FALSE);
          FSystemChange := FALSE;
        finally
          StatusBar.Panels[3].Text := '';
          GaugeListTreeview.Items.EndUpdate;
        end;

        if ((FAppModules.Model.ModelData as IRainfallModelData).GaugeList.TotalCount = 0) then
          ShowMessage(FAppModules.Language.GetString('Rainfall.WarningNoGaugesLoaded'))
        else
        begin
          if Assigned (RaingaugeGISPanel) then
          begin
            RaingaugeGISPanel.Refresh;
            RaingaugeGISPanel.Visible := True;
          end;
        end;
        StatusBar.Panels[3].Text := '';
      end;
    end;
    Result := TRUE;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TRainfallGaugeSelectionValidator.SaveProjectGaugesToDB;
const OPNAME = 'TRainfallGaugeSelectionValidator.SaveProjectGaugesToDB';
begin
  try
{
    if Assigned((FAppModules.Model.ModelData as IRainfallModelData).GaugeList()) then
    begin
      (FAppModules.Model.ModelData as IRainfallModelData).GaugeList.SaveToDB;
      (FAppModules.Model.ModelData as IRainfallModelData).LoadMonthlyData;
    end;
}
    (FAppModules.Model.ModelData as IRainfallModelData).SaveProjectGauges;
    (FAppModules.Model.ModelData as IRainfallModelData).LoadMonthlyData;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TRainfallGaugeSelectionValidator.CanPrint : Boolean;
const OPNAME = 'TRainfallGaugeSelectionValidator.CanPrint';
begin
  Result := false;
  try
    Result := RainfallGaugeSelectionDialog.RaingaugeGISPanel.CanPrint;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TRainfallGaugeSelectionValidator.DoPrint;
const OPNAME = 'TRainfallGaugeSelectionValidator.DoPrint';
var
  LPrinterOrientation : TPrinterOrientation;
  LBitmap : TBitmap;
  LX1 : integer;
  LY1 : integer;
  LX2 : integer;
  LY2 : integer;
  lSpoolFile : string;
begin
  try
    lSpoolFile := ExtractFilePath ( ApplicationExeName ) + 'wrcdata\SpoolFile.bmp';
    //RainfallGaugeSelectionDialog.RaingaugeGISPanel.GISViewer.Output_ExportMap ( 1, lSpoolFile, 1 );
    if ( Printer.Printers.Count > 0 ) then
    begin
      LBitmap := TBitmap.Create;
      try
        LBitmap.Width  := RainfallGaugeSelectionDialog.RaingaugeGISPanel.GISViewer.Width;
        LBitmap.Height := RainfallGaugeSelectionDialog.RaingaugeGISPanel.GISViewer.Height;
        LBitmap.LoadFromFile ( lSpoolFile );
        LPrinterOrientation := Printer.Orientation;
        try
          Printer.Orientation := poLandscape;
          LX1 := LBitmap.Width;
          LY1 := LBitMap.Height;
          LX2 := Printer.PageWidth;
          LY2 := Printer.PageHeight;
          ReduceSecondAspectRatioToFirst ( LX1,LY1,LX2,LY2 );
          if LX2 < Printer.PageWidth then
          begin
            LX1 := ((Printer.PageWidth - LX2) div 2);
            LY1 := 0;
            LX2 := LX2 + ((Printer.PageWidth - LX2) div 2 );
            LY2 := LY2;
          end;
          if LY2 < Printer.PageHeight then
          begin
            LX1 := 0;
            LY1:= ((Printer.PageHeight - LY2) div 2);
            LX2 := LX2;
            LY2 := LY2 + ((Printer.PageHeight - LY2) div 2);
          end;
          Printer.BeginDoc;
          Printer.Canvas.Stretchdraw(Rect(LX1, LY1 ,LX2 , LY2), LBitMap);
          Printer.EndDoc;
        finally
          Printer.Orientation := LPrinterOrientation;
        end;
      finally
        FreeAndNil(LBitmap);
        DeleteFile ( lSpoolFile );
      end;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TRainfallGaugeSelectionValidator.CanCopyToCLipboard : Boolean;
const OPNAME = 'TRainfallGaugeSelectionValidator.CanCopyToCLipboard';
begin
  Result := False;
  try
    Result := RainfallGaugeSelectionDialog.RaingaugeGISPanel.CanCopyToClipboard;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TRainfallGaugeSelectionValidator.CanExport : Boolean;
const OPNAME = 'TRainfallGaugeSelectionValidator.CanExport';
begin
  Result := False;
  try
    Result := RainfallGaugeSelectionDialog.RaingaugeGISPanel.CanExport
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TRainfallGaugeSelectionValidator.DoCopyToCLipboard;
const OPNAME = 'TRainfallGaugeSelectionValidator.DoCopyToCLipboard';
begin
  inherited;
  try
    //RainfallGaugeSelectionDialog.RaingaugeGISPanel.GISViewer.Output_CopyMap ( 1 );
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TRainfallGaugeSelectionValidator.DoExport(AFileName: string = '');
const OPNAME = 'TRainfallGaugeSelectionValidator.DoExport';
var
  LDialog : TSaveDialog;
begin
  try
    try
      LDialog := TSaveDialog.Create ( nil );
      LDialog.DefaultExt  := 'BMP';
      LDialog.Filter      := FAppModules.Language.GetString('Rainfall.Filter');
      LDialog.FileName    := AFileName;
      LDialog.FilterIndex := 1;
      if ( LDialog.Execute ) then
        //RainfallGaugeSelectionDialog.RaingaugeGISPanel.GISViewer.Output_ExportMap ( 1, LDialog.FileName, 1 );
      finally
        FreeAndNil ( LDialog );
      end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TRainfallGaugeSelectionValidator.ReduceSecondAspectRatioToFirst(AX1, AY1 : integer; var AX2,AY2 : integer);
const OPNAME = 'TRainfallGaugeSelectionValidator.ReduceSecondAspectRatioToFirst';
var
  LAspectRatio : Double;
begin
  // This function "reduces" the denominator or numerator of second ratio ( AX2/AY2 )to give the same
  // ratio as the first ( AX1/AY2 ).
  try
    if (AY1 <> 0) and (AY2 <> 0) then
    begin
      LAspectRatio := (AX1 / AY1);
      if LAspectRatio > (AX2 / AY2) then
      begin
        AY2 := Trunc(AX2 / LAspectRatio);
      end else begin
        AX2 := Trunc(AY2 * LAspectRatio);
      end;
    end else begin
      AX2 := 0;
      AY2 := 0;
    end;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TRainfallGaugeSelectionValidator.DoCustomEvent (ACustomModelEvent : TModelMenuData): Boolean;
const OPNAME = 'TRainfallGaugeSelectionValidator.DoCustomEvent';
begin
  Result := FALSE;
  try
    if Assigned( ACustomModelEvent ) then
    begin
      Result := True;
      case ACustomModelEvent.Action of
        MeViewStatusBar          : DoViewStatusbar(nil);
        meToggleRainGaugeTree    : DoToggleTree(nil);
        MeUnSelectAll            : DoUnSelectAll(nil);
        MeUnSelect               : DoUnSelect(nil);
        MeSelectByStationNumber  : DoSelectByStationNumber(nil);
        MeSelectByStationName    : DoSelectByStationName(nil);
        MeSelectRect             : DoSelectByRect(nil);
        MeSelectByDistance       : DoSelectByDistance(nil);
        MeOptionAddToSelection   : DoSetAddToSelection(nil);
      else
      end;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TRainfallGaugeSelectionValidator.DoSelectedGaugesListBoxOnDrawItem( Control: TWinControl; Index: Integer; Rect: TRect;
                                                                              State: TOwnerDrawState);
const OPNAME = 'TRainfallGaugeSelectionValidator.DoSelectedGaugesListBoxOnDrawItem';
var
  LGauge : IRainGauge;
begin
  FCurrentStationID := (FAppModules.Model.ModelData as IRainfallModelData).CurrentStationID;
//  RainfallGaugeSelectionDialog.RaingaugeGISPanel.SelectedList
//  LGauge := (FAppModules.Model.ModelData as IRainfallModelData).GaugeList.GetGaugeByNumber(lGaugeNr)
  if ( LGauge.GaugeID = FCurrentStationID ) then
    RainfallGaugeSelectionDialog.SelectedGaugesListBox.Selected [ Index ] := True;

end;

procedure TRainfallGaugeSelectionValidator.DoClearUserData (Sender : TObject);
const OPNAME = 'TRainfallGaugeSelectionValidator.DoClearUserData';
var
  lGauge       : IRainGauge;
  lGaugeID     : integer;
  lGaugeNr     : string;
  lGaugeNrStar : string;
  lMessage     : string;
  lDataset     : TAbstractModelDataset;
  lSQL         : string;
begin
  try
    if Assigned((FAppModules.Model.ModelData as IRainfallModelData).GaugeList()) then
    begin
      with RainfallGaugeSelectionDialog do
      begin
        if (GaugeCheckListBox.ItemIndex < 0) then
        begin
          lMessage := FAppModules.Language.GetString('Rainfall.SelectUserImported') +
                      FAppModules.Language.GetString('Rainfall.InGaugeCheckListBox');
          ShowMessage(lMessage);
        end
        else
        begin
          lGaugeNrStar := GaugeCheckListBox.Items[GaugeCheckListBox.ItemIndex];
          if (Pos('*', lGaugeNrStar) > 0) then
            lGaugeNr := Copy(lGaugeNrStar, 1, Length(lGaugeNrStar) - 2)
          else
            lGaugeNr := lGaugeNrStar;
          lGauge := (FAppModules.Model.ModelData as IRainfallModelData).GaugeList.GetGaugeByNumber(lGaugeNr);
          if (lGauge <> nil) then
          begin
            lGaugeID := lGauge.GaugeID;
            lGauge   := nil;
            if (lGaugeID <= 100000) then
            begin
              lMessage := lGaugeNr + FAppModules.Language.GetString('Rainfall.NotUserImportedStation');
              ShowMessage(lMessage);
            end
            else
            begin
              FAppModules.Database.CreateDataset(integer(dtExecSQL), lDataset);
              try
                if (Assigned(lDataset)) then
                begin
                  lDataset.DataSet.Close;
                  lSQL := 'SELECT * FROM RainfallProjectGauges ' +
                          ' WHERE StationID = ' + IntToStr(lGaugeID);
                  lDataset.SetSQL(lSQL);
                  lDataset.Dataset.Open;
                  if (NOT lDataset.DataSet.Eof) then
                  begin
                    lMessage := lGaugeNr + FAppModules.Language.GetString('Rainfall.GaugeUsedMayNotBeDeleted') +
                                Trim(lDataset.Dataset.FieldByName('StudyAreaName').AsString) + ' : ' +
                                Trim(lDataset.Dataset.FieldByName('SubArea').AsString);
                    ShowMessage(lMessage);
                  end
                  else
                  begin
                    lMessage := FAppModules.Language.GetString('Rainfall.SureYouWantToDelete') + lGaugeNr +
                                FAppModules.Language.GetString('Rainfall.AllItsData');
                    if (MessageDlg(lMessage, mtConfirmation, [mbYes, mbNo], 0) = mrYes) then
                    begin
                      FAppModules.Database.StartTransaction;
                      try
                        lDataset.DataSet.Close;
                        lSQL := 'DELETE * FROM RainfallUserMonthlyData ' +
                                ' WHERE StationID = ' + IntToStr(lGaugeID);
                        lDataSet.SetSQL(lSQL);
                        lDataset.ExecSQL;

                        lDataset.DataSet.Close;
                        lSQL := 'DELETE * FROM RainfallUserStations ' +
                                ' WHERE StationID = ' + IntToStr(lGaugeID);
                        lDataSet.SetSQL(lSQL);
                        lDataset.ExecSQL;

                        lDataset.DataSet.Close;
                        FAppModules.Database.Commit;
                        FAppModules.Model.StudyDataHasChanged
                          (sdccDelete, FAppModules.Language.GetString('Rainfall.UserGauges'), '', '');
                      except
                        FAppModules.Database.Rollback;
                        raise;
                      end;
                    end;
                  end;
                end;
              finally
                if Assigned(lDataset) then
                begin
                  lDataset.Dataset.Close;
                  FreeAndNil(lDataset);
                end;
              end;
            end;
          end;
        end;
      end;
    end;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

end.


