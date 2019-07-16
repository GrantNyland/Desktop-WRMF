unit main;

interface

uses

  // Delphi
  Windows,
  SysUtils,
  Classes,
  Controls,
  Forms,
  Dialogs,
  Menus,
  ExtCtrls,
  StdCtrls,
  ImgList,
  CheckLst,
  ComCtrls,
  ToolWin,

  // DWAF - PDNA
  UAbstractGauge,
  UAbstractGaugeList,
  UMenuItemManager,
  URaingaugeGISPanel,
  URGProgressDialog,
  USearchDialog, System.ImageList;

type
  TFrmMain = class(TForm)
    StbStatus       : TStatusBar;
    TbrMainToolBar  : TToolBar;
    BtnLoad         : TToolButton;
    BtnSave         : TToolButton;
    BtnSelectAll    : TToolButton;
    BtnUnselectAll  : TToolButton;
    BtnInvertAll    : TToolButton;
    SepToolSection1 : TToolButton;
    SepToolSection2 : TToolButton;
    ImageList1: TImageList;
    BtnShowGauges: TToolButton;
    BtnClose: TToolButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FTrvGaugeListChange(Sender: TObject; Node: TTreeNode);
    procedure FClbGaugeSClick(Sender: TObject);
    procedure FClbGaugeSClickCheck(Sender: TObject);
    procedure BtnLoadClick(Sender: TObject);
    procedure BtnSaveClick(Sender: TObject);
    procedure BtnSelectAllClick(Sender: TObject);
    procedure BtnUnselectAllClick(Sender: TObject);
    procedure BtnInvertAllClick(Sender: TObject);
    procedure BtnCloseClick(Sender: TObject);
    procedure BtnUpdateGISViewer(Sender : TObject);
    procedure ShowSelectionCount;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
  private
    procedure ProcessStationMonthlyFile(AStringList : TStringList);
    procedure ShiftMonths(AStringListIn : TStringList; AStringListOut : TStringList);
    procedure CalculateYearTotals(AStringList : TStringList);
  protected
    FPanelForTreeView : TPanel;
    FPanelForSelectedList : TPanel;
    FPanelForGISViewer : TPanel;

    FLblTreeview : TLabel;
    FLblSelectedList : TLabel;
    FLblGisViewer : TLabel;

    FSplTreeView : TSplitter;
    FSplSelectedGauges  : TSplitter;
    FSplTreeviewAndListBox : TSplitter;

    FPnlSelectedGauges : TPanel;
    FClbGaugeS         : TCheckListBox;
    FTrvGaugeList      : TTreeView;
    FLbxSelectedGauges : TListBox;

    FReplaceSelection  : boolean;
    FUpdateGISLive     : boolean;
    FRaingaugeGISPanel : TRaingaugeGISPanel;

    FBtnUpdateGISViewer : TButton;

    FSearchDialog : TRGSearchDialog;
    FSelectionHasChanged : boolean;
    FSelectionIsDirty : boolean;
    FMenuItemManager : TMenuItemManager;
    FGaugeList : TAbstractGaugeList;
    FOpenDialog : TOpenDialog;
    FSaveDialog : TSaveDialog;

    FMessage : string;
    procedure PopulateTreeView(ARGProgressDialog : TRGProgressDialog = nil; AShowprogress : Boolean = true);
    procedure UpdateStatusPanel;
    procedure SplitMonthlyFile;
    procedure DoProcessMenu(Sender : TObject);
    procedure DoLoadFile(Sender: TObject);
    procedure DoDailyExtract(Sender: TObject);
    procedure DoMonthlyExtract(Sender: TObject);
    procedure DoAutoLoadFile(AFilename : string);
    procedure DoSaveSelection(Sender: TObject);
    procedure DoExitProgram(Sender: TObject);
    procedure DoSetAddToSelection(Sender : TObject);
    procedure DoSelectAll;
    procedure DoUnSelectAll;
    procedure DoInvertSelection;
    procedure DoSelectFromfile;
    procedure DoSelectByStationNumber;
    procedure DoSelectByStationName;
    procedure DoSelectByRect;
    procedure DoSelectByBuffer;
    procedure DoViewToolBar(Sender : TObject);
    procedure DoViewStatusbar(Sender : TObject);
    procedure DoHelpContents;
    procedure DoHelpAbout;
    procedure DoExtractListOfStations(ASender : TObject);
    procedure OnKeyDownListBox(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure SelectionHasChanged(Sender : TObject);
    procedure SynchroniseCheckList;
    procedure SynchroniseGISViewer;
    procedure SynchroniseSelectionList;
    procedure DoGisSelectionHasChanged(Sender : TObject);

    procedure CreateMemberObjects;
    procedure InitialiseMemberObjects;
    procedure DestroyMemberObjects;
  public
    { Public declarations }
  end;

var
  FrmMain: TFrmMain;

implementation

{$R *.DFM}


uses

  // Delphi
  Inifiles,
  Math,
  UShellExecuteObject,

  // DWAF - arivia.kom
  UExtractData,
  UErrorHandlingOperations,
  UGaugeList,
  USplashScreen,
  ULoadingProgressDialog;

procedure TFrmMain.DoExitProgram(Sender: TObject);
const OPNAME = 'TFrmMain.DoExitProgram';
begin
  try
    Close;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TFrmMain.FormCreate(Sender: TObject);
const OPNAME = 'TFrmMain.FormCreate';
var
  LSplashScreen : TSplashScreen;
  LAutoLoadFileName : string;
  LIniFileWidth  : integer;
  LIniFileHeight : integer;
  LIniFileTop    : integer;
  LIniFileLeft   : integer;
begin
  try
    Self.Caption := Application.Title;
    LSplashScreen := TSplashScreen.Create;
    CreateMemberObjects;
    InitialiseMemberObjects;
    try
      LSplashScreen.SplashScreenMode;
      LAutoLoadFileName := '';
      FOpenDialog := TOpenDialog.Create(Self);
      FSaveDialog := TSaveDialog.Create(Self);
      FOpenDialog.InitialDir := ExtractFilePath(Application.ExeName);
      FSaveDialog.InitialDir := ExtractFilePath(Application.ExeName);
      LSplashScreen.AddProgressString(FAppModules.Language.GetString('Message.ApplicationLoadingStarted'));
      Application.HelpFile := ExtractFilePath(Application.ExeName) + 'Help\RainGaugeGui.hlp';
      FSelectionHasChanged := false;
      FMenuItemManager := TMenuItemManager.Create(Self);
      if Assigned(FMenuItemManager) then
        FMenuItemManager.OnMenuClick := DoProcessMenu;

      with TIniFile.Create(ExtractFilePath(Application.ExeName) + 'GaugeSelector.ini') do
      begin
        FReplaceSelection := ReadBool(Self.ClassName,'ReplaceSelection',True);
        FUpdateGISLive := ReadBool(Self.ClassName,'UpdateGISViewerLive',True);
        LAutoLoadFileName := ReadString(Self.ClassName,'AutoLoadFileName','');
        LIniFileWidth := ReadInteger(Self.ClassName,'FormWidth',Screen.Width);
        LIniFileHeight := ReadInteger(Self.ClassName,'FormHeight',Screen.Height);
        LIniFileTop := ReadInteger(Self.ClassName,'FormTop', 0);
        LIniFileLeft := ReadInteger(Self.ClassName,'FormLeft', 0);
        Free;
      end;
      Self.Width := Max(160, Min(LIniFileWidth, Screen.Width));
      Self.Height := Max(120, Min(LIniFileHeight, Screen.Height));
      Self.Top := Max(0, Min(LIniFileTop, Screen.Height - Self.Height));
      Self.Left := Max(0, Min(LIniFileLeft, Screen.Width - Self.Width));
      // Begin Hack -- should be using constants or functions here
      FMenuItemManager.SetMenuState('View','Toolbar',MsChecked);
      FMenuItemManager.SetMenuState('View','Status Bar',MsChecked);
      FMenuItemManager.SetMenuState('Options','Replace selection',MsUnChecked);
      case FReplaceSelection of
        true : FMenuItemManager.SetMenuState('Options','Replace selection',MsChecked);
        false : FMenuItemManager.SetMenuState('Options','Replace selection',MsUnChecked);
      end;
      case FUpdateGISLive of
        true : FMenuItemManager.SetMenuState('Options','Update GIS Viewer Live',MsChecked);
        false : FMenuItemManager.SetMenuState('Options','Update GIS Viewer Live',MsUnChecked);
      end;
      // End Hack
      FTrvGaugeList.ReadOnly := true;
      FGaugeList := TGaugeList.Create;
      if (LAutoLoadFileName <> '') and (FileExists(LAutoLoadFileName)) then
      begin
        LSplashScreen.AddProgressString(FAppModules.Language.GetString('Message.CurrentlyLoadingListOfGauges'));
        DoAutoLoadFile(LAutoLoadFileName);
        LSplashScreen.AddProgressString(FAppModules.Language.GetString('Message.FinishedLoadingListOfGauges'));
      end;
      FGaugeList.OnSelectionChange := SelectionHasChanged;
      LSplashScreen.AddProgressString(FAppModules.Language.GetString('Message.InitialisingGISViewer'));
      FRaingaugeGISPanel := TRaingaugeGISPanel.Create(FPnlSelectedGauges);
      if Assigned(FRaingaugeGISPanel) then
      begin
        FRaingaugeGISPanel.Parent := FPanelForGISViewer;
        FRaingaugeGISPanel.Align := alClient;
        FRaingaugeGISPanel.Visible := false;
        FRaingaugeGISPanel.Align := AlClient;
        FRaingaugeGISPanel.ToggleView;
        FRaingaugeGISPanel.OnGISSelectionChanged := DoGisSelectionHasChanged;
        FRaingaugeGISPanel.Refresh;
        FRaingaugeGISPanel.Visible := True;
      end;
      LSplashScreen.AddProgressString(FAppModules.Language.GetString('Message.FinishedInitialisingGISViewer'));
      UpdateStatusPanel;
    finally
      LSplashScreen.HideForm;
      FreeAndNil(LSplashScreen);
    end;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TFrmMain.FormDestroy(Sender: TObject);
const OPNAME = 'TFrmMain.FormDestroy';
begin
  try
    DestroyMemberObjects;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TFrmMain.DoLoadFile(Sender: TObject);
const OPNAME = 'TFrmMain.DoLoadFile';
var
  LRGProgressDialog : TRGProgressDialog;
begin
  try
    if Assigned(FGaugeList) then
    begin
      StbStatus.Panels[0].Text := '';
      StbStatus.Panels[1].Text := '';
      StbStatus.Panels[2].Text := '';
      StbStatus.Panels[3].Text := FAppModules.Language.GetString('Rainfall.Nodes');
      FOpenDialog.DefaultExt := 'csv';
      FOpenDialog.FileName := '';
      FOpenDialog.Filter := 'Comma separated values (*.csv)|*.csv|Text Files (*.txt)|*.txt|All files (*.*)|*.*';
      FOpenDialog.Execute;
      if FOpenDialog.FileName <> '' then
      begin
        LRGProgressDialog := TRGProgressDialog.Create;
        try
          LRGProgressDialog.ShowLabel;
          LRGProgressDialog.ShowForm;
          LRGProgressDialog.SetLabelCaption(FAppModules.Language.GetString('LabelText.LoadingFile'));
          FGaugeList.LoadFileName := FOpenDialog.FileName;
          FGaugeList.LoadFromFile();
          LRGProgressDialog.ShowProgressBar;
          PopulateTreeView(LRGProgressDialog, true);
        finally
          if Assigned(LRGProgressDialog) then
          begin
            LRGProgressDialog.HideForm;
            LRGProgressDialog.Free;
          end;
        end;
        FGaugeList.DeSelectAll;
        if FGaugeList.TotalCount = 0 then
          ShowMessage(FAppModules.Language.GetString('Message.NoGauges'));
        else
          ShowMessage(Format('%d Gauge(s) were loaded.',[FGaugeList.TotalCount]));
      end;
      if Assigned(FRaingaugeGISPanel) then
      begin
        FRaingaugeGISPanel.Refresh;
        FRaingaugeGISPanel.Visible := True;
      end;
      StbStatus.Panels[3].Text := '';
    end;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TFrmMain.DoSaveSelection(Sender: TObject);
const OPNAME = 'TFrmMain.DoSaveSelection';
var
  LSavedDirectory : string;
begin
  try
    if Assigned(FSaveDialog) then
    begin
      LSavedDirectory := GetCurrentDir;
      FSaveDialog.DefaultExt := '.txt';
      FSaveDialog.Filter := 'Comma separated values (*.csv)|*.csv|Text Files (*.txt)|*.txt|All files (*.*)|*.*';
      FSaveDialog.FileName := '';
      FSaveDialog.Execute;
      if Assigned(FGaugeList) and (FSaveDialog.FileName<> '') then
      begin
        FGaugeList.SaveFileName := FSaveDialog.FileName;
        FGaugeList.SaveToFile;
        SetCurrentDir(LSavedDirectory);
        FSelectionIsDirty := false;
      end;
    end;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TFrmMain.PopulateTreeView(ARGProgressDialog : TRGProgressDialog = nil;
  AShowProgress : boolean = true);
const OPNAME = 'TFrmMain.PopulateTreeView';
var
  LIndex         : integer;
  LMaxNodes      : integer;
  LGauge         : TAbstractGauge;
  LPrevGroup     : string;
  LPrevSubGroup  : string;
  LCurrNode      : TTreeNode;
begin
  try
    LMaxNodes := FGaugeList.TotalCount;
    StbStatus.Panels[3].Text := FAppModules.Language.GetString('Rainfall.ReadFile');
    FGaugeList.SetSortOption(glsoGroup);
    LPrevGroup := '';
    LCurrNode := nil;
    FTrvGaugeList.Items.Clear;
    FTrvGaugeList.Items.BeginUpdate;
    try
      if AShowprogress then
        if LMaxNodes > 0 then
        begin
          if Assigned(ARGProgressDialog) then
          begin
            ARGProgressDialog.Position := 0;
            ARGProgressDialog.MaxProgress := LMaxNodes - 1;
            ARGProgressDialog.ShowForm;
          end;
        end;
      for LIndex := 0 to LMaxNodes - 1 do
      begin
        if AShowprogress then
          if Assigned(ARGProgressDialog) then
            ARGProgressDialog.Position := LIndex;
        LGauge := TAbstractGauge(FGaugeList.GetItem(LIndex));
        if LPrevGroup <> LGauge.Group then
        begin
          LCurrNode := FTrvGaugeList.Items.Add(nil,LGauge.Group);
          LPrevGroup := LGauge.Group;
          LPrevSubGroup := '';
        end;
        if (LGauge.Group = LPrevGroup) then
        begin
          if (LPrevSubGroup <> LGauge.SubGroup) then
          begin
            FTrvGaugeList.Items.AddChild(LCurrNode, LGauge.SubGroup);
            LPrevSubGroup := LGauge.SubGroup
          end;
        end;
        StbStatus.Panels[3].Text := Format(' Adding node %d of %d',[LIndex + 1,LMaxNodes]);
        StbStatus.Update;
      end;
    finally
      StbStatus.Panels[3].Text := '';
      FTrvGaugeList.Items.EndUpdate;
    end;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TFrmMain.DoSelectAll;
const OPNAME = 'TFrmMain.DoSelectAll';
begin
  try
    if Assigned(FGaugeList) then
      if (FGaugeList.TotalCount = 0) then
      begin
        ShowMessage(FMessage);
      end
      else
      begin
        FGaugeList.SelectAll;
        SelectionHasChanged(nil);
      end;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TFrmMain.DoInvertSelection;
const OPNAME = 'TFrmMain.DoInvertSelection';
begin
  try
    if Assigned(FGaugeList) then
      if FGaugeList.TotalCount = 0 then
        ShowMessage(FMessage)
      else
      begin
        FGaugeList.InvertSelection;
        SelectionHasChanged(nil);
      end;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TFrmMain.DoUnSelectAll;
const OPNAME = 'TFrmMain.DoUnSelectAll';
begin
  try
    if Assigned(FGaugeList) then
      if FGaugeList.TotalCount = 0 then
        ShowMessage(FMessage)
      else
      begin
        FGaugeList.DeSelectAll;
        SelectionHasChanged(nil);
      end;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TFrmMain.DoSelectByBuffer;
const OPNAME = 'TFrmMain.DoSelectByBuffer';
var
  LPoint : TPoint;
begin
  try
    try
      FSearchDialog := TRGSearchDialog.Create(StBuffer, Application.Title);
      FSearchDialog.ShowFormModal(StBuffer);
      LPoint.X := FSearchDialog.TopValue;
      LPoint.Y := FSearchDialog.LeftValue;
      FGaugeList.SelectByClosestPoint(LPoint,FSearchDialog.BufferValue,FReplaceSelection);
    finally
      FSearchDialog.Free;
    end;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TFrmMain.DoSelectByStationNumber;
const OPNAME = 'TFrmMain.DoSelectByStationNumber';
begin
  try
    try
      FSearchDialog := TRGSearchDialog.Create(StStationNumber, Application.Title);
      FSearchDialog.ShowFormModal(StStationNumber);
      if FSearchDialog.StringValue <> '' then
        FGaugeList.SelectByStationNumber(FSearchDialog.StringValue, FReplaceSelection);
    finally
      FSearchDialog.Free;
    end;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TFrmMain.DoSelectByRect;
const OPNAME = 'TFrmMain.DoSelectByRect';
var
  LRect : TRect;
begin
  try
    try
      FSearchDialog := TRGSearchDialog.Create(StRect, Application.Title);
      FSearchDialog.ShowFormModal(StRect);
      try
        LRect.Top := FSearchDialog.TopValue;
        LRect.Left := FSearchDialog.LeftValue;
        LRect.Bottom := FSearchDialog.BottomValue;
        LRect.Right := FSearchDialog.RightValue;
        FGaugeList.SelectByCoOrdinateWindow(LRect,FReplaceSelection);
      except
        ShowMessage(FAppModules.Language.GetString('Message.InvalidCoordinateEntered'));
      end;
    finally
      FSearchDialog.Free;
    end;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TFrmMain.DoSelectByStationName;
const OPNAME = 'TFrmMain.DoSelectByStationName';
begin
  try
    try
      FSearchDialog := TRGSearchDialog.Create(StStationName, Application.Title);
      FSearchDialog.ShowFormModal(StStationName);
      FGaugeList.SelectByStationName(FSearchDialog.StringValue,FReplaceSelection);
    finally
      FSearchDialog.Free;
    end;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TFrmMain.UpdateStatusPanel;
const OPNAME = 'TFrmMain.UpdateStatusPanel';
begin
  try
    if Assigned(FGaugeList) then
    begin
      StbStatus.Panels[0].Text := Format('Total = %d',[FGaugeList.TotalCount]);
      StbStatus.Panels[1].Text := Format('Selected = %d',[FGaugeList.SelectedCount]);
      StbStatus.Panels[2].Text := Format('Unselected = %d',[FGaugeList.UnselectedCount]);
      StbStatus.Panels[3].Text := '';
    end
    else
    begin
      StbStatus.Panels[0].Text := Format('Total = %d',[0]);
      StbStatus.Panels[1].Text := Format('Selected = %d',[0]);
      StbStatus.Panels[2].Text := Format('Unselected = %d',[0]);
      StbStatus.Panels[3].Text := '';
    end;
    StbStatus.Update;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TFrmMain.DoViewStatusbar(Sender : TObject);
const OPNAME = 'TFrmMain.DoViewStatusbar';
begin
  try
    TMenuItem(Sender).Checked := not TMenuItem(Sender).Checked;
      StbStatus.Visible := TMenuItem(Sender).Checked;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TFrmMain.DoViewToolBar(Sender : TObject);
const OPNAME = 'TFrmMain.DoViewToolBar';
begin
  try
    TMenuItem(Sender).Checked := not TMenuItem(Sender).Checked;
    TbrMainToolBar.Visible := TMenuItem(Sender).Checked;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TFrmMain.DoProcessMenu(Sender: TObject);
const OPNAME = 'TFrmMain.DoProcessMenu';
begin
  try
    if Assigned(Sender) then
    begin
      case TRGMenuItem(Sender).MenuEvent of
        MeFile                   : ;
        MeEdit                   : ;
        MeView                   : ;
        MeSelect                 : ;
        MeOption                 : ;
        MeHelp                   : ;
        MeFileLoad               : DoLoadFile(Sender);
        MeDailyExtract           : DoDailyExtract(Sender);
        MeMonthlyExtract         : DoMonthlyExtract(Sender);
        MeFileSave               : DoSaveSelection(Sender);
        MeFileExit               : DoExitProgram(Sender);
        MeViewStatusBar          : DoViewStatusbar(Sender);
        MeViewToolBar            : DoViewToolBar(Sender);
        MeSelectAll              : DoSelectAll;
        MeInvertSelection        : DoInvertSelection;
        MeUnSelectAll            : DoUnSelectAll;
        MeSelectFromFile         : DoSelectFromfile;
        MeSelectByStationNumber  : DoSelectByStationNumber;
        MeSelectByStationName    : DoSelectByStationName;
        MeSelectRect             : DoSelectByRect;
        MeSelectByBuffer         : DoSelectByBuffer;
        MeOptionAddToSelection   : DoSetAddToSelection(Sender);
        MeOptionUpdateGISLive    : ShowMessage(FAppModules.Language.GetString('Message.OptionNotImplemented') + #10 + #13 +
                                               FAppModules.Language.GetString('Message.ImplementWhenGISViewerImproves'));
        MeHelpContents           : DoHelpContents;
        MeHelpAbout              : DoHelpAbout;
      else
        ShowMessage(FAppModules.Language.GetString('Message.MenuClickNotHandled'));
      end;
      UpdateStatusPanel;
      if FSelectionHasChanged then
      begin
        SynchroniseCheckList;
        SynchroniseSelectionList;
      end;
      if TRGMenuItem(Sender).MenuEvent in [
        MeSelectFromFile, MeSelectAll, MeInvertSelection, MeUnSelectAll, MeSelectFromFile,
        MeSelectByStationNumber, MeSelectByStationName, MeSelectRect, MeSelectByBuffer] then
          if Assigned(FRaingaugeGISPanel) then
            SynchroniseGISViewer;
    end;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TFrmMain.FTrvGaugeListChange(Sender: TObject; Node: TTreeNode);
const OPNAME = 'TFrmMain.FTrvGaugeListChange';
var
  LIndex : integer;
  LAddedIndex : integer;
  LGauge : TAbstractGauge;
begin
  try
    if Assigned(FTrvGaugeList.Selected) then
    begin
      FClbGaugeS.Items.Clear;
      if (FTrvGaugeList.Selected.Level = 1) then
      begin
        for LIndex := 0 to FGaugeList.TotalCount - 1 do
        begin
          LGauge := TAbstractGauge(FGaugeList.GetItem(LIndex));
          if (UpperCase(FTrvGaugeList.Selected.Parent.Text) = UpperCase(LGauge.Group)) then
          begin
            if (UpperCase(FTrvGaugeList.Selected.Text) = UpperCase(LGauge.SubGroup))
              or (Trim(LGauge.SubGroup) = '') then
            begin
              LAddedIndex := FClbGaugeS.Items.Add(LGauge.GaugeNumber);
              FClbGaugeS.Checked[LAddedIndex] :=  LGauge.IsSelected;
            end;
          end;
        end;
      end;
    end;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TFrmMain.FClbGaugeSClick(Sender: TObject);
const OPNAME = 'TFrmMain.FClbGaugeSClick';
begin
  try

  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TFrmMain.FClbGaugeSClickCheck(Sender: TObject);
const OPNAME = 'TFrmMain.FClbGaugeSClickCheck';
var
  LGauge : TAbstractGauge;
begin
  try
    if Assigned(FGaugeList) then
    begin
      LGauge := TAbstractGauge(FGaugeList.GetItem(FClbGaugeS.Items[FClbGaugeS.ItemIndex]));
      LGauge.SetIsSelected(FClbGaugeS.Checked[FClbGaugeS.ItemIndex]);
      UpdateStatusPanel;
      case FClbGaugeS.Checked[FClbGaugeS.ItemIndex] of
        true : begin
                 FRaingaugeGISPanel.AddToSelection(FClbGaugeS.Items[FClbGaugeS.ItemIndex]);
                 if FLbxSelectedGauges.Items.IndexOf(FClbGaugeS.Items[FClbGaugeS.ItemIndex]) < 0 then
                   FLbxSelectedGauges.Items.Add(FClbGaugeS.Items[FClbGaugeS.ItemIndex]);
               end;
        false : begin
                  FRaingaugeGISPanel.RemoveFromSelection(FClbGaugeS.Items[FClbGaugeS.ItemIndex]);
                  if FLbxSelectedGauges.Items.IndexOf(FClbGaugeS.Items[FClbGaugeS.ItemIndex]) >= 0 then
                    FLbxSelectedGauges.Items.Delete(FLbxSelectedGauges.Items.IndexOf(FClbGaugeS.Items[FClbGaugeS.ItemIndex]));
                end;
      end;
      // SynchroniseSelectionList;
      SynchroniseGISViewer;
      FSelectionHasChanged := true;
    end;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TFrmMain.SelectionHasChanged (Sender : TObject);
const OPNAME = 'TFrmMain.SelectionHasChanged';
begin
  try
    FSelectionHasChanged := true;
    FSelectionIsDirty := true;
  except on E:Exception do HandleError(E,OPNAME) end;
end;

procedure TFrmMain.SynchroniseCheckList;
const OPNAME = 'TFrmMain.SynchroniseCheckList';
var
  LIndex : integer;
begin
  try
    if Assigned(FGaugeList) then
    begin
      for LIndex := 0 to FClbGaugeS.Items.Count - 1 do
        FClbGaugeS.Checked[LIndex] := TAbstractGauge(FGaugeList.GetItem(FClbGaugeS.Items[LIndex])).IsSelected;
      FSelectionHasChanged := false;
    end;
  except on E:Exception do HandleError(E,OPNAME) end;
end;

procedure TFrmMain.DoHelpAbout;
const OPNAME = 'TFrmMain.DoHelpAbout';
begin
  try
    with TSplashScreen.Create do
    begin
      try
        ShowFormModal;
        HideForm;
      finally
        Free;
      end;
    end;
  except on E:Exception do HandleError(E,OPNAME) end;
end;

procedure TFrmMain.DoHelpContents;
const OPNAME = 'TFrmMain.DoHelpContents';
begin
  try
    Application.HelpContext(0);
  except on E:Exception do HandleError(E,OPNAME) end;
end;

procedure TFrmMain.DoSelectFromfile;
const OPNAME = 'TFrmMain.DoSelectFromfile';
var
  LSavedDirectory  : string;
  LStringList      : TStringList;
  LIndex           : integer;
  LGauge           : TAbstractGauge;
  LTotalCount      : integer;
  LFileFoundIndex  : integer;
  LStringListCount : integer;
begin
  try
    // We should have 2 possible strategies here
    // if file size >x% of FGaugeList.Totalcount Loop through GaugeList looking for gauges in file FileStringList
    // if file size <x% of FGaugeList.Totalcount Loop through FileStringList looking for gauges in FGaugeList
    // x to be determined by by trial and error
    if Assigned(FGaugeList) then
    begin
      LSavedDirectory := GetCurrentDir;
      LStringList := TStringList.Create;
      try
        FOpenDialog.FileName := '';
        FOpenDialog.DefaultExt := 'csv';
        FOpenDialog.Filter := 'Comma separated values (*.csv)|*.csv|Text Files (*.txt)|*.txt|All files (*.*)|*.*';
        FOpenDialog.Execute;
        if FOpenDialog.FileName <> '' then
        begin
          LStringList.LoadFromFile(FOpenDialog.FileName);
          LStringList.Sorted := true;
          LStringList.Duplicates := dupIgnore;

          // LogSilentError('Started Running Through Gauge List(GetItem by Index) ','');
          LTotalCount := FGaugeList.TotalCount;
          LStringListCount := LStringList.Count;

          for LIndex := 0 to LTotalCount - 1 do
          begin
            if (LIndex + 1) mod 100 = 0 then
            begin
              StbStatus.Panels[3].Text := Format('Procesing Gauge %d of %d',[LIndex + 1, LTotalCount]);
              StbStatus.Update;
            end;
            if LStringListCount > 0 then
            begin
              LGauge := TAbstractGauge(FGaugeList.GetItem(LIndex));
              if Assigned(LGauge) then
              begin
                LFileFoundIndex := LStringList.IndexOf(LGauge.GaugeNumber);
                if LFileFoundIndex >= 0 then
                begin
                  LGauge.Select;
                  LStringList.Delete(LFileFoundIndex);
                  Dec(LStringListCount);
                end;
              end;
            end;
          end;
          StbStatus.Panels[3].Text := Format('Procesing Gauge %d of %d',[LTotalCount, LTotalCount]);
          StbStatus.Update;
        end;
      finally
        FreeAndNil(LStringList);
        SetCurrentDir(LSavedDirectory);
      end;
      StbStatus.Panels[3].Text := '';
      UpdateStatusPanel;
    end;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TFrmMain.BtnLoadClick(Sender: TObject);
const OPNAME = 'TFrmMain.BtnLoadClick';
begin
  try
    DoLoadFile(nil);
    UpdateStatusPanel;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TFrmMain.BtnSaveClick(Sender: TObject);
const OPNAME = 'TFrmMain.BtnSaveClick';
begin
  try
    DoSaveSelection(nil);
    UpdateStatusPanel;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TFrmMain.BtnSelectAllClick(Sender: TObject);
const OPNAME = 'TFrmMain.BtnSelectAllClick';
begin
  try
    DoSelectAll;
    UpdateStatusPanel;
    if FSelectionHasChanged then
    begin
      SynchroniseCheckList;
      SynchroniseSelectionList;
    end;
    if Assigned(FRaingaugeGISPanel) then
      SynchroniseGISViewer;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TFrmMain.BtnUnselectAllClick(Sender: TObject);
const OPNAME = 'TFrmMain.BtnUnselectAllClick';
begin
  try
    DoUnSelectAll;
    UpdateStatusPanel;
    if FSelectionHasChanged then
    begin
      SynchroniseCheckList;
      SynchroniseSelectionList;
    end;
    if Assigned(FRaingaugeGISPanel) then
      SynchroniseGISViewer;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TFrmMain.BtnInvertAllClick(Sender: TObject);
const OPNAME = 'TFrmMain.BtnInvertAllClick';
begin
  try
    DoInvertSelection;
    UpdateStatusPanel;
    if FSelectionHasChanged then
    begin
      SynchroniseCheckList;
      SynchroniseSelectionList;
    end;
    if Assigned(FRaingaugeGISPanel) then
      SynchroniseGISViewer;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TFrmMain.BtnCloseClick(Sender: TObject);
const OPNAME = 'TFrmMain.BtnCloseClick';
begin
  try
    DoExitProgram(nil);
    UpdateStatusPanel;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TFrmMain.DoGisSelectionHasChanged(Sender: TObject);
const OPNAME = 'TFrmMain.DoGisSelectionHasChanged';
var
  LIndex : integer;
  LGauge : TAbstractGauge;
  LProgressDialog : TRGProgressDialog;
  LStringList : TStringList;
  LTotalCount : integer;
  LStringListCount : integer;
  LFoundIndex : integer;
begin
  try
    if Assigned(FGaugeList) and Assigned(FRaingaugeGISPanel.SelectedList) then
    begin
      LProgressDialog := TRGProgressDialog.Create;
      LStringList := TStringList.Create;
      try
        LStringList.Sorted := true;
        LStringList.Text := FRaingaugeGISPanel.SelectedList.Text;

        LTotalCount := FGaugeList.TotalCount;
        LStringListCount := LStringList.Count;

        LProgressDialog.MaxProgress := LTotalCount;
        LProgressDialog.ShowProgressBar;
        LProgressDialog.ShowForm;
        FGaugeList.DeSelectAll;

        for LIndex := 0 to LTotalCount - 1 do
        begin
          if (LIndex + 1) mod 100 = 0 then
            LProgressDialog.Position := LIndex;
          if LStringListCount > 0 then
          begin
            LGauge := TAbstractGauge(FGaugeList.GetItem(LIndex));
            if Assigned(LGauge) then
            begin
              LFoundIndex := LStringList.IndexOf(LGauge.GaugeNumber);
              if LFoundIndex >= 0 then
              begin
                LGauge.Select;
                LStringList.Delete(LFoundIndex);
                Dec(LStringListCount);
              end;
            end;
          end;
        end;
        LProgressDialog.Position := FRaingaugeGISPanel.SelectedList.Count;
        LProgressDialog.HideForm;
      finally
        FreeAndNil(LStringList);
        FreeAndNil(LProgressDialog);
      end;
    end;
    UpdateStatusPanel;
    SynchroniseCheckList;
    SynchroniseSelectionList;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TFrmMain.ShowSelectionCount;
const OPNAMe = 'TFrmMain.ShowSelectionCount';
begin
  try
    if Assigned(FGaugeList) then
      ShowMessage(Format('%d Gauge(s) selected.',[FGaugeList.SelectedCount]));
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TFrmMain.DoSetAddToSelection(Sender : TObject);
const OPNAME = 'TFrmMain.DoSetAddToSelection';
begin
  try
    TMenuItem(Sender).Checked := not TMenuItem(Sender).Checked;
    FReplaceSelection := TMenuItem(Sender).Checked;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TFrmMain.DoAutoLoadFile(AFileName : string);
const OPNAME = 'TFrmMain.DoAutoLoadFile';
begin
  try
    if Assigned(FGaugeList) then
    begin
      StbStatus.Panels[0].Text := '';
      StbStatus.Panels[1].Text := '';
      StbStatus.Panels[2].Text := '';
      StbStatus.Panels[3].Text := FAppModules.Language.GetString('Rainfall.Nodes');
      if Sysutils.FileExists(AFilename) then
      begin
        FGaugeList.LoadFileName := AFilename;
        FGaugeList.LoadFromFile();
        FGaugeList.DeSelectAll;
        PopulateTreeView(nil, false);
        if FGaugeList.TotalCount = 0 then
          ShowMessage(FAppModules.Language.GetString('Message.NoGauges'))
        else
        begin
          if Assigned (FRaingaugeGISPanel) then
          begin
            FRaingaugeGISPanel.Refresh;
            FRaingaugeGISPanel.Visible := True;
          end;
        end;
      end;
      StbStatus.Panels[3].Text := '';
    end;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TFrmMain.SynchroniseGISViewer;
const OPNAME = 'TFrmMain.SynchroniseGISViewer';
var
  LIndex : integer;
  LGauge : TAbstractGauge;
begin
  try
    if Assigned(FGaugeList) and Assigned(FRaingaugeGISPanel) and FUpdateGISLive then
    begin
      FRaingaugeGISPanel.SelectedList.Clear;
      for LIndex := 0 to FGaugeList.TotalCount do
      begin
        LGauge := TAbstractGauge(FGaugeList.GetItem(LIndex));
        if Assigned(LGauge) then
          if LGauge.IsSelected then
            FRaingaugeGISPanel.SelectedList.Add(LGauge.GaugeNumber);
      end;
      FRaingaugeGISPanel.UpdateFromList;
    end;
  except on E:Exception do HandleError(E,OPNAME) end;
end;

procedure TFrmMain.FormClose(Sender: TObject; var Action: TCloseAction);
const OPNAME = 'TFrmMain.FormClose';
begin
  try
    with TIniFile.Create(ExtractFilePath(Application.ExeName) + 'GaugeSelector.ini') do
    begin
      try
        WriteInteger(Self.ClassName,'FormWidth', Self.Width);
        WriteInteger(Self.ClassName,'FormHeight', Self.Height);
        WriteInteger(Self.ClassName,'FormTop', Self.Top);
        WriteInteger(Self.ClassName,'FormLeft', Self.Left);
        Free;
      except
      end;
    end;
  except on E:Exception do HandleError(E,OPNAME) end;
end;

procedure TFrmMain.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
const OPNAME = 'TFrmMain.FormCloseQuery';
begin
  try
    CanClose := false;
    if FSelectionIsDirty then
      CanClose :=  MessageDlg(FAppModules.Language.GetString('Message.CurrentSelectionNotSaved'),
        mtConfirmation, [mbYes,mbNo],0) = mrYes;
    if CanClose then
      DoSaveSelection(nil);
    CanClose := true;
  except on E:Exception do HandleError(E,OPNAME) end;
end;

procedure TFrmMain.CreateMemberObjects;
const OPNAME = 'TFrmMain.CreateMemberObjects';
begin
  try
    FPanelForTreeView := TPanel.Create(Self);
    FPanelForSelectedList := TPanel.Create(Self);
    FPanelForGISViewer := TPanel.Create(Self);

    FSplTreeView := TSplitter.Create(Self);
    FSplSelectedGauges := TSplitter.Create(Self);
    FSplTreeviewAndListBox := TSplitter.Create(FPanelForTreeView);

    FLblTreeview := TLabel.Create(FPanelForTreeView);
    FLblSelectedList := TLabel.Create(FPanelForSelectedList);
    FLblGisViewer := TLabel.Create(FPanelForGISViewer);

    FTrvGaugeList := TTreeView.Create(FPanelForTreeView);
    FLbxSelectedGauges := TListBox.Create(FPanelForTreeView);
    FClbGaugeS := TCheckListBox.Create(FPanelForSelectedList);
    FPnlSelectedGauges := TPanel.Create(FPanelForGISViewer);

    FBtnUpdateGISViewer := TButton.Create(FPanelForGISViewer);
  except on E:Exception do HandleError(E,OPNAME) end;
end;

procedure TFrmMain.InitialiseMemberObjects;
const OPNAME = 'TFrmMain.InitialiseMemberObjects';
begin
  try
    FPanelForTreeView.Parent := Self;
    FPanelForSelectedList.Parent := Self;
    FPanelForGISViewer.Parent := Self;

    FSplTreeView.Parent := Self;
    FSplSelectedGauges.Parent := Self;
    FSplTreeviewAndListBox.Parent := FPanelForTreeView;

    FLblTreeview.Parent := FPanelForTreeView;
    FLblSelectedList.Parent := FPanelForSelectedList;
    FLblGisViewer.Parent := FPanelForGISViewer;

    FTrvGaugeList.Parent := FPanelForTreeView;
    FLbxSelectedGauges.Parent := FPanelForSelectedList;
    FClbGaugeS.Parent := FPanelForTreeView;

    FBtnUpdateGISViewer.Parent := FPanelForGISViewer;

    FLbxSelectedGauges.MultiSelect := True;

    FMessage := FAppModules.Language.GetString('TFrmMain.strGaugesNotLoaded');
    FLblTreeview.Caption := FAppModules.Language.GetString('TreeViewCaption.GaugeGroup');
    FLblSelectedList.Caption := FAppModules.Language.GetString('LabelText.GaugeSelection');
    FLblGisViewer.Caption := FAppModules.Language.GetString('LabelText.SelectedAndUnselectedGauges');
    FLblTreeview.Align := alTop;
    FLblSelectedList.Align := alTop;
    FLblGisViewer.Align := alTop;

    FTrvGaugeList.Align := alTop;
    FSplTreeviewAndListBox.Top := FTrvGaugeList.Top + FTrvGaugeList.Height + 1;
    FSplTreeviewAndListBox.Align := alTop;
    FClbGaugeS.Align := alClient;

    FLbxSelectedGauges.Align := alClient;

    FPanelForTreeView.Top := 20;
    FPanelForSelectedList.Top := 20;
    FPanelForGISViewer.Top := 20;

    FPanelForTreeView.Width := 100;

    FPanelForTreeView.Align := alLeft;
    FSplTreeView.Left := FPanelForTreeView.Left + FPanelForTreeView.Width + 1;
    FSplTreeView.Align := alLeft;

    FPanelForSelectedList.Left := FSplTreeView.Left + FSplTreeView.Width + 1;
    FPanelForSelectedList.Align := alLeft;
    FPanelForSelectedList.Width := 100;

    FSplSelectedGauges.Left := FPanelForSelectedList.Left + FPanelForSelectedList.Width + 1;
    FSplSelectedGauges.Align := alLeft;

    FBtnUpdateGISViewer.Align := alBottom;
    FPanelForGISViewer.Align := alClient;

    FTrvGaugeList.Height := FPanelForTreeView.Height div 2;

    FClbGaugeS.OnClickCheck := FClbGaugeSClickCheck;
    FTrvGaugeList.OnChange := FTrvGaugeListChange;
    FLbxSelectedGauges.OnKeyDown := OnKeyDownListBox;

    FLbxSelectedGauges.Sorted := true;
    FBtnUpdateGISViewer.Caption := FAppModules.Language.GetString('ButtonCaption.Rainfall');

    FBtnUpdateGISViewer.OnClick := BtnUpdateGISViewer;
  except on E:Exception do HandleError(E,OPNAME) end;
end;

procedure TFrmMain.DestroyMemberObjects;
const OPNAME = 'TFrmMain.DestroyMemberObjects';
begin
  try
    FreeAndNil(FGaugeList);
    FreeAndNil(FOpenDialog);
    FreeAndNil(FSaveDialog);
    FreeAndNil(FMenuItemManager);
  except on E:Exception do HandleError(E,OPNAME) end;
end;

procedure TFrmMain.SynchroniseSelectionList;
const OPNAME = 'TFrmMain.SynchroniseSelectionList';
var
  LStringList : TStringList;
begin
  try
    if Assigned(FGaugeList) then
    begin
      LStringList := TStringList.Create;
      try
        FGaugeList.GetSelectedList(LStringList);
        FLbxSelectedGauges.Items.Text := LStringList.GetText;
      finally
        FreeAndNil(LStringList);
      end;
    end;
  except on E:Exception do HandleError(E,OPNAME) end;
end;

procedure TFrmMain.OnKeyDownListBox(Sender: TObject; var Key: Word;
  Shift: TShiftState);
const OPNAME = 'TFrmMain.OnKeyDownListBox';
var
  LIndex : integer;
begin
  try
    if (Key = VK_DELETE) then
    begin
      for LIndex := 0 to FLbxSelectedGauges.Count - 1 do
      begin
        if FLbxSelectedGauges.Selected[LIndex] then
          if Assigned(FGaugeList) then
            TAbstractGauge(FGaugeList.GetItem(FLbxSelectedGauges.Items[LIndex])).UnSelect;
      end;
      UpdateStatusPanel;
      SynchroniseSelectionList;
      SynchroniseCheckList;
      SynchroniseGISViewer;
    end
  except on E:Exception do HandleError(E,OPNAME) end;
end;

procedure TFrmMain.BtnUpdateGISViewer(Sender: TObject);
const OPNAME = 'TFrmMain.BtnUpdateGISViewer';
var
  LSavedBoolean : boolean;
  LRGProgressDialog : TRGProgressDialog;
begin
  try
    LSavedBoolean := FUpdateGISLive;
    TButton(Sender).Enabled := false;
    LRGProgressDialog := TRGProgressDialog.Create;
    try
      LRGProgressDialog.ShowLabel;
      LRGProgressDialog.SetLabelCaption(FAppModules.Language.GetString('LabelText.WaitWhileGISViewerIsUpdated'));
      LRGProgressDialog.ShowForm;
      FUpdateGISLive := True;
      SynchroniseGISViewer;
      LRGProgressDialog.HideForm;
    finally
      FreeAndNil(LRGProgressDialog);
      FUpdateGISLive := LSavedBoolean;
      TButton(Sender).Enabled := true;
    end;
  except on E:Exception do HandleError(E,OPNAME) end;
end;

procedure TFrmMain.DoDailyExtract(Sender: TObject);
const OPNAME = 'TFrmMain.DoDailyExtract';
var
  LExtractData : TExtractData;
begin
  try
    if Assigned(FGaugeList) then
      if FGaugeList.SelectedCount > 0 then
      begin

        LExtractData := TExtractData.Create;
        try
          LExtractData.OnExtractRequest := DoExtractListOfStations;
          LExtractData.ShowFormModal;
        finally
          LExtractData.Free;
        end;

      end
      else
        ShowMessage(FAppModules.Language.GetString('Message.NoGaugesSelected'));

  except on E:Exception do HandleError(E,OPNAME) end;
end;

procedure TFrmMain.DoExtractListOfStations(ASender: TObject);
const OPNAME = 'TFrmMain.DoExtractListOfStations';
var
  LFileName : string;
begin
  try

    LFileName := ExtractFilePath(Application.ExeName) + 'WRCDATA\FILE.STN';

    if Assigned(FGaugeList)  then
    begin
      FGaugeList.SaveFileName := LFileName;
      FGaugeList.SaveToFile;
      FSelectionIsDirty := false;
    end;

  except on E:Exception do HandleError(E,OPNAME) end;
end;

procedure TFrmMain.DoMonthlyExtract(Sender: TObject);
const OPNAME = 'TFrmMain.DoMonthlyExtract';
var
  LFileStream : TFileStream;
  LStringList : TStringList;
  LCurrentDir : string;
  LDone : boolean;
begin
  try
    if Assigned(FGaugeList) then
      if FGaugeList.SelectedCount > 0 then
      begin
        DoExtractListOfStations(nil);

        LCurrentDir := ExtractFilePath(Application.ExeName) + 'WRCDATA\';

        LFileStream := TFileStream.Create(LCurrentDir + 'INPUTTO_BR_ASTATS.TXT', fmCreate or fmShareDenyWrite);
        LStringList := TStringList.Create;
        try
          LStringList.Clear;
          LStringList.Add('2');
          LStringList.Add(' ');
          LStringList.Add(' ');
          LStringList.SaveToStream(LFileStream);
        finally
          FreeAndNil(LFileStream);
          FreeAndNil(LStringList);
        end;

        LFileStream := TFileStream.Create(LCurrentDir + 'LAUNCH_BR_ASTATS.BAT', fmCreate or fmShareDenyWrite);
        LStringList := TStringList.Create;
        try
          LStringList.Clear;
          LStringList.Add('@CLS ');
          LStringList.Add('@BR_ASTATS.EXE < INPUTTO_BR_ASTATS.TXT ');
          LStringList.Add(' ');
          LStringList.SaveToStream(LFileStream);
        finally
          FreeAndNil(LFileStream);
          FreeAndNil(LStringList);
        end;

        DeleteFile(LCurrentDir + FAppModules.Language.GetString('Rainfall.DelFileYes'));
        DeleteFile(LCurrentDir + FAppModules.Language.GetString('Rainfall.DelFileNo'));
        DeleteFile(LCurrentDir + FAppModules.Language.GetString('Rainfall.DelFileStat'));

        TShellExecuteObject.ExecuteShellAction('OPEN', 'LAUNCH_BR_ASTATS.BAT', '', LCurrentDir);
        Sleep(250);

        while not FileExists(LCurrentDir + FAppModules.Language.GetString('Rainfall.DelFileStat')) do Sleep(500);

        LDone := False;

        while not LDone do
        begin
          try
            try
              LFileStream := TFileStream.Create(LCurrentDir + FAppModules.Language.GetString('Rainfall.DelFileStat'), fmOpenRead or fmShareDenyWrite);
              LDone := True;
            except
              // Ignore Exception, the extractor is still running
            end;
          finally
            FreeAndNil(LFileStream);
          end;
          Sleep(500);
        end;

        SplitMonthlyFile;

        DeleteFile(LCurrentDir + FAppModules.Language.GetString('Rainfall.InputTo'));
        DeleteFile(LCurrentDir + FAppModules.Language.GetString('Rainfall.Launch'));

        ShowMessage(FAppModules.Language.GetString('Message.ExtractionComplete'));

        TShellExecuteObject.ExecuteShellAction('EXPLORE', LCurrentDir + 'MONTHLY Data', '', '');

      end
      else
        ShowMessage(FAppModules.Language.GetString('Message.NoGaugesSelected'));

  except on E:Exception do HandleError(E,OPNAME) end;
end;

procedure TFrmMain.SplitMonthlyFile;
const OPNAME = 'TFrmMain.SplitMonthlyFile';
var
  LStringListIn  : TStringList;
  LOutList       : TStringList;
  LCurrentDir    : string;
  LIndex         : integer;
begin
  try
    LCurrentDir := ExtractFilePath(Application.ExeName) + 'WRCDATA\Monthly Data\';

    if not DirectoryExists(LCurrentDir) then
      ForceDirectories(LCurrentDir);

    // Here we should check for previously extracted files first
    // The user might want to add files to an existing set or not

    LStringListIn := TStringList.Create;
    LOutList := TStringList.Create;
    try
      LStringListIn.LoadFromFile(LCurrentDir + '..\ASTATS.BR');

      LIndex := 0;

      while LIndex <= LStringListIN.Count - 1 do
      begin
        LOutList.Add(LStringListIn.Strings[LIndex]);
        if (LIndex = LStringListIN.Count - 1) or
          ((LIndex < (LStringListIN.Count - 1))  and (Length(Trim(LStringListIn.Strings[LIndex + 1])) < 10)) then
          if LOutList.Count > 1 then
          begin
            ProcessStationMonthlyFile(LOutList);
            // LOutList.Add(LStringListIn.Strings[LIndex]);
          end;
        Inc(LIndex);
      end;

    finally
      FreeAndNil(LStringListIn);
      FreeAndNil(LOutList);
    end;

  except on E:Exception do HandleError(E,OPNAME) end;
end;

procedure TFrmMain.ProcessStationMonthlyFile(AStringList: TStringList);
const OPNAME = 'TFrmMain.ProcessStationMonthlyFile';
var
  LCurrentDir : string;
  LFileStreamOut : TFileStream;
  LIndex : integer;
  AOutList : TStringList;
begin
  try
    if Assigned(AStringList) then
      if (AStringList.Count > 0) then
      begin
        LCurrentDir := ExtractFilePath(Application.ExeName) + 'WRCDATA\Monthly Data\';

        // The FileStream is used to deny writing to the file (extreme defensive coding), TStringList.SaveToFile does not deny writing (in D6 SP2)
        LFileStreamOut := TFileStream.Create(LCurrentDir + Trim(AStringList.Strings[0]) + '.TXT' , fmCreate or fmShareDenyWrite);
        try
          // Cleanup header string
          AStringList.Delete(0);

          // Cleanup Trailer Strings
          for LIndex := 1 to 9 do
            if AStringList.Count > 0 then
              AStringList.Delete(AStringList.Count - 1);

          AOutList := TStringList.Create;
          try
            ShiftMonths(AStringList, AOutList);
            CalculateYearTotals(AOutList);
            AOutList.SaveToStream(LFileStreamOut);
          finally
            FreeAndNil(AOutList);
          end;

        finally
          FreeAndNil(LFileStreamOut);
        end;
      end;

    // Side Effect
    AStringList.Clear;
  except on E:Exception do HandleError(E,OPNAME) end;
end;

procedure TFrmMain.ShiftMonths(AStringListIn: TStringList; AStringListOut: TStringList);
const OPNAME = 'TFrmMain.ShiftMonths';
var
  LPrevYear : string;
  LCurrYear : string;
  LNewYear : string;
  LIndex : integer;
begin
  try
    if AStringListIn.Count > 0 then
    begin
      LIndex := 0;
      LPrevYear := Format('%4d', [StrToInt(Copy(AStringListIn.Strings[LIndex],1,4)) - 1]) +
        ' -999.9 -999.9 -999.9 -999.9 -999.9 -999.9 -999.9 -999.9 -999.9 -999.9 -999.9 -999.9    0.0+';
      for LIndex := 0 to AStringListIn.Count do
      begin
        if LIndex = AStringListIn.Count then
          LCurrYear := '9999 -999.9 -999.9 -999.9 -999.9 -999.9 -999.9 -999.9 -999.9 -999.9 -999.9 -999.9 -999.9    0.0+'
        else
          LCurrYear := AStringListIn.Strings[LIndex];
        LNewYear := Copy(LPrevYear,1, 5) + Copy(LPrevYear, 69,21) + Copy(LCurrYear, 6, 63) + '0000.0 ';
        AStringListOut.Add(LNewYear);
        LPrevYear := LCurrYear;
      end;
    end;
  except on E:Exception do HandleError(E,OPNAME) end;
end;

procedure TFrmMain.CalculateYearTotals(AStringList: TStringList);
const
  OPNAME = 'TFrmMain.CalculateYearTotals';
var
  LIndex : integer;
  LMonthIndex : integer;
  LTotType : string;
  LTotal : Double;
begin
  try
    for LIndex := 0 to AStringList.Count - 1 do
    begin
      LTotType := ' ';
      LTotal := 0;
      for LMonthIndex := 0 to 11 do
      begin
        if Copy(AStringList.Strings[LIndex], 5 + 1 + 7 * LMonthIndex, 6) = '-999.9' then
          LTotType := '+'
        else
          LTotal := LTotal + StrToFloat(Copy(AStringList.Strings[LIndex], 5 + 1 + 7 * LMonthIndex, 6));
      end;
      AStringList.Strings[LIndex] := Copy(AStringList.Strings[LIndex], 1, 7*12 + 5) + Format('%6.1F',[LTotal]) + LTotType;
    end;
  except on E:Exception do HandleError(E,OPNAME) end;
end;

end.
