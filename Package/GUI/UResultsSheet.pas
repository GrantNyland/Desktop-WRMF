//
//
//  UNIT      : Contains TResultsSheet Class
//  AUTHOR    : Dziedzi Ramulondi (PDNA)
//  DATE      : 2002/08/22
//  COPYRIGHT : Copyright © 2002 DWAF
//
//
unit UResultsSheet;

interface

uses
  VCLTee.Chart,
  VCL.dialogs,
  VCL.ComCtrls,
  UViewDataItem,
  UViewDataList,
  UHelpContexts,
  UAbstractObject,
  UMenuItemManager,
  UAbstractComponent,
  Classes;
type
  TResultsSheet = class(TAbstractTabSheet)
  protected
    FPageControl: TPageControl;
    FCurrentSheet: TAbstractTabSheet;
    FSelectedNodeCaption : WideString;

    // Overriden from TAbstractTabSheet
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    procedure AssignHelpContext; override;
    function GetToolBar: TAbstractToolBar; override;
    procedure PageControlPageChange(Sender: TObject);
    procedure PageControlOnShow(Sender: TObject);
    procedure OnTabChangeRequest(Sender: TObject; var AllowChange: Boolean);
    function GetModelTabSheetName:TModelTabSheetName;override;
  public
    function Initialise: Boolean; override;
    function CanCopyToCLipboard: boolean; override;
    function CanExport: boolean; override;
    function CanPrint: boolean; override;
    procedure DoCopyToCLipboard; override;
    procedure DoExport(AFileName: string = ''); override;
    procedure DoPrint; override;
    procedure TabHasChanged; override;

    procedure SetMenuVisible(AVisible: boolean); override;
    property CurrentSheet:TAbstractTabSheet read FCurrentSheet;

    property PageControl: TPageControl read FPageControl;
    property ToolBar : TAbstractToolBar Read GetToolBar;
  end;

implementation

uses
  SysUtils,
  VCL.Controls,
  VCL.Graphics,
  Windows,
  VCL.Clipbrd,
  VCL.Printers,
  UStudyArea,
  UTreeViewTabSheet,
  UMainMenuEventType,
  UErrorHandlingOperations;

procedure TResultsSheet.CreateMemberObjects;
const OPNAME = 'TResultsSheet.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
    FTabCaptionKey := 'Results';
    FPageControl := TPageControl.Create(Self);
    FPageControl.Parent := Self;
    FPageControl.TabPosition := tpTop;
    FPageControl.OnChange := PageControlPageChange;
    FPageControl.OnChanging := OnTabChangeRequest;
    Self.OnShow := PageControlOnShow;
    FCurrentSheet := nil;
    FSelectedNodeCaption := '';
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TResultsSheet.AssignHelpContext;
const OPNAME = 'TResultsSheet.AssignHelpContext';
begin
  try
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TResultsSheet.DestroyMemberObjects;
const OPNAME = 'TResultsSheet.DestroyMemberObjects';
begin
  try
    inherited DestroyMemberObjects;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TResultsSheet.Initialise: Boolean;
const OPNAME = 'TResultsSheet.Initialise';
begin
  Result := inherited Initialise;
  try
    if (FPageControl.PageCount > 0) then
      FCurrentSheet:= TAbstractTabSheet(FPageControl.ActivePage);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TResultsSheet.GetToolBar: TAbstractToolBar;
const OPNAME = 'TResultsSheet.GetToolBar';
begin
  Result := nil;
  try
    if Assigned(FCurrentSheet) then
      Result := FCurrentSheet.ToolBar;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TResultsSheet.CanCopyToClipboard: boolean;
const OPNAME = 'TResultsSheet.CanCopyToClipboard';
begin
  Result := False;
  try
    Result := Assigned(FCurrentSheet) and FCurrentSheet.CanCopyToClipboard;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TResultsSheet.CanExport: boolean;
const OPNAME = 'TResultsSheet.CanExport';
begin
  Result := False;
  try
    Result := Assigned(FCurrentSheet) and FCurrentSheet.CanExport;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TResultsSheet.CanPrint: boolean;
const OPNAME = 'TResultsSheet.CanPrint';
begin
  Result := False;
  try
    Result := Assigned(FCurrentSheet) and FCurrentSheet.CanPrint;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TResultsSheet.DoCopyToClipboard;
const OPNAME = 'TResultsSheet.DoCopyToClipboard';
begin
  try
    if Assigned(FCurrentSheet) then
       FCurrentSheet.DoCopyToClipboard;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TResultsSheet.DoExport(AFileName: string = '');
const OPNAME = 'TResultsSheet.DoExport';
begin
  try
    if Assigned(FCurrentSheet) then
       FCurrentSheet.DoExport(AFileName);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TResultsSheet.DoPrint;
const OPNAME = 'TResultsSheet.DoPrint';
begin
  try
    if Assigned(FCurrentSheet) then
       FCurrentSheet.DoPrint;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TResultsSheet.PageControlPageChange(Sender: TObject);
const OPNAME = 'TResultsSheet.PageControlPageChange';
var
  x           : Integer;
  LTemp,
  LTempStr    : WideString;
  LTempList   : TStringList;
  LCurIndex,
  LLevel      : Integer;
  LFound      : Boolean;
begin
  try
    FCurrentSheet := TAbstractTabSheet(FPageControl.Pages[FPageControl.ActivePageIndex]);
    FAppModules.Model.ProcessEvent(CmeResultPageControlChanged,nil);
    FCurrentSheet.TabHasChanged;

    LTemp := FSelectedNodeCaption;
    LTempList := TStringList.Create;
    try
      LFound    := False;
      LTempStr  := '';
      x         := 1;
      while x < Length(LTemp)+2 do
      begin

        if LFound or (x = Length(LTemp)+1) then
        begin
          LTempList.Add(Trim(LTempStr));
          LFound    := False;
          LTempStr  := '';
        end;

        if (x < Length(LTemp)+1) then
        begin
          if LTemp[x] <> '\' then
            LTempStr := LTempStr + LTemp[x]
          else
            LFound := True;
        end;

        Inc(x);  
      end;

      LLevel              := 0;
      LCurIndex           := -1;
      for x:=0 to TTreeViewTabSheet(FCurrentSheet).TreeView.Items.Count - 1 do
      begin
        if LLevel < LTempList.Count then
          if Pos(UpperCase(LTempList[LLevel]), UpperCase(Trim(TTreeViewTabSheet(FCurrentSheet).TreeView.Items[x].Text))) <> 0 then
          begin
            Inc(LLevel);
            LCurIndex := x;
          end;
      end;
      if LCurIndex <> -1 then
        TTreeViewTabSheet(FCurrentSheet).TreeView.Items[LCurIndex].Selected := True;
    finally
      if LTempList <> nil then
        FreeAndNil(LTempList);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TResultsSheet.OnTabChangeRequest(Sender: TObject; var AllowChange: Boolean);
const OPNAME = 'TResultsSheet.OnTabChangeRequest';
var
  LParent : TTreeNode;
begin
  try
    if Assigned(CurrentSheet) then
      FAppModules.Model.ProcessEvent(CmeResultPageControlChanging,nil);

    if TTreeViewTabSheet(FCurrentSheet).TreeView.Selected <> nil then
    begin
      FSelectedNodeCaption := TTreeViewTabSheet(FCurrentSheet).TreeView.Selected.Text;
      LParent := TTreeViewTabSheet(FCurrentSheet).TreeView.Selected.Parent;
      while LParent <> nil do
      begin
        FSelectedNodeCaption := Trim(LParent.Text)+'\'+Trim(FSelectedNodeCaption);
        LParent := LParent.Parent;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TResultsSheet.PageControlOnShow(Sender: TObject);
const OPNAME = 'TResultsSheet.PageControlOnShow';
begin
  try
    FPageControl.Align := alClient;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TResultsSheet.SetMenuVisible(AVisible: boolean);
const OPNAME = 'TResultsSheet.SetMenuVisible';
begin
  try
    if Assigned(CurrentSheet) then
      CurrentSheet.SetMenuVisible(AVisible);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TResultsSheet.TabHasChanged;
const OPNAME = 'TResultsSheet.TabHasChanged';
begin
  try
    FCurrentSheet := TAbstractTabSheet(FPageControl.Pages[FPageControl.ActivePageIndex]);
    FCurrentSheet.TabHasChanged;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TResultsSheet.GetModelTabSheetName: TModelTabSheetName;
const OPNAME = 'TResultsSheet.GetModelTabSheetName';
begin
  Result := mtsnNone;
  try
    if Assigned(CurrentSheet) then
      Result := CurrentSheet.ModelTabSheetName;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.

