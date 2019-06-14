//
//
//  UNIT      : Contains the class TStudyPanel.
//  AUTHOR    : Grant Nyland
//  DATE      : 2002/02/20
//  COPYRIGHT : Copyright © 2002 DWAF
//
//
unit UStudyPanel;

interface

uses
  Classes,
  Vcl.StdCtrls,
  Vcl.ExtCtrls,
  UAbstractObject,
  UAbstractComponent;

type
  TStudyPanelButton = class(TAbstractMainSpeedButton)
  public
    constructor Create(AOwner: TComponent; AAppModules: TAppModules; AButtonKey: string); override;
  end;
  TStudyPanel = class(TAbstractPanel)
  protected
    pnlSubArea: TPanel;
    cmbSubArea: TComboBox;
    pnlSubAreaDescription: TPanel;
    pnlScenario: TPanel;
    cmbScenario: TComboBox;
    pnlScenarioDescription: TPanel;
    pnlDate: TPanel;
    pnlModel: TPanel;
    pnlVersion: TPanel;
    pnlUser: TPanel;
    btnSubAreaReport: TStudyPanelButton;
    btnScenarioReport: TStudyPanelButton;
    FPanels: TList;
    FCombos: TList;
    FButtons: TList;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    function IsSubAreaVisible: boolean;
    function IsScenarioVisible: boolean;
    procedure SetPositions;
    procedure SetPositionsSubArea;
    procedure SetPositionsScenario;
    procedure SetPositionsDetails;
    procedure OnReportButtonClick(ASender: TObject);
    procedure OnComboChange(ASender: TObject);
    procedure AssignHelpContext; override;
  public
    property UserPanel : TPanel read pnlUser;
    procedure Resize; override;
    function StudyHasChanged: boolean; override;
    procedure UserHasChanged;
    function SaveState: boolean; override;
    function ResetState: boolean; override;
    procedure RefreshState; virtual;
  end;

implementation

uses
  SysUtils,
  Vcl.Controls,
  UHelpContexts,
  UMainMenuEventType,
  USystemModelLinkClasses,
  UErrorHandlingOperations;

const
  CBorder = 2;
  CLineHeight = 21;

{ TStudyPanelButton }

constructor TStudyPanelButton.Create(AOwner: TComponent; AAppModules: TAppModules; AButtonKey: string);
const OPNAME = 'TStudyPanelButton.Create';
begin
  try
    inherited;
    FData := TStudyDocumentDetail.Create;
    TStudyDocumentDetail(FData).CategoryKey := '';
    TStudyDocumentDetail(FData).Filename    := '';
    TStudyDocumentDetail(FData).BookMark    := AButtonKey;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{ TStudyPanel }

procedure TStudyPanel.CreateMemberObjects;
const OPNAME = 'TStudyPanel.CreateMemberObjects';
var LIndex: integer;
begin
  try

    // Create lists for the objects.
    FPanels  := TList.Create;
    FCombos  := TList.Create;
    FButtons := TList.Create;

    // Create the panels.
    pnlSubArea := TPanel.Create(self);
    FPanels.Add(pnlSubArea);
    pnlSubAreaDescription := TPanel.Create(self);
    FPanels.Add(pnlSubAreaDescription);
    pnlScenario := TPanel.Create(self);
    FPanels.Add(pnlScenario);
    pnlScenarioDescription := TPanel.Create(self);
    FPanels.Add(pnlScenarioDescription);
    pnlDate := TPanel.Create(self);
    FPanels.Add(pnlDate);
    pnlModel := TPanel.Create(self);
    FPanels.Add(pnlModel);
    pnlVersion := TPanel.Create(self);
    FPanels.Add(pnlVersion);
    pnlUser := TPanel.Create(self);
    FPanels.Add(pnlUser);

    // Set the common properties.
    for LIndex := 0 to FPanels.Count - 1 do
    begin
      TPanel(FPanels.Items[LIndex]).Parent := self;
      TPanel(FPanels.Items[LIndex]).BevelOuter := bvLowered;
      TPanel(FPanels.Items[LIndex]).Alignment := taLeftJustify;
      TPanel(FPanels.Items[LIndex]).Top := CBorder;
      TPanel(FPanels.Items[LIndex]).Left := CBorder;
      TPanel(FPanels.Items[LIndex]).Height := CLineHeight;
    end;

    // Create the combos.
    cmbSubArea := TComboBox.Create(self);
    cmbSubArea.Parent := pnlSubArea;
    FCombos.Add(cmbSubArea);
    cmbScenario := TComboBox.Create(self);
    cmbScenario.Parent := pnlScenario;
    FCombos.Add(cmbScenario);

    // Set the common properties.
    for LIndex := 0 to FCombos.Count - 1 do
    begin
      //TComboBox(FCombos.Items[LIndex]).Parent := self;
      TComboBox(FCombos.Items[LIndex]).Style := csDropDownList;
      TComboBox(FCombos.Items[LIndex]).Top := CBorder;
      TComboBox(FCombos.Items[LIndex]).Height := CLineHeight;
      TComboBox(FCombos.Items[LIndex]).OnChange := OnComboChange;
    end;

    // Create the buttons.
    btnSubAreaReport := TStudyPanelButton.Create(pnlSubAreaDescription, FAppModules, 'SubArea');
    btnSubAreaReport.Parent := pnlSubAreaDescription;
    FButtons.Add(btnSubAreaReport);
    btnScenarioReport := TStudyPanelButton.Create(pnlScenarioDescription, FAppModules, 'Scenario');
    btnScenarioReport.Parent := pnlScenarioDescription;
    FButtons.Add(btnScenarioReport);

    // Set the common properties.
    for LIndex := 0 to FButtons.Count - 1 do
    begin
      TStudyPanelButton(FButtons.Items[LIndex]).Glyph.LoadFromResourceName(HImagesInstance, 'REPORT');
      TStudyPanelButton(FButtons.Items[LIndex]).NumGlyphs := 2;
      TStudyPanelButton(FButtons.Items[LIndex]).OnClick := OnReportButtonClick;
      TStudyPanelButton(FButtons.Items[LIndex]).Top    := 1;
      TStudyPanelButton(FButtons.Items[LIndex]).Width  := CLineHeight + 2;
      TStudyPanelButton(FButtons.Items[LIndex]).Height := CLineHeight - 2;
    end;

  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TStudyPanel.AssignHelpContext;
const OPNAME = 'TStudyPanel.AssignHelpContext';
begin
  try
    SetControlHelpContext(Self,HC_StudyPanel);
    SetControlHelpContext(cmbSubArea,HC_StudyPanelSubAreaDropDown);
    SetControlHelpContext(cmbScenario,HC_StudyPanelScenarioDropDown);
    SetControlHelpContext(btnSubAreaReport,HC_StudyPanelReportbutton);
    //SetControlHelpContext(btnScenarioReport,HC_DirectorySelectorButton);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TStudyPanel.DestroyMemberObjects;
const OPNAME = 'TStudyPanel.DestroyMemberObjects';
begin
  try
    FreeAndNil(FPanels);
    FreeAndNil(FCombos);
    FreeAndNil(FButtons);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TStudyPanel.SetPositions;
const OPNAME = 'TStudyPanel.SetPositions';
var LHeight: integer;
begin
  try
    LHeight := CLineHeight + CBorder * 2;
    if IsScenarioVisible then
      Inc(LHeight, CLineHeight);
    if IsSubAreaVisible then
      Inc(LHeight, CLineHeight);
    Height := LHeight;
    SetPositionsSubArea;
    SetPositionsScenario;
    SetPositionsDetails;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TStudyPanel.IsSubAreaVisible: boolean;
const OPNAME = 'TStudyPanel.IsSubAreaVisible';
begin
  Result := False;
  try
    if Assigned(FAppModules.StudyArea()) then
      Result := (FAppModules.StudyArea.SubAreaScenario.RowCount > 0);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TStudyPanel.IsScenarioVisible: boolean;
const OPNAME = 'TStudyPanel.IsScenarioVisible';
begin
  Result := False;
  try
    if Assigned(FAppModules.StudyArea()) then
      Result := (FAppModules.StudyArea.SubAreaScenario.RowItemsCount(0) > 0);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TStudyPanel.SetPositionsSubArea;
const OPNAME = 'TStudyPanel.SetPositionsSubArea';
begin
  try
    if Assigned(FAppModules.StudyArea()) and (FAppModules.StudyArea.SubAreaScenario.RowCount > 1) then
    begin
      cmbSubArea.Visible := True;
    end else begin
      cmbSubArea.Visible := False;
    end;
    pnlSubAreaDescription.Visible := IsSubAreaVisible;
    pnlSubArea.Top := CBorder;
    cmbSubArea.Top := -1;
    pnlSubAreaDescription.Top := CBorder;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TStudyPanel.SetPositionsScenario;
const OPNAME = 'TStudyPanel.SetPositionsScenario';
begin
  try
    if IsScenarioVisible then
    begin
      if IsSubAreaVisible then
      begin
        pnlScenario.Top := CBorder + CLineHeight;
        cmbScenario.Top := -1;
        pnlScenarioDescription.Top := CBorder + CLineHeight;
      end else begin
        pnlScenario.Top := CBorder;
        cmbScenario.Top := -1;
        pnlScenarioDescription.Top := CBorder;
      end;
      if Assigned(FAppModules.StudyArea()) and
         (FAppModules.StudyArea.SubAreaScenario.RowItemsCount(FAppModules.StudyArea.SubAreaIndex) > 1) then
      begin
        cmbScenario.Visible := True;
      end else begin
        cmbScenario.Visible := False;
      end;
      pnlScenarioDescription.Visible := True;
    end else begin
      cmbScenario.Visible := False;
      pnlScenarioDescription.Visible := False;
      pnlScenario.Top := CBorder;
      cmbScenario.Top := -1;
      pnlScenarioDescription.Top := CBorder;
    end;

  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TStudyPanel.SetPositionsDetails;
const OPNAME = 'TStudyPanel.SetPositionsDetails';
var LTop: integer;
begin
  try
    LTop := CBorder;
    if IsScenarioVisible then
      Inc(LTop, CLineHeight);
    if IsSubAreaVisible then
      Inc(LTop, CLineHeight);
    pnlDate.Top    := LTop;
    pnlModel.Top   := LTop;
    pnlVersion.Top := LTop;
    pnlUser.Top    := LTop;

  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TStudyPanel.Resize;
const OPNAME = 'TStudyPanel.Resize';
var LWidth, LWidthRemaining: integer;
begin
  try

    // The panel is divided into four sections.
    LWidth := (ClientWidth - CBorder * 2) div 4;
    LWidthRemaining := ClientWidth - LWidth - CBorder * 2;

    // Set the widths.
    pnlSubArea.Width             := LWidth;
    cmbSubArea.Left              := (LWidth div 2);
    cmbSubArea.Width             := LWidth div 2;
    cmbSubArea.Top               := -1;
    pnlSubAreaDescription.Width  := LWidthRemaining;
    pnlScenario.Width            := LWidth;
    cmbScenario.Left             := (LWidth div 2);
    cmbScenario.Width            := LWidth div 2;
    cmbScenario.Top              := -1;
    pnlScenarioDescription.Width := LWidthRemaining;
    pnlDate.Width                := LWidth;
    pnlModel.Width               := LWidth;
    pnlVersion.Width             := LWidth;
    pnlUser.Width                := LWidthRemaining - LWidth * 2;

    // Set the left positions.
    pnlSubAreaDescription.Left  := CBorder + LWidth;
    pnlScenarioDescription.Left := CBorder + LWidth;
    pnlModel.Left               := CBorder + LWidth;
    pnlVersion.Left             := CBorder + LWidth * 2;
    pnlUser.Left                := CBorder + LWidth * 3;

    // Set the button positions.
    btnSubAreaReport.Left       := pnlSubAreaDescription.Width  - (CLineHeight - 2) - 5;
    btnScenarioReport.Left      := pnlScenarioDescription.Width - (CLineHeight - 2) - 5;

  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TStudyPanel.StudyHasChanged: boolean;
const OPNAME = 'TStudyPanel.StudyHasChanged';
var
  LSubAreaIndex, LScenarioIndex: integer;
  LSubArea,
  LScenario: string;
  LRow : TStringList;
begin
  Result := False;
  try
    if Assigned(FAppModules.StudyArea()) then
    begin
      SetPositions;
      if (FAppModules.StudyArea.SubAreaScenario.RowCount > 0) then
      begin
        LSubAreaIndex  := FAppModules.StudyArea.SubAreaIndex;
        LScenarioIndex := FAppModules.StudyArea.ScenarioIndex;
        LSubArea       := FAppModules.StudyArea.SubAreaScenario.Items[LSubAreaIndex];
        LScenario      := FAppModules.StudyArea.SubAreaScenario.Row[LSubAreaIndex][LScenarioIndex];

        // Set the sub area details.
        cmbSubArea.Items.Clear;
        cmbSubArea.Items.AddStrings(FAppModules.StudyArea.SubAreaScenario.Items);
        if cmbSubArea.Visible then
          pnlSubArea.Caption := FAppModules.Language.GetString('TStudyPanel.SubArea')
        else
          pnlSubArea.Caption := FAppModules.Language.GetString('TStudyPanel.SubArea') + FAppModules.StudyArea.SubAreaScenario.Items[LSubAreaIndex];

        cmbSubArea.ItemIndex := LSubAreaIndex;
        pnlSubAreaDescription.Caption  := FAppModules.StudyArea.SubAreaDescriptions.Items[LSubAreaIndex];
        if (FAppModules.StudyArea.SubAreaDocumentDetail.IsDocumentSet('SubAreaDescription')) then
        begin
          btnSubAreaReport.Visible := True;
          TStudyDocumentDetail(btnSubAreaReport.Data).AssignFrom(FAppModules.StudyArea.SubAreaDocumentDetail.DocumentDetail['SubAreaDescription']);
        end else begin
          btnSubAreaReport.Visible := False;
          TStudyDocumentDetail(btnSubAreaReport.Data).Reset;
        end;

        // Set the scenario details.
        cmbScenario.Items.Clear;
        cmbScenario.Items.AddStrings(FAppModules.StudyArea.SubAreaScenario.Row[LSubAreaIndex]);
        if cmbScenario.Visible then
          pnlScenario.Caption := FAppModules.Language.GetString('TStudyPanel.Scenario')
        else
          pnlScenario.Caption := FAppModules.Language.GetString('TStudyPanel.Scenario') + FAppModules.StudyArea.SubAreaScenario.Row[LSubAreaIndex][LScenarioIndex];
        cmbScenario.ItemIndex := LScenarioIndex;
        LRow := FAppModules.StudyArea.ScenarioDescriptions.FindRow(LSubArea+LScenario);
        if(LRow = nil) or (LRow.Count = 0) then
          pnlScenarioDescription.Caption := ''
        else
          pnlScenarioDescription.Caption := LRow[0];
        if (FAppModules.StudyArea.ScenarioDocumentDetail.IsDocumentSet('ScenarioDescription')) then
        begin
          btnScenarioReport.Visible := True;
          TStudyDocumentDetail(btnScenarioReport.Data).AssignFrom(FAppModules.StudyArea.ScenarioDocumentDetail.DocumentDetail['ScenarioDescription']);
        end else begin
          btnScenarioReport.Visible := False;
          TStudyDocumentDetail(btnScenarioReport.Data).Reset;
        end;
      end;
      pnlDate.Caption    := FAppModules.Language.GetString('PanelCaption.Date')      + DateToStr(Now);
      pnlModel.Caption   := FAppModules.Language.GetString('PanelCaption.ModelType') + FAppModules.StudyArea.ModelCode;
      pnlVersion.Caption := FAppModules.Language.GetString('PanelCaption.Study')     + FAppModules.StudyArea.StudyAreaCode;
    end;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TStudyPanel.UserHasChanged;
const OPNAME = 'TStudyPanel.UserHasChanged';
begin
  try
    if Assigned(FAppModules.User()) and (FAppModules.User.UserId <> '') then
    begin
      pnlUser.Caption := FAppModules.Language.GetString('TStudyPanel.User') + FAppModules.User.Initials + ' ' + FAppModules.User.LastName;
    end
    else
    begin
      //pnlUser.Caption := 'User : '
      pnlUser.Caption := FAppModules.Language.GetString('TStudyPanel.LastUser') + FAppModules.IniFile.ReadString('USER','UserId','');
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TStudyPanel.OnReportButtonClick(ASender: TObject);
const OPNAME = 'TStudyPanel.OnReportButtonClick';
begin
  try
    if Assigned(FAppModules.Model()) then
      FAppModules.Model.ProcessEvent(CmeLaunchStudyReport, TStudyPanelButton(ASender).Data);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TStudyPanel.OnComboChange(ASender: TObject);
const OPNAME = 'TStudyPanel.OnComboChange';
var
  LStudyLabels : TStudyLabels;
  LSubArea,
  LScenario: string;
  LRow : TStringList;
begin
  try
    if Assigned(FAppModules.StudyArea()) then
    begin
      if Assigned(FAppModules.Model()) then
      begin
        LStudyLabels := TStudyLabels.Create;
        try
          LStudyLabels.StudyLabel    := FAppModules.StudyArea.StudyLabel;
          LStudyLabels.ModelLabel    := FAppModules.StudyArea.ModelLabel;
          LStudyLabels.ScenarioLabel := '';
          LStudyLabels.SubAreaLabel  := '';
          if (ASender = cmbSubArea) then
          begin
            LStudyLabels.SubAreaLabel  := FAppModules.StudyArea.SubAreaLabels.Items[cmbSubArea.ItemIndex];
          end;
          if (ASender = cmbScenario) then
          begin
            LStudyLabels.SubAreaLabel  := FAppModules.StudyArea.SubAreaLabel;
            LSubArea       := FAppModules.StudyArea.SubAreaScenario.Items[cmbSubArea.ItemIndex];
            LScenario      := FAppModules.StudyArea.SubAreaScenario.Row[cmbSubArea.ItemIndex][cmbScenario.ItemIndex];
            LRow := FAppModules.StudyArea.ScenarioLabels.FindRow(LSubArea+LScenario);
            if(LRow = nil) or (LRow.Count = 0) then
              LStudyLabels.ScenarioLabel := ''
            else
              LStudyLabels.ScenarioLabel := LRow[0];
          end;
          FAppModules.ProcessEvent(CmeSelectStudyDetails, LStudyLabels);
        finally
          LStudyLabels.Free;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TStudyPanel.ResetState: boolean;
const OPNAME = 'TStudyPanel.ResetState';
var
  LVisible: integer;
begin
  Result := False;
  try
    LVisible := FAppModules.ViewIni.ReadInteger(ClassName, 'Visible', 1);
    Self.Visible := LVisible <> 0;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TStudyPanel.SaveState: boolean;
const OPNAME = 'TStudyPanel.SaveState';
begin
  Result := False;
  try
    if(Self.Visible) then
      FAppModules.ViewIni.WriteInteger(ClassName, 'Visible', 1)
    else
      FAppModules.ViewIni.WriteInteger(ClassName, 'Visible', 0);
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TStudyPanel.RefreshState;
const OPNAME = 'TStudyPanel.RefreshState';
var
  LVisible: integer;
begin
  try
    LVisible := FAppModules.ViewIni.ReadInteger(ClassName, 'Visible', 1);
    Self.Visible := LVisible <> 0;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
