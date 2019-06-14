//
//
//  UNIT      : Contains TModelCapabilityTabSheet Class
//  AUTHOR    : Dziedzi Ramulondi(Aravia)
//  DATE      : 2004/12/22
//  COPYRIGHT : Copyright © 2004 DWAF
//
//
unit UModelCapabilityTabSheet;

interface

uses                                             
  VCL.Menus,
  Classes,
  VCL.stdctrls,
  VCL.ComCtrls,
  contnrs,
  UHelpContexts,
  UAbstractObject,
  UAbstractComponent,
  UTreeViewTabSheet,
  UModelCapabilityFieldPanel,
  UModelCapabilityValidationPanel,
  UModelCapabilityArrayPanel,
  UAbstractModelCapabilityPanel;
type

  TModelCapabilityTabSheet = class(TTreeViewTabSheet)
  protected
    FCurrentPanel: TAbstractModelCapabilityPanel;
    FValidationPanel: TModelCapabilityValidationPanel;
    FArrayPanel:TModelCapabilityArrayPanel;
    FFieldPanel:TModelCapabilityFieldPanel;
    procedure CreateMemberObjects; override;
    procedure AssignHelpContext; override;
    procedure DoTreeNodeAboutToChange(ASender: TObject; ANode: TTreeNode; var AAllowChange: Boolean); virtual;
    procedure DoTreeNodeHasChanged(ASender: TObject; ANode: TTreeNode); virtual;
    procedure PopulateValidationPanel;
    procedure PopulateArrayPanel;
    procedure PopulateFieldPanel;

  public
    function Initialise: boolean; override;
    function LanguageHasChanged: boolean; override;
    function StudyHasChanged: boolean; override;
    function CanCopyToCLipboard: boolean; override;
    function CanExport: boolean; override;
    function CanPrint: boolean; override;
    procedure DoCopyToCLipboard; override;
    procedure DoExport(AFileName: string = ''); override;
    procedure DoPrint; override;
    function GetToolBar: TAbstractToolBar; override;
  end;

implementation

{$WARN UNIT_PLATFORM OFF}

uses
  Math,
  UConstants,
  windows,
  VCL.Forms,
  VCL.Graphics,
  SysUtils,
  VCL.Controls,
  //FileCtrl,
  UUtilities,
  UFileNameConstants,
  UMainMenuEventType,
  UErrorHandlingOperations;

{ TModelCapabilityTabSheet }

procedure TModelCapabilityTabSheet.CreateMemberObjects;
const OPNAME = 'TModelCapabilityTabSheet.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
    FCurrentPanel     := nil;
    FTabCaptionKey    := 'ModelCapability';
    FValidationPanel  := TModelCapabilityValidationPanel.Create(Self,FAppModules);
    FArrayPanel       := TModelCapabilityArrayPanel.Create(Self,FAppModules);
    FFieldPanel       := TModelCapabilityFieldPanel.Create(Self,FAppModules);

    FValidationPanel.Parent := Self;
    FArrayPanel.Parent := Self;
    FFieldPanel.Parent := Self;

    FValidationPanel.Align := alClient;
    FArrayPanel.Align := alClient;
    FFieldPanel.Align := alClient;

    FValidationPanel.Visible := False;
    FArrayPanel.Visible := False;
    FFieldPanel.Visible := False;

    // Put these event handlers last.
    FTreeView.ReadOnly   := True;
    FTreeView.OnChanging := DoTreeNodeAboutToChange;
    FTreeView.OnChange   := DoTreeNodeHasChanged;

  // Handle exceptions.
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TModelCapabilityTabSheet.Initialise: boolean;
const OPNAME = 'TModelCapabilityTabSheet.Initialise';
begin
  Result := inherited Initialise;
  try
    FValidationPanel.Initialise;
    FArrayPanel.Initialise;
    FFieldPanel.Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TModelCapabilityTabSheet.AssignHelpContext;
const OPNAME = 'TModelCapabilityTabSheet.AssignHelpContext';
begin
  try
    FValidationPanel.AssignHelpContext;
    FArrayPanel.AssignHelpContext;
    FFieldPanel.AssignHelpContext;
    {SetControlHelpContext(Self,HC_FileSelection);
    SetControlHelpContext(FTreeView,HC_FileSelectionTreeView);
    SetControlHelpContext(FRichEdit,HC_FileViewer);}
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TModelCapabilityTabSheet.StudyHasChanged: boolean;
const OPNAME = 'TModelCapabilityTabSheet.StudyHasChanged';
begin
  Result := inherited StudyHasChanged;
  try
    FValidationPanel.StudyHasChanged;
    FArrayPanel.StudyHasChanged;
    FFieldPanel.StudyHasChanged;
    PopulateValidationPanel;
    PopulateArrayPanel;
    PopulateFieldPanel;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TModelCapabilityTabSheet.LanguageHasChanged: boolean;
const OPNAME = 'TModelCapabilityTabSheet.LanguageHasChanged';
var
 LNode:TTreeNode;
begin
  Result := inherited LanguageHasChanged;
  try
    FArrayPanel.LanguageHasChanged;
    FFieldPanel.LanguageHasChanged;
    FValidationPanel.LanguageHasChanged;
    FTreeView.Items.Clear;
    LNode := FTreeView.Items.Add(nil,FArrayPanel.Caption);
    LNode.Data := TObject(1);
    LNode := FTreeView.Items.Add(nil,FFieldPanel.Caption);
    LNode.Data := TObject(2);
    LNode := FTreeView.Items.Add(nil,FValidationPanel.Caption);
    LNode.Data := TObject(3);
    FTreeView.FullExpand;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TModelCapabilityTabSheet.DoTreeNodeAboutToChange(ASender: TObject; ANode: TTreeNode; var AAllowChange: Boolean);
const OPNAME = 'TModelCapabilityTabSheet.DoTreeNodeAboutToChange';
begin
  try
    if (csDestroying in Self.ComponentState) then Exit;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TModelCapabilityTabSheet.DoTreeNodeHasChanged(ASender: TObject; ANode: TTreeNode);
const OPNAME = 'TModelCapabilityTabSheet.DoTreeNodeHasChanged';
var
  LIndex: integer;
begin
  try
    if (csDestroying in Self.ComponentState) then Exit;

    if Assigned(FCurrentPanel) then
    begin
      FCurrentPanel.Visible := False;
      FCurrentPanel := nil;
    end;

    LIndex := Integer(ANode.Data);
    case LIndex of
      1: FCurrentPanel := FArrayPanel;
      2: FCurrentPanel := FFieldPanel;
      3: FCurrentPanel := FValidationPanel;
    end;

    if Assigned(FCurrentPanel) then
    begin
      FCurrentPanel.Visible := True;
      FCurrentPanel.BringToFront;
      FAppModules.MainForm.MenuItemManager.SetExportEnabled(Self.CanExport);
      FAppModules.MainForm.MenuItemManager.SetClipboardEnabled(Self.CanCopyToClipboard);
      FAppModules.PrintManager.SetPrintEnabled(Self.CanPrint);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TModelCapabilityTabSheet.CanCopyToClipboard: boolean;
const OPNAME = 'TModelCapabilityTabSheet.CanCopyToClipboard';
begin
  Result := False;
  try
    Result := FCurrentPanel <> nil;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TModelCapabilityTabSheet.CanExport: boolean;
const OPNAME = 'TModelCapabilityTabSheet.CanExport';
begin
  Result := False;
  try
    Result := FCurrentPanel <> nil;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TModelCapabilityTabSheet.CanPrint: boolean;
const OPNAME = 'TModelCapabilityTabSheet.CanPrint';
begin
  Result := False;
  try
    Result := FCurrentPanel <> nil;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TModelCapabilityTabSheet.DoCopyToClipboard;
const OPNAME = 'TModelCapabilityTabSheet.DoCopyToClipboard';
begin
  try
    FCurrentPanel.DataGrid.DoCopyToClipboard;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TModelCapabilityTabSheet.DoExport(AFileName: string = '');
const OPNAME = 'TModelCapabilityTabSheet.DoExport';
begin
  try
    FCurrentPanel.DataGrid.DoExport(AFileName);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TModelCapabilityTabSheet.DoPrint;
const OPNAME = 'TModelCapabilityTabSheet.DoPrint';
begin
  try
    FCurrentPanel.DataGrid.DoPrint(FCurrentPanel.Caption);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TModelCapabilityTabSheet.PopulateArrayPanel;
const OPNAME = 'TModelCapabilityTabSheet.PopulateArrayPanel';
var
  LAllFieldContainer,
  LFieldContainer  : TObjectList;
  LFieldProperty   : TAbstractFieldProperty;
  LIndex           : integer;
  LModelValues     : TStringList;
  LModel           : string;
begin
  try
    LFieldContainer    := TObjectList.Create(False);
    LAllFieldContainer := TObjectList.Create(False);
    LModelValues       := TStringList.Create;
    try
      if FAppModules.FieldProperties.GetFieldPropertiesList(LAllFieldContainer) then
      begin
        for LIndex := 0 to LAllFieldContainer.Count -1 do
         begin
          LFieldProperty := TAbstractFieldProperty(LAllFieldContainer.Items[LIndex]);
          LModel := FAppModules.StudyArea.ModelSubCode;
          LModelValues.CommaText := LFieldProperty.ModeIsEditable;
          if((LFieldProperty.FieldGroup in [fgArray, fgMinMaxArray]) and
             (LModelValues.IndexOf(LModel) >= 0)) then
               LFieldContainer.Add(LFieldProperty);
        end;

        FArrayPanel.DataGrid.RowCount := Max(2,LFieldContainer.Count + 1);
        for LIndex := 0 to LFieldContainer.Count -1 do
        begin
          LFieldProperty :=  TAbstractFieldProperty(LFieldContainer.Items[LIndex]);
          FArrayPanel.DataGrid.cells[0,LIndex+1] := LFieldProperty.FieldName;
          FArrayPanel.DataGrid.cells[1,LIndex+1] := FAppModules.Language.GetString(LFieldProperty.FieldDescription);
          FArrayPanel.DataGrid.cells[2,LIndex+1] := LFieldProperty.FileFieldName;
          FArrayPanel.DataGrid.Cells[3,LIndex+1] :=
          StringReplace(LFieldProperty.FieldArrayLength,',','..',[rfReplaceAll, rfIgnoreCase]);
        end;
      end;
    finally
       LFieldContainer.Free;
       LModelValues.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TModelCapabilityTabSheet.PopulateFieldPanel;
const OPNAME = 'TModelCapabilityTabSheet.PopulateFieldPanel';
var
  LAllFieldContainer,
  LFieldContainer  : TObjectList;
  LFieldProperty   : TAbstractFieldProperty;
  LIndex           : integer;
  LModelPos        : integer;
  LVersion         : TStringList;
  LModel           : string;
  LModelVersion    : string;
  LModelValues     : TStringList;
begin
  try
    LFieldContainer    := TObjectList.Create(False);
    LAllFieldContainer := TObjectList.Create(False);
    LVersion           := TStringList.Create;
    LModelValues       := TStringList.Create;
    try
      if FAppModules.FieldProperties.GetFieldPropertiesList(LAllFieldContainer) then
      begin
        for LIndex := 0 to LAllFieldContainer.Count -1 do
        begin
          LFieldProperty := TAbstractFieldProperty(LAllFieldContainer.Items[LIndex]);
          LModel := FAppModules.StudyArea.ModelSubCode;
          LModelValues.CommaText := LFieldProperty.ModeIsEditable;
          if((LFieldProperty.FieldGroup in [fgminmax,fgnone]) and (LFieldProperty.FileFieldName <> '') and
             (LModelValues.IndexOf(LModel) >= 0)) then
             LFieldContainer.Add(LFieldProperty);
        end;

        FFieldPanel.DataGrid.RowCount := Max(2,LFieldContainer.Count + 1);
        for LIndex := 0 to LFieldContainer.Count -1 do
        begin
          LFieldProperty := TAbstractFieldProperty(LFieldContainer.Items[LIndex]);
          FFieldPanel.DataGrid.cells[0,LIndex+1] := LFieldProperty.FieldName;
          FFieldPanel.DataGrid.cells[1,LIndex+1] := FAppModules.Language.GetString(LFieldProperty.FieldDescription);
          FFieldPanel.DataGrid.cells[2,LIndex+1] := LFieldProperty.FileFieldName;

          LVersion.CommaText := LFieldProperty.ModelVersionNumber;
          LModelVersion := FAppModules.StudyArea.ModelVersion;
          LModelPos := LVersion.IndexOf(LModelVersion);
          if LModelPos >= 0 then
          begin
            if(LFieldProperty.FieldMinimumValue = NegativeInf) then
              FFieldPanel.DataGrid.cells[3,LIndex+1] := FAppModules.Language.GetString('NegativeInfinity.<0')
            else
              FFieldPanel.DataGrid.cells[3,LIndex+1] := LFieldProperty.FieldMinimumValue;
            if(LFieldProperty.FieldMaximumValue = PositiveInf) then
              FFieldPanel.DataGrid.cells[4,LIndex+1] := FAppModules.Language.GetString('PositiveInfinity.>0')
            else
              FFieldPanel.DataGrid.cells[4,LIndex+1] := LFieldProperty.FieldMaximumValue;
          end
          else
          begin
            FFieldPanel.DataGrid.cells[3,LIndex+1] := '-';
            FFieldPanel.DataGrid.cells[4,LIndex+1] := '-';
          end;
        end;
      end;
    finally
      LFieldContainer.Free;
      LVersion.Free;
      LModelValues.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TModelCapabilityTabSheet.PopulateValidationPanel;
const OPNAME = 'TModelCapabilityTabSheet.PopulateValidationPanel';
var
  LAllFieldContainer,
  LFieldContainer     : TObjectList;
  LFieldProperty      :TAbstractFieldProperty;
  LIndex              : integer;
  LModelValues        : TStringList;
  LModel              : string;
begin
  try
    LAllFieldContainer := TObjectList.Create(False);
    LFieldContainer    := TObjectList.Create(False);
    LModelValues       := TStringList.Create;
    try
      if FAppmodules.FieldProperties.GetFieldPropertiesList(LAllFieldContainer) then
      begin
        for LIndex := 0 to LAllFieldContainer.Count -1 do
        begin
          LFieldProperty := TAbstractFieldProperty(LAllFieldContainer.Items[LIndex]);
          LModel := FAppModules.StudyArea.ModelSubCode;
          LModelValues.CommaText := LFieldProperty.ModeIsEditable;
          if ((LFieldProperty.FieldGroup in [fgValidation, fgValidationArray]) and
              (LModelValues.IndexOf(LModel) >= 0)) then
                 LFieldContainer.Add(LFieldProperty);
        end;
        FValidationPanel.DataGrid.RowCount := Max(2,LFieldContainer.Count + 1);
        for LIndex := 0 to LFieldContainer.Count -1 do
        begin
          LFieldProperty := TAbstractFieldProperty(LFieldContainer.Items[LIndex]);
          FValidationPanel.DataGrid.Cells[0,LIndex+1] := LFieldProperty.FieldName;
          FValidationPanel.DataGrid.Cells[1,LIndex+1] := FAppModules.Language.GetString(LFieldProperty.FieldDescription);
          FValidationPanel.DataGrid.Cells[2,LIndex+1] := LFieldProperty.FileFieldName;
          FValidationPanel.DataGrid.Cells[3,LIndex+1] := LFieldProperty.FieldAcceptedValues;
        end;
      end;
    finally
     LAllFieldContainer.Free;
     LFieldContainer.Free;
     LModelValues.Free;
   end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TModelCapabilityTabSheet.GetToolBar: TAbstractToolBar;
const OPNAME = 'TModelCapabilityTabSheet.GetToolBar';
begin
  Result := nil;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.

