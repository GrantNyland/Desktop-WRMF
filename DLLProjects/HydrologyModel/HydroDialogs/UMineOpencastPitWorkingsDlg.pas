unit UMineOpencastPitWorkingsDlg;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, VCL.Graphics, VCL.Controls, VCL.Forms,
  VCL.Dialogs, VCL.StdCtrls, xmldom, XMLIntf, XMLDoc, VCL.ExtCtrls, VCL.Buttons, VCL.Menus,

  UXMLAgent,
  UControlIterator,
  HydrologyCom_TLB,
  UXMLAgentMineOpencastPitWorkings,
  UWRMFEdit, UWRMFComboBox, UWRMFControl, UWRMFCheckBox, VCL.ComCtrls,
  VCL.Grids, UCellChangeStringGrid, UWRMFGrid, Xml.Win.msxmldom;

type
  TMineOpencastPitWorkingsDlg = class(TForm)
    PnlBottom                : TPanel;
    BtnApply                 : TButton;
    BtnReset                 : TButton;
    FXMLDocumentIn           : TXMLDocument;
    FXMLDocumentOut          : TXMLDocument;
    ScrClient                : TScrollBox;
    GrdWorkings              : TWRMFGrid;
    LblWorkingsInterpolationType : TLabel;
    CbxWorkingsInterpolationType : TWRMFComboBox;
    BtnAddRow                : TSpeedButton;
    BtnDeleteRow             : TSpeedButton;
    BtnRowUp                 : TSpeedButton;
    BtnRowDown               : TSpeedButton;
    procedure FormShow(Sender: TObject);
    procedure ControlExit(Sender: TObject);
    procedure BtnApplyClick(Sender: TObject);
    procedure BtnResetClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ParamChangeIndicatorClicked (Sender: TObject);
    procedure MetaDataIndicatorClicked (Sender: TObject);
    procedure GridParamChangeIndicatorClicked (Sender: TObject; ACol, ARow: integer);
    procedure GridMetaDataIndicatorClicked (Sender: TObject; ACol, ARow: integer);
    procedure BtnAddRowClick(Sender: TObject);
    procedure BtnDeleteRowClick(Sender: TObject);
    procedure BtnRowUpClick(Sender: TObject);
    procedure BtnRowDownClick(Sender: TObject);
    procedure GrdWorkingsDataCellExit(ASender: TObject; ACol, ARow: Integer);
    procedure GrdWorkingsExit(Sender: TObject);
  private
    { Private declarations }
    FErrorList        : TStringList;
    FIdentifier       : String;
    FKeyValues        : WideString;
    FControlIterator  : TControlIterator;
    function LoadXMLData : Boolean;
    function StoreXMLData : Boolean;
    function DoValidation (AContext      : TValidationContext;
                           APropertyName : String;
                           AFieldIndex   : String) : Boolean;
    procedure SetIndicators;
    procedure SetControls;
    procedure SetAllValid;
  public
    { Public declarations }
    FMayChangeNetwork : Boolean;
    FXMLAgent         : TXMLAgentMineOpencastPitWorkings;
    FHydrologyModel   : IHydrologyModel;
  end;

implementation

uses
  UErrorHandlingOperations;

{$R *.dfm}

procedure TMineOpencastPitWorkingsDlg.FormCreate(Sender: TObject);
const OPNAME = 'TMineOpencastPitWorkingsDlg.FormCreate';
begin
  try
    FErrorList := TStringList.Create;
    FControlIterator := TControlIterator.Create;
    FControlIterator.ControlParent := ScrClient;
    FXMLAgent := TXMLAgentMineOpencastPitWorkings.Create;
    FXMLAgent.DlgXMLDocumentIn  := FXMLDocumentIn;
    FXMLAgent.DlgXMLDocumentOut := FXMLDocumentOut;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMineOpencastPitWorkingsDlg.FormDestroy(Sender: TObject);
const OPNAME = 'TMineOpencastPitWorkingsDlg.FormDestroy';
begin
  try
    FreeAndNil(FErrorList);
    FreeAndNil(FControlIterator);
    FreeAndNil(FXMLAgent);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMineOpencastPitWorkingsDlg.FormShow(Sender: TObject);
const OPNAME = 'TMineOpencastPitWorkingsDlg.FormShow';
begin
  try
    LoadXMLData;
    SetControls;
    SetIndicators;
    GrdWorkings.Cells[0, 0] := 'Year';
    GrdWorkings.Cells[1, 0] := 'Growth';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMineOpencastPitWorkingsDlg.SetControls;
const OPNAME = 'TMineOpencastPitWorkingsDlg.SetControls';
begin
  try
    FControlIterator.SetControls(FMayChangeNetwork);
    BtnAddRow.Enabled    := FMayChangeNetwork;
    BtnDeleteRow.Enabled := FMayChangeNetwork;
    BtnRowUp.Enabled     := FMayChangeNetwork;
    BtnRowDown.Enabled   := FMayChangeNetwork;

    BtnApply.Enabled     := FMayChangeNetwork;
    BtnReset.Enabled     := FMayChangeNetwork;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMineOpencastPitWorkingsDlg.SetIndicators;
const OPNAME = 'TMineOpencastPitWorkingsDlg.SetIndicators';
begin
  try
    FControlIterator.SetIndicators;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMineOpencastPitWorkingsDlg.SetAllValid;
const OPNAME = 'TMineOpencastPitWorkingsDlg.SetAllValid';
begin
  try
    FControlIterator.SetAllValid;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMineOpencastPitWorkingsDlg.LoadXMLData: boolean;
const OPNAME = 'TMineOpencastPitWorkingsDlg.LoadXMLData';
var
  LRootNode : IXMLNode;
begin
  Result := FALSE;
  try
    FXMLDocumentIn.Active  := TRUE;
    FXMLDocumentOut.Active := TRUE;
    LRootNode              := FXMLDocumentIn.DocumentElement;

    FIdentifier := LRootNode.ChildNodes['Identifier'].Text;
    FKeyValues  := 'Model='       + QuotedStr(LRootNode.ChildNodes['Model'].Text) +
                   ',NetworkID='  + QuotedStr(LRootNode.ChildNodes['NetworkID'].Text) +
                   ',ModuleType=' + QuotedStr(LRootNode.ChildNodes['ModuleType'].Text) +
                   ',Identifier=' + LRootNode.ChildNodes['Identifier'].Text;

    // InterpolationType
    CbxWorkingsInterpolationType.Items.Clear;
    CbxWorkingsInterpolationType.Items.AddObject('Linear', pointer(1));
    CbxWorkingsInterpolationType.Items.AddObject('Exponential', pointer(2));
    CbxWorkingsInterpolationType.Items.AddObject('Defined', pointer(3));

    Result := FControlIterator.LoadXMLData(LRootNode);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMineOpencastPitWorkingsDlg.StoreXMLData : Boolean;
const OPNAME = 'TMineOpencastPitWorkingsDlg.StoreXMLData';
var
  LRootNode : IXMLNode;
begin
  Result := FALSE;
  try
    LRootNode := FXMLDocumentOut.DocumentElement;
    LRootNode.ChildNodes['Identifier'].Text := FIdentifier;
    Result := FControlIterator.StoreXMLData(LRootNode);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMineOpencastPitWorkingsDlg.ControlExit(Sender: TObject);
const OPNAME = 'TMineOpencastPitWorkingsDlg.ControlExit';
var
  LControl : TWRMFControl;
begin
  try
    LControl := TWRMFControl(TControl(Sender).Parent);
    LControl.IsValid := TRUE;
    DoValidation(tcChange, LControl.PropertyName, '');
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMineOpencastPitWorkingsDlg.BtnApplyClick(Sender: TObject);
const OPNAME = 'TMineOpencastPitWorkingsDlg.BtnApplyClick';
begin
  try
    SetAllValid;
    if (DoValidation(tcApply, '', '')) then
    begin
      FHydrologyModel.UpdateNetworkData(FXMLDocumentOut.XML.Text);
      LoadXMLData;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMineOpencastPitWorkingsDlg.BtnResetClick(Sender: TObject);
const OPNAME = 'TMineOpencastPitWorkingsDlg.BtnResetClick';
begin
  try
    LoadXMLData;
    SetAllValid;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMineOpencastPitWorkingsDlg.DoValidation (AContext      : TValidationContext;
                                                   APropertyName : String;
                                                   AFieldIndex   : String) : Boolean;
const OPNAME = 'TMineOpencastPitWorkingsDlg.DoValidation';
var
  LRootNode : IXMLNode;
begin
  Result := FALSE;
  try
    if ((AContext = tcChange) OR (AContext = tcApply)) then
    begin
      Result := StoreXMLData;
      if (Result) then
      begin
        LRootNode := FXMLDocumentOut.DocumentElement;
        Result := FControlIterator.DoValidation(AContext, APropertyName, AFieldIndex,
                                      FXMLDocumentIn.XML.Text, FXMLDocumentOut.XML.Text, FALSE,
                                      FErrorList, FXMLAgent, LRootNode);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMineOpencastPitWorkingsDlg.ParamChangeIndicatorClicked (Sender: TObject);
const OPNAME = 'TMineOpencastPitWorkingsDlg.ParamChangeIndicatorClicked';
{var
  LControl : TWRMFControl;}
begin
  try
{    if (Sender is TWRMFControl) then
    begin
      LControl := TWRMFControl(Sender);
      if (LControl.HasParamChange) then
        ShowMessage ('SHOW Param Changes for ' + LControl.PropertyName + ' ' + FKeyValues)
      else
        ShowMessage ('NEW Param Changes for ' + LControl.PropertyName + ' ' + FKeyValues);
    end;}
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMineOpencastPitWorkingsDlg.MetaDataIndicatorClicked (Sender: TObject);
const OPNAME = 'TMineOpencastPitWorkingsDlg.MetaDataIndicatorClicked';
{var
  LControl : TWRMFControl;}
begin
  try
{    if (Sender is TWRMFControl) then
    begin
      LControl := TWRMFControl(Sender);
      if ((Sender is TWRMFControl) AND (TWRMFControl(Sender).HasMetaData)) then
        ShowMessage ('SHOW Meta Data for ' + LControl.PropertyName + ' ' + FKeyValues)
      else
        ShowMessage ('NEW Meta Data for ' + LControl.PropertyName + ' ' + FKeyValues);
    end;}
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMineOpencastPitWorkingsDlg.GridParamChangeIndicatorClicked (Sender: TObject; ACol, ARow: integer);
const OPNAME = 'TMineOpencastPitWorkingsDlg.GridParamChangeIndicatorClicked';
{var
  LGrid     : TWRMFGrid;
  LCellInfo : TCellInfo;}
begin
  try
{    if (Sender is TWRMFGrid) then
    begin
      LGrid := TWRMFGrid(Sender);
      LCellInfo := LGrid.CellInfo[ARow, ACol];
      if (LCellInfo.HasParamChange) then
        ShowMessage ('SHOW Param Changes for ' + LCellInfo.PropertyName + ' (' + IntToStr(ARow) + ') ' + FKeyValues)
      else
        ShowMessage ('NEW Param Changes for ' + LCellInfo.PropertyName + ' (' + IntToStr(ARow) + ') ' + FKeyValues);
    end;}
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMineOpencastPitWorkingsDlg.GridMetaDataIndicatorClicked (Sender: TObject; ACol, ARow: integer);
const OPNAME = 'TMineOpencastPitWorkingsDlg.GridMetaDataIndicatorClicked';
{var
  LGrid     : TWRMFGrid;
  LCellInfo : TCellInfo;}
begin
  try
{    if (Sender is TWRMFGrid) then
    begin
      LGrid := TWRMFGrid(Sender);
      LCellInfo := LGrid.CellInfo[ARow, ACol];
      if (LCellInfo.HasMetaData) then
        ShowMessage ('SHOW Meta Data for ' + LCellInfo.PropertyName + ' (' + IntToStr(ARow) + ') ' + FKeyValues)
      else
        ShowMessage ('NEW Meta Data for ' + LCellInfo.PropertyName + ' (' + IntToStr(ARow) + ') ' + FKeyValues);
    end;}
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMineOpencastPitWorkingsDlg.BtnAddRowClick(Sender: TObject);
const OPNAME = 'TMineOpencastPitWorkingsDlg.BtnAddRowClick';
begin
  try
    GrdWorkings.InsertRow(GrdWorkings.Row);
    if (GrdWorkings.RowCount > GrdWorkings.CellsInfo.NoOfHeadingRows) then
      GrdWorkings.FixedRows := GrdWorkings.CellsInfo.NoOfHeadingRows;
    GrdWorkings.Enabled := (GrdWorkings.RowCount > GrdWorkings.CellsInfo.NoOfHeadingRows);
    if (GrdWorkings.Enabled) then
      GrdWorkings.SetFocus;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMineOpencastPitWorkingsDlg.BtnDeleteRowClick(Sender: TObject);
const OPNAME = 'TMineOpencastPitWorkingsDlg.BtnDeleteRowClick';
begin
  try
    if (GrdWorkings.Row >= GrdWorkings.CellsInfo.NoOfHeadingRows) then
    begin
      GrdWorkings.DeleteRow(GrdWorkings.Row);
      GrdWorkings.Enabled := (GrdWorkings.RowCount > GrdWorkings.CellsInfo.NoOfHeadingRows);
      if (GrdWorkings.Enabled) then
        GrdWorkings.SetFocus;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMineOpencastPitWorkingsDlg.BtnRowUpClick(Sender: TObject);
const OPNAME = 'TMineOpencastPitWorkingsDlg.BtnRowUpClick';
begin
  try
    if (GrdWorkings.Row > GrdWorkings.FixedRows) then
    begin
      GrdWorkings.MoveRowUp(GrdWorkings.Row);
      if (GrdWorkings.Row > 0) then
        GrdWorkings.Row := GrdWorkings.Row - 1;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMineOpencastPitWorkingsDlg.BtnRowDownClick(Sender: TObject);
const OPNAME = 'TMineOpencastPitWorkingsDlg.BtnRowDownClick';
begin
  try
    if (GrdWorkings.Row >= GrdWorkings.FixedRows) AND (GrdWorkings.Row < GrdWorkings.RowCount - 1) then
    begin
      GrdWorkings.MoveRowDown(GrdWorkings.Row);
      if (GrdWorkings.Row < GrdWorkings.RowCount) then
        GrdWorkings.Row := GrdWorkings.Row + 1;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMineOpencastPitWorkingsDlg.GrdWorkingsDataCellExit(ASender: TObject; ACol, ARow: Integer);
const OPNAME = 'TMineOpencastPitWorkingsDlg.GrdWorkingsDataCellExit';
var
  LCellInfo : TCellInfo;
  LGrid     : TWRMFGrid;
begin
  try
    LGrid := TWRMFGrid(ASender);
    LCellInfo := LGrid.CellInfo[ARow, ACol];
    LCellInfo.IsValid := TRUE;
    DoValidation(tcChange, TWRMFGrid(ASender).CellInfo[ARow, ACol].PropertyName, IntToStr(ARow));
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMineOpencastPitWorkingsDlg.GrdWorkingsExit(Sender: TObject);
const OPNAME = 'TMineOpencastPitWorkingsDlg.GrdWorkingsExit';
var
  LWRMFGrid : TWRMFGrid;
begin
  try
    LWRMFGrid := TWRMFGrid(Sender);
    GrdWorkingsDataCellExit(LWRMFGrid, LWRMFGrid.Col, LWRMFGrid.Row);
  except on E: Exception do HandleError(E, OPNAME) end;
end;


end.
