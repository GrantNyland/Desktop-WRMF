unit UMineOpencastPitDecantDlg;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, VCL.Graphics, VCL.Controls, VCL.Forms,
  VCL.Dialogs, VCL.StdCtrls, xmldom, XMLIntf, XMLDoc, VCL.ExtCtrls, VCL.Buttons, VCL.Menus,

  UXMLAgent,
  UControlIterator,
  HydrologyCom_TLB,
  UXMLAgentMineOpencastPitDecant,
  UWRMFEdit, UWRMFComboBox, UWRMFControl, UWRMFCheckBox, VCL.ComCtrls,
  VCL.Grids, UCellChangeStringGrid, UWRMFGrid, Xml.Win.msxmldom;

type
  TMineOpencastPitDecantDlg = class(TForm)
    PnlBottom                : TPanel;
    BtnApply                 : TButton;
    BtnReset                 : TButton;
    FXMLDocumentIn           : TXMLDocument;
    FXMLDocumentOut          : TXMLDocument;
    ScrClient                : TScrollBox;
    GrdDecant                : TWRMFGrid;
    LblDecantInterpolationType: TLabel;
    CbxDecantInterpolationType: TWRMFComboBox;
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
    procedure GrdDecantDataCellExit(ASender: TObject; ACol, ARow: Integer);
    procedure GrdDecantExit(Sender: TObject);
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
    FXMLAgent         : TXMLAgentMineOpencastPitDecant;
    FHydrologyModel   : IHydrologyModel;
  end;

implementation

uses
  UErrorHandlingOperations;

{$R *.dfm}

procedure TMineOpencastPitDecantDlg.FormCreate(Sender: TObject);
const OPNAME = 'TMineOpencastPitDecantDlg.FormCreate';
begin
  try
    FErrorList := TStringList.Create;
    FControlIterator := TControlIterator.Create;
    FControlIterator.ControlParent := ScrClient;
    FXMLAgent := TXMLAgentMineOpencastPitDecant.Create;
    FXMLAgent.DlgXMLDocumentIn  := FXMLDocumentIn;
    FXMLAgent.DlgXMLDocumentOut := FXMLDocumentOut;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMineOpencastPitDecantDlg.FormDestroy(Sender: TObject);
const OPNAME = 'TMineOpencastPitDecantDlg.FormDestroy';
begin
  try
    FreeAndNil(FErrorList);
    FreeAndNil(FControlIterator);
    FreeAndNil(FXMLAgent);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMineOpencastPitDecantDlg.FormShow(Sender: TObject);
const OPNAME = 'TMineOpencastPitDecantDlg.FormShow';
begin
  try
    LoadXMLData;
    SetControls;
    SetIndicators;
    GrdDecant.Cells[0, 0] := 'Year';
    GrdDecant.Cells[1, 0] := 'Growth';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMineOpencastPitDecantDlg.SetControls;
const OPNAME = 'TMineOpencastPitDecantDlg.SetControls';
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

procedure TMineOpencastPitDecantDlg.SetIndicators;
const OPNAME = 'TMineOpencastPitDecantDlg.SetIndicators';
begin
  try
    FControlIterator.SetIndicators;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMineOpencastPitDecantDlg.SetAllValid;
const OPNAME = 'TMineOpencastPitDecantDlg.SetAllValid';
begin
  try
    FControlIterator.SetAllValid;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMineOpencastPitDecantDlg.LoadXMLData: boolean;
const OPNAME = 'TMineOpencastPitDecantDlg.LoadXMLData';
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
    CbxDecantInterpolationType.Items.Clear;
    CbxDecantInterpolationType.Items.AddObject('Linear', pointer(1));
    CbxDecantInterpolationType.Items.AddObject('Exponential', pointer(2));
    CbxDecantInterpolationType.Items.AddObject('Defined', pointer(3));

    Result := FControlIterator.LoadXMLData(LRootNode);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMineOpencastPitDecantDlg.StoreXMLData : Boolean;
const OPNAME = 'TMineOpencastPitDecantDlg.StoreXMLData';
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

procedure TMineOpencastPitDecantDlg.ControlExit(Sender: TObject);
const OPNAME = 'TMineOpencastPitDecantDlg.ControlExit';
var
  LControl : TWRMFControl;
begin
  try
    LControl := TWRMFControl(TControl(Sender).Parent);
    LControl.IsValid := TRUE;
    DoValidation(tcChange, LControl.PropertyName, '');
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMineOpencastPitDecantDlg.BtnApplyClick(Sender: TObject);
const OPNAME = 'TMineOpencastPitDecantDlg.BtnApplyClick';
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

procedure TMineOpencastPitDecantDlg.BtnResetClick(Sender: TObject);
const OPNAME = 'TMineOpencastPitDecantDlg.BtnResetClick';
begin
  try
    LoadXMLData;
    SetAllValid;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMineOpencastPitDecantDlg.DoValidation (AContext      : TValidationContext;
                                                 APropertyName : String;
                                                 AFieldIndex   : String) : Boolean;
const OPNAME = 'TMineOpencastPitDecantDlg.DoValidation';
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

procedure TMineOpencastPitDecantDlg.ParamChangeIndicatorClicked (Sender: TObject);
const OPNAME = 'TMineOpencastPitDecantDlg.ParamChangeIndicatorClicked';
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

procedure TMineOpencastPitDecantDlg.MetaDataIndicatorClicked (Sender: TObject);
const OPNAME = 'TMineOpencastPitDecantDlg.MetaDataIndicatorClicked';
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

procedure TMineOpencastPitDecantDlg.GridParamChangeIndicatorClicked (Sender: TObject; ACol, ARow: integer);
const OPNAME = 'TMineOpencastPitDecantDlg.GridParamChangeIndicatorClicked';
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

procedure TMineOpencastPitDecantDlg.GridMetaDataIndicatorClicked (Sender: TObject; ACol, ARow: integer);
const OPNAME = 'TMineOpencastPitDecantDlg.GridMetaDataIndicatorClicked';
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

procedure TMineOpencastPitDecantDlg.BtnAddRowClick(Sender: TObject);
const OPNAME = 'TMineOpencastPitDecantDlg.BtnAddRowClick';
begin
  try
    GrdDecant.InsertRow(GrdDecant.Row);
    if (GrdDecant.RowCount > GrdDecant.CellsInfo.NoOfHeadingRows) then
      GrdDecant.FixedRows := GrdDecant.CellsInfo.NoOfHeadingRows;
    GrdDecant.Enabled := (GrdDecant.RowCount > GrdDecant.CellsInfo.NoOfHeadingRows);
    if (GrdDecant.Enabled) then
      GrdDecant.SetFocus;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMineOpencastPitDecantDlg.BtnDeleteRowClick(Sender: TObject);
const OPNAME = 'TMineOpencastPitDecantDlg.BtnDeleteRowClick';
begin
  try
    if (GrdDecant.Row >= GrdDecant.CellsInfo.NoOfHeadingRows) then
    begin
      GrdDecant.DeleteRow(GrdDecant.Row);
      GrdDecant.Enabled := (GrdDecant.RowCount > GrdDecant.CellsInfo.NoOfHeadingRows);
      if (GrdDecant.Enabled) then
        GrdDecant.SetFocus;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMineOpencastPitDecantDlg.BtnRowUpClick(Sender: TObject);
const OPNAME = 'TMineOpencastPitDecantDlg.BtnRowUpClick';
begin
  try
    if (GrdDecant.Row > GrdDecant.FixedRows) then
    begin
      GrdDecant.MoveRowUp(GrdDecant.Row);
      if (GrdDecant.Row > 0) then
        GrdDecant.Row := GrdDecant.Row - 1;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMineOpencastPitDecantDlg.BtnRowDownClick(Sender: TObject);
const OPNAME = 'TMineOpencastPitDecantDlg.BtnRowDownClick';
begin
  try
    if (GrdDecant.Row >= GrdDecant.FixedRows) AND (GrdDecant.Row < GrdDecant.RowCount - 1) then
    begin
      GrdDecant.MoveRowDown(GrdDecant.Row);
      if (GrdDecant.Row < GrdDecant.RowCount) then
        GrdDecant.Row := GrdDecant.Row + 1;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMineOpencastPitDecantDlg.GrdDecantDataCellExit(ASender: TObject; ACol, ARow: Integer);
const OPNAME = 'TMineOpencastPitDecantDlg.GrdDecantDataCellExit';
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

procedure TMineOpencastPitDecantDlg.GrdDecantExit(Sender: TObject);
const OPNAME = 'TMineOpencastPitDecantDlg.GrdDecantExit';
var
  LWRMFGrid : TWRMFGrid;
begin
  try
    LWRMFGrid := TWRMFGrid(Sender);
    GrdDecantDataCellExit(LWRMFGrid, LWRMFGrid.Col, LWRMFGrid.Row);
  except on E: Exception do HandleError(E, OPNAME) end;
end;


end.
