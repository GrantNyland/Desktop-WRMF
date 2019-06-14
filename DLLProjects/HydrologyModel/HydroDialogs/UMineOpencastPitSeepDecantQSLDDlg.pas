unit UMineOpencastPitSeepDecantQSLDDlg;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, VCL.Graphics, VCL.Controls, VCL.Forms,
  VCL.Dialogs, VCL.StdCtrls, xmldom, XMLIntf, XMLDoc, VCL.ExtCtrls, VCL.Buttons, VCL.Menus,

  UXMLAgent,
  UControlIterator,
  HydrologyCom_TLB,
  UXMLAgentMineOpencastPitSeepDecantQSLD,
  UWRMFEdit, UWRMFComboBox, UWRMFControl, UWRMFCheckBox, VCL.ComCtrls,
  VCL.Grids, UCellChangeStringGrid, UWRMFGrid, Xml.Win.msxmldom;

type
  TMineOpencastPitSeepDecantQSLDDlg = class(TForm)
    PnlBottom                : TPanel;
    BtnApply                 : TButton;
    BtnReset                 : TButton;
    FXMLDocumentIn           : TXMLDocument;
    FXMLDocumentOut          : TXMLDocument;
    ScrClient                : TScrollBox;
    GrdSeepDecantQSLD        : TWRMFGrid;
    LblStdDevSeepageDecant   : TLabel;
    EdtStdDevSeepageDecant   : TWRMFEdit;
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
    procedure GrdSeepDecantQSLDDataCellExit(ASender: TObject; ACol, ARow: Integer);
    procedure GrdSeepDecantQSLDExit(Sender: TObject);
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
    FXMLAgent         : TXMLAgentMineOpencastPitSeepDecantQSLD;
    FHydrologyModel   : IHydrologyModel;
  end;

implementation

uses
  UErrorHandlingOperations;

{$R *.dfm}

procedure TMineOpencastPitSeepDecantQSLDDlg.FormCreate(Sender: TObject);
const OPNAME = 'TMineOpencastPitSeepDecantQSLDDlg.FormCreate';
begin
  try
    FErrorList := TStringList.Create;
    FControlIterator := TControlIterator.Create;
    FControlIterator.ControlParent := ScrClient;
    FXMLAgent := TXMLAgentMineOpencastPitSeepDecantQSLD.Create;
    FXMLAgent.DlgXMLDocumentIn  := FXMLDocumentIn;
    FXMLAgent.DlgXMLDocumentOut := FXMLDocumentOut;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMineOpencastPitSeepDecantQSLDDlg.FormDestroy(Sender: TObject);
const OPNAME = 'TMineOpencastPitSeepDecantQSLDDlg.FormDestroy';
begin
  try
    FreeAndNil(FErrorList);
    FreeAndNil(FControlIterator);
    FreeAndNil(FXMLAgent);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMineOpencastPitSeepDecantQSLDDlg.FormShow(Sender: TObject);
const OPNAME = 'TMineOpencastPitSeepDecantQSLDDlg.FormShow';
begin
  try
    LoadXMLData;
    SetControls;
    SetIndicators;
    GrdSeepDecantQSLD.Cells[0, 0] := 'Flow';
    GrdSeepDecantQSLD.Cells[1, 0] := 'Load';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMineOpencastPitSeepDecantQSLDDlg.SetControls;
const OPNAME = 'TMineOpencastPitSeepDecantQSLDDlg.SetControls';
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

procedure TMineOpencastPitSeepDecantQSLDDlg.SetIndicators;
const OPNAME = 'TMineOpencastPitSeepDecantQSLDDlg.SetIndicators';
begin
  try
    FControlIterator.SetIndicators;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMineOpencastPitSeepDecantQSLDDlg.SetAllValid;
const OPNAME = 'TMineOpencastPitSeepDecantQSLDDlg.SetAllValid';
begin
  try
    FControlIterator.SetAllValid;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMineOpencastPitSeepDecantQSLDDlg.LoadXMLData: boolean;
const OPNAME = 'TMineOpencastPitSeepDecantQSLDDlg.LoadXMLData';
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

    Result := FControlIterator.LoadXMLData(LRootNode);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMineOpencastPitSeepDecantQSLDDlg.StoreXMLData : Boolean;
const OPNAME = 'TMineOpencastPitSeepDecantQSLDDlg.StoreXMLData';
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

procedure TMineOpencastPitSeepDecantQSLDDlg.ControlExit(Sender: TObject);
const OPNAME = 'TMineOpencastPitSeepDecantQSLDDlg.ControlExit';
var
  LControl : TWRMFControl;
begin
  try
    LControl := TWRMFControl(TControl(Sender).Parent);
    LControl.IsValid := TRUE;
    DoValidation(tcChange, LControl.PropertyName, '');
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMineOpencastPitSeepDecantQSLDDlg.BtnApplyClick(Sender: TObject);
const OPNAME = 'TMineOpencastPitSeepDecantQSLDDlg.BtnApplyClick';
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

procedure TMineOpencastPitSeepDecantQSLDDlg.BtnResetClick(Sender: TObject);
const OPNAME = 'TMineOpencastPitSeepDecantQSLDDlg.BtnResetClick';
begin
  try
    LoadXMLData;
    SetAllValid;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMineOpencastPitSeepDecantQSLDDlg.DoValidation (AContext      : TValidationContext;
                                                         APropertyName : String;
                                                         AFieldIndex   : String) : Boolean;
const OPNAME = 'TMineOpencastPitSeepDecantQSLDDlg.DoValidation';
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

procedure TMineOpencastPitSeepDecantQSLDDlg.ParamChangeIndicatorClicked (Sender: TObject);
const OPNAME = 'TMineOpencastPitSeepDecantQSLDDlg.ParamChangeIndicatorClicked';
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

procedure TMineOpencastPitSeepDecantQSLDDlg.MetaDataIndicatorClicked (Sender: TObject);
const OPNAME = 'TMineOpencastPitSeepDecantQSLDDlg.MetaDataIndicatorClicked';
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

procedure TMineOpencastPitSeepDecantQSLDDlg.GridParamChangeIndicatorClicked (Sender: TObject; ACol, ARow: integer);
const OPNAME = 'TMineOpencastPitSeepDecantQSLDDlg.GridParamChangeIndicatorClicked';
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

procedure TMineOpencastPitSeepDecantQSLDDlg.GridMetaDataIndicatorClicked (Sender: TObject; ACol, ARow: integer);
const OPNAME = 'TMineOpencastPitSeepDecantQSLDDlg.GridMetaDataIndicatorClicked';
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

procedure TMineOpencastPitSeepDecantQSLDDlg.BtnAddRowClick(Sender: TObject);
const OPNAME = 'TMineOpencastPitSeepDecantQSLDDlg.BtnAddRowClick';
begin
  try
    GrdSeepDecantQSLD.InsertRow(GrdSeepDecantQSLD.Row);
    if (GrdSeepDecantQSLD.RowCount > GrdSeepDecantQSLD.CellsInfo.NoOfHeadingRows) then
      GrdSeepDecantQSLD.FixedRows := GrdSeepDecantQSLD.CellsInfo.NoOfHeadingRows;
    GrdSeepDecantQSLD.Enabled := (GrdSeepDecantQSLD.RowCount > GrdSeepDecantQSLD.CellsInfo.NoOfHeadingRows);
    if (GrdSeepDecantQSLD.Enabled) then
      GrdSeepDecantQSLD.SetFocus;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMineOpencastPitSeepDecantQSLDDlg.BtnDeleteRowClick(Sender: TObject);
const OPNAME = 'TMineOpencastPitSeepDecantQSLDDlg.BtnDeleteRowClick';
begin
  try
    if (GrdSeepDecantQSLD.Row >= GrdSeepDecantQSLD.CellsInfo.NoOfHeadingRows) then
    begin
      GrdSeepDecantQSLD.DeleteRow(GrdSeepDecantQSLD.Row);
      GrdSeepDecantQSLD.Enabled := (GrdSeepDecantQSLD.RowCount > GrdSeepDecantQSLD.CellsInfo.NoOfHeadingRows);
      if (GrdSeepDecantQSLD.Enabled) then
        GrdSeepDecantQSLD.SetFocus;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMineOpencastPitSeepDecantQSLDDlg.BtnRowUpClick(Sender: TObject);
const OPNAME = 'TMineOpencastPitSeepDecantQSLDDlg.BtnRowUpClick';
begin
  try
    if (GrdSeepDecantQSLD.Row > GrdSeepDecantQSLD.FixedRows) then
    begin
      GrdSeepDecantQSLD.MoveRowUp(GrdSeepDecantQSLD.Row);
      if (GrdSeepDecantQSLD.Row > 0) then
        GrdSeepDecantQSLD.Row := GrdSeepDecantQSLD.Row - 1;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMineOpencastPitSeepDecantQSLDDlg.BtnRowDownClick(Sender: TObject);
const OPNAME = 'TMineOpencastPitSeepDecantQSLDDlg.BtnRowDownClick';
begin
  try
    if (GrdSeepDecantQSLD.Row >= GrdSeepDecantQSLD.FixedRows) AND (GrdSeepDecantQSLD.Row < GrdSeepDecantQSLD.RowCount - 1) then
    begin
      GrdSeepDecantQSLD.MoveRowDown(GrdSeepDecantQSLD.Row);
      if (GrdSeepDecantQSLD.Row < GrdSeepDecantQSLD.RowCount) then
        GrdSeepDecantQSLD.Row := GrdSeepDecantQSLD.Row + 1;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMineOpencastPitSeepDecantQSLDDlg.GrdSeepDecantQSLDDataCellExit(ASender: TObject; ACol, ARow: Integer);
const OPNAME = 'TMineOpencastPitSeepDecantQSLDDlg.GrdSeepDecantQSLDDataCellExit';
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

procedure TMineOpencastPitSeepDecantQSLDDlg.GrdSeepDecantQSLDExit(Sender: TObject);
const OPNAME = 'TMineOpencastPitSeepDecantQSLDDlg.GrdSeepDecantQSLDExit';
var
  LWRMFGrid : TWRMFGrid;
begin
  try
    LWRMFGrid := TWRMFGrid(Sender);
    GrdSeepDecantQSLDDataCellExit(LWRMFGrid, LWRMFGrid.Col, LWRMFGrid.Row);
  except on E: Exception do HandleError(E, OPNAME) end;
end;


end.
