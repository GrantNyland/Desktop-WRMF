unit UMineUndergroundBoardPillarDlg;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, VCL.Graphics, VCL.Controls, VCL.Forms,
  VCL.Dialogs, VCL.StdCtrls, xmldom, XMLIntf, XMLDoc, VCL.ExtCtrls, VCL.Buttons, VCL.Menus,

  HydrologyCom_TLB,
  UControlIterator,
  UXMLAgent,
  UXMLAgentMineUndergroundBoardPillar,
  UWRMFEdit, UWRMFComboBox, UWRMFControl, UWRMFCheckBox, VCL.ComCtrls,
  VCL.Grids, UCellChangeStringGrid, UWRMFGrid;

type
  TMineUndergroundBoardPillarDlg = class(TForm)
    PnlBottom                : TPanel;
    BtnApply                 : TButton;
    BtnReset                 : TButton;
    FXMLDocumentIn           : TXMLDocument;
    FXMLDocumentOut          : TXMLDocument;
    ScrClient                : TScrollBox;
    GrdBoardPillar           : TWRMFGrid;
    LblBoardAndPillarInterpolationOption: TLabel;
    CbxBoardAndPillarInterpolationOption: TWRMFComboBox;
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
    procedure GrdBoardPillarDataCellExit(ASender: TObject; ACol, ARow: Integer);
    procedure GrdBoardPillarExit(Sender: TObject);
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
    FXMLAgent         : TXMLAgentMineUndergroundBoardPillar;
    FHydrologyModel   : IHydrologyModel;
  end;

implementation

uses
  UErrorHandlingOperations;

{$R *.dfm}

procedure TMineUndergroundBoardPillarDlg.FormCreate(Sender: TObject);
const OPNAME = 'TMineUndergroundBoardPillarDlg.FormCreate';
begin
  try
    FErrorList := TStringList.Create;
    FControlIterator := TControlIterator.Create;
    FControlIterator.ControlParent := ScrClient;
    FXMLAgent := TXMLAgentMineUndergroundBoardPillar.Create;
    FXMLAgent.DlgXMLDocumentIn  := FXMLDocumentIn;
    FXMLAgent.DlgXMLDocumentOut := FXMLDocumentOut;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMineUndergroundBoardPillarDlg.FormDestroy(Sender: TObject);
const OPNAME = 'TMineUndergroundBoardPillarDlg.FormDestroy';
begin
  try
    FreeAndNil(FErrorList);
    FreeAndNil(FControlIterator);
    FreeAndNil(FXMLAgent);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMineUndergroundBoardPillarDlg.FormShow(Sender: TObject);
const OPNAME = 'TMineUndergroundBoardPillarDlg.FormShow';
begin
  try
    LoadXMLData;
    SetControls;
    SetIndicators;
    GrdBoardPillar.Cells[0, 0] := 'Year';
    GrdBoardPillar.Cells[1, 0] := 'Growth';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMineUndergroundBoardPillarDlg.SetControls;
const OPNAME = 'TMineUndergroundBoardPillarDlg.SetControls';
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

procedure TMineUndergroundBoardPillarDlg.SetIndicators;
const OPNAME = 'TMineUndergroundBoardPillarDlg.SetIndicators';
begin
  try
    FControlIterator.SetIndicators;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMineUndergroundBoardPillarDlg.SetAllValid;
const OPNAME = 'TMineUndergroundBoardPillarDlg.SetAllValid';
begin
  try
    FControlIterator.SetAllValid;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMineUndergroundBoardPillarDlg.LoadXMLData: boolean;
const OPNAME = 'TMineUndergroundBoardPillarDlg.LoadXMLData';
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
    CbxBoardAndPillarInterpolationOption.Items.Clear;
    CbxBoardAndPillarInterpolationOption.Items.AddObject('Linear', pointer(1));
    CbxBoardAndPillarInterpolationOption.Items.AddObject('Exponential', pointer(2));
    CbxBoardAndPillarInterpolationOption.Items.AddObject('Defined', pointer(3));

    Result := FControlIterator.LoadXMLData(LRootNode);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMineUndergroundBoardPillarDlg.StoreXMLData : Boolean;
const OPNAME = 'TMineUndergroundBoardPillarDlg.StoreXMLData';
var
  LRootNode     : IXMLNode;
begin
  Result := FALSE;
  try
    LRootNode := FXMLDocumentOut.DocumentElement;
    LRootNode.ChildNodes['Identifier'].Text := FIdentifier;
    Result := FControlIterator.StoreXMLData(LRootNode);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMineUndergroundBoardPillarDlg.ControlExit(Sender: TObject);
const OPNAME = 'TMineUndergroundBoardPillarDlg.ControlExit';
var
  LControl : TWRMFControl;
begin
  try
    LControl := TWRMFControl(TControl(Sender).Parent);
    LControl.IsValid := TRUE;
    DoValidation(tcChange, LControl.PropertyName, '');
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMineUndergroundBoardPillarDlg.BtnApplyClick(Sender: TObject);
const OPNAME = 'TMineUndergroundBoardPillarDlg.BtnApplyClick';
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

procedure TMineUndergroundBoardPillarDlg.BtnResetClick(Sender: TObject);
const OPNAME = 'TMineUndergroundBoardPillarDlg.BtnResetClick';
begin
  try
    LoadXMLData;
    SetAllValid;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMineUndergroundBoardPillarDlg.DoValidation (AContext      : TValidationContext;
                                                      APropertyName : String;
                                                      AFieldIndex   : String) : Boolean;
const OPNAME = 'TMineUndergroundBoardPillarDlg.DoValidation';
var
  lRootNode : IXMLNode;
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

procedure TMineUndergroundBoardPillarDlg.ParamChangeIndicatorClicked (Sender: TObject);
const OPNAME = 'TMineUndergroundBoardPillarDlg.ParamChangeIndicatorClicked';
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

procedure TMineUndergroundBoardPillarDlg.MetaDataIndicatorClicked (Sender: TObject);
const OPNAME = 'TMineUndergroundBoardPillarDlg.MetaDataIndicatorClicked';
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

procedure TMineUndergroundBoardPillarDlg.GridParamChangeIndicatorClicked (Sender: TObject; ACol, ARow: integer);
const OPNAME = 'TMineUndergroundBoardPillarDlg.GridParamChangeIndicatorClicked';
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

procedure TMineUndergroundBoardPillarDlg.GridMetaDataIndicatorClicked (Sender: TObject; ACol, ARow: integer);
const OPNAME = 'TMineUndergroundBoardPillarDlg.GridMetaDataIndicatorClicked';
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

procedure TMineUndergroundBoardPillarDlg.BtnAddRowClick(Sender: TObject);
const OPNAME = 'TMineUndergroundBoardPillarDlg.BtnAddRowClick';
begin
  try
    GrdBoardPillar.InsertRow(GrdBoardPillar.Row);
    if (GrdBoardPillar.RowCount > GrdBoardPillar.CellsInfo.NoOfHeadingRows) then
      GrdBoardPillar.FixedRows := GrdBoardPillar.CellsInfo.NoOfHeadingRows;
    GrdBoardPillar.Enabled := (GrdBoardPillar.RowCount > GrdBoardPillar.CellsInfo.NoOfHeadingRows);
    if (GrdBoardPillar.Enabled) then
      GrdBoardPillar.SetFocus;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMineUndergroundBoardPillarDlg.BtnDeleteRowClick(Sender: TObject);
const OPNAME = 'TMineUndergroundBoardPillarDlg.BtnDeleteRowClick';
begin
  try
    if (GrdBoardPillar.Row >= GrdBoardPillar.CellsInfo.NoOfHeadingRows) then
    begin
      GrdBoardPillar.DeleteRow(GrdBoardPillar.Row);
      GrdBoardPillar.Enabled := (GrdBoardPillar.RowCount > GrdBoardPillar.CellsInfo.NoOfHeadingRows);
      if (GrdBoardPillar.Enabled) then
        GrdBoardPillar.SetFocus;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMineUndergroundBoardPillarDlg.BtnRowUpClick(Sender: TObject);
const OPNAME = 'TMineUndergroundBoardPillarDlg.BtnRowUpClick';
begin
  try
    if (GrdBoardPillar.Row > GrdBoardPillar.FixedRows) then
    begin
      GrdBoardPillar.MoveRowUp(GrdBoardPillar.Row);
      if (GrdBoardPillar.Row > 0) then
        GrdBoardPillar.Row := GrdBoardPillar.Row - 1;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMineUndergroundBoardPillarDlg.BtnRowDownClick(Sender: TObject);
const OPNAME = 'TMineUndergroundBoardPillarDlg.BtnRowDownClick';
begin
  try
    if (GrdBoardPillar.Row >= GrdBoardPillar.FixedRows) AND (GrdBoardPillar.Row < GrdBoardPillar.RowCount - 1) then
    begin
      GrdBoardPillar.MoveRowDown(GrdBoardPillar.Row);
      if (GrdBoardPillar.Row < GrdBoardPillar.RowCount) then
        GrdBoardPillar.Row := GrdBoardPillar.Row + 1;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMineUndergroundBoardPillarDlg.GrdBoardPillarDataCellExit(ASender: TObject; ACol, ARow: Integer);
const OPNAME = 'TMineUndergroundBoardPillarDlg.GrdBoardPillarDataCellExit';
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

procedure TMineUndergroundBoardPillarDlg.GrdBoardPillarExit(Sender: TObject);
const OPNAME = 'TMineUndergroundBoardPillarDlg.GrdBoardPillarExit';
var
  LWRMFGrid : TWRMFGrid;
begin
  try
    LWRMFGrid := TWRMFGrid(Sender);
    GrdBoardPillarDataCellExit(LWRMFGrid, LWRMFGrid.Col, LWRMFGrid.Row);
  except on E: Exception do HandleError(E, OPNAME) end;
end;


end.
