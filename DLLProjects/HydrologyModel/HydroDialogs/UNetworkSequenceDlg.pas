unit UNetworkSequenceDlg;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, VCL.Graphics, VCL.Controls, VCL.Forms,
  VCL.Dialogs, VCL.StdCtrls, xmldom, XMLIntf, XMLDoc, VCL.ExtCtrls, VCL.Buttons, VCL.Menus,

  UHostDlg,
  HydrologyCom_TLB,
  UXMLAgent,
  UXMLAgentNetworkSequence,
  UWRMFEdit, UWRMFComboBox, UWRMFControl, UWRMFCheckBox, VCL.ComCtrls,
  VCL.Grids, UCellChangeStringGrid, UWRMFGrid;

type
  TNetworkSequenceDlg = class(TForm)
    FXMLDocumentIn           : TXMLDocument;
    FXMLDocumentOut          : TXMLDocument;
    PnlBottom                : TPanel;
    BtnReset                 : TButton;
    BtnApply                 : TButton;
    ScrClient                : TScrollBox;
    BtnRowUp                 : TSpeedButton;
    BtnRowDown               : TSpeedButton;
    GrdModules               : TStringGrid;
    procedure FormShow(Sender: TObject);
    procedure BtnApplyClick(Sender: TObject);
    procedure BtnResetClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure BtnRowUpClick(Sender: TObject);
    procedure BtnRowDownClick(Sender: TObject);
  private
    { Private declarations }
    function LoadXMLData : Boolean;
    function StoreXMLData : Boolean;
    procedure SetControls;
  public
    { Public declarations }
    FMayChangeNetwork : Boolean;
//    FXSDText          : String;
    FXMLAgent         : TXMLAgentNetworkSequence;
    FHydrologyModel   : IHydrologyModel;
    FModuleID         : Integer;
  end;

implementation

uses
  UErrorHandlingOperations;

{$R *.dfm}

procedure TNetworkSequenceDlg.FormCreate(Sender: TObject);
const OPNAME = 'TNetworkSequenceDlg.FormCreate';
begin
  try
    FXMLAgent := TXMLAgentNetworkSequence.Create;
    FXMLAgent.DlgXMLDocumentIn  := FXMLDocumentIn;
    FXMLAgent.DlgXMLDocumentOut := FXMLDocumentOut;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TNetworkSequenceDlg.FormDestroy(Sender: TObject);
const OPNAME = 'TNetworkSequenceDlg.FormDestroy';
begin
  try
    FreeAndNil(FXMLAgent);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TNetworkSequenceDlg.FormShow(Sender: TObject);
const OPNAME = 'TNetworkSequenceDlg.FormShow';
begin
  try
    LoadXMLData;
    SetControls;
    GrdModules.Cells[0, 0] := 'Sequence';
    GrdModules.Cells[1, 0] := 'Module';
    BtnApply.Enabled := FALSE;
    BtnReset.Enabled := FALSE;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TNetworkSequenceDlg.SetControls;
const OPNAME = 'TNetworkSequenceDlg.SetControls';
begin
  try
    BtnRowUp.Enabled     := FMayChangeNetwork;
    BtnRowDown.Enabled   := FMayChangeNetwork;

    BtnApply.Enabled     := FMayChangeNetwork;
    BtnReset.Enabled     := FMayChangeNetwork;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TNetworkSequenceDlg.LoadXMLData : Boolean;
const OPNAME = 'TNetworkSequenceDlg.LoadXMLData';
var
  LRootNode        : IXMLNode;
  LSectionNode     : IXMLNode;
  LDataListNode    : IXMLNode;
  LNode            : IXMLNode;
  LIndex           : Integer;
  LModuleID        : Integer;
  LModuleText      : String;
  LRow             : Integer;
begin
  Result := FALSE;
  try
    LRow := 1;
    FXMLDocumentIn.Active  := TRUE;
    FXMLDocumentOut.Active := TRUE;
    LRootNode     := FXMLDocumentIn.DocumentElement;
    LSectionNode  := LRootNode.ChildNodes['ChangeNetworkSequence'];
    LDataListNode := LSectionNode.ChildNodes['DataList'];
    GrdModules.RowCount := 1 + LDataListNode.ChildNodes.Count;
    for LIndex := 1 to LDataListNode.ChildNodes.Count do
    begin
      LNode            := LDataListNode.ChildNodes.Get(LIndex-1);
      LModuleID        := StrToInt(LNode.ChildNodes['ModuleID'].Text);
      if (LModuleID = FModuleID) then
        LRow := LIndex;
      LModuleText      := LNode.ChildNodes['ModuleText'].Text;
      GrdModules.Cells[0, LIndex] := IntToStr(LIndex);
      GrdModules.Cells[1, LIndex] := LModuleText;
    end;
    GrdModules.Row := LRow;
//    Result := FControlIterator.LoadXMLData(LRootNode);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TNetworkSequenceDlg.StoreXMLData : Boolean;
const OPNAME = 'TNetworkSequenceDlg.StoreXMLData';
var
  LRootNode        : IXMLNode;
  LSectionNode     : IXMLNode;
  LDataListNode    : IXMLNode;
  LNode            : IXMLNode;
  LRow             : Integer;
  LModuleText      : String;
  LCount           : Integer;
  LFound           : Boolean;
begin
  Result := FALSE;
  try
    Result := TRUE;
    LRootNode := FXMLDocumentOut.DocumentElement;
    LSectionNode  := LRootNode.ChildNodes['ChangeNetworkSequence'];
    LDataListNode := LSectionNode.ChildNodes['DataList'];
    for LRow := 1 to GrdModules.RowCount - 1 do
    begin
      LModuleText := GrdModules.Cells[1, LRow];
      LFound := FALSE;
      LCount := 0;
      while ((NOT LFound) AND (LCount < LDataListNode.ChildNodes.Count)) do
      begin
        LNode := LDataListNode.ChildNodes.Get(LCount);
        if (LNode.ChildNodes['ModuleText'].Text = LModuleText) then
        begin
          LNode.ChildNodes['NetworkSequence'].Text := IntToStr(LRow);
          LFound := TRUE;
        end
        else
          LCount := LCount + 1;
      end;
      Result := Result AND LFound;
    end;
//    Result    := FControlIterator.StoreXMLData(LRootNode);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TNetworkSequenceDlg.BtnApplyClick(Sender: TObject);
const OPNAME = 'TNetworkSequenceDlg.BtnApplyClick';
begin
  try
    if (StoreXMLData) then
    begin
      ModalResult := mrOk;
      FHydrologyModel.UpdateNetworkData(FXMLDocumentOut.XML.Text);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TNetworkSequenceDlg.BtnResetClick(Sender: TObject);
const OPNAME = 'TNetworkSequenceDlg.BtnResetClick';
begin
  try
//    ModalResult := mrCancel;
    BtnApply.Enabled := FALSE;
    BtnReset.Enabled := FALSE;
    LoadXMLData;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TNetworkSequenceDlg.BtnRowUpClick(Sender: TObject);
const OPNAME = 'TNetworkSequenceDlg.BtnRowUpClick';
var
  LTemp     : String;
  LRow      : Integer;
begin
  try
    if (GrdModules.Row > GrdModules.FixedRows) then
    begin
//      GrdModules.MoveRowUp(GrdModules.Row);
      LRow := GrdModules.Row;
      LTemp := GrdModules.Cells[1, LRow];
      GrdModules.Cells[1, LRow] := GrdModules.Cells[1, LRow-1];
      GrdModules.Cells[1, LRow-1] := LTemp;

      if (GrdModules.Row > 0) then
        GrdModules.Row := GrdModules.Row - 1;

      BtnApply.Enabled := TRUE;
      BtnReset.Enabled := TRUE;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TNetworkSequenceDlg.BtnRowDownClick(Sender: TObject);
const OPNAME = 'TNetworkSequenceDlg.BtnRowDownClick';
var
  LTemp     : String;
  LRow      : Integer;
begin
  try
    if (GrdModules.Row >= GrdModules.FixedRows) AND (GrdModules.Row < GrdModules.RowCount - 1) then
    begin
//      GrdModules.MoveRowDown(GrdModules.Row);
      LRow := GrdModules.Row;
      LTemp := GrdModules.Cells[1, LRow];
      GrdModules.Cells[1, LRow] := GrdModules.Cells[1, LRow+1];
      GrdModules.Cells[1, LRow+1] := LTemp;
      if (GrdModules.Row < GrdModules.RowCount) then
        GrdModules.Row := GrdModules.Row + 1;

      BtnApply.Enabled := TRUE;
      BtnReset.Enabled := TRUE;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


end.
