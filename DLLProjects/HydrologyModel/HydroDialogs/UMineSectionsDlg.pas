unit UMineSectionsDlg;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, VCL.Graphics, VCL.Controls, VCL.Forms,
  VCL.Dialogs, VCL.StdCtrls, xmldom, XMLIntf, XMLDoc, VCL.ExtCtrls, VCL.Buttons, VCL.Menus,

  UHostDlg,
  HydrologyCom_TLB,
  UXMLAgent,
  UXMLAgentMineSections,
  UWRMFEdit, UWRMFComboBox, UWRMFControl, UWRMFCheckBox, VCL.ComCtrls,
  VCL.Grids, UCellChangeStringGrid, UWRMFGrid, Xml.Win.msxmldom;

type
  TMineSectionsDlg = class(TForm)
    FXMLDocumentIn           : TXMLDocument;
    FXMLDocumentOut          : TXMLDocument;
    PnlBottom                : TPanel;
    BtnCancel                : TButton;
    BtnApply                 : TButton;
    ScrClient                : TScrollBox;
    RgpAction                : TRadioGroup;
    RgpType                  : TRadioGroup;
    LblSection               : TLabel;
    CbxSection               : TComboBox;
    procedure FormShow(Sender: TObject);
    procedure BtnApplyClick(Sender: TObject);
    procedure BtnCancelClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure OnControlClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    procedure ResetControls;
    function StoreXMLData : Boolean;
  public
    { Public declarations }
    FMayChangeNetwork : Boolean;
    FXMLAgent         : TXMLAgentMineSections;
    FHydrologyModel   : IHydrologyModel;
  end;

implementation

uses
  UErrorHandlingOperations;

{$R *.dfm}

procedure TMineSectionsDlg.FormCreate(Sender: TObject);
const OPNAME = 'TMineSectionsDlg.FormCreate';
begin
  try
    FXMLAgent := TXMLAgentMineSections.Create;
    FXMLAgent.DlgXMLDocumentIn  := FXMLDocumentIn;
    FXMLAgent.DlgXMLDocumentOut := FXMLDocumentOut;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMineSectionsDlg.FormDestroy(Sender: TObject);
const OPNAME = 'TMineSectionsDlg.FormDestroy';
begin
  try
    FreeAndNil(FXMLAgent);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMineSectionsDlg.FormShow(Sender: TObject);
const OPNAME = 'TMineSectionsDlg.FormShow';
begin
  try
    RgpAction.ItemIndex := -1;
    RgpType.ItemIndex   := -1;
    ResetControls;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMineSectionsDlg.ResetControls;
const OPNAME = 'TMineSectionsDlg.ResetControls';
var
  LRootNode            : IXMLNode;
  LSectionsNode        : IXMLNode;
  LNode                : IXMLNode;
  LIndex               : Integer;
  LSectionNo           : Integer;
  LName                : String;
begin
	try
    FXMLDocumentIn.Active  := TRUE;
    FXMLDocumentOut.Active := TRUE;
    LRootNode := FXMLDocumentIn.DocumentElement;
    CbxSection.Clear;
    LblSection.Visible := RgpAction.ItemIndex = 1;
    CbxSection.Visible := RgpAction.ItemIndex = 1;
    if (RgpAction.ItemIndex = 1) then
    // Remove an existing item
    begin
      LSectionsNode := nil;
      case RgpType.ItemIndex of
        0 : LSectionsNode := LRootNode.ChildNodes['AllOpencastSections'];
        1 : LSectionsNode := LRootNode.ChildNodes['AllUndergroundSections'];
        2 : LSectionsNode := LRootNode.ChildNodes['AllSlurryDumps'];
      else
      end;
      if (RgpType.ItemIndex >= 0) then
      begin
        for LIndex := 0 to LSectionsNode.ChildNodes.Count - 1 do
        begin
          LNode      := LSectionsNode.ChildNodes.Get(LIndex);
          LSectionNo := StrToInt(LNode.ChildNodes['SectionNo'].Text);
          LName      := LNode.ChildNodes['Name'].Text;
          CbxSection.Items.AddObject(LName, TObject(LSectionNo));
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TMineSectionsDlg.StoreXMLData : Boolean;
const OPNAME = 'TMineSectionsDlg.StoreXMLData';
var
  LRootNode     : IXMLNode;
  LSectionNode  : IXMLNode;
  LTypeIndex    : Integer;
  LActionIndex  : Integer;
  LSectionIndex : Integer;
  LSectionNo    : Integer;
  LMessage      : String;
begin
  Result := FALSE;
  try
    LRootNode    := FXMLDocumentOut.DocumentElement;
    LSectionNode := LRootNode.ChildNodes['MineSections'];

    LActionIndex  := RgpAction.ItemIndex;
    LTypeIndex    := RgpType.ItemIndex;
    LSectionIndex := CbxSection.ItemIndex;
    if (LActionIndex < 0) then
    begin
      LMessage := 'Please select the action - or Cancel.';
      ShowMessage(LMessage);
    end
    else
    begin
      if (LTypeIndex < 0) then
      begin
        LMessage := 'Please select the type of section - or Cancel.';
        ShowMessage(LMessage);
      end
      else
      begin
        if ((LActionIndex = 1) AND (LSectionIndex < 0)) then
        begin
          LMessage := 'Please select the section to remove - or Cancel.';
          ShowMessage(LMessage);
        end
        else
        begin
          case LActionIndex of
            0 : LSectionNode.ChildNodes['Action'].Text := 'Add';
            1 : LSectionNode.ChildNodes['Action'].Text := 'Remove';
          else
          end;
          case LTypeIndex of
            0 : LSectionNode.ChildNodes['SectionType'].Text := 'Opencast';
            1 : LSectionNode.ChildNodes['SectionType'].Text := 'Underground';
            2 : LSectionNode.ChildNodes['SectionType'].Text := 'SlurryDump';
          else
          end;
          if (LSectionIndex >= 0) then
          begin
            LSectionNo := Integer(CbxSection.Items.Objects[LSectionIndex]);
            LSectionNode.ChildNodes['SectionNo'].Text := IntToStr(LSectionNo);
          end;
          Result := TRUE;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMineSectionsDlg.BtnApplyClick(Sender: TObject);
const OPNAME = 'TMineSectionsDlg.BtnApplyClick';
begin
  try
    if (StoreXMLData) then
    begin
      ModalResult := mrOk;
      FHydrologyModel.UpdateNetworkData(FXMLDocumentOut.XML.Text);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMineSectionsDlg.BtnCancelClick(Sender: TObject);
const OPNAME = 'TMineSectionsDlg.BtnCancelClick';
begin
  try
{    RgpAction.ItemIndex := -1;
    RgpType.ItemIndex   := -1;}
    ModalResult := mrCancel;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMineSectionsDlg.OnControlClick(Sender: TObject);
const OPNAME = 'TMineSectionsDlg.OnControlClick';
begin
  try
    ResetControls;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
