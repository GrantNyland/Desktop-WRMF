unit UHydroNVSelectElementDlg;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, VCL.Graphics, VCL.Controls, VCL.Forms,
  VCL.Dialogs, VCL.StdCtrls, xmldom, XMLIntf, XMLDoc, VCL.Menus,
  Xml.Win.msxmldom;

type
  THydroNVSelectElementDlg = class(TForm)
    FScrollBox          : TScrollBox;
    FExistGrp           : TGroupBox;
    FExistRdb           : TRadioButton;
    FNewRdb             : TRadioButton;
    FShowDuplicatesChx  : TCheckBox;
    FNamesCbx           : TComboBox;
    FDuplicateLbl       : TLabel;
    FCancelBtn          : TButton;
    FOKBtn              : TButton;
    FYesBtn             : TButton;
    FNoBtn              : TButton;
    FXMLDocumentOut     : TXMLDocument;
    FXMLDocumentIn      : TXMLDocument;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure OnOKBtnClick (Sender: TObject);
    procedure OnYesBtnClick (Sender: TObject);
    procedure OnCancelBtnClick (Sender: TObject);
    procedure OnControlClick (Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    FExistingStr       : TStringList;
    FMayChangeNetwork  : Boolean;
    FElementType       : String;
    procedure ResetControls;
  public
  end;

    function ShowNVSelectElementDlg (AMayChangeNetwork : Boolean;
                                     AElementType      : String;
                                     AInputXML         : String;
                                     var AOutputXML    : String): Boolean;

implementation

uses

  UErrorHandlingOperations;

{$R *.dfm}

function ShowNVSelectElementDlg (AMayChangeNetwork : Boolean;
                                 AElementType      : String;
                                 AInputXML         : String;
                                 var AOutputXML    : String): Boolean;
const OPNAME = 'THydroNVSelectElementDlg.ShowNVSelectElementDlg';
var
  FNVSelectElementDlg : THydroNVSelectElementDlg;
begin
  Result := FALSE;
  try
    FNVSelectElementDlg := THydroNVSelectElementDlg.Create(nil);
    try
      FNVSelectElementDlg.FMayChangeNetwork        := AMayChangeNetwork;
      FNVSelectElementDlg.FElementType             := AElementType;
      FNVSelectElementDlg.FXMLDocumentIn.XML.Text  := AInputXML;
      FNVSelectElementDlg.FXMLDocumentOut.XML.Text := AOutputXML;
//      FNVSelectElementDlg.FNewRdb.Enabled          := FALSE; // RianaChangeStructure
      FNVSelectElementDlg.FNewRdb.Enabled          := AMayChangeNetwork;

      Result := (FNVSelectElementDlg.ShowModal = mrOK);
      if (Result) then
      begin
        AOutputXML := FNVSelectElementDlg.FXMLDocumentOut.XML.Text;
      end;
    finally
      FNVSelectElementDlg.FXMLDocumentIn.Active := FALSE;
      FNVSelectElementDlg.FXMLDocumentOut.Active := FALSE;
      FNVSelectElementDlg.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure THydroNVSelectElementDlg.FormCreate(Sender: TObject);
const OPNAME = 'THydroNVSelectElementDlg.FormCreate';
begin
  try
    FExistingStr    := TStringList.Create;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure THydroNVSelectElementDlg.FormDestroy(Sender: TObject);
const OPNAME = 'THydroNVSelectElementDlg.FormDestroy';
begin
  try
    FreeAndNil(FExistingStr);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure THydroNVSelectElementDlg.FormShow(Sender: TObject);
const OPNAME = 'THydroNVSelectElementDlg.FormShow';
var
  LRootNode   : IXMLNode;
  LExistNode  : IXMLNode;
  LIndex      : Integer;
begin
  try
    Caption := 'Please select a ' + FElementType;
    FShowDuplicatesChx.Caption := 'Include ' + FElementType + ' elements already on diagram';
    FXMLDocumentIn.Active := TRUE;
    FXMLDocumentOut.Active := TRUE;
    FExistingStr.Clear;
    LRootNode  := FXMLDocumentIn.DocumentElement;
    LExistNode := LRootNode.ChildNodes['Exist'];
    for LIndex := 0 to LExistNode.ChildNodes.Count - 1 do
      FExistingStr.Add(LExistNode.ChildNodes[LIndex].Text);
    FExistRdb.Checked := TRUE;

  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure THydroNVSelectElementDlg.ResetControls;
const OPNAME = 'THydroNVSelectElementDlg.ResetControls';
var
  LCount       : integer;
  LTempStr     : string;
  LName        : string;
  LID          : string;
  LIdx         : integer;
  LNode        : IXMLNode;
  LRootNode    : IXMLNode;
  LAllNode     : IXMLNode;
  LExistNode   : IXMLNode;
  LNewNode     : IXMLNode;
begin
	try
    FShowDuplicatesChx.Enabled := FExistRdb.Checked;

    FNamesCbx.Enabled := FExistRdb.Checked OR (FElementType = 'Observation Point');
    if (FExistRdb.Checked) then
    begin
      FNamesCbx.Items.Clear;
      LRootNode  := FXMLDocumentIn.DocumentElement;
      LAllNode   := LRootNode.ChildNodes['All'];
      LExistNode := LRootNode.ChildNodes['Exist'];
      for LCount := 0 to LAllNode.ChildNodes.Count - 1 do
      begin
        LNode       := LAllNode.ChildNodes.Get(LCount);
        LID         := LNode.ChildNodes['ID'].Text;
        LName       := LNode.ChildNodes['Name'].Text;
        LIdx        := FExistingStr.IndexOf(LID);
        if (FShowDuplicatesChx.Checked OR (LIdx < 0)) then
        begin
          if (FElementType = 'Observation Point') then
            lTempStr := '(Route ' + LID + ') ' + LName
          else
            lTempStr := '(' + LID + ') ' + LName;
          FNamesCbx.Items.AddObject(lTempStr, TObject(StrToInt(LID)));
        end;
      end;
      FNamesCbx.Sorted := TRUE;
      if (FNamesCbx.Items.Count > 0) then
        FNamesCbx.ItemIndex := 0;
    end
    else if (FElementType = 'Observation Point') then
    begin
      FNamesCbx.Items.Clear;
      LRootNode  := FXMLDocumentIn.DocumentElement;
      LNewNode   := LRootNode.ChildNodes['New'];
      for LCount := 0 to LNewNode.ChildNodes.Count - 1 do
      begin
        LNode    := LNewNode.ChildNodes.Get(LCount);
        LID      := LNode.Text;
        lTempStr := '(Route ' + LID + ') ';
        FNamesCbx.Items.AddObject(lTempStr, TObject(StrToInt(LID)));
      end;
      FNamesCbx.Sorted := TRUE;
      if (FNamesCbx.Items.Count > 0) then
        FNamesCbx.ItemIndex := 0;

    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure THydroNVSelectElementDlg.OnOKBtnClick(Sender : TObject);
const OPNAME = 'THydroNVSelectElementDlg.OnOKBtnClick';
var
  lIndex      : integer;
  lExistIdx   : integer;
  lMsg        : string;
  LSelectedID : Integer;
  LRootNode   : IXMLNode;
begin
	try
    if (FNewRdb.Checked) then
    begin
      LRootNode  := FXMLDocumentOut.DocumentElement;
      if (FElementType = 'Observation Point') then
      begin
        lIndex := FNamesCbx.ItemIndex;
        LSelectedID := Integer(FNamesCbx.Items.Objects[lIndex]);
        LRootNode.ChildNodes['Selected'].Text := IntToStr(LSelectedID);
      end
      else  
        LRootNode.ChildNodes['Selected'].Text := IntToStr(0);
      ModalResult := mrOk;
    end
    else
    begin
      lIndex := FNamesCbx.ItemIndex;
      if (lIndex < 0) then
      begin
        lMsg := 'Please select a ' + FElementType + ' - or Cancel.';
        ShowMessage(lMsg);
      end
      else
      begin
        LSelectedID := Integer(FNamesCbx.Items.Objects[lIndex]);
        lExistIdx   :=  FExistingStr.IndexOf(IntToStr(LSelectedID));
        if (lExistIdx >= 0) then
        begin
          lMsg := FElementType + ' %d is already on the diagram. Are you sure you want to add a duplicate?';
          lMsg := Format(lMsg, [LSelectedID]);
          FDuplicateLbl.Caption := lMsg;
          FDuplicateLbl.Visible := TRUE;
          FYesBtn.Visible       := TRUE;
          FNoBtn.Visible        := TRUE;
          FOKBtn.Visible        := FALSE;
          FCancelBtn.Visible    := FALSE;
        end
        else
        begin
          LRootNode  := FXMLDocumentOut.DocumentElement;
          LRootNode.ChildNodes['Selected'].Text := IntToStr(LSelectedID);
          ModalResult := mrOk;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure THydroNVSelectElementDlg.OnYesBtnClick(Sender : TObject);
const OPNAME = 'THydroNVSelectElementDlg.OnYesBtnClick';
var
  lIndex      : integer;
  lMsg        : string;
  LSelectedID : Integer;
  LRootNode   : IXMLNode;
begin
	try
    if (FExistRdb.Checked) then
    begin
      lIndex := FNamesCbx.ItemIndex;
      if (lIndex < 0) then
      begin
        lMsg := 'Please select a ' + FElementType + ' - or Cancel.';
        ShowMessage(lMsg);
      end
      else
      begin
        LSelectedID := Integer(FNamesCbx.Items.Objects[lIndex]);
        LRootNode  := FXMLDocumentOut.DocumentElement;
        LRootNode.ChildNodes['Selected'].Text := IntToStr(LSelectedID);
        ModalResult := mrOk;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure THydroNVSelectElementDlg.OnCancelBtnClick(Sender : TObject);
const OPNAME = 'THydroNVSelectElementDlg.OnCancelBtnClick';
begin
	try
	  ModalResult := mrCancel;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure THydroNVSelectElementDlg.OnControlClick(Sender : TObject);
const OPNAME = 'THydroNVSelectElementDlg.OnControlClick';
begin
	try
		ResetControls;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

end.
