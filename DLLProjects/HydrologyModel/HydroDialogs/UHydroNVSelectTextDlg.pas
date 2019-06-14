unit UHydroNVSelectTextDlg;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, VCL.Graphics, VCL.Controls, VCL.Forms,
  VCL.Dialogs, VCL.StdCtrls, xmldom, XMLIntf, XMLDoc, VCL.Menus,
  Xml.Win.msxmldom;

type
  THydroNVSelectTextDlg = class(TForm)
    FScrollBox          : TScrollBox;
    FAssignParentChx    : TCheckBox;
    FElementTypeCbx     : TComboBox;
    FCancelBtn          : TButton;
    FOKBtn              : TButton;
    FXMLDocumentOut     : TXMLDocument;
    FXMLDocumentIn      : TXMLDocument;
    FNamesCbx           : TComboBox;
    FTypeLbl            : TLabel;
    FTextTypeCbx        : TComboBox;
    FSubTypesCbx        : TComboBox;
    FSectionsCbx        : TComboBox;
    procedure OnOKBtnClick (Sender: TObject);
    procedure OnYesBtnClick (Sender: TObject);
    procedure OnCancelBtnClick (Sender: TObject);
    procedure OnControlClick (Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FElementTypeCbxSelect(Sender: TObject);
    procedure FSubTypesCbxSelect(Sender: TObject);
    procedure FNamesCbxSelect(Sender: TObject);
  private
    FElementType    : String;
    FElementNo      : Integer;
    FElementSubType : Integer;
    FHasSubTypes    : Boolean;

    procedure ResetControls;
    procedure PopulateElementTypesCbx;
    procedure SelectElementType;
    procedure PopulateSubTypesCbx;
    procedure SelectSubType;
    procedure PopulateNamesCbx;
    procedure SelectElement;
    procedure PopulateSectionsCbx;
    procedure PopulateTextTypesCbx;
  public
  end;

    function ShowNVSelectTextDlg (AInputXML         : String;
                                  var AOutputXML    : String): Boolean;

implementation

uses

  UErrorHandlingOperations;

{$R *.dfm}

function ShowNVSelectTextDlg (AInputXML         : String;
                              var AOutputXML    : String): Boolean;
const OPNAME = 'THydroNVSelectTextDlg.ShowNVSelectTextDlg';
var
  FNVSelectTextDlg : THydroNVSelectTextDlg;
begin
  Result := FALSE;
  try
    FNVSelectTextDlg := THydroNVSelectTextDlg.Create(nil);
    try
      FNVSelectTextDlg.FXMLDocumentIn.XML.Text  := AInputXML;
      FNVSelectTextDlg.FXMLDocumentOut.XML.Text := AOutputXML;

      Result := (FNVSelectTextDlg.ShowModal = mrOK);
      if (Result) then
      begin
        AOutputXML := FNVSelectTextDlg.FXMLDocumentOut.XML.Text;
      end;
    finally
      FNVSelectTextDlg.FXMLDocumentIn.Active := FALSE;
      FNVSelectTextDlg.FXMLDocumentOut.Active := FALSE;
      FNVSelectTextDlg.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure THydroNVSelectTextDlg.FormShow(Sender: TObject);
const OPNAME = 'THydroNVSelectTextDlg.FormShow';
begin
  try
    FElementType    := '';
    FElementNo      := 0;
    FElementSubType := 0;
    FHasSubTypes    := FALSE;
    
    FXMLDocumentIn.Active := TRUE;
    FXMLDocumentOut.Active := TRUE;
    PopulateElementTypesCbx;
    FElementTypeCbx.Enabled := FALSE;
    FNamesCbx.Enabled       := FALSE;
    FTextTypeCbx.Enabled    := FALSE;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure THydroNVSelectTextDlg.PopulateElementTypesCbx;
const OPNAME = 'THydroNVSelectTextDlg.PopulateElementTypesCbx';
var
  LRootNode            : IXMLNode;
  LAllElementTypesNode : IXMLNode;
  LElementTypeNode     : IXMLNode;
  LIndex               : Integer;
begin
  try
    LRootNode            := FXMLDocumentIn.DocumentElement;
    LAllElementTypesNode := LRootNode.ChildNodes['AllElementTypes'];
    FElementTypeCbx.Clear;
    for LIndex := 0 to LAllElementTypesNode.ChildNodes.Count - 1 do
    begin
      LElementTypeNode := LAllElementTypesNode.ChildNodes.Get(LIndex);
      FElementTypeCbx.Items.Add(LElementTypeNode.ChildNodes['ElementTypeName'].Text);
    end;
    if (FElementTypeCbx.Items.Count > 0) then
      FElementTypeCbx.ItemIndex := 0;
    SelectElementType;  
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure THydroNVSelectTextDlg.FElementTypeCbxSelect(Sender: TObject);
const OPNAME = 'THydroNVSelectTextDlg.FElementTypeCbxSelect';
begin
  try
    SelectElementType;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure THydroNVSelectTextDlg.SelectElementType;
const OPNAME = 'THydroNVSelectTextDlg.SelectElementType';
begin
  try
    FElementType    := '';
    FElementSubType := 0;
    FElementNo      := 0;
    FHasSubTypes    := FALSE;
    if (FElementTypeCbx.ItemIndex >= 0) then
    begin
      FElementType := FElementTypeCbx.Text;
      PopulateSubTypesCbx;
      PopulateNamesCbx;
      PopulateTextTypesCbx;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure THydroNVSelectTextDlg.PopulateSubTypesCbx;
const OPNAME = 'THydroNVSelectTextDlg.PopulateSubTypesCbx';
var
  LRootNode            : IXMLNode;
  LAllElementTypesNode : IXMLNode;
  LElementTypeNode     : IXMLNode;
  LAllSubTypesNode     : IXMLNode;
  LSubTypeNode         : IXMLNode;
  LIndex               : Integer;
  LID                  : Integer;
  LName                : String;
  LCount               : integer;
begin
  try
    FSubTypesCbx.Items.Clear;
    if (FAssignParentChx.Checked) then
    begin
      LRootNode            := FXMLDocumentIn.DocumentElement;
      LAllElementTypesNode := LRootNode.ChildNodes['AllElementTypes'];
      // Loop for each element type eg. Reservoir, Mine, Channel reach, etc.
      for LIndex := 0 to LAllElementTypesNode.ChildNodes.Count - 1 do
      begin
        LElementTypeNode := LAllElementTypesNode.ChildNodes.Get(LIndex);
        if (FElementType = LElementTypeNode.ChildNodes['ElementTypeName'].Text) then
        begin
          // If this element type is the selected element type then
          // Loop for all the sub-types belonging to the type
          LAllSubTypesNode := LElementTypeNode.ChildNodes['AllSubTypes'];
          FHasSubTypes := LAllSubTypesNode.ChildNodes.Count > 0;
          FSubTypesCbx.Visible := FHasSubTypes;
          FSectionsCbx.Visible := FHasSubTypes;
          if (FHasSubTypes) then
          begin
            FSubTypesCbx.Visible := TRUE;
            FSectionsCbx.Visible := TRUE;
            // If the element type has sub-types
            // Loop for all the sub-types
            for LCount := 0 to LAllSubTypesNode.ChildNodes.Count - 1 do
            begin
              LSubTypeNode := LAllSubTypesNode.ChildNodes.Get(LCount);
              LID          := StrToInt(LSubTypeNode.ChildNodes['ElementSubType'].Text);
              LName        := LSubTypeNode.ChildNodes['Name'].Text;
              FSubTypesCbx.Items.AddObject(LName, TObject(LID));
            end;
            if (FSubTypesCbx.Items.Count > 0) then
              FSubTypesCbx.ItemIndex := 0;
            SelectSubType;
          end;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure THydroNVSelectTextDlg.FSubTypesCbxSelect(Sender: TObject);
const OPNAME = 'THydroNVSelectTextDlg.FSubTypesCbxSelect';
begin
  try
    SelectSubType;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure THydroNVSelectTextDlg.SelectSubType;
const OPNAME = 'THydroNVSelectTextDlg.SelectSubType';
var
  LIndex : Integer;
begin
  try
    FElementSubType := 0;
    LIndex          := FSubTypesCbx.ItemIndex;
    if (LIndex >= 0) then
    begin
      FElementSubType := Integer(FSubTypesCbx.Items.Objects[LIndex]);
      PopulateSectionsCbx;
      PopulateTextTypesCbx;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure THydroNVSelectTextDlg.PopulateNamesCbx;
const OPNAME = 'THydroNVSelectTextDlg.PopulateNamesCbx';
var
  LRootNode            : IXMLNode;
  LAllElementTypesNode : IXMLNode;
  LElementTypeNode     : IXMLNode;
  LAllElementsNode     : IXMLNode;
  LElementNode         : IXMLNode;
  LIndex               : Integer;
  LID                  : Integer;
  LName                : String;
  LCount               : integer;
  LTempStr             : string;
begin
  try
    FNamesCbx.Items.Clear;
    if (FAssignParentChx.Checked) then
    begin
      LRootNode            := FXMLDocumentIn.DocumentElement;
      LAllElementTypesNode := LRootNode.ChildNodes['AllElementTypes'];
      // Loop for each element type eg. Reservoir, Mine, Channel reach, etc.
      for LIndex := 0 to LAllElementTypesNode.ChildNodes.Count - 1 do
      begin
        LElementTypeNode := LAllElementTypesNode.ChildNodes.Get(LIndex);
//        if (FElementTypeCbx.Text = LElementTypeNode.ChildNodes['ElementTypeName'].Text) then
        if (FElementType = LElementTypeNode.ChildNodes['ElementTypeName'].Text) then
        begin
          // If this element type is the selected element type then
          // Loop for all the elements belonging to the type
          LAllElementsNode := LElementTypeNode.ChildNodes['AllElements'];
          for LCount := 0 to LAllElementsNode.ChildNodes.Count - 1 do
          begin
            LElementNode := LAllElementsNode.ChildNodes.Get(LCount);
            LID          := StrToInt(LElementNode.ChildNodes['ID'].Text);
            LName        := LElementNode.ChildNodes['Name'].Text;
            LTempStr     := '(' + IntToStr(LID) + ') ' + LName;
            FNamesCbx.Items.AddObject(LTempStr, TObject(LID));
          end;
          if (FNamesCbx.Items.Count > 0) then
            FNamesCbx.ItemIndex := 0;
          SelectElement;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure THydroNVSelectTextDlg.FNamesCbxSelect(Sender: TObject);
const OPNAME = 'THydroNVSelectTextDlg.FNamesCbxSelect';
begin
  try
    SelectElement;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure THydroNVSelectTextDlg.SelectElement;
const OPNAME = 'THydroNVSelectTextDlg.SelectElement';
var
  LIndex : Integer;
begin
  try
    FElementNo := 0;
    LIndex     := FNamesCbx.ItemIndex;
    if (LIndex >= 0) then
    begin
      FElementNo := Integer(FNamesCbx.Items.Objects[LIndex]);
      PopulateSectionsCbx;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure THydroNVSelectTextDlg.PopulateSectionsCbx;
const OPNAME = 'THydroNVSelectTextDlg.PopulateSectionsCbx';
var
  LRootNode            : IXMLNode;
  LAllElementTypesNode : IXMLNode;
  LElementTypeNode     : IXMLNode;
  LAllElementsNode     : IXMLNode;
  LElementNode         : IXMLNode;
  LAllSubElementsNode  : IXMLNode;
  LSubElementNode      : IXMLNode;
  LIndex               : Integer;
  LID                  : Integer;
  LName                : String;
  LCount               : integer;
  LSubIndex            : Integer;
  LElementSubType      : Integer;
begin
  try
    FSectionsCbx.Items.Clear;
    if ((FAssignParentChx.Checked) AND (FElementSubType <> 0) AND (FElementNo <> 0)) then
    begin
      LRootNode            := FXMLDocumentIn.DocumentElement;
      LAllElementTypesNode := LRootNode.ChildNodes['AllElementTypes'];
      // Loop for each element type eg. Reservoir, Mine, Channel reach, etc.
      for LIndex := 0 to LAllElementTypesNode.ChildNodes.Count - 1 do
      begin
        LElementTypeNode := LAllElementTypesNode.ChildNodes.Get(LIndex);
        if (FElementType = LElementTypeNode.ChildNodes['ElementTypeName'].Text) then
        begin
          // If this element type is the selected element type then
          // Loop for all the elements belonging to the type
          LAllElementsNode := LElementTypeNode.ChildNodes['AllElements'];
          for LCount := 0 to LAllElementsNode.ChildNodes.Count - 1 do
          begin
            LElementNode := LAllElementsNode.ChildNodes.Get(LCount);
            LID          := StrToInt(LElementNode.ChildNodes['ID'].Text);
            if (FElementNo = LID) then
            begin
              LAllSubElementsNode := LElementNode.ChildNodes['AllSubElements'];
              for LSubIndex := 0 to LAllSubElementsNode.ChildNodes.Count - 1 do
              begin
                LSubElementNode := LAllSubElementsNode.ChildNodes.Get(LSubIndex);
                LElementSubType := StrToInt(LSubElementNode.ChildNodes['ElementSubType'].Text);
                if (LElementSubType = FElementSubType) then
                begin
                  LID      := StrToInt(LSubElementNode.ChildNodes['ID'].Text);
                  LName    := LSubElementNode.ChildNodes['Name'].Text;
                  FSectionsCbx.Items.AddObject(LName, TObject(LID));
                end;
              end;
            end;
          end;
          if (FSectionsCbx.Items.Count > 0) then
            FSectionsCbx.ItemIndex := 0;
//          SelectElement;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure THydroNVSelectTextDlg.PopulateTextTypesCbx;
const OPNAME = 'THydroNVSelectTextDlg.PopulateTextTypesCbx';
var
  LRootNode            : IXMLNode;
  LAllElementTypesNode : IXMLNode;
  LElementTypeNode     : IXMLNode;
  LAllTextTypesNode    : IXMLNode;
  LTextTypeNode        : IXMLNode;
  LIndex               : Integer;
  LID                  : Integer;
  LName                : String;
  LElementSubType      : Integer;
  LCount               : integer;
begin
	try
    FTextTypeCbx.Items.Clear;
    if (FAssignParentChx.Checked) then
    begin
      LRootNode            := FXMLDocumentIn.DocumentElement;
      LAllElementTypesNode := LRootNode.ChildNodes['AllElementTypes'];
      // Loop for each element type eg. Reservoir, Mine, Channel reach, etc.
      for LIndex := 0 to LAllElementTypesNode.ChildNodes.Count - 1 do
      begin
        LElementTypeNode := LAllElementTypesNode.ChildNodes.Get(LIndex);
        if (FElementType = LElementTypeNode.ChildNodes['ElementTypeName'].Text) then
        begin
          LAllTextTypesNode := LElementTypeNode.ChildNodes['AllTextTypes'];
          for LCount := 0 to LAllTextTypesNode.ChildNodes.Count - 1 do
          begin
            LTextTypeNode := LAllTextTypesNode.ChildNodes.Get(LCount);
            LElementSubType := 0;
            if (FHasSubTypes) then
              LElementSubType := StrToInt(LTextTypeNode.ChildNodes['ElementSubType'].Text);
            if ((NOT FHasSubTypes) OR (FHasSubTypes AND (FElementSubType = LElementSubType))) then
            begin
              LID   := StrToInt(LTextTypeNode.ChildNodes['ID'].Text);
              LName := LTextTypeNode.ChildNodes['Name'].Text;
              FTextTypeCbx.Items.AddObject(LName, TObject(LID));
            end;
          end;
          if (FTextTypeCbx.Items.Count > 0) then
            FTextTypeCbx.ItemIndex := 0;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure THydroNVSelectTextDlg.ResetControls;
const OPNAME = 'THydroNVSelectTextDlg.ResetControls';
begin
	try
    FElementTypeCbx.Enabled := FAssignParentChx.Checked;
    FNamesCbx.Enabled       := FAssignParentChx.Checked;
    FTextTypeCbx.Enabled    := FAssignParentChx.Checked;
    if (FAssignParentChx.Checked) then
      SelectElementType;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure THydroNVSelectTextDlg.OnOKBtnClick(Sender : TObject);
const OPNAME = 'THydroNVSelectTextDlg.OnOKBtnClick';
var
  LIndex          : integer;
  LMsg            : string;
  LRootNode       : IXMLNode;
  LElementType    : String;
  LElementID      : Integer;
  LTextType       : Integer;
  LElementSubType : Integer;
  LSectionNo      : Integer;
begin
	try
    if (FAssignParentChx.Checked) then
    begin
      LElementType    := '';
      LElementID      := -1;
      LTextType       := -1;
      LSectionNo      := 0;
      LElementSubType := 0;
      if (FNamesCbx.ItemIndex < 0) then
      begin
        LMsg := 'Please select the network element to which the text label must be assigned.';
        ShowMessage(LMsg);
      end
      else if (FHasSubTypes AND (FSubTypesCbx.ItemIndex < 0)) then
      begin
        LMsg := 'Please element sub-type to which the text label must be assigned.';
        ShowMessage(LMsg);
      end
      else if (FHasSubTypes AND (FSectionsCbx.ItemIndex < 0)) then
      begin
        LMsg := 'Please sub-type element to which the text label must be assigned.';
        ShowMessage(LMsg);
      end
      else if (FTextTypeCbx.ItemIndex < 0) then
      begin
        LMsg := 'Select text type';
        ShowMessage(LMsg);
      end
      else
      begin
        LIndex       := FNamesCbx.ItemIndex;
        LElementID   := Integer(FNamesCbx.Items.Objects[lIndex]);
        LElementType := FElementTypeCbx.Text;
        LIndex       := FTextTypeCbx.ItemIndex;
        LTextType    := Integer(FTextTypeCbx.Items.Objects[lIndex]);
        if (FHasSubTypes) then
        begin
          LIndex          := FSubTypesCbx.ItemIndex;
          LElementSubType := Integer(FSubTypesCbx.Items.Objects[lIndex]);
          LIndex          := FSectionsCbx.ItemIndex;
          LSectionNo      := Integer(FSectionsCbx.Items.Objects[lIndex]);
        end;
      end;
      if ((LElementID >= 0) AND (LElementType <> '') AND (LTextType >= 0)) then
      begin
        LRootNode  := FXMLDocumentOut.DocumentElement;
        LRootNode.ChildNodes['ElementType'].Text    := LElementType;
        LRootNode.ChildNodes['ElementNo'].Text      := IntToStr(LElementID);
        LRootNode.ChildNodes['TextType'].Text       := IntToStr(LTextType);
        LRootNode.ChildNodes['ElementSubType'].Text := IntToStr(LElementSubType);
        LRootNode.ChildNodes['SectionNo'].Text      := IntToStr(LSectionNo);
        ModalResult := mrOk;
      end  
    end
    else
      ModalResult := mrOk;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure THydroNVSelectTextDlg.OnYesBtnClick(Sender : TObject);
const OPNAME = 'THydroNVSelectTextDlg.OnYesBtnClick';
begin
  try
    ModalResult := mrOk;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure THydroNVSelectTextDlg.OnCancelBtnClick(Sender : TObject);
const OPNAME = 'THydroNVSelectTextDlg.OnCancelBtnClick';
begin
	try
	  ModalResult := mrCancel;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure THydroNVSelectTextDlg.OnControlClick(Sender : TObject);
const OPNAME = 'THydroNVSelectTextDlg.OnControlClick';
begin
	try
		ResetControls;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

end.
