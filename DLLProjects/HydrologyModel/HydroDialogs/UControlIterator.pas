unit UControlIterator;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, VCL.Graphics, VCL.Controls, VCL.Forms,
  VCL.Dialogs, VCL.StdCtrls, xmldom, XMLIntf, XMLDoc, VCL.ExtCtrls, VCL.Buttons, VCL.Menus,
  VCL.ComCtrls,

  UXMLAgent,
  UWRMFEdit, UWRMFComboBox, UWRMFControl, UWRMFCheckBox, UWRMFGrid;

type

  TControlIterator = class(TObject)
  protected
    FWRMFGridList      : TStringList;
    FWRMFEditList      : TStringList;
    FWRMFComboBoxList  : TStringList;
    FWRMFCheckBoxList  : TStringList;
    FWRMFControlList   : TStringList;
    FControlParent      : TWinControl;
    procedure SetControlParent (AParent : TWinControl);
  public
    constructor Create;
    destructor Destroy; override;
    procedure SetIndicators;
    procedure SetControls (AMayChangeNetwork : Boolean);
    procedure SetAllValid;
    function LoadXMLData (ARootNode : IXMLNode): Boolean;
    function StoreXMLData (ARootNode : IXMLNode): Boolean;
    function DoValidation (AContext          : TValidationContext;
                           APropertyName     : String;
                           AFieldIndex       : String;
                           AInputXML         : String;
                           AOutputXML        : String;
                           AStopOnFirstError : Boolean;
                           AErrorList        : TStringList;
                           AXMLAgent         : TXMLAgent;
                           ARootNode         : IXMLNode) : Boolean;
    property ControlParent : TWinControl read FControlParent write SetControlParent;
  end;

implementation

uses
  UErrorHandlingOperations;

constructor TControlIterator.Create;
const OPNAME = 'TControlIterator.Create';
begin
  try
    inherited Create;
    FWRMFGridList      := TStringList.Create;
    FWRMFEditList      := TStringList.Create;
    FWRMFComboBoxList  := TStringList.Create;
    FWRMFCheckBoxList  := TStringList.Create;
    FWRMFControlList   := TStringList.Create;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

destructor TControlIterator.Destroy;
const OPNAME = 'TControlIterator.Destroy';
begin
  try
    FreeAndNil(FWRMFGridList);
    FreeAndNil(FWRMFEditList);
    FreeAndNil(FWRMFComboBoxList);
    FreeAndNil(FWRMFCheckBoxList);
    FreeAndNil(FWRMFControlList);
    inherited Destroy;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TControlIterator.SetControlParent (AParent : TWinControl);
const OPNAME = 'TControlIterator.SetControlParent';
var
  LIndex  : Integer;
  LIdxStr : String;
begin
  try
    FWRMFGridList.Clear;
    FWRMFEditList.Clear;
    FWRMFComboBoxList.Clear;
    FWRMFCheckBoxList.Clear;
    FWRMFControlList.Clear;
    FControlParent := AParent;
    for LIndex := 0 to FControlParent.ControlCount - 1 do
    begin
      LIdxStr := IntToStr(LIndex);
      if (FControlParent.Controls[LIndex] is TWRMFGrid) then
        FWRMFGridList.Add(LIdxStr);
      if (FControlParent.Controls[LIndex] is TWRMFEdit) then
      begin
        FWRMFEditList.Add(LIdxStr);
        FWRMFControlList.Add(LIdxStr);
      end;
      if (FControlParent.Controls[LIndex] is TWRMFCheckBox) then
      begin
        FWRMFCheckBoxList.Add(LIdxStr);
        FWRMFControlList.Add(LIdxStr);
      end;
      if (FControlParent.Controls[LIndex] is TWRMFComboBox) then
      begin
        FWRMFComboBoxList.Add(LIdxStr);
        FWRMFControlList.Add(LIdxStr);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TControlIterator.SetIndicators;
const OPNAME = 'TControlIterator.SetIndicators';
var
  LCount   : Integer;
  LControl : TWRMFControl;
  LGrid    : TWRMFGrid;
  LRow     : Integer;
  LCol     : Integer;
  LIndex   : Integer;
begin
  try
    for LCount := 0 to FWRMFGridList.Count - 1 do
    begin
      LIndex := StrToInt(FWRMFGridList.Strings[LCount]);
      LGrid := TWRMFGrid(FControlParent.Controls[LIndex]);
      for LRow := 1 to LGrid.RowCount - 1 do
      begin
        for LCol := 0 to LGrid.ColCount - 1 do
        begin
          LGrid.CellInfo[LRow, LCol].HasParamChange := FALSE;
          LGrid.CellInfo[LRow, LCol].HasMetaData    := FALSE;
        end;
      end;
    end;
    for LCount := 0 to FWRMFControlList.Count - 1 do
    begin
      LIndex := StrToInt(FWRMFControlList.Strings[LCount]);
      LControl := TWRMFControl(FControlParent.Controls[LIndex]);
      LControl.HasParamChange := FALSE;
      LControl.HasMetaData    := FALSE;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TControlIterator.SetControls (AMayChangeNetwork : Boolean);
const OPNAME = 'TControlIterator.SetControls';
var
  LCount   : Integer;
  LIndex   : Integer;
begin
  try
    for LCount := 0 to FWRMFGridList.Count - 1 do
    begin
      LIndex := StrToInt(FWRMFGridList.Strings[LCount]);
      TWRMFGrid(FControlParent.Controls[LIndex]).Active := AMayChangeNetwork;
    end;
    for LCount := 0 to FWRMFControlList.Count - 1 do
    begin
      LIndex := StrToInt(FWRMFControlList.Strings[LCount]);
      TWRMFControl(FControlParent.Controls[LIndex]).Active := AMayChangeNetwork;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TControlIterator.SetAllValid;
const OPNAME = 'TControlIterator.SetAllValid';
var
  LCount    : Integer;
  LIndex    : Integer;
  LRow      : Integer;
  LCol      : Integer;
  LCellInfo : TCellInfo;
  LGrid     : TWRMFGrid;
begin
  try
    for LCount := 0 to FWRMFGridList.Count - 1 do
    begin
      LIndex := StrToInt(FWRMFGridList.Strings[LCount]);
      LGrid := TWRMFGrid(FControlParent.Controls[LIndex]);
      for LRow := 1 to LGrid.RowCount - 1 do
      begin
        for LCol := 0 to LGrid.ColCount - 1 do
        begin
          LCellInfo := LGrid.CellInfo[LRow, LCol];
          LCellInfo.IsValid := TRUE;
        end;
      end;
    end;
    for LCount := 0 to FWRMFControlList.Count - 1 do
    begin
      LIndex := StrToInt(FWRMFControlList.Strings[LCount]);
      TWRMFControl(FControlParent.Controls[LIndex]).IsValid := TRUE;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TControlIterator.LoadXMLData (ARootNode : IXMLNode): boolean;
const OPNAME = 'TControlIterator.LoadXMLData';
var
  LSectionNode   : IXMLNode;
  LCount         : Integer;
  LEditControl   : TWRMFEdit;
  LCheckControl  : TWRMFCheckBox;
  LComboControl  : TWRMFComboBox;
  LGridControl   : TWRMFGrid;
  LPropertyName  : String;
  LSection       : String;
  LNumber        : Integer;
  LIndex         : Integer;
  LDataListName  : String;
  LDataListNode  : IXMLNode;
  LNode          : IXMLNode;
  LCellInfo      : TCellInfo;
  LCol           : Integer;
  LRow           : Integer;
begin
  Result := FALSE;
  try
    LSection     := ARootNode.ChildNodes['Section'].Text;
    LSectionNode := ARootNode.ChildNodes[LSection];

    for LCount := 0 to FControlParent.ControlCount - 1 do
    begin
      if (FControlParent.Controls[LCount] is TWRMFGrid) then
      begin
        LGridControl  := TWRMFGrid(FControlParent.Controls[LCount]);
        LDataListName := LGridControl.CellsInfo.DataListName;
        LDataListNode := LSectionNode.ChildNodes[LDataListName];
        LGridControl.RowCount := 1 + LDataListNode.ChildNodes.Count;
        for LCol := 0 to LGridControl.ColCount - 1 do
        begin
          LCellInfo := LGridControl.CellInfo[0, LCol];
          for LRow := 1 to LGridControl.RowCount - 1 do
            LGridControl.CellInfo[LRow, LCol].PropertyName := LCellInfo.PropertyName;
        end;
        LGridControl.Enabled := (LGridControl.RowCount > LGridControl.CellsInfo.NoOfHeadingRows);
        if (LGridControl.RowCount > LGridControl.CellsInfo.NoOfHeadingRows) then
          LGridControl.FixedRows := LGridControl.CellsInfo.NoOfHeadingRows;
        for LIndex := 1 to LDataListNode.ChildNodes.Count do
        begin
          LNode := LDataListNode.ChildNodes.Get(LIndex-1);
          for LCol := 0 to LGridControl.ColCount - 1 do
          begin
            LCellInfo := LGridControl.CellInfo[0, lCol];
            LPropertyName := LCellInfo.PropertyName;
            LGridControl.Cells[LCol, LIndex] := LNode.ChildNodes[LPropertyName].Text;
          end;
        end;
      end;
      if (FControlParent.Controls[LCount] is TWRMFEdit) then
      begin
        LEditControl  := TWRMFEdit(FControlParent.Controls[LCount]);
        LPropertyName := LEditControl.PropertyName;
        try
          LEditControl.Text := LSectionNode.ChildNodes[LPropertyName].Text;
        except
          LEditControl.Text := '';
        end;
      end;
      if (FControlParent.Controls[LCount] is TWRMFCheckBox) then
      begin
        LCheckControl := TWRMFCheckBox(FControlParent.Controls[LCount]);
        LPropertyName := LCheckControl.PropertyName;
        try
          LCheckControl.Checked := (LSectionNode.ChildNodes[LPropertyName].Text = 'Y') OR
                                   (LSectionNode.ChildNodes[LPropertyName].Text = '1');
        except
          LCheckControl.Checked := FALSE;
        end;
      end;
      if (FControlParent.Controls[LCount] is TWRMFComboBox) then
      begin
        LComboControl := TWRMFComboBox(FControlParent.Controls[LCount]);
        LPropertyName := LComboControl.PropertyName;
        try
          LNumber := StrToInt(LSectionNode.ChildNodes[LPropertyName].Text);
          LIndex  := LComboControl.Items.IndexOfObject(pointer(LNumber));
          LComboControl.ItemIndex := LIndex;
        except
          LComboControl.ItemIndex := -1;
        end;
      end;
    end;
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TControlIterator.StoreXMLData (ARootNode : IXMLNode): Boolean;
const OPNAME = 'TControlIterator.StoreXMLData';
var
  LSectionNode  : IXMLNode;
  LIndex        : Integer;
  LCount        : Integer;
  LEditControl  : TWRMFEdit;
  LComboControl : TWRMFComboBox;
  LGridControl  : TWRMFGrid;
  LPropertyName : String;
  LSection      : String;
  LDataListName : String;
  LDataListNode : IXMLNode;
  LNode         : IXMLNode;
  LCellInfo     : TCellInfo;
  LCol          : Integer;
begin
  Result := FALSE;
  try
    LSection     := ARootNode.ChildNodes['Section'].Text;
    LSectionNode := ARootNode.ChildNodes[LSection];
    try
      for LCount := 0 to FControlParent.ControlCount - 1 do
      begin
        if (FControlParent.Controls[LCount] is TWRMFGrid) then
        begin
          LGridControl  := TWRMFGrid(FControlParent.Controls[LCount]);
          LDataListName := LGridControl.CellsInfo.DataListName;
          LDataListNode := LSectionNode.ChildNodes[LDataListName];
          LDataListNode.ChildNodes.Clear;
          for LIndex := 1 to LGridControl.RowCount - 1 do
          begin
            LNode := LDataListNode.AddChild('Data');
            for LCol := 0 to LGridControl.ColCount - 1 do
            begin
              LCellInfo := LGridControl.CellInfo[0, lCol];
              LPropertyName := LCellInfo.PropertyName;
              LNode.AddChild(LPropertyName);
              LNode.ChildNodes[LPropertyName].Text := LGridControl.Cells[LCol, LIndex];
            end;
          end;
        end;
        if (FControlParent.Controls[LCount] is TWRMFComboBox) then
        begin
          LComboControl := TWRMFComboBox(FControlParent.Controls[LCount]);
          LPropertyName := LComboControl.PropertyName;
          LIndex := LComboControl.ItemIndex;
          if (LIndex >= 0) then
            LSectionNode.ChildNodes[LPropertyName].Text := IntToStr(integer(LComboControl.Items.Objects[LIndex]))
          else
            LSectionNode.ChildNodes[LPropertyName].Text := '';
        end;
        if (FControlParent.Controls[LCount] is TWRMFEdit) then
        begin
          LEditControl  := TWRMFEdit(FControlParent.Controls[LCount]);
          LPropertyName := LEditControl.PropertyName;
          LSectionNode.ChildNodes[LPropertyName].Text := Trim(LEditControl.Text);
        end;
      end;
      Result := TRUE;
    except
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TControlIterator.DoValidation (AContext          : TValidationContext;
                                        APropertyName     : String;
                                        AFieldIndex       : String;
                                        AInputXML         : String;
                                        AOutputXML        : String;
                                        AStopOnFirstError : Boolean;
                                        AErrorList        : TStringList;
                                        AXMLAgent         : TXMLAgent;
                                        ARootNode         : IXMLNode) : Boolean;
const OPNAME = 'TControlIterator.DoValidation';
var
  LErrorMessages : TStringList;
  LFirstError    : String;
  LIndex         : Integer;
  LRow           : Integer;
  LCol           : Integer;
  LControl       : TWRMFControl;
  LGrid          : TWRMFGrid;
  LCellInfo      : TCellInfo;
  LFoundControl  : Boolean;
begin
  Result := FALSE;
  try
    LErrorMessages := TStringList.Create;
    try
      Result := AXMLAgent.Validate(AContext,
                                   APropertyName,
                                   AFieldIndex,
                                   AInputXML,
                                   AOutputXML,
                                   AStopOnFirstError,
                                   AErrorList,
                                   LErrorMessages);
      for LIndex := 0 to FControlParent.ControlCount - 1 do
      begin
        if (FControlParent.Controls[LIndex] is TWRMFControl) then
        begin
          LControl := TWRMFControl(FControlParent.Controls[LIndex]);
          LControl.IsValid := LControl.IsValid AND (AErrorList.IndexOf(LControl.PropertyName) < 0);
        end;
        if (FControlParent.Controls[LIndex] is TWRMFGrid) then
        begin
          LGrid := TWRMFGrid(FControlParent.Controls[LIndex]);
          for LRow := 1 to LGrid.RowCount - 1 do
          begin
            for LCol := 0 to LGrid.ColCount - 1 do
            begin
              LCellInfo := LGrid.CellInfo[LRow, LCol];
              LCellInfo.IsValid := AErrorList.IndexOf(LCellInfo.PropertyName + ',' + IntToStr(LRow)) < 0;
            end;
          end;
        end;
      end;

      if (NOT Result) then
      begin
        ShowMessage(LErrorMessages.Text);
        LFirstError := '';
        if (AErrorList.Count > 0) then
          LFirstError := AErrorList.Strings[0];
        if (AContext = tcApply) then
        begin
          LFoundControl := FALSE;
          LIndex        := 0;
          while ((NOT LFoundControl) AND (LIndex < FControlParent.ControlCount)) do
          begin
            if (FControlParent.Controls[LIndex] is TWRMFControl) then
            begin
              LControl := TWRMFControl(FControlParent.Controls[LIndex]);
              if (LControl.PropertyName = LFirstError) then
              begin
                LFoundControl := TRUE;
                LControl.SetFocus;
              end;
            end;
            LIndex := LIndex + 1;
          end;
        end;
      end;
    finally
      LErrorMessages.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


end.

