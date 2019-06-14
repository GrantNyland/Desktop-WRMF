unit UHydroDlgUtilities;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, xmldom, XMLIntf, XMLDoc, ExtCtrls, Buttons, Menus,
  ComCtrls,

  UXMLAgent,
  UWRMFEdit, UWRMFComboBox, UWRMFControl, UWRMFCheckBox, UWRMFGrid;

    procedure UtilitySetIndicators (AScrollBox : TScrollBox);
    procedure UtilitySetControls (AScrollBox        : TScrollBox;
                                  AMayChangeNetwork : Boolean);
    procedure UtilitySetAllValid (AScrollBox : TScrollBox);
    function UtilityLoadXMLData (ARootNode  : IXMLNode;
                                 AScrollBox : TScrollBox): Boolean;
    function UtilityStoreXMLData (ARootNode  : IXMLNode;
                                  AScrollBox : TScrollBox): Boolean;
    function UtilityDoValidation (AContext          : TValidationContext;
                                  APropertyName     : String;
                                  AFieldIndex       : String;
                                  AXSD              : String;
                                  AInputXML         : String;
                                  AOutputXML        : String;
                                  AStopOnFirstError : Boolean;
                                  AErrorList        : TStringList;
                                  AXMLAgent         : TXMLAgent;
                                  ARootNode         : IXMLNode;
                                  AScrollBox        : TScrollBox) : Boolean;

implementation

uses
  UErrorHandlingOperations;

procedure UtilitySetIndicators (AScrollBox : TScrollBox);
const OPNAME = 'UHydroDlgUtilities.UtilitySetIndicators';
var
  LIndex   : Integer;
  LControl : TWRMFControl;
  LGrid    : TWRMFGrid;
  LRow     : Integer;
  LCol     : Integer;
begin
  try
    for LIndex := 0 to AScrollBox.ControlCount - 1 do
    begin
      if (AScrollBox.Controls[LIndex] is TWRMFGrid) then
      begin
        LGrid := TWRMFGrid(AScrollBox.Controls[LIndex]);
        for LRow := 1 to LGrid.RowCount - 1 do
        begin
          for LCol := 0 to LGrid.ColCount - 1 do
          begin
            LGrid.CellInfo[LRow, LCol].HasParamChange := FALSE;
            LGrid.CellInfo[LRow, LCol].HasMetaData    := FALSE;
          end;
        end;
      end
      else if (AScrollBox.Controls[LIndex] is TWRMFControl) then
      begin
        LControl := TWRMFControl(AScrollBox.Controls[LIndex]);
        LControl.HasParamChange := FALSE;
        LControl.HasMetaData    := FALSE;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure UtilitySetControls (AScrollBox        : TScrollBox;
                              AMayChangeNetwork : Boolean);
const OPNAME = 'UHydroDlgUtilities.UtilitySetControls';
var
  LIndex   : Integer;
begin
  try
    for LIndex := 0 to AScrollBox.ControlCount - 1 do
    begin
      if (AScrollBox.Controls[LIndex] is TWRMFGrid) then
        TWRMFGrid(AScrollBox.Controls[LIndex]).Active := AMayChangeNetwork
      else if (AScrollBox.Controls[LIndex] is TWRMFControl) then
        TWRMFControl(AScrollBox.Controls[LIndex]).Active := AMayChangeNetwork;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure UtilitySetAllValid (AScrollBox : TScrollBox);
const OPNAME = 'UHydroDlgUtilities.UtilitySetAllValid';
var
  LIndex    : Integer;
  LRow      : Integer;
  LCol      : Integer;
  LCellInfo : TCellInfo;
  LGrid     : TWRMFGrid;
begin
  try
    for LIndex := 0 to AScrollBox.ControlCount - 1 do
    begin
      if (AScrollBox.Controls[LIndex] is TWRMFGrid) then
      begin
        LGrid := TWRMFGrid(AScrollBox.Controls[LIndex]);
        for LRow := 1 to LGrid.RowCount - 1 do
        begin
          for LCol := 0 to LGrid.ColCount - 1 do
          begin
            LCellInfo := LGrid.CellInfo[LRow, LCol];
            LCellInfo.IsValid := TRUE;
          end;
        end;
      end
      else if (AScrollBox.Controls[LIndex] is TWRMFControl) then
        TWRMFControl(AScrollBox.Controls[LIndex]).IsValid := TRUE;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function UtilityLoadXMLData (ARootNode  : IXMLNode;
                             AScrollBox : TScrollBox): boolean;
const OPNAME = 'UHydroDlgUtilities.UtilityLoadXMLData';
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

    for LCount := 0 to AScrollBox.ControlCount - 1 do
    begin
      if (AScrollBox.Controls[LCount] is TWRMFGrid) then
      begin
        LGridControl  := TWRMFGrid(AScrollBox.Controls[LCount]);
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
      if (AScrollBox.Controls[LCount] is TWRMFEdit) then
      begin
        LEditControl  := TWRMFEdit(AScrollBox.Controls[LCount]);
        LPropertyName := LEditControl.PropertyName;
        try
          LEditControl.Text := LSectionNode.ChildNodes[LPropertyName].Text;
        except
          LEditControl.Text := '';
        end;
      end;
      if (AScrollBox.Controls[LCount] is TWRMFCheckBox) then
      begin
        LCheckControl := TWRMFCheckBox(AScrollBox.Controls[LCount]);
        LPropertyName := LCheckControl.PropertyName;
        try
          LCheckControl.Checked := (LSectionNode.ChildNodes[LPropertyName].Text = 'Y') OR
                                   (LSectionNode.ChildNodes[LPropertyName].Text = '1');
        except
          LCheckControl.Checked := FALSE;
        end;
      end;
      if (AScrollBox.Controls[LCount] is TWRMFComboBox) then
      begin
        LComboControl := TWRMFComboBox(AScrollBox.Controls[LCount]);
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

function UtilityStoreXMLData  (ARootNode  : IXMLNode;
                               AScrollBox : TScrollBox): Boolean;
const OPNAME = 'UHydroDlgUtilities.UtilityStoreXMLData';
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
      for LCount := 0 to AScrollBox.ControlCount - 1 do
      begin
        if (AScrollBox.Controls[LCount] is TWRMFGrid) then
        begin
          LGridControl  := TWRMFGrid(AScrollBox.Controls[LCount]);
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
        if (AScrollBox.Controls[LCount] is TWRMFComboBox) then
        begin
          LComboControl := TWRMFComboBox(AScrollBox.Controls[LCount]);
          LPropertyName := LComboControl.PropertyName;
          LIndex := LComboControl.ItemIndex;
          if (LIndex >= 0) then
            LSectionNode.ChildNodes[LPropertyName].Text := IntToStr(integer(LComboControl.Items.Objects[LIndex]))
          else
            LSectionNode.ChildNodes[LPropertyName].Text := '';
        end;
        if (AScrollBox.Controls[LCount] is TWRMFEdit) then
        begin
          LEditControl  := TWRMFEdit(AScrollBox.Controls[LCount]);
          LPropertyName := LEditControl.PropertyName;
          LSectionNode.ChildNodes[LPropertyName].Text := Trim(LEditControl.Text);
        end;
      end;
      Result := TRUE;
    except
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function UtilityDoValidation (AContext          : TValidationContext;
                              APropertyName     : String;
                              AFieldIndex       : String;
                              AXSD              : String;
                              AInputXML         : String;
                              AOutputXML        : String;
                              AStopOnFirstError : Boolean;
                              AErrorList        : TStringList;
                              AXMLAgent         : TXMLAgent;
                              ARootNode         : IXMLNode;
                              AScrollBox        : TScrollBox) : Boolean;
const OPNAME = 'UHydroDlgUtilities.UtilityDoValidation';
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
                                   AXSD,
                                   AInputXML,
                                   AOutputXML,
                                   AStopOnFirstError,
                                   AErrorList,
                                   LErrorMessages);
      for LIndex := 0 to AScrollBox.ControlCount - 1 do
      begin
        if (AScrollBox.Controls[LIndex] is TWRMFControl) then
        begin
          LControl := TWRMFControl(AScrollBox.Controls[LIndex]);
          LControl.IsValid := LControl.IsValid AND (AErrorList.IndexOf(LControl.PropertyName) < 0);
        end;
        if (AScrollBox.Controls[LIndex] is TWRMFGrid) then
        begin
          LGrid := TWRMFGrid(AScrollBox.Controls[LIndex]);
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
          while ((NOT LFoundControl) AND (LIndex < AScrollBox.ControlCount)) do
          begin
            if (AScrollBox.Controls[LIndex] is TWRMFControl) then
            begin
              LControl := TWRMFControl(AScrollBox.Controls[LIndex]);
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

