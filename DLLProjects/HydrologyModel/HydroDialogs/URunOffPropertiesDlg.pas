unit URunOffPropertiesDlg;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, VCL.Graphics, VCL.Controls, VCL.Forms,
  VCL.Dialogs, VCL.StdCtrls, xmldom, XMLIntf, XMLDoc, VCL.ExtCtrls, VCL.Buttons, VCL.Menus,

  HydrologyCom_TLB,
  UControlIterator,
  UXMLAgent,
  UXMLAgentRunOffProperties,
  UWRMFEdit, UWRMFComboBox, UWRMFControl, UWRMFCheckBox, VCL.ComCtrls,
  VCL.Grids, UCellChangeStringGrid, UWRMFGrid, Xml.Win.msxmldom;

type
  TRunOffPropertiesDlg = class(TForm)
    PnlBottom                : TPanel;
    BtnApply                 : TButton;
    BtnReset                 : TButton;
    FXMLDocumentIn           : TXMLDocument;
    FXMLDocumentOut          : TXMLDocument;
    ScrClient                : TScrollBox;
    LblNetworkSequence       : TLabel;
    LblModuleNumber          : TLabel;
    ChbActive                : TWRMFCheckBox;
    LblLatitude              : TLabel;
    LblLongitude             : TLabel;
    LblRunOffName            : TLabel;
    LblVersionNo             : TLabel;
    LblCatchmentArea         : TLabel;
    LblCatchmentMAP          : TLabel;
    LblRainfallFileName      : TLabel;
    ChbNaturalisedFlows      : TWRMFCheckBox;
    EdtRainfallFileName      : TWRMFEdit;
    EdtCatchmentMAP          : TWRMFEdit;
    EdtCatchmentArea         : TWRMFEdit;
    EdtVersionNo             : TWRMFEdit;
    EdtRunOffName            : TWRMFEdit;
    EdtLongitude             : TWRMFEdit;
    EdtLatitude              : TWRMFEdit;
    EdtModuleNumber          : TWRMFEdit;
    EdtNetworkSequence       : TWRMFEdit;
    GrdAPan                  : TWRMFGrid;
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
    procedure GrdAPanDataCellExit(ASender: TObject; ACol, ARow: Integer);
    procedure GrdAPanExit(Sender: TObject);
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
    FXMLAgent         : TXMLAgentRunOffProperties;
    FHydrologyModel   : IHydrologyModel;
  end;


implementation

uses
  UErrorHandlingOperations;

{$R *.dfm}

procedure TRunOffPropertiesDlg.FormCreate(Sender: TObject);
const OPNAME = 'TRunOffPropertiesDlg.FormCreate';
begin
  try
    FErrorList := TStringList.Create;
    FControlIterator := TControlIterator.Create;
    FControlIterator.ControlParent := ScrClient;
    FXMLAgent := TXMLAgentRunOffProperties.Create;
    FXMLAgent.DlgXMLDocumentIn  := FXMLDocumentIn;
    FXMLAgent.DlgXMLDocumentOut := FXMLDocumentOut;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRunOffPropertiesDlg.FormDestroy(Sender: TObject);
const OPNAME = 'TRunOffPropertiesDlg.FormDestroy';
begin
  try
    FreeAndNil(FErrorList);
    FreeAndNil(FControlIterator);
    FreeAndNil(FXMLAgent);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRunOffPropertiesDlg.FormShow(Sender: TObject);
const OPNAME = 'TRunOffPropertiesDlg.FormShow';
begin
  try
    LoadXMLData;
    SetControls;
    SetIndicators;
    GrdAPan.Cells[0, 0] := 'Month';
    GrdAPan.Cells[1, 0] := 'A-pan factors';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRunOffPropertiesDlg.SetControls;
const OPNAME = 'TRunOffPropertiesDlg.SetControls';
begin
  try
    FControlIterator.SetControls(FMayChangeNetwork);
    EdtNetworkSequence.Active  := FALSE;
    EdtModuleNumber.Active     := FALSE;
    BtnApply.Enabled           := FMayChangeNetwork;
    BtnReset.Enabled           := FMayChangeNetwork;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRunOffPropertiesDlg.SetIndicators;
const OPNAME = 'TRunOffPropertiesDlg.SetIndicators';
begin
  try
    FControlIterator.SetIndicators;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRunOffPropertiesDlg.SetAllValid;
const OPNAME = 'TRunOffPropertiesDlg.SetAllValid';
begin
  try
    FControlIterator.SetAllValid;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRunOffPropertiesDlg.LoadXMLData: boolean;
const OPNAME = 'TRunOffPropertiesDlg.LoadXMLData';
var
  LRootNode         : IXMLNode;
  LSectionNode      : IXMLNode;
begin
  Result := FALSE;
  try
    FXMLDocumentIn.Active  := TRUE;
    FXMLDocumentOut.Active := TRUE;
    LRootNode              := FXMLDocumentIn.DocumentElement;
    LSectionNode           := LRootNode.ChildNodes['RunOffProperties'];

    FIdentifier := LRootNode.ChildNodes['Identifier'].Text;
    FKeyValues  := 'Model='       + QuotedStr(LRootNode.ChildNodes['Model'].Text) +
                   ',NetworkID='  + QuotedStr(LRootNode.ChildNodes['NetworkID'].Text) +
                   ',ModuleType=' + QuotedStr(LRootNode.ChildNodes['ModuleType'].Text) +
                   ',Identifier=' + LRootNode.ChildNodes['Identifier'].Text;

    // Caption
    try
      Caption := 'RunOff Module RU' + LSectionNode.ChildNodes['ModuleNumber'].Text;
    except
      Caption := 'RunOff Module';
    end;

    Result := FControlIterator.LoadXMLData(LRootNode);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRunOffPropertiesDlg.StoreXMLData : Boolean;
const OPNAME = 'TRunOffPropertiesDlg.StoreXMLData';
var
  LRootNode        : IXMLNode;
  LSectionNode     : IXMLNode;
begin
  Result := FALSE;
  try
    LRootNode         := FXMLDocumentOut.DocumentElement;
    LSectionNode      := LRootNode.ChildNodes['RunOffProperties'];
    LRootNode.ChildNodes['Identifier'].Text := FIdentifier;
    Result := FControlIterator.StoreXMLData(LRootNode);
    if (ChbActive.Checked) then
      LSectionNode.ChildNodes['Active'].Text := 'Y'
    else
      LSectionNode.ChildNodes['Active'].Text := 'N';
    if (ChbNaturalisedFlows.Checked) then
      LSectionNode.ChildNodes['ProduceNaturalisedFlows'].Text := '1'
    else
      LSectionNode.ChildNodes['ProduceNaturalisedFlows'].Text := '0';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRunOffPropertiesDlg.ControlExit(Sender: TObject);
const OPNAME = 'TRunOffPropertiesDlg.ControlExit';
var
  LControl : TWRMFControl;
begin
  try
    LControl := TWRMFControl(TControl(Sender).Parent);
    LControl.IsValid := TRUE;
    DoValidation(tcChange, LControl.PropertyName, '');
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRunOffPropertiesDlg.GrdAPanDataCellExit(ASender: TObject; ACol, ARow: Integer);
const OPNAME = 'TRunOffPropertiesDlg.GrdAPanDataCellExit';
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

procedure TRunOffPropertiesDlg.GrdAPanExit(Sender: TObject);
const OPNAME = 'TRunOffPropertiesDlg.GrdAPanExit';
var
  LWRMFGrid : TWRMFGrid;
begin
  try
    LWRMFGrid := TWRMFGrid(Sender);
    GrdAPanDataCellExit(LWRMFGrid, LWRMFGrid.Col, LWRMFGrid.Row);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRunOffPropertiesDlg.BtnApplyClick(Sender: TObject);
const OPNAME = 'TRunOffPropertiesDlg.BtnApplyClick';
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

procedure TRunOffPropertiesDlg.BtnResetClick(Sender: TObject);
const OPNAME = 'TRunOffPropertiesDlg.BtnResetClick';
begin
  try
    LoadXMLData;
    SetAllValid;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRunOffPropertiesDlg.DoValidation (AContext      : TValidationContext;
                                            APropertyName : String;
                                            AFieldIndex   : String) : Boolean;
const OPNAME = 'TRunOffPropertiesDlg.DoValidation';
var
  LRootNode  : IXMLNode;
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

procedure TRunOffPropertiesDlg.ParamChangeIndicatorClicked (Sender: TObject);
const OPNAME = 'TRunOffPropertiesDlg.ParamChangeIndicatorClicked';
//var
//  LControl : TWRMFControl;
begin
  try
{
    if (Sender is TWRMFControl) then
    begin
      LControl := TWRMFControl(Sender);
      if (LControl.HasParamChange) then
        ShowMessage ('SHOW Param Changes for ' + LControl.PropertyName + ' ' + FKeyValues)
      else
        ShowMessage ('NEW Param Changes for ' + LControl.PropertyName + ' ' + FKeyValues);
    end;
}
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRunOffPropertiesDlg.MetaDataIndicatorClicked (Sender: TObject);
const OPNAME = 'TRunOffPropertiesDlg.MetaDataIndicatorClicked';
//var
//  LControl : TWRMFControl;
begin
  try
{
    if (Sender is TWRMFControl) then
    begin
      LControl := TWRMFControl(Sender);
      if ((Sender is TWRMFControl) AND (TWRMFControl(Sender).HasMetaData)) then
        ShowMessage ('SHOW Meta Data for ' + LControl.PropertyName + ' ' + FKeyValues)
      else
        ShowMessage ('NEW Meta Data for ' + LControl.PropertyName + ' ' + FKeyValues);
    end;
}
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRunOffPropertiesDlg.GridParamChangeIndicatorClicked (Sender: TObject; ACol, ARow: integer);
const OPNAME = 'TRunOffPropertiesDlg.GridParamChangeIndicatorClicked';
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

procedure TRunOffPropertiesDlg.GridMetaDataIndicatorClicked (Sender: TObject; ACol, ARow: integer);
const OPNAME = 'TRunOffPropertiesDlg.GridMetaDataIndicatorClicked';
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


end.
