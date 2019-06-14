unit UReservoirPropertiesDlg;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, VCL.Graphics, VCL.Controls, VCL.Forms,
  VCL.Dialogs, VCL.StdCtrls, xmldom, XMLIntf, XMLDoc, VCL.ExtCtrls, VCL.Buttons, VCL.Menus,

  HydrologyCom_TLB,
  UControlIterator,
  UXMLAgent,
  UXMLAgentReservoirProperties,
  UWRMFEdit, UWRMFComboBox, UWRMFControl, UWRMFCheckBox, VCL.ComCtrls,
  VCL.Grids, UCellChangeStringGrid, UWRMFGrid, Xml.Win.msxmldom;

type
  TReservoirPropertiesDlg = class(TForm)
    FXMLDocumentIn           : TXMLDocument;
    FXMLDocumentOut          : TXMLDocument;
    PnlBottom                : TPanel;
    BtnReset                 : TButton;
    BtnApply                 : TButton;
    ScrClient                : TScrollBox;
    LblNetworkSequence       : TLabel;
    EdtNetworkSequence       : TWRMFEdit;
    LblModuleNumber          : TLabel;
    EdtModuleNumber          : TWRMFEdit;
    ChbActive                : TWRMFCheckBox;
    LblLatitude              : TLabel;
    LblLongitude             : TLabel;
    LblReservoirName         : TLabel;
    LblMAP                   : TLabel;
    LblRainfallFileName      : TLabel;
    LblAreaPower             : TLabel;
    LblSpillageRoute         : TLabel;
    LblInitialStorage        : TLabel;
    EdtInitialStorage        : TWRMFEdit;
    CbxSpillageRoute         : TWRMFComboBox;
    EdtAreaPower             : TWRMFEdit;
    EdtRainfallFileName      : TWRMFEdit;
    EdtMAP                   : TWRMFEdit;
    EdtReservoirName         : TWRMFEdit;
    EdtLongitude             : TWRMFEdit;
    EdtLatitude              : TWRMFEdit;
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
  private
    { Private declarations }
    FErrorList        : TStringList;
    FIdentifier       : String;
    FKeyValues        : WideString;
    FControlIterator  : TControlIterator;
    function LoadXMLData : Boolean;
    function StoreXMLData : Boolean;
    function DoValidation (AContext      : TValidationContext;
                           APropertyName : String) : Boolean;
    procedure SetIndicators;
    procedure SetControls;
    procedure SetAllValid;
  public
    { Public declarations }
    FMayChangeNetwork : Boolean;
    FXMLAgent         : TXMLAgentReservoirProperties;
    FHydrologyModel   : IHydrologyModel;
  end;

implementation

uses
  UErrorHandlingOperations;

{$R *.dfm}

procedure TReservoirPropertiesDlg.FormCreate(Sender: TObject);
const OPNAME = 'TReservoirPropertiesDlg.FormCreate';
begin
  try
    FErrorList := TStringList.Create;
    FControlIterator := TControlIterator.Create;
    FControlIterator.ControlParent := ScrClient;
    FXMLAgent := TXMLAgentReservoirProperties.Create;
    FXMLAgent.DlgXMLDocumentIn  := FXMLDocumentIn;
    FXMLAgent.DlgXMLDocumentOut := FXMLDocumentOut;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirPropertiesDlg.FormDestroy(Sender: TObject);
const OPNAME = 'TReservoirPropertiesDlg.FormDestroy';
begin
  try
    FreeAndNil(FErrorList);
    FreeAndNil(FControlIterator);
    FreeAndNil(FXMLAgent);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirPropertiesDlg.FormShow(Sender: TObject);
const OPNAME = 'TReservoirPropertiesDlg.FormShow';
begin
  try
    LoadXMLData;
    SetControls;
    SetIndicators;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirPropertiesDlg.SetControls;
const OPNAME = 'TReservoirPropertiesDlg.SetControls';
begin
  try
    FControlIterator.SetControls(FMayChangeNetwork);
    EdtNetworkSequence.Active  := FALSE;
    EdtModuleNumber.Active     := FALSE;
    BtnApply.Enabled := FMayChangeNetwork;
    BtnReset.Enabled := FMayChangeNetwork;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirPropertiesDlg.SetAllValid;
const OPNAME = 'TReservoirPropertiesDlg.SetAllValid';
begin
  try
    FControlIterator.SetAllValid;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirPropertiesDlg.SetIndicators;
const OPNAME = 'TReservoirPropertiesDlg.SetIndicators';
begin
  try
    FControlIterator.SetIndicators;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirPropertiesDlg.LoadXMLData: boolean;
const OPNAME = 'TReservoirPropertiesDlg.LoadXMLData';
var
  LRootNode                : IXMLNode;
  LSectionNode             : IXMLNode;
  LOutflowRoutesNode       : IXMLNode;
begin
  Result := FALSE;
  try
    FXMLDocumentIn.Active    := TRUE;
    FXMLDocumentOut.Active   := TRUE;
    LRootNode                := FXMLDocumentIn.DocumentElement;
    LSectionNode             := LRootNode.ChildNodes['ReservoirProperties'];
    LOutflowRoutesNode       := LRootNode.ChildNodes['OutflowRoutes'];

    FIdentifier := LRootNode.ChildNodes['Identifier'].Text;
    FKeyValues  := 'Model='       + QuotedStr(LRootNode.ChildNodes['Model'].Text) +
                   ',NetworkID='  + QuotedStr(LRootNode.ChildNodes['NetworkID'].Text) +
                   ',ModuleType=' + QuotedStr(LRootNode.ChildNodes['ModuleType'].Text) +
                   ',Identifier=' + LRootNode.ChildNodes['Identifier'].Text;

    // Caption.
    try
      Caption := 'Reservoir Module RV' + LSectionNode.ChildNodes['ModuleNumber'].Text;
    except
      Caption := 'Reservoir Module';
    end;

    // Populate spillage route combobox.
    FXMLAgent.PopulateCbxAllNetworkRoutes(CbxSpillageRoute, LOutflowRoutesNode, FALSE);

    Result := FControlIterator.LoadXMLData(LRootNode);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirPropertiesDlg.StoreXMLData : Boolean;
const OPNAME = 'TReservoirPropertiesDlg.StoreXMLData';
var
  LRootNode    : IXMLNode;
  LSectionNode : IXMLNode;
begin
  Result := FALSE;
  try
    LRootNode := FXMLDocumentOut.DocumentElement;
    LSectionNode := LRootNode.ChildNodes['ReservoirProperties'];
    LRootNode.ChildNodes['Identifier'].Text := FIdentifier;
    Result := FControlIterator.StoreXMLData(LRootNode);
    if (ChbActive.Checked) then
      LSectionNode.ChildNodes['Active'].Text := 'Y'
    else
      LSectionNode.ChildNodes['Active'].Text := 'N';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirPropertiesDlg.ControlExit(Sender: TObject);
const OPNAME = 'TReservoirPropertiesDlg.ControlExit';
var
  LControl : TWRMFControl;
begin
  try
    LControl := TWRMFControl(TControl(Sender).Parent);
    LControl.IsValid := TRUE;
    DoValidation(tcChange, LControl.PropertyName);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirPropertiesDlg.BtnApplyClick(Sender: TObject);
const OPNAME = 'TReservoirPropertiesDlg.BtnApplyClick';
begin
  try
    SetAllValid;
    if (DoValidation(tcApply, '')) then
    begin
      FHydrologyModel.UpdateNetworkData(FXMLDocumentOut.XML.Text);
      LoadXMLData;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirPropertiesDlg.BtnResetClick(Sender: TObject);
const OPNAME = 'TReservoirPropertiesDlg.BtnResetClick';
begin
  try
    LoadXMLData;
    SetAllValid;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirPropertiesDlg.DoValidation (AContext      : TValidationContext;
                                               APropertyName : String) : Boolean;
const OPNAME = 'TReservoirPropertiesDlg.DoValidation';
var
  LRootNode      : IXMLNode;
begin
  Result := FALSE;
  try
    if ((AContext = tcChange) OR (AContext = tcApply)) then
    begin
      Result := StoreXMLData;
      if (Result) then
      begin
        LRootNode := FXMLDocumentOut.DocumentElement;
        Result := FControlIterator.DoValidation(AContext, APropertyName, '', 
                                      FXMLDocumentIn.XML.Text, FXMLDocumentOut.XML.Text, FALSE,
                                      FErrorList, FXMLAgent, LRootNode);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirPropertiesDlg.ParamChangeIndicatorClicked (Sender: TObject);
const OPNAME = 'TReservoirPropertiesDlg.ParamChangeIndicatorClicked';
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

procedure TReservoirPropertiesDlg.MetaDataIndicatorClicked (Sender: TObject);
const OPNAME = 'TReservoirPropertiesDlg.MetaDataIndicatorClicked';
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

procedure TReservoirPropertiesDlg.GridParamChangeIndicatorClicked (Sender: TObject; ACol, ARow: integer);
const OPNAME = 'TReservoirPropertiesDlg.GridParamChangeIndicatorClicked';
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

procedure TReservoirPropertiesDlg.GridMetaDataIndicatorClicked (Sender: TObject; ACol, ARow: integer);
const OPNAME = 'TReservoirPropertiesDlg.GridMetaDataIndicatorClicked';
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
