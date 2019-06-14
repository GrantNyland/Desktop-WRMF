unit UIrrigationPropertiesDlg;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, VCL.Graphics, VCL.Controls, VCL.Forms,
  VCL.Dialogs, VCL.StdCtrls, xmldom, XMLIntf, XMLDoc, VCL.ExtCtrls, VCL.Buttons, VCL.Menus,

  HydrologyCom_TLB,
  UControlIterator,
  UXMLAgent,
  UXMLAgentIrrigationProperties,
  UWRMFEdit, UWRMFComboBox, UWRMFControl, UWRMFCheckBox, VCL.ComCtrls,
  VCL.Grids, UCellChangeStringGrid, UWRMFGrid, Xml.Win.msxmldom;

type
  TIrrigationPropertiesDlg = class(TForm)
    LblNetworkSequence                  : TLabel;
    LblModuleNumber                     : TLabel;
    LblLatitude                         : TLabel;
    LblLongitude                        : TLabel;
    LblIrrigationName                   : TLabel;
    LblVersionNo                        : TLabel;
    LblModelType                        : TLabel;
    LblLastUsedModelType                : TLabel;
    LblMAP                              : TLabel;
    LblRainfallFileName                 : TLabel;
    LblMaxAnnualIrrAllocation           : TLabel;
    LblAbstractionRoute                 : TLabel;
    LblReturnFlowRoute                  : TLabel;
    LblReturnFlowPercentage             : TLabel;
    PnlBottom                           : TPanel;
    BtnApply                            : TButton;
    BtnReset                            : TButton;
    FXMLDocumentIn                      : TXMLDocument;
    FXMLDocumentOut                     : TXMLDocument;
    ScrClient                           : TScrollBox;
    ChbActive                           : TWRMFCheckBox;
    EdtReturnFlowPercentage             : TWRMFEdit;
    CbxReturnFlowRoute                  : TWRMFComboBox;
    CbxAbstractionRoute                 : TWRMFComboBox;
    EdtMaxAnnualIrrAllocation           : TWRMFEdit;
    EdtRainfallFileName                 : TWRMFEdit;
    EdtMAP                              : TWRMFEdit;
    CbxLastUsedModelType                : TWRMFComboBox;
    CbxModelType                        : TWRMFComboBox;
    EdtVersionNo                        : TWRMFEdit;
    EdtIrrigationName                   : TWRMFEdit;
    EdtLongitude                        : TWRMFEdit;
    EdtLatitude                         : TWRMFEdit;
    EdtModuleNumber                     : TWRMFEdit;
    EdtNetworkSequence                  : TWRMFEdit;
    procedure FormShow(Sender: TObject);
    procedure ControlExit(Sender: TObject);
    procedure BtnApplyClick(Sender: TObject);
    procedure BtnResetClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ParamChangeIndicatorClicked (Sender: TObject);
    procedure MetaDataIndicatorClicked (Sender: TObject);
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
    FXMLAgent         : TXMLAgentIrrigationProperties;
    FHydrologyModel   : IHydrologyModel;
  end;

implementation

uses
  UErrorHandlingOperations;

{$R *.dfm}

procedure TIrrigationPropertiesDlg.FormCreate(Sender: TObject);
const OPNAME = 'TIrrigationPropertiesDlg.FormCreate';
begin
  try
    FErrorList := TStringList.Create;
    FControlIterator := TControlIterator.Create;
    FControlIterator.ControlParent := ScrClient;
    FXMLAgent := TXMLAgentIrrigationProperties.Create;
    FXMLAgent.DlgXMLDocumentIn  := FXMLDocumentIn;
    FXMLAgent.DlgXMLDocumentOut := FXMLDocumentOut;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrigationPropertiesDlg.FormDestroy(Sender: TObject);
const OPNAME = 'TIrrigationPropertiesDlg.FormDestroy';
begin
  try
    FreeAndNil(FErrorList);
    FreeAndNil(FControlIterator);
    FreeAndNil(FXMLAgent);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrigationPropertiesDlg.FormShow(Sender: TObject);
const OPNAME = 'TIrrigationPropertiesDlg.FormShow';
begin
  try
    LoadXMLData;
    SetControls;
    SetIndicators;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrigationPropertiesDlg.SetControls;
const OPNAME = 'TIrrigationPropertiesDlg.SetControls';
begin
  try
    FControlIterator.SetControls(FMayChangeNetwork);
    EdtNetworkSequence.Active  := FALSE;
    EdtModuleNumber.Active     := FALSE;
    BtnApply.Enabled := FMayChangeNetwork;
    BtnReset.Enabled := FMayChangeNetwork;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrigationPropertiesDlg.SetIndicators;
const OPNAME = 'TIrrigationPropertiesDlg.SetIndicators';
begin
  try
    FControlIterator.SetIndicators;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrigationPropertiesDlg.SetAllValid;
const OPNAME = 'TIrrigationPropertiesDlg.SetAllValid';
begin
  try
    FControlIterator.SetAllValid;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TIrrigationPropertiesDlg.LoadXMLData: boolean;
const OPNAME = 'TIrrigationPropertiesDlg.LoadXMLData';
var
  LRootNode                 : IXMLNode;
  LSectionNode              : IXMLNode;
  LAllOutflowRoutesNode     : IXMLNode;
  LAllInflowRoutesNode      : IXMLNode;
begin
  Result := FALSE;
  try
    FXMLDocumentIn.Active     := TRUE;
    FXMLDocumentOut.Active    := TRUE;
    LRootNode                 := FXMLDocumentIn.DocumentElement;
    LSectionNode              := LRootNode.ChildNodes['IrrigationProperties'];
    LAllOutflowRoutesNode     := LRootNode.ChildNodes['OutflowRoutes'];
    LAllInflowRoutesNode      := LRootNode.ChildNodes['InflowRoutes'];

    FIdentifier := LRootNode.ChildNodes['Identifier'].Text;
    FKeyValues  := 'Model='       + QuotedStr(LRootNode.ChildNodes['Model'].Text) +
                   ',NetworkID='  + QuotedStr(LRootNode.ChildNodes['NetworkID'].Text) +
                   ',ModuleType=' + QuotedStr(LRootNode.ChildNodes['ModuleType'].Text) +
                   ',Identifier=' + LRootNode.ChildNodes['Identifier'].Text;

    // Caption.
    try
      Caption := 'Irrigation Module RR' + LSectionNode.ChildNodes['ModuleNumber'].Text;
    except
      Caption := 'Irrigation Module';
    end;

    // ModelType
    CbxModelType.Items.Clear;
    CbxModelType.Items.AddObject('WRSM', pointer(1));
    CbxModelType.Items.AddObject('WQT',  pointer(2));
    CbxModelType.Items.AddObject('WRSM + WQT', pointer(3));
    // LastUsedModelType
    CbxLastUsedModelType.Items.Clear;
    CbxLastUsedModelType.Items.AddObject('WRSM', pointer(1));
    CbxLastUsedModelType.Items.AddObject('WQT',  pointer(2));
    CbxLastUsedModelType.Items.AddObject('WRSM + WQT', pointer(3));
    // Populate route comboboxes.
    FXMLAgent.PopulateCbxAllNetworkRoutes(CbxAbstractionRoute, LAllInflowRoutesNode, FALSE);
    FXMLAgent.PopulateCbxAllNetworkRoutes(CbxReturnFlowRoute, LAllOutflowRoutesNode, FALSE);

    Result := FControlIterator.LoadXMLData(LRootNode);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TIrrigationPropertiesDlg.StoreXMLData : Boolean;
const OPNAME = 'TIrrigationPropertiesDlg.StoreXMLData';
var
  LRootNode    : IXMLNode;
  LSectionNode : IXMLNode;
begin
  Result := FALSE;
  try
    LRootNode := FXMLDocumentOut.DocumentElement;
    LSectionNode := LRootNode.ChildNodes['IrrigationProperties'];
    LRootNode.ChildNodes['Identifier'].Text := FIdentifier;
    Result := FControlIterator.StoreXMLData(LRootNode);
    if (ChbActive.Checked) then
      LSectionNode.ChildNodes['Active'].Text := 'Y'
    else
      LSectionNode.ChildNodes['Active'].Text := 'N';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrigationPropertiesDlg.ControlExit(Sender: TObject);
const OPNAME = 'TIrrigationPropertiesDlg.ControlExit';
var
  LControl : TWRMFControl;
begin
  try
    LControl := TWRMFControl(TControl(Sender).Parent);
    LControl.IsValid := TRUE;
    DoValidation(tcChange, LControl.PropertyName);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrigationPropertiesDlg.BtnApplyClick(Sender: TObject);
const OPNAME = 'TIrrigationPropertiesDlg.BtnApplyClick';
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

procedure TIrrigationPropertiesDlg.BtnResetClick(Sender: TObject);
const OPNAME = 'TIrrigationPropertiesDlg.BtnResetClick';
begin
  try
    LoadXMLData;
    SetAllValid;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TIrrigationPropertiesDlg.DoValidation (AContext      : TValidationContext;
                                                APropertyName : String) : Boolean;
const OPNAME = 'TIrrigationPropertiesDlg.DoValidation';
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
        Result := FControlIterator.DoValidation(AContext, APropertyName, '',
                                      FXMLDocumentIn.XML.Text, FXMLDocumentOut.XML.Text, FALSE,
                                      FErrorList, FXMLAgent, LRootNode);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrigationPropertiesDlg.ParamChangeIndicatorClicked (Sender: TObject);
const OPNAME = 'TIrrigationPropertiesDlg.ParamChangeIndicatorClicked';
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

procedure TIrrigationPropertiesDlg.MetaDataIndicatorClicked (Sender: TObject);
const OPNAME = 'TIrrigationPropertiesDlg.MetaDataIndicatorClicked';
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

end.
