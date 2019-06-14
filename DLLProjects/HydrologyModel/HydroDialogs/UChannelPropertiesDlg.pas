unit UChannelPropertiesDlg;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, VCL.Graphics, VCL.Controls, VCL.Forms,
  VCL.Dialogs, VCL.StdCtrls, xmldom, XMLIntf, XMLDoc, VCL.ExtCtrls, VCL.Buttons, VCL.Menus,

  HydrologyCom_TLB,
  UControlIterator,
  UXMLAgent,
  UXMLAgentChannelProperties,
  UWRMFEdit, UWRMFComboBox, UWRMFControl, UWRMFCheckBox, VCL.ComCtrls,
  VCL.Grids, UCellChangeStringGrid, UWRMFGrid, Xml.Win.msxmldom;

type
  TChannelPropertiesDlg = class(TForm)
    PnlBottom                          : TPanel;
    BtnApply                           : TButton;
    BtnReset                           : TButton;
    FXMLDocumentIn                     : TXMLDocument;
    FXMLDocumentOut                    : TXMLDocument;
    ScrClient                          : TScrollBox;
    LblNetworkSequence                 : TLabel;
    LblModuleNumber                    : TLabel;
    ChbActive                          : TWRMFCheckBox;
    LblLatitude                        : TLabel;
    LblLongitude                       : TLabel;
    LblChannelName                     : TLabel;
    LblVersionNo                       : TLabel;
    LblWetlandMAP                      : TLabel;
    LblRainfallFileName                : TLabel;
    LblMonthlyBedLoss                  : TLabel;
    LblWetlandStorage                  : TLabel;
    LblWetlandArea                     : TLabel;
    LblWetlandRechargeCoefficient      : TLabel;
    LblPrincipalOutflowRoute           : TLabel;
    CbxPrincipalOutflowRoute           : TWRMFComboBox;
    EdtWetlandRechargeCoefficient      : TWRMFEdit;
    EdtWetlandArea                     : TWRMFEdit;
    EdtWetlandStorage                  : TWRMFEdit;
    EdtMonthlyBedLoss                  : TWRMFEdit;
    EdtRainfallFileName                : TWRMFEdit;
    EdtWetlandMAP                      : TWRMFEdit;
    EdtVersionNo                       : TWRMFEdit;
    EdtChannelName                     : TWRMFEdit;
    EdtLongitude                       : TWRMFEdit;
    EdtLatitude                        : TWRMFEdit;
    EdtModuleNumber                    : TWRMFEdit;
    EdtNetworkSequence                 : TWRMFEdit;
    LblWetlandsInflowRoute             : TLabel;
    LblWetlandsOutflowRoute            : TLabel;
    LblDiversionRoute                  : TLabel;
    LblBankfillCapacity                : TLabel;
    LblDiversionEfficiency             : TLabel;
    LblMaxMonthlyDiversionCapacity     : TLabel;
    LblWetlandType                     : TLabel;
    LblBankfillArea                    : TLabel;
    LblBankfillVolume                  : TLabel;
    LblPowerOfAreaCapCurve             : TLabel;
    LblBankfillCapacityComp            : TLabel;
    LblWetlandInflowProportion         : TLabel;
    LblChannelInflowProportion         : TLabel;
    LblQDIV                            : TLabel;
    EdtQDiv                            : TWRMFEdit;
    EdtChannelInflowProportion         : TWRMFEdit;
    EdtWetlandInflowProportion         : TWRMFEdit;
    EdtBankfillCapacityComp            : TWRMFEdit;
    EdtPowerOfAreaCapCurve             : TWRMFEdit;
    EdtBankfillVolume                  : TWRMFEdit;
    EdtBankfillArea                    : TWRMFEdit;
    CbxWetlandType                     : TWRMFComboBox;
    EdtMaxMonthlyDiversionCapacity     : TWRMFEdit;
    EdtDiversionEfficiency             : TWRMFEdit;
    EdtBankfillCapacity                : TWRMFEdit;
    CbxDiversionRoute                  : TWRMFComboBox;
    CbxWetlandsOutflowRoute            : TWRMFComboBox;
    CbxWetlandsInflowRoute             : TWRMFComboBox;
    procedure FormShow(Sender: TObject);
    procedure ControlExit(Sender: TObject);
    procedure BtnApplyClick(Sender: TObject);
    procedure BtnResetClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ParamChangeIndicatorClicked (Sender: TObject);
    procedure MetaDataIndicatorClicked (Sender: TObject);
    procedure CbxWetlandTypeChange(Sender: TObject);
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
//    procedure SetWetlandTypeDependentControls;
  public
    { Public declarations }
    FMayChangeNetwork : Boolean;
    FXMLAgent         : TXMLAgentChannelProperties;
    FHydrologyModel   : IHydrologyModel;
  end;

implementation

uses
  UErrorHandlingOperations;

{$R *.dfm}

procedure TChannelPropertiesDlg.FormCreate(Sender: TObject);
const OPNAME = 'TChannelPropertiesDlg.FormCreate';
begin
  try
    FErrorList := TStringList.Create;
    FControlIterator := TControlIterator.Create;
    FControlIterator.ControlParent := ScrClient;
    FXMLAgent := TXMLAgentChannelProperties.Create;
    FXMLAgent.DlgXMLDocumentIn  := FXMLDocumentIn;
    FXMLAgent.DlgXMLDocumentOut := FXMLDocumentOut;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TChannelPropertiesDlg.FormDestroy(Sender: TObject);
const OPNAME = 'TChannelPropertiesDlg.FormDestroy';
begin
  try
    FreeAndNil(FErrorList);
    FreeAndNil(FControlIterator);
    FreeAndNil(FXMLAgent);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TChannelPropertiesDlg.FormShow(Sender: TObject);
const OPNAME = 'TChannelPropertiesDlg.FormShow';
begin
  try
    LoadXMLData;
    SetIndicators;
    SetControls;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TChannelPropertiesDlg.SetIndicators;
const OPNAME = 'TChannelPropertiesDlg.SetIndicators';
begin
  try
    FControlIterator.SetIndicators;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TChannelPropertiesDlg.SetAllValid;
const OPNAME = 'TChannelPropertiesDlg.SetAllValid';
begin
  try
    FControlIterator.SetAllValid;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TChannelPropertiesDlg.SetControls;
const OPNAME = 'TChannelPropertiesDlg.SetControls';
begin
  try
    FControlIterator.SetControls(FMayChangeNetwork);
    EdtNetworkSequence.Active  := FALSE;
    EdtModuleNumber.Active     := FALSE;
    BtnApply.Enabled := FMayChangeNetwork;
    BtnReset.Enabled := FMayChangeNetwork;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TChannelPropertiesDlg.LoadXMLData: boolean;
const OPNAME = 'TChannelPropertiesDlg.LoadXMLData';
var
  LRootNode              : IXMLNode;
  LSectionNode           : IXMLNode;
  LWetlandType           : Integer;
  LOutflowRoutesNode     : IXMLNode;
  LInflowRoutesNode      : IXMLNode;
begin
  Result := FALSE;
  try
    FXMLDocumentIn.Active  := TRUE;
    FXMLDocumentOut.Active := TRUE;
    LRootNode              := FXMLDocumentIn.DocumentElement;
    LSectionNode           := LRootNode.ChildNodes['ChannelProperties'];
    LOutflowRoutesNode     := LRootNode.ChildNodes['OutflowRoutes'];
    LInflowRoutesNode      := LRootNode.ChildNodes['InflowRoutes'];

    FIdentifier := LRootNode.ChildNodes['Identifier'].Text;
    FKeyValues  := 'Model='       + QuotedStr(LRootNode.ChildNodes['Model'].Text) +
                   ',NetworkID='  + QuotedStr(LRootNode.ChildNodes['NetworkID'].Text) +
                   ',ModuleType=' + QuotedStr(LRootNode.ChildNodes['ModuleType'].Text) +
                   ',Identifier=' + LRootNode.ChildNodes['Identifier'].Text;

    // Caption
    try
      Caption := 'Channel Module CR' + LSectionNode.ChildNodes['ModuleNumber'].Text;
    except
      Caption := 'Channel Module';
    end;
    // Populate route comboboxes.
    FXMLAgent.PopulateCbxAllNetworkRoutes(CbxPrincipalOutflowRoute, LOutflowRoutesNode, FALSE);
    FXMLAgent.PopulateCbxAllNetworkRoutes(CbxWetlandsInflowRoute, LInflowRoutesNode, TRUE);
    FXMLAgent.PopulateCbxAllNetworkRoutes(CbxWetlandsOutflowRoute, LOutflowRoutesNode, TRUE);
    FXMLAgent.PopulateCbxAllNetworkRoutes(CbxDiversionRoute, LOutflowRoutesNode, TRUE);
    // WetlandType
    CbxWetlandType.Items.Clear;
    CbxWetlandType.Items.AddObject('No wetland', pointer(0));
    CbxWetlandType.Items.AddObject('Basic', pointer(1));
    CbxWetlandType.Items.AddObject('Comprehensive', pointer(2));
    CbxWetlandType.Items.AddObject('Diversion channel', pointer(3));
    CbxWetlandType.ItemIndex := 0;
    Result := FControlIterator.LoadXMLData(LRootNode);
    LWetlandType := StrToInt(LSectionNode.ChildNodes['WetlandType'].Text);
    if (LWetlandType <> 2) then
    begin
      EdtBankfillArea.Text            := '0';
      EdtBankfillVolume.Text          := '0';
      EdtPowerOfAreaCapCurve.Text     := '0';
      EdtBankfillCapacityComp.Text    := '0';
      EdtWetlandInflowProportion.Text := '0';
      EdtChannelInflowProportion.Text := '0';
    end;

    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

(*
procedure TChannelPropertiesDlg.SetWetlandTypeDependentControls;
const OPNAME = 'TChannelPropertiesDlg.SetWetlandTypeDependentControls';
var
  LVisible : Boolean;
begin
  try
    LVisible := (CbxWetlandType.ItemIndex = 2);
    EdtBankfillArea.Visible            := LVisible;
    EdtBankfillVolume.Visible          := LVisible;
    EdtPowerOfAreaCapCurve.Visible     := LVisible;
    EdtBankfillCapacityComp.Visible    := LVisible;
    EdtWetlandInflowProportion.Visible := LVisible;
    EdtChannelInflowProportion.Visible := LVisible;
    LblBankfillArea.Visible            := LVisible;
    LblBankfillVolume.Visible          := LVisible;
    LblPowerOfAreaCapCurve.Visible     := LVisible;
    LblBankfillCapacityComp.Visible    := LVisible;
    LblWetlandInflowProportion.Visible := LVisible;
    LblChannelInflowProportion.Visible := LVisible;
  except on E: Exception do HandleError(E, OPNAME) end;
end;
*)

procedure TChannelPropertiesDlg.CbxWetlandTypeChange(Sender: TObject);
const OPNAME = 'TChannelPropertiesDlg.CbxWetlandTypeChange';
begin
  try
//    SetWetlandTypeDependentControls;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TChannelPropertiesDlg.StoreXMLData : Boolean;
const OPNAME = 'TChannelPropertiesDlg.StoreXMLData';
var
  LRootNode    : IXMLNode;
  LSectionNode : IXMLNode;
begin
  Result := FALSE;
  try
    LRootNode := FXMLDocumentOut.DocumentElement;
    LSectionNode := LRootNode.ChildNodes['ChannelProperties'];
    LRootNode.ChildNodes['Identifier'].Text := FIdentifier;
    Result := FControlIterator.StoreXMLData(LRootNode);
    if (ChbActive.Checked) then
      LSectionNode.ChildNodes['Active'].Text := 'Y'
    else
      LSectionNode.ChildNodes['Active'].Text := 'N';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TChannelPropertiesDlg.ControlExit(Sender: TObject);
const OPNAME = 'TChannelPropertiesDlg.ControlExit';
var
  LControl : TWRMFControl;
begin
  try
    LControl := TWRMFControl(TControl(Sender).Parent);
    LControl.IsValid := TRUE;
    DoValidation(tcChange, LControl.PropertyName);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TChannelPropertiesDlg.BtnApplyClick(Sender: TObject);
const OPNAME = 'TChannelPropertiesDlg.BtnApplyClick';
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

procedure TChannelPropertiesDlg.BtnResetClick(Sender: TObject);
const OPNAME = 'TChannelPropertiesDlg.BtnResetClick';
begin
  try
    LoadXMLData;
    SetAllValid;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TChannelPropertiesDlg.DoValidation (AContext      : TValidationContext;
                                             APropertyName : String) : Boolean;
const OPNAME = 'TChannelPropertiesDlg.DoValidation';
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

procedure TChannelPropertiesDlg.ParamChangeIndicatorClicked (Sender: TObject);
const OPNAME = 'TChannelPropertiesDlg.ParamChangeIndicatorClicked';
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

procedure TChannelPropertiesDlg.MetaDataIndicatorClicked (Sender: TObject);
const OPNAME = 'TChannelPropertiesDlg.MetaDataIndicatorClicked';
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
