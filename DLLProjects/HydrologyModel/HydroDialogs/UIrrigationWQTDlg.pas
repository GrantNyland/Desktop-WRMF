unit UIrrigationWQTDlg;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, VCL.Graphics, VCL.Controls, VCL.Forms,
  VCL.Dialogs, VCL.StdCtrls, xmldom, XMLIntf, XMLDoc, VCL.ExtCtrls, VCL.Buttons, VCL.Menus,

  HydrologyCom_TLB,
  UControlIterator,
  UXMLAgent,
  UXMLAgentIrrigationWQT,
  UWRMFEdit, UWRMFComboBox, UWRMFControl, UWRMFCheckBox, VCL.ComCtrls,
  VCL.Grids, UCellChangeStringGrid, UWRMFGrid, Xml.Win.msxmldom;

type
  TIrrigationWQTDlg = class(TForm)
    PnlBottom                               : TPanel;
    BtnApply                                : TButton;
    BtnReset                                : TButton;
    FXMLDocumentIn                          : TXMLDocument;
    FXMLDocumentOut                         : TXMLDocument;
    ScrClient                               : TScrollBox;
    LblMaxWaterAllocation                   : TLabel;
    LblWaterAllocationInterpolationType     : TLabel;
    LblRunOffModule                         : TLabel;
    LblTransferCanalSeepage                 : TLabel;
    LblTransferCanalFlowLossProportion      : TLabel;
    LblTransferCanalSaltLossProportion      : TLabel;
    LblIrrigationEfficiencyFactor           : TLabel;
    LblReturnFlowFactor                     : TLabel;
    LblUpperZoneReturnFlowProportion        : TLabel;
    LblLowerZoneReturnFlowProportion        : TLabel;
    LblSaltConcentrationFactor              : TLabel;
    LblLandSaltLossProportion               : TLabel;
    LblSaltLoad1                            : TLabel;
    LblSaltLoad2                            : TLabel;
    LblInitialSaltLoadUpperZone             : TLabel;
    LblInitialSaltLoadLowerZone             : TLabel;
    LblSoilMoistureStorageCapacityUpperZone : TLabel;
    LblSoilMoistureStorageCapacityLowerZone : TLabel;
    LblTargetSoilMoisture                   : TLabel;
    LblInitialSoilMoisture                  : TLabel;
    LblEffectiveRainfallFactor1             : TLabel;
    LblEffectiveRainfallFactor2             : TLabel;
    ChbProduceNetReturnFlows                : TWRMFCheckBox;
    EdtIrrigationEfficiencyFactor           : TWRMFEdit;
    EdtTransferCanalSaltLossProportion      : TWRMFEdit;
    EdtTransferCanalFlowLossProportion      : TWRMFEdit;
    EdtTransferCanalSeepage                 : TWRMFEdit;
    CbxRunOffModule                         : TWRMFComboBox;
    CbxWaterAllocationInterpolationType     : TWRMFComboBox;
    EdtMaxWaterAllocation                   : TWRMFEdit;
    EdtReturnFlowFactor                     : TWRMFEdit;
    EdtUpperZoneReturnFlowProportion        : TWRMFEdit;
    EdtLowerZoneReturnFlowProportion        : TWRMFEdit;
    EdtSaltConcentrationFactor              : TWRMFEdit;
    EdtLandSaltLossProportion               : TWRMFEdit;
    EdtSaltLoad1                            : TWRMFEdit;
    EdtSaltLoad2                            : TWRMFEdit;
    EdtInitialSaltLoadLowerZone             : TWRMFEdit;
    EdtInitialSaltLoadUpperZone             : TWRMFEdit;
    EdtSoilMoistureStorageCapacityLowerZone : TWRMFEdit;
    EdtSoilMoistureStorageCapacityUpperZone : TWRMFEdit;
    EdtInitialSoilMoisture                  : TWRMFEdit;
    EdtTargetSoilMoisture                   : TWRMFEdit;
    EdtEffectiveRainfallFactor1             : TWRMFEdit;
    EdtEffectiveRainfallFactor2             : TWRMFEdit;
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
    FXMLAgent         : TXMLAgentIrrigationWQT;
    FHydrologyModel   : IHydrologyModel;
  end;

implementation

uses
  UErrorHandlingOperations;

{$R *.dfm}

procedure TIrrigationWQTDlg.FormCreate(Sender: TObject);
const OPNAME = 'TIrrigationWQTDlg.FormCreate';
begin
  try
    FErrorList := TStringList.Create;
    FControlIterator := TControlIterator.Create;
    FControlIterator.ControlParent := ScrClient;
    FXMLAgent := TXMLAgentIrrigationWQT.Create;
    FXMLAgent.DlgXMLDocumentIn  := FXMLDocumentIn;
    FXMLAgent.DlgXMLDocumentOut := FXMLDocumentOut;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrigationWQTDlg.FormDestroy(Sender: TObject);
const OPNAME = 'TIrrigationWQTDlg.FormDestroy';
begin
  try
    FreeAndNil(FErrorList);
    FreeAndNil(FControlIterator);
    FreeAndNil(FXMLAgent);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrigationWQTDlg.FormShow(Sender: TObject);
const OPNAME = 'TIrrigationWQTDlg.FormShow';
begin
  try
    LoadXMLData;
    SetControls;
    SetIndicators;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrigationWQTDlg.SetControls;
const OPNAME = 'TIrrigationWQTDlg.SetControls';
begin
  try
    FControlIterator.SetControls(FMayChangeNetwork);
    BtnApply.Enabled := FMayChangeNetwork;
    BtnReset.Enabled := FMayChangeNetwork;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrigationWQTDlg.SetIndicators;
const OPNAME = 'TIrrigationWQTDlg.SetIndicators';
begin
  try
    FControlIterator.SetIndicators;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrigationWQTDlg.SetAllValid;
const OPNAME = 'TIrrigationWQTDlg.SetAllValid';
begin
  try
    FControlIterator.SetAllValid;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TIrrigationWQTDlg.LoadXMLData: boolean;
const OPNAME = 'TIrrigationWQTDlg.LoadXMLData';
var
  LRootNode              : IXMLNode;
  LAllRunOffModulesNode  : IXMLNode;
begin
  Result := FALSE;
  try
    FXMLDocumentIn.Active := TRUE;
    FXMLDocumentOut.Active := TRUE;
    LRootNode             := FXMLDocumentIn.DocumentElement;
    LAllRunOffModulesNode := LRootNode.ChildNodes['AllRunOffModules'];

    FIdentifier := LRootNode.ChildNodes['Identifier'].Text;
    FKeyValues  := 'Model='       + QuotedStr(LRootNode.ChildNodes['Model'].Text) +
                   ',NetworkID='  + QuotedStr(LRootNode.ChildNodes['NetworkID'].Text) +
                   ',ModuleType=' + QuotedStr(LRootNode.ChildNodes['ModuleType'].Text) +
                   ',Identifier=' + LRootNode.ChildNodes['Identifier'].Text;

   // WaterAllocationInterpolationType
    CbxWaterAllocationInterpolationType.Items.Clear;
    CbxWaterAllocationInterpolationType.Items.AddObject('Linear', pointer(1));
    CbxWaterAllocationInterpolationType.Items.AddObject('Exponential', pointer(2));
    CbxWaterAllocationInterpolationType.Items.AddObject('Defined', pointer(3));
    // RunOffModule
    FXMLAgent.PopulateCbxAllNetworkModules(CbxRunOffModule, LAllRunOffModulesNode, FALSE);

    Result := FControlIterator.LoadXMLData(LRootNode);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TIrrigationWQTDlg.StoreXMLData : Boolean;
const OPNAME = 'TIrrigationWQTDlg.StoreXMLData';
var
  LRootNode    : IXMLNode;
  LSectionNode : IXMLNode;
begin
  Result := FALSE;
  try
    LRootNode := FXMLDocumentOut.DocumentElement;
    LSectionNode := LRootNode.ChildNodes['IrrigationWQT'];
    LRootNode.ChildNodes['Identifier'].Text := FIdentifier;
    Result := FControlIterator.StoreXMLData(LRootNode);
    if (ChbProduceNetReturnFlows.Checked) then
      LSectionNode.ChildNodes['ProduceNetReturnFlows'].Text := '1'
    else
      LSectionNode.ChildNodes['ProduceNetReturnFlows'].Text := '0';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrigationWQTDlg.ControlExit(Sender: TObject);
const OPNAME = 'TIrrigationWQTDlg.ControlExit';
var
  LControl : TWRMFControl;
begin
  try
    LControl := TWRMFControl(TControl(Sender).Parent);
    LControl.IsValid := TRUE;
    DoValidation(tcChange, LControl.PropertyName);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrigationWQTDlg.BtnApplyClick(Sender: TObject);
const OPNAME = 'TIrrigationWQTDlg.BtnApplyClick';
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

procedure TIrrigationWQTDlg.BtnResetClick(Sender: TObject);
const OPNAME = 'TIrrigationWQTDlg.BtnResetClick';
begin
  try
    LoadXMLData;
    SetAllValid;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TIrrigationWQTDlg.DoValidation (AContext      : TValidationContext;
                                         APropertyName : String) : Boolean;
const OPNAME = 'TIrrigationWQTDlg.DoValidation';
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

procedure TIrrigationWQTDlg.ParamChangeIndicatorClicked (Sender: TObject);
const OPNAME = 'TIrrigationWQTDlg.ParamChangeIndicatorClicked';
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

procedure TIrrigationWQTDlg.MetaDataIndicatorClicked (Sender: TObject);
const OPNAME = 'TIrrigationWQTDlg.MetaDataIndicatorClicked';
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
