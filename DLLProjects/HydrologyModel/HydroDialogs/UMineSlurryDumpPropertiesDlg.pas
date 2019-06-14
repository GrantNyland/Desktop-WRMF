unit UMineSlurryDumpPropertiesDlg;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, VCL.Graphics, VCL.Controls, VCL.Forms,
  VCL.Dialogs, VCL.StdCtrls, xmldom, XMLIntf, XMLDoc, VCL.ExtCtrls, VCL.Buttons, VCL.Menus,
  VCL.ComCtrls,

  HydrologyCom_TLB,
  UControlIterator,
  UXMLAgent,
  UXMLAgentMineSlurryDumpProperties,
  UWRMFEdit, UWRMFComboBox, UWRMFControl, UWRMFCheckBox;

type
  TMineSlurryDumpPropertiesDlg = class(TForm)
    FXMLDocumentOut                      : TXMLDocument;
    FXMLDocumentIn                       : TXMLDocument;
    PnlBottom                            : TPanel;
    BtnApply                             : TButton;
    BtnReset                             : TButton;
    ScrClient                            : TScrollBox;
    LblSectionName                       : TLabel;
    LblSlurrySeepProportion              : TLabel;
    LblSlurryPCDFullSupplyVolume         : TLabel;
    LblSlurryDumpArea                    : TLabel;
    LblSlurryDumpRunOffFactor            : TLabel;
    LblSlurryPCDFullSupplyArea           : TLabel;
    LblSlurryPCDInitialVolume            : TLabel;
    EdtSlurryDumpSectionName             : TWRMFEdit;
    EdtSlurrySeepProportion              : TWRMFEdit;
    EdtSlurryPCDFullSupplyVolume         : TWRMFEdit;
    EdtSlurryDumpRunOffFactor            : TWRMFEdit;
    EdtSlurryPCDFullSupplyArea           : TWRMFEdit;
    EdtSlurryDumpArea                    : TWRMFEdit;
    EdtSlurryPCDInitialVolume            : TWRMFEdit;
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
    FXMLAgent         : TXMLAgentMineSlurryDumpProperties;
    FHydrologyModel   : IHydrologyModel;
  end;

implementation

uses
  UErrorHandlingOperations;

{$R *.dfm}

procedure TMineSlurryDumpPropertiesDlg.FormCreate(Sender: TObject);
const OPNAME = 'TMineSlurryDumpPropertiesDlg.FormCreate';
begin
  try
    FErrorList := TStringList.Create;
    FControlIterator := TControlIterator.Create;
    FControlIterator.ControlParent := ScrClient;
    FXMLAgent := TXMLAgentMineSlurryDumpProperties.Create;
    FXMLAgent.DlgXMLDocumentIn  := FXMLDocumentIn;
    FXMLAgent.DlgXMLDocumentOut := FXMLDocumentOut;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMineSlurryDumpPropertiesDlg.FormDestroy(Sender: TObject);
const OPNAME = 'TMineSlurryDumpPropertiesDlg.FormDestroy';
begin
  try
    FreeAndNil(FErrorList);
    FreeAndNil(FControlIterator);
    FreeAndNil(FXMLAgent);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMineSlurryDumpPropertiesDlg.FormShow(Sender: TObject);
const OPNAME = 'TMineSlurryDumpPropertiesDlg.FormShow';
begin
  try
    LoadXMLData;
    SetControls;
    SetIndicators;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMineSlurryDumpPropertiesDlg.SetIndicators;
const OPNAME = 'TMineSlurryDumpPropertiesDlg.SetIndicators';
begin
  try
    FControlIterator.SetIndicators;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMineSlurryDumpPropertiesDlg.SetControls;
const OPNAME = 'TMineSlurryDumpPropertiesDlg.SetControls';
begin
  try
    FControlIterator.SetControls(FMayChangeNetwork);
    BtnApply.Enabled := FMayChangeNetwork;
    BtnReset.Enabled := FMayChangeNetwork;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMineSlurryDumpPropertiesDlg.SetAllValid;
const OPNAME = 'TMineSlurryDumpPropertiesDlg.SetAllValid';
begin
  try
    FControlIterator.SetAllValid;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMineSlurryDumpPropertiesDlg.LoadXMLData: boolean;
const OPNAME = 'TMineSlurryDumpPropertiesDlg.LoadXMLData';
var
  LRootNode : IXMLNode;
begin
  Result := FALSE;
  try
    FXMLDocumentIn.Active  := TRUE;
    FXMLDocumentOut.Active := TRUE;
    LRootNode              := FXMLDocumentIn.DocumentElement;

    FIdentifier := LRootNode.ChildNodes['Identifier'].Text;
    FKeyValues  := 'Model='       + QuotedStr(LRootNode.ChildNodes['Model'].Text) +
                   ',NetworkID='  + QuotedStr(LRootNode.ChildNodes['NetworkID'].Text) +
                   ',ModuleType=' + QuotedStr(LRootNode.ChildNodes['ModuleType'].Text) +
                   ',Identifier=' + LRootNode.ChildNodes['Identifier'].Text;

    Result := FControlIterator.LoadXMLData(LRootNode);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMineSlurryDumpPropertiesDlg.StoreXMLData : Boolean;
const OPNAME = 'TMineSlurryDumpPropertiesDlg.StoreXMLData';
var
  LRootNode : IXMLNode;
begin
  Result := FALSE;
  try
    LRootNode := FXMLDocumentOut.DocumentElement;
    LRootNode.ChildNodes['Identifier'].Text := FIdentifier;
    Result := FControlIterator.StoreXMLData(LRootNode);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMineSlurryDumpPropertiesDlg.ControlExit(Sender: TObject);
const OPNAME = 'TMineSlurryDumpPropertiesDlg.ControlExit';
var
  LControl : TWRMFControl;
begin
  try
    LControl := TWRMFControl(TControl(Sender).Parent);
    LControl.IsValid := TRUE;
    DoValidation(tcChange, LControl.PropertyName);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMineSlurryDumpPropertiesDlg.BtnApplyClick(Sender: TObject);
const OPNAME = 'TMineSlurryDumpPropertiesDlg.BtnApplyClick';
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

procedure TMineSlurryDumpPropertiesDlg.BtnResetClick(Sender: TObject);
const OPNAME = 'TMineSlurryDumpPropertiesDlg.BtnResetClick';
begin
  try
    LoadXMLData;
    SetAllValid;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMineSlurryDumpPropertiesDlg.DoValidation (AContext      : TValidationContext;
                                                    APropertyName : String) : Boolean;
const OPNAME = 'TMineSlurryDumpPropertiesDlg.DoValidation';
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

procedure TMineSlurryDumpPropertiesDlg.ParamChangeIndicatorClicked (Sender: TObject);
const OPNAME = 'TMineSlurryDumpPropertiesDlg.ParamChangeIndicatorClicked';
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

procedure TMineSlurryDumpPropertiesDlg.MetaDataIndicatorClicked (Sender: TObject);
const OPNAME = 'TMineSlurryDumpPropertiesDlg.MetaDataIndicatorClicked';
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

