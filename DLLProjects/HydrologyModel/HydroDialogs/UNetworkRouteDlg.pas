unit UNetworkRouteDlg;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, VCL.Graphics, VCL.Controls, VCL.Forms,
  VCL.Dialogs, VCL.StdCtrls, xmldom, XMLIntf, XMLDoc, VCL.ExtCtrls, VCL.Buttons, VCL.Menus,
  VCL.ComCtrls,

  HydrologyCom_TLB,
  UControlIterator,
  UXMLAgent,
  UXMLAgentNetworkRoute,
  UWRMFEdit, UWRMFComboBox, UWRMFControl, UWRMFCheckBox;

type
  TNetworkRouteDlg = class(TForm)
    FXMLDocumentOut : TXMLDocument;
    FXMLDocumentIn  : TXMLDocument;
    PnlBottom       : TPanel;
    BtnApply        : TButton;
    BtnReset        : TButton;
    ScrClient       : TScrollBox;
    LblRouteNo      : TLabel;
    LblSourceModule : TLabel;
    LblSinkModule   : TLabel;
    CbxSinkModule   : TWRMFComboBox;
    CbxSourceModule : TWRMFComboBox;
    EdtRouteNo      : TWRMFEdit;
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
    FXMLAgent         : TXMLAgentNetworkRoute;
    FHydrologyModel   : IHydrologyModel;
  end;

implementation

uses
  UErrorHandlingOperations;

{$R *.dfm}

procedure TNetworkRouteDlg.FormCreate(Sender: TObject);
const OPNAME = 'TNetworkRouteDlg.FormCreate';
begin
  try
    FErrorList := TStringList.Create;
    FControlIterator := TControlIterator.Create;
    FControlIterator.ControlParent := ScrClient;
    FXMLAgent := TXMLAgentNetworkRoute.Create;
    FXMLAgent.DlgXMLDocumentIn  := FXMLDocumentIn;
    FXMLAgent.DlgXMLDocumentOut := FXMLDocumentOut;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TNetworkRouteDlg.FormDestroy(Sender: TObject);
const OPNAME = 'TNetworkRouteDlg.FormDestroy';
begin
  try
    FreeAndNil(FErrorList);
    FreeAndNil(FControlIterator);
    FreeAndNil(FXMLAgent);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TNetworkRouteDlg.BtnApplyClick(Sender: TObject);
const OPNAME = 'TNetworkRouteDlg.BtnApplyClick';
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

procedure TNetworkRouteDlg.FormShow(Sender: TObject);
const OPNAME = 'TNetworkRouteDlg.FormShow';
begin
  try
    LoadXMLData;
    SetControls;
    SetIndicators;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TNetworkRouteDlg.SetIndicators;
const OPNAME = 'TNetworkRouteDlg.SetIndicators';
begin
  try
    FControlIterator.SetIndicators;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TNetworkRouteDlg.SetControls;
const OPNAME = 'TNetworkRouteDlg.SetControls';
begin
  try
    FControlIterator.SetControls(FMayChangeNetwork);
    EdtRouteNo.Active      := FALSE;
    CbxSourceModule.Active := FALSE;
    CbxSinkModule.Active   := FALSE;
    BtnApply.Enabled       := FMayChangeNetwork;
    BtnReset.Enabled       := FMayChangeNetwork;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TNetworkRouteDlg.SetAllValid;
const OPNAME = 'TNetworkRouteDlg.SetAllValid';
begin
  try
    FControlIterator.SetAllValid;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TNetworkRouteDlg.LoadXMLData: boolean;
const OPNAME = 'TNetworkRouteDlg.LoadXMLData';
var
  LRootNode               : IXMLNode;
  LAllNetworkModulesNode  : IXMLNode;
begin
  Result := FALSE;
  try
    FXMLDocumentIn.Active  := TRUE;
    FXMLDocumentOut.Active := TRUE;
    LRootNode              := FXMLDocumentIn.DocumentElement;
    LAllNetworkModulesNode := LRootNode.ChildNodes['AllNetworkModules'];

    FIdentifier := LRootNode.ChildNodes['Identifier'].Text;
    FKeyValues  := 'Model='       + QuotedStr(LRootNode.ChildNodes['Model'].Text) +
                   ',NetworkID='  + QuotedStr(LRootNode.ChildNodes['NetworkID'].Text) +
                   ',ModuleType=' + QuotedStr(LRootNode.ChildNodes['ModuleType'].Text) +
                   ',Identifier=' + LRootNode.ChildNodes['Identifier'].Text;

    // Populate upstream and downstream node number comboboxes.
    FXMLAgent.PopulateCbxAllNetworkModulesID(CbxSourceModule, LAllNetworkModulesNode, FALSE);
    FXMLAgent.PopulateCbxAllNetworkModulesID(CbxSinkModule, LAllNetworkModulesNode, FALSE);

    Result := FControlIterator.LoadXMLData(LRootNode);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TNetworkRouteDlg.StoreXMLData : Boolean;
const OPNAME = 'TNetworkRouteDlg.StoreXMLData';
var
  LRootNode    : IXMLNode;
begin
  Result := FALSE;
  try
    LRootNode := FXMLDocumentOut.DocumentElement;
    LRootNode.ChildNodes['Identifier'].Text := FIdentifier;
    Result := FControlIterator.StoreXMLData(LRootNode);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TNetworkRouteDlg.ControlExit(Sender: TObject);
const OPNAME = 'TNetworkRouteDlg.ControlExit';
var
  LControl : TWRMFControl;
begin
  try
    LControl := TWRMFControl(TControl(Sender).Parent);
    LControl.IsValid := TRUE;
    DoValidation(tcChange, LControl.PropertyName);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TNetworkRouteDlg.BtnResetClick(Sender: TObject);
const OPNAME = 'TNetworkRouteDlg.BtnResetClick';
begin
  try
    LoadXMLData;
    SetAllValid;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TNetworkRouteDlg.DoValidation (AContext      : TValidationContext;
                                        APropertyName : String) : Boolean;
const OPNAME = 'TNetworkRouteDlg.DoValidation';
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

procedure TNetworkRouteDlg.ParamChangeIndicatorClicked (Sender: TObject);
const OPNAME = 'TNetworkRouteDlg.ParamChangeIndicatorClicked';
//var
//  LControl : TWRMFControl;
begin
  try
{
    if (Sender is TWRMFControl) then
    begin
      LControl := TWRMFControl(Sender);
      if (LControl.HasParamChange) then
        ShowMessage ('SHOW Param Changes')
      else
        ShowMessage ('NEW Param Changes');
      end;
}
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TNetworkRouteDlg.MetaDataIndicatorClicked (Sender: TObject);
const OPNAME = 'TNetworkRouteDlg.MetaDataIndicatorClicked';
begin
  try
{
    if ((Sender is TWRMFControl) AND (TWRMFControl(Sender).HasMetaData)) then
      ShowMessage ('SHOW Meta Data')
    else
      ShowMessage ('NEW Meta Data');
}
  except on E: Exception do HandleError(E, OPNAME) end;
end;


end.

