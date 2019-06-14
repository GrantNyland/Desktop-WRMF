unit UObservationPointDlg;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, VCL.Graphics, VCL.Controls, VCL.Forms,
  VCL.Dialogs, VCL.StdCtrls, xmldom, XMLIntf, XMLDoc, VCL.ExtCtrls, VCL.Buttons, VCL.Menus,
  VCL.ComCtrls,

  HydrologyCom_TLB,
  UControlIterator,
  UXMLAgent,
  UXMLAgentObservationPoint,
  UWRMFEdit, UWRMFComboBox, UWRMFControl, UWRMFCheckBox;

type
  TObservationPointDlg = class(TForm)
    FXMLDocumentOut      : TXMLDocument;
    FXMLDocumentIn       : TXMLDocument;
    PnlBottom            : TPanel;
    BtnApply             : TButton;
    BtnReset             : TButton;
    ScrClient            : TScrollBox;
    LblRouteNo           : TLabel;
    LblName              : TLabel;
    LblFlowdataFileName  : TLabel;
    CbxRouteNo           : TWRMFComboBox;
    EdtFlowDataFileName  : TWRMFEdit;
    EdtName              : TWRMFEdit;
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
    FXMLAgent         : TXMLAgentObservationPoint;
    FHydrologyModel   : IHydrologyModel;
  end;

implementation

uses
  UErrorHandlingOperations;

{$R *.dfm}

procedure TObservationPointDlg.FormCreate(Sender: TObject);
const OPNAME = 'TObservationPointDlg.FormCreate';
begin
  try
    FErrorList := TStringList.Create;
    FControlIterator := TControlIterator.Create;
    FControlIterator.ControlParent := ScrClient;
    FXMLAgent := TXMLAgentObservationPoint.Create;
    FXMLAgent.DlgXMLDocumentIn  := FXMLDocumentIn;
    FXMLAgent.DlgXMLDocumentOut := FXMLDocumentOut;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TObservationPointDlg.FormDestroy(Sender: TObject);
const OPNAME = 'TObservationPointDlg.FormDestroy';
begin
  try
    FreeAndNil(FErrorList);
    FreeAndNil(FControlIterator);
    FreeAndNil(FXMLAgent);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TObservationPointDlg.BtnApplyClick(Sender: TObject);
const OPNAME = 'TObservationPointDlg.BtnApplyClick';
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

procedure TObservationPointDlg.FormShow(Sender: TObject);
const OPNAME = 'TObservationPointDlg.FormShow';
begin
  try
    LoadXMLData;
    SetControls;
    SetIndicators;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TObservationPointDlg.SetIndicators;
const OPNAME = 'TObservationPointDlg.SetIndicators';
begin
  try
    FControlIterator.SetIndicators;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TObservationPointDlg.SetControls;
const OPNAME = 'TObservationPointDlg.SetControls';
begin
  try
    FControlIterator.SetControls(FMayChangeNetwork);
    CbxRouteNo.Active  := FALSE;
    BtnApply.Enabled   := FMayChangeNetwork;
    BtnReset.Enabled   := FMayChangeNetwork;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TObservationPointDlg.SetAllValid;
const OPNAME = 'TObservationPointDlg.SetAllValid';
begin
  try
    FControlIterator.SetAllValid;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TObservationPointDlg.LoadXMLData: boolean;
const OPNAME = 'TObservationPointDlg.LoadXMLData';
var
  LRootNode          : IXMLNode;
  LAllNetworkRoutes  : IXMLNode;
begin
  Result := FALSE;
  try
    FXMLDocumentIn.Active  := TRUE;
    FXMLDocumentOut.Active := TRUE;
    LRootNode              := FXMLDocumentIn.DocumentElement;
    LAllNetworkRoutes      := LRootNode.ChildNodes['AllNetworkRoutes'];

    FIdentifier := LRootNode.ChildNodes['Identifier'].Text;
    FKeyValues  := 'Model='       + QuotedStr(LRootNode.ChildNodes['Model'].Text) +
                   ',NetworkID='  + QuotedStr(LRootNode.ChildNodes['NetworkID'].Text) +
                   ',ModuleType=' + QuotedStr(LRootNode.ChildNodes['ModuleType'].Text) +
                   ',Identifier=' + LRootNode.ChildNodes['Identifier'].Text;

    // Populate Route number combobox.
    FXMLAgent.PopulateCbxAllNetworkRoutes(CbxRouteNo, LAllNetworkRoutes, FALSE);

    Result := FControlIterator.LoadXMLData(LRootNode);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TObservationPointDlg.StoreXMLData : Boolean;
const OPNAME = 'TObservationPointDlg.StoreXMLData';
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

procedure TObservationPointDlg.ControlExit(Sender: TObject);
const OPNAME = 'TObservationPointDlg.ControlExit';
var
  LControl : TWRMFControl;
begin
  try
    LControl := TWRMFControl(TControl(Sender).Parent);
    LControl.IsValid := TRUE;
    DoValidation(tcChange, LControl.PropertyName);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TObservationPointDlg.BtnResetClick(Sender: TObject);
const OPNAME = 'TObservationPointDlg.BtnResetClick';
begin
  try
    LoadXMLData;
    SetAllValid;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TObservationPointDlg.DoValidation (AContext      : TValidationContext;
                                        APropertyName : String) : Boolean;
const OPNAME = 'TObservationPointDlg.DoValidation';
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

procedure TObservationPointDlg.ParamChangeIndicatorClicked (Sender: TObject);
const OPNAME = 'TObservationPointDlg.ParamChangeIndicatorClicked';
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

procedure TObservationPointDlg.MetaDataIndicatorClicked (Sender: TObject);
const OPNAME = 'TObservationPointDlg.MetaDataIndicatorClicked';
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

