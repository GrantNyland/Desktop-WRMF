unit UMineUndergroundPropertiesDlg;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, VCL.Graphics, VCL.Controls, VCL.Forms,
  VCL.Dialogs, VCL.StdCtrls, xmldom, XMLIntf, XMLDoc, VCL.ExtCtrls, VCL.Buttons, VCL.Menus,
  VCL.ComCtrls,

  HydrologyCom_TLB,
  UControlIterator,
  UXMLAgent,
  UXMLAgentMineUndergroundProperties,
  UWRMFEdit, UWRMFComboBox, UWRMFControl, UWRMFCheckBox;

type
  TMineUndergroundPropertiesDlg = class(TForm)
    FXMLDocumentOut                      : TXMLDocument;
    FXMLDocumentIn                       : TXMLDocument;
    PnlBottom                            : TPanel;
    BtnApply                             : TButton;
    BtnReset                             : TButton;
    ScrClient                            : TScrollBox;
    LblSectionName                       : TLabel;
    LblBoardAndPillarArea                : TLabel;
    LblHighExtractionArea                : TLabel;
    LblUndergroundOutflowRoute           : TLabel;
    LblUpstreamCatchmentArea             : TLabel;
    LblSurfaceRunOffFactor               : TLabel;
    EdtUndergroundSectionName            : TWRMFEdit;
    EdtBoardAndPillarArea                : TWRMFEdit;
    EdtHighExtractionArea                : TWRMFEdit;
    EdtUpstreamCatchmentArea             : TWRMFEdit;
    EdtSurfaceRunOffFactor               : TWRMFEdit;
    CbxUndergroundOutflowRouteNo         : TWRMFComboBox;
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
    FXMLAgent         : TXMLAgentMineUndergroundProperties;
    FHydrologyModel   : IHydrologyModel;
  end;

implementation

uses
  UErrorHandlingOperations;

{$R *.dfm}

procedure TMineUndergroundPropertiesDlg.FormCreate(Sender: TObject);
const OPNAME = 'TMineUndergroundPropertiesDlg.FormCreate';
begin
  try
    FErrorList := TStringList.Create;
    FControlIterator := TControlIterator.Create;
    FControlIterator.ControlParent := ScrClient;
    FXMLAgent := TXMLAgentMineUndergroundProperties.Create;
    FXMLAgent.DlgXMLDocumentIn  := FXMLDocumentIn;
    FXMLAgent.DlgXMLDocumentOut := FXMLDocumentOut;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMineUndergroundPropertiesDlg.FormDestroy(Sender: TObject);
const OPNAME = 'TMineUndergroundPropertiesDlg.FormDestroy';
begin
  try
    FreeAndNil(FErrorList);
    FreeAndNil(FControlIterator);
    FreeAndNil(FXMLAgent);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMineUndergroundPropertiesDlg.FormShow(Sender: TObject);
const OPNAME = 'TMineUndergroundPropertiesDlg.FormShow';
begin
  try
    LoadXMLData;
    SetControls;
    SetIndicators;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMineUndergroundPropertiesDlg.SetIndicators;
const OPNAME = 'TMineUndergroundPropertiesDlg.SetIndicators';
begin
  try
    FControlIterator.SetIndicators;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMineUndergroundPropertiesDlg.SetAllValid;
const OPNAME = 'TMineUndergroundPropertiesDlg.SetAllValid';
begin
  try
    FControlIterator.SetAllValid;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMineUndergroundPropertiesDlg.SetControls;
const OPNAME = 'TMineUndergroundPropertiesDlg.SetControls';
begin
  try
    FControlIterator.SetControls(FMayChangeNetwork);
    BtnApply.Enabled := FMayChangeNetwork;
    BtnReset.Enabled := FMayChangeNetwork;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMineUndergroundPropertiesDlg.LoadXMLData: boolean;
const OPNAME = 'TMineUndergroundPropertiesDlg.LoadXMLData';
var
  LRootNode              : IXMLNode;
  LAllOutflowRoutesNode  : IXMLNode;
begin
  Result := FALSE;
  try
    FXMLDocumentIn.Active  := TRUE;
    FXMLDocumentOut.Active := TRUE;
    LRootNode              := FXMLDocumentIn.DocumentElement;
    LAllOutflowRoutesNode  := LRootNode.ChildNodes['OutflowRoutes'];

    FIdentifier := LRootNode.ChildNodes['Identifier'].Text;
    FKeyValues  := 'Model='       + QuotedStr(LRootNode.ChildNodes['Model'].Text) +
                   ',NetworkID='  + QuotedStr(LRootNode.ChildNodes['NetworkID'].Text) +
                   ',ModuleType=' + QuotedStr(LRootNode.ChildNodes['ModuleType'].Text) +
                   ',Identifier=' + LRootNode.ChildNodes['Identifier'].Text;

    // UndergroundOutflowRoute.
    FXMLAgent.PopulateCbxAllNetworkRoutes(CbxUndergroundOutflowRouteNo, LAllOutflowRoutesNode, FALSE);

    Result := FControlIterator.LoadXMLData(LRootNode);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMineUndergroundPropertiesDlg.StoreXMLData : Boolean;
const OPNAME = 'TMineUndergroundPropertiesDlg.StoreXMLData';
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

procedure TMineUndergroundPropertiesDlg.ControlExit(Sender: TObject);
const OPNAME = 'TMineUndergroundPropertiesDlg.ControlExit';
var
  LControl : TWRMFControl;
begin
  try
    LControl := TWRMFControl(TControl(Sender).Parent);
    LControl.IsValid := TRUE;
    DoValidation(tcChange, LControl.PropertyName);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMineUndergroundPropertiesDlg.BtnApplyClick(Sender: TObject);
const OPNAME = 'TMineUndergroundPropertiesDlg.BtnApplyClick';
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

procedure TMineUndergroundPropertiesDlg.BtnResetClick(Sender: TObject);
const OPNAME = 'TMineUndergroundPropertiesDlg.BtnResetClick';
begin
  try
    LoadXMLData;
    SetAllValid;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMineUndergroundPropertiesDlg.DoValidation (AContext      : TValidationContext;
                                                     APropertyName : String) : Boolean;
const OPNAME = 'TMineUndergroundPropertiesDlg.DoValidation';
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

procedure TMineUndergroundPropertiesDlg.ParamChangeIndicatorClicked (Sender: TObject);
const OPNAME = 'TMineUndergroundPropertiesDlg.ParamChangeIndicatorClicked';
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

procedure TMineUndergroundPropertiesDlg.MetaDataIndicatorClicked (Sender: TObject);
const OPNAME = 'TMineUndergroundPropertiesDlg.MetaDataIndicatorClicked';
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

