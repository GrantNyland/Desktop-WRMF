unit UMinePropertiesDlg;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, VCL.Graphics, VCL.Controls, VCL.Forms,
  VCL.Dialogs, VCL.StdCtrls, xmldom, XMLIntf, XMLDoc, VCL.ExtCtrls, VCL.Buttons, VCL.Menus,

  UHostDlg,
  HydrologyCom_TLB,
  UControlIterator,
  UXMLAgent,
  UXMLAgentMineProperties,
  UWRMFEdit, UWRMFComboBox, UWRMFControl, UWRMFCheckBox, VCL.ComCtrls,
  VCL.Grids, UCellChangeStringGrid, UWRMFGrid, Xml.Win.msxmldom;

type
  TMinePropertiesDlg = class(TForm)
    FXMLDocumentIn           : TXMLDocument;
    FXMLDocumentOut          : TXMLDocument;
    PnlBottom                : TPanel;
    BtnReset                 : TButton;
    BtnApply                 : TButton;
    ScrClient                : TScrollBox;
    LblNetworkSequence       : TLabel;
    LblModuleNumber          : TLabel;
    LblLatitude              : TLabel;
    LblLongitude             : TLabel;
    LblMineName              : TLabel;
    LblMAP                   : TLabel;
    LblRainfallFileName      : TLabel;
    LblRunOffModuleNo        : TLabel;
    LblOutflowRouteNoToRiver : TLabel;
    LblPlantArea             : TLabel;
    LblVersionNo             : TLabel;
    LblOutflowRouteNoToPCD   : TLabel;
    LblPlantAreaRunOffFactor : TLabel;
    LblSaltBuildUpRate       : TLabel;
    LblSaltWashOffFactor     : TLabel;
    LblInitialSaltStore      : TLabel;
    EdtNetworkSequence       : TWRMFEdit;
    EdtModuleNumber          : TWRMFEdit;
    ChbActive                : TWRMFCheckBox;
    EdtPlantArea             : TWRMFEdit;
    CbxOutflowRouteNoToRiver : TWRMFComboBox;
    EdtRainfallFileName      : TWRMFEdit;
    EdtMAP                   : TWRMFEdit;
    EdtMineName              : TWRMFEdit;
    EdtLongitude             : TWRMFEdit;
    EdtLatitude              : TWRMFEdit;
    EdtVersionNo             : TWRMFEdit;
    CbxRunOffModuleNo        : TWRMFComboBox;
    CbxOutflowRouteNoToPCD   : TWRMFComboBox;
    EdtPlantAreaRunOffFactor : TWRMFEdit;
    EdtSaltBuildUpRate       : TWRMFEdit;
    EdtSaltWashOffFactor     : TWRMFEdit;
    EdtInitialSaltStore      : TWRMFEdit;
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
    FXMLAgent         : TXMLAgentMineProperties;
    FHydrologyModel   : IHydrologyModel;
  end;

implementation

uses
  UErrorHandlingOperations;

{$R *.dfm}

procedure TMinePropertiesDlg.FormCreate(Sender: TObject);
const OPNAME = 'TMinePropertiesDlg.FormCreate';
begin
  try
    FErrorList := TStringList.Create;
    FControlIterator := TControlIterator.Create;
    FControlIterator.ControlParent := ScrClient;
    FXMLAgent := TXMLAgentMineProperties.Create;
    FXMLAgent.DlgXMLDocumentIn  := FXMLDocumentIn;
    FXMLAgent.DlgXMLDocumentOut := FXMLDocumentOut;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMinePropertiesDlg.FormDestroy(Sender: TObject);
const OPNAME = 'TMinePropertiesDlg.FormDestroy';
begin
  try
    FreeAndNil(FErrorList);
    FreeAndNil(FControlIterator);
    FreeAndNil(FXMLAgent);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMinePropertiesDlg.FormShow(Sender: TObject);
const OPNAME = 'TMinePropertiesDlg.FormShow';
begin
  try
    LoadXMLData;
    SetIndicators;
    SetControls;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMinePropertiesDlg.SetIndicators;
const OPNAME = 'TMinePropertiesDlg.SetIndicators';
begin
  try
   FControlIterator.SetIndicators;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMinePropertiesDlg.SetAllValid;
const OPNAME = 'TMinePropertiesDlg.SetAllValid';
begin
  try
    FControlIterator.SetAllValid;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMinePropertiesDlg.SetControls;
const OPNAME = 'TMinePropertiesDlg.SetControls';
begin
  try
    FControlIterator.SetControls(FMayChangeNetwork);
    EdtNetworkSequence.Active  := FALSE;
    EdtModuleNumber.Active     := FALSE;
    BtnApply.Enabled := FMayChangeNetwork;
    BtnReset.Enabled := FMayChangeNetwork;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMinePropertiesDlg.LoadXMLData: boolean;
const OPNAME = 'TMinePropertiesDlg.LoadXMLData';
var
  LRootNode              : IXMLNode;
  LSectionNode           : IXMLNode;
  LAllOutflowRoutesNode  : IXMLNode;
  LAllRunOffModulesNode  : IXMLNode;
begin
  Result := FALSE;
  try
    FXMLDocumentIn.Active  := TRUE;
    FXMLDocumentOut.Active := TRUE;
    LRootNode              := FXMLDocumentIn.DocumentElement;
    LSectionNode           := LRootNode.ChildNodes['MineProperties'];
    LAllOutflowRoutesNode  := LRootNode.ChildNodes['OutflowRoutes'];
    LAllRunOffModulesNode  := LRootNode.ChildNodes['AllRunOffModules'];

    FIdentifier := LRootNode.ChildNodes['Identifier'].Text;
    FKeyValues  := 'Model='       + QuotedStr(LRootNode.ChildNodes['Model'].Text) +
                   ',NetworkID='  + QuotedStr(LRootNode.ChildNodes['NetworkID'].Text) +
                   ',ModuleType=' + QuotedStr(LRootNode.ChildNodes['ModuleType'].Text) +
                   ',Identifier=' + LRootNode.ChildNodes['Identifier'].Text;

    // Caption
    try
      Caption := 'Mine Module MM' + LSectionNode.ChildNodes['ModuleNumber'].Text;
    except
      Caption := 'Mine Module';
    end;

    // Populate route comboboxes.
    FXMLAgent.PopulateCbxAllNetworkModules(CbxRunOffModuleNo, LAllRunOffModulesNode, FALSE);
    FXMLAgent.PopulateCbxAllNetworkRoutes(CbxOutflowRouteNoToRiver, LAllOutflowRoutesNode, TRUE);
    FXMLAgent.PopulateCbxAllNetworkRoutes(CbxOutflowRouteNoToPCD, LAllOutflowRoutesNode, TRUE);

    Result := FControlIterator.LoadXMLData(LRootNode);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMinePropertiesDlg.StoreXMLData : Boolean;
const OPNAME = 'TMinePropertiesDlg.StoreXMLData';
var
  LRootNode     : IXMLNode;
  LSectionNode  : IXMLNode;
begin
  Result := FALSE;
  try
    LRootNode := FXMLDocumentOut.DocumentElement;
    LSectionNode := LRootNode.ChildNodes['MineProperties'];
    LRootNode.ChildNodes['Identifier'].Text := FIdentifier;
    Result := FControlIterator.StoreXMLData(LRootNode);
    if (ChbActive.Checked) then
      LSectionNode.ChildNodes['Active'].Text := 'Y'
    else
      LSectionNode.ChildNodes['Active'].Text := 'N';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMinePropertiesDlg.ControlExit(Sender: TObject);
const OPNAME = 'TMinePropertiesDlg.ControlExit';
var
  LControl : TWRMFControl;
begin
  try
    LControl := TWRMFControl(TControl(Sender).Parent);
    LControl.IsValid := TRUE;
    DoValidation(tcChange, LControl.PropertyName);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMinePropertiesDlg.BtnApplyClick(Sender: TObject);
const OPNAME = 'TMinePropertiesDlg.BtnApplyClick';
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

procedure TMinePropertiesDlg.BtnResetClick(Sender: TObject);
const OPNAME = 'TMinePropertiesDlg.BtnResetClick';
begin
  try
    LoadXMLData;
    SetAllValid;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMinePropertiesDlg.DoValidation (AContext      : TValidationContext;
                                          APropertyName : String) : Boolean;
const OPNAME = 'TMinePropertiesDlg.DoValidation';
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

procedure TMinePropertiesDlg.ParamChangeIndicatorClicked (Sender: TObject);
const OPNAME = 'TMinePropertiesDlg.ParamChangeIndicatorClicked';
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

procedure TMinePropertiesDlg.MetaDataIndicatorClicked (Sender: TObject);
const OPNAME = 'TMinePropertiesDlg.MetaDataIndicatorClicked';
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
