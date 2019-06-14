unit URunOffSamiDlg;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, VCL.Graphics, VCL.Controls, VCL.Forms,
  VCL.Dialogs, VCL.StdCtrls, xmldom, XMLIntf, XMLDoc, VCL.ExtCtrls, VCL.Buttons, VCL.Menus,

  HydrologyCom_TLB,
  UControlIterator,
  UXMLAgent,
  UXMLAgentRunOffSami,
  UWRMFEdit, UWRMFComboBox, UWRMFControl, UWRMFCheckBox, VCL.ComCtrls,
  VCL.Grids, UCellChangeStringGrid, UWRMFGrid;

type
  TRunOffSamiDlg = class(TForm)
    PnlBottom                        : TPanel;
    BtnApply                         : TButton;
    BtnReset                         : TButton;
    FXMLDocumentIn                   : TXMLDocument;
    FXMLDocumentOut                  : TXMLDocument;
    ScrClient                        : TScrollBox;
    LblAquiferThickness              : TLabel;
    LblSamiStorativity               : TLabel;
    LblInitialAquiferStorage         : TLabel;
    LblStaticWaterLevel              : TLabel;
    LblUnsaturatedStorage            : TLabel;
    LblInitialUnsaturatedZoneStorage : TLabel;
    LblPerculationPower              : TLabel;
    LblMaxDischarge                  : TLabel;
    LblInteractionCurvePower         : TLabel;
    LblMaxHydrologicalGradient       : TLabel;
    LblSamiTransmissivity            : TLabel;
    LblBoreholeDistanceToRiver       : TLabel;
    LblGroundWaterEvaporationArea    : TLabel;
    LblInterflowLag                  : TLabel;
    LblRechargeAveragedNoMonths      : TLabel;
    LblSamiGPOW                      : TLabel;
    LblSamiHGSL                      : TLabel;
    LblSamiHGGW                      : TLabel;
    LblK2                            : TLabel;
    LblK3                            : TLabel;
    LblSamiPOW                       : TLabel;
    LblSamiSL                        : TLabel;
    LblSamiST                        : TLabel;
    LblSamiFT                        : TLabel;
    LblSamiGW                        : TLabel;
    LblSamiZMIN                      : TLabel;
    LblSamiZMAX                      : TLabel;
    LblSamiPI                        : TLabel;
    LblSamiTL                        : TLabel;
    LblSamiGL                        : TLabel;
    LblSamiR                         : TLabel;
    LblSamiFF                        : TLabel;
    EdtAquiferThickness              : TWRMFEdit;
    EdtSamiStorativity               : TWRMFEdit;
    EdtInitialAquiferStorage         : TWRMFEdit;
    EdtStaticWaterLevel              : TWRMFEdit;
    EdtUnsaturatedStorage            : TWRMFEdit;
    EdtInitialUnsaturatedZoneStorage : TWRMFEdit;
    EdtPerculationPower              : TWRMFEdit;
    EdtMaxDischarge                  : TWRMFEdit;
    EdtInteractionCurvePower         : TWRMFEdit;
    EdtMaxHydrologicalGradient       : TWRMFEdit;
    EdtSamiTransmissivity            : TWRMFEdit;
    EdtBoreholeDistanceToRiver       : TWRMFEdit;
    EdtGroundWaterEvaporationArea    : TWRMFEdit;
    EdtInterflowLag                  : TWRMFEdit;
    EdtRechargeAveragedNoMonths      : TWRMFEdit;
    ChbUseAbstractions               : TWRMFCheckBox;
    EdtSamiGPOW                      : TWRMFEdit;
    EdtSamiHGSL                      : TWRMFEdit;
    EdtSamiHGGW                      : TWRMFEdit;
    EdtK2                            : TWRMFEdit;
    EdtK3                            : TWRMFEdit;
    EdtSamiPOW                       : TWRMFEdit;
    EdtSamiSL                        : TWRMFEdit;
    EdtSamiST                        : TWRMFEdit;
    EdtSamiFT                        : TWRMFEdit;
    EdtSamiGW                        : TWRMFEdit;
    EdtSamiZMIN                      : TWRMFEdit;
    EdtSamiZMAX                      : TWRMFEdit;
    EdtSamiPI                        : TWRMFEdit;
    EdtSamiTL                        : TWRMFEdit;
    EdtSamiGL                        : TWRMFEdit;
    EdtSamiR                         : TWRMFEdit;
    EdtSamiFF                        : TWRMFEdit;
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
    FXMLAgent         : TXMLAgentRunOffSami;
    FHydrologyModel   : IHydrologyModel;
  end;


implementation

uses
  UErrorHandlingOperations;

{$R *.dfm}

procedure TRunOffSamiDlg.FormCreate(Sender: TObject);
const OPNAME = 'TRunOffSamiDlg.FormCreate';
begin
  try
    FErrorList := TStringList.Create;
    FControlIterator := TControlIterator.Create;
    FControlIterator.ControlParent := ScrClient;
    FXMLAgent := TXMLAgentRunOffSami.Create;
    FXMLAgent.DlgXMLDocumentIn  := FXMLDocumentIn;
    FXMLAgent.DlgXMLDocumentOut := FXMLDocumentOut;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRunOffSamiDlg.FormDestroy(Sender: TObject);
const OPNAME = 'TRunOffSamiDlg.FormDestroy';
begin
  try
    FreeAndNil(FErrorList);
    FreeAndNil(FControlIterator);
    FreeAndNil(FXMLAgent);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRunOffSamiDlg.FormShow(Sender: TObject);
const OPNAME = 'TRunOffSamiDlg.FormShow';
begin
  try
    LoadXMLData;
    SetControls;
    SetIndicators;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRunOffSamiDlg.SetControls;
const OPNAME = 'TRunOffSamiDlg.SetControls';
begin
  try
    FControlIterator.SetControls(FMayChangeNetwork);
    BtnApply.Enabled           := FMayChangeNetwork;
    BtnReset.Enabled           := FMayChangeNetwork;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRunOffSamiDlg.SetIndicators;
const OPNAME = 'TRunOffSamiDlg.SetIndicators';
begin
  try
    FControlIterator.SetIndicators;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRunOffSamiDlg.SetAllValid;
const OPNAME = 'TRunOffSamiDlg.SetAllValid';
begin
  try
    FControlIterator.SetAllValid;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRunOffSamiDlg.LoadXMLData: boolean;
const OPNAME = 'TRunOffSamiDlg.LoadXMLData';
var
  LRootNode      : IXMLNode;
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


function TRunOffSamiDlg.StoreXMLData : Boolean;
const OPNAME = 'TRunOffSamiDlg.StoreXMLData';
var
  LRootNode       : IXMLNode;
  LSectionNode    : IXMLNode;
begin
  Result := FALSE;
  try
    LRootNode    := FXMLDocumentOut.DocumentElement;
    LSectionNode := LRootNode.ChildNodes['RunOffSami'];
    LRootNode.ChildNodes['Identifier'].Text := FIdentifier;
    Result := FControlIterator.StoreXMLData(LRootNode);
    if (ChbUseAbstractions.Checked) then
      LSectionNode.ChildNodes['UseAbstractions'].Text := '1'
    else
      LSectionNode.ChildNodes['UseAbstractions'].Text := '0';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRunOffSamiDlg.ControlExit(Sender: TObject);
const OPNAME = 'TRunOffSamiDlg.ControlExit';
var
  LControl : TWRMFControl;
begin
  try
    LControl := TWRMFControl(TControl(Sender).Parent);
    LControl.IsValid := TRUE;
    DoValidation(tcChange, LControl.PropertyName);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRunOffSamiDlg.BtnApplyClick(Sender: TObject);
const OPNAME = 'TRunOffSamiDlg.BtnApplyClick';
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

procedure TRunOffSamiDlg.BtnResetClick(Sender: TObject);
const OPNAME = 'TRunOffSamiDlg.BtnResetClick';
begin
  try
    LoadXMLData;
    SetAllValid;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRunOffSamiDlg.DoValidation (AContext      : TValidationContext;
                                      APropertyName : String) : Boolean;
const OPNAME = 'TRunOffSamiDlg.DoValidation';
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

procedure TRunOffSamiDlg.ParamChangeIndicatorClicked (Sender: TObject);
const OPNAME = 'TRunOffSamiDlg.ParamChangeIndicatorClicked';
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

procedure TRunOffSamiDlg.MetaDataIndicatorClicked (Sender: TObject);
const OPNAME = 'TRunOffSamiDlg.MetaDataIndicatorClicked';
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
