unit UMineOpencastPitPropertiesDlg;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, VCL.Graphics, VCL.Controls, VCL.Forms,
  VCL.Dialogs, VCL.StdCtrls, xmldom, XMLIntf, XMLDoc, VCL.ExtCtrls, VCL.Buttons, VCL.Menus,
  VCL.ComCtrls,

  HydrologyCom_TLB,
  UControlIterator,
  UXMLAgent,
  UXMLAgentMineOpencastPitProperties,
  UWRMFEdit, UWRMFComboBox, UWRMFControl, UWRMFCheckBox, Xml.Win.msxmldom;

type
  TMineOpencastPitPropertiesDlg = class(TForm)
    FXMLDocumentOut                      : TXMLDocument;
    FXMLDocumentIn                       : TXMLDocument;
    PnlBottom                            : TPanel;
    BtnApply                             : TButton;
    BtnReset                             : TButton;
    ScrClient                            : TScrollBox;
    LblOpencastPitName                   : TLabel;
    LblCoalReserveArea                   : TLabel;
    LblWorkingArea                       : TLabel;
    LblCommissionYear                    : TLabel;
    LblCommissionMonth                   : TLabel;
    LblDeCommissionYear                  : TLabel;
    LblDeCommissionMonth                 : TLabel;
    LblDisturbedArea                     : TLabel;
    LblRehabilitatedArea                 : TLabel;
    LblEvaporationArea                   : TLabel;
    LblDisturbedAreaRunOffFactor         : TLabel;
    LblDisturbedWorkingAreaRunOffFactor  : TLabel;
    LblWashOffParameter                  : TLabel;
    LblSulphateBuildUpRate               : TLabel;
    LblInitialSaltMass                   : TLabel;
    LblInspoilsStorageSeepage            : TLabel;
    LblInspoilsStorageDecant             : TLabel;
    LblInspoilsStorageInitialVolume      : TLabel;
    LblMaxSeepageRate                    : TLabel;
    LblSeepageEquationExponent           : TLabel;
    LblPCDFullSurfaceArea                : TLabel;
    LblPCDCapacity                       : TLabel;
    LblPCDInitialVolume                  : TLabel;
    LblInspoilsDamConcentration          : TLabel;
    EdtOpencastPitName                   : TWRMFEdit;
    EdtCoalReserveArea                   : TWRMFEdit;
    EdtWorkingArea                       : TWRMFEdit;
    EdtCommissionYear                    : TWRMFEdit;
    EdtCommissionMonth                   : TWRMFEdit;
    EdtDeCommissionYear                  : TWRMFEdit;
    EdtDeCommissionMonth                 : TWRMFEdit;
    EdtDisturbedArea                     : TWRMFEdit;
    EdtRehabilitatedArea                 : TWRMFEdit;
    EdtEvaporationArea                   : TWRMFEdit;
    EdtDisturbedAreaRunOffFactor         : TWRMFEdit;
    EdtDisturbedWorkingAreaRunOffFactor  : TWRMFEdit;
    EdtWashOffParameter                  : TWRMFEdit;
    EdtSulphateBuildUpRate               : TWRMFEdit;
    EdtInitialSaltMass                   : TWRMFEdit;
    EdtInspoilsStorageDecant             : TWRMFEdit;
    EdtInspoilsStorageSeepage            : TWRMFEdit;
    EdtSeepageEquationExponent           : TWRMFEdit;
    EdtMaxSeepageRate                    : TWRMFEdit;
    EdtInspoilsStorageInitialVolume      : TWRMFEdit;
    EdtInspoilsDamConcentration          : TWRMFEdit;
    EdtPCDInitialVolume                  : TWRMFEdit;
    EdtPCDCapacity                       : TWRMFEdit;
    EdtPCDFullSurfaceArea                : TWRMFEdit;
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
    FXMLAgent         : TXMLAgentMineOpencastPitProperties;
    FHydrologyModel   : IHydrologyModel;
  end;

implementation

uses
  UErrorHandlingOperations;

{$R *.dfm}

procedure TMineOpencastPitPropertiesDlg.FormCreate(Sender: TObject);
const OPNAME = 'TMineOpencastPitPropertiesDlg.FormCreate';
begin
  try
    FErrorList := TStringList.Create;
    FControlIterator := TControlIterator.Create;
    FControlIterator.ControlParent := ScrClient;
    FXMLAgent := TXMLAgentMineOpencastPitProperties.Create;
    FXMLAgent.DlgXMLDocumentIn  := FXMLDocumentIn;
    FXMLAgent.DlgXMLDocumentOut := FXMLDocumentOut;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMineOpencastPitPropertiesDlg.FormDestroy(Sender: TObject);
const OPNAME = 'TMineOpencastPitPropertiesDlg.FormDestroy';
begin
  try
    FreeAndNil(FErrorList);
    FreeAndNil(FControlIterator);
    FreeAndNil(FXMLAgent);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMineOpencastPitPropertiesDlg.FormShow(Sender: TObject);
const OPNAME = 'TMineOpencastPitPropertiesDlg.FormShow';
begin
  try
    LoadXMLData;
    SetControls;
    SetIndicators;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMineOpencastPitPropertiesDlg.SetIndicators;
const OPNAME = 'TMineOpencastPitPropertiesDlg.SetIndicators';
begin
  try
    FControlIterator.SetIndicators;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMineOpencastPitPropertiesDlg.SetAllValid;
const OPNAME = 'TMineOpencastPitPropertiesDlg.SetAllValid';
begin
  try
    FControlIterator.SetAllValid;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMineOpencastPitPropertiesDlg.SetControls;
const OPNAME = 'TMineOpencastPitPropertiesDlg.SetControls';
begin
  try
    FControlIterator.SetControls(FMayChangeNetwork);
    BtnApply.Enabled := FMayChangeNetwork;
    BtnReset.Enabled := FMayChangeNetwork;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMineOpencastPitPropertiesDlg.LoadXMLData: boolean;
const OPNAME = 'TMineOpencastPitPropertiesDlg.LoadXMLData';
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

function TMineOpencastPitPropertiesDlg.StoreXMLData : Boolean;
const OPNAME = 'TMineOpencastPitPropertiesDlg.StoreXMLData';
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

procedure TMineOpencastPitPropertiesDlg.ControlExit(Sender: TObject);
const OPNAME = 'TMineOpencastPitPropertiesDlg.ControlExit';
var
  LControl : TWRMFControl;
begin
  try
    LControl := TWRMFControl(TControl(Sender).Parent);
    LControl.IsValid := TRUE;
    DoValidation(tcChange, LControl.PropertyName);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMineOpencastPitPropertiesDlg.BtnApplyClick(Sender: TObject);
const OPNAME = 'TMineOpencastPitPropertiesDlg.BtnApplyClick';
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

procedure TMineOpencastPitPropertiesDlg.BtnResetClick(Sender: TObject);
const OPNAME = 'TMineOpencastPitPropertiesDlg.BtnResetClick';
begin
  try
    LoadXMLData;
    SetAllValid;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMineOpencastPitPropertiesDlg.DoValidation (AContext      : TValidationContext;
                                                     APropertyName : String) : Boolean;
const OPNAME = 'TMineOpencastPitPropertiesDlg.DoValidation';
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

procedure TMineOpencastPitPropertiesDlg.ParamChangeIndicatorClicked (Sender: TObject);
const OPNAME = 'TMineOpencastPitPropertiesDlg.ParamChangeIndicatorClicked';
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

procedure TMineOpencastPitPropertiesDlg.MetaDataIndicatorClicked (Sender: TObject);
const OPNAME = 'TMineOpencastPitPropertiesDlg.MetaDataIndicatorClicked';
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

