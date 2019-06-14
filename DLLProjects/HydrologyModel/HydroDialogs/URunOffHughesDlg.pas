unit URunOffHughesDlg;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, VCL.Graphics, VCL.Controls, VCL.Forms,
  VCL.Dialogs, VCL.StdCtrls, xmldom, XMLIntf, XMLDoc, VCL.ExtCtrls, VCL.Buttons, VCL.Menus,

  HydrologyCom_TLB,
  UControlIterator,
  UXMLAgent,
  UXMLAgentRunOffHughes,
  UWRMFEdit, UWRMFComboBox, UWRMFControl, UWRMFCheckBox, VCL.ComCtrls,
  VCL.Grids, UCellChangeStringGrid, UWRMFGrid;

type
  TRunOffHughesDlg = class(TForm)
    PnlBottom                        : TPanel;
    BtnApply                         : TButton;
    BtnReset                         : TButton;
    FXMLDocumentIn                   : TXMLDocument;
    FXMLDocumentOut                  : TXMLDocument;
    ScrClient                        : TScrollBox;
    LblInflowRouteNo                 : TLabel;
    LblInfluenceROMNo                : TLabel;
    LblGroundWaterModel              : TLabel;
    LblDrainageDensity               : TLabel;
    ChbUseNoOfReaches                : TWRMFCheckBox;
    LblNumberOfReaches               : TLabel;
    LblRiparianAreaWidthPercentage   : TLabel;
    LblRiparianStripFactor           : TLabel;
    LblRestWaterlevel                : TLabel;
    LblTransmissivity                : TLabel;
    LblStorativity                   : TLabel;
    LblGroundwaterSlope              : TLabel;
    LblHughesHGSL                    : TLabel;
    LblHughesGPOW                    : TLabel;
    EdtHughesGPOW                    : TWRMFEdit;
    EdtHughesHGSL                    : TWRMFEdit;
    EdtGroundwaterSlope              : TWRMFEdit;
    EdtStorativity                   : TWRMFEdit;
    EdtTransmissivity                : TWRMFEdit;
    EdtRestWaterlevel                : TWRMFEdit;
    EdtRiparianStripFactor           : TWRMFEdit;
    EdtRiparianAreaWidthPercentage   : TWRMFEdit;
    EdtNumberOfReaches               : TWRMFEdit;
    EdtDrainageDensity               : TWRMFEdit;
    CbxGroundwaterModel              : TWRMFComboBox;
    CbxInfluenceROMNo                : TWRMFComboBox;
    CbxInflowRouteNo                 : TWRMFComboBox;
    LblHughesTLGMax                  : TLabel;
    LblHughesHGGW                    : TLabel;
    LblHughesPOW                     : TLabel;
    LblHughesSL                      : TLabel;
    LblHughesST                      : TLabel;
    LblHughesFT                      : TLabel;
    LblHughesGW                      : TLabel;
    LblHughesZMIN                    : TLabel;
    LblHughesZMAX                    : TLabel;
    LblHughesPI                      : TLabel;
    LblHughesTL                      : TLabel;
    LblHughesGL                      : TLabel;
    LblHughesR                       : TLabel;
    LblHughesFF                      : TLabel;
    EdtHughesFF                      : TWRMFEdit;
    EdtHughesR                       : TWRMFEdit;
    EdtHughesGL                      : TWRMFEdit;
    EdtHughesTL                      : TWRMFEdit;
    EdtHughesPI                      : TWRMFEdit;
    EdtHughesZMAX                    : TWRMFEdit;
    EdtHughesZMIN                    : TWRMFEdit;
    EdtHughesGW                      : TWRMFEdit;
    EdtHughesFT                      : TWRMFEdit;
    EdtHughesST                      : TWRMFEdit;
    EdtHughesSL                      : TWRMFEdit;
    EdtHughesPOW                     : TWRMFEdit;
    EdtHughesHGGW                    : TWRMFEdit;
    EdtHughesTLGMax                  : TWRMFEdit;
    LblAnnualUpperZoneAbstraction    : TLabel;
    LblAnnualRiparianZoneAbstraction : TLabel;
    LblGrdHughesHeading              : TLabel;
    EdtAnnualRiparianZoneAbstraction : TWRMFEdit;
    EdtAnnualUpperZoneAbstraction    : TWRMFEdit;
    GrdHughes                        : TWRMFGrid;
    procedure FormShow(Sender: TObject);
    procedure ControlExit(Sender: TObject);
    procedure BtnApplyClick(Sender: TObject);
    procedure BtnResetClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ParamChangeIndicatorClicked (Sender: TObject);
    procedure MetaDataIndicatorClicked (Sender: TObject);
    procedure GridParamChangeIndicatorClicked (Sender: TObject; ACol, ARow: integer);
    procedure GridMetaDataIndicatorClicked (Sender: TObject; ACol, ARow: integer);
    procedure GrdHughesDataCellExit(ASender: TObject; ACol, ARow: Integer);
    procedure GrdHughesExit(Sender: TObject);
  private
    { Private declarations }
    FErrorList        : TStringList;
    FIdentifier       : String;
    FKeyValues        : WideString;
    FControlIterator  : TControlIterator;
    function LoadXMLData : Boolean;
    function StoreXMLData : Boolean;
    function DoValidation (AContext      : TValidationContext;
                           APropertyName : String;
                           AFieldIndex   : String) : Boolean;
    procedure SetIndicators;
    procedure SetControls;
    procedure SetAllValid;
  public
    { Public declarations }
    FMayChangeNetwork : Boolean;
    FXMLAgent         : TXMLAgentRunOffHughes;
    FHydrologyModel   : IHydrologyModel;
  end;


implementation

uses
  UErrorHandlingOperations;

{$R *.dfm}

procedure TRunOffHughesDlg.FormCreate(Sender: TObject);
const OPNAME = 'TRunOffHughesDlg.FormCreate';
begin
  try
    FErrorList := TStringList.Create;
    FControlIterator := TControlIterator.Create;
    FControlIterator.ControlParent := ScrClient;
    FXMLAgent := TXMLAgentRunOffHughes.Create;
    FXMLAgent.DlgXMLDocumentIn  := FXMLDocumentIn;
    FXMLAgent.DlgXMLDocumentOut := FXMLDocumentOut;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRunOffHughesDlg.FormDestroy(Sender: TObject);
const OPNAME = 'TRunOffHughesDlg.FormDestroy';
begin
  try
    FreeAndNil(FErrorList);
    FreeAndNil(FControlIterator);
    FreeAndNil(FXMLAgent);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRunOffHughesDlg.FormShow(Sender: TObject);
const OPNAME = 'TRunOffHughesDlg.FormShow';
begin
  try
    LoadXMLData;
    SetControls;
    SetIndicators;
    GrdHughes.Cells[0, 0] := 'Month';
    GrdHughes.Cells[1, 0] := 'Upper zone';
    GrdHughes.Cells[2, 0] := 'Riparian zone';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRunOffHughesDlg.SetControls;
const OPNAME = 'TRunOffHughesDlg.SetControls';
begin
  try
    FControlIterator.SetControls(FMayChangeNetwork);
    BtnApply.Enabled           := FMayChangeNetwork;
    BtnReset.Enabled           := FMayChangeNetwork;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRunOffHughesDlg.SetIndicators;
const OPNAME = 'TRunOffHughesDlg.SetIndicators';
begin
  try
    FControlIterator.SetIndicators;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRunOffHughesDlg.SetAllValid;
const OPNAME = 'TRunOffHughesDlg.SetAllValid';
begin
  try
    FControlIterator.SetAllValid;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRunOffHughesDlg.LoadXMLData: boolean;
const OPNAME = 'TRunOffHughesDlg.LoadXMLData';
var
  LRootNode               : IXMLNode;
  LAllNetworkRoutesNode   : IXMLNode;
  LAllRunOffModulesNode   : IXMLNode;
begin
  Result := FALSE;
  try
    FXMLDocumentIn.Active  := TRUE;
    FXMLDocumentOut.Active := TRUE;
    LRootNode              := FXMLDocumentIn.DocumentElement;
    LAllNetworkRoutesNode  := LRootNode.ChildNodes['AllNetworkRoutes'];
    LAllRunOffModulesNode  := LRootNode.ChildNodes['AllRunOffModules'];

    FIdentifier := LRootNode.ChildNodes['Identifier'].Text;
    FKeyValues  := 'Model='       + QuotedStr(LRootNode.ChildNodes['Model'].Text) +
                   ',NetworkID='  + QuotedStr(LRootNode.ChildNodes['NetworkID'].Text) +
                   ',ModuleType=' + QuotedStr(LRootNode.ChildNodes['ModuleType'].Text) +
                   ',Identifier=' + LRootNode.ChildNodes['Identifier'].Text;

    // Populate comboboxes
    CbxGroundwaterModel.Items.Clear;
    CbxGroundwaterModel.Items.AddObject('Pitman', pointer(0));
    CbxGroundwaterModel.Items.AddObject('Hughes', pointer(1));
    CbxGroundwaterModel.Items.AddObject('Sami', pointer(2));
    FXMLAgent.PopulateCbxAllNetworkModules(CbxInfluenceROMNo, LAllRunOffModulesNode, TRUE);
    FXMLAgent.PopulateCbxAllNetworkRoutes(CbxInflowRouteNo, LAllNetworkRoutesNode, TRUE);

    Result := FControlIterator.LoadXMLData(LRootNode);
  except on E: Exception do HandleError(E, OPNAME) end;
end;


function TRunOffHughesDlg.StoreXMLData : Boolean;
const OPNAME = 'TRunOffHughesDlg.StoreXMLData';
var
  LRootNode    : IXMLNode;
  LSectionNode : IXMLNode;
begin
  Result := FALSE;
  try
    LRootNode               := FXMLDocumentOut.DocumentElement;
    LSectionNode            := LRootNode.ChildNodes['RunOffHughes'];
    LRootNode.ChildNodes['Identifier'].Text := FIdentifier;
    Result := FControlIterator.StoreXMLData(LRootNode);
    if (ChbUseNoOfReaches.Checked) then
      LSectionNode.ChildNodes['UseNoOfReaches'].Text := '1'
    else
      LSectionNode.ChildNodes['UseNoOfReaches'].Text := '0';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRunOffHughesDlg.ControlExit(Sender: TObject);
const OPNAME = 'TRunOffHughesDlg.ControlExit';
var
  LControl : TWRMFControl;
begin
  try
    LControl := TWRMFControl(TControl(Sender).Parent);
    LControl.IsValid := TRUE;
    DoValidation(tcChange, LControl.PropertyName, '');
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRunOffHughesDlg.GrdHughesDataCellExit(ASender: TObject; ACol, ARow: Integer);
const OPNAME = 'TRunOffHughesDlg.GrdHughesDataCellExit';
var
  LCellInfo : TCellInfo;
  LGrid     : TWRMFGrid;
begin
  try
    LGrid := TWRMFGrid(ASender);
    LCellInfo := LGrid.CellInfo[ARow, ACol];
    LCellInfo.IsValid := TRUE;
    DoValidation(tcChange, TWRMFGrid(ASender).CellInfo[ARow, ACol].PropertyName, IntToStr(ARow));
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRunOffHughesDlg.GrdHughesExit(Sender: TObject);
const OPNAME = 'TRunOffHughesDlg.GrdHughesExit';
var
  LWRMFGrid : TWRMFGrid;
begin
  try
    LWRMFGrid := TWRMFGrid(Sender);
    GrdHughesDataCellExit(LWRMFGrid, LWRMFGrid.Col, LWRMFGrid.Row);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRunOffHughesDlg.BtnApplyClick(Sender: TObject);
const OPNAME = 'TRunOffHughesDlg.BtnApplyClick';
begin
  try
    SetAllValid;
    if (DoValidation(tcApply, '', '')) then
    begin
      FHydrologyModel.UpdateNetworkData(FXMLDocumentOut.XML.Text);
      LoadXMLData;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRunOffHughesDlg.BtnResetClick(Sender: TObject);
const OPNAME = 'TRunOffHughesDlg.BtnResetClick';
begin
  try
    LoadXMLData;
    SetAllValid;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRunOffHughesDlg.DoValidation (AContext      : TValidationContext;
                                        APropertyName : String;
                                        AFieldIndex   : String) : Boolean;
const OPNAME = 'TRunOffHughesDlg.DoValidation';
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
        Result := FControlIterator.DoValidation(AContext, APropertyName, AFieldIndex,
                                      FXMLDocumentIn.XML.Text, FXMLDocumentOut.XML.Text, FALSE,
                                      FErrorList, FXMLAgent, LRootNode);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRunOffHughesDlg.ParamChangeIndicatorClicked (Sender: TObject);
const OPNAME = 'TRunOffHughesDlg.ParamChangeIndicatorClicked';
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

procedure TRunOffHughesDlg.MetaDataIndicatorClicked (Sender: TObject);
const OPNAME = 'TRunOffHughesDlg.MetaDataIndicatorClicked';
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

procedure TRunOffHughesDlg.GridParamChangeIndicatorClicked (Sender: TObject; ACol, ARow: integer);
const OPNAME = 'TRunOffHughesDlg.GridParamChangeIndicatorClicked';
{var
  LGrid     : TWRMFGrid;
  LCellInfo : TCellInfo;}
begin
  try
{    if (Sender is TWRMFGrid) then
    begin
      LGrid := TWRMFGrid(Sender);
      LCellInfo := LGrid.CellInfo[ARow, ACol];
      if (LCellInfo.HasParamChange) then
        ShowMessage ('SHOW Param Changes for ' + LCellInfo.PropertyName + ' (' + IntToStr(ARow) + ') ' + FKeyValues)
      else
        ShowMessage ('NEW Param Changes for ' + LCellInfo.PropertyName + ' (' + IntToStr(ARow) + ') ' + FKeyValues);
    end;}
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRunOffHughesDlg.GridMetaDataIndicatorClicked (Sender: TObject; ACol, ARow: integer);
const OPNAME = 'TRunOffHughesDlg.GridMetaDataIndicatorClicked';
{var
  LGrid     : TWRMFGrid;
  LCellInfo : TCellInfo;}
begin
  try
{    if (Sender is TWRMFGrid) then
    begin
      LGrid := TWRMFGrid(Sender);
      LCellInfo := LGrid.CellInfo[ARow, ACol];
      if (LCellInfo.HasMetaData) then
        ShowMessage ('SHOW Meta Data for ' + LCellInfo.PropertyName + ' (' + IntToStr(ARow) + ') ' + FKeyValues)
      else
        ShowMessage ('NEW Meta Data for ' + LCellInfo.PropertyName + ' (' + IntToStr(ARow) + ') ' + FKeyValues);
    end;}
  except on E: Exception do HandleError(E, OPNAME) end;
end;



end.
