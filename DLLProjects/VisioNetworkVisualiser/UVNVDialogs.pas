{******************************************************************************}
{*  UNIT      : Contains the class VNVDialogs.                                *}
{*  AUTHOR    : Riana Steyn                                                   *}
{*  DATE      : 2005/10/01                                                    *}
{*  COPYRIGHT : Copyright © 2005 DWAF                                         *}
{******************************************************************************}

unit UVNVDialogs;

interface

uses
  VCL.Forms,
  VCL.Controls,
  VCL.ExtCtrls,
  VCL.StdCtrls,
  Classes,
  VCL.XPMan,
  UAbstractComponent,
  UYieldModelDataObject,
  VoaimsCom_TLB,
  SysUtils,
  VCL.ComCtrls,
  CheckCombo,
  VCL.ListActns;

type
  TVNVTextType = (nvtNone, nvtGeneral, nvtDemandFile, nvtName, nvtInflow, nvtElevation, nvtNetBasinRunoff,
                  nvtVolume,nvtRainfall,nvtEvaporation, nvtPercFull, nvtChannelFlow, nvtSFRASubCatchment,
                  nvtAvgElevation, nvtAvgVolume, nvtAvgNetBasinRunoff,nvtAvgRainfall, nvtAvgEvaporation,
                  nvtAvgChannelFlow,nvtReservoirStorageChange,nvtTimePeriod);
type
  TVNVReservoirDialog = class(TAbstractForm)
  private
    FScrollBox         : TScrollBox;
    FExistGrp          : TGroupBox;
    FExistRdb          : TRadioButton;
    FNewRdb            : TRadioButton;
    FShowDuplicatesChx : TCheckBox;
//    FNamesCbx          : TComboBox;
    FNamesCbx          : TCheckedComboBox;
    FOKBtn             : TButton;
    FCancelBtn         : TButton;
    FYesBtn            : TButton;
    FNoBtn             : TButton;
    FDuplicateLbl      : TLabel;
    FExistingStr       : TStringList;
    FSelectedNr        : integer;
    procedure OnOKBtnClick (Sender: TObject);
    procedure OnYesBtnClick (Sender: TObject);
    procedure OnCancelBtnClick (Sender: TObject);
    procedure OnControlClick (Sender: TObject);
    procedure ResetControls;
  protected
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
  public
    function LanguageHasChanged: boolean; override;
	  function ShowReservoirsDialog (const AExistingReservoirs : WideString;
                                   var AExistNew             : integer;
                                   var ADuplicates           : integer): integer;
  end;

  TVNVNodesDialog = class(TAbstractForm)
	private
    FScrollBox         : TScrollBox;
    FInflowGrp         : TGroupBox;
    FWithInflowRdb     : TRadioButton;
    FWithoutInflowRdb  : TRadioButton;
    FExistGrp          : TGroupBox;
    FExistRdb          : TRadioButton;
    FNewRdb            : TRadioButton;
    FShowDuplicatesChx : TCheckBox;
    FNamesCbx          : TComboBox;
    FOKBtn             : TButton;
    FCancelBtn         : TButton;
    FYesBtn            : TButton;
    FNoBtn             : TButton;
    FDuplicateLbl      : TLabel;
    FExistingStr       : TStringList;
    FSelectedNr        : Integer;
    procedure OnOKBtnClick(Sender: TObject);
    procedure OnYesBtnClick (Sender: TObject);
    procedure OnCancelBtnClick(Sender: TObject);
    procedure OnControlClick(Sender: TObject);
    procedure ResetControls;
  protected
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
  public
    function LanguageHasChanged: boolean; override;
	  function ShowNodesDialog (const AExistingNodes : WideString;
                              var AWithInflow      : integer;
                              var AExistNew        : integer;
                              var ADuplicates      : integer): integer;
  end;

  TVNVChannelsDialog = class(TAbstractForm)
	private
    FScrollBox         : TScrollBox;
    FExistGrp          : TGroupBox;
    FExistRdb          : TRadioButton;
    FNewRdb            : TRadioButton;
    FShowDuplicatesChx : TCheckBox;
    FNamesCbx          : TComboBox;
    FOKBtn             : TButton;
    FCancelBtn         : TButton;
    FYesBtn            : TButton;
    FNoBtn             : TButton;
    FDuplicateLbl      : TLabel;
    FExistingStr       : TStringList;
    FSelectedNr        : Integer;
    procedure OnOKBtnClick (Sender: TObject);
    procedure OnYesBtnClick (Sender: TObject);
    procedure OnCancelBtnClick (Sender: TObject);
    procedure OnControlClick (Sender: TObject);
    procedure ResetControls;
  protected
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
  public
    function LanguageHasChanged: boolean; override;
	  function ShowChannelsDialog (const AExistingChannels : WideString;
                                 var AExistNew           : integer;
                                 var ADuplicates         : integer): integer;
  end;

  TVNVReservoirPenaltyDialog = class(TAbstractForm)
	private
    FScrollBox          : TScrollBox;
    FShowDuplicatesChx  : TCheckBox;
    FNamesCbx           : TComboBox;
    FOKBtn              : TButton;
    FCancelBtn          : TButton;
    FYesBtn             : TButton;
    FNoBtn              : TButton;
    FDuplicateLbl       :  TLabel;
    FExistingReservoirs : TStringList;
    FExistingPenalties  : TStringList;
    FSelectedNr         : Integer;
    procedure OnOKBtnClick (Sender: TObject);
    procedure OnYesBtnClick (Sender: TObject);
    procedure OnCancelBtnClick (Sender: TObject);
    procedure OnControlClick (Sender: TObject);
    procedure ResetControls;
  protected
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
  public
    function LanguageHasChanged: boolean; override;
	  function ShowReservoirPenaltiesDialog (const AExistingReservoirs : WideString;
                                           const AExistingPenalties  : WideString;
                                           var ADuplicates           : integer): integer;
  end;

  TVNVChannelPenaltyDialog = class(TAbstractForm)
	private
    FScrollBox          : TScrollBox;
    FShowDuplicatesChx  : TCheckBox;
    FNamesCbx           : TComboBox;
    FOKBtn              : TButton;
    FCancelBtn          : TButton;
    FYesBtn             : TButton;
    FNoBtn              : TButton;
    FDuplicateLbl       : TLabel;
    FExistingChannels   : TStringList;
    FExistingPenalties  : TStringList;
    FSelectedNr         : Integer;
    procedure OnOKBtnClick (Sender: TObject);
    procedure OnYesBtnClick (Sender: TObject);
    procedure OnCancelBtnClick (Sender: TObject);
    procedure OnControlClick (Sender: TObject);
    procedure ResetControls;
  protected
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
  public
    function LanguageHasChanged: boolean; override;
	  function ShowChannelPenaltiesDialog (const AExistingChannels  : WideString;
                                         const AExistingPenalties : WideString;
                                         var ADuplicates          : integer): integer;
  end;

  TVNVTextDialog = class(TAbstractForm)
	private
    FScrollBox          : TScrollBox;
    FAssignParentChx    : TCheckBox;
    FReservoirRdb       : TRadioButton;
    FNodeRdb            : TRadioButton;
    FChannelRdb         : TRadioButton;
    FWetlandRdb         : TRadioButton;
    FNamesCbx           : TComboBox;
    FTypeLbl            : TLabel;
    FTypeCbx            : TComboBox;
    FOKBtn              : TButton;
    FCancelBtn          : TButton;
    FElementNr          : integer;
    FElementType        : string;
    FTextType           : TVNVTextType;
    FReservoirs         : TStringList;
    FNodes              : TStringList;
    FChannels           : TStringList;
    FWetlands           : TStringList;
    procedure OnOKBtnClick (Sender: TObject);
    procedure OnCancelBtnClick (Sender: TObject);
    procedure OnControlClick (Sender: TObject);
    procedure OnSelectElement (Sender: TObject);
    procedure ResetControls;
    procedure PopulateReservoirTextTypes;
    procedure PopulateNodeTextTypes;
    procedure PopulateChannelTextTypes;
  protected
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
  public
    function LanguageHasChanged: boolean; override;
	  function ShowTextDialog (const AReservoirs  : WideString;
                             const ANodes       : WideString;
                             const AChannels    : WideString;
                             const AWetlands    : WideString;
                             var   AElementNr   : integer;
                             var   AElementType : string;
                             var   ATextType    : TVNVTextType): boolean;
  end;

  TVNVIrrigationBlockDialog = class(TAbstractForm)
  public
    FScrollBox         : TScrollBox;
    FExistGrp          : TGroupBox;
    FExistRdb          : TRadioButton;
    FNewRdb            : TRadioButton;
    FShowDuplicatesChx : TCheckBox;
    FNamesCbx          : TComboBox;
    FOKBtn             : TButton;
    FCancelBtn         : TButton;
    FYesBtn            : TButton;
    FNoBtn             : TButton;
    FDuplicateLbl      : TLabel;
    FExistingStr       : TStringList;
    FSelectedNr        : integer;
    procedure OnOKBtnClick (Sender: TObject);
    procedure OnYesBtnClick (Sender: TObject);
    procedure OnCancelBtnClick (Sender: TObject);
    procedure OnControlClick (Sender: TObject);
    procedure ResetControls;
  protected
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
  public
    function LanguageHasChanged: boolean; override;
	  function ShowIrrigationBlockDialog (const AExistingIrrigationBlock : WideString;
                                        var AExistNew   : integer;var ADuplicates : integer): integer;
  end;

  TVNVIrrigationAreaDialog = class(TAbstractForm)
  public
    FScrollBox         : TScrollBox;
    FExistGrp          : TGroupBox;
    FExistRdb          : TRadioButton;
    FNewRdb            : TRadioButton;
    FShowDuplicatesChx : TCheckBox;
    FNamesCbx          : TComboBox;
    FOKBtn             : TButton;
    FCancelBtn         : TButton;
    FYesBtn            : TButton;
    FNoBtn             : TButton;
    FDuplicateLbl      : TLabel;
    FExistingStr       : TStringList;
    FSelectedNr        : integer;
    procedure OnOKBtnClick (Sender: TObject);
    procedure OnYesBtnClick (Sender: TObject);
    procedure OnCancelBtnClick (Sender: TObject);
    procedure OnControlClick (Sender: TObject);
    procedure ResetControls;
  protected
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
  public
    function LanguageHasChanged: boolean; override;
	  function ShowIrrigationAreaDialog (const AExistingIrrigationArea : WideString;
                                        var AExistNew   : integer;var ADuplicates : integer): integer;
  end;


  TVNVWetlandDialog = class(TAbstractForm)
  public
    FScrollBox         : TScrollBox;
    FExistGrp          : TGroupBox;
    FExistRdb          : TRadioButton;
    FNewRdb            : TRadioButton;
    FShowDuplicatesChx : TCheckBox;
    FNamesCbx          : TComboBox;
    FOKBtn             : TButton;
    FCancelBtn         : TButton;
    FYesBtn            : TButton;
    FNoBtn             : TButton;
    FDuplicateLbl      : TLabel;
    FExistingStr       : TStringList;
    FSelectedNr        : integer;
    procedure OnOKBtnClick (Sender: TObject);
    procedure OnYesBtnClick (Sender: TObject);
    procedure OnCancelBtnClick (Sender: TObject);
    procedure OnControlClick (Sender: TObject);
    procedure ResetControls;
  protected
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
  public
    function LanguageHasChanged: boolean; override;
	  function ShowWetlandDialog (const AExistingWetland : WideString;
                                        var AExistNew   : integer;var ADuplicates : integer): integer;
  end;

  TVNVDemandCentreDialog = class(TAbstractForm)
  public
    FScrollBox         : TScrollBox;
    FExistGrp          : TGroupBox;
    FExistRdb          : TRadioButton;
    FNewRdb            : TRadioButton;
    FShowDuplicatesChx : TCheckBox;
    FNamesCbx          : TComboBox;
    FOKBtn             : TButton;
    FCancelBtn         : TButton;
    FYesBtn            : TButton;
    FNoBtn             : TButton;
    FDuplicateLbl      : TLabel;
    FExistingStr       : TStringList;
    FSelectedNr        : integer;
    procedure OnOKBtnClick (Sender: TObject);
    procedure OnYesBtnClick (Sender: TObject);
    procedure OnCancelBtnClick (Sender: TObject);
    procedure OnControlClick (Sender: TObject);
    procedure ResetControls;
  protected
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
  public
    function LanguageHasChanged: boolean; override;
    function ShowDemandCentreDialog (const AExistingDemandCentre : WideString;
                                        var AExistNew   : integer;var ADuplicates : integer): integer;
  end;

  TVNVSubCatchmentDialog = class(TAbstractForm)
  public
    FScrollBox         : TScrollBox;
    FExistGrp          : TGroupBox;
    FExistRdb          : TRadioButton;
    FNewRdb            : TRadioButton;
    FShowDuplicatesChx : TCheckBox;
    FNamesCbx          : TComboBox;
    FOKBtn             : TButton;
    FCancelBtn         : TButton;
    FYesBtn            : TButton;
    FNoBtn             : TButton;
    FDuplicateLbl      : TLabel;
    FExistingStr       : TStringList;
    FSelectedNr        : integer;
    FNodeNr            : integer;
    FCreateOnlyAllowed : Boolean;
    procedure OnOKBtnClick (Sender: TObject);
    procedure OnYesBtnClick (Sender: TObject);
    procedure OnCancelBtnClick (Sender: TObject);
    procedure OnControlClick (Sender: TObject);
    procedure ResetControls;
  protected
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
  public
    function LanguageHasChanged: boolean; override;
    function ShowSubCatchmentDialog (const AExistingSubCatchment : WideString;
                                       var AExistNew   : integer;
                                       var ADuplicates : integer;
                                       ANodeNr :  integer;
                                       AllowOnlyCreate  :  Boolean): integer;
  end;

  TVNVMineDialog = class(TAbstractForm)
  private
    FScrollBox         : TScrollBox;
    FExistGrp          : TGroupBox;
    FExistRdb          : TRadioButton;
    FNewRdb            : TRadioButton;
    FShowDuplicatesChx : TCheckBox;
    FNamesCbx          : TComboBox;
    FOKBtn             : TButton;
    FCancelBtn         : TButton;
    FYesBtn            : TButton;
    FNoBtn             : TButton;
    FDuplicateLbl      : TLabel;
    FExistingStr       : TStringList;
    FSelectedNr        : integer;
    procedure OnOKBtnClick (Sender: TObject);
    procedure OnYesBtnClick (Sender: TObject);
    procedure OnCancelBtnClick (Sender: TObject);
    procedure OnControlClick (Sender: TObject);
    procedure ResetControls;
  protected
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
  public
    function LanguageHasChanged: boolean; override;
	  function ShowMinesDialog (const AExistingMines : WideString;
                                   var AExistNew             : integer;
                                   var ADuplicates           : integer): integer;
  end;

  TVNVDroughtRestrictionDialog = class(TAbstractForm)
  private
    FScrollBox         : TScrollBox;
    FExistGrp          : TGroupBox;
    FExistRdb          : TRadioButton;
    FNewRdb            : TRadioButton;
    FShowAllChx : TCheckBox;
    FNamesCbx          : TComboBox;
    FOKBtn             : TButton;
    FCancelBtn         : TButton;
    FYesBtn            : TButton;
    FNoBtn             : TButton;
    FDuplicateLbl      : TLabel;
    FExistingStr       : TStringList;
    FSelectedNr        : integer;
    procedure OnOKBtnClick (Sender: TObject);
    procedure OnYesBtnClick (Sender: TObject);
    procedure OnCancelBtnClick (Sender: TObject);
    procedure OnControlClick (Sender: TObject);
    procedure ResetControls;
  protected
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
  public
    function LanguageHasChanged: boolean; override;
	  function ShowDroughtRestrictionDialog (const AUsedDroughtRestrictions : WideString;
                                           AExistNew, AShowAll : integer): integer;
  end;


  TVNVGroundWaterDialog = class(TAbstractForm)
  private
    FScrollBox         : TScrollBox;
    FExistGrp          : TGroupBox;
    FExistRdb          : TRadioButton;
    FNewRdb            : TRadioButton;
    FShowDuplicatesChx : TCheckBox;
    FNamesCbx          : TComboBox;
    FOKBtn             : TButton;
    FCancelBtn         : TButton;
    FYesBtn            : TButton;
    FNoBtn             : TButton;
    FDuplicateLbl      : TLabel;
    FExistingStr       : TStringList;
    FSelectedNr        : integer;
    procedure OnOKBtnClick (Sender: TObject);
    procedure OnYesBtnClick (Sender: TObject);
    procedure OnCancelBtnClick (Sender: TObject);
    procedure OnControlClick (Sender: TObject);
    procedure ResetControls;
  protected
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
  public
    function LanguageHasChanged: boolean; override;
	  function ShowGroundWaterDialog (const AExistingGroundWater : WideString;
                                   var AExistNew             : integer;
                                   var ADuplicates           : integer): integer;
  end;

  TVNVPowerPlantDialog = class(TAbstractForm)
  private
    FScrollBox         : TScrollBox;
    FExistGrp          : TGroupBox;
    FExistRdb          : TRadioButton;
    FNewRdb            : TRadioButton;
    FShowDuplicatesChx : TCheckBox;
    FNamesCbx          : TComboBox;
    FOKBtn             : TButton;
    FCancelBtn         : TButton;
    FYesBtn            : TButton;
    FNoBtn             : TButton;
    FDuplicateLbl      : TLabel;
    FExistingStr       : TStringList;
    FSelectedNr        : integer;
    procedure OnOKBtnClick (Sender: TObject);
    procedure OnYesBtnClick (Sender: TObject);
    procedure OnCancelBtnClick (Sender: TObject);
    procedure OnControlClick (Sender: TObject);
    procedure ResetControls;
  protected
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
  public
    function LanguageHasChanged: boolean; override;
	  function ShowPowerPlantDialog (const AExistingPowerPlants : WideString;
                                   var AExistNew             : integer;
                                   var ADuplicates           : integer): integer;
  end;

implementation
uses
	VCL.Dialogs,
  Windows,
  StrUtils,
  VCL.Graphics,
  UConstants,
  UErrorHandlingOperations;

{******************************************************************************}
{* TVNVReservoirDialog                                                        *}
{******************************************************************************}

procedure TVNVReservoirDialog.CreateMemberObjects;
const OPNAME = 'TVNVReservoirDialog.CreateMemberObjects';
begin
	try
    FSelectedNr     := -1;
    FExistingStr    := TStringList.Create;

    Position     := poDesktopCenter;
    ClientHeight := 200;
    ClientWidth  := 350;

    FScrollBox := TScrollBox.Create(Self);
    FScrollBox.Parent     := Self;
    FScrollBox.Left       := 0;
    FScrollBox.Top        := 0;
    FScrollBox.Align      := alClient;
    FScrollBox.BevelInner := bvNone;

    FExistGrp        := TGroupBox.Create(Self);
    FExistGrp.Parent := FScrollBox;
    FExistGrp.Left   := 10;
    FExistGrp.Top    := 5;
    FExistGrp.Width  := 250;
    FExistGrp.Height := 30;

    FExistRdb := TRadioButton.Create(Self);
    FExistRdb.Parent  := FExistGrp;
    FExistRdb.Left    := 10;
    FExistRdb.Top     := 10;
    FExistRdb.Width   := 100;
    FExistRdb.OnClick := OnControlClick;

    FNewRdb := TRadioButton.Create(Self);
    FNewRdb.Parent  := FExistGrp;
    FNewRdb.Left    := 120;
    FNewRdb.Top     := 10;
    FNewRdb.Width   := 120;
    FNewRdb.OnClick := OnControlClick;

    FShowDuplicatesChx := TCheckBox.Create(Self);
    FShowDuplicatesChx.Parent  := FScrollBox;
    FShowDuplicatesChx.OnClick := OnControlClick;
    FShowDuplicatesChx.Width   := 250;
    FShowDuplicatesChx.Top     := 40;
    FShowDuplicatesChx.Left    := 10;

//    FNamesCbx        := TComboBox.Create(Self);
    FNamesCbx        := TCheckedComboBox.Create(Self);
    FNamesCbx.Parent := FScrollBox;
    FNamesCbx.Top    := 70;
    FNamesCbx.Width  := 250;
//    FNamesCbx.Style  := csDropDownList;
    FNamesCbx.Left   := 10;

    FDuplicateLbl            := TLabel.Create(Self);
    FDuplicateLbl.Parent     := FScrollBox;
    FDuplicateLbl.AutoSize   := FALSE;
    FDuplicateLbl.Top        := 100;
    FDuplicateLbl.Width      := 250;
    FDuplicateLbl.Left       := 10;
    FDuplicateLbl.Height     := 40;
    FDuplicateLbl.Font.Color := clRed;
    FDuplicateLbl.Font.Style := [fsBold];
    FDuplicateLbl.Visible    := FALSE;
    FDuplicateLbl.WordWrap   := TRUE;

    FCancelBtn         := TButton.Create(Self);
    FCancelBtn.Parent  := FScrollBox;
    FCancelBtn.OnClick := OnCancelBtnClick;
    FCancelBtn.Left    := 10;
    FCancelBtn.Top     := 160;
    FCancelBtn.Width   := 60;

    FOKBtn             := TButton.Create(Self);
    FOKBtn.Parent      := FScrollBox;
    FOKBtn.OnClick     := OnOKBtnClick;
    FOKBtn.Left        := 75;
    FOKBtn.Top         := 160;
    FOKBtn.Width       := 60;

    FYesBtn            := TButton.Create(Self);
    FYesBtn.Parent     := FScrollBox;
    FYesBtn.OnClick    := OnYesBtnClick;
    FYesBtn.Left       := 140;
    FYesBtn.Top        := 160;
    FYesBtn.Width      := 60;
    FYesBtn.Visible    := FALSE;

    FNoBtn             := TButton.Create(Self);
    FNoBtn.Parent      := FScrollBox;
    FNoBtn.OnClick     := OnCancelBtnClick;
    FNoBtn.Left        := 205;
    FNoBtn.Top         := 160;
    FNoBtn.Width       := 60;
    FNoBtn.Visible     := FALSE;

  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TVNVReservoirDialog.DestroyMemberObjects;
const OPNAME = 'TVNVReservoirDialog.DestroyMemberObjects';
begin
	try
    FreeAndNil(FExistingStr);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TVNVReservoirDialog.LanguageHasChanged: boolean;
const OPNAME = 'TVNVReservoirDialog.LanguageHasChanged';
begin
  Result := inherited LanguageHasChanged;
	try
    Caption                    := FAppModules.Language.GetString('VNV.DlgCaptionReservoir');
    FExistRdb.Caption          := FAppModules.Language.GetString('VNV.UseExisting');
    FNewRdb.Caption            := FAppModules.Language.GetString('VNV.CreateNew');
    FShowDuplicatesChx.Caption := FAppModules.Language.GetString('VNV.InclExistReservoirs');
    FCancelBtn.Caption         := FAppModules.Language.GetString('ButtonCaption.Cancel');
    FOKBtn.Caption             := FAppModules.Language.GetString('ButtonCaption.OK');
    FYesBtn.Caption            := FAppModules.Language.GetString('LabelText.Yes');
    FNoBtn.Caption             := FAppModules.Language.GetString('LabelText.No');
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TVNVReservoirDialog.ShowReservoirsDialog (const AExistingReservoirs : WideString;
                                                   var   AExistNew           : integer;
                                                   var   ADuplicates         : integer): integer;
const OPNAME = 'TVNVReservoirDialog.ShowReservoirsDialog';
begin
  Result := -1;
	try
    FExistingStr.CommaText     := AExistingReservoirs;
    FExistRdb.Checked          := AExistNew = 0;
    FNewRdb.Checked            := AExistNew = 1;
    FShowDuplicatesChx.Checked := ADuplicates = 1;
    if (ShowModal = mrOk) then
    begin
      if (FExistRdb.Checked) then
        AExistNew := 0
      else
        AExistNew := 1;
      if (FShowDuplicatesChx.Checked) then
        ADuplicates := 1
      else
        ADuplicates := 0;
      if (FSelectedNr <> -1) then
        Result := FSelectedNr;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TVNVReservoirDialog.ResetControls;
const OPNAME = 'TVNVReservoirDialog.ResetControls';
var
  lIndex        : integer;
  lTempStr      : string;
  lReservoir    : IReservoirData;
  lReservoirLst : IReservoirDataList;
  lReservoirNr  : integer;
  lResIdx       : integer;
begin
	try
    FShowDuplicatesChx.Enabled := FExistRdb.Checked;
    FNamesCbx.Enabled          := FExistRdb.Checked;

    if (FExistRdb.Checked) then
    begin
      FNamesCbx.Items.Clear;
      lReservoirLst := (FAppModules.Model.ModelData as IYieldModelData).NetworkElementData.ReservoirList;
      for lIndex := 0 to lReservoirLst.ReservoirCount - 1 do
      begin
        lReservoir   := lReservoirLst.ReservoirByIndex[lIndex];
        if(lReservoir <> nil) then
        begin
          lReservoirNr := lReservoir.ReservoirConfigurationData.ReservoirIdentifier;
          // Node with ID 0 is a special node reserved for system use, so don't put it
          if (lReservoir.ReservoirConfigurationData.NodeType = ntReservoir) then
          begin
            lResIdx :=  FExistingStr.IndexOf(IntToStr(lReservoirNr));
            if (FShowDuplicatesChx.Checked) OR (lResIdx < 0) then
            begin
              lTempStr := '(' + IntToStr(lReservoirNr) + ') ' +
                          lReservoir.ReservoirConfigurationData.ReservoirName;
              FNamesCbx.Items.AddObject(lTempStr, TObject(lReservoirNr));
            end;
          end;
        end;
      end;
      FNamesCbx.Sorted := TRUE;
      if (FNamesCbx.Items.Count > 0) then
//        FNamesCbx.ItemIndex := 0;
        FNamesCbx.SetUnCheckedAll(nil);
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TVNVReservoirDialog.OnOKBtnClick(Sender : TObject);
const OPNAME = 'TVNVReservoirDialog.OnOKBtnClick';
var
  //lIndex,
  lCounter  : integer;
  lExistIdx : integer;
  lMsg      : string;
  LReservoir :IReservoirData;
begin
	try
    if (FNewRdb.Checked) then
    begin
      if (NOT (FAppModules.User.UserRights in CUR_EditData)) then
      begin
        lMsg := FAppModules.Language.GetString('VNV.NoUserRights');
        ShowMessage(lMsg);
        ModalResult := mrCancel;
      end
      else if (FAppModules.StudyArea.ScenarioLocked) then
      begin
        lMsg := FAppModules.Language.GetString('VNV.ScenarioLocked');
        ShowMessage(lMsg);
        ModalResult := mrCancel;
      end
      else
      begin
        LReservoir := (FAppModules.Model as IYieldModel).DoCreateReservoir;
        if(LReservoir <> nil) then
        begin
          FSelectedNr := LReservoir.ReservoirConfigurationData.ReservoirIdentifier;
          ModalResult := mrOk;
        end;
      end;
    end
    else
    begin
      //lIndex := FNamesCbx.ItemIndex;
      //lIndex := FNamesCbx.CheckedCount;
      if (FNamesCbx.CheckedCount < 1) then
      begin
        lMsg := FAppModules.Language.GetString('VNV.SelectReservoir');
        ShowMessage(lMsg);
      end
      else
      begin
        for lCounter := 0 to FNamesCbx.Items.Count -1 do
        begin
          if FNamesCbx.IsChecked(lCounter) then
          begin
            FSelectedNr := Integer(FNamesCbx.Items.Objects[lCounter]);
            lExistIdx   :=  FExistingStr.IndexOf(IntToStr(FSelectedNr));
            if (lExistIdx >= 0) then
            begin
              lMsg := FAppModules.Language.GetString('VNV.DuplicateReservoir');
              lMsg := Format(lMsg, [FSelectedNr]);
              FDuplicateLbl.Caption := lMsg;
              FDuplicateLbl.Visible := TRUE;
              FYesBtn.Visible       := TRUE;
              FNoBtn.Visible        := TRUE;
              FOKBtn.Visible        := FALSE;
              FCancelBtn.Visible    := FALSE;
            end
            else
              ModalResult := mrOk;
          end;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TVNVReservoirDialog.OnYesBtnClick(Sender : TObject);
const OPNAME = 'TVNVReservoirDialog.OnYesBtnClick';
begin
  try
    ModalResult := mrOk;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TVNVReservoirDialog.OnCancelBtnClick(Sender : TObject);
const OPNAME = 'TVNVReservoirDialog.OnCancelBtnClick';
begin
	try
    FSelectedNr := -1;
	  ModalResult := mrCancel;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TVNVReservoirDialog.OnControlClick(Sender : TObject);
const OPNAME = 'TVNVReservoirDialog.OnControlClick';
begin
	try
		ResetControls;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

{******************************************************************************}
{* TVNVNodesDialog                                                            *}
{******************************************************************************}

procedure TVNVNodesDialog.CreateMemberObjects;
const OPNAME = 'TVNVNodesDialog.CreateMemberObjects';
begin
	try
    FSelectedNr     := -1;
    FExistingStr    := TStringList.Create;

    Position     := poDesktopCenter;
    ClientHeight := 180;
    ClientWidth  := 350;

    FScrollBox := TScrollBox.Create(Self);
    FScrollBox.Parent     := Self;
    FScrollBox.Left       := 0;
    FScrollBox.Top        := 0;
    FScrollBox.Align      := alClient;
    FScrollBox.BevelInner := bvNone;

    FInflowGrp        := TGroupBox.Create(Self);
    FInflowGrp.Parent := FScrollBox;
    FInflowGrp.Left   := 10;
    FInflowGrp.Top    := 5;
    FInflowGrp.Width  := 250;
    FInflowGrp.Height := 30;

    FWithInflowRdb := TRadioButton.Create(Self);
    FWithInflowRdb.Parent  := FInflowGrp;
    FWithInflowRdb.Left    := 10;
    FWithInflowRdb.Top     := 10;
    FWithInflowRdb.Width   := 100;
    FWithInflowRdb.OnClick := OnControlClick;

    FWithoutInflowRdb := TRadioButton.Create(Self);
    FWithoutInflowRdb.Parent  := FInflowGrp;
    FWithoutInflowRdb.Left    := 120;
    FWithoutInflowRdb.Top     := 10;
    FWithoutInflowRdb.Width   := 100;
    FWithoutInflowRdb.OnClick := OnControlClick;

    FExistGrp        := TGroupBox.Create(Self);
    FExistGrp.Parent := FScrollBox;
    FExistGrp.Left   := 10;
    FExistGrp.Top    := 40;
    FExistGrp.Width  := 250;
    FExistGrp.Height := 30;

    FExistRdb := TRadioButton.Create(Self);
    FExistRdb.Parent  := FExistGrp;
    FExistRdb.Left    := 10;
    FExistRdb.Top     := 10;
    FExistRdb.Width   := 100;
    FExistRdb.OnClick := OnControlClick;

    FNewRdb := TRadioButton.Create(Self);
    FNewRdb.Parent  := FExistGrp;
    FNewRdb.Left    := 120;
    FNewRdb.Top     := 10;
    FNewRdb.Width   := 100;
    FNewRdb.OnClick := OnControlClick;

    FShowDuplicatesChx := TCheckBox.Create(Self);
    FShowDuplicatesChx.Parent  := FScrollBox;
    FShowDuplicatesChx.OnClick := OnControlClick;
    FShowDuplicatesChx.Width   := 250;
    FShowDuplicatesChx.Top     := 80;
    FShowDuplicatesChx.Left    := 10;

    FNamesCbx        := TComboBox.Create(Self);
    FNamesCbx.Parent := FScrollBox;
    FNamesCbx.Top    := 110;
    FNamesCbx.Width  := 250;
    FNamesCbx.Style  := csDropDownList;
    FNamesCbx.Left   := 10;

    FDuplicateLbl            := TLabel.Create(Self);
    FDuplicateLbl.Parent     := FScrollBox;
    FDuplicateLbl.AutoSize   := FALSE;
    FDuplicateLbl.Top        := 140;
    FDuplicateLbl.Width      := 250;
    FDuplicateLbl.Left       := 10;
    FDuplicateLbl.Height     := 40;
    FDuplicateLbl.Font.Color := clRed;
    FDuplicateLbl.Font.Style := [fsBold];
    FDuplicateLbl.Visible    := FALSE;
    FDuplicateLbl.WordWrap   := TRUE;

    FCancelBtn         := TButton.Create(Self);
    FCancelBtn.Parent  := FScrollBox;
    FCancelBtn.OnClick := OnCancelBtnClick;
    FCancelBtn.Left    := 10;
    FCancelBtn.Top     := 200;
    FCancelBtn.Width   := 60;

    FOKBtn             := TButton.Create(Self);
    FOKBtn.Parent      := FScrollBox;
    FOKBtn.OnClick     := OnOKBtnClick;
    FOKBtn.Left        := 75;
    FOKBtn.Top         := 200;
    FOKBtn.Width       := 60;

    FYesBtn            := TButton.Create(Self);
    FYesBtn.Parent     := FScrollBox;
    FYesBtn.OnClick    := OnYesBtnClick;
    FYesBtn.Left       := 140;
    FYesBtn.Top        := 200;
    FYesBtn.Width      := 60;
    FYesBtn.Visible    := FALSE;

    FNoBtn             := TButton.Create(Self);
    FNoBtn.Parent      := FScrollBox;
    FNoBtn.OnClick     := OnCancelBtnClick;
    FNoBtn.Left        := 205;
    FNoBtn.Top         := 200;
    FNoBtn.Width       := 60;
    FNoBtn.Visible     := FALSE;

  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TVNVNodesDialog.DestroyMemberObjects;
const OPNAME = 'TVNVNodesDialog.DestroyMemberObjects';
begin
	try
    FreeAndNil(FExistingStr);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TVNVNodesDialog.LanguageHasChanged: boolean;
const OPNAME = 'TVNVNodesDialog.LanguageHasChanged';
begin
  Result := inherited LanguageHasChanged;
	try
    Caption                    := FAppModules.Language.GetString('VNV.DlgCaptionNode');
    FWithInflowRdb.Caption     := FAppModules.Language.GetString('VNV.WithInflow');
    FWithoutInflowRdb.Caption  := FAppModules.Language.GetString('VNV.WithoutInflow');
    FExistRdb.Caption          := FAppModules.Language.GetString('VNV.UseExisting');
    FNewRdb.Caption            := FAppModules.Language.GetString('VNV.CreateNew');
    FShowDuplicatesChx.Caption := FAppModules.Language.GetString('VNV.InclExistNodes');
    FCancelBtn.Caption         := FAppModules.Language.GetString('ButtonCaption.Cancel');
    FOKBtn.Caption             := FAppModules.Language.GetString('ButtonCaption.OK');
    FYesBtn.Caption            := FAppModules.Language.GetString('LabelText.Yes');
    FNoBtn.Caption             := FAppModules.Language.GetString('LabelText.No');
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TVNVNodesDialog.ShowNodesDialog (const AExistingNodes : WideString;
                                          var AWithInflow      : integer;
                                          var AExistNew        : integer;
                                          var ADuplicates      : integer): integer;
const OPNAME = 'TVNVNodesDialog.ShowNodesDialog';
begin
  Result := -1;
	try
    FExistingStr.CommaText     := AExistingNodes;
    FExistRdb.Checked          := AExistNew = 0;
    FNewRdb.Checked            := AExistNew = 1;
    FWithInflowRdb.Checked     := AWithInflow = 0;
    FWithoutInflowRdb.Checked  := AWithInflow = 1;
    FShowDuplicatesChx.Checked := ADuplicates = 1;
    if (ShowModal = mrOk) then
    begin
      if (FWithInflowRdb.Checked) then
        AWithInflow := 0
      else
        AWithInflow := 1;
      if (FExistRdb.Checked) then
        AExistNew := 0
      else
        AExistNew := 1;
      if (FShowDuplicatesChx.Checked) then
        ADuplicates := 1
      else
        ADuplicates := 0;
      if (FSelectedNr <> -1) then
        Result := FSelectedNr;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TVNVNodesDialog.ResetControls;
const OPNAME = 'TVNVNodesDialog.ResetControls';
var
  lIndex        : integer;
  lTempStr      : string;
  lReservoir    : IReservoirData;
  lReservoirLst : IReservoirDataList;
  lReservoirNr  : integer;
  lResIdx       : integer;
begin
	try
    FShowDuplicatesChx.Enabled := FExistRdb.Checked;
    FNamesCbx.Enabled          := FExistRdb.Checked;

    if (FExistRdb.Checked) then
    begin
      FNamesCbx.Items.Clear;
      lReservoirLst := (FAppModules.Model.ModelData as IYieldModelData).NetworkElementData.ReservoirList;
      if (FWithInflowRdb.Checked) then
      begin
        for lIndex := 0 to lReservoirLst.NodesWithInflowCount - 1 do
        begin
          lReservoir   := lReservoirLst.NodeWithInflowByIndex[lIndex];
          if(lReservoir <> nil) then
          begin
            lReservoirNr := lReservoir.ReservoirConfigurationData.ReservoirIdentifier;
            if (lReservoirNr <> 0) then
            begin
              lResIdx :=  FExistingStr.IndexOf(IntToStr(lReservoirNr));
              if (FShowDuplicatesChx.Checked) OR (lResIdx < 0) then
              begin
                lTempStr := '(' + IntToStr(lReservoirNr) + ') ' +
                            lReservoir.ReservoirConfigurationData.ReservoirName;
                FNamesCbx.Items.AddObject(lTempStr, TObject(lReservoirNr));
              end;
            end;
          end;
        end;
      end
      else
      begin
        for lIndex := 0 to lReservoirLst.NodesWithoutInflowCount - 1 do
        begin
          lReservoir   := lReservoirLst.NodeWithoutInflowByIndex[lIndex];
          if(lReservoir <> nil) then
          begin
            if(lReservoir.ReservoirConfigurationData.NodeType in [ntMineNode,ntDemandCentreNode, ntBaseFlowNode, ntAbstractionNode,ntCollectionNode]) then
              Continue;

            lReservoirNr := lReservoir.ReservoirConfigurationData.ReservoirIdentifier;
            if (lReservoirNr <> 0) then
            begin
              lResIdx :=  FExistingStr.IndexOf(IntToStr(lReservoirNr));
              if (FShowDuplicatesChx.Checked) OR (lResIdx < 0) then
              begin
                lTempStr := '(' + IntToStr(lReservoirNr) + ') ' +
                            lReservoir.ReservoirConfigurationData.ReservoirName;
                FNamesCbx.Items.AddObject(lTempStr, TObject(lReservoirNr));
              end;
            end;
          end;
        end;
      end;
      FNamesCbx.Sorted := TRUE;
      if (FNamesCbx.Items.Count > 0) then
        FNamesCbx.ItemIndex := 0;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TVNVNodesDialog.OnOKBtnClick(Sender : TObject);
const OPNAME = 'TVNVNodesDialog.OnOKBtnClick';
var
  lIndex    : integer;
  lExistIdx : integer;
  lMsg      : string;
  LReservoir :IReservoirData;
begin
	try
    if (FNewRdb.Checked) then
    begin
      if (NOT (FAppModules.User.UserRights in CUR_EditData)) then
      begin
        lMsg := FAppModules.Language.GetString('VNV.NoUserRights');
        ShowMessage(lMsg);
        ModalResult := mrCancel;
      end
      else if (FAppModules.StudyArea.ScenarioLocked) then
      begin
        lMsg := FAppModules.Language.GetString('VNV.ScenarioLocked');
        ShowMessage(lMsg);
        ModalResult := mrCancel;
      end
      else
      begin
        if (FWithInflowRdb.Checked) then
           LReservoir:= (FAppModules.Model as IYieldModel).DoCreateNodeWithInflow
        else
          LReservoir := (FAppModules.Model as IYieldModel).DoCreateNodeWithoutInflow;

        if(LReservoir <> nil) then
        begin
          FSelectedNr := LReservoir.ReservoirConfigurationData.ReservoirIdentifier;
          ModalResult := mrOk;
        end;
      end;
    end
    else
    begin
      lIndex := FNamesCbx.ItemIndex;
      if (lIndex < 0) then
      begin
        lMsg := FAppModules.Language.GetString('VNV.SelectNode');
        ShowMessage(lMsg);
      end
      else
      begin
        FSelectedNr := Integer(FNamesCbx.Items.Objects[lIndex]);
        lExistIdx   :=  FExistingStr.IndexOf(IntToStr(FSelectedNr));
        if (lExistIdx >= 0) then
        begin
          lMsg := FAppModules.Language.GetString('VNV.DuplicateNode');
          lMsg := Format(lMsg, [FSelectedNr]);
          FDuplicateLbl.Caption := lMsg;
          FDuplicateLbl.Visible := TRUE;
          FYesBtn.Visible       := TRUE;
          FNoBtn.Visible        := TRUE;
          FOKBtn.Visible        := FALSE;
          FCancelBtn.Visible    := FALSE;
        end
        else
          ModalResult := mrOk;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TVNVNodesDialog.OnYesBtnClick(Sender : TObject);
const OPNAME = 'TVNVNodesDialog.OnYesBtnClick';
begin
  try
    ModalResult := mrOk;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TVNVNodesDialog.OnCancelBtnClick(Sender : TObject);
const OPNAME = 'TVNVNodesDialog.OnCancelBtnClick';
begin
	try
    FSelectedNr := -1;
	  ModalResult := mrCancel;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TVNVNodesDialog.OnControlClick(Sender : TObject);
const OPNAME = 'TVNVNodesDialog.OnControlClick';
begin
	try
		ResetControls;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

{******************************************************************************}
{* TVNVChannelsDialog                                                         *}
{******************************************************************************}

procedure TVNVChannelsDialog.CreateMemberObjects;
const OPNAME = 'TVNVChannelsDialog.CreateMemberObjects';
begin
	try
    FSelectedNr     := -1;
    FExistingStr    := TStringList.Create;

    Position     := poDesktopCenter;
    ClientHeight := 180;
    ClientWidth  := 350;

    FScrollBox := TScrollBox.Create(Self);
    FScrollBox.Parent     := Self;
    FScrollBox.Left       := 0;
    FScrollBox.Top        := 0;
    FScrollBox.Align      := alClient;
    FScrollBox.BevelInner := bvNone;

    FExistGrp        := TGroupBox.Create(Self);
    FExistGrp.Parent := FScrollBox;
    FExistGrp.Left   := 10;
    FExistGrp.Top    := 5;
    FExistGrp.Width  := 250;
    FExistGrp.Height := 30;

    FExistRdb := TRadioButton.Create(Self);
    FExistRdb.Parent  := FExistGrp;
    FExistRdb.Left    := 10;
    FExistRdb.Top     := 10;
    FExistRdb.Width   := 100;
    FExistRdb.OnClick := OnControlClick;

    FNewRdb := TRadioButton.Create(Self);
    FNewRdb.Parent  := FExistGrp;
    FNewRdb.Left    := 120;
    FNewRdb.Top     := 10;
    FNewRdb.Width   := 120;
    FNewRdb.OnClick := OnControlClick;

    FShowDuplicatesChx := TCheckBox.Create(Self);
    FShowDuplicatesChx.Parent  := FScrollBox;
    FShowDuplicatesChx.OnClick := OnControlClick;
    FShowDuplicatesChx.Width   := 250;
    FShowDuplicatesChx.Top     := 40;
    FShowDuplicatesChx.Left    := 10;

    FNamesCbx        := TComboBox.Create(Self);
    FNamesCbx.Parent := FScrollBox;
    FNamesCbx.Top    := 70;
    FNamesCbx.Width  := 250;
    FNamesCbx.Style  := csDropDownList;
    FNamesCbx.Left   := 10;

    FDuplicateLbl            := TLabel.Create(Self);
    FDuplicateLbl.Parent     := FScrollBox;
    FDuplicateLbl.AutoSize   := FALSE;
    FDuplicateLbl.Top        := 100;
    FDuplicateLbl.Width      := 250;
    FDuplicateLbl.Left       := 10;
    FDuplicateLbl.Height     := 40;
    FDuplicateLbl.Font.Color := clRed;
    FDuplicateLbl.Font.Style := [fsBold];
    FDuplicateLbl.Visible    := FALSE;
    FDuplicateLbl.WordWrap   := TRUE;

    FCancelBtn         := TButton.Create(Self);
    FCancelBtn.Parent  := FScrollBox;
    FCancelBtn.OnClick := OnCancelBtnClick;
    FCancelBtn.Left    := 10;
    FCancelBtn.Top     := 160;
    FCancelBtn.Width   := 60;

    FOKBtn             := TButton.Create(Self);
    FOKBtn.Parent      := FScrollBox;
    FOKBtn.OnClick     := OnOKBtnClick;
    FOKBtn.Left        := 75;
    FOKBtn.Top         := 160;
    FOKBtn.Width       := 60;

    FYesBtn            := TButton.Create(Self);
    FYesBtn.Parent     := FScrollBox;
    FYesBtn.OnClick    := OnYesBtnClick;
    FYesBtn.Left       := 140;
    FYesBtn.Top        := 160;
    FYesBtn.Width      := 60;
    FYesBtn.Visible    := FALSE;

    FNoBtn             := TButton.Create(Self);
    FNoBtn.Parent      := FScrollBox;
    FNoBtn.OnClick     := OnCancelBtnClick;
    FNoBtn.Left        := 205;
    FNoBtn.Top         := 160;
    FNoBtn.Width       := 60;
    FNoBtn.Visible     := FALSE;

  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TVNVChannelsDialog.DestroyMemberObjects;
const OPNAME = 'TVNVChannelsDialog.DestroyMemberObjects';
begin
	try
    FreeAndNil(FExistingStr);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TVNVChannelsDialog.LanguageHasChanged: boolean;
const OPNAME = 'TVNVChannelsDialog.LanguageHasChanged';
begin
  Result := inherited LanguageHasChanged;
	try
    Caption                    := FAppModules.Language.GetString('VNV.DlgCaptionChannel');
    FExistRdb.Caption          := FAppModules.Language.GetString('VNV.UseExisting');
    FNewRdb.Caption            := FAppModules.Language.GetString('VNV.CreateNew');
    FShowDuplicatesChx.Caption := FAppModules.Language.GetString('VNV.InclExistChannels');
    FCancelBtn.Caption         := FAppModules.Language.GetString('ButtonCaption.Cancel');
    FOKBtn.Caption             := FAppModules.Language.GetString('ButtonCaption.OK');
    FYesBtn.Caption            := FAppModules.Language.GetString('LabelText.Yes');
    FNoBtn.Caption             := FAppModules.Language.GetString('LabelText.No');
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TVNVChannelsDialog.ShowChannelsDialog (const AExistingChannels: WideString;
                                                var AExistNew           : integer;
                                                var ADuplicates         : integer): integer;
const OPNAME = 'TVNVChannelsDialog.ShowChannelsDialog';
begin
  Result := -1;
	try
    FExistingStr.CommaText     := AExistingChannels;
    FExistRdb.Checked          := AExistNew = 0;
    FNewRdb.Checked            := AExistNew = 1;
    FShowDuplicatesChx.Checked := ADuplicates = 1;
    if (ShowModal = mrOk) then
    begin
      if (FExistRdb.Checked) then
        AExistNew := 0
      else
        AExistNew := 1;
      if (FShowDuplicatesChx.Checked) then
        ADuplicates := 1
      else
        ADuplicates := 0;
      if (FSelectedNr <> -1) then
        Result := FSelectedNr;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TVNVChannelsDialog.ResetControls();
const OPNAME = 'TVNVChannelsDialog.ResetControls';
var
  lIndex        : integer;
  lTempStr      : string;
  lChannel      : IGeneralFlowChannel;
  lChannelLst   : IChannelList;
  lChannelNr    : integer;
  lChannelIdx   : integer;
begin
	try
    FShowDuplicatesChx.Enabled := FExistRdb.Checked;
    FNamesCbx.Enabled          := FExistRdb.Checked;

    if (FExistRdb.Checked) then
    begin
      FNamesCbx.Items.Clear;
      lChannelLst := (FAppModules.Model.ModelData as IYieldModelData).NetworkElementData.ChannelList;
      for lIndex := 0 to lChannelLst.ChannelCount - 1 do
      begin
        lChannel    := lChannelLst.ChannelByIndex[lIndex];
        if(lChannel <> nil) then
        begin
          lChannelNr  := lChannel.ChannelNumber;
          lChannelIdx :=  FExistingStr.IndexOf(IntToStr(lChannelNr));
          if (FShowDuplicatesChx.Checked) OR (lChannelIdx < 0) then
          begin
            lTempStr := '(' + IntToStr(lChannelNr) + ') ' +
                        lChannel.ChannelName;
            FNamesCbx.Items.AddObject(lTempStr, TObject(lChannelNr));
          end;
        end;
      end;
      FNamesCbx.Sorted := TRUE;
      if (FNamesCbx.Items.Count > 0) then
        FNamesCbx.ItemIndex := 0;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TVNVChannelsDialog.OnOKBtnClick(Sender : TObject);
const OPNAME = 'TVNVChannelsDialog.OnOKBtnClick';
var
  lIndex    : integer;
  lExistIdx : integer;
  lMsg      : string;
  LChannel:IGeneralFlowChannel;
begin
	try
    if (FNewRdb.Checked) then
    begin
      if (NOT (FAppModules.User.UserRights in CUR_EditData)) then
      begin
        lMsg := FAppModules.Language.GetString('VNV.NoUserRights');
        ShowMessage(lMsg);
        ModalResult := mrCancel;
      end
      else if (FAppModules.StudyArea.ScenarioLocked) then
      begin
        lMsg := FAppModules.Language.GetString('VNV.ScenarioLocked');
        ShowMessage(lMsg);
        ModalResult := mrCancel;
      end
      else
      begin
        LChannel := (FAppModules.Model as IYieldModel).DoCreateChannel(0,0);
        if(LChannel <> nil) then
        begin
          FSelectedNr := LChannel.ChannelNumber;
          ModalResult := mrOk;
        end;
      end;
    end
    else
    begin
      lIndex := FNamesCbx.ItemIndex;
      if (lIndex < 0) then
      begin
        lMsg := FAppModules.Language.GetString('VNV.SelectChannel');
        ShowMessage(lMsg);
      end
      else
      begin
        FSelectedNr := Integer(FNamesCbx.Items.Objects[lIndex]);
        lExistIdx   :=  FExistingStr.IndexOf(IntToStr(FSelectedNr));
        if (lExistIdx >= 0) then
        begin
          lMsg := FAppModules.Language.GetString('VNV.DuplicateChannel');
          lMsg := Format(lMsg, [FSelectedNr]);
          FDuplicateLbl.Caption := lMsg;
          FDuplicateLbl.Visible := TRUE;
          FYesBtn.Visible       := TRUE;
          FNoBtn.Visible        := TRUE;
          FOKBtn.Visible        := FALSE;
          FCancelBtn.Visible    := FALSE;
        end
        else
          ModalResult := mrOk;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TVNVChannelsDialog.OnYesBtnClick(Sender : TObject);
const OPNAME = 'TVNVChannelsDialog.OnYesBtnClick';
begin
  try
    ModalResult := mrOk;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TVNVChannelsDialog.OnCancelBtnClick(Sender : TObject);
const OPNAME = 'TVNVChannelsDialog.OnCancelBtnClick';
begin
	try
    FSelectedNr := -1;
	  ModalResult := mrCancel;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TVNVChannelsDialog.OnControlClick (Sender : TObject);
const OPNAME = 'TVNVChannelsDialog.OnControlClick';
begin
	try
		ResetControls;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

{******************************************************************************}
{* TVNVReservoirPenaltyDialog                                                 *}
{******************************************************************************}

procedure TVNVReservoirPenaltyDialog.CreateMemberObjects;
const OPNAME = 'TVNVReservoirPenaltyDialog.CreateMemberObjects';
begin
	try
    FSelectedNr     := -1;
    FExistingReservoirs := TStringList.Create;
    FExistingPenalties  := TStringList.Create;

    Position     := poDesktopCenter;
    ClientHeight := 200;
    ClientWidth  := 350;

    FScrollBox := TScrollBox.Create(Self);
    FScrollBox.Parent     := Self;
    FScrollBox.Left       := 0;
    FScrollBox.Top        := 0;
    FScrollBox.Align      := alClient;
    FScrollBox.BevelInner := bvNone;

    FShowDuplicatesChx := TCheckBox.Create(Self);
    FShowDuplicatesChx.Parent  := FScrollBox;
    FShowDuplicatesChx.OnClick := OnControlClick;
    FShowDuplicatesChx.Width   := 250;
    FShowDuplicatesChx.Top     := 10;
    FShowDuplicatesChx.Left    := 10;

    FNamesCbx        := TComboBox.Create(Self);
    FNamesCbx.Parent := FScrollBox;
    FNamesCbx.Top    := 40;
    FNamesCbx.Width  := 250;
    FNamesCbx.Style  := csDropDownList;
    FNamesCbx.Left   := 10;

    FDuplicateLbl            := TLabel.Create(Self);
    FDuplicateLbl.Parent     := FScrollBox;
    FDuplicateLbl.AutoSize   := FALSE;
    FDuplicateLbl.Top        := 70;
    FDuplicateLbl.Width      := 250;
    FDuplicateLbl.Left       := 10;
    FDuplicateLbl.Height     := 40;
    FDuplicateLbl.Font.Color := clRed;
    FDuplicateLbl.Font.Style := [fsBold];
    FDuplicateLbl.Visible    := FALSE;
    FDuplicateLbl.WordWrap   := TRUE;

    FCancelBtn         := TButton.Create(Self);
    FCancelBtn.Parent  := FScrollBox;
    FCancelBtn.OnClick := OnCancelBtnClick;
    FCancelBtn.Left    := 10;
    FCancelBtn.Top     := 130;
    FCancelBtn.Width   := 60;

    FOKBtn             := TButton.Create(Self);
    FOKBtn.Parent      := FScrollBox;
    FOKBtn.OnClick     := OnOKBtnClick;
    FOKBtn.Left        := 75;
    FOKBtn.Top         := 130;
    FOKBtn.Width       := 60;

    FYesBtn            := TButton.Create(Self);
    FYesBtn.Parent     := FScrollBox;
    FYesBtn.OnClick    := OnYesBtnClick;
    FYesBtn.Left       := 140;
    FYesBtn.Top        := 130;
    FYesBtn.Width      := 60;
    FYesBtn.Visible    := FALSE;

    FNoBtn             := TButton.Create(Self);
    FNoBtn.Parent      := FScrollBox;
    FNoBtn.OnClick     := OnCancelBtnClick;
    FNoBtn.Left        := 205;
    FNoBtn.Top         := 130;
    FNoBtn.Width       := 60;
    FNoBtn.Visible     := FALSE;

  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TVNVReservoirPenaltyDialog.DestroyMemberObjects;
const OPNAME = 'TVNVReservoirPenaltyDialog.DestroyMemberObjects';
begin
	try
    FreeAndNil(FExistingReservoirs);
    FreeAndNil(FExistingPenalties);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TVNVReservoirPenaltyDialog.LanguageHasChanged: boolean;
const OPNAME = 'TVNVReservoirPenaltyDialog.LanguageHasChanged';
begin
  Result := inherited LanguageHasChanged;
	try
    Caption                    := FAppModules.Language.GetString('VNV.DlgCaptionReservoir');
    FShowDuplicatesChx.Caption := FAppModules.Language.GetString('VNV.InclExistResPenalties');
    FCancelBtn.Caption         := FAppModules.Language.GetString('ButtonCaption.Cancel');
    FOKBtn.Caption             := FAppModules.Language.GetString('ButtonCaption.OK');
    FYesBtn.Caption            := FAppModules.Language.GetString('LabelText.Yes');
    FNoBtn.Caption             := FAppModules.Language.GetString('LabelText.No');
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TVNVReservoirPenaltyDialog.ShowReservoirPenaltiesDialog (const AExistingReservoirs : WideString;
                                                                  const AExistingPenalties  : WideString;
                                                                  var ADuplicates           : integer): integer;
const OPNAME = 'TVNVReservoirPenaltyDialog.ShowReservoirPenaltiesDialog';
begin
  Result := -1;
	try
    FExistingReservoirs.CommaText := AExistingReservoirs;
    FExistingPenalties.CommaText  := AExistingPenalties;
    if (FExistingReservoirs.Count > 0) then
    begin
      FShowDuplicatesChx.Checked := ADuplicates = 1;
      ResetControls;
      if (ShowModal = mrOk) then
      begin
        if (FShowDuplicatesChx.Checked) then
          ADuplicates := 1
        else
          ADuplicates := 0;
        if (FSelectedNr <> -1) then
          Result := FSelectedNr;
      end;
    end
    else
      ShowMessage(FAppModules.Language.GetString('Message.AddReservoirToDraw'));
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TVNVReservoirPenaltyDialog.ResetControls;
const OPNAME = 'TVNVReservoirPenaltyDialog.ResetControls';
var
  lIndex        : integer;
  lTempStr      : string;
  lReservoir    : IReservoirData;
  lReservoirLst : IReservoirDataList;
  lReservoirNr  : integer;
  lResPenIdx    : integer;
begin
	try
    FNamesCbx.Items.Clear;
    lReservoirLst := (FAppModules.Model.ModelData as IYieldModelData).NetworkElementData.ReservoirList;
    for lIndex := 0 to FExistingReservoirs.Count - 1 do
    begin
      lReservoirNr := StrToInt(FExistingReservoirs.Strings[lIndex]);
      lReservoir   := lReservoirLst.ReservoirByIdentifier[lReservoirNr];
      if(lReservoir <> nil) then
      begin
        lResPenIdx   := FExistingPenalties.IndexOf(IntToStr(lReservoirNr));
        if (FShowDuplicatesChx.Checked) OR (lResPenIdx < 0) then
        begin
          lTempStr := '(' + IntToStr(lReservoirNr) + ') ' +
                      lReservoir.ReservoirConfigurationData.ReservoirName;
          FNamesCbx.Items.AddObject(lTempStr, TObject(lReservoirNr));
        end;
      end;
    end;
    FNamesCbx.Sorted := TRUE;
    if (FNamesCbx.Items.Count > 0) then
      FNamesCbx.ItemIndex := 0;
  except on E: Exception do HandleError(E, OPNAME); end;
end;
procedure TVNVReservoirPenaltyDialog.OnOKBtnClick(Sender : TObject);
const OPNAME = 'TVNVReservoirPenaltyDialog.OnOKBtnClick';
var
  lIndex    : integer;
  lExistIdx : integer;
  lMsg      : string;
begin
	try
    lIndex := FNamesCbx.ItemIndex;
    if (lIndex < 0) then
    begin
      lMsg := FAppModules.Language.GetString('VNV.SelectResPenalty');
      ShowMessage(lMsg);
    end
    else
    begin
      FSelectedNr := Integer(FNamesCbx.Items.Objects[lIndex]);
      lExistIdx   := FExistingPenalties.IndexOf(IntToStr(FSelectedNr));
      if (lExistIdx >= 0) then
      begin
        lMsg := FAppModules.Language.GetString('VNV.DuplicateResPenalty');
        lMsg := Format(lMsg, [FSelectedNr]);
        FDuplicateLbl.Caption := lMsg;
        FDuplicateLbl.Visible := TRUE;
        FYesBtn.Visible       := TRUE;
        FNoBtn.Visible        := TRUE;
        FOKBtn.Visible        := FALSE;
        FCancelBtn.Visible    := FALSE;
      end
      else
        ModalResult := mrOk;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TVNVReservoirPenaltyDialog.OnCancelBtnClick(Sender : TObject);
const OPNAME = 'TVNVReservoirPenaltyDialog.OnCancelBtnClick';
begin
	try
    FSelectedNr := -1;
	  ModalResult := mrCancel;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TVNVReservoirPenaltyDialog.OnYesBtnClick(Sender : TObject);
const OPNAME = 'TVNVReservoirPenaltyDialog.OnYesBtnClick';
begin
  try
    ModalResult := mrOk;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TVNVReservoirPenaltyDialog.OnControlClick(Sender : TObject);
const OPNAME = 'TVNVReservoirPenaltyDialog.OnControlClick';
begin
	try
		ResetControls;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

{******************************************************************************}
{* TVNVChannelPenaltyDialog                                                   *}
{******************************************************************************}

procedure TVNVChannelPenaltyDialog.CreateMemberObjects;
const OPNAME = 'TVNVChannelPenaltyDialog.CreateMemberObjects';
begin
	try
    FSelectedNr     := -1;
    FExistingChannels  := TStringList.Create;
    FExistingPenalties := TStringList.Create;

    Position     := poDesktopCenter;
    ClientHeight := 200;
    ClientWidth  := 350;

    FScrollBox := TScrollBox.Create(Self);
    FScrollBox.Parent     := Self;
    FScrollBox.Left       := 0;
    FScrollBox.Top        := 0;
    FScrollBox.Align      := alClient;
    FScrollBox.BevelInner := bvNone;

    FShowDuplicatesChx := TCheckBox.Create(Self);
    FShowDuplicatesChx.Parent  := FScrollBox;
    FShowDuplicatesChx.OnClick := OnControlClick;
    FShowDuplicatesChx.Width   := 250;
    FShowDuplicatesChx.Top     := 10;
    FShowDuplicatesChx.Left    := 10;

    FNamesCbx        := TComboBox.Create(Self);
    FNamesCbx.Parent := FScrollBox;
    FNamesCbx.Top    := 40;
    FNamesCbx.Width  := 250;
    FNamesCbx.Style  := csDropDownList;
    FNamesCbx.Left   := 10;

    FDuplicateLbl            := TLabel.Create(Self);
    FDuplicateLbl.Parent     := FScrollBox;
    FDuplicateLbl.AutoSize   := FALSE;
    FDuplicateLbl.Top        := 70;
    FDuplicateLbl.Width      := 250;
    FDuplicateLbl.Left       := 10;
    FDuplicateLbl.Height     := 40;
    FDuplicateLbl.Font.Color := clRed;
    FDuplicateLbl.Font.Style := [fsBold];
    FDuplicateLbl.Visible    := FALSE;
    FDuplicateLbl.WordWrap   := TRUE;

    FCancelBtn         := TButton.Create(Self);
    FCancelBtn.Parent  := FScrollBox;
    FCancelBtn.OnClick := OnCancelBtnClick;
    FCancelBtn.Left    := 10;
    FCancelBtn.Top     := 130;
    FCancelBtn.Width   := 60;

    FOKBtn             := TButton.Create(Self);
    FOKBtn.Parent      := FScrollBox;
    FOKBtn.OnClick     := OnOKBtnClick;
    FOKBtn.Left        := 75;
    FOKBtn.Top         := 130;
    FOKBtn.Width       := 60;

    FYesBtn            := TButton.Create(Self);
    FYesBtn.Parent     := FScrollBox;
    FYesBtn.OnClick    := OnYesBtnClick;
    FYesBtn.Left       := 140;
    FYesBtn.Top        := 130;
    FYesBtn.Width      := 60;
    FYesBtn.Visible    := FALSE;

    FNoBtn             := TButton.Create(Self);
    FNoBtn.Parent      := FScrollBox;
    FNoBtn.OnClick     := OnCancelBtnClick;
    FNoBtn.Left        := 205;
    FNoBtn.Top         := 130;
    FNoBtn.Width       := 60;
    FNoBtn.Visible     := FALSE;

  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TVNVChannelPenaltyDialog.LanguageHasChanged: boolean;
const OPNAME = 'TVNVChannelPenaltyDialog.LanguageHasChanged';
begin
  Result := inherited LanguageHasChanged;
	try
    Caption                    := FAppModules.Language.GetString('VNV.DlgCaptionChannel');
    FShowDuplicatesChx.Caption := FAppModules.Language.GetString('VNV.InclExistChanPenalties');
    FCancelBtn.Caption         := FAppModules.Language.GetString('ButtonCaption.Cancel');
    FOKBtn.Caption             := FAppModules.Language.GetString('ButtonCaption.OK');
    FYesBtn.Caption            := FAppModules.Language.GetString('LabelText.Yes');
    FNoBtn.Caption             := FAppModules.Language.GetString('LabelText.No');
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TVNVChannelPenaltyDialog.DestroyMemberObjects;
const OPNAME = 'TVNVChannelPenaltyDialog.DestroyMemberObjects';
begin
	try
    FreeAndNil(FExistingChannels);
    FreeAndNil(FExistingPenalties)
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TVNVChannelPenaltyDialog.ShowChannelPenaltiesDialog (const AExistingChannels  : WideString;
                                                              const AExistingPenalties : WideString;
                                                              var ADuplicates          : integer): integer;
const OPNAME = 'TVNVChannelPenaltyDialog.ShowChannelPenaltiesDialog';
begin
  Result := -1;
	try
    FExistingChannels.CommaText := AExistingChannels;
    FExistingPenalties.CommaText := AExistingPenalties;

    if (FExistingChannels.Count > 0) then
    begin
      FShowDuplicatesChx.Checked := ADuplicates = 1;
      ResetControls;
      if (ShowModal = mrOk) then
      begin
        if (FShowDuplicatesChx.Checked) then
          ADuplicates := 1
        else
          ADuplicates := 0;
        if (FSelectedNr <> -1) then
          Result := FSelectedNr;
      end;
    end
    else
      ShowMessage(FAppModules.Language.GetString('Message.AddChannelToDraw'));
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TVNVChannelPenaltyDialog.ResetControls;
const OPNAME = 'TVNVChannelPenaltyDialog.ResetControls';
var
  lIndex       : integer;
  lTempStr     : string;
  lChannel     : IGeneralFlowChannel;
  lChannelLst  : IChannelList;
  lChannelNr   : integer;
  lChanePenIdx : integer;
begin
	try
    FNamesCbx.Items.Clear;
    lChannelLst := (FAppModules.Model.ModelData as IYieldModelData).NetworkElementData.ChannelList;
    for lIndex := 0 to FExistingChannels.Count - 1 do
    begin
      lChannelNr   := StrToInt(FExistingChannels.Strings[lIndex]);
      lChannel     := lChannelLst.ChannelByChannelNumber[lChannelNr];
      if(lChannel <> nil) then
      begin
        lChanePenIdx := FExistingPenalties.IndexOf(IntToStr(lChannelNr));
        if (FShowDuplicatesChx.Checked) OR (lChanePenIdx < 0) then
        begin
          lTempStr   := '(' + IntToStr(lChannelNr) + ') ' + lChannel.ChannelName;
          FNamesCbx.Items.AddObject(lTempStr, TObject(lChannelNr));
        end;
      end;
    end;
    FNamesCbx.Sorted := TRUE;
    if (FNamesCbx.Items.Count > 0) then
      FNamesCbx.ItemIndex := 0;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TVNVChannelPenaltyDialog.OnOKBtnClick(Sender : TObject);
const OPNAME = 'TVNVChannelPenaltyDialog.OnOKBtnClick';
var
  lIndex    : integer;
  lExistIdx : integer;
  lMsg      : string;
begin
	try
    lIndex := FNamesCbx.ItemIndex;
    if (lIndex < 0) then
    begin
      lMsg := FAppModules.Language.GetString('VNV.SelectChanPenalty');
      ShowMessage(lMsg);
    end
    else
    begin
      FSelectedNr := Integer(FNamesCbx.Items.Objects[lIndex]);
      lExistIdx   := FExistingPenalties.IndexOf(IntToStr(FSelectedNr));
      if (lExistIdx >= 0) then
      begin
        lMsg := FAppModules.Language.GetString('VNV.DuplicateChanPenalty');
        lMsg := Format(lMsg, [FSelectedNr]);
        FDuplicateLbl.Caption := lMsg;
        FDuplicateLbl.Visible := TRUE;
        FYesBtn.Visible       := TRUE;
        FNoBtn.Visible        := TRUE;
        FOKBtn.Visible        := FALSE;
        FCancelBtn.Visible    := FALSE;
      end
      else
        ModalResult := mrOk;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TVNVChannelPenaltyDialog.OnCancelBtnClick(Sender : TObject);
const OPNAME = 'TVNVChannelPenaltyDialog.OnCancelBtnClick';
begin
	try
    FSelectedNr := -1;
	  ModalResult := mrCancel;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TVNVChannelPenaltyDialog.OnYesBtnClick(Sender : TObject);
const OPNAME = 'TVNVChannelPenaltyDialog.OnYesBtnClick';
begin
  try
    ModalResult := mrOk;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TVNVChannelPenaltyDialog.OnControlClick(Sender : TObject);
const OPNAME = 'TVNVChannelPenaltyDialog.OnControlClick';
begin
	try
		ResetControls;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

{******************************************************************************}
{* TVNVTextDialog                                                             *}
{******************************************************************************}

procedure TVNVTextDialog.CreateMemberObjects;
const OPNAME = 'TVNVTextDialog.CreateMemberObjects';
begin
	try
    FElementNr   := -1;
    FElementType := '';
    FTextType    := nvtGeneral;
    FReservoirs := TStringList.Create;
    FNodes      := TStringList.Create;
    FChannels   := TStringList.Create;
    FWetlands   := TStringList.Create;

    Position     := poDesktopCenter;
    ClientHeight := 200;
    ClientWidth  := 350;

    FScrollBox := TScrollBox.Create(Self);
    FScrollBox.Parent     := Self;
    FScrollBox.Left       := 0;
    FScrollBox.Top        := 0;
    FScrollBox.Align      := alClient;
    FScrollBox.BevelInner := bvNone;

    FAssignParentChx := TCheckBox.Create(Self);
    FAssignParentChx.Parent  := FScrollBox;
    FAssignParentChx.Width   := 250;
    FAssignParentChx.Top     := 10;
    FAssignParentChx.Left    := 10;

    FReservoirRdb := TRadioButton.Create(Self);
    FReservoirRdb.Parent  := FScrollBox;
    FReservoirRdb.Width   := 80;
    FReservoirRdb.Top     := 35;
    FReservoirRdb.Left    := 5;

    FNodeRdb := TRadioButton.Create(Self);
    FNodeRdb.Parent  := FScrollBox;
    FNodeRdb.Width   := 80;
    FNodeRdb.Top     := 35;
    FNodeRdb.Left    := 90;

    FChannelRdb := TRadioButton.Create(Self);
    FChannelRdb.Parent  := FScrollBox;
    FChannelRdb.Width   := 80;
    FChannelRdb.Top     := 35;
    FChannelRdb.Left    := 170;

    FWetlandRdb := TRadioButton.Create(Self);
    FWetlandRdb.Parent  := FScrollBox;
    FWetlandRdb.Width   := 70;
    FWetlandRdb.Top     := 35;
    FWetlandRdb.Left    := 250;

    FNamesCbx        := TComboBox.Create(Self);
    FNamesCbx.Parent := FScrollBox;
    FNamesCbx.Top    := 60;
    FNamesCbx.Width  := 250;
    FNamesCbx.Style  := csDropDownList;
    FNamesCbx.Left   := 10;

    FTypeLbl            := TLabel.Create(Self);
    FTypeLbl.Parent     := FScrollBox;
    FTypeLbl.AutoSize   := FALSE;
    FTypeLbl.Top        := 90;
    FTypeLbl.Width      := 90;
    FTypeLbl.Left       := 10;
    FTypeLbl.Height     := 21;
    FTypeLbl.Layout     := tlCenter;

    FTypeCbx        := TComboBox.Create(Self);
    FTypeCbx.Parent := FScrollBox;
    FTypeCbx.Top    := 90;
    FTypeCbx.Width  := 150;
    FTypeCbx.Style  := csDropDownList;
    FTypeCbx.Left   := 110;

    FCancelBtn         := TButton.Create(Self);
    FCancelBtn.Parent  := FScrollBox;
    FCancelBtn.Caption := FAppModules.Language.GetString('ButtonCaption.Cancel');
    FCancelBtn.OnClick := OnCancelBtnClick;
    FCancelBtn.Left    := 10;
    FCancelBtn.Top     := 130;
    FCancelBtn.Width   := 60;

    FOKBtn             := TButton.Create(Self);
    FOKBtn.Parent      := FScrollBox;
    FOKBtn.Caption     := FAppModules.Language.GetString('ButtonCaption.OK');
    FOKBtn.OnClick     := OnOKBtnClick;
    FOKBtn.Left        := 75;
    FOKBtn.Top         := 130;
    FOKBtn.Width       := 60;

  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TVNVTextDialog.DestroyMemberObjects;
const OPNAME = 'TVNVTextDialog.DestroyMemberObjects';
begin
	try
    FreeAndNil(FChannels);
    FreeAndNil(FNodes);
    FreeAndNil(FReservoirs);
    FreeAndNil(FWetlands);

  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TVNVTextDialog.LanguageHasChanged: boolean;
const OPNAME = 'TVNVTextDialog.LanguageHasChanged';
begin
  Result := inherited LanguageHasChanged;
	try
    Caption                  := FAppModules.Language.GetString('VNV.AddTextLabel');
    FAssignParentChx.Caption := FAppModules.Language.GetString('VNV.AssignTextToElement');
    FReservoirRdb.Caption    := FAppModules.Language.GetString('VNV.Reservoir');
    FNodeRdb.Caption         := FAppModules.Language.GetString('VNV.Node');
    FChannelRdb.Caption      := FAppModules.Language.GetString('VNV.Channel');
    FWetlandRdb.Caption      := FAppModules.Language.GetString('VNV.Wetland');
    FTypeLbl.Caption         := FAppModules.Language.GetString('VNV.TextType');
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TVNVTextDialog.ShowTextDialog (const AReservoirs  : WideString;
                                        const ANodes       : WideString;
                                        const AChannels    : WideString;
                                        const AWetlands    : WideString;
                                        var   AElementNr   : integer;
                                        var   AElementType : string;
                                        var   ATextType    : TVNVTextType): boolean;
const OPNAME = 'TVNVTextDialog.ShowTextDialog';
begin
  Result := FALSE;
	try
    FReservoirs.CommaText     := AReservoirs;
    FNodes.CommaText          := ANodes;
    FChannels.CommaText       := AChannels;
    FWetlands.CommaText       := AWetlands;
    FAssignParentChx.Checked  := FALSE;

    FChannelRdb.Enabled       := FALSE;
    FNodeRdb.Enabled          := FALSE;
    FReservoirRdb.Enabled     := FALSE;
    FWetlandRdb.Enabled       := False;
    FNamesCbx.Enabled         := FALSE;
    FAssignParentChx.Enabled  := (FReservoirs.Count > 0) OR (FNodes.Count > 0) OR (FChannels.Count > 0) or (FWetlands.Count > 0);
    if (FReservoirs.Count > 0) then
      FReservoirRdb.Checked := TRUE
    else if (FNodes.Count > 0) then
      FNodeRdb.Checked := TRUE
    else if (FChannels.Count > 0) then
      FChannelRdb.Checked := TRUE
    else if (FWetlands.Count > 0) then
      FWetlandRdb.Checked := TRUE;

    FAssignParentChx.OnClick := OnControlClick;
    FReservoirRdb.OnClick    := OnControlClick;
    FNodeRdb.OnClick         := OnControlClick;
    FChannelRdb.OnClick      := OnControlClick;
    FWetlandRdb.OnClick      := OnControlClick;
    FNamesCbx.OnSelect       := OnSelectElement;
    ResetControls;
    if (ShowModal = mrOk) then
    begin
      AElementNr   := FElementNr;
      AElementType := FElementType;
      ATextType    := FTextType;
      if (NOT FAssignParentChx.Checked) OR ((FElementNr <> -1) AND (FElementType <> '')) then
        Result := TRUE;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TVNVTextDialog.ResetControls;
const OPNAME = 'TVNVTextDialog.ResetControls';
var
  lIndex        : integer;
  lTempStr      : string;
  lNode         : IReservoirData;
  lNodeLst      : IReservoirDataList;
  lChannelLst   : IChannelList;
  lChannel      : IGeneralFlowChannel;
  LWetlandList  : IWetlandList;
  LWetland      : IWetland;
  lNumber       : integer;
begin
	try
    FChannelRdb.Enabled   := (FAssignParentChx.Checked) AND (FChannels.Count > 0);
    FNodeRdb.Enabled      := (FAssignParentChx.Checked) AND (FNodes.Count > 0);
    FReservoirRdb.Enabled := (FAssignParentChx.Checked) AND (FReservoirs.Count > 0);
    FWetlandRdb.Enabled   := (FAssignParentChx.Checked) AND (FWetlands.Count > 0);
    FNamesCbx.Enabled     := FAssignParentChx.Checked;
    FTypeCbx.Enabled      := FAssignParentChx.Checked;

    FNamesCbx.Items.Clear;
    if (FReservoirRdb.Checked) then
    begin
      lNodeLst := (FAppModules.Model.ModelData as IYieldModelData).NetworkElementData.ReservoirList;
      for lIndex := 0 to FReservoirs.Count - 1 do
      begin
        lNumber    := StrToInt(FReservoirs.Strings[lIndex]);
        lNode      := lNodeLst.ReservoirByIdentifier[lNumber];
        if(lNode <> nil) then
        begin
          lTempStr   := '(' + IntToStr(lNumber) + ') ' + lNode.ReservoirConfigurationData.ReservoirName;
          FNamesCbx.Items.AddObject(lTempStr, TObject(lNumber));
        end;
      end;
    end
    else if (FNodeRdb.Checked) then
    begin
      lNodeLst := (FAppModules.Model.ModelData as IYieldModelData).NetworkElementData.ReservoirList;
      for lIndex := 0 to FNodes.Count - 1 do
      begin
        lNumber    := StrToInt(FNodes.Strings[lIndex]);
        lNode      := lNodeLst.ReservoirOrNodeByIdentifier[lNumber];
        if(lNode <> nil) then
        begin
          lTempStr   := '(' + IntToStr(lNumber) + ') ' + lNode.ReservoirConfigurationData.ReservoirName;
          FNamesCbx.Items.AddObject(lTempStr, TObject(lNumber));
        end;
      end;
    end
    else if (FChannelRdb.Checked) then
    begin
      lChannelLst := (FAppModules.Model.ModelData as IYieldModelData).NetworkElementData.ChannelList;
      for lIndex := 0 to FChannels.Count - 1 do
      begin
        lNumber := StrToInt(FChannels.Strings[lIndex]);
        lChannel   := lChannelLst.ChannelByChannelNumber[lNumber];
        if(lChannel <> nil) then
        begin
          lTempStr   := '(' + IntToStr(lNumber) + ') ' + lChannel.ChannelName;
          FNamesCbx.Items.AddObject(lTempStr, TObject(lNumber));
        end;
      end;
    end
    else if (FWetlandRdb.Checked) then
    begin
      LWetlandList := (FAppModules.Model.ModelData as IYieldModelData).NetworkFeaturesData.WetlandList;
      for lIndex := 0 to FWetlands.Count - 1 do
      begin
        lNumber   := StrToInt(FWetlands.Strings[lIndex]);
        LWetland  := LWetlandList.WetlandByNodeNumber[lNumber];
        if(LWetland <> nil) then
        begin
          lTempStr   := '(' + IntToStr(lNumber) + ') ' + LWetland.Name;
          FNamesCbx.Items.AddObject(lTempStr, TObject(lNumber));
        end;
      end;
    end;
    FNamesCbx.Sorted := TRUE;
    if (FNamesCbx.Items.Count > 0) then
      FNamesCbx.ItemIndex := 0;

    if (FTypeCbx.Enabled) then
    begin
      if (FReservoirRdb.Checked) then
        PopulateReservoirTextTypes
      else if (FNodeRdb.Checked) then
        PopulateNodeTextTypes
      else if (FChannelRdb.Checked) then
        PopulateChannelTextTypes
      else if (FWetlandRdb.Checked) then
        PopulateReservoirTextTypes;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TVNVTextDialog.PopulateReservoirTextTypes;
const OPNAME = 'TVNVTextDialog.PopulateReservoirTextTypes';
begin
	try
    FTypeCbx.Items.Clear;
    FTypeCbx.Items.AddObject(FAppModules.Language.GetString('VNV.nvtGeneral'),       TObject(nvtGeneral));
    FTypeCbx.Items.AddObject(FAppModules.Language.GetString('VNV.nvtName'),          TObject(nvtName));
    FTypeCbx.Items.AddObject(FAppModules.Language.GetString('VNV.nvtNetBasinRunoff'),TObject(nvtNetBasinRunoff));
    FTypeCbx.Items.AddObject(FAppModules.Language.GetString('VNV.nvtElevation'),     TObject(nvtElevation));
    FTypeCbx.Items.AddObject(FAppModules.Language.GetString('VNV.nvtVolume'),        TObject(nvtVolume));
    FTypeCbx.Items.AddObject(FAppModules.Language.GetString('VNV.nvtRainfall'),      TObject(nvtRainfall));
    FTypeCbx.Items.AddObject(FAppModules.Language.GetString('VNV.nvtEvaporation'),   TObject(nvtEvaporation));
    FTypeCbx.Items.AddObject(FAppModules.Language.GetString('VNV.nvtPercFull'),      TObject(nvtPercFull));

    FTypeCbx.Items.AddObject(FAppModules.Language.GetString('VNV.nvtAvgElevation'),     TObject(nvtAvgElevation));
    FTypeCbx.Items.AddObject(FAppModules.Language.GetString('VNV.nvtAvgVolume'),        TObject(nvtAvgVolume));
    FTypeCbx.Items.AddObject(FAppModules.Language.GetString('VNV.nvtAvgEvaporation'),   TObject(nvtAvgEvaporation));
    FTypeCbx.Items.AddObject(FAppModules.Language.GetString('VNV.nvtAvgRainfall'),      TObject(nvtAvgRainfall));
    FTypeCbx.Items.AddObject(FAppModules.Language.GetString('VNV.nvtAvgNetBasinRunoff'),TObject(nvtAvgNetBasinRunoff));
    FTypeCbx.Items.AddObject(FAppModules.Language.GetString('VNV.nvtReservoirStorageChange'),TObject(nvtReservoirStorageChange));
    FTypeCbx.Items.AddObject(FAppModules.Language.GetString('VNV.nvtTimePeriod'),TObject(nvtTimePeriod));

    FTypeCbx.ItemIndex := 0;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TVNVTextDialog.PopulateNodeTextTypes;
const OPNAME = 'TVNVTextDialog.PopulateNodeTextTypes';
var
  lIndex     : integer;
  lElementNr : integer;
  lNode      : IReservoirData;
begin
	try
    FTypeCbx.Items.Clear;
    FTypeCbx.Items.AddObject(FAppModules.Language.GetString('VNV.nvtGeneral'), TObject(nvtGeneral));
    FTypeCbx.Items.AddObject(FAppModules.Language.GetString('VNV.nvtName'),    TObject(nvtName));

    lIndex := FNamesCbx.ItemIndex;
    if (lIndex >= 0) then
    begin
      lElementNr := Integer(FNamesCbx.Items.Objects[lIndex]);
      lNode      := (FAppModules.Model.ModelData as IYieldModelData).NetworkElementData.
                      ReservoirList.ReservoirOrNodeByIdentifier[lElementNr];
      if (lNode <> nil) AND (lNode.ReservoirConfigurationData.NodeType = ntNodeWithInflow) then
      begin
        FTypeCbx.Items.AddObject(FAppModules.Language.GetString('VNV.nvtNetBasinRunoff'), TObject(nvtNetBasinRunoff));
        FTypeCbx.Items.AddObject(FAppModules.Language.GetString('VNV.nvtAvgNetBasinRunoff'),TObject(nvtAvgNetBasinRunoff));
      end;
    end;
    FTypeCbx.Items.AddObject(FAppModules.Language.GetString('VNV.nvtTimePeriod'),TObject(nvtTimePeriod));

    FTypeCbx.ItemIndex := 0;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TVNVTextDialog.PopulateChannelTextTypes;
const OPNAME = 'TVNVTextDialog.PopulateChannelTextTypes';
var
  lIndex     : integer;
  lElementNr : integer;
  lChannel   : IGeneralFlowChannel;
begin
	try
    FTypeCbx.Items.Clear;
    FTypeCbx.Items.AddObject(FAppModules.Language.GetString('VNV.nvtGeneral'),     TObject(nvtGeneral));
    FTypeCbx.Items.AddObject(FAppModules.Language.GetString('VNV.nvtName'),        TObject(nvtName));
    FTypeCbx.Items.AddObject(FAppModules.Language.GetString('VNV.nvtChannelFlow'), TObject(nvtChannelFlow));
    FTypeCbx.Items.AddObject(FAppModules.Language.GetString('VNV.nvtAvgChannelFlow'), TObject(nvtAvgChannelFlow));

    lIndex := FNamesCbx.ItemIndex;
    if (lIndex >= 0) then
    begin
      lElementNr := Integer(FNamesCbx.Items.Objects[lIndex]);
      lChannel   := (FAppModules.Model.ModelData as IYieldModelData).NetworkElementData.
                      ChannelList.ChannelByChannelNumber[lElementNr];
      if (lChannel <> nil) AND (lChannel.SpecifiedDemandFeature <> nil) then
        FTypeCbx.Items.AddObject(FAppModules.Language.GetString('VNV.nvtDemandFile'), TObject(nvtDemandFile));
    end;
    FTypeCbx.Items.AddObject(FAppModules.Language.GetString('VNV.nvtTimePeriod'),TObject(nvtTimePeriod));

    FTypeCbx.ItemIndex := 0;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TVNVTextDialog.OnControlClick(Sender : TObject);
const OPNAME = 'TVNVTextDialog.OnControlClick';
begin
	try
		ResetControls;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TVNVTextDialog.OnSelectElement(Sender : TObject);
const OPNAME = 'TVNVTextDialog.OnSelectElement';
begin
	try
    if (FReservoirRdb.Checked) then
      PopulateReservoirTextTypes
    else if (FWetlandRdb.Checked) then
      PopulateReservoirTextTypes
    else if (FNodeRdb.Checked) then
      PopulateNodeTextTypes
    else if (FChannelRdb.Checked) then
      PopulateChannelTextTypes;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TVNVTextDialog.OnOKBtnClick(Sender : TObject);
const OPNAME = 'TVNVTextDialog.OnOKBtnClick';
var
  lIndex    : integer;
  lMsg      : string;
begin
	try
    if (FAssignParentChx.Checked) then
    begin
      if (FNamesCbx.ItemIndex < 0) then
      begin
        lMsg := FAppModules.Language.GetString('VNV.SelectNetworkElement');
        ShowMessage(lMsg);
      end
      else
      if (FTypeCbx.ItemIndex < 0) then
      begin
        lMsg := FAppModules.Language.GetString('VisioNetwork.MsgSelectText');
        ShowMessage(lMsg);
      end
      else
      begin
        lIndex     := FNamesCbx.ItemIndex;
        FElementNr := Integer(FNamesCbx.Items.Objects[lIndex]);

        if (FReservoirRdb.Checked) then
          FElementType := 'Reservoir (WRYM)'
        else if (FNodeRdb.Checked) then
          FElementType := 'Node (WRYM)'
        else if (FChannelRdb.Checked) then
          FElementType := 'Channel (WRYM)'
        else if (FWetlandRdb.Checked) then
          FElementType := 'Wetland';

        lIndex    := FTypeCbx.ItemIndex;
        FTextType := TVNVTextType(FTypeCbx.Items.Objects[lIndex]);
      end;
      if (FElementNr <> -1) AND (FElementType <> '') then
        ModalResult := mrOk;
    end
    else
      ModalResult := mrOk;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TVNVTextDialog.OnCancelBtnClick(Sender : TObject);
const OPNAME = 'TVNVTextDialog.OnCancelBtnClick';
begin
	try
    FElementNr   := -1;
    FElementType := '';
    FTextType    := nvtGeneral;
	  ModalResult  := mrCancel;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

{ TVNVIrrigationBlockDialog }

procedure TVNVIrrigationBlockDialog.OnOKBtnClick(Sender: TObject);
const OPNAME = 'TVNVIrrigationBlockDialog.OnOKBtnClick';
var
  lIndex    : integer;
  lExistIdx : integer;
  lMsg      : string;
  LIrrigationBlock:IIrrigationBlock;
begin
	try
    if (FNewRdb.Checked) then
    begin
      if (NOT (FAppModules.User.UserRights in CUR_EditData)) then
      begin
        lMsg := FAppModules.Language.GetString('VNV.NoUserRights');
        ShowMessage(lMsg);
        ModalResult := mrCancel;
      end
      else if (FAppModules.StudyArea.ScenarioLocked) then
      begin
        lMsg := FAppModules.Language.GetString('VNV.ScenarioLocked');
        ShowMessage(lMsg);
        ModalResult := mrCancel;
      end
      else
      begin
        LIrrigationBlock := (FAppModules.Model as IYieldModel).DoCreateIrrigationBlock;
        if(LIrrigationBlock <> nil) then
        begin
          FSelectedNr := LIrrigationBlock.BlockNodeNumber;
          ModalResult := mrOk;
        end;
      end;
    end
    else
    begin
      lIndex := FNamesCbx.ItemIndex;
      if (lIndex < 0) then
      begin
        lMsg := FAppModules.Language.GetString('VNV.SelectIrrigation');
        ShowMessage(lMsg);
      end
      else
      begin
        FSelectedNr := Integer(FNamesCbx.Items.Objects[lIndex]);
        lExistIdx   :=  FExistingStr.IndexOf(IntToStr(FSelectedNr));
        if (lExistIdx >= 0) then
        begin
          lMsg := FAppModules.Language.GetString('VNV.DuplicateIrrBlock');
          lMsg := Format(lMsg, [FSelectedNr]);
          FDuplicateLbl.Caption := lMsg;
          FDuplicateLbl.Visible := TRUE;
          FYesBtn.Visible       := TRUE;
          FNoBtn.Visible        := TRUE;
          FOKBtn.Visible        := FALSE;
          FCancelBtn.Visible    := FALSE;
        end
        else
          ModalResult := mrOk;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TVNVIrrigationBlockDialog.OnYesBtnClick(Sender: TObject);
const OPNAME = 'TVNVIrrigationBlockDialog.OnYesBtnClick';
begin
  try
    ModalResult := mrOk;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TVNVIrrigationBlockDialog.OnCancelBtnClick(Sender: TObject);
const OPNAME = 'TVNVIrrigationBlockDialog.OnCancelBtnClick';
begin
	try
    FSelectedNr := -1;
	  ModalResult := mrCancel;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TVNVIrrigationBlockDialog.OnControlClick(Sender: TObject);
const OPNAME = 'TVNVIrrigationBlockDialog.OnControlClick';
begin
	try
		ResetControls;
  except on E: Exception do HandleError(E, OPNAME); end;
end;


procedure TVNVIrrigationBlockDialog.ResetControls;
const OPNAME = 'TVNVIrrigationBlockDialog.ResetControls';
var
  lIndex        : integer;
  lTempStr      : string;
  lIrrigationBlock     : IIrrigationBlock;
  lIrrigationBlockList : IIrrigationBlockList;
  lIrrigationNr : integer;
  lResIdx       : integer;
begin
	try
    FShowDuplicatesChx.Enabled := FExistRdb.Checked;
    FNamesCbx.Enabled          := FExistRdb.Checked;

    if (FExistRdb.Checked) then
    begin
      FNamesCbx.Items.Clear;
      lIrrigationBlockList := (FAppModules.Model.ModelData as IYieldModelData).NetworkFeaturesData.IrrigationBlockList;
      for lIndex := 0 to lIrrigationBlockList.IrrigationBlockCount - 1 do
      begin
        lIrrigationBlock   := lIrrigationBlockList.IrrigationBlockByIndex[lIndex];
        if(lIrrigationBlock <> nil) then
        begin
          lIrrigationNr := lIrrigationBlock.BlockNodeNumber;
          if (lIrrigationNr <> 0) then
          begin
            lResIdx :=  FExistingStr.IndexOf(IntToStr(lIrrigationNr));
            if (FShowDuplicatesChx.Checked) OR (lResIdx < 0) then
            begin
              lTempStr := '(' + IntToStr(lIrrigationNr) + ') ' +
                          lIrrigationBlock.BlockName;
              FNamesCbx.Items.AddObject(lTempStr, TObject(lIrrigationNr));
            end;
          end;
        end;
      end;
      FNamesCbx.Sorted := TRUE;
      if (FNamesCbx.Items.Count > 0) then
        FNamesCbx.ItemIndex := 0;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TVNVIrrigationBlockDialog.CreateMemberObjects;
const OPNAME = 'TVNVIrrigationBlockDialog.CreateMemberObjects';
begin
	try
    FSelectedNr     := -1;
    FExistingStr    := TStringList.Create;

    Position     := poDesktopCenter;
    ClientHeight := 200;
    ClientWidth  := 350;

    FScrollBox := TScrollBox.Create(Self);
    FScrollBox.Parent     := Self;
    FScrollBox.Left       := 0;
    FScrollBox.Top        := 0;
    FScrollBox.Align      := alClient;
    FScrollBox.BevelInner := bvNone;

    FExistGrp        := TGroupBox.Create(Self);
    FExistGrp.Parent := FScrollBox;
    FExistGrp.Left   := 10;
    FExistGrp.Top    := 5;
    FExistGrp.Width  := 250;
    FExistGrp.Height := 30;

    FExistRdb := TRadioButton.Create(Self);
    FExistRdb.Parent  := FExistGrp;
    FExistRdb.Left    := 10;
    FExistRdb.Top     := 10;
    FExistRdb.Width   := 100;
    FExistRdb.OnClick := OnControlClick;

    FNewRdb := TRadioButton.Create(Self);
    FNewRdb.Parent  := FExistGrp;
    FNewRdb.Left    := 120;
    FNewRdb.Top     := 10;
    FNewRdb.Width   := 120;
    FNewRdb.OnClick := OnControlClick;

    FShowDuplicatesChx := TCheckBox.Create(Self);
    FShowDuplicatesChx.Parent  := FScrollBox;
    FShowDuplicatesChx.OnClick := OnControlClick;
    FShowDuplicatesChx.Width   := 250;
    FShowDuplicatesChx.Top     := 40;
    FShowDuplicatesChx.Left    := 10;

    FNamesCbx        := TComboBox.Create(Self);
    FNamesCbx.Parent := FScrollBox;
    FNamesCbx.Top    := 70;
    FNamesCbx.Width  := 250;
    FNamesCbx.Style  := csDropDownList;
    FNamesCbx.Left   := 10;

    FDuplicateLbl            := TLabel.Create(Self);
    FDuplicateLbl.Parent     := FScrollBox;
    FDuplicateLbl.AutoSize   := FALSE;
    FDuplicateLbl.Top        := 100;
    FDuplicateLbl.Width      := 250;
    FDuplicateLbl.Left       := 10;
    FDuplicateLbl.Height     := 40;
    FDuplicateLbl.Font.Color := clRed;
    FDuplicateLbl.Font.Style := [fsBold];
    FDuplicateLbl.Visible    := FALSE;
    FDuplicateLbl.WordWrap   := TRUE;

    FCancelBtn         := TButton.Create(Self);
    FCancelBtn.Parent  := FScrollBox;
    FCancelBtn.OnClick := OnCancelBtnClick;
    FCancelBtn.Left    := 10;
    FCancelBtn.Top     := 160;
    FCancelBtn.Width   := 60;

    FOKBtn             := TButton.Create(Self);
    FOKBtn.Parent      := FScrollBox;
    FOKBtn.OnClick     := OnOKBtnClick;
    FOKBtn.Left        := 75;
    FOKBtn.Top         := 160;
    FOKBtn.Width       := 60;

    FYesBtn            := TButton.Create(Self);
    FYesBtn.Parent     := FScrollBox;
    FYesBtn.OnClick    := OnYesBtnClick;
    FYesBtn.Left       := 140;
    FYesBtn.Top        := 160;
    FYesBtn.Width      := 60;
    FYesBtn.Visible    := FALSE;

    FNoBtn             := TButton.Create(Self);
    FNoBtn.Parent      := FScrollBox;
    FNoBtn.OnClick     := OnCancelBtnClick;
    FNoBtn.Left        := 205;
    FNoBtn.Top         := 160;
    FNoBtn.Width       := 60;
    FNoBtn.Visible     := FALSE;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TVNVIrrigationBlockDialog.DestroyMemberObjects;
const OPNAME = 'TVNVIrrigationBlockDialog.DestroyMemberObjects';
begin
	try
    FreeAndNil(FExistingStr);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TVNVIrrigationBlockDialog.ShowIrrigationBlockDialog(const AExistingIrrigationBlock: WideString;
         var AExistNew,ADuplicates: integer): integer;
const OPNAME = 'TVNVIrrigationBlockDialog.ShowIrrigationBlockDialog';
begin
  Result := -1;
	try
    FExistingStr.CommaText     := AExistingIrrigationBlock;
    FExistRdb.Checked          := AExistNew = 0;
    FNewRdb.Checked            := AExistNew = 1;
    FShowDuplicatesChx.Checked := ADuplicates = 1;
    if (ShowModal = mrOk) then
    begin
      if (FExistRdb.Checked) then
        AExistNew := 0
      else
        AExistNew := 1;
      if (FShowDuplicatesChx.Checked) then
        ADuplicates := 1
      else
        ADuplicates := 0;
      if (FSelectedNr <> -1) then
        Result := FSelectedNr;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;


function TVNVIrrigationBlockDialog.LanguageHasChanged: boolean;
const OPNAME = 'TVNVIrrigationBlockDialog.LanguageHasChanged';
begin
  Result := inherited LanguageHasChanged;
	try
    Caption                    := FAppModules.Language.GetString('VNV.DlgCaptionIrrigation');
    FExistRdb.Caption          := FAppModules.Language.GetString('VNV.UseExisting');
    FNewRdb.Caption            := FAppModules.Language.GetString('VNV.CreateNew');
    FShowDuplicatesChx.Caption := FAppModules.Language.GetString('VNV.InclExistIrrigation');
    FCancelBtn.Caption         := FAppModules.Language.GetString('ButtonCaption.Cancel');
    FOKBtn.Caption             := FAppModules.Language.GetString('ButtonCaption.OK');
    FYesBtn.Caption            := FAppModules.Language.GetString('LabelText.Yes');
    FNoBtn.Caption             := FAppModules.Language.GetString('LabelText.No');
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

{ TVNVWetlandDialog }

procedure TVNVWetlandDialog.OnOKBtnClick(Sender: TObject);
const OPNAME = 'TVNVWetlandDialog.OnOKBtnClick';
var
  lIndex    : integer;
  lExistIdx : integer;
  lMsg      : string;
  LWetland  : IWetland;
begin
	try
    if (FNewRdb.Checked) then
    begin
      if (NOT (FAppModules.User.UserRights in CUR_EditData)) then
      begin
        lMsg := FAppModules.Language.GetString('VNV.NoUserRights');
        ShowMessage(lMsg);
        ModalResult := mrCancel;
      end
      else if (FAppModules.StudyArea.ScenarioLocked) then
      begin
        lMsg := FAppModules.Language.GetString('VNV.ScenarioLocked');
        ShowMessage(lMsg);
        ModalResult := mrCancel;
      end
      else
      begin
        LWetland    := (FAppModules.Model as IYieldModel).DoCreateWetland;
        if(LWetland <> nil) then
        begin
          FSelectedNr := LWetland.NodeNumber;
          ModalResult := mrOk;
        end;
      end;
    end
    else
    begin
      lIndex := FNamesCbx.ItemIndex;
      if (lIndex < 0) then
      begin
        lMsg := FAppModules.Language.GetString('VNV.SelectWetland');
        ShowMessage(lMsg);
      end
      else
      begin
        FSelectedNr := Integer(FNamesCbx.Items.Objects[lIndex]);
        lExistIdx   :=  FExistingStr.IndexOf(IntToStr(FSelectedNr));
        if (lExistIdx >= 0) then
        begin
          lMsg := FAppModules.Language.GetString('VNV.DuplicateWetland');
          lMsg := Format(lMsg, [FSelectedNr]);
          FDuplicateLbl.Caption := lMsg;
          FDuplicateLbl.Visible := TRUE;
          FYesBtn.Visible       := TRUE;
          FNoBtn.Visible        := TRUE;
          FOKBtn.Visible        := FALSE;
          FCancelBtn.Visible    := FALSE;
        end
        else
          ModalResult := mrOk;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TVNVWetlandDialog.OnYesBtnClick(Sender: TObject);
const OPNAME = 'TVNVWetlandDialog.OnYesBtnClick';
begin
  try
    ModalResult := mrOk;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TVNVWetlandDialog.OnCancelBtnClick(Sender: TObject);
const OPNAME = 'TVNVWetlandDialog.OnCancelBtnClick';
begin
	try
    FSelectedNr := -1;
	  ModalResult := mrCancel;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TVNVWetlandDialog.OnControlClick(Sender: TObject);
const OPNAME = 'TVNVWetlandDialog.OnControlClick';
begin
	try
		ResetControls;
  except on E: Exception do HandleError(E, OPNAME); end;
end;


procedure TVNVWetlandDialog.ResetControls;
const OPNAME = 'TVNVWetlandDialog.ResetControls';
var
  lIndex        : integer;
  lTempStr      : string;
  lWetland      : IWetland;
  lWetlandList  : IWetlandList;
  LWetlandNr    : integer;
  lResIdx       : integer;
begin
	try
    FShowDuplicatesChx.Enabled := FExistRdb.Checked;
    FNamesCbx.Enabled          := FExistRdb.Checked;

    if (FExistRdb.Checked) then
    begin
      FNamesCbx.Items.Clear;
      lWetlandList := (FAppModules.Model.ModelData as IYieldModelData).NetworkFeaturesData.WetlandList;
      for lIndex := 0 to lWetlandList.WetlandCount - 1 do
      begin
        lWetland   := lWetlandList.WetlandByIndex[lIndex];
        if(lWetland <> nil) then
        begin
          LWetlandNr := lWetland.NodeNumber;
          if (LWetlandNr <> 0) then
          begin
            lResIdx :=  FExistingStr.IndexOf(IntToStr(LWetlandNr));
            if (FShowDuplicatesChx.Checked) OR (lResIdx < 0) then
            begin
              lTempStr := '(' + IntToStr(LWetlandNr) + ') ' +
                          lWetland.Name;
              FNamesCbx.Items.AddObject(lTempStr, TObject(LWetlandNr));
            end;
          end;
        end;
      end;
      FNamesCbx.Sorted := TRUE;
      if (FNamesCbx.Items.Count > 0) then
        FNamesCbx.ItemIndex := 0;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TVNVWetlandDialog.CreateMemberObjects;
const OPNAME = 'TVNVWetlandDialog.CreateMemberObjects';
begin
	try
    FSelectedNr     := -1;
    FExistingStr    := TStringList.Create;

    Position     := poDesktopCenter;
    ClientHeight := 200;
    ClientWidth  := 350;

    FScrollBox := TScrollBox.Create(Self);
    FScrollBox.Parent     := Self;
    FScrollBox.Left       := 0;
    FScrollBox.Top        := 0;
    FScrollBox.Align      := alClient;
    FScrollBox.BevelInner := bvNone;

    FExistGrp        := TGroupBox.Create(Self);
    FExistGrp.Parent := FScrollBox;
    FExistGrp.Left   := 10;
    FExistGrp.Top    := 5;
    FExistGrp.Width  := 250;
    FExistGrp.Height := 30;

    FExistRdb := TRadioButton.Create(Self);
    FExistRdb.Parent  := FExistGrp;
    FExistRdb.Left    := 10;
    FExistRdb.Top     := 10;
    FExistRdb.Width   := 100;
    FExistRdb.OnClick := OnControlClick;

    FNewRdb := TRadioButton.Create(Self);
    FNewRdb.Parent  := FExistGrp;
    FNewRdb.Left    := 120;
    FNewRdb.Top     := 10;
    FNewRdb.Width   := 120;
    FNewRdb.OnClick := OnControlClick;

    FShowDuplicatesChx := TCheckBox.Create(Self);
    FShowDuplicatesChx.Parent  := FScrollBox;
    FShowDuplicatesChx.OnClick := OnControlClick;
    FShowDuplicatesChx.Width   := 250;
    FShowDuplicatesChx.Top     := 40;
    FShowDuplicatesChx.Left    := 10;

    FNamesCbx        := TComboBox.Create(Self);
    FNamesCbx.Parent := FScrollBox;
    FNamesCbx.Top    := 70;
    FNamesCbx.Width  := 250;
    FNamesCbx.Style  := csDropDownList;
    FNamesCbx.Left   := 10;

    FDuplicateLbl            := TLabel.Create(Self);
    FDuplicateLbl.Parent     := FScrollBox;
    FDuplicateLbl.AutoSize   := FALSE;
    FDuplicateLbl.Top        := 100;
    FDuplicateLbl.Width      := 250;
    FDuplicateLbl.Left       := 10;
    FDuplicateLbl.Height     := 40;
    FDuplicateLbl.Font.Color := clRed;
    FDuplicateLbl.Font.Style := [fsBold];
    FDuplicateLbl.Visible    := FALSE;
    FDuplicateLbl.WordWrap   := TRUE;

    FCancelBtn         := TButton.Create(Self);
    FCancelBtn.Parent  := FScrollBox;
    FCancelBtn.OnClick := OnCancelBtnClick;
    FCancelBtn.Left    := 10;
    FCancelBtn.Top     := 160;
    FCancelBtn.Width   := 60;

    FOKBtn             := TButton.Create(Self);
    FOKBtn.Parent      := FScrollBox;
    FOKBtn.OnClick     := OnOKBtnClick;
    FOKBtn.Left        := 75;
    FOKBtn.Top         := 160;
    FOKBtn.Width       := 60;

    FYesBtn            := TButton.Create(Self);
    FYesBtn.Parent     := FScrollBox;
    FYesBtn.OnClick    := OnYesBtnClick;
    FYesBtn.Left       := 140;
    FYesBtn.Top        := 160;
    FYesBtn.Width      := 60;
    FYesBtn.Visible    := FALSE;

    FNoBtn             := TButton.Create(Self);
    FNoBtn.Parent      := FScrollBox;
    FNoBtn.OnClick     := OnCancelBtnClick;
    FNoBtn.Left        := 205;
    FNoBtn.Top         := 160;
    FNoBtn.Width       := 60;
    FNoBtn.Visible     := FALSE;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TVNVWetlandDialog.DestroyMemberObjects;
const OPNAME = 'TVNVWetlandDialog.DestroyMemberObjects';
begin
	try
    FreeAndNil(FExistingStr);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TVNVWetlandDialog.ShowWetlandDialog(const AExistingWetland: WideString;
         var AExistNew,ADuplicates: integer): integer;
const OPNAME = 'TVNVWetlandDialog.ShowWetlandDialog';
begin
  Result := -1;
	try
    FExistingStr.CommaText     := AExistingWetland;
    FExistRdb.Checked          := AExistNew = 0;
    FNewRdb.Checked            := AExistNew = 1;
    FShowDuplicatesChx.Checked := ADuplicates = 1;
    if (ShowModal = mrOk) then
    begin
      if (FExistRdb.Checked) then
        AExistNew := 0
      else
        AExistNew := 1;
      if (FShowDuplicatesChx.Checked) then
        ADuplicates := 1
      else
        ADuplicates := 0;
      if (FSelectedNr <> -1) then
        Result := FSelectedNr;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;


function TVNVWetlandDialog.LanguageHasChanged: boolean;
const OPNAME = 'TVNVWetlandDialog.LanguageHasChanged';
begin
  Result := inherited LanguageHasChanged;
	try
    Caption                    := FAppModules.Language.GetString('VNV.DlgCaptionWetland');
    FExistRdb.Caption          := FAppModules.Language.GetString('VNV.UseExisting');
    FNewRdb.Caption            := FAppModules.Language.GetString('VNV.CreateNew');
    FShowDuplicatesChx.Caption := FAppModules.Language.GetString('VNV.InclExistWetland');
    FCancelBtn.Caption         := FAppModules.Language.GetString('ButtonCaption.Cancel');
    FOKBtn.Caption             := FAppModules.Language.GetString('ButtonCaption.OK');
    FYesBtn.Caption            := FAppModules.Language.GetString('LabelText.Yes');
    FNoBtn.Caption             := FAppModules.Language.GetString('LabelText.No');
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

(*----------------------------------------------------------------------------------------------------*)
{ TVNVDemandCentreDialog }

procedure TVNVDemandCentreDialog.OnOKBtnClick(Sender: TObject);
const OPNAME = 'TVNVDemandCentreDialog.OnOKBtnClick';
var
  lIndex        : integer;
  lExistIdx     : integer;
  lMsg          : string;
  lDemandCentre : IYMDemandCentre;
begin
	try
    if (FNewRdb.Checked) then
    begin
      if (NOT (FAppModules.User.UserRights in CUR_EditData)) then
      begin
        lMsg := FAppModules.Language.GetString('VNV.NoUserRights');
        ShowMessage(lMsg);
        ModalResult := mrCancel;
      end
      else if (FAppModules.StudyArea.ScenarioLocked) then
      begin
        lMsg := FAppModules.Language.GetString('VNV.ScenarioLocked');
        ShowMessage(lMsg);
        ModalResult := mrCancel;
      end
      else
      begin
        lDemandCentre    := (FAppModules.Model as IYieldModel).DoCreateYMDemandCentre;
        if(lDemandCentre <> nil) then
        begin
          FSelectedNr := lDemandCentre.NodeNumber; //   Identifier; //LWetland.NodeNumber;
          ModalResult := mrOk;
        end;
      end;
    end
    else
    begin
      lIndex := FNamesCbx.ItemIndex;
      if (lIndex < 0) then
      begin
        lMsg := FAppModules.Language.GetString('VNV.SelectDemandCentre');
        ShowMessage(lMsg);
      end
      else
      begin
        FSelectedNr := Integer(FNamesCbx.Items.Objects[lIndex]);
        lExistIdx   :=  FExistingStr.IndexOf(IntToStr(FSelectedNr));
        if (lExistIdx >= 0) then
        begin
          lMsg := FAppModules.Language.GetString('VNV.DuplicateDemandCentre');
          lMsg := Format(lMsg, [FSelectedNr]);
          FDuplicateLbl.Caption := lMsg;
          FDuplicateLbl.Visible := TRUE;
          FYesBtn.Visible       := TRUE;
          FNoBtn.Visible        := TRUE;
          FOKBtn.Visible        := FALSE;
          FCancelBtn.Visible    := FALSE;
        end
        else
          ModalResult := mrOk;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TVNVDemandCentreDialog.OnYesBtnClick(Sender: TObject);
const OPNAME = 'TVNVDemandCentreDialog.OnYesBtnClick';
begin
  try
    ModalResult := mrOk;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TVNVDemandCentreDialog.OnCancelBtnClick(Sender: TObject);
const OPNAME = 'TVNVDemandCentreDialog.OnCancelBtnClick';
begin
  try
    FSelectedNr := -1;
	  ModalResult := mrCancel;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TVNVDemandCentreDialog.OnControlClick(Sender: TObject);
const OPNAME = 'TVNVDemandCentreDialog.OnControlClick';
begin
  try
		ResetControls;
  except on E: Exception do HandleError(E, OPNAME); end;
end;


procedure TVNVDemandCentreDialog.ResetControls;
const OPNAME = 'TVNVDemandCentreDialog.ResetControls';
var
  lIndex             : integer;
  lTempStr           : string;
  lDemandCentre      : IYMDemandCentre;
  lDemandCentreList  : IYMDemandCentreList;
  LDemandCentreNr    : integer;
  lResIdx            : integer;
begin
  try
    FShowDuplicatesChx.Enabled := FExistRdb.Checked;
    FNamesCbx.Enabled          := FExistRdb.Checked;

    if (FExistRdb.Checked) then
    begin
      FNamesCbx.Items.Clear;
      lDemandCentreList := (FAppModules.Model.ModelData as IYieldModelData).NetworkFeaturesData.YMDemandCentreList;
      for lIndex := 0 to lDemandCentreList.YMDemandCentreCount - 1 do
      begin
        lDemandCentre := lDemandCentreList.YMDemandCentreByIndex[lIndex];
//        lDemandCentre.
        if(lDemandCentre <> nil) then
        begin
          lDemandCentreNr := lDemandCentre.NodeNumber; //jkw check if this is correct
          if (lDemandCentreNr <> 0) then
          begin
            lResIdx :=  FExistingStr.IndexOf(IntToStr(lDemandCentreNr));
            if (FShowDuplicatesChx.Checked) OR (lResIdx < 0) then
            begin
              lTempStr := '(' + IntToStr(lDemandCentreNr) + ') ' +
                          lDemandCentre.Name;
              FNamesCbx.Items.AddObject(lTempStr, TObject(lDemandCentreNr));
            end;
          end;
        end;
      end;
      FNamesCbx.Sorted := TRUE;
      if (FNamesCbx.Items.Count > 0) then
        FNamesCbx.ItemIndex := 0;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TVNVDemandCentreDialog.CreateMemberObjects;
const OPNAME = 'TVNVDemandCentreDialog.CreateMemberObjects';
begin
  try
    FSelectedNr     := -1;
    FExistingStr    := TStringList.Create;

    Position     := poDesktopCenter;
    ClientHeight := 200;
    ClientWidth  := 350;

    FScrollBox := TScrollBox.Create(Self);
    FScrollBox.Parent     := Self;
    FScrollBox.Left       := 0;
    FScrollBox.Top        := 0;
    FScrollBox.Align      := alClient;
    FScrollBox.BevelInner := bvNone;

    FExistGrp        := TGroupBox.Create(Self);
    FExistGrp.Parent := FScrollBox;
    FExistGrp.Left   := 10;
    FExistGrp.Top    := 5;
    FExistGrp.Width  := 250;
    FExistGrp.Height := 30;

    FExistRdb := TRadioButton.Create(Self);
    FExistRdb.Parent  := FExistGrp;
    FExistRdb.Left    := 10;
    FExistRdb.Top     := 10;
    FExistRdb.Width   := 100;
    FExistRdb.OnClick := OnControlClick;

    FNewRdb := TRadioButton.Create(Self);
    FNewRdb.Parent  := FExistGrp;
    FNewRdb.Left    := 120;
    FNewRdb.Top     := 10;
    FNewRdb.Width   := 120;
    FNewRdb.OnClick := OnControlClick;

    FShowDuplicatesChx := TCheckBox.Create(Self);
    FShowDuplicatesChx.Parent  := FScrollBox;
    FShowDuplicatesChx.OnClick := OnControlClick;
    FShowDuplicatesChx.Width   := 250;
    FShowDuplicatesChx.Top     := 40;
    FShowDuplicatesChx.Left    := 10;

    FNamesCbx        := TComboBox.Create(Self);
    FNamesCbx.Parent := FScrollBox;
    FNamesCbx.Top    := 70;
    FNamesCbx.Width  := 250;
    FNamesCbx.Style  := csDropDownList;
    FNamesCbx.Left   := 10;

    FDuplicateLbl            := TLabel.Create(Self);
    FDuplicateLbl.Parent     := FScrollBox;
    FDuplicateLbl.AutoSize   := FALSE;
    FDuplicateLbl.Top        := 100;
    FDuplicateLbl.Width      := 250;
    FDuplicateLbl.Left       := 10;
    FDuplicateLbl.Height     := 40;
    FDuplicateLbl.Font.Color := clRed;
    FDuplicateLbl.Font.Style := [fsBold];
    FDuplicateLbl.Visible    := FALSE;
    FDuplicateLbl.WordWrap   := TRUE;

    FCancelBtn         := TButton.Create(Self);
    FCancelBtn.Parent  := FScrollBox;
    FCancelBtn.OnClick := OnCancelBtnClick;
    FCancelBtn.Left    := 10;
    FCancelBtn.Top     := 160;
    FCancelBtn.Width   := 60;

    FOKBtn             := TButton.Create(Self);
    FOKBtn.Parent      := FScrollBox;
    FOKBtn.OnClick     := OnOKBtnClick;
    FOKBtn.Left        := 75;
    FOKBtn.Top         := 160;
    FOKBtn.Width       := 60;

    FYesBtn            := TButton.Create(Self);
    FYesBtn.Parent     := FScrollBox;
    FYesBtn.OnClick    := OnYesBtnClick;
    FYesBtn.Left       := 140;
    FYesBtn.Top        := 160;
    FYesBtn.Width      := 60;
    FYesBtn.Visible    := FALSE;

    FNoBtn             := TButton.Create(Self);
    FNoBtn.Parent      := FScrollBox;
    FNoBtn.OnClick     := OnCancelBtnClick;
    FNoBtn.Left        := 205;
    FNoBtn.Top         := 160;
    FNoBtn.Width       := 60;
    FNoBtn.Visible     := FALSE;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TVNVDemandCentreDialog.DestroyMemberObjects;
const OPNAME = 'TVNVDemandCentreDialog.DestroyMemberObjects';
begin
  try
    FreeAndNil(FExistingStr);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TVNVDemandCentreDialog.ShowDemandCentreDialog(const AExistingDemandCentre: WideString;
         var AExistNew,ADuplicates: integer): integer;
const OPNAME = 'TVNVDemandCentreDialog.ShowDemandCentreDialog';
begin
  Result := -1;
  try
    FExistingStr.CommaText     := AExistingDemandCentre;
    FExistRdb.Checked          := AExistNew = 0;
    FNewRdb.Checked            := AExistNew = 1;
    FShowDuplicatesChx.Checked := ADuplicates = 1;
    if (ShowModal = mrOk) then
    begin
      if (FExistRdb.Checked) then
        AExistNew := 0
      else
        AExistNew := 1;
      if (FShowDuplicatesChx.Checked) then
        ADuplicates := 1
      else
        ADuplicates := 0;
      if (FSelectedNr <> -1) then
        Result := FSelectedNr;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;


function TVNVDemandCentreDialog.LanguageHasChanged: boolean;
const OPNAME = 'TVNVDemandCentreDialog.LanguageHasChanged';
begin
  Result := inherited LanguageHasChanged;
	try
    Caption                    := FAppModules.Language.GetString('VNV.DlgCaptionDemandCentre');
    FExistRdb.Caption          := FAppModules.Language.GetString('VNV.UseExisting');
    FNewRdb.Caption            := FAppModules.Language.GetString('VNV.CreateNew');
    FShowDuplicatesChx.Caption := FAppModules.Language.GetString('VNV.InclExistDemandCentre');
    FCancelBtn.Caption         := FAppModules.Language.GetString('ButtonCaption.Cancel');
    FOKBtn.Caption             := FAppModules.Language.GetString('ButtonCaption.OK');
    FYesBtn.Caption            := FAppModules.Language.GetString('LabelText.Yes');
    FNoBtn.Caption             := FAppModules.Language.GetString('LabelText.No');
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME); end;
end;



(*----------------------------------------------------------------------------------------------------*)
{ TVNVSubCatchmentDialog }

procedure TVNVSubCatchmentDialog.OnOKBtnClick(Sender: TObject);
const OPNAME = 'TVNVSubCatchmentDialog.OnOKBtnClick';
var
  lIndex        : integer;
  lExistIdx     : integer;
  lMsg          : string;
  lSubCatchment : IStreamFlowReduction;
begin
	try
    if (FNewRdb.Checked) then
    begin
      if (NOT (FAppModules.User.UserRights in CUR_EditData)) then
      begin
        lMsg := FAppModules.Language.GetString('VNV.NoUserRights');
        ShowMessage(lMsg);
        ModalResult := mrCancel;
      end
      else if (FAppModules.StudyArea.ScenarioLocked) then
      begin
        lMsg := FAppModules.Language.GetString('VNV.ScenarioLocked');
        ShowMessage(lMsg);
        ModalResult := mrCancel;
      end
      else
      begin
        lSubCatchment    := (FAppModules.Model as IYieldModel).DoCreateSFRSubCatchment;
        if(lSubCatchment <> nil) then
        begin
          FSelectedNr := lSubCatchment.Identifier;
          lSubCatchment.InflowNodeNumber := FNodeNr;
          ModalResult := mrOk;
        end;
      end;
    end
    else
    begin
      lIndex := FNamesCbx.ItemIndex;
      if (lIndex < 0) then
      begin
        lMsg := FAppModules.Language.GetString('VNV.SelectSubCatchment');
        ShowMessage(lMsg);
      end
      else
      begin
        FSelectedNr := Integer(FNamesCbx.Items.Objects[lIndex]);
        lExistIdx   :=  FExistingStr.IndexOf(IntToStr(FSelectedNr));
        if (lExistIdx >= 0) then
        begin
          lMsg := FAppModules.Language.GetString('VNV.DuplicateSubCatchment');
          lMsg := Format(lMsg, [FSelectedNr]);
          FDuplicateLbl.Caption := lMsg;
          FDuplicateLbl.Visible := TRUE;
          FYesBtn.Visible       := TRUE;
          FNoBtn.Visible        := TRUE;
          FOKBtn.Visible        := FALSE;
          FCancelBtn.Visible    := FALSE;
        end
        else
          ModalResult := mrOk;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TVNVSubCatchmentDialog.OnYesBtnClick(Sender: TObject);
const OPNAME = 'TVNVSubCatchmentDialog.OnYesBtnClick';
begin
  try
    ModalResult := mrOk;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TVNVSubCatchmentDialog.OnCancelBtnClick(Sender: TObject);
const OPNAME = 'TVNVSubCatchmentDialog.OnCancelBtnClick';
begin
  try
    FSelectedNr := -1;
	  ModalResult := mrCancel;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TVNVSubCatchmentDialog.OnControlClick(Sender: TObject);
const OPNAME = 'TVNVSubCatchmentDialog.OnControlClick';
begin
  try
		ResetControls;
  except on E: Exception do HandleError(E, OPNAME); end;
end;


procedure TVNVSubCatchmentDialog.ResetControls;
const OPNAME = 'TVNVSubCatchmentDialog.ResetControls';
var
  lIndex             : integer;
  lTempStr           : string;
//  lResOrNode         : IReservoirData;
//  lResOrNodeList     : IReservoirDataList;
  lSFRAList          : IStreamFlowReductionList;
  lSFRA              : IStreamFlowReduction;
  lSFRANr            : integer;
  lSFRAIdx           : integer;
  lSFRAStringList    : TStringList;
//  lResNr             : integer;
//  lResIdx            : integer;

//  lYieldModelData    : IYieldModelData;
//  lReservoir         : IReservoirData;

begin
  try
    FShowDuplicatesChx.Enabled := FExistRdb.Checked;
    FNamesCbx.Enabled          := FExistRdb.Checked;

    if FCreateOnlyAllowed then
    begin
      FExistRdb.Checked := False;
      FExistRdb.Enabled := False;
      FNewRdb.Enabled := True;
      FNewRdb.Checked := True;
      FShowDuplicatesChx.Enabled := False;
      FNamesCbx.Enabled := False;
      exit;
    end
    else
    begin
      FExistRdb.Checked := True;
      FExistRdb.Enabled := True;
      FNewRdb.Checked := False;
      FNewRdb.Enabled := False;
      FShowDuplicatesChx.Enabled := False;
      FNamesCbx.Enabled := True;
    end;


    if (FExistRdb.Checked) then
    begin
      FNamesCbx.Items.Clear;

      lSFRAList := (FAppModules.Model.ModelData as IYieldModelData).NetworkFeaturesData.StreamFlowReductionList;
      lTempStr := lSFRAList.StreamFlowReductionIDsPerInflowNode(FNodeNr);

      lSFRAStringList := TStringList.Create;
      if lTempStr <> '' then
        lSFRAStringList.CommaText := lTempStr;

      for lIndex := 0 to lSFRAStringList.Count - 1 do
      begin
        lSFRA := lSFRAList.StreamFlowReductionByID[StrToInt(lSFRAStringList.Strings[lIndex])];
        if(lSFRA <> nil) then
        begin
          lSFRANr := lSFRA.Identifier;
          if (lSFRANr <> 0) then
          begin
            lSFRAIdx :=  FExistingStr.IndexOf(IntToStr(lSFRANr));
            if (FShowDuplicatesChx.Checked) OR (lSFRAIdx < 0) then
            begin
              lTempStr := '(' + IntToStr(lSFRANr) + ') ' +
                          lSFRA.SFRName;
              FNamesCbx.Items.AddObject(lTempStr, TObject(lSFRANr));
            end;
          end;
        end;
      end;

      FNamesCbx.Sorted := TRUE;
      if (FNamesCbx.Items.Count > 0) then
        FNamesCbx.ItemIndex := 0;
    end;

  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TVNVSubCatchmentDialog.CreateMemberObjects;
const OPNAME = 'TVNVSubCatchmentDialog.CreateMemberObjects';
begin
  try
    FSelectedNr     := -1;
    FExistingStr    := TStringList.Create;

    Position     := poDesktopCenter;
    ClientHeight := 200;
    ClientWidth  := 350;

    FScrollBox := TScrollBox.Create(Self);
    FScrollBox.Parent     := Self;
    FScrollBox.Left       := 0;
    FScrollBox.Top        := 0;
    FScrollBox.Align      := alClient;
    FScrollBox.BevelInner := bvNone;

    FExistGrp        := TGroupBox.Create(Self);
    FExistGrp.Parent := FScrollBox;
    FExistGrp.Left   := 10;
    FExistGrp.Top    := 5;
    FExistGrp.Width  := 250;
    FExistGrp.Height := 30;

    FExistRdb := TRadioButton.Create(Self);
    FExistRdb.Parent  := FExistGrp;
    FExistRdb.Left    := 10;
    FExistRdb.Top     := 10;
    FExistRdb.Width   := 100;
    FExistRdb.OnClick := OnControlClick;

    FNewRdb := TRadioButton.Create(Self);
    FNewRdb.Parent  := FExistGrp;
    FNewRdb.Left    := 120;
    FNewRdb.Top     := 10;
    FNewRdb.Width   := 120;
    FNewRdb.OnClick := OnControlClick;

    FShowDuplicatesChx := TCheckBox.Create(Self);
    FShowDuplicatesChx.Parent  := FScrollBox;
    FShowDuplicatesChx.OnClick := OnControlClick;
    FShowDuplicatesChx.Width   := 250;
    FShowDuplicatesChx.Top     := 40;
    FShowDuplicatesChx.Left    := 10;

    FNamesCbx        := TComboBox.Create(Self);
    FNamesCbx.Parent := FScrollBox;
    FNamesCbx.Top    := 70;
    FNamesCbx.Width  := 250;
    FNamesCbx.Style  := csDropDownList;
    FNamesCbx.Left   := 10;

    FDuplicateLbl            := TLabel.Create(Self);
    FDuplicateLbl.Parent     := FScrollBox;
    FDuplicateLbl.AutoSize   := FALSE;
    FDuplicateLbl.Top        := 100;
    FDuplicateLbl.Width      := 250;
    FDuplicateLbl.Left       := 10;
    FDuplicateLbl.Height     := 40;
    FDuplicateLbl.Font.Color := clRed;
    FDuplicateLbl.Font.Style := [fsBold];
    FDuplicateLbl.Visible    := FALSE;
    FDuplicateLbl.WordWrap   := TRUE;

    FCancelBtn         := TButton.Create(Self);
    FCancelBtn.Parent  := FScrollBox;
    FCancelBtn.OnClick := OnCancelBtnClick;
    FCancelBtn.Left    := 10;
    FCancelBtn.Top     := 160;
    FCancelBtn.Width   := 60;

    FOKBtn             := TButton.Create(Self);
    FOKBtn.Parent      := FScrollBox;
    FOKBtn.OnClick     := OnOKBtnClick;
    FOKBtn.Left        := 75;
    FOKBtn.Top         := 160;
    FOKBtn.Width       := 60;

    FYesBtn            := TButton.Create(Self);
    FYesBtn.Parent     := FScrollBox;
    FYesBtn.OnClick    := OnYesBtnClick;
    FYesBtn.Left       := 140;
    FYesBtn.Top        := 160;
    FYesBtn.Width      := 60;
    FYesBtn.Visible    := FALSE;

    FNoBtn             := TButton.Create(Self);
    FNoBtn.Parent      := FScrollBox;
    FNoBtn.OnClick     := OnCancelBtnClick;
    FNoBtn.Left        := 205;
    FNoBtn.Top         := 160;
    FNoBtn.Width       := 60;
    FNoBtn.Visible     := FALSE;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TVNVSubCatchmentDialog.DestroyMemberObjects;
const OPNAME = 'TVNVSubCatchmentDialog.DestroyMemberObjects';
begin
  try
    FreeAndNil(FExistingStr);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TVNVSubCatchmentDialog.ShowSubCatchmentDialog(const AExistingSubCatchment: WideString;
         var AExistNew,ADuplicates: integer; ANodeNr  :  integer; AllowOnlyCreate : Boolean): integer;
const OPNAME = 'TVNVSubCatchmentDialog.ShowSubCatchmentDialog';
begin
  Result := -1;
  try
    FExistingStr.CommaText     := AExistingSubCatchment;
    FExistRdb.Checked          := AExistNew = 0;
    FNewRdb.Checked            := AExistNew = 1;
    FNodeNr                    := ANodeNr;
    FCreateOnlyAllowed         := AllowOnlyCreate;
    FShowDuplicatesChx.Checked := ADuplicates = 1;
    ResetControls;
    if (ShowModal = mrOk) then
    begin
      if (FExistRdb.Checked) then
        AExistNew := 0
      else
        AExistNew := 1;
      if (FShowDuplicatesChx.Checked) then
        ADuplicates := 1
      else
        ADuplicates := 0;
      if (FSelectedNr <> -1) then
        Result := FSelectedNr;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;


function TVNVSubCatchmentDialog.LanguageHasChanged: boolean;
const OPNAME = 'TVNVSubCatchmentDialog.LanguageHasChanged';
begin
  Result := inherited LanguageHasChanged;
	try
    Caption                    := FAppModules.Language.GetString('VNV.DlgCaptionSubCatchment');
    FExistRdb.Caption          := FAppModules.Language.GetString('VNV.UseExisting');
    FNewRdb.Caption            := FAppModules.Language.GetString('VNV.CreateNew');
    FShowDuplicatesChx.Caption := FAppModules.Language.GetString('VNV.InclExistSubCatchment');
    FCancelBtn.Caption         := FAppModules.Language.GetString('ButtonCaption.Cancel');
    FOKBtn.Caption             := FAppModules.Language.GetString('ButtonCaption.OK');
    FYesBtn.Caption            := FAppModules.Language.GetString('LabelText.Yes');
    FNoBtn.Caption             := FAppModules.Language.GetString('LabelText.No');
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME); end;
end;


{ TVNVMineDialog }

procedure TVNVMineDialog.CreateMemberObjects;
const OPNAME = 'TVNVMineDialog.CreateMemberObjects';
begin
	try
    FSelectedNr     := -1;
    FExistingStr    := TStringList.Create;

    Position     := poDesktopCenter;
    ClientHeight := 200;
    ClientWidth  := 350;

    FScrollBox := TScrollBox.Create(Self);
    FScrollBox.Parent     := Self;
    FScrollBox.Left       := 0;
    FScrollBox.Top        := 0;
    FScrollBox.Align      := alClient;
    FScrollBox.BevelInner := bvNone;

    FExistGrp        := TGroupBox.Create(Self);
    FExistGrp.Parent := FScrollBox;
    FExistGrp.Left   := 10;
    FExistGrp.Top    := 5;
    FExistGrp.Width  := 250;
    FExistGrp.Height := 30;

    FExistRdb := TRadioButton.Create(Self);
    FExistRdb.Parent  := FExistGrp;
    FExistRdb.Left    := 10;
    FExistRdb.Top     := 10;
    FExistRdb.Width   := 100;
    FExistRdb.OnClick := OnControlClick;

    FNewRdb := TRadioButton.Create(Self);
    FNewRdb.Parent  := FExistGrp;
    FNewRdb.Left    := 120;
    FNewRdb.Top     := 10;
    FNewRdb.Width   := 120;
    FNewRdb.OnClick := OnControlClick;

    FShowDuplicatesChx := TCheckBox.Create(Self);
    FShowDuplicatesChx.Parent  := FScrollBox;
    FShowDuplicatesChx.OnClick := OnControlClick;
    FShowDuplicatesChx.Width   := 250;
    FShowDuplicatesChx.Top     := 40;
    FShowDuplicatesChx.Left    := 10;

    FNamesCbx        := TComboBox.Create(Self);
    FNamesCbx.Parent := FScrollBox;
    FNamesCbx.Top    := 70;
    FNamesCbx.Width  := 250;
    FNamesCbx.Style  := csDropDownList;
    FNamesCbx.Left   := 10;

    FDuplicateLbl            := TLabel.Create(Self);
    FDuplicateLbl.Parent     := FScrollBox;
    FDuplicateLbl.AutoSize   := FALSE;
    FDuplicateLbl.Top        := 100;
    FDuplicateLbl.Width      := 250;
    FDuplicateLbl.Left       := 10;
    FDuplicateLbl.Height     := 40;
    FDuplicateLbl.Font.Color := clRed;
    FDuplicateLbl.Font.Style := [fsBold];
    FDuplicateLbl.Visible    := FALSE;
    FDuplicateLbl.WordWrap   := TRUE;

    FCancelBtn         := TButton.Create(Self);
    FCancelBtn.Parent  := FScrollBox;
    FCancelBtn.OnClick := OnCancelBtnClick;
    FCancelBtn.Left    := 10;
    FCancelBtn.Top     := 160;
    FCancelBtn.Width   := 60;

    FOKBtn             := TButton.Create(Self);
    FOKBtn.Parent      := FScrollBox;
    FOKBtn.OnClick     := OnOKBtnClick;
    FOKBtn.Left        := 75;
    FOKBtn.Top         := 160;
    FOKBtn.Width       := 60;

    FYesBtn            := TButton.Create(Self);
    FYesBtn.Parent     := FScrollBox;
    FYesBtn.OnClick    := OnYesBtnClick;
    FYesBtn.Left       := 140;
    FYesBtn.Top        := 160;
    FYesBtn.Width      := 60;
    FYesBtn.Visible    := FALSE;

    FNoBtn             := TButton.Create(Self);
    FNoBtn.Parent      := FScrollBox;
    FNoBtn.OnClick     := OnCancelBtnClick;
    FNoBtn.Left        := 205;
    FNoBtn.Top         := 160;
    FNoBtn.Width       := 60;
    FNoBtn.Visible     := FALSE;

  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TVNVMineDialog.DestroyMemberObjects;
const OPNAME = 'TVNVMineDialog.DestroyMemberObjects';
begin
	try
    FreeAndNil(FExistingStr);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TVNVMineDialog.LanguageHasChanged: boolean;
const OPNAME = 'TVNVMineDialog.LanguageHasChanged';
begin
  Result := inherited LanguageHasChanged;
	try
    Caption                    := FAppModules.Language.GetString('VNV.DlgCaptionMine');
    FExistRdb.Caption          := FAppModules.Language.GetString('VNV.UseExisting');
    FNewRdb.Caption            := FAppModules.Language.GetString('VNV.CreateNew');
    FShowDuplicatesChx.Caption := FAppModules.Language.GetString('VNV.InclExistMines');
    FCancelBtn.Caption         := FAppModules.Language.GetString('ButtonCaption.Cancel');
    FOKBtn.Caption             := FAppModules.Language.GetString('ButtonCaption.OK');
    FYesBtn.Caption            := FAppModules.Language.GetString('LabelText.Yes');
    FNoBtn.Caption             := FAppModules.Language.GetString('LabelText.No');
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME); end;end;

procedure TVNVMineDialog.OnCancelBtnClick(Sender: TObject);
const OPNAME = 'TVNVMineDialog.OnCancelBtnClick';
begin
	try
    FSelectedNr := -1;
	  ModalResult := mrCancel;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TVNVMineDialog.OnControlClick(Sender: TObject);
const OPNAME = 'TVNVMineDialog.OnControlClick';
begin
	try
		ResetControls;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TVNVMineDialog.OnOKBtnClick(Sender: TObject);
const OPNAME = 'TVNVMineDialog.OnOKBtnClick';
var
  lIndex    : integer;
  lExistIdx : integer;
  lMsg      : string;
  LMine     : IMine;
begin
	try
    if (FNewRdb.Checked) then
    begin
      if (NOT (FAppModules.User.UserRights in CUR_EditData)) then
      begin
        lMsg := FAppModules.Language.GetString('VNV.NoUserRights');
        ShowMessage(lMsg);
        ModalResult := mrCancel;
      end
      else if (FAppModules.StudyArea.ScenarioLocked) then
      begin
        lMsg := FAppModules.Language.GetString('VNV.ScenarioLocked');
        ShowMessage(lMsg);
        ModalResult := mrCancel;
      end
      else
      begin
        LMine := (FAppModules.Model as IYieldModel).DoCreateMine;
        if(LMine <> nil) then
        begin
          FSelectedNr := LMine.NodeNumber;
          ModalResult := mrOk;
        end;
      end;
    end
    else
    begin
      lIndex := FNamesCbx.ItemIndex;
      if (lIndex < 0) then
      begin
        lMsg := FAppModules.Language.GetString('VNV.SelectMine');
        ShowMessage(lMsg);
      end
      else
      begin
        FSelectedNr := Integer(FNamesCbx.Items.Objects[lIndex]);
        lExistIdx   :=  FExistingStr.IndexOf(IntToStr(FSelectedNr));
        if (lExistIdx >= 0) then
        begin
          lMsg := FAppModules.Language.GetString('VNV.DuplicateMine');
          lMsg := Format(lMsg, [FSelectedNr]);
          FDuplicateLbl.Caption := lMsg;
          FDuplicateLbl.Visible := TRUE;
          FYesBtn.Visible       := TRUE;
          FNoBtn.Visible        := TRUE;
          FOKBtn.Visible        := FALSE;
          FCancelBtn.Visible    := FALSE;
        end
        else
          ModalResult := mrOk;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TVNVMineDialog.OnYesBtnClick(Sender: TObject);
const OPNAME = 'TVNVMineDialog.OnYesBtnClick';
begin
  try
    ModalResult := mrOk;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TVNVMineDialog.ResetControls;
const OPNAME = 'TVNVMineDialog.ResetControls';
var
  lIndex        : integer;
  lTempStr      : string;
  lMine         : IMine;
  lMineLst      : IMineList;
  lMineNr       : integer;
  lMineIdx      : integer;
begin
	try
    FShowDuplicatesChx.Enabled := FExistRdb.Checked;
    FNamesCbx.Enabled          := FExistRdb.Checked;

    if (FExistRdb.Checked) then
    begin
      FNamesCbx.Items.Clear;
      lMineLst := (FAppModules.Model.ModelData as IYieldModelData).NetworkFeaturesData.MineList;
      for lIndex := 0 to lMineLst.MineCount - 1 do
      begin
        lMine   := lMineLst.MineByIndex[lIndex];
        if(lMine <> nil) then
        begin
          lMineNr := lMine.NodeNumber;
          // Node with ID 0 is a special node reserved for system use, so don't put it
          if (lMineNr <> 0) then
          begin
            lMineIdx :=  FExistingStr.IndexOf(IntToStr(lMineNr));
            if (FShowDuplicatesChx.Checked) OR (lMineIdx < 0) then
            begin
              lTempStr := '(' + IntToStr(lMineNr) + ') ' + lMine.MineName;
              FNamesCbx.Items.AddObject(lTempStr, TObject(lMineNr));
            end;
          end;
        end;
      end;
      FNamesCbx.Sorted := TRUE;
      if (FNamesCbx.Items.Count > 0) then
        FNamesCbx.ItemIndex := 0;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TVNVMineDialog.ShowMinesDialog(const AExistingMines: WideString;
                                          var AExistNew, ADuplicates: integer): integer;
const OPNAME = 'TVNVMineDialog.ShowMinesDialog';
begin
  Result := -1;
	try
    FExistingStr.CommaText     := AExistingMines;
    FExistRdb.Checked          := AExistNew = 0;
    FNewRdb.Checked            := AExistNew = 1;
    FShowDuplicatesChx.Checked := ADuplicates = 1;
    if (ShowModal = mrOk) then
    begin
      if (FExistRdb.Checked) then
        AExistNew := 0
      else
        AExistNew := 1;
      if (FShowDuplicatesChx.Checked) then
        ADuplicates := 1
      else
        ADuplicates := 0;
      if (FSelectedNr <> -1) then
        Result := FSelectedNr;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

{ TVNVIrrigationAreaDialog }

procedure TVNVIrrigationAreaDialog.CreateMemberObjects;
const OPNAME = 'TVNVIrrigationAreaDialog.CreateMemberObjects';
begin
	try
    FSelectedNr     := -1;
    FExistingStr    := TStringList.Create;

    Position     := poDesktopCenter;
    ClientHeight := 200;
    ClientWidth  := 350;

    FScrollBox := TScrollBox.Create(Self);
    FScrollBox.Parent     := Self;
    FScrollBox.Left       := 0;
    FScrollBox.Top        := 0;
    FScrollBox.Align      := alClient;
    FScrollBox.BevelInner := bvNone;

    FExistGrp        := TGroupBox.Create(Self);
    FExistGrp.Parent := FScrollBox;
    FExistGrp.Left   := 10;
    FExistGrp.Top    := 5;
    FExistGrp.Width  := 250;
    FExistGrp.Height := 30;

    FExistRdb := TRadioButton.Create(Self);
    FExistRdb.Parent  := FExistGrp;
    FExistRdb.Left    := 10;
    FExistRdb.Top     := 10;
    FExistRdb.Width   := 100;
    FExistRdb.OnClick := OnControlClick;

    FNewRdb := TRadioButton.Create(Self);
    FNewRdb.Parent  := FExistGrp;
    FNewRdb.Left    := 120;
    FNewRdb.Top     := 10;
    FNewRdb.Width   := 120;
    FNewRdb.OnClick := OnControlClick;

    FShowDuplicatesChx := TCheckBox.Create(Self);
    FShowDuplicatesChx.Parent  := FScrollBox;
    FShowDuplicatesChx.OnClick := OnControlClick;
    FShowDuplicatesChx.Width   := 250;
    FShowDuplicatesChx.Top     := 40;
    FShowDuplicatesChx.Left    := 10;

    FNamesCbx        := TComboBox.Create(Self);
    FNamesCbx.Parent := FScrollBox;
    FNamesCbx.Top    := 70;
    FNamesCbx.Width  := 250;
    FNamesCbx.Style  := csDropDownList;
    FNamesCbx.Left   := 10;

    FDuplicateLbl            := TLabel.Create(Self);
    FDuplicateLbl.Parent     := FScrollBox;
    FDuplicateLbl.AutoSize   := FALSE;
    FDuplicateLbl.Top        := 100;
    FDuplicateLbl.Width      := 250;
    FDuplicateLbl.Left       := 10;
    FDuplicateLbl.Height     := 40;
    FDuplicateLbl.Font.Color := clRed;
    FDuplicateLbl.Font.Style := [fsBold];
    FDuplicateLbl.Visible    := FALSE;
    FDuplicateLbl.WordWrap   := TRUE;

    FCancelBtn         := TButton.Create(Self);
    FCancelBtn.Parent  := FScrollBox;
    FCancelBtn.OnClick := OnCancelBtnClick;
    FCancelBtn.Left    := 10;
    FCancelBtn.Top     := 160;
    FCancelBtn.Width   := 60;

    FOKBtn             := TButton.Create(Self);
    FOKBtn.Parent      := FScrollBox;
    FOKBtn.OnClick     := OnOKBtnClick;
    FOKBtn.Left        := 75;
    FOKBtn.Top         := 160;
    FOKBtn.Width       := 60;

    FYesBtn            := TButton.Create(Self);
    FYesBtn.Parent     := FScrollBox;
    FYesBtn.OnClick    := OnYesBtnClick;
    FYesBtn.Left       := 140;
    FYesBtn.Top        := 160;
    FYesBtn.Width      := 60;
    FYesBtn.Visible    := FALSE;

    FNoBtn             := TButton.Create(Self);
    FNoBtn.Parent      := FScrollBox;
    FNoBtn.OnClick     := OnCancelBtnClick;
    FNoBtn.Left        := 205;
    FNoBtn.Top         := 160;
    FNoBtn.Width       := 60;
    FNoBtn.Visible     := FALSE;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TVNVIrrigationAreaDialog.DestroyMemberObjects;
const OPNAME = 'TVNVIrrigationAreaDialog.DestroyMemberObjects';
begin
	try
    FreeAndNil(FExistingStr);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TVNVIrrigationAreaDialog.LanguageHasChanged: boolean;
const OPNAME = 'TVNVIrrigationAreaDialog.LanguageHasChanged';
begin
  Result := inherited LanguageHasChanged;
	try
    Caption                    := FAppModules.Language.GetString('VNV.DlgCaptionIrrigationArea');
    FExistRdb.Caption          := FAppModules.Language.GetString('VNV.UseExisting');
    FNewRdb.Caption            := FAppModules.Language.GetString('VNV.CreateNew');
    FShowDuplicatesChx.Caption := FAppModules.Language.GetString('VNV.InclExistIrrigationArea');
    FCancelBtn.Caption         := FAppModules.Language.GetString('ButtonCaption.Cancel');
    FOKBtn.Caption             := FAppModules.Language.GetString('ButtonCaption.OK');
    FYesBtn.Caption            := FAppModules.Language.GetString('LabelText.Yes');
    FNoBtn.Caption             := FAppModules.Language.GetString('LabelText.No');
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TVNVIrrigationAreaDialog.OnCancelBtnClick(Sender: TObject);
const OPNAME = 'TVNVIrrigationAreaDialog.OnCancelBtnClick';
begin
	try
    FSelectedNr := -1;
	  ModalResult := mrCancel;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TVNVIrrigationAreaDialog.OnControlClick(Sender: TObject);
const OPNAME = 'TVNVIrrigationAreaDialog.OnControlClick';
begin
	try
		ResetControls;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TVNVIrrigationAreaDialog.OnOKBtnClick(Sender: TObject);
const OPNAME = 'TVNVIrrigationAreaDialog.OnOKBtnClick';
var
  lIndex          : integer;
  lExistIdx       : integer;
  lMsg            : string;
  LIrrigationArea :IIrrigationArea;
begin
	try
    if (FNewRdb.Checked) then
    begin
      if (NOT (FAppModules.User.UserRights in CUR_EditData)) then
      begin
        lMsg := FAppModules.Language.GetString('VNV.NoUserRights');
        ShowMessage(lMsg);
        ModalResult := mrCancel;
      end
      else if (FAppModules.StudyArea.ScenarioLocked) then
      begin
        lMsg := FAppModules.Language.GetString('VNV.ScenarioLocked');
        ShowMessage(lMsg);
        ModalResult := mrCancel;
      end
      else
      begin
        LIrrigationArea := (FAppModules.Model as IYieldModel).DoCreateIrrigationArea;
        if(LIrrigationArea <> nil) then
        begin
          FSelectedNr := LIrrigationArea.IrrigationNodeNumber;
          ModalResult := mrOk;
        end;
      end;
    end
    else
    begin
      lIndex := FNamesCbx.ItemIndex;
      if (lIndex < 0) then
      begin
        lMsg := FAppModules.Language.GetString('VNV.SelectIrrigationArea');
        ShowMessage(lMsg);
      end
      else
      begin
        FSelectedNr := Integer(FNamesCbx.Items.Objects[lIndex]);
        lExistIdx   :=  FExistingStr.IndexOf(IntToStr(FSelectedNr));
        if (lExistIdx >= 0) then
        begin
          lMsg := FAppModules.Language.GetString('VNV.DuplicateIrrArea');
          lMsg := Format(lMsg, [FSelectedNr]);
          FDuplicateLbl.Caption := lMsg;
          FDuplicateLbl.Visible := TRUE;
          FYesBtn.Visible       := TRUE;
          FNoBtn.Visible        := TRUE;
          FOKBtn.Visible        := FALSE;
          FCancelBtn.Visible    := FALSE;
        end
        else
          ModalResult := mrOk;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TVNVIrrigationAreaDialog.OnYesBtnClick(Sender: TObject);
const OPNAME = 'TVNVIrrigationAreaDialog.OnYesBtnClick';
begin
  try
    ModalResult := mrOk;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TVNVIrrigationAreaDialog.ResetControls;
const OPNAME = 'TVNVIrrigationAreaDialog.ResetControls';
var
  lIndex               : integer;
  lTempStr             : string;
  lIrrigationArea      : IIrrigationArea;
  lIrrigationAreaList  : IIrrigationAreaList;
  lIrrigationAreaNr    : integer;
  lResIdx              : integer;
begin
	try
    FShowDuplicatesChx.Enabled := FExistRdb.Checked;
    FNamesCbx.Enabled          := FExistRdb.Checked;

    if (FExistRdb.Checked) then
    begin
      FNamesCbx.Items.Clear;
      lIrrigationAreaList := (FAppModules.Model.ModelData as IYieldModelData).NetworkFeaturesData.IrrigationAreaList;
      for lIndex := 0 to lIrrigationAreaList.IrrigationAreaCount - 1 do
      begin
        lIrrigationArea   := lIrrigationAreaList.IrrigationAreaByIndex[lIndex];
        if(lIrrigationArea <> nil) then
        begin
          lIrrigationAreaNr := lIrrigationArea.IrrigationNodeNumber;
          if (lIrrigationAreaNr <> 0) then
          begin
            lResIdx :=  FExistingStr.IndexOf(IntToStr(lIrrigationAreaNr));
            if (FShowDuplicatesChx.Checked) OR (lResIdx < 0) then
            begin
              lTempStr := '(' + IntToStr(lIrrigationAreaNr) + ') ' +
                          lIrrigationArea.FeatureName;
              FNamesCbx.Items.AddObject(lTempStr, TObject(lIrrigationAreaNr));
            end;
          end;
        end;
      end;
      FNamesCbx.Sorted := TRUE;
      if (FNamesCbx.Items.Count > 0) then
        FNamesCbx.ItemIndex := 0;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TVNVIrrigationAreaDialog.ShowIrrigationAreaDialog(const AExistingIrrigationArea: WideString; var AExistNew,
                                                                 ADuplicates: integer): integer;
const OPNAME = 'TVNVIrrigationAreaDialog.ShowIrrigationAreaDialog';
begin
  Result := -1;
	try
    FExistingStr.CommaText     := AExistingIrrigationArea;
    FExistRdb.Checked          := AExistNew = 0;
    FNewRdb.Checked            := AExistNew = 1;
    FShowDuplicatesChx.Checked := ADuplicates = 1;
    if (ShowModal = mrOk) then
    begin
      if (FExistRdb.Checked) then
        AExistNew := 0
      else
        AExistNew := 1;
      if (FShowDuplicatesChx.Checked) then
        ADuplicates := 1
      else
        ADuplicates := 0;
      if (FSelectedNr <> -1) then
        Result := FSelectedNr;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

{******************************************************************************}
{* TVNVDroughtRestrictionDialog                                                        *}
{******************************************************************************}

procedure TVNVDroughtRestrictionDialog.CreateMemberObjects;
const OPNAME = 'TVNVDroughtRestrictionDialog.CreateMemberObjects';
begin
	try
    FSelectedNr     := -1;
    FExistingStr    := TStringList.Create;

    Position     := poDesktopCenter;
    ClientHeight := 200;
    ClientWidth  := 350;

    FScrollBox := TScrollBox.Create(Self);
    FScrollBox.Parent     := Self;
    FScrollBox.Left       := 0;
    FScrollBox.Top        := 0;
    FScrollBox.Align      := alClient;
    FScrollBox.BevelInner := bvNone;

    FExistGrp        := TGroupBox.Create(Self);
    FExistGrp.Parent := FScrollBox;
    FExistGrp.Left   := 10;
    FExistGrp.Top    := 5;
    FExistGrp.Width  := 250;
    FExistGrp.Height := 30;

    FExistRdb := TRadioButton.Create(Self);
    FExistRdb.Parent  := FExistGrp;
    FExistRdb.Left    := 10;
    FExistRdb.Top     := 10;
    FExistRdb.Width   := 100;
    FExistRdb.OnClick := OnControlClick;

    FNewRdb := TRadioButton.Create(Self);
    FNewRdb.Parent  := FExistGrp;
    FNewRdb.Left    := 120;
    FNewRdb.Top     := 10;
    FNewRdb.Width   := 120;
    FNewRdb.OnClick := OnControlClick;

    FShowAllChx := TCheckBox.Create(Self);
    FShowAllChx.Parent  := FScrollBox;
    FShowAllChx.OnClick := OnControlClick;
    FShowAllChx.Width   := 250;
    FShowAllChx.Top     := 40;
    FShowAllChx.Left    := 10;

    FNamesCbx        := TComboBox.Create(Self);
    FNamesCbx.Parent := FScrollBox;
    FNamesCbx.Top    := 70;
    FNamesCbx.Width  := 250;
    FNamesCbx.Style  := csDropDownList;
    FNamesCbx.Left   := 10;

    FDuplicateLbl            := TLabel.Create(Self);
    FDuplicateLbl.Parent     := FScrollBox;
    FDuplicateLbl.AutoSize   := FALSE;
    FDuplicateLbl.Top        := 100;
    FDuplicateLbl.Width      := 250;
    FDuplicateLbl.Left       := 10;
    FDuplicateLbl.Height     := 40;
    FDuplicateLbl.Font.Color := clRed;
    FDuplicateLbl.Font.Style := [fsBold];
    FDuplicateLbl.Visible    := FALSE;
    FDuplicateLbl.WordWrap   := TRUE;

    FCancelBtn         := TButton.Create(Self);
    FCancelBtn.Parent  := FScrollBox;
    FCancelBtn.OnClick := OnCancelBtnClick;
    FCancelBtn.Left    := 10;
    FCancelBtn.Top     := 160;
    FCancelBtn.Width   := 60;

    FOKBtn             := TButton.Create(Self);
    FOKBtn.Parent      := FScrollBox;
    FOKBtn.OnClick     := OnOKBtnClick;
    FOKBtn.Left        := 75;
    FOKBtn.Top         := 160;
    FOKBtn.Width       := 60;

    FYesBtn            := TButton.Create(Self);
    FYesBtn.Parent     := FScrollBox;
    FYesBtn.OnClick    := OnYesBtnClick;
    FYesBtn.Left       := 140;
    FYesBtn.Top        := 160;
    FYesBtn.Width      := 60;
    FYesBtn.Visible    := FALSE;

    FNoBtn             := TButton.Create(Self);
    FNoBtn.Parent      := FScrollBox;
    FNoBtn.OnClick     := OnCancelBtnClick;
    FNoBtn.Left        := 205;
    FNoBtn.Top         := 160;
    FNoBtn.Width       := 60;
    FNoBtn.Visible     := FALSE;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TVNVDroughtRestrictionDialog.DestroyMemberObjects;
const OPNAME = 'TVNVDroughtRestrictionDialog.DestroyMemberObjects';
begin
	try
    FreeAndNil(FExistingStr);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TVNVDroughtRestrictionDialog.LanguageHasChanged: boolean;
const OPNAME = 'TVNVDroughtRestrictionDialog.LanguageHasChanged';
begin
  Result := inherited LanguageHasChanged;
	try
    Caption                    := FAppModules.Language.GetString('MenuCaption.DroughtRestriction');
    FExistRdb.Caption          := FAppModules.Language.GetString('VNV.UseExisting');
    FNewRdb.Caption            := FAppModules.Language.GetString('VNV.CreateNew');
    FShowAllChx.Caption := FAppModules.Language.GetString('VNV.InclAllRestriction');
    FCancelBtn.Caption         := FAppModules.Language.GetString('ButtonCaption.Cancel');
    FOKBtn.Caption             := FAppModules.Language.GetString('ButtonCaption.OK');
    FYesBtn.Caption            := FAppModules.Language.GetString('LabelText.Yes');
    FNoBtn.Caption             := FAppModules.Language.GetString('LabelText.No');
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TVNVDroughtRestrictionDialog.ShowDroughtRestrictionDialog (const AUsedDroughtRestrictions : WideString;
                                                                    AExistNew, AShowAll : integer): integer;
const OPNAME = 'TVNVDroughtRestrictionDialog.ShowDroughtRestrictionDialog';
begin
  Result := -1;
	try
    FExistingStr.CommaText     := AUsedDroughtRestrictions;
    FExistRdb.Checked          := AExistNew = 1;
    FShowAllChx.Checked        := AShowAll  = 1;
    FNewRdb.Checked            := AExistNew = 0;

    if (ShowModal = mrOk) then
    begin
      if (FSelectedNr <> -1) then
        Result := FSelectedNr;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TVNVDroughtRestrictionDialog.ResetControls;
const OPNAME = 'TVNVDroughtRestrictionDialog.ResetControls';
var
  lIndex                 : integer;
  lTempStr               : string;
  LDroughtRestriction    : IDroughtRestriction;
  lCurtailmentAndDrought : ICurtailmentAndDrought;
  lDroughtRestrictionNr  : integer;
  lDroughtRestrictionIdx : integer;
begin
	try
    FShowAllChx.Enabled := FExistRdb.Checked;
    FNamesCbx.Enabled   := FExistRdb.Checked;

    if (FExistRdb.Checked) then
    begin
      FNamesCbx.Items.Clear;
      lCurtailmentAndDrought := (FAppModules.Model.ModelData as IYieldModelData).NetworkFeaturesData.CurtailmentAndDrought;
      for lIndex := 0 to lCurtailmentAndDrought.DroughtRestrictionCount - 1 do
      begin
        LDroughtRestriction   := lCurtailmentAndDrought.DroughtRestrictionByIndex[lIndex];
        if(LDroughtRestriction <> nil) then
        begin
          lDroughtRestrictionNr  := LDroughtRestriction.Identifier;
          if FShowAllChx.Checked then
          begin
            lTempStr := '(' + IntToStr(lDroughtRestrictionNr) + ') ' +
                        LDroughtRestriction.DroughtRestrictionName;
            FNamesCbx.Items.AddObject(lTempStr, TObject(lDroughtRestrictionNr));
          end
          else
          begin
            lDroughtRestrictionIdx :=  FExistingStr.IndexOf(IntToStr(lDroughtRestrictionNr));
            if(lDroughtRestrictionIdx >= 0) then
            begin
              lTempStr := '(' + IntToStr(lDroughtRestrictionNr) + ') ' +
                          LDroughtRestriction.DroughtRestrictionName;
              FNamesCbx.Items.AddObject(lTempStr, TObject(lDroughtRestrictionNr));
            end;
          end;
        end;
      end;
      FNamesCbx.Sorted := TRUE;
      if (FNamesCbx.Items.Count > 0) then
        FNamesCbx.ItemIndex := 0;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TVNVDroughtRestrictionDialog.OnOKBtnClick(Sender : TObject);
const OPNAME = 'TVNVDroughtRestrictionDialog.OnOKBtnClick';
var
  lIndex    : integer;
  //lExistIdx : integer;
  lMsg      : string;
  LDroughtRestriction :IDroughtRestriction;
begin
	try
    if (FNewRdb.Checked) then
    begin
      if (NOT (FAppModules.User.UserRights in CUR_EditData)) then
      begin
        lMsg := FAppModules.Language.GetString('VNV.NoUserRights');
        ShowMessage(lMsg);
        ModalResult := mrCancel;
      end
      else if (FAppModules.StudyArea.ScenarioLocked) then
      begin
        lMsg := FAppModules.Language.GetString('VNV.ScenarioLocked');
        ShowMessage(lMsg);
        ModalResult := mrCancel;
      end
      else
      begin
        LDroughtRestriction := (FAppModules.Model as IYieldModel).DoCreateDroughtRestriction;
        if(LDroughtRestriction <> nil) then
        begin
          FSelectedNr := LDroughtRestriction.Identifier;
          ModalResult := mrOk;
        end;
      end;
    end
    else
    begin
      lIndex := FNamesCbx.ItemIndex;
      if (lIndex < 0) then
      begin
        lMsg := FAppModules.Language.GetString('VNV.SelectDroughtRestriction');
        ShowMessage(lMsg);
      end
      else
      begin
        FSelectedNr := Integer(FNamesCbx.Items.Objects[lIndex]);
        {lExistIdx   :=  FExistingStr.IndexOf(IntToStr(FSelectedNr));
        if (lExistIdx >= 0) then
        begin
          lMsg := FAppModules.Language.GetString('VNV.DuplicateReservoir');
          lMsg := Format(lMsg, [FSelectedNr]);
          FDuplicateLbl.Caption := lMsg;
          FDuplicateLbl.Visible := TRUE;
          FYesBtn.Visible       := TRUE;
          FNoBtn.Visible        := TRUE;
          FOKBtn.Visible        := FALSE;
          FCancelBtn.Visible    := FALSE;
        end
        else}
          ModalResult := mrOk;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TVNVDroughtRestrictionDialog.OnYesBtnClick(Sender : TObject);
const OPNAME = 'TVNVDroughtRestrictionDialog.OnYesBtnClick';
begin
  try
    ModalResult := mrOk;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TVNVDroughtRestrictionDialog.OnCancelBtnClick(Sender : TObject);
const OPNAME = 'TVNVDroughtRestrictionDialog.OnCancelBtnClick';
begin
	try
    FSelectedNr := -1;
	  ModalResult := mrCancel;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TVNVDroughtRestrictionDialog.OnControlClick(Sender : TObject);
const OPNAME = 'TVNVDroughtRestrictionDialog.OnControlClick';
begin
	try
		ResetControls;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

{ TVNVGroundWaterDialog }

procedure TVNVGroundWaterDialog.CreateMemberObjects;
const OPNAME = 'TVNVGroundWaterDialog.CreateMemberObjects';
begin
	try
    FSelectedNr     := -1;
    FExistingStr    := TStringList.Create;

    Position     := poDesktopCenter;
    ClientHeight := 200;
    ClientWidth  := 350;

    FScrollBox := TScrollBox.Create(Self);
    FScrollBox.Parent     := Self;
    FScrollBox.Left       := 0;
    FScrollBox.Top        := 0;
    FScrollBox.Align      := alClient;
    FScrollBox.BevelInner := bvNone;

    FExistGrp        := TGroupBox.Create(Self);
    FExistGrp.Parent := FScrollBox;
    FExistGrp.Left   := 10;
    FExistGrp.Top    := 5;
    FExistGrp.Width  := 250;
    FExistGrp.Height := 30;

    FExistRdb := TRadioButton.Create(Self);
    FExistRdb.Parent  := FExistGrp;
    FExistRdb.Left    := 10;
    FExistRdb.Top     := 10;
    FExistRdb.Width   := 100;
    FExistRdb.OnClick := OnControlClick;

    FNewRdb := TRadioButton.Create(Self);
    FNewRdb.Parent  := FExistGrp;
    FNewRdb.Left    := 120;
    FNewRdb.Top     := 10;
    FNewRdb.Width   := 120;
    FNewRdb.OnClick := OnControlClick;

    FShowDuplicatesChx := TCheckBox.Create(Self);
    FShowDuplicatesChx.Parent  := FScrollBox;
    FShowDuplicatesChx.OnClick := OnControlClick;
    FShowDuplicatesChx.Width   := 250;
    FShowDuplicatesChx.Top     := 40;
    FShowDuplicatesChx.Left    := 10;

    FNamesCbx        := TComboBox.Create(Self);
    FNamesCbx.Parent := FScrollBox;
    FNamesCbx.Top    := 70;
    FNamesCbx.Width  := 250;
    FNamesCbx.Style  := csDropDownList;
    FNamesCbx.Left   := 10;

    FDuplicateLbl            := TLabel.Create(Self);
    FDuplicateLbl.Parent     := FScrollBox;
    FDuplicateLbl.AutoSize   := FALSE;
    FDuplicateLbl.Top        := 100;
    FDuplicateLbl.Width      := 250;
    FDuplicateLbl.Left       := 10;
    FDuplicateLbl.Height     := 40;
    FDuplicateLbl.Font.Color := clRed;
    FDuplicateLbl.Font.Style := [fsBold];
    FDuplicateLbl.Visible    := FALSE;
    FDuplicateLbl.WordWrap   := TRUE;

    FCancelBtn         := TButton.Create(Self);
    FCancelBtn.Parent  := FScrollBox;
    FCancelBtn.OnClick := OnCancelBtnClick;
    FCancelBtn.Left    := 10;
    FCancelBtn.Top     := 160;
    FCancelBtn.Width   := 60;

    FOKBtn             := TButton.Create(Self);
    FOKBtn.Parent      := FScrollBox;
    FOKBtn.OnClick     := OnOKBtnClick;
    FOKBtn.Left        := 75;
    FOKBtn.Top         := 160;
    FOKBtn.Width       := 60;

    FYesBtn            := TButton.Create(Self);
    FYesBtn.Parent     := FScrollBox;
    FYesBtn.OnClick    := OnYesBtnClick;
    FYesBtn.Left       := 140;
    FYesBtn.Top        := 160;
    FYesBtn.Width      := 60;
    FYesBtn.Visible    := FALSE;

    FNoBtn             := TButton.Create(Self);
    FNoBtn.Parent      := FScrollBox;
    FNoBtn.OnClick     := OnCancelBtnClick;
    FNoBtn.Left        := 205;
    FNoBtn.Top         := 160;
    FNoBtn.Width       := 60;
    FNoBtn.Visible     := FALSE;

  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TVNVGroundWaterDialog.DestroyMemberObjects;
const OPNAME = 'TVNVGroundWaterDialog.DestroyMemberObjects';
begin
	try
    FreeAndNil(FExistingStr);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TVNVGroundWaterDialog.LanguageHasChanged: boolean;
const OPNAME = 'TVNVGroundWaterDialog.LanguageHasChanged';
begin
  Result := inherited LanguageHasChanged;
	try
    Caption                    := FAppModules.Language.GetString('VNV.DlgCaptionGroundWater');
    FExistRdb.Caption          := FAppModules.Language.GetString('VNV.UseExisting');
    FNewRdb.Caption            := FAppModules.Language.GetString('VNV.CreateNew');
    FShowDuplicatesChx.Caption := FAppModules.Language.GetString('VNV.InclExistGroundWater');
    FCancelBtn.Caption         := FAppModules.Language.GetString('ButtonCaption.Cancel');
    FOKBtn.Caption             := FAppModules.Language.GetString('ButtonCaption.OK');
    FYesBtn.Caption            := FAppModules.Language.GetString('LabelText.Yes');
    FNoBtn.Caption             := FAppModules.Language.GetString('LabelText.No');
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME); end;end;

procedure TVNVGroundWaterDialog.OnCancelBtnClick(Sender: TObject);
const OPNAME = 'TVNVGroundWaterDialog.OnCancelBtnClick';
begin
	try
    FSelectedNr := -1;
	  ModalResult := mrCancel;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TVNVGroundWaterDialog.OnControlClick(Sender: TObject);
const OPNAME = 'TVNVGroundWaterDialog.OnControlClick';
begin
	try
		ResetControls;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TVNVGroundWaterDialog.OnOKBtnClick(Sender: TObject);
const OPNAME = 'TVNVGroundWaterDialog.OnOKBtnClick';
var
  lIndex    : integer;
  lExistIdx : integer;
  lMsg      : string;
  LGroundWater     : IGroundWater;
begin
	try
    if (FNewRdb.Checked) then
    begin
      if (NOT (FAppModules.User.UserRights in CUR_EditData)) then
      begin
        lMsg := FAppModules.Language.GetString('VNV.NoUserRights');
        ShowMessage(lMsg);
        ModalResult := mrCancel;
      end
      else if (FAppModules.StudyArea.ScenarioLocked) then
      begin
        lMsg := FAppModules.Language.GetString('VNV.ScenarioLocked');
        ShowMessage(lMsg);
        ModalResult := mrCancel;
      end
      else
      begin
        LGroundWater := (FAppModules.Model as IYieldModel).DoCreateGroundWater;
        if(LGroundWater <> nil) then
        begin
          FSelectedNr := LGroundWater.AquiferNodeNr;
          ModalResult := mrOk;
        end;
      end;
    end
    else
    begin
      lIndex := FNamesCbx.ItemIndex;
      if (lIndex < 0) then
      begin
        lMsg := FAppModules.Language.GetString('VNV.SelectGroundWater');
        ShowMessage(lMsg);
      end
      else
      begin
        FSelectedNr := Integer(FNamesCbx.Items.Objects[lIndex]);
        lExistIdx   :=  FExistingStr.IndexOf(IntToStr(FSelectedNr));
        if (lExistIdx >= 0) then
        begin
          lMsg := FAppModules.Language.GetString('VNV.DuplicateGroundWater');
          lMsg := Format(lMsg, [FSelectedNr]);
          FDuplicateLbl.Caption := lMsg;
          FDuplicateLbl.Visible := TRUE;
          FYesBtn.Visible       := TRUE;
          FNoBtn.Visible        := TRUE;
          FOKBtn.Visible        := FALSE;
          FCancelBtn.Visible    := FALSE;
        end
        else
          ModalResult := mrOk;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TVNVGroundWaterDialog.OnYesBtnClick(Sender: TObject);
const OPNAME = 'TVNVGroundWaterDialog.OnYesBtnClick';
begin
  try
    ModalResult := mrOk;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TVNVGroundWaterDialog.ResetControls;
const OPNAME = 'TVNVGroundWaterDialog.ResetControls';
var
  lIndex        : integer;
  lTempStr      : string;
  lGroundWater         : IGroundWater;
  lGroundWaterList     : IGroundWaterList;
  lAquiferNodeNumber   : integer;
  lAquiferNodeIdx      : integer;
begin
	try
    FShowDuplicatesChx.Enabled := FExistRdb.Checked;
    FNamesCbx.Enabled          := FExistRdb.Checked;

    if (FExistRdb.Checked) then
    begin
      FNamesCbx.Items.Clear;
      lGroundWaterList := (FAppModules.Model.ModelData as IYieldModelData).NetworkFeaturesData.GroundWaterList;
      for lIndex := 0 to lGroundWaterList.GroundWaterCount - 1 do
      begin
        lGroundWater   := lGroundWaterList.GroundWaterByIndex[lIndex];
        if(lGroundWater <> nil) then
        begin
          lAquiferNodeNumber := lGroundWater.AquiferNodeNr;
          if (lAquiferNodeNumber <> 0) then
          begin
            lAquiferNodeIdx :=  FExistingStr.IndexOf(IntToStr(lAquiferNodeNumber));
            if (FShowDuplicatesChx.Checked) OR (lAquiferNodeIdx < 0) then
            begin
              lTempStr := '(' + IntToStr(lAquiferNodeNumber) + ') ' + lGroundWater.Name;
              FNamesCbx.Items.AddObject(lTempStr, TObject(lAquiferNodeNumber));
            end;
          end;
        end;
      end;
      FNamesCbx.Sorted := TRUE;
      if (FNamesCbx.Items.Count > 0) then
        FNamesCbx.ItemIndex := 0;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TVNVGroundWaterDialog.ShowGroundWaterDialog(const AExistingGroundWater: WideString;
                                          var AExistNew, ADuplicates: integer): integer;
const OPNAME = 'TVNVGroundWaterDialog.ShowGroundWaterDialog';
begin
  Result := -1;
	try
    FExistingStr.CommaText     := AExistingGroundWater;
    FExistRdb.Checked          := AExistNew = 0;
    FNewRdb.Checked            := AExistNew = 1;
    FShowDuplicatesChx.Checked := ADuplicates = 1;
    if (ShowModal = mrOk) then
    begin
      if (FExistRdb.Checked) then
        AExistNew := 0
      else
        AExistNew := 1;
      if (FShowDuplicatesChx.Checked) then
        ADuplicates := 1
      else
        ADuplicates := 0;
      if (FSelectedNr <> -1) then
        Result := FSelectedNr;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

{******************************************************************************}
{* TVNVPowerPlantDialog                                                        *}
{******************************************************************************}

procedure TVNVPowerPlantDialog.CreateMemberObjects;
const OPNAME = 'TVNVPowerPlantDialog.CreateMemberObjects';
begin
	try
    FSelectedNr     := -1;
    FExistingStr    := TStringList.Create;

    Position     := poDesktopCenter;
    ClientHeight := 200;
    ClientWidth  := 350;

    FScrollBox := TScrollBox.Create(Self);
    FScrollBox.Parent     := Self;
    FScrollBox.Left       := 0;
    FScrollBox.Top        := 0;
    FScrollBox.Align      := alClient;
    FScrollBox.BevelInner := bvNone;

    FExistGrp        := TGroupBox.Create(Self);
    FExistGrp.Parent := FScrollBox;
    FExistGrp.Left   := 10;
    FExistGrp.Top    := 5;
    FExistGrp.Width  := 250;
    FExistGrp.Height := 30;

    FExistRdb := TRadioButton.Create(Self);
    FExistRdb.Parent  := FExistGrp;
    FExistRdb.Left    := 10;
    FExistRdb.Top     := 10;
    FExistRdb.Width   := 100;
    FExistRdb.OnClick := OnControlClick;

    FNewRdb := TRadioButton.Create(Self);
    FNewRdb.Parent  := FExistGrp;
    FNewRdb.Left    := 120;
    FNewRdb.Top     := 10;
    FNewRdb.Width   := 120;
    FNewRdb.OnClick := OnControlClick;

    FShowDuplicatesChx := TCheckBox.Create(Self);
    FShowDuplicatesChx.Parent  := FScrollBox;
    FShowDuplicatesChx.OnClick := OnControlClick;
    FShowDuplicatesChx.Width   := 250;
    FShowDuplicatesChx.Top     := 40;
    FShowDuplicatesChx.Left    := 10;

    FNamesCbx        := TComboBox.Create(Self);
    FNamesCbx.Parent := FScrollBox;
    FNamesCbx.Top    := 70;
    FNamesCbx.Width  := 250;
    FNamesCbx.Style  := csDropDownList;
    FNamesCbx.Left   := 10;

    FDuplicateLbl            := TLabel.Create(Self);
    FDuplicateLbl.Parent     := FScrollBox;
    FDuplicateLbl.AutoSize   := FALSE;
    FDuplicateLbl.Top        := 100;
    FDuplicateLbl.Width      := 250;
    FDuplicateLbl.Left       := 10;
    FDuplicateLbl.Height     := 40;
    FDuplicateLbl.Font.Color := clRed;
    FDuplicateLbl.Font.Style := [fsBold];
    FDuplicateLbl.Visible    := FALSE;
    FDuplicateLbl.WordWrap   := TRUE;

    FCancelBtn         := TButton.Create(Self);
    FCancelBtn.Parent  := FScrollBox;
    FCancelBtn.OnClick := OnCancelBtnClick;
    FCancelBtn.Left    := 10;
    FCancelBtn.Top     := 160;
    FCancelBtn.Width   := 60;

    FOKBtn             := TButton.Create(Self);
    FOKBtn.Parent      := FScrollBox;
    FOKBtn.OnClick     := OnOKBtnClick;
    FOKBtn.Left        := 75;
    FOKBtn.Top         := 160;
    FOKBtn.Width       := 60;

    FYesBtn            := TButton.Create(Self);
    FYesBtn.Parent     := FScrollBox;
    FYesBtn.OnClick    := OnYesBtnClick;
    FYesBtn.Left       := 140;
    FYesBtn.Top        := 160;
    FYesBtn.Width      := 60;
    FYesBtn.Visible    := FALSE;

    FNoBtn             := TButton.Create(Self);
    FNoBtn.Parent      := FScrollBox;
    FNoBtn.OnClick     := OnCancelBtnClick;
    FNoBtn.Left        := 205;
    FNoBtn.Top         := 160;
    FNoBtn.Width       := 60;
    FNoBtn.Visible     := FALSE;

  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TVNVPowerPlantDialog.DestroyMemberObjects;
const OPNAME = 'TVNVPowerPlantDialog.DestroyMemberObjects';
begin
	try
    FreeAndNil(FExistingStr);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TVNVPowerPlantDialog.LanguageHasChanged: boolean;
const OPNAME = 'TVNVPowerPlantDialog.LanguageHasChanged';
begin
  Result := inherited LanguageHasChanged;
	try
    Caption                    := FAppModules.Language.GetString('VNV.DlgCaptionPowerPlant');
    FExistRdb.Caption          := FAppModules.Language.GetString('VNV.UseExisting');
    FNewRdb.Caption            := FAppModules.Language.GetString('VNV.CreateNew');
    FShowDuplicatesChx.Caption := FAppModules.Language.GetString('VNV.InclExistPowerPlant');
    FCancelBtn.Caption         := FAppModules.Language.GetString('ButtonCaption.Cancel');
    FOKBtn.Caption             := FAppModules.Language.GetString('ButtonCaption.OK');
    FYesBtn.Caption            := FAppModules.Language.GetString('LabelText.Yes');
    FNoBtn.Caption             := FAppModules.Language.GetString('LabelText.No');
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TVNVPowerPlantDialog.ShowPowerPlantDialog (const AExistingPowerPlants : WideString;
                                                   var   AExistNew           : integer;
                                                   var   ADuplicates         : integer): integer;
const OPNAME = 'TVNVPowerPlantDialog.ShowPowerPlantDialog';
begin
  Result := -1;
	try
    FExistingStr.CommaText     := AExistingPowerPlants;
    FExistRdb.Checked          := AExistNew = 0;
    FNewRdb.Checked            := AExistNew = 1;
    FShowDuplicatesChx.Checked := ADuplicates = 1;
    if (ShowModal = mrOk) then
    begin
      if (FExistRdb.Checked) then
        AExistNew := 0
      else
        AExistNew := 1;
      if (FShowDuplicatesChx.Checked) then
        ADuplicates := 1
      else
        ADuplicates := 0;
      if (FSelectedNr <> -1) then
        Result := FSelectedNr;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TVNVPowerPlantDialog.ResetControls;
const OPNAME = 'TVNVPowerPlantDialog.ResetControls';
var
  lIndex          : integer;
  lTempStr        : string;
  lPowerPlantList : IPowerPlantList;
  lPowerPlant     : IPowerPlant;
  lPowerPlantNr   : integer;
  lPowerPlantIdx  : integer;
begin
	try
    FShowDuplicatesChx.Enabled := FExistRdb.Checked;
    FNamesCbx.Enabled          := FExistRdb.Checked;

    if (FExistRdb.Checked) then
    begin
      FNamesCbx.Items.Clear;
      lPowerPlantList := (FAppModules.Model.ModelData as IYieldModelData).NetworkFeaturesData.PowerPlantList;
      for lIndex := 0 to lPowerPlantList.PowerPlantCount - 1 do
      begin
        lPowerPlant := lPowerPlantList.PowerPlantByIndex[lIndex];
        if(lPowerPlant <> nil) then
        begin
          lPowerPlantNr := lPowerPlant.FeatureID;
          if (lPowerPlant.FeatureType = ntMineNode) then
          begin
            lPowerPlantIdx :=  FExistingStr.IndexOf(IntToStr(lPowerPlantNr));
            if (FShowDuplicatesChx.Checked) OR (lPowerPlantIdx < 0) then
            begin
              lTempStr := '(' + IntToStr(lPowerPlantNr) + ') ' +
                          lPowerPlant.FeatureName;
              FNamesCbx.Items.AddObject(lTempStr, TObject(lPowerPlantNr));
            end;
          end;
        end;
      end;
      FNamesCbx.Sorted := TRUE;
      if (FNamesCbx.Items.Count > 0) then
        FNamesCbx.ItemIndex := 0;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TVNVPowerPlantDialog.OnOKBtnClick(Sender : TObject);
const OPNAME = 'TVNVPowerPlantDialog.OnOKBtnClick';
var
  lIndex,
  lExistIdx : integer;
  lMsg      : string;
  LPowerPlant :IPowerPlant;
begin
	try
    if (FNewRdb.Checked) then
    begin
      if (NOT (FAppModules.User.UserRights in CUR_EditData)) then
      begin
        lMsg := FAppModules.Language.GetString('VNV.NoUserRights');
        ShowMessage(lMsg);
        ModalResult := mrCancel;
      end
      else if (FAppModules.StudyArea.ScenarioLocked) then
      begin
        lMsg := FAppModules.Language.GetString('VNV.ScenarioLocked');
        ShowMessage(lMsg);
        ModalResult := mrCancel;
      end
      else
      begin
        LPowerPlant := (FAppModules.Model as IYieldModel).DoCreatePowerPlant;
        if(LPowerPlant <> nil) then
        begin
          FSelectedNr := LPowerPlant.FeatureID;
          ModalResult := mrOk;
        end;
      end;
    end
    else
    begin
      lIndex := FNamesCbx.ItemIndex;
      if (lIndex < 0) then
      begin
        lMsg := FAppModules.Language.GetString('VNV.SelectPowerPlant');
        ShowMessage(lMsg);
      end
      else
      begin
        FSelectedNr := Integer(FNamesCbx.Items.Objects[lIndex]);
        lExistIdx   :=  FExistingStr.IndexOf(IntToStr(FSelectedNr));
        if (lExistIdx >= 0) then
        begin
          lMsg := FAppModules.Language.GetString('VNV.DuplicatePowerPlant');
          lMsg := Format(lMsg, [FSelectedNr]);
          FDuplicateLbl.Caption := lMsg;
          FDuplicateLbl.Visible := TRUE;
          FYesBtn.Visible       := TRUE;
          FNoBtn.Visible        := TRUE;
          FOKBtn.Visible        := FALSE;
          FCancelBtn.Visible    := FALSE;
        end
        else
          ModalResult := mrOk;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TVNVPowerPlantDialog.OnYesBtnClick(Sender : TObject);
const OPNAME = 'TVNVPowerPlantDialog.OnYesBtnClick';
begin
  try
    ModalResult := mrOk;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TVNVPowerPlantDialog.OnCancelBtnClick(Sender : TObject);
const OPNAME = 'TVNVPowerPlantDialog.OnCancelBtnClick';
begin
	try
    FSelectedNr := -1;
	  ModalResult := mrCancel;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TVNVPowerPlantDialog.OnControlClick(Sender : TObject);
const OPNAME = 'TVNVPowerPlantDialog.OnControlClick';
begin
	try
		ResetControls;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

end.

