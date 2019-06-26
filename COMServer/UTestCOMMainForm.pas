unit UTestCOMMainForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Grids, ExtCtrls, StdCtrls, ComCtrls,ComObj, ActiveX,OleServer,VoaimsCom_TLB;

type
  TfrmMain = class(TForm)
    pgctrlMain: TPageControl;
    tsSetup: TTabSheet;
    tsSFR: TTabSheet;
    PnlClient: TPanel;
    srtgrdFileData: TStringGrid;
    pnlBottom: TPanel;
    btnViewRuoffFile: TButton;
    pnlTop: TPanel;
    GroupBox1: TGroupBox;
    lblStudy: TLabel;
    lblModel: TLabel;
    lblSubArea: TLabel;
    lblScenario: TLabel;
    edtStudy: TEdit;
    edtModel: TEdit;
    edtSubArea: TEdit;
    edtScenario: TEdit;
    GroupBox2: TGroupBox;
    lblUserID: TLabel;
    Label2: TLabel;
    edtUserID: TEdit;
    edtPassword: TEdit;
    GroupBox3: TGroupBox;
    btnLogon: TButton;
    btnSelectStudy: TButton;
    btnViewSoilMoistureFile: TButton;
    lblSFR: TLabel;
    cmbboxSFR: TComboBox;
    lblRunOffFile: TLabel;
    edtRunoff: TEdit;
    lblSoilMoisture: TLabel;
    edtSoilMoisture: TEdit;
    pnlMessg: TPanel;
    tsDemandCenter: TTabSheet;
    Panel1: TPanel;
    Panel2: TPanel;
    Label1: TLabel;
    cmbDemandCentres: TComboBox;
    GroupBox4: TGroupBox;
    Label3: TLabel;
    edtDemandCentreNodeNumber: TEdit;
    Label4: TLabel;
    edtDemandCentreName: TEdit;
    GroupBox5: TGroupBox;
    Label5: TLabel;
    Label6: TLabel;
    edtDCCCChannelNumber: TEdit;
    edtDCCCChannelName: TEdit;
    Label7: TLabel;
    Label8: TLabel;
    edtDCCCChannelUpstreamNodeNumber: TEdit;
    edtDCCCChannelDownstreamNodeNumber: TEdit;
    GroupBox6: TGroupBox;
    Label9: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    edtDCRPChannelNumber: TEdit;
    edtDCRPChannelName: TEdit;
    edtDCRPChannelUpstreamNodeNumber: TEdit;
    edtDCRPChannelDownstreamNodeNumber: TEdit;
    chkboxDCRPChannelPlantExist: TCheckBox;
    GroupBox7: TGroupBox;
    strgrdReturnFlowChanells: TStringGrid;
    Stomsa: TTabSheet;
    Edit1: TEdit;
    procedure btnLogonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure btnSelectStudyClick(Sender: TObject);
    procedure pgctrlMainChange(Sender: TObject);
    procedure cmbboxSFRChange(Sender: TObject);
    procedure btnViewRuoffFileClick(Sender: TObject);
    procedure btnViewSoilMoistureFileClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure cmbDemandCentresChange(Sender: TObject);
  private
    { Private declarations }
    FVoaimsComServer : IVoaimsComObject;
    FSFRInitilalised: boolean;
    FDemandCentreInitilalised: boolean;
    procedure CreateVoaimsComServer;
    procedure InitialiseSFR;
    procedure InitialiseDemandCentre;
    procedure PopulateSFRGrid(AFileData: string);
  public
    { Public declarations }
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.dfm}

procedure TfrmMain.FormCreate(Sender: TObject);
begin
 FVoaimsComServer := nil;
 pgctrlMain.ActivePageIndex := 0;
end;

procedure TfrmMain.FormClose(Sender: TObject; var Action: TCloseAction);
begin
 if(FVoaimsComServer <> nil) then
 begin
   if FVoaimsComServer.IsStudySelected then
      FVoaimsComServer.CloseScenario;
   if FVoaimsComServer.IsUserLoggedOn then
      FVoaimsComServer.Logoff;
   FVoaimsComServer := nil;
 end;
end;

procedure TfrmMain.CreateVoaimsComServer;
begin
  if(FVoaimsComServer = nil) then
  begin
   FVoaimsComServer := (CreateComObject(CLASS_VoaimsComObject) as IVoaimsComObject);
   if(FVoaimsComServer <> nil) then
     FVoaimsComServer.Initialise;
  end;
end;

procedure TfrmMain.btnLogonClick(Sender: TObject);
var
  LUserID,
  LPassword: string;
begin
  if(FVoaimsComServer = nil) then
    CreateVoaimsComServer;
  if(FVoaimsComServer <> nil) and (FVoaimsComServer.IsServerInitialised) and (not FVoaimsComServer.IsUserLoggedOn) then
  begin
    LUserID   := edtUserID.Text;
    LPassword := edtPassword.Text;
    FVoaimsComServer.Logon(LUserID,LPassword);
    btnSelectStudy.Enabled := FVoaimsComServer.IsUserLoggedOn;
    if btnSelectStudy.Enabled then
      pnlMessg.Caption := '(' + LUserID + ') is currently logged on';
  end;
end;

procedure TfrmMain.btnSelectStudyClick(Sender: TObject);
var
  LStudy     : string;
  LModel     : string;
  LSubArea   : string;
  LScenario  : string;
begin
  if(FVoaimsComServer <> nil) and FVoaimsComServer.IsUserLoggedOn  then
  begin
    LStudy     := edtStudy.Text;
    LModel     := edtModel.Text;
    LSubArea   := edtSubArea.Text;
    LScenario  := edtScenario.Text;
    FVoaimsComServer.SelectStudy(LModel,LStudy,LSubArea,LScenario);
    cmbboxSFR.Enabled := FVoaimsComServer.IsStudySelected;
    if cmbboxSFR.Enabled then
    begin
      FSFRInitilalised := False;
      FDemandCentreInitilalised := False;
      pnlMessg.Caption := '(' + 'Study='+LStudy+ ' Model='+LModel+ ' SubArea='+LSubArea+ ' Scenario='+LScenario + ') is the selected study';
    end;
    if (FVoaimsComServer.) then
    begin
      FSFRInitilalised := False;
      FDemandCentreInitilalised := False;
      pnlMessg.Caption := '(' + 'Study='+LStudy+ ' Model='+LModel+ ' SubArea='+LSubArea+ ' Scenario='+LScenario + ') is the selected study';
    end;
  end;
end;

procedure TfrmMain.pgctrlMainChange(Sender: TObject);
begin
  if(pgctrlMain.ActivePage = tsSFR) then
    InitialiseSFR;
  if(pgctrlMain.ActivePage = tsDemandCenter) then
    InitialiseDemandCentre;
end;

procedure TfrmMain.InitialiseSFR;
var
  LIndex : integer;
  LStreamFlowReduction : IStreamFlowReduction;
begin
  if not ((FVoaimsComServer <> nil) and FVoaimsComServer.IsUserLoggedOn)  then Exit;
  if not FSFRInitilalised then
  begin
    cmbboxSFR.Clear;
    if(FVoaimsComServer <> nil) and FVoaimsComServer.IsStudySelected then
    begin
      if(FVoaimsComServer.YieldModel <> nil) then
      begin
        for LIndex := 0 to FVoaimsComServer.YieldModel.YieldModelData.NetworkFeaturesData.StreamFlowReductionList.StreamFlowReductionCount-1 do
        begin
          LStreamFlowReduction := FVoaimsComServer.YieldModel.YieldModelData.NetworkFeaturesData.StreamFlowReductionList.StreamFlowReductionByIndex[LIndex];
          cmbboxSFR.Items.AddObject(LStreamFlowReduction.SFRName,TObject(LStreamFlowReduction.Identifier));
        end;
        FSFRInitilalised := True;
      end;
    end;
  end;
end;

procedure TfrmMain.InitialiseDemandCentre;
var
  LIndex : integer;
  LDemandCentre : IYMDemandCentre;
begin
  if not ((FVoaimsComServer <> nil) and FVoaimsComServer.IsUserLoggedOn)  then Exit;
  if not FDemandCentreInitilalised then
  begin
    cmbDemandCentres.Clear;
    if(FVoaimsComServer <> nil) and FVoaimsComServer.IsStudySelected then
    begin
      if(FVoaimsComServer.YieldModel <> nil) then
      begin
        for LIndex := 0 to FVoaimsComServer.YieldModel.YieldModelData.NetworkFeaturesData.YMDemandCentreList.YMDemandCentreCount-1 do
        begin
          LDemandCentre := FVoaimsComServer.YieldModel.YieldModelData.NetworkFeaturesData.YMDemandCentreList.YMDemandCentreByIndex[LIndex];
          cmbDemandCentres.Items.AddObject(LDemandCentre.Name,TObject(LDemandCentre.NodeNumber));
        end;
        FDemandCentreInitilalised := True;
      end;
    end;
  end;
end;

procedure TfrmMain.cmbboxSFRChange(Sender: TObject);
var
  LIndex,
  LIdentifier : integer;
  LStreamFlowReduction : IStreamFlowReduction;
begin
  LIndex := cmbboxSFR.ItemIndex;
  LIdentifier := Integer(cmbboxSFR.Items.Objects[LIndex]);
  LStreamFlowReduction := FVoaimsComServer.YieldModel.YieldModelData.NetworkFeaturesData.StreamFlowReductionList.StreamFlowReductionByID[LIdentifier];
  if(LStreamFlowReduction <> nil) then
  begin
    edtRunoff.Text := LStreamFlowReduction.UnitRunoffFileName;
    edtSoilMoisture.Text := LStreamFlowReduction.SoilMoistureFileName;
    btnViewRuoffFile.Enabled := (edtRunoff.Text <> '');
    btnViewSoilMoistureFile.Enabled := (edtSoilMoisture.Text <> '');
  end;
end;

procedure TfrmMain.PopulateSFRGrid(AFileData: string);
var
  LIndex               : integer;
  LCommaTextData       : TStringList;
begin
  if(AFileData <> '') then
  begin
    LCommaTextData := TStringList.Create;
    try
      LCommaTextData.CommaText := AFileData;
      srtgrdFileData.RowCount  := LCommaTextData.Count+1;
      for LIndex := 0 to LCommaTextData.Count-1 do
      begin
        srtgrdFileData.Rows[LIndex+1].CommaText := LCommaTextData[LIndex];
      end;
    finally
      LCommaTextData.Free;
    end;
  end;
end;

procedure TfrmMain.btnViewRuoffFileClick(Sender: TObject);
var
  LData                : String;
  LIndex               : integer;
  LIdentifier          : integer;
  LStreamFlowReduction : IStreamFlowReduction;
begin
  LIndex := cmbboxSFR.ItemIndex;
  LIdentifier := Integer(cmbboxSFR.Items.Objects[LIndex]);
  srtgrdFileData.RowCount := 2;
  srtgrdFileData.Rows[1].Clear;
  LStreamFlowReduction := FVoaimsComServer.YieldModel.YieldModelData.NetworkFeaturesData.StreamFlowReductionList.StreamFlowReductionByID[LIdentifier];
  if(LStreamFlowReduction <> nil) then
  begin
    LData := LStreamFlowReduction.UnitRunoffFileData;
    PopulateSFRGrid(LData);
  end;
end;

procedure TfrmMain.btnViewSoilMoistureFileClick(Sender: TObject);
var
  LData                : String;
  LIndex               : integer;
  LIdentifier          : integer;
  LStreamFlowReduction : IStreamFlowReduction;
begin
  LIndex := cmbboxSFR.ItemIndex;
  LIdentifier := Integer(cmbboxSFR.Items.Objects[LIndex]);
  srtgrdFileData.RowCount := 2;
  srtgrdFileData.Rows[1].Clear;
  LStreamFlowReduction := FVoaimsComServer.YieldModel.YieldModelData.NetworkFeaturesData.StreamFlowReductionList.StreamFlowReductionByID[LIdentifier];
  if(LStreamFlowReduction <> nil) then
  begin
    LData := LStreamFlowReduction.SoilMoistureFileData;
    PopulateSFRGrid(LData);
  end;
end;

procedure TfrmMain.FormShow(Sender: TObject);
begin
  srtgrdFileData.Rows[0].CommaText := 'Year,OCT,NOV,DEC,JAN,FEB,MAR,APR,MAY,JUN,JUL,AUG,SEP,Total';
end;

procedure TfrmMain.cmbDemandCentresChange(Sender: TObject);
var
  LIndex               : integer;
  LNodeNumber          : integer;
  LDemandCentre        : IYMDemandCentre;
  LChannel             : IGeneralFlowChannel;
  LReturnFlowFeature   : IYMDemandCentreReturnFlowFeature;
begin
  edtDemandCentreNodeNumber.Text           := '';
  edtDemandCentreName.Text                 := '';
  edtDCCCChannelNumber.Text                := '';
  edtDCCCChannelName.Text                  := '';
  edtDCCCChannelUpstreamNodeNumber.Text    := '';
  edtDCCCChannelDownstreamNodeNumber.Text  := '';
  edtDCRPChannelNumber.Text                := '';
  edtDCRPChannelName.Text                  := '';
  edtDCRPChannelUpstreamNodeNumber.Text    := '';
  edtDCRPChannelDownstreamNodeNumber.Text  := '';
  chkboxDCRPChannelPlantExist.Checked      := False;
  for LIndex := 0 to strgrdReturnFlowChanells.RowCount-1 do
    strgrdReturnFlowChanells.Rows[LIndex].Clear;

  LIndex := cmbDemandCentres.ItemIndex;
  LNodeNumber := Integer(cmbDemandCentres.Items.Objects[LIndex]);
  LDemandCentre := FVoaimsComServer.YieldModel.YieldModelData.NetworkFeaturesData.YMDemandCentreList.YMDemandCentreByNodeNumber[LNodeNumber];
  if(LDemandCentre <> nil) then
  begin
    edtDemandCentreNodeNumber.Text            := IntToStr(LDemandCentre.NodeNumber);
    edtDemandCentreName.Text                  := LDemandCentre.Name;
    LChannel                                  := LDemandCentre.ConsumptiveUseChannel;
    if(LChannel <> nil) then
    begin
      edtDCCCChannelNumber.Text               := IntToStr(LChannel.ChannelNumber);
      edtDCCCChannelName.Text                 := LChannel.ChannelName;
      edtDCCCChannelUpstreamNodeNumber.Text   := IntToStr(LChannel.UpStreamNodeNumber);
      edtDCCCChannelDownstreamNodeNumber.Text := IntToStr(LChannel.DownStreamNodeNumber);
    end;

    chkboxDCRPChannelPlantExist.Checked       := LDemandCentre.ReclaimationPlantExists;

    LChannel                                  := LDemandCentre.ReclaimationChannel;
    if(LChannel <> nil) then
    begin
      edtDCRPChannelNumber.Text               := IntToStr(LChannel.ChannelNumber);
      edtDCRPChannelName.Text                 := LChannel.ChannelName;
      edtDCRPChannelUpstreamNodeNumber.Text   := IntToStr(LChannel.UpStreamNodeNumber);
      edtDCRPChannelDownstreamNodeNumber.Text := IntToStr(LChannel.DownStreamNodeNumber);
    end;
    if(LDemandCentre.ReturnFlowFeatureList.ReturnFlowFeatureCount > 0) then
    begin
      strgrdReturnFlowChanells.RowCount   := LDemandCentre.ReturnFlowFeatureList.ReturnFlowFeatureCount+1;
      strgrdReturnFlowChanells.Cells[0,0] := 'No#';
      strgrdReturnFlowChanells.Cells[1,0] := 'Channel Number';
      strgrdReturnFlowChanells.Cells[2,0] := 'Channel Name';
      strgrdReturnFlowChanells.Cells[3,0] := 'Upstream Node';
      strgrdReturnFlowChanells.Cells[4,0] := 'Downstream Node';
      strgrdReturnFlowChanells.Cells[5,0] := 'Penalty Number';
      for LIndex := 0 to LDemandCentre.ReturnFlowFeatureList.ReturnFlowFeatureCount-1 do
      begin
        LReturnFlowFeature := LDemandCentre.ReturnFlowFeatureList.ReturnFlowFeatureByIndex[LIndex];
        strgrdReturnFlowChanells.Cells[0,LIndex+1] := IntToStr(LIndex+1);
        strgrdReturnFlowChanells.Cells[1,LIndex+1] := IntToStr(LReturnFlowFeature.Channel.ChannelNumber);
        strgrdReturnFlowChanells.Cells[2,LIndex+1] := LReturnFlowFeature.Channel.ChannelName;
        strgrdReturnFlowChanells.Cells[3,LIndex+1] := LReturnFlowFeature.Channel.UpStreamNode.ReservoirConfigurationData.ReservoirName;
        strgrdReturnFlowChanells.Cells[4,LIndex+1] := LReturnFlowFeature.Channel.DownStreamNode.ReservoirConfigurationData.ReservoirName;
        strgrdReturnFlowChanells.Cells[5,LIndex+1] := IntToStr(LReturnFlowFeature.Channel.ChannelPenaltyNumber);
      end;
    end;
  end;
end;

end.
