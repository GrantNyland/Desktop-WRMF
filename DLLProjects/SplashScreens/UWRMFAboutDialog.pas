//
//
//  UNIT      : Contains the class TVaalDBMSAboutDialog.
//  AUTHOR    : Grant Nyland
//  DATE      : 2002/02/20
//  COPYRIGHT : Copyright © 2002 DWAF
//
//
unit UWRMFAboutDialog;

interface

uses Vcl.StdCtrls, Vcl.Graphics, Vcl.ExtCtrls, Vcl.Controls, System.Classes,
     SysUtils, VCL.Forms, Windows, VCL.Buttons,
     EMailLabel,URLLabel, UAbstractObject, UAbstractComponent;

type
  TAboutRec = record
    ProductVersion : string;
    DiskSpace      : string;
    FreeSpace      : string;
  end;

  TVaalDBMSAboutDialog = class(TAbstractForm)
    pnlPanel: TPanel;
    imgProgramIcon: TImage;
    lblVersionDetails: TLabel;
    lblSystemDescription: TLabel;
    lblCopyright: TLabel;
    lblAcknowledgements: TLabel;
    lblProjectManagement: TLabel;
    lblProjectManagerName: TLabel;
    lblProjectManagerOrg: TLabel;
    lblBusinessSupport: TLabel;
    lblBusinessSupportNames: TLabel;
    lblBusinessSupportOrg: TLabel;
    lblITDevelopment: TLabel;
    lblITDevelopmentNames: TLabel;
    lblITDevelopmentOrg: TLabel;
    Role: TLabel;
    Name: TLabel;
    Organisation: TLabel;
    lblOwnership: TLabel;
    lblAddress: TLabel;
    btnOK: TButton;
    lblOwnershipDecription: TLabel;
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  protected
    FEMailLabel : TEMailLabel;
    FURLLabel   : TURLLabel;
    FData : TAboutRec;
  public
    procedure AfterConstruction; override;
    function LanguageHasChanged: boolean; override;
    procedure PopulateDialogData;
  end;

procedure DoAboutDialog(AAppModules: TObject); export; stdcall;

implementation

uses

  UModuleVersionOperations, UErrorHandlingOperations;

{$R *.DFM}

procedure DoAboutDialog(AAppModules: TObject);
const OPNAME = 'UWRMFAboutDialog.DoAboutDialog';
var LDailog: TVaalDBMSAboutDialog;
begin
  try
    LDailog := TVaalDBMSAboutDialog.Create(nil, TAppModules(AAppModules));
    try
      LDailog.PopulateDialogData;
      LDailog.LanguageHasChanged;
      LDailog.ShowModal;
      if LDailog.ModalResult = mrOK then
        LDailog.Close;
    finally
      LDailog.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TVaalDBMSAboutDialog.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
const OPNAME = 'TVaalDBMSAboutDialog.FormKeyDown';
begin
  try
    if (Key = VK_F1) then
    begin
      Application.HelpContext(0);
      Windows.SetFocus(handle);
    end;
    if (Key = VK_ESCAPE) then
    begin
      Close;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TVaalDBMSAboutDialog.PopulateDialogData;
const OPNAME = 'TVaalDBMSAboutDialog.PopulateDialogData';
{var
  LFreeMem,LDiskSpace: Int64;
  LFreeSpace,LDisk : Extended; }
begin
  try
    imgProgramIcon.Picture.Bitmap.LoadFromResourceName(HInstance, 'ABOUTLOGO');
    FData.ProductVersion := GetModuleVersion(CurrentModuleFileName);
{    FEMailLabel := TEMailLabel.Create(pnlPanel);
    FURLLabel   := TURLLabel.Create(Self);
    FEMailLabel.Parent := pnlPanel;
    FURLLabel.Parent := pnlPanel;
    FEMailLabel.Top   := lblEmail.Top;
    FEMailLabel.Left  := lblEmail.Left + lblEmail.Width + 1;
    FEMailLabel.Font.Color  := clBlue;
    FEMailLabel.FontColorAfter  := clBlue;
    FEMailLabel.FontColorOut  := clBlue;
    FURLLabel.Top   := lblURL.Top;
    FURLLabel.Left  := lblURL.Left + lblURL.Width + 1;
    FEMailLabel.EMailAddress := FAppModules.Language.GetString('LabelText.DwafEmail');
    FURLLabel.URL := FAppModules.Language.GetString('LabelText.DwafURL');
    FURLLabel.FontColorAfter  := clBlue;
    FURLLabel.FontColorOut  := clBlue;
    FURLLabel.Font.Color  := clBlue;
    LFreeMem             := DiskFree(3);
    LFreeSpace           := LFreeMem div 1024;
    FData.FreeSpace      := Format('%3.0n KB',[LFreeSpace]);
    LDiskSpace           := DiskSize(3);
    LDisk                := LDiskSpace div 1024;
    FData.DiskSpace      := Format('%3.0n KB',[LDisk]); }
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TVaalDBMSAboutDialog.LanguageHasChanged: boolean;
const OPNAME = 'TVaalDBMSAboutDialog.LanguageHasChanged';
begin
  Result := True;
  try
    Caption                        := FAppModules.Language.GetString('HelpAbout.FormCaption');
    lblOwnershipDecription.Caption := FAppModules.Language.GetString('HelpAbout.CompanyName');
    lblVersionDetails.Caption      := FAppModules.Language.GetString('HelpAbout.VersionDetails') + FData.ProductVersion;
    lblAcknowledgements.Caption    := FAppModules.Language.GetString('HelpAbout.Acknowledgements');// + '      : ';
    lblOwnership.Caption           := FAppModules.Language.GetString('HelpAbout.Ownership');// + '      : ';
    lblSystemDescription.Caption   := FAppModules.Language.GetString('HelpAbout.Description');//GetString('HelpAbout.CompanyName');
    lblAddress.Caption             := FAppModules.Language.GetString('HelpAbout.Address');
    lblCopyright.Caption           := FAppModules.Language.GetString('HelpAbout.CopyRight');

    lblProjectManagement.Caption   :=  FAppModules.Language.GetString('HelpAbout.ProjectManagement');
    lblProjectManagerName.Caption  :=  FAppModules.Language.GetString('HelpAbout.ProjectManagementName');
    lblProjectManagerOrg.Caption   :=  FAppModules.Language.GetString('HelpAbout.ProjectManagementOrg');
    lblBusinessSupport.Caption     :=  FAppModules.Language.GetString('HelpAbout.BusinessSupport');
    lblBusinessSupportNames.Caption:=  FAppModules.Language.GetString('HelpAbout.BusinessSupportNames');
    lblBusinessSupportOrg.Caption  :=  FAppModules.Language.GetString('HelpAbout.BusinessSupportOrgs');
    lblITDevelopment.Caption       :=  FAppModules.Language.GetString('HelpAbout.ITDevelopment');
    lblITDevelopmentNames.Caption  :=  FAppModules.Language.GetString('HelpAbout.ITDevelopmentNames');
    lblITDevelopmentOrg.Caption    :=  FAppModules.Language.GetString('HelpAbout.ITDevelopmentOrgs');
//    lblUserSupport.Caption         :=  FAppModules.Language.GetString('HelpAbout.UserSupport');
//    lblUserSupportNames.Caption    :=  FAppModules.Language.GetString('HelpAbout.UserSupportNames');
//    lblUserSupportOrg.Caption  :=  FAppModules.Language.GetString('HelpAbout.UserSupportOrgs');

//    lblVersion.Caption            := FAppModules.Language.GetString('HelpAbout.Version');// + '           : ' ;
//    lblColon2.Caption             := FAppModules.Language.GetString('HelpAbout.Colon2');
//    lblAddress1.Caption           := FAppModules.Language.GetString('HelpAbout.Address1');
//    lblAddress2.Caption           := FAppModules.Language.GetString('HelpAbout.Address2');
//    lblSpace.Caption              := FAppModules.Language.GetString('HelpAbout.DiskSpace');//+ '      : ' ;
//    lblColon3.Caption             := FAppModules.Language.GetString('HelpAbout.colon3');
//    lblSpaceDetails.Caption       := FData.DiskSpace;
//    lblFreeMem.Caption            := FAppModules.Language.GetString('HelpAbout.FreeMemory');//+ '   : ' ;
//    lblColon4.Caption             := FAppModules.Language.GetString('HelpAbout.Colon4');
//    lblFreeMemDetails.Caption     := FData.FreeSpace;
//    lblSupport.Caption            := FAppModules.Language.GetString('HelpAbout.Support');
//    lblColon.Caption              := FAppModules.Language.GetString('HelpAbout.Colon');
//    lblSupportDetails.Caption     := FAppModules.Language.GetString('HelpAbout.SupportDetails');
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TVaalDBMSAboutDialog.AfterConstruction;
const OPNAME = 'TVaalDBMSAboutDialog.AfterConstruction';
begin
  try
    if Assigned(FAppModules) and Assigned(FAppModules.ViewIni()) then
      FAppModules.ViewIni.SaveFormView(self);
  finally
    inherited;
  end;
end;

end.





