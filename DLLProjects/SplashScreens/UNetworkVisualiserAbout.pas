unit UNetworkVisualiserAbout;

interface

procedure DoAboutDialog(AAppModules: TObject); export; stdcall;

implementation

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, UAbstractObject,
  UAbstractComponent, Buttons,UModuleVersionOperations, StdCtrls,
  UErrorHandlingOperations;

type
  TAboutRec = record
    ProductVersion : string;
    DiskSpace      : string;
    FreeSpace      : string;
  end;
  TNetworkVisualiserAboutDialog = class(TAbstractForm)
    Panel1: TPanel;
    Image1: TImage;
    lblcolon1: TLabel;
    lblVersionDetails: TLabel;
    lblColon2: TLabel;
    lblOwnershipDetails: TLabel;
    lblAddress: TLabel;
    lblAddress1: TLabel;
    lblAddress2: TLabel;
    lblSpace: TLabel;
    lblColon3: TLabel;
    lblSpaceDetails: TLabel;
    lblFreeMem: TLabel;
    lblColon4: TLabel;
    lblFreeMemDetails: TLabel;
    lblSupport: TLabel;
    lblColon: TLabel;
    lblSupportDetails: TLabel;
    lblCopyright: TLabel;
    lblOwnership: TLabel;
    lblVersion: TLabel;
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  protected
    FData : TAboutRec;
  public
    procedure AfterConstruction; override;
    function LanguageHasChanged: boolean; override;
    procedure PopulateDialogData;
  end;

{$R *.DFM}

procedure DoAboutDialog(AAppModules: TObject);
const OPNAME = 'UNetworkVisualiserAbout.DoAboutDialog';
var LDailog: TNetworkVisualiserAboutDialog;
begin
  try
    LDailog := TNetworkVisualiserAboutDialog.Create(nil, TAppModules(AAppModules));
    try
      LDailog.PopulateDialogData;
      LDailog.LanguageHasChanged;
      LDailog.ShowModal;
    finally
      LDailog.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TNetworkVisualiserAboutDialog.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
const OPNAME = 'TNetworkVisualiserAboutDialog.FormKeyDown';
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

procedure TNetworkVisualiserAboutDialog.PopulateDialogData;
const OPNAME = 'TNetworkVisualiserAboutDialog.PopulateDialogData';
var
  LFreeMem,LDiskSpace: Int64;
  LFreeSpace,LDisk : Extended;
begin
  try
    Image1.Picture.Bitmap.LoadFromResourceName(HInstance, 'ABOUTLOGO');
    FData.ProductVersion := GetModuleVersion(CurrentModuleFileName);
    LFreeMem             := DiskFree(3);
    LFreeSpace           := LFreeMem div 1024;
    FData.FreeSpace      := Format('%3.0n KB',[LFreeSpace]);
    LDiskSpace           := DiskSize(3);
    LDisk                := LDiskSpace div 1024;
    FData.DiskSpace      := Format('%3.0n KB',[LDisk]);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TNetworkVisualiserAboutDialog.LanguageHasChanged: boolean;
const OPNAME = 'TNetworkVisualiserAboutDialog.LanguageHasChanged';
begin
  Result := True;
  try
    Caption                       := FAppModules.Language.GetString('HelpAboutNV.FormCaption');
    lblVersion.Caption            := FAppModules.Language.GetString('HelpAboutNV.Version');
    lblcolon1.Caption             := FAppModules.Language.GetString('HelpAboutNV.Colon1');
    lblVersionDetails.Caption     := FData.ProductVersion;
    lblOwnership.Caption          := FAppModules.Language.GetString('HelpAboutNV.Ownership');
    lblColon2.Caption             := FAppModules.Language.GetString('HelpAboutNV.Colon2');
    lblOwnershipDetails.Caption   := FAppModules.Language.GetString('HelpAboutNV.CompanyName');
    lblAddress.Caption            := FAppModules.Language.GetString('HelpAboutNV.Address');
    lblAddress1.Caption           := FAppModules.Language.GetString('HelpAboutNV.Address1');
    lblAddress2.Caption           := FAppModules.Language.GetString('HelpAboutNV.Address2');
    lblSpace.Caption              := FAppModules.Language.GetString('HelpAboutNV.DiskSpace');
    lblColon3.Caption             := FAppModules.Language.GetString('HelpAboutNV.colon3');
    lblSpaceDetails.Caption       := FData.DiskSpace;
    lblFreeMem.Caption            := FAppModules.Language.GetString('HelpAboutNV.FreeMemory');
    lblColon4.Caption             := FAppModules.Language.GetString('HelpAboutNV.Colon4');
    lblFreeMemDetails.Caption     := FData.FreeSpace;
    lblSupport.Caption            := FAppModules.Language.GetString('HelpAboutNV.Support');
    lblColon.Caption              := FAppModules.Language.GetString('HelpAboutNV.Colon');
    lblSupportDetails.Caption     := FAppModules.Language.GetString('HelpAboutNV.SupportDetails');
    lblCopyright.Caption          := FAppModules.Language.GetString('HelpAboutNV.CopyRight');
  except on E: Exception do HandleError(E, OPNAME) end;
end;


procedure TNetworkVisualiserAboutDialog.AfterConstruction;
const OPNAME = 'TNetworkVisualiserAboutDialog.AfterConstruction';
begin
  try
    if Assigned(FAppModules) and Assigned(FAppModules.ViewIni()) then
      FAppModules.ViewIni.SaveFormView(self);
  finally
    inherited;
  end;
end;

end.

