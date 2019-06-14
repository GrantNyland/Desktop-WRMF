//
//  UNIT      : Contains TfrmImportHydrologyStudies Class
//  AUTHOR    : Dziedzi Ramulondi(Arivia)
//  DATE      : 2009/10/11
//  COPYRIGHT : Copyright © 2009 DWAF
//

unit UImportHydrologyStudiesForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms,
  Vcl.Dialogs, Vcl.ExtCtrls, Vcl.StdCtrls, Vcl.Buttons, Vcl.ComCtrls, Vcl.CheckLst;

type
  TImportHydrologyDialogType = (dtLink,dtUnLink);
  TfrmImportHydrologyStudies = class(TForm)
    pnlButtons: TPanel;
    btnCancel: TBitBtn;
    btnOk: TBitBtn;
    chklstboxHydrologyStudies: TCheckListBox;
    chkboxInvertSelection: TCheckBox;
    procedure chkboxInvertSelectionClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  protected
    FNameValueItems : TStringList;
    FDialogType     : TImportHydrologyDialogType;
    procedure Set_DialogType(AValue : TImportHydrologyDialogType);
  public
    function LanguageHasChanged: boolean; 
    function PopulateEditDialog(ANetworkList: TStrings): boolean;
    function Get_SelectedNetworkd(ANetworkList: TStrings): boolean;
    property DialogType : TImportHydrologyDialogType read FDialogType write Set_DialogType;
  end;

  var
    frmImportHydrologyStudies : TfrmImportHydrologyStudies;

implementation

uses
  UUtilities,
  UDataSetType,
  UErrorHandlingOperations;

{$R *.dfm}

{ TfrmStudyCopyForm }

procedure TfrmImportHydrologyStudies.FormCreate(Sender: TObject);
const OPNAME = 'TfrmStudyCopyForm.FormCreate';
begin
  try
    FNameValueItems := TStringList.Create;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TfrmImportHydrologyStudies.FormDestroy(Sender: TObject);
const OPNAME = 'TfrmStudyCopyForm.FormCreate';
begin
  try
    FreeAndNil(FNameValueItems);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TfrmImportHydrologyStudies.LanguageHasChanged: boolean;
const OPNAME = 'TfrmStudyCopyForm.LanguageHasChanged';
begin
  Result := False;
  try
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TfrmImportHydrologyStudies.PopulateEditDialog(ANetworkList: TStrings): boolean;
const OPNAME = 'TfrmStudyCopyForm.PopulateEditDialog';
var
  LIndex : integer;
begin
  Result := False;
  try
    FNameValueItems.Assign(ANetworkList);
    for LIndex := 0 to FNameValueItems.Count-1 do
    begin
      chklstboxHydrologyStudies.Items.Add(FNameValueItems.Names[LIndex]);
      chklstboxHydrologyStudies.Checked[LIndex] := True;
    end;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TfrmImportHydrologyStudies.Get_SelectedNetworkd(ANetworkList: TStrings): boolean;
const OPNAME = 'TfrmStudyCopyForm.Get_SelectedNetworkd';
var
  LIndex : integer;
begin
  Result := False;
  try
    ANetworkList.Clear;
    for LIndex := 0 to chklstboxHydrologyStudies.Items.Count-1 do
    begin
      if chklstboxHydrologyStudies.Checked[LIndex] then
       ANetworkList.Add(FNameValueItems[LIndex])
    end;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TfrmImportHydrologyStudies.chkboxInvertSelectionClick(Sender: TObject);
const OPNAME = 'TfrmStudyCopyForm.chkboxInvertSelectionClick';
var
  LIndex : integer;
begin
  try
    for LIndex := 0 to chklstboxHydrologyStudies.Items.Count-1 do
    begin
      chklstboxHydrologyStudies.Checked[LIndex] := not chklstboxHydrologyStudies.Checked[LIndex];
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TfrmImportHydrologyStudies.Set_DialogType(AValue: TImportHydrologyDialogType);
const OPNAME = 'TfrmStudyCopyForm.Set_DialogType';
begin
  try
    FDialogType := AValue;
    if(FDialogType = dtLink) then
      Self.Caption := 'Select Hydrology Study/ies to Access'
    else
      Self.Caption := 'Select Hydrology Study/ies to Discard';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
