unit UPopupDialogForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.Contnrs,
  System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.ComCtrls, Vcl.StdCtrls, Vcl.ExtCtrls,

  //UFrame,
  //UFrameInterface,
  UTabForm,
  UAbstractObject,
  UDataComponent,
  UAbstractComponent;

type
  TFormClass = class of TForm;

  IForm = interface
  ['{23DFCDC7-A395-4954-9BC5-CA43AA945CEE}']

end;

 TIForm = class(TInterfacedObject, IForm)
 private
   ContainedForm : TForm;
 public
   constructor Create(AFormClass : TFormClass);
   destructor Destroy; override;
 end;

type
  TBtnClickToPopupForm = procedure(ASender :TObject;AValidator: TAbstractDataDialogValidator) of object;
  TfrmPopupDialog = class(TForm)
    btnOK: TButton;
    btnCancel: TButton;
    pnlButton: TPanel;
    tcMain: TTabControl;

    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormResize(Sender: TObject);
  
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    //FCurrentTabSheet : TAbstractDataDialogValidator;
    FValidatorList : TObjectList;
    FTabIndex : integer;
    FrmContainer : TObjectList;
    FOnBtnClickToPopupForm : TBtnClickToPopupForm;
    //function GetClickedBtnByIndex(AIndex : integer) : TButton;
    procedure AddTab(ATabname: String; ATabSheet: TAbstractDataTabSheet);
    procedure DoOnchange(Sender: TObject);
    procedure DoOnchanging(Sender: TObject; var AllowChange: Boolean);
    //procedure DoBtnClick(Sender: TObject);
    function GetCurrentTabSheet(AIndex: Integer): TAbstractDataTabSheet;
    //function GetCurrentForm(AIndex: Integer): TForm;
    function GetTabSheetByName(ATabCaption : string; var ATabIndex : integer): TAbstractDataTabSheet;
    function GetFormByIndex(AIndex : integer) : TForm;

  public
    FCanCloseForm : boolean;
    function Initialise: boolean;
    function CanShowValidators : boolean;
    procedure AddValidator(AValidator: TAbstractDataDialogValidator);
    procedure FormProc;
    property CurrentTabSheet[AIndex : integer] : TAbstractDataTabSheet read GetCurrentTabSheet;
    property TabIndex : integer read FTabIndex write FTabIndex;
    property CanCloseForm : boolean read FCanCloseForm;
    property OnBtnClickToPopupForm : TBtnClickToPopupForm read FOnBtnClickToPopupForm write FOnBtnClickToPopupForm;
  end;

var
  frmPopupDialog: TfrmPopupDialog;

implementation
  Uses
    UErrorHandlingOperations;


{$R *.dfm}



function TfrmPopupDialog.Initialise: boolean;
const OPNAME = 'TfrmPopupDialog.Initialise';
begin
  Result := False;
  try
    Self.Position := poScreenCenter;
    Self.FormStyle := fsNormal;
    Self.Height := 548;
    Self.Width := 848;
    BorderStyle := bsSizeToolWin;
    Caption := Application.Title;
    FTabIndex := 0;
    tcMain.OnChange :=  DoOnchange;
    tcMain.OnChanging := DoOnchanging;
    FrmContainer.Clear;
    FValidatorList.Clear;
    FCanCloseForm := True;
    //OnBtnClickToPopupForm := DoOnBtnClickToPopupForm;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TfrmPopupDialog.GetTabSheetByName(ATabCaption : string; var ATabIndex : integer): TAbstractDataTabSheet;
const OPNAME = 'TfrmPopupDialog.GetTabSheetByName';
var
  LIndex : integer;
begin
  Result := nil;
  try
    LIndex := tcMain.Tabs.IndexOf(ATabCaption);
    if (LIndex >=0) then
      Result := TAbstractDataTabSheet(tcMain.Tabs.Objects[LIndex]);
    ATabIndex := LIndex;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TfrmPopupDialog.GetCurrentTabSheet(AIndex: Integer): TAbstractDataTabSheet;
const OPNAME = 'TfrmPopupDialog.GetCurrentTabSheet';
begin
  Result := nil;
  try
    if (AIndex >= 0) and (AIndex <= tcMain.Tabs.Count-1)   then
      Result := tcMain.Tabs.Objects[AIndex] as TAbstractDataTabSheet;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


procedure TfrmPopupDialog.AddValidator(AValidator: TAbstractDataDialogValidator);
const OPNAME = 'TfrmPopupDialog.AddValidator';
var
  LTabSheet :TAbstractDataTabSheet;
  LForm : TForm;
begin
  try

    LTabSheet := TAbstractDataTabSheet.Create(tcMain, nil);
    LTabSheet.Parent := tcMain;
    LForm := TForm.Create(tcMain);
    LForm.Parent := tcMain;
    LForm.OnShow := FormShow;

    AValidator.Panel.Parent := LForm;
    FrmContainer.Add(LForm);
    AValidator.Panel.Align := alClient;
    FValidatorList.Add(AValidator);
    AddTab('Tab '+IntToStr(FValidatorList.Count), LTabSheet);
    LForm.Caption := 'Tab '+IntToStr(FValidatorList.Count);

  except on E: Exception do HandleError(E, OPNAME) end;
end;



{
function TfrmPopupDialog.GetClickedBtnByIndex(AIndex : integer) : TButton;
const OPNAME = 'TfrmPopupDialog.GetClickedBtnByIndex';
begin
  Result := nil;
  try
    if (AIndex >=0) and (AIndex <= FBtnContainer.Count-1)  then
      Result := TButton(FBtnContainer.Items[AIndex]);
  except on E: Exception do HandleError(E, OPNAME) end;
end;
   }

procedure TfrmPopupDialog.DoOnchange(Sender: TObject);
const OPNAME = 'TfrmPopupDialog.DoOnchange';
var
  LForm : TForm;
begin
  try
    LForm := GetFormByIndex(tcMain.TabIndex);
    if Assigned(LForm) then
      FTabIndex := tcMain.TabIndex;
    close;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TfrmPopupDialog.DoOnchanging(Sender: TObject; var AllowChange: Boolean);
const OPNAME = 'TfrmPopupDialog.DoOnchanging';
var
  LForm : TForm;
begin
  try
    AllowChange := True;
    FCanCloseForm := False;
    LForm := GetFormByIndex(tcMain.TabIndex);
    if Assigned(LForm) then
      LForm.Hide;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TfrmPopupDialog.GetFormByIndex(AIndex : integer) : TForm;
const OPNAME = 'TfrmPopupDialog.GetFormByIndex';
begin
  Result := nil;
  try
    if (AIndex >= 0) and (AIndex <= FrmContainer.Count-1) then
      Result := TForm(FrmContainer.Items[AIndex]);
  except on E: Exception do HandleError(E, OPNAME) end;
end;


function TfrmPopupDialog.CanShowValidators: boolean;
const OPNAME = 'TfrmPopupDialog.CanShowValidators';
var
  LIndex,LTabIndex : integer;
  LValidator : TAbstractDataDialogValidator;
  LTabsheet : TAbstractDataTabSheet;
begin
  Result := False;
  try
    for LIndex := 0 to FValidatorList.Count-1 do
    begin
      LValidator := TAbstractDataDialogValidator(FValidatorList.Items[LIndex]);
      LTabsheet := GetTabSheetByName('Tab '+IntToStr(LIndex+1), LTabIndex);
      if (LValidator <> nil) and (LTabsheet <> nil) then
      begin
        LValidator.LanguageHasChanged;
        LTabsheet.Caption := LValidator.TabShetCaption;
        tcMain.Tabs.Delete(LTabIndex);
        AddTab(LTabsheet.Caption, LTabSheet);
        FCanCloseForm := True;
      end;
    end;
    tcMain.Align := alClient;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


procedure TfrmPopupDialog.FormClose(Sender: TObject; var Action: TCloseAction);
const OPNAME = 'TfrmPopupDialog.FormDestroy';
begin
  try
   
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TfrmPopupDialog.FormCreate(Sender: TObject);
const OPNAME = 'TfrmPopupDialog.FormDestroy';
begin
  try
    FValidatorList := TObjectList.Create(False);
    FrmContainer := TObjectList.Create(False);
    //FBtnContainer := TObjectList.Create(False);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TfrmPopupDialog.FormDestroy(Sender: TObject);
const OPNAME = 'TfrmPopupDialog.FormDestroy';
begin
  try
    FreeAndNil(FValidatorList);
    FreeAndNil(FrmContainer);
    //FreeAndNil(FBtnContainer);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TfrmPopupDialog.FormProc;
const OPNAME = 'TfrmPopupDialog.FormProc';
begin
  try

  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TfrmPopupDialog.FormResize(Sender: TObject);
const OPNAME = 'TfrmPopupDialog.FormShow';
begin
  try

 except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TfrmPopupDialog.FormShow(Sender: TObject);
const OPNAME = 'TfrmPopupDialog.FormShow';
var
  LValidator : TAbstractDataDialogValidator;
  LForm : TForm;
begin
  try
    LValidator := TAbstractDataDialogValidator(FValidatorList.Items[FTabIndex]);
    LForm := GetFormByIndex(FTabIndex);
    if (LForm <> nil) and (LValidator <> nil) then
    begin
        LValidator.Panel.Parent := LForm;
      LForm.Align := alClient;
      LForm.BorderStyle := bsNone;
      LForm.Show;
      LForm.BringToFront;
      FCanCloseForm := True;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TfrmPopupDialog.AddTab(ATabname: String; ATabSheet: TAbstractDataTabSheet);
const OPNAME = 'TfrmPopupDialog.AddTab';
begin
  try
    tcMain.Tabs.AddObject(ATabName, ATabSheet);
  except on E: Exception do HandleError(E, OPNAME) end;
end;


{ TIForm }


constructor TIForm.Create(AFormClass: TFormClass);
const OPNAME = 'TIForm.Create';
begin
  try
    ContainedForm := AFormClass.Create(nil);
    ContainedForm.Show;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

destructor TIForm.destroy;
const OPNAME = 'TIForm.destroy';
begin
  try
    if Assigned(ContainedForm) then
     FreeAndNil(ContainedForm);
    inherited;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


end.
