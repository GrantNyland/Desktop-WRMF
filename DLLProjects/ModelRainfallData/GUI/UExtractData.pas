unit UExtractData;

interface

uses
  Classes,
  ExtCtrls,
  StdCtrls,
  Forms;

type
  TExtractData = class(TObject)
  protected

    { protected - can be private }
    FForm : TForm;
    FOutFormat : TRadioGroup;
    FOutQuality : TRadioGroup;
    FBtnExtract : TButton;
    FBtnClose : TButton;
    FDoOnExtractRequest : TNotifyEvent;
    FDoOnCloseRequest : TNotifyEvent;
  protected

    { protected }
    procedure CreateMemberObjects; virtual;
    procedure InitialiseMemberObjects; virtual;
    procedure DestroyMemberObjects; virtual;
    procedure DoOnExtractRequest(ASender : TObject);
    procedure DoOnCloseRequest(ASender : TObject);
    procedure DoOnFormResize(ASender : TObject);
  public

    { public }
    constructor Create;
    destructor Destroy; override;
    procedure ShowForm;
    procedure ShowFormModal;
    procedure HideForm;

    property OnExtractRequest : TNotifyEvent read FDoOnExtractRequest write FDoOnExtractRequest;
    property OnCloseRequest : TNotifyEvent read FDoOnCloseRequest write FDoOnCloseRequest;

    property OutFormat : TRadioGroup read FOutFormat;
    property OutQuality : TRadioGroup read FOutQuality;
  end;


implementation

{ TExtractData }

uses
  // Delphi VCL, RTL, etc
  SysUtils,
  Math,
  Dialogs,
  // arivia.kom
  UShellExecuteObject,
  UErrorHandlingOperations,
  UAbstractComponent;

constructor TExtractData.Create;
const OPNAME = 'TExtractData.Create';
begin
  inherited Create;
  try
    CreateMemberObjects;
    InitialiseMemberObjects;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TExtractData.CreateMemberObjects;
const OPNAME = 'TExtractData.CreateMemberObjects';
begin
  try
    FForm := TForm.CreateNew(nil);
    
    FOutFormat := TRadioGroup.Create(FForm);
    FOutQuality := TRadioGroup.Create(FForm);
    FBtnExtract := TButton.Create(FForm);
    FBtnClose := TButton.Create(FForm);

    FBtnExtract.Parent := FForm;
    FBtnClose.Parent := FForm;
    FOutFormat.Parent := FForm;
    FOutQuality.Parent := FForm;

  except on E : Exception do HandleError(E,OPNAME); end;
end;

destructor TExtractData.Destroy;
const OPNAME = 'TExtractData.Destroy';
begin
  try
    DestroyMemberObjects;
  except on E : Exception do HandleError(E,OPNAME); end;
  inherited Destroy;
end;

procedure TExtractData.DestroyMemberObjects;
const OPNAME = 'TExtractData.DestroyMemberObjects';
begin
  try
    if Assigned(FForm) then
      FForm.Release;
    FreeAndNil(FForm);
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TExtractData.DoOnCloseRequest(ASender: TObject);
const OPNAME = 'TExtractData.DoOnCloseRequest';
begin
  try

    if Assigned(FDoOnCloseRequest) then
      FDoOnCloseRequest(ASender);

    HideForm;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TExtractData.DoOnExtractRequest(ASender: TObject);
const OPNAME = 'TExtractData.DoOnExtractRequest';
var
  LFileStream : TFileStream;
  LStringList : TStringList;
  LCurrentDir : string;
  LMissingFiles : string;
  LIndex : integer;
begin
  try
    TButton(ASender).Enabled := False;
    try
      if Assigned(FDoOnExtractRequest) then
        FDoOnExtractRequest(ASender);

      LCurrentDir := ExtractFilePath(Application.ExeName) + 'WRCDATA\';

          //Check for data[N.B. ], index[N.B.] and exe files exist
        LStringList := TStringList.Create;
        try
          LStringList.Clear;
          LStringList.Add(LCurrentDir + 'rainfall.dat');
          LStringList.Add(LCurrentDir + 'rainfall.idx');
          LStringList.Add(LCurrentDir + 'br_sawb.exe');

          LMissingFiles := '';
          for LIndex := 0 to LStringList.Count -1 do
          begin
            if not(FileExists(LStringList.Strings[LIndex])) then
            begin
              LMissingFiles := LMissingFiles + #10#13 + ExtractFileName(LStringList.Strings[LIndex]);
            end;
          end;

          if (LMissingFiles <> '') then
          begin
            //ShowMessageFmt(FAppModules.Language.GetString('RDDataExtract.ErrorMsg'),[LMissingFiles]);
            ShowMessage('Following file(s) are missing: ' + LMissingFiles + ' and can be found on the WRC Project K5/1155/0/1 cd');
            Exit;
          end;
        finally
          FreeAndNil(LStringList);
        end;

      LFileStream := TFileStream.Create(LCurrentDir + 'INPUTTO_BR_SAWB.TXT', fmCreate or fmShareDenyWrite);
      LStringList := TStringList.Create;
      try
        LStringList.Clear;
        LStringList.Add(IntToStr(FOutFormat.ItemIndex + 1));
        LStringList.Add(IntToStr(FOutQuality.ItemIndex + 1));
        LStringList.Add('2');
        LStringList.Add('0');
        LStringList.Add(' ');
        LStringList.SaveToStream(LFileStream);
      finally
        FreeAndNil(LFileStream);
        FreeAndNil(LStringList);
      end;

      LFileStream := TFileStream.Create(LCurrentDir + 'LAUNCH_BR_SAWB.BAT', fmCreate or fmShareDenyWrite);
      LStringList := TStringList.Create;
      try
        LStringList.Clear;
        LStringList.Add('@CLS ');
        LStringList.Add('@BR_SAWB.EXE < INPUTTO_BR_SAWB.TXT ');
        LStringList.Add(' ');
        LStringList.SaveToStream(LFileStream);
      finally
        FreeAndNil(LFileStream);
        FreeAndNil(LStringList);
      end;

      DeleteFile(LCurrentDir + FAppModules.Language.GetString('Rainfall.DelFileYes'));
      DeleteFile(LCurrentDir + FAppModules.Language.GetString('Rainfall.DelFileNo'));

      TShellExecuteObject.ExecuteShellAction('OPEN', 'LAUNCH_BR_SAWB.BAT', '', LCurrentDir);
      ShowMessage(FAppModules.Language.GetString('Message.DataIsBeingExtracted'));

    finally
      Sleep(1000);
      TButton(ASender).Enabled := True;
      FForm.Close;
    end;
  except on E:Exception do HandleError(E,OPNAME) end;
end;

procedure TExtractData.DoOnFormResize(ASender: TObject);
const OPNAME = 'TExtractData.DoOnFormResize';
begin
  try
    FForm.ClientHeight := FOutFormat.Height + FOutQuality.Height + FBtnClose.Height + 5 * 4;

    FOutFormat.Top := 5;
    FOutQuality.Top := FOutFormat.Top + FOutFormat.Height + 5;
    FBtnExtract.Top := FForm.ClientHeight - FBtnExtract.Height - 5;
    FBtnClose.Top := FForm.ClientHeight - FBtnClose.Height - 5;

    FOutFormat.Left := 5;
    FOutQuality.Left := 5;
    FBtnClose.Left := FForm.ClientWidth - FBtnClose.Width - 5;
    FBtnExtract.Left := FBtnClose.Left - FBtnExtract.Width - 5;

  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TExtractData.HideForm;
const OPNAME = 'TExtractData.HideForm';
begin
  try
    if Assigned(FForm) then
      if FForm.Visible then
        FForm.Close;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TExtractData.InitialiseMemberObjects;
const OPNAME = 'TExtractData.InitialiseMemberObjects';
begin
  try
    FDoOnExtractRequest := nil;
    FDoOnCloseRequest := nil;
    FForm.OnResize := nil;

    FBtnExtract.Caption := FAppModules.Language.GetString('ButtonCaption.Extract');
    FBtnClose.Caption := FAppModules.Language.GetString('ButtonCaption.Close');
    FForm.Caption := Application.Title;

    FOutFormat.Width := 225;
    FOutFormat.Height := 75;

    FOutQuality.Width := 325;
    FOutQuality.Height := 95;

    FOutFormat.Caption := FAppModules.Language.GetString('RadioGroupCaption.OutputInWhichFormat');
    FOutFormat.Items.Clear;
    FOutFormat.Items.Add('1.....SAWB/SDL');
    FOutFormat.Items.Add('2.....ACRU single format');
    FOutFormat.Items.Add('3.....ASCII columnar');

    FOutQuality.Caption := FAppModules.Language.GetString('RadioCaption.Quality');
    FOutQuality.Items.Clear;
    FOutQuality.Items.Add('1....all data');
    FOutQuality.Items.Add('2....only observed data');
    FOutQuality.Items.Add('3....exclude ARC data');
    FOutQuality.Items.Add('4....exclude JCS, Kevin, Mpatched and e-999 (i.e. b,c,d,e)');

    FBtnExtract.OnClick := DoOnExtractRequest;
    FBtnClose.OnClick := DoOnCloseRequest;
    FForm.OnResize := DoOnFormResize;

    FForm.Position := poScreenCenter;
    FForm.BorderStyle := bsToolWindow;
    FForm.ClientWidth := Max(FOutFormat.Width + 10, FOutQuality.Width + 10);

    FOutFormat.ItemIndex := 2;
    FOutQuality.ItemIndex := 0;

  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TExtractData.ShowForm;
const OPNAME = 'TExtractData.ShowForm';
begin
  try
    if Assigned(FForm) then
      if not FForm.Visible then
        FForm.Show;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TExtractData.ShowFormModal;
const OPNAME = 'TExtractData.ShowFormModal';
begin
  try
    if Assigned(FForm) then
      if not FForm.Visible then
        FForm.ShowModal;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

end.




