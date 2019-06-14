unit UCreateWrymForm;

interface
{$WARN UNIT_PLATFORM OFF}
uses

  // Delphi VCL, RTL, etc

  Windows,
  Messages,
  SysUtils,
  Variants,
  Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.ExtCtrls,
  Vcl.StdCtrls,
  Vcl.Buttons,
  Vcl.ComCtrls,
  Vcl.FileCtrl,

  // DWAF/arivia.kom
  UAbstractComponent,
  UHelpContexts,
  UStudyObjects,
  UAbstractObject;

type
  TValidationContext = (valcNone, valcPrefix, valcInput, valcOutput,valcParam);
  TfrmCreateWrymForm = class(TAbstractForm)
    pnlButtons: TPanel;
    btnReset: TBitBtn;
    btnCancel: TBitBtn;
    btnCreate: TBitBtn;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    edtFilesPrefix: TEdit;
    edtInputPath: TEdit;
    btnInputPath: TButton;
    edtOutputPath: TEdit;
    btnOutputPath: TButton;
    Label4: TLabel;
    lblParam: TLabel;
    edtParam: TEdit;
    btnParam: TButton;
    odParam: TOpenDialog;
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure edtFilesPrefixKeyPress(Sender: TObject; var Key: Char);
    procedure btnResetClick(Sender: TObject);
    procedure btnCreateClick(Sender: TObject);
    procedure edtOutputPathExit(Sender: TObject);
    procedure btnInputPathClick(Sender: TObject);
    procedure btnOutputPathClick(Sender: TObject);
    procedure edtFilesPrefixExit(Sender: TObject);
    procedure edtInputPathExit(Sender: TObject);
    procedure btnParamClick(Sender: TObject);
    procedure edtParamExit(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure PopulateDataAndOutPutDir(APath : string);
  protected
    FModelName     : string;
    FModelVersion  : string;
    FFileName: string;
    function CheckNewFilePath(APath: string): boolean;
    function Validate(LContext:TValidationContext): boolean;
    procedure PopulateForm;
    procedure AssignHelpContext; override;
    //function OnHelpRequest(Command: Word; Data: THelpEventData; var CallHelp: Boolean): Boolean;
  public
    property FileName : string read FFileName write FFileName;
    property ModelVersion : string read FModelVersion write FModelVersion;
    property ModelName : string read FModelName write FModelName;
  end;

implementation

uses
  System.UITypes,
  UUtilities,
  UDataSetType,
  UErrorHandlingOperations;

{$R *.dfm}



{ TfrmCreateWrymForm }

procedure TfrmCreateWrymForm.FormCreate(Sender: TObject);
const OPNAME = 'TfrmCreateWrymForm.FormCreate';
begin
  try
    FFileName := '';
   //OnHelp := OnHelpRequest;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TfrmCreateWrymForm.PopulateForm;
const OPNAME = 'TfrmCreateWrymForm.PopulateForm';
//var
  //LFileContents: TStringList;
begin
  try
    btnCreate.Enabled := False;
    btnReset.Enabled  := True;
    btnCancel.Enabled := True;

    Self.Height  := 270;
    Self.Width   := 500;
    {if (Trim(FFileName) <> '') and FileExists(FFileName) then
    begin
      LFileContents := TStringList.Create;
      try
        LFileContents.LoadFromFile(FFileName);
        if(LFileContents.Count >= 1) then
          edtFilesPrefix.Text := Trim(LFileContents[0]);
        if(LFileContents.Count >= 2) then
         edtInputPath.Text   := Trim(LFileContents[1]);
        if(LFileContents.Count >= 3) then
         edtOutputPath.Text  := Trim(LFileContents[2]);
        Validate(valcNone);
      finally
       FreeAndNil(LFileContents);
      end;
    end
    else
    begin
      edtFilesPrefix.Text := FAppModules.ViewIni.ReadString ( ClassName,'FilesPrefix','');
      edtInputPath.Text   := FAppModules.ViewIni.ReadString ( ClassName,'InputPath','');
      edtOutputPath.Text  := FAppModules.ViewIni.ReadString ( ClassName,'OutputPath','');
      edtParam.Text       := FAppModules.ViewIni.ReadString ( ClassName,'Param','');
    end;
    }
    edtFilesPrefix.Text := '';
    edtInputPath.Text   := '';
    edtOutputPath.Text  := '';
    edtParam.Text       := '';
    edtParam.Enabled    := (ModelName <> CDDTS);
    lblParam.Enabled    := edtParam.Enabled;
    btnParam.Enabled    := edtParam.Enabled;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TfrmCreateWrymForm.btnCreateClick(Sender: TObject);
const OPNAME = 'TfrmCreateWrymForm.btnCreateClick';
var
  LFileContents                  : TStringList;
  LFileName                      : string;
  LParamName                     : string;
  LPeriod                        : integer;
  LStrPeriod                     : ShortString;
  LYears                         : integer;
  LStrYears                      : ShortString;
  LStartYearGregorian            : integer;
  LStrStartYearGregorian         : ShortString;
  FGroundWaterFeatureImplemented : boolean;
begin
  try
    if Validate(valcNone) then
    begin
      LFileContents := TStringList.Create;
      try
        //WRYM.DAT
        LFileContents.Add(Trim(edtFilesPrefix.Text));
        LFileContents.Add(Trim(edtInputPath.Text));
        LFileContents.Add(Trim(edtOutputPath.Text));
        LParamName := Trim(edtParam.Text);
        LPeriod := 0;
        LStartYearGregorian := 1920;
        FFileName := IncludeTrailingPathDelimiter(Trim(edtInputPath.Text))+'WRYM.dat';
        if(ModelName = CPlanning) then
          FFileName := IncludeTrailingPathDelimiter(Trim(edtInputPath.Text))+'WRPM.dat';
         if(ModelName = CDDTS) then
          FFileName := IncludeTrailingPathDelimiter(Trim(edtInputPath.Text))+'DDTS.dat';
        LFileContents.SaveToFile(FFileName);
        GetAnalysisYearsFromParamDataFile(LParamName,LPeriod,LStartYearGregorian);
        if(ModelName = CDDTS) then
        begin
          Self.ModalResult := mrOk;
          Exit;
        end;

        LStrPeriod := '      ';
        Str(LPeriod:6,LStrPeriod);
        LYears := LPeriod * 12;
        LStrYears  := '      ';
        Str(LYears:6,LStrYears);
        LStrStartYearGregorian  := '      ';
        Str(LStartYearGregorian:6,LStrStartYearGregorian);

        //F01.DAT
        LFileContents.Clear;
        LFileContents.Add('RUNTITLE1');
        LFileContents.Add('RUNTITLE2');
        LFileContents.Add('RUNTITLE3');                              
        LFileContents.Add( string(LStrYears) + '     1'+ string(LStrStartYearGregorian) +'    1'+ string(LStrYears) + '    -3     0     1     0     1     N     0     0     0     0');
        LFileContents.Add('   OCT   NOV   DEC   JAN   FEB   MAR   APR   MAY   JUN   JUL   AUG   SEP');
        LFileContents.Add(' 31.00 30.00 31.00 31.00 28.25 31.00 30.00 31.00 30.00 31.00 31.00 30.00');
        LFileContents.Add( string(LStrPeriod) + '     1     1     1     H     0    '+ Trim(edtParam.Text));
        LFileContents.Add('     1');
        LFileContents.Add('    0.');
        LFileContents.Add('    0.');
        LFileContents.Add('    0.');
        LFileContents.Add('     0');
        LFileName := IncludeTrailingPathDelimiter(Trim(edtInputPath.Text))+ Trim(edtFilesPrefix.Text) + 'F01.dat';
        LFileContents.SaveToFile(LFileName);

        //F02.DAT
        LFileContents.Clear;
        LFileContents.Add('    0  MCM');
        LFileName := IncludeTrailingPathDelimiter(Trim(edtInputPath.Text))+ Trim(edtFilesPrefix.Text) + 'F02.dat';
        LFileContents.SaveToFile(LFileName);

        //F03.DAT
        LFileContents.Clear;
        LFileContents.Add('    0');
        LFileContents.Add('    0');
        LFileContents.Add('    0');
        LFileContents.Add('    0');
        LFileContents.Add('    0');
        LFileContents.Add('    0');
        LFileContents.Add('    0');
        LFileContents.Add('    0');
        LFileContents.Add('    0');
        LFileContents.Add('    0');
        LFileContents.Add('    0');
        LFileContents.Add('    0');
        LFileContents.Add('    0');
        if (FModelVersion = '7') then
        begin
          LFileContents.Add('    0');
          LFileContents.Add('    0');
          LFileContents.Add('    0');

          FGroundWaterFeatureImplemented := Boolean(FAppModules.IniFile.ReadInteger(ClassName, 'GroundWaterFeatureImplemented',1));
          if FGroundWaterFeatureImplemented then
            LFileContents.Add('    0');
        end;  
        LFileName := IncludeTrailingPathDelimiter(Trim(edtInputPath.Text))+ Trim(edtFilesPrefix.Text) + 'F03.dat';
        LFileContents.SaveToFile(LFileName);

        //F04.DAT
        LFileContents.Clear;
        LFileContents.Add('    0');
        LFileName := IncludeTrailingPathDelimiter(Trim(edtInputPath.Text))+ Trim(edtFilesPrefix.Text) + 'F04.dat';
        LFileContents.SaveToFile(LFileName);

        //F05.DAT
        LFileContents.Clear;
        LFileContents.Add('    0    0    0');
        LFileName := IncludeTrailingPathDelimiter(Trim(edtInputPath.Text))+ Trim(edtFilesPrefix.Text) + 'F05.dat';
        LFileContents.SaveToFile(LFileName);

        //F06.DAT
        LFileContents.Clear;
        LFileContents.Add('');
        LFileName := IncludeTrailingPathDelimiter(Trim(edtInputPath.Text))+ Trim(edtFilesPrefix.Text) + 'F06.dat';
        LFileContents.SaveToFile(LFileName);

        //F07.DAT
        LFileContents.Clear;
        LFileContents.Add('');
        LFileContents.Add('');
        LFileContents.Add('');
        LFileName := IncludeTrailingPathDelimiter(Trim(edtInputPath.Text))+ Trim(edtFilesPrefix.Text) + 'F07.dat';
        LFileContents.SaveToFile(LFileName);

        //F08.DAT
        LFileContents.Clear;
        LFileContents.Add('');
        LFileContents.Add('');
        LFileContents.Add('');
        LFileContents.Add('');
        LFileContents.Add('');
        LFileName := IncludeTrailingPathDelimiter(Trim(edtInputPath.Text))+ Trim(edtFilesPrefix.Text) + 'F08.dat';
        LFileContents.SaveToFile(LFileName);

        //F09.DAT
        LFileContents.Clear;
        LFileContents.Add('');
        LFileContents.Add('');
        LFileContents.Add('');
        LFileName := IncludeTrailingPathDelimiter(Trim(edtInputPath.Text))+ Trim(edtFilesPrefix.Text) + 'F09.dat';
        LFileContents.SaveToFile(LFileName);

        //F10.DAT
        LFileContents.Clear;
        LFileContents.Add('');
        LFileContents.Add('');
        LFileContents.Add('');
        LFileName := IncludeTrailingPathDelimiter(Trim(edtInputPath.Text))+ Trim(edtFilesPrefix.Text) + 'F10.dat';
        LFileContents.SaveToFile(LFileName);

        //F11.DAT
        LFileContents.Clear;
        LFileContents.Add('');
        LFileContents.Add('');
        LFileContents.Add('');
        LFileName := IncludeTrailingPathDelimiter(Trim(edtInputPath.Text))+ Trim(edtFilesPrefix.Text) + 'F11.dat';
        LFileContents.SaveToFile(LFileName);

        //F12.DAT
        LFileContents.Clear;
        LFileContents.Add('');
        LFileContents.Add('');
        LFileName := IncludeTrailingPathDelimiter(Trim(edtInputPath.Text))+ Trim(edtFilesPrefix.Text) + 'F12.dat';
        LFileContents.SaveToFile(LFileName);

        //F13.DAT
        LFileContents.Clear;
        LFileContents.Add(' ');
        LFileName := IncludeTrailingPathDelimiter(Trim(edtInputPath.Text))+ Trim(edtFilesPrefix.Text) + 'F13.dat';
        LFileContents.SaveToFile(LFileName);

        //F14.DAT
        LFileContents.Clear;
        LFileContents.Add('    0    1');
        LFileContents.Add('');
        LFileName := IncludeTrailingPathDelimiter(Trim(edtInputPath.Text))+ Trim(edtFilesPrefix.Text) + 'F14.dat';
        LFileContents.SaveToFile(LFileName);

        //F16.DAT
        LFileContents.Clear;
        LFileContents.Add('    0    0    0');
        LFileContents.Add('    0');
        LFileContents.Add('    0');
        LFileName := IncludeTrailingPathDelimiter(Trim(edtInputPath.Text))+ Trim(edtFilesPrefix.Text) + 'F16.dat';
        LFileContents.SaveToFile(LFileName);

      finally
       FreeAndNil(LFileContents);
      end;
      Self.ModalResult := mrOk;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TfrmCreateWrymForm.FormShow(Sender: TObject);
const OPNAME = 'TfrmCreateWrymForm.FormShow';
begin
  try
    PopulateForm;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TfrmCreateWrymForm.btnResetClick(Sender: TObject);
const OPNAME = 'TfrmCreateWrymForm.btnResetClick';
begin
  try
    PopulateForm;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TfrmCreateWrymForm.Validate(LContext:TValidationContext): boolean;
const OPNAME = 'TfrmCreateWrymForm.Validate';
begin
  Result := False;
  try
    btnCreate.Enabled := Result;
    Label4.Caption := '';
    case LContext of
      valcPrefix:
        begin
          if(Trim(edtFilesPrefix.Text) = '')  then
          begin
            Label4.Caption := FAppModules.Language.GetString('LabelText.PrefixIsEmpty');
            Exit;
          end;
        end;
      valcInput :
        begin
          if(Trim(edtInputPath.Text) = '')  then
          begin
            Label4.Caption := FAppModules.Language.GetString('LabelText.PathIsEmpty');
            Exit;
          end;
          if not SysUtils.DirectoryExists(Trim(edtInputPath.Text))  then
          begin
            Label4.Caption := FAppModules.Language.GetString('LabelText.PathDoesNotExist');
            Exit;
          end;
          if not FilePathIsDosCompatible(FAppModules,Trim(edtInputPath.Text))  then
          begin
            Label4.Caption := FAppModules.Language.GetString('LabelText.CompatibleWithDos');
            Exit;
          end;
        end;
      valcOutput:
        begin
          if(Trim(edtOutputPath.Text) = '')  then
          begin
            Label4.Caption := FAppModules.Language.GetString('LabelText.FilesPathEmpty');
            Exit;
          end;
          if not SysUtils.DirectoryExists(Trim(edtOutputPath.Text))  then
          begin
            Label4.Caption := FAppModules.Language.GetString('LabelText.FilesPathNotExist');
            Exit;
          end;
          if not FilePathIsDosCompatible(FAppModules,Trim(edtOutputPath.Text))  then
          begin
            Label4.Caption := FAppModules.Language.GetString('LabelText.OutputFilesNotCompatible');
            Exit;
          end;
        end;
      valcParam:
        begin
          if(Trim(edtParam.Text) = '') and (ModelName <> CDDTS)  then
          begin
            Label4.Caption := FAppModules.Language.GetString('LabelText.ParamFileEmpty');
            Exit;
          end;
          if not FileExists(Trim(edtParam.Text)) and (ModelName <> CDDTS)   then
          begin
            Label4.Caption := FAppModules.Language.GetString('LabelText.ParamFileNotExist');
            Exit;
          end;
        end
      else
      begin
        if(Trim(edtFilesPrefix.Text) = '')  then
        begin
          Label4.Caption := FAppModules.Language.GetString('LabelText.PrefixIsEmpty');
          Exit;
        end;

        if(Trim(edtInputPath.Text) = '')  then
        begin
          Label4.Caption := FAppModules.Language.GetString('LabelText.PathIsEmpty');
          Exit;
        end;
        if not SysUtils.DirectoryExists(Trim(edtInputPath.Text))  then
        begin
          Label4.Caption := FAppModules.Language.GetString('LabelText.PathDoesNotExist');
          Exit;
        end;
        if not FilePathIsDosCompatible(FAppModules,Trim(edtInputPath.Text))  then
        begin
          Label4.Caption := FAppModules.Language.GetString('LabelText.CompatibleWithDos');
          Exit;
        end;

        if(Trim(edtOutputPath.Text) = '')  then
        begin
          Label4.Caption := FAppModules.Language.GetString('LabelText.FilesPathEmpty');
          Exit;
        end;
        if not SysUtils.DirectoryExists(Trim(edtOutputPath.Text))  then
        begin
          Label4.Caption := FAppModules.Language.GetString('LabelText.FilesPathNotExist');
          Exit;
        end;
        if not FilePathIsDosCompatible(FAppModules,Trim(edtOutputPath.Text))  then
        begin
          Label4.Caption := FAppModules.Language.GetString('LabelText.OutputFilesNotCompatible');
          Exit;
        end;

        if(Trim(edtParam.Text) = '') and (ModelName <> CDDTS)  then
        begin
          Label4.Caption := FAppModules.Language.GetString('LabelText.ParamFileEmpty');
          Exit;
        end;
        if not FileExists(Trim(edtParam.Text)) and (ModelName <> CDDTS)  then
        begin
          Label4.Caption := FAppModules.Language.GetString('LabelText.ParamFileNotExist');
          Exit;
        end;
      end;
    end;//case

    if (Trim(edtFilesPrefix.Text) <> '') and
      SysUtils.DirectoryExists(Trim(edtInputPath.Text)) and
      SysUtils.DirectoryExists(Trim(edtOutputPath.Text)) and
      FileExists(Trim(edtParam.Text))    then
      btnCreate.Enabled := True
    else
    if (Trim(edtFilesPrefix.Text) <> '') and
      SysUtils.DirectoryExists(Trim(edtInputPath.Text)) and
      SysUtils.DirectoryExists(Trim(edtOutputPath.Text)) and(ModelName = CDDTS) then
       btnCreate.Enabled := True;
    Result := btnCreate.Enabled;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TfrmCreateWrymForm.btnInputPathClick(Sender: TObject);
const OPNAME = 'TfrmCreateWrymForm.btnInputPathClick';
var
  LPath: string;
begin
  try
    LPath := Trim(edtInputPath.Text);
    if SelectDirectory(LPath,[sdAllowCreate, sdPerformCreate,sdPrompt],0) then
    begin
      edtInputPath.Text := ExcludeTrailingPathDelimiter(LPath);
      Validate(valcInput);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TfrmCreateWrymForm.btnOutputPathClick(Sender: TObject);
const OPNAME = 'TfrmCreateWrymForm.btnOutputPathClick';
var
  LPath: string;
begin
  try
    LPath := Trim(edtOutputPath.Text);
    if(LPath = '') then
      LPath := Trim(edtInputPath.Text);
    if SelectDirectory(LPath,[sdAllowCreate, sdPerformCreate,sdPrompt],0) then
    begin
      edtOutputPath.Text := ExcludeTrailingPathDelimiter(LPath);
      Validate(valcOutput);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TfrmCreateWrymForm.btnParamClick(Sender: TObject);
const OPNAME = 'TfrmCreateWrymForm.btnParamClick';
var
  LPath: string;
begin
  try
    LPath := ExtractFilePath(edtParam.Text);
    if(LPath = '') then
      LPath := Trim(edtOutputPath.Text);
    if(LPath = '') then
      LPath := Trim(edtInputPath.Text);
    odParam.InitialDir := LPath;
    if odParam.Execute then
      edtParam.Text := odParam.FileName;
    PopulateDataAndOutPutDir(ExtractFilePath(edtParam.Text));
    Validate(valcParam);

  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TfrmCreateWrymForm.edtFilesPrefixKeyPress(Sender: TObject;var Key: Char);
const OPNAME = 'TfrmCreateWrymForm.edtFilesPrefixKeyPress';
begin
  try
    if(Key <> #8) and (Length(edtFilesPrefix.Text) >= 5) then
    begin
      edtFilesPrefix.Text := Copy(Trim(edtFilesPrefix.Text),1,5);
      Key := #0;
      Beep;
    end
    else
    edtFilesPrefix.Text := Trim(edtFilesPrefix.Text);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TfrmCreateWrymForm.CheckNewFilePath(APath: string): boolean;
const OPNAME = 'TfrmCreateWrymForm.CheckNewFilePath';
var
  LMesg: string;
begin
  Result := False;
  try
    if(Trim(APath) = '') then
      Exit;
    if not SysUtils.DirectoryExists(APath)then
    begin
      if not FilePathIsDosCompatible(FAppModules,Trim(APath)) then
        Label4.Caption := FAppModules.Language.GetString('LabelText.PathBracket')+APath+FAppModules.Language.GetString('LabelText.DosApplications')
      else
      begin
        LMesg := FAppModules.Language.GetString('LabelText.DirectoryNotExist')+APath+FAppModules.Language.GetString('LabelText.CreatingIt');
        if (MessageDlg(LMesg,mtConfirmation,[mbYes, mbNo],0) = mrYes) then
          if not SysUtils.ForceDirectories(APath) then
            Label4.Caption := FAppModules.Language.GetString('LabelText.PathBracket')+APath+FAppModules.Language.GetString('LabelText.PleaseSpecify');
      end;
    end;
    Result := SysUtils.DirectoryExists(APath);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TfrmCreateWrymForm.edtFilesPrefixExit(Sender: TObject);
const OPNAME = 'TfrmCreateWrymForm.edtFilesPrefixExit';
begin
  try
    Validate(valcPrefix);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TfrmCreateWrymForm.edtInputPathExit(Sender: TObject);
const OPNAME = 'TfrmCreateWrymForm.edtInputPathExit';
begin
  try
    if CheckNewFilePath(Trim(edtInputPath.Text)) then
      Validate(valcInput);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TfrmCreateWrymForm.edtOutputPathExit(Sender: TObject);
const OPNAME = 'TfrmCreateWrymForm.edtOutputPathExit';
begin
  try
    if CheckNewFilePath(Trim(edtOutputPath.Text)) then
      Validate(valcOutput);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TfrmCreateWrymForm.edtParamExit(Sender: TObject);
const OPNAME = 'TfrmCreateWrymForm.edtParamExit';
begin
  try
    Validate(valcParam);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TfrmCreateWrymForm.AssignHelpContext;
const OPNAME = 'TfrmCreateWrymForm.AssignHelpContext';
begin
  try
    SetControlHelpContext(Self,           HC_WaterResourcesYieldModel);
    SetControlHelpContext(edtFilesPrefix, HC_RunDescription);
    SetControlHelpContext(edtInputPath,   HC_DataFileLocation);
    SetControlHelpContext(edtOutputPath,  HC_DataFileLocation);
    SetControlHelpContext(edtParam,       HC_DataFileLocation);
  except on E: Exception do HandleError(E, OPNAME); end;
end;
{
function TfrmCreateWrymForm.OnHelpRequest(Command: Word; Data: THelpEventData;var CallHelp: Boolean): Boolean;
const OPNAME = 'TfrmCreateWrymForm.OnHelpRequest';
begin
  Result := FALSE;
  try
    if(Data = HC_WaterResourcesYieldModel) or (Data = HC_RunDescription) or
      (Data = HC_DataFileLocation) then
      Application.OnHelp(Command,Data,CallHelp);
    //Application.HelpContext(Data);

    CallHelp := True;
    Result   := True;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;
}

procedure TfrmCreateWrymForm.FormClose(Sender: TObject; var Action: TCloseAction);
const OPNAME = 'TfrmCreateWrymForm.FormClose';
begin
  try
    FAppModules.ViewIni.WriteString(ClassName,'FilesPrefix',edtFilesPrefix.Text);
    FAppModules.ViewIni.WriteString(ClassName,'InputPath',edtInputPath.Text);
    FAppModules.ViewIni.WriteString(ClassName,'OutputPath',edtOutputPath.Text);
    FAppModules.ViewIni.WriteString(ClassName,'Param',edtParam.Text);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TfrmCreateWrymForm.PopulateDataAndOutPutDir(APath : string);
const OPNAME = 'TfrmCreateWrymForm.PopulateDataAndOutPutDir';
var
  LPath : string;
  LPos : integer;
begin
  try
    if APath <> '' then
    begin
      LPath := ExcludeTrailingPathDelimiter(APath);
      LPath := ExtractFilePath(LPath);
      if ((edtOutputPath.Text = '') or (edtInputPath.Text = '')) and (LPath <> '') then
      begin
        LPos := Pos('HYDRO',UpperCase(LPath));
        if (LPos > 0) then
          Delete(LPath,LPos,Length(LPath));
        if edtInputPath.Text = '' then
          edtInputPath.Text := LPath+'Data';
        SysUtils.ForceDirectories(edtInputPath.Text);
        if edtOutputPath.Text = '' then
          edtOutputPath.Text := LPath+'Output';
        SysUtils.ForceDirectories(edtOutputPath.Text);
      end;
    end;  
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
