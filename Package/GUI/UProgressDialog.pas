//
//
//  UNIT      : Contains the class TProgressDialog.
//  AUTHOR    : Dziedzi Ramulondi (PDNA)
//  DATE      : 2002/03/05
//  COPYRIGHT : Copyright © 2002 DWAF
//
//
unit UProgressDialog;


interface


{$R *.DFM}


//
// Interface dependencies
//
uses
  // DWAF
  UConstants,
  UAbstractObject,
  UHelpContexts,
  UAbstractComponent,

  // Delphi
  Vcl.Forms,
  Windows,
  Classes,
  Vcl.Controls,
  Vcl.ComCtrls,
  Vcl.ExtCtrls,
  Vcl.StdCtrls,
  Vcl.Buttons,
  Contnrs,
  Vcl.Dialogs,
  Vcl.CheckLst;

//
// This dialog is used to select a study area.
//
type
  TProgressDialog = class(TAbstractForm)
    FButtonPanel      : TPanel;
    FOKButton         : TBitBtn;
    FStartButton      : TBitBtn;
    FBackPanel        : TPanel;
    ProgressRichEdit  : TRichEdit;
    FStopButton       : TBitBtn;
    ActionProgressBar : TProgressBar;
    dlgSaveFile       : TSaveDialog;
    FPrintButton      : TBitBtn;
    FSaveButton       : TBitBtn;
    clbOptions        : TCheckListBox;
    procedure FormActivate(Sender: TObject);
    procedure ShowProgress(AProgress: String; AProgressType: TProgressType; var AStop: boolean; AUpdateSteps: boolean=False);
    procedure ShowProgressInPlace(AOldProgress,ANewProgress: String; AProgressType: TProgressType; var AStop: boolean; AUpdateSteps: boolean=False);
    procedure FStopButtonClick(Sender: TObject);
    procedure FStartButtonClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FPrintButtonClick(Sender: TObject);
    procedure FSaveButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure AssignHelpContext; override;
    procedure FormCanResize(Sender: TObject; var NewWidth,
      NewHeight: Integer; var Resize: Boolean);
    function ButtonSpacing: integer;
    procedure FormShow(Sender: TObject);
  protected
    FErrorCount: integer;
    FSilent,
    FStop: boolean;
    FSuccsess: Boolean;
    FKeepStartBtnDisabled: boolean;
    //FExecFunction:TExecFunction;
    FExecFunctionCount: integer;
    FExecFunctions : TExecFunctionArray;
    FAddedButtons  : TObjectList;
    procedure ClearExecFunction;
    procedure ClearExtraButtons;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
  public
    procedure UpdateProgressBar;
    function Initialise: boolean; override;
    function AddExecutionFunctions(AExecFunction:TExecFunction): boolean;
    function AddButton(ACaption: string): TButton;
    property Silent: Boolean read FSilent write FSilent;
    property Stopped: Boolean read FStop;
    property Succsessful: Boolean read FSuccsess write FSuccsess;
    //property ExecFunction:TExecFunction read FExecFunction write FExecFunction;
    property KeepStartBtnDisabled:boolean read FKeepStartBtnDisabled write FKeepStartBtnDisabled;
    property ErrorCount:integer read FErrorCount write FErrorCount;
  end;


implementation


//
// Implementation dependencies
//
uses

  // Delphi
  SysUtils,
  Vcl.Graphics,

  // DWAF
  UErrorHandlingOperations;

procedure TProgressDialog.FormActivate(Sender: TObject);
const OPNAME = 'TProgressDialog.FormActivate';
begin
  try
   ActionProgressBar.Position := ActionProgressBar.Min;
   FStopButton.Enabled := False;
   FPrintButton.Enabled := ProgressRichEdit.Lines.Count > 0;
   FSaveButton.Enabled := ProgressRichEdit.Lines.Count > 0;
   FOKButton.Enabled := True;
   //FStartButton.Enabled := ProgressRichEdit.Lines.Count = 0;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

Procedure TProgressDialog.AssignHelpContext;
const OPNAME = 'TProgressDialog.AssignHelpContext';
begin
  try
    SetControlHelpContext(Self,             HC_ModelVerificationAndTesting);
    SetControlHelpContext(ProgressRichEdit, HC_ModelVerificationAndTesting);
    SetControlHelpContext(FOKButton,        HC_ExportFilesClose);
    SetControlHelpContext(FStartButton,     HC_ExportFilesStart);
    SetControlHelpContext(FStopButton,      HC_ExportFilesStop);
    SetControlHelpContext(FPrintButton,     HC_ExportFilesPrint);
    SetControlHelpContext(FSaveButton,      HC_ExportFilesSaveToFile);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TProgressDialog.ShowProgress(AProgress: String; AProgressType: TProgressType; var AStop: boolean; AUpdateSteps: boolean=False);
const OPNAME = 'TProgressDialog.ShowProgress';
//var
  //LDefaultColor: TColor;
begin
  try
    Application.ProcessMessages;
    AStop := FStop;
    //LDefaultColor := ProgressRichEdit.SelAttributes.Color;
    //try
      case AProgressType of
        ptError:
          begin
            ProgressRichEdit.SelAttributes.Color := clRed;
            FErrorCount := FErrorCount + 1;
          end;
        ptWarning : ProgressRichEdit.SelAttributes.Color := clTeal;
      else
        ProgressRichEdit.SelAttributes.Color := clBlack;
      end;
      if(Trim(AProgress) <> '') then
        ProgressRichEdit.Lines.Add(AProgress);
    {finally
     ProgressRichEdit.SelAttributes.Color := LDefaultColor;
    end;
    ProgressRichEdit.SelStart := Length(ProgressRichEdit.Lines.text)-1;
    ProgressRichEdit.SelLength := 0;
    Self.ActiveControl := ProgressRichEdit;
    }
    if AUpdateSteps then
      ActionProgressBar.Position := ActionProgressBar.Position+1;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TProgressDialog.ShowProgressInPlace(AOldProgress, ANewProgress: String; AProgressType: TProgressType;   var AStop: boolean;  AUpdateSteps: boolean=False);
const OPNAME = 'TProgressDialog.ShowProgressInPlace';
procedure ReplaceLine(AOldProgress, ANewProgress: string) ;
const OPNAME = 'UProgressDialog.ReplaceLine';
var
  LX,
  LToEnd : integer;
begin
   LockWindowUpdate(Self.Handle);
   try
     LX := 0;
     LToEnd := length(ProgressRichEdit.Text) ;
     LX     := ProgressRichEdit.FindText(AOldProgress, LX, LToEnd, []) ;
     ProgressRichEdit.SelStart  := LX;
     ProgressRichEdit.SelLength := Length(AOldProgress) ;
     ProgressRichEdit.SelText   := ANewProgress;
   finally
     LockWindowUpdate(0);
   end;
end;

//var
  //LIndex : integer;
  //LDefaultColor: TColor;
begin
  try
    Application.ProcessMessages;
    AStop := FStop;
    //LDefaultColor := ProgressRichEdit.SelAttributes.Color;
    //try
      case AProgressType of
        ptError:
          begin
            ProgressRichEdit.SelAttributes.Color := clRed;
            FErrorCount := FErrorCount + 1;
          end;
        ptWarning : ProgressRichEdit.SelAttributes.Color := clTeal;
      else
        ProgressRichEdit.SelAttributes.Color := clBlack;
      end;

      if(Trim(ANewProgress) <> '') then
      begin
        if(AOldProgress = '') then
        begin
          ProgressRichEdit.Lines.Add('');
          ProgressRichEdit.Lines.Add(ANewProgress);
          ProgressRichEdit.Lines.Add('');
        end
        else
        begin
          ReplaceLine(AOldProgress, ANewProgress);
        end;
      end;
    {finally
     ProgressRichEdit.SelAttributes.Color := LDefaultColor;
    end;
    ProgressRichEdit.SelStart := Length(ProgressRichEdit.Lines.text)-1;
    ProgressRichEdit.SelLength := 0;
    Self.ActiveControl := ProgressRichEdit;
    }
    if AUpdateSteps then
      ActionProgressBar.Position := ActionProgressBar.Position+1;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TProgressDialog.FStopButtonClick(Sender: TObject);
const OPNAME = 'TProgressDialog.FStopButtonClick';
begin
  try
    FStop := True;
    FStopButton.Enabled := False;
    FStartButton.Enabled := True;
    FPrintButton.Enabled := True;
    FSaveButton.Enabled  := True;
    FOKButton.Enabled := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TProgressDialog.FormShow(Sender: TObject);
const OPNAME = 'TProgressDialog.FormShow';
begin
  try
     clbOptions.Visible := (clbOptions.Items.Count > 0);
     if clbOptions.Visible then
       clbOptions.Height  := 4 + (clbOptions.Items.Count * clbOptions.ItemHeight);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TProgressDialog.UpdateProgressBar;
const OPNAME = 'TProgressDialog.UpdateProgressBar';
begin
  try
    ActionProgressBar.Position := ActionProgressBar.Position + 1;
    Application.ProcessMessages;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TProgressDialog.FStartButtonClick(Sender: TObject);
const OPNAME = 'TProgressDialog.FStartButtonClick';
var
  LCount: integer;

begin
  try
    FStop                 := False;
    FSuccsess             := False;
    FKeepStartBtnDisabled := False;
    FStopButton.Enabled   := True;
    FStartButton.Enabled  := False;
    FPrintButton.Enabled  := False;
    FSaveButton.Enabled   := False;
    FOKButton.Enabled     := False;
    ProgressRichEdit.Clear;
    Application.ProcessMessages;
    ActionProgressBar.Position := ActionProgressBar.Min;
    FSuccsess := True;
    Self.ActiveControl := ProgressRichEdit;
    for LCount := 1 to FExecFunctionCount do
    begin
      Application.ProcessMessages;
      if FStop then
      begin
        FSuccsess := False;
        Break;
      end;

      if Assigned(FExecFunctions[LCount]) then
      begin
        if not FExecFunctions[LCount](ShowProgress) then
          FSuccsess := False;
        if FAppModules.GlobalData.StopOnFirstErr and (not FSuccsess) then
          Break;
      end;
    end;

    FStopButton.Enabled  := False;
    FStartButton.Enabled := not FKeepStartBtnDisabled;
    FPrintButton.Enabled := True;
    FSaveButton.Enabled  := True;
    FOKButton.Enabled    := True;
    if Silent then
      Self.Close;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TProgressDialog.FormCanResize(Sender: TObject; var NewWidth,NewHeight: Integer; var Resize: Boolean);
const OPNAME = 'TProgressDialog.FormCanResize';
var
  LSpacing:integer;
begin
  try
    // check height
    if(NewHeight < 400) then//(FButtonPanel.Height + ActionProgressBar.Height + (Self.Height - Self.ClientHeight))) then
    begin
      Resize := False;
    end
    else
    begin
      // check Width
      LSpacing := ButtonSpacing;
      LSpacing := LSpacing * 7;
      LSpacing := LSpacing + FOKButton.Width + FStartButton.Width +  FStopButton.Width + FPrintButton.Width +
                FSaveButton.Width + (Self.Width - Self.ClientWidth);
      if(LSpacing > NewWidth) then
        Resize := False;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TProgressDialog.ButtonSpacing: integer;
const OPNAME = 'TProgressDialog.ButtonSpacing';
var
  LSpace : double;
begin
  Result := 0;
  try
    LSpace := (1.5 / 100) * Self.Width;
    Result := Round(LSpace);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TProgressDialog.FormResize(Sender: TObject);
const OPNAME = 'TProgressDialog.FormResize';
var
  LSpacing:integer;
begin
  try
    LSpacing            := ButtonSpacing;
    FOKButton.Left      := LSpacing;
    FStartButton.Left   := FOKButton.Left + FOKButton.Width + LSpacing;
    FStopButton.Left    := FStartButton.Left + FStartButton.Width + LSpacing;
    FPrintButton.Left   := FStopButton.Left + FStopButton.Width + LSpacing;
    FSaveButton.Left    := FPrintButton.Left + FPrintButton.Width + LSpacing;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TProgressDialog.FPrintButtonClick(Sender: TObject);
const OPNAME = 'TProgressDialog.FPrintButtonClick';
begin
  try
    ProgressRichEdit.Print('Validation Results');
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TProgressDialog.FSaveButtonClick(Sender: TObject);
const OPNAME = 'TProgressDialog.FSaveButtonClick';
var
  LFileName :string;
  LOldPlainText: boolean;
begin
  try
    dlgSaveFile.InitialDir := FAppModules.StudyArea.DataFilesPrefix;
    if dlgSaveFile.Execute then
    begin
      LFileName := dlgSaveFile.FileName;
      LOldPlainText :=  ProgressRichEdit.PlainText;
      ProgressRichEdit.PlainText := True;
      ProgressRichEdit.Lines.SaveToFile(LFileName);
      ProgressRichEdit.PlainText := LOldPlainText;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TProgressDialog.FormCreate(Sender: TObject);
const OPNAME = 'TProgressDialog.FormCreate';
begin
  try
    FSilent     := False;
    FErrorCount := 0;
    ProgressRichEdit.WordWrap := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TProgressDialog.ClearExecFunction;
const OPNAME = 'TProgressDialog.ClearExecFunction';
var
  LCount: integer;
begin
  try
    FExecFunctionCount := 0;
    for LCount := 1 to 10 do
      FExecFunctions[LCount] := nil;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TProgressDialog.ClearExtraButtons;
const OPNAME = 'TProgressDialog.ClearExtraButtons';
var
  LIndex: integer;
  LButton : TButton;
begin
  try
    for LIndex := 0 to FAddedButtons.Count-1 do
    begin
      LButton := TButton(FAddedButtons.Items[LIndex]);
      LButton.Parent := nil;
      Self.RemoveComponent(LButton);
      LButton.Free;
    end;
    FAddedButtons.Clear;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TProgressDialog.FormClose(Sender: TObject; var Action: TCloseAction);
const OPNAME = 'TProgressDialog.FormClose';
begin
  try
    ClearExecFunction;
    ClearExtraButtons;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TProgressDialog.AddExecutionFunctions(AExecFunction: TExecFunction): boolean;
const OPNAME = 'TProgressDialog.AddExecutionFunctions';
begin
  Result := False;
  try
    if Assigned(AExecFunction) and (FExecFunctionCount < 10) then
    begin
      FExecFunctionCount := FExecFunctionCount + 1;
      FExecFunctions[FExecFunctionCount] := AExecFunction;
      FStartButton.Enabled := True;
    end;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TProgressDialog.Initialise: boolean;
const OPNAME = 'TProgressDialog.Initialise';
begin
  Result := False;
  try
    ProgressRichEdit.Lines.Clear;
    clbOptions.Items.Clear;
    ActionProgressBar.Min      := 0;
    ActionProgressBar.Position := 0;
    ActionProgressBar.Max      := 0;
    FErrorCount                := 0;
    FSilent                    := False;
    FStop                      := False;
    FSuccsess                  := True;
    FKeepStartBtnDisabled      := False;
    ClearExecFunction;
    ClearExtraButtons;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TProgressDialog.AddButton(ACaption: string): TButton;
const OPNAME = 'TProgressDialog.AddButton';
begin
  Result := TButton.Create(Self);
  try
    Result.Parent  := FButtonPanel;
    Result.Top     := FStartButton.Top;
    Result.Width   := 80;
    Result.Height  := FStartButton.Height;
    Result.Caption := ACaption;
    if(FAddedButtons.Count = 0) then
     Result.Left := FButtonPanel.Width - Result.Width - 5
    else
     Result.Left := TButton(FAddedButtons.Items[FAddedButtons.Count-1]).Left  - Result.Width - 5;

    Result.Anchors := [akTop, akRight, akBottom];
    FAddedButtons.Add(Result);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TProgressDialog.CreateMemberObjects;
const OPNAME = 'TProgressDialog.CreateMemberObjects';
begin
  inherited;
  try
    FAddedButtons := TObjectList.Create(False);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TProgressDialog.DestroyMemberObjects;
const OPNAME = 'TProgressDialog.DestroyMemberObjects';
begin
  inherited;
  try
    FreeAndNil(FAddedButtons);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
