unit UYRCMergeSumOutFilesDialog;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, VCL.Graphics, VCL.Controls, VCL.Forms,
  VCL.Dialogs, VCL.StdCtrls, VCL.CheckLst, VCL.ExtCtrls,

  UAbstractObject,
  UAbstractComponent,
  UDataComponent,
  UDataEditComponent;

type
  TYRCMergeSumOutFilesDialog = class(TForm)
    Panel1: TPanel;
    Panel2: TPanel;
    Button1: TButton;
    Button2: TButton;
    Splitter1: TSplitter;
    Panel3: TPanel;
    Label1: TLabel;
    Label2: TLabel;
    CheckListBox1: TCheckListBox;
    CheckListBox2: TCheckListBox;
    procedure CheckListBox1ClickCheck(Sender: TObject);
 //   procedure FormResize(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    function PopulateData(ATDToBeDeleteCommaText,ATDToBeAddedCommaText: String): boolean;
    function GetSeletedData(var ATDToBeDeleteCommaText,ATDToBeAddedCommaText: String):boolean;
  end;

var
  YRCMergeSumOutFilesDialog: TYRCMergeSumOutFilesDialog;

implementation

{$R *.dfm}

uses
  UErrorHandlingOperations;

{ TfrmPenaltyAction }

function TYRCMergeSumOutFilesDialog.PopulateData(ATDToBeDeleteCommaText,ATDToBeAddedCommaText: String): boolean;
const OPNAME = 'TYRCMergeSumOutFilesDialog.PopulateData';
begin
  Result := False;
  try
    CheckListBox1.Items.CommaText := ATDToBeDeleteCommaText;
    CheckListBox2.Items.CommaText := ATDToBeAddedCommaText;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYRCMergeSumOutFilesDialog.GetSeletedData(var ATDToBeDeleteCommaText,ATDToBeAddedCommaText: String): boolean;
const OPNAME = 'TYRCMergeSumOutFilesDialog.GetSeletedData';
var
  LIndex: integer;
begin
  Result := False;
  try
    ATDToBeDeleteCommaText := '';
    ATDToBeAddedCommaText  := '';
    for LIndex := 0 to CheckListBox1.Items.Count-1 do
    begin
      if CheckListBox1.Checked[LIndex] then
        ATDToBeDeleteCommaText := ATDToBeDeleteCommaText + ','+IntToStr(LIndex);
    end;
    for LIndex := 0 to CheckListBox2.Items.Count-1 do
    begin
      if CheckListBox2.Checked[LIndex] then
        ATDToBeAddedCommaText := ATDToBeAddedCommaText + ','+IntToStr(LIndex);
    end;
    if(Length(ATDToBeDeleteCommaText) > 0)  then
       System.Delete(ATDToBeDeleteCommaText,1,1);
    if(Length(ATDToBeAddedCommaText) > 0)  then
       System.Delete(ATDToBeAddedCommaText,1,1);
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYRCMergeSumOutFilesDialog.CheckListBox1ClickCheck(Sender: TObject);
const OPNAME = 'TYRCMergeSumOutFilesDialog.CheckListBox1ClickCheck';
begin
  Button1.Enabled := False;
  try
   if (CheckListBox1.SelCount = 0) and (CheckListBox1.SelCount = 0) then Exit;

    Button1.Enabled := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
