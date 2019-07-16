//
//
//  UNIT      : Contains TPDFDocumentLauncher Class
//  AUTHOR    : Grant Nyland
//  DATE      : 2002/04/18
//  COPYRIGHT : Copyright © 2002 DWAF
//
//
unit UPDFDocumentLauncher;

interface

uses
  Vcl.Forms , ShellApi;

type
  TPDFDocumentLauncher = class(TObject)
  protected
    //FMainForm: TForm;
    //FPDF: TAcroPDF;
    FFileName: string;
    //procedure GotoPageNumber(APageNumber: integer);
  public
    constructor Create;
    destructor Destroy; override;
    procedure LaunchAtPageNumber(APageNumber: integer = -1);
    class procedure Launch(AFileName: string; APageNumber: integer = -1);
    class procedure DestroyPDFDocumentLauncher;
    property FileName: string read FFileName write FFileName;
  end;

implementation

uses
  Winapi.Windows,
  SysUtils,
  Vcl.Controls,
  UWaitScreen,
  UErrorHandlingOperations;

var GLauncher: TPDFDocumentLauncher;

class procedure TPDFDocumentLauncher.Launch(AFileName: string; APageNumber: integer = -1);
const OPNAME = 'TPDFDocumentLauncher.Launch';
begin
  try

    // Display a wait message.
    LaunchWaitScreen('Loading....', AFileName);

    // Close the previous version.
    if Assigned(GLauncher) then
      FreeAndNil(GLauncher);

    // Open a new version.
    GLauncher := TPDFDocumentLauncher.Create;
    GLauncher.FFileName := AFileName;
    GLauncher.LaunchAtPageNumber(APageNumber);

    // Make sure that the wait screen closed.
    CloseWaitScreen;

  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME) end;
end;

class procedure TPDFDocumentLauncher.DestroyPDFDocumentLauncher;
const OPNAME = 'TPDFDocumentLauncher.DestroyPDFDocumentLauncher';
begin
  try
    FreeAndNil(GLauncher);

  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME) end;
end;

constructor TPDFDocumentLauncher.Create;
const OPNAME = 'TPDFDocumentLauncher.Create';
begin
  try
    inherited Create;
    //FMainForm := nil;
    //FPDF      := nil;
    FFileName := '';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

destructor TPDFDocumentLauncher.Destroy;
const OPNAME = 'TPDFDocumentLauncher.Destroy';
begin
  try
   // FreeAndNil(FMainForm);
    //FreeAndNil(FPDF);
    inherited Destroy;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TPDFDocumentLauncher.LaunchAtPageNumber(APageNumber: integer);
const OPNAME = 'TPDFDocumentLauncher.LaunchAtBookMark';
begin
  try
    if (not FileExists(FFileName)) then
    begin
      raise Exception.Create('Could not locate the file : ' + FFileName);
    end else begin

      // Make sure the form is destroyed.
  //    FreeAndNil(FMainForm);

      // Create the form.
{      FMainForm := TForm.CreateNew(nil);
      try
        FMainForm.BorderStyle := bsDialog;
        FMainForm.Width  := Screen.WorkAreaWidth;
        FMainForm.Height := Screen.WorkAreaHeight;

        // Create the PDF viewer OCX.
        FPDF := TAcroPDF.Create(nil);
        try
          try
            FPDF.Parent := FMainForm;
            FPDF.Align := alCLient;
            FPDF.src := FFileName;

            // Go to the required page.
            GotoPageNumber(APageNumber);

            // Close the wait message.
            CloseWaitScreen;

            // Show the form modally
            FMainForm.ShowModal;

          // Done.
          finally
            FreeAndNil(FPDF);
          end;
        finally
          FreeAndNil(FMainForm);
 }
      try
        ShellExecute(Application.Handle, 'open', 'AcroRd32.exe' , PChar(FFileName),
                 PChar(ExtractFilePath(FFileName)), SW_SHOW);

      // Trap the exception and raise a more usefull one.
      except on E: Exception do
        raise Exception.CreateFmt('Could not launch the PDF viewer. System error : [%s]', [E.Message]);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;
{
procedure TPDFDocumentLauncher.GotoPageNumber(APageNumber: integer);
const OPNAME = 'TPDFDocumentLauncher.GotoPageNumber';
begin
  try
    try
      if (APageNumber > 0) then
        FPDF.setCurrentPage(APageNumber);
    except
      raise Exception.CreateFmt(
        'Could not locate the page number [%d] in the PDF document : [%s]', [APageNumber, FFileName]);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;
 }
initialization
  GLauncher := nil;

finalization
  FreeAndNil(GLauncher);

end.
