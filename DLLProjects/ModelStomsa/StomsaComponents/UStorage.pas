unit UStorage;

interface
Uses
  Classes,SysUtils;


Type
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  //Array types
  arrayD       = Array[1..1000] of Double; //Dynamic arrays in D4
  arrayCoefs   = Array[1..4] of Double;
  arrayT       = Array[1..1] of Double;
  FittedArray  = Array[0..3,0..1000] Of Double; //Dynamic arrays in D4
  arrayTI      = Array[1..1] of Integer;
  arrayT2_30   = Array[1..2,1..30] of Double;
  arrayT9_2_30 = Array[1..9,1..2,1..30] of Double;

  ProjectPointer = ^ProjectData;
  ProjectData = record
    //File information
    FileDir,Filename : string;
    FileDate : TDateTime; //Save date of the INC file - used to check if the data has changed
    DataRead : Boolean; //Tag showing whether INC file data has been read in.
    //Line fitting data
    Yin: arrayD;
    X,W :  arrayD;
    Ga, De, Xl, Xx, Cr : arrayCoefs;
    N, Istart, Nzf, Icurv : integer;
    IFinish : integer;
    itype, ifault : integer;
    //AJV data
    SNVR,GAMMA,DELTA,XLAM,XI:arrayT;
    //Calculation state flags
    FittedMarginal : Boolean;
    TimeSeriesFitted : Boolean;

    //Fitted and standardised curves for the guage
    Fitted,Standardised : FittedArray;
    //User marginal values
    userCurv: Integer; //mirror variable for ICurv
    userGa, userDe, userXl, userXx, userCr : ArrayCoefs;
    //Time series parameters
    NLAG : arrayTI;
    NCORR : arrayT2_30;
    PCORR : arrayT9_2_30;
    //Linked list
    Next : ProjectPointer;
  end;

  //Event procedure types
  TIncFileEvent = procedure(Sender: TObject; FileDir,FileName:String) of object;
  TGlobFileEvent = Procedure(Sender: TObject; CurrentNum,TotalNum:Integer) of object;
  TIncChangedEvent = Procedure(Sender: TObject; FileDir,FileName:String;Var FileContinue:Boolean) of object;

  TDataStorage = class(TComponent)
  private
    { Private declarations }
    //Variables
    Inc_Head,Inc_Move,Read_Head : ProjectPointer;
    Inc_FileCount : Integer;
    TmpData : ProjectData;
    FCurrentProjData : ProjectPointer;
    FContinueRead : Boolean;

    //Event variables
    FOnIncRead : TIncFileEvent;
    FOnGlobFileAction : TGlobFileEvent;
    FOnIncChanged : TIncChangedEvent;

    //Private procedures
    Procedure Inc_ReadFile(FileDir,Filename : string;Var ErrorCode:Integer);
  protected
    { Protected declarations }
  public
    { Public declarations }

    //Properties
    Property CurrentProjData : ProjectPointer Read FCurrentProjData Write FCurrentProjData;
    Property ContinueRead : Boolean Read FContinueRead Write FContinueRead;

    Constructor Create (AOwner:TComponent); override;
    Destructor Destroy; Override;

    //Global procedures
    Procedure Global_ClearAll;
    Procedure Global_ReadAll(FileName:String; Var ErrorCode:Integer);
    Procedure Global_WriteAll(FileName:String; Var ErrorCode:Integer);

    //INC File procedures
    Function Inc_NumOfFiles:Integer;
    Procedure Inc_AddName(FileDir,FileName:String);
    Procedure Inc_RemoveName(FileDir,FileName:String);
    Procedure Inc_ReadAll(Var ErrorCode:Integer);
    Procedure Inc_First(Var FileDir,FileName:String);
    Procedure Inc_Next(Var FileDir,FileName:String);

    //Data handling
    Procedure Data_Update;


  published
    { Published declarations - available at design time}
    //Inc_ReadFile completed event
    Property OnIncRead:TIncFileEvent Read FOnIncRead Write FOnIncRead;
    Property OnGlobFile:TGlobFileEvent Read FOnGlobFileAction Write FOnGlobFileAction;
    Property OnIncChanged:TIncChangedEvent Read FOnIncChanged Write FOnIncChanged;
End;

procedure Register;

implementation
Uses Vcl.Dialogs;


//--- Object procedures ------------------------------------------------
Constructor TDataStorage.Create (AOwner:TComponent);
//This will create the object
Begin
  Inherited Create(AOwner);
  New(Inc_Head);
  Inc_Head:=Nil;
End;

Destructor TDataStorage.Destroy;
//This will destroy the object
Begin
  Inherited Destroy;
End;

//--- End of Object procedures ------------------------------------------

//--- Global Procedures -------------------------------------------------

Procedure TDataStorage.Global_ClearAll;
//This will clear all the structures in memory
Var
  TmpHead : ProjectPointer;
Begin
  TmpHead:=Inc_Head;
  While TmpHead<>Nil Do Begin
    Inc_RemoveName(TmpHead.FileDir,TmpHead.Filename);
    TmpHead:=Inc_Head;
  End;
End;

Procedure TDataStorage.Global_ReadAll(FileName:String; Var ErrorCode:Integer);
//This will read the file defined by Filename and populate the
//various datastructures
//Errorcodes are:
//0 - All is hunky dory
//1 - Could not find the file
//2 - Wrong file type
Var
  Filer: TextFile;
  InStr,IncDir,IncName : String;
  InInt,I,J,K,Count,FileCount,TmpCount : Integer;
  TmpHead : ProjectPointer;
  IncDate : TDateTime;
  InDouble : Double;

Begin
  If FileExists(FileName)=False Then Begin
    ErrorCode:=1;
    Exit;
  End;
  AssignFile(Filer,FileName);
  Reset(Filer);

  //Write an identifier to the file first
  Readln(Filer,InStr);
  If InStr<>'STOMSA' Then Begin
    ErrorCode:=2;
    CloseFile(Filer);
    Exit;
  End;
  Readln(Filer,InStr); //Version number

  Readln(Filer,FileCount); //Number of Inc FileNames to read in
  Count:=1;
  While Count<=FileCount Do Begin
    //Read the names
    Readln(Filer,IncDir);
    Readln(Filer,IncName);
    Readln(Filer,IncDate);
    //create the entry
    Inc_AddName(IncDir,IncName);
    TmpHead:=Inc_Head;
    While (TmpHead.FileDir<>IncDir) Or (TmpHead.Filename<>IncName) Do
      TmpHead:=TmpHead.Next;
    Read_Head:=TmpHead; //For the Inc_Read procedure

    //Read the data from the Inc_File
    Inc_ReadFile(IncDir,IncName,InInt);
    If InInt<>0 Then
      FContinueRead:=False //Don't store any more stuff in memory
    Else
      FContinueRead:=True;

    //Read the rest of the stuff after moving to the correct entry
    If (TmpHead.FileDate-IncDate)>0.000001 Then Begin
      FContinueRead:=False; //Assume that we will not read the data from the global file
      //Raise the event and let the user decide if he wants to read this stuff in anyway
      If Assigned(FOnIncChanged) Then FOnIncChanged(Self,IncDir,IncName,FContinueRead);
      If FContinueRead=False Then TmpHead.FileDate:=IncDate; //keep the old date if the user is not going to read the data
    End;

    //Now read all the stuff so that we progress through the file
    //but only store it in the list if ContinueRead is true
    i:=1;
    Repeat
      Read(Filer,InDouble);
      If FContinueRead Then TmpHead.X[i]:=InDouble;
      ReadLn(Filer,InDouble);
      If FContinueRead Then TmpHead.W[i]:=InDouble;
//      Readln(Filer,TmpHead.Yin[i],TmpHead.X[i],TmpHead.W[i]);
      If (InDouble<-998) then Begin
       //Reached the end of the list - exit now
//       TmpHead.Yin[i]:=0;
       TmpHead.X[i]:=0;
       TmpHead.W[i]:=0;
       i:=1000;
      End;
      Inc(i);
    Until(i>1000);
    i:=1;
    Repeat
      Read(Filer,InDouble);
      If FContinueRead Then TmpHead.Ga[i]:=InDouble;
      Read(Filer,InDouble);
      If FContinueRead Then TmpHead.De[i]:=InDouble;
      Read(Filer,InDouble);
      If FContinueRead Then TmpHead.Xl[i]:=InDouble;
      Read(Filer,InDouble);
      If FContinueRead Then TmpHead.Xx[i]:=InDouble;
      ReadLn(Filer,InDouble);
      If FContinueRead Then TmpHead.Cr[i]:=InDouble;
      Inc(i);
    Until(i>4);

    i:=1;
    Repeat
      Read(Filer,InDouble);
      If FContinueRead Then TmpHead.userGa[i]:=InDouble;
      Read(Filer,InDouble);
      If FContinueRead Then TmpHead.userDe[i]:=InDouble;
      Read(Filer,InDouble);
      If FContinueRead Then TmpHead.userXl[i]:=InDouble;
      Read(Filer,InDouble);
      If FContinueRead Then TmpHead.userXx[i]:=InDouble;
      ReadLn(Filer,InDouble);
      If FContinueRead Then TmpHead.userCr[i]:=InDouble;
      Inc(i);
    Until(i>4);

    Read(Filer,InInt);
    If FContinueRead Then TmpHead.Nzf:=InInt;
    Read(Filer,InInt);
    If FContinueRead Then TmpHead.itype:=InInt;
    ReadLn(Filer,InInt);
    If FContinueRead Then TmpHead.ifault:=InInt;

//    Readln(Filer,TmpHead.N,TmpHead.Nzf,TmpHead.IType,TmpHead.IFault);
    Read(Filer,InInt);
    If FContinueRead Then TmpHead.Icurv:=InInt;
    ReadLn(Filer,InInt);
    If FContinueRead Then TmpHead.userCurv:=InInt;
//    Readln(Filer,TmpHead.Icurv,TmpHead.UCurv);
    i:=1;
    Repeat
      Read(Filer,InDouble);
      If FContinueRead Then TmpHead.SNVR[i]:=InDouble;
      Read(Filer,InDouble);
      If FContinueRead Then TmpHead.GAMMA[i]:=InDouble;
      Read(Filer,InDouble);
      If FContinueRead Then TmpHead.DELTA[i]:=InDouble;
      Read(Filer,InDouble);
      If FContinueRead Then TmpHead.XLAM[i]:=InDouble;
      ReadLn(Filer,InDouble);
      If FContinueRead Then TmpHead.XI[i]:=InDouble;
//      Readln(Filer,TmpHead.SNVR[i],TmpHead.GAMMA[i],TmpHead.DELTA[i],TmpHead.XLAM[i],TmpHead.XI[i]);
      Inc(i);
    Until(i>1);

    Readln(Filer,InStr);
    If FContinueRead Then Begin
      If InStr='TRUE' Then
        TmpHead.FittedMarginal:=True
      Else
        TmpHead.FittedMarginal:=False;
    End;
    ReadLn(Filer,TmpCount); //number of curve entries to read
    i:=1;
    While (i<=TmpCount) Do Begin
      //fitted curve
      Read(Filer,InDouble);
      If FContinueRead Then TmpHead.Fitted[0,i]:=InDouble;
      Read(Filer,InDouble);
      If FContinueRead Then TmpHead.Fitted[1,i]:=InDouble;
      Read(Filer,InDouble);
      If FContinueRead Then TmpHead.Fitted[2,i]:=InDouble;
      ReadLn(Filer,InDouble);
      If FContinueRead Then TmpHead.Fitted[3,i]:=InDouble;
      //standardised curve
      Read(Filer,InDouble);
      If FContinueRead Then TmpHead.Standardised[0,i]:=InDouble;
      Read(Filer,InDouble);
      If FContinueRead Then TmpHead.Standardised[1,i]:=InDouble;
      Read(Filer,InDouble);
      If FContinueRead Then TmpHead.Standardised[2,i]:=InDouble;
      ReadLn(Filer,InDouble);
      If FContinueRead Then TmpHead.Standardised[3,i]:=InDouble;
      Inc(i);
    End;//while

    Readln(Filer,InStr);
    If FContinueRead Then Begin
      If InStr='TRUE' Then
        TmpHead.TimeSeriesFitted:=True
      Else
        TmpHead.TimeSeriesFitted:=False;
    End;

    i:=1;
    Repeat
      Readln(Filer,InInt);
      If FContinueRead Then TmpHead.NLAG[i]:=InInt;
      Inc(i);
    Until(i>1);

    i:=1;
    Repeat
      j:=1;
      Repeat
        Readln(Filer,InDouble);
        If FContinueRead Then TmpHead.NCORR[i,j]:=InDouble;
        Inc(j);
      Until(j>30);
      Inc(i);
    Until(i>2);


    i:=1;
    Repeat
      j:=1;
      Repeat
        k:=1;
        Repeat
          Readln(Filer,InDouble);
          If FContinueRead Then TmpHead.PCORR[i,j,k]:=InDouble;
          Inc(k);
        Until(k>30);
        Inc(j);
      Until(j>2);
      Inc(i);
    Until(i>9);

    If FContinueRead And Assigned(FOnGlobFileAction) Then FOnGlobFileAction(Self,Count,FileCount);

    Inc(Count);
  End;

  CloseFile(Filer);
  ErrorCode:=0; //No errors encountered
End;

Procedure TDataStorage.Global_WriteAll(FileName:String; Var ErrorCode:Integer);
//This will write the data to a file defined by Filename
//Errorcodes are:
//0 - All is hunky dory
//1 - Could not open the file
Var
  Filer: TextFile;
  i,j,k,Count : Integer;
  TmpHead : ProjectPointer;
Begin
  AssignFile(Filer,FileName);
  {$I-} Rewrite(Filer); {$I+}
  If IOResult<>0 Then Begin
    ErrorCode:=1;
    Exit;
  End;

  //Write an identifier to the file first
  Writeln(Filer,'STOMSA');
  Writeln(Filer,'1.0'); //Version

  //Save the Filenames to the file
  //Write the number of files to be saved
  Writeln(Filer,Inc_FileCount);
  TmpHead:=Inc_Head;
  Count:=1;
  While TmpHead<>Nil Do Begin
    If Assigned(FOnGlobFileAction) Then FOnGlobFileAction(Self,Count,Inc_FileCount);
    //Write the names
    Writeln(Filer,TmpHead.FileDir);
    Writeln(Filer,TmpHead.FileName);
    Writeln(Filer,TmpHead.FileDate);
    //Write the rest of the stuff
    i:=1;
    Repeat
      If (TmpHead.Yin[i]=0) And (TmpHead.X[i]=0) And (TmpHead.W[i]=0) then Begin
       //Reached the end of the list - exit now
//       TmpHead.Yin[i]:=-999; --  read from INC file
       TmpHead.X[i]:=-999;
       TmpHead.W[i]:=-999;
       Writeln(Filer,{TmpHead.Yin[i]:0:6,' ',}TmpHead.X[i]:0:6,' ',TmpHead.W[i]:0:6);
       i:=1000;
      End
      Else
       Writeln(Filer,{TmpHead.Yin[i]:0:6,' ',}TmpHead.X[i]:0:6,' ',TmpHead.W[i]:0:6);
      Inc(i);
    Until(i>1000);
    i:=1;
    Repeat
      Writeln(Filer,TmpHead.Ga[i]:0:6,' ',TmpHead.De[i]:0:6,' ',TmpHead.Xl[i]:0:6,' ',TmpHead.Xx[i]:0:6,' ',TmpHead.Cr[i]:0:6);
      Inc(i);
    Until(i>4);
    i:=1;
    Repeat
      Writeln(Filer,TmpHead.userGa[i]:0:6,' ',TmpHead.userDe[i]:0:6,' ',TmpHead.userXl[i]:0:6,' ',TmpHead.userXx[i]:0:6,' ',TmpHead.userCr[i]:0:6);
      Inc(i);
    Until(i>4);

    Writeln(Filer,{TmpHead.N,' ',}TmpHead.Nzf,' ',TmpHead.IType,' ',TmpHead.IFault);
    Writeln(Filer,TmpHead.Icurv,' ',TmpHead.userCurv);
    i:=1;
    Repeat
      Writeln(Filer,TmpHead.SNVR[i]:0:6,' ',TmpHead.GAMMA[i]:0:6,' ',TmpHead.DELTA[i]:0:6,' ',TmpHead.XLAM[i]:0:6,' ',TmpHead.XI[i]:0:6);
      Inc(i);
    Until(i>1);

    Writeln(Filer,TmpHead.FittedMarginal);
    Writeln(Filer,TmpHead.N); //Number of entries to write
    i:=1;
    While (i<=TmpHead.N) Do Begin
      //fitted curve
      Writeln(Filer,TmpHead.Fitted[0,i]:0:6,' ',TmpHead.Fitted[1,i]:0:6,' ',
                    TmpHead.Fitted[2,i]:0:6,' ',TmpHead.Fitted[3,i]:0:6);
      //standardised curve
      Writeln(Filer,TmpHead.Standardised[0,i]:0:6,' ',TmpHead.Standardised[1,i]:0:6,' ',
                    TmpHead.Standardised[2,i]:0:6,' ',TmpHead.Standardised[3,i]:0:6);
      Inc(i);
    End;//while

    Writeln(Filer,TmpHead.TimeSeriesFitted);
    i:=1;
    Repeat
      Writeln(Filer,TmpHead.NLAG[i]);
      Inc(i);
    Until(i>1);

    i:=1;
    Repeat
      j:=1;
      Repeat
        Writeln(Filer,TmpHead.NCORR[i,j]:0:6);
        Inc(j);
      Until(j>30);
      Inc(i);
    Until(i>2);


    i:=1;
    Repeat
      j:=1;
      Repeat
        k:=1;
        Repeat
          Writeln(Filer,TmpHead.PCORR[i,j,k]:0:6);
          Inc(k);
        Until(k>30);
        Inc(j);
      Until(j>2);
      Inc(i);
    Until(i>9);

    //Move to the next entry
    TmpHead:=TmpHead.Next;
    Inc(Count); //counter for the status event
  End;//while

  CloseFile(Filer);
  ErrorCode:=0; //No errors encountered
End;

//--- End of Global Procedures -------------------------------------

//--- INC File procedures ------------------------------------------

Function TDataStorage.Inc_NumOfFiles:Integer;
//This will return the number of filenames currently in memory
Begin
   Inc_NumOfFiles:=Inc_FileCount;
End;

Procedure TDataStorage.Inc_AddName(FileDir,FileName:String);
//This will add a single filename to the list of names
Var
  TmpHead,PrevHead,NewHead : ProjectPointer;
Begin
  //Add a backslash to the end of the directory name if it does not
  //have one already
  FileDir:=FileDir+'\';
  If Pos('\\',FileDir)>0 Then FileDir:=Copy(FileDir,1,Length(FileDir)-1);
//  TmpStr:=Copy(FileDir,Length(FileDir)-1,1);
//  If TmpStr<>'\' Then FileDir:=FileDir+'\';
  TmpHead:=Inc_Head;
  PrevHead:=Inc_Head;
  While TmpHead<>Nil Do Begin
    If (TmpHead.FileDir=FileDir) and (TmpHead.Filename=FileName) Then Exit; //skip existing records
    PrevHead:=TmpHead;
    TmpHead:=TmpHead.Next;
  End;//while

  //create a new entry
  New(NewHead);
  //Define default entries
  NewHead.FileDir:=FileDir;
  NewHead.Filename:=FileName;
  NewHead.FittedMarginal:=False; //Marginals are not fitted yet.
  NewHead.DataRead:=False;//default - data is not read in
  NewHead.Next:=Nil;//new end of the list

  //Insert the entry into the list
  If Inc_Head=Nil Then
    Inc_Head:=NewHead
  Else
    PrevHead.Next:=Newhead;

  //Increment the file counter
  Inc(Inc_FileCount);
End;

Procedure TDataStorage.Inc_RemoveName(FileDir,FileName:String);
//This will remove a single filename from the list
Var
  TmpHead : ProjectPointer;
Begin
  TmpHead:=Inc_Head;
  While TmpHead<>Nil Do Begin
    If (TmpHead.FileDir=FileDir) And (TmpHead.Filename=FileName) Then Begin
      //If we are deleting the first entry then move the head pointer along
      If Inc_Head=TmpHead Then Inc_Head:=Inc_Head.Next;
      Dispose(TmpHead);
      Dec(Inc_FileCount);//Decrease the file counter
      //Move the Inc_Move to the start as a safety measure
      Inc_Move:=Inc_Head;
      Exit;
    End;
    TmpHead:=TmpHead.Next;
  End;//while
End;

Procedure TDataStorage.Inc_ReadFile(FileDir,Filename : string;Var ErrorCode:Integer);
//This will read an INC file and store it in the entry defined by
//the Read_Head pointer
//Modified July 1999
var
  fyle : Textfile;
  s, d1 : string;
  {i,} Iy : integer;
//  AnualTot : double;
begin
   ErrorCode:=1; //default error assumption
   If FileExists(FileDir+FileName)=False Then Exit;
   AssignFile(fyle,FileDir+Filename);
   {$i-} Reset(fyle); {$i+}
   if IOResult<>0 Then Exit;
   FileAge(FileDir+FileName,Read_Head.FileDate);
   //Read_Head.FileDate:=FileDateToDateTime(Iy);
   Readln(fyle,s);
{   AppendNode;
   ProjectCurr.Filename := Filename;
   ProjectCurr.Istart := strtoint(copy(s,5,4));}
   Read_Head.Istart:=strtoint(copy(s,5,4));
   Iy := 1;
   While not eof(fyle) do
   begin
{    AnualTot := 0.0;
    for i:=1 to 12 do
    begin
      AnualTot:= AnualTot + strtofloat(copy(s,10+(i-1)*8,7));
    end;
    ProjectCurr.Yin[Iy] := AnualTot;}
    Read_Head.Yin[Iy]:=strtofloat(copy(s,106,9));
    Iy := Iy + 1;
    readln(fyle,s);
//    ProjectCurr.IFinish := strtoint(copy(s,5,4));
    Read_Head.IFinish:=strtoint(copy(s,5,4));
   end; // while
   // Process the last record
   d1 := copy(s,5,4);
   if d1 <> '    ' then
   begin
{    AnualTot := 0.0;
    for i:=1 to 12 do
    begin
      AnualTot:= AnualTot + strtofloat(copy(s,10+(i-1)*8,7));
    end;
    ProjectCurr.Yin[Iy] := AnualTot}
    Read_Head.Yin[Iy]:=strtofloat(copy(s,106,9));
   end
   else
   begin
   Iy := Iy - 1;
   end;
//   ProjectCurr.N := Iy;
   Read_Head.n:=Iy;
   CloseFile(fyle);
   Read_Head.DataRead:=True; //Mark the data as read
   ErrorCode:=0; //No errors encountered

   If Assigned(FOnIncRead) Then FOnIncRead(Self,Read_Head.FileDir,Read_Head.Filename); //raise the read done event

end;//Inc_ReadFile

Procedure TDataStorage.Inc_ReadAll(Var ErrorCode:Integer);
//This will read all the INC files into memory that have not already
//been read in.
Var
  TmpCode:Integer;
Begin
  ErrorCode:=0;
  Read_Head:=Inc_Head;
  While (Read_Head<>Nil) Do Begin
    if (Read_Head.DataRead=False) Then Begin
      Inc_ReadFile(Read_Head.FileDir,Read_Head.FileName,TmpCode);
      ErrorCode:=ErrorCode+TmpCode; //Add the number of files incorrectly read
    End;//if
    Read_Head:=Read_Head.Next;
  End;//whlie
End;

Procedure TDataStorage.Inc_First(Var FileDir,FileName:String);
//This will move to the first entry in the filelist
Begin
  //Default values for error returns
  FileDir:='';
  FileName:='';
  CurrentProjData:=Nil;
  Inc_Move:=Inc_Head;
  If Inc_Move<>Nil Then Begin
    FileDir:=Inc_Move.FileDir;
    FileName:=Inc_Move.Filename;
    TmpData:=Inc_Move^; //Separate this data into another variable to insulate changes from the list
    CurrentProjData:=@TmpData; //Make this data available to the user
  End;
End;

Procedure TDataStorage.Inc_Next(Var FileDir,FileName:String);
//This will move to the first entry in the filelist
Begin
  //Default values for error returns
  FileDir:='';
  FileName:='';
  CurrentProjData:=Nil;

  If Inc_Move.Next<>Nil Then Begin
    Inc_Move:=Inc_Move.Next;
    FileDir:=Inc_Move.FileDir;
    FileName:=Inc_Move.Filename;
    TmpData:=Inc_Move^; //Separate this data into another variable to insulate changes from the list
    CurrentProjData:=@TmpData; //Make this data available to the user
  End;
End;

//--- End of INC File procedures -----------------------------------

//--- Data update --------------------------------------------------
Procedure TDataStorage.Data_Update;
//This will take the value of the CurrentProjData and store its
//values to the entry defined by Inc_Move
Begin
  Inc_Move^:=TmpData;
End;

//--- End of Data update -------------------------------------------

procedure Register;
begin
  RegisterComponents('Stomsa', [TDataStorage]);
end;

end.
