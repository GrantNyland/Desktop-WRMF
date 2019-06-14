//
//
//  UNIT      : Contains a copy of the Fletcher-Powell searcher downloaded from the internet.
//  AUTHOR    : Grant Nyland (PDNA)
//  DATE      : 2002/10/29
//  COPYRIGHT : Copyright © 2002 DWAF
//
//
unit UFletcherPowellSearch;

interface

uses
  UFletcherPowellSearch_YRC;

TYPE
  FMFPType = (FMFPError, Converged, NotConverged, NoMinimum);
  DataType = array of real;
  ParmType = array of real;
VAR
  NumVar: integer;      // Actual number of model fitting parameters in use.
  ExptX, ExptY, CalcY: DataType;
  FirstPt, LastPt, IncrPt, NumDataPts: integer;
  X, XC, XR: ParmType;  // Model fitting parameters (used, constraned, restored).
  G, GC, GR: ParmType;  // Model fitting gradients  (used, constraned, restored).
  Grad: ParmType;       // Model fitting gradients.
  Index: array of integer;

procedure RunTheProgram(ACoefficientCount: integer);

implementation

  PROCEDURE FitToModel(NumToFit : INTEGER;
                       VAR XR,GR: ParmType;
                       VAR PHI  : REAL);
  {Takes info from FMFP and re-evaluates the selected model function}
    VAR
      I, J  : INTEGER;
      Prefix: REAL;
    const OPNAME = 'FitToModel';
    BEGIN
      FOR I:=1 TO NumVar DO
      BEGIN
        XC[I]:=XR[Index[I]];
        X[I]:=XC[I]; // Grant
        G[I]:=0;
      END;
      PHI:=0;
      I:=FirstPt;
      REPEAT
        Evaluate(ExptX[I],CalcY[I],Grad);
        Prefix:=-2*(ExptY[I]-CalcY[I])*Weight(I);
        FOR J:=1 TO NumVar DO G[J]:=G[J]+Prefix*Grad[J];
        PHI:=PHI+Weight(I)*SQR(ExptY[I]-CalcY[I]);
        I:=I+IncrPt;
      UNTIL I>LastPt;
      FOR I:=1 TO NumVar DO IF Index[I]>NumToFit THEN G[I]:=0;
      FOR I:=1 TO NumVar DO
      BEGIN
        GC[I]:=G[I]; // Grant
        GR[Index[I]]:=GC[I];
      END;
    END;

  PROCEDURE FMFP (N: INTEGER; VAR X,G: ParmType; VAR F: REAL; EST,EPS: REAL;
                  Limit: integer; VAR Kount: integer; VAR IER: FMFPType);

   {This Turbo Pascal version was translated directly from the FORTRAN
    program FMFP (Fletcher and Powell Unconstrained Search Technique),
    which is described on pp. 221-225 of System/360 Scientific Subroutine
    Package, H20-0205-3 (International Business Machines Corp.).}

    {I'll buy a beer for anyone who can code this subroutine efficiently
     in Turbo Pascal without using GOTO LABEL's - EMP, 5/23/88}

    LABEL 10,20,30,40,50,60,70,80,90,100;
    VAR
      I,J,K,KL,L,N2,N3,N31,NJ: INTEGER;
      H: ARRAY[1..50] OF REAL;
      ALFA,AMBDA,DALFA,DX,DY,FX,FY,GNRM,HNRM,OLDF,T,WF,ZF: REAL;
    const OPNAME = 'FMFP';
    BEGIN
      KL := 0; // Grant
      FitToModel (N,X,G,F);
      Kount:=1;
      IER:=Converged;
      N2:=N+N;
      N3:=N2+N;
      N31:=N3+1;
  10:
      K:=N31;
      FOR J:=1 TO N DO
      BEGIN
        H[K]:=1;
        NJ:=N-J;
        IF NJ<=0 THEN GOTO 20;
        FOR L:=1 TO NJ DO
        BEGIN
          KL:=K+L;
          H[KL]:=0;
        END;
        K:=KL+1;
      END;
  20:
      Kount:=Kount+1;
      OLDF:=F;
      FOR J:=1 TO N DO
      BEGIN
        K:=N+J;
        H[K]:=GR[J];
        K:=K+N;
        H[K]:=XR[J];
        K:=J+N3;
        T:=0;
        FOR L:=1 TO N DO
        BEGIN
          T:=T-GR[L]*H[K];
          IF (L-J)>=0 THEN K:=K+1 ELSE K:=K+N-L;
        END;
        H[J]:=T;
      END;
      DY:=0;
      HNRM:=0;
      GNRM:=0;
      FOR J:=1 TO N DO
      BEGIN
        HNRM:=HNRM+ABS(H[J]);
        GNRM:=GNRM+ABS(GR[J]);
        DY:=DY+H[J]*GR[J];
      END;
      IF (DY>=0) OR ((HNRM/GNRM-EPS)<=0) THEN GOTO 100;
      FY:=F;
      ALFA:=2*(EST-F)/DY;
      AMBDA:=1;
      IF (ALFA>0) AND ((ALFA-AMBDA)<0) THEN AMBDA:=ALFA;
      ALFA:=0;
  30:
      FX:=FY;
      DX:=DY;
      FOR I:=1 TO N DO XR[I]:=XR[I]+AMBDA*H[I];
      FitToModel (N,X,G,F);
      FY:=F;
      DY:=0;
      FOR I:=1 TO N DO DY:=DY+GR[I]*H[I];
      IF DY=0 THEN GOTO 80 ELSE IF (DY>0) OR ((FY-FX)>=0) THEN GOTO 40;
      AMBDA:=AMBDA+ALFA;
      ALFA:=AMBDA;
      IF (HNRM*AMBDA-1.0000000000E10)<=0 THEN GOTO 30;
      IER:=NoMinimum;
      EXIT;
  40:
      T:=0;
  50:
      IF AMBDA=0 THEN GOTO 80;
      ZF:=3*(FX-FY)/AMBDA+DX+DY;
      ALFA:=ABS(ZF);
      IF ABS(DX)>ALFA THEN ALFA:=ABS(DX);
      IF ABS(DY)>ALFA THEN ALFA:=ABS(DY);
      DALFA:=ZF/ALFA;
      DALFA:=SQR(DALFA)-DX/ALFA*DY/ALFA;
      IF DALFA<0 THEN GOTO 100;
      WF:=ALFA*SQRT(DALFA);
      ALFA:=DY-DX+WF+WF;
      IF ALFA=0 THEN ALFA:=(DY+ZF-WF)/(ZF+DX+ZF+DY) ELSE ALFA:=(DY-ZF+WF)/ALFA;
      ALFA:=ALFA*AMBDA;
      FOR I:=1 TO N DO XR[I]:=XR[I]+(T-ALFA)*H[I];
      FitToModel (N,X,G,F);
      IF ((F-FX)<=0) AND ((F-FY)<=0) THEN GOTO 80;
      DALFA:=0;
      FOR I:=1 TO N DO DALFA:=DALFA+GR[I]*H[I];
      IF DALFA>=0 THEN GOTO 70 ELSE IF (F-FX)<0 THEN GOTO 60;
      IF (F-FX)>0 THEN GOTO 70 ELSE IF (DX-DALFA)=0 THEN GOTO 80;
  60:
      FX:=F;
      DX:=DALFA;
      T:=ALFA;
      AMBDA:=ALFA;
      GOTO 50;
  70:
      IF ((FY-F)=0) AND ((DY-DALFA)=0) THEN GOTO 80;
      FY:=F;
      DY:=DALFA;
      AMBDA:=AMBDA-ALFA;
      GOTO 40;
  80:
      IF (OLDF-F+EPS)<0 THEN GOTO 100;
      FOR J:=1 TO N DO
      BEGIN
        K:=N+J;
        H[K]:=GR[J]-H[K];
        K:=N+K;
        H[K]:=XR[J]-H[K];
      END;
      IER:=Converged;
      T:=0;
      ZF:=0;
      FOR J:=1 TO N DO
      BEGIN
        K:=N+J;
        WF:=H[K];
        K:=K+N;
        T:=T+ABS(H[K]);
        ZF:=ZF+WF*H[K];
      END;
      IF ((HNRM-EPS)<=0) AND ((T-EPS)<=0) THEN EXIT;
      IF (Kount-Limit)>=0 THEN GOTO 90;
      ALFA:=0;
      FOR J:=1 TO N DO
      BEGIN
        K:=J+N3;
        WF:=0;
        FOR L:=1 TO N DO
        BEGIN
          KL:=N+L;
          WF:=WF+H[KL]*H[K];
          IF (L-J)>=0 THEN K:=K+1 ELSE K:=K+N-L;
        END;
        K:=N+J;
        ALFA:=ALFA+WF*H[K];
        H[J]:=WF;
      END;
      IF ZF*ALFA=0 THEN GOTO 10;
      K:=N31;
      FOR L:=1 TO N DO
      BEGIN
        KL:=N2+L;
        FOR J:=L TO N DO
        BEGIN
          NJ:=N2+J;
          H[K]:=H[K]+H[KL]*H[NJ]/ZF-H[L]*H[J]/ALFA;
          K:=K+1;
        END;
      END;
      GOTO 20;
  90:
      IER:=NotConverged;
      EXIT;
  100:
      FOR J:=1 TO N DO
      BEGIN
        K:=N2+J;
        XR[J]:=H[K];
      END;
      FitToModel (N,X,G,F);
      IF (GNRM-EPS)<=0 THEN IER:=Converged ELSE
      IF IER>=Converged THEN
      BEGIN
        IER:=FMFPError;
        GOTO 10;
      END;
    END;

procedure FitCurve(N: INTEGER; VAR X,G: ParmType);
const
  MinRSS  = 0.0;       // Estimate of the RSS function minimum
  Epsilon = 0.00001;   // Convergence Limit for Fletcher-Powell
  MaxIterations = 100; // Maximum iterations between updates
  OPNAME = 'FitCurve';
var
  Phi: REAL;
  Limit, Kount: integer;
  IER: FMFPType;
  I: integer;
begin

  // Initialise.
  Phi:=0;                         {Set Residual Sum of Squares to zero}
  Limit:=MaxIterations;           {Number of iterations to use in FMFP}
  FirstPt:=1;                     {First data pair to be fit}
  LastPt:=NumDataPts;             {Last data pair to be fit}
  IncrPt:=1;                      {Data step size - fit every pair}
  for I:=0 to High(G) do          {Set gradients to zero}
    G[I]:=0;
  for I:=0 to High(CalcY) do      {Set CalcY Array to zero}
    CalcY[I]:=0;

  // Call the function.
  FMFP(N,X,G,Phi,MinRSS,Epsilon,Limit,Kount,IER);
end;


procedure RunTheProgram(ACoefficientCount: integer);
const OPNAME = 'RunTheProgram';
var
  I: INTEGER;
begin
  for I:=1 to NumVar do
    Index[I]:=I;
  FOR I:=1 to NumVar do
  begin
    XC[I]:=X[I]; // Grant
    XR[Index[I]]:=XC[I];
  end;
  FitCurve(ACoefficientCount,XR,GR);
  for I:=1 to NumVar do
  begin
    XC[I]:=XR[Index[I]];
    GC[I]:=GR[Index[I]];
    X[I]:=XC[I]; // Grant
    G[I]:=GC[I]; // Grant
  end;
end;

end.
