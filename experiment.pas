unit experiment;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, Forms, Graphics, ExtCtrls;

procedure DebugView;
procedure DoMouseConditionA;
procedure DoMouseConditionB;

procedure CentralizeCursorPos;
procedure Move(AImage : TImage; ACenter: TPoint);
function InCircle(ACenter, APoint : TPoint): Boolean;

procedure ResizeStimuli(ACoin, AClow : TImage);
procedure StartExperiment(ABackGround : TCustomControl;
  AParticipantName : string);
procedure NextCondition;
function ConditionAsString : string;


type TExperimentalCondition = (ConditionTest, ConditionA,  ConditionB);
var CoinPoint : TPoint;
var ClowPoint : TPoint;
var ExperimentalCondition : TExperimentalCondition;

implementation

uses Dialogs, TabDelimitedReport.Custom;

var Conditions : array of TExperimentalCondition;
var CurrentCondition : integer = -1;
var Background : TCustomControl = nil;

function ConditionACoinPoint : TPoint;
begin
  Result := Point((Background.Width*1) div 3, Background.Height div 2);
end;

function ConditionAClowPoint : TPoint;
begin
  Result := ConditionACoinPoint;
  Result.x := Result.x div 2;
end;

function ConditionBCoinPoint : TPoint;
begin
  Result := Point((Background.Width*2) div 3, Background.Height div 2);
end;

function ConditionBClowPoint : TPoint;
begin
  Result := ConditionBCoinPoint;
  Result.x := Result.x + ConditionAClowPoint.x;
end;

function CentralPoint : TPoint;
begin
  Result := Point(Background.Width div 2, Background.Height div 2);
end;

function InsideCircle(ACenterX, ACenterY, ARadius, AX, AY : integer): Boolean; inline;
var Delta : integer;
begin
  Delta := ((AX - ACenterX) * (AX - ACenterX)) +
           ((AY - ACenterY) * (AY - ACenterY));
  if (Delta <= (ARadius * ARadius)) then
      Result := True
  else
      Result := False;
end;

const PixelsInCentimeter = 28 { 72 dpi monitor };

function InCircle(ACenter, APoint : TPoint): Boolean;
begin
  Result := InsideCircle(
   ACenter.X, ACenter.Y, PixelsInCentimeter, APoint.X, APoint.Y);
end;

procedure ResizeStimuli(ACoin, AClow : TImage);
begin
  ACoin.Width := PixelsInCentimeter*2;
  ACoin.Height:= ACoin.Width;

  AClow.Width := PixelsInCentimeter*2*5;
  AClow.Height:= AClow.Width;
end;

procedure DebugView;
begin
  if Background <> nil then
  begin
    Background.Canvas.Brush.Color := clRed;
    Background.Canvas.EllipseC(
      CoinPoint.x, CoinPoint.y, PixelsInCentimeter, PixelsInCentimeter);
  end;
end;

procedure DoMouseConditionA;
begin
  Mouse.CursorPos := Background.ClientToScreen(ConditionACoinPoint);
end;

procedure DoMouseConditionB;
begin
  Mouse.CursorPos := Background.ClientToScreen(ConditionBCoinPoint);
end;

procedure CentralizeCursorPos;
begin
  Mouse.CursorPos := Background.ClientToScreen(CentralPoint);
end;

procedure Move(AImage: TImage; ACenter: TPoint);
begin
  AImage.Left := ACenter.X - (AImage.Width div 2);
  AImage.Top := ACenter.Y - (AImage.Height div 2);
end;

procedure StartCondition(ACondition : TExperimentalCondition);
  procedure StartConditionA;
  begin
    ExperimentalCondition := ConditionA;
    CentralizeCursorPos;
    CoinPoint := ConditionACoinPoint;
    ClowPoint := ConditionAClowPoint;
    Background.Color := clGreen;
  end;
  procedure StartConditionB;
  begin
    ExperimentalCondition := ConditionB;
  	CentralizeCursorPos;
  	CoinPoint := ConditionBCoinPoint;
  	ClowPoint := ConditionBClowPoint;
    Background.Color := clGreen;
  end;
  procedure StartTest;
  begin
    ExperimentalCondition := ConditionTest;
    Background.Color := clBlack
  end;
begin
  case ACondition of
    ConditionA : StartConditionA;
    ConditionB : StartConditionB;
    ConditionTest : StartTest;
  end;
end;

procedure StartExperiment(ABackGround: TCustomControl; AParticipantName : string);
begin
  Background := ABackGround;
  Report.StartTime := GetTickCount64;
  Report.WriteRow(['Participante:', AParticipantName]);
  Report.WriteHeader;
  NextCondition;
end;

procedure NextCondition;
begin
  Inc(CurrentCondition);
  if CurrentCondition < Length(Conditions) then
    StartCondition(Conditions[CurrentCondition])
  else
    begin
      ShowMessage('O experimento chegou ao final. Obrigado por sua participação!');
      Report.WriteFooter;
      Report.Free;
    end;
end;

function ConditionAsString: string;
begin
  WriteStr(Result, ExperimentalCondition);
end;

procedure InitializeConditions;
var
  i: Integer;
begin
  SetLength(Conditions, 6);
  i := Random(1);
  if i = 0 then
  begin
    Conditions[0] := ConditionA;
    Conditions[1] := ConditionB;
    Conditions[2] := ConditionA;
    Conditions[3] := ConditionB;
    Conditions[4] := ConditionA;
  end
  else
  begin
    Conditions[0] := ConditionB;
    Conditions[1] := ConditionA;
    Conditions[2] := ConditionB;
    Conditions[3] := ConditionA;
    Conditions[4] := ConditionB;
  end;
  Conditions[5] := ConditionTest;
end;

initialization
  Randomize;
  InitializeConditions;


end.

