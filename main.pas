unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, Buttons, ExtCtrls, Menus, lNetComponents, lNet;

type

  TChargeStatus = (st_Init,st_ready,st_connected,st_charging,st_error,st_standby,st_Invalid);

  { TFormMain }

  TFormMain = class(TForm)
    ButtonDiconnect: TButton;
    ButtonConnect: TButton;
    Connected: TImage;
    Error: TImage;
    Label1: TLabel;
    Standby: TImage;
    Ready: TImage;
    Charging: TImage;
    Status: TImage;
    LTCP: TLTCPComponent;
    EditPort: TEdit;
    EditIP: TEdit;
    LabelPort: TLabel;
    LabelHostName: TLabel;
    ButtonSend: TButton;
    EditSend: TEdit;
    MemoText: TMemo;
    TimerUpdate: TTimer;
    TimerQuit: TTimer;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormPaint(Sender: TObject);
    procedure ReadyClick(Sender: TObject);
    procedure LTCPComponentConnect(aSocket: TLSocket);
    procedure ConnectButtonClick(Sender: TObject);
    procedure DiconnectButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure LTCPComponentError(const msg: string; aSocket: TLSocket);
    procedure LTCPComponentAccept(aSocket: TLSocket);
    procedure LTCPComponentReceive(aSocket: TLSocket);
    procedure LTcpComponentDisconnect(aSocket: TLSocket);
    procedure MenuItemAboutClick(Sender: TObject);
    procedure MenuItemExitClick(Sender: TObject);
    procedure SendButtonClick(Sender: TObject);
    procedure SendEditKeyPress(Sender: TObject; var Key: char);
    procedure TimerQuitTimer(Sender: TObject);
    procedure TimerUpdateTimer(Sender: TObject);
  private
    FNet: TLConnection;
    FIsServer: Boolean;
    FChargeStatus : TChargeStatus;
    procedure SendToAll(const aMsg: string);
    procedure SetChargeStatus(AValue: TChargeStatus);
  public
    property ChargeStatus:TChargeStatus read FChargeStatus write SetChargeStatus;
    { public declarations }
  end; 

var
  FormMain: TFormMain;

implementation

uses
  lCommon;

{ TFormMain }

procedure TFormMain.ConnectButtonClick(Sender: TObject);
begin
  if FNet.Connect(EditIP.Text, StrToInt(EditPort.Text)) then
    FIsServer := False;
end;


procedure TFormMain.LTCPComponentConnect(aSocket: TLSocket);
begin
  MemoText.Append('Connected to remote host');

end;

procedure TFormMain.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CloseAction := caFree;

  if FNet.Connected then begin
    CloseAction := caNone; // make sure we quit gracefuly
    FNet.Disconnect; // call disconnect (soft)
    TimerQuit.Enabled := True; // if time runs out, quit ungracefully
  end;
end;


procedure TFormMain.FormPaint(Sender: TObject);
begin
  //
  Case ChargeStatus of
    st_ready: Canvas.Draw(0,0,Ready.Picture.Bitmap);
    st_connected: Canvas.Draw(0,0,Connected.Picture.Bitmap);
    st_charging: Canvas.Draw(0,0,Charging.Picture.Bitmap);
  end;
end;

procedure TFormMain.ReadyClick(Sender: TObject);
begin

end;

procedure TFormMain.LTCPComponentError(const msg: string; aSocket: TLSocket);
begin
  MemoText.Append(msg);
  MemoText.SelStart := Length(MemoText.Lines.Text);
end;

procedure TFormMain.LTCPComponentAccept(aSocket: TLSocket);
begin
  MemoText.Append('Connection accepted');
  MemoText.SelStart := Length(MemoText.Lines.Text);
end;


function CRC(Data: AnsiString): AnsiString; // 8-bit XOR Checksum
var
 i,iSum : Integer;
begin
 iSum := 0;
 for i := 1 to Length(Data) do
 begin
  iSum := iSum xor Ord(Data[i]);
 end;
 Result := IntToHex(iSum,2)
end;



procedure TFormMain.LTCPComponentReceive(aSocket: TLSocket);
var
  s,l,r,rec_cs,calc_cs: string;
  len: Int64;

begin
  if aSocket.GetMessage(s) > 0 then begin
    MemoText.Append(s);
//    MemoText.Append(IntToStr(Length(s)));
    len := Length(s) - 4;    // Commando length without checksum
    l := LeftStr(s,len);     // Commando String $cc pp pp ...
    r := RightStr(s,3);      // Checksum String xk\r
    rec_cs := LeftStr(r,2);  // received Checksum String
    calc_cs := CRC(l);       // calculated Checksum String
//    MemoText.Append(l);
//    MemoText.Append(rec_cs);
//    MemoText.Append(calc_cs);
    if rec_cs = calc_cs then  // Antwort verarbeiten
      begin

        case l of
           '$ST 01': begin
                 MemoText.Append('Status: Bereit');
                 ChargeStatus := st_ready;
                 //Canvas.Draw(0,0,Ready.Picture.Bitmap);
           end;
           '$ST 02': begin
                MemoText.Append('Status: Verbunden');
                ChargeStatus := st_connected;
                //Canvas.Draw(0,0,Connected.Picture.Bitmap);
           end;
           '$ST 03': begin
                 MemoText.Append('Status: Laden');
                 ChargeStatus := st_charging;
                 //Canvas.Draw(0,0,Charging.Picture.Bitmap);
           end;
           '$ST 04','$ST 05': begin
                MemoText.Append('Status: Fehler');
                ChargeStatus := st_error;
                //Canvas.Draw(0,0,Error.Picture.Bitmap);
           end;
           '$ST fe': begin
                MemoText.Append('Status: Pause');
                ChargeStatus := st_standby;
                //Canvas.Draw(0,0,Standby.Picture.Bitmap);
           end;
           otherwise begin
                MemoText.Append('?????');
           end;
        end;
      end
    else
     begin
       MemoText.Append('Checksum error....');
     end;

    MemoText.SelStart := Length(MemoText.Lines.Text);
  end;
end;

procedure TFormMain.LTcpComponentDisconnect(aSocket: TLSocket);
begin
  MemoText.Append('Connection lost');
  MemoText.SelStart := Length(MemoText.Lines.Text);
end;

procedure TFormMain.MenuItemAboutClick(Sender: TObject);
begin
  MessageDlg('EVSE TCP Control V0.01',
             mtInformation, [mbOK], 0);
end;

procedure TFormMain.MenuItemExitClick(Sender: TObject);
begin
  Close;
end;



procedure TFormMain.SendButtonClick(Sender: TObject);
var str : AnsiString;
var checksum : AnsiString;

begin
  if Length(EditSend.Text) > 0 then begin
    if FNet.Connected then begin
       EditSend.Text := '$' + EditSend.Text;
       str :=  EditSend.Text;
       checksum := CRC(str);
       EditSend.Text := EditSend.Text + '^' + checksum + #13;
       MemoText.Append(EditSend.Text);
       FNet.SendMessage(EditSend.Text);
       EditSend.Text := '';
    end;
  end;
end;

procedure TFormMain.DiconnectButtonClick(Sender: TObject);
begin
  FNet.Disconnect;
  MemoText.Append('Disconnected');
end;



procedure TFormMain.FormCreate(Sender: TObject);
begin
  FNet := LTCP;
  LTCP.SocketNet := LAF_INET;
  if EditIP.Text = '::1' then
    EditIP.Text := 'localhost';
  FIsServer := False;
  FChargeStatus := st_ready;
end;


procedure TFormMain.SendEditKeyPress(Sender: TObject; var Key: char);
begin
  if Key = #13 then
    SendButtonClick(Sender);
end;

procedure TFormMain.TimerQuitTimer(Sender: TObject);
begin
  Close;
end;

procedure TFormMain.TimerUpdateTimer(Sender: TObject);
begin

//   testing only

     Label1.Caption := DateTimeToStr(now());

//   ChargeStatus := TChargeStatus(Ord(ChargeStatus)+1);
//   if ChargeStatus=st_Invalid then
//    ChargeStatus := st_Init;

end;

procedure TFormMain.SendToAll(const aMsg: string);
var
  n: Integer;
begin
   // TCP
    FNet.IterReset; // start at server socket
    while FNet.IterNext do begin // skip server socket, go to clients only
      n := FNet.SendMessage(aMsg, FNet.Iterator);
      if n < Length(aMsg) then
        MemoText.Append('Error on send [' + IntToStr(n) + ']');
    end;
 end;

procedure TFormMain.SetChargeStatus(AValue: TChargeStatus);
begin
  FChargeStatus:=AValue;
  Refresh;
end;

initialization
  {$I main.lrs}

end.

