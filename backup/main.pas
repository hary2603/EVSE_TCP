unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, Buttons, ExtCtrls, Menus, lNetComponents, lNet,syncobjs;

type

  { TFormMain }

  TFormMain = class(TForm)
    ButtonDiconnect: TButton;
    ButtonConnect: TButton;
    Connected: TImage;
    Error: TImage;
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
    TimerQuit: TTimer;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
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
  private
    FNet: TLConnection;
    FIsServer: Boolean;
    procedure SendToAll(const aMsg: string);
  public
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
                 Canvas.Draw(0,0,Ready.Picture.Bitmap);
           end;
           '$ST 02': begin
                MemoText.Append('Status: Verbunden');
                Canvas.Draw(0,0,Connected.Picture.Bitmap);
           end;
           '$ST 03': begin
                 MemoText.Append('Status: Laden');
                 Canvas.Draw(0,0,Charging.Picture.Bitmap);
           end;
           '$ST 04','$ST 05': begin
                MemoText.Append('Status: Fehler');
                Canvas.Draw(0,0,Error.Picture.Bitmap);
           end;
           '$ST fe': begin
                MemoText.Append('Status: Pause');
                Canvas.Draw(0,0,Standby.Picture.Bitmap);
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

    if s = '$ST 02^01' then
      begin
         MemoText.Append('Verbunden');
         Canvas.Draw(0,50,Status.Picture.Bitmap);
      end;

//    if FNet is TLUdp then begin // echo to sender if UDP
//      if FIsServer then
//        FNet.SendMessage(s);
//    end else if FIsServer then // echo to all if TCP
//      SendToAll(s);
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
 // FNet.Disconnect;
  FNet := LTCP;
  LTCP.SocketNet := LAF_INET;
  if EditIP.Text = '::1' then
    EditIP.Text := 'localhost';
  FIsServer := False;
 // SSL.SSLActive := False;
  Canvas.Draw(0,0,Status.Picture.Bitmap);
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

initialization
  {$I main.lrs}

end.

