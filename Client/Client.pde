import processing.core.PApplet;
import java.util.List;
import java.util.Set;



Button loginBtn;
Button registarBtn;
Button playBtn;
Button voltarBtn;
Button enviarBtn;
Button leaderBoardBtn;
Button deleteAccBtn;
InputField usernameField, passwordField;
Button logoutBtn;
PImage player1;
PImage player2;
PImage background;
int level;



int Wscreen = 800;
int Hscreen = 800;



void setup() {
  try {
    String serverHost = "localhost"; // The server host (e.g., "localhost")
    int serverPort = 8000; // The server port number
    Socket s = new Socket(serverHost, serverPort);
    TCP tcp = new TCP(s);
    Keyboard keys = new Keyboard();
    Board board = new Board();
    Data data = new Data();

    Socket s2 = new Socket(serverHost, serverPort);
    TCP tcp2 = new TCP(s2);
    Keyboard keys2 = new Keyboard();
    Board board2 = new Board();
    Data data2 = new Data();

    initializeBoard();
    new Thread(new Screen(keys, board, data)).start();

    new Thread(new Postman(tcp, keys, board, data)).start();

    new Thread(new Screen(keys2, board2, data2)).start();

    new Thread(new Postman(tcp2, keys2, board2, data2)).start();
  }
  catch (Exception e) {
    println(e.getMessage());
  }
}



void initializeBoard() {

  player1= loadImage("./images/avatar1grande.png");
  player2= loadImage("./images/avatar2grande.png");
  player1.resize(60,60);
  player2.resize(60,60);
  background = loadImage("./images/background.png");
  background.resize(800, 800);
  loginBtn = new Button("./images/loginbtn.png");
  loginBtn.updatePosition(Wscreen/2-loginBtn.width/2, Hscreen/2-loginBtn.height/2);

  registarBtn = new Button("./images/registarbtn.png");
  registarBtn.updatePosition(Wscreen/2-loginBtn.width/2, Hscreen/2-loginBtn.height/2);
  registarBtn.updatePosition(loginBtn.x, loginBtn.y+loginBtn.height+10);

  voltarBtn = new Button("./images/voltarbtn.png");
  //voltarBtn.updatePosition(width/2-loginBtn.width/2, height/2-loginBtn.height/2);
  //voltarBtn.updatePosition(registarBtn.x,registarBtn.y+loginBtn.height+10);
  voltarBtn.updatePosition(10, 10);

  deleteAccBtn = new Button("./images/deleteaccbtn.png");
  deleteAccBtn.updatePosition(10, Hscreen-80);


  playBtn = new Button("./images/playbtn.png");
  playBtn.updatePosition(Wscreen/2-playBtn.width/2, Hscreen/2-playBtn.height/2);

  leaderBoardBtn = new Button("./images/leaderboardbtn.png");
  leaderBoardBtn.updatePosition(playBtn.x, playBtn.y+playBtn.height+10);

  logoutBtn = new Button("./images/logoutbtn.png");

  logoutBtn.updatePosition(Wscreen/2-leaderBoardBtn.width/2, Hscreen/2-leaderBoardBtn.height/2);
  logoutBtn.updatePosition(leaderBoardBtn.x, leaderBoardBtn.y+leaderBoardBtn.height);

  usernameField = new InputField("./images/caixatexto.png", "USER");
  usernameField.updatePosition( Wscreen/2-usernameField.width/2, Hscreen/2-usernameField.height/2+10);
  passwordField = new InputField("./images/caixatexto.png", "PASS");
  passwordField.updatePosition( usernameField.x, usernameField.y+usernameField.height+10);

  enviarBtn = new Button("./images/enviarbtn.png");
  enviarBtn.updatePosition(passwordField.x, passwordField.y+passwordField.height+10);
}
