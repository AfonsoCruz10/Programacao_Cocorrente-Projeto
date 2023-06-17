
import processing.core.PApplet;



import java.util.List;
import java.util.Set;



enum State {
    MENU,
    LOGGED_IN,
    LOGOUT,
    DELETE,
    LOGIN,
    CREATE_ACCOUNT,
    FINDMATCH,
    PLAY,
    JOIN,
    LEADERBOARD,
    GAME,
    LEAVE,
    QUIT,
    WINNER,
    LOSER
}


public class Screen extends PApplet implements Runnable {
  private Keyboard keys;
  private Board board;
  private Data data;
  private int Wscreen = 800;
  private int Hscreen = 800;
  private State state;



  public Screen(Keyboard keys, Board board, Data data) {
    this.keys = keys;
    this.board = board;
    this.data = data;
    state = State.MENU;
  }

  public void settings() {
    size(Wscreen, Hscreen);
  }

  public void draw() {
    clear();
    switch (state) {
    case MENU:
      starterMenu();
      break;
    case LOGIN:
      login();
      break;
    case LOGGED_IN:
      loggedInMenu();
      break;
    case CREATE_ACCOUNT:
      data.option = State.CREATE_ACCOUNT;
      createAccount();
      break;
    case DELETE:
      handleTCPState(State.MENU, State.MENU);
      break;
    case FINDMATCH:
      findMatch();
      handleTCPFM(State.FINDMATCH, State.LOGGED_IN);
      break;
    case LEADERBOARD:
      leaderboard();
      break;
    case GAME:
      println("GAME DRAW");
      game();
      handleTCPState(State.GAME, State.LOGGED_IN);
      break;
    case LOSER:
      loser();
      break;
    case WINNER:
      winner();
      break;
    default:
      break;
    }
  }

  void button(String text, int x, int y, State buttonState) {
    fill(240, 240, 240);
    rect(x, y, Wscreen/2, Hscreen/8, 15);
    fill(0, 0, 0);
    text(text, x + Wscreen/4 - text.length()*4, y + Hscreen/16);
    if (keyPressed && overRect(x, y, Wscreen/2, Hscreen/8)) {
      state = buttonState;
    }
  }

  boolean overRect(int x, int y, int width, int height) {
    return mouseX >= x && mouseX <= x + width &&
      mouseY >= y && mouseY <= y + height;
  }



  void mouseClicked() {
    if (mouseX > loginBtn.x && mouseX < (loginBtn.x + loginBtn.width) && this.state == State.MENU) {
      if (mouseY>loginBtn.y && mouseY<(loginBtn.y+loginBtn.height)) {
        this.state = State.LOGIN; // neste if avançamos para o menu Login
      }
      if (mouseY>registarBtn.y && mouseY<(registarBtn.y+registarBtn.height)) {
        this.state=State.CREATE_ACCOUNT;  // neste if avancamos para o menu Resgistar
      }
      loginBtn.reset();
      registarBtn.reset();
    }

    if (mouseX > usernameField.x && mouseX<(usernameField.x+usernameField.width) && (this.state==State.LOGIN || this.state==State.CREATE_ACCOUNT)) {
      if (mouseY>usernameField.y && mouseY<(usernameField.y+usernameField.height)) {
        //estamos a usar a caixa do username
        usernameField.activate();
        passwordField.deactivate();
        if (passwordField.value.equals("")) passwordField.text = "PASS";
      }
      if (mouseY>passwordField.y && mouseY<(passwordField.y+passwordField.height)) {
        //Atualiza a password e estamos a usar a caixa da pass
        passwordField.activate();
        usernameField.deactivate();
        if (usernameField.value.equals("")) usernameField.text = "USER";
      }
    } else {
      if (this.state==State.LOGIN || this.state==State.CREATE_ACCOUNT) {
        passwordField.deactivate();
        usernameField.deactivate();
      }
    }


    if (mouseX>enviarBtn.x && mouseX<enviarBtn.x+enviarBtn.width && mouseY>enviarBtn.y && mouseY<enviarBtn.y+enviarBtn.height && (this.state==State.LOGIN || this.state==State.CREATE_ACCOUNT)) {
      //verificar se está no ecrã de login
      if (this.state==State.LOGIN) {
        //fazer request e analisar a resposta
        data.username = usernameField.value;
        data.password = passwordField.value;
        handleTCPState(State.LOGGED_IN, State.LOGIN);

        usernameField.reset();
        passwordField.reset();
        enviarBtn.reset();
      }
      if (this.state==State.CREATE_ACCOUNT) {
        //fazer request e analisar a resposta
        data.username = usernameField.value;
        data.password = passwordField.value;
        handleTCPState(State.MENU, State.CREATE_ACCOUNT);

        usernameField.reset();
        passwordField.reset();
        enviarBtn.reset();
      }
    }
    //caso tenhamos selecionado o botao play ficamos a espera de encontrar partida (findingMatch)
    else if (this.state == State.LOGGED_IN) {
      if (mouseX > logoutBtn.x && mouseX<logoutBtn.x+logoutBtn.width && mouseY>logoutBtn.y && mouseY<logoutBtn.y+playBtn.height ) {
        state = State.LOGOUT;
        handleTCPState(State.MENU, State.LOGGED_IN);
      } else if (mouseX>playBtn.x && mouseX<playBtn.x+playBtn.width && mouseY>playBtn.y && mouseY<playBtn.y+playBtn.height) {
        data.findingMatch = true;
        state = State.JOIN;
        handleTCPState(State.FINDMATCH, State.LOGGED_IN);
        data.option = State.FINDMATCH;
      } else if (mouseX>leaderBoardBtn.x && mouseX<leaderBoardBtn.x+leaderBoardBtn.width && mouseY>leaderBoardBtn.y && mouseY<leaderBoardBtn.y+leaderBoardBtn.height) {
        state = State.LEADERBOARD;
        //COMPLETAR
        handleTCPState(State.LEADERBOARD, State.LOGGED_IN);
        usernameField.reset();
        passwordField.reset();
        voltarBtn.reset();
        leaderBoardBtn.reset();
        playBtn.reset();
        deleteAccBtn.reset();
        logoutBtn.reset();
      } else if (mouseX>deleteAccBtn.x && mouseX<deleteAccBtn.x+deleteAccBtn.width && mouseY>deleteAccBtn.y && mouseY<deleteAccBtn.y+deleteAccBtn.height) {

        state = State.DELETE;
        //COMPLETAR
        handleTCPState(State.MENU, State.LOGGED_IN);
      }
    }
    if (this.state == State.FINDMATCH && data.option == State.GAME) this.state = State.GAME;
    if (mouseX>voltarBtn.x && mouseX<voltarBtn.x+voltarBtn.width && mouseY>voltarBtn.y && mouseY<voltarBtn.y+voltarBtn.height && this.state != State.LOGGED_IN) {
      if (this.state==State.MENU) {
        exit();
      } else if (this.state==State.LOGIN || this.state==State.CREATE_ACCOUNT) {
        this.state=State.MENU;
        usernameField.reset();
        passwordField.reset();
        voltarBtn.reset();
      } else if (this.state==State.LEADERBOARD || this.state==State.LOSER || this.state==State.WINNER) {
        this.state=State.LOGGED_IN;
        usernameField.reset();
        passwordField.reset();
        voltarBtn.reset();
      } else if (this.state==State.FINDMATCH) {
        this.state=State.LEAVE;
        handleTCPState(State.LOGGED_IN, State.LOGGED_IN);
        usernameField.reset();
        passwordField.reset();
        voltarBtn.reset();
      }
    }
  }

  void keyPressed() {
    if (this.state!= State.GAME) {
      if (usernameField.isActive()) {
        usernameField.text=usernameField.value;
        usernameField.processKey(key);
      }
      if (passwordField.isActive()) {
        passwordField.text = passwordField.value;
        passwordField.processKey(key);
      }
    } else {
      if (key == 'w') {
        keys.move = true;
      } else if (key == 'a') {
        keys.rotate = Rotate.a;
      } else if (key == 'd') {
        keys.rotate = Rotate.d;
      }
    }
  }

  void keyReleased() {
    if (key == 'a') {
      keys.rotate = Rotate.n;
    } else if (key == 'w') {
      keys.move = false;
    } else if (key == 'd') {
      keys.rotate = Rotate.n;
    }
  }



  void starterMenu() {
    image(background, 0, 0);
    fill(255);
    textSize(50);
    textAlign(CENTER, CENTER);
    text("GET READY TO RUMBLE!", width/2, 200);
    image(loginBtn.image, loginBtn.x, loginBtn.y);
    image(registarBtn.image, registarBtn.x, registarBtn.y);
    image(voltarBtn.image, voltarBtn.x, voltarBtn.y);
  }

  void loggedInMenu() {
    image(background, 0, 0);
    fill(255);
    textSize(50);
    textAlign(CENTER, CENTER);
    text("Bem-vindo!", width/2, 200);
    image(playBtn.image, playBtn.x, playBtn.y);
    image(logoutBtn.image, logoutBtn.x, logoutBtn.y);
    image(leaderBoardBtn.image, leaderBoardBtn.x, leaderBoardBtn.y);
    image(deleteAccBtn.image, deleteAccBtn.x, deleteAccBtn.y);
  }

  void login() {
    image(background, 0, 0);
    fill(255);
    textSize(50);
    textAlign(CENTER, CENTER);
    text("Aceda à sua conta!", width/2, 200);
    image(enviarBtn.image, enviarBtn.x, enviarBtn.y);
    image(voltarBtn.image, voltarBtn.x, voltarBtn.y);
    image(usernameField.image, usernameField.x, usernameField.y);
    image(passwordField.image, passwordField.x, passwordField.y);
    textSize(20);
    fill(255, 165, 0);
    text(usernameField.text, usernameField.x + 88, usernameField.y + 45);
    text(passwordField.text, passwordField.x + 88, passwordField.y + 45);
  }

  void createAccount() {
    image(background, 0, 0);
    fill(255);
    textSize(50);
    textAlign(CENTER, CENTER);
    text("Crie a sua conta!", width/2, 200);
    image(enviarBtn.image, enviarBtn.x, enviarBtn.y);
    image(voltarBtn.image, voltarBtn.x, voltarBtn.y);
    image(usernameField.image, usernameField.x, usernameField.y);
    image(passwordField.image, passwordField.x, passwordField.y);
    textSize(20);
    fill(255, 165, 0);
    text(usernameField.text, usernameField.x + 88, usernameField.y + 45);
    text(passwordField.text, passwordField.x + 88, passwordField.y + 45);
  }

  void findMatch() {
    image(background, 0, 0);
    fill(255);
    textSize(50);
    textAlign(CENTER, CENTER);
    text("AGUARDE!", width/2, 200);
    StringBuilder sb1 = new StringBuilder();
    sb1.append("Fila nivel - ").append(level);
    fill(155);
    text(sb1.toString(), width/2, 280);
    fill(55);
    text("À ESPERA DE ADVERSÁRIO!", width/2, 400);
    image(voltarBtn.image, voltarBtn.x, voltarBtn.y);
  }


  void game() {
    clear();
    Tuple<Tuple<Piece, Piece>, List<Piece>> pieces = board.getBoard();
    image(background, 0, 0);

    StringBuilder sb1 = new StringBuilder();
    sb1.append(pieces.first.first.s).append(" - SCORE - ").append(pieces.first.second.s);
    textSize(50);
    textAlign(CENTER, CENTER);
    text(sb1.toString(), width/2, 10);

    pushMatrix();
    translate(pieces.first.first.x, pieces.first.first.y);
    rotate(pieces.first.first.d);
    image(player1, -30, -30);
    popMatrix();

    pushMatrix();
    translate(pieces.first.second.x, pieces.first.second.y);
    rotate(pieces.first.second.d);
    image(player2, -30, -30);
    popMatrix();


    for (Piece p : pieces.second) {
      if (p.isPlayer) {
        fill(p.r, p.g, p.b, 255);
        circle(p.x, p.y, p.size);
      } else drawCrystal(p);
    }
  }

  void handleTCPState(State nextState, State errorState) {
    try {
      data.lock.lock();
      data.option = state;
      data.waitPostman.signal();
      while (data.response == Response.NOTHING) data.waitScreen.await();

      if (data.response == Response.DONE) {
        state = nextState;
      } else if (data.response == Response.SWITCH) {
        state = data.option;
      } else {
        state = errorState;
        println("ERROR!!!");
      }
      data.response = Response.NOTHING;
    }
    catch (InterruptedException e) {
      throw new RuntimeException(e);
    }
    finally {
      data.lock.unlock();
    }
  }
  void handleTCPFM(State nextState, State errorState) {
    try {
      data.lock.lock();
      data.waitPostman.signal();
      while (data.response == Response.NOTHING) data.waitScreen.await();

      if (data.response == Response.DONE) {
        state = nextState;
      } else if (data.response == Response.SWITCH) {
        state = data.option;
      } else {
        state = errorState;
        println("ERROR!!!");
      }
      data.response = Response.NOTHING;
    }
    catch (InterruptedException e) {
      throw new RuntimeException(e);
    }
    finally {
      data.lock.unlock();
    }
  }

  void leaderboard() {
    image(background, 0, 0);
    fill(255);
    image(voltarBtn.image, voltarBtn.x, voltarBtn.y);
    Set<Tuple<String, Integer>> lb = data.leaderboard;
    StringBuilder sb1 = new StringBuilder();
    StringBuilder sb2 = new StringBuilder();

    for (Tuple<String, Integer> t : lb) {
      sb1.append(t.first).append("\n");
      sb2.append(t.second).append("\n");
    }
    fill(255, 0, 0);
    textSize(50);
    textAlign(CENTER, CENTER);
    String title = "*** Leaderboard ***";
    text(title, Wscreen/2 - title.length()*3+20, Hscreen/4);
    fill(255, 0, 255);
    textSize(20);
    textAlign(CENTER, CENTER);
    text(sb1.toString(), Wscreen/4+100, Hscreen/2+100);
    text(sb2.toString(), 2*Wscreen/4+100, Hscreen/2+100);
  }

  void loser() {
    image(background, 0, 0);
    fill(255);
    image(voltarBtn.image, voltarBtn.x, voltarBtn.y);
    fill(255, 0, 0);
    textSize(200);
    textAlign(CENTER, CENTER);
    String title = "LOSER!";
    text(title, Wscreen/2 - title.length()*3+20, Hscreen/4);
  }
  void winner() {
    image(background, 0, 0);
    fill(255);
    image(voltarBtn.image, voltarBtn.x, voltarBtn.y);
    fill(255, 0, 0);
    textSize(200);
    textAlign(CENTER, CENTER);
    String title = "WINNER!";
    text(title, Wscreen/2 - title.length()*3+20, Hscreen/4);
  }

  void drawCrystal(Piece p) {
    float x1 = 400 + p.x + p.size;
    float y1 = 400 + p.y;
    float x2 = 400 + p.x - p.size;
    float y2 = 400 + p.y;
    float x3 = 400 + p.x;
    float y3 = 400 + p.y - p.size;
    float x4 = 400 + p.x;
    float y4 = 400 + p.y + p.size;
    fill(p.r, p.g, p.b, 255);
    quad(x1, y1, x3, y3, x2, y2, x4, y4);
  }


  @Override
    public void run() {
    String[] processingArgs = {"Screen"};
    Screen screen = new Screen(this.keys, this.board, this.data);
    PApplet.runSketch(processingArgs, screen);
  }
}
