
//Primeiro abrir uma thread para falar entre o cliente e o servidor
//Método de registo
//Método de login

//Server criar queue
// Hashmap <int(nivel), listaDePlayers>




/*
0 menu inical
1 gameplay
2 end game
*/

/*
Button loginBtn;
Button registarBtn;
Button playBtn;
Button voltarBtn;
Button enviarBtn;
InputField usernameField,passwordField;
Button logoutBtn;
//piece player1;
//piece player2;
boolean findingMatch = false;
PImage background;





enum Screen{
  mainMenu,
  login,
  register,
  play,
  findMatch 
}

Screen screen = Screen.mainMenu;

void setup(){
  println("\nasdasHello, world!");
   size(720, 720);
   initializeBoard(); 
}



void draw(){
  background(255);
  
  switch(screen){
    case mainMenu: // main menu
    {
      
      image(background,0,0);
      fill(255);
      textSize(50);
      textAlign(CENTER,CENTER);
      text("GET READY TO RUMBLE!",width/2,200);
      image(loginBtn.image,loginBtn.x,loginBtn.y);
      image(registarBtn.image,registarBtn.x,registarBtn.y);
      image(voltarBtn.image,voltarBtn.x,voltarBtn.y);
      
      
      
    }break;
    case login: // login
    {
      image(background,0,0);
      fill(255);
      textSize(50);
      textAlign(CENTER,CENTER);
      text("Aceda à sua conta!",width/2,200);
      image(enviarBtn.image, enviarBtn.x, enviarBtn.y);
      image(voltarBtn.image, voltarBtn.x, voltarBtn.y);
      image(usernameField.image, usernameField.x, usernameField.y);
      image(passwordField.image, passwordField.x, passwordField.y);
      textSize(20);
      fill(255,165,0);
      text(usernameField.text, usernameField.x + 88 , usernameField.y + 45);
      text(passwordField.text, passwordField.x + 88, passwordField.y + 45);
      
      
    }break;
    case register: // criar conta
    {
      image(background,0,0);
      fill(255);
      textSize(50);
      textAlign(CENTER,CENTER);
      text("Crie a sua conta!",width/2,200);
      image(enviarBtn.image, enviarBtn.x, enviarBtn.y);
      image(voltarBtn.image, voltarBtn.x, voltarBtn.y);
      image(usernameField.image, usernameField.x, usernameField.y);
      image(passwordField.image, passwordField.x, passwordField.y);
      textSize(20);
      fill(255,165,0);
      text(usernameField.text, usernameField.x + 88 , usernameField.y + 45);
      text(passwordField.text, passwordField.x + 88, passwordField.y + 45);
      //image(enviarBtn);
      
    }break;
    case play: // jogar
    {
      image(background,0,0);
      fill(255);
      textSize(50);
      textAlign(CENTER,CENTER);
      text("Bem-vindo!",width/2,200);
      image(voltarBtn.image,voltarBtn.x,voltarBtn.y);
      image(playBtn.image,playBtn.x,playBtn.y);
      image(logoutBtn.image,logoutBtn.x,logoutBtn.y);
      
    }break;
    case findMatch:
    {
      image(background,0,0);
      fill(255);
      textSize(50);
      textAlign(CENTER,CENTER);
      text("AGUARDE!",width/2,200);
      text("À ESPERA DE ADVERSÁRIO!",width/2,360);
    }
    
  }
  //fill(player1.r, player1.g, player1.b);
  //ellipse(player1.x,player1.y, 20,20);
  //fill(player2.r, player2.g, player2.b);
  //ellipse(player2.x, player2.y, 20,20);
}


void initializeBoard(){
  //player1= new piece("Player1", "255,255,255", 0,0,1);
  //player2= new piece("Player2", "120,120,120", 100,100,1);
  background = loadImage("./images/background.png");
  loginBtn = new Button("./images/loginbtn.png");
  loginBtn.updatePosition(width/2-loginBtn.width/2, height/2-loginBtn.height/2);
  
  registarBtn = new Button("./images/registarbtn.png");
  registarBtn.updatePosition(width/2-loginBtn.width/2, height/2-loginBtn.height/2);
  registarBtn.updatePosition(loginBtn.x,loginBtn.y+loginBtn.height+10);
  
  voltarBtn = new Button("./images/voltarbtn.png");
  //voltarBtn.updatePosition(width/2-loginBtn.width/2, height/2-loginBtn.height/2);
  //voltarBtn.updatePosition(registarBtn.x,registarBtn.y+loginBtn.height+10);
  voltarBtn.updatePosition(10, 10);
  
  playBtn = new Button("./images/playbtn.png");
  playBtn.updatePosition(width/2-playBtn.width/2, height/2-playBtn.height/2);
  
  logoutBtn = new Button("./images/logoutbtn.png");
 
  logoutBtn.updatePosition(width/2-playBtn.width/2, height/2-playBtn.height/2);
  logoutBtn.updatePosition(playBtn.x,playBtn.y+playBtn.height+10);
  
  usernameField = new InputField("./images/caixatexto.png","USER");
  usernameField.updatePosition( width/2-usernameField.width/2,height/2-usernameField.height/2+10);
  passwordField = new InputField("./images/caixatexto.png","PASS");
  passwordField.updatePosition( usernameField.x, usernameField.y+usernameField.height+10);
  
  enviarBtn = new Button("./images/enviarbtn.png");
  enviarBtn.updatePosition(passwordField.x,passwordField.y+passwordField.height+10);
  
  
  screen = Screen.mainMenu;
}

void mouseClicked(){
  
  if(mouseX > loginBtn.x && mouseX < (loginBtn.x + loginBtn.width) && screen == Screen.mainMenu){
    if(mouseY>loginBtn.y && mouseY<(loginBtn.y+loginBtn.height)){
      loginBtn.reset();
      screen = Screen.login; // neste if avançamos para o menu Login
      
    }
    if(mouseY>registarBtn.y && mouseY<(registarBtn.y+registarBtn.height)){
      screen=Screen.register;  // neste if avancamos para o menu Resgistar
      registarBtn.reset();
    }
    
  }
  
  if(mouseX > usernameField.x && mouseX<(usernameField.x+usernameField.width) && (screen==Screen.login || screen==Screen.register)){
    if(mouseY>usernameField.y && mouseY<(usernameField.y+usernameField.height)){
      //estamos a usar a caixa do username
      usernameField.activate();
      passwordField.deactivate();

    }
    if(mouseY>passwordField.y && mouseY<(passwordField.y+passwordField.height)){
      //Atualiza a password e estamos a usar a caixa da pass
      passwordField.activate();
      usernameField.deactivate();
    }
  }
  else{
    if(screen==Screen.login){
      passwordField.deactivate();
      usernameField.deactivate();
    }
  }
  
  
  if(mouseX>enviarBtn.x && mouseX<enviarBtn.x+enviarBtn.width && mouseY>enviarBtn.y && mouseY<enviarBtn.y+enviarBtn.height){
     //verificar se está no ecrã de login
     if(screen==Screen.login){
        //fazer request e analisar a resposta  
        usernameField.reset();
        passwordField.reset();
        enviarBtn.reset();
        screen=Screen.play;
      
     }
     if(screen==Screen.register){
        //fazer request e analisar a resposta
        screen=Screen.mainMenu;
        usernameField.reset();
        passwordField.reset();
        voltarBtn.reset();
     }
  }
  //caso tenhamos selecionado o botao play ficamos a espera de encontrar partida (findingMatch)
  if(screen == Screen.play){
    if(mouseX > logoutBtn.x && mouseX<logoutBtn.x+logoutBtn.width && mouseY>logoutBtn.y && mouseY<logoutBtn.y+playBtn.height){
      screen = Screen.mainMenu;
    }
    if(mouseX>playBtn.x && mouseX<playBtn.x+playBtn.width && mouseY>playBtn.y && mouseY<playBtn.y+playBtn.height){
        findingMatch=true; 
        screen=Screen.findMatch;
     } 
  }
  
  if(mouseX>voltarBtn.x && mouseX<voltarBtn.x+voltarBtn.width && mouseY>voltarBtn.y && mouseY<voltarBtn.y+voltarBtn.height){
    if (screen==Screen.mainMenu){
       exit();
    }
    else{
      screen=Screen.mainMenu;
      usernameField.reset();
      passwordField.reset();
      voltarBtn.reset();
      playBtn.reset();
    }
  }
}

void keyPressed(){
  if(usernameField.isActive()){
    usernameField.text=usernameField.value;
    usernameField.processKey(key);
  }
  if(passwordField.isActive()){
    passwordField.text = passwordField.value;
    passwordField.processKey(key);
  }
}*/
