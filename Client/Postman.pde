import java.io.IOException;
import java.util.List;

public class Postman implements Runnable {

  private TCP tcp;
  private Keyboard keys;
  private Data data;
  private Board board;


  public Postman(TCP tcp, Keyboard keys, Board board, Data data) {
    this.tcp = tcp;
    this.keys = keys;
    this.board = board;
    this.data = data;
     
  }

  public void run() {
    while (true) {
      try {
        data.lock.lock();
        data.waitPostman.await();
        switch(data.option) {
        case LOGIN:
          tcp.login(data.username, data.password);
          data.response = Response.DONE;
          break;
        case CREATE_ACCOUNT:
          tcp.create_account(data.username, data.password);
          data.username = "";
          data.password = "";
          data.response = Response.DONE;
          break;
        case DELETE:
          tcp.remove_account(data.username, data.password);
          data.username = "";
          data.password = "";
          data.response = Response.DONE;
          break;
        case LOGOUT:
          tcp.logout(data.username);
          data.username = "";
          data.password = "";
          data.response = Response.DONE;
          break;
        case LEADERBOARD:
          data.leaderboard = tcp.leaderboard();
          data.response= Response.DONE;
          break;
        case JOIN:
          level = tcp.join(data.username, data.password);
          data.response = Response.DONE;
          new Thread(()-> {
            try {
              while (data.findingMatch) {
                String response = tcp.con.receive("Play");
                if (response.equals("start")) {
                  data.option = State.GAME;
                  response = tcp.con.receive("Game");
                  board.setBoard(data.username, response);
                  data.findingMatch = false;
                }
              }
            }
            catch (IOException e) {
              println(e);
            }
          }
          ).start();
          break;
        case FINDMATCH:
          data.response = Response.SWITCH;
          break;
        case GAME:
          String response = tcp.con.receive("Game");
          if (response == null || response.equals("defeat")) {
            data.option = State.LOSER;
            data.response = Response.SWITCH;
          } else if (response.equals("winner")) {
            data.option = State.WINNER;
            data.response = Response.SWITCH;
          } else {
            board.setBoard(data.username, response);
            tcp.keys(keys.toString());
            data.response = Response.DONE;
          }
          break;
        case LEAVE:
          data.findingMatch = false;
          //myThread.interrupt();
          tcp.leave(data.username);
          data.response = Response.DONE;
          break;
        case QUIT:
          tcp.con.send("leave", "");
          tcp.con.receive("QUIT");
          data.response = Response.DONE;
          break;
        default:
          break;
        }
        data.waitScreen.signal();
      }
      catch (InterruptedException | IOException e) {
        throw new RuntimeException(e);
      }
      catch (InvalidPassword | InvalidAccount | UserExists | FullServer e) {
        data.response = Response.ERROR;
        data.username = "";
        data.password = "";
        data.waitScreen.signal();
      }
      finally {
        data.lock.unlock();
      }
    }
  }
}
