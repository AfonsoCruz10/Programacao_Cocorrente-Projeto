import java.io.BufferedReader; //<>//
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.PrintWriter;
import java.net.Socket;
import java.util.*;

public class TCP {
  private ConnectionManager con = new ConnectionManager() ;
  private Socket s;
  StringBuilder sb = new StringBuilder();

  public TCP(Socket s) throws IOException {
    this.s = s;
    this.con.start(s);
  }

  public Socket getSocket(){return this.s;}

  public void login(String username, String password) throws IOException, InvalidPassword, InvalidAccount {
    String type = "login";
    sb.append(username);
    sb.append(" ");
    sb.append(password);
    this.con.send(type, sb.toString()); // login#username password
    sb.setLength(0);

    String response = this.con.receive(type);

    switch (response) {
    case "done":
      break;
    case "invalid_account":
      throw new InvalidAccount("Invalid account.");
    case "invalid_password":
      throw new InvalidPassword("Invalid password.");
    }
  }

  public void logout(String username) throws IOException, InvalidPassword, InvalidAccount {
    String type="logout";
    sb.append(username);
    this.con.send(type, sb.toString()); // logout#username password
    sb.setLength(0);

    String response = this.con.receive(type);
    switch (response) {
    case "done":
      break;
    case "invalid_account":
      throw new InvalidAccount("Invalid account.");
    case "invalid_password":
      throw new InvalidPassword("Invalid password.");
    }
  }
  public int join(String username, String password) throws IOException, InvalidAccount, FullServer {
    String type="join";
    sb.append(username);
    sb.append(" ");
    sb.append(password);
    this.con.send(type, sb.toString()); // login#username password
    sb.setLength(0);
    String response = this.con.receive(type);
    String responseParts[] = response.split(" ");
    switch (responseParts[0]) {
    case "done":
      break;
    case "invalid_auth":
      throw new InvalidAccount("Invalid account.");
    case "full_server":
      throw new FullServer("Full server.");
    }
    return Integer.parseInt(responseParts[1]);
  }

  public void leave(String username) throws IOException {
    String type="leave";
    sb.append(username);
    this.con.send(type, sb.toString());
    sb.setLength(0);
    
    String response = this.con.receive(type);
    println("leave response");
    println(response);
    switch (response) {
    case "done":
      println("donezito");
      break;
    case "invalid_command":
      throw new IOException("Invalid command.");
    }
  }

  public void create_account(String username, String password) throws IOException, InvalidPassword, UserExists {
    String type="create_account";
    sb.append(username);
    sb.append(" ");
    sb.append(password);
    this.con.send(type, sb.toString()); // create_account#username password
    sb.setLength(0);

    String response = this.con.receive(type);
    switch (response) {

    case "done":
      break;
    case "user_exists":
      throw new UserExists("User already exists.");
    case "invalid_password":
      throw new InvalidPassword("Invalid password.");
    }
  }

  public void remove_account(String username, String password) throws IOException, InvalidPassword, InvalidAccount {
    String type="remove_account";
    sb.append(username);
    sb.append(" ");
    sb.append(password);
    this.con.send(type, sb.toString()); // remove_account#username password
    sb.setLength(0);

    String response = this.con.receive(type);
    switch (response) {
    case "done":
      break;
    case "invalid_account":
      throw new InvalidAccount("Invalid account.");
    case "invalid_password":
      throw new InvalidPassword("Invalid password.");
    }
  }

  public Set<Tuple<String, Integer>> leaderboard() throws IOException {
    String type="leaderboard";
    this.con.send(type, ""); // leaderboard:
    sb.setLength(0);

    String response = this.con.receive(type);
    String[] playerStrings = response.split("\\|");


    Comparator<Tuple<String, Integer>> comp = (a1, a2) -> (!Objects.equals(a1.second, a2.second)) ? (a2.second - a1.second) : a1.first.compareTo(a2.first);
    Set<Tuple<String, Integer>> players = new TreeSet<>(comp);

    for (String playerString : playerStrings) {
      String[] playerInfo = playerString.split(" ");
      Tuple<String, Integer> newPlayer = new Tuple<>(playerInfo[0], Integer.parseInt(playerInfo[1]));
      players.add(newPlayer);
    }

    return players;
  }


  public void keys(String keys) {
    String type="move";
    sb.append(keys);
    try {
      this.con.send(type, sb.toString()); // login#username password
    }
    catch (IOException e) {
      println(e.getMessage());
    }
    sb.setLength(0);
  }

  public Set<String> online() throws IOException {
    String type="online";
    this.con.send(type, sb.toString());
    sb.setLength(0);

    String response = this.con.receive(type);
    String[] playerStrings = response.split(" ");
    Set<String> users = new TreeSet<>();

    for (String user : playerStrings) users.add(user);

    return users;
  }
}
