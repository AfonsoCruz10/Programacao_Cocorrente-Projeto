import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.net.Socket;
import java.io.PrintWriter;
import java.util.*;


class ConnectionManager implements AutoCloseable {
  private Socket socket;
  private BufferedReader reader;
  private BufferedWriter writer;

  public ConnectionManager start(Socket socket) throws IOException {

    this.socket = socket;
    this.reader = new BufferedReader(new InputStreamReader(socket.getInputStream()));
    this.writer = new BufferedWriter(new OutputStreamWriter(socket.getOutputStream()));
    return this;
  }
  
  public void send(String type, String message) throws IOException {

    String formattedMessage = type + ":" + message + "\n";
    print(formattedMessage);
    writer.write(formattedMessage);
    writer.flush();
  }

  public String receive(String type) throws IOException {
    while (!Thread.currentThread().isInterrupted()) {
      String message = reader.readLine();
      println(message);
      if (message.startsWith(type + ":")) {
        return message.substring(type.length() + 1);
      }
    }
    throw new IOException("Thread interrupted");
  }


  @Override
    public synchronized void close() throws IOException {
    socket.close();
  }
}
