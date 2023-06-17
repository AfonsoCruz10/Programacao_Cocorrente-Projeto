import java.util.ArrayList;
import java.util.List;


public class Board {
  public Piece me;
  public Piece opponent;
  public List<Piece> pieces = new ArrayList<>();

  public synchronized Piece getMe() {
    return this.me;
  }

  public synchronized void setBoard(Piece p, List<Piece> lp) {
    this.me = p;
    this.pieces = lp;
  }

  public synchronized void setBoard(String username, String response) {
    this.pieces = new ArrayList<>();
    String[] positionsString = response.split("\\|");
    for (String pieceString : positionsString) {
      String[] pieceInfo = pieceString.split(" ");
      if (!pieceInfo[0].equals("<>")) {
        Piece piece = new Piece(pieceInfo[0], Float.parseFloat(pieceInfo[1]), Float.parseFloat(pieceInfo[2]), Float.parseFloat(pieceInfo[3]), Integer.parseInt(pieceInfo[4]));
        if (piece.username.equals(username)) {
          this.me = piece;
          me.seti(player1);
        } else {
          this.opponent = piece;
          opponent.seti(player2);
        }
      } else {
        Piece piece = new Piece(pieceInfo[0], Float.parseFloat(pieceInfo[2]), Float.parseFloat(pieceInfo[3]), pieceInfo[1]);
        this.pieces.add(piece);
      }
    }
  }

  public synchronized Tuple<Tuple<Piece,Piece>, List<Piece>> getBoard() {
    return new Tuple<Tuple<Piece,Piece>, List<Piece>>( new Tuple(this.me,this.opponent), this.pieces);
  }
}
