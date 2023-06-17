

enum Rotate {
  a,
    d,
    n
}


public class Keyboard {

  public boolean move = false;
  public Rotate rotate = Rotate.n;


  public synchronized void setKeys(boolean W, Rotate R) {
    this.move = W;
    this.rotate = R;
  }



  public synchronized String toString() {
    StringBuilder sb = new StringBuilder();
    if (this.move)
      sb.append("w ");
    else sb.append("x ");
    if (this.rotate  != Rotate.n) {
      if (this.rotate  == Rotate.a)
        sb.append("a");
      else if (this.rotate  == Rotate.d)
        sb.append("d");
    } else sb.append("x");
    return sb.toString();
  }
}
