import java.util.Objects;

public class Piece {
  public float x;
  public float y;
  public float d;
  public String username;
  public int r = 0;
  public int g = 0;
  public int b = 0;
  public int size;
  public int s = 0;
  public PImage image;
  public boolean isPlayer;

  public void seti (PImage i){
     image = i;
  }

  public Piece(String username, float x, float y, String c) {
    this.username = username;
    setc(c);
    this.x = x;
    this.y = y;
    this.isPlayer = false;
    this.size = 15;
  }
  public Piece(String username, float x, float y, float d, int s) {
    this.username = username;
    this.d = d;
    this.x = x;
    this.y = y;
    this.s = s;
    this.isPlayer = true;
    this.size = 60;
  }


  void setc(String c) {

    if (c.equals("blue"))
      this.b = 255;
    else if (c.equals("red"))
      this.r = 255;
    else if (c.equals("green"))
      this.g  = 255;
    else if (c.equals("p1")) {
      this.b = 255;
      this.g  = 255;
    } else {
      this.r = 255;
      this.g  = 255;
    }
  }
  
}
