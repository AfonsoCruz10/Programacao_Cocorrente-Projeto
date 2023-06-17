

class Button {
  PImage image;
  PImage hover;
  PImage regular;
  float x;
  float y;
  float width;
  float height;

  Button(String path) {
    this.image=loadImage(path);
    this.regular=loadImage(path);
    this.hover=loadImage(path);
    this.width=this.image.width;
    this.height=this.image.height;
  }

  Button(String path, String hoverPath) {
    this.image=loadImage(path);
    this.hover = loadImage(hoverPath);
    this.regular = this.image;
    this.width=this.image.width;
    this.height=this.image.height;
  }

  void updatePosition(float x, float y) {
    this.x=x;
    this.y=y;
  }

  void reset() {
    this.image=this.regular;
  }
}

class InputField {
  PImage image;
  float x;
  float y;
  float width;
  float height;
  String text="";
  //default text to store
  String dft="";
  String value="";
  boolean isActive;

  InputField(String path) {
    this.image=loadImage(path);
    this.width=this.image.width;
    this.height=this.image.height;
  }

  InputField(String path, String text) {
    this.image=loadImage(path);
    this.width=this.image.width;
    this.height=this.image.height;
    this.text=text;
    this.dft=text;
  }

  void updateText(String text) {
    this.text=text;
  }

  void updatePosition(float x, float y) {
    this.x=x;
    this.y=y;
  }

  void activate() {
    if (value.equals("")) text = "";
    this.isActive=true;
  }

  void deactivate() {

    this.isActive=false;
  }

  boolean isActive() {
    return this.isActive;
  }

  void processKey(char key) {
    if (this.isActive ) {
      int temp=(int)key;
      if (temp==8)
        deleteChar();
      else {
        if (temp>=32 && temp<127)
          this.value+=key;
      }
      this.text=this.value;
    }
  }

  void deleteChar() {
    if (value.length() > 0) {
      value = value.substring(0, value.length()-1);
    }
  }

  void reset() {
    this.value = "";
    this.text=this.dft;
  }
}
