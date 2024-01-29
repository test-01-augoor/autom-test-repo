public class Message {
  int x = 5;
  public static void message(String[] args) {
    Message myObj = new Message();
    System.out.println(myObj.x);
    System.out.println("some change");
  }
}