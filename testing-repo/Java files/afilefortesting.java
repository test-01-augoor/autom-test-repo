public class Message {
  int x = 5;
  public static void message(String[] args) {
    Message myObj = new Message();
    System.out.println(myObj.x);
  }
  
  public long doRemoteFunction(int value) {

    long hold = (long) Math.floor(randomProvider.random() * 100);

    try {
      sleep(hold/2);
    } catch (InterruptedException e) {
      LOGGER.error("Thread sleep state interrupted", e);
    }
    return waitTime <= THRESHOLD ? value * 10
            : RemoteServiceStatus.FAILURE.getRemoteServiceStatusValue();
  }
}
