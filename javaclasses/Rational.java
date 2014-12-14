public class Rational {

     public int num, denom;

     public Rational(double d) {
          String s = String.valueOf(d);
          int digitsDec = s.length() - 1 - s.indexOf('.');        

          int denom = 1;
          for(int i = 0; i < digitsDec; i++){
             d *= 10;
             denom *= 10;
          }
          int num = (int) Math.round(d);

          this.num = num; this.denom = denom;
     }

     public Rational(int num, int denom) {
          this.num = num; this.denom = denom;
     }

     public String toString() {
          return String.valueOf(num) + "/" + String.valueOf(denom);
     }

     public static void main(String[] args) {
          System.out.println(new Rational(123.456));
     }
}