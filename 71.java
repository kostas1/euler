import org.apache.commons.math3.fraction.Fraction;
import org.apache.commons.math3.util.ArithmeticUtils;

/**
 * Created by Kostas on 2016.10.16.
 */
public class Application {

    public static void main(String[] args) {

        double upperBound = 0.42857142857142857142857142857143;
        double minBound = 0.375;

        Fraction currentBest = null;
        for (int d = 1; d <= 1000000; d++) {
            int n = (int) (d * minBound);
            while (((double) n / d) < upperBound) {
                if (ArithmeticUtils.gcd(n, d) == 1) {
                    Fraction current = new Fraction(n, d);
                    if (currentBest == null) {
                        currentBest = current;
                        minBound = currentBest.doubleValue();
                    } else if (current.compareTo(currentBest) > 0) {
                        currentBest = current;
                        minBound = currentBest.doubleValue();
                    }
                }
                n++;
            }
        }
        System.out.println(currentBest);
    }
}
