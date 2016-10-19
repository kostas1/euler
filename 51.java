import org.apache.commons.math3.primes.Primes;
import org.apache.commons.math3.util.Combinations;

import java.util.ArrayList;
import java.util.List;

/**
 * Created by Kostas on 2016.10.16.
 */
public class Application {

    public static void main(String[] args) {
        for (int prime = Primes.nextPrime(101);; prime = Primes.nextPrime(prime + 1)) {
            String primeString = String.valueOf(prime);
            int take = primeString.length() - 1;
            List<int[]> indexReplacements = takeUpTo(primeString.length(), take);
            for (int[] replacements: indexReplacements) {
                List<Integer> primes = new ArrayList<>();
                for (int n = 0; n < 10; n++) {
                    char[] primeClone = primeString.toCharArray().clone();
                    for (int index: replacements) {
                        primeClone[index] = Character.forDigit(n, 10);
                    }
                    int newPrime = Integer.parseInt(new String(primeClone));
                    if (primeString.length() == String.valueOf(newPrime).length() && Primes.isPrime(newPrime)) {
                        primes.add(newPrime);
                    }
                }
                if (primes.size() > 7) {
                    for (int nPrime: primes) {
                        System.out.println(nPrime);
                    }
                    return;
                }
            }
        }
    }

    private static List<int[]> takeUpTo(int total, int take) {
        List<int[]> takeList = new ArrayList<>();
        for (int i = 1; i <= take; i++) {
            new Combinations(total, i).forEach(takeList::add);
        }
        return takeList;
    }
}
