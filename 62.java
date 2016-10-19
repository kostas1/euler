import java.util.*;

/**
 * Created by Kostas on 2016.10.16.
 */
public class Application {

    public static void main(String[] args) {
        Map<String, List<Long>> counts = new HashMap<>();

        for (long c = 1;; c++) {
            if (c % 100 == 0) {
                System.out.println(c);
            }

            long cube = c * c * c;
            String sorted = sorted(cube);
            List<Long> cs = counts.getOrDefault(sorted, new ArrayList<>());
            cs.add(cube);
            counts.put(sorted, cs);
            if (cs.size() == 5) {
                for (long l: cs) {
                    System.out.println(l);
                }
                return;
            }
        }
    }

    private static String sorted(long value) {
        char[] arr = String.valueOf(value).toCharArray();
        Arrays.sort(arr);
        return new String(arr);
    }
}
