import java.util.*;
import java.util.stream.*;

public class CountPrimes {

	public static int half(int v) {
		return (v - 1) / 2;
	}

	public static int countPrimes(int n) {
		if (n < 2) {
			return 0;
		}
		if (n < 3) {
			return 1;
		}
		int sqrtn = (int) Math.sqrt(n);
		int mxndx = (sqrtn - 1) / 2;

		int[] smalls = IntStream.rangeClosed(0, mxndx).toArray();
		int[] roughs = IntStream.rangeClosed(0, mxndx).map(i -> 2 * i + 1).toArray();
		int[] larges = IntStream.rangeClosed(0, mxndx)
				.map(i -> {
					int d = 2 * i + 1;
					return (n / d - 1) / 2;
				})
				.toArray();

		boolean[] skips = new boolean[mxndx + 1];
		int npc = 0;
		int mxri = mxndx;

		for (int i = 1;; ++i) {
			int sqri = (i + i) * (i + 1);
			if (sqri > mxndx) {
				break;
			}	
			if (skips[i]) {
				continue;
			}
			skips[i] = true;
			int bp = 2 * i + 1;
			for (int k = sqri; k <= mxndx; k += bp) {
				skips[k] = true;
			}
			int ri = 0; // rough index
			for (int k = 0; k <= mxri; ++k) {
				int q = roughs[k]; // always odd
				int qi = q / 2;
				if (skips[qi]) {
					continue;
				}
				int d = bp * q;
				larges[ri] = larges[k] 
					- (d <= sqrtn 
						? larges[smalls[d / 2] - npc]
						: smalls[half(n / d)])
					+ npc;
				roughs[ri] = q;
				++ri;
			}
			int m = mxndx;
			for (int k = (sqrtn / bp - 1) | 1; k >= bp; k -= 2) {
				int c = smalls[k / 2] - npc;
				int e = bp * k / 2;
				for (; m >= e; --m) {
					smalls[m] -= c;
				}
			}
			mxri = ri - 1;
			++npc;
		}
		int result = larges[0];
		for (int i = 1; i <= mxri; ++i) {
			result -= larges[i];
		}
		result += (mxri + 1 + 2 * (npc - 1)) * mxri / 2;
		for (int j = 1; j <= mxri; ++j) {
			int p = roughs[j];
			int m = n / p;
			int e = smalls[half(m / p)] - npc;
			if (e <= j) {
				break;
			}
			for (int k = j + 1; k <= e; ++k) {
				result += smalls[half(m / roughs[k])];
			}
			result -= (e - j) * (npc + j - 1);
		}
		++result;
		return result;
	}
	
	public static void main(String[] args) { 
		for (int i = 0; i <= 9; ++i) {
			int n = (int) Math.pow(10, i);
			System.out.println(countPrimes(n));
		}
	}
}
