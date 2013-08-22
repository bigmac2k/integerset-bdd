package cc.sven;

import cc.sven.intset.IntSet;


public class Main {
	public static void main(String[] args) {
		IntSet<Integer> foo = IntSet.applyJava(new Integer(1));
		System.out.println(foo.contains(new Integer(1)));
	}
}
