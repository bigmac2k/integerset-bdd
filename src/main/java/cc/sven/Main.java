package cc.sven;


import cc.sven.intset.*;
import cc.sven.bounded.*;

public class Main {
	public static void main(String[] args) {
		IntSet<Integer> intset = IntSet$.MODULE$.apply(new JIntegerIsIntegral(), new JIntegerIsBounded(), new JIntegerIsBoundedBits());
		intset = intset.unary_$bang();
		intset = intset.bitExtract(0, 2);
		System.out.println("gogogo");
		for(Integer i : intset.java()) {
			System.out.println(i.toString());
		}
	}
}