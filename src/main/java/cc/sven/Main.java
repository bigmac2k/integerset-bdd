package cc.sven;


import cc.sven.integral.*;
import cc.sven.bounded.*;
import cc.sven.tlike.*;
import cc.sven.misc.*;

public class Main {
	public static void main(String[] args) {
		Castable<Pair<Integer, Integer>, Pair<Integer, Integer>> iCaster = new IdCaster<Pair<Integer, Integer>>();
		//IntLikeSet<Integer, Integer> set = IntLikeSet$.MODULE$.apply(32, new JIntegerIsIntegral(), new JIntegerIsBounded(), new JIntegerIsBoundedBits(), DynBoundedBits$.MODULE$.boundedBitsToDynBoundedBits(new JIntegerIsBoundedBits()), iCaster, iCaster);
		/*set = set.$plus(4);
		set = set.$plus(88);
		for(Integer i : set.java()) {
			System.out.println(i);
		}
		System.out.println("hallo");*/
	}
}