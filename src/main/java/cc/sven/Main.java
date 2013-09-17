package cc.sven;


import scala.Tuple2;
import cc.sven.integral.*;
import cc.sven.bounded.*;
import cc.sven.tlike.*;

public class Main {
	public static void main(String[] args) {
		/*Castable<Tuple2<Integer, Integer>, Tuple2<Integer, Integer>> iCaster = new IdCaster<Tuple2<Integer, Integer>>();
		IntLikeSet<Integer, Integer> set = IntLikeSet$.MODULE$.apply(32, new JIntegerIsIntegral(), new JIntegerIsBounded(), new JIntegerIsBoundedBits(), DynBoundedBits$.MODULE$.boundedBitsToDynBoundedBits(new JIntegerIsBoundedBits()), iCaster, iCaster);
		set = set.$plus(4);
		set = set.$plus(88);
		for(Integer i : set.java()) {
			System.out.println(i);
		}*/
		System.out.println("hallo");
	}
}