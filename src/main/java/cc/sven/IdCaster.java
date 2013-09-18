package cc.sven;

import cc.sven.tlike.Castable;

public class IdCaster<A> implements Castable<A, A> {
  public A apply(A a) { return a; };
}
