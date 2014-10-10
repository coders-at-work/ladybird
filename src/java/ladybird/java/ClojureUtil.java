package ladybird.java;

import clojure.lang.RT;
import clojure.lang.Var;
import clojure.lang.IFn;
import clojure.lang.Symbol;

public class ClojureUtil {
	private static IFn requireFn = fn("clojure.core", "require");

	public static Symbol symbol(String s) {
		return Symbol.intern(s);
	}
	
	public static Var var(String ns, String name) {
		return RT.var(ns, name);
	}

	public static IFn fn(String ns, String name) {
		return var(ns, name).fn();
	}

	public static void require(String ns) {
		requireFn.invoke(symbol(ns));
	}

}
