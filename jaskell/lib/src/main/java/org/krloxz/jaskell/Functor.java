package org.krloxz.jaskell;

import java.util.function.Function;

/**
 * Functors are containers that encapsulate morphisms between categories.
 * <p>
 * Instances should satisfy the following rules:
 *
 * <pre>
 *  // Identity:
 *  x.map(Function.identity()) == Function.identity().apply(x) == x
 *
 *  // Composition (f & g are instances of Function):
 *  x.map(g.compose(f)) == x.map(f).map(g)
 * </pre>
 *
 * Please notice that the purpose of a functor is to preserve structure between category
 * transformations by preserving identity and composition.
 *
 * @author Carlos Gomez
 * @param <A>
 */
public interface Functor<A> {

  /**
   * Maps (or applies) one function over the contents of this functor.
   *
   * @param <B>
   *        target type of the function to map
   * @param f
   *        a function mapping A's to B's
   * @return a new functor containing objects of type B
   */
  <B> Functor<B> map(Function<A, B> f);

}
