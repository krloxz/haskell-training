package org.krloxz.jaskell;

import java.util.function.Function;

/**
 * Functors are containers that encapsulate morphisms between categories.
 * <p>
 * Instances should satisfy the following rules:
 *
 * <pre>
 *  // Identity:
 *  x.map(Function.identity()) == x == Function.identity().apply(x)
 *
 *  // Associativity (f & g are instances of Function):
 *  x.map(g.compose(f)) == x.map(f).map(g)
 * </pre>
 *
 * Please notice that the purpose of a functor is to preserve composition (structure) between
 * category transformations.
 *
 * @author Carlos Gomez
 * @param <A>
 */
public interface Monad2<A> {

  <B> Monad2<B> flatMap(Function<? super A, ? extends Monad2<? extends B>> f);

  public interface Higher<WITNESS, T> {
  }

  public class ListKind<T> implements Higher<ListKind, T> {

    private final List<T> boxed;

    public ListKind(final List<T> list) {
      this.boxed = list;
    }

    public static <T> List<T> narrowK(final Higher<ListKind, T> hkt) {
      final ListKind<T> list = (ListKind<T>) hkt;
      return list.boxed;
    }
  }

}
