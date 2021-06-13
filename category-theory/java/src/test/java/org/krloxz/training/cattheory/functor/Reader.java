package org.krloxz.training.cattheory.functor;

import java.util.function.Function;

/**
 * {@link Functor} that encapsulates function types.
 *
 * @author Carlos Gomez
 * @param <R>
 *        source type of the function encapsulated by this reader
 * @param <A>
 *        target type of the function encapsulated by this reader
 */
@FunctionalInterface
public interface Reader<R, A> extends Function<R, A>, Functor<A> {

  @Override
  default <B> Reader<R, B> map(final Function<A, B> f) {
    return x -> f.compose(this).apply(x);
  }

  /**
   * A synonym that intends to improve readability of {@link Function#compose(Function)}.
   *
   * @see Function#compose(Function)
   */
  default <V> Reader<V, A> after(final Function<? super V, ? extends R> f) {
    return x -> this.compose(f).apply(x);
  }

}
