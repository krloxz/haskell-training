package org.krloxz.training.cattheory.functor;

import java.util.function.Function;

/**
 * A mathematical functor: a container that encapsulates a morphism between categories.
 * <p>
 * <h3>Functor Laws</h3>
 * <p>
 * A functor must map all the objects and morphisms from one category into another preserving
 * composition and identity:
 * <ul>
 * <li>fmap id = id
 * <li>fmap (f . g) = fmap f . fmap g
 * </ul>
 *
 * @author Carlos Gomez
 * @param <A>
 *        the data type contained by this functor
 */
public interface Functor<A> {

  /**
   * Maps one function over the contents of this functor. Called {@code fmap} in Haskell.
   *
   * @param <B>
   *        target type of the function to map
   * @param f
   *        a function mapping A's to B's
   * @return a new functor containing objects of type B
   */
  <B> Functor<B> map(Function<A, B> f);

  /**
   * Lifts one function so that it can operate over functors containing its source and target types.
   * For instance a function mapping Boolean to Integer can be lifted to map Maybe&lt;Boolean&gt;
   * into Maybe&lt;Integers&gt;:
   *
   * <pre>
   * {@code
   * Function<Boolean, Integer> f = b -> b ? 1 : 0
   * Function<Maybe<Boolean>, Maybe<Integer>> fmapF = Functor.lift(f);
   * }
   * </pre>
   * <p>
   * Lifting a function would be equivalent to partially applying <code>fmap</code> to a function in
   * Haskell.
   *
   * @param <A>
   *        source type of the function to lift
   * @param <B>
   *        target type of the function to lift
   * @param <FA>
   *        source type for the lifted function
   * @param <FB>
   *        target type for the lifted function. Given to generics limitations the client needs to
   *        make sure that source and target functors are the same
   * @param f
   *        a function from A to B
   * @return a lifted function encapsulated as a {@link Reader} from FA to FB
   */
  @SuppressWarnings("unchecked")
  static <A, B, FA extends Functor<A>, FB extends Functor<B>> Reader<FA, FB> lift(
      final Function<A, B> f) {
    return functor -> (FB) functor.map(f);
  }

}
