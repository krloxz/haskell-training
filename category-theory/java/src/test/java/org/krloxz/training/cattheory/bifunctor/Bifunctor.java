package org.krloxz.training.cattheory.bifunctor;

import java.util.function.Function;
import org.krloxz.training.cattheory.functor.Functor;

/**
 * A {@link Functor} that maps two categories into one. From the container perspective: a container
 * with two values that can be mapped at once.
 * <p>
 * If {@link Functor}'s can be considered single-parameter functions between categories, this
 * functor is the equivalent to a function with two parameters.
 * <p>
 * <h3>Bifunctor Laws</h3>
 * <p>
 * A functor must map all the objects and morphisms from two categories into one preserving
 * composition and identity:
 * <ul>
 * <li>bimap id id = id
 * <li>bimap f g . bimap f' g' = bimap (f . f') (g . g')
 * </ul>
 *
 * @author Carlos Gomez
 * @param <First>
 *        type of the first element
 * @param <Second>
 *        type of the second element
 */
public interface Bifunctor<First, Second> {

  /**
   * Maps the contents of this functor applying two functions at once.
   *
   * @param <FirstResult>
   *        target type of the first mapping
   * @param <SecondResult>
   *        target type of the second mapping
   * @param firstMapping
   *        function applied to the first element
   * @param secondMapping
   *        function applied the second element
   * @return a new functor containing elements of type FirstResult and SecondResult
   */
  <FirstResult, SecondResult> Bifunctor<FirstResult, SecondResult> bimap(
      Function<? super First, ? extends FirstResult> firstMapping,
      Function<? super Second, ? extends SecondResult> secondMapping);

}
