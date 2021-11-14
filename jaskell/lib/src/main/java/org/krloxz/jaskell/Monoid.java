package org.krloxz.jaskell;

/**
 * Monoids, or categories with a single element, are types with an associative binary operation that
 * has an identity.
 * <p>
 * Instances should satisfy the following rules:
 *
 * <pre>
 *  // Right identity:
 *  x.append(Monoid.empty()) == x
 *
 *  // Left identity:
 *  Monoid.empty().append(x) == x
 *
 *  // Associativity:
 *  x.append(y.append(z) == x.append(y).append(z)
 * </pre>
 *
 * Some types can be viewed as a monoid in more than one way, e.g. both addition and multiplication
 * on numbers. In such cases two different implementations must be created.
 *
 * @author Carlos Gomez
 * @see <a href="https://en.wikipedia.org/wiki/Monoid">Monoid in Wikipedia</a>
 * @see <a href=
 *      "https://github.com/isc-carlos-gomez/haskell-training/blob/master/category-theory/notes/3.%20Categories%20Great%20and%20Small.md#monoids">
 *      Monoids in Category Theory </a>
 * @param <T>
 */
public interface Monoid<M extends Monoid<M>> {

  static <M extends Monoid<M>> Monoid<M> empty() {
    throw new UnsupportedOperationException();
  }

  M append(M monoid);

}
