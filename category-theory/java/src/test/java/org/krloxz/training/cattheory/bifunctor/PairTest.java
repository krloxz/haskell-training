package org.krloxz.training.cattheory.bifunctor;

import static java.util.function.Function.identity;
import static org.assertj.core.api.Assertions.assertThat;
import java.util.function.UnaryOperator;
import org.junit.jupiter.api.Test;

/**
 * Proves Bifunctor laws for the {@link Pair}.
 *
 * @author Carlos Gomez
 */
class PairTest {

  @Test
  void preservesIdentity() {
    final Pair<Integer, Integer> aPair = Pair.of(1, 2);
    assertThat(aPair.bimap(identity(), identity()))
        .isEqualTo(aPair);
  }

  @Test
  void preservesComposition() {
    final UnaryOperator<Integer> plus1 = x -> x + 1;
    final UnaryOperator<Integer> plus2 = x -> x + 2;
    final UnaryOperator<Integer> plus3 = x -> x + 3;
    final UnaryOperator<Integer> plus4 = x -> x + 4;

    final Pair<Integer, Integer> aPair = Pair.of(1, 2);
    assertThat(aPair.bimap(plus1, plus2).bimap(plus3, plus4))
        .isEqualTo(aPair.bimap(plus1.andThen(plus3), plus2.andThen(plus4)))
        .isEqualTo(Pair.of(5, 8));
  }

}
