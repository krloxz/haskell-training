package org.krloxz.training.cattheory.functor;

import static org.assertj.core.api.Assertions.assertThat;
import java.util.function.Function;
import org.junit.jupiter.api.Test;

/**
 * Demonstrates how a maybeTail function could be coded in Java.
 *
 * @author Carlos Gomez
 */
class MaybeTailTest {

  @Test
  void testMaybeTail() {
    final Function<Integer, Integer> plus1 = x -> x + 1;

    assertThat(maybeTail(List.of(1, 2)).map(List.lift(plus1)))
        .isEqualTo(Maybe.just(List.of(3)));
  }

  private <A> Maybe<List<A>> maybeTail(final List<A> list) {
    return list.apply(
        Maybe::nothing,
        l -> l.tail().apply(Maybe::nothing, Maybe::just));
  }

}
