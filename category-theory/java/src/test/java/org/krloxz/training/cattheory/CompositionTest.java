package org.krloxz.training.cattheory;

import static org.assertj.core.api.Assertions.assertThat;
import java.util.function.Function;
import org.junit.jupiter.api.Test;

/**
 * Tests the properties of composition.
 *
 * @author carlos
 *
 */
class CompositionTest {

  @Test
  void testComposition() {
    final var text = "Hello Category Theory!";
    assertThat(isSentenceLength(countWords(text)))
        .isEqualTo(compose(this::countWords, this::isSentenceLength).apply(text));
  }

  @Test
  void testIdentity() {
    final var text = "Hello Category Theory!";
    assertThat(identity(1)).isEqualTo(1);
    assertThat(countWords(text))
        .isEqualTo(compose(this::countWords, this::identity).apply(text))
        .isEqualTo(this.<String, String, Integer>compose(this::identity, this::countWords).apply(text));
  }

  @Test
  void testAssociativity() {
    final var text = "Hello Category Theory!";
    assertThat(isTrue(isSentenceLength(countWords(text))))
        .isEqualTo(compose(compose(this::countWords, this::isSentenceLength), this::isTrue).apply(text))
        .isEqualTo(compose(this::countWords, compose(this::isSentenceLength, this::isTrue)).apply(text));
  }

  private <T> T identity(final T x) {
    return x;
  }

  private <A, B, C> Function<A, C> compose(final Function<A, B> f, final Function<B, C> g) {
    return x -> g.apply(f.apply(x));
  }

  private int countWords(final String text) {
    return text.split("\\s").length;
  }

  private boolean isSentenceLength(final int length) {
    return length > 1;
  }

  private String isTrue(final boolean bool) {
    return bool ? "Yes" : "No";
  }

}
