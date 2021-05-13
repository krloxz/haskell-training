package org.krloxz.training.cattheory.coproduct;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatIllegalArgumentException;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

/**
 * Proofs {@link Either} is better coproduct than int.
 *
 * @author Carlos Gomez
 *
 */
class EitherCoproductTests {

  @Nested
  class WhenInjectionsOverlap {

    @Test
    void eitherIsBetterCoproductThanInt() {
      final var number = 1;
      final var bool = true;

      assertThat(intToInt(number)).isEqualTo(eitherFactorizer(Either.left(number)));
      assertThat(boolToInt(bool)).isEqualTo(eitherFactorizer(Either.right(bool)));

      assertThatIllegalArgumentException().isThrownBy(() -> {
        assertThat(Either.left(number)).isEqualTo(intFactorizer(intToInt(number)));
      });
      assertThatIllegalArgumentException().isThrownBy(() -> {
        assertThat(Either.right(bool)).isEqualTo(intFactorizer(boolToInt(bool)));
      });
    }

    private int intToInt(final int number) {
      return number;
    }

    private int boolToInt(final boolean bool) {
      return bool ? 0 : 1;
    }

    private int eitherFactorizer(final Either<Integer, Boolean> either) {
      return either.apply(this::intToInt, this::boolToInt);
    }

    private Either<Integer, Boolean> intFactorizer(final int number) {
      if (number == 0 || number == 1) {
        throw new IllegalArgumentException(
            "It's impossible to know if this value was generated after an integer or a boolean");
      }
      return Either.left(number);
    }

  }

  @Nested
  class WhenInjectionsDontOverlap {

    @Test
    void eitherIsBetterCoproductThanInt() {
      final var number = 1;
      final var bool = true;

      assertThat(intToInt(number)).isEqualTo(eitherFactorizer(Either.left(number)));
      assertThat(boolToInt(bool)).isEqualTo(eitherFactorizer(Either.right(bool)));

      assertThat(Either.left(number)).isEqualTo(intFactorizer(intToInt(number)));
      assertThat(Either.right(bool)).isEqualTo(intFactorizer(boolToInt(bool)));

      assertThat(intToInt(Integer.MAX_VALUE))
          .isEqualTo(eitherFactorizer(Either.left(Integer.MAX_VALUE)));
      assertThat(Either.left(Integer.MAX_VALUE))
          .describedAs("intFactorizer should overflow on integer's max value")
          .isNotEqualTo(intFactorizer(intToInt(Integer.MAX_VALUE)));
    }

    private int intToInt(final int number) {
      if (number < 0) {
        return number;
      }
      return number + 2;
    }

    private int boolToInt(final boolean bool) {
      return bool ? 0 : 1;
    }

    private int eitherFactorizer(final Either<Integer, Boolean> either) {
      return either.apply(this::intToInt, this::boolToInt);
    }

    private Either<Integer, Boolean> intFactorizer(final int number) {
      if (number == 0 || number == 1) {
        return Either.right(number == 0);
      }
      if (number < 0) {
        return Either.left(number);
      }
      return Either.left(number - 2);
    }

  }

}
