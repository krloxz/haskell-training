package org.krloxz.training.cattheory;

import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.jupiter.api.Assertions.assertTimeoutPreemptively;
import java.time.Duration;
import java.util.HashMap;
import java.util.Map;
import java.util.Random;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.function.Function;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

/**
 * Tests function memoization.
 *
 * @author Carlos Gomez
 *
 */
class FunctionMemoizationTest {

  private Map<Object, Object> cache;

  @BeforeEach
  void initCache() {
    this.cache = new HashMap<>();
  }

  @Test
  void memoizeSimpleComputation() {
    final var compute = memoize(this::compute);

    final var firstComputation = new AtomicInteger();
    assertTimeoutPreemptively(Duration.ofSeconds(2), () -> {
      firstComputation.set(compute.apply(1));
    });

    assertTimeoutPreemptively(Duration.ofSeconds(1), () -> {
      assertThat(compute.apply(1)).isEqualTo(firstComputation.get());
    });
  }

  @Test
  void memoizedRandomIsNotEqualToRegularRandom() {
    final var bound = 1_000_000;
    final var random = new Random();
    final var memoizedNextInt = this.<Integer, Integer>memoize(random::nextInt);

    assertThat(memoizedNextInt.apply(bound) == memoizedNextInt.apply(bound))
        .isNotEqualTo(random.nextInt(bound) == random.nextInt(bound));
  }

  @Test
  void canMemoizeRandomGenerationByProvidingSeed() {
    final var seed = 86785;
    final var memoizedRandomInt = memoize(this::randomFromSeed);

    assertThat(memoizedRandomInt.apply(seed) == memoizedRandomInt.apply(seed))
        .isEqualTo(randomFromSeed(seed) == randomFromSeed(seed));
  }

  private int compute(final int value) {
    try {
      Thread.sleep(1500);
      return value;
    } catch (final InterruptedException e) {
      throw new IllegalStateException(e);
    }
  }

  private int randomFromSeed(final int seed) {
    return new Random(seed).nextInt();
  }

  @SuppressWarnings("unchecked")
  private <T, R> Function<T, R> memoize(final Function<T, R> compute) {
    return (final var input) -> {
      final var genericCompute = (Function<? super Object, ? extends Object>) compute;
      return (R) this.cache.computeIfAbsent(input, genericCompute);
    };
  }

}
