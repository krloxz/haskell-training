package org.krloxz.training.cattheory;

import static org.assertj.core.api.Assertions.assertThat;
import java.util.NoSuchElementException;
import java.util.Objects;
import java.util.function.Function;
import org.junit.jupiter.api.Test;

/**
 * Demonstrates Kleisli Categories.
 *
 * @author Carlos Gomez
 *
 */
public class KleisliCategoryTest {

  @Test
  void safeRootReturnsOptionalResults() {
    assertThat(safeRoot(9).value()).isEqualTo(3);
    assertThat(safeRoot(9).isEmpty()).isFalse();
    assertThat(safeRoot(-9).isEmpty()).isTrue();
  }

  @Test
  void safeRootReciprocalReturnsOptionalResults() {
    assertThat(safeRootReciprocal(9).value()).isEqualTo((double) 1 / 3);
    assertThat(safeRootReciprocal(9).isEmpty()).isFalse();
    assertThat(safeRootReciprocal(-9).isEmpty()).isTrue();
    assertThat(safeRootReciprocal(0).isEmpty()).isTrue();
  }

  @Test
  void optionalFunctionsCompose() {
    final var value = 9d;
    assertThat(
        Optional.compose(this::safeReciprocal, this::safeRoot).apply(value))
            .isEqualTo(
                Optional.compose(
                    Optional.compose(this::safeReciprocal, this::safeRoot),
                    Optional::kleisliIdentity)
                    .apply(value))
            .isEqualTo(
                Optional.compose(
                    this::safeReciprocal,
                    Optional.compose(this::safeRoot, Optional::kleisliIdentity))
                    .apply(value))
            .isEqualTo(
                safeReciprocal(safeRoot(value).value()));
  }

  @Test
  void optionalsCompositionHasIdentity() {
    final var value = 9d;
    assertThat(safeRoot(9))
        .isEqualTo(
            Optional.compose(this::safeRoot, Optional::kleisliIdentity).apply(value))
        .isEqualTo(
            Optional.compose(
                (Function<Double, Optional<Double>>) Optional::kleisliIdentity, this::safeRoot)
                .apply(value));
  }

  @Test
  void optionalsCompositionIsAssociative() {
    final var value = 9d;
    assertThat(safeReciprocal(safeRoot(value).value()))
        .isEqualTo(
            Optional.compose(
                Optional.compose(this::safeReciprocal, this::safeRoot),
                Optional::kleisliIdentity)
                .apply(value))
        .isEqualTo(
            Optional.compose(
                this::safeReciprocal,
                Optional.compose(this::safeRoot, Optional::kleisliIdentity))
                .apply(value));
  }

  private Optional<Double> safeRoot(final double x) {
    if (x >= 0) {
      return Optional.of(Math.sqrt(x));
    }
    return Optional.empty();
  }

  private Optional<Double> safeReciprocal(final double x) {
    if (x > 0) {
      return Optional.of(1 / x);
    }
    return Optional.empty();
  }

  private Optional<Double> safeRootReciprocal(final double x) {
    return Optional.compose(this::safeReciprocal, this::safeRoot).apply(x);
  }

}


class Optional<T> {

  private final T value;

  private Optional(final T value) {
    this.value = value;
  }

  public static <T> Optional<T> empty() {
    return new Optional<>(null);
  }

  public static <T> Optional<T> of(final T value) {
    return new Optional<>(value);
  }

  public boolean isEmpty() {
    return this.value == null;
  }

  public T value() {
    if (isEmpty()) {
      throw new NoSuchElementException();
    }
    return this.value;
  }

  public static <T> Optional<T> kleisliIdentity(final T value) {
    return of(Objects.requireNonNull(value));
  }

  public static <A, B, C> Function<A, Optional<C>> compose(
      final Function<A, Optional<B>> f,
      final Function<B, Optional<C>> g) {
    return x -> {
      final var result = f.apply(x);
      if (result.isEmpty()) {
        return empty();
      }
      return g.apply(result.value());
    };
  }

  @Override
  public boolean equals(final Object obj) {
    if (this == obj) {
      return true;
    }
    if (obj == null) {
      return false;
    }
    if (getClass() != obj.getClass()) {
      return false;
    }
    final var other = Optional.class.cast(obj);
    return Objects.equals(this.value, other.value);
  }

  @Override
  public int hashCode() {
    return Objects.hash(this.value);
  }

}
