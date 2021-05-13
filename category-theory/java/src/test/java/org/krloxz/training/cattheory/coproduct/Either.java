package org.krloxz.training.cattheory.coproduct;

import java.util.Objects;
import java.util.function.Function;

/**
 * A value with two possibilities, either left or right; but never both.
 *
 * @author Carlos Gomez
 *
 * @param <L> type of the left value
 * @param <R> type of the right value
 * @see https://hackage.haskell.org/package/base-4.15.0.0/docs/Data-Either.html
 */
public interface Either<L, R> {

  static <L, R> Either<L, R> left(final L value) {
    return new Either.Left<>(value);
  }

  static <L, R> Either<L, R> right(final R value) {
    return new Either.Right<>(value);
  }

  <T> T apply(Function<L, T> left, Function<R, T> right);

  <T> Either<L, T> map(Function<R, T> f);

  static class Left<L, R> implements Either<L, R> {

    private final L value;

    private Left(final L value) {
      this.value = value;
    }

    @Override
    public <T> T apply(final Function<L, T> left, final Function<R, T> right) {
      return left.apply(this.value);
    }

    @Override
    public <T> Either<L, T> map(final Function<R, T> f) {
      return new Either.Left<>(this.value);
    }

    @Override
    public int hashCode() {
      return Objects.hash(this.value);
    }

    @Override
    public boolean equals(final Object obj) {
      if (this == obj) {
        return true;
      }
      if (!(obj instanceof Left)) {
        return false;
      }
      final var other = Left.class.cast(obj);
      return Objects.equals(this.value, other.value);
    }

    @Override
    public String toString() {
      return "Left [value=" + this.value + "]";
    }

  }

  static class Right<L, R> implements Either<L, R> {

    private final R value;

    private Right(final R value) {
      this.value = value;
    }

    @Override
    public <T> T apply(final Function<L, T> left, final Function<R, T> right) {
      return right.apply(this.value);
    }

    @Override
    public <T> Either<L, T> map(final Function<R, T> f) {
      return new Either.Right<>(f.apply(this.value));
    }

    @Override
    public int hashCode() {
      return Objects.hash(this.value);
    }

    @Override
    public boolean equals(final Object obj) {
      if (this == obj) {
        return true;
      }
      if (!(obj instanceof Right)) {
        return false;
      }
      final var other = Right.class.cast(obj);
      return Objects.equals(this.value, other.value);
    }

    @Override
    public String toString() {
      return "Right [value=" + this.value + "]";
    }

  }

}
