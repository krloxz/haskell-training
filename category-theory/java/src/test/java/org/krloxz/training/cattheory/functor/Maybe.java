package org.krloxz.training.cattheory.functor;

import java.util.Objects;
import java.util.function.Function;

/**
 * {@link Functor} that may or may not contain a value. This is the data structure that inspired
 * {@link java.util.Optional}.
 *
 * @author Carlos Gomez
 * @param <A>
 *        the type of value that may be contained by this functor
 */
public abstract class Maybe<A> implements Functor<A> {

  private static final Nothing<?> nothing = new Nothing<>();

  @SuppressWarnings("unchecked")
  public static <A> Maybe<A> nothing() {
    return (Maybe<A>) nothing;
  }

  public static <A> Maybe<A> just(final A value) {
    return new Just<>(value);
  }

  public static <A, B> Function<Maybe<A>, Maybe<B>> lift(final Function<A, B> f) {
    return maybe -> maybe.map(f);
  }

  @Override
  public abstract <B> Maybe<B> map(Function<A, B> f);

  private static class Nothing<A> extends Maybe<A> {

    private Nothing() {}

    @Override
    @SuppressWarnings("unchecked")
    public <B> Maybe<B> map(final Function<A, B> f) {
      return (Maybe<B>) this;
    }

    @Override
    public String toString() {
      return "Nothing";
    }

  }

  private static class Just<A> extends Maybe<A> {

    private final A value;

    private Just(final A value) {
      this.value = value;
    }

    @Override
    public <B> Maybe<B> map(final Function<A, B> f) {
      return new Just<>(f.apply(this.value));
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
      if (!(obj instanceof Just)) {
        return false;
      }
      final Just<?> other = Just.class.cast(obj);
      return Objects.equals(this.value, other.value);
    }

    @Override
    public String toString() {
      return "Just " + this.value.toString();
    }

  }

}
