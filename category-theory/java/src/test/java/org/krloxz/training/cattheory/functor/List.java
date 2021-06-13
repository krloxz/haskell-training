package org.krloxz.training.cattheory.functor;

import java.util.Arrays;
import java.util.Objects;
import java.util.function.Function;
import java.util.function.Supplier;

/**
 * {@link Functor} that contains a list of values.
 *
 * @author Carlos Gomez
 * @param <A>
 *        the type of values contained by this functor
 */
public abstract class List<A> implements Functor<A> {

  private static final Empty<?> empty = new Empty<>();

  @SuppressWarnings("unchecked")
  public static <A> List<A> empty() {
    return (List<A>) empty;
  }

  @SafeVarargs
  public static <A> List<A> of(final A... elements) {
    if (elements.length == 0) {
      return empty();
    }
    if (elements.length == 1) {
      return new Full<>(elements[0], empty());
    }
    return new Full<>(elements[0], List.of(Arrays.copyOfRange(elements, 1, elements.length)));
  }

  public List<A> add(final A element) {
    return new Full<>(element, this);
  }

  public static <A, B> Function<List<A>, List<B>> lift(final Function<A, B> f) {
    return list -> list.map(f);
  }

  public abstract List<A> tail();

  @Override
  public abstract <B> List<B> map(Function<A, B> f);

  public abstract <B> B apply(final Supplier<B> whenEmpty, final Function<List<A>, B> whenFull);

  @Override
  public String toString() {
    return "[" + this.show() + "]";
  }

  abstract String show();

  private static class Empty<A> extends List<A> {

    private Empty() {}

    @Override
    public List<A> tail() {
      throw new IllegalStateException("Empty lists have no tails");
    }

    @Override
    @SuppressWarnings("unchecked")
    public <B> List<B> map(final Function<A, B> f) {
      return (List<B>) this;
    }

    @Override
    public <B> B apply(final Supplier<B> whenEmpty, final Function<List<A>, B> whenFull) {
      return whenEmpty.get();
    }

    @Override
    String show() {
      return "";
    }

    @Override
    public boolean equals(final Object obj) {
      return super.equals(obj);
    }

  }

  private static class Full<A> extends List<A> {

    private final A head;
    private final List<A> tail;

    private Full(final A head, final List<A> tail) {
      this.head = head;
      this.tail = tail;
    }

    @Override
    public List<A> tail() {
      return this.tail;
    }

    @Override
    public <B> List<B> map(final Function<A, B> f) {
      return new Full<>(f.apply(this.head), this.tail.map(f));
    }

    @Override
    public <B> B apply(final Supplier<B> whenEmpty, final Function<List<A>, B> whenFull) {
      return whenFull.apply(this);
    }

    @Override
    String show() {
      return this.head + ", " + this.tail.show();
    }

    @Override
    public int hashCode() {
      return Objects.hash(this.head, this.tail);
    }

    @Override
    public boolean equals(final Object obj) {
      if (this == obj) {
        return true;
      }
      if (!(obj instanceof Full)) {
        return false;
      }
      final Full<?> other = Full.class.cast(obj);
      return Objects.equals(this.head, other.head)
          && Objects.equals(this.tail, other.tail);
    }

  }

}
