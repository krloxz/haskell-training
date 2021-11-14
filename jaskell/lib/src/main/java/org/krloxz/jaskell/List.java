package org.krloxz.jaskell;

import java.util.function.Function;
import org.krloxz.jaskell.List.Empty;
import org.krloxz.jaskell.List.Full;

public sealed interface List<T> extends Monoid<List<T>>, Functor<T>, Monad2<T>permits Empty<T>,Full<T> {

  static <T> List<T> empty() {
    return new Empty<>();
  }

  static <T> List<T> of(final T element) {
    return new Full<>(element, empty());
  }

  T head();

  List<T> tail();

  @Override
  <B> List<B> map(Function<T, B> f);

  // @Override
  // default <B> List<B> flatMap(final Function<? super T, ? extends Monad<? extends B>> f) {
  // return null;
  // }

  <B> List<B> flatMap(final Function<? super T, ? extends List<? extends B>> f);

  record Empty<T> () implements List<T> {

    @Override
    public T head() {
      throw new UnsupportedOperationException();
    }

    @Override
    public List<T> tail() {
      throw new UnsupportedOperationException();
    }

    @Override
    public List<T> append(final List<T> list) {
      return list;
    }

    @Override
    public <B> List<B> map(final Function<T, B> f) {
      return empty();
    }

    @Override
    public <B> List<B> flatMap(final KleisliArrow<T, B> arrow) {
      return empty();
    }

  }

  record Full<T> (T head, List<T> tail) implements List<T> {

    @Override
    public List<T> append(final List<T> list) {
      if (list instanceof final Empty<T> empty) {
        return this;
      } else if (list instanceof final Full<T> full) {
        return new Full<>(full.head, append(full.tail));
      }
      throw new IllegalArgumentException();
    }

    @Override
    public <B> List<B> map(final Function<T, B> f) {
      return new Full<>(f.apply(this.head), this.tail.map(f));
    }

    @Override
    public <B> List<B> flatMap(final KleisliArrow<T, B> arrow) {
      return new Full<>(arrow.apply(this.head));
    }

  }

}
