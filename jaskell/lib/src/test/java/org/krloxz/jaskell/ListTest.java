package org.krloxz.jaskell;

import static org.assertj.core.api.Assertions.assertThat;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.Set;
import java.util.function.Function;
import org.junit.jupiter.api.Test;

class ListTest {

  @Test
  void isMonoid() {
    final List<String> x = List.of("X");
    final List<String> y = List.of("Y");
    final List<String> z = List.of("Z");

    assertThat(List.<String>empty().append(x)).isEqualTo(x);
    assertThat(x.append(List.empty())).isEqualTo(x);
    assertThat(x.append(y.append(z))).isEqualTo(x.append(y).append(z));
  }

  @Test
  void isFunctor() {
    final List<String> x = List.of("X");
    final Function<String, Integer> f = String::length;
    final Function<Integer, Boolean> g = i -> i > 0;

    assertThat(x.map(Function.identity())).isEqualTo(x);
    assertThat(x.map(g.compose(f))).isEqualTo(x.map(f).map(g));
  }

  void testMethod() {

    method(new ArrayList<HashSet<Integer>>());
  }

  private void method(final java.util.List<? extends Set<? extends Number>> arrayList) {
    // TODO Auto-generated method stub

  }

  void testFlatMap() {
    final Function<Object, ArrayList<HashSet>> f = x -> new ArrayList<>();
    flatMap(f);
  }

  private Object flatMap(final Function<Object, ? extends java.util.List<? extends Set>> f) {
    return null;
  }

  private interface MonadTest {

    <B> Object flatMap(final B f);

  }

  private static class MonadTestImpl implements MonadTest {

    @Override
    public <B> String flatMap(final B f) {
      return null;
    }

  }

}
