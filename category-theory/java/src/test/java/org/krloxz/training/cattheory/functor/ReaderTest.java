package org.krloxz.training.cattheory.functor;

import static org.assertj.core.api.Assertions.assertThat;
import java.util.function.Function;
import org.junit.jupiter.api.Test;

/**
 * Proves Functor laws for the {@link Reader} functor.
 *
 * @author Carlos Gomez
 */
class ReaderTest {

  @Test
  void preservesIdentity() {
    final Reader<Integer, String> reader = Object::toString;

    assertThat(reader.map(Function.identity()).apply(1))
        .isEqualTo(Function.<Reader<Integer, String>>identity().apply(reader).apply(1))
        .isEqualTo("1");
  }

  @Test
  void preservesComposition() {
    final Reader<String, Boolean> reader = Boolean::parseBoolean;
    final Reader<Boolean, Integer> g = b -> b ? 1 : 0;
    final Reader<Integer, String> f = Object::toString;

    assertThat(reader.map(f.after(g)).apply("true"))
        .isEqualTo(reader.map(g).map(f).apply("true"))
        .isEqualTo("1");
  }

  @Test
  void supportsPartialApplicationThroughLifting() {
    final Reader<String, Boolean> reader = Boolean::parseBoolean;
    final Reader<Boolean, Integer> g = b -> b ? 1 : 0;
    final Reader<Integer, String> f = Object::toString;

    final Reader<Reader<String, Integer>, Reader<String, String>> fmapF = Functor.lift(f);
    final Reader<Reader<String, Boolean>, Reader<String, Integer>> fmapG = Functor.lift(g);

    assertThat(reader.map(f.after(g)).apply("true"))
        .isEqualTo(fmapF.after(fmapG).apply(reader).apply("true"))
        .isEqualTo("1");
  }

}
