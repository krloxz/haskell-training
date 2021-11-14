package org.krloxz.jaskell;

public class KleisliCategory {

  public <A, MA> KleisliArrow<A, MA> identity(final A a) {
    return null;
  }

  public <A, B, MB, MC> KleisliArrow<A, MC> compose(final KleisliArrow<A, MB> f, final KleisliArrow<B, MC> g) {
    return null;
  }

}
