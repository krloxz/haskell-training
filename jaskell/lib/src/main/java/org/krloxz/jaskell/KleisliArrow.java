package org.krloxz.jaskell;

public interface KleisliArrow<A, MB> {

  MB apply(A a);

}
