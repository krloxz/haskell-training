package org.krloxz.training.cattheory.bifunctor;

import java.util.Objects;
import java.util.function.Function;

/**
 * A pair of elements implemented as a {@link Bifunctor}.
 *
 * @author Carlos Gomez
 * @param <Left>
 *        type of the left element
 * @param <Right>
 *        type of the right element
 */
public class Pair<Left, Right> implements Bifunctor<Left, Right> {

  private final Left left;
  private final Right right;

  private Pair(final Left left, final Right right) {
    this.left = left;
    this.right = right;
  }

  public static <Left, Right> Pair<Left, Right> of(final Left left, final Right right) {
    return new Pair<>(left, right);
  }

  @Override
  public <LeftResult, RightResult> Pair<LeftResult, RightResult> bimap(
      final Function<? super Left, ? extends LeftResult> leftFunction,
      final Function<? super Right, ? extends RightResult> rightFunction) {
    return Pair.of(leftFunction.apply(this.left), rightFunction.apply(this.right));
  }

  @Override
  public int hashCode() {
    return Objects.hash(this.left, this.right);
  }

  @Override
  public boolean equals(final Object obj) {
    if (this == obj) {
      return true;
    }
    if (!(obj instanceof Pair)) {
      return false;
    }
    final Pair<?, ?> other = Pair.class.cast(obj);
    return Objects.equals(this.left, other.left) && Objects.equals(this.right, other.right);
  }

}
