package com.webforj.component.table.renderer;

import java.util.ArrayList;
import java.util.List;

/**
 * A renderer that delegates to different sub-renderers based on client-side conditions evaluated
 * against the column's value function result. Conditions are evaluated in order and the first match
 * wins.
 *
 * <pre>{@code
 * BadgeRenderer<MusicRecord> rockBadge = new BadgeRenderer<>();
 * rockBadge.setTheme(BadgeTheme.PRIMARY);
 *
 * BadgeRenderer<MusicRecord> jazzBadge = new BadgeRenderer<>();
 * jazzBadge.setTheme(BadgeTheme.SUCCESS);
 *
 * BadgeRenderer<MusicRecord> popBadge = new BadgeRenderer<>();
 * popBadge.setTheme(BadgeTheme.WARNING);
 *
 * BadgeRenderer<MusicRecord> defaultBadge = new BadgeRenderer<>();
 * defaultBadge.setTheme(BadgeTheme.DEFAULT);
 *
 * ConditionalRenderer<MusicRecord> renderer = new ConditionalRenderer<>();
 * renderer.when("Rock", rockBadge);
 * renderer.when("Jazz", jazzBadge);
 * renderer.when(Condition.in("Pop", "R&B"), popBadge);
 * renderer.otherwise(defaultBadge);
 *
 * table.addColumn("musicType", MusicRecord::getMusicType).setRenderer(renderer);
 * }</pre>
 *
 * @param <T> the row data type
 *
 * @see Condition
 *
 * @author Hyyan Abo Fakher
 * @since 25.12
 */
public class ConditionalRenderer<T> extends Renderer<T> {
  private final List<ConditionEntry<T>> entries = new ArrayList<>();
  private Renderer<T> fallback;

  /**
   * Adds a condition-renderer pair.
   *
   * @param condition the condition to evaluate
   * @param renderer the renderer to use when the condition is true
   * @return this renderer
   */
  public ConditionalRenderer<T> when(Condition condition, Renderer<T> renderer) {
    entries.add(new ConditionEntry<>(condition, renderer));
    renderer.addChangeListener(e -> fireChangeEvent());
    fireChangeEvent();
    return this;
  }

  /**
   * Adds a value-equality condition.
   *
   * @param value the cell value to match
   * @param renderer the renderer to use when the value matches
   * @return this renderer
   */
  public ConditionalRenderer<T> when(String value, Renderer<T> renderer) {
    return when(Condition.equalTo(value), renderer);
  }

  /**
   * Sets the fallback renderer used when no condition matches.
   *
   * @param renderer the fallback renderer
   * @return this renderer
   */
  public ConditionalRenderer<T> otherwise(Renderer<T> renderer) {
    this.fallback = renderer;
    renderer.addChangeListener(e -> fireChangeEvent());
    fireChangeEvent();
    return this;
  }

  /** {@inheritDoc} */
  @Override
  public String build() {
    if (entries.isEmpty() && fallback != null) {
      return fallback.build();
    }

    StringBuilder sb = new StringBuilder();

    for (int i = 0; i < entries.size(); i++) {
      ConditionEntry<T> entry = entries.get(i);
      if (i == 0) {
        sb.append("<% if (").append(entry.condition.toExpression()).append(") { %>");
      } else {
        sb.append("<% } else if (").append(entry.condition.toExpression()).append(") { %>");
      }
      sb.append(entry.renderer.build());
    }

    if (fallback != null) {
      sb.append("<% } else { %>");
      sb.append(fallback.build());
    }

    if (!entries.isEmpty()) {
      sb.append("<% } %>");
    }

    return sb.toString();
  }

  /** {@inheritDoc} */
  @Override
  protected void onAttach() {
    super.onAttach();
    for (ConditionEntry<T> entry : entries) {
      entry.renderer.setTable(getTable());
    }

    if (fallback != null) {
      fallback.setTable(getTable());
    }
  }

  private static class ConditionEntry<T> {
    final Condition condition;
    final Renderer<T> renderer;

    ConditionEntry(Condition condition, Renderer<T> renderer) {
      this.condition = condition;
      this.renderer = renderer;
    }
  }
}
